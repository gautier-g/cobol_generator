# file: cobolsmartgen/generate/_llm_codegen.py
from __future__ import annotations
import json
import logging
import os
import re
from datetime import datetime
from pathlib import Path
from typing import Dict, List, Optional
import time
from ..utils import fs, trace, pipeline_tracer
from ..adapters import llm_auto
from . import procedure_templates
from . import prompt_formatters
from . import code_context_builder

LOG = logging.getLogger(__name__)

def _layer_folder(layer: str) -> str:
    l = (layer or "logic").lower()
    return l if l in ("dal", "logic", "business") else "logic"

def _load_template(template_name: str) -> str:
    candidates = [
        Path(__file__).parent.parent / "prompts" / template_name,
        Path.cwd() / "cobolsmartgen" / "prompts" / template_name,
        Path.cwd() / "prompts" / template_name,
    ]
    
    for template_path in candidates:
        if template_path.exists():
            return fs.read_text(str(template_path))
    
    raise FileNotFoundError(f"Template not found: {template_name}")

def _render_template(template: str, variables: Dict[str, str]) -> str:
    result = template
    for key, value in variables.items():
        placeholder = "{" + key + "}"
        result = result.replace(placeholder, str(value))
    return result

def _extract_rag_rules(normalized_spec: Dict) -> str:
    rules = []
    for req in normalized_spec.get("exigences", []):
        if req.get("type") in ["business", "regle_metier"]:
            rule_text = req.get("regle", "")
            if rule_text:
                rules.append(f"- {req.get('id', '?')}: {rule_text}")
    
    nommage = normalized_spec.get("nommage", {})
    if nommage:
        rules.append(f"- Naming: variables={nommage.get('variables_case', 'UPPER_SNAKE')}, programs={nommage.get('programmes_case', 'UPPER-KEBAB')}")
    
    return "\n".join(rules) if rules else "No specific rules."

def _extract_requirements_json(normalized_spec: Dict) -> str:
    reqs = normalized_spec.get("exigences", [])
    return json.dumps(reqs, indent=2, ensure_ascii=False)

def _extract_io_for_entity(io_map: Dict, entity_name: str) -> str:
    for entity in io_map.get("entities", []):
        # Check both 'name' and 'entity_name' for compatibility
        if (entity.get("name") == entity_name or 
            entity.get("entity_name") == entity_name):
            return json.dumps(entity, indent=2, ensure_ascii=False)
    return "{}"

def _contains_forbidden_headers(text: str) -> bool:
    """Détecte si un snippet contient des divisions ou PROGRAM-ID (non souhaité pour une procédure)."""
    if not text:
        return False
    forbidden = (
        "IDENTIFICATION DIVISION",
        "ENVIRONMENT DIVISION",
        "DATA DIVISION",
        "PROCEDURE DIVISION",
        "PROGRAM-ID",
    )
    t = text.upper()
    return any(tok in t for tok in forbidden)


def _build_allowed_identifiers(io_map: Dict) -> set:
    """Construit la whitelist des identifiants COBOL autorisés à partir de io_map."""
    allowed = set()
    for entity in io_map.get("entities", []):
        for k in ("inputs", "outputs", "fields"):
            for item in entity.get(k, []) or []:
                name = item.get("cobol_name")
                if name:
                    allowed.add(name.upper())
    # Identifiants standards autorisés (retours / SQL)
    allowed.update({
        "END-OF-FILE",
        "SQLCA", "SQLCODE", "SQLERRM", "SQLERRML", "SQLERRMC", "SQLERRP",
        "RETURN-CODE", "WS-RETURN-CODE", "STATUS-CODE",
        "SQLSTATE", "SQLWARN", "SQLERRD", "SQLERRD-1", "SQLERRD-2", "SQLERRD-3",
    })
    return allowed


def _violates_identifier_policy(text: str, allowed_identifiers: set, allow_data: bool = True) -> bool:
    """
    Vérifie si le texte introduit des définitions de données non whitelistées ou des sections interdites.
    - allow_data=False : aucune ligne de niveau 01/05/77/78/88 ne doit apparaître (procédures).
    """
    upper = text.upper()
    if any(section in upper for section in ("FILE SECTION", "REPORT SECTION", "LINKAGE SECTION", "ENVIRONMENT DIVISION")):
        return True

    level_pattern = re.compile(r'^\\s*(0?[1-9][0-9]|77|78|88)\\s+([A-Z0-9\\-]+)', re.MULTILINE)
    for match in level_pattern.finditer(text):
        level_name = match.group(2).upper()
        if not allow_data:
            return True

        # Accepter les variables standards COBOL
        # WS-* : Working Storage (variables temporaires, constantes, flags)
        # DB-* : Variables pour SQL (DECLARE SECTION)
        if level_name.startswith('WS-') or level_name.startswith('DB-'):
            continue

        # Vérifier que la variable est dans allowed_identifiers
        if level_name not in allowed_identifiers:
            return True
    return False


def _read_snippet(path: Path, max_chars: int = 2000) -> str:
    """Read a file and return a trimmed snippet (for prompts)."""
    if not path or not path.exists():
        return ""
    try:
        content = path.read_text()
        return content[:max_chars]
    except Exception as exc:
        LOG.debug(f"Failed to read snippet from {path}: {exc}")
        return ""


def _find_entity_copybook(out_dir: Path, entity: str) -> Optional[Path]:
    if not entity:
        return None
    candidates = [
        out_dir / "copy" / f"{entity.upper()}-RECORD.cpy",
        out_dir / "copy" / f"{entity.capitalize()}-RECORD.cpy",
    ]
    for cand in candidates:
        if cand.exists():
            return cand
    return None


def _find_entity_sql(out_dir: Path, entity: str) -> Optional[Path]:
    if not entity:
        return None
    candidates = [
        out_dir / "sql" / "ddl" / f"{entity.lower()}.sql",
        out_dir / "sql" / "ddl" / f"{entity.upper()}__ddl.sql",
    ]
    for cand in candidates:
        if cand.exists():
            return cand
    return None


def _build_manual_interface_for_prior_layers(current_layer: str,
                                             current_entity: str,
                                             program_plan: Dict,
                                             contract: Dict,
                                             out_dir: Path) -> str:
    """
    Fallback quand aucun paragraphe n'est détecté dans les couches précédentes.
    Construit une interface textuelle avec les paragraphes/variables autorisés.
    """
    layers_order = ["dal", "logic", "business"]
    if current_layer not in layers_order:
        return ""
    idx = layers_order.index(current_layer)
    prior_layers = set(layers_order[:idx])
    if not prior_layers:
        return ""

    contract_procs = contract.get("allowed_procedures", {}) if contract else {}
    contract_vars = contract.get("allowed_variables", {}) if contract else {}
    parts = []

    for prog in program_plan.get("programs", []):
        layer = (prog.get("layer") or "").lower()
        if layer not in prior_layers:
            continue
        pid = prog.get("id") or prog.get("name", "PROGRAM")
        entity = prog.get("entity") or current_entity or "ENTITE"
        procs = contract_procs.get(pid, [])
        vars_ = contract_vars.get(pid, [])

        cb_path = _find_entity_copybook(out_dir, entity)
        cb_text = _read_snippet(cb_path) if cb_path else ""

        parts.append(f"=== PROGRAMME EXISTANT (couche {layer.upper()}): {pid} ===")
        parts.append("  Ne pas modifier ce programme, seulement l'appeler.")
        if procs:
            parts.append("Paragraphes disponibles : " + ", ".join(procs))
        if vars_:
            parts.append("Variables autorisées : " + ", ".join(vars_))
        if cb_text:
            parts.append("Structure COBOL :\n" + cb_text.strip())
        parts.append("")

    return "\n".join(parts).strip()


def _build_pipeline_notes(layer: str, entity: str, allowed_procs: List[str], allowed_vars: List[str],
                          copybook_text: str, sql_text: str) -> str:
    """Format pipeline guidance according to the checklist (SQL -> DAL -> Métier -> Logique)."""
    notes = [
        "Ordre pipeline: 1) SQL + structures -> 2) DAL -> 3) Métier (calcul) -> 4) Logique/Batch.",
        f"Tu es sur l'étape: {layer.upper()} pour {entity or 'ENTITÉ'}."
    ]

    if layer == "dal":
        notes.append("Réutilise EXACTEMENT les noms issus du SQL et du copybook ci-dessous.")
        if allowed_procs:
            notes.append(f"Paragraphes attendus: {', '.join(allowed_procs)}.")
        notes.append("Pas de logique métier; uniquement EXEC SQL / gestion curseur.")
    elif layer == "logic":
        notes.append("Implémente les règles métier sans EXEC SQL, uniquement calculs/validations.")
        if allowed_procs:
            notes.append(f"Procédures à générer: {', '.join(allowed_procs)}.")
    elif layer == "business":
        notes.append("Orchestre les couches existantes sans les modifier; appelle leurs paragraphes.")
        if allowed_procs:
            notes.append(f"Procédures d'orchestration: {', '.join(allowed_procs)}.")

    if allowed_vars:
        notes.append(f"Variables autorisées (contrat): {', '.join(allowed_vars)}.")

    return "\n".join(notes)


# NOTE: _format_employee_structure() removed (12 lines, dead code - only used by _simple_program_full)
# NOTE: _simple_program_full() removed (83 lines, dead code with CSG_SIMPLE_PROMPTS not set)


def _call_llm(prompt: str, system: str, config: Dict, timeout_s: int = 900) -> str:
    if "out_dir" not in config:
        config["out_dir"] = "out"
    
    response = llm_auto.generate(
        prompt=prompt,
        system=system,
        timeout_s=timeout_s,
        config=config
    )
    return response

def _looks_like_cobol(text: str) -> bool:
    if not text or len(text.strip()) < 20:
        return False
    t = text.upper()
    has_identification = "IDENTIFICATION DIVISION" in t
    has_structure = ("PROCEDURE DIVISION" in t or "DATA DIVISION" in t or "WORKING-STORAGE" in t)
    return has_identification and has_structure


def _build_expected_call_params(normalized_spec: Dict) -> Dict[str, List[List[str]]]:
    """
    Build expected CALL USING parameters per program.
    Returns dict: program_id -> list of acceptable name variants per param.
    Example: [["LK-OPERATION","OPERATION"], ["LK-END-OF-FILE","END-OF-FILE"], ...]
    """
    expected: Dict[str, List[List[str]]] = {}
    for prog in normalized_spec.get("programmes", []) or []:
        program_id = prog.get("name")
        if not program_id:
            continue
        call_iface = prog.get("call_interface") or {}
        params = call_iface.get("linkage_parameters", []) or []
        if not params:
            continue
        variants: List[List[str]] = []
        for p in params:
            name = (p.get("name") or "").strip().upper()
            if not name:
                continue
            names = [name]
            if name.startswith("LK-"):
                names.append(name[3:])
            variants.append(names)
        if variants:
            expected[program_id.upper()] = variants
    return expected


def _extract_call_blocks(text: str) -> List[Dict[str, str]]:
    """
    Extract CALL blocks from COBOL text.
    Returns list of dicts: {program: 'NAME', block: 'CALL ... END-CALL'}
    """
    lines = text.splitlines()
    blocks: List[Dict[str, str]] = []
    i = 0
    while i < len(lines):
        line = lines[i]
        match = re.search(r"\\bCALL\\s+['\\\"]?([A-Z0-9-]+)['\\\"]?", line, re.IGNORECASE)
        if not match:
            i += 1
            continue
        program = match.group(1).upper()
        block_lines = [line.strip()]
        i += 1
        while i < len(lines):
            l = lines[i].strip()
            block_lines.append(l)
            if "END-CALL" in l.upper() or l.strip() == ".":
                break
            i += 1
        blocks.append({"program": program, "block": " ".join(block_lines)})
        i += 1
    return blocks


def _validate_call_blocks(blocks: List[Dict[str, str]], expected_calls: Dict[str, List[List[str]]]) -> List[str]:
    errors: List[str] = []
    for b in blocks:
        program = b.get("program", "")
        block = b.get("block", "").upper()
        if program not in expected_calls:
            continue
        expected_params = expected_calls[program]
        missing = []
        for variants in expected_params:
            if not any(v in block for v in variants):
                missing.append("/".join(variants))
        if missing:
            errors.append(f"CALL {program}: missing params {missing}")
    return errors


def _validate_full_program_output(text: str, layer: str, expected_calls: Dict[str, List[List[str]]]) -> List[str]:
    errors: List[str] = []
    upper = text.upper()

    if upper.count("PROGRAM-ID.") != 1:
        errors.append("invalid PROGRAM-ID count (must be 1)")
    if upper.count("IDENTIFICATION DIVISION") != 1:
        errors.append("invalid IDENTIFICATION DIVISION count (must be 1)")
    if "+++++" in text:
        errors.append("output contains extra file marker '+++++'")

    # Prevent DATA/FILE sections after PROCEDURE DIVISION
    proc_idx = upper.find("PROCEDURE DIVISION")
    if proc_idx >= 0:
        tail = upper[proc_idx:]
        if "WORKING-STORAGE SECTION" in tail or "FILE SECTION" in tail:
            errors.append("DATA/FILE section appears after PROCEDURE DIVISION")

    if layer == "business":
        for frag in ("FILE SECTION", "FILE-CONTROL", "INPUT-OUTPUT SECTION"):
            if frag in upper:
                errors.append(f"forbidden section in BUSINESS: {frag}")
        if "STOP RUN" in upper:
            errors.append("STOP RUN forbidden in BUSINESS")
    if layer == "dal":
        for frag in ("COMPUTE", "MULTIPLY", "DIVIDE", "SUBTRACT", "ADD"):
            if frag in upper:
                errors.append(f"business logic forbidden in DAL: {frag}")

    # Validate CALL blocks against call interface
    call_blocks = _extract_call_blocks(text)
    if layer == "business" and call_blocks:
        errors.append("CALL statements forbidden in BUSINESS layer")
    errors.extend(_validate_call_blocks(call_blocks, expected_calls))
    return errors

def _clean_llm_response(response: str, allow_snippet: bool = False) -> str:
    """Nettoie la réponse du LLM pour extraire uniquement le code COBOL pur.

    allow_snippet: si True, renvoie le contenu nettoyé même sans IDENTIFICATION DIVISION
    (utile pour les procédures isolées générées par le LLM).
    """
    if not response:
        return ""

    # Étape 1: Supprimer les blocs markdown
    response = re.sub(r'^```(?:cobol)?\s*\n', '', response, flags=re.MULTILINE | re.IGNORECASE)
    response = re.sub(r'\n```\s*$', '', response, flags=re.MULTILINE)

    lines = response.split('\n')

    # Étape 2: Trouver le début du code COBOL (IDENTIFICATION DIVISION)
    cobol_start = -1
    for i, line in enumerate(lines):
        # Ignorer les lignes qui sont clairement des explications
        if any(phrase in line.lower() for phrase in ['voici', 'le code', 'ce code', 'respecte', 'implémente']):
            continue
        if re.match(r'^\s*(\*>)?\s*(IDENTIFICATION\s+DIVISION\.?)', line, re.IGNORECASE):
            cobol_start = i
            break

    if cobol_start == -1:
        # Pour les snippets (procédures), on peut accepter l'absence d'IDENTIFICATION
        if allow_snippet:
            return response.strip()
        LOG.warning("No IDENTIFICATION DIVISION found in response")
        return ""

    # Étape 3: Trouver la fin du code COBOL (dernière instruction valide avant les explications)
    cobol_end = len(lines)

    # Chercher la vraie fin du code (dernière ligne valide COBOL)
    in_cobol_section = False
    last_valid_cobol_line = cobol_start

    for i in range(cobol_start, len(lines)):
        line = lines[i].strip()

        # Ignorer les lignes vides
        if not line:
            continue

        # Détection d'explications textuelles (indicateurs que ce n'est plus du COBOL)
        # CORRECTION: Marqueurs plus spécifiques pour éviter de couper le code COBOL
        explanation_markers = [
            'ce code respecte',  # Au lieu de juste "ce code"
            'ce programme respecte',
            'cette implémentation respecte',
            'voici le code complet',  # Au lieu de juste "voici"
            'le code est production-ready',
            '1. **structure',  # Listes markdown avec contexte
            '2. **règles',
            '3. **conventions',
            '**compatibilité**',
            '**robustesse**',
            'immédiatement compilable et prêt',
            'prêt pour l\'intégration'
        ]

        line_lower = line.lower()
        # Stop if LLM started a second program or an artifact marker
        if i > cobol_start and re.match(r'^\s*IDENTIFICATION\s+DIVISION\b', line, re.IGNORECASE):
            cobol_end = i
            break
        if line.startswith("+++++"):
            cobol_end = i
            break
        if any(marker in line_lower for marker in explanation_markers):
            # CORRECTION: Seulement couper si on a au moins 10 lignes COBOL valides avant
            # Cela évite de couper le code à cause de commentaires ou d'explications introductives
            if in_cobol_section and (i - cobol_start) > 10:
                # On a trouvé le début des explications APRÈS le code
                cobol_end = i
                break
            # Sinon, ignorer ce marqueur (probablement dans un commentaire ou introduction)

        # Vérifier si c'est une ligne COBOL valide
        # CORRECTION: Élargir la détection pour inclure plus de constructions COBOL
        is_cobol_line = (
            line.startswith('*>') or  # Commentaire COBOL (inclut @ANCHOR)
            line.startswith('*') or   # Commentaire ancien style
            line.endswith('.') or     # Instruction se termine par .
            '@ANCHOR' in line.upper() or  # NOUVEAU: Ancres de génération
            'DIVISION' in line.upper() or
            'SECTION' in line.upper() or
            'PROCEDURE' in line.upper() or
            'PERFORM' in line.upper() or
            'MOVE' in line.upper() or
            'COMPUTE' in line.upper() or  # NOUVEAU: Calculs
            'CALL' in line.upper() or     # NOUVEAU: Appels de sous-programmes
            'DISPLAY' in line.upper() or
            'ACCEPT' in line.upper() or   # NOUVEAU: Lecture entrées
            'EXEC SQL' in line.upper() or
            'END-EXEC' in line.upper() or
            'GOBACK' in line.upper() or
            'STOP RUN' in line.upper() or # NOUVEAU
            'COPY' in line.upper() or     # NOUVEAU: Copybooks
            'LINKAGE' in line.upper() or  # NOUVEAU: Section de linkage
            'PIC ' in line.upper() or     # NOUVEAU: Définitions de données
            ' PIC ' in line.upper() or    # NOUVEAU: PIC avec espace avant
            'VALUE' in line.upper() or    # NOUVEAU: Valeurs initiales
            'REDEFINES' in line.upper() or # NOUVEAU
            'OCCURS' in line.upper() or   # NOUVEAU: Tables
            'DEPENDING' in line.upper() or # NOUVEAU
            'FD ' in line.upper() or      # NOUVEAU: File Description
            'SELECT' in line.upper() or   # NOUVEAU: File Control
            'OPEN' in line.upper() or     # NOUVEAU: Opérations fichiers
            'CLOSE' in line.upper() or    # NOUVEAU
            'READ' in line.upper() or     # NOUVEAU
            'WRITE' in line.upper() or    # NOUVEAU
            'REWRITE' in line.upper() or  # NOUVEAU
            'DELETE' in line.upper() or   # NOUVEAU
            'IF ' in line.upper() or      # NOUVEAU: Conditionnelles
            'ELSE' in line.upper() or     # NOUVEAU
            'END-IF' in line.upper() or   # NOUVEAU
            'EVALUATE' in line.upper() or # NOUVEAU
            'WHEN' in line.upper() or     # NOUVEAU
            'END-EVALUATE' in line.upper() or # NOUVEAU
            re.match(r'^\s*\d{2}\s+', line) or  # Niveau de données (01, 05, etc.)
            re.match(r'^\s*[A-Z][A-Z0-9\-]+', line)  # Nom de paragraphe
        )

        if is_cobol_line:
            last_valid_cobol_line = i
            in_cobol_section = True

        # CORRECTION: Forcer in_cobol_section après DATA/PROCEDURE DIVISION
        # Si on trouve ces divisions, on est forcément dans du code COBOL
        if 'DATA DIVISION' in line.upper() or 'PROCEDURE DIVISION' in line.upper():
            in_cobol_section = True

    # Utiliser la dernière ligne valide trouvée
    if in_cobol_section:
        cobol_end = last_valid_cobol_line + 1

    # Étape 4: Extraire uniquement les lignes COBOL
    cobol_lines = lines[cobol_start:cobol_end]

    # Étape 5: Filtrer les lignes invalides
    filtered_lines = []
    seen_identification = False

    for line in cobol_lines:
        # Éviter les duplications d'IDENTIFICATION DIVISION
        if re.match(r'^\s*(IDENTIFICATION\s+DIVISION\.?)', line, re.IGNORECASE):
            if seen_identification:
                continue
            seen_identification = True

        # Supprimer les marqueurs de prompt
        if any(marker in line for marker in ['--- RÉPONSE', 'Écris UNIQUEMENT', 'Commence EXACTEMENT', 'RÉPONSE LIBRE', 'RÉPONSE STRICTE']):
            continue

        filtered_lines.append(line)

    # Étape 6: Joindre et nettoyer les espaces multiples
    result = '\n'.join(filtered_lines)
    result = re.sub(r'\n{3,}', '\n\n', result)

    return result.strip()

def _format_cobol_code(code: str, dialect: str = "gnucobol") -> str:
    """
    Formate le code COBOL pour qu'il soit propre et correctement indenté.
    Pour GnuCOBOL en format free, assure une indentation cohérente.
    """
    if not code:
        return code

    lines = code.split('\n')
    formatted_lines = []

    indent_base = 7  # décaler tout le code (y compris la première ligne) pour respecter le retrait souhaité
    indent_size = 5   # indentation cohérente pour le free format
    indent_level = 0
    in_procedure = False

    def _is_comment(l: str) -> bool:
        return l.strip().startswith('*')

    def _is_paragraph(l: str) -> bool:
        stripped = l.strip()
        if not re.match(r'^[A-Z0-9][A-Z0-9\-]*\.$', stripped):
            return False
        # Exclure les mots-clés de fin de bloc (END-IF, END-PERFORM...)
        return not stripped.startswith('END-')

    def _data_indent(l: str) -> int:
        stripped = l.strip()
        if not stripped:
            return 0

        m = re.match(r'(\d{2})\s', stripped)
        if m:
            level = int(m.group(1))
            if level in (77, 78, 88):
                return 0
            return max(0, level // 5) * 2  # 01 → 0, 05 → 2, 10 → 4...
        # Instructions sans niveau (EXEC SQL, COPY, etc.)
        if stripped.startswith('EXEC SQL'):
            return 2
        return 0

    for raw_line in lines:
        line = raw_line.rstrip()

        if not line.strip():
            formatted_lines.append('')
            continue

        upper = line.strip().upper()

        # Détection des divisions / sections pour réinitialiser l'indentation
        if 'PROCEDURE DIVISION' in upper:
            in_procedure = True
            indent_level = 0
            formatted_lines.append(' ' * indent_base + line.strip())
            continue
        if 'DIVISION' in upper or 'SECTION' in upper:
            indent_level = 0
            formatted_lines.append(' ' * indent_base + line.strip())
            continue

        # Commentaires : alignés sur le niveau courant (procédure) sinon bruts
        if _is_comment(line):
            indent = indent_level * indent_size if in_procedure else 0
            formatted_lines.append(' ' * (indent_base + indent) + line.strip())
            continue

        # Indentation spécifique aux zones de données
        if not in_procedure:
            indent = _data_indent(line)
            formatted_lines.append(' ' * (indent_base + indent) + line.strip())
            continue

        # Procédure : gestion du niveau d'indentation
        is_paragraph = _is_paragraph(line)

        # Dé-décalage avant les fermetures / ELSE / WHEN
        dedent_tokens = ('END-IF', 'END-EVALUATE', 'END-PERFORM', 'END-EXEC', 'END-COMPUTE')
        if upper.startswith(dedent_tokens) or upper.startswith('ELSE') or upper.startswith('WHEN '):
            indent_level = max(0, indent_level - 1)

        current_indent = 0 if is_paragraph else indent_level * indent_size
        formatted_lines.append(' ' * (indent_base + current_indent) + line.strip())

        # Paragraphes : ré-indente les instructions qui suivent
        if is_paragraph:
            indent_level = 1
            continue

        # Ouvertures qui augmentent l'indentation
        if re.match(r'^(IF\b|ELSE\b|WHEN\b|EVALUATE\b)', upper):
            indent_level += 1
        elif re.match(r'^PERFORM\s+(UNTIL|VARYING)\b', upper):
            indent_level += 1
        elif upper.startswith('EXEC SQL') and 'END-EXEC' not in upper:
            indent_level += 1
        elif upper.startswith('ON SIZE ERROR'):
            indent_level += 1
        elif 'COMPUTE' in upper and 'ON SIZE ERROR' in upper and 'END-COMPUTE' not in upper:
            indent_level += 1

    # Forcer au moins 7 espaces sur la première ligne non vide
    for idx, l in enumerate(formatted_lines):
        if l.strip():
            if not l.startswith(' ' * 7):
                formatted_lines[idx] = (' ' * 7) + l.lstrip()
            break

    return '\n'.join(formatted_lines)

def _post_process_cobol(code: str, dialect: str = "gnucobol") -> str:
    """
    Post-traite le code COBOL pour s'assurer qu'il est propre et valide.
    - Supprime les explications résiduelles
    - Vérifie que le code commence et finit correctement
    - Formate les colonnes si nécessaire
    """
    if not code:
        return code

    lines = code.split('\n')
    clean_lines = []

    # Supprimer toute ligne qui ressemble à une explication
    for line in lines:
        line_lower = line.lower().strip()

        # Skip les lignes d'explications
        if any(marker in line_lower for marker in [
            'ce code', 'ce programme', 'cette implémentation',
            'respecte toutes', 'voici', 'le code suit',
            'exigences du projet', 'conventions de nommage',
            'règles métier', 'immédiatement compilable'
        ]):
            continue

        # Skip les listes numérotées markdown
        if re.match(r'^\d+\.\s+\*\*', line_lower):
            continue

        # Skip les titres markdown
        if line.strip().startswith('#'):
            continue

        clean_lines.append(line)

    code = '\n'.join(clean_lines)

    # CORRECTION: Supprimer le code stub résiduel (AUTO-GENERATED PROCEDURES, MAIN-SECTION)
    # Ce code stub apparaît souvent après le dernier GOBACK fonctionnel
    lines = code.split('\n')

    # Chercher le début du code stub dans tout le fichier
    stub_start_idx = -1
    for i in range(len(lines)):
        line_upper = lines[i].upper().strip()
        line_stripped = lines[i].strip()

        # Détecter les marqueurs de code stub
        if any(marker in line_upper for marker in [
            'AUTO-GENERATED PROCEDURES',
            'AUTO-GENERATED HEADER',
        ]):
            # Vérifier qu'il y a au moins 50 lignes avant (pour éviter les faux positifs)
            if i > 50:
                stub_start_idx = i
                break

        # Ou détecter (stub) dans un commentaire
        if '(STUB)' in line_upper and line_stripped.startswith('*'):
            if i > 50:
                stub_start_idx = i
                break

    # Si du code stub a été détecté, couper le code à cet endroit
    if stub_start_idx > 0:
        # Remonter pour supprimer aussi les lignes de séparation avant le stub
        while stub_start_idx > 0 and lines[stub_start_idx - 1].strip() in ['', '*> ' + '=' * 48]:
            stub_start_idx -= 1

        lines = lines[:stub_start_idx]
        LOG.info(f"Code stub résiduel détecté et supprimé à partir de la ligne {stub_start_idx}")

    code = '\n'.join(lines)

    # Formater le code
    code = _format_cobol_code(code, dialect)

    # Nettoyer les lignes vides excessives
    code = re.sub(r'\n{4,}', '\n\n\n', code)

    # Ne pas supprimer le leading newline éventuel (pour forcer le décalage de ligne)
    return code.rstrip()

def _save_generation_artifacts(out_dir: str, program_id: str, stage: str, prompt: str, raw_response: str, cleaned_response: str, final_code: str, meta: Dict):
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    run_id = os.environ.get("CSG_RUN_ID", timestamp)
    
    artifacts_dir = Path(out_dir) / "trace" / "generations" / run_id / program_id / stage
    fs.ensure_dir(artifacts_dir)
    
    fs.write_text(str(artifacts_dir / "01_prompt.txt"), prompt, atomic=True)
    fs.write_text(str(artifacts_dir / "02_raw_response.txt"), raw_response, atomic=True)
    fs.write_text(str(artifacts_dir / "03_cleaned_response.txt"), cleaned_response, atomic=True)
    fs.write_text(str(artifacts_dir / "04_final_code.cbl"), final_code, atomic=True)
    
    meta_data = {
        "program_id": program_id,
        "stage": stage,
        "timestamp": timestamp,
        "run_id": run_id,
        "model": os.getenv("LLM_MODEL", "unknown"),
        "prompt_length": len(prompt),
        "raw_response_length": len(raw_response),
        "cleaned_response_length": len(cleaned_response),
        "final_code_length": len(final_code),
        **meta
    }
    fs.write_json(str(artifacts_dir / "00_meta.json"), meta_data, sort_keys=True, atomic=True)
    
    LOG.info(f"Artifacts saved: {artifacts_dir}")


# NOTE: rewrite_headers_with_llm() removed (340 lines, dead code with CSG_USE_LLM_HEADERS=0)


def rewrite_procedures_with_llm(out_dir: str, config: Dict) -> List[str]:
    LOG.info("Starting LLM procedure generation")
    
    out = Path(out_dir)
    program_plan = fs.read_json(str(out / "program_plan.json"))
    normalized_spec = fs.read_json(str(out / "normalized_spec.json"))
    io_map = fs.read_json(str(out / "io_map.json"))
    
    template = _load_template("procedure_template.txt")
    dialecte = normalized_spec.get("dialecte_cobol", "gnucobol")
    allowed_identifiers = _build_allowed_identifiers(io_map)
    contract = {}
    contract_path = out / "architecture_contract.json"
    if contract_path.exists():
        contract = fs.read_json(str(contract_path))
    force_program_mode = os.getenv("CSG_LLM_PROGRAM_MODE", "0") == "1"

    # Configuration des modes de génération
    # CSG_USE_LLM_PROCS=1 : Force LLM pour TOUTES les procédures (ignore templates)
    # CSG_ENABLE_LLM_PROCS=0 : Templates uniquement, pas de LLM
    # Default : Templates si disponibles, LLM en fallback
    force_llm = os.getenv("CSG_USE_LLM_PROCS", "0") == "1"
    disable_llm = os.getenv("CSG_ENABLE_LLM_PROCS", "1") in ("0", "false", "False")

    if disable_llm and not force_llm:
        LOG.info("LLM procedure generation disabled - using templates only (CSG_ENABLE_LLM_PROCS=0)")
    elif force_llm:
        LOG.info("LLM procedure generation forced for all procedures (CSG_USE_LLM_PROCS=1)")
    else:
        LOG.info("Using templates when available, LLM as fallback")

    expected_calls = _build_expected_call_params(normalized_spec)
    updated_files = []
    
    for program in program_plan.get("programs", []):
        program_id = program.get("id") or program.get("name", "PROGRAM")
        layer = program.get("layer", "logic")
        entity = program.get("entity", "")
        
        target_file = out / _layer_folder(layer) / f"{program_id}.cbl"
        
        if not target_file.exists():
            LOG.warning(f"File not found for {program_id}: {target_file}")
            continue

        # Mode génération en un seul appel par programme
        if force_program_mode:
            # NOTE: simple_program_mode branch removed (dead code, CSG_SIMPLE_PROMPTS not used)

            LOG.info(f" Program-level generation (single call) for {program_id}")

            cb_path = _find_entity_copybook(out, entity)
            sql_path = _find_entity_sql(out, entity)
            copybook_text = _read_snippet(cb_path) if cb_path else ""
            sql_text = _read_snippet(sql_path) if sql_path else ""
            allowed_procs = contract.get("allowed_procedures", {}).get(program_id, [])
            allowed_vars = contract.get("allowed_variables", {}).get(program_id, [])
            io_json = _extract_io_for_entity(io_map, entity)
            try:
                io_data = json.loads(io_json) if isinstance(io_json, str) else io_json
            except Exception:
                io_data = io_json

            pipeline_notes = _build_pipeline_notes(
                layer=layer,
                entity=entity,
                allowed_procs=allowed_procs,
                allowed_vars=allowed_vars,
                copybook_text=copybook_text,
                sql_text=sql_text
            )

            exigences_list = normalized_spec.get("exigences", [])
            code_context = ""
            if os.getenv("CSG_INCLUDE_CODE_CONTEXT", "1") == "1":
                try:
                    code_context = code_context_builder.build_code_context(
                        output_dir=out,
                        current_layer=layer,
                        current_program=program_id
                    )
                except Exception as e:
                    LOG.warning(f"Failed to build code context for {program_id}: {e}")
            if not code_context:
                manual_ctx = _build_manual_interface_for_prior_layers(
                    current_layer=layer,
                    current_entity=entity,
                    program_plan=program_plan,
                    contract=contract,
                    out_dir=out
                )
                code_context = manual_ctx

            prompt = prompt_formatters.build_concise_prompt(
                program_id=program_id,
                layer=layer,
                io_data=io_data,
                exigences=exigences_list,
                contract=contract,
                normalized_spec=normalized_spec,
                dialecte=dialecte,
                code_context=code_context,
                pipeline_notes=pipeline_notes,
                copybook_context=copybook_text,
                sql_context=sql_text
            )

            system = "Expert COBOL. Generate production-ready code following the specifications exactly."
            response = _call_llm(prompt, system, config, timeout_s=int(os.getenv("CSG_LLM_TIMEOUT_S","600")))
            cleaned = _clean_llm_response(response, allow_snippet=False)
            cleaned = _post_process_cobol(cleaned, dialecte)

            errors = _validate_full_program_output(cleaned, layer, expected_calls)
            if not _looks_like_cobol(cleaned) or errors:
                if errors:
                    strict_prompt = (
                        prompt
                        + "\n\nLes erreurs suivantes ont ete detectees, corrige-les strictement:\n"
                        + "\n".join(f"- {e}" for e in errors)
                    )
                else:
                    strict_prompt = prompt
                response2 = _call_llm(strict_prompt, system, config, timeout_s=int(os.getenv("CSG_LLM_TIMEOUT_S","600")))
                cleaned2 = _clean_llm_response(response2, allow_snippet=False)
                cleaned2 = _post_process_cobol(cleaned2, dialecte)
                errors2 = _validate_full_program_output(cleaned2, layer, expected_calls)
                if not _looks_like_cobol(cleaned2) or errors2:
                    raise ValueError(f"LLM full program for {program_id} invalid after retry: {errors2}")
                response = response2
                cleaned = cleaned2

            fs.write_text(str(target_file), cleaned, atomic=True)
            trace.write_sidecar_hash(target_file)
            trace.write_meta(target_file, kind="program_llm", extra={
                "program_id": program_id,
                "layer": layer,
                "generated_by": "llm_full_program"
            })

            _save_generation_artifacts(
                out_dir=out_dir,
                program_id=program_id,
                stage="program_full",
                prompt=prompt,
                raw_response=response,
                cleaned_response=cleaned,
                final_code=cleaned,
                meta={"layer": layer, "mode": "full_program"}
            )

            updated_files.append(str(target_file))
            continue

        # NOTE: Procedure-by-procedure mode removed (dead code with CSG_LLM_PROGRAM_MODE=1)
    
    LOG.info(f"Procedure generation complete: {len(updated_files)} files")
    return updated_files

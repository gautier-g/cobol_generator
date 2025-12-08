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
        parts.append("⚠️  Ne pas modifier ce programme, seulement l'appeler.")
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


def _format_employee_structure(entity_fields: List[Dict]) -> List[str]:
    """Builds COBOL 01 EMPLOYEE lines from io_map fields."""
    lines = []
    lines.append("       01 EMPLOYEE.")
    for f in entity_fields:
        cobol_name = f.get("cobol_name", naming.to_cobol_name(f.get("name", "FIELD")))
        pic = f.get("cobol_type") or f.get("pic") or "X(30)"
        if not pic.upper().startswith("PIC"):
            pic = f"PIC {pic}"
        lines.append(f"           05 {cobol_name:<15} {pic}.")
    return lines


def _simple_program_full(layer: str,
                         program_id: str,
                         entity_fields: List[Dict],
                         out_file: Path) -> str:
    """Generate deterministic program (no LLM) matching the 8.2/8.3/8.4 targets."""
    ws_struct = "\n".join(_format_employee_structure(entity_fields))

    if layer == "dal":
        code = f"""       IDENTIFICATION DIVISION.
       PROGRAM-ID. {program_id}.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 END-OF-FILE PIC X VALUE 'N'.
{ws_struct}

       EXEC SQL
           DECLARE C_EMP CURSOR FOR
           SELECT EMP_ID, EMP_NAME, SALARY_BRUT, SALARY_NET
           FROM EMPLOYEE
       END-EXEC.

       PROCEDURE DIVISION.
       OPEN-CURSOR.
           EXEC SQL
               OPEN C_EMP
           END-EXEC.

       READ-EMPLOYEE.
           EXEC SQL
               FETCH C_EMP INTO :EMP-ID, :EMP-NAME, :SALARY-BRUT, :SALARY-NET
           END-EXEC
           IF SQLCODE = 100
               MOVE 'Y' TO END-OF-FILE
           END-IF.

       SAVE-EMPLOYEE.
           EXEC SQL
               UPDATE EMPLOYEE
               SET SALARY-NET = :SALARY-NET
               WHERE EMP_ID = :EMP-ID
           END-EXEC.
"""
    elif layer == "logic":
        code = f"""       IDENTIFICATION DIVISION.
       PROGRAM-ID. {program_id}.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 END-OF-FILE PIC X VALUE 'N'.
{ws_struct}

       PROCEDURE DIVISION.
       MAIN-PROCESS.
           PERFORM UNTIL END-OF-FILE = 'Y'
               PERFORM READ-EMPLOYEE
               IF END-OF-FILE = 'N'
                   PERFORM CALCULATE-NET
                   PERFORM SAVE-EMPLOYEE
               END-IF
           END-PERFORM
           STOP RUN.

       CALCULATE-NET.
           COMPUTE SALARY-NET = SALARY-BRUT * 0.7.
           *> Arrondi à deux décimales si nécessaire
"""
    else:  # business
        code = f"""       IDENTIFICATION DIVISION.
       PROGRAM-ID. {program_id}.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
{ws_struct}

       PROCEDURE DIVISION.
       DISPLAY-EMPLOYEE.
           DISPLAY "Employé:" EMP-NAME
           DISPLAY "Salaire Brut:" SALARY-BRUT
           DISPLAY "Salaire Net:" SALARY-NET.
"""
    fs.write_text(str(out_file), code, atomic=True)
    return code

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
# 
# def rewrite_headers_with_llm(out_dir: str, config: Dict) -> List[str]:
#     LOG.info("Starting LLM header generation with mAInframer-34b")
#     
#     out = Path(out_dir)
#     normalized_spec = fs.read_json(str(out / "normalized_spec.json"))
#     io_map = fs.read_json(str(out / "io_map.json"))
#     program_plan = fs.read_json(str(out / "program_plan.json"))
#     
#     template = _load_template("header_template.txt")
#     
#     dialecte = normalized_spec.get("dialecte_cobol", "gnucobol")
#     sql_cible = normalized_spec.get("sql_cible", "postgres")
#     nommage = json.dumps(normalized_spec.get("nommage", {}), indent=2)
#     rag_rules = _extract_rag_rules(normalized_spec)
#     exigences_json = _extract_requirements_json(normalized_spec)
#     date = datetime.now().strftime("%Y-%m-%d")
#     format_cobol = "free" if dialecte == "gnucobol" else "fixed"
#     
#     copybooks = ", ".join([cb.get("name", "") for cb in io_map.get("global_copybooks", [])])
#     if not copybooks:
#         copybooks = "STATUS-CODES"
#     
#     generated_files = []
#     
#     for program in program_plan.get("programs", []):
#         program_id = program.get("id") or program.get("name", "PROGRAM")
#         layer = program.get("layer", "logic")
#         entity = program.get("entity", "")
#         
#         LOG.info(f"Generating header for {program_id} (layer={layer})")
#         
#         io_json = _extract_io_for_entity(io_map, entity)
#         
#         variables = {
#             "program_id": program_id,
#             "couche": layer,
#             "dialecte": dialecte,
#             "sql_cible": sql_cible,
#             "nommage": nommage,
#             "io_json": io_json,
#             "exigences_json": exigences_json,
#             "rag_rules": rag_rules,
#             "format": format_cobol,
#             "date": date,
#             "copybooks": copybooks,
#         }
#         
#         prompt = _render_template(template, variables)
#         system = "You are an expert COBOL programmer. Generate clean, production-ready COBOL code."
#         
#         try:
#             LOG.info(f"LLM call for {program_id} (timeout=600s)")
#             response = _call_llm(prompt, system, config, timeout_s=int(os.getenv("CSG_LLM_TIMEOUT_S","600")))
#             LOG.info(f"LLM response: {len(response)} chars")
#             
#             cleaned = _clean_llm_response(response)
#             
#             if not _looks_like_cobol(cleaned):
#                 LOG.warning(f"First attempt for {program_id} invalid, retrying with strict prompt")
#                 
#                 strict_prompt = f"{prompt}\n\n--- RÉPONSE STRICTE ---\nGénérez UNIQUEMENT du code COBOL compilable.\nCommencez EXACTEMENT par:\nIDENTIFICATION DIVISION.\nPROGRAM-ID. {program_id}."
#                 
#                 response2 = _call_llm(strict_prompt, system, config, timeout_s=int(os.getenv("CSG_LLM_TIMEOUT_S","600")))
#                 cleaned2 = _clean_llm_response(response2)
#                 
#                 if _looks_like_cobol(cleaned2):
#                     LOG.info(f"Retry succeeded for {program_id}")
#                     response = response2
#                     cleaned = cleaned2
#                 else:
#                     raise ValueError(f"Failed to generate valid COBOL for {program_id} after retry")
#             
#             target_dir = out / _layer_folder(layer)
#             fs.ensure_dir(target_dir)
#             target_file = target_dir / f"{program_id}.cbl"
#             
#             existing_content = ""
#             if target_file.exists():
#                 existing_content = fs.read_text(str(target_file))
#             
#             final_code = cleaned
#             if existing_content and not existing_content.strip().startswith("IDENTIFICATION DIVISION"):
#                 final_code = cleaned + "\n\n" + existing_content
#             
#             fs.write_text(str(target_file), final_code, atomic=True)
#             trace.write_sidecar_hash(target_file)
#             trace.write_meta(target_file, kind="header_llm", extra={
#                 "program_id": program_id,
#                 "layer": layer,
#                 "model": os.getenv("LLM_MODEL", "mAInframer-34b"),
#                 "generated_by": "llm"
#             })
#             
#             _save_generation_artifacts(
#                 out_dir=out_dir,
#                 program_id=program_id,
#                 stage="header",
#                 prompt=prompt,
#                 raw_response=response,
#                 cleaned_response=cleaned,
#                 final_code=final_code,
#                 meta={"layer": layer}
#             )
#             
#             generated_files.append(str(target_file))
#             LOG.info(f"Generated: {target_file}")
#             
#         except Exception as e:
#             LOG.error(f"Failed for {program_id}: {e}", exc_info=True)
#             raise
#     
#     LOG.info(f"Header generation complete: {len(generated_files)} files")
#     return generated_files

def rewrite_headers_with_llm(out_dir: str, config: Dict) -> List[str]:
    LOG.info("Starting LLM header generation with Contract-Enforced Constraints")

    out = Path(out_dir)
    normalized_spec = fs.read_json(str(out / "normalized_spec.json"))
    io_map = fs.read_json(str(out / "io_map.json"))
    program_plan = fs.read_json(str(out / "program_plan.json"))
    allowed_identifiers = _build_allowed_identifiers(io_map)

    # Load contract if available
    contract = {}
    contract_path = out / "architecture_contract.json"
    if contract_path.exists():
        contract = fs.read_json(str(contract_path))
        LOG.info(f"   📋 Loaded contract from {contract_path}")
    
    template = _load_template("header_template.txt")
    
    dialecte = normalized_spec.get("dialecte_cobol", "gnucobol")
    sql_cible = normalized_spec.get("sql_cible", "postgres")
    nommage = json.dumps(normalized_spec.get("nommage", {}), indent=2)
    rag_rules = _extract_rag_rules(normalized_spec)
    exigences_json = _extract_requirements_json(normalized_spec)
    date = datetime.now().strftime("%Y-%m-%d")
    format_cobol = "free" if dialecte == "gnucobol" else "fixed"
    
    copybooks = ", ".join([cb.get("name", "") for cb in io_map.get("global_copybooks", [])])
    if not copybooks:
        copybooks = "STATUS-CODES"
    
    generated_files = []
    
    for program in program_plan.get("programs", []):
        program_id = program.get("id") or program.get("name", "PROGRAM")
        layer = program.get("layer", "logic")
        entity = program.get("entity", "")
        
        LOG.info(f"Generating header for {program_id} (layer={layer})")
        
        io_json = _extract_io_for_entity(io_map, entity)
        
        # ====== DÉBUT AJOUT: Instructions spécifiques par couche ======
        # REMPLACER TOUT LE DICTIONNAIRE layer_instructions PAR:

        # Instructions purement fonctionnelles - AUCUNE mention de structure COBOL
        layer_instructions = {
    "dal": """
RESPONSABILITÉS DE LA COUCHE DAL (Data Access Layer):
- Fournir un accès aux données de l'entité (lecture/écriture)
- Implémenter UNIQUEMENT les opérations de données demandées dans les exigences
- Utiliser UNIQUEMENT les champs définis dans io_json
- N'ajouter aucune logique métier (calculs, validations) - juste l'accès aux données
""",
    "logic": """
RESPONSABILITÉS DE LA COUCHE LOGIC (Application Layer):
- Implémenter la logique métier définie dans les règles métier
- Effectuer les calculs spécifiés dans les exigences
- Traiter les données en respectant les formules fournies
- Utiliser UNIQUEMENT les champs définis dans io_json
- N'ajouter aucune fonctionnalité non spécifiée
""",
    "business": """
RESPONSABILITÉS DE LA COUCHE BUSINESS (Presentation/Orchestration Layer):
- Orchestrer les appels aux autres couches si nécessaire
- Présenter les résultats selon les exigences
- Utiliser UNIQUEMENT les champs définis dans io_json
- N'ajouter aucun menu, rapport ou fonctionnalité non demandée
"""
}
#         layer_instructions = {
#             "dal": """
# INSTRUCTIONS SPÉCIFIQUES POUR LA COUCHE DAL (Data Access Layer) :
# 
# 1. WORKING-STORAGE doit contenir :
#    - 01 END-OF-FILE PIC X VALUE 'N'
#    - 01 EMPLOYEE avec les champs : EMP-ID, EMP-NAME, SALARY-BRUT, SALARY-NET
#    - EXEC SQL INCLUDE SQLCA END-EXEC
# 
# 2. Déclare un CURSOR pour lire la table EMPLOYEE :
#    EXEC SQL
#        DECLARE C-EMP CURSOR FOR
#        SELECT EMP_ID, EMP_NAME, SALARY_BRUT, SALARY_NET
#        FROM EMPLOYEE
#    END-EXEC
# 
# 3. PROCEDURE DIVISION doit inclure au minimum :
#    - OPEN-CURSOR : ouvre le curseur
#    - READ-EMPLOYEE : lit un enregistrement (FETCH)
#    - SAVE-EMPLOYEE : sauvegarde via UPDATE
#    - CLOSE-CURSOR : ferme le curseur
# 
# IMPORTANT : Ce programme doit gérer l'accès SQL à la base de données.
# """,
#             "logic": """
# INSTRUCTIONS SPÉCIFIQUES POUR LA COUCHE LOGIC (Application Layer) :
# 
# 1. WORKING-STORAGE doit contenir :
#    - 01 END-OF-FILE PIC X VALUE 'N'
#    - 01 EMPLOYEE (même structure que DAL)
#    - 01 WS-TEMP-RESULT PIC 9(6)V99 (pour calculs intermédiaires)
# 
# 2. PROCEDURE DIVISION - Logique principale :
#    MAIN-PROCEDURE.
#        PERFORM UNTIL END-OF-FILE = 'Y'
#            CALL 'EMPLOYEE-DAL' USING EMPLOYEE
#            IF END-OF-FILE = 'N'
#                PERFORM CALCULATE-NET
#                CALL 'EMPLOYEE-DAL' USING EMPLOYEE
#            END-IF
#        END-PERFORM
#        GOBACK.
# 
# 3. CALCULATE-NET doit implémenter la règle métier :
#    - Calcul : SALARY-NET = ROUND(SALARY-BRUT * 0.7, 2)
#    - Validation : Si SALARY-NET < 0, mettre à 0
#    - Afficher un WARNING si négatif
# 
# IMPORTANT : Ce programme contient la logique métier (calculs et validations).
# """,
#             "business": """
# INSTRUCTIONS SPÉCIFIQUES POUR LA COUCHE BUSINESS (Business Layer) :
# 
# 1. WORKING-STORAGE doit contenir :
#    - 01 EMPLOYEE (même structure que les autres couches)
#    - 01 WS-EMPLOYEE-COUNT PIC 9(4) VALUE ZERO (compteur)
# 
# 2. PROCEDURE DIVISION - Orchestration :
#    MAIN-PROCEDURE.
#        DISPLAY "Program {program_id} starting"
#        DISPLAY "======================================"
#        PERFORM PROCESS-ALL-EMPLOYEES
#        DISPLAY "======================================"
#        DISPLAY "Total employees processed: " WS-EMPLOYEE-COUNT
#        GOBACK.
# 
# 3. PROCESS-ALL-EMPLOYEES :
#    - Appelle EMPLOYEE-LOGIC via CALL
#    - Affiche les résultats (DISPLAY-EMPLOYEE)
#    - Incrémente le compteur
# 
# 4. DISPLAY-EMPLOYEE affiche :
#    - Nom de l'employé
#    - ID
#    - Salaire brut
#    - Salaire net calculé
# 
# IMPORTANT : Ce programme orchestre et affiche les résultats.
# """
#         }
        
        # Récupérer les instructions pour la couche courante
        layer_specific = layer_instructions.get(layer, "")
        # ====== FIN AJOUT ======

        # NOUVEAU SYSTÈME: Utiliser les formatters pour prompts concis
        use_concise_prompts = os.getenv("CSG_USE_CONCISE_PROMPTS", "1") == "1"

        if use_concise_prompts:
            # Système concis avec formatters
            try:
                io_data = json.loads(io_json) if isinstance(io_json, str) else io_json
            except:
                io_data = {}

            try:
                exigences_list = json.loads(exigences_json) if isinstance(exigences_json, str) else []
            except:
                exigences_list = []

            # NOUVEAU: Construire le contexte de code déjà généré (couches précédentes)
            code_context = ""
            if os.getenv("CSG_INCLUDE_CODE_CONTEXT", "1") == "1":
                try:
                    code_context = code_context_builder.build_code_context(
                        output_dir=Path(out_dir),
                        current_layer=layer,
                        current_program=program_id
                    )
                    if code_context:
                        LOG.info(f"📋 Including code context from previous layers ({len(code_context)} chars)")
                except Exception as e:
                    LOG.warning(f"Failed to build code context: {e}")
            if not code_context:
                manual_ctx = _build_manual_interface_for_prior_layers(
                    current_layer=layer,
                    current_entity=entity,
                    program_plan=program_plan,
                    contract=contract,
                    out_dir=out
                )
                code_context = manual_ctx

            # Contexte structure / SQL pour respecter l'ordre pipeline
            cb_path = _find_entity_copybook(out, entity)
            sql_path = _find_entity_sql(out, entity)
            copybook_text = _read_snippet(cb_path) if cb_path else ""
            sql_text = _read_snippet(sql_path) if sql_path else ""

            allowed_procs = contract.get("allowed_procedures", {}).get(program_id, [])
            allowed_vars = contract.get("allowed_variables", {}).get(program_id, [])
            pipeline_notes = _build_pipeline_notes(
                layer=layer,
                entity=entity,
                allowed_procs=allowed_procs,
                allowed_vars=allowed_vars,
                copybook_text=copybook_text,
                sql_text=sql_text
            )

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

        else:
            # Ancien système avec template
            allowed_vars = contract.get("allowed_variables", {}).get(program_id, [])
            allowed_procs = contract.get("allowed_procedures", {}).get(program_id, [])
            forbidden_patterns = contract.get("forbidden_patterns", [])

            # Formater les contraintes pour le prompt
            contract_constraints = ""
            if contract:
                contract_constraints = f"""
CONTRAINTES DU CONTRAT (OBLIGATOIRES):
- Variables autorisées UNIQUEMENT: {', '.join(allowed_vars[:10]) if allowed_vars else 'Voir io_json'}
{f'  (+ {len(allowed_vars) - 10} autres)' if len(allowed_vars) > 10 else ''}
- Procédures autorisées: {', '.join(allowed_procs[:5]) if allowed_procs else 'À définir selon exigences'}
{f'  (+ {len(allowed_procs) - 5} autres)' if len(allowed_procs) > 5 else ''}
- Patterns interdits: {', '.join(forbidden_patterns) if forbidden_patterns else 'Aucun'}

⚠️  N'ajouter AUCUNE variable non listée dans io_json
⚠️  N'ajouter AUCUNE procédure non demandée dans les exigences
"""

            variables = {
                "program_id": program_id,
                "couche": layer,
                "layer_instructions": layer_specific,
                "contract_constraints": contract_constraints,
                "dialecte": dialecte,
                "sql_cible": sql_cible,
                "nommage": nommage,
                "io_json": io_json,
                "exigences_json": exigences_json,
                "rag_rules": rag_rules,
                "format": format_cobol,
                "date": date,
                "copybooks": copybooks,
            }

            prompt = _render_template(template, variables)
            system = "You are an expert COBOL programmer. Generate clean, production-ready COBOL code."
        
        response = ""
        cleaned = ""
        final_code = ""
        success = False
        
        try:
            # ====== MODIFICATION 3: Améliorer les logs avant l'appel LLM ======
            LOG.info(f"🤖 ═══════════════════════════════════════════════════")
            LOG.info(f"🤖 Generating header for {program_id} (layer={layer})")
            LOG.info(f"🤖 Prompt length: {len(prompt)} chars")
            LOG.info(f"🤖 Timeout: 600s (please wait, LLM is thinking...)")
            LOG.info(f"🤖 ═══════════════════════════════════════════════════")
            
            start_time = time.time()
            response = _call_llm(prompt, system, config, timeout_s=int(os.getenv("CSG_LLM_TIMEOUT_S","600")))
            elapsed = time.time() - start_time
            
            LOG.info(f"✅ LLM response received: {len(response)} chars in {elapsed:.1f}s")
            
            if len(response) < 100:
                LOG.warning(f"⚠️  Response suspiciously short: {response[:200]}")
            # ====== FIN MODIFICATION 3 ======
            
            # AJOUTÉ : Tracer l'appel LLM (partie existante)
            pipeline_tracer.save_llm_call(
                program_id=program_id,
                stage="header_attempt1",
                prompt=prompt,
                response=response,
                metadata={
                    "model": os.getenv("LLM_MODEL"),
                    "temperature": os.getenv("LLM_TEMPERATURE"),
                    "response_length": len(response),
                    "request": {
                        "system": system,
                        "prompt_length": len(prompt),
                    }
                }
            )
            
            cleaned = _clean_llm_response(response)

            # NOUVEAU : Post-traiter pour supprimer les explications résiduelles
            cleaned = _post_process_cobol(cleaned, dialecte)

            # Rejeter si le code ajoute des identifiants ou sections non autorisés
            if _violates_identifier_policy(cleaned, allowed_identifiers, allow_data=True):
                raise ValueError(f"LLM header for {program_id} introduced forbidden sections or identifiers")

            # AJOUTÉ : Sauvegarder le cleaned
            pipeline_tracer.save_intermediate(
                f"llm_calls/{program_id}/header_attempt1/04_cleaned.txt",
                cleaned
            )

            if not _looks_like_cobol(cleaned):
                # ====== MODIFICATION 4: Améliorer le retry avec logs ======
                LOG.warning(f"⚠️  First attempt for {program_id} invalid (no valid code structure detected)")
                LOG.warning(f"⚠️  Response was: {response[:200]}...")
                LOG.info(f"🔁 Retrying with stricter prompt...")

                strict_prompt = f"""Generate ONLY COBOL code for program {program_id} (layer: {layer}).

Requirements:
- Generate a complete COBOL program
- Use MINIMAL code: only variables from the spec/io_map provided
- Do NOT add extra features, variables, or functionalities not requested in specifications
- Follow the layer-specific responsibilities for {layer} layer
- NO explanations, NO markdown, ONLY compilable COBOL code

Begin now:"""
                
                LOG.info(f"🤖 Retry prompt length: {len(strict_prompt)} chars")
                start_retry = time.time()
                response2 = _call_llm(strict_prompt, system, config, timeout_s=int(os.getenv("CSG_LLM_TIMEOUT_S","600")))
                elapsed_retry = time.time() - start_retry
                LOG.info(f"✅ Retry response: {len(response2)} chars in {elapsed_retry:.1f}s")
                # ====== FIN MODIFICATION 4 ======
                
                # AJOUTÉ : Tracer le retry (partie existante)
                pipeline_tracer.save_llm_call(
                    program_id=program_id,
                    stage="header_retry",
                    prompt=strict_prompt,
                    response=response2,
                    metadata={
                        "model": os.getenv("LLM_MODEL"),
                        "retry": True,
                        "response_length": len(response2),
                    }
                )
                
                cleaned2 = _clean_llm_response(response2)

                # NOUVEAU : Post-traiter le retry aussi
                cleaned2 = _post_process_cobol(cleaned2, dialecte)

                # AJOUTÉ
                pipeline_tracer.save_intermediate(
                    f"llm_calls/{program_id}/header_retry/04_cleaned.txt",
                    cleaned2
                )
                
                if _looks_like_cobol(cleaned2) and not _violates_identifier_policy(cleaned2, allowed_identifiers, allow_data=True):
                    LOG.info(f"Retry succeeded for {program_id}")
                    response = response2
                    cleaned = cleaned2
                else:
                    raise ValueError(f"Failed to generate valid COBOL for {program_id} after retry")
            
            target_dir = out / _layer_folder(layer)
            fs.ensure_dir(target_dir)
            target_file = target_dir / f"{program_id}.cbl"

            existing_content = ""
            if target_file.exists():
                existing_content = fs.read_text(str(target_file))

            final_code = cleaned
            if existing_content and not existing_content.strip().startswith("IDENTIFICATION DIVISION"):
                final_code = cleaned + "\n\n" + existing_content

            # NOUVEAU : Dernière passe de nettoyage sur le code final
            final_code = _post_process_cobol(final_code, dialecte)
            
            fs.write_text(str(target_file), final_code, atomic=True)
            trace.write_sidecar_hash(target_file)
            trace.write_meta(target_file, kind="header_llm", extra={
                "program_id": program_id,
                "layer": layer,
                "model": os.getenv("LLM_MODEL", "mAInframer-34b"),
                "generated_by": "llm"
            })
            
            _save_generation_artifacts(
                out_dir=out_dir,
                program_id=program_id,
                stage="header",
                prompt=prompt,
                raw_response=response,
                cleaned_response=cleaned,
                final_code=final_code,
                meta={"layer": layer}
            )
            
            generated_files.append(str(target_file))
            LOG.info(f"Generated: {target_file}")
            success = True
            
        except Exception as e:
            LOG.error(f"Failed for {program_id}: {e}", exc_info=True)
            raise
    
    LOG.info(f"Header generation complete: {len(generated_files)} files")
    return generated_files
    
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
    simple_program_mode = os.getenv("CSG_SIMPLE_PROMPTS", "0") == "1"

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
            # Mode simplifié sans LLM : génère directement le code cible 8.2/8.3/8.4
            if simple_program_mode:
                # Pour DAL, ajuster le nom si besoin
                target_pid = program_id
                if layer == "dal" and not program_id.upper().endswith("-DB"):
                    target_pid = f"{program_id}-DB"

                # Récupérer les champs COBOL de l'entité
                entity_fields = []
                for ent in io_map.get("entities", []):
                    if ent.get("name") == entity or ent.get("entity_name") == entity:
                        entity_fields = ent.get("inputs", []) + ent.get("outputs", [])
                        break
                # Suppression des doublons par nom
                seen = set()
                uniq_fields = []
                for f in entity_fields:
                    name = f.get("cobol_name", f.get("name"))
                    if name and name not in seen:
                        uniq_fields.append(f)
                        seen.add(name)

                code = _simple_program_full(layer, target_pid, uniq_fields, target_file)
                trace.write_sidecar_hash(target_file)
                trace.write_meta(target_file, kind="program_simple", extra={
                    "program_id": target_pid,
                    "layer": layer,
                    "generated_by": "simple_template"
                })
                _save_generation_artifacts(
                    out_dir=out_dir,
                    program_id=target_pid,
                    stage="program_full_simple",
                    prompt="(simple template, no LLM)",
                    raw_response=code,
                    cleaned_response=code,
                    final_code=code,
                    meta={"layer": layer, "mode": "simple_program"}
                )
                updated_files.append(str(target_file))
                continue

            LOG.info(f"🤖 Program-level generation (single call) for {program_id}")

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

            if not _looks_like_cobol(cleaned):
                raise ValueError(f"LLM full program for {program_id} is not valid COBOL")

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
        
        content = fs.read_text(str(target_file))
        procedures_section = "\n\n      *> ===== GENERATED PROCEDURES =====\n"
        has_procedures = False
        
        for operation in program.get("procedures", []):
            proc_name = operation.get("name", "UNKNOWN")
            proc_desc = operation.get("description", "")

            # Vérifier si un template existe pour cette procédure
            has_proc_template = procedure_templates.has_template(proc_name)
            use_template = has_proc_template and not force_llm

            if use_template:
                LOG.info(f"✅ Using template for {proc_name} in {program_id}")
            elif has_proc_template and force_llm:
                LOG.info(f"🤖 Template exists for {proc_name}, but forcing LLM (CSG_USE_LLM_PROCS=1)")
            elif disable_llm:
                LOG.warning(f"⚠️  No template for {proc_name}, LLM disabled - using minimal template")
                use_template = True  # Force minimal template
            else:
                LOG.info(f"🤖 No template for {proc_name}, using LLM fallback")

            cleaned = ""
            response = ""

            # Si on utilise un template, l'obtenir directement
            if use_template:
                cleaned = procedure_templates.get_procedure_template(proc_name)
                # Formatter le template
                cleaned = _post_process_cobol(cleaned, dialecte)

                # Pas besoin d'appel LLM, passer directement à l'insertion
                if cleaned and len(cleaned.strip()) > 10:
                    procedures_section += f"\n          {proc_name}.\n"
                    procedures_section += f"        *>  {proc_desc}\n"
                    procedures_section += cleaned + "\n           .\n"
                    has_procedures = True

                    # Log de traçabilité
                    LOG.info(f"   ✅ Template applied for {proc_name}")

                # Continuer avec la procédure suivante
                continue

            # Sinon, utiliser LLM (code existant)
            LOG.info(f"Generating procedure {proc_name} with LLM for {program_id}")

            # Sécuriser inputs/outputs: convertir tout élément en str pour éviter les dict non sérialisables
            safe_inputs = [str(x) for x in operation.get("inputs", [])]
            safe_outputs = [str(x) for x in operation.get("outputs", [])]

            cb_path = _find_entity_copybook(out, entity)
            sql_path = _find_entity_sql(out, entity)
            copybook_text = _read_snippet(cb_path) if cb_path else ""
            sql_text = _read_snippet(sql_path) if sql_path else ""
            allowed_procs = contract.get("allowed_procedures", {}).get(program_id, [])
            allowed_vars = contract.get("allowed_variables", {}).get(program_id, [])
            pipeline_notes = _build_pipeline_notes(
                layer=layer,
                entity=entity,
                allowed_procs=allowed_procs,
                allowed_vars=allowed_vars,
                copybook_text=copybook_text,
                sql_text=sql_text
            )

            variables = {
                "program_id": program_id,
                "procedure_name": proc_name,
                "procedure_type": operation.get("type", "GENERAL"),
                "procedure_description": proc_desc,
                "inputs": ", ".join(safe_inputs),
                "outputs": ", ".join(safe_outputs),
                "requirement": "",
                "dialecte": dialecte,
                "rag_examples": "Standard COBOL patterns",
            }

            prompt = _render_template(template, variables)

            # Inject pipeline notes + structures/SQL en préambule pour guider le LLM
            preface_parts = []
            if pipeline_notes:
                preface_parts.append(pipeline_notes)
            if copybook_text:
                preface_parts.append("Structure COBOL (référence):\n" + copybook_text.strip())
            if sql_text and layer == "dal":
                preface_parts.append("SQL DDL (référence noms de colonnes):\n" + sql_text.strip())
            if preface_parts:
                prompt = "\n\n".join(preface_parts) + "\n\n" + prompt

            system = "Generate concise COBOL procedure code based strictly on the provided specifications."

            try:
                response = _call_llm(prompt, system, config, timeout_s=int(os.getenv("CSG_LLM_TIMEOUT_S","600")))
                
                # AJOUTÉ : Tracer l'appel LLM pour procedure
                pipeline_tracer.save_llm_call(
                    program_id=program_id,
                    stage=f"procedure_{proc_name}_attempt1",
                    prompt=prompt,
                    response=response,
                    metadata={
                        "model": os.getenv("LLM_MODEL"),
                        "procedure": proc_name,
                        "response_length": len(response),
                    }
                )
                
                cleaned = _clean_llm_response(response, allow_snippet=True)
                cleaned = _post_process_cobol(cleaned, dialecte)

                # Rejeter si le snippet contient des divisions/PROGRAM-ID ou des variables non autorisées
                if cleaned and (_contains_forbidden_headers(cleaned) or _violates_identifier_policy(cleaned, allowed_identifiers, allow_data=False)):
                    LOG.warning(f"⚠️  {proc_name}: forbidden headers detected in response, retrying with strict prompt")
                    cleaned = ""

                if not cleaned or len(cleaned.strip()) < 10:
                    LOG.warning(f"First attempt for {proc_name} invalid, retrying")

                    strict_prompt = f"""{prompt}

--- RÉPONSE STRICTE ---
Générez UNIQUEMENT le code de la procédure {proc_name}.
Utilisez SEULEMENT les variables spécifiées dans les entrées/sorties.
N'ajoutez AUCUNE nouvelle donnée ou fonctionnalité."""
                    
                    response2 = _call_llm(strict_prompt, system, config, timeout_s=int(os.getenv("CSG_LLM_TIMEOUT_S","600")))
                    cleaned2 = _clean_llm_response(response2, allow_snippet=True)
                    cleaned2 = _post_process_cobol(cleaned2, dialecte)

                    # Rejeter si le snippet contient des divisions/PROGRAM-ID ou des variables non autorisées
                    if cleaned2 and (_contains_forbidden_headers(cleaned2) or _violates_identifier_policy(cleaned2, allowed_identifiers, allow_data=False)):
                        LOG.warning(f"⚠️  {proc_name}: forbidden headers detected after retry, discarding snippet")
                        cleaned2 = ""
                    
                    if cleaned2 and len(cleaned2.strip()) > 10:
                        response = response2
                        cleaned = cleaned2
                
                if cleaned and len(cleaned.strip()) > 10:
                    procedures_section += f"\n          {proc_name}.\n"
                    procedures_section += f"        *>  {proc_desc}\n"
                    procedures_section += cleaned + "\n           .\n"
                    has_procedures = True
                    
                    _save_generation_artifacts(
                        out_dir=out_dir,
                        program_id=program_id,
                        stage=f"procedure_{proc_name}",
                        prompt=prompt,
                        raw_response=response,
                        cleaned_response=cleaned,
                        final_code=cleaned,
                        meta={"procedure": proc_name, "layer": layer}
                    )
                
            except Exception as e:
                LOG.error(f"Failed procedure {proc_name}: {e}")
                continue
        
        if has_procedures:
            final_content = content + procedures_section
            final_content = _post_process_cobol(final_content, dialecte)
            fs.write_text(str(target_file), final_content, atomic=True)
            trace.write_sidecar_hash(target_file)
            trace.write_meta(target_file, kind="procedures_llm", extra={
                "program_id": program_id,
                "layer": layer,
                "generated_by": "llm"
            })
            updated_files.append(str(target_file))
            LOG.info(f"Updated: {target_file}")
    
    LOG.info(f"Procedure generation complete: {len(updated_files)} files")
    return updated_files

# file: cobolsmartgen/generate/cobol_procedures.py
"""
COBOL procedure generation - LLM-only from specs.
NO static templates, NO code examples.
"""
from __future__ import annotations
import logging
import os
from pathlib import Path
from typing import Dict, List

from ..utils import fs, trace
from cobolsmartgen.adapters import llm_auto
import json

LOG = logging.getLogger(__name__)

def _layer_folder(layer: str) -> str:
    l = (layer or "logic").lower()
    return l if l in ("dal", "logic", "business") else "logic"

def _ensure_file(path: Path, program_id: str, layer: str) -> None:
    """Crée UNIQUEMENT le header minimal sans PROCEDURE DIVISION."""
    if path.exists():
        return

    # Header minimal - AUCUN code de procédure statique
    header = f"""       IDENTIFICATION DIVISION.
       PROGRAM-ID. {program_id}.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
"""
    path.parent.mkdir(parents=True, exist_ok=True)
    fs.write_text(str(path), header, atomic=True)
    trace.write_sidecar_hash(path)
    trace.write_meta(path, kind="header", extra={"program_id": program_id, "layer": layer})

def _generate_static_procedures(config: Dict, out_dir: str) -> List[str]:
    """Crée UNIQUEMENT les fichiers vides - AUCUN code statique."""
    out = Path(out_dir)
    plan_p = out / "program_plan.json"
    if not plan_p.exists():
        raise ValueError(f"Program plan not found: {plan_p}")
    plan = fs.read_json(str(plan_p))

    touched: List[str] = []
    for item in plan.get("programs", []):
        pid = item.get("id") or item.get("name") or "PROGRAM"
        layer = item.get("layer") or "logic"
        target = out / _layer_folder(layer) / f"{pid}.cbl"

        # Créer UNIQUEMENT le fichier header vide
        _ensure_file(target, pid, layer)
        touched.append(str(target))

    LOG.info(f"Empty headers created: {len(touched)} files (no static code)")
    return touched

def _run(config: Dict, out_dir: str) -> List[str]:
    """Point d'entrée - Force l'utilisation du LLM."""
    # Créer les fichiers vides
    touched = _generate_static_procedures(config, out_dir)

    # Mode strict 8.2-8.5 : un appel LLM par programme avec prompts dédiés
    if os.getenv("CSG_STRICT_825", "0") == "1":
        LOG.info("CSG_STRICT_825=1 -> génération LLM une fois par programme (8.2/8.3/8.4)")
        return _generate_strict_programs(config, out_dir)

    use_llm = os.environ.get("CSG_USE_LLM_PROCS", "1")  # Activé par défaut

    if use_llm == "0":
        LOG.warning("⚠️  LLM procedure generation DISABLED - files will be empty!")
        LOG.warning("⚠️  Set CSG_USE_LLM_PROCS=1 to generate code from specs")
        return touched

    LOG.info("🤖 LLM procedure generation from specs ONLY (no static templates)...")

    try:
        from . import _llm_codegen
        llm_files = _llm_codegen.rewrite_procedures_with_llm(out_dir, config)
        LOG.info(f"✅ LLM generated {len(llm_files)} files from specs")
        return llm_files
    except Exception as e:
        LOG.error(f"❌ LLM procedure generation failed: {e}")
        LOG.warning("⚠️  Files remain empty (no fallback to static code)")
        return touched

def run(*args, **kwargs):
    from pathlib import Path as _P
    config = kwargs.get("config")
    out_dir = kwargs.get("out_dir")

    for a in args:
        if isinstance(a, dict) and config is None:
            config = a
            break

    for a in reversed(args):
        if isinstance(a, (str, _P)) and out_dir is None:
            out_dir = str(a)
            break

    if config is None:
        config = {}
    if out_dir is None:
        out_dir = "out"

    return _run(config, out_dir)


def _generate_strict_programs(config: Dict, out_dir: str) -> List[str]:
    """Génère exactement 8.2/8.3/8.4 via LLM (un appel par programme)."""
    out = Path(out_dir)
    plan = fs.read_json(str(out / "program_plan.json"))
    norm = fs.read_json(str(out / "normalized_spec.json"))
    io_map = fs.read_json(str(out / "io_map.json"))
    contract = {}
    contract_path = out / "architecture_contract.json"
    if contract_path.exists():
        contract = fs.read_json(str(contract_path))

    written = []
    for program in plan.get("programs", []):
        # Align program_id with YAML "programmes" when possible (source de vérité)
        program_id = program.get("id") or program.get("name", "PROGRAM")
        layer = program.get("layer", "logic")
        entity = program.get("entity", "EMPLOYEE")

        # If the planner used legacy ids, override with YAML programme name on same layer/entity
        for prog_yaml in norm.get("programmes", []):
            if prog_yaml.get("layer") == layer and entity in prog_yaml.get("entities", [entity]):
                program_id = prog_yaml.get("name", program_id)
                break

        target = out / _layer_folder(layer) / f"{program_id}.cbl"

        prompt_base = _build_strict_prompt(
            program_id=program_id,
            layer=layer,
            entity=entity,
            norm=norm,
            io_map=io_map,
            contract=contract,
            plan=plan,
        )
        system = "Expert COBOL. Génère uniquement le code COBOL demandé."
        response, final_prompt = _generate_with_validation(
            prompt_base=prompt_base,
            system=system,
            program_id=program_id,
            layer=layer,
            entity=entity,
            norm=norm,
            config=config,
        )

        fs.write_text(str(target), response, atomic=True)
        trace.write_sidecar_hash(target)
        trace.write_meta(target, kind="program_full_llm_strict", extra={
            "program_id": program_id,
            "layer": layer
        })
        written.append(str(target))

        # Sauvegarde de la trace
        art_dir = out / "trace" / "generations" / "strict_825" / program_id
        art_dir.mkdir(parents=True, exist_ok=True)
        fs.write_text(str(art_dir / "01_prompt.txt"), final_prompt, atomic=True)
        fs.write_text(str(art_dir / "02_raw_response.txt"), response, atomic=True)

    return written


def _clean_markdown_cobol(text: str) -> str:
    """Retire les ``` éventuels et trim."""
    if "```" not in text:
        return text.strip()
    import re
    m = re.search(r"```[a-zA-Z]*\n(.*?)```", text, re.S)
    if m:
        return m.group(1).strip()
    return text.replace("```", "").strip()


def _generate_with_validation(prompt_base: str, system: str, program_id: str, layer: str,
                              entity: str, norm: Dict, config: Dict) -> (str, str):
    """Gen LLM puis valide; si échec, renvoie avec message correctif (1 retry)."""
    last_prompt = prompt_base
    for attempt in range(2):
        resp = llm_auto.generate(prompt=last_prompt, system=system, config=config)
        clean = _clean_markdown_cobol(resp)
        errors = _validate_program(clean, program_id, layer, entity, norm)
        if not errors:
            return clean, last_prompt
        # Ajout des erreurs en correctif
        corrections = "\n".join(f"- {e}" for e in errors)
        last_prompt = f"""{prompt_base}

Les erreurs suivantes ont été détectées, corrige-les strictement sans ajouter de texte explicatif:
{corrections}"""
    # Retourne dernier résultat (même si erreurs) pour inspection
    return clean, last_prompt


def _build_strict_prompt(program_id: str, layer: str, entity: str, norm: Dict, io_map: Dict,
                         contract: Dict = None, plan: Dict = None) -> str:
    """Construit un prompt contractuel (pas de code donné) pour 8.2/8.3/8.4 en lisant la spec YAML normalisée."""
    # Infos programme (sections, call interface, curseur principal)
    prog_info = {}
    for prog in norm.get("programmes", []):
        if prog.get("name") == program_id:
            prog_info = prog
            break
    # Extraire l'entité et ses attributs (avec cobol_pic)
    entity_fields = {}
    for ent in norm.get("mcd", {}).get("entites", []):
        if ent.get("name") == entity or ent.get("normalized_name") == entity:
            entity_fields = ent
            break

    fields_lines = []
    for attr in entity_fields.get("attrs", []):
        name = attr.get("name", "").replace("_", "-")
        pic = attr.get("cobol_pic") or attr.get("pic") or attr.get("cobol_type")
        if not pic:
            # fallback mapping simple
            t = attr.get("type", "").upper()
            if "INT" in t:
                pic = "9(4)"
            elif "VARCHAR" in t:
                import re
                m = re.search(r'VARCHAR\((\d+)\)', t)
                l = m.group(1) if m else "30"
                pic = f"A({l})"
            elif "DECIMAL" in t:
                pic = "9(6)V99"
            else:
                pic = "X(30)"
        if not pic.upper().startswith("PIC"):
            pic = f"PIC {pic}"
        fields_lines.append(f"    05 {name:<15} {pic}.")
    employee_struct = "\n".join(fields_lines)

    # Paragraphes attendus issus de la spec (fonctions/programmes) — source de vérité YAML
    fonctions = norm.get("fonctions", []) or []
    allowed_paras = [fn.get("name") for fn in fonctions if fn.get("programme") == program_id and fn.get("name")]
    para_list_text = ", ".join(allowed_paras) if allowed_paras else "(paragraphes déclarés dans fonctions)"

    # Call interface (si définie dans le YAML)
    call_iface = prog_info.get("call_interface", {}) if prog_info else {}
    ci_txt = ""
    if call_iface:
        lp = call_iface.get("linkage_parameters", []) or []
        lp_lines = []
        for p in lp:
            pname = p.get("name")
            ppic = p.get("pic")
            pdir = p.get("direction")
            if pname and ppic:
                lp_lines.append(f"    {pname}: {ppic} (direction: {pdir})")
        if lp_lines:
            ci_txt = "Interface CALL/USING attendue:\n" + "\n".join(lp_lines)
        else:
            ci_txt = "Interface CALL/USING attendue selon la section call_interface."

    # Sections COBOL attendues
    cobol_sections = prog_info.get("cobol_sections", {}) if prog_info else {}
    sections_txt = ""
    if cobol_sections:
        parts = []
        for k, v in cobol_sections.items():
            if isinstance(v, dict):
                continue
            parts.append(f"{k}: {v}")
        if parts:
            sections_txt = "Sections requises : " + ", ".join(parts)

    # Infos techniques (EOF, SQLCA, curseur)
    eof_flag = norm.get("technique", {}).get("eof_flag", {})
    eof_name = eof_flag.get("name", "END-OF-FILE")
    eof_pic = eof_flag.get("pic", "X")
    eof_true = eof_flag.get("true_value", "Y")
    eof_false = eof_flag.get("false_value", "N")
    sqlca_required = norm.get("technique", {}).get("sqlca_required", False)
    cursor_name = ""
    for prog in norm.get("programmes", []):
        if prog.get("name") == program_id and prog.get("main_cursor"):
            cursor_name = prog.get("main_cursor")
            break
    if not cursor_name and norm.get("technique", {}).get("cursor_naming", {}).get("example", {}).get(entity):
        cursor_name = norm["technique"]["cursor_naming"]["example"][entity]

    # Fonctions détaillées pour ce programme
    fn_lines = []
    for fn in fonctions:
        if fn.get("programme") == program_id:
            desc = fn.get("description", "")
            name = fn.get("name", "")
            inputs = ", ".join(fn.get("inputs", []))
            outputs = ", ".join(fn.get("outputs", []))
            sql_decl = fn.get("sql", {}).get("declare_cursor") or fn.get("sql", {}).get("fetch_into") or fn.get("sql", {}).get("update_statement")
            loc = fn.get("cobol_location", {}) or {}
            loc_text = ""
            if loc:
                parts = []
                if loc.get("division"):
                    parts.append(f"division={loc.get('division')}")
                if loc.get("section"):
                    parts.append(f"section={loc.get('section')}")
                if loc.get("paragraph"):
                    parts.append(f"paragraphe={loc.get('paragraph')}")
                if parts:
                    loc_text = "  Emplacement: " + ", ".join(parts)
            fn_lines.append(f"- {name}: {desc}")
            if inputs:
                fn_lines.append(f"  Inputs: {inputs}")
            if outputs:
                fn_lines.append(f"  Outputs: {outputs}")
            if sql_decl:
                fn_lines.append(f"  SQL: {sql_decl}")
            if loc_text:
                fn_lines.append(loc_text)

    global_constraints = (
        "Réponds uniquement avec du code COBOL compilable, sans Markdown ni texte autour. "
        "La première ligne doit être IDENTIFICATION DIVISION. "
        "Un seul PROGRAM-ID, pas de sous-programmes."
    )

    # Contraintes globales (pas de markdown, un seul PROGRAM-ID, COBOL pur)
    global_constraints = (
        "Réponds uniquement avec du code COBOL compilable, sans Markdown ni texte autour. "
        "La première ligne doit être IDENTIFICATION DIVISION. "
        "Un seul PROGRAM-ID, pas de sous-programmes."
    )

    if layer == "dal":
        pid = program_id
        linkage_struct = f"""LINKAGE SECTION.
01 {eof_name} PIC {eof_pic}.
01 {entity.upper()}.
{employee_struct}

PROCEDURE DIVISION USING {eof_name} {entity.upper()}."""

        prompt = f"""{global_constraints}

Contexte métier/SQL:
- Table EMPLOYEE (issue du MCD) : EMP_ID INT PK, EMP_NAME VARCHAR(30), SALARY_BRUT DECIMAL(8,2), SALARY_NET DECIMAL(8,2).
- SGBD: PostgreSQL. Dialecte COBOL: gnucobol.
- Règle: ce programme est la couche DAL pour EMPLOYEE.
{sections_txt if sections_txt else ''}

Structure imposée (LINKAGE uniquement) :
- Le flag EOF et la structure EMPLOYEE sont reçus en paramètres (LINKAGE SECTION / USING).
- Exemple de forme attendue :
{linkage_struct}

Contraintes de conception (pas de code donné) :
- PROGRAM-ID doit être exactement : {pid}.
- Ne redéclare pas {eof_name} ni EMPLOYEE en WORKING-STORAGE. WORKING-STORAGE doit contenir uniquement les variables internes (SQLCA, WS-CONNECTED, WS-CURSOR-OPEN, etc.).
- Inclure SQLCA car SQLCODE est utilisé ({'obligatoire' if sqlca_required else 'facultatif'}).
- Utiliser un curseur SQL pour lire EMPLOYEE (DECLARE/OPEN/FETCH/CLOSE). Nom du curseur : {cursor_name or 'à déduire via technique.cursor_naming'}.
- Flag EOF : {eof_name} PIC {eof_pic}, valeurs {eof_true}/{eof_false}.
- Paragraphes attendus (exhaustif) : {para_list_text} et aucun autre paragraphe public.
- Interdictions : aucun DISPLAY, aucun autre PROGRAM-ID, pas de sous-programmes.
- Interface CALL/USING (si besoin) :
{ci_txt if ci_txt else '  (voir call_interface dans la spec)'}

Fonctions (depuis YAML.fonctions):
{chr(10).join(fn_lines) if fn_lines else '  - (voir fonctions)'}

Tâche : génère le programme COBOL complet respectant ces contraintes (code seulement)."""
        return prompt

    if layer == "logic":
        pid = program_id
        # Interface DAL pour la logique (programme externe)
        dal_prog = ""
        for prog_yaml in norm.get("programmes", []):
            if prog_yaml.get("layer") == "dal":
                dal_prog = prog_yaml.get("name", "")
                break
        dal_paras = [fn.get("name") for fn in fonctions if fn.get("programme") == dal_prog and fn.get("name")]
        dal_paras_text = ", ".join(dal_paras) if dal_paras else "(paragraphes DAL définis dans YAML)"
        # Call interface de la DAL (si définie)
        dal_ci = ""
        for prog_yaml in norm.get("programmes", []):
            if prog_yaml.get("name") == dal_prog and prog_yaml.get("call_interface"):
                lp = prog_yaml["call_interface"].get("linkage_parameters", []) or []
                lines = []
                for p in lp:
                    pname = p.get("name")
                    ppic = p.get("pic")
                    pdir = p.get("direction")
                    if pname and ppic:
                        lines.append(f"    {pname}: {ppic} (direction: {pdir})")
                if lines:
                    dal_ci = "Interface d'appel vers la DAL (CALL USING):\n" + "\n".join(lines)
                break
        # Programme BUSINESS (présentation) à appeler
        biz_prog = ""
        for prog_yaml in norm.get("programmes", []):
            if prog_yaml.get("layer") == "business":
                biz_prog = prog_yaml.get("name", "")
                break
        biz_ci = ""
        if biz_prog:
            biz_ci = (
                "Interface BUSINESS (appel obligatoire pour afficher) :\n"
                f"  - Programme : {biz_prog}\n"
                f"  - Appel attendu : CALL '{biz_prog}' USING EMPLOYEE\n"
            )
        interface_block = (dal_ci or "") + ("\n" + biz_ci if biz_ci else "")
        prompt = f"""{global_constraints}

Contexte métier :
- Règle principale : SALARY_NET = ROUND(SALARY_BRUT * 0.7, 2).
- Batch : traiter tous les employés.
{sections_txt if sections_txt else ''}

Interface DAL (sans code) :
- PROGRAM-ID DAL : {dal_prog or 'EMPLOYEE-DAL-DB'}
- Paragraphes disponibles : {dal_paras_text}.
- Record EMPLOYEE : 4 champs (EMP-ID, EMP-NAME, SALARY-BRUT, SALARY-NET), flag EOF partagé : {eof_name} ({eof_true}/{eof_false}).
{interface_block}

WORKING-STORAGE imposé :
01 {eof_name} PIC {eof_pic} VALUE '{eof_false}'.
77 OPERATION PIC X(4).
01 EMPLOYEE.
{employee_struct}

Contraintes (pas de code fourni) :
- PROGRAM-ID doit être exactement : {pid}.
- Ne déclare aucune autre variable en WORKING-STORAGE que celles listées ci-dessus.
- AUCUN EXEC SQL. AUCUN DISPLAY dans ce programme : l'affichage se fait en appelant le programme BUSINESS ({biz_prog or 'EMPLOYEE-BUSINESS'}) via CALL USING EMPLOYEE.
- Les accès base se font uniquement en appelant le programme DAL ({dal_prog or 'EMPLOYEE-DAL-DB'}) via CALL USING l'interface ci-dessus. N'inventes pas de paragraphes DAL dans ce programme.
- La boucle principale doit itérer jusqu’à {eof_name} = '{eof_true}', appeler la DAL pour lire, PERFORM CALCULATE-NET, rappeler la DAL pour sauvegarder, puis appeler le programme BUSINESS pour afficher l'employé courant.
- Paragraphes attendus (exhaustif) : {para_list_text or '(voir fonctions YAML)'} et aucun autre paragraphe public.
Fonctions (depuis YAML.fonctions):
{chr(10).join(fn_lines) if fn_lines else '  - (voir fonctions)'}

Tâche : génère le programme COBOL complet respectant ces contraintes (code seulement)."""
        return prompt

    # business
    pid = program_id
    # display_lines pour BUSINESS
    display_lines = []
    for fn in fonctions:
        if fn.get("programme") == program_id and fn.get("display_lines"):
            display_lines = fn.get("display_lines")
            break
    display_text = "\n".join(display_lines) if display_lines else ""
    linkage_struct = f"""LINKAGE SECTION.
01 EMPLOYEE.
{employee_struct}

PROCEDURE DIVISION USING EMPLOYEE."""
    prompt = f"""{global_constraints}

Contexte :
- Structure EMPLOYEE : 4 champs (EMP-ID, EMP-NAME, SALARY-BRUT, SALARY-NET).
- Rôle : couche présentation simple, affiche les valeurs.
{sections_txt if sections_txt else ''}

Contraintes :
- PROGRAM-ID doit être exactement : {pid}.
- WORKING-STORAGE : aucun autre 01 que nécessaire. La structure EMPLOYEE est reçue en USING (voir ci-dessous).
- LINKAGE SECTION / USING : structure imposée :
{linkage_struct}
- Aucun EXEC SQL, aucun CALL, pas de logique métier. Aucun STOP RUN.
- Une procédure qui affiche EMP-NAME, SALARY-BRUT, SALARY-NET (lignes imposées ci-dessous si présentes).
- Paragraphes attendus (exhaustif) : {para_list_text or 'DISPLAY-EMPLOYEE'} et aucun autre paragraphe public.
- Ne déclare aucune autre variable en WORKING-STORAGE que celles listées ci-dessus.
Fonctions (depuis YAML.fonctions):
{chr(10).join(fn_lines) if fn_lines else '  - (voir fonctions)'}
Lignes d'affichage (si définies) :
{display_text}

Tâche : génère le programme COBOL complet respectant ces contraintes (code seulement)."""
    return prompt


def _validate_program(code: str, program_id: str, layer: str, entity: str, norm: Dict) -> List[str]:
    """Validation minimale (markdown, PROGRAM-ID, PIC, paragraphes, interdictions)."""
    errors: List[str] = []
    lines = [l.rstrip() for l in code.splitlines() if l.strip() != ""]

    # Markdown / fences
    if "```" in code or code.strip().startswith("```"):
        errors.append("Supprimer les balises Markdown ```.")

    # PROGRAM-ID exact
    pid_ok = False
    for l in lines:
        if l.strip().upper().startswith("PROGRAM-ID"):
            if program_id.replace("_", "-").upper() in l.upper():
                pid_ok = True
            else:
                errors.append(f"PROGRAM-ID doit être exactement {program_id}.")
            break
    if not pid_ok:
        errors.append("PROGRAM-ID manquant ou incorrect.")

    # Paragraphes
    allowed = [fn.get("name") for fn in norm.get("fonctions", []) if fn.get("programme") == program_id]
    allowed = [p for p in allowed if p]
    if allowed:
        import re
        paras = []
        for l in lines:
            m = re.match(r"^\s*([A-Z0-9\-]+)\.", l)
            if not m:
                continue
            upper_line = l.upper()
            if any(skip in upper_line for skip in ("DIVISION", "SECTION", "PROGRAM-ID")):
                continue
            paras.append(m.group(1))
        missing = [p for p in allowed if p not in paras]
        extra = [p for p in paras if p not in allowed]
        if missing:
            errors.append(f"Paragraphes manquants: {', '.join(missing)}.")
        if extra:
            errors.append(f"Paragraphes en trop: {', '.join(extra)}.")

    # PIC check (champ essentiels)
    attr_map = {}
    for ent in norm.get("mcd", {}).get("entites", []):
        if ent.get("name") == entity or ent.get("normalized_name") == entity:
            attr_map = {a.get("name"): a.get("cobol_pic") for a in ent.get("attrs", [])}
            break
    for name, pic in attr_map.items():
        if not pic:
            continue
        pic_norm = pic if pic.upper().startswith("PIC") else f"PIC {pic}"
        cobol_name = name.replace("_", "-")
        if cobol_name not in code or pic_norm not in code:
            errors.append(f"Le champ {cobol_name} doit utiliser {pic_norm}.")

    # EOF flag
    eof = norm.get("technique", {}).get("eof_flag", {})
    eof_name = eof.get("name", "END-OF-FILE")
    eof_pic = eof.get("pic", "X")
    if layer != "business":  # EOF non requis en business
        if eof_name and eof_name not in code:
            errors.append(f"Le flag EOF {eof_name} doit être déclaré.")
        if eof_pic and f"PIC {eof_pic}" not in code:
            errors.append(f"Le flag EOF doit utiliser PIC {eof_pic}.")

    # Interdictions spécifiques
    if layer == "dal":
        if "DISPLAY" in code.upper():
            errors.append("Aucun DISPLAY autorisé dans la DAL.")
        if "EXEC SQL" not in code.upper():
            errors.append("La DAL doit contenir des EXEC SQL (curseur/maj).")
    else:
        if "EXEC SQL" in code.upper():
            errors.append("Pas de EXEC SQL dans ce programme.")
    if layer == "business" and "STOP RUN" in code.upper():
        errors.append("Pas de STOP RUN dans la couche business.")

    # Variables WS supplémentaires : on tolère uniquement le groupe EMPLOYEE et EOF (et SQLCA s'il apparaît via INCLUDE)
    allowed_ws = {"EMPLOYEE"}
    if eof_name:
        allowed_ws.add(eof_name)
    import re
    ws_names = []
    for l in lines:
        m = re.match(r"^\s*0?1\s+([A-Z0-9\-]+)", l)
        if m:
            ws_names.append(m.group(1))
    extra_ws = [w for w in ws_names if w not in allowed_ws]
    if extra_ws:
        errors.append(f"Variables WORKING-STORAGE en trop: {', '.join(extra_ws)}.")

    return errors

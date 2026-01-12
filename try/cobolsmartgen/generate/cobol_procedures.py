# file: cobolsmartgen/generate/cobol_procedures.py
"""
COBOL procedure generation - LLM-only from specs.
NO static templates, NO code examples.
"""
from __future__ import annotations
import logging
import os
import re
from pathlib import Path
from typing import Dict, List

from ..utils import fs, trace
from cobolsmartgen.adapters import llm_auto
from . import cobol_constraints
import json

LOG = logging.getLogger(__name__)

def _layer_folder(layer: str) -> str:
    l = (layer or "logic").lower()
    return l if l in ("dal", "logic", "business") else "logic"

def _format_table_signature(entity_fields: Dict) -> str:
    attrs = entity_fields.get("attrs", []) if entity_fields else []
    parts: List[str] = []
    for attr in attrs:
        name = attr.get("name")
        sql_type = attr.get("sql_type") or attr.get("type") or ""
        pk = " PK" if attr.get("pk") else ""
        if name and sql_type:
            parts.append(f"{name} {sql_type}{pk}")
        elif name:
            parts.append(name)
    return ", ".join(parts) if parts else "(no fields)"

def _format_entity_fields_list(entity_fields: Dict) -> str:
    attrs = entity_fields.get("attrs", []) if entity_fields else []
    parts: List[str] = []
    for attr in attrs:
        name = (attr.get("name") or "").replace("_", "-")
        pic = attr.get("cobol_pic") or attr.get("pic") or attr.get("cobol_type") or ""
        if pic and not pic.upper().startswith("PIC"):
            pic = f"PIC {pic}"
        if name and pic:
            parts.append(f"{name} {pic}")
        elif name:
            parts.append(name)
    return ", ".join(parts) if parts else ""

def _render_lines(lines: List[str], variables: Dict) -> List[str]:
    rendered: List[str] = []
    for line in lines or []:
        if not isinstance(line, str):
            continue
        try:
            rendered.append(line.format_map(variables))
        except Exception:
            rendered.append(line)
    return rendered

def _format_call_interface(call_iface: Dict) -> str:
    if not call_iface:
        return ""
    lp = call_iface.get("linkage_parameters", []) or []
    parts: List[str] = []
    for p in lp:
        pname = p.get("name")
        ppic = p.get("pic")
        pdir = p.get("direction")
        if pname and ppic:
            parts.append(f"{pname}: {ppic} (direction: {pdir})")
        elif pname:
            parts.append(pname)
    return "; ".join(parts)

def _build_linkage_struct(prog_info: Dict, entity: str, entity_struct: str) -> str:
    call_iface = prog_info.get("call_interface", {}) if prog_info else {}
    linkage_params = call_iface.get("linkage_parameters", []) or []
    using_clause = (
        (prog_info.get("cobol_sections", {}) or {})
        .get("procedure_division", {})
        .get("using_clause", [])
    )
    if not using_clause and linkage_params:
        using_clause = [p.get("name") for p in linkage_params if p.get("name")]

    lines: List[str] = ["LINKAGE SECTION."]
    if linkage_params:
        for p in linkage_params:
            pname = p.get("name")
            ppic = p.get("pic", "")
            if not pname:
                continue
            # If the spec indicates a group/structure (eg. '01 level structure',
            # 'group', 'groupe', 'level', 'structure'), render the full entity
            # structure under this linkage name so PIC clauses come from the
            # entity copybook (entity_struct). This handles vague descriptors
            # produced by normalization instead of concrete PIC clauses.
            if isinstance(ppic, str) and re.search(r"\b(groupe|group|01|level|structure)\b", ppic.lower()):
                lines.append(f"01 {pname}.")
                if entity_struct:
                    prefix = "LK-" if pname.upper().startswith("LK-") else ""
                    for l in entity_struct.splitlines():
                        line = l.strip()
                        if prefix:
                            m = re.match(r"^05\s+([A-Z0-9\-]+)\s+(PIC .+)$", line)
                            if m:
                                line = f"05 {prefix}{m.group(1)} {m.group(2)}"
                        lines.append(f"    {line}")
            else:
                if ppic and not ppic.upper().startswith("PIC"):
                    ppic = f"PIC {ppic}"
                if ppic:
                    lines.append(f"01 {pname} {ppic}.")
                else:
                    lines.append(f"01 {pname}.")
    elif using_clause:
        for item in using_clause:
            if item and item.upper() == (entity or "").upper():
                lines.append(f"01 {entity}.")
                if entity_struct:
                    for l in entity_struct.splitlines():
                        lines.append(f"    {l.strip()}")
            else:
                lines.append(f"01 {item} PIC X.")
    elif entity:
        lines.append(f"01 {entity}.")
        if entity_struct:
            for l in entity_struct.splitlines():
                lines.append(f"    {l.strip()}")

    using_text = " ".join(using_clause) if using_clause else ""
    if using_text:
        lines.append("")
        lines.append(f"PROCEDURE DIVISION USING {using_text}.")
    return "\n".join(lines)

def _build_connection_details(norm: Dict) -> str:
    conn = (norm.get("sql", {}) or {}).get("connection", {}) if norm else {}
    if not conn:
        return ""
    lines: List[str] = []
    engine = conn.get("engine")
    method = conn.get("method")
    host = conn.get("host")
    port = conn.get("port")
    dbname = conn.get("dbname")
    user = conn.get("user")
    if any([engine, method, host, port, dbname, user]):
        lines.append(f"engine={engine}, method={method}, host={host}, port={port}, dbname={dbname}, user={user}")
    env_vars = conn.get("env_vars", {}) or {}
    if env_vars:
        env_pairs = ", ".join(f"{k}={v}" for k, v in env_vars.items())
        lines.append(f"env_vars: {env_pairs}")
    env_set_method = conn.get("env_set_method", []) or []
    if env_set_method:
        lines.append("env_set_method: " + " | ".join(env_set_method))
    ocesql = conn.get("ocesql", {}) or {}
    if ocesql:
        start_call = ocesql.get("start_call")
        connect_call = ocesql.get("connect_call")
        end_call = ocesql.get("end_call")
        if start_call or connect_call or end_call:
            lines.append(f"OCESQL calls: {start_call}, {connect_call}, {end_call}")
        connect_args = ocesql.get("connect_args", []) or []
        if connect_args:
            lines.append("connect_args: " + ", ".join(connect_args))
        order_note = ocesql.get("connect_args_order_note")
        if order_note:
            lines.append(order_note)
    return "\n".join(lines)

def _build_format_details(norm: Dict) -> str:
    fmt = (norm.get("cobol_format", {}) or {}) if norm else {}
    if not fmt:
        return ""
    lines: List[str] = []
    line_format = fmt.get("line_format")
    if line_format:
        lines.append(f"line_format: {line_format}")
    indent_spaces = fmt.get("indent_spaces")
    if indent_spaces is not None:
        lines.append(f"indent_spaces: {indent_spaces}")
    max_len = fmt.get("max_line_length")
    if max_len is not None:
        lines.append(f"max_line_length: {max_len}")
    comment = fmt.get("comment", {}) or {}
    comment_col = comment.get("column")
    comment_prefix = comment.get("prefix")
    if comment_col or comment_prefix:
        lines.append(f"comment: column={comment_col}, prefix={comment_prefix}")
    literal_quotes = fmt.get("literal_quotes")
    if literal_quotes:
        lines.append(f"literal_quotes: {literal_quotes}")
    paragraph_terminator = fmt.get("paragraph_terminator")
    if paragraph_terminator:
        lines.append(f"paragraph_terminator: {paragraph_terminator}")
    allow_source_directive = fmt.get("allow_source_format_directive")
    if allow_source_directive is not None:
        lines.append(f"allow_source_format_directive: {allow_source_directive}")
    return "\n".join(lines)

def _build_naming_conventions(norm: Dict) -> str:
    naming = norm.get("nommage", {}) if norm else {}
    rules = norm.get("naming_rules", {}) if norm else {}
    lines: List[str] = []
    if naming:
        var_case = naming.get("variables_case")
        prog_case = naming.get("programmes_case")
        if var_case or prog_case:
            lines.append(f"variables_case={var_case}, programmes_case={prog_case}")
    prefixes = (rules.get("prefixes", {}) or {})
    if prefixes:
        pref_pairs = ", ".join(f"{k}={v}" for k, v in prefixes.items())
        lines.append(f"prefixes: {pref_pairs}")
    paragraph_style = rules.get("paragraph_style")
    if paragraph_style:
        lines.append(f"paragraph_style: {paragraph_style}")
    return "\n".join(lines)

def _build_sql_behavior_details(norm: Dict) -> str:
    behavior = (norm.get("sql", {}) or {}).get("behavior", {}) if norm else {}
    if not behavior:
        return ""
    lines: List[str] = []
    if "check_sqlcode" in behavior:
        lines.append(f"check_sqlcode: {behavior.get('check_sqlcode')}")
    if "check_sqlstate" in behavior:
        lines.append(f"check_sqlstate: {behavior.get('check_sqlstate')}")
    if behavior.get("sqlcode_success") is not None:
        lines.append(f"sqlcode_success: {behavior.get('sqlcode_success')}")
    if behavior.get("sqlcode_eof") is not None:
        lines.append(f"sqlcode_eof: {behavior.get('sqlcode_eof')}")
    if "rollback_on_error" in behavior:
        lines.append(f"rollback_on_error: {behavior.get('rollback_on_error')}")
    commit = behavior.get("commit", {}) or {}
    if commit:
        commit_mode = commit.get("mode")
        commit_stmt = commit.get("statement")
        lines.append(f"commit: mode={commit_mode}, statement={commit_stmt}")
    disconnect = behavior.get("disconnect", {}) or {}
    if disconnect:
        disc_stmt = disconnect.get("statement")
        lines.append(f"disconnect: statement={disc_stmt}")
    if "null_indicators" in behavior:
        lines.append(f"null_indicators: {behavior.get('null_indicators')}")

    # Note: cursor_commit_policy est maintenant géré par _build_cursor_commit_policy_rules
    # pour fournir des explications détaillées au LLM

    return "\n".join(lines)

def _build_cursor_commit_policy_rules(norm: Dict, program_id: str) -> str:
    """
    Génère automatiquement les règles COMMIT basées sur cursor_commit_policy.
    S'adapte à toute spec - pas spécifique à DAL.

    Returns:
        Section de prompt expliquant la politique COMMIT/curseur
    """
    behavior = (norm.get("sql", {}) or {}).get("behavior", {})
    cursor_policy = behavior.get("cursor_commit_policy", "")

    if not cursor_policy:
        return ""

    # Analyser les fonctions pour trouver où le COMMIT final est fait
    functions = [fn for fn in norm.get("fonctions", []) if fn.get("programme") == program_id]

    commit_allowed_in = []
    no_commit_in = []

    for fn in functions:
        fn_name = fn.get("name", "")
        flow_desc = (fn.get("description", "") or "").upper()

        # Détecter la fonction de fin (CLOSE + COMMIT + DISCONNECT)
        if any(keyword in flow_desc for keyword in ["CLOSE", "DISCONNECT", "FIN", "END", "CLEANUP"]):
            commit_allowed_in.append(fn_name)
        elif any(keyword in fn_name.upper() for keyword in ["END", "CLOSE", "CLEANUP"]):
            commit_allowed_in.append(fn_name)
        else:
            no_commit_in.append(fn_name)

    if cursor_policy == "no_commit_while_open":
        lines = [
            "",
            "=== POLITIQUE COMMIT/CURSOR (CRITIQUE) ===",
            f"cursor_commit_policy: {cursor_policy}",
            "",
            "SIGNIFICATION:",
            "- Le curseur DOIT rester ouvert pendant tout le traitement",
            "- PostgreSQL ferme automatiquement les curseurs non-WITH-HOLD sur COMMIT",
            "- DONC: NE PAS mettre EXEC SQL COMMIT dans les fonctions du traitement",
            "",
            "RÈGLES STRICTES:",
        ]

        if commit_allowed_in:
            lines.append(f"✅ COMMIT AUTORISÉ dans: {', '.join(commit_allowed_in)}")

        if no_commit_in:
            lines.append(f"❌ COMMIT INTERDIT dans: {', '.join(no_commit_in)}")

        lines.extend([
            "",
            "POURQUOI:",
            "- Un COMMIT dans une fonction du traitement fermerait le curseur",
            "- Les prochains FETCH échoueraient avec SQLCODE=-400 (curseur fermé)",
            "- Le batch s'arrêterait prématurément après 1 seul enregistrement",
            "",
            "SI ERREUR UPDATE/INSERT/DELETE:",
            "- ✅ AUTORISÉ: EXEC SQL ROLLBACK END-EXEC (annule sans fermer curseur)",
            "- ❌ INTERDIT: EXEC SQL COMMIT END-EXEC (fermerait le curseur)",
            f"- Le COMMIT final sera fait dans {commit_allowed_in[0] if commit_allowed_in else 'la fonction de fin'}",
            ""
        ])

        return "\n".join(lines)

    # Autres politiques (pour future extension)
    return f"cursor_commit_policy: {cursor_policy}"

def _build_sql_formatting_details(norm: Dict) -> str:
    fmt = (norm.get("sql", {}) or {}).get("formatting", {}) if norm else {}
    if not fmt:
        return ""
    lines: List[str] = []
    max_len = fmt.get("max_line_length")
    if max_len is not None:
        lines.append(f"max_line_length: {max_len}")
    if "host_vars_one_per_line" in fmt:
        lines.append(f"host_vars_one_per_line: {fmt.get('host_vars_one_per_line')}")
    if "split_long_exec_sql" in fmt:
        lines.append(f"split_long_exec_sql: {fmt.get('split_long_exec_sql')}")
    return "\n".join(lines)

def _format_entry_block_rules(rules: List[Dict]) -> str:
    if not rules:
        return ""
    lines: List[str] = []
    for rule in rules:
        if not isinstance(rule, dict):
            continue
        cond = rule.get("condition_regex") or rule.get("condition") or ""
        must = rule.get("must_contain", []) or []
        lines.append(f"condition: {cond}")
        if must:
            lines.append("must_contain:")
            for item in must:
                lines.append(f"  - {item}")
    return "\n".join(lines)

def _format_entry_exit_sequence(entry_exit: Dict) -> str:
    if not entry_exit or not isinstance(entry_exit, dict):
        return ""
    para = entry_exit.get("paragraph")
    order = entry_exit.get("must_follow_order", []) or []
    lines: List[str] = []
    if para:
        lines.append(f"paragraph: {para}")
    if order:
        lines.append("must_follow_order:")
        for item in order:
            lines.append(f"  - {item}")
    return "\n".join(lines)

def _format_paragraph_constraints(constraints: List[Dict]) -> str:
    if not constraints:
        return ""
    lines: List[str] = []
    for rule in constraints:
        if not isinstance(rule, dict):
            continue
        para = rule.get("paragraph")
        forbid = rule.get("forbid_contains", []) or []
        require = rule.get("require_contains", []) or []
        if para:
            lines.append(f"paragraph: {para}")
        if require:
            lines.append("require_contains:")
            for item in require:
                lines.append(f"  - {item}")
        if forbid:
            lines.append("forbid_contains:")
            for item in forbid:
                lines.append(f"  - {item}")
    return "\n".join(lines)

def _extract_program_info(norm: Dict, program_id: str) -> Dict:
    for prog in norm.get("programmes", []):
        if prog.get("name") == program_id:
            return prog
    return {}

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

def _wrap_long_lines_fixed(code: str, max_len: int) -> str:
    """Wrap long PROCEDURE DIVISION lines for FIXED format without touching literals."""
    out: List[str] = []
    in_proc = False
    for line in code.splitlines():
        stripped = line.rstrip()
        upper = stripped.strip().upper()
        if "PROCEDURE DIVISION" in upper:
            in_proc = True
        if not in_proc or len(stripped) <= max_len:
            out.append(stripped)
            continue
        # Do not wrap lines with literals or EXEC SQL blocks
        if "'" in stripped or "\"" in stripped:
            out.append(stripped)
            continue
        if "EXEC SQL" in upper or "END-EXEC" in upper:
            out.append(stripped)
            continue

        indent = re.match(r"^\s*", stripped).group(0)
        content = stripped.strip()
        while len(indent + content) > max_len:
            cut = content.rfind(" ", 0, max_len - len(indent))
            if cut <= 0:
                break
            out.append(indent + content[:cut].rstrip())
            content = content[cut + 1:].lstrip()
        out.append(indent + content)
    return "\n".join(out)


def _inject_missing_cursor_declarations(code: str, program_id: str, norm: Dict) -> str:
    """
    Post-validation: Garantit que DECLARE CURSOR et OPEN CURSOR sont dans des blocs séparés.

    Contexte: Le LLM peut:
    1. Omettre complètement le DECLARE CURSOR
    2. Mettre DECLARE et OPEN dans le même bloc EXEC SQL (cause des bugs OCESQL)

    Cette fonction garantit que:
    - DECLARE CURSOR est présent
    - DECLARE et OPEN sont dans des blocs EXEC SQL SÉPARÉS (comme EMPLOYEE qui fonctionne)

    Args:
        code: Code COBOL généré par le LLM
        program_id: ID du programme (ex: ACCOUNT-DAL-DB)
        norm: Spec normalisée contenant les declare_cursor

    Returns:
        Code COBOL avec DECLARE et OPEN dans des blocs séparés
    """
    # Extraire les fonctions de ce programme
    program_funcs = [fn for fn in norm.get("fonctions", []) if fn.get("programme") == program_id]

    lines = code.splitlines()
    modified = False

    for fn in program_funcs:
        sql = fn.get("sql", {}) or {}
        declare_cursor = sql.get("declare_cursor", "").strip()
        open_cursor = sql.get("open_cursor", "").strip()

        # Si pas de declare_cursor dans la spec, rien à faire
        if not declare_cursor or not open_cursor:
            continue

        # Extraire le nom du curseur (ex: C_ACC depuis "OPEN C_ACC")
        cursor_match = re.search(r'OPEN\s+(\w+)', open_cursor, re.IGNORECASE)
        if not cursor_match:
            continue

        cursor_name = cursor_match.group(1)

        # Vérifier si DECLARE est présent dans le code
        code_upper = code.upper()
        declare_pattern = f"DECLARE\\s+{cursor_name}\\s+CURSOR"

        # CAS 1: DECLARE et OPEN dans le même bloc (problème OCESQL)
        # Chercher un bloc qui contient les deux
        declare_idx = None
        open_idx = None

        for i, line in enumerate(lines):
            if re.search(rf'\bDECLARE\s+{cursor_name}\s+CURSOR', line, re.IGNORECASE):
                declare_idx = i
            if re.search(rf'\bOPEN\s+{cursor_name}\b', line, re.IGNORECASE):
                open_idx = i

        if declare_idx is not None and open_idx is not None:
            # DECLARE et OPEN trouvés - vérifier s'ils sont dans le même bloc
            # Trouver le END-EXEC entre DECLARE et OPEN
            end_exec_between = False
            for i in range(declare_idx, open_idx):
                if re.search(r'END-EXEC', lines[i], re.IGNORECASE):
                    end_exec_between = True
                    break

            if not end_exec_between:
                # DECLARE et OPEN dans le MÊME bloc → Séparer!
                LOG.warning(f"⚠️  DECLARE et OPEN {cursor_name} dans le même bloc EXEC SQL, séparation requise")

                # Trouver EXEC SQL de départ
                exec_sql_start = None
                for j in range(declare_idx - 1, -1, -1):
                    if re.search(r'EXEC\s+SQL\s*$', lines[j], re.IGNORECASE):
                        exec_sql_start = j
                        break

                # Trouver END-EXEC de fin
                exec_sql_end = None
                for j in range(open_idx, len(lines)):
                    if re.search(r'END-EXEC', lines[j], re.IGNORECASE):
                        exec_sql_end = j
                        break

                if exec_sql_start and exec_sql_end:
                    # Récupérer l'indentation
                    exec_indent = re.match(r'^(\s*)', lines[exec_sql_start]).group(0)
                    open_indent = re.match(r'^(\s*)', lines[open_idx]).group(0)

                    # Créer le bloc OPEN séparé
                    open_block = [
                        f"{exec_indent}EXEC SQL",
                        f"{open_indent}OPEN {cursor_name}",
                        f"{exec_indent}END-EXEC"
                    ]

                    # Supprimer la ligne OPEN du bloc actuel
                    del lines[open_idx]

                    # Insérer le END-EXEC après DECLARE (si pas déjà là)
                    # et ajouter le nouveau bloc OPEN après
                    lines[exec_sql_end:exec_sql_end] = open_block

                    modified = True
                    LOG.warning(f"✅ DECLARE et OPEN {cursor_name} séparés en deux blocs EXEC SQL")
                    # Pas de break car on continue pour d'autres curseurs potentiels
                    continue

            # Si on arrive ici, DECLARE existe déjà et est bien séparé
            continue

        # CAS 2: DECLARE manquant complètement

        # DECLARE manquant! Chercher le OPEN et injecter le DECLARE avant
        LOG.warning(f"⚠️  DECLARE {cursor_name} CURSOR manquant dans {program_id}, injection automatique")

        # Chercher la ligne contenant OPEN cursor_name
        for i, line in enumerate(lines):
            if re.search(rf'\bOPEN\s+{cursor_name}\b', line, re.IGNORECASE):
                # Trouver le début du bloc EXEC SQL (en remontant)
                exec_sql_start = None
                for j in range(i - 1, -1, -1):
                    if re.search(r'EXEC\s+SQL\s*$', lines[j], re.IGNORECASE):
                        exec_sql_start = j
                        break

                if exec_sql_start is None:
                    LOG.error(f"Cannot find EXEC SQL before OPEN {cursor_name}")
                    continue

                # Récupérer l'indentation du bloc EXEC SQL
                exec_indent = re.match(r'^(\s*)', lines[exec_sql_start]).group(1)
                # Récupérer l'indentation du contenu SQL (généralement exec_indent + quelques espaces)
                sql_content_indent = re.match(r'^(\s*)', lines[i]).group(1)

                # Préparer le bloc DECLARE complet (EXEC SQL ... END-EXEC)
                declare_block = []

                # EXEC SQL
                declare_block.append(f"{exec_indent}EXEC SQL")

                # DECLARE avec indentation du contenu
                declare_block.append(f"{sql_content_indent}DECLARE {cursor_name} CURSOR FOR")

                # Extraire la partie SELECT du declare_cursor
                select_part = declare_cursor
                if "DECLARE" in select_part.upper():
                    # Extraire seulement après DECLARE ... FOR
                    select_match = re.search(r'CURSOR\s+FOR\s+(SELECT\s+.+)', select_part, re.DOTALL | re.IGNORECASE)
                    if select_match:
                        select_part = select_match.group(1)
                    else:
                        # Sinon extraire SELECT directement
                        select_match = re.search(r'(SELECT\s+.+)', select_part, re.DOTALL | re.IGNORECASE)
                        if select_match:
                            select_part = select_match.group(1)

                # Ajouter les lignes SELECT avec même indentation que le DECLARE
                for select_line in select_part.strip().splitlines():
                    if select_line.strip():
                        declare_block.append(f"{sql_content_indent}     {select_line.strip()}")

                # END-EXEC
                declare_block.append(f"{exec_indent}END-EXEC")

                # Insérer le bloc DECLARE complet AVANT le bloc EXEC SQL qui contient OPEN
                lines[exec_sql_start:exec_sql_start] = declare_block
                modified = True
                LOG.warning(f"✅ DECLARE {cursor_name} CURSOR injecté dans un bloc EXEC SQL séparé")
                break

    if modified:
        return "\n".join(lines)
    return code


def _generate_with_validation(prompt_base: str, system: str, program_id: str, layer: str,
                              entity: str, norm: Dict, config: Dict) -> (str, str):
    """Gen LLM puis valide; si échec, renvoie avec message correctif (1 retry)."""
    last_prompt = prompt_base
    for attempt in range(2):
        resp = llm_auto.generate(prompt=last_prompt, system=system, config=config)
        clean = _clean_markdown_cobol(resp)
        fmt = norm.get("cobol_format", {}) or {}
        if fmt.get("line_format") == "fixed":
            max_len = fmt.get("max_line_length")
            if isinstance(max_len, int) and max_len > 0:
                clean = _wrap_long_lines_fixed(clean, max_len)

        # Post-injection: Garantir que DECLARE CURSOR est présent si nécessaire
        clean = _inject_missing_cursor_declarations(clean, program_id, norm)

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


def _extract_select_columns_from_sql(sql_query: str) -> List[str]:
    """Parse un SELECT pour extraire les noms de colonnes."""
    columns = []
    # Trouver la partie SELECT ... FROM
    match = re.search(r'SELECT\s+(.*?)\s+FROM', sql_query, re.IGNORECASE | re.DOTALL)
    if not match:
        return columns

    select_part = match.group(1)
    # Découper par virgules et nettoyer
    for col in select_part.split(','):
        col = col.strip()
        # Extraire le nom de colonne (supporter o.OP_ID, CLIENT_NAME, etc.)
        # Prendre la dernière partie après le point si présent
        if '.' in col:
            col = col.split('.')[-1].strip()
        columns.append(col)

    return columns

def _build_linkage_fields_from_select(select_columns: List[str], norm: Dict) -> List[str]:
    """Construit les lignes de champs LINKAGE à partir des colonnes SELECT et du MCD."""
    fields_lines: List[str] = []

    # Construire un dictionnaire de tous les attributs de toutes les entités
    all_attrs = {}
    for ent in norm.get("mcd", {}).get("entites", []):
        for attr in ent.get("attrs", []):
            attr_name = attr.get("name", "")
            if attr_name:
                all_attrs[attr_name] = attr

    # Pour chaque colonne du SELECT, trouver son type
    for col in select_columns:
        col_upper = col.upper().replace("-", "_")

        # Chercher l'attribut dans le dictionnaire
        attr = all_attrs.get(col_upper)
        if not attr:
            # Si pas trouvé, chercher avec des variantes
            for key, val in all_attrs.items():
                if key.replace("_", "") == col_upper.replace("_", ""):
                    attr = val
                    break

        # Construire le nom COBOL (remplacer _ par -)
        name = col_upper.replace("_", "-")

        # Déterminer le PIC
        if attr:
            pic = attr.get("cobol_pic") or attr.get("pic") or attr.get("cobol_type")
            if not pic:
                t = (attr.get("type", "") or "").upper()
                if "INT" in t:
                    pic = "9(8)" if "ID" in name else "9(4)"
                elif "VARCHAR" in t:
                    m = re.search(r'VARCHAR\((\d+)\)', t)
                    l = m.group(1) if m else "30"
                    pic = f"X({l})"
                elif "DECIMAL" in t:
                    m = re.search(r'DECIMAL\((\d+),(\d+)\)', t)
                    if m:
                        p, s = m.group(1), m.group(2)
                        pic = f"S9({int(p)-int(s)})V99" if int(s) > 0 else f"9({p})"
                    else:
                        pic = "9(6)V99"
                else:
                    pic = "X(30)"
        else:
            # Fallback si attribut non trouvé
            pic = "X(30)"

        if not pic.upper().startswith("PIC"):
            pic = f"PIC {pic}"
        fields_lines.append(f"    05 {name:<15} {pic}.")

    return fields_lines

def _build_strict_prompt(program_id: str, layer: str, entity: str, norm: Dict, io_map: Dict,
                         contract: Dict = None, plan: Dict = None) -> str:
    """Construit un prompt contractuel (pas de code donné) pour 8.2/8.3/8.4 en lisant la spec YAML normalisée."""
    prog_info = _extract_program_info(norm, program_id)
    prompting = norm.get("prompting", {}) or {}

    # CRITICAL FIX (2026-01-11): Pour les programmes DAL avec JOIN, extraire TOUS les champs du SELECT
    # Pas seulement les champs de l'entité principale
    fields_lines: List[str] = []

    # Chercher si ce programme a un SELECT avec JOIN
    select_query = None
    fonctions = norm.get("fonctions", []) or []
    for fn in fonctions:
        if fn.get("programme") == program_id:
            sql = fn.get("sql", {}) or {}
            select_list = sql.get("select", []) or []
            if select_list and isinstance(select_list, list):
                for sel in select_list:
                    if isinstance(sel, str) and "JOIN" in sel.upper():
                        select_query = sel
                        break
            if select_query:
                break

    # Chercher l'entité principale pour table_signature et entity_fields_list (toujours nécessaire)
    entity_fields = {}
    for ent in norm.get("mcd", {}).get("entites", []):
        if ent.get("name") == entity or ent.get("normalized_name") == entity:
            entity_fields = ent
            break

    if select_query:
        # Parser le SELECT pour extraire toutes les colonnes
        select_columns = _extract_select_columns_from_sql(select_query)
        if select_columns:
            fields_lines = _build_linkage_fields_from_select(select_columns, norm)

    # Fallback: si pas de SELECT avec JOIN trouvé, utiliser l'ancienne méthode (entité unique)
    if not fields_lines:
        for attr in entity_fields.get("attrs", []):
            name = attr.get("name", "").replace("_", "-")
            pic = attr.get("cobol_pic") or attr.get("pic") or attr.get("cobol_type")
            if not pic:
                t = (attr.get("type", "") or "").upper()
                if "INT" in t:
                    pic = "9(4)"
                elif "VARCHAR" in t:
                    m = re.search(r'VARCHAR\((\d+)\)', t)
                    l = m.group(1) if m else "30"
                    pic = f"X({l})"
                elif "DECIMAL" in t:
                    pic = "9(6)V99"
                else:
                    pic = "X(30)"
            if not pic.upper().startswith("PIC"):
                pic = f"PIC {pic}"
            fields_lines.append(f"    05 {name:<15} {pic}.")

    entity_struct = "\n".join(fields_lines)

    # Récupérer les fonctions pour ce programme (déjà chargé plus haut)
    program_funcs = [fn for fn in fonctions if fn.get("programme") == program_id]
    public_funcs = [fn for fn in program_funcs if fn.get("visibility", "public") != "internal"]
    internal_funcs = [fn for fn in program_funcs if fn.get("visibility") == "internal"]
    public_paras = [fn.get("name") for fn in public_funcs if fn.get("name")]
    internal_paras = [fn.get("name") for fn in internal_funcs if fn.get("name")]
    internal_paras.extend(prog_info.get("internal_paragraphs", []) or [])

    call_iface = prog_info.get("call_interface", {}) if prog_info else {}
    call_interface_text = _format_call_interface(call_iface)
    dal_call_interface_text = ""
    for prog_yaml in norm.get("programmes", []):
        if prog_yaml.get("layer") == "dal":
            dal_call_interface_text = _format_call_interface(prog_yaml.get("call_interface", {}))
            break
    if layer == "logic" and dal_call_interface_text:
        call_interface_text = dal_call_interface_text
    linkage_struct = _build_linkage_struct(prog_info, (entity or "").upper(), entity_struct)

    table_signature = _format_table_signature(entity_fields)
    entity_fields_list = _format_entity_fields_list(entity_fields)

    # Variables techniques
    eof_flag = norm.get("technique", {}).get("eof_flag", {})
    eof_name = eof_flag.get("name", "END-OF-FILE")
    eof_pic = eof_flag.get("pic", "X")
    entry_paragraph = (
        (prog_info.get("cobol_sections", {}) or {})
        .get("procedure_division", {})
        .get("entry_paragraph", "")
        or prog_info.get("entry_paragraph", "")
    )
    using_clause = (
        (prog_info.get("cobol_sections", {}) or {})
        .get("procedure_division", {})
        .get("using_clause", [])
    )
    procedure_division_line = "PROCEDURE DIVISION."
    if using_clause:
        procedure_division_line = f"PROCEDURE DIVISION USING {' '.join(using_clause)}."

    cursor_name = ""
    for prog in norm.get("programmes", []):
        if prog.get("name") == program_id and prog.get("main_cursor"):
            cursor_name = prog.get("main_cursor")
            break
    if not cursor_name and norm.get("technique", {}).get("cursor_naming", {}).get("example", {}).get(entity):
        cursor_name = norm["technique"]["cursor_naming"]["example"][entity]

    # Fonctions détaillées pour ce programme
    fn_lines: List[str] = []
    sql_lines: List[str] = []
    for fn in program_funcs:
        name = fn.get("name", "")
        desc = fn.get("description", "")
        visibility = fn.get("visibility", "public")
        required = fn.get("required", True)
        inputs = ", ".join(fn.get("inputs", []))
        outputs = ", ".join(fn.get("outputs", []))
        steps = fn.get("steps", []) or []
        loc = fn.get("cobol_location", {}) or {}

        if name:
            fn_lines.append(f"- {name}: {desc}")
            fn_lines.append(f"  visibility: {visibility}, required: {required}")
            if inputs:
                fn_lines.append(f"  Inputs: {inputs}")
            if outputs:
                fn_lines.append(f"  Outputs: {outputs}")
            if steps:
                fn_lines.append("  Steps:")
                for step in steps:
                    fn_lines.append(f"    - {step}")
            if loc:
                parts = []
                if loc.get("division"):
                    parts.append(f"division={loc.get('division')}")
                if loc.get("section"):
                    parts.append(f"section={loc.get('section')}")
                if loc.get("paragraph"):
                    parts.append(f"paragraphe={loc.get('paragraph')}")
                if parts:
                    fn_lines.append("  Emplacement: " + ", ".join(parts))

        sql_block = fn.get("sql", {}) or {}
        if sql_block:
            sql_lines.append(f"{name}:")
            for key, val in sql_block.items():
                if isinstance(val, list):
                    for item in val:
                        sql_lines.append(f"  {key}: {item}")
                else:
                    sql_lines.append(f"  {key}: {val}")

    sql_text = "\n".join(sql_lines) if sql_lines else ""

    # Flow / logging / WS
    flow_notes = "\n".join(prog_info.get("flow", []) or [])
    logging_lines = "\n".join(prog_info.get("logging_lines", []) or [])
    working_storage_lines = "\n".join(prog_info.get("working_storage_lines", []) or [])
    environment_lines = "\n".join(prog_info.get("environment_lines", []) or [])
    header_comment_lines = "\n".join(prog_info.get("header_comment_lines", []) or [])
    style_lines = "\n".join(prog_info.get("style_lines", []) or [])

    # Programmes DAL / BUSINESS
    dal_program = ""
    biz_program = ""
    for prog_yaml in norm.get("programmes", []):
        if prog_yaml.get("layer") == "dal":
            dal_program = prog_yaml.get("name", "")
        if prog_yaml.get("layer") == "business":
            biz_program = prog_yaml.get("name", "")

    # Exigences (toutes) + règles métier
    # UPDATED 2026-01-10: Filter requirements by layer to prevent logic leakage into BUSINESS
    all_requirements = norm.get("exigences", [])
    filtered_requirements = cobol_constraints.filter_requirements_by_layer(all_requirements, layer)

    business_rules = []
    exigences_lines: List[str] = []
    for req in filtered_requirements:
        req_id = req.get("id", "").strip()
        req_type = req.get("type", "").strip()
        req_rule = req.get("regle", "").strip()
        req_notes = req.get("notes", "").strip()
        if req_rule:
            label_parts = []
            if req_id:
                label_parts.append(req_id)
            if req_type:
                label_parts.append(req_type)
            label = "[" + "][".join(label_parts) + "] " if label_parts else ""
            line = f"{label}{req_rule}"
            if req_notes:
                line = f"{line} (notes: {req_notes})"
            exigences_lines.append(line)
        if req.get("type") in ["business", "regle_metier"] and req_rule:
            business_rules.append(req_rule)

    # Add note if requirements were filtered for BUSINESS layer
    if layer == "business" and len(filtered_requirements) < len(all_requirements):
        business_rules_text = "Aucune (regles metier dans LOGIC)"
        exigences_text = "Aucune (presentation seulement)"
    else:
        business_rules_text = "; ".join(business_rules) if business_rules else "Aucune"
        exigences_text = "\n".join(exigences_lines) if exigences_lines else "Aucune"

    # Interdictions dynamiques
    forbidden_items: List[str] = []
    if not prog_info.get("allow_display", False):
        forbidden_items.append("DISPLAY")
    if not prog_info.get("allowed_sql", False):
        forbidden_items.append("EXEC SQL")
    if layer == "business":
        forbidden_items.append("STOP RUN")
    forbidden_items.extend(prompting.get("forbidden_items", []) or [])
    forbidden_items.extend(prog_info.get("forbidden_items", []) or [])
    forbidden_text = ", ".join(forbidden_items) if forbidden_items else "Aucune"

    connection_details = _build_connection_details(norm)
    format_details = _build_format_details(norm)
    naming_conventions = _build_naming_conventions(norm)
    sql_behavior_details = _build_sql_behavior_details(norm)
    cursor_commit_policy_rules = _build_cursor_commit_policy_rules(norm, program_id)
    sql_formatting_details = _build_sql_formatting_details(norm)
    generation_strategy = "\n".join(prompting.get("generation_strategy", []) or []) or "Aucune"
    formatting_guidelines = "\n".join(prompting.get("formatting_guidelines", []) or [])
    entry_block_rules_text = _format_entry_block_rules(prog_info.get("entry_block_rules", []) or [])
    paragraph_constraints_text = _format_paragraph_constraints(prog_info.get("paragraph_constraints", []) or [])
    entry_exit_sequence_text = _format_entry_exit_sequence(prog_info.get("entry_exit_sequence", {}) or {})

    variables = {
        "program_id": program_id,
        "layer": layer,
        "entity": entity,
        "dialecte_cobol": norm.get("dialecte_cobol", ""),
        "sql_cible": norm.get("sql_cible", ""),
        "table_signature": table_signature,
        "entity_struct": entity_struct,
        "entity_fields_list": entity_fields_list,
        "linkage_struct": linkage_struct,
        "working_storage_lines": working_storage_lines,
        "environment_lines": environment_lines,
        "header_comment_lines": header_comment_lines,
        "style_lines": style_lines,
        "sql_statements": sql_text,
        "flow_notes": flow_notes,
        "logging_lines": logging_lines,
        "call_interface": call_interface_text,
        "public_paragraphs": ", ".join(public_paras) if public_paras else "(none)",
        "internal_paragraphs": ", ".join(internal_paras) if internal_paras else "(none)",
        "forbidden_items": forbidden_text,
        "function_details": "\n".join(fn_lines) if fn_lines else "(voir fonctions)",
        "display_lines": "",
        "business_rules": business_rules_text,
        "exigences_text": exigences_text,
        "dal_program": dal_program,
        "business_program": biz_program,
        "cursor_name": cursor_name,
        "eof_name": eof_name,
        "eof_pic": eof_pic,
        "connection_details": connection_details,
        "entry_paragraph": entry_paragraph,
        "procedure_division_line": procedure_division_line,
        "required_statements": ", ".join(prog_info.get("required_statements", []) or []) or "Aucun",
        "formatting_guidelines": formatting_guidelines,
        "format_details": format_details,
        "naming_conventions": naming_conventions,
        "sql_behavior_details": sql_behavior_details,
        "cursor_commit_policy_rules": cursor_commit_policy_rules,
        "sql_formatting_details": sql_formatting_details,
        "generation_strategy": generation_strategy,
        "entry_block_rules": entry_block_rules_text,
        "entry_exit_sequence": entry_exit_sequence_text,
        "paragraph_constraints": paragraph_constraints_text,
    }

    # display_lines via fonctions si dispo
    for fn in program_funcs:
        if fn.get("display_lines"):
            variables["display_lines"] = "\n".join(fn.get("display_lines", []))
            break

    global_constraints = prompting.get("global_constraints")
    if not isinstance(global_constraints, list) or not global_constraints:
        global_constraints = [
            "Reponds uniquement avec du code COBOL compilable, sans Markdown ni texte autour.",
            "CRITIQUE: NE PAS OUBLIER LE POINT FINAL POUR TERMINER LES PARAGRAPHES.",
            "La premiere ligne doit etre IDENTIFICATION DIVISION.",
            "Un seul PROGRAM-ID, pas de sous-programmes."
        ]
    global_directives = prompting.get("global_directives", []) or []

    program_prompt = (prompting.get("programs") or {}).get(program_id, {}) or {}
    sections_order = program_prompt.get("sections_order", []) or [
        "context", "interface", "structure", "working_storage", "sql",
        "flow", "logging", "constraints", "fonctions"
    ]
    if "requirements" not in sections_order:
        if "context" in sections_order:
            idx = sections_order.index("context") + 1
            sections_order.insert(idx, "requirements")
        else:
            sections_order.append("requirements")

    default_sections = {
        "strategy": [
            "{generation_strategy}"
        ],
        "context": [
            "Programme: {program_id} (couche {layer})",
            "SGBD: {sql_cible}. Dialecte COBOL: {dialecte_cobol}.",
            "Table {entity}: {table_signature}"
        ],
        "requirements": [
            "Exigences (spec):",
            "{exigences_text}"
        ],
        "style": [
            "Guidelines globales: {formatting_guidelines}",
            "Header commentaire (verbatim):",
            "{header_comment_lines}",
            "Style specifique: {style_lines}"
        ],
        "format": [
            "Format COBOL: {format_details}"
        ],
        "naming": [
            "Conventions de nommage: {naming_conventions}"
        ],
        "environment": [
            "{environment_lines}"
        ],
        "interface": [
            "Interface CALL/USING: {call_interface}"
        ],
        "structure": [
            "{linkage_struct}"
        ],
        "working_storage": [
            "{working_storage_lines}"
        ],
        "sql": [
            "{connection_details}",
            "{sql_statements}"
        ],
        "sql_format": [
            "{sql_formatting_details}"
        ],
        "sql_behavior": [
            "{sql_behavior_details}",
            "{cursor_commit_policy_rules}"
        ],
        "flow": [
            "{flow_notes}"
        ],
        "logging": [
            "{logging_lines}"
        ],
        "constraints": [
            "Interdictions: {forbidden_items}",
            "Paragraphes publics attendus: {public_paragraphs}",
            "Paragraphes internes autorises: {internal_paragraphs}",
            "Entry paragraph: {entry_paragraph}"
        ],
        "fonctions": [
            "{function_details}"
        ]
    }

    prompt_parts: List[str] = []
    prompt_parts.append("\n".join(global_constraints))
    if global_directives:
        prompt_parts.append("\n".join(global_directives))

    # ADDED 2026-01-10: Add hardcoded COBOL constraints (not in specs)
    prompt_parts.append("")
    prompt_parts.append(cobol_constraints.get_gnucobol_intrinsic_functions_guide())

    # ADDED 2026-01-11: Add OCESQL embedded SQL guide when SQL is detected
    has_sql = any(
        fn.get("sql_actions") or fn.get("sql")
        for fn in program_funcs
    )
    if has_sql:
        prompt_parts.append("")
        prompt_parts.append(cobol_constraints.get_ocesql_embedded_sql_guide())

    # ADDED 2026-01-10: Add strict BUSINESS layer rules if applicable
    if layer == "business":
        prompt_parts.append("")
        prompt_parts.append(cobol_constraints.get_business_layer_strict_rules())

    for section in sections_order:
        section_lines = program_prompt.get(section)
        if not section_lines:
            section_lines = default_sections.get(section, [])
        rendered = _render_lines(section_lines, variables)
        if not rendered:
            continue
        prompt_parts.append(f"\n=== {section.upper()} ===")
        prompt_parts.append("\n".join(rendered))

    task_line = program_prompt.get("task_line") or "Tache : genere le programme COBOL complet respectant ces contraintes (code seulement)."
    prompt_parts.append("")
    prompt_parts.append(task_line)

    return "\n".join(prompt_parts).strip()


def _validate_program(code: str, program_id: str, layer: str, entity: str, norm: Dict) -> List[str]:
    """Validation minimale (markdown, PROGRAM-ID, PIC, paragraphes, interdictions)."""
    errors: List[str] = []
    lines = [l.rstrip() for l in code.splitlines() if l.strip() != ""]
    upper_code = code.upper()
    prompting = norm.get("prompting", {}) or {}

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
    prog_info = _extract_program_info(norm, program_id)
    program_funcs = [fn for fn in norm.get("fonctions", []) if fn.get("programme") == program_id]
    public_required = [
        fn.get("name") for fn in program_funcs
        if fn.get("visibility", "public") != "internal" and fn.get("required", True) and fn.get("name")
    ]
    internal_allowed = [
        fn.get("name") for fn in program_funcs
        if fn.get("visibility") == "internal" and fn.get("name")
    ]
    required_internal = [
        fn.get("name") for fn in program_funcs
        if fn.get("visibility") == "internal" and fn.get("required", True) and fn.get("name")
    ]
    entry_para = (
        (prog_info.get("cobol_sections", {}) or {})
        .get("procedure_division", {})
        .get("entry_paragraph")
        or prog_info.get("entry_paragraph")
    )
    allowed = set(public_required + internal_allowed + (prog_info.get("internal_paragraphs", []) or []))
    if entry_para:
        allowed.add(entry_para)
    required = list(public_required)
    if entry_para:
        required.append(entry_para)
    required.extend(required_internal)

    if allowed:
        paras = []
        for l in lines:
            m = re.match(r"^\s*([A-Z0-9\-]+)\.", l)
            if not m:
                continue
            upper_line = l.upper()
            if any(skip in upper_line for skip in ("DIVISION", "SECTION", "PROGRAM-ID")):
                continue
            keyword = m.group(1).upper()
            if keyword in {
                "REPOSITORY", "CONFIGURATION", "GOBACK", "STOP", "STOP-RUN",
                "END-IF", "END-EVALUATE", "END-PERFORM", "END-EXEC"
            }:
                continue
            paras.append(m.group(1))
        missing = [p for p in required if p not in paras]
        extra = [p for p in paras if p not in allowed]
        if missing:
            errors.append(f"Paragraphes manquants: {', '.join(missing)}.")
        if extra:
            errors.append(f"Paragraphes en trop: {', '.join(extra)}.")

        # Paragraphe sans point final
        for name in allowed:
            for l in lines:
                if re.match(rf"^\s*{re.escape(name)}\s*$", l):
                    errors.append(f"Le paragraphe {name} doit etre declare avec un point (ex: {name}.).")
                    break

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

    # ENVIRONMENT lines required
    env_lines = prog_info.get("environment_lines", []) or []
    if env_lines:
        def _norm_line(s: str) -> str:
            return re.sub(r"\s+", " ", s.strip()).upper()

        code_norm = {_norm_line(l) for l in lines}
        for env_line in env_lines:
            if not isinstance(env_line, str):
                continue
            if _norm_line(env_line) and _norm_line(env_line) not in code_norm:
                errors.append(f"Ligne ENVIRONMENT manquante: {env_line}")

    # Required lines (ex: header comment block)
    required_lines = prog_info.get("required_lines", []) or []
    if required_lines:
        def _norm_line(s: str) -> str:
            return re.sub(r"\s+", " ", s.strip()).upper()

        code_norm = {_norm_line(l) for l in lines}
        for req_line in required_lines:
            if not isinstance(req_line, str):
                continue
            if _norm_line(req_line) and _norm_line(req_line) not in code_norm:
                errors.append(f"Ligne requise manquante: {req_line}")

    # Required statements from spec
    required_statements = prog_info.get("required_statements", []) or []
    for stmt in required_statements:
        if not isinstance(stmt, str) or not stmt.strip():
            continue
        if stmt.upper() not in upper_code:
            errors.append(f"Instruction obligatoire manquante: {stmt}")

    # Procedure sections forbidden
    if prog_info.get("allow_procedure_sections") is False:
        in_proc = False
        for l in lines:
            up = l.upper()
            if "PROCEDURE DIVISION" in up:
                in_proc = True
                continue
            if not in_proc:
                continue
            if "SECTION." in up:
                errors.append("SECTION interdite dans PROCEDURE DIVISION (spec).")
            break

    # Longueur de ligne en format FIXED
    fmt = norm.get("cobol_format", {}) or {}
    max_len = fmt.get("max_line_length")
    if isinstance(max_len, int) and max_len > 0:
        for line in lines:
            if len(line) > max_len:
                errors.append(f"Ligne trop longue (> {max_len}): {line.strip()}")
                break

    # Interdictions spécifiques
    allow_display = prog_info.get("allow_display", False)
    allow_sql = prog_info.get("allowed_sql", False)
    if not allow_display and "DISPLAY" in code.upper():
        errors.append("DISPLAY interdit par la spec.")
    if allow_sql:
        if "EXEC SQL" not in code.upper():
            errors.append("Le programme doit contenir des EXEC SQL (spec).")
        # Assure SQLCA si SQL present (OCESQL en depend)
        if "EXEC SQL" in upper_code:
            has_sqlca = (
                "INCLUDE SQLCA" in upper_code
                or "COPY SQLCA" in upper_code
                or "COPY \"SQLCA" in upper_code
                or "01  SQLCA." in upper_code
            )
            if not has_sqlca:
                errors.append("SQLCA manquante: ajouter EXEC SQL INCLUDE SQLCA END-EXEC.")
    else:
        if "EXEC SQL" in code.upper():
            errors.append("EXEC SQL interdit par la spec.")
    if layer == "business" and "STOP RUN" in upper_code:
        errors.append("STOP RUN interdit par la spec pour la couche business.")

    # Verifier la presence d'un point uniquement sur ligne seule (pas en fin d'instruction)
    def _is_allowed_dot_line(line: str) -> bool:
        stripped = line.strip()
        if stripped == ".":
            return True
        if re.match(r"^[A-Z0-9\-]+\.$", stripped):
            return True
        upper = stripped.upper()
        if "DIVISION." in upper or "SECTION." in upper:
            return True
        if upper.startswith("PROGRAM-ID."):
            return True
        if upper.startswith("EXEC SQL") and "END-EXEC." in upper:
            return True
        return False

    def _is_data_entry(line: str) -> bool:
        return re.match(r"^\s*(01|05|77|88)\b", line) is not None

    in_data = False
    in_proc = False
    for line in lines:
        upper = line.strip().upper()
        if "DATA DIVISION" in upper:
            in_data = True
        if "PROCEDURE DIVISION" in upper:
            in_proc = True
        if line.strip().endswith(".") and not _is_allowed_dot_line(line):
            if in_data and not in_proc and _is_data_entry(line):
                continue
            errors.append("Point final interdit sur instruction (utiliser '.' uniquement sur ligne seule).")
            break

    # DATA DIVISION: lignes avec PIC/VALUE doivent finir par un point
    in_data = False
    in_proc = False
    for line in lines:
        upper = line.strip().upper()
        if "DATA DIVISION" in upper:
            in_data = True
        if "PROCEDURE DIVISION" in upper:
            in_proc = True
        if not in_data or in_proc:
            continue
        if _is_data_entry(line) and re.search(r"\b(PIC|VALUE)\b", upper):
            if not line.strip().endswith("."):
                errors.append(
                    "Ligne DATA avec PIC/VALUE doit se terminer par un point."
                )
                break

    # Interdictions explicites (spec globale + programme)
    forbidden_spec: List[str] = []
    forbidden_spec.extend(prompting.get("forbidden_items", []) or [])
    forbidden_spec.extend(prog_info.get("forbidden_items", []) or [])
    for item in forbidden_spec:
        if isinstance(item, str) and item.strip():
            if item.upper() in upper_code:
                errors.append(f"Element interdit present: {item}")

    # Divisions/sections requises
    sections = prog_info.get("cobol_sections", {}) or {}
    if sections.get("identification_division") == "required":
        if "IDENTIFICATION DIVISION" not in upper_code:
            errors.append("IDENTIFICATION DIVISION manquante (spec).")
    if sections.get("environment_division") == "required":
        if "ENVIRONMENT DIVISION" not in upper_code:
            errors.append("ENVIRONMENT DIVISION manquante (spec).")
    data_div = sections.get("data_division", {}) or {}
    if data_div.get("linkage") == "required":
        if "LINKAGE SECTION" not in upper_code:
            errors.append("LINKAGE SECTION manquante (spec).")
    if sections.get("procedure_division") is not None:
        if "PROCEDURE DIVISION" not in upper_code:
            errors.append("PROCEDURE DIVISION manquante (spec).")

    # Format COBOL (directives interdites)
    fmt = norm.get("cobol_format", {}) or {}
    allow_source = fmt.get("allow_source_format_directive")
    if allow_source is False and "SOURCE FORMAT" in upper_code:
        errors.append("Directive SOURCE FORMAT interdite par la spec.")

    # ADDED 2026-01-11: Validations CRITIQUES
    in_procedure = False
    for i, l in enumerate(lines):
        # Detecter PROCEDURE DIVISION
        if "PROCEDURE DIVISION" in l.upper():
            in_procedure = True
            continue

        # CRITIQUE: Pas de ligne avec juste un point dans PROCEDURE DIVISION
        if in_procedure and re.match(r"^\s*\.\s*$", l):
            errors.append("Point final interdit sur instruction (utiliser '.' uniquement sur ligne seule).")

        # CRITIQUE: EXEC SQL INCLUDE SQLCA doit avoir un point
        if "EXEC SQL INCLUDE SQLCA END-EXEC" in l.upper() and not l.strip().endswith("."):
            errors.append("EXEC SQL INCLUDE SQLCA END-EXEC doit se terminer par un point.")

    # Contraintes de bloc dans le paragraphe d'entree
    entry_block_rules = prog_info.get("entry_block_rules", []) or []
    if entry_block_rules and prog_info.get("cobol_sections", {}).get("procedure_division", {}):
        entry_para = (
            (prog_info.get("cobol_sections", {}) or {})
            .get("procedure_division", {})
            .get("entry_paragraph")
            or prog_info.get("entry_paragraph")
        )

        def _paragraph_ranges(lines_list: List[str]) -> Dict[str, tuple]:
            ranges: Dict[str, tuple] = {}
            idxs = []
            for i, l in enumerate(lines_list):
                m = re.match(r"^\s*([A-Z0-9\-]+)\.\s*$", l)
                if m:
                    idxs.append((m.group(1), i))
            for j, (name, start) in enumerate(idxs):
                end = idxs[j + 1][1] if j + 1 < len(idxs) else len(lines_list)
                ranges[name] = (start, end)
            return ranges

        para_ranges = _paragraph_ranges(lines)
        if entry_para and entry_para in para_ranges:
            start, end = para_ranges[entry_para]
            para_lines = lines[start:end]
            for rule in entry_block_rules:
                if not isinstance(rule, dict):
                    continue
                cond_regex = rule.get("condition_regex")
                must_contain = rule.get("must_contain", []) or []
                if not cond_regex:
                    continue
                cond_idx = None
                cond_re = re.compile(cond_regex, re.IGNORECASE)
                for i, l in enumerate(para_lines):
                    if cond_re.search(l):
                        cond_idx = i
                        break
                if cond_idx is None:
                    errors.append(f"Condition de bloc manquante: {cond_regex}")
                    continue
                depth = 0
                end_idx = None
                for i in range(cond_idx, len(para_lines)):
                    up = para_lines[i].strip().upper()
                    if re.match(r"^IF\\b", up):
                        depth += 1
                    if "END-IF" in up:
                        depth -= 1
                        if depth == 0:
                            end_idx = i
                            break
                if end_idx is None:
                    errors.append("END-IF manquant pour la condition de bloc MAIN-ENTRY.")
                    continue
                block_lines = "\n".join(para_lines[cond_idx + 1:end_idx]).upper()
                for item in must_contain:
                    if isinstance(item, str) and item.upper() not in block_lines:
                        errors.append(f"Instruction manquante dans le bloc MAIN-ENTRY: {item}")
        else:
            errors.append("Entry paragraph introuvable pour les regles de bloc.")

    # Sequence de sortie obligatoire du paragraphe d'entree
    entry_exit = prog_info.get("entry_exit_sequence", {}) or {}
    if entry_exit:
        para_name = entry_exit.get("paragraph")
        must_order = entry_exit.get("must_follow_order", []) or []

        def _paragraph_ranges(lines_list: List[str]) -> Dict[str, tuple]:
            ranges: Dict[str, tuple] = {}
            idxs = []
            for i, l in enumerate(lines_list):
                m = re.match(r"^\s*([A-Z0-9\-]+)\.\s*$", l)
                if m:
                    idxs.append((m.group(1), i))
            for j, (name, start) in enumerate(idxs):
                end = idxs[j + 1][1] if j + 1 < len(idxs) else len(lines_list)
                ranges[name] = (start, end)
            return ranges

        para_ranges = _paragraph_ranges(lines)
        if para_name and para_name in para_ranges:
            start, end = para_ranges[para_name]
            para_lines = lines[start:end]
            idx = -1
            for token in must_order:
                if token == ".":
                    found = next((i for i in range(idx + 1, len(para_lines)) if para_lines[i].strip() == "."), None)
                else:
                    found = next((i for i in range(idx + 1, len(para_lines)) if token.upper() in para_lines[i].upper()), None)
                if found is None:
                    errors.append(f"Sequence MAIN-ENTRY incomplete: {token} manquant.")
                    break
                idx = found
        else:
            errors.append("Entry paragraph introuvable pour la sequence de sortie.")

    # Contraintes par paragraphe (contenu requis/interdit)
    paragraph_constraints = prog_info.get("paragraph_constraints", []) or []
    if paragraph_constraints:
        def _paragraph_ranges(lines_list: List[str]) -> Dict[str, tuple]:
            ranges: Dict[str, tuple] = {}
            idxs = []
            for i, l in enumerate(lines_list):
                m = re.match(r"^\s*([A-Z0-9\-]+)\.\s*$", l)
                if m:
                    idxs.append((m.group(1), i))
            for j, (name, start) in enumerate(idxs):
                end = idxs[j + 1][1] if j + 1 < len(idxs) else len(lines_list)
                ranges[name] = (start, end)
            return ranges

        para_ranges = _paragraph_ranges(lines)
        for rule in paragraph_constraints:
            if not isinstance(rule, dict):
                continue
            para = rule.get("paragraph")
            if not para or para not in para_ranges:
                continue
            start, end = para_ranges[para]
            para_text = "\n".join(lines[start:end]).upper()
            for item in rule.get("require_contains", []) or []:
                if isinstance(item, str) and item.upper() not in para_text:
                    errors.append(f"Instruction requise manquante dans {para}: {item}")
            for item in rule.get("forbid_contains", []) or []:
                if isinstance(item, str) and item.upper() in para_text:
                    errors.append(f"Instruction interdite dans {para}: {item}")

    # Variables WS supplémentaires : autoriser uniquement celles de la spec
    allowed_ws = set()
    for ws_line in prog_info.get("working_storage_lines", []) or []:
        m = re.match(r"^\s*(0?1|77|88)\s+([A-Z0-9\-]+)", ws_line)
        if m:
            allowed_ws.add(m.group(2))
    if eof_name:
        allowed_ws.add(eof_name)
    if entity:
        allowed_ws.add(entity.upper())
    ws_names = []
    in_ws = False
    for l in lines:
        upper = l.upper()
        if "WORKING-STORAGE SECTION" in upper:
            in_ws = True
            continue
        if "LINKAGE SECTION" in upper or "PROCEDURE DIVISION" in upper:
            in_ws = False
        if not in_ws:
            continue
        m = re.match(r"^\s*(0?1|77)\s+([A-Z0-9\-]+)", l)
        if m:
            ws_names.append(m.group(2))
    if prog_info.get("require_working_storage_section"):
        if "WORKING-STORAGE SECTION" not in upper_code:
            errors.append("WORKING-STORAGE SECTION manquant (spec).")

    if allowed_ws:
        extra_ws = [w for w in ws_names if w not in allowed_ws]
        if extra_ws:
            errors.append(f"Variables WORKING-STORAGE en trop: {', '.join(extra_ws)}.")

    return errors

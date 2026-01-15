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
from cobolsmartgen1.adapters import llm_auto
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


def _order_ws_lines_for_prompt(ws_lines: List[str], prefer_sqlca_first: bool) -> List[str]:
    if not prefer_sqlca_first or not ws_lines:
        return ws_lines
    sqlca_lines = []
    other_lines = []
    for line in ws_lines:
        if isinstance(line, str) and "EXEC SQL INCLUDE SQLCA" in line.upper():
            sqlca_lines.append(line)
        else:
            other_lines.append(line)
    return sqlca_lines + other_lines if sqlca_lines else ws_lines


def _ensure_sqlca_top_of_ws(code: str) -> str:
    lines = code.splitlines()
    ws_idx = None
    for i, line in enumerate(lines):
        if "WORKING-STORAGE SECTION" in line.upper():
            ws_idx = i
            break
    if ws_idx is None:
        return code

    sqlca_idx = None
    for i in range(ws_idx + 1, len(lines)):
        up = lines[i].upper()
        if "LINKAGE SECTION" in up or "PROCEDURE DIVISION" in up:
            break
        if "EXEC SQL INCLUDE SQLCA END-EXEC" in up:
            sqlca_idx = i
            break

    if sqlca_idx is None:
        return code

    first_decl_idx = None
    for i in range(ws_idx + 1, len(lines)):
        up = lines[i].upper()
        if "LINKAGE SECTION" in up or "PROCEDURE DIVISION" in up:
            break
        if re.match(r"^\s*(01|05|77|88)\b", lines[i]):
            first_decl_idx = i
            break

    if first_decl_idx is None or sqlca_idx <= first_decl_idx:
        return code

    sqlca_line = lines.pop(sqlca_idx)
    insert_at = ws_idx + 1
    lines.insert(insert_at, sqlca_line)
    return "\n".join(lines)

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
    # FIX 2026-01-12: Si le programme a une clé linkage_structure dans la spec YAML,
    # l'utiliser directement au lieu de la reconstruire (pour avoir tous les champs JOIN, CLIENT, etc.)
    if prog_info and prog_info.get("linkage_structure"):
        linkage_str = prog_info.get("linkage_structure", "").strip()
        if linkage_str:
            # Si la structure ne commence pas par LINKAGE SECTION, l'ajouter
            if not linkage_str.upper().startswith("LINKAGE SECTION"):
                return "LINKAGE SECTION.\n" + linkage_str
            return linkage_str

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

def _format_conn_vars(conn_vars: object) -> List[str]:
    items: List[str] = []
    if isinstance(conn_vars, dict):
        for key, val in conn_vars.items():
            if val in [None, ""]:
                continue
            if key:
                items.append(f"{key}={val}")
            else:
                items.append(str(val))
    elif isinstance(conn_vars, (list, tuple)):
        for entry in conn_vars:
            if isinstance(entry, dict):
                role = entry.get("role") or entry.get("key") or entry.get("name")
                var = entry.get("var") or entry.get("value") or entry.get("variable")
                if role and var:
                    items.append(f"{role}={var}")
                elif var:
                    items.append(str(var))
                elif role:
                    items.append(str(role))
            elif isinstance(entry, str) and entry.strip():
                items.append(entry.strip())
    elif isinstance(conn_vars, str) and conn_vars.strip():
        items.append(conn_vars.strip())
    return items

def _normalize_conn_vars(conn_vars: object) -> Dict[str, str]:
    mapping: Dict[str, str] = {}
    if isinstance(conn_vars, dict):
        for key, val in conn_vars.items():
            if not key or val in [None, ""]:
                continue
            mapping[str(key).strip().lower()] = str(val).strip()
    elif isinstance(conn_vars, (list, tuple)):
        for entry in conn_vars:
            if isinstance(entry, dict):
                role = entry.get("role") or entry.get("key") or entry.get("name")
                var = entry.get("var") or entry.get("value") or entry.get("variable")
                if role and var:
                    mapping[str(role).strip().lower()] = str(var).strip()
    return mapping

def _normalize_env_vars(env_vars: object) -> List[str]:
    if isinstance(env_vars, dict):
        return [str(k) for k in env_vars.keys() if k]
    if isinstance(env_vars, (list, tuple)):
        return [str(v) for v in env_vars if v]
    if isinstance(env_vars, str) and env_vars.strip():
        return [env_vars.strip()]
    return []

def _infer_env_mapping(env_vars: List[str], conn_vars: Dict[str, str]) -> List[str]:
    mapping: List[str] = []
    for env in env_vars:
        env_upper = env.upper()
        role = None
        if "PASS" in env_upper:
            role = "password"
        elif "USER" in env_upper:
            role = "user"
        elif "DATABASE" in env_upper or env_upper.endswith("DB") or "DBNAME" in env_upper:
            role = "dbname"
        elif "HOST" in env_upper:
            role = "host"
        elif "PORT" in env_upper:
            role = "port"
        if role and conn_vars.get(role):
            mapping.append(f"{env}->{conn_vars[role]}")
    return mapping

def _extract_ws_names(ws_lines: List[str]) -> List[str]:
    names = set()
    for line in ws_lines or []:
        if not isinstance(line, str):
            continue
        m = re.match(r"^\s*\d+\s+([A-Z0-9-]+)\b", line.strip().upper())
        if m:
            names.add(m.group(1))
    return sorted(names)

def _extract_flag_names(ws_lines: List[str]) -> List[str]:
    names = _extract_ws_names(ws_lines)
    return [n for n in names if n.endswith("-FLAG")]

def _get_flag_values(norm: Dict) -> tuple[str, str] | None:
    eof = (norm.get("technique", {}) or {}).get("eof_flag", {}) if norm else {}
    true_val = eof.get("true_value")
    false_val = eof.get("false_value")
    if true_val and false_val:
        return str(true_val), str(false_val)
    return None

def _normalize_cobol_name(name: str) -> str:
    if not name:
        return ""
    return re.sub(r"[^A-Z0-9\-]", "-", name.upper().replace("_", "-"))

def _extract_ws_names_ordered(ws_lines: List[str]) -> List[str]:
    names: List[str] = []
    seen = set()
    for line in ws_lines or []:
        if not isinstance(line, str):
            continue
        m = re.match(r"^\s*\d+\s+([A-Z0-9-]+)\b", line.strip().upper())
        if not m:
            continue
        name = m.group(1)
        if name not in seen:
            names.append(name)
            seen.add(name)
    return names

def _get_entity_attrs(norm: Dict, entity_name: str) -> List[Dict]:
    for ent in (norm.get("mcd", {}) or {}).get("entites", []) or []:
        if ent.get("name") == entity_name or ent.get("normalized_name") == entity_name:
            return ent.get("attrs", []) or []
    return []

def _find_operation_param(call_iface: Dict) -> str | None:
    for p in call_iface.get("linkage_parameters", []) or []:
        name = p.get("name") or ""
        if "OPERATION" in name.upper():
            return name
    return None

def _find_eof_param(call_iface: Dict, norm: Dict) -> str:
    for p in call_iface.get("linkage_parameters", []) or []:
        name = p.get("name") or ""
        if "END-OF-FILE" in name.upper() or "EOF" in name.upper():
            return name
    eof = (norm.get("technique", {}) or {}).get("eof_flag", {}) if norm else {}
    return eof.get("name", "END-OF-FILE")

def _find_flag_name(ws_names: List[str], keyword: str) -> str | None:
    for name in ws_names:
        if keyword.upper() in name.upper():
            return name
    return None

def _score_name_match(ws_base: str, attr_cob: str, entity_cob: str) -> int:
    if not ws_base or not attr_cob:
        return 0
    if ws_base == attr_cob:
        return 100
    if ws_base.endswith(attr_cob) or attr_cob.endswith(ws_base):
        return 90
    attr_no_entity = attr_cob
    if entity_cob and attr_cob.startswith(entity_cob + "-"):
        attr_no_entity = attr_cob[len(entity_cob) + 1:]
    if attr_no_entity and (ws_base.endswith(attr_no_entity) or attr_no_entity.endswith(ws_base)):
        return 80
    ws_tokens = [t for t in ws_base.split("-") if t]
    attr_tokens = [t for t in attr_cob.split("-") if t]
    overlap = len(set(ws_tokens) & set(attr_tokens))
    if overlap >= 2:
        return 60 + overlap
    if overlap == 1:
        return 50
    return 0

def _filter_ws_candidates(ws_lines: List[str], conn_vars_map: Dict[str, str]) -> List[str]:
    ws_names = _extract_ws_names_ordered(ws_lines)
    conn_vars_upper = {v.upper() for v in conn_vars_map.values() if v}
    candidates: List[str] = []
    for name in ws_names:
        upper = name.upper()
        if upper in conn_vars_upper:
            continue
        if upper.endswith("-FLAG"):
            continue
        if upper.startswith("WS-DB-") or upper.startswith("DB-"):
            continue
        if "SQLCA" in upper or "SQLSTATE" in upper or "SQLCODE" in upper:
            continue
        if "CURSOR" in upper:
            continue
        candidates.append(upper)
    return candidates

def _map_attrs_to_ws(attrs: List[Dict], ws_candidates: List[str], entity_name: str) -> Dict[str, str]:
    mapping: Dict[str, str] = {}
    used = set()
    entity_cob = _normalize_cobol_name(entity_name)
    for attr in attrs:
        attr_name = attr.get("name") or ""
        attr_cob = _normalize_cobol_name(attr_name)
        best_ws = None
        best_score = 0
        for ws in ws_candidates:
            if ws in used:
                continue
            ws_base = ws.upper()
            if ws_base.startswith("WS-"):
                ws_base = ws_base[3:]
            score = _score_name_match(ws_base, attr_cob, entity_cob)
            if score > best_score:
                best_score = score
                best_ws = ws
        if best_ws and best_score >= 60:
            mapping[attr_name] = best_ws
            used.add(best_ws)

    remaining_attrs = [a for a in attrs if a.get("name") not in mapping]
    remaining_ws = [w for w in ws_candidates if w not in used]
    for attr, ws in zip(remaining_attrs, remaining_ws):
        mapping[attr.get("name") or ""] = ws
    return mapping

def _build_sql_for_dal(entity: str, cursor_name: str, attrs: List[Dict], ws_map: Dict[str, str]) -> Dict[str, str]:
    columns = [a.get("name") for a in attrs if a.get("name")]
    pk_cols = [a.get("name") for a in attrs if a.get("name") and a.get("pk")]
    non_pk_cols = [a.get("name") for a in attrs if a.get("name") and not a.get("pk")]
    if not pk_cols:
        pk_cols = columns[:1]
    if not non_pk_cols:
        non_pk_cols = [c for c in columns if c not in pk_cols] or columns

    declare_lines = [
        f"DECLARE {cursor_name} CURSOR FOR",
        f"SELECT {', '.join(columns)}",
        f"FROM {entity}",
    ]
    if pk_cols:
        declare_lines.append(f"ORDER BY {', '.join(pk_cols)}")
    declare_cursor = "\n".join(declare_lines) + "\n"

    fetch_vars: List[str] = []
    for col in columns:
        ws_var = ws_map.get(col) or ws_map.get(col.upper()) or ws_map.get(col.lower())
        if ws_var:
            fetch_vars.append(f":{ws_var}")
    fetch_into = f"FETCH {cursor_name} INTO " + ", ".join(fetch_vars) if fetch_vars else ""

    set_lines: List[str] = []
    for col in non_pk_cols:
        ws_var = ws_map.get(col) or ws_map.get(col.upper()) or ws_map.get(col.lower())
        if ws_var:
            set_lines.append(f"{col} = :{ws_var}")
    where_lines: List[str] = []
    for col in pk_cols:
        ws_var = ws_map.get(col) or ws_map.get(col.upper()) or ws_map.get(col.lower())
        if ws_var:
            where_lines.append(f"{col} = :{ws_var}")

    update_stmt = ""
    if set_lines:
        update_stmt = "UPDATE " + entity + "\nSET " + ",\n    ".join(set_lines)
        if where_lines:
            update_stmt += "\nWHERE " + " AND ".join(where_lines)
        update_stmt += "\n"

    return {
        "declare_cursor": declare_cursor,
        "open_cursor": f"OPEN {cursor_name}",
        "fetch_into": fetch_into,
        "close_cursor": f"CLOSE {cursor_name}",
        "update_statement": update_stmt,
        "commit": "COMMIT",
        "disconnect": "DISCONNECT ALL",
    }

def _augment_missing_dal_functions(norm: Dict) -> None:
    if not norm:
        return
    programmes = norm.get("programmes", []) or []
    fonctions = norm.get("fonctions")
    if fonctions is None:
        fonctions = []
        norm["fonctions"] = fonctions

    conn = (norm.get("sql", {}) or {}).get("connection", {}) if norm else {}
    conn_vars = conn.get("vars") or conn.get("variables") or conn.get("var_map")
    conn_vars_map = _normalize_conn_vars(conn_vars)
    env_vars = _normalize_env_vars(conn.get("env_vars"))
    env_mapping = _infer_env_mapping(env_vars, conn_vars_map)
    flag_values = _get_flag_values(norm)
    true_val = flag_values[0] if flag_values else "Y"
    false_val = flag_values[1] if flag_values else "N"

    for prog in programmes:
        if (prog.get("layer") or "").lower() != "dal":
            continue
        program_id = prog.get("name")
        if not program_id:
            continue
        call_iface = prog.get("call_interface", {}) or {}
        ops = call_iface.get("operations", {}) or {}
        if not ops:
            continue
        entity_list = prog.get("entities", []) or []
        entity = entity_list[0] if isinstance(entity_list, list) and entity_list else (entity_list if isinstance(entity_list, str) else "")
        if not entity:
            continue

        attrs = _get_entity_attrs(norm, entity)
        if not attrs:
            continue

        existing = [fn for fn in fonctions if fn.get("programme") == program_id]
        existing_names = {fn.get("name") for fn in existing if fn.get("name")}

        ws_lines = prog.get("working_storage_lines", []) or []
        ws_names_ordered = _extract_ws_names_ordered(ws_lines)
        ws_names_set = {n.upper() for n in ws_names_ordered}
        ws_candidates = _filter_ws_candidates(ws_lines, conn_vars_map)
        ws_map = _map_attrs_to_ws(attrs, ws_candidates, entity)
        cursor_name = prog.get("main_cursor") or ""
        if not cursor_name:
            cursor_example = (
                (norm.get("technique", {}) or {})
                .get("cursor_naming", {})
                .get("example", {})
                .get(entity)
            )
            if cursor_example:
                cursor_name = cursor_example
        sql_block = _build_sql_for_dal(entity, cursor_name, attrs, ws_map) if cursor_name else {}

        op_param = _find_operation_param(call_iface) or "LK-OPERATION"
        eof_param = _find_eof_param(call_iface, norm)
        connected_flag = _find_flag_name(ws_names_ordered, "CONNECTED")
        cursor_flag = _find_flag_name(ws_names_ordered, "CURSOR")
        unknown_msg = ""
        for line in prog.get("logging_lines", []) or []:
            if isinstance(line, str) and "Operation inconnue" in line:
                unknown_msg = line
                break

        flow_lines = prog.get("flow", []) or []
        mapping_parts = []
        for key, val in ops.items():
            fn_name = f"DAL-{str(key).upper()}"
            mapping_parts.append(f"'{val}' -> {fn_name}")
        if mapping_parts:
            flow_line = f"MAIN-ENTRY: router {op_param} ({', '.join(mapping_parts)})."
            if unknown_msg:
                flow_line += f" WHEN OTHER: afficher {unknown_msg}"
            if flow_line not in flow_lines:
                flow_lines.append(flow_line)
                prog["flow"] = flow_lines

        env_mapping_use = []
        for mapping in env_mapping:
            if "->" not in mapping:
                continue
            env_name, ws_var = mapping.split("->", 1)
            if ws_var.strip().upper() in ws_names_set:
                env_mapping_use.append(mapping)

        if "DAL-CONNECT" not in existing_names:
            steps = []
            if connected_flag:
                steps.append(f"Si {connected_flag} = '{true_val}', sortir sans reconnecter.")
            if env_mapping_use:
                steps.append("Initialiser les variables de connexion via l'environnement externe (mapping env_vars).")
            steps.append("Etablir la connexion SQL embarquee avec les variables de connexion de WORKING-STORAGE.")
            steps.append("Si SQLCODE < 0: journaliser l'erreur et arreter le flux logique.")
            fonctions.append({
                "id": f"AUTO-{program_id}-DAL-CONNECT",
                "name": "DAL-CONNECT",
                "programme": program_id,
                "layer": "dal",
                "visibility": "internal",
                "required": True,
                "entity": entity,
                "description": "Connexion SQL embarquee paresseuse (variables de connexion issues de la spec).",
                "steps": steps,
                "sql": {
                    "connection_method": "embedded_sql_precompile (voir sql.connection)",
                },
            })
            existing_names.add("DAL-CONNECT")

        if "DAL-SET-ENV" in (prog.get("internal_paragraphs", []) or []) and "DAL-SET-ENV" not in existing_names:
            steps = []
            if env_mapping_use:
                for mapping in env_mapping_use:
                    if "->" in mapping:
                        env_name, ws_var = mapping.split("->", 1)
                        steps.append(f"Charger {ws_var} depuis la variable d'environnement {env_name}.")
            else:
                steps.append("Charger les variables de connexion depuis l'environnement externe (voir sql.connection).")
            fonctions.append({
                "id": f"AUTO-{program_id}-DAL-SET-ENV",
                "name": "DAL-SET-ENV",
                "programme": program_id,
                "layer": "dal",
                "visibility": "internal",
                "required": True,
                "entity": entity,
                "description": "Initialisation des variables d'environnement de connexion.",
                "steps": steps,
            })
            existing_names.add("DAL-SET-ENV")

        for key, val in ops.items():
            fn_name = f"DAL-{str(key).upper()}"
            if fn_name in existing_names:
                continue
            op_value = str(val)
            desc = f"Operation DAL pour OPERATION='{op_value}'."
            steps: List[str] = []
            if fn_name == "DAL-READ":
                if connected_flag:
                    steps.append(f"Si {connected_flag} != '{true_val}', appeler DAL-CONNECT.")
                if cursor_flag:
                    if cursor_name:
                        steps.append(f"Si {cursor_flag} != '{true_val}', declarer et ouvrir le curseur {cursor_name}.")
                    else:
                        steps.append(f"Si {cursor_flag} != '{true_val}', declarer et ouvrir le curseur principal (spec).")
                steps.append(f"Avant FETCH, mettre {eof_param} a '{false_val}'.")
                steps.append("Effectuer le FETCH et remplir les variables WS dans le meme ordre que les colonnes SQL.")
                steps.append("Si SQLCODE = 0, copier WS -> LK pour chaque champ de l'entite.")
                steps.append(f"Si SQLCODE = 100, mettre {eof_param} a '{true_val}'.")
                steps.append(f"Sinon, journaliser l'erreur et mettre {eof_param} a '{true_val}'.")
                sql_read = {}
                for key in ("declare_cursor", "open_cursor", "fetch_into"):
                    val = sql_block.get(key) if sql_block else None
                    if val:
                        sql_read[key] = val
                fn_payload = {
                    "id": f"AUTO-{program_id}-DAL-READ",
                    "name": "DAL-READ",
                    "programme": program_id,
                    "layer": "dal",
                    "visibility": "public",
                    "required": True,
                    "entity": entity,
                    "description": desc,
                    "operation_code": op_value,
                    "steps": steps,
                    "sql": sql_read,
                    "sql_actions": ["declare_cursor", "open_cursor", "fetch_cursor", "check_sqlcode", "map_to_linkage", "set_eof"],
                    "eof_logic": {
                        "eof_flag_name": eof_param,
                        "eof_true_value": true_val,
                        "eof_false_value": false_val,
                    },
                }
                fonctions.append(fn_payload)
            elif fn_name == "DAL-SAVE":
                steps.append("Copier les champs LK -> WS pour les colonnes a mettre a jour.")
                steps.append("Executer l'UPDATE avec les colonnes non-PK et une clause WHERE sur la cle primaire.")
                steps.append("Si SQLCODE != 0, journaliser l'erreur SQLCODE.")
                sql_save = {}
                if sql_block.get("update_statement"):
                    sql_save["update_statement"] = sql_block.get("update_statement")
                fn_payload = {
                    "id": f"AUTO-{program_id}-DAL-SAVE",
                    "name": "DAL-SAVE",
                    "programme": program_id,
                    "layer": "dal",
                    "visibility": "public",
                    "required": True,
                    "entity": entity,
                    "description": desc,
                    "operation_code": op_value,
                    "steps": steps,
                    "sql": sql_save,
                }
                fonctions.append(fn_payload)
            elif fn_name == "DAL-END":
                steps.append("Fermer le curseur s'il est ouvert.")
                steps.append("Executer COMMIT puis DISCONNECT ALL.")
                if cursor_flag:
                    steps.append(f"Mettre {cursor_flag} a '{false_val}'.")
                if connected_flag:
                    steps.append(f"Mettre {connected_flag} a '{false_val}'.")
                sql_end = {}
                for key in ("close_cursor", "commit", "disconnect"):
                    val = sql_block.get(key) if sql_block else None
                    if val:
                        sql_end[key] = val
                fn_payload = {
                    "id": f"AUTO-{program_id}-DAL-END",
                    "name": "DAL-END",
                    "programme": program_id,
                    "layer": "dal",
                    "visibility": "public",
                    "required": True,
                    "entity": entity,
                    "description": desc,
                    "operation_code": op_value,
                    "steps": steps,
                    "sql": sql_end,
                }
                fonctions.append(fn_payload)
            else:
                steps.append(f"Appliquer l'operation '{op_value}' selon la spec.")
                fonctions.append({
                    "id": f"AUTO-{program_id}-{fn_name}",
                    "name": fn_name,
                    "programme": program_id,
                    "layer": "dal",
                    "visibility": "public",
                    "required": True,
                    "entity": entity,
                    "description": desc,
                    "operation_code": op_value,
                    "steps": steps,
                })

def _build_connection_details(norm: Dict, prog_info: Dict | None = None) -> str:
    conn = (norm.get("sql", {}) or {}).get("connection", {}) if norm else {}
    if not conn:
        return ""
    lines: List[str] = []

    details: List[str] = []
    engine = conn.get("engine")
    method = conn.get("method")
    host = conn.get("host")
    port = conn.get("port")
    dbname = conn.get("dbname")
    user = conn.get("user")
    env_vars = conn.get("env_vars")
    env_vars_text = ""
    has_inline_values = bool(host or dbname or user or port is not None)
    if isinstance(env_vars, dict):
        env_vars_text = ", ".join(f"{k}={v}" for k, v in env_vars.items())
        if any(v not in [None, ""] for v in env_vars.values()):
            has_inline_values = True
    elif isinstance(env_vars, (list, tuple)):
        env_vars_text = ", ".join(str(v) for v in env_vars if v)
    elif isinstance(env_vars, str):
        env_vars_text = env_vars

    source = conn.get("source")
    if source:
        source_lower = str(source).lower()
        if not has_inline_values:
            if source_lower in ["env", "environment", "external"]:
                lines.append("connection_source: env/config external (no inline values)")
            else:
                lines.append(f"connection_source: {source}")
        elif source_lower not in ["env", "environment", "external"]:
            lines.append(f"connection_source: {source}")

    if engine:
        details.append(f"engine={engine}")
    if method:
        details.append(f"method={method}")
    if host:
        details.append(f"host={host}")
    if port is not None:
        details.append(f"port={port}")
    if dbname:
        details.append(f"dbname={dbname}")
    if user:
        details.append(f"user={user}")
    if details:
        lines.append("connection_details: " + ", ".join(details))

    if env_vars_text:
        lines.append(f"env_vars: {env_vars_text}")

    conn_vars = conn.get("vars")
    if conn_vars is None:
        conn_vars = conn.get("variables")
    if conn_vars is None:
        conn_vars = conn.get("var_map")
    conn_vars_text = _format_conn_vars(conn_vars)
    if conn_vars_text:
        lines.append("connection_vars: " + ", ".join(conn_vars_text))

    conn_vars_map = _normalize_conn_vars(conn_vars)
    env_vars_list = _normalize_env_vars(env_vars)
    env_mapping = _infer_env_mapping(env_vars_list, conn_vars_map)
    layer = (prog_info or {}).get("layer", "").lower()
    allow_sql = (prog_info or {}).get("allowed_sql", False)
    if layer == "dal" or allow_sql:
        lines.append("host_vars_sql: utiliser uniquement :WS-* (WORKING-STORAGE), jamais :LK-*.")
        user_var = conn_vars_map.get("user") or conn_vars_map.get("username")
        pass_var = conn_vars_map.get("password") or conn_vars_map.get("pass")
        db_var = (
            conn_vars_map.get("dbname")
            or conn_vars_map.get("database")
            or conn_vars_map.get("db")
        )
        if user_var and pass_var and db_var:
            lines.append(
                "connect_form: EXEC SQL CONNECT :"
                f"{user_var} IDENTIFIED BY :{pass_var} USING :{db_var} END-EXEC."
            )
            lines.append("connect_form_forbid: CONNECT TO ... USER ... USING ... (non supporte)")
        if env_mapping:
            lines.append("env_mapping: " + ", ".join(env_mapping))
            lines.append("env_usage: utiliser ACCEPT <var> FROM ENVIRONMENT '<ENV>' selon le mapping.")
            lines.append("env_identifiers_forbidden: ne jamais utiliser PG* comme identifiants COBOL.")
        flag_names = _extract_flag_names((prog_info or {}).get("working_storage_lines", []) or [])
        flag_values = _get_flag_values(norm)
        if flag_names and flag_values:
            true_val, false_val = flag_values
            lines.append(f"flag_values: true='{true_val}', false='{false_val}' (spec)")
            lines.append("flags: " + ", ".join(flag_names) + " -> MOVE '<val>' vers *-FLAG.")
            lines.append("INTERDIT: SET <flag> TO TRUE/FALSE si aucune clause 88 false/true n'est definie.")

    env_set_method = conn.get("env_set_method", []) or []
    if env_set_method:
        lines.append("env_set_method: " + " | ".join(env_set_method))

    method_lower = (method or "").lower()
    ocesql = conn.get("ocesql", {}) or {}
    if ocesql and method_lower == "direct":
        start_call = ocesql.get("start_call")
        connect_call = ocesql.get("connect_call")
        end_call = ocesql.get("end_call")
        if start_call or connect_call or end_call:
            lines.append(f"OCESQL calls: {start_call}, {connect_call}, {end_call}")
        connect_args = ocesql.get("connect_args", []) or []
        if connect_call and connect_args:
            lines.append(f"connect_signature: {connect_call}(" + ", ".join(connect_args) + ")")
        elif connect_args:
            lines.append("connect_args: " + ", ".join(connect_args))
        order_note = ocesql.get("connect_args_order_note")
        if order_note:
            lines.append(order_note)

    lines.append("variables_connexion: utiliser uniquement les variables deja presentes")
    lines.append("dans WORKING-STORAGE (pas d'invention, pas de hardcode).")

    behavior = (norm.get("sql", {}) or {}).get("behavior", {}) if norm else {}
    if behavior.get("check_sqlcode"):
        success = behavior.get("sqlcode_success")
        if success is None:
            success = 0
        lines.append(f"error_policy: SQLCODE < 0 -> log + stop; SQLCODE = {success} -> OK")
    if behavior.get("cursor_commit_policy") == "no_commit_while_open":
        lines.append("transaction_policy: no COMMIT during FETCH; COMMIT only in DAL-END")

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
    prog_info = _extract_program_info(norm, program_id)
    if not prog_info.get("allowed_sql", False) or (prog_info.get("layer") or "").lower() != "dal":
        return ""

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
            lines.append(f" COMMIT AUTORISÉ dans: {', '.join(commit_allowed_in)}")

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
            "-  AUTORISÉ: EXEC SQL ROLLBACK END-EXEC (annule sans fermer curseur)",
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
        LOG.warning("  LLM procedure generation DISABLED - files will be empty!")
        LOG.warning("  Set CSG_USE_LLM_PROCS=1 to generate code from specs")
        return touched

    LOG.info(" LLM procedure generation from specs ONLY (no static templates)...")

    try:
        from . import _llm_codegen
        llm_files = _llm_codegen.rewrite_procedures_with_llm(out_dir, config)
        LOG.info(f" LLM generated {len(llm_files)} files from specs")
        return llm_files
    except Exception as e:
        LOG.error(f"❌ LLM procedure generation failed: {e}")
        LOG.warning("  Files remain empty (no fallback to static code)")
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
    _augment_missing_dal_functions(norm)
    io_map = fs.read_json(str(out / "io_map.json"))
    contract = {}
    contract_path = out / "architecture_contract.json"
    if contract_path.exists():
        contract = fs.read_json(str(contract_path))

    written = []
    prog_names = {p.get("name") for p in norm.get("programmes", []) if p.get("name")}
    for program in plan.get("programs", []):
        # Align program_id with YAML "programmes" when possible (source de vérité)
        program_id = program.get("id") or program.get("name", "PROGRAM")
        layer = program.get("layer", "logic")
        entity = program.get("entity", "EMPLOYEE")

        # If the planner used legacy ids, override with YAML programme name on same layer/entity
        if program_id not in prog_names:
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

def _replace_sql_host_vars(code: str, ws_names: List[str]) -> str:
    if not ws_names:
        return code
    ws_set = {n.upper() for n in ws_names}
    lines = code.splitlines()
    in_sql = False
    for idx, line in enumerate(lines):
        upper = line.upper()
        if "EXEC SQL" in upper:
            in_sql = True
        if in_sql and ":LK-" in upper:
            def _repl(m: re.Match) -> str:
                lk = m.group(1).upper()
                ws = f"WS-{lk}"
                if ws in ws_set:
                    return f":{ws}"
                return m.group(0)
            lines[idx] = re.sub(r":LK-([A-Z0-9-]+)", _repl, line, flags=re.IGNORECASE)
        if "END-EXEC" in upper:
            in_sql = False
    return "\n".join(lines)

def _replace_exit_perform(code: str) -> str:
    return re.sub(r"\bEXIT\s+PERFORM\b", "EXIT PARAGRAPH", code, flags=re.IGNORECASE)

# 
# def _fix_block_statement_dots(code: str) -> str:
#     """
#     Remove trailing dots from block-opening statements.
# 
#     In COBOL, block statements like PERFORM UNTIL, IF, EVALUATE must NOT
#     have a period after them when using END-xxx scope terminators.
# 
#     FIX 2026-01-15: The LLM sometimes generates:
#         PERFORM UNTIL END-OF-FILE = 'Y'.
#     which terminates the PERFORM immediately (empty inline PERFORM).
# 
#     Correct form is:
#         PERFORM UNTIL END-OF-FILE = 'Y'
#             ... body ...
#         END-PERFORM.
# 
#     This function removes trailing dots from:
#     - PERFORM UNTIL ...
#     - PERFORM VARYING ...
#     - IF ...
#     - EVALUATE ...
#     - EXEC SQL (when not followed by END-EXEC on same line)
#     """
#     lines = code.splitlines()
#     out = []
# 
#     # Patterns for block-opening statements that shouldn't end with a period
#     # when they have a corresponding END-xxx terminator
#     block_patterns = [
#         (r"^(\s*)(PERFORM\s+UNTIL\s+.+)\.$", r"\1\2"),     # PERFORM UNTIL cond.
#         (r"^(\s*)(PERFORM\s+VARYING\s+.+)\.$", r"\1\2"),   # PERFORM VARYING ...
#         (r"^(\s*)(IF\s+.+)\.$", r"\1\2"),                  # IF condition.
#         (r"^(\s*)(EVALUATE\s+.+)\.$", r"\1\2"),            # EVALUATE expr.
#     ]
# 
#     for line in lines:
#         modified = line
#         for pattern, replacement in block_patterns:
#             # Only remove dot if the line matches a block-opening pattern
#             new_line = re.sub(pattern, replacement, modified, flags=re.IGNORECASE)
#             if new_line != modified:
#                 modified = new_line
#                 break
#         out.append(modified)
# 
#     return "\n".join(out)
# 

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
                LOG.warning(f"  DECLARE et OPEN {cursor_name} dans le même bloc EXEC SQL, séparation requise")

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
                    LOG.warning(f" DECLARE et OPEN {cursor_name} séparés en deux blocs EXEC SQL")
                    # Pas de break car on continue pour d'autres curseurs potentiels
                    continue

            # Si on arrive ici, DECLARE existe déjà et est bien séparé
            continue

        # CAS 2: DECLARE manquant complètement

        # DECLARE manquant! Chercher le OPEN et injecter le DECLARE avant
        LOG.warning(f"  DECLARE {cursor_name} CURSOR manquant dans {program_id}, injection automatique")

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
                LOG.warning(f" DECLARE {cursor_name} CURSOR injecté dans un bloc EXEC SQL séparé")
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

        # FIX 2026-01-15: Remove dots from block-opening statements
        # This fixes "PERFORM UNTIL cond." being interpreted as empty PERFORM
        # clean = _fix_block_statement_dots(clean)

        # Post-injection: Garantir que DECLARE CURSOR est présent si nécessaire
        clean = _inject_missing_cursor_declarations(clean, program_id, norm)
        prog_info = _extract_program_info(norm, program_id)
        ws_names = _extract_ws_names((prog_info.get("working_storage_lines") or []))
        clean = _replace_sql_host_vars(clean, ws_names)
        if (prog_info.get("layer") or "").lower() == "dal":
            clean = _replace_exit_perform(clean)
            clean = _ensure_sqlca_top_of_ws(clean)

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
    ws_lines_list = prog_info.get("working_storage_lines", []) or []
    if (prog_info.get("layer") or "").lower() == "dal":
        ws_lines_list = _order_ws_lines_for_prompt(ws_lines_list, True)
    working_storage_lines = "\n".join(ws_lines_list)
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

    connection_details = _build_connection_details(norm, prog_info)
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
    sections_order = program_prompt.get("sections_order", []) or []
    if not sections_order:
        if layer == "dal":
            sections_order = [
                "strategy",
                "context",
                "requirements",
                "style",
                "format",
                "naming",
                "environment",
                "interface",
                "structure",
                "working_storage",
                "sql",
                "sql_format",
                "sql_behavior",
                "flow",
                "logging",
                "constraints",
                "skeleton",
                "fonctions",
            ]
        elif layer == "logic":
            sections_order = [
                "strategy",
                "context",
                "requirements",
                "style",
                "format",
                "naming",
                "environment",
                "interface",
                "working_storage",
                "flow",
                "logging",
                "constraints",
                "skeleton",
                "fonctions",
            ]
        else:  # business
            sections_order = [
                "strategy",
                "context",
                "requirements",
                "style",
                "format",
                "naming",
                "environment",
                "interface",
                "structure",
                "working_storage",
                "logging",
                "constraints",
                "skeleton",
                "fonctions",
            ]
    if "requirements" not in sections_order:
        if "context" in sections_order:
            idx = sections_order.index("context") + 1
            sections_order.insert(idx, "requirements")
        else:
            sections_order.append("requirements")

    # FIX 2026-01-12: Si le programme a une linkage_structure dans la spec,
    # s'assurer que "linkage" ou "structure" est dans sections_order (pour afficher LINKAGE)
    if prog_info.get("linkage_structure") and "linkage" not in sections_order and "structure" not in sections_order:
        # Ajouter "linkage" après "interface" si elle existe, sinon après "context"
        if "interface" in sections_order:
            idx = sections_order.index("interface") + 1
            sections_order.insert(idx, "linkage")
        elif "context" in sections_order:
            idx = sections_order.index("context") + 1
            sections_order.insert(idx, "linkage")
        else:
            sections_order.insert(0, "linkage")

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
        "linkage": [
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
        ],
        "skeleton": [
            "Squelette attendu (rappel structure):",
            "IDENTIFICATION DIVISION.",
            "PROGRAM-ID. {program_id}.",
            "{header_comment_lines}",
            "{environment_lines}",
            "DATA DIVISION.",
            "WORKING-STORAGE SECTION.",
            "{working_storage_lines}",
            "{procedure_division_line}"
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
    if "EXIT PERFORM" in upper_code:
        errors.append("EXIT PERFORM interdit (utiliser EXIT PARAGRAPH).")
    ws_lines = prog_info.get("working_storage_lines", []) or []
    has_88_in_spec = any(re.match(r"^\s*88\b", str(l)) for l in ws_lines)
    if not has_88_in_spec and (" TO TRUE" in upper_code or " TO FALSE" in upper_code) and "SET " in upper_code:
        errors.append("SET ... TO TRUE/FALSE interdit sans 88-level dans la spec (utiliser MOVE vers *-FLAG).")
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
            # SQLCA doit etre la premiere ligne du WORKING-STORAGE en DAL
            if (prog_info.get("layer") or "").lower() == "dal":
                ws_started = False
                sqlca_idx = None
                first_data_idx = None
                for idx, line in enumerate(lines):
                    up = line.upper()
                    if "WORKING-STORAGE SECTION" in up:
                        ws_started = True
                        continue
                    if ws_started:
                        if "LINKAGE SECTION" in up or "PROCEDURE DIVISION" in up:
                            break
                        if "EXEC SQL INCLUDE SQLCA END-EXEC" in up:
                            sqlca_idx = idx
                            continue
                        if first_data_idx is None and re.match(r"^\s*(01|05|77|88)\b", line):
                            first_data_idx = idx
                if sqlca_idx is not None and first_data_idx is not None and sqlca_idx > first_data_idx:
                    errors.append(
                        "EXEC SQL INCLUDE SQLCA doit etre place en premier dans WORKING-STORAGE "
                        "(avant toute declaration 01/05/77/88)."
                    )
            if ":LK-" in upper_code:
                errors.append("EXEC SQL doit utiliser uniquement des host vars WS-* (LK-* interdites).")
            if "CONNECT TO" in upper_code:
                errors.append("CONNECT doit utiliser: EXEC SQL CONNECT :USER IDENTIFIED BY :PASS USING :DB END-EXEC.")
            env_vars = _normalize_env_vars((norm.get("sql", {}) or {}).get("connection", {}).get("env_vars"))
            if env_vars:
                stripped = re.sub(r"'[^']*'|\"[^\"]*\"", "", upper_code)
                for env_name in env_vars:
                    token = str(env_name).upper()
                    if token and re.search(rf"\b{re.escape(token)}\b", stripped):
                        errors.append(f"Variable d'environnement {env_name} utilisee comme identifiant COBOL (interdit).")
                        break
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

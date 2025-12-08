# """
# Create SQL DDL/DML files from normalized_spec using prompts/sql_template.txt + optional RAG/LLM.
# 
# Public API:
#     run(normalized_spec_path: str, config: dict, out_dir: str) -> list[str]
# 
# Rules:
# - Emit files under out/sql/ddl/*.sql and out/sql/dml/*.sql
# - Respect target SGBD from config.defaults.sql_cible
# - Deterministic file order; include header comment with generation date and target dialect
# - Log prompt/response with utils.trace
# """
# 
# from __future__ import annotations
# 
# import json
# from datetime import datetime, timezone
# from pathlib import Path
# from typing import Any, Dict, List, Optional
# 
# from cobolsmartgen.utils.fs import ensure_dir, read_json, write_text
# from cobolsmartgen.utils.trace import log_prompt, write_sidecar_hash, write_meta
# from cobolsmartgen.adapters.llm_auto import chat as llm_chat
# 
# 
# def _utc_now_iso() -> str:
#     return datetime.now(timezone.utc).replace(microsecond=0).isoformat().replace("+00:00", "Z")
# 
# 
# def _read_text_if_exists(path: Path) -> Optional[str]:
#     return path.read_text(encoding="utf-8") if path.exists() else None
# 
# 
# def run(normalized_spec_path: str, config: Dict[str, Any], out_dir: str) -> List[str]:
#     spec = read_json(normalized_spec_path)
#     dialect = (config.get("defaults", {}) or {}).get("sql_cible") or spec.get("sql_cible") or "postgres"
#     entities = spec.get("entities") or spec.get("tables") or {}
#     if isinstance(entities, list):
#         # turn list of dicts with 'name' into dict
#         entities = {str(e.get("name")): e for e in entities if isinstance(e, dict) and e.get("name")}
#     if not isinstance(entities, dict):
#         entities = {}
# 
#     tpl_path = Path("prompts") / "sql_template.txt"
#     template = _read_text_if_exists(tpl_path) or (
#         "-- DIALECT: {dialect}\n"
#         "-- DATE: {date}\n"
#         "-- ENTITY: {entity}\n"
#         "-- SPEC:\n{entity_json}\n\n"
#         "-- Emit DDL here.\n"
#     )
# 
#     ddl_dir = ensure_dir(Path(out_dir) / "sql" / "ddl")
#     dml_dir = ensure_dir(Path(out_dir) / "sql" / "dml")
# 
#     written: List[str] = []
#     # Deterministic order
#     for name in sorted(entities.keys(), key=lambda s: s.upper()):
#         entity = entities[name]
#         ctx = {
#             "dialect": dialect,
#             "date": _utc_now_iso(),
#             "entity": name,
#             "entity_json": json.dumps(entity, ensure_ascii=False, indent=2),
#             "full_spec_json": json.dumps(spec, ensure_ascii=False, indent=2)[:20000],  # guard huge
#         }
#         user_prompt = f"[template]\n{template}\n\n[context]\n{json.dumps(ctx, ensure_ascii=False, indent=2)}"
#         messages = [
#             {"role": "system", "content": "Generate only SQL for the requested dialect. Keep output idempotent and include IF NOT EXISTS where applicable."},
#             {"role": "user", "content": user_prompt},
#         ]
#         ddl_sql = llm_chat(messages=messages, timeout_s=int(config.get("llm", {}).get("timeout_s", 120)), config=config).strip()
# 
#         # Header
#         header = f"-- Generated: {ctx['date']} UTC | Dialect: {dialect} | Entity: {name}\n"
#         ddl_text = header + ddl_sql + ("\n" if not ddl_sql.endswith("\n") else "")
# 
#         ddl_path = ddl_dir / f"{name.upper()}__ddl.sql"
#         write_text(str(ddl_path), ddl_text, encoding="utf-8", atomic=True)
#         write_sidecar_hash(ddl_path)
#         write_meta(ddl_path, kind="sql-ddl", extra={"dialect": dialect, "entity": name})
#         log_prompt(out_dir=out_dir, kind="generation", name=f"sql:ddl:{name}", prompt=user_prompt, response=ddl_sql, meta={"dialect": dialect})
# 
#         # Minimal seed DML (optional; basic insert skeleton)
#         dml_header = f"-- Generated: {ctx['date']} UTC | Dialect: {dialect} | Entity: {name}\n"
#         dml_body = f"-- TODO: seed data for {name}\n"
#         dml_path = dml_dir / f"{name.upper()}__dml.sql"
#         write_text(str(dml_path), dml_header + dml_body, encoding="utf-8", atomic=True)
#         write_sidecar_hash(dml_path)
#         write_meta(dml_path, kind="sql-dml", extra={"dialect": dialect, "entity": name})
# 
#         written.extend([str(ddl_path), str(dml_path)])
# 
#     return written
"""
SQL file generation module.
Creates DDL and DML scripts for database setup.
"""

import logging
from datetime import datetime
from pathlib import Path
from typing import Dict, List
import os

from ..utils import fs, naming
from cobolsmartgen.adapters import llm_auto

LOG = logging.getLogger(__name__)


def _run_core(normalized_spec_path: str, io_map_path: str, config: Dict, out_dir: str) -> List[str]:
    """Generate SQL DDL and DML files."""
    
    # Load inputs
    spec = fs.read_json(normalized_spec_path)
    io_map = fs.read_json(io_map_path)
    
    # Mode strict 8.5 : un seul fichier SQL via LLM
    if os.getenv("CSG_STRICT_825", "0") == "1":
        return generate_strict_sql(spec, io_map, out_dir, config)
    
    # Get SQL target
    sql_target = spec.get('sql_cible', 'postgres').lower()
    
    # Generate scripts
    generated_files = []
    
    # Generate DDL
    ddl_files = generate_ddl(spec, io_map, sql_target, out_dir)
    generated_files.extend(ddl_files)
    
    # Generate DML
    dml_files = generate_dml(spec, io_map, sql_target, out_dir)
    generated_files.extend(dml_files)
    
    LOG.info(f"Generated {len(generated_files)} SQL files")
    return generated_files


def generate_ddl(spec: Dict, io_map: Dict, sql_target: str, out_dir: str) -> List[str]:
    """Generate DDL scripts."""
    out = Path(out_dir)
    ddl_dir = out / "sql" / "ddl"
    fs.ensure_dir(ddl_dir)
    
    generated = []
    
    for entity in spec.get('mcd', {}).get('entites', []):
        entity_name = entity['name'].upper()
        
        # Build CREATE TABLE statement
        lines = [f"-- Table creation for {entity_name}"]
        lines.append(f"CREATE TABLE IF NOT EXISTS {entity_name} (")
        
        # Add columns
        columns = []
        for attr in entity.get('attrs', []):
            col_name = attr.get('normalized_name', attr['name']).upper()
            sql_type = attr.get('normalized_type', {}).get('sql_type', attr.get('type', 'VARCHAR(255)'))
            
            col_def = f"    {col_name} {sql_type}"
            
            if attr.get('pk'):
                col_def += " PRIMARY KEY"
            elif not attr.get('nullable', True):
                col_def += " NOT NULL"
            
            if attr.get('default') is not None:
                default = attr['default']
                if isinstance(default, str):
                    col_def += f" DEFAULT '{default}'"
                else:
                    col_def += f" DEFAULT {default}"
            
            columns.append(col_def)
        
        lines.append(",\n".join(columns))
        lines.append(");")
        
        # Add indexes for foreign keys
        for attr in entity.get('attrs', []):
            if attr.get('fk'):
                idx_name = f"idx_{entity_name}_{attr['name']}".lower()
                lines.append(f"\nCREATE INDEX IF NOT EXISTS {idx_name} ON {entity_name}({attr['name'].upper()});")
        
        # Write DDL file
        ddl_file = ddl_dir / f"{entity_name.lower()}.sql"
        content = "\n".join(lines)
        fs.write_text(str(ddl_file), content, atomic=True)
        generated.append(str(ddl_file))
    
    return generated


def generate_dml(spec: Dict, io_map: Dict, sql_target: str, out_dir: str) -> List[str]:
    """Generate DML scripts with sample data."""
    out = Path(out_dir)
    dml_dir = out / "sql" / "dml"
    fs.ensure_dir(dml_dir)
    
    generated = []
    
    for entity in spec.get('mcd', {}).get('entites', []):
        entity_name = entity['name'].upper()
        
        # Build INSERT statements
        lines = [f"-- Sample data for {entity_name}"]
        
        # Generate sample records
        if entity_name == "EMPLOYEE":
            lines.append("INSERT INTO EMPLOYEE (EMP_ID, EMP_NAME, SALARY_BRUT, SALARY_NET)")
            lines.append("VALUES (1, 'Dupont', 3000.00, 0.00);")
            lines.append("")
            lines.append("INSERT INTO EMPLOYEE (EMP_ID, EMP_NAME, SALARY_BRUT, SALARY_NET)")
            lines.append("VALUES (2, 'Martin', 3500.00, 0.00);")
            lines.append("")
            lines.append("INSERT INTO EMPLOYEE (EMP_ID, EMP_NAME, SALARY_BRUT, SALARY_NET)")
            lines.append("VALUES (3, 'Bernard', 4000.00, 0.00);")
        
        # Write DML file
        if lines and len(lines) > 1:
            dml_file = dml_dir / f"{entity_name.lower()}_data.sql"
            content = "\n".join(lines)
            fs.write_text(str(dml_file), content, atomic=True)
            generated.append(str(dml_file))
    
    return generated


def run(*args, **kwargs):
    """
    Compat: accepte normalized_spec_path/spec_path + io_map_path + config + out_dir,
    en positionnel ou nommé.
    """
    # 1) chemins
    normalized_spec_path = (
        kwargs.pop("normalized_spec_path", None)
        or kwargs.pop("spec_path", None)
        or (args[0] if len(args) >= 1 and isinstance(args[0], str) else None)
    )
    io_map_path = (
        kwargs.pop("io_map_path", None)
        or (args[1] if len(args) >= 2 and isinstance(args[1], str) else None)
    )

    # 2) config & out_dir
    config = (
        kwargs.pop("config", None)
        or (args[2] if len(args) >= 3 and isinstance(args[2], dict) else {})
        or {}
    )
    out_dir = (
        kwargs.pop("out_dir", None)
        or (args[3] if len(args) >= 4 and isinstance(args[3], str) else "out")
    )

    if not normalized_spec_path or not io_map_path:
        raise TypeError("run() requires 'normalized_spec_path' and 'io_map_path'.")

    return _run_core(normalized_spec_path, io_map_path, config, out_dir)


def _extract_entity(spec: Dict) -> Dict:
    ent = {}
    for e in spec.get("mcd", {}).get("entites", []):
        if isinstance(e, dict):
            ent = e
            break
    return ent


def _render_sql_from_spec(spec: Dict) -> str:
    """Construit le SQL directement depuis la spec (ddl + initial_data) si présent."""
    ent = _extract_entity(spec)
    name = ent.get("name", "EMPLOYEE")

    sql_spec = spec.get("sql", {}) or {}
    ddl_spec = sql_spec.get("ddl", {}) or {}
    init_spec = sql_spec.get("initial_data", {}) or {}
    idx_spec = sql_spec.get("indexes", []) or []

    ddl = ddl_spec.get(name)
    inserts = init_spec.get(name, [])

    if not ddl:
        return ""

    lines = [ddl.rstrip()]

    for idx in idx_spec:
        if idx.get("entity") == name:
            cols = ", ".join(idx.get("columns", []))
            lines.append(f"CREATE INDEX {idx.get('name')} ON {name} ({cols});")

    attrs = ent.get("attrs", [])
    col_order = [a.get("name") for a in attrs if a.get("name")]
    for row in inserts:
        cols = col_order or list(row.keys())
        values_sql = []
        for c in cols:
            v = row.get(c)
            if isinstance(v, str):
                values_sql.append(f"'{v}'")
            elif v is None:
                values_sql.append("NULL")
            else:
                values_sql.append(str(v))
        lines.append(f"INSERT INTO {name} ({', '.join(cols)}) VALUES ({', '.join(values_sql)});")

    return "\n".join(lines).strip() + "\n"


def _strip_markdown_sql(text: str) -> str:
    """Retire d'éventuelles fences ```sql ...```."""
    if "```" not in text:
        return text.strip()
    import re
    m = re.search(r"```[a-zA-Z]*\n(.*?)```", text, re.S)
    if m:
        return m.group(1).strip()
    return text.replace("```", "").strip()


def generate_strict_sql(spec: Dict, io_map: Dict, out_dir: str, config: Dict) -> List[str]:
    """Génère un unique fichier SQL (8.5) en utilisant la spec si possible, sinon LLM."""
    entity = _extract_entity(spec)
    name = entity.get("name", "EMPLOYEE")

    # 1) Essayer de produire directement depuis le YAML (source de vérité)
    sql_text = _render_sql_from_spec(spec)

    # 2) Fallback LLM si pas de ddl dans la spec
    prompt = ""
    if not sql_text:
        attrs = entity.get("attrs", [])
        cols_lines = []
        for a in attrs:
            t = a.get("type", "VARCHAR(30)")
            n = a.get("name", "COL")
            extra = " PRIMARY KEY" if a.get("pk") else ""
            cols_lines.append(f"- {n} {t}{extra}")

        idx = None
        for i in spec.get("sql", {}).get("indexes", []):
            if i.get("entity") == name:
                idx = i
                break

        idx_line = ""
        if idx:
            cols = ", ".join(idx.get("columns", []))
            idx_line = f"CREATE INDEX {idx.get('name')} ON {name} ({cols});"

        prompt = f"""Contexte:
SGBD: PostgreSQL
Table à créer: {name}
Colonnes:
{chr(10).join(cols_lines)}
Index: {idx_line or 'aucun'}

Contraintes de réponse:
- Réponds uniquement avec du SQL (pas de Markdown, pas de texte avant/après, pas de ```).
- Première ligne doit commencer par CREATE TABLE.
- Types identiques à la spec, PK sur la colonne marquée pk.

Tâche:
1) CREATE TABLE {name} (...) avec les colonnes ci-dessus.
2) {idx_line if idx_line else 'Aucun index si non défini.'}
3) Un INSERT d'exemple:
   INSERT INTO {name} (EMP_ID, EMP_NAME, SALARY_BRUT, SALARY_NET) VALUES (1, 'Dupont', 3000.00, 0.00);"""

        system = "Expert SQL PostgreSQL. Génère uniquement le SQL demandé."
        sql_text = llm_auto.generate(prompt=prompt, system=system, config=config)
        sql_text = _strip_markdown_sql(sql_text)

    sql_dir = fs.ensure_dir(Path(out_dir) / "sql")
    target = sql_dir / f"{name}.sql"
    fs.write_text(str(target), sql_text, atomic=True)

    # Trace minimal
    trace_dir = Path(out_dir) / "trace" / "generations" / "strict_825_sql"
    trace_dir.mkdir(parents=True, exist_ok=True)
    if prompt:
        fs.write_text(str(trace_dir / "01_prompt.txt"), prompt, atomic=True)
    fs.write_text(str(trace_dir / "02_raw_response.txt"), sql_text, atomic=True)

    LOG.info(f"Generated strict SQL file: {target}")
    return [str(target)]

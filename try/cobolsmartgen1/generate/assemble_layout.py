
"""
Assemble final project layout.
Generate copybooks and organize output structure.
"""

from __future__ import annotations

import json
from pathlib import Path
from typing import Any, Dict, List

from cobolsmartgen1.utils import fs, naming
from cobolsmartgen1.utils.trace import write_sidecar_hash, write_meta, log_prompt


def _ensure_dirs(base: Path) -> Dict[str, Path]:
    subdirs = {
        "business": base / "business",
        "logic": base / "logic",
        "dal": base / "dal",
        "copy": base / "copy",
        "sql-ddl": base / "sql" / "ddl",
        "sql-dml": base / "sql" / "dml",
        "tests": base / "tests",
        "reports": base / "reports",
        "trace": base / "trace",
        "bin": base / "bin",
    }
    for p in subdirs.values():
        fs.ensure_dir(p)
    return subdirs


def _write_sqlca(copy_dir: Path) -> Path:
    """Generate SQLCA copybook (fixed/free safe) and a sqlca.cbl alias for OCESQL."""
    path = copy_dir / "SQLCA.cpy"
    alias = copy_dir / "sqlca.cbl"
    body = (
        "       01  SQLCA.\n"
        "           05  SQLCAID     PIC X(8).\n"
        "           05  SQLCABC     PIC S9(9) COMP-5.\n"
        "           05  SQLCODE     PIC S9(9) COMP-5.\n"
        "           05  SQLERRM.\n"
        "               49  SQLERRML PIC S9(4) COMP-5.\n"
        "               49  SQLERRMC PIC X(70).\n"
        "           05  SQLERRP     PIC X(8).\n"
        "           05  SQLERRD     OCCURS 6 TIMES PIC S9(9) COMP-5.\n"
        "           05  SQLWARN.\n"
        "               10  SQLWARN0 PIC X.\n"
        "               10  SQLWARN1 PIC X.\n"
        "               10  SQLWARN2 PIC X.\n"
        "               10  SQLWARN3 PIC X.\n"
        "               10  SQLWARN4 PIC X.\n"
        "               10  SQLWARN5 PIC X.\n"
        "               10  SQLWARN6 PIC X.\n"
        "               10  SQLWARN7 PIC X.\n"
        "           05  SQLSTATE    PIC X(5).\n"
    )

    def _needs_upgrade(p: Path) -> bool:
        if not p.exists():
            return True
        try:
            text = p.read_text(encoding="utf-8", errors="ignore")
        except Exception:
            return True
        # Minimal stubs often miss SQLERRMC/SQLERRD/SQLWARN
        return ("SQLERRMC" not in text) or ("SQLERRD" not in text) or ("SQLWARN0" not in text)

    if _needs_upgrade(path):
        fs.write_text(str(path), body, encoding="utf-8", atomic=True)
        write_sidecar_hash(path)
        write_meta(path, kind="copybook", extra={"name": "SQLCA"})

    if _needs_upgrade(alias):
        fs.write_text(str(alias), body, encoding="utf-8", atomic=True)
        write_sidecar_hash(alias)
        write_meta(alias, kind="copybook", extra={"name": "sqlca"})

    return path


def _write_status_codes(copy_dir: Path) -> Path:
    """Generate STATUS-CODES copybook."""
    path = copy_dir / "STATUS-CODES.cpy"
    if not path.exists():
        body = """      *> Common status fields
       01  STATUS-CODES.
           05 WS-RETURN-CODE     PIC S9(4) COMP VALUE 0.
           05 WS-ERROR-MSG       PIC X(80) VALUE SPACES.
           05 WS-STATUS          PIC X VALUE 'N'."""
        fs.write_text(str(path), body, encoding="utf-8", atomic=True)
        write_sidecar_hash(path)
        write_meta(path, kind="copybook", extra={"name": "STATUS-CODES"})
    return path


def _entity_record_copybook(copy_dir: Path, entity: str, fields: List[Dict[str, Any]]) -> Path:
    """Generate entity record copybook matching the example format."""
    name = f"{entity.upper()}-RECORD.cpy"
    path = copy_dir / name
    
    if not path.exists():
        lines = []
        
        # Add header comment
        lines.append(f"      *> Record structure for {entity.upper()}")
        
        # Record level
        lines.append(f"       01 {entity.upper()}.")
        
        # Process fields
        for field in fields:
            if isinstance(field, dict):
                cobol_name = field.get('cobol_name', naming.to_cobol_name(field.get('name', 'FIELD')))
                cobol_type = field.get('cobol_type', 'PIC X(30)')
                
                # Format the field line properly
                # Remove PIC prefix if already present in type
                if cobol_type.startswith('PIC '):
                    pic_clause = cobol_type
                else:
                    pic_clause = f"PIC {cobol_type}"
                
                # Format: level name PIC-clause
                lines.append(f"           05 {cobol_name:<20} {pic_clause}.")
        
        content = '\n'.join(lines)
        fs.write_text(str(path), content, encoding="utf-8", atomic=True)
        write_sidecar_hash(path)
        write_meta(path, kind="copybook", extra={"entity": entity})
    
    return path


def _write_tests(base: Path, files: List[str]) -> List[str]:
    """Generate test scripts if template exists."""
    tmpl_path = Path("prompts") / "compiler_test_template.txt"
    if not tmpl_path.exists():
        tmpl_path = Path("cobolsmartgen") / "prompts" / "compiler_test_template.txt"
    
    if not tmpl_path.exists():
        return []
    
    template = tmpl_path.read_text(encoding="utf-8")
    written: List[str] = []
    
    for f in files:
        p = Path(f)
        name = p.stem.upper()
        body = template.replace("{file}", str(p)).replace("{program_id}", name)
        out = base / "tests" / f"compile_{name}.sh"
        fs.write_text(str(out), body, encoding="utf-8", atomic=True)
        written.append(str(out))
    
    return written


def run(program_plan_path: str, generated_files: List[str], config: Dict[str, Any], out_dir: str) -> str:
    """
    Assemble the final layout with all copybooks and structure.
    """
    base = Path(out_dir)
    dirs = _ensure_dirs(base)

    # Generate mandatory copybooks
    _write_sqlca(dirs["copy"])
    _write_status_codes(dirs["copy"])

    # Find and load io_map
    io_map_path = config.get("io_map_path", "")
    if not io_map_path:
        # Try to find io_map in output directory
        possible_path = base / "io_map.json"
        if possible_path.exists():
            io_map_path = str(possible_path)
    
    # Generate entity copybooks from io_map
    if io_map_path and Path(io_map_path).exists():
        io_map = fs.read_json(io_map_path)
        entities = io_map.get("entities", [])
        
        for entity_data in entities:
            entity_name = entity_data.get('name') or entity_data.get('entity_name', 'UNKNOWN')
            
            # Combine inputs and outputs, removing duplicates
            all_fields = []
            seen_names = set()
            
            for field in entity_data.get('inputs', []) + entity_data.get('outputs', []):
                cobol_name = field.get('cobol_name', naming.to_cobol_name(field['name']))
                if cobol_name not in seen_names:
                    all_fields.append(field)
                    seen_names.add(cobol_name)
            
            if all_fields:
                _entity_record_copybook(dirs["copy"], entity_name, all_fields)

    # Optional: generate compile test scripts
    test_files = _write_tests(base, generated_files)

    # Log assembly actions
    log_prompt(
        out_dir=out_dir,
        kind="assembly",
        name="assemble_layout",
        prompt=json.dumps({"out_dir": out_dir, "generated_files": generated_files}, ensure_ascii=False, indent=2),
        response=json.dumps({"created_dirs": list(map(str, dirs.values())), "tests": test_files}, ensure_ascii=False, indent=2),
        meta={"action": "assemble"},
    )

    return str(base)


def prepare_structures(io_map_path: str, out_dir: str) -> List[str]:
    """
    Prépare les structures de référence (copybooks) AVANT les appels LLM.

    - Crée l'arborescence minimale (dal/logic/business/copy/sql/...).
    - Génère SQLCA.cpy, STATUS-CODES.cpy.
    - Génère les copybooks entité à partir de io_map (inputs + outputs fusionnés).
    """
    base = Path(out_dir)
    dirs = _ensure_dirs(base)

    created: List[str] = []
    created.append(str(_write_sqlca(dirs["copy"])))
    created.append(str(_write_status_codes(dirs["copy"])))

    if io_map_path and Path(io_map_path).exists():
        io_map = fs.read_json(io_map_path)
        entities = io_map.get("entities", [])

        for entity_data in entities:
            entity_name = entity_data.get("name") or entity_data.get("entity_name", "UNKNOWN")

            all_fields = []
            seen_names = set()
            for field in entity_data.get("inputs", []) + entity_data.get("outputs", []):
                cobol_name = field.get('cobol_name', naming.to_cobol_name(field.get('name', 'FIELD')))
                if cobol_name not in seen_names:
                    all_fields.append(field)
                    seen_names.add(cobol_name)

            if all_fields:
                cpy_path = _entity_record_copybook(dirs["copy"], entity_name, all_fields)
                created.append(str(cpy_path))

    return created

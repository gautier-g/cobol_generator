# file: cobolsmartgen/generate/steps_generator.py
"""
Generate missing procedure steps from spec using LLM.
This fills 'fonctions[].steps' when absent, before COBOL generation.

IMPROVEMENTS (2026-01-07):
==========================

1. VALIDATION ERROR FEEDBACK (CRITIQUE)
   - Retry now sends detailed validation errors back to LLM
   - LLM receives specific error messages (unknown identifiers, line length, etc.)
   - Increases success rate by providing concrete feedback

2. EXTENDED COBOL KEYWORDS (MAJEUR)
   - Expanded from ~40 to 70+ keywords
   - Added: FUNCTION, OF, IN, ROUNDED, SQL keywords (WHERE, JOIN, etc.)
   - Added: intrinsic functions (ROUND, TRIM, LENGTH, etc.)
   - Reduces false positives in identifier validation

3. STRUCTURE NOTATION SUPPORT (MAJEUR)
   - New _validate_identifier_with_structure() function
   - Handles "FIELD OF STRUCTURE" and "FIELD IN STRUCTURE" notation
   - Validates both field and structure separately
   - Rejects invalid dot notation (STRUCTURE.FIELD)

4. FUNCTION CALL VALIDATION (IMPORTANT)
   - New _validate_function_call() function
   - Validates FUNCTION keyword usage specifically
   - Whitelist of 25+ intrinsic functions (ROUND, INTEGER, etc.)
   - Prevents rejection of valid FUNCTION calls

5. ENHANCED PROMPT CONSTRAINTS (IMPORTANT)
   - Added detailed examples (valid vs invalid)
   - Explicit structure notation rules
   - Stronger emphasis on using only declared identifiers
   - Minimalisme principle guidance

6. IMPROVED LOGGING (MINEUR)
   - Success/failure logging for each step generation
   - Retry outcome tracking
   - Error count reporting (shows first 3, mentions total)
   - Metadata includes validation status
"""
from __future__ import annotations

import json
import logging
import os
import re
from pathlib import Path
from typing import Dict, List, Optional

from cobolsmartgen1.adapters import llm_auto
from cobolsmartgen1.utils import fs, trace, pipeline_tracer, naming
from . import cobol_constraints

LOG = logging.getLogger(__name__)


def _entity_from_function(fn: Dict) -> str:
    return fn.get("entity") or fn.get("entite") or ""


def _find_program_info(spec: Dict, program_name: str) -> Dict:
    for prog in spec.get("programmes", []) or []:
        if prog.get("name") == program_name:
            return prog
    return {}


def _find_entity(spec: Dict, entity_name: str) -> Dict:
    for ent in spec.get("mcd", {}).get("entites", []) or []:
        if ent.get("name") == entity_name or ent.get("normalized_name") == entity_name:
            return ent
    return {}


def _find_entity_io(io_map: Optional[Dict], entity_name: str) -> Dict:
    if not io_map:
        return {}
    for ent in io_map.get("entities", []) or []:
        if ent.get("name") == entity_name or ent.get("entity_name") == entity_name:
            return ent
    return {}


def _format_entity_fields(ent: Dict, io_ent: Dict) -> str:
    parts: List[str] = []
    if io_ent and io_ent.get("inputs"):
        for f in io_ent.get("inputs", []) or []:
            name = f.get("cobol_name") or naming.to_cobol_name(f.get("name", ""))
            ctype = f.get("cobol_type") or ""
            sql_type = f.get("sql_type") or ""
            nullable = f.get("nullable")
            null_text = "nullable" if nullable else "not null"
            if name:
                parts.append(f"{name} [{ctype}] ({sql_type}, {null_text})")
        if parts:
            return "\n".join(parts)
    attrs = ent.get("attrs", []) if ent else []
    for a in attrs:
        name = naming.to_cobol_name(a.get("normalized_name", a.get("name", "")))
        sql_type = a.get("normalized_type", {}).get("sql_type", a.get("type", ""))
        nullable = a.get("nullable")
        null_text = "nullable" if nullable else "not null"
        if name:
            parts.append(f"{name} ({sql_type}, {null_text})")
    return "\n".join(parts) if parts else "(no fields)"


def _extract_entity_field_names(ent: Dict, io_ent: Dict) -> List[str]:
    names: List[str] = []
    if io_ent:
        for f in io_ent.get("inputs", []) or []:
            cobol = f.get("cobol_name") or naming.to_cobol_name(f.get("name", ""))
            if cobol:
                names.append(str(cobol).upper())
        for f in io_ent.get("outputs", []) or []:
            cobol = f.get("cobol_name") or naming.to_cobol_name(f.get("name", ""))
            if cobol:
                names.append(str(cobol).upper())
    for a in (ent.get("attrs", []) or []) if ent else []:
        cobol = naming.to_cobol_name(a.get("normalized_name", a.get("name", "")))
        if cobol:
            names.append(str(cobol).upper())
    return sorted(set([n for n in names if n]))


def _extract_linkage_group_params(prog_info: Dict) -> List[str]:
    call_iface = prog_info.get("call_interface") or {}
    group_params = []
    for p in call_iface.get("linkage_parameters", []) or []:
        name = p.get("name") or ""
        pic = (p.get("pic") or "").lower()
        if name and ("groupe" in pic or "group" in pic):
            group_params.append(name.upper())
    return group_params


def _linkage_prefix_for_group(group_name: str) -> str:
    up = (group_name or "").upper()
    if up.startswith("LK-"):
        return "LK-"
    return ""


def _extract_linkage_structure_fields(linkage_structure: str) -> Dict[str, List[str]]:
    """
    Extract real field names from linkage_structure text.

    Parses COBOL LINKAGE structure to extract actual field names, handling
    abbreviated names correctly (e.g., LK-ACTIVITE-NBPART instead of
    LK-ACTIVITE-NBPARTICIPANTS).

    Returns mapping: {group_name: [field1, field2, ...]}

    FIX 2026-01-15: This fixes the mismatch between linkage field names
    (abbreviated like LK-ACTIVITE-NBPART) and entity field names
    (full like ACTIVITE-NBPARTICIPANTS) that caused compilation errors.
    """
    if not linkage_structure:
        return {}

    mapping: Dict[str, List[str]] = {}
    current_group = None

    for line in linkage_structure.splitlines():
        line = line.strip()
        if not line:
            continue

        # Match 01 level (group definition): "01 LK-ACTIVITE."
        match_01 = re.match(r'^01\s+([A-Z][A-Z0-9-]+)', line, re.IGNORECASE)
        if match_01:
            current_group = match_01.group(1).upper()
            mapping[current_group] = []
            continue

        # Match 05 level (field definition): "05 LK-ACTIVITE-NBPART PIC 9(9)."
        match_05 = re.match(r'^05\s+([A-Z][A-Z0-9-]+)', line, re.IGNORECASE)
        if match_05 and current_group:
            field_name = match_05.group(1).upper()
            if field_name not in mapping[current_group]:
                mapping[current_group].append(field_name)

    # Sort fields for consistency
    for group in mapping:
        mapping[group] = sorted(mapping[group])

    return mapping


def _build_linkage_field_map(prog_info: Dict, ent: Dict, io_ent: Dict) -> Dict[str, List[str]]:
    """
    Build a mapping of linkage group names to their field names.

    FIX 2026-01-15: Now prioritizes extracting real field names from
    linkage_structure over constructing them from entity names. This fixes
    compilation errors when linkage uses abbreviated names.

    Priority:
    1. Extract from linkage_structure (real names, possibly abbreviated)
    2. Fallback to constructing from entity names (full names)
    """
    group_params = _extract_linkage_group_params(prog_info)
    if not group_params:
        return {}

    # FIX 2026-01-15: Try to extract real field names from linkage_structure first
    linkage_structure = prog_info.get("linkage_structure", "")
    if linkage_structure:
        structure_fields = _extract_linkage_structure_fields(linkage_structure)
        if structure_fields:
            # Filter to only include groups that are in group_params
            mapping = {g: fields for g, fields in structure_fields.items()
                      if g in group_params and fields}
            if mapping:
                LOG.debug("_build_linkage_field_map: using real names from linkage_structure")
                return mapping

    # Fallback: construct field names from entity (original behavior)
    base_names = _extract_entity_field_names(ent, io_ent)
    if not base_names:
        return {}
    mapping: Dict[str, List[str]] = {}
    for group in group_params:
        prefix = _linkage_prefix_for_group(group)
        if not prefix:
            continue
        fields = sorted({n if n.startswith(prefix) else f"{prefix}{n}" for n in base_names})
        if fields:
            mapping[group] = fields
    return mapping


def _extract_fetch_ws_vars(sql_block: Dict) -> List[str]:
    if not sql_block:
        return []
    fetch = sql_block.get("fetch_into") or ""
    if isinstance(fetch, list):
        text = "\n".join([str(x) for x in fetch if x])
    else:
        text = str(fetch)
    if not text:
        return []
    vars_found = re.findall(r":(WS-[A-Z0-9-]+)\b", text.upper())
    return sorted(set([v for v in vars_found if v]))


def _inject_missing_fetch_mappings(
    steps: List[str],
    sql_block: Dict,
    linkage_field_map: Dict[str, List[str]],
) -> List[str]:
    if not steps or not sql_block or not linkage_field_map:
        return steps
    fetch_vars = _extract_fetch_ws_vars(sql_block)
    if not fetch_vars:
        return steps
    group_names = list(linkage_field_map.keys())
    if not group_names:
        return steps
    group = group_names[0]
    lk_fields = {f.upper() for f in linkage_field_map.get(group, []) or []}
    if not lk_fields:
        return steps

    existing = set()
    pattern = re.compile(r"\bMOVE\s+(WS-[A-Z0-9-]+)\s+TO\s+(LK-[A-Z0-9-]+)\s+(?:OF|IN)\s+([A-Z0-9-]+)\b", re.IGNORECASE)
    for step in steps:
        if not isinstance(step, str):
            continue
        m = pattern.search(step)
        if m:
            existing.add((m.group(1).upper(), m.group(2).upper(), m.group(3).upper()))

    new_moves = []
    for ws_var in fetch_vars:
        base = ws_var[3:] if ws_var.startswith("WS-") else ws_var
        lk_var = f"LK-{base}"
        if lk_var not in lk_fields:
            continue
        key = (ws_var, lk_var, group)
        if key in existing:
            continue
        new_moves.append(f"MOVE {ws_var} TO {lk_var} OF {group}")

    if not new_moves:
        return steps

    # Insert after WHEN ZERO if present, otherwise append at end.
    inserted = False
    out = []
    for step in steps:
        out.append(step)
        if not inserted and isinstance(step, str) and step.strip().upper() == "WHEN ZERO":
            out.extend(new_moves)
            inserted = True
    if not inserted:
        out.extend(new_moves)
    return out


def _normalize_loop_control_steps(steps: List[str], fn: Dict) -> List[str]:
    if not steps:
        return steps
    loop = fn.get("loop_control") or {}
    eof_name = loop.get("eof_flag_name")
    true_val = loop.get("eof_true_value")
    false_val = loop.get("eof_false_value")
    if not (eof_name and true_val and false_val):
        return steps

    eof_upper = str(eof_name).upper()
    true_lit = f"'{true_val}'"
    false_lit = f"'{false_val}'"
    updated = []
    for step in steps:
        if not isinstance(step, str):
            updated.append(step)
            continue
        up = step.upper()
        # Replace MOVE SPACE(S) TO EOF with MOVE '<false>' TO EOF
        if re.search(rf"\bMOVE\s+SPACES?\s+TO\s+{re.escape(eof_upper)}\b", up):
            updated.append(f"MOVE {false_lit} TO {eof_name}")
            continue
        # Replace PERFORM UNTIL EOF NOT EQUAL SPACE with PERFORM UNTIL EOF = '<true>'
        if re.search(rf"\bPERFORM\s+UNTIL\s+{re.escape(eof_upper)}\s+NOT\s+EQUAL\s+SPACE\b", up):
            updated.append(f"PERFORM UNTIL {eof_name} = {true_lit}")
            continue
        if re.search(rf"\bPERFORM\s+UNTIL\s+{re.escape(eof_upper)}\s+NOT\s+EQUAL\s+SPACES\b", up):
            updated.append(f"PERFORM UNTIL {eof_name} = {true_lit}")
            continue
        updated.append(step)
    return updated


def _normalize_dal_connect_steps(steps: List[str], prog_info: Dict, fn: Dict) -> List[str]:
    if not steps or (fn.get("name") or "").upper() != "DAL-CONNECT":
        return steps
    ws_names = _extract_ws_names(prog_info.get("working_storage_lines", []) or [])
    connected_names = [n for n in ws_names if isinstance(n, str) and "CONNECTED" in n.upper()]
    if not connected_names:
        return steps
    connected = connected_names[0].upper()
    updated = list(steps)
    for idx, step in enumerate(steps):
        if not isinstance(step, str):
            continue
        up = step.upper()
        if "IF" in up and connected in up:
            for j in range(idx + 1, min(idx + 4, len(steps))):
                if isinstance(steps[j], str) and steps[j].strip().upper() == "GOBACK":
                    updated[j] = "EXIT PARAGRAPH"
                    return updated
    return updated

def _format_requirements(spec: Dict, related: List[str]) -> str:
    reqs = spec.get("exigences", []) or []
    if related:
        reqs = [r for r in reqs if r.get("id") in related]
    lines: List[str] = []
    for r in reqs:
        rid = r.get("id", "").strip()
        rtype = r.get("type", "").strip()
        rule = r.get("regle", "").strip()
        if not rule:
            continue
        label = f"[{rid}][{rtype}] " if rid or rtype else ""
        lines.append(f"{label}{rule}")
    return "\n".join(lines) if lines else "Aucune"


def _extract_cursor_name(sql_block: Dict) -> str:
    """
    Extract cursor name from SQL metadata (DECLARE/OPEN/FETCH).
    Returns uppercase name or empty string.
    """
    if not sql_block:
        return ""
    declare = sql_block.get("declare_cursor") or ""
    match = re.search(r"DECLARE\s+([A-Z0-9_-]+)\s+CURSOR", declare, re.IGNORECASE)
    if match:
        return match.group(1).upper()
    open_line = sql_block.get("open_cursor") or ""
    match = re.search(r"OPEN\s+([A-Z0-9_-]+)", open_line, re.IGNORECASE)
    if match:
        return match.group(1).upper()
    fetch_line = sql_block.get("fetch_into") or ""
    match = re.search(r"FETCH\s+([A-Z0-9_-]+)", fetch_line, re.IGNORECASE)
    if match:
        return match.group(1).upper()
    return ""


def _extract_ws_names(lines: List[str]) -> List[str]:
    """
    Extract ALL identifiers from WORKING-STORAGE lines.
    Includes: 01, 05, 10, 15, 77, 88 level items.
    Also extracts sub-field qualifications (e.g., FIELD-TEXT from "05 FIELD-TEXT").

    IMPROVED (2026-01-07): Extracts ALL levels and sub-fields, not just 01/77.
    This fixes the "unknown identifiers" issue where LLM uses valid WS vars but they're not in allowed list.
    """
    names: List[str] = []
    for l in lines or []:
        # Match any level number (01-88) followed by identifier
        # Also handle level-66 (RENAMES) and level-88 (condition names)
        m = re.match(r"\s*(?:01|05|10|15|20|25|30|35|40|45|49|66|77|88)\s+([A-Z0-9-]+)", l.strip().upper())
        if m:
            names.append(m.group(1))
    return sorted(set(names))


def _extract_01_structures(lines: List[str]) -> List[str]:
    """
    Extract 01-level structure names from WORKING-STORAGE lines.
    Returns names of group items (01 level with sub-fields).
    """
    structures: List[str] = []
    for i, l in enumerate(lines or []):
        # Match 01 level items (not 01 standalone vars like "01 VAR PIC X.")
        m = re.match(r"\s*01\s+([A-Z0-9-]+)\.?\s*$", l.strip().upper())
        if m:
            struct_name = m.group(1)
            # Check if next line has 05 (indicating it's a group structure)
            if i + 1 < len(lines):
                next_line = lines[i + 1].strip().upper()
                if re.match(r"\s*05\s+", next_line):
                    structures.append(struct_name)
    return structures


def _extract_linkage_names(prog_info: Dict) -> List[str]:
    """
    Extract linkage parameter names from program call interface.
    Uses program-level call_interface.linkage_parameters when present.

    FIX 2026-01-12: Also extract sub-fields from linkage_structure to prevent
    "OP-ID OF LK-OPERATION-REC n'est pas défini" errors when LLM generates steps.
    """
    names: List[str] = []
    call_iface = prog_info.get("call_interface") or {}
    for param in call_iface.get("linkage_parameters", []) or []:
        name = param.get("name")
        if isinstance(name, str) and name.strip():
            names.append(name.strip().upper())

    # FIX 2026-01-12: Extract all sub-fields from linkage_structure
    linkage_structure = prog_info.get("linkage_structure", "")
    if linkage_structure:
        # Parse COBOL structure to extract field names (01 and 05 levels)
        # Example: "05 LK-OP-ID PIC 9(8)." -> extract "LK-OP-ID"
        for line in linkage_structure.splitlines():
            line = line.strip()
            # Match: "01 FIELD-NAME" or "05 FIELD-NAME PIC ..."
            match = re.match(r'^\d+\s+([A-Z][A-Z0-9-]+)', line)
            if match:
                field_name = match.group(1).upper()
                names.append(field_name)

    result = sorted(set(names))
    # DEBUG: Log extracted linkage names
    if result:
        LOG.info(f"_extract_linkage_names: extracted {len(result)} names: {', '.join(result[:5])}...")
    return result


def _extract_sql_identifiers(sql_block: Dict) -> List[str]:
    """
    Extract SQL identifiers (cursor names, etc.) from function SQL block.
    """
    if not sql_block:
        return []
    sql_lines: List[str] = []
    for _, v in sql_block.items():
        if isinstance(v, list):
            sql_lines.extend([str(x) for x in v if x])
        elif v:
            sql_lines.append(str(v))
    if not sql_lines:
        return []
    text = "\n".join(sql_lines).upper()
    names = set()
    # Cursor names
    for pat in [
        r"\bDECLARE\s+([A-Z][A-Z0-9_-]*)\s+CURSOR\b",
        r"\bOPEN\s+([A-Z][A-Z0-9_-]*)\b",
        r"\bFETCH\s+([A-Z][A-Z0-9_-]*)\b",
        r"\bCLOSE\s+([A-Z][A-Z0-9_-]*)\b",
    ]:
        names.update(re.findall(pat, text))
    return sorted(names)


def _extract_call_targets(spec: Dict, current_program: str) -> Dict[str, List[str]]:
    """
    Extract valid CALL targets from spec with their interfaces.
    Returns dict with:
      - "programs": list of COBOL program names (from spec.programmes)
      - "interfaces": dict mapping program name to call_interface info
      - "ocesql": list of OCESQL function names (if any SQL detected)
      - "system": list of system functions

    ADDED (2026-01-07): Explicit CALL targets to prevent LLM from inventing targets.
    UPDATED (2026-01-08): Extract complete call_interface for each program.
    CRITICAL FIX (2026-01-11): Ne plus suggérer OCESQL runtime API en mode précompilé.
    Avec OCESQL précompilation (ocesql file.cbl file.cob), le .cbl doit contenir du
    SQL embarqué standard (EXEC SQL CONNECT...), pas des CALL 'OCESQLConnect'.
    """
    programs = []
    interfaces = {}
    ocesql_funcs = []
    system_funcs = []

    # Extract all program names and interfaces (excluding current program)
    for prog in spec.get("programmes", []) or []:
        prog_name = prog.get("name", "")
        if prog_name and prog_name != current_program:
            programs.append(prog_name)

            # Extract call_interface if present
            call_iface = prog.get("call_interface")
            if call_iface:
                interfaces[prog_name] = call_iface

    # CRITICAL FIX (2026-01-11): Ne PAS suggérer les fonctions OCESQL runtime en mode précompilé
    # Le SQL doit être écrit avec EXEC SQL, pas avec CALL 'OCESQL*'
    # Vérifier si le mode SQL est "direct" (appels runtime) ou "precompile" (défaut)
    sql_config = spec.get("sql", {}) or {}
    sql_method = (sql_config.get("connection", {}) or {}).get("method", "precompile")

    # Seulement si mode "direct" (appels runtime OCESQL), ajouter les fonctions
    # En mode "precompile" (défaut), ocesql_funcs reste vide []
    if sql_method == "direct":
        # Mode direct: le code .cbl appelle directement l'API OCESQL (rare)
        has_sql = False
        for fn in spec.get("fonctions", []) or []:
            if fn.get("sql") or fn.get("layer") == "dal":
                has_sql = True
                break

        if has_sql:
            ocesql_funcs = [
                "OCESQLStartSQL",
                "OCESQLConnect",
                "OCESQLExec",
                "OCESQLEndSQL",
                "OCESQLDisconnect",
                "OCESQLCursorDeclare",
                "OCESQLCursorOpen",
                "OCESQLCursorFetchOne",
                "OCESQLCursorClose",
            ]

    # System functions (if needed)
    # system_funcs = ["ACCEPT", "DISPLAY"]  # Already COBOL keywords

    return {
        "programs": sorted(set(programs)),
        "interfaces": interfaces,
        "ocesql": sorted(set(ocesql_funcs)),
        "system": sorted(set(system_funcs)),
    }


def _analyze_88_level_conditions(ws_lines: List[str]) -> Dict[str, Dict]:
    """
    Analyze 88-level conditions in WORKING-STORAGE to determine if they have FALSE clauses.

    Returns dict mapping condition name -> {
        'parent_field': str,
        'has_false_clause': bool,
        'true_value': str,
        'false_value': str or None
    }

    ADDED (2026-01-08): Extract 88-level condition info for proper SET/MOVE usage.
    """
    conditions = {}
    current_parent = None

    for line in ws_lines:
        # Remove quotes and strip
        line_clean = line.strip().strip('"')

        # Detect parent field (01 or 77 level)
        match_parent = re.match(r'^(01|77)\s+([A-Z0-9\-]+)', line_clean, re.IGNORECASE)
        if match_parent:
            current_parent = match_parent.group(2).upper()
            continue

        # Detect 88-level condition
        # Pattern: 88  CONDITION-NAME  VALUE 'Y' [FALSE 'N'].
        match_88 = re.match(
            r'^88\s+([A-Z0-9\-]+)\s+VALUE\s+[\'"](.*?)[\'"](?:\s+FALSE\s+[\'"](.*?)[\'"])?',
            line_clean,
            re.IGNORECASE
        )

        if match_88 and current_parent:
            condition_name = match_88.group(1).upper()
            true_value = match_88.group(2)
            false_value = match_88.group(3) if match_88.group(3) else None

            conditions[condition_name] = {
                'parent_field': current_parent,
                'has_false_clause': false_value is not None,
                'true_value': true_value,
                'false_value': false_value
            }

    return conditions


def _generate_88_level_instructions(conditions: Dict[str, Dict]) -> str:
    """
    Generate LLM instructions for manipulating 88-level conditions.

    ADDED (2026-01-08): Provide explicit guidance on SET vs MOVE based on FALSE clause presence.
    ENHANCED (2026-01-08): Add concrete examples and compilation errors to reinforce learning.
    """
    if not conditions:
        return ""

    instructions = [
        "=== MANIPULATION DES CONDITIONS 88-LEVEL (CRITIQUE) ===",
        "ATTENTION: Cette section est CRITIQUE pour eviter des erreurs de compilation.",
        "Les conditions 88-level ci-dessous ont des regles STRICTES en COBOL:",
        ""
    ]

    conditions_with_false = []
    conditions_without_false = []

    for cond_name, info in sorted(conditions.items()):
        if info['has_false_clause']:
            conditions_with_false.append((cond_name, info))
        else:
            conditions_without_false.append((cond_name, info))

    if conditions_with_false:
        instructions.append("Conditions AVEC clause FALSE (tu peux utiliser SET):")
        for cond_name, info in conditions_with_false:
            instructions.append(
                f"  - {cond_name}: SET {cond_name} TO TRUE  (met '{info['true_value']}' dans {info['parent_field']})"
            )
            instructions.append(
                f"    ou SET {cond_name} TO FALSE (met '{info['false_value']}' dans {info['parent_field']})"
            )
        instructions.append("")

    if conditions_without_false:
        instructions.extend([
            "Conditions SANS clause FALSE (INTERDIT d'utiliser SET TO FALSE):",
            ""])

        for cond_name, info in conditions_without_false:
            parent = info['parent_field']
            true_val = info['true_value']

            instructions.extend([
                f"  - {cond_name} (parent: {parent}, VALUE '{true_val}', PAS de clause FALSE)",
                f"    Pour activer  : SET {cond_name} TO TRUE",
                f"    Pour desactiver: MOVE 'N' TO {parent}  <-- UTILISE CECI, PAS SET TO FALSE",
                ""
            ])

        # Add concrete examples with ACTUAL condition names
        first_cond = conditions_without_false[0]
        cond_name = first_cond[0]
        parent = first_cond[1]['parent_field']

        instructions.extend([
            "EXEMPLES CONCRETS POUR CE PROGRAMME:",
            "",
            "  EXEMPLE CORRECT (utilise MOVE pour desactiver):",
            f"    IF {cond_name}",
            "        EXEC SQL CLOSE C_EMP END-EXEC",
            f"        MOVE 'N' TO {parent}  <-- CORRECT",
            "    END-IF",
            "",
            "  EXEMPLE INCORRECT (cause une erreur de compilation):",
            f"    IF {cond_name}",
            "        EXEC SQL CLOSE C_EMP END-EXEC",
            f"        SET {cond_name} TO FALSE  <-- ERREUR! 'le champ n'a pas une clause FALSE'",
            "    END-IF",
            "",
            "RAPPEL CRITIQUE:",
            f"  - JAMAIS: SET {cond_name} TO FALSE  (provoque: 'erreur: le champ n'a pas une clause FALSE')",
            f"  - TOUJOURS: MOVE 'N' TO {parent}  (pour desactiver la condition)",
            ""
        ])

        # Add all condition examples
        instructions.append("LISTE COMPLETE DES DESACTIVATIONS REQUISES:")
        for cond_name, info in conditions_without_false:
            instructions.append(f"  - Pour desactiver {cond_name}: MOVE 'N' TO {info['parent_field']}")

    return "\n".join(instructions)


def _collect_allowed_identifiers(
    fn: Dict,
    prog_info: Dict,
    ent: Dict,
    io_ent: Dict,
    all_fn_names: List[str],
) -> List[str]:
    """
    Collect all allowed identifiers for validation.

    IMPROVED (2026-01-07): Now extracts ALL WS identifiers (including sub-fields),
    not just top-level 01/77. This fixes "unknown identifiers" errors.
    """
    allowed: List[str] = []
    for k in ("inputs", "outputs", "modifies"):
        allowed.extend(fn.get(k, []) or [])
    if io_ent and io_ent.get("inputs"):
        allowed.extend([f.get("cobol_name") for f in io_ent.get("inputs", []) if f.get("cobol_name")])
    for attr in ent.get("attrs", []) or []:
        allowed.append(naming.to_cobol_name(attr.get("normalized_name", attr.get("name", ""))))
    ws_names = _extract_ws_names(prog_info.get("working_storage_lines", []) or [])
    allowed.extend(ws_names)
    linkage_names = _extract_linkage_names(prog_info)
    allowed.extend(linkage_names)
    linkage_field_map = _build_linkage_field_map(prog_info, ent, io_ent)
    for fields in linkage_field_map.values():
        allowed.extend(fields)
    allowed.extend(_extract_sql_identifiers(fn.get("sql") or {}))
    allowed.extend(all_fn_names)
    allowed.extend(["SQLCA", "SQLCODE", "SQLSTATE", "SQLERRMC"])
    allowed.append(ent.get("name", ""))
    # If linkage defines LK-* subfields, drop unprefixed base names to avoid invalid LK references.
    lk_fields = {n for n in linkage_names if isinstance(n, str) and n.upper().startswith("LK-")}
    if linkage_field_map:
        base_names = set(_extract_entity_field_names(ent, io_ent))
        allowed = [a for a in allowed if not (isinstance(a, str) and a.strip().upper() in base_names)]
    elif lk_fields:
        base_names = {n[3:] for n in lk_fields if len(n) > 3}
        allowed = [a for a in allowed if not (isinstance(a, str) and a.strip().upper() in base_names)]

    cleaned = [a for a in allowed if isinstance(a, str) and a.strip()]
    return sorted(set(cleaned))


def _normalize_lk_field_refs(steps: List[str], prog_info: Dict, ent: Dict, io_ent: Dict) -> List[str]:
    """
    Remplace FIELD OF LK-GROUP par LK-FIELD OF LK-GROUP si LK-FIELD existe.
    Applique uniquement quand un groupe LK-* est defini dans l'interface.
    """
    if not steps:
        return steps
    linkage_field_map = _build_linkage_field_map(prog_info, ent, io_ent)
    if not linkage_field_map:
        return steps
    group_params = set(linkage_field_map.keys())
    lk_fields = {n.upper() for fields in linkage_field_map.values() for n in fields}

    def _repl(match: re.Match) -> str:
        field = match.group(1).upper()
        rel = match.group(2).upper()
        group = match.group(3).upper()
        if group in group_params:
            prefix = _linkage_prefix_for_group(group)
            if field.startswith(prefix):
                lk_field = field
            else:
                lk_field = f"{prefix}{field}"
            if lk_field in lk_fields:
                return f"{lk_field} {rel} {group}"
        return match.group(0)

    updated = []
    pattern = re.compile(r"\b([A-Z0-9-]+)\s+(OF|IN)\s+(LK-[A-Z0-9-]+)\b", re.IGNORECASE)
    for step in steps:
        if not isinstance(step, str):
            updated.append(step)
            continue
        updated.append(pattern.sub(_repl, step))
    return updated


def _clean_identifier(s: str) -> Optional[str]:
    """
    Clean and validate a single identifier.
    Returns the identifier if valid (matches [A-Z][A-Z0-9-]*), None otherwise.

    ADDED (2026-01-07): Filter out phrases, assignments, and invalid tokens.
    This fixes the "identifiants pollués" issue where prompts contained phrases like
    "Connexion déjà établie" or "OPERATION = 'READ'" as identifiers.

    CRITICAL FIX (2026-01-07): Removed underscore from pattern - COBOL only allows hyphens!
    """
    if not s or not isinstance(s, str):
        return None
    s = s.strip().upper()
    # Must match valid COBOL identifier pattern: starts with letter, contains letters/digits/hyphens ONLY
    # COBOL does NOT allow underscores!
    if re.match(r'^[A-Z][A-Z0-9-]*$', s):
        # Exclude overly long identifiers (likely sentences)
        if len(s) <= 50:  # COBOL max is usually 30-31, but be generous
            return s
    return None


def _clean_identifiers_list(identifiers: List[str]) -> List[str]:
    """
    Clean a list of identifiers, removing phrases and invalid tokens.

    ADDED (2026-01-07): Prevents "identifiants pollués" by filtering with regex [A-Z][A-Z0-9-]*.
    """
    cleaned = []
    for ident in identifiers:
        clean = _clean_identifier(ident)
        if clean:
            cleaned.append(clean)
    return sorted(set(cleaned))


def _format_allowed_identifiers(allowed_ids: List[str]) -> str:
    """Format allowed identifiers as comma-separated list (cleaned)."""
    cleaned = _clean_identifiers_list(allowed_ids)
    return ", ".join(cleaned) if cleaned else "(non specifie)"


def _strip_fences(text: str) -> str:
    if not text:
        return ""
    text = re.sub(r"^```[a-zA-Z]*\\s*", "", text.strip())
    text = re.sub(r"```$", "", text.strip())
    return text.strip()


def _parse_steps(text: str) -> List[str]:
    cleaned = _strip_fences(text)
    if not cleaned:
        return []
    # Try JSON array
    try:
        data = json.loads(cleaned)
        if isinstance(data, list):
            return [str(x).strip() for x in data if str(x).strip()]
    except Exception:
        pass
    # Try to extract JSON array from within text
    if "[" in cleaned and "]" in cleaned:
        frag = cleaned[cleaned.find("["):cleaned.rfind("]") + 1]
        try:
            data = json.loads(frag)
            if isinstance(data, list):
                return [str(x).strip() for x in data if str(x).strip()]
        except Exception:
            pass
    # Fallback: lines
    steps: List[str] = []
    for line in cleaned.splitlines():
        s = line.strip().lstrip("-").strip()
        if s:
            steps.append(s)
    return steps


_FORBIDDEN_FRAGMENTS = [
    "IDENTIFICATION DIVISION", "ENVIRONMENT DIVISION", "DATA DIVISION",
    "WORKING-STORAGE", "LINKAGE SECTION", "PROCEDURE DIVISION",
    "PROGRAM-ID", "AUTHOR.", "INSTALLATION.", "DATE-WRITTEN.",
]

# ADDED (2026-01-07): Forbidden keywords/patterns that don't exist in COBOL
# IMPORTANT (2026-01-07 evening): ALIGNED WITH PROMPT
# We ONLY forbid keywords that truly don't exist in COBOL or are always wrong.
# FUNCTION and END-COMPUTE are valid COBOL, so they're NOT forbidden here.
# The prompt will encourage COMPUTE ROUNDED as best practice, but FUNCTION ROUND is acceptable.
_FORBIDDEN_KEYWORDS = [
    "IS NEGATIVE",    # Doesn't exist in COBOL, use IF field < 0
    "IS POSITIVE",    # Doesn't exist in COBOL, use IF field > 0
    # Note: "IS NULL" is allowed in EXEC SQL context (handled separately in validation)
]

_COBOL_KEYWORDS = {
    # Control flow
    "IF", "ELSE", "END-IF", "END", "END-PERFORM", "END-EVALUATE", "END-READ", "END-CALL",
    "PERFORM", "CALL", "EVALUATE", "WHEN", "CONTINUE", "STOP", "RUN", "GOBACK",
    "EXIT", "THRU", "THROUGH", "UNTIL", "VARYING", "TIMES",
    # Data operations
    "MOVE", "COMPUTE", "ADD", "SUBTRACT", "MULTIPLY", "DIVIDE", "INITIALIZE",
    "SET", "DISPLAY", "ACCEPT", "INSPECT", "STRING", "UNSTRING",
    # SQL embedded
    "EXEC", "SQL", "END-EXEC", "OPEN", "FETCH", "CLOSE", "DECLARE",
    "CURSOR", "INTO", "USING", "COMMIT", "ROLLBACK",
    "SELECT", "INSERT", "UPDATE", "DELETE", "EXECUTE", "PREPARE",
    "WHERE", "FROM", "JOIN", "ON", "GROUP", "HAVING", "ORDER", "ASC", "DESC",
    "LIMIT", "OFFSET", "DISTINCT", "AS", "UNION", "ALL",
    # Operators & conditions
    "NUMERIC", "NOT", "AND", "OR", "IS", "ZERO", "NEGATIVE", "POSITIVE",
    "EQUAL", "GREATER", "LESS", "THAN", "NULL",
    # Modifiers & keywords
    "BY", "REFERENCE", "VALUE", "ROUNDED", "CONTENT",
    "TO", "WITH", "AT", "GIVING", "REMAINDER", "SIZE", "ERROR",
    "OF", "IN", "POINTER", "ADDRESS",
    # IO operations
    "READ", "WRITE", "REWRITE", "START", "OPEN", "CLOSE",
    # Functions & intrinsics
    "FUNCTION", "INTRINSIC", "ALL",
    "ROUND", "INTEGER", "LENGTH", "UPPER-CASE", "LOWER-CASE",
    "TRIM", "CURRENT-DATE", "WHEN-COMPILED", "NUMVAL", "MAX", "MIN",
    "SUM", "MEAN", "MEDIAN", "RANGE", "VARIANCE", "STANDARD-DEVIATION",
    # Other common keywords
    "OTHER", "TRUE", "FALSE", "CORRESPONDING", "CORR",
    "SPACE", "SPACES",
    "ENVIRONMENT", "ENVIRONMENT-NAME", "ENVIRONMENT-VALUE", "UPON",
}


def _validate_function_call(line_upper: str, allowed_set: set) -> Optional[str]:
    """
    Validate FUNCTION keyword usage. Returns error message if invalid, None if OK.
    Examples of valid usage: FUNCTION ROUND(...), FUNCTION LENGTH(...)
    """
    if "FUNCTION" not in line_upper:
        return None

    # Extract function names after FUNCTION keyword
    func_pattern = r"FUNCTION\s+([A-Z][A-Z0-9-]*)"
    matches = re.findall(func_pattern, line_upper)

    # Common COBOL intrinsic functions (always allowed)
    intrinsic_functions = {
        "ROUND", "INTEGER", "LENGTH", "UPPER-CASE", "LOWER-CASE",
        "TRIM", "CURRENT-DATE", "WHEN-COMPILED", "NUMVAL", "MAX", "MIN",
        "SUM", "MEAN", "MEDIAN", "RANGE", "VARIANCE", "STANDARD-DEVIATION",
        "RANDOM", "ABS", "SQRT", "LOG", "EXP", "SIN", "COS", "TAN",
        "ASIN", "ACOS", "ATAN", "MOD", "REM", "SIGN", "ANNUITY",
    }

    invalid_funcs = []
    for func_name in matches:
        if func_name not in intrinsic_functions and func_name not in allowed_set:
            invalid_funcs.append(func_name)

    if invalid_funcs:
        return f"unknown function(s) after FUNCTION keyword: {sorted(set(invalid_funcs))}"
    return None


def _validate_identifier_with_structure(line_upper: str, allowed_set: set) -> tuple[List[str], List[str]]:
    """
    Validate identifiers including COBOL structure notation (FIELD OF STRUCT, FIELD IN STRUCT).
    Returns: (unknown_identifiers, valid_structure_refs)

    Examples:
    - "SALARY OF EMPLOYEE" -> checks SALARY and EMPLOYEE separately
    - "WS-TOTAL" -> checks WS-TOTAL
    - "EMPLOYEE.SALARY" -> invalid (dot notation not allowed in COBOL)
    """
    # Remove quoted strings to avoid false positives
    no_strings = re.sub(r"'[^']*'", "", line_upper)

    # Extract all potential identifiers
    # CRITICAL FIX (2026-01-07): Use correct COBOL identifier pattern (no underscores!)
    # COBOL only allows: letters, digits, hyphens (not underscores)
    tokens = re.findall(r"[A-Z][A-Z0-9-]*", no_strings)

    unknown = []
    valid_structures = []

    # Check for structure notation: "FIELD OF STRUCT" or "FIELD IN STRUCT"
    struct_pattern = r"([A-Z][A-Z0-9-]*)\s+(?:OF|IN)\s+([A-Z][A-Z0-9-]*)"
    struct_matches = re.findall(struct_pattern, no_strings)

    for field, struct in struct_matches:
        # Both field and struct must be in allowed set
        if field in _COBOL_KEYWORDS or field in allowed_set:
            if struct in _COBOL_KEYWORDS or struct in allowed_set:
                valid_structures.append(f"{field} OF {struct}")
            else:
                unknown.append(struct)
        else:
            unknown.append(field)

    # Check remaining tokens (not part of OF/IN structure)
    # Remove tokens that are part of structure notation
    structure_tokens = set()
    for field, struct in struct_matches:
        structure_tokens.add(field)
        structure_tokens.add(struct)

    for t in tokens:
        if t in structure_tokens:
            continue  # Already validated as part of structure
        if t in _COBOL_KEYWORDS:
            continue
        if t in allowed_set:
            continue
        unknown.append(t)

    return (unknown, valid_structures)


def _validate_steps(
    steps: List[str],
    allowed_ids: List[str],
    max_cols: int,
    sql_actions: Optional[List[str]] = None,
    cursor_name: str = "",
    has_88_in_spec: bool = True,
) -> List[str]:
    """
    Validate generated steps against COBOL constraints.
    Returns list of error messages (empty if all valid).

    IMPROVED (2026-01-07): Added validation for forbidden keywords (FUNCTION, END-COMPUTE, IS NEGATIVE, etc.)
    IMPROVED (2026-01-11): Auto-add SQL identifiers (SQLCA, SQLCODE, etc.) when SQL is enabled
    """
    errors: List[str] = []
    allowed_set = {a.upper() for a in allowed_ids if a}

    # CRITICAL FIX (2026-01-11): Auto-add SQL identifiers when SQL is enabled
    # This prevents retry loops that remove SQLCA from CALL statements
    if sql_actions or cursor_name:
        sql_identifiers = {"SQLCA", "SQLCODE", "SQLSTATE", "SQLERRM", "SQLERRMC"}
        allowed_set.update(sql_identifiers)

    # CRITICAL FIX (2026-01-11): Forbid INCLUDE SQLCA in steps
    # SQLCA is already injected in WORKING-STORAGE by headers
    for idx, line in enumerate(steps, start=1):
        if "INCLUDE" in line.upper() and "SQLCA" in line.upper():
            errors.append(f"line {idx}: EXEC SQL INCLUDE SQLCA forbidden in steps (already in WORKING-STORAGE)")
            break

    # CRITICAL FIX (2026-01-11): Validate OCESQL signatures
    # Ensure OCESQL calls have correct parameters
    for idx, line in enumerate(steps, start=1):
        upper = line.upper()

        # OCESQLStartSQL must have USING SQLCA
        if "CALL 'OCESQLSTARTSQL'" in upper or 'CALL "OCESQLSTARTSQL"' in upper:
            if "USING SQLCA" not in upper:
                errors.append(f"line {idx}: CALL 'OCESQLStartSQL' must include USING SQLCA")

        # OCESQLConnect must have SQLCA as first parameter
        if "CALL 'OCESQLCONNECT'" in upper or 'CALL "OCESQLCONNECT"' in upper:
            if "USING SQLCA" not in upper:
                errors.append(f"line {idx}: CALL 'OCESQLConnect' must include USING SQLCA as first parameter")

        # CRITICAL FIX (2026-01-11): Detect UPDATE without RTRIM on enum/status columns
        # Prevents trailing spaces in database (LOW -> 'LOW       ')
        if "EXEC SQL" in upper and "UPDATE" in upper:
            enum_columns = ["RISK_LEVEL", "COMPLIANCE_STATUS", "RISK-LEVEL", "COMPLIANCE-STATUS"]
            for col in enum_columns:
                # Check if column is assigned without RTRIM/TRIM
                if f"{col} =" in upper:
                    # Extract the line to check for RTRIM
                    if "RTRIM(" not in upper and "TRIM(" not in upper:
                        # Check if it's a host var assignment (not a constant)
                        if ":WS-" in upper or ":LK-" in upper:
                            errors.append(f"line {idx}: use RTRIM(:hostvar) for {col} to avoid trailing spaces")
                            break

    in_sql_block = False

    for idx, line in enumerate(steps, start=1):
        # Check line length
        if len(line) > max_cols:
            errors.append(f"line {idx}: >{max_cols} cols (len={len(line)})")
            continue

        upper = line.upper()
        if "EXIT PERFORM" in upper:
            errors.append(f"line {idx}: EXIT PERFORM interdit (utiliser EXIT PARAGRAPH)")
            continue
        if not has_88_in_spec and "SET " in upper and (" TO TRUE" in upper or " TO FALSE" in upper):
            errors.append(f"line {idx}: SET ... TO TRUE/FALSE interdit sans 88-level dans la spec")
            continue
        starts_sql = "EXEC SQL" in upper
        ends_sql = "END-EXEC" in upper
        if starts_sql:
            in_sql_block = True

        # Check forbidden fragments (DIVISION, SECTION, etc.)
        if any(frag in upper for frag in _FORBIDDEN_FRAGMENTS):
            errors.append(f"line {idx}: forbidden fragment (DIVISION/SECTION/PROGRAM-ID)")
            continue

        # Check forbidden keywords (IS NEGATIVE, IS POSITIVE) - ADDED 2026-01-07
        # These keywords truly don't exist in COBOL standard
        for forbidden in _FORBIDDEN_KEYWORDS:
            if forbidden in upper:
                errors.append(f"line {idx}: forbidden keyword '{forbidden}' (use alternative)")
                break

        # Special check for IS NULL outside SQL context
        if "IS NULL" in upper or "IS NOT NULL" in upper:
            is_sql_context = in_sql_block or starts_sql or ends_sql
            if not is_sql_context:
                errors.append(f"line {idx}: 'IS NULL' only allowed in EXEC SQL blocks (use IS NUMERIC check instead)")

        # Check paragraph labels (e.g., "MY-PARA.")
        if re.match(r"^[A-Z0-9-]+\.$", upper) and upper not in {"END-IF.", "END-EVALUATE.", "END-PERFORM.", "END-EXEC."}:
            errors.append(f"line {idx}: paragraph label not allowed ('{line.strip()}')")
            continue

        # Check solitary period
        if upper.strip() == ".":
            errors.append(f"line {idx}: solitary period not allowed")
            continue

        # Skip identifier validation inside EXEC SQL blocks
        if in_sql_block or starts_sql or ends_sql:
            if ":LK-" in upper:
                errors.append(f"line {idx}: host vars in EXEC SQL must use WS-* (LK-* interdit)")
            if "CONNECT TO" in upper:
                errors.append(
                    f"line {idx}: syntaxe CONNECT non supportee (utiliser CONNECT :USER IDENTIFIED BY :PASS USING :DB)"
                )
            if ends_sql:
                in_sql_block = False
            continue

        # Validate FUNCTION calls (check if function name is valid)
        # FUNCTION keyword itself is allowed, but function names must be valid
        func_error = _validate_function_call(upper, allowed_set)
        if func_error:
            errors.append(f"line {idx}: {func_error}")
            continue

        # Validate identifiers (including structure notation)
        unknown, valid_structs = _validate_identifier_with_structure(upper, allowed_set)
        if unknown:
            errors.append(f"line {idx}: unknown identifiers {sorted(set(unknown))}")

        if ends_sql:
            in_sql_block = False

    # SQL action validation (order + presence)
    actions = [a.upper() for a in (sql_actions or []) if a]
    if actions:
        upper_steps = [s.upper() for s in steps]

        def _find_idx_contains(token: str) -> int:
            for i, s in enumerate(upper_steps):
                if token in s:
                    return i
            return -1

        cursor = cursor_name.upper() if cursor_name else ""
        declare_idx = -1
        open_idx = -1
        fetch_idx = -1

        if "DECLARE_CURSOR" in actions:
            token = f"DECLARE {cursor}" if cursor else "DECLARE "
            declare_idx = _find_idx_contains(token)
            if declare_idx < 0:
                errors.append("missing DECLARE CURSOR (sql_actions: declare_cursor)")

        if "OPEN_CURSOR" in actions:
            token = f"OPEN {cursor}" if cursor else "OPEN "
            open_idx = _find_idx_contains(token)
            if open_idx < 0:
                errors.append("missing OPEN CURSOR (sql_actions: open_cursor)")

        if "FETCH_CURSOR" in actions:
            token = f"FETCH {cursor}" if cursor else "FETCH "
            fetch_idx = _find_idx_contains(token)
            if fetch_idx < 0:
                errors.append("missing FETCH CURSOR (sql_actions: fetch_cursor)")

        if declare_idx >= 0 and open_idx >= 0 and declare_idx > open_idx:
            errors.append("DECLARE CURSOR must appear before OPEN CURSOR")
        if open_idx >= 0 and fetch_idx >= 0 and open_idx > fetch_idx:
            errors.append("OPEN CURSOR must appear before FETCH CURSOR")

    return errors


def _build_function_specific_commit_rules(spec: Dict, fn: Dict, program_id: str) -> str:
    """
    LEVEL 2 - DEFENSE IN DEPTH: Generate function-specific COMMIT interdictions.

    Analyzes cursor_commit_policy from spec and identifies which functions
    allow/forbid COMMIT, then generates explicit warnings for the current function.

    This complements Level 1 (general prompt enrichment in cobol_procedures.py)
    by adding function-specific interdictions in steps generation.
    """
    prog_info = _find_program_info(spec, program_id)
    if not prog_info.get("allowed_sql", False) or (prog_info.get("layer") or "").lower() != "dal":
        return ""

    behavior = (spec.get("sql", {}) or {}).get("behavior", {})
    cursor_policy = behavior.get("cursor_commit_policy", "")

    if not cursor_policy or cursor_policy != "no_commit_while_open":
        return ""

    fn_name = fn.get("name", "").upper()
    sql_actions = fn.get("sql_actions") or []

    # Identify END/CLOSE functions (allow COMMIT)
    is_end_function = any(action in ["close_cursor", "disconnect"] for action in sql_actions)
    is_end_function = is_end_function or "END" in fn_name or "CLOSE" in fn_name or "DISCONNECT" in fn_name

    # Identify processing functions (forbid COMMIT)
    is_processing_function = any(action in ["fetch_cursor", "insert", "update", "delete"] for action in sql_actions)
    is_processing_function = is_processing_function or any(name in fn_name for name in ["READ", "SAVE", "UPDATE", "DELETE", "INSERT", "FETCH", "PROCESS"])

    if is_end_function:
        # COMMIT allowed
        return f"""=== POLITIQUE COMMIT POUR {fn_name} ===
 COMMIT AUTORISE dans cette fonction ({fn_name}).
Raison: Cette fonction ferme le curseur (close_cursor/disconnect).
Tu peux utiliser: EXEC SQL COMMIT END-EXEC
"""
    elif is_processing_function:
        # COMMIT FORBIDDEN
        return f"""=== POLITIQUE COMMIT POUR {fn_name} (CRITIQUE) ===
❌ INTERDIT: EXEC SQL COMMIT END-EXEC

Raison: cursor_commit_policy = {cursor_policy}
Cette fonction fait partie du traitement (READ/SAVE/UPDATE/etc.).
COMMIT fermerait le curseur et causerait SQLCODE=-400 sur le prochain FETCH.

 UTILISE A LA PLACE:
  - Pour erreurs: EXEC SQL ROLLBACK END-EXEC
  - Pour success: NE RIEN FAIRE (le COMMIT sera fait dans la fonction END)

IMPORTANT: Si tu generes 'EXEC SQL COMMIT END-EXEC' dans {fn_name},
le programme plantera apres le premier enregistrement avec SQLCODE=-400.
"""
    else:
        # Unknown function type - be conservative
        return f"""=== POLITIQUE COMMIT POUR {fn_name} ===
  ATTENTION: cursor_commit_policy = {cursor_policy}
Si cette fonction fait partie d'un traitement avec curseur ouvert,
N'UTILISE PAS: EXEC SQL COMMIT END-EXEC (sauf si fonction de cloture/fin)
"""


def _validate_and_fix_commit_in_steps(spec: Dict, fn: Dict, steps: List[str]) -> tuple[List[str], List[str]]:
    """
    LEVEL 3 - DEFENSE IN DEPTH: Post-validation cleanup for COMMIT violations.

    Validates generated steps against cursor_commit_policy and automatically
    removes COMMIT from processing functions where it would break the cursor.

    Returns:
        tuple: (cleaned_steps, warnings)
            - cleaned_steps: Steps with COMMIT removed if necessary
            - warnings: List of warning messages for violations found and fixed
    """
    behavior = (spec.get("sql", {}) or {}).get("behavior", {})
    cursor_policy = behavior.get("cursor_commit_policy", "")

    if not cursor_policy or cursor_policy != "no_commit_while_open":
        return steps, []

    fn_name = fn.get("name", "").upper()
    sql_actions = fn.get("sql_actions") or []

    # Identify END/CLOSE functions (allow COMMIT)
    is_end_function = any(action in ["close_cursor", "disconnect"] for action in sql_actions)
    is_end_function = is_end_function or "END" in fn_name or "CLOSE" in fn_name or "DISCONNECT" in fn_name

    # Identify processing functions (forbid COMMIT)
    is_processing_function = any(action in ["fetch_cursor", "insert", "update", "delete"] for action in sql_actions)
    is_processing_function = is_processing_function or any(name in fn_name for name in ["READ", "SAVE", "UPDATE", "DELETE", "INSERT", "FETCH", "PROCESS"])

    if is_end_function or not is_processing_function:
        # COMMIT is allowed in this function
        return steps, []

    # COMMIT is forbidden - check and remove
    cleaned_steps = []
    warnings = []
    removed_count = 0

    for i, step in enumerate(steps):
        step_upper = step.upper().strip()

        # Detect COMMIT statements (various formats)
        has_commit = False
        if "EXEC SQL" in step_upper and "COMMIT" in step_upper:
            has_commit = True
        elif step_upper == "COMMIT" or step_upper.startswith("COMMIT "):
            has_commit = True

        if has_commit:
            # Remove this step
            removed_count += 1
            warnings.append(f"REMOVED step {i+1} in {fn_name}: '{step}' (violates cursor_commit_policy={cursor_policy})")
            LOG.warning(f"Level 3 cleanup: Removed COMMIT from {fn_name} step {i+1}")
        else:
            cleaned_steps.append(step)

    if removed_count > 0:
        warnings.append(f"Total: {removed_count} COMMIT statement(s) removed from {fn_name} to prevent SQLCODE=-400")

    return cleaned_steps, warnings


def _build_prompt(
    spec: Dict,
    fn: Dict,
    prog_info: Dict,
    ent: Dict,
    io_ent: Dict,
) -> str:
    """
    Build LLM prompt for steps generation.

    IMPROVED (2026-01-07 - Round 2):
    - Added SEPARATION DES COUCHES (layer-specific rules)
    - Added MAPPING ORCHESTRATION (strict operation rules)
    - Cleaned identifiers list (regex filtering)
    - Cleaned inputs/outputs extraction
    - Clarified OF keyword rules
    """
    def _normalize_connection_vars(conn: Dict) -> Dict[str, str]:
        raw = conn.get("vars")
        if raw is None:
            raw = conn.get("variables")
        if raw is None:
            raw = conn.get("var_map")
        out: Dict[str, str] = {}
        if isinstance(raw, dict):
            for key, val in raw.items():
                if key and val not in [None, ""]:
                    out[str(key).lower()] = str(val)
        elif isinstance(raw, (list, tuple)):
            for entry in raw:
                if isinstance(entry, dict):
                    role = entry.get("role") or entry.get("key") or entry.get("name")
                    var = entry.get("var") or entry.get("value") or entry.get("variable")
                    if role and var:
                        out[str(role).lower()] = str(var)
        return out

    def _extract_pic_map(ws_lines: List[str]) -> Dict[str, str]:
        pic_map: Dict[str, str] = {}
        for line in ws_lines:
            if not isinstance(line, str):
                continue
            upper = line.upper()
            if " PIC " not in upper:
                continue
            m = re.search(r"\b([A-Z0-9-]+)\b\s+PIC\s+([A-Z0-9()V\.-]+)", upper)
            if not m:
                continue
            name = m.group(1)
            pic = m.group(2)
            if name and pic:
                pic_map[name] = pic
        return pic_map

    def _extract_ws_names(ws_lines: List[str]) -> List[str]:
        names = []
        for line in ws_lines:
            if not isinstance(line, str):
                continue
            m = re.match(r"^\s*\d+\s+([A-Z0-9-]+)\b", line.strip().upper())
            if m:
                names.append(m.group(1))
        return names

    def _is_alphanumeric_pic(pic: str) -> bool:
        pic = (pic or "").strip().upper()
        return pic.startswith("X") or pic.startswith("A") or pic.startswith("N")

    def _role_from_env_name(env_name: str) -> str:
        up = str(env_name).upper()
        if "PASS" in up:
            return "password"
        if "USER" in up:
            return "user"
        if "HOST" in up:
            return "host"
        if "PORT" in up:
            return "port"
        if "DATABASE" in up or "DBNAME" in up or "DB" in up or "NAME" in up:
            return "dbname"
        return ""

    def _infer_env_mapping(env_vars: List[str], conn_vars: Dict[str, str]) -> List[str]:
        mapping: List[str] = []
        for env_name in env_vars:
            role = _role_from_env_name(env_name)
            if role and conn_vars.get(role):
                mapping.append(f"{env_name} -> {conn_vars[role]}")
        return mapping
    entity = _entity_from_function(fn)
    program_id = fn.get("programme") or ""
    layer = fn.get("layer") or ""
    fmt = (spec.get("cobol_format") or {}).get("line_format", "fixed").lower()
    max_cols = 72 if fmt == "fixed" else 999
    required = fn.get("required", True)
    related = fn.get("related_exigences") or []
    req_text = _format_requirements(spec, related)
    sql_block = fn.get("sql") or {}
    sql_lines = []
    for k, v in sql_block.items():
        if isinstance(v, list):
            for item in v:
                sql_lines.append(f"{k}: {item}")
        else:
            sql_lines.append(f"{k}: {v}")
    sql_text = "\n".join(sql_lines) if sql_lines else "(aucun)"
    sql_actions = fn.get("sql_actions") or []
    sql_actions_text = " -> ".join(sql_actions) if sql_actions else "(aucune)"
    cursor_name = _extract_cursor_name(sql_block)

    # ADDED 2026-01-07: Extract and clarify business rules
    # Separate validation rules from business logic rules
    # UPDATED 2026-01-10: Filter by layer to prevent logic leakage into BUSINESS
    all_requirements = [r for r in spec.get("exigences", []) or [] if r.get("id") in related]
    filtered_requirements = cobol_constraints.filter_requirements_by_layer(all_requirements, layer)

    validation_rules = []
    business_logic_rules = []
    for r in filtered_requirements:
        rtype = r.get("type", "").lower()
        rule = r.get("regle", "").strip()
        if rtype == "validation":
            validation_rules.append(f"[{r.get('id')}] {rule}")
        elif rtype in ("regle_metier", "business_rule", "calcul"):
            business_logic_rules.append(f"[{r.get('id')}] {rule}")
        else:
            business_logic_rules.append(f"[{r.get('id')}] {rule}")

    # Add note if requirements were filtered for BUSINESS layer
    if layer == "business" and len(filtered_requirements) < len(all_requirements):
        validation_rules_text = "Aucune (regles de validation dans LOGIC)"
        business_rules_text = "Aucune (regles metier dans LOGIC)"
    else:
        validation_rules_text = "\n".join(validation_rules) if validation_rules else "Aucune"
        business_rules_text = "\n".join(business_logic_rules) if business_logic_rules else "Aucune"

    fields_text = _format_entity_fields(ent, io_ent)
    linkage_field_map = _build_linkage_field_map(prog_info, ent, io_ent)
    fn_names = [f.get("name") for f in spec.get("fonctions", []) or [] if f.get("programme") == program_id and f.get("name")]
    allowed_ids = _collect_allowed_identifiers(fn, prog_info, ent, io_ent, fn_names)
    allowed_text = _format_allowed_identifiers(allowed_ids)  # Now cleaned!
    ws_lines_list = prog_info.get("working_storage_lines", []) or []
    ws_lines = "\n".join(ws_lines_list)

    # ADDED 2026-01-08: Analyze 88-level conditions for proper SET/MOVE usage
    conditions_88 = _analyze_88_level_conditions(ws_lines_list)
    conditions_88_text = _generate_88_level_instructions(conditions_88)

    fn_display_lines = fn.get("display_lines") or []
    fn_display_text = "\n".join(fn_display_lines) if fn_display_lines else ""
    display_spec = fn.get("display") or {}
    display_spec_lines = []
    sep = display_spec.get("separator")
    if sep:
        display_spec_lines.append(f"separator: {sep!r}")
    for idx, line in enumerate(display_spec.get("lines", []) or [], start=1):
        parts = []
        label = line.get("label")
        field = line.get("field")
        cobol_name = line.get("cobol_name")
        if label:
            parts.append(f"label={label!r}")
        if field:
            parts.append(f"field={field}")
        if cobol_name:
            parts.append(f"cobol_name={cobol_name}")
        if parts:
            display_spec_lines.append(f"{idx}. " + ", ".join(parts))
    display_spec_text = "\n".join(display_spec_lines) if display_spec_lines else ""
    required_statements = []
    required_statements.extend(prog_info.get("required_statements", []) or [])
    required_statements.extend(fn.get("required_statements", []) or [])
    required_statements = list(dict.fromkeys([s for s in required_statements if s]))
    required_statements_text = ", ".join(required_statements) if required_statements else "(aucun)"
    linkage_params = (prog_info.get("call_interface") or {}).get("linkage_parameters", []) or []
    linkage_lines = []
    for param in linkage_params:
        name = param.get("name")
        if not name:
            continue
        pic = param.get("pic", "")
        direction = param.get("direction", "")
        parts = [name]
        if pic:
            parts.append(f"[{pic}]")
        if direction:
            parts.append(f"({direction})")
        linkage_lines.append(" ".join(parts))
    linkage_text = "\n".join(linkage_lines) if linkage_lines else ""
    logging_lines = "\n".join(prog_info.get("logging_lines", []) or [])
    flow_lines = "\n".join(prog_info.get("flow", []) or [])
    formatting_guidelines = "\n".join((spec.get("prompting", {}) or {}).get("formatting_guidelines", []) or [])

    # ADDED 2026-01-07: Extract CALL targets and paragraphs
    # UPDATED 2026-01-08: Include complete interfaces with parameters
    call_targets = _extract_call_targets(spec, program_id)
    call_targets_text = []
    if call_targets["programs"]:
        call_targets_text.append("Programmes COBOL:")
        for prog in call_targets["programs"]:
            call_targets_text.append(f"  - {prog}")

            # Add interface details if available
            prog_iface = call_targets.get("interfaces", {}).get(prog)
            if prog_iface:
                description = prog_iface.get("description", "")
                if description:
                    call_targets_text.append(f"    Interface: {description}")

                # Add linkage parameters
                linkage_params = prog_iface.get("linkage_parameters", [])
                if linkage_params:
                    call_targets_text.append(f"    Parametres USING (dans l'ordre):")
                    for param in linkage_params:
                        param_name = param.get("name", "")
                        param_pic = param.get("pic", "")
                        param_dir = param.get("direction", "")
                        param_desc = f"{param_name}"
                        if param_pic:
                            param_desc += f" [{param_pic}]"
                        if param_dir:
                            param_desc += f" ({param_dir})"
                        call_targets_text.append(f"      {param_desc}")
                    call_targets_text.append("")  # Blank line between programs

    if call_targets["ocesql"]:
        call_targets_text.append("Fonctions OCESQL:")
        for func in call_targets["ocesql"]:
            call_targets_text.append(f"  - {func}")
    call_targets_block = "\n".join(call_targets_text) if call_targets_text else "(aucun CALL autorise)"

    paragraphs_text = ", ".join(fn_names) if fn_names else "(aucun)"

    # ADDED 2026-01-07: Clean inputs/outputs (extract COBOL names only)
    inputs_clean = _clean_identifiers_list(fn.get("inputs", []) or [])
    outputs_clean = _clean_identifiers_list(fn.get("outputs", []) or [])
    modifies_clean = _clean_identifiers_list(fn.get("modifies", []) or [])

    # ADDED 2026-01-07: Layer-specific rules
    layer_rules = []
    if layer == "dal":
        layer_rules = [
            "COUCHE DAL: Tu generes du code d'acces aux donnees UNIQUEMENT.",
            "- AUTORISE: EXEC SQL, OPEN, FETCH, CLOSE, MOVE variables SQL",
            "- INTERDIT: COMPUTE, calculs metier, logique business",
            "- SCOPE: Lecture/ecriture base de donnees, pas de traitement",
        ]
    elif layer == "logic":
        layer_rules = [
            "COUCHE LOGIC: Tu orchestres les appels DAL et BUSINESS.",
            "- AUTORISE: PERFORM, CALL programmes, IF/ELSE pour flux",
            "- INTERDIT: SQL direct, DISPLAY (utilise BUSINESS pour affichage)",
            "- SCOPE: Boucles, conditions, orchestration uniquement",
        ]
    elif layer == "business":
        layer_rules = [
            "COUCHE BUSINESS: Tu generes uniquement de l'affichage.",
            "- AUTORISE: DISPLAY, formatage de sortie",
            "- INTERDIT: SQL, calculs, PERFORM/CALL",
            "- SCOPE: Presentation des donnees uniquement",
        ]
    layer_rules_text = "\n".join(layer_rules) if layer_rules else ""

    # ADDED 2026-01-07: Orchestration mapping (for LOGIC layer)
    orchestration_rules = []
    if layer == "logic":
        # Check if this is MAIN-PROCESS (needs strict flow template)
        is_main_process = fn.get("name", "").upper() in ["MAIN-PROCESS", "MAIN-ENTRY", "PROCESS-MAIN"]
        has_orchestration_calls = any(
            "DAL-DB" in str(call) or "BUSINESS" in str(call) for call in (fn.get("calls") or [])
        )
        if is_main_process or has_orchestration_calls:
            if is_main_process:
                # Strict flow template for MAIN-PROCESS
                dal_program = None
                business_program = None
                base_name = program_id or ""
                for suffix in ["-LOGIC", "-BUSINESS", "-DAL-DB"]:
                    if base_name.endswith(suffix):
                        base_name = base_name[: -len(suffix)]
                        break
                preferred_dal = f"{base_name}-DAL-DB" if base_name else None
                preferred_business = f"{base_name}-BUSINESS" if base_name else None
                if preferred_dal in call_targets["programs"]:
                    dal_program = preferred_dal
                else:
                    for prog in call_targets["programs"]:
                        if "DAL" in prog.upper():
                            dal_program = prog
                            break
                if preferred_business in call_targets["programs"]:
                    business_program = preferred_business
                else:
                    for prog in call_targets["programs"]:
                        if "BUSINESS" in prog.upper():
                            business_program = prog
                            break

                orchestration_rules = [
                    "FLOW TEMPLATE STRICT (OBLIGATOIRE pour MAIN-PROCESS):",
                    "Tu DOIS generer EXACTEMENT ce flow (ne pas inventer):",
                    "",
                    "NOTE: Les '...' dans les CALL ci-dessous signifient 'complete avec TOUS les parametres",
                    "      listes dans l'interface du programme' (voir section CALL TARGETS ci-dessous).",
                    "",
                    "1. DISPLAY bandeau de debut",
                    f"2. MOVE 'READ' TO OPERATION",
                    f"3. CALL '{dal_program or 'xxx-DAL-DB'}' USING OPERATION END-OF-FILE ...",
                    "4. PERFORM UNTIL EOF-REACHED",
                    "     - Incrementer compteur (ADD 1 TO WS-COUNT)",
                    "     - PERFORM fonction calcul metier (ex: CALCULATE-NET)",
                    f"     - MOVE 'SAVE' TO OPERATION",
                    f"     - CALL '{dal_program or 'xxx-DAL-DB'}' USING OPERATION ...",
                    f"     - CALL '{business_program or 'xxx-BUSINESS'}' USING ... (pour affichage)",
                    f"     - MOVE 'READ' TO OPERATION",
                    f"     - CALL '{dal_program or 'xxx-DAL-DB'}' USING OPERATION ...",
                    "   END-PERFORM",
                    f"5. MOVE 'END ' TO OPERATION (avec espace)",
                    f"6. CALL '{dal_program or 'xxx-DAL-DB'}' USING OPERATION ...",
                    "7. DISPLAY bandeau de fin + compteur",
                    "8. STOP RUN",
                    "",
                    "IMPORTANT: Ne PAS faire PERFORM DAL-xxx, toujours CALL le programme.",
                    "IMPORTANT: Chaque CALL doit inclure TOUS les parametres de l'interface (pas juste OPERATION).",
                ]
            elif has_orchestration_calls:
                # Generic orchestration mapping
                orchestration_rules = [
                    "MAPPING ORCHESTRATION (OBLIGATOIRE):",
                    "- Si OPERATION = 'READ' → CALL 'xxx-DAL-DB' USING OPERATION ...",
                    "- Si OPERATION = 'SAVE' → CALL 'xxx-DAL-DB' USING OPERATION ...",
                    "- Si OPERATION = 'END ' → CALL 'xxx-DAL-DB' USING OPERATION ...",
                    "- Entre chaque operation: PERFORM calculs metier si necessaire",
                    "NOTE: Les '...' signifient 'complete avec TOUS les parametres de l'interface'.",
                    "IMPORTANT: Respecte strictement ce flux. Pas d'invention.",
                ]
    orchestration_text = "\n".join(orchestration_rules) if orchestration_rules else ""

    # ADDED 2026-01-08: Add 88-level warnings at the top
    early_warnings = []
    if conditions_88_text and "SANS clause FALSE" in conditions_88_text:
        # Extract condition names for the warning
        no_false_conditions = [name for name, info in conditions_88.items() if not info['has_false_clause']]
        if no_false_conditions:
            early_warnings = [
                "!!! AVERTISSEMENT CRITIQUE - ERREUR DE COMPILATION FREQUENTE !!!",
                "",
                f"Ce programme contient des conditions 88-level SANS clause FALSE: {', '.join(no_false_conditions)}",
                "Pour ces conditions, tu DOIS utiliser MOVE pour les desactiver:",
                ""
            ]
            for name, info in conditions_88.items():
                if not info['has_false_clause']:
                    early_warnings.append(f"  - Pour desactiver {name}: MOVE 'N' TO {info['parent_field']}  (PAS 'SET {name} TO FALSE')")
            early_warnings.extend([
                "",
                "Si tu utilises 'SET condition TO FALSE' sans clause FALSE declaree,",
                "tu auras l'erreur: 'le champ n'a pas une clause FALSE' et la compilation ECHOUERA.",
                "Voir section MANIPULATION DES CONDITIONS 88-LEVEL ci-dessous pour details.",
                "",
                "=" * 70,
                ""
            ])

    instructions = early_warnings + [
        "Tu generes UNIQUEMENT une liste JSON de lignes de steps (liste de strings).",
        "Pas de Markdown, pas d'explications, pas de code complet.",
        f"Chaque ligne doit tenir sur {max_cols} colonnes max (format {fmt}).",
        "Si une condition est longue, decoupe en IF imbriques ou EVALUATE.",
        "Utilise END-IF/END-PERFORM/END-EVALUATE.",
        "",
        "=== REGLES STRICTES (CRITIQUE) ===",
        "1. Format JSON uniquement: [\"step1\", \"step2\", ...]",
        "2. Pas d'indentation dans les steps (sera ajoutee plus tard)",
        f"3. Maximum {max_cols} colonnes par ligne",
        "4. N'invente JAMAIS de variables ou CALL targets",
        "5. Utilise UNIQUEMENT les mots-cles COBOL minimaux (voir liste ci-dessous)",
        "",
        "=== MOTS-CLES COBOL AUTORISES ===",
        "IF, ELSE, END-IF, END-CALL, MOVE, COMPUTE, ROUNDED, ADD, SUBTRACT, MULTIPLY, DIVIDE,",
        "DISPLAY, PERFORM, CALL, EVALUATE, WHEN, END-EVALUATE, END-PERFORM, FUNCTION,",
        "EXEC SQL, END-EXEC, OPEN, FETCH, CLOSE, COMMIT, ROLLBACK,",
        "IS NUMERIC, IS NOT NUMERIC, EQUAL, NOT EQUAL, GREATER, LESS, ZERO, OF, IN,",
        "CONTINUE, GOBACK, STOP RUN, INITIALIZE, SET, ACCEPT, FROM, ENVIRONMENT, UPON, SPACE, SPACES",
        "",
        "=== IDENTIFIANTS (CRITIQUE) ===",
        "N'invente JAMAIS de variables: utilise UNIQUEMENT les identifiants de la liste 'IDENTIFIANTS AUTORISES'.",
        "Chaque identifiant (WS-*, LK-*, nom de champ) doit exister dans cette liste.",
        "Syntaxe structure: AUTORISE 'FIELD OF STRUCTURE' ou champ direct 'FIELD'.",
        "INTERDIT: notation pointee (STRUCTURE.FIELD), identifiants non declares.",
        "NOTE: Si tu vois SALARY-BRUT et EMPLOYEE dans la liste, tu peux utiliser:",
        "  - 'SALARY-BRUT' seul (prefere)",
        "  - 'SALARY-BRUT OF EMPLOYEE' (acceptable si necessaire pour clarte)",
        "",
        "=== EXEMPLES VALIDES ===",
        "  MOVE SALARY-BRUT TO WS-BRUT",
        "  MOVE SALARY-BRUT OF EMPLOYEE TO WS-BRUT",
        "  COMPUTE SALARY-NET ROUNDED = WS-BRUT * 0.7",
        "  IF WS-BRUT IS NOT NUMERIC",
        "  IF WS-TOTAL < 0",
        "  DISPLAY 'Erreur: salaire invalide'",
        "  CALL 'PROGRAM-NAME' USING PARAM1 PARAM2 PARAM3  <- Utilise TOUS les params de l'interface",
        "  PERFORM CALCULATE-TOTAL",
        "  END-IF",
        "",
        "=== EXEMPLES INVALIDES ===",
        "  MOVE EMPLOYEE.SALARY TO WS-TOTAL  <- INTERDIT (notation pointee)",
        "  COMPUTE WS-NEW-VAR = 100  <- INTERDIT (WS-NEW-VAR non declare)",
        "  IF SALARY IS NEGATIVE  <- INTERDIT (n'existe pas, utilise: IF SALARY < 0)",
        "  IF SALARY IS NOT NULL  <- INTERDIT hors SQL (utilise IS NOT NUMERIC)",
        "  CALL 'INVENTED-PROGRAM'  <- INTERDIT (pas dans liste CALL TARGETS)",
        "  CALL 'PROGRAM' USING PARAM1  <- INTERDIT si l'interface definit 3 params (manque PARAM2, PARAM3)",
        "  PERFORM DAL-SAVE dans LOGIC  <- INTERDIT (CALL le programme DAL, pas PERFORM)",
        "",
        "=== INTERDICTIONS (CRITIQUE) ===",
        "INTERDIT: divisions/sections, PROGRAM-ID, paragraphes (labels avec point), DATA/WS/LINKAGE.",
        "INTERDIT: ligne avec un seul point.",
        "INTERDIT: identifiants non declares ou inventes.",
        "INTERDIT: IS NEGATIVE, IS POSITIVE (n'existent pas en COBOL, utilise < 0, > 0).",
        "INTERDIT: IS NULL hors EXEC SQL (dans COBOL normal, utilise IS NOT NUMERIC).",
        "INTERDIT: CALL vers programme non liste dans CALL TARGETS.",
        "SCOPE: uniquement les instructions du paragraphe courant (pas de nouveaux paragraphes).",
        "",
        "=== BONNES PRATIQUES (recommandations) ===",
        "PREFERE: COMPUTE ROUNDED au lieu de FUNCTION ROUND (plus simple).",
        "PREFERE: Pas de END-COMPUTE (pas necessaire en COBOL, juste COMPUTE suffit).",
        "AUTORISE mais moins bien: FUNCTION ROUND(...) est valide mais verbose.",
        "",
        "=== FORMAT DE SORTIE ===",
        "JSON uniquement, pas d'espaces en debut de ligne (l'indentation sera ajoutee plus tard).",
        "Format: [\"instruction 1\", \"instruction 2\", ...]",
        "Les steps doivent etre exploitables par un generateur COBOL.",
        "",
        "=== PRINCIPE DE MINIMALISME ===",
        "Genere le minimum d'instructions necessaires pour satisfaire les exigences.",
        "Evite les variables intermediaires inutiles.",
        "Privilegie les COMPUTE directs plutot que MOVE + ADD + MOVE.",
    ]

    # ADDED 2026-01-10: Add GnuCOBOL intrinsic functions guide (hardcoded in Python, not in spec)
    instructions.extend(["", cobol_constraints.get_gnucobol_intrinsic_functions_guide(), ""])

    # ADDED 2026-01-11: Add OCESQL embedded SQL guide when SQL is detected
    if sql_actions or cursor_name:
        instructions.extend(["", cobol_constraints.get_ocesql_embedded_sql_guide(), ""])

    # Add specific 88-level interdictions if applicable
    if conditions_88_text and "SANS clause FALSE" in conditions_88_text:
        no_false_conditions = [name for name, info in conditions_88.items() if not info['has_false_clause']]
        if no_false_conditions:
            for cond_name in no_false_conditions:
                instructions.append(f"INTERDIT: SET {cond_name} TO FALSE (cause erreur compilation, utilise MOVE 'N' TO {conditions_88[cond_name]['parent_field']})")

    # Build prompt parts
    prompt_parts = [
        "\n".join(instructions),
        "",
        "=== CONTEXTE ===",
        f"Programme: {program_id}",
        f"Couche: {layer}",
        f"Entite: {entity}",
        f"Fonction: {fn.get('name')} (required={required})",
        f"Description: {fn.get('description', '')}",
    ]

    # Add layer-specific rules if present
    if layer_rules_text:
        prompt_parts.extend([
            "",
            "=== SEPARATION DES COUCHES (CRITIQUE) ===",
            layer_rules_text,
        ])
        # ADDED 2026-01-10: Reinforce BUSINESS = display only (hardcoded in Python)
        if layer == "business":
            prompt_parts.extend([
                "",
                cobol_constraints.get_business_layer_strict_rules(),
                ""
            ])
        if layer == "business" and (fn_display_text or display_spec_text):
            prompt_parts.extend([
                "NOTE BUSINESS: Si display_lines sont fournies, les steps DOIVENT etre exactement ces lignes,",
                "dans le meme ordre, puis ajouter uniquement les statements obligatoires (ex: GOBACK).",
                "AUCUN calcul, AUCUN IF, AUCUN CALL, AUCUN SQL.",
            ])
            if display_spec_text:
                prompt_parts.extend([
                    "NOTE BUSINESS (display spec): Si display (structure) est fourni et display_lines est vide,",
                    "genere UNIQUEMENT des DISPLAY a partir du spec: label + champ cobol_name, dans le meme ordre.",
                    "Ajouter uniquement les statements obligatoires (ex: GOBACK).",
                ])
        if layer == "dal" and sql_actions:
            prompt_parts.extend([
                "NOTE DAL: Tu DOIS respecter l'ordre des actions SQL ci-dessous.",
                "Si 'declare_cursor' est present, DECLARE doit apparaitre avant OPEN.",
                "Si 'open_cursor' est present, OPEN doit apparaitre avant FETCH.",
            ])

    # ADDED 2026-01-10: Add env_set_method for DAL functions that set PostgreSQL env vars
    if layer == "dal" and fn.get("name", "").upper() in ["DAL-SET-ENV", "SET-ENV", "SETUP-ENV"]:
        env_set_method = (spec.get("sql", {}) or {}).get("connection", {}).get("env_set_method", [])
        if env_set_method:
            env_guide = cobol_constraints.get_env_set_method_guide(env_set_method)
            prompt_parts.extend(["", env_guide, ""])
            # Add ENVIRONMENT-NAME and ENVIRONMENT-VALUE to allowed identifiers
            if "ENVIRONMENT-NAME" not in allowed_ids:
                allowed_ids.append("ENVIRONMENT-NAME")
            if "ENVIRONMENT-VALUE" not in allowed_ids:
                allowed_ids.append("ENVIRONMENT-VALUE")
            # Rebuild allowed_text with new identifiers
            allowed_text = _format_allowed_identifiers(allowed_ids)

    # ADDED: Connection mapping guidance for DAL-CONNECT / DAL-SET-ENV
    fn_name_upper = (fn.get("name") or "").upper()
    if layer == "dal" and ("CONNECT" in fn_name_upper or "SET-ENV" in fn_name_upper):
        conn = (spec.get("sql", {}) or {}).get("connection", {}) or {}
        conn_vars = _normalize_connection_vars(conn)
        env_vars = conn.get("env_vars") or []
        if isinstance(env_vars, str):
            env_vars = [env_vars]
        env_vars_list = [str(v) for v in env_vars if v]
        env_mapping = _infer_env_mapping(env_vars_list, conn_vars)
        pic_map = _extract_pic_map(ws_lines_list)
        alpha_vars = []
        for role, var in conn_vars.items():
            pic = pic_map.get(var.upper())
            if pic and _is_alphanumeric_pic(pic):
                alpha_vars.append(var)
        if conn_vars or env_vars_list:
            conn_vars_text = ", ".join(f"{k}={v}" for k, v in conn_vars.items()) if conn_vars else "(aucune)"
            env_vars_text = ", ".join(env_vars_list) if env_vars_list else "(aucune)"
            prompt_parts.extend([
                "",
                "=== MAPPING CONNEXION (CRITIQUE) ===",
                f"Variables de connexion (spec): {conn_vars_text}",
                f"Env vars (spec): {env_vars_text}",
            ])
            if env_mapping:
                prompt_parts.append("Mapping env -> variable: " + ", ".join(env_mapping))
            prompt_parts.extend([
                "INTERDIT: valeurs litterales (ex: 'postgres', 'password123').",
                "INTERDIT: CALL GETENV/C$GETENV ou usage direct de noms d'env comme identifiants COBOL.",
                "INTERDIT: variables hote LK-* dans EXEC SQL (utiliser WS-* uniquement).",
                "Si tu charges l'env: utiliser ACCEPT <var> FROM ENVIRONMENT '<ENV>' selon le mapping.",
            ])
            if alpha_vars:
                prompt_parts.append(
                    "Variables alphanumeriques (PIC X/A/N): "
                    + ", ".join(alpha_vars)
                    + " -> NE PAS utiliser IS NUMERIC; verifier SPACES uniquement."
                )
            if "SET-ENV" in fn_name_upper:
                prompt_parts.append("DAL-SET-ENV: charger l'env, pas de EXEC SQL CONNECT ici.")
                prompt_parts.append("DAL-SET-ENV: ne pas utiliser GOBACK/STOP RUN (paragraphe interne).")
            if "CONNECT" in fn_name_upper and "SET-ENV" not in fn_name_upper:
                user_var = conn_vars.get("user")
                pass_var = conn_vars.get("password")
                db_var = conn_vars.get("dbname")
                if user_var and pass_var and db_var:
                    prompt_parts.append(
                        f"CONNECT attendu: EXEC SQL CONNECT :{user_var} IDENTIFIED BY :{pass_var} USING :{db_var} END-EXEC."
                    )
                    prompt_parts.append("INTERDIT: CONNECT TO ... USER ... USING ... (non supporte OCESQL).")
                prompt_parts.append(
                    "DAL-CONNECT: si deja connecte (flag WS-* = true), NE PAS faire GOBACK/STOP RUN; "
                    "utiliser EXIT PARAGRAPH ou CONTINUE."
                )
        eof_flag = (spec.get("technique", {}) or {}).get("eof_flag", {})
        true_val = eof_flag.get("true_value")
        false_val = eof_flag.get("false_value")
        ws_flag_names = [
            name for name in _extract_ws_names(ws_lines_list)
            if isinstance(name, str) and name.endswith("-FLAG")
        ]
        if ws_flag_names and true_val and false_val:
            prompt_parts.extend([
                f"Valeurs de flag (spec): true='{true_val}', false='{false_val}'.",
                "Utiliser MOVE '<val>' vers *-FLAG (pas de SET ... TO TRUE/FALSE sans 88-level).",
                "Flags: " + ", ".join(ws_flag_names),
            ])

    # Add orchestration mapping if present
    if orchestration_text:
        prompt_parts.extend([
            "",
            orchestration_text,
        ])

    # ADDED 2026-01-10: Level 2 - Function-specific COMMIT interdictions
    commit_policy_rules = _build_function_specific_commit_rules(spec, fn, program_id)
    if commit_policy_rules:
        prompt_parts.extend([
            "",
            commit_policy_rules,
        ])

    # Add remaining sections
    prompt_parts.extend([
        "",
        "=== CALL TARGETS AUTORISES ===",
        call_targets_block,
        "IMPORTANT: N'utilise QUE les programmes listes ci-dessus. Inventer un CALL est INTERDIT.",
        "",
        "CRITIQUE: Pour chaque CALL vers un programme, tu DOIS passer TOUS les parametres listes",
        "dans 'Parametres USING (dans l'ordre)' ci-dessus, dans le MEME ORDRE, a CHAQUE appel.",
        "Exemple: Si un programme attend 3 parametres (A, B, C), tu dois TOUJOURS faire:",
        "  CALL 'PROGRAMME' USING A B C",
        "JAMAIS juste 'CALL PROGRAMME USING A' ou 'CALL PROGRAMME USING A B'.",
        "Les parametres sont OBLIGATOIRES et definis par la LINKAGE SECTION du programme appele.",
        "",
        "=== PARAGRAPHES AUTORISES (PERFORM) ===",
        paragraphs_text,
        "IMPORTANT: Utilise PERFORM uniquement vers les paragraphes listes ci-dessus.",
        "",
        "=== ENTREES / SORTIES (noms COBOL uniquement) ===",
        f"Inputs: {', '.join(inputs_clean) if inputs_clean else '(aucun)'}",
        f"Outputs: {', '.join(outputs_clean) if outputs_clean else '(aucun)'}",
        f"Modifies: {', '.join(modifies_clean) if modifies_clean else '(aucun)'}",
        "",
        "=== CHAMPS / STRUCTURE ===",
        fields_text,
        "",
        "=== WORKING-STORAGE (verbatim) ===",
        ws_lines or "(vide)",
        "",
    ])

    # ADDED 2026-01-11: Add field qualification note for 01-level structures
    structures_01 = _extract_01_structures(ws_lines_list)
    if structures_01:
        qual_note = "=== QUALIFICATION DES CHAMPS (CRITIQUE) ===\n"
        qual_note += f"ATTENTION: Les structures suivantes contiennent des sous-champs: {', '.join(structures_01)}\n"
        qual_note += "REGLE ABSOLUE: Tous les sous-champs DOIVENT etre qualifies avec 'OF <structure>'.\n\n"
        for struct in structures_01:
            qual_note += f"Exemple pour {struct}:\n"
            qual_note += f"  CORRECT:   MOVE 100 TO FIELD-NAME OF {struct}\n"
            qual_note += f"  CORRECT:   COMPUTE RESULT OF {struct} = VALUE1 OF {struct} + VALUE2 OF {struct}\n"
            qual_note += f"  INCORRECT: MOVE 100 TO FIELD-NAME  <-- ERREUR AMBIGUITE\n\n"
        qual_note += "IMPORTANT: Dans les regles metier ci-dessous, les noms de champs (ex: FEE-AMOUNT, AMOUNT-BRUT)\n"
        qual_note += f"font reference aux sous-champs de {structures_01[0] if len(structures_01) == 1 else 'la structure appropriee'}.\n"
        qual_note += "Tu DOIS toujours qualifier ces champs avec 'OF <structure>' dans ton code COBOL."
        prompt_parts.extend([
            qual_note,
            "",
        ])

    # ADDED 2026-01-08: Add 88-level condition manipulation instructions
    if conditions_88_text:
        prompt_parts.extend([
            conditions_88_text,
            "",
        ])

    if linkage_text:
        prompt_parts.extend([
            "=== LINKAGE PARAMETERS ===",
            linkage_text,
            "IMPORTANT: Utilise ces noms exacts dans USING et dans les steps.",
            "",
        ])
    if linkage_field_map:
        lines = ["=== LINKAGE GROUP FIELDS (prefixe requis) ==="]
        for group, fields in linkage_field_map.items():
            lines.append(f"{group}:")
            for field in fields:
                lines.append(f"  - {field}")
        lines.append("IMPORTANT: Ces noms LK-* doivent etre utilises pour les sous-champs des groupes.")
        prompt_parts.extend(lines + [""])

    loop = fn.get("loop_control") or {}
    if loop and loop.get("eof_flag_name") and loop.get("eof_true_value") and loop.get("eof_false_value"):
        prompt_parts.extend([
            "=== LOOP CONTROL (EOF) ===",
            f"eof_flag_name: {loop.get('eof_flag_name')}",
            f"eof_true_value: {loop.get('eof_true_value')}",
            f"eof_false_value: {loop.get('eof_false_value')}",
            "REGLE: initialiser l'EOF a la valeur false avant le premier READ.",
            "REGLE: boucle = PERFORM UNTIL EOF = eof_true_value (NE PAS utiliser SPACE).",
            "",
        ])

    if sql_actions and ("open_cursor" in sql_actions or "declare_cursor" in sql_actions):
        prompt_parts.extend([
            "=== CURSOR LIFECYCLE (CRITIQUE) ===",
            "Si tu ouvres un curseur ici, tu DOIS le fermer dans DAL-END (ou sur EOF si specifie).",
            "Ne pas re-declarer/re-ouvrir un curseur deja ouvert: utilise un flag WS-*.",
            "A la fermeture, remettre le flag d'ouverture a la valeur false (spec).",
            "",
        ])

    if linkage_field_map and sql_block.get("fetch_into"):
        group = list(linkage_field_map.keys())[0]
        prompt_parts.extend([
            "=== FETCH -> LINKAGE (CRITIQUE) ===",
            "Pour chaque variable WS- lue par FETCH, si un champ LK- correspondant existe",
            f"dans {group}, tu DOIS faire un MOVE WS-xxx TO LK-xxx OF {group} quand SQLCODE = 0.",
            "Cela inclut les colonnes de JOIN (ex: WS-ANTENNE-NOM -> LK-ANTENNE-NOM).",
            "",
        ])

    prompt_parts.extend([
        "=== SQL ACTIONS (ordre obligatoire) ===",
        sql_actions_text,
        "",
        "=== SQL (si applicable) ===",
        sql_text,
        "",
        "=== DISPLAY SPEC (structure) ===",
        display_spec_text or "(aucune)",
        "",
        "=== DISPLAY LINES (verbatim) ===",
        fn_display_text or "(aucune)",
        "",
        "=== STATEMENTS OBLIGATOIRES ===",
        required_statements_text,
        "",
        "=== REGLES METIER (calculs et transformations) ===",
        business_rules_text,
        "NOTE: Ces regles definissent les calculs a effectuer (ex: SALARY_NET = BRUT * 0.7).",
        "",
        "=== REGLES DE VALIDATION (controles) ===",
        validation_rules_text,
        "NOTE: Ces regles definissent les controles a effectuer AVANT calcul.",
        "Pour tester NULL/invalide en COBOL normal: utilise 'IS NOT NUMERIC' (pas 'IS NULL').",
        "Exemple: IF SALARY-BRUT IS NOT NUMERIC → traiter comme invalide.",
        "",
    ])

    if flow_lines:
        prompt_parts.extend(["", "=== FLOW ===", flow_lines])
    if logging_lines:
        prompt_parts.extend(["", "=== LOGGING ===", logging_lines])
    if formatting_guidelines:
        prompt_parts.extend(["", "=== FORMAT ===", formatting_guidelines])

    prompt_parts.extend([
        "",
        "=== IDENTIFIANTS AUTORISES ===",
        allowed_text or "(non specifie)",
        "",
        "=== SORTIE ATTENDUE ===",
        "Retourne un JSON valide: [\"step 1\", \"step 2\", ...]"
    ])

    return "\n".join(prompt_parts).strip()


def run(
    normalized_spec_path: str,
    config: Optional[Dict] = None,
    out_dir: Optional[str] = None,
    io_map_path: Optional[str] = None,
) -> str:
    if config is None:
        config = {}
    if out_dir is None:
        out_dir = "out"

    use_llm_steps = os.getenv("CSG_USE_LLM_STEPS", "1").lower() not in ("0", "false", "no")
    if not use_llm_steps:
        LOG.info("CSG_USE_LLM_STEPS=0 -> generation des steps ignoree")
        return normalized_spec_path

    spec = fs.read_json(normalized_spec_path)
    fonctions = spec.get("fonctions", []) or []
    missing = [fn for fn in fonctions if not (fn.get("steps") or [])]
    if not missing:
        LOG.info("Aucun step manquant dans la spec (skip)")
        return normalized_spec_path

    io_map = None
    if io_map_path:
        io_map = fs.read_json(io_map_path)
    else:
        io_path = Path(out_dir) / "io_map.json"
        if io_path.exists():
            io_map = fs.read_json(str(io_path))

    generated = []
    system = "Generate COBOL procedure steps from spec constraints."
    timeout_s = int(os.getenv("CSG_LLM_TIMEOUT_S", "600"))

    for fn in missing:
        program_id = fn.get("programme") or "PROGRAM"
        entity = _entity_from_function(fn)
        ent = _find_entity(spec, entity)
        io_ent = _find_entity_io(io_map, entity)
        prog_info = _find_program_info(spec, program_id)
        linkage_field_map = _build_linkage_field_map(prog_info, ent, io_ent)

        fn_names = [f.get("name") for f in spec.get("fonctions", []) or [] if f.get("programme") == program_id and f.get("name")]
        allowed_ids = _collect_allowed_identifiers(fn, prog_info, ent, io_ent, fn_names)
        call_targets = _extract_call_targets(spec, program_id)
        allowed_ids.extend(call_targets.get("programs", []))
        allowed_ids.extend(call_targets.get("ocesql", []))

        # CRITICAL FIX (2026-01-07): Clean allowed_ids with same logic as prompt
        # This ensures validation and prompt use the same identifier list
        allowed_ids = _clean_identifiers_list(allowed_ids)

        fmt = (spec.get("cobol_format") or {}).get("line_format", "fixed").lower()
        max_cols = 72 if fmt == "fixed" else 999

        prompt = _build_prompt(spec, fn, prog_info, ent, io_ent)
        response = llm_auto.generate(prompt=prompt, system=system, timeout_s=timeout_s, config=config)

        # Trace the LLM call if tracer is active
        try:
            if pipeline_tracer.get_trace_dir():
                pipeline_tracer.save_llm_call(
                    program_id=program_id,
                    stage=f"steps_{fn.get('name', 'UNKNOWN')}",
                    prompt=prompt,
                    response=response,
                    metadata={
                        "procedure": fn.get("name"),
                        "response_length": len(response or ""),
                    },
                )
            else:
                trace.log_prompt(out_dir, "steps", fn.get("name", "UNKNOWN"), prompt, response)
        except Exception as e:
            LOG.warning("Trace LLM steps failed: %s", e)

        steps = _parse_steps(response or "")
        steps = _normalize_lk_field_refs(steps, prog_info, ent, io_ent)
        steps = _normalize_loop_control_steps(steps, fn)
        steps = _normalize_dal_connect_steps(steps, prog_info, fn)
        steps = _inject_missing_fetch_mappings(steps, fn.get("sql") or {}, linkage_field_map)

        # ADDED 2026-01-10: Level 3 - Post-validation COMMIT cleanup
        steps, commit_warnings = _validate_and_fix_commit_in_steps(spec, fn, steps)
        if commit_warnings:
            for warning in commit_warnings:
                LOG.warning(warning)

        sql_actions = fn.get("sql_actions") or []
        cursor_name = _extract_cursor_name(fn.get("sql") or {})
        ws_lines_spec = prog_info.get("working_storage_lines", []) or []
        has_88_in_spec = any(re.match(r"^\s*88\b", str(l)) for l in ws_lines_spec)
        errors = _validate_steps(
            steps,
            allowed_ids,
            max_cols,
            sql_actions,
            cursor_name,
            has_88_in_spec=has_88_in_spec,
        )
        if errors:
            # Build detailed error feedback for retry
            error_feedback = "\n".join([f"  - {err}" for err in errors[:10]])  # Limit to first 10 errors
            if len(errors) > 10:
                error_feedback += f"\n  ... et {len(errors) - 10} autres erreurs"

            strict_prompt = (
                prompt
                + "\n\n=== ERREURS DE VALIDATION (CRITIQUE) ===\n"
                + "Ta reponse precedente contenait les erreurs suivantes:\n"
                + error_feedback
                + "\n\n"
                + "Corrige ces erreurs en respectant STRICTEMENT:\n"
                + "1. N'utilise QUE les identifiants de la liste 'IDENTIFIANTS AUTORISES' (pas d'invention).\n"
                + "2. Utilise 'FIELD OF STRUCTURE' pour acceder aux champs (pas de notation pointee).\n"
                + f"3. Respecte la limite de {max_cols} colonnes par ligne.\n"
                + "4. Ne genere JAMAIS de DIVISION, SECTION, PROGRAM-ID, ou labels de paragraphe.\n"
                + "5. Retourne uniquement un JSON valide: [\"step1\", \"step2\", ...]\n"
                + "\n"
                + "Si tu ne peux pas generer de steps valides, retourne [] uniquement."
            )
            response2 = llm_auto.generate(prompt=strict_prompt, system=system, timeout_s=timeout_s, config=config)
            try:
                if pipeline_tracer.get_trace_dir():
                    pipeline_tracer.save_llm_call(
                        program_id=program_id,
                        stage=f"steps_{fn.get('name', 'UNKNOWN')}_retry",
                        prompt=strict_prompt,
                        response=response2,
                        metadata={
                            "procedure": fn.get("name"),
                            "response_length": len(response2 or ""),
                            "retry": True,
                        },
                    )
                else:
                    trace.log_prompt(out_dir, "steps_retry", fn.get("name", "UNKNOWN"), strict_prompt, response2)
            except Exception:
                pass
            steps = _parse_steps(response2 or "")
            steps = _normalize_lk_field_refs(steps, prog_info, ent, io_ent)
            steps = _normalize_loop_control_steps(steps, fn)
            steps = _normalize_dal_connect_steps(steps, prog_info, fn)
            steps = _inject_missing_fetch_mappings(steps, fn.get("sql") or {}, linkage_field_map)

            # ADDED 2026-01-10: Level 3 - Post-validation COMMIT cleanup (retry path)
            steps, commit_warnings = _validate_and_fix_commit_in_steps(spec, fn, steps)
            if commit_warnings:
                for warning in commit_warnings:
                    LOG.warning(warning)

            errors = _validate_steps(
                steps,
                allowed_ids,
                max_cols,
                sql_actions,
                cursor_name,
                has_88_in_spec=has_88_in_spec,
            )

            # Log retry outcome
            if not errors and steps:
                LOG.info("Retry successful for %s.%s: %d steps generated", program_id, fn.get("name"), len(steps))
            else:
                LOG.warning("Retry failed for %s.%s: still has %d errors", program_id, fn.get("name"), len(errors))

        if errors or not steps:
            LOG.warning("Steps invalid for %s.%s (total %d errors): %s",
                       program_id, fn.get("name"), len(errors), "; ".join(errors[:3]))
            if len(errors) > 3:
                LOG.warning("  ... et %d autres erreurs (voir logs)", len(errors) - 3)
            continue

        # Success: save steps
        fn["steps"] = steps
        fn["steps_generated"] = True
        generated.append({
            "programme": program_id,
            "fonction": fn.get("name"),
            "count": len(steps),
            "validated": True
        })
        LOG.info("Steps generated successfully for %s.%s: %d steps", program_id, fn.get("name"), len(steps))

    spec.setdefault("metadata", {})
    spec["metadata"]["steps_generated"] = {
        "count": len(generated),
        "functions": generated,
    }

    out_path = Path(out_dir) / "normalized_spec.json"
    fs.write_json(str(out_path), spec, sort_keys=True)
    trace.write_sidecar_hash(out_path)
    trace.write_meta(out_path, kind="normalized_spec_steps", extra={"generated_functions": len(generated)})

    LOG.info("Steps generation complete: %s fonctions mises a jour", len(generated))
    return str(out_path)

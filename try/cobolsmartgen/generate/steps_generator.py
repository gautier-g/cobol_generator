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

from cobolsmartgen.adapters import llm_auto
from cobolsmartgen.utils import fs, trace, pipeline_tracer, naming

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
    Also extracts sub-field qualifications (e.g., WS-DB-USER-TEXT from "05 WS-DB-USER-TEXT").

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


def _extract_linkage_names(prog_info: Dict) -> List[str]:
    """
    Extract linkage parameter names from program call interface.
    Uses program-level call_interface.linkage_parameters when present.
    """
    names: List[str] = []
    call_iface = prog_info.get("call_interface") or {}
    for param in call_iface.get("linkage_parameters", []) or []:
        name = param.get("name")
        if isinstance(name, str) and name.strip():
            names.append(name.strip().upper())
    return sorted(set(names))


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

    # Check if any SQL is used in spec → add OCESQL functions
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
    allowed.extend(_extract_linkage_names(prog_info))
    allowed.extend(_extract_sql_identifiers(fn.get("sql") or {}))
    allowed.extend(all_fn_names)
    allowed.extend(["SQLCA", "SQLCODE", "SQLSTATE", "SQLERRMC"])
    allowed.append(ent.get("name", ""))
    cleaned = [a for a in allowed if isinstance(a, str) and a.strip()]
    return sorted(set(cleaned))


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
) -> List[str]:
    """
    Validate generated steps against COBOL constraints.
    Returns list of error messages (empty if all valid).

    IMPROVED (2026-01-07): Added validation for forbidden keywords (FUNCTION, END-COMPUTE, IS NEGATIVE, etc.)
    """
    errors: List[str] = []
    allowed_set = {a.upper() for a in allowed_ids if a}
    in_sql_block = False

    for idx, line in enumerate(steps, start=1):
        # Check line length
        if len(line) > max_cols:
            errors.append(f"line {idx}: >{max_cols} cols (len={len(line)})")
            continue

        upper = line.upper()
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

    # ADDED 2026-01-07: Extract and clarify business rules
    # Separate validation rules from business logic rules
    validation_rules = []
    business_logic_rules = []
    for r in spec.get("exigences", []) or []:
        if r.get("id") in related:
            rtype = r.get("type", "").lower()
            rule = r.get("regle", "").strip()
            if rtype == "validation":
                validation_rules.append(f"[{r.get('id')}] {rule}")
            elif rtype in ("regle_metier", "business_rule", "calcul"):
                business_logic_rules.append(f"[{r.get('id')}] {rule}")
            else:
                business_logic_rules.append(f"[{r.get('id')}] {rule}")

    validation_rules_text = "\n".join(validation_rules) if validation_rules else "Aucune"
    business_rules_text = "\n".join(business_logic_rules) if business_logic_rules else "Aucune"

    fields_text = _format_entity_fields(ent, io_ent)
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

        has_operation = "OPERATION" in allowed_text.upper()
        if is_main_process or has_operation:
            if is_main_process:
                # Strict flow template for MAIN-PROCESS
                dal_program = None
                business_program = None
                for prog in call_targets["programs"]:
                    if "DAL" in prog.upper():
                        dal_program = prog
                    if "BUSINESS" in prog.upper():
                        business_program = prog

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
            else:
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
        "CONTINUE, GOBACK, STOP RUN, INITIALIZE, SET, ACCEPT",
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

    # Add orchestration mapping if present
    if orchestration_text:
        prompt_parts.extend([
            "",
            orchestration_text,
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
        sql_actions = fn.get("sql_actions") or []
        cursor_name = _extract_cursor_name(fn.get("sql") or {})
        errors = _validate_steps(steps, allowed_ids, max_cols, sql_actions, cursor_name)
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
            errors = _validate_steps(steps, allowed_ids, max_cols, sql_actions, cursor_name)

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


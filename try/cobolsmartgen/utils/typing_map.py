"""
SQL → COBOL typing helpers.

- normalize_sql_type(sql: str) -> dict
- sql_to_cobol(sql: str, dialect: str, mapping: dict | None = None) -> str

If `mapping` is None, attempts to load YAML from 'config/mapping_types.yaml'
via utils.fs.read_yaml. The mapping format is flexible:
we try the following lookups (first match wins):
    mapping['dialects'][dialect][BASE]
    mapping[dialect][BASE]
    mapping[BASE]
where value can be either a string PIC like "X(30)" or an object like:
    {'pic': '9(1)', 'comment': 'boolean 0/1'}

Supported SQL types (case-insensitive):
  INT, SMALLINT, BIGINT,
  DECIMAL/NUMERIC(p[,s]),
  CHAR(n), VARCHAR(n),
  DATE, TIME, TIMESTAMP, BOOLEAN
"""

from __future__ import annotations

import re
from pathlib import Path
from typing import Any, Dict, Optional

from .fs import read_yaml


_INT_RE = re.compile(r"^\s*(?:INT|INTEGER)\s*$", re.I)
_SMALLINT_RE = re.compile(r"^\s*SMALLINT\s*$", re.I)
_BIGINT_RE = re.compile(r"^\s*BIGINT\s*$", re.I)
_DEC_RE = re.compile(
    r"^\s*(?:DECIMAL|NUMERIC)\s*(?:\(\s*(\d+)\s*(?:,\s*(\d+)\s*)?\))?\s*$", re.I
)
_CHAR_RE = re.compile(r"^\s*CHAR(?:ACTER)?\s*\(\s*(\d+)\s*\)\s*$", re.I)
_VARCHAR_RE = re.compile(
    r"^\s*(?:VARCHAR|CHARACTER\s+VARYING)\s*\(\s*(\d+)\s*\)\s*$", re.I
)
_DATE_RE = re.compile(r"^\s*DATE\s*$", re.I)
_TIME_RE = re.compile(r"^\s*TIME\s*$", re.I)
_TIMESTAMP_RE = re.compile(
    r"^\s*TIMESTAMP(?:\s+(?:WITH|WITHOUT)\s+TIME\s+ZONE)?\s*$", re.I
)
_BOOLEAN_RE = re.compile(r"^\s*BOOL(?:EAN)?\s*$", re.I)


def normalize_sql_type(sql: str) -> Dict[str, Any]:
    """
    Parse an SQL type declaration string and return a normalized dict.

    Examples:
      "DECIMAL(8,2)" -> {"base": "DECIMAL", "precision": 8, "scale": 2}
      "VARCHAR(30)"  -> {"base": "VARCHAR", "length": 30}
      "INT"          -> {"base": "INT"}
    """
    s = sql.strip()

    if _INT_RE.match(s):
        return {"base": "INT"}
    if _SMALLINT_RE.match(s):
        return {"base": "SMALLINT"}
    if _BIGINT_RE.match(s):
        return {"base": "BIGINT"}

    m = _DEC_RE.match(s)
    if m:
        p = int(m.group(1)) if m.group(1) else 18
        s_ = int(m.group(2)) if m.group(2) else 0
        if s_ < 0 or p <= 0:
            raise ValueError(f"Invalid DECIMAL precision/scale in: {sql}")
        if s_ > p:
            raise ValueError(f"DECIMAL scale cannot exceed precision: {sql}")
        return {"base": "DECIMAL", "precision": p, "scale": s_}

    m = _CHAR_RE.match(s)
    if m:
        n = int(m.group(1))
        if n <= 0:
            raise ValueError(f"CHAR length must be > 0: {sql}")
        return {"base": "CHAR", "length": n}

    m = _VARCHAR_RE.match(s)
    if m:
        n = int(m.group(1))
        if n <= 0:
            raise ValueError(f"VARCHAR length must be > 0: {sql}")
        return {"base": "VARCHAR", "length": n}

    if _DATE_RE.match(s):
        return {"base": "DATE"}
    if _TIME_RE.match(s):
        return {"base": "TIME"}
    if _TIMESTAMP_RE.match(s):
        return {"base": "TIMESTAMP"}
    if _BOOLEAN_RE.match(s):
        return {"base": "BOOLEAN"}

    raise ValueError(f"Unsupported or ambiguous SQL type: {sql!r}")


# -----------------------------
# Mapping and conversion
# -----------------------------

_DEFAULTS = {
    # Integers: typical COBOL PIC sizes
    "SMALLINT": "9(5)",
    "INT": "9(9)",
    "BIGINT": "9(18)",
    # Character data
    "CHAR": lambda n: f"X({n})",
    "VARCHAR": lambda n: f"X({n})",
    # Date/time as strings by default (mapping can override)
    "DATE": "X(10)",        # YYYY-MM-DD
    "TIME": "X(8)",         # HH:MM:SS
    "TIMESTAMP": "X(26)",   # ISO-like with subseconds
    # DECIMAL handled specially
    # BOOLEAN handled specially (default: text Y/N)
}


def _load_mapping_if_needed(mapping: Optional[Dict[str, Any]]) -> Optional[Dict[str, Any]]:
    if mapping is not None:
        return mapping
    cfg_path = Path("config") / "mapping_types.yaml"
    if cfg_path.exists():
        return read_yaml(str(cfg_path))
    return None


def _lookup_pic(base: str, dialect: str, mapping: Optional[Dict[str, Any]]) -> Optional[Dict[str, Any] | str]:
    if not mapping:
        return None
    # Try nested "dialects" first
    for candidate in (
        mapping.get("dialects", {}).get(dialect, {}),
        mapping.get(dialect, {}),
        mapping,
    ):
        if not isinstance(candidate, dict):
            continue
        if base in candidate:
            return candidate[base]
    return None


def _ensure_decimal_bounds(precision: int, scale: int) -> tuple[int, int]:
    """Clamp DECIMAL to safe COBOL bounds (max 18 digits)."""
    if precision > 18:
        # Clamp precision; adjust scale if needed to keep non-negative integer digits
        shrink = precision - 18
        precision = 18
        scale = max(0, scale - shrink)
    return precision, scale


def _decimal_pic(precision: int, scale: int) -> str:
    """Build a COBOL PIC for a DECIMAL p,s using unsigned format."""
    precision, scale = _ensure_decimal_bounds(precision, scale)
    int_digits = precision - scale
    if int_digits < 1:
        # At least one integer digit; move one digit from scale if necessary
        int_digits = 1
        scale = max(0, precision - 1)
    if scale == 0:
        return f"9({int_digits})"
    return f"9({int_digits})V9({scale})"


def _apply_comment(pic_or_obj: str | Dict[str, Any], fallback_comment: Optional[str]) -> str:
    if isinstance(pic_or_obj, dict):
        pic = pic_or_obj.get("pic") or pic_or_obj.get("PIC") or ""
        comment = pic_or_obj.get("comment") or pic_or_obj.get("COMMENT")
        if comment:
            return f"{pic} *> {comment}"
        return pic
    if fallback_comment:
        return f"{pic_or_obj} *> {fallback_comment}"
    return str(pic_or_obj)


def sql_to_cobol(sql: str, dialect: str, mapping: Optional[Dict[str, Any]] = None) -> str:
    """
    Convert an SQL type declaration to a COBOL PIC string.

    Examples:
      sql_to_cobol("INT", "db2") -> "9(9)"
      sql_to_cobol("DECIMAL(8,2)", "db2") -> "9(6)V9(2)"
      sql_to_cobol("BOOLEAN", "postgres") -> "X(1) *> boolean (Y/N)"
    """
    info = normalize_sql_type(sql)
    base = info["base"]
    mapping = _load_mapping_if_needed(mapping)

    # Mapping override?
    mapped = _lookup_pic(base, dialect, mapping)
    # DECIMAL special handling (allow mapping to override entirely)
    if base == "DECIMAL":
        p = int(info.get("precision", 18))
        s = int(info.get("scale", 0))
        if s > p:
            raise ValueError(f"DECIMAL scale cannot exceed precision: {sql}")
        if isinstance(mapped, (str, dict)):
            return _apply_comment(mapped, None)
        return _decimal_pic(p, s)

    # BOOLEAN handling: default text Y/N unless mapping overrides
    if base == "BOOLEAN":
        if isinstance(mapped, (str, dict)):
            return _apply_comment(mapped, None)
        # Default: single char Y/N with comment
        return _apply_comment("X(1)", "boolean (Y/N)")

    # CHAR/VARCHAR with length
    if base in ("CHAR", "VARCHAR"):
        n = int(info.get("length", 1))
        if n <= 0:
            raise ValueError(f"{base} length must be > 0: {sql}")
        if isinstance(mapped, dict) and "pic" in mapped:
            # Allow templates like "X({length})"
            pic_tpl = str(mapped["pic"])
            pic = pic_tpl.replace("{length}", str(n))
            return _apply_comment({"pic": pic, "comment": mapped.get("comment")}, None)
        if isinstance(mapped, str):
            # Allow templates like "X({length})"
            return mapped.replace("{length}", str(n))
        # Defaults
        fn = _DEFAULTS[base]
        return fn(n) if callable(fn) else str(fn)

    # DATE/TIME/TIMESTAMP and integers
    if isinstance(mapped, (str, dict)):
        return _apply_comment(mapped, None)

    # Fallback to defaults
    default = _DEFAULTS.get(base)
    if default is None:
        # Shouldn't happen due to normalize guard; keep defensive
        raise ValueError(f"No mapping available for base type: {base}")
    return default if isinstance(default, str) else default  # type: ignore[return-value]

"""
Naming helpers for CobolSmartGen.

- program_id(entity, layer, scheme="UPPER-KEBAB") -> str
- var_case(name, scheme="UPPER_SNAKE") -> str
- normalize_identifier(name) -> str  (accent-safe, deterministic)

Rules:
- Layers normalized to BUSINESS | LOGIC | DAL.
- Remove accents, collapse whitespace.
- Allow A–Z, 0–9 and hyphen/underscore depending on scheme.
"""

from __future__ import annotations

import re
import unicodedata
from typing import Iterable


_LAYER_MAP = {
    "BUSINESS": "BUSINESS",
    "METIER": "BUSINESS",
    "DOMAIN": "BUSINESS",
    "LOGIC": "LOGIC",
    "APPLICATION": "LOGIC",
    "APP": "LOGIC",
    "DAL": "DAL",
    "DATA": "DAL",
    "DATAACCESS": "DAL",
    "DATA-ACCESS": "DAL",
    "DATA_ACCESS": "DAL",
}


def _strip_accents(s: str) -> str:
    nfkd = unicodedata.normalize("NFKD", s)
    return "".join(c for c in nfkd if not unicodedata.combining(c))


def _tokenize(name: str) -> list[str]:
    """
    Convert raw string to a list of alnum tokens (accent-free, uppercased).
    Separators: any non-alphanumeric char.
    """
    s = _strip_accents(name)
    s = s.upper()
    # Replace non-alnum with spaces, then split
    s = re.sub(r"[^A-Z0-9]+", " ", s)
    toks = [t for t in s.strip().split() if t]
    return toks


def normalize_identifier(name: str) -> str:
    """
    Normalize a single identifier in a scheme-agnostic way:
    - Strip accents
    - Uppercase A-Z0-9
    - Replace other chars with underscores
    - Collapse multiple underscores
    """
    s = _strip_accents(name).upper()
    s = re.sub(r"[^A-Z0-9_-]+", "_", s)
    s = re.sub(r"__+", "_", s)
    s = s.strip("_-")
    return s or "ID"


def _join(tokens: Iterable[str], scheme: str) -> str:
    scheme = scheme.upper()
    if scheme in {"UPPER_SNAKE", "SNAKE", "SNAKE_CASE"}:
        return "_".join(tokens)
    if scheme in {"UPPER-KEBAB", "KEBAB", "KEBAB-CASE"}:
        return "-".join(tokens)
    # Default: snake
    return "_".join(tokens)


def _normalize_layer(layer: str) -> str:
    key = normalize_identifier(layer).replace("-", "").replace("_", "")
    return _LAYER_MAP.get(key, normalize_identifier(layer))


def var_case(name: str, scheme: str = "UPPER_SNAKE") -> str:
    """
    Convert an arbitrary name to the given UPPER_* scheme.
    Examples:
        var_case("employee name", "UPPER_SNAKE") -> "EMPLOYEE_NAME"
        var_case("employee-name", "UPPER-KEBAB") -> "EMPLOYEE-NAME"
    """
    tokens = _tokenize(name)
    if not tokens:
        return "NAME"
    return _join(tokens, scheme)


def program_id(entity: str, layer: str, scheme: str = "UPPER-KEBAB") -> str:
    """
    Build a PROGRAM-ID like 'EMPLOYEE-LOGIC' or 'EMPLOYEE_DAL'.
    Layers are normalized to BUSINESS | LOGIC | DAL.
    """
    ent = var_case(entity, scheme)
    lay = _normalize_layer(layer)
    lay = var_case(lay, scheme)
    return _join([ent, lay], scheme)

def to_cobol_name(sql_name: str) -> str:
    """
    Convert SQL-style name to COBOL-style name with hyphens.
    Examples:
        to_cobol_name("SALARY_NET") -> "SALARY-NET"
        to_cobol_name("emp_id") -> "EMP-ID"
    """
    # Strip accents and normalize to uppercase
    s = _strip_accents(sql_name).upper()
    # Replace underscores with hyphens
    s = re.sub(r"[^A-Z0-9]+", "-", s)
    # Remove leading/trailing hyphens
    s = s.strip("-")
    # Collapse multiple hyphens
    s = re.sub(r"-+", "-", s)
    return s or "FIELD"

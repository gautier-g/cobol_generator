"""
Filesystem utilities for CobolSmartGen.

Python 3.10+, stdlib only.
Public API:
- ensure_dir
- read_text / write_text
- read_json / write_json
- read_yaml / write_yaml
- list_files
- atomic_write
"""

from __future__ import annotations

import json
import os
import secrets
from pathlib import Path
from typing import Any, Dict, List


def ensure_dir(path: str | os.PathLike[str]) -> Path:
    """
    Ensure that a directory exists, creating parents as needed.

    Returns the Path object for the directory.
    """
    p = Path(path)
    p.mkdir(parents=True, exist_ok=True)
    return p


def _normalize_newlines(text: str) -> str:
    """Normalize line endings to LF."""
    # Replace CRLF first, then lone CR
    return text.replace("\r\n", "\n").replace("\r", "\n")


def read_text(path: str | os.PathLike[str], encoding: str = "utf-8") -> str:
    """Read a text file using given encoding."""
    with Path(path).open("r", encoding=encoding, newline="") as f:
        return f.read()


def atomic_write(path: str | os.PathLike[str], data: bytes) -> Path:
    """
    Atomically write bytes to `path`.

    Strategy: write to `<path>.tmp-<pid>-<rand>` in the same directory,
    fsync it, then os.replace() to target path.
    """
    target = Path(path)
    ensure_dir(target.parent)
    tmp_name = f"{target.name}.tmp-{os.getpid()}-{secrets.token_hex(8)}"
    tmp_path = target.with_name(tmp_name)

    # Write bytes
    with tmp_path.open("wb") as f:
        f.write(data)
        f.flush()
        os.fsync(f.fileno())

    # On most platforms, replace is atomic when on same filesystem
    os.replace(tmp_path, target)
    return target


def write_text(
    path: str | os.PathLike[str],
    data: str,
    encoding: str = "utf-8",
    atomic: bool = True,
) -> Path:
    """Write text, normalizing to LF. Create parent directories as needed."""
    target = Path(path)
    ensure_dir(target.parent)
    text = _normalize_newlines(data)

    if atomic:
        return atomic_write(target, text.encode(encoding))
    else:
        with target.open("w", encoding=encoding, newline="\n") as f:
            f.write(text)
            f.flush()
            os.fsync(f.fileno())
        return target


def read_json(path: str | os.PathLike[str]) -> Dict[str, Any]:
    """Read a JSON file and return a dict."""
    with Path(path).open("r", encoding="utf-8") as f:
        obj = json.load(f)
    if not isinstance(obj, dict):
        raise ValueError(f"JSON root must be an object/dict: {path}")
    return obj


def write_json(
    path: str | os.PathLike[str],
    data: Dict[str, Any],
    sort_keys: bool = True,
    atomic: bool = True,
) -> Path:
    """Write a dict as pretty JSON (UTF-8, indent=2, ensure_ascii=False)."""
    text = json.dumps(data, ensure_ascii=False, indent=2, sort_keys=sort_keys)
    return write_text(path, text, encoding="utf-8", atomic=atomic)


def read_yaml(path: str | os.PathLike[str]) -> Dict[str, Any]:
    """
    Read a YAML file using PyYAML safe_load.

    Raises ImportError with a clear message if PyYAML is not installed.
    """
    try:
        import yaml  # type: ignore
    except Exception as exc:  # pragma: no cover - import path
        raise ImportError(
            "PyYAML is required to read YAML files. Install with: pip install pyyaml"
        ) from exc

    with Path(path).open("r", encoding="utf-8") as f:
        obj = yaml.safe_load(f)

    if obj is None:
        return {}
    if not isinstance(obj, dict):
        raise ValueError(f"YAML root must be a mapping/dict: {path}")
    return obj


def write_yaml(
    path: str | os.PathLike[str],
    data: Dict[str, Any],
    atomic: bool = True,
) -> Path:
    """
    Write a dict to YAML using PyYAML safe_dump.

    Raises ImportError with a clear message if PyYAML is not installed.
    """
    try:
        import yaml  # type: ignore
    except Exception as exc:  # pragma: no cover - import path
        raise ImportError(
            "PyYAML is required to write YAML files. Install with: pip install pyyaml"
        ) from exc

    # Safe dump with unicode and block style
    text = yaml.safe_dump(
        data,
        sort_keys=True,
        allow_unicode=True,
        default_flow_style=False,
    )
    # Ensure LF endings
    return write_text(path, text, encoding="utf-8", atomic=atomic)


def list_files(
    root: str | os.PathLike[str],
    pattern: str = "*",
    recursive: bool = True,
) -> List[str]:
    """
    List files under `root` matching `pattern`.

    Returns a list of string paths (POSIX-style) sorted lexicographically.
    """
    r = Path(root)
    if not r.exists():
        return []
    paths = (r.rglob(pattern) if recursive else r.glob(pattern))
    files = [str(p) for p in paths if p.is_file()]
    files.sort()
    return files

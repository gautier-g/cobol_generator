"""
Artifact tracing utilities for CobolSmartGen.

- Hash artifacts (bytes, text, files)
- Write sidecar hash (.sha256) and metadata (.meta.json)
- Record prompts/responses for LLM calls with UTC timestamps

Depends on stdlib + utils.fs only.
"""

from __future__ import annotations

import hashlib
import os
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Dict, Optional

from .fs import (
    ensure_dir,
    read_json,
    write_json,
    write_text,
)


# -------------------------
# Hash helpers
# -------------------------

def sha256_bytes(data: bytes) -> str:
    """Return SHA-256 hex digest for raw bytes."""
    h = hashlib.sha256()
    h.update(data)
    return h.hexdigest()


def sha256_text(text: str, encoding: str = "utf-8") -> str:
    """Return SHA-256 hex digest for text."""
    return sha256_bytes(text.encode(encoding))


def sha256_file(path: str | os.PathLike[str], chunk_size: int = 1024 * 1024) -> str:
    """Return SHA-256 hex digest for file content."""
    h = hashlib.sha256()
    with Path(path).open("rb") as f:
        while True:
            chunk = f.read(chunk_size)
            if not chunk:
                break
            h.update(chunk)
    return h.hexdigest()


# -------------------------
# Sidecar writers
# -------------------------

def _utc_now_iso() -> str:
    """UTC timestamp in ISO-8601 with 'Z' suffix, seconds precision."""
    return datetime.now(timezone.utc).replace(microsecond=0).isoformat().replace("+00:00", "Z")


def write_sidecar_hash(path: str | os.PathLike[str]) -> str:
    """
    Compute SHA-256 of `path` and write `<path>.sha256` containing the hex digest.
    Returns the hex digest.
    """
    p = Path(path)
    digest = sha256_file(p)
    write_text(str(p.with_suffix(p.suffix + ".sha256")), f"{digest}\n", atomic=True)
    return digest


def write_meta(
    path: str | os.PathLike[str],
    kind: str,
    extra: Optional[Dict[str, Any]] = None,
) -> Path:
    """
    Write `<path>.meta.json` with basic metadata:
      - path, name, size, mtime_utc, sha256, kind, created_utc, extra
    Returns the Path to the meta file.
    """
    p = Path(path)
    st = p.stat()
    mtime_utc = datetime.fromtimestamp(st.st_mtime, tz=timezone.utc).replace(microsecond=0).isoformat().replace("+00:00", "Z")

    meta: Dict[str, Any] = {
        "path": str(p),
        "name": p.name,
        "size": st.st_size,
        "mtime_utc": mtime_utc,
        "sha256": sha256_file(p),
        "kind": kind,
        "created_utc": _utc_now_iso(),
    }
    if extra:
        meta["extra"] = extra

    meta_path = p.with_suffix(p.suffix + ".meta.json")
    write_json(str(meta_path), meta, sort_keys=True, atomic=True)
    return meta_path


# -------------------------
# Prompt logging
# -------------------------

def _slugify(name: str, max_len: int = 80) -> str:
    """
    Make a filename-safe slug (ASCII, -, _, .) from an arbitrary name.
    """
    import re
    import unicodedata

    # Strip accents
    name = unicodedata.normalize("NFKD", name)
    name = "".join(c for c in name if not unicodedata.combining(c))
    # Replace invalids with hyphen
    name = re.sub(r"[^A-Za-z0-9._-]+", "-", name)
    # Collapse duplicates
    name = re.sub(r"[-_]{2,}", "-", name)
    # Trim hyphens/dots
    name = name.strip(".-_")
    if not name:
        name = "untitled"
    if len(name) > max_len:
        name = name[:max_len].rstrip(".-")
    return name


def log_prompt(
    out_dir: str | os.PathLike[str],
    kind: str,
    name: str,
    prompt: str,
    response: Optional[str] = None,
    meta: Optional[Dict[str, Any]] = None,
) -> Dict[str, Any]:
    """
    Record an LLM prompt/response trio to:
      out/trace/prompts/{kind}/YYYYMMDD/HHMMSS_<name>.{prompt,response}.txt
    and write a small JSON meta beside them.

    Returns a dict with written paths and timestamps.
    """
    base_out = Path(out_dir)
    now = datetime.now(timezone.utc)
    day = now.strftime("%Y%m%d")
    hms = now.strftime("%H%M%S")
    safe_name = _slugify(name)

    dir_path = base_out / "trace" / "prompts" / kind / day
    ensure_dir(dir_path)

    base = dir_path / f"{hms}_{safe_name}"

    prompt_path = base.with_suffix(".prompt.txt")
    write_text(str(prompt_path), prompt, atomic=True)

    response_path = None
    if response is not None:
        response_path = base.with_suffix(".response.txt")
        write_text(str(response_path), response, atomic=True)

    meta_doc: Dict[str, Any] = {
        "kind": kind,
        "name": name,
        "slug": safe_name,
        "created_utc": now.replace(microsecond=0).isoformat().replace("+00:00", "Z"),
        "files": {
            "prompt": str(prompt_path),
            "response": str(response_path) if response_path else None,
        },
    }
    if meta:
        meta_doc["extra"] = meta

    meta_path = base.with_suffix(".meta.json")
    write_json(str(meta_path), meta_doc, sort_keys=True, atomic=True)

    return {
        "prompt": str(prompt_path),
        "response": str(response_path) if response_path else None,
        "meta": str(meta_path),
        "dir": str(dir_path),
        "timestamp_utc": meta_doc["created_utc"],
    }

# -------- Optional convenience wrappers (used by some modules) --------
def compute_hash(obj: Any) -> str:
    """
    Compute a stable SHA-256 over a JSON-serialized object (dict/list) or str.
    """
    import json as _json
    if isinstance(obj, (dict, list)):
        text = _json.dumps(obj, ensure_ascii=False, sort_keys=True)
    else:
        text = str(obj)
    return sha256_text(text)

def write_artifact(path: str | os.PathLike[str], kind: str, data: Any) -> Path:
    """
    Write an artifact (JSON if dict/list, otherwise text) then emit sidecar hash & meta.
    Returns the Path to the artifact.
    """
    p = Path(path)
    ensure_dir(p.parent)
    if isinstance(data, (dict, list)):
        write_json(str(p), data, sort_keys=True, atomic=True)
    else:
        write_text(str(p), str(data), atomic=True)
    write_sidecar_hash(p)
    write_meta(p, kind=kind)
    return p

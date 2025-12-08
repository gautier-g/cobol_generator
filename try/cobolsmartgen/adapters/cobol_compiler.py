"""
Abstract COBOL compilation.

Public API:
    compile_file(src: str, out_bin_dir: str, config: dict) -> tuple[int, str, str]

Details:
- Default compiler: GnuCOBOL (cobc).
- config["compiler"] supports:
    {
      "name": "cobc" | "ibm" | "microfocus",
      "line_format": "free" | "fixed",
      "flags": {
         "base": "-Wall",
         "debug": "",
         "optimize": ""
      },
      "copy_dirs": ["out/copy","copy"],
      "extra_args": "-Dsomething"  # optional string appended
    }
- For cobc: include -x and -free or -fixed + -I for each copy dir.
- Output binary in out_bin_dir/<PROGRAM-ID> where PROGRAM-ID = basename without .cbl
- Uses subprocess with text mode; timeout 300s.
"""

from __future__ import annotations

import shlex
import subprocess
from pathlib import Path
from typing import Dict, List, Tuple, Any

from cobolsmartgen.utils.fs import ensure_dir


_TIMEOUT_S = 300


def _split_flags(s: str | None) -> List[str]:
    if not s:
        return []
    # Respect quoted arguments
    return shlex.split(s)


def _program_id_from_src(src: Path) -> str:
    # PROGRAM-ID derived from filename stem, uppercased, hyphens preserved if present in name
    return src.stem.upper()


def _build_cobc_cmd(src: Path, out_bin: Path, compiler_cfg: Dict[str, Any]) -> List[str]:
    line_format = (compiler_cfg.get("line_format") or "free").lower()
    fmt_flag = "-free" if line_format == "free" else "-fixed"
    flags = compiler_cfg.get("flags", {}) or {}
    copy_dirs = compiler_cfg.get("copy_dirs", []) or []
    extra_args = compiler_cfg.get("extra_args", "")

    cmd: List[str] = ["cobc", "-x", fmt_flag, f"-o{str(out_bin)}"]
    cmd += _split_flags(flags.get("base"))
    # Include both debug/optimize if provided (leave empty if not)
    cmd += _split_flags(flags.get("debug"))
    cmd += _split_flags(flags.get("optimize"))
    for d in copy_dirs:
        cmd.append(f"-I{d}")
    cmd += _split_flags(extra_args)
    cmd.append(str(src))
    return cmd


def _build_ibm_cmd(src: Path, out_bin: Path, compiler_cfg: Dict[str, Any]) -> List[str]:
    """
    Best-effort IBM Enterprise COBOL CLI (cob2) invocation.
    Note: environments vary widely; allow overrides via flags/extra_args.
    """
    flags = compiler_cfg.get("flags", {}) or {}
    copy_dirs = compiler_cfg.get("copy_dirs", []) or []
    extra_args = compiler_cfg.get("extra_args", "")

    # 'cob2' typically links by default to a.out; use -o to set the name if supported.
    cmd: List[str] = ["cob2", f"-o{str(out_bin)}"]
    cmd += _split_flags(flags.get("base"))
    cmd += _split_flags(flags.get("debug"))
    cmd += _split_flags(flags.get("optimize"))
    for d in copy_dirs:
        cmd.append(f"-I{d}")
    cmd += _split_flags(extra_args)
    cmd.append(str(src))
    return cmd


def _build_mf_cmd(src: Path, out_bin: Path, compiler_cfg: Dict[str, Any]) -> List[str]:
    """
    Micro Focus COBOL (cob) typical flow:
      cob -x src.cbl -o out
    """
    flags = compiler_cfg.get("flags", {}) or {}
    copy_dirs = compiler_cfg.get("copy_dirs", []) or []
    extra_args = compiler_cfg.get("extra_args", "")

    cmd: List[str] = ["cob", "-x", str(src), f"-o{str(out_bin)}"]
    cmd += _split_flags(flags.get("base"))
    cmd += _split_flags(flags.get("debug"))
    cmd += _split_flags(flags.get("optimize"))
    for d in copy_dirs:
        cmd.append(f"-I{d}")
    cmd += _split_flags(extra_args)
    return cmd


def compile_file(src: str, out_bin_dir: str, config: Dict[str, Any]) -> Tuple[int, str, str]:
    """
    Compile a single COBOL file and return (rc, stdout, stderr).
    """
    src_path = Path(src)
    compiler_cfg: Dict[str, Any] = (config or {}).get("compiler", {}) or {}
    name = (compiler_cfg.get("name") or "cobc").lower()
    program_id = _program_id_from_src(src_path)
    out_dir = ensure_dir(out_bin_dir)
    out_bin = out_dir / program_id

    if name == "cobc":
        cmd = _build_cobc_cmd(src_path, out_bin, compiler_cfg)
    elif name in {"ibm", "cob2", "enterprise"}:
        cmd = _build_ibm_cmd(src_path, out_bin, compiler_cfg)
    elif name in {"microfocus", "mf", "cob"}:
        cmd = _build_mf_cmd(src_path, out_bin, compiler_cfg)
    else:
        raise ValueError(f"Unsupported compiler name: {name!r}")

    try:
        proc = subprocess.run(
            cmd,
            capture_output=True,
            text=True,
            timeout=_TIMEOUT_S,
        )
        return proc.returncode, proc.stdout, proc.stderr
    except subprocess.TimeoutExpired as e:
        return 124, e.stdout or "", (e.stderr or "") + f"\n[TIMEOUT after {_TIMEOUT_S}s]"

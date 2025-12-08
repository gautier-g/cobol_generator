# file: cobolsmartgen/utils/pipeline_tracer.py
from __future__ import annotations
import json
import os
import shutil
import time
from datetime import datetime
from pathlib import Path
from typing import Any, Dict, List, Optional

_TRACE_DIR: Optional[Path] = None
_CURRENT_STEP: Optional[str] = None
_STEP_START_TIME: float = 0

def init_trace(out_dir: str, run_id: Optional[str] = None):
    global _TRACE_DIR
    if run_id is None:
        run_id = datetime.now().strftime("%Y%m%d_%H%M%S")
    
    _TRACE_DIR = Path(out_dir) / "trace" / "pipeline_debug" / run_id
    _TRACE_DIR.mkdir(parents=True, exist_ok=True)
    
    # Sauvegarder la config d'exécution
    config = {
        "run_id": run_id,
        "timestamp": datetime.now().isoformat(),
        "environment": {
            "OPENAI_BASE_URL": os.getenv("OPENAI_BASE_URL"),
            "LLM_MODEL": os.getenv("LLM_MODEL"),
            "CSG_LLM_USE_COMPLETIONS": os.getenv("CSG_LLM_USE_COMPLETIONS"),
            "LLM_TEMPERATURE": os.getenv("LLM_TEMPERATURE"),
            "LLM_MAX_TOKENS": os.getenv("LLM_MAX_TOKENS"),
            "GROQ_TIMEOUT_S": os.getenv("GROQ_TIMEOUT_S"),
            "CSG_USE_LLM_HEADERS": os.getenv("CSG_USE_LLM_HEADERS"),
            "CSG_USE_LLM_PROCS": os.getenv("CSG_USE_LLM_PROCS"),
        }
    }
    
    with open(_TRACE_DIR / "00_run_config.json", "w", encoding="utf-8") as f:
        json.dump(config, f, indent=2, ensure_ascii=False)
    
    return _TRACE_DIR

def start_step(step_number: int, step_name: str):
    global _CURRENT_STEP, _STEP_START_TIME
    _CURRENT_STEP = f"step_{step_number:02d}_{step_name}"
    _STEP_START_TIME = time.time()
    
    step_dir = _TRACE_DIR / _CURRENT_STEP
    step_dir.mkdir(parents=True, exist_ok=True)
    
    return step_dir

def save_input(name: str, data: Any, step_dir: Optional[Path] = None):
    if step_dir is None:
        step_dir = _TRACE_DIR / _CURRENT_STEP
    
    filepath = step_dir / f"input_{name}"
    _save_data(filepath, data)

def save_output(name: str, data: Any, step_dir: Optional[Path] = None):
    if step_dir is None:
        step_dir = _TRACE_DIR / _CURRENT_STEP
    
    filepath = step_dir / f"output_{name}"
    _save_data(filepath, data)

def save_intermediate(name: str, data: Any, step_dir: Optional[Path] = None):
    if step_dir is None:
        step_dir = _TRACE_DIR / _CURRENT_STEP
    
    filepath = step_dir / name
    _save_data(filepath, data)

def end_step(success: bool = True, error: Optional[str] = None, extra_meta: Optional[Dict] = None):
    duration = time.time() - _STEP_START_TIME
    
    metadata = {
        "step": _CURRENT_STEP,
        "success": success,
        "duration_seconds": round(duration, 3),
        "timestamp": datetime.now().isoformat(),
    }
    
    if error:
        metadata["error"] = str(error)
    
    if extra_meta:
        metadata.update(extra_meta)
    
    step_dir = _TRACE_DIR / _CURRENT_STEP
    with open(step_dir / "metadata.json", "w", encoding="utf-8") as f:
        json.dump(metadata, f, indent=2, ensure_ascii=False)

def _save_data(filepath: Path, data: Any):
    if isinstance(data, (dict, list)):
        with open(f"{filepath}.json", "w", encoding="utf-8") as f:
            json.dump(data, f, indent=2, ensure_ascii=False)
    elif isinstance(data, str):
        ext = ".txt" if not str(filepath).endswith((".txt", ".cbl", ".yaml", ".json")) else ""
        with open(f"{filepath}{ext}", "w", encoding="utf-8") as f:
            f.write(data)
    elif isinstance(data, bytes):
        with open(f"{filepath}.bin", "wb") as f:
            f.write(data)
    elif hasattr(data, '__dict__'):
        with open(f"{filepath}.json", "w", encoding="utf-8") as f:
            json.dump(data.__dict__, f, indent=2, ensure_ascii=False, default=str)
    else:
        with open(f"{filepath}.txt", "w", encoding="utf-8") as f:
            f.write(str(data))

def save_llm_call(program_id: str, stage: str, prompt: str, response: str, metadata: Dict):
    llm_dir = _TRACE_DIR / _CURRENT_STEP / "llm_calls" / program_id / stage
    llm_dir.mkdir(parents=True, exist_ok=True)
    
    with open(llm_dir / "01_prompt.txt", "w", encoding="utf-8") as f:
        f.write(prompt)
    
    with open(llm_dir / "02_llm_request.json", "w", encoding="utf-8") as f:
        json.dump(metadata.get("request", {}), f, indent=2, ensure_ascii=False)
    
    with open(llm_dir / "03_llm_response_raw.txt", "w", encoding="utf-8") as f:
        f.write(response)
    
    with open(llm_dir / "metadata.json", "w", encoding="utf-8") as f:
        json.dump(metadata, f, indent=2, ensure_ascii=False)

def get_trace_dir() -> Path:
    return _TRACE_DIR

def finalize_trace(success: bool = True, error: Optional[str] = None):
    summary = {
        "run_id": _TRACE_DIR.name,
        "timestamp_end": datetime.now().isoformat(),
        "success": success,
    }
    
    if error:
        summary["error"] = str(error)
    
    # Lister toutes les étapes
    steps = []
    for step_dir in sorted(_TRACE_DIR.glob("step_*")):
        meta_file = step_dir / "metadata.json"
        if meta_file.exists():
            with open(meta_file, "r", encoding="utf-8") as f:
                steps.append(json.load(f))
    
    summary["steps"] = steps
    
    with open(_TRACE_DIR / "summary.json", "w", encoding="utf-8") as f:
        json.dump(summary, f, indent=2, ensure_ascii=False)
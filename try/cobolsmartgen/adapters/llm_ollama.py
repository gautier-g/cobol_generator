from __future__ import annotations
import os, json, urllib.request
from typing import Optional, Dict, Any
from . import llm_auto

OLLAMA_HOST = os.getenv("OLLAMA_HOST", "http://localhost:11434")
DEFAULT_GROQ_MODEL = os.getenv("GROQ_MODEL", "llama-3.1-8b-instant")

def _ollama_up(timeout: float = 0.5) -> bool:
    try:
        with urllib.request.urlopen(OLLAMA_HOST + "/api/tags", timeout=timeout) as r:
            return getattr(r, "status", 200) == 200
    except Exception:
        return False

def _groq_cfg(config: Optional[Dict[str, Any]]) -> Dict[str, Any]:
    base = {"llm": {"provider": "groq", "model": DEFAULT_GROQ_MODEL}}
    if config and "llm" in config:
        base["llm"].update(config["llm"])
    return base

def _ollama_generate(prompt: str, timeout_s: int, model: str) -> str:
    data = {"model": model, "prompt": prompt, "stream": False}
    req = urllib.request.Request(
        OLLAMA_HOST + "/api/generate",
        data=json.dumps(data).encode("utf-8"),
        headers={"Content-Type": "application/json"},
    )
    with urllib.request.urlopen(req, timeout=timeout_s) as r:
        resp = json.loads(r.read().decode("utf-8"))
        return resp.get("response", "")

def generate(prompt: str, timeout_s: int = 60, config: Optional[Dict[str, Any]] = None) -> str:
    force_groq = (config or {}).get("llm", {}).get("provider") == "groq" or os.getenv("CSG_LLM_PROVIDER") == "groq"
    if force_groq or not _ollama_up():
        return llm_auto.generate(prompt, timeout_s=timeout_s, config=_groq_cfg(config))
    try:
        model = os.getenv("OLLAMA_MODEL", "llama3.1")
        return _ollama_generate(prompt, timeout_s, model)
    except Exception:
        # Dernier recours: bascule Groq
        return llm_auto.generate(prompt, timeout_s=timeout_s, config=_groq_cfg(config))

def chat(messages, timeout_s: int = 60, config: Optional[Dict[str, Any]] = None) -> str:
    prompt = "\n".join([(m.get("role", "user") + ": " + m.get("content", "")) for m in messages])
    return generate(prompt, timeout_s=timeout_s, config=config)

# === [CSG PATCH] kwargs/system wrapper ===
try:
    _csg_orig_generate = generate
    def generate(*args, **kwargs):
        # tolère system=..., et autres kwargs inconnus
        system = kwargs.pop('system', None)
        # Préfixe le message utilisateur si disponible
        if system is not None:
            if len(args) >= 1 and isinstance(args[0], str):
                args = (f"{system}\n\n{args[0]}",) + args[1:]
            elif 'prompt' in kwargs and isinstance(kwargs.get('prompt'), str):
                kwargs['prompt'] = f"{system}\n\n{kwargs['prompt']}"
        return _csg_orig_generate(*args, **kwargs)
except Exception:
    # Si generate n'existe pas encore, on ignore
    pass

# (Optionnel) même traitement pour chat() s'il existe
try:
    _csg_orig_chat = chat  # type: ignore[name-defined]
    def chat(*args, **kwargs):  # type: ignore[func-returns-value]
        system = kwargs.pop('system', None)
        if system is not None:
            if len(args) >= 1 and isinstance(args[0], str):
                args = (f"{system}\n\n{args[0]}",) + args[1:]
            elif 'prompt' in kwargs and isinstance(kwargs.get('prompt'), str):
                kwargs['prompt'] = f"{system}\n\n{kwargs['prompt']}"
        return _csg_orig_chat(*args, **kwargs)  # type: ignore[misc]
except Exception:
    pass
# === [/CSG PATCH] ===

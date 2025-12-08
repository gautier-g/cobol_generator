# file: cobolsmartgen/adapters/llm_auto.py
from __future__ import annotations
from typing import Any, Dict, List, Optional
import os

def _provider(cfg: Optional[Dict[str, Any]]) -> str:
    p = os.getenv("CSG_LLM_PROVIDER") or ((cfg or {}).get("llm", {}) or {}).get("provider") or "groq"
    return str(p).lower()

def generate(prompt: str, system: Optional[str]=None, timeout_s: int = 600, config: Optional[Dict[str, Any]] = None, **kwargs) -> str:
    prov = _provider(config)

    # Check for Mistral (handles both "mistral" and model IDs like "mistral-large-latest")
    if "mistral" in prov.lower():
        from cobolsmartgen.adapters import llm_mistral
        return llm_mistral.generate(prompt, system=system, timeout_s=timeout_s, config=config, **kwargs)
    elif "groq" in prov.lower():
        from cobolsmartgen.adapters import llm_groq
        return llm_groq.generate(prompt, system=system, timeout_s=timeout_s, config=config, **kwargs)
    else:
        raise ValueError(f"Unsupported LLM provider: {prov}")

def chat(messages: List[Dict[str, str]], timeout_s: int = 600, config: Optional[Dict[str, Any]] = None, **kwargs) -> str:
    prov = _provider(config)

    # Check for Mistral (handles both "mistral" and model IDs like "mistral-large-latest")
    if "mistral" in prov.lower():
        from cobolsmartgen.adapters import llm_mistral
        return llm_mistral.chat(messages, timeout_s=timeout_s, config=config, **kwargs)
    elif "groq" in prov.lower():
        from cobolsmartgen.adapters import llm_groq
        return llm_groq.chat(messages, timeout_s=timeout_s, config=config, **kwargs)
    else:
        raise ValueError(f"Unsupported LLM provider: {prov}")

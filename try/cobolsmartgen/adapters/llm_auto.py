# file: cobolsmartgen/adapters/llm_auto.py
# Simplified: Only Mistral API supported (other providers removed 2026-01-08)
from __future__ import annotations
from typing import Any, Dict, List, Optional
from cobolsmartgen.adapters import llm_mistral

def generate(prompt: str, system: Optional[str]=None, timeout_s: int = 600, config: Optional[Dict[str, Any]] = None, **kwargs) -> str:
    """Generate text using Mistral API."""
    return llm_mistral.generate(prompt, system=system, timeout_s=timeout_s, config=config, **kwargs)

def chat(messages: List[Dict[str, str]], timeout_s: int = 600, config: Optional[Dict[str, Any]] = None, **kwargs) -> str:
    """Chat using Mistral API."""
    return llm_mistral.chat(messages, timeout_s=timeout_s, config=config, **kwargs)

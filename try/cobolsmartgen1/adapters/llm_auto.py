# file: cobolsmartgen/adapters/llm_auto.py
# =============================================================================
# CONFIGURATION DU PROVIDER LLM
# =============================================================================
# Pour changer de provider, commenter/décommenter les imports et les fonctions
# ci-dessous. Un seul provider doit être actif à la fois.
# =============================================================================
from __future__ import annotations
from typing import Any, Dict, List, Optional

# =============================================================================
# PROVIDER ACTIF: Décommenter UNE SEULE des lignes d'import ci-dessous
# =============================================================================

# --- Option 1: Mistral API ---
# from cobolsmartgen1.adapters import llm_mistral as _llm_provider

# --- Option 2: Gemini API (Google) ---
from cobolsmartgen1.adapters import llm_gemini as _llm_provider

# =============================================================================
# FONCTIONS D'INTERFACE (ne pas modifier)
# =============================================================================

def generate(prompt: str, system: Optional[str]=None, timeout_s: int = 600, config: Optional[Dict[str, Any]] = None, **kwargs) -> str:
    """Generate text using the active LLM provider."""
    return _llm_provider.generate(prompt, system=system, timeout_s=timeout_s, config=config, **kwargs)

def chat(messages: List[Dict[str, str]], timeout_s: int = 600, config: Optional[Dict[str, Any]] = None, **kwargs) -> str:
    """Chat using the active LLM provider."""
    return _llm_provider.chat(messages, timeout_s=timeout_s, config=config, **kwargs)

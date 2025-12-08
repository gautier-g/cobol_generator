
# file: cobolsmartgen/adapters/llm_groq.py
from __future__ import annotations
import os
import json
import socket
import time
import random
import urllib.request
import urllib.error
from typing import Any, Dict, List, Optional
import logging
from cobolsmartgen.utils.trace import log_prompt
import time
LOG = logging.getLogger(__name__)

_DEFAULT_BASE = (os.getenv('OPENAI_BASE_URL') or os.getenv('GROQ_BASE_URL') or os.getenv('LLM_BASE_URL') or 'https://api.groq.com/openai/v1')
_DEFAULT_MODEL = "llama-3.1-8b-instant"

def _resolve(cfg: Optional[Dict[str, Any]]) -> tuple[str, str, float, int, int]:
    llm = (cfg or {}).get("llm", {}) or {}
    base = os.getenv("GROQ_BASE_URL") or os.getenv("OPENAI_BASE_URL") or llm.get("base_url") or _DEFAULT_BASE
    model = os.getenv("GROQ_MODEL") or os.getenv("LLM_MODEL") or llm.get("model") or _DEFAULT_MODEL
    temp = float(os.getenv("LLM_TEMPERATURE") or os.getenv("GROQ_TEMPERATURE") or llm.get("temperature") or 0.2)
    timeout = int(os.getenv("GROQ_TIMEOUT_S") or llm.get("timeout_s") or 600)
    max_tokens = int(os.getenv("LLM_MAX_TOKENS") or llm.get("max_tokens") or 2560)
    return base.rstrip("/"), model, temp, timeout, max_tokens
def _wrap_inst_if_needed(model: str, prompt: str, system: Optional[str]) -> str:
    m = (model or "").lower()
    if any(k in m for k in ("codellama", "llama")):
        sys_txt = system or "Tu es un générateur COBOL. Réponds uniquement avec du COBOL compilable (format free)."
        return f"<s>[INST] <<SYS>>\n{sys_txt}\n<</SYS>>\n{prompt}\n[/INST]"
    return prompt

def _stops_for(model: str) -> list:
    stops = ["```", "###", "\n\n---END-OF-COBOL---"]
    m = (model or "").lower()
    if any(k in m for k in ("codellama", "llama")):
        stops += ["</s>", "[INST]", "[/INST]", "<|im_end|>"]
    return stops

def _api_key() -> str:
    key = os.getenv("GROQ_API_KEY") or os.getenv("OPENAI_API_KEY") or "dummy-local"
    return key
# 
# def _post_json_with_retry(url: str, payload: Dict[str, Any], timeout_s: int, max_retries: int = 5) -> Dict[str, Any]:
#     data = json.dumps(payload).encode("utf-8")
#     
#     for attempt in range(max_retries):
#         try:
#             req = urllib.request.Request(
#                 url,
#                 data=data,
#                 headers={
#                     "Content-Type": "application/json",
#                     "Authorization": f"Bearer {_api_key()}",
#                 },
#                 method="POST",
#             )
#             
#             with urllib.request.urlopen(req, timeout=timeout_s) as resp:
#                 if resp.status != 200:
#                     body = resp.read(1024*16).decode("utf-8", errors="replace")
#                     raise RuntimeError(f"HTTP {resp.status} at {url}: {body}")
#                 text = resp.read().decode("utf-8", errors="replace")
#                 return json.loads(text)
#                 
#         except urllib.error.HTTPError as e:
#             body = e.read(1024*16).decode("utf-8", errors="replace") if e.fp else ""
#             
#             if e.code in (503, 429, 408, 502, 504):
#                 if attempt < max_retries - 1:
#                     backoff = (2 ** attempt) + random.uniform(0, 1)
#                     LOG.warning(f"HTTP {e.code} at {url}, retry {attempt + 1}/{max_retries} after {backoff:.1f}s")
#                     time.sleep(backoff)
#                     continue
#             
#             raise RuntimeError(f"HTTPError {e.code} at {url}: {body}") from e
#             
#         except urllib.error.URLError as e:
#             if attempt < max_retries - 1:
#                 backoff = (2 ** attempt) + random.uniform(0, 1)
#                 LOG.warning(f"URLError at {url}, retry {attempt + 1}/{max_retries} after {backoff:.1f}s: {e.reason}")
#                 time.sleep(backoff)
#                 continue
#             raise RuntimeError(f"Connection failed at {url}: {e.reason}") from e
#             
#         except socket.timeout as e:
#             if attempt < max_retries - 1:
#                 backoff = (2 ** attempt) + random.uniform(0, 1)
#                 LOG.warning(f"Timeout at {url}, retry {attempt + 1}/{max_retries} after {backoff:.1f}s")
#                 time.sleep(backoff)
#                 continue
#             raise RuntimeError(f"Request timed out after {timeout_s}s at {url}") from e
#     
#     raise RuntimeError(f"Max retries ({max_retries}) exceeded for {url}")

def _post_json_with_retry(url: str, payload: Dict[str, Any], timeout_s: int, max_retries: int = 5) -> Dict[str, Any]:
    data_bytes = json.dumps(payload).encode("utf-8")
    headers = {
        "Content-Type": "application/json",
        "Authorization": f"Bearer {_api_key()}",
    }
    
    for attempt in range(max_retries):
        try:
            req = urllib.request.Request(url, data=data_bytes, headers=headers)
            
            # Logs de progression
            start_time = time.time()
            last_log_time = start_time
            
            LOG.info(f"🚀 Sending request to {url} (timeout={timeout_s}s)")
            
            # Ouvrir la connexion
            with urllib.request.urlopen(req, timeout=timeout_s) as resp:
                # Lire par chunks pour afficher la progression
                chunks = []
                total_bytes = 0
                
                while True:
                    chunk = resp.read(1024)  # Lire par blocs de 1KB
                    if not chunk:
                        break
                    
                    chunks.append(chunk)
                    total_bytes += len(chunk)
                    
                    # Log toutes les 5 secondes
                    current_time = time.time()
                    if current_time - last_log_time >= 5.0:
                        elapsed = current_time - start_time
                        LOG.info(f"📊 Receiving data... {total_bytes} bytes in {elapsed:.1f}s")
                        last_log_time = current_time
                
                # Reconstruire la réponse
                body_bytes = b''.join(chunks)
                elapsed_total = time.time() - start_time
                LOG.info(f"✅ Received {total_bytes} bytes in {elapsed_total:.1f}s")
                
                if resp.status != 200:
                    body = body_bytes.decode("utf-8", errors="replace")
                    raise RuntimeError(f"HTTP {resp.status} at {url}: {body}")
                
                text = body_bytes.decode("utf-8", errors="replace")
                return json.loads(text)
                
        except urllib.error.HTTPError as e:
            body = e.read(1024*16).decode("utf-8", errors="replace") if e.fp else ""
            
            if e.code in (503, 429, 408, 502, 504):
                # Retry logic continue normalement ici
                if attempt < max_retries - 1:
                    wait = min((2 ** attempt) + random.uniform(0, 1), 60)
                    LOG.warning(f"HTTP {e.code} at {url}, retrying in {wait:.1f}s... (attempt {attempt+1}/{max_retries})")
                    time.sleep(wait)
                    continue
                else:
                    raise RuntimeError(f"HTTP {e.code} at {url} after {max_retries} retries: {body}") from e
            else:
                raise RuntimeError(f"HTTP {e.code} at {url}: {body}") from e
                
        except (urllib.error.URLError, ConnectionError) as e:
            if attempt < max_retries - 1:
                wait = min((2 ** attempt) + random.uniform(0, 1), 60)
                LOG.warning(f"Connection error to {url}: {e}, retrying in {wait:.1f}s... (attempt {attempt+1}/{max_retries})")
                time.sleep(wait)
                continue
            else:
                raise RuntimeError(f"Connection failed to {url} after {max_retries} retries") from e
                
        except Exception as e:
            LOG.error(f"Unexpected error calling {url}: {e}")
            raise
    
    raise RuntimeError(f"Failed to get response from {url} after {max_retries} retries")

def call_completions_groq(prompt: str, config: Optional[Dict] = None, timeout_s: int = 0) -> str:
    """Generate COBOL code using mAInframer model via completions endpoint."""
    base, model, temp, cfg_to, max_tokens = _resolve(config)
    
    if timeout_s > 0:
        timeout_s = timeout_s
    elif cfg_to > 0:
        timeout_s = cfg_to
    
    url = f"{base}/completions"
    payload: Dict[str, Any] = {
        "model": model,
        "prompt": prompt,
        "temperature": temp,
        "max_tokens": max_tokens,
        "stream": False,
        "stream": False,
        "stop": ["```", "###", "\n\n---END-OF-COBOL---"]  # MODIFIÉ
    }
    
    prompt_log = prompt[:2000] + "…" if len(prompt) > 2000 else prompt
    
    # Log avant l'appel
    LOG.info(f"📤 Calling completions API with model={model}, temperature={temp}, max_tokens={max_tokens}")
    LOG.info(f"📤 Prompt preview: {prompt_log[:200]}...")
    
    log_prompt(
        out_dir=(config or {}).get("out_dir", "out"),
        kind="groq",
        name="completions",
        prompt=prompt_log,
        response=None,
        meta={"base": base, "model": model, "endpoint": "completions"}
    )
    
    # Appel avec logs de progression
    start_time = time.time()
    resp = _post_json_with_retry(url, payload, timeout_s)
    elapsed = time.time() - start_time
    
    LOG.info(f"📥 Response received in {elapsed:.1f}s")
    
    try:
        content = resp["choices"][0]["text"]
    except (KeyError, IndexError) as e:
        raise RuntimeError(f"Invalid completions response: {resp!r}") from e
    
    LOG.info(f"✅ Generated {len(content)} chars of COBOL code")
    
    log_prompt(
        out_dir=(config or {}).get("out_dir", "out"),
        kind="groq",
        name="completions",
        prompt=prompt_log,
        response=content[:2000] + "…" if len(content) > 2000 else content,
        meta={"base": base, "model": model, "endpoint": "completions", "response_length": len(content)}
    )
    
    return content

def completions(prompt: str, timeout_s: int = 600, config: Optional[Dict[str, Any]] = None) -> str:
    base, model, temp, cfg_to, max_tokens = _resolve(config)   

    if timeout_s is None:
        timeout_s = cfg_to
    
    url = f"{base}/completions"
    payload: Dict[str, Any] = {
        "model": model,
        "prompt": prompt,
        "temperature": temp,
        "max_tokens": max_tokens,
        "stream": False,
        "stop": ["```", "###", "\n\n---END-OF-COBOL---"]    }
    
    prompt_log = prompt[:2000] + "…" if len(prompt) > 2000 else prompt
    log_prompt(
        out_dir=(config or {}).get("out_dir", "out"),
        kind="groq",
        name="completions",
        prompt=prompt_log,
        response=None,
        meta={"base": base, "model": model, "endpoint": "completions"}
    )
    
    resp = _post_json_with_retry(url, payload, timeout_s)
    
    try:
        content = resp["choices"][0]["text"]
    except (KeyError, IndexError) as e:
        raise RuntimeError(f"Invalid completions response: {resp!r}") from e
    
    log_prompt(
        out_dir=(config or {}).get("out_dir", "out"),
        kind="groq",
        name="completions",
        prompt=prompt_log,
        response=content,
        meta={"base": base, "model": model, "endpoint": "completions"}
    )
    
    return str(content)

def chat(messages: List[Dict[str, str]], timeout_s: int = 600, config: Optional[Dict[str, Any]] = None) -> str:
    base, model, temp, cfg_to, max_tokens = _resolve(config)
    if timeout_s is None:
        timeout_s = cfg_to
    
    url = f"{base}/chat/completions"
    payload: Dict[str, Any] = {
        "model": model,
        "messages": messages,
        "temperature": temp,
        "max_tokens": max_tokens,
        "stream": False,
    }
    
    joined = []
    for m in messages:
        c = str(m.get("content",""))
        if len(c) > 2000: c = c[:2000] + "…"
        joined.append(f"[{m.get('role','user')}] {c}")
    prompt_log = "\n".join(joined)
    
    log_prompt(
        out_dir=(config or {}).get("out_dir", "out"),
        kind="groq",
        name="chat",
        prompt=prompt_log,
        response=None,
        meta={"base": base, "model": model, "endpoint": "chat"}
    )
    
    resp = _post_json_with_retry(url, payload, timeout_s)
    
    try:
        content = resp["choices"][0]["message"]["content"]
    except (KeyError, IndexError) as e:
        raise RuntimeError(f"Invalid chat response: {resp!r}") from e
    
    log_prompt(
        out_dir=(config or {}).get("out_dir", "out"),
        kind="groq",
        name="chat",
        prompt=prompt_log,
        response=content,
        meta={"base": base, "model": model, "endpoint": "chat"}
    )
    
    return str(content)

def generate(prompt: str, system: Optional[str]=None, timeout_s: int = 600, config: Optional[Dict[str, Any]] = None, **kwargs) -> str:
    use_completions = os.getenv("CSG_LLM_USE_COMPLETIONS", "1") == "1"
    # 
    # if use_completions:
    #     instruct_prompt = ""
    #     if system:
    #         instruct_prompt += f"{system}\n\n"
    #     instruct_prompt += f"{prompt}\n\nRépondez UNIQUEMENT avec du code COBOL compilable. Commencez par IDENTIFICATION DIVISION."
    #     return completions(instruct_prompt, timeout_s=timeout_s, config=config)
    if use_completions:
        # construit le prompt utilisateur
        base_prompt = f"{prompt}\n\nRépondez UNIQUEMENT avec du code COBOL compilable. Commencez par IDENTIFICATION DIVISION."
        # récupère le model effectif
        _, model, _, _, _ = _resolve(config)
        # enveloppe en [INST] si nécessaire
        actual = _wrap_inst_if_needed(model, base_prompt, system)
        return completions(actual, timeout_s=timeout_s, config=config)

    else:
        msgs = []
        if system:
            msgs.append({"role":"system","content":system})
        msgs.append({"role":"user","content":prompt})
        return chat(msgs, timeout_s=timeout_s, config=config)


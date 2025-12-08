# file: cobolsmartgen/adapters/llm_mistral.py
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

LOG = logging.getLogger(__name__)

# Configuration par défaut pour l'API Mistral
_DEFAULT_BASE = "https://api.mistral.ai/v1"
_DEFAULT_MODEL = "mistral-large-latest"

def _resolve(cfg: Optional[Dict[str, Any]]) -> tuple[str, str, float, int, int]:
    """Résout la configuration pour Mistral API."""
    llm = (cfg or {}).get("llm", {}) or {}

    # IMPORTANT: Toujours utiliser l'URL de Mistral API par défaut
    # Ne pas utiliser base_url du config s'il pointe vers localhost
    base = os.getenv("MISTRAL_BASE_URL") or _DEFAULT_BASE

    # Ignorer llm.get("base_url") s'il contient localhost/127.0.0.1 (config pour groq/ollama)
    config_base = llm.get("base_url", "")
    if config_base and "localhost" not in config_base and "127.0.0.1" not in config_base:
        base = config_base

    model = os.getenv("MISTRAL_MODEL") or llm.get("model") or _DEFAULT_MODEL
    temp = float(os.getenv("MISTRAL_TEMPERATURE") or llm.get("temperature") or 0.2)
    timeout = int(os.getenv("MISTRAL_TIMEOUT_S") or llm.get("timeout_s") or 600)
    max_tokens = int(os.getenv("MISTRAL_MAX_TOKENS") or llm.get("max_tokens") or 2560)
    return base.rstrip("/"), model, temp, timeout, max_tokens

def _api_key() -> str:
    """Récupère la clé API Mistral."""
    key = os.getenv("MISTRAL_API_KEY") or "SNCDXcbUtpbN4NZ5nwMo6U322cBsSIf3"
    return key

def _post_json_with_retry(url: str, payload: Dict[str, Any], timeout_s: int, max_retries: int = 5) -> Dict[str, Any]:
    """Effectue un POST JSON avec retry en cas d'erreur."""
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

            LOG.info(f"🚀 Sending request to Mistral API at {url} (timeout={timeout_s}s)")

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
                        LOG.info(f"📊 Receiving data from Mistral... {total_bytes} bytes in {elapsed:.1f}s")
                        last_log_time = current_time

                # Reconstruire la réponse
                body_bytes = b''.join(chunks)
                elapsed_total = time.time() - start_time
                LOG.info(f"✅ Received {total_bytes} bytes from Mistral in {elapsed_total:.1f}s")

                if resp.status != 200:
                    body = body_bytes.decode("utf-8", errors="replace")
                    raise RuntimeError(f"HTTP {resp.status} at {url}: {body}")

                text = body_bytes.decode("utf-8", errors="replace")
                return json.loads(text)

        except urllib.error.HTTPError as e:
            body = e.read(1024*16).decode("utf-8", errors="replace") if e.fp else ""

            if e.code in (503, 429, 408, 502, 504):
                # Retry logic pour les erreurs temporaires
                if attempt < max_retries - 1:
                    wait = min((2 ** attempt) + random.uniform(0, 1), 60)
                    LOG.warning(f"HTTP {e.code} from Mistral API, retrying in {wait:.1f}s... (attempt {attempt+1}/{max_retries})")
                    time.sleep(wait)
                    continue
                else:
                    raise RuntimeError(f"HTTP {e.code} from Mistral API after {max_retries} retries: {body}") from e
            else:
                raise RuntimeError(f"HTTP {e.code} from Mistral API: {body}") from e

        except (urllib.error.URLError, ConnectionError) as e:
            if attempt < max_retries - 1:
                wait = min((2 ** attempt) + random.uniform(0, 1), 60)
                LOG.warning(f"Connection error to Mistral API: {e}, retrying in {wait:.1f}s... (attempt {attempt+1}/{max_retries})")
                time.sleep(wait)
                continue
            else:
                raise RuntimeError(f"Connection failed to Mistral API after {max_retries} retries") from e

        except Exception as e:
            LOG.error(f"Unexpected error calling Mistral API at {url}: {e}")
            raise

    raise RuntimeError(f"Failed to get response from Mistral API after {max_retries} retries")

def chat(messages: List[Dict[str, str]], timeout_s: int = 600, config: Optional[Dict[str, Any]] = None) -> str:
    """Appelle l'API Mistral avec des messages de chat."""
    base, model, temp, cfg_to, max_tokens = _resolve(config)
    if timeout_s is None:
        timeout_s = cfg_to

    url = f"{base}/chat/completions"
    payload: Dict[str, Any] = {
        "model": model,
        "messages": messages,
        "temperature": temp,
        "max_tokens": max_tokens,
    }

    # Préparer le log du prompt
    joined = []
    for m in messages:
        c = str(m.get("content",""))
        if len(c) > 2000: c = c[:2000] + "…"
        joined.append(f"[{m.get('role','user')}] {c}")
    prompt_log = "\n".join(joined)

    log_prompt(
        out_dir=(config or {}).get("out_dir", "out"),
        kind="mistral",
        name="chat",
        prompt=prompt_log,
        response=None,
        meta={"base": base, "model": model, "endpoint": "chat"}
    )

    resp = _post_json_with_retry(url, payload, timeout_s)

    try:
        content = resp["choices"][0]["message"]["content"]
    except (KeyError, IndexError) as e:
        raise RuntimeError(f"Invalid Mistral chat response: {resp!r}") from e

    log_prompt(
        out_dir=(config or {}).get("out_dir", "out"),
        kind="mistral",
        name="chat",
        prompt=prompt_log,
        response=content,
        meta={"base": base, "model": model, "endpoint": "chat"}
    )

    return str(content)

def generate(prompt: str, system: Optional[str]=None, timeout_s: int = 600, config: Optional[Dict[str, Any]] = None, **kwargs) -> str:
    """Génère une réponse via l'API Mistral en utilisant le format chat."""
    msgs = []
    if system:
        msgs.append({"role": "system", "content": system})
    msgs.append({"role": "user", "content": prompt})
    return chat(msgs, timeout_s=timeout_s, config=config)

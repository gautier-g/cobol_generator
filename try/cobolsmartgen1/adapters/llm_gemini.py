# # file: cobolsmartgen/adapters/llm_gemini.py
# from __future__ import annotations
# 
# import os
# import json
# import time
# import random
# import urllib.request
# import urllib.error
# from typing import Any, Dict, List, Optional, Tuple
# import logging
# 
# from cobolsmartgen1.utils.trace import log_prompt
# 
# LOG = logging.getLogger(__name__)
# 
# # =========================
# # CONFIG GEMINI (v1beta)
# # =========================
# _DEFAULT_BASE = "https://generativelanguage.googleapis.com/v1beta"
# _DEFAULT_MODEL = "gemini-3-pro-preview"
# 
# # Vous demandez 10k tokens max en sortie
# _DEFAULT_MAX_OUTPUT_TOKENS = 12_000
# 
# # Hyperparamètres orientés code
# _DEFAULT_TEMPERATURE = 0.15
# _DEFAULT_TOP_P = 0.90
# _DEFAULT_TOP_K = 40
# 
# # Gemini 3 Pro: thinking piloté par thinkingLevel (low|high)
# # Vous voulez "le reste en thinking" => on met HIGH par défaut.
# _DEFAULT_THINKING_LEVEL_PRO = "high"
# 
# # Reproductibilité (optionnel)
# _DEFAULT_SEED = 42
# 
# # NOTE sécurité: vous avez demandé de "mettre la key en dur".
# # Je ne recopie PAS votre clé réelle dans ce message (sinon elle re-circule encore).
# # Remplacez la valeur ci-dessous par votre clé SI vous insistez vraiment.
# # Recommandation: garder une clé en variable d'environnement est nettement plus sûr.
# _HARDCODED_GEMINI_API_KEY = "AIzaSyBMTCSd_oYymrb7rokKg5lJctghP8JwMJE"
# 
# 
# def _resolve(cfg: Optional[Dict[str, Any]]) -> Tuple[str, str, float, int, int]:
#     """Résout la configuration minimale pour Gemini API."""
#     llm = (cfg or {}).get("llm", {}) or {}
# 
#     base = os.getenv("GEMINI_BASE_URL") or llm.get("base_url") or _DEFAULT_BASE
#     if isinstance(base, str) and ("localhost" in base or "127.0.0.1" in base):
#         base = _DEFAULT_BASE
# 
#     model = os.getenv("GEMINI_MODEL") or llm.get("gemini_model") or _DEFAULT_MODEL
# 
#     temp = float(os.getenv("GEMINI_TEMPERATURE") or llm.get("temperature") or _DEFAULT_TEMPERATURE)
#     timeout = int(os.getenv("GEMINI_TIMEOUT_S") or llm.get("timeout_s") or 2400)
# 
#     # Sortie: 10k tokens max par défaut (overridable)
#     max_tokens = int(
#         os.getenv("GEMINI_MAX_OUTPUT_TOKENS")
#         or os.getenv("GEMINI_MAX_TOKENS")  # compat
#         or llm.get("max_output_tokens")
#         or llm.get("max_tokens")
#         or _DEFAULT_MAX_OUTPUT_TOKENS
#     )
# 
#     # Garde-fous: éviter votre cas "thinking uniquement / réponse vide"
#     if max_tokens <= 0:
#         max_tokens = _DEFAULT_MAX_OUTPUT_TOKENS
#     if max_tokens > 12_000:
#         # Vous avez demandé une limite max à 10k
#         max_tokens = 12_000
# 
#     return str(base).rstrip("/"), str(model), float(temp), int(timeout), int(max_tokens)
# 
# 
# def _api_key() -> str:
#     """Récupère la clé API Gemini (hardcodée, comme demandé)."""
#     key = (_HARDCODED_GEMINI_API_KEY or "").strip()
#     if not key or key == "PASTE_YOUR_GEMINI_API_KEY_HERE":
#         raise RuntimeError(
#             "API key manquante. Remplacez _HARDCODED_GEMINI_API_KEY par votre clé Gemini."
#         )
#     return key
# 
# 
# def _convert_messages_to_gemini_format(
#     messages: List[Dict[str, str]]
# ) -> Tuple[Optional[str], List[Dict[str, Any]]]:
#     """
#     Convertit les messages au format OpenAI/Mistral vers le format Gemini.
#     Retourne (system_instruction, contents).
#     """
#     system_instruction = None
#     contents: List[Dict[str, Any]] = []
# 
#     for msg in messages:
#         role = msg.get("role", "user")
#         content = msg.get("content", "")
# 
#         if role == "system":
#             system_instruction = content
#         elif role == "assistant":
#             contents.append({"role": "model", "parts": [{"text": content}]})
#         else:  # user
#             contents.append({"role": "user", "parts": [{"text": content}]})
# 
#     return system_instruction, contents
# 
# 
# def _post_json_with_retry(
#     url: str,
#     payload: Dict[str, Any],
#     timeout_s: int,
#     max_retries: int = 5,
# ) -> Dict[str, Any]:
#     """Effectue un POST JSON avec retry en cas d'erreur transitoire."""
#     data_bytes = json.dumps(payload).encode("utf-8")
#     headers = {
#         "Content-Type": "application/json",
#         # Auth via header (évite d’exposer la clé dans l’URL/logs)
#         "x-goog-api-key": _api_key(),
#     }
# 
#     for attempt in range(max_retries):
#         try:
#             req = urllib.request.Request(url, data=data_bytes, headers=headers, method="POST")
# 
#             start_time = time.time()
#             last_log_time = start_time
#             LOG.info(f"🚀 Sending request to Gemini API at {url} (timeout={timeout_s}s)")
# 
#             with urllib.request.urlopen(req, timeout=timeout_s) as resp:
#                 chunks: List[bytes] = []
#                 total_bytes = 0
# 
#                 while True:
#                     chunk = resp.read(1024)
#                     if not chunk:
#                         break
#                     chunks.append(chunk)
#                     total_bytes += len(chunk)
# 
#                     now = time.time()
#                     if now - last_log_time >= 5.0:
#                         elapsed = now - start_time
#                         LOG.info(f"📊 Receiving data from Gemini... {total_bytes} bytes in {elapsed:.1f}s")
#                         last_log_time = now
# 
#                 body_bytes = b"".join(chunks)
#                 elapsed_total = time.time() - start_time
#                 LOG.info(f"✅ Received {total_bytes} bytes from Gemini in {elapsed_total:.1f}s")
# 
#                 if resp.status != 200:
#                     body = body_bytes.decode("utf-8", errors="replace")
#                     raise RuntimeError(f"HTTP {resp.status} at {url}: {body}")
# 
#                 text = body_bytes.decode("utf-8", errors="replace")
#                 return json.loads(text)
# 
#         except urllib.error.HTTPError as e:
#             body = e.read(1024 * 64).decode("utf-8", errors="replace") if e.fp else ""
# 
#             if e.code in (503, 429, 408, 502, 504):
#                 if attempt < max_retries - 1:
#                     wait = min((2 ** attempt) + random.uniform(0, 1), 60)
#                     LOG.warning(
#                         f"HTTP {e.code} from Gemini API, retrying in {wait:.1f}s... "
#                         f"(attempt {attempt + 1}/{max_retries})"
#                     )
#                     time.sleep(wait)
#                     continue
#                 raise RuntimeError(f"HTTP {e.code} from Gemini API after {max_retries} retries: {body}") from e
# 
#             raise RuntimeError(f"HTTP {e.code} from Gemini API: {body}") from e
# 
#         except (urllib.error.URLError, ConnectionError) as e:
#             if attempt < max_retries - 1:
#                 wait = min((2 ** attempt) + random.uniform(0, 1), 60)
#                 LOG.warning(
#                     f"Connection error to Gemini API: {e}, retrying in {wait:.1f}s... "
#                     f"(attempt {attempt + 1}/{max_retries})"
#                 )
#                 time.sleep(wait)
#                 continue
#             raise RuntimeError(f"Connection failed to Gemini API after {max_retries} retries") from e
# 
#         except Exception as e:
#             LOG.error(f"Unexpected error calling Gemini API at {url}: {e}")
#             raise
# 
#     raise RuntimeError(f"Failed to get response from Gemini API after {max_retries} retries")
# 
# 
# def _build_generation_config(model: str, temp: float, max_tokens: int, cfg: Optional[Dict[str, Any]]) -> Dict[str, Any]:
#     llm = (cfg or {}).get("llm", {}) or {}
# 
#     top_p = float(os.getenv("GEMINI_TOP_P") or llm.get("top_p") or _DEFAULT_TOP_P)
#     top_k = int(os.getenv("GEMINI_TOP_K") or llm.get("top_k") or _DEFAULT_TOP_K)
#     seed = int(os.getenv("GEMINI_SEED") or llm.get("seed") or _DEFAULT_SEED)
# 
#     gen: Dict[str, Any] = {
#         "temperature": float(temp),
#         "maxOutputTokens": int(min(max_tokens, 10_000)),  # limite demandée
#         "topP": float(top_p),
#         "topK": int(top_k),
#         "candidateCount": 1,
#         "seed": seed,
#         # IMPORTANT: pas de stopSequences (vous l'avez demandé)
#     }
# 
#     # Thinking: pour Gemini 3 Pro, on utilise thinkingLevel.
#     # "rajoute le reste en thinking" => niveau HIGH.
#     if model.startswith("gemini-3-"):
#         thinking_level = (
#             os.getenv("GEMINI_THINKING_LEVEL")
#             or llm.get("thinking_level")
#             or _DEFAULT_THINKING_LEVEL_PRO
#         ).strip().lower()
# 
#         # Gemini 3 Pro: low|high uniquement.
#         if model.startswith("gemini-3-pro") and thinking_level not in ("low", "high"):
#             thinking_level = _DEFAULT_THINKING_LEVEL_PRO
# 
#         gen["thinkingConfig"] = {"thinkingLevel": thinking_level}
#     else:
#         # Fallback si jamais vous changez de modèle ailleurs
#         thinking_budget = int(os.getenv("GEMINI_THINKING_BUDGET") or llm.get("thinking_budget") or 8192)
#         gen["thinkingConfig"] = {"thinkingBudget": thinking_budget}
# 
#     return gen
# 
# 
# def _extract_text(resp: Dict[str, Any]) -> str:
#     """
#     Extraction robuste du texte.
#     Gère le cas candidates[0].content = {} (zéro tokens de sortie visible).
#     """
#     cands = resp.get("candidates") or []
#     if not cands:
#         raise RuntimeError(f"Invalid Gemini response: missing candidates: {resp!r}")
# 
#     content = cands[0].get("content") or {}
#     parts = content.get("parts") or []
#     text = "".join((p.get("text") or "") for p in parts).strip()
#     if text:
#         return text
# 
#     usage = resp.get("usageMetadata") or {}
#     prompt = int(usage.get("promptTokenCount") or 0)
#     thoughts = int(usage.get("thoughtsTokenCount") or 0)
#     total = int(usage.get("totalTokenCount") or 0)
# 
#     if total and (prompt + thoughts == total):
#         raise RuntimeError(
#             "Gemini returned an empty candidate (0 output tokens). "
#             "Vous êtes limité à 10k tokens de sortie; si ça arrive, baissez le thinkingLevel "
#             "(GEMINI_THINKING_LEVEL=low) ou simplifiez le prompt. "
#             f"usageMetadata={usage!r}"
#         )
# 
#     raise RuntimeError(f"Gemini returned empty content.parts; response={resp!r}")
# 
# 
# def chat(messages: List[Dict[str, str]], timeout_s: int = 1800, config: Optional[Dict[str, Any]] = None) -> str:
#     """Appelle l'API Gemini avec des messages de chat."""
#     base, model, temp, cfg_to, max_tokens = _resolve(config)
#     if timeout_s is None:
#         timeout_s = cfg_to
# 
#     # URL SANS ?key=...
#     url = f"{base}/models/{model}:generateContent"
# 
#     system_instruction, contents = _convert_messages_to_gemini_format(messages)
# 
#     generation_config = _build_generation_config(model=model, temp=temp, max_tokens=max_tokens, cfg=config)
# 
#     payload: Dict[str, Any] = {
#         "contents": contents,
#         "generationConfig": generation_config,
#     }
# 
#     if system_instruction:
#         payload["systemInstruction"] = {"parts": [{"text": system_instruction}]}
# 
#     # Log du prompt (tronqué)
#     joined = []
#     for m in messages:
#         c = str(m.get("content", ""))
#         if len(c) > 2000:
#             c = c[:2000] + "…"
#         joined.append(f"[{m.get('role', 'user')}] {c}")
#     prompt_log = "\n".join(joined)
# 
#     log_prompt(
#         out_dir=(config or {}).get("out_dir", "out"),
#         kind="gemini",
#         name="chat",
#         prompt=prompt_log,
#         response=None,
#         meta={"base": base, "model": model, "endpoint": "generateContent"},
#     )
# 
#     resp = _post_json_with_retry(url, payload, timeout_s)
# 
#     try:
#         content = _extract_text(resp)
#     except RuntimeError as first_err:
#         # Fallback: si vous obtenez "thinking sans sortie", retenter 1 fois en LOW.
#         # Ça conserve le modèle "thinking" mais réduit la probabilité de sortie vide/latence énorme.
#         if model.startswith("gemini-3-pro"):
#             try:
#                 gen2 = dict(generation_config)
#                 gen2["thinkingConfig"] = {"thinkingLevel": "low"}
#                 payload2 = dict(payload)
#                 payload2["generationConfig"] = gen2
#                 LOG.warning("Empty output received; retrying once with thinkingLevel=low for gemini-3-pro...")
#                 resp2 = _post_json_with_retry(url, payload2, timeout_s)
#                 content = _extract_text(resp2)
#                 resp = resp2
#             except Exception:
#                 raise first_err
#         else:
#             raise
# 
#     log_prompt(
#         out_dir=(config or {}).get("out_dir", "out"),
#         kind="gemini",
#         name="chat",
#         prompt=prompt_log,
#         response=content,
#         meta={"base": base, "model": model, "endpoint": "generateContent", "usageMetadata": resp.get("usageMetadata")},
#     )
# 
#     return str(content)
# 
# 
# def generate(
#     prompt: str,
#     system: Optional[str] = None,
#     timeout_s: int = 1800,
#     config: Optional[Dict[str, Any]] = None,
#     **kwargs: Any,
# ) -> str:
#     """Génère une réponse via l'API Gemini en utilisant le format chat."""
#     msgs: List[Dict[str, str]] = []
#     if system:
#         msgs.append({"role": "system", "content": system})
#     msgs.append({"role": "user", "content": prompt})
#     return chat(msgs, timeout_s=timeout_s, config=config)

# file: cobolsmartgen/adapters/llm_gemini.py
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
from cobolsmartgen1.utils.trace import log_prompt

LOG = logging.getLogger(__name__)

# Configuration par défaut pour l'API Gemini
_DEFAULT_BASE = "https://generativelanguage.googleapis.com/v1beta"
# _DEFAULT_MODEL = "gemini-3-flash-preview"  # Le plus gros et récent modèle Gemini
_DEFAULT_MODEL = "gemini-2.5-pro"
def _resolve(cfg: Optional[Dict[str, Any]]) -> tuple[str, str, float, int, int]:
    """Résout la configuration pour Gemini API."""
    llm = (cfg or {}).get("llm", {}) or {}

    # URL de base pour Gemini API
    base = os.getenv("GEMINI_BASE_URL") or _DEFAULT_BASE

    # Ignorer les URLs localhost
    config_base = llm.get("base_url", "")
    if config_base and "localhost" not in config_base and "127.0.0.1" not in config_base:
        if "googleapis.com" in config_base:
            base = config_base

    model = os.getenv("GEMINI_MODEL") or llm.get("gemini_model") or _DEFAULT_MODEL
    temp = float(os.getenv("GEMINI_TEMPERATURE") or llm.get("temperature") or 0.2)
    timeout = int(os.getenv("GEMINI_TIMEOUT_S") or llm.get("timeout_s") or 2400)
    # max_tokens = int(os.getenv("GEMINI_MAX_TOKENS") or llm.get("max_tokens") or 118000)
    # Dans _resolve, changez la valeur par défaut ou mettez à jour votre .env
    max_tokens = int(os.getenv("GEMINI_MAX_TOKENS") or llm.get("max_tokens") or 16889    )#65536)
    return base.rstrip("/"), model, temp, timeout, max_tokens
def _api_key() -> str:
    """Récupère la clé API Gemini."""
    key = os.getenv("GEMINI_API_KEY") or "AIzaSyBMTCSd_oYymrb7rokKg5lJctghP8JwMJE"
    return key

def _convert_messages_to_gemini_format(messages: List[Dict[str, str]]) -> tuple[Optional[str], List[Dict[str, Any]]]:
    """
    Convertit les messages au format OpenAI/Mistral vers le format Gemini.
    Retourne (system_instruction, contents).
    """
    system_instruction = None
    contents = []

    for msg in messages:
        role = msg.get("role", "user")
        content = msg.get("content", "")

        if role == "system":
            # Gemini utilise systemInstruction séparément
            system_instruction = content
        elif role == "assistant":
            contents.append({
                "role": "model",
                "parts": [{"text": content}]
            })
        else:  # user
            contents.append({
                "role": "user",
                "parts": [{"text": content}]
            })

    return system_instruction, contents

def _post_json_with_retry(url: str, payload: Dict[str, Any], timeout_s: int, max_retries: int = 5) -> Dict[str, Any]:
    """Effectue un POST JSON avec retry en cas d'erreur."""
    data_bytes = json.dumps(payload).encode("utf-8")
    headers = {
        "Content-Type": "application/json",
    }

    for attempt in range(max_retries):
        try:
            req = urllib.request.Request(url, data=data_bytes, headers=headers)

            # Logs de progression
            start_time = time.time()
            last_log_time = start_time

            LOG.info(f"🚀 Sending request to Gemini API at {url} (timeout={timeout_s}s)")

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
                        LOG.info(f" Receiving data from Gemini... {total_bytes} bytes in {elapsed:.1f}s")
                        last_log_time = current_time

                # Reconstruire la réponse
                body_bytes = b''.join(chunks)
                elapsed_total = time.time() - start_time
                LOG.info(f" Received {total_bytes} bytes from Gemini in {elapsed_total:.1f}s")

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
                    LOG.warning(f"HTTP {e.code} from Gemini API, retrying in {wait:.1f}s... (attempt {attempt+1}/{max_retries})")
                    time.sleep(wait)
                    continue
                else:
                    raise RuntimeError(f"HTTP {e.code} from Gemini API after {max_retries} retries: {body}") from e
            else:
                raise RuntimeError(f"HTTP {e.code} from Gemini API: {body}") from e

        except (urllib.error.URLError, ConnectionError) as e:
            if attempt < max_retries - 1:
                wait = min((2 ** attempt) + random.uniform(0, 1), 60)
                LOG.warning(f"Connection error to Gemini API: {e}, retrying in {wait:.1f}s... (attempt {attempt+1}/{max_retries})")
                time.sleep(wait)
                continue
            else:
                raise RuntimeError(f"Connection failed to Gemini API after {max_retries} retries") from e

        except Exception as e:
            LOG.error(f"Unexpected error calling Gemini API at {url}: {e}")
            raise

    raise RuntimeError(f"Failed to get response from Gemini API after {max_retries} retries")

def chat(messages: List[Dict[str, str]], timeout_s: int = 1800, config: Optional[Dict[str, Any]] = None) -> str:
    """Appelle l'API Gemini avec des messages de chat."""
    base, model, temp, cfg_to, max_tokens = _resolve(config)
    if timeout_s is None:
        timeout_s = cfg_to

    # URL format Gemini: /models/{model}:generateContent?key={API_KEY}
    url = f"{base}/models/{model}:generateContent?key={_api_key()}"

    # Convertir les messages au format Gemini
    system_instruction, contents = _convert_messages_to_gemini_format(messages)

    # Budget thinking pour les modèles "thinking" (gemini-3-*)
    thinking_budget = int(os.getenv("GEMINI_THINKING_BUDGET") or 65535)  # Max: 65535

    # Construire le payload Gemini
    payload: Dict[str, Any] = {
        "contents": contents,
        "generationConfig": {
            "temperature": temp,
            "maxOutputTokens": max_tokens,
            "topP": 0.95,
            "topK": 40,
            "thinkingConfig": {
                "thinkingBudget": thinking_budget
            }
        }
    }

    # Ajouter system instruction si présent
    if system_instruction:
        payload["systemInstruction"] = {
            "parts": [{"text": system_instruction}]
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
        kind="gemini",
        name="chat",
        prompt=prompt_log,
        response=None,
        meta={"base": base, "model": model, "endpoint": "generateContent"}
    )

    resp = _post_json_with_retry(url, payload, timeout_s)

    try:
        # Format de réponse Gemini: candidates[0].content.parts[0].text
        content = resp["candidates"][0]["content"]["parts"][0]["text"]
    except (KeyError, IndexError) as e:
        raise RuntimeError(f"Invalid Gemini chat response: {resp!r}") from e

    log_prompt(
        out_dir=(config or {}).get("out_dir", "out"),
        kind="gemini",
        name="chat",
        prompt=prompt_log,
        response=content,
        meta={"base": base, "model": model, "endpoint": "generateContent"}
    )

    return str(content)

def generate(prompt: str, system: Optional[str]=None, timeout_s: int = 1800, config: Optional[Dict[str, Any]] = None, **kwargs) -> str:
    """Génère une réponse via l'API Gemini en utilisant le format chat."""
    msgs = []
    if system:
        msgs.append({"role": "system", "content": system})
    msgs.append({"role": "user", "content": prompt})
    return chat(msgs, timeout_s=timeout_s, config=config)

# adapters

Adapters for LLM providers and COBOL compilation.

## Files

### llm_auto.py
Purpose: routes LLM calls to the active provider.
Inputs: prompt or messages, optional system, timeout_s, config["llm"].
Outputs: response string from the provider.
Config/env: CSG_LLM_PROVIDER or config.llm.provider ("mistral" or "groq").
Exchanges: imports and calls llm_mistral or llm_groq.

### llm_mistral.py
Purpose: HTTP client for Mistral chat/completions.
Inputs: prompt/messages, config["llm"].
Outputs: response text.
Config/env: MISTRAL_API_KEY, MISTRAL_MODEL, MISTRAL_BASE_URL, MISTRAL_TIMEOUT_S, MISTRAL_TEMPERATURE, MISTRAL_MAX_TOKENS.
Exchanges: logs prompt/response via utils.trace.log_prompt into out/trace/prompts/mistral.

### llm_groq.py
Purpose: HTTP client for Groq OpenAI style API (chat or completions).
Inputs: prompt/messages, config["llm"].
Outputs: response text.
Config/env: GROQ_API_KEY, GROQ_MODEL, GROQ_BASE_URL, OPENAI_BASE_URL, LLM_MODEL, LLM_TEMPERATURE, GROQ_TIMEOUT_S, LLM_MAX_TOKENS, CSG_LLM_USE_COMPLETIONS.
Exchanges: logs prompt/response via utils.trace.log_prompt into out/trace/prompts/groq.

### llm_ollama.py
Purpose: local Ollama client with fallback to Groq.
Inputs: prompt/messages, optional system wrapper.
Outputs: response text.
Config/env: OLLAMA_HOST, OLLAMA_MODEL.
Exchanges: when Ollama is down or provider is forced to groq, it calls llm_auto.generate with a Groq config.

### cobol_compiler.py
Purpose: compile a single COBOL file.
Inputs: src path, out_bin_dir, config["compiler"] (name, line_format, flags, copy_dirs, extra_args).
Outputs: (rc, stdout, stderr) and a binary in out/bin/<PROGRAM-ID>.
Exchanges: uses subprocess to call cobc/cob2/cob.

### MISTRAL_USAGE.md
Purpose: human setup notes for Mistral.

### __init__.py
Purpose: package marker (no runtime logic).

## Related docs
README.md: pipeline overview and CLI usage.

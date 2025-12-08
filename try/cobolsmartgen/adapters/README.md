# adapters

Adaptateurs vers les LLM et le compilateur COBOL.

Fichiers cles :
- `llm_auto.py` : route vers le provider actif (Mistral/Groq/Ollama) selon `CSG_LLM_PROVIDER` et expose `generate/chat`.
- `llm_mistral.py` : client Mistral (utilise `MISTRAL_API_KEY`, `MISTRAL_MODEL`, `MISTRAL_BASE_URL`, timeouts).
- `llm_groq.py` : client Groq (LLM rapide pour prototypage).
- `llm_ollama.py` : client Ollama local/offline.
- `cobol_compiler.py` : helper pour compiler ou tester le COBOL genere quand la phase compile est active.
- `MISTRAL_USAGE.md` : instructions detaillees pour Mistral.

Point d'entree : importer `from cobolsmartgen.adapters import llm_auto` puis appeler `generate()` ou `chat()` avec une config.

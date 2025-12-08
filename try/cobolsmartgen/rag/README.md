# rag

Indexation et recuperation de contexte pour alimenter les prompts LLM.

Fichiers cles :
- `indexer.py` : prepare les chunks de contexte issus des specs/plans pour l'index RAG.
- `retriever.py` : fournit des snippets pertinents pour enrichir les prompts de generation.

Utilise par `cli/main.py` avant les phases de generation COBOL/SQL.

# CobolSmartGen

Lancer la generation en mode Mistral (depuis ce dossier) :

```bash
CSG_LLM_PROVIDER=mistral \
MISTRAL_API_KEY=<votre_cle_mistral> \
MISTRAL_MODEL=mistral-large-latest \
CSG_USE_LLM_HEADERS=0 CSG_LLM_PROGRAM_MODE=1 CSG_USE_LLM_PROCS=1 CSG_STRICT_825=1 \
python3 -m cobolsmartgen.cli.main generate \
  --input salaire_net.yaml \
  --out out \
  --dialecte gnucobol \
  --sgbd postgres \
  --single-pass \
  --auto-llm
```

Variables clefs :
- `MISTRAL_API_KEY` / `MISTRAL_MODEL` : credentials et modele utilises par l'adaptateur Mistral.
- `CSG_USE_LLM_HEADERS` / `CSG_USE_LLM_PROCS` : active les appels LLM pour generer headers et procedures COBOL.
- `CSG_LLM_PROGRAM_MODE=1` : force le mode generation de programmes.
- `CSG_STRICT_825=1` : active les garde-fous COBOL 8.2.5.
- `CSG_RUN_ID` (optionnel) : etiquette pour tracer le run dans `out/trace/`.

Pipeline (mode `generate` / `--single-pass`) :
1) `ingest` : validation et normalisation de la spec YAML.
2) `analyze` : extraction des IO et planification des programmes.
3) `rag` : indexation/retrieval des prompts de contexte.
4) `generate` : generation SQL, headers et procedures via LLM.
5) `generate/assemble_layout` : assemblage final des fichiers COBOL.

Structure du dossier : chaque sous-dossier contient un README dedie.
- `adapters/` : clients LLM et compilateur.
- `analyze/` : extraction IO et planification.
- `cli/` : point d'entree CLI et orchestration.
- `core/` : helpers de contrat et structures internes.
- `generate/` : code generation COBOL/SQL et templates.
- `ingest/` : parsing/validation des specs.
- `prompts/` : templates texte pour les appels LLM.
- `rag/` : indexation et recuperation de contexte.
- `config/` : configuration par defaut (YAML).
- `schemas/` : schemas JSON des artefacts intermediaires.
- `utils/` : utilitaires communs (fs, naming, traces).

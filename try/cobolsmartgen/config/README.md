# config

Configuration par defaut du pipeline.

Fichiers :
- `project.yaml` : valeurs par defaut (dialecte COBOL, SGBD, logging, options LLM). Peut etre surcharge via variables d'environnement ou CLI.
- `mapping_types.yaml` : correspondances de types pour generer les colonnes SQL/COBOL.

Chargement : `cli/main.py` lit `project.yaml`, applique les variables `COBOL_*`, `LLM_*`, `DB_*`, puis les arguments CLI.

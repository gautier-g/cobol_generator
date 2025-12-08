# ingest

Preparation des specifications en entree avant l'analyse.

Fichiers cles :
- `validate_and_normalize.py` : charge le YAML utilisateur, applique les validations/schema et produit `normalized_spec.json`.
- `diagram_parser.py` : parse et nettoie les schemas de sequence/diagrammes si fournis.

Premier maillon du pipeline, appele au debut de `cli/main.py`.

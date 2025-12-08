# cli

Point d'entree CLI du pipeline CobolSmartGen.

Fichier cle :
- `main.py` : parse les arguments, charge la config, orchestre les etapes (ingest -> analyze -> rag -> generate) et gere les traces.

Usage direct : `python3 -m cobolsmartgen.cli.main generate --input <spec.yaml> --out <out_dir> --dialecte gnucobol --sgbd postgres --single-pass --auto-llm` (variables `CSG_*` pour parametrer le LLM).

# analyze

Modules d'analyse de la specification normalisee pour preparer la generation.

Fichiers cles :
- `extract_io.py` : construit la cartographie des entrees/sorties (io_map.json) a partir de la spec normalisee.
- `plan_programs.py` : planifie les programmes COBOL a generer (program_plan.json) selon les entites et flux IO.

Ces modules sont appeles depuis `cli/main.py` juste apres la phase d'ingestion.

# utils

Utilitaires communs reutilises dans tout le pipeline.

Fichiers cles :
- `fs.py` : lecture/ecriture JSON-YAML, gestion chemins et dossiers temporaires.
- `naming.py` : conventions de nommage COBOL/SQL.
- `pipeline_tracer.py` : creation des dossiers de trace et enregistrement des inputs/outputs par etape.
- `trace.py` : helpers de logging/formatage.
- `typing_map.py` : correspondances de types pour les mappings internes.

Importer via `from cobolsmartgen.utils import fs, naming, pipeline_tracer, trace` selon le besoin.

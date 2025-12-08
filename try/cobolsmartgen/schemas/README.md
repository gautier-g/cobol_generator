# schemas

Schemas JSON des artefacts intermediaires produits par le pipeline.

Fichiers :
- `input_spec.schema.json` / `normalized_spec.schema.json` : structure attendue pour les specs initiales et normalisees.
- `io_map.schema.json` : schema de la cartographie IO.
- `program_plan.schema.json` : schema du plan de programmes.
- `compile_report.schema.json`, `diagnosis.schema.json`, `patch_plan.schema.json` : schemas utilises lors des phases de compile/diagnostic.

Les validations s'appuient sur ces schemas pour garantir la coherence des fichiers generes.

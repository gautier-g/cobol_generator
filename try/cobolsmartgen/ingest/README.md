# ingest

Input specification validation and normalization.

## Files

### validate_and_normalize.py
Purpose: load a YAML/JSON spec, validate it, and normalize naming and types.
Inputs: input spec file, schemas/input_spec.schema.json, config defaults.
Outputs: out/normalized_spec.json plus .meta/.sha256 sidecars.
Exchanges: uses utils.fs/trace/typing_map; calls diagram_parser when spec has a diagramme field.

### diagram_parser.py
Purpose: parse diagram strings (Mermaid ERD, PlantUML, or UML JSON) into entities/relations.
Inputs: spec["diagramme"] string.
Outputs: a dict with entities and relations merged into the normalized spec.
Exchanges: used by validate_and_normalize.

### __init__.py
Purpose: package marker (no runtime logic).

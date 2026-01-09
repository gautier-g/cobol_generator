# config

Default configuration files used by the pipeline.

## Files

### project.yaml
Purpose: default settings for dialect, SGBD, compiler, logging, and LLM.
Inputs: loaded by cli/main.py at startup.
Outputs: merged config dict (then overridden by CLI args and env).
Exchanges: values flow into ingest/analyze/generate/compile modules.

### mapping_types.yaml
Purpose: SQL type to COBOL PIC mappings per dialect.
Inputs: read by analyze/extract_io.py and utils.typing_map.
Outputs: COBOL PIC clauses for io_map.json and prompts.
Notes: map SQL VARCHAR to PIC X(n) (not PIC A) to keep SQL strings safe by default.

### __init__.py
Purpose: package marker (no runtime logic).

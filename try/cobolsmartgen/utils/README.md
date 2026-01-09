# utils

Shared utilities used by most pipeline stages.

## Files

### fs.py
Purpose: filesystem helpers (read/write JSON/YAML, atomic writes, list files).
Inputs: file paths and data objects.
Outputs: files on disk with normalized LF endings.
Exchanges: used across ingest/analyze/generate/compile.

### naming.py
Purpose: naming conventions for program ids and COBOL identifiers.
Inputs: raw names and naming scheme.
Outputs: normalized program ids and variable names.
Exchanges: used by analyze/plan_programs.py and generate steps.

### typing_map.py
Purpose: map SQL types to COBOL PIC clauses.
Inputs: SQL type strings and mapping_types.yaml.
Outputs: COBOL PIC strings.
Exchanges: used by ingest and analyze/extract_io.py. Default fallback maps SQL VARCHAR to PIC X(n).

### trace.py
Purpose: write sidecar hashes, metadata, and prompt logs.
Inputs: paths, prompt text, response text, metadata.
Outputs: *.sha256, *.meta.json, and out/trace/prompts/* files.
Exchanges: used by adapters and generators.

### pipeline_tracer.py
Purpose: per-step pipeline tracing (inputs/outputs per step).
Inputs: step numbers, data objects.
Outputs: out/trace/pipeline_debug/<run_id>/step_* files.
Exchanges: used by cli/main.py and some generators.

### __init__.py
Purpose: package marker (no runtime logic).

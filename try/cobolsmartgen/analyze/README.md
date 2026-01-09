# analyze

Analysis modules that prepare the normalized spec for generation.

## Files

### extract_io.py
Purpose: build io_map.json from the normalized spec.
Inputs: out/normalized_spec.json, config/mapping_types.yaml, config.defaults.dialecte_cobol.
Outputs: out/io_map.json with entities, inputs/outputs, and global copybooks.
Exchanges: uses utils.typing_map.sql_to_cobol (VARCHAR -> PIC X fallback) and
utils.naming.to_cobol_name; reads exigences to detect computed fields.

### plan_programs.py
Purpose: build program_plan.json for a 3 layer architecture (dal/logic/business).
Inputs: out/normalized_spec.json and out/io_map.json.
Outputs: out/program_plan.json with programs, procedures, copybooks, and compile sequence.
Exchanges: output is consumed by generate/* and compile.

### __init__.py
Purpose: package marker (no runtime logic).

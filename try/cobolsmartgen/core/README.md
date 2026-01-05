# core

Core architecture contract and shared constraints.

## Files

### contract_generator.py
Purpose: build an immutable architecture_contract.json.
Inputs: out/normalized_spec.json, out/io_map.json, out/program_plan.json.
Outputs: out/architecture_contract.json with allowed variables/procedures per program and layer rules.
Config/env: CSG_COMPILE_MAX_PASSES, CSG_MAX_LLM_CORRECTIONS, CSG_FAIL_FAST influence contract compilation rules.
Exchanges: consumed by generate/cobol_procedures.py strict validation and by other validators.

### __init__.py
Purpose: package marker (no runtime logic).

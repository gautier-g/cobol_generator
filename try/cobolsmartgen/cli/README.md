# cli

CLI entrypoint and pipeline orchestrator.

## main.py
Purpose: parse CLI arguments, load config, and run the pipeline.
Inputs: CLI args, env vars, config/project.yaml, input spec YAML/JSON.
Outputs: out/* artifacts, out/trace/* traces, logs to stdout (and file if configured).
Exchanges: calls ingest, analyze, core, rag, generate, and compile modules.

## Commands
run: full pipeline (generate + validation + compile).
generate: generation without compile (validation runs unless --single-pass).
compile: compile existing outputs using out/program_plan.json.
fix: run autofix using out/reports/compile.json.

## Common options
--input: spec YAML/JSON (required for run/generate).
--out: output directory (default: out).
--dialecte: gnucobol|ibm|microfocus.
--sgbd: postgres|mysql|oracle|sqlserver|db2|sqlite.
--compiler: override compiler name.
--single-pass: stop after assembly (skip validation/autofix/compile).
--auto-llm: skip confirmation before LLM phases.
--force-rerun: skip rerun confirmation if out/ already exists.
--log-level: DEBUG|INFO|WARNING|ERROR.

## Environment variables used by main
CSG_STRICT_825: strict prompt+validation path for procedures.
CSG_USE_LLM_HEADERS: enable/disable LLM headers.
CSG_USE_LLM_PROCS: enable/disable LLM procedures (legacy path).
CSG_LLM_PROGRAM_MODE: legacy LLM program mode flag.
CSG_USE_CONTRACT: enable architecture contract generation.
CSG_RUN_ID: trace run id for out/trace/pipeline_debug.

## Pipeline flow (run/generate)
Step 1 normalize: ingest/validate_and_normalize -> out/normalized_spec.json.
Step 2 io_map: analyze/extract_io -> out/io_map.json.
Step 3 plan: analyze/plan_programs -> out/program_plan.json.
Step 4 contract: core/contract_generator -> out/architecture_contract.json.
Step 5 rag_index: rag/indexer -> out/rag_index/index.json.
Step 6 sql: generate/sql_files -> out/sql/*.
Step 7 structures: generate/assemble_layout.prepare_structures -> out/copy/*.
Step 8 headers: generate/cobol_headers -> out/<layer>/*.cbl.
Step 9 procedures: generate/cobol_procedures -> out/<layer>/*.cbl and out/trace/generations/*.
Step 10 assembly: generate/assemble_layout -> final layout under out/.
Step 10.5 validation/autofix: validate/run_correction_pipeline (skipped in single-pass).
Step 11 compilation: compile/run_compile_suite (run command only).

## Guardrails and prompts
Rerun guard: if out/ already has artifacts, main.py asks for confirmation unless --force-rerun.
LLM guard: before headers/procs/autofix, main.py asks for confirmation unless --auto-llm.

## Files

### __init__.py
Purpose: package marker (no runtime logic).

## Related docs
SPEC_YAML.md: spec structure and strict formatting rules.
TEST_SCRIPTS.md: how to build setup_db_quick.sh, compile_all.sh, and test_all.sh.

# CobolSmartGen

COBOL generation pipeline driven by a YAML specification and optional LLM calls.

## Quick start (Mistral)

```bash
CSG_LLM_PROVIDER=mistral \
MISTRAL_API_KEY=<your_key> \
MISTRAL_MODEL=mistral-large-latest \
CSG_USE_LLM_HEADERS=0 CSG_LLM_PROGRAM_MODE=1 CSG_USE_LLM_PROCS=1 CSG_STRICT_825=1 \
python3 -m cobolsmartgen.cli.main generate \
  --input salaire_net.yaml \
  --out out \
  --dialecte gnucobol \
  --sgbd postgres \
  --single-pass \
  --auto-llm \
  --force-rerun
```

## Key env vars
MISTRAL_API_KEY / MISTRAL_MODEL: Mistral credentials and model.
CSG_USE_LLM_HEADERS / CSG_USE_LLM_PROCS: enable LLM for headers and procedures.
CSG_LLM_PROGRAM_MODE=1: legacy program mode (safe even in strict mode).
CSG_STRICT_825=1: strict prompt+validation mode for COBOL procedures.
CSG_RUN_ID: custom run id for out/trace/pipeline_debug.

## Docs
cli/README.md: full pipeline and CLI behavior.
SPEC_YAML.md: how the YAML spec is structured and used.
generate/README.md: COBOL/SQL generation details.

## Folder map
adapters/: LLM clients and COBOL compiler adapter.
analyze/: io_map and program_plan builders.
cli/: CLI entrypoint (main.py).
config/: defaults and type mappings.
core/: architecture contract.
generate/: COBOL/SQL generators and assembly.
ingest/: spec validation and normalization.
prompts/: LLM prompt templates.
rag/: local index and retrieval.
schemas/: JSON schemas for artifacts.
utils/: shared helpers.

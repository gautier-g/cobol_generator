# generate

COBOL and SQL generation plus output assembly.

## Files

### cobol_headers.py
Purpose: create minimal COBOL headers for each program.
Inputs: out/program_plan.json.
Outputs: out/<layer>/<PROGRAM-ID>.cbl files with IDENTIFICATION/ENVIRONMENT/DATA/PROCEDURE divisions.
Config/env: CSG_USE_LLM_HEADERS=0 keeps static headers; non-zero enables legacy LLM headers.
Exchanges: writes .meta/.sha256 via utils.trace.

### cobol_procedures.py
Purpose: generate PROCEDURE DIVISION code for each program.
Inputs: out/program_plan.json, out/normalized_spec.json, out/io_map.json, out/architecture_contract.json.
Outputs: out/<layer>/<PROGRAM-ID>.cbl with full program code and trace files under out/trace/generations/.
Config/env: CSG_STRICT_825=1 enables strict prompt+validation; CSG_USE_LLM_PROCS=1 enables legacy LLM mode when strict is off.
Exchanges: calls adapters.llm_auto and validates output against spec rules. Strict mode enforces
SQLCA presence for SQL programs, DATA DIVISION dot rules for PIC/VALUE lines, and wraps long
PROCEDURE DIVISION lines when using fixed format.

### _llm_codegen.py
Purpose: legacy LLM generator for headers and procedures using prompt templates.
Inputs: normalized_spec, io_map, program_plan, prompts/*.txt, and existing .cbl context.
Outputs: updated .cbl files and prompt traces under out/trace/prompts/.
Exchanges: uses prompt_formatters and code_context_builder, then calls llm_auto.

### code_context_builder.py
Purpose: extract interfaces from already generated COBOL to feed later prompts.
Inputs: out/<layer>/*.cbl files.
Outputs: ProgramInterface objects and formatted context strings.
Exchanges: used by _llm_codegen for layered generation.

### prompt_formatters.py
Purpose: format io_map and exigences into prompt friendly text.
Inputs: io_map entries, exigences list, PIC clauses.
Outputs: formatted strings (variables, requirements, explanations).
Exchanges: used by _llm_codegen.

### procedure_templates.py
Purpose: deterministic COBOL snippets for common procedures.
Inputs: procedure name lookup.
Outputs: COBOL snippet strings.
Exchanges: used by _llm_codegen to avoid LLM for standard patterns.

### sql_files.py
Purpose: generate SQL DDL and DML files.
Inputs: out/normalized_spec.json, out/io_map.json, spec.sql section.
Outputs: out/sql/ddl/*.sql and out/sql/dml/*.sql (legacy) or out/sql/<ENTITY>.sql (strict mode).
Config/env: CSG_STRICT_825=1 triggers strict single file and optional LLM fallback.
Exchanges: may call llm_auto when strict mode needs SQL and spec has no DDL.

### assemble_layout.py
Purpose: assemble the final output tree and generate copybooks.
Inputs: out/program_plan.json, generated file list, config.io_map_path or out/io_map.json.
Outputs: out/copy/SQLCA.cpy, out/copy/sqlca.cbl (alias for OCESQL), STATUS-CODES.cpy,
<ENTITY>-RECORD.cpy, and optional out/tests scripts.
Exchanges: logs assembly with utils.trace.log_prompt.

### __init__.py
Purpose: package marker (no runtime logic).

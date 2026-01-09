# prompts

Text templates used to build LLM requests and helper scripts.
Strict mode reads prompt content from the spec `prompting` section, not these templates.

## Files

### header_template.txt
Purpose: prompt skeleton for COBOL header generation (legacy LLM path).
Inputs: variables injected by _llm_codegen.
Outputs: used to build the final user prompt text.
Exchanges: consumed by generate/_llm_codegen.py.

### procedure_template.txt
Purpose: prompt skeleton for COBOL procedure generation (legacy LLM path).
Inputs: variables injected by _llm_codegen and prompt_formatters.
Outputs: used to build the final user prompt text.
Exchanges: consumed by generate/_llm_codegen.py.

### sql_template.txt
Purpose: prompt skeleton for SQL generation in legacy mode.
Inputs: spec and entity context injected by sql generator.
Outputs: used to build SQL LLM prompt text.
Exchanges: consumed by generate/sql_files.py (legacy path).

### diagnosis_template.txt
Purpose: prompt skeleton to analyze errors and propose fixes.
Inputs: error logs or compiler output.
Outputs: used by diagnosis/repair pipelines (autofix).
Exchanges: consumed by validate/autofix modules.

### repair_template.txt
Purpose: prompt skeleton to repair COBOL code based on a diagnosis.
Inputs: diagnosis output and current code.
Outputs: used by repair pipeline to rewrite code.
Exchanges: consumed by validate/autofix modules.

### compiler_test_template.txt
Purpose: shell script template for compile tests.
Inputs: file path and program id placeholders.
Outputs: scripts written under out/tests by assemble_layout.
Exchanges: consumed by generate/assemble_layout.py.

### __init__.py
Purpose: package marker (no runtime logic).

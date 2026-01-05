# SPEC_YAML

This document explains how the YAML spec is structured, how the pipeline uses it, and how to read and author it.

## Where the spec is used
- ingest/validate_and_normalize.py reads the YAML and writes out/normalized_spec.json.
- analyze/extract_io.py builds out/io_map.json from mcd and exigences.
- analyze/plan_programs.py builds out/program_plan.json from io_map and nommage.
- core/contract_generator.py builds out/architecture_contract.json from spec + io_map + plan.
- generate/sql_files.py builds out/sql/* from spec.sql and mcd.
- generate/cobol_procedures.py builds LLM prompts from prompting + programmes + fonctions.

## Required top-level fields
These are required by validate_and_normalize:
- title
- fonctionnalites (list of {id, resume})
- exigences (list of {id, type, regle})
- mcd (contains entites[] with attrs[])
- nommage (variables_case, programmes_case)
- dialecte_cobol (gnucobol|ibm|microfocus)
- sql_cible (postgres|mysql|oracle|sqlserver|db2|sqlite)

## Core data model (mcd)
mcd.entites[].attrs[] defines the schema used for SQL and COBOL:
- name: attribute name (SQL and COBOL base name)
- type: SQL type string (INTEGER, DECIMAL(8,2), VARCHAR(30))
- pk/fk/nullable/default: basic constraints
- cobol_pic: optional explicit PIC clause (overrides type mapping)

## Business rules (exigences)
exigences drives logic and computed fields:
- type: business, regle_metier, calcul, validation, etc.
- regle: the rule text (also used by prompts and io_map computed fields)

## Program configuration (programmes and fonctions)
programmes[] describes each generated program:
- name: PROGRAM-ID (must match functions.programme)
- layer: dal | logic | business
- entities: list of entity names from mcd
- allow_display, allowed_sql, required_statements
- flow, logging_lines, working_storage_lines, environment_lines
- call_interface and using_clause for CALL/USING
- entry_block_rules, entry_exit_sequence, paragraph_constraints for strict validation

fonctions[] describes paragraphs per program:
- programme: PROGRAM-ID this function belongs to
- name: paragraph name
- visibility: public or internal
- required: true/false
- steps: ordered text steps for the LLM prompt
- sql: SQL fragments or intent used in prompts
- display_lines: exact DISPLAY lines to include
- cobol_location: where the paragraph must appear

## SQL section (sql)
sql controls DDL/DML and connection hints:
- ddl: raw DDL lines (optional)
- initial_data: seed data rows
- indexes: list of index definitions
- connection: database name and env keys
- behavior/formatting: SQLCODE checks, commit rules, and line splitting rules

## Prompting section (prompting)
prompting controls strict LLM prompts:
- global_constraints and global_directives
- formatting_guidelines
- forbidden_items (global)
- programs.<PROGRAM-ID>.sections_order
- programs.<PROGRAM-ID>.<section> arrays with template lines

Strict mode reads prompting and injects variables like:
program_id, entity_struct, working_storage_lines, sql_behavior_details, entry_exit_sequence.

## COBOL format and naming
- cobol_format: line_format, max_line_length, comment prefix/column
- naming_rules: prefixes for WS/DB, paragraph style, variable case

## Diagram field (diagramme)
Optional diagram text (Mermaid, PlantUML, or UML JSON) is parsed and merged into the spec.

## Minimal example
```
title: salaire_net
dialecte_cobol: gnucobol
sql_cible: postgres
nommage:
  variables_case: UPPER_SNAKE
  programmes_case: UPPER-KEBAB
fonctionnalites:
  - id: F1
    resume: Calcul du salaire net
exigences:
  - id: R1
    type: business
    regle: "SALARY_NET = ROUND(SALARY_BRUT * 0.7, 2)"
mcd:
  entites:
    - name: EMPLOYEE
      attrs:
        - name: EMP_ID
          type: INTEGER
          pk: true
        - name: EMP_NAME
          type: VARCHAR(30)
        - name: SALARY_BRUT
          type: DECIMAL(8,2)
          cobol_pic: 9(6)V99
        - name: SALARY_NET
          type: DECIMAL(8,2)
          cobol_pic: 9(6)V99
programmes:
  - name: EMPLOYEE-DAL-DB
    layer: dal
    entities: [EMPLOYEE]
  - name: EMPLOYEE-LOGIC
    layer: logic
    entities: [EMPLOYEE]
  - name: EMPLOYEE-BUSINESS
    layer: business
    entities: [EMPLOYEE]
fonctions:
  - programme: EMPLOYEE-LOGIC
    name: CALCULATE-NET
    required: true
    steps:
      - COMPUTE SALARY-NET ROUNDED = SALARY-BRUT * 0.7
```

## Authoring tips
- Use cobol_pic for every numeric field to avoid ambiguous PIC inference.
- Keep PROGRAM-ID names consistent between programmes[] and fonctions[].
- Put strict formatting rules in prompting to avoid line length or dot errors.
- Keep SQL and COBOL names aligned (EMP_ID -> EMP-ID).

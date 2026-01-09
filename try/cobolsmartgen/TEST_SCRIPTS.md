# TEST_SCRIPTS

This document explains how to create test scripts like `test_all.sh`, `compile_all.sh`,
and `setup_db_quick.sh` for any generated output folder.

The goal is to make the scripts deterministic and aligned with the spec, the SQL
layout, and the COBOL programs produced by the pipeline.

## Inputs to align
- Program IDs and layers from `programmes[]`.
- Entity names and columns from `mcd`.
- DDL/DML location from `sql` (single file per entity in strict mode, or legacy
  `sql/create_tables.sql` + `sql/insert_data.sql`).
- Expected calculations and rules from `exigences`.
- DISPLAY lines and logging lines from `programmes[].logging_lines` and `fonctions[].display_lines`.

## setup_db_quick.sh
Purpose: create the database/user, load schema, and insert seed data.

Key parameters:
- `DB_NAME`, `DB_USER`, `DB_PASSWORD`, `DB_HOST`, `DB_PORT`
- SQL file paths under `out/sql/`

Template (adjust names, passwords, and SQL paths):
```bash
#!/bin/bash
set -e

cd "$(dirname "$0")"

DB_NAME="bankdb"
DB_USER="bankuser"
DB_PASSWORD="BANKPWD"
DB_HOST="localhost"
DB_PORT="5432"

sudo -u postgres psql << 'SQL'
DROP DATABASE IF EXISTS bankdb;
DROP ROLE IF EXISTS bankuser;
CREATE ROLE bankuser LOGIN PASSWORD 'BANKPWD';
CREATE DATABASE bankdb OWNER bankuser;
GRANT ALL PRIVILEGES ON DATABASE bankdb TO bankuser;
SQL

# Strict mode: single SQL file per entity
PGPASSWORD=$DB_PASSWORD psql -h $DB_HOST -p $DB_PORT -U $DB_USER -d $DB_NAME \
  -f sql/ACCOUNT.sql

# Legacy mode example:
# PGPASSWORD=$DB_PASSWORD psql -h $DB_HOST -p $DB_PORT -U $DB_USER -d $DB_NAME \
#   -f sql/create_tables.sql
# PGPASSWORD=$DB_PASSWORD psql -h $DB_HOST -p $DB_PORT -U $DB_USER -d $DB_NAME \
#   -f sql/insert_data.sql
```

## compile_all.sh
Purpose: precompile embedded SQL with OCESQL, then compile with GnuCOBOL.

Key parameters:
- Program IDs and layer names.
- Copybook directory (`./copy`) and precompiled directory (`./precompiled`).
- COBOL line format (`-free` or `-fixed`), detected from the first line.
- Link flags for OCESQL and libpq.

Common structure:
- Detect format (free/fixed).
- `ocesql --inc=copy src.cbl precompiled.cob`
- `cobc -m` for shared modules, `-x` for the main logic program.
- Outputs in `./bin`.

## test_all.sh
Purpose: validate environment, database, files, compilation, and runtime behavior.

Recommended structure:
1. Config section (DB params, program names, log paths).
2. Helper functions:
   - `test_result` to count pass/fail.
   - `run_sql` and `run_sql_cmd` to query the DB.
3. Cleanup and log reset (avoid stale logs).
4. Section 1: environment checks (psql, cobc, ocesql, libpq).
5. Section 2: database checks (connection, table schema, indexes, seed data).
6. Section 3: file checks (dal/logic/business .cbl, copybooks).
7. Section 4: compilation (run `./compile_all.sh`, verify binaries).
8. Section 5: execution (run main program, capture logs).
9. Section 5B: scenario tests (table empty, invalid numeric, negative values, 10 rows).
10. Section 5C: dump logs for audit.
11. Section 6: spec compliance (layering, naming, SQL separation).

Guidelines:
- Pull expected values from the spec (exigences and initial_data).
- Use `TRIM()` when comparing CHAR/VARCHAR values.
- Log file checks should match DISPLAY lines exactly.
- For scenario tests, back up and restore the table to keep tests isolated.

## Checklist before you run
- DB name/user/password match in all scripts.
- Program IDs match generated file names.
- SQL file path matches strict vs legacy layout.
- Copybooks exist (`SQLCA`, `STATUS-CODES`, `<ENTITY>-RECORD`).
- COB_LIBRARY_PATH is set to `.` when running the main program.

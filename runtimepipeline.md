# Runtime Pipeline (CobolSmartGen)

Ce document explique comment fonctionne le pipeline Python, sur quoi il repose, comment les prompts sont construits, quelles donnees sont utilisees, et comment la spec YAML est transformee en code COBOL.

## Vue d'ensemble

Le pipeline prend une spec YAML (ex: `try/salaire_net.yaml`), la normalise, construit un plan de programmes, genere des fichiers SQL et COBOL, puis (optionnellement) compile et applique un cycle d'autofix. La generation COBOL peut etre 100% LLM (mode strict), ou hybride (templates + LLM) selon les variables d'environnement.

Le pipeline est orchestre par `try/cobolsmartgen/cli/main.py` et utilise les modules:
- `ingest/validate_and_normalize.py` (validation + normalisation)
- `analyze/extract_io.py` (IO map)
- `analyze/plan_programs.py` (program plan)
- `core/contract_generator.py` (contrat d'architecture)
- `rag/indexer.py` (index RAG)
- `generate/sql_files.py` (DDL/DML)
- `generate/assemble_layout.py` (copybooks/structure)
- `generate/cobol_headers.py` (headers COBOL)
- `generate/cobol_procedures.py` (procedures COBOL)
- `compile/run_compile_suite.py` (compilation)
- `autofix/diagnose.py` + `autofix/repair.py` (autofix)

## Commandes et parametres

### Commandes CLI
- `run` : pipeline complet
- `generate` : generation seule (pas de compilation ni autofix)
- `compile` : compile les fichiers deja generes
- `fix` : relance l'autofix sur le dernier rapport

Exemples:
```
python3 -m cobolsmartgen.cli.main run --input salaire_net.yaml --out out
python3 -m cobolsmartgen.cli.main generate --input salaire_net.yaml --out out --single-pass --auto-llm
python3 -m cobolsmartgen.cli.main compile --out out
python3 -m cobolsmartgen.cli.main fix --out out
```

### Options CLI
- `--input` : spec YAML/JSON (obligatoire pour `run`/`generate`)
- `--out` : dossier de sortie (defaut: `out`)
- `--dialecte` : `gnucobol|ibm|microfocus`
- `--sgbd` : `postgres|mysql|oracle|sqlserver|db2|sqlite`
- `--compiler` : override du compilateur
- `--force-rerun` : ignore la confirmation de rerun si des artefacts existent
- `--auto-llm` : ne pas demander confirmation avant les phases LLM
- `--single-pass` : stop apres generation (pas de validation/autofix/compile)
- `--log-level` : `DEBUG|INFO|WARNING|ERROR`

### Configuration (project.yaml)
Le fichier `try/cobolsmartgen/config/project.yaml` fournit des defaults:
- `defaults.dialecte_cobol`, `defaults.sql_cible`
- `compiler` (name, line_format, flags, copy_dirs)
- `llm` (provider, base_url, model, temperature, timeout)

### Variables d'environnement importantes
LLM:
- `CSG_LLM_PROVIDER` : selection du provider (`mistral`, `groq`)
- `MISTRAL_API_KEY`, `MISTRAL_MODEL`, `MISTRAL_TIMEOUT_S`, `MISTRAL_MAX_TOKENS`
- `GROQ_API_KEY`, `GROQ_MODEL`, `GROQ_TIMEOUT_S`

Pipeline:
- `CSG_STRICT_825=1` : mode strict LLM (un prompt par programme + validation stricte)
- `CSG_USE_LLM_HEADERS=1|0` : active LLM pour headers
- `CSG_USE_LLM_PROCS=1|0` : active LLM pour procedures
- `CSG_LLM_PROGRAM_MODE=1` : force generation de programmes complets (mode legacy LLM)
- `CSG_USE_CONTRACT=1|0` : genere un contrat d'architecture
- `CSG_RUN_ID` : etiquette de trace pour `out/trace/`

## Exemple de lancement (mode strict + single-pass)

Commande type (une seule ligne, cle API remplacee):
```
CSG_LLM_PROVIDER=mistral MISTRAL_API_KEY=<MISTRAL_API_KEY> MISTRAL_MODEL=mistral-large-latest CSG_USE_LLM_HEADERS=0 CSG_USE_LLM_PROCS=1 CSG_LLM_PROGRAM_MODE=1 CSG_STRICT_825=1 python3 -m cobolsmartgen.cli.main generate --input salaire_net.yaml --out out --dialecte gnucobol --sgbd postgres --single-pass --auto-llm --force-rerun
```

Commande type (multi-lignes, plus lisible):
```
CSG_LLM_PROVIDER=mistral \
MISTRAL_API_KEY=<MISTRAL_API_KEY> \
MISTRAL_MODEL=mistral-large-latest \
CSG_USE_LLM_HEADERS=0 \
CSG_USE_LLM_PROCS=1 \
CSG_LLM_PROGRAM_MODE=1 \
CSG_STRICT_825=1 \
python3 -m cobolsmartgen.cli.main generate \
  --input salaire_net.yaml \
  --out out \
  --dialecte gnucobol \
  --sgbd postgres \
  --single-pass \
  --auto-llm \
  --force-rerun
```

Ce que fait chaque parametre:
- `CSG_LLM_PROVIDER` / `MISTRAL_*` : selectionne le fournisseur et le modele LLM.
- `CSG_USE_LLM_HEADERS=0` : genere les headers COBOL en statique (pas LLM).
- `CSG_USE_LLM_PROCS=1` : force LLM pour les procedures (utile en mode non strict).
- `CSG_LLM_PROGRAM_MODE=1` : mode legacy LLM (inutile en strict, mais pas bloquant).
- `CSG_STRICT_825=1` : active le pipeline strict (un prompt par programme + validation).
- `--input` : spec YAML.
- `--out` : dossier de sortie.
- `--dialecte` / `--sgbd` : override du `project.yaml` (propages aux defaults).
- `--single-pass` : stop apres l'assemblage (pas de validation/autofix/compile).
- `--auto-llm` : ne demande pas de confirmation avant les phases LLM.
- `--force-rerun` : ignore le guard si `out/` existe deja.

Deroulement exact pendant l'execution (logs Step 1/11 a 11/11):
1. Normalise la spec (`out/normalized_spec.json`) et sauvegarde l'input brut dans la trace.
2. Construit l'IO map (`out/io_map.json`).
3. Planifie les programmes (`out/program_plan.json`).
4. Genere le contrat d'architecture (`out/architecture_contract.json`) si `CSG_USE_CONTRACT=1`.
5. Construit un index RAG (`out/rag_index/`).
6. Genere le SQL (`out/sql/EMPLOYEE.sql`).
7. Genere les copybooks/structures (`out/copy/*.cpy`).
8. Genere les headers COBOL (statiques si `CSG_USE_LLM_HEADERS=0`).
9. Genere les procedures COBOL via LLM (mode strict si `CSG_STRICT_825=1`).
10. Assemble les fichiers finaux dans `out/`.
10.5. Validation/autofix (saute si `--single-pass`).
11. Compilation (executee uniquement par `run`/`compile`, pas par `generate`).

## Etapes du pipeline (run)

1) **Normalize** (`validate_and_normalize.run`)  
   - Valide la spec contre un schema JSON.  
   - Normalise types, conventions de nommage, defaults.  
   - Produit `out/normalized_spec.json`.

2) **IO mapping** (`extract_io.run`)  
   - Derive les champs (inputs/outputs) a partir du MCD et des regles.  
   - Produit `out/io_map.json`.

3) **Program plan** (`plan_programs.run`)  
   - Planifie DAL/LOGIC/BUSINESS et leurs procedures.  
   - Produit `out/program_plan.json`.

4) **Architecture contract** (`core/contract_generator.run`)  
   - Liste variables/procedures autorisees et interdits par couche.  
   - Produit `out/architecture_contract.json`.

5) **RAG index** (`rag/indexer.run`)  
   - Indexe spec + config pour usage LLM (si active).  
   - Produit `out/rag_index/`.

6) **SQL files** (`generate/sql_files.run`)  
   - Genere DDL/DML depuis `spec.sql`.  
   - Produit `out/sql/`.

7) **Structures / copybooks** (`assemble_layout.prepare_structures`)  
   - Genere `copy/*.cpy` a partir du `io_map`.  
   - Peut aussi generer des scripts de test si un template existe.

8) **Headers COBOL** (`generate/cobol_headers.run`)  
   - Cree des fichiers `.cbl` minimalistes si besoin.
   - Peut etre LLM si `CSG_USE_LLM_HEADERS=1`.

9) **Procedures COBOL** (`generate/cobol_procedures.run`)  
   - Mode strict (`CSG_STRICT_825=1`): un prompt par programme, validation stricte.  
   - Mode legacy: `_llm_codegen` avec templates/prompt formatters.

10) **Assembly** (`assemble_layout.run`)  
   - Assemble le layout final dans `out/`.

10.5) **Validation/Correction** (`validate.run_correction_pipeline`)  
   - Corrige (copybooks, decimals, signatures, etc).  
   - Produit `out/reports/validation_report.md`.

11) **Compilation** (`compile/run_compile_suite.run`)  
   - Compile les `.cbl` generes et produit `out/reports/compile.json`.  
   - Optionnellement, relance autofix si des erreurs persistent.

## Comment les prompts sont generes (mode strict)

Le mode strict (`CSG_STRICT_825=1`) utilise `generate/cobol_procedures.py`:

1) **Collecte des donnees**  
   - `out/normalized_spec.json` (spec normalisee)  
   - `out/io_map.json` (champs COBOL)  
   - `out/program_plan.json` (liste programmes)  
   - `out/architecture_contract.json` (contraintes)

2) **Construction du prompt**  
   - Le pipeline choisit le `PROGRAM-ID` depuis `program_plan.json`, puis le recale sur `programmes[]` de la spec (meme layer + entity).  
   - `_build_strict_prompt()` assemble un prompt textuel a partir de `prompting` + donnees derivees.  
   - Chaque programme a son bloc `prompting.programs.<PROGRAM-ID>`.  
   - Les sections sont ordonnees par `sections_order` (spec), sinon un ordre par defaut est applique.  
   - Chaque section est rendue via substitution `{variables}` (ex: `{program_id}`, `{entity_struct}`, etc.).
   - Les variables de contexte injectees incluent:
     - Structure/IO: `table_signature`, `entity_struct`, `entity_fields_list`, `linkage_struct`
     - Contexte COBOL: `working_storage_lines`, `environment_lines`, `header_comment_lines`
     - Workflow: `flow_notes`, `logging_lines`, `required_statements`
     - SQL/DB: `connection_details`, `sql_behavior_details`, `sql_formatting_details`
     - Regles: `format_details`, `naming_conventions`, `entry_block_rules`, `entry_exit_sequence`, `paragraph_constraints`
     - Meta: `public_paragraphs`, `internal_paragraphs`, `forbidden_items`

3) **Validation + retry**  
   - `_generate_with_validation()` appelle le LLM, valide, puis relance une fois si erreurs.  
   - Les erreurs detectees sont ajoutees en bas du prompt ("corrige-les strictement").  
   - Chaque programme peut donc declencher 1 ou 2 appels LLM.  
   - La validation appliquee par `_validate_program()` couvre:
     - Pas de Markdown / fences.
     - `PROGRAM-ID` exact.
     - Paragraphes requis presents, pas de paragraphes en trop.
     - Paragraphes declares avec un point (ex: `MAIN-ENTRY.`).
     - Champs COBOL (PIC) conformes a la spec.
     - EOF flag present (sauf couche business).
     - Lignes `ENVIRONMENT` et lignes requises presentes.
     - `WORKING-STORAGE SECTION` obligatoire si specifiee.
     - Longueur max de ligne (`cobol_format.max_line_length`).
     - Interdictions `DISPLAY` / `EXEC SQL` / `STOP RUN` selon la couche.
     - Point final uniquement sur ligne seule (pas sur une instruction).
     - Divisions/sections obligatoires.
     - `SOURCE FORMAT` interdit si la spec le refuse.
     - Regles d'entree (bloc `MAIN-ENTRY`) + sequence de sortie imposee.
     - Contraintes par paragraphe (contenu requis/interdit).
     - Variables WORKING-STORAGE en trop refusees.

4) **Trace des prompts**  
   - `out/trace/generations/strict_825/<PROGRAM-ID>/01_prompt.txt`
   - `out/trace/generations/strict_825/<PROGRAM-ID>/02_raw_response.txt`

## Comment la spec YAML est interpretee

Le pipeline s'appuie directement sur la spec YAML, notamment:

### Identite et nommage
- `title`, `dialecte_cobol`, `sql_cible`
- `nommage.variables_case`, `nommage.programmes_case`
- `naming_rules.prefixes`, `naming_rules.paragraph_style`

### Mode COBOL
- `cobol_format.line_format` (fixed/free)
- `cobol_format.max_line_length` (ex: 72)
- `cobol_format.comment` (colonne et prefixe)

### Donnees et MCD
- `mcd.entites[].attrs[]`
  - `name`, `type`, `sql_type`
  - `cobol_pic` (PIC exact)
  - `pk`, `nullable`

### SQL
- `sql.ddl`, `sql.initial_data`, `sql.indexes`
- `sql.connection` (host, user, pass, env vars)
- `sql.behavior` (COMMIT, DISCONNECT, SQLCODE)
- `sql.formatting` (host_vars_one_per_line, split_long_exec_sql)

### Technique
- `technique.cursor_naming`
- `technique.eof_flag`
- `technique.dal_call_pattern`

### Programmes
Chaque entree `programmes[]` definit:
- `name`, `layer`, `entities`
- `allowed_sql`, `allow_display`
- `header_comment_lines`, `working_storage_lines`, `environment_lines`
- `flow`, `logging_lines`, `required_statements`
- `call_interface` + `using_clause`
- contraintes (ex: `entry_block_rules`, `entry_exit_sequence`)

### Fonctions (procedures)
`fonctions[]` definit:
- `programme`, `name`, `visibility`, `required`
- `steps`, `sql`, `display_lines`
- `cobol_location` (division/paragraph)

### Prompting
`prompting` controle la construction des prompts:
- `global_constraints`
- `global_directives`
- `formatting_guidelines`
- `programs.<PROGRAM-ID>.sections_order`
- sections: `context`, `style`, `format`, `sql`, `flow`, `constraints`, etc.

## Sorties produites

Apres execution, le dossier `out/` contient:
- `normalized_spec.json`, `io_map.json`, `program_plan.json`
- `architecture_contract.json`
- `sql/` (DDL/DML)
- `copy/` (copybooks)
- `dal/`, `logic/`, `business/` (sources COBOL)
- `bin/` (executables si compile)
- `reports/` (rapport compile/validation)
- `trace/` (prompts, logs, pipeline_debug)

## Conseils pour eviter les erreurs COBOL/OCESQL

- Respecter le format FIXED (colonne 8..72).
- Ne jamais depasser 72 colonnes.
- Scinder les `EXEC SQL ... FETCH/UPDATE` sur plusieurs lignes.
- Placer `GOBACK` a la fin de `MAIN-ENTRY` (pas de fall-through).

## Ou trouver les traces d'execution

- Prompts: `out/trace/generations/strict_825/.../01_prompt.txt`
- Reponses LLM: `out/trace/generations/strict_825/.../02_raw_response.txt`
- Pipeline debug: `out/trace/pipeline_debug/`
- Logs compile: `out/reports/compile.json`

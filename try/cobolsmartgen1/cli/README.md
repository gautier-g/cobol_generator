# cli - Point d'entree et orchestration

Ce module contient le **point d'entree principal** de CobolSmartGen et orchestre l'execution complete du pipeline de generation.

## Role dans le pipeline

```
┌─────────────────────────────────────────────────────────────────────┐
│                            main.py                                  │
│                     (Chef d'orchestre)                              │
├─────────────────────────────────────────────────────────────────────┤
│  1. Parse les arguments CLI                                         │
│  2. Charge la configuration (project.yaml + env vars)               │
│  3. Execute les 10 etapes du pipeline dans l'ordre                  │
│  4. Gere les confirmations utilisateur (LLM, rerun)                 │
│  5. Produit les traces d'execution                                  │
└─────────────────────────────────────────────────────────────────────┘
```

## Fichiers

### main.py

**Fonction principale** : Orchestrer le pipeline complet de generation COBOL.

| Aspect | Description |
|--------|-------------|
| **Entrees** | Arguments CLI, variables d'environnement, `config/project.yaml`, spec YAML/JSON |
| **Sorties** | `out/*` (artefacts), `out/trace/*` (traces), logs stdout |
| **Appels** | `ingest`, `analyze`, `core`, `generate` |

## Commandes disponibles

| Commande | Description |
|----------|-------------|
| `generate` | Generation complete (recommandee) - s'arrete apres l'assemblage |
| `run` | Pipeline complet avec compilation |
| `compile` | Compile les sorties existantes |

```bash
# Exemple d'utilisation
python3 -m cobolsmartgen.cli.main generate \
  --input spec.yaml \
  --out out \
  --dialecte gnucobol \
  --sgbd postgres \
  --single-pass \
  --auto-llm
```

## Options CLI

| Option | Description | Defaut |
|--------|-------------|--------|
| `--input` | Fichier spec YAML/JSON (obligatoire) | - |
| `--out` | Repertoire de sortie | `out` |
| `--dialecte` | Dialecte COBOL: `gnucobol`, `ibm`, `microfocus` | `gnucobol` |
| `--sgbd` | Base de donnees: `postgres`, `mysql`, `oracle`, `db2`, `sqlite` | `postgres` |
| `--single-pass` | Arrete apres l'assemblage (pas de validation/compile) | `false` |
| `--auto-llm` | Pas de confirmation avant les appels LLM | `false` |
| `--force-rerun` | Pas de confirmation si `out/` existe deja | `false` |
| `--log-level` | Niveau de log: `DEBUG`, `INFO`, `WARNING`, `ERROR` | `INFO` |

## Variables d'environnement

| Variable | Role |
|----------|------|
| `CSG_STRICT_825` | Active le mode strict (validation renforcee) |
| `CSG_USE_LLM_PROCS` | Active la generation LLM des procedures |
| `CSG_USE_CONTRACT` | Active la generation du contrat d'architecture |
| `CSG_RUN_ID` | ID personnalise pour les traces (`out/trace/pipeline_debug`) |

## Deroulement du pipeline

Le pipeline s'execute en **10 etapes sequentielles** :

```
Etape 1: normalize
    └── ingest/validate_and_normalize.py
    └── Sortie: out/normalized_spec.json

Etape 2: io_map
    └── analyze/extract_io.py
    └── Sortie: out/io_map.json

Etape 3: plan
    └── analyze/plan_programs.py
    └── Sortie: out/program_plan.json

Etape 4: contract
    └── core/contract_generator.py
    └── Sortie: out/architecture_contract.json

Etape 5: sql
    └── generate/sql_files.py
    └── Sortie: out/sql/*.sql

Etape 6: structures
    └── generate/assemble_layout.py (prepare_structures)
    └── Sortie: out/copy/*.cpy

Etape 7: headers
    └── generate/cobol_headers.py
    └── Sortie: out/<layer>/*.cbl (squelettes)

Etape 8: procedures
    └── generate/cobol_procedures.py
    └── Sortie: out/<layer>/*.cbl (complets)

Etape 9: assembly
    └── generate/assemble_layout.py
    └── Sortie: structure finale + scripts test

Etape 10: compilation (optionnel)
    └── compile/run_compile_suite.py
    └── Sortie: out/bin/*.so
```

## Gardes-fous

Le CLI integre des mecanismes de protection :

1. **Garde rerun** : Si `out/` contient deja des artefacts, demande confirmation
   - Bypass avec `--force-rerun`

2. **Garde LLM** : Avant les appels LLM (headers, procedures), demande confirmation
   - Bypass avec `--auto-llm`

## Tracabilite

Toutes les executions sont tracees dans :
- `out/trace/pipeline_debug/<run_id>/` : Entrees/sorties de chaque etape
- `out/trace/generations/` : Prompts et reponses LLM
- `out/trace/prompts/` : Historique des appels API

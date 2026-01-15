# CobolSmartGen

**Generateur intelligent de code COBOL** a partir d'une specification YAML, utilisant une architecture 3-tiers et l'IA generative (LLM).

## Vue d'ensemble

CobolSmartGen transforme une specification metier en YAML en un projet COBOL complet et compilable. Le systeme genere automatiquement:
- Des programmes COBOL structures en 3 couches (DAL/Logic/Business)
- Des scripts SQL (DDL/DML)
- Des copybooks partages
- Des scripts de compilation et de test

```
┌─────────────────────────────────────────────────────────────────────┐
│                        SPECIFICATION YAML                           │
│    (entites, attributs, regles metier, contraintes SQL)             │
└─────────────────────────────────────────────────────────────────────┘
                                   │
                                   ▼
┌─────────────────────────────────────────────────────────────────────┐
│                          PIPELINE (11 etapes)                       │
│  normalize → io_map → plan → contract → sql → headers → procs      │
└─────────────────────────────────────────────────────────────────────┘
                                   │
                                   ▼
┌─────────────────────────────────────────────────────────────────────┐
│                         PROJET COBOL COMPLET                        │
│  ├── business/  (couche presentation)                               │
│  ├── logic/     (couche metier)                                     │
│  ├── dal/       (couche acces donnees)                              │
│  ├── copy/      (copybooks partages)                                │
│  ├── sql/       (scripts base de donnees)                           │
│  └── tests/     (scripts de compilation)                            │
└─────────────────────────────────────────────────────────────────────┘
```

## Architecture 3-tiers

Le code COBOL genere suit une architecture en 3 couches:

| Couche | Role | Responsabilites |
|--------|------|-----------------|
| **BUSINESS** | Presentation | Affichage, interface utilisateur, orchestration |
| **LOGIC** | Metier | Calculs, validations, transformations |
| **DAL** | Donnees | Operations SQL (CRUD), gestion curseurs |

Cette separation garantit un code maintenable et modulaire.

## Demarrage rapide

```bash
# Configuration
export CSG_LLM_PROVIDER=mistral
export MISTRAL_API_KEY=<votre_cle>
export MISTRAL_MODEL=mistral-large-latest

# Generation
python3 -m cobolsmartgen.cli.main generate \
  --input spec.yaml \
  --out out \
  --dialecte gnucobol \
  --sgbd postgres \
  --single-pass \
  --auto-llm \
  --force-rerun
```

## Variables d'environnement principales

| Variable | Description |
|----------|-------------|
| `CSG_LLM_PROVIDER` | Provider LLM: `mistral`, `groq`, `gemini`, `ollama` |
| `MISTRAL_API_KEY` | Cle API Mistral |
| `CSG_STRICT_825` | Mode strict avec validation renforcee (1=active) |
| `CSG_USE_LLM_PROCS` | Active la generation LLM des procedures |

## Structure du projet

```
cobolsmartgen1/
├── cli/        # Point d'entree et orchestration du pipeline
├── ingest/     # Validation et normalisation de la specification
├── analyze/    # Extraction IO et planification des programmes
├── core/       # Generation du contrat d'architecture
├── generate/   # Generateurs COBOL et SQL
├── adapters/   # Clients LLM (Mistral, Groq, Gemini, Ollama)
├── config/     # Configuration par defaut et mappings de types
├── schemas/    # Schemas JSON de validation
└── utils/      # Utilitaires partages (fichiers, nommage, traces)
```

## Documentation detaillee

Chaque module possede son propre README expliquant:
- Son role dans le pipeline
- Les fichiers qu'il contient
- Les entrees/sorties de chaque composant

| Module | README |
|--------|--------|
| CLI & Pipeline | [cli/README.md](cli/README.md) |
| Ingestion | [ingest/README.md](ingest/README.md) |
| Analyse | [analyze/README.md](analyze/README.md) |
| Contrat | [core/README.md](core/README.md) |
| Generation | [generate/README.md](generate/README.md) |
| Adaptateurs LLM | [adapters/README.md](adapters/README.md) |
| Configuration | [config/README.md](config/README.md) |
| Schemas | [schemas/README.md](schemas/README.md) |
| Utilitaires | [utils/README.md](utils/README.md) |

## Flux de donnees du pipeline

```
1. YAML Input
      │
      ▼
2. normalize ──────► normalized_spec.json
      │
      ▼
3. extract_io ─────► io_map.json
      │
      ▼
4. plan_programs ──► program_plan.json
      │
      ▼
5. contract ───────► architecture_contract.json
      │
      ▼
6. sql_files ──────► out/sql/*.sql
      │
      ▼
7. structures ─────► out/copy/*.cpy
      │
      ▼
8. headers ────────► out/<layer>/*.cbl (squelettes)
      │
      ▼
9. procedures ─────► out/<layer>/*.cbl (code complet)
      │
      ▼
10. assembly ──────► Structure finale + scripts test
```

## Sortie generee

```
out/
├── normalized_spec.json          # Specification normalisee
├── io_map.json                   # Mappings entrees/sorties
├── program_plan.json             # Plan des programmes
├── architecture_contract.json    # Contrat d'architecture
├── business/                     # Programmes couche BUSINESS
├── logic/                        # Programmes couche LOGIC
├── dal/                          # Programmes couche DAL
├── copy/                         # Copybooks (SQLCA, records)
├── sql/                          # Scripts SQL
├── tests/                        # Scripts de compilation
└── trace/                        # Traces d'execution et prompts
```

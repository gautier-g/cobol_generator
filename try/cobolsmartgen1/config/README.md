# config - Configuration par defaut

Ce module contient les **fichiers de configuration** qui definissent les valeurs par defaut du pipeline.

## Role dans le pipeline

```
┌─────────────────────────────────────────────────────────────────────┐
│                            CONFIG                                   │
├─────────────────────────────────────────────────────────────────────┤
│                                                                     │
│  ┌──────────────────────┐    ┌──────────────────────┐              │
│  │    project.yaml      │    │  mapping_types.yaml  │              │
│  │                      │    │                      │              │
│  │  Dialecte COBOL      │    │  SQL → COBOL         │              │
│  │  SGBD cible          │    │  Type mappings       │              │
│  │  Options compilateur │    │                      │              │
│  │  Config LLM          │    │                      │              │
│  └──────────────────────┘    └──────────────────────┘              │
│           │                            │                            │
│           └────────────┬───────────────┘                            │
│                        ▼                                            │
│              Tous les modules du pipeline                           │
│                                                                     │
└─────────────────────────────────────────────────────────────────────┘
```

## Fichiers

### project.yaml

**Fonction** : Configuration globale par defaut du projet.

| Section | Description |
|---------|-------------|
| `dialecte_cobol` | Dialecte COBOL (`gnucobol`, `ibm`, `microfocus`) |
| `sgbd` | Base de donnees cible (`postgres`, `mysql`, `oracle`, `db2`) |
| `compiler` | Options du compilateur (nom, flags, copy_dirs) |
| `llm` | Configuration LLM (provider, model, temperature) |
| `logging` | Niveau de log et format |

#### Exemple de structure

```yaml
# project.yaml
dialecte_cobol: gnucobol
sgbd: postgres

compiler:
  name: cobc
  line_format: fixed
  flags:
    - -x
    - -free
  copy_dirs:
    - out/copy

llm:
  provider: mistral
  model: mistral-large-latest
  temperature: 0.2
  max_tokens: 2560

logging:
  level: INFO
  format: "[%(levelname)s] %(message)s"
```

#### Priorite de configuration

```
1. Variables d'environnement (CSG_*, MISTRAL_*, etc.)
      │
      ▼
2. Arguments CLI (--dialecte, --sgbd, etc.)
      │
      ▼
3. project.yaml (valeurs par defaut)
```

### mapping_types.yaml

**Fonction** : Definir les correspondances entre types SQL et clauses PIC COBOL.

#### Structure du fichier

```yaml
# mapping_types.yaml
gnucobol:
  VARCHAR: "PIC X({size})"
  INTEGER: "PIC S9(9) COMP-5"
  DECIMAL: "PIC S9({precision})V9({scale})"
  DATE: "PIC X(10)"
  BOOLEAN: "PIC 9"
  TIMESTAMP: "PIC X(26)"

ibm:
  VARCHAR: "PIC X({size})"
  INTEGER: "PIC S9(9) COMP"
  # ... variantes IBM
```

#### Utilisation dans le pipeline

```
mapping_types.yaml
        │
        ▼
analyze/extract_io.py
        │
        ▼
io_map.json (contient les PIC clauses)
```

#### Exemple de conversion

| Type SQL | Dialecte | Clause PIC |
|----------|----------|------------|
| `VARCHAR(50)` | gnucobol | `PIC X(50)` |
| `INTEGER` | gnucobol | `PIC S9(9) COMP-5` |
| `DECIMAL(10,2)` | gnucobol | `PIC S9(8)V99` |

## Points cles pour la soutenance

1. **Configuration centralisee** : Toutes les valeurs par defaut dans un seul endroit
2. **Flexibilite** : Les valeurs peuvent etre surchargees par env vars ou CLI
3. **Mapping deterministe** : Les conversions SQL→COBOL sont previsibles
4. **Multi-dialecte** : Support de plusieurs dialectes COBOL

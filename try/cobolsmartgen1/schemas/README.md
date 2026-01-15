# schemas - Schemas JSON de validation

Ce module contient les **schemas JSON** qui definissent la structure attendue des artefacts du pipeline.

## Role dans le pipeline

```
┌─────────────────────────────────────────────────────────────────────┐
│                           SCHEMAS                                   │
├─────────────────────────────────────────────────────────────────────┤
│                                                                     │
│  Validation des artefacts a chaque etape du pipeline :              │
│                                                                     │
│  spec.yaml ──► input_spec.schema.json                               │
│       │                                                             │
│       ▼                                                             │
│  normalized_spec.json ──► normalized_spec.schema.json               │
│       │                                                             │
│       ▼                                                             │
│  io_map.json ──► io_map.schema.json                                 │
│       │                                                             │
│       ▼                                                             │
│  program_plan.json ──► program_plan.schema.json                     │
│                                                                     │
└─────────────────────────────────────────────────────────────────────┘
```

## Fichiers

### input_spec.schema.json

**Fonction** : Valider la specification YAML/JSON fournie par l'utilisateur.

| Champ requis | Description |
|--------------|-------------|
| `nom` | Nom du projet |
| `entites` | Liste des entites metier |
| `entites[].nom` | Nom de l'entite |
| `entites[].attributs` | Liste des attributs |

#### Exemple de spec valide

```yaml
nom: "MonProjet"
entites:
  - nom: "EMPLOYEE"
    attributs:
      - nom: "ID"
        type: "INTEGER"
        nullable: false
      - nom: "NOM"
        type: "VARCHAR(50)"
```

### normalized_spec.schema.json

**Fonction** : Definir la structure de `normalized_spec.json`.

| Section | Description |
|---------|-------------|
| `nom` | Nom normalise du projet |
| `entites` | Entites avec noms normalises |
| `prompting` | Regles pour les prompts LLM |
| `cobol_format` | Regles de format COBOL |

### io_map.schema.json

**Fonction** : Definir la structure de `io_map.json`.

```json
{
  "entities": {
    "EMPLOYEE": {
      "inputs": [
        {"name": "EMP-ID", "pic": "PIC S9(9) COMP-5", "type": "INTEGER"}
      ],
      "outputs": [
        {"name": "EMP-NAME", "pic": "PIC X(50)", "type": "VARCHAR(50)"}
      ]
    }
  },
  "copybooks": ["SQLCA", "STATUS-CODES"]
}
```

### program_plan.schema.json

**Fonction** : Definir la structure de `program_plan.json`.

```json
{
  "programs": [
    {
      "program_id": "EMPLOYEE-DAL-DB",
      "layer": "dal",
      "entity": "EMPLOYEE",
      "procedures": ["DAL-CONNECT", "DAL-READ"],
      "copybooks": ["EMPLOYEE-RECORD", "SQLCA"]
    }
  ],
  "compile_sequence": ["EMPLOYEE-DAL-DB", "EMPLOYEE-LOGIC"]
}
```

### compile_report.schema.json

**Fonction** : Definir la structure des rapports de compilation.

```json
{
  "programs": [
    {
      "program_id": "EMPLOYEE-DAL-DB",
      "status": "success",
      "errors": [],
      "warnings": []
    }
  ],
  "summary": {
    "total": 3,
    "success": 3,
    "failed": 0
  }
}
```

### diagnosis.schema.json & patch_plan.schema.json

**Fonction** : Structures pour le systeme d'autofix (correction automatique).

## Utilisation dans le code

```python
import jsonschema
from utils.fs import read_json

# Charger le schema
schema = read_json("schemas/input_spec.schema.json")

# Valider les donnees
data = read_json("spec.yaml")
jsonschema.validate(data, schema)  # Leve une exception si invalide
```

## Avantages de la validation par schema

| Avantage | Description |
|----------|-------------|
| **Detection precoce** | Erreurs detectees avant la generation |
| **Messages clairs** | Le schema indique exactement ce qui manque |
| **Documentation** | Le schema documente la structure attendue |
| **Coherence** | Garantit que les artefacts sont valides |

## Points cles pour la soutenance

1. **Validation stricte** : Chaque artefact est valide contre son schema
2. **Documentation vivante** : Les schemas documentent les structures
3. **Detection precoce des erreurs** : Evite les bugs en aval
4. **Standard JSON Schema** : Utilise un standard reconnu (JSON Schema Draft-07)

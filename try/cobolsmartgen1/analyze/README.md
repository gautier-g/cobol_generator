# analyze - Analyse et planification

Ce module analyse la specification normalisee pour **extraire les mappings I/O** et **planifier l'architecture 3-tiers** des programmes COBOL.

## Role dans le pipeline

```
┌─────────────────────────────────────────────────────────────────────┐
│                    normalized_spec.json                             │
└─────────────────────────────────────────────────────────────────────┘
                                │
                                ▼
┌─────────────────────────────────────────────────────────────────────┐
│                          ANALYZE                                    │
├─────────────────────────────────────────────────────────────────────┤
│                                                                     │
│  ┌──────────────────┐          ┌──────────────────┐                │
│  │   extract_io.py  │          │  plan_programs.py │                │
│  │                  │          │                   │                │
│  │  SQL → COBOL     │    ───►  │  Architecture     │                │
│  │  Type mapping    │          │  3-tiers          │                │
│  └──────────────────┘          └───────────────────┘                │
│           │                              │                          │
│           ▼                              ▼                          │
│     io_map.json                  program_plan.json                  │
│                                                                     │
└─────────────────────────────────────────────────────────────────────┘
```

## Fichiers

### extract_io.py

**Fonction principale** : Construire le mapping des entrees/sorties pour chaque entite.

| Aspect | Description |
|--------|-------------|
| **Entrees** | `normalized_spec.json`, `config/mapping_types.yaml` |
| **Sorties** | `out/io_map.json` |
| **Dependances** | `utils.typing_map`, `utils.naming` |

#### Conversion SQL → COBOL

Le module convertit les types SQL en clauses PIC COBOL :

| Type SQL | Clause PIC COBOL |
|----------|------------------|
| `VARCHAR(n)` | `PIC X(n)` |
| `INTEGER` | `PIC S9(9) COMP-5` |
| `DECIMAL(p,s)` | `PIC S9(p-s)V9(s)` |
| `DATE` | `PIC X(10)` |
| `BOOLEAN` | `PIC 9` |

#### Structure de io_map.json

```json
{
  "entities": {
    "EMPLOYEE": {
      "inputs": [
        {"name": "EMP-ID", "pic": "PIC S9(9) COMP-5"}
      ],
      "outputs": [
        {"name": "EMP-NAME", "pic": "PIC X(50)"}
      ]
    }
  },
  "copybooks": ["SQLCA", "STATUS-CODES"]
}
```

### plan_programs.py

**Fonction principale** : Planifier l'architecture 3-tiers des programmes.

| Aspect | Description |
|--------|-------------|
| **Entrees** | `normalized_spec.json`, `io_map.json` |
| **Sorties** | `out/program_plan.json` |
| **Consommateurs** | `generate/*`, `compile/*` |

#### Architecture 3-tiers generee

Pour chaque entite, le module cree **3 programmes** :

```
EMPLOYEE (entite)
    │
    ├── EMPLOYEE-DAL-DB.cbl     (couche DAL)
    │   └── Operations SQL: CONNECT, READ, SAVE, END
    │
    ├── EMPLOYEE-LOGIC.cbl      (couche LOGIC)
    │   └── Regles metier: calculs, validations
    │
    └── EMPLOYEE-BUSINESS.cbl   (couche BUSINESS)
        └── Presentation: affichage, orchestration
```

#### Structure de program_plan.json

```json
{
  "programs": [
    {
      "program_id": "EMPLOYEE-DAL-DB",
      "layer": "dal",
      "entity": "EMPLOYEE",
      "procedures": ["DAL-CONNECT", "DAL-READ", "DAL-SAVE", "DAL-END"],
      "copybooks": ["EMPLOYEE-RECORD", "SQLCA"]
    }
  ],
  "compile_sequence": [
    "EMPLOYEE-DAL-DB",
    "EMPLOYEE-LOGIC",
    "EMPLOYEE-BUSINESS"
  ]
}
```

## Points cles pour la soutenance

1. **Separation des responsabilites** : Chaque couche a un role distinct
   - DAL : Acces aux donnees uniquement
   - LOGIC : Regles metier uniquement
   - BUSINESS : Interface utilisateur uniquement

2. **Sequence de compilation** : L'ordre garantit que les dependances sont resolues
   - DAL compile en premier (pas de dependances)
   - LOGIC compile ensuite (depend de DAL)
   - BUSINESS compile en dernier (depend de LOGIC)

3. **Mapping deterministe** : Les types SQL sont convertis de maniere previsible

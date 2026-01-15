# core - Contrat d'architecture

Ce module genere le **contrat d'architecture**, un document immuable qui definit les regles et contraintes que le code COBOL genere doit respecter.

## Role dans le pipeline

```
┌─────────────────────────────────────────────────────────────────────┐
│  normalized_spec.json + io_map.json + program_plan.json             │
└─────────────────────────────────────────────────────────────────────┘
                                │
                                ▼
┌─────────────────────────────────────────────────────────────────────┐
│                            CORE                                     │
├─────────────────────────────────────────────────────────────────────┤
│                    contract_generator.py                            │
│                                                                     │
│  Genere les regles de gouvernance :                                 │
│  • Variables autorisees par programme                               │
│  • Procedures autorisees par couche                                 │
│  • Acces SQL par couche                                             │
│  • Contraintes de validation                                        │
└─────────────────────────────────────────────────────────────────────┘
                                │
                                ▼
┌─────────────────────────────────────────────────────────────────────┐
│                  architecture_contract.json                         │
│                (Document de gouvernance IMMUTABLE)                  │
└─────────────────────────────────────────────────────────────────────┘
```

## Fichiers

### contract_generator.py

**Fonction principale** : Generer le contrat d'architecture immuable.

| Aspect | Description |
|--------|-------------|
| **Entrees** | `normalized_spec.json`, `io_map.json`, `program_plan.json` |
| **Sorties** | `out/architecture_contract.json` |
| **Consommateurs** | `generate/cobol_procedures.py` (validation stricte) |

## Le contrat d'architecture

Le contrat definit **ce qui est autorise** pour chaque programme et chaque couche.

### Regles par couche

| Couche | SQL autorise | Variables | Responsabilites |
|--------|--------------|-----------|-----------------|
| **DAL** | Oui (EXEC SQL) | Toutes | CRUD, curseurs, connexion |
| **LOGIC** | Non | Calculs, flags | Validations, transformations |
| **BUSINESS** | Non | Affichage | Interface, orchestration |

### Structure du contrat

```json
{
  "programs": {
    "EMPLOYEE-DAL-DB": {
      "layer": "dal",
      "allowed_variables": ["EMP-ID", "EMP-NAME", "SQLCA", "SQLCODE"],
      "allowed_procedures": ["DAL-CONNECT", "DAL-READ", "DAL-SAVE", "DAL-END"],
      "sql_access": true
    },
    "EMPLOYEE-LOGIC": {
      "layer": "logic",
      "allowed_variables": ["EMP-ID", "EMP-NAME", "CALC-RESULT"],
      "allowed_procedures": ["MAIN-PROCESS", "VALIDATE-DATA"],
      "sql_access": false
    }
  },
  "layer_rules": {
    "dal": {
      "sql_access": true,
      "can_call": []
    },
    "logic": {
      "sql_access": false,
      "can_call": ["dal"]
    },
    "business": {
      "sql_access": false,
      "can_call": ["logic"]
    }
  }
}
```

## Validation stricte

Le contrat est utilise par `cobol_procedures.py` pour valider le code genere par le LLM :

1. **Variables** : Seules les variables declarees dans `allowed_variables` peuvent etre utilisees
2. **Procedures** : Seules les procedures listees dans `allowed_procedures` sont acceptees
3. **SQL** : Les instructions EXEC SQL sont interdites si `sql_access: false`
4. **Appels** : Une couche ne peut appeler que les couches autorisees par `can_call`

### Exemple de rejet

```cobol
* Programme EMPLOYEE-LOGIC (couche logic)
* Le contrat interdit l'acces SQL dans cette couche

EXEC SQL                           ← REJETE : sql_access = false
    SELECT * FROM EMPLOYEE
END-EXEC
```

## Points cles pour la soutenance

1. **Gouvernance stricte** : Le contrat empeche les violations d'architecture
2. **Document immuable** : Une fois genere, le contrat ne change pas
3. **Validation automatique** : Le LLM recoit un feedback precis sur les violations
4. **Separation des couches** : Garantit que le code respecte l'architecture 3-tiers

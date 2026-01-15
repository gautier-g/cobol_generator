# ingest - Validation et normalisation

Ce module est la **porte d'entree** du pipeline. Il valide la specification YAML fournie par l'utilisateur et la normalise pour garantir la coherence des etapes suivantes.

## Role dans le pipeline

```
┌─────────────────────────────────────────────────────────────────────┐
│                      SPECIFICATION YAML                             │
│                    (fournie par l'utilisateur)                      │
└─────────────────────────────────────────────────────────────────────┘
                                │
                                ▼
┌─────────────────────────────────────────────────────────────────────┐
│                         INGEST                                      │
├─────────────────────────────────────────────────────────────────────┤
│  1. Validation contre le schema JSON                                │
│  2. Normalisation des noms (UPPER-KEBAB)                            │
│  3. Application des valeurs par defaut                              │
│  4. Conversion des types SQL → COBOL                                │
└─────────────────────────────────────────────────────────────────────┘
                                │
                                ▼
┌─────────────────────────────────────────────────────────────────────┐
│                    normalized_spec.json                             │
│              (specification prete pour le pipeline)                 │
└─────────────────────────────────────────────────────────────────────┘
```

## Fichiers

### validate_and_normalize.py

**Fonction principale** : Transformer la specification brute en format normalise.

| Aspect | Description |
|--------|-------------|
| **Entrees** | Fichier YAML/JSON, `schemas/input_spec.schema.json`, config |
| **Sorties** | `out/normalized_spec.json` + fichiers sidecars (`.meta.json`, `.sha256`) |
| **Dependances** | `utils.fs`, `utils.trace`, `utils.typing_map`, `diagram_parser` |

#### Processus de normalisation

1. **Chargement** : Lecture du fichier YAML/JSON
2. **Validation** : Verification contre le schema JSON
3. **Normalisation des noms** :
   - Entites → UPPER-KEBAB (ex: `employee` → `EMPLOYEE`)
   - Attributs → UPPER-KEBAB (ex: `salaire_net` → `SALAIRE-NET`)
4. **Application des defauts** : Valeurs par defaut pour les champs manquants
5. **Preservation** : Conservation des sections `prompting` et `cobol_format`

#### Exemple de transformation

```yaml
# AVANT (spec.yaml)
entites:
  - nom: employee
    attributs:
      - nom: salaire_net
        type: DECIMAL(10,2)

# APRES (normalized_spec.json)
{
  "entites": [{
    "nom": "EMPLOYEE",
    "attributs": [{
      "nom": "SALAIRE-NET",
      "type": "DECIMAL(10,2)",
      "pic": "S9(8)V99"
    }]
  }]
}
```

### diagram_parser.py

**Fonction** : Parser les diagrammes textuels en structure de donnees.

| Aspect | Description |
|--------|-------------|
| **Entrees** | Champ `spec["diagramme"]` (chaine de caracteres) |
| **Sorties** | Dictionnaire avec `entities` et `relations` |
| **Formats supportes** | Mermaid ERD, PlantUML, UML JSON |

#### Exemple Mermaid ERD

```yaml
# Dans le fichier YAML
diagramme: |
  erDiagram
    EMPLOYEE {
      int id PK
      string nom
      decimal salaire
    }
```

## Artefacts produits

| Fichier | Description |
|---------|-------------|
| `normalized_spec.json` | Specification normalisee |
| `normalized_spec.json.meta.json` | Metadonnees (timestamp, version) |
| `normalized_spec.json.sha256` | Hash pour verification d'integrite |

## Points cles pour la soutenance

1. **Validation stricte** : Le schema JSON garantit que la spec est valide avant traitement
2. **Normalisation deterministe** : Meme entree = meme sortie (pas d'aleatoire)
3. **Tracabilite** : Les fichiers sidecars permettent de verifier l'integrite
4. **Extensibilite** : Support de plusieurs formats de diagrammes

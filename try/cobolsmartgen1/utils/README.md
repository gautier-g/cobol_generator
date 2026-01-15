# utils - Utilitaires partages

Ce module contient les **fonctions utilitaires** utilisees par tous les autres modules du pipeline.

## Role dans le pipeline

```
┌─────────────────────────────────────────────────────────────────────┐
│                            UTILS                                    │
├─────────────────────────────────────────────────────────────────────┤
│                                                                     │
│  ┌────────────┐  ┌────────────┐  ┌────────────┐  ┌────────────┐    │
│  │   fs.py    │  │ naming.py  │  │typing_map  │  │  trace.py  │    │
│  │            │  │            │  │    .py     │  │            │    │
│  │ Lecture/   │  │ Nommage    │  │ SQL→COBOL  │  │ Tracage    │    │
│  │ Ecriture   │  │ COBOL      │  │ Types      │  │ Logs       │    │
│  └────────────┘  └────────────┘  └────────────┘  └────────────┘    │
│         │              │              │              │              │
│         └──────────────┼──────────────┼──────────────┘              │
│                        ▼                                            │
│              Utilises par TOUS les modules                          │
│                                                                     │
└─────────────────────────────────────────────────────────────────────┘
```

## Fichiers

### fs.py

**Fonction** : Operations sur le systeme de fichiers.

| Fonction | Description |
|----------|-------------|
| `read_json(path)` | Lit un fichier JSON |
| `write_json(path, data)` | Ecrit un fichier JSON avec formatage |
| `read_yaml(path)` | Lit un fichier YAML |
| `write_yaml(path, data)` | Ecrit un fichier YAML |
| `atomic_write(path, content)` | Ecriture atomique (evite corruption) |
| `list_files(dir, pattern)` | Liste les fichiers selon un pattern |

#### Caracteristiques
- **Fins de ligne normalisees** : Toujours LF (Unix style)
- **Ecriture atomique** : Ecrit dans un fichier temporaire puis renomme
- **Encodage UTF-8** : Par defaut pour tous les fichiers

### naming.py

**Fonction** : Conventions de nommage COBOL.

| Fonction | Description |
|----------|-------------|
| `to_cobol_name(name)` | Convertit en identifiant COBOL valide |
| `to_program_id(name)` | Convertit en PROGRAM-ID valide |
| `normalize_entity(name)` | Normalise un nom d'entite |

#### Regles de nommage

```
Entree          → Sortie
─────────────────────────────
"salaire_net"   → "SALAIRE-NET"
"employé"       → "EMPLOYE"
"id_client"     → "ID-CLIENT"
"123abc"        → "X123ABC"     (prefixe si commence par chiffre)
```

#### Schemas supportes
- **UPPER-KEBAB** : `SALAIRE-NET` (defaut)
- **UPPER_SNAKE** : `SALAIRE_NET`

### typing_map.py

**Fonction** : Mapping des types SQL vers COBOL PIC.

| Fonction | Description |
|----------|-------------|
| `sql_to_cobol(sql_type, dialect)` | Convertit un type SQL en PIC |
| `get_pic_clause(type_info)` | Genere la clause PIC complete |

#### Table de conversion

```
SQL Type         → COBOL PIC
───────────────────────────────────
VARCHAR(50)      → PIC X(50)
INTEGER          → PIC S9(9) COMP-5
DECIMAL(10,2)    → PIC S9(8)V99
DATE             → PIC X(10)
BOOLEAN          → PIC 9
TEXT             → PIC X(1000)
```

### trace.py

**Fonction** : Tracage et metadonnees.

| Fonction | Description |
|----------|-------------|
| `write_sha256(path)` | Ecrit le hash SHA256 d'un fichier |
| `write_meta(path, meta)` | Ecrit les metadonnees (.meta.json) |
| `log_prompt(provider, prompt, response)` | Log un echange LLM |

#### Fichiers sidecars generes

```
out/normalized_spec.json           # Fichier principal
out/normalized_spec.json.sha256    # Hash SHA256
out/normalized_spec.json.meta.json # Metadonnees
```

#### Structure des metadonnees

```json
{
  "timestamp": "2026-01-15T14:30:00Z",
  "source": "ingest/validate_and_normalize.py",
  "version": "1.0.0",
  "checksum": "sha256:abc123..."
}
```

### pipeline_tracer.py

**Fonction** : Tracage detaille du pipeline.

| Fonction | Description |
|----------|-------------|
| `start_step(step_num, name)` | Demarre le tracage d'une etape |
| `log_input(data)` | Log les donnees d'entree |
| `log_output(data)` | Log les donnees de sortie |
| `end_step()` | Termine le tracage |

#### Structure des traces

```
out/trace/pipeline_debug/20260115_143052/
├── step_01_normalize/
│   ├── input_raw.txt
│   ├── output_normalized.json
│   └── metadata.json
├── step_02_io_map/
│   ├── input_normalized.json
│   └── output_io_map.json
└── summary.json
```

## Points cles pour la soutenance

1. **Reutilisabilite** : Fonctions utilisees par tous les modules
2. **Determinisme** : Les conversions sont previsibles et reproductibles
3. **Tracabilite** : Chaque operation est enregistree avec metadonnees
4. **Robustesse** : Ecriture atomique, encodage normalise

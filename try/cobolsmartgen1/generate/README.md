# generate - Generation de code

Ce module est le **coeur de la generation**. Il produit le code COBOL et SQL a partir des artefacts d'analyse, en utilisant le LLM pour generer les procedures.

## Role dans le pipeline

```
┌─────────────────────────────────────────────────────────────────────┐
│        program_plan.json + io_map.json + contract.json              │
└─────────────────────────────────────────────────────────────────────┘
                                │
                                ▼
┌─────────────────────────────────────────────────────────────────────┐
│                          GENERATE                                   │
├─────────────────────────────────────────────────────────────────────┤
│                                                                     │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐                 │
│  │ sql_files   │  │cobol_headers│  │cobol_procs  │                 │
│  │             │  │             │  │   + LLM     │                 │
│  └─────────────┘  └─────────────┘  └─────────────┘                 │
│        │                │                 │                         │
│        ▼                ▼                 ▼                         │
│   out/sql/*.sql    out/<layer>/     out/<layer>/                   │
│                    *.cbl (shell)    *.cbl (complet)                │
│                                                                     │
│  ┌─────────────────────────────────────────────────────────────┐   │
│  │                    assemble_layout.py                        │   │
│  │         Assemblage final + copybooks + scripts test          │   │
│  └─────────────────────────────────────────────────────────────┘   │
│                                                                     │
└─────────────────────────────────────────────────────────────────────┘
```

## Fichiers

### cobol_headers.py

**Fonction** : Creer les squelettes COBOL (divisions IDENTIFICATION, ENVIRONMENT, DATA).

| Aspect | Description |
|--------|-------------|
| **Entrees** | `program_plan.json` |
| **Sorties** | `out/<layer>/<PROGRAM-ID>.cbl` (squelettes) |
| **Mode** | Statique (pas de LLM si `CSG_USE_LLM_HEADERS=0`) |

#### Structure du squelette genere

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EMPLOYEE-DAL-DB.
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           COPY SQLCA.
           COPY EMPLOYEE-RECORD.
      *
       PROCEDURE DIVISION.
      * (a remplir par cobol_procedures.py)
```

### cobol_procedures.py

**Fonction** : Generer la PROCEDURE DIVISION complete via LLM.

| Aspect | Description |
|--------|-------------|
| **Entrees** | `program_plan.json`, `normalized_spec.json`, `io_map.json`, `architecture_contract.json` |
| **Sorties** | `out/<layer>/<PROGRAM-ID>.cbl` (complet) + traces |
| **Mode** | LLM avec validation stricte (`CSG_STRICT_825=1`) |

#### Processus de generation

```
1. Construction du prompt
   └── Contexte: spec, io_map, contract, regles COBOL

2. Appel LLM (Mistral)
   └── Generation de la PROCEDURE DIVISION

3. Validation stricte
   ├── Variables utilisees vs autorisees
   ├── Procedures vs contract
   ├── SQL vs couche
   └── Format COBOL (72 colonnes, END-IF, etc.)

4. Si echec validation → Retry avec feedback

5. Assemblage dans le fichier .cbl
```

### sql_files.py

**Fonction** : Generer les scripts SQL (DDL/DML).

| Aspect | Description |
|--------|-------------|
| **Entrees** | `normalized_spec.json`, `io_map.json` |
| **Sorties** | `out/sql/<ENTITY>.sql` |

#### Exemple de sortie

```sql
-- out/sql/EMPLOYEE.sql
CREATE TABLE EMPLOYEE (
    EMP_ID INTEGER PRIMARY KEY,
    EMP_NAME VARCHAR(50) NOT NULL,
    SALARY DECIMAL(10,2)
);
```

### assemble_layout.py

**Fonction** : Assembler la structure finale et generer les copybooks.

| Aspect | Description |
|--------|-------------|
| **Entrees** | `program_plan.json`, fichiers generes, `io_map.json` |
| **Sorties** | Copybooks, scripts de test, structure finale |

#### Copybooks generes

| Fichier | Contenu |
|---------|---------|
| `SQLCA.cpy` | Structure SQL Communication Area |
| `STATUS-CODES.cpy` | Codes de retour standards |
| `<ENTITY>-RECORD.cpy` | Structure d'enregistrement de l'entite |

### Autres fichiers

| Fichier | Role |
|---------|------|
| `code_context_builder.py` | Extrait les interfaces des programmes deja generes |
| `prompt_formatters.py` | Formate les donnees pour les prompts LLM |
| `cobol_constraints.py` | Valide la syntaxe COBOL |
| `steps_generator.py` | Genere les etapes manquantes via LLM |

## Mode strict (CSG_STRICT_825=1)

Le mode strict active plusieurs validations :

1. **SQLCA obligatoire** pour les programmes SQL
2. **Regles DATA DIVISION** : lignes PIC/VALUE terminees par point
3. **Format fixe** : lignes de 72 caracteres max
4. **Identifiants declares** : toutes les variables doivent etre dans io_map

## Traces generees

```
out/trace/
├── generations/
│   └── <PROGRAM>/
│       ├── 01_prompt.txt       # Prompt envoye au LLM
│       └── 02_raw_response.txt # Reponse brute du LLM
└── prompts/mistral/
    └── YYYYMMDD/
        ├── HHMMSS_chat.prompt.txt
        └── HHMMSS_chat.response.txt
```

## Points cles pour la soutenance

1. **Generation hybride** : Headers statiques + procedures LLM
2. **Validation stricte** : Le code LLM est valide contre le contrat
3. **Retry intelligent** : En cas d'echec, le LLM recoit le feedback d'erreur
4. **Tracabilite complete** : Tous les prompts et reponses sont enregistres

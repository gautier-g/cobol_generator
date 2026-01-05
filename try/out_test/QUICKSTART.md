# Guide de démarrage rapide

## Installation automatique (RECOMMANDÉE)

### Option 1: Installation complète en une commande

```bash
make install
```

Ce script va automatiquement:
- ✓ Installer PostgreSQL (si absent)
- ✓ Installer GnuCOBOL (si absent)
- ✓ Installer ocesql (si absent)
- ✓ Configurer la base de données empdb
- ✓ Compiler les programmes COBOL

Ensuite, validez l'installation:
```bash
make test
```

### Option 2: Installation manuelle par étapes

Si vous avez déjà les outils (PostgreSQL, GnuCOBOL, ocesql):

```bash
make all      # setup + compile
make test     # validation
```

## Utilisation

### Exécuter le batch

```bash
make run
```

Résultat attendu:
```
==========================================
DEBUT TRAITEMENT BATCH CALCUL SALAIRE NET
==========================================
Connexion DB reussie: empdb
Curseur C_EMP ouvert
----------------------------------------
EMPLOYE : Dupont
ID      : 0001
BRUT    : 00003000.00
NET     : 00002100.00
----------------------------------------
EMPLOYE : Durand
ID      : 0002
BRUT    : 00001500.00
NET     : 00001050.00
==========================================
FIN TRAITEMENT BATCH
Nombre employes traites: 0002
==========================================
```

### Vérifier les résultats

```bash
make verify
```

Vous devriez voir les salaires nets correctement calculés (BRUT × 0.7).

## Tests de validation

Le script `test_all.sh` effectue 30 tests de validation:

```bash
./test_all.sh
```

**Tests effectués:**
1. **Environnement** (4 tests)
   - PostgreSQL installé
   - GnuCOBOL installé
   - ocesql installé
   - libpq installé

2. **Base de données** (7 tests)
   - Service PostgreSQL actif
   - Connexion réussie
   - Table EMPLOYEE existe
   - Schéma correct (4 colonnes)
   - Index présent
   - Données initiales présentes

3. **Fichiers COBOL** (7 tests)
   - 3 programmes .cbl présents
   - 3 fichiers COPY présents
   - DAL contient READ, SAVE, END
   - LOGIC contient calcul 0.7
   - Curseur C_EMP utilisé

4. **Compilation** (3 tests)
   - Script compile_all.sh exécutable
   - Compilation réussie
   - 3 exécutables générés

5. **Exécution** (6 tests)
   - Batch s'exécute sans erreur
   - **R1**: Calcul correct SALARY_NET = BRUT × 0.7
   - **R2**: Journalisation console
   - Tous les employés traités
   - Aucun salaire négatif
   - Affichage correct

6. **Conformité spec** (3 tests)
   - Architecture 3 couches
   - Convention nommage UPPER-KEBAB
   - Séparation stricte (DAL sans logique, LOGIC sans SQL)

**Résultat:**
```
==========================================
RESUME DES TESTS
==========================================
Tests executes:  30
Tests reussis:   30
Tests echoues:   0

==========================================
✓ TOUS LES TESTS SONT PASSES!
==========================================
Le projet est conforme a la spec salaire_net.yaml
```

## Commandes Make disponibles

```bash
make help        # Afficher l'aide
make install     # Installation complète (1ère fois)
make all         # setup + compile
make setup       # Initialiser la base seulement
make compile     # Compiler les programmes
make run         # Exécuter le batch
make test        # Tests complets de validation
make verify      # Vérifier les résultats
make clean       # Nettoyer les fichiers générés
make info        # Informations sur le projet
```

## Scripts disponibles

| Script | Description |
|--------|-------------|
| `install.sh` | Installation automatique complète |
| `sql/setup_db.sh` | Configuration de la base de données |
| `compile_all.sh` | Compilation des programmes COBOL |
| `test_all.sh` | Tests de validation (30 tests) |
| `verify_results.sh` | Vérification des résultats |

## En cas de problème

### Voir les logs

```bash
make show-logs
```

### Informations sur le projet

```bash
make info
```

### Réinstallation complète

```bash
make clean-all   # Supprime tout (y compris la DB)
make install     # Réinstalle tout
```

### Tests spécifiques

Pour tester uniquement la base de données:
```bash
cd sql
./setup_db.sh
```

Pour tester uniquement la compilation:
```bash
./compile_all.sh
```

## Structure finale après installation

```
out_test/
├── bin/                    # Programmes compilés
│   ├── EMPLOYEE-DAL-DB.so
│   ├── EMPLOYEE-BUSINESS.so
│   └── EMPLOYEE-LOGIC
├── business/               # Couche présentation
├── copy/                   # Structures COBOL partagées
├── dal/                    # Couche accès données
├── logic/                  # Couche orchestration
├── precompiled/            # Fichiers précompilés par ocesql
├── sql/                    # Scripts SQL
├── install.sh              # Installation auto (NOUVEAU)
├── compile_all.sh          # Script de compilation
├── test_all.sh             # Tests de validation (NOUVEAU)
├── verify_results.sh       # Vérification résultats
├── Makefile                # Automatisation
├── QUICKSTART.md           # Ce fichier
└── README.md               # Documentation complète
```

## Workflow complet

### Première installation

```bash
# 1. Installation complète
make install

# 2. Validation de l'installation
make test

# 3. Exécution du batch
make run

# 4. Vérification des résultats
make verify
```

### Utilisation quotidienne

```bash
# Exécuter le batch
make run

# Vérifier les résultats
make verify
```

### Développement

```bash
# Modifier les fichiers COBOL
vim logic/EMPLOYEE-LOGIC.cbl

# Recompiler
make compile

# Tester
make run
make verify
```

## Support

Pour plus de détails, consultez:
- [README.md](README.md) - Documentation complète
- [INDEX.txt](INDEX.txt) - Index des fichiers
- Logs: `/tmp/compile_output.log`, `/tmp/batch_output.log`

# 🚀 DÉMARRAGE RAPIDE - Projet COBOL Calcul Salaire Net

## Installation en 1 commande

```bash
make install
```

**Ce script fait TOUT automatiquement:**
- ✅ Installe PostgreSQL (si nécessaire)
- ✅ Installe GnuCOBOL (si nécessaire)
- ✅ Installe ocesql (si nécessaire)
- ✅ Crée la base de données empdb
- ✅ Compile les 3 programmes COBOL
- ✅ Prêt à exécuter!

## Validation de l'installation

```bash
make test
```

**30 tests automatiques** vérifient:
- Environnement (PostgreSQL, GnuCOBOL, ocesql)
- Base de données (tables, données, connexion)
- Programmes COBOL (présence, contenu)
- Compilation (réussie, exécutables générés)
- Exécution (batch fonctionne, calculs corrects)
- Conformité spec (architecture, règles métier)

**Résultat attendu:**
```
Tests executes:  30
Tests reussis:   30
Tests echoues:   0

✓ TOUS LES TESTS SONT PASSES!
Le projet est conforme a la spec salaire_net.yaml
```

## Exécution du batch

```bash
make run
```

**Sortie attendue:**
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

## Vérification des résultats

```bash
make verify
```

Affiche les données en base et vérifie que les calculs sont corrects.

---

## 📚 Documentation complète

| Document | Description |
|----------|-------------|
| [QUICKSTART.md](QUICKSTART.md) | Guide de démarrage détaillé |
| [README.md](README.md) | Documentation technique complète |
| [INDEX.txt](INDEX.txt) | Index de tous les fichiers |

## 🔧 Commandes Make

```bash
make help        # Liste toutes les commandes
make install     # Installation complète
make all         # Setup + compile
make run         # Exécuter le batch
make test        # Tests de validation (30 tests)
make verify      # Vérifier résultats
make info        # Infos sur le projet
make clean       # Nettoyer
```

## 📁 Structure du projet

```
out_test/
├── business/           # Couche BUSINESS (affichage)
│   └── EMPLOYEE-BUSINESS.cbl
├── dal/                # Couche DAL (base de données)
│   └── EMPLOYEE-DAL-DB.cbl
├── logic/              # Couche LOGIC (orchestration)
│   └── EMPLOYEE-LOGIC.cbl
├── copy/               # Structures COBOL partagées
├── sql/                # Scripts SQL et setup DB
├── install.sh          # Installation automatique
├── test_all.sh         # Tests de validation
└── Makefile            # Automatisation
```

## ✅ Conformité à la spécification

**Basé sur:** `salaire_net.yaml`

**Règles implémentées et testées:**
- ✅ **R1:** SALARY_NET = ROUND(SALARY_BRUT × 0.7, 2)
- ✅ **R2:** Batch avec journalisation console
- ✅ **R3:** Validation SALARY_BRUT
- ✅ Architecture 3 couches (DAL, LOGIC, BUSINESS)
- ✅ Connexion PostgreSQL
- ✅ Curseur SQL (C_EMP)
- ✅ Opérations READ, SAVE, END

## 🆘 Besoin d'aide?

### Problème d'installation
```bash
make show-logs    # Voir les logs d'erreur
make info         # État du projet
```

### Réinstallation complète
```bash
make clean-all    # Supprime tout
make install      # Réinstalle
```

### Tests spécifiques
```bash
cd sql && ./setup_db.sh     # Test DB seulement
./compile_all.sh            # Test compilation seulement
./test_all.sh               # Tous les tests
```

## 📊 Tests de validation (30 tests)

Le script `test_all.sh` vérifie **automatiquement**:

| Section | Tests | Vérifie |
|---------|-------|---------|
| 1. Environnement | 4 | PostgreSQL, GnuCOBOL, ocesql, libpq |
| 2. Base de données | 7 | Connexion, tables, schéma, index, données |
| 3. Fichiers COBOL | 7 | Programmes, COPY, contenu correct |
| 4. Compilation | 3 | Scripts, compilation, exécutables |
| 5. Exécution | 6 | Batch, calculs (R1), logs (R2), résultats |
| 6. Conformité | 3 | Architecture, nommage, séparation |

**Total:** 30 tests automatiques pour garantir la conformité à 100%

---

## 🎯 Workflow complet

### Première utilisation
```bash
make install    # ← Commence ici
make test       # Valide l'installation
make run        # Exécute le batch
make verify     # Vérifie les résultats
```

### Utilisation quotidienne
```bash
make run        # Exécute le batch
make verify     # Vérifie
```

### Développement
```bash
vim logic/EMPLOYEE-LOGIC.cbl    # Modifie
make compile                     # Recompile
make run                         # Teste
```

---

## 💡 Points clés

1. **Installation en 1 commande:** `make install`
2. **30 tests automatiques:** `make test`
3. **Conformité garantie:** Tous les tests passent = 100% conforme à la spec
4. **Documentation complète:** README.md, QUICKSTART.md, INDEX.txt
5. **Scripts robustes:** Gestion d'erreurs, logs détaillés, messages couleur

---

## 📝 Logs disponibles

| Log | Contenu |
|-----|---------|
| `/tmp/cobol_install.log` | Installation des outils |
| `/tmp/compile_output.log` | Compilation COBOL |
| `/tmp/batch_output.log` | Exécution du batch |
| `/tmp/create_tables.log` | Création tables SQL |
| `/tmp/insert_data.log` | Insertion données |

Voir tous les logs: `make show-logs`

---

**🎉 Prêt à commencer? Lancez `make install` !**

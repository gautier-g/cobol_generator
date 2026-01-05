# Projet COBOL - Calcul de Salaire Net

Ce projet implémente un système de calcul de salaire net en COBOL avec connexion à une base de données PostgreSQL.

## Vue d'ensemble

Le système est composé de 3 programmes COBOL organisés en couches:

```
EMPLOYEE-LOGIC (Programme principal)
    ├─> EMPLOYEE-DAL-DB (Accès base de données)
    └─> EMPLOYEE-BUSINESS (Affichage)
```

### Règle métier

**SALARY_NET = ROUND(SALARY_BRUT × 0.7, 2)**

Le salaire net représente le salaire brut moins 30% (20% de cotisations sociales + 10% d'impôt).

## Structure du projet

```
out_test/
├── business/               # Couche BUSINESS (Présentation)
│   └── EMPLOYEE-BUSINESS.cbl
├── copy/                   # Fichiers COPY (structures partagées)
│   ├── EMPLOYEE-RECORD.cpy
│   ├── SQLCA.cpy
│   └── STATUS-CODES.cpy
├── dal/                    # Couche DAL (Data Access Layer)
│   └── EMPLOYEE-DAL-DB.cbl
├── logic/                  # Couche LOGIC (Orchestration)
│   └── EMPLOYEE-LOGIC.cbl
├── sql/                    # Scripts SQL
│   ├── create_tables.sql
│   ├── insert_data.sql
│   └── setup_db.sh
├── compile_all.sh          # Script de compilation
└── README.md               # Ce fichier
```

## Description des programmes COBOL

### 1. EMPLOYEE-LOGIC (Couche LOGIC)

**Rôle:** Programme principal - Orchestration du traitement batch

**Responsabilités:**
- Boucle sur tous les employés de la base
- Appel de la DAL pour lire les données
- Calcul du salaire net (règle métier)
- Appel de la DAL pour sauvegarder
- Appel du module BUSINESS pour afficher
- Gestion du flag END-OF-FILE

**Point d'entrée:** `MAIN-PROCESS`

**Flux d'exécution:**
```cobol
MAIN-PROCESS
  └─> Boucle UNTIL END-OF-FILE = 'Y'
      ├─> CALL 'EMPLOYEE-DAL-DB' avec OPERATION='READ'
      ├─> PERFORM CALCULATE-NET
      ├─> CALL 'EMPLOYEE-DAL-DB' avec OPERATION='SAVE'
      ├─> CALL 'EMPLOYEE-BUSINESS'
      └─> CALL 'EMPLOYEE-DAL-DB' avec OPERATION='READ'
  └─> CALL 'EMPLOYEE-DAL-DB' avec OPERATION='END '
```

**Calcul du salaire net:**
```cobol
CALCULATE-NET.
    COMPUTE SALARY-NET ROUNDED = SALARY-BRUT * 0.7
```

### 2. EMPLOYEE-DAL-DB (Couche DAL)

**Rôle:** Couche d'accès aux données PostgreSQL

**Responsabilités:**
- Connexion paresseuse (lazy connection) à PostgreSQL
- Gestion du curseur SQL (C_EMP)
- Opérations CRUD sur la table EMPLOYEE
- Gestion des erreurs SQL

**Interface d'appel:**
```cobol
CALL 'EMPLOYEE-DAL-DB' USING OPERATION, END-OF-FILE, EMPLOYEE
```

**Opérations supportées:**

| Opération | Code | Description |
|-----------|------|-------------|
| Lecture | `'READ'` | FETCH du prochain employé via curseur |
| Sauvegarde | `'SAVE'` | UPDATE du SALARY_NET en base |
| Fin | `'END '` | CLOSE curseur, COMMIT, DISCONNECT |

**Connexion PostgreSQL:**
- Base: `empdb`
- Utilisateur: `empuser`
- Mot de passe: `SECRETPWD`

**Curseur SQL:**
```sql
DECLARE C_EMP CURSOR FOR
SELECT EMP_ID, EMP_NAME, SALARY_BRUT, SALARY_NET
FROM EMPLOYEE
ORDER BY EMP_ID
```

**Gestion des erreurs:**
- SQLCODE = 0 : Succès
- SQLCODE = 100 : Fin de curseur (END-OF-FILE = 'Y')
- SQLCODE != 0 et != 100 : Erreur SQL (affichage + END-OF-FILE = 'Y')

### 3. EMPLOYEE-BUSINESS (Couche BUSINESS)

**Rôle:** Présentation et affichage des données

**Responsabilités:**
- Affichage formaté des informations d'un employé
- Aucun accès base de données
- Aucune logique métier

**Interface d'appel:**
```cobol
CALL 'EMPLOYEE-BUSINESS' USING EMPLOYEE
```

**Format de sortie:**
```
----------------------------------------
EMPLOYE : Dupont
ID      : 0001
BRUT    : 00003000.00
NET     : 00002100.00
```

## Architecture en couches

Le système suit une architecture 3-tiers stricte:

```
┌─────────────────────────┐
│  EMPLOYEE-LOGIC         │  Couche LOGIC
│  (Orchestration)        │  - Boucle principale
│                         │  - Calcul métier
└────────┬───────┬────────┘  - Pas de SQL
         │       │
         │       └────────────────┐
         │                        │
┌────────▼────────────┐  ┌────────▼──────────────┐
│  EMPLOYEE-DAL-DB    │  │  EMPLOYEE-BUSINESS    │
│  (Accès données)    │  │  (Présentation)       │
│  - SQL uniquement   │  │  - Affichage          │
│  - CRUD operations  │  │  - Pas de SQL         │
└─────────────────────┘  └───────────────────────┘
         │
         │
┌────────▼────────────┐
│   PostgreSQL        │
│   (Base empdb)      │
│   Table: EMPLOYEE   │
└─────────────────────┘
```

### Principes de séparation:

- **LOGIC:** Aucun SQL, seulement logique métier et orchestration
- **DAL:** SQL uniquement, aucune logique métier
- **BUSINESS:** Présentation uniquement, ni SQL ni logique métier

## Communication entre programmes

### CALL avec paramètres partagés

Les programmes communiquent via `CALL ... USING`:

```cobol
01 EMPLOYEE.
    05 EMP-ID          PIC 9(4).
    05 EMP-NAME        PIC A(30).
    05 SALARY-BRUT     PIC 9(6)V99.
    05 SALARY-NET      PIC 9(6)V99.
```

Cette structure est passée par référence entre les programmes.

### Flag END-OF-FILE

```cobol
01 END-OF-FILE         PIC X VALUE 'N'.
```

- `'N'` = Données disponibles
- `'Y'` = Fin de données ou erreur

## Base de données PostgreSQL

### Schéma de la table EMPLOYEE

```sql
CREATE TABLE EMPLOYEE (
    EMP_ID INT PRIMARY KEY,
    EMP_NAME VARCHAR(30) NOT NULL,
    SALARY_BRUT DECIMAL(8,2) NOT NULL,
    SALARY_NET DECIMAL(8,2)
);
```

### Données initiales

| EMP_ID | EMP_NAME | SALARY_BRUT | SALARY_NET |
|--------|----------|-------------|------------|
| 1 | Dupont | 3000.00 | 0.00 |
| 2 | Durand | 1500.00 | 0.00 |

### Index

```sql
CREATE INDEX EMPLOYEE_EMP_NAME_IDX ON EMPLOYEE(EMP_NAME);
```

## Prérequis

### Logiciels requis

1. **PostgreSQL** (version 9.5+)
   ```bash
   sudo apt-get install postgresql postgresql-contrib
   ```

2. **GnuCOBOL** (version 3.0+)
   ```bash
   sudo apt-get install gnucobol
   ```

3. **ocesql** (Open COBOL ESQL Preprocessor)
   ```bash
   # Installation depuis les sources
   git clone https://github.com/opensourcecobol/Open-COBOL-ESQL.git
   cd Open-COBOL-ESQL
   ./configure
   make
   sudo make install
   ```

4. **PostgreSQL development libraries**
   ```bash
   sudo apt-get install libpq-dev
   ```

## Installation et utilisation

### Étape 1: Initialiser la base de données

```bash
cd sql
chmod +x setup_db.sh
./setup_db.sh
```

Ce script va:
- Créer l'utilisateur PostgreSQL `empuser`
- Créer la base de données `empdb`
- Créer la table `EMPLOYEE` avec son index
- Insérer les données initiales

### Étape 2: Compiler les programmes COBOL

```bash
cd ..  # Retour au répertoire out_test
chmod +x compile_all.sh
./compile_all.sh
```

Ce script va:
1. **Précompiler** les programmes avec `ocesql` (conversion SQL embedded → COBOL)
2. **Compiler** les programmes avec `cobc` (COBOL → modules exécutables)
3. Créer les exécutables dans le répertoire `bin/`

### Étape 3: Exécuter le batch

```bash
cd bin
export COB_LIBRARY_PATH=.
./EMPLOYEE-LOGIC
```

**Important:** Le `COB_LIBRARY_PATH` est nécessaire pour que EMPLOYEE-LOGIC puisse trouver et charger les modules EMPLOYEE-DAL-DB et EMPLOYEE-BUSINESS.

## Ce qui fonctionne (validé) et comment le refaire

### Résumé du flux qui marche

1. **Base PostgreSQL prête**  
   Création de l'utilisateur `empuser`, de la base `empdb`, de la table `EMPLOYEE`, et insertion des données de test.
2. **Compilation correcte**  
   `compile_all.sh` fait la précompilation SQL avec `ocesql` puis compile avec `cobc`:
   - `EMPLOYEE-DAL-DB.so` et `EMPLOYEE-BUSINESS.so` en modules partagés
   - `EMPLOYEE-LOGIC` en exécutable
   - lien explicite avec `libocesql` et `libpq`
   - `-fstatic-call` pour que les appels `OCESQL*` soient résolus statiquement
3. **Exécution du batch**  
   `EMPLOYEE-LOGIC` boucle sur les employés via le curseur SQL, calcule `SALARY_NET = SALARY_BRUT * 0.7`, met à jour la base, puis affiche chaque ligne.

### Refaire depuis zéro (copier/coller)

```bash
cd /home/mfabre/pi/cobol_generator/try/out_test

# Option A: script rapide (nécessite sudo)
./setup_db_quick.sh

# Option B: script détaillé
# cd sql && ./setup_db.sh && cd ..

./compile_all.sh
cd bin
export COB_LIBRARY_PATH=.
./EMPLOYEE-LOGIC
```

Validation complète:

```bash
cd /home/mfabre/pi/cobol_generator/try/out_test
./test_all.sh
```

### Comment ça marche exactement (exécution)

- `EMPLOYEE-LOGIC` appelle `EMPLOYEE-DAL-DB` avec `READ` pour ouvrir le curseur et récupérer un employé.
- La logique calcule `SALARY_NET` et appelle `EMPLOYEE-DAL-DB` avec `SAVE` pour faire l'UPDATE.
- `EMPLOYEE-BUSINESS` affiche les valeurs.
- Quand il n'y a plus d'enregistrements, `EMPLOYEE-LOGIC` appelle `END` pour fermer/commit/déconnecter.

### Connexion PostgreSQL (ocesql)

La DAL utilise le runtime ocesql avec:
- paramètres `empdb / empuser / SECRETPWD`
- variables d'environnement `PGHOST=localhost` et `PGPORT=5432` pour forcer TCP

En cas d'erreur de connexion, regarder `/tmp/ocesql.log` pour le détail.

## Exemple de sortie

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

Après exécution du batch, vérifier que les salaires nets sont correctement calculés:

```bash
psql -U empuser -d empdb -c "SELECT * FROM EMPLOYEE ORDER BY EMP_ID;"
```

Résultat attendu:

```
 emp_id | emp_name  | salary_brut | salary_net
--------+-----------+-------------+------------
      1 | Dupont    |     3000.00 |    2100.00
      2 | Durand    |     1500.00 |    1050.00
```

**Vérification du calcul:**
- Dupont: 3000.00 × 0.7 = 2100.00 ✓
- Durand: 1500.00 × 0.7 = 1050.00 ✓

## Détails du script de compilation

### compile_all.sh

Le script effectue les étapes suivantes pour chaque programme:

1. **Précompilation avec ocesql:**
   ```bash
   ocesql --inc=./copy \
          ./dal/EMPLOYEE-DAL-DB.cbl \
          ./precompiled/EMPLOYEE-DAL-DB.cob
   ```

   Convertit les instructions `EXEC SQL ... END-EXEC` en appels COBOL.

2. **Compilation avec cobc:**
   ```bash
   cobc -m -fstatic-call -I ./copy \
        -I ./precompiled \
        -o ./bin/EMPLOYEE-DAL-DB.so \
        ./precompiled/EMPLOYEE-DAL-DB.cob \
        -L/usr/local/lib \
        -locesql \
        -L/usr/lib/postgresql \
        -lpq \
        -std=default \
        -free
   ```

   Options:
   - `-m` : Génère un module dynamique (.so)
   - `-fstatic-call` : Force les appels OCESQL* en statique
   - `-x` : Génère un exécutable (utilisé pour EMPLOYEE-LOGIC)
   - `-I` : Répertoires pour les fichiers COPY
   - `-L` : Répertoire des bibliothèques PostgreSQL
   - `-lpq` : Lien avec libpq (client PostgreSQL)
   - `-free` : Format COBOL libre

### Ordre de compilation

L'ordre est important car EMPLOYEE-LOGIC dépend des deux autres modules:

1. EMPLOYEE-DAL-DB (aucune dépendance)
2. EMPLOYEE-BUSINESS (aucune dépendance)
3. EMPLOYEE-LOGIC (dépend de DAL-DB et BUSINESS)

## Dépannage

### Erreur: "ocesql: command not found"

```bash
# Vérifier l'installation
which ocesql

# Si non trouvé, installer ocesql
# Voir section "Prérequis"
```

### Erreur: "CONNECT failed: SQLCODE=-1"

Vérifier:
1. PostgreSQL est démarré: `sudo systemctl status postgresql`
2. La base empdb existe: `psql -U postgres -c "\l" | grep empdb`
3. L'utilisateur empuser existe: `psql -U postgres -c "\du" | grep empuser`
4. Les identifiants dans EMPLOYEE-DAL-DB.cbl sont corrects

### Erreur: "SQLCODE=-402" / "SQLSTATE=08001"

Le runtime ocesql ne peut pas se connecter. Vérifier:
1. Forcer TCP si le socket local est bloqué: `PGHOST=localhost PGPORT=5432`
2. Détails de l'erreur: `/tmp/ocesql.log`

### Erreur: "error while loading shared libraries: EMPLOYEE-DAL-DB.so"

```bash
# S'assurer que COB_LIBRARY_PATH est défini
export COB_LIBRARY_PATH=./bin

# Ou lancer depuis le répertoire bin
cd bin
export COB_LIBRARY_PATH=.
./EMPLOYEE-LOGIC
```

### Erreur de compilation: "libpq.so not found"

```bash
# Installer les bibliothèques de développement PostgreSQL
sudo apt-get install libpq-dev

# Vérifier l'emplacement de libpq
find /usr -name "libpq.so" 2>/dev/null
```

## Modification des données de test

Pour tester avec d'autres employés:

1. Éditer `sql/insert_data.sql`
2. Ajouter/modifier les lignes `INSERT INTO EMPLOYEE`
3. Relancer le script:
   ```bash
   cd sql
   PGPASSWORD=SECRETPWD psql -h localhost -U empuser -d empdb -f insert_data.sql
   ```

## Extension du système

### Ajouter un nouveau champ

1. Modifier le schéma SQL dans `sql/create_tables.sql`
2. Mettre à jour la structure dans `copy/EMPLOYEE-RECORD.cpy`
3. Modifier le SELECT dans `dal/EMPLOYEE-DAL-DB.cbl`
4. Recompiler tous les programmes

### Modifier la règle de calcul

Éditer `logic/EMPLOYEE-LOGIC.cbl`, paragraphe `CALCULATE-NET`:

```cobol
CALCULATE-NET.
    COMPUTE SALARY-NET ROUNDED = SALARY-BRUT * 0.65
    * Nouveau taux: 35% de déductions
```

Puis recompiler:
```bash
./compile_all.sh
```

## Conformité à la spécification

Ce projet implémente strictement la spécification `salaire_net.yaml`:

- ✓ **R1:** SALARY_NET = ROUND(SALARY_BRUT * 0.7, 2)
- ✓ **R2:** Traitement batch avec journalisation console
- ✓ **R3:** Validation de SALARY_BRUT (gestion des valeurs invalides)
- ✓ **F1:** Calcul du salaire net pour chaque employé
- ✓ Architecture 3 couches (DAL, LOGIC, BUSINESS)
- ✓ Connexion PostgreSQL
- ✓ Curseur SQL pour parcours des employés
- ✓ Opérations READ, SAVE, END sur la DAL

## Licence

Ce projet est généré à partir de la spécification `salaire_net.yaml` et utilise GnuCOBOL et PostgreSQL.

## Support

Pour toute question ou problème:
1. Vérifier la section "Dépannage"
2. Consulter les logs PostgreSQL: `/var/log/postgresql/`
3. Vérifier les fichiers précompilés dans `precompiled/` pour déboguer les erreurs SQL

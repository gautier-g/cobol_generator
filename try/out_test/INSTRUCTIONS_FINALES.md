# Instructions finales - Configuration complète

## ✅ Corrections appliquées

1. **ocesql syntaxe** → ✅ Corrigée dans `compile_all.sh`
2. **Connexion DB** → ✅ Corrigée dans `dal/EMPLOYEE-DAL-DB.cbl` (appel direct `OCESQLConnect`)

## 🚀 Étapes à suivre (copier-coller chaque commande)

### Étape 1: Créer la base PostgreSQL

**Copiez-collez dans votre terminal:**

```bash
cd /home/mfabre/pi/cobol_generator/try/out_test/sql
```

Puis:

```bash
sudo -u postgres psql << 'SQL'
DROP DATABASE IF EXISTS empdb;
DROP USER IF EXISTS empuser;
CREATE USER empuser WITH PASSWORD 'SECRETPWD';
CREATE DATABASE empdb OWNER empuser;
GRANT ALL PRIVILEGES ON DATABASE empdb TO empuser;
SQL
```

### Étape 2: Créer les tables

```bash
PGPASSWORD=SECRETPWD psql -h localhost -U empuser -d empdb -f create_tables.sql
```

### Étape 3: Insérer les données

```bash
PGPASSWORD=SECRETPWD psql -h localhost -U empuser -d empdb -f insert_data.sql
```

### Étape 4: Vérifier

```bash
PGPASSWORD=SECRETPWD psql -h localhost -U empuser -d empdb -c "SELECT * FROM employee ORDER BY emp_id;"
```

Vous devriez voir:

```
 emp_id | emp_name  | salary_brut | salary_net
--------+-----------+-------------+------------
      1 | Dupont    |     3000.00 |       0.00
      2 | Durand    |     1500.00 |       0.00
```

### Étape 5: Compiler les programmes COBOL

```bash
cd /home/mfabre/pi/cobol_generator/try/out_test
./compile_all.sh
```

**Résultat attendu:**

```
==========================================
Compilation des programmes COBOL
==========================================
Verification des outils...
Outils detectes: ocesql et cobc

Compilation de EMPLOYEE-DAL-DB (DAL)...
  [1/3] Precompilation SQL (ocesql)...
  [2/3] Compilation COBOL (cobc)...
  [3/3] EMPLOYEE-DAL-DB compile avec succes

Compilation de EMPLOYEE-BUSINESS (BUSINESS)...
  [1/3] Precompilation SQL (ocesql)...
  [2/3] Compilation COBOL (cobc)...
  [3/3] EMPLOYEE-BUSINESS compile avec succes

Compilation de EMPLOYEE-LOGIC (LOGIC)...
  [1/3] Precompilation SQL (ocesql)...
  [2/3] Compilation COBOL (cobc)...
  [3/3] EMPLOYEE-LOGIC compile avec succes

==========================================
COMPILATION REUSSIE
==========================================
```

### Étape 6: Exécuter le batch

```bash
cd bin
export COB_LIBRARY_PATH=.
./EMPLOYEE-LOGIC
```

**Résultat attendu:**

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

### Étape 7: Vérifier les résultats

```bash
cd ..
PGPASSWORD=SECRETPWD psql -h localhost -U empuser -d empdb -c "SELECT emp_id, emp_name, salary_brut, salary_net FROM employee ORDER BY emp_id;"
```

Vous devriez voir les salaires nets calculés:

```
 emp_id | emp_name  | salary_brut | salary_net
--------+-----------+-------------+------------
      1 | Dupont    |     3000.00 |    2100.00
      2 | Durand    |     1500.00 |    1050.00
```

### Étape 8: Tests de validation

```bash
make test
```

**Résultat attendu:** 30/30 tests passés ✅

---

## 📝 Résumé des corrections

| Fichier | Problème | Correction |
|---------|----------|-----------|
| `compile_all.sh` | Syntaxe ocesql `-I` `-o` | Changé en `--inc=` ✅ |
| `dal/EMPLOYEE-DAL-DB.cbl` | `CONNECT TO` en erreur de précompilation | Connexion via `OCESQLConnect` ✅ |

---

## ⚠️ Notes importantes

### Limitations ocesql

ocesql ne supporte pas:
- `CONNECT TO` dans ce contexte → Utiliser `OCESQLConnect`
- Syntaxe GNU-style (`-I`, `-o`) → Utiliser `--inc=`

### Configuration PostgreSQL

Les paramètres de connexion sont **en dur** dans le code COBOL:
- Database: `empdb`
- User: `empuser`
- Password: `SECRETPWD`

Pour changer ces valeurs, modifier `dal/EMPLOYEE-DAL-DB.cbl` (variables `WS-DB-*`)

---

## 🎯 Commandes tout-en-un

Si vous préférez tout faire d'un coup (après avoir créé la base):

```bash
# 1. Créer la base (nécessite sudo interactif)
cd /home/mfabre/pi/cobol_generator/try/out_test/sql
sudo -u postgres psql << 'SQL'
DROP DATABASE IF EXISTS empdb;
DROP USER IF EXISTS empuser;
CREATE USER empuser WITH PASSWORD 'SECRETPWD';
CREATE DATABASE empdb OWNER empuser;
GRANT ALL PRIVILEGES ON DATABASE empdb TO empuser;
SQL

# 2. Tables, données, compilation et exécution
PGPASSWORD=SECRETPWD psql -h localhost -U empuser -d empdb -f create_tables.sql && \
PGPASSWORD=SECRETPWD psql -h localhost -U empuser -d empdb -f insert_data.sql && \
cd .. && \
./compile_all.sh && \
cd bin && \
COB_LIBRARY_PATH=. ./EMPLOYEE-LOGIC && \
cd .. && \
make verify
```

---

## 🆘 En cas de problème

### Erreur de compilation ocesql

Si vous voyez `syntax error` lors de la précompilation:
- Vérifiez que vous avez bien les dernières versions des fichiers
- Les corrections ont été appliquées à `EMPLOYEE-DAL-DB.cbl`

### Erreur PostgreSQL "relation does not exist"

```bash
# Vérifier que les tables existent
PGPASSWORD=SECRETPWD psql -h localhost -U empuser -d empdb -c "\dt"
```

Si aucune table, relancer l'étape 2 (create_tables.sql)

### Voir les logs

```bash
make show-logs
```

---

## ✨ Tout fonctionne?

Si tout s'est bien passé:
- ✅ Base créée
- ✅ Tables créées
- ✅ Données insérées
- ✅ 3 programmes compilés
- ✅ Batch exécuté
- ✅ Calculs corrects (Dupont: 2100, Durand: 1050)
- ✅ 30 tests passés

**Félicitations! Le projet est 100% fonctionnel et conforme à la spec `salaire_net.yaml`** 🎉

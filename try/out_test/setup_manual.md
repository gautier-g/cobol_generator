# Configuration manuelle de PostgreSQL

Le script automatique nécessite sudo. Voici les étapes manuelles:

## Étape 1: Créer l'utilisateur et la base

Exécutez cette commande dans votre terminal:

```bash
sudo -u postgres psql << 'SQL'
-- Supprimer si existe (optionnel)
DROP DATABASE IF EXISTS empdb;
DROP USER IF EXISTS empuser;

-- Créer l'utilisateur
CREATE USER empuser WITH PASSWORD 'SECRETPWD';

-- Créer la base
CREATE DATABASE empdb OWNER empuser;

-- Donner les privilèges
GRANT ALL PRIVILEGES ON DATABASE empdb TO empuser;

-- Vérifier
\l empdb
\du empuser
SQL
```

## Étape 2: Créer les tables

```bash
cd /home/mfabre/pi/cobol_generator/try/out_test/sql
PGPASSWORD=SECRETPWD psql -h localhost -U empuser -d empdb -f create_tables.sql
```

## Étape 3: Insérer les données

```bash
PGPASSWORD=SECRETPWD psql -h localhost -U empuser -d empdb -f insert_data.sql
```

## Étape 4: Vérifier

```bash
PGPASSWORD=SECRETPWD psql -h localhost -U empuser -d empdb -c "SELECT * FROM employee;"
```

## Commande tout-en-un

```bash
# Copier-coller cette commande complète:
cd /home/mfabre/pi/cobol_generator/try/out_test/sql && \
sudo -u postgres psql << 'SQL'
DROP DATABASE IF EXISTS empdb;
DROP USER IF EXISTS empuser;
CREATE USER empuser WITH PASSWORD 'SECRETPWD';
CREATE DATABASE empdb OWNER empuser;
GRANT ALL PRIVILEGES ON DATABASE empdb TO empuser;
SQL
 && \
PGPASSWORD=SECRETPWD psql -h localhost -U empuser -d empdb -f create_tables.sql && \
PGPASSWORD=SECRETPWD psql -h localhost -U empuser -d empdb -f insert_data.sql && \
PGPASSWORD=SECRETPWD psql -h localhost -U empuser -d empdb -c "SELECT * FROM employee ORDER BY emp_id;" && \
echo "✓ Base de données configurée avec succès!"
```

## Après la configuration

Une fois la base configurée, retournez au répertoire principal et compilez:

```bash
cd /home/mfabre/pi/cobol_generator/try/out_test
./compile_all.sh
```

Puis testez:

```bash
make test
make run
```

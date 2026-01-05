#!/bin/bash
#########################################################################
# setup_db_sudo.sh - Setup DB en mode sudo (pas de prompt interactif)  #
#                                                                       #
# Usage: sudo ./setup_db_sudo.sh                                        #
#########################################################################

# Verifier si execute avec sudo
if [ "$EUID" -ne 0 ]; then
    echo "ERREUR: Ce script doit etre execute avec sudo"
    echo "Usage: sudo ./setup_db_sudo.sh"
    exit 1
fi

# Configuration
DB_NAME="empdb"
DB_USER="empuser"
DB_PASSWORD="SECRETPWD"
DB_HOST="localhost"
DB_PORT="5432"

# Couleurs
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

echo "=========================================="
echo "Configuration PostgreSQL (mode sudo)"
echo "=========================================="
echo ""

echo -e "${BLUE}[1/4] Creation utilisateur et base${NC}"

# Executer en tant qu'utilisateur postgres
sudo -u postgres psql << SQL
-- Supprimer si existe
DROP DATABASE IF EXISTS $DB_NAME;
DROP USER IF EXISTS $DB_USER;

-- Creer l'utilisateur
CREATE USER $DB_USER WITH PASSWORD '$DB_PASSWORD';

-- Creer la base
CREATE DATABASE $DB_NAME OWNER $DB_USER;

-- Donner les privileges
GRANT ALL PRIVILEGES ON DATABASE $DB_NAME TO $DB_USER;
SQL

if [ $? -eq 0 ]; then
    echo -e "${GREEN}✓ Utilisateur et base crees${NC}"
else
    echo "ERREUR lors de la creation"
    exit 1
fi

echo ""
echo -e "${BLUE}[2/4] Creation des tables${NC}"

# Trouver l'utilisateur reel (celui qui a lance sudo)
REAL_USER=${SUDO_USER:-$USER}

# Changer au repertoire du script
cd "$(dirname "$0")"

# Creer les tables
sudo -u "$REAL_USER" PGPASSWORD=$DB_PASSWORD psql -h $DB_HOST -U $DB_USER -d $DB_NAME -f create_tables.sql > /dev/null 2>&1

if [ $? -eq 0 ]; then
    echo -e "${GREEN}✓ Tables creees${NC}"
else
    echo "ERREUR lors de la creation des tables"
    exit 1
fi

echo ""
echo -e "${BLUE}[3/4] Insertion des donnees${NC}"

sudo -u "$REAL_USER" PGPASSWORD=$DB_PASSWORD psql -h $DB_HOST -U $DB_USER -d $DB_NAME -f insert_data.sql > /dev/null 2>&1

if [ $? -eq 0 ]; then
    echo -e "${GREEN}✓ Donnees inserees${NC}"
else
    echo "ERREUR lors de l'insertion"
    exit 1
fi

echo ""
echo -e "${BLUE}[4/4] Verification${NC}"

echo "Contenu de la table EMPLOYEE:"
sudo -u "$REAL_USER" PGPASSWORD=$DB_PASSWORD psql -h $DB_HOST -U $DB_USER -d $DB_NAME -c "SELECT emp_id, emp_name, salary_brut, salary_net FROM employee ORDER BY emp_id;"

echo ""
echo "=========================================="
echo -e "${GREEN}✓ Configuration terminee avec succes!${NC}"
echo "=========================================="
echo ""
echo "Base de donnees: $DB_NAME"
echo "Utilisateur: $DB_USER"
echo "Employes: $(sudo -u "$REAL_USER" PGPASSWORD=$DB_PASSWORD psql -h $DB_HOST -U $DB_USER -d $DB_NAME -tAc "SELECT COUNT(*) FROM employee;")"
echo ""
echo "Prochaine etape:"
echo "  cd .."
echo "  ./compile_all.sh"
echo "=========================================="

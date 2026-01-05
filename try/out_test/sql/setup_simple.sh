#!/bin/bash
#########################################################################
# setup_simple.sh - Configuration simple PostgreSQL                     #
#########################################################################

DB_NAME="empdb"
DB_USER="empuser"
DB_PASSWORD="SECRETPWD"

GREEN='\033[0;32m'
BLUE='\033[0;34m'
NC='\033[0m'

echo "=========================================="
echo "Configuration PostgreSQL (mode simple)"
echo "=========================================="
echo ""

echo -e "${BLUE}[1/3] Creation utilisateur et base${NC}"
sudo -u postgres psql << 'SQL'
DROP DATABASE IF EXISTS empdb;
DROP USER IF EXISTS empuser;
CREATE USER empuser WITH PASSWORD 'SECRETPWD';
CREATE DATABASE empdb OWNER empuser;
GRANT ALL PRIVILEGES ON DATABASE empdb TO empuser;
SQL

if [ $? -ne 0 ]; then
    echo "ERREUR lors de la creation"
    exit 1
fi
echo -e "${GREEN}✓ Base et utilisateur crees${NC}"

echo ""
echo -e "${BLUE}[2/3] Creation des tables et insertion des donnees${NC}"
PGPASSWORD=$DB_PASSWORD psql -h localhost -U $DB_USER -d $DB_NAME -f create_tables.sql > /dev/null 2>&1
PGPASSWORD=$DB_PASSWORD psql -h localhost -U $DB_USER -d $DB_NAME -f insert_data.sql > /dev/null 2>&1

if [ $? -eq 0 ]; then
    echo -e "${GREEN}✓ Tables creees et donnees inserees${NC}"
else
    echo "ERREUR lors de la creation des tables"
    exit 1
fi

echo ""
echo -e "${BLUE}[3/3] Verification${NC}"
echo "Contenu de la table EMPLOYEE:"
PGPASSWORD=$DB_PASSWORD psql -h localhost -U $DB_USER -d $DB_NAME -c "SELECT emp_id, emp_name, salary_brut, salary_net FROM employee ORDER BY emp_id;"

echo ""
echo "=========================================="
echo -e "${GREEN}✓ Configuration terminee!${NC}"
echo "=========================================="
echo ""
echo "Prochaine etape:"
echo "  cd .."
echo "  ./compile_all.sh"
echo "=========================================="

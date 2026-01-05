#!/bin/bash
#########################################################################
# setup_db.sh - Script d'initialisation complete de la base de donnees #
#                                                                       #
# Ce script cree la base empdb avec tests de validation                #
#########################################################################

# Configuration
DB_NAME="empdb"
DB_USER="empuser"
DB_PASSWORD="SECRETPWD"
DB_HOST="localhost"
DB_PORT="5432"

# Couleurs
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

echo "=========================================="
echo "Initialisation de la base de donnees"
echo "=========================================="
echo ""

#########################################################################
# ETAPE 1: VERIFICATION DES PREREQUIS
#########################################################################
echo -e "${BLUE}[ETAPE 1/6] Verification des prerequis${NC}"

# Verifier que PostgreSQL est installe
if ! command -v psql &> /dev/null; then
    echo -e "${RED}ERREUR: PostgreSQL (psql) n'est pas installe${NC}"
    echo "Installez PostgreSQL avec:"
    echo "  sudo apt-get update"
    echo "  sudo apt-get install postgresql postgresql-contrib"
    exit 1
fi
echo -e "${GREEN}✓${NC} PostgreSQL est installe"

# Verifier que PostgreSQL est actif
if ! sudo systemctl is-active --quiet postgresql 2>/dev/null && ! pgrep -x postgres > /dev/null; then
    echo -e "${YELLOW}! PostgreSQL n'est pas actif, tentative de demarrage...${NC}"
    sudo systemctl start postgresql
    sleep 2

    if ! sudo systemctl is-active --quiet postgresql 2>/dev/null && ! pgrep -x postgres > /dev/null; then
        echo -e "${RED}ERREUR: Impossible de demarrer PostgreSQL${NC}"
        echo "Demarrez PostgreSQL manuellement avec:"
        echo "  sudo systemctl start postgresql"
        exit 1
    fi
fi
echo -e "${GREEN}✓${NC} Service PostgreSQL est actif"

echo ""

#########################################################################
# ETAPE 2: CREATION DE L'UTILISATEUR
#########################################################################
echo -e "${BLUE}[ETAPE 2/6] Creation de l'utilisateur PostgreSQL${NC}"

# Verifier si l'utilisateur existe deja
USER_EXISTS=$(sudo -u postgres psql -tAc "SELECT 1 FROM pg_roles WHERE rolname='$DB_USER';" 2>/dev/null)

if [ "$USER_EXISTS" = "1" ]; then
    echo -e "${YELLOW}! Utilisateur $DB_USER existe deja${NC}"
    echo "  Reconfiguration du mot de passe..."
    sudo -u postgres psql -c "ALTER USER $DB_USER WITH PASSWORD '$DB_PASSWORD';" 2>/dev/null
else
    echo "  Creation de l'utilisateur $DB_USER..."
    ERROR_MSG=$(sudo -u postgres psql -c "CREATE USER $DB_USER WITH PASSWORD '$DB_PASSWORD';" 2>&1)

    if [ $? -eq 0 ]; then
        echo -e "${GREEN}✓${NC} Utilisateur $DB_USER cree"
    else
        echo -e "${RED}ERREUR: Impossible de creer l'utilisateur${NC}"
        echo "  Details: $ERROR_MSG"

        # Verifier si l'erreur est due au fait que l'utilisateur existe deja
        if echo "$ERROR_MSG" | grep -q "already exists"; then
            echo -e "${YELLOW}! Utilisateur existe deja, reconfiguration...${NC}"
            sudo -u postgres psql -c "ALTER USER $DB_USER WITH PASSWORD '$DB_PASSWORD';" 2>/dev/null
        else
            exit 1
        fi
    fi
fi

# Octroyer les privileges de creation de base
sudo -u postgres psql -c "ALTER USER $DB_USER CREATEDB;" 2>/dev/null

echo -e "${GREEN}✓${NC} Utilisateur $DB_USER configure"
echo ""

#########################################################################
# ETAPE 3: CREATION DE LA BASE DE DONNEES
#########################################################################
echo -e "${BLUE}[ETAPE 3/6] Creation de la base de donnees${NC}"

# Verifier si la base existe deja
DB_EXISTS=$(sudo -u postgres psql -tAc "SELECT 1 FROM pg_database WHERE datname='$DB_NAME';" 2>/dev/null)

if [ "$DB_EXISTS" = "1" ]; then
    echo -e "${YELLOW}! Base $DB_NAME existe deja${NC}"
    echo "  Voulez-vous la supprimer et la recreer? (y/N)"
    read -r response
    if [[ "$response" =~ ^[Yy]$ ]]; then
        echo "  Suppression de la base $DB_NAME..."

        # Terminer toutes les connexions actives
        sudo -u postgres psql -c "SELECT pg_terminate_backend(pid) FROM pg_stat_activity WHERE datname='$DB_NAME';" 2>/dev/null

        sudo -u postgres psql -c "DROP DATABASE $DB_NAME;" 2>/dev/null
        echo "  Base supprimee"

        echo "  Recreation de la base $DB_NAME..."
        sudo -u postgres psql -c "CREATE DATABASE $DB_NAME OWNER $DB_USER;" 2>/dev/null

        if [ $? -eq 0 ]; then
            echo -e "${GREEN}✓${NC} Base $DB_NAME recreee"
        else
            echo -e "${RED}ERREUR: Impossible de recreer la base${NC}"
            exit 1
        fi
    else
        echo "  Conservation de la base existante"
    fi
else
    echo "  Creation de la base $DB_NAME..."
    sudo -u postgres psql -c "CREATE DATABASE $DB_NAME OWNER $DB_USER;" 2>/dev/null

    if [ $? -eq 0 ]; then
        echo -e "${GREEN}✓${NC} Base $DB_NAME creee"
    else
        echo -e "${RED}ERREUR: Impossible de creer la base${NC}"
        exit 1
    fi
fi

# Octroyer tous les privileges
sudo -u postgres psql -c "GRANT ALL PRIVILEGES ON DATABASE $DB_NAME TO $DB_USER;" 2>/dev/null
echo -e "${GREEN}✓${NC} Privileges octroyes a $DB_USER"

echo ""

#########################################################################
# ETAPE 4: TEST DE CONNEXION
#########################################################################
echo -e "${BLUE}[ETAPE 4/6] Test de connexion a la base${NC}"

# Tester la connexion
if PGPASSWORD=$DB_PASSWORD psql -h $DB_HOST -p $DB_PORT -U $DB_USER -d $DB_NAME -c "SELECT 1;" &> /dev/null; then
    echo -e "${GREEN}✓${NC} Connexion a la base $DB_NAME reussie"
    echo "  Host: $DB_HOST:$DB_PORT"
    echo "  User: $DB_USER"
    echo "  Database: $DB_NAME"
else
    echo -e "${RED}ERREUR: Impossible de se connecter a la base${NC}"
    echo "Verifiez les parametres de connexion:"
    echo "  Host: $DB_HOST"
    echo "  Port: $DB_PORT"
    echo "  User: $DB_USER"
    echo "  Database: $DB_NAME"
    exit 1
fi

echo ""

#########################################################################
# ETAPE 5: CREATION DES TABLES
#########################################################################
echo -e "${BLUE}[ETAPE 5/6] Creation des tables${NC}"

if [ ! -f "create_tables.sql" ]; then
    echo -e "${RED}ERREUR: Fichier create_tables.sql introuvable${NC}"
    exit 1
fi

echo "  Execution de create_tables.sql..."
PGPASSWORD=$DB_PASSWORD psql -h $DB_HOST -p $DB_PORT -U $DB_USER -d $DB_NAME -f create_tables.sql > /tmp/create_tables.log 2>&1

if [ $? -eq 0 ]; then
    echo -e "${GREEN}✓${NC} Tables creees avec succes"
else
    echo -e "${RED}ERREUR lors de la creation des tables${NC}"
    echo "Voir les details: /tmp/create_tables.log"
    exit 1
fi

# Verification que la table existe
TABLE_EXISTS=$(PGPASSWORD=$DB_PASSWORD psql -h $DB_HOST -p $DB_PORT -U $DB_USER -d $DB_NAME -tAc "SELECT COUNT(*) FROM information_schema.tables WHERE table_name='employee';")

if [ "$TABLE_EXISTS" = "1" ]; then
    echo -e "${GREEN}✓${NC} Table EMPLOYEE existe"

    # Afficher la structure de la table
    echo ""
    echo "  Structure de la table EMPLOYEE:"
    PGPASSWORD=$DB_PASSWORD psql -h $DB_HOST -p $DB_PORT -U $DB_USER -d $DB_NAME -c "\d employee"
else
    echo -e "${RED}ERREUR: Table EMPLOYEE n'a pas ete creee${NC}"
    exit 1
fi

# Verifier l'index
INDEX_EXISTS=$(PGPASSWORD=$DB_PASSWORD psql -h $DB_HOST -p $DB_PORT -U $DB_USER -d $DB_NAME -tAc "SELECT COUNT(*) FROM pg_indexes WHERE tablename='employee' AND indexname='employee_emp_name_idx';")

if [ "$INDEX_EXISTS" = "1" ]; then
    echo -e "${GREEN}✓${NC} Index EMPLOYEE_EMP_NAME_IDX existe"
else
    echo -e "${YELLOW}! Index EMPLOYEE_EMP_NAME_IDX non trouve${NC}"
fi

echo ""

#########################################################################
# ETAPE 6: INSERTION DES DONNEES
#########################################################################
echo -e "${BLUE}[ETAPE 6/6] Insertion des donnees initiales${NC}"

if [ ! -f "insert_data.sql" ]; then
    echo -e "${RED}ERREUR: Fichier insert_data.sql introuvable${NC}"
    exit 1
fi

echo "  Execution de insert_data.sql..."
PGPASSWORD=$DB_PASSWORD psql -h $DB_HOST -p $DB_PORT -U $DB_USER -d $DB_NAME -f insert_data.sql > /tmp/insert_data.log 2>&1

if [ $? -eq 0 ]; then
    echo -e "${GREEN}✓${NC} Donnees inserees avec succes"
else
    echo -e "${RED}ERREUR lors de l'insertion des donnees${NC}"
    echo "Voir les details: /tmp/insert_data.log"
    exit 1
fi

# Verification du nombre d'enregistrements
ROW_COUNT=$(PGPASSWORD=$DB_PASSWORD psql -h $DB_HOST -p $DB_PORT -U $DB_USER -d $DB_NAME -tAc "SELECT COUNT(*) FROM employee;")

echo -e "${GREEN}✓${NC} Nombre d'employes inseres: $ROW_COUNT"

# Affichage des donnees
echo ""
echo "  Donnees inserees:"
PGPASSWORD=$DB_PASSWORD psql -h $DB_HOST -p $DB_PORT -U $DB_USER -d $DB_NAME -c "SELECT emp_id, emp_name, salary_brut, salary_net FROM employee ORDER BY emp_id;"

echo ""

#########################################################################
# VALIDATION FINALE
#########################################################################
echo "=========================================="
echo -e "${GREEN}INITIALISATION TERMINEE AVEC SUCCES${NC}"
echo "=========================================="
echo ""
echo "Resume de la configuration:"
echo "  Base de donnees: $DB_NAME"
echo "  Utilisateur: $DB_USER"
echo "  Host: $DB_HOST:$DB_PORT"
echo "  Table: EMPLOYEE (4 colonnes)"
echo "  Enregistrements: $ROW_COUNT employes"
echo ""
echo "Parametres de connexion pour COBOL:"
echo "  DB_NAME: $DB_NAME"
echo "  DB_USER: $DB_USER"
echo "  DB_PASSWORD: $DB_PASSWORD"
echo ""
echo "Prochaines etapes:"
echo "  1. Retour au repertoire principal: cd .."
echo "  2. Compiler les programmes: ./compile_all.sh"
echo "  3. Executer le batch: cd bin && COB_LIBRARY_PATH=. ./EMPLOYEE-LOGIC"
echo "=========================================="

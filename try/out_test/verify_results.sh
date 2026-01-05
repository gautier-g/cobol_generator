#!/bin/bash
#########################################################################
# verify_results.sh - Script de verification des resultats             #
#                                                                       #
# Ce script verifie que les calculs ont bien ete effectues             #
#########################################################################

# Always run from the script directory
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
cd "$SCRIPT_DIR"

DB_NAME="empdb"
DB_USER="empuser"
DB_PASSWORD="SECRETPWD"

echo "=========================================="
echo "Verification des resultats"
echo "=========================================="

# Verification que la base existe
if ! PGPASSWORD=$DB_PASSWORD psql -h localhost -U $DB_USER -d $DB_NAME -c "SELECT 1;" &> /dev/null; then
    echo "ERREUR: Impossible de se connecter a la base $DB_NAME"
    echo "Lancez d'abord: cd sql && ./setup_db.sh"
    exit 1
fi

echo "Contenu de la table EMPLOYEE:"
echo ""

PGPASSWORD=$DB_PASSWORD psql -h localhost -U $DB_USER -d $DB_NAME -c "
    SELECT
        EMP_ID as \"ID\",
        EMP_NAME as \"Nom\",
        SALARY_BRUT as \"Salaire Brut\",
        SALARY_NET as \"Salaire Net\",
        CASE
            WHEN SALARY_NET = 0 THEN 'NON CALCULE'
            WHEN SALARY_NET = ROUND(SALARY_BRUT * 0.7, 2) THEN 'OK'
            ELSE 'ERREUR'
        END as \"Statut\"
    FROM EMPLOYEE
    ORDER BY EMP_ID;
"

echo ""
echo "Formule appliquee: SALARY_NET = ROUND(SALARY_BRUT * 0.7, 2)"
echo "=========================================="

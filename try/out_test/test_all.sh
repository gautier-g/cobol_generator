#!/bin/bash
#########################################################################
# test_all.sh - Script de test complet du projet                       #
#                                                                       #
# Ce script verifie:                                                    #
# - Connexion PostgreSQL                                                #
# - Tables creees correctement                                          #
# - Donnees inserees                                                    #
# - Programmes compiles                                                 #
# - Execution du batch                                                  #
# - Conformite aux exigences de la spec                                 #
#########################################################################

# Always run from the script directory
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
cd "$SCRIPT_DIR"

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
NC='\033[0m' # No Color

# Compteurs
TESTS_PASSED=0
TESTS_FAILED=0
TESTS_TOTAL=0

# Fonction pour afficher le resultat d'un test
test_result() {
    local test_name=$1
    local result=$2
    TESTS_TOTAL=$((TESTS_TOTAL + 1))

    if [ $result -eq 0 ]; then
        echo -e "${GREEN}✓${NC} Test $TESTS_TOTAL: $test_name"
        TESTS_PASSED=$((TESTS_PASSED + 1))
    else
        echo -e "${RED}✗${NC} Test $TESTS_TOTAL: $test_name"
        TESTS_FAILED=$((TESTS_FAILED + 1))
    fi
}

# Fonction pour executer une requete SQL
run_sql() {
    PGPASSWORD=$DB_PASSWORD psql -h $DB_HOST -p $DB_PORT -U $DB_USER -d $DB_NAME -tAc "$1" 2>/dev/null
}

# Fonction pour executer une commande SQL (erreur si echec)
run_sql_cmd() {
    PGPASSWORD=$DB_PASSWORD psql -h $DB_HOST -p $DB_PORT -U $DB_USER -d $DB_NAME -v ON_ERROR_STOP=1 -c "$1" > /dev/null 2>&1
}

# Sauvegarde/restauration de la table employee pour tests de scenarios
BACKUP_TABLE="employee_backup_test_$$"
BACKUP_CREATED=0

backup_employee_table() {
    if run_sql_cmd "CREATE TABLE $BACKUP_TABLE AS TABLE employee;"; then
        BACKUP_CREATED=1
    else
        BACKUP_CREATED=0
    fi
}

restore_employee_table() {
    if [ "$BACKUP_CREATED" = "1" ]; then
        run_sql_cmd "TRUNCATE employee;"
        run_sql_cmd "INSERT INTO employee SELECT * FROM $BACKUP_TABLE ORDER BY emp_id;"
        run_sql_cmd "DROP TABLE $BACKUP_TABLE;"
        BACKUP_CREATED=0
    fi
}

cleanup() {
    restore_employee_table
}

trap cleanup EXIT

echo "=========================================="
echo "TESTS DE VALIDATION DU PROJET COBOL"
echo "Specification: salaire_net.yaml"
echo "=========================================="
echo ""

#########################################################################
# SECTION 1: VERIFICATION DE L'ENVIRONNEMENT
#########################################################################
echo -e "${BLUE}[SECTION 1] Verification de l'environnement${NC}"

# Test 1: PostgreSQL installe
if command -v psql &> /dev/null; then
    test_result "PostgreSQL (psql) est installe" 0
    PSQL_VERSION=$(psql --version | head -n1)
    echo "  Version: $PSQL_VERSION"
else
    test_result "PostgreSQL (psql) est installe" 1
    echo -e "${RED}  ERREUR: Installez PostgreSQL: sudo apt-get install postgresql${NC}"
fi

# Test 2: GnuCOBOL installe
if command -v cobc &> /dev/null; then
    test_result "GnuCOBOL (cobc) est installe" 0
    COBC_VERSION=$(cobc --version | head -n1)
    echo "  Version: $COBC_VERSION"
else
    test_result "GnuCOBOL (cobc) est installe" 1
    echo -e "${RED}  ERREUR: Installez GnuCOBOL: sudo apt-get install gnucobol${NC}"
fi

# Test 3: ocesql installe
if command -v ocesql &> /dev/null; then
    test_result "ocesql est installe" 0
    OCESQL_VERSION=$(ocesql --version 2>&1 | head -n1)
    echo "  Version: $OCESQL_VERSION"
else
    test_result "ocesql est installe" 1
    echo -e "${RED}  ERREUR: Installez ocesql depuis https://github.com/opensourcecobol/Open-COBOL-ESQL${NC}"
fi

# Test 4: libpq (PostgreSQL client library)
if ldconfig -p | grep -q libpq.so; then
    test_result "libpq (PostgreSQL client library) est installee" 0
else
    test_result "libpq (PostgreSQL client library) est installee" 1
    echo -e "${RED}  ERREUR: Installez libpq-dev: sudo apt-get install libpq-dev${NC}"
fi

echo ""

#########################################################################
# SECTION 2: VERIFICATION DE LA BASE DE DONNEES
#########################################################################
echo -e "${BLUE}[SECTION 2] Verification de la base de donnees${NC}"

# Test 5: Service PostgreSQL actif
if sudo systemctl is-active --quiet postgresql 2>/dev/null || pgrep -x postgres > /dev/null; then
    test_result "Service PostgreSQL est actif" 0
else
    test_result "Service PostgreSQL est actif" 1
    echo -e "${RED}  ERREUR: Demarrez PostgreSQL: sudo systemctl start postgresql${NC}"
fi

# Test 6: Connexion a PostgreSQL possible
if PGPASSWORD=$DB_PASSWORD psql -h $DB_HOST -p $DB_PORT -U $DB_USER -d $DB_NAME -c "SELECT 1;" &> /dev/null; then
    test_result "Connexion a la base $DB_NAME reussie" 0
else
    test_result "Connexion a la base $DB_NAME reussie" 1
    echo -e "${YELLOW}  INFO: Lancez d'abord: cd sql && ./setup_db.sh${NC}"
fi

# Test 7: Table EMPLOYEE existe
TABLE_EXISTS=$(run_sql "SELECT COUNT(*) FROM information_schema.tables WHERE table_name='employee';")
if [ "$TABLE_EXISTS" = "1" ]; then
    test_result "Table EMPLOYEE existe" 0
else
    test_result "Table EMPLOYEE existe" 1
fi

# Test 8: Schema de la table EMPLOYEE correct
if [ "$TABLE_EXISTS" = "1" ]; then
    COLUMNS=$(run_sql "SELECT COUNT(*) FROM information_schema.columns WHERE table_name='employee';")
    if [ "$COLUMNS" = "4" ]; then
        test_result "Table EMPLOYEE a 4 colonnes (EMP_ID, EMP_NAME, SALARY_BRUT, SALARY_NET)" 0

        # Verification des types de colonnes
        EMP_ID_TYPE=$(run_sql "SELECT data_type FROM information_schema.columns WHERE table_name='employee' AND column_name='emp_id';")
        EMP_NAME_TYPE=$(run_sql "SELECT data_type FROM information_schema.columns WHERE table_name='employee' AND column_name='emp_name';")
        SALARY_BRUT_TYPE=$(run_sql "SELECT data_type FROM information_schema.columns WHERE table_name='employee' AND column_name='salary_brut';")
        SALARY_NET_TYPE=$(run_sql "SELECT data_type FROM information_schema.columns WHERE table_name='employee' AND column_name='salary_net';")

        echo "  EMP_ID: $EMP_ID_TYPE"
        echo "  EMP_NAME: $EMP_NAME_TYPE"
        echo "  SALARY_BRUT: $SALARY_BRUT_TYPE"
        echo "  SALARY_NET: $SALARY_NET_TYPE"
    else
        test_result "Table EMPLOYEE a 4 colonnes" 1
    fi
fi

# Test 9: Index sur EMP_NAME existe
INDEX_EXISTS=$(run_sql "SELECT COUNT(*) FROM pg_indexes WHERE tablename='employee' AND indexname='employee_emp_name_idx';")
if [ "$INDEX_EXISTS" = "1" ]; then
    test_result "Index EMPLOYEE_EMP_NAME_IDX existe" 0
else
    test_result "Index EMPLOYEE_EMP_NAME_IDX existe" 1
fi

# Test 10: Donnees initiales presentes
if [ "$TABLE_EXISTS" = "1" ]; then
    ROW_COUNT=$(run_sql "SELECT COUNT(*) FROM employee;")
    if [ "$ROW_COUNT" -ge "2" ]; then
        test_result "Donnees initiales presentes (au moins 2 employes)" 0
        echo "  Nombre d'employes: $ROW_COUNT"
    else
        test_result "Donnees initiales presentes" 1
    fi
fi

# Test 11: Donnees attendues (Dupont et Durand)
DUPONT=$(run_sql "SELECT COUNT(*) FROM employee WHERE emp_name='Dupont' AND salary_brut=3000.00;")
DURAND=$(run_sql "SELECT COUNT(*) FROM employee WHERE emp_name='Durand' AND salary_brut=1500.00;")
if [ "$DUPONT" = "1" ] && [ "$DURAND" = "1" ]; then
    test_result "Donnees de test correctes (Dupont 3000, Durand 1500)" 0
else
    test_result "Donnees de test correctes" 1
fi

echo ""

#########################################################################
# SECTION 3: VERIFICATION DES FICHIERS COBOL
#########################################################################
echo -e "${BLUE}[SECTION 3] Verification des fichiers COBOL${NC}"

# Test 12: Fichier EMPLOYEE-DAL-DB.cbl existe
if [ -f "dal/EMPLOYEE-DAL-DB.cbl" ]; then
    test_result "Fichier dal/EMPLOYEE-DAL-DB.cbl existe" 0
    LINES=$(wc -l < dal/EMPLOYEE-DAL-DB.cbl)
    echo "  Lignes: $LINES"
else
    test_result "Fichier dal/EMPLOYEE-DAL-DB.cbl existe" 1
fi

# Test 13: Fichier EMPLOYEE-LOGIC.cbl existe
if [ -f "logic/EMPLOYEE-LOGIC.cbl" ]; then
    test_result "Fichier logic/EMPLOYEE-LOGIC.cbl existe" 0
    LINES=$(wc -l < logic/EMPLOYEE-LOGIC.cbl)
    echo "  Lignes: $LINES"
else
    test_result "Fichier logic/EMPLOYEE-LOGIC.cbl existe" 1
fi

# Test 14: Fichier EMPLOYEE-BUSINESS.cbl existe
if [ -f "business/EMPLOYEE-BUSINESS.cbl" ]; then
    test_result "Fichier business/EMPLOYEE-BUSINESS.cbl existe" 0
    LINES=$(wc -l < business/EMPLOYEE-BUSINESS.cbl)
    echo "  Lignes: $LINES"
else
    test_result "Fichier business/EMPLOYEE-BUSINESS.cbl existe" 1
fi

# Test 15: Fichiers COPY existent
if [ -f "copy/EMPLOYEE-RECORD.cpy" ] && [ -f "copy/SQLCA.cpy" ] && [ -f "copy/STATUS-CODES.cpy" ]; then
    test_result "Fichiers COPY presents (EMPLOYEE-RECORD, SQLCA, STATUS-CODES)" 0
else
    test_result "Fichiers COPY presents" 1
fi

# Test 16: DAL contient operations READ, SAVE, END
if [ -f "dal/EMPLOYEE-DAL-DB.cbl" ]; then
    if grep -q "DAL-READ" dal/EMPLOYEE-DAL-DB.cbl && \
       grep -q "DAL-SAVE" dal/EMPLOYEE-DAL-DB.cbl && \
       grep -q "DAL-END" dal/EMPLOYEE-DAL-DB.cbl; then
        test_result "DAL contient operations READ, SAVE, END" 0
    else
        test_result "DAL contient operations READ, SAVE, END" 1
    fi
fi

# Test 17: LOGIC contient le calcul SALARY_NET
if [ -f "logic/EMPLOYEE-LOGIC.cbl" ]; then
    if grep -q "CALCULATE-NET" logic/EMPLOYEE-LOGIC.cbl && \
       grep -q "0.7" logic/EMPLOYEE-LOGIC.cbl; then
        test_result "LOGIC contient le calcul SALARY_NET = BRUT * 0.7" 0
    else
        test_result "LOGIC contient le calcul SALARY_NET = BRUT * 0.7" 1
    fi
fi

# Test 18: DAL utilise curseur C_EMP
if [ -f "dal/EMPLOYEE-DAL-DB.cbl" ]; then
    if grep -q "C_EMP" dal/EMPLOYEE-DAL-DB.cbl; then
        test_result "DAL utilise le curseur C_EMP" 0
    else
        test_result "DAL utilise le curseur C_EMP" 1
    fi
fi

echo ""

#########################################################################
# SECTION 4: COMPILATION DES PROGRAMMES
#########################################################################
echo -e "${BLUE}[SECTION 4] Compilation des programmes${NC}"

# Test 19: Script compile_all.sh existe et est executable
if [ -x "compile_all.sh" ]; then
    test_result "Script compile_all.sh est executable" 0
else
    test_result "Script compile_all.sh est executable" 1
fi

# Test 20: Compilation reussie
if [ -x "compile_all.sh" ]; then
    echo "  Lancement de la compilation..."
    if ./compile_all.sh > /tmp/compile_output.log 2>&1; then
        test_result "Compilation des 3 programmes reussie" 0
    else
        test_result "Compilation des 3 programmes reussie" 1
        echo -e "${YELLOW}  Voir les erreurs: /tmp/compile_output.log${NC}"
    fi
fi

# Test 21: Executables generes
if [ -f "bin/EMPLOYEE-DAL-DB.so" ] && \
   [ -f "bin/EMPLOYEE-BUSINESS.so" ] && \
   [ -f "bin/EMPLOYEE-LOGIC" ]; then
    test_result "Executables generes (DAL, BUSINESS, LOGIC)" 0
    echo "  EMPLOYEE-DAL-DB.so: $(ls -lh bin/EMPLOYEE-DAL-DB.so | awk '{print $5}')"
    echo "  EMPLOYEE-BUSINESS.so: $(ls -lh bin/EMPLOYEE-BUSINESS.so | awk '{print $5}')"
    echo "  EMPLOYEE-LOGIC: $(ls -lh bin/EMPLOYEE-LOGIC | awk '{print $5}')"
else
    test_result "Executables generes" 1
fi

echo ""

#########################################################################
# SECTION 5: EXECUTION DU BATCH ET TESTS FONCTIONNELS
#########################################################################
echo -e "${BLUE}[SECTION 5] Execution du batch et tests fonctionnels${NC}"

# Reinitialiser les donnees avant le test
echo "  Reinitialisation des donnees de test..."
PGPASSWORD=$DB_PASSWORD psql -h $DB_HOST -p $DB_PORT -U $DB_USER -d $DB_NAME -c "UPDATE employee SET salary_net = 0;" &> /dev/null

# Test 22: Execution du programme principal
if [ -f "bin/EMPLOYEE-LOGIC" ]; then
    echo "  Execution du batch EMPLOYEE-LOGIC..."
    cd bin
    if COB_LIBRARY_PATH=. ./EMPLOYEE-LOGIC > /tmp/batch_output.log 2>&1; then
        test_result "Execution du batch EMPLOYEE-LOGIC reussie" 0
        cd ..
    else
        test_result "Execution du batch EMPLOYEE-LOGIC reussie" 1
        echo -e "${YELLOW}  Voir les erreurs: /tmp/batch_output.log${NC}"
        cd ..
    fi
else
    test_result "Execution du batch EMPLOYEE-LOGIC reussie" 1
    echo -e "${YELLOW}  Programme non compile${NC}"
fi

# Test 23: Verification R1 - Calcul correct du salaire net
DUPONT_NET=$(run_sql "SELECT salary_net FROM employee WHERE emp_name='Dupont';")
DURAND_NET=$(run_sql "SELECT salary_net FROM employee WHERE emp_name='Durand';")

DUPONT_EXPECTED="2100.00"
DURAND_EXPECTED="1050.00"

if [ "${DUPONT_NET// /}" = "$DUPONT_EXPECTED" ] && [ "${DURAND_NET// /}" = "$DURAND_EXPECTED" ]; then
    test_result "R1: Calcul correct SALARY_NET = ROUND(BRUT * 0.7, 2)" 0
    echo "  Dupont: 3000.00 * 0.7 = $DUPONT_NET (attendu: $DUPONT_EXPECTED)"
    echo "  Durand: 1500.00 * 0.7 = $DURAND_NET (attendu: $DURAND_EXPECTED)"
else
    test_result "R1: Calcul correct SALARY_NET = ROUND(BRUT * 0.7, 2)" 1
    echo "  Dupont: obtenu=$DUPONT_NET, attendu=$DUPONT_EXPECTED"
    echo "  Durand: obtenu=$DURAND_NET, attendu=$DURAND_EXPECTED"
fi

# Test 24: Verification R2 - Journalisation console
if [ -f "/tmp/batch_output.log" ]; then
    if grep -q "DEBUT TRAITEMENT BATCH" /tmp/batch_output.log && \
       grep -q "FIN TRAITEMENT BATCH" /tmp/batch_output.log; then
        test_result "R2: Journalisation console presente" 0
    else
        test_result "R2: Journalisation console presente" 1
    fi
fi

# Test 25: Tous les employes traites
ALL_UPDATED=$(run_sql "SELECT COUNT(*) FROM employee WHERE salary_net > 0;")
TOTAL_EMP=$(run_sql "SELECT COUNT(*) FROM employee;")
if [ "$ALL_UPDATED" = "$TOTAL_EMP" ]; then
    test_result "Tous les employes ont ete traites" 0
    echo "  Employes traites: $ALL_UPDATED / $TOTAL_EMP"
else
    test_result "Tous les employes ont ete traites" 1
    echo "  Employes traites: $ALL_UPDATED / $TOTAL_EMP"
fi

# Test 26: Verification que SALARY_NET >= 0 (R1)
NEGATIVE=$(run_sql "SELECT COUNT(*) FROM employee WHERE salary_net < 0;")
if [ "$NEGATIVE" = "0" ]; then
    test_result "R1: Aucun salaire net negatif" 0
else
    test_result "R1: Aucun salaire net negatif" 1
    echo -e "${RED}  ERREUR: $NEGATIVE employes ont un salaire net negatif${NC}"
fi

# Test 27: Affichage des employes
if [ -f "/tmp/batch_output.log" ]; then
    if grep -q "EMPLOYE" /tmp/batch_output.log && \
       grep -q "BRUT" /tmp/batch_output.log && \
       grep -q "NET" /tmp/batch_output.log; then
        test_result "Affichage des informations employes" 0
    else
        test_result "Affichage des informations employes" 1
    fi
fi

# Test 27B: Journalisation DAL/LOGIC/BUSINESS complete
if [ -f "/tmp/batch_output.log" ]; then
    MISSING_LOG=0
    for MSG in "Connexion DB reussie: empdb" "Curseur C_EMP ouvert" "Fermeture connexion"; do
        if ! grep -q "$MSG" /tmp/batch_output.log; then
            MISSING_LOG=1
            echo -e "${YELLOW}  Log manquant: $MSG${NC}"
        fi
    done
    if [ "$MISSING_LOG" -eq 0 ]; then
        test_result "R2: Journalisation DAL/LOGIC/BUSINESS complete" 0
    else
        test_result "R2: Journalisation DAL/LOGIC/BUSINESS complete" 1
    fi
fi

# Test 27C: Affichage exact des 2 employes (Dupont/Durand)
if [ -f "/tmp/batch_output.log" ]; then
    if grep -q "ID      : 0001" /tmp/batch_output.log && \
       grep -q "ID      : 0002" /tmp/batch_output.log && \
       grep -q "NET     : +002100.00" /tmp/batch_output.log && \
       grep -q "NET     : +001050.00" /tmp/batch_output.log; then
        test_result "Affichage exact Dupont/Durand" 0
    else
        test_result "Affichage exact Dupont/Durand" 1
        echo -e "${YELLOW}  Voir: /tmp/batch_output.log${NC}"
    fi
fi

echo ""

#########################################################################
# SECTION 5B: TESTS DE BRANCHES ET SCENARIOS
#########################################################################
echo -e "${BLUE}[SECTION 5B] Tests de branches et scenarios${NC}"

if [ -f "bin/EMPLOYEE-LOGIC" ] && [ "$TABLE_EXISTS" = "1" ]; then
    backup_employee_table
    if [ "$BACKUP_CREATED" = "1" ]; then
        # Scenario 1: table vide -> EOF direct
        run_sql_cmd "TRUNCATE employee;"
        if (cd bin && COB_LIBRARY_PATH=. ./EMPLOYEE-LOGIC > /tmp/batch_output_empty.log 2>&1); then
            if grep -q "Nombre employes traites: 0000" /tmp/batch_output_empty.log; then
                test_result "Scenario table vide: batch reussit et traite 0 employe" 0
            else
                test_result "Scenario table vide: batch reussit et traite 0 employe" 1
                echo -e "${YELLOW}  Voir: /tmp/batch_output_empty.log${NC}"
            fi
        else
            test_result "Scenario table vide: batch reussit et traite 0 employe" 1
            echo -e "${YELLOW}  Voir: /tmp/batch_output_empty.log${NC}"
        fi

        # Scenario 2: salaire_brut non numerique -> refuse par schema
        INVALID_LOG="/tmp/batch_output_invalid_salary.log"
        if PGPASSWORD=$DB_PASSWORD psql -h $DB_HOST -p $DB_PORT -U $DB_USER -d $DB_NAME \
            -c "INSERT INTO employee (emp_id, emp_name, salary_brut, salary_net) VALUES (99, 'Invalide', 'ABC', 0);" \
            > "$INVALID_LOG" 2>&1; then
            test_result "Scenario salary_brut non numerique refuse par schema" 1
            echo -e "${YELLOW}  ERREUR: insertion invalide acceptee${NC}"
        else
            test_result "Scenario salary_brut non numerique refuse par schema" 0
        fi

        # Scenario 3: salaire negatif -> correction du salaire net
        run_sql_cmd "TRUNCATE employee;"
        run_sql_cmd "INSERT INTO employee (emp_id, emp_name, salary_brut, salary_net) VALUES (1, 'Negatif', -100.00, 0.00), (2, 'Normal', 1000.00, 0.00);"
        if (cd bin && COB_LIBRARY_PATH=. ./EMPLOYEE-LOGIC > /tmp/batch_output_negative.log 2>&1); then
            NEG_MSG=$(grep -c "ANOMALIE: Salaire net negatif corrige" /tmp/batch_output_negative.log 2>/dev/null || true)
            NEG_NET=$(run_sql "SELECT salary_net FROM employee WHERE emp_id = 1;")
            if [ "$NEG_MSG" -ge "1" ] && [ "${NEG_NET// /}" = "0.00" ]; then
                test_result "Scenario salaire negatif: correction appliquee" 0
            else
                test_result "Scenario salaire negatif: correction appliquee" 1
                echo -e "${YELLOW}  Log: /tmp/batch_output_negative.log${NC}"
                echo "  salary_net emp_id=1: $NEG_NET"
            fi
        else
            test_result "Scenario salaire negatif: correction appliquee" 1
            echo -e "${YELLOW}  Log: /tmp/batch_output_negative.log${NC}"
        fi

        # Verifier l'affichage anomalie dans les logs
        if [ -f "/tmp/batch_output_negative.log" ] && \
           grep -q "ANOMALIE: Salaire net negatif corrige" /tmp/batch_output_negative.log && \
           grep -q "EMP_ID=0001" /tmp/batch_output_negative.log && \
           grep -q "NET     : +000000.00" /tmp/batch_output_negative.log; then
            test_result "Scenario salaire negatif: affichage anomalie" 0
        else
            test_result "Scenario salaire negatif: affichage anomalie" 1
            echo -e "${YELLOW}  Voir: /tmp/batch_output_negative.log${NC}"
        fi

        # Scenario 4: 10 employes -> verification complete des affichages et calculs
        run_sql_cmd "TRUNCATE employee;"
        run_sql_cmd "INSERT INTO employee (emp_id, emp_name, salary_brut, salary_net) VALUES \
            (1, 'Emp01', 1000.00, 0.00), \
            (2, 'Emp02', 1100.00, 0.00), \
            (3, 'Emp03', 1200.00, 0.00), \
            (4, 'Emp04', 1300.00, 0.00), \
            (5, 'Emp05', 1400.00, 0.00), \
            (6, 'Emp06', 1500.00, 0.00), \
            (7, 'Emp07', 1600.00, 0.00), \
            (8, 'Emp08', 1700.00, 0.00), \
            (9, 'Emp09', 1800.00, 0.00), \
            (10, 'Emp10', 1900.00, 0.00);"
        if (cd bin && COB_LIBRARY_PATH=. ./EMPLOYEE-LOGIC > /tmp/batch_output_10.log 2>&1); then
            EMP_COUNT=$(grep -c "EMPLOYE :" /tmp/batch_output_10.log 2>/dev/null || true)
            ID_COUNT=$(grep -c "ID      :" /tmp/batch_output_10.log 2>/dev/null || true)
            BRUT_COUNT=$(grep -c "BRUT    :" /tmp/batch_output_10.log 2>/dev/null || true)
            NET_COUNT=$(grep -c "NET     :" /tmp/batch_output_10.log 2>/dev/null || true)
            MISMATCH=$(run_sql "SELECT COUNT(*) FROM employee WHERE salary_net <> ROUND(salary_brut * 0.7, 2);")
            if [ "$EMP_COUNT" = "10" ] && [ "$ID_COUNT" = "10" ] && \
               [ "$BRUT_COUNT" = "10" ] && [ "$NET_COUNT" = "10" ] && \
               [ "$MISMATCH" = "0" ] && \
               grep -q "Nombre employes traites: 0010" /tmp/batch_output_10.log; then
                test_result "Scenario 10 employes: affichages et calculs complets" 0
            else
                test_result "Scenario 10 employes: affichages et calculs complets" 1
                echo -e "${YELLOW}  Log: /tmp/batch_output_10.log${NC}"
                echo "  EMPLOYE=$EMP_COUNT ID=$ID_COUNT BRUT=$BRUT_COUNT NET=$NET_COUNT mismatch=$MISMATCH"
            fi
        else
            test_result "Scenario 10 employes: affichages et calculs complets" 1
            echo -e "${YELLOW}  Log: /tmp/batch_output_10.log${NC}"
        fi

        # Restaurer les donnees initiales apres scenarios
        restore_employee_table
        RESTORED_COUNT=$(run_sql "SELECT COUNT(*) FROM employee;")
        RESTORED_REF=$(run_sql "SELECT COUNT(*) FROM employee WHERE emp_name IN ('Dupont','Durand');")
        if [ "$RESTORED_COUNT" -ge "2" ] && [ "$RESTORED_REF" = "2" ]; then
            test_result "Donnees initiales restaurees apres scenarios" 0
        else
            test_result "Donnees initiales restaurees apres scenarios" 1
            echo -e "${YELLOW}  Nombre employes: $RESTORED_COUNT${NC}"
        fi
    else
        echo -e "${YELLOW}  INFO: Sauvegarde employee impossible, tests de scenarios ignores${NC}"
    fi
else
    echo -e "${YELLOW}  INFO: Tests de scenarios ignores (table EMPLOYEE ou binaire manquant)${NC}"
fi

echo ""

#########################################################################
# SECTION 5C: LOGS D'EXECUTION
#########################################################################
echo -e "${BLUE}[SECTION 5C] Logs d'execution${NC}"

print_log() {
    local log_file=$1
    if [ -f "$log_file" ]; then
        local lines
        lines=$(wc -l < "$log_file" 2>/dev/null || echo "0")
        echo "------------------------------------------"
        echo "Log: $log_file ($lines lignes)"
        echo "------------------------------------------"
        if [ "$lines" -le 200 ]; then
            cat "$log_file"
        else
            tail -n 200 "$log_file"
        fi
        echo ""
    else
        echo "Log manquant: $log_file"
    fi
}

print_log "/tmp/batch_output.log"
print_log "/tmp/batch_output_empty.log"
print_log "/tmp/batch_output_negative.log"
print_log "/tmp/batch_output_invalid_salary.log"
print_log "/tmp/batch_output_10.log"

echo ""

#########################################################################
# SECTION 6: VERIFICATION DE LA CONFORMITE A LA SPEC
#########################################################################
echo -e "${BLUE}[SECTION 6] Verification de la conformite a la spec${NC}"

# Test 28: Architecture 3 couches presente
if [ -d "dal" ] && [ -d "logic" ] && [ -d "business" ]; then
    test_result "Architecture 3 couches (DAL, LOGIC, BUSINESS)" 0
else
    test_result "Architecture 3 couches" 1
fi

# Test 29: Convention de nommage UPPER-KEBAB pour programmes
if [ -f "dal/EMPLOYEE-DAL-DB.cbl" ] && \
   [ -f "logic/EMPLOYEE-LOGIC.cbl" ] && \
   [ -f "business/EMPLOYEE-BUSINESS.cbl" ]; then
    test_result "Convention nommage programmes: UPPER-KEBAB" 0
else
    test_result "Convention nommage programmes: UPPER-KEBAB" 1
fi

# Test 30: Separation stricte des responsabilites
DAL_NO_LOGIC=$(grep -c "COMPUTE" dal/EMPLOYEE-DAL-DB.cbl 2>/dev/null || true)
LOGIC_NO_SQL=$(grep -c "EXEC SQL" logic/EMPLOYEE-LOGIC.cbl 2>/dev/null || true)

if [ "$DAL_NO_LOGIC" = "0" ] && [ "$LOGIC_NO_SQL" = "0" ]; then
    test_result "Separation stricte: DAL sans logique, LOGIC sans SQL" 0
else
    test_result "Separation stricte: DAL sans logique, LOGIC sans SQL" 1
fi

echo ""

#########################################################################
# RESUME FINAL
#########################################################################
echo "=========================================="
echo -e "${BLUE}RESUME DES TESTS${NC}"
echo "=========================================="
echo -e "Tests executes:  ${BLUE}$TESTS_TOTAL${NC}"
echo -e "Tests reussis:   ${GREEN}$TESTS_PASSED${NC}"
echo -e "Tests echoues:   ${RED}$TESTS_FAILED${NC}"
echo ""

if [ $TESTS_FAILED -eq 0 ]; then
    echo -e "${GREEN}=========================================="
    echo "✓ TOUS LES TESTS SONT PASSES!"
    echo "=========================================="
    echo "Le projet est conforme a la spec salaire_net.yaml"
    echo -e "${NC}"
    exit 0
else
    echo -e "${RED}=========================================="
    echo "✗ CERTAINS TESTS ONT ECHOUE"
    echo "=========================================="
    echo "Verifiez les erreurs ci-dessus"
    echo -e "${NC}"

    # Suggestions
    echo ""
    echo "Suggestions:"
    if ! command -v ocesql &> /dev/null; then
        echo "  - Installez ocesql: voir README.md section Prerequisites"
    fi
    if ! PGPASSWORD=$DB_PASSWORD psql -h $DB_HOST -p $DB_PORT -U $DB_USER -d $DB_NAME -c "SELECT 1;" &> /dev/null; then
        echo "  - Initialisez la base: cd sql && ./setup_db.sh"
    fi
    if [ ! -f "bin/EMPLOYEE-LOGIC" ]; then
        echo "  - Compilez les programmes: ./compile_all.sh"
    fi

    exit 1
fi

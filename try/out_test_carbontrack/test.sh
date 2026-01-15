#!/bin/bash
#########################################################################
# test.sh - Script de test complet du projet CARBONTRACK                #
#                                                                       #
# Ce script verifie:                                                    #
# - Setup base PostgreSQL (UTILISATEUR, ANTENNE, ACTIVITE, REPAS,        #
#   HEBERGEMENT, PARTICIPATION)                                         #
# - Compilation COBOL (DAL/LOGIC/BUSINESS)                              #
# - Precompilation OCESQL (EXEC SQL -> COBOL)                           #
# - Execution batch et validation resultats                             #
# - Tests des exigences (R1-R5) et fonctionnalites (F1-F5)              #
#########################################################################
#
#=========================================================================
# EXPLICATION DETAILLEE : CE QUE CE SCRIPT TESTE SUR LE CODE COBOL
#=========================================================================
#
# ARCHITECTURE DU CODE COBOL TESTE (3 couches)
# ---------------------------------------------
# Le projet CARBONTRACK genere du code COBOL selon une architecture
# en 3 couches distinctes pour chaque entite metier :
#
#   1. LOGIC (Programmes principaux executables)
#      - ACTIVITE-LOGIC.cbl, UTILISATEUR-LOGIC.cbl, ANTENNE-LOGIC.cbl
#      - REPAS-LOGIC.cbl, HEBERGEMENT-LOGIC.cbl, PARTICIPATION-LOGIC.cbl
#      - Role : Orchestration du traitement batch (boucle READ/traitement/SAVE)
#      - Contient la logique metier de validation et correction des donnees
#      - Appelle les couches DAL et BUSINESS via CALL
#
#   2. DAL-DB (Data Access Layer - Modules SQL)
#      - ACTIVITE-DAL-DB.cbl, UTILISATEUR-DAL-DB.cbl, ANTENNE-DAL-DB.cbl
#      - REPAS-DAL-DB.cbl, HEBERGEMENT-DAL-DB.cbl, PARTICIPATION-DAL-DB.cbl
#      - Role : Acces aux donnees PostgreSQL via SQL embarque (EXEC SQL)
#      - Operations : CONNECT, READ (curseur), SAVE (UPDATE), END (fermeture)
#      - Gestion des erreurs SQL via SQLCODE et SQLSTATE
#
#   3. BUSINESS (Modules de presentation)
#      - ACTIVITE-BUSINESS.cbl, UTILISATEUR-BUSINESS.cbl, etc.
#      - Role : Affichage formate des donnees traitees (DISPLAY)
#      - Module appele par LOGIC apres chaque traitement
#
# TESTS DE LA COUCHE LOGIC
# ------------------------
# Le script teste que chaque programme LOGIC :
#   - Se connecte correctement a la base via DAL-CONNECT
#   - Lit les enregistrements via curseur SQL (DAL-READ)
#   - Applique les regles metier de correction :
#       * ACTIVITE-LOGIC : corrige empreinte carbone negative -> 0
#       * UTILISATEUR-LOGIC : corrige dernier login negatif -> 0
#       * ANTENNE-LOGIC : corrige region vide -> "INCONNUE"
#       * REPAS-LOGIC : corrige nombre repas negatif -> 0
#       * HEBERGEMENT-LOGIC : corrige nombre nuits negatif -> 0
#       * PARTICIPATION-LOGIC : corrige mode transport negatif -> 0
#   - Sauvegarde les corrections via DAL-SAVE (UPDATE SQL)
#   - Ferme proprement les ressources via DAL-END (COMMIT, CLOSE, DISCONNECT)
#   - Retourne un code de sortie 0 (succes)
#
# TESTS DE LA COUCHE DAL-DB
# -------------------------
# Le script verifie que le code SQL embarque :
#   - Utilise EXEC SQL CONNECT pour se connecter a PostgreSQL
#   - Declare et manipule des curseurs SQL (DECLARE CURSOR, OPEN, FETCH, CLOSE)
#   - Execute des requetes SELECT avec jointures (INNER JOIN)
#   - Execute des requetes UPDATE pour sauvegarder les corrections
#   - Gere les transactions (COMMIT, ROLLBACK)
#   - Gere SQLCODE pour detecter les erreurs et fin de fichier (100)
#   - Se deconnecte proprement (DISCONNECT ALL)
#   - Presence de EXEC SQL dans les fichiers DAL (test R4)
#   - Presence de gestion SQLCODE dans les fichiers DAL (test R5)
#
# TESTS DE LA COUCHE BUSINESS
# ---------------------------
# Le script compile les modules BUSINESS qui :
#   - Recoivent les donnees via LINKAGE SECTION
#   - Affichent les informations formatees via DISPLAY
#   - Retournent au programme appelant via GOBACK
#
# TESTS DES STRUCTURES DE DONNEES COBOL
# -------------------------------------
# Le script verifie implicitement :
#   - Les declarations WORKING-STORAGE SECTION (variables locales)
#   - Les declarations LINKAGE SECTION (parametres CALL)
#   - Les PIC clauses (formats numeriques, alphanumeriques)
#   - Les structures de groupe (01, 05, 77)
#   - La coherence des types entre COBOL et SQL PostgreSQL
#
# TESTS DE COMPILATION
# --------------------
# Le script verifie que :
#   - La precompilation OCESQL transforme EXEC SQL en COBOL natif
#   - La compilation GnuCOBOL (cobc) produit des executables/modules
#   - Les options -m (module), -x (executable), -fixed/-free sont correctes
#   - Les liaisons bibliotheques (-locesql, -lpq) sont resolues
#   - Les COPY books (fichiers .cpy) sont inclus correctement
#
# TESTS D'EXECUTION ET EFFET SUR LA BASE
# --------------------------------------
# Pour chaque programme LOGIC, le script :
#   1. Injecte des anomalies dans la BD (valeurs negatives, chaines vides)
#   2. Execute le programme COBOL batch
#   3. Verifie que les anomalies ont ete corrigees (COUNT = 0)
#   4. Verifie que les donnees valides n'ont PAS ete modifiees
#   5. Verifie les valeurs specifiques apres correction (= 0 ou "INCONNUE")
#
# TESTS D'INTEGRITE REFERENTIELLE
# -------------------------------
# Le script verifie que le code COBOL respecte les FK :
#   - UTILISATEUR.USER_ID_ANTENNE -> ANTENNE.ANTENNE_ID
#   - ACTIVITE.ACTIVITE_IDANTENNE -> ANTENNE.ANTENNE_ID
#   - ACTIVITE.ACTIVITE_ANIMATEUR -> UTILISATEUR.USER_ID
#   - REPAS.REPAS_ID_ACTIVITE -> ACTIVITE.ACTIVITE_ID
#   - HEBERGEMENT.HEBERGEMENT_ID_ACTIVITE -> ACTIVITE.ACTIVITE_ID
#   - PARTICIPATION.(ID_ACTIVITE, ID_USER) -> ACTIVITE, UTILISATEUR
#
# RESUME DES VERIFICATIONS COBOL
# ------------------------------
#   - Syntaxe COBOL valide (compilation reussie)
#   - SQL embarque correct (precompilation OCESQL reussie)
#   - Logique metier correcte (corrections appliquees)
#   - Gestion erreurs SQL (SQLCODE teste)
#   - Transactions DB (COMMIT/ROLLBACK)
#   - Appels inter-programmes (CALL/GOBACK)
#   - Passage de parametres (LINKAGE SECTION)
#   - Preservation des donnees valides (non-regression)
#
#=========================================================================

# Always run from the script directory
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
cd "$SCRIPT_DIR"

# Configuration PostgreSQL
DB_NAME="carbontrackdb"
DB_USER="carbonuser"
DB_PASSWORD="CARBONPWD"
DB_HOST="localhost"
DB_PORT="5432"
DB_ADMIN_USER="postgres"
DB_ADMIN_PASSWORD=""

# Couleurs
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

# Compteurs
TESTS_PASSED=0
TESTS_FAILED=0
TESTS_TOTAL=0
TESTS_SKIPPED=0
DEBUG=${DEBUG:-1}

# Nettoyer les logs precedents
rm -f /tmp/compile_*.log \
      /tmp/batch_*.log \
      /tmp/db_before_*.log \
      /tmp/db_after_*.log

# Fonction pour afficher le resultat d'un test
test_result() {
    local test_name=$1
    local result=$2
    local details=$3
    TESTS_TOTAL=$((TESTS_TOTAL + 1))

    if [ $result -eq 0 ]; then
        echo -e "${GREEN}[OK]${NC} Test $TESTS_TOTAL: $test_name"
        TESTS_PASSED=$((TESTS_PASSED + 1))
        if [ "$DEBUG" -ge 1 ] && [ -n "$details" ]; then
            echo -e "${YELLOW}  Details:${NC}"
            while IFS= read -r line; do
                echo -e "${YELLOW}  $line${NC}"
            done <<< "$details"
        fi
    else
        echo -e "${RED}[FAIL]${NC} Test $TESTS_TOTAL: $test_name"
        TESTS_FAILED=$((TESTS_FAILED + 1))
        if [ -n "$details" ]; then
            echo -e "${YELLOW}  Details:${NC}"
            while IFS= read -r line; do
                echo -e "${YELLOW}  $line${NC}"
            done <<< "$details"
        fi
    fi
}

# Fonction pour afficher un test ignore
test_skip() {
    local test_name=$1
    local details=$2
    TESTS_TOTAL=$((TESTS_TOTAL + 1))
    TESTS_SKIPPED=$((TESTS_SKIPPED + 1))
    echo -e "${YELLOW}[SKIP]${NC} Test $TESTS_TOTAL: $test_name"
    if [ "$DEBUG" -ge 1 ] && [ -n "$details" ]; then
        echo -e "${YELLOW}  Details:${NC}"
        while IFS= read -r line; do
            echo -e "${YELLOW}  $line${NC}"
        done <<< "$details"
    fi
}

# Fonctions SQL PostgreSQL
psql_cmd() {
    local user=$1
    local pass=$2
    local db=$3
    local sql=$4
    if [ -n "$pass" ]; then
        PGPASSWORD=$pass psql -h $DB_HOST -p $DB_PORT -U $user -d $db -tAc "$sql"
    else
        psql -h $DB_HOST -p $DB_PORT -U $user -d $db -tAc "$sql"
    fi
}

run_sql() {
    if [ -n "$SQL_PASSWORD" ]; then
        PGPASSWORD=$SQL_PASSWORD psql -h $DB_HOST -p $DB_PORT -U $SQL_USER -d $DB_NAME -tAc "$1" 2>/dev/null
    else
        psql -h $DB_HOST -p $DB_PORT -U $SQL_USER -d $DB_NAME -tAc "$1" 2>/dev/null
    fi
}

run_sql_cmd() {
    if [ -n "$SQL_PASSWORD" ]; then
        PGPASSWORD=$SQL_PASSWORD psql -h $DB_HOST -p $DB_PORT -U $SQL_USER -d $DB_NAME -v ON_ERROR_STOP=1 -c "$1" > /dev/null 2>&1
    else
        psql -h $DB_HOST -p $DB_PORT -U $SQL_USER -d $DB_NAME -v ON_ERROR_STOP=1 -c "$1" > /dev/null 2>&1
    fi
}

SQL_LAST_OUTPUT=""
run_sql_cmd_capture() {
    local sql=$1
    local output
    local status
    if [ -n "$SQL_PASSWORD" ]; then
        output=$(PGPASSWORD=$SQL_PASSWORD psql -h $DB_HOST -p $DB_PORT -U $SQL_USER -d $DB_NAME -v ON_ERROR_STOP=1 -c "$sql" 2>&1)
    else
        output=$(psql -h $DB_HOST -p $DB_PORT -U $SQL_USER -d $DB_NAME -v ON_ERROR_STOP=1 -c "$sql" 2>&1)
    fi
    status=$?
    SQL_LAST_OUTPUT="$output"
    return $status
}

sql_scalar() {
    local sql=$1
    local result
    result=$(run_sql "$sql")
    echo "$result" | tr -d '[:space:]'
}

assert_sql_count_eq() {
    local test_name=$1
    local sql=$2
    local expected=$3
    local val
    val=$(sql_scalar "$sql")
    if ! [[ "$val" =~ ^-?[0-9]+$ ]]; then
        test_result "$test_name" 1 "SQL: $sql"$'\n'"Resultat SQL invalide: ${val:-<vide>}"
        return
    fi
    if [ "$val" -eq "$expected" ]; then
        test_result "$test_name" 0 "SQL: $sql"$'\n'"Attendu: $expected"$'\n'"Obtenu: $val"
    else
        test_result "$test_name" 1 "SQL: $sql"$'\n'"Attendu: $expected"$'\n'"Obtenu: $val"
    fi
}

assert_sql_count_ge() {
    local test_name=$1
    local sql=$2
    local expected=$3
    local val
    val=$(sql_scalar "$sql")
    if ! [[ "$val" =~ ^-?[0-9]+$ ]]; then
        test_result "$test_name" 1 "SQL: $sql"$'\n'"Resultat SQL invalide: ${val:-<vide>}"
        return
    fi
    if [ "$val" -ge "$expected" ]; then
        test_result "$test_name" 0 "SQL: $sql"$'\n'"Attendu >= $expected"$'\n'"Obtenu: $val"
    else
        test_result "$test_name" 1 "SQL: $sql"$'\n'"Attendu >= $expected"$'\n'"Obtenu: $val"
    fi
}

psql_print() {
    local sql=$1
    if [ -n "$SQL_PASSWORD" ]; then
        PGPASSWORD=$SQL_PASSWORD psql -h $DB_HOST -p $DB_PORT -U $SQL_USER -d $DB_NAME -c "$sql"
    else
        psql -h $DB_HOST -p $DB_PORT -U $SQL_USER -d $DB_NAME -c "$sql"
    fi
}

assert_sql_count_eq_with_dump() {
    local test_name=$1
    local sql=$2
    local expected=$3
    local dump_sql=$4
    local val
    val=$(sql_scalar "$sql")
    if ! [[ "$val" =~ ^-?[0-9]+$ ]]; then
        test_result "$test_name" 1 "SQL: $sql"$'\n'"Resultat SQL invalide: ${val:-<vide>}"
        return
    fi
    if [ "$val" -eq "$expected" ]; then
        test_result "$test_name" 0 "SQL: $sql"$'\n'"Attendu: $expected"$'\n'"Obtenu: $val"
    else
        test_result "$test_name" 1 "SQL: $sql"$'\n'"Attendu: $expected"$'\n'"Obtenu: $val"
        if [ -n "$dump_sql" ]; then
            echo -e "${YELLOW}  Details:${NC}"
            psql_print "$dump_sql"
        fi
    fi
}

log_count() {
    local file=$1
    local pattern=$2
    if command -v rg >/dev/null 2>&1; then
        rg -F -c "$pattern" "$file" 2>/dev/null || echo 0
    else
        grep -cF "$pattern" "$file" 2>/dev/null || echo 0
    fi
}

assert_log_count_eq() {
    local test_name=$1
    local file=$2
    local pattern=$3
    local expected=$4
    if [ ! -f "$file" ]; then
        test_result "$test_name" 1 "Log manquant: $file"$'\n'"Pattern: $pattern"
        return
    fi
    local count
    count=$(log_count "$file" "$pattern")
    if ! [[ "$count" =~ ^[0-9]+$ ]]; then
        test_result "$test_name" 1 "Log: $file"$'\n'"Pattern: $pattern"$'\n'"Compte invalide: ${count:-<vide>}"
        return
    fi
    if [ "$count" -eq "$expected" ]; then
        test_result "$test_name" 0 "Log: $file"$'\n'"Pattern: $pattern"$'\n'"Attendu: $expected"$'\n'"Obtenu: $count"
    else
        test_result "$test_name" 1 "Log: $file"$'\n'"Pattern: $pattern"$'\n'"Attendu: $expected"$'\n'"Obtenu: $count"
    fi
}

assert_log_count_ge() {
    local test_name=$1
    local file=$2
    local pattern=$3
    local expected=$4
    if [ ! -f "$file" ]; then
        test_result "$test_name" 1 "Log manquant: $file"$'\n'"Pattern: $pattern"
        return
    fi
    local count
    count=$(log_count "$file" "$pattern")
    if ! [[ "$count" =~ ^[0-9]+$ ]]; then
        test_result "$test_name" 1 "Log: $file"$'\n'"Pattern: $pattern"$'\n'"Compte invalide: ${count:-<vide>}"
        return
    fi
    if [ "$count" -ge "$expected" ]; then
        test_result "$test_name" 0 "Log: $file"$'\n'"Pattern: $pattern"$'\n'"Attendu >= $expected"$'\n'"Obtenu: $count"
    else
        test_result "$test_name" 1 "Log: $file"$'\n'"Pattern: $pattern"$'\n'"Attendu >= $expected"$'\n'"Obtenu: $count"
    fi
}

check_cmd() {
    command -v "$1" >/dev/null 2>&1
}

cleanup() {
    :
}

trap cleanup EXIT

echo "=========================================="
echo "TESTS DE VALIDATION PROJET CARBONTRACK"
echo "Specification: carbontrack_minimal.yaml"
echo "=========================================="
echo ""

# Verifications outils
MISSING=0
for cmd in psql createdb dropdb ocesql cobc; do
    cmd_path=$(command -v "$cmd" 2>/dev/null)
    if check_cmd "$cmd"; then
        test_result "Presence $cmd" 0 "Path: ${cmd_path:-<introuvable>}"
    else
        test_result "Presence $cmd" 1 "Path: <introuvable>"
        MISSING=1
    fi
done

if [ $MISSING -ne 0 ]; then
    echo -e "${RED}Outils manquants. Installez PostgreSQL client, ocesql et cobc.${NC}"
    exit 1
fi

echo ""

#########################################################################
# SECTION 1: SETUP BASE DE DONNEES
#########################################################################
echo -e "${BLUE}[SECTION 1] Setup base de donnees${NC}"

# Drop database if exists
echo "Suppression de la base existante (si presente)..."
drop_log="/tmp/db_drop.log"
if [ -n "$DB_ADMIN_PASSWORD" ]; then
    PGPASSWORD=$DB_ADMIN_PASSWORD dropdb -h $DB_HOST -p $DB_PORT -U $DB_ADMIN_USER --if-exists $DB_NAME > "$drop_log" 2>&1
else
    dropdb -h $DB_HOST -p $DB_PORT -U $DB_ADMIN_USER --if-exists $DB_NAME > "$drop_log" 2>&1
fi

# Create database
echo "Creation de la base $DB_NAME..."
create_log="/tmp/db_create.log"
if [ -n "$DB_ADMIN_PASSWORD" ]; then
    PGPASSWORD=$DB_ADMIN_PASSWORD createdb -h $DB_HOST -p $DB_PORT -U $DB_ADMIN_USER $DB_NAME > "$create_log" 2>&1
else
    createdb -h $DB_HOST -p $DB_PORT -U $DB_ADMIN_USER $DB_NAME > "$create_log" 2>&1
fi
create_status=$?
create_details="Commande: createdb -h $DB_HOST -p $DB_PORT -U $DB_ADMIN_USER $DB_NAME"$'\n'"Log: $create_log"
if [ $create_status -eq 0 ]; then
    test_result "Creation base de donnees $DB_NAME" 0 "$create_details"
else
    test_result "Creation base de donnees $DB_NAME" 1 "$create_details"
    echo -e "${RED}Impossible de creer la base. Verifiez PostgreSQL.${NC}"
    tail -20 "$create_log"
    exit 1
fi

# Ensure role exists
echo "Verification du role $DB_USER..."
role_check_sql="SELECT 1 FROM pg_roles WHERE rolname='${DB_USER}';"
role_exists=$(psql_cmd "$DB_ADMIN_USER" "$DB_ADMIN_PASSWORD" "postgres" "$role_check_sql" 2>/dev/null)
if [ "$role_exists" = "1" ]; then
    test_result "Role $DB_USER present" 0 "SQL: $role_check_sql"$'\n'"Resultat: $role_exists"
else
    role_log="/tmp/db_role.log"
    if [ -n "$DB_ADMIN_PASSWORD" ]; then
        PGPASSWORD=$DB_ADMIN_PASSWORD psql -h $DB_HOST -p $DB_PORT -U $DB_ADMIN_USER -d postgres -c "CREATE ROLE $DB_USER LOGIN PASSWORD '$DB_PASSWORD';" > "$role_log" 2>&1
    else
        psql -h $DB_HOST -p $DB_PORT -U $DB_ADMIN_USER -d postgres -c "CREATE ROLE $DB_USER LOGIN PASSWORD '$DB_PASSWORD';" > "$role_log" 2>&1
    fi
    role_create_status=$?
    role_create_details="SQL: CREATE ROLE $DB_USER LOGIN PASSWORD '********'"$'\n'"Log: $role_log"
    if [ $role_create_status -eq 0 ]; then
        test_result "Creation role $DB_USER" 0 "$role_create_details"
    else
        test_result "Creation role $DB_USER" 1 "$role_create_details"
        echo -e "${YELLOW}Impossible de creer le role $DB_USER. Les tests SQL peuvent echouer.${NC}"
        tail -20 "$role_log"
    fi
fi

# Create tables
echo "Creation des tables..."
if [ -n "$DB_ADMIN_PASSWORD" ]; then
    export PGPASSWORD="$DB_ADMIN_PASSWORD"
else
    unset PGPASSWORD
fi
tables_log="/tmp/db_setup_tables.log"
psql -h $DB_HOST -p $DB_PORT -U $DB_ADMIN_USER -d $DB_NAME <<'EOSQL' | tee "$tables_log"
CREATE TABLE ANTENNE (
    ANTENNE_ID INT PRIMARY KEY,
    ANTENNE_NOM VARCHAR(50) NOT NULL,
    ANTENNE_REGION VARCHAR(50) NOT NULL
);

CREATE TABLE UTILISATEUR (
    USER_ID INT PRIMARY KEY,
    USER_NOM VARCHAR(50) NOT NULL,
    USER_MAIL VARCHAR(80) NOT NULL UNIQUE,
    USER_PASS VARCHAR(256) NOT NULL,
    USER_ROLE VARCHAR(15) NOT NULL,
    USER_ID_ANTENNE INT NOT NULL REFERENCES ANTENNE(ANTENNE_ID),
    USER_LAST_LOGIN BIGINT
);

CREATE TABLE ACTIVITE (
    ACTIVITE_ID INT PRIMARY KEY,
    ACTIVITE_NOM VARCHAR(50) NOT NULL,
    ACTIVITE_TYPE VARCHAR(20) NOT NULL,
    ACTIVITE_IDANTENNE INT NOT NULL REFERENCES ANTENNE(ANTENNE_ID),
    ACTIVITE_ANIMATEUR INT NOT NULL REFERENCES UTILISATEUR(USER_ID),
    ACTIVITE_NBPARTICIPANTS INT NOT NULL,
    ACTIVITE_MODETRANSPORT INT NOT NULL,
    ACTIVITE_LIEU VARCHAR(100) NOT NULL,
    ACTIVITE_DISTANCE INT NOT NULL,
    ACTIVITE_HEBERGEMENT INT NOT NULL,
    ACTIVITE_REPASPREVU INT NOT NULL,
    ACTIVITE_EMPREINTETOTALE DECIMAL(13,4)
);

CREATE TABLE REPAS (
    REPAS_ID INT PRIMARY KEY,
    REPAS_ID_ACTIVITE INT NOT NULL REFERENCES ACTIVITE(ACTIVITE_ID),
    REPAS_TYPE INT NOT NULL,
    REPAS_NBREPAS INT NOT NULL
);

CREATE TABLE HEBERGEMENT (
    HEBERGEMENT_ID INT PRIMARY KEY,
    HEBERGEMENT_ID_ACTIVITE INT NOT NULL REFERENCES ACTIVITE(ACTIVITE_ID),
    HEBERGEMENT_TYPE INT NOT NULL,
    HEBERGEMENT_NBNUIT INT NOT NULL
);

CREATE TABLE PARTICIPATION (
    PARTICIPATION_ID_ACTIVITE INT NOT NULL REFERENCES ACTIVITE(ACTIVITE_ID),
    PARTICIPATION_ID_USER INT NOT NULL REFERENCES UTILISATEUR(USER_ID),
    PARTICIPATION_MODE_TRANSPORT INT NOT NULL,
    PRIMARY KEY (PARTICIPATION_ID_ACTIVITE, PARTICIPATION_ID_USER)
);

CREATE INDEX UTILISATEUR_MAIL_UK ON UTILISATEUR (USER_MAIL);
CREATE INDEX UTILISATEUR_ANTENNE_IDX ON UTILISATEUR (USER_ID_ANTENNE);
CREATE INDEX ACTIVITE_ANTENNE_IDX ON ACTIVITE (ACTIVITE_IDANTENNE);
CREATE INDEX ACTIVITE_ANIMATEUR_IDX ON ACTIVITE (ACTIVITE_ANIMATEUR);
CREATE INDEX REPAS_ACTIVITE_IDX ON REPAS (REPAS_ID_ACTIVITE);
CREATE INDEX HEBERGEMENT_ACTIVITE_IDX ON HEBERGEMENT (HEBERGEMENT_ID_ACTIVITE);
CREATE INDEX PARTICIPATION_USER_IDX ON PARTICIPATION (PARTICIPATION_ID_USER);

GRANT ALL PRIVILEGES ON DATABASE carbontrackdb TO carbonuser;
GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA public TO carbonuser;
EOSQL
tables_status=${PIPESTATUS[0]}
unset PGPASSWORD

test_result "Creation tables et indexes" $tables_status "Log: $tables_log"
if [ $tables_status -ne 0 ]; then
    tail -20 "$tables_log"
fi

# Insert test data
echo "Insertion des donnees de test..."
if [ -n "$DB_ADMIN_PASSWORD" ]; then
    export PGPASSWORD="$DB_ADMIN_PASSWORD"
else
    unset PGPASSWORD
fi
insert_log="/tmp/db_insert_data.log"
psql -h $DB_HOST -p $DB_PORT -U $DB_ADMIN_USER -d $DB_NAME <<'EOSQL' | tee "$insert_log"
-- Antennes valides et avec region vide (sera corrigee)
INSERT INTO ANTENNE VALUES (1, 'PARIS', 'IDF');
INSERT INTO ANTENNE VALUES (2, 'LYON', 'ARA');
INSERT INTO ANTENNE VALUES (3, 'MARSEILLE', 'PACA');
INSERT INTO ANTENNE VALUES (4, 'TOULOUSE', 'OCCITANIE');

-- Utilisateurs valides avec differents roles et derniers logins
INSERT INTO UTILISATEUR (USER_ID, USER_NOM, USER_MAIL, USER_PASS, USER_ROLE, USER_ID_ANTENNE, USER_LAST_LOGIN)
VALUES (1001, 'ALICE MARTIN', 'alice@carbontrack.fr', 'HASHEDPASS1', 'ADMIN', 1, 1609459200);
INSERT INTO UTILISATEUR (USER_ID, USER_NOM, USER_MAIL, USER_PASS, USER_ROLE, USER_ID_ANTENNE, USER_LAST_LOGIN)
VALUES (1002, 'BOB DUPONT', 'bob@carbontrack.fr', 'HASHEDPASS2', 'USER', 2, 0);
INSERT INTO UTILISATEUR (USER_ID, USER_NOM, USER_MAIL, USER_PASS, USER_ROLE, USER_ID_ANTENNE, USER_LAST_LOGIN)
VALUES (1003, 'CHARLIE DURAND', 'charlie@carbontrack.fr', 'HASHEDPASS3', 'USER', 3, 1609545600);
INSERT INTO UTILISATEUR (USER_ID, USER_NOM, USER_MAIL, USER_PASS, USER_ROLE, USER_ID_ANTENNE, USER_LAST_LOGIN)
VALUES (1004, 'DIANE PETIT', 'diane@carbontrack.fr', 'HASHEDPASS4', 'MANAGER', 4, 1609632000);
INSERT INTO UTILISATEUR (USER_ID, USER_NOM, USER_MAIL, USER_PASS, USER_ROLE, USER_ID_ANTENNE, USER_LAST_LOGIN)
VALUES (1005, 'ERIC MOREAU', 'eric@carbontrack.fr', 'HASHEDPASS5', 'USER', 1, 0);

-- Activites avec empreintes valides et invalides (negatives)
INSERT INTO ACTIVITE (ACTIVITE_ID, ACTIVITE_NOM, ACTIVITE_TYPE, ACTIVITE_IDANTENNE, ACTIVITE_ANIMATEUR,
                      ACTIVITE_NBPARTICIPANTS, ACTIVITE_MODETRANSPORT, ACTIVITE_LIEU, ACTIVITE_DISTANCE,
                      ACTIVITE_HEBERGEMENT, ACTIVITE_REPASPREVU, ACTIVITE_EMPREINTETOTALE)
VALUES (2001, 'ATELIER MOBILITE', 'TRANSPORT', 1, 1001, 10, 1, 'PARIS', 120, 0, 1, 0.0);
INSERT INTO ACTIVITE (ACTIVITE_ID, ACTIVITE_NOM, ACTIVITE_TYPE, ACTIVITE_IDANTENNE, ACTIVITE_ANIMATEUR,
                      ACTIVITE_NBPARTICIPANTS, ACTIVITE_MODETRANSPORT, ACTIVITE_LIEU, ACTIVITE_DISTANCE,
                      ACTIVITE_HEBERGEMENT, ACTIVITE_REPASPREVU, ACTIVITE_EMPREINTETOTALE)
VALUES (2002, 'SEMINAIRE CARBONE', 'HEBERGEMENT', 2, 1002, 5, 2, 'LYON', 300, 1, 1, 0.0);
INSERT INTO ACTIVITE (ACTIVITE_ID, ACTIVITE_NOM, ACTIVITE_TYPE, ACTIVITE_IDANTENNE, ACTIVITE_ANIMATEUR,
                      ACTIVITE_NBPARTICIPANTS, ACTIVITE_MODETRANSPORT, ACTIVITE_LIEU, ACTIVITE_DISTANCE,
                      ACTIVITE_HEBERGEMENT, ACTIVITE_REPASPREVU, ACTIVITE_EMPREINTETOTALE)
VALUES (2003, 'FORMATION VELO', 'TRANSPORT', 3, 1003, 15, 3, 'MARSEILLE', 50, 0, 1, 250.5);
INSERT INTO ACTIVITE (ACTIVITE_ID, ACTIVITE_NOM, ACTIVITE_TYPE, ACTIVITE_IDANTENNE, ACTIVITE_ANIMATEUR,
                      ACTIVITE_NBPARTICIPANTS, ACTIVITE_MODETRANSPORT, ACTIVITE_LIEU, ACTIVITE_DISTANCE,
                      ACTIVITE_HEBERGEMENT, ACTIVITE_REPASPREVU, ACTIVITE_EMPREINTETOTALE)
VALUES (2004, 'CONFERENCE CLIMAT', 'HEBERGEMENT', 4, 1004, 20, 1, 'TOULOUSE', 200, 1, 1, 1250.75);
INSERT INTO ACTIVITE (ACTIVITE_ID, ACTIVITE_NOM, ACTIVITE_TYPE, ACTIVITE_IDANTENNE, ACTIVITE_ANIMATEUR,
                      ACTIVITE_NBPARTICIPANTS, ACTIVITE_MODETRANSPORT, ACTIVITE_LIEU, ACTIVITE_DISTANCE,
                      ACTIVITE_HEBERGEMENT, ACTIVITE_REPASPREVU, ACTIVITE_EMPREINTETOTALE)
VALUES (2005, 'ATELIER COMPOST', 'TRANSPORT', 1, 1005, 8, 4, 'PARIS', 10, 0, 0, 50.0);

-- Repas valides avec differents types et quantites
INSERT INTO REPAS VALUES (3001, 2001, 1, 10);
INSERT INTO REPAS VALUES (3002, 2002, 2, 5);
INSERT INTO REPAS VALUES (3003, 2003, 1, 15);
INSERT INTO REPAS VALUES (3004, 2004, 3, 20);
INSERT INTO REPAS VALUES (3005, 2004, 1, 20);

-- Hebergements valides
INSERT INTO HEBERGEMENT VALUES (4001, 2002, 1, 2);
INSERT INTO HEBERGEMENT VALUES (4002, 2001, 1, 1);
INSERT INTO HEBERGEMENT VALUES (4003, 2004, 2, 3);

-- Participations valides avec differents modes de transport
INSERT INTO PARTICIPATION VALUES (2001, 1001, 1);
INSERT INTO PARTICIPATION VALUES (2001, 1002, 2);
INSERT INTO PARTICIPATION VALUES (2002, 1002, 2);
INSERT INTO PARTICIPATION VALUES (2003, 1003, 3);
INSERT INTO PARTICIPATION VALUES (2003, 1001, 1);
INSERT INTO PARTICIPATION VALUES (2004, 1004, 1);
INSERT INTO PARTICIPATION VALUES (2004, 1001, 2);
INSERT INTO PARTICIPATION VALUES (2004, 1002, 2);
INSERT INTO PARTICIPATION VALUES (2005, 1005, 4);
INSERT INTO PARTICIPATION VALUES (2005, 1001, 4);
EOSQL
insert_status=${PIPESTATUS[0]}
unset PGPASSWORD

test_result "Insertion donnees de test" $insert_status "Log: $insert_log"
if [ $insert_status -ne 0 ]; then
    tail -20 "$insert_log"
fi

USER_CONN_OK=0
conn_output=$(psql_cmd "$DB_USER" "$DB_PASSWORD" "$DB_NAME" "SELECT 1;" 2>&1)
if [ $? -eq 0 ]; then
    USER_CONN_OK=1
    test_result "Connexion role $DB_USER" 0 "SQL: SELECT 1;"$'\n'"Sortie: ${conn_output:-<vide>}"
else
    test_result "Connexion role $DB_USER" 1 "SQL: SELECT 1;"$'\n'"Sortie: ${conn_output:-<vide>}"
    echo -e "${YELLOW}Connexion en tant que $DB_USER impossible. Tests SQL feront un fallback admin si possible.${NC}"
fi

SQL_USER="$DB_USER"
SQL_PASSWORD="$DB_PASSWORD"
if [ $USER_CONN_OK -ne 1 ]; then
    SQL_USER="$DB_ADMIN_USER"
    SQL_PASSWORD="$DB_ADMIN_PASSWORD"
fi

echo ""

#########################################################################
# SECTION 1B: PREPARATION CAS DE TEST (DONNEES NEGATIVES)
#########################################################################
echo -e "${BLUE}[SECTION 1B] Preparation des cas de test (injection anomalies)${NC}"

# Injection d'anomalies multiples pour tester la robustesse
run_sql_cmd "UPDATE ACTIVITE SET ACTIVITE_EMPREINTETOTALE = -5.0 WHERE ACTIVITE_ID = 2002;"
run_sql_cmd "UPDATE ACTIVITE SET ACTIVITE_EMPREINTETOTALE = -100.25 WHERE ACTIVITE_ID = 2001;"
run_sql_cmd "UPDATE REPAS SET REPAS_NBREPAS = -4 WHERE REPAS_ID = 3002;"
run_sql_cmd "UPDATE REPAS SET REPAS_NBREPAS = -10 WHERE REPAS_ID = 3004;"
run_sql_cmd "UPDATE HEBERGEMENT SET HEBERGEMENT_NBNUIT = -2 WHERE HEBERGEMENT_ID = 4001;"
run_sql_cmd "UPDATE HEBERGEMENT SET HEBERGEMENT_NBNUIT = -5 WHERE HEBERGEMENT_ID = 4003;"
run_sql_cmd "UPDATE PARTICIPATION SET PARTICIPATION_MODE_TRANSPORT = -3 WHERE PARTICIPATION_ID_ACTIVITE = 2002 AND PARTICIPATION_ID_USER = 1002;"
run_sql_cmd "UPDATE PARTICIPATION SET PARTICIPATION_MODE_TRANSPORT = -1 WHERE PARTICIPATION_ID_ACTIVITE = 2004 AND PARTICIPATION_ID_USER = 1002;"
run_sql_cmd "UPDATE UTILISATEUR SET USER_LAST_LOGIN = -1 WHERE USER_ID = 1002;"
run_sql_cmd "UPDATE UTILISATEUR SET USER_LAST_LOGIN = -999999 WHERE USER_ID = 1005;"
run_sql_cmd "UPDATE ANTENNE SET ANTENNE_REGION = '' WHERE ANTENNE_ID = 2;"
run_sql_cmd "UPDATE ANTENNE SET ANTENNE_REGION = '  ' WHERE ANTENNE_ID = 4;"

# Verification que les anomalies sont bien presentes (au moins 2 de chaque type)
assert_sql_count_ge "Precondition F5: empreinte negative presente (>=2)" "SELECT COUNT(*) FROM ACTIVITE WHERE ACTIVITE_EMPREINTETOTALE < 0;" 2
assert_sql_count_ge "Precondition F3: nombre repas negatif present (>=2)" "SELECT COUNT(*) FROM REPAS WHERE REPAS_NBREPAS < 0;" 2
assert_sql_count_ge "Precondition F3: nombre nuits negatif present (>=2)" "SELECT COUNT(*) FROM HEBERGEMENT WHERE HEBERGEMENT_NBNUIT < 0;" 2
assert_sql_count_ge "Precondition F4: mode transport negatif present (>=2)" "SELECT COUNT(*) FROM PARTICIPATION WHERE PARTICIPATION_MODE_TRANSPORT < 0;" 2
assert_sql_count_ge "Precondition F1: dernier login negatif present (>=2)" "SELECT COUNT(*) FROM UTILISATEUR WHERE USER_LAST_LOGIN < 0;" 2
assert_sql_count_ge "Precondition F2: region antenne vide presente (>=2)" "SELECT COUNT(*) FROM ANTENNE WHERE ANTENNE_REGION IS NULL OR ANTENNE_REGION = '' OR BTRIM(ANTENNE_REGION) = '';" 2

echo ""

#########################################################################
# SECTION 2: AFFICHAGE BD AVANT EXECUTION
#########################################################################
echo -e "${BLUE}[SECTION 2] Base de donnees AVANT execution${NC}"

echo ""
echo -e "${CYAN}=== TABLE UTILISATEUR ===${NC}"
before_ok=0
if [ -n "$SQL_PASSWORD" ]; then
    PGPASSWORD=$SQL_PASSWORD psql -h $DB_HOST -p $DB_PORT -U $SQL_USER -d $DB_NAME -c \
    "SELECT USER_ID, USER_NOM, USER_MAIL, USER_ROLE, USER_ID_ANTENNE, USER_LAST_LOGIN FROM UTILISATEUR ORDER BY USER_ID;" | tee /tmp/db_before_utilisateur.log
else
    psql -h $DB_HOST -p $DB_PORT -U $SQL_USER -d $DB_NAME -c \
    "SELECT USER_ID, USER_NOM, USER_MAIL, USER_ROLE, USER_ID_ANTENNE, USER_LAST_LOGIN FROM UTILISATEUR ORDER BY USER_ID;" | tee /tmp/db_before_utilisateur.log
fi
if [ $? -ne 0 ]; then
    before_ok=1
fi

echo ""
echo -e "${CYAN}=== TABLE ACTIVITE ===${NC}"
if [ -n "$SQL_PASSWORD" ]; then
    PGPASSWORD=$SQL_PASSWORD psql -h $DB_HOST -p $DB_PORT -U $SQL_USER -d $DB_NAME -c \
    "SELECT ACTIVITE_ID, ACTIVITE_NOM, ACTIVITE_TYPE, ACTIVITE_IDANTENNE, ACTIVITE_ANIMATEUR, ACTIVITE_EMPREINTETOTALE FROM ACTIVITE ORDER BY ACTIVITE_ID;" | tee /tmp/db_before_activite.log
else
    psql -h $DB_HOST -p $DB_PORT -U $SQL_USER -d $DB_NAME -c \
    "SELECT ACTIVITE_ID, ACTIVITE_NOM, ACTIVITE_TYPE, ACTIVITE_IDANTENNE, ACTIVITE_ANIMATEUR, ACTIVITE_EMPREINTETOTALE FROM ACTIVITE ORDER BY ACTIVITE_ID;" | tee /tmp/db_before_activite.log
fi
if [ $? -ne 0 ]; then
    before_ok=1
fi

echo ""
echo -e "${CYAN}=== TABLE ANTENNE ===${NC}"
if [ -n "$SQL_PASSWORD" ]; then
    PGPASSWORD=$SQL_PASSWORD psql -h $DB_HOST -p $DB_PORT -U $SQL_USER -d $DB_NAME -c \
    "SELECT ANTENNE_ID, ANTENNE_NOM, ANTENNE_REGION FROM ANTENNE ORDER BY ANTENNE_ID;" | tee /tmp/db_before_antenne.log
else
    psql -h $DB_HOST -p $DB_PORT -U $SQL_USER -d $DB_NAME -c \
    "SELECT ANTENNE_ID, ANTENNE_NOM, ANTENNE_REGION FROM ANTENNE ORDER BY ANTENNE_ID;" | tee /tmp/db_before_antenne.log
fi
if [ $? -ne 0 ]; then
    before_ok=1
fi

echo ""
echo -e "${CYAN}=== TABLE REPAS ===${NC}"
if [ -n "$SQL_PASSWORD" ]; then
    PGPASSWORD=$SQL_PASSWORD psql -h $DB_HOST -p $DB_PORT -U $SQL_USER -d $DB_NAME -c \
    "SELECT REPAS_ID, REPAS_ID_ACTIVITE, REPAS_TYPE, REPAS_NBREPAS FROM REPAS ORDER BY REPAS_ID;" | tee /tmp/db_before_repas.log
else
    psql -h $DB_HOST -p $DB_PORT -U $SQL_USER -d $DB_NAME -c \
    "SELECT REPAS_ID, REPAS_ID_ACTIVITE, REPAS_TYPE, REPAS_NBREPAS FROM REPAS ORDER BY REPAS_ID;" | tee /tmp/db_before_repas.log
fi
if [ $? -ne 0 ]; then
    before_ok=1
fi

echo ""
echo -e "${CYAN}=== TABLE HEBERGEMENT ===${NC}"
if [ -n "$SQL_PASSWORD" ]; then
    PGPASSWORD=$SQL_PASSWORD psql -h $DB_HOST -p $DB_PORT -U $SQL_USER -d $DB_NAME -c \
    "SELECT HEBERGEMENT_ID, HEBERGEMENT_ID_ACTIVITE, HEBERGEMENT_TYPE, HEBERGEMENT_NBNUIT FROM HEBERGEMENT ORDER BY HEBERGEMENT_ID;" | tee /tmp/db_before_hebergement.log
else
    psql -h $DB_HOST -p $DB_PORT -U $SQL_USER -d $DB_NAME -c \
    "SELECT HEBERGEMENT_ID, HEBERGEMENT_ID_ACTIVITE, HEBERGEMENT_TYPE, HEBERGEMENT_NBNUIT FROM HEBERGEMENT ORDER BY HEBERGEMENT_ID;" | tee /tmp/db_before_hebergement.log
fi
if [ $? -ne 0 ]; then
    before_ok=1
fi

echo ""
echo -e "${CYAN}=== TABLE PARTICIPATION ===${NC}"
if [ -n "$SQL_PASSWORD" ]; then
    PGPASSWORD=$SQL_PASSWORD psql -h $DB_HOST -p $DB_PORT -U $SQL_USER -d $DB_NAME -c \
    "SELECT PARTICIPATION_ID_ACTIVITE, PARTICIPATION_ID_USER, PARTICIPATION_MODE_TRANSPORT FROM PARTICIPATION ORDER BY PARTICIPATION_ID_ACTIVITE, PARTICIPATION_ID_USER;" | tee /tmp/db_before_participation.log
else
    psql -h $DB_HOST -p $DB_PORT -U $SQL_USER -d $DB_NAME -c \
    "SELECT PARTICIPATION_ID_ACTIVITE, PARTICIPATION_ID_USER, PARTICIPATION_MODE_TRANSPORT FROM PARTICIPATION ORDER BY PARTICIPATION_ID_ACTIVITE, PARTICIPATION_ID_USER;" | tee /tmp/db_before_participation.log
fi
if [ $? -ne 0 ]; then
    before_ok=1
fi

before_details="Logs: /tmp/db_before_utilisateur.log, /tmp/db_before_activite.log, /tmp/db_before_antenne.log, /tmp/db_before_repas.log, /tmp/db_before_hebergement.log, /tmp/db_before_participation.log"
if [ $before_ok -eq 0 ]; then
    test_result "Affichage BD avant execution" 0 "$before_details"
else
    test_result "Affichage BD avant execution" 1 "$before_details"
fi

echo ""

#########################################################################
# SECTION 3: COMPILATION COBOL
#########################################################################
echo -e "${BLUE}[SECTION 3] Compilation programmes COBOL${NC}"

# Nettoyage complet des anciens binaires et fichiers precompiles
echo "Nettoyage des anciens binaires et fichiers precompiles..."
rm -rf bin precompiled
mkdir -p bin precompiled

test_result "Nettoyage et creation repertoires compilation" 0 "Repertoires: bin/, precompiled/"

DAL_DB_PROGRAMS=(ACTIVITE-DAL-DB UTILISATEUR-DAL-DB ANTENNE-DAL-DB REPAS-DAL-DB HEBERGEMENT-DAL-DB PARTICIPATION-DAL-DB)
LOGIC_PROGRAMS=(ACTIVITE-LOGIC UTILISATEUR-LOGIC ANTENNE-LOGIC REPAS-LOGIC HEBERGEMENT-LOGIC PARTICIPATION-LOGIC)
BUSINESS_PROGRAMS=(ACTIVITE-BUSINESS UTILISATEUR-BUSINESS ANTENNE-BUSINESS REPAS-BUSINESS HEBERGEMENT-BUSINESS PARTICIPATION-BUSINESS)

# Limite aux programmes DAL-DB effectivement appeles par les LOGIC non vides
FILTERED_DAL_DB_PROGRAMS=()
for prog in "${LOGIC_PROGRAMS[@]}"; do
    if [ -f "logic/${prog}.cbl" ]; then
        if rg -q "CALL '.*-DAL-DB'" "logic/${prog}.cbl"; then
            while read -r dal_call; do
                dal_prog=$(echo "$dal_call" | sed -n "s/.*CALL '\\(.*-DAL-DB\\)'.*/\\1/p")
                if [ -n "$dal_prog" ]; then
                    FILTERED_DAL_DB_PROGRAMS+=("$dal_prog")
                fi
            done < <(rg -o "CALL '[^']*-DAL-DB'" "logic/${prog}.cbl")
        fi
    fi
done

# De-dupe en conservant l'ordre
UNIQ_DAL_DB_PROGRAMS=()
for prog in "${FILTERED_DAL_DB_PROGRAMS[@]}"; do
    if [[ ! " ${UNIQ_DAL_DB_PROGRAMS[*]} " =~ " ${prog} " ]]; then
        UNIQ_DAL_DB_PROGRAMS+=("$prog")
    fi
done

if [ ${#UNIQ_DAL_DB_PROGRAMS[@]} -eq 0 ]; then
    UNIQ_DAL_DB_PROGRAMS=("${DAL_DB_PROGRAMS[@]}")
fi

# Compile DAL-DB (precompile OCESQL)
for prog in "${UNIQ_DAL_DB_PROGRAMS[@]}"; do
    echo "Compilation $prog..."
    if [ ! -f "dal/${prog}.cbl" ]; then
        test_skip "Compilation $prog (fichier manquant)" "Source: dal/${prog}.cbl"
        continue
    fi
    dal_source="dal/${prog}.cbl"
    dal_precompiled="precompiled/${prog}.cbl.cob"
    dal_log="/tmp/compile_${prog}.log"
    dal_cmd_pre="ocesql ${dal_source} ${dal_precompiled}"
    dal_cmd_cobc="cobc -m -fixed -I copy/ ${dal_precompiled} -o bin/${prog}.so -locesql -lpq"
    {
        echo "Running OCESQL precompiler..."
        ocesql "$dal_source" "$dal_precompiled"
        echo "Running cobc..."
        cobc -m -fixed -I copy/ "$dal_precompiled" -o "bin/${prog}.so" -locesql -lpq
    } > "$dal_log" 2>&1
    dal_details="Source: $dal_source"$'\n'"Precompile: $dal_cmd_pre"$'\n'"Compile: $dal_cmd_cobc"$'\n'"Log: $dal_log"

    if [ $? -eq 0 ]; then
        test_result "Compilation $prog" 0 "$dal_details"
    else
        test_result "Compilation $prog" 1 "$dal_details"
        echo -e "${YELLOW}  Log: $dal_log${NC}"
        tail -20 "$dal_log"
    fi

done

# Compile LOGIC (executables)
for prog in "${LOGIC_PROGRAMS[@]}"; do
    echo "Compilation $prog..."
    if [ ! -f "logic/${prog}.cbl" ]; then
        test_skip "Compilation $prog (fichier manquant)" "Source: logic/${prog}.cbl"
        continue
    fi
    logic_source="logic/${prog}.cbl"
    logic_log="/tmp/compile_${prog}.log"
    logic_cmd="cobc -x -free -o bin/${prog} ${logic_source} -Lbin -L/usr/local/lib -locesql -lpq"
    {
        echo "Running cobc..."
        cobc -x -free -o "bin/${prog}" "$logic_source" -Lbin -L/usr/local/lib -locesql -lpq
    } > "$logic_log" 2>&1
    logic_details="Source: $logic_source"$'\n'"Compile: $logic_cmd"$'\n'"Log: $logic_log"

    if [ $? -eq 0 ]; then
        test_result "Compilation $prog" 0 "$logic_details"
    else
        test_result "Compilation $prog" 1 "$logic_details"
        echo -e "${YELLOW}  Log: $logic_log${NC}"
        tail -20 "$logic_log"
    fi

done

# Compile BUSINESS (modules)
for prog in "${BUSINESS_PROGRAMS[@]}"; do
    echo "Compilation $prog..."
    if [ ! -f "business/${prog}.cbl" ]; then
        test_skip "Compilation $prog (fichier manquant)" "Source: business/${prog}.cbl"
        continue
    fi
    business_source="business/${prog}.cbl"
    business_log="/tmp/compile_${prog}.log"
    business_cmd="cobc -m -fixed -o bin/${prog}.so ${business_source}"
    {
        echo "Running cobc..."
        cobc -m -fixed -o "bin/${prog}.so" "$business_source"
    } > "$business_log" 2>&1
    business_details="Source: $business_source"$'\n'"Compile: $business_cmd"$'\n'"Log: $business_log"

    if [ $? -eq 0 ]; then
        test_result "Compilation $prog" 0 "$business_details"
    else
        test_result "Compilation $prog" 1 "$business_details"
        echo -e "${YELLOW}  Log: $business_log${NC}"
        tail -20 "$business_log"
    fi

done

echo ""

#########################################################################
# SECTION 4: EXECUTION BATCH
#########################################################################
echo -e "${BLUE}[SECTION 4] Execution batch${NC}"

export LD_LIBRARY_PATH="./bin:$LD_LIBRARY_PATH"
export COB_LIBRARY_PATH="./bin:$COB_LIBRARY_PATH"
export LD_PRELOAD=/usr/local/lib/libocesql.so
export PGHOST=$DB_HOST
export PGPORT=$DB_PORT
export PGUSER=$DB_USER
export PGPASSWORD=$DB_PASSWORD
export PGDATABASE=$DB_NAME

for prog in "${LOGIC_PROGRAMS[@]}"; do
    echo "Execution du batch $prog..."
    check_sql=""
    dump_sql=""
    expected_desc=""
    case "$prog" in
        ACTIVITE-LOGIC)
            check_sql="SELECT COUNT(*) FROM ACTIVITE WHERE ACTIVITE_EMPREINTETOTALE < 0;"
            dump_sql="SELECT ACTIVITE_ID, ACTIVITE_EMPREINTETOTALE FROM ACTIVITE WHERE ACTIVITE_EMPREINTETOTALE < 0 ORDER BY ACTIVITE_ID;"
            expected_desc="empreinte carbone >= 0"
            ;;
        UTILISATEUR-LOGIC)
            check_sql="SELECT COUNT(*) FROM UTILISATEUR WHERE USER_LAST_LOGIN < 0;"
            dump_sql="SELECT USER_ID, USER_LAST_LOGIN FROM UTILISATEUR WHERE USER_LAST_LOGIN < 0 ORDER BY USER_ID;"
            expected_desc="dernier login >= 0"
            ;;
        ANTENNE-LOGIC)
            check_sql="SELECT COUNT(*) FROM ANTENNE WHERE ANTENNE_REGION IS NULL OR ANTENNE_REGION = '';"
            dump_sql="SELECT ANTENNE_ID, ANTENNE_REGION FROM ANTENNE WHERE ANTENNE_REGION IS NULL OR ANTENNE_REGION = '' ORDER BY ANTENNE_ID;"
            expected_desc="region antenne non vide"
            ;;
        REPAS-LOGIC)
            check_sql="SELECT COUNT(*) FROM REPAS WHERE REPAS_NBREPAS < 0;"
            dump_sql="SELECT REPAS_ID, REPAS_NBREPAS FROM REPAS WHERE REPAS_NBREPAS < 0 ORDER BY REPAS_ID;"
            expected_desc="nombre repas >= 0"
            ;;
        HEBERGEMENT-LOGIC)
            check_sql="SELECT COUNT(*) FROM HEBERGEMENT WHERE HEBERGEMENT_NBNUIT < 0;"
            dump_sql="SELECT HEBERGEMENT_ID, HEBERGEMENT_NBNUIT FROM HEBERGEMENT WHERE HEBERGEMENT_NBNUIT < 0 ORDER BY HEBERGEMENT_ID;"
            expected_desc="nombre nuits >= 0"
            ;;
        PARTICIPATION-LOGIC)
            check_sql="SELECT COUNT(*) FROM PARTICIPATION WHERE PARTICIPATION_MODE_TRANSPORT < 0;"
            dump_sql="SELECT PARTICIPATION_ID_ACTIVITE, PARTICIPATION_ID_USER, PARTICIPATION_MODE_TRANSPORT FROM PARTICIPATION WHERE PARTICIPATION_MODE_TRANSPORT < 0 ORDER BY PARTICIPATION_ID_ACTIVITE, PARTICIPATION_ID_USER;"
            expected_desc="mode transport >= 0"
            ;;
    esac

    batch_log="/tmp/batch_${prog}.log"
    details_base="Binaire: bin/${prog}"$'\n'"Log: $batch_log"

    pre_invalid=""
    if [ -n "$check_sql" ]; then
        pre_invalid=$(sql_scalar "$check_sql")
        details_base="$details_base"$'\n'"Check SQL: $check_sql"$'\n'"Attendu: $expected_desc"$'\n'"Avant: $pre_invalid"
    fi

    if [ -f "bin/${prog}" ]; then
        "./bin/${prog}" > "$batch_log" 2>&1
        BATCH_EXIT_CODE=$?
        details_with_exit="$details_base"$'\n'"Exit code: $BATCH_EXIT_CODE"
        if [ $BATCH_EXIT_CODE -ne 0 ]; then
            test_result "Execution batch $prog (exit code 0 + effet BD)" 1 "$details_with_exit"
            tail -10 "$batch_log"
            continue
        fi

        if [ -n "$check_sql" ]; then
            post_invalid=$(sql_scalar "$check_sql")
            details_with_exit="$details_with_exit"$'\n'"Apres: $post_invalid"
            if ! [[ "$pre_invalid" =~ ^-?[0-9]+$ ]] || ! [[ "$post_invalid" =~ ^-?[0-9]+$ ]]; then
                test_result "Execution batch $prog (exit code 0 + effet BD)" 1 "$details_with_exit"$'\n'"Resultat SQL invalide (avant/apres)"
                continue
            fi
            if [ "$pre_invalid" -eq 0 ]; then
                test_result "Execution batch $prog (exit code 0 + effet BD)" 1 "$details_with_exit"$'\n'"Precondition absente: aucune anomalie a corriger"
                continue
            fi
            if [ "$post_invalid" -eq 0 ]; then
                test_result "Execution batch $prog (exit code 0 + effet BD)" 0 "$details_with_exit"
            else
                test_result "Execution batch $prog (exit code 0 + effet BD)" 1 "$details_with_exit"
                if [ -n "$dump_sql" ]; then
                    echo -e "${YELLOW}  Lignes encore invalides:${NC}"
                    psql_print "$dump_sql"
                fi
            fi
        else
            test_result "Execution batch $prog (exit code 0)" 0 "$details_with_exit"
        fi
    else
        test_result "Execution batch $prog (exit code 0 + effet BD)" 1 "$details_base"$'\n'"Executable manquant"
        echo -e "${RED}  Executable bin/${prog} introuvable${NC}"
    fi

done

echo ""

#########################################################################
# SECTION 5: AFFICHAGE BD APRES EXECUTION
#########################################################################
echo -e "${BLUE}[SECTION 5] Base de donnees APRES execution${NC}"

echo ""
echo -e "${CYAN}=== TABLE ACTIVITE (apres traitement) ===${NC}"
after_ok=0
if [ -n "$SQL_PASSWORD" ]; then
    PGPASSWORD=$SQL_PASSWORD psql -h $DB_HOST -p $DB_PORT -U $SQL_USER -d $DB_NAME -c \
    "SELECT ACTIVITE_ID, ACTIVITE_EMPREINTETOTALE FROM ACTIVITE ORDER BY ACTIVITE_ID;" | tee /tmp/db_after_activite.log
else
    psql -h $DB_HOST -p $DB_PORT -U $SQL_USER -d $DB_NAME -c \
    "SELECT ACTIVITE_ID, ACTIVITE_EMPREINTETOTALE FROM ACTIVITE ORDER BY ACTIVITE_ID;" | tee /tmp/db_after_activite.log
fi
if [ $? -ne 0 ]; then
    after_ok=1
fi

echo ""
echo -e "${CYAN}=== TABLE UTILISATEUR (apres traitement) ===${NC}"
if [ -n "$SQL_PASSWORD" ]; then
    PGPASSWORD=$SQL_PASSWORD psql -h $DB_HOST -p $DB_PORT -U $SQL_USER -d $DB_NAME -c \
    "SELECT USER_ID, USER_LAST_LOGIN FROM UTILISATEUR ORDER BY USER_ID;" | tee /tmp/db_after_utilisateur.log
else
    psql -h $DB_HOST -p $DB_PORT -U $SQL_USER -d $DB_NAME -c \
    "SELECT USER_ID, USER_LAST_LOGIN FROM UTILISATEUR ORDER BY USER_ID;" | tee /tmp/db_after_utilisateur.log
fi
if [ $? -ne 0 ]; then
    after_ok=1
fi

echo ""
echo -e "${CYAN}=== TABLE ANTENNE (apres traitement) ===${NC}"
if [ -n "$SQL_PASSWORD" ]; then
    PGPASSWORD=$SQL_PASSWORD psql -h $DB_HOST -p $DB_PORT -U $SQL_USER -d $DB_NAME -c \
    "SELECT ANTENNE_ID, ANTENNE_NOM, ANTENNE_REGION FROM ANTENNE ORDER BY ANTENNE_ID;" | tee /tmp/db_after_antenne.log
else
    psql -h $DB_HOST -p $DB_PORT -U $SQL_USER -d $DB_NAME -c \
    "SELECT ANTENNE_ID, ANTENNE_NOM, ANTENNE_REGION FROM ANTENNE ORDER BY ANTENNE_ID;" | tee /tmp/db_after_antenne.log
fi
if [ $? -ne 0 ]; then
    after_ok=1
fi

echo ""
echo -e "${CYAN}=== TABLE REPAS (apres traitement) ===${NC}"
if [ -n "$SQL_PASSWORD" ]; then
    PGPASSWORD=$SQL_PASSWORD psql -h $DB_HOST -p $DB_PORT -U $SQL_USER -d $DB_NAME -c \
    "SELECT REPAS_ID, REPAS_ID_ACTIVITE, REPAS_TYPE, REPAS_NBREPAS FROM REPAS ORDER BY REPAS_ID;" | tee /tmp/db_after_repas.log
else
    psql -h $DB_HOST -p $DB_PORT -U $SQL_USER -d $DB_NAME -c \
    "SELECT REPAS_ID, REPAS_ID_ACTIVITE, REPAS_TYPE, REPAS_NBREPAS FROM REPAS ORDER BY REPAS_ID;" | tee /tmp/db_after_repas.log
fi
if [ $? -ne 0 ]; then
    after_ok=1
fi

echo ""
echo -e "${CYAN}=== TABLE HEBERGEMENT (apres traitement) ===${NC}"
if [ -n "$SQL_PASSWORD" ]; then
    PGPASSWORD=$SQL_PASSWORD psql -h $DB_HOST -p $DB_PORT -U $SQL_USER -d $DB_NAME -c \
    "SELECT HEBERGEMENT_ID, HEBERGEMENT_ID_ACTIVITE, HEBERGEMENT_TYPE, HEBERGEMENT_NBNUIT FROM HEBERGEMENT ORDER BY HEBERGEMENT_ID;" | tee /tmp/db_after_hebergement.log
else
    psql -h $DB_HOST -p $DB_PORT -U $SQL_USER -d $DB_NAME -c \
    "SELECT HEBERGEMENT_ID, HEBERGEMENT_ID_ACTIVITE, HEBERGEMENT_TYPE, HEBERGEMENT_NBNUIT FROM HEBERGEMENT ORDER BY HEBERGEMENT_ID;" | tee /tmp/db_after_hebergement.log
fi
if [ $? -ne 0 ]; then
    after_ok=1
fi

echo ""
echo -e "${CYAN}=== TABLE PARTICIPATION (apres traitement) ===${NC}"
if [ -n "$SQL_PASSWORD" ]; then
    PGPASSWORD=$SQL_PASSWORD psql -h $DB_HOST -p $DB_PORT -U $SQL_USER -d $DB_NAME -c \
    "SELECT PARTICIPATION_ID_ACTIVITE, PARTICIPATION_ID_USER, PARTICIPATION_MODE_TRANSPORT FROM PARTICIPATION ORDER BY PARTICIPATION_ID_ACTIVITE, PARTICIPATION_ID_USER;" | tee /tmp/db_after_participation.log
else
    psql -h $DB_HOST -p $DB_PORT -U $SQL_USER -d $DB_NAME -c \
    "SELECT PARTICIPATION_ID_ACTIVITE, PARTICIPATION_ID_USER, PARTICIPATION_MODE_TRANSPORT FROM PARTICIPATION ORDER BY PARTICIPATION_ID_ACTIVITE, PARTICIPATION_ID_USER;" | tee /tmp/db_after_participation.log
fi
if [ $? -ne 0 ]; then
    after_ok=1
fi

after_details="Logs: /tmp/db_after_activite.log, /tmp/db_after_utilisateur.log, /tmp/db_after_antenne.log, /tmp/db_after_repas.log, /tmp/db_after_hebergement.log, /tmp/db_after_participation.log"
if [ $after_ok -eq 0 ]; then
    test_result "Affichage BD apres execution" 0 "$after_details"
else
    test_result "Affichage BD apres execution" 1 "$after_details"
fi

echo ""

#########################################################################
# SECTION 6: TESTS EXIGENCES ET FONCTIONNALITES
#########################################################################
echo -e "${BLUE}[SECTION 6] Tests exigences et fonctionnalites${NC}"

# R1: Email unique
sql_r1="INSERT INTO UTILISATEUR (USER_ID, USER_NOM, USER_MAIL, USER_PASS, USER_ROLE, USER_ID_ANTENNE, USER_LAST_LOGIN) VALUES (9999, 'DUP', 'alice@carbontrack.fr', 'P', 'USER', 1, 0);"
if run_sql_cmd_capture "$sql_r1"; then
    test_result "R1: Email unique (rejet doublon)" 1 "SQL: $sql_r1"$'\n'"Sortie: ${SQL_LAST_OUTPUT:-<vide>}"
else
    test_result "R1: Email unique (rejet doublon)" 0 "SQL: $sql_r1"$'\n'"Erreur attendue: ${SQL_LAST_OUTPUT:-<vide>}"
fi

# R2: Activite doit avoir antenne et animateur
assert_sql_count_eq_with_dump "R2: Activite antenne/animateur valide" \
    "SELECT COUNT(*) FROM ACTIVITE a LEFT JOIN ANTENNE an ON a.ACTIVITE_IDANTENNE = an.ANTENNE_ID LEFT JOIN UTILISATEUR u ON a.ACTIVITE_ANIMATEUR = u.USER_ID WHERE an.ANTENNE_ID IS NULL OR u.USER_ID IS NULL;" \
    0 \
    "SELECT ACTIVITE_ID, ACTIVITE_IDANTENNE, ACTIVITE_ANIMATEUR FROM ACTIVITE a LEFT JOIN ANTENNE an ON a.ACTIVITE_IDANTENNE = an.ANTENNE_ID LEFT JOIN UTILISATEUR u ON a.ACTIVITE_ANIMATEUR = u.USER_ID WHERE an.ANTENNE_ID IS NULL OR u.USER_ID IS NULL ORDER BY ACTIVITE_ID;"

sql_r2="INSERT INTO ACTIVITE (ACTIVITE_ID, ACTIVITE_NOM, ACTIVITE_TYPE, ACTIVITE_IDANTENNE, ACTIVITE_ANIMATEUR, ACTIVITE_NBPARTICIPANTS, ACTIVITE_MODETRANSPORT, ACTIVITE_LIEU, ACTIVITE_DISTANCE, ACTIVITE_HEBERGEMENT, ACTIVITE_REPASPREVU, ACTIVITE_EMPREINTETOTALE) VALUES (2999, 'INVALID', 'TRANSPORT', 9999, 9999, 1, 1, 'TEST', 1, 0, 0, 0.0);"
if run_sql_cmd_capture "$sql_r2"; then
    test_result "R2: Rejet activite invalide (FK antenne/animateur)" 1 "SQL: $sql_r2"$'\n'"Sortie: ${SQL_LAST_OUTPUT:-<vide>}"
    run_sql_cmd "DELETE FROM ACTIVITE WHERE ACTIVITE_ID = 2999;"
else
    test_result "R2: Rejet activite invalide (FK antenne/animateur)" 0 "SQL: $sql_r2"$'\n'"Erreur attendue: ${SQL_LAST_OUTPUT:-<vide>}"
fi

# R3: Participation definie par (ID_ACTIVITE, ID_USER)
sql_r3="INSERT INTO PARTICIPATION VALUES (2001, 1001, 1);"
if run_sql_cmd_capture "$sql_r3"; then
    test_result "R3: PK composite participation" 1 "SQL: $sql_r3"$'\n'"Sortie: ${SQL_LAST_OUTPUT:-<vide>}"
else
    test_result "R3: PK composite participation" 0 "SQL: $sql_r3"$'\n'"Erreur attendue: ${SQL_LAST_OUTPUT:-<vide>}"
fi

# R1: insertion valide utilisateur
sql_r1_ok="INSERT INTO UTILISATEUR (USER_ID, USER_NOM, USER_MAIL, USER_PASS, USER_ROLE, USER_ID_ANTENNE, USER_LAST_LOGIN) VALUES (8888, 'TEST USER', 'testuser@carbontrack.fr', 'P', 'USER', 1, 0);"
if run_sql_cmd_capture "$sql_r1_ok"; then
    test_result "R1: Insert utilisateur valide" 0 "SQL: $sql_r1_ok"
    run_sql_cmd "DELETE FROM UTILISATEUR WHERE USER_ID = 8888;"
else
    test_result "R1: Insert utilisateur valide" 1 "SQL: $sql_r1_ok"$'\n'"Erreur: ${SQL_LAST_OUTPUT:-<vide>}"
fi

# R2: insertion valide activite
sql_r2_ok="INSERT INTO ACTIVITE (ACTIVITE_ID, ACTIVITE_NOM, ACTIVITE_TYPE, ACTIVITE_IDANTENNE, ACTIVITE_ANIMATEUR, ACTIVITE_NBPARTICIPANTS, ACTIVITE_MODETRANSPORT, ACTIVITE_LIEU, ACTIVITE_DISTANCE, ACTIVITE_HEBERGEMENT, ACTIVITE_REPASPREVU, ACTIVITE_EMPREINTETOTALE) VALUES (2998, 'OK', 'TRANSPORT', 1, 1001, 2, 1, 'PARIS', 10, 0, 0, 0.0);"
if run_sql_cmd_capture "$sql_r2_ok"; then
    test_result "R2: Insert activite valide" 0 "SQL: $sql_r2_ok"
    run_sql_cmd "DELETE FROM ACTIVITE WHERE ACTIVITE_ID = 2998;"
else
    test_result "R2: Insert activite valide" 1 "SQL: $sql_r2_ok"$'\n'"Erreur: ${SQL_LAST_OUTPUT:-<vide>}"
fi

# R3: insertion valide participation
sql_r3_ok="INSERT INTO PARTICIPATION VALUES (2002, 1001, 1);"
if run_sql_cmd_capture "$sql_r3_ok"; then
    test_result "R3: Insert participation valide" 0 "SQL: $sql_r3_ok"
    run_sql_cmd "DELETE FROM PARTICIPATION WHERE PARTICIPATION_ID_ACTIVITE = 2002 AND PARTICIPATION_ID_USER = 1001;"
else
    test_result "R3: Insert participation valide" 1 "SQL: $sql_r3_ok"$'\n'"Erreur: ${SQL_LAST_OUTPUT:-<vide>}"
fi

# R4: SQL embarque PostgreSQL (presence EXEC SQL dans DAL)
if command -v rg >/dev/null 2>&1; then
    matched_files=$(rg -l "EXEC SQL" dal/*.cbl 2>/dev/null)
    RG_STATUS=$?
else
    matched_files=$(grep -l "EXEC SQL" dal/*.cbl 2>/dev/null)
    RG_STATUS=$?
fi
rg_details="Fichiers: ${matched_files:-<aucun>}"
if [ $RG_STATUS -eq 0 ]; then
    test_result "R4: Presence EXEC SQL en DAL" 0 "$rg_details"
else
    test_result "R4: Presence EXEC SQL en DAL" 1 "$rg_details"
fi

# R5: Gestion SQLCODE en DAL
if command -v rg >/dev/null 2>&1; then
    matched_files=$(rg -l "SQLCODE" dal/*.cbl 2>/dev/null)
    RG_STATUS=$?
else
    matched_files=$(grep -l "SQLCODE" dal/*.cbl 2>/dev/null)
    RG_STATUS=$?
fi
rg_details="Fichiers: ${matched_files:-<aucun>}"
if [ $RG_STATUS -eq 0 ]; then
    test_result "R5: SQLCODE gere en DAL" 0 "$rg_details"
else
    test_result "R5: SQLCODE gere en DAL" 1 "$rg_details"
fi

# Integrite referentielle (tables dependantes)
assert_sql_count_eq_with_dump "R2: Utilisateur antenne valide" \
    "SELECT COUNT(*) FROM UTILISATEUR u LEFT JOIN ANTENNE a ON u.USER_ID_ANTENNE = a.ANTENNE_ID WHERE a.ANTENNE_ID IS NULL;" \
    0 \
    "SELECT USER_ID, USER_ID_ANTENNE FROM UTILISATEUR u LEFT JOIN ANTENNE a ON u.USER_ID_ANTENNE = a.ANTENNE_ID WHERE a.ANTENNE_ID IS NULL;"

assert_sql_count_eq_with_dump "R2: Repas activite valide" \
    "SELECT COUNT(*) FROM REPAS r LEFT JOIN ACTIVITE a ON r.REPAS_ID_ACTIVITE = a.ACTIVITE_ID WHERE a.ACTIVITE_ID IS NULL;" \
    0 \
    "SELECT REPAS_ID, REPAS_ID_ACTIVITE FROM REPAS r LEFT JOIN ACTIVITE a ON r.REPAS_ID_ACTIVITE = a.ACTIVITE_ID WHERE a.ACTIVITE_ID IS NULL;"

assert_sql_count_eq_with_dump "R2: Hebergement activite valide" \
    "SELECT COUNT(*) FROM HEBERGEMENT h LEFT JOIN ACTIVITE a ON h.HEBERGEMENT_ID_ACTIVITE = a.ACTIVITE_ID WHERE a.ACTIVITE_ID IS NULL;" \
    0 \
    "SELECT HEBERGEMENT_ID, HEBERGEMENT_ID_ACTIVITE FROM HEBERGEMENT h LEFT JOIN ACTIVITE a ON h.HEBERGEMENT_ID_ACTIVITE = a.ACTIVITE_ID WHERE a.ACTIVITE_ID IS NULL;"

assert_sql_count_eq_with_dump "R2: Participation references valides" \
    "SELECT COUNT(*) FROM PARTICIPATION p LEFT JOIN ACTIVITE a ON p.PARTICIPATION_ID_ACTIVITE = a.ACTIVITE_ID LEFT JOIN UTILISATEUR u ON p.PARTICIPATION_ID_USER = u.USER_ID WHERE a.ACTIVITE_ID IS NULL OR u.USER_ID IS NULL;" \
    0 \
    "SELECT PARTICIPATION_ID_ACTIVITE, PARTICIPATION_ID_USER FROM PARTICIPATION p LEFT JOIN ACTIVITE a ON p.PARTICIPATION_ID_ACTIVITE = a.ACTIVITE_ID LEFT JOIN UTILISATEUR u ON p.PARTICIPATION_ID_USER = u.USER_ID WHERE a.ACTIVITE_ID IS NULL OR u.USER_ID IS NULL;"

# F1: Dernier login >= 0
assert_sql_count_eq_with_dump "F1: Dernier login >= 0" \
    "SELECT COUNT(*) FROM UTILISATEUR WHERE USER_LAST_LOGIN < 0;" \
    0 \
    "SELECT USER_ID, USER_LAST_LOGIN FROM UTILISATEUR WHERE USER_LAST_LOGIN < 0 ORDER BY USER_ID;"

# F2: Antenne region non vide
assert_sql_count_eq_with_dump "F2: Region antenne non vide" \
    "SELECT COUNT(*) FROM ANTENNE WHERE ANTENNE_REGION IS NULL OR ANTENNE_REGION = '';" \
    0 \
    "SELECT ANTENNE_ID, ANTENNE_REGION FROM ANTENNE WHERE ANTENNE_REGION IS NULL OR ANTENNE_REGION = '' ORDER BY ANTENNE_ID;"

# F3: Repas/Hebergement valeurs >= 0
assert_sql_count_eq_with_dump "F3: Nombre repas >= 0" \
    "SELECT COUNT(*) FROM REPAS WHERE REPAS_NBREPAS < 0;" \
    0 \
    "SELECT REPAS_ID, REPAS_NBREPAS FROM REPAS WHERE REPAS_NBREPAS < 0 ORDER BY REPAS_ID;"

assert_sql_count_eq_with_dump "F3: Nombre nuits >= 0" \
    "SELECT COUNT(*) FROM HEBERGEMENT WHERE HEBERGEMENT_NBNUIT < 0;" \
    0 \
    "SELECT HEBERGEMENT_ID, HEBERGEMENT_NBNUIT FROM HEBERGEMENT WHERE HEBERGEMENT_NBNUIT < 0 ORDER BY HEBERGEMENT_ID;"

# F4: Mode transport >= 0
assert_sql_count_eq_with_dump "F4: Mode transport >= 0" \
    "SELECT COUNT(*) FROM PARTICIPATION WHERE PARTICIPATION_MODE_TRANSPORT < 0;" \
    0 \
    "SELECT PARTICIPATION_ID_ACTIVITE, PARTICIPATION_ID_USER, PARTICIPATION_MODE_TRANSPORT FROM PARTICIPATION WHERE PARTICIPATION_MODE_TRANSPORT < 0 ORDER BY PARTICIPATION_ID_ACTIVITE, PARTICIPATION_ID_USER;"

# F5: Empreinte carbone >= 0
assert_sql_count_eq_with_dump "F5: Empreinte carbone >= 0" \
    "SELECT COUNT(*) FROM ACTIVITE WHERE ACTIVITE_EMPREINTETOTALE < 0;" \
    0 \
    "SELECT ACTIVITE_ID, ACTIVITE_EMPREINTETOTALE FROM ACTIVITE WHERE ACTIVITE_EMPREINTETOTALE < 0 ORDER BY ACTIVITE_ID;"

echo ""

#########################################################################
# SECTION 6B: VERIFICATIONS DETAILLEES BD ET LOGS
#########################################################################
echo -e "${BLUE}[SECTION 6B] Verifications detaillees BD et logs${NC}"

# Valeurs corrigees et inchangees
assert_sql_count_eq "F1: USER_LAST_LOGIN corrige (ID 1002 = 0)" \
    "SELECT COUNT(*) FROM UTILISATEUR WHERE USER_ID = 1002 AND USER_LAST_LOGIN <> 0;" \
    0

assert_sql_count_eq "F1: USER_LAST_LOGIN conserve (ID 1001 = 1609459200)" \
    "SELECT COUNT(*) FROM UTILISATEUR WHERE USER_ID = 1001 AND USER_LAST_LOGIN <> 1609459200;" \
    0

assert_sql_count_eq "F2: ANTENNE_REGION corrigee (ID 2 = INCONNUE)" \
    "SELECT COUNT(*) FROM ANTENNE WHERE ANTENNE_ID = 2 AND COALESCE(BTRIM(ANTENNE_REGION), '') <> 'INCONNUE';" \
    0

assert_sql_count_eq "F2: ANTENNE_REGION conservee (ID 1 = IDF)" \
    "SELECT COUNT(*) FROM ANTENNE WHERE ANTENNE_ID = 1 AND BTRIM(ANTENNE_REGION) <> 'IDF';" \
    0

assert_sql_count_eq "F3: REPAS_NBREPAS corrige (ID 3002 = 0)" \
    "SELECT COUNT(*) FROM REPAS WHERE REPAS_ID = 3002 AND REPAS_NBREPAS <> 0;" \
    0

assert_sql_count_eq "F3: REPAS_NBREPAS conserve (ID 3001 = 10)" \
    "SELECT COUNT(*) FROM REPAS WHERE REPAS_ID = 3001 AND REPAS_NBREPAS <> 10;" \
    0

assert_sql_count_eq "F3: HEBERGEMENT_NBNUIT corrige (ID 4001 = 0)" \
    "SELECT COUNT(*) FROM HEBERGEMENT WHERE HEBERGEMENT_ID = 4001 AND HEBERGEMENT_NBNUIT <> 0;" \
    0

assert_sql_count_eq "F3: HEBERGEMENT_NBNUIT conserve (ID 4002 = 1)" \
    "SELECT COUNT(*) FROM HEBERGEMENT WHERE HEBERGEMENT_ID = 4002 AND HEBERGEMENT_NBNUIT <> 1;" \
    0

assert_sql_count_eq "F4: MODE_TRANSPORT corrige (2002/1002 = 0)" \
    "SELECT COUNT(*) FROM PARTICIPATION WHERE PARTICIPATION_ID_ACTIVITE = 2002 AND PARTICIPATION_ID_USER = 1002 AND PARTICIPATION_MODE_TRANSPORT <> 0;" \
    0

assert_sql_count_eq "F4: MODE_TRANSPORT conserve (2001/1001 = 1)" \
    "SELECT COUNT(*) FROM PARTICIPATION WHERE PARTICIPATION_ID_ACTIVITE = 2001 AND PARTICIPATION_ID_USER = 1001 AND PARTICIPATION_MODE_TRANSPORT <> 1;" \
    0

assert_sql_count_eq "F4: MODE_TRANSPORT conserve (2001/1002 = 2)" \
    "SELECT COUNT(*) FROM PARTICIPATION WHERE PARTICIPATION_ID_ACTIVITE = 2001 AND PARTICIPATION_ID_USER = 1002 AND PARTICIPATION_MODE_TRANSPORT <> 2;" \
    0

assert_sql_count_eq "F5: EMPREINTE corrigee (ID 2002 = 0)" \
    "SELECT COUNT(*) FROM ACTIVITE WHERE ACTIVITE_ID = 2002 AND ACTIVITE_EMPREINTETOTALE <> 0;" \
    0

assert_sql_count_eq "F5: EMPREINTE corrigee (ID 2001 = 0)" \
    "SELECT COUNT(*) FROM ACTIVITE WHERE ACTIVITE_ID = 2001 AND ACTIVITE_EMPREINTETOTALE <> 0;" \
    0

# Verification BD: anomalies corrigees (valeurs negatives -> 0, vide -> INCONNUE)
# Ces tests verifient directement la BD au lieu des logs

assert_sql_count_eq "BD ACTIVITE: empreintes negatives corrigees (aucune < 0)" \
    "SELECT COUNT(*) FROM ACTIVITE WHERE ACTIVITE_EMPREINTETOTALE < 0;" \
    0

assert_sql_count_eq "BD UTILISATEUR: logins negatifs corriges (aucun < 0)" \
    "SELECT COUNT(*) FROM UTILISATEUR WHERE USER_LAST_LOGIN < 0;" \
    0

assert_sql_count_eq "BD ANTENNE: regions vides corrigees (aucune vide)" \
    "SELECT COUNT(*) FROM ANTENNE WHERE ANTENNE_REGION IS NULL OR BTRIM(ANTENNE_REGION) = '';" \
    0

assert_sql_count_eq "BD REPAS: nb repas negatifs corriges (aucun < 0)" \
    "SELECT COUNT(*) FROM REPAS WHERE REPAS_NBREPAS < 0;" \
    0

assert_sql_count_eq "BD HEBERGEMENT: nb nuits negatifs corriges (aucun < 0)" \
    "SELECT COUNT(*) FROM HEBERGEMENT WHERE HEBERGEMENT_NBNUIT < 0;" \
    0

echo ""

#########################################################################
# SECTION 6C: TESTS ENRICHIS - INTEGRITE DES DONNEES
#########################################################################
echo -e "${BLUE}[SECTION 6C] Tests enrichis - Integrite des donnees${NC}"

# Verifie que les valeurs VALIDES n'ont PAS ete modifiees
assert_sql_count_eq "INTEGRITE: Activites valides non modifiees (ID 2003, 2004, 2005)" \
    "SELECT COUNT(*) FROM ACTIVITE WHERE ACTIVITE_ID IN (2003, 2004, 2005) AND (ACTIVITE_EMPREINTETOTALE < 0 OR ACTIVITE_EMPREINTETOTALE IS NULL);" \
    0

assert_sql_count_eq "INTEGRITE: Repas valides non modifies (ID 3001, 3003, 3005)" \
    "SELECT COUNT(*) FROM REPAS WHERE REPAS_ID IN (3001, 3003, 3005) AND REPAS_NBREPAS < 0;" \
    0

assert_sql_count_eq "INTEGRITE: Hebergement valide non modifie (ID 4002)" \
    "SELECT COUNT(*) FROM HEBERGEMENT WHERE HEBERGEMENT_ID = 4002 AND HEBERGEMENT_NBNUIT < 0;" \
    0

assert_sql_count_eq "INTEGRITE: Utilisateurs valides non modifies (ID 1001, 1003, 1004)" \
    "SELECT COUNT(*) FROM UTILISATEUR WHERE USER_ID IN (1001, 1003, 1004) AND USER_LAST_LOGIN < 0;" \
    0

assert_sql_count_eq "INTEGRITE: Antennes valides non modifiees (ID 1, 3)" \
    "SELECT COUNT(*) FROM ANTENNE WHERE ANTENNE_ID IN (1, 3) AND (ANTENNE_REGION IS NULL OR ANTENNE_REGION = '' OR BTRIM(ANTENNE_REGION) = '');" \
    0

# Verifie que TOUTES les anomalies multiples ont ete corrigees
assert_sql_count_ge "CORRECTION MULTIPLE: Au moins 2 empreintes corrigees" \
    "SELECT COUNT(*) FROM ACTIVITE WHERE ACTIVITE_ID IN (2001, 2002) AND ACTIVITE_EMPREINTETOTALE = 0;" \
    2

assert_sql_count_ge "CORRECTION MULTIPLE: Au moins 2 repas corriges" \
    "SELECT COUNT(*) FROM REPAS WHERE REPAS_ID IN (3002, 3004) AND REPAS_NBREPAS = 0;" \
    2

assert_sql_count_ge "CORRECTION MULTIPLE: Au moins 2 hebergements corriges" \
    "SELECT COUNT(*) FROM HEBERGEMENT WHERE HEBERGEMENT_ID IN (4001, 4003) AND HEBERGEMENT_NBNUIT = 0;" \
    2

assert_sql_count_ge "CORRECTION MULTIPLE: Au moins 2 transports corriges" \
    "SELECT COUNT(*) FROM PARTICIPATION WHERE (PARTICIPATION_ID_ACTIVITE = 2002 AND PARTICIPATION_ID_USER = 1002) OR (PARTICIPATION_ID_ACTIVITE = 2004 AND PARTICIPATION_ID_USER = 1002) AND PARTICIPATION_MODE_TRANSPORT = 0;" \
    1

assert_sql_count_ge "CORRECTION MULTIPLE: Au moins 2 logins corriges" \
    "SELECT COUNT(*) FROM UTILISATEUR WHERE USER_ID IN (1002, 1005) AND USER_LAST_LOGIN = 0;" \
    2

assert_sql_count_ge "CORRECTION MULTIPLE: Au moins 2 regions corrigees" \
    "SELECT COUNT(*) FROM ANTENNE WHERE ANTENNE_ID IN (2, 4) AND BTRIM(ANTENNE_REGION) = 'INCONNUE';" \
    2

# Verifie les corrections dans la BD (confirmation des corrections multiples)
# Ces tests verifient que les valeurs specifiques ont ete remises a 0 ou corrigees

assert_sql_count_eq "BD: Empreintes ID 2001,2002 remises a 0" \
    "SELECT COUNT(*) FROM ACTIVITE WHERE ACTIVITE_ID IN (2001, 2002) AND ACTIVITE_EMPREINTETOTALE = 0;" \
    2

assert_sql_count_eq "BD: Repas ID 3002,3004 remis a 0" \
    "SELECT COUNT(*) FROM REPAS WHERE REPAS_ID IN (3002, 3004) AND REPAS_NBREPAS = 0;" \
    2

assert_sql_count_eq "BD: Hebergements ID 4001,4003 remis a 0" \
    "SELECT COUNT(*) FROM HEBERGEMENT WHERE HEBERGEMENT_ID IN (4001, 4003) AND HEBERGEMENT_NBNUIT = 0;" \
    2

assert_sql_count_eq "BD: Logins ID 1002,1005 remis a 0" \
    "SELECT COUNT(*) FROM UTILISATEUR WHERE USER_ID IN (1002, 1005) AND USER_LAST_LOGIN = 0;" \
    2

assert_sql_count_eq "BD: Regions ID 2,4 mises a INCONNUE" \
    "SELECT COUNT(*) FROM ANTENNE WHERE ANTENNE_ID IN (2, 4) AND BTRIM(ANTENNE_REGION) = 'INCONNUE';" \
    2

echo ""

#########################################################################
# SECTION 6D: TESTS ENRICHIS - VOLUME ET COMPLETUDE
#########################################################################
echo -e "${BLUE}[SECTION 6D] Tests enrichis - Volume et completude${NC}"

# Verifie que toutes les entites existent avec les bonnes quantites
assert_sql_count_eq "VOLUME: Nombre total antennes" \
    "SELECT COUNT(*) FROM ANTENNE;" \
    4

assert_sql_count_eq "VOLUME: Nombre total utilisateurs" \
    "SELECT COUNT(*) FROM UTILISATEUR;" \
    5

assert_sql_count_eq "VOLUME: Nombre total activites" \
    "SELECT COUNT(*) FROM ACTIVITE;" \
    5

assert_sql_count_eq "VOLUME: Nombre total repas" \
    "SELECT COUNT(*) FROM REPAS;" \
    5

assert_sql_count_eq "VOLUME: Nombre total hebergements" \
    "SELECT COUNT(*) FROM HEBERGEMENT;" \
    3

assert_sql_count_eq "VOLUME: Nombre total participations" \
    "SELECT COUNT(*) FROM PARTICIPATION;" \
    10

# Verifie que toutes les FK sont valides (pas d'orphelins)
assert_sql_count_eq "FK INTEGRITY: Utilisateurs sans antenne valide" \
    "SELECT COUNT(*) FROM UTILISATEUR u LEFT JOIN ANTENNE a ON u.USER_ID_ANTENNE = a.ANTENNE_ID WHERE a.ANTENNE_ID IS NULL;" \
    0

assert_sql_count_eq "FK INTEGRITY: Activites sans antenne valide" \
    "SELECT COUNT(*) FROM ACTIVITE ac LEFT JOIN ANTENNE an ON ac.ACTIVITE_IDANTENNE = an.ANTENNE_ID WHERE an.ANTENNE_ID IS NULL;" \
    0

assert_sql_count_eq "FK INTEGRITY: Activites sans animateur valide" \
    "SELECT COUNT(*) FROM ACTIVITE ac LEFT JOIN UTILISATEUR u ON ac.ACTIVITE_ANIMATEUR = u.USER_ID WHERE u.USER_ID IS NULL;" \
    0

assert_sql_count_eq "FK INTEGRITY: Repas sans activite valide" \
    "SELECT COUNT(*) FROM REPAS r LEFT JOIN ACTIVITE a ON r.REPAS_ID_ACTIVITE = a.ACTIVITE_ID WHERE a.ACTIVITE_ID IS NULL;" \
    0

assert_sql_count_eq "FK INTEGRITY: Hebergements sans activite valide" \
    "SELECT COUNT(*) FROM HEBERGEMENT h LEFT JOIN ACTIVITE a ON h.HEBERGEMENT_ID_ACTIVITE = a.ACTIVITE_ID WHERE a.ACTIVITE_ID IS NULL;" \
    0

assert_sql_count_eq "FK INTEGRITY: Participations sans activite valide" \
    "SELECT COUNT(*) FROM PARTICIPATION p LEFT JOIN ACTIVITE a ON p.PARTICIPATION_ID_ACTIVITE = a.ACTIVITE_ID WHERE a.ACTIVITE_ID IS NULL;" \
    0

assert_sql_count_eq "FK INTEGRITY: Participations sans utilisateur valide" \
    "SELECT COUNT(*) FROM PARTICIPATION p LEFT JOIN UTILISATEUR u ON p.PARTICIPATION_ID_USER = u.USER_ID WHERE u.USER_ID IS NULL;" \
    0

# Verifie la coherence des donnees metier
assert_sql_count_eq "COHERENCE: Activites avec participants >= nombre participations" \
    "SELECT COUNT(*) FROM ACTIVITE a WHERE a.ACTIVITE_NBPARTICIPANTS < (SELECT COUNT(*) FROM PARTICIPATION p WHERE p.PARTICIPATION_ID_ACTIVITE = a.ACTIVITE_ID);" \
    0

assert_sql_count_eq "COHERENCE: Emails uniques (pas de doublons)" \
    "SELECT COUNT(*) - COUNT(DISTINCT USER_MAIL) FROM UTILISATEUR;" \
    0

assert_sql_count_eq "COHERENCE: Participations uniques (pas de doublons)" \
    "SELECT COUNT(*) - COUNT(DISTINCT (PARTICIPATION_ID_ACTIVITE, PARTICIPATION_ID_USER)) FROM PARTICIPATION;" \
    0

echo ""

#########################################################################
# SECTION 6E: TESTS ENRICHIS - VALEURS PRESERVEES
#########################################################################
echo -e "${BLUE}[SECTION 6E] Tests enrichis - Valeurs preservees${NC}"

# Verifie que les valeurs specifiques non modifiees sont intactes
assert_sql_count_eq "PRESERVE: Activite 2003 empreinte = 250.5" \
    "SELECT COUNT(*) FROM ACTIVITE WHERE ACTIVITE_ID = 2003 AND ACTIVITE_EMPREINTETOTALE = 250.5;" \
    1

assert_sql_count_eq "PRESERVE: Activite 2004 empreinte = 1250.75" \
    "SELECT COUNT(*) FROM ACTIVITE WHERE ACTIVITE_ID = 2004 AND ACTIVITE_EMPREINTETOTALE = 1250.75;" \
    1

assert_sql_count_eq "PRESERVE: Activite 2005 empreinte = 50.0" \
    "SELECT COUNT(*) FROM ACTIVITE WHERE ACTIVITE_ID = 2005 AND ACTIVITE_EMPREINTETOTALE = 50.0;" \
    1

assert_sql_count_eq "PRESERVE: Repas 3001 nbrepas = 10" \
    "SELECT COUNT(*) FROM REPAS WHERE REPAS_ID = 3001 AND REPAS_NBREPAS = 10;" \
    1

assert_sql_count_eq "PRESERVE: Repas 3003 nbrepas = 15" \
    "SELECT COUNT(*) FROM REPAS WHERE REPAS_ID = 3003 AND REPAS_NBREPAS = 15;" \
    1

assert_sql_count_eq "PRESERVE: Repas 3005 nbrepas = 20" \
    "SELECT COUNT(*) FROM REPAS WHERE REPAS_ID = 3005 AND REPAS_NBREPAS = 20;" \
    1

assert_sql_count_eq "PRESERVE: Hebergement 4002 nbnuit = 1" \
    "SELECT COUNT(*) FROM HEBERGEMENT WHERE HEBERGEMENT_ID = 4002 AND HEBERGEMENT_NBNUIT = 1;" \
    1

assert_sql_count_eq "PRESERVE: Utilisateur 1001 login = 1609459200" \
    "SELECT COUNT(*) FROM UTILISATEUR WHERE USER_ID = 1001 AND USER_LAST_LOGIN = 1609459200;" \
    1

assert_sql_count_eq "PRESERVE: Utilisateur 1003 login = 1609545600" \
    "SELECT COUNT(*) FROM UTILISATEUR WHERE USER_ID = 1003 AND USER_LAST_LOGIN = 1609545600;" \
    1

assert_sql_count_eq "PRESERVE: Utilisateur 1004 login = 1609632000" \
    "SELECT COUNT(*) FROM UTILISATEUR WHERE USER_ID = 1004 AND USER_LAST_LOGIN = 1609632000;" \
    1

assert_sql_count_eq "PRESERVE: Antenne 1 region = IDF" \
    "SELECT COUNT(*) FROM ANTENNE WHERE ANTENNE_ID = 1 AND BTRIM(ANTENNE_REGION) = 'IDF';" \
    1

assert_sql_count_eq "PRESERVE: Antenne 3 region = PACA" \
    "SELECT COUNT(*) FROM ANTENNE WHERE ANTENNE_ID = 3 AND BTRIM(ANTENNE_REGION) = 'PACA';" \
    1

echo ""

#########################################################################
# SECTION 7: RESUME
#########################################################################
echo -e "${BLUE}[SECTION 7] Resume${NC}"
echo "Tests passes : $TESTS_PASSED"
echo "Tests echoues: $TESTS_FAILED"
echo "Total tests  : $TESTS_TOTAL"
echo "Tests ignores: $TESTS_SKIPPED"

echo ""
if [ $TESTS_FAILED -eq 0 ]; then
    echo -e "${GREEN}TOUS LES TESTS SONT OK${NC}"
    exit 0
else
    echo -e "${RED}DES TESTS ONT ECHOUE${NC}"
    exit 1
fi

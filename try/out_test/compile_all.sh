#!/bin/bash
#########################################################################
# compile_all.sh - Script de compilation des programmes COBOL          #
#                                                                       #
# Ce script compile les 3 programmes COBOL avec ocesql (PostgreSQL)    #
# et GnuCOBOL                                                           #
#                                                                       #
# Usage: ./compile_all.sh                                               #
#########################################################################

# Always run from the script directory
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
cd "$SCRIPT_DIR"

# Configuration
COPYBOOK_DIR="./copy"
OUTPUT_DIR="./bin"
PRECOMPILE_DIR="./precompiled"

# Couleurs pour l'affichage
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo "=========================================="
echo "Compilation des programmes COBOL"
echo "=========================================="

# Creation des repertoires de sortie
mkdir -p $OUTPUT_DIR
mkdir -p $PRECOMPILE_DIR

# Verification des outils necessaires
echo "Verification des outils..."

if ! command -v ocesql &> /dev/null; then
    echo -e "${RED}ERREUR: ocesql n'est pas installe${NC}"
    echo "Installez ocesql: https://github.com/opensourcecobol/Open-COBOL-ESQL"
    exit 1
fi

if ! command -v cobc &> /dev/null; then
    echo -e "${RED}ERREUR: GnuCOBOL (cobc) n'est pas installe${NC}"
    echo "Installez GnuCOBOL: sudo apt-get install gnucobol"
    exit 1
fi

echo -e "${GREEN}Outils detectes: ocesql et cobc${NC}"

# Fonction de compilation
compile_program() {
    local src_file=$1
    local prog_name=$2
    local layer=$3
    local cobc_mode=$4
    local output_name=$5
    local cobc_extra=$6

    echo ""
    echo -e "${YELLOW}Compilation de $prog_name ($layer)...${NC}"

    # 1. Precompilation avec ocesql (conversion SQL embedded -> COBOL)
    echo "  [1/3] Precompilation SQL (ocesql)..."
    # Syntaxe ocesql: ocesql [options] SOURCE [DESTFILE] [LOGFILE]
    # --inc=dir pour les includes
    ocesql --inc=$COPYBOOK_DIR \
           $src_file \
           $PRECOMPILE_DIR/${prog_name}.cob

    if [ $? -ne 0 ]; then
        echo -e "${RED}ERREUR lors de la precompilation de $prog_name${NC}"
        return 1
    fi

    # 2. Compilation COBOL -> module objet
    echo "  [2/3] Compilation COBOL (cobc)..."
    cobc $cobc_mode $cobc_extra -I $COPYBOOK_DIR \
         -I $PRECOMPILE_DIR \
         -o $OUTPUT_DIR/$output_name \
         $PRECOMPILE_DIR/${prog_name}.cob \
         -L/usr/local/lib \
         -locesql \
         -L/usr/lib/postgresql \
         -lpq \
         -std=default

    if [ $? -ne 0 ]; then
        echo -e "${RED}ERREUR lors de la compilation de $prog_name${NC}"
        return 1
    fi

    echo -e "${GREEN}  [3/3] $prog_name compile avec succes${NC}"
    return 0
}

# Compilation de EMPLOYEE-DAL-DB (couche DAL)
compile_program \
    "./dal/EMPLOYEE-DAL-DB.cbl" \
    "EMPLOYEE-DAL-DB" \
    "DAL" \
    "-m" \
    "EMPLOYEE-DAL-DB.so" \
    "-fstatic-call"

if [ $? -ne 0 ]; then
    echo -e "${RED}Echec de la compilation de EMPLOYEE-DAL-DB${NC}"
    exit 1
fi

# Compilation de EMPLOYEE-BUSINESS (couche BUSINESS)
compile_program \
    "./business/EMPLOYEE-BUSINESS.cbl" \
    "EMPLOYEE-BUSINESS" \
    "BUSINESS" \
    "-m" \
    "EMPLOYEE-BUSINESS.so" \
    ""

if [ $? -ne 0 ]; then
    echo -e "${RED}Echec de la compilation de EMPLOYEE-BUSINESS${NC}"
    exit 1
fi

# Compilation de EMPLOYEE-LOGIC (couche LOGIC)
compile_program \
    "./logic/EMPLOYEE-LOGIC.cbl" \
    "EMPLOYEE-LOGIC" \
    "LOGIC" \
    "-x" \
    "EMPLOYEE-LOGIC" \
    ""

if [ $? -ne 0 ]; then
    echo -e "${RED}Echec de la compilation de EMPLOYEE-LOGIC${NC}"
    exit 1
fi

echo ""
echo "=========================================="
echo -e "${GREEN}COMPILATION REUSSIE${NC}"
echo "=========================================="
echo "Programmes compiles:"
echo "  - EMPLOYEE-DAL-DB.so (DAL)"
echo "  - EMPLOYEE-BUSINESS.so (BUSINESS)"
echo "  - EMPLOYEE-LOGIC (LOGIC - programme principal)"
echo ""
echo "Executables dans: $OUTPUT_DIR/"
echo ""
echo "Pour executer le batch:"
echo "  cd $OUTPUT_DIR"
echo "  export COB_LIBRARY_PATH=."
echo "  ./EMPLOYEE-LOGIC"
echo "=========================================="

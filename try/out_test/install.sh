#!/bin/bash
#########################################################################
# install.sh - Installation automatique complete du projet             #
#                                                                       #
# Ce script installe tous les prerequis et configure le projet         #
#########################################################################

# Couleurs
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

INSTALL_LOG="/tmp/cobol_install.log"

echo "=========================================="
echo "INSTALLATION AUTOMATIQUE DU PROJET COBOL"
echo "Calcul de Salaire Net"
echo "=========================================="
echo ""
echo "Ce script va installer:"
echo "  - PostgreSQL (si necessaire)"
echo "  - GnuCOBOL (si necessaire)"
echo "  - ocesql (si necessaire)"
echo "  - Bibliotheques PostgreSQL"
echo "  - Configuration de la base de donnees"
echo ""
echo -e "${YELLOW}Attention: Ce script peut demander votre mot de passe sudo${NC}"
echo ""
read -p "Continuer? (y/N) " -n 1 -r
echo
if [[ ! $REPLY =~ ^[Yy]$ ]]; then
    echo "Installation annulee"
    exit 1
fi

echo "" > $INSTALL_LOG

#########################################################################
# FONCTION: Installation d'un package
#########################################################################
install_package() {
    local package=$1
    local name=$2

    echo -e "${BLUE}Installation de $name...${NC}"
    echo "Installation de $package" >> $INSTALL_LOG

    sudo apt-get install -y $package >> $INSTALL_LOG 2>&1

    if [ $? -eq 0 ]; then
        echo -e "${GREEN}✓${NC} $name installe"
    else
        echo -e "${RED}✗${NC} Erreur lors de l'installation de $name"
        echo "Voir les details: $INSTALL_LOG"
        return 1
    fi
}

#########################################################################
# ETAPE 1: MISE A JOUR DES PAQUETS
#########################################################################
echo ""
echo -e "${BLUE}[ETAPE 1/7] Mise a jour des paquets${NC}"
sudo apt-get update >> $INSTALL_LOG 2>&1
echo -e "${GREEN}✓${NC} Mise a jour terminee"

#########################################################################
# ETAPE 2: INSTALLATION DE POSTGRESQL
#########################################################################
echo ""
echo -e "${BLUE}[ETAPE 2/7] Verification/Installation de PostgreSQL${NC}"

if command -v psql &> /dev/null; then
    echo -e "${GREEN}✓${NC} PostgreSQL deja installe"
    psql --version | head -n1
else
    install_package "postgresql postgresql-contrib" "PostgreSQL"
fi

# Demarrer PostgreSQL
if ! sudo systemctl is-active --quiet postgresql 2>/dev/null; then
    echo "  Demarrage de PostgreSQL..."
    sudo systemctl start postgresql
    sudo systemctl enable postgresql
    echo -e "${GREEN}✓${NC} PostgreSQL demarre et active au demarrage"
fi

#########################################################################
# ETAPE 3: INSTALLATION DES BIBLIOTHEQUES POSTGRESQL
#########################################################################
echo ""
echo -e "${BLUE}[ETAPE 3/7] Installation des bibliotheques PostgreSQL${NC}"

if dpkg -l | grep -q libpq-dev; then
    echo -e "${GREEN}✓${NC} libpq-dev deja installe"
else
    install_package "libpq-dev" "libpq-dev (PostgreSQL client library)"
fi

#########################################################################
# ETAPE 4: INSTALLATION DE GNUCOBOL
#########################################################################
echo ""
echo -e "${BLUE}[ETAPE 4/7] Verification/Installation de GnuCOBOL${NC}"

if command -v cobc &> /dev/null; then
    echo -e "${GREEN}✓${NC} GnuCOBOL deja installe"
    cobc --version | head -n1
else
    install_package "gnucobol" "GnuCOBOL"
fi

#########################################################################
# ETAPE 5: INSTALLATION DE OCESQL
#########################################################################
echo ""
echo -e "${BLUE}[ETAPE 5/7] Verification/Installation de ocesql${NC}"

if command -v ocesql &> /dev/null; then
    echo -e "${GREEN}✓${NC} ocesql deja installe"
    ocesql --version 2>&1 | head -n1
else
    echo -e "${YELLOW}! ocesql n'est pas installe${NC}"
    echo "  ocesql doit etre installe depuis les sources"
    echo ""
    echo "  Tentative d'installation automatique..."

    # Installer les dependances pour ocesql
    echo "  Installation des dependances..."
    sudo apt-get install -y build-essential autoconf automake libtool flex bison >> $INSTALL_LOG 2>&1

    # Telecharger et compiler ocesql
    OCESQL_DIR="/tmp/ocesql_install"
    if [ -d "$OCESQL_DIR" ]; then
        rm -rf "$OCESQL_DIR"
    fi

    echo "  Telechargement de ocesql..."
    git clone https://github.com/opensourcecobol/Open-COBOL-ESQL.git "$OCESQL_DIR" >> $INSTALL_LOG 2>&1

    if [ $? -eq 0 ]; then
        cd "$OCESQL_DIR"
        echo "  Configuration..."
        ./configure >> $INSTALL_LOG 2>&1

        echo "  Compilation..."
        make >> $INSTALL_LOG 2>&1

        echo "  Installation..."
        sudo make install >> $INSTALL_LOG 2>&1

        # Mise a jour du cache des bibliotheques
        sudo ldconfig

        cd - > /dev/null

        if command -v ocesql &> /dev/null; then
            echo -e "${GREEN}✓${NC} ocesql installe avec succes"
        else
            echo -e "${RED}✗${NC} Erreur lors de l'installation de ocesql"
            echo "Installez ocesql manuellement depuis:"
            echo "  https://github.com/opensourcecobol/Open-COBOL-ESQL"
        fi
    else
        echo -e "${RED}✗${NC} Erreur lors du telechargement de ocesql"
        echo "Installez ocesql manuellement depuis:"
        echo "  https://github.com/opensourcecobol/Open-COBOL-ESQL"
    fi
fi

#########################################################################
# ETAPE 6: CONFIGURATION DE LA BASE DE DONNEES
#########################################################################
echo ""
echo -e "${BLUE}[ETAPE 6/7] Configuration de la base de donnees${NC}"

cd sql
./setup_db.sh

if [ $? -eq 0 ]; then
    echo -e "${GREEN}✓${NC} Base de donnees configuree"
else
    echo -e "${RED}✗${NC} Erreur lors de la configuration de la base"
    exit 1
fi

cd ..

#########################################################################
# ETAPE 7: COMPILATION DES PROGRAMMES COBOL
#########################################################################
echo ""
echo -e "${BLUE}[ETAPE 7/7] Compilation des programmes COBOL${NC}"

./compile_all.sh

if [ $? -eq 0 ]; then
    echo -e "${GREEN}✓${NC} Programmes compiles avec succes"
else
    echo -e "${RED}✗${NC} Erreur lors de la compilation"
    echo "Verifiez que tous les outils sont installes correctement"
    exit 1
fi

#########################################################################
# VALIDATION FINALE
#########################################################################
echo ""
echo "=========================================="
echo -e "${GREEN}INSTALLATION TERMINEE AVEC SUCCES!${NC}"
echo "=========================================="
echo ""
echo "Resume:"
echo "  ✓ PostgreSQL installe et configure"
echo "  ✓ GnuCOBOL installe"
echo "  ✓ ocesql installe"
echo "  ✓ Base de donnees empdb creee"
echo "  ✓ Programmes COBOL compiles"
echo ""
echo "Pour executer le batch:"
echo "  cd bin"
echo "  export COB_LIBRARY_PATH=."
echo "  ./EMPLOYEE-LOGIC"
echo ""
echo "Pour executer les tests:"
echo "  ./test_all.sh"
echo ""
echo "Log d'installation: $INSTALL_LOG"
echo "=========================================="

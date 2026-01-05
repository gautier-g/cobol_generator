# Makefile pour le projet COBOL Calcul Salaire Net
# Usage: make [target]

.PHONY: all install setup compile run test verify clean help

# Cible par defaut
all: setup compile

# Aide
help:
	@echo "=========================================="
	@echo "Makefile - Projet COBOL Calcul Salaire"
	@echo "=========================================="
	@echo ""
	@echo "Targets disponibles:"
	@echo "  make install    - Installation complete (prerequisites + setup + compile)"
	@echo "  make setup      - Initialiser la base de donnees"
	@echo "  make compile    - Compiler les programmes COBOL"
	@echo "  make run        - Executer le batch"
	@echo "  make test       - Executer tous les tests de validation"
	@echo "  make verify     - Verifier les resultats en base"
	@echo "  make all        - setup + compile (sans installation)"
	@echo "  make clean      - Nettoyer les fichiers generes"
	@echo "  make help       - Afficher cette aide"
	@echo ""
	@echo "Workflow recommande:"
	@echo "  1. make install  # Premiere fois (installe tout)"
	@echo "  2. make test     # Valider l'installation"
	@echo "  3. make run      # Executer le batch"
	@echo ""
	@echo "OU si les outils sont deja installes:"
	@echo "  1. make all      # setup + compile"
	@echo "  2. make run      # Executer le batch"
	@echo "  3. make verify   # Verifier les resultats"
	@echo "=========================================="

# Installation complete (prerequisites + setup + compile)
install:
	@echo "Lancement de l'installation complete..."
	./install.sh

# Initialisation de la base de donnees
setup:
	@echo "Initialisation de la base de donnees..."
	cd sql && ./setup_db.sh

# Compilation des programmes COBOL
compile:
	@echo "Compilation des programmes COBOL..."
	./compile_all.sh

# Execution du batch
run:
	@echo "Execution du batch EMPLOYEE-LOGIC..."
	@if [ ! -f bin/EMPLOYEE-LOGIC ]; then \
		echo "ERREUR: Programmes non compiles. Lancez 'make compile' d'abord."; \
		exit 1; \
	fi
	cd bin && COB_LIBRARY_PATH=. ./EMPLOYEE-LOGIC

# Tests complets de validation
test:
	@echo "Execution des tests de validation..."
	./test_all.sh

# Verification des resultats
verify:
	@echo "Verification des resultats..."
	./verify_results.sh

# Nettoyage
clean:
	@echo "Nettoyage des fichiers generes..."
	rm -rf bin/
	rm -rf precompiled/
	rm -f /tmp/compile_output.log
	rm -f /tmp/batch_output.log
	rm -f /tmp/cobol_install.log
	@echo "Nettoyage termine."

# Nettoyage complet (incluant la base de donnees)
clean-all: clean
	@echo "Suppression de la base de donnees..."
	@echo "ATTENTION: Ceci va supprimer la base empdb et toutes ses donnees!"
	@read -p "Continuer? (y/N) " -n 1 -r; \
	echo; \
	if [[ $$REPLY =~ ^[Yy]$$ ]]; then \
		sudo -u postgres psql -c "DROP DATABASE IF EXISTS empdb;" 2>/dev/null; \
		sudo -u postgres psql -c "DROP USER IF EXISTS empuser;" 2>/dev/null; \
		echo "Base de donnees supprimee."; \
	else \
		echo "Annule."; \
	fi

# Reinstallation complete
reinstall: clean all
	@echo "Reinstallation complete terminee."

# Test rapide (compile + run + verify)
quick-test: compile run verify

# Afficher les logs
show-logs:
	@echo "=== Logs de compilation ==="
	@if [ -f /tmp/compile_output.log ]; then cat /tmp/compile_output.log; else echo "Aucun log"; fi
	@echo ""
	@echo "=== Logs d'execution ==="
	@if [ -f /tmp/batch_output.log ]; then cat /tmp/batch_output.log; else echo "Aucun log"; fi

# Informations sur le projet
info:
	@echo "=========================================="
	@echo "Informations sur le projet"
	@echo "=========================================="
	@echo "Nom: Calcul de Salaire Net"
	@echo "Spec: salaire_net.yaml"
	@echo ""
	@echo "Programmes COBOL:"
	@if [ -f dal/EMPLOYEE-DAL-DB.cbl ]; then echo "  ✓ EMPLOYEE-DAL-DB.cbl ($(shell wc -l < dal/EMPLOYEE-DAL-DB.cbl) lignes)"; fi
	@if [ -f logic/EMPLOYEE-LOGIC.cbl ]; then echo "  ✓ EMPLOYEE-LOGIC.cbl ($(shell wc -l < logic/EMPLOYEE-LOGIC.cbl) lignes)"; fi
	@if [ -f business/EMPLOYEE-BUSINESS.cbl ]; then echo "  ✓ EMPLOYEE-BUSINESS.cbl ($(shell wc -l < business/EMPLOYEE-BUSINESS.cbl) lignes)"; fi
	@echo ""
	@echo "Executables:"
	@if [ -f bin/EMPLOYEE-DAL-DB.so ]; then echo "  ✓ bin/EMPLOYEE-DAL-DB.so"; fi
	@if [ -f bin/EMPLOYEE-LOGIC ]; then echo "  ✓ bin/EMPLOYEE-LOGIC"; fi
	@if [ -f bin/EMPLOYEE-BUSINESS.so ]; then echo "  ✓ bin/EMPLOYEE-BUSINESS.so"; fi
	@echo ""
	@echo "Base de donnees:"
	@if PGPASSWORD=SECRETPWD psql -h localhost -U empuser -d empdb -c "SELECT COUNT(*) FROM employee;" 2>/dev/null | grep -q "[0-9]"; then \
		echo "  ✓ empdb connectee"; \
		echo "  Employes: $$(PGPASSWORD=SECRETPWD psql -h localhost -U empuser -d empdb -tAc 'SELECT COUNT(*) FROM employee;' 2>/dev/null)"; \
	else \
		echo "  ✗ empdb non accessible"; \
	fi
	@echo "=========================================="

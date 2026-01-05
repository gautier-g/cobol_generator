#!/bin/bash
# Script de setup simplifie sans sudo

DB_NAME="empdb"
DB_USER="empuser"
DB_PASSWORD="SECRETPWD"

echo "Creation de l'utilisateur et de la base..."
echo "Executez ces commandes manuellement en tant qu'utilisateur postgres:"
echo ""
echo "sudo -u postgres psql << SQL"
echo "CREATE USER $DB_USER WITH PASSWORD '$DB_PASSWORD';"
echo "CREATE DATABASE $DB_NAME OWNER $DB_USER;"
echo "GRANT ALL PRIVILEGES ON DATABASE $DB_NAME TO $DB_USER;"
echo "SQL"
echo ""
echo "Ou executez directement:"
echo ""
echo "sudo -u postgres createuser -P $DB_USER"
echo "sudo -u postgres createdb -O $DB_USER $DB_NAME"

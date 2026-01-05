#!/bin/bash
#########################################################################
# setup_db_quick.sh - Quick PostgreSQL setup (requires sudo)
#########################################################################

set -e

cd "$(dirname "$0")"

echo "=========================================="
echo "Configuration PostgreSQL"
echo "=========================================="

# Create user and database
sudo -u postgres psql << 'SQL'
DROP DATABASE IF EXISTS empdb;
DROP USER IF EXISTS empuser;
CREATE USER empuser WITH LOGIN PASSWORD 'SECRETPWD';
CREATE DATABASE empdb OWNER empuser;
GRANT ALL PRIVILEGES ON DATABASE empdb TO empuser;
SQL

echo "✓ User and database created"

# Create tables
PGPASSWORD=SECRETPWD psql -h localhost -U empuser -d empdb -f sql/create_tables.sql

echo "✓ Tables created"

# Insert test data
PGPASSWORD=SECRETPWD psql -h localhost -U empuser -d empdb -f sql/insert_data.sql

echo ""
echo "=========================================="
echo "✓ Database setup complete!"
echo "=========================================="
echo ""
echo "Next steps:"
echo "  1. cd bin"
echo "  2. export COB_LIBRARY_PATH=."
echo "  3. ./EMPLOYEE-LOGIC"
echo "=========================================="

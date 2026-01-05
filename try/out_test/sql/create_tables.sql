-- create_tables.sql
-- Creation de la base de donnees et de la table EMPLOYEE
-- Pour PostgreSQL

-- Creation de la table EMPLOYEE
DROP TABLE IF EXISTS EMPLOYEE CASCADE;

CREATE TABLE EMPLOYEE (
    EMP_ID INT PRIMARY KEY,
    EMP_NAME VARCHAR(30) NOT NULL,
    SALARY_BRUT DECIMAL(8,2) NOT NULL,
    SALARY_NET DECIMAL(8,2)
);

-- Creation de l'index sur EMP_NAME
CREATE INDEX EMPLOYEE_EMP_NAME_IDX ON EMPLOYEE(EMP_NAME);

-- Affichage de confirmation
SELECT 'Table EMPLOYEE creee avec succes' AS status;

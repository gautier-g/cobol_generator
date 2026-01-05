-- insert_data.sql
-- Insertion des donnees initiales dans la table EMPLOYEE

-- Suppression des donnees existantes
DELETE FROM EMPLOYEE;

-- Insertion des donnees de test
INSERT INTO EMPLOYEE (EMP_ID, EMP_NAME, SALARY_BRUT, SALARY_NET)
VALUES
    (1, 'Dupont', 3000.00, 0.00),
    (2, 'Durand', 1500.00, 0.00);

-- Affichage des donnees inserees
SELECT EMP_ID, EMP_NAME, SALARY_BRUT, SALARY_NET
FROM EMPLOYEE
ORDER BY EMP_ID;

-- Affichage de confirmation
SELECT 'Donnees inserees avec succes' AS status;
SELECT COUNT(*) AS nb_employes FROM EMPLOYEE;

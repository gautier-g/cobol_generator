# file: cobolsmartgen/generate/procedure_templates.py
"""
Procedure Templates - Templates COBOL standards pour génération déterministe.
Élimine les appels LLM pour les procédures standards.
"""

PROCEDURE_TEMPLATES = {
    # ===== DAL (Data Access Layer) =====

    "OPEN-DATABASE": """
       OPEN-DATABASE.
           EXEC SQL
               CONNECT TO DATABASE
           END-EXEC
           MOVE SQLCODE TO WS-RETURN-CODE.
    """,

    "CLOSE-DATABASE": """
       CLOSE-DATABASE.
           EXEC SQL
               DISCONNECT ALL
           END-EXEC
           MOVE SQLCODE TO WS-RETURN-CODE.
    """,

    "SELECT-EMPLOYEE": """
       SELECT-EMPLOYEE.
           EXEC SQL
               SELECT EMP_NAME, SALARY_BRUT
               INTO :EMP-NAME, :SALARY-BRUT
               FROM EMPLOYEE
               WHERE EMP_ID = :EMP-ID
           END-EXEC
           IF SQLCODE = 0
               COMPUTE SALARY-NET ROUNDED = SALARY-BRUT * 0.7
           END-IF.
    """,

    "INSERT-EMPLOYEE": """
       INSERT-EMPLOYEE.
           EXEC SQL
               INSERT INTO EMPLOYEE (EMP_ID, EMP_NAME, SALARY_BRUT)
               VALUES (:EMP-ID, :EMP-NAME, :SALARY-BRUT)
           END-EXEC
           MOVE SQLCODE TO WS-RETURN-CODE.
    """,

    "UPDATE-EMPLOYEE": """
       UPDATE-EMPLOYEE.
           EXEC SQL
               UPDATE EMPLOYEE
               SET EMP_NAME = :EMP-NAME,
                   SALARY_BRUT = :SALARY-BRUT
               WHERE EMP_ID = :EMP-ID
           END-EXEC
           MOVE SQLCODE TO WS-RETURN-CODE.
    """,

    "DELETE-EMPLOYEE": """
       DELETE-EMPLOYEE.
           EXEC SQL
               DELETE FROM EMPLOYEE
               WHERE EMP_ID = :EMP-ID
           END-EXEC
           MOVE SQLCODE TO WS-RETURN-CODE.
    """,

    "OPEN-EMPLOYEE-CURSOR": """
       OPEN-EMPLOYEE-CURSOR.
           EXEC SQL
               OPEN EMPLOYEE_CURSOR
           END-EXEC
           MOVE SQLCODE TO WS-RETURN-CODE.
    """,

    "FETCH-EMPLOYEE-CURSOR": """
       FETCH-EMPLOYEE-CURSOR.
           EXEC SQL
               FETCH EMPLOYEE_CURSOR
               INTO :EMP-NAME, :SALARY-BRUT
           END-EXEC
           EVALUATE SQLCODE
               WHEN 0
                   COMPUTE SALARY-NET ROUNDED = SALARY-BRUT * 0.7
                   MOVE 'N' TO WS-EOF-FLAG
               WHEN 100
                   MOVE 'Y' TO WS-EOF-FLAG
               WHEN OTHER
                   MOVE 'Y' TO WS-EOF-FLAG
           END-EVALUATE.
    """,

    "CLOSE-EMPLOYEE-CURSOR": """
       CLOSE-EMPLOYEE-CURSOR.
           EXEC SQL
               CLOSE EMPLOYEE_CURSOR
           END-EXEC
           MOVE SQLCODE TO WS-RETURN-CODE.
    """,

    # ===== LOGIC (Application Layer) =====

    "PROCESS-EMPLOYEE": """
       PROCESS-EMPLOYEE.
           COMPUTE SALARY-NET ROUNDED = SALARY-BRUT * 0.7
           ON SIZE ERROR
               CONTINUE
           END-COMPUTE.
    """,

    "CALCULATE-EMPLOYEE": """
       CALCULATE-EMPLOYEE.
           COMPUTE SALARY-NET ROUNDED = SALARY-BRUT * 0.7
           ON SIZE ERROR
               MOVE 0 TO SALARY-NET
           END-COMPUTE.
    """,

    "VALIDATE-EMPLOYEE": """
       VALIDATE-EMPLOYEE.
           IF EMP-ID = ZERO OR EMP-NAME = SPACES
               MOVE 'INVALID' TO WS-RETURN-CODE
           ELSE
               MOVE 'VALID' TO WS-RETURN-CODE
           END-IF.
    """,

    "READ-EMPLOYEE": """
       READ-EMPLOYEE.
           EXEC SQL
               SELECT EMP_NAME, SALARY_BRUT
               INTO :EMP-NAME, :SALARY-BRUT
               FROM EMPLOYEE
               WHERE EMP_ID = :EMP-ID
           END-EXEC
           IF SQLCODE NOT = 0
               MOVE SPACES TO EMP-NAME
               MOVE ZEROS TO SALARY-BRUT
           ELSE
               COMPUTE SALARY-NET ROUNDED = SALARY-BRUT * 0.7
           END-IF.
    """,

    "WRITE-EMPLOYEE": """
       WRITE-EMPLOYEE.
           COMPUTE SALARY-NET ROUNDED = SALARY-BRUT * 0.7
           MOVE 0 TO SQLCODE.
    """,

    "TRANSFORM-EMPLOYEE": """
       TRANSFORM-EMPLOYEE.
           COMPUTE SALARY-NET ROUNDED = SALARY-BRUT * 0.7
           ON SIZE ERROR
               MOVE 0 TO SALARY-NET
           END-COMPUTE.
    """,

    # ===== BUSINESS (Presentation Layer) =====

    "MAIN-PROCEDURE": """
       MAIN-PROCEDURE.
           DISPLAY "Program starting"
           PERFORM PROCESS-EMPLOYEES
           GOBACK.
    """,

    "PROCESS-EMPLOYEES": """
       PROCESS-EMPLOYEES.
           PERFORM UNTIL WS-EOF-FLAG = 'Y'
               PERFORM READ-EMPLOYEE
               IF WS-EOF-FLAG = 'N'
                   PERFORM DISPLAY-EMPLOYEE
               END-IF
           END-PERFORM.
    """,

    "DISPLAY-EMPLOYEE": """
       DISPLAY-EMPLOYEE.
           DISPLAY "Employee: " EMP-NAME
           DISPLAY "ID: " EMP-ID
           DISPLAY "Gross Salary: " SALARY-BRUT
           DISPLAY "Net Salary: " SALARY-NET.
    """,

    "ADD-EMPLOYEE": """
       ADD-EMPLOYEE.
           EXEC SQL
               INSERT INTO EMPLOYEE (EMP_ID, EMP_NAME, SALARY_BRUT)
               VALUES (:EMP-ID, :EMP-NAME, :SALARY-BRUT)
           END-EXEC.
    """,

    "VIEW-EMPLOYEE": """
       VIEW-EMPLOYEE.
           COMPUTE SALARY-NET ROUNDED = SALARY-BRUT * 0.7
           DISPLAY "Employee: " EMP-NAME
           DISPLAY "Gross Salary: " SALARY-BRUT
           DISPLAY "Net Salary: " SALARY-NET.
    """,

    "PRESENT-EMPLOYEE-MENU": """
       PRESENT-EMPLOYEE-MENU.
           DISPLAY "1. Add Employee"
           DISPLAY "2. View Employee"
           DISPLAY "3. Update Employee"
           DISPLAY "4. Delete Employee"
           DISPLAY "5. Exit"
           ACCEPT WS-USER-OPTION.
    """,

    "PROCESS-EMPLOYEE-OPTION": """
       PROCESS-EMPLOYEE-OPTION.
           EVALUATE WS-USER-OPTION
               WHEN '1' PERFORM ADD-EMPLOYEE
               WHEN '2' PERFORM VIEW-EMPLOYEE
               WHEN '3' PERFORM UPDATE-EMPLOYEE
               WHEN '4' PERFORM DELETE-EMPLOYEE
               WHEN OTHER DISPLAY 'Invalid option'
           END-EVALUATE.
    """,
}


def get_procedure_template(proc_name: str) -> str:
    """
    Retourne le template pour une procédure donnée.

    Args:
        proc_name: Nom de la procédure (ex: "SELECT-EMPLOYEE")

    Returns:
        Le code COBOL de la procédure, ou un template minimal si non trouvé
    """
    # Normaliser le nom
    normalized_name = proc_name.upper().strip()

    # Chercher le template
    if normalized_name in PROCEDURE_TEMPLATES:
        return PROCEDURE_TEMPLATES[normalized_name]

    # Template minimal par défaut
    return f"""
       {normalized_name}.
           CONTINUE.
    """


def has_template(proc_name: str) -> bool:
    """Vérifie si un template existe pour une procédure."""
    return proc_name.upper().strip() in PROCEDURE_TEMPLATES


def list_available_templates() -> list:
    """Retourne la liste des templates disponibles."""
    return sorted(list(PROCEDURE_TEMPLATES.keys()))

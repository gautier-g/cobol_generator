IDENTIFICATION DIVISION.
       PROGRAM-ID. EMPLOYEE-DAL-DB.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  EMP-RECORD.
           05  EMP-ID          PIC 9(4).
           05  EMP-NAME        PIC A(30).
           05  SALARY-BRUT     PIC 9(6)V99.
           05  SALARY-NET      PIC 9(6)V99.
       01  END-OF-FILE         PIC X VALUE 'N'.
           88 EOF              VALUE 'Y'.

       PROCEDURE DIVISION.
       DAL-READ.
           EXEC SQL
               DECLARE EMP_CURSOR CURSOR FOR
               SELECT EMP_ID, EMP_NAME, SALARY_BRUT, SALARY_NET
               FROM EMPLOYEE
               WHERE EMP_ID = :EMP-ID
           END-EXEC.

           EXEC SQL
               OPEN EMP_CURSOR
           END-EXEC.

           EXEC SQL
               FETCH EMP_CURSOR INTO
                   :EMP-ID,
                   :EMP-NAME,
                   :SALARY-BRUT,
                   :SALARY-NET
           END-EXEC.

           IF SQLCODE NOT = 0
               MOVE 'Y' TO END-OF-FILE
           END-IF.

           EXEC SQL
               CLOSE EMP_CURSOR
           END-EXEC.

       DAL-SAVE.
           EXEC SQL
               UPDATE EMPLOYEE
               SET EMP_NAME = :EMP-NAME,
                   SALARY_BRUT = :SALARY-BRUT,
                   SALARY_NET = :SALARY-NET
               WHERE EMP_ID = :EMP-ID
           END-EXEC.

       DAL-END.
           EXIT.
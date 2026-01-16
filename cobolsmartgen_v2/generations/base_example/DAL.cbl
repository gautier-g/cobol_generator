       IDENTIFICATION DIVISION.
       PROGRAM-ID. EMPLOYEEDAL.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-CONNECTED           PIC X VALUE 'N'.
       01 WS-CURSOR-OPEN         PIC X VALUE 'N'.
       01 WS-SQLCODE             PIC S9(9) COMP-5.

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01 DBNAME                 PIC X(30) VALUE 'postgres'.
       01 USERNAME               PIC X(30) VALUE 'postgres'.
       01 PASSWD                 PIC X(30) VALUE 'postgres'.
       01 WS-EMPID               PIC 9(4).
       01 WS-EMPNAME             PIC X(30).
       01 WS-SALARYBRUT          PIC 9(6)V99.
       01 WS-SALARYNET           PIC 9(6)V99.
       EXEC SQL END DECLARE SECTION END-EXEC.

       EXEC SQL INCLUDE SQLCA END-EXEC.

       LINKAGE SECTION.
       01 LK-OPERATION           PIC X(4).
       01 LK-END-OF-FILE         PIC X.
       01 LK-EMPLOYEE.
           05 LK-EMPID           PIC 9(4).
           05 LK-EMPNAME         PIC X(30).
           05 LK-SALARYBRUT      PIC 9(6)V99.
           05 LK-SALARYNET       PIC 9(6)V99.

       PROCEDURE DIVISION USING LK-OPERATION LK-END-OF-FILE
           LK-EMPLOYEE.
       MAINLOGIC.
           EVALUATE LK-OPERATION
               WHEN 'READ'
                   PERFORM READPROC
               WHEN 'SAVE'
                   PERFORM SAVEPROC
               WHEN 'END '
                   PERFORM ENDPROC
           END-EVALUATE.
           EXIT PROGRAM.

       READPROC.
           IF WS-CONNECTED = 'N'
               EXEC SQL
                   CONNECT :USERNAME IDENTIFIED BY :PASSWD USING :DBNAME
               END-EXEC
               EXEC SQL
                   SET client_encoding TO 'LATIN1'
               END-EXEC
               IF SQLCODE NOT = 0
                   MOVE 'Y' TO LK-END-OF-FILE
                   EXIT PARAGRAPH
               END-IF
               MOVE 'Y' TO WS-CONNECTED
           END-IF.

           IF WS-CURSOR-OPEN = 'N'
               EXEC SQL
                   DECLARE CEMP CURSOR FOR
                   SELECT EMP_ID, EMP_NAME, SALARY_BRUT, SALARY_NET
                   FROM EMPLOYEE
               END-EXEC
               EXEC SQL
                   OPEN CEMP
               END-EXEC
               IF SQLCODE NOT = 0
                   MOVE 'Y' TO LK-END-OF-FILE
                   EXIT PARAGRAPH
               END-IF
               MOVE 'Y' TO WS-CURSOR-OPEN
           END-IF.

           EXEC SQL
               FETCH CEMP INTO
                   :WS-EMPID,
                   :WS-EMPNAME,
                   :WS-SALARYBRUT,
                   :WS-SALARYNET
           END-EXEC.

           IF SQLCODE NOT = 0
               MOVE 'Y' TO LK-END-OF-FILE
           ELSE
               MOVE WS-EMPID        TO LK-EMPID
               MOVE WS-EMPNAME      TO LK-EMPNAME
               MOVE WS-SALARYBRUT   TO LK-SALARYBRUT
               MOVE WS-SALARYNET    TO LK-SALARYNET
           END-IF.

       SAVEPROC.
           MOVE LK-EMPID        TO WS-EMPID.
           MOVE LK-SALARYNET    TO WS-SALARYNET.

           EXEC SQL
               UPDATE EMPLOYEE
               SET SALARY_NET = :WS-SALARYNET
               WHERE EMP_ID = :WS-EMPID
           END-EXEC.

       ENDPROC.
           IF WS-CURSOR-OPEN = 'Y'
               EXEC SQL
                   CLOSE CEMP
               END-EXEC
               MOVE 'N' TO WS-CURSOR-OPEN
           END-IF.

           EXEC SQL
               COMMIT
           END-EXEC.

           IF WS-CONNECTED = 'Y'
               EXEC SQL
                   DISCONNECT ALL
               END-EXEC
               MOVE 'N' TO WS-CONNECTED
           END-IF.
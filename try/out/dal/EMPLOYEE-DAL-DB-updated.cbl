       IDENTIFICATION DIVISION.
       PROGRAM-ID. EMPLOYEE-DAL-DB.
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
       01 WS-EMP-ID              PIC 9(4).
       01 WS-EMP-NAME            PIC X(30).
       01 WS-SALARY-BRUT         PIC 9(6)V99.
       01 WS-SALARY-NET          PIC 9(6)V99.
       EXEC SQL END DECLARE SECTION END-EXEC.
       
       EXEC SQL INCLUDE SQLCA END-EXEC.
       
       LINKAGE SECTION.
       01 LK-OPERATION           PIC X(4).
       01 LK-END-OF-FILE         PIC X.
       01 LK-EMPLOYEE.
           05 LK-EMP-ID          PIC 9(4).
           05 LK-EMP-NAME        PIC A(30).
           05 LK-SALARY-BRUT     PIC 9(6)V99.
           05 LK-SALARY-NET      PIC 9(6)V99.
           
       PROCEDURE DIVISION USING LK-OPERATION LK-END-OF-FILE 
           LK-EMPLOYEE.
       MAIN-LOGIC.
           EVALUATE LK-OPERATION
               WHEN 'READ'
                   PERFORM DAL-READ
               WHEN 'SAVE'
                   PERFORM DAL-SAVE
               WHEN 'END '
                   PERFORM DAL-END
           END-EVALUATE.
           EXIT PROGRAM.
           
       DAL-READ.
           IF WS-CONNECTED = 'N'
               EXEC SQL
                   CONNECT :USERNAME IDENTIFIED BY :PASSWD USING :DBNAME
               END-EXEC
               IF SQLCODE NOT = 0
                   MOVE 'Y' TO LK-END-OF-FILE
                   EXIT PARAGRAPH
               END-IF
               MOVE 'Y' TO WS-CONNECTED
           END-IF.
           
           IF WS-CURSOR-OPEN = 'N'
               EXEC SQL
                   DECLARE C_EMP CURSOR FOR
                   SELECT EMP_ID, EMP_NAME, SALARY_BRUT, SALARY_NET
                   FROM EMPLOYEE
               END-EXEC
               EXEC SQL
                   OPEN C_EMP
               END-EXEC
               IF SQLCODE NOT = 0
                   DISPLAY "[DAL-READ] ERREUR ouverture curseur!"
                   MOVE 'Y' TO LK-END-OF-FILE
                   EXIT PARAGRAPH
               END-IF
               MOVE 'Y' TO WS-CURSOR-OPEN
           END-IF.
           
           EXEC SQL
               FETCH C_EMP INTO
                   :WS-EMP-ID,
                   :WS-EMP-NAME,
                   :WS-SALARY-BRUT,
                   :WS-SALARY-NET
           END-EXEC.
           
           IF SQLCODE NOT = 0
               MOVE 'Y' TO LK-END-OF-FILE
           ELSE
               MOVE WS-EMP-ID        TO LK-EMP-ID
               MOVE WS-EMP-NAME      TO LK-EMP-NAME
               MOVE WS-SALARY-BRUT   TO LK-SALARY-BRUT
               MOVE WS-SALARY-NET    TO LK-SALARY-NET
           END-IF.
           
       DAL-SAVE.
           MOVE LK-EMP-ID        TO WS-EMP-ID.
           MOVE LK-SALARY-NET    TO WS-SALARY-NET.
           
           EXEC SQL
               UPDATE EMPLOYEE
               SET SALARY_NET = :WS-SALARY-NET
               WHERE EMP_ID = :WS-EMP-ID
           END-EXEC.
           
       DAL-END.
           IF WS-CURSOR-OPEN = 'Y'
               EXEC SQL
                   CLOSE C_EMP
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
           
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DEPARTMENT-DAL-DB.
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
       01 WS-DEPT-ID             PIC 9(4).
       01 WS-DEPT-NAME           PIC X(40).
       EXEC SQL END DECLARE SECTION END-EXEC.

       EXEC SQL INCLUDE SQLCA END-EXEC.

       LINKAGE SECTION.
       01 LK-OPERATION           PIC X(4).
       01 LK-END-OF-FILE         PIC X.
       01 LK-DEPARTMENT.
           05 LK-DEPT-ID         PIC 9(4).
           05 LK-DEPT-NAME       PIC X(40).

       PROCEDURE DIVISION USING LK-OPERATION LK-END-OF-FILE
           LK-DEPARTMENT.
       MAIN-LOGIC.
           EVALUATE LK-OPERATION
               WHEN 'READ'
                   PERFORM DALREAD
               WHEN 'SAVE'
                   PERFORM DALSAVE
               WHEN 'END '
                   PERFORM DALEND
           END-EVALUATE.
           EXIT PROGRAM.

       DALREAD.
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
                   DECLARE CDEPT CURSOR FOR
                   SELECT DEPT_ID, DEPT_NAME
                   FROM DEPARTMENT
               END-EXEC
               EXEC SQL
                   OPEN CDEPT
               END-EXEC
               IF SQLCODE NOT = 0
                   MOVE 'Y' TO LK-END-OF-FILE
                   EXIT PARAGRAPH
               END-IF
               MOVE 'Y' TO WS-CURSOR-OPEN
           END-IF.

           EXEC SQL
               FETCH CDEPT INTO
                   :WS-DEPT-ID,
                   :WS-DEPT-NAME
           END-EXEC.

           IF SQLCODE NOT = 0
               MOVE 'Y' TO LK-END-OF-FILE
           ELSE
               MOVE WS-DEPT-ID      TO LK-DEPT-ID
               MOVE WS-DEPT-NAME    TO LK-DEPT-NAME
           END-IF.

       DALSAVE.
           MOVE LK-DEPT-ID         TO WS-DEPT-ID.
           MOVE LK-DEPT-NAME       TO WS-DEPT-NAME.

           EXEC SQL
               INSERT INTO DEPARTMENT (DEPT_ID, DEPT_NAME)
               VALUES (:WS-DEPT-ID, :WS-DEPT-NAME)
           END-EXEC.

           IF SQLCODE NOT = 0
               EXEC SQL
                   UPDATE DEPARTMENT
                   SET DEPT_NAME = :WS-DEPT-NAME
                   WHERE DEPT_ID = :WS-DEPT-ID
               END-EXEC
           END-IF.

       DALEND.
           IF WS-CURSOR-OPEN = 'Y'
               EXEC SQL
                   CLOSE CDEPT
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
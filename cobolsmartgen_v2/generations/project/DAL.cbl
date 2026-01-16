       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROJECT-DAL-DB.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-CONNECTED           PIC X VALUE 'N'.
       01 WS-CURSOROPEN          PIC X VALUE 'N'.
       01 WS-SQLCODE             PIC S9(9) COMP-5.

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01 DBNAME                 PIC X(30) VALUE 'postgres'.
       01 USERNAME               PIC X(30) VALUE 'postgres'.
       01 PASSWD                 PIC X(30) VALUE 'postgres'.
       01 WS-PROJID              PIC 9(9).
       01 WS-PROJNAME            PIC X(60).
       01 WS-BUDGET              PIC 9(10)V99.
       EXEC SQL END DECLARE SECTION END-EXEC.

       EXEC SQL INCLUDE SQLCA END-EXEC.

       LINKAGE SECTION.
       01 LK-OPERATION           PIC X(4).
       01 LK-ENDOFFILE           PIC X.
       01 LK-PROJECT.
           05 LK-PROJID          PIC 9(9).
           05 LK-PROJNAME        PIC X(60).
           05 LK-BUDGET          PIC 9(10)V99.

       PROCEDURE DIVISION USING LK-OPERATION LK-ENDOFFILE
           LK-PROJECT.
       MAINLOGIC.
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
                   MOVE 'Y' TO LK-ENDOFFILE
                   EXIT PARAGRAPH
               END-IF
               MOVE 'Y' TO WS-CONNECTED
           END-IF.

           IF WS-CURSOROPEN = 'N'
               EXEC SQL
                   DECLARE CPROJ CURSOR FOR
                   SELECT PROJ_ID, PROJ_NAME, BUDGET
                   FROM PROJECT
               END-EXEC
               EXEC SQL
                   OPEN CPROJ
               END-EXEC
               IF SQLCODE NOT = 0
                   MOVE 'Y' TO LK-ENDOFFILE
                   EXIT PARAGRAPH
               END-IF
               MOVE 'Y' TO WS-CURSOROPEN
           END-IF.

           EXEC SQL
               FETCH CPROJ INTO
                   :WS-PROJID,
                   :WS-PROJNAME,
                   :WS-BUDGET
           END-EXEC.

           IF SQLCODE NOT = 0
               MOVE 'Y' TO LK-ENDOFFILE
           ELSE
               MOVE WS-PROJID        TO LK-PROJID
               MOVE WS-PROJNAME      TO LK-PROJNAME
               MOVE WS-BUDGET        TO LK-BUDGET
           END-IF.

       DALSAVE.
           MOVE LK-PROJID        TO WS-PROJID.
           MOVE LK-PROJNAME      TO WS-PROJNAME.
           MOVE LK-BUDGET        TO WS-BUDGET.

           EXEC SQL
               UPDATE PROJECT
               SET PROJ_NAME = :WS-PROJNAME,
                   BUDGET = :WS-BUDGET
               WHERE PROJ_ID = :WS-PROJID
           END-EXEC.

           IF SQLCODE NOT = 0
               EXEC SQL
                   INSERT INTO PROJECT
                   (PROJ_ID, PROJ_NAME, BUDGET)
                   VALUES
                   (:WS-PROJID, :WS-PROJNAME, :WS-BUDGET)
               END-EXEC
           END-IF.

       DALEND.
           IF WS-CURSOROPEN = 'Y'
               EXEC SQL
                   CLOSE CPROJ
               END-EXEC
               MOVE 'N' TO WS-CURSOROPEN
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
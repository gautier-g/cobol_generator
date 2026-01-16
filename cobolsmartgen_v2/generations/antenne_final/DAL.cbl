       IDENTIFICATION DIVISION.
       PROGRAM-ID. ANTENNE-DAL-DB.
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
       01 WS-ANTENNE-ID          PIC 9(9).
       01 WS-ANTENNE-NOM         PIC X(50).
       01 WS-ANTENNE-REGION      PIC X(50).
       EXEC SQL END DECLARE SECTION END-EXEC.

       EXEC SQL INCLUDE SQLCA END-EXEC.

       LINKAGE SECTION.
       01 LK-OPERATION           PIC X(4).
       01 LK-END-OF-FILE         PIC X.
       01 LK-ANTENNE.
           05 LK-ANTENNE-ID      PIC 9(9).
           05 LK-ANTENNE-NOM     PIC X(50).
           05 LK-ANTENNE-REGION  PIC X(50).

       PROCEDURE DIVISION USING LK-OPERATION LK-END-OF-FILE
           LK-ANTENNE.
       MAIN-LOGIC.
           EVALUATE LK-OPERATION
               WHEN 'CREA'
                   PERFORM DALCREATE
               WHEN 'READ'
                   PERFORM DALREAD
               WHEN 'LIST'
                   PERFORM DALLIST
               WHEN 'UPDA'
                   PERFORM DALUPDATE
               WHEN 'DELE'
                   PERFORM DALDELETE
               WHEN 'END '
                   PERFORM DALEND
           END-EVALUATE.
           EXIT PROGRAM.

       DALCREATE.
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

           MOVE LK-ANTENNE-ID     TO WS-ANTENNE-ID.
           MOVE LK-ANTENNE-NOM    TO WS-ANTENNE-NOM.
           MOVE LK-ANTENNE-REGION TO WS-ANTENNE-REGION.

           EXEC SQL
               INSERT INTO ANTENNE
               (ANTENNE_ID, ANTENNE_NOM, ANTENNE_REGION)
               VALUES
               (:WS-ANTENNE-ID, :WS-ANTENNE-NOM, :WS-ANTENNE-REGION)
           END-EXEC.

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

           MOVE LK-ANTENNE-ID TO WS-ANTENNE-ID.

           EXEC SQL
               SELECT ANTENNE_NOM, ANTENNE_REGION
               INTO :WS-ANTENNE-NOM, :WS-ANTENNE-REGION
               FROM ANTENNE
               WHERE ANTENNE_ID = :WS-ANTENNE-ID
           END-EXEC.

           IF SQLCODE NOT = 0
               MOVE 'Y' TO LK-END-OF-FILE
           ELSE
               MOVE WS-ANTENNE-NOM    TO LK-ANTENNE-NOM
               MOVE WS-ANTENNE-REGION TO LK-ANTENNE-REGION
           END-IF.

       DALLIST.
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
                   DECLARE CANTENNE CURSOR FOR
                   SELECT ANTENNE_ID, ANTENNE_NOM, ANTENNE_REGION
                   FROM ANTENNE
               END-EXEC
               EXEC SQL
                   OPEN CANTENNE
               END-EXEC
               IF SQLCODE NOT = 0
                   MOVE 'Y' TO LK-END-OF-FILE
                   EXIT PARAGRAPH
               END-IF
               MOVE 'Y' TO WS-CURSOR-OPEN
           END-IF.

           EXEC SQL
               FETCH CANTENNE INTO
                   :WS-ANTENNE-ID,
                   :WS-ANTENNE-NOM,
                   :WS-ANTENNE-REGION
           END-EXEC.

           IF SQLCODE NOT = 0
               MOVE 'Y' TO LK-END-OF-FILE
           ELSE
               MOVE WS-ANTENNE-ID     TO LK-ANTENNE-ID
               MOVE WS-ANTENNE-NOM    TO LK-ANTENNE-NOM
               MOVE WS-ANTENNE-REGION TO LK-ANTENNE-REGION
           END-IF.

       DALUPDATE.
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

           MOVE LK-ANTENNE-ID     TO WS-ANTENNE-ID.
           MOVE LK-ANTENNE-NOM    TO WS-ANTENNE-NOM.
           MOVE LK-ANTENNE-REGION TO WS-ANTENNE-REGION.

           EXEC SQL
               UPDATE ANTENNE
               SET ANTENNE_NOM = :WS-ANTENNE-NOM,
                   ANTENNE_REGION = :WS-ANTENNE-REGION
               WHERE ANTENNE_ID = :WS-ANTENNE-ID
           END-EXEC.

       DALDELETE.
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

           MOVE LK-ANTENNE-ID TO WS-ANTENNE-ID.

           EXEC SQL
               DELETE FROM ANTENNE
               WHERE ANTENNE_ID = :WS-ANTENNE-ID
           END-EXEC.

       DALEND.
           IF WS-CURSOR-OPEN = 'Y'
               EXEC SQL
                   CLOSE CANTENNE
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
       IDENTIFICATION DIVISION.
       PROGRAM-ID. STOCK-DAL-DB.
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
       01 WS-STOCKID             PIC 9(9).
       01 WS-PRODID              PIC 9(9).
       01 WS-QUANTITY            PIC 9(9).
       EXEC SQL END DECLARE SECTION END-EXEC.

       EXEC SQL INCLUDE SQLCA END-EXEC.

       LINKAGE SECTION.
       01 LK-OPERATION           PIC X(4).
       01 LK-END-OF-FILE         PIC X.
       01 LK-STOCK.
           05 LK-STOCKID         PIC 9(9).
           05 LK-PRODID          PIC 9(9).
           05 LK-QUANTITY        PIC 9(9).

       PROCEDURE DIVISION USING LK-OPERATION LK-END-OF-FILE
           LK-STOCK.
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
                   DECLARE CSTOCK CURSOR FOR
                   SELECT STOCK_ID, PROD_ID, QUANTITY
                   FROM STOCK
               END-EXEC
               EXEC SQL
                   OPEN CSTOCK
               END-EXEC
               IF SQLCODE NOT = 0
                   MOVE 'Y' TO LK-END-OF-FILE
                   EXIT PARAGRAPH
               END-IF
               MOVE 'Y' TO WS-CURSOR-OPEN
           END-IF.

           EXEC SQL
               FETCH CSTOCK INTO
                   :WS-STOCKID,
                   :WS-PRODID,
                   :WS-QUANTITY
           END-EXEC.

           IF SQLCODE NOT = 0
               MOVE 'Y' TO LK-END-OF-FILE
           ELSE
               MOVE WS-STOCKID      TO LK-STOCKID
               MOVE WS-PRODID       TO LK-PRODID
               MOVE WS-QUANTITY     TO LK-QUANTITY
           END-IF.

       DALSAVE.
           MOVE LK-STOCKID      TO WS-STOCKID.
           MOVE LK-PRODID       TO WS-PRODID.
           MOVE LK-QUANTITY     TO WS-QUANTITY.

           IF WS-CONNECTED = 'N'
               EXEC SQL
                   CONNECT :USERNAME IDENTIFIED BY :PASSWD USING :DBNAME
               END-EXEC
               EXEC SQL
                   SET client_encoding TO 'LATIN1'
               END-EXEC
               IF SQLCODE NOT = 0
                   EXIT PARAGRAPH
               END-IF
               MOVE 'Y' TO WS-CONNECTED
           END-IF.

           EXEC SQL
               UPDATE STOCK
               SET PROD_ID = :WS-PRODID,
                   QUANTITY = :WS-QUANTITY
               WHERE STOCK_ID = :WS-STOCKID
           END-EXEC.

           IF SQLCODE NOT = 0
               EXEC SQL
                   INSERT INTO STOCK
                   (STOCK_ID, PROD_ID, QUANTITY)
                   VALUES
                   (:WS-STOCKID, :WS-PRODID, :WS-QUANTITY)
               END-EXEC
           END-IF.

       DALEND.
           IF WS-CURSOR-OPEN = 'Y'
               EXEC SQL
                   CLOSE CSTOCK
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
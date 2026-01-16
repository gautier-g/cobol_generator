       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRODUCTDAL.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-CONNECTED           PIC X VALUE 'N'.
       01 WS-SQLCODE             PIC S9(9) COMP-5.

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01 DBNAME                 PIC X(30) VALUE 'postgres'.
       01 USERNAME               PIC X(30) VALUE 'postgres'.
       01 PASSWD                 PIC X(30) VALUE 'postgres'.
       01 WS-PRODID              PIC 9(9).
       01 WS-PRODNAME            PIC X(50).
       01 WS-PRICEHT             PIC 9(6)V99.
       01 WS-PRICETTC            PIC 9(6)V99.
       EXEC SQL END DECLARE SECTION END-EXEC.

       EXEC SQL INCLUDE SQLCA END-EXEC.

       LINKAGE SECTION.
       01 LK-OPERATION           PIC X(4).
       01 LK-END-OF-FILE         PIC X.
       01 LK-PRODUCT.
           05 LK-PRODID          PIC 9(9).
           05 LK-PRODNAME        PIC X(50).
           05 LK-PRICEHT         PIC 9(6)V99.
           05 LK-PRICETTC        PIC 9(6)V99.

       PROCEDURE DIVISION USING LK-OPERATION LK-END-OF-FILE
           LK-PRODUCT.
       MAIN-LOGIC.
           EVALUATE LK-OPERATION
               WHEN 'FIND'
                   PERFORM FIND-PRODUCT
               WHEN 'SAVE'
                   PERFORM SAVE-PRODUCT
               WHEN 'END '
                   PERFORM END-PROGRAM
           END-EVALUATE.
           EXIT PROGRAM.

       CONNECT-DB.
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

       FIND-PRODUCT.
           PERFORM CONNECT-DB.
           IF LK-END-OF-FILE = 'Y'
               EXIT PARAGRAPH
           END-IF.

           MOVE LK-PRODID TO WS-PRODID.
           EXEC SQL
               SELECT PROD_NAME, PRICE_HT, PRICE_TTC
               INTO :WS-PRODNAME, :WS-PRICEHT, :WS-PRICETTC
               FROM PRODUCT
               WHERE PROD_ID = :WS-PRODID
           END-EXEC.

           IF SQLCODE NOT = 0
               MOVE 'Y' TO LK-END-OF-FILE
           ELSE
               MOVE WS-PRODNAME    TO LK-PRODNAME
               MOVE WS-PRICEHT     TO LK-PRICEHT
               MOVE WS-PRICETTC    TO LK-PRICETTC
           END-IF.

       SAVE-PRODUCT.
           PERFORM CONNECT-DB.
           IF LK-END-OF-FILE = 'Y'
               EXIT PARAGRAPH
           END-IF.

           MOVE LK-PRODID      TO WS-PRODID.
           MOVE LK-PRODNAME    TO WS-PRODNAME.
           MOVE LK-PRICEHT     TO WS-PRICEHT.
           MOVE LK-PRICETTC    TO WS-PRICETTC.

           EXEC SQL
               UPDATE PRODUCT
               SET PROD_NAME = :WS-PRODNAME,
                   PRICE_HT = :WS-PRICEHT,
                   PRICE_TTC = :WS-PRICETTC
               WHERE PROD_ID = :WS-PRODID
           END-EXEC.

           IF SQLCODE NOT = 0
               EXEC SQL
                   INSERT INTO PRODUCT
                   (PROD_ID, PROD_NAME, PRICE_HT, PRICE_TTC)
                   VALUES
                   (:WS-PRODID, :WS-PRODNAME, :WS-PRICEHT,
                    :WS-PRICETTC)
               END-EXEC
           END-IF.

       END-PROGRAM.
           EXEC SQL
               COMMIT
           END-EXEC.

           IF WS-CONNECTED = 'Y'
               EXEC SQL
                   DISCONNECT ALL
               END-EXEC
               MOVE 'N' TO WS-CONNECTED
           END-IF.
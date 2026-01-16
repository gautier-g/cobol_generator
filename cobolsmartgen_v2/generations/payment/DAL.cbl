       IDENTIFICATION DIVISION.
       PROGRAM-ID. PAYMENT-DAL-DB.
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
       01 WS-PAY-ID              PIC 9(9).
       01 WS-ORDER-ID            PIC 9(9).
       01 WS-PAY-AMOUNT          PIC 9(6)V99.
       EXEC SQL END DECLARE SECTION END-EXEC.

       EXEC SQL INCLUDE SQLCA END-EXEC.

       LINKAGE SECTION.
       01 LK-OPERATION           PIC X(4).
       01 LK-END-OF-FILE         PIC X.
       01 LK-PAYMENT.
           05 LK-PAY-ID          PIC 9(9).
           05 LK-ORDER-ID        PIC 9(9).
           05 LK-PAY-AMOUNT      PIC 9(6)V99.

       PROCEDURE DIVISION USING LK-OPERATION LK-END-OF-FILE
           LK-PAYMENT.
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
                   DECLARE CPAY CURSOR FOR
                   SELECT PAY_ID, ORDER_ID, PAY_AMOUNT
                   FROM PAYMENT
               END-EXEC
               EXEC SQL
                   OPEN CPAY
               END-EXEC
               IF SQLCODE NOT = 0
                   MOVE 'Y' TO LK-END-OF-FILE
                   EXIT PARAGRAPH
               END-IF
               MOVE 'Y' TO WS-CURSOR-OPEN
           END-IF.

           EXEC SQL
               FETCH CPAY INTO
                   :WS-PAY-ID,
                   :WS-ORDER-ID,
                   :WS-PAY-AMOUNT
           END-EXEC.

           IF SQLCODE NOT = 0
               MOVE 'Y' TO LK-END-OF-FILE
           ELSE
               MOVE WS-PAY-ID      TO LK-PAY-ID
               MOVE WS-ORDER-ID    TO LK-ORDER-ID
               MOVE WS-PAY-AMOUNT  TO LK-PAY-AMOUNT
           END-IF.

       DALSAVE.
           MOVE LK-PAY-ID      TO WS-PAY-ID.
           MOVE LK-ORDER-ID    TO WS-ORDER-ID.
           MOVE LK-PAY-AMOUNT  TO WS-PAY-AMOUNT.

           IF WS-CONNECTED = 'N'
               EXEC SQL
                   CONNECT :USERNAME IDENTIFIED BY :PASSWD USING :DBNAME
               END-EXEC
               IF SQLCODE NOT = 0
                   EXIT PARAGRAPH
               END-IF
               MOVE 'Y' TO WS-CONNECTED
           END-IF.

           EXEC SQL
               INSERT INTO PAYMENT (PAY_ID, ORDER_ID, PAY_AMOUNT)
               VALUES (:WS-PAY-ID, :WS-ORDER-ID, :WS-PAY-AMOUNT)
           END-EXEC.

       DALEND.
           IF WS-CURSOR-OPEN = 'Y'
               EXEC SQL
                   CLOSE CPAY
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
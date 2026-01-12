       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTOMER-DAL-DB.
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
       01 WS-CUST-ID             PIC 9(9).
       01 WS-CUST-NAME           PIC X(40).
       01 WS-CUST-CITY           PIC X(30).
       EXEC SQL END DECLARE SECTION END-EXEC.

       EXEC SQL INCLUDE SQLCA END-EXEC.

       LINKAGE SECTION.
       01 LK-OPERATION           PIC X(4).
       01 LK-END-OF-FILE         PIC X.
       01 LK-CUSTOMER.
           05 LK-CUST-ID         PIC 9(9).
           05 LK-CUST-NAME       PIC X(40).
           05 LK-CUST-CITY       PIC X(30).

       PROCEDURE DIVISION USING LK-OPERATION LK-END-OF-FILE
           LK-CUSTOMER.
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
                   DECLARE CCUST CURSOR FOR
                   SELECT CUST_ID, CUST_NAME, CUST_CITY
                   FROM CUSTOMER
               END-EXEC
               EXEC SQL
                   OPEN CCUST
               END-EXEC
               IF SQLCODE NOT = 0
                   MOVE 'Y' TO LK-END-OF-FILE
                   EXIT PARAGRAPH
               END-IF
               MOVE 'Y' TO WS-CURSOR-OPEN
           END-IF.

           EXEC SQL
               FETCH CCUST INTO
                   :WS-CUST-ID,
                   :WS-CUST-NAME,
                   :WS-CUST-CITY
           END-EXEC.

           IF SQLCODE NOT = 0
               MOVE 'Y' TO LK-END-OF-FILE
           ELSE
               MOVE WS-CUST-ID     TO LK-CUST-ID
               MOVE WS-CUST-NAME   TO LK-CUST-NAME
               MOVE WS-CUST-CITY   TO LK-CUST-CITY
           END-IF.

       DALSAVE.
           MOVE LK-CUST-ID        TO WS-CUST-ID.
           MOVE LK-CUST-NAME      TO WS-CUST-NAME.
           MOVE LK-CUST-CITY      TO WS-CUST-CITY.

           EXEC SQL
               INSERT INTO CUSTOMER
               (CUST_ID, CUST_NAME, CUST_CITY)
               VALUES
               (:WS-CUST-ID, :WS-CUST-NAME, :WS-CUST-CITY)
           END-EXEC.

           IF SQLCODE NOT = 0
               EXEC SQL
                   UPDATE CUSTOMER
                   SET CUST_NAME = :WS-CUST-NAME,
                       CUST_CITY = :WS-CUST-CITY
                   WHERE CUST_ID = :WS-CUST-ID
               END-EXEC
           END-IF.

       DALEND.
           IF WS-CURSOR-OPEN = 'Y'
               EXEC SQL
                   CLOSE CCUST
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
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTOMER-LIST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-OPERATION           PIC X(4) VALUE 'READ'.
       01 WS-EOF                 PIC X VALUE 'N'.
       01 WS-CUSTOMER.
           05 WS-CUST-ID         PIC 9(9).
           05 WS-CUST-NAME       PIC X(40).
           05 WS-CUST-CITY       PIC X(30).
OCESQL*
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           PERFORM UNTIL WS-EOF = 'Y'
               CALL 'CUSTOMER-DAL-DB' USING WS-OPERATION WS-EOF
                   WS-CUSTOMER
               IF WS-EOF = 'N'
                   CALL 'CUSTOMER-DISPLAY' USING WS-CUSTOMER
               END-IF
           END-PERFORM
           MOVE 'END ' TO WS-OPERATION
           CALL 'CUSTOMER-DAL-DB' USING WS-OPERATION WS-EOF
               WS-CUSTOMER
           STOP RUN.
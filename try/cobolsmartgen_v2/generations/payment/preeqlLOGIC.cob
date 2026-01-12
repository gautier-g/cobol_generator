       IDENTIFICATION DIVISION.
       PROGRAM-ID. PAYMENT-REPORT.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-OPERATION           PIC X(4) VALUE 'READ'.
       01 WS-END-OF-FILE         PIC X VALUE 'N'.
       01 WS-PAYMENT.
          05 WS-PAY-ID           PIC 9(9).
          05 WS-ORDER-ID         PIC 9(9).
          05 WS-PAY-AMOUNT       PIC 9(6)V99.
OCESQL*
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           PERFORM UNTIL WS-END-OF-FILE = 'Y'
               CALL 'PAYMENT-DAL-DB' USING WS-OPERATION
                   WS-END-OF-FILE WS-PAYMENT
               IF WS-END-OF-FILE NOT = 'Y'
                   CALL 'DISPLAY-PAYMENT' USING WS-PAYMENT
               END-IF
           END-PERFORM.
           STOP RUN.
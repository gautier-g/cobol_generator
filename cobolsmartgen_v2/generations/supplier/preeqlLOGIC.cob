       IDENTIFICATION DIVISION.
       PROGRAM-ID. LIST-SUPPLIERS.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-END-OF-FILE         PIC X VALUE 'N'.
       01 WS-OPERATION           PIC X(4) VALUE 'READ'.
       01 WS-SUPPLIER.
           05 WS-SUP-ID          PIC 9(9).
           05 WS-SUP-NAME        PIC X(50).
OCESQL*
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           PERFORM UNTIL WS-END-OF-FILE = 'Y'
               CALL 'SUPPLIER-DAL-DB' USING
                   WS-OPERATION WS-END-OF-FILE WS-SUPPLIER
               END-CALL
               IF WS-END-OF-FILE NOT = 'Y'
                   CALL 'DISPLAY-SUPPLIER' USING WS-SUPPLIER
               END-IF
           END-PERFORM.
           MOVE 'END ' TO WS-OPERATION.
           CALL 'SUPPLIER-DAL-DB' USING
               WS-OPERATION WS-END-OF-FILE WS-SUPPLIER
           END-CALL.
           STOP RUN.
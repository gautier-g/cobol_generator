       IDENTIFICATION DIVISION.
       PROGRAM-ID. INVOICE-PROCESSOR.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-OPERATION            PIC X(4) VALUE 'READ'.
       01 WS-ENDOFFILE            PIC X VALUE 'N'.
       01 WS-INVOICE.
           05 WS-INVID            PIC 9(9).
           05 WS-INVDATE          PIC X(10).
           05 WS-TOTALHT          PIC 9(7)V99.
           05 WS-TOTALTTC         PIC 9(7)V99.
       01 WS-TVA                  PIC 9V99 VALUE 1.20.
OCESQL*
       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           PERFORM UNTIL WS-ENDOFFILE = 'Y'
               CALL 'INVOICE-DAL-DB' USING WS-OPERATION
                                           WS-ENDOFFILE
                                           WS-INVOICE
               IF WS-ENDOFFILE = 'N'
                   COMPUTE WS-TOTALTTC = WS-TOTALHT * WS-TVA
                   CALL 'INVOICE-DISPLAY' USING WS-INVOICE
                   MOVE 'SAVE' TO WS-OPERATION
                   CALL 'INVOICE-DAL-DB' USING WS-OPERATION
                                               WS-ENDOFFILE
                                               WS-INVOICE
                   MOVE 'READ' TO WS-OPERATION
               END-IF
           END-PERFORM.

           MOVE 'END ' TO WS-OPERATION.
           CALL 'INVOICE-DAL-DB' USING WS-OPERATION
                                       WS-ENDOFFILE
                                       WS-INVOICE.
           STOP RUN.
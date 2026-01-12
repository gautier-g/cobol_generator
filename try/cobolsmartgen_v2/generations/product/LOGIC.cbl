       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROCESSPRODUCTS.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-END-OF-FILE         PIC X VALUE 'N'.
       01 WS-PRODUCT.
           05 WS-PRODID          PIC 9(9).
           05 WS-PRODNAME        PIC X(50).
           05 WS-PRICEHT         PIC 9(6)V99.
           05 WS-PRICETTC        PIC 9(6)V99.
       01 WS-OPERATION           PIC X(4) VALUE SPACES.
       01 WS-TVA                 PIC 9V99 VALUE 1.20.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           PERFORM UNTIL WS-END-OF-FILE = 'Y'
               MOVE 'FIND' TO WS-OPERATION
               CALL 'PRODUCTDAL' USING WS-OPERATION WS-END-OF-FILE
                   WS-PRODUCT
               IF WS-END-OF-FILE = 'N'
                   COMPUTE WS-PRICETTC = WS-PRICEHT * WS-TVA
                   MOVE 'SAVE' TO WS-OPERATION
                   CALL 'PRODUCTDAL' USING WS-OPERATION WS-END-OF-FILE
                       WS-PRODUCT
                   ADD 1 TO WS-PRODID
               END-IF
           END-PERFORM
           MOVE 'END ' TO WS-OPERATION
           CALL 'PRODUCTDAL' USING WS-OPERATION WS-END-OF-FILE
               WS-PRODUCT
           STOP RUN.
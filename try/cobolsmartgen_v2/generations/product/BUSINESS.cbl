       IDENTIFICATION DIVISION.
       PROGRAM-ID. DISPLAYPRODUCT.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-END-OF-FILE         PIC X VALUE 'N'.
       01 WS-PRODUCT.
           05 WS-PRODID          PIC 9(9).
           05 WS-PRODNAME        PIC X(50).
           05 WS-PRICEHT         PIC 9(6)V99.
           05 WS-PRICETTC        PIC 9(6)V99.
       01 WS-DISPLAY.
           05 FILLER             PIC X(20) VALUE "ID: ".
           05 WS-DISP-ID         PIC 9(9).
           05 FILLER             PIC X(20) VALUE " | NOM: ".
           05 WS-DISP-NAME       PIC X(50).
           05 FILLER             PIC X VALUE "|".
       01 WS-DISPLAY-PRICE.
           05 FILLER             PIC X(20) VALUE "PRIX HT: ".
           05 WS-DISP-PRICEHT    PIC ZZZZZ9.99.
           05 FILLER             PIC X(20) VALUE " | PRIX TTC: ".
           05 WS-DISP-PRICETTC   PIC ZZZZZ9.99.
           05 FILLER             PIC X VALUE "|".

       LINKAGE SECTION.
       01 LK-PRODUCT.
           05 LK-PRODID          PIC 9(9).
           05 LK-PRODNAME        PIC X(50).
           05 LK-PRICEHT         PIC 9(6)V99.
           05 LK-PRICETTC        PIC 9(6)V99.

       PROCEDURE DIVISION USING LK-PRODUCT.
       MAIN-LOGIC.
           MOVE LK-PRODID        TO WS-PRODID
           MOVE LK-PRODNAME      TO WS-PRODNAME
           MOVE LK-PRICEHT       TO WS-PRICEHT
           MOVE LK-PRICETTC      TO WS-PRICETTC

           MOVE WS-PRODID        TO WS-DISP-ID
           MOVE WS-PRODNAME      TO WS-DISP-NAME
           MOVE WS-PRICEHT       TO WS-DISP-PRICEHT
           MOVE WS-PRICETTC      TO WS-DISP-PRICETTC

           DISPLAY WS-DISPLAY
           DISPLAY WS-DISPLAY-PRICE

           EXIT PROGRAM.
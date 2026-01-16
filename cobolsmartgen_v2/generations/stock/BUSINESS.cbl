       IDENTIFICATION DIVISION.
       PROGRAM-ID. STOCK-DISPLAY.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-END-OF-FILE         PIC X VALUE 'N'.
       01 WS-STOCK.
           05 WS-STOCKID         PIC 9(9).
           05 WS-PRODID          PIC 9(9).
           05 WS-QUANTITY        PIC 9(9).
       01 WS-DISPLAY.
           05 FILLER             PIC X(5) VALUE "ID: ".
           05 WS-D-STOCKID       PIC 9(9).
           05 FILLER             PIC X(10) VALUE " | PROD: ".
           05 WS-D-PRODID        PIC 9(9).
           05 FILLER             PIC X(12) VALUE " | QTY: ".
           05 WS-D-QUANTITY      PIC 9(9).

       LINKAGE SECTION.
       01 LK-OPERATION           PIC X(4).

       PROCEDURE DIVISION USING LK-OPERATION.
       MAIN-LOGIC.
           IF LK-OPERATION = 'DISP'
               PERFORM DISPLAY-STOCK
           END-IF.
           EXIT PROGRAM.

       DISPLAY-STOCK.
           MOVE 'N' TO WS-END-OF-FILE.
           PERFORM UNTIL WS-END-OF-FILE = 'Y'
               CALL 'STOCK-DAL-DB' USING 'READ' WS-END-OF-FILE WS-STOCK
               IF WS-END-OF-FILE = 'N'
                   MOVE WS-STOCKID TO WS-D-STOCKID
                   MOVE WS-PRODID TO WS-D-PRODID
                   MOVE WS-QUANTITY TO WS-D-QUANTITY
                   DISPLAY WS-DISPLAY
               END-IF
           END-PERFORM.
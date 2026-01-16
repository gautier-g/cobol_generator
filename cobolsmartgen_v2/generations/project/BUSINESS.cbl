       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROJECT-DISPLAY.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-ENDOFFILE           PIC X VALUE 'N'.
       01 WS-PROJECT.
           05 WS-PROJID          PIC 9(9).
           05 WS-PROJNAME        PIC X(60).
           05 WS-BUDGET          PIC 9(10)V99.
       01 WS-BUDGET-DISPLAY      PIC ZZZZZZZZZ9.99.

       LINKAGE SECTION.
       01 LK-OPERATION           PIC X(4).
       01 LK-PROJECT.
           05 LK-PROJID          PIC 9(9).
           05 LK-PROJNAME        PIC X(60).
           05 LK-BUDGET          PIC 9(10)V99.

       PROCEDURE DIVISION USING LK-OPERATION LK-PROJECT.
       MAINLOGIC.
           EVALUATE LK-OPERATION
               WHEN 'DISP'
                   PERFORM DISPLAY-PROJECT
           END-EVALUATE.
           EXIT PROGRAM.

       DISPLAY-PROJECT.
           MOVE LK-PROJID        TO WS-PROJID.
           MOVE LK-PROJNAME      TO WS-PROJNAME.
           MOVE LK-BUDGET        TO WS-BUDGET.

           MOVE WS-BUDGET        TO WS-BUDGET-DISPLAY.

           DISPLAY "PROJET ID: " WS-PROJID.
           DISPLAY "NOM:      " WS-PROJNAME.
           DISPLAY "BUDGET:   " WS-BUDGET-DISPLAY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCULSALAIRE.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-EOF                 PIC X VALUE 'N'.
       01 WS-EMPLOYEE.
           05 WS-EMPID           PIC 9(4).
           05 WS-EMPNAME         PIC X(30).
           05 WS-SALBRUT         PIC 9(6)V99.
           05 WS-SALNET          PIC 9(6)V99.
       PROCEDURE DIVISION.
       MAIN.
           PERFORM UNTIL WS-EOF = 'Y'
               CALL 'EMPLOYEEDAL' USING 'READ' WS-EOF WS-EMPLOYEE
               IF WS-EOF NOT = 'Y'
                   COMPUTE WS-SALNET = WS-SALBRUT * 0.8
                   MOVE WS-EMPLOYEE TO WS-EMPLOYEE
                   CALL 'DISPLAYEMP' USING WS-EMPLOYEE
                   CALL 'EMPLOYEEDAL' USING 'SAVE' WS-EOF WS-EMPLOYEE
               END-IF
           END-PERFORM.
           CALL 'EMPLOYEEDAL' USING 'END ' WS-EOF WS-EMPLOYEE.
           STOP RUN.
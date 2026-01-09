       IDENTIFICATION DIVISION.
       PROGRAM-ID. DISPLAYEMP.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-EOF                 PIC X VALUE 'N'.
       01 WS-EMPLOYEE.
           05 WS-EMPID           PIC 9(4).
           05 WS-EMPNAME         PIC X(30).
           05 WS-SALARYBRUT      PIC 9(6)V99.
           05 WS-SALARYNET       PIC 9(6)V99.
       01 WS-DISPLAY.
           05 WS-DISP-ID         PIC ZZZ9.
           05 FILLER             PIC X VALUE SPACE.
           05 WS-DISP-NAME       PIC X(30).
           05 FILLER             PIC X VALUE SPACE.
           05 WS-DISP-BRUT       PIC ZZZZZ9.99.
           05 FILLER             PIC X VALUE SPACE.
           05 WS-DISP-NET        PIC ZZZZZ9.99.
       LINKAGE SECTION.
       PROCEDURE DIVISION.
       MAIN.
           PERFORM READ-EMPLOYEE.
           IF WS-EOF = 'N'
               PERFORM DISPLAY-EMPLOYEE
           END-IF.
           EXIT PROGRAM.

       READ-EMPLOYEE.
           CALL 'EMPLOYEEDAL' USING 'READ' WS-EOF WS-EMPLOYEE.
           IF WS-EOF = 'N'
               MOVE WS-EMPID      TO WS-DISP-ID
               MOVE WS-EMPNAME    TO WS-DISP-NAME
               MOVE WS-SALARYBRUT TO WS-DISP-BRUT
               MOVE WS-SALARYNET  TO WS-DISP-NET
           END-IF.

       DISPLAY-EMPLOYEE.
           DISPLAY "ID: " WS-DISP-ID.
           DISPLAY "NOM: " WS-DISP-NAME.
           DISPLAY "SALAIRE BRUT: " WS-DISP-BRUT.
           DISPLAY "SALAIRE NET: " WS-DISP-NET.
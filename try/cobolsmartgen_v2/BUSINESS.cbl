       IDENTIFICATION DIVISION.
       PROGRAM-ID. DISPLAYEMP.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-EOF                 PIC X VALUE 'N'.
       LINKAGE SECTION.
       01 LK-EMPLOYEE.
           05 LK-EMPID           PIC 9(4).
           05 LK-EMPNAME         PIC X(30).
           05 LK-SALBRUT         PIC 9(6)V99.
           05 LK-SALNET          PIC 9(6)V99.
       PROCEDURE DIVISION USING LK-EMPLOYEE.
       MAIN.
           DISPLAY "ID: " LK-EMPID.
           DISPLAY "NOM: " LK-EMPNAME.
           DISPLAY "SALAIRE BRUT: " LK-SALBRUT.
           DISPLAY "SALAIRE NET: " LK-SALNET.
           EXIT PROGRAM.
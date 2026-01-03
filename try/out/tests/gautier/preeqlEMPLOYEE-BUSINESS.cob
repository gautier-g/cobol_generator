       IDENTIFICATION DIVISION.
       PROGRAM-ID. EMPLOYEE-BUSINESS.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       LINKAGE SECTION.
       01 EMPLOYEE.
           05 EMP-ID          PIC 9(4).
           05 EMP-NAME        PIC A(30).
           05 SALARY-BRUT     PIC 9(6)V99.
           05 SALARY-NET      PIC 9(6)V99.
       
       PROCEDURE DIVISION USING EMPLOYEE.
       DISPLAY-EMPLOYEE.
           DISPLAY "EMPLOYE : " EMP-NAME
           DISPLAY "ID      : " EMP-ID
           DISPLAY "BRUT    : " SALARY-BRUT
           DISPLAY "NET     : " SALARY-NET.
           
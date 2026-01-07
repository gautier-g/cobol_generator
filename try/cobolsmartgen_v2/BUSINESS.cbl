       IDENTIFICATION DIVISION.
       PROGRAM-ID. DISPLAY-EMPLOYEE.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  EMPLOYEE.
           05  EMP-ID          PIC 9(5).
           05  EMP-NAME        PIC X(50).
           05  SALARY-BRUT     PIC 9(7)V99.
           05  SALARY-NET      PIC 9(7)V99.
       LINKAGE SECTION.
       01  LK-EMPLOYEE.
           05  LK-EMP-ID       PIC 9(5).
           05  LK-EMP-NAME     PIC X(50).
           05  LK-SALARY-BRUT  PIC 9(7)V99.
           05  LK-SALARY-NET   PIC 9(7)V99.
       PROCEDURE DIVISION USING LK-EMPLOYEE.
           MOVE LK-EMP-ID      TO EMP-ID
           MOVE LK-EMP-NAME    TO EMP-NAME
           MOVE LK-SALARY-BRUT TO SALARY-BRUT
           MOVE LK-SALARY-NET  TO SALARY-NET

           DISPLAY "Employee ID: " EMP-ID
           DISPLAY "Employee Name: " EMP-NAME
           DISPLAY "Brut Salary: " SALARY-BRUT
           DISPLAY "Net Salary: " SALARY-NET

           GOBACK.
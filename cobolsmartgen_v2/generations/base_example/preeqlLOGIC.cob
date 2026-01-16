       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCNETSALARY.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-EMPLOYEE.
           05 WS-EMPID            PIC 9(4).
           05 WS-EMPNAME          PIC X(30).
           05 WS-SALARYBRUT       PIC 9(6)V99.
           05 WS-SALARYNET        PIC 9(6)V99.
       01 WS-END-OF-FILE          PIC X VALUE 'N'.
       01 WS-OPERATION            PIC X(4) VALUE SPACES.

OCESQL*
       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           PERFORM UNTIL WS-END-OF-FILE = 'Y'
               MOVE 'READ' TO WS-OPERATION
               CALL 'EMPLOYEEDAL' USING WS-OPERATION
                   WS-END-OF-FILE WS-EMPLOYEE
               IF WS-END-OF-FILE NOT = 'Y'
                   COMPUTE WS-SALARYNET = WS-SALARYBRUT * 0.8
                   MOVE 'SAVE' TO WS-OPERATION
                   CALL 'EMPLOYEEDAL' USING WS-OPERATION
                       WS-END-OF-FILE WS-EMPLOYEE
                   CALL 'DISPLAYEMP' USING WS-EMPLOYEE
               END-IF
           END-PERFORM.

           MOVE 'END ' TO WS-OPERATION.
           CALL 'EMPLOYEEDAL' USING WS-OPERATION
               WS-END-OF-FILE WS-EMPLOYEE.
           STOP RUN.
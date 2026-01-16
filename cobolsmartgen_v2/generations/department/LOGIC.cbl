       IDENTIFICATION DIVISION.
       PROGRAM-ID. DISPLAY-ALL-DEPTS.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-DEPARTMENT.
           05 WS-DEPT-ID      PIC 9(4).
           05 WS-DEPT-NAME    PIC X(40).
       01 WS-END-OF-FILE      PIC X VALUE 'N'.
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           PERFORM UNTIL WS-END-OF-FILE = 'Y'
               CALL 'DEPARTMENT-DAL-DB' USING
                   BY CONTENT 'READ'
                   BY REFERENCE WS-END-OF-FILE
                   BY REFERENCE WS-DEPARTMENT
               END-CALL
               IF WS-END-OF-FILE NOT = 'Y'
                   CALL 'DISPLAY-DEPT' USING WS-DEPARTMENT
               END-IF
           END-PERFORM.
           STOP RUN.
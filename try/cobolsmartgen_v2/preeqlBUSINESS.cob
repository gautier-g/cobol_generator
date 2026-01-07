       IDENTIFICATION DIVISION.
       PROGRAM-ID. DISPLAYEMP.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-DISPLAY-LINE.
           05 FILLER              PIC X(5) VALUE SPACES.
           05 WS-DISP-EMPID       PIC 9(4).
           05 FILLER              PIC X(5) VALUE SPACES.
           05 WS-DISP-EMPNAME     PIC X(30).
           05 FILLER              PIC X(5) VALUE SPACES.
           05 WS-DISP-SALBRUT     PIC ZZZZZ9.99.
           05 FILLER              PIC X(5) VALUE SPACES.
           05 WS-DISP-SALNET      PIC ZZZZZ9.99.

OCESQL*
       LINKAGE SECTION.
       01 LK-EMPLOYEE.
           05 LK-EMPID            PIC 9(4).
           05 LK-EMPNAME          PIC X(30).
           05 LK-SALBRUT          PIC 9(6)V99.
           05 LK-SALNET           PIC 9(6)V99.

       PROCEDURE DIVISION USING LK-EMPLOYEE.
       MAIN.
           MOVE LK-EMPID          TO WS-DISP-EMPID
           MOVE LK-EMPNAME        TO WS-DISP-EMPNAME
           MOVE LK-SALBRUT        TO WS-DISP-SALBRUT
           MOVE LK-SALNET         TO WS-DISP-SALNET

           DISPLAY WS-DISPLAY-LINE
           EXIT PROGRAM.
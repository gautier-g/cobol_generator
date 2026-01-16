       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTOMER-DISPLAY.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-EOF                  PIC X VALUE 'N'.
       01 WS-CUSTOMER.
           05 WS-CUST-ID          PIC 9(9).
           05 WS-CUST-NAME        PIC X(40).
           05 WS-CUST-CITY        PIC X(30).
       LINKAGE SECTION.
       01 LK-CUSTOMER.
           05 LK-CUST-ID          PIC 9(9).
           05 LK-CUST-NAME        PIC X(40).
           05 LK-CUST-CITY        PIC X(30).
       PROCEDURE DIVISION USING LK-CUSTOMER.
       MAIN-LOGIC.
           MOVE LK-CUST-ID        TO WS-CUST-ID
           MOVE LK-CUST-NAME      TO WS-CUST-NAME
           MOVE LK-CUST-CITY      TO WS-CUST-CITY

           DISPLAY "ID CLIENT: " WS-CUST-ID
           DISPLAY "NOM: " WS-CUST-NAME
           DISPLAY "VILLE: " WS-CUST-CITY
           EXIT PROGRAM.
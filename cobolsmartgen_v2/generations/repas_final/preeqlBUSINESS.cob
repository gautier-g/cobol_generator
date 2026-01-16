       IDENTIFICATION DIVISION.
       PROGRAM-ID. AFFICHEREPAS.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-REPAS-TYPE-LIB.
          05 PIC X(20) VALUE "Petit-dejeuner".
          05 PIC X(20) VALUE "Dejeuner".
          05 PIC X(20) VALUE "Diner".
          05 PIC X(20) VALUE "Collation".
          05 PIC X(20) VALUE "Autre".
       01 WS-TYPE-TAB REDEFINES WS-REPAS-TYPE-LIB.
          05 WS-TYPE OCCURS 5 TIMES PIC X(20).
OCESQL*
       LINKAGE SECTION.
       01 LK-REPAS.
          05 LK-REPAS-ID                PIC 9(9).
          05 LK-REPAS-ID-ACTIVITE       PIC 9(9).
          05 LK-REPAS-TYPE              PIC 9(9).
          05 LK-REPAS-NBREPAS           PIC 9(9).
       PROCEDURE DIVISION USING LK-REPAS.
       MAIN-LOGIC.
           DISPLAY "ID Repas: " LK-REPAS-ID
           DISPLAY "ID Activite: " LK-REPAS-ID-ACTIVITE
           DISPLAY "Type: " WS-TYPE(LK-REPAS-TYPE)
           DISPLAY "Nombre de repas: " LK-REPAS-NBREPAS
           EXIT PROGRAM.
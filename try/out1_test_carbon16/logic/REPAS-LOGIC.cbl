       IDENTIFICATION DIVISION.
       PROGRAM-ID. REPAS-LOGIC.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GnuCOBOL.
       OBJECT-COMPUTER. GnuCOBOL.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  END-OF-FILE            PIC X.
       77  OPERATION              PIC X(4).
       77  WS-COUNT-TOTAL         PIC 9(6).
       77  WS-COUNT-ERROR         PIC 9(6).
       01  REPAS.
           05 REPAS-ID            PIC 9(9).
           05 REPAS-ID-ACTIVITE   PIC 9(9).
           05 REPAS-TYPE          PIC 9(2).
           05 REPAS-NBREPAS       PIC S9(5).
       PROCEDURE DIVISION.
       MAIN-PROCESS.
           DISPLAY '=========================================='
           DISPLAY 'DEBUT TRAITEMENT BATCH REPAS'
           DISPLAY '=========================================='
           INITIALIZE WS-COUNT-TOTAL
           INITIALIZE WS-COUNT-ERROR
           MOVE 'N' TO END-OF-FILE
           MOVE 'READ' TO OPERATION
           CALL 'REPAS-DAL-DB' USING OPERATION END-OF-FILE REPAS
           END-CALL
           PERFORM UNTIL END-OF-FILE EQUAL 'Y'
               ADD 1 TO WS-COUNT-TOTAL
               PERFORM CALCULATE-REPAS
               MOVE 'SAVE' TO OPERATION
               CALL 'REPAS-DAL-DB' USING OPERATION END-OF-FILE REPAS
               END-CALL
               CALL 'REPAS-BUSINESS' USING REPAS
               END-CALL
               MOVE 'READ' TO OPERATION
               CALL 'REPAS-DAL-DB' USING OPERATION END-OF-FILE REPAS
               END-CALL
           END-PERFORM
           MOVE 'END ' TO OPERATION
           CALL 'REPAS-DAL-DB' USING OPERATION END-OF-FILE REPAS
           END-CALL
           DISPLAY '=========================================='
           DISPLAY 'FIN TRAITEMENT BATCH REPAS'
           DISPLAY 'Nombre repas traites: ' WS-COUNT-TOTAL
           DISPLAY 'Repas en erreur: ' WS-COUNT-ERROR
           DISPLAY '=========================================='
           STOP RUN
       .
       CALCULATE-REPAS.
           IF REPAS-NBREPAS OF REPAS < ZERO
               DISPLAY 'ANOMALIE: Nombre repas invalide'
               MOVE ZERO TO REPAS-NBREPAS OF REPAS
               ADD 1 TO WS-COUNT-ERROR
           END-IF
       .
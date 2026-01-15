       IDENTIFICATION DIVISION.
       PROGRAM-ID. PARTICIPATION-LOGIC.
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
       01  PARTICIPATION.
           05 PARTICIPATION-ID-ACTIVITE     PIC 9(9).
           05 PARTICIPATION-ID-USER         PIC 9(9).
           05 PARTICIPATION-MODE-TRANSPORT  PIC S9(2).
       PROCEDURE DIVISION.
       MAIN-PROCESS.
           INITIALIZE WS-COUNT-TOTAL WS-COUNT-ERROR
           MOVE 'N' TO END-OF-FILE
           DISPLAY '=========================================='
           DISPLAY 'DEBUT TRAITEMENT BATCH PARTICIPATIONS'
           DISPLAY '=========================================='
           MOVE 'READ' TO OPERATION
           CALL 'PARTICIPATION-DAL-DB' USING OPERATION
                                             END-OF-FILE
                                             PARTICIPATION
           PERFORM UNTIL END-OF-FILE = 'Y'
               ADD 1 TO WS-COUNT-TOTAL
               PERFORM CALCULATE-TRANSPORT
               MOVE 'SAVE' TO OPERATION
               CALL 'PARTICIPATION-DAL-DB' USING OPERATION
                                                 END-OF-FILE
                                                 PARTICIPATION
               CALL 'PARTICIPATION-BUSINESS' USING PARTICIPATION
               MOVE 'READ' TO OPERATION
               CALL 'PARTICIPATION-DAL-DB' USING OPERATION
                                                 END-OF-FILE
                                                 PARTICIPATION
           END-PERFORM
           MOVE 'END ' TO OPERATION
           CALL 'PARTICIPATION-DAL-DB' USING OPERATION
                                             END-OF-FILE
                                             PARTICIPATION
           DISPLAY '=========================================='
           DISPLAY 'FIN TRAITEMENT BATCH PARTICIPATIONS'
           DISPLAY 'Nombre participations traitees: ' WS-COUNT-TOTAL
           DISPLAY 'Participations en erreur: ' WS-COUNT-ERROR
           DISPLAY '=========================================='
           STOP RUN.
       CALCULATE-TRANSPORT.
           IF PARTICIPATION-MODE-TRANSPORT OF PARTICIPATION < 0
               MOVE 0 TO PARTICIPATION-MODE-TRANSPORT OF PARTICIPATION
               ADD 1 TO WS-COUNT-ERROR
               DISPLAY 'ANOMALIE: Mode transport invalide'
           END-IF.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACTIVITE-LOGIC.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  END-OF-FILE            PIC X VALUE 'N'.
           88  EOF-REACHED        VALUE 'Y'.
           88  NOT-EOF            VALUE 'N'.
       77  OPERATION              PIC X(4).
       77  WS-COUNT-TOTAL         PIC 9(6) VALUE 0.
       77  WS-COUNT-ERROR         PIC 9(6) VALUE 0.
       01  ACTIVITE.
           05 ACTIVITE-ID         PIC 9(9).
           05 ACTIVITE-NOM        PIC X(50).
           05 ACTIVITE-TYPE       PIC X(20).
           05 ACTIVITE-IDANTENNE  PIC 9(9).
           05 ACTIVITE-ANIMATEUR  PIC 9(9).
           05 ACTIVITE-NBPART     PIC 9(9).
           05 ACTIVITE-TRANSPORT  PIC 9(2).
           05 ACTIVITE-LIEU       PIC X(100).
           05 ACTIVITE-DISTANCE   PIC 9(10).
           05 ACTIVITE-HEBERG     PIC 9(1).
           05 ACTIVITE-REPAS      PIC 9(1).
           05 ACTIVITE-EMPREINTE  PIC S9(9)V9(4).
           05 ANTENNE-NOM         PIC X(50).
           05 ANTENNE-REGION      PIC X(50).
           05 USER-NOM            PIC X(50).
           05 USER-MAIL           PIC X(80).
       PROCEDURE DIVISION.
       MAIN-PROCESS.
           DISPLAY '=========================================='
           DISPLAY 'DEBUT TRAITEMENT BATCH ACTIVITES CARBONE'
           DISPLAY '=========================================='
           MOVE 'READ' TO OPERATION
           CALL 'ACTIVITE-DAL-DB' USING OPERATION END-OF-FILE ACTIVITE
           PERFORM UNTIL EOF-REACHED
               ADD 1 TO WS-COUNT-TOTAL
               PERFORM CALCULATE-EMPREINTE
               MOVE 'SAVE' TO OPERATION
               CALL 'ACTIVITE-DAL-DB' USING OPERATION END-OF-FILE 
               ACTIVITE
               CALL 'ACTIVITE-BUSINESS' USING ACTIVITE
               MOVE 'READ' TO OPERATION
               CALL 'ACTIVITE-DAL-DB' USING OPERATION END-OF-FILE 
               ACTIVITE
           END-PERFORM
           MOVE 'END ' TO OPERATION
           CALL 'ACTIVITE-DAL-DB' USING OPERATION END-OF-FILE ACTIVITE
           DISPLAY '=========================================='
           DISPLAY 'FIN TRAITEMENT BATCH ACTIVITES CARBONE'
           DISPLAY 'Nombre activites traitees: ' WS-COUNT-TOTAL
           DISPLAY 'Activites en erreur: ' WS-COUNT-ERROR
           DISPLAY '=========================================='
           STOP RUN.
       
       CALCULATE-EMPREINTE.

           IF ACTIVITE-EMPREINTE < 0
               MOVE 0 TO ACTIVITE-EMPREINTE
               ADD 1 TO WS-COUNT-ERROR
               DISPLAY 'ANOMALIE: Empreinte carbone invalide'
           END-IF.
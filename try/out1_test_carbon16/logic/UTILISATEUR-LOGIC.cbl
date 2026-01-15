       IDENTIFICATION DIVISION.
       PROGRAM-ID. UTILISATEUR-LOGIC.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GnuCOBOL.
       OBJECT-COMPUTER. GnuCOBOL.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  END-OF-FILE            PIC X.
       77  OPERATION              PIC X(4).
       77  WS-COUNT-TOTAL         PIC 9(6).
       77  WS-COUNT-ERROR         PIC 9(6).
       01  UTILISATEUR.
           05 USER-ID             PIC 9(9).
           05 USER-NOM            PIC X(50).
           05 USER-MAIL           PIC X(80).
           05 USER-PASS           PIC X(256).
           05 USER-ROLE           PIC X(15).
           05 USER-ID-ANTENNE     PIC 9(9).
           05 USER-LAST-LOGIN     PIC S9(11).
       PROCEDURE DIVISION.
       MAIN-PROCESS.
           DISPLAY '=========================================='
           DISPLAY 'DEBUT TRAITEMENT BATCH UTILISATEURS'
           DISPLAY '=========================================='
           INITIALIZE WS-COUNT-TOTAL
           INITIALIZE WS-COUNT-ERROR
           MOVE 'N' TO END-OF-FILE
           MOVE 'READ' TO OPERATION
           CALL 'UTILISATEUR-DAL-DB' USING OPERATION END-OF-FILE 
           UTILISATEUR
           PERFORM UNTIL END-OF-FILE = 'Y'
              ADD 1 TO WS-COUNT-TOTAL
              PERFORM CALCULATE-LOGIN
              MOVE 'SAVE' TO OPERATION
              CALL 'UTILISATEUR-DAL-DB' USING OPERATION END-OF-FILE 
              UTILISATEUR
              CALL 'UTILISATEUR-BUSINESS' USING UTILISATEUR
              MOVE 'READ' TO OPERATION
              CALL 'UTILISATEUR-DAL-DB' USING OPERATION END-OF-FILE 
              UTILISATEUR
           END-PERFORM
           MOVE 'END ' TO OPERATION
           CALL 'UTILISATEUR-DAL-DB' USING OPERATION END-OF-FILE 
           UTILISATEUR
           DISPLAY '=========================================='
           DISPLAY 'FIN TRAITEMENT BATCH UTILISATEURS'
           DISPLAY 'Nombre utilisateurs traites: ' WS-COUNT-TOTAL
           DISPLAY 'Utilisateurs en erreur: ' WS-COUNT-ERROR
           DISPLAY '=========================================='
           STOP RUN
       .
       CALCULATE-LOGIN.
           IF USER-LAST-LOGIN OF UTILISATEUR < ZERO
              MOVE ZERO TO USER-LAST-LOGIN OF UTILISATEUR
              ADD 1 TO WS-COUNT-ERROR
           END-IF
       .
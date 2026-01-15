       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTGESTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-OPERATION           PIC X(10).
       01 WS-END-OF-FILE         PIC X VALUE 'N'.
       01 WS-ACTIVITE.
           05 WS-ACTIVITE-ID             PIC 9(9) VALUE 1.
           05 WS-ACTIVITE-NOM            PIC X(50)
              VALUE "Activite de test".
           05 WS-ACTIVITE-TYPE           PIC X(20)
              VALUE "Sport".
           05 WS-ACTIVITE-IDANTENNE      PIC 9(9) VALUE 1.
           05 WS-ACTIVITE-ANIMATEUR      PIC 9(9) VALUE 1.
           05 WS-ACTIVITE-NBPARTICIPANTS PIC 9(9) VALUE 10.
           05 WS-ACTIVITE-MODETRANSPORT  PIC 9(9) VALUE 1.
           05 WS-ACTIVITE-LIEU           PIC X(100)
              VALUE "Stade municipal".
           05 WS-ACTIVITE-DISTANCE       PIC 9(9) VALUE 5.
           05 WS-ACTIVITE-HEBERGEMENT    PIC 9(9) VALUE 0.
           05 WS-ACTIVITE-REPASPREVU     PIC 9(9) VALUE 1.
           05 WS-ACTIVITE-EMPREINTETOTALE PIC 9(13)V9(4)
              VALUE 100.5000.
       01 WS-REPAS.
           05 WS-REPAS-ID                PIC 9(9) VALUE 1.
           05 WS-REPAS-ID-ACTIVITE       PIC 9(9) VALUE 1.
           05 WS-REPAS-TYPE              PIC 9(9) VALUE 1.
           05 WS-REPAS-NBREPAS           PIC 9(9) VALUE 10.
       01 WS-REPAS-TEST.
           05 WS-REPAS-ID-TEST           PIC 9(9).
           05 WS-REPAS-ID-ACTIVITE-TEST  PIC 9(9).
           05 WS-REPAS-TYPE-TEST         PIC 9(9).
           05 WS-REPAS-NBREPAS-TEST      PIC 9(9).
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY "TEST CREATEACT".
           MOVE "CREATEACT" TO WS-OPERATION.
           CALL "GESTIONACTIVITE" USING WS-OPERATION WS-END-OF-FILE
               WS-ACTIVITE WS-REPAS.
           IF WS-END-OF-FILE = 'Y'
               DISPLAY "ERREUR CREATEACT"
           ELSE
               DISPLAY "CREATEACT OK"
           END-IF.

           DISPLAY "TEST CREATEREP".
           MOVE "CREATEREP" TO WS-OPERATION.
           CALL "GESTIONACTIVITE" USING WS-OPERATION WS-END-OF-FILE
               WS-ACTIVITE WS-REPAS.
           IF WS-END-OF-FILE = 'Y'
               DISPLAY "ERREUR CREATEREP"
           ELSE
               DISPLAY "CREATEREP OK"
           END-IF.

           DISPLAY "TEST READREP".
           MOVE 1 TO WS-REPAS-ID.
           MOVE "READREP" TO WS-OPERATION.
           CALL "GESTIONACTIVITE" USING WS-OPERATION WS-END-OF-FILE
               WS-ACTIVITE WS-REPAS.
           IF WS-END-OF-FILE = 'Y'
               DISPLAY "ERREUR READREP"
           ELSE
               DISPLAY "READREP OK"
               MOVE WS-REPAS TO WS-REPAS-TEST
               CALL "AFFICHEREPAS" USING WS-REPAS-TEST
           END-IF.

           DISPLAY "TEST LISTREP".
           MOVE 1 TO WS-REPAS-ID-ACTIVITE.
           MOVE "LISTREP" TO WS-OPERATION.
           PERFORM UNTIL WS-END-OF-FILE = 'Y'
               CALL "GESTIONACTIVITE" USING WS-OPERATION WS-END-OF-FILE
                   WS-ACTIVITE WS-REPAS
               IF WS-END-OF-FILE = 'N'
                   MOVE WS-REPAS TO WS-REPAS-TEST
                   CALL "AFFICHEREPAS" USING WS-REPAS-TEST
               END-IF
           END-PERFORM.
           MOVE 'N' TO WS-END-OF-FILE.
           DISPLAY "LISTREP OK".

           DISPLAY "TEST UPDATEREP".
           MOVE 1 TO WS-REPAS-ID.
           MOVE 2 TO WS-REPAS-TYPE.
           MOVE 15 TO WS-REPAS-NBREPAS.
           MOVE "UPDATEREP" TO WS-OPERATION.
           CALL "GESTIONACTIVITE" USING WS-OPERATION WS-END-OF-FILE
               WS-ACTIVITE WS-REPAS.
           IF WS-END-OF-FILE = 'Y'
               DISPLAY "ERREUR UPDATEREP"
           ELSE
               DISPLAY "UPDATEREP OK"
           END-IF.

           DISPLAY "TEST DELETEREP".
           MOVE 1 TO WS-REPAS-ID.
           MOVE "DELETEREP" TO WS-OPERATION.
           CALL "GESTIONACTIVITE" USING WS-OPERATION WS-END-OF-FILE
               WS-ACTIVITE WS-REPAS.
           IF WS-END-OF-FILE = 'Y'
               DISPLAY "ERREUR DELETEREP"
           ELSE
               DISPLAY "DELETEREP OK"
           END-IF.

           MOVE "END" TO WS-OPERATION.
           CALL "GESTIONACTIVITE" USING WS-OPERATION WS-END-OF-FILE
               WS-ACTIVITE WS-REPAS.
           STOP RUN.
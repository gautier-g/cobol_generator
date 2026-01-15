       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-ANTENNE-DAL.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-ANTENNE.
           05 WS-ANTENNE-ID      PIC 9(9) VALUE 123456789.
           05 WS-ANTENNE-NOM     PIC X(50) VALUE "ANTENNE TEST".
           05 WS-ANTENNE-REGION  PIC X(50) VALUE "REGION TEST".
       01 WS-OPERATION           PIC X(4).
       01 WS-END-OF-FILE         PIC X.
       01 WS-TEST-READ.
           05 WS-R-ANTENNE-ID    PIC 9(9).
           05 WS-R-ANTENNE-NOM   PIC X(50).
           05 WS-R-ANTENNE-REG   PIC X(50).
       01 WS-TEST-LIST.
           05 WS-L-ANTENNE-ID    PIC 9(9).
           05 WS-L-ANTENNE-NOM   PIC X(50).
           05 WS-L-ANTENNE-REG   PIC X(50).

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY "DEBUT DES TESTS".

      *    TEST CREATE
           MOVE "CREA" TO WS-OPERATION.
           MOVE "N" TO WS-END-OF-FILE.
           CALL "ANTENNE-DAL-DB" USING WS-OPERATION WS-END-OF-FILE
                                       WS-ANTENNE.
           IF WS-END-OF-FILE = "Y"
               DISPLAY "ERREUR CREATION ANTENNE"
           ELSE
               DISPLAY "CREATION ANTENNE OK"
           END-IF.

      *    TEST READ
           MOVE 123456789 TO WS-R-ANTENNE-ID.
           MOVE SPACES TO WS-R-ANTENNE-NOM WS-R-ANTENNE-REG.
           MOVE WS-R-ANTENNE-ID TO WS-ANTENNE-ID.
           MOVE "READ" TO WS-OPERATION.
           MOVE "N" TO WS-END-OF-FILE.
           CALL "ANTENNE-DAL-DB" USING WS-OPERATION WS-END-OF-FILE
                                       WS-ANTENNE.
           IF WS-END-OF-FILE = "Y"
               DISPLAY "ERREUR LECTURE ANTENNE"
           ELSE
               DISPLAY "LECTURE ANTENNE OK"
               DISPLAY "NOM: " WS-ANTENNE-NOM
               DISPLAY "REGION: " WS-ANTENNE-REGION
           END-IF.

      *    TEST UPDATE
           MOVE "ANTENNE MODIFIEE" TO WS-ANTENNE-NOM.
           MOVE "REGION MODIFIEE" TO WS-ANTENNE-REGION.
           MOVE "UPDA" TO WS-OPERATION.
           MOVE "N" TO WS-END-OF-FILE.
           CALL "ANTENNE-DAL-DB" USING WS-OPERATION WS-END-OF-FILE
                                       WS-ANTENNE.
           DISPLAY "MISE A JOUR ANTENNE OK".

      *    TEST LIST
           MOVE "LIST" TO WS-OPERATION.
           MOVE "N" TO WS-END-OF-FILE.
           PERFORM UNTIL WS-END-OF-FILE = "Y"
               CALL "ANTENNE-DAL-DB" USING WS-OPERATION WS-END-OF-FILE
                                           WS-ANTENNE
               IF WS-END-OF-FILE = "N"
                   DISPLAY "ANTENNE TROUVEE: " WS-ANTENNE-ID
                   CALL "AFFICHE-ANTENNE" USING WS-ANTENNE
               END-IF
           END-PERFORM.
           DISPLAY "FIN LISTE ANTENNES".

      *    TEST DELETE
           MOVE 123456789 TO WS-ANTENNE-ID.
           MOVE "DELE" TO WS-OPERATION.
           MOVE "N" TO WS-END-OF-FILE.
           CALL "ANTENNE-DAL-DB" USING WS-OPERATION WS-END-OF-FILE
                                       WS-ANTENNE.
           DISPLAY "SUPPRESSION ANTENNE OK".

      *    TEST END
           MOVE "END " TO WS-OPERATION.
           MOVE "N" TO WS-END-OF-FILE.
           CALL "ANTENNE-DAL-DB" USING WS-OPERATION WS-END-OF-FILE
                                       WS-ANTENNE.
           DISPLAY "FIN DES TESTS".
           STOP RUN.
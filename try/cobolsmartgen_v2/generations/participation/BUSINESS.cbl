       IDENTIFICATION DIVISION.
       PROGRAM-ID. DISPLAY-PARTICIPATION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-EOF               PIC X VALUE 'N'.
       01 WS-COUNT             PIC 9(3) VALUE 0.

       LINKAGE SECTION.
       01 LK-JOINTURE.
           05 LKJOINTACTIVITEID PIC 9(9).
           05 LKJOINTACTIVITENOM PIC X(50).
           05 LKJOINTACTIVITETYPE PIC X(20).
           05 LKJOINTACTIVITELIEU PIC X(100).
           05 LKJOINUSERID      PIC 9(9).
           05 LKJOINUSERNOM     PIC X(50).
           05 LKJOINUSERMAIL    PIC X(80).
           05 LKJOINMODETRANS   PIC 9(9).
       01 LK-END-OF-FILE       PIC X.

       PROCEDURE DIVISION USING LK-JOINTURE LK-END-OF-FILE.
       MAIN-LOGIC.
           DISPLAY "INFORMATIONS DE PARTICIPATION:".
           DISPLAY "----------------------------------------".

           IF LKJOINTACTIVITEID = 0
               DISPLAY "Aucune participation trouvee."
               MOVE 'Y' TO LK-END-OF-FILE
               EXIT PROGRAM
           END-IF.

           DISPLAY "ID Activite: " LKJOINTACTIVITEID.
           DISPLAY "Nom Activite: " LKJOINTACTIVITENOM.
           DISPLAY "Type Activite: " LKJOINTACTIVITETYPE.
           DISPLAY "Lieu: " LKJOINTACTIVITELIEU.
           DISPLAY "ID Utilisateur: " LKJOINUSERID.
           DISPLAY "Nom Utilisateur: " LKJOINUSERNOM.
           DISPLAY "Email: " LKJOINUSERMAIL.

           EVALUATE LKJOINMODETRANS
               WHEN 1 DISPLAY "Mode Transport: Voiture"
               WHEN 2 DISPLAY "Mode Transport: Train"
               WHEN 3 DISPLAY "Mode Transport: Bus"
               WHEN 4 DISPLAY "Mode Transport: Avion"
               WHEN 5 DISPLAY "Mode Transport: Covoiturage"
               WHEN OTHER DISPLAY "Mode Transport: Inconnu"
           END-EVALUATE.

           DISPLAY "----------------------------------------".
           EXIT PROGRAM.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACTIVITE-DAL-DB.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GnuCOBOL.
       OBJECT-COMPUTER. GnuCOBOL.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           EXEC SQL INCLUDE SQLCA END-EXEC.
       01  WS-CONNECTED-FLAG      PIC X.
       01  WS-CURSOR-OPEN-FLAG    PIC X.
       01  WS-ACTIVITE-ID         PIC 9(9).
       01  WS-ACTIVITE-NOM        PIC X(50).
       01  WS-ACTIVITE-TYPE       PIC X(20).
       01  WS-ACTIVITE-IDANTENNE  PIC 9(9).
       01  WS-ACTIVITE-ANIMATEUR  PIC 9(9).
       01  WS-ACTIVITE-NBPART     PIC 9(9).
       01  WS-ACTIVITE-TRANSPORT  PIC 9(2).
       01  WS-ACTIVITE-LIEU       PIC X(100).
       01  WS-ACTIVITE-DISTANCE   PIC 9(10).
       01  WS-ACTIVITE-HEBERG     PIC 9(1).
       01  WS-ACTIVITE-REPAS      PIC 9(1).
       01  WS-ACTIVITE-EMPREINTE  PIC S9(9)V9(4).
       01  WS-ANTENNE-NOM         PIC X(50).
       01  WS-ANTENNE-REGION      PIC X(50).
       01  WS-USER-NOM            PIC X(50).
       01  WS-USER-MAIL           PIC X(80).
       01  WS-DB-NAME             PIC X(64).
       01  WS-DB-USER             PIC X(64).
       01  WS-DB-PASSWORD         PIC X(64).
       LINKAGE SECTION.
       01  LK-OPERATION           PIC X(4).
       01  LK-END-OF-FILE         PIC X.
       01  LK-ACTIVITE.
           05 LK-ACTIVITE-ID PIC 9(9).
           05 LK-ACTIVITE-NOM PIC X(50).
           05 LK-ACTIVITE-TYPE PIC X(20).
           05 LK-ACTIVITE-IDANTENNE PIC 9(9).
           05 LK-ACTIVITE-ANIMATEUR PIC 9(9).
           05 LK-ACTIVITE-NBPART PIC 9(9).
           05 LK-ACTIVITE-TRANSPORT PIC 9(2).
           05 LK-ACTIVITE-LIEU PIC X(100).
           05 LK-ACTIVITE-DISTANCE PIC 9(10).
           05 LK-ACTIVITE-HEBERG PIC 9(1).
           05 LK-ACTIVITE-REPAS PIC 9(1).
           05 LK-ACTIVITE-EMPREINTE PIC S9(9)V9(4).
           05 LK-ANTENNE-NOM PIC X(50).
           05 LK-ANTENNE-REGION PIC X(50).
           05 LK-USER-NOM PIC X(50).
           05 LK-USER-MAIL PIC X(80).
       PROCEDURE DIVISION USING LK-OPERATION LK-END-OF-FILE LK-ACTIVITE.
       MAIN-ENTRY.
           EVALUATE LK-OPERATION
               WHEN 'READ'
                   PERFORM DAL-CONNECT
                   PERFORM DAL-READ
               WHEN 'SAVE'
                   PERFORM DAL-CONNECT
                   PERFORM DAL-SAVE
               WHEN 'END '
                   PERFORM DAL-END
               WHEN OTHER
                   DISPLAY 'ERREUR: Operation inconnue: ' LK-OPERATION
           END-EVALUATE
           GOBACK.
       DAL-SET-ENV.
           MOVE 'N' TO WS-CONNECTED-FLAG
           MOVE 'N' TO WS-CURSOR-OPEN-FLAG
           ACCEPT WS-DB-USER FROM ENVIRONMENT 'PGUSER'
           ACCEPT WS-DB-PASSWORD FROM ENVIRONMENT 'PGPASSWORD'
           ACCEPT WS-DB-NAME FROM ENVIRONMENT 'PGDATABASE'.
       DAL-CONNECT.
           IF WS-CONNECTED-FLAG = 'Y'
               CONTINUE
           ELSE
               ACCEPT WS-DB-USER FROM ENVIRONMENT 'PGUSER'
               ACCEPT WS-DB-PASSWORD FROM ENVIRONMENT 'PGPASSWORD'
               ACCEPT WS-DB-NAME FROM ENVIRONMENT 'PGDATABASE'
               EXEC SQL
                   CONNECT :WS-DB-USER IDENTIFIED BY :WS-DB-PASSWORD
                       USING :WS-DB-NAME
               END-EXEC
               IF SQLCODE = ZERO
                   DISPLAY 'Connexion DB reussie'
                   MOVE 'Y' TO WS-CONNECTED-FLAG
               ELSE
                   DISPLAY 'ERREUR CONNECT: SQLCODE=' SQLCODE
                   DISPLAY 'SQLSTATE=' SQLSTATE
                   DISPLAY 'SQLERRMC=' SQLERRMC
                   GOBACK
               END-IF
           END-IF.
       DAL-READ.
           MOVE 'N' TO LK-END-OF-FILE
           IF WS-CURSOR-OPEN-FLAG NOT EQUAL 'Y'
               EXEC SQL
                   DECLARE C_ACT CURSOR FOR
                   SELECT a.ACTIVITE_ID, a.ACTIVITE_NOM, a.ACTIVITE_TYPE
                          ,
                          a.ACTIVITE_IDANTENNE, a.ACTIVITE_ANIMATEUR,
                          a.ACTIVITE_NBPARTICIPANTS,
                          a.ACTIVITE_MODETRANSPORT,
                          a.ACTIVITE_LIEU, a.ACTIVITE_DISTANCE,
                          a.ACTIVITE_HEBERGEMENT,
                          a.ACTIVITE_REPASPREVU,
                          a.ACTIVITE_EMPREINTETOTALE,
                          an.ANTENNE_NOM, an.ANTENNE_REGION,
                          u.USER_NOM, u.USER_MAIL
                   FROM ACTIVITE a
                   INNER JOIN ANTENNE an
                       ON a.ACTIVITE_IDANTENNE = an.ANTENNE_ID
                   INNER JOIN UTILISATEUR u
                       ON a.ACTIVITE_ANIMATEUR = u.USER_ID
                   ORDER BY a.ACTIVITE_ID
               END-EXEC
               EXEC SQL OPEN C_ACT END-EXEC
               IF SQLCODE NOT EQUAL ZERO
                   DISPLAY 'ERREUR OPEN: SQLCODE=' SQLCODE
                   MOVE 'Y' TO LK-END-OF-FILE
                   GOBACK
               END-IF
               MOVE 'Y' TO WS-CURSOR-OPEN-FLAG
               DISPLAY 'Curseur C_ACT ouvert'
           END-IF
           EXEC SQL
               FETCH C_ACT INTO
                   :WS-ACTIVITE-ID,
                   :WS-ACTIVITE-NOM,
                   :WS-ACTIVITE-TYPE,
                   :WS-ACTIVITE-IDANTENNE,
                   :WS-ACTIVITE-ANIMATEUR,
                   :WS-ACTIVITE-NBPART,
                   :WS-ACTIVITE-TRANSPORT,
                   :WS-ACTIVITE-LIEU,
                   :WS-ACTIVITE-DISTANCE,
                   :WS-ACTIVITE-HEBERG,
                   :WS-ACTIVITE-REPAS,
                   :WS-ACTIVITE-EMPREINTE,
                   :WS-ANTENNE-NOM,
                   :WS-ANTENNE-REGION,
                   :WS-USER-NOM,
                   :WS-USER-MAIL
           END-EXEC
           EVALUATE TRUE
               WHEN SQLCODE EQUAL ZERO
                   CONTINUE
               WHEN SQLCODE EQUAL 100
                   MOVE 'Y' TO LK-END-OF-FILE
               WHEN OTHER
                   DISPLAY 'ERREUR FETCH: SQLCODE=' SQLCODE
                   MOVE 'Y' TO LK-END-OF-FILE
           END-EVALUATE
           IF SQLCODE EQUAL ZERO
               MOVE WS-ACTIVITE-ID TO LK-ACTIVITE-ID OF LK-ACTIVITE
               MOVE WS-ACTIVITE-NOM TO LK-ACTIVITE-NOM OF LK-ACTIVITE
               MOVE WS-ACTIVITE-TYPE TO LK-ACTIVITE-TYPE OF LK-ACTIVITE
               MOVE WS-ACTIVITE-IDANTENNE TO LK-ACTIVITE-IDANTENNE OF
               LK-ACTIVITE
               MOVE WS-ACTIVITE-ANIMATEUR TO LK-ACTIVITE-ANIMATEUR OF
               LK-ACTIVITE
               MOVE WS-ACTIVITE-NBPART TO LK-ACTIVITE-NBPART OF 
               LK-ACTIVITE
               MOVE WS-ACTIVITE-TRANSPORT TO LK-ACTIVITE-TRANSPORT OF
               LK-ACTIVITE
               MOVE WS-ACTIVITE-LIEU TO LK-ACTIVITE-LIEU OF LK-ACTIVITE
               MOVE WS-ACTIVITE-DISTANCE TO LK-ACTIVITE-DISTANCE OF 
               LK-ACTIVITE
               MOVE WS-ACTIVITE-HEBERG TO LK-ACTIVITE-HEBERG OF 
               LK-ACTIVITE
               MOVE WS-ACTIVITE-REPAS TO LK-ACTIVITE-REPAS OF 
               LK-ACTIVITE
               MOVE WS-ACTIVITE-EMPREINTE TO LK-ACTIVITE-EMPREINTE OF
               LK-ACTIVITE
               MOVE WS-ANTENNE-NOM TO LK-ANTENNE-NOM OF LK-ACTIVITE
               MOVE WS-ANTENNE-REGION TO LK-ANTENNE-REGION OF 
               LK-ACTIVITE
               MOVE WS-USER-NOM TO LK-USER-NOM OF LK-ACTIVITE
               MOVE WS-USER-MAIL TO LK-USER-MAIL OF LK-ACTIVITE
           END-IF.
       DAL-SAVE.
           MOVE LK-ACTIVITE-ID OF LK-ACTIVITE TO WS-ACTIVITE-ID
           MOVE LK-ACTIVITE-EMPREINTE OF LK-ACTIVITE TO 
           WS-ACTIVITE-EMPREINTE
           EXEC SQL
               UPDATE ACTIVITE
               SET ACTIVITE_EMPREINTETOTALE = :WS-ACTIVITE-EMPREINTE
               WHERE ACTIVITE_ID = :WS-ACTIVITE-ID
           END-EXEC
           IF SQLCODE NOT EQUAL ZERO
               DISPLAY 'ERREUR UPDATE: SQLCODE=' SQLCODE
               EXEC SQL ROLLBACK END-EXEC
               GOBACK
           END-IF.
       DAL-END.
           IF WS-CURSOR-OPEN-FLAG = 'Y'
               EXEC SQL CLOSE C_ACT END-EXEC
               IF SQLCODE NOT EQUAL ZERO
                   DISPLAY 'ERREUR CLOSE: SQLCODE=' SQLCODE
               END-IF
               MOVE 'N' TO WS-CURSOR-OPEN-FLAG
           END-IF
           IF WS-CONNECTED-FLAG = 'Y'
               EXEC SQL COMMIT END-EXEC
               IF SQLCODE NOT EQUAL ZERO
                   DISPLAY 'ERREUR COMMIT: SQLCODE=' SQLCODE
               END-IF
               DISPLAY 'Fermeture connexion'
               EXEC SQL DISCONNECT ALL END-EXEC
               IF SQLCODE NOT EQUAL ZERO
                   DISPLAY 'ERREUR DISCONNECT: SQLCODE=' SQLCODE
               END-IF
               MOVE 'N' TO WS-CONNECTED-FLAG
           END-IF
           GOBACK.
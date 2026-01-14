       IDENTIFICATION DIVISION.
       PROGRAM-ID. UTILISATEUR-DAL-DB.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       EXEC SQL INCLUDE SQLCA END-EXEC.
       01  WS-CONNECTED-FLAG      PIC X VALUE 'N'.
           88  WS-CONNECTED           VALUE 'Y'.
       01  WS-CURSOR-OPEN-FLAG    PIC X VALUE 'N'.
           88  WS-CURSOR-OPEN         VALUE 'Y'.
       01  WS-USER-ID             PIC 9(9).
       01  WS-USER-NOM            PIC X(50).
       01  WS-USER-MAIL           PIC X(80).
       01  WS-USER-PASS           PIC X(256).
       01  WS-USER-ROLE           PIC X(15).
       01  WS-USER-ID-ANTENNE     PIC 9(9).
       01  WS-USER-LAST-LOGIN     PIC S9(11).
       01  WS-DB-NAME.
           05  WS-DB-NAME-TEXT     PIC X(13) VALUE 'carbontrackdb'.
           05  WS-DB-NAME-TERM     PIC X VALUE X'00'.
       01  WS-DB-USER.
           05  WS-DB-USER-TEXT     PIC X(10) VALUE 'carbonuser'.
           05  WS-DB-USER-TERM     PIC X VALUE X'00'.
       01  WS-DB-PASSWORD.
           05  WS-DB-PASSWORD-TEXT PIC X(9) VALUE 'CARBONPWD'.
           05  WS-DB-PASSWORD-TERM PIC X VALUE X'00'.
       01  WS-DB-NAME-LEN         PIC S9(4) COMP-5 VALUE 13.
       01  WS-DB-USER-LEN         PIC S9(4) COMP-5 VALUE 10.
       01  WS-DB-PASSWORD-LEN     PIC S9(4) COMP-5 VALUE 9.
       01  WS-PGHOST-NAME         PIC X(6) VALUE 'PGHOST'.
       01  WS-PGHOST-VALUE        PIC X(64) VALUE 'localhost'.
       01  WS-PGPORT-NAME         PIC X(6) VALUE 'PGPORT'.
       01  WS-PGPORT-VALUE        PIC X(5) VALUE '5432'.
       01  WS-PGUSER-NAME         PIC X(6) VALUE 'PGUSER'.
       01  WS-PGUSER-VALUE        PIC X(64) VALUE 'carbonuser'.
       01  WS-PGPASSWORD-NAME     PIC X(10) VALUE 'PGPASSWORD'.
       01  WS-PGPASSWORD-VALUE    PIC X(64) VALUE 'CARBONPWD'.
       01  WS-PGDATABASE-NAME     PIC X(10) VALUE 'PGDATABASE'.
       01  WS-PGDATABASE-VALUE    PIC X(64) VALUE 'carbontrackdb'.
       LINKAGE SECTION.
       01 LK-OPERATION PIC X(4).
       01 LK-END-OF-FILE PIC X.
       01 LK-UTILISATEUR.
           05 LK-USER-ID PIC 9(9).
           05 LK-USER-NOM PIC X(50).
           05 LK-USER-MAIL PIC X(80).
           05 LK-USER-PASS PIC X(256).
           05 LK-USER-ROLE PIC X(15).
           05 LK-USER-ID-ANTENNE PIC 9(9).
           05 LK-USER-LAST-LOGIN PIC S9(11).
       PROCEDURE DIVISION USING LK-OPERATION LK-END-OF-FILE 
           LK-UTILISATEUR.
       MAIN-ENTRY.
           IF LK-OPERATION NOT = 'END '
               PERFORM DAL-CONNECT
               IF NOT WS-CONNECTED
                   MOVE 'Y' TO LK-END-OF-FILE
                   GOBACK
               END-IF
           END-IF
           EVALUATE LK-OPERATION
               WHEN 'READ'
                   PERFORM DAL-READ
               WHEN 'SAVE'
                   PERFORM DAL-SAVE
               WHEN 'END '
                   PERFORM DAL-END
               WHEN OTHER
                   DISPLAY 'ERREUR: Operation inconnue: ' LK-OPERATION
           END-EVALUATE
           GOBACK.
       
       DAL-SET-ENV.
           DISPLAY WS-PGHOST-NAME UPON ENVIRONMENT-NAME
           DISPLAY WS-PGHOST-VALUE UPON ENVIRONMENT-VALUE
           DISPLAY WS-PGPORT-NAME UPON ENVIRONMENT-NAME
           DISPLAY WS-PGPORT-VALUE UPON ENVIRONMENT-VALUE
           DISPLAY WS-PGUSER-NAME UPON ENVIRONMENT-NAME
           DISPLAY WS-PGUSER-VALUE UPON ENVIRONMENT-VALUE
           DISPLAY WS-PGPASSWORD-NAME UPON ENVIRONMENT-NAME
           DISPLAY WS-PGPASSWORD-VALUE UPON ENVIRONMENT-VALUE
           DISPLAY WS-PGDATABASE-NAME UPON ENVIRONMENT-NAME
           DISPLAY WS-PGDATABASE-VALUE UPON ENVIRONMENT-VALUE
           EXIT PARAGRAPH.
       
       DAL-CONNECT.
           PERFORM DAL-SET-ENV
           CALL "OCESQLStartSQL" END-CALL
           CALL "OCESQLConnect" USING
               BY REFERENCE SQLCA
               BY REFERENCE WS-DB-USER
               BY VALUE WS-DB-USER-LEN
               BY REFERENCE WS-DB-PASSWORD
               BY VALUE WS-DB-PASSWORD-LEN
               BY REFERENCE WS-DB-NAME
               BY VALUE WS-DB-NAME-LEN
           END-CALL
           CALL "OCESQLEndSQL" END-CALL
           IF SQLCODE = 0
               MOVE 'Y' TO WS-CONNECTED-FLAG
               DISPLAY 'carbontrackdb'
           ELSE
               DISPLAY 'ERREUR CONNECT: SQLCODE=' SQLCODE
               DISPLAY 'SQLSTATE=' SQLSTATE
               DISPLAY 'SQLERRMC=' SQLERRMC
           END-IF
           EXIT PARAGRAPH.
       
       DAL-READ.
           IF LK-OPERATION NOT EQUAL 'READ'
               EXIT PARAGRAPH
           END-IF
           IF NOT WS-CURSOR-OPEN
               EXEC SQL DECLARE C_USER CURSOR FOR
                   SELECT USER_ID, USER_NOM, USER_MAIL, USER_PASS, 
                   USER_ROLE,
                          USER_ID_ANTENNE, USER_LAST_LOGIN
                   FROM UTILISATEUR
                   ORDER BY USER_ID
               END-EXEC
               EXEC SQL OPEN C_USER END-EXEC
               IF SQLCODE NOT EQUAL ZERO
                   DISPLAY 'ERREUR OPEN: SQLCODE=' SQLCODE
                   MOVE 'Y' TO LK-END-OF-FILE
                   EXIT PARAGRAPH
               END-IF
               MOVE 'Y' TO WS-CURSOR-OPEN-FLAG
               DISPLAY 'Curseur C_USER ouvert'
           END-IF
           EXEC SQL FETCH C_USER INTO
               :WS-USER-ID,
               :WS-USER-NOM,
               :WS-USER-MAIL,
               :WS-USER-PASS,
               :WS-USER-ROLE,
               :WS-USER-ID-ANTENNE,
               :WS-USER-LAST-LOGIN
           END-EXEC
           EVALUATE SQLCODE
               WHEN ZERO
                   MOVE WS-USER-ID TO LK-USER-ID OF LK-UTILISATEUR
                   MOVE WS-USER-NOM TO LK-USER-NOM OF LK-UTILISATEUR
                   MOVE WS-USER-MAIL TO LK-USER-MAIL OF LK-UTILISATEUR
                   MOVE WS-USER-PASS TO LK-USER-PASS OF LK-UTILISATEUR
                   MOVE WS-USER-ROLE TO LK-USER-ROLE OF LK-UTILISATEUR
                   MOVE WS-USER-ID-ANTENNE TO LK-USER-ID-ANTENNE OF
                   LK-UTILISATEUR
                   MOVE WS-USER-LAST-LOGIN TO LK-USER-LAST-LOGIN OF
                   LK-UTILISATEUR
                   MOVE 'N' TO LK-END-OF-FILE
               WHEN 100
                   MOVE 'Y' TO LK-END-OF-FILE
               WHEN OTHER
                   DISPLAY 'ERREUR FETCH: SQLCODE=' SQLCODE
                   MOVE 'Y' TO LK-END-OF-FILE
           END-EVALUATE
           EXIT PARAGRAPH.
       
       DAL-SAVE.
           MOVE LK-USER-ID OF LK-UTILISATEUR TO WS-USER-ID
           MOVE LK-USER-LAST-LOGIN OF LK-UTILISATEUR TO 
           WS-USER-LAST-LOGIN
           EXEC SQL
               UPDATE UTILISATEUR
               SET USER_LAST_LOGIN = :WS-USER-LAST-LOGIN
               WHERE USER_ID = :WS-USER-ID
           END-EXEC
           EVALUATE SQLCODE
               WHEN 0
                   CONTINUE
               WHEN OTHER
                   DISPLAY 'ERREUR UPDATE: SQLCODE=' SQLCODE
                   DISPLAY 'SQLSTATE=' SQLSTATE
                   DISPLAY 'SQLERRMC=' SQLERRMC
                   EXEC SQL ROLLBACK END-EXEC
           END-EVALUATE
           EXIT PARAGRAPH.
       
       DAL-END.
           IF WS-CURSOR-OPEN
               EXEC SQL CLOSE C_USER END-EXEC
               MOVE 'N' TO WS-CURSOR-OPEN-FLAG
               IF SQLCODE NOT = 0
                   DISPLAY 'ERREUR CLOSE: SQLCODE=' SQLCODE
               END-IF
           END-IF
           EXEC SQL COMMIT END-EXEC
           IF SQLCODE NOT = 0
               DISPLAY 'ERREUR COMMIT: SQLCODE=' SQLCODE
           END-IF
           IF WS-CONNECTED
               EXEC SQL DISCONNECT ALL END-EXEC
               MOVE 'N' TO WS-CONNECTED-FLAG
               IF SQLCODE NOT = 0
                   DISPLAY 'ERREUR DISCONNECT: SQLCODE=' SQLCODE
               END-IF
               DISPLAY 'Fermeture connexion'
           END-IF
           EXIT PARAGRAPH.
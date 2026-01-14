       IDENTIFICATION DIVISION.
       PROGRAM-ID. HEBERGEMENT-DAL-DB.

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
       01  WS-HEBERGEMENT-ID      PIC 9(9).
       01  WS-HEBERG-ID-ACTIVITE  PIC 9(9).
       01  WS-HEBERGEMENT-TYPE    PIC 9(2).
       01  WS-HEBERGEMENT-NBNUIT  PIC S9(3).
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
       01 LK-HEBERGEMENT.
           05 LK-HEBERGEMENT-ID PIC 9(9).
           05 LK-HEBERGEMENT-ID-ACTIVITE PIC 9(9).
           05 LK-HEBERGEMENT-TYPE PIC 9(2).
           05 LK-HEBERGEMENT-NBNUIT PIC S9(3).
       
       PROCEDURE DIVISION USING LK-OPERATION LK-END-OF-FILE 
           LK-HEBERGEMENT.
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
           DISPLAY WS-PGDATABASE-VALUE UPON ENVIRONMENT-VALUE.
       
       DAL-CONNECT.
           IF NOT WS-CONNECTED
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
                   SET WS-CONNECTED TO TRUE
                   DISPLAY 'carbontrackdb'
               ELSE
                   DISPLAY 'ERREUR CONNECT: SQLCODE=' SQLCODE
                   DISPLAY 'SQLSTATE=' SQLSTATE
                   DISPLAY 'SQLERRMC=' SQLERRMC
               END-IF
           END-IF.
       
       DAL-READ.
           IF NOT WS-CURSOR-OPEN
               EXEC SQL DECLARE C_HEB CURSOR FOR
                   SELECT HEBERGEMENT_ID, HEBERGEMENT_ID_ACTIVITE,
                          HEBERGEMENT_TYPE, HEBERGEMENT_NBNUIT
                   FROM HEBERGEMENT
                   ORDER BY HEBERGEMENT_ID
               END-EXEC
               EXEC SQL OPEN C_HEB END-EXEC
               IF SQLCODE NOT EQUAL ZERO
                   DISPLAY 'ERREUR OPEN: SQLCODE=' SQLCODE
                   MOVE 'Y' TO LK-END-OF-FILE
                   EXEC SQL ROLLBACK END-EXEC
                   EXIT PARAGRAPH
               END-IF
               SET WS-CURSOR-OPEN TO TRUE
               DISPLAY 'Curseur C_HEB ouvert'
           END-IF
           EXEC SQL FETCH C_HEB INTO
               :WS-HEBERGEMENT-ID,
               :WS-HEBERG-ID-ACTIVITE,
               :WS-HEBERGEMENT-TYPE,
               :WS-HEBERGEMENT-NBNUIT
           END-EXEC
           EVALUATE SQLCODE
               WHEN 0
                   MOVE WS-HEBERGEMENT-ID TO LK-HEBERGEMENT-ID OF
                   LK-HEBERGEMENT
                   MOVE WS-HEBERG-ID-ACTIVITE TO 
                   LK-HEBERGEMENT-ID-ACTIVITE OF
                   LK-HEBERGEMENT
                   MOVE WS-HEBERGEMENT-TYPE TO LK-HEBERGEMENT-TYPE OF
                   LK-HEBERGEMENT
                   MOVE WS-HEBERGEMENT-NBNUIT TO LK-HEBERGEMENT-NBNUIT 
                   OF
                   LK-HEBERGEMENT
               WHEN 100
                   MOVE 'Y' TO LK-END-OF-FILE
                   EXEC SQL CLOSE C_HEB END-EXEC
                   MOVE 'N' TO WS-CURSOR-OPEN-FLAG
               WHEN OTHER
                   DISPLAY 'ERREUR FETCH: SQLCODE=' SQLCODE
                   MOVE 'Y' TO LK-END-OF-FILE
                   EXEC SQL ROLLBACK END-EXEC
           END-EVALUATE.
       
       DAL-SAVE.
           MOVE LK-HEBERGEMENT-ID OF LK-HEBERGEMENT TO WS-HEBERGEMENT-ID
           MOVE LK-HEBERGEMENT-NBNUIT OF LK-HEBERGEMENT TO
           WS-HEBERGEMENT-NBNUIT
           EXEC SQL UPDATE HEBERGEMENT
               SET HEBERGEMENT_NBNUIT = :WS-HEBERGEMENT-NBNUIT
               WHERE HEBERGEMENT_ID = :WS-HEBERGEMENT-ID
           END-EXEC
           IF SQLCODE NOT = 0
               DISPLAY 'ERREUR UPDATE: SQLCODE=' SQLCODE
               EXEC SQL ROLLBACK END-EXEC
               MOVE 'Y' TO LK-END-OF-FILE
           END-IF.
       
       DAL-END.
           IF WS-CURSOR-OPEN
               EXEC SQL CLOSE C_HEB END-EXEC
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
           END-IF.
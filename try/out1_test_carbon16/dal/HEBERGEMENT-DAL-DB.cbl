       IDENTIFICATION DIVISION.
       PROGRAM-ID. HEBERGEMENT-DAL-DB.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           EXEC SQL INCLUDE SQLCA END-EXEC.
       01  WS-CONNECTED-FLAG      PIC X VALUE 'N'.
       01  WS-CURSOR-OPEN-FLAG    PIC X VALUE 'N'.
       01  WS-HEBERGEMENT-ID      PIC 9(9).
       01  WS-HEBERG-ID-ACTIVITE  PIC 9(9).
       01  WS-HEBERGEMENT-TYPE    PIC 9(2).
       01  WS-HEBERGEMENT-NBNUIT  PIC S9(3).
       01  WS-DB-NAME             PIC X(64).
       01  WS-DB-USER             PIC X(64).
       01  WS-DB-PASSWORD         PIC X(64).
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
           ACCEPT WS-DB-USER FROM ENVIRONMENT 'PGUSER'
           ACCEPT WS-DB-PASSWORD FROM ENVIRONMENT 'PGPASSWORD'
           ACCEPT WS-DB-NAME FROM ENVIRONMENT 'PGDATABASE'.
       
       DAL-CONNECT.
           IF WS-CONNECTED-FLAG = 'Y'
               CONTINUE
           ELSE
               PERFORM DAL-SET-ENV
               EXEC SQL
                   CONNECT :WS-DB-USER IDENTIFIED BY :WS-DB-PASSWORD
                       USING :WS-DB-NAME
               END-EXEC
               IF SQLCODE < 0
                   DISPLAY 'ERREUR CONNECT: SQLCODE=' SQLCODE
                   DISPLAY 'SQLSTATE=' SQLSTATE
                   DISPLAY 'SQLERRMC=' SQLERRMC
                   STOP RUN
               ELSE
                   DISPLAY 'Connexion DB reussie'
                   MOVE 'Y' TO WS-CONNECTED-FLAG
               END-IF
           END-IF.
       
       DAL-READ.
           PERFORM DAL-CONNECT
           IF WS-CURSOR-OPEN-FLAG NOT EQUAL 'Y'
               EXEC SQL
                   DECLARE C_HEB CURSOR FOR
                   SELECT HEBERGEMENT_ID, HEBERGEMENT_ID_ACTIVITE,
                          HEBERGEMENT_TYPE, HEBERGEMENT_NBNUIT
                   FROM HEBERGEMENT
                   ORDER BY HEBERGEMENT_ID
               END-EXEC
               EXEC SQL
                   OPEN C_HEB
               END-EXEC
               IF SQLCODE NOT EQUAL ZERO
                   DISPLAY 'ERREUR OPEN: SQLCODE=' SQLCODE
                   STOP RUN
               ELSE
                   DISPLAY 'Curseur C_HEB ouvert'
                   MOVE 'Y' TO WS-CURSOR-OPEN-FLAG
               END-IF
           END-IF
           MOVE 'N' TO LK-END-OF-FILE
           EXEC SQL
               FETCH C_HEB INTO
                   :WS-HEBERGEMENT-ID,
                   :WS-HEBERG-ID-ACTIVITE,
                   :WS-HEBERGEMENT-TYPE,
                   :WS-HEBERGEMENT-NBNUIT
           END-EXEC
           EVALUATE SQLCODE
               WHEN 0
                   MOVE WS-HEBERGEMENT-ID TO LK-HEBERGEMENT-ID
                   MOVE WS-HEBERG-ID-ACTIVITE TO 
                   LK-HEBERGEMENT-ID-ACTIVITE
                   MOVE WS-HEBERGEMENT-TYPE TO LK-HEBERGEMENT-TYPE
                   MOVE WS-HEBERGEMENT-NBNUIT TO LK-HEBERGEMENT-NBNUIT
               WHEN 100
                   MOVE 'Y' TO LK-END-OF-FILE
               WHEN OTHER
                   DISPLAY 'ERREUR FETCH: SQLCODE=' SQLCODE
                   MOVE 'Y' TO LK-END-OF-FILE
           END-EVALUATE.
       
       DAL-SAVE.
           PERFORM DAL-CONNECT
           MOVE LK-HEBERGEMENT-ID OF LK-HEBERGEMENT
               TO WS-HEBERGEMENT-ID
           MOVE LK-HEBERGEMENT-ID-ACTIVITE OF LK-HEBERGEMENT
               TO WS-HEBERG-ID-ACTIVITE
           MOVE LK-HEBERGEMENT-TYPE OF LK-HEBERGEMENT
               TO WS-HEBERGEMENT-TYPE
           MOVE LK-HEBERGEMENT-NBNUIT OF LK-HEBERGEMENT
               TO WS-HEBERGEMENT-NBNUIT
           EXEC SQL
               UPDATE HEBERGEMENT
               SET HEBERGEMENT_ID_ACTIVITE = :WS-HEBERG-ID-ACTIVITE,
                   HEBERGEMENT_TYPE = :WS-HEBERGEMENT-TYPE,
                   HEBERGEMENT_NBNUIT = :WS-HEBERGEMENT-NBNUIT
               WHERE HEBERGEMENT_ID = :WS-HEBERGEMENT-ID
           END-EXEC
           IF SQLCODE NOT EQUAL ZERO
               DISPLAY 'ERREUR UPDATE: SQLCODE=' SQLCODE
           END-IF.
       
       DAL-END.
           IF WS-CURSOR-OPEN-FLAG = 'Y'
               EXEC SQL CLOSE C_HEB END-EXEC
               IF SQLCODE NOT EQUAL ZERO
                   DISPLAY 'ERREUR CLOSE: SQLCODE=' SQLCODE
               END-IF
               MOVE 'N' TO WS-CURSOR-OPEN-FLAG
           END-IF
           IF WS-CONNECTED-FLAG = 'Y'
               DISPLAY 'Fermeture connexion'
               EXEC SQL COMMIT END-EXEC
               IF SQLCODE NOT EQUAL ZERO
                   DISPLAY 'ERREUR COMMIT: SQLCODE=' SQLCODE
               END-IF
               EXEC SQL DISCONNECT ALL END-EXEC
               IF SQLCODE NOT EQUAL ZERO
                   DISPLAY 'ERREUR DISCONNECT: SQLCODE=' SQLCODE
               END-IF
               MOVE 'N' TO WS-CONNECTED-FLAG
           END-IF.
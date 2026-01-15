       IDENTIFICATION DIVISION.
       PROGRAM-ID. ANTENNE-DAL-DB.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
              EXEC SQL INCLUDE SQLCA END-EXEC.
       01  WS-CONNECTED-FLAG      PIC X VALUE 'N'.
       01  WS-CURSOR-OPEN-FLAG    PIC X VALUE 'N'.
       01  WS-ANTENNE-ID          PIC 9(9).
       01  WS-ANTENNE-NOM         PIC X(50).
       01  WS-ANTENNE-REGION      PIC X(50).
       01  WS-DB-NAME             PIC X(64).
       01  WS-DB-USER             PIC X(64).
       01  WS-DB-PASSWORD         PIC X(64).
       LINKAGE SECTION.
       01 LK-OPERATION PIC X(4).
       01 LK-END-OF-FILE PIC X.
       01 LK-ANTENNE.
           05 LK-ANTENNE-ID PIC 9(9).
           05 LK-ANTENNE-NOM PIC X(50).
           05 LK-ANTENNE-REGION PIC X(50).
       PROCEDURE DIVISION USING LK-OPERATION LK-END-OF-FILE LK-ANTENNE.
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
           GOBACK
           .
       DAL-CONNECT.
           IF WS-CONNECTED-FLAG = 'Y'
               CONTINUE
           ELSE
               PERFORM DAL-SET-ENV
               EXEC SQL
                   CONNECT :WS-DB-USER IDENTIFIED BY :WS-DB-PASSWORD
                       USING :WS-DB-NAME
               END-EXEC
               IF SQLCODE EQUAL ZERO
                   MOVE 'Y' TO WS-CONNECTED-FLAG
                   DISPLAY 'Connexion DB reussie'
               ELSE
                   DISPLAY 'ERREUR CONNECT: SQLCODE=' SQLCODE
                   DISPLAY 'SQLSTATE=' SQLSTATE
                   DISPLAY 'SQLERRMC=' SQLERRMC
                   MOVE 'N' TO WS-CONNECTED-FLAG
                   GOBACK
               END-IF
           END-IF
           .
       DAL-SET-ENV.
           ACCEPT WS-DB-USER FROM ENVIRONMENT 'PGUSER'
           ACCEPT WS-DB-PASSWORD FROM ENVIRONMENT 'PGPASSWORD'
           ACCEPT WS-DB-NAME FROM ENVIRONMENT 'PGDATABASE'
           .
       DAL-END.
           IF WS-CURSOR-OPEN-FLAG EQUAL 'Y'
               EXEC SQL
                   CLOSE C_ANT
               END-EXEC
               IF SQLCODE NOT EQUAL ZERO
                   DISPLAY 'ERREUR CLOSE: SQLCODE=' SQLCODE
               END-IF
           END-IF
           EXEC SQL COMMIT END-EXEC
           IF SQLCODE NOT EQUAL ZERO
               DISPLAY 'ERREUR COMMIT: SQLCODE=' SQLCODE
           END-IF
           EXEC SQL DISCONNECT ALL END-EXEC
           IF SQLCODE NOT EQUAL ZERO
               DISPLAY 'ERREUR DISCONNECT: SQLCODE=' SQLCODE
           END-IF
           DISPLAY 'Fermeture connexion'
           MOVE 'N' TO WS-CURSOR-OPEN-FLAG
           MOVE 'N' TO WS-CONNECTED-FLAG
           .
       DAL-READ.
           PERFORM DAL-CONNECT
           IF WS-CURSOR-OPEN-FLAG NOT EQUAL 'Y'
               EXEC SQL
                   DECLARE C_ANT CURSOR FOR
                   SELECT ANTENNE_ID, ANTENNE_NOM, ANTENNE_REGION
                   FROM ANTENNE
                   ORDER BY ANTENNE_ID
               END-EXEC
               EXEC SQL
                   OPEN C_ANT
               END-EXEC
               IF SQLCODE EQUAL ZERO
                   MOVE 'Y' TO WS-CURSOR-OPEN-FLAG
                   DISPLAY 'Curseur C_ANT ouvert'
               ELSE
                   DISPLAY 'ERREUR OPEN: SQLCODE=' SQLCODE
                   GOBACK
               END-IF
           END-IF
           MOVE 'N' TO LK-END-OF-FILE
           EXEC SQL
               FETCH C_ANT INTO
                   :WS-ANTENNE-ID,
                   :WS-ANTENNE-NOM,
                   :WS-ANTENNE-REGION
           END-EXEC
           EVALUATE SQLCODE
               WHEN 0
                   MOVE WS-ANTENNE-ID TO LK-ANTENNE-ID OF LK-ANTENNE
                   MOVE WS-ANTENNE-NOM TO LK-ANTENNE-NOM OF LK-ANTENNE
                   MOVE WS-ANTENNE-REGION TO LK-ANTENNE-REGION OF 
                   LK-ANTENNE
               WHEN 100
                   MOVE 'Y' TO LK-END-OF-FILE
               WHEN OTHER
                   DISPLAY 'ERREUR FETCH: SQLCODE=' SQLCODE
                   MOVE 'Y' TO LK-END-OF-FILE
           END-EVALUATE
           .
       DAL-SAVE.
           PERFORM DAL-CONNECT
           MOVE LK-ANTENNE-ID OF LK-ANTENNE TO WS-ANTENNE-ID
           MOVE LK-ANTENNE-NOM OF LK-ANTENNE TO WS-ANTENNE-NOM
           MOVE LK-ANTENNE-REGION OF LK-ANTENNE TO WS-ANTENNE-REGION
           EXEC SQL
               UPDATE ANTENNE
               SET ANTENNE_NOM = :WS-ANTENNE-NOM,
                   ANTENNE_REGION = :WS-ANTENNE-REGION
               WHERE ANTENNE_ID = :WS-ANTENNE-ID
           END-EXEC
           IF SQLCODE NOT EQUAL ZERO
               DISPLAY 'ERREUR UPDATE: SQLCODE=' SQLCODE
           END-IF
           .
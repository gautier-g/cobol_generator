       IDENTIFICATION DIVISION.
       PROGRAM-ID. REPAS-DAL-DB.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           EXEC SQL INCLUDE SQLCA END-EXEC.
       01  WS-CONNECTED-FLAG      PIC X VALUE 'N'.
       01  WS-CURSOR-OPEN-FLAG    PIC X VALUE 'N'.
       01  WS-REPAS-ID            PIC 9(9).
       01  WS-REPAS-ID-ACTIVITE   PIC 9(9).
       01  WS-REPAS-TYPE          PIC 9(2).
       01  WS-REPAS-NBREPAS       PIC S9(5).
       01  WS-DB-NAME             PIC X(64).
       01  WS-DB-USER             PIC X(64).
       01  WS-DB-PASSWORD         PIC X(64).
       
       
       LINKAGE SECTION.
       01 LK-OPERATION            PIC X(4).
       01 LK-END-OF-FILE          PIC X.
       01 LK-REPAS.
           05 LK-REPAS-ID PIC 9(9).
           05 LK-REPAS-ID-ACTIVITE PIC 9(9).
           05 LK-REPAS-TYPE PIC 9(2).
           05 LK-REPAS-NBREPAS PIC S9(5).
       
       PROCEDURE DIVISION USING LK-OPERATION LK-END-OF-FILE LK-REPAS.
       MAIN-ENTRY.
           EVALUATE LK-OPERATION
               WHEN 'END '
                   PERFORM DAL-END
               WHEN 'READ'
                   PERFORM DAL-READ
               WHEN 'SAVE'
                   PERFORM DAL-SAVE
               WHEN OTHER
                   DISPLAY 'ERREUR: Operation inconnue: ' LK-OPERATION
           END-EVALUATE
           GOBACK
           .
       
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
               IF SQLCODE < ZERO
                   DISPLAY 'ERREUR CONNECT: SQLCODE=' SQLCODE
                   DISPLAY 'SQLSTATE=' SQLSTATE
                   DISPLAY 'SQLERRMC=' SQLERRMC
                   GOBACK
               ELSE
                   DISPLAY 'Connexion DB reussie'
                   MOVE 'Y' TO WS-CONNECTED-FLAG
               END-IF
           END-IF
           .
       
       DAL-READ.
           PERFORM DAL-CONNECT
           IF WS-CURSOR-OPEN-FLAG NOT EQUAL 'Y'
               EXEC SQL
                   DECLARE C_REP CURSOR FOR
                   SELECT REPAS_ID, REPAS_ID_ACTIVITE, REPAS_TYPE,
                          REPAS_NBREPAS
                   FROM REPAS
                   ORDER BY REPAS_ID
               END-EXEC
               EXEC SQL OPEN C_REP END-EXEC
               IF SQLCODE NOT EQUAL ZERO
                   DISPLAY 'ERREUR OPEN: SQLCODE=' SQLCODE
                   GOBACK
               ELSE
                   DISPLAY 'Curseur C_REP ouvert'
                   MOVE 'Y' TO WS-CURSOR-OPEN-FLAG
               END-IF
           END-IF
       
           MOVE 'N' TO LK-END-OF-FILE
           EXEC SQL
               FETCH C_REP INTO
                   :WS-REPAS-ID,
                   :WS-REPAS-ID-ACTIVITE,
                   :WS-REPAS-TYPE,
                   :WS-REPAS-NBREPAS
           END-EXEC
       
           EVALUATE SQLCODE
               WHEN 0
                   MOVE WS-REPAS-ID TO LK-REPAS-ID OF LK-REPAS
                   MOVE WS-REPAS-ID-ACTIVITE TO
                       LK-REPAS-ID-ACTIVITE OF LK-REPAS
                   MOVE WS-REPAS-TYPE TO LK-REPAS-TYPE OF LK-REPAS
                   MOVE WS-REPAS-NBREPAS TO LK-REPAS-NBREPAS OF LK-REPAS
               WHEN 100
                   MOVE 'Y' TO LK-END-OF-FILE
               WHEN OTHER
                   DISPLAY 'ERREUR FETCH: SQLCODE=' SQLCODE
                   MOVE 'Y' TO LK-END-OF-FILE
           END-EVALUATE
           .
       
       DAL-SAVE.
           PERFORM DAL-CONNECT
           MOVE LK-REPAS-ID OF LK-REPAS TO WS-REPAS-ID
           MOVE LK-REPAS-ID-ACTIVITE OF LK-REPAS TO WS-REPAS-ID-ACTIVITE
           MOVE LK-REPAS-TYPE OF LK-REPAS TO WS-REPAS-TYPE
           MOVE LK-REPAS-NBREPAS OF LK-REPAS TO WS-REPAS-NBREPAS
       
           EXEC SQL
               UPDATE REPAS
               SET REPAS_ID_ACTIVITE = :WS-REPAS-ID-ACTIVITE,
                   REPAS_TYPE = :WS-REPAS-TYPE,
                   REPAS_NBREPAS = :WS-REPAS-NBREPAS
               WHERE REPAS_ID = :WS-REPAS-ID
           END-EXEC
       
           IF SQLCODE NOT EQUAL ZERO
               DISPLAY 'ERREUR UPDATE: SQLCODE=' SQLCODE
           END-IF
           .
       
       DAL-END.
           IF WS-CURSOR-OPEN-FLAG = 'Y'
               EXEC SQL CLOSE C_REP END-EXEC
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
               EXEC SQL DISCONNECT ALL END-EXEC
               IF SQLCODE NOT EQUAL ZERO
                   DISPLAY 'ERREUR DISCONNECT: SQLCODE=' SQLCODE
               END-IF
               DISPLAY 'Fermeture connexion'
               MOVE 'N' TO WS-CONNECTED-FLAG
           END-IF
           .
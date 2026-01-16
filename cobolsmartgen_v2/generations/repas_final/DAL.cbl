       IDENTIFICATION DIVISION.
       PROGRAM-ID. GESTIONACTIVITE.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-CONNECTED           PIC X VALUE 'N'.
       01 WS-CURSOR-OPEN         PIC X VALUE 'N'.
       01 WS-SQLCODE             PIC S9(9) COMP-5.

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01 DBNAME                 PIC X(30) VALUE 'postgres'.
       01 USERNAME               PIC X(30) VALUE 'postgres'.
       01 PASSWD                 PIC X(30) VALUE 'postgres'.
       01 WS-ACTIVITE.
           05 WS-ACTIVITE-ID             PIC 9(9).
           05 WS-ACTIVITE-NOM            PIC X(50).
           05 WS-ACTIVITE-TYPE           PIC X(20).
           05 WS-ACTIVITE-IDANTENNE      PIC 9(9).
           05 WS-ACTIVITE-ANIMATEUR      PIC 9(9).
           05 WS-ACTIVITE-NBPARTICIPANTS PIC 9(9).
           05 WS-ACTIVITE-MODETRANSPORT  PIC 9(9).
           05 WS-ACTIVITE-LIEU           PIC X(100).
           05 WS-ACTIVITE-DISTANCE       PIC 9(9).
           05 WS-ACTIVITE-HEBERGEMENT    PIC 9(9).
           05 WS-ACTIVITE-REPASPREVU     PIC 9(9).
           05 WS-ACTIVITE-EMPREINTETOTALE PIC 9(13)V9(4).
       01 WS-REPAS.
           05 WS-REPAS-ID                PIC 9(9).
           05 WS-REPAS-ID-ACTIVITE       PIC 9(9).
           05 WS-REPAS-TYPE              PIC 9(9).
           05 WS-REPAS-NBREPAS           PIC 9(9).
       EXEC SQL END DECLARE SECTION END-EXEC.

       EXEC SQL INCLUDE SQLCA END-EXEC.

       LINKAGE SECTION.
       01 LK-OPERATION           PIC X(10).
       01 LK-END-OF-FILE         PIC X.
       01 LK-ACTIVITE.
           05 LK-ACTIVITE-ID             PIC 9(9).
           05 LK-ACTIVITE-NOM            PIC X(50).
           05 LK-ACTIVITE-TYPE           PIC X(20).
           05 LK-ACTIVITE-IDANTENNE      PIC 9(9).
           05 LK-ACTIVITE-ANIMATEUR      PIC 9(9).
           05 LK-ACTIVITE-NBPARTICIPANTS PIC 9(9).
           05 LK-ACTIVITE-MODETRANSPORT  PIC 9(9).
           05 LK-ACTIVITE-LIEU           PIC X(100).
           05 LK-ACTIVITE-DISTANCE       PIC 9(9).
           05 LK-ACTIVITE-HEBERGEMENT    PIC 9(9).
           05 LK-ACTIVITE-REPASPREVU     PIC 9(9).
           05 LK-ACTIVITE-EMPREINTETOTALE PIC 9(13)V9(4).
       01 LK-REPAS.
           05 LK-REPAS-ID                PIC 9(9).
           05 LK-REPAS-ID-ACTIVITE       PIC 9(9).
           05 LK-REPAS-TYPE              PIC 9(9).
           05 LK-REPAS-NBREPAS           PIC 9(9).

       PROCEDURE DIVISION USING LK-OPERATION LK-END-OF-FILE
           LK-ACTIVITE LK-REPAS.
       MAIN-LOGIC.
           EVALUATE LK-OPERATION
               WHEN 'CREATEACT'
                   PERFORM CREATEACTIVITE
               WHEN 'CREATEREP'
                   PERFORM CREATEREPAS
               WHEN 'READREP'
                   PERFORM READREPAS
               WHEN 'LISTREP'
                   PERFORM LISTREPAS
               WHEN 'UPDATEREP'
                   PERFORM UPDATEREPAS
               WHEN 'DELETEREP'
                   PERFORM DELETEREPAS
               WHEN 'END'
                   PERFORM ENDPROGRAM
           END-EVALUATE.
           EXIT PROGRAM.

       CONNECTDB.
           IF WS-CONNECTED = 'N'
               EXEC SQL
                   CONNECT :USERNAME IDENTIFIED BY :PASSWD USING :DBNAME
               END-EXEC
               EXEC SQL
                   SET client_encoding TO 'LATIN1'
               END-EXEC
               IF SQLCODE = 0
                   MOVE 'Y' TO WS-CONNECTED
               ELSE
                   MOVE 'Y' TO LK-END-OF-FILE
               END-IF
           END-IF.

       CREATEACTIVITE.
           PERFORM CONNECTDB.
           IF LK-END-OF-FILE = 'Y' EXIT PARAGRAPH END-IF.

           MOVE LK-ACTIVITE TO WS-ACTIVITE.

           EXEC SQL
               INSERT INTO ACTIVITE
               (ACTIVITE_ID, ACTIVITE_NOM, ACTIVITE_TYPE,
                ACTIVITE_IDANTENNE, ACTIVITE_ANIMATEUR,
                ACTIVITE_NBPARTICIPANTS, ACTIVITE_MODETRANSPORT,
                ACTIVITE_LIEU, ACTIVITE_DISTANCE,
                ACTIVITE_HEBERGEMENT, ACTIVITE_REPASPREVU,
                ACTIVITE_EMPREINTETOTALE)
               VALUES
               (:WS-ACTIVITE-ID, :WS-ACTIVITE-NOM, :WS-ACTIVITE-TYPE,
                :WS-ACTIVITE-IDANTENNE, :WS-ACTIVITE-ANIMATEUR,
                :WS-ACTIVITE-NBPARTICIPANTS, :WS-ACTIVITE-MODETRANSPORT,
                :WS-ACTIVITE-LIEU, :WS-ACTIVITE-DISTANCE,
                :WS-ACTIVITE-HEBERGEMENT, :WS-ACTIVITE-REPASPREVU,
                :WS-ACTIVITE-EMPREINTETOTALE)
           END-EXEC.

           IF SQLCODE NOT = 0
               MOVE 'Y' TO LK-END-OF-FILE
           END-IF.

       CREATEREPAS.
           PERFORM CONNECTDB.
           IF LK-END-OF-FILE = 'Y' EXIT PARAGRAPH END-IF.

           MOVE LK-REPAS TO WS-REPAS.

           EXEC SQL
               INSERT INTO REPAS
               (REPAS_ID, REPAS_ID_ACTIVITE, REPAS_TYPE, REPAS_NBREPAS)
               VALUES
               (:WS-REPAS-ID, :WS-REPAS-ID-ACTIVITE,
                :WS-REPAS-TYPE, :WS-REPAS-NBREPAS)
           END-EXEC.

           IF SQLCODE NOT = 0
               MOVE 'Y' TO LK-END-OF-FILE
           END-IF.

       READREPAS.
           PERFORM CONNECTDB.
           IF LK-END-OF-FILE = 'Y' EXIT PARAGRAPH END-IF.

           MOVE LK-REPAS-ID TO WS-REPAS-ID.

           EXEC SQL
               SELECT REPAS_ID, REPAS_ID_ACTIVITE, REPAS_TYPE,
                      REPAS_NBREPAS
               INTO :WS-REPAS-ID, :WS-REPAS-ID-ACTIVITE,
                    :WS-REPAS-TYPE, :WS-REPAS-NBREPAS
               FROM REPAS
               WHERE REPAS_ID = :WS-REPAS-ID
           END-EXEC.

           IF SQLCODE = 0
               MOVE WS-REPAS TO LK-REPAS
           ELSE
               MOVE 'Y' TO LK-END-OF-FILE
           END-IF.

       LISTREPAS.
           PERFORM CONNECTDB.
           IF LK-END-OF-FILE = 'Y' EXIT PARAGRAPH END-IF.

           MOVE LK-REPAS-ID-ACTIVITE TO WS-REPAS-ID-ACTIVITE.

           IF WS-CURSOR-OPEN = 'N'
               EXEC SQL
                   DECLARE CREP CURSOR FOR
                   SELECT REPAS_ID, REPAS_ID_ACTIVITE, REPAS_TYPE,
                          REPAS_NBREPAS
                   FROM REPAS
                   WHERE REPAS_ID_ACTIVITE = :WS-REPAS-ID-ACTIVITE
               END-EXEC
               EXEC SQL
                   OPEN CREP
               END-EXEC
               IF SQLCODE = 0
                   MOVE 'Y' TO WS-CURSOR-OPEN
               ELSE
                   MOVE 'Y' TO LK-END-OF-FILE
                   EXIT PARAGRAPH
               END-IF
           END-IF.

           EXEC SQL
               FETCH CREP INTO
                   :WS-REPAS-ID, :WS-REPAS-ID-ACTIVITE,
                   :WS-REPAS-TYPE, :WS-REPAS-NBREPAS
           END-EXEC.

           IF SQLCODE = 0
               MOVE WS-REPAS TO LK-REPAS
           ELSE
               MOVE 'Y' TO LK-END-OF-FILE
           END-IF.

       UPDATEREPAS.
           PERFORM CONNECTDB.
           IF LK-END-OF-FILE = 'Y' EXIT PARAGRAPH END-IF.

           MOVE LK-REPAS TO WS-REPAS.

           EXEC SQL
               UPDATE REPAS
               SET REPAS_TYPE = :WS-REPAS-TYPE,
                   REPAS_NBREPAS = :WS-REPAS-NBREPAS
               WHERE REPAS_ID = :WS-REPAS-ID
           END-EXEC.

           IF SQLCODE NOT = 0
               MOVE 'Y' TO LK-END-OF-FILE
           END-IF.

       DELETEREPAS.
           PERFORM CONNECTDB.
           IF LK-END-OF-FILE = 'Y' EXIT PARAGRAPH END-IF.

           MOVE LK-REPAS-ID TO WS-REPAS-ID.

           EXEC SQL
               DELETE FROM REPAS
               WHERE REPAS_ID = :WS-REPAS-ID
           END-EXEC.

           IF SQLCODE NOT = 0
               MOVE 'Y' TO LK-END-OF-FILE
           END-IF.

       ENDPROGRAM.
           IF WS-CURSOR-OPEN = 'Y'
               EXEC SQL
                   CLOSE CREP
               END-EXEC
               MOVE 'N' TO WS-CURSOR-OPEN
           END-IF.

           EXEC SQL
               COMMIT
           END-EXEC.

           IF WS-CONNECTED = 'Y'
               EXEC SQL
                   DISCONNECT ALL
               END-EXEC
               MOVE 'N' TO WS-CONNECTED
           END-IF.
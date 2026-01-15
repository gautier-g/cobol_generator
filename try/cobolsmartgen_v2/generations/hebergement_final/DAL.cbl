       IDENTIFICATION DIVISION.
       PROGRAM-ID. HEBERGEMENT-DAL-DB.
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
       01 WS-HEBERGEMENT.
           05 WS-HEB-ID          PIC 9(9).
           05 WS-HEB-ID-ACT      PIC 9(9).
           05 WS-HEB-TYPE        PIC 9(4).
           05 WS-HEB-NBNUIT      PIC 9(4).
       01 WS-ACTIVITE.
           05 WS-ACT-ID          PIC 9(9).
           05 WS-ACT-NOM         PIC X(50).
           05 WS-ACT-TYPE        PIC X(20).
           05 WS-ACT-IDANTENNE   PIC 9(9).
           05 WS-ACT-ANIMATEUR   PIC 9(9).
           05 WS-ACT-NBPART      PIC 9(4).
           05 WS-ACT-MODETRANS   PIC 9(4).
           05 WS-ACT-LIEU        PIC X(100).
           05 WS-ACT-DISTANCE    PIC 9(9).
           05 WS-ACT-HEBERG      PIC 9(9).
           05 WS-ACT-REPAS       PIC 9(4).
           05 WS-ACT-EMPREINTE   PIC 9(13)V9(4).
       EXEC SQL END DECLARE SECTION END-EXEC.

       EXEC SQL INCLUDE SQLCA END-EXEC.

       LINKAGE SECTION.
       01 LK-OPERATION           PIC X(10).
       01 LK-END-OF-FILE         PIC X.
       01 LK-HEBERGEMENT.
           05 LK-HEB-ID          PIC 9(9).
           05 LK-HEB-ID-ACT      PIC 9(9).
           05 LK-HEB-TYPE        PIC 9(4).
           05 LK-HEB-NBNUIT      PIC 9(4).
       01 LK-ACTIVITE.
           05 LK-ACT-ID          PIC 9(9).
           05 LK-ACT-NOM         PIC X(50).
           05 LK-ACT-TYPE        PIC X(20).
           05 LK-ACT-IDANTENNE   PIC 9(9).
           05 LK-ACT-ANIMATEUR   PIC 9(9).
           05 LK-ACT-NBPART      PIC 9(4).
           05 LK-ACT-MODETRANS   PIC 9(4).
           05 LK-ACT-LIEU        PIC X(100).
           05 LK-ACT-DISTANCE    PIC 9(9).
           05 LK-ACT-HEBERG      PIC 9(9).
           05 LK-ACT-REPAS       PIC 9(4).
           05 LK-ACT-EMPREINTE   PIC 9(13)V9(4).

       PROCEDURE DIVISION USING LK-OPERATION LK-END-OF-FILE
           LK-HEBERGEMENT LK-ACTIVITE.
       MAIN-LOGIC.
           EVALUATE LK-OPERATION
               WHEN 'CREATEHEB'
                   PERFORM CREATE-HEBERGEMENT
               WHEN 'CREATEACT'
                   PERFORM CREATE-ACTIVITE
               WHEN 'ADDACTHEB'
                   PERFORM ADD-ACTIVITE-HEBERGEMENT
               WHEN 'READHEB'
                   PERFORM READ-HEBERGEMENT
               WHEN 'LISTHEBACT'
                   PERFORM LIST-HEBERGEMENTS-ACTIVITE
               WHEN 'UPDATEHEB'
                   PERFORM UPDATE-HEBERGEMENT
               WHEN 'DELETEHEB'
                   PERFORM DELETE-HEBERGEMENT
               WHEN 'END'
                   PERFORM END-PROGRAM
           END-EVALUATE.
           EXIT PROGRAM.

       CONNECT-DB.
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

       CREATE-HEBERGEMENT.
           PERFORM CONNECT-DB.
           IF LK-END-OF-FILE = 'Y' EXIT PARAGRAPH.

           MOVE LK-HEB-ID TO WS-HEB-ID.
           MOVE LK-HEB-ID-ACT TO WS-HEB-ID-ACT.
           MOVE LK-HEB-TYPE TO WS-HEB-TYPE.
           MOVE LK-HEB-NBNUIT TO WS-HEB-NBNUIT.

           EXEC SQL
               INSERT INTO HEBERGEMENT
               (HEBERGEMENT_ID, HEBERGEMENT_ID_ACTIVITE,
                HEBERGEMENT_TYPE, HEBERGEMENT_NBNUIT)
               VALUES
               (:WS-HEB-ID, :WS-HEB-ID-ACT, :WS-HEB-TYPE,
                :WS-HEB-NBNUIT)
           END-EXEC.

           IF SQLCODE NOT = 0
               MOVE 'Y' TO LK-END-OF-FILE
           END-IF.

       CREATE-ACTIVITE.
           PERFORM CONNECT-DB.
           IF LK-END-OF-FILE = 'Y' EXIT PARAGRAPH.

           MOVE LK-ACT-ID TO WS-ACT-ID.
           MOVE LK-ACT-NOM TO WS-ACT-NOM.
           MOVE LK-ACT-TYPE TO WS-ACT-TYPE.
           MOVE LK-ACT-IDANTENNE TO WS-ACT-IDANTENNE.
           MOVE LK-ACT-ANIMATEUR TO WS-ACT-ANIMATEUR.
           MOVE LK-ACT-NBPART TO WS-ACT-NBPART.
           MOVE LK-ACT-MODETRANS TO WS-ACT-MODETRANS.
           MOVE LK-ACT-LIEU TO WS-ACT-LIEU.
           MOVE LK-ACT-DISTANCE TO WS-ACT-DISTANCE.
           MOVE LK-ACT-HEBERG TO WS-ACT-HEBERG.
           MOVE LK-ACT-REPAS TO WS-ACT-REPAS.
           MOVE LK-ACT-EMPREINTE TO WS-ACT-EMPREINTE.

           EXEC SQL
               INSERT INTO ACTIVITE
               (ACTIVITE_ID, ACTIVITE_NOM, ACTIVITE_TYPE,
                ACTIVITE_IDANTENNE, ACTIVITE_ANIMATEUR,
                ACTIVITE_NBPARTICIPANTS, ACTIVITE_MODETRANSPORT,
                ACTIVITE_LIEU, ACTIVITE_DISTANCE, ACTIVITE_HEBERGEMENT,
                ACTIVITE_REPASPREVU, ACTIVITE_EMPREINTETOTALE)
               VALUES
               (:WS-ACT-ID, :WS-ACT-NOM, :WS-ACT-TYPE,
                :WS-ACT-IDANTENNE, :WS-ACT-ANIMATEUR,
                :WS-ACT-NBPART, :WS-ACT-MODETRANS,
                :WS-ACT-LIEU, :WS-ACT-DISTANCE, :WS-ACT-HEBERG,
                :WS-ACT-REPAS, :WS-ACT-EMPREINTE)
           END-EXEC.

           IF SQLCODE NOT = 0
               MOVE 'Y' TO LK-END-OF-FILE
           END-IF.

       ADD-ACTIVITE-HEBERGEMENT.
           PERFORM CONNECT-DB.
           IF LK-END-OF-FILE = 'Y' EXIT PARAGRAPH.

           MOVE LK-HEB-ID TO WS-HEB-ID.
           MOVE LK-ACT-ID TO WS-ACT-ID.

           EXEC SQL
               UPDATE ACTIVITE
               SET ACTIVITE_HEBERGEMENT = :WS-HEB-ID
               WHERE ACTIVITE_ID = :WS-ACT-ID
           END-EXEC.

           IF SQLCODE NOT = 0
               MOVE 'Y' TO LK-END-OF-FILE
           END-IF.

       READ-HEBERGEMENT.
           PERFORM CONNECT-DB.
           IF LK-END-OF-FILE = 'Y' EXIT PARAGRAPH.

           MOVE LK-HEB-ID TO WS-HEB-ID.

           EXEC SQL
               SELECT HEBERGEMENT_ID, HEBERGEMENT_ID_ACTIVITE,
                      HEBERGEMENT_TYPE, HEBERGEMENT_NBNUIT
               INTO :WS-HEB-ID, :WS-HEB-ID-ACT, :WS-HEB-TYPE,
                    :WS-HEB-NBNUIT
               FROM HEBERGEMENT
               WHERE HEBERGEMENT_ID = :WS-HEB-ID
           END-EXEC.

           IF SQLCODE = 0
               MOVE WS-HEB-ID TO LK-HEB-ID
               MOVE WS-HEB-ID-ACT TO LK-HEB-ID-ACT
               MOVE WS-HEB-TYPE TO LK-HEB-TYPE
               MOVE WS-HEB-NBNUIT TO LK-HEB-NBNUIT
           ELSE
               MOVE 'Y' TO LK-END-OF-FILE
           END-IF.

       LIST-HEBERGEMENTS-ACTIVITE.
           PERFORM CONNECT-DB.
           IF LK-END-OF-FILE = 'Y' EXIT PARAGRAPH.

           MOVE LK-ACT-ID TO WS-ACT-ID.

           IF WS-CURSOR-OPEN = 'N'
               EXEC SQL
                   DECLARE CHEB CURSOR FOR
                   SELECT HEBERGEMENT_ID, HEBERGEMENT_ID_ACTIVITE,
                          HEBERGEMENT_TYPE, HEBERGEMENT_NBNUIT
                   FROM HEBERGEMENT
                   WHERE HEBERGEMENT_ID_ACTIVITE = :WS-ACT-ID
               END-EXEC
               EXEC SQL
                   OPEN CHEB
               END-EXEC
               IF SQLCODE = 0
                   MOVE 'Y' TO WS-CURSOR-OPEN
               ELSE
                   MOVE 'Y' TO LK-END-OF-FILE
                   EXIT PARAGRAPH
               END-IF
           END-IF.

           EXEC SQL
               FETCH CHEB INTO
                   :WS-HEB-ID, :WS-HEB-ID-ACT, :WS-HEB-TYPE,
                   :WS-HEB-NBNUIT
           END-EXEC.

           IF SQLCODE = 0
               MOVE WS-HEB-ID TO LK-HEB-ID
               MOVE WS-HEB-ID-ACT TO LK-HEB-ID-ACT
               MOVE WS-HEB-TYPE TO LK-HEB-TYPE
               MOVE WS-HEB-NBNUIT TO LK-HEB-NBNUIT
           ELSE
               MOVE 'Y' TO LK-END-OF-FILE
               IF WS-CURSOR-OPEN = 'Y'
                   EXEC SQL
                       CLOSE CHEB
                   END-EXEC
                   MOVE 'N' TO WS-CURSOR-OPEN
               END-IF
           END-IF.

       UPDATE-HEBERGEMENT.
           PERFORM CONNECT-DB.
           IF LK-END-OF-FILE = 'Y' EXIT PARAGRAPH.

           MOVE LK-HEB-ID TO WS-HEB-ID.
           MOVE LK-HEB-TYPE TO WS-HEB-TYPE.
           MOVE LK-HEB-NBNUIT TO WS-HEB-NBNUIT.

           EXEC SQL
               UPDATE HEBERGEMENT
               SET HEBERGEMENT_TYPE = :WS-HEB-TYPE,
                   HEBERGEMENT_NBNUIT = :WS-HEB-NBNUIT
               WHERE HEBERGEMENT_ID = :WS-HEB-ID
           END-EXEC.

           IF SQLCODE NOT = 0
               MOVE 'Y' TO LK-END-OF-FILE
           END-IF.

       DELETE-HEBERGEMENT.
           PERFORM CONNECT-DB.
           IF LK-END-OF-FILE = 'Y' EXIT PARAGRAPH.

           MOVE LK-HEB-ID TO WS-HEB-ID.

           EXEC SQL
               DELETE FROM HEBERGEMENT
               WHERE HEBERGEMENT_ID = :WS-HEB-ID
           END-EXEC.

           IF SQLCODE NOT = 0
               MOVE 'Y' TO LK-END-OF-FILE
           END-IF.

       END-PROGRAM.
           IF WS-CURSOR-OPEN = 'Y'
               EXEC SQL
                   CLOSE CHEB
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
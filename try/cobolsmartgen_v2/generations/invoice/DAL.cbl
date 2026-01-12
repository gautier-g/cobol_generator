       IDENTIFICATION DIVISION.
       PROGRAM-ID. INVOICE-DAL-DB.
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
       01 WSINVID                PIC 9(9).
       01 WSINVDATE              PIC X(10).
       01 WSTOTALHT              PIC 9(7)V99.
       01 WSTOTALTTC             PIC 9(7)V99.
       EXEC SQL END DECLARE SECTION END-EXEC.

       EXEC SQL INCLUDE SQLCA END-EXEC.

       LINKAGE SECTION.
       01 LKOPERATION            PIC X(4).
       01 LKENDOFFILE            PIC X.
       01 LKINVOICE.
           05 LKINVID            PIC 9(9).
           05 LKINVDATE          PIC X(10).
           05 LKTOTALHT          PIC 9(7)V99.
           05 LKTOTALTTC         PIC 9(7)V99.

       PROCEDURE DIVISION USING LKOPERATION LKENDOFFILE
           LKINVOICE.
       MAINLOGIC.
           EVALUATE LKOPERATION
               WHEN 'READ'
                   PERFORM DALREAD
               WHEN 'SAVE'
                   PERFORM DALSAVE
               WHEN 'END '
                   PERFORM DALEND
           END-EVALUATE.
           EXIT PROGRAM.

       DALREAD.
           IF WS-CONNECTED = 'N'
               EXEC SQL
                   CONNECT :USERNAME IDENTIFIED BY :PASSWD USING :DBNAME
               END-EXEC
               EXEC SQL
                   SET client_encoding TO 'LATIN1'
               END-EXEC
               IF SQLCODE NOT = 0
                   MOVE 'Y' TO LKENDOFFILE
                   EXIT PARAGRAPH
               END-IF
               MOVE 'Y' TO WS-CONNECTED
           END-IF.

           IF WS-CURSOR-OPEN = 'N'
               EXEC SQL
                   DECLARE CINV CURSOR FOR
                   SELECT INV_ID, INV_DATE, TOTAL_HT, TOTAL_TTC
                   FROM INVOICE
               END-EXEC
               EXEC SQL
                   OPEN CINV
               END-EXEC
               IF SQLCODE NOT = 0
                   MOVE 'Y' TO LKENDOFFILE
                   EXIT PARAGRAPH
               END-IF
               MOVE 'Y' TO WS-CURSOR-OPEN
           END-IF.

           EXEC SQL
               FETCH CINV INTO
                   :WSINVID,
                   :WSINVDATE,
                   :WSTOTALHT,
                   :WSTOTALTTC
           END-EXEC.

           IF SQLCODE NOT = 0
               MOVE 'Y' TO LKENDOFFILE
           ELSE
               MOVE WSINVID        TO LKINVID
               MOVE WSINVDATE      TO LKINVDATE
               MOVE WSTOTALHT      TO LKTOTALHT
               MOVE WSTOTALTTC     TO LKTOTALTTC
           END-IF.

       DALSAVE.
           MOVE LKINVID        TO WSINVID.
           MOVE LKINVDATE      TO WSINVDATE.
           MOVE LKTOTALHT      TO WSTOTALHT.
           MOVE LKTOTALTTC     TO WSTOTALTTC.

           EXEC SQL
               UPDATE INVOICE
               SET INV_DATE = :WSINVDATE,
                   TOTAL_HT = :WSTOTALHT,
                   TOTAL_TTC = :WSTOTALTTC
               WHERE INV_ID = :WSINVID
           END-EXEC.

           IF SQLCODE NOT = 0
               EXEC SQL
                   INSERT INTO INVOICE
                   (INV_ID, INV_DATE, TOTAL_HT, TOTAL_TTC)
                   VALUES
                   (:WSINVID, :WSINVDATE, :WSTOTALHT, :WSTOTALTTC)
               END-EXEC
           END-IF.

       DALEND.
           IF WS-CURSOR-OPEN = 'Y'
               EXEC SQL
                   CLOSE CINV
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
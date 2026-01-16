       IDENTIFICATION DIVISION.
       PROGRAM-ID. USERDAL.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-CONNECTED           PIC X VALUE 'N'.
       01 WS-SQLCODE             PIC S9(9) COMP-5.

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01 DBNAME                 PIC X(30) VALUE 'postgres'.
       01 USERNAME               PIC X(30) VALUE 'postgres'.
       01 PASSWD                 PIC X(30) VALUE 'postgres'.
       01 WSUSER.
           05 WSUSERID           PIC 9(9).
           05 WSUSERNOM          PIC X(50).
           05 WSUSERMAIL         PIC X(80).
           05 WSUSERPASS         PIC X(256).
           05 WSUSERROLE         PIC X(15).
           05 WSUSERIDANTENNE    PIC 9(9).
           05 WSUSERLASTLOGIN    PIC 9(18).
       01 WSMAIL                 PIC X(80).
       EXEC SQL END DECLARE SECTION END-EXEC.

       EXEC SQL INCLUDE SQLCA END-EXEC.

       LINKAGE SECTION.
       01 LKOPERATION            PIC X(6).
       01 LKRETURN               PIC S9(9) COMP-5.
       01 LKUSER.
           05 LKUSERID           PIC 9(9).
           05 LKUSERNOM          PIC X(50).
           05 LKUSERMAIL         PIC X(80).
           05 LKUSERPASS         PIC X(256).
           05 LKUSERROLE         PIC X(15).
           05 LKUSERIDANTENNE    PIC 9(9).
           05 LKUSERLASTLOGIN    PIC 9(18).

       PROCEDURE DIVISION USING LKOPERATION LKRETURN LKUSER.
       MAINLOGIC.
           EVALUATE LKOPERATION
               WHEN 'CREATE'
                   PERFORM CREATEUSER
               WHEN 'READ  '
                   PERFORM READUSER
               WHEN 'UPDATE'
                   PERFORM UPDATEUSER
               WHEN 'DELETE'
                   PERFORM DELETEUSER
               WHEN 'END   '
                   PERFORM ENDPROG
           END-EVALUATE.
           EXIT PROGRAM.

       CONNECTDB.
           IF WS-CONNECTED = 'N'
               EXEC SQL
                   CONNECT :USERNAME IDENTIFIED BY :PASSWD USING :DBNAME
               END-EXEC
               IF SQLCODE NOT = 0
                   MOVE SQLCODE TO LKRETURN
                   EXIT PARAGRAPH
               END-IF
               EXEC SQL
                   SET client_encoding TO 'LATIN1'
               END-EXEC
               MOVE 'Y' TO WS-CONNECTED
           END-IF.

       CREATEUSER.
           PERFORM CONNECTDB.
           IF LKRETURN NOT = 0
               EXIT PARAGRAPH
           END-IF.

           MOVE LKUSERMAIL TO WSMAIL.
           EXEC SQL
               SELECT COUNT(*) INTO :WSUSERID
               FROM UTILISATEUR
               WHERE USER_MAIL = :WSMAIL
           END-EXEC.
           IF WSUSERID > 0
               MOVE -803 TO LKRETURN
               EXIT PARAGRAPH
           END-IF.

           MOVE LKUSERID TO WSUSERID.
           MOVE LKUSERNOM TO WSUSERNOM.
           MOVE LKUSERMAIL TO WSUSERMAIL.
           MOVE LKUSERPASS TO WSUSERPASS.
           MOVE LKUSERROLE TO WSUSERROLE.
           MOVE LKUSERIDANTENNE TO WSUSERIDANTENNE.
           MOVE LKUSERLASTLOGIN TO WSUSERLASTLOGIN.

           EXEC SQL
               INSERT INTO UTILISATEUR
               (USER_ID, USER_NOM, USER_MAIL, USER_PASS, USER_ROLE,
                USER_ID_ANTENNE, USER_LAST_LOGIN)
               VALUES
               (:WSUSERID, :WSUSERNOM, :WSUSERMAIL, :WSUSERPASS,
                :WSUSERROLE, :WSUSERIDANTENNE, :WSUSERLASTLOGIN)
           END-EXEC.
           MOVE SQLCODE TO LKRETURN.

       READUSER.
           PERFORM CONNECTDB.
           IF LKRETURN NOT = 0
               EXIT PARAGRAPH
           END-IF.

           MOVE LKUSERMAIL TO WSMAIL.
           EXEC SQL
               SELECT USER_ID, USER_NOM, USER_MAIL, USER_PASS,
                      USER_ROLE, USER_ID_ANTENNE, USER_LAST_LOGIN
               INTO :WSUSERID, :WSUSERNOM, :WSUSERMAIL, :WSUSERPASS,
                    :WSUSERROLE, :WSUSERIDANTENNE, :WSUSERLASTLOGIN
               FROM UTILISATEUR
               WHERE USER_MAIL = :WSMAIL
           END-EXEC.
           IF SQLCODE = 0
               MOVE WSUSERID TO LKUSERID
               MOVE WSUSERNOM TO LKUSERNOM
               MOVE WSUSERMAIL TO LKUSERMAIL
               MOVE WSUSERPASS TO LKUSERPASS
               MOVE WSUSERROLE TO LKUSERROLE
               MOVE WSUSERIDANTENNE TO LKUSERIDANTENNE
               MOVE WSUSERLASTLOGIN TO LKUSERLASTLOGIN
           END-IF.
           MOVE SQLCODE TO LKRETURN.

       UPDATEUSER.
           PERFORM CONNECTDB.
           IF LKRETURN NOT = 0
               EXIT PARAGRAPH
           END-IF.

           MOVE LKUSERMAIL TO WSMAIL.
           MOVE LKUSERNOM TO WSUSERNOM.
           MOVE LKUSERPASS TO WSUSERPASS.
           MOVE LKUSERLASTLOGIN TO WSUSERLASTLOGIN.

           EXEC SQL
               UPDATE UTILISATEUR
               SET USER_NOM = :WSUSERNOM,
                   USER_PASS = :WSUSERPASS,
                   USER_LAST_LOGIN = :WSUSERLASTLOGIN
               WHERE USER_MAIL = :WSMAIL
           END-EXEC.
           MOVE SQLCODE TO LKRETURN.

       DELETEUSER.
           PERFORM CONNECTDB.
           IF LKRETURN NOT = 0
               EXIT PARAGRAPH
           END-IF.

           MOVE LKUSERMAIL TO WSMAIL.
           EXEC SQL
               DELETE FROM UTILISATEUR
               WHERE USER_MAIL = :WSMAIL
           END-EXEC.
           MOVE SQLCODE TO LKRETURN.

       ENDPROG.
           EXEC SQL
               COMMIT
           END-EXEC.
           IF WS-CONNECTED = 'Y'
               EXEC SQL
                   DISCONNECT ALL
               END-EXEC
               MOVE 'N' TO WS-CONNECTED
           END-IF.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. USERLIST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-USER.
           05 WS-USERID           PIC 9(9).
           05 WS-USERNOM          PIC X(50).
           05 WS-USERMAIL         PIC X(80).
           05 WS-USERPASS         PIC X(256).
           05 WS-USERROLE         PIC X(15).
           05 WS-USERIDANTENNE    PIC 9(9).
           05 WS-USERLASTLOGIN    PIC 9(18).
       01 WS-OPERATION           PIC X(6) VALUE 'READ  '.
       01 WS-RETURN              PIC S9(9) COMP-5.
       01 WS-FIRST-READ          PIC X VALUE 'Y'.
       01 WS-EMPTY-LOGIN         PIC X VALUE 'N'.
       01 WS-FORMATTED-INFO.
           05 FILLER             PIC X(10) VALUE "NOM: ".
           05 WS-FMT-NOM         PIC X(50).
           05 FILLER             PIC X(10) VALUE " | MAIL: ".
           05 WS-FMT-MAIL        PIC X(80).
           05 FILLER             PIC X(10) VALUE " | ROLE: ".
           05 WS-FMT-ROLE        PIC X(15).
OCESQL*
       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           PERFORM UNTIL WS-RETURN NOT = 0
               CALL 'USERDAL' USING WS-OPERATION WS-RETURN WS-USER
               IF WS-RETURN = 0
                   IF WS-FIRST-READ = 'Y'
                       MOVE 'N' TO WS-FIRST-READ
                       MOVE WS-USERMAIL TO WS-USERMAIL
                       PERFORM READ-NEXT
                   ELSE
                       MOVE WS-USERNOM TO WS-FMT-NOM
                       MOVE WS-USERMAIL TO WS-FMT-MAIL
                       MOVE WS-USERROLE TO WS-FMT-ROLE
                       DISPLAY WS-FORMATTED-INFO
                       IF WS-USERLASTLOGIN = 0
                           MOVE 'Y' TO WS-EMPTY-LOGIN
                           DISPLAY "  -> Derniere connexion absente"
                       END-IF
                       PERFORM READ-NEXT
                   END-IF
               END-IF
           END-PERFORM.
           IF WS-EMPTY-LOGIN = 'N'
               DISPLAY "Tous les utilisateurs ont une date de connexion"
           END-IF.
           CALL 'USERDAL' USING 'END   ' WS-RETURN WS-USER.
           STOP RUN.

       READ-NEXT.
           MOVE 'READ  ' TO WS-OPERATION.
           MOVE 0 TO WS-USERID WS-USERLASTLOGIN.
           MOVE SPACES TO WS-USERNOM WS-USERPASS WS-USERROLE.
           MOVE WS-USERMAIL TO WS-USERMAIL.
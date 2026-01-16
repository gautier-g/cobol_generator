       IDENTIFICATION DIVISION.
       PROGRAM-ID. LISTUSERS.
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
       01 WS-RETURN               PIC S9(9) COMP-5.
       01 WS-EOF                  PIC X VALUE 'N'.
           88 EOF                 VALUE 'Y'.
       01 WS-DISPLAY.
           05 FILLER              PIC X(10) VALUE "NOM: ".
           05 WS-DISPLAY-NOM      PIC X(50).
           05 FILLER              PIC X(10) VALUE " | MAIL: ".
           05 WS-DISPLAY-MAIL     PIC X(80).
       01 WS-DISPLAY2.
           05 FILLER              PIC X(10) VALUE "ROLE: ".
           05 WS-DISPLAY-ROLE     PIC X(15).
       01 WS-NO-LOGIN             PIC X(30) VALUE
           " (pas de date de connexion)".

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           MOVE 0 TO WS-USERID.
           PERFORM UNTIL EOF
               CALL "USERDAL" USING "READID" WS-RETURN WS-USER
               IF WS-RETURN NOT = 0
                   IF WS-RETURN = 100
                       MOVE 'Y' TO WS-EOF
                   ELSE
                       DISPLAY "ERREUR LECTURE UTILISATEUR: " WS-RETURN
                       EXIT PERFORM
                   END-IF
               ELSE
                   MOVE WS-USERNOM TO WS-DISPLAY-NOM
                   MOVE WS-USERMAIL TO WS-DISPLAY-MAIL
                   MOVE WS-USERROLE TO WS-DISPLAY-ROLE
                   DISPLAY WS-DISPLAY
                   DISPLAY WS-DISPLAY2
                   IF WS-USERLASTLOGIN = 0
                       DISPLAY WS-NO-LOGIN
                   END-IF
                   DISPLAY " "
               END-IF
           END-PERFORM.
           STOP RUN.
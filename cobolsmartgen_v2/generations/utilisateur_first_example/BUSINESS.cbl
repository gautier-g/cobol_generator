       IDENTIFICATION DIVISION.
       PROGRAM-ID. DISPLAYUSER.
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
       01 WS-DISPLAY.
           05 FILLER              PIC X(20) VALUE "ID: ".
           05 WS-DISPLAY-ID       PIC 9(9).
           05 FILLER              PIC X(20) VALUE " | NOM: ".
           05 WS-DISPLAY-NOM      PIC X(50).
       01 WS-DISPLAY2.
           05 FILLER              PIC X(20) VALUE "MAIL: ".
           05 WS-DISPLAY-MAIL     PIC X(80).
           05 FILLER              PIC X(20) VALUE " | ROLE: ".
           05 WS-DISPLAY-ROLE     PIC X(15).
       01 WS-DISPLAY3.
           05 FILLER              PIC X(20) VALUE "ID ANTENNE: ".
           05 WS-DISPLAY-IDANT    PIC 9(9).
           05 FILLER              PIC X(20) VALUE " | LAST LOGIN: ".
           05 WS-DISPLAY-LOGIN    PIC 9(18).

       LINKAGE SECTION.
       01 LK-EMAIL                PIC X(80).
       01 LK-RETURN               PIC S9(9) COMP-5.

       PROCEDURE DIVISION USING LK-EMAIL LK-RETURN.
       MAIN-PARAGRAPH.
           CALL "USERDAL" USING "READ  " WS-RETURN WS-USER
           IF WS-RETURN NOT = 0
               MOVE WS-RETURN TO LK-RETURN
               EXIT PROGRAM
           END-IF.

           MOVE WS-USERID TO WS-DISPLAY-ID.
           MOVE WS-USERNOM TO WS-DISPLAY-NOM.
           MOVE WS-USERMAIL TO WS-DISPLAY-MAIL.
           MOVE WS-USERROLE TO WS-DISPLAY-ROLE.
           MOVE WS-USERIDANTENNE TO WS-DISPLAY-IDANT.
           MOVE WS-USERLASTLOGIN TO WS-DISPLAY-LOGIN.

           DISPLAY WS-DISPLAY.
           DISPLAY WS-DISPLAY2.
           DISPLAY WS-DISPLAY3.

           MOVE 0 TO LK-RETURN.
           EXIT PROGRAM.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. USERDISP.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-USER.
           05 WS-USERID           PIC 9(9).
           05 WS-USERNOM          PIC X(50).
           05 WS-USERMAIL         PIC X(80).
           05 WS-USERROLE         PIC X(15).
           05 WS-USERIDANTENNE    PIC 9(9).
           05 WS-USERLASTLOGIN    PIC 9(18).
       01 WS-FORMATTED-DATA.
           05 FILLER              PIC X(20) VALUE "ID: ".
           05 WS-FMT-ID           PIC 9(9).
           05 FILLER              PIC X(20) VALUE " | NOM: ".
           05 WS-FMT-NOM          PIC X(50).
           05 FILLER              PIC X VALUE "|".
       01 WS-FORMATTED-DATA2.
           05 FILLER              PIC X(20) VALUE "MAIL: ".
           05 WS-FMT-MAIL         PIC X(80).
           05 FILLER              PIC X(20) VALUE " | ROLE: ".
           05 WS-FMT-ROLE         PIC X(15).
           05 FILLER              PIC X VALUE "|".
       01 WS-FORMATTED-DATA3.
           05 FILLER              PIC X(20) VALUE "ANTENNE: ".
           05 WS-FMT-ANTENNE      PIC 9(9).
           05 FILLER              PIC X(20) VALUE " | LOGIN: ".
           05 WS-FMT-LOGIN        PIC 9(18).
           05 FILLER              PIC X VALUE "|".
OCESQL*
       LINKAGE SECTION.
       01 LK-USER.
           05 LK-USERID           PIC 9(9).
           05 LK-USERNOM          PIC X(50).
           05 LK-USERMAIL         PIC X(80).
           05 LK-USERPASS         PIC X(256).
           05 LK-USERROLE         PIC X(15).
           05 LK-USERIDANTENNE    PIC 9(9).
           05 LK-USERLASTLOGIN    PIC 9(18).
       PROCEDURE DIVISION USING LK-USER.
       MAIN-PARAGRAPH.
           MOVE LK-USERID TO WS-USERID.
           MOVE LK-USERNOM TO WS-USERNOM.
           MOVE LK-USERMAIL TO WS-USERMAIL.
           MOVE LK-USERROLE TO WS-USERROLE.
           MOVE LK-USERIDANTENNE TO WS-USERIDANTENNE.
           MOVE LK-USERLASTLOGIN TO WS-USERLASTLOGIN.

           MOVE WS-USERID TO WS-FMT-ID.
           MOVE WS-USERNOM TO WS-FMT-NOM.
           MOVE WS-USERMAIL TO WS-FMT-MAIL.
           MOVE WS-USERROLE TO WS-FMT-ROLE.
           MOVE WS-USERIDANTENNE TO WS-FMT-ANTENNE.
           MOVE WS-USERLASTLOGIN TO WS-FMT-LOGIN.

           DISPLAY WS-FORMATTED-DATA.
           DISPLAY WS-FORMATTED-DATA2.
           DISPLAY WS-FORMATTED-DATA3.
           EXIT PROGRAM.
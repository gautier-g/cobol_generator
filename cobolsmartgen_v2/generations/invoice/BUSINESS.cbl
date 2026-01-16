       IDENTIFICATION DIVISION.
       PROGRAM-ID. INVOICE-DISPLAY.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-LINE                 PIC X(72) VALUE ALL "-".
       01 WS-TITLE.
           05 FILLER              PIC X(28) VALUE SPACES.
           05 FILLER              PIC X(16) VALUE "FACTURE CLIENT".
       01 WS-HEADER.
           05 FILLER              PIC X(10) VALUE "NUMERO: ".
           05 WS-DISPLAY-INVID    PIC 9(9).
           05 FILLER              PIC X(10) VALUE SPACES.
           05 FILLER              PIC X(8) VALUE "DATE: ".
           05 WS-DISPLAY-INVDATE  PIC X(10).
       01 WS-LINE-DETAILS.
           05 FILLER              PIC X(15) VALUE "MONTANT HT: ".
           05 WS-DISPLAY-HT       PIC Z(6)9.99.
           05 FILLER              PIC X(15) VALUE SPACES.
           05 FILLER              PIC X(15) VALUE "MONTANT TTC: ".
           05 WS-DISPLAY-TTC      PIC Z(6)9.99.
       LINKAGE SECTION.
       01 LKINVOICE.
           05 LKINVID             PIC 9(9).
           05 LKINVDATE           PIC X(10).
           05 LKTOTALHT           PIC 9(7)V99.
           05 LKTOTALTTC          PIC 9(7)V99.
       PROCEDURE DIVISION USING LKINVOICE.
       MAIN-PARAGRAPH.
           MOVE LKINVID           TO WS-DISPLAY-INVID.
           MOVE LKINVDATE         TO WS-DISPLAY-INVDATE.
           MOVE LKTOTALHT         TO WS-DISPLAY-HT.
           MOVE LKTOTALTTC        TO WS-DISPLAY-TTC.

           DISPLAY WS-LINE.
           DISPLAY WS-TITLE.
           DISPLAY WS-LINE.
           DISPLAY WS-HEADER.
           DISPLAY WS-LINE.
           DISPLAY WS-LINE-DETAILS.
           DISPLAY WS-LINE.

           EXIT PROGRAM.
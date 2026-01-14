       IDENTIFICATION DIVISION.
       PROGRAM-ID. ANTENNE-BUSINESS.

       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       LINKAGE SECTION.
       01 LK-ANTENNE.
           05 LK-ANTENNE-ID PIC 9(9).
           05 LK-ANTENNE-NOM PIC X(50).
           05 LK-ANTENNE-REGION PIC X(50).
       
       PROCEDURE DIVISION USING LK-ANTENNE.
       DISPLAY-ANTENNE.
           DISPLAY '----------------------------------------'
           DISPLAY 'ANTENNE   : ' LK-ANTENNE-NOM
           DISPLAY 'ID        : ' LK-ANTENNE-ID
           DISPLAY 'REGION    : ' LK-ANTENNE-REGION
           DISPLAY '----------------------------------------'
           GOBACK
       .
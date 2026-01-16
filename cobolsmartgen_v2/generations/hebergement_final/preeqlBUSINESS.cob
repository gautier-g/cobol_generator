       IDENTIFICATION DIVISION.
       PROGRAM-ID. AFFICHE-HEBERGEMENT.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-HEBERGEMENT.
           05 WS-HEB-ID          PIC 9(9).
           05 WS-HEB-ID-ACT      PIC 9(9).
           05 WS-HEB-TYPE        PIC 9(4).
           05 WS-HEB-NBNUIT      PIC 9(4).
       01 WS-TYPE-HEB            PIC X(20).
       01 WS-FORMAT-NUM          PIC Z(8)9.
OCESQL*
       LINKAGE SECTION.
       01 LK-HEBERGEMENT.
           05 LK-HEB-ID          PIC 9(9).
           05 LK-HEB-ID-ACT      PIC 9(9).
           05 LK-HEB-TYPE        PIC 9(4).
           05 LK-HEB-NBNUIT      PIC 9(4).
       PROCEDURE DIVISION USING LK-HEBERGEMENT.
       MAIN-LOGIC.
           MOVE LK-HEB-ID TO WS-HEB-ID.
           MOVE LK-HEB-ID-ACT TO WS-HEB-ID-ACT.
           MOVE LK-HEB-TYPE TO WS-HEB-TYPE.
           MOVE LK-HEB-NBNUIT TO WS-HEB-NBNUIT.

           EVALUATE WS-HEB-TYPE
               WHEN 1 MOVE "Hôtel" TO WS-TYPE-HEB
               WHEN 2 MOVE "Auberge" TO WS-TYPE-HEB
               WHEN 3 MOVE "Camping" TO WS-TYPE-HEB
               WHEN 4 MOVE "Gîte" TO WS-TYPE-HEB
               WHEN OTHER MOVE "Inconnu" TO WS-TYPE-HEB
           END-EVALUATE.

           DISPLAY "ID Hébergement: " WS-HEB-ID.
           DISPLAY "ID Activité: " WS-HEB-ID-ACT.
           DISPLAY "Type: " WS-TYPE-HEB.
           MOVE WS-HEB-NBNUIT TO WS-FORMAT-NUM.
           DISPLAY "Nombre de nuits: " WS-FORMAT-NUM.

           EXIT PROGRAM.
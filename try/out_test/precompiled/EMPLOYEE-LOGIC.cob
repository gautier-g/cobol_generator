       IDENTIFICATION DIVISION.
       PROGRAM-ID. EMPLOYEE-LOGIC.
      *****************************************************************
      * Programme: EMPLOYEE-LOGIC                                      *
      * Couche: LOGIC (Orchestration)                                  *
      * Role: Traitement batch principal                               *
      *       - Boucle sur les employes                                *
      *       - Calcul du salaire net (BRUT * 0.7)                     *
      *       - Sauvegarde en base                                     *
      *       - Affichage des resultats                                *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  END-OF-FILE            PIC X VALUE 'N'.
           88  EOF-REACHED        VALUE 'Y'.
           88  NOT-EOF            VALUE 'N'.
       77  OPERATION              PIC X(4).
       77  WS-COUNT               PIC 9(4) VALUE 0.
       01  EMPLOYEE.
           05 EMP-ID              PIC 9(4).
           05 EMP-NAME            PIC A(30).
           05 SALARY-BRUT         PIC S9(6)V99.
           05 SALARY-NET          PIC S9(6)V99.

OCESQL*
       PROCEDURE DIVISION.
       MAIN-PROCESS.
           DISPLAY '=========================================='
           DISPLAY 'DEBUT TRAITEMENT BATCH CALCUL SALAIRE NET'
           DISPLAY '=========================================='
           MOVE 'READ' TO OPERATION
           CALL 'EMPLOYEE-DAL-DB' USING OPERATION
                                         END-OF-FILE
                                         EMPLOYEE
           PERFORM UNTIL EOF-REACHED
               ADD 1 TO WS-COUNT
               PERFORM CALCULATE-NET
               MOVE 'SAVE' TO OPERATION
               CALL 'EMPLOYEE-DAL-DB' USING OPERATION
                                            END-OF-FILE
                                            EMPLOYEE
               CALL 'EMPLOYEE-BUSINESS' USING EMPLOYEE
               MOVE 'READ' TO OPERATION
               CALL 'EMPLOYEE-DAL-DB' USING OPERATION
                                            END-OF-FILE
                                            EMPLOYEE
           END-PERFORM
           MOVE 'END ' TO OPERATION
           CALL 'EMPLOYEE-DAL-DB' USING OPERATION
                                         END-OF-FILE
                                         EMPLOYEE
           DISPLAY '=========================================='
           DISPLAY 'FIN TRAITEMENT BATCH'
           DISPLAY 'Nombre employes traites: ' WS-COUNT
           DISPLAY '=========================================='
           STOP RUN
           .

       CALCULATE-NET.
      *****************************************************************
      * Regle metier R1: SALARY_NET = ROUND(SALARY_BRUT * 0.7, 2)    *
      * Calcul: Salaire net = Salaire brut - 30%                      *
      *         (20% cotisations sociales + 10% impot)                *
      *****************************************************************
           IF SALARY-BRUT IS NUMERIC
               COMPUTE SALARY-NET ROUNDED = SALARY-BRUT * 0.7
               IF SALARY-NET < 0
                   MOVE 0 TO SALARY-NET
                   DISPLAY 'ANOMALIE: Salaire net negatif corrige'
                   DISPLAY '  EMP_ID=' EMP-ID
               END-IF
           ELSE
               DISPLAY 'ANOMALIE: SALARY_BRUT invalide'
               DISPLAY '  EMP_ID=' EMP-ID
               DISPLAY '  EMP_NAME=' EMP-NAME
               DISPLAY '  SALARY_BRUT=' SALARY-BRUT
               MOVE 0 TO SALARY-NET
           END-IF
           .
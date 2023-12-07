       IDENTIFICATION DIVISION. 
       PROGRAM-ID. CALCULATOR.
       ENVIRONMENT DIVISION. 
       INPUT-OUTPUT SECTION. 
       FILE-CONTROL.
           SELECT INP-FILE ASSIGN TO "input"
                      STATUS INP-ST.
           SELECT OUT-REC ASSIGN to "outres"
                      STATUS OUT-ST.
       DATA DIVISION. 
       FILE SECTION.
       FD  INP-FILE.
       01  NUMBERS-REC.
         05 INP-NUMBER-1 PIC 9(4).
         05 INP-OPERATOR PIC X(3).
         05 INP-NUMBER-2 PIC 9(4).
       FD  OUT-REC RECORDING MODE F.
       01  OUT-RES  PIC X(8).
       WORKING-STORAGE SECTION.
       77  RESULT   PIC S9(8).
       01  S-NUMBS.
         05  NUMBER-1 PIC 9(4).
         05  OPERATOR PIC X(3).
         05  NUMBER-2 PIC 9(4).
       01  OUT-ST   PIC 9(2).
       01  INP-ST   PIC 9(2).
       01  HEADERS.
         05 FILLER  PIC X(8) VALUE 'RES =>  '.
       PROCEDURE DIVISION.
       MAIN-PROC.
           PERFORM OPEN-FILES.
           PERFORM WRITE-HEADERS.
           PERFORM PROC-FILES.
           PERFORM WRITE-FILES.
           PERFORM EXIT-FILES.
       MAIN-PROC-END. EXIT.
      ************************
       WRITE-HEADERS.
           WRITE OUT-RES FROM HEADERS.
           MOVE SPACES TO OUT-RES.
       WRITE-HEADERS-END. EXIT.
      ************************
       OPEN-FILES.
           OPEN INPUT  INP-FILE.
           OPEN OUTPUT OUT-REC.
           IF (OUT-ST NOT = 0) AND (OUT-ST NOT = 97)
           DISPLAY 'hata' OUT-ST
           IF (INP-ST NOT = 0) AND (INP-ST NOT = 97)
           DISPLAY 'hata' INP-ST
           END-IF.
           READ  INP-FILE.
       OPEN-FILES-END. EXIT.
      ************************
       PROC-FILES.
           MOVE NUMBERS-REC TO S-NUMBS.
           IF OPERATOR = " + " 
              COMPUTE  RESULT = NUMBER-1 + NUMBER-2 
           ELSE IF OPERATOR = " - " 
              COMPUTE  RESULT = NUMBER-1 - NUMBER-2
           ELSE IF OPERATOR = " * "
              COMPUTE RESULT = NUMBER-1 * NUMBER-2 
           ELSE IF OPERATOR = " / " 
              COMPUTE RESULT  = NUMBER-1 / NUMBER-2 
           ELSE
               DISPLAY 'Wrong Operator!' OPERATOR
               PERFORM EXIT-FILES
           END-IF.
       PROC-FILES-END. EXIT.
      ************************
       WRITE-FILES.
           MOVE RESULT TO OUT-RES.
           WRITE OUT-RES.
       WRITE-FILES-END. EXIT.
       EXIT-FILES.
           CLOSE OUT-REC.
           CLOSE INP-FILE.
                    STOP RUN.
       EXIT-FILES-END. EXIT.

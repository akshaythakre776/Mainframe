       IDENTIFICATION DIVISION.
       PROGRAM-ID. APROG4.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
          SELECT EMPLOYEE-FILE ASSIGN TO 'EMPLOYEE.DAT'
           ORGANIZATION IS SEQUENTIAL
           ACCESS MODE  IS SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  EMPLOYEE-FILE
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 50 CHARACTERS.
       01  EMPLOYEE-RECORD.
           03  EMP-ID          PIC X(05).
           03  EMP-NAME        PIC X(20).
           03  EMP-CITY        PIC X(03).
           03  EMP-DOB         PIC 9(08).
           03  EMP-EARN        PIC 9(5)V99.
           03  EMP-DEDN        PIC 9(5)V99.
       WORKING-STORAGE SECTION.
       01  WS-FILE-FLAG        PIC X(01) VALUE 'N'.
           88  END-OF-FILE               VALUE 'Y'.
       01  TOTAL-COUNTERS.
           03  TOTAL-RECS      PIC 9(02) VALUE ZERO.
           03  TOTAL-EARN      PIC 9(06)V99 VALUE ZERO.
           03  TOTAL-DEDN      PIC 9(06)V99 VALUE ZERO.
           03  TOTAL-SALARY    PIC 9(06)V99 VALUE ZERO.
           03  EMP-SALARY      PIC 9(05)V99 VALUE ZERO.
       01  DISPLAY-COUNTERS.
           03  OUT-RECS        PIC Z9 VALUE ZERO.
           03  OUT-EARN        PIC ZZZ,ZZ9.99 VALUE ZERO.
           03  OUT-DEDN        PIC ZZZ,ZZ9.99 VALUE ZERO.
           03  OUT-SALARY      PIC ZZZ,ZZ9.99 VALUE ZERO.
           03  OUT-DATE        PIC 9999/99/99.
       PROCEDURE DIVISION.
       MAIN-PARA.
           OPEN INPUT EMPLOYEE-FILE
           PERFORM READ-PARA
           PERFORM  UNTIL  END-OF-FILE
              PERFORM CALC-N-TOTAL-PARA
              PERFORM DISPLAY-PARA
              PERFORM READ-PARA
           END-PERFORM
           PERFORM TOTALS-PARA
           CLOSE EMPLOYEE-FILE
           STOP RUN.
       READ-PARA.
           READ EMPLOYEE-FILE
               AT END
                   MOVE 'Y' TO WS-FILE-FLAG
               NOT AT END
                   ADD 1 TO TOTAL-RECS
             END-READ.
       CALC-N-TOTAL-PARA.
           COMPUTE EMP-SALARY = EMP-EARN - EMP-DEDN       
           ADD EMP-EARN    TO TOTAL-EARN
           ADD EMP-DEDN    TO TOTAL-DEDN
           ADD EMP-SALARY  TO TOTAL-SALARY.
       DISPLAY-PARA.           
           MOVE EMP-EARN   TO OUT-EARN
           MOVE EMP-DEDN   TO OUT-DEDN
           MOVE EMP-SALARY TO OUT-SALARY
           MOVE EMP-DOB    TO OUT-DATE
           DISPLAY EMP-ID, '  ', EMP-NAME, '  ', EMP-CITY, '  ',
               OUT-DATE, ' ', OUT-EARN, ' ', OUT-DEDN, ' ', OUT-SALARY.
       TOTALS-PARA.        
           MOVE TOTAL-RECS     TO OUT-RECS
           MOVE TOTAL-EARN     TO OUT-EARN
           MOVE TOTAL-DEDN     TO OUT-DEDN
           MOVE TOTAL-SALARY   TO OUT-SALARY
           DISPLAY '** COMPANY TOTALS **'
           DISPLAY 'TOTALS ', OUT-RECS, '  ', OUT-EARN, '  ',
               OUT-DEDN, '  ', OUT-SALARY.
           DISPLAY 'END OF PROGRAM REACHED'
           DISPLAY '**------------------**'.

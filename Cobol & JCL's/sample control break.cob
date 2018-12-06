       IDENTIFICATION DIVISION.
       PROGRAM-ID. APROG7.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLOYEE-FILE ASSIGN TO 'EMPLOYEE.DAT'
           ORGANIZATION IS SEQUENTIAL
           ACCESS MODE  IS SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  EMPLOYEE-FILE.
       01  EMPLOYEE-RECORD.
           03  EMP-ID          PIC X(05).
           03  EMP-NAME        PIC X(15).
           03  EMP-LOC         PIC X(03).
           03  EMP-TECH        PIC X(05).
           03  EMP-DOB         PIC X(08).
           03  EMP-EARN        PIC 9(05)V99.
           03  EMP-DEDN        PIC 9(05)V99.
       WORKING-STORAGE SECTION.
       01  WS-FILE-FLAG    PIC X(01) VALUE 'N'.
           88  END-OF-FILE           VALUE 'Y'.
       01  PREV-LOC        PIC X(03) VALUE SPACES.
       01  PREV-TECH       PIC X(05) VALUE SPACES.
       PROCEDURE DIVISION.
       MAIN-PARA.
           PERFORM INIT-PARA.
           PERFORM PROCESS-PARA  UNTIL  END-OF-FILE
           PERFORM TERM-PARA.
           STOP RUN.
       INIT-PARA.
           OPEN INPUT EMPLOYEE-FILE.
           PERFORM READ-EMPLOYEE
           MOVE EMP-LOC    TO PREV-LOC
           MOVE EMP-TECH   TO PREV-TECH.
       PROCESS-PARA.
           IF EMP-LOC = PREV-LOC
               IF EMP-TECH = PREV-TECH
                   NEXT SENTENCE
               ELSE
                   PERFORM TECH-CHANGE-PARA
               END-IF
           ELSE
               PERFORM TECH-CHANGE-PARA
               PERFORM LOC-CHANGE-PARA
           END-IF.
           PERFORM PROCESS-EMPLOYEE
           PERFORM READ-EMPLOYEE.
       READ-EMPLOYEE.
           READ EMPLOYEE-FILE
               AT END
                   MOVE 'Y' TO WS-FILE-FLAG
           END-READ.
       PROCESS-EMPLOYEE.
           DISPLAY 'EMP DTL ', EMPLOYEE-RECORD.
       TECH-CHANGE-PARA.
           DISPLAY '*------------------*'
           DISPLAY 'OLD TECH ', PREV-TECH
           DISPLAY 'NEW TECH ', EMP-TECH.
           MOVE EMP-TECH TO PREV-TECH.
       LOC-CHANGE-PARA.
           DISPLAY '*------------------*'
           DISPLAY 'OLD LOC ', PREV-LOC
           DISPLAY 'NEW LOC ', EMP-LOC.
           MOVE EMP-LOC TO PREV-LOC.
       TERM-PARA.
           DISPLAY 'END OF PROGRAM'
           CLOSE EMPLOYEE-FILE.

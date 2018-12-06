       IDENTIFICATION DIVISION.
       PROGRAM-ID. SORTPRG1.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLOYEE-FILE-IN ASSIGN TO 'EMPLOYEE.DAT'.
           SELECT EMPLOYEE-FILE-OUT ASSIGN TO 'EMPSORT.DAT'.
           SELECT SORT-WORK-FILE    ASSIGN TO 'SORTWOEK.DAT'.
       DATA DIVISION.
       FILE SECTION.
       FD  EMPLOYEE-FILE-IN.
       01  EMP-REC-IN.
           03  I-EMP-ID            PIC X(05).
           03  I-EMP-NAME          PIC X(15).
           03  I-EMP-LOC           PIC X(03).
           03  I-EMP-TECH          PIC X(05).
           03  FILLER              PIC X(22).
       FD  EMPLOYEE-FILE-OUT.
       01  EMP-REC-OUT.
           03  O-EMP-ID            PIC X(05).
           03  O-EMP-NAME          PIC X(15).
           03  O-EMP-LOC           PIC X(03).
           03  O-EMP-TECH          PIC X(05).
           03  FILLER              PIC X(22).
       SD  SORT-WORK-FILE.
       01  SORT-REC.
           03  S-EMP-ID            PIC X(05).
           03  S-EMP-NAME          PIC X(15).
           03  S-EMP-LOC           PIC X(03).
           03  S-EMP-TECH          PIC X(05).
           03  FILLER              PIC X(22).
       WORKING-STORAGE SECTION.
       01  WS-FILE-FLAG            PIC X(01) VALUE 'N'.
           88  END-OF-FILE                   VALUE 'Y'.
       01  WS-INPUT-LOC            PIC X(03) VALUE SPACES.
       01  WS-INPUT-TECH           PIC X(05) VALUE SPACES.
       PROCEDURE DIVISION.
       MAIN-PARA. 
           DISPLAY 'ENTER INPUT LOC ', WITH NO ADVANCING
           ACCEPT WS-INPUT-LOC
           DISPLAY 'ENTER OUTPUT TECH ', WITH NO ADVANCING
           ACCEPT WS-INPUT-TECH
      *     
           SORT SORT-WORK-FILE
      *     ON DESCENDING KEY S-EMP-LOC 
      *     ON ASCENDING KEY  S-EMP-TECH
           ON ASCENDING KEY S-EMP-ID    
           INPUT  PROCEDURE 1000-INPUT-PROCESS
           OUTPUT PROCEDURE 2000-OUTPUT-PROCESS
           STOP RUN.
       1000-INPUT-PROCESS.
           OPEN INPUT EMPLOYEE-FILE-IN
           PERFORM 1100-READ-EMPLOYEE
           DISPLAY '** INPUT SORT RECORDS **'
           PERFORM UNTIL END-OF-FILE
               IF I-EMP-LOC = WS-INPUT-LOC        
                   RELEASE SORT-REC FROM EMP-REC-IN
                   DISPLAY 'EMP ', EMP-REC-IN                   
               END-IF
               PERFORM 1100-READ-EMPLOYEE
           END-PERFORM
           CLOSE EMPLOYEE-FILE-IN.
       1100-READ-EMPLOYEE.
           READ EMPLOYEE-FILE-IN
               AT END
                   MOVE 'Y' TO WS-FILE-FLAG
           END-READ.
       2000-OUTPUT-PROCESS.
           MOVE 'N' TO WS-FILE-FLAG
           OPEN OUTPUT EMPLOYEE-FILE-OUT
           PERFORM 2100-READ-SORT
           DISPLAY '** SORTED OUTPUT RECORDS **'
           PERFORM  UNTIL  END-OF-FILE
               IF S-EMP-TECH = WS-INPUT-TECH
                   WRITE EMP-REC-OUT
                   DISPLAY 'EMP ', EMP-REC-OUT
               END-IF
               PERFORM 2100-READ-SORT
           END-PERFORM
           CLOSE EMPLOYEE-FILE-OUT.
       2100-READ-SORT.
           RETURN SORT-WORK-FILE RECORD INTO EMP-REC-OUT
               AT END
                   MOVE 'Y' TO WS-FILE-FLAG
           END-RETURN.
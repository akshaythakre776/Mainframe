       IDENTIFICATION DIVISION.
       PROGRAM-ID. CSRCHPRG.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLOYEE-FILE ASSIGN TO 'EMPLOYEE.DAT'
           ORGANIZATION IS SEQUENTIAL
           ACCESS MODE  IS SEQUENTIAL
           FILE STATUS  IS WS-FILE-STAT.
       DATA DIVISION.
       FILE SECTION.
       FD  EMPLOYEE-FILE
           RECORD CONTAINS 50 CHARACTERS.
       01  EMPLOYEE-RECORD.
           03  EMP-ID              PIC X(05).
           03  EMP-NAME            PIC X(15).
           03  EMP-LOC             PIC X(03).
           03  FILLER              PIC X(27).
       WORKING-STORAGE SECTION.
       01  WS-FILE-STAT            PIC X(02).
       01  WS-FILE-FLAG            PIC X(01) VALUE 'N'.
           88  END-OF-FILE         VALUE 'Y'.
       01  WS-LOC-NAME             PIC X(12).
       01  WS-PROG-NAME            PIC X(08) VALUE 'CLOCPROG'.
       01  WS-LOCATION-TABLE.
           03  LOCATION-TABLE      OCCURS 10 TIMES 
                   INDEXED BY LOC-IDX.
               05  LOC-CODE        PIC X(03).
               04  LOC-NAME        PIC X(12).
       PROCEDURE DIVISION.
       0000-MAIN-PARA.
           PERFORM 1000-INITIALIZATION-PARA.
           PERFORM 2000-PROCESS-PARA  UNTIL  END-OF-FILE
           PERFORM 9000-TERMINATION-PARA.
           STOP RUN.
       1000-INITIALIZATION-PARA.
           OPEN INPUT EMPLOYEE-FILE
           IF WS-FILE-STAT = '00'
               DISPLAY 'EMPLOYEE FILE OPENED SUCCESSFULLY'
           END-IF.
           DISPLAY '**----------------------**'
           DISPLAY 'BEFORE CALLING SUB-ROUTINE'
           CALL WS-PROG-NAME USING WS-LOCATION-TABLE.
           DISPLAY 'AFTER  CALLING SUB ROUTINE'
           DISPLAY WS-LOCATION-TABLE
           DISPLAY '**----------------------**'
           PERFORM 1500-READ-EMPLOYEE.
       2000-PROCESS-PARA.
           PERFORM 3000-SEARCH-LOCATION.
           DISPLAY 'DTLS= ', EMP-ID, EMP-NAME, WS-LOC-NAME 
           PERFORM 1500-READ-EMPLOYEE.
       1500-READ-EMPLOYEE.
           READ EMPLOYEE-FILE
               AT END
                   MOVE 'Y' TO WS-FILE-FLAG
           END-READ.              
       3000-SEARCH-LOCATION.
           SET LOC-IDX TO 1
           SEARCH LOCATION-TABLE
               AT END
                   MOVE 'INVALID LOC.' TO WS-LOC-NAME
               WHEN EMP-LOC = LOC-CODE(LOC-IDX)
                   MOVE LOC-NAME(LOC-IDX) TO WS-LOC-NAME
           END-SEARCH.
       9000-TERMINATION-PARA.
           DISPLAY '**----------------------------------**'
           CLOSE EMPLOYEE-FILE.

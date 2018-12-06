       IDENTIFICATION DIVISION.
       PROGRAM-ID. APROG5.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
           SELECT EMPLOYEE-FILE ASSIGN TO 'EMPLOYEE.DAT'
           ORGANIZATION IS SEQUENTIAL
           ACCESS MODE  IS SEQUENTIAL
           FILE STATUS  IS WS-FILE-STAT1.
           
           SELECT REPORT-FILE ASSIGN TO 'REPORT.DAT'
           ORGANIZATION IS SEQUENTIAL
           ACCESS MODE  IS SEQUENTIAL
           FILE STATUS  IS WS-FILE-STAT2.
       DATA DIVISION.
       FILE SECTION.
       FD  EMPLOYEE-FILE
           RECORD CONTAINS 50 CHARACTERS.
       01  EMPLOYEE-RECORD.
           03  EMP-ID              PIC X(05).
           03  EMP-NAME            PIC X(20).
           03  EMP-CITY            PIC X(03).
           03  EMP-DOB.
                05  EMP-YR         PIC 9(04).
                05  EMP-MM         PIC 9(02).
                05  EMP-DD         PIC 9(02).
           03  EMP-EARN            PIC 9(5)V99.
           03  EMP-DEDN            PIC 9(5)V99.           
       FD  REPORT-FILE
           RECORD CONTAINS 80 CHARACTERS.
       01  REPORT-RECORD           PIC X(80).
       WORKING-STORAGE SECTION.
       01  WS-FILE-STAT1           PIC X(02).
       01  WS-FILE-STAT2           PIC X(02).
       01  WS-FILE-FLAG            PIC X(01) VALUE 'N'.
           88  END-OF-FILE         VALUE 'Y'.
       01  WS-DATE.
           03  WS-DATE-YR          PIC 9(04).
           03  WS-DATE-MTH         PIC 9(02).
           03  WS-DATE-DD          PIC 9(02).
       01  WS-TIME                 PIC 9(08).
           03  WS-TIME-HH          PIC 9(02).
           03  WS-TIME-MM          PIC 9(02).
           03  WS-TIME-SS          PIC 9(02).
           03  WS-TIME-FS          PIC 9(02).
       01  HEADING-LINE1.
           03  FILLER              PIC X(06) VALUE 'DATE:'.
           03  OUT-DATE.
                05  WS-DATE-DD     PIC 9(02) VALUE ZERO.
                05  FILLER         PIC X(01) VALUE '/'.
                05  WS-DATE-MTH    PIC 9(02) VALUE ZERO.
                05  FILLER         PIC X(01) VALUE '/'.
                05  WS-DATE-YR     PIC 9(04) VALUE ZERO.
           03  FILLER              PIC X(50) VALUE SPACES.
           03  FILLER              PIC X(06) VALUE 'TIME:'.
           03  OUT-TIME.
                05  WS-TIME-HH     PIC 9(02) VALUE ZERO.
                05  FILLER         PIC X(01) VALUE ':'.
                05  WS-TIME-MM     PIC 9(02) VALUE ZERO.
                05  FILLER         PIC X(01) VALUE ':'.
                05  WS-TIME-SS     PIC 9(02) VALUE ZERO.
       01  HEADING-LINE2.
           03  FILLER              PIC X(20) VALUE
                'LISTING OF EMPLOYEES'.
           03  FILLER              PIC X(46) VALUE SPACES.
           03  FILLER              PIC X(06) VALUE 'PAGE:'.
           03  OUT-PAGE            PIC Z9 VALUE ZERO.
       01  HEADING-LINE3.
           03  FILLER              PIC X(07) VALUE 'ID'.
           03  FILLER              PIC X(22) VALUE 'EMP NAME'.
           03  FILLER              PIC X(05) VALUE 'LOC'.
           03  FILLER              PIC X(12) VALUE 'BIRTH DATE'.
           03  FILLER              PIC X(10) VALUE ' EARNINGS'.
           03  FILLER              PIC X(12) VALUE 'DEDUCTIONS'.
           03  FILLER              PIC X(09) VALUE 'TOTAL SAL'.
       01  DETAIL-LINE.
           03  OUT-EMP-ID          PIC X(05) VALUE SPACES.
           03  FILLER              PIC X(02) VALUE SPACES.
           03  OUT-EMP-NAME        PIC X(20) VALUE SPACES.
           03  FILLER              PIC X(02) VALUE SPACES.
           03  OUT-EMP-CITY        PIC X(03) VALUE SPACES.
           03  FILLER              PIC X(02) VALUE SPACES.
           03  OUT-EMP-DOB.
                05  EMP-DD         PIC 9(02) VALUE ZERO.
                05  FILLER         PIC X(01) VALUE '/'.
                05  EMP-MM         PIC 9(02) VALUE ZERO.
                05  FILLER         PIC X(01) VALUE '/'.
                05  EMP-YR         PIC 9(04) VALUE ZERO.
           03  FILLER              PIC X(02) VALUE SPACES.
           03  OUT-EMP-EARN        PIC ZZ,ZZ9.99 VALUE ZERO.
           03  FILLER              PIC X(02) VALUE SPACES.
           03  OUT-EMP-DEDN        PIC ZZ,ZZ9.99 VALUE ZERO.
           03  FILLER              PIC X(02) VALUE SPACES.
           03  OUT-EMP-SAL         PIC ZZ,ZZ9.99 VALUE ZERO.
       01  WS-VARIABLES.
           03  PAGE-COUNT          PIC 9(02) VALUE ZERO.
           03  LINE-COUNT          PIC 9(02) VALUE 10.
       PROCEDURE DIVISION.
       0000-MAIN-PARA.
           PERFORM 1000-INITIALIZATION-PARA.
           PERFORM 2000-PROCESS-PARA  UNTIL  END-OF-FILE
           PERFORM 9000-TERMINATION-PARA.
           STOP RUN.
       1000-INITIALIZATION-PARA.
           OPEN INPUT EMPLOYEE-FILE
           IF WS-FILE-STAT1 = '00'
               DISPLAY 'EMPLOYEE FILE OPENED SUCCESSFULLY'
           END-IF.
           OPEN OUTPUT REPORT-FILE
           IF WS-FILE-STAT2 = '00'
               DISPLAY 'REPORT FILE OPENED SUCCESSFULLY'
           END-IF.
           PERFORM 1100-ACCEPT-DATE-TIME.
           PERFORM 1500-READ-PARA.
       1100-ACCEPT-DATE-TIME.
           ACCEPT WS-DATE FROM DATE YYYYMMDD
           ACCEPT WS-TIME FROM TIME.
           MOVE CORRESPONDING WS-DATE TO OUT-DATE
           MOVE CORRESPONDING WS-TIME TO OUT-TIME.
       1500-READ-PARA.
           READ EMPLOYEE-FILE
               AT END
                   MOVE 'Y' TO WS-FILE-FLAG
               NOT AT END
                   ADD 1 TO WS-TOT-EMP
           END-READ.              
       2000-PROCESS-PARA.
           PERFORM 2500-MOVE-PARA
           IF LINE-COUNT > 9
               PERFORM 2200-HEADING-PARA
           END-IF
           WRITE REPORT-RECORD FROM DETAIL-LINE.
           ADD 1 TO LINE-COUNT
           PERFORM 1500-READ-PARA.
       2200-HEADING-PARA.           
           ADD 1 TO PAGE-COUNT
           MOVE PAGE-COUNT TO OUT-PAGE
           WRITE REPORT-RECORD FROM HEADING-LINE1
           WRITE REPORT-RECORD FROM HEADING-LINE2
           WRITE REPORT-RECORD FROM HEADING-LINE3
           MOVE ZERO TO LINE-COUNT.
       2500-MOVE-PARA.
           MOVE EMP-ID     TO OUT-EMP-ID
           MOVE EMP-NAME   TO OUT-EMP-NAME
           MOVE EMP-CITY   TO OUT-EMP-CITY
           MOVE CORRESPONDING EMP-DOB TO OUT-EMP-DOB
           MOVE EMP-EARN   TO OUT-EMP-EARN
           MOVE EMP-DEDN   TO OUT-EMP-DEDN
           COMPUTE EMP-SAL = EMP-EARN - EMP-DEDN
           MOVE EMP-SAL    TO OUT-EMP-SAL.
           
           
                 
          
           

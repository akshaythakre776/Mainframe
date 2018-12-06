       IDENTIFICATION DIVISION.
       PROGRAM-ID. READVSM2.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLOYEE-MASTER ASSIGN TO 'EMPMAST.DAT'
       	ORGANIZATION IS INDEXED
       	ACCESS MODE  IS RANDOM
       	RECORD KEY   IS EMP-ID
       	FILE STATUS  IS WS-FILE-STAT1.
           SELECT REPORT-FILE ASSIGN TO 'REPORT.DAT'
           ORGANIZATION IS SEQUENTIAL
           ACCESS MODE  IS SEQUENTIAL
           FILE STATUS  IS WS-FILE-STAT2.
       DATA DIVISION.
       FILE SECTION.
       FD  EMPLOYEE-MASTER.
       01  EMPLOYEE-RECORD.
           03  EMP-ID              PIC X(05).
           03  EMP-NAME            PIC X(15).
           03  EMP-LOC             PIC X(03).
           03  EMP-TECH            PIC X(05).
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
       01  WS-FILE-STAT1	PIC X(02).
       01  WS-FILE-STAT2	PIC X(02).
       01  EMP-SAL         PIC 9(05)V99 VALUE ZERO.
       01  WS-EMP-ID		PIC X(05).
           88  END-OF-DATA VALUE 'XXXXX'.
       01  WS-EMP-FLAG		PIC X(01) VALUE 'N'.
           88  EMP-FOUND             VALUE 'Y'.
       01  DETAIL-LINE.
           03  OUT-EMP-ID          PIC X(05) VALUE SPACES.
           03  FILLER              PIC X(01) VALUE SPACES.
           03  OUT-EMP-NAME        PIC X(15) VALUE SPACES.
           03  FILLER              PIC X(01) VALUE SPACES.
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
       PROCEDURE DIVISION.
       MAIN-PARA.
           OPEN INPUT EMPLOYEE-MASTER
               OUTPUT REPORT-FILE
           PERFORM ACCEPT-PARA.           
           PERFORM UNTIL  END-OF-DATA       
               PERFORM READ-EMPLOYEE
               PERFORM PROCESS-EMPLOYEE
               PERFORM ACCEPT-PARA
           END-PERFORM.           
           CLOSE EMPLOYEE-MASTER
                 REPORT-FILE
           STOP RUN.
       ACCEPT-PARA.
           DISPLAY 'ENTER EMP ID ', WITH NO ADVANCING
           ACCEPT WS-EMP-ID.
           IF END-OF-DATA
               DISPLAY 'END OF DATA REACHED'
           END-IF.
       READ-EMPLOYEE.
           MOVE WS-EMP-ID TO EMP-ID
           READ EMPLOYEE-MASTER
           	KEY IS EMP-ID
       	       INVALID KEY
                    MOVE 'N' TO WS-EMP-FLAG
                  NOT INVALID KEY
                    MOVE 'Y' TO WS-EMP-FLAG
           END-READ
           DISPLAY '*--------*'
           DISPLAY 'READ FS ', WS-FILE-STAT1.
       PROCESS-EMPLOYEE.
           IF EMP-FOUND
               PERFORM MOVE-PARA
           ELSE
               MOVE SPACES          TO DETAIL-LINE
               MOVE WS-EMP-ID       TO OUT-EMP-ID
               MOVE 'EMP NOT FOUND' TO OUT-EMP-NAME
           END-IF
           WRITE REPORT-RECORD     FROM DETAIL-LINE.
       MOVE-PARA.    
           MOVE EMP-ID     TO OUT-EMP-ID
           MOVE EMP-NAME   TO OUT-EMP-NAME
           MOVE CORRESPONDING EMP-DOB TO OUT-EMP-DOB
           MOVE EMP-EARN   TO OUT-EMP-EARN
           MOVE EMP-DEDN   TO OUT-EMP-DEDN
           COMPUTE EMP-SAL = EMP-EARN - EMP-DEDN
           MOVE EMP-SAL    TO OUT-EMP-SAL.
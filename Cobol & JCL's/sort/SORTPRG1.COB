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
       PROCEDURE DIVISION.
       MAIN-PARA. 
           SORT SORT-WORK-FILE
           ON DESCENDING KEY S-EMP-LOC 
           ON ASCENDING KEY  S-EMP-TECH
           ON DESCENDING KEY S-EMP-ID    
           USING EMPLOYEE-FILE-IN GIVING EMPLOYEE-FILE-OUT.
           STOP RUN.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SPROG1.
       DATA DIVISION.
       LINKAGE SECTION.
       01  LK-F1   PIC 99.
       01  LK-F2   PIC 99.
       01  LK-OUT  PIC 999.
       PROCEDURE DIVISION USING LK-F1, LK-F2, LK-OUT.
       MAIN-PARA.
           DISPLAY '**----------**'
           DISPLAY 'IN SUB PROGRAM'
           COMPUTE LK-OUT = LK-F1 + LK-F2
           DISPLAY 'AFTER COMPUTE'.
           GOBACK.

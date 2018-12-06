       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG2.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  INPUT-FIELD     PIC X(10).
       PROCEDURE DIVISION.
           ACCEPT INPUT-FIELD
           DISPLAY '*--------*'
           DISPLAY INPUT-FIELD
           DISPLAY '*--------*'
           STOP RUN.
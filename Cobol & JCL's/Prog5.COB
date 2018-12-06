       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG5.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  INPUT-FIELDS.
           03  INPUT-FIELD1    PIC X(10).
           03  INPUT-FIELD2    PIC X(10).
           03  INPUT-FIELD3    PIC X(10) VALUE 'CAP GEMINI'.
           03  INPUT-FIELD4    PIC 9(05) VALUE 10101.
       PROCEDURE DIVISION.
           DISPLAY 'ENTER FIELD-1 ' WITH NO ADVANCING
           ACCEPT INPUT-FIELD1
           DISPLAY 'ENTER FIELD-2 ' WITH NO ADVANCING
           ACCEPT INPUT-FIELD2
           DISPLAY '*--------*'
           DISPLAY INPUT-FIELD1
           DISPLAY '*--------*'
           DISPLAY INPUT-FIELD2
           DISPLAY '*--------*'
           DISPLAY INPUT-FIELD1, ' ', INPUT-FIELD2
           DISPLAY '*--------*'
           DISPLAY INPUT-FIELD3, ' ', INPUT-FIELD4
           DISPLAY '*--------*', ' ', 'hard coded literal'
           DISPLAY '*--------*', ' ', 12345
           STOP RUN.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG4.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  INPUT-FIELDS.
           03  INPUT-FIELD1    PIC X(10).
           03  INPUT-FIELD2    PIC X(10).
       PROCEDURE DIVISION.
      *     DISPLAY 'ENTER FIELD-1 ' WITH NO ADVANCING
      *     ACCEPT INPUT-FIELD1
      *     DISPLAY 'ENTER FIELD-2 ' WITH NO ADVANCING
      *     ACCEPT INPUT-FIELD2
           DISPLAY 'ENTER INPUT FIELDS ' WITH NO ADVANCING
           ACCEPT INPUT-FIELDS
           DISPLAY '*--------*'
           DISPLAY INPUT-FIELD1
           DISPLAY '*--------*'
           DISPLAY INPUT-FIELD2
           DISPLAY '*--------*'
           DISPLAY INPUT-FIELD1, ' ', INPUT-FIELD2
           DISPLAY '*--------*'
           DISPLAY INPUT-FIELD1, INPUT-FIELD2
           DISPLAY '*--------*'
           STOP RUN.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG7.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  INPUT-FIELDS.
           03  A       PIC 9(02).
           03  B       PIC 9(02).
           03  C       PIC 9(03).
           03  D       PIC S9(02).
           03  E       PIC 9(04).
           03  F       PIC 9(02)V99.
       01  OUTPUT-FIELDS.
           03  C-O1    PIC ZZ9.
           03  C-O2    PIC $999.
           03  C-O3    PIC $$$9.
           03  D-O1    PIC -9(02).
           03  D-O2    PIC --9.
           03  E-O1    PIC ZZZ9.
           03  E-O2    PIC ***9.
           03  E-O3    PIC Z,ZZ9.
           03  F-O     PIC 9(02).99.
       PROCEDURE DIVISION.
           DISPLAY 'ENTER FIELD-1 ' WITH NO ADVANCING
           ACCEPT A
           DISPLAY 'ENTER FIELD-2 ' WITH NO ADVANCING
           ACCEPT B
           DISPLAY '*--------*'
           DISPLAY A, ' ' B
           DISPLAY '*--------*'
           ADD A B GIVING C
           SUBTRACT A FROM B GIVING D
           MULTIPLY A BY B GIVING E
           IF B > ZERO
               DIVIDE B INTO A GIVING F
           ELSE    
               MOVE ZERO TO F
               DISPLAY 'DIVISION BY ZERO NOT POSSIBLE'
           END-IF    
           MOVE C TO C-O1, C-O2, C-O3
           IF D IS POSITIVE
               DISPLAY 'SUBTRACTION IS POSITIVE'
           ELSE
               DISPLAY 'SUBTRACTION IS NEGATIVE'
           END-IF
           MOVE D TO D-O1, D-O2
           MOVE E TO E-O1, E-O2, E-O3
           IF F > ZERO
               DISPLAY 'DIVISION IS > ZERO'
           ELSE
               DISPLAY 'DIVISION IS < ZERO'
           END-IF
           MOVE F TO F-O
           DISPLAY 'ADD  = ', C-O1, ' ', C-O2, ' ', C-O3
           DISPLAY 'SUB  = ', D-O1, ' ', D-O2
           DISPLAY 'MULT = ', E-O1, ' ', E-O2, ' ', E-O3
           DISPLAY 'DIV  = ', F-O
           DISPLAY '*--------*'
           STOP RUN.

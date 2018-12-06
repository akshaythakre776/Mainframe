       IDENTIFICATION DIVISION.
       PROGRAM-ID. APROG6.
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
       MAIN-PARA.            
           DISPLAY 'IN MAIN-PARA'
           PERFORM ACCEPT-PARA.
           IF A > ZERO AND B > ZERO
                PERFORM PARA-ADD
                PERFORM PARA-SUBTRACT
                PERFORM PARA-MULTIPLY
                PERFORM PARA-DIVIDE
           ELSE
                DISPLAY 'INPUTS ARE NOT > ZERO'
           END-IF
           STOP RUN.
       ACCEPT-PARA.
           DISPLAY 'IN ACCEPT-PARA'
           DISPLAY 'ENTER FIELD-1 ' WITH NO ADVANCING
           ACCEPT A
           DISPLAY 'ENTER FIELD-2 ' WITH NO ADVANCING
           ACCEPT B
           DISPLAY '*--------*'
           DISPLAY 'INPUTS ARE ',  A, ' ' B
           DISPLAY '*--------*'.
       PARA-ADD.          
           DISPLAY 'IN PARA-ADD'
           ADD A B GIVING C
           MOVE C TO C-O1, C-O2, C-O3
           DISPLAY 'ADD  = ', C-O1, ' ', C-O2, ' ', C-O3
           DISPLAY '*--------*'.
       PARA-SUBTRACT.
           DISPLAY 'IN PARA-SUBTRACT'
           SUBTRACT A FROM B GIVING D
           MOVE D TO D-O1, D-O2
           DISPLAY 'SUB  = ', D-O1, ' ', D-O2
           DISPLAY '*--------*'.
       PARA-MULTIPLY.
           DISPLAY 'IN PARA-MULTIPLY'
           MULTIPLY A BY B GIVING E
           MOVE E TO E-O1, E-O2, E-O3
           DISPLAY 'MULT = ', E-O1, ' ', E-O2, ' ', E-O3
           DISPLAY '*--------*'.
       PARA-DIVIDE.
           DISPLAY 'IN PARA-DIVIDE'
           DIVIDE B INTO A GIVING F
           MOVE F TO F-O
           DISPLAY 'DIV  = ', F-O
           DISPLAY '*--------*'.

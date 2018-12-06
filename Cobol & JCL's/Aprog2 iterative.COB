       IDENTIFICATION DIVISION.
       PROGRAM-ID. APROG2.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  OUTPUT-FIELDS.
           03  C       PIC 9(03).
           03  D       PIC S9(02).
           03  E       PIC 9(04).
           03  F       PIC 9(02)V99.
       01  FORMAT-FIELDS.
           03  C-O1    PIC ZZ9.
           03  C-O2    PIC $999.
           03  C-O3    PIC $$$9.
           03  D-O1    PIC -9(02).
           03  D-O2    PIC --9.
           03  E-O1    PIC ZZZ9.
           03  E-O2    PIC ***9.
           03  E-O3    PIC Z,ZZ9.
           03  F-O     PIC 9(02).99.
       01  INPUT-FIELDS.
           88  END-OF-DATA VALUE 'XXXX'.
           03  A       PIC 9(02).
           03  B       PIC 9(02).
       PROCEDURE DIVISION.
       MAIN-PARA.            
           INITIALIZE INPUT-FIELDS
           PERFORM ACCEPT-PARA
           PERFORM  UNTIL  END-OF-DATA
               IF INPUT-FIELDS IS NOT NUMERIC
                   DISPLAY 'INPUTS ARE NOT NUMERIC'
               ELSE    
                   IF A > ZERO AND B > ZERO
                       PERFORM PARA-ADD       
                       PERFORM PARA-SUBTRACT  
                       PERFORM PARA-MULTIPLY  
                       PERFORM PARA-DIVIDE    
                   ELSE
                       DISPLAY 'INPUTS ARE NOT > ZERO'
                   END-IF    
               END-IF
               PERFORM ACCEPT-PARA
           END-PERFORM        
           STOP RUN.
       ACCEPT-PARA.
           DISPLAY 'ENTER FIELDS ' WITH NO ADVANCING
           ACCEPT INPUT-FIELDS
           IF INPUT-FIELDS = 'XXXX'
               DISPLAY 'END OF DATA'
           ELSE    
               DISPLAY '*--------*'
               DISPLAY 'INPUTS ARE ',  A, ' ' B
               DISPLAY '*--------*'
           END-IF.
       PARA-ADD.          
           ADD A B GIVING C
           MOVE C TO C-O1, C-O2, C-O3
           DISPLAY 'ADD  = ', C-O1, ' ', C-O2, ' ', C-O3
           DISPLAY '*--------*'.
       PARA-SUBTRACT.
           SUBTRACT A FROM B GIVING D
           MOVE D TO D-O1, D-O2
           DISPLAY 'SUB  = ', D-O1, ' ', D-O2
           DISPLAY '*--------*'.
       PARA-MULTIPLY.
           MULTIPLY A BY B GIVING E
           MOVE E TO E-O1, E-O2, E-O3
           DISPLAY 'MULT = ', E-O1, ' ', E-O2, ' ', E-O3
           DISPLAY '*--------*'.
       PARA-DIVIDE.
           DIVIDE B INTO A GIVING F
           MOVE F TO F-O
           DISPLAY 'DIV  = ', F-O
           DISPLAY '*--------*'.

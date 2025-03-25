       IDENTIFICATION DIVISION.
       PROGRAM-ID. EX07.
      *ESTE PROGRAMA FOI DESENVOLVIDO EM LINUX E COMPILADO USANDO GNUCBL
       AUTHOR. Matheus Souza Tertuliano.
       DATE-WRITTEN. 21/03/2025.
       DATE-COMPILED.
       SECURITY. APENAS O AUTOR PODE MODIFICA-LO.


       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. MY-PC.
       OBJECT-COMPUTER. MY-PC.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CADALU ASSIGN "CADALU.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT CADATU ASSIGN "CADATU.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD CADALU.
       01 DADOS-IN.
           05 NUMERO-IN       PIC 9(5).
           05 NOME-IN         PIC X(20).
           05 NOTA1-IN        PIC 9(2)V99.
           05 NOTA2-IN        PIC 9(2)V99.
           05 NOTA3-IN        PIC 9(2)V99.
           05 NOTA4-IN        PIC 9(2)V99.
           05 SEXO-IN         PIC X(1).

       FD CADATU.
       01 DADOS-OUT.
           05 NUMERO-OUT       PIC 9(5).
           05 NOME-OUT         PIC X(20).
           05 MEDIA-OUT        PIC 9(2)V99.
           05 SEXO-OUT         PIC X(1).

       WORKING-STORAGE SECTION.
       01 FIMARQ PIC X(1) VALUE "N".
       01 MEDIA PIC 9(2)V99.

       PROCEDURE DIVISION.
       INICIO.
           OPEN INPUT CADALU.
           OPEN OUTPUT CADATU.

           PERFORM UNTIL FIMARQ = "S"
               READ CADALU INTO DADOS-IN
                   AT END 
                       MOVE "S" TO FIMARQ
                   NOT AT END
                       COMPUTE MEDIA = 
                       (NOTA1-IN + NOTA2-IN + NOTA3-IN + NOTA4-IN) / 4
                       IF SEXO-IN = "F"
                           MOVE NUMERO-IN TO NUMERO-OUT
                           MOVE NOME-IN TO NOME-OUT
                           MOVE MEDIA TO MEDIA-OUT
                           MOVE SEXO-IN TO SEXO-OUT
                           WRITE DADOS-OUT
                       END-IF
               END-READ
           END-PERFORM

           CLOSE CADALU.
           CLOSE CADATU.
           STOP RUN.
           
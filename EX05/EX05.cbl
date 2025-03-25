       IDENTIFICATION DIVISION.
       PROGRAM-ID. EX05.
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
           SELECT CADAPR ASSIGN "CADAPR.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD CADALU.
       01 DADOS-IN.
           05 NUMERO-IN       PIC 9(5).
           05 NOME-IN         PIC X(20).
           05 NOTA1-IN        PIC 9(2)V99.
           05 NOTA2-IN        PIC 9(2)V99.
           05 FALTAS-IN       PIC 9(2).

       FD CADAPR.
       01 DADOS-OUT.
           05 NUMERO-OUT       PIC 9(5).
           05 NOME-OUT         PIC X(20).
           05 MEDIA-OUT        PIC 9(2)V99.

       WORKING-STORAGE SECTION.
       01 MEDIA PIC 9(2)V99.
       01 FIMARQ PIC X(1) VALUE "N".

       PROCEDURE DIVISION.
       INICIO.
           OPEN INPUT CADALU.
           OPEN OUTPUT CADAPR.

           PERFORM UNTIL FIMARQ = "S"
               READ CADALU AT END
                   MOVE "S" TO FIMARQ
               NOT AT END
                   COMPUTE MEDIA = (NOTA1-IN + NOTA2-IN) / 2
                   IF MEDIA >= 7 AND FALTAS-IN <= 18
                       MOVE NUMERO-IN TO NUMERO-OUT
                       MOVE NOME-IN TO NOME-OUT
                       MOVE MEDIA TO MEDIA-OUT
                       WRITE DADOS-OUT
                   END-IF
               END-READ
           END-PERFORM

           CLOSE CADALU.
           CLOSE CADAPR.
           STOP RUN.

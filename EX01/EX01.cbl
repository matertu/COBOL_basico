       IDENTIFICATION DIVISION.
      *ESTE PROGRAMA FOI DESENVOLVIDO EM LINUX E COMPILADO USANDO GNUCBL
       PROGRAM-ID. EX01.
       AUTHOR. Matheus.
       DATE-WRITTEN. 17/03/2025.
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
           SELECT CADASTRO-INT ASSIGN TO "CADCLI1.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT CADASTRO-OUT ASSIGN TO "CADCLI2.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD CADASTRO-INT.
       01 DADOS-INT.
           05 CODIGO-INT   PIC X(5).
           05 NOME-INT     PIC X(20).

       FD CADASTRO-OUT.
       01 DADOS-OUT.
           05 NOME-OUT         PIC X(20).
           05 CODIGO-OUT       PIC X(5).

       WORKING-STORAGE SECTION.
       01 FIMARQ PIC X(1) VALUE "N".

       PROCEDURE DIVISION.
       INICIO.
           OPEN INPUT CADASTRO-INT
           OPEN OUTPUT CADASTRO-OUT

           PERFORM UNTIL FIMARQ = "S"
               READ CADASTRO-INT AT END
                   MOVE "S" TO FIMARQ
               NOT AT END
                   MOVE NOME-INT TO NOME-OUT
                   MOVE CODIGO-INT TO CODIGO-OUT
                   WRITE DADOS-OUT
               END-READ
           END-PERFORM

           CLOSE CADASTRO-INT
           CLOSE CADASTRO-OUT
           STOP RUN.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. EX06.
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
           SELECT CADENT ASSIGN TO "CADENT.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT CADSAI ASSIGN TO "CADSAI.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD CADENT.
       01 DADOS-IN.
           05 NUMERO-IN     PIC 9(5).
           05 NOME-IN       PIC X(20).
           05 SALARIO-IN    PIC 9(5)V99.

       FD CADSAI.
       01 DADOS-OUT.
           05 NUMERO-OUT    PIC 9(5).
           05 NOME-OUT      PIC X(20).
           05 SALARIO-OUT  PIC 9(5)V99.

       WORKING-STORAGE SECTION.
       01 FIMARQ         PIC X(1) VALUE "N".

       PROCEDURE DIVISION.
       INICIO.
           OPEN INPUT CADENT.
           OPEN OUTPUT CADSAI.
           PERFORM UNTIL FIMARQ = "S"
               READ CADENT
                   AT END MOVE "S" TO FIMARQ
                   NOT AT END
                   IF SALARIO-IN > 3000
                       MOVE NUMERO-IN TO NUMERO-OUT
                       MOVE NOME-IN TO NOME-OUT
                       MOVE SALARIO-IN TO SALARIO-OUT
                       WRITE DADOS-OUT
               END-READ
           END-PERFORM

           CLOSE CADENT.
           CLOSE CADSAI.
           STOP RUN.

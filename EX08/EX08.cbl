       IDENTIFICATION DIVISION.
       PROGRAM-ID. EX08.
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
           SELECT CADFUN ASSIGN TO "CADFUN.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT CADSAI ASSIGN TO "CADSAI.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  CADFUN.
       01  DADOS-IN.
           05 NUMERO-IN     PIC 9(5).
           05 NOME-IN       PIC X(20).
           05 SALARIOBRUTO  PIC 9(5)V99.

       FD  CADSAI.
       01  DADOS-OUT.
           05 NUMERO-OUT    PIC 9(5).
           05 NOME-OUT      PIC X(20).
           05 SALARIOLIQ-OUT PIC 9(5)V99.

       WORKING-STORAGE SECTION.
       01  SALARIOLIQ     PIC 9(5)V99.
       01  FIMARQ         PIC X(1) VALUE "N".

       PROCEDURE DIVISION.
       INICIO.
           OPEN INPUT CADFUN
           OPEN OUTPUT CADSAI

           PERFORM UNTIL FIMARQ = "S"
               READ CADFUN INTO DADOS-IN
                   AT END
                       MOVE "S" TO FIMARQ
                   NOT AT END
                       IF SALARIOBRUTO <= 1000
                           COMPUTE SALARIOLIQ = SALARIOBRUTO*(112 / 100)
                           PERFORM CRIA-ARQUIVO
                       END-IF
                       IF SALARIOBRUTO > 1000 AND SALARIOBRUTO <= 2000
                           COMPUTE SALARIOLIQ = SALARIOBRUTO*(111 / 100)
                           PERFORM CRIA-ARQUIVO
                       END-IF
                       IF SALARIOBRUTO > 2000
                           COMPUTE SALARIOLIQ = SALARIOBRUTO*(110 / 100)
                           PERFORM CRIA-ARQUIVO
                       END-IF
               END-READ
           END-PERFORM

           CLOSE CADFUN
           CLOSE CADSAI
           STOP RUN.

       CRIA-ARQUIVO.
           MOVE NUMERO-IN TO NUMERO-OUT
           MOVE NOME-IN TO NOME-OUT
           MOVE SALARIOLIQ TO SALARIOLIQ-OUT
           WRITE DADOS-OUT

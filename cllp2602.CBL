      *===============================================================*
       IDENTIFICATION DIVISION.
      *===============================================================*
       PROGRAM-ID. CLLP2602.
       AUTHOR.     FRANCISCO FREIRE.
      *===============================================================*
      *                   I N F O S E R V E R                         *
      *---------------------------------------------------------------*
      *                                                               *
      *      PROGRAMA     : CLLP2602                                  *
      *      ANALISTA     : FRANCISCO FREIRE - BRQ IT SERVICES        *
      *      PROGRAMADOR  : FRANCISCO FREIRE - BRQ IT SERVICES        *
      *      DATA         : 03/10/2013                                *
      *                                                               *
      *      OBJETIVO     : VERIFICAR RESTRICAO POR AVISO NA TABELA   *
      *                     LPCLB037.                                 *
      *                                                               *
      *      ARQUIVOS:                                                *
      *         DDNAME                           INCLUDE/BOOK         *
      *         ARQDB22S                           I#CLLPKA.          *
      *         TABELA37                                              *
      *         BLQREST                                               *
      *                                                               *
      *===============================================================*
           EJECT
      *===============================================================*
       ENVIRONMENT DIVISION.
      *===============================================================*

      *---------------------------------------------------------------*
       CONFIGURATION SECTION.
      *---------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

           EJECT
      *---------------------------------------------------------------*
       INPUT-OUTPUT SECTION.
      *---------------------------------------------------------------*

       FILE-CONTROL.


      *===============================================================*
       DATA DIVISION.
      *===============================================================*

      *---------------------------------------------------------------*
       FILE SECTION.
      *---------------------------------------------------------------*


      *---------------------------------------------------------------*
       WORKING-STORAGE SECTION.
      *---------------------------------------------------------------*

       01  WRK-CARTEIRA                PIC X(03)        VALUE 'C1A'.

       01  WRK-CARTEIRA-R              REDEFINES WRK-CARTEIRA.
           03  WRK-CART-3              PIC X(01).
           03  WRK-CART-2              PIC X(01).
           03  WRK-CART-1              PIC X(01).

       01  WRK-CARTEIRA-CONV           PIC 9(13)        VALUE ZEROS.

       01  WRK-EVALUTE-IN              PIC X(01)        VALUE SPACES.
       01  WRK-CARTEIRA-OUT-1          PIC 9(02)        VALUE ZEROS.
       01  WRK-CARTEIRA-OUT-2          PIC 9(02)        VALUE ZEROS.


       01  WRK-BYTE-6                  PIC 9(02)        VALUE ZEROS.
       01  WRK-BYTE-5                  PIC 9(02)        VALUE ZEROS.
       01  WRK-BYTE-4                  PIC 9(02)        VALUE ZEROS.
       01  WRK-BYTE-3                  PIC 9(02)        VALUE ZEROS.
       01  WRK-BYTE-2                  PIC 9(02)        VALUE ZEROS.
       01  WRK-BYTE-1                  PIC 9(02)        VALUE ZEROS.

      *===============================================================*
       PROCEDURE      DIVISION.
      *===============================================================*

      *---------------------------------------------------------------*
       0900-INICIAR SECTION.
      *---------------------------------------------------------------*

           MOVE WRK-CART-3 TO WRK-EVALUTE-IN
           PERFORM 1000-EVALUATE-NUMERO
           MOVE WRK-CARTEIRA-OUT-2 TO WRK-BYTE-6
           MOVE WRK-CARTEIRA-OUT-1 TO WRK-BYTE-5


           MOVE WRK-CART-2 TO WRK-EVALUTE-IN
           PERFORM 1000-EVALUATE-NUMERO
           MOVE WRK-CARTEIRA-OUT-2 TO WRK-BYTE-4
           MOVE WRK-CARTEIRA-OUT-1 TO WRK-BYTE-3

           MOVE WRK-CART-1 TO WRK-EVALUTE-IN
           PERFORM 1000-EVALUATE-NUMERO
           MOVE WRK-CARTEIRA-OUT-2 TO WRK-BYTE-2
           MOVE WRK-CARTEIRA-OUT-1 TO WRK-BYTE-1

           COMPUTE WRK-CARTEIRA-CONV = (WRK-BYTE-6 * 16 ** 5) +
                                       (WRK-BYTE-5 * 16 ** 4) +
                                       (WRK-BYTE-4 * 16 ** 3) +
                                       (WRK-BYTE-3 * 16 ** 2) +
                                       (WRK-BYTE-2 * 16 ** 1) +
                                       (WRK-BYTE-1 * 16 ** 0)

           DISPLAY WRK-CARTEIRA-CONV.
      *---------------------------------------------------------------*
       0900-99-FIM. EXIT.
      *---------------------------------------------------------------*


      *---------------------------------------------------------------*
       1000-EVALUATE-NUMERO SECTION.
      *---------------------------------------------------------------*

           EVALUATE WRK-EVALUTE-IN
      *        C1
               WHEN 'A'
                   MOVE 12 TO WRK-CARTEIRA-OUT-2
                   MOVE 1  TO WRK-CARTEIRA-OUT-1

      *        C2
               WHEN 'B'
                   MOVE 12 TO WRK-CARTEIRA-OUT-2
                   MOVE 2  TO WRK-CARTEIRA-OUT-1

      *        C3
               WHEN 'C'
                   MOVE 12 TO WRK-CARTEIRA-OUT-2
                   MOVE 3  TO WRK-CARTEIRA-OUT-1

      *        C4
               WHEN 'D'
                   MOVE 12 TO WRK-CARTEIRA-OUT-2
                   MOVE 4  TO WRK-CARTEIRA-OUT-1

      *        C5
               WHEN 'E'
                   MOVE 12 TO WRK-CARTEIRA-OUT-2
                   MOVE 5  TO WRK-CARTEIRA-OUT-1

      *        C6
               WHEN 'F'
                   MOVE 12 TO WRK-CARTEIRA-OUT-2
                   MOVE 6  TO WRK-CARTEIRA-OUT-1

      *        C7
               WHEN 'G'
                   MOVE 12 TO WRK-CARTEIRA-OUT-2
                   MOVE 7  TO WRK-CARTEIRA-OUT-1

      *        C8
               WHEN 'H'
                   MOVE 12 TO WRK-CARTEIRA-OUT-2
                   MOVE 8  TO WRK-CARTEIRA-OUT-1

      *        C9
               WHEN 'I'
                   MOVE 12 TO WRK-CARTEIRA-OUT-2
                   MOVE 9  TO WRK-CARTEIRA-OUT-1

      *        D1
               WHEN 'J'
                   MOVE 13 TO WRK-CARTEIRA-OUT-2
                   MOVE 1  TO WRK-CARTEIRA-OUT-1

      *        D2
               WHEN 'K'
                   MOVE 13 TO WRK-CARTEIRA-OUT-2
                   MOVE 2  TO WRK-CARTEIRA-OUT-1

      *        D3
               WHEN 'L'
                   MOVE 13 TO WRK-CARTEIRA-OUT-2
                   MOVE 3  TO WRK-CARTEIRA-OUT-1

      *        D4
               WHEN 'M'
                   MOVE 13 TO WRK-CARTEIRA-OUT-2
                   MOVE 4  TO WRK-CARTEIRA-OUT-1

      *        D5
               WHEN 'N'
                   MOVE 13 TO WRK-CARTEIRA-OUT-2
                   MOVE 5  TO WRK-CARTEIRA-OUT-1

      *        D6
               WHEN 'O'
                   MOVE 13 TO WRK-CARTEIRA-OUT-2
                   MOVE 6  TO WRK-CARTEIRA-OUT-1

      *        D7
               WHEN 'P'
                   MOVE 13 TO WRK-CARTEIRA-OUT-2
                   MOVE 7  TO WRK-CARTEIRA-OUT-1

      *        D8
               WHEN 'Q'
                   MOVE 13 TO WRK-CARTEIRA-OUT-2
                   MOVE 8  TO WRK-CARTEIRA-OUT-1

      *        D9
               WHEN 'R'
                   MOVE 13 TO WRK-CARTEIRA-OUT-2
                   MOVE 9  TO WRK-CARTEIRA-OUT-1

      *        E2
               WHEN 'S'
                   MOVE 14 TO WRK-CARTEIRA-OUT-2
                   MOVE 2  TO WRK-CARTEIRA-OUT-1

      *        E3
               WHEN 'T'
                   MOVE 14 TO WRK-CARTEIRA-OUT-2
                   MOVE 3  TO WRK-CARTEIRA-OUT-1

      *        E4
               WHEN 'U'
                   MOVE 14 TO WRK-CARTEIRA-OUT-2
                   MOVE 4  TO WRK-CARTEIRA-OUT-1

      *        E5
               WHEN 'V'
                   MOVE 14 TO WRK-CARTEIRA-OUT-2
                   MOVE 5  TO WRK-CARTEIRA-OUT-1

      *        E6
               WHEN 'W'
                   MOVE 14 TO WRK-CARTEIRA-OUT-2
                   MOVE 6  TO WRK-CARTEIRA-OUT-1

      *        E7
               WHEN 'X'
                   MOVE 14 TO WRK-CARTEIRA-OUT-2
                   MOVE 7  TO WRK-CARTEIRA-OUT-1

      *        E8
               WHEN 'Y'
                   MOVE 14 TO WRK-CARTEIRA-OUT-2
                   MOVE 8  TO WRK-CARTEIRA-OUT-1

      *        E9
               WHEN 'Z'
                   MOVE 14 TO WRK-CARTEIRA-OUT-2
                   MOVE 9  TO WRK-CARTEIRA-OUT-1

      *        F0
               WHEN '0'
                   MOVE 15 TO WRK-CARTEIRA-OUT-2
                   MOVE 0  TO WRK-CARTEIRA-OUT-1

      *        F1
               WHEN '1'
                   MOVE 15 TO WRK-CARTEIRA-OUT-2
                   MOVE 1  TO WRK-CARTEIRA-OUT-1

      *        F2
               WHEN '2'
                   MOVE 15 TO WRK-CARTEIRA-OUT-2
                   MOVE 2  TO WRK-CARTEIRA-OUT-1

      *        F3
               WHEN '3'
                   MOVE 15 TO WRK-CARTEIRA-OUT-2
                   MOVE 3  TO WRK-CARTEIRA-OUT-1

      *        F4
               WHEN '4'
                   MOVE 15 TO WRK-CARTEIRA-OUT-2
                   MOVE 4  TO WRK-CARTEIRA-OUT-1

      *        F5
               WHEN '5'
                   MOVE 15 TO WRK-CARTEIRA-OUT-2
                   MOVE 5  TO WRK-CARTEIRA-OUT-1

      *        F6
               WHEN '6'
                   MOVE 15 TO WRK-CARTEIRA-OUT-2
                   MOVE 6  TO WRK-CARTEIRA-OUT-1

      *        F7
               WHEN '7'
                   MOVE 15 TO WRK-CARTEIRA-OUT-2
                   MOVE 7  TO WRK-CARTEIRA-OUT-1

      *        F8
               WHEN '8'
                   MOVE 15 TO WRK-CARTEIRA-OUT-2
                   MOVE 8  TO WRK-CARTEIRA-OUT-1

      *        F9
               WHEN '9'
                   MOVE 15 TO WRK-CARTEIRA-OUT-2
                   MOVE 9  TO WRK-CARTEIRA-OUT-1

           END-EVALUATE.



      *---------------------------------------------------------------*
       0900-99-FIM. EXIT.
      *---------------------------------------------------------------*

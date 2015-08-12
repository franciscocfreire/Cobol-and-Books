      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID.                     CLLP0182.
       AUTHOR.                         ROBSON.

      ******************************************************************
      *               B R Q     I T     S E R V I C E S                *
      *================================================================*
      *                                                                *
      * PROGRAMA     : CLLP0182                                        *
      * ANALISTA     : ROBSON VELLASQUES           - BRQ               *
      * DATA         : 15/01/2013                                      *
      *                                                                *
      * PROJETO      : 2013-0358 - ADEQUACAO LEI DA TRANSPARENCIA      *
      *                                                                *
      * RESPONSAVEIS : PAULO JACINTO LIMA          - DDS               *
      *              : ROBSON VELLASQUES           - BRQ               *
      *                                                                *
      *================================================================*
      *                       O B J E T I V O :                        *
      *                                                                *
      *                                                                *
      *                                                                *
      *========================================================== =====*
      *                       A R Q U I V O S :                        *
      *                                                                *
      *----------+---+------------------------------+--------+---------*
      *   DDNAME |I/O|         DESCRICAO            |   BOOK   | LRECL *
      *----------+---+------------------------------+----------+-------*
      * ARQENT01 | I |                       - LEI  | I#CLLPH6 |  314  *
      * ARQENT02 | I |                       - LEI  | I#CLLPH7 |  414  *
      * ARQEXP01 | O |                       - LEI  | I#CLLPH7 |  414  *
      *----------+---+------------------------------+----------+-------*
      *                                                                *
      *================================================================*
      *                        M O D U L O S :                         *
      *                                                                *
      *----------+----------+------------------------------------------*
      *  MODULO  |   BOOK   |          FUNCAO                          *
      *----------+----------+------------------------------------------*
      * POOL7100 | POOL710C | TRATAR ERROS                             *
      * POOL7600 | -------- | OBTER DATA E HORA DO SISTEMA             *
      * BRAD0160 | -------- | OBTER O NOME DO JOB EXECUTADO            *
      *----------+----------+------------------------------------------*
      *                                                                *
      *================================================================*
      *              B O O K ' S    F U N C I O N A I S :              *
      *                                                                *
      *----------+-----------------------------------------------------*
      *   BOOK   |          DESCRICAO                                  *
      *----------+-----------------------------------------------------*
      * I#CLLPZY | ESTATISTICAS DE PROCESSAMENTO                       *
      *----------+-----------------------------------------------------*
      *                                                                *
      ******************************************************************

      *================================================================*
        ENVIRONMENT                    DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.
      *----------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.
      *----------------------------------------------------------------*
       FILE-CONTROL.

           SELECT ARQENT01  ASSIGN      TO UT-S-ARQENT01
                      FILE STATUS      IS WRK-FS-ARQENT01.

           SELECT ARQENT02  ASSIGN      TO UT-S-ARQENT02
                      FILE STATUS      IS WRK-FS-ARQENT02.

           SELECT ARQEXP01  ASSIGN      TO UT-S-ARQEXP01
                      FILE STATUS      IS WRK-FS-ARQEXP01.




      *================================================================*
       DATA                            DIVISION.
      *================================================================*
      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT  :  ARQUIVO DA ROTINA CLLP NORMAL                     *
      *                                                                *
      *              ORG. BLOCADO      -   LRECL = 0668                *
      *----------------------------------------------------------------*
       FD  ARQENT01
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

           COPY 'I#CLLPH6'.

      *----------------------------------------------------------------*
      *    OUTPUT :  ARQUIVO ATUALIZADO COM BALANCE LINE               *
      *              ORG. BLOCADO      -   LRECL = 0768                *
      *----------------------------------------------------------------*
       FD  ARQENT02
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

           COPY 'I#CLLPH7'.

      *----------------------------------------------------------------*
      *    OUTPUT :  ARQUIVO ATUALIZADO COM BALANCE LINE               *
      *              ORG. BLOCADO      -   LRECL = 0768                *
      *----------------------------------------------------------------*
       FD  ARQEXP01
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

           COPY 'I#CLLPH7'.


      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** INICIO DA WORKING CLLP0182 ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA ACUMULADORES ***'.
      *----------------------------------------------------------------*

       77  ACU-LIDOS-ARQENT01          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-LIDOS-ARQENT02          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAVS-ARQEXP01          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAVS-ATUALIZA          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAVS-NATUALIZ          PIC  9(009) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
      *                       AREAS DE FILE STATUS                     *
      *----------------------------------------------------------------*
       01  WRK-FS-ARQENT01              PIC  X(002)        VALUE SPACES.
       01  WRK-FS-ARQENT02              PIC  X(002)        VALUE SPACES.
       01  WRK-FS-ARQEXP01              PIC  X(002)        VALUE SPACES.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE VARIAVEIS AUXILIARES ***'.
      *----------------------------------------------------------------*

       77  WRK-OPERACAO                PIC  X(13) VALUE SPACES.

       01  WRK-CHAVE-ARQENT01.
           03  WRK-CH1-AGENCIA         PIC  9(05)  COMP-3 VALUE ZEROS.
           03  WRK-CH1-CONTA           PIC  9(07)  COMP-3 VALUE ZEROS.
           03  WRK-CH1-CART            PIC  X(03)         VALUE SPACES.
           03  WRK-CH1-CONTRATO        PIC  9(07)  COMP-3 VALUE ZEROS.
           03  WRK-CH1-DT-PGTO         PIC  9(09)  COMP-3 VALUE ZEROS.
           03  WRK-CH1-DT-VCTO         PIC  9(09)  COMP-3 VALUE ZEROS.

       01  WRK-CHAVE-ARQENT02.
           03  WRK-CH2-AGENCIA         PIC  9(05)  COMP-3 VALUE ZEROS.
           03  WRK-CH2-CONTA           PIC  9(07)  COMP-3 VALUE ZEROS.
           03  WRK-CH2-CART            PIC  X(03)         VALUE SPACES.
           03  WRK-CH2-CONTRATO        PIC  9(07)  COMP-3 VALUE ZEROS.
           03  WRK-CH2-DT-PGTO         PIC  9(09)  COMP-3 VALUE ZEROS.
           03  WRK-CH2-DT-VCTO         PIC  9(09)  COMP-3 VALUE ZEROS.

       01  FILLER.
           05 WRK-LIT-OBJETIVO         PIC  X(64) VALUE '     GERAR ARQU
      -    'IVO EXPANDIDO PARA CARGA DA TABELA CLLPB000'.
           05 WRK-LIT-PROGRAMA         PIC  X(08) VALUE 'CLLP0182'.
           05 WRK-LIT-ARQENT01         PIC  X(08) VALUE 'ARQENT01'.
           05 WRK-LIT-DESC-ENT01       PIC  X(27) VALUE
                                          'IMAGEM DA TABELA CLLPB000  '.
           05 WRK-LIT-ARQENT02         PIC  X(08) VALUE 'ARQENT02'.
           05 WRK-LIT-DESC-ENT02       PIC  X(27) VALUE
                                          'CAMPOS DA LEI TRANSPARENCIA'.
           05 WRK-LIT-ARQEXP01         PIC  X(08) VALUE 'ARQEXP01'.
           05 WRK-LIT-DESC-EXP01       PIC  X(27) VALUE
                                          'ARQ. DE CARGA EXPANDIDO    '.
           05 WRK-LIT-ENTRADA          PIC  X(01) VALUE 'I'.
           05 WRK-LIT-SAIDA            PIC  X(01) VALUE 'O'.
           05 WRK-LIT-POOL7100         PIC  X(08) VALUE 'POOL7100'.
           05 WRK-LIT-DESC7100         PIC  X(32) VALUE 'TRATAR ERROS'.
           05 WRK-LIT-POOL7100-BOOK    PIC  X(08) VALUE 'POOL710C'.
           05 WRK-LIT-POOL7600         PIC  X(08) VALUE 'POOL7600'.
           05 WRK-LIT-DESC7600         PIC  X(32) VALUE
                                         'OBTER DATA E HORA DO SISTEMA'.
           05 WRK-LIT-BRAD0160         PIC  X(08) VALUE 'BRAD0160'.
           05 WRK-LIT-DESC0160         PIC  X(32) VALUE
                                        'OBTER O NOME DO JOB EXECUTADO'.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA DO ARQUIVO DE SAIDA ARQEXP01   ***'.
      *----------------------------------------------------------------*

       01  WRK-FD-ARQEXP01.
           05  FILLER                      PIC  X(314) VALUE SPACES.
      * NOVOS CAMPOS DA LEI DA TRANSPARENCIA 100 BYTES
           05  WRK-MOV-LT.
               10 WRK-TAXA-CONTRATO-N      PIC  9(02)V9(06) COMP-3
                                                   VALUE ZEROS.
               10 WRK-VR-REMUNERATORIO-N   PIC S9(13)V99 COMP-3
                                                   VALUE ZEROS.
               10 WRK-VALOR-MORATORIO-N    PIC S9(13)V99 COMP-3
                                                   VALUE ZEROS.
               10 WRK-VALOR-MULTA-N        PIC S9(13)V99 COMP-3
                                                   VALUE ZEROS.
               10 WRK-DESP-JUD-CUSTAS-N    PIC S9(11)V99 COMP-3
                                                   VALUE ZEROS.
               10 WRK-HONORARIOS-N         PIC S9(11)V99 COMP-3
                                                   VALUE ZEROS.
               10 WRK-VL-TOTAL-DIVIDA-N    PIC S9(15)V99 COMP-3
                                                   VALUE ZEROS.
               10 WRK-VL-TAXA-TARIFA-N     PIC S9(15)V99 COMP-3
                                                   VALUE ZEROS.
               10 FILLER               PIC  X(39)  VALUE SPACES.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE TRATAMENTO DE ERRO         ***'.
      *----------------------------------------------------------------*

       77  WRK-BATCH                   PIC  X(08) VALUE 'BATCH'.

           COPY 'POL7100C'.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA BRAD0160                   ***'.
      *----------------------------------------------------------------*

       01 WRK-AREA-BRAD0160.
          05 WRK-JOBNAME-BRAD0160      PIC  X(08) VALUE SPACES.
          05 WRK-VLRFAC-BRAD0160       PIC  9(05)V99 COMP-3 VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE DISPLAY DE ESTATISTICAS    ***'.
      *----------------------------------------------------------------*

           COPY 'I#CLLPZY'.

         03 WRK-CLLPZY-DISP16.
           05 FILLER                   PIC  X(03) VALUE '** '.
           05 FILLER                   PIC  X(08) VALUE ' MODULO '.
           05 FILLER                   PIC  X(03) VALUE ' | '.
           05 FILLER                   PIC  X(08) VALUE '  BOOK  '.
           05 FILLER                   PIC  X(03) VALUE ' | '.
           05 FILLER                   PIC  X(42) VALUE 'DESCRICAO'.
           05 FILLER                   PIC  X(03) VALUE ' **'.

         03 WRK-CLLPZY-DISP17.
           05 FILLER                   PIC  X(03) VALUE '** '.
           05 FILLER                   PIC  X(08) VALUE  ALL '-'.
           05 FILLER                   PIC  X(03) VALUE ' | '.
           05 FILLER                   PIC  X(08) VALUE  ALL '-'.
           05 FILLER                   PIC  X(03) VALUE ' | '.
           05 FILLER                   PIC  X(42) VALUE  ALL '-'.
           05 FILLER                   PIC  X(03) VALUE ' **'.

         03 WRK-CLLPZY-DISP18.
           05 FILLER                   PIC  X(03) VALUE '** '.
           05 WRK-CLLPZY-MODULO-DISP   PIC  X(08) VALUE SPACES.
           05 FILLER                   PIC  X(03) VALUE ' | '.
           05 WRK-CLLPZY-MODULO-BOOK   PIC  X(08) VALUE SPACES.
           05 FILLER                   PIC  X(03) VALUE ' | '.
           05 WRK-CLLPZY-MODULO-DESC   PIC  X(42) VALUE SPACES.
           05 FILLER                   PIC  X(03) VALUE ' **'.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA UTILIZADA PELA POOL7600       ***'.
      *----------------------------------------------------------------*

       01 WRK-7600-DATA-HORA.
           03 WRK-DT-JULIANA-7600      PIC  9(05) COMP-3 VALUE ZEROS.
           03 WRK-DT-AAMMDD-7600       PIC  9(07) COMP-3 VALUE ZEROS.
           03 WRK-DT-AAAAMMDD-7600     PIC  9(09) COMP-3 VALUE ZEROS.
           03 WRK-TI-HHMMSS-7600       PIC  9(07) COMP-3 VALUE ZEROS.
           03 WRK-TI-HHMMSSMMMMMM-7600 PIC  9(13) COMP-3 VALUE ZEROS.
           03 WRK-TIMESTAMP-7600.
               05 WRK-ANO-7600         PIC  9(04) VALUE ZEROS.
               05 WRK-MES-7600         PIC  9(02) VALUE ZEROS.
               05 WRK-DIA-7600         PIC  9(02) VALUE ZEROS.
               05 WRK-HORA-7600        PIC  9(02) VALUE ZEROS.
               05 WRK-MINUTOS-7600     PIC  9(02) VALUE ZEROS.
               05 WRK-SEGUNDOS-7600    PIC  9(02) VALUE ZEROS.
               05 WRK-MICROSEGUNDOS-7600
                                       PIC  9(06) VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA AUXILIAR DE DATA E HORA       ***'.
      *----------------------------------------------------------------*

       01 WRK-AAAAMMDD                 PIC  9(09) VALUE ZEROS.
       01 WRK-AAAAMMDD-AUX             REDEFINES  WRK-AAAAMMDD.
          03 FILLER                    PIC  X(01).
          03 WRK-AAAAMMDD-R.
             05 WRK-ANO                PIC  9(04).
             05 WRK-MES                PIC  9(02).
             05 WRK-DIA                PIC  9(02).

       01 WRK-DDMMAAAA.
          03 WRK-DIA                   PIC  9(02)  VALUE ZEROS.
          03 FILLER                    PIC  X(01)  VALUE '/'.
          03 WRK-MES                   PIC  9(02)  VALUE ZEROS.
          03 FILLER                    PIC  X(01)  VALUE '/'.
          03 WRK-ANO                   PIC  9(04)  VALUE ZEROS.
       01 WRK-DDMMAAAA-R               REDEFINES   WRK-DDMMAAAA
                                       PIC  X(10).

       01 WRK-HORA                     PIC  9(09)  VALUE ZEROS.
       01 WRK-HORA-AUX                 REDEFINES   WRK-HORA.
          03 FILLER                    PIC  X(03).
          03 WRK-HORA-R.
             05 WRK-HH                 PIC  9(02).
             05 WRK-MM                 PIC  9(02).
             05 WRK-SS                 PIC  9(02).

       01 WRK-HORA-EDITADA.
          03 WRK-HH                    PIC  9(02)  VALUE ZEROS.
          03 FILLER                    PIC  X(01)  VALUE ':'.
          03 WRK-MM                    PIC  9(02)  VALUE ZEROS.
          03 FILLER                    PIC  X(01)  VALUE ':'.
          03 WRK-SS                    PIC  9(02)  VALUE ZEROS.
       01 WRK-HORA-EDITADA-R           REDEFINES   WRK-HORA-EDITADA
                                       PIC  X(08).
       01  WRK-MSG-ERRO-BRAD0315.
           03  FILLER                  PIC X(07)           VALUE SPACES.
           03  FILLER                  PIC X(02)           VALUE '* '.
           03  FILLER                  PIC X(05)           VALUE 'ERRO'.
           03  WRK-MSG-ERRO-OPERACAO   PIC X(13)           VALUE SPACES.
           03  FILLER                  PIC X(12)           VALUE
               ' DO ARQUIVO '.
           03  WRK-MSG-ERRO-ARQUIVO    PIC X(08)           VALUE SPACES.
           03  FILLER                  PIC X(17)           VALUE
               ' - FILE-STATUS = '.
           03  WRK-MSG-ERRO-RC2.
               05  WRK-MSG-ERRO-RC2-N  PIC 9(04)           VALUE ZEROS.
           03  FILLER                  PIC X(02)           VALUE ' *'.


      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** FIM DA WORKING CLLP0182 ***'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      ******************************************************************
      *    ROTINA PRINCIPAL DO PROGRAMA.                               *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.
      *----------------------------------------------------------------*

           OPEN  INPUT  ARQENT01
                        ARQENT02
                 OUTPUT ARQEXP01

           PERFORM 2000-FORMATAR-DATA-HORA

           PERFORM 3000-VERIFICAR-VAZIO

           PERFORM 4000-PROCESSAR
             UNTIL WRK-CHAVE-ARQENT01  EQUAL HIGH-VALUES.

           PERFORM 5000-PROCEDIMENTOS-FINAIS

           .

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA PARA FORMATAR DATA E HORA DE PROCESSAMENTO           *
      ******************************************************************
      *---------------------------------------------------------------*
       2000-FORMATAR-DATA-HORA         SECTION.
      *---------------------------------------------------------------*

           CALL  'POOL7600'            USING WRK-7600-DATA-HORA
           MOVE   WRK-DT-AAAAMMDD-7600 TO    WRK-AAAAMMDD
           MOVE CORR WRK-AAAAMMDD-R    TO    WRK-DDMMAAAA
           MOVE   WRK-TI-HHMMSS-7600   TO    WRK-HORA
           MOVE CORR WRK-HORA-R        TO    WRK-HORA-EDITADA
           .

      *---------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      ******************************************************************
      *    ROTINA PARA VERIFICACAO DE ARQUIVO VAZIO                    *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 3010-VERIFICAR-ARQENT01
           PERFORM 3020-VERIFICAR-ARQENT02
           .

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA PARA VERIFICACAO DE ARQENT01 ESTA VAZIO              *
      ******************************************************************
      *----------------------------------------------------------------*
       3010-VERIFICAR-ARQENT01         SECTION.
      *----------------------------------------------------------------*

           PERFORM 3100-LER-ARQENT01

           IF ACU-LIDOS-ARQENT01       EQUAL ZEROS
              DISPLAY '******************* CLLP0182 *******************'
              DISPLAY '*                                              *'
              DISPLAY '*          ARQUIVO ARQENT01 ESTA VAZIO         *'
              DISPLAY '*                                              *'
              DISPLAY '*              PROGRAMA ENCERRADO              *'
              DISPLAY '*                                              *'
              DISPLAY '******************* CLLP0182 *******************'
              PERFORM  5000-PROCEDIMENTOS-FINAIS
           END-IF

           .

      *----------------------------------------------------------------*
       3010-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA PARA VERIFICACAO DE ARQENT02 ESTA VAZIO              *
      ******************************************************************
      *----------------------------------------------------------------*
       3020-VERIFICAR-ARQENT02         SECTION.
      *----------------------------------------------------------------*

           PERFORM 3200-LER-ARQENT02

           IF ACU-LIDOS-ARQENT02       EQUAL ZEROS
              DISPLAY '******************* CLLP0182 *******************'
              DISPLAY '*                                              *'
              DISPLAY '*          ARQUIVO ARQENT02 ESTA VAZIO         *'
              DISPLAY '*                                              *'
              DISPLAY '*                  ESTA VAZIO                  *'
              DISPLAY '*                                              *'
              DISPLAY '******************* CLLP0182 *******************'
           END-IF

           .

      *----------------------------------------------------------------*
       3020-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE LEITURA DO ARQUIVO ARQENT01                       *
      ******************************************************************
      *----------------------------------------------------------------*
       3100-LER-ARQENT01               SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                  REG-ENTRADA1.

           READ ARQENT01.

           IF WRK-FS-ARQENT01  EQUAL '10'
               MOVE HIGH-VALUES    TO WRK-CHAVE-ARQENT01
               GO                  TO 3100-99-FIM
           END-IF.

           ADD  1                      TO ACU-LIDOS-ARQENT01

           MOVE  ENT1-AGENCIA          TO WRK-CH1-AGENCIA
           MOVE  ENT1-NUM-CC           TO WRK-CH1-CONTA
           MOVE  ENT1-CARTEIRA         TO WRK-CH1-CART
           MOVE  ENT1-CONTRATO         TO WRK-CH1-CONTRATO

           MOVE  ENT1-DATA-VENCTO      TO WRK-DDMMAAAA-R
           MOVE  WRK-DIA OF WRK-DDMMAAAA TO WRK-DIA OF WRK-AAAAMMDD-R
           MOVE  WRK-MES OF WRK-DDMMAAAA TO WRK-MES OF WRK-AAAAMMDD-R
           MOVE  WRK-ANO OF WRK-DDMMAAAA TO WRK-ANO OF WRK-AAAAMMDD-R

           MOVE  WRK-AAAAMMDD          TO WRK-CH1-DT-VCTO

           MOVE  ENT1-DATA-BAIXA       TO WRK-DDMMAAAA-R
           MOVE  WRK-DIA OF WRK-DDMMAAAA TO WRK-DIA OF WRK-AAAAMMDD-R
           MOVE  WRK-MES OF WRK-DDMMAAAA TO WRK-MES OF WRK-AAAAMMDD-R
           MOVE  WRK-ANO OF WRK-DDMMAAAA TO WRK-ANO OF WRK-AAAAMMDD-R
           MOVE  WRK-AAAAMMDD          TO WRK-CH1-DT-PGTO.


      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE LEITURA DO ARQUIVO ARQENT02                       *
      ******************************************************************
      *----------------------------------------------------------------*
       3200-LER-ARQENT02                SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                  REG-ENTRADA2 OF ARQENT02.

           READ ARQENT02.

           IF WRK-FS-ARQENT02  EQUAL '10'
               MOVE HIGH-VALUES    TO WRK-CHAVE-ARQENT02
               GO                  TO 3200-99-FIM
           END-IF.

           ADD  1                      TO ACU-LIDOS-ARQENT02

           MOVE  ENT2-AGENCIA OF ARQENT02          TO WRK-CH2-AGENCIA
           MOVE  ENT2-NUM-CC OF ARQENT02           TO WRK-CH2-CONTA
           MOVE  ENT2-CARTEIRA OF ARQENT02         TO WRK-CH2-CART
           MOVE  ENT2-CONTRATO OF ARQENT02         TO WRK-CH2-CONTRATO

           MOVE  ENT2-DATA-VENCTO OF ARQENT02      TO WRK-DDMMAAAA-R
           MOVE  WRK-DIA OF WRK-DDMMAAAA TO WRK-DIA OF WRK-AAAAMMDD-R
           MOVE  WRK-MES OF WRK-DDMMAAAA TO WRK-MES OF WRK-AAAAMMDD-R
           MOVE  WRK-ANO OF WRK-DDMMAAAA TO WRK-ANO OF WRK-AAAAMMDD-R

           MOVE  WRK-AAAAMMDD          TO WRK-CH2-DT-VCTO

           MOVE  ENT2-DATA-BAIXA OF ARQENT02       TO WRK-DDMMAAAA-R
           MOVE  WRK-DIA OF WRK-DDMMAAAA TO WRK-DIA OF WRK-AAAAMMDD-R
           MOVE  WRK-MES OF WRK-DDMMAAAA TO WRK-MES OF WRK-AAAAMMDD-R
           MOVE  WRK-ANO OF WRK-DDMMAAAA TO WRK-ANO OF WRK-AAAAMMDD-R
           MOVE  WRK-AAAAMMDD          TO WRK-CH2-DT-PGTO
           .

      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * EFETUA BALANCE LINE                                            *
      ******************************************************************
      *----------------------------------------------------------------*
       4000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           EVALUATE TRUE

             WHEN WRK-CHAVE-ARQENT01   EQUAL   WRK-CHAVE-ARQENT02
                   MOVE REG-ENTRADA1   TO WRK-FD-ARQEXP01
                   MOVE ENT2-MOV-LT OF ARQENT02    TO WRK-MOV-LT
                   ADD 1 TO ACU-GRAVS-ATUALIZA
                   PERFORM 4200-GRAVAR-ARQEXP01
                   PERFORM 3100-LER-ARQENT01
                   PERFORM 3200-LER-ARQENT02

             WHEN WRK-CHAVE-ARQENT01   LESS    WRK-CHAVE-ARQENT02
                 MOVE REG-ENTRADA1     TO WRK-FD-ARQEXP01
                 PERFORM 4300-INICIALIZAR-CAMPOS-LEI
                 ADD 1 TO ACU-GRAVS-NATUALIZ
                 PERFORM 4200-GRAVAR-ARQEXP01
                 PERFORM 3100-LER-ARQENT01

             WHEN WRK-CHAVE-ARQENT01   GREATER WRK-CHAVE-ARQENT02

                  PERFORM 3200-LER-ARQENT02

           END-EVALUATE

           .

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA GRAVACAO DO ARQUIVO DE SAIDA ARQEXP01              *
      ******************************************************************
      *----------------------------------------------------------------*
       4200-GRAVAR-ARQEXP01            SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-FD-ARQEXP01 TO REG-ENTRADA2 OF ARQEXP01
           WRITE REG-ENTRADA2 OF ARQEXP01.

           MOVE 'GRAVACAO'               TO WRK-MSG-ERRO-OPERACAO.
           MOVE WRK-LIT-ARQEXP01         TO WRK-MSG-ERRO-ARQUIVO.

           IF  WRK-FS-ARQEXP01         NOT EQUAL '00'
               DISPLAY '************* CLLP0182 **************'
               DISPLAY '*   ERRO ' WRK-OPERACAO ' DO ARQUIVO   *'
               DISPLAY '*              CLIEFICA             *'
               DISPLAY '*        FILE STATUS =  ' WRK-FS-ARQEXP01
                                                 '          *'
               DISPLAY '************* CLLP0182 **************'
               PERFORM 9000-ROTINA-ERRO
           END-IF.

           ADD 1                       TO ACU-GRAVS-ARQEXP01
           .

      *----------------------------------------------------------------*
       4200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4300-INICIALIZAR-CAMPOS-LEI     SECTION.
      *----------------------------------------------------------------*

               MOVE ZEROES TO WRK-TAXA-CONTRATO-N.
               MOVE ZEROES TO  WRK-VR-REMUNERATORIO-N.
               MOVE ZEROES TO  WRK-VALOR-MORATORIO-N.
               MOVE ZEROES TO WRK-VALOR-MULTA-N.
               MOVE ZEROES TO WRK-DESP-JUD-CUSTAS-N.
               MOVE ZEROES TO WRK-HONORARIOS-N.
               MOVE ZEROES TO WRK-VL-TOTAL-DIVIDA-N.
               MOVE ZEROES TO WRK-VL-TAXA-TARIFA-N.

      *----------------------------------------------------------------*
       4300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*


      ******************************************************************
      *    ROTINA PARA PROCEDIMENTOS FINAIS                            *
      ******************************************************************
      *----------------------------------------------------------------*
       5000-PROCEDIMENTOS-FINAIS       SECTION.
      *----------------------------------------------------------------*

           CLOSE        ARQENT01
                        ARQENT02
                        ARQEXP01.

           PERFORM 5200-EMITIR-TOTAIS


           MOVE ZEROS                  TO RETURN-CODE
           PERFORM 9999-FINALIZAR
           .

      *----------------------------------------------------------------*
       5000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA ESTATISTICAS DO PROCESSAMENTO                      *
      ******************************************************************
      *----------------------------------------------------------------*
       5200-EMITIR-TOTAIS              SECTION.
      *----------------------------------------------------------------*

           MOVE  WRK-LIT-OBJETIVO      TO  CLLPZY-COMENTARIO
           MOVE  WRK-LIT-PROGRAMA      TO  CLLPZY-PROGNAME

           CALL  'BRAD0160'            USING  WRK-JOBNAME-BRAD0160
                                              WRK-VLRFAC-BRAD0160

           MOVE  WRK-JOBNAME-BRAD0160  TO  CLLPZY-JOBNAME
           MOVE  WRK-DDMMAAAA-R        TO  CLLPZY-DATAPROC
                                           CLLPZY-DATAMOV
           MOVE  WRK-HORA-EDITADA-R    TO  CLLPZY-HORAPROC

           DISPLAY  CLLPZY-DISP1
           DISPLAY  CLLPZY-DISP2
           DISPLAY  CLLPZY-DISP3
           DISPLAY  CLLPZY-DISP13
           DISPLAY  CLLPZY-DISP3
           DISPLAY  CLLPZY-DISP4
           DISPLAY  CLLPZY-DISP5
           DISPLAY  CLLPZY-DISP6
           DISPLAY  CLLPZY-DISP3
           DISPLAY  CLLPZY-DISP7
           DISPLAY  CLLPZY-DISP8

           MOVE  WRK-LIT-ARQENT01      TO  CLLPZY-DDNAME
           MOVE  WRK-LIT-ENTRADA       TO  CLLPZY-I-O
           MOVE  WRK-LIT-DESC-ENT01    TO  CLLPZY-DESCARQ
           MOVE  ACU-LIDOS-ARQENT01    TO  CLLPZY-QTDEARQ

           DISPLAY  CLLPZY-DISP9

           MOVE  WRK-LIT-ARQENT02      TO  CLLPZY-DDNAME
           MOVE  WRK-LIT-ENTRADA       TO  CLLPZY-I-O
           MOVE  WRK-LIT-DESC-ENT02    TO  CLLPZY-DESCARQ
           MOVE  ACU-LIDOS-ARQENT02    TO  CLLPZY-QTDEARQ

           DISPLAY  CLLPZY-DISP9

           MOVE  WRK-LIT-ARQEXP01      TO  CLLPZY-DDNAME
           MOVE  WRK-LIT-SAIDA         TO  CLLPZY-I-O
           MOVE  WRK-LIT-DESC-EXP01    TO  CLLPZY-DESCARQ
           MOVE  ACU-GRAVS-ARQEXP01    TO  CLLPZY-QTDEARQ

           DISPLAY  CLLPZY-DISP9

           MOVE  SPACES                TO  CLLPZY-DDNAME
           MOVE  SPACES                TO  CLLPZY-I-O
           MOVE  ' - ARQ ATUALIZADOS'  TO  CLLPZY-DESCARQ
           MOVE  ACU-GRAVS-ATUALIZA    TO  CLLPZY-QTDEARQ

           DISPLAY  CLLPZY-DISP9

           MOVE  SPACES                TO  CLLPZY-DDNAME
           MOVE  SPACES                TO  CLLPZY-I-O
           MOVE  ' - ARQ NAO ATUALIZADOS'  TO  CLLPZY-DESCARQ
           MOVE  ACU-GRAVS-NATUALIZ    TO  CLLPZY-QTDEARQ

           DISPLAY  CLLPZY-DISP9
           DISPLAY  CLLPZY-DISP3
           DISPLAY  WRK-CLLPZY-DISP16
           DISPLAY  WRK-CLLPZY-DISP17

           MOVE  WRK-LIT-POOL7100      TO  WRK-CLLPZY-MODULO-DISP
           MOVE  WRK-LIT-POOL7100-BOOK TO  WRK-CLLPZY-MODULO-BOOK
           MOVE  WRK-LIT-DESC7100      TO  WRK-CLLPZY-MODULO-DESC

           DISPLAY  WRK-CLLPZY-DISP18

           MOVE  WRK-LIT-POOL7600      TO  WRK-CLLPZY-MODULO-DISP
           MOVE  SPACES                TO  WRK-CLLPZY-MODULO-BOOK
           MOVE  WRK-LIT-DESC7600      TO  WRK-CLLPZY-MODULO-DESC

           DISPLAY  WRK-CLLPZY-DISP18

           MOVE  WRK-LIT-BRAD0160      TO  WRK-CLLPZY-MODULO-DISP
           MOVE  SPACES                TO  WRK-CLLPZY-MODULO-BOOK
           MOVE  WRK-LIT-DESC0160      TO  WRK-CLLPZY-MODULO-DESC

           DISPLAY  WRK-CLLPZY-DISP18
           DISPLAY  CLLPZY-DISP1
           .

      *----------------------------------------------------------------*
       5200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA DE TRATAMENTO DE ERROS.                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       9000-ROTINA-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE  WRK-LIT-PROGRAMA      TO ERR-PGM
           MOVE  'APL'                 TO ERR-TIPO-ACESSO
           CALL  'POOL7100'            USING WRK-BATCH
                                             ERRO-AREA
           GOBACK
           .

      *----------------------------------------------------------------*
       9000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA PARA FINALIZAR O PROGRAMA                            *
      ******************************************************************
      *----------------------------------------------------------------*
       9999-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           STOP RUN.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

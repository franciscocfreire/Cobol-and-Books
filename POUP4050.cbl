      *===============================================================*
       IDENTIFICATION DIVISION.
      *===============================================================*

       PROGRAM-ID. POUP4050.
       AUTHOR. FRANCISCO.

      *================================================================*
      *               B R Q   -   I T   S E R V I C E S                *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA     : POUP4050                                     *
      *    ANALISTA     : FERNANDA CARUSO        - BRQ IT SERVICES.    *
      *    PROGRAMADOR  : FRANCISCO FREIRE       - BRQ IT SERVICES.    *
      *    DATA         : 11/08/2015                                   *
      *                                                                *
      *    OBJETIVO     : OBTER DATA DE CADASTRO                       *
      *                                                                *
      *                           ARQUIVOS                             *
      *           +---------------------------------------+            *
      *           |  DDNAME  |   I/O   |   BOOK   | LRECL |            *
      *           |----------|---------|----------|-------|            *
      *           | CADASTRO | INPUT   | PSDCW033 | 0642  |            *
      *           | ARQRENDA | INPUT   | PSDCW033 | 0642  |            *
      *           | ARQRENDS | OUTPUT  | PSDCW033 | 0642  |            *
      *           +---------------------------------------+            *
      *                                                                *
      *    MODULOS :                                                   *
      *                                                                *
      *    -> BRAD7100 (    -   ) - TRATAMENTO DE ERRO DB2             *
      *    -> BRAD7600 (    -   ) - OBTEM DATA E HORA DO SISTEMA       *
      *                                                                *
      *----------------------------------------------------------------*

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

           SELECT  CADASTRO  ASSIGN  TO  UT-S-CADASTRO
                   FILE      STATUS  IS  WRK-FS-CADASTRO.

           SELECT  ARQRENDE  ASSIGN  TO  UT-S-ARQRENDE
                   FILE      STATUS  IS  WRK-FS-ARQRENDE.

           SELECT  ARQRENDS  ASSIGN  TO  UT-S-ARQRENDS
                   FILE      STATUS  IS  WRK-FS-ARQRENDS.

      *===============================================================*
       DATA DIVISION.
      *===============================================================*

      *---------------------------------------------------------------*
       FILE SECTION.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
      *  INPUT....:                                                   *
      *             ORG. SEQUENCIAL    -  LRECL = 642 BYTES           *
      *---------------------------------------------------------------*

       FD  CADASTRO
           LABEL       RECORD    STANDARD
           RECORDING   MODE      F
           BLOCK       CONTAINS  0.

       COPY 'PSDCW033'.
      *---------------------------------------------------------------*
      *  INPUT....:                                                   *
      *             ORG. SEQUENCIAL    -  LRECL = 642 BYTES           *
      *---------------------------------------------------------------*

       FD  ARQRENDE
           LABEL       RECORD    STANDARD
           RECORDING   MODE      F
           BLOCK       CONTAINS  0.

       COPY 'PSDCW033'.
      *---------------------------------------------------------------*
      *  OUTPUT...:                                                   *
      *             ORG. SEQUENCIAL   -   LRECL = 642 BYTES           *
      *---------------------------------------------------------------*

       FD  ARQRENDS
           LABEL       RECORD    STANDARD
           RECORDING   MODE      F
           BLOCK       CONTAINS  0.


       COPY 'PSDCW033'.

      *---------------------------------------------------------------*
       WORKING-STORAGE SECTION.
      *---------------------------------------------------------------*
       01   FILLER                     PIC  X(32) VALUE
            'INICIO DA WORKING POUP4050'.

      *-- ------------------------------------------------------------*
      *          CAMPOS UTILIZADOS PARA CANCELAR PROCESSAMENTO        *
      *---------------------------------------------------------------*

       01  FILLER.
           03  WRK-FS-CADASTRO         PIC  X(02) VALUE SPACES.
           03  WRK-FS-ARQRENDE         PIC  X(02) VALUE SPACES.
           03  WRK-FS-ARQRENDS         PIC  X(02) VALUE SPACES.
           03  WRK-OPERACAO            PIC  X(13) VALUE SPACES.
           03  WRK-ABERTURA            PIC  X(13) VALUE 'NA ABERTURA'.
           03  WRK-LEITURA             PIC  X(13) VALUE 'NA LEITURA'.
           03  WRK-GRAVACAO            PIC  X(13) VALUE 'NA GRAVACAO'.
           03  WRK-FECHAMENTO          PIC  X(13) VALUE 'NO FECHAMENTO'.
           03  WRK-BATCH               PIC  X(08) VALUE 'BATCH'.
      *---------------------------------------------------------------*
      *--  ACUMULADORES.

       01  FILLER.
           03  ACU-LDS-CADASTRO        PIC 9(09) COMP-3    VALUE  ZEROS.
           03  ACU-LDS-ARQRENDE        PIC 9(09) COMP-3    VALUE  ZEROS.
           03  ACU-GRV-ARQRENDS        PIC 9(09) COMP-3    VALUE  ZEROS.
      *
      *----------------------------------------------------------------*
      *--  EDICAO.

       01  FILLER.
           03  WRK-EDIT01              PIC ZZZ.ZZZ.ZZ9     VALUE  ZEROS.
           03  WRK-EDIT02              PIC ZZZ.ZZZ.ZZ9     VALUE  ZEROS.
           03  WRK-EDIT05              PIC ZZZ.ZZZ.ZZ9     VALUE  ZEROS.

      *----------------------------------------------------------------*
      *--  AREA AUXILIAR.

       01  WRK-AUX-NUM010              PIC 9(10) VALUE  ZEROS.
       01  WRK-AUX-NUM010-S            REDEFINES
           WRK-AUX-NUM010              PIC S9(10).

      *--  CHAVE CADASTRO.
       01  WRK-CHV-CADASTRO.
           03  WRK-CHV-0A4-CCLUB       PIC 9(10) VALUE  ZEROS.

      *--  CHAVE ARQRENDE.
       01  WRK-CHV-ARQRENDE.
           03  WRK-CHV-018-CCLUB       PIC 9(10) VALUE  ZEROS.

      *----------------------------------------------------------------*
       01  FILLER   PIC  X(32) VALUE '*       AREA DA BRAD7100       *'.
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *---------------------------------------------------------------*
       01   FILLER                     PIC  X(32)    VALUE
            'FIM DA WORKING POUP4050'.

      *===============================================================*
       PROCEDURE DIVISION.
      *===============================================================*

      *---------------------------------------------------------------*
       0000-INICIAR SECTION.
      *---------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-LER-CADASTRO
           IF      WRK-FS-CADASTRO     EQUAL '10'
                   DISPLAY '**************** POUP4050 ***************'
                   DISPLAY '*                                       *'
                   DISPLAY '* ARQUIVO DE ENTRADA - CADASTRO - VAZIO *'
                   DISPLAY '*        PROCESSAMENTO ENCERRADO        *'
                   DISPLAY '*                                       *'
                   DISPLAY '**************** POUP4050 ***************'
                   PERFORM  9000-FINALIZAR
           END-IF.

           PERFORM 3000-LER-ARQRENDE
           IF      WRK-FS-ARQRENDE     EQUAL '10'
                   DISPLAY '**************** POUP4050 ***************'
                   DISPLAY '*                                       *'
                   DISPLAY '* ARQUIVO DE ENTRADA - ARQRENDE - VAZIO *'
                   DISPLAY '*        PROCESSAMENTO ENCERRADO        *'
                   DISPLAY '*                                       *'
                   DISPLAY '**************** POUP4050 ***************'
           END-IF

           PERFORM 4000-PROCESSAR     UNTIL
                  (WRK-FS-CADASTRO     EQUAL '10')


           PERFORM 7000-DISPLAY-TOTAIS

           PERFORM 9000-FINALIZAR.

      *---------------------------------------------------------------*
       0000-99-FIM.   EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1000-INICIALIZAR              SECTION.
      *---------------------------------------------------------------*

           OPEN  INPUT  CADASTRO
                        ARQRENDE
                 OUTPUT ARQRENDS

           MOVE  WRK-ABERTURA          TO    WRK-OPERACAO
           PERFORM  1100-TESTAR-FILE-STATUS.

      *---------------------------------------------------------------*
       1000-99-FIM.   EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS       SECTION.
      *---------------------------------------------------------------*

           PERFORM  1110-TESTAR-FS-CADASTRO
           PERFORM  1120-TESTAR-FS-ARQRENDE
           PERFORM  1150-TESTAR-FS-ARQRENDS.

      *---------------------------------------------------------------*
       1100-99-FIM.   EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1110-TESTAR-FS-CADASTRO       SECTION.
      *---------------------------------------------------------------*

           IF WRK-FS-CADASTRO         NOT EQUAL  '00'
              DISPLAY '************** POUP4050 *************'
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
              DISPLAY '*              CADASTRO             *'
              DISPLAY '*         FILE STATUS =  ' WRK-FS-CADASTRO
                                                 '         *'
              DISPLAY '************** POUP4050 *************'
              PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       1110-99-FIM.   EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1120-TESTAR-FS-ARQRENDE       SECTION.
      *---------------------------------------------------------------*

           IF WRK-FS-ARQRENDE         NOT EQUAL  '00'
              DISPLAY '************** POUP4050 *************'
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
              DISPLAY '*              ARQRENDE             *'
              DISPLAY '*         FILE STATUS =  ' WRK-FS-ARQRENDE
                                                 '         *'
              DISPLAY '************** POUP4050 *************'
              PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       1120-99-FIM.   EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1150-TESTAR-FS-ARQRENDS        SECTION.
      *---------------------------------------------------------------*

           IF WRK-FS-ARQRENDS           NOT EQUAL  '00'
              DISPLAY '************** POUP4050 *************'
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
              DISPLAY '*              ARQRENDS              *'
              DISPLAY '*         FILE STATUS =  ' WRK-FS-ARQRENDS
                                                 '         *'
              DISPLAY '************** POUP4050 *************'
              PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       1150-99-FIM.   EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2000-LER-CADASTRO            SECTION.
      *---------------------------------------------------------------*

           READ    CADASTRO

           IF      WRK-FS-CADASTRO     EQUAL  '10'
                   MOVE HIGH-VALUES    TO      WRK-CHV-CADASTRO
                   GO                  TO      2000-99-FIM
           END-IF.

           MOVE    WRK-LEITURA         TO      WRK-OPERACAO
           PERFORM 1110-TESTAR-FS-CADASTRO


           MOVE PSDCW033-01-CCLUB OF CADASTRO
                                       TO  WRK-AUX-NUM010-S.
           MOVE WRK-AUX-NUM010         TO  WRK-CHV-0A4-CCLUB.

           ADD     1                      TO  ACU-LDS-CADASTRO.

      *---------------------------------------------------------------*
       2000-99-FIM.   EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3000-LER-ARQRENDE            SECTION.
      *---------------------------------------------------------------*

           READ    ARQRENDE

           IF      WRK-FS-ARQRENDE     EQUAL  '10'
                   MOVE HIGH-VALUES    TO      WRK-CHV-ARQRENDE
                   GO                  TO      3000-99-FIM
           END-IF.

           MOVE    WRK-LEITURA         TO      WRK-OPERACAO
           PERFORM 1120-TESTAR-FS-ARQRENDE


           MOVE PSDCW033-08-CCLUB OF ARQRENDE
                                       TO  WRK-AUX-NUM010-S
           MOVE WRK-AUX-NUM010         TO  WRK-CHV-018-CCLUB.

           ADD     1                   TO      ACU-LDS-ARQRENDE.

      *---------------------------------------------------------------*
       3000-99-FIM.   EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       4000-PROCESSAR               SECTION.
      *---------------------------------------------------------------*

           IF      WRK-CHV-CADASTRO   GREATER   WRK-CHV-ARQRENDE
                   PERFORM 3000-LER-ARQRENDE
           ELSE
              IF      WRK-CHV-CADASTRO    LESS    WRK-CHV-ARQRENDE
                      PERFORM 2000-LER-CADASTRO
              ELSE
                      PERFORM 4200-CHAVES-IGUAIS
                      PERFORM 2000-LER-CADASTRO
                      PERFORM 3000-LER-ARQRENDE
              END-IF
           END-IF.

      *---------------------------------------------------------------*
       4000-99-FIM.   EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       4200-CHAVES-IGUAIS           SECTION.
      *---------------------------------------------------------------*

           INITIALIZE PSDCW033-08-INFO-PROFS OF ARQRENDS

           MOVE PSDCW033-08-INFO-PROFS OF ARQRENDE
                           TO PSDCW033-08-INFO-PROFS OF ARQRENDS

           MOVE PSDCW033-01-DCAD-PSSOA OF CADASTRO
               TO PSDCW033-08-INFO-PROFS OF ARQRENDS(296:10)


           PERFORM 5000-GRAVAR-ARQRENDS.

      *---------------------------------------------------------------*
       4200-99-FIM.   EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       5000-GRAVAR-ARQRENDS          SECTION.
      *---------------------------------------------------------------*

           WRITE PSDCW033-REGISTRO OF ARQRENDS
           MOVE   WRK-GRAVACAO         TO      WRK-OPERACAO
           PERFORM  1150-TESTAR-FS-ARQRENDS

           ADD 1                      TO   ACU-GRV-ARQRENDS.

      *---------------------------------------------------------------*
       5000-99-FIM.   EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       7000-DISPLAY-TOTAIS        SECTION.
      *---------------------------------------------------------------*

           MOVE     ACU-LDS-CADASTRO   TO    WRK-EDIT01
           MOVE     ACU-LDS-ARQRENDE   TO    WRK-EDIT02
           MOVE     ACU-GRV-ARQRENDS   TO    WRK-EDIT05

           DISPLAY '******************** POUP4050 ********************'
           DISPLAY '*                                                *'
           DISPLAY '*  TOTAL REG. LIDOS    - CADASTRO : 'WRK-EDIT01'  *'
           DISPLAY '*  TOTAL REG. LIDOS    - ARQRENDE : 'WRK-EDIT02'  *'
           DISPLAY '*  TOTAL REG. GRAVADOS - ARQRENDS : 'WRK-EDIT05'  *'
           DISPLAY '*                                                *'
           DISPLAY '******************** POUP4050 ********************'.

      *---------------------------------------------------------------*
       7000-99-FIM.   EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       9000-FINALIZAR             SECTION.
      *---------------------------------------------------------------*

           CLOSE  CADASTRO
                  ARQRENDE
                  ARQRENDS.

           MOVE   WRK-FECHAMENTO       TO      WRK-OPERACAO
           PERFORM  1100-TESTAR-FILE-STATUS.

           STOP RUN.

      *---------------------------------------------------------------*
       9000-99-FIM.   EXIT.
      *---------------------------------------------------------------*

      *----------------------------------------------------------------*
       9999-ROTINA-ERRO SECTION.
      *----------------------------------------------------------------*

           MOVE   'POUP4050'           TO  ERR-PGM.
           MOVE   'APL'                TO ERR-TIPO-ACESSO.
           CALL   'BRAD7100'        USING  WRK-BATCH
                                           ERRO-AREA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM. EXIT.
      *----------------------------------------------------------------*


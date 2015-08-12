      *===============================================================*
       IDENTIFICATION DIVISION.
      *===============================================================*

       PROGRAM-ID. POUP4055.
       AUTHOR. FRANCISCO.

      *================================================================*
      *               B R Q   -   I T   S E R V I C E S                *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA     : POUP4055                                     *
      *    ANALISTA     : FERNANDA CARUSO        - BRQ IT SERVICES.    *
      *    PROGRAMADOR  : FRANCISCO FREIRE       - BRQ IT SERVICES.    *
      *    DATA         : 11/08/2015                                   *
      *                                                                *
      *    OBJETIVO     : OBTEM ARQRENDA ATUAL                         *
      *                                                                *
      *                           ARQUIVOS                             *
      *           +---------------------------------------+            *
      *           |  DDNAME  |   I/O   |   BOOK   | LRECL |            *
      *           |----------|---------|----------|-------|            *
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

      *---------------------------------------------------------------*
       INPUT-OUTPUT SECTION.
      *---------------------------------------------------------------*

       FILE-CONTROL.

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
            'INICIO DA WORKING POUP4055'.

      *-- ------------------------------------------------------------*
      *          CAMPOS UTILIZADOS PARA CANCELAR PROCESSAMENTO        *
      *---------------------------------------------------------------*

       01  FILLER.
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
           03  ACU-LDS-ARQRENDE        PIC 9(09) COMP-3    VALUE  ZEROS.
           03  ACU-GRV-ARQRENDS        PIC 9(09) COMP-3    VALUE  ZEROS.
      *
      *----------------------------------------------------------------*
      *--  EDICAO.

       01  FILLER.
           03  WRK-EDIT01              PIC ZZZ.ZZZ.ZZ9     VALUE  ZEROS.
           03  WRK-EDIT02              PIC ZZZ.ZZZ.ZZ9     VALUE  ZEROS.

      *----------------------------------------------------------------*
      *--  AREA AUXILIAR.

       01  WRK-AUX-ARQRENDS.
           05  WRK-AUX-CPF-CNPJ.
               07  WRK-AUX-CPF-CNPJ-NRO     PIC S9(009) COMP-3
                                                           VALUE  ZEROS.
               07  WRK-AUX-CPF-CNPJ-CTR     PIC S9(002) COMP-3
                                                           VALUE  ZEROS.
               07  WRK-AUX-CPF-CNPJ-FIL     PIC S9(004) COMP-3
                                                           VALUE  ZEROS.
           05  WRK-AUX-CCLUB                PIC S9(010) COMP-3
                                                           VALUE  ZEROS.
           05  WRK-AUX-TP-REG               PIC  X(003)
                                                           VALUE SPACES.
           05  WRK-AUX-CPTCAO-ESPAC-TBELA   PIC S9(002) COMP-3
                                                           VALUE  ZEROS.
           05  WRK-AUX-IRZ-SCIAL            PIC  X(070)
                                                           VALUE SPACES.
           05  WRK-AUX-CCNPJ-EMPR           PIC S9(009) COMP-3
                                                           VALUE  ZEROS.
           05  WRK-AUX-CCTRL-CNPJ-EMPR      PIC S9(002) COMP-3
                                                           VALUE  ZEROS.
           05  WRK-AUX-CFLIAL-CNPJ-EMPR     PIC S9(004) COMP-3
                                                           VALUE  ZEROS.
           05  WRK-AUX-CPORTE-EMPR          PIC S9(003) COMP-3
                                                           VALUE  ZEROS.
           05  WRK-AUX-CCOMPS-EMPR          PIC S9(001) COMP-3
                                                           VALUE  ZEROS.
           05  WRK-AUX-CPROFS-PF            PIC S9(005) COMP-3
                                                           VALUE  ZEROS.
           05  WRK-AUX-CCATEG-PROFS-CREDT   PIC S9(005) COMP-3
                                                           VALUE  ZEROS.
           05  WRK-AUX-DINIC-VALDD          PIC  X(010)
                                                           VALUE SPACES.
           05  WRK-AUX-CCARGO-CLI-PF        PIC S9(005) COMP-3
                                                           VALUE  ZEROS.
           05  WRK-AUX-DMES-ANO-ATVDD       PIC S9(006) COMP-3
                                                           VALUE  ZEROS.
           05  WRK-AUX-DADMIS               PIC  X(010)
                                                           VALUE SPACES.
           05  WRK-AUX-CCNPJ-ANTER-CLI      PIC S9(009) COMP-3
                                                           VALUE  ZEROS.
           05  WRK-AUX-CCTRL-CNPJ-ANTER     PIC S9(002) COMP-3
                                                           VALUE  ZEROS.
           05  WRK-AUX-CFLIAL-CNPJ-ANTER    PIC S9(004) COMP-3
                                                           VALUE  ZEROS.
           05  WRK-AUX-IRZ-SCIAL-ANTER      PIC  X(070)
                                                           VALUE SPACES.
           05  WRK-AUX-QANO-EMPR-ANTER      PIC S9(002) COMP-3
                                                           VALUE  ZEROS.
           05  WRK-AUX-QMES-EMPR-ANTER      PIC S9(002) COMP-3
                                                           VALUE  ZEROS.
           05  WRK-AUX-VRENDA-MES        PIC S9(013)V99 COMP-3
                                                           VALUE  ZEROS.
           05  WRK-AUX-VRENDA-INFRD      PIC S9(013)V99 COMP-3
                                                           VALUE  ZEROS.
           05  WRK-AUX-VRENDA-FAMLR-MES  PIC S9(013)V99 COMP-3
                                                           VALUE  ZEROS.
           05  WRK-AUX-CID-COMPV-RENDA      PIC  X(001)
                                                           VALUE SPACES.
           05  WRK-AUX-RDEPTO-ALOC          PIC  X(040)
                                                           VALUE SPACES.
           05  WRK-AUX-CTPO-COMPV-RENDA     PIC S9(002) COMP-3
                                                           VALUE  ZEROS.
           05  WRK-AUX-CEMPR                PIC S9(010) COMP-3
                                                           VALUE  ZEROS.
           05  WRK-AUX-STATUS               PIC  X(01)
                                                           VALUE SPACES.
           05  WRK-AUX-RESERVA              PIC  X(347)
                                                           VALUE SPACES.

       01  WRK-AUX-NUM009              PIC 9(09) VALUE  ZEROS.
       01  WRK-AUX-NUM009-S            REDEFINES
           WRK-AUX-NUM009              PIC S9(09).

      *--  CHAVE ARQRENDE.
       01  WRK-CHV-ARQRENDE-ATU.
           05 WRK-CPF-PRINC            PIC 9(09) VALUE ZEROS.

      *--  CHAVE ARQRENDE.
       01  WRK-CHV-ARQRENDE-ANT.
           05 WRK-CPF-PRINC            PIC 9(09) VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER   PIC  X(32) VALUE '*       AREA DA BRAD7100       *'.
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *---------------------------------------------------------------*
       01   FILLER                     PIC  X(32)    VALUE
            'FIM DA WORKING POUP4055'.

      *===============================================================*
       PROCEDURE DIVISION.
      *===============================================================*

      *---------------------------------------------------------------*
       0000-INICIAR SECTION.
      *---------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 4000-PROCESSAR     UNTIL
                  (WRK-FS-ARQRENDE     EQUAL '10')

           PERFORM 5000-GRAVAR-ARQRENDS

           PERFORM 7000-DISPLAY-TOTAIS

           PERFORM 9000-FINALIZAR.

      *---------------------------------------------------------------*
       0000-99-FIM.   EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1000-INICIALIZAR              SECTION.
      *---------------------------------------------------------------*

           OPEN  INPUT  ARQRENDE
                 OUTPUT ARQRENDS

           MOVE  WRK-ABERTURA          TO    WRK-OPERACAO
           PERFORM  1100-TESTAR-FILE-STATUS.

      *----> LE PRIMEIRO REGISTRO
           PERFORM 3000-LER-ARQRENDE

           IF      WRK-FS-ARQRENDE     EQUAL '10'
                   DISPLAY '**************** POUP4055 ***************'
                   DISPLAY '*                                       *'
                   DISPLAY '* ARQUIVO DE ENTRADA - ARQRENDE - VAZIO *'
                   DISPLAY '*        PROCESSAMENTO ENCERRADO        *'
                   DISPLAY '*                                       *'
                   DISPLAY '**************** POUP4055 ***************'
                   PERFORM 9999-ROTINA-ERRO
           END-IF

           MOVE WRK-CHV-ARQRENDE-ATU   TO WRK-CHV-ARQRENDE-ANT

           MOVE PSDCW033-08-INFO-PROFS OF ARQRENDE
                                       TO WRK-AUX-ARQRENDS.

      *---------------------------------------------------------------*
       1000-99-FIM.   EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS       SECTION.
      *---------------------------------------------------------------*

           PERFORM  1120-TESTAR-FS-ARQRENDE
           PERFORM  1150-TESTAR-FS-ARQRENDS.

      *---------------------------------------------------------------*
       1100-99-FIM.   EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1120-TESTAR-FS-ARQRENDE       SECTION.
      *---------------------------------------------------------------*

           IF WRK-FS-ARQRENDE         NOT EQUAL  '00'
              DISPLAY '************** POUP4055 *************'
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
              DISPLAY '*              ARQRENDE             *'
              DISPLAY '*         FILE STATUS =  ' WRK-FS-ARQRENDE
                                                 '         *'
              DISPLAY '************** POUP4055 *************'
              PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       1120-99-FIM.   EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1150-TESTAR-FS-ARQRENDS        SECTION.
      *---------------------------------------------------------------*

           IF WRK-FS-ARQRENDS           NOT EQUAL  '00'
              DISPLAY '************** POUP4055 *************'
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
              DISPLAY '*              ARQRENDS              *'
              DISPLAY '*         FILE STATUS =  ' WRK-FS-ARQRENDS
                                                 '         *'
              DISPLAY '************** POUP4055 *************'
              PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       1150-99-FIM.   EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3000-LER-ARQRENDE            SECTION.
      *---------------------------------------------------------------*

           MOVE WRK-CHV-ARQRENDE-ATU   TO WRK-CHV-ARQRENDE-ANT

           READ    ARQRENDE

           IF      WRK-FS-ARQRENDE     EQUAL  '10'
                   MOVE HIGH-VALUES    TO      WRK-CHV-ARQRENDE-ATU
                   GO                  TO      3000-99-FIM
           END-IF.

           MOVE    WRK-LEITURA         TO      WRK-OPERACAO
           PERFORM 1120-TESTAR-FS-ARQRENDE

           MOVE PSDCW033-08-CPF-CNPJ-NRO OF ARQRENDE
                                       TO  WRK-AUX-NUM009-S

           MOVE WRK-AUX-NUM009         TO  WRK-CPF-PRINC
                                               OF WRK-CHV-ARQRENDE-ATU.

           ADD     1                   TO      ACU-LDS-ARQRENDE.

      *---------------------------------------------------------------*
       3000-99-FIM.   EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       4000-PROCESSAR               SECTION.
      *---------------------------------------------------------------*

           IF  WRK-CHV-ARQRENDE-ATU  NOT EQUAL WRK-CHV-ARQRENDE-ANT
               PERFORM 5000-GRAVAR-ARQRENDS
               MOVE PSDCW033-08-INFO-PROFS OF ARQRENDE
                                           TO WRK-AUX-ARQRENDS
           ELSE
               IF WRK-AUX-VRENDA-MES EQUAL ZEROS AND
                   PSDCW033-08-VRENDA-MES OF ARQRENDE NOT EQUAL ZEROS
                       MOVE PSDCW033-08-INFO-PROFS OF ARQRENDE
                                               TO WRK-AUX-ARQRENDS
               END-IF
           END-IF.

           PERFORM 3000-LER-ARQRENDE.

      *---------------------------------------------------------------*
       4000-99-FIM.   EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       5000-GRAVAR-ARQRENDS          SECTION.
      *---------------------------------------------------------------*

           MOVE WRK-AUX-ARQRENDS   TO PSDCW033-08-INFO-PROFS OF ARQRENDS

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

           MOVE     ACU-LDS-ARQRENDE   TO    WRK-EDIT01
           MOVE     ACU-GRV-ARQRENDS   TO    WRK-EDIT02

           DISPLAY '******************** POUP4055 ********************'
           DISPLAY '*                                                *'
           DISPLAY '*  TOTAL REG. LIDOS    - ARQRENDE : 'WRK-EDIT01' *'
           DISPLAY '*  TOTAL REG. GRAVADOS - ARQRENDS : 'WRK-EDIT02' *'
           DISPLAY '*                                                *'
           DISPLAY '******************** POUP4055 ********************'.

      *---------------------------------------------------------------*
       7000-99-FIM.   EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       9000-FINALIZAR             SECTION.
      *---------------------------------------------------------------*

           CLOSE  ARQRENDE
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

           MOVE   'POUP4055'           TO  ERR-PGM.
           MOVE   'APL'                TO ERR-TIPO-ACESSO.
           CALL   'BRAD7100'        USING  WRK-BATCH
                                           ERRO-AREA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM. EXIT.
      *----------------------------------------------------------------*


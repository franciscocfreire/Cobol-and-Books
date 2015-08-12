      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. CLLPXXX1.
       AUTHOR.     CESAR S PEREGO.
      *================================================================*
      *             C P M  B R A X I S  -  S I S T E M A S             *
      *----------------------------------------------------------------*
      *    PROGRAMA....: CLLPXXX1                                      *
      *    PROGRAMADOR.: CESAR S PEREGO           - CPM BRAXIS - ALPHA *
      *    ANALISTA....: CESAR S PEREGO           - CPM BRAXIS - ALPHA *
      *    DATA........: 26/07/2011                                    *
      *----------------------------------------------------------------*
      *    OBJETIVO....:                                               *
      *      FORMATAR ARQUIVO DE AVISO DE COBRANCA INCLUINDO OS DADOS  *
      *      NECESSARIOS PARA COMPOR O FORMULARIO.                     *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *      DDNAME                                      INCLUDE/BOOK  *
      *      EMODLAVS                                      RECRWIAA    *
      *      SMODLAVS                                      RECRWIJA    *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *      I#FRWKGE - BOOK DE COMUNICACAO COM FRWK2999.              *
      *      I#FRWKAR - COMMAREA FRWK2999 (LOG DE ERROS ARQUIVO).      *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *      BRAD0450 - MODULO PARA ABENDAR PROGRAMA.                  *
      *      CKRS1000 - CONEXAO COM DB2.                               *
      *      CKRS0105 - EFETUA CONEXAO COM DB2.                        *
      *      FRWK2999 - MODULO PARA TRATAMENTO DE ERROS.               *
      *----------------------------------------------------------------*
ZKA   *    ALTERACAO...: 16.06.2014                                    *
 |    * -SE EXISTIR PELO MENOS 1 CONTRATO QUE TIVER SEGURO PRESTAMISTA *
ZKA   *  NA HORA DE ENVIAR ALIMENTAR COM '1'(WRK-SEG-PRESTAMISTA)      *
      *================================================================*

      *================================================================*
       ENVIRONMENT                     DIVISION.
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

           SELECT  EMODLAVS   ASSIGN   TO  EMODLAVS
                   FILE       STATUS   IS  WRK-FS-EMODLAVS.

           SELECT  SMODLAVS   ASSIGN   TO  SMODLAVS
                   FILE       STATUS   IS  WRK-FS-SMODLAVS.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT : ARQUIVO DE ENTRADA                                  *
      *            ORG. SEQUENCIAL     LRECL = 800                     *
      *----------------------------------------------------------------*

       FD  EMODLAVS
           RECORDING MODE IS F
           BLOCK     CONTAINS 0 RECORDS
           LABEL     RECORD IS STANDARD.

       01  FD-EMODLAVS                 PIC  X(800)         VALUE SPACES.

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE SAIDA                                    *
      *            ORG. SEQUENCIAL     LRECL = 1500                    *
      *----------------------------------------------------------------*

       FD  SMODLAVS
           RECORDING MODE IS F
           BLOCK     CONTAINS 0 RECORDS
           LABEL     RECORD IS STANDARD.

       01  FD-SMODLAVS                 PIC  X(1500)        VALUE SPACES.

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(32)          VALUE
           '*  INICIO DA WORKING CLLPXXX1  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(32)          VALUE
           '*     VARIAVEIS AUXILIARES     *'.
      *----------------------------------------------------------------*

       77  WRK-PROGRAMA                PIC  X(08)          VALUE
           'CLLPXXX1'.

       77  WRK-LOCAL                   PIC  X(04)          VALUE SPACES.

       77  WRK-MASCARA                 PIC  ZZZ.ZZZ.ZZ9    VALUE ZEROS.

       77  WRK-MASCARA-SQLCODE         PIC  ZZZ.ZZZ.ZZ9-   VALUE ZEROS.

       77  WRK-VALOR-CONTR-ACUM        PIC  9(15)V99 COMP-3 VALUE ZEROS.
       77  WRK-VALOR-CONTR-AUX         PIC  9(15)V99 COMP-3 VALUE ZEROS.
       77  WRK-QTDE-OCORRENCIAS        PIC  9(03)    COMP-3 VALUE ZEROS.

ZKA    77  WRK-SEG-PRESTAMISTA         PIC  9(01)           VALUE ZEROS.

       01  WRK-LOCAL-DATA-FORM.
           05 WRK-LOCAL-FORM           PIC  X(47)          VALUE SPACES.
DIEGOC     05 FILLER                   PIC  X(03)          VALUE SPACES.
           05 WRK-DATA-FORM            PIC  X(10)          VALUE SPACES.

       01  WRK-CONTA-FORM-X.
           05 WRK-CONTA-FORM           PIC  9(13)          VALUE ZEROS.
           05 FILLER                   PIC  X(37)          VALUE SPACES.

       01  WRK-ENDER-NUM-FORM.
           05 WRK-ENDER-FORM           PIC  X(43)          VALUE SPACES.
           05 WRK-NUM-FORM             PIC  X(07)          VALUE SPACES.

       01  WRK-TP-CLIENTE-3            PIC  9(03)          VALUE ZEROS.
       01  FILLER REDEFINES WRK-TP-CLIENTE-3.
           05  FILLER                  PIC  9(02).
           05  WRK-TP-CLIENTE-1        PIC  9(01).

       01  WRK-DATA-ATUAL              PIC  X(10)          VALUE SPACES.
       01  FILLER REDEFINES WRK-DATA-ATUAL.
           05  WRK-DIA-ATUAL           PIC  9(02).
           05  FILLER                  PIC  X(01).
           05  WRK-MES-ATUAL           PIC  9(02).
           05  FILLER                  PIC  X(01).
           05  WRK-ANO-ATUAL           PIC  9(04).

       01  WRK-CODIGO-BARRA-AUX.
           05 WRK-DOCUMENTO-BR.
              10 WRK-NUM-DOCUMENTO-BR  PIC  9(09)          VALUE ZEROS.
              10 WRK-NUM-FILIAL-BR     PIC  9(04)          VALUE ZEROS.
              10 WRK-NUM-DIGITO-BR     PIC  9(02)          VALUE ZEROS.
           05 WRK-DT-MOVIMENTO-BR.
              10 WRK-DT-DIA-BR         PIC  9(02)          VALUE ZEROS.
              10 WRK-DT-MES-BR         PIC  9(02)          VALUE ZEROS.
              10 WRK-DT-ANO-BR         PIC  9(04)          VALUE ZEROS.
           05 WRK-AGENCIA-BR           PIC  9(05)          VALUE ZEROS.
           05 WRK-PARTICIPANTE-BR      PIC  9(01)          VALUE ZEROS.
           05 WRK-MOTIVO-DEVOLUCAO     PIC  X(02)          VALUE SPACES.
       01  FILLER REDEFINES  WRK-CODIGO-BARRA-AUX.
           05  WRK-CODIGO-BARRA        PIC  X(31).

       01  WRK-CHAVE-AVISO-RC1.
           05 WRK-CHV-RC1-CCLUB        PIC  9(10)          VALUE ZEROS.
           05 WRK-CHV-RC1-CCLUB-TIT    PIC  9(10)          VALUE ZEROS.
           05 WRK-CHV-RC1-CAG-CONTR    PIC  9(05)          VALUE ZEROS.
           05 WRK-CHV-RC1-CCTA-CONTR   PIC  9(13)          VALUE ZEROS.
       01  WRK-CHAVE-AVS-RC1-ANT       PIC  X(38)          VALUE SPACES.

       01  WRK-CHAVE-AVISO-RC2.
           05 WRK-CHV-RC2-CCLUB        PIC  9(10)          VALUE ZEROS.
           05 WRK-CHV-RC2-CCLUB-TIT    PIC  9(10)          VALUE ZEROS.
           05 WRK-CHV-RC2-CAG-CONTR    PIC  9(05)          VALUE ZEROS.
           05 WRK-CHV-RC2-CCTA-CONTR   PIC  9(13)          VALUE ZEROS.
       01  WRK-CHAVE-AVS-RC2-ANT       PIC  X(38)          VALUE SPACES.

       01  WRK-CHAVE-AVISO-RC3.
           05 WRK-CHV-RC3-CCLUB        PIC  9(10)          VALUE ZEROS.
           05 WRK-CHV-RC3-CAG-CONTR    PIC  9(05)          VALUE ZEROS.
           05 WRK-CHV-RC3-CCTA-CONTR   PIC  9(13)          VALUE ZEROS.
       01  WRK-CHAVE-AVS-RC3-ANT       PIC  X(28)          VALUE SPACES.

       01  WRK-CHAVE-AVISO-RC4.
           05 WRK-CHV-RC4-CCLUB        PIC  9(10)          VALUE ZEROS.
       01  WRK-CHAVE-AVS-RC4-ANT       PIC  X(10)          VALUE SPACES.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(32)          VALUE
           '*         ACUMULADORES         *'.
      *----------------------------------------------------------------*

       77  ACU-LIDOS-EMODLAVS          PIC  9(09)  COMP-3  VALUE ZEROS.
       77  ACU-GRAVADOS-SMODLAVS       PIC  9(09)  COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(32)          VALUE
           '*         INDEXADORES          *'.
      *----------------------------------------------------------------*

       77  IND-1                       PIC  9(03)  COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(32)          VALUE
           '* AREA DE REGISTRO DE EMODLAVS *'.
      *----------------------------------------------------------------*

       01  WRK-AREA-RECRWIAA.
           COPY 'RECRWIAA'.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(32)          VALUE
           '* AREA DE REGISTRO DE SMODLAVS *'.
      *----------------------------------------------------------------*

       01  WRK-AREA-RECRWIJA.
           COPY 'RECRWIJA'.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(32)          VALUE
           '* AREA DE FORMULARIOS DE AVISO *'.
      *----------------------------------------------------------------*

       01  WRK-AREA-AVISO-RC001.
           COPY RECRW0AA.

       01  WRK-AREA-AVISO-RC002.
           COPY RECRW0BA.

       01  WRK-AREA-AVISO-RC003.
           COPY RECRW0CA.

       01  WRK-AREA-AVISO-RC004.
           COPY RECRW0DA.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(32)          VALUE
           '*     AREA PARA FILE-STATUS    *'.
      *----------------------------------------------------------------*

       01  WRK-FS-EMODLAVS             PIC  X(02)          VALUE SPACES.
       01  WRK-FS-SMODLAVS             PIC  X(02)          VALUE SPACES.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(32)          VALUE
           '*       AREA DA BRAD0450       *'.
      *----------------------------------------------------------------*

       77  WRK-ABEND-0450              PIC S9(04)  COMP    VALUE +1111.
       77  WRK-DUMP-0450               PIC  X(01)          VALUE 'S'.
       01  WRK-ABEND                   PIC  X(01)          VALUE SPACES.
           88  WRK-ABENDAR                                 VALUE 'S'.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)          VALUE
           '* AREA DE COMUNICACAO DO PROGRAMA CKRS0105 *'.
      *----------------------------------------------------------------*

       77  WRK-CKRS0105                PIC  X(08)          VALUE
           'CKRS0105'.

       COPY 'I#CKRS04'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '* AREA DE COMUNICACAO COM O MODULO DE DATAS *'.
      *----------------------------------------------------------------*

       77  WRK-RECR2DAO                PIC  X(08)          VALUE
           'RECR2DAO'.

       01  WRK-AREA-RECR2DAO.
           COPY RECRWDAA.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)          VALUE
           '* AREA DE COMUNICACAO DO PROGRAMA FRWK2999 *'.
      *----------------------------------------------------------------*

       77  WRK-FRWK2999                PIC  X(08)          VALUE
           'FRWK2999'.

       01  WRK-AREA-ERRO.
           COPY 'I#FRWKGE'.
           05 WRK-BLOCO-INFO-ERRO      VALUE SPACES.
              10 WRK-CHAR-INFO-ERRO    PIC X(01) OCCURS 0 TO 30000 TIMES
                                       DEPENDING ON FRWKGHEA-TAM-DADOS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)          VALUE
           '* AREA DE TRATAMENTO DE ERROS DE ARQUIVO *'.
      *----------------------------------------------------------------*

       01  WRK-AREA-ERRO-ARQ.
           COPY 'I#FRWKAR'.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)          VALUE
           '* AREA DE TRATAMENTO DE ERROS DE MODULO *'.
      *----------------------------------------------------------------*

       01  WRK-AREA-ERRO-MOD.
           COPY 'I#FRWKMD'.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)          VALUE
           '* AREA DE TRATAMENTO DE ERRO DB2 *'.
      *----------------------------------------------------------------*

       01  WRK-AREA-ERRO-DB2.
           COPY 'I#FRWKDB'.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(32)          VALUE
           '*    FIM DA WORKING CLLPXXX1   *'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
      * ROTINA PRINCIPAL DO PROGRAMA                                   *
      *----------------------------------------------------------------*
       0000-PRINCIPAL                  SECTION.
      *----------------------------------------------------------------*

           CALL 'CKRS1000'.

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-VERIFICAR-VAZIO.

           PERFORM 3000-PROCESSAR
             UNTIL WRK-FS-EMODLAVS     EQUAL '10'.

           PERFORM 4000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA DE INICIALIZACAO DO PROGRAMA                            *
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           INITIALIZE FRWKGHEA-REGISTRO
                      FRWKGARQ-REGISTRO.

           DISPLAY 'CLLPXXX1 - INICIO DO PROCESSAMENTO'.

           MOVE '1000-INICIALIZAR'     TO  FRWKGHEA-IDEN-PARAGRAFO.
           MOVE '0010'                 TO  WRK-LOCAL.

           PERFORM 7000-INICIALIZAR-CKRS0105.

           SET   ARQ-OPEN              TO  TRUE.

           OPEN  INPUT  EMODLAVS
                 OUTPUT SMODLAVS.

           PERFORM 1100-TESTAR-FILE-STATUS.

           PERFORM 1200-OBTER-DATA-MOVIMENTO.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA PARA TESTAR FILE STATUS DOS ARQUIVOS
      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 1110-TESTAR-FS-EMODLAVS.

           PERFORM 1120-TESTAR-FS-SMODLAVS.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * TESTAR FILE STATUS EMODLAVS                                    *
      *----------------------------------------------------------------*
       1110-TESTAR-FS-EMODLAVS         SECTION.
      *----------------------------------------------------------------*

           IF (WRK-FS-EMODLAVS         NOT EQUAL '00' AND '10')
               MOVE  WRK-FS-EMODLAVS   TO  FRWKGARQ-FILE-STATUS
               MOVE 'EMODLAVS'         TO  FRWKGARQ-NOME-ARQUIVO
               SET   WRK-ABENDAR       TO  TRUE
               PERFORM 9000-ERRO-ARQ
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * TESTAR FILE STATUS SMODLAVS                                    *
      *----------------------------------------------------------------*
       1120-TESTAR-FS-SMODLAVS         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-SMODLAVS         NOT EQUAL '00'
               MOVE  WRK-FS-SMODLAVS   TO  FRWKGARQ-FILE-STATUS
               MOVE 'SMODLAVS'         TO  FRWKGARQ-NOME-ARQUIVO
               SET   WRK-ABENDAR       TO  TRUE
               PERFORM 9000-ERRO-ARQ
           END-IF.

      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA PARA OBTER A DATA DO SISTEMA
      *----------------------------------------------------------------*
       1200-OBTER-DATA-MOVIMENTO       SECTION.
      *----------------------------------------------------------------*

           MOVE '1200-OBTER-DATA-MOVIMENTO'
                                       TO  FRWKGHEA-IDEN-PARAGRAFO.

           CALL  WRK-RECR2DAO       USING  WRK-AREA-RECR2DAO.

           EVALUATE RECRWDAA-COD-RETORNO
             WHEN ZEROS
               CONTINUE

             WHEN 8
               MOVE  WRK-RECR2DAO      TO  FRWKGMOD-NOME-MODULO
               MOVE  RECRWDAA-BLOCO-RETORNO
                                       TO  FRWKGMOD-BLOCO-RETORNO
               PERFORM 9100-ERRO-MODULO

             WHEN 12
               MOVE  WRK-RECR2DAO      TO  FRWKGHEA-NOME-PROGRAMA
               MOVE  RECRWDAA-AREA-ERRO-DB2
                                       TO  WRK-AREA-ERRO-DB2
               PERFORM 9200-ERRO-DB2

           END-EVALUATE.

           MOVE  RECRWDAA-DT-MVTO-P-DDMMAAAA
                                       TO  WRK-DATA-ATUAL.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA PARA VERIFICAR SE O ARQUIVO DE ENTRADA ESTA VAZIO       *
      *----------------------------------------------------------------*
       2000-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-LER-EMODLAVS.

           IF  WRK-FS-EMODLAVS         EQUAL '10'
               DISPLAY '******** PROGRAMA CLLPXXX1 ********'
               DISPLAY '*                                 *'
               DISPLAY '*      ARQUIVO EMODLAVS VAZIO     *'
               DISPLAY '*                                 *'
               DISPLAY '******** PROGRAMA CLLPXXX1 ********'
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * LEITURA DO ARQUIVO EMODLAVS                                    *
      *----------------------------------------------------------------*
       2100-LER-EMODLAVS               SECTION.
      *----------------------------------------------------------------*

           MOVE '2100-LER-EMODLAVS'    TO  FRWKGHEA-IDEN-PARAGRAFO.
           MOVE '0020'                 TO  WRK-LOCAL.
           SET   ARQ-READ              TO  TRUE.

           READ  EMODLAVS            INTO  WRK-AREA-RECRWIAA.

           PERFORM 1110-TESTAR-FS-EMODLAVS.

           IF  WRK-FS-EMODLAVS         EQUAL '00'
               ADD   1                 TO  ACU-LIDOS-EMODLAVS
               PERFORM 2110-CARREGAR-CHV-TPO-AVISO
           ELSE
               MOVE  HIGH-VALUES       TO  WRK-CHAVE-AVISO-RC1
                                           WRK-CHAVE-AVISO-RC2
                                           WRK-CHAVE-AVISO-RC3
                                           WRK-CHAVE-AVISO-RC4
           END-IF.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA PARA CARREGAR A CHAVE DE CONTROLE DO PROGRAMA DE ACORDO *
      * COM O AVISO DE COBRANCA                                        *
      *----------------------------------------------------------------*
       2110-CARREGAR-CHV-TPO-AVISO     SECTION.
      *----------------------------------------------------------------*

           EVALUATE  RECRWIAA-MODELO-AVISO
             WHEN  'RC001   '
               MOVE  RECRWIAA-CCLUB    TO  WRK-CHV-RC1-CCLUB
               MOVE  RECRWIAA-CCLUB-TITULAR
                                       TO  WRK-CHV-RC1-CCLUB-TIT
               MOVE  RECRWIAA-CAG-CONTR-ORIGE
                                       TO  WRK-CHV-RC1-CAG-CONTR
               MOVE  RECRWIAA-CCTA-CONTR-ORIGE
                                       TO  WRK-CHV-RC1-CCTA-CONTR

             WHEN  'RC002   '
               MOVE  RECRWIAA-CCLUB    TO  WRK-CHV-RC2-CCLUB
               MOVE  RECRWIAA-CCLUB-TITULAR
                                       TO  WRK-CHV-RC2-CCLUB-TIT
               MOVE  RECRWIAA-CAG-CONTR-ORIGE
                                       TO  WRK-CHV-RC2-CAG-CONTR
               MOVE  RECRWIAA-CCTA-CONTR-ORIGE
                                       TO  WRK-CHV-RC2-CCTA-CONTR

             WHEN  'RC003   '
               MOVE  RECRWIAA-CCLUB    TO  WRK-CHV-RC3-CCLUB
               MOVE  RECRWIAA-CAG-CONTR-ORIGE
                                       TO  WRK-CHV-RC3-CAG-CONTR
               MOVE  RECRWIAA-CCTA-CONTR-ORIGE
                                       TO  WRK-CHV-RC3-CCTA-CONTR

             WHEN  'RC004   '
               MOVE  RECRWIAA-CCLUB    TO  WRK-CHV-RC4-CCLUB

             WHEN  OTHER
               PERFORM 2100-LER-EMODLAVS

           END-EVALUATE.

      *----------------------------------------------------------------*
       2110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA DE PROCESSAMENTO DO PROGRAMA                            *
      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 3001-INICIALIZAR-VARIAVEIS.

           PERFORM 3050-MOVER-CODIGO-BARRA.

           EVALUATE  RECRWIAA-MODELO-AVISO
             WHEN  'RC001   '
               PERFORM 3100-TRATAR-TIPO-AVISO-RC001

             WHEN  'RC002   '
               PERFORM 3200-TRATAR-TIPO-AVISO-RC002

             WHEN  'RC003   '
               PERFORM 3300-TRATAR-TIPO-AVISO-RC003

             WHEN  'RC004   '
               PERFORM 3400-TRATAR-TIPO-AVISO-RC004

           END-EVALUATE.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA PARA INICIALIZAR AS VARIAVEIS AUXILIARES                *
      *----------------------------------------------------------------*
       3001-INICIALIZAR-VARIAVEIS      SECTION.
      *----------------------------------------------------------------*

           MOVE  SPACES                TO  RECRWIJA-AREA-DADOS-DICD
                                           WRK-LOCAL-FORM
                                           WRK-DATA-FORM
                                           WRK-ENDER-FORM
                                           WRK-NUM-FORM.

           MOVE  ZEROS                 TO  WRK-VALOR-CONTR-ACUM
                                           WRK-VALOR-CONTR-AUX
                                           WRK-CONTA-FORM
                                           WRK-QTDE-OCORRENCIAS.

           MOVE  RECRWIAA-REGISTRO(1:797)
                                       TO  RECRWIJA-REGISTRO.

      *----------------------------------------------------------------*
       3001-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA QUE FAZ A MOVIMETACAO DO CODIGO DE BARRA.               *
      *----------------------------------------------------------------*
       3050-MOVER-CODIGO-BARRA         SECTION.
      *----------------------------------------------------------------*

           MOVE  RECRWIAA-CPF-CNPJ-NRO TO  WRK-NUM-DOCUMENTO-BR.
           MOVE  RECRWIAA-CPF-CNPJ-FIL TO  WRK-NUM-FILIAL-BR.
           MOVE  RECRWIAA-CPF-CNPJ-CTR TO  WRK-NUM-DIGITO-BR.

           MOVE  WRK-DIA-ATUAL         TO  WRK-DT-DIA-BR.
           MOVE  WRK-MES-ATUAL         TO  WRK-DT-MES-BR.
           MOVE  WRK-ANO-ATUAL         TO  WRK-DT-ANO-BR.

           MOVE  RECRWIAA-CAG-CONTR-ORIGE
                                       TO  WRK-AGENCIA-BR.

           MOVE  RECRWIAA-CTPO-PRTCP-PSSOA
                                       TO  WRK-TP-CLIENTE-3.
           MOVE  WRK-TP-CLIENTE-1      TO  WRK-PARTICIPANTE-BR.

           MOVE  SPACES                TO  WRK-MOTIVO-DEVOLUCAO.

      *----------------------------------------------------------------*
       3050-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA PARA TRATAR O TIPO DE AVISO DE COBRANCA RC001           *
      *----------------------------------------------------------------*
       3100-TRATAR-TIPO-AVISO-RC001    SECTION.
      *----------------------------------------------------------------*

           MOVE  RECRWIAA-ICIDDE-ENDER-UORG
                                       TO  WRK-LOCAL-FORM.
           MOVE  WRK-DATA-ATUAL        TO  WRK-DATA-FORM.
           MOVE  WRK-LOCAL-DATA-FORM   TO  LOCAL-DATA
                                       OF  WRK-AREA-AVISO-RC001.

DCM        MOVE  RECRWIAA-IPSSOA-COPLT-TITULAR
                                       TO  NM-CLIENTE
                                       OF  WRK-AREA-AVISO-RC001.

      *   XMOVE  WRK-CODIGO-BARRA     XTO  CODIGO-DEVOLUCAO
      *                               XOF  WRK-AREA-AVISO-RC001.
      *
      *----------------------------------------------------------------*
      *--> ENDERECO DA AGENCIA
      *   XMOVE  RECRWIAA-CAG-CONTR-ORIGE
      *                               XTO  CODIGO-AGENCIA
      *                               XOF  WRK-AREA-AVISO-RC001.
      *   XMOVE  RECRWIAA-CDIG-UND-ORGNZ
      *                               XTO  DIGITO-AGENCIA
      *                               XOF  WRK-AREA-AVISO-RC001.
      *   XMOVE  RECRWIAA-NM-AGENCIA-UORG(1:20)
      *                               XTO  NOME-AGENCIA
      *                               XOF  WRK-AREA-AVISO-RC001.
      *
      *----------------------------------------------------------------*
      *--> IDENTIFICACAO DA EMPRESA
           IF  RECRWIAA-CPSSOA-JURID-CONTR
                                       EQUAL 26788691
               MOVE  2                 TO  IMPRIME-EMPRESA
                                       OF  WRK-AREA-AVISO-RC001
           ELSE
               MOVE  1                 TO  IMPRIME-EMPRESA
                                       OF  WRK-AREA-AVISO-RC001
           END-IF.

      *----------------------------------------------------------------*

           MOVE  WRK-CHAVE-AVISO-RC1   TO  WRK-CHAVE-AVS-RC1-ANT.

           PERFORM VARYING IND-1       FROM  1 BY 1
             UNTIL         IND-1       GREATER    10
                OR WRK-CHAVE-AVISO-RC1 NOT EQUAL  WRK-CHAVE-AVS-RC1-ANT

              MOVE RECRWIAA-DESC-NATUREZA(1:20)
                                       TO  DS-NATUREZA-ACAO-REST
                                       OF  WRK-AREA-AVISO-RC001(IND-1)
              MOVE RECRWIAA-DBASE-INADP-CONTR
                                       TO  DT-VENCTO-PARC-ATRS
                                       OF  WRK-AREA-AVISO-RC001(IND-1)
              MOVE RECRWIAA-VVENCD-CONTR
                                       TO  WRK-VALOR-CONTR-AUX
                                           VR-VENCIDO-CONTRATO
                                       OF  WRK-AREA-AVISO-RC001(IND-1)
              ADD  WRK-VALOR-CONTR-AUX TO  WRK-VALOR-CONTR-ACUM
              ADD  1                   TO  WRK-QTDE-OCORRENCIAS

              PERFORM 2100-LER-EMODLAVS
           END-PERFORM.

           MOVE  WRK-QTDE-OCORRENCIAS  TO  QTDE-OCORRENCIAS-2
                                       OF  WRK-AREA-AVISO-RC001.

           MOVE  WRK-VALOR-CONTR-ACUM  TO  VR-TOTAL-ACUMULADO
                                       OF  WRK-AREA-AVISO-RC001.

           MOVE  WRK-AREA-AVISO-RC001  TO  RECRWIJA-AREA-DADOS-DICD.

           PERFORM 3500-GRAVAR-SMODLAVS.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA PARA TRATAR O TIPO DE AVISO DE COBRANCA RC002           *
      *----------------------------------------------------------------*
       3200-TRATAR-TIPO-AVISO-RC002    SECTION.
      *----------------------------------------------------------------*

           MOVE  RECRWIAA-ICIDDE-ENDER-UORG
                                       TO  WRK-LOCAL-FORM.
           MOVE  WRK-DATA-ATUAL        TO  WRK-DATA-FORM.
           MOVE  WRK-LOCAL-DATA-FORM   TO  LOCAL-DATA
                                       OF  WRK-AREA-AVISO-RC002.

DCM        MOVE  RECRWIAA-IPSSOA-COPLT-TITULAR
                                       TO  NM-CLIENTE
                                       OF  WRK-AREA-AVISO-RC002.

      *   XMOVE  WRK-CODIGO-BARRA     XTO  CODIGO-DEVOLUCAO
      *                               XOF  WRK-AREA-AVISO-RC002.
      *
      *----------------------------------------------------------------*
      *--> ENDERECO DA AGENCIA
      *   XMOVE  RECRWIAA-CAG-CONTR-ORIGE
      *                               XTO  CODIGO-AGENCIA
      *                               XOF  WRK-AREA-AVISO-RC002.
      *   XMOVE  RECRWIAA-CDIG-UND-ORGNZ
      *                               XTO  DIGITO-AGENCIA
      *                               XOF  WRK-AREA-AVISO-RC002.
           MOVE  RECRWIAA-NM-AGENCIA-UORG(1:20)
                                       TO  NOME-AGENCIA
                                       OF  WRK-AREA-AVISO-RC002.

           MOVE  RECRWIAA-ELOGDR-PSSO-UORG(1:25)
                                       TO  ENDE-AGENCIA
                                       OF  WRK-AREA-AVISO-RC002.

           MOVE  RECRWIAA-ICIDDE-ENDER-UORG(1:17)
                                       TO  NOME-CIDADE
                                       OF  WRK-AREA-AVISO-RC002.

      *----------------------------------------------------------------*
      *--> IDENTIFICACAO DA EMPRESA
           IF  RECRWIAA-CPSSOA-JURID-CONTR
                                       EQUAL 26788691
               MOVE  2                 TO  IMPRIME-EMPRESA
                                       OF  WRK-AREA-AVISO-RC002
           ELSE
               MOVE  1                 TO  IMPRIME-EMPRESA
                                       OF  WRK-AREA-AVISO-RC002
           END-IF.

      *----------------------------------------------------------------*

           MOVE  RECRWIAA-CCTA-CONTR-ORIGE
                                       TO  WRK-CONTA-FORM.

DIEGOC     INSPECT WRK-CONTA-FORM-X REPLACING LEADING ZEROS BY ' '.

           MOVE  WRK-CONTA-FORM-X      TO  CONTA-BCRIA
                                       OF  WRK-AREA-AVISO-RC002.

           MOVE  WRK-CHAVE-AVISO-RC2   TO  WRK-CHAVE-AVS-RC2-ANT.

           PERFORM VARYING IND-1       FROM  1 BY 1
             UNTIL         IND-1       GREATER    7
                OR WRK-CHAVE-AVISO-RC2 NOT EQUAL  WRK-CHAVE-AVS-RC2-ANT

              MOVE RECRWIAA-DESC-NATUREZA(1:20)
                                       TO  DS-NATUREZA-ACAO-REST
                                       OF  WRK-AREA-AVISO-RC002(IND-1)
              MOVE RECRWIAA-DBASE-INADP-CONTR
                                       TO  DT-VENCTO-PARC-ATRS
                                       OF  WRK-AREA-AVISO-RC002(IND-1)
              MOVE RECRWIAA-VVENCD-CONTR
                                       TO  WRK-VALOR-CONTR-AUX
                                           VR-VENCIDO-CONTRATO
                                       OF  WRK-AREA-AVISO-RC002(IND-1)
              ADD  WRK-VALOR-CONTR-AUX TO  WRK-VALOR-CONTR-ACUM
              ADD  1                   TO  WRK-QTDE-OCORRENCIAS

              PERFORM 2100-LER-EMODLAVS
           END-PERFORM.

           MOVE  WRK-QTDE-OCORRENCIAS  TO  QTDE-OCORRENCIAS-3
                                       OF  WRK-AREA-AVISO-RC002.

           MOVE  WRK-VALOR-CONTR-ACUM  TO  VR-TOTAL-ACUMULADO
                                       OF  WRK-AREA-AVISO-RC002.

           MOVE  WRK-AREA-AVISO-RC002  TO  RECRWIJA-AREA-DADOS-DICD.

           PERFORM 3500-GRAVAR-SMODLAVS.

      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA PARA TRATAR O TIPO DE AVISO DE COBRANCA RC003           *
      *----------------------------------------------------------------*
       3300-TRATAR-TIPO-AVISO-RC003    SECTION.
      *----------------------------------------------------------------*

           MOVE  RECRWIAA-ICIDDE-ENDER-UORG
                                       TO  WRK-LOCAL-FORM.
           MOVE  WRK-DATA-ATUAL        TO  WRK-DATA-FORM.
           MOVE  WRK-LOCAL-DATA-FORM   TO  LOCAL-DATA
                                       OF  WRK-AREA-AVISO-RC003.

DCM        MOVE  RECRWIAA-IPSSOA-COPLT-TITULAR
                                       TO  NM-CLIENTE
                                       OF  WRK-AREA-AVISO-RC003.

      *   XMOVE  WRK-CODIGO-BARRA     XTO  CODIGO-DEVOLUCAO
      *                               XOF  WRK-AREA-AVISO-RC003.
      *
      *----------------------------------------------------------------*
      *--> ENDERECO DA AGENCIA
      *   XMOVE  RECRWIAA-CAG-CONTR-ORIGE
      *                               XTO  CODIGO-AGENCIA
      *                               XOF  WRK-AREA-AVISO-RC003.
      *   XMOVE  RECRWIAA-CDIG-UND-ORGNZ
      *                               XTO  DIGITO-AGENCIA
      *                               XOF  WRK-AREA-AVISO-RC003.
           MOVE  RECRWIAA-NM-AGENCIA-UORG(1:20)
                                       TO  NOME-AGENCIA
                                       OF  WRK-AREA-AVISO-RC003.

           MOVE  RECRWIAA-ELOGDR-PSSO-UORG(1:25)
                                       TO  ENDE-AGENCIA
                                       OF  WRK-AREA-AVISO-RC003.

           MOVE  RECRWIAA-ICIDDE-ENDER-UORG(1:17)
                                       TO  CIDADE-AGENCIA
                                       OF  WRK-AREA-AVISO-RC003.

      *----------------------------------------------------------------*
      *--> IDENTIFICACAO DA EMPRESA
           IF  RECRWIAA-CPSSOA-JURID-CONTR
                                       EQUAL 26788691
               MOVE  2                 TO  IMPRIME-EMPRESA
                                       OF  WRK-AREA-AVISO-RC003
           ELSE
               MOVE  1                 TO  IMPRIME-EMPRESA
                                       OF  WRK-AREA-AVISO-RC003
           END-IF.

           MOVE  RECRWIAA-CCTA-CONTR-ORIGE
                                       TO  WRK-CONTA-FORM.

DIEGOC     INSPECT WRK-CONTA-FORM-X REPLACING LEADING ZEROS BY ' '.

           MOVE  WRK-CONTA-FORM-X      TO  CONTA-BCRIA
                                       OF  WRK-AREA-AVISO-RC003.

           MOVE  WRK-CHAVE-AVISO-RC3   TO  WRK-CHAVE-AVS-RC3-ANT.

ZKA        MOVE  ZEROS                 TO  WRK-SEG-PRESTAMISTA

           PERFORM VARYING IND-1       FROM  1 BY 1
             UNTIL         IND-1       GREATER    9
                OR WRK-CHAVE-AVISO-RC3 NOT EQUAL  WRK-CHAVE-AVS-RC3-ANT

              MOVE RECRWIAA-DESC-NATUREZA(1:20)
                                       TO  DS-NATUREZA-ACAO-REST
                                       OF  WRK-AREA-AVISO-RC003(IND-1)
              MOVE RECRWIAA-DBASE-INADP-CONTR
                                       TO  DT-VENCTO-PARC-ATRS
                                       OF  WRK-AREA-AVISO-RC003(IND-1)
              MOVE RECRWIAA-VVENCD-CONTR
                                       TO  WRK-VALOR-CONTR-AUX
                                           VR-VENCIDO-CONTRATO
                                       OF  WRK-AREA-AVISO-RC003(IND-1)
              ADD  WRK-VALOR-CONTR-AUX TO  WRK-VALOR-CONTR-ACUM
              ADD  1                   TO  WRK-QTDE-OCORRENCIAS

ZKA************ VERIFICA SE EXISTE PELO MENOS 1 CONTRATO PRESTAMISTA
              IF  RECRWIAA-SEG-PRESTAMISTA
                                       EQUAL 1
                  MOVE RECRWIAA-SEG-PRESTAMISTA
                                       TO  WRK-SEG-PRESTAMISTA
              END-IF

              PERFORM 2100-LER-EMODLAVS
           END-PERFORM.

ZKA   *----------------------------------------------------------------*
 |    *--> IDENTIFICACAO DE SEGURO PRESTAMISTA
 |
 |         MOVE  WRK-SEG-PRESTAMISTA   TO  IMPRIME-PRESTAMISTA
 |                                     OF  WRK-AREA-AVISO-RC003.
 |
ZKA   *----------------------------------------------------------------*

           MOVE  WRK-QTDE-OCORRENCIAS  TO  QTDE-OCORRENCIAS-1
                                       OF  WRK-AREA-AVISO-RC003.

           MOVE  WRK-VALOR-CONTR-ACUM  TO  VR-TOTAL-ACUMULADO
                                       OF  WRK-AREA-AVISO-RC003.

           MOVE  WRK-AREA-AVISO-RC003  TO  RECRWIJA-AREA-DADOS-DICD.

           PERFORM 3500-GRAVAR-SMODLAVS.

      *----------------------------------------------------------------*
       3300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA PARA TRATAR O TIPO DE AVISO DE COBRANCA RC004           *
      *----------------------------------------------------------------*
       3400-TRATAR-TIPO-AVISO-RC004    SECTION.
      *----------------------------------------------------------------*

           MOVE  RECRWIAA-ICIDDE-ENDER-UORG
                                       TO  WRK-LOCAL-FORM.
           MOVE  WRK-DATA-ATUAL        TO  WRK-DATA-FORM.
           MOVE  WRK-LOCAL-DATA-FORM   TO  LOCAL-DATA
                                       OF  WRK-AREA-AVISO-RC004.

DCM        MOVE  RECRWIAA-IPSSOA-COPLT-TITULAR
                                       TO  NM-CLIENTE
                                       OF  WRK-AREA-AVISO-RC004.

      *   XMOVE  WRK-CODIGO-BARRA     XTO  CODIGO-DEVOLUCAO
      *                               XOF  WRK-AREA-AVISO-RC004.
      *
      *----------------------------------------------------------------*
      *--> ENDERECO DA AGENCIA
      *   XMOVE  RECRWIAA-CAG-CONTR-ORIGE
      *                               XTO  CODIGO-AGENCIA
      *                               XOF  WRK-AREA-AVISO-RC004.
      *   XMOVE  RECRWIAA-CDIG-UND-ORGNZ
      *                               XTO  DIGITO-AGENCIA
      *                               XOF  WRK-AREA-AVISO-RC004.
      *   XMOVE  RECRWIAA-NM-AGENCIA-UORG(1:20)
      *                               XTO  NOME-AGENCIA
      *                               XOF  WRK-AREA-AVISO-RC004.
      *
      *----------------------------------------------------------------*
      *--> IDENTIFICACAO DA EMPRESA
           IF  RECRWIAA-CPSSOA-JURID-CONTR
                                       EQUAL 26788691
               MOVE  2                 TO  IMPRIME-EMPRESA
                                       OF  WRK-AREA-AVISO-RC004
           ELSE
               MOVE  1                 TO  IMPRIME-EMPRESA
                                       OF  WRK-AREA-AVISO-RC004
           END-IF.


           MOVE  WRK-CHAVE-AVISO-RC4   TO  WRK-CHAVE-AVS-RC4-ANT.

ZKA        MOVE  ZEROS                 TO  WRK-SEG-PRESTAMISTA

           PERFORM VARYING IND-1       FROM  1 BY 1
             UNTIL         IND-1       GREATER    10
                OR WRK-CHAVE-AVISO-RC4 NOT EQUAL  WRK-CHAVE-AVS-RC4-ANT

              MOVE RECRWIAA-DESC-NATUREZA(1:20)
                                       TO  DS-NATUREZA-ACAO-REST
                                       OF  WRK-AREA-AVISO-RC004(IND-1)
              MOVE RECRWIAA-DBASE-INADP-CONTR
                                       TO  DT-VENCTO-PARC-ATRS
                                       OF  WRK-AREA-AVISO-RC004(IND-1)
              MOVE RECRWIAA-VVENCD-CONTR
                                       TO  WRK-VALOR-CONTR-AUX
                                           VR-VENCIDO-CONTRATO
                                       OF  WRK-AREA-AVISO-RC004(IND-1)
              ADD  WRK-VALOR-CONTR-AUX TO  WRK-VALOR-CONTR-ACUM
              ADD  1                   TO  WRK-QTDE-OCORRENCIAS

ZKA************ VERIFICA SE EXISTE PELO MENOS 1 CONTRATO PRESTAMISTA
              IF  RECRWIAA-SEG-PRESTAMISTA
                                       EQUAL 1
                  MOVE RECRWIAA-SEG-PRESTAMISTA
                                       TO  WRK-SEG-PRESTAMISTA
              END-IF

              PERFORM 2100-LER-EMODLAVS
           END-PERFORM.

ZKA   *----------------------------------------------------------------*
 |    *--> IDENTIFICACAO DE SEGURO PRESTAMISTA
 |
 |         MOVE  WRK-SEG-PRESTAMISTA   TO  IMPRIME-PRESTAMISTA
 |                                     OF  WRK-AREA-AVISO-RC004.
 |
ZKA   *----------------------------------------------------------------*

           MOVE  WRK-QTDE-OCORRENCIAS  TO  QTDE-OCORRENCIAS-4
                                       OF  WRK-AREA-AVISO-RC004.

           MOVE  WRK-VALOR-CONTR-ACUM  TO  VR-TOTAL-ACUMULADO
                                       OF  WRK-AREA-AVISO-RC004.

           MOVE  WRK-AREA-AVISO-RC004  TO  RECRWIJA-AREA-DADOS-DICD.

           PERFORM 3500-GRAVAR-SMODLAVS.

      *----------------------------------------------------------------*
       3400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA PARA GRAVAR O ARQUIVO SMODLAVS                          *
      *----------------------------------------------------------------*
       3500-GRAVAR-SMODLAVS            SECTION.
      *----------------------------------------------------------------*

           MOVE '3500-GRAVAR-SMODLAVS' TO  FRWKGHEA-IDEN-PARAGRAFO.
           MOVE '0030'                 TO  WRK-LOCAL.
           SET   ARQ-WRITE             TO  TRUE.

           WRITE FD-SMODLAVS         FROM  WRK-AREA-RECRWIJA.

           PERFORM 1120-TESTAR-FS-SMODLAVS.

           ADD   1                     TO  ACU-GRAVADOS-SMODLAVS.

      *----------------------------------------------------------------*
       3500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA DE FINALIZACAO DO PROGRAMA                              *
      *----------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           MOVE '4000-FINALIZAR'       TO  FRWKGHEA-IDEN-PARAGRAFO.
           MOVE '0040'                 TO  WRK-LOCAL.
           SET   ARQ-CLOSE             TO  TRUE.

           CLOSE EMODLAVS
                 SMODLAVS.

           PERFORM 1100-TESTAR-FILE-STATUS.

           PERFORM 4100-DISPLAYS-TOTAIS.

           PERFORM 7100-FINALIZAR-CKRS0105.

           IF  ACU-GRAVADOS-SMODLAVS  GREATER ZEROS
               DISPLAY '************* PROGRAMA CLLPXXX1 *************'
               DISPLAY '*                                           *'
               DISPLAY '*  PROGRAMA CLLPXXX1 ENCERRADO COM SUCESSO  *'
               DISPLAY '*                                           *'
               DISPLAY '************* PROGRAMA CLLPXXX1 *************'
           END-IF.

           PERFORM 4200-ENCERRAR-STOP-RUN.

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA PARA APRESENTAR AS ESTATISTICAS DO PROGRAMA             *
      *----------------------------------------------------------------*
       4100-DISPLAYS-TOTAIS            SECTION.
      *----------------------------------------------------------------*

           DISPLAY '************* PROGRAMA CLLPXXX1 *************'.
           DISPLAY '*                                           *'.
           MOVE  ACU-LIDOS-EMODLAVS    TO  WRK-MASCARA.
           DISPLAY '* REGISTROS LIDOS EMODLAVS...:  ' WRK-MASCARA ' *'.
           DISPLAY '*                                           *'.
           MOVE  ACU-GRAVADOS-SMODLAVS TO  WRK-MASCARA.
           DISPLAY '* REGISTROS GRAVADOS SMODLAVS:  ' WRK-MASCARA ' *'.
           DISPLAY '*                                           *'.
           DISPLAY '*********************************************'.

      *----------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA DE FINALIZACAO DO PROGRAMA                              *
      *----------------------------------------------------------------*
       4200-ENCERRAR-STOP-RUN          SECTION.
      *----------------------------------------------------------------*

           STOP RUN.

      *----------------------------------------------------------------*
       4200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA PARA CONEXAO CKRS0105                                   *
      *----------------------------------------------------------------*
       7000-INICIALIZAR-CKRS0105       SECTION.
      *----------------------------------------------------------------*

           INITIALIZE PARM-CKRS0105.

           MOVE 'C'                    TO  PARM-OP.
           MOVE 'DB2 '                 TO  PARM-SSID.
           MOVE 'CLLPXXX1'             TO  PARM-PLAN.

           CALL  WRK-CKRS0105       USING  PARM-CKRS0105.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               PERFORM 7200-ENCERRAR-GOBACK
           END-IF.

      *----------------------------------------------------------------*
       7000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA PARA DESCONEXAO CKRS0105                                *
      *----------------------------------------------------------------*
       7100-FINALIZAR-CKRS0105         SECTION.
      *----------------------------------------------------------------*

           INITIALIZE PARM-CKRS0105.

           MOVE 'D'                    TO  PARM-OP.
           MOVE 'DB2 '                 TO  PARM-SSID.
           MOVE 'CLLPXXX1'             TO  PARM-PLAN.

           CALL  WRK-CKRS0105       USING  PARM-CKRS0105.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               PERFORM 7200-ENCERRAR-GOBACK
           END-IF.

      *----------------------------------------------------------------*
       7100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ENCERRAR PROGRAMA COM ERRO DO CKRS0105                         *
      *----------------------------------------------------------------*
       7200-ENCERRAR-GOBACK            SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       7200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * TRATAMENTO DE ERRO DE ARQUIVO                                  *
      *----------------------------------------------------------------*
       9000-ERRO-ARQ                   SECTION.
      *----------------------------------------------------------------*

           SET   ERRO-ARQUIVO          TO  TRUE.

           MOVE  WRK-PROGRAMA          TO  FRWKGHEA-NOME-PROGRAMA.
           MOVE  FRWKGARQ-TAM-LAYOUT   TO  FRWKGHEA-TAM-DADOS.
           MOVE  WRK-AREA-ERRO-ARQ     TO  WRK-BLOCO-INFO-ERRO.

           DISPLAY ' '.
           DISPLAY 'FRWKGARQ-FILE-STATUS   = ' FRWKGARQ-FILE-STATUS.
           DISPLAY 'FRWKGARQ-NOME-ARQUIVO  = ' FRWKGARQ-NOME-ARQUIVO.
           DISPLAY 'FRWKGARQ-COMANDO       = ' FRWKGARQ-COMANDO.
           DISPLAY 'WRK-LOCAL              = ' WRK-LOCAL.

           PERFORM 9990-API-ERROS.

      *----------------------------------------------------------------*
       9000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * PROCESSAMENTO DE ERRO DE MODULO                                *
      *----------------------------------------------------------------*
       9100-ERRO-MODULO                SECTION.
      *----------------------------------------------------------------*

           SET   ERRO-MODULO           TO  TRUE

           MOVE  WRK-PROGRAMA          TO  FRWKGHEA-NOME-PROGRAMA
           MOVE  FRWKGMOD-TAM-LAYOUT   TO  FRWKGHEA-TAM-DADOS
           MOVE  WRK-AREA-ERRO-MOD     TO  WRK-BLOCO-INFO-ERRO

           DISPLAY '** DADOS DE ERRO DE MODULO **'
           DISPLAY 'FRWKGMOD-NOME-MODULO  = ' FRWKGMOD-NOME-MODULO
           DISPLAY 'FRWKGMOD-COD-RETORNO  = ' FRWKGMOD-COD-RETORNO
           DISPLAY 'FRWKGMOD-COD-ERRO     = ' FRWKGMOD-COD-ERRO
           DISPLAY 'FRWKGMOD-COD-MENSAGEM = ' FRWKGMOD-COD-MENSAGEM

           PERFORM 9990-API-ERROS.

      *----------------------------------------------------------------*
       9100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * PROCESSAMENTO DE ERRO DE DB2                                   *
      *----------------------------------------------------------------*
       9200-ERRO-DB2                   SECTION.
      *----------------------------------------------------------------*

           SET   ERRO-DB2              TO  TRUE.

           MOVE  FRWKGDB2-TAM-LAYOUT   TO  FRWKGHEA-TAM-DADOS.
           MOVE  WRK-AREA-ERRO-DB2     TO  WRK-BLOCO-INFO-ERRO
           MOVE  FRWKGDB2-SQLCODE      TO  WRK-MASCARA-SQLCODE.

           DISPLAY ' '
           DISPLAY 'FRWKGDB2-NOME-TABELA    = ' FRWKGDB2-NOME-TABELA.
           DISPLAY 'FRWKGDB2-LOCAL          = ' FRWKGDB2-LOCAL.
           DISPLAY 'FRWKGDB2-SQLCODE        = ' WRK-MASCARA-SQLCODE.
           DISPLAY 'FRWKGDB2-SQLSTATE       = ' FRWKGDB2-SQLSTATE.
           DISPLAY 'FRWKGDB2-SQLCA          = ' FRWKGDB2-SQLCA.

           PERFORM 9990-API-ERROS.

      *----------------------------------------------------------------*
       9200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA PARA TRATAMENTO DE ERRO                                 *
      *----------------------------------------------------------------*
       9990-API-ERROS                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 4100-DISPLAYS-TOTAIS.

           PERFORM 9991-CHAMAR-FRWK2999.

           IF  WRK-ABENDAR
               PERFORM 9992-CHAMAR-BRAD0450
           END-IF.

           PERFORM 4200-ENCERRAR-STOP-RUN.

      *----------------------------------------------------------------*
       9990-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA PARA ACESSO AO FRWK                                     *
      *----------------------------------------------------------------*
       9991-CHAMAR-FRWK2999            SECTION.
      *----------------------------------------------------------------*

           DISPLAY '*** FRWK2999 CHAMADO. SEGUEM SEUS PARAMETROS ***'.
           DISPLAY ' '
           DISPLAY 'FRWKGHEA-TIPO-ERRO      = ' FRWKGHEA-TIPO-ERRO.
           DISPLAY 'FRWKGHEA-NOME-PROGRAMA  = ' FRWKGHEA-NOME-PROGRAMA.
           DISPLAY 'FRWKGHEA-IDEN-PARAGRAFO = ' FRWKGHEA-IDEN-PARAGRAFO.
           DISPLAY 'FRWKGHEA-TAM-DADOS      = ' FRWKGHEA-TAM-DADOS.

           CALL  WRK-FRWK2999       USING  WRK-AREA-ERRO.

           DISPLAY '*** PARAMETROS RETORNADOS PELO FRWK2999 ***'.
           DISPLAY 'FRWKGERR-COD-RETORNO    = ' FRWKGERR-COD-RETORNO.
           DISPLAY 'FRWKGERR-COD-ERRO       = ' FRWKGERR-COD-ERRO.
           DISPLAY 'FRWKGERR-COD-MENSAGEM   = ' FRWKGERR-COD-MENSAGEM.

      *----------------------------------------------------------------*
       9991-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA PARA ABENDAR O SISTEMA                                  *
      *----------------------------------------------------------------*
       9992-CHAMAR-BRAD0450            SECTION.
      *----------------------------------------------------------------*

           DISPLAY '*** BRAD0450 CHAMADO PARA ABENDAR O PROGRAMA ***'.
           DISPLAY ' '.

           CALL 'BRAD0450'          USING  WRK-ABEND-0450
                                           WRK-DUMP-0450.

      *----------------------------------------------------------------*
       9992-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

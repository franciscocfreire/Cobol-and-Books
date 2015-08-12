      *===============================================================*
       IDENTIFICATION DIVISION.
      *===============================================================*

       PROGRAM-ID. CLLP7202.
      *===============================================================*
      *                   C P M   S I S T E M A S                     *
      *---------------------------------------------------------------*
      *                                                               *
      *      PROGRAMADORA.: GISELE PAUL        - CPM/FPOLIS           *
      *      ANALISTA CPM.: RESENDE            - CPM/FPOLIS           *
      *      ANALISTA.....: TOMOKO             - GP. 82               *
      *      DATA.........: 13/11/2000                                *
      *                                                               *
      *      OBJETIVO.....:                                           *
      *        LER ARQUIVO ARQDB2 SUMARIZADO SOMENTE COM OS DADOS     *
      *        UTILIZADOS NOS PROGRAMAS SEGUINTES.                    *
      *        INCLUIDO AS CONSISTENCIAS DO PROGRAMA CLLP7640(VELHO)  *
      *                                                               *
STF   *===============================================================*
STF   *                        A L T E R A C A O                      *
STF   *---------------------------------------------------------------*
STF   *                                                               *
STF   *      PROGRAMADOR  : JOCILEIDE AP. MARIANO  - STEFANINI        *
STF   *      SUPERVISOR   : RICARDO BA             - STEFANINI        *
STF   *      ANALISTA     : ENEAS CARVALHO         - F.(11)4196-0994  *
STF   *      DATA         : 04/09/2002                                *
STF   *                                                               *
STF   *      OBJETIVO     : ENVIAR O SEGUNDO AVALISTA PARA SPC/SERASA *
STF   *                                                               *
STF   *      OBS.: ESTE PROGRAMA EH COPIA DO PROGRAMA CLLP7516        *
STF   *                                                               *
STF   *===============================================================*
CPMMRS*                        A L T E R A C A O                      *
CPMMRS*---------------------------------------------------------------*
CPMMRS*                                                               *
CPMMRS*      PROGRAMADOR  : MAURICIO ROQUE DA SILVA - CPM             *
CPMMRS*      ANALISTA     : MAURICIO ROQUE DA SILVA - 4196-0570       *
CPMMRS*      DATA         : 30/05/2007                                *
CPMMRS*                                                               *
CPMMRS*      OBJETIVO     : INCLUSAO DO PARM VERIFICAR ENDERECO NA    *
CPMMRS*                     LOCALIZADORA OU ENVIAR CARTA DE AVISO     *
CPMMRS*                     0 - ENVIAR CARTA DE AVISO                 *
CPMMRS*                     0 - ENVIAR PARA LOCALIZADORA              *
CPMMRS*                                                               *
CPMMRS*===============================================================*
BRQ141******************************************************************
BRQ141* MAIO/2012 - (BRQ) - TRATAMENTO DE CARTEIRA  ALFANUMERICA
BRQ141******************************************************************
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

           SELECT  ARQDB2  ASSIGN TO UT-S-ARQDB2
                      FILE STATUS IS WRK-FS-ARQDB2.

           SELECT   TOTAIS ASSIGN TO UT-S-TOTAIS
                      FILE STATUS IS WRK-FS-TOTAIS.

           SELECT  ARQDB2S ASSIGN TO UT-S-ARQDB2S
                      FILE STATUS IS WRK-FS-ARQDB2S.

           SELECT PARMCLLP ASSIGN TO UT-S-PARMCLLP
                      FILE STATUS IS WRK-FS-PARMCLLP.

           SELECT ARQDATA  ASSIGN TO UT-S-ARQDATA
                      FILE STATUS IS WRK-FS-ARQDATA.
           EJECT
      *===============================================================*
       DATA DIVISION.
      *===============================================================*

      *---------------------------------------------------------------*
       FILE SECTION.
      *---------------------------------------------------------------*


      *---------------------------------------------------------------*
      *    INPUT:     ARQQUIVO DE TABELA DB2                          *
      *               ORG. SEQUENCIAL   -   LRECL = 313               *
      *---------------------------------------------------------------*

       FD  ARQDB2
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

      ******************************************************************
      *        ARQDB2 SUMARIZADO                          LRECL = 313  *
      *               CAMPO  DB2-VCTO = AAAAMMDD                       *
      ******************************************************************
       01  DB2-REGTO.
           03  DB2-CHAVE.
               05  DB2-CPF-DEV.
                07 DB2-NUMERO-DEV          PIC 9(09)    COMP-3.
                07 DB2-FILIAL              PIC 9(05)    COMP-3.
                07 DB2-CTR-DEV             PIC 9(02).
               05  DB2-COD-NATUREZA-OPER   PIC X(03).
               05  DB2-CONTRATO            PIC 9(07)    COMP-3.
           03  DB2-VCTO                    PIC 9(08).
           03  DB2-EMPRESA                 PIC 9(05)    COMP-3.
           03  DB2-AGENCIA                 PIC 9(05)    COMP-3.
           03  DB2-CONTA                   PIC 9(07)    COMP-3.
           03  DB2-CARTEIRA                PIC X(03).
           03  DB2-ID                      PIC X(02).
           03  DB2-MOEDA                   PIC X(02).
           03  DB2-TIPO-PEND               PIC 9(05)    COMP-3.
           03  DB2-CODIGO                  PIC 9(03)    COMP-3.
           03  DB2-PRINCIPAL               PIC 9(13)V99 COMP-3.
           03  DB2-VR-VENCIDOS             PIC 9(13)V99 COMP-3.
           03  DB2-VR-VINCENDOS            PIC 9(13)V99 COMP-3.
           03  DB2-NOME-DEVEDOR.
               05  DB2-NOME-20             PIC X(20).
               05  FILLER                  PIC X(20).
           03  DB2-NOME-AVALISTA           PIC X(40).
           03  DB2-CPF-AVAL.
               05  DB2-NUMERO-AVAL         PIC 9(09)    COMP-3.
               05  DB2-FILIAL-AVAL         PIC 9(05)    COMP-3.
               05  DB2-CTR-AVAL            PIC 9(02).
           03  DB2-NOME-AVAL2              PIC X(40).
           03  DB2-CPF-AVAL2.
               05  DB2-NUMERO-AVAL2        PIC 9(09)    COMP-3.
               05  DB2-FILIAL-AVAL2        PIC 9(05)    COMP-3.
               05  DB2-CTR-AVAL2           PIC 9(02).
           03  DB2-COD-EMPR                PIC 9(02).
           03  DB2-LT                      PIC X(100).

           EJECT
      *---------------------------------------------------------------*
      *    OUTPUT:    DEVEDORES SELECIONADOS P/ AVISO COBRANCA        *
      *               ORG. SEQUENCIAL   -   LRECL = 313               *
      *---------------------------------------------------------------*

       FD  ARQDB2S
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  REG-ARQDB2S                  PIC X(313).

      *---------------------------------------------------------------*
      *    OUTPUT:    RELATORIO DE TOTAIS                             *
      *               ORG. SEQUENCIAL   -   LRECL = 133               *
      *---------------------------------------------------------------*

       FD  TOTAIS
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  REG-TOTAIS                  PIC X(133).


      *---------------------------------------------------------------*
      *    INPUT:     ORG. SEQUENCIAL   -   LRECL = 250               *
      *---------------------------------------------------------------*

       FD  PARMCLLP
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  REG-PARAMETRO.
           05  PARM-TAREFA.
               10  PARM-CCUSTO-1       PIC X(04).
               10  PARM-CODIGO-1       PIC 9(04).
               10  PARM-CCUSTO-2       PIC X(04).
               10  PARM-CODIGO-2       PIC 9(04).
           05  PARM-SEQUENCIA          PIC 9(07).
           05  PARM-NR-DIAS            PIC 9(03).
           05  PARM-VL-CORTE           PIC 9(15).
           05  FILLER                  PIC X(209).

      *---------------------------------------------------------------*
      *    INPUT:     ORG. SEQUENCIAL   -                             *
      *---------------------------------------------------------------*

       FD  ARQDATA
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

-INC I#CLLPGA

           EJECT
      *---------------------------------------------------------------*
       WORKING-STORAGE SECTION.
      *---------------------------------------------------------------*

       77  FILLER                      PIC X(32)        VALUE
           '* INICIO DA WORKING CLLP7202 *'.

       77  WRK-ABEND                   PIC S9(04) COMP  VALUE +1111.
       77  WRK-FIM-ARQDB2              PIC  X(01)        VALUE  'N'.
       77  WRK-FINALIZA                PIC  X(01)        VALUE  'N'.
       77  WRK-RETURN-CODE             PIC S9(05)        VALUE ZEROS.
       77  WRK-DESVIO                  PIC X(01)         VALUE 'N'.

       77  WRK-FS-ARQDB2               PIC  X(02)        VALUE  SPACES.
       77  WRK-FS-TOTAIS               PIC  X(02)        VALUE  SPACES.
       77  WRK-FS-ARQDB2S              PIC  X(02)        VALUE  SPACES.
       77  WRK-FS-ARQDATA              PIC  X(02)        VALUE  SPACES.
       77  WRK-FS-PARMCLLP             PIC  X(02)        VALUE  SPACES.

       77  WRK-OPERACAO                PIC  X(13) VALUE SPACES.
       77  WRK-ABERTURA                PIC  X(13) VALUE 'NA ABERTURA'.
       77  WRK-LEITURA                 PIC  X(13) VALUE 'NA LEITURA'.
       77  WRK-GRAVACAO                PIC  X(13) VALUE 'NA GRAVACAO'.
       77  WRK-FECHAMENTO              PIC  X(13) VALUE 'NO FECHAMENTO'.

       77  ACU-LIDOS-ARQDB2            PIC  9(08) COMP-3 VALUE ZEROS.
       77  ACU-GRAVA-ARQDB2S           PIC  9(08) COMP-3 VALUE ZEROS.
       77  ACU-DESPREZA-AVAL           PIC  9(08) COMP-3 VALUE ZEROS.
       77  ACU-DESPREZA-DATA           PIC  9(08) COMP-3 VALUE ZEROS.
       77  ACU-VAL-RESG                PIC  9(11)V99     VALUE ZEROS.
       77  ACU-ENC-VENC                PIC  9(11)V99     VALUE ZEROS.
       77  ACU-ENC-VINC                PIC  9(11)V99     VALUE ZEROS.

       01  WRK-DIAS5                   PIC S9(03)         VALUE  ZEROS.

       01  WRK-VL-CORTE5               PIC S9(15)         VALUE  ZEROS.

       01  WRK-DIAS-EDIT               PIC ZZZZZ9.
       01  WRK-DATA-EDIT               PIC 9999.99.99.

      *---------------------------------------------------------------*
      *             DEFINICAO DAS DATAS DE VENCIMENTO                 *
      *---------------------------------------------------------------*


       01  WRK-DT-AVALISTA             PIC  9(08).
       01  FILLER  REDEFINES  WRK-DT-AVALISTA.
           03  WRK-DT-MOV-AAAA         PIC  9(04).
           03  WRK-DT-MOV-MM           PIC  9(02).
           03  WRK-DT-MOV-DD           PIC  9(02).

       01  WRK-DT-MENOS01D             PIC  9(08).
       01  FILLER  REDEFINES  WRK-DT-MENOS01D.
           03  WRK-DT01-MOV-AAAA       PIC  9(04).
           03  WRK-DT01-MOV-MM         PIC  9(02).
           03  WRK-DT01-MOV-DD         PIC  9(02).

       01  WRK-TI-SYS                  PIC  9(06).
       01  FILLER  REDEFINES  WRK-TI-SYS.
           03  WRK-TI-HOR              PIC  9(02).
           03  WRK-TI-MIN              PIC  9(02).
           03  WRK-TI-SEG              PIC  9(02).

       01  WRK-AREA-POOL7600.
           03  WRK-DT-JULIANA          PIC  9(05) COMP-3.
           03  WRK-DT-AAMMDD           PIC  9(07) COMP-3.
           03  WRK-DT-AAAAMMDD         PIC  9(09) COMP-3.
           03  WRK-TI-HHMMSS           PIC  9(07) COMP-3.
           03  WRK-TI-HHMMSSMMMMMM     PIC  9(13) COMP-3.
           03  WRK-TIMESTAMP           PIC  X(20).

       01  WRK-AREA-POOL1285.
           03  WRK-DT-ENTRADA          PIC  9(08) COMP-3 VALUE ZEROS.
           03  WRK-NUM-DIAS            PIC S9(05) COMP-3 VALUE ZEROS.
           03  WRK-DT-SAIDA            PIC  9(08) COMP-3 VALUE ZEROS.
           03  WRK-MENSAGEM            PIC  X(50)        VALUE SPACES.

       01  WRK-AREA-POOL0025.
           03  WRK-DT-ABERT            PIC  9(09) COMP-3 VALUE ZEROS.
           03  WRK-PRAZO               PIC S9(05) COMP-3 VALUE ZEROS.
           03  WRK-DT-LIMI5ANOS        PIC  9(09) COMP-3 VALUE ZEROS.

       01  WRK1-AREA-POOL0025.
           03  WRK1-DT-ABERT            PIC  9(09) COMP-3 VALUE ZEROS.
           03  WRK1-PRAZO               PIC S9(05) COMP-3 VALUE ZEROS.
           03  WRK1-DT-SAIDA            PIC  9(09) COMP-3 VALUE ZEROS.

       01  WRK-AREA-POOL0110.
           03  WRK-POOL0110.
               05  WRK-NUMERO-0110     PIC  9(09)        VALUE ZEROS.
               05  WRK-FILIAL-0110     PIC  9(05)        VALUE ZEROS.
           03  WRK-CONTROLE-0110       PIC  X(02)        VALUE SPACES.

       01  AUX-DT-INV-AAAAMMDD         PIC  9(08).
       01  FILLER  REDEFINES  AUX-DT-INV-AAAAMMDD.
           03  AUX-DT-INV-AAAA         PIC  9(04).
           03  AUX-DT-INV-MM           PIC  9(02).
           03  AUX-DT-INV-DD           PIC  9(02).

       01  AUX-DT-DB2.
           03  AUX-DT-DB2-DD           PIC  9(02).
           03  FILLER                  PIC  X(01).
           03  AUX-DT-DB2-MM           PIC  9(02).
           03  FILLER                  PIC  X(01).
           03  AUX-DT-DB2-AAAA         PIC  9(04).
           03  FILLER REDEFINES AUX-DT-DB2-AAAA.
               05  AUX-DT-DB2-SEC      PIC  9(02).
               05  AUX-DT-DB2-ANO      PIC  9(02).

       01  WRK-CH-ARQDB2-ATU.
           03  WRK-CGCCPF-NUM-ATU      PIC 9(09) VALUE ZEROS.
           03  WRK-CGCCPF-FIL-ATU      PIC 9(04) VALUE ZEROS.
           03  WRK-CGCCPF-CTR-ATU      PIC X(02) VALUE SPACES.
           03  WRK-AGENCIA-ATU         PIC 9(05) VALUE ZEROS.
           03  WRK-CONTA-ATU           PIC 9(07) VALUE ZEROS.

       01  WRK-CONTRATO-ATU            PIC 9(07) VALUE ZEROS.

       01  WRK-CH-ARQDB2-ANT.
           03  WRK-CGCCPF-NUM-ANT      PIC 9(09) VALUE ZEROS.
           03  WRK-CGCCPF-FIL-ANT      PIC 9(04) VALUE ZEROS.
           03  WRK-CGCCPF-CTR-ANT      PIC X(02) VALUE SPACES.
           03  WRK-AGENCIA-ANT         PIC 9(05) VALUE ZEROS.
           03  WRK-CONTA-ANT           PIC 9(07) VALUE ZEROS.

       01  WRK-CONTRATO-ANT        PIC 9(07) VALUE ZEROS.

      *---------------------------------------------------------------*
      *                 DEFINICAO DO RELATORIO DE TOTAIS              *
      *---------------------------------------------------------------*

       01  CABEC1.
           03  CB1-CARRO               PIC X(01) VALUE '1'.
           03  FILLER                  PIC X(28) VALUE 'CLLP7202'.
           03  FILLER                  PIC X(37) VALUE
               'BANCO   BRADESCO  S.A.'.
           03  FILLER                  PIC X(7)   VALUE 'FOLHA:'.
           03  CB1-PAG                 PIC ZZZ.ZZ9.

       01  CABEC2.
           03  CB2-CARRO               PIC X(01) VALUE '0'.
           03  FILLER.
               05  CB2-DTSYS-DD        PIC 9(2)/.
               05  CB2-DTSYS-MM        PIC 9(2)/.
               05  CB2-DTSYS-AAAA      PIC 9(4).
           03  FILLER                  PIC X(07) VALUE SPACES.
           03  FILLER                  PIC X(37) VALUE
               'RELATORIO DE TOTAIS - DATA MOVIMENTO'.
           03  FILLER.
               05  CB2-DTMV-DD         PIC 9(2)/.
               05  CB2-DTMV-MM         PIC 9(2)/.
               05  CB2-DTMV-AAAA       PIC 9(4).
           03  FILLER                  PIC X(07) VALUE SPACES.
           03  FILLER.
               05  CB2-TIME-HOR        PIC 9(2).
               05  FILLER              PIC X(1) VALUE ':'.
               05  CB2-TIME-MIN        PIC 9(2).
               05  FILLER              PIC X(1) VALUE ':'.
               05  CB2-TIME-SEG        PIC 9(2).

       01  CABEC3.
           03  CB2-CARRO               PIC X(01) VALUE '0'.
           03  FILLER                  PIC X(17) VALUE SPACES.
           03  FILLER                  PIC X(37) VALUE
               'DATA PARAMETRO P/ NATUREZA = CARTAO '.
           03  FILLER.
               05  CB2-DTMV35-DD         PIC 9(2)/.
               05  CB2-DTMV35-MM         PIC 9(2)/.
               05  CB2-DTMV35-AAAA       PIC 9(4).
           03  FILLER                  PIC X(15) VALUE SPACES.


       01  LINSPACE.
           03  LP1-CARRO               PIC X(01) VALUE '-'.
           03  FILLER                  PIC X(79) VALUE SPACES.


       01  LINDET1.
           03  LD1-CARRO               PIC X(01) VALUE ' '.
           03  FILLER                  PIC X(02) VALUE SPACES.
           03  LD1-CGC-DEV             PIC 999.999.999.
           03  FILLER                  PIC X(01) VALUE '/'.
           03  LD1-FIL-DEV             PIC 9999.
           03  FILLER                  PIC X(01) VALUE '-'.
           03  LD1-CTR-DEV             PIC 99.
           03  FILLER                  PIC X(02) VALUE SPACES.
           03  LD1-CGC-AVA1            PIC 999.999.999.
           03  FILLER                  PIC X(01) VALUE '/'.
           03  LD1-FIL-AVA1            PIC 9999.
           03  FILLER                  PIC X(01) VALUE '-'.
           03  LD1-CTR-AVA1            PIC 99.
           03  FILLER                  PIC X(02) VALUE SPACES.
           03  LD1-CGC-AVA2            PIC 999.999.999.
           03  FILLER                  PIC X(01) VALUE '/'.
           03  LD1-FIL-AVA2            PIC 9999.
           03  FILLER                  PIC X(01) VALUE '-'.
           03  LD1-CTR-AVA2            PIC 99.
           03  FILLER                  PIC X(02) VALUE SPACES.
           03  LD1-AGENCIA             PIC ZZZZ9.
           03  FILLER                  PIC X(02) VALUE SPACES.
           03  LD1-CONTA               PIC ZZZZZZ9.
           03  FILLER                  PIC X(02) VALUE SPACES.
           03  LD1-CONTRATO            PIC ZZZZZZ9.
           03  FILLER                  PIC X(02) VALUE SPACES.
           03  LD1-VCTO                PIC 9999.99.99.

       01  LINTOT1.
           03  LT1-CARRO               PIC X(01) VALUE '0'.
           03  FILLER                  PIC X(17) VALUE SPACES.
           03  FILLER                  PIC X(32) VALUE
               'REGISTROS LIDOS - ARQDB2      = '.
           03  LT1-LIDOS-ARQDB2        PIC ZZ.ZZZ.ZZ9.


       01  LINTOT2.
           03  LT2-CARRO               PIC X(01) VALUE '0'.
           03  FILLER                  PIC X(17) VALUE SPACES.
           03  FILLER                  PIC X(32) VALUE
               'REGISTROS GRAVADOS - ARQDB2S  = '.
           03  LT2-GRAVA-ARQDB2S       PIC ZZ.ZZZ.ZZ9.

       01  LINTOT3.
           03  LT3-CARRO               PIC X(01) VALUE '0'.
           03  FILLER                  PIC X(17) VALUE SPACES.
           03  FILLER                  PIC X(32) VALUE
               'REGISTROS GRAVADOS - ARQINCON = '.
           03  LT3-GRAVA-ARQINCON      PIC ZZ.ZZZ.ZZ9.

       01  LINTOT4.
           03  LT4-CARRO               PIC X(01) VALUE '0'.
           03  FILLER                  PIC X(17) VALUE SPACES.
           03  FILLER                  PIC X(32) VALUE
               'REGISTROS DESPREZADO - AVAL   = '.
           03  LT4-DESPR-AVALISTA      PIC ZZ.ZZZ.ZZ9.

       01  LINTOT5.
           03  LT5-CARRO               PIC X(01) VALUE '0'.
           03  FILLER                  PIC X(17) VALUE SPACES.
           03  FILLER                  PIC X(32) VALUE
               'REGISTROS DESPREZADO - AVAL   = '.
           03  LT5-DESPR-DATA          PIC ZZ.ZZZ.ZZ9.

BRQ=E******-INC I#CLLPCJ
-INC I#CLLP26

       01  FILLER                      PIC X(32)        VALUE
           '*  FIM DA WORKING CLLP7202 *'.
           EJECT

CPMMRS*---------------------------------------------------------------*
CPMMRS LINKAGE                        SECTION.
CPMMRS*---------------------------------------------------------------*
CPMMRS 01  LKG-PARM.
CPMMRS     05   LKG-TAMANHO      PIC S9(04) COMP.
CPMMRS     05   LKG-QTD-DIA      PIC  9(02).
CPMMRS
CPMMRS*===============================================================*
CPMMRS PROCEDURE      DIVISION      USING       LKG-PARM.
CPMMRS*---------------------------------------------------------------*

      *---------------------------------------------------------------*
       0000-INICIAR SECTION.
      *---------------------------------------------------------------*

           OPEN INPUT   ARQDB2
                        PARMCLLP
                        ARQDATA
                OUTPUT  TOTAIS
                        ARQDB2S.

           MOVE    WRK-ABERTURA            TO   WRK-OPERACAO.
           PERFORM 1000-TESTAR-FILE-STATUS.

CPMMRS     IF   LKG-QTD-DIA   EQUAL SPACES OR LOW-VALUES
CPMMRS          DISPLAY '************* CLLP7202 **************'
CPMMRS          DISPLAY '*                                   *'
CPMMRS          DISPLAY '*   PARM NAO FOI INFORMADO          *'
CPMMRS          DISPLAY '*                                   *'
CPMMRS          DISPLAY '************* CLLP7202 **************'
CPMMRS          MOVE    4 TO RETURN-CODE
CPMMRS          GO  TO  0000-FINALIZAR.
CPMMRS
CPMMRS     IF   LKG-QTD-DIA    NOT EQUAL 02 AND 00
CPMMRS          DISPLAY '************* CLLP7202 **************'
CPMMRS          DISPLAY '*                                   *'
CPMMRS          DISPLAY '*   PARM INFORMADO ESTA INVALIDO    *'
CPMMRS          DISPLAY '*                                   *'
CPMMRS          DISPLAY '************* CLLP7202 **************'
CPMMRS          MOVE    4 TO RETURN-CODE
CPMMRS          GO  TO  0000-FINALIZAR.

           PERFORM 2000-INICIALIZA.

           IF  WRK-FINALIZA EQUAL 'S'
               GO  TO  0000-FINALIZAR.

           PERFORM 3000-PROCESSA-ARQDB2 UNTIL
                                        WRK-FIM-ARQDB2 EQUAL 'S'.

           PERFORM  4000-IMPRIME-TOTAIS.

       0000-FINALIZAR.
      *---------------*

           CLOSE  ARQDB2
                  TOTAIS
                  ARQDB2S
                  ARQDATA
                  PARMCLLP.

           MOVE    WRK-FECHAMENTO          TO   WRK-OPERACAO.
           PERFORM 1000-TESTAR-FILE-STATUS.

           GOBACK.

      *---------------------------------------------------------------*
       0000-99-FIM. EXIT.
      *---------------------------------------------------------------*
           EJECT
      *---------------------------------------------------------------*
       1000-TESTAR-FILE-STATUS SECTION.
      *---------------------------------------------------------------*

           PERFORM 1100-TESTAR-FS-ARQDB2.

           PERFORM 1200-TESTAR-FS-TOTAIS.

           PERFORM 1300-TESTAR-FS-ARQDB2S.

           PERFORM 1400-TESTAR-FS-ARQDATA.

           PERFORM 1500-TESTAR-FS-PARMCLLP.

      *---------------------------------------------------------------*
       1000-99-FIM. EXIT.
      *---------------------------------------------------------------*
           EJECT
      *---------------------------------------------------------------*
       1100-TESTAR-FS-ARQDB2  SECTION.
      *---------------------------------------------------------------*

           IF WRK-FS-ARQDB2 NOT EQUAL '00'
              DISPLAY '************** CLLP7202 *************'
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
              DISPLAY '*              ARQDB2               *'
              DISPLAY '*         FILE STATUS =  ' WRK-FS-ARQDB2
                                                 '         *'
              DISPLAY '************** CLLP7202 *************'
              CALL 'ILBOABN0'     USING WRK-ABEND.

      *---------------------------------------------------------------*
       1100-99-FIM. EXIT.
      *---------------------------------------------------------------*
           EJECT
      *---------------------------------------------------------------*
       1200-TESTAR-FS-TOTAIS   SECTION.
      *---------------------------------------------------------------*

           IF WRK-FS-TOTAIS NOT EQUAL '00'
              DISPLAY '************** CLLP7202 *************'
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
              DISPLAY '*              TOTAIS               *'
              DISPLAY '*         FILE STATUS =  ' WRK-FS-TOTAIS
                                                 '         *'
              DISPLAY '************** CLLP7202 *************'
              CALL 'ILBOABN0'     USING WRK-ABEND.

      *---------------------------------------------------------------*
       1200-99-FIM. EXIT.
      *---------------------------------------------------------------*
           EJECT
      *---------------------------------------------------------------*
       1300-TESTAR-FS-ARQDB2S  SECTION.
      *---------------------------------------------------------------*

           IF WRK-FS-ARQDB2S NOT EQUAL '00'
              DISPLAY '************** CLLP7202 *************'
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
              DISPLAY '*              ARQDB2S              *'
              DISPLAY '*         FILE STATUS =  ' WRK-FS-ARQDB2S
                                                 '         *'
              DISPLAY '************** CLLP7202 *************'
              CALL 'ILBOABN0'     USING WRK-ABEND.

      *---------------------------------------------------------------*
       1300-99-FIM. EXIT.
      *---------------------------------------------------------------*
           EJECT
      *---------------------------------------------------------------*
       1400-TESTAR-FS-ARQDATA  SECTION.
      *---------------------------------------------------------------*

           IF WRK-FS-ARQDATA NOT EQUAL '00'
              DISPLAY '************** CLLP7202 *************'
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
              DISPLAY '*              ARQDATA              *'
              DISPLAY '*         FILE STATUS =  ' WRK-FS-ARQDATA
                                                 '         *'
              DISPLAY '************** CLLP7202 *************'
              CALL 'ILBOABN0'     USING WRK-ABEND.

      *---------------------------------------------------------------*
       1400-99-FIM. EXIT.
      *---------------------------------------------------------------*
           EJECT
      *---------------------------------------------------------------*
       1500-TESTAR-FS-PARMCLLP SECTION.
      *---------------------------------------------------------------*

           IF WRK-FS-PARMCLLP NOT EQUAL '00'
              DISPLAY '************** CLLP7202 *************'
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
              DISPLAY '*              PARMCLLP             *'
              DISPLAY '*         FILE STATUS =  ' WRK-FS-PARMCLLP
                                                 '         *'
              DISPLAY '************** CLLP7202 *************'
              CALL 'ILBOABN0'     USING WRK-ABEND.

      *---------------------------------------------------------------*
       1500-99-FIM. EXIT.
      *---------------------------------------------------------------*
           EJECT
      *---------------------------------------------------------------*
       2000-INICIALIZA SECTION.
      *---------------------------------------------------------------*

           PERFORM 2200-LER-PARMCLLP.

           IF  WRK-FS-PARMCLLP   EQUAL   '10'
               DISPLAY '************** CLLP7202 *************'
               DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
               DISPLAY '*              PARMCLLP             *'
               DISPLAY '*         FILE STATUS =  ' WRK-FS-PARMCLLP
                                                  '         *'
               DISPLAY '************** CLLP7202 *************'
               CALL 'ILBOABN0'     USING WRK-ABEND.

           PERFORM 2300-LER-ARQDATA.

           IF  WRK-FS-ARQDATA    EQUAL   '10'
               DISPLAY '************** CLLP7202 *************'
               DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
               DISPLAY '*              ARQDATA              *'
               DISPLAY '*         FILE STATUS =  ' WRK-FS-ARQDATA
                                                  '         *'
               DISPLAY '************** CLLP7202 *************'
               CALL 'ILBOABN0'     USING WRK-ABEND.

CPMMRS     COMPUTE  WRK-DIAS5 =  PARM-NR-DIAS - LKG-QTD-DIA

           CALL   'POOL7600' USING WRK-AREA-POOL7600.

           MOVE   WRK-DT-AAAAMMDD       TO  AUX-DT-INV-AAAAMMDD.
           MOVE   AUX-DT-INV-DD         TO  CB2-DTSYS-DD.
           MOVE   AUX-DT-INV-MM         TO  CB2-DTSYS-MM.
           MOVE   AUX-DT-INV-AAAA       TO  CB2-DTSYS-AAAA.

           MOVE   WRK-TI-HHMMSS         TO  WRK-TI-SYS.
           MOVE   WRK-TI-HOR            TO  CB2-TIME-HOR.
           MOVE   WRK-TI-MIN            TO  CB2-TIME-MIN.
           MOVE   WRK-TI-SEG            TO  CB2-TIME-SEG.

           MOVE   WRK-DT-AAAAMMDD       TO  WRK-DT-ABERT.
           MOVE   -1825                 TO  WRK-PRAZO.

           CALL 'POOL0025'      USING  WRK-DT-ABERT
                                       WRK-PRAZO
                                       WRK-DT-LIMI5ANOS

           IF RETURN-CODE NOT EQUAL ZEROS
              MOVE    RETURN-CODE   TO  WRK-RETURN-CODE
              DISPLAY '******************* CLLP7202 *******************'
              DISPLAY '*   ERRO  UTILIZANDO  A  POOL0025              *'
              DISPLAY '*                                              *'
              DISPLAY '*   DATA-ENTRADA (AAAAMMDD)    = '
                            AUX-DT-INV-AAAAMMDD                '      *'
              DISPLAY '*   RETURN-CODE                = '
                           WRK-RETURN-CODE                   '        *'
              DISPLAY '*                                              *'
              DISPLAY '******************* CLLP7202 *******************'
              CALL 'ILBOABN0'           USING WRK-ABEND.

           MOVE    DTMOVINV          TO     WRK-DT-AVALISTA.
           MOVE   WRK-DT-MOV-DD      TO  CB2-DTMV-DD
           MOVE   WRK-DT-MOV-MM      TO  CB2-DTMV-MM
           MOVE   WRK-DT-MOV-AAAA    TO  CB2-DTMV-AAAA.

           MOVE    DTMOVINV          TO     WRK-DT-ENTRADA
           COMPUTE WRK-NUM-DIAS      =    ( WRK-DIAS5 - 1)
           COMPUTE WRK-NUM-DIAS      =    ( WRK-NUM-DIAS * -1)

           MOVE WRK-DT-ENTRADA       TO  WRK-DATA-EDIT.
           MOVE WRK-NUM-DIAS         TO  WRK-DIAS-EDIT.

      *    DISPLAY 'DATA ENTRADA = ' WRK-DATA-EDIT.
      *    DISPLAY 'DIAS ENTRADA = ' WRK-DIAS-EDIT.

           CALL   'POOL1285' USING WRK-AREA-POOL1285.

           IF RETURN-CODE NOT EQUAL ZEROS
              MOVE    RETURN-CODE   TO  WRK-RETURN-CODE
              DISPLAY '******************* CLLP7202 *******************'
              DISPLAY '*   ERRO  UTILIZANDO  A  POOL1285    '
              DISPLAY '*                                    '
              DISPLAY '*   DATA-ENTRADA     = ' WRK-DT-ENTRADA
              DISPLAY '*   RETURN-CODE      = ' WRK-RETURN-CODE
              DISPLAY '*   MENSAGEM DO ERRO = ' WRK-MENSAGEM
              DISPLAY '*                                    '
              DISPLAY '******************* CLLP7202 *******************'
              MOVE  'S'  TO  WRK-FINALIZA
           ELSE
              MOVE   WRK-DT-SAIDA       TO  WRK-DT-AVALISTA.


           MOVE WRK-DT-SAIDA         TO  WRK-DATA-EDIT.

      *    DISPLAY 'DATA SAIDA   = ' WRK-DATA-EDIT.

           MOVE   WRK-DIAS5          TO     WRK-NUM-DIAS
           COMPUTE WRK-NUM-DIAS      =    ( WRK-NUM-DIAS * -1)

           MOVE WRK-DT-ENTRADA       TO  WRK-DATA-EDIT.
           MOVE WRK-NUM-DIAS         TO  WRK-DIAS-EDIT.

      *    DISPLAY 'DATA ENTRADA = ' WRK-DATA-EDIT.
      *    DISPLAY 'DIAS ENTRADA = ' WRK-DIAS-EDIT.

           CALL   'POOL1285' USING WRK-AREA-POOL1285.

           IF RETURN-CODE NOT EQUAL ZEROS
              MOVE    RETURN-CODE   TO  WRK-RETURN-CODE
              DISPLAY '******************* CLLP7202 *******************'
              DISPLAY '*   ERRO  UTILIZANDO  A  POOL1285    '
              DISPLAY '*                                    '
              DISPLAY '*   DATA-ENTRADA     = ' WRK-DT-ENTRADA
              DISPLAY '*   RETURN-CODE      = ' WRK-RETURN-CODE
              DISPLAY '*   MENSAGEM DO ERRO = ' WRK-MENSAGEM
              DISPLAY '*                                    '
              DISPLAY '******************* CLLP7202 *******************'
              MOVE  'S'  TO  WRK-FINALIZA
           ELSE
              MOVE   WRK-DT-SAIDA       TO  WRK-DT-MENOS01D.


           MOVE WRK-DT-SAIDA         TO  WRK-DATA-EDIT.

      *    DISPLAY 'DATA MENOS   = ' WRK-DATA-EDIT.

           PERFORM 2100-LER-ARQDB2.

      *---------------------------------------------------------------*
       2000-99-FIM. EXIT.
      *---------------------------------------------------------------*
           EJECT
      *---------------------------------------------------------------*
       2100-LER-ARQDB2   SECTION.
      *---------------------------------------------------------------*


           READ ARQDB2.

           IF WRK-FS-ARQDB2 EQUAL '10'
              MOVE  'S'        TO WRK-FIM-ARQDB2
              GO  TO 2100-99-FIM.

           MOVE WRK-LEITURA             TO  WRK-OPERACAO.
           PERFORM 1100-TESTAR-FS-ARQDB2.

           ADD 1                        TO  ACU-LIDOS-ARQDB2.

           MOVE DB2-NUMERO-DEV          TO  WRK-CGCCPF-NUM-ATU
           MOVE DB2-FILIAL              TO  WRK-CGCCPF-FIL-ATU
           MOVE DB2-CTR-DEV             TO  WRK-CGCCPF-CTR-ATU
           MOVE DB2-AGENCIA             TO  WRK-AGENCIA-ATU
           MOVE DB2-CONTA               TO  WRK-CONTA-ATU
           MOVE DB2-CONTRATO            TO  WRK-CONTRATO-ATU

           IF  DB2-ID EQUAL 'MO'
               IF  DB2-COD-EMPR EQUAL 01
                   MOVE  04120  TO  DB2-EMPRESA
               ELSE
                   IF  DB2-COD-EMPR EQUAL 03
                       MOVE  05240  TO  DB2-EMPRESA
                   ELSE
                       IF  DB2-COD-EMPR EQUAL 04
                           MOVE  05150  TO  DB2-EMPRESA
                       ELSE
                           IF  DB2-COD-EMPR EQUAL 05
                               MOVE  06500  TO  DB2-EMPRESA
                           ELSE
                               IF  DB2-COD-EMPR EQUAL 06
                                   MOVE  04900  TO  DB2-EMPRESA.

      *---------------------------------------------------------------*
       2100-99-FIM. EXIT.
      *---------------------------------------------------------------*
           EJECT

      *---------------------------------------------------------------*
       2200-LER-PARMCLLP             SECTION.
      *---------------------------------------------------------------*

           READ   PARMCLLP.

           IF  WRK-FS-PARMCLLP   EQUAL   '10'
               GO                TO      2200-99-FIM.

           MOVE    WRK-LEITURA             TO   WRK-OPERACAO.
           PERFORM 1500-TESTAR-FS-PARMCLLP.

      *    DISPLAY 'PARM-CCUSTO-1 = '  PARM-CCUSTO-1.
      *    DISPLAY 'PARM-CODIGO-1 = '  PARM-CODIGO-1.
      *    DISPLAY 'PARM-CCUSTO-2 = '  PARM-CCUSTO-2.
      *    DISPLAY 'PARM-CODIGO-2 = '  PARM-CODIGO-2.

           IF  PARM-CCUSTO-1   NOT EQUAL   'CLLP'   OR
               PARM-CODIGO-1   NOT EQUAL   '0002'   OR
               PARM-CCUSTO-2   NOT EQUAL   'CLLP'   OR
               PARM-CODIGO-2   NOT EQUAL   '7615'
               DISPLAY '****************  CLLP7202  *******************'
               DISPLAY '*           ABEND 1111 - FORCADO              *'
               DISPLAY '*  PROGRAMA CANCELADO - PARAMETRO INVALIDO    *'
               DISPLAY '* '  PARM-CCUSTO-1 ' / ' PARM-CODIGO-1
                       ' / ' PARM-CCUSTO-2 ' / ' PARM-CODIGO-2
               DISPLAY '*   AVISAR ANALISTA RESPONSAVEL DA ROTINA     *'
               DISPLAY '*****************  CLLP7202  ******************'
               CALL  'ILBOABN0'  USING  WRK-ABEND.

           IF  PARM-SEQUENCIA   NOT EQUAL   5
               GO               TO   2200-LER-PARMCLLP.

      *---------------------------------------------------------------*
       2200-99-FIM.                  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2300-LER-ARQDATA              SECTION.
      *---------------------------------------------------------------*

           READ   ARQDATA.

           IF  WRK-FS-ARQDATA    EQUAL   '10'
               GO                TO      2300-99-FIM.

           MOVE    WRK-LEITURA             TO   WRK-OPERACAO.
           PERFORM 1400-TESTAR-FS-ARQDATA.


      *---------------------------------------------------------------*
       2300-99-FIM.                  EXIT.
      *---------------------------------------------------------------*


      *---------------------------------------------------------------*
       3000-PROCESSA-ARQDB2    SECTION.
      *---------------------------------------------------------------*


           MOVE WRK-CGCCPF-NUM-ATU     TO WRK-CGCCPF-NUM-ANT
           MOVE WRK-CGCCPF-FIL-ATU     TO WRK-CGCCPF-FIL-ANT
           MOVE WRK-CGCCPF-CTR-ATU     TO WRK-CGCCPF-CTR-ANT
           MOVE WRK-AGENCIA-ATU        TO WRK-AGENCIA-ANT.
           MOVE WRK-CONTA-ATU          TO WRK-CONTA-ANT.
           MOVE WRK-CONTRATO-ATU       TO WRK-CONTRATO-ANT.

           IF (DB2-VCTO                 GREATER   WRK-DT-MENOS01D AND
               DB2-VCTO             NOT GREATER   WRK-DT-AVALISTA)
               PERFORM 3200-MOVER-DADOS-ARQDB2S
                       UNTIL WRK-CH-ARQDB2-ATU NOT EQUAL
                             WRK-CH-ARQDB2-ANT OR
                             WRK-FIM-ARQDB2 EQUAL 'S'
           ELSE
               PERFORM 2100-LER-ARQDB2
                       UNTIL WRK-CH-ARQDB2-ATU NOT EQUAL
                             WRK-CH-ARQDB2-ANT OR
                             WRK-FIM-ARQDB2 EQUAL 'S'.


      *---------------------------------------------------------------*
       3000-99-FIM.  EXIT.
      *---------------------------------------------------------------*
           EJECT
      *---------------------------------------------------------------*
       3200-MOVER-DADOS-ARQDB2S SECTION.
      *---------------------------------------------------------------*

           PERFORM 3300-GRAVAR-ARQDB2S.

           PERFORM 2100-LER-ARQDB2.

      *---------------------------------------------------------------*
       3200-99-FIM.    EXIT.
      *---------------------------------------------------------------*
           EJECT
      *---------------------------------------------------------------*
       3300-GRAVAR-ARQDB2S SECTION.
      *---------------------------------------------------------------*


           WRITE REG-ARQDB2S           FROM DB2-REGTO.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.
           PERFORM 1300-TESTAR-FS-ARQDB2S.

           ADD   1                     TO  ACU-GRAVA-ARQDB2S.

      *---------------------------------------------------------------*
       3300-99-FIM.    EXIT.
      *---------------------------------------------------------------*
           EJECT
      *---------------------------------------------------------------*
       4000-IMPRIME-TOTAIS SECTION.
      *---------------------------------------------------------------*

           MOVE ACU-LIDOS-ARQDB2           TO   LT1-LIDOS-ARQDB2.
           MOVE ACU-GRAVA-ARQDB2S          TO   LT2-GRAVA-ARQDB2S.
           MOVE WRK-GRAVACAO               TO   WRK-OPERACAO.

           MOVE 1                          TO   CB1-PAG.

           WRITE REG-TOTAIS                FROM CABEC1.
           PERFORM 1200-TESTAR-FS-TOTAIS.

           WRITE REG-TOTAIS                FROM CABEC2.
           PERFORM 1200-TESTAR-FS-TOTAIS.

           WRITE REG-TOTAIS                FROM CABEC3.
           PERFORM 1200-TESTAR-FS-TOTAIS.

           WRITE REG-TOTAIS                FROM LINSPACE.
           PERFORM 1200-TESTAR-FS-TOTAIS.

           WRITE REG-TOTAIS                FROM LINTOT1.
           PERFORM 1200-TESTAR-FS-TOTAIS.

           WRITE REG-TOTAIS                FROM LINTOT2.
           PERFORM 1200-TESTAR-FS-TOTAIS.

      *---------------------------------------------------------------*
       4000-99-FIM. EXIT.
      *---------------------------------------------------------------*


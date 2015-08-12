      *===============================================================*
       IDENTIFICATION DIVISION.
      *===============================================================*

       PROGRAM-ID. CLLP7111.
       AUTHOR. MAURICIO FRANCO DA SILVA.
      *===============================================================*
      *                   C P M   S I S T E M A S                     *
      *---------------------------------------------------------------*
      *                                                               *
      *   PROGRAMA...: CLLP7111.                                      *
      *   PROGRAMADOR: MAURICIO - CPM.                                *
      *   ANALISTA...: MAURICIO - CPM.                                *
      *   ORIENTADORA: TOMOKO - CPM.                                  *
      *   DATA.......: MAIO/2001.                                     *
      *                                                               *
      *   OBJETIVO...: ENTRA O ARQUIVO ARQLPCL E SAI OS ARQUIVOS      *
      *                ARQINCON CADLPCL E TOTAIS.                     *
      *                                                               *
      *      ARQUIVOS:                                                *
      *         DDNAME                           INCLUDE/BOOK         *
      *         ARQLPCL                            I#CLLPCJ           *
      *         CADLPCL                                               *
      *         TOTAIS                                                *
      *                                                               *
      *   OBS.: ESTE PROGRAMA EH COPIA DO PROGRAMA CLLP7518           *
      *                                                               *
      *===============================================================*
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

           SELECT  ARQLPCL   ASSIGN  TO  UT-S-ARQLPCL
                   FILE      STATUS  IS  WRK-FS-ARQLPCL.

           SELECT  CADLPCL   ASSIGN  TO  UT-S-CADLPCL
                   FILE      STATUS  IS  WRK-FS-CADLPCL.

           SELECT  TOTAIS    ASSIGN  TO  UT-S-TOTAIS
                   FILE      STATUS  IS  WRK-FS-TOTAIS.

           EJECT
      *===============================================================*
       DATA DIVISION.
      *===============================================================*

      *---------------------------------------------------------------*
       FILE SECTION.
      *---------------------------------------------------------------*

       FD  ARQLPCL
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

BRQ=E******-INC I#CLLPCJ
-INC I#CLLPDM
           EJECT
      *---------------------------------------------------------------*
      *    OUTPUT:    DEVEDORES SELECIONADOS P/ SERASA/SPC            *
      *               ORG. SEQUENCIAL   -   LRECL = 385               *
      *---------------------------------------------------------------*

       FD  CADLPCL
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

*******-INC*I#CLLPLC
-INC I#CLLPDM

           EJECT
      *---------------------------------------------------------------*
      *    OUTPUT:    RELATORIO DE TOTAIS                             *
      *               ORG. SEQUENCIAL   -   LRECL = 080               *
      *---------------------------------------------------------------*

       FD  TOTAIS
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  REG-TOTAIS                       PIC X(80).

           EJECT
      *---------------------------------------------------------------*
       WORKING-STORAGE SECTION.
      *---------------------------------------------------------------*

       77  FILLER                      PIC X(32)        VALUE
           '* INICIO DA WORKING CLLP7111 *'.

       77  WRK-ABEND                   PIC S9(04) COMP  VALUE +1111.
       77  WRK-FIM-ARQLPCL             PIC  X(01)        VALUE  'N'.
       77  WRK-FINALIZA                PIC  X(01)        VALUE  'N'.
       77  WRK-RETURN-CODE             PIC S9(05)        VALUE ZEROS.
       77  WRK-DESVIO                  PIC X(01)         VALUE 'N'.

       77  WRK-FS-ARQLPCL              PIC  X(02)        VALUE  SPACES.
       77  WRK-FS-CADLPCL              PIC  X(02)        VALUE  SPACES.
       77  WRK-FS-TOTAIS               PIC  X(02)        VALUE  SPACES.

       77  WRK-OPERACAO                PIC  X(13) VALUE SPACES.
       77  WRK-ABERTURA                PIC  X(13) VALUE 'NA ABERTURA'.
       77  WRK-LEITURA                 PIC  X(13) VALUE 'NA LEITURA'.
       77  WRK-GRAVACAO                PIC  X(13) VALUE 'NA GRAVACAO'.
       77  WRK-FECHAMENTO              PIC  X(13) VALUE 'NO FECHAMENTO'.

       77  ACU-LIDOS-ARQLPCL           PIC  9(08) COMP-3 VALUE ZEROS.
       77  ACU-GRAVA-CADLPCL           PIC  9(08) COMP-3 VALUE ZEROS.
       77  ACU-VAL-RESG                PIC  9(11)V99     VALUE ZEROS.
       77  ACU-ENC-VENC                PIC  9(11)V99     VALUE ZEROS.
       77  ACU-ENC-VINC                PIC  9(11)V99     VALUE ZEROS.
       77  ACU-CONT-NAT                PIC  9(05) COMP-3 VALUE ZEROS.
       77  ACU-CONT-REG                PIC  9(05) COMP-3 VALUE ZEROS.
       77  ACU-CONT-REGC               PIC  9(05) COMP-3 VALUE 1.

       01  WRK-VCTO-INV                PIC  9(08).
       01  FILLER   REDEFINES   WRK-VCTO-INV.
           03  WRK-VCTO-INV-AAAA       PIC  9(04).
           03  WRK-VCTO-INV-MM         PIC  9(02).
           03  WRK-VCTO-INV-DD         PIC  9(02).

       01  WRK-DT-MOV                  PIC  9(08).
       01  FILLER   REDEFINES   WRK-DT-MOV.
           03  WRK-DT-MOV-AAAA         PIC  9(04).
           03  WRK-DT-MOV-MM           PIC  9(02).
           03  WRK-DT-MOV-DD           PIC  9(02).

       01  WRK-TI-SYS                  PIC  9(06).
       01  FILLER   REDEFINES   WRK-TI-SYS.
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
           03  WRK-DT-SAIDA25          PIC  9(09) COMP-3 VALUE ZEROS.

       01  WRK-AREA-POOL0110.
           03  WRK-POOL0110.
               05  WRK-NUMERO-0110     PIC  9(09)        VALUE ZEROS.
               05  WRK-FILIAL-0110     PIC  9(05)        VALUE ZEROS.
           03  WRK-CONTROLE-0110       PIC  X(02)        VALUE SPACES.

       01  WRK-CONTROLE-0110-AUX       PIC  9(03)   COMP-3.

       01  AUX-DT-INV-AAAAMMDD         PIC  9(08).
       01  FILLER   REDEFINES   AUX-DT-INV-AAAAMMDD.
           03  AUX-DT-INV-AAAA         PIC  9(04).
           03  AUX-DT-INV-MM           PIC  9(02).
           03  AUX-DT-INV-DD           PIC  9(02).

       01  AUX-DT-DB2.
           03  AUX-DT-DB2-DD           PIC  9(02).
           03  FILLER                  PIC  X(01).
           03  AUX-DT-DB2-MM           PIC  9(02).
           03  FILLER                  PIC  X(01).
           03  AUX-DT-DB2-AAAA         PIC  9(04).
           03  FILLER   REDEFINES   AUX-DT-DB2-AAAA.
               05  AUX-DT-DB2-SEC      PIC  9(02).
               05  AUX-DT-DB2-ANO      PIC  9(02).

       01  WRK-CH-ARQLPCL-ATU.
           03  WRK-CPF-NUM-ATU         PIC 9(09)   COMP-3.
           03  WRK-CPF-FIL-ATU         PIC 9(05)   COMP-3.
           03  WRK-CPF-CTR-ATU         PIC 9(03)   COMP-3.
           03  WRK-CPF-NUM-AVA-ATU     PIC 9(09)   COMP-3.
           03  WRK-CPF-FIL-AVA-ATU     PIC 9(05)   COMP-3.
           03  WRK-CPF-CTR-AVA-ATU     PIC 9(03)   COMP-3.
           03  WRK-AGENCIA-ATU         PIC 9(05)   COMP-3.
           03  WRK-CONTA-ATU           PIC 9(07)   COMP-3.
           03  WRK-NATUREZA-ATU        PIC X(02).

       01  WRK-CH-ARQLPCL-ANT.
           03  WRK-CPF-NUM-ANT         PIC 9(09)   COMP-3.
           03  WRK-CPF-FIL-ANT         PIC 9(05)   COMP-3.
           03  WRK-CPF-CTR-ANT         PIC 9(03)   COMP-3.
           03  WRK-CPF-NUM-AVA-ANT     PIC 9(09)   COMP-3.
           03  WRK-CPF-FIL-AVA-ANT     PIC 9(05)   COMP-3.
           03  WRK-CPF-CTR-AVA-ANT     PIC 9(03)   COMP-3.
           03  WRK-AGENCIA-ANT         PIC 9(05)   COMP-3.
           03  WRK-CONTA-ANT           PIC 9(07)   COMP-3.
           03  WRK-NATUREZA-ANT        PIC X(02).

       01  WRK-CONTRATO-ATU      PIC 9(07)   COMP-3.

       01  WRK-CONTRATO-ANT      PIC 9(07)   COMP-3.

      *---------------------------------------------------------------*
      *                 DEFINICAO DO RELATORIO DE TOTAIS              *
      *---------------------------------------------------------------*

       01  CABEC1.
           03  CB1-CARRO               PIC X(01) VALUE '1'.
           03  FILLER                  PIC X(28) VALUE 'CLLP7111'.
SID        03  CB1-NOME-BCO            PIC X(37) VALUE SPACES.
SID************'BANCO   BRADESCO  S.A.'.
           03  FILLER                  PIC X(7)   VALUE 'FOLHA:'.
           03  CB1-PAG                 PIC ZZZ.ZZ9.

       01  CABEC2.
           03  CB2-CARRO               PIC X(01) VALUE '0'.
           03  FILLER.
               05  CB2-DTSYS-DD        PIC 9(2)/.
               05  CB2-DTSYS-MM        PIC 9(2)/.
               05  CB2-DTSYS-AAAA      PIC 9(4).
           03  FILLER                  PIC X(07) VALUE SPACES.
           03  FILLER                  PIC X(43) VALUE
               'RELATORIO DE TOTAIS - AVISO PARA AVALISTAS'.
           03  FILLER                  PIC X(04) VALUE SPACES.
           03  FILLER.
               05  CB2-TIME-HOR        PIC 9(2).
               05  FILLER              PIC X(1) VALUE ':'.
               05  CB2-TIME-MIN        PIC 9(2).
               05  FILLER              PIC X(1) VALUE ':'.
               05  CB2-TIME-SEG        PIC 9(2).


       01  LINSPACE.
           03  LP1-CARRO               PIC X(01) VALUE '-'.
           03  FILLER                  PIC X(79) VALUE SPACES.

       01  LINTOT1.
           03  LT1-CARRO               PIC X(01) VALUE '0'.
           03  FILLER                  PIC X(17) VALUE SPACES.
           03  FILLER                  PIC X(32) VALUE
               'REGISTROS LIDOS - ARQLPCL     = '.
           03  LT1-LIDOS-ARQLPCL       PIC ZZ.ZZZ.ZZ9.

       01  LINTOT2.
           03  LT2-CARRO               PIC X(01) VALUE '0'.
           03  FILLER                  PIC X(17) VALUE SPACES.
           03  FILLER                  PIC X(32) VALUE
               'REGISTROS GRAVADOS - CADLPCL  = '.
           03  LT2-GRAVA-CADLPCL       PIC ZZ.ZZZ.ZZ9.

       01  FILLER                      PIC X(32)        VALUE
           '*  FIM DA WORKING CLLP7111 *'.
           EJECT

SID   *================================================================*
SID    LINKAGE                         SECTION.
SID   *================================================================*
SID
SID    01  LNK-CAMPOS-PARM.
SID        03 LNK-TAMANHO-PARM              PIC S9(04)  COMP.
SID        03 LNK-CCUSTO-PARM               PIC X(04).

      *===============================================================*
SID    PROCEDURE      DIVISION      USING       LNK-CAMPOS-PARM.
      *===============================================================*

      *---------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *---------------------------------------------------------------*

           OPEN  INPUT   ARQLPCL
                 OUTPUT  CADLPCL  TOTAIS.

           MOVE  WRK-ABERTURA          TO   WRK-OPERACAO.
           PERFORM   1000-TESTAR-FILE-STATUS.

           PERFORM   2000-INICIALIZA.

           IF  WRK-FINALIZA   EQUAL   'S'
               GO             TO      0000-FINALIZAR.

           PERFORM   3000-PROCESSA-ARQLPCL  UNTIL
                           WRK-FIM-ARQLPCL  EQUAL   'S'.

           PERFORM   4000-IMPRIME-TOTAIS.

       0000-FINALIZAR.
      *---------------*

           CLOSE  ARQLPCL   CADLPCL   TOTAIS.

           MOVE  WRK-FECHAMENTO        TO   WRK-OPERACAO.
           PERFORM   1000-TESTAR-FILE-STATUS.

           GOBACK.

      *---------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *---------------------------------------------------------------*
           EJECT
      *---------------------------------------------------------------*
       1000-TESTAR-FILE-STATUS         SECTION.
      *---------------------------------------------------------------*

           PERFORM   1100-TESTAR-FS-ARQLPCL.

           PERFORM   1200-TESTAR-FS-TOTAIS.

           PERFORM   1300-TESTAR-FS-CADLPCL.

      *---------------------------------------------------------------*
       1000-99-FIM. EXIT.
      *---------------------------------------------------------------*
           EJECT
      *---------------------------------------------------------------*
       1100-TESTAR-FS-ARQLPCL          SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-ARQLPCL  NOT EQUAL   '00'
               DISPLAY '************** CLLP7111 *************'
               DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
               DISPLAY '*              ARQLPCL              *'
               DISPLAY '*         FILE STATUS =  ' WRK-FS-ARQLPCL
                                                  '         *'
               DISPLAY '************** CLLP7111 *************'
               CALL   'ILBOABN0'   USING   WRK-ABEND.

      *---------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *---------------------------------------------------------------*
           EJECT
      *---------------------------------------------------------------*
       1200-TESTAR-FS-TOTAIS           SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-TOTAIS   NOT EQUAL   '00'
               DISPLAY '************** CLLP7111 *************'
               DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
               DISPLAY '*              TOTAIS               *'
               DISPLAY '*         FILE STATUS =  ' WRK-FS-TOTAIS
                                                  '         *'
               DISPLAY '************** CLLP7111 *************'
               CALL   'ILBOABN0'   USING   WRK-ABEND.

      *---------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *---------------------------------------------------------------*
           EJECT
      *---------------------------------------------------------------*
       1300-TESTAR-FS-CADLPCL          SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-CADLPCL  NOT EQUAL   '00'
               DISPLAY '************** CLLP7111 *************'
               DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
               DISPLAY '*              CADLPCL              *'
               DISPLAY '*         FILE STATUS =  ' WRK-FS-CADLPCL
                                                  '         *'
               DISPLAY '************** CLLP7111 *************'
               CALL   'ILBOABN0'   USING   WRK-ABEND.

      *---------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *---------------------------------------------------------------*
           EJECT
      *---------------------------------------------------------------*
       2000-INICIALIZA                 SECTION.
      *---------------------------------------------------------------*

           MOVE   'N'                  TO   CONTR-MULTIPLO-CADLPCL
                                                          OF CADLPCL.

           CALL   'POOL7600'   USING   WRK-AREA-POOL7600.

           MOVE  WRK-DT-AAAAMMDD       TO   AUX-DT-INV-AAAAMMDD.
           MOVE  AUX-DT-INV-AAAA       TO   CB2-DTSYS-AAAA.
           MOVE  AUX-DT-INV-MM         TO CB2-DTSYS-MM
           MOVE  AUX-DT-INV-DD         TO CB2-DTSYS-DD.

           MOVE  WRK-TI-HHMMSS         TO   WRK-TI-SYS.
           MOVE  WRK-TI-HOR            TO   CB2-TIME-HOR.
           MOVE  WRK-TI-MIN            TO   CB2-TIME-MIN.
           MOVE  WRK-TI-SEG            TO   CB2-TIME-SEG.

           MOVE  WRK-DT-SAIDA          TO   WRK-DT-MOV.

           PERFORM   2100-LER-ARQLPCL.

      *---------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *---------------------------------------------------------------*
           EJECT
      *---------------------------------------------------------------*
       2100-LER-ARQLPCL                SECTION.
      *---------------------------------------------------------------*

           READ   ARQLPCL.

           IF  WRK-FS-ARQLPCL  EQUAL   '10'
               MOVE  'S'               TO   WRK-FIM-ARQLPCL
                                            WRK-FINALIZA
               GO                      TO   2100-99-FIM.

           MOVE  WRK-LEITURA           TO   WRK-OPERACAO.
           PERFORM   1100-TESTAR-FS-ARQLPCL.

           ADD   1                     TO   ACU-LIDOS-ARQLPCL.

           MOVE  CGCCPF-NUM-WOR OF ARQLPCL    TO   WRK-CPF-NUM-ATU.
           MOVE  CGCCPF-FIL-WOR OF ARQLPCL    TO   WRK-CPF-FIL-ATU.
           MOVE  CGCCPF-CTR-WOR OF ARQLPCL    TO   WRK-CPF-CTR-ATU.
           MOVE  CAD-NCGC1-WOR  OF ARQLPCL    TO   WRK-CPF-NUM-AVA-ATU.
           MOVE  CAD-FIL1-WOR OF ARQLPCL      TO   WRK-CPF-FIL-AVA-ATU.
           MOVE  CAD-CTR1-WOR OF ARQLPCL      TO   WRK-CPF-CTR-AVA-ATU.
           MOVE  NATUREZA-WOR OF ARQLPCL      TO   WRK-NATUREZA-ATU.
           MOVE  AGENCIA-WOR  OF ARQLPCL      TO   WRK-AGENCIA-ATU.
           MOVE  CONTA-WOR  OF ARQLPCL        TO   WRK-CONTA-ATU.

           MOVE  CONTRATO-WOR   OF ARQLPCL   TO   WRK-CONTRATO-ATU.

      *---------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *---------------------------------------------------------------*
           EJECT
      *---------------------------------------------------------------*
       3000-PROCESSA-ARQLPCL           SECTION.
      *---------------------------------------------------------------*

           PERFORM   3200-MOVER-DADOS-CADLPCL.

           PERFORM   3300-GRAVAR-CADLPCL.

           PERFORM   3400-ZERAR-ACUMULADORES.

      *---------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *---------------------------------------------------------------*
           EJECT
      *---------------------------------------------------------------*
       3200-MOVER-DADOS-CADLPCL        SECTION.
      *---------------------------------------------------------------*

           MOVE  REG-WOR-EXP OF ARQLPCL TO REG-WOR-EXP OF CADLPCL

           MOVE  WRK-CH-ARQLPCL-ATU    TO   WRK-CH-ARQLPCL-ANT.

           MOVE  WRK-CONTRATO-ATU      TO   WRK-CONTRATO-ANT.

           PERFORM   3210-ACUMULAR-VALORES   UNTIL
                        WRK-CH-ARQLPCL-ATU   NOT EQUAL
                        WRK-CH-ARQLPCL-ANT   OR
                        WRK-FIM-ARQLPCL      EQUAL   'S'.

           MOVE  ACU-VAL-RESG      TO   VAL-RESG-WOR OF CADLPCL
           MOVE  ACU-ENC-VENC      TO   VAL-ENC-VEN-WOR OF CADLPCL
           MOVE  ACU-ENC-VINC      TO   VAL-ENC-VIN-WOR OF CADLPCL.
      *---------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *---------------------------------------------------------------*
           EJECT
      *---------------------------------------------------------------*
       3210-ACUMULAR-VALORES           SECTION.
      *---------------------------------------------------------------*

           ADD  VAL-RESG-WOR OF ARQLPCL           TO   ACU-VAL-RESG.
           ADD  VAL-ENC-VEN-WOR OF ARQLPCL        TO   ACU-ENC-VENC.
           ADD  VAL-ENC-VIN-WOR OF ARQLPCL        TO   ACU-ENC-VINC.
           ADD  1                      TO   ACU-CONT-REG.

           IF   WRK-CONTRATO-ATU     NOT EQUAL  WRK-CONTRATO-ANT
                ADD     1              TO   ACU-CONT-REGC.

           PERFORM   2100-LER-ARQLPCL.

      *---------------------------------------------------------------*
       3210-99-FIM.                    EXIT.
      *---------------------------------------------------------------*
           EJECT
      *---------------------------------------------------------------*
       3300-GRAVAR-CADLPCL             SECTION.
      *---------------------------------------------------------------*

           WRITE   REG-WOR-EXP OF CADLPCL.

           MOVE  WRK-GRAVACAO          TO   WRK-OPERACAO.
           PERFORM   1300-TESTAR-FS-CADLPCL.

           ADD   1                     TO   ACU-GRAVA-CADLPCL.

      *---------------------------------------------------------------*
       3300-99-FIM.                    EXIT.
      *---------------------------------------------------------------*
           EJECT
      *---------------------------------------------------------------*
       3400-ZERAR-ACUMULADORES         SECTION.
      *---------------------------------------------------------------*

           MOVE  ZEROS                 TO   ACU-VAL-RESG
                                            ACU-ENC-VENC
                                            ACU-ENC-VINC
                                            ACU-CONT-REG

           MOVE  1                     TO   ACU-CONT-REGC.

      *---------------------------------------------------------------*
       3400-99-FIM.                    EXIT.
      *---------------------------------------------------------------*
           EJECT
      *---------------------------------------------------------------*
       4000-IMPRIME-TOTAIS             SECTION.
      *---------------------------------------------------------------*

           MOVE  ACU-LIDOS-ARQLPCL     TO   LT1-LIDOS-ARQLPCL.
           MOVE  ACU-GRAVA-CADLPCL     TO   LT2-GRAVA-CADLPCL.
           MOVE  WRK-GRAVACAO          TO   WRK-OPERACAO.

           MOVE  1                     TO   CB1-PAG.

SID        IF  LNK-CCUSTO-PARM EQUAL 'LEAS'
SID            MOVE 'BRADESCO LEASING S/A' TO CB1-NOME-BCO
SID        ELSE
SID            MOVE 'BANCO BRADESCO S.A  ' TO CB1-NOME-BCO
SID        END-IF.

           WRITE   REG-TOTAIS          FROM   CABEC1.
           PERFORM   1200-TESTAR-FS-TOTAIS.

           WRITE   REG-TOTAIS          FROM   CABEC2.
           PERFORM   1200-TESTAR-FS-TOTAIS.

           WRITE   REG-TOTAIS          FROM   LINSPACE.
           PERFORM   1200-TESTAR-FS-TOTAIS.

           WRITE   REG-TOTAIS          FROM   LINTOT1.
           PERFORM   1200-TESTAR-FS-TOTAIS.

           WRITE   REG-TOTAIS          FROM   LINTOT2.
           PERFORM   1200-TESTAR-FS-TOTAIS.

      *---------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

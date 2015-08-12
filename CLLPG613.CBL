      *===============================================================*
       IDENTIFICATION DIVISION.
      *===============================================================*

       PROGRAM-ID. CLLPG613.
       AUTHOR. FRANCISCO FREIRE.
      *===============================================================*
      *                   C P M   S I S T E M A S                     *
      *---------------------------------------------------------------*
      *                                                               *
      *      PROGRAMA     : CLLPG613                                  *
      *      PROGRAMADOR  : FRANCISCO FREIRE (BRQ)                    *
      *      ANALISTA     : FRANCISCO FREIRE (BRQ)                    *
      *      DATA         : 23/01/2015                                *
      *                                                               *
      *      OBJETIVO     :                                           *
      *        GERAR CACALPCL (TAMANHO 313 BYTES COMPRIMIDO) A PARTIR *
      *        DO ARQDB2 (IMAGEM DO DB2 LPCLB998).                    *
      *                                                               *
      *      ARQUIVOS:                                                *
      *         DDNAME                           INCLUDE/BOOK         *
      *         ARQDB2                                                *
      *         ARQDB2S                                               *
      *         TOTAIS                                                *
      *                                                               *
      *                                                               *
      *===============================================================*
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

           SELECT  ARQDB2   ASSIGN TO UT-S-ARQDB2
                      FILE STATUS IS WRK-FS-ARQDB2.

           SELECT  ARQDB2S  ASSIGN TO UT-S-ARQDB2S
                      FILE STATUS IS WRK-FS-ARQDB2S.

           SELECT   TOTAIS  ASSIGN TO UT-S-TOTAIS
                      FILE STATUS IS WRK-FS-TOTAIS.
      *===============================================================*
       DATA DIVISION.
      *===============================================================*

      *---------------------------------------------------------------*
       FILE SECTION.
      *---------------------------------------------------------------*


      *---------------------------------------------------------------*
      *    INPUT:     ARQQUIVO DE TABELA DB2                          *
      *               ORG. SEQUENCIAL   -   LRECL = 690               *
      *---------------------------------------------------------------*

       FD  ARQDB2
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

           COPY 'I#CLLPFD'.

      *---------------------------------------------------------------*
      *    INPUT:     ARQQUIVO DE TABELA DB2                          *
      *               ORG. SEQUENCIAL   -   LRECL = 313               *
      *---------------------------------------------------------------*

       FD  ARQDB2S
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

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
           03  DB2-LT.
                05 DB2-TAXA-CONTRATO       PIC 9(02)V9(06)  COMP-3.
                05 DB2-VR-REMUNERATORIO    PIC S9(13)V99    COMP-3.
                05 DB2-VALOR-MORATORIO     PIC S9(13)V99    COMP-3.
                05 DB2-VALOR-MULTA         PIC S9(13)V99    COMP-3.
                05 DB2-DESP-JUD-CUSTAS     PIC S9(11)V99    COMP-3.
                05 DB2-HONORARIOS          PIC S9(11)V99    COMP-3.
                05 DB2-VL-TOTAL-DIVIDA     PIC S9(15)V99    COMP-3.
                05 DB2-VL-TAXA-TARIFA      PIC S9(15)V99    COMP-3.
                05 FILLER                  PIC  X(39).

      *---------------------------------------------------------------*
      *    OUTPUT:    RELATORIO DE TOTAIS                             *
      *               ORG. SEQUENCIAL   -   LRECL = 080               *
      *---------------------------------------------------------------*

       FD  TOTAIS
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  REG-TOTAIS                  PIC X(80).

      *---------------------------------------------------------------*
       WORKING-STORAGE SECTION.
      *---------------------------------------------------------------*

       77  FILLER                      PIC X(32)        VALUE
           '* INICIO DA WORKING CLLPG613 *'.

       77  WRK-ABEND                   PIC S9(04) COMP  VALUE +1111.
       77  ACU-LIDOS-ARQDB2            PIC  9(08) COMP-3 VALUE ZEROS.
       77  ACU-GRAVA-ARQDB2S           PIC  9(08) COMP-3 VALUE ZEROS.
       77  WRK-FIM-ARQDB2              PIC  X(01)        VALUE  'N'.
       77  WRK-FINALIZA                PIC  X(01)        VALUE  'N'.
       77  WRK-IND                     PIC  9(03)        VALUE ZEROS.
       77  WRK-RETURN-CODE             PIC S9(05)        VALUE ZEROS.

       77  WRK-FS-ARQDB2               PIC  X(02)        VALUE  SPACES.
       77  WRK-FS-ARQDB2S              PIC  X(02)        VALUE  SPACES.
       77  WRK-FS-TOTAIS               PIC  X(02)        VALUE  SPACES.

       77  WRK-OPERACAO                PIC  X(13) VALUE SPACES.
       77  WRK-ABERTURA                PIC  X(13) VALUE 'NA ABERTURA'.
       77  WRK-LEITURA                 PIC  X(13) VALUE 'NA LEITURA'.
       77  WRK-GRAVACAO                PIC  X(13) VALUE 'NA GRAVACAO'.
       77  WRK-FECHAMENTO              PIC  X(13) VALUE 'NO FECHAMENTO'.

       01  WRK-CARTEIRA                PIC  X(03).
       01  FILLER  REDEFINES WRK-CARTEIRA.
           03  WRK-CART                PIC  9(03).

       01  WRK-COD-NATUREZA-OPER       PIC  X(03).
       01  FILLER  REDEFINES WRK-COD-NATUREZA-OPER.
           03  WRK-NATUREZA            PIC  9(03).

       01  WRK-VCTO-INV                PIC  9(08).
       01  FILLER  REDEFINES  WRK-VCTO-INV.
           03  WRK-VCTO-INV-AAAA       PIC  9(04).
           03  WRK-VCTO-INV-MM         PIC  9(02).
           03  WRK-VCTO-INV-DD         PIC  9(02).

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

       01  AUX-DT-INV-AAAAMMDD         PIC  9(08).
       01  FILLER  REDEFINES  AUX-DT-INV-AAAAMMDD.
           03  AUX-DT-INV-AAAA         PIC  9(04).
           03  AUX-DT-INV-MM           PIC  9(02).
           03  AUX-DT-INV-DD           PIC  9(02).

       01  AUX-DT-DDMMAA               PIC  9(08).
       01  FILLER  REDEFINES  AUX-DT-DDMMAA.
           03  AUX-DT-DD               PIC  9(02).
           03  AUX-DT-MM               PIC  9(02).
           03  AUX-DT-AA               PIC  9(04).

       01  AUX-DT-DB2.
           03  AUX-DT-DB2-DD           PIC  9(02).
           03  FILLER                  PIC  X(01).
           03  AUX-DT-DB2-MM           PIC  9(02).
           03  FILLER                  PIC  X(01).
           03  AUX-DT-DB2-AAAA         PIC  9(04).
           03  FILLER REDEFINES AUX-DT-DB2-AAAA.
               05  AUX-DT-DB2-SEC      PIC  9(02).
               05  AUX-DT-DB2-ANO      PIC  9(02).

      *---------------------------------------------------------------*
      *                 DEFINICAO DO RELATORIO DE TOTAIS              *
      *---------------------------------------------------------------*

       01  CABEC1.
           03  CB1-CARRO               PIC X(01) VALUE '1'.
           03  FILLER                  PIC X(28) VALUE 'CLLPG613'.
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
               'RELATORIO DE TOTAIS                 '.
           03  FILLER                  PIC X(17) VALUE SPACES.
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
               'REGISTROS LIDOS - ARQDB2      = '.
           03  LT1-LIDOS-ARQDB2        PIC ZZ.ZZZ.ZZ9.


       01  LINTOT2.
           03  LT2-CARRO               PIC X(01) VALUE '0'.
           03  FILLER                  PIC X(17) VALUE SPACES.
           03  FILLER                  PIC X(32) VALUE
               'REGISTROS GRAVADOS - CADALPCL = '.
           03  LT2-GRAVA-CADALPCL      PIC ZZ.ZZZ.ZZ9.


       01  FILLER                      PIC X(32)        VALUE
           '*  FIM DA WORKING CLLPG613 *'.
      *===============================================================*
       PROCEDURE DIVISION.
      *===============================================================*

      *---------------------------------------------------------------*
       0000-INICIAR SECTION.
      *---------------------------------------------------------------*

           OPEN INPUT   ARQDB2
                        ARQDB2S
                OUTPUT  TOTAIS.

           MOVE    WRK-ABERTURA            TO   WRK-OPERACAO.
           PERFORM 1000-TESTAR-FILE-STATUS.

           PERFORM 2000-INICIALIZA.

           IF  WRK-FINALIZA EQUAL 'S'
               GO  TO  0000-FINALIZAR.

           PERFORM 3000-PROCESSA-ARQDB2 UNTIL
                                        WRK-FIM-ARQDB2 EQUAL 'S'.

           PERFORM  4000-IMPRIME-TOTAIS.

       0000-FINALIZAR.
      *---------------*

           CLOSE  ARQDB2
                  ARQDB2S
                  TOTAIS.

           MOVE    WRK-FECHAMENTO          TO   WRK-OPERACAO.
           PERFORM 1000-TESTAR-FILE-STATUS.

           STOP RUN.

      *---------------------------------------------------------------*
       0000-99-FIM. EXIT.
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
       1000-TESTAR-FILE-STATUS SECTION.
      *---------------------------------------------------------------*

           PERFORM 1100-TESTAR-FS-ARQDB2.

           PERFORM 1110-TESTAR-FS-ARQDB2S.

           PERFORM 1200-TESTAR-FS-TOTAIS.

      *---------------------------------------------------------------*
       1000-99-FIM. EXIT.
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
       1100-TESTAR-FS-ARQDB2  SECTION.
      *---------------------------------------------------------------*

           IF WRK-FS-ARQDB2 NOT EQUAL '00'
              DISPLAY '************** CLLPG613 *************'
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
              DISPLAY '*              ARQDB2               *'
              DISPLAY '*         FILE STATUS =  ' WRK-FS-ARQDB2
                                                 '         *'
              DISPLAY '************** CLLPG613 *************'
              CALL 'ILBOABN0'     USING WRK-ABEND.

      *---------------------------------------------------------------*
       1100-99-FIM. EXIT.
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
       1110-TESTAR-FS-ARQDB2S  SECTION.
      *---------------------------------------------------------------*

           IF WRK-FS-ARQDB2S NOT EQUAL '00'
              DISPLAY '************** CLLPG613 *************'
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
              DISPLAY '*              ARQDB2S              *'
              DISPLAY '*         FILE STATUS =  ' WRK-FS-ARQDB2S
                                                 '         *'
              DISPLAY '************** CLLPG613 *************'
              CALL 'ILBOABN0'     USING WRK-ABEND.

      *---------------------------------------------------------------*
       1110-99-FIM. EXIT.
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
       1200-TESTAR-FS-TOTAIS   SECTION.
      *---------------------------------------------------------------*

           IF WRK-FS-TOTAIS NOT EQUAL '00'
              DISPLAY '************** CLLPG613 *************'
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
              DISPLAY '*              TOTAIS               *'
              DISPLAY '*         FILE STATUS =  ' WRK-FS-TOTAIS
                                                 '         *'
              DISPLAY '************** CLLPG613 *************'
              CALL 'ILBOABN0'     USING WRK-ABEND.

      *---------------------------------------------------------------*
       1200-99-FIM. EXIT.
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
       2000-INICIALIZA SECTION.
      *---------------------------------------------------------------*


           CALL   'POOL7600' USING WRK-AREA-POOL7600.

           MOVE   WRK-DT-AAAAMMDD       TO  AUX-DT-INV-AAAAMMDD.
           MOVE   AUX-DT-INV-DD         TO  CB2-DTSYS-DD.
           MOVE   AUX-DT-INV-MM         TO  CB2-DTSYS-MM.
           MOVE   AUX-DT-INV-AAAA       TO  CB2-DTSYS-AAAA.

           MOVE   WRK-TI-HHMMSS         TO  WRK-TI-SYS.
           MOVE   WRK-TI-HOR            TO  CB2-TIME-HOR.
           MOVE   WRK-TI-MIN            TO  CB2-TIME-MIN.
           MOVE   WRK-TI-SEG            TO  CB2-TIME-SEG.

           PERFORM 2100-LER-ARQDB2.

      *---------------------------------------------------------------*
       2000-99-FIM. EXIT.
      *---------------------------------------------------------------*
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

           MOVE  DB2-VCTO OF ARQDB2     TO  AUX-DT-DB2.
           MOVE  AUX-DT-DB2-DD          TO  WRK-VCTO-INV-DD
           MOVE  AUX-DT-DB2-MM          TO  WRK-VCTO-INV-MM
           MOVE  AUX-DT-DB2-AAAA        TO  WRK-VCTO-INV-AAAA.

      *---------------------------------------------------------------*
       2100-99-FIM. EXIT.
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
       3000-PROCESSA-ARQDB2    SECTION.
      *---------------------------------------------------------------*



           MOVE DB2-CPF-AVAL2 OF ARQDB2  TO DB2-CPF-DEV       OF ARQDB2S

           MOVE  DB2-COD-NATUREZA-OPER OF ARQDB2
                                     TO DB2-COD-NATUREZA-OPER OF ARQDB2S

           MOVE  DB2-CONTRATO  OF ARQDB2 TO DB2-CONTRATO      OF ARQDB2S
           MOVE  DB2-VCTO      OF ARQDB2 TO DB2-VCTO          OF ARQDB2S
           MOVE  DB2-EMPRESA   OF ARQDB2 TO DB2-EMPRESA       OF ARQDB2S
           MOVE  DB2-AGENCIA   OF ARQDB2 TO DB2-AGENCIA       OF ARQDB2S
           MOVE  DB2-CONTA     OF ARQDB2 TO DB2-CONTA         OF ARQDB2S
           MOVE  DB2-CARTEIRA  OF ARQDB2 TO DB2-CARTEIRA      OF ARQDB2S
           MOVE  DB2-ID        OF ARQDB2 TO DB2-ID            OF ARQDB2S
           MOVE  DB2-MOEDA     OF ARQDB2 TO DB2-MOEDA         OF ARQDB2S
           MOVE  DB2-TIPO-PEND OF ARQDB2 TO DB2-TIPO-PEND     OF ARQDB2S
           MOVE  DB2-CODIGO    OF ARQDB2 TO DB2-CODIGO        OF ARQDB2S
           MOVE  DB2-PRINCIPAL OF ARQDB2 TO DB2-PRINCIPAL     OF ARQDB2S

           MOVE  DB2-VR-VENCIDOS OF ARQDB2
                                         TO DB2-VR-VENCIDOS   OF ARQDB2S
           MOVE  DB2-VR-VINCENDOS OF ARQDB2
                                         TO DB2-VR-VINCENDOS  OF ARQDB2S

           MOVE  DB2-NOME-DEVEDOR OF ARQDB2
                                         TO DB2-NOME-DEVEDOR  OF ARQDB2S

           MOVE  DB2-NOME-AVALISTA
                               OF ARQDB2 TO DB2-NOME-AVALISTA OF ARQDB2S

           MOVE  DB2-CPF-AVAL  OF ARQDB2 TO DB2-CPF-AVAL      OF ARQDB2S
           MOVE  DB2-NOME-AVAL2 OF ARQDB2  TO DB2-NOME-AVAL2  OF ARQDB2S

           MOVE  DB2-CPF-AVAL2 OF ARQDB2 TO DB2-CPF-AVAL2     OF ARQDB2S
           MOVE  DB2-COD-EMPR  OF ARQDB2  TO DB2-COD-EMPR     OF ARQDB2S

           MOVE   DB2-TAXA-CONTRATO    OF ARQDB2
                               TO     DB2-TAXA-CONTRATO    OF ARQDB2S
           MOVE   DB2-VR-REMUNERATORIO OF ARQDB2
                               TO     DB2-VR-REMUNERATORIO OF ARQDB2S
           MOVE   DB2-VALOR-MORATORIO  OF ARQDB2
                               TO     DB2-VALOR-MORATORIO  OF ARQDB2S
           MOVE   DB2-VALOR-MULTA      OF ARQDB2
                               TO     DB2-VALOR-MULTA      OF ARQDB2S
           MOVE   DB2-DESP-JUD-CUSTAS  OF ARQDB2
                               TO     DB2-DESP-JUD-CUSTAS  OF ARQDB2S
           MOVE   DB2-HONORARIOS       OF ARQDB2
                               TO     DB2-HONORARIOS       OF ARQDB2S
           MOVE   DB2-VL-TOTAL-DIVIDA  OF ARQDB2
                               TO     DB2-VL-TOTAL-DIVIDA  OF ARQDB2S
           MOVE   DB2-VL-TAXA-TARIFA   OF ARQDB2
                               TO     DB2-VL-TAXA-TARIFA   OF ARQDB2S

      *    VERIFICAR SE OS CAMPOS DA TRANSPARENCIA ESTAO PREENCHIDOS
      *    CASO NAO ESTIVEREM O TOTAL DA DIVIDA SERA O VALOR DE COBRANCA

           IF      DB2-VR-REMUNERATORIO OF ARQDB2 EQUAL ZEROS
               AND DB2-VALOR-MORATORIO  OF ARQDB2 EQUAL ZEROS
               AND DB2-VALOR-MULTA      OF ARQDB2 EQUAL ZEROS
               AND DB2-DESP-JUD-CUSTAS  OF ARQDB2 EQUAL ZEROS
               AND DB2-HONORARIOS       OF ARQDB2 EQUAL ZEROS
               AND DB2-VL-TAXA-TARIFA   OF ARQDB2 EQUAL ZEROS

               MOVE  DB2-COBRANCA OF ARQDB2
                               TO     DB2-VL-TOTAL-DIVIDA  OF ARQDB2S
           ELSE
               MOVE  DB2-VL-TOTAL-DIVIDA OF ARQDB2
                               TO     DB2-VL-TOTAL-DIVIDA  OF ARQDB2S
           END-IF

           WRITE   DB2-REGTO OF ARQDB2S
           PERFORM 1110-TESTAR-FS-ARQDB2S.

           ADD   1                     TO  ACU-GRAVA-ARQDB2S.

           PERFORM  2100-LER-ARQDB2.

      *---------------------------------------------------------------*
       3000-99-FIM.  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       4000-IMPRIME-TOTAIS SECTION.
      *---------------------------------------------------------------*

           MOVE ACU-LIDOS-ARQDB2           TO   LT1-LIDOS-ARQDB2.
           MOVE ACU-GRAVA-ARQDB2S          TO   LT2-GRAVA-CADALPCL.
           MOVE WRK-GRAVACAO               TO   WRK-OPERACAO.

           MOVE 1                          TO   CB1-PAG.

           WRITE REG-TOTAIS                FROM CABEC1.
           PERFORM 1200-TESTAR-FS-TOTAIS.

           WRITE REG-TOTAIS                FROM CABEC2.
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

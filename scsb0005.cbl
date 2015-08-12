      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. SCSB0005.
       AUTHOR.     FRANCISCO FREIRE.

      *================================================================*
      *               B R Q   -   I T   S E R V I C E S                *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA     : SCSB0005                                     *
      *    ANALISTA     : FERNANDA CARUSO        - BRQ IT SERVICES.    *
      *    PROGRAMADOR  : FRANCISCO FREIRE       - BRQ IT SERVICES.    *
      *    DATA         : 04/08/2015                                   *
      *                                                                *
      *    OBJETIVO     : VALIDAR ARQUIVO RECEPCIONADO PARA UPLOAD -   *
      *                   REGISTRO PRINCIPAL                           *
      *                                                                *
      *                           ARQUIVOS                             *
      *           +---------------------------------------+            *
      *           |  DDNAME  |   I/O   |   BOOK   | LRECL |            *
      *           |----------|---------|----------|-------|            *
      *           | ARQPRINC | INPUT   | I#SCSB90 | 1750  |            *
      *           | PRINCVAL | OUTPUT  | I#SCSB90 | 1750  |            *
      *           +---------------------------------------+            *
      *                                                                *
      *           +---------------------------------------+            *
      *           | TABELA                    |  INCLUDE  |            *
      *           |---------------------------|-----------|            *
      *           | DB2PRD.TPAIS_GEOGR_RECTA  | SCSBB011  |            *
      *           | DB2PRD.TREG_COMPR_RECTA   | SCSBB013  |            *
      *           | DB2PRD.TREG_VDA_RECTA     | SCSBB016  |            *
      *           | DB2PRD.TGSTAO_CORP_EXTER  | SCSBB026  |            *
      *           +---------------------------------------+            *
      *                                                                *
      *    MODULOS :                                                   *
      *                                                                *
      *    -> BRAD0160 (    -   ) - OBTER JOBNAME                      *
      *    -> BRAD0450 (    -   ) - ABENDAR PROGRAMA                   *
      *    -> BRAD1050 (    -   ) - CONEXAO DB2                        *
      *    -> CALE2000 (I#CALE01) - COMPONENTE CALENDARIO              *
      *    -> UORG2397 (UORGBW97) - CONVERTE EMPRESA SAP               *
      *    -> UORG2389 (INTERNO ) - CONVERTE DEPENDENCIA SAP           *
      *    -> FRWK2999 (I#FRWKGE) - GRAV. DE ERRO NO COMPONENTE GLOG   *
      *                (I#FRWKHE) - AREA COMUM DE ERROS                *
      *                (I#FRWKAR) - INFORMACOES DE ERRO -> ARQUIVO     *
      *                (I#FRWKDB) - INFORMACOES DE ERRO -> DB2         *
      *                (I#FRWKMD) - INFORMACOES DE ERRO -> MODULO      *
      *                                                                *
      *----------------------------------------------------------------*

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

           SELECT ARQPRINC ASSIGN      TO ARQPRINC
                      FILE STATUS      IS WRK-FS-ARQPRINC.

           SELECT PRINCVAL ASSIGN      TO PRINCVAL
                      FILE STATUS      IS WRK-FS-PRINCVAL.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * INPUT -                                                        *
      *                                                                *
      *                 ORG. SEQUENCIAL  -  LRECL 1750                 *
      *----------------------------------------------------------------*

       FD  ARQPRINC
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-ARQPRINC                 PIC X(1750).

      *----------------------------------------------------------------*
      * OUTPUT -                                                       *
      *                                                                *
      *                 ORG. SEQUENCIAL  -  LRECL 1750                 *
      *----------------------------------------------------------------*

       FD  PRINCVAL
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-PRINCVAL                 PIC X(1750).

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER PIC X(34) VALUE  '** INICIO DA WORKING SCSB0005 **  '.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER PIC X(34) VALUE  '** FILE-STATUS DOS ARQUIVOS **    '.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       01  WRK-FS-ARQPRINC             PIC  X(002)  VALUE SPACES.
       01  WRK-FS-PRINCVAL             PIC  X(002)  VALUE SPACES.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER PIC X(34) VALUE  '** BOOK DOS ARQUIVOS **           '.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

      *-->  ARQPRINC
       COPY 'I#SCSB90'.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER PIC X(34) VALUE  '** AREA DA BRAD0160 **            '.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       01  WRK-B0160-VALOR-FAC         PIC  9(005)  COMP-3 VALUE ZEROS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER PIC X(34) VALUE  '** AREA DA BRAD0450 **            '.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       01  WRK-B0450-ABEND             PIC S9(004)  COMP   VALUE +1111.
       01  WRK-B0450-DUMP              PIC  X(001)         VALUE 'S'.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER PIC X(34) VALUE  '** AREA DA API - CALE2000 **      '.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       COPY 'I#CALE01'.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER PIC X(34) VALUE  '** AREA DA API - FRWK2999 **      '.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       01  WRK-AREA-ERRO.
           COPY 'I#FRWKGE'.

           05  WRK-BLOCO-INFO-ERRO.
             10  WRK-CHAR-INFO-ERRO    PIC  X(001)
                                       OCCURS  0 TO 30000 TIMES
                                       DEPENDING ON FRWKGHEA-TAM-DADOS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER PIC X(34) VALUE  '** INFORMACOES DE ERRO - ARQ **   '.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       01  WRK-AREA-ERRO-ARQUIVO.
           COPY 'I#FRWKAR'.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER PIC X(34) VALUE  '** INFORMACOES DE ERRO - DB2 **   '.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       01  WRK-AREA-ERRO-DB2.
           COPY 'I#FRWKDB'.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER PIC X(34) VALUE  '** INFORMACOES DE ERRO - MODULO **'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       01  WRK-AREA-ERRO-MODULO.
           COPY 'I#FRWKMD'.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER PIC X(34) VALUE  '** AREA PARA ESTATISTICA **       '.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       COPY 'SCSBWB99'.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER PIC X(34) VALUE  '** AREA DA API - UORG2397 **      '.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       01  WRK-AREA-UORG2334.
           COPY    'UORGWB25'.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER PIC X(34) VALUE  '** AREA DA AGEO2000 **            '.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

           COPY    'AGEOWAAI'.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER PIC X(34) VALUE  '** AREA DA INEC2PPI **            '.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       01  WRK-AREA-INECWPHI.
           COPY    'INECWPHI'.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER PIC X(34) VALUE  '** ACUMULADORES **                '.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       01  ACU-LIDOS-ARQPRINC          PIC  9(009)  COMP-3 VALUE ZEROS.
       01  ACU-GRAVS-PRINCVAL          PIC  9(009)  COMP-3 VALUE ZEROS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER PIC X(34) VALUE  '** AREA DE CHAVES **              '.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       01  WRK-CHV-ATU.
           05  WRK-CHV-TPO-SERVC       PIC  9(001)  VALUE ZEROS.
           05  WRK-CHV-NREG            PIC  9(009)  VALUE ZEROS.

       01  WRK-CHV-ANT.
           05  WRK-CHV-TPO-SERVC-ANT   PIC  9(001)  VALUE ZEROS.
           05  WRK-CHV-NREG-ANT        PIC  9(009)  VALUE ZEROS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER PIC X(34) VALUE  '** VARIAVEIS AUXILIARES **        '.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       01  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       01  WRK-I                       PIC  9(003)  COMP-3 VALUE ZEROS.
       01  WRK-POSICAO                 PIC  9(009)  COMP-3 VALUE ZEROS.
       01  WRK-SEQ                     PIC  9(009)  COMP-3 VALUE ZEROS.
       01  WRK-ERRO-OCORRIDO           PIC  9(003)         VALUE ZEROS.
       01  WRK-SPACES-DUPLO            PIC  9(001)         VALUE ZEROS.

       01  WRK-STRING                  PIC X(1000)         VALUE SPACES.
       01  WRK-STRING-LEN              PIC 9(09)           VALUE ZEROS.
       01  WRK-CHAR                    PIC X(01)           VALUE SPACES.
       01  WRK-CHAR-ANT                PIC X(01)           VALUE SPACES.

       01 WRK-E-CD-PAIS-AGEO           PIC X(10)  VALUE SPACES.
       01 WRK-E-CD-PAIS-AGEO-R         REDEFINES WRK-E-CD-PAIS-AGEO.
          05 FILLER                    PIC X(07).
          05 WRK-CD-PAIS-AGEO-E        PIC 9(03).

      *--> "LITERAIS UTILIZADAS PELO PROGRAMA"
      *

       01  WRK-LIT-PGM                 PIC  X(008)  VALUE 'SCSB0005'.
       01  WRK-FRWK2999                PIC  X(008)  VALUE 'FRWK2999'.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER PIC X(34) VALUE  '** AREA DE INCLUDES PARA DB2 **   '.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

           EXEC SQL
              INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
              INCLUDE SCSBB011
           END-EXEC.

           EXEC SQL
              INCLUDE SCSBB013
           END-EXEC.

           EXEC SQL
              INCLUDE SCSBB016
           END-EXEC.

           EXEC SQL
              INCLUDE SCSBB026
           END-EXEC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER PIC X(34) VALUE  '** FIM DA WORKING SCSB0005 **     '.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
      * SECAO PRINCIPAL DE CONTROLE DO FLUXO DO PROGRAMA.              *
      *----------------------------------------------------------------*
       0000-PRINCIPAL                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR

           PERFORM 3000-PROCESSAR      UNTIL
                   WRK-FS-ARQPRINC     EQUAL  '10'

           PERFORM 9000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * PROCEDIMENTOS INICIAIS.                                        *
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           MOVE  '1000-INICIALIZAR'    TO  FRWKGHEA-IDEN-PARAGRAFO

           CALL  'CKRS1000'
           CALL  'CKRS1050'

           INITIALIZE  FRWKGERR-BLOCO-RETORNO
                       FRWKGHEA-REGISTRO
                       FRWKGARQ-REGISTRO
                       FRWKGDB2-REGISTRO
                       FRWKGMOD-REGISTRO

      *
      *--> "ABERTURA DOS ARQUIVOS"
      *

           SET  ARQ-OPEN               TO  TRUE

           OPEN INPUT  ARQPRINC
                OUTPUT PRINCVAL

           PERFORM 1100-TESTAR-FILE-STATUS

      *
      *--> "OBTER DATA E HORA DO SISTEMA"
      *

           PERFORM 1200-OBTER-DATA-HORA-SISTEMA

      *
      *--> "OBTER JOBNAME"
      *

           PERFORM 1300-OBTER-JOBNAME

      *
      *--> "PRIMEIRA LEITURA"
      *

           PERFORM 2000-VERIFICAR-VAZIO.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * FAZER TESTE DE FILE-STATUS PARA OS ARQUIVOS.                   *
      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           MOVE  '1100-TESTAR-FILE-STATUS'  TO  FRWKGHEA-IDEN-PARAGRAFO

           PERFORM 1110-TESTAR-FS-ARQPRINC
           PERFORM 1120-TESTAR-FS-PRINCVAL.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * TESTAR FILE-STATUS DO ARQUIVO DE ENTRADA ARQPRINC.             *
      *----------------------------------------------------------------*
       1110-TESTAR-FS-ARQPRINC         SECTION.
      *----------------------------------------------------------------*

           MOVE  '1110-TESTAR-FS-ARQPRINC'  TO  FRWKGHEA-IDEN-PARAGRAFO


           IF WRK-FS-ARQPRINC          NOT EQUAL '00'
              SET   ERRO-ARQUIVO       TO  TRUE
              MOVE  WRK-FS-ARQPRINC    TO  FRWKGARQ-FILE-STATUS
              MOVE  'ARQPRINC'         TO  FRWKGARQ-NOME-ARQUIVO
              PERFORM 9999-FINALIZAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * TESTAR FILE-STATUS DO ARQUIVO DE SAIDA PRINCVAL.               *
      *----------------------------------------------------------------*
       1120-TESTAR-FS-PRINCVAL         SECTION.
      *----------------------------------------------------------------*

           MOVE  '1120-TESTAR-FS-PRINCVAL'  TO  FRWKGHEA-IDEN-PARAGRAFO

           IF WRK-FS-PRINCVAL          NOT EQUAL '00'
              SET   ERRO-ARQUIVO       TO  TRUE
              MOVE  WRK-FS-PRINCVAL    TO  FRWKGARQ-FILE-STATUS
              MOVE  'PRINCVAL'         TO  FRWKGARQ-NOME-ARQUIVO
              PERFORM 9999-FINALIZAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * OBTER DATA E HORA DO SISTEMA ATRAVES DA API CALE2000.          *
      *----------------------------------------------------------------*
       1200-OBTER-DATA-HORA-SISTEMA    SECTION.
      *----------------------------------------------------------------*

           MOVE  '1200-OBTER-DATA-HORA-SISTEMA'
                                       TO  FRWKGHEA-IDEN-PARAGRAFO

           INITIALIZE  CALE01-ENTRADA

      *--  F3     = CONSULTA
      *--  SF3002 = FORNECER DATA E HORARIO ATUAL

           MOVE  'CALE2000'            TO  WRK-MODULO
           MOVE  'CALE0001'            TO  CALE01-ID-BLOCO
           MOVE  1182                  TO  CALE01-TAM-BLOCO
           MOVE  'F3'                  TO  CALE01-FUNCAO
           MOVE  'SF3002'              TO  CALE01-SUB-FUNCAO

           CALL  WRK-MODULO            USING  CALE01-REGISTRO

           IF CALE01-COD-RETORNO       NOT EQUAL ZEROS
              SET   ERRO-MODULO        TO  TRUE
              MOVE  WRK-MODULO         TO  FRWKGMOD-NOME-MODULO
              MOVE  CALE01-COD-RETORNO TO  FRWKGMOD-COD-RETORNO
              MOVE  CALE01-COD-ERRO    TO  FRWKGMOD-COD-ERRO
              MOVE  CALE01-COD-MENSAGEM-GMSG
                                       TO  FRWKGMOD-COD-MENSAGEM
              PERFORM 9999-FINALIZAR-ERRO
           END-IF

           MOVE  CALE01-DT-GREGO-P-DDMMAAAA
                                       TO  WRK-DATAPROC
                                           WRK-DATAMOV
           MOVE  CALE01-HH-2PONTOS-HHMMSS
                                       TO  WRK-HORAPROC.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * OBTER JOBNAME ATRAVES DA BRAD0160.                             *
      *----------------------------------------------------------------*
       1300-OBTER-JOBNAME              SECTION.
      *----------------------------------------------------------------*

           MOVE  '1300-OBTER-JOBNAME'  TO  FRWKGHEA-IDEN-PARAGRAFO

           MOVE   SPACES               TO     WRK-JOBNAME

           CALL  'BRAD0160'            USING  WRK-JOBNAME
                                              WRK-B0160-VALOR-FAC.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * VERIFICAR SE ARQUIVOS DE ENTRADA ESTAO VAZIOS.                 *
      *----------------------------------------------------------------*
       2000-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           MOVE  '2000-VERIFICAR-VAZIO'  TO  FRWKGHEA-IDEN-PARAGRAFO

           PERFORM 2100-LER-ARQPRINC

           IF WRK-FS-ARQPRINC          EQUAL '10'
              DISPLAY '***================================***'
              DISPLAY '*              SCSB0005              *'
              DISPLAY '*------------------------------------*'
              DISPLAY '*                                    *'
              DISPLAY '*     ARQUIVO ARQPRINC VAZIO !!!     *'
              DISPLAY '*                                    *'
              DISPLAY '***================================***'
              SET   ERRO-ARQUIVO       TO  TRUE
              MOVE  WRK-FS-PRINCVAL    TO  FRWKGARQ-FILE-STATUS
              MOVE  'ARQPRINC'         TO  FRWKGARQ-NOME-ARQUIVO
              PERFORM 9000-FINALIZAR
           END-IF.

           MOVE  LOW-VALUES              TO  WRK-CHV-ANT.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * FAZER A LEITURA DO ARQUIVO ARQPRINC.                           *
      *----------------------------------------------------------------*
       2100-LER-ARQPRINC               SECTION.
      *----------------------------------------------------------------*

           MOVE  '2100-LER-ARQPRINC'   TO  FRWKGHEA-IDEN-PARAGRAFO

           MOVE  WRK-CHV-ATU           TO  WRK-CHV-ANT

           SET  ARQ-READ               TO   TRUE
           READ ARQPRINC               INTO SCSB90-REG

           IF WRK-FS-ARQPRINC          NOT EQUAL '10'
              PERFORM 1110-TESTAR-FS-ARQPRINC
              ADD   1                  TO  ACU-LIDOS-ARQPRINC
              ADD   1                  TO  WRK-SEQ
              MOVE  ZEROS              TO  WRK-I
              MOVE SCSB90-TPO-SERVC    TO  WRK-CHV-TPO-SERVC
              MOVE SCSB90-NREG         TO  WRK-CHV-NREG
           END-IF.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * PROCESSAMENTO PRINCIPAL.                                       *
      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           MOVE  '3000-PROCESSAR'  TO  FRWKGHEA-IDEN-PARAGRAFO

           PERFORM 3100-VALIDAR-DADOS.

           PERFORM 3600-GRAVAR-PRINCVAL.

           PERFORM 2100-LER-ARQPRINC.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * CARREGA REGISTRO DETALHE.                                      *
      *----------------------------------------------------------------*
       3100-VALIDAR-DADOS              SECTION.
      *----------------------------------------------------------------*

           MOVE  '3100-VALIDAR-DADOS'  TO  FRWKGHEA-IDEN-PARAGRAFO

           PERFORM 3110-VALIDAR-TPO-SERVC.

           PERFORM 3120-VALIDAR-NREG.

           PERFORM 3130-VALIDAR-CNPJ.

           PERFORM 3140-VALIDAR-NOME.

           PERFORM 3150-VALIDAR-ENDERECO.

           PERFORM 3160-VALIDAR-PAIS.

           PERFORM 3170-VALIDAR-MOEDA.

           PERFORM 3180-VALIDAR-VALOR-CONVERTIDO.

           PERFORM 3190-VALIDAR-INF-COMPLEMENTAR.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3101-ADICIONA-ERRO              SECTION.
      *----------------------------------------------------------------*

           MOVE  '3101-ADICIONA-ERRO'  TO  FRWKGHEA-IDEN-PARAGRAFO

           ADD 1                       TO  WRK-I.
           MOVE WRK-ERRO-OCORRIDO      TO  SCSB90-COD-ERRO(WRK-I).

      *----------------------------------------------------------------*
       3101-FIM.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3110-VALIDAR-TPO-SERVC          SECTION.
      *----------------------------------------------------------------*

           MOVE  '3110-VALIDAR-TPO-SERVC'   TO  FRWKGHEA-IDEN-PARAGRAFO

           IF  SCSB90-TPO-SERVC NOT NUMERIC OR
               (SCSB90-TPO-SERVC NOT EQUAL 1 AND
                SCSB90-TPO-SERVC NOT EQUAL 2)
               MOVE 001                TO  WRK-ERRO-OCORRIDO
               PERFORM 3101-ADICIONA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       3110-FIM.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3120-VALIDAR-NREG               SECTION.
      *----------------------------------------------------------------*

           MOVE  '3120-VALIDAR-NREG'   TO  FRWKGHEA-IDEN-PARAGRAFO.

           IF  SCSB90-NREG NOT NUMERIC
               MOVE 002                TO  WRK-ERRO-OCORRIDO
               PERFORM 3101-ADICIONA-ERRO
           ELSE
               IF SCSB90-NREG EQUAL ZEROS
                   MOVE 003            TO  WRK-ERRO-OCORRIDO
                   PERFORM 3101-ADICIONA-ERRO
               ELSE
                   IF WRK-CHV-ATU  EQUAL WRK-CHV-ANT
                       MOVE 005            TO  WRK-ERRO-OCORRIDO
                       PERFORM 3101-ADICIONA-ERRO
                   END-IF
                   PERFORM 3121-VERIFICA-SERV-CADASTRADO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       3120-FIM.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3121-VERIFICA-SERV-CADASTRADO   SECTION.
      *----------------------------------------------------------------*

           MOVE  '3121-VERIFICA-SERV-CADASTRADO'
                                   TO  FRWKGHEA-IDEN-PARAGRAFO

           IF  SCSB90-TPO-SERVC EQUAL 1

               MOVE SCSB90-NPROCS      TO  DANO-VDA-RECTA OF SCSBB016
               MOVE SCSB90-NREG        TO  NREG-VDA-RECTA OF SCSBB016

               EXEC SQL
                   SELECT
                           CPSSOA_UND_ORGNZ
                   INTO
                           :SCSBB016.CPSSOA-UND-ORGNZ
                   FROM  DB2PRD.TREG_VDA_RECTA
                   WHERE DANO_VDA_RECTA   = :SCSBB016.DANO-VDA-RECTA
                    AND  NREG_VDA_RECTA   = :SCSBB016.NREG-VDA-RECTA
               END-EXEC

               IF (SQLCODE     NOT EQUAL ZEROS AND
                   SQLCODE     NOT EQUAL +100) OR
                   (SQLWARN0                    EQUAL 'W')
                   SET     ERRO-DB2            TO  TRUE
                   MOVE   'TREG_VDA_RECTA'     TO  FRWKGDB2-NOME-TABELA
                   SET     DB2-SELECT          TO  TRUE
                   MOVE   '1'                  TO  FRWKGDB2-LOCAL
                   PERFORM 9999-FINALIZAR-ERRO
               END-IF

               IF (SQLCODE                  EQUAL ZEROS)
                   MOVE 006            TO  WRK-ERRO-OCORRIDO
                   PERFORM 3101-ADICIONA-ERRO
               END-IF

           END-IF.

           IF  SCSB90-TPO-SERVC EQUAL 2

               MOVE SCSB90-NPROCS      TO  DANO-COMPR-RECTA OF SCSBB013
               MOVE SCSB90-NREG        TO  NREG-COMPR-RECTA OF SCSBB013

               EXEC SQL
                   SELECT
                           CPSSOA_UND_ORGNZ
                   INTO
                           :SCSBB013.CPSSOA-UND-ORGNZ
                   FROM  DB2PRD.TREG_COMPR_RECTA
                   WHERE DANO_COMPR_RECTA   = :SCSBB013.DANO-COMPR-RECTA
                    AND  NREG_COMPR_RECTA   = :SCSBB013.NREG-COMPR-RECTA
               END-EXEC

               IF (SQLCODE     NOT EQUAL ZEROS AND
                   SQLCODE     NOT EQUAL +100) OR
                   (SQLWARN0                    EQUAL 'W')
                   SET     ERRO-DB2            TO  TRUE
                   MOVE   'TREG_COMPR_RECTA'   TO  FRWKGDB2-NOME-TABELA
                   SET     DB2-SELECT          TO  TRUE
                   MOVE   '2'                  TO  FRWKGDB2-LOCAL
                   PERFORM 9999-FINALIZAR-ERRO
               END-IF

               IF (SQLCODE                  EQUAL ZEROS)
                   MOVE 006            TO  WRK-ERRO-OCORRIDO
                   PERFORM 3101-ADICIONA-ERRO
               END-IF

           END-IF.

      *----------------------------------------------------------------*
       3121-FIM.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3130-VALIDAR-CNPJ               SECTION.
      *----------------------------------------------------------------*

           MOVE  '3130-VALIDAR-CNPJ'   TO  FRWKGHEA-IDEN-PARAGRAFO

           IF  SCSB90-CNPJ-PRINC NOT NUMERIC OR
               SCSB90-CNPJ-FLIAL NOT NUMERIC OR
               SCSB90-CNPJ-CTRL  NOT NUMERIC

               MOVE 002            TO  WRK-ERRO-OCORRIDO
               PERFORM 3101-ADICIONA-ERRO

           ELSE
               IF  SCSB90-CNPJ-PRINC EQUAL ZEROS
                   MOVE 002            TO  WRK-ERRO-OCORRIDO
                   PERFORM 3101-ADICIONA-ERRO
               ELSE
                   PERFORM 3131-VALIDAR-EMPRESA
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       3130-FIM.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3131-VALIDAR-EMPRESA            SECTION.
      *----------------------------------------------------------------*

           MOVE  '3131-VALIDAR-EMPRESA'   TO  FRWKGHEA-IDEN-PARAGRAFO

           PERFORM 7000-CALL-UORG2334.

           IF      UORGWB25-COD-RETORNO  EQUAL 08
                   MOVE 006            TO  WRK-ERRO-OCORRIDO
                   PERFORM 3101-ADICIONA-ERRO
           ELSE
                   IF UORGWB25-S-CEMPR NOT EQUAL SCSB90-EMPR-DEPEND
                       PERFORM 3132-VALIDAR-EMP-GESTOR-CORPOR
                   END-IF
           END-IF.


      *----------------------------------------------------------------*
       3131-FIM.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3132-VALIDAR-EMP-GESTOR-CORPOR SECTION.
      *----------------------------------------------------------------*

           MOVE  '3132-VALIDAR-EMP-GESTOR-CORPOR'
                                       TO  FRWKGHEA-IDEN-PARAGRAFO

               MOVE SCSB90-EMPR-DEPEND TO CPSSOA-UND-ORGNZ OF SCSBB026
               MOVE SCSB90-DEPEND      TO NSEQ-UND-ORGNZ OF SCSBB026

               EXEC SQL
                   SELECT
                           CINDCD_EMPR_MSTER
                   INTO
                           :SCSBB026.CINDCD-EMPR-MSTER
                   FROM  DB2PRD.TGSTAO_CORP_EXTER
                   WHERE CPSSOA_UND_ORGNZ   = :SCSBB026.CPSSOA-UND-ORGNZ
                    AND   NSEQ_UND_ORGNZ    = :SCSBB026.NSEQ-UND-ORGNZ
                    AND   DBLOQ_EMPR_EXTER  IS NULL
               END-EXEC

               IF (SQLCODE     NOT EQUAL ZEROS AND
                   SQLCODE EQUAL +100) OR
                   (SQLWARN0                    EQUAL 'W')
                   SET     ERRO-DB2            TO  TRUE
                   MOVE   'TGSTAO_CORP_EXTER'  TO  FRWKGDB2-NOME-TABELA
                   SET     DB2-SELECT          TO  TRUE
                   MOVE   '3'                  TO  FRWKGDB2-LOCAL
                   PERFORM 9999-FINALIZAR-ERRO
               END-IF

               IF (SQLCODE                 EQUAL +100)
                   MOVE 009            TO  WRK-ERRO-OCORRIDO
                   PERFORM 3101-ADICIONA-ERRO
               ELSE
                   IF CINDCD-EMPR-MSTER OF SCSBB026 EQUAL ZERO

                       MOVE SCSB90-EMPR-DEPEND TO
                                           CPSSOA-UND-ORGNZ OF SCSBB026
                       MOVE SCSB90-DEPEND      TO
                                           NSEQ-UND-ORGNZ  OF SCSBB026
                       MOVE UORGWB25-S-CEMPR   TO
                                           CPSSOA-JURID    OF SCSBB026
                       EXEC SQL
                           SELECT
                               CINDCD_EMPR_MSTER
                           INTO
                               :SCSBB026.CINDCD-EMPR-MSTER
                           FROM  DB2PRD.TGSTAO_CORP_EXTER
                           WHERE CPSSOA_UND_ORGNZ   =
                                           :SCSBB026.CPSSOA-UND-ORGNZ
                            AND   NSEQ_UND_ORGNZ
                                           = :SCSBB026.NSEQ-UND-ORGNZ
                            AND   CPSSOA_JURID
                                           = :SCSBB026.CPSSOA-JURID
                            AND   DBLOQ_EMPR_EXTER  IS NULL
                       END-EXEC

                       IF (SQLCODE     NOT EQUAL ZEROS AND
                           SQLCODE EQUAL +100) OR
                           (SQLWARN0                    EQUAL 'W')
                           SET     ERRO-DB2        TO  TRUE
                           MOVE   'TGSTAO_CORP_EXTER'  TO
                                                   FRWKGDB2-NOME-TABELA
                           SET     DB2-SELECT      TO  TRUE
                           MOVE   '3'
                                                   TO  FRWKGDB2-LOCAL
                           PERFORM 9999-FINALIZAR-ERRO
                       END-IF

                       IF (SQLCODE                 EQUAL +100)
                           MOVE 004            TO  WRK-ERRO-OCORRIDO
                           PERFORM 3101-ADICIONA-ERRO
                       END-IF
                  END-IF
               END-IF.

      *----------------------------------------------------------------*
       3132-FIM.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3140-VALIDAR-NOME               SECTION.
      *----------------------------------------------------------------*

           MOVE  '3140-VALIDAR-NOME'   TO  FRWKGHEA-IDEN-PARAGRAFO

           IF  SCSB90-NOME EQUAL SPACES
               MOVE 003                TO  WRK-ERRO-OCORRIDO
               PERFORM 3101-ADICIONA-ERRO

           ELSE
               MOVE SCSB90-NOME        TO WRK-STRING
               MOVE 150                TO WRK-STRING-LEN
               PERFORM 8000-VALIDAR-CHAR-INVALIDOS
           END-IF.

      *----------------------------------------------------------------*
       3140-FIM.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3150-VALIDAR-ENDERECO           SECTION.
      *----------------------------------------------------------------*

           MOVE  '3150-VALIDAR-ENDERECO'   TO  FRWKGHEA-IDEN-PARAGRAFO

           IF  SCSB90-ENDERECO EQUAL SPACES
               MOVE 003                TO  WRK-ERRO-OCORRIDO
               PERFORM 3101-ADICIONA-ERRO

           ELSE
               MOVE SCSB90-ENDERECO    TO WRK-STRING
               MOVE 150                TO WRK-STRING-LEN
               PERFORM 8000-VALIDAR-CHAR-INVALIDOS
           END-IF.

      *----------------------------------------------------------------*
       3150-FIM.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3160-VALIDAR-PAIS               SECTION.
      *----------------------------------------------------------------*

           MOVE  '3160-VALIDAR-PAIS'   TO  FRWKGHEA-IDEN-PARAGRAFO

           IF SCSB90-CD-PAIS-RF NOT NUMERIC OR
              SCSB90-CD-PAIS-RF EQUAL ZEROS
               MOVE 010                TO  WRK-ERRO-OCORRIDO
               PERFORM 3101-ADICIONA-ERRO
           ELSE
               PERFORM 7200-CALL-AGEO200
           END-IF.

      *----------------------------------------------------------------*
       3160-FIM.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3170-VALIDAR-MOEDA              SECTION.
      *----------------------------------------------------------------*

           MOVE  '3170-VALIDAR-MOEDA'  TO  FRWKGHEA-IDEN-PARAGRAFO

           IF SCSB90-CD-MOEDA NOT NUMERIC OR
              SCSB90-CD-MOEDA EQUAL ZEROS
               MOVE 011                TO  WRK-ERRO-OCORRIDO
               PERFORM 3101-ADICIONA-ERRO
           ELSE
               PERFORM 7100-CALL-INEC2PPI
               IF INECWPHI-COD-RETORNO EQUAL 08
                   MOVE 011                TO  WRK-ERRO-OCORRIDO
                   PERFORM 3101-ADICIONA-ERRO
               END-IF

           END-IF.

      *----------------------------------------------------------------*
       3170-FIM.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3180-VALIDAR-VALOR-CONVERTIDO   SECTION.
      *----------------------------------------------------------------*

           MOVE  '3180-VALIDAR-VALOR-CONVERTIDO'
                                       TO  FRWKGHEA-IDEN-PARAGRAFO

           IF SCSB90-VLR-CONVERT NOT NUMERIC OR
              SCSB90-VLR-CONVERT EQUAL ZEROS
               MOVE 012                TO  WRK-ERRO-OCORRIDO
               PERFORM 3101-ADICIONA-ERRO
           END-IF.
      *----------------------------------------------------------------*
       3180-FIM.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3190-VALIDAR-INF-COMPLEMENTAR   SECTION.
      *----------------------------------------------------------------*

           MOVE  '3190-VALIDAR-INF-COMPLEMENTAR'
                                       TO  FRWKGHEA-IDEN-PARAGRAFO

           IF  SCSB90-INF-COMPLEM NOT EQUAL SPACES
               MOVE SCSB90-INF-COMPLEM TO WRK-STRING
               MOVE 1000               TO WRK-STRING-LEN
               PERFORM 8000-VALIDAR-CHAR-INVALIDOS
           END-IF.

      *----------------------------------------------------------------*
       3190-FIM.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * GRAVAR ARQUIVO DE SAIDA PRINCVAL.                              *
      *----------------------------------------------------------------*
       3600-GRAVAR-PRINCVAL            SECTION.
      *----------------------------------------------------------------*

           MOVE '3600-GRAVAR-PRINCVAL' TO  FRWKGHEA-IDEN-PARAGRAFO

           SET   ARQ-WRITE             TO  TRUE

           WRITE FD-PRINCVAL           FROM SCSB90-REG

           PERFORM 1120-TESTAR-FS-PRINCVAL

           ADD   1                     TO  ACU-GRAVS-PRINCVAL.

      *----------------------------------------------------------------*
       3600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * VALIDA CNPJ                                                    *
      *----------------------------------------------------------------*
       7000-CALL-UORG2334              SECTION.
      *----------------------------------------------------------------*

           MOVE  '7000-CALL-UORG2334'      TO
                                           FRWKGHEA-IDEN-PARAGRAFO.

           INITIALIZE  UORGWB25-REGISTRO.

           MOVE    SCSB90-CNPJ-PRINC       TO
                                           UORGWB25-E-CNPJ-CORPO.
           MOVE    'UORG2334'              TO      WRK-MODULO

           CALL    WRK-MODULO              USING   WRK-AREA-UORG2334

           IF      UORGWB25-COD-RETORNO    NOT EQUAL ZEROS AND 08
                   SET  ERRO-MODULO        TO      TRUE
                   MOVE WRK-MODULO         TO      FRWKGMOD-NOME-MODULO
                   MOVE UORGWB25-COD-RETORNO
                                           TO      FRWKGMOD-COD-RETORNO
                   PERFORM 9999-FINALIZAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       7000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       7100-CALL-INEC2PPI              SECTION.
      *----------------------------------------------------------------*

           MOVE  '7100-CALL-INEC2PPI'     TO
                                           FRWKGHEA-IDEN-PARAGRAFO.

           INITIALIZE  WRK-AREA-INECWPHI.

           MOVE    SCSB90-CD-MOEDA         TO     INECWPHI-CINDCD-ECONM.

           MOVE    'INEC2PPI'              TO      WRK-MODULO.

           CALL    WRK-MODULO              USING   WRK-AREA-INECWPHI.

           IF      INECWPHI-COD-RETORNO  NOT EQUAL ZEROS AND 08
                   SET  ERRO-MODULO        TO      TRUE
                   MOVE WRK-MODULO         TO      FRWKGMOD-NOME-MODULO
                   MOVE INECWPHI-COD-RETORNO
                                           TO      FRWKGMOD-COD-RETORNO
                   PERFORM 9999-FINALIZAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       7100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       7200-CALL-AGEO200               SECTION.
      *----------------------------------------------------------------*

           MOVE  '7200-CALL-AGEO200'   TO  FRWKGHEA-IDEN-PARAGRAFO.

           MOVE    'AGEO2000'              TO      WRK-MODULO.

           MOVE 'SF1007'                   TO AGEOWAAI-FUNCAO.
           MOVE 001                        TO AGEOWAAI-VERSAO.
           MOVE 'T'                        TO AGEOWAAI-TIP-TERRITORIO.
           MOVE 'C'                        TO AGEOWAAI-IND-CLAS-LIST.
           MOVE SCSB90-CD-PAIS-RF          TO WRK-CD-PAIS-AGEO-E
           MOVE WRK-E-CD-PAIS-AGEO         TO AGEOWAAI-COD-PAIS-E.
           MOVE 61                         TO AGEOWAAI-CIRECP-SAIDA.
           MOVE 001                        TO AGEOWAAI-IDIOMA-SAIDA.
           MOVE 01                         TO AGEOWAAI-QTDE-SOLIC.
           MOVE 'I'                        TO AGEOWAAI-IND-PAGINA.

           CALL    WRK-MODULO              USING   AGEOWAAI.

           IF AGEOWAAI-COD-RET       NOT EQUAL ZEROS AND 01 AND 08
              PERFORM 9999-FINALIZAR-ERRO
           END-IF.

           IF AGEOWAAI-COD-RET       EQUAL 08
              PERFORM 7300-SELECT-SCSBB011
           END-IF.

      *----------------------------------------------------------------*
       7200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       7300-SELECT-SCSBB011               SECTION.
      *----------------------------------------------------------------*

           MOVE '7300-SELECT-SCSBB011'    TO FRWKGHEA-IDEN-PARAGRAFO

           MOVE WRK-CD-PAIS-AGEO-E      TO CPAIS-RECTA-FEDRL OF SCSBB011

           EXEC SQL
                SELECT CPAIS
              INTO
               :SCSBB011.CPAIS
              FROM DB2PRD.TPAIS_GEOGR_RECTA
              WHERE CPAIS_RECTA_FEDRL  = :SCSBB011.CPAIS-RECTA-FEDRL
           END-EXEC.

           IF
               (SQLCODE        NOT     EQUAL   ZEROS AND +100) OR
               (SQLWARN0               EQUAL   'W')
                SET ERRO-DB2             TO      TRUE
                MOVE 'TPAIS_GEOGR_RECTA' TO      FRWKGDB2-NOME-TABELA
                PERFORM 9999-FINALIZAR-ERRO
           END-IF.

           IF (SQLCODE                 EQUAL +100)
               MOVE 010            TO  WRK-ERRO-OCORRIDO
               PERFORM 3101-ADICIONA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       7300-99-FIM.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       8000-VALIDAR-CHAR-INVALIDOS     SECTION.
      *----------------------------------------------------------------*

           MOVE '8000-VALIDAR-CHAR-INVALIDOS'
                                       TO FRWKGHEA-IDEN-PARAGRAFO

           MOVE ZEROS                  TO WRK-SPACES-DUPLO
           MOVE SPACES                 TO WRK-CHAR-ANT
           PERFORM VARYING WRK-POSICAO FROM 1 BY 1
                     UNTIL WRK-POSICAO GREATER WRK-STRING-LEN

                     MOVE WRK-STRING(WRK-POSICAO:1) TO WRK-CHAR
                     IF(WRK-CHAR EQUAL '!' OR WRK-CHAR EQUAL '@'
                      OR  WRK-CHAR EQUAL '#' OR  WRK-CHAR EQUAL '$'
                      OR  WRK-CHAR EQUAL '%' OR  WRK-CHAR EQUAL '*'
                      OR  WRK-CHAR EQUAL '(' OR  WRK-CHAR EQUAL ')'
                      OR  WRK-CHAR EQUAL '_' OR  WRK-CHAR EQUAL '='
                      OR  WRK-CHAR EQUAL '[' OR  WRK-CHAR EQUAL ']'
                      OR  WRK-CHAR EQUAL '{' OR  WRK-CHAR EQUAL '}'
                      OR  WRK-CHAR EQUAL '|' OR  WRK-CHAR EQUAL '\'
                      OR  WRK-CHAR EQUAL ':' OR  WRK-CHAR EQUAL '?'
                      OR  WRK-CHAR EQUAL '/' OR  WRK-CHAR EQUAL '+')
                       MOVE 007            TO  WRK-ERRO-OCORRIDO
                       PERFORM 3101-ADICIONA-ERRO
                       GO TO 8000-FIM
                     END-IF

                     IF WRK-CHAR-ANT EQUAL SPACES AND
                        WRK-CHAR EQUAL SPACES
                        MOVE 1                 TO WRK-SPACES-DUPLO
                     END-IF

                     IF WRK-SPACES-DUPLO NOT EQUAL ZEROS AND
                        WRK-CHAR         NOT EQUAL SPACES
                       MOVE 007            TO  WRK-ERRO-OCORRIDO
                       PERFORM 3101-ADICIONA-ERRO
                       GO TO 8000-FIM
                     END-IF

                     MOVE WRK-CHAR         TO WRK-CHAR-ANT

           END-PERFORM.

      *----------------------------------------------------------------*
       8000-FIM.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * PROCEDIMENTOS FINAIS DO PROGRAMA.                              *
      *----------------------------------------------------------------*
       9000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           MOVE  '9000-FINALIZAR'      TO  FRWKGHEA-IDEN-PARAGRAFO

      *
      *--> "APRESENTAR O RESUMO DA EXECUCAO"
      *
           PERFORM 9100-EXIBIR-ESTATISTICA

      *
      *--> "FECHAR ARQUIVOS"
      *

           SET  ARQ-CLOSE              TO  TRUE

           CLOSE  ARQPRINC
                  PRINCVAL

           PERFORM 1100-TESTAR-FILE-STATUS

           STOP RUN.

      *----------------------------------------------------------------*
       9000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * APRESENTAR O RESUMO DA EXECUCAO.                               *
      *----------------------------------------------------------------*
       9100-EXIBIR-ESTATISTICA         SECTION.
      *----------------------------------------------------------------*

           MOVE  '9100-EXIBIR-ESTATISTICA'
                                       TO  FRWKGHEA-IDEN-PARAGRAFO

           DISPLAY WRK-DISP1
           DISPLAY WRK-DISP2
           DISPLAY WRK-DISP3
           DISPLAY WRK-DISP4
           DISPLAY WRK-DISP5

           MOVE    WRK-LIT-PGM                   TO  WRK-PROGNAME

           DISPLAY WRK-DISP6
           DISPLAY WRK-DISP3
           DISPLAY WRK-DISP7
           DISPLAY WRK-DISP8

           MOVE   'ARQPRINC'                     TO  WRK-DDNAME
           MOVE   'I'                            TO  WRK-I-O
           MOVE   'TOTAL LIDOS                '  TO  WRK-DESCARQ
           MOVE    ACU-LIDOS-ARQPRINC            TO  WRK-QTDEARQ
           DISPLAY WRK-DISP9

           MOVE   'PRINCVAL'                     TO  WRK-DDNAME
           MOVE   'O'                            TO  WRK-I-O
           MOVE   'TOTAL GRAVADOS             '  TO  WRK-DESCARQ
           MOVE    ACU-GRAVS-PRINCVAL            TO  WRK-QTDEARQ
           DISPLAY WRK-DISP9

           DISPLAY WRK-DISP1.

      *----------------------------------------------------------------*
       9100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * FINALIZAR PROGRAMA COM ABEND.                                  *
      *----------------------------------------------------------------*
       9999-FINALIZAR-ERRO             SECTION.
      *----------------------------------------------------------------*

           MOVE  12                    TO  FRWKGERR-COD-RETORNO
           MOVE  WRK-LIT-PGM           TO  FRWKGHEA-NOME-PROGRAMA

           EVALUATE FRWKGHEA-TIPO-ERRO

             WHEN  'AR'
                SET   FRWKGERR-FSTATUS-INCOR    TO  TRUE
                MOVE  FRWKGARQ-TAM-LAYOUT       TO  FRWKGHEA-TAM-DADOS
                MOVE  WRK-AREA-ERRO-ARQUIVO     TO  WRK-BLOCO-INFO-ERRO

             WHEN  'DB'
                MOVE  SQLSTATE                  TO  FRWKGDB2-SQLSTATE
                MOVE  SQLCODE                   TO  FRWKGDB2-SQLCODE
                SET   FRWKGERR-SQLCODE-INCOR    TO  TRUE
                MOVE  FRWKGDB2-TAM-LAYOUT       TO  FRWKGHEA-TAM-DADOS
                MOVE  WRK-AREA-ERRO-DB2         TO  WRK-BLOCO-INFO-ERRO

             WHEN  'MO'
                SET   FRWKGERR-COD-RETOR-INCOR  TO  TRUE
                MOVE  FRWKGMOD-TAM-LAYOUT       TO  FRWKGHEA-TAM-DADOS
                MOVE  WRK-AREA-ERRO-MODULO      TO  WRK-BLOCO-INFO-ERRO

           END-EVALUATE

           CALL  WRK-FRWK2999          USING  WRK-AREA-ERRO

           CALL  'BRAD0450'            USING  WRK-B0450-ABEND
                                              WRK-B0450-DUMP.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*


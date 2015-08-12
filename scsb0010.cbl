      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. SCSB0010.
       AUTHOR.     FRANCISCO FREIRE.

      *================================================================*
      *               B R Q   -   I T   S E R V I C E S                *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA     : SCSB0010                                     *
      *    ANALISTA     : FERNANDA CARUSO        - BRQ IT SERVICES.    *
      *    PROGRAMADOR  : FRANCISCO FREIRE       - BRQ IT SERVICES.    *
      *    DATA         : 04/08/2015                                   *
      *                                                                *
      *    OBJETIVO     : VALIDAR ARQUIVO RECEPCIONADO PARA UPLOAD -   *
      *                   REGISTRO OPERACAO                            *
      *                                                                *
      *                           ARQUIVOS                             *
      *           +---------------------------------------+            *
      *           |  DDNAME  |   I/O   |   BOOK   | LRECL |            *
      *           |----------|---------|----------|-------|            *
      *           | ARQOPER  | INPUT   | I#SCSB91 | 0400  |            *
      *           | ARQPRINC | INPUT   | I#SCSB90 | 1750  |            *
      *           | OPERVALD | OUTPUT  | I#SCSB91 | 0400  |            *
      *           | PRINCVAL | OUTPUT  | I#SCSB90 | 1750  |            *
      *           +---------------------------------------+            *
      *                                                                *
      *           +---------------------------------------+            *
      *           | TABELA                    |  INCLUDE  |            *
      *           |---------------------------|-----------|            *
      *           | DB2PRD.TPAIS_GEOGR_RECTA  | SCSBB011  |            *
      *           | DB2PRD.TSERVC_VAR_PATRM   | SCSBB017  |            *
      *           +---------------------------------------+            *
      *                                                                *
      *    MODULOS :                                                   *
      *                                                                *
      *    -> BRAD0160 (    -   ) - OBTER JOBNAME                      *
      *    -> BRAD0450 (    -   ) - ABENDAR PROGRAMA                   *
      *    -> BRAD1050 (    -   ) - CONEXAO DB2                        *
      *    -> CALE2000 (I#CALE01) - COMPONENTE CALENDARIO              *
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

           SELECT ARQOPER  ASSIGN      TO ARQOPER
                      FILE STATUS      IS WRK-FS-ARQOPER.

           SELECT ARQPRINC ASSIGN      TO ARQPRINC
                      FILE STATUS      IS WRK-FS-ARQPRINC.

           SELECT OPERVALD ASSIGN      TO OPERVALD
                      FILE STATUS      IS WRK-FS-OPERVALD.

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
      *                 ORG. SEQUENCIAL  -  LRECL 0400                 *
      *----------------------------------------------------------------*

       FD  ARQOPER
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-ARQOPER                  PIC X(0400).

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
      * INPUT -                                                        *
      *                                                                *
      *                 ORG. SEQUENCIAL  -  LRECL 0400                 *
      *----------------------------------------------------------------*

       FD  OPERVALD
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-OPERVALD                 PIC X(0400).

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
       01  FILLER PIC X(34) VALUE  '** INICIO DA WORKING SCSB0010 **  '.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER PIC X(34) VALUE  '** FILE-STATUS DOS ARQUIVOS **    '.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       01  WRK-FS-ARQOPER              PIC  X(002)  VALUE SPACES.
       01  WRK-FS-ARQPRINC             PIC  X(002)  VALUE SPACES.
       01  WRK-FS-OPERVALD             PIC  X(002)  VALUE SPACES.
       01  WRK-FS-PRINCVAL             PIC  X(002)  VALUE SPACES.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER PIC X(34) VALUE  '** BOOK DOS ARQUIVOS **           '.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

      *-->  ARQOPER
       COPY 'I#SCSB91'.

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
       01  FILLER PIC X(34) VALUE  '** AREA DA AGEO2000 **            '.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

           COPY    'AGEOWAAI'.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER PIC X(34) VALUE  '** ACUMULADORES **                '.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       01  ACU-LIDOS-ARQOPER           PIC  9(009)  COMP-3 VALUE ZEROS.
       01  ACU-LIDOS-ARQPRINC          PIC  9(009)  COMP-3 VALUE ZEROS.
       01  ACU-GRAVS-OPERVALD          PIC  9(009)  COMP-3 VALUE ZEROS.
       01  ACU-GRAVS-PRINCVAL          PIC  9(009)  COMP-3 VALUE ZEROS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER PIC X(34) VALUE  '** AREA DE CHAVES **              '.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       01  WRK-CHV-ARQPRINC-ATU.
           05  WRK-CHV-TPO-SERVC       PIC  9(001)  VALUE ZEROS.
           05  WRK-CHV-NREG            PIC  9(009)  VALUE ZEROS.

       01  WRK-CHV-ARQPRINC-ANT.
           05  WRK-CHV-TPO-SERVC-ANT   PIC  9(001)  VALUE ZEROS.
           05  WRK-CHV-NREG-ANT        PIC  9(009)  VALUE ZEROS.

       01  WRK-CHV-ARQOPER-ATU-COMP.
           05 WRK-CHV-ARQOPER-ATU.
               07  WRK-CHV-TPO-SERVC       PIC  9(001)  VALUE ZEROS.
               07  WRK-CHV-NREG            PIC  9(009)  VALUE ZEROS.
           05 WRK-CHV-ARQOPER-CD-OPER      PIC  9(010)  VALUE ZEROS.

       01  WRK-CHV-ARQOPER-ANT-COMP.
           05 WRK-CHV-ARQOPER-ANT.
               07  WRK-CHV-TPO-SERVC-ANT   PIC  9(001)  VALUE ZEROS.
               07  WRK-CHV-NREG-ANT        PIC  9(009)  VALUE ZEROS.
           05 WRK-CHV-ARQOPER-CD-OPER-ANT  PIC  9(010)  VALUE ZEROS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER PIC X(34) VALUE  '** VARIAVEIS AUXILIARES **        '.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       01  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       01  WRK-I-ARQPRINC              PIC  9(003)  COMP-3 VALUE ZEROS.
       01  WRK-I-ARQOPER               PIC  9(003)  COMP-3 VALUE ZEROS.
       01  WRK-ERRO-OCORRIDO           PIC  9(003)         VALUE ZEROS.

       01 WRK-E-CD-PAIS-AGEO           PIC X(10)  VALUE SPACES.
       01 WRK-E-CD-PAIS-AGEO-R         REDEFINES WRK-E-CD-PAIS-AGEO.
          05 FILLER                    PIC X(07).
          05 WRK-CD-PAIS-AGEO-E        PIC 9(03).

       01 WRK-CD-NBS-X.
           05 WRK-CD-NBS-N             PIC 9(09) VALUE ZEROS.

       01 WRK-AAAAMMDD-INI.
           05 WRK-AAAAMMDD-INI-N       PIC 9(08) VALUE ZEROS.

       01 WRK-AAAAMMDD-FIM.
           05 WRK-AAAAMMDD-FIM-N       PIC 9(08) VALUE ZEROS.

       01 WRK-CALE-AAAMMDD             PIC 9(08) VALUE ZEROS.

      *--> "LITERAIS UTILIZADAS PELO PROGRAMA"
      *

       01  WRK-LIT-PGM                 PIC  X(008)  VALUE 'SCSB0010'.
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
              INCLUDE SCSBB017
           END-EXEC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER PIC X(34) VALUE  '** FIM DA WORKING SCSB0010 **     '.
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
                   WRK-FS-ARQOPER      EQUAL  '10' AND
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
                       ARQOPER
                OUTPUT PRINCVAL
                       OPERVALD

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

           PERFORM 2000-VERIFICAR-VAZIO-ARQPRINC.

           PERFORM 2010-VERIFICAR-VAZIO-ARQOPER.

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
           PERFORM 1130-TESTAR-FS-ARQOPER.
           PERFORM 1140-TESTAR-FS-OPERVALD.

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
      * TESTAR FILE-STATUS DO ARQUIVO DE ENTRADA ARQOPER .             *
      *----------------------------------------------------------------*
       1130-TESTAR-FS-ARQOPER          SECTION.
      *----------------------------------------------------------------*

           MOVE  '1130-TESTAR-FS-ARQOPER '  TO  FRWKGHEA-IDEN-PARAGRAFO

           IF WRK-FS-ARQOPER           NOT EQUAL '00'
              SET   ERRO-ARQUIVO       TO  TRUE
              MOVE  WRK-FS-ARQOPER     TO  FRWKGARQ-FILE-STATUS
              MOVE  'ARQOPER '         TO  FRWKGARQ-NOME-ARQUIVO
              PERFORM 9999-FINALIZAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * TESTAR FILE-STATUS DO ARQUIVO DE SAIDA OPERVALD.               *
      *----------------------------------------------------------------*
       1140-TESTAR-FS-OPERVALD         SECTION.
      *----------------------------------------------------------------*

           MOVE  '1140-TESTAR-FS-OPERVALD'  TO  FRWKGHEA-IDEN-PARAGRAFO

           IF WRK-FS-OPERVALD          NOT EQUAL '00'
              SET   ERRO-ARQUIVO       TO  TRUE
              MOVE  WRK-FS-OPERVALD    TO  FRWKGARQ-FILE-STATUS
              MOVE  'OPERVALD'         TO  FRWKGARQ-NOME-ARQUIVO
              PERFORM 9999-FINALIZAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1140-99-FIM.                    EXIT.
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
           MOVE  CALE01-DT-GREGO-AAAAMMDD
                                       TO  WRK-CALE-AAAMMDD.
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
       2000-VERIFICAR-VAZIO-ARQPRINC   SECTION.
      *----------------------------------------------------------------*

           MOVE  '2000-VERIFICAR-VAZIO-ARQPRINC'
                                       TO  FRWKGHEA-IDEN-PARAGRAFO

           PERFORM 2100-LER-ARQPRINC

           IF WRK-FS-ARQPRINC          EQUAL '10'
              DISPLAY '***================================***'
              DISPLAY '*              SCSB0010              *'
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

           MOVE  LOW-VALUES              TO  WRK-CHV-ARQPRINC-ANT.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * VERIFICAR SE ARQUIVOS DE ENTRADA ESTAO VAZIOS.                 *
      *----------------------------------------------------------------*
       2010-VERIFICAR-VAZIO-ARQOPER    SECTION.
      *----------------------------------------------------------------*

           MOVE  '2010-VERIFICAR-VAZIO-ARQOPER'
                                       TO  FRWKGHEA-IDEN-PARAGRAFO

           PERFORM 2110-LER-ARQOPER

           IF WRK-FS-ARQOPER           EQUAL '10'
              DISPLAY '***================================***'
              DISPLAY '*              SCSB0010              *'
              DISPLAY '*------------------------------------*'
              DISPLAY '*                                    *'
              DISPLAY '*     ARQUIVO ARQOPER  VAZIO !!!     *'
              DISPLAY '*                                    *'
              DISPLAY '***================================***'
              SET   ERRO-ARQUIVO       TO  TRUE
              MOVE  WRK-FS-PRINCVAL    TO  FRWKGARQ-FILE-STATUS
              MOVE  'ARQOPER '         TO  FRWKGARQ-NOME-ARQUIVO
              PERFORM 9000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2010-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * FAZER A LEITURA DO ARQUIVO ARQPRINC.                           *
      *----------------------------------------------------------------*
       2100-LER-ARQPRINC               SECTION.
      *----------------------------------------------------------------*

           MOVE  '2100-LER-ARQPRINC'   TO  FRWKGHEA-IDEN-PARAGRAFO

           MOVE  WRK-CHV-ARQPRINC-ATU  TO  WRK-CHV-ARQPRINC-ANT

           SET  ARQ-READ               TO   TRUE
           READ ARQPRINC               INTO SCSB90-REG

           IF WRK-FS-ARQPRINC          NOT EQUAL '10'
              PERFORM 1110-TESTAR-FS-ARQPRINC
              ADD   1                  TO  ACU-LIDOS-ARQPRINC
              MOVE  ZEROS              TO  WRK-I-ARQPRINC
              PERFORM 3103-BUSCA-ULT-ERRO-ARQPRINC
              MOVE SCSB90-TPO-SERVC
                         TO  WRK-CHV-TPO-SERVC OF WRK-CHV-ARQPRINC-ATU
              MOVE SCSB90-NREG
                         TO  WRK-CHV-NREG      OF WRK-CHV-ARQPRINC-ATU
           ELSE
               MOVE HIGH-VALUES        TO WRK-CHV-ARQPRINC-ATU
           END-IF.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * FAZER A LEITURA DO ARQUIVO ARQOPER.                            *
      *----------------------------------------------------------------*
       2110-LER-ARQOPER                SECTION.
      *----------------------------------------------------------------*

           MOVE  '2110-LER-ARQOPER '   TO  FRWKGHEA-IDEN-PARAGRAFO

           MOVE  WRK-CHV-ARQOPER-ATU-COMP
                                       TO WRK-CHV-ARQOPER-ANT-COMP

           SET  ARQ-READ               TO   TRUE
           READ ARQOPER                INTO SCSB91-REG

           IF WRK-FS-ARQOPER           NOT EQUAL '10'
              PERFORM 1130-TESTAR-FS-ARQOPER
              ADD   1                  TO  ACU-LIDOS-ARQOPER
              MOVE  ZEROS              TO  WRK-I-ARQOPER
              MOVE SCSB91-TPO-SERVC
                         TO  WRK-CHV-TPO-SERVC OF WRK-CHV-ARQOPER-ATU
              MOVE SCSB91-NREG
                         TO  WRK-CHV-NREG      OF WRK-CHV-ARQOPER-ATU

              MOVE SCSB91-CD-OPER
                         TO  WRK-CHV-ARQOPER-CD-OPER-ANT
              PERFORM 3100-VALIDAR-OPERACAO
           ELSE
               MOVE HIGH-VALUES        TO WRK-CHV-ARQOPER-ATU
           END-IF.

      *----------------------------------------------------------------*
       2110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * PROCESSAMENTO PRINCIPAL.                                       *
      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           MOVE  '3000-PROCESSAR'  TO  FRWKGHEA-IDEN-PARAGRAFO

           EVALUATE TRUE
               WHEN WRK-CHV-ARQPRINC-ATU < WRK-CHV-ARQOPER-ATU
                   PERFORM 4000-CHAVE-MENOR
               WHEN WRK-CHV-ARQPRINC-ATU > WRK-CHV-ARQOPER-ATU
                   PERFORM 4100-CHAVE-MAIOR
               WHEN WRK-CHV-ARQPRINC-ATU EQUAL WRK-CHV-ARQOPER-ATU
                   PERFORM 4200-CHAVE-IGUAL
           END-EVALUATE.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * VALIDAR REGISTRO DE OPERACAO.                                  *
      *----------------------------------------------------------------*
       3100-VALIDAR-OPERACAO           SECTION.
      *----------------------------------------------------------------*

           MOVE  '3100-VALIDAR-OPERACAO'
                                       TO  FRWKGHEA-IDEN-PARAGRAFO

           PERFORM 3110-VALIDAR-CD-OPER.

           PERFORM 3160-VALIDAR-PAIS.

           PERFORM 3120-VALIDAR-CD-NBS.

           PERFORM 3130-VALIDAR-CD-MODO-PREST.

           PERFORM 3140-VALIDAR-DT-INICIO

           PERFORM 3150-VALIDAR-DT-CONCLUSAO

           PERFORM 3180-VALIDAR-VALOR-CONVERTIDO.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3101-ADICIONA-ERRO-ARQPRINC     SECTION.
      *----------------------------------------------------------------*

           MOVE  '3101-ADICIONA-ERRO-ARQPRINC'
                                       TO  FRWKGHEA-IDEN-PARAGRAFO

           ADD 1                       TO  WRK-I-ARQPRINC.
           MOVE WRK-ERRO-OCORRIDO TO  SCSB90-COD-ERRO(WRK-I-ARQPRINC).

      *----------------------------------------------------------------*
       3101-FIM.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3102-ADICIONA-ERRO-ARQOPER      SECTION.
      *----------------------------------------------------------------*

           MOVE  '3102-ADICIONA-ERRO-ARQOPER'
                                       TO  FRWKGHEA-IDEN-PARAGRAFO

           ADD 1                       TO  WRK-I-ARQOPER.
           MOVE WRK-ERRO-OCORRIDO  TO  SCSB91-COD-ERRO(WRK-I-ARQOPER).

      *----------------------------------------------------------------*
       3102-FIM.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3103-BUSCA-ULT-ERRO-ARQPRINC    SECTION.
      *----------------------------------------------------------------*

           MOVE  '3103-BUSCA-ULTIMO-ERRO'
                                       TO  FRWKGHEA-IDEN-PARAGRAFO.

           PERFORM VARYING WRK-I-ARQPRINC FROM 1 BY 1
                     UNTIL WRK-I-ARQPRINC GREATER 100

                     IF SCSB90-COD-ERRO(WRK-I-ARQPRINC) EQUAL ZEROS
                       GO TO 3103-FIM
                     END-IF

           END-PERFORM.

      *----------------------------------------------------------------*
       3103-FIM.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3110-VALIDAR-CD-OPER            SECTION.
      *----------------------------------------------------------------*

           MOVE  '3110-VALIDAR-CD-OPER' TO  FRWKGHEA-IDEN-PARAGRAFO

           IF  SCSB91-CD-OPER          NOT NUMERIC
               MOVE 002                TO  WRK-ERRO-OCORRIDO
               PERFORM 3102-ADICIONA-ERRO-ARQOPER
           ELSE
               IF  SCSB91-CD-OPER      EQUAL ZEROS
                   MOVE 003                TO  WRK-ERRO-OCORRIDO
                   PERFORM 3102-ADICIONA-ERRO-ARQOPER
               ELSE
                   IF WRK-CHV-ARQOPER-ATU-COMP EQUAL
                       WRK-CHV-ARQOPER-ANT-COMP
                       MOVE 013                TO  WRK-ERRO-OCORRIDO
                       PERFORM 3102-ADICIONA-ERRO-ARQOPER
                   END-IF
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       3110-FIM.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3120-VALIDAR-CD-NBS             SECTION.
      *----------------------------------------------------------------*

           MOVE  '3120-VALIDAR-CD-NBS' TO  FRWKGHEA-IDEN-PARAGRAFO.

           MOVE SCSB91-CD-NBS          TO  WRK-CD-NBS-X

           IF  WRK-CD-NBS-X EQUAL SPACES
               MOVE 014                TO  WRK-ERRO-OCORRIDO
               PERFORM 3102-ADICIONA-ERRO-ARQOPER
           ELSE
               IF WRK-CD-NBS-N NOT NUMERIC OR
                  WRK-CD-NBS-N < 100000000
                   MOVE 014                TO  WRK-ERRO-OCORRIDO
                   PERFORM 3102-ADICIONA-ERRO-ARQOPER
               ELSE
                   PERFORM 3121-VERIFICA-CD-NBS
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       3120-FIM.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3121-VERIFICA-CD-NBS            SECTION.
      *----------------------------------------------------------------*

           MOVE  '3121-VERIFICA-CD-NBS'
                                       TO  FRWKGHEA-IDEN-PARAGRAFO

           MOVE SCSB91-CD-NBS      TO  CSERVC-VAR-PATRM OF SCSBB017

           EXEC SQL
               SELECT
                       CINDCD_EXIBC_SERVC
               INTO
                       :SCSBB017.CINDCD-EXIBC-SERVC
               FROM  DB2PRD.TSERVC_VAR_PATRM
               WHERE CSERVC_VAR_PATRM   = :SCSBB017.CSERVC-VAR-PATRM
                   AND  DINIC_SERVC_VAR   <= CURRENT DATE
                   AND  DFIM_SERVC_VAR    >= CURRENT DATE
           END-EXEC

           IF (SQLCODE     NOT EQUAL ZEROS AND
               SQLCODE     NOT EQUAL +100) OR
               (SQLWARN0                    EQUAL 'W')
               SET     ERRO-DB2            TO  TRUE
               MOVE   'TSERVC_VAR_PATRM'   TO  FRWKGDB2-NOME-TABELA
               SET     DB2-SELECT          TO  TRUE
               MOVE   '1'                  TO  FRWKGDB2-LOCAL
               PERFORM 9999-FINALIZAR-ERRO
           END-IF

           IF (SQLCODE                  EQUAL +100)
               MOVE 014                TO  WRK-ERRO-OCORRIDO
               PERFORM 3102-ADICIONA-ERRO-ARQOPER
           END-IF.

      *----------------------------------------------------------------*
       3121-FIM.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3130-VALIDAR-CD-MODO-PREST      SECTION.
      *----------------------------------------------------------------*

           MOVE  '3130-VALIDAR-CD-MODO-PREST'
                                       TO  FRWKGHEA-IDEN-PARAGRAFO

           IF  SCSB91-CD-MODO-PREST NOT NUMERIC OR
               (SCSB91-CD-MODO-PREST NOT EQUAL 1 OR
                SCSB91-CD-MODO-PREST NOT EQUAL 2 OR
                SCSB91-CD-MODO-PREST NOT EQUAL 4)

               MOVE 015                TO  WRK-ERRO-OCORRIDO
               PERFORM 3102-ADICIONA-ERRO-ARQOPER
           END-IF.

      *----------------------------------------------------------------*
       3130-FIM.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3140-VALIDAR-DT-INICIO          SECTION.
      *----------------------------------------------------------------*

           MOVE  '3140-VALIDAR-DT-INICIO'   TO  FRWKGHEA-IDEN-PARAGRAFO

           IF  SCSB91-DT-INICIO EQUAL SPACES
               MOVE 016                TO  WRK-ERRO-OCORRIDO
               PERFORM 3102-ADICIONA-ERRO-ARQOPER

           ELSE
               STRING SCSB91-DT-INICIO(07:04)
                      SCSB91-DT-INICIO(04:02)
                      SCSB91-DT-INICIO(01:02)
               DELIMITED BY SIZE           INTO WRK-AAAAMMDD-INI

               IF WRK-AAAAMMDD-INI-N < 20120801 OR
                  WRK-AAAAMMDD-INI-N > WRK-CALE-AAAMMDD
                  MOVE 016                TO  WRK-ERRO-OCORRIDO
                  PERFORM 3102-ADICIONA-ERRO-ARQOPER
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       3140-FIM.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3150-VALIDAR-DT-CONCLUSAO       SECTION.
      *----------------------------------------------------------------*

           MOVE  '3150-VALIDAR-DT-CONCLUSAO'
                                       TO  FRWKGHEA-IDEN-PARAGRAFO

           IF  SCSB91-DT-CONCLUSAO EQUAL SPACES
               MOVE 017                TO  WRK-ERRO-OCORRIDO
               PERFORM 3102-ADICIONA-ERRO-ARQOPER

           ELSE
               STRING SCSB91-DT-INICIO(07:04)
                      SCSB91-DT-INICIO(04:02)
                      SCSB91-DT-INICIO(01:02)
               DELIMITED BY SIZE           INTO WRK-AAAAMMDD-INI

               STRING SCSB91-DT-CONCLUSAO(07:04)
                      SCSB91-DT-CONCLUSAO(04:02)
                      SCSB91-DT-CONCLUSAO(01:02)
               DELIMITED BY SIZE           INTO WRK-AAAAMMDD-FIM

               IF WRK-AAAAMMDD-FIM-N < WRK-AAAAMMDD-INI-N
                  MOVE 017                TO  WRK-ERRO-OCORRIDO
                  PERFORM 3102-ADICIONA-ERRO-ARQOPER
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       3150-FIM.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3160-VALIDAR-PAIS               SECTION.
      *----------------------------------------------------------------*

           MOVE  '3160-VALIDAR-PAIS'   TO  FRWKGHEA-IDEN-PARAGRAFO

           IF SCSB91-CD-PAIS-DESTNO NOT NUMERIC OR
              SCSB91-CD-PAIS-DESTNO EQUAL ZEROS
               MOVE 010                TO  WRK-ERRO-OCORRIDO
               PERFORM 3102-ADICIONA-ERRO-ARQOPER
           ELSE
               PERFORM 7200-CALL-AGEO200
           END-IF.

      *----------------------------------------------------------------*
       3160-FIM.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3180-VALIDAR-VALOR-CONVERTIDO   SECTION.
      *----------------------------------------------------------------*

           MOVE  '3180-VALIDAR-VALOR-CONVERTIDO'
                                       TO  FRWKGHEA-IDEN-PARAGRAFO

           IF SCSB91-VLR-OPER          NOT NUMERIC OR
              SCSB91-VLR-OPER          EQUAL ZEROS
               MOVE 012                TO  WRK-ERRO-OCORRIDO
               PERFORM 3102-ADICIONA-ERRO-ARQOPER
           END-IF.
      *----------------------------------------------------------------*
       3180-FIM.                       EXIT.
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
      * GRAVAR ARQUIVO DE SAIDA OPERVALD.                              *
      *----------------------------------------------------------------*
       3610-GRAVAR-OPERVALD            SECTION.
      *----------------------------------------------------------------*

           MOVE '3610-GRAVAR-OPERVALD' TO  FRWKGHEA-IDEN-PARAGRAFO

           SET   ARQ-WRITE             TO  TRUE

           WRITE FD-OPERVALD           FROM SCSB91-REG

           PERFORM 1140-TESTAR-FS-OPERVALD

           ADD   1                     TO  ACU-GRAVS-OPERVALD.

      *----------------------------------------------------------------*
       3610-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4000-CHAVE-MENOR                SECTION.
      *----------------------------------------------------------------*

           MOVE  '4000-CHAVE-MENOR'    TO FRWKGHEA-IDEN-PARAGRAFO.

           IF WRK-CHV-ARQPRINC-ATU NOT EQUAL WRK-CHV-ARQPRINC-ANT
               MOVE 019                TO  WRK-ERRO-OCORRIDO
               PERFORM 3101-ADICIONA-ERRO-ARQPRINC
               PERFORM 3600-GRAVAR-PRINCVAL
           END-IF.

           PERFORM 2100-LER-ARQPRINC.

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4100-CHAVE-MAIOR                SECTION.
      *----------------------------------------------------------------*

           MOVE  '4100-CHAVE-MAIOR'    TO FRWKGHEA-IDEN-PARAGRAFO.

           MOVE  019                TO  WRK-ERRO-OCORRIDO
           PERFORM 3102-ADICIONA-ERRO-ARQOPER
           PERFORM 3610-GRAVAR-OPERVALD
           PERFORM 2110-LER-ARQOPER.

      *----------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4200-CHAVE-IGUAL                SECTION.
      *----------------------------------------------------------------*

           MOVE  '4200-CHAVE-IGUAL'    TO FRWKGHEA-IDEN-PARAGRAFO.

           IF WRK-CHV-ARQPRINC-ATU NOT EQUAL WRK-CHV-ARQPRINC-ANT
               PERFORM 3600-GRAVAR-PRINCVAL
               MOVE WRK-CHV-ARQPRINC-ATU   TO WRK-CHV-ARQPRINC-ANT
           END-IF

           PERFORM 3610-GRAVAR-OPERVALD
           PERFORM 2110-LER-ARQOPER.

      *----------------------------------------------------------------*
       4200-99-FIM.                    EXIT.
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
           MOVE SCSB91-CD-PAIS-DESTNO      TO WRK-CD-PAIS-AGEO-E
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
               PERFORM 3102-ADICIONA-ERRO-ARQOPER
           END-IF.

      *----------------------------------------------------------------*
       7300-99-FIM.                       EXIT.
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
                  ARQOPER
                  PRINCVAL
                  OPERVALD

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


      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. SCSB0001.
       AUTHOR.     FRANCISCO FREIRE.

      *================================================================*
      *               B R Q   -   I T   S E R V I C E S                *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA     : SCSB0001                                     *
      *    ANALISTA     : FERNANDA CARUSO        - BRQ IT SERVICES.    *
      *    PROGRAMADOR  : FRANCISCO FREIRE       - BRQ IT SERVICES.    *
      *    DATA         : 30/07/2015                                   *
      *                                                                *
      *    OBJETIVO     :                                              *
      *                                                                *
      *                           ARQUIVOS                             *
      *           +---------------------------------------+            *
      *           |  DDNAME  |   I/O   |   BOOK   | LRECL |            *
      *           |----------|---------|----------|-------|            *
      *           | ARQRECEB | INPUT   | I#SCSB99 | 1450  |            *
      *           | ARQORIGN | OUTPUT  | I#SCSB89 | 1459  |            *
      *           | ARQPRINC | OUTPUT  | I#SCSB90 | 1750  |            *
      *           | ARQOPER  | OUTPUT  | I#SCSB91 | 0400  |            *
      *           | ARQENQUA | OUTPUT  | I#SCSB92 | 0360  |            *
      *           | ARQREDI  | OUTPUT  | I#SCSB93 | 0350  |            *
      *           | ARQFATPG | OUTPUT  | I#SCSB94 | 0450  |            *
      *           | FTPGOPER | OUTPUT  | I#SCSB95 | 0400  |            *
      *           | FTPGREDI | OUTPUT  | I#SCSB96 | 0360  |            *
      *           | TPREGNOK | OUTPUT  | I#SCSB97 | 0050  |            *
      *           +---------------------------------------+            *
      *                                                                *
      *           +---------------------------------------+            *
      *           | TABELA                    |  INCLUDE  |            *
      *           |---------------------------|-----------|            *
      *           | DB2PRD.TTPO_PROCS_EXTER   | SCSBB045  |            *
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

           SELECT ARQRECEB ASSIGN      TO ARQRECEB
                      FILE STATUS      IS WRK-FS-ARQRECEB.

           SELECT ARQORIGN ASSIGN      TO ARQORIGN
                      FILE STATUS      IS WRK-FS-ARQORIGN.

           SELECT ARQPRINC ASSIGN      TO ARQPRINC
                      FILE STATUS      IS WRK-FS-ARQPRINC.

           SELECT ARQOPER ASSIGN       TO ARQOPER
                      FILE STATUS      IS WRK-FS-ARQOPER.

           SELECT ARQENQUA ASSIGN      TO ARQENQUA
                      FILE STATUS      IS WRK-FS-ARQENQUA.

           SELECT ARQREDI ASSIGN       TO ARQREDI
                      FILE STATUS      IS WRK-FS-ARQREDI.

           SELECT ARQFATPG ASSIGN      TO ARQFATPG
                      FILE STATUS      IS WRK-FS-ARQFATPG.

           SELECT FTPGOPER ASSIGN      TO FTPGOPER
                      FILE STATUS      IS WRK-FS-FTPGOPER.

           SELECT FTPGREDI ASSIGN      TO FTPGREDI
                      FILE STATUS      IS WRK-FS-FTPGREDI.

           SELECT TPREGNOK ASSIGN      TO TPREGNOK
                      FILE STATUS      IS WRK-FS-TPREGNOK.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * INPUT -                                                        *
      *                                                                *
      *                 ORG. SEQUENCIAL  -  LRECL 1450                 *
      *----------------------------------------------------------------*

       FD  ARQRECEB
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-ARQRECEB                 PIC X(1450).

      *----------------------------------------------------------------*
      * OUTPUT -                                                       *
      *                                                                *
      *                 ORG. SEQUENCIAL  -  LRECL 1459                 *
      *----------------------------------------------------------------*

       FD  ARQORIGN
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-ARQORIGN                 PIC X(1459).

      *----------------------------------------------------------------*
      * OUTPUT -                                                       *
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
      *                 ORG. SEQUENCIAL  -  LRECL 400                  *
      *----------------------------------------------------------------*

       FD  ARQOPER
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-ARQOPER                  PIC X(400).

      *----------------------------------------------------------------*
      * OUTPUT -                                                       *
      *                                                                *
      *                 ORG. SEQUENCIAL  -  LRECL 360                  *
      *----------------------------------------------------------------*

       FD  ARQENQUA
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-ARQENQUA                 PIC X(360).

      *----------------------------------------------------------------*
      * OUTPUT -                                                       *
      *                                                                *
      *                 ORG. SEQUENCIAL  -  LRECL 350                  *
      *----------------------------------------------------------------*

       FD  ARQREDI
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-ARQREDI                  PIC X(350).

      *----------------------------------------------------------------*
      * OUTPUT -                                                       *
      *                                                                *
      *                 ORG. SEQUENCIAL  -  LRECL 450                  *
      *----------------------------------------------------------------*

       FD  ARQFATPG
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-ARQFATPG                 PIC X(450).

      *----------------------------------------------------------------*
      * OUTPUT -                                                       *
      *                                                                *
      *                 ORG. SEQUENCIAL  -  LRECL 400                  *
      *----------------------------------------------------------------*

       FD  FTPGOPER
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-FTPGOPER                 PIC X(400).

      *----------------------------------------------------------------*
      * OUTPUT -                                                       *
      *                                                                *
      *                 ORG. SEQUENCIAL  -  LRECL 360                  *
      *----------------------------------------------------------------*

       FD  FTPGREDI
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-FTPGREDI                 PIC X(360).

      *----------------------------------------------------------------*
      * OUTPUT -                                                       *
      *                                                                *
      *                 ORG. SEQUENCIAL  -  LRECL 50                   *
      *----------------------------------------------------------------*

       FD  TPREGNOK
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-TPREGNOK                 PIC X(50).


      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER PIC X(34) VALUE  '** INICIO DA WORKING SCSB0001 **  '.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER PIC X(34) VALUE  '** FILE-STATUS DOS ARQUIVOS **    '.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       01  WRK-FS-ARQRECEB             PIC  X(002)  VALUE SPACES.
       01  WRK-FS-ARQORIGN             PIC  X(002)  VALUE SPACES.
       01  WRK-FS-ARQPRINC             PIC  X(002)  VALUE SPACES.
       01  WRK-FS-ARQOPER              PIC  X(002)  VALUE SPACES.
       01  WRK-FS-ARQENQUA             PIC  X(002)  VALUE SPACES.
       01  WRK-FS-ARQREDI              PIC  X(002)  VALUE SPACES.
       01  WRK-FS-ARQFATPG             PIC  X(002)  VALUE SPACES.
       01  WRK-FS-FTPGOPER             PIC  X(002)  VALUE SPACES.
       01  WRK-FS-FTPGREDI             PIC  X(002)  VALUE SPACES.
       01  WRK-FS-TPREGNOK             PIC  X(002)  VALUE SPACES.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER PIC X(34) VALUE  '** BOOK DOS ARQUIVOS **           '.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

      *-->  ARQRECEB
       COPY 'I#SCSB99'.

      *-->  ARQPRINC
       COPY 'I#SCSB90'.

      *-->  ARQOPER
       COPY 'I#SCSB91'.

      *-->  ARQENQUA
       COPY 'I#SCSB92'.

      *-->  ARQREDI
       COPY 'I#SCSB93'.

      *-->  ARQFATPG
       COPY 'I#SCSB94'.

      *-->  FTPGOPER
       COPY 'I#SCSB95'.

      *-->  FTPGREDI
       COPY 'I#SCSB96'.

      *-->  TPREGNOK
       COPY 'I#SCSB97'.

      *-->  ARQORIGN
       COPY 'I#SCSB89'.

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

       01  WRK-AREA-UORG2397.
           COPY    'UORGWB97'.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER PIC X(34) VALUE  '** AREA DA API - UORG2389 **      '.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       01  WRK-AREA-UORG2389.
           COPY    'UORGWB88'.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER PIC X(34) VALUE  '** ACUMULADORES **                '.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       01  ACU-LIDOS-ARQRECEB          PIC  9(009)  COMP-3 VALUE ZEROS.
       01  ACU-GRAVS-ARQORIGN          PIC  9(009)  COMP-3 VALUE ZEROS.
       01  ACU-GRAVS-ARQPRINC          PIC  9(009)  COMP-3 VALUE ZEROS.
       01  ACU-GRAVS-ARQOPER           PIC  9(009)  COMP-3 VALUE ZEROS.
       01  ACU-GRAVS-ARQENQUA          PIC  9(009)  COMP-3 VALUE ZEROS.
       01  ACU-GRAVS-ARQREDI           PIC  9(009)  COMP-3 VALUE ZEROS.
       01  ACU-GRAVS-ARQFATPG          PIC  9(009)  COMP-3 VALUE ZEROS.
       01  ACU-GRAVS-FTPGOPER          PIC  9(009)  COMP-3 VALUE ZEROS.
       01  ACU-GRAVS-FTPGREDI          PIC  9(009)  COMP-3 VALUE ZEROS.
       01  ACU-GRAVS-TPREGNOK          PIC  9(009)  COMP-3 VALUE ZEROS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER PIC X(34) VALUE  '** VARIAVEIS AUXILIARES **        '.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       01  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       01  WRK-SEQ                     PIC  9(009)  COMP-3 VALUE ZEROS.
       01  WRK-QTDE-REG                PIC  9(009)  COMP-3 VALUE ZEROS.

       01  WRK-DEPEND-R                PIC  9(06)          VALUE ZEROS.
       01  WRK-DEPEND-X                REDEFINES
           WRK-DEPEND-R                PIC  X(06).

       01  WRK-CPSSOA-UND-ORGNZ        PIC S9(10)          VALUE ZEROS.
       01  WRK-CPSSOA-UND-ORGNZ-R      REDEFINES
           WRK-CPSSOA-UND-ORGNZ        PIC 9(10).

       01  WRK-NSEQ-UND-ORGNZ          PIC S9(08)          VALUE ZEROS.
       01  WRK-NSEQ-UND-ORGNZ-R        REDEFINES
           WRK-NSEQ-UND-ORGNZ          PIC 9(08).

       01  WRK-TPO-SERVC               PIC 9(001)          VALUE ZEROS.
       01  WRK-TPO-SERVC-X             REDEFINES
           WRK-TPO-SERVC               PIC X(001).

       01  WRK-NREG                    PIC 9(009)          VALUE ZEROS.
       01  WRK-NREG-X                  REDEFINES
           WRK-NREG                    PIC X(009).

       01  WRK-CNPJ-PRINC              PIC 9(009)          VALUE ZEROS.
       01  WRK-CNPJ-PRINC-X            REDEFINES
           WRK-CNPJ-PRINC              PIC X(009).

       01  WRK-CNPJ-FLIAL              PIC 9(005)          VALUE ZEROS.
       01  WRK-CNPJ-FLIAL-X            REDEFINES
           WRK-CNPJ-FLIAL              PIC X(005).

       01  WRK-CNPJ-CTRL               PIC 9(002)          VALUE ZEROS.
       01  WRK-CNPJ-CTRL-X             REDEFINES
           WRK-CNPJ-CTRL               PIC X(002).

       01  WRK-CD-PAIS-RF              PIC 9(003)          VALUE ZEROS.
       01  WRK-CD-PAIS-RF-X            REDEFINES
           WRK-CD-PAIS-RF              PIC X(003).

       01  WRK-CD-MOEDA                PIC 9(005)          VALUE ZEROS.
       01  WRK-CD-MOEDA-X              REDEFINES
           WRK-CD-MOEDA                PIC X(005).

       01  WRK-VLR-CONVERT             PIC 9(013)V99       VALUE ZEROS.
       01  WRK-VLR-CONVERT-X           REDEFINES
           WRK-VLR-CONVERT             PIC X(015).

       01  WRK-VLR-OPER                PIC 9(013)V99       VALUE ZEROS.
       01  WRK-VLR-OPER-X              REDEFINES
           WRK-VLR-OPER                PIC X(015).

       01  WRK-VLR-FATMT-PGTO          PIC 9(013)V99       VALUE ZEROS.
       01  WRK-VLR-FATMT-PGTO-X        REDEFINES
           WRK-VLR-FATMT-PGTO          PIC X(015).

       01  WRK-VLR-MANTD-EXTR          PIC 9(013)V99       VALUE ZEROS.
       01  WRK-VLR-MANTD-EXTR-X        REDEFINES
           WRK-VLR-MANTD-EXTR          PIC X(015).

       01  WRK-CD-OPER                PIC 9(010)          VALUE ZEROS.
       01  WRK-CD-OPER-X              REDEFINES
           WRK-CD-OPER                PIC X(010).

       01  WRK-CD-ENQUA                PIC 9(010)          VALUE ZEROS.
       01  WRK-CD-ENQUA-X              REDEFINES
           WRK-CD-ENQUA                PIC X(010).

       01  WRK-CD-DI                   PIC 9(010)          VALUE ZEROS.
       01  WRK-CD-DI-X                 REDEFINES
           WRK-CD-DI                   PIC X(010).

       01  WRK-CD-FATMT-PGTO           PIC 9(010)          VALUE ZEROS.
       01  WRK-CD-FATMT-PGTO-X         REDEFINES
           WRK-CD-FATMT-PGTO           PIC X(010).

       01  WRK-CD-MODO-PREST           PIC 9(001)          VALUE ZEROS.
       01  WRK-CD-MODO-PREST-X         REDEFINES
           WRK-CD-MODO-PREST           PIC X(001).

       01  WRK-TPO-REG                 PIC 9(001)          VALUE ZEROS.
       01  WRK-TPO-REG-X               REDEFINES
           WRK-TPO-REG                 PIC X(001).

       01  WRK-RC                      PIC 9(008)          VALUE ZEROS.
       01  WRK-RC-X                    REDEFINES
           WRK-RC                      PIC X(008).

       01  WRK-CD-RE                   PIC 9(012)          VALUE ZEROS.
       01  WRK-CD-RE-X                 REDEFINES
           WRK-CD-RE                   PIC X(012).

       01  WRK-CD-PAIS-DESTNO          PIC 9(003)          VALUE ZEROS.
       01  WRK-CD-PAIS-DESTNO-X        REDEFINES
           WRK-CD-PAIS-DESTNO          PIC X(003).


      *--> "LITERAIS UTILIZADAS PELO PROGRAMA"
      *

       01  WRK-LIT-PGM                 PIC  X(008)  VALUE 'SCSB0001'.
       01  WRK-FRWK2999                PIC  X(008)  VALUE 'FRWK2999'.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER PIC X(34) VALUE  '** AREA DE INCLUDES PARA DB2 **   '.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

           EXEC SQL
              INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
              INCLUDE SCSBB045
           END-EXEC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER PIC X(34) VALUE  '** FIM DA WORKING SCSB0001 **     '.
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
                   WRK-FS-ARQRECEB     EQUAL  '10'

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

           OPEN INPUT  ARQRECEB
                OUTPUT ARQORIGN
                       ARQPRINC
                       ARQOPER
                       ARQENQUA
                       ARQREDI
                       ARQFATPG
                       FTPGOPER
                       FTPGREDI
                       TPREGNOK

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

      *
      *--> "VERIFICA PRIMEIRO REGISTRO"
      *

           PERFORM 2200-VERIFICAR-HEADER.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * FAZER TESTE DE FILE-STATUS PARA OS ARQUIVOS.                   *
      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 1110-TESTAR-FS-ARQRECEB
           PERFORM 1120-TESTAR-FS-ARQORIGN.
           PERFORM 1121-TESTAR-FS-ARQPRINC.
           PERFORM 1122-TESTAR-FS-ARQOPER .
           PERFORM 1123-TESTAR-FS-ARQENQUA.
           PERFORM 1124-TESTAR-FS-ARQREDI .
           PERFORM 1125-TESTAR-FS-ARQFATPG.
           PERFORM 1126-TESTAR-FS-FTPGOPER.
           PERFORM 1127-TESTAR-FS-FTPGREDI.
           PERFORM 1128-TESTAR-FS-TPREGNOK.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * TESTAR FILE-STATUS DO ARQUIVO DE ENTRADA ARQRECEB.             *
      *----------------------------------------------------------------*
       1110-TESTAR-FS-ARQRECEB         SECTION.
      *----------------------------------------------------------------*

           IF WRK-FS-ARQRECEB          NOT EQUAL '00'
              SET   ERRO-ARQUIVO       TO  TRUE
              MOVE  WRK-FS-ARQRECEB    TO  FRWKGARQ-FILE-STATUS
              MOVE  'ARQRECEB'         TO  FRWKGARQ-NOME-ARQUIVO
              PERFORM 9999-FINALIZAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * TESTAR FILE-STATUS DO ARQUIVO DE SAIDA ARQORIGN.               *
      *----------------------------------------------------------------*
       1120-TESTAR-FS-ARQORIGN         SECTION.
      *----------------------------------------------------------------*

           IF WRK-FS-ARQORIGN          NOT EQUAL '00'
              SET   ERRO-ARQUIVO       TO  TRUE
              MOVE  WRK-FS-ARQORIGN    TO  FRWKGARQ-FILE-STATUS
              MOVE  'ARQORIGN'         TO  FRWKGARQ-NOME-ARQUIVO
              PERFORM 9999-FINALIZAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * TESTAR FILE-STATUS DO ARQUIVO DE SAIDA ARQPRINC.               *
      *----------------------------------------------------------------*
       1121-TESTAR-FS-ARQPRINC         SECTION.
      *----------------------------------------------------------------*

           IF WRK-FS-ARQPRINC          NOT EQUAL '00'
              SET   ERRO-ARQUIVO       TO  TRUE
              MOVE  WRK-FS-ARQPRINC    TO  FRWKGARQ-FILE-STATUS
              MOVE  'ARQPRINC'         TO  FRWKGARQ-NOME-ARQUIVO
              PERFORM 9999-FINALIZAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1121-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * TESTAR FILE-STATUS DO ARQUIVO DE SAIDA ARQOPER.                *
      *----------------------------------------------------------------*
       1122-TESTAR-FS-ARQOPER         SECTION.
      *----------------------------------------------------------------*

           IF WRK-FS-ARQOPER          NOT EQUAL '00'
              SET   ERRO-ARQUIVO       TO  TRUE
              MOVE  WRK-FS-ARQOPER    TO  FRWKGARQ-FILE-STATUS
              MOVE  'ARQOPER'         TO  FRWKGARQ-NOME-ARQUIVO
              PERFORM 9999-FINALIZAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1122-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * TESTAR FILE-STATUS DO ARQUIVO DE SAIDA ARQENQUA.               *
      *----------------------------------------------------------------*
       1123-TESTAR-FS-ARQENQUA         SECTION.
      *----------------------------------------------------------------*

           IF WRK-FS-ARQENQUA          NOT EQUAL '00'
              SET   ERRO-ARQUIVO       TO  TRUE
              MOVE  WRK-FS-ARQENQUA    TO  FRWKGARQ-FILE-STATUS
              MOVE  'ARQENQUA'         TO  FRWKGARQ-NOME-ARQUIVO
              PERFORM 9999-FINALIZAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1123-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * TESTAR FILE-STATUS DO ARQUIVO DE SAIDA ARQREDI.                *
      *----------------------------------------------------------------*
       1124-TESTAR-FS-ARQREDI         SECTION.
      *----------------------------------------------------------------*

           IF WRK-FS-ARQREDI          NOT EQUAL '00'
              SET   ERRO-ARQUIVO      TO  TRUE
              MOVE  WRK-FS-ARQREDI    TO  FRWKGARQ-FILE-STATUS
              MOVE  'ARQREDI'         TO  FRWKGARQ-NOME-ARQUIVO
              PERFORM 9999-FINALIZAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1124-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * TESTAR FILE-STATUS DO ARQUIVO DE SAIDA ARQFATPG.               *
      *----------------------------------------------------------------*
       1125-TESTAR-FS-ARQFATPG         SECTION.
      *----------------------------------------------------------------*

           IF WRK-FS-ARQFATPG          NOT EQUAL '00'
              SET   ERRO-ARQUIVO       TO  TRUE
              MOVE  WRK-FS-ARQFATPG    TO  FRWKGARQ-FILE-STATUS
              MOVE  'ARQFATPG'         TO  FRWKGARQ-NOME-ARQUIVO
              PERFORM 9999-FINALIZAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1125-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * TESTAR FILE-STATUS DO ARQUIVO DE SAIDA FTPGOPER.               *
      *----------------------------------------------------------------*
       1126-TESTAR-FS-FTPGOPER         SECTION.
      *----------------------------------------------------------------*

           IF WRK-FS-FTPGOPER          NOT EQUAL '00'
              SET   ERRO-ARQUIVO       TO  TRUE
              MOVE  WRK-FS-FTPGOPER    TO  FRWKGARQ-FILE-STATUS
              MOVE  'FTPGOPER'         TO  FRWKGARQ-NOME-ARQUIVO
              PERFORM 9999-FINALIZAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1126-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * TESTAR FILE-STATUS DO ARQUIVO DE SAIDA FTPGREDI.               *
      *----------------------------------------------------------------*
       1127-TESTAR-FS-FTPGREDI         SECTION.
      *----------------------------------------------------------------*

           IF WRK-FS-FTPGREDI          NOT EQUAL '00'
              SET   ERRO-ARQUIVO       TO  TRUE
              MOVE  WRK-FS-FTPGREDI    TO  FRWKGARQ-FILE-STATUS
              MOVE  'FTPGREDI'         TO  FRWKGARQ-NOME-ARQUIVO
              PERFORM 9999-FINALIZAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1127-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * TESTAR FILE-STATUS DO ARQUIVO DE SAIDA TPREGNOK.               *
      *----------------------------------------------------------------*
       1128-TESTAR-FS-TPREGNOK         SECTION.
      *----------------------------------------------------------------*

           IF WRK-FS-TPREGNOK          NOT EQUAL '00'
              SET   ERRO-ARQUIVO       TO  TRUE
              MOVE  WRK-FS-TPREGNOK    TO  FRWKGARQ-FILE-STATUS
              MOVE  'TPREGNOK'         TO  FRWKGARQ-NOME-ARQUIVO
              PERFORM 9999-FINALIZAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1128-99-FIM.                    EXIT.
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

           PERFORM 2100-LER-ARQRECEB

           IF WRK-FS-ARQRECEB          EQUAL '10'
              DISPLAY '***================================***'
              DISPLAY '*              SCSB0001              *'
              DISPLAY '*------------------------------------*'
              DISPLAY '*                                    *'
              DISPLAY '*     ARQUIVO ARQRECEB VAZIO !!!     *'
              DISPLAY '*                                    *'
              DISPLAY '***================================***'
              SET   ERRO-ARQUIVO       TO  TRUE
              MOVE  WRK-FS-ARQORIGN    TO  FRWKGARQ-FILE-STATUS
              MOVE  'ARQRECEB'         TO  FRWKGARQ-NOME-ARQUIVO
              PERFORM 9999-FINALIZAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * FAZER A LEITURA DO ARQUIVO ARQRECEB.                           *
      *----------------------------------------------------------------*
       2100-LER-ARQRECEB               SECTION.
      *----------------------------------------------------------------*

           MOVE  '2100-LER-ARQRECEB'   TO  FRWKGHEA-IDEN-PARAGRAFO

           SET  ARQ-READ               TO   TRUE
           READ ARQRECEB               INTO SCSB99-REG

           IF WRK-FS-ARQRECEB          NOT EQUAL '10'
              PERFORM 1110-TESTAR-FS-ARQRECEB
              ADD   1                  TO  ACU-LIDOS-ARQRECEB
              ADD   1                  TO  WRK-SEQ
           END-IF.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * VERIFICA SE O PRIMEIRO REGISTRO EH HEADER                      *
      *----------------------------------------------------------------*
       2200-VERIFICAR-HEADER           SECTION.
      *----------------------------------------------------------------*

           MOVE  '2200-VERIFICAR-HEADER' TO  FRWKGHEA-IDEN-PARAGRAFO

           IF SCSB99-ID-REG            NOT EQUAL 'H'
              SET   ERRO-ARQUIVO       TO  TRUE
              MOVE  WRK-FS-ARQORIGN    TO  FRWKGARQ-FILE-STATUS
              MOVE  'ARQRECEB'         TO  FRWKGARQ-NOME-ARQUIVO
              PERFORM 9999-FINALIZAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * PROCESSAMENTO PRINCIPAL.                                       *
      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           IF SCSB99-ID-REG EQUAL 'D'
               PERFORM 3100-MONTA-DETALHE
           ELSE
               IF SCSB99-ID-REG EQUAL 'H'
                   PERFORM 3200-MONTA-HEADER
               ELSE
                   IF SCSB99-ID-REG EQUAL 'T'
                       PERFORM 3300-MONTA-TRAILER
                   END-IF
               END-IF
           END-IF.

           PERFORM 2100-LER-ARQRECEB.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * CARREGA REGISTRO DETALHE.                                      *
      *----------------------------------------------------------------*
       3100-MONTA-DETALHE              SECTION.
      *----------------------------------------------------------------*

           EVALUATE SCSB99-TPO-REG
               WHEN 1
                   PERFORM 3610-GRAVAR-ARQPRINC

               WHEN 2
                   PERFORM 3620-GRAVAR-ARQOPER

               WHEN 3
                   PERFORM 3630-GRAVAR-ARQENQUA
               WHEN 4
               WHEN 5
                   PERFORM 3640-GRAVAR-ARQREDI

               WHEN 6
                   PERFORM 3650-GRAVAR-ARQFATPG

               WHEN 7
                   PERFORM 3660-GRAVAR-FTPGOPER

               WHEN 8
               WHEN 9
                   PERFORM 3670-GRAVAR-FTPGREDI

               WHEN OTHER
                   PERFORM 3680-GRAVAR-TPREGNOK
           END-EVALUATE.

           PERFORM 3600-GRAVAR-ARQORIGN.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * CARREGA REGISTRO HEADER .                                      *
      *----------------------------------------------------------------*
       3200-MONTA-HEADER              SECTION.
      *----------------------------------------------------------------*

           PERFORM 3210-CONSISTI-HEADER.

      *Chamar UORG2397 para converter a empresa SAP (SCSB99-EMPR-DEPEND)
      *para UORG
           PERFORM 7000-CALL-UORG2397

      *Chamar UORG2389 pra converter a dependência SAP (SCSB99-DEPEND)
      * para UORG

           PERFORM 8000-CALL-UORG2389

      *- Fazer buscar na tabela SCSBB045 com o número do processo
      *  (SCSB99-NPROCS)para obter a empresa e dependência parametrizada

           PERFORM 3220-OBTEM-EMPRESA-PARAM

      *Comparar a empresa/dependência (header convertida) com a
      *empresa/dependência parametrizada

           IF UORGWB97-S-CPSSOA-JURID NOT EQUAL WRK-CPSSOA-UND-ORGNZ-R
             SET   ERRO-ARQUIVO       TO  TRUE
             MOVE  WRK-FS-ARQORIGN    TO  FRWKGARQ-FILE-STATUS
             MOVE  'ARQRECEB'         TO  FRWKGARQ-NOME-ARQUIVO
             DISPLAY '***================================***'
             DISPLAY '*              SCSB001               *'
             DISPLAY '*------------------------------------*'
             DISPLAY '*                                    *'
             DISPLAY '*     CAMPO EMPR-DEPEND INVALIDO     *'
             DISPLAY '*                                    *'
             DISPLAY '***================================***'
             MOVE  04                    TO  RETURN-CODE
             PERFORM 9000-FINALIZAR
           END-IF

           IF UORGWB88-S-NSEQ-UND-ORGNZ NOT EQUAL WRK-NSEQ-UND-ORGNZ-R
             SET   ERRO-ARQUIVO       TO  TRUE
             MOVE  WRK-FS-ARQORIGN    TO  FRWKGARQ-FILE-STATUS
             MOVE  'ARQRECEB'         TO  FRWKGARQ-NOME-ARQUIVO
             DISPLAY '***================================***'
             DISPLAY '*              SCSB001               *'
             DISPLAY '*------------------------------------*'
             DISPLAY '*                                    *'
             DISPLAY '*     CAMPO DEPEND INVALIDO          *'
             DISPLAY '*                                    *'
             DISPLAY '***================================***'
             MOVE  04                    TO  RETURN-CODE
             PERFORM 9000-FINALIZAR
           END-IF

      *Gravar ARQORIGN (I#SCSB89) -> mesmo layout do arquivo de entrada
      *I#SCSB99 (mover SCSB99-REG para SCSB89-REG 10:1450) +
      *mover WRK-SEQ para SCSB89-NSEQ.

           PERFORM 3600-GRAVAR-ARQORIGN.

      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * CONSISTENCIA FISICA DO HEADER.                                 *
      *----------------------------------------------------------------*
       3210-CONSISTI-HEADER      SECTION.
      *----------------------------------------------------------------*

           MOVE '3210-CONSISTI-HEADER' TO  FRWKGHEA-IDEN-PARAGRAFO

           IF SCSB99-EMPR-DEPEND EQUAL SPACES
             SET   ERRO-ARQUIVO       TO  TRUE
             MOVE  WRK-FS-ARQORIGN    TO  FRWKGARQ-FILE-STATUS
             MOVE  'ARQRECEB'         TO  FRWKGARQ-NOME-ARQUIVO
             DISPLAY '***================================***'
             DISPLAY '*              SCSB001               *'
             DISPLAY '*------------------------------------*'
             DISPLAY '*                                    *'
             DISPLAY '*     CAMPO EMPR-DEPEND INVALIDO     *'
             DISPLAY '*                                    *'
             DISPLAY '***================================***'
             MOVE  04                    TO  RETURN-CODE
             PERFORM 9000-FINALIZAR
           END-IF

           IF SCSB99-DEPEND NOT NUMERIC OR
              SCSB99-DEPEND EQUAL ZEROS
             SET   ERRO-ARQUIVO       TO  TRUE
             MOVE  WRK-FS-ARQORIGN    TO  FRWKGARQ-FILE-STATUS
             MOVE  'ARQRECEB'         TO  FRWKGARQ-NOME-ARQUIVO
             DISPLAY '***================================***'
             DISPLAY '*              SCSB001               *'
             DISPLAY '*------------------------------------*'
             DISPLAY '*                                    *'
             DISPLAY '*     CAMPO DEPEND INVALIDO          *'
             DISPLAY '*                                    *'
             DISPLAY '***================================***'
             MOVE  04                    TO  RETURN-CODE
             PERFORM 9000-FINALIZAR
           END-IF

           IF SCSB99-DATA EQUAL SPACES
             SET   ERRO-ARQUIVO       TO  TRUE
             MOVE  WRK-FS-ARQORIGN    TO  FRWKGARQ-FILE-STATUS
             MOVE  'ARQRECEB'         TO  FRWKGARQ-NOME-ARQUIVO
             DISPLAY '***================================***'
             DISPLAY '*              SCSB001               *'
             DISPLAY '*------------------------------------*'
             DISPLAY '*                                    *'
             DISPLAY '*     CAMPO DATA INVALIDO            *'
             DISPLAY '*                                    *'
             DISPLAY '***================================***'
             MOVE  04                    TO  RETURN-CODE
             PERFORM 9000-FINALIZAR
           END-IF

           IF SCSB99-DATA NOT EQUAL WRK-DATAPROC
             SET   ERRO-ARQUIVO       TO  TRUE
             MOVE  WRK-FS-ARQORIGN    TO  FRWKGARQ-FILE-STATUS
             MOVE  'ARQRECEB'         TO  FRWKGARQ-NOME-ARQUIVO
             DISPLAY '***================================***'
             DISPLAY '*              SCSB001               *'
             DISPLAY '*------------------------------------*'
             DISPLAY '*                                    *'
             DISPLAY '*     DATA ATUAL DIFERENTE DA DATA   *'
             DISPLAY '*             DO HEADER              *'
             DISPLAY '*                                    *'
             DISPLAY '***================================***'
             MOVE  04                    TO  RETURN-CODE
             PERFORM 9000-FINALIZAR
           END-IF


           IF SCSB99-NPROCS NOT NUMERIC OR
              SCSB99-NPROCS EQUAL ZEROS
             SET   ERRO-ARQUIVO       TO  TRUE
             MOVE  WRK-FS-ARQORIGN    TO  FRWKGARQ-FILE-STATUS
             MOVE  'ARQRECEB'         TO  FRWKGARQ-NOME-ARQUIVO
             DISPLAY '***================================***'
             DISPLAY '*              SCSB001               *'
             DISPLAY '*------------------------------------*'
             DISPLAY '*                                    *'
             DISPLAY '*     CAMPO NPROCS INVALIDO          *'
             DISPLAY '*                                    *'
             DISPLAY '***================================***'
             MOVE  04                    TO  RETURN-CODE
             PERFORM 9000-FINALIZAR
           END-IF

           IF SCSB99-CUSUAR EQUAL SPACES
             SET   ERRO-ARQUIVO       TO  TRUE
             MOVE  WRK-FS-ARQORIGN    TO  FRWKGARQ-FILE-STATUS
             MOVE  'ARQRECEB'         TO  FRWKGARQ-NOME-ARQUIVO
             DISPLAY '***================================***'
             DISPLAY '*              SCSB001               *'
             DISPLAY '*------------------------------------*'
             DISPLAY '*                                    *'
             DISPLAY '*     CAMPO CUSUAR INVALIDO          *'
             DISPLAY '*                                    *'
             DISPLAY '***================================***'
             MOVE  04                    TO  RETURN-CODE
             PERFORM 9000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       3210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * OBTEM EMPRESA PARAMETRIZADA NA TABELA SCSBB045                 *
      *----------------------------------------------------------------*
       3220-OBTEM-EMPRESA-PARAM        SECTION.
      *----------------------------------------------------------------*

           MOVE  '3220-OBTEM-EMPRESA-PARAM'
                                       TO  FRWKGHEA-IDEN-PARAGRAFO
           MOVE  SCSB99-NPROCS         TO  CTPO-PROCS-EXTER OF SCSBB045

           EXEC SQL
              SELECT
                    CPSSOA_UND_ORGNZ
                    ,NSEQ_UND_ORGNZ
              INTO
                    :SCSBB045.CPSSOA-UND-ORGNZ,
                    :SCSBB045.NSEQ-UND-ORGNZ
              FROM  DB2PRD.TTPO_PROCS_EXTER
              WHERE CTPO_PROCS_EXTER   = :SCSBB045.CTPO-PROCS-EXTER
           END-EXEC

           IF (SQLCODE                     NOT EQUAL ZEROS) OR
              (SQLWARN0                    EQUAL 'W')
              IF (SQLCODE                     EQUAL +100)
                   DISPLAY '***================================***'
                   DISPLAY '*              SCSB001               *'
                   DISPLAY '*------------------------------------*'
                   DISPLAY '*                                    *'
                   DISPLAY '*    CTPO_PROCS_EXTER NAO ENCONTRADO *'
                   DISPLAY '*                                    *'
                   DISPLAY '***================================***'
                   MOVE  04                    TO  RETURN-CODE
                   PERFORM 9000-FINALIZAR
              END-IF

              SET     ERRO-DB2            TO  TRUE
              MOVE   'TTPO_PROCS_EXTER'  TO  FRWKGDB2-NOME-TABELA
              SET     DB2-SELECT          TO  TRUE
              MOVE   '1'                  TO  FRWKGDB2-LOCAL
              PERFORM 9999-FINALIZAR-ERRO
           END-IF.

           MOVE CPSSOA-UND-ORGNZ OF SCSBB045 TO WRK-CPSSOA-UND-ORGNZ.
           MOVE NSEQ-UND-ORGNZ OF SCSBB045   TO WRK-NSEQ-UND-ORGNZ.

      *----------------------------------------------------------------*
       3220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * CARREGA REGISTRO TRAILER.                                      *
      *----------------------------------------------------------------*
       3300-MONTA-TRAILER              SECTION.
      *----------------------------------------------------------------*

           IF SCSB99-QTDE-REG NOT NUMERIC OR
              SCSB99-QTDE-REG EQUAL ZEROS
             SET   ERRO-ARQUIVO       TO  TRUE
             MOVE  WRK-FS-ARQORIGN    TO  FRWKGARQ-FILE-STATUS
             MOVE  'ARQRECEB'         TO  FRWKGARQ-NOME-ARQUIVO
             DISPLAY '***================================***'
             DISPLAY '*              SCSB001               *'
             DISPLAY '*------------------------------------*'
             DISPLAY '*                                    *'
             DISPLAY '*     CAMPO QTDE-REG INVALIDO        *'
             DISPLAY '*                                    *'
             DISPLAY '***================================***'
             MOVE  04                    TO  RETURN-CODE
             PERFORM 9000-FINALIZAR
           END-IF

           MOVE SCSB99-QTDE-REG  TO WRK-QTDE-REG

           PERFORM 3600-GRAVAR-ARQORIGN.

           IF WRK-QTDE-REG NOT EQUAL ACU-LIDOS-ARQRECEB
             SET   ERRO-ARQUIVO       TO  TRUE
             MOVE  WRK-FS-ARQORIGN    TO  FRWKGARQ-FILE-STATUS
             MOVE  'ARQRECEB'         TO  FRWKGARQ-NOME-ARQUIVO
             DISPLAY '***================================***'
             DISPLAY '*              SCSB001               *'
             DISPLAY '*------------------------------------*'
             DISPLAY '*                                    *'
             DISPLAY '*   QUANTIDADE INFORMADA NO TRAILLER *'
             DISPLAY '*      DIFERENTE DA QUANTIDADE DE    *'
             DISPLAY '*          REGISTROS LIDOS           *'
             DISPLAY '*                                    *'
             DISPLAY '***================================***'
             MOVE  04                    TO  RETURN-CODE
             PERFORM 9000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       3300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * GRAVAR ARQUIVO DE SAIDA ARQORIGN.                              *
      *----------------------------------------------------------------*
       3600-GRAVAR-ARQORIGN            SECTION.
      *----------------------------------------------------------------*

           MOVE '3600-GRAVAR-ARQORIGN' TO  FRWKGHEA-IDEN-PARAGRAFO

           INITIALIZE SCSB89-REG

           MOVE WRK-SEQ                TO  SCSB89-NSEQ

           MOVE SCSB99-REG             TO  SCSB89-REG(10:)

           SET   ARQ-WRITE             TO  TRUE

           WRITE FD-ARQORIGN           FROM SCSB89-REG

           PERFORM 1120-TESTAR-FS-ARQORIGN

           ADD   1                     TO  ACU-GRAVS-ARQORIGN.

      *----------------------------------------------------------------*
       3600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * GRAVAR ARQUIVO DE SAIDA ARQPRINC.                              *
      *----------------------------------------------------------------*
       3610-GRAVAR-ARQPRINC            SECTION.
      *----------------------------------------------------------------*

           MOVE '3610-GRAVAR-ARQPRINC' TO  FRWKGHEA-IDEN-PARAGRAFO

           INITIALIZE SCSB90-REG
           MOVE WRK-SEQ                TO SCSB90-NSEQ
           MOVE SCSB99-TPO-SERVC       TO WRK-TPO-SERVC
           MOVE WRK-TPO-SERVC-X        TO SCSB90-TPO-SERVC-X
           MOVE SCSB99-NPROCS          TO SCSB90-NPROCS
           MOVE SCSB99-NREG            TO WRK-NREG
           MOVE WRK-NREG-X             TO SCSB90-NREG-X
           MOVE SCSB99-CNPJ-PRINC      TO WRK-CNPJ-PRINC
           MOVE WRK-CNPJ-PRINC-X       TO SCSB90-CNPJ-PRINC-X
           MOVE SCSB99-CNPJ-FLIAL      TO WRK-CNPJ-FLIAL
           MOVE WRK-CNPJ-FLIAL-X       TO SCSB90-CNPJ-FLIAL-X
           MOVE SCSB99-CNPJ-CTRL       TO WRK-CNPJ-CTRL
           MOVE WRK-CNPJ-CTRL-X        TO SCSB90-CNPJ-CTRL-X
           MOVE UORGWB97-S-CPSSOA-JURID
                                       TO SCSB90-EMPR-DEPEND

           MOVE UORGWB88-S-NSEQ-UND-ORGNZ
                                       TO SCSB90-DEPEND
           MOVE SCSB99-NOME            TO SCSB90-NOME
           MOVE SCSB99-ENDERECO        TO SCSB90-ENDERECO
           MOVE SCSB99-CD-PAIS         TO WRK-CD-PAIS-RF
           MOVE WRK-CD-PAIS-RF-X       TO SCSB90-CD-PAIS-RF-X
           MOVE SCSB99-NIF             TO SCSB90-NIF
           MOVE SCSB99-CD-MOEDA        TO WRK-CD-MOEDA
           MOVE WRK-CD-MOEDA-X         TO SCSB90-CD-MOEDA-X
           MOVE SCSB99-VLR-CONVERT     TO WRK-VLR-CONVERT
           MOVE WRK-VLR-CONVERT-X      TO SCSB90-VLR-CONVERT-X
           MOVE SCSB99-INF-COMPLEM     TO SCSB90-INF-COMPLEM

           SET   ARQ-WRITE             TO  TRUE

           WRITE FD-ARQPRINC           FROM SCSB90-REG

           PERFORM 1121-TESTAR-FS-ARQPRINC

           ADD   1                     TO  ACU-GRAVS-ARQPRINC.

      *----------------------------------------------------------------*
       3610-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * GRAVAR ARQUIVO DE SAIDA ARQOPER.                               *
      *----------------------------------------------------------------*
       3620-GRAVAR-ARQOPER            SECTION.
      *----------------------------------------------------------------*

           MOVE '3620-GRAVAR-ARQOPER' TO  FRWKGHEA-IDEN-PARAGRAFO

           INITIALIZE SCSB91-REG

            MOVE WRK-SEQ               TO SCSB91-NSEQ
            MOVE SCSB99-TPO-SERVC      TO WRK-TPO-SERVC
            MOVE WRK-TPO-SERVC-X       TO SCSB91-TPO-SERVC-X
            MOVE SCSB99-NPROCS         TO SCSB91-NPROCS
            MOVE SCSB99-NREG           TO WRK-NREG
            MOVE WRK-NREG-X            TO SCSB91-NREG-X
            MOVE SCSB99-CD-OPER OF SCSB99-OPERACAO
                                       TO WRK-CD-OPER
            MOVE WRK-CD-OPER-X         TO SCSB91-CD-OPER-X
            MOVE SCSB99-CD-PAIS-DESTNO TO WRK-CD-PAIS-DESTNO
            MOVE WRK-CD-PAIS-DESTNO-X  TO SCSB91-CD-PAIS-DESTNO-X
            MOVE SCSB99-CD-NBS         TO SCSB91-CD-NBS
            MOVE SCSB99-CD-MODO-PREST  TO WRK-CD-MODO-PREST
            MOVE WRK-CD-MODO-PREST-X   TO SCSB91-CD-MODO-PREST-X
            MOVE SCSB99-DT-INICIO      TO SCSB91-DT-INICIO
            MOVE SCSB99-DT-CONCLUSAO   TO SCSB91-DT-CONCLUSAO
            MOVE SCSB99-VLR-OPER       TO WRK-VLR-OPER
            MOVE WRK-VLR-OPER-X        TO SCSB91-VLR-OPER-X

           SET   ARQ-WRITE             TO  TRUE

           WRITE FD-ARQOPER            FROM SCSB91-REG

           PERFORM 1122-TESTAR-FS-ARQOPER

           ADD   1                     TO  ACU-GRAVS-ARQOPER.

      *----------------------------------------------------------------*
       3620-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * GRAVAR ARQUIVO DE SAIDA ARQENQUA.                              *
      *----------------------------------------------------------------*
       3630-GRAVAR-ARQENQUA            SECTION.
      *----------------------------------------------------------------*

           MOVE '3630-GRAVAR-ARQENQUA' TO  FRWKGHEA-IDEN-PARAGRAFO

           INITIALIZE SCSB92-REG

           MOVE WRK-SEQ                TO SCSB92-NSEQ
           MOVE SCSB99-TPO-SERVC       TO WRK-TPO-SERVC
           MOVE WRK-TPO-SERVC-X        TO SCSB92-TPO-SERVC-X
           MOVE SCSB99-NPROCS          TO SCSB92-NPROCS
           MOVE SCSB99-NREG            TO WRK-NREG
           MOVE WRK-NREG-X             TO SCSB92-NREG-X
           MOVE SCSB99-CD-OPER OF SCSB99-ENQUADRAMENTO
                                       TO WRK-CD-OPER
           MOVE WRK-CD-OPER-X          TO SCSB92-CD-OPER-X
           MOVE SCSB99-CD-ENQUA        TO WRK-CD-ENQUA
           MOVE WRK-CD-ENQUA-X         TO SCSB92-CD-ENQUA-X
           MOVE SCSB99-RC              TO WRK-RC
           MOVE WRK-RC-X               TO SCSB92-RC-X

           SET   ARQ-WRITE             TO  TRUE

           WRITE FD-ARQENQUA            FROM SCSB92-REG

           PERFORM 1123-TESTAR-FS-ARQENQUA

           ADD   1                     TO  ACU-GRAVS-ARQENQUA.

      *----------------------------------------------------------------*
       3630-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * GRAVAR ARQUIVO DE SAIDA ARQREDI.                               *
      *----------------------------------------------------------------*
       3640-GRAVAR-ARQREDI            SECTION.
      *----------------------------------------------------------------*

           MOVE '3640-GRAVAR-ARQREDI'  TO  FRWKGHEA-IDEN-PARAGRAFO

           INITIALIZE SCSB93-REG

           MOVE WRK-SEQ                TO SCSB93-NSEQ
           MOVE SCSB99-TPO-SERVC       TO WRK-TPO-SERVC
           MOVE WRK-TPO-SERVC-X        TO SCSB93-TPO-SERVC-X
           MOVE SCSB99-NPROCS          TO SCSB93-NPROCS
           MOVE SCSB99-NREG            TO WRK-NREG
           MOVE WRK-NREG-X             TO SCSB93-NREG-X
           IF   SCSB99-TPO-REG EQUAL 4
               MOVE SCSB99-CD-RE OF SCSB99-RE
                                       TO WRK-CD-RE
               MOVE WRK-CD-RE-X        TO SCSB93-CD-RE-DI-X
           ELSE
               MOVE SCSB99-CD-DI OF SCSB99-DI
                                       TO WRK-CD-DI
               MOVE WRK-CD-DI-X        TO SCSB93-CD-RE-DI-X
           END-IF

           SET   ARQ-WRITE             TO  TRUE

           WRITE FD-ARQREDI            FROM SCSB93-REG

           PERFORM 1124-TESTAR-FS-ARQREDI

           ADD   1                     TO  ACU-GRAVS-ARQREDI.

      *----------------------------------------------------------------*
       3640-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * GRAVAR ARQUIVO DE SAIDA ARQFATPG.                              *
      *----------------------------------------------------------------*
       3650-GRAVAR-ARQFATPG            SECTION.
      *----------------------------------------------------------------*

           MOVE '3650-GRAVAR-ARQFATPG' TO  FRWKGHEA-IDEN-PARAGRAFO

           INITIALIZE SCSB94-REG

           MOVE WRK-SEQ                TO SCSB94-NSEQ
           MOVE SCSB99-TPO-SERVC       TO WRK-TPO-SERVC
           MOVE WRK-TPO-SERVC-X        TO SCSB94-TPO-SERVC-X
           MOVE UORGWB97-S-CPSSOA-JURID
                                       TO SCSB94-EMPR-DEPEND
           MOVE UORGWB88-S-NSEQ-UND-ORGNZ
                                       TO SCSB94-DEPEND
           MOVE SCSB99-NPROCS          TO SCSB94-NPROCS

           MOVE SCSB99-CD-FATMT-PGTO OF SCSB99-FATMT-PGTO
                                       TO WRK-CD-FATMT-PGTO
           MOVE WRK-CD-FATMT-PGTO-X    TO SCSB94-CD-FATMT-PGTO-X

           MOVE SCSB99-NOTA-FISCAL     TO SCSB94-NOTA-FISCAL
           MOVE SCSB99-DT-FATMT-PGTO   TO SCSB94-DT-FATMT-PGTO



           SET   ARQ-WRITE             TO  TRUE

           WRITE FD-ARQFATPG           FROM SCSB94-REG

           PERFORM 1125-TESTAR-FS-ARQFATPG

           ADD   1                     TO  ACU-GRAVS-ARQFATPG.

      *----------------------------------------------------------------*
       3650-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * GRAVAR ARQUIVO DE SAIDA FTPGOPER.                              *
      *----------------------------------------------------------------*
       3660-GRAVAR-FTPGOPER            SECTION.
      *----------------------------------------------------------------*

           MOVE '3660-GRAVAR-FTPGOPER' TO  FRWKGHEA-IDEN-PARAGRAFO

           INITIALIZE SCSB95-REG

           MOVE WRK-SEQ                TO SCSB95-NSEQ
           MOVE SCSB99-TPO-SERVC       TO WRK-TPO-SERVC
           MOVE WRK-TPO-SERVC-X        TO SCSB95-TPO-SERVC-X
           MOVE SCSB99-NPROCS          TO SCSB95-NPROCS
           MOVE SCSB99-NREG            TO WRK-NREG
           MOVE WRK-NREG-X             TO SCSB95-NREG-X
           MOVE SCSB99-CD-FATMT-PGTO OF SCSB99-OPER-FATMT-PGTO
                                       TO WRK-CD-FATMT-PGTO
           MOVE WRK-CD-FATMT-PGTO-X    TO SCSB95-CD-FATMT-PGTO-X

           MOVE SCSB99-CD-OPER OF SCSB99-OPER-FATMT-PGTO
                                       TO WRK-CD-OPER
           MOVE WRK-CD-OPER-X          TO SCSB95-CD-OPER-X
           MOVE SCSB99-VLR-FATMT-PGTO  TO WRK-VLR-FATMT-PGTO
           MOVE WRK-VLR-FATMT-PGTO-X   TO SCSB95-VLR-FATMT-PGTO-X
           MOVE SCSB99-VLR-MANTD-EXTR  TO WRK-VLR-MANTD-EXTR
           MOVE WRK-VLR-MANTD-EXTR-X   TO SCSB95-VLR-MANTD-EXTR-X

           SET   ARQ-WRITE             TO  TRUE

           WRITE FD-FTPGOPER           FROM SCSB95-REG

           PERFORM 1126-TESTAR-FS-FTPGOPER

           ADD   1                     TO  ACU-GRAVS-FTPGOPER.

      *----------------------------------------------------------------*
       3660-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * GRAVAR ARQUIVO DE SAIDA FTPGREDI.                              *
      *----------------------------------------------------------------*
       3670-GRAVAR-FTPGREDI            SECTION.
      *----------------------------------------------------------------*

           MOVE '3670-GRAVAR-FTPGREDI' TO  FRWKGHEA-IDEN-PARAGRAFO

           INITIALIZE SCSB96-REG

           MOVE WRK-SEQ                TO SCSB96-NSEQ
           MOVE SCSB99-TPO-SERVC       TO WRK-TPO-SERVC
           MOVE WRK-TPO-SERVC-X        TO SCSB96-TPO-SERVC-X
           MOVE SCSB99-NPROCS          TO SCSB96-NPROCS
           MOVE SCSB99-NREG            TO WRK-NREG
           MOVE WRK-NREG-X             TO SCSB96-NREG-X
           MOVE SCSB99-TPO-REG         TO WRK-TPO-REG
           MOVE WRK-TPO-REG-X          TO SCSB96-TPO-REG-X
           IF   SCSB99-TPO-REG EQUAL 8
               MOVE SCSB99-CD-FATMT-PGTO OF SCSB99-RE-FATMT-PGTO
                                       TO WRK-CD-FATMT-PGTO
               MOVE WRK-CD-FATMT-PGTO-X
                                       TO SCSB96-CD-FATMT-PGTO-X
               MOVE SCSB99-CD-RE  OF SCSB99-RE-FATMT-PGTO
                                       TO WRK-CD-RE
               MOVE WRK-CD-RE-X        TO SCSB96-CD-RE-DI-X
           ELSE
               MOVE SCSB99-CD-FATMT-PGTO OF SCSB99-DI-FATMT-PGTO
                                       TO WRK-CD-FATMT-PGTO
               MOVE WRK-CD-FATMT-PGTO-X
                                       TO SCSB96-CD-FATMT-PGTO-X
               MOVE SCSB99-CD-DI  OF SCSB99-DI-FATMT-PGTO
                                       TO WRK-CD-DI
               MOVE WRK-CD-DI-X        TO SCSB96-CD-RE-DI-X
           END-IF

           SET   ARQ-WRITE             TO  TRUE

           WRITE FD-FTPGREDI           FROM SCSB96-REG

           PERFORM 1127-TESTAR-FS-FTPGREDI

           ADD   1                     TO  ACU-GRAVS-FTPGREDI.

      *----------------------------------------------------------------*
       3670-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * GRAVAR ARQUIVO DE SAIDA TPREGNOK.                              *
      *----------------------------------------------------------------*
       3680-GRAVAR-TPREGNOK            SECTION.
      *----------------------------------------------------------------*

           MOVE '3680-GRAVAR-TPREGNOK' TO  FRWKGHEA-IDEN-PARAGRAFO

           INITIALIZE SCSB97-REG

           MOVE WRK-SEQ                TO SCSB97-NSEQ
           MOVE SCSB99-TPO-SERVC       TO WRK-TPO-SERVC
           MOVE WRK-TPO-SERVC-X        TO SCSB97-TPO-SERVC-X
           MOVE SCSB99-NPROCS          TO SCSB97-NPROCS
           MOVE SCSB99-NREG            TO WRK-NREG
           MOVE WRK-NREG-X             TO SCSB97-NREG-X
           MOVE SCSB99-TPO-REG         TO WRK-TPO-REG
           MOVE WRK-TPO-REG-X          TO SCSB97-TPO-REG-X
           MOVE UORGWB97-S-CPSSOA-JURID
                                       TO SCSB97-EMPR-DEPEND
           MOVE UORGWB88-S-NSEQ-UND-ORGNZ
                                       TO SCSB97-DEPEND

           SET   ARQ-WRITE             TO  TRUE

           WRITE FD-TPREGNOK           FROM SCSB97-REG

           PERFORM 1128-TESTAR-FS-TPREGNOK

           ADD   1                     TO  ACU-GRAVS-TPREGNOK.

      *----------------------------------------------------------------*
       3680-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * BUSCA ENMPRESA SAP                                             *
      *----------------------------------------------------------------*
       7000-CALL-UORG2397              SECTION.
      *----------------------------------------------------------------*

           MOVE  '7000-CALL-UORG2397'      TO
                                           FRWKGHEA-IDEN-PARAGRAFO.

           INITIALIZE  UORGWB97-REGISTRO.

           MOVE    'D'                     TO      UORGWB97-E-AMBIENTE
           MOVE    SCSB99-EMPR-DEPEND      TO
                                           UORGWB97-E-CEMPR-SAP.
           MOVE    'UORG2397'              TO      WRK-MODULO

           CALL    WRK-MODULO              USING   WRK-AREA-UORG2397

           IF      UORGWB97-COD-RETORNO  NOT EQUAL ZEROS AND 08
                   SET  ERRO-MODULO        TO      TRUE
                   MOVE WRK-MODULO         TO      FRWKGMOD-NOME-MODULO
                   MOVE UORGWB97-COD-RETORNO
                                           TO      FRWKGMOD-COD-RETORNO
                   PERFORM 9999-FINALIZAR-ERRO
           END-IF.

           IF      UORGWB97-COD-RETORNO  EQUAL 08
             DISPLAY 'UORGWB97-E-AMBIENTE:'   UORGWB97-E-AMBIENTE
             DISPLAY 'UORGWB97-E-CEMPR-SAP: ' UORGWB97-E-CEMPR-SAP
             DISPLAY '***================================***'
             DISPLAY '*              SCSB001               *'
             DISPLAY '*------------------------------------*'
             DISPLAY '*                                    *'
             DISPLAY '* EMPRESA NAO ENCONTRADA NO UORG2397 *'
             DISPLAY '*                                    *'
             DISPLAY '***================================***'
             MOVE  04                    TO  RETURN-CODE
             PERFORM 9000-FINALIZAR

           END-IF.

      *----------------------------------------------------------------*
       7000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * BUSCA DEPENDENCIA SAP                                          *
      *----------------------------------------------------------------*
       8000-CALL-UORG2389              SECTION.
      *----------------------------------------------------------------*

           MOVE  '8000-CALL-UORG2389'      TO
                                           FRWKGHEA-IDEN-PARAGRAFO.

           INITIALIZE  UORGWB88-REGISTRO.

           MOVE '2'                        TO UORGWB88-E-CTPO-PESQUISA
           MOVE UORGWB97-S-CPSSOA-JURID    TO UORGWB88-E-CPSSOA-JURID
           MOVE ZEROS                      TO UORGWB88-E-NSEQ-UND-ORGNZ
           MOVE 1                          TO UORGWB88-E-CIDTFD-TPO-UND
           MOVE SCSB99-DEPEND              TO WRK-DEPEND-R
           MOVE WRK-DEPEND-X               TO UORGWB88-E-CRCONH-UND(1:6)

           MOVE    'UORG2389'              TO      WRK-MODULO

           CALL    WRK-MODULO              USING   WRK-AREA-UORG2389

           IF      UORGWB88-COD-RETORNO  NOT EQUAL ZEROS AND 08
                   SET  ERRO-MODULO        TO      TRUE
                   MOVE WRK-MODULO         TO      FRWKGMOD-NOME-MODULO
                   MOVE UORGWB88-COD-RETORNO
                                           TO      FRWKGMOD-COD-RETORNO
                   PERFORM 9999-FINALIZAR-ERRO
           END-IF.

           IF      UORGWB88-COD-RETORNO  EQUAL 08
             DISPLAY 'UORGWB88-E-CTPO-PESQUISA:'
                                           UORGWB88-E-CTPO-PESQUISA
             DISPLAY 'UORGWB88-E-CPSSOA-JURID: ' UORGWB88-E-CPSSOA-JURID
             DISPLAY 'UORGWB88-E-NSEQ-UND-ORGNZ:'
                                       UORGWB88-E-NSEQ-UND-ORGNZ
             DISPLAY 'UORGWB88-E-CIDTFD-TPO-UND: '
                                       UORGWB88-E-CIDTFD-TPO-UND
             DISPLAY 'UORGWB88-E-CRCONH-UND: '
                                           UORGWB88-E-CRCONH-UND(1:6)

             DISPLAY '***================================***'
             DISPLAY '*              SCSB001               *'
             DISPLAY '*------------------------------------*'
             DISPLAY '*                                    *'
             DISPLAY '*    DEPENDENCIA NAO ENCONTRADA NO   *'
             DISPLAY '*              UORG2389              *'
             DISPLAY '*                                    *'
             DISPLAY '***================================***'
             MOVE  04                    TO  RETURN-CODE
             PERFORM 9000-FINALIZAR

           END-IF.

      *----------------------------------------------------------------*
       8000-99-FIM.                    EXIT.
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
           IF RETURN-CODE EQUAL ZEROS
               PERFORM 9100-EXIBIR-ESTATISTICA
           END-IF

      *
      *--> "FECHAR ARQUIVOS"
      *

           SET  ARQ-CLOSE              TO  TRUE

           CLOSE  ARQRECEB
                  ARQORIGN
                  ARQPRINC
                  ARQOPER
                  ARQENQUA
                  ARQREDI
                  ARQFATPG
                  FTPGOPER
                  FTPGREDI
                  TPREGNOK

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

           MOVE   'ARQRECEB'                     TO  WRK-DDNAME
           MOVE   'I'                            TO  WRK-I-O
           MOVE   'TOTAL LIDOS                '  TO  WRK-DESCARQ
           MOVE    ACU-LIDOS-ARQRECEB            TO  WRK-QTDEARQ
           DISPLAY WRK-DISP9

           MOVE   'ARQORIGN'                     TO  WRK-DDNAME
           MOVE   'O'                            TO  WRK-I-O
           MOVE   'TOTAL GRAVADOS             '  TO  WRK-DESCARQ
           MOVE    ACU-GRAVS-ARQORIGN            TO  WRK-QTDEARQ
           DISPLAY WRK-DISP9

           MOVE   'ARQPRINC'                     TO  WRK-DDNAME
           MOVE   'O'                            TO  WRK-I-O
           MOVE   'TOTAL GRAVADOS             '  TO  WRK-DESCARQ
           MOVE    ACU-GRAVS-ARQPRINC            TO  WRK-QTDEARQ
           DISPLAY WRK-DISP9

           MOVE   'ARQOPER'                      TO  WRK-DDNAME
           MOVE   'O'                            TO  WRK-I-O
           MOVE   'TOTAL GRAVADOS             '  TO  WRK-DESCARQ
           MOVE    ACU-GRAVS-ARQOPER             TO  WRK-QTDEARQ
           DISPLAY WRK-DISP9

           MOVE   'ARQENQUA'                     TO  WRK-DDNAME
           MOVE   'O'                            TO  WRK-I-O
           MOVE   'TOTAL GRAVADOS             '  TO  WRK-DESCARQ
           MOVE    ACU-GRAVS-ARQENQUA            TO  WRK-QTDEARQ
           DISPLAY WRK-DISP9

           MOVE   'ARQREDI'                      TO  WRK-DDNAME
           MOVE   'O'                            TO  WRK-I-O
           MOVE   'TOTAL GRAVADOS             '  TO  WRK-DESCARQ
           MOVE    ACU-GRAVS-ARQREDI             TO  WRK-QTDEARQ
           DISPLAY WRK-DISP9

           MOVE   'ARQFATPG'                     TO  WRK-DDNAME
           MOVE   'O'                            TO  WRK-I-O
           MOVE   'TOTAL GRAVADOS             '  TO  WRK-DESCARQ
           MOVE    ACU-GRAVS-ARQFATPG            TO  WRK-QTDEARQ
           DISPLAY WRK-DISP9

           MOVE   'FTPGOPER'                     TO  WRK-DDNAME
           MOVE   'O'                            TO  WRK-I-O
           MOVE   'TOTAL GRAVADOS             '  TO  WRK-DESCARQ
           MOVE    ACU-GRAVS-FTPGOPER            TO  WRK-QTDEARQ
           DISPLAY WRK-DISP9

           MOVE   'FTPGREDI'                     TO  WRK-DDNAME
           MOVE   'O'                            TO  WRK-I-O
           MOVE   'TOTAL GRAVADOS             '  TO  WRK-DESCARQ
           MOVE    ACU-GRAVS-FTPGREDI            TO  WRK-QTDEARQ
           DISPLAY WRK-DISP9

           MOVE   'TPREGNOK'                     TO  WRK-DDNAME
           MOVE   'O'                            TO  WRK-I-O
           MOVE   'TOTAL GRAVADOS             '  TO  WRK-DESCARQ
           MOVE    ACU-GRAVS-TPREGNOK            TO  WRK-QTDEARQ
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


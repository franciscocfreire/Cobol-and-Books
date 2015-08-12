      *===============================================================*
       IDENTIFICATION DIVISION.
      *===============================================================*

       PROGRAM-ID. RDAB0903.
       AUTHOR. FRANCISCO.

      *===============================================================*
      *                 B R Q    I T   S E R V I C E S                *
      *---------------------------------------------------------------*
      *                                                               *
      *   PROGRAMA    : RDAB0903                                      *
      *   PROGRAMADOR : FRANCISCO FREIRE       - BRQ IT SERVICES      *
      *   ANALISTA    : FRANCISCO FREIRE       - BRQ IT SERVICES      *
      *   SUPERVISOR  : JUNIOR RIBAMAR         - GP. 70               *
      *   DATA        : ABRIL/2014                                    *
      *                                                               *
      *   OBJETIVO    : BALANCE LINE PARA MANTER O MELHOR CADASTRO DO *
      *                 CADU                                          *
      *   ARQUIVOS:                                                   *
      *   ---------------------------------------------------------   *
      *   DDNAME   |I/O| DESCRICAO               | BOOK     | LRECL   *
      *   ---------+---+-------------------------+----------+------   *
      *   CADUV001 | I | PENDENCIA RENEGOCIADAS  | I#RDABAR |   570   *
      *   CADUV000 | I | INFO PROFISSIONAIS      | I#RDABAP |   320   *
      *   ARQSAIDA | O | DADOS PRINCIPAIS        | I#RDABAR |   570   *
      *   ---------------------------------------------------------   *

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

           SELECT  CADUV001  ASSIGN  TO  UT-S-CADUV001
                   FILE      STATUS  IS  WRK-FS-CADUV001.

           SELECT  CADUV000  ASSIGN  TO  UT-S-CADUV000
                   FILE      STATUS  IS  WRK-FS-CADUV000.

           SELECT  ARQSAIDA  ASSIGN  TO  UT-S-ARQSAIDA
                   FILE      STATUS  IS  WRK-FS-ARQSAIDA.

           SELECT  ARQDESPR  ASSIGN  TO  UT-S-ARQDESPR
                   FILE      STATUS  IS  WRK-FS-ARQDESPR.

      *===============================================================*
       DATA DIVISION.
      *===============================================================*

      *---------------------------------------------------------------*
       FILE SECTION.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
      *  INPUT....: ARQ. DADOS PRINCIPAIS                             *
      *             ORG. SEQUENCIAL    -  LRECL = 570 BYTES           *
      *---------------------------------------------------------------*

       FD  CADUV001
           LABEL       RECORD    STANDARD
           RECORDING   MODE      F
           BLOCK       CONTAINS  0.

       COPY 'I#RDABAR'.
      *---------------------------------------------------------------*
      *  INPUT....: ARQ. ENDERECO                                     *
      *             ORG. SEQUENCIAL    -  LRECL = 320 BYTES           *
      *---------------------------------------------------------------*

       FD  CADUV000
           LABEL       RECORD    STANDARD
           RECORDING   MODE      F
           BLOCK       CONTAINS  0.

       COPY 'I#RDABAP'.
      *---------------------------------------------------------------*
      *  OUTPUT...: ARQ. DE DADOS DA PESSOA                           *
      *             ORG. SEQUENCIAL   -   LRECL = 570 BYTES           *
      *---------------------------------------------------------------*

       FD  ARQSAIDA
           LABEL       RECORD    STANDARD
           RECORDING   MODE      F
           BLOCK       CONTAINS  0.


       COPY 'I#RDABAR'.

      *---------------------------------------------------------------*
      *  OUTPUT...: ARQ. DESPREZADOS                                  *
      *             ORG. SEQUENCIAL   -   LRECL = 570 BYTES           *
      *---------------------------------------------------------------*

       FD  ARQDESPR
           LABEL       RECORD    STANDARD
           RECORDING   MODE      F
           BLOCK       CONTAINS  0.


       COPY 'I#RDABAR'.

      *---------------------------------------------------------------*
       WORKING-STORAGE SECTION.
      *---------------------------------------------------------------*
       01   FILLER                     PIC  X(32) VALUE
            'INICIO DA WORKING RDAB0903'.

      *-- ------------------------------------------------------------*
      *          CAMPOS UTILIZADOS PARA CANCELAR PROCESSAMENTO        *
      *---------------------------------------------------------------*

       01  FILLER.
           03  WRK-FS-CADUV001         PIC  X(02) VALUE SPACES.
           03  WRK-FS-CADUV000         PIC  X(02) VALUE SPACES.
           03  WRK-FS-ARQSAIDA         PIC  X(02) VALUE SPACES.
           03  WRK-FS-ARQDESPR         PIC  X(02) VALUE SPACES.
           03  WRK-OPERACAO            PIC  X(13) VALUE SPACES.
           03  WRK-ABERTURA            PIC  X(13) VALUE 'NA ABERTURA'.
           03  WRK-LEITURA             PIC  X(13) VALUE 'NA LEITURA'.
           03  WRK-GRAVACAO            PIC  X(13) VALUE 'NA GRAVACAO'.
           03  WRK-FECHAMENTO          PIC  X(13) VALUE 'NO FECHAMENTO'.
           03  WRK-BATCH               PIC  X(08) VALUE 'BATCH'.
      *---------------------------------------------------------------*
      *--  ACUMULADORES.

       01  FILLER.
           03  ACU-LDS-CADUV001        PIC 9(09) COMP-3    VALUE  ZEROS.
           03  ACU-LDS-CADUV000        PIC 9(09) COMP-3    VALUE  ZEROS.
           03  ACU-GRV-ARQSAIDA        PIC 9(09) COMP-3    VALUE  ZEROS.
           03  ACU-GRV-ARQDESPR        PIC 9(09) COMP-3    VALUE  ZEROS.
      *
      *----------------------------------------------------------------*
      *--  EDICAO.

       01  FILLER.
           03  WRK-EDIT01              PIC ZZZ.ZZZ.ZZ9     VALUE  ZEROS.
           03  WRK-EDIT02              PIC ZZZ.ZZZ.ZZ9     VALUE  ZEROS.
           03  WRK-EDIT05              PIC ZZZ.ZZZ.ZZ9     VALUE  ZEROS.
           03  WRK-EDIT06              PIC ZZZ.ZZZ.ZZ9     VALUE  ZEROS.

      *----------------------------------------------------------------*
      *--  AREA AUXILIAR.

      *--  CHAVE CADUV001.
       01  WRK-CHV-CADUV001.
           03  WRK-CHV-001-CCLUB       PIC 9(10) VALUE  ZEROS.

      *--  CHAVE CADUV000.
       01  WRK-CHV-CADUV000.
           03  WRK-CHV-000-CCLUB       PIC 9(10) VALUE  ZEROS.

      *----------------------------------------------------------------*
       01  FILLER   PIC  X(32) VALUE '*       AREA DA BRAD7100       *'.
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *---------------------------------------------------------------*
       01   FILLER                     PIC  X(32)    VALUE
            'FIM DA WORKING RDAB0903'.

      *===============================================================*
       PROCEDURE DIVISION.
      *===============================================================*

      *---------------------------------------------------------------*
       0000-INICIAR SECTION.
      *---------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-LER-CADUV001
           IF      WRK-FS-CADUV001     EQUAL '10'
                   DISPLAY '**************** RDAB0903 ***************'
                   DISPLAY '*                                       *'
                   DISPLAY '* ARQUIVO DE ENTRADA - CADUV001 - VAZIO *'
                   DISPLAY '*        PROCESSAMENTO ENCERRADO        *'
                   DISPLAY '*                                       *'
                   DISPLAY '**************** RDAB0903 ***************'
                   PERFORM  9000-FINALIZAR
           END-IF.

           PERFORM 3000-LER-CADUV000
           IF      WRK-FS-CADUV000     EQUAL '10'
                   DISPLAY '**************** RDAB0903 ***************'
                   DISPLAY '*                                       *'
                   DISPLAY '* ARQUIVO DE ENTRADA - CADUV000 - VAZIO *'
                   DISPLAY '*        PROCESSAMENTO ENCERRADO        *'
                   DISPLAY '*                                       *'
                   DISPLAY '**************** RDAB0903 ***************'
           END-IF

           PERFORM 4000-PROCESSAR     UNTIL
                  (WRK-FS-CADUV001     EQUAL '10')


           PERFORM 7000-DISPLAY-TOTAIS

           PERFORM 9000-FINALIZAR.

      *---------------------------------------------------------------*
       0000-99-FIM.   EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1000-INICIALIZAR              SECTION.
      *---------------------------------------------------------------*

           OPEN  INPUT  CADUV001
                        CADUV000
                 OUTPUT ARQSAIDA
                        ARQDESPR

           MOVE  WRK-ABERTURA          TO    WRK-OPERACAO
           PERFORM  1100-TESTAR-FILE-STATUS.

      *---------------------------------------------------------------*
       1000-99-FIM.   EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS       SECTION.
      *---------------------------------------------------------------*

           PERFORM  1110-TESTAR-FS-CADUV001
           PERFORM  1120-TESTAR-FS-CADUV000
           PERFORM  1150-TESTAR-FS-ARQSAIDA
           PERFORM  1160-TESTAR-FS-ARQDESPR.

      *---------------------------------------------------------------*
       1100-99-FIM.   EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1110-TESTAR-FS-CADUV001       SECTION.
      *---------------------------------------------------------------*

           IF WRK-FS-CADUV001         NOT EQUAL  '00'
              DISPLAY '************** RDAB0903 *************'
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
              DISPLAY '*              CADUV001             *'
              DISPLAY '*         FILE STATUS =  ' WRK-FS-CADUV001
                                                 '         *'
              DISPLAY '************** RDAB0903 *************'
              PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       1110-99-FIM.   EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1120-TESTAR-FS-CADUV000       SECTION.
      *---------------------------------------------------------------*

           IF WRK-FS-CADUV000         NOT EQUAL  '00'
              DISPLAY '************** RDAB0903 *************'
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
              DISPLAY '*              CADUV000             *'
              DISPLAY '*         FILE STATUS =  ' WRK-FS-CADUV000
                                                 '         *'
              DISPLAY '************** RDAB0903 *************'
              PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       1120-99-FIM.   EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1150-TESTAR-FS-ARQSAIDA        SECTION.
      *---------------------------------------------------------------*

           IF WRK-FS-ARQSAIDA           NOT EQUAL  '00'
              DISPLAY '************** RDAB0903 *************'
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
              DISPLAY '*              ARQSAIDA              *'
              DISPLAY '*         FILE STATUS =  ' WRK-FS-ARQSAIDA
                                                 '         *'
              DISPLAY '************** RDAB0903 *************'
              PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       1150-99-FIM.   EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1160-TESTAR-FS-ARQDESPR        SECTION.
      *---------------------------------------------------------------*

           IF WRK-FS-ARQDESPR           NOT EQUAL  '00'
              DISPLAY '************** RDAB0903 *************'
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
              DISPLAY '*              ARQSAIDA              *'
              DISPLAY '*         FILE STATUS =  ' WRK-FS-ARQDESPR
                                                 '         *'
              DISPLAY '************** RDAB0903 *************'
              PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       1150-99-FIM.   EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2000-LER-CADUV001            SECTION.
      *---------------------------------------------------------------*

           READ    CADUV001

           IF      WRK-FS-CADUV001     EQUAL  '10'
                   MOVE HIGH-VALUES    TO      WRK-CHV-CADUV001
                   GO                  TO      2000-99-FIM
           END-IF.

           MOVE    WRK-LEITURA         TO      WRK-OPERACAO
           PERFORM 1110-TESTAR-FS-CADUV001

           MOVE CADUV001-CCLUB OF CADUV001 TO  WRK-CHV-001-CCLUB.

           ADD     1                      TO  ACU-LDS-CADUV001.

      *---------------------------------------------------------------*
       2000-99-FIM.   EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3000-LER-CADUV000            SECTION.
      *---------------------------------------------------------------*

           READ    CADUV000

           IF      WRK-FS-CADUV000     EQUAL  '10'
                   MOVE HIGH-VALUES    TO      WRK-CHV-CADUV000
                   GO                  TO      3000-99-FIM
           END-IF.

           MOVE    WRK-LEITURA         TO      WRK-OPERACAO
           PERFORM 1120-TESTAR-FS-CADUV000

           MOVE CADUV000-CCLUB         TO  WRK-CHV-000-CCLUB.

           ADD     1                   TO      ACU-LDS-CADUV000.

      *---------------------------------------------------------------*
       3000-99-FIM.   EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       4000-PROCESSAR               SECTION.
      *---------------------------------------------------------------*

           IF      WRK-CHV-CADUV001   GREATER   WRK-CHV-CADUV000
                   PERFORM 3000-LER-CADUV000
           ELSE
              IF      WRK-CHV-CADUV001    LESS    WRK-CHV-CADUV000

                      PERFORM 4100-CHAVES-MENOR
                      PERFORM 2000-LER-CADUV001
              ELSE
                      PERFORM 4200-CHAVES-IGUAIS
                      PERFORM 2000-LER-CADUV001
                      PERFORM 3000-LER-CADUV000
              END-IF
           END-IF.

      *---------------------------------------------------------------*
       4000-99-FIM.   EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       4100-CHAVES-MENOR            SECTION.
      *---------------------------------------------------------------*

           INITIALIZE REG-CADUV001 OF ARQDESPR.

           MOVE REG-CADUV001 OF CADUV001 TO REG-CADUV001 OF ARQDESPR.

           PERFORM 5100-GRAVAR-ARQDESPR.

      *---------------------------------------------------------------*
       4100-99-FIM.   EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       4200-CHAVES-IGUAIS           SECTION.
      *---------------------------------------------------------------*

           INITIALIZE REG-CADUV001 OF ARQSAIDA.

           MOVE CADUV000-HULT-ATULZ TO CADUV001-HULT-ATULZ OF ARQSAIDA.

           MOVE REG-CADUV001 OF CADUV001 TO REG-CADUV001 OF ARQSAIDA.

           PERFORM 5000-GRAVAR-ARQSAIDA.

      *---------------------------------------------------------------*
       4200-99-FIM.   EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       5000-GRAVAR-ARQSAIDA          SECTION.
      *---------------------------------------------------------------*

           WRITE  REG-CADUV001 OF ARQSAIDA
           MOVE   WRK-GRAVACAO         TO      WRK-OPERACAO
           PERFORM  1150-TESTAR-FS-ARQSAIDA

           ADD 1                      TO   ACU-GRV-ARQSAIDA.

      *---------------------------------------------------------------*
       5000-99-FIM.   EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       5100-GRAVAR-ARQDESPR          SECTION.
      *---------------------------------------------------------------*

           WRITE  REG-CADUV001 OF ARQDESPR
           MOVE   WRK-GRAVACAO         TO      WRK-OPERACAO
           PERFORM  1160-TESTAR-FS-ARQDESPR

           ADD 1                      TO   ACU-GRV-ARQDESPR.

      *---------------------------------------------------------------*
       5000-99-FIM.   EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       7000-DISPLAY-TOTAIS        SECTION.
      *---------------------------------------------------------------*

           MOVE     ACU-LDS-CADUV001   TO    WRK-EDIT01
           MOVE     ACU-LDS-CADUV000   TO    WRK-EDIT02
           MOVE     ACU-GRV-ARQSAIDA   TO    WRK-EDIT05
           MOVE     ACU-GRV-ARQDESPR   TO    WRK-EDIT06

           DISPLAY '******************** RDAB0903 ********************'
           DISPLAY '*                                                *'
           DISPLAY '*  TOTAL REG. LIDOS    - CADUV001 : 'WRK-EDIT01'  *'
           DISPLAY '*  TOTAL REG. LIDOS    - CADUV000 : 'WRK-EDIT02'  *'
           DISPLAY '*  TOTAL REG. GRAVADOS - ARQSAIDA : 'WRK-EDIT05'  *'
           DISPLAY '*  TOTAL REG. GRAVADOS - ARQDESPR : 'WRK-EDIT06'  *'
           DISPLAY '*                                                *'
           DISPLAY '******************** RDAB0903 ********************'.

      *---------------------------------------------------------------*
       7000-99-FIM.   EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       9000-FINALIZAR             SECTION.
      *---------------------------------------------------------------*

           CLOSE  CADUV001
                  CADUV000
                  ARQSAIDA
                  ARQDESPR.

           MOVE   WRK-FECHAMENTO       TO      WRK-OPERACAO
           PERFORM  1100-TESTAR-FILE-STATUS.

           STOP RUN.

      *---------------------------------------------------------------*
       9000-99-FIM.   EXIT.
      *---------------------------------------------------------------*

      *----------------------------------------------------------------*
       9999-ROTINA-ERRO SECTION.
      *----------------------------------------------------------------*

           MOVE   'RDAB0903'           TO  ERR-PGM.
           MOVE   'APL'                TO ERR-TIPO-ACESSO.
           CALL   'BRAD7100'        USING  WRK-BATCH
                                           ERRO-AREA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM. EXIT.
      *----------------------------------------------------------------*


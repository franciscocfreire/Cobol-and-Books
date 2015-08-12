      *================================================================*
       IDENTIFICATION                  DIVISION.                        
      *================================================================*
                                                                        
       PROGRAM-ID.                     RDAB7482.                        
       AUTHOR.                         ELSINO SILVA.                    
                                                                        
      ******************************************************************
      *               B R Q     I T     S E R V I C E S                *
      *================================================================*
      *                                                                *
      * PROGRAMA     : RDAB7482                                        *
      * PROGRAMADOR  : ELSINO SILVA                                    *
      * ANALISTA     : SANDRA H. PANSANI                               *
      * DATA         : SETEMBRO / 2013                                 *
      *                                                                *
      * PROJETO      : ---------------------------------------------   *
      *                                                                *
      * RESPONSAVEIS : MARIA AP. PRADO POI DE SOUZA - DDS              *
      *              : ROBSON VELLASQUES            - BRQ              *
      *                                                                *
      *================================================================*
      *                       O B J E T I V O :                        *
      *                                                                *
      * EFETUAR O BALANCE-LINE ENTRE OS ARQUIVOS ARQEXTRC E BDRPTRAN   *
      * GERANDO O ARQUIVO ARQSAIDA (EXTRACT DE ERROS ZERADO)           *
      *                                                                *
      * OBS: PROGRAMA BASEADO NO RDAB7480                              *
      *                                                                *
      *================================================================*
      *                       A R Q U I V O S :                        *
      *                                                                *
      *    BDRPTRAN : MX.CACS.BDRPTRAN.PSSOAJUR                        *
      *                                                                *
      *----------+---+------------------------------+----------+-------*
      *   DDNAME |I/O|         DESCRICAO            |   BOOK   | LRECL *
      *----------+---+------------------------------+----------+-------*
      * BDRPTRAN | I | ARQ. TRANSITO ERRROS DO CACS | -------- |  0396 *
      * ARQEXTRC | I | ARQ. EXTRACT DO DIA ANTERIOR | I#RDABS0 |  3400 *
      * ARQSAIDA | O | ARQ. EXTRACT ZERADO          | I#RDABS0 |  3400 *
      * BDRPSCOR | O | ARQ. TRANSITO SEM CORRESPOND.| -------- |  0396 *
      *----------+---+------------------------------+----------+-------*
      *                                                                *
      *================================================================*
      *                        M O D U L O S :                         *
      *                                                                *
      *----------+----------+------------------------------------------*
      *  MODULO  |   BOOK   |          FUNCAO                          *
      *----------+----------+------------------------------------------*
      * POL71000 | POL7100C | TRATAR ERROS                             *
      * POOL7600 | -------- | OBTER DATA E HORA DO SISTEMA             *
      * BRAD0160 | -------- | OBTER O NOME DO JOB EXECUTADO            *
      *----------+----------+------------------------------------------*
      *                                                                *
      ******************************************************************
                                                                        
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
                                                                        
           SELECT ARQEXTRC ASSIGN      TO  UT-S-ARQEXTRC                
                      FILE STATUS      IS  WRK-FS-ARQEXTRC.             
                                                                        
           SELECT BDRPTRAN ASSIGN      TO  UT-S-BDRPTRAN                
                      FILE STATUS      IS  WRK-FS-BDRPTRAN.             
                                                                        
           SELECT ARQSAIDA ASSIGN      TO  UT-S-ARQSAIDA                
                      FILE STATUS      IS  WRK-FS-ARQSAIDA.             
                                                                        
           SELECT BDRPSCOR ASSIGN      TO  UT-S-BDRPSCOR                
                      FILE STATUS      IS  WRK-FS-BDRPSCOR.             
                                                                        
      *================================================================*
       DATA                            DIVISION.                        
      *================================================================*
                                                                        
      *----------------------------------------------------------------*
       FILE                            SECTION.                         
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *    INPUT   :  ARQUIVO DE ENTRADA ARQEXTRC                      *
      *               ORG. SEQUENCIAL   -   LRECL  =  3400             *
      *----------------------------------------------------------------*
                                                                        
       FD  ARQEXTRC                                                     
           RECORDING MODE              IS F                             
           LABEL RECORD                IS STANDARD                      
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       COPY 'I#RDABS0'.                                                 
                                                                        
      *----------------------------------------------------------------*
      *    INPUT   :  ARQUIVO BDRPTRAN ERROS DO CACS                  * 
      *               ORG. SEQUENCIAL   -   LRECL  =  396             * 
      *----------------------------------------------------------------*
                                                                        
       FD  BDRPTRAN                                                     
           RECORDING MODE              IS F                             
           LABEL RECORD                IS STANDARD                      
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01 FD-DETAIL-TRANS-ERROR.                                        
          03 FD-DIAD-RPT-REC-4.                                         
             05 FD-RPT-NUM             PIC  999.                        
             05 FD-LOCATION-MODE       PIC  X(06).                      
             05 FD-RPT-DATA-4.                                          
                10 FD-ACCT-NUM         PIC  X(018).                     
                10 FD-TRANSATION-CODE  PIC XXX.                         
                10 FD-TRANS-IMAGE      PIC  X(080).                     
                10 FD-ERROR-CODE       PIC  9(006).                     
             05  FILLER                PIC  X(280).                     
                                                                        
      *----------------------------------------------------------------*
      *    OUTPUT  :  EXTRACT ZERADO COM CONTAS A SEREM INATIVADAS    * 
      *               ORG. SEQUENCIAL   -   LRECL  =  3400            * 
      *----------------------------------------------------------------*
                                                                        
       FD  ARQSAIDA                                                     
           RECORDING MODE              IS F                             
           LABEL RECORD                IS STANDARD                      
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       COPY 'I#RDABS0'.                                                 
                                                                        
      *----------------------------------------------------------------*
      *    OUTPUT  :  BDRPTRAN DE ERROS DO CACS COM REGS DESPREZADOS  * 
      *               ORG. SEQUENCIAL   -   LRECL  =  396             * 
      *----------------------------------------------------------------*
                                                                        
       FD  BDRPSCOR                                                     
           RECORDING MODE              IS F                             
           LABEL RECORD                IS STANDARD                      
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01 FD-DETAIL-TRANS-ERROR.                                        
          03 FD-DIAD-RPT-REC-4.                                         
             05 FD-RPT-NUM             PIC  999.                        
             05 FD-LOCATION-MODE       PIC  X(06).                      
             05 FD-RPT-DATA-4.                                          
                10 FD-ACCT-NUM         PIC  X(018).                     
                10 FD-TRANSATION-CODE  PIC  XXX.                        
                10 FD-TRANS-IMAGE      PIC  X(080).                     
                10 FD-ERROR-CODE       PIC  9(006).                     
             05 FILLER                 PIC  X(280).                     
                                                                        
      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.                         
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(32) VALUE                 
                                    '*  INICIO DA WORKING RDAB7482  *'. 
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *                       AREAS DE FILE STATUS                     *
      *----------------------------------------------------------------*
                                                                        
       01  FILLER.                                                      
           03 WRK-FS-ARQEXTRC          PIC  X(02) VALUE SPACES.         
           03 WRK-FS-BDRPTRAN          PIC  X(02) VALUE SPACES.         
           03 WRK-FS-ARQSAIDA          PIC  X(02) VALUE SPACES.         
           03 WRK-FS-BDRPSCOR          PIC  X(02) VALUE SPACES.         
                                                                        
       01  FILLER.                                                      
           03 WRK-OPERACAO             PIC  X(13) VALUE SPACES.         
           03 WRK-ABERTURA             PIC  X(13) VALUE 'NA ABERTURA'.  
           03 WRK-LEITURA              PIC  X(13) VALUE 'NA LEITURA'.   
           03 WRK-GRAVACAO             PIC  X(13) VALUE 'NA GRAVACAO'.  
           03 WRK-FECHAMENTO           PIC  X(13) VALUE 'NO FECHAMENTO'.
                                                                        
      *----------------------------------------------------------------*
      *                          ACUMULADORES                          *
      *----------------------------------------------------------------*
                                                                        
       01  FILLER.                                                      
           03 ACU-LIDOS-ARQEXTRC       PIC  9(09) COMP-3 VALUE ZEROS.   
           03 ACU-LIDOS-BDRPTRAN       PIC  9(09) COMP-3 VALUE ZEROS.   
           03 ACU-GRAVA-ARQSAIDA       PIC  9(09) COMP-3 VALUE ZEROS.   
           03 ACU-GRAVA-BDRPSCOR       PIC  9(09) COMP-3 VALUE ZEROS.   
           03 ACU-GRAVA-EXTRCCOR       PIC  9(09) COMP-3 VALUE ZEROS.   
           03 ACU-GRAVA-EXTRSCOR       PIC  9(09) COMP-3 VALUE ZEROS.   
                                                                        
      *----------------------------------------------------------------*
      *    AREA DAS CHAVES DE BATIMENTO                                *
      *----------------------------------------------------------------*
                                                                        
       01  WRK-CHV-ARQEXTRC.                                            
           03  WRK-EXT-LOCATION-CODE   PIC  X(06) VALUE SPACES.         
           03  WRK-EXT-ACCT-NUM        PIC  X(18) VALUE SPACES.         
                                                                        
       01  WRK-CHV-BDRPTRAN.                                            
           03  WRK-CAC-LOCATION-CODE   PIC  X(06) VALUE SPACES.         
           03  WRK-CAC-ACCT-NUM        PIC  X(18) VALUE SPACES.         
                                                                        
      *----------------------------------------------------------------*
      *    AREA DE AUXILIARES                                          *
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *    AREA PARA ARMAZENAR O PRIMEIRO REGISTRO DO EXTRACT          *
      *----------------------------------------------------------------*
                                                                        
       01  WRK-BASE-EXTRACT.                                            
           03  WRK-EXTRACT-SEG.                                         
               05 WRK-EXTRACT-ALTERNATE-KEY.                            
                   10 WRK-SEARCH-NAME          PIC X(15)   VALUE SPACES.
                   10 WRK-EXTRACT-SEGMENT-KEY.                          
                       15 WRK-LOCATION-CODE    PIC X(06)   VALUE SPACES.
                       15 WRK-ACCT-NUM         PIC X(18)   VALUE SPACES.
           03  FILLER                  PIC  X(404)  VALUE SPACES.       
           03  WRK-SOCIAL-SECURITY-NUM.                                 
               05  WRK-CPFCNPJ-BASE    PIC  X(15)   VALUE SPACES.       
               05  WRK-CNPJ-FILIAL     PIC  X(03)   VALUE SPACES.       
               05  WRK-CPF-CONTROL     PIC  X(02)   VALUE SPACES.       
           03  FILLER                  PIC  X(1704) VALUE SPACES.       
           03  WRK-CUSTOMER-INFO-NUM   PIC  X(24)   VALUE SPACES.       
           03  FILLER                  PIC  X(1209) VALUE SPACES.       
                                                                        
       01  WRK-BASE-CPF-AUX.                                            
           03  WRK-CPFCNPJ-BASE-AUX    PIC  X(15)   VALUE               
                                                      '999999999999999'.
           03  WRK-CPF-CONTROL-AUX     PIC  X(02)   VALUE '  '.         
                                                                        
      *----------------------------------------------------------------*
      *    AREA DE TRATAMENTO DE ERRO   -   POOL7100                   *
      *----------------------------------------------------------------*
                                                                        
       01  WRK-BATCH                   PIC  X(08) VALUE 'BATCH'.        
                                                                        
       01  WRK-TEXTO.                                                   
           03  FILLER                  PIC  X(10) VALUE SPACES.         
           03  FILLER                  PIC  X(05) VALUE 'ERRO'.         
           03  WRK-OPERACAO-TXT        PIC  X(13) VALUE SPACES.         
           03  FILLER                  PIC  X(12) VALUE ' DO ARQUIVO '. 
           03  WRK-ARQ-TXT             PIC  X(08) VALUE SPACES.         
           03  FILLER                  PIC  X(14) VALUE                 
                                                       'FILE STATUS = '.
           03  WRK-FILE-STATUS         PIC  X(02) VALUE SPACES.         
                                                                        
       COPY 'POL7100C'.                                                 
                                                                        
      *---------------------------------------------------------------* 
      *  CAMPO UTILIZADO PELA POOL7600 - OBTEM DATA E HORA DO SISTEMA * 
      *---------------------------------------------------------------* 
                                                                        
       01 WRK-7600-DATA-HORA.                                           
           03 WRK-DT-JULIANA-7600      PIC  9(05) COMP-3 VALUE ZEROS.   
           03 WRK-DT-AAMMDD-7600       PIC  9(07) COMP-3 VALUE ZEROS.   
           03 WRK-DT-AAAAMMDD-7600     PIC  9(09) COMP-3 VALUE ZEROS.   
           03 WRK-TI-HHMMSS-7600       PIC  9(07) COMP-3 VALUE ZEROS.   
           03 WRK-TI-HHMMSSMMMMMM-7600 PIC  9(13) COMP-3 VALUE ZEROS.   
           03 WRK-TIMESTAMP-7600.                                       
               05 WRK-ANO-7600         PIC  9(04) VALUE ZEROS.          
               05 WRK-MES-7600         PIC  9(02) VALUE ZEROS.          
               05 WRK-DIA-7600         PIC  9(02) VALUE ZEROS.          
               05 WRK-HORA-7600        PIC  9(02) VALUE ZEROS.          
               05 WRK-MINUTOS-7600     PIC  9(02) VALUE ZEROS.          
               05 WRK-SEGUNDOS-7600    PIC  9(02) VALUE ZEROS.          
               05 WRK-MICROSEGUNDOS-7600                                
                                       PIC  9(06) VALUE ZEROS.          
                                                                        
      *---------------------------------------------------------------* 
      *               CAMPOS AUXILIARES DE DATA E HORA                * 
      *---------------------------------------------------------------* 
                                                                        
       01 WRK-AAAAMMDD                 PIC  9(09) VALUE ZEROS.          
       01 WRK-AAAAMMDD-AUX             REDEFINES  WRK-AAAAMMDD.         
          03 FILLER                    PIC  X(01).                      
          03 WRK-AAAAMMDD-R.                                            
             05 WRK-ANO                PIC  9(04).                      
             05 WRK-MES                PIC  9(02).                      
             05 WRK-DIA                PIC  9(02).                      
                                                                        
       01 WRK-DDMMAAAA.                                                 
          03 WRK-DIA                   PIC  9(02)  VALUE ZEROS.         
          03 FILLER                    PIC  X(01)  VALUE '/'.           
          03 WRK-MES                   PIC  9(02)  VALUE ZEROS.         
          03 FILLER                    PIC  X(01)  VALUE '/'.           
          03 WRK-ANO                   PIC  9(04)  VALUE ZEROS.         
       01 WRK-DDMMAAAA-R               REDEFINES   WRK-DDMMAAAA         
                                       PIC  X(10).                      
                                                                        
       01 WRK-HORA                     PIC  9(09)  VALUE ZEROS.         
       01 WRK-HORA-AUX                 REDEFINES   WRK-HORA.            
          03 FILLER                    PIC  X(03).                      
          03 WRK-HORA-R.                                                
             05 WRK-HH                 PIC  9(02).                      
             05 WRK-MM                 PIC  9(02).                      
             05 WRK-SS                 PIC  9(02).                      
                                                                        
       01 WRK-HORA-EDITADA.                                             
          03 WRK-HH                    PIC  9(02)  VALUE ZEROS.         
          03 FILLER                    PIC  X(01)  VALUE ':'.           
          03 WRK-MM                    PIC  9(02)  VALUE ZEROS.         
          03 FILLER                    PIC  X(01)  VALUE ':'.           
          03 WRK-SS                    PIC  9(02)  VALUE ZEROS.         
       01 WRK-HORA-EDITADA-R           REDEFINES   WRK-HORA-EDITADA     
                                       PIC  X(08).                      
                                                                        
      *----------------------------------------------------------------*
      *    AREA DA BRAD0160                                            *
      *----------------------------------------------------------------*
                                                                        
       01 WRK-AREA-BRAD0160.                                            
          03 WRK-JOBNAME-BRAD0160      PIC  X(08) VALUE SPACES.         
          03 WRK-VLRFAC-BRAD0160       PIC  9(05)V99 COMP-3 VALUE ZEROS.
                                                                        
      *----------------------------------------------------------------*
      *    AREA DE LITERAIS                                            *
      *----------------------------------------------------------------*
                                                                        
       01  FILLER.                                                      
           03 WRK-LIT-OBJETIVO         PIC  X(64) VALUE 'GERAR EXTRACT C
      -    'OM ERRO ZERADO A PARTIR DO BATIMENTO COM O CACS  '.         
           03 WRK-LIT-PROGRAMA         PIC  X(08) VALUE 'RDAB7482'.     
           03 WRK-LIT-ARQEXTRC         PIC  X(08) VALUE 'ARQEXTRC'.     
           03 WRK-LIT-DESC-ARQEXTRC    PIC  X(27) VALUE                 
                                          'ARQ EXTRACT DO DIA ANTERIOR'.
           03 WRK-LIT-BDRPTRAN         PIC  X(08) VALUE 'BDRPTRAN'.     
           03 WRK-LIT-DESC-BDRPTRAN    PIC  X(27) VALUE                 
                                          'ARQ TRANSITO ERRROS DO CACS'.
           03 WRK-LIT-ARQSAIDA         PIC  X(08) VALUE 'ARQSAIDA'.     
           03 WRK-LIT-DESC-ARQSAIDA    PIC  X(27) VALUE                 
                                          'ARQ EXTRACT ZERADO         '.
           03 WRK-LIT-BDRPSCOR         PIC  X(08) VALUE 'BDRPSCOR'.     
           03 WRK-LIT-DESC-BDRPSCOR    PIC  X(27) VALUE                 
                                          'ARQ TRANSITO S/ CORRESPOND.'.
           03 WRK-LIT-DESC-EXTRCCOR    PIC  X(27) VALUE                 
                                          'REGS EXTRACT CONTA INATIVA '.
           3 WRK-LIT-DESC-EXTRSCOR    PIC  X(27) VALUE                  
                                          'REGS EXTRACT S/ CORRESPOND.'.
           03 WRK-LIT-ENTRADA          PIC  X(01) VALUE 'I'.            
           03 WRK-LIT-SAIDA            PIC  X(01) VALUE 'O'.            
           03 WRK-LIT-NULL             PIC  X(01) VALUE '-'.            
           03 WRK-LIT-POOL7100         PIC  X(08) VALUE 'POOL7100'.     
           03 WRK-LIT-DESC7100         PIC  X(32) VALUE 'TRATAR ERROS'. 
           03 WRK-LIT-POOL7100-BOOK    PIC  X(08) VALUE 'POOL710C'.     
           03 WRK-LIT-POOL7600         PIC  X(08) VALUE 'POOL7600'.     
           03 WRK-LIT-DESC7600         PIC  X(32) VALUE                 
                                         'OBTER DATA E HORA DO SISTEMA'.
           03 WRK-LIT-BRAD0160         PIC  X(08) VALUE 'BRAD0160'.     
           03 WRK-LIT-DESC0160         PIC  X(32) VALUE                 
                                        'OBTER O NOME DO JOB EXECUTADO'.
                                                                        
      *----------------------------------------------------------------*
      *    AREA DE DISPLAY DE ESTATISTICAS DE PROCESSAMENTO            *
      *----------------------------------------------------------------*
                                                                        
       COPY 'I#RDABYY'.                                                 
                                                                        
          03 WRK-DISP16.                                                
             05 FILLER                 PIC  X(03) VALUE '** '.          
             05 FILLER                 PIC  X(08) VALUE ' MODULO '.     
             05 FILLER                 PIC  X(03) VALUE ' | '.          
             05 FILLER                 PIC  X(08) VALUE '  BOOK  '.     
             05 FILLER                 PIC  X(03) VALUE ' | '.          
             05 FILLER                 PIC  X(42) VALUE 'DESCRICAO'.    
             05 FILLER                 PIC  X(03) VALUE ' **'.          
                                                                        
          03 WRK-DISP17.                                                
             05 FILLER                 PIC  X(03) VALUE '** '.          
             05 FILLER                 PIC  X(08) VALUE  ALL '-'.       
             05 FILLER                 PIC  X(03) VALUE ' | '.          
             05 FILLER                 PIC  X(08) VALUE  ALL '-'.       
             05 FILLER                 PIC  X(03) VALUE ' | '.          
             05 FILLER                 PIC  X(42) VALUE  ALL '-'.       
             05 FILLER                 PIC  X(03) VALUE ' **'.          
                                                                        
          03 WRK-DISP18.                                                
             05 FILLER                 PIC  X(03) VALUE '** '.          
             05 WRK-MODULO-DISP        PIC  X(08) VALUE SPACES.         
             05 FILLER                 PIC  X(03) VALUE ' | '.          
             05 WRK-MODULO-BOOK        PIC  X(08) VALUE SPACES.         
             05 FILLER                 PIC  X(03) VALUE ' | '.          
             05 WRK-MODULO-DESC        PIC  X(42) VALUE SPACES.         
             05 FILLER                 PIC  X(03) VALUE ' **'.          
                                                                        
      *----------------------------------------------------------------*
       77   FILLER                     PIC  X(32) VALUE                 
            '*   FIM  DA WORKING RDAB7482   *'.                         
      *----------------------------------------------------------------*
                                                                        
      *================================================================*
       PROCEDURE                       DIVISION.                        
      *================================================================*
                                                                        
      *----------------------------------------------------------------*
      * ROTINA INICIAR                                                 *
      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           PERFORM 1000-INICIALIZAR.                                    
                                                                        
           PERFORM 1200-FORMATAR-DATA-HORA.                             
                                                                        
           PERFORM 2000-PROCESSAR                                       
             UNTIL WRK-CHV-BDRPTRAN    EQUAL  HIGH-VALUES AND
                   WRK-CHV-ARQEXTRC    EQUAL  HIGH-VALUES
           PERFORM 3000-FINALIZAR.                                      
                                                                        
      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * ROTINA INICIALIZAR                                             *
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           OPEN    INPUT   ARQEXTRC                                     
                           BDRPTRAN                                     
                   OUTPUT  ARQSAIDA                                     
                           BDRPSCOR.                                    
                                                                        
           MOVE    WRK-ABERTURA        TO  WRK-OPERACAO.                
           PERFORM 1100-TESTAR-FILE-STATUS.                             
                                                                        
           PERFORM 1300-PRIMEIRA-LEITURA.                               
                                                                        
                                                                        
      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * ROTINA DE TESTE DE FILE STATUS DOS ARQUIVOS                    *
      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS         SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           PERFORM 1110-TESTAR-FS-ARQEXTRC.                             
                                                                        
           PERFORM 1120-TESTAR-FS-BDRPTRAN.                             
                                                                        
           PERFORM 1130-TESTAR-FS-ARQSAIDA.                             
                                                                        
           PERFORM 1140-TESTAR-FS-BDRPSCOR.                             
                                                                        
      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * ROTINA DE TESTE DE FILE STATUS DO ARQUIVO ARQEXTRC             *
      *----------------------------------------------------------------*
       1110-TESTAR-FS-ARQEXTRC         SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           IF  (WRK-FS-ARQEXTRC        NOT EQUAL '00' AND               
                                       NOT EQUAL '10')                  
               DISPLAY '************** RDAB7482 *************'          
               DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'      
               DISPLAY '*             ARQEXTRC              *'          
               DISPLAY '*         FILE STATUS =  ' WRK-FS-ARQEXTRC      
                                                  '         *'          
               DISPLAY '*         PROGRAMA CANCELADO        *'          
               DISPLAY '************** RDAB7482 *************'          
               MOVE WRK-OPERACAO       TO WRK-OPERACAO-TXT              
               MOVE 'ARQEXTRC'         TO WRK-ARQ-TXT                   
               MOVE WRK-TEXTO          TO ERR-TEXTO                     
               PERFORM 9999-ROTINA-ERRO                                 
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * ROTINA DE TESTE DE FILE STATUS DO ARQUIVO BDRPTRAN             *
      *----------------------------------------------------------------*
       1120-TESTAR-FS-BDRPTRAN         SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           IF  (WRK-FS-BDRPTRAN        NOT EQUAL '00' AND               
                                       NOT EQUAL '10')                  
               DISPLAY '************** RDAB7482 *************'          
               DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'      
               DISPLAY '*             BDRPTRAN              *'          
               DISPLAY '*         FILE STATUS =  ' WRK-FS-BDRPTRAN      
                                                  '         *'          
               DISPLAY '*         PROGRAMA CANCELADO        *'          
               DISPLAY '************** RDAB7482 *************'          
               MOVE WRK-OPERACAO       TO WRK-OPERACAO-TXT              
               MOVE 'BDRPTRAN'         TO WRK-ARQ-TXT                   
               MOVE WRK-TEXTO          TO ERR-TEXTO                     
               PERFORM 9999-ROTINA-ERRO                                 
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * ROTINA DE TESTE DE FILE STATUS DO ARQUIVO ARQSAIDA             *
      *----------------------------------------------------------------*
       1130-TESTAR-FS-ARQSAIDA         SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           IF  (WRK-FS-ARQSAIDA        NOT EQUAL '00' AND               
                                       NOT EQUAL '10')                  
               DISPLAY '************** RDAB7482 *************'          
               DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'      
               DISPLAY '*             ARQSAIDA              *'          
               DISPLAY '*         FILE STATUS =  ' WRK-FS-ARQSAIDA      
                                                  '         *'          
               DISPLAY '*         PROGRAMA CANCELADO        *'          
               DISPLAY '************** RDAB7482 *************'          
               MOVE WRK-OPERACAO       TO WRK-OPERACAO-TXT              
               MOVE 'ARQSAIDA'         TO WRK-ARQ-TXT                   
               MOVE WRK-TEXTO          TO ERR-TEXTO                     
               PERFORM 9999-ROTINA-ERRO                                 
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       1130-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * ROTINA DE TESTE DE FILE STATUS DO ARQUIVO BDRPSCOR             *
      *----------------------------------------------------------------*
       1140-TESTAR-FS-BDRPSCOR         SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           IF  (WRK-FS-BDRPSCOR        NOT EQUAL '00' AND               
                                       NOT EQUAL '10')                  
               DISPLAY '************** RDAB7482 *************'          
               DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'      
               DISPLAY '*             BDRPSCOR              *'          
               DISPLAY '*         FILE STATUS =  ' WRK-FS-BDRPSCOR      
                                                  '         *'          
               DISPLAY '*         PROGRAMA CANCELADO        *'          
               DISPLAY '************** RDAB7482 *************'          
               MOVE WRK-OPERACAO       TO WRK-OPERACAO-TXT              
               MOVE 'BDRPSCOR'         TO WRK-ARQ-TXT                   
               MOVE WRK-TEXTO          TO ERR-TEXTO                     
               PERFORM 9999-ROTINA-ERRO                                 
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       1140-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * ROTINA QUE FORMATA DATA E HORA DO SISTEMA                      *
      *----------------------------------------------------------------*
       1200-FORMATAR-DATA-HORA         SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           CALL  'POOL7600'            USING WRK-7600-DATA-HORA.        
                                                                        
           MOVE   WRK-DT-AAAAMMDD-7600 TO    WRK-AAAAMMDD.              
           MOVE CORR WRK-AAAAMMDD-R    TO    WRK-DDMMAAAA.              
                                                                        
           MOVE   WRK-TI-HHMMSS-7600   TO    WRK-HORA.                  
           MOVE CORR WRK-HORA-R        TO    WRK-HORA-EDITADA.          
                                                                        
      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
                                                                        
      *----------------------------------------------------------------*
      * ROTINA DE PRIMEIRA LEITURA DOS ARQUIVOS DE ENTRADA             *
      *----------------------------------------------------------------*
       1300-PRIMEIRA-LEITURA           SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           PERFORM 1310-PRIMEIRA-LEITURA-ARQEXTRC.                      
                                                                        
           PERFORM 1320-PRIMEIRA-LEITURA-BDRPTRAN.                      
                                                                        
      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * ROTINA DE PRIMEIRA LEITURA DO ARQUIVO ARQEXTRC                 *
      *----------------------------------------------------------------*
       1310-PRIMEIRA-LEITURA-ARQEXTRC  SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           PERFORM 1400-LER-ARQEXTRC.                                   
                                                                        
           IF  ACU-LIDOS-ARQEXTRC      EQUAL  ZEROS                     
               DISPLAY '************ RDAB7482 ************'             
               DISPLAY '*                                *'             
               DISPLAY '*     ARQUIVO ARQEXTRC VAZIO     *'             
               DISPLAY '*                                *'             
               DISPLAY '************ RDAB7482 ************'             
           END-IF.                                                      
                                                                        
           MOVE  REG-BASE-EXTRACT      OF  ARQEXTRC                     
                                       TO  WRK-BASE-EXTRACT             
                                                                        
           MOVE  WRK-CPFCNPJ-BASE-AUX  TO  WRK-CPFCNPJ-BASE
                                           WRK-CUSTOMER-INFO-NUM.
           MOVE  WRK-CPF-CONTROL-AUX   TO  WRK-CPF-CONTROL.             
                                                                        
      *----------------------------------------------------------------*
       1310-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * ROTINA DE PRIMEIRA LEITURA DO ARQUIVO BDRPTRAN                 *
      *----------------------------------------------------------------*
       1320-PRIMEIRA-LEITURA-BDRPTRAN  SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           PERFORM 1500-LER-BDRPTRAN.                                   
                                                                        
           IF  ACU-LIDOS-BDRPTRAN      EQUAL  ZEROS                     
               DISPLAY '************ RDAB7482 ************'             
               DISPLAY '*                                *'             
               DISPLAY '*     ARQUIVO BDRPTRAN VAZIO     *'             
               DISPLAY '*                                *'             
               DISPLAY '************ RDAB7482 ************'             
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       1320-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * ROTINA DE LEITURA DO ARQUIVO ARQEXTRC                          *
      *----------------------------------------------------------------*
       1400-LER-ARQEXTRC               SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           READ    ARQEXTRC.                                            
                                                                        
           IF   WRK-FS-ARQEXTRC EQUAL '10'                              
                MOVE  HIGH-VALUES      TO  WRK-CHV-ARQEXTRC             
                GO                     TO  1400-99-FIM                  
           END-IF.                                                      
                                                                        
           MOVE    WRK-LEITURA         TO  WRK-OPERACAO.                
           PERFORM 1110-TESTAR-FS-ARQEXTRC.                             
                                                                        
           ADD     1                   TO  ACU-LIDOS-ARQEXTRC.          
                                                                        
           MOVE  LOCATION-CODE         OF  ARQEXTRC                     
                                       TO  WRK-EXT-LOCATION-CODE.       
           MOVE  ACCT-NUM              OF  ARQEXTRC                     
                                       TO  WRK-EXT-ACCT-NUM.            
                                                                        
      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * ROTINA DE LEITURA DO ARQUIVO BDRPTRAN                          *
      *----------------------------------------------------------------*
       1500-LER-BDRPTRAN               SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           READ    BDRPTRAN.                                            
                                                                        
           IF   WRK-FS-BDRPTRAN EQUAL '10'                              
                MOVE  HIGH-VALUES      TO  WRK-CHV-BDRPTRAN             
                GO                     TO  1500-99-FIM                  
           END-IF.                                                      
                                                                        
           MOVE    WRK-LEITURA         TO  WRK-OPERACAO.                
           PERFORM 1120-TESTAR-FS-BDRPTRAN.                             
                                                                        
           ADD     1                   TO  ACU-LIDOS-BDRPTRAN.          
                                                                        
           MOVE  FD-LOCATION-MODE      OF  BDRPTRAN                     
                                       TO  WRK-CAC-LOCATION-CODE.       
           MOVE  FD-ACCT-NUM           OF  BDRPTRAN                     
                                       TO  WRK-CAC-ACCT-NUM.            
                                                                        
      *----------------------------------------------------------------*
       1500-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * ROTINA DE PROCESSAMENTO                                        *
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           IF WRK-CHV-ARQEXTRC         EQUAL  WRK-CHV-BDRPTRAN          
              MOVE REG-BASE-EXTRACT OF  ARQEXTRC                        
                                    TO  REG-BASE-EXTRACT  OF ARQSAIDA   
                                                                        
              IF SEARCH-NAME OF ARQEXTRC EQUAL 'INATIVAR CONTA'         
                 MOVE SPACES        TO SOCIAL-SECURITY-NUM  OF ARQSAIDA 
                                       CUSTOMER-INFO-NUMBER OF ARQSAIDA 
                 MOVE WRK-CPFCNPJ-BASE-AUX                              
                                    TO SOCIAL-SECURITY-NUM  OF ARQSAIDA 
                                       CUSTOMER-INFO-NUMBER OF ARQSAIDA 
              END-IF                                                    
              MOVE 'INATIVAR CONTA3' TO SEARCH-NAME          OF ARQSAIDA
              MOVE 'INATIVAR CONTA3' TO CUSTOMER-NAME        OF ARQSAIDA
              MOVE 'INATIVAR CONTA3' TO CUSTOMER-ADDRESS     OF ARQSAIDA
              MOVE ZEROS            TO TELEPHONE-NUMBER-DATA OF ARQSAIDA
              MOVE ZEROS            TO TOTAL-DELINQ-AMT      OF ARQSAIDA
              MOVE ZEROS            TO DISPUTED-AMT          OF ARQSAIDA
              MOVE ZEROS            TO INTEREST-DELQ-AMT     OF ARQSAIDA
              MOVE ZEROS            TO OTH-DELQ-AMT          OF ARQSAIDA
              MOVE ZEROS            TO REPOSSESSION-CHARGES  OF ARQSAIDA
              MOVE ZEROS            TO TOTAL-DUE-AMT         OF ARQSAIDA
                                                                        
              PERFORM  2100-GRAVAR-ARQSAIDA                             
              ADD  1                   TO  ACU-GRAVA-EXTRCCOR           
              PERFORM  1400-LER-ARQEXTRC                                
              PERFORM  1500-LER-BDRPTRAN                                
           ELSE                                                         
              IF WRK-CHV-ARQEXTRC      GREATER  WRK-CHV-BDRPTRAN        
                 MOVE WRK-BASE-EXTRACT TO REG-BASE-EXTRACT   OF ARQSAIDA
                 MOVE WRK-CAC-LOCATION-CODE TO LOCATION-CODE OF ARQSAIDA
                 MOVE WRK-CAC-ACCT-NUM      TO ACCT-NUM      OF ARQSAIDA
                 MOVE 'INATIVAR CONTA1'
                                       TO SEARCH-NAME        OF ARQSAIDA
                 MOVE 'INATIVAR CONTA1'
                                       TO CUSTOMER-NAME      OF ARQSAIDA
                 MOVE 'INATIVAR CONTA1'
                                       TO CUSTOMER-ADDRESS   OF ARQSAIDA
                 MOVE ZEROS         TO TELEPHONE-NUMBER-DATA OF ARQSAIDA
                 MOVE ZEROS         TO TOTAL-DELINQ-AMT      OF ARQSAIDA
                 MOVE ZEROS         TO DISPUTED-AMT          OF ARQSAIDA
                 MOVE ZEROS         TO INTEREST-DELQ-AMT     OF ARQSAIDA
                 MOVE ZEROS         TO OTH-DELQ-AMT          OF ARQSAIDA
                 MOVE ZEROS         TO REPOSSESSION-CHARGES  OF ARQSAIDA
                 MOVE ZEROS         TO TOTAL-DUE-AMT         OF ARQSAIDA
                                                                        
                 PERFORM  2100-GRAVAR-ARQSAIDA                          
                                                                        
                 MOVE FD-DETAIL-TRANS-ERROR                  OF BDRPTRAN
                                    TO FD-DETAIL-TRANS-ERROR OF BDRPSCOR
                                                                        
                 PERFORM  2200-GRAVAR-BDRPSCOR                          
                 PERFORM  1500-LER-BDRPTRAN                             
              ELSE
                 IF SEARCH-NAME OF ARQEXTRC EQUAL 'INATIVAR CONTA'
                   MOVE REG-BASE-EXTRACT OF  ARQEXTRC
                                       TO REG-BASE-EXTRACT  OF ARQSAIDA
                   MOVE SPACES     TO SOCIAL-SECURITY-NUM   OF ARQSAIDA
                                       CUSTOMER-INFO-NUMBER OF ARQSAIDA
                   MOVE WRK-CPFCNPJ-BASE-AUX
                                    TO SOCIAL-SECURITY-NUM  OF ARQSAIDA
                                       CUSTOMER-INFO-NUMBER OF ARQSAIDA
                   MOVE 'INATIVAR CONTA2'
                                       TO SEARCH-NAME       OF ARQSAIDA
                   MOVE 'INATIVAR CONTA2'
                                       TO CUSTOMER-NAME     OF ARQSAIDA
                   MOVE 'INATIVAR CONTA2'
                                       TO CUSTOMER-ADDRESS  OF ARQSAIDA

                   MOVE ZEROS      TO TELEPHONE-NUMBER-DATA OF ARQSAIDA
                   MOVE ZEROS      TO TOTAL-DELINQ-AMT      OF ARQSAIDA
                   MOVE ZEROS      TO DISPUTED-AMT          OF ARQSAIDA
                   MOVE ZEROS      TO INTEREST-DELQ-AMT     OF ARQSAIDA
                   MOVE ZEROS      TO OTH-DELQ-AMT          OF ARQSAIDA
                   MOVE ZEROS      TO REPOSSESSION-CHARGES  OF ARQSAIDA
                   MOVE ZEROS      TO TOTAL-DUE-AMT         OF ARQSAIDA

                   PERFORM  2100-GRAVAR-ARQSAIDA
                 END-IF
                 ADD  1                   TO  ACU-GRAVA-EXTRSCOR        
                 PERFORM  1400-LER-ARQEXTRC                             
              END-IF                                                    
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * ROTINA DE GRAVACAO DO ARQUIVO ARQSAIDA                         *
      *----------------------------------------------------------------*
       2100-GRAVAR-ARQSAIDA            SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE    WRK-GRAVACAO        TO  WRK-OPERACAO.                
                                                                        
           WRITE   REG-BASE-EXTRACT    OF  ARQSAIDA.                    
           PERFORM 1130-TESTAR-FS-ARQSAIDA.                             
                                                                        
           ADD     1                   TO  ACU-GRAVA-ARQSAIDA.          
                                                                        
      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * ROTINA DE GRAVACAO DO ARQUIVO BDRPSCOR                         *
      *----------------------------------------------------------------*
       2200-GRAVAR-BDRPSCOR            SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE    WRK-GRAVACAO        TO  WRK-OPERACAO.                
                                                                        
           WRITE   FD-DETAIL-TRANS-ERROR OF BDRPSCOR.                   
           PERFORM 1140-TESTAR-FS-BDRPSCOR.                             
                                                                        
           ADD     1                   TO  ACU-GRAVA-BDRPSCOR.          
                                                                        
      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * ROTINA DE FINALIZACAO                                          *
      *----------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           PERFORM 3100-MOSTRAR-DISPLAY.                                
                                                                        
           CLOSE   ARQEXTRC                                             
                   BDRPTRAN                                             
                   BDRPSCOR                                             
                   ARQSAIDA.                                            
                                                                        
           MOVE    WRK-FECHAMENTO      TO  WRK-OPERACAO.                
           PERFORM 1100-TESTAR-FILE-STATUS.                             
                                                                        
           STOP    RUN.                                                 
                                                                        
      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * ROTINA DE EXIBICAO DE ESTATISTICA EXECECAO                     *
      *----------------------------------------------------------------*
       3100-MOSTRAR-DISPLAY            SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE  WRK-LIT-OBJETIVO      TO  WRK-COMENTARIO.              
           MOVE  WRK-LIT-PROGRAMA      TO  WRK-PROGNAME.                
                                                                        
           CALL  'BRAD0160'            USING  WRK-JOBNAME-BRAD0160      
                                              WRK-VLRFAC-BRAD0160.      
                                                                        
           MOVE  WRK-JOBNAME-BRAD0160  TO  WRK-JOBNAME.                 
           MOVE  WRK-DDMMAAAA-R        TO  WRK-DATAPROC                 
                                           WRK-DATAMOV.                 
           MOVE  WRK-HORA-EDITADA-R    TO  WRK-HORAPROC.                
                                                                        
           DISPLAY  WRK-DISP1.                                          
           DISPLAY  WRK-DISP2.                                          
           DISPLAY  WRK-DISP3.                                          
           DISPLAY  WRK-DISP13.                                         
           DISPLAY  WRK-DISP3.                                          
           DISPLAY  WRK-DISP4.                                          
           DISPLAY  WRK-DISP5.                                          
           DISPLAY  WRK-DISP6.                                          
           DISPLAY  WRK-DISP3.                                          
           DISPLAY  WRK-DISP7.                                          
           DISPLAY  WRK-DISP8.                                          
                                                                        
           MOVE  WRK-LIT-ARQEXTRC      TO  WRK-DDNAME.                  
           MOVE  WRK-LIT-ENTRADA       TO  WRK-I-O.                     
           MOVE  WRK-LIT-DESC-ARQEXTRC TO  WRK-DESCARQ.                 
           MOVE  ACU-LIDOS-ARQEXTRC    TO  WRK-QTDEARQ.                 
                                                                        
           DISPLAY  WRK-DISP9.                                          
                                                                        
           MOVE  WRK-LIT-BDRPTRAN      TO  WRK-DDNAME.                  
           MOVE  WRK-LIT-ENTRADA       TO  WRK-I-O.                     
           MOVE  WRK-LIT-DESC-BDRPTRAN TO  WRK-DESCARQ.                 
           MOVE  ACU-LIDOS-BDRPTRAN    TO  WRK-QTDEARQ.                 
                                                                        
           DISPLAY  WRK-DISP9.                                          
                                                                        
           MOVE  WRK-LIT-ARQSAIDA      TO  WRK-DDNAME.                  
           MOVE  WRK-LIT-SAIDA         TO  WRK-I-O.                     
           MOVE  WRK-LIT-DESC-ARQSAIDA TO  WRK-DESCARQ.                 
           MOVE  ACU-GRAVA-ARQSAIDA    TO  WRK-QTDEARQ.                 
                                                                        
           DISPLAY  WRK-DISP9.                                          
                                                                        
           MOVE  SPACES                TO  WRK-DDNAME.                  
           MOVE  WRK-LIT-NULL          TO  WRK-I-O.                     
           MOVE  WRK-LIT-DESC-EXTRCCOR TO  WRK-DESCARQ.                 
           MOVE  ACU-GRAVA-EXTRCCOR    TO  WRK-QTDEARQ.                 
                                                                        
           DISPLAY  WRK-DISP9.                                          
                                                                        
           MOVE  SPACES                TO  WRK-DDNAME.                  
           MOVE  WRK-LIT-NULL          TO  WRK-I-O.                     
           MOVE  WRK-LIT-DESC-EXTRSCOR TO  WRK-DESCARQ.                 
           MOVE  ACU-GRAVA-EXTRSCOR    TO  WRK-QTDEARQ.                 
                                                                        
           DISPLAY  WRK-DISP9.                                          
                                                                        
           MOVE  WRK-LIT-BDRPSCOR      TO  WRK-DDNAME.                  
           MOVE  WRK-LIT-SAIDA         TO  WRK-I-O.                     
           MOVE  WRK-LIT-DESC-BDRPSCOR TO  WRK-DESCARQ.                 
           MOVE  ACU-GRAVA-BDRPSCOR    TO  WRK-QTDEARQ.                 
                                                                        
           DISPLAY  WRK-DISP9.                                          
           DISPLAY  WRK-DISP3.                                          
           DISPLAY  WRK-DISP16.                                         
           DISPLAY  WRK-DISP17.                                         
                                                                        
           MOVE  WRK-LIT-POOL7100      TO  WRK-MODULO-DISP.             
           MOVE  WRK-LIT-POOL7100-BOOK TO  WRK-MODULO-BOOK.             
           MOVE  WRK-LIT-DESC7100      TO  WRK-MODULO-DESC.             
                                                                        
           DISPLAY  WRK-DISP18.                                         
                                                                        
           MOVE  WRK-LIT-POOL7600      TO  WRK-MODULO-DISP.             
           MOVE  SPACES                TO  WRK-MODULO-BOOK.             
           MOVE  WRK-LIT-DESC7600      TO  WRK-MODULO-DESC.             
                                                                        
           DISPLAY  WRK-DISP18.                                         
                                                                        
           MOVE  WRK-LIT-BRAD0160      TO  WRK-MODULO-DISP.             
           MOVE  SPACES                TO  WRK-MODULO-BOOK.             
           MOVE  WRK-LIT-DESC0160      TO  WRK-MODULO-DESC.             
                                                                        
           DISPLAY  WRK-DISP18.                                         
           DISPLAY  WRK-DISP1.                                          
                                                                        
      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * ROTINA DE ERRO ATRAVES DA POOL7100                             *
      *----------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE 'RDAB7482'             TO  ERR-PGM.                     
           MOVE 'APL'                  TO  ERR-TIPO-ACESSO.             
                                                                        
           CALL 'POOL7100'             USING  WRK-BATCH                 
                                              ERRO-AREA.                
                                                                        
           GOBACK.                                                      
                                                                        
      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*

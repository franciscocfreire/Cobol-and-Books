      *===============================================================* 
       IDENTIFICATION                  DIVISION.                        
      *===============================================================* 
       PROGRAM-ID. RDAB7480.                                            
       AUTHOR. RENATO SALINAS.                                          
                                                                        
      *===============================================================* 
      *                    C P M    S I S T E M A S                   * 
      *---------------------------------------------------------------* 
      *                                                               * 
      *      PROGRAMA     :  RDAB7480                                 * 
      *      PROGRAMADOR  :  RENATO SALINAS     CPM - ALPHAVILLE      * 
      *      ANALISTA     :  ADRIANO BEHRENS    GRUPO 71              * 
      *      DATA         :  19/02/2009                               * 
      *                                                               * 
      *      OBJETIVO     :                                           * 
      *          FAZER BALANCE-LINE ENTRE OS ARQUIVOS ARQEXTRC        * 
      *          E BDRPTRAN GERANDO O ARQUIVO ARQSAIDA.               * 
      *                                                               * 
      *      ARQUIVOS     :                                           * 
      *        DDNAME                            INCLUDE/BOOK         * 
      *          ARQEXTRC                          I#RDABS0           * 
      *          BDRPTRAN                          --------           * 
      *          ARQSAIDA                          I#RDABS0           * 
      *                                                               * 
      *      MODULOS CHAMADOS  :                                      * 
      *          POOL7100  -  MODULO DE TRATAMENTO DE ERROS.          * 
      *                                                               * 
      *===============================================================* 
      ******************************************************************
      *                       A L T E R A C A O                        *
      *================================================================*
      *                                                                *
      * PROGRAMADOR  : ELSINO SILVA                 - BRQ              *
      * ANALISTA     : SANDRA H. PANSANI            - BRQ              *
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
      * GERANDO O ARQUIVO COM CARTEIRAS INATIVADAS                     *
      *                                                                *
      * LOCATIONS: 010101 E 010102                                     *
      * ERRO     : 000260                                              *
      *                                                                *
      *================================================================*
      *                       A R Q U I V O S :                        *
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
                                                                        
           EJECT                                                        
      *===============================================================* 
       ENVIRONMENT                     DIVISION.                        
      *===============================================================* 
                                                                        
      *---------------------------------------------------------------* 
       CONFIGURATION                   SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
       SPECIAL-NAMES.                                                   
           DECIMAL-POINT IS COMMA.                                      
                                                                        
      *---------------------------------------------------------------* 
       INPUT-OUTPUT                    SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
       FILE-CONTROL.                                                    
                                                                        
           SELECT ARQEXTRC ASSIGN      TO UT-S-ARQEXTRC                 
                      FILE STATUS      IS WRK-FS-ARQEXTRC.              
                                                                        
           SELECT BDRPTRAN ASSIGN      TO UT-S-BDRPTRAN                 
                      FILE STATUS      IS WRK-FS-BDRPTRAN.              
                                                                        
           SELECT ARQSAIDA ASSIGN      TO UT-S-ARQSAIDA                 
                      FILE STATUS      IS WRK-FS-ARQSAIDA.              
                                                                        
           SELECT BDRPSCOR ASSIGN      TO  UT-S-BDRPSCOR                
                      FILE STATUS      IS  WRK-FS-BDRPSCOR.             
                                                                        
           EJECT                                                        
      *===============================================================* 
       DATA                            DIVISION.                        
      *===============================================================* 
                                                                        
      *---------------------------------------------------------------* 
       FILE                            SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
      *    INPUT   :  ARQUIVO DE ENTRADA ARQEXTRC                     * 
      *               ORG. SEQUENCIAL   -   LRECL  =  3400            * 
      *---------------------------------------------------------------* 
                                                                        
       FD  ARQEXTRC                                                     
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       COPY 'I#RDABS0'.                                                 
                                                                        
      *---------------------------------------------------------------* 
      *    INPUT   :  CACS                                            * 
      *               ORG. SEQUENCIAL   -   LRECL  =  126             * 
      *---------------------------------------------------------------* 
                                                                        
       FD  BDRPTRAN                                                     
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
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
                                                                        
      *---------------------------------------------------------------* 
      *    OUTPUT  :  ARQUIVO DE SAIDA ARQSAIDA                       * 
      *               ORG. SEQUENCIAL   -   LRECL  =  3400            * 
      *---------------------------------------------------------------* 
                                                                        
       FD  ARQSAIDA                                                     
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  FD-SAI-REGTO                PIC  X(3400).                    
                                                                        
           EJECT                                                        
                                                                        
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
                                                                        
      *---------------------------------------------------------------* 
       WORKING-STORAGE                 SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
       77   FILLER                     PIC  X(32) VALUE                 
            '*  INICIO DA WORKING RDAB7480  *'.                         
                                                                        
      *--- AREAS P/TESTE FILE STATUS ---------------------------------* 
                                                                        
       77  WRK-FS-ARQEXTRC             PIC  X(02) VALUE SPACES.         
       77  WRK-FS-BDRPTRAN             PIC  X(02) VALUE SPACES.         
       77  WRK-FS-ARQSAIDA             PIC  X(02) VALUE SPACES.         
       77  WRK-FS-BDRPSCOR             PIC  X(02) VALUE SPACES.         
                                                                        
       77  WRK-OPERACAO                PIC  X(13) VALUE SPACES.         
       77  WRK-ABERTURA                PIC  X(13) VALUE 'NA ABERTURA'.  
       77  WRK-LEITURA                 PIC  X(13) VALUE 'NA LEITURA'.   
       77  WRK-GRAVACAO                PIC  X(13) VALUE 'NA GRAVACAO'.  
       77  WRK-FECHAMENTO              PIC  X(13) VALUE 'NO FECHAMENTO'.
       77  WRK-BATCH                   PIC  X(08) VALUE 'BATCH'.        
                                                                        
      *--- ACUMULADORES E AUXILIARES ---------------------------------* 
                                                                        
       01  WRK-REG-BASE-EXTRACT.                                        
       03  WRK-ACTG-EXTRACT-SEG.                                        
           05 WRK-CTG-EXTRACT-ALTERNATE-KEY.                            
              10 WRK-SEARCH-NAME    PIC X(15) VALUE SPACES.             
              10 WRK-ACTG-EXTRACT-SEGMENT-KEY.                          
                15 WRK-LOCATION-CODE    PIC X(06) VALUE SPACES.         
                15 WRK-ACCT-NUM         PIC X(18) VALUE SPACES.         
           05 WRK-CTG-EXTRACT-SEGMENT-DATA.                             
            10 WRK-ON-LINE-SHARED-USE-DATA.                             
             15 WRK-CUSTOMER-NAME   PIC X(45) VALUE SPACES.             
             15 WRK-CUST-FIRST-NAME PIC X(25) VALUE SPACES.             
             15 WRK-CUST-MIDDLE-NAME PIC X(25) VALUE SPACES.            
             15 WRK-CUST-LAST-NAME  PIC X(35) VALUE SPACES.             
             15 WRK-CUST-MATERNAL   PIC X(35) VALUE SPACES.             
             15 WRK-CUST-PREFIX     PIC X(04) VALUE SPACES.             
             15 WRK-CUST-SUFFIX     PIC X(04) VALUE SPACES.             
             15 WRK-CUSTOMER-ADDRESS.                                   
              20 WRK-CUSTOMER-ADDRESS-STREET                            
                                    PIC X(35)  OCCURS 3 TIMES.          
              20 WRK-CUSTOMER-ADDRES-CITY-STATE.                        
                 25 WRK-CUSTOMER-CITY PIC X(30) VALUE SPACES.           
                 25 WRK-CUSTOMER-STATE PIC X(02) VALUE SPACES.          
              20 WRK-CUST-ZIP-POST-CODE PIC X(09) VALUE SPACES.         
             15 WRK-CUSTOMER-COUNTY PIC X(30) VALUE SPACES.             
             15 WRK-CUST-ISO-COUNTRY-CODE                               
                                    PIC X(02) VALUE SPACES.             
             15 WRK-CUST-EMAIL-ADDRESS PIC X(48) VALUE SPACES.          
             15 WRK-PRI-LANGUAGE-IND PIC X(02) VALUE SPACES.            
             15 WRK-EXT-CUST-REF-NUM  PIC X(03) VALUE SPACES.           
             15 WRK-SOCIAL-SECURITY-NUM PIC X(20) VALUE SPACES.         
             15 WRK-EXT-CUST-TAX-ID-TYPE PIC X(01) VALUE SPACES.        
             15 WRK-PRIMARY-CUST-REL PIC X(01) VALUE SPACES.            
             15 WRK-PRIMARY-ADDR-REL PIC 9(01) VALUE ZEROS.             
             15 WRK-SECONDARY-CUST-NAME PIC X(45) VALUE SPACES.         
             15 WRK-PRIMARY-BUSINESS-NAME                               
                                    PIC X(45) VALUE SPACES.             
             15 WRK-PRIMARY-BUSINESS-REL                                
                                    PIC X(01) VALUE SPACES.             
             15 WRK-PRIMARY-BUS-ADDR-REL                                
                                    PIC 9(01) VALUE ZEROS.              
             15 WRK-PRIMARY-BUSINESS-ADDRESS.                           
              20 WRK-BUSINESS-ADDRESS-STREET                            
                                    PIC X(35) OCCURS 3 TIMES.           
              20 WRK-BUSINESS-ADDRES-CITY-STATE.                        
               25 WRK-BUSINESS-CITY PIC X(30) VALUE SPACES.             
               25 WRK-BUSINESS-STATE PIC X(02) VALUE SPACES.            
              20 WRK-BUS-ZIP-POST-CODE PIC X(09) VALUE SPACES.          
             15 WRK-BUSINESS-COUNTY PIC X(30) VALUE SPACES.             
             15 WRK-BUS-ISO-COUNTRY-CODE                                
                                    PIC X(02) VALUE SPACES.             
             15 WRK-BUSINESS-EMAIL-ADDRESS                              
                                    PIC X(48) VALUE SPACES.             
             15 WRK-BUS-LANGUAGE-IND PIC X(02) VALUE SPACES.            
             15 WRK-EXT-BUS-REF-NUM     PIC X(03) VALUE SPACES.         
             15 WRK-BUS-SOCIAL-SEC-NUM  PIC X(20) VALUE SPACES.         
             15 WRK-EXT-BUS-TAX-ID-TYPE PIC X(01) VALUE SPACES.         
             15 WRK-TELEPHONE-NUMBER-DATA.                              
              20 WRK-PHONE-NUMBER-DATA OCCURS 6 TIMES.                  
               25 WRK-PHONE-TYPE-CODE PIC X VALUE SPACES.               
               25 WRK-PHONE-AVAILABILITY-CODE                           
                                    PIC X VALUE SPACES.                 
               25 WRK-PHONE-NUMBER.                                     
                30 WRK-PHONE-AREA-CODE PIC X(03) VALUE SPACES.          
                30 WRK-PHONE-PREFIX  PIC X(03) VALUE SPACES.            
                30 WRK-PHONE-SUFFIX  PIC X(04) VALUE SPACES.            
                30 WRK-PHONE-EXTENSION    PIC X(07) VALUE SPACES.       
               25 WRK-EXT-PHONE-NUMBER-FMT PIC X(02) VALUE SPACES.      
             15 WRK-CREDIT-LIMIT-AMT PIC S9(12) COMP-3 VALUE ZEROS.     
                                                                        
             15 WRK-EXPIRATION-DATE PIC 9(6) COMP-3 VALUE ZEROS.        
             15 WRK-BALANCE-AMT     PIC S9(12)V9(6) COMP-3 VALUE ZEROS. 
             15 WRK-OVERLIMIT-AMT   PIC S9(12)V9(6) COMP-3 VALUE ZEROS. 
             15 WRK-DISPUTED-AMT    PIC S9(12)V9(6) COMP-3 VALUE ZEROS. 
             15 WRK-TOTAL-DUE-AMT   PIC S9(12)V9(6) COMP-3 VALUE ZEROS. 
             15 WRK-CURRENT-DUE-AMT PIC S9(12)V9(6) COMP-3 VALUE ZEROS. 
             15 WRK-COLLECTOR-ASSIGN-UD-1                               
                                    PIC X(12) VALUE SPACES.             
             15 WRK-COLLECTOR-ASSIGN-UD-1-TYPE                          
                                    PIC X(02) VALUE SPACES.             
             15 WRK-COLLECTOR-ASSIGN-UD-1-FMT                           
                                    PIC 9(02) VALUE ZEROS.              
             15 WRK-COLLECTOR-ASSIGN-UD-2 PIC X(12) VALUE SPACES.       
             15 WRK-COLLECTOR-ASSIGN-UD-2-TYPE                          
                                    PIC X(02) VALUE SPACES.             
             15 WRK-COLLECTOR-ASSIGN-UD-2-FMT                           
                                    PIC 9(02) VALUE ZEROS.              
             15 WRK-TOTAL-DELINQ-AMT PIC S9(12)V9(6) COMP-3 VALUE ZEROS.
             15 WRK-CYCLES-DELINQUENT-CNT                               
                                    PIC S99 COMP-3 VALUE ZEROS.         
             15 WRK-DAYS-DELINQUENT-NUM PIC S9(03) COMP-3 VALUE ZEROS.  
             15 WRK-LAST-PAYMENT-AMT PIC S9(12)V9(6) COMP-3 VALUE ZEROS.
             15 WRK-CHARGE-OFF-DATE PIC 9(6) COMP-3 VALUE ZEROS.        
             15 WRK-DELQ-DAYS-AT-CHARGE-OFF                             
                                    PIC S9(03) COMP-3 VALUE ZEROS.      
             15 WRK-USER-DEFINED-7  PIC X(12) VALUE SPACES.             
             15 WRK-USER-DEFINED-7-TYPE PIC X(02) VALUE SPACES.         
             15 WRK-USER-DEFINED-7-FMT PIC 9(02) VALUE ZEROS.           
             15 WRK-ACCT-OPEN-DATE  PIC 9(06) VALUE ZEROS.              
             15 WRK-FILLER     REDEFINES WRK-ACCT-OPEN-DATE.            
              20 WRK-ORIGIN-YEAR    PIC 99.                             
              20 WRK-ANNIV-MONTH    PIC 99.                             
              20 WRK-ANNIV-DAY      PIC 99.                             
                                                                        
             15 WRK-IL-SHARED-DATA.                                     
              20 WRK-NEXT-DUE-DATE  PIC 9(06) COMP-3 VALUE ZEROS.       
              20 WRK-INTEREST-RATE  PIC S9(3)V9(3) COMP-3 VALUE ZEROS.  
              20 WRK-OTH-DELQ-AMT   PIC S9(12)V9(6) COMP-3 VALUE ZEROS. 
              20 WRK-REPOSSESSION-CHARGES                               
                                    PIC S9(12)V9(6) COMP-3 VALUE ZEROS. 
              20 WRK-REPOSSESSION-DATE PIC 9(06) COMP-3 VALUE ZEROS.    
              20 WRK-MATURITY-DATE  PIC 9(06) COMP-3 VALUE ZEROS.       
              20 WRK-PAYMENT-AMT    PIC S9(12)V9(6) COMP-3 VALUE ZEROS. 
              20 WRK-PRINCIPAL-DELQ-AMT                                 
                                    PIC S9(12)V9(6) COMP-3 VALUE ZEROS. 
              20 WRK-INTEREST-DELQ-AMT                                  
                                    PIC S9(12)V9(6) COMP-3 VALUE ZEROS. 
                                                                        
            10 WRK-ALPHA-SEARCH-DATA.                                   
             15 WRK-USER-DEFINED-SEARCH-1                               
                                   PIC X(10) VALUE SPACES.              
             15 WRK-USER-DEFINED-SEARCH-1-TYPE                          
                                   PIC X(02) VALUE SPACES.              
             15 WRK-USER-DEFINED-SEARCH-1-FMT                           
                                   PIC 9(02) VALUE ZEROS.               
             15 WRK-USER-DEFINED-SEARCH-2                               
                                   PIC X(10) VALUE SPACES.              
             15 WRK-USER-DEFINED-SEARCH-2-TYPE                          
                                   PIC X(02) VALUE SPACES.              
             15 WRK-USER-DEFINED-SEARCH-2-FMT                           
                                   PIC 9(02) VALUE ZEROS.               
            10 WRK-ON-LINE-DISPLAY-DATA.                                
             15 WRK-ACCT-NUM-SORT   PIC X(07) VALUE SPACES.             
             15 WRK-CYCLE-ID        PIC 99 VALUE ZEROS.                 
             15 WRK-UD-DATE-FIELD-1 PIC 9(06) VALUE ZEROS.              
             15 WRK-LAST-BILLING-DATE PIC 9(06) COMP-3 VALUE ZEROS.     
             15 WRK-AGED-HISTORY    PIC X(12) VALUE SPACES.             
             15 WRK-CREDIT-LIMIT-DATE PIC 9(06) COMP-3 VALUE ZEROS.     
             15 WRK-INT-AND-LATE-CHG-AMT                                
                                    PIC S9(12)V9(6) COMP-3 VALUE ZEROS. 
             15 WRK-DELINQ-AMT-BY-AGE PIC S9(12)V9(6) COMP-3 OCCURS 4.  
             15 WRK-NUMBER-OF-CARDS PIC X(2) VALUE SPACES.              
             15 WRK-WARNING-BULLETIN-DATE                               
                                    PIC 9(6) COMP-3 VALUE ZEROS.        
             15 WRK-WARNING-BULLETIN-ZONE                               
                                    PIC X(04) VALUE SPACES.             
             15 WRK-OTHER-ACCT-NUM  PIC X(18) VALUE SPACES.             
             15 WRK-CREDIT-SCORE    PIC X(05) VALUE SPACES.             
             15 WRK-BEHAVIOR-INDEX  PIC X(04) VALUE SPACES.             
             15 WRK-LAST-PAYMENT-DATE PIC 9(06) COMP-3 VALUE ZEROS.     
             15 WRK-LAST-MONETARY-AMT                                   
                                     PIC S9(12)V9(6) COMP-3 VALUE ZEROS.
             15 WRK-LAST-MONETARY-DATE PIC 9(06) COMP-3 VALUE ZEROS.    
             15 WRK-LAST-MONETARY-TYPE PIC XX VALUE SPACES.             
             15 WRK-CHARGE-OFF-AMT  PIC S9(12)V9(6) COMP-3 VALUE ZEROS. 
             15 WRK-SORT-VALUE-DATA.                                    
              20 WRK-SORT-VALUE-1   PIC 9(12) VALUE ZEROS.              
              20 WRK-SORT-VALUE-1-TYPE PIC X(02) VALUE SPACES.          
              20 WRK-SORT-VALUE-1-FMT PIC 9(02) VALUE ZEROS.            
              20 WRK-SORT-VALUE-2   PIC X(12) VALUE SPACES.             
              20 WRK-SORT-VALUE-2-TYPE PIC X(02) VALUE SPACES.          
              20 WRK-SORT-VALUE-2-FMT PIC 9(02) VALUE ZEROS.            
             15 WRK-USER-DEFINED-DATA.                                  
              20 WRK-USER-DEFINED-1      PIC X(17) VALUE SPACES.        
              20 WRK-USER-DEFINED-1-TYPE PIC X(02) VALUE SPACES.        
              20 WRK-USER-DEFINED-1-FMT  PIC 9(02) VALUE ZEROS.         
              20 WRK-USER-DEFINED-2      PIC X(30) VALUE SPACES.        
              20 WRK-USER-DEFINED-2-TYPE PIC X(02) VALUE SPACES.        
              20 WRK-USER-DEFINED-2-FMT  PIC 9(02) VALUE ZEROS.         
              20 WRK-USER-DEFINED-3      PIC X(14) VALUE SPACES.        
              20 WRK-USER-DEFINED-3-TYPE PIC X(02) VALUE SPACES.        
              20 WRK-USER-DEFINED-3-FMT  PIC 9(02) VALUE ZEROS.         
              20 WRK-USER-DEFINED-4      PIC X(79) VALUE SPACES.        
              20 WRK-USER-DEFINED-4-TYPE PIC X(02) VALUE SPACES.        
              20 WRK-USER-DEFINED-4-FMT  PIC 9(02) VALUE ZEROS.         
              20 WRK-USER-DEFINED-5      PIC X(79) VALUE SPACES.        
              20 WRK-USER-DEFINED-5-TYPE PIC X(02) VALUE SPACES.        
              20 WRK-USER-DEFINED-5-FMT  PIC 9(02) VALUE ZEROS.         
              20 WRK-USER-DEFINED-6      PIC X(12) VALUE SPACES.        
              20 WRK-USER-DEFINED-6-TYPE PIC X(02) VALUE SPACES.        
              20 WRK-USER-DEFINED-6-FMT  PIC 9(02) VALUE ZEROS.         
              20 WRK-USER-DEFINED-8      PIC X(79) OCCURS 5 TIMES.      
              20 WRK-USER-DEFINED-8-TYPE PIC X(02) OCCURS 5 TIMES.      
              20 WRK-USER-DEFINED-8-FMT  PIC 9(02) OCCURS 5 TIMES.      
             15 WRK-IL-DISPLAY-DATA.                                    
              20 WRK-PRINCIPAL-AMT  PIC S9(12)V9(6) COMP-3 VALUE ZEROS. 
              20 WRK-OTHER-AMT      PIC S9(12)V9(6) COMP-3 VALUE ZEROS. 
              20 WRK-ORIG-BALANCE-AMT                                   
                                    PIC S9(12)V9(6) COMP-3 VALUE ZEROS. 
              20 WRK-OFFICER-NUM    PIC X(9) VALUE SPACES.              
              20 WRK-BRANCH-NUM     PIC X(06) VALUE SPACES.             
              20 WRK-ORIGINAL-TERM  PIC X(04) VALUE SPACES.             
              20 WRK-ORIG-INTEREST-RATE                                 
                                    PIC S9(3)V9(3) COMP-3  VALUE ZEROS. 
              20 WRK-DEALER-NUM     PIC X(07) VALUE SPACES.             
              20 WRK-PAYMENT-FREQUENCY PIC X(2) VALUE SPACES.           
              20 WRK-REFINANCE-DATE PIC 9(6) COMP-3 VALUE ZEROS.        
            15 WRK-UTILITY-DISPLAY-DATA.                                
              20 WRK-DEPOSIT-IND     PIC X VALUE SPACES.                
              20 WRK-ON-BUDGET-IND   PIC X VALUE SPACES.                
              20 WRK-DPA-IND         PIC X VALUE SPACES.                
              20 WRK-CUT-OFF-DATE    PIC 9(06) COMP-3 VALUE ZEROS.      
              20 WRK-CUT-OFF-CODE    PIC X(03) VALUE SPACES.            
              20 WRK-ACCOUNT-TYPE    PIC X(03) VALUE SPACES.            
              20 WRK-SERVICE-TYPE    PIC X(03) VALUE SPACES.            
              20 WRK-BUDGET-PAYMENT-AMT                                 
                                    PIC S9(12)V9(6) COMP-3  VALUE ZEROS.
              20 WRK-TERMINATE-AMT   PIC S9(12)V9(6) COMP-3 VALUE ZEROS.
              20 WRK-TERMINATE-EXP-DATE PIC 9(6) COMP-3 VALUE ZEROS.    
            10 WRK-STATE-ASSIGNMENT-DATA.                               
               15 WRK-SAT-USER-DEFINED-1 PIC X(12) VALUE SPACES.        
               15 WRK-SAT-USER-DEFINED-1-TYPE                           
                                       PIC X(02) VALUE SPACES.          
               15 WRK-SAT-USER-DEFINED-1-FMT                            
                                       PIC 9(02) VALUE ZEROS.           
               15 WRK-SAT-USER-DEFINED-2 PIC X(12) VALUE SPACES.        
               15 WRK-SAT-USER-DEFINED-2-TYPE                           
                                       PIC X(02) VALUE SPACES.          
               15 WRK-SAT-USER-DEFINED-2-FMT                            
                                       PIC 9(02) VALUE ZEROS.           
            10 WRK-MULTIPLE-ACT-PROCESING-DAT.                          
               15 WRK-CUSTOMER-INFO-NUMBER PIC X(24) VALUE SPACES.      
            10 WRK-USER-DEFINED-LETTER-DATA.                            
               15 WRK-LETTER-USER-DEFINED-1 PIC X(30) VALUE SPACES.     
               15 WRK-LETTER-USER-DEFINED-1-TYPE                        
                                          PIC X(02) VALUE SPACES.       
               15 WRK-LETTER-USER-DEFINED-1-FMT                         
                                          PIC 9(02) VALUE ZEROS.        
               15 WRK-LETTER-USER-DEFINED-2 PIC X(30) VALUE SPACES.     
               15 WRK-LETTER-USER-DEFINED-2-TYPE                        
                                          PIC X(02) VALUE SPACES.       
               15 WRK-LETTER-USER-DEFINED-2-FMT                         
                                          PIC 9(02) VALUE ZEROS.        
               15 WRK-LETTER-USER-DEFINED-3 PIC X(30) VALUE SPACES.     
               15 WRK-LETTER-USER-DEFINED-3-TYPE                        
                                          PIC X(02) VALUE SPACES.       
               15 WRK-LETTER-USER-DEFINED-3-FMT                         
                                          PIC 9(02) VALUE ZEROS.        
               15 WRK-LETTER-USER-DEFINED-4 PIC X(30) VALUE SPACES.     
               15 WRK-LETTER-USER-DEFINED-4-TYPE                        
                                          PIC X(02) VALUE SPACES.       
               15 WRK-LETTER-USER-DEFINED-4-FMT                         
                                          PIC 9(02) VALUE ZEROS.        
               15 WRK-LETTER-USER-DEFINED-5 PIC X(30) VALUE SPACES.     
               15 WRK-LETTER-USER-DEFINED-5-TYPE                        
                                          PIC X(02) VALUE SPACES.       
               15 WRK-LETTER-USER-DEFINED-5-FMT                         
                                          PIC 9(02) VALUE ZEROS.        
               15 WRK-LETTER-USER-DEFINED-6 PIC X(30) VALUE SPACES.     
               15 WRK-LETTER-USER-DEFINED-6-TYPE                        
                                          PIC X(02) VALUE SPACES.       
               15 WRK-LETTER-USER-DEFINED-6-FMT                         
                                          PIC 9(02) VALUE ZEROS.        
               15 WRK-LETTER-USER-DEFINED-7 PIC X(30) VALUE SPACES.     
               15 WRK-LETTER-USER-DEFINED-7-TYPE                        
                                          PIC X(02) VALUE SPACES.       
               15 WRK-LETTER-USER-DEFINED-7-FMT                         
                                          PIC 9(02) VALUE ZEROS.        
               15 WRK-LETTER-USER-DEFINED-8                             
                                     PIC S9(12)V9(6) COMP-3 VALUE ZEROS.
               15 WRK-LETTER-USER-DEFINED-8-TYPE                        
                                          PIC X(02) VALUE SPACES.       
               15 WRK-LETTER-USER-DEFINED-8-FMT                         
                                          PIC 9(02) VALUE ZEROS.        
               15 WRK-LETTER-USER-DEFINED-9                             
                                     PIC S9(12)V9(6) COMP-3 VALUE ZEROS.
               15 WRK-LETTER-USER-DEFINED-9-TYPE                        
                                          PIC X(02) VALUE SPACES.       
               15 WRK-LETTER-USER-DEFINED-9-FMT                         
                                          PIC 9(02) VALUE ZEROS.        
               15 WRK-LETTER-USER-DEFINED-10                            
                                     PIC S9(12)V9(6) COMP-3 VALUE ZEROS.
               15 WRK-LETER-USER-DEFINED-10-TYPE                        
                                          PIC X(02) VALUE SPACES.       
               15 WRK-LETTER-USER-DEFINED-10-FMT                        
                                          PIC 9(02) VALUE ZEROS.        
            10 WRK-AMS-ACTG-DATA.                                       
               15 WRK-BANK-NUM         PIC S9(05) COMP-3 VALUE ZEROS.   
               15 WRK-APP-CODE         PIC S9(03) COMP-3 VALUE ZEROS.   
            10 WRK-ESTIMATED-MONTHLY-PAYMENT                            
                                     PIC S9(12)V9(6) COMP-3 VALUE ZEROS.
            10 WRK-STRATA-FIELDS.                                       
               15 WRK-ACI-INDICATOR    PIC X VALUE SPACES.              
               15 WRK-SERVICING-SYSTEM PIC X(08) VALUE SPACES.          
            10 WRK-GLOBAL-DATA.                                         
               15 WRK-NATL-CURRENCY    PIC X(03) VALUE SPACES.          
               15 WRK-BASE-CURRENCY    PIC X(03) VALUE SPACES.          
               15 WRK-PREF-CURRENCY    PIC X(03) VALUE SPACES.          
            10 WRK-OTHER-ACCTS OCCURS  10 TIMES.                        
               15 WRK-ACCT-TYPE-CODE      PIC X(02) VALUE SPACES.       
               15 WRK-LABEL-1-CODE        PIC X(02) VALUE SPACES.       
               15 WRK-STRING-VALUE-1      PIC X(18) VALUE SPACES.       
               15 WRK-CURR-VALUE-1 REDEFINES                            
                  WRK-STRING-VALUE-1      PIC S9(12)V9(6) COMP-3.       
               15 WRK-DATE-VALUE-1 REDEFINES                            
                  WRK-STRING-VALUE-1      PIC 9(08).                    
               15 WRK-VALUE-1-TYPE-CODE   PIC X(01) VALUE SPACES.       
               15 WRK-LABEL-2-CODE        PIC X(02) VALUE SPACES.       
               15 WRK-STRING-VALUE-2      PIC X(18) VALUE SPACES.       
               15 WRK-CURR-VALUE-2 REDEFINES                            
                  WRK-STRING-VALUE-2      PIC S9(12)V9(6) COMP-3.       
               15 WRK-DATE-VALUE-2 REDEFINES                            
                  WRK-STRING-VALUE-2      PIC 9(08).                    
               15 WRK-VALUE-2-TYPE-CODE   PIC X(01) VALUE SPACES.       
            10 WRK-CUSTOMER-SINCE-DATE    PIC 9(06) COMP-3 VALUE ZEROS. 
            10 WRK-VALUE-CODE             PIC X(02) VALUE SPACES.       
            10 WRK-RISK-CODE              PIC X(02) VALUE SPACES.       
            10 WRK-RECOMMENDED-ACTION-CODE PIC X(02) VALUE SPACES.      
            10 WRK-TIME-ZONE              PIC X(05) VALUE SPACES.       
            10 WRK-ACTG-SYSTEM-ID         PIC X(04) VALUE SPACES.       
            10 WRK-ACCOUNT-STATUS-IND.                                  
               15 WRK-ACCT-STATUS-CODE-1  PIC X(01) VALUE SPACES.       
               15 WRK-ACCT-STATUS-CODE-2  PIC X(01) VALUE SPACES.       
               15 WRK-ACCT-STATUS-CODE-3  PIC X(01) VALUE SPACES.       
               15 WRK-ACCT-STATUS-CODE-4  PIC X(01) VALUE SPACES.       
               15 WRK-ACCT-STATUS-CODE-5  PIC X(01) VALUE SPACES.       
               15 WRK-ACCT-STATUS-CODE-6  PIC X(01) VALUE SPACES.       
               15 WRK-ACCT-STATUS-CODE-7  PIC X(01) VALUE SPACES.       
               15 WRK-ACCT-STATUS-CODE-8  PIC X(01) VALUE SPACES.       
               15 WRK-ACCT-STATUS-CODE-9  PIC X(01) VALUE SPACES.       
               15 WRK-ACCT-STATUS-CODE-10 PIC X(01) VALUE SPACES.       
               15 WRK-LEGAL-STATUS-CODE   PIC X(03) VALUE SPACES.       
            10 WRK-CAS-SPECIFIC-DATA.                                   
               15 WRK-RECOVERED-TOTALS.                                 
                  20 WRK-REC-INT-MTD-AMT                                
                                    PIC S9(12)V9(6) COMP-3 VALUE ZEROS. 
                  20 WRK-REC-PRIN-MTD-AMT                               
                                    PIC S9(12)V9(6) COMP-3 VALUE ZEROS. 
                  20 WRK-REC-TOT-COST-MTD-AMT                           
                                    PIC S9(12)V9(6) COMP-3 VALUE ZEROS. 
               15 WRK-JUDGMENT-DATA.                                    
                  20 WRK-JGMT-EFFECTIVE-DATE                            
                                           PIC 9(6) COMP-3 VALUE ZEROS. 
                  20 WRK-JGMT-INT-AMT                                   
                                    PIC S9(12)V9(6) COMP-3 VALUE ZEROS. 
                  20 WRK-JGMT-PRIN-AMT                                  
                                    PIC S9(12)V9(6) COMP-3 VALUE ZEROS. 
                  20 WRK-JGMT-TOT-COST-AMT                              
                                    PIC S9(12)V9(6) COMP-3 VALUE ZEROS. 
                  20 WRK-ADDL-JGMTS-IND     PIC X(01) VALUE SPACES.     
               15 WRK-RECOVERY-COST-DATA.                               
                  20 WRK-REC-COSTS-IN-BAL-AMT                           
                                    PIC S9(12)V9(6) COMP-3 VALUE ZEROS. 
                  20 WRK-REC-COSTS-NOT-IN-BAL-AMT                       
                                    PIC S9(12)V9(6) COMP-3 VALUE ZEROS. 
                  20 WRK-REC-COSTS-ASSESSED-MTD-AMT                     
                                    PIC S9(12)V9(6) COMP-3 VALUE ZEROS. 
                  20 WRK-REC-COSTS-WAIVED-MTD-AMT                       
                                    PIC S9(12)V9(6) COMP-3 VALUE ZEROS. 
                  20 WRK-REC-COSTS-CAPTLZED-LTD-AMT                     
                                    PIC S9(12)V9(6) COMP-3 VALUE ZEROS. 
                  20 WRK-REC-COSTS-BUCKETS.                             
                      25 WRK-REC-COST-1-CAT PIC X(03) VALUE SPACES.     
                      25 WRK-REC-COST-1-AMT                             
                                    PIC S9(12)V9(6) COMP-3 VALUE ZEROS. 
                      25 WRK-REC-COST-2-CAT PIC X(03) VALUE SPACES.     
                      25 WRK-REC-COST-2-AMT                             
                                    PIC S9(12)V9(6) COMP-3 VALUE ZEROS. 
                      25 WRK-REC-COST-3-CAT PIC X(03) VALUE SPACES.     
                      25 WRK-REC-COST-3-AMT                             
                                    PIC S9(12)V9(6) COMP-3 VALUE ZEROS. 
                      25 WRK-REC-COST-4-CAT PIC X(03) VALUE SPACES.     
                      25 WRK-REC-COST-4-AMT                             
                                    PIC S9(12)V9(6) COMP-3 VALUE ZEROS. 
                      25 WRK-REC-COST-5-CAT PIC X(03) VALUE SPACES.     
                      25 WRK-REC-COST-5-AMT                             
                                    PIC S9(12)V9(6) COMP-3 VALUE ZEROS. 
                      25 WRK-REC-COST-6-CAT PIC X(03) VALUE SPACES.     
                      25 WRK-REC-COST-6-AMT                             
                                    PIC S9(12)V9(6) COMP-3 VALUE ZEROS. 
                      25 WRK-REC-COST-7-CAT PIC X(03) VALUE SPACES.     
                      25 WRK-REC-COST-7-AMT                             
                                    PIC S9(12)V9(6) COMP-3 VALUE ZEROS. 
                      25 WRK-REC-COST-8-CAT PIC X(03) VALUE SPACES.     
                      25 WRK-REC-COST-8-AMT                             
                                    PIC S9(12)V9(6) COMP-3 VALUE ZEROS. 
                      25 WRK-REC-COST-9-CAT PIC X(03) VALUE SPACES.     
                      25 WRK-REC-COST-9-AMT                             
                                     PIC S9(12)V9(6) COMP-3 VALUE ZEROS.
                      25 WRK-REC-COST-10-CAT PIC X(03) VALUE SPACES.    
                      25 WRK-REC-COST-10-AMT                            
                                     PIC S9(12)V9(6) COMP-3 VALUE ZEROS.
                      25 WRK-REC-COST-11-CAT PIC X(03) VALUE SPACES.    
                      25 WRK-REC-COST-11-AMT                            
                                     PIC S9(12)V9(6) COMP-3 VALUE ZEROS.
                      25 WRK-REC-COST-12-CAT PIC X(03) VALUE SPACES.    
                      25 WRK-REC-COST-12-AMT                            
                                     PIC S9(12)V9(6) COMP-3 VALUE ZEROS.
                      25 WRK-REC-COST-13-CAT PIC X(03) VALUE SPACES.    
                      25 WRK-REC-COST-13-AMT                            
                                     PIC S9(12)V9(6) COMP-3 VALUE ZEROS.
                      25 WRK-REC-COST-14-CAT PIC X(03) VALUE SPACES.    
                      25 WRK-REC-COST-14-AMT                            
                                     PIC S9(12)V9(6) COMP-3 VALUE ZEROS.
                      25 WRK-REC-COST-15-CAT PIC X(03) VALUE SPACES.    
                      25 WRK-REC-COST-15-AMT                            
                                     PIC S9(12)V9(6) COMP-3 VALUE ZEROS.
                      25 WRK-REC-COST-16-CAT PIC X(03) VALUE SPACES.    
                      25 WRK-REC-COST-16-AMT                            
                                     PIC S9(12)V9(6) COMP-3 VALUE ZEROS.
                      25 WRK-REC-COST-17-CAT PIC X(03) VALUE SPACES.    
                      25 WRK-REC-COST-17-AMT                            
                                     PIC S9(12)V9(6) COMP-3 VALUE ZEROS.
                      25 WRK-REC-COST-18-CAT PIC X(03) VALUE SPACES.    
                      25 WRK-REC-COST-18-AMT                            
                                     PIC S9(12)V9(6) COMP-3 VALUE ZEROS.
                      25 WRK-REC-COST-19-CAT PIC X(03) VALUE SPACES.    
                      25 WRK-REC-COST-19-AMT                            
                                     PIC S9(12)V9(6) COMP-3 VALUE ZEROS.
                      25 WRK-REC-COST-20-CAT PIC X(03) VALUE SPACES.    
                      25 WRK-REC-COST-20-AMT                            
                                     PIC S9(12)V9(6) COMP-3 VALUE ZEROS.
               15 WRK-LOAN-POOL-NUM        PIC S9(7) COMP-3 VALUE ZEROS.
               15 WRK-LOAN-POOL-SOLD-DATE   PIC 9(6) COMP-3 VALUE ZEROS.
               15 WRK-ORIG-PORTFOLIO-TYPE   PIC X(01) VALUE SPACES.     
               15 WRK-NUM-DAYS-SINCE-CHARGE-OFF                         
                                     PIC S9(05) COMP-3 VALUE ZEROS.     
               15 WRK-LAST-EXT-UPD-DATE     PIC 9(6) COMP-3 VALUE ZEROS.
               15 WRK-LEGAL-STATE           PIC X(03) VALUE SPACES.     
               15 WRK-CHARGE-OFF-CHARGE-DOWN-IND PIC 9(01) VALUE ZEROS. 
               15 WRK-INVESTOR-NUM     PIC S9(7) COMP-3 VALUE ZEROS.    
            10 WRK-EXT-RANDOM-NUM           PIC 9(03) VALUE ZEROS.      
            10 WRK-FILLER                   PIC X(22) VALUE SPACES.     
                                                                        
       77  ACU-LIDOS-ARQEXTRC          PIC  9(09) COMP-3 VALUE ZEROS.   
       77  ACU-LIDOS-BDRPTRAN          PIC  9(09) COMP-3 VALUE ZEROS.   
       77  ACU-GRAVA-ARQSAIDA          PIC  9(09) COMP-3 VALUE ZEROS.   
       77  ACU-GRAVA-BDRPSCOR          PIC  9(09) COMP-3 VALUE ZEROS.   
       77  ACU-GRAVA-EXTRCCOR          PIC  9(09) COMP-3 VALUE ZEROS.   
       77  ACU-GRAVA-EXTRSCOR          PIC  9(09) COMP-3 VALUE ZEROS.   
                                                                        
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
                                                                        
       01  WRK-BASE-EXTRACT.                                            
           03  FILLER                  PIC  X(443)  VALUE SPACES.       
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
                                                                        
      *--- MSG ERRO FILE STATUS --------------------------------------* 
                                                                        
       01  WRK-MSG-FS-ERRO.                                             
           03  FILLER                 PIC  X(10) VALUE SPACES.          
           03  FILLER                 PIC  X(05) VALUE 'ERRO '.         
           03  WRK-OPERACAO-TXT       PIC  X(13) VALUE SPACES.          
           03  FILLER                 PIC  X(12) VALUE ' DO ARQUIVO '.  
           03  WRK-NOME-ARQ           PIC  X(08) VALUE SPACES.          
           03  FILLER                 PIC  X(14) VALUE 'FILE STATUS = '.
           03  WRK-FILE-STATUS        PIC  X(02) VALUE SPACES.          
                                                                        
      *--- AREA POOL7100 ---------------------------------------------* 
                                                                        
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
           03 WRK-LIT-OBJETIVO         PIC  X(64) VALUE                 
           'INATIVAR CONTAS QUE APRESENTEM O ERRO 260 NO BDRPTRAN'.     
           03 WRK-LIT-PROGRAMA         PIC  X(08) VALUE 'RDAB7480'.     
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
                                                                        
      *---------------------------------------------------------------* 
                                                                        
       01   FILLER                     PIC  X(32) VALUE                 
            '*   FIM  DA WORKING RDAB7480   *'.                         
                                                                        
           EJECT                                                        
      *===============================================================* 
       PROCEDURE                       DIVISION.                        
      *===============================================================* 
                                                                        
      *---------------------------------------------------------------* 
       0000-ROTINA-PRINCIPAL           SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           PERFORM 1000-INICIALIZAR.                                    
                                                                        
           PERFORM 1200-FORMATAR-DATA-HORA.                             
                                                                        
           PERFORM 2000-PROCESSAR                                       
             UNTIL WRK-CHV-BDRPTRAN    EQUAL  HIGH-VALUE AND
                   WRK-CHV-ARQEXTRC    EQUAL  HIGH-VALUE.
                                                                        
           PERFORM 3000-FINALIZAR.                                      
                                                                        
      *---------------------------------------------------------------* 
       0000-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       1000-INICIALIZAR                SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           OPEN    INPUT   ARQEXTRC                                     
                           BDRPTRAN                                     
                   OUTPUT  ARQSAIDA                                     
                           BDRPSCOR.                                    
                                                                        
           MOVE    WRK-ABERTURA  TO  WRK-OPERACAO.                      
           PERFORM 1100-TESTAR-FILE-STATUS.                             
                                                                        
           PERFORM 1300-PRIMEIRA-LEITURA.                               
                                                                        
      *---------------------------------------------------------------* 
       1000-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       1100-TESTAR-FILE-STATUS         SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           PERFORM 1110-TESTAR-FS-ARQEXTRC.                             
                                                                        
           PERFORM 1120-TESTAR-FS-BDRPTRAN.                             
                                                                        
           PERFORM 1130-TESTAR-FS-ARQSAIDA.                             
                                                                        
           PERFORM 1140-TESTAR-FS-BDRPSCOR.                             
                                                                        
      *---------------------------------------------------------------* 
       1100-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       1110-TESTAR-FS-ARQEXTRC         SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           IF  WRK-FS-ARQEXTRC         NOT EQUAL '00'  AND              
                                       NOT EQUAL '10'                   
               DISPLAY '************** RDAB7480 *************'          
               DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'      
               DISPLAY '*             ARQEXTRC              *'          
               DISPLAY '*         FILE STATUS =  ' WRK-FS-ARQEXTRC      
                                                  '         *'          
               DISPLAY '*         PROGRAMA CANCELADO        *'          
               DISPLAY '************** RDAB7480 *************'          
               MOVE   'ARQEXTRC'        TO  WRK-NOME-ARQ                
               MOVE WRK-OPERACAO        TO  WRK-OPERACAO-TXT            
               MOVE WRK-FS-ARQEXTRC     TO  WRK-FILE-STATUS             
               MOVE  'APL'              TO  ERR-TIPO-ACESSO             
               MOVE WRK-MSG-FS-ERRO     TO  ERR-TEXTO                   
               PERFORM 9999-ROTINA-ERRO                                 
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       1110-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       1120-TESTAR-FS-BDRPTRAN         SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           IF  WRK-FS-BDRPTRAN         NOT EQUAL '00' AND               
                                       NOT EQUAL '10'                   
               DISPLAY '************** RDAB7480 *************'          
               DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'      
               DISPLAY '*             BDRPTRAN              *'          
               DISPLAY '*         FILE STATUS =  ' WRK-FS-BDRPTRAN      
                                                  '         *'          
               DISPLAY '*         PROGRAMA CANCELADO        *'          
               DISPLAY '************** RDAB7480 *************'          
               MOVE 'BDRPTRAN'         TO  WRK-NOME-ARQ                 
               MOVE  WRK-OPERACAO      TO  WRK-OPERACAO-TXT             
               MOVE  WRK-FS-BDRPTRAN   TO  WRK-FILE-STATUS              
               MOVE 'APL'              TO  ERR-TIPO-ACESSO              
               MOVE  WRK-MSG-FS-ERRO   TO  ERR-TEXTO                    
               PERFORM 9999-ROTINA-ERRO                                 
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       1120-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       1130-TESTAR-FS-ARQSAIDA         SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           IF  WRK-FS-ARQSAIDA         NOT EQUAL '00' AND               
                                       NOT EQUAL '10'                   
               DISPLAY '************** RDAB7480 *************'          
               DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'      
               DISPLAY '*             ARQSAIDA              *'          
               DISPLAY '*         FILE STATUS =  ' WRK-FS-ARQSAIDA      
                                                  '         *'          
               DISPLAY '*         PROGRAMA CANCELADO        *'          
               DISPLAY '************** RDAB7480 *************'          
               MOVE WRK-OPERACAO       TO  WRK-OPERACAO-TXT             
               MOVE 'ARQSAIDA'         TO  WRK-NOME-ARQ                 
               MOVE  WRK-FS-ARQSAIDA   TO  WRK-FILE-STATUS              
               MOVE 'APL'              TO  ERR-TIPO-ACESSO              
               MOVE  WRK-MSG-FS-ERRO   TO  ERR-TEXTO                    
               PERFORM 9999-ROTINA-ERRO                                 
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       1130-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       1140-TESTAR-FS-BDRPSCOR         SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           IF  WRK-FS-BDRPSCOR         NOT EQUAL '00' AND               
                                       NOT EQUAL '10'                   
               DISPLAY '************** RDAB7480 *************'          
               DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'      
               DISPLAY '*             BDRPSCOR              *'          
               DISPLAY '*         FILE STATUS =  ' WRK-FS-BDRPSCOR      
                                                  '         *'          
               DISPLAY '*         PROGRAMA CANCELADO        *'          
               DISPLAY '************** RDAB7480 *************'          
               MOVE WRK-OPERACAO       TO  WRK-OPERACAO-TXT             
               MOVE 'BDRPSCOR'         TO  WRK-NOME-ARQ                 
               MOVE  WRK-FS-BDRPSCOR   TO  WRK-FILE-STATUS              
               MOVE 'APL'              TO  ERR-TIPO-ACESSO              
               MOVE  WRK-MSG-FS-ERRO   TO  ERR-TEXTO                    
               PERFORM 9999-ROTINA-ERRO                                 
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       1140-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
           EJECT                                                        
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
           EJECT                                                        
      *----------------------------------------------------------------*
       1300-PRIMEIRA-LEITURA           SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           PERFORM 1310-PRIMEIRA-LEITURA-ARQEXTRC.                      
                                                                        
           PERFORM 1320-PRIMEIRA-LEITURA-BDRPTRAN.                      
                                                                        
      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
           EJECT                                                        
      *----------------------------------------------------------------*
       1310-PRIMEIRA-LEITURA-ARQEXTRC  SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           PERFORM 1400-LER-ARQEXTRC.                                   
                                                                        
           IF  ACU-LIDOS-ARQEXTRC      EQUAL  ZEROS                     
               DISPLAY '************ RDAB7480 ************'             
               DISPLAY '*                                *'             
               DISPLAY '*     ARQUIVO ARQEXTRC VAZIO     *'             
               DISPLAY '*                                *'             
               DISPLAY '************ RDAB7480 ************'             
           END-IF.                                                      
                                                                        
           MOVE  REG-BASE-EXTRACT      TO  WRK-BASE-EXTRACT.            
                                                                        
           MOVE  WRK-CPFCNPJ-BASE-AUX  TO  WRK-CPFCNPJ-BASE
                                           WRK-CUSTOMER-INFO-NUM.
           MOVE  WRK-CPF-CONTROL-AUX   TO  WRK-CPF-CONTROL.             
                                                                        
      *----------------------------------------------------------------*
       1310-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
           EJECT                                                        
      *----------------------------------------------------------------*
       1320-PRIMEIRA-LEITURA-BDRPTRAN  SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           PERFORM 1500-LER-BDRPTRAN.                                   
                                                                        
           IF  ACU-LIDOS-BDRPTRAN      EQUAL  ZEROS                     
               DISPLAY '************ RDAB7480 ************'             
               DISPLAY '*                                *'             
               DISPLAY '*     ARQUIVO BDRPTRAN VAZIO     *'             
               DISPLAY '*                                *'             
               DISPLAY '************ RDAB7480 ************'             
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       1320-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
           EJECT                                                        
      *---------------------------------------------------------------* 
       1400-LER-ARQEXTRC               SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           READ    ARQEXTRC.                                            
                                                                        
           IF   WRK-FS-ARQEXTRC EQUAL '10'                              
                MOVE  HIGH-VALUE       TO  WRK-CHV-ARQEXTRC             
                GO                     TO  1400-99-FIM                  
           END-IF.                                                      
                                                                        
           MOVE    WRK-LEITURA         TO  WRK-OPERACAO.                
           PERFORM 1110-TESTAR-FS-ARQEXTRC.                             
                                                                        
           ADD     1                   TO  ACU-LIDOS-ARQEXTRC.          
                                                                        
           MOVE  LOCATION-CODE         OF  ARQEXTRC                     
                                       TO  WRK-EXT-LOCATION-CODE.       
           MOVE  ACCT-NUM              OF  ARQEXTRC                     
                                       TO  WRK-EXT-ACCT-NUM.            
                                                                        
      *---------------------------------------------------------------* 
       1400-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       1500-LER-BDRPTRAN               SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           READ    BDRPTRAN.                                            
                                                                        
           IF   WRK-FS-BDRPTRAN        EQUAL '10'                       
                MOVE  HIGH-VALUE       TO  WRK-CHV-BDRPTRAN             
                GO                     TO  1500-99-FIM                  
           END-IF.                                                      
                                                                        
           MOVE    WRK-LEITURA         TO  WRK-OPERACAO.                
           PERFORM 1120-TESTAR-FS-BDRPTRAN.                             
                                                                        
           ADD     1                   TO  ACU-LIDOS-BDRPTRAN.          
                                                                        
           MOVE  FD-LOCATION-MODE      OF  BDRPTRAN                     
                                       TO  WRK-CAC-LOCATION-CODE.       
           MOVE  FD-ACCT-NUM           OF  BDRPTRAN                     
                                       TO  WRK-CAC-ACCT-NUM.            
                                                                        
      *---------------------------------------------------------------* 
       1500-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       2000-PROCESSAR                  SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-CHV-ARQEXTRC         EQUAL  WRK-CHV-BDRPTRAN          
              MOVE   REG-BASE-EXTRACT  TO   WRK-REG-BASE-EXTRACT        
                                                                        
              IF WRK-SEARCH-NAME    EQUAL 'INATIVAR CONTA'              
                 MOVE SPACES                                            
                     TO WRK-SOCIAL-SECURITY-NUM  OF WRK-REG-BASE-EXTRACT
                        WRK-CUSTOMER-INFO-NUMBER OF WRK-REG-BASE-EXTRACT
                 MOVE WRK-CPFCNPJ-BASE-AUX                              
                     TO WRK-SOCIAL-SECURITY-NUM  OF WRK-REG-BASE-EXTRACT
                        WRK-CUSTOMER-INFO-NUMBER OF WRK-REG-BASE-EXTRACT
              END-IF                                                    
              MOVE 'INATIVAR CONTA3'   TO   WRK-CUSTOMER-NAME
              MOVE 'INATIVAR CONTA3'   TO   WRK-SEARCH-NAME
              MOVE 'INATIVAR CONTA3'   TO   WRK-CUSTOMER-ADDRESS
              MOVE ZEROS               TO   WRK-TELEPHONE-NUMBER-DATA   
              MOVE ZEROS               TO   WRK-TOTAL-DELINQ-AMT        
              MOVE ZEROS               TO   WRK-DISPUTED-AMT            
              MOVE ZEROS               TO   WRK-INTEREST-DELQ-AMT       
              MOVE ZEROS               TO   WRK-OTH-DELQ-AMT            
              MOVE ZEROS               TO   WRK-REPOSSESSION-CHARGES    
              MOVE ZEROS               TO   WRK-TOTAL-DUE-AMT           
                                                                        
              PERFORM  2100-GRAVAR-ARQSAIDA                             
              ADD  1                   TO  ACU-GRAVA-EXTRCCOR           
              PERFORM  1400-LER-ARQEXTRC                                
              PERFORM  1500-LER-BDRPTRAN                                
           ELSE                                                         
              IF WRK-CHV-ARQEXTRC      GREATER  WRK-CHV-BDRPTRAN        
                 MOVE WRK-BASE-EXTRACT    TO   WRK-REG-BASE-EXTRACT     
                 MOVE WRK-CAC-LOCATION-CODE TO WRK-LOCATION-CODE        
                 MOVE WRK-CAC-ACCT-NUM    TO   WRK-ACCT-NUM             
                 MOVE 'INATIVAR CONTA1'   TO   WRK-CUSTOMER-NAME
                 MOVE 'INATIVAR CONTA1'   TO   WRK-SEARCH-NAME
                 MOVE 'INATIVAR CONTA1'   TO   WRK-CUSTOMER-ADDRESS
                 MOVE ZEROS               TO   WRK-TELEPHONE-NUMBER-DATA
                 MOVE ZEROS               TO   WRK-TOTAL-DELINQ-AMT     
                 MOVE ZEROS               TO   WRK-DISPUTED-AMT         
                 MOVE ZEROS               TO   WRK-INTEREST-DELQ-AMT    
                 MOVE ZEROS               TO   WRK-OTH-DELQ-AMT         
                 MOVE ZEROS               TO   WRK-REPOSSESSION-CHARGES 
                 MOVE ZEROS               TO   WRK-TOTAL-DUE-AMT        
                                                                        
                 PERFORM  2100-GRAVAR-ARQSAIDA                          
                                                                        
                 MOVE FD-DETAIL-TRANS-ERROR                  OF BDRPTRAN
                                    TO FD-DETAIL-TRANS-ERROR OF BDRPSCOR
                                                                        
                 PERFORM  2200-GRAVAR-BDRPSCOR                          
                 PERFORM  1500-LER-BDRPTRAN                             
              ELSE
                 IF SEARCH-NAME EQUAL 'INATIVAR CONTA'
                   MOVE REG-BASE-EXTRACT OF  ARQEXTRC
                                       TO WRK-REG-BASE-EXTRACT
                   MOVE SPACES
                     TO WRK-SOCIAL-SECURITY-NUM  OF WRK-REG-BASE-EXTRACT
                        WRK-CUSTOMER-INFO-NUMBER OF WRK-REG-BASE-EXTRACT
                   MOVE WRK-CPFCNPJ-BASE-AUX
                     TO WRK-SOCIAL-SECURITY-NUM  OF WRK-REG-BASE-EXTRACT
                        WRK-CUSTOMER-INFO-NUMBER OF WRK-REG-BASE-EXTRACT

                   MOVE 'INATIVAR CONTA2'
                                       TO WRK-SEARCH-NAME
                   MOVE 'INATIVAR CONTA2'
                                       TO WRK-CUSTOMER-NAME
                   MOVE 'INATIVAR CONTA2'
                                       TO WRK-CUSTOMER-ADDRESS

                   MOVE ZEROS      TO WRK-TELEPHONE-NUMBER-DATA
                   MOVE ZEROS      TO WRK-TOTAL-DELINQ-AMT
                   MOVE ZEROS      TO WRK-DISPUTED-AMT
                   MOVE ZEROS      TO WRK-INTEREST-DELQ-AMT
                   MOVE ZEROS      TO WRK-OTH-DELQ-AMT
                   MOVE ZEROS      TO WRK-REPOSSESSION-CHARGES
                   MOVE ZEROS      TO WRK-TOTAL-DUE-AMT

                   PERFORM  2100-GRAVAR-ARQSAIDA
                 END-IF

                 ADD  1                   TO  ACU-GRAVA-EXTRSCOR        
                 PERFORM  1400-LER-ARQEXTRC                             
              END-IF                                                    
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       2000-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       2100-GRAVAR-ARQSAIDA            SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           MOVE  WRK-REG-BASE-EXTRACT TO   FD-SAI-REGTO.                
           MOVE  WRK-GRAVACAO          TO  WRK-OPERACAO.                
                                                                        
           WRITE   FD-SAI-REGTO.                                        
           PERFORM 1130-TESTAR-FS-ARQSAIDA.                             
                                                                        
           ADD     1                   TO  ACU-GRAVA-ARQSAIDA.          
                                                                        
      *---------------------------------------------------------------* 
       2100-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
           EJECT                                                        
      *----------------------------------------------------------------*
       2200-GRAVAR-BDRPSCOR            SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE    WRK-GRAVACAO        TO  WRK-OPERACAO.                
                                                                        
           WRITE   FD-DETAIL-TRANS-ERROR                                
                                       OF  BDRPSCOR.                    
           PERFORM 1140-TESTAR-FS-BDRPSCOR.                             
                                                                        
           ADD     1                   TO  ACU-GRAVA-BDRPSCOR.          
                                                                        
      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
           EJECT                                                        
      *---------------------------------------------------------------* 
       3000-FINALIZAR                  SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           PERFORM 3100-MOSTRAR-DISPLAY.                                
                                                                        
           CLOSE   ARQEXTRC                                             
                   BDRPTRAN                                             
                   BDRPSCOR                                             
                   ARQSAIDA.                                            
                                                                        
           MOVE    WRK-FECHAMENTO      TO  WRK-OPERACAO.                
           PERFORM 1100-TESTAR-FILE-STATUS.                             
                                                                        
           STOP    RUN.                                                 
                                                                        
      *---------------------------------------------------------------* 
       3000-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       3100-MOSTRAR-DISPLAY            SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
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
                                                                        
      *---------------------------------------------------------------* 
       3100-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       9999-ROTINA-ERRO                SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           MOVE 'RDAB7480'             TO  ERR-PGM.                     
                                                                        
           CALL 'POOL7100'             USING  WRK-BATCH                 
                                              ERRO-AREA.                
                                                                        
           GOBACK.                                                      
                                                                        
      *---------------------------------------------------------------* 
       9999-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 

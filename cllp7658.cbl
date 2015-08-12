      *================================================================*
       IDENTIFICATION DIVISION.                                         
      *================================================================*
       PROGRAM-ID.CLLP7658.                                             
       AUTHOR. GUILHERME.                                               
      *================================================================*
      *                    I N F O S E R V E R                         *
      *----------------------------------------------------------------*
      *                                                                *
      *      PROGRAMA     : CLLP7658                                   *
      *      PROGRAMADOR  :  GUILHERME ALCANTARA - EGM                 *
      *      ANALISTA     :  GUILHERME ALCANTARA - EGM                 *
      *      DATA         :  20/09/2012                                *
      *                                                                *
      *      OBJETIVO     :                                            *
      *          RETIRA DO MOVIMENTO DE NEGATIVACAO REGISTROS MIGRADOS.*
      *                                                                *
      *      ARQUIVOS     :                                            *
      *        DDNAME                            INCLUDE/BOOK          *
      *          CONTMIGR /ENTRADA                 -------- - 90       *
      *          ARQSAIDA /ENTRADA                 I#CLLPCJ - 285      *
      *          ARQSAIDS /SAIDA                   I#CLLPCJ - 285      *
      *          ARQMIGR  /SAIDA                   I#CLLPCJ - 285      *
      *                                                                *
      *      MODULOS CHAMADOS  :                                       *
      *          POOL7100  -  MODULO DE TRATAMENTO DE ERROS.           *
      *                                                                *
      *================================================================*
                                                                        
      *================================================================*
       ENVIRONMENT DIVISION.                                            
      *================================================================*
                                                                        
      *----------------------------------------------------------------*
       CONFIGURATION SECTION.                                           
      *----------------------------------------------------------------*
                                                                        
       SPECIAL-NAMES.                                                   
           DECIMAL-POINT IS COMMA.                                      
                                                                        
      *----------------------------------------------------------------*
       INPUT-OUTPUT SECTION.                                            
      *----------------------------------------------------------------*
                                                                        
       FILE-CONTROL.                                                    
                                                                        
           SELECT  CONTMIGR  ASSIGN  TO  UT-S-CONTMIGR                  
                       FILE  STATUS  IS  WRK-FS-CONTMIGR.               
                                                                        
           SELECT  ARQSAIDA  ASSIGN  TO  UT-S-ARQSAIDA                  
                       FILE  STATUS  IS  WRK-FS-ARQSAIDA.               
                                                                        
           SELECT  ARQSAIDS ASSIGN   TO  UT-S-ARQSAIDS                  
                       FILE  STATUS  IS  WRK-FS-ARQSAIDS.               
                                                                        
           SELECT  ARQMIGR  ASSIGN   TO  UT-S-ARQMIGR                   
                       FILE  STATUS  IS  WRK-FS-ARQMIGR.                
                                                                        
           SELECT  RELCONS   ASSIGN  TO  UT-S-RELCONS                   
                       FILE  STATUS  IS  WRK-FS-RELCONS.                
                                                                        
           SELECT  RELINCON  ASSIGN  TO  UT-S-RELINCON                  
                       FILE  STATUS  IS  WRK-FS-RELINCON.               
                                                                        
      *================================================================*
       DATA DIVISION.                                                   
      *================================================================*
                                                                        
      *----------------------------------------------------------------*
       FILE SECTION.                                                    
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *    INPUT   :  ARQUIVO DE ENTRADA CONTMIGR                      *
      *               ORG. SEQUENCIAL   -   LRECL  =  90               *
      *----------------------------------------------------------------*
                                                                        
       FD  CONTMIGR                                                     
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  FD-RECR-MIGR.                                                
           05 FD-NCONTR-RCUPC-CVIVE       PIC S9(08) COMP-3.            
           05 FD-CPSSOA-JURID-CONTR       PIC S9(10) COMP-3.            
           05 FD-CPSSOA-JURID-CONTR-NU    PIC  X(01).                   
           05 FD-CTPO-CONTR-NEGOC         PIC S9(03) COMP-3.            
           05 FD-CTPO-CONTR-NEGOC-NULL    PIC  X(01).                   
           05 FD-NSEQ-CONTR-NEGOC         PIC S9(10) COMP-3.            
           05 FD-NSEQ-CONTR-NEGOC-NULL    PIC  X(01).                   
           05 FD-CPSSOA-CONTR-CVIVE       PIC S9(10) COMP-3.            
           05 FD-CPSSOA-CONTR-CVIVE-NU    PIC  X(01).                   
           05 FD-CTPO-CONTR-CVIVE         PIC S9(03) COMP-3.            
           05 FD-CTPO-CONTR-CVIVE-NULL    PIC  X(01).                   
           05 FD-NSEQ-CONTR-CVIVE         PIC S9(10) COMP-3.            
           05 FD-NSEQ-CONTR-CVIVE-NULL    PIC  X(01).                   
           05 FD-CBCO-CONTR-ORIGE         PIC S9(03) COMP-3.            
           05 FD-CAG-CONTR-ORIGE          PIC S9(05) COMP-3.            
           05 FD-CCTA-CONTR-ORIGE         PIC S9(13) COMP-3.            
           05 FD-CCART-CONTR-ORIGE        PIC  X(03).                   
           05 FD-CCONTR-NEGOC-ORIGE       PIC S9(17) COMP-3.            
           05 FD-CSIT-CONTR-CVIVE         PIC S9(01) COMP-3.            
           05 FD-HMANUT-REG               PIC  X(26).                   
           05 FD-CPF                      PIC S9(09) COMP-3.            
           05 FD-FILIAL                   PIC S9(04) COMP-3.            
           05 FD-DIG-CPF                  PIC S9(02) COMP-3.            
                                                                        
      *----------------------------------------------------------------*
      *    INPUT   :  ARQUIVO DE ENTRADA ARQSAIDA                      *
      *               ORG. SEQUENCIAL   -   LRECL  = 285               *
      *----------------------------------------------------------------*
                                                                        
       FD  ARQSAIDA                                                     
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       COPY 'I#CLLP26'.
                                                                        
      *----------------------------------------------------------------*
      *    OUTPUT  :  ARQUIVO DE SAIDA ARQSAIDS                        *
      *               ORG. SEQUENCIAL   -   LRECL  =  285              *
      *----------------------------------------------------------------*
                                                                        
       FD  ARQSAIDS                                                     
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  FD-ARQSAIDS                     PIC X(285).                  
                                                                        
      *----------------------------------------------------------------*
      *    OUTPUT  :  ARQUIVO DE SAIDA ARQMIGR                         *
      *               ORG. SEQUENCIAL   -   LRECL  =  285              *
      *----------------------------------------------------------------*
                                                                        
       FD  ARQMIGR                                                      
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  FD-ARQMIGR                      PIC X(285).                  
                                                                        
      *---------------------------------------------------------------- 
      *                                                                 
      *    OUTPUT  :  RELATORIO DE REGISTROS ACEITOS                   *
      *               ORG. SEQUENCIAL   -   LRECL  =  133              *
      *----------------------------------------------------------------*
                                                                        
       FD  RELCONS                                                      
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  FD-RELATO               PIC X(133).                          
                                                                        
      *----------------------------------------------------------------*
      *    OUTPUT  :  RELATORIO DE REGISTROS CONSISTENTES              *
      *               ORG. SEQUENCIAL   -   LRECL  =  133              *
      *----------------------------------------------------------------*
                                                                        
       FD  RELINCON                                                     
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  FD-RELATO1              PIC X(133).                          
                                                                        
      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.                                         
      *----------------------------------------------------------------*
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       77  FILLER   PIC  X(32) VALUE '*  INICIO DA WORKINGCLLP7658  *'. 
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       77  FILLER   PIC  X(32) VALUE '*  AREAS P/ TESTE FILE STATUS  *'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
       77  WRK-FS-ARQSAIDA             PIC  X(02) VALUE SPACES.         
       77  WRK-FS-CONTMIGR             PIC  X(02) VALUE SPACES.         
       77  WRK-FS-ARQSAIDS             PIC  X(02) VALUE SPACES.         
       77  WRK-FS-ARQMIGR              PIC  X(02) VALUE SPACES.         
       77  WRK-FS-RELINCON             PIC  X(02) VALUE SPACES.         
       77  WRK-FS-RELCONS              PIC  X(02) VALUE SPACES.         
                                                                        
       77  WRK-ABERTURA                PIC  X(13) VALUE 'NA ABERTURA'.  
       77  WRK-LEITURA                 PIC  X(13) VALUE 'NA LEITURA'.   
       77  WRK-GRAVACAO                PIC  X(13) VALUE 'NA GRAVACAO'.  
       77  WRK-FECHAMENTO              PIC  X(13) VALUE 'NO FECHAMENTO'.
       77  WRK-BATCH                   PIC  X(08) VALUE 'BATCH'.        
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       77  FILLER   PIC  X(32) VALUE '*  ACUMULADORES E AUXILIARES   *'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
       77  ACU-LIDOS-ARQSAIDA          PIC  9(06) VALUE ZEROS COMP-3.   
       77  ACU-LINHAS                  PIC  9(10) COMP-3 VALUE 59.      
       77  ACU-LINHAS1                 PIC  9(10) COMP-3 VALUE 59.      
       77  ACU-LIDOS-CONTMIGR          PIC  9(06) VALUE ZEROS COMP-3.   
       77  ACU-GRAVA-ARQSAIDS          PIC  9(06) VALUE ZEROS COMP-3.   
       77  ACU-GRAVA-ARQMIGR           PIC  9(06) VALUE ZEROS COMP-3.   
       77  ACU-RETIRADOS               PIC  9(06) VALUE ZEROS COMP-3.   
       77  ACU-PAGINAS                 PIC  9(09) VALUE ZEROS COMP-3.   
       77  ACU-PAGINAS1                PIC  9(09) VALUE ZEROS COMP-3.   
                                                                        
       77  WRK-L-SEG                   PIC  Z.ZZZ.ZZZ.ZZ9 VALUE ZEROS.  
       77  WRK-L-ENT                   PIC  Z.ZZZ.ZZZ.ZZ9 VALUE ZEROS.  
       77  WRK-GRAVA                   PIC  Z.ZZZ.ZZZ.ZZ9 VALUE ZEROS.  
       77  WRK-GRAVA1                  PIC  Z.ZZZ.ZZZ.ZZ9 VALUE ZEROS.  
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       77  FILLER   PIC  X(32) VALUE '*            F CHVS            *'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
       01  WRK-CONTRATO-AUX            PIC +9(17) VALUE ZEROS.          
       01  WRK-CONTRATO-AUX-R    REDEFINES  WRK-CONTRATO-AUX.           
           03  FILLER                  PIC  X(01).                      
           03  FILLER                  PIC  X(10).                      
           03  WRK-CONTRATO-AUX-RR     PIC  9(07).                      
                                                                        
       01  WRK-CONTRATO-1              PIC +9(07) VALUE ZEROS.          
       01  WRK-CONTRATO-1-R      REDEFINES  WRK-CONTRATO-1.             
           03  FILLER                  PIC  X(01).                      
           03  WRK-CONTRATO-1-RR       PIC  9(07).                      
                                                                        
       01  WRK-AGENCIA-AUX             PIC +9(05) VALUE ZEROS.          
       01  WRK-AGENCIA-AUX-R     REDEFINES  WRK-AGENCIA-AUX.            
           03  FILLER                  PIC  X(01).                      
           03  WRK-AGENCIA-AUX-RR      PIC  9(05).                      
                                                                        
       01  WRK-CONTA-AUX               PIC +9(13) VALUE ZEROS.          
       01  WRK-CONTA-AUX-R       REDEFINES  WRK-CONTA-AUX.              
           03  FILLER                  PIC  X(01).                      
           03  FILLER                  PIC  9(06).                      
           03  WRK-CONTA-AUX-RR        PIC  9(07).                      
                                                                        
       01  WRK-CPSSOA-AUX               PIC +9(10) VALUE ZEROS.         
       01  WRK-CONTA-AUX-R       REDEFINES  WRK-CPSSOA-AUX.             
           03  FILLER                  PIC  X(01).                      
           03  WRK-CPSSOA-AUX-RR       PIC  9(10).                      
                                                                        
       01  WRK-NSEQ-AUX               PIC +9(10) VALUE ZEROS.           
       01  WRK-NSEQ-AUX-R       REDEFINES  WRK-NSEQ-AUX.                
           03  FILLER                  PIC  X(01).                      
           03  WRK-NSEQ-AUX-RR         PIC  9(10).                      
                                                                        
       01  WRK-CTPO-AUX               PIC +9(03) VALUE ZEROS.           
       01  WRK-CTPO-AUX-R       REDEFINES  WRK-CTPO-AUX.                
           03  FILLER                  PIC  X(01).                      
           03  WRK-CTPO-AUX-RR         PIC  9(03).                      
                                                                        
       01  WRK-CHV-CONTMIGR.                                            
           03  WRK-CPF-MIGR            PIC  9(09) VALUE ZEROS.          
           03  WRK-FIL-MIGR            PIC  9(04) VALUE ZEROS.          
           03  WRK-DIG-MIGR            PIC  9(02) VALUE ZEROS.          
                                                                        
       01  WRK-CHV-ARQSAIDA.                                            
           03  WRK-CPF-MOVIMENTO       PIC  9(09) VALUE ZEROS.          
           03  WRK-FIL-MOVIMENTO       PIC  9(04) VALUE ZEROS.          
           03  WRK-DIG-MOVIMENTO       PIC  9(02) VALUE ZEROS.          
                                                                        
       01  WRK-CPF-AUX                 PIC +9(09) VALUE ZEROS.          
       01  FILLER    REDEFINES WRK-CPF-AUX.                             
           03 FILLER                   PIC  X(01).                      
           03 WRK-CPF-AUX-RR           PIC  9(09).                      
                                                                        
       01  WRK-FILIAL-AUX              PIC +9(04) VALUE ZEROS.          
       01  FILLER    REDEFINES WRK-FILIAL-AUX.                          
           03 FILLER                   PIC  X(01).                      
           03 WRK-FILIAL-AUX-RR        PIC  9(04).                      
                                                                        
       01  WRK-FIL-MOV-AUX             PIC 9(05) VALUE ZEROS.           
       01  FILLER    REDEFINES WRK-FIL-MOV-AUX.                         
           03 FILLER                   PIC 9(01).                       
           03 WRK-FIL-MOV-AUX-RR       PIC 9(04).                       
                                                                        
       01  WRK-DIG-AUX                 PIC +9(02) VALUE ZEROS.          
       01  FILLER    REDEFINES WRK-DIG-AUX.                             
           03 FILLER                   PIC  X(01).                      
           03 WRK-DIG-AUX-RR           PIC  9(02).                      
                                                                        
       01  WRK-DIG-MOV-AUX             PIC  9(03) VALUE ZEROS.          
       01  FILLER    REDEFINES WRK-DIG-MOV-AUX.                         
           03 FILLER                   PIC  9(01).                      
           03 WRK-DIG-MOV-AUX-RR       PIC  9(02).                      
                                                                        
       01  WRK-DATA-RELATORIO.                                          
           03 WRK-DIA-REL              PIC 9(02) VALUE ZEROS.           
           03 FILLER                   PIC X(01) VALUE '/'.             
           03 WRK-MES-REL              PIC 9(02) VALUE ZEROS.           
           03 FILLER                   PIC X(01) VALUE '/'.             
           03 WRK-ANO-REL              PIC 9(04) VALUE ZEROS.           
                                                                        
       01  WRK-DATA-HORA.                                               
           03 WRK-DT-JULIANA           PIC 9(05)  COMP-3 VALUE ZEROS.   
           03 WRK-DT-AAMMDD            PIC 9(07)  COMP-3 VALUE ZEROS.   
           03 WRK-DT-AAAAMMDD          PIC 9(09)  COMP-3 VALUE ZEROS.   
           03 WRK-TI-HHMMSS            PIC 9(07)  COMP-3 VALUE ZEROS.   
           03 WRK-TI-HHMMSSMMMMMM      PIC 9(13)  COMP-3 VALUE ZEROS.   
           03 WRK-TIMESTAMP            PIC X(20)         VALUE SPACES.  
                                                                        
       01  WRK-DT-AAAAMMDD-AUX         PIC 9(09) VALUE ZEROS.           
       01  WRK-DT-AAAAMMDD-R REDEFINES WRK-DT-AAAAMMDD-AUX.             
           03 WRK-DESPREZA             PIC  9(001).                     
           03 WRK-SS-AUX               PIC  9(002).                     
           03 WRK-AA-AUX               PIC  9(002).                     
           03 WRK-MM-AUX               PIC  9(002).                     
           03 WRK-DD-AUX               PIC  9(002).                     
                                                                        
       01  WRK-DATA-AUX                PIC 9(09) VALUE ZEROS.           
       01  WRK-DATA          REDEFINES WRK-DATA-AUX.                    
           03 WRK-DESPREZA             PIC  9(001).                     
           03 WRK-ANO-AUX1             PIC  9(004).                     
           03 WRK-MES-AUX1             PIC  9(002).                     
           03 WRK-DIA-AUX1             PIC  9(002).                     
                                                                        
       01  WRK-DT-SIST.                                                 
           03 WRK-SS-SIST              PIC  9(002) VALUE ZEROS.         
           03 WRK-AA-SIST              PIC  9(002) VALUE ZEROS.         
           03 WRK-MM-SIST              PIC  9(002) VALUE ZEROS.         
           03 WRK-DD-SIST              PIC  9(002) VALUE ZEROS.         
       01  WRK-DT-SIST-R REDEFINES     WRK-DT-SIST PIC 9(08).           
                                                                        
       01  WRK-HR-SIST                 PIC  9(008)    VALUE ZEROS.      
       01  WRK-HR-SIST-R REDEFINES     WRK-HR-SIST.                     
           03 WRK-HO-SIST              PIC  9(002).                     
           03 WRK-MI-SIST              PIC  9(002).                     
           03 WRK-SE-SIST              PIC  9(002).                     
           03 WRK-SC-SIST              PIC  9(002).                     
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER   PIC  X(32) VALUE '* AREA MENSAGEM ERRO APLICACAO *'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
       01  WRK-MSG-FS-ERRO.                                             
           03  FILLER                  PIC  X(10) VALUE SPACES.         
           03  FILLER                  PIC  X(05) VALUE 'ERRO '.        
           03  WRK-OPERACAO            PIC  X(13) VALUE SPACES.         
           03  FILLER                  PIC  X(12) VALUE ' DO ARQUIVO '. 
           03  WRK-NOME-ARQ            PIC  X(09) VALUE SPACES.         
           03  FILLER                  PIC  X(14) VALUE 'FILE STATUS ='.
           03  WRK-FILE-STATUS         PIC  X(02) VALUE SPACES.         
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER   PIC  X(32) VALUE '*       AREA DA POOL7100       *'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
       COPY POL7100C.                                                   
                                                                        
       01 WRK-LINBRANCO              PIC X(133)       VALUE SPACES.     
                                                                        
       01  CABEC1.                                                      
           03  FILLER                  PIC X(001)       VALUE '1'.      
           03  FILLER                  PIC X(008)      VALUE 'CLLP7658'.
           03  FILLER                  PIC X(043)       VALUE SPACES.   
           03  FILLER                  PIC X(018)       VALUE           
               'BANCO BRADESCO S/A'.                                    
           03  FILLER                  PIC X(046)       VALUE SPACES.   
           03  FILLER                  PIC X(006)       VALUE 'FOLHA:'. 
           03  LINDET-PAGINA           PIC ZZZ.ZZZ.ZZ9 VALUE ZEROS.     
                                                                        
       01  CABEC5.                                                      
           03  FILLER                  PIC X(001)       VALUE '1'.      
           03  FILLER                  PIC X(008)      VALUE 'CLLP7658'.
           03  FILLER                  PIC X(043)       VALUE SPACES.   
           03  FILLER                  PIC X(018)       VALUE           
               'BANCO BRADESCO S/A'.                                    
           03  FILLER                  PIC X(046)       VALUE SPACES.   
           03  FILLER                  PIC X(006)       VALUE 'FOLHA:'. 
           03  LINDET-PAGINA1          PIC ZZZ.ZZZ.ZZ9 VALUE ZEROS.     
                                                                        
       01  WRK-LINTESTE                PIC X(133)       VALUE ALL '-'.  
                                                                        
       01  CABEC2.                                                      
           03  FILLER                  PIC  X(001)      VALUE '0'.      
           03  CB2-DT-SIST.                                             
               05  CB2-DD-SIST         PIC  9(002)      VALUE ZEROS.    
               05  FILLER              PIC  X(001)      VALUE '/'.      
               05  CB2-MM-SIST         PIC  9(002)      VALUE ZEROS.    
               05  FILLER              PIC  X(001)      VALUE '/'.      
               05  CB2-SC-SIST         PIC  9(002)      VALUE ZEROS.    
               05  CB2-AA-SIST         PIC  9(002)      VALUE ZEROS.    
           03  FILLER                  PIC  X(023)      VALUE SPACES.   
           03  FILLER                  PIC  X(058)      VALUE           
           'CONFRONTO CONTRATOS MIGRADOS X CONTRATOS COM ARQSAIDA'.     
           03  FILLER                  PIC  X(033)      VALUE SPACES.   
           03  CB2-HR-SIST.                                             
               05  CB2-HO-SIST         PIC  9(002)      VALUE ZEROS.    
               05  FILLER              PIC  X(001)      VALUE ':'.      
               05  CB2-MI-SIST         PIC  9(002)      VALUE ZEROS.    
               05  FILLER              PIC  X(001)      VALUE ':'.      
               05  CB2-SE-SIST         PIC  9(002)      VALUE ZEROS.    
                                                                        
       01  CABEC3.                                                      
           03  FILLER                  PIC  X(001)      VALUE '0'.      
           03  FILLER                  PIC  X(035)      VALUE SPACES.   
           03  FILLER                  PIC  X(051)      VALUE           
               'REGISTROS NAO MIGRADOS - CONTINUAM NO PROCESSAMENTO'.   
                                                                        
       01  CABEC4.                                                      
           03  FILLER                  PIC  X(001)      VALUE '0'.      
           03  FILLER                  PIC  X(039)      VALUE SPACES.   
           03  FILLER                  PIC  X(047)      VALUE           
               'REGISTROS MIGRADOS - RETIRADOS DO PROCESSAMENTO'.       
                                                                        
       01  LINDET-TOT-LIDOS.                                            
           03  FILLER                  PIC  X(041)      VALUE SPACES.   
           03  FILLER                  PIC  X(035)      VALUE           
               'TOTAL DE LIDOS MIGRADOS           :'.                   
           03  LDT-TOT-LIDOS-CONTR-M   PIC  ZZZ.ZZ9     VALUE ZEROS.    
                                                                        
       01  LINDET-TOT-LIDOS-ARQSAIDA.                                   
           03  FILLER                  PIC  X(041)      VALUE SPACES.   
           03  FILLER                  PIC  X(035)      VALUE           
               'TOTAL DE LIDOS ARQSAIDA           :'.                   
           03  LDT-TOT-LIDOS-ARQSAIDA  PIC  ZZZ.ZZ9     VALUE ZEROS.    
                                                                        
       01  LINDET-TOT-GRAVADOS.                                         
           03  FILLER                  PIC  X(041)      VALUE SPACES.   
           03  FILLER                  PIC  X(035)      VALUE           
               'TOTAL DE GRAVADOS P/ PROCESSAMENTO:'.                   
           03  LDT-TOT-GRAVADOS        PIC  ZZZ.ZZ9     VALUE ZEROS.    
                                                                        
       01  LINDET-TOT-RETIRADOS.                                        
           03  FILLER                  PIC  X(041)      VALUE SPACES.   
           03  FILLER                  PIC  X(035)      VALUE           
               'TOTAL RETIRADOS DO PROCESSAMENTO  :'.                   
           03  LDT-TOT-RETIRADOS       PIC  ZZZ.ZZ9     VALUE ZEROS.    
                                                                        
       01  WRK-LINCAB1.                                                 
           03  FILLER                  PIC  X(002)       VALUE SPACES.  
           03  FILLER                  PIC  X(007)       VALUE          
               'AGENCIA'.                                               
           03  FILLER                  PIC  X(008)       VALUE SPACES.  
           03  FILLER                  PIC  X(005)       VALUE          
               'CONTA'.                                                 
           03  FILLER                  PIC  X(006)       VALUE SPACES.  
           03  FILLER                  PIC  X(008)       VALUE          
               'CARTEIRA'.                                              
           03  FILLER                  PIC  X(006)       VALUE SPACES.  
           03  FILLER                  PIC  X(008)       VALUE          
               'CONTRATO'.                                              
           03  FILLER                  PIC  X(007)       VALUE SPACES.  
           03  FILLER                  PIC  X(010)       VALUE          
               'VENCIMENTO'.                                            
           03  FILLER                  PIC  X(006)       VALUE SPACES.  
           03  FILLER                  PIC  X(012)       VALUE          
               'CPSSOA CONTR'.                                          
           03  FILLER                  PIC  X(011)       VALUE SPACES.  
           03  FILLER                  PIC  X(010)       VALUE          
               'CTPO CONTR'.                                            
           03  FILLER                  PIC  X(016)       VALUE SPACES.  
           03  FILLER                  PIC  X(010)       VALUE          
               'NSEQ CONTR'.                                            
                                                                        
       01  WRK-LINCAB2.                                                 
           03  FILLER                  PIC  X(002)       VALUE SPACES.  
           03  FILLER                  PIC  X(007)       VALUE          
               'AGENCIA'.                                               
           03  FILLER                  PIC  X(008)       VALUE SPACES.  
           03  FILLER                  PIC  X(005)       VALUE          
               'CONTA'.                                                 
           03  FILLER                  PIC  X(006)       VALUE SPACES.  
           03  FILLER                  PIC  X(008)       VALUE          
               'CARTEIRA'.                                              
           03  FILLER                  PIC  X(006)       VALUE SPACES.  
           03  FILLER                  PIC  X(008)       VALUE          
               'CONTRATO'.                                              
           03  FILLER                  PIC  X(007)       VALUE SPACES.  
           03  FILLER                  PIC  X(010)       VALUE          
               'VENCIMENTO'.                                            
           03  FILLER                  PIC  X(006)       VALUE SPACES.  
           03  FILLER                  PIC  X(012)       VALUE          
               'CPSSOA CONTR'.                                          
           03  FILLER                  PIC  X(011)       VALUE SPACES.  
           03  FILLER                  PIC  X(010)       VALUE          
               'CTPO CONTR'.                                            
           03  FILLER                  PIC  X(016)       VALUE SPACES.  
           03  FILLER                  PIC  X(010)       VALUE          
               'NSEQ CONTR'.                                            
                                                                        
       01  LINDET1.                                                     
           03  FILLER                  PIC  X(004)       VALUE SPACES.  
           03  LDT-AGENCIA             PIC  ZZZZ9        VALUE ZEROS.   
           03  FILLER                  PIC  X(006)       VALUE SPACES.  
           03  LDT-CONTA               PIC  ZZZZZZ9      VALUE ZEROS.   
           03  FILLER                  PIC  X(009)       VALUE SPACES.  
           03  LDT-CARTEIRA            PIC  X(003)       VALUE SPACES.  
           03  FILLER                  PIC  X(009)       VALUE SPACES.  
           03  LDT-CONTRATO            PIC  ZZZZZZ9      VALUE ZEROS.   
           03  FILLER                  PIC  X(007)       VALUE SPACES.  
           03  LDT-VENCIMENTO          PIC  X(010)       VALUE SPACES.  
           03  FILLER                  PIC  X(006)       VALUE SPACES.  
           03  LDT-CPSSOA-CONTR-CVIVE  PIC  ZZZZZZZZZ9   VALUE ZEROS.   
           03  FILLER                  PIC  X(015)       VALUE SPACES.  
           03  LDT-CTPO-CONTR-CVIVE    PIC  ZZ9          VALUE ZEROS.   
           03  FILLER                  PIC  X(019)       VALUE SPACES.  
           03  LDT-NSEQ-CONTR-CVIVE    PIC  ZZZZZZZZZ9   VALUE ZEROS.   
                                                                        
       01  LINDET2.                                                     
           03  FILLER                  PIC  X(004)       VALUE SPACES.  
           03  LDT-AGENCIA1            PIC  ZZZZ9        VALUE ZEROS.   
           03  FILLER                  PIC  X(006)       VALUE SPACES.  
           03  LDT-CONTA1              PIC  ZZZZZZ9      VALUE ZEROS.   
           03  FILLER                  PIC  X(009)       VALUE SPACES.  
           03  LDT-CARTEIRA1           PIC  X(003)       VALUE SPACES.  
           03  FILLER                  PIC  X(009)       VALUE SPACES.  
           03  LDT-CONTRATO1           PIC  ZZZZZZ9      VALUE ZEROS.   
           03  FILLER                  PIC  X(007)       VALUE SPACES.  
           03  LDT-VENCIMENTO1         PIC  X(010)       VALUE SPACES.  
           03  FILLER                  PIC  X(006)       VALUE SPACES.  
           03  LDT-CPSSOA-CONTR-CVIVE1 PIC  ZZZZZZZZZ9   VALUE ZEROS.   
           03  FILLER                  PIC  X(015)       VALUE SPACES.  
           03  LDT-CTPO-CONTR-CVIVE1   PIC  ZZ9          VALUE ZEROS.   
           03  FILLER                  PIC  X(019)       VALUE SPACES.  
           03  LDT-NSEQ-CONTR-CVIVE1   PIC  ZZZZZZZZZ9   VALUE ZEROS.   
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER   PIC  X(32) VALUE '*   FIM DA WORKINGCLLP7658    *'. 
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
      *================================================================*
       PROCEDURE DIVISION.                                              
      *================================================================*
                                                                        
      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL SECTION.                                   
      *----------------------------------------------------------------*
                                                                        
           PERFORM 1000-INICIALIZAR.                                    
                                                                        
           PERFORM 2000-PROCESSAR                                       
                   UNTIL WRK-CHV-CONTMIGR EQUAL HIGH-VALUES             
                     AND WRK-CHV-ARQSAIDA EQUAL HIGH-VALUES.            
                                                                        
           PERFORM 3000-FINALIZAR.                                      
                                                                        
      *----------------------------------------------------------------*
       0000-99-FIM. EXIT.                                               
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       1000-INICIALIZAR SECTION.                                        
      *----------------------------------------------------------------*
                                                                        
           OPEN INPUT  CONTMIGR                                         
                       ARQSAIDA                                         
                OUTPUT ARQSAIDS                                         
                       ARQMIGR                                          
                       RELCONS                                          
                       RELINCON.                                        
                                                                        
           PERFORM 1100-TESTAR-FILE-STATUS.                             
                                                                        
           CALL  'POOL7600'         USING  WRK-DATA-HORA.               
                                                                        
           MOVE WRK-DT-AAAAMMDD        TO WRK-DT-AAAAMMDD-AUX.          
                                                                        
           MOVE WRK-SS-AUX             TO WRK-SS-SIST.                  
           MOVE WRK-AA-AUX             TO WRK-AA-SIST.                  
           MOVE WRK-MM-AUX             TO WRK-MM-SIST.                  
           MOVE WRK-DD-AUX             TO WRK-DD-SIST.                  
                                                                        
           MOVE    WRK-DD-SIST         TO CB2-DD-SIST.                  
           MOVE    WRK-MM-SIST         TO CB2-MM-SIST.                  
           MOVE    WRK-SS-SIST         TO CB2-SC-SIST.                  
           MOVE    WRK-AA-SIST         TO CB2-AA-SIST.                  
                                                                        
           ACCEPT  WRK-HR-SIST         FROM TIME.                       
           MOVE    WRK-HO-SIST         TO CB2-HO-SIST.                  
           MOVE    WRK-MI-SIST         TO CB2-MI-SIST.                  
           MOVE    WRK-SE-SIST         TO CB2-SE-SIST.                  
                                                                        
           MOVE    WRK-ABERTURA        TO WRK-OPERACAO.                 
                                                                        
           PERFORM 1200-LER-CONTMIGR.                                   
                                                                        
           IF (ACU-LIDOS-CONTMIGR EQUAL ZEROS)                          
               DISPLAY '*********CLLP7658 *********'                    
               DISPLAY '*                          *'                   
               DISPLAY '*  ARQUIVO CONTMIGR VAZIO   *'                  
               DISPLAY '*                          *'                   
               DISPLAY '*********CLLP7658 *********'                    
           END-IF.                                                      
                                                                        
           PERFORM 1400-LER-ARQSAIDA.                                   
                                                                        
           IF (ACU-LIDOS-ARQSAIDA EQUAL ZEROS)                          
               DISPLAY '*********CLLP7658 *********'                    
               DISPLAY '*                          *'                   
               DISPLAY '*  ARQUIVO ARQSAIDA VAZIO   *'                  
               DISPLAY '*                          *'                   
               DISPLAY '*********CLLP7658 *********'                    
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       1000-99-FIM. EXIT.                                               
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS SECTION.                                 
      *----------------------------------------------------------------*
                                                                        
           PERFORM 1110-TESTAR-FS-CONTMIGR.                             
                                                                        
           PERFORM 1130-TESTAR-FS-ARQSAIDS.                             
                                                                        
           PERFORM 1131-TESTAR-FS-ARQMIGR.                              
                                                                        
           PERFORM 1140-TESTAR-FS-ARQSAIDA.                             
                                                                        
           PERFORM 1150-TESTAR-FS-RELCONS.                              
                                                                        
           PERFORM 1160-TESTAR-FS-RELINCON.                             
                                                                        
      *----------------------------------------------------------------*
       1100-99-FIM. EXIT.                                               
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       1110-TESTAR-FS-CONTMIGR SECTION.                                 
      *----------------------------------------------------------------*
                                                                        
           IF (WRK-FS-CONTMIGR NOT EQUAL '00')                          
               MOVE   'CONTMIGR'       TO  WRK-NOME-ARQ                 
               MOVE    WRK-FS-CONTMIGR TO  WRK-FILE-STATUS              
               MOVE   'APL'            TO  ERR-TIPO-ACESSO              
               MOVE    WRK-MSG-FS-ERRO TO  ERR-TEXTO                    
               PERFORM 9999-ROTINA-ERRO                                 
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       1110-99-FIM. EXIT.                                               
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       1130-TESTAR-FS-ARQSAIDS SECTION.                                 
      *----------------------------------------------------------------*
                                                                        
           IF (WRK-FS-ARQSAIDS NOT EQUAL '00')                          
               MOVE   'ARQSAIDS'       TO  WRK-NOME-ARQ                 
               MOVE    WRK-FS-ARQSAIDS TO WRK-FILE-STATUS               
               MOVE   'APL'            TO  ERR-TIPO-ACESSO              
               MOVE    WRK-MSG-FS-ERRO TO  ERR-TEXTO                    
               PERFORM 9999-ROTINA-ERRO                                 
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       1130-99-FIM. EXIT.                                               
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       1131-TESTAR-FS-ARQMIGR  SECTION.                                 
      *----------------------------------------------------------------*
                                                                        
           IF (WRK-FS-ARQMIGR  NOT EQUAL '00')                          
               MOVE   'ARQMIGR '       TO  WRK-NOME-ARQ                 
               MOVE    WRK-FS-ARQMIGR  TO WRK-FILE-STATUS               
               MOVE   'APL'            TO  ERR-TIPO-ACESSO              
               MOVE    WRK-MSG-FS-ERRO TO  ERR-TEXTO                    
               PERFORM 9999-ROTINA-ERRO                                 
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       1131-99-FIM. EXIT.                                               
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       1140-TESTAR-FS-ARQSAIDA SECTION.                                 
      *----------------------------------------------------------------*
                                                                        
           IF (WRK-FS-ARQSAIDA NOT EQUAL '00')                          
               MOVE   'ARQSAIDA'       TO  WRK-NOME-ARQ                 
               MOVE    WRK-FS-ARQSAIDA TO  WRK-FILE-STATUS              
               MOVE   'APL'            TO  ERR-TIPO-ACESSO              
               MOVE    WRK-MSG-FS-ERRO TO  ERR-TEXTO                    
               PERFORM 9999-ROTINA-ERRO                                 
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       1140-99-FIM. EXIT.                                               
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       1150-TESTAR-FS-RELCONS  SECTION.                                 
      *----------------------------------------------------------------*
                                                                        
           IF (WRK-FS-RELCONS  NOT EQUAL '00')                          
               MOVE   'RELCONS'        TO  WRK-NOME-ARQ                 
               MOVE    WRK-FS-RELCONS  TO  WRK-FILE-STATUS              
               MOVE   'APL'            TO  ERR-TIPO-ACESSO              
               MOVE    WRK-MSG-FS-ERRO TO  ERR-TEXTO                    
               PERFORM 9999-ROTINA-ERRO                                 
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       1150-99-FIM. EXIT.                                               
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       1160-TESTAR-FS-RELINCON SECTION.                                 
      *----------------------------------------------------------------*
                                                                        
           IF (WRK-FS-RELINCON NOT EQUAL '00')                          
               MOVE   'RELINCON'       TO  WRK-NOME-ARQ                 
               MOVE    WRK-FS-RELINCON TO  WRK-FILE-STATUS              
               MOVE   'APL'            TO  ERR-TIPO-ACESSO              
               MOVE    WRK-MSG-FS-ERRO TO  ERR-TEXTO                    
               PERFORM 9999-ROTINA-ERRO                                 
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       1160-99-FIM. EXIT.                                               
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       1200-LER-CONTMIGR SECTION.                                       
      *----------------------------------------------------------------*
                                                                        
           READ    CONTMIGR.                                            
                                                                        
           IF (WRK-FS-CONTMIGR EQUAL '10')                              
               MOVE    HIGH-VALUES     TO  WRK-CHV-CONTMIGR             
               GO                      TO  1200-99-FIM                  
           END-IF.                                                      
                                                                        
           MOVE    WRK-LEITURA         TO  WRK-OPERACAO.                
                                                                        
           PERFORM 1110-TESTAR-FS-CONTMIGR.                             
                                                                        
           ADD     1                   TO  ACU-LIDOS-CONTMIGR.          
                                                                        
           MOVE    FD-CPF                TO  WRK-CPF-AUX.               
           MOVE    WRK-CPF-AUX-RR        TO  WRK-CPF-MIGR.              
           MOVE    FD-FILIAL             TO  WRK-FILIAL-AUX.            
           MOVE    WRK-FILIAL-AUX-RR     TO  WRK-FIL-MIGR.              
           MOVE    FD-DIG-CPF            TO  WRK-DIG-AUX.               
           MOVE    WRK-DIG-AUX-RR        TO  WRK-DIG-MIGR.              
                                                                        
      *----------------------------------------------------------------*
       1200-99-FIM. EXIT.                                               
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       1310-GRAVAR-ARQSAIDS SECTION.                                    
      *----------------------------------------------------------------*
                                                                        
           WRITE FD-ARQSAIDS FROM REG-WOR.                              
                                                                        
           MOVE WRK-GRAVACAO TO WRK-OPERACAO.                           
                                                                        
           PERFORM 1130-TESTAR-FS-ARQSAIDS.                             
                                                                        
           ADD 01                      TO ACU-GRAVA-ARQSAIDS.           
                                                                        
      *----------------------------------------------------------------*
       1310-99-FIM. EXIT.                                               
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       1311-GRAVAR-ARQMIGR  SECTION.                                    
      *----------------------------------------------------------------*
                                                                        
           WRITE FD-ARQMIGR  FROM REG-WOR.                              
                                                                        
           MOVE WRK-GRAVACAO TO WRK-OPERACAO.                           
                                                                        
           PERFORM 1131-TESTAR-FS-ARQMIGR.                              
                                                                        
           ADD 01                      TO ACU-GRAVA-ARQMIGR.            
                                                                        
      *----------------------------------------------------------------*
       1311-99-FIM. EXIT.                                               
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       1400-LER-ARQSAIDA SECTION.                                       
      *----------------------------------------------------------------*
                                                                        
           READ    ARQSAIDA.                                            
                                                                        
           IF (WRK-FS-ARQSAIDA EQUAL '10')                              
               MOVE    HIGH-VALUES     TO  WRK-CHV-ARQSAIDA             
               GO                      TO  1400-99-FIM                  
           END-IF.                                                      
                                                                        
           MOVE    WRK-LEITURA         TO  WRK-OPERACAO.                
                                                                        
           PERFORM 1140-TESTAR-FS-ARQSAIDA.                             
                                                                        
           ADD     1                   TO  ACU-LIDOS-ARQSAIDA.          
                                                                        
           MOVE  CGCCPF-NUM-WOR      TO WRK-CPF-MOVIMENTO.              
           MOVE  CGCCPF-FIL-WOR      TO WRK-FIL-MOV-AUX.                
           MOVE  WRK-FIL-MOV-AUX-RR  TO WRK-FIL-MOVIMENTO.              
           MOVE  CGCCPF-CTR-WOR      TO WRK-DIG-MOV-AUX.                
           MOVE  WRK-DIG-MOV-AUX-RR  TO WRK-DIG-MOVIMENTO.              
                                                                        
      *----------------------------------------------------------------*
       1400-99-FIM. EXIT.                                               
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       1410-IMPRIMIR-MIGRADOS       SECTION.                            
      *----------------------------------------------------------------*
                                                                        
           IF   ACU-LINHAS  GREATER 58                                  
                WRITE FD-RELATO FROM WRK-LINBRANCO                      
                PERFORM 1430-MONTA-CABECALHO                            
                MOVE ZEROS      TO ACU-LINHAS                           
           END-IF.                                                      
                                                                        
           MOVE AGENCIA-WOR           TO WRK-AGENCIA-AUX.               
           MOVE WRK-AGENCIA-AUX-RR    TO LDT-AGENCIA.                   
           MOVE CONTA-WOR             TO WRK-CONTA-AUX.                 
           MOVE WRK-CONTA-AUX-RR      TO LDT-CONTA.                     
           MOVE CARTEIRA-WOR          TO LDT-CARTEIRA.
           MOVE CONTRATO-WOR          TO WRK-CONTRATO-AUX.              
           MOVE WRK-CONTRATO-AUX-RR   TO LDT-CONTRATO.                  
           MOVE DAT-VENCTO-WOR        TO WRK-DATA-AUX                   
           MOVE WRK-ANO-AUX1          TO WRK-ANO-REL                    
           MOVE WRK-MES-AUX1          TO WRK-MES-REL                    
           MOVE WRK-DIA-AUX1          TO WRK-DIA-REL                    
           MOVE WRK-DATA-RELATORIO    TO LDT-VENCIMENTO.                
                                                                        
           IF   FD-CPSSOA-CONTR-CVIVE-NU EQUAL '?'                      
                MOVE ZEROS                 TO LDT-CPSSOA-CONTR-CVIVE    
           ELSE                                                         
                MOVE FD-CPSSOA-CONTR-CVIVE TO WRK-CPSSOA-AUX            
                MOVE WRK-CPSSOA-AUX-RR     TO LDT-CPSSOA-CONTR-CVIVE    
           END-IF.                                                      
                                                                        
           IF FD-CTPO-CONTR-CVIVE-NULL EQUAL '?'                        
              MOVE ZEROS                 TO LDT-CTPO-CONTR-CVIVE        
           ELSE                                                         
              MOVE FD-CTPO-CONTR-CVIVE   TO WRK-CTPO-AUX                
              MOVE WRK-CTPO-AUX-RR       TO LDT-CTPO-CONTR-CVIVE        
           END-IF.                                                      
                                                                        
           IF FD-NSEQ-CONTR-CVIVE-NULL EQUAL '?'                        
              MOVE ZEROS                 TO LDT-NSEQ-CONTR-CVIVE        
           ELSE                                                         
              MOVE FD-NSEQ-CONTR-CVIVE   TO WRK-NSEQ-AUX                
              MOVE WRK-NSEQ-AUX-RR       TO LDT-NSEQ-CONTR-CVIVE        
           END-IF.                                                      
                                                                        
           WRITE FD-RELATO FROM LINDET1.                                
                                                                        
           ADD   1                 TO ACU-LINHAS.                       
                                                                        
      *----------------------------------------------------------------*
       1410-99-FIM. EXIT.                                               
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       1420-IMPRIMIR-NAO-MIGRADOS        SECTION.                       
      *----------------------------------------------------------------*
                                                                        
           IF   ACU-LINHAS1  GREATER 58                                 
                WRITE FD-RELATO1 FROM WRK-LINBRANCO                     
                PERFORM 1440-MONTA-CABECALHO1                           
           END-IF.                                                      
                                                                        
           MOVE AGENCIA-WOR           TO WRK-AGENCIA-AUX.               
           MOVE WRK-AGENCIA-AUX-RR    TO LDT-AGENCIA1.                  
           MOVE CONTA-WOR             TO WRK-CONTA-AUX.                 
           MOVE WRK-CONTA-AUX-RR      TO LDT-CONTA1.                    
           MOVE CARTEIRA-WOR          TO LDT-CARTEIRA1.
           MOVE CONTRATO-WOR          TO WRK-CONTRATO-AUX.              
           MOVE WRK-CONTRATO-AUX-RR   TO LDT-CONTRATO1.                 
           MOVE DAT-VENCTO-WOR        TO WRK-DATA-AUX                   
           MOVE WRK-ANO-AUX1          TO WRK-ANO-REL                    
           MOVE WRK-MES-AUX1          TO WRK-MES-REL                    
           MOVE WRK-DIA-AUX1          TO WRK-DIA-REL                    
           MOVE WRK-DATA-RELATORIO    TO LDT-VENCIMENTO1.               
                                                                        
           MOVE ZEROS                 TO LDT-CPSSOA-CONTR-CVIVE1        
                                         LDT-CTPO-CONTR-CVIVE1          
                                         LDT-NSEQ-CONTR-CVIVE1.         
                                                                        
           WRITE FD-RELATO1 FROM LINDET2.                               
                                                                        
           ADD   1                 TO ACU-LINHAS1.                      
                                                                        
      *----------------------------------------------------------------*
       1420-99-FIM. EXIT.                                               
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       1430-MONTA-CABECALHO     SECTION.                                
      *----------------------------------------------------------------*
                                                                        
           ADD   1                 TO   ACU-PAGINAS.                    
           MOVE  ACU-PAGINAS       TO   LINDET-PAGINA.                  
           MOVE  5                 TO   ACU-LINHAS.                     
           WRITE FD-RELATO         FROM CABEC1.                         
           WRITE FD-RELATO         FROM CABEC2.                         
           WRITE FD-RELATO         FROM CABEC4.                         
           WRITE FD-RELATO         FROM WRK-LINBRANCO.                  
           WRITE FD-RELATO         FROM WRK-LINTESTE.                   
           WRITE FD-RELATO         FROM WRK-LINCAB1.                    
           WRITE FD-RELATO         FROM WRK-LINTESTE.                   
                                                                        
      *----------------------------------------------------------------*
       1430-99-FIM. EXIT.                                               
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       1440-MONTA-CABECALHO1    SECTION.                                
      *----------------------------------------------------------------*
                                                                        
           ADD   1                 TO   ACU-PAGINAS1.                   
           MOVE  ACU-PAGINAS1      TO   LINDET-PAGINA1.                 
           MOVE  5                 TO   ACU-LINHAS1.                    
           WRITE FD-RELATO1        FROM CABEC5.                         
           WRITE FD-RELATO1        FROM CABEC2.                         
           WRITE FD-RELATO1        FROM CABEC3.                         
           WRITE FD-RELATO1        FROM WRK-LINBRANCO.                  
           WRITE FD-RELATO1        FROM WRK-LINTESTE.                   
           WRITE FD-RELATO1        FROM WRK-LINCAB2.                    
           WRITE FD-RELATO1        FROM WRK-LINTESTE.                   
                                                                        
      *----------------------------------------------------------------*
       1440-99-FIM. EXIT.                                               
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       2000-PROCESSAR SECTION.                                          
      *----------------------------------------------------------------*
                                                                        
           IF (WRK-CHV-ARQSAIDA EQUAL WRK-CHV-CONTMIGR)                 
               PERFORM 1410-IMPRIMIR-MIGRADOS                           
               PERFORM 1311-GRAVAR-ARQMIGR                              
               PERFORM 1400-LER-ARQSAIDA                                
           ELSE                                                         
               IF (WRK-CHV-ARQSAIDA LESS WRK-CHV-CONTMIGR)              
                   PERFORM 1420-IMPRIMIR-NAO-MIGRADOS                   
                   PERFORM 1310-GRAVAR-ARQSAIDS                         
                   PERFORM 1400-LER-ARQSAIDA                            
               ELSE                                                     
                   PERFORM 1200-LER-CONTMIGR                            
               END-IF                                                   
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       2000-99-FIM. EXIT.                                               
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       3000-FINALIZAR SECTION.                                          
      *----------------------------------------------------------------*
                                                                        
           PERFORM 3100-MOSTRAR-DISPLAY.                                
                                                                        
           CLOSE   CONTMIGR                                             
                   ARQSAIDA                                             
                   ARQSAIDS                                             
                   ARQMIGR                                              
                   RELCONS                                              
                   RELINCON.                                            
                                                                        
           MOVE    WRK-FECHAMENTO      TO  WRK-OPERACAO.                
                                                                        
           PERFORM 1100-TESTAR-FILE-STATUS.                             
                                                                        
           STOP    RUN.                                                 
                                                                        
      *----------------------------------------------------------------*
       3000-99-FIM. EXIT.                                               
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       3100-MOSTRAR-DISPLAY SECTION.                                    
      *----------------------------------------------------------------*
                                                                        
           MOVE    ACU-LIDOS-CONTMIGR  TO  WRK-L-SEG                    
                                           LDT-TOT-LIDOS-CONTR-M.       
           MOVE    ACU-LIDOS-ARQSAIDA  TO  WRK-L-ENT                    
                                           LDT-TOT-LIDOS-ARQSAIDA.      
           MOVE    ACU-GRAVA-ARQSAIDS  TO  WRK-GRAVA                    
                                           LDT-TOT-GRAVADOS.            
           MOVE    ACU-GRAVA-ARQMIGR   TO  WRK-GRAVA1                   
                                           LDT-TOT-RETIRADOS.           
                                                                        
           IF ACU-LIDOS-CONTMIGR NOT EQUAL ZEROS                        
              WRITE FD-RELATO1 FROM WRK-LINBRANCO                       
              WRITE FD-RELATO1 FROM LINDET-TOT-LIDOS                    
              WRITE FD-RELATO1 FROM LINDET-TOT-LIDOS-ARQSAIDA           
              WRITE FD-RELATO1 FROM LINDET-TOT-GRAVADOS                 
              WRITE FD-RELATO1 FROM LINDET-TOT-RETIRADOS                
           END-IF.                                                      
                                                                        
           IF ACU-LIDOS-ARQSAIDA NOT EQUAL ZEROS                        
              WRITE FD-RELATO  FROM WRK-LINBRANCO                       
              WRITE FD-RELATO  FROM LINDET-TOT-LIDOS                    
              WRITE FD-RELATO  FROM LINDET-TOT-LIDOS-ARQSAIDA           
              WRITE FD-RELATO  FROM LINDET-TOT-GRAVADOS                 
              WRITE FD-RELATO  FROM LINDET-TOT-RETIRADOS                
           END-IF.                                                      
                                                                        
           DISPLAY '*****************CLLP7658 ******************'.      
           DISPLAY '* TOTAL DE LIDOS ARQSAIDA   :' WRK-L-ENT ' *'.      
           DISPLAY '* TOTAL DE LIDOS CONTMIGR   :' WRK-L-SEG ' *'.      
           DISPLAY '* GRAVADOS  P/ PROCESSAMENTO:' WRK-GRAVA ' *'.      
           DISPLAY '* RETIRADOS DO PROCESSAMENTO:' WRK-GRAVA1' *'.      
           DISPLAY '*****************CLLP7658 ******************'.      
                                                                        
      *----------------------------------------------------------------*
       3100-99-FIM. EXIT.                                               
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       9999-ROTINA-ERRO SECTION.                                        
      *----------------------------------------------------------------*
                                                                        
           MOVE   'CLLP7658'           TO  ERR-PGM.                     
                                                                        
           CALL   'POOL7100'        USING  WRK-BATCH                    
                                           ERRO-AREA.                   
                                                                        
           GOBACK.                                                      
                                                                        
      *----------------------------------------------------------------*
       9999-99-FIM. EXIT.                                               
      *----------------------------------------------------------------*

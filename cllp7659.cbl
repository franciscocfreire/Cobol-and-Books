      *================================================================*
       IDENTIFICATION DIVISION.                                         
      *================================================================*
       PROGRAM-ID. CLLP7659.                                            
       AUTHOR. GUILHERME.                                               
      *================================================================*
      *                    I N F O S E R V E R                         *
      *----------------------------------------------------------------*
      *                                                                *
      *      PROGRAMA     :  CLLP7659                                  *
      *      PROGRAMADOR  :  GUILHERME ALCANTARA - INFOSERVER          *
      *      ANALISTA     :  GUILHERME ALCANTARA - INFOSERVER          *
      *      DATA         :  03/09/2013                                *
      *                                                                *
      *      OBJETIVO     :                                            *
      *          RETIRAR REGISTROS MIGRADOS DO MOVIMENTO DE AVISOS.    *
      *                                                                *
      *      ARQUIVOS     :                                            *
      *        DDNAME                            INCLUDE/BOOK          *
      *          CONTMIGR                          -------- - 56       *
      *          ARQAVISO                          -------- - 690      *
      *          AVINMIGR                          -------- - 690      *
      *          AVIMIGR                           -------- - 690      *
      *          RELCONS                           -------- - 133      *
      *          RELINCON                          -------- - 133      *
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
                                                                        
           SELECT  ARQAVISO  ASSIGN  TO  UT-S-ARQAVISO                  
                       FILE  STATUS  IS  WRK-FS-ARQAVISO.               
                                                                        
           SELECT  AVINMIGR  ASSIGN  TO  UT-S-AVINMIGR                  
                       FILE  STATUS  IS  WRK-FS-AVINMIGR.               
                                                                        
           SELECT  AVIMIGR  ASSIGN  TO  UT-S-AVIMIGR                    
                       FILE  STATUS  IS  WRK-FS-AVIMIGR.                
                                                                        
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
      *    INPUT   :  ARQUIVO DE ENTRADA ARQAVISO                      *
      *               ORG. SEQUENCIAL   -   LRECL  =  590              *
      *----------------------------------------------------------------*
                                                                        
       FD  ARQAVISO                                                     
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       COPY 'I#CLLPFD'.
                                                                        
      *----------------------------------------------------------------*
      *    OUTPUT  :  ARQUIVO DE SAIDA AVINMIGR                        *
      *               ORG. SEQUENCIAL   -   LRECL  =  690              *
      *----------------------------------------------------------------*
                                                                        
       FD  AVINMIGR                                                     
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  FD-AVINMIGR                     PIC X(690).
                                                                        
      *----------------------------------------------------------------*
      *    OUTPUT  :  ARQUIVO DE SAIDA AVIMIGR                         *
      *               ORG. SEQUENCIAL   -   LRECL  =  690              *
      *----------------------------------------------------------------*
                                                                        
       FD  AVIMIGR                                                      
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  FD-AVIMIGR                      PIC X(690).
                                                                        
      *----------------------------------------------------------------*
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
       77  FILLER   PIC  X(32) VALUE '*  INICIO DA WORKING CLLP7659  *'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       77  FILLER   PIC  X(32) VALUE '*  AREAS P/ TESTE FILE STATUS  *'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
       77  WRK-FS-ARQAVISO             PIC  X(02) VALUE SPACES.         
       77  WRK-FS-CONTMIGR             PIC  X(02) VALUE SPACES.         
       77  WRK-FS-AVINMIGR             PIC  X(02) VALUE SPACES.         
       77  WRK-FS-AVIMIGR              PIC  X(02) VALUE SPACES.         
       77  WRK-FS-RELCONS              PIC  X(02) VALUE SPACES.         
       77  WRK-FS-RELINCON             PIC  X(02) VALUE SPACES.         
                                                                        
       77  WRK-ABERTURA                PIC  X(13) VALUE 'NA ABERTURA'.  
       77  WRK-LEITURA                 PIC  X(13) VALUE 'NA LEITURA'.   
       77  WRK-GRAVACAO                PIC  X(13) VALUE 'NA GRAVACAO'.  
       77  WRK-FECHAMENTO              PIC  X(13) VALUE 'NO FECHAMENTO'.
       77  WRK-BATCH                   PIC  X(08) VALUE 'BATCH'.        
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       77  FILLER   PIC  X(32) VALUE '*  ACUMULADORES E AUXILIARES   *'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
       77  ACU-LIDOS-ARQAVISO          PIC  9(06) VALUE ZEROS COMP-3.   
       77  ACU-REGISTROS-MIGRADOS      PIC  9(06) VALUE ZEROS COMP-3.   
       77  ACU-REGISTROS-ACEITOS       PIC  9(06) VALUE ZEROS COMP-3.   
       77  ACU-LINHAS                  PIC  9(10) COMP-3 VALUE 59.      
       77  ACU-LINHAS1                 PIC  9(10) COMP-3 VALUE 59.      
       77  ACU-LIDOS-CONTMIGR          PIC  9(06) VALUE ZEROS COMP-3.   
       77  ACU-PAGINAS                 PIC  9(09) VALUE ZEROS COMP-3.   
       77  ACU-PAGINAS1                PIC  9(09) VALUE ZEROS COMP-3.   
                                                                        
       77  WRK-L-SEG                   PIC  Z.ZZZ.ZZZ.ZZ9 VALUE ZEROS.  
       77  WRK-L-ENT                   PIC  Z.ZZZ.ZZZ.ZZ9 VALUE ZEROS.  
       77  WRK-GRAVA                   PIC  Z.ZZZ.ZZZ.ZZ9 VALUE ZEROS.  
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       77  FILLER   PIC  X(32) VALUE '*            CHAVES            *'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
       01  WRK-CONTRATO-AUX            PIC +9(17) VALUE ZEROS.          
       01  WRK-CONTRATO-AUX-R    REDEFINES  WRK-CONTRATO-AUX.           
           03  FILLER                  PIC  X(01).                      
           03  FILLER                  PIC  9(10).                      
           03  WRK-CONTRATO-AUX-RR     PIC  9(07).                      
                                                                        
       01  WRK-CPF-AUX                 PIC +9(09) VALUE ZEROS.          
       01  WRK-CONTRATO-AUX-R    REDEFINES  WRK-CPF-AUX.                
           03  FILLER                  PIC  X(01).                      
           03  WRK-CPF-AUX-RR          PIC  9(09).                      
                                                                        
       01  WRK-FILIAL-AUX              PIC +9(04) VALUE ZEROS.          
       01  FILLER    REDEFINES WRK-FILIAL-AUX.                          
           03 FILLER                   PIC  X(01).                      
           03 WRK-FILIAL-AUX-RR        PIC  9(04).                      
                                                                        
       01  WRK-FIL-AVI-AUX             PIC  9(05) VALUE ZEROS.          
       01  FILLER    REDEFINES WRK-FIL-AVI-AUX.                         
           03 FILLER                   PIC  9(01).                      
           03 WRK-FIL-AVI-AUX-RR       PIC  9(04).                      
                                                                        
       01  WRK-DIG-AUX                 PIC +9(02) VALUE ZEROS.          
       01  FILLER    REDEFINES WRK-DIG-AUX.                             
           03 FILLER                   PIC  X(01).                      
           03 WRK-DIG-AUX-RR           PIC  9(02).                      
                                                                        
       01  WRK-CONTRATO-AUX1           PIC +9(07) VALUE ZEROS.          
       01  WRK-CONTRATO-AUX1-R    REDEFINES  WRK-CONTRATO-AUX1.         
           03  FILLER                  PIC  X(01).                      
           03  WRK-CONTRATO-AUX1-RR    PIC  9(07).                      
                                                                        
       01  WRK-AGENCIA-AUX             PIC +9(05) VALUE ZEROS.          
       01  WRK-AGENCIA-AUX-R     REDEFINES  WRK-AGENCIA-AUX.            
           03  FILLER                  PIC  X(01).                      
           03  WRK-AGENCIA-AUX-RR      PIC  9(05).                      
                                                                        
       01  WRK-CONTA-AUX               PIC +9(07) VALUE ZEROS.          
       01  WRK-CONTA-AUX-R       REDEFINES  WRK-CONTA-AUX.              
           03  FILLER                  PIC  X(01).                      
           03  WRK-CONTA-AUX-RR        PIC  9(07).                      
                                                                        
       01  WRK-CONT1-AUX               PIC +9(13) VALUE ZEROS.          
       01  WRK-CONT1-AUX-R       REDEFINES  WRK-CONT1-AUX.              
           03  FILLER                  PIC  X(01).                      
           03  FILLER                  PIC  9(06).                      
           03  WRK-CONT1-AUX-RR        PIC  9(07).                      
                                                                        
       01  WRK-CNPJ-AUX               PIC +9(10) VALUE ZEROS.           
       01  WRK-CNPJ-AUX-R       REDEFINES  WRK-CNPJ-AUX.                
           03  FILLER                 PIC  X(01).                       
           03  WRK-CNPJ-AUX-RR        PIC  9(10).                       
                                                                        
       01  WRK-TIPO-AUX               PIC +9(03) VALUE ZEROS.           
       01  WRK-TIPO-AUX-R       REDEFINES  WRK-TIPO-AUX.                
           03  FILLER                 PIC  X(01).                       
           03  WRK-TIPO-AUX-RR        PIC  9(03).                       
                                                                        
       01  WRK-SEQ-AUX               PIC +9(10) VALUE ZEROS.            
       01  WRK-SEQ-AUX-R       REDEFINES  WRK-SEQ-AUX.                  
           03  FILLER                 PIC  X(01).                       
           03  WRK-SEQ-AUX-RR        PIC  9(10).                        
                                                                        
       01  WRK-CHV-CONTMIGR.                                            
           03  WRK-AG-MIGR             PIC  9(05) VALUE ZEROS.          
           03  WRK-CC-MIGR             PIC  9(07) VALUE ZEROS.          
           03  WRK-CART-MIGR           PIC  X(03) VALUE SPACES.         
           03  WRK-CONTR-MIGR          PIC  9(07) VALUE ZEROS.          
                                                                        
       01  WRK-CHV-ARQAVISO.                                            
           03  WRK-AG-AVISO            PIC  9(05) VALUE ZEROS.          
           03  WRK-CC-AVISO            PIC  9(07) VALUE ZEROS.          
           03  WRK-CART-AVISO          PIC  X(03) VALUE SPACES.         
           03  WRK-CONTR-AVISO         PIC  9(07) VALUE ZEROS.          
                                                                        
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
                                                                        
       01  CABEC1.                                                      
           03  FILLER                  PIC X(001)       VALUE '1'.      
           03  FILLER                  PIC X(008)      VALUE 'CLLP7659'.
           03  FILLER                  PIC X(043)       VALUE SPACES.   
           03  FILLER                  PIC X(018)       VALUE           
               'BANCO BRADESCO S/A'.                                    
           03  FILLER                  PIC X(046)       VALUE SPACES.   
           03  FILLER                  PIC X(006)       VALUE 'FOLHA:'. 
           03  LINDET-PAGINA           PIC ZZZ.ZZZ.ZZ9 VALUE ZEROS.     
                                                                        
       01  CABEC5.                                                      
           03  FILLER                  PIC X(001)       VALUE '1'.      
           03  FILLER                  PIC X(008)      VALUE 'CLLP7659'.
           03  FILLER                  PIC X(043)       VALUE SPACES.   
           03  FILLER                  PIC X(018)       VALUE           
               'BANCO BRADESCO S/A'.                                    
           03  FILLER                  PIC X(046)       VALUE SPACES.   
           03  FILLER                  PIC X(006)       VALUE 'FOLHA:'. 
           03  LINDET-PAGINA1          PIC ZZZ.ZZZ.ZZ9   VALUE ZEROS.   
                                                                        
       01  WRK-LINBRANCO               PIC X(133)       VALUE SPACES.   
                                                                        
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
           03  FILLER                  PIC  X(022)      VALUE SPACES.   
           03  FILLER                  PIC  X(064)      VALUE           
              'PROCESSO RETIRADA DOS CONTRATOS MIGRADOS DA BASE CLLP (AV
      -       'ISOS)'.                                                  
           03  FILLER                  PIC  X(028)      VALUE SPACES.   
           03  CB2-HR-SIST.                                             
               05  CB2-HO-SIST         PIC  9(002)      VALUE ZEROS.    
               05  FILLER              PIC  X(001)      VALUE ':'.      
               05  CB2-MI-SIST         PIC  9(002)      VALUE ZEROS.    
               05  FILLER              PIC  X(001)      VALUE ':'.      
               05  CB2-SE-SIST         PIC  9(002)      VALUE ZEROS.    
                                                                        
       01  CABEC3.                                                      
           03  FILLER                  PIC  X(001)      VALUE '0'.      
           03  FILLER                  PIC  X(024)      VALUE SPACES.   
           03  FILLER                  PIC  X(080)      VALUE           
               'REGISTROS QUE CONTINUAM NO PROCESSAMENTO - NAO ENCONTRAD
      -        'OS NA BASE DE MIGRADOS'.                                
                                                                        
       01  CABEC4.                                                      
           03  FILLER                  PIC  X(001)      VALUE '0'.      
           03  FILLER                  PIC  X(022)      VALUE SPACES.   
           03  FILLER                  PIC  X(085)      VALUE           
               'REGISTROS RETIRADOS DO PROCESSAMENTO - ENCONTRADOS NA BA
      -        'SE DE MIGRADOS             '.                           
                                                                        
       01  LINDET-TOT-LIDOS.                                            
           03  FILLER                  PIC  X(041)      VALUE SPACES.   
           03  FILLER                  PIC  X(035)      VALUE           
               'TOTAL DE LIDOS CONTRATOS MIGRADOS :'.                   
           03  LDT-TOT-LIDOS-CONTR-M   PIC  ZZZ.ZZ9     VALUE ZEROS.    
                                                                        
       01  LINDET-TOT-LIDOS-PEND.                                       
           03  FILLER                  PIC  X(041)      VALUE SPACES.   
           03  FILLER                  PIC  X(035)      VALUE           
               'TOTAL DE LIDOS MOVIMENTO DE AVISOS:'.                   
           03  LDT-TOT-LIDOS-PEND      PIC  ZZZ.ZZ9     VALUE ZEROS.    
                                                                        
       01  LINDET-TOT-REG-MIGRADOS.                                     
           03  FILLER                  PIC  X(041)      VALUE SPACES.   
           03  FILLER                  PIC  X(035)      VALUE           
               'TOT RETIRADOS DO PROCESSO MIGRADOS:'.                   
           03  LDT-TOT-REG-MIGRADOS    PIC  ZZZ.ZZ9     VALUE ZEROS.    
                                                                        
       01  LINDET-TOT-REG-ACEITOS.                                      
           03  FILLER                  PIC  X(041)      VALUE SPACES.   
           03  FILLER                  PIC  X(035)      VALUE           
               'TOT DE REGISTROS P/ PROCESSAMENTO :'.                   
           03  LDT-TOT-REG-ACEITOS     PIC  ZZZ.ZZ9     VALUE ZEROS.    
                                                                        
       01  LINDET-SEM-DADOS1.                                           
           03  FILLER                  PIC  X(041)      VALUE SPACES.   
           03  FILLER                  PIC  X(051)      VALUE           
               '** ARQUIVO DE CONTRATOS MIGRADOS VAZIO           **'.   
                                                                        
       01  LINDET-SEM-DADOS2.                                           
           03  FILLER                  PIC  X(041)      VALUE SPACES.   
           03  FILLER                  PIC  X(051)      VALUE           
               '** ARQUIVO ARQDB22 VAZIO                         **'.   
                                                                        
       01  WRK-LINCAB1.                                                 
           03  FILLER                  PIC  X(012)       VALUE SPACES.  
           03  FILLER                  PIC  X(008)       VALUE          
               'CPF/CNPJ'.                                              
           03  FILLER                  PIC  X(010)       VALUE SPACES.  
           03  FILLER                  PIC  X(007)       VALUE          
               'AGENCIA'.                                               
           03  FILLER                  PIC  X(014)       VALUE SPACES.  
           03  FILLER                  PIC  X(005)       VALUE          
               'CONTA'.                                                 
           03  FILLER                  PIC  X(014)       VALUE SPACES.  
           03  FILLER                  PIC  X(008)       VALUE          
               'CARTEIRA'.                                              
           03  FILLER                  PIC  X(013)       VALUE SPACES.  
           03  FILLER                  PIC  X(008)       VALUE          
               'CONTRATO'.                                              
           03  FILLER                  PIC  X(015)       VALUE SPACES.  
           03  FILLER                  PIC  X(010)       VALUE          
               'VENCIMENTO'.                                            
                                                                        
       01  WRK-LINCAB2.                                                 
           03  FILLER                  PIC  X(008)       VALUE SPACES.  
           03  FILLER                  PIC  X(008)       VALUE          
               'CPF/CNPJ'.                                              
           03  FILLER                  PIC  X(009)       VALUE SPACES.  
           03  FILLER                  PIC  X(007)       VALUE          
               'AGENCIA'.                                               
           03  FILLER                  PIC  X(010)       VALUE SPACES.  
           03  FILLER                  PIC  X(005)       VALUE          
               'CONTA'.                                                 
           03  FILLER                  PIC  X(009)       VALUE SPACES.  
           03  FILLER                  PIC  X(008)       VALUE          
               'CARTEIRA'.                                              
           03  FILLER                  PIC  X(009)       VALUE SPACES.  
           03  FILLER                  PIC  X(008)       VALUE          
               'CONTRATO'.                                              
           03  FILLER                  PIC  X(008)       VALUE SPACES.  
           03  FILLER                  PIC  X(010)       VALUE          
               'VENCIMENTO'.                                            
           03  FILLER                  PIC  X(009)       VALUE SPACES.  
           03  FILLER                  PIC  X(020)       VALUE          
               'C H A V E      N A S'.                                  
                                                                        
       01  LINDET1.                                                     
           03  FILLER                  PIC  X(007)       VALUE SPACES.  
           03  LDT-CPF                 PIC  ZZZZZZZZ9    VALUE ZEROS.   
           03  FILLER                  PIC  X(001)       VALUE '/'.     
           03  LDT-FILIAL              PIC  99999        VALUE ZEROS.   
           03  FILLER                  PIC  X(001)       VALUE '-'.     
           03  LDT-DIG                 PIC  99           VALUE ZEROS.   
           03  FILLER                  PIC  X(007)       VALUE SPACES.  
           03  LDT-AGENCIA             PIC  ZZZZ9        VALUE ZEROS.   
           03  FILLER                  PIC  X(012)       VALUE SPACES.  
           03  LDT-CONTA               PIC  ZZZZZZ9      VALUE ZEROS.   
           03  FILLER                  PIC  X(019)       VALUE SPACES.  
           03  LDT-CARTEIRA            PIC  X(003)       VALUE SPACES.  
           03  FILLER                  PIC  X(012)       VALUE SPACES.  
           03  LDT-CONTRATO            PIC  ZZZZZZZZ9    VALUE ZEROS.   
           03  FILLER                  PIC  X(015)       VALUE SPACES.  
           03  LDT-DATA-VENCIMENTO     PIC  X(10)        VALUE SPACES.  
                                                                        
       01  LINDET2.                                                     
           03  FILLER                  PIC  X(003)       VALUE SPACES.  
           03  LDT-CPF1                PIC  ZZZZZZZZ9    VALUE ZEROS.   
           03  FILLER                  PIC  X(001)       VALUE '/'.     
           03  LDT-FILIAL1             PIC  99999        VALUE ZEROS.   
           03  FILLER                  PIC  X(001)       VALUE '-'.     
           03  LDT-DIG1                PIC  99           VALUE ZEROS.   
           03  FILLER                  PIC  X(006)       VALUE SPACES.  
           03  LDT-AGENCIA1            PIC  ZZZZ9        VALUE ZEROS.   
           03  FILLER                  PIC  X(008)       VALUE SPACES.  
           03  LDT-CONTA1              PIC  ZZZZZZ9      VALUE ZEROS.   
           03  FILLER                  PIC  X(014)       VALUE SPACES.  
           03  LDT-CARTEIRA1           PIC  X(003)       VALUE SPACES.  
           03  FILLER                  PIC  X(007)       VALUE SPACES.  
           03  LDT-CONTRATO1           PIC  ZZZZZZZZ9    VALUE ZEROS.   
           03  FILLER                  PIC  X(009)       VALUE SPACES.  
           03  LDT-DATA-VENCIMENTO1    PIC  X(10)        VALUE SPACES.  
           03  FILLER                  PIC  X(005)       VALUE SPACES.  
           03  LDT-CONTRATO-CVIVE      PIC  ZZZZZZZZZZ9  VALUE ZEROS.   
           03  FILLER                  PIC  X(001)       VALUE SPACES.  
           03  LDT-TIPO-CONTRATO-CVIVE PIC  ZZ9          VALUE ZEROS.   
           03  FILLER                  PIC  X(001)       VALUE SPACES.  
           03  LDT-SEQUENCIA-CVIVE     PIC  ZZZZZZZZZZ9  VALUE ZEROS.   
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER   PIC  X(32) VALUE '*   FIM DA WORKING CLLP7659    *'.
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
                     AND WRK-CHV-ARQAVISO EQUAL HIGH-VALUES.            
                                                                        
           PERFORM 3000-FINALIZAR.                                      
                                                                        
      *----------------------------------------------------------------*
       0000-99-FIM. EXIT.                                               
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       1000-INICIALIZAR SECTION.                                        
      *----------------------------------------------------------------*
                                                                        
           OPEN INPUT  CONTMIGR                                         
                       ARQAVISO                                         
                OUTPUT AVINMIGR                                         
                       AVIMIGR                                          
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
               DISPLAY '********* CLLP7659 *********'                   
               DISPLAY '*                          *'                   
               DISPLAY '*  ARQUIVO CONTMIGR VAZIO   *'                  
               DISPLAY '*                          *'                   
               DISPLAY '********* CLLP7659 *********'                   
               WRITE FD-RELATO FROM LINDET-SEM-DADOS1                   
           END-IF.                                                      
                                                                        
           PERFORM 1400-LER-ARQAVISO.                                   
                                                                        
           IF (ACU-LIDOS-ARQAVISO EQUAL ZEROS)                          
               DISPLAY '********* CLLP7659 *********'                   
               DISPLAY '*                          *'                   
               DISPLAY '*  ARQUIVO ARQAVISO VAZIO   *'                  
               DISPLAY '*                          *'                   
               DISPLAY '********* CLLP7659 *********'                   
               WRITE FD-RELATO FROM LINDET-SEM-DADOS2                   
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       1000-99-FIM. EXIT.                                               
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS SECTION.                                 
      *----------------------------------------------------------------*
                                                                        
           PERFORM 1110-TESTAR-FS-CONTMIGR.                             
                                                                        
           PERFORM 1130-TESTAR-FS-AVINMIGR.                             
                                                                        
           PERFORM 1131-TESTAR-FS-AVIMIGR.                              
                                                                        
           PERFORM 1140-TESTAR-FS-ARQAVISO.                             
                                                                        
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
       1130-TESTAR-FS-AVINMIGR SECTION.                                 
      *----------------------------------------------------------------*
                                                                        
           IF (WRK-FS-AVINMIGR NOT EQUAL '00')                          
               MOVE   'AVINMIGR'       TO  WRK-NOME-ARQ                 
               MOVE    WRK-FS-AVINMIGR TO  WRK-FILE-STATUS              
               MOVE   'APL'            TO  ERR-TIPO-ACESSO              
               MOVE    WRK-MSG-FS-ERRO TO  ERR-TEXTO                    
               PERFORM 9999-ROTINA-ERRO                                 
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       1130-99-FIM. EXIT.                                               
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       1131-TESTAR-FS-AVIMIGR SECTION.                                  
      *----------------------------------------------------------------*
                                                                        
           IF (WRK-FS-AVIMIGR NOT EQUAL '00')                           
               MOVE   'AVIMIGR'        TO  WRK-NOME-ARQ                 
               MOVE    WRK-FS-AVIMIGR  TO  WRK-FILE-STATUS              
               MOVE   'APL'            TO  ERR-TIPO-ACESSO              
               MOVE    WRK-MSG-FS-ERRO TO  ERR-TEXTO                    
               PERFORM 9999-ROTINA-ERRO                                 
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       1131-99-FIM. EXIT.                                               
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       1140-TESTAR-FS-ARQAVISO SECTION.                                 
      *----------------------------------------------------------------*
                                                                        
           IF (WRK-FS-ARQAVISO NOT EQUAL '00')                          
               MOVE   'ARQAVISO'       TO  WRK-NOME-ARQ                 
               MOVE    WRK-FS-ARQAVISO TO  WRK-FILE-STATUS              
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
                                                                        
           IF (WRK-FS-RELCONS NOT EQUAL '00')                           
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
       1160-TESTAR-FS-RELINCON    SECTION.                              
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
                                                                        
           MOVE FD-CAG-CONTR-ORIGE     TO  WRK-AGENCIA-AUX.             
           MOVE WRK-AGENCIA-AUX-RR     TO  WRK-AG-MIGR.                 
           MOVE FD-CCTA-CONTR-ORIGE    TO  WRK-CONT1-AUX.               
           MOVE WRK-CONT1-AUX-RR       TO  WRK-CC-MIGR.                 
           MOVE FD-CCART-CONTR-ORIGE   TO  WRK-CART-MIGR.               
           MOVE FD-CCONTR-NEGOC-ORIGE  TO  WRK-CONTRATO-AUX.            
           MOVE WRK-CONTRATO-AUX-RR    TO  WRK-CONTR-MIGR.              
                                                                        
      *----------------------------------------------------------------*
       1200-99-FIM. EXIT.                                               
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       1310-GRAVAR-AVINMIGR SECTION.                                    
      *----------------------------------------------------------------*
                                                                        
           INITIALIZE FD-AVINMIGR     OF AVINMIGR.                      
                                                                        
           WRITE FD-AVINMIGR     FROM DB2-REGTO.                        
                                                                        
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.                 
                                                                        
           PERFORM 1130-TESTAR-FS-AVINMIGR.                             
                                                                        
           ADD 1 TO ACU-REGISTROS-ACEITOS.                              
                                                                        
      *----------------------------------------------------------------*
       1310-99-FIM. EXIT.                                               
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       1311-GRAVAR-AVIMIGR SECTION.                                     
      *----------------------------------------------------------------*
                                                                        
           INITIALIZE FD-AVIMIGR     OF AVIMIGR.                        
                                                                        
           WRITE FD-AVIMIGR          FROM DB2-REGTO.                    
                                                                        
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.                 
                                                                        
           PERFORM 1131-TESTAR-FS-AVIMIGR.                              
                                                                        
           ADD 1 TO ACU-REGISTROS-MIGRADOS.                             
                                                                        
      *----------------------------------------------------------------*
       1311-99-FIM. EXIT.                                               
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       1400-LER-ARQAVISO SECTION.                                       
      *----------------------------------------------------------------*
                                                                        
           READ    ARQAVISO.                                            
                                                                        
           IF (WRK-FS-ARQAVISO EQUAL '10')                              
               MOVE    HIGH-VALUES     TO  WRK-CHV-ARQAVISO             
               GO                      TO  1400-99-FIM                  
           END-IF.                                                      
                                                                        
           MOVE    WRK-LEITURA         TO  WRK-OPERACAO.                
                                                                        
           PERFORM 1140-TESTAR-FS-ARQAVISO.                             
                                                                        
           ADD     1                   TO  ACU-LIDOS-ARQAVISO.          
                                                                        
           MOVE DB2-AGENCIA            TO  WRK-AG-AVISO.                
           MOVE DB2-CONTA              TO  WRK-CC-AVISO.                
           MOVE DB2-CARTEIRA           TO  WRK-CART-AVISO.              
           MOVE DB2-CONTRATO           TO  WRK-CONTR-AVISO.             
                                                                        
      *----------------------------------------------------------------*
       1400-99-FIM. EXIT.                                               
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       1410-IMPRIMIR-MIGRADOS       SECTION.                            
      *----------------------------------------------------------------*
                                                                        
           IF   ACU-LINHAS1  GREATER 58                                 
                WRITE FD-RELATO1 FROM WRK-LINBRANCO                     
                PERFORM 1422-MONTA-CABECALHO1                           
                MOVE ZEROS      TO ACU-LINHAS1                          
           END-IF.                                                      
                                                                        
           MOVE DB2-NUMERO-DEV           TO LDT-CPF1.                   
           MOVE DB2-FILIAL               TO LDT-FILIAL1.                
           MOVE DB2-CTR-DEV              TO LDT-DIG1.                   
           MOVE DB2-AGENCIA              TO WRK-AGENCIA-AUX.            
           MOVE WRK-AGENCIA-AUX-RR       TO LDT-AGENCIA1.               
           MOVE DB2-CONTA                TO WRK-CONTA-AUX.              
           MOVE WRK-CONTA-AUX-RR         TO LDT-CONTA1.                 
           MOVE DB2-CARTEIRA             TO LDT-CARTEIRA1.              
           MOVE DB2-CONTRATO             TO WRK-CONTRATO-AUX1.          
           MOVE WRK-CONTRATO-AUX1-RR     TO LDT-CONTRATO1.              
           MOVE DB2-VCTO                 TO LDT-DATA-VENCIMENTO1.       
                                                                        
           MOVE FD-CCONTR-NEGOC-ORIGE       TO WRK-CONTRATO-AUX.        
           MOVE WRK-CONTRATO-AUX-RR         TO LDT-CONTRATO1.           
           MOVE FD-CPSSOA-CONTR-CVIVE       TO WRK-CNPJ-AUX.            
           MOVE WRK-CNPJ-AUX-RR             TO LDT-CONTRATO-CVIVE.      
           MOVE FD-CTPO-CONTR-CVIVE         TO WRK-TIPO-AUX.            
           MOVE WRK-TIPO-AUX-RR             TO LDT-TIPO-CONTRATO-CVIVE. 
           MOVE FD-NSEQ-CONTR-CVIVE         TO WRK-SEQ-AUX.             
           MOVE WRK-SEQ-AUX-RR              TO LDT-SEQUENCIA-CVIVE.     
                                                                        
           WRITE FD-RELATO1 FROM LINDET2.                               
                                                                        
           ADD   1                 TO ACU-LINHAS1.                      
                                                                        
      *----------------------------------------------------------------*
       1410-99-FIM. EXIT.                                               
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       1420-IMPRIMIR-NAO-MIGRADOS   SECTION.                            
      *----------------------------------------------------------------*
                                                                        
      *    INIBIDO PARA NAO HAVER ESTOURO DE RELATORIO                  
                                                                        
           GO TO 1420-99-FIM.                                           
                                                                        
           IF   ACU-LINHAS   GREATER 58                                 
                WRITE FD-RELATO FROM WRK-LINBRANCO                      
                PERFORM 1421-MONTA-CABECALHO                            
                MOVE ZEROS      TO ACU-LINHAS                           
           END-IF.                                                      
                                                                        
           MOVE DB2-NUMERO-DEV           TO LDT-CPF.                    
           MOVE DB2-FILIAL               TO LDT-FILIAL.                 
           MOVE DB2-CTR-DEV              TO LDT-DIG.                    
           MOVE DB2-AGENCIA              TO WRK-AGENCIA-AUX.            
           MOVE WRK-AGENCIA-AUX-RR       TO LDT-AGENCIA.                
           MOVE DB2-CONTA                TO WRK-CONTA-AUX.              
           MOVE WRK-CONTA-AUX-RR         TO LDT-CONTA.                  
           MOVE DB2-CARTEIRA             TO LDT-CARTEIRA.               
           MOVE DB2-CONTRATO             TO WRK-CONTRATO-AUX1.          
           MOVE WRK-CONTRATO-AUX1-RR     TO LDT-CONTRATO.               
           MOVE DB2-VCTO                 TO LDT-DATA-VENCIMENTO.        
                                                                        
           WRITE FD-RELATO FROM LINDET1.                                
                                                                        
           ADD   1                 TO ACU-LINHAS.                       
                                                                        
      *----------------------------------------------------------------*
       1420-99-FIM. EXIT.                                               
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       1421-MONTA-CABECALHO     SECTION.                                
      *----------------------------------------------------------------*
                                                                        
           ADD   1                 TO   ACU-PAGINAS.                    
           MOVE  ACU-PAGINAS       TO   LINDET-PAGINA.                  
           MOVE  5                 TO   ACU-LINHAS.                     
           WRITE FD-RELATO         FROM CABEC1.                         
           WRITE FD-RELATO         FROM CABEC2.                         
           WRITE FD-RELATO         FROM CABEC3.                         
           WRITE FD-RELATO         FROM WRK-LINBRANCO.                  
           WRITE FD-RELATO         FROM WRK-LINTESTE.                   
           WRITE FD-RELATO         FROM WRK-LINCAB1.                    
           WRITE FD-RELATO         FROM WRK-LINTESTE.                   
                                                                        
      *----------------------------------------------------------------*
       1421-99-FIM. EXIT.                                               
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       1422-MONTA-CABECALHO1    SECTION.                                
      *----------------------------------------------------------------*
                                                                        
           ADD   1                 TO   ACU-PAGINAS1.                   
           MOVE  ACU-PAGINAS1      TO   LINDET-PAGINA1.                 
           MOVE  5                 TO   ACU-LINHAS1.                    
           WRITE FD-RELATO1        FROM CABEC5.                         
           WRITE FD-RELATO1        FROM CABEC2.                         
           WRITE FD-RELATO1        FROM CABEC4.                         
           WRITE FD-RELATO1        FROM WRK-LINBRANCO.                  
           WRITE FD-RELATO1        FROM WRK-LINTESTE.                   
           WRITE FD-RELATO1        FROM WRK-LINCAB2.                    
           WRITE FD-RELATO1        FROM WRK-LINTESTE.                   
                                                                        
      *----------------------------------------------------------------*
       1422-99-FIM. EXIT.                                               
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       2000-PROCESSAR SECTION.                                          
      *----------------------------------------------------------------*
                                                                        
           IF (WRK-CHV-ARQAVISO EQUAL WRK-CHV-CONTMIGR)                 
               PERFORM 1410-IMPRIMIR-MIGRADOS                           
               PERFORM 1311-GRAVAR-AVIMIGR                              
               PERFORM 1400-LER-ARQAVISO                                
           ELSE                                                         
               IF (WRK-CHV-ARQAVISO LESS WRK-CHV-CONTMIGR)              
                   PERFORM 1420-IMPRIMIR-NAO-MIGRADOS                   
                   PERFORM 1310-GRAVAR-AVINMIGR                         
                   PERFORM 1400-LER-ARQAVISO                            
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
                                                                        
           MOVE ACU-LIDOS-CONTMIGR      TO LDT-TOT-LIDOS-CONTR-M.       
           MOVE ACU-LIDOS-ARQAVISO      TO LDT-TOT-LIDOS-PEND.          
           MOVE ACU-REGISTROS-MIGRADOS  TO LDT-TOT-REG-MIGRADOS.        
           MOVE ACU-REGISTROS-ACEITOS   TO LDT-TOT-REG-ACEITOS.         
                                                                        
           IF ACU-LIDOS-CONTMIGR NOT EQUAL ZEROS                        
              WRITE FD-RELATO1 FROM WRK-LINBRANCO                       
              WRITE FD-RELATO1 FROM LINDET-TOT-LIDOS                    
              WRITE FD-RELATO1 FROM LINDET-TOT-LIDOS-PEND               
              WRITE FD-RELATO1 FROM LINDET-TOT-REG-ACEITOS              
              WRITE FD-RELATO1 FROM LINDET-TOT-REG-MIGRADOS             
           END-IF.                                                      
                                                                        
           IF ACU-LIDOS-ARQAVISO NOT EQUAL ZEROS                        
              WRITE FD-RELATO  FROM WRK-LINBRANCO                       
              WRITE FD-RELATO  FROM LINDET-TOT-LIDOS                    
              WRITE FD-RELATO  FROM LINDET-TOT-LIDOS-PEND               
              WRITE FD-RELATO  FROM LINDET-TOT-REG-ACEITOS              
              WRITE FD-RELATO  FROM LINDET-TOT-REG-MIGRADOS             
           END-IF.                                                      
                                                                        
           PERFORM 3100-MOSTRAR-DISPLAY.                                
                                                                        
           CLOSE   CONTMIGR                                             
                   ARQAVISO                                             
                   AVINMIGR                                             
                   AVIMIGR                                              
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
                                                                        
           MOVE ACU-LIDOS-CONTMIGR    TO  WRK-L-SEG.                    
           MOVE ACU-LIDOS-ARQAVISO    TO  WRK-L-ENT.                    
           MOVE ACU-REGISTROS-ACEITOS TO  WRK-GRAVA.                    
                                                                        
           DISPLAY '***************** CLLP7659 *******************'.    
           DISPLAY '* TOTAL LIDOS    ARQAVISO: ' WRK-L-ENT '     *'.    
           DISPLAY '* TOTAL LIDOS    CONTMIGR: ' WRK-L-SEG '     *'.    
           DISPLAY '* TOTAL GRAVADOS AVINMIGR: ' WRK-GRAVA '     *'.    
           DISPLAY '***************** CLLP7659 *******************'.    
                                                                        
      *----------------------------------------------------------------*
       3100-99-FIM. EXIT.                                               
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       9999-ROTINA-ERRO SECTION.                                        
      *----------------------------------------------------------------*
                                                                        
           MOVE   'CLLP7659'           TO  ERR-PGM.                     
                                                                        
           CALL   'POOL7100'        USING  WRK-BATCH                    
                                           ERRO-AREA.                   
                                                                        
           GOBACK.                                                      
                                                                        
      *----------------------------------------------------------------*
       9999-99-FIM. EXIT.                                               
      *----------------------------------------------------------------*

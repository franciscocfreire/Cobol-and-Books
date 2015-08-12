      *================================================================*
       IDENTIFICATION                  DIVISION.                        
      *================================================================*
                                                                        
       PROGRAM-ID. CLLP8670.                                            
       AUTHOR. MARCIO RODRIGO DA CUNHA.                                 
      *================================================================*
      *                   C P M   S I S T E M A S                      *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA     : CLLP8670                                     *
      *    PROGRAMADORA : MARCIO RODRIGO DA CUNHA      - CPM/FPOLIS    *
      *    ANALISTA CPM : JOSIANE TEREZINHA JOARES     - CPM/FPOLIS    *
      *    ANALISTA     : MAGDA - CPM                  - GRUPO 70      *
      *    DATA         : 24/03/2005                                   *
      *                                                                *
      *    OBJETIVO     :                                              *
      *      VERIFICAR SE O CLIENTE CONTINUA COM PENDENCIAS NO LPCL.   *
      *                                                                *
      *    ARQUIVOS:                                                   *
      *      DDNAME                              INCLUDE/BOOK          *
      *      MVTOEMIE                              I#CLLPPE            *
      *      DBSECARG                              I#CLLPFD            *
      *      MVTOEMIS                              I#CLLPPE            *
      *      DBSECARS                              I#CLLPFD            *
      *      RELATO                                --------            *
      *                                                                *
      *    MODULOS CHAMADOS:                                           *
      *      POOL7100 - TRATAMENTO DE ERROS DA APLICACAO               *
      *      POOL7600 - FORNECE DATA E HORA DO SISTEMA                 *
      *                                                                *
      *================================================================*
                                                                        
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
                                                                        
           SELECT MVTOEMIE ASSIGN      TO UT-S-MVTOEMIE                 
                      FILE STATUS      IS WRK-FS-MVTOEMIE.              
                                                                        
           SELECT DBSECARG ASSIGN      TO UT-S-DBSECARG                 
                      FILE STATUS      IS WRK-FS-DBSECARG.              
                                                                        
           SELECT MVTOEMIS ASSIGN      TO UT-S-MVTOEMIS                 
                      FILE STATUS      IS WRK-FS-MVTOEMIS.              
                                                                        
           SELECT DBSECARS ASSIGN      TO UT-S-DBSECARS                 
                      FILE STATUS      IS WRK-FS-DBSECARS.              
                                                                        
VH1112     SELECT MVTONENC ASSIGN      TO UT-S-MVTONENC                 
VH1112                FILE STATUS      IS WRK-FS-MVTONENC.              
                                                                        
           SELECT RELATO   ASSIGN      TO UT-S-RELATO                   
                      FILE STATUS      IS WRK-FS-RELATO.                
                                                                        
      *================================================================*
       DATA                            DIVISION.                        
      *================================================================*
                                                                        
      *----------------------------------------------------------------*
       FILE                            SECTION.                         
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *    INPUT  :   ARQUIVO DE ENTRADA MVTOEMIE                      *
      *               ORG. SEQUENCIAL   -   LRECL = 300                *
      *----------------------------------------------------------------*
                                                                        
       FD  MVTOEMIE                                                     
           RECORDING MODE IS F                                          
           LABEL RECORD   IS STANDARD                                   
           BLOCK CONTAINS  0 RECORDS.                                   
                                                                        
-INC I#CLLPPE                                                           
                                                                        
      *----------------------------------------------------------------*
      *    INPUT  :   ARQUIVO DE ENTRADA DBSECARG                      *
      *               ORG. SEQUENCIAL   -   LRECL = 690                *
      *----------------------------------------------------------------*
                                                                        
       FD  DBSECARG                                                     
           RECORDING MODE IS F                                          
           LABEL RECORD   IS STANDARD                                   
           BLOCK CONTAINS  0 RECORDS.                                   
                                                                        
-INC I#CLLPFD
                                                                        
      *----------------------------------------------------------------*
      *   OUTPUT  :   ARQUIVO DE SAIDA MVTOEMIS                        *
      *               ORG. SEQUENCIAL   -   LRECL = 300                *
      *----------------------------------------------------------------*
                                                                        
       FD  MVTOEMIS                                                     
           RECORDING MODE IS F                                          
           LABEL RECORD   IS STANDARD                                   
           BLOCK CONTAINS  0 RECORDS.                                   
                                                                        
-INC I#CLLPPE                                                           
                                                                        
      *----------------------------------------------------------------*
      *   OUTPUT  :   ARQUIVO DE SAIDA   DBSECARS                      *
      *               ORG. SEQUENCIAL   -   LRECL = 690                *
      *----------------------------------------------------------------*
                                                                        
       FD  DBSECARS                                                     
           RECORDING MODE IS F                                          
           LABEL RECORD   IS STANDARD                                   
           BLOCK CONTAINS  0 RECORDS.                                   
                                                                        
-INC I#CLLPFD
                                                                        
VH1112*----------------------------------------------------------------*
VH1112*   OUTPUT  :   ARQUIVO DE SAIDA MVTONENC                        *
VH1112*               ORG. SEQUENCIAL   -   LRECL = 300                *
VH1112*----------------------------------------------------------------*
VH1112                                                                  
VH1112 FD  MVTONENC                                                     
VH1112     RECORDING MODE IS F                                          
VH1112     LABEL RECORD   IS STANDARD                                   
VH1112     BLOCK CONTAINS  0 RECORDS.                                   
VH1112                                                                  
VH1112 01  FD-REG-MVTONENC             PIC  X(300).                     
                                                                        
                                                                        
      *----------------------------------------------------------------*
      *    OUTPUT :   RELATORIO DE MOVIMENTO PARA A EMISSAO DE AVISOS  *
      *               ORG. SEQUENCIAL   -   LRECL = 080                *
      *----------------------------------------------------------------*
                                                                        
       FD  RELATO                                                       
           RECORDING MODE IS F                                          
           LABEL RECORD   IS STANDARD                                   
           BLOCK CONTAINS  0 RECORDS.                                   
                                                                        
       01  REG-RELATO                  PIC X(80).                       
                                                                        
      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.                         
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(32)        VALUE          
           '*  INICIO DA WORKING CLLP8670  *'.                          
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(32)        VALUE          
           '*  ACUMULADORES  *'.                                        
      *----------------------------------------------------------------*
                                                                        
       77  ACU-LIDOS-MVTOEMIE          PIC  9(09) COMP-3 VALUE ZEROS.   
       77  ACU-LIDOS-DBSECARG          PIC  9(09) COMP-3 VALUE ZEROS.   
       77  ACU-GRAV-MVTOEMIS           PIC  9(09) COMP-3 VALUE ZEROS.   
       77  ACU-GRAV-DBSECARS           PIC  9(09) COMP-3 VALUE ZEROS.   
VH1112 77  ACU-GRAV-MVTONENC           PIC  9(09) COMP-3 VALUE ZEROS.   
                                                                        
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(32)        VALUE          
           '*  VARIAVEIS AUXILIARES  *'.                                
      *----------------------------------------------------------------*
                                                                        
       77  WRK-BATCH                   PIC  X(08)        VALUE 'BATCH'. 
       77  WRK-DATA-AUX                PIC  9(08)        VALUE ZEROS.   
       77  WRK-HORA-AUX                PIC  9(06)        VALUE ZEROS.   
       77  WRK-IND                     PIC  9(03)        VALUE ZEROS.   
       77  WRK-ACHOU                   PIC  X(01)        VALUE SPACES.  
       77  WRK-FS-MVTOEMIE             PIC  X(02)        VALUE SPACES.  
       77  WRK-FS-DBSECARG             PIC  X(02)        VALUE SPACES.  
       77  WRK-FS-MVTOEMIS             PIC  X(02)        VALUE SPACES.  
       77  WRK-FS-DBSECARS             PIC  X(02)        VALUE SPACES.  
VH1112 77  WRK-FS-MVTONENC             PIC  X(02)        VALUE SPACES.  
       77  WRK-FS-RELATO               PIC  X(02)        VALUE SPACES.  
       77  WRK-ABERTURA                PIC  X(13)        VALUE          
           'NA ABERTURA  '.                                             
       77  WRK-LEITURA                 PIC  X(13)        VALUE          
           'NA LEITURA   '.                                             
       77  WRK-GRAVACAO                PIC  X(13)        VALUE          
           'NA GRAVACAO  '.                                             
       77  WRK-FECHAMENTO              PIC  X(13)        VALUE          
           'NO FECHAMENTO'.                                             
                                                                        
                                                                        
       01         WRK-CHAVE-MVTOEMIE.                                   
          03      WRK-NUMCPF           PIC  9(09)        VALUE ZEROS.   
          03      WRK-FILCPF           PIC  9(05)        VALUE ZEROS.   
          03      WRK-CTRCPF           PIC  9(02)        VALUE ZEROS.   
                                                                        
                                                                        
       01         WRK-CHAVE-DBSECARG.                                   
          03      WRK-NUMCPF-DB        PIC  9(09)        VALUE ZEROS.   
          03      WRK-FILCPF-DB        PIC  9(05)        VALUE ZEROS.   
          03      WRK-CTRCPF-DB        PIC  9(02)        VALUE ZEROS.   
                                                                        
                                                                        
       01  WRK-DATA-AAAAMMDD.                                           
           03  WRK-ANO                 PIC 9(04)        VALUE ZEROS.    
           03  WRK-MES                 PIC 9(02)        VALUE ZEROS.    
           03  WRK-DIA                 PIC 9(02)        VALUE ZEROS.    
       01  WRK-DATA-AMD-R              REDEFINES                        
           WRK-DATA-AAAAMMDD           PIC 9(08).                       
                                                                        
       01  WRK-DATA-DDMMAAAA.                                           
           03  WRK-DD                  PIC 9(02)        VALUE ZEROS.    
           03  FILLER                  PIC X(01)        VALUE SPACES.   
           03  WRK-MM                  PIC 9(02)        VALUE ZEROS.    
           03  FILLER                  PIC X(01)        VALUE SPACES.   
           03  WRK-AAAA                PIC 9(04)        VALUE ZEROS.    
                                                                        
       01  WRK-VCTO.                                                    
           03  WRK-VCTO-EMIS           PIC  9(008).                     
                                                                        
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE          
           '*  AREAS DE MENSAGENS  *'.                                  
      *----------------------------------------------------------------*
                                                                        
       01  WRK-TEXTO.                                                   
           03  FILLER                  PIC X(07)          VALUE SPACES. 
           03  FILLER                  PIC X(07)          VALUE         
               '* ERRO '.                                               
           03  WRK-OPERACAO            PIC X(13)          VALUE SPACES. 
           03  FILLER                  PIC X(12)          VALUE         
               ' DO ARQUIVO '.                                          
           03  WRK-ARQUIVO             PIC X(08)          VALUE SPACES. 
           03  FILLER                  PIC X(17)          VALUE         
               ' - FILE STATUS = '.                                     
           03  WRK-FS                  PIC X(02)          VALUE SPACES. 
           03  FILLER                  PIC X(02)          VALUE ' *'.   
                                                                        
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE          
           '*  AREA DA POOL7600  *'.                                    
      *----------------------------------------------------------------*
                                                                        
       01  WRK-DATA-HORA.                                               
           03  WRK-DT-JULIANA          PIC  9(05) COMP-3 VALUE ZEROS.   
           03  WRK-DT-AAMMDD           PIC  9(07) COMP-3 VALUE ZEROS.   
           03  WRK-DT-AAAAMMDD         PIC  9(09) COMP-3 VALUE ZEROS.   
           03  WRK-TI-HHMMSS           PIC  9(07) COMP-3 VALUE ZEROS.   
           03  WRK-TI-HHMMSSMMMMMM     PIC  9(13) COMP-3 VALUE ZEROS.   
           03  WRK-TIMESTAMP           PIC  X(20)        VALUE SPACES.  
                                                                        
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE          
           '*  AREA DE CABECALHO *'.                                    
      *----------------------------------------------------------------*
                                                                        
       01  CABEC1.                                                      
           03  CB1-CARRO               PIC X(01)         VALUE '1'.     
           03  FILLER                  PIC X(30)         VALUE          
               'CLLP8670'.                                              
           03  FILLER                  PIC X(41)         VALUE          
               'BANCO BRADESCO S/A'.                                    
           03  FILLER                  PIC X(05)         VALUE          
               'PAG. '.                                                 
           03  CB1-PAG                 PIC ZZ9.                         
                                                                        
       01  CABEC2.                                                      
           03  CB2-CARRO               PIC X(01)         VALUE '0'.     
           03  CB2-DATA                PIC X(10)         VALUE SPACES.  
           03  FILLER                  PIC X(22)         VALUE          
               '            MOVIMENTO'.                                 
           03  FILLER                  PIC X(21)         VALUE          
               'PARA A EMISSAO DE AVI'.                                 
           03  FILLER                  PIC X(18)         VALUE          
               'SOS'.                                                   
           03  CB2-HORA                PIC X(08)         VALUE SPACES.  
                                                                        
       01  CABEC3.                                                      
           03  CB3-CARRO               PIC X(01)         VALUE '-'.     
           03  FILLER                  PIC X(21)         VALUE          
               '              CLLP867'.                                 
           03  FILLER                  PIC X(22)         VALUE          
               '0 - ARQUIVO MOVIMENTO'.                                 
           03  FILLER                  PIC X(21)         VALUE          
               'DE EMISSAO ESTA VAZIO'.                                 
                                                                        
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE          
           '*  AREA DE TOTAIS  *'.                                      
      *----------------------------------------------------------------*
                                                                        
       01  LINTOT1.                                                     
           03  LT1-CARRO               PIC X(01)         VALUE '-'.     
           03  FILLER                  PIC X(21)         VALUE SPACES.  
           03  FILLER                  PIC X(21)         VALUE          
               '************** CLLP86'.                                 
           03  FILLER                  PIC X(16)         VALUE          
               '70 *************'.                                      
                                                                        
       01  LINTOT2.                                                     
           03  LT2-CARRO               PIC X(01)         VALUE '0'.     
           03  FILLER                  PIC X(21)         VALUE SPACES.  
           03  FILLER                  PIC X(21)         VALUE          
               'REG. LIDOS     MVTOEM'.                                 
           03  FILLER                  PIC X(05)         VALUE          
               'IE -'.                                                  
           03  LT2-TOT-MVTOEMIE        PIC ZZZ.ZZZ.ZZ9.                 
                                                                        
       01  LINTOT3.                                                     
           03  LT3-CARRO               PIC X(01)         VALUE ' '.     
           03  FILLER                  PIC X(21)         VALUE SPACES.  
           03  FILLER                  PIC X(21)         VALUE          
               'REG. LIDOS     DBSECA'.                                 
           03  FILLER                  PIC X(05)         VALUE          
               'RG -'.                                                  
           03  LT3-TOT-DBSECARG        PIC ZZZ.ZZZ.ZZ9.                 
                                                                        
       01  LINTOT4.                                                     
           03  LT4-CARRO               PIC X(01)         VALUE ' '.     
           03  FILLER                  PIC X(21)         VALUE SPACES.  
           03  FILLER                  PIC X(21)         VALUE          
               'REG. GRAVADOS  MVTOEM'.                                 
           03  FILLER                  PIC X(05)         VALUE          
               'IS -'.                                                  
           03  LT4-TOT-MVTOEMIS        PIC ZZZ.ZZZ.ZZ9.                 
                                                                        
       01  LINTOT5.                                                     
           03  LT5-CARRO               PIC X(01)         VALUE ' '.     
           03  FILLER                  PIC X(21)         VALUE SPACES.  
           03  FILLER                  PIC X(21)         VALUE          
               'REG. GRAVADOS  DBSECA'.                                 
           03  FILLER                  PIC X(05)         VALUE          
               'RS -'.                                                  
           03  LT5-TOT-DBSECARS        PIC ZZZ.ZZZ.ZZ9.                 
                                                                        
       01  LINTOT6.                                                     
           03  LT6-CARRO               PIC X(01)         VALUE '0'.     
           03  FILLER                  PIC X(21)         VALUE SPACES.  
           03  FILLER                  PIC X(21)         VALUE          
               '************** CLLP86'.                                 
           03  FILLER                  PIC X(16)         VALUE          
               '70 *************'.                                      
                                                                        
VH1112 01  LINTOT7.                                                     
VH1112     03  LT7-CARRO               PIC X(01)         VALUE ' '.     
VH1112     03  FILLER                  PIC X(21)         VALUE SPACES.  
VH1112     03  FILLER                  PIC X(21)         VALUE          
VH1112         'REG. GRAVADOS  MVNENC'.                                 
VH1112     03  FILLER                  PIC X(05)         VALUE          
VH1112         'RS -'.                                                  
VH1112     03  LT7-TOT-MVTONENC        PIC ZZZ.ZZZ.ZZ9   VALUE ZEROS.   
                                                                        
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE          
           '*  AREA DA POOL7100  *'.                                    
      *----------------------------------------------------------------*
                                                                        
-INC POL7100C                                                           
                                                                        
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE          
           '*  FIM DA WORKING CLLP8670  *'.                             
      *----------------------------------------------------------------*
                                                                        
      *================================================================*
       PROCEDURE                       DIVISION.                        
      *================================================================*
                                                                        
      *----------------------------------------------------------------*
       000000-INICIAR                  SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           PERFORM 100000-INICIALIZAR.                                  
                                                                        
           PERFORM 200000-VERIFICAR-VAZIO.                              
                                                                        
           PERFORM 300000-PROCESSAR                                     
               UNTIL WRK-CHAVE-MVTOEMIE                                 
                                       EQUAL HIGH-VALUES.               
                                                                        
           PERFORM 400000-FINALIZAR.                                    
                                                                        
      *----------------------------------------------------------------*
       000000-99-FIM.                  EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       100000-INICIALIZAR              SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           OPEN  INPUT MVTOEMIE                                         
                       DBSECARG                                         
                OUTPUT MVTOEMIS                                         
                       DBSECARS                                         
VH1112                 MVTONENC                                         
                       RELATO.                                          
                                                                        
           MOVE WRK-ABERTURA           TO WRK-OPERACAO.                 
                                                                        
           PERFORM 110000-TESTAR-FILE-STATUS.                           
                                                                        
           PERFORM 120000-OBTER-DATA-HORA.                              
                                                                        
      *----------------------------------------------------------------*
       100000-99-FIM.                  EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       110000-TESTAR-FILE-STATUS       SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           PERFORM 111000-TESTAR-FS-MVTOEMIE.                           
                                                                        
           PERFORM 112000-TESTAR-FS-DBSECARG.                           
                                                                        
           PERFORM 113000-TESTAR-FS-MVTOEMIS.                           
                                                                        
           PERFORM 114000-TESTAR-FS-RELATO.                             
                                                                        
           PERFORM 115000-TESTAR-FS-DBSECARS.                           
                                                                        
VH1112     PERFORM 1160-00-TESTAR-FS-MVTONENC.                          
                                                                        
      *----------------------------------------------------------------*
       110000-99-FIM.                  EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       111000-TESTAR-FS-MVTOEMIE       SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           IF WRK-FS-MVTOEMIE          NOT EQUAL '00'                   
              MOVE 'APL'               TO ERR-TIPO-ACESSO               
              MOVE 'MVTOEMIE'          TO WRK-ARQUIVO                   
              MOVE WRK-FS-MVTOEMIE     TO WRK-FS                        
              MOVE WRK-TEXTO           TO ERR-TEXTO                     
              PERFORM 999999-TRATAR-ERRO                                
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       111000-99-FIM.                  EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       112000-TESTAR-FS-DBSECARG       SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           IF WRK-FS-DBSECARG          NOT EQUAL '00'                   
              MOVE 'APL'               TO ERR-TIPO-ACESSO               
              MOVE 'DBSECARG'          TO WRK-ARQUIVO                   
              MOVE WRK-FS-DBSECARG     TO WRK-FS                        
              MOVE WRK-TEXTO           TO ERR-TEXTO                     
              PERFORM 999999-TRATAR-ERRO                                
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       112000-99-FIM.                  EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       113000-TESTAR-FS-MVTOEMIS       SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           IF WRK-FS-MVTOEMIS          NOT EQUAL '00'                   
              MOVE 'APL'               TO ERR-TIPO-ACESSO               
              MOVE 'MVTOEMIS'          TO WRK-ARQUIVO                   
              MOVE WRK-FS-MVTOEMIS     TO WRK-FS                        
              MOVE WRK-TEXTO           TO ERR-TEXTO                     
              PERFORM 999999-TRATAR-ERRO                                
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       113000-99-FIM.                  EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       114000-TESTAR-FS-RELATO         SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           IF WRK-FS-RELATO            NOT EQUAL '00'                   
              MOVE 'APL'               TO ERR-TIPO-ACESSO               
              MOVE 'RELATO'            TO WRK-ARQUIVO                   
              MOVE WRK-FS-RELATO       TO WRK-FS                        
              MOVE WRK-TEXTO           TO ERR-TEXTO                     
              PERFORM 999999-TRATAR-ERRO                                
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       114000-99-FIM.                  EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       115000-TESTAR-FS-DBSECARS       SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           IF WRK-FS-DBSECARS          NOT EQUAL '00'                   
              MOVE 'APL'               TO ERR-TIPO-ACESSO               
              MOVE 'DBSECARS'          TO WRK-ARQUIVO                   
              MOVE WRK-FS-DBSECARS     TO WRK-FS                        
              MOVE WRK-TEXTO           TO ERR-TEXTO                     
              PERFORM 999999-TRATAR-ERRO                                
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       115000-99-FIM.                  EXIT.                            
      *----------------------------------------------------------------*
                                                                        
VH1112*----------------------------------------------------------------*
VH1112 1160-00-TESTAR-FS-MVTONENC       SECTION.                        
VH1112*----------------------------------------------------------------*
VH1112                                                                  
VH1112     IF WRK-FS-MVTONENC          NOT EQUAL '00'                   
VH1112        MOVE 'APL'               TO ERR-TIPO-ACESSO               
VH1112        MOVE 'MVTONENC'          TO WRK-ARQUIVO                   
VH1112        MOVE WRK-FS-MVTONENC     TO WRK-FS                        
VH1112        MOVE WRK-TEXTO           TO ERR-TEXTO                     
VH1112        PERFORM 999999-TRATAR-ERRO                                
VH1112     END-IF.                                                      
VH1112                                                                  
VH1112*----------------------------------------------------------------*
VH1112 1160-00-99-FIM.                  EXIT.                           
VH1112*----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       120000-OBTER-DATA-HORA          SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           CALL 'POOL7600'             USING WRK-DATA-HORA.             
                                                                        
           MOVE WRK-DT-AAAAMMDD        TO WRK-DATA-AUX.                 
           MOVE WRK-TI-HHMMSS          TO WRK-HORA-AUX.                 
                                                                        
           STRING WRK-DATA-AUX(7:2) '/'                                 
                  WRK-DATA-AUX(5:2) '/'                                 
                  WRK-DATA-AUX(1:4)                                     
           DELIMITED BY SIZE           INTO CB2-DATA.                   
                                                                        
           STRING WRK-HORA-AUX(1:2) ':'                                 
                  WRK-HORA-AUX(3:2) ':'                                 
                  WRK-HORA-AUX(5:2)                                     
           DELIMITED BY SIZE           INTO CB2-HORA.                   
                                                                        
      *----------------------------------------------------------------*
       120000-99-FIM.                  EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       200000-VERIFICAR-VAZIO          SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           PERFORM 210000-LER-MVTOEMIE.                                 
                                                                        
           IF  WRK-CHAVE-MVTOEMIE      EQUAL HIGH-VALUES                
               DISPLAY '********** CLLP8670 **********'                 
               DISPLAY '*                            *'                 
               DISPLAY '*   ARQUIVO MVTOEMIE VAZIO   *'                 
               DISPLAY '*                            *'                 
               DISPLAY '********** CLLP8670 **********'                 
               MOVE WRK-GRAVACAO       TO WRK-OPERACAO                  
               MOVE 1                  TO CB1-PAG                       
               WRITE REG-RELATO        FROM CABEC1                      
               PERFORM 114000-TESTAR-FS-RELATO                          
               WRITE REG-RELATO        FROM CABEC2                      
               PERFORM 114000-TESTAR-FS-RELATO                          
               WRITE REG-RELATO        FROM CABEC3                      
               PERFORM 114000-TESTAR-FS-RELATO                          
               PERFORM 400000-FINALIZAR                                 
           END-IF.                                                      
                                                                        
           PERFORM 220000-LER-DBSECARG.                                 
                                                                        
           IF  WRK-CHAVE-DBSECARG      EQUAL HIGH-VALUES                
               DISPLAY '****************** CLLP8670 ******************' 
               DISPLAY '*                                            *' 
               DISPLAY '*   CLLP8670 - ARQUIVO DBSECARG ESTA VAZIO   *' 
               DISPLAY '*           PROCESSAMENTO CANCELADO          *' 
               DISPLAY '*                                            *' 
               DISPLAY '****************** CLLP8670 ******************' 
               MOVE 'APL'              TO ERR-TIPO-ACESSO               
               MOVE 'CLLP8670 - ARQUIVO DBSECARG ESTA VAZIO'            
                                       TO ERR-TEXTO                     
               PERFORM 999999-TRATAR-ERRO                               
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       200000-99-FIM.                  EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       210000-LER-MVTOEMIE             SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           READ MVTOEMIE.                                               
                                                                        
           IF WRK-FS-MVTOEMIE          EQUAL '10'                       
              MOVE HIGH-VALUES         TO WRK-CHAVE-MVTOEMIE            
              GO                       TO 210000-99-FIM                 
           END-IF.                                                      
                                                                        
           MOVE WRK-LEITURA            TO WRK-OPERACAO.                 
                                                                        
           PERFORM 111000-TESTAR-FS-MVTOEMIE.                           
                                                                        
           ADD 1                       TO ACU-LIDOS-MVTOEMIE.           
                                                                        
           MOVE CMVTO-NUMCPF  OF MVTOEMIE                               
                                       TO WRK-NUMCPF.                   
           MOVE CMVTO-FILCPF  OF MVTOEMIE                               
                                       TO WRK-FILCPF.                   
           MOVE CMVTO-CTRCPF  OF MVTOEMIE                               
                                       TO WRK-CTRCPF.                   
                                                                        
      *----------------------------------------------------------------*
       210000-99-FIM.                  EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       220000-LER-DBSECARG             SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           READ DBSECARG.                                               
                                                                        
           IF WRK-FS-DBSECARG          EQUAL '10'                       
              MOVE HIGH-VALUES         TO WRK-CHAVE-DBSECARG            
              GO                       TO 220000-99-FIM                 
           END-IF.                                                      
                                                                        
           MOVE WRK-LEITURA            TO WRK-OPERACAO.                 
                                                                        
           PERFORM 112000-TESTAR-FS-DBSECARG.                           
                                                                        
           ADD 1                       TO ACU-LIDOS-DBSECARG.           
                                                                        
           MOVE DB2-NUMERO-DEV OF DBSECARG  TO WRK-NUMCPF-DB.           
           MOVE DB2-FILIAL     OF DBSECARG  TO WRK-FILCPF-DB.           
           MOVE DB2-CTR-DEV    OF DBSECARG  TO WRK-CTRCPF-DB.           
                                                                        
      *----------------------------------------------------------------*
       220000-99-FIM.                  EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       300000-PROCESSAR                SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           IF  WRK-CHAVE-MVTOEMIE      LESS WRK-CHAVE-DBSECARG          
               MOVE  CMVTO-VCTO  OF  MVTOEMIE  TO  WRK-VCTO-EMIS        
VH1112*........XISPLAY                                                  
VH1112*        '************************************************'       
VH1112*........XISPLAY                                                  
VH1112*        '*                                              *'       
VH1112*........XISPLAY                                                  
VH1112*        '* CPF/CNPJ  INEXISTENTE NO DBSECARG            *'       
VH1112*........XISPLAY                                                  
VH1112*        '* CPF/CNPJ  MVTOEMIE      - ' WRK-CHAVE-MVTOEMIE        
VH1112*........XISPLAY                                                  
VH1112*        '* VENCIMENTO DO MOVIMENTO - ' WRK-VCTO-EMIS             
VH1112*........XISPLAY                                                  
VH1112*        '*                                              *'       
VH1112*........XISPLAY                                                  
VH1112*        '************************************************'       
VH1112         PERFORM 3150-00-GRAVAR-MVTONENC                          
               PERFORM 210000-LER-MVTOEMIE                              
           ELSE                                                         
               IF  WRK-CHAVE-MVTOEMIE  GREATER WRK-CHAVE-DBSECARG       
                   PERFORM 220000-LER-DBSECARG                          
               ELSE                                                     
                   PERFORM 310000-PROCESSA-IGUAIS                       
               END-IF                                                   
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       300000-99-FIM.                  EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       310000-PROCESSA-IGUAIS          SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           PERFORM 313000-GRAVAR-MVTOEMIS.                              
                                                                        
VH1112     PERFORM UNTIL                                                
VH1112             WRK-CHAVE-DBSECARG  GREATER WRK-CHAVE-MVTOEMIE       
VH1112             PERFORM 314000-GRAVAR-DBSECARS                       
VH1112             PERFORM 220000-LER-DBSECARG                          
VH1112     END-PERFORM                                                  
                                                                        
VH1112     PERFORM 210000-LER-MVTOEMIE.                                 
                                                                        
      *----------------------------------------------------------------*
       310000-99-FIM.                  EXIT.                            
      *----------------------------------------------------------------*
                                                                        
                                                                        
      *----------------------------------------------------------------*
       313000-GRAVAR-MVTOEMIS          SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.                 
                                                                        
           WRITE REG-CONTMVTO OF MVTOEMIS FROM REG-CONTMVTO OF MVTOEMIE.
                                                                        
           PERFORM 113000-TESTAR-FS-MVTOEMIS.                           
                                                                        
           ADD 1                       TO ACU-GRAV-MVTOEMIS.            
                                                                        
      *----------------------------------------------------------------*
       313000-99-FIM.                  EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       314000-GRAVAR-DBSECARS          SECTION.                         
      *----------------------------------------------------------------*
                                                                        
                                                                        
           WRITE DB2-REGTO OF DBSECARS FROM DB2-REGTO OF DBSECARG.      
                                                                        
           MOVE  WRK-GRAVACAO            TO  WRK-OPERACAO.              
                                                                        
           PERFORM 115000-TESTAR-FS-DBSECARS.                           
                                                                        
           ADD 1                         TO  ACU-GRAV-DBSECARS.         
                                                                        
      *----------------------------------------------------------------*
       314000-99-FIM.                  EXIT.                            
      *----------------------------------------------------------------*
                                                                        
                                                                        
VH1112*----------------------------------------------------------------*
VH1112 3150-00-GRAVAR-MVTONENC          SECTION.                        
VH1112*----------------------------------------------------------------*
VH1112                                                                  
VH1112     MOVE WRK-GRAVACAO           TO WRK-OPERACAO.                 
VH1112                                                                  
VH1112     WRITE FD-REG-MVTONENC          FROM REG-CONTMVTO OF MVTOEMIE.
VH1112                                                                  
VH1112     PERFORM 1160-00-TESTAR-FS-MVTONENC.                          
VH1112                                                                  
VH1112     ADD 1                       TO ACU-GRAV-MVTONENC.            
VH1112                                                                  
VH1112*----------------------------------------------------------------*
VH1112 3150-00-99-FIM.                  EXIT.                           
VH1112*----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       400000-FINALIZAR                SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           IF  ACU-LIDOS-MVTOEMIE      GREATER ZEROS                    
               PERFORM 410000-EMITIR-RELATORIO                          
           END-IF.                                                      
                                                                        
           CLOSE MVTOEMIE                                               
                 DBSECARG                                               
                 MVTOEMIS                                               
                 DBSECARS                                               
VH1112           MVTONENC                                               
                 RELATO.                                                
                                                                        
           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.                 
                                                                        
           PERFORM 110000-TESTAR-FILE-STATUS.                           
                                                                        
           GOBACK.                                                      
                                                                        
      *----------------------------------------------------------------*
       400000-99-FIM.                  EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       410000-EMITIR-RELATORIO         SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.                 
           MOVE 1                      TO CB1-PAG.                      
           MOVE ACU-LIDOS-MVTOEMIE     TO LT2-TOT-MVTOEMIE.             
           MOVE ACU-LIDOS-DBSECARG     TO LT3-TOT-DBSECARG.             
           MOVE ACU-GRAV-MVTOEMIS      TO LT4-TOT-MVTOEMIS.             
           MOVE ACU-GRAV-DBSECARS      TO LT5-TOT-DBSECARS.             
VH1112     MOVE ACU-GRAV-MVTONENC      TO LT7-TOT-MVTONENC.             
                                                                        
           WRITE REG-RELATO            FROM CABEC1.                     
           PERFORM 114000-TESTAR-FS-RELATO.                             
                                                                        
           WRITE REG-RELATO            FROM CABEC2.                     
           PERFORM 114000-TESTAR-FS-RELATO.                             
                                                                        
           WRITE REG-RELATO            FROM LINTOT1.                    
           PERFORM 114000-TESTAR-FS-RELATO.                             
                                                                        
           WRITE REG-RELATO            FROM LINTOT2.                    
           PERFORM 114000-TESTAR-FS-RELATO.                             
                                                                        
           WRITE REG-RELATO            FROM LINTOT3.                    
           PERFORM 114000-TESTAR-FS-RELATO.                             
                                                                        
           WRITE REG-RELATO            FROM LINTOT4.                    
           PERFORM 114000-TESTAR-FS-RELATO.                             
                                                                        
           WRITE REG-RELATO            FROM LINTOT5.                    
           PERFORM 114000-TESTAR-FS-RELATO.                             
                                                                        
VH1112     WRITE REG-RELATO            FROM LINTOT7.                    
VH1112     PERFORM 114000-TESTAR-FS-RELATO.                             
                                                                        
           WRITE REG-RELATO            FROM LINTOT6.                    
           PERFORM 114000-TESTAR-FS-RELATO.                             
                                                                        
      *----------------------------------------------------------------*
       410000-99-FIM.                  EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       999999-TRATAR-ERRO              SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE 'CLLP8670'             TO ERR-PGM.                      
                                                                        
           CALL 'POOL7100'             USING WRK-BATCH                  
                                             ERRO-AREA.                 
                                                                        
           GOBACK.                                                      
                                                                        
      *----------------------------------------------------------------*
       999999-99-FIM.                  EXIT.                            
      *----------------------------------------------------------------*

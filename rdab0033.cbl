      *================================================================*
       IDENTIFICATION                  DIVISION.                        
      *================================================================*
                                                                        
       PROGRAM-ID. RDAB0033.                                            
       AUTHOR.     FERNANDO BATISTI.                                    
      *================================================================*
      *                    C P M  -  S I S T E M A S                   *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   RDAB0033                                    *
      *    PROGRAMADOR.:   FERNANDO BATISTI        -  CPM PATO BRANCO  *
      *    ANALISTA CPM:   EDINA FATINA GNOATO     -  CPM PATO BRANCO  *
      *    ANALISTA....:   CARMEN                  -  G&P / GRUPO 82   *
      *    DATA........:   19/03/2004                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   FORMATAR DADOS CADASTRAIS DO SOCIO.         *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME                          INCLUDE/BOOK    *
      *                    ENTSOCIO                      I#RDAB17      *
      *                    PENDFICA                      I#FICAC8      *
      *                    SAISOCIO                      I#RDAB17      *
      *                    RELATO                            -         *
      *----------------------------------------------------------------*
      *    INC'S.......:                                               *
      *    POL7100C - AREA PARA TRATAMENTO DE ERRO PELA POOL7100       *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    POOL7100 - TRATAMENTO DE ERRO QUANDO PROGRAMA INVALIDO      *
      *    POOL7600 - OBTER DATA/HORA DO SISTEMA                       *
      *================================================================*
      *                                                               * 
BRQ059* ============================================================= * 
      * |                      ALTERACAO                            | * 
      * ------------------------------------------------------------+ * 
      *  ANALISTA     : ROBSON VELASQUES / BRQ - IT SERVICES        | * 
      *  PROGRAMADOR  : CRISTIANO SOUZA  / BRQ - IT SERVICES        | * 
      *  DATA         : 04/2012                                     | * 
      *  OBJETIVO     : INSERIR UM DIGITO A MAIS AOS CAMPOS DE      | * 
      *                 TELEFONES                                   | * 
      *  PROJETO      : 2012/0059 - ADEQUAR NRO TELEFONE(RES.553)   | * 
      * ============================================================+ * 
BRQ059***************************************************************** 
           EJECT                                                        
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
                                                                        
           SELECT ENTSOCIO ASSIGN      TO UT-S-ENTSOCIO                 
           FILE STATUS                 IS WRK-FS-ENTSOCIO.              
                                                                        
           SELECT PENDFICA ASSIGN      TO UT-S-PENDFICA                 
           FILE STATUS                 IS WRK-FS-PENDFICA.              
                                                                        
           SELECT SAISOCIO ASSIGN      TO UT-S-SAISOCIO                 
           FILE STATUS                 IS WRK-FS-SAISOCIO.              
                                                                        
           SELECT RELATO   ASSIGN      TO UT-S-RELATO                   
           FILE STATUS                 IS WRK-FS-RELATO.                
                                                                        
           EJECT                                                        
      *================================================================*
       DATA                            DIVISION.                        
      *================================================================*
                                                                        
      *----------------------------------------------------------------*
       FILE                            SECTION.                         
      *----------------------------------------------------------------*
           EJECT                                                        
      *----------------------------------------------------------------*
      *    INPUT  -  ARQUIVO DE SOCIO                                  *
      *              ORG. SEQUENCIAL    -    LRECL   =  223            *
      *----------------------------------------------------------------*
                                                                        
       FD  ENTSOCIO                                                     
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       COPY 'I#RDAB17'.
                                                                        
      *----------------------------------------------------------------*
      *    INPUT  -  ARQUIVO IMAGEM DA TABELA FICAV001                 *
      *              ORG. SEQUENCIAL    -    LRECL   =  516            *
      *----------------------------------------------------------------*
                                                                        
       FD  PENDFICA                                                     
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       COPY 'I#FICAC8'.

      *----------------------------------------------------------------*
      *    OUTPUT -  ARQUIVO DE SOCIOS                                 *
      *              ORG. SEQUENCIAL    -    LRECL   =  223            *
      *----------------------------------------------------------------*
                                                                        
       FD  SAISOCIO                                                     
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    

       COPY 'I#RDAB17'.
                                                                        
      *----------------------------------------------------------------*
      *    OUTPUT -  RELATORIO QUANTIDADE DE REGISTROS TRATADOS        *
      *              ORG. SEQUENCIAL    -    LRECL   =  080            *
      *----------------------------------------------------------------*
                                                                        
       FD  RELATO                                                       
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  REG-RELATO                  PIC  X(080).                     
                                                                        
           EJECT                                                        
      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.                         
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)          VALUE       
           '*** RDAB0033 - INICIO DA AREA DE WORKING ***'.              
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE        
           '*** AREA DE ACUMULADORES ***'.                              
      *----------------------------------------------------------------*
                                                                        
       77  ACU-LIDOS-ENTSOCIO          PIC  9(013) COMP-3  VALUE ZEROS. 
       77  ACU-LIDOS-PENDFICA          PIC  9(013) COMP-3  VALUE ZEROS. 
       77  ACU-DESPR-PENDFICA          PIC  9(013) COMP-3  VALUE ZEROS. 
       77  ACU-CORRE-PENDFICA          PIC  9(013) COMP-3  VALUE ZEROS. 
       77  ACU-GRAVA-SAISOCIO          PIC  9(013) COMP-3  VALUE ZEROS. 
                                                                        
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE        
           '*** AREA PARA TESTES DE FILE STATUS ***'.                   
      *----------------------------------------------------------------*
                                                                        
       77  WRK-FS-ENTSOCIO             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-PENDFICA             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-SAISOCIO             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-RELATO               PIC  X(002)         VALUE SPACES.
                                                                        
       77  WRK-ABERTURA                PIC  X(013)         VALUE        
           ' NA ABERTURA '.                                             
       77  WRK-LEITURA                 PIC  X(013)         VALUE        
           ' NA  LEITURA '.                                             
       77  WRK-GRAVACAO                PIC  X(013)         VALUE        
           ' NA GRAVACAO '.                                             
       77  WRK-FECHAMENTO              PIC  X(013)         VALUE        
           'NO FECHAMENTO'.                                             
                                                                        
           EJECT                                                        
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE        
           '*** VARIAVEIS AUXILIARES ***'.                              
      *----------------------------------------------------------------*
                                                                        
       77  WRK-BATCH                   PIC  X(008)         VALUE        
           'BATCH'.                                                     
                                                                        
BRQ059 01  WRK-CFONE                   PIC S9(011)         VALUE ZEROS. 
BRQ059 01  WRK-CFONE-R                 REDEFINES                        
BRQ059     WRK-CFONE                   PIC  9(011).                     
                                                                        
BRQ059 01  WRK-CFONE-X.                                                 
BRQ059     05  WRK-CFONE-9             PIC  9(011)         VALUE ZEROS. 
                                                                        
       01  WRK-ENRO-LOGDR              PIC  9(005)         VALUE ZEROS. 
       01  WRK-ENRO-LOGDR-R REDEFINES  WRK-ENRO-LOGDR                   
                                       PIC  X(005).                     
                                                                        
       01  WRK-CHV-ENTSOCIO.                                            
           03  WRK-CPSSOA-CADTR-ENT    PIC  X(026)         VALUE SPACES.
                                                                        
       01  WRK-CHV-PENDFICA.                                            
           03  WRK-CPSSOA-CADTR-PEN    PIC  X(026)         VALUE SPACES.
                                                                        
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE        
           '*** AREA MENSAGEM DE ERRO FILE STATUS ***'.                 
      *----------------------------------------------------------------*
                                                                        
       01  WRK-ERRO-ARQUIVO.                                            
           03  FILLER                  PIC  X(008)         VALUE        
               '** ERRO '.                                              
           03  WRK-OPERACAO            PIC  X(013)         VALUE SPACES.
           03  FILLER                  PIC  X(012)         VALUE        
               ' DO ARQUIVO '.                                          
           03  WRK-NOME-ARQUIVO        PIC  X(008)         VALUE SPACES.
           03  FILLER                  PIC  X(017)         VALUE        
               ' - FILE STATUS = '.                                     
           03  WRK-FILE-STATUS         PIC  X(002)         VALUE SPACES.
           03  FILLER                  PIC  X(003)         VALUE ' **'. 
                                                                        
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE        
           '*** AREA DA POOL7600 ***'.                                  
      *----------------------------------------------------------------*
                                                                        
       01  WRK-DATA-HORA.                                               
           03  WRK-DATA-JULIANA        PIC  9(005) COMP-3  VALUE ZEROS. 
           03  WRK-DATA-AAMMDD         PIC  9(007) COMP-3  VALUE ZEROS. 
           03  WRK-DATA-AAAAMMDD       PIC  9(009) COMP-3  VALUE ZEROS. 
           03  WRK-HORA-HHMMSS         PIC  9(007) COMP-3  VALUE ZEROS. 
           03  WRK-HORA-HHMMSSMMMMMM   PIC  9(013) COMP-3  VALUE ZEROS. 
           03  WRK-TIMESTAMP.                                           
               05  WRK-ANO-SIS         PIC  9(004)         VALUE ZEROS. 
               05  WRK-MES-SIS         PIC  9(002)         VALUE ZEROS. 
               05  WRK-DIA-SIS         PIC  9(002)         VALUE ZEROS. 
               05  WRK-HOR-SIS         PIC  9(002)         VALUE ZEROS. 
               05  WRK-MIN-SIS         PIC  9(002)         VALUE ZEROS. 
               05  WRK-SEG-SIS         PIC  9(002)         VALUE ZEROS. 
               05  FILLER              PIC  X(006)         VALUE SPACES.
                                                                        
           EJECT                                                        
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE        
           '*** AREA PARA LAYOUT DO RELATORIO ***'.                     
      *----------------------------------------------------------------*
                                                                        
       01  CABEC1.                                                      
           03  FILLER                  PIC  X(001)         VALUE '1'.   
           03  FILLER                  PIC  X(018)         VALUE        
               'RDAB0033'.                                              
           03  FILLER                  PIC  X(013)         VALUE        
               'B A N C O'.                                             
           03  FILLER                  PIC  X(018)         VALUE        
               'B R A D E S C O'.                                       
           03  FILLER                  PIC  X(014)         VALUE        
               'S / A'.                                                 
           03  FILLER                  PIC  X(010)         VALUE        
               'PAG. UNICA'.                                            
                                                                        
       01  CABEC2.                                                      
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  CB2-DIA                 PIC  99.                         
           03  FILLER                  PIC  X(001)         VALUE '/'.   
           03  CB2-MES                 PIC  99.                         
           03  FILLER                  PIC  X(001)         VALUE '/'.   
           03  CB2-ANO                 PIC  9999.                       
           03  FILLER                  PIC  X(015)         VALUE        
               '   D A D O S'.                                          
           03  FILLER                  PIC  X(022)         VALUE        
               'C A D A S T R A I S'.                                   
           03  FILLER                  PIC  X(006)         VALUE 'D O'. 
           03  FILLER                  PIC  X(012)         VALUE        
               'S O C I O'.                                             
           03  CB2-HORA                PIC  99.                         
           03  FILLER                  PIC  X(001)         VALUE ':'.   
           03  CB2-MINUTO              PIC  99.                         
           03  FILLER                  PIC  X(001)         VALUE ':'.   
           03  CB2-SEGUNDO             PIC  99.                         
                                                                        
       01  CABEC3.                                                      
           03  FILLER                  PIC  X(001)         VALUE '0'.   
           03  FILLER                  PIC  X(013)         VALUE SPACES.
           03  FILLER                  PIC  X(009)         VALUE        
               'ARQUIVO'.                                               
           03  FILLER                  PIC  X(008)         VALUE        
               'ENTSOCIO'.                                              
                                                                        
       01  CABEC4.                                                      
           03  FILLER                  PIC  X(001)         VALUE '0'.   
           03  FILLER                  PIC  X(013)         VALUE SPACES.
           03  FILLER                  PIC  X(009)         VALUE        
               'ARQUIVO'.                                               
           03  FILLER                  PIC  X(008)         VALUE        
               'PENDFICA'.                                              
                                                                        
       01  CABEC5.                                                      
           03  FILLER                  PIC  X(001)         VALUE '0'.   
           03  FILLER                  PIC  X(013)         VALUE SPACES.
           03  FILLER                  PIC  X(009)         VALUE        
               'ARQUIVO'.                                               
           03  FILLER                  PIC  X(008)         VALUE        
               'SAISOCIO'.                                              
                                                                        
       01  LINTOT1.                                                     
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(022)         VALUE SPACES.
           03  FILLER                  PIC  X(021)         VALUE        
               'REGISTROS LIDOS'.                                       
           03  FILLER                  PIC  X(003)         VALUE ':  '. 
           03  LT1-LIDOS-ENTSOCIO      PIC  ZZZZ.ZZZ.ZZZ.ZZ9.           
                                                                        
       01  LINTOT2.                                                     
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(022)         VALUE SPACES.
           03  FILLER                  PIC  X(021)         VALUE        
               'REGISTROS LIDOS'.                                       
           03  FILLER                  PIC  X(003)         VALUE ':  '. 
           03  LT2-LIDOS-PENDFICA      PIC  ZZZZ.ZZZ.ZZZ.ZZ9.           
                                                                        
       01  LINTOT3.                                                     
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(022)         VALUE SPACES.
           03  FILLER                  PIC  X(024)         VALUE        
               'SEM CORRESP. ENTSOCIO:'.                                
           03  LT3-DESPR-PENDFICA      PIC  ZZZZ.ZZZ.ZZZ.ZZ9.           
                                                                        
       01  LINTOT4.                                                     
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(022)         VALUE SPACES.
           03  FILLER                  PIC  X(024)         VALUE        
               'COM CORRESP. ENTSOCIO:'.                                
           03  LT4-CORRE-PENDFICA      PIC  ZZZZ.ZZZ.ZZZ.ZZ9.           
                                                                        
       01  LINTOT5.                                                     
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(022)         VALUE SPACES.
           03  FILLER                  PIC  X(021)         VALUE        
               'REGISTROS GRAVADOS'.                                    
           03  FILLER                  PIC  X(003)         VALUE ':  '. 
           03  LT5-GRAVA-SAISOCIO      PIC  ZZZZ.ZZZ.ZZZ.ZZ9.           
                                                                        
           EJECT                                                        
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE        
           '*** AREA DA POOL7100 ***'.                                  
      *----------------------------------------------------------------*
                                                                        
-INC POL7100C                                                           
                                                                        
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE        
           '*** RDAB0033 - FIM DA AREA DE WORKING ***'.                 
      *----------------------------------------------------------------*
           EJECT                                                        
      *================================================================*
       PROCEDURE                       DIVISION.                        
      *================================================================*
                                                                        
      *----------------------------------------------------------------*
       000000-INICIAR                  SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           PERFORM 100000-INICIALIZAR.                                  
                                                                        
           PERFORM 200000-VERIFICAR-VAZIO.                              
                                                                        
           PERFORM 300000-PROCESSAR                                     
             UNTIL WRK-FS-ENTSOCIO     EQUAL '10'.                      
                                                                        
           PERFORM 400000-FINALIZAR.                                    
                                                                        
      *----------------------------------------------------------------*
       000000-99-FIM.                  EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       100000-INICIALIZAR              SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           OPEN INPUT  ENTSOCIO                                         
                       PENDFICA                                         
                OUTPUT SAISOCIO                                         
                       RELATO.                                          
                                                                        
           MOVE WRK-ABERTURA           TO WRK-OPERACAO.                 
                                                                        
           PERFORM 110000-TESTAR-FILE-STATUS.                           
                                                                        
           PERFORM 120000-OBTER-DATA-HORA-SISTEMA.                      
                                                                        
      *----------------------------------------------------------------*
       100000-99-FIM.                  EXIT.                            
      *----------------------------------------------------------------*
           EJECT                                                        
      *----------------------------------------------------------------*
       110000-TESTAR-FILE-STATUS       SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           PERFORM 111000-TESTAR-FS-ENTSOCIO.                           
                                                                        
           PERFORM 112000-TESTAR-FS-PENDFICA.                           
                                                                        
           PERFORM 113000-TESTAR-FS-SAISOCIO.                           
                                                                        
           PERFORM 114000-TESTAR-FS-RELATO.                             
                                                                        
      *----------------------------------------------------------------*
       110000-99-FIM.                  EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       111000-TESTAR-FS-ENTSOCIO       SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           IF  WRK-FS-ENTSOCIO         NOT EQUAL ZEROS                  
               MOVE  WRK-FS-ENTSOCIO   TO WRK-FILE-STATUS               
               MOVE 'ENTSOCIO'         TO WRK-NOME-ARQUIVO              
               MOVE  WRK-ERRO-ARQUIVO  TO ERR-TEXTO                     
               PERFORM 999999-PROCESSAR-ROTINA-ERRO                     
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       111000-99-FIM.                  EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       112000-TESTAR-FS-PENDFICA       SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           IF  WRK-FS-PENDFICA         NOT EQUAL ZEROS                  
               MOVE  WRK-FS-PENDFICA   TO WRK-FILE-STATUS               
               MOVE 'PENDFICA'         TO WRK-NOME-ARQUIVO              
               MOVE  WRK-ERRO-ARQUIVO  TO ERR-TEXTO                     
               PERFORM 999999-PROCESSAR-ROTINA-ERRO                     
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       112000-99-FIM.                  EXIT.                            
      *----------------------------------------------------------------*
           EJECT                                                        
      *----------------------------------------------------------------*
       113000-TESTAR-FS-SAISOCIO       SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           IF  WRK-FS-SAISOCIO         NOT EQUAL ZEROS                  
               MOVE  WRK-FS-SAISOCIO   TO WRK-FILE-STATUS               
               MOVE 'SAISOCIO'         TO WRK-NOME-ARQUIVO              
               MOVE  WRK-ERRO-ARQUIVO  TO ERR-TEXTO                     
               PERFORM 999999-PROCESSAR-ROTINA-ERRO                     
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       113000-99-FIM.                  EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       114000-TESTAR-FS-RELATO         SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           IF  WRK-FS-RELATO           NOT EQUAL ZEROS                  
               MOVE  WRK-FS-RELATO     TO WRK-FILE-STATUS               
               MOVE 'RELATO'           TO WRK-NOME-ARQUIVO              
               MOVE  WRK-ERRO-ARQUIVO  TO ERR-TEXTO                     
               PERFORM 999999-PROCESSAR-ROTINA-ERRO                     
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       114000-99-FIM.                  EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       120000-OBTER-DATA-HORA-SISTEMA  SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           CALL 'POOL7600'             USING WRK-DATA-HORA.             
                                                                        
           MOVE WRK-DIA-SIS            TO CB2-DIA.                      
           MOVE WRK-MES-SIS            TO CB2-MES.                      
           MOVE WRK-ANO-SIS            TO CB2-ANO.                      
           MOVE WRK-HOR-SIS            TO CB2-HORA.                     
           MOVE WRK-MIN-SIS            TO CB2-MINUTO.                   
           MOVE WRK-SEG-SIS            TO CB2-SEGUNDO.                  
                                                                        
      *----------------------------------------------------------------*
       120000-99-FIM.                  EXIT.                            
      *----------------------------------------------------------------*
           EJECT                                                        
      *----------------------------------------------------------------*
       200000-VERIFICAR-VAZIO          SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           PERFORM 210000-LER-PENDFICA.                                 
                                                                        
           IF  WRK-CHV-PENDFICA        EQUAL HIGH-VALUES                
               DISPLAY '****************** RDAB0033 ******************' 
               DISPLAY '*                                            *' 
               DISPLAY '*          ARQUIVO PENDFICA VAZIO            *' 
               DISPLAY '*                                            *' 
               DISPLAY '****************** RDAB0033 ******************' 
           END-IF.                                                      
                                                                        
           PERFORM 220000-LER-ENTSOCIO.                                 
                                                                        
           IF  WRK-CHV-ENTSOCIO        EQUAL HIGH-VALUES                
               DISPLAY '****************** RDAB0033 ******************' 
               DISPLAY '*                                            *' 
               DISPLAY '*          ARQUIVO ENTSOCIO VAZIO            *' 
               DISPLAY '*                                            *' 
               DISPLAY '*         PROCESSAMENTO  ENCERRADO           *' 
               DISPLAY '*                                            *' 
               DISPLAY '****************** RDAB0033 ******************' 
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       200000-99-FIM.                  EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       210000-LER-PENDFICA             SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           READ PENDFICA.                                               
                                                                        
           IF  WRK-FS-PENDFICA         EQUAL '10'                       
               MOVE HIGH-VALUES        TO WRK-CPSSOA-CADTR-PEN          
               GO TO 210000-99-FIM                                      
           END-IF.                                                      
                                                                        
           MOVE WRK-LEITURA            TO WRK-OPERACAO.                 
                                                                        
           PERFORM 112000-TESTAR-FS-PENDFICA.                           
                                                                        
           MOVE V001-E-CPSSOA-CADTR    TO WRK-CPSSOA-CADTR-PEN.         
                                                                        
           ADD  1                      TO ACU-LIDOS-PENDFICA.           
                                                                        
      *----------------------------------------------------------------*
       210000-99-FIM.                  EXIT.                            
      *----------------------------------------------------------------*
           EJECT                                                        
      *----------------------------------------------------------------*
       220000-LER-ENTSOCIO             SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           READ ENTSOCIO.                                               
                                                                        
           IF  WRK-FS-ENTSOCIO         EQUAL '10'                       
               MOVE HIGH-VALUES        TO WRK-CPSSOA-CADTR-ENT          
               GO TO 220000-99-FIM                                      
           END-IF.                                                      
                                                                        
           MOVE WRK-LEITURA            TO WRK-OPERACAO.                 
                                                                        
           PERFORM 111000-TESTAR-FS-ENTSOCIO.                           
                                                                        
           MOVE SOC-CPSSOA-CADTR       OF ENTSOCIO                      
                                       TO WRK-CPSSOA-CADTR-ENT.         
                                                                        
           ADD  1                      TO ACU-LIDOS-ENTSOCIO.           
                                                                        
      *----------------------------------------------------------------*
       220000-99-FIM.                  EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       300000-PROCESSAR                SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           IF  SOC-CGCFIL OF ENTSOCIO  NOT EQUAL ZEROS OR               
               WRK-CPSSOA-CADTR-ENT    EQUAL SPACES                     
               MOVE SOC-REGISTRO       OF ENTSOCIO                      
                                       TO SOC-REGISTRO OF SAISOCIO      
               PERFORM 310000-GRAVAR-SAISOCIO                           
           ELSE                                                         
               IF WRK-CPSSOA-CADTR-ENT  EQUAL WRK-CPSSOA-CADTR-PEN      
                  ADD 1               TO ACU-CORRE-PENDFICA             
                  MOVE SOC-REGISTRO   OF ENTSOCIO                       
                                      TO SOC-REGISTRO OF SAISOCIO       
                  PERFORM 320000-TRATAR-SOCIO                           
               ELSE                                                     
                  IF WRK-CPSSOA-CADTR-ENT GREATER WRK-CPSSOA-CADTR-PEN  
                     PERFORM 210000-LER-PENDFICA    UNTIL               
                          WRK-CPSSOA-CADTR-PEN                          
                                     NOT LESS  WRK-CPSSOA-CADTR-ENT OR  
                          WRK-FS-PENDFICA     EQUAL '10'                
                     IF  WRK-CPSSOA-CADTR-ENT EQUAL WRK-CPSSOA-CADTR-PEN
                         ADD 1               TO ACU-CORRE-PENDFICA      
                         MOVE SOC-REGISTRO   OF ENTSOCIO                
                                             TO SOC-REGISTRO OF SAISOCIO
                         PERFORM 320000-TRATAR-SOCIO                    
                     ELSE                                               
                         MOVE SOC-REGISTRO OF ENTSOCIO                  
                                       TO SOC-REGISTRO OF SAISOCIO      
                         PERFORM 310000-GRAVAR-SAISOCIO.                
                                                                        
           PERFORM 220000-LER-ENTSOCIO.                                 
                                                                        
      *----------------------------------------------------------------*
       300000-99-FIM.                  EXIT.                            
      *----------------------------------------------------------------*
           EJECT                                                        
      *----------------------------------------------------------------*
       310000-GRAVAR-SAISOCIO          SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           WRITE SOC-REGISTRO           OF SAISOCIO.                    
                                                                        
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.                 
                                                                        
           PERFORM 113000-TESTAR-FS-SAISOCIO.                           
                                                                        
           ADD 1                       TO ACU-GRAVA-SAISOCIO.           
                                                                        
      *----------------------------------------------------------------*
       310000-99-FIM.                  EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       320000-TRATAR-SOCIO             SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE SPACES                 TO SOC-EBAIRO       OF SAISOCIO  
                                          SOC-CLETRA-RTING OF SAISOCIO. 
                                                                        
           IF   V001-E-COCUPC-PF-NN   NOT  EQUAL '?'                    
                MOVE V001-E-COCUPC-PF   TO SOC-RAMO-ATVDD   OF SAISOCIO 
           ELSE                                                         
                MOVE  ZEROS             TO SOC-RAMO-ATVDD   OF SAISOCIO.
           MOVE V001-E-ELOGDR          TO SOC-ELOGDR       OF SAISOCIO. 
           MOVE V001-E-ENRO-LOGDR      TO WRK-ENRO-LOGDR.               
           MOVE WRK-ENRO-LOGDR-R       TO SOC-ENRO-LOGDR   OF SAISOCIO. 
           MOVE V001-E-ECOMPL-LOGDR    TO SOC-ECOMPL-LOGDR OF SAISOCIO. 
           MOVE V001-E-CCEP-RESID      TO SOC-CCEP         OF SAISOCIO. 
           IF   V001-E-CCEP-COMPL-RESID-NN  NOT  EQUAL '?'              
                MOVE V001-E-CCEP-COMPL-RESID                            
                                        TO SOC-CCEP-COMPL   OF SAISOCIO 
           ELSE                                                         
                MOVE  ZEROS             TO SOC-CCEP-COMPL   OF SAISOCIO.
           MOVE V001-E-IMUN-RESID      TO SOC-RMUN         OF SAISOCIO. 
           IF   V001-E-CSGL-UF-RESID-NN  NOT  EQUAL '?'                 
                MOVE V001-E-CSGL-UF-RESID  TO SOC-CSGL-UF   OF SAISOCIO 
           ELSE                                                         
                MOVE  SPACES               TO SOC-CSGL-UF   OF SAISOCIO.
           MOVE V001-E-CDDD-RESID      TO SOC-CDDD         OF SAISOCIO. 
                                                                        
BRQ059     IF   V001-E-CFONE-RESID-NOVO NOT NUMERIC                     
BRQ059          MOVE ZEROS              TO SOC-CFONE OF SAISOCIO        
BRQ059     ELSE                                                         
BRQ059          MOVE V001-E-CFONE-RESID-NOVO                            
BRQ059                                  TO WRK-CFONE                    
BRQ059          MOVE WRK-CFONE-R        TO WRK-CFONE-9                  
BRQ059          MOVE WRK-CFONE-X        TO SOC-CFONE OF SAISOCIO        
BRQ059     END-IF.                                                      
                                                                        
           PERFORM 310000-GRAVAR-SAISOCIO.                              
                                                                        
      *----------------------------------------------------------------*
       320000-99-FIM.                  EXIT.                            
      *----------------------------------------------------------------*
           EJECT                                                        
      *----------------------------------------------------------------*
       400000-FINALIZAR                SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           PERFORM 410000-EMITIR-RELATO.                                
                                                                        
           CLOSE ENTSOCIO                                               
                 PENDFICA                                               
                 SAISOCIO                                               
                 RELATO.                                                
                                                                        
           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.                 
                                                                        
           PERFORM 110000-TESTAR-FILE-STATUS.                           
                                                                        
           GOBACK.                                                      
                                                                        
      *----------------------------------------------------------------*
       400000-99-FIM.                  EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       410000-EMITIR-RELATO            SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE ACU-LIDOS-ENTSOCIO     TO LT1-LIDOS-ENTSOCIO.           
           MOVE ACU-LIDOS-PENDFICA     TO LT2-LIDOS-PENDFICA.           
           MOVE ACU-DESPR-PENDFICA     TO LT3-DESPR-PENDFICA.           
           MOVE ACU-CORRE-PENDFICA     TO LT4-CORRE-PENDFICA.           
           MOVE ACU-GRAVA-SAISOCIO     TO LT5-GRAVA-SAISOCIO.           
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.                 
                                                                        
           WRITE REG-RELATO            FROM CABEC1.                     
           PERFORM 114000-TESTAR-FS-RELATO.                             
                                                                        
           WRITE REG-RELATO            FROM CABEC2.                     
           PERFORM 114000-TESTAR-FS-RELATO.                             
                                                                        
           WRITE REG-RELATO            FROM CABEC3.                     
           PERFORM 114000-TESTAR-FS-RELATO.                             
                                                                        
           WRITE REG-RELATO            FROM LINTOT1.                    
           PERFORM 114000-TESTAR-FS-RELATO.                             
                                                                        
           WRITE REG-RELATO            FROM CABEC4.                     
           PERFORM 114000-TESTAR-FS-RELATO.                             
                                                                        
           WRITE REG-RELATO            FROM LINTOT2.                    
           PERFORM 114000-TESTAR-FS-RELATO.                             
                                                                        
           WRITE REG-RELATO            FROM LINTOT4.                    
           PERFORM 114000-TESTAR-FS-RELATO.                             
                                                                        
           WRITE REG-RELATO            FROM CABEC5.                     
           PERFORM 114000-TESTAR-FS-RELATO.                             
                                                                        
           WRITE REG-RELATO            FROM LINTOT5.                    
           PERFORM 114000-TESTAR-FS-RELATO.                             
                                                                        
      *----------------------------------------------------------------*
       410000-99-FIM.                  EXIT.                            
      *----------------------------------------------------------------*
           EJECT                                                        
      *----------------------------------------------------------------*
       999999-PROCESSAR-ROTINA-ERRO    SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE 'APL'                  TO ERR-TIPO-ACESSO.              
           MOVE 'RDAB0033'             TO ERR-PGM.                      
                                                                        
           CALL 'POOL7100'             USING WRK-BATCH                  
                                             ERRO-AREA.                 
                                                                        
           GOBACK.                                                      
                                                                        
      *----------------------------------------------------------------*
       999999-99-FIM.                  EXIT.                            
      *----------------------------------------------------------------*

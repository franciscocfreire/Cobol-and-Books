      *===============================================================* 
       IDENTIFICATION                  DIVISION.                        
      *===============================================================* 
                                                                        
       PROGRAM-ID. RDAB0032.                                            
       AUTHOR.     JOSSIANE CARLA GAZZONI.                              
      *===============================================================* 
      *                    C P M  -  S I S T E M A S                  * 
      *---------------------------------------------------------------* 
      *    PROGRAMA....:   RDAB0032                                   * 
      *    PROGRAMADORA:   JOSSIANE CARLA GAZZONI - CPM PATO BRANCO   * 
      *    ANALISTA CPM:   SAULE VILSON FEVERSANI - CPM PATO BRANCO   * 
      *    ANALISTA....:   CARMEN                 - GRUPO 82          * 
      *    DATA........:   19/03/2004                                 * 
      *---------------------------------------------------------------* 
      *    OBJETIVO....:   OBTER CPSSOA-CADTR, CPF/CNPJ E NOME SOCIO. * 
      *---------------------------------------------------------------* 
      *    ARQUIVOS....:                                              * 
      *                DDNAME                      INCLUDE/BOOK       * 
      *                    ARQPARVV                  I#RDAB01         * 
      *                    ARQUIRES                  I#IRES11         * 
      *                    PENDFICA                  I#FICAC6         * 
      *                    ARQSOCIO                  I#RDAB17         * 
      *                    RELATO                        -            * 
      *---------------------------------------------------------------* 
      *    INC'S.......:                                              * 
      *    POL7100C - AREA PARA TRATAMENTO DE ERROS PELA POOL7100.    * 
      *---------------------------------------------------------------* 
      *    MODULOS.....:                                              * 
      *    POOL7100 - TRATAMENTO DE ERROS QUANDO PROGRAMA INVALIDO.   * 
      *    POOL7600 - OBTER DATA E HORA DO PROCESSAMENTO.             * 
      *===============================================================* 
      *                                                               * 
BRQ059* ============================================================= * 
      * |                      ALTERACAO                            | * 
      * ------------------------------------------------------------+ * 
      *  ANALISTA     : ROBSON VELASQUES / BRQ - IT SERVICES        | * 
      *  PROGRAMADOR  : CRISTIANO SOUZA  / BRQ - IT SERVICES        | * 
      *  DATA         : 02/2012                                     | * 
      *  OBJETIVO     : INSERIR UM DIGITO A MAIS AOS CAMPOS DE      | * 
      *                 TELEFONES                                   | * 
      *  PROJETO      : 2012/0059 - ADEQUAR NRO TELEFONE(RES.553)   | * 
      * ============================================================+ * 
BRQ059***************************************************************** 
           EJECT                                                        
      *===============================================================* 
       ENVIRONMENT                     DIVISION.                        
      *===============================================================* 
                                                                        
      *---------------------------------------------------------------* 
       CONFIGURATION                   SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
       SPECIAL-NAMES.                                                   
           DECIMAL-POINT               IS COMMA.                        
                                                                        
      *---------------------------------------------------------------* 
       INPUT-OUTPUT                    SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
       FILE-CONTROL.                                                    
                                                                        
           SELECT ARQPARVV ASSIGN      TO UT-S-ARQPARVV                 
           FILE STATUS                 IS WRK-FS-ARQPARVV.              
                                                                        
           SELECT ARQUIRES ASSIGN      TO UT-S-ARQUIRES                 
           FILE STATUS                 IS WRK-FS-ARQUIRES.              
                                                                        
           SELECT PENDFICA ASSIGN      TO UT-S-PENDFICA                 
           FILE STATUS                 IS WRK-FS-PENDFICA.              
                                                                        
           SELECT ARQSOCIO ASSIGN      TO UT-S-ARQSOCIO                 
           FILE STATUS                 IS WRK-FS-ARQSOCIO.              
                                                                        
           SELECT RELATO   ASSIGN      TO UT-S-RELATO                   
           FILE STATUS                 IS WRK-FS-RELATO.                
                                                                        
           EJECT                                                        
      *===============================================================* 
       DATA                            DIVISION.                        
      *===============================================================* 
                                                                        
      *---------------------------------------------------------------* 
       FILE                            SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
      *    INPUT:  ARQUIVO IMAGEM DA TABELA DE PARAMETROS.            * 
      *            ORG. SEQUENCIAL     -   LRECL   =   150            * 
      *---------------------------------------------------------------* 
                                                                        
       FD  ARQPARVV                                                     
           RECORDING MODE IS F                                          
           LABEL RECORD   IS STANDARD                                   
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       COPY 'I#RDAB01'.

      *---------------------------------------------------------------* 
      *    INPUT:  ARQUIVO DO IRES.                                   * 
      *            ORG. SEQUENCIAL     -   LRECL   =   169            * 
      *---------------------------------------------------------------* 
                                                                        
       FD  ARQUIRES                                                     
           RECORDING MODE IS F                                          
           LABEL RECORD   IS STANDARD                                   
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       COPY 'I#IRES11'.

      *---------------------------------------------------------------* 
      *     INPUT: ARQUIVO IMAGEM DA TABELA FICAV000.                 * 
      *            ORG. SEQUENCIAL     -   LRECL   =   339            * 
      *---------------------------------------------------------------* 
                                                                        
       FD  PENDFICA                                                     
           RECORDING MODE IS F                                          
           LABEL RECORD   IS STANDARD                                   
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       COPY 'I#FICAC6'.
                                                                        
           EJECT                                                        
      *---------------------------------------------------------------* 
      *    OUTPUT: ARQUIVO DOS SOCIOS.                                * 
      *            ORG. SEQUENCIAL     -   LRECL   =   223            * 
      *---------------------------------------------------------------* 
                                                                        
       FD  ARQSOCIO                                                     
           RECORDING MODE IS F                                          
           LABEL RECORD   IS STANDARD                                   
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       COPY 'I#RDAB17'.
                                                                        
      *---------------------------------------------------------------* 
      *    OUTPUT: RELATORIO QUANTIDADE DE REGISTROS TRATADOS.        * 
      *            ORG. SEQUENCIAL     -   LRECL   =   073            * 
      *---------------------------------------------------------------* 
                                                                        
       FD  RELATO                                                       
           RECORDING MODE IS F                                          
           LABEL RECORD   IS STANDARD                                   
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  REG-RELATO                  PIC  X(073).                     
                                                                        
      *---------------------------------------------------------------* 
       WORKING-STORAGE                 SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       77  FILLER                      PIC  X(050)         VALUE        
           '*** RDAB0032 - INICIO DA AREA DE WORKING ***'.              
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       77  FILLER                      PIC  X(050)         VALUE        
           '*** AREA PARA ACUMULADORES ***'.                            
      *---------------------------------------------------------------* 
                                                                        
       77  ACU-LIDO-ARQPARVV           PIC 9(013)  COMP-3  VALUE ZEROS. 
       77  ACU-DESP1-ARQPARVV          PIC 9(013)  COMP-3  VALUE ZEROS. 
       77  ACU-DESP2-ARQPARVV          PIC 9(013)  COMP-3  VALUE ZEROS. 
       77  ACU-LIDO-PENDFICA           PIC 9(013)  COMP-3  VALUE ZEROS. 
       77  ACU-DESP-PENDFICA           PIC 9(013)  COMP-3  VALUE ZEROS. 
       77  ACU-LIDO-ARQUIRES           PIC 9(013)  COMP-3  VALUE ZEROS. 
       77  ACU-DESP-ARQUIRES           PIC 9(013)  COMP-3  VALUE ZEROS. 
       77  ACU-GRAV-ARQSOCIO           PIC 9(013)  COMP-3  VALUE ZEROS. 
       77  ACU-GRAV-ATUALIZADOS        PIC 9(013)  COMP-3  VALUE ZEROS. 
       77  ACU-GRAV-SEM-PENDFICA       PIC 9(013)  COMP-3  VALUE ZEROS. 
       77  ACU-GRAV-FILIAL-ZEROS       PIC 9(013)  COMP-3  VALUE ZEROS. 
                                                                        
           EJECT                                                        
      *---------------------------------------------------------------* 
       77  FILLER                      PIC  X(050)         VALUE        
           '*** AREA PARA TESTE DE FILE STATUS ***'.                    
      *---------------------------------------------------------------* 
                                                                        
       77  WRK-FS-ARQPARVV             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-ARQUIRES             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-PENDFICA             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-ARQSOCIO             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-RELATO               PIC  X(002)         VALUE SPACES.
                                                                        
       77  WRK-ABERTURA                PIC  X(013)         VALUE        
           ' NA ABERTURA '.                                             
       77  WRK-LEITURA                 PIC  X(013)         VALUE        
           ' NA  LEITURA '.                                             
       77  WRK-GRAVACAO                PIC  X(013)         VALUE        
           ' NA GRAVACAO '.                                             
       77  WRK-FECHAMENTO              PIC  X(013)         VALUE        
           'NO FECHAMENTO'.                                             
                                                                        
      *---------------------------------------------------------------* 
       77  FILLER                      PIC  X(050)         VALUE        
           '*** AREA PARA VARIAVEIS AUXILIARES ***'.                    
      *---------------------------------------------------------------* 
                                                                        
       77  WRK-BATCH                   PIC  X(008)         VALUE        
           'BATCH'.                                                     
                                                                        
BRQ059 01  WRK-E-CFONE                 PIC S9(011)         VALUE ZEROS. 
BRQ059 01  WRK-E-CFONE-R               REDEFINES                        
BRQ059     WRK-E-CFONE                 PIC  9(011).                     
                                                                        
BRQ059 01  WRK-E-CFONE-X.                                               
BRQ059     05  WRK-E-CFONE-9           PIC  9(011)         VALUE ZEROS. 
                                                                        
       01  WRK-E-ENRO-LOGDR            PIC  9(005)         VALUE ZEROS. 
       01  WRK-E-ENRO-LOGDR-R          REDEFINES                        
           WRK-E-ENRO-LOGDR            PIC  X(005).                     
                                                                        
      *---------------------------------------------------------------* 
       01  FILLER                      PIC  X(050)         VALUE        
           '*** AREA PARA CHAVES ***'.                                  
      *---------------------------------------------------------------* 
                                                                        
       01  WRK-CHV-ARQPARVV            PIC  X(026)         VALUE SPACES.
       01  WRK-CHV-ARQUIRES            PIC  X(026)         VALUE SPACES.
       01  WRK-CHV-PENDFICA            PIC  X(026)         VALUE SPACES.
                                                                        
      *---------------------------------------------------------------* 
       01  FILLER                      PIC  X(050)         VALUE        
           '*** MENSAGEM DE ERRO DE FILE STATUS ***'.                   
      *---------------------------------------------------------------* 
                                                                        
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
                                                                        
           EJECT                                                        
      *---------------------------------------------------------------* 
       01  FILLER                      PIC  X(050)         VALUE        
           '*** AREA DE CABECALHOS RELATO ***'.                         
      *---------------------------------------------------------------* 
                                                                        
       01  CABEC1.                                                      
           03  FILLER                  PIC  X(001)         VALUE '1'.   
           03  FILLER                  PIC  X(072)         VALUE ALL    
               '-'.                                                     
                                                                        
       01  CABEC2.                                                      
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(019)         VALUE        
               ' RDAB0032'.                                             
           03  FILLER                  PIC  X(013)         VALUE        
               'B A N C O'.                                             
           03  FILLER                  PIC  X(033)         VALUE        
               'B R A D E S C O...S / A'.                               
           03  FILLER                  PIC  X(006)         VALUE 'PAG.'.
           03  FILLER                  PIC  X(001)         VALUE '1'.   
                                                                        
       01  CABEC3.                                                      
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  CB3-DIA                 PIC  99.                         
           03  FILLER                  PIC  X(001)         VALUE '/'.   
           03  CB3-MES                 PIC  99.                         
           03  FILLER                  PIC  X(001)         VALUE '/'.   
           03  CB3-ANO                 PIC  9999.                       
           03  FILLER                  PIC  X(012)         VALUE SPACES.
           03  FILLER                  PIC  X(012)         VALUE        
               'D A D O S'.                                             
           03  FILLER                  PIC  X(006)         VALUE 'D O'. 
           03  FILLER                  PIC  X(023)         VALUE        
               'S O C I O'.                                             
           03  CB3-HOR                 PIC  99.                         
           03  FILLER                  PIC  X(001)         VALUE ':'.   
           03  CB3-MIN                 PIC  99.                         
           03  FILLER                  PIC  X(001)         VALUE ':'.   
           03  CB3-SEG                 PIC  99.                         
                                                                        
      *---------------------------------------------------------------* 
       01  FILLER                      PIC  X(050)         VALUE        
           '*** AREA DE TOTAIS RELATO ***'.                             
      *---------------------------------------------------------------* 
                                                                        
       01  LINTOT1.                                                     
           03  FILLER                  PIC  X(001)         VALUE '0'.   
           03  FILLER                  PIC  X(014)         VALUE SPACES.
           03  FILLER                  PIC  X(009)         VALUE        
               'ARQUIVO'.                                               
           03  FILLER                  PIC  X(008)         VALUE        
               'ARQPARVV'.                                              
                                                                        
       01  LINTOT2.                                                     
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(023)         VALUE SPACES.
           03  FILLER                  PIC  X(021)         VALUE        
               'REGISTROS LIDOS'.                                       
           03  FILLER                  PIC  X(003)         VALUE ':  '. 
           03  LT2-LID-ARQPARVV        PIC  ZZZZ.ZZZ.ZZZ.ZZ9.           
                                                                        
       01  LINTOT3.                                                     
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(023)         VALUE SPACES.
           03  FILLER                  PIC  X(024)         VALUE        
               'GRAVADOS ATUALIZADOS :'.                                
           03  LT3-GRAV-ATUALIZADOS    PIC  ZZZZ.ZZZ.ZZZ.ZZ9.           
                                                                        
       01  LINTOT4.                                                     
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(023)         VALUE SPACES.
           03  FILLER                  PIC  X(024)         VALUE        
               'GRAVADOS FILIAL ZEROS:'.                                
           03  LT4-GRAV-FILIAL-ZEROS   PIC  ZZZZ.ZZZ.ZZZ.ZZ9.           
                                                                        
       01  LINTOT5.                                                     
           03  FILLER                  PIC  X(001)         VALUE '0'.   
           03  FILLER                  PIC  X(014)         VALUE SPACES.
           03  FILLER                  PIC  X(009)         VALUE        
               'ARQUIVO'.                                               
           03  FILLER                  PIC  X(008)         VALUE        
               'PENDFICA'.                                              
                                                                        
       01  LINTOT6.                                                     
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(023)         VALUE SPACES.
           03  FILLER                  PIC  X(021)         VALUE        
               'REGISTROS LIDOS'.                                       
           03  FILLER                  PIC  X(003)         VALUE ':  '. 
           03  LT6-LID-PENDFICA        PIC  ZZZZ.ZZZ.ZZZ.ZZ9.           
                                                                        
       01  LINTOT7.                                                     
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(023)         VALUE SPACES.
           03  FILLER                  PIC  X(024)         VALUE        
               'GRAVADOS SEM PENDFICA:'.                                
           03  LT7-GRAV-SEM-PENDFICA   PIC  ZZZZ.ZZZ.ZZZ.ZZ9.           
                                                                        
       01  LINTOT8.                                                     
           03  FILLER                  PIC  X(001)         VALUE '0'.   
           03  FILLER                  PIC  X(014)         VALUE SPACES.
           03  FILLER                  PIC  X(009)         VALUE        
               'ARQUIVO'.                                               
           03  FILLER                  PIC  X(008)         VALUE        
               'ARQUIRES'.                                              
                                                                        
       01  LINTOT9.                                                     
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(023)         VALUE SPACES.
           03  FILLER                  PIC  X(021)         VALUE        
               'REGISTROS LIDOS'.                                       
           03  FILLER                  PIC  X(003)         VALUE ':  '. 
           03  LT9-LID-ARQUIRES        PIC  ZZZZ.ZZZ.ZZZ.ZZ9.           
                                                                        
       01  LINTOT10.                                                    
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(023)         VALUE SPACES.
           03  FILLER                  PIC  X(024)         VALUE        
               'SEM CORRESP. ARQPARVV:'.                                
           03  LT10-DESP-ARQUIRES      PIC  ZZZZ.ZZZ.ZZZ.ZZ9.           
                                                                        
       01  LINTOT11.                                                    
           03  FILLER                  PIC  X(001)         VALUE '0'.   
           03  FILLER                  PIC  X(014)         VALUE SPACES.
           03  FILLER                  PIC  X(009)         VALUE        
               'ARQUIVO'.                                               
           03  FILLER                  PIC  X(008)         VALUE        
               'ARQSOCIO'.                                              
                                                                        
       01  LINTOT12.                                                    
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(023)         VALUE SPACES.
           03  FILLER                  PIC  X(021)         VALUE        
               'REGISTROS GRAVADOS'.                                    
           03  FILLER                  PIC  X(003)         VALUE ':  '. 
           03  LT12-GRAV-ARQSOCIO      PIC  ZZZZ.ZZZ.ZZZ.ZZ9.           
                                                                        
           EJECT                                                        
      *---------------------------------------------------------------* 
       01  FILLER                      PIC  X(050)         VALUE        
           '*** AREA DA POOL7600 ***'.                                  
      *---------------------------------------------------------------* 
                                                                        
       01  WRK-DATA-HORA.                                               
           03  WRK-DT-JULIANA          PIC  9(005) COMP-3  VALUE ZEROS. 
           03  WRK-DT-AAMMDD           PIC  9(007) COMP-3  VALUE ZEROS. 
           03  WRK-DT-AAAAMMDD         PIC  9(009) COMP-3  VALUE ZEROS. 
           03  WRK-TI-HHMMSS           PIC  9(007) COMP-3  VALUE ZEROS. 
           03  WRK-TI-HHMMSSMMMMMM     PIC  9(013) COMP-3  VALUE ZEROS. 
           03  WRK-TIMESTAMP.                                           
               05  WRK-P7600-ANO       PIC  9(004)         VALUE ZEROS. 
               05  WRK-P7600-MES       PIC  9(002)         VALUE ZEROS. 
               05  WRK-P7600-DIA       PIC  9(002)         VALUE ZEROS. 
               05  WRK-P7600-HOR       PIC  9(002)         VALUE ZEROS. 
               05  WRK-P7600-MIN       PIC  9(002)         VALUE ZEROS. 
               05  WRK-P7600-SEG       PIC  9(002)         VALUE ZEROS. 
               05  FILLER              PIC  X(006)         VALUE SPACES.
                                                                        
           EJECT                                                        
      *---------------------------------------------------------------* 
       01  FILLER                      PIC  X(050)         VALUE        
           '*** AREA PARA POOL7100 ***'.                                
      *---------------------------------------------------------------* 
                                                                        
-INC POL7100C                                                           
                                                                        
      *---------------------------------------------------------------* 
       01  FILLER                      PIC  X(050)         VALUE        
           '*** RDAB0032 - FIM DA AREA DE WORKING ***'.                 
      *---------------------------------------------------------------* 
           EJECT                                                        
      *===============================================================* 
       PROCEDURE                       DIVISION.                        
      *===============================================================* 
                                                                        
      *---------------------------------------------------------------* 
       000000-INICIAR                  SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           PERFORM 100000-INICIALIZAR.                                  
                                                                        
           PERFORM 200000-VERIFICAR-VAZIO.                              
                                                                        
           PERFORM 300000-PROCESSAR    UNTIL                            
                   WRK-CHV-ARQPARVV    EQUAL HIGH-VALUES.               
                                                                        
           PERFORM 400000-FINALIZAR.                                    
                                                                        
      *---------------------------------------------------------------* 
       000000-99-FIM.                  EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       100000-INICIALIZAR              SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           OPEN INPUT  ARQPARVV                                         
                       ARQUIRES                                         
                       PENDFICA                                         
                OUTPUT ARQSOCIO                                         
                       RELATO.                                          
                                                                        
           MOVE WRK-ABERTURA           TO WRK-OPERACAO.                 
                                                                        
           PERFORM 110000-TESTAR-FILE-STATUS.                           
                                                                        
           PERFORM 120000-OBTER-DATA-HORA.                              
                                                                        
      *---------------------------------------------------------------* 
       100000-99-FIM.                  EXIT.                            
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       110000-TESTAR-FILE-STATUS       SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           PERFORM 111000-TESTAR-FS-ARQPARVV.                           
           PERFORM 112000-TESTAR-FS-PENDFICA.                           
           PERFORM 113000-TESTAR-FS-ARQUIRES.                           
           PERFORM 114000-TESTAR-FS-ARQSOCIO.                           
           PERFORM 115000-TESTAR-FS-RELATO.                             
                                                                        
      *---------------------------------------------------------------* 
       110000-99-FIM.                  EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       111000-TESTAR-FS-ARQPARVV       SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           IF  WRK-FS-ARQPARVV         NOT EQUAL '00'                   
               MOVE 'ARQPARVV'         TO WRK-NOME-ARQUIVO              
               MOVE WRK-FS-ARQPARVV    TO WRK-FILE-STATUS               
               MOVE WRK-ERRO-ARQUIVO   TO ERR-TEXTO                     
               PERFORM 999999-PROCESSAR-ROTINA-ERRO                     
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       111000-99-FIM.                  EXIT.                            
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       112000-TESTAR-FS-PENDFICA       SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           IF  WRK-FS-PENDFICA         NOT EQUAL '00'                   
               MOVE 'PENDFICA'         TO WRK-NOME-ARQUIVO              
               MOVE WRK-FS-PENDFICA    TO WRK-FILE-STATUS               
               MOVE WRK-ERRO-ARQUIVO   TO ERR-TEXTO                     
               PERFORM 999999-PROCESSAR-ROTINA-ERRO                     
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       112000-99-FIM.                  EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       113000-TESTAR-FS-ARQUIRES       SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           IF  WRK-FS-ARQUIRES         NOT EQUAL '00'                   
               MOVE 'ARQUIRES'         TO WRK-NOME-ARQUIVO              
               MOVE WRK-FS-ARQUIRES    TO WRK-FILE-STATUS               
               MOVE WRK-ERRO-ARQUIVO   TO ERR-TEXTO                     
               PERFORM 999999-PROCESSAR-ROTINA-ERRO                     
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       113000-99-FIM.                  EXIT.                            
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       114000-TESTAR-FS-ARQSOCIO       SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           IF  WRK-FS-ARQSOCIO         NOT EQUAL '00'                   
               MOVE 'ARQSOCIO'         TO WRK-NOME-ARQUIVO              
               MOVE WRK-FS-ARQSOCIO    TO WRK-FILE-STATUS               
               MOVE WRK-ERRO-ARQUIVO   TO ERR-TEXTO                     
               PERFORM 999999-PROCESSAR-ROTINA-ERRO                     
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       114000-99-FIM.                  EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       115000-TESTAR-FS-RELATO         SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           IF  WRK-FS-RELATO           NOT EQUAL '00'                   
               MOVE 'RELATO'           TO WRK-NOME-ARQUIVO              
               MOVE WRK-FS-RELATO      TO WRK-FILE-STATUS               
               MOVE WRK-ERRO-ARQUIVO   TO ERR-TEXTO                     
               PERFORM 999999-PROCESSAR-ROTINA-ERRO                     
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       115000-99-FIM.                  EXIT.                            
      *---------------------------------------------------------------* 
           EJECT                                                        
      *----------------------------------------------------------------*
       120000-OBTER-DATA-HORA          SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           CALL 'POOL7600'             USING WRK-DATA-HORA.             
                                                                        
           MOVE WRK-P7600-DIA          TO CB3-DIA.                      
           MOVE WRK-P7600-MES          TO CB3-MES.                      
           MOVE WRK-P7600-ANO          TO CB3-ANO.                      
           MOVE WRK-P7600-HOR          TO CB3-HOR.                      
           MOVE WRK-P7600-MIN          TO CB3-MIN.                      
           MOVE WRK-P7600-SEG          TO CB3-SEG.                      
                                                                        
      *----------------------------------------------------------------*
       120000-99-FIM.                  EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *---------------------------------------------------------------* 
       200000-VERIFICAR-VAZIO          SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           PERFORM 210000-LER-ARQPARVV.                                 
                                                                        
           IF  WRK-CHV-ARQPARVV        EQUAL HIGH-VALUES                
               DISPLAY '***************** RDAB0032 *****************'   
               DISPLAY '*                                          *'   
               DISPLAY '*          ARQUIVO ARQPARVV VAZIO          *'   
               DISPLAY '*                                          *'   
               DISPLAY '*         PROCESSAMENTO  ENCERRADO         *'   
               DISPLAY '*                                          *'   
               DISPLAY '***************** RDAB0032 *****************'   
           END-IF.                                                      
                                                                        
           PERFORM 220000-LER-PENDFICA.                                 
                                                                        
           IF  WRK-CHV-PENDFICA        EQUAL HIGH-VALUES                
               DISPLAY '***************** RDAB0032 *****************'   
               DISPLAY '*                                          *'   
               DISPLAY '*          ARQUIVO PENDFICA VAZIO          *'   
               DISPLAY '*                                          *'   
               DISPLAY '*         PROCESSAMENTO  ENCERRADO         *'   
               DISPLAY '*                                          *'   
               DISPLAY '***************** RDAB0032 *****************'   
           END-IF.                                                      
                                                                        
           PERFORM 230000-LER-ARQUIRES.                                 
                                                                        
           IF  WRK-CHV-ARQUIRES        EQUAL HIGH-VALUES                
               DISPLAY '***************** RDAB0032 *****************'   
               DISPLAY '*                                          *'   
               DISPLAY '*          ARQUIVO ARQUIRES VAZIO          *'   
               DISPLAY '*                                          *'   
               DISPLAY '*         PROCESSAMENTO  ENCERRADO         *'   
               DISPLAY '*                                          *'   
               DISPLAY '***************** RDAB0032 *****************'   
           END-IF.                                                      
                                                                        
           IF  WRK-CHV-PENDFICA        EQUAL HIGH-VALUES OR             
               WRK-CHV-ARQUIRES        EQUAL HIGH-VALUES                
               PERFORM 400000-FINALIZAR                                 
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       200000-99-FIM.                  EXIT.                            
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       210000-LER-ARQPARVV             SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           READ ARQPARVV.                                               
                                                                        
           IF  WRK-FS-ARQPARVV         EQUAL '10'                       
               MOVE HIGH-VALUES        TO WRK-CHV-ARQPARVV              
               GO TO 210000-99-FIM                                      
           END-IF.                                                      
                                                                        
           MOVE WRK-LEITURA            TO WRK-OPERACAO.                 
                                                                        
           PERFORM 111000-TESTAR-FS-ARQPARVV.                           
                                                                        
           ADD  1                      TO ACU-LIDO-ARQPARVV.            
                                                                        
           MOVE PVV-CPSSOA-LIGADA      TO WRK-CHV-ARQPARVV.             
                                                                        
      *---------------------------------------------------------------* 
       210000-99-FIM.                  EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       220000-LER-PENDFICA             SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           READ PENDFICA.                                               
                                                                        
           IF  WRK-FS-PENDFICA         EQUAL '10'                       
               MOVE HIGH-VALUES        TO WRK-CHV-PENDFICA              
               GO TO 220000-99-FIM                                      
           END-IF.                                                      
                                                                        
           MOVE WRK-LEITURA            TO WRK-OPERACAO.                 
                                                                        
           PERFORM 112000-TESTAR-FS-PENDFICA.                           
                                                                        
           ADD 1                       TO ACU-LIDO-PENDFICA.            
                                                                        
           MOVE V000-E-CPSSOA          TO WRK-CHV-PENDFICA.             
                                                                        
      *---------------------------------------------------------------* 
       220000-99-FIM.                  EXIT.                            
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       230000-LER-ARQUIRES             SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           READ ARQUIRES.                                               
                                                                        
           IF  WRK-FS-ARQUIRES         EQUAL '10'                       
               MOVE HIGH-VALUES        TO WRK-CHV-ARQUIRES              
               GO TO 230000-99-FIM                                      
           END-IF.                                                      
                                                                        
           MOVE WRK-LEITURA            TO WRK-OPERACAO.                 
                                                                        
           PERFORM 113000-TESTAR-FS-ARQUIRES.                           
                                                                        
           ADD 1                       TO ACU-LIDO-ARQUIRES.            
                                                                        
           MOVE V001-CPSSOA            TO WRK-CHV-ARQUIRES.             
                                                                        
      *---------------------------------------------------------------* 
       230000-99-FIM.                  EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       300000-PROCESSAR                SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           IF  WRK-CHV-ARQPARVV EQUAL WRK-CHV-ARQUIRES                  
               PERFORM 310000-GRAVAR-ARQSOCIO                           
               PERFORM 210000-LER-ARQPARVV                              
           ELSE                                                         
               IF  WRK-CHV-ARQPARVV LESS WRK-CHV-ARQUIRES               
                   PERFORM 210000-LER-ARQPARVV                          
               ELSE                                                     
                   PERFORM 230000-LER-ARQUIRES.                         
                                                                        
      *---------------------------------------------------------------* 
       300000-99-FIM.                  EXIT.                            
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       310000-GRAVAR-ARQSOCIO          SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           PERFORM 220000-LER-PENDFICA  UNTIL                           
                  WRK-CHV-PENDFICA NOT LESS WRK-CHV-ARQPARVV OR         
                  WRK-CHV-PENDFICA EQUAL    HIGH-VALUES.                
                                                                        
           MOVE WRK-GRAVACAO             TO WRK-OPERACAO.               
           MOVE PVV-CGCNUM               TO SOC-CGCNUM-EMPRESA.         
           MOVE PVV-CGCFIL               TO SOC-CGCFIL-EMPRESA.         
           MOVE V001-CCGC-CPF            TO SOC-CGCNUM.                 
           MOVE V001-CFLIAL-CGC          TO SOC-CGCFIL.                 
           MOVE V001-CCTRL-CPF-CGC       TO SOC-CGCCTR.                 
           MOVE V001-IPSSOA-RSUMO        TO SOC-NOME.                   
           MOVE ZEROS                    TO SOC-RAMO-ATVDD.             
                                                                        
           IF  WRK-CHV-ARQPARVV    EQUAL WRK-CHV-PENDFICA AND           
               WRK-CHV-PENDFICA    NOT EQUAL HIGH-VALUES                
               MOVE V000-E-CPSSOA-CADTR      TO SOC-CPSSOA-CADTR        
               IF  V001-CFLIAL-CGC EQUAL ZEROS                          
                   MOVE SPACES               TO SOC-ELOGDR              
                                                SOC-ENRO-LOGDR          
                                                SOC-ECOMPL-LOGDR        
                                                SOC-EBAIRO              
                                                SOC-RMUN                
                                                SOC-CSGL-UF             
                                                SOC-CDDD                
                                                SOC-CFONE               
                                                SOC-CLETRA-RTING        
                   MOVE ZEROS                TO SOC-CCEP                
                                                SOC-CCEP-COMPL          
                   ADD  1                    TO ACU-GRAV-FILIAL-ZEROS   
               ELSE                                                     
                   MOVE V000-E-ELOGDR        TO SOC-ELOGDR              
                   MOVE V000-E-ENRO-LOGDR    TO WRK-E-ENRO-LOGDR        
                   MOVE WRK-E-ENRO-LOGDR-R   TO SOC-ENRO-LOGDR          
                   MOVE V000-E-ECOMPL-LOGDR  TO SOC-ECOMPL-LOGDR        
                   MOVE SPACES               TO SOC-EBAIRO              
                   MOVE V000-E-CCEP          TO SOC-CCEP                
                   MOVE V000-E-CCEP-COMPL    TO SOC-CCEP-COMPL          
                   MOVE V000-E-IMUN          TO SOC-RMUN                
                   MOVE V000-E-CSGL-UF-COML  TO SOC-CSGL-UF             
                   MOVE V000-E-CDDD          TO SOC-CDDD                
                                                                        
BRQ059             IF   V000-E-CFONE-NOVO    NOT NUMERIC                
BRQ059                  MOVE ZEROS                TO SOC-CFONE          
BRQ059             ELSE                                                 
BRQ059                  MOVE V000-E-CFONE-NOVO    TO WRK-E-CFONE        
BRQ059                  MOVE WRK-E-CFONE-R        TO WRK-E-CFONE-9      
BRQ059                  MOVE WRK-E-CFONE-X        TO SOC-CFONE          
BRQ059             END-IF                                               
                                                                        
                   MOVE SPACES               TO SOC-CLETRA-RTING        
                   ADD  1                    TO ACU-GRAV-ATUALIZADOS    
           ELSE                                                         
                   MOVE SPACES               TO SOC-CPSSOA-CADTR        
                                                SOC-ELOGDR              
                                                SOC-ENRO-LOGDR          
                                                SOC-ECOMPL-LOGDR        
                                                SOC-EBAIRO              
                                                SOC-RMUN                
                                                SOC-CSGL-UF             
                                                SOC-CDDD                
                                                SOC-CFONE               
                                                SOC-CLETRA-RTING        
                   MOVE ZEROS                TO SOC-CCEP                
                                                SOC-CCEP-COMPL          
                   ADD  1                    TO ACU-GRAV-SEM-PENDFICA.  
                                                                        
           WRITE SOC-REGISTRO.                                          
                                                                        
           PERFORM 114000-TESTAR-FS-ARQSOCIO.                           
                                                                        
           ADD 1                         TO ACU-GRAV-ARQSOCIO.          
                                                                        
      *---------------------------------------------------------------* 
       310000-99-FIM.                  EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       400000-FINALIZAR                SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           PERFORM 410000-EMITIR-RELATO.                                
                                                                        
           CLOSE ARQPARVV                                               
                 ARQUIRES                                               
                 PENDFICA                                               
                 ARQSOCIO                                               
                 RELATO.                                                
                                                                        
           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.                 
                                                                        
           PERFORM 110000-TESTAR-FILE-STATUS.                           
                                                                        
           GOBACK.                                                      
                                                                        
      *---------------------------------------------------------------* 
       400000-99-FIM.                  EXIT.                            
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       410000-EMITIR-RELATO            SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.                 
           MOVE ACU-LIDO-ARQPARVV      TO LT2-LID-ARQPARVV.             
           MOVE ACU-GRAV-ATUALIZADOS   TO LT3-GRAV-ATUALIZADOS.         
           MOVE ACU-GRAV-FILIAL-ZEROS  TO LT4-GRAV-FILIAL-ZEROS.        
           MOVE ACU-LIDO-PENDFICA      TO LT6-LID-PENDFICA.             
           MOVE ACU-GRAV-SEM-PENDFICA  TO LT7-GRAV-SEM-PENDFICA.        
           MOVE ACU-LIDO-ARQUIRES      TO LT9-LID-ARQUIRES.             
           MOVE ACU-DESP-ARQUIRES      TO LT10-DESP-ARQUIRES.           
           MOVE ACU-GRAV-ARQSOCIO      TO LT12-GRAV-ARQSOCIO.           
                                                                        
           WRITE REG-RELATO            FROM CABEC1.                     
                                                                        
           PERFORM 115000-TESTAR-FS-RELATO.                             
                                                                        
           WRITE REG-RELATO            FROM CABEC2.                     
                                                                        
           PERFORM 115000-TESTAR-FS-RELATO.                             
                                                                        
           WRITE REG-RELATO            FROM CABEC3.                     
                                                                        
           PERFORM 115000-TESTAR-FS-RELATO.                             
                                                                        
           WRITE REG-RELATO            FROM LINTOT1.                    
                                                                        
           PERFORM 115000-TESTAR-FS-RELATO.                             
                                                                        
           WRITE REG-RELATO            FROM LINTOT2.                    
                                                                        
           PERFORM 115000-TESTAR-FS-RELATO.                             
                                                                        
           WRITE REG-RELATO            FROM LINTOT3.                    
                                                                        
           PERFORM 115000-TESTAR-FS-RELATO.                             
                                                                        
           WRITE REG-RELATO            FROM LINTOT4.                    
                                                                        
           PERFORM 115000-TESTAR-FS-RELATO.                             
                                                                        
           WRITE REG-RELATO            FROM LINTOT5.                    
                                                                        
           PERFORM 115000-TESTAR-FS-RELATO.                             
                                                                        
           WRITE REG-RELATO            FROM LINTOT6.                    
                                                                        
           PERFORM 115000-TESTAR-FS-RELATO.                             
                                                                        
           WRITE REG-RELATO            FROM LINTOT7.                    
                                                                        
           PERFORM 115000-TESTAR-FS-RELATO.                             
                                                                        
           WRITE REG-RELATO            FROM LINTOT8.                    
                                                                        
           PERFORM 115000-TESTAR-FS-RELATO.                             
                                                                        
           WRITE REG-RELATO            FROM LINTOT9.                    
                                                                        
           PERFORM 115000-TESTAR-FS-RELATO.                             
                                                                        
           WRITE REG-RELATO            FROM LINTOT11.                   
                                                                        
           PERFORM 115000-TESTAR-FS-RELATO.                             
                                                                        
           WRITE REG-RELATO            FROM LINTOT12.                   
                                                                        
           PERFORM 115000-TESTAR-FS-RELATO.                             
                                                                        
      *---------------------------------------------------------------* 
       410000-99-FIM.                  EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       999999-PROCESSAR-ROTINA-ERRO    SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           MOVE 'APL'                  TO ERR-TIPO-ACESSO.              
           MOVE 'RDAB0032'             TO ERR-PGM.                      
                                                                        
           CALL 'POOL7100'             USING WRK-BATCH                  
                                             ERRO-AREA.                 
                                                                        
           GOBACK.                                                      
                                                                        
      *---------------------------------------------------------------* 
       999999-99-FIM.                  EXIT.                            
      *---------------------------------------------------------------* 
                                                                        

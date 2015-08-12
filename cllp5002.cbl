      *================================================================*
       IDENTIFICATION                  DIVISION.                        
      *================================================================*
       PROGRAM-ID. CLLP5002.                                            
       AUTHOR.     CPMACT 1.4.                                          
      *REMARKS.                                                         
      *    PROGRAMA ORIGINALMENTE CODIFICADO EM ASSEMBLER,              
      *    CONVERTIDO PARA COBOL ENTERPRISE PELO CONVERSOR:             
      *    CPMBRAXIS   "ACT 1.4 -  ASSEMBLER-COBOL TRANSLATOR".         
      *INSTALLATION.                                                    
      *DATE-WRITTEN.                                                    
      *DATE-COMPILED.                                                   
      *SECURITY.                                                        
      *=================================================================
      *                   U L T I M A   A L T E R A C A O               
      *-----------------------------------------------------------------
      *     PROGRAMADOR  :                                              
      *     SUPERVISOR   :                                              
      *     ANALISTA     :                                              
      *     DATA         :                                              
      *                                                                 
      *     OBJETIVO     : ATUALIZA VALORES DOS REGISTROS EM CR$        
      *                                                                 
      *                                                                 
      *                                                                 
      *================================================================*
BRQ141******************************************************************
BRQ141* MAIO/2012 - (BRQ) - TRATAMENTO DE CARTEIRA  ALFANUMERICA        
BRQ141******************************************************************
       ENVIRONMENT                     DIVISION.                        
      *================================================================*
       CONFIGURATION                   SECTION.                         
                                                                        
       SPECIAL-NAMES.                                                   
                                       DECIMAL-POINT IS COMMA           
                                       C01 IS CANAL1                    
                                       CONSOLE IS OPERADOR.             
                                                                        
      *----------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.                         
      *----------------------------------------------------------------*
                                                                        
       FILE-CONTROL.                                                    
           SELECT CADACR ASSIGN        TO  UT-S-CADACR                  
           FILE STATUS                 IS  WRK-FS-CADACR.               
           SELECT CADANT ASSIGN        TO  UT-S-CADANT                  
           FILE STATUS                 IS  WRK-FS-CADANT.               
           SELECT CADATU ASSIGN        TO  UT-S-CADATU                  
           FILE STATUS                 IS  WRK-FS-CADATU.               
      *================================================================*
       DATA                            DIVISION.                        
      *================================================================*
       FILE                            SECTION.                         
      *----------------------------------------------------------------*
      *      INPUT  : ARQUIVO CADACR                                   *
      *               ORG.           - LRECL = 260                     *
      *----------------------------------------------------------------*
       FD  CADACR                                                       
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
       01  FD-REG-CADACR               PIC  X(260).                     
      *----------------------------------------------------------------*
      *      INPUT  : ARQUIVO CADANT                                   *
      *               ORG.           - LRECL = 262                     *
      *----------------------------------------------------------------*
       FD  CADANT                                                       
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
       01  FD-REG-CADANT               PIC  X(362).
      *----------------------------------------------------------------*
      *      OUTPUT : ARQUIVO CADATU                                   *
      *               ORG.           - LRECL = 362                     *
      *----------------------------------------------------------------*
       FD  CADATU                                                       
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
       01  FD-REG-CADATU               PIC  X(362).
      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.                         
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE        
           'INICIO DA WORKING STORAGE SECTION '.                        
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       77  WRK-BATCH                   PIC  X(008)         VALUE        
           'BATCH'.                                                     
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(020)         VALUE        
           'AREA DE FILE-STATUS'.                                       
      *----------------------------------------------------------------*
       77  WRK-FS-CADACR               PIC  X(002)         VALUE SPACES.
       77  WRK-FS-CADANT               PIC  X(002)         VALUE SPACES.
       77  WRK-FS-CADATU               PIC  X(002)         VALUE SPACES.
       77  WRK-ABERTURA                PIC  X(013)         VALUE        
           ' NA ABERTURA'.                                              
       77  WRK-LEITURA                 PIC  X(013)         VALUE        
           ' NA LEITURA '.                                              
       77  WRK-GRAVACAO                PIC  X(013)         VALUE        
           ' NA GRAVACAO'.                                              
       77  WRK-FECHAMENTO              PIC  X(013)         VALUE        
           'NO FECHAMENTO'.                                             
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(031)         VALUE        
           'MENSAGEM DE ERRO DE FILE-STATUS'.                           
      *----------------------------------------------------------------*
       01  WRK-ERRO-BRAD7100.                                           
           05 FILLER                   PIC  X(009)         VALUE        
           '*** ERRO '.                                                 
           05 WRK-OPERACAO             PIC  X(013)         VALUE SPACES.
           05 FILLER                   PIC  X(012)         VALUE        
           ' DO ARQUIVO '.                                              
           05 WRK-NOME-ARQ             PIC  X(008)         VALUE SPACES.
           05 FILLER                   PIC  X(017)         VALUE        
           ' - FILE-STATUS = '.                                         
           05 WRK-FILE-STATUS          PIC  X(002)         VALUE SPACES.
           05 FILLER                   PIC  X(004)         VALUE ' ***'.
           05 FILLER                   PIC  X(010)         VALUE SPACES.
                                                                        
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(030)         VALUE        
           'AREA DO MODULO ATIV8400'.                                   
      *----------------------------------------------------------------*
CPMCAC 01  WRK-ATIV8400                PIC  X(008)         VALUE        
CPMCAC     'ATIV8400'.                                                  
CPMCAC 01  WRK-ATIV8400-TAMANHO        PIC S9(004) COMP    VALUE ZEROS. 
CPMCAC 01  WRK-ATIV8400-OPERACAO       PIC  X(002)         VALUE SPACES.
      *                                                                 
CPMCAC 01  WRK-ATIV8400-OPERANDO2      PIC  X(001)         VALUE SPACES.
CPMCAC 01  WRK-ATIV8400-MASCARA2       PIC  X(001)         VALUE SPACES.
                                                                        
       01  WRK-WKANT.                                                   
           05  FILLER                  PIC  X(017)         VALUE SPACES.
           05  WRK-VRANT               PIC  9(013) COMP-3  VALUE ZEROS. 
BRQ=E******05  FILLER                  PIC  X(214)         VALUE SPACES.
BRQ=I      05  FILLER                  PIC  X(215)         VALUE SPACES.
           05  WRK-CHVANT.                                              
               10  WRK-EMPANT          PIC  9(005) COMP-3  VALUE ZEROS. 
               10  WRK-EMPANT-R        REDEFINES  WRK-EMPANT            
                                       PIC  X(003).                     
CPMCAC         10  WRK-AGANT           PIC  9(005) COMP-3  VALUE ZEROS. 
CPMCAC         10  WRK-AGANT-R         REDEFINES  WRK-AGANT             
                                       PIC  X(003).                     
               10  WRK-NUMANT          PIC  9(015) COMP-3  VALUE ZEROS. 
CPMCAC         10  WRK-NUMANT-R        REDEFINES  WRK-NUMANT            
CPMCAC                                 PIC  X(008).                     
BRQ=E******05  FILLER                  PIC  X(010)         VALUE SPACES.
BRQ=I      05  FILLER                  PIC  X(009)         VALUE SPACES.
           05  WRK-LT-ANT.
               07 WRK-TAXA-CONTRATO-ANT        PIC 9(02)V9(06)  COMP-3.
               07 WRK-VR-REMUNERATORIO-ANT     PIC S9(13)V99    COMP-3.
               07 WRK-VALOR-MORATORIO-ANT      PIC S9(13)V99    COMP-3.
               07 WRK-VALOR-MULTA-ANT          PIC S9(13)V99    COMP-3.
               07 WRK-DESP-JUD-CUSTAS-ANT      PIC S9(11)V99    COMP-3.
               07 WRK-HONORARIOS-ANT           PIC S9(11)V99    COMP-3.
               07 WRK-VL-TOTAL-DIVIDA-ANT      PIC S9(15)V99    COMP-3.
               07 WRK-VL-TAXA-TARIFA-ANT       PIC S9(15)V99    COMP-3.
               07 FILLER                       PIC  X(39).
                                                                        
       01  WRK-WKCR.                                                    
           05  FILLER                  PIC  X(017)         VALUE SPACES.
           05  WRK-VRCR                PIC  9(013) COMP-3  VALUE ZEROS. 
           05  FILLER                  PIC  X(214)         VALUE SPACES.
           05  WRK-CHVCR.                                               
               10  WRK-EMPCR           PIC  9(005) COMP-3  VALUE ZEROS. 
CPMCAC         10  WRK-EMPCR-R         REDEFINES  WRK-EMPCR             
CPMCAC                                 PIC  X(003).                     
               10  WRK-AGCR            PIC  9(005) COMP-3  VALUE ZEROS. 
CPMCAC         10  WRK-AGCR-R          REDEFINES  WRK-AGCR              
CPMCAC                                 PIC  X(003).                     
               10  WRK-NUMCR           PIC  9(015) COMP-3  VALUE ZEROS. 
CPMCAC         10  WRK-NUMCR-R         REDEFINES WRK-NUMCR              
CPMCAC                                 PIC  X(008).                     
           05  FILLER                  PIC  X(008)         VALUE SPACES.
                                                                        
       01  WRK-QTDEL                   PIC  9(007) COMP-3  VALUE ZEROS. 
       01  WRK-QTDEA                   PIC  9(007) COMP-3  VALUE ZEROS. 
       01  WRK-QTDEG                   PIC  9(007) COMP-3  VALUE ZEROS. 
CPMCAC 01  WRK-PAD10                   PIC  BZ.ZZZ.ZZ9     VALUE SPACES.
CPMCAC 01  WRK-PAD10-R                 REDEFINES  WRK-PAD10             
CPMCAC                                 PIC  X(010).                     
                                                                        
       COPY 'I#BRAD7C'.
                                                                        
      *----------------------------------------------------------------*
       01  FILLER                     PIC  X(050)          VALUE        
           'FIM DA WORKING STORAGE SECTION '.                           
      *----------------------------------------------------------------*
                                                                        
      *================================================================*
       PROCEDURE                       DIVISION.                        
      *================================================================*
           OPEN  INPUT CADACR                                           
                       CADANT                                           
                OUTPUT CADATU                                           
           MOVE  WRK-ABERTURA          TO  WRK-OPERACAO                 
           PERFORM 9000-TESTAR-FILE-STATUS THRU 9000-99-FIM             
      *                                                                 
           PERFORM 0050-LECR         THRU  0050-99-FIM.                 
                                                                        
      *----------------------------------------------------------------*
       0010-LER.                                                        
      *----------------------------------------------------------------*
           PERFORM 0060-LEANT        THRU  0060-99-FIM.                 
                                                                        
      *----------------------------------------------------------------*
       0020-CLC.                                                        
      *----------------------------------------------------------------*
           IF  WRK-CHVANT           EQUAL  WRK-CHVCR                    
               GO TO 0030-ATUALIZA                                      
           END-IF                                                       
           IF  WRK-CHVANT            LESS  WRK-CHVCR                    
               GO TO 0040-GRATU                                         
           END-IF                                                       
           PERFORM 0050-LECR         THRU  0050-99-FIM                  
           GO TO 0020-CLC.                                              
                                                                        
      *----------------------------------------------------------------*
       0030-ATUALIZA.                                                   
      *----------------------------------------------------------------*
      *    ATUALIZA VALOR EM CR$                                        
           MOVE  WRK-VRCR              TO  WRK-VRANT                    
           ADD 1                       TO  WRK-QTDEA.                   
                                                                        
      *----------------------------------------------------------------*
       0040-GRATU.                                                      
      *----------------------------------------------------------------*
           WRITE  FD-REG-CADATU      FROM  WRK-WKANT                    
           MOVE  WRK-GRAVACAO          TO  WRK-OPERACAO                 
           PERFORM 9030-TESTAR-FS-CADATU THRU 9030-99-FIM               
      *                                                                 
           ADD 1                       TO  WRK-QTDEG                    
           GO TO 0010-LER.                                              
                                                                        
      *----------------------------------------------------------------*
       0050-LECR.                                                       
      *----------------------------------------------------------------*
           READ  CADACR              INTO  WRK-WKCR END-READ            
           MOVE  WRK-LEITURA           TO  WRK-OPERACAO                 
           IF  WRK-FS-CADACR        EQUAL  '10'                         
               GO TO 0070-FIMCR                                         
           ELSE                                                         
               PERFORM 9010-TESTAR-FS-CADACR THRU 9010-99-FIM           
           END-IF.                                                      
                                                                        
CPMCAC     MOVE    1                   TO  WRK-ATIV8400-TAMANHO         
CPMCAC     MOVE    'OI'                TO  WRK-ATIV8400-OPERACAO        
CPMCAC     MOVE    WRK-EMPCR-R(3:1)    TO  WRK-ATIV8400-OPERANDO2       
CPMCAC     MOVE    X'0F'               TO  WRK-ATIV8400-MASCARA2        
                                                                        
CPMCAC     CALL    WRK-ATIV8400     USING  WRK-ATIV8400-TAMANHO         
CPMCAC                                     WRK-ATIV8400-OPERACAO        
CPMCAC                                     WRK-ATIV8400-OPERANDO2       
CPMCAC                                     WRK-ATIV8400-MASCARA2        
                                                                        
CPMCAC     MOVE    WRK-ATIV8400-OPERANDO2                               
CPMCAC                                 TO  WRK-EMPCR-R(3:1)             
                                                                        
CPMCAC     MOVE    1                   TO  WRK-ATIV8400-TAMANHO         
CPMCAC     MOVE    'OI'                TO  WRK-ATIV8400-OPERACAO        
CPMCAC     MOVE    WRK-AGCR-R(3:1)     TO  WRK-ATIV8400-OPERANDO2       
CPMCAC     MOVE    X'0F'               TO  WRK-ATIV8400-MASCARA2        
                                                                        
CPMCAC     CALL    WRK-ATIV8400     USING  WRK-ATIV8400-TAMANHO         
CPMCAC                                     WRK-ATIV8400-OPERACAO        
CPMCAC                                     WRK-ATIV8400-OPERANDO2       
CPMCAC                                     WRK-ATIV8400-MASCARA2        
                                                                        
CPMCAC     MOVE    WRK-ATIV8400-OPERANDO2                               
CPMCAC                                 TO  WRK-AGCR-R(3:1)              
                                                                        
CPMCAC     MOVE    1                   TO  WRK-ATIV8400-TAMANHO         
CPMCAC     MOVE    'OI'                TO  WRK-ATIV8400-OPERACAO        
CPMCAC     MOVE    WRK-NUMCR-R(8:1)    TO  WRK-ATIV8400-OPERANDO2       
CPMCAC     MOVE    X'0F'               TO  WRK-ATIV8400-MASCARA2        
                                                                        
CPMCAC     CALL    WRK-ATIV8400     USING  WRK-ATIV8400-TAMANHO         
CPMCAC                                     WRK-ATIV8400-OPERACAO        
CPMCAC                                     WRK-ATIV8400-OPERANDO2       
CPMCAC                                     WRK-ATIV8400-MASCARA2        
                                                                        
CPMCAC     MOVE    WRK-ATIV8400-OPERANDO2                               
CPMCAC                                 TO  WRK-NUMCR-R(8:1).            
      *                                                                 
      *----------------------------------------------------------------*
       0050-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
      *                                                                 
      *----------------------------------------------------------------*
       0060-LEANT.                                                      
      *----------------------------------------------------------------*
           READ  CADANT              INTO  WRK-WKANT END-READ           
           MOVE  WRK-LEITURA           TO  WRK-OPERACAO                 
           IF  WRK-FS-CADANT        EQUAL  '10'                         
               GO TO 0080-FIMANT                                        
           ELSE                                                         
               PERFORM 9020-TESTAR-FS-CADANT THRU 9020-99-FIM           
           END-IF                                                       
      *                                                                 
           ADD 1                       TO  WRK-QTDEL.                   
                                                                        
CPMCAC     MOVE    1                   TO  WRK-ATIV8400-TAMANHO         
CPMCAC     MOVE    'OI'                TO  WRK-ATIV8400-OPERACAO        
CPMCAC     MOVE    WRK-EMPANT-R(3:1)   TO  WRK-ATIV8400-OPERANDO2       
CPMCAC     MOVE    X'0F'               TO  WRK-ATIV8400-MASCARA2        
                                                                        
CPMCAC     CALL    WRK-ATIV8400     USING  WRK-ATIV8400-TAMANHO         
CPMCAC                                     WRK-ATIV8400-OPERACAO        
CPMCAC                                     WRK-ATIV8400-OPERANDO2       
CPMCAC                                     WRK-ATIV8400-MASCARA2        
                                                                        
CPMCAC     MOVE    WRK-ATIV8400-OPERANDO2                               
CPMCAC                                 TO  WRK-EMPANT-R(3:1)            
                                                                        
CPMCAC     MOVE    1                   TO  WRK-ATIV8400-TAMANHO         
CPMCAC     MOVE    'OI'                TO  WRK-ATIV8400-OPERACAO        
CPMCAC     MOVE    WRK-AGANT-R(3:1)    TO  WRK-ATIV8400-OPERANDO2       
CPMCAC     MOVE    X'0F'               TO  WRK-ATIV8400-MASCARA2        
                                                                        
CPMCAC     CALL    WRK-ATIV8400     USING  WRK-ATIV8400-TAMANHO         
CPMCAC                                     WRK-ATIV8400-OPERACAO        
CPMCAC                                     WRK-ATIV8400-OPERANDO2       
CPMCAC                                     WRK-ATIV8400-MASCARA2        
                                                                        
CPMCAC     MOVE    WRK-ATIV8400-OPERANDO2                               
CPMCAC                                 TO  WRK-AGANT-R(3:1)             
                                                                        
CPMCAC     MOVE    1                   TO  WRK-ATIV8400-TAMANHO         
CPMCAC     MOVE    'OI'                TO  WRK-ATIV8400-OPERACAO        
CPMCAC     MOVE    WRK-NUMANT-R(8:1)   TO  WRK-ATIV8400-OPERANDO2       
CPMCAC     MOVE    X'0F'               TO  WRK-ATIV8400-MASCARA2        
                                                                        
CPMCAC     CALL    WRK-ATIV8400     USING  WRK-ATIV8400-TAMANHO         
CPMCAC                                     WRK-ATIV8400-OPERACAO        
CPMCAC                                     WRK-ATIV8400-OPERANDO2       
CPMCAC                                     WRK-ATIV8400-MASCARA2        
                                                                        
CPMCAC     MOVE    WRK-ATIV8400-OPERANDO2                               
CPMCAC                                  TO  WRK-NUMANT-R(8:1).          
                                                                        
      *----------------------------------------------------------------*
       0060-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
      *                                                                 
      *----------------------------------------------------------------*
       0070-FIMCR.                                                      
      *----------------------------------------------------------------*
           MOVE X'99999F99999F999999999999999F'                         
                                       TO  WRK-CHVCR                    
           GO TO 0020-CLC.                                              
      *                                                                 
      *----------------------------------------------------------------*
       0080-FIMANT.                                                     
      *----------------------------------------------------------------*
           MOVE  WRK-QTDEL             TO  WRK-PAD10                    
           DISPLAY  'REGISTROS LIDOS    -' WRK-PAD10                    
                                       UPON OPERADOR                    
                                                                        
           MOVE  WRK-QTDEA             TO  WRK-PAD10                    
           DISPLAY 'REGISTROS ALTERADOS-' WRK-PAD10                     
                                       UPON OPERADOR                    
                                                                        
           MOVE  WRK-QTDEG             TO  WRK-PAD10                    
           DISPLAY 'REGISTROS GRAVADOS -'  WRK-PAD10                    
                                       UPON OPERADOR                    
      *                                                                 
           CLOSE CADACR                                                 
                 CADANT                                                 
                 CADATU                                                 
           MOVE  WRK-FECHAMENTO        TO  WRK-OPERACAO                 
           PERFORM 9000-TESTAR-FILE-STATUS THRU 9000-99-FIM             
      *                                                                 
           GOBACK.                                                      
      *                                                                 
      *----------------------------------------------------------------*
       9000-TESTAR-FILE-STATUS.                                         
      *----------------------------------------------------------------*
           PERFORM 9010-TESTAR-FS-CADACR THRU 9010-99-FIM               
           PERFORM 9020-TESTAR-FS-CADANT THRU 9020-99-FIM               
           PERFORM 9030-TESTAR-FS-CADATU THRU 9030-99-FIM.              
      *                                                                 
      *----------------------------------------------------------------*
       9000-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
      *                                                                 
      *----------------------------------------------------------------*
       9010-TESTAR-FS-CADACR.                                           
      *----------------------------------------------------------------*
                                                                        
           IF (WRK-FS-CADACR    NOT EQUAL  '00')                        
               MOVE 'CADACR'           TO  WRK-NOME-ARQ                 
               MOVE WRK-FS-CADACR      TO  WRK-FILE-STATUS              
               MOVE WRK-ERRO-BRAD7100  TO  ERR-TEXTO                    
               PERFORM 9999-PROCESSAR-ROTINA-ERRO                       
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       9010-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       9020-TESTAR-FS-CADANT.                                           
      *----------------------------------------------------------------*
                                                                        
           IF (WRK-FS-CADANT    NOT EQUAL  '00')                        
               MOVE 'CADANT'           TO  WRK-NOME-ARQ                 
               MOVE WRK-FS-CADANT      TO  WRK-FILE-STATUS              
               MOVE WRK-ERRO-BRAD7100  TO  ERR-TEXTO                    
               PERFORM 9999-PROCESSAR-ROTINA-ERRO                       
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       9020-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       9030-TESTAR-FS-CADATU.                                           
      *----------------------------------------------------------------*
                                                                        
           IF (WRK-FS-CADATU    NOT EQUAL  '00')                        
               MOVE 'CADATU'           TO  WRK-NOME-ARQ                 
               MOVE WRK-FS-CADATU      TO  WRK-FILE-STATUS              
               MOVE WRK-ERRO-BRAD7100  TO  ERR-TEXTO                    
               PERFORM 9999-PROCESSAR-ROTINA-ERRO                       
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       9030-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO.                                      
      *----------------------------------------------------------------*
           MOVE 'CLLP5002'             TO  ERR-PGM.                     
           MOVE 'APL'                  TO  ERR-TIPO-ACESSO              
           CALL 'BRAD7100'          USING  WRK-BATCH                    
                                           ERRO-AREA.                   
           GOBACK.                                                      
      *                                                                 
      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*

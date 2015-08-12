      *===============================================================* 
       IDENTIFICATION DIVISION.                                         
      *===============================================================* 
                                                                        
       PROGRAM-ID. CLLP9008.                                            
       AUTHOR. PAULOSO.                                                 
      *===============================================================* 
      *                   C P M   S I S T E M A S                     * 
      *---------------------------------------------------------------* 
      *                                                               * 
      *      PROGRAMA     : CLLP9008                                  * 
      *      ANALISTA     : PAULOSO                                   * 
      *      DATA         : MAIO DE 2001                              * 
      *      OBJETIVO     :                                           * 
      *        ACERTA TIPO DE BAIXA PARA OS REGISTROS DA PRESTACAO    * 
      *        CONTAS.                                                * 
      *===============================================================* 
                                                                        
       ENVIRONMENT DIVISION.                                            
      *===============================================================* 
                                                                        
      *---------------------------------------------------------------* 
       CONFIGURATION SECTION.                                           
      *---------------------------------------------------------------* 
                                                                        
       SPECIAL-NAMES.                                                   
           DECIMAL-POINT IS COMMA.                                      
                                                                        
      *---------------------------------------------------------------* 
       INPUT-OUTPUT SECTION.                                            
      *---------------------------------------------------------------* 
                                                                        
       FILE-CONTROL.                                                    
                                                                        
           SELECT  BAIXAENT ASSIGN TO UT-S-BAIXAENT                     
                       FILE STATUS IS WRK-FS-BAIXAENT.                  
                                                                        
           SELECT  PRESTCTA ASSIGN TO UT-S-PRESTCTA                     
                       FILE STATUS IS WRK-FS-PRESTCTA.                  
                                                                        
           SELECT  BAIXASAI ASSIGN TO UT-S-BAIXASAI                     
                       FILE STATUS IS WRK-FS-BAIXASAI.                  
                                                                        
           SELECT  RELATO   ASSIGN TO UT-S-RELATO                       
                       FILE STATUS IS WRK-FS-RELATO.                    
                                                                        
      *===============================================================* 
       DATA DIVISION.                                                   
      *===============================================================* 
                                                                        
      *---------------------------------------------------------------* 
       FILE SECTION.                                                    
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
      *    INPUT:     ARQUIVO DE BAIXAS                               * 
      *               ORG. SEQUENCIAL   -   LRECL = 313               * 
      *---------------------------------------------------------------* 
                                                                        
       FD  BAIXAENT                                                     
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  REG-BAIXAENT.                                                
           05  BX-CHAVE.                                                
               10 BX-EMPRESA        PIC  9(05) COMP-3.                  
               10 BX-AGENCIA        PIC  9(05) COMP-3.                  
               10 BX-NUMCL          PIC  9(15) COMP-3.                  
               10 BX-DIGITO         PIC  X(01).                         
           05  BX-DATA-BAIXA        PIC  X(10).                         
           05  BX-CONTA-CORR.                                           
               10 BX-NUM-CC         PIC  9(07) COMP-3.                  
               10 BX-DIG-CC         PIC  X(01).                         
           05  BX-CARTEIRA          PIC  X(03).                         
           05  BX-CONTRATO          PIC  9(07) COMP-3.                  
           05  BX-ID                PIC  X(02).                         
           05  BX-TIPO-BAIXA        PIC  X(01).                         
           05  BX-DEVEDOR.                                              
               10 BX-NOME           PIC  X(40).                         
               10 BX-CGC-CPF.                                           
                  15 BX-NUM-CGC-CPF PIC  9(09) COMP-3.                  
                  15 BX-FIL-CGC-CPF PIC  9(05) COMP-3.                  
                  15 BX-CTR-CGC-CPF PIC  9(02).                         
           05  BX-DATA-VENCTO       PIC  X(10).                         
           05  BX-DATA-CL           PIC  X(10).                         
           05  BX-DATA-LP           PIC  X(10).                         
           05  BX-VR-CONTABIL       PIC  9(13)V9(02) COMP-3.            
           05  BX-VR-DEV-INICIAL    PIC  9(13)V9(02) COMP-3.            
           05  BX-VR-BASE           PIC  9(13)V9(02) COMP-3.            
           05  BX-ENCARGO-VENCIDO   PIC  9(13)V9(02) COMP-3.            
           05  BX-ENCARGO-VINCENDO  PIC  9(13)V9(02) COMP-3.            
           05  FD-BX-DEBITO-CC      PIC  9(13)V9(02) COMP-3.            
           05  BX-PRINCIPAL         PIC  9(13)V9(02) COMP-3.            
           05  BX-JUROS-MORA        PIC  9(13)V9(02) COMP-3.            
           05  BX-CORR-MONETARIA    PIC  9(13)V9(02) COMP-3.            
           05  BX-JUROS-12PAA       PIC  9(13)V9(02) COMP-3.            
           05  BX-COBRANCA-DIARIO   PIC  9(13)V9(02) COMP-3.            
           05  BX-TOTAL-CONTRATO    PIC  9(13)V9(02) COMP-3.            
           05  BX-COD-EMPRESA       PIC  X(02).                         
           05  BX-COD-DIR-REGIONAL  PIC  9(05) COMP-3.                  
           05  BX-DT-CLPDD-180      PIC  X(10).                         
           05  BX-MARCA-PDD-180     PIC  X(01).                         
           05  BX-RAZAO-PRINCIP     PIC  9(05) COMP-3.                  
           05  BX-RAZAO-RENDAS      PIC  9(05) COMP-3.                  
           05  BX-COD-NAT-OPER      PIC  X(03).                         
           05  BX-MOEDA             PIC  X(02).                         
           05  BX-TIPO-GARANTIA     PIC  X(02).                         
           05  BX-LOCAL             PIC  X(02).                         
           05  BX-TIPO-PENDENCI     PIC  9(05) COMP-3.                  
           05  BX-MARCA-IMPE        PIC  X(01).                         
           05  BX-DATA-AJUIZAMENTO  PIC  X(10).                         
           05  BX-IOF-NORMAL        PIC  9(13)V9(02) COMP-3.            
           05  BX-IOF-COMPL         PIC  9(13)V9(02) COMP-3.            
           05  BX-ALIQ-CONT-RECOL   PIC  9(03)V9(06) COMP-3.            
           05  BX-ALIQ-REC-TR-CL    PIC  9(03)V9(06) COMP-3.            
           05  BX-ALIQ-COMPL        PIC  9(03)V9(06) COMP-3.            
           05  BX-IOF-OPCAO         PIC  X(01).                         
           05  BX-VR-BASE-IOF       PIC  9(13)V9(02) COMP-3.            
           05  BX-DATA-SISTEL       PIC  X(10).                         
           05  BX-IND-SISTEL        PIC  X(01).                         
           05  BX-NOTIF-SISTEL      PIC  X(01).                         
           05  BX-COMPL-TIPO-BAIXA  PIC  X(01).                         
BRQ        05  BX-LT.
               10 BX-TAXA-CONTRATO    PIC  9(02)V9(06) COMP-3.
               10 BX-VR-REMUNERATORIO PIC S9(13)V99 COMP-3.
               10 BX-VALOR-MORATORIO  PIC S9(13)V99 COMP-3.
               10 BX-VALOR-MULTA      PIC S9(13)V99 COMP-3.
               10 BX-DESP-JUD-CUSTAS  PIC S9(11)V99 COMP-3.
               10 BX-HONORARIOS       PIC S9(11)V99 COMP-3.
               10 BX-VL-TOTAL-DIVIDA  PIC S9(15)V99 COMP-3.
               10 BX-VL-TAXA-TARIFA   PIC S9(15)V99 COMP-3.
BRQLEI         10 BX-VL-IOF           PIC S9(13)V99 USAGE COMP-3.
BRQLEI         10 BX-VL-CORRECAO      PIC S9(13)V99 USAGE COMP-3.
BRQLEI         10 BX-VL-JUROS-12PCA   PIC S9(13)V99 USAGE COMP-3.
BRQLEI         10 BX-PERIODICIDADE    PIC S9(02)   USAGE COMP-3.
BRQLEI         10 FILLER              PIC  X(13).
001030
                                                                        
      *---------------------------------------------------------------* 
      *    INPUT:     ARQUIVO DE PRESTACAO DE CONTAS                  * 
      *               ORG. SEQUENCIAL   -   LRECL = 070               * 
      *---------------------------------------------------------------* 
                                                                        
       FD  PRESTCTA                                                     
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  REG-PRESTCTA.                                                
           05  PRES-CCGC-CPF           PIC 9(09).                       
           05  PRES-CJUNC-DEPDC        PIC 9(04).                       
           05  PRES-CCTA-CLI           PIC 9(07).                       
BRQ        05  FD-PRES-CCART-BDSCO-COM-FILLER.                          
BRQ            10  FD-PRES-CCART-BDSCO PIC X(03).                       
BRQ            10  FILLER              PIC X(02).                       
           05  PRES-CCONTR             PIC 9(09).                       
           05  PRES-DVCTO              PIC X(10).                       
           05  PRES-DBAIXA-INADP       PIC X(10).                       
           05  PRES-CPREST-ADVOG       PIC 9(06).                       
           05  PRES-DPREST             PIC X(10).                       
                                                                        
      *---------------------------------------------------------------* 
      *    INPUT:    ARQ. DE BAIXAS MAIS DADOS DA PRESTACAO DE CONTAS * 
      *              ORG. SEQUENCIAL   -   LRECL = 314                * 
      *---------------------------------------------------------------* 
                                                                        
       FD  BAIXASAI                                                     
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  REG-BAIXASAI                PIC X(414).
                                                                        
      *---------------------------------------------------------------* 
      *    OUTPUT    RELATORIO                                        * 
      *              ORG. SEQUENCIAL   -   LRECL = 133                * 
      *---------------------------------------------------------------* 
                                                                        
       FD  RELATO                                                       
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  REG-RELATO                  PIC X(133).                      
                                                                        
      *---------------------------------------------------------------* 
       WORKING-STORAGE SECTION.                                         
      *---------------------------------------------------------------* 
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -* 
       77  FILLER PIC X(32) VALUE  '*  INICIO DA WORKING CLLP9008  *'.  
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -* 
                                                                        
       77  WRK-FS-BAIXAENT             PIC X(02)        VALUE  SPACES.  
       77  WRK-FS-PRESTCTA             PIC X(02)        VALUE  SPACES.  
       77  WRK-FS-BAIXASAI             PIC X(02)        VALUE  SPACES.  
       77  WRK-FS-RELATO               PIC X(02)        VALUE  SPACES.  
       77  WRK-OPERACAO                PIC X(13) VALUE  SPACES.         
       77  WRK-ABERTURA                PIC X(13) VALUE  'NA ABERTURA'.  
       77  WRK-LEITURA                 PIC X(13) VALUE  'NA LEITURA'.   
       77  WRK-GRAVACAO                PIC X(13) VALUE  'NA GRAVACAO'.  
       77  WRK-FECHAMENTO              PIC X(13) VALUE  'NO FECHAMENTO'.
       77  WRK-ABEND                   PIC S9(04) COMP  VALUE +1111.    
       77  WRK-ACHOU                   PIC X(03)           VALUE SPACES.
       77  WRK-TIPO-BAIXA              PIC X(01)           VALUE SPACES.
       77  ACU-LIDOS-BAIXAENT          PIC 9(09)    COMP-3 VALUE ZEROS. 
       77  ACU-LIDOS-PRESTCTA          PIC 9(09)    COMP-3 VALUE ZEROS. 
       77  ACU-GRAVS-BAIXASAI          PIC 9(09)    COMP-3 VALUE ZEROS. 
       77  ACU-PAGINA                  PIC 9(09)    COMP-3 VALUE ZEROS. 
       77  ACU-LINHA                   PIC 9(09)    COMP-3 VALUE 99.    
                                                                        
       01  WRK-CHAVE-BAIXA.                                             
           03  WRK-CJUNC-DEPDC-BX     PIC 9(04) VALUE ZEROS.            
           03  WRK-CCTA-CLI-BX        PIC 9(07) VALUE ZEROS.            
BRQ        03  WRK-CCART-BDSCO-BX     PIC X(03) VALUE SPACES.           
           03  WRK-CCONTR-BX          PIC 9(09) VALUE ZEROS.            
           03  WRK-DVCTO-BX           PIC 9(08) VALUE ZEROS.            
           03  WRK-DBAIXA-BX          PIC 9(08) VALUE ZEROS.            
                                                                        
       01  WRK-CHAVE-PREST.                                             
           03  WRK-CJUNC-DEPDC-PS     PIC 9(04) VALUE ZEROS.            
           03  WRK-CCTA-CLI-PS        PIC 9(07) VALUE ZEROS.            
BRQ        03  WRK-CCART-BDSCO-PS     PIC X(03) VALUE SPACES.           
           03  WRK-CCONTR-PS          PIC 9(09) VALUE ZEROS.            
           03  WRK-DVCTO-PS           PIC 9(08) VALUE ZEROS.            
           03  WRK-DBAIXA-PS          PIC 9(08) VALUE ZEROS.            
                                                                        
       01  WRK-DATA-DB2.                                                
           03  WRK-DIA-DB2            PIC 9(02) VALUE ZEROS.            
           03  FILLER                 PIC X(01) VALUE SPACES.           
           03  WRK-MES-DB2            PIC 9(02) VALUE ZEROS.            
           03  FILLER                 PIC X(01) VALUE SPACES.           
           03  WRK-SEC-DB2            PIC 9(02) VALUE ZEROS.            
           03  WRK-ANO-DB2            PIC 9(02) VALUE ZEROS.            
                                                                        
       01  WRK-DATA.                                                    
           03  WRK-SEC                PIC 9(02) VALUE ZEROS.            
           03  WRK-ANO                PIC 9(02) VALUE ZEROS.            
           03  WRK-MES                PIC 9(02) VALUE ZEROS.            
           03  WRK-DIA                PIC 9(02) VALUE ZEROS.            
       01  WRK-DATA-R                 REDEFINES                         
           WRK-DATA                   PIC 9(08).                        
                                                                        
BRQ    01  WRK-BX-DEBITO-TOTAL        PIC 9(13)V99 VALUE ZEROS.         
BRQ    01  FILLER REDEFINES WRK-BX-DEBITO-TOTAL.                        
BRQ        03 FILLER                  PIC X(04).                        
BRQ        03 WRK-BX-DEBITO-REDUZIDO  PIC 9(09)V99.                     
001580                                                                  
001590 01  WRK-HORA.                                                    
001600     03  WRK-HH                 PIC 9(02) VALUE ZEROS.            
001610     03  WRK-MM                 PIC 9(02) VALUE ZEROS.            
001620     03  WRK-SS                 PIC 9(02) VALUE ZEROS.            
001630 01  WRK-HORA-R                 REDEFINES                         
001640     WRK-HORA                   PIC 9(06).                        
001650                                                                  
001660* AREA DA POOL7600 *                                              
001670 01  WRK-DATA-HORA.                                               
001680     03  WRK-DT-JULIANA          PIC 9(05) COMP-3 VALUE ZEROS.    
001690     03  WRK-DT-AAMMDD           PIC 9(07) COMP-3 VALUE ZEROS.    
001700     03  WRK-DT-AAAAMMDD         PIC 9(09) COMP-3 VALUE ZEROS.    
001710     03  WRK-TI-HHMMSS           PIC 9(07) COMP-3 VALUE ZEROS.    
001720     03  WRK-TI-HHMMSSMMMMMM     PIC 9(13) COMP-3 VALUE ZEROS.    
001730     03  WRK-TIMESTAMP           PIC X(20)        VALUE SPACES.   
001740                                                                  
001750*---------------------------------------------------------------* 
001760* DEFINICAO DA AREA DE RELATORIO                                  
001770*---------------------------------------------------------------* 
001780 01  CABEC1.                                                      
001790     03  CAB1-CARRO              PIC X(01) VALUE '1'.             
001800     03  CAB1-DIA                PIC 9(02) VALUE ZEROS.           
001810     03  FILLER                  PIC X(01) VALUE '/'.             
001820     03  CAB1-MES                PIC 9(02) VALUE ZEROS.           
001830     03  FILLER                  PIC X(01) VALUE '/'.             
001840     03  CAB1-SEC                PIC 9(02) VALUE ZEROS.           
001850     03  CAB1-ANO                PIC 9(02) VALUE ZEROS.           
001860     03  FILLER                  PIC X(11) VALUE  SPACES.         
001870     03  FILLER                  PIC X(36) VALUE                  
001880         'B A N C O   B R A D E S C O   S / A.'.                  
001890     03  FILLER                  PIC X(14) VALUE  SPACES.         
001900     03  CAB1-HH                 PIC 9(02) VALUE ZEROS.           
001910     03  FILLER                  PIC X(01) VALUE ':'.             
001920     03  CAB1-MM                 PIC 9(02) VALUE ZEROS.           
001930     03  FILLER                  PIC X(01) VALUE ':'.             
001940     03  CAB1-SS                 PIC 9(02) VALUE ZEROS.           
001950                                                                  
001960 01  CABEC1A.                                                     
001970     03  CAB1-CARRO              PIC X(01) VALUE ' '.             
001980     03  FILLER                  PIC X(08) VALUE 'CLLP9008'.      
001990     03  FILLER                  PIC X(12) VALUE  SPACES.         
002000     03  FILLER                  PIC X(38) VALUE                  
002010         'DEPARTAMENTO DE RECUPERACAO DE CREDITO'.                
002020     03  FILLER                  PIC X(11) VALUE  SPACES.         
002030     03  FILLER                  PIC X(06) VALUE 'PAG.: '.        
002040     03  CAB1-PAGINA             PIC ZZZ9 VALUE ZEROS.            
002050                                                                  
002060 01  CABEC2.                                                      
002070     03  CAB2-CARRO              PIC X(01) VALUE '0'.             
002080     03  FILLER                  PIC X(07) VALUE SPACES.          
002090     03  FILLER                  PIC X(50) VALUE                  
002100         'ATUALIZACAO DO TIPO DE BAIXA - PRESTACAO DE CONTAS'.    
002120                                                                  
002130 01  CABEC3.                                                      
002140     03  CAB3-CARRO              PIC X(01)  VALUE '0'.            
002150     03  FILLER                  PIC X(02)  VALUE SPACES.         
002160     03  FILLER                  PIC X(07)  VALUE                 
002170         'AGENCIA'.                                               
002180     03  FILLER                  PIC X(09)  VALUE                 
002190         '  CONTA  '.                                             
002200     03  FILLER                  PIC X(14)  VALUE                 
002210         '    CARTEIRA'.                                          
002220     03  FILLER                  PIC X(10)  VALUE                 
002230         ' CONTRATO'.                                             
002240     03  FILLER                  PIC X(12)  VALUE                 
002250         '  VENCIMENTO'.                                          
002260     03  FILLER                  PIC X(14)  VALUE                 
002270         '    TIPO BAIXA'.                                        
002280     03  FILLER                  PIC X(12)  VALUE                 
002290         '     BAIXA  '.                                          
002280     03  FILLER                  PIC X(18)  VALUE                 
002290         ' VALOR BAIXADO    '.                                    
002280     03  FILLER                  PIC X(06)  VALUE                 
002290         '    ID'.                                                
002300                                                                  
002310 01  LINDET.                                                      
002320     03  LINDET-CARRO            PIC X(01)  VALUE ' '.            
002330     03  FILLER                  PIC X(02)  VALUE SPACES.         
002340     03  LINDET-AGENCIA          PIC ZZZZ9 VALUE ZEROS.           
002350     03  FILLER                  PIC X(04)  VALUE SPACES.         
002360     03  LINDET-CONTA            PIC ZZZZZZ9 VALUE ZEROS.         
002370     03  FILLER                  PIC X(07)  VALUE SPACES.         
002380     03  LINDET-CARTEIRA         PIC X(03) VALUE SPACES.          
002390     03  FILLER                  PIC X(04)  VALUE SPACES.         
002400     03  LINDET-CONTRATO         PIC ZZZZZZ9 VALUE ZEROS.         
002410     03  FILLER                  PIC X(05)  VALUE SPACES.         
002420     03  LINDET-DATA-VENCTO      PIC X(10) VALUE SPACES.          
002430     03  FILLER                  PIC X(08) VALUE SPACES.          
002440     03  LINDET-TIPO-BAIXA       PIC X(01) VALUE SPACES.          
002450     03  FILLER                  PIC X(01) VALUE SPACES.          
002460     03  LINDET-COMPL-TPO-BAIXA  PIC X(01) VALUE SPACES.          
002470     03  FILLER                  PIC X(04) VALUE SPACES.          
002480     03  LINDET-DATA-BAIXA       PIC X(10) VALUE SPACES.          
002470     03  FILLER                  PIC X(04) VALUE SPACES.          
002480     03  LINDET-VALOR-BAIXA      PIC ZZZ.ZZZ.ZZ9,99 VALUE ZEROS.  
002470     03  FILLER                  PIC X(04) VALUE SPACES.          
002480     03  LINDET-ID               PIC X(02) VALUE ZEROS.           
002490                                                                  
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -* 
       01  FILLER PIC X(32) VALUE  '*   FIM DA WORKING CLLP9008  *'.    
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -* 
      *===============================================================* 
       PROCEDURE DIVISION.                                              
      *===============================================================* 
                                                                        
      *---------------------------------------------------------------* 
       0000-INICIAR SECTION.                                            
      *---------------------------------------------------------------* 
                                                                        
           OPEN INPUT   BAIXAENT                                        
                        PRESTCTA                                        
                OUTPUT  BAIXASAI                                        
                        RELATO.                                         
                                                                        
           MOVE    WRK-ABERTURA            TO   WRK-OPERACAO.           
           PERFORM 1100-TESTAR-FILE-STATUS.                             
                                                                        
002750     CALL 'POOL7600'             USING WRK-DATA-HORA.             
002760     MOVE WRK-DT-AAAAMMDD        TO WRK-DATA-R.                   
002770     MOVE WRK-TI-HHMMSS          TO WRK-HORA-R.                   
002780     MOVE WRK-SEC                TO CAB1-SEC.                     
002790     MOVE WRK-ANO                TO CAB1-ANO.                     
002800     MOVE WRK-MES                TO CAB1-MES.                     
002810     MOVE WRK-DIA                TO CAB1-DIA.                     
002820                                                                  
002830     MOVE WRK-HH                 TO CAB1-HH.                      
002840     MOVE WRK-MM                 TO CAB1-MM.                      
002850     MOVE WRK-SS                 TO CAB1-SS.                      
002860                                                                  
           PERFORM 1200-LER-BAIXAENT.                                   
                                                                        
           PERFORM 1300-LER-PRESTCTA.                                   
                                                                        
           PERFORM 2000-PROCESSA UNTIL WRK-FS-BAIXAENT = '10' AND       
                                       WRK-FS-PRESTCTA = '10'.          
                                                                        
           PERFORM 3000-EMITE-DISPLAY-FINAL.                            
                                                                        
           CLOSE BAIXAENT                                               
                 PRESTCTA                                               
                 BAIXASAI                                               
                 RELATO.                                                
                                                                        
           MOVE    WRK-FECHAMENTO TO  WRK-OPERACAO.                     
           PERFORM 1100-TESTAR-FILE-STATUS.                             
                                                                        
           STOP RUN.                                                    
                                                                        
      *---------------------------------------------------------------* 
       0000-99-FIM. EXIT.                                               
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1100-TESTAR-FILE-STATUS SECTION.                                 
      *---------------------------------------------------------------* 
                                                                        
           PERFORM 1110-TESTAR-FS-BAIXAENT.                             
                                                                        
           PERFORM 1120-TESTAR-FS-PRESTCTA.                             
                                                                        
           PERFORM 1160-TESTAR-FS-BAIXASAI.                             
                                                                        
           PERFORM 1170-TESTAR-FS-RELATO.                               
                                                                        
      *---------------------------------------------------------------* 
       1100-99-FIM. EXIT.                                               
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1110-TESTAR-FS-BAIXAENT SECTION.                                 
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-FS-BAIXAENT NOT EQUAL '00'                            
              DISPLAY '************** CLLP9008 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              BAIXAENT             *'           
              DISPLAY '*         FILE STATUS =  ' WRK-FS-BAIXAENT       
                                                 '         *'           
              DISPLAY '************** CLLP9008 *************'           
              CALL 'ILBOABN0'     USING WRK-ABEND.                      
                                                                        
      *---------------------------------------------------------------* 
       1110-99-FIM. EXIT.                                               
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1120-TESTAR-FS-PRESTCTA SECTION.                                 
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-FS-PRESTCTA NOT EQUAL '00'                            
              DISPLAY '************** CLLP9008 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              PRESTCTA             *'           
              DISPLAY '*         FILE STATUS =  ' WRK-FS-PRESTCTA       
                                                 '         *'           
              DISPLAY '************** CLLP9008 *************'           
              CALL 'ILBOABN0'     USING WRK-ABEND.                      
                                                                        
      *---------------------------------------------------------------* 
       1120-99-FIM. EXIT.                                               
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1160-TESTAR-FS-BAIXASAI SECTION.                                 
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-FS-BAIXASAI NOT EQUAL '00'                            
              DISPLAY '************** CLLP9008 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              BAIXASAI             *'           
              DISPLAY '*         FILE STATUS =  ' WRK-FS-BAIXASAI       
                                                 '         *'           
              DISPLAY '************** CLLP9008 *************'           
              CALL 'ILBOABN0'     USING WRK-ABEND.                      
                                                                        
      *---------------------------------------------------------------* 
       1160-99-FIM. EXIT.                                               
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1170-TESTAR-FS-RELATO   SECTION.                                 
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-FS-RELATO   NOT EQUAL '00'                            
              DISPLAY '************** CLLP9008 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              RELATO               *'           
              DISPLAY '*         FILE STATUS =  ' WRK-FS-RELATO         
                                                 '         *'           
              DISPLAY '************** CLLP9008 *************'           
              CALL 'ILBOABN0'     USING WRK-ABEND.                      
                                                                        
      *---------------------------------------------------------------* 
       1170-99-FIM. EXIT.                                               
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1200-LER-BAIXAENT  SECTION.                                      
      *---------------------------------------------------------------* 
                                                                        
           READ BAIXAENT.                                               
                                                                        
           IF WRK-FS-BAIXAENT EQUAL '10'                                
              MOVE  HIGH-VALUES  TO  WRK-CHAVE-BAIXA                    
              GO    TO  1200-99-FIM.                                    
                                                                        
           MOVE   WRK-LEITURA              TO   WRK-OPERACAO.           
           PERFORM   1110-TESTAR-FS-BAIXAENT.                           
                                                                        
           ADD  1  TO  ACU-LIDOS-BAIXAENT.                              
                                                                        
           MOVE  BX-AGENCIA      TO  WRK-CJUNC-DEPDC-BX.                
           MOVE  BX-NUM-CC       TO  WRK-CCTA-CLI-BX.                   
BRQ        MOVE  BX-CARTEIRA     TO  WRK-CCART-BDSCO-BX.                
           MOVE  BX-CONTRATO     TO  WRK-CCONTR-BX.                     
           MOVE  BX-DATA-VENCTO  TO  WRK-DATA-DB2.                      
           MOVE  WRK-DIA-DB2     TO  WRK-DIA.                           
           MOVE  WRK-MES-DB2     TO  WRK-MES.                           
           MOVE  WRK-SEC-DB2     TO  WRK-SEC.                           
           MOVE  WRK-ANO-DB2     TO  WRK-ANO.                           
           MOVE  WRK-DATA-R      TO  WRK-DVCTO-BX.                      
           MOVE  BX-DATA-BAIXA   TO  WRK-DATA-DB2.                      
           MOVE  WRK-DIA-DB2     TO  WRK-DIA.                           
           MOVE  WRK-MES-DB2     TO  WRK-MES.                           
           MOVE  WRK-SEC-DB2     TO  WRK-SEC.                           
           MOVE  WRK-ANO-DB2     TO  WRK-ANO.                           
           MOVE  WRK-DATA-R      TO  WRK-DBAIXA-BX.                     
                                                                        
      *---------------------------------------------------------------* 
       1200-99-FIM. EXIT.                                               
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1300-LER-PRESTCTA  SECTION.                                      
      *---------------------------------------------------------------* 
                                                                        
           READ PRESTCTA.                                               
                                                                        
           IF WRK-FS-PRESTCTA EQUAL '10'                                
              MOVE  HIGH-VALUES  TO  WRK-CHAVE-PREST                    
              GO    TO  1300-99-FIM.                                    
                                                                        
           MOVE   WRK-LEITURA              TO   WRK-OPERACAO.           
           PERFORM   1120-TESTAR-FS-PRESTCTA.                           
                                                                        
           ADD  1  TO  ACU-LIDOS-PRESTCTA.                              
                                                                        
           MOVE  PRES-CJUNC-DEPDC TO  WRK-CJUNC-DEPDC-PS.               
           MOVE  PRES-CCTA-CLI    TO  WRK-CCTA-CLI-PS.                  
BRQ        MOVE  FD-PRES-CCART-BDSCO TO  WRK-CCART-BDSCO-PS.            
           MOVE  PRES-CCONTR      TO  WRK-CCONTR-PS.                    
           MOVE  PRES-DVCTO       TO  WRK-DATA-DB2.                     
           MOVE  WRK-DIA-DB2      TO  WRK-DIA.                          
           MOVE  WRK-MES-DB2      TO  WRK-MES.                          
           MOVE  WRK-SEC-DB2      TO  WRK-SEC.                          
           MOVE  WRK-ANO-DB2      TO  WRK-ANO.                          
           MOVE  WRK-DATA-R       TO  WRK-DVCTO-PS.                     
           MOVE  PRES-DBAIXA-INADP TO  WRK-DATA-DB2.                    
           MOVE  WRK-DIA-DB2       TO  WRK-DIA.                         
           MOVE  WRK-MES-DB2       TO  WRK-MES.                         
           MOVE  WRK-SEC-DB2       TO  WRK-SEC.                         
           MOVE  WRK-ANO-DB2       TO  WRK-ANO.                         
           MOVE  WRK-DATA-R        TO  WRK-DBAIXA-PS.                   
                                                                        
      *---------------------------------------------------------------* 
       1300-99-FIM. EXIT.                                               
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       2000-PROCESSA SECTION.                                           
      *---------------------------------------------------------------* 
                                                                        
           IF  WRK-CHAVE-BAIXA  EQUAL  WRK-CHAVE-PREST                  
               MOVE  'SIM'  TO  WRK-ACHOU                               
               PERFORM 2100-GRAVA-BAIXASAI                              
               PERFORM 2300-IMPRIME-RELATO                              
               PERFORM 1200-LER-BAIXAENT                                
               PERFORM 1300-LER-PRESTCTA                                
           ELSE                                                         
               IF  WRK-CHAVE-BAIXA  LESS  WRK-CHAVE-PREST               
                   MOVE  'NAO'  TO  WRK-ACHOU                           
                   PERFORM 2100-GRAVA-BAIXASAI                          
                   PERFORM 1200-LER-BAIXAENT                            
               ELSE                                                     
                   PERFORM 1300-LER-PRESTCTA.                           
                                                                        
      *---------------------------------------------------------------* 
       2000-99-FIM.  EXIT.                                              
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       2100-GRAVA-BAIXASAI SECTION.                                     
      *---------------------------------------------------------------* 
                                                                        
           IF  WRK-ACHOU  EQUAL  'SIM'                                  
               MOVE BX-TIPO-BAIXA       TO WRK-TIPO-BAIXA               
               IF  BX-TIPO-BAIXA        =   '2'                         
                   MOVE 'T'             TO  BX-TIPO-BAIXA               
               ELSE                                                     
               IF  BX-TIPO-BAIXA        =   '3'                         
                   MOVE 'W'             TO  BX-TIPO-BAIXA               
               ELSE                                                     
               IF  BX-TIPO-BAIXA        =   '4'                         
                   MOVE 'U'             TO  BX-TIPO-BAIXA               
               ELSE                                                     
               IF  BX-TIPO-BAIXA        =   '5'                         
                   MOVE 'X'             TO  BX-TIPO-BAIXA               
               ELSE                                                     
               IF  BX-TIPO-BAIXA        =   '6'                         
                   MOVE 'V'             TO  BX-TIPO-BAIXA               
               ELSE                                                     
               IF  BX-TIPO-BAIXA        =   '7'                         
                   MOVE 'Y'             TO  BX-TIPO-BAIXA.              
                                                                        
           WRITE  REG-BAIXASAI    FROM REG-BAIXAENT.                    
                                                                        
           ADD  1  TO  ACU-GRAVS-BAIXASAI.                              
                                                                        
      *---------------------------------------------------------------* 
       2100-99-FIM.  EXIT.                                              
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       2200-EMITE-DISPLAY     SECTION.                                  
      *---------------------------------------------------------------* 
                                                                        
           DISPLAY '*************** CLLP9008 ***************'           
           DISPLAY '*                                      *'           
           DISPLAY '* ARQUIVO DE PRESTACAO DE CONTAS COM   *'           
           DISPLAY '* REGISTRO   SEM  CORRESPONDENTE  NO   *'           
           DISPLAY '* ARQUIVO DE BAIXAS.                   *'           
           DISPLAY '*                                      *'           
           DISPLAY '* AGENCIA  = ' WRK-CJUNC-DEPDC-PS                   
           DISPLAY '* CONTA    = ' WRK-CCTA-CLI-PS                      
           DISPLAY '* CARTEIRA = ' WRK-CCART-BDSCO-PS                   
           DISPLAY '* CONTRATO = ' WRK-CCONTR-PS                        
           DISPLAY '* DT. VCTO = ' WRK-DVCTO-PS                         
           DISPLAY '*                                      *'           
           DISPLAY '*************** CLLP9008 ***************'.          
                                                                        
      *---------------------------------------------------------------* 
       2200-99-FIM.  EXIT.                                              
      *---------------------------------------------------------------* 
                                                                        
005230                                                                  
005240*---------------------------------------------------------------* 
005250 2300-IMPRIME-RELATO   SECTION.                                   
005260*---------------------------------------------------------------* 
005270                                                                  
005270     IF BX-TIPO-BAIXA      =   WRK-TIPO-BAIXA                     
005270        GO                 TO  2300-99-FIM.                       
005270                                                                  
005280     IF  ACU-LINHA GREATER 60                                     
005290         MOVE  ACU-PAGINA  TO  CAB1-PAGINA                        
005300         ADD   1           TO  ACU-PAGINA                         
005310         WRITE REG-RELATO  FROM  CABEC1                           
005320         PERFORM 1170-TESTAR-FS-RELATO                            
005330         WRITE REG-RELATO  FROM  CABEC1A                          
005340         PERFORM 1170-TESTAR-FS-RELATO                            
005350         WRITE REG-RELATO  FROM  CABEC2                           
005360         PERFORM 1170-TESTAR-FS-RELATO                            
005370         WRITE REG-RELATO  FROM  CABEC3                           
005380         PERFORM 1170-TESTAR-FS-RELATO                            
005390         MOVE  SPACES  TO   REG-RELATO                            
005400         WRITE REG-RELATO                                         
005410         PERFORM 1170-TESTAR-FS-RELATO                            
005420         MOVE  6           TO  ACU-LINHA.                         
005430                                                                  
005440     MOVE  BX-AGENCIA                   TO LINDET-AGENCIA.        
005450     MOVE  BX-NUM-CC                    TO LINDET-CONTA.          
005460     MOVE  BX-CARTEIRA                  TO LINDET-CARTEIRA.       
005470     MOVE  BX-CONTRATO                  TO LINDET-CONTRATO.       
005480     MOVE  BX-TIPO-BAIXA                TO LINDET-TIPO-BAIXA.     
005480     MOVE  WRK-TIPO-BAIXA               TO LINDET-COMPL-TPO-BAIXA.
HEXA       MOVE  FD-BX-DEBITO-CC              TO WRK-BX-DEBITO-TOTAL.   
HEXA       MOVE  WRK-BX-DEBITO-REDUZIDO       TO LINDET-VALOR-BAIXA.    
005490     MOVE  BX-ID                        TO LINDET-ID.             
005500                                                                  
005510     MOVE  BX-DATA-VENCTO              TO  LINDET-DATA-VENCTO.    
005520     INSPECT LINDET-DATA-VENCTO REPLACING ALL '.' BY '/'.         
005530                                                                  
005540     MOVE  BX-DATA-BAIXA               TO  LINDET-DATA-BAIXA.     
005550     INSPECT LINDET-DATA-BAIXA  REPLACING ALL '.' BY '/'.         
005560                                                                  
005570     WRITE  REG-RELATO  FROM  LINDET.                             
005580     PERFORM 1170-TESTAR-FS-RELATO.                               
005590                                                                  
005600     ADD  1  TO  ACU-LINHA.                                       
005610                                                                  
005620*---------------------------------------------------------------* 
005630 2300-99-FIM. EXIT.                                               
005640*---------------------------------------------------------------* 
005650                                                                  
      *---------------------------------------------------------------* 
       3000-EMITE-DISPLAY-FINAL  SECTION.                               
      *---------------------------------------------------------------* 
                                                                        
           DISPLAY '*************** CLLP9008 ***************'           
           DISPLAY '*                                      *'           
           DISPLAY '* LIDOS EM BAIXAENT = ' ACU-LIDOS-BAIXAENT          
           DISPLAY '* LIDOS EM PRESTCTA = ' ACU-LIDOS-PRESTCTA          
           DISPLAY '* GRAVS EM BAIXASAI = ' ACU-GRAVS-BAIXASAI          
           DISPLAY '*                                      *'           
           DISPLAY '*************** CLLP9008 ***************'.          
                                                                        
      *---------------------------------------------------------------* 
       3000-99-FIM.  EXIT.                                              
      *---------------------------------------------------------------* 

      *================================================================*
       IDENTIFICATION                  DIVISION.                        
      *================================================================*
       PROGRAM-ID. CLLP1999.                                            
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
      *     OBJETIVO     : ACERTA VALOR CONTABIL BRUTO                  
      *                                                                 
BRQ141*----------------------------------------------------------------*
BRQ141*    ANALISTA    : TALLES AUGUSTO - BRQ                          *
BRQ141*    DATA        :    09/2012                                    *
BRQ141*    OBJETIVO    : PROJETO CARTEIRA ALFANUMERICA - BRQ141        *
BRQ141*                  CONVERSAO DE CARTEIRAS.                       *
BRQ141*----------------------------------------------------------------*
      *================================================================*
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
           SELECT CADANT ASSIGN        TO  UT-S-CADANT
           FILE STATUS                 IS  WRK-FS-CADANT.
           SELECT CADATU ASSIGN        TO  UT-S-CADATU                  
           FILE STATUS                 IS  WRK-FS-CADATU.               
           SELECT CADACER ASSIGN       TO  UT-S-CADACER                 
           FILE STATUS                 IS  WRK-FS-CADACER.              
           SELECT RELATO ASSIGN        TO  UT-S-RELATO                  
           FILE STATUS                 IS  WRK-FS-RELATO.               
      *================================================================*
       DATA                            DIVISION.                        
      *================================================================*
       FILE                            SECTION.                         
      *----------------------------------------------------------------*
      *      INPUT  : ARQUIVO CADATU                                   *
      *               ORG.           - LRECL = 600                     *
      *----------------------------------------------------------------*

       FD  CADANT
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.
       01  FD-REG-CADANT               PIC X(600).

       FD  CADATU                                                       
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
       01  FD-REG-CADATU               PIC X(600).                      
      *----------------------------------------------------------------*
      *      OUTPUT : ARQUIVO CADACER                                  *
      *               ORG.           - LRECL = 600                     *
      *----------------------------------------------------------------*
       FD  CADACER                                                      
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
       01  FD-REG-CADACER              PIC X(600).                      
      *----------------------------------------------------------------*
      *      OUTPUT : ARQUIVO RELATO                                   *
      *               ORG.           - LRECL = 133                     *
      *----------------------------------------------------------------*
       FD  RELATO                                                       
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
       01  FD-REG-RELATO               PIC X(133).                      
                                                                        
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
       77  WRK-FS-CADANT               PIC  X(002)         VALUE SPACES.
       77  WRK-FS-CADATU               PIC  X(002)         VALUE SPACES.
       77  WRK-FS-CADACER              PIC  X(002)         VALUE SPACES.
       77  WRK-FS-RELATO               PIC  X(002)         VALUE SPACES.
       77  WRK-ABERTURA                PIC  X(013)         VALUE        
           ' NA ABERTURA'.                                              
       77  WRK-GRAVACAO                PIC  X(013)         VALUE        
           ' NA GRAVACAO'.                                              
       77  WRK-FECHAMENTO              PIC  X(013)         VALUE        
           'NO FECHAMENTO'.                                             
                                                                        
       01  WRK-WK                      PIC  X(600)         VALUE SPACES.
       01  WRK-PAD10                   PIC  BZ.ZZZ.ZZZ     VALUE SPACES.
CPMCAC 01  WRK-PAD10-R                 REDEFINES  WRK-PAD10             
CPMCAC                                 PIC  X(010).                     
       01  WRK-PAD20                   PIC  BZZZZ.ZZZ.ZZZ.ZZ9,99        
                                                           VALUE SPACES.
CPMCAC 01  WRK-PAD20-R                 REDEFINES WRK-PAD20              
                                       PIC  X(020).                     
       01  WRK-FOL                     PIC  9(003) COMP-3  VALUE 55.    
       01  WRK-VRDIFER                 PIC  9(015) COMP-3  VALUE ZEROS. 
CPMCAC 01  WRK-VRDIFER-R               REDEFINES  WRK-VRDIFER           
                                       PIC  9(013)V99 COMP-3.           
       01  WRK-TVRRAP                  PIC  9(015) COMP-3  VALUE ZEROS. 
CPMCAC 01  WRK-TVRRAP-R                REDEFINES  WRK-TVRRAP            
CPMCAC                                 PIC  9(013)V99 COMP-3.           
       01  WRK-TVRCONT1                PIC  9(015) COMP-3  VALUE ZEROS. 
CPMCAC 01  WRK-TVRCONT1-R              REDEFINES  WRK-TVRCONT1          
CPMCAC                                 PIC  9(013)V99 COMP-3.           
       01  WRK-TVRCONT2                PIC  9(015) COMP-3  VALUE ZEROS. 
CPMCAC 01  WRK-TVRCONT2-R              REDEFINES  WRK-TVRCONT2          
CPMCAC                                 PIC  9(013)V99 COMP-3.           
       01  WRK-TVRDIFER                PIC  9(015) COMP-3  VALUE ZEROS. 
       01  WRK-TVRDIFER-R              REDEFINES  WRK-TVRDIFER          
                                       PIC  9(013)V99 COMP-3.           
       01  WRK-QTDELE                  PIC  9(009) COMP-3  VALUE ZEROS. 
       01  WRK-QTDEGR                  PIC  9(009) COMP-3  VALUE ZEROS. 
       01  WRK-QTDEALT                 PIC  9(009) COMP-3  VALUE ZEROS. 
       01  WRK-PAD12                   PIC BZZZ.ZZZ.ZZ9    VALUE SPACES.
CPMCAC 01  WRK-PAD12-R                 REDEFINES  WRK-PAD12             
CPMCAC                                 PIC  X(012).                     
                                                                        
       01  WRK-CAB0.                                                    
           05  FILLER                  PIC  X(001)         VALUE X'89'. 
           05  FILLER                  PIC  X(132)         VALUE ' '.   
                                                                        
       01  WRK-CAB1.                                                    
           05  FILLER                  PIC  X(001)         VALUE X'11'. 
           05  FILLER                  PIC  X(015)         VALUE        
               '*CLLP1999*'.                                            
           05  FILLER                  PIC  X(117)         VALUE        
               ' ACERTOS NAS PENDENCIAS TRANSFERIDAS PARA CL2'.         
                                                                        
       01  WRK-CAB2.                                                    
           05  FILLER                  PIC  X(001)         VALUE X'11'. 
           05  FILLER                  PIC  X(004)         VALUE 'EMPR'.
           05  FILLER                  PIC  X(002)         VALUE ' '.   
           05  FILLER                  PIC  X(004)         VALUE 'AGEN'.
           05  FILLER                  PIC  X(010)         VALUE        
               '       C/C'.                                            
           05  FILLER                  PIC  X(002)         VALUE ' '.   
           05  FILLER                  PIC  X(004)         VALUE        
               'CART'.                                                  
           05  FILLER                  PIC  X(010)         VALUE        
               '  CONTRATO'.                                            
           05  FILLER                  PIC  X(012)         VALUE        
               '  VENCTO'.                                              
           05  FILLER                  PIC  X(020)         VALUE        
               '    VR CONT ANTERIOR'.                                  
           05  FILLER                  PIC  X(020)         VALUE        
               '        VALOR DO RAP'.                                  
           05  FILLER                  PIC  X(020)         VALUE        
               '       VR CONT ATUAL'.                                  
           05  FILLER                  PIC  X(020)         VALUE        
               '        VR RAP ATUAL'.                                  
           05  FILLER                  PIC  X(010)         VALUE ' '.   
                                                                        
       01  WRK-LDET.                                                    
           05  FILLER                  PIC  X(001)         VALUE X'09'. 
           05  WRK-LEMP                PIC  X(004)         VALUE SPACES.
           05  WRK-LEMP-REDCPMACT      REDEFINES  WRK-LEMP              
                                       PIC  9(004).                     
           05  FILLER                  PIC  X(002)         VALUE ' '.   
           05  WRK-LAG                 PIC  X(004)         VALUE SPACES.
           05  WRK-LAG-REDCPMACT       REDEFINES  WRK-LAG               
                                       PIC  9(004).                     
           05  WRK-LCC                 PIC  X(010)         VALUE SPACES.
           05  FILLER                  PIC  X(003)         VALUE ' '.   
BRQ141     05  WRK-LCART               PIC  X(003)         VALUE SPACES.
           05  WRK-LCONTR              PIC  X(010)         VALUE SPACES.
           05  WRK-LVCTO               PIC  BZZZ/ZZ/ZZZZ   VALUE SPACES.
           05  WRK-LVRCONT1            PIC  X(020)         VALUE SPACES.
           05  WRK-LVRRAP              PIC  X(020)         VALUE SPACES.
           05  WRK-LVRCONT2            PIC  X(020)         VALUE SPACES.
           05  WRK-LVRDIFER            PIC  X(020)         VALUE SPACES.
           05  FILLER                  PIC  X(010)         VALUE ' '.   
                                                                        
BRQ141 01  WRK-CARTEIRA-AUX            PIC  X(003)         VALUE SPACES.
CPMCAC 01  WRK-9-3-DESC                PIC  9(003)         VALUE ZEROS. 
CPMCAC 01  WRK-9-3-DESC-R              REDEFINES   WRK-9-3-DESC         
                                       PIC  X(003).                     
                                                                        
CPMCAC 01  WRK-X-2                     PIC  X(002)         VALUE SPACES.
       01  WRK-X-2-R                   REDEFINES  WRK-X-2               
                                       PIC  9(003) COMP-3.              
CPMCAC 01  WRK-X-3                     PIC  X(003)         VALUE SPACES.
CPMCAC 01  WRK-X-3-R                   REDEFINES  WRK-X-3               
                                       PIC  9(005) COMP-3.              
                                                                        
CPMCAC 01  WRK-X-4                     PIC  X(004)         VALUE SPACES.
CPMCAC 01  WRK-X-4-R                   REDEFINES  WRK-X-4               
                                       PIC  9(007) COMP-3.              
                                                                        
                                                                        
CPMCAC 01  WRK-S9-5                    PIC  9(005) COMP-3  VALUE ZEROS. 
CPMCAC 01  WRK-S9-3-R                  REDEFINES  WRK-S9-5              
CPMCAC                                 PIC  X(003).                     
                                                                        
       01  WRK-9-5-CS                  PIC  9(005)         VALUE ZEROS. 
       01  FILLER                      REDEFINES  WRK-9-5-CS.           
           05  FILLER                  PIC  X(001).                     
           05  WRK-9-4-R               PIC  9(004).                     
                                                                        
CPMCAC 01  WRK-S9-9                    PIC  9(009) COMP-3  VALUE ZEROS. 
CPMCAC 01  WRK-S9-5-R                  REDEFINES WRK-S9-9               
                                       PIC  X(005).                     
                                                                        
CPMCAC 01  WRK-9-9-COMP                PIC S9(009) COMP-3  VALUE ZEROS. 
CPMCAC 01  WRK-9-9-COMP-R              REDEFINES  WRK-9-9-COMP          
                                       PIC  X(005).                     
                                                                        
CPMCAC 01  WRK-9-13V99                 PIC  9(013)V99                   
                                                   COMP-3  VALUE ZEROS. 
CPMCAC 01  WRK-9-8-R                   REDEFINES  WRK-9-13V99           
CPMCAC                                 PIC  X(008).                     
                                                                        
CPMCAC 01  WRK-9-15                    PIC S9(015) COMP-3  VALUE ZEROS. 
CPMCAC 01  WRK-9-8-R1                  REDEFINES  WRK-9-15              
CPMCAC                                 PIC  X(008).                     
                                                                        
CPMCAC 01  WRK-CS9-9                   PIC +9(009)         VALUE ZEROS. 
CPMCAC 01  FILLER                      REDEFINES  WRK-CS9-9.            
CPMCAC     05  FILLER                  PIC  X(001).                     
CPMCAC     05  WRK-SS9-9               PIC  9(009).                     
                                                                        
CPMCAC 01  WRK-CADANT                  PIC  X(008)         VALUE        
CPMCAC     'CADANT'.                                                    
                                                                        
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
       01  FILLER                     PIC  X(050)          VALUE        
           'FIM DA WORKING STORAGE SECTION '.                           
      *----------------------------------------------------------------*
                                                                        
       COPY 'I#BRAD7C'.
                                                                        
      *================================================================*
       PROCEDURE                       DIVISION.                        
      *================================================================*
           OPEN  INPUT CADANT
                 OUTPUT CADATU
                        CADACER                                         
                        RELATO                                          
           MOVE  WRK-ABERTURA          TO  WRK-OPERACAO                 
           PERFORM 9000-TESTAR-FILE-STATUS THRU 9000-99-FIM.            
      *                                                                 
      *----------------------------------------------------------------*
       0010-LER.                                                        
      *----------------------------------------------------------------*
      *    CALL 'BRAD0300'          USING  WRK-CADANT
      *                                    WRK-WK

           READ    CADANT INTO WRK-WK
           IF (WRK-FS-CADANT EQUAL '10')
               GO TO 0200-FIM
           END-IF.


      *   MOVE FD-REG-CADACER TO WRK-WK
           IF  RETURN-CODE          EQUAL  4                            
               GO TO 0200-FIM                                           
           END-IF                                                       
      *                                                                 
           ADD 1                       TO  WRK-QTDELE                   
           IF  WRK-WK(600:1)    NOT EQUAL  '1'                          
               GO TO 0190-SOGRAVA                                       
           END-IF                                                       
                                                                        
CPMCAC     MOVE WRK-WK(1:3)            TO  WRK-S9-3-R                   
CPMCAC     IF  WRK-S9-5         NOT EQUAL  4120                         
               GO TO 0190-SOGRAVA                                       
           END-IF                                                       
CPMCAC     MOVE WRK-WK(55:5)           TO  WRK-S9-5-R                   
CPMCAC     IF  WRK-S9-9         NOT EQUAL  27122002                     
               GO TO 0190-SOGRAVA                                       
           END-IF                                                       
BRQ141     MOVE  WRK-WK(33:3)          TO  WRK-CARTEIRA-AUX             
BRQ141     IF  WRK-CARTEIRA-AUX  NOT EQUAL  '788'                       
               GO TO 0190-SOGRAVA                                       
           END-IF                                                       
CPMCAC     MOVE  WRK-WK(4:3)           TO  WRK-X-3                      
CPMCAC     IF  WRK-X-3-R        NOT EQUAL  64
               GO TO 0190-SOGRAVA
           END-IF
CPMCAC     MOVE  WRK-WK(16:4)          TO  WRK-X-4                      
CPMCAC     IF  WRK-X-4-R            EQUAL  8839                         
               GO TO 0020-ACERTAR                                       
           END-IF                                                       
CPMCAC     MOVE  WRK-WK(16:4)          TO  WRK-X-4                      
CPMCAC     IF  WRK-X-4-R            EQUAL  73468                        
               GO TO 0020-ACERTAR                                       
           END-IF                                                       
CPMCAC     MOVE  WRK-WK(16:4)          TO  WRK-X-4                      
CPMCAC     IF  WRK-X-4-R        NOT EQUAL  201200                       
               GO TO 0190-SOGRAVA                                       
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       0020-ACERTAR.                                                    
      *----------------------------------------------------------------*
           ADD  1                      TO  WRK-QTDEALT                  
           MOVE  WRK-WK(1:3)           TO  WRK-X-3                      
CPMCAC     MOVE  WRK-X-3-R             TO  WRK-9-5-CS                   
CPMCAC     MOVE  WRK-9-4-R             TO  WRK-LEMP-REDCPMACT           
CPMCAC     MOVE  WRK-WK(4:3)           TO  WRK-X-3                      
CPMCAC     MOVE  WRK-X-3-R             TO  WRK-9-5-CS                   
           MOVE  WRK-9-4-R             TO  WRK-LAG-REDCPMACT            
CPMCAC     MOVE  WRK-WK(16:4)          TO  WRK-X-4                      
           MOVE  WRK-X-4-R             TO  WRK-PAD10                    
           MOVE  WRK-PAD10-R           TO  WRK-LCC                      
                                                                        
      *    ACERTA A CARTEIRA PARA 787                                   
BRQ141     MOVE  '787'                 TO  WRK-WK(33:3)                 
                                                                        
BRQ141     MOVE  WRK-WK(33:3)          TO  WRK-LCART                    
           MOVE  WRK-WK(29:4)          TO  WRK-X-4                      
           MOVE  WRK-X-4-R             TO  WRK-PAD10                    
           MOVE  WRK-PAD10-R           TO  WRK-LCONTR                   
           MOVE  WRK-WK(50:5)          TO  WRK-S9-5-R                   
           IF  WRK-S9-9         NOT EQUAL  27122002                     
               GO TO 0030-EDVCTO                                        
           END-IF                                                       
                                                                        
      *    ACERTA VENCTO 27/12 P/ 01/10/2018                            
CPMCAC     MOVE 01102018               TO  WRK-9-9-COMP                 
CPMCAC     MOVE WRK-9-9-COMP-R         TO  WRK-WK(50:5).                
                                                                        
      *----------------------------------------------------------------*
       0030-EDVCTO.                                                     
      *----------------------------------------------------------------*
           MOVE  WRK-WK(50:5)          TO  WRK-9-9-COMP-R               
           MOVE  WRK-9-9-COMP          TO  WRK-CS9-9                    
           MOVE  WRK-SS9-9             TO  WRK-LVCTO                    
           MOVE  WRK-WK(177:8)         TO  WRK-9-8-R                    
           MOVE  WRK-9-13V99           TO  WRK-PAD20                    
           MOVE  WRK-PAD20-R           TO  WRK-LVRRAP                   
           MOVE  WRK-WK(177:8)         TO  WRK-9-8-R                    
           ADD  WRK-9-13V99            TO  WRK-TVRRAP-R                 
           MOVE  WRK-WK(113:8)         TO  WRK-9-8-R                    
           MOVE  WRK-9-13V99           TO  WRK-PAD20                    
           MOVE  WRK-PAD20-R           TO  WRK-LVRCONT1                 
           MOVE  WRK-WK(113:8)         TO  WRK-9-8-R                    
           ADD  WRK-9-13V99            TO  WRK-TVRCONT1-R               
           MOVE  WRK-WK(29:4)          TO  WRK-X-4                      
           IF  WRK-X-4-R        NOT EQUAL  130536                       
               GO TO 0040-VE130537                                      
           END-IF                                                       
                                                                        
      *    ATUALIZA VR RAP                                              
           MOVE 163480420              TO  WRK-9-15                     
           MOVE WRK-9-8-R1             TO  WRK-WK(177:8)                
                                                                        
      *    ATUALIZA VR CONTABIL                                         
           MOVE 298440058              TO  WRK-9-15                     
           MOVE WRK-9-8-R1             TO  WRK-WK(113:8)                
                                                                        
           GO TO 0170-EDVRCON.                                          
      *----------------------------------------------------------------*
       0040-VE130537.                                                   
      *----------------------------------------------------------------*
           MOVE  WRK-WK(29:4)          TO  WRK-X-4                      
           IF  WRK-X-4-R        NOT EQUAL  130537                       
               GO TO 0050-VE130538                                      
           END-IF                                                       
                                                                        
      *    ATUALIZA VR RAP                                              
           MOVE 637369110              TO  WRK-9-15                     
           MOVE WRK-9-8-R1             TO  WRK-WK(177:8)                
                                                                        
      *    ATUALIZA VR CONTABIL                                         
           MOVE 1192072982             TO  WRK-9-15                     
           MOVE WRK-9-8-R1             TO  WRK-WK(113:8)                
           GO TO 0170-EDVRCON.                                          
                                                                        
      *----------------------------------------------------------------*
       0050-VE130538.                                                   
      *----------------------------------------------------------------*
           MOVE  WRK-WK(29:4)          TO  WRK-X-4                      
           IF  WRK-X-4-R        NOT EQUAL  130538                       
               GO TO 0060-VE130539                                      
           END-IF                                                       
                                                                        
      *    ATUALIZA VR RAP                                              
           MOVE 192656971              TO  WRK-9-15                     
           MOVE WRK-9-8-R1             TO  WRK-WK(177:8)                
                                                                        
      *    ATUALIZA VR CONTABIL                                         
           MOVE 366214092              TO  WRK-9-15                     
           MOVE WRK-9-8-R1             TO  WRK-WK(113:8)                
           GO TO 0170-EDVRCON.                                          
      *----------------------------------------------------------------*
       0060-VE130539.                                                   
      *----------------------------------------------------------------*
           MOVE  WRK-WK(29:4)          TO  WRK-X-4                      
           IF  WRK-X-4-R        NOT EQUAL  130539                       
               GO TO 0070-VE130540                                      
           END-IF                                                       
                                                                        
      *    ATUALIZA VR RAP                                              
           MOVE 2473850306             TO  WRK-9-15                     
           MOVE WRK-9-8-R1             TO  WRK-WK(177:8)                
                                                                        
      *    ATUALIZA VR CONTABIL                                         
           MOVE 4612227544             TO  WRK-9-15                     
           MOVE WRK-9-8-R1             TO  WRK-WK(113:8)                
           GO TO 0170-EDVRCON.                                          
                                                                        
      *----------------------------------------------------------------*
       0070-VE130540.                                                   
      *----------------------------------------------------------------*
           MOVE  WRK-WK(29:4)          TO  WRK-X-4                      
           IF  WRK-X-4-R        NOT EQUAL  130540                       
               GO TO 0080-VE130544                                      
           END-IF                                                       
                                                                        
      *    ATUALIZA VR RAP                                              
           MOVE 157447449              TO  WRK-9-15                     
           MOVE WRK-9-8-R1             TO  WRK-WK(177:8)                
                                                                        
      *    ATUALIZA VR CONTABIL                                         
           MOVE 300836646              TO  WRK-9-15                     
           MOVE WRK-9-8-R1             TO  WRK-WK(113:8)                
           GO TO 0170-EDVRCON.                                          
                                                                        
      *----------------------------------------------------------------*
       0080-VE130544.                                                   
      *----------------------------------------------------------------*
           MOVE  WRK-WK(29:4)          TO  WRK-X-4                      
           IF  WRK-X-4-R        NOT EQUAL  130544                       
               GO TO 0090-VE130545                                      
           END-IF                                                       
                                                                        
      *    ATUALIZA VR RAP                                              
           MOVE 424625028              TO  WRK-9-15                     
           MOVE WRK-9-8-R1             TO  WRK-WK(177:8)                
                                                                        
      *    ATUALIZA VR CONTABIL                                         
           MOVE 751544739              TO  WRK-9-15                     
           MOVE WRK-9-8-R1             TO  WRK-WK(113:8)                
           GO TO 0170-EDVRCON.                                          
                                                                        
      *----------------------------------------------------------------*
       0090-VE130545.                                                   
      *----------------------------------------------------------------*
           MOVE  WRK-WK(29:4)          TO  WRK-X-4                      
           IF  WRK-X-4-R        NOT EQUAL  130545                       
               GO TO 0100-VE130546                                      
           END-IF                                                       
                                                                        
      *    ATUALIZA VR RAP                                              
           MOVE 1559776858             TO  WRK-9-15                     
           MOVE WRK-9-8-R1             TO  WRK-WK(177:8)                
                                                                        
      *    ATUALIZA VR CONTABIL                                         
           MOVE 2740552362             TO  WRK-9-15                     
           MOVE WRK-9-8-R1             TO  WRK-WK(113:8)                
           GO TO 0170-EDVRCON.                                          
                                                                        
      *----------------------------------------------------------------*
       0100-VE130546.                                                   
      *----------------------------------------------------------------*
           MOVE  WRK-WK(29:4)          TO  WRK-X-4                      
           IF  WRK-X-4-R        NOT EQUAL  130546                       
               GO TO 0110-VE130547                                      
           END-IF                                                       
                                                                        
      *    ATUALIZA VR RAP                                              
           MOVE 15795301               TO  WRK-9-15                     
           MOVE WRK-9-8-R1             TO  WRK-WK(177:8)                
                                                                        
      *    ATUALIZA VR CONTABIL                                         
           MOVE 29653095               TO  WRK-9-15                     
           MOVE WRK-9-8-R1             TO  WRK-WK(113:8)                
           GO TO 0170-EDVRCON.                                          
                                                                        
      *----------------------------------------------------------------*
       0110-VE130547.                                                   
      *----------------------------------------------------------------*
           MOVE  WRK-WK(29:4)          TO  WRK-X-4                      
           IF  WRK-X-4-R        NOT EQUAL  130547                       
               GO TO 0120-VE130548                                      
           END-IF                                                       
                                                                        
      *    ATUALIZA VR RAP                                              
           MOVE 345891716              TO  WRK-9-15                     
           MOVE WRK-9-8-R1             TO  WRK-WK(177:8)                
                                                                        
      *    ATUALIZA VR CONTABIL                                         
           MOVE 613439801              TO  WRK-9-15                     
           MOVE WRK-9-8-R1             TO  WRK-WK(113:8)                
           GO TO 0170-EDVRCON.                                          
                                                                        
      *----------------------------------------------------------------*
       0120-VE130548.                                                   
      *----------------------------------------------------------------*
           MOVE  WRK-WK(29:4)          TO  WRK-X-4                      
           IF  WRK-X-4-R        NOT EQUAL  130548                       
               GO TO 0130-VE130541                                      
           END-IF                                                       
                                                                        
      *    ATUALIZA VR RAP                                              
           MOVE 180248344              TO  WRK-9-15                     
           MOVE WRK-9-8-R1             TO  WRK-WK(177:8)                
                                                                        
      *    ATUALIZA VR CONTABIL                                         
           MOVE 323236238              TO  WRK-9-15                     
           MOVE WRK-9-8-R1             TO  WRK-WK(113:8)                
           GO TO 0170-EDVRCON.                                          
                                                                        
      *----------------------------------------------------------------*
       0130-VE130541.                                                   
      *----------------------------------------------------------------*
           MOVE  WRK-WK(29:4)          TO  WRK-X-4                      
           IF  WRK-X-4-R        NOT EQUAL  130541                       
               GO TO 0140-VE130542                                      
           END-IF                                                       
                                                                        
      *    ATUALIZA VR RAP                                              
           MOVE 235590269              TO  WRK-9-15                     
           MOVE WRK-9-8-R1             TO  WRK-WK(177:8)                
                                                                        
      *    ATUALIZA VR CONTABIL                                         
           MOVE 420212319              TO  WRK-9-15                     
           MOVE WRK-9-8-R1             TO  WRK-WK(113:8)                
           GO TO 0170-EDVRCON.                                          
                                                                        
      *----------------------------------------------------------------*
       0140-VE130542.                                                   
      *----------------------------------------------------------------*
           MOVE  WRK-WK(29:4)          TO  WRK-X-4                      
           IF  WRK-X-4-R        NOT EQUAL  130542                       
               GO TO 0150-VE130543                                      
           END-IF                                                       
                                                                        
      *    ATUALIZA VR RAP                                              
           MOVE 66951338               TO  WRK-9-15                     
           MOVE WRK-9-8-R1             TO  WRK-WK(177:8)                
                                                                        
      *    ATUALIZA VR CONTABIL                                         
           MOVE 123905536              TO  WRK-9-15                     
           MOVE WRK-9-8-R1             TO  WRK-WK(113:8)                
           GO TO 0170-EDVRCON.                                          
                                                                        
      *----------------------------------------------------------------*
       0150-VE130543.                                                   
      *----------------------------------------------------------------*
           MOVE  WRK-WK(29:4)          TO  WRK-X-4                      
           IF  WRK-X-4-R        NOT EQUAL  130543                       
               GO TO 0160-VE130560                                      
           END-IF                                                       
                                                                        
      *    ATUALIZA VR RAP                                              
           MOVE 813295151              TO  WRK-9-15                     
           MOVE WRK-9-8-R1             TO  WRK-WK(177:8)                
                                                                        
      *    ATUALIZA VR CONTABIL                                         
           MOVE 1432769351             TO  WRK-9-15                     
           MOVE WRK-9-8-R1             TO  WRK-WK(113:8)                
           GO TO 0170-EDVRCON.                                          
      *    ATUALIZA VR RAP                                              
      *----------------------------------------------------------------*
       0160-VE130560.                                                   
      *----------------------------------------------------------------*
           MOVE 11861928               TO  WRK-9-15                     
           MOVE WRK-9-8-R1             TO  WRK-WK(177:8)                
                                                                        
      *    ATUALIZA VR CONTABIL                                         
           MOVE 22267417               TO  WRK-9-15                     
           MOVE WRK-9-8-R1             TO  WRK-WK(113:8).               
      *----------------------------------------------------------------*
       0170-EDVRCON.                                                    
      *----------------------------------------------------------------*
           MOVE  WRK-WK(113:8)         TO  WRK-9-8-R                    
           MOVE  WRK-9-13V99           TO  WRK-VRDIFER-R                
           MOVE  WRK-WK(113:8)         TO  WRK-9-8-R                    
           MOVE  WRK-9-13V99           TO  WRK-PAD20                    
           MOVE  WRK-PAD20-R           TO  WRK-LVRCONT2                 
           MOVE  WRK-WK(113:8)         TO  WRK-9-8-R                    
           ADD  WRK-9-13V99            TO  WRK-TVRCONT2-R               
           MOVE  WRK-WK(177:8)         TO  WRK-9-8-R                    
           MOVE  WRK-9-13V99           TO  WRK-PAD20                    
           MOVE  WRK-PAD20-R           TO  WRK-LVRDIFER                 
           MOVE  WRK-WK(177:8)         TO  WRK-9-8-R                    
           ADD  WRK-9-13V99            TO  WRK-TVRDIFER-R               
           IF  WRK-FOL               LESS  55                           
               GO TO 0180-NQB                                           
           END-IF                                                       
           MOVE ZEROS                  TO  WRK-FOL                      
           WRITE FD-REG-RELATO       FROM  WRK-CAB0                     
           MOVE  WRK-GRAVACAO          TO  WRK-OPERACAO                 
           PERFORM 9040-TESTAR-FS-RELATO THRU 9040-99-FIM               
      *                                                                 
           WRITE FD-REG-RELATO       FROM  WRK-CAB1                     
           MOVE  WRK-GRAVACAO          TO  WRK-OPERACAO                 
           PERFORM 9040-TESTAR-FS-RELATO THRU 9040-99-FIM               
      *                                                                 
           WRITE FD-REG-RELATO       FROM  WRK-CAB2                     
           MOVE  WRK-GRAVACAO          TO  WRK-OPERACAO                 
           PERFORM 9040-TESTAR-FS-RELATO THRU 9040-99-FIM.              
      *                                                                 
      *----------------------------------------------------------------*
       0180-NQB.                                                        
      *----------------------------------------------------------------*
           ADD 1                       TO  WRK-FOL                      
           WRITE FD-REG-RELATO       FROM  WRK-LDET                     
           MOVE  WRK-GRAVACAO          TO  WRK-OPERACAO                 
           PERFORM 9040-TESTAR-FS-RELATO THRU 9040-99-FIM               
      *                                                                 
           WRITE FD-REG-CADACER      FROM  WRK-WK                       
           MOVE  WRK-GRAVACAO          TO  WRK-OPERACAO                 
           PERFORM 9030-TESTAR-FS-CADACER THRU 9030-99-FIM.             
      *                                                                 
      *                                                                 
      *----------------------------------------------------------------*
       0190-SOGRAVA.                                                    
      *----------------------------------------------------------------*
           ADD 1                       TO  WRK-QTDEGR                   
           WRITE FD-REG-CADATU       FROM  WRK-WK                       
           MOVE  WRK-GRAVACAO          TO  WRK-OPERACAO                 
           PERFORM 9020-TESTAR-FS-CADATU THRU 9020-99-FIM               
      *                                                                 
           GO TO 0010-LER.                                              
                                                                        
      *----------------------------------------------------------------*
       0200-FIM.                                                        
      *----------------------------------------------------------------*
      *                                                                '
           MOVE  ' '                   TO  WRK-LDET(2:132)              
CPMCAC     MOVE  WRK-TVRCONT1-R        TO  WRK-PAD20                    
CPMCAC     MOVE  WRK-PAD20-R           TO  WRK-LVRCONT1                 
CPMCAC     MOVE  WRK-TVRCONT2-R        TO  WRK-PAD20                    
CPMCAC     MOVE  WRK-PAD20-R           TO  WRK-LVRCONT2                 
CPMCAC     MOVE  WRK-TVRRAP-R          TO  WRK-PAD20                    
CPMCAC     MOVE  WRK-PAD20-R           TO  WRK-LVRRAP                   
CPMCAC     MOVE  WRK-TVRDIFER-R        TO  WRK-PAD20                    
CPMCAC     MOVE  WRK-PAD20-R           TO  WRK-LVRDIFER                 
           WRITE FD-REG-RELATO       FROM  WRK-LDET                     
           MOVE  WRK-GRAVACAO          TO  WRK-OPERACAO                 
           PERFORM 9040-TESTAR-FS-RELATO THRU 9040-99-FIM               
      *                                                  DE REGISTROS -'
           MOVE  'TOTAIS DE REGISTROS -'                                
                                       TO  WRK-LDET(2:132)              
           MOVE  WRK-QTDELE            TO  WRK-PAD12                    
           MOVE  WRK-PAD12-R           TO  WRK-LDET(23:12)              
           MOVE  '-LIDOS'              TO  WRK-LDET(35:8)               
           MOVE  WRK-QTDEALT           TO  WRK-PAD12                    
           MOVE  WRK-PAD12-R           TO  WRK-LDET(43:12)              
           MOVE  '-ALTERADOS'          TO  WRK-LDET(55:10)              
           MOVE  WRK-QTDEGR            TO  WRK-PAD12                    
           MOVE  WRK-PAD12-R           TO  WRK-LDET(65:12)              
           MOVE  '-GRAVADOS'           TO  WRK-LDET(77:10)              
           WRITE FD-REG-RELATO       FROM  WRK-LDET                     
           MOVE  WRK-GRAVACAO          TO  WRK-OPERACAO                 
           PERFORM 9040-TESTAR-FS-RELATO THRU 9040-99-FIM               
      *                                                                 
           CLOSE CADATU                                                 
                 CADACER                                                
                 RELATO                                                 
           MOVE  WRK-FECHAMENTO        TO  WRK-OPERACAO                 
           PERFORM 9000-TESTAR-FILE-STATUS THRU 9000-99-FIM             
      *                                                                 
           GOBACK.                                                      
      *                                                                 
      *----------------------------------------------------------------*
       9000-TESTAR-FILE-STATUS.                                         
      *----------------------------------------------------------------*
           PERFORM 9020-TESTAR-FS-CADATU  THRU 9020-99-FIM              
           PERFORM 9030-TESTAR-FS-CADACER THRU 9030-99-FIM              
           PERFORM 9040-TESTAR-FS-RELATO  THRU 9040-99-FIM.             
      *                                                                 
      *----------------------------------------------------------------*
       9000-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
      *                                                                 
      *----------------------------------------------------------------*
       9020-TESTAR-FS-CADATU.                                           
      *----------------------------------------------------------------*
                                                                        
           IF (WRK-FS-CADATU    NOT EQUAL  '00')                        
               MOVE 'CADATU'           TO  WRK-NOME-ARQ                 
               MOVE WRK-FS-CADATU      TO  WRK-FILE-STATUS              
               MOVE WRK-ERRO-BRAD7100  TO  ERR-TEXTO                    
               PERFORM 9999-PROCESSAR-ROTINA-ERRO                       
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       9020-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       9030-TESTAR-FS-CADACER.                                          
      *----------------------------------------------------------------*
                                                                        
           IF (WRK-FS-CADACER   NOT EQUAL  '00')                        
               MOVE 'CADACER'          TO  WRK-NOME-ARQ                 
               MOVE WRK-FS-CADACER     TO  WRK-FILE-STATUS              
               MOVE WRK-ERRO-BRAD7100  TO  ERR-TEXTO                    
               PERFORM 9999-PROCESSAR-ROTINA-ERRO                       
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       9030-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       9040-TESTAR-FS-RELATO.                                           
      *----------------------------------------------------------------*
                                                                        
           IF (WRK-FS-RELATO     NOT EQUAL  '00')                       
               MOVE 'RELATO'            TO  WRK-NOME-ARQ                
               MOVE WRK-FS-RELATO       TO  WRK-FILE-STATUS             
               MOVE WRK-ERRO-BRAD7100   TO  ERR-TEXTO                   
               PERFORM 9999-PROCESSAR-ROTINA-ERRO                       
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       9040-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO.                                      
      *----------------------------------------------------------------*
           MOVE 'CLLP1999'             TO  ERR-PGM.                     
           MOVE 'APL'                  TO  ERR-TIPO-ACESSO              
           CALL 'BRAD7100'          USING  WRK-BATCH                    
                                           ERRO-AREA.                   
           GOBACK.                                                      
      *                                                                 
      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*

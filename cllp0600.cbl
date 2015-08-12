      *================================================================*
       IDENTIFICATION                  DIVISION.                        
      *================================================================*
       PROGRAM-ID. CLLP0600.                                            
       AUTHOR.     CPMACT 1.4.                                          
      *REMARKS.                                                         
      *    PROGRAMA ORIGINALMENTE CODIFICADO EM ASSEMBLER,              
      *    CONVERTIDO PARA COBOL ENTERPRISE PELO CONVERSOR:             
      *    CPMBRAXIS   "ACT 1.4 -  ASSEMBLER-COBOL TRANSLATOR".         
      *INSTALLATION.                                                    
      *DATE-WRITTEN.                                                    
      *DATE-COMPILED.                                                   
      *SECURITY.                                                        
                                                                        
      ******************************************************************
      *                        C L L P 0 6 0 0                          
      *                        = = = = = = = =                          
      *                                                                 
      *     AUTOR            -  ELAINE KOGA                             
      *     ANALISTA         -  ZULU                                    
      *     DATA             -  JUNHO / 1999                            
      *                                                                 
      *                                                                 
      *     OBJETIVOS        -  GRAVA REGISTRO NO ARQUIVO DIGIT COM TAM.
      *                         575, SEM DIVIDI-LOS EM 3 TIPOS.         
      *                                                                 
      ******************************************************************
                                                                        
BRQ141******************************************************************
BRQ141* FEV/2014     - (BRQ) - TRATAMENTO DE CARTEIRA  ALFANUMERICA     
BRQ141******************************************************************
                                                                        
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
           SELECT ARQDATA ASSIGN       TO UT-S-ARQDATA                  
           FILE STATUS                 IS WRK-FS-ARQDATA.               
           SELECT MOVIM ASSIGN         TO UT-S-MOVIM                    
           FILE STATUS                 IS WRK-FS-MOVIM.                 
           SELECT ARQTAB ASSIGN        TO UT-S-ARQTAB                   
           FILE STATUS                 IS WRK-FS-ARQTAB.                
           SELECT DIGIT ASSIGN         TO UT-S-DIGIT                    
           FILE STATUS                 IS WRK-FS-DIGIT.                 
           SELECT RELCONS ASSIGN       TO UT-S-RELCONS                  
           FILE STATUS                 IS WRK-FS-RELCONS.               
           SELECT ARQCGC ASSIGN        TO UT-S-ARQCGC                   
           FILE STATUS                 IS WRK-FS-ARQCGC.                
           SELECT LISTACR ASSIGN       TO UT-S-LISTACR                  
           FILE STATUS                 IS WRK-FS-LISTACR.               
      *================================================================*
       DATA                            DIVISION.                        
      *================================================================*
       FILE                            SECTION.                         
      *----------------------------------------------------------------*
      *      INPUT : ARQUIVO ARQDATA                                   *
      *               ORG.           -  LRECL = 060                    *
      *----------------------------------------------------------------*
       FD  ARQDATA                                                      
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
       01  FD-REG-ARQDATA              PIC X(060).                      
      *----------------------------------------------------------------*
      *      INPUT : ARQUIVO MOVIM                                     *
      *               ORG.           -  LRECL = 301                    *
      *----------------------------------------------------------------*
       FD  MOVIM                                                        
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
       01  FD-REG-MOVIM                PIC X(301).                      
      *----------------------------------------------------------------*
      *      INPUT : ARQUIVO ARQTAB                                    *
      *               ORG.           -  LRECL = 168                    *
      *----------------------------------------------------------------*
       FD  ARQTAB                                                       
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
       01  FD-REG-ARQTAB               PIC  X(168).                     
      *----------------------------------------------------------------*
      *      OUTPUT : ARQUIVO DIGIT                                    *
      *               ORG.           -  LRECL = 575                    *
      *----------------------------------------------------------------*
       FD  DIGIT                                                        
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
       01  FD-REG-DIGIT                PIC X(575).                      
      *----------------------------------------------------------------*
      *      OUTPUT : ARQUIVO RELCONS                                  *
      *               ORG.           -  LRECL = 150                    *
      *----------------------------------------------------------------*
       FD  RELCONS                                                      
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
       01  FD-REG-RELCONS              PIC  X(150).                     
      *----------------------------------------------------------------*
      *      OUTPUT : ARQUIVO ARQCGC                                   *
      *               ORG.           -  LRECL = 095                    *
      *----------------------------------------------------------------*
       FD  ARQCGC                                                       
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
       01  FD-REG-ARQCGC               PIC X(095).                      
      *----------------------------------------------------------------*
      *      OUTPUT : ARQUIVO LISTACR                                  *
      *               ORG.           -  LRECL = 133                    *
      *----------------------------------------------------------------*
       FD  LISTACR                                                      
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
       01  FD-REG-LISTACR              PIC X(133).                      
      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.                         
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE        
           'INICIO DA WORKING STORAGE SECTION '.                        
      *----------------------------------------------------------------*
       77  WRK-BATCH                   PIC  X(008)         VALUE        
           'BATCH'.                                                     
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(020)         VALUE        
           'AREA DE FILE-STATUS'.                                       
      *----------------------------------------------------------------*
       77  WRK-FS-ARQDATA              PIC  X(002)         VALUE SPACES.
       77  WRK-FS-MOVIM                PIC  X(002)         VALUE SPACES.
       77  WRK-FS-ARQTAB               PIC  X(002)         VALUE SPACES.
       77  WRK-FS-DIGIT                PIC  X(002)         VALUE SPACES.
       77  WRK-FS-RELCONS              PIC  X(002)         VALUE SPACES.
       77  WRK-FS-ARQCGC               PIC  X(002)         VALUE SPACES.
       77  WRK-FS-LISTACR              PIC  X(002)         VALUE SPACES.
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
       01  WRK-PEGA                    PIC  X(006)         VALUE SPACES.
       01  WRK-CONTROLE                PIC  X(001)         VALUE SPACES.
       01  WRK-DTMAQ.                                                   
           05  WRK-DIAMAQ              PIC  X(002)         VALUE SPACES.
           05  WRK-MESMAQ              PIC  X(002)         VALUE SPACES.
           05  WRK-SECMAQ              PIC  X(002)         VALUE SPACES.
           05  WRK-ANOMAQ              PIC  X(002)         VALUE SPACES.
      *                                                                 
       01  WRK-WKDATAR                 PIC  X(010)         VALUE SPACES.
       01  WRK-WKDATAF                 PIC  X(002)         VALUE '02'.  
       01  WRK-WKMENS                  PIC  X(080)         VALUE SPACES.
      *                                                                 
       01  WRK-DATENV                  PIC  X(008)         VALUE SPACES.
       01  WRK-SECENV                  PIC  X(002)         VALUE SPACES.
       01  WRK-ANOENV                  PIC  X(002)         VALUE SPACES.
       01  WRK-MESENV                  PIC  X(002)         VALUE SPACES.
       01  WRK-DIAENV                  PIC  X(002)         VALUE SPACES.
       01  WRK-PRAZO                   PIC S9(005) COMP-3  VALUE ZEROS. 
       01  WRK-PRAZO-R                 REDEFINES           WRK-PRAZO    
                                       PIC  X(003).                     
       01  WRK-DATREC                  PIC  X(008)         VALUE '0'.   
       01  WRK-DATREC-R                REDEFINES           WRK-DATREC   
                                       PIC  9(008).                     
       01  WRK-PVAL1                   PIC  9(015) COMP-3  VALUE ZEROS. 
       01  WRK-VLRTIT                  PIC  9(015) COMP-3  VALUE ZEROS. 
       01  WRK-VLRTIT-R                REDEFINES           WRK-VLRTIT   
                                       PIC  X(008).                     
       01  WRK-VLRENC                  PIC  9(015) COMP-3  VALUE ZEROS. 
       01  WRK-ACLINHA                 PIC S9(003) COMP-3  VALUE 51.    
       01  WRK-ACPAGIN                 PIC  9(005) COMP-3  VALUE ZEROS. 
       01  WRK-WKVALOR                 PIC  9(015) COMP-3  VALUE ZEROS. 
       01  WRK-WKVRTIT2                PIC  X(015)         VALUE SPACES.
       01  WRK-WKVRIOF                 PIC  9(015) COMP-3  VALUE ZEROS. 
       01  WRK-WKVRIOF-R               REDEFINES           WRK-WKVRIOF  
                                       PIC  X(008).                     
       01  WRK-DDMMAA.                                                  
           05  WRK-DIANOR              PIC  X(002)         VALUE SPACES.
           05  WRK-MESNOR              PIC  X(002)         VALUE SPACES.
           05  WRK-ANONOR              PIC  X(004)         VALUE SPACES.
       01  WRK-AAMMDD.                                                  
           05  WRK-ANOINV              PIC  X(004)         VALUE SPACES.
           05  WRK-MESINV              PIC  X(002)         VALUE SPACES.
           05  WRK-DIAINV              PIC  X(002)         VALUE SPACES.
       01  WRK-DTVCTINV                PIC S9(009) COMP-3  VALUE ZEROS. 
       01  WRK-DTOPEINV                PIC S9(009) COMP-3  VALUE ZEROS. 
       01  WRK-AUXPEND                 PIC  X(005)         VALUE '0'.   
       01  WRK-AUXPEND-RED             REDEFINES           WRK-AUXPEND  
                                       PIC  9(005).                     
      *                                                                 
       01  WRK-CODDIR                  PIC  X(002)         VALUE SPACES.
       01  WRK-CODDIR-RED              REDEFINES           WRK-CODDIR   
                                       PIC  9(002).                     
       01  WRK-NOMEDIR                 PIC  X(020)         VALUE SPACES.
       01  WRK-JUNCAO                  PIC  X(004)         VALUE SPACES.
       01  WRK-JUNCAO-RED              REDEFINES           WRK-JUNCAO   
                                       PIC  9(004).                     
       01  WRK-NOMEAG                  PIC  X(020)         VALUE SPACES.
      *                                                                 
       01  WRK-CABEC1.                                                  
           05  FILLER                  PIC  X(020)         VALUE        
           '1*CLLP0600*         '.                                      
           05 FILLER                   PIC  X(047)         VALUE        
           'LISTAGEM DOS REGISTROS TRANSFERIDOS DA MORA  -'.            
           05  FILLER                  PIC  X(024)         VALUE        
           'CONSISTIDOS     MOVTO-'.                                    
           05  WRK-DTMVTCB1            PIC  X(010)         VALUE SPACES.
           05  FILLER                  PIC  X(006)         VALUE        
           '   EM '.                                                    
           05  WRK-DTDIACB1            PIC  X(010)         VALUE SPACES.
           05  FILLER                  PIC  X(022)         VALUE SPACES.
           05  FILLER                  PIC  X(005)         VALUE        
           'FOLHA'.                                                     
           05  WRK-PAGCAB1             PIC BZ9999          VALUE SPACES.
      *                                                                 
       01  WRK-CABEC2.                                                  
           05  FILLER                  PIC  X(042)         VALUE        
           '0 EMP    AG        C/C  PEND/CART/CONTR'.                   
           05  FILLER                  PIC  X(033)         VALUE        
           'VENCTO   INIC.OP.  DT.MOVTO'.                               
           05  FILLER                  PIC  X(023)         VALUE        
           'VALOR DO TITULO'.                                           
           05  FILLER                  PIC  X(036)         VALUE        
           'VALOR DO IOF    VR. ENC.VENCIDOS'.                          
           05 FILLER                   PIC  X(016)         VALUE        
           ' ENC.ORIGEM-EMPF'.                                          
      *                                                                 
       01  WRK-CABEC3.                                                  
           05  FILLER                  PIC  X(040)         VALUE        
           ' NOME DO DEVEDOR'.                                          
           05  FILLER                  PIC  X(008)         VALUE        
           ' TIPO   '.                                                  
           05  FILLER                  PIC  X(026)         VALUE        
           'CPF/CGC     CART.OR'.                                       
           05  FILLER                  PIC  X(042)         VALUE        
           'VR.ENC.VINCENDOS     VR. DEV.INICIAL'.                      
           05  FILLER                  PIC  X(034)         VALUE        
           'VALOR CONTABIL'.                                            
      *                                                                 
       01  WRK-LINDET.                                                  
           05  WRK-CARRO               PIC  X(001)         VALUE '0'.   
           05  WRK-DETEMPR             PIC  X(004)         VALUE SPACES.
           05  FILLER                  PIC  X(002)         VALUE SPACES.
           05  WRK-DETAGEN             PIC  X(004)         VALUE SPACES.
           05  FILLER                  PIC  X(003)         VALUE SPACES.
           05  WRK-DETCONTA            PIC  X(007)         VALUE SPACES.
           05  FILLER                  PIC  X(002)         VALUE SPACES.
           05  WRK-DETTPEND            PIC  X(004)         VALUE SPACES.
           05  FILLER                  PIC  X(001)         VALUE '/'.   
           05  WRK-DETCART             PIC  X(003)         VALUE SPACES.
           05  FILLER                  PIC  X(001)         VALUE '/'.   
           05  WRK-DETCONTR            PIC  X(007)         VALUE SPACES.
           05  FILLER                  PIC  X(002)         VALUE SPACES.
           05  WRK-DETVENCT            PIC  X(008)         VALUE SPACES.
           05  FILLER                  PIC  X(002)         VALUE SPACES.
           05  WRK-DETDTINI            PIC  X(008)         VALUE SPACES.
           05  FILLER                  PIC  X(002)         VALUE SPACES.
           05  WRK-DETDTMOV            PIC  X(008)         VALUE SPACES.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  WRK-DETVROP             PIC BZZZZ.ZZZ.ZZZ.ZZZ,99         
                                                           VALUE SPACES.
           05  WRK-DETVRIOF            PIC BZZZZ.ZZZ.ZZZ.ZZZ,99         
                                                           VALUE SPACES.
           05  WRK-DETVRVEN            PIC BZZZZ.ZZZ.ZZZ.ZZZ,99         
                                                           VALUE SPACES.
           05  WRK-DETVR59             PIC BZZZZ.ZZZ.ZZZ.ZZZ,99         
                                                           VALUE SPACES.
           05  FILLER                  PIC  X(011)         VALUE SPACES.
      *                                                                 
       01  WRK-LINDET2.                                                 
           05  WRK-CARRO2              PIC  X(001)         VALUE SPACES.
           05  WRK-DETNOME             PIC  X(039)         VALUE SPACES.
           05  FILLER                  PIC  X(002)         VALUE SPACES.
           05  WRK-DETTIPO             PIC  X(002)         VALUE SPACES.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  WRK-DETCGC              PIC  X(009)         VALUE        
                                                             LOW-VALUES.
           05  FILLER                  PIC  X(001)         VALUE '/'.   
           05  WRK-DETFIL              PIC  X(004)         VALUE        
                                                             LOW-VALUES.
           05  FILLER                  PIC  X(001)         VALUE '/'.   
           05  WRK-DETCTR              PIC  X(002)         VALUE        
                                                             LOW-VALUES.
           05  FILLER                  PIC  X(003)         VALUE SPACES.
           05  WRK-DETCARTO            PIC  X(003)         VALUE SPACES.
           05  FILLER                  PIC  X(002)         VALUE SPACES.
           05  WRK-DETVRVIN            PIC BZZZZ.ZZZ.ZZZ.ZZZ,99         
                                                           VALUE SPACES.
           05  WRK-DETVRDEV            PIC BZZZZ.ZZZ.ZZZ.ZZZ,99         
                                                           VALUE SPACES.
           05  WRK-DETVRCON            PIC BZZZZ.ZZZ.ZZZ.ZZZ,99         
                                                           VALUE SPACES.
           05  FILLER                  PIC  X(023)         VALUE SPACES.
      *                                                                 
       01  WRK-WDATA.                                                   
           05  WRK-DTMOVTO             PIC  X(010)         VALUE        
           '31/01/1991'.                                                
           05  WRK-ENTNORM             PIC  X(008)         VALUE        
           '31011991'.                                                  
TESTE      05  WRK-ENTNORM-R           REDEFINES           WRK-ENTNORM  
                                       PIC  9(008).                     
           05  WRK-ENTINV              PIC  X(008)         VALUE        
           '19910131'.                                                  
           05  WRK-ENTINV-R            REDEFINES           WRK-ENTINV   
                                       PIC  9(008).                     
           05  WRK-ENTNPK              PIC S9(009) COMP-3  VALUE ZEROS. 
           05  WRK-DTMOVINV            PIC S9(009) COMP-3  VALUE ZEROS. 
           05  WRK-ENTCOD              PIC S9(007) COMP-3  VALUE ZEROS. 
           05  WRK-ENTULT              PIC  X(010)         VALUE        
           '31/01/1991'.                                                
           05  WRK-ENTULTPK            PIC S9(009) COMP-3  VALUE ZEROS. 
           05  WRK-ENTDT20             PIC S9(009) COMP-3  VALUE ZEROS. 
      *                                                                 
       01  WRK-WKARQCR.                                                 
           05  WRK-WKAGCR              PIC  9(005) COMP-3  VALUE ZEROS. 
           05  WRK-WKCCCR              PIC  9(007) COMP-3  VALUE ZEROS. 
BRQ141*    05  WRK-WKCARCR             PIC  9(005) COMP-3  VALUE ZEROS. 
BRQ141     05  WRK-WKCARCR             PIC  X(003)         VALUE SPACES.
           05  WRK-WKCONCR             PIC  9(007) COMP-3  VALUE ZEROS. 
           05  WRK-WKVCTOCR            PIC  9(009) COMP-3  VALUE ZEROS. 
           05  WRK-WKVCTOCR-R          REDEFINES           WRK-WKVCTOCR 
                                       PIC  X(005).                     
           05  WRK-WKVALCR             PIC  9(015) COMP-3  VALUE ZEROS. 
           05  WRK-WKVALCR-R           REDEFINES           WRK-WKVALCR  
                                       PIC  X(008).                     
           05  WRK-WKDEBCR             PIC  9(015) COMP-3  VALUE ZEROS. 
           05  WRK-WKDEBCR-R           REDEFINES           WRK-WKDEBCR  
                                       PIC  X(008).                     
           05  WRK-WKNOMECR            PIC  X(040)         VALUE SPACES.
           05  WRK-WKIDCR              PIC  X(001)         VALUE SPACES.
           05  WRK-WKDIRRCR            PIC  9(003) COMP-3  VALUE ZEROS. 
           05  WRK-WKDIRRCR-R          REDEFINES           WRK-WKDIRRCR 
                                       PIC  X(002).                     
           05  WRK-WKJDIRCR            PIC  9(005) COMP-3  VALUE ZEROS. 
           05  WRK-WKJDIRCR-R          REDEFINES           WRK-WKJDIRCR 
                                       PIC  X(003).                     
           05  WRK-WKCAMBIO            PIC  X(003)         VALUE        
           X'00000F'.                                                   
           05  WRK-WKRAMOCR            PIC  X(001)         VALUE SPACES.
           05  WRK-WKCGCCR             PIC  9(009) COMP-3  VALUE ZEROS. 
           05  WRK-WKFILCR             PIC  9(005) COMP-3  VALUE ZEROS. 
           05  WRK-WKCTRCR             PIC  X(002)         VALUE '0'.   
      *                                                                 
       01  WRK-WKAUXVR                 PIC  9(015) COMP-3  VALUE ZEROS. 
      *                                                                 
       01  WRK-LCABCR0.                                                 
           05  FILLER                  PIC  X(001)         VALUE X'89'. 
           05  FILLER                  PIC  X(132)         VALUE ' '.   
      *                                                                 
       01  WRK-LCABCR1.                                                 
           05  FILLER                  PIC  X(001)         VALUE X'11'. 
           5  FILLER                   PIC  X(019)         VALUE        
           '*CLLP0600*'.                                                
           05  FILLER                  PIC  X(054)         VALUE        
           'TRANSFERENCIA DE MORA PARA CL EM CRUZEIROS REAIS'.          
           05 FILLER                   PIC  X(007)         VALUE        
           'MOVTO-'.                                                    
           05 WRK-LDTMOVCR             PIC  X(010)         VALUE SPACES.
           05 FILLER                   PIC  X(011)         VALUE        
           '       EM'.                                                 
           05 WRK-LDTPROCR             PIC  X(010)         VALUE SPACES.
      *                                                                 
       01  WRK-LCABCR2.                                                 
           05  FILLER                  PIC  X(001)         VALUE X'11'. 
           05  FILLER                  PIC  X(012)         VALUE ' AG'. 
           05  FILLER                  PIC  X(120)         VALUE        
           'C/C  CART   CONTRATO   VENCTO       VALOR PRINCIPAL     VALO
      -    'R ATUALIZADO  NOME DO DEVEDOR'.                             
      *                                                                 
       01  WRK-LDETCR.                                                  
           05  FILLER                  PIC  X(001)         VALUE X'09'. 
           05  WRK-LAGCR               PIC  X(004)         VALUE SPACES.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  WRK-LCCCR               PIC BZ.ZZZ.ZZ9      VALUE SPACES.
           05  FILLER                  PIC  X(003)         VALUE SPACES.
           05  WRK-LCARTCR             PIC  X(003)         VALUE SPACES.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  WRK-LCONTCR             PIC BZ.ZZZ.ZZ9      VALUE SPACES.
           05  FILLER                  PIC  X(002)         VALUE SPACES.
           05  WRK-LVCTOCR             PIC  X(008)         VALUE SPACES.
           05  WRK-LVCTOCR-RED         REDEFINES           WRK-LVCTOCR  
                                       PIC  9(008).                     
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  WRK-LVRPRICR            PIC BZZZZ.ZZZ.ZZZ.ZZZ,99         
                                                           VALUE SPACES.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  WRK-LVRATUCR            PIC BZZZZ.ZZZ.ZZZ.ZZZ,99         
                                                           VALUE SPACES.
           05  FILLER                  PIC  X(002)         VALUE SPACES.
           05  WRK-LNOMECR             PIC  X(040)         VALUE SPACES.
      *                                                                 
       01  WRK-QTDECR                  PIC S9(003) COMP-3  VALUE 60.    
      *                                                                 
       01  WRK-TABREG.                                                  
           05  WRK-FILLER              PIC  X(002)         VALUE '00'.  
           05  WRK-FILLER              PIC  X(001)         VALUE '0'.   
           05  WRK-FILLER              PIC  X(002)         VALUE '01'.  
           05  WRK-FILLER              PIC  X(001)         VALUE '3'.   
           05  WRK-FILLER              PIC  X(002)         VALUE '02'.  
           05  WRK-FILLER              PIC  X(001)         VALUE '4'.   
           05  WRK-FILLER              PIC  X(002)         VALUE '03'.  
           05  WRK-FILLER              PIC  X(001)         VALUE '1'.   
      *                                                                 
       01  WRK-TABTIPO.                                                 
           05  FILLER                  PIC  X(002)         VALUE '00'.  
           05  FILLER                  PIC  X(002)         VALUE '01'.  
           05  FILLER                  PIC  X(002)         VALUE '02'.  
           05  FILLER                  PIC  X(002)         VALUE '03'.  
           05  FILLER                  PIC  X(002)         VALUE '04'.  
           05  FILLER                  PIC  X(002)         VALUE '05'.  
           05  FILLER                  PIC  X(002)         VALUE '06'.  
           05  FILLER                  PIC  X(002)         VALUE '07'.  
           05  FILLER                  PIC  X(002)         VALUE '08'.  
           05  FILLER                  PIC  X(002)         VALUE '09'.  
           05  FILLER                  PIC  X(002)         VALUE '10'.  
           05  FILLER                  PIC  X(002)         VALUE '11'.  
           05  FILLER                  PIC  X(002)         VALUE '12'.  
           05  FILLER                  PIC  X(002)         VALUE '13'.  
           05  FILLER                  PIC  X(002)         VALUE '14'.  
           05  FILLER                  PIC  X(002)         VALUE '15'.  
           05  FILLER                  PIC  X(002)         VALUE '16'.  
           05  FILLER                  PIC  X(002)         VALUE '17'.  
           05  FILLER                  PIC  X(002)         VALUE '18'.  
           05  FILLER                  PIC  X(002)         VALUE '19'.  
           05  FILLER                  PIC  X(002)         VALUE '20'.  
           05  FILLER                  PIC  X(002)         VALUE '21'.  
           05  FILLER                  PIC  X(002)         VALUE '98'.  
           05  FILLER                  PIC  X(002)         VALUE '99'.  
      *                                                                 
       01  WRK-TABCONV.                                                 
           05  FILLER                  PIC  X(003)         VALUE '726'. 
           05  FILLER                  PIC  X(003)         VALUE '736'. 
           05  FILLER                  PIC  X(006)         VALUE '*'.   
      *                                                                 
       01  WRK-DATAHORA.                                                
           05  WRK-JULIANA             PIC  9(005) COMP-3  VALUE ZEROS. 
           05  WRK-AAMMDD2             PIC  9(007) COMP-3  VALUE ZEROS. 
           05  WRK-AAAAMMDD            PIC  9(009) COMP-3  VALUE ZEROS. 
           05  WRK-HHMMSS              PIC  9(007) COMP-3  VALUE ZEROS. 
           05  WRK-HHMMSSMM            PIC  9(013) COMP-3  VALUE ZEROS. 
           05  WRK-TIMESTA             PIC  X(020)         VALUE SPACES.
      *                                                                 
       01  WRK-DATAZ.                                                   
           05  WRK-ANO                 PIC  X(004)         VALUE SPACES.
           05  WRK-MES                 PIC  X(002)         VALUE SPACES.
           05  WRK-DIA                 PIC  X(002)         VALUE SPACES.
      *                                                                 
       01  WRK-DATSYS.                                                  
           05  WRK-DIASYS              PIC  X(002)         VALUE SPACES.
           05  WRK-MESSYS              PIC  X(002)         VALUE SPACES.
           05  WRK-ANOSYS              PIC  X(004)         VALUE SPACES.
      *                                                                 
       01  WRK-DATEDT.                                                  
           05  WRK-DIAEDT              PIC  X(002)         VALUE SPACES.
           05  FILLER                  PIC  X(001)         VALUE '/'.   
           05  WRK-MESEDT              PIC  X(002)         VALUE SPACES.
           05  FILLER                  PIC  X(001)         VALUE '/'.   
           05  WRK-ANOEDT              PIC  X(004)         VALUE SPACES.
      *                                                                 
       01  WRK-TABELA                  PIC  X(169000)      VALUE ALL    
           '00'.                                                        
      *----------------------------------------------------------------*
      *         --  INC  I#CLLPG3  --                                   
      *    ARQUIVO DE TRANSFERENCIA DE MORA/OTNF PARA CL                
      *    ARQUIVO MOVIM - LRECL=301                                    
      *----------------------------------------------------------------*
       01  WRK-MOVREG.                                                  
           05  WRK-MOVFIXO             PIC  X(002)         VALUE '0'.   
           05  WRK-MOVCOAGE            PIC  X(004)         VALUE '0'.   
           05  WRK-MOVCOAGE-R          REDEFINES           WRK-MOVCOAGE 
                                       PIC  9(004).                     
           05  WRK-MOVNUCTA            PIC  X(007)         VALUE '0'.   
           05  WRK-MOVNUCTA-R          REDEFINES           WRK-MOVNUCTA 
                                       PIC  9(007).                     
           05  WRK-MOVSIGLA            PIC  X(004)         VALUE SPACES.
           05  WRK-MOVCART             PIC  X(003)         VALUE '0'.   
           05  WRK-MOVCART-R           REDEFINES           WRK-MOVCART  
                                       PIC  9(003).                     
           05  WRK-MOVNUTIT            PIC  X(007)         VALUE '0'.   
           05  WRK-MOVNUTIT-R          REDEFINES           WRK-MOVNUTIT 
                                       PIC  9(007).                     
           05  WRK-MOVVENC             PIC  X(008)         VALUE '0'.   
           05  WRK-MOVVENC-R           REDEFINES           WRK-MOVVENC  
                                       PIC  9(008).                     
           05  WRK-MOVDTINI            PIC  X(008)         VALUE '0'.   
           05  WRK-MOVDTMOV            PIC  X(008)         VALUE '0'.   
           05  WRK-MOVMOEDA            PIC  X(002)         VALUE SPACES.
           05  WRK-MOVTPGAR            PIC  X(002)         VALUE '0'.   
           05  WRK-MOVMARCA            PIC  X(001)         VALUE SPACES.
           05  WRK-MOVMOTIV            PIC  X(002)         VALUE '0'.   
           05  WRK-MOVDUPRO            PIC  X(030)         VALUE SPACES.
           05  WRK-MOVVTIT             PIC  X(015)         VALUE '0'.   
           05  WRK-MOVVTIT-R           REDEFINES           WRK-MOVVTIT  
                                       PIC  9(015).                     
           05  WRK-MOVVRCON            PIC  X(015)         VALUE '0'.   
           05  WRK-MOVVRCON-R          REDEFINES           WRK-MOVVRCON 
                                       PIC  9(015).                     
           05  WRK-MOVENCCT            PIC  X(015)         VALUE '0'.   
           05  WRK-MOVENCCT-R          REDEFINES           WRK-MOVENCCT 
                                       PIC  9(015).                     
           05  WRK-MOVENCSO            PIC  X(015)         VALUE '0'.   
           05  WRK-MOVENCSO-R          REDEFINES           WRK-MOVENCSO 
                                       PIC  9(015).                     
           05  WRK-MOVIOF              PIC  X(015)         VALUE '0'.   
           05  WRK-MOVIOF-R            REDEFINES           WRK-MOVIOF   
                                       PIC  9(015).                     
           05  WRK-MOVNODEV            PIC  X(040)         VALUE SPACES.
           05  WRK-MOVCOOB1            PIC  X(040)         VALUE SPACES.
           05  WRK-MOVCOOB2            PIC  X(040)         VALUE SPACES.
           05  WRK-MOVCGC.                                              
               10  WRK-MOVNCGC         PIC  X(009)         VALUE '0'.   
               10  WRK-MOVNCGC-R       REDEFINES           WRK-MOVNCGC  
                                       PIC  9(009).                     
               10  WRK-MOVFCGC         PIC  X(004)         VALUE '0'.   
               10  WRK-MOVFCGC-R       REDEFINES           WRK-MOVFCGC  
                                       PIC  9(004).                     
               10  WRK-MOVCCGC         PIC  X(002)         VALUE '0'.   
           05  WRK-MOVNATU             PIC  X(003)         VALUE SPACES.
      *----------------------------------------------------------------*
      *  ++ INC I#CLLPM8                                                
      *----------------------------------------------------------------*
       01  WRK-REGMOVTO.                                                
           05  WRK-ACCHAVEX.                                            
               10  WRK-ACTIPOX         PIC  X(001)         VALUE '1'.   
               10  WRK-FIRMA           PIC  X(005)         VALUE        
               '00000'.                                                 
               10  WRK-AGENCIA         PIC  X(005)         VALUE        
               '00000'.                                                 
               10  WRK-NUMCL           PIC  X(015)         VALUE        
               '000000000000000'.                                       
               10  WRK-DIGCL           PIC  X(001)         VALUE SPACES.
           05  WRK-CONT.                                                
               10  WRK-RAZCL           PIC  X(005)         VALUE        
               '00000'.                                                 
               10  WRK-CTACL           PIC  X(007)         VALUE        
               '0000000'.                                               
               10  WRK-RAZAZOR         PIC  X(005)         VALUE        
               '00000'.                                                 
               10  WRK-CTAOR           PIC  X(007)         VALUE        
               '0000000'.                                               
           05  WRK-CONTA               PIC  X(007)         VALUE        
           '0000000'.                                                   
           05  WRK-DATAS.                                               
               10  WRK-VENCIM          PIC  X(008)         VALUE        
               '00000000'.                                              
               10  WRK-ENTRADA         PIC  X(008)         VALUE        
               '00000000'.                                              
               10  WRK-BAIXACL         PIC  X(008)         VALUE        
               '00000000'.                                              
               10  WRK-OPER            PIC  X(008)         VALUE        
               '00000000'.                                              
               10  WRK-AJUIZAM         PIC  X(008)         VALUE        
               '00000000'.                                              
               10  WRK-ENTRALP         PIC  X(008)         VALUE        
               '00000000'.                                              
               10  WRK-BAIXALP         PIC  X(008)         VALUE        
               '00000000'.                                              
               10  WRK-DTAMOVTO        PIC  X(008)         VALUE        
               '00000000'.                                              
           05  WRK-DTTRANS             PIC  X(008)         VALUE        
           '00000000'.                                                  
           05  WRK-SIGLA               PIC  X(004)         VALUE SPACES.
           05  WRK-ANOOP               PIC  X(003)         VALUE '000'. 
           05  WRK-NUMOP               PIC  X(007)         VALUE        
           '0000000'.                                                   
           05  WRK-VALOP               PIC  X(015)         VALUE        
           '000000000000000'.                                           
           05  WRK-VALOP-R             REDEFINES           WRK-VALOP    
                                       PIC  9(015).                     
           05  WRK-VALRES              PIC  X(015)         VALUE        
           '000000000000000'.                                           
           05  WRK-VALRES-R            REDEFINES           WRK-VALRES   
                                       PIC  9(015).                     
           05  WRK-MOEDA               PIC  X(002)         VALUE SPACES.
           05  WRK-LOCAL               PIC  X(002)         VALUE 'FO'.  
           05  WRK-CONTAB              PIC  X(001)         VALUE SPACES.
           05  WRK-TPUNIT              PIC  X(001)         VALUE SPACES.
           05  WRK-ID                  PIC  X(002)         VALUE 'CL'.  
           05  WRK-NOMED               PIC  X(040)         VALUE SPACES.
           05  WRK-CGC                 PIC  X(015)         VALUE SPACES.
           05  WRK-COMP1               PIC  X(001)         VALUE SPACES.
           05  WRK-COOB0               PIC  X(040)         VALUE SPACES.
           05  WRK-CGC1                PIC  X(015)         VALUE        
           '000000000000000'.                                           
           05  WRK-COOB1               PIC  X(040)         VALUE SPACES.
           05  WRK-CGC2                PIC  X(015)         VALUE        
           '000000000000000'.                                           
           05  WRK-VRBASE              PIC  X(015)         VALUE        
           '000000000000000'.                                           
           05  WRK-DTRECEXP            PIC  X(008)         VALUE        
           '00000000'.                                                  
           05  WRK-SUBSTA              PIC  X(001)         VALUE SPACES.
           05  WRK-CPFADV              PIC  X(009)         VALUE        
           '000000000'.                                                 
           05  WRK-CTRADV              PIC  X(002)         VALUE '00'.  
           05  WRK-CART                PIC  X(003)         VALUE '000'. 
           05  WRK-TIPOGAR             PIC  X(002)         VALUE '00'.  
           05  WRK-IDENT               PIC  X(004)         VALUE '0000'.
           05  WRK-MARCA               PIC  X(001)         VALUE SPACES.
           05  WRK-STAREAT             PIC  X(001)         VALUE SPACES.
           05  WRK-AGRESP              PIC  X(004)         VALUE '0000'.
           05  FILLER                  PIC  X(002)         VALUE SPACES.
           05  WRK-VRBIOF              PIC  X(015)         VALUE        
           '000000000000000'.                                           
           05  WRK-GARANTIA            PIC  X(032)         VALUE SPACES.
           05  WRK-CODACER             PIC  X(001)         VALUE '1'.   
           05  WRK-VALVENC             PIC  X(015)         VALUE        
           '000000000000000'.                                           
           05  WRK-VALVINC             PIC  X(015)         VALUE        
           '000000000000000'.                                           
           05  WRK-VALVINC-R           REDEFINES           WRK-VALVINC  
                                       PIC  9(015).                     
           05  WRK-VALBXA              PIC  X(015)         VALUE        
           '000000000000000'.                                           
           05  WRK-VALINIC             PIC  X(015)         VALUE        
           '000000000000000'.                                           
           05  WRK-VALINIC-R           REDEFINES           WRK-VALINIC  
                                       PIC  9(015).                     
           05  WRK-VALORIOF            PIC  X(015)         VALUE        
           '000000000000000'.                                           
           05  WRK-VALORIOF-R          REDEFINES           WRK-VALORIOF 
                                       PIC  9(015).                     
           05  WRK-VRDEBCC             PIC  X(015)         VALUE        
           '000000000000000'.                                           
           05  WRK-VRCOBR              PIC  X(015)         VALUE        
           '000000000000000'.                                           
           05  WRK-CDOCOR              PIC  X(002)         VALUE '00'.  
           05  WRK-BLOQTR              PIC  X(001)         VALUE SPACES.
           05  WRK-DTPGTO              PIC  X(008)         VALUE        
           '00000000'.                                                  
           05  WRK-ACERTOSX            PIC  X(001)         VALUE '1'.   
           05  WRK-ORIGBXA             PIC  X(002)         VALUE SPACES.
           05  WRK-TRANSST             PIC  X(001)         VALUE SPACES.
           05  WRK-DEBCC               PIC  X(001)         VALUE SPACES.
           05  WRK-FIXO                PIC  X(002)         VALUE '00'.  
           05  WRK-SOL8273X            PIC  X(004)         VALUE SPACES.
      *                                                                 
CPMCAC 01  WRK-08                      PIC  X(008)         VALUE SPACES.
CPMCAC 01  WRK-08-RED                  REDEFINES           WRK-08       
CPMCAC                                 PIC  9(008).                     
CPMCAC 01  WRK-09                      PIC  9(009)         VALUE ZEROS. 
CPMCAC 01  WRK-09-RED                  REDEFINES           WRK-09       
CPMCAC                                 PIC  X(009).                     
CPMCAC 01  WRK-15                      PIC  9(015)         VALUE ZEROS. 
CPMCAC 01  WRK-15-RED                  REDEFINES           WRK-15       
CPMCAC                                 PIC  X(015).                     
CPMCAC 01  WRK-INDR5                   PIC S9(015) COMP-3  VALUE ZEROS. 
CPMCAC 01  WRK-INDR5B                  PIC S9(015) COMP-3  VALUE ZEROS. 
CPMCAC 01  WRK-INDR6                   PIC S9(015) COMP-3  VALUE ZEROS. 
CPMCAC 01  WRK-INDR7                   PIC S9(015) COMP-3  VALUE ZEROS. 
CPMCAC 01  WRK-INDR9                   PIC S9(015) COMP-3  VALUE ZEROS. 
CPMCAC 01  WRK-INDR9B                  PIC S9(015) COMP-3  VALUE ZEROS. 
CPMCAC 01  WRK-INDR10                  PIC S9(015) COMP-3  VALUE ZEROS. 
CPMCAC 01  WRK-05                      PIC  9(005) COMP-3  VALUE ZEROS. 
CPMCAC 01  WRK-05-RED                  REDEFINES           WRK-05       
CPMCAC                                 PIC  X(003).                     
CPMCAC 01  WRK-95                      PIC  9(005)         VALUE ZEROS. 
CPMCAC 01  WRK-95-RED                  REDEFINES           WRK-95       
                                       PIC  X(005).                     
CPMCAC 01  WRK-V99                     PIC  9(015)         VALUE ZEROS. 
CPMCAC 01  WRK-V99-RED                 REDEFINES           WRK-V99      
CPMCAC                                 PIC  9(013)V99.                  
CPMCAC 01  WRK-V99-ALP                 REDEFINES           WRK-V99      
CPMCAC                                 PIC  X(015).                     
CPMCAC 01  WRK-04                      PIC  X(004)         VALUE SPACES.
CPMCAC 01  WRK-04-RED                  REDEFINES           WRK-04       
                                       PIC  9(004).                     
CPMCAC 01  WRK-55                      PIC  9(005)         VALUE ZEROS. 
CPMCAC 01  FILLER                      REDEFINES           WRK-55.      
CPMCAC     05  FILLER                  PIC  X(001).                     
CPMCAC     05  WRK-55-R                PIC  9(004).                     
CPMCAC 01  WRK-02                      PIC  X(002)         VALUE SPACES.
CPMCAC 01  WRK-02-RED                  REDEFINES           WRK-02       
CPMCAC                                 PIC  9(002).                     
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)          VALUE       
           'FIM DA WORKING STORAGE SECTION '.                           
      *----------------------------------------------------------------*
           COPY I#BRAD7C.                                               
      *================================================================*
       PROCEDURE                       DIVISION.                        
      *================================================================*
           CALL    'BRAD7600'          USING WRK-DATAHORA               
           MOVE    WRK-AAAAMMDD        TO  WRK-09                       
           MOVE    WRK-09-RED(2:8)     TO  WRK-DATAZ(1:8)               
           MOVE    WRK-DIA             TO  WRK-DIASYS                   
           MOVE    WRK-MES             TO  WRK-MESSYS                   
           MOVE    WRK-ANO             TO  WRK-ANOSYS                   
           MOVE    WRK-DIA             TO  WRK-DIAMAQ                   
           MOVE    WRK-MES             TO  WRK-MESMAQ                   
           MOVE    WRK-ANO(1:2)        TO  WRK-SECMAQ(1:2)              
           MOVE    WRK-ANO(3:2)        TO  WRK-ANOMAQ(1:2)              
           MOVE    WRK-DIASYS          TO  WRK-DIAEDT                   
           MOVE    WRK-MESSYS          TO  WRK-MESEDT                   
           MOVE    WRK-ANOSYS          TO  WRK-ANOEDT.                  
      *================================================================*
       0010-JAMOV.                                                      
      *================================================================*
           MOVE    WRK-DATEDT          TO  WRK-DTDIACB1                 
           MOVE    WRK-DATEDT          TO  WRK-LDTPROCR                 
      *                                                                 
           OPEN    INPUT                                                
                   ARQDATA                                              
           MOVE    WRK-ABERTURA        TO  WRK-OPERACAO                 
           PERFORM 9010-TESTAR-FS-ARQDATA THRU 9010-99-FIM.             
      *================================================================*
       0020-LEDATA.                                                     
      *================================================================*
           READ    ARQDATA             INTO WRK-WDATA                   
           END-READ                                                     
           MOVE    WRK-LEITURA         TO  WRK-OPERACAO                 
           IF      WRK-FS-ARQDATA      EQUAL '10'                       
               GO TO 0030-FIMDATA                                       
           ELSE                                                         
               PERFORM 9010-TESTAR-FS-ARQDATA THRU 9010-99-FIM          
           END-IF                                                       
      *                                                                 
           MOVE    '*'                 TO  WRK-CONTROLE.                
      *================================================================*
       0030-FIMDATA.                                                    
      *================================================================*
           IF      WRK-CONTROLE        NOT EQUAL '*'                    
               GO TO 0040-DTVAZIO                                       
           END-IF                                                       
      *                                                                 
           MOVE    WRK-DTMOVTO         TO  WRK-LDTMOVCR                 
           MOVE    WRK-DTMOVTO         TO  WRK-DTMVTCB1                 
           MOVE    WRK-ENTNORM         TO  WRK-ENTRADA                  
           GO TO 0100-CRIATAB.                                          
      *================================================================*
       0040-DTVAZIO.                                                    
      *================================================================*
           DISPLAY '*CLLP0600* ARQUIVO DE DATA ESTA VAZIO'              
           UPON OPERADOR.                                               
      *================================================================*
       0050-PEDENOVO.                                                   
      *================================================================*
           DISPLAY                                                      
           '*CLLP0600* TECLAR A DATA DO MOVIMENTO NO FORMATO DDMMAAAA'  
           WRK-ENTNORM                                                  
           UPON OPERADOR                                                
           ACCEPT  WRK-ENTNORM         FROM CONSOLE                     
      *                                                                 
           MOVE    WRK-ENTNORM(5:4)    TO  WRK-ENTINV(1:4)              
           MOVE    WRK-ENTNORM(3:2)    TO  WRK-ENTINV(3:2)              
           MOVE    WRK-ENTNORM(1:2)    TO  WRK-ENTINV(5:2).             
      *================================================================*
       0060-CONSISTE.                                                   
      *================================================================*
           IF      WRK-ENTNORM-R       IS NOT  NUMERIC                  
               GO TO 0080-DTINCON                                       
           END-IF                                                       
      *                                                                 
CPMCAC     MOVE    WRK-ENTNORM(1:2)    TO  WRK-02                       
CPMCAC     IF      WRK-02-RED          NOT GREATER ZEROS                
               GO TO 0080-DTINCON                                       
           END-IF                                                       
      *                                                                 
CPMCAC     IF      WRK-02-RED          GREATER  31                      
               GO TO 0080-DTINCON                                       
           END-IF                                                       
      *                                                                 
CPMCAC     MOVE    WRK-ENTNORM(3:2)    TO  WRK-02                       
CPMCAC     IF      WRK-02-RED          NOT GREATER ZEROS                
               GO TO 0080-DTINCON                                       
           END-IF                                                       
      *                                                                 
           IF      WRK-02-RED          GREATER  12                      
               GO TO 0080-DTINCON                                       
           END-IF                                                       
      *                                                                 
CPMCAC     MOVE    WRK-ENTNORM(5:4)    TO  WRK-04                       
CPMCAC     IF      WRK-04-RED          NOT GREATER ZEROS                
               GO TO 0080-DTINCON                                       
           END-IF                                                       
      *                                                                 
           MOVE    WRK-DIAMAQ          TO  WRK-DIAENV                   
           MOVE    WRK-MESMAQ          TO  WRK-MESENV                   
           MOVE    WRK-ANOMAQ          TO  WRK-ANOENV                   
           MOVE    WRK-SECMAQ          TO  WRK-SECENV.                  
      *================================================================*
       0070-CHAMA.                                                      
      *================================================================*
           MOVE    X'00007D'           TO  WRK-PRAZO-R                  
           CALL    'BRAD0025'          USING WRK-DATENV                 
                                             WRK-PRAZO                  
                                             WRK-DATREC                 
      *                                                                 
           IF      WRK-ENTINV-R        LESS WRK-DATREC-R                
               GO TO 0080-DTINCON                                       
           END-IF                                                       
      *                                                                 
           MOVE    X'00007C'           TO  WRK-PRAZO-R                  
           CALL    'BRAD0025'          USING WRK-DATENV                 
                                             WRK-PRAZO                  
                                             WRK-DATREC                 
           IF      WRK-ENTINV-R        NOT GREATER WRK-DATREC-R         
               GO TO 0090-MOVDATB                                       
           END-IF.                                                      
      *================================================================*
       0080-DTINCON.                                                    
      *================================================================*
           DISPLAY                                                      
           '*CLLP0600* DATA TECLADA ESTA INCONSISTENTE'                 
           UPON OPERADOR                                                
           GO TO 0050-PEDENOVO.                                         
      *================================================================*
       0090-MOVDATB.                                                    
      *================================================================*
           MOVE    WRK-ENTNORM         TO  WRK-ENTRADA.                 
      *================================================================*
       0100-CRIATAB.                                                    
      *================================================================*
           MOVE    1                   TO WRK-INDR5                     
           MOVE    13000               TO WRK-INDR9                     
           OPEN    INPUT                                                
                   ARQTAB                                               
           MOVE    WRK-ABERTURA        TO  WRK-OPERACAO                 
           PERFORM 9030-TESTAR-FS-ARQTAB THRU 9030-99-FIM.              
      *================================================================*
       0110-LERTAB.                                                     
      *================================================================*
           READ    ARQTAB                                               
           END-READ                                                     
           MOVE    WRK-LEITURA         TO  WRK-OPERACAO                 
           IF      WRK-FS-ARQTAB       EQUAL '10'                       
               GO TO 0130-FIMTAB                                        
           ELSE                                                         
               PERFORM 9030-TESTAR-FS-ARQTAB THRU 9030-99-FIM           
           END-IF                                                       
      *                                                                 
      *    * SE EMPRESA = 4120 OU 5201 DESPREZA CARTEIRA 288           *
      *                                                                 
           MOVE    FD-REG-ARQTAB(1:3)  TO  WRK-05-RED                   
           IF      WRK-05              EQUAL  4120 OR 5201              
               GO TO 0112-PULA4120                                      
           END-IF                                                       
      *                                                                 
      *    ** SE EMPRESA = 7000  NAO DESPREZA CARTEIRA 288             *
      *                                                                 
CPMCAC     IF      WRK-05              NOT EQUAL  7000                  
               GO TO 0113-PULA7000                                      
           END-IF.                                                      
      *================================================================*
       0111-VER288.                                                     
      *================================================================*
           IF      FD-REG-ARQTAB(7:3)  NOT EQUAL '288'                  
               GO TO 0110-LERTAB                                        
           END-IF                                                       
      *                                                                 
           IF      FD-REG-ARQTAB(7:3)  EQUAL '288'                      
               GO TO 0113-PULA7000                                      
           END-IF.                                                      
      *================================================================*
       0112-PULA4120.                                                   
      *================================================================*
           IF      FD-REG-ARQTAB(7:3)  EQUAL '288'                      
               GO TO 0110-LERTAB                                        
           END-IF.                                                      
      *================================================================*
       0113-PULA7000.                                                   
      *================================================================*
           MOVE    FD-REG-ARQTAB(1:13) TO  WRK-TABELA(WRK-INDR5:13)     
      *                                                                 
           MOVE    WRK-TABELA(WRK-INDR5 + 3:3)                          
                                       TO  WRK-05-RED                   
           IF      WRK-05              GREATER 6999                     
               GO TO 0110-LERTAB                                        
           END-IF                                                       
      *                                                                 
           ADD     13                  TO  WRK-INDR5                    
           SUBTRACT 1                  FROM WRK-INDR9                   
      *                                                                 
           IF      WRK-INDR9           GREATER ZEROS                    
               GO TO 0110-LERTAB                                        
           END-IF.                                                      
      *================================================================*
       0120-ESTOURO.                                                    
      *================================================================*
           DISPLAY                                                      
           '******************************************************'     
           UPON    OPERADOR                                             
           DISPLAY                                                      
           '** CLLP0600 ** SR. OPERADOR,  TABELA EXCEDEU 13000   *'     
           UPON    OPERADOR                                             
           DISPLAY                                                      
           '*              OCORRENCIAS. FAVOR ENTRAR EM CONTATO  *'     
           UPON    OPERADOR                                             
           DISPLAY                                                      
           '*              COM O ANALISTA RESPONSAVEL.           *'     
           UPON    OPERADOR                                             
           DISPLAY '* TECLE "CIENTE" PARA CANCELAR PROGRAMA *'          
           UPON    OPERADOR                                             
           ACCEPT  WRK-PEGA            FROM CONSOLE                     
           IF      WRK-PEGA            EQUAL 'CIENTE'                   
           GO TO 0580-RETORNA                                           
           END-IF                                                       
           GO TO 0120-ESTOURO.                                          
      *================================================================*
       0130-FIMTAB.                                                     
      *================================================================*
           CLOSE   ARQTAB                                               
                   ARQDATA                                              
           MOVE    WRK-FECHAMENTO      TO  WRK-OPERACAO                 
           PERFORM 9030-TESTAR-FS-ARQTAB  THRU 9030-99-FIM              
           PERFORM 9010-TESTAR-FS-ARQDATA THRU 9010-99-FIM              
      *                                                                 
           MOVE    '*'                 TO  WRK-TABELA(WRK-INDR5:1)      
      *                                                                 
           OPEN    INPUT   MOVIM                                        
                   OUTPUT  DIGIT                                        
                           RELCONS                                      
                           ARQCGC                                       
                           LISTACR                                      
           MOVE    WRK-ABERTURA        TO  WRK-OPERACAO                 
           PERFORM 9020-TESTAR-FS-MOVIM   THRU 9020-99-FIM              
           PERFORM 9040-TESTAR-FS-DIGIT   THRU 9040-99-FIM              
           PERFORM 9050-TESTAR-FS-RELCONS THRU 9050-99-FIM              
           PERFORM 9060-TESTAR-FS-ARQCGC  THRU 9060-99-FIM              
           PERFORM 9070-TESTAR-FS-LISTACR THRU 9070-99-FIM.             
                                                                        
      *================================================================*
       0140-LERMOV.                                                     
      *================================================================*
           READ    MOVIM               INTO WRK-MOVREG                  
           END-READ                                                     
           MOVE    WRK-LEITURA         TO  WRK-OPERACAO                 
           IF      WRK-FS-MOVIM        EQUAL '10'                       
               GO TO 0570-FIMOV                                         
           ELSE                                                         
               PERFORM 9020-TESTAR-FS-MOVIM THRU 9020-99-FIM            
           END-IF                                                       
      *                                                                 
           IF      WRK-MOVFIXO         NOT EQUAL '99'                   
               GO TO 0190-NORMAL                                        
           END-IF.                                                      
      *================================================================*
       0150-MTACR.                                                      
      *================================================================*
           MOVE    WRK-MOVCOAGE-R      TO  WRK-WKAGCR                   
           MOVE    WRK-MOVCOAGE        TO  WRK-LAGCR                    
           MOVE    WRK-MOVNUCTA-R      TO  WRK-WKCCCR                   
           MOVE    WRK-WKCCCR          TO  WRK-LCCCR                    
BRQ141*    MOVE    WRK-MOVCART-R       TO  WRK-WKCARCR                  
BRQ141     MOVE    WRK-MOVCART         TO  WRK-WKCARCR                  
           MOVE    WRK-MOVCART         TO  WRK-LCARTCR                  
           MOVE    WRK-MOVNUTIT-R      TO  WRK-WKCONCR                  
           MOVE    WRK-WKCONCR         TO  WRK-LCONTCR                  
CPMCAC     MOVE    WRK-MOVVENC(1:5)    TO  WRK-WKVCTOCR-R               
CPMCAC     MOVE    WRK-WKVCTOCR        TO  WRK-09                       
CPMCAC     MOVE    WRK-09-RED(2:8)     TO  WRK-LVCTOCR                  
           MOVE    WRK-MOVVTIT-R       TO  WRK-WKVALCR                  
           MOVE    WRK-MOVIOF-R        TO  WRK-WKVRIOF                  
           SUBTRACT WRK-WKVRIOF        FROM  WRK-WKVALCR                
      *                                                                 
CPMCAC     MOVE    WRK-WKVALCR         TO  WRK-V99                      
           MOVE    WRK-V99-RED         TO  WRK-LVRPRICR                 
           MOVE    WRK-MOVVTIT-R       TO  WRK-WKDEBCR                  
           MOVE    WRK-MOVVRCON-R      TO  WRK-WKAUXVR                  
           ADD     WRK-WKAUXVR         TO  WRK-WKDEBCR                  
      *                                                                 
CPMCAC     MOVE    WRK-WKDEBCR         TO  WRK-V99                      
CPMCAC     MOVE    WRK-V99-RED         TO  WRK-LVRATUCR                 
           MOVE    WRK-MOVNODEV        TO  WRK-WKNOMECR                 
           MOVE    WRK-WKNOMECR        TO  WRK-LNOMECR                  
           MOVE    '0'                 TO  WRK-WKIDCR                   
           MOVE    SPACES              TO  WRK-WKRAMOCR                 
           MOVE    WRK-MOVNCGC-R       TO  WRK-WKCGCCR                  
           MOVE    WRK-MOVFCGC-R       TO  WRK-WKFILCR                  
           MOVE    WRK-MOVCCGC         TO  WRK-WKCTRCR                  
CPMCAC     MOVE    WRK-WKAGCR          TO  WRK-55                       
      *                                                                 
           CALL    'BRAD1200'          USING WRK-WKAGCR                 
                                             WRK-CODDIR-RED             
                                             WRK-NOMEDIR                
                                             WRK-JUNCAO-RED             
                                             WRK-NOMEAG                 
           IF      RETURN-CODE         NOT EQUAL   ZEROS                
               GO  TO  0160-ERRO                                        
           END-IF                                                       
      *                                                                 
           MOVE    WRK-CODDIR-RED      TO  WRK-WKDIRRCR                 
           MOVE    WRK-JUNCAO-RED      TO  WRK-WKJDIRCR                 
           GO TO 0170-GRAVACGC.                                         
      *================================================================*
       0160-ERRO.                                                       
      *================================================================*
           MOVE    X'000F'             TO  WRK-WKDIRRCR-R               
           MOVE    X'00000F'           TO  WRK-WKJDIRCR-R.              
      *================================================================*
       0170-GRAVACGC.                                                   
      *================================================================*
           WRITE   FD-REG-ARQCGC       FROM WRK-WKARQCR                 
           MOVE    WRK-GRAVACAO        TO  WRK-OPERACAO                 
               PERFORM 9060-TESTAR-FS-ARQCGC THRU 9060-99-FIM           
      *                                                                 
           IF      WRK-QTDECR          LESS 60                          
               GO TO 0180-NQBCR                                         
           END-IF                                                       
      *                                                                 
           WRITE   FD-REG-LISTACR      FROM WRK-LCABCR0                 
           MOVE    WRK-GRAVACAO        TO  WRK-OPERACAO                 
               PERFORM 9070-TESTAR-FS-LISTACR THRU 9070-99-FIM          
      *                                                                 
           WRITE   FD-REG-LISTACR      FROM WRK-LCABCR1                 
           MOVE    WRK-GRAVACAO        TO  WRK-OPERACAO                 
               PERFORM 9070-TESTAR-FS-LISTACR THRU 9070-99-FIM          
      *                                                                 
           WRITE   FD-REG-LISTACR      FROM WRK-LCABCR2                 
           MOVE    WRK-GRAVACAO        TO  WRK-OPERACAO                 
               PERFORM 9070-TESTAR-FS-LISTACR THRU 9070-99-FIM          
      *                                                                 
           MOVE    ZEROS               TO  WRK-QTDECR.                  
      *================================================================*
       0180-NQBCR.                                                      
      *================================================================*
           ADD     1                   TO  WRK-QTDECR                   
           WRITE   FD-REG-LISTACR      FROM WRK-LDETCR                  
           MOVE    WRK-GRAVACAO        TO  WRK-OPERACAO                 
               PERFORM 9070-TESTAR-FS-LISTACR THRU 9070-99-FIM          
      *                                                                 
           GO TO 0140-LERMOV.                                           
      *================================================================*
       0190-NORMAL.                                                     
      *================================================================*
           MOVE    '000000000000000'   TO  WRK-VALVINC.                 
      *================================================================*
       0200-CARGATB.                                                    
      *================================================================*
           MOVE    1                   TO  WRK-INDR5B.                  
      *================================================================*
       0210-CLCCART.                                                    
      *================================================================*
           IF      WRK-MOVCART         EQUAL WRK-TABCONV(WRK-INDR5B:3)  
               GO TO 0220-CONV                                          
           END-IF                                                       
      *                                                                 
           ADD     6                   TO  WRK-INDR5B                   
           IF      WRK-TABCONV(WRK-INDR5B:1)                            
                                       NOT EQUAL '*'                    
               GO TO 0210-CLCCART                                       
           END-IF                                                       
           GO TO 0230-CONTINUA.                                         
      *================================================================*
       0220-CONV.                                                       
      *================================================================*
           MOVE    WRK-TABCONV(WRK-INDR5B + 3:3)                        
                                       TO  WRK-MOVCART.                 
      *================================================================*
       0230-CONTINUA.                                                   
      *================================================================*
           MOVE    WRK-MOVDTINI        TO  WRK-OPER                     
           MOVE    SPACES              TO  WRK-WKDATAR                  
           MOVE    WRK-MOVDTINI(1:8)   TO  WRK-WKDATAR(3:8)             
      *                                                                 
           CALL    'BRAD1570'          USING WRK-WKDATAR                
                                             WRK-WKDATAF                
                                             WRK-WKMENS                 
           IF      RETURN-CODE         EQUAL  ZEROS                     
               GO  TO  0240-SEGY                                        
           END-IF                                                       
      *                                                                 
           MOVE    '00000000'          TO  WRK-OPER.                    
      *================================================================*
       0240-SEGY.                                                       
      *================================================================*
           MOVE    WRK-MOVNODEV        TO  WRK-NOMED                    
           MOVE    SPACES              TO  WRK-MOVDUPRO(29:2)           
           MOVE    '@'                 TO  WRK-NOMED(40:1).             
      *================================================================*
       0250-NAOMORA.                                                    
      *================================================================*
           MOVE    4                   TO  WRK-INDR7                    
           MOVE    1                   TO  WRK-INDR6.                   
      *================================================================*
       0260-VOLTA.                                                      
      *================================================================*
           IF      WRK-MOVMOTIV        EQUAL WRK-TABREG(WRK-INDR6: 2)   
               GO TO 0270-ACHOU                                         
           END-IF                                                       
      *                                                                 
           ADD     3                   TO  WRK-INDR6                    
           SUBTRACT 1                  FROM WRK-INDR7                   
      *                                                                 
           IF      WRK-INDR7           GREATER ZEROS                    
               GO TO 0260-VOLTA                                         
           END-IF                                                       
      *                                                                 
           MOVE    '00'                TO  WRK-MOVMOTIV                 
           GO TO 0250-NAOMORA.                                          
      *================================================================*
       0270-ACHOU.                                                      
      *================================================================*
           MOVE    1                   TO  WRK-INDR10                   
           MOVE    24                  TO  WRK-INDR9B.                  
      *================================================================*
       0280-PROCURA.                                                    
      *================================================================*
           IF      WRK-MOVTPGAR        EQUAL                            
                                           WRK-TABTIPO(WRK-INDR10:2)    
               GO TO 0290-TIPOK                                         
           END-IF                                                       
      *                                                                 
           ADD     2                   TO  WRK-INDR10                   
           SUBTRACT 1                  FROM WRK-INDR9B                  
      *                                                                 
           IF      WRK-INDR9B          GREATER ZEROS                    
               GO TO 0280-PROCURA                                       
           END-IF.                                                      
      *================================================================*
       0290-TIPOK.                                                      
      *================================================================*
           MOVE    WRK-MOVTPGAR        TO WRK-TIPOGAR                   
           MOVE    WRK-TABREG(WRK-INDR6 + 2:1)                          
                                       TO  WRK-CONTAB                   
           MOVE    WRK-MOVCOAGE(1:4)   TO  WRK-AGENCIA(2:4)             
           MOVE    WRK-MOVNUCTA        TO  WRK-CONTA                    
           MOVE    WRK-MOVVENC         TO  WRK-VENCIM                   
           MOVE    WRK-MOVCART         TO  WRK-ANOOP                    
           MOVE    WRK-MOVNUTIT        TO  WRK-NUMOP                    
           MOVE    WRK-MOVMARCA        TO  WRK-MARCA                    
      *                                                                 
           IF      WRK-MARCA           EQUAL 'Y'                        
               GO TO 0300-PULA                                          
           END-IF                                                       
      *                                                                 
           MOVE    SPACES              TO  WRK-MARCA.                   
      *================================================================*
       0300-PULA.                                                       
      *================================================================*
           MOVE    WRK-MOVIOF-R        TO  WRK-WKVRIOF                  
           MOVE    WRK-MOVVTIT-R       TO  WRK-VLRTIT                   
           MOVE    WRK-MOVVRCON-R      TO  WRK-VLRENC                   
CPMCAC     MOVE    WRK-VLRENC          TO  WRK-15                       
CPMCAC     MOVE    WRK-15-RED          TO  WRK-VALRES                   
      *                                                                 
           IF      WRK-MOVFIXO         NOT EQUAL '14'                   
               GO TO 0310-VEIOF                                         
           END-IF                                                       
      *                                                                 
           MOVE    WRK-MOVIOF          TO  WRK-VALVINC                  
           MOVE    '000000000000000'   TO  WRK-MOVIOF                   
           MOVE    X'000000000000000F' TO  WRK-WKVRIOF-R.               
      *================================================================*
       0310-VEIOF.                                                      
      *================================================================*
           SUBTRACT WRK-WKVRIOF        FROM  WRK-VLRTIT                 
CPMCAC     MOVE    WRK-VLRTIT          TO  WRK-15                       
CPMCAC     MOVE    WRK-15-RED          TO  WRK-VALOP                    
           MOVE    WRK-MOVDTINI        TO  WRK-DDMMAA                   
           MOVE    WRK-DIANOR          TO  WRK-DIAINV                   
           MOVE    WRK-MESNOR          TO  WRK-MESINV                   
           MOVE    WRK-ANONOR          TO  WRK-ANOINV                   
      *                                                                 
           MOVE    WRK-AAMMDD          TO  WRK-08                       
           MOVE    WRK-08-RED          TO  WRK-DTOPEINV                 
           MOVE    WRK-MOVVENC         TO  WRK-DDMMAA                   
           MOVE    WRK-DIANOR          TO  WRK-DIAINV                   
           MOVE    WRK-MESNOR          TO  WRK-MESINV                   
           MOVE    WRK-ANONOR          TO  WRK-ANOINV                   
           MOVE    WRK-AAMMDD          TO  WRK-08                       
           MOVE    WRK-08-RED          TO  WRK-DTVCTINV.                
      *================================================================*
       0320-EHNOVO.                                                     
      *================================================================*
           MOVE    WRK-VLRTIT          TO  WRK-V99                      
           MOVE    WRK-V99-ALP         TO  WRK-WKVRTIT2                 
           MOVE    WRK-WKVRTIT2        TO  WRK-VALINIC.                 
      *================================================================*
       0330-VEIOF2.                                                     
      *================================================================*
           MOVE    WRK-MOVIOF          TO  WRK-VALORIOF                 
           MOVE    WRK-MOVMOEDA        TO  WRK-MOEDA                    
      *                                                                 
           IF      WRK-MOEDA           EQUAL 'CR'                       
               GO TO 0340-SEGUE                                         
           END-IF                                                       
           MOVE    'CZ'                TO  WRK-MOEDA.                   
      *================================================================*
       0340-SEGUE.                                                      
      *================================================================*
           MOVE    WRK-MOVDUPRO        TO  WRK-GARANTIA                 
           MOVE    SPACES              TO  WRK-GARANTIA(31:2)           
           MOVE    WRK-MOVVTIT-R       TO  WRK-PVAL1                    
           MOVE    WRK-PVAL1           TO  WRK-15                       
           MOVE    WRK-15-RED          TO  WRK-VRBASE                   
           MOVE    WRK-MOVSIGLA        TO  WRK-SIGLA                    
      *                                                                 
           IF      WRK-MOVSIGLA(1:3)   NOT EQUAL 'PCP'                  
               GO TO 0350-VE202                                         
           END-IF                                                       
      *                                                                '
           MOVE    'PCD '              TO  WRK-MOVSIGLA.                
      *================================================================*
       0350-VE202.                                                      
      *================================================================*
           CONTINUE.                                                    
      *================================================================*
       0360-MVCVRENC.                                                   
      *================================================================*
           MOVE    WRK-MOVENCCT        TO  WRK-VALVENC.                 
      *================================================================*
       0370-PESQPEND.                                                   
      *================================================================*
           MOVE    1                   TO  WRK-INDR5.                   
      *================================================================*
       0380-COMPARA.                                                    
      *================================================================*
           IF      WRK-TABELA(WRK-INDR5:1)                              
                                       EQUAL '*'                        
               GO TO 0390-PESQ02                                        
           END-IF                                                       
      *                                                                 
           IF      WRK-TABELA(WRK-INDR5 + 6:3)                          
                                       EQUAL WRK-MOVCART                
               GO TO 0420-ACHEI                                         
           END-IF                                                       
      *                                                                 
      ******F      WRK-MOVSIGLA        EQUAL                            
      ******                               WRK-TABELA(WRK-INDR5 + 9:4)  
      **********O TO 0420-ACHEI                                         
      ******ND-IF                                                       
      *                                                                 
           ADD     13                  TO  WRK-INDR5                    
           GO TO 0380-COMPARA.                                          
      *================================================================*
       0390-PESQ02.                                                     
      *================================================================*
           MOVE    1                   TO  WRK-INDR5.                   
      *================================================================*
       0400-COMP02.                                                     
      *================================================================*
           IF      WRK-TABELA(WRK-INDR5:1)                              
                                       EQUAL '*'                        
               GO TO 0410-ZERA                                          
           END-IF                                                       
      *                                                                 
           IF WRK-TABELA(WRK-INDR5 + 6:3)                               
                                       EQUAL WRK-MOVCART                
               GO TO 0490-ACHEI1                                        
           END-IF                                                       
      *                                                                 
      ******F      WRK-MOVSIGLA        EQUAL                            
      ******                               WRK-TABELA(WRK-INDR5 + 9:4)  
      **********O TO 0490-ACHEI1                                        
      ******ND-IF                                                       
      *                                                                 
           ADD     13                  TO  WRK-INDR5                    
           GO TO 0400-COMP02.                                           
      *================================================================*
       0410-ZERA.                                                       
      *================================================================*
           MOVE    '00000'             TO  WRK-FIRMA                    
           MOVE    '0'                 TO  WRK-IDENT(1:1)               
           MOVE    '000'               TO  WRK-CART                     
           GO TO 0500-RESTO.                                            
      *================================================================*
       0420-ACHEI.                                                      
      *================================================================*
CPMCAC     MOVE    WRK-TABELA(WRK-INDR5:3)                              
CPMCAC                                 TO  WRK-05-RED                   
CPMCAC     IF      WRK-05              EQUAL  4120                      
               GO TO 0430-VEDTINI                                       
           END-IF                                                       
      *                                                                 
           IF      WRK-05              NOT EQUAL 4900                   
               GO TO 0450-FAIXA000                                      
           END-IF.                                                      
      *================================================================*
       0430-VEDTINI.                                                    
      *================================================================*
           IF      WRK-DTOPEINV        NOT LESS 19941021                
               GO TO 0440-VECART                                        
           END-IF                                                       
      *                                                                 
           IF      WRK-DTVCTINV        LESS 19941021                    
               GO TO 0450-FAIXA000                                      
           END-IF.                                                      
      *================================================================*
       0440-VECART.                                                     
      *================================================================*
           IF      WRK-MOVNATU         EQUAL '005'                      
               GO TO 0470-FAIXA200                                      
           END-IF                                                       
      *                                                                 
           IF      WRK-MOVNATU         EQUAL '008'                      
               GO TO 0470-FAIXA200                                      
           END-IF                                                       
      *                                                                 
           IF      WRK-MOVNATU         EQUAL '016'                      
               GO TO 0470-FAIXA200                                      
           END-IF                                                       
      *                                                                 
           IF      WRK-MOVNATU         EQUAL '019'                      
               GO TO 0470-FAIXA200                                      
           END-IF                                                       
      *                                                                 
           IF      WRK-MOVNATU         EQUAL '025'                      
               GO TO 0470-FAIXA200                                      
           END-IF                                                       
      *                                                                 
           IF      WRK-MOVNATU         EQUAL '031'                      
               GO TO 0470-FAIXA200                                      
           END-IF                                                       
      *                                                                 
           IF      WRK-MOVNATU         EQUAL '033'                      
               GO TO 0470-FAIXA200                                      
           END-IF                                                       
      *                                                                 
           IF      WRK-MOVNATU         EQUAL '036'                      
               GO TO 0470-FAIXA200                                      
           END-IF                                                       
      *                                                                 
           IF      WRK-MOVNATU         EQUAL '044'                      
               GO TO 0470-FAIXA200                                      
           END-IF                                                       
      *                                                                 
           IF      WRK-MOVNATU         EQUAL '047'                      
               GO TO 0470-FAIXA200                                      
           END-IF                                                       
      *                                                                 
           IF      WRK-MOVNATU         EQUAL '064'                      
               GO TO 0470-FAIXA200                                      
           END-IF                                                       
      *                                                                 
           IF      WRK-DTOPEINV        LESS 19941021                    
               GO TO 0460-FAIXA100                                      
           END-IF                                                       
      *                                                                 
           IF      WRK-DTVCTINV        GREATER WRK-DTMOVINV             
               GO TO 0470-FAIXA200                                      
           END-IF                                                       
           GO TO 0460-FAIXA100.                                         
      *================================================================*
       0450-FAIXA000.                                                   
      *================================================================*
CPMCAC     MOVE    WRK-TABELA(WRK-INDR5 + 3:3)                          
CPMCAC                                 TO  WRK-05-RED                   
CPMCAC     MOVE    WRK-05              TO  WRK-95                       
CPMCAC     MOVE    WRK-95-RED          TO  WRK-AUXPEND                  
      *                                                                 
CPMCAC     IF      WRK-05              EQUAL ZEROS                      
               GO TO 0480-COMPARA1                                      
           END-IF                                                       
      *                                                                 
           IF      WRK-AUXPEND(3:1)    EQUAL '0'                        
               GO TO 0490-ACHEI1                                        
           END-IF                                                       
           GO TO 0480-COMPARA1.                                         
      *================================================================*
       0460-FAIXA100.                                                   
      *================================================================*
CPMCAC     MOVE    WRK-TABELA(WRK-INDR5 + 3:3)                          
CPMCAC                                 TO  WRK-05-RED                   
CPMCAC     MOVE    WRK-05              TO  WRK-95                       
CPMCAC     MOVE    WRK-95-RED          TO  WRK-AUXPEND                  
      *                                                                 
           IF      WRK-AUXPEND(3:1)    EQUAL '1'                        
               GO TO 0490-ACHEI1                                        
           END-IF                                                       
           GO TO 0480-COMPARA1.                                         
      *================================================================*
       0470-FAIXA200.                                                   
      *================================================================*
CPMCAC     MOVE    WRK-TABELA(WRK-INDR5 + 3:3)                          
CPMCAC                                 TO  WRK-05-RED                   
CPMCAC     MOVE    WRK-05              TO  WRK-95                       
CPMCAC     MOVE    WRK-95-RED          TO  WRK-AUXPEND                  
      *                                                                 
           IF      WRK-AUXPEND(3:1)    EQUAL '2'                        
               GO TO 0490-ACHEI1                                        
           END-IF.                                                      
      *================================================================*
       0480-COMPARA1.                                                   
      *================================================================*
           ADD     13                  TO  WRK-INDR5                    
           GO TO 0380-COMPARA.                                          
      *================================================================*
       0490-ACHEI1.                                                     
      *================================================================*
           MOVE    WRK-TABELA(WRK-INDR5:3)                              
CPMCAC                                 TO  WRK-05-RED                   
CPMCAC     MOVE    WRK-05              TO  WRK-95                       
CPMCAC     MOVE    WRK-95-RED          TO  WRK-FIRMA                    
CPMCAC     MOVE    WRK-TABELA(WRK-INDR5 + 3:3)                          
CPMCAC                                 TO  WRK-05-RED                   
CPMCAC     MOVE    WRK-05              TO  WRK-95                       
CPMCAC     MOVE    WRK-95-RED(2:4)     TO  WRK-IDENT                    
           MOVE    WRK-TABELA(WRK-INDR5 + 6:3)                          
                                       TO  WRK-CART                     
           MOVE    WRK-MOVDTMOV        TO  WRK-DTAMOVTO.                
      *================================================================*
       0500-RESTO.                                                      
      *================================================================*
           MOVE    WRK-MOVCOAGE        TO  WRK-AGRESP                   
      *                                                                 
CPMCAC     MOVE    WRK-AGRESP          TO  WRK-04                       
CPMCAC     IF      WRK-04-RED          LESS 4900                        
               GO TO 0510-AGINCON                                       
           END-IF                                                       
      *                                                                 
CPMCAC     IF      WRK-04-RED          GREATER 4959                     
               GO TO 0510-AGINCON                                       
           END-IF                                                       
      *                                                                 
           IF      WRK-MOVSIGLA        IS NOT  NUMERIC                  
               GO TO 0510-AGINCON                                       
           END-IF                                                       
      *                                                                 
           MOVE    WRK-MOVSIGLA        TO  WRK-AGRESP.                  
      *================================================================*
       0510-AGINCON.                                                    
      *================================================================*
           MOVE    WRK-MOVCOOB1        TO  WRK-COOB0                    
           MOVE    WRK-MOVCOOB1        TO  WRK-COOB0                    
           MOVE    WRK-MOVCOOB2        TO  WRK-COOB1                    
           MOVE    WRK-MOVCGC          TO  WRK-CGC                      
           MOVE    WRK-MOVFIXO         TO  WRK-FIXO                     
      *                                                                 
           IF      WRK-FIRMA           NOT EQUAL '05240'                
               GO TO 0520-VENATU                                        
           END-IF                                                       
      *                                                                 
           IF      WRK-MOVNATU         NOT EQUAL '050'                  
               GO TO 0540-LISTAR                                        
           END-IF                                                       
           GO TO 0540-LISTAR.                                           
      *================================================================*
       0520-VENATU.                                                     
      *================================================================*
           IF      WRK-MOVNATU         EQUAL '025'                      
               GO TO 0530-EGAR98                                        
           END-IF                                                       
      *                                                                 
           IF      WRK-MOVNATU         EQUAL '032'                      
               GO TO 0530-EGAR98                                        
           END-IF                                                       
      *                                                                 
           IF      WRK-MOVNATU         EQUAL '064'                      
               GO TO 0530-EGAR98                                        
           END-IF                                                       
      *                                                                 
           IF      WRK-MOVNATU         EQUAL '090'                      
               GO TO 0530-EGAR98                                        
           END-IF                                                       
      *                                                                 
           IF      WRK-MOVNATU         NOT EQUAL '091'                  
               GO TO 0540-LISTAR                                        
           END-IF.                                                      
      *================================================================*
       0530-EGAR98.                                                     
      *================================================================*
           CONTINUE.                                                    
      *================================================================*
       0540-LISTAR.                                                     
      *================================================================*
           PERFORM 0550-LISTAREL       THRU  0560-99-FIM                
           WRITE   FD-REG-DIGIT        FROM WRK-REGMOVTO                
           MOVE    WRK-GRAVACAO        TO  WRK-OPERACAO                 
               PERFORM 9040-TESTAR-FS-DIGIT THRU 9040-99-FIM            
      *                                                                 
           GO TO 0140-LERMOV.                                           
      *================================================================*
       0550-LISTAREL.                                                   
      *================================================================*
           IF      WRK-ACLINHA         LESS 21                          
               GO TO 0560-LISTREL                                       
           END-IF                                                       
      *                                                                 
           ADD     1                   TO  WRK-ACPAGIN                  
           MOVE    WRK-ACPAGIN         TO  WRK-PAGCAB1                  
      *                                                                 
           WRITE   FD-REG-RELCONS      FROM WRK-CABEC1                  
           MOVE    WRK-GRAVACAO        TO  WRK-OPERACAO                 
               PERFORM 9050-TESTAR-FS-RELCONS THRU 9050-99-FIM          
      *                                                                 
           WRITE   FD-REG-RELCONS      FROM WRK-CABEC2                  
           MOVE    WRK-GRAVACAO        TO  WRK-OPERACAO                 
               PERFORM 9050-TESTAR-FS-RELCONS THRU 9050-99-FIM          
      *                                                                 
           WRITE   FD-REG-RELCONS      FROM WRK-CABEC3                  
           MOVE    WRK-GRAVACAO        TO  WRK-OPERACAO                 
               PERFORM 9050-TESTAR-FS-RELCONS THRU 9050-99-FIM          
      *                                                                 
           SUBTRACT WRK-ACLINHA        FROM  WRK-ACLINHA.               
      *================================================================*
       0560-LISTREL.                                                    
      *================================================================*
           MOVE    WRK-FIRMA(2:4)      TO  WRK-DETEMPR(1:4)             
           MOVE    WRK-AGENCIA(2:4)    TO  WRK-DETAGEN(1:4)             
           MOVE    WRK-CONTA           TO  WRK-DETCONTA                 
           MOVE    WRK-VENCIM          TO  WRK-DETVENCT                 
           MOVE    WRK-OPER            TO  WRK-DETDTINI                 
           MOVE    WRK-DTAMOVTO        TO  WRK-DETDTMOV                 
           MOVE    WRK-IDENT           TO  WRK-DETTPEND                 
           MOVE    WRK-ANOOP           TO  WRK-DETCART                  
           MOVE    WRK-CART            TO  WRK-DETCARTO                 
           MOVE    WRK-NUMOP           TO  WRK-DETCONTR                 
           MOVE    WRK-VALOP-R         TO  WRK-WKVALOR                  
CPMCAC     MOVE    WRK-WKVALOR         TO  WRK-V99                      
CPMCAC     MOVE    WRK-V99-RED         TO  WRK-DETVROP                  
           MOVE    WRK-VALRES-R        TO  WRK-WKVALOR                  
CPMCAC     MOVE    WRK-WKVALOR         TO  WRK-V99                      
CPMCAC     MOVE    WRK-V99-RED         TO  WRK-DETVRCON                 
           MOVE    WRK-VALINIC-R       TO  WRK-WKVALOR                  
CPMCAC     MOVE    WRK-WKVALOR         TO  WRK-V99                      
CPMCAC     MOVE    WRK-V99-RED         TO  WRK-DETVRDEV                 
           MOVE    WRK-VALORIOF-R      TO  WRK-WKVALOR                  
CPMCAC     MOVE    WRK-WKVALOR         TO  WRK-V99                      
CPMCAC     MOVE    WRK-V99-RED         TO  WRK-DETVRIOF                 
           MOVE    WRK-MOVENCCT-R      TO  WRK-WKVALOR                  
CPMCAC     MOVE    WRK-WKVALOR         TO  WRK-V99                      
CPMCAC     MOVE    WRK-V99-RED         TO  WRK-DETVRVEN                 
           MOVE    WRK-MOVENCSO-R      TO  WRK-WKVALOR                  
CPMCAC     MOVE    WRK-WKVALOR         TO  WRK-V99                      
CPMCAC     MOVE    WRK-V99-RED         TO  WRK-DETVR59                  
           MOVE    WRK-VALVINC-R       TO  WRK-WKVALOR                  
CPMCAC     MOVE    WRK-WKVALOR         TO  WRK-V99                      
CPMCAC     MOVE    WRK-V99-RED         TO  WRK-DETVRVIN                 
           MOVE    WRK-NOMED(1:39)     TO  WRK-DETNOME(1:39)            
           MOVE    WRK-MOVFIXO         TO  WRK-DETTIPO                  
      *                                                                 
           ADD     1                   TO  WRK-ACLINHA                  
           WRITE   FD-REG-RELCONS      FROM WRK-LINDET                  
           MOVE    WRK-GRAVACAO        TO  WRK-OPERACAO                 
               PERFORM 9050-TESTAR-FS-RELCONS THRU 9050-99-FIM          
      *                                                                 
           WRITE   FD-REG-RELCONS      FROM WRK-LINDET2                 
           MOVE    WRK-GRAVACAO        TO  WRK-OPERACAO                 
               PERFORM 9050-TESTAR-FS-RELCONS THRU 9050-99-FIM.         
      *                                                                 
       0560-99-FIM.                    EXIT.                            
      *================================================================*
       0570-FIMOV.                                                      
      *================================================================*
           CLOSE   MOVIM                                                
                   DIGIT                                                
                   RELCONS                                              
                   ARQCGC                                               
                   LISTACR                                              
           MOVE    WRK-FECHAMENTO      TO  WRK-OPERACAO                 
           PERFORM 9000-TESTAR-FILE-STATUS THRU 9000-99-FIM.            
      *================================================================*
       0580-RETORNA.                                                    
      *================================================================*
            GOBACK.                                                     
      *----------------------------------------------------------------*
       9000-TESTAR-FILE-STATUS.                                         
      *----------------------------------------------------------------*
           PERFORM 9010-TESTAR-FS-ARQDATA THRU 9010-99-FIM              
           PERFORM 9020-TESTAR-FS-MOVIM THRU 9020-99-FIM                
           PERFORM 9030-TESTAR-FS-ARQTAB THRU 9030-99-FIM               
           PERFORM 9040-TESTAR-FS-DIGIT THRU 9040-99-FIM                
           PERFORM 9050-TESTAR-FS-RELCONS THRU 9050-99-FIM              
           PERFORM 9060-TESTAR-FS-ARQCGC THRU 9060-99-FIM               
           PERFORM 9070-TESTAR-FS-LISTACR THRU 9070-99-FIM.             
      *                                                                 
       9000-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
       9010-TESTAR-FS-ARQDATA.                                          
      *----------------------------------------------------------------*
           IF (WRK-FS-ARQDATA           NOT EQUAL '00')                 
               MOVE 'ARQDATA'          TO WRK-NOME-ARQ                  
               MOVE WRK-FS-ARQDATA     TO WRK-FILE-STATUS               
               MOVE WRK-ERRO-BRAD7100  TO ERR-TEXTO                     
               PERFORM 9999-PROCESSAR-ROTINA-ERRO                       
           END-IF.                                                      
      *                                                                 
       9010-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
       9020-TESTAR-FS-MOVIM.                                            
      *----------------------------------------------------------------*
           IF (WRK-FS-MOVIM            NOT EQUAL '00')                  
               MOVE 'MOVIM'            TO WRK-NOME-ARQ                  
               MOVE WRK-FS-MOVIM       TO WRK-FILE-STATUS               
               MOVE WRK-ERRO-BRAD7100  TO ERR-TEXTO                     
               PERFORM 9999-PROCESSAR-ROTINA-ERRO                       
           END-IF.                                                      
      *                                                                 
       9020-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
       9030-TESTAR-FS-ARQTAB.                                           
      *----------------------------------------------------------------*
           IF (WRK-FS-ARQTAB           NOT EQUAL '00')                  
               MOVE 'ARQTAB'           TO WRK-NOME-ARQ                  
               MOVE WRK-FS-ARQTAB      TO WRK-FILE-STATUS               
               MOVE WRK-ERRO-BRAD7100  TO ERR-TEXTO                     
               PERFORM 9999-PROCESSAR-ROTINA-ERRO                       
           END-IF.                                                      
      *                                                                 
       9030-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
       9040-TESTAR-FS-DIGIT.                                            
      *----------------------------------------------------------------*
           IF (WRK-FS-DIGIT            NOT EQUAL '00')                  
               MOVE 'DIGIT'            TO WRK-NOME-ARQ                  
               MOVE WRK-FS-DIGIT       TO WRK-FILE-STATUS               
               MOVE WRK-ERRO-BRAD7100  TO ERR-TEXTO                     
               PERFORM 9999-PROCESSAR-ROTINA-ERRO                       
           END-IF.                                                      
      *                                                                 
       9040-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
       9050-TESTAR-FS-RELCONS.                                          
      *----------------------------------------------------------------*
           IF (WRK-FS-RELCONS          NOT EQUAL '00')                  
               MOVE 'RELCONS'          TO WRK-NOME-ARQ                  
               MOVE WRK-FS-RELCONS     TO WRK-FILE-STATUS               
               MOVE WRK-ERRO-BRAD7100  TO ERR-TEXTO                     
               PERFORM 9999-PROCESSAR-ROTINA-ERRO                       
           END-IF.                                                      
      *                                                                 
       9050-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
       9060-TESTAR-FS-ARQCGC.                                           
      *----------------------------------------------------------------*
           IF (WRK-FS-ARQCGC           NOT EQUAL '00')                  
               MOVE 'ARQCGC'           TO WRK-NOME-ARQ                  
               MOVE WRK-FS-ARQCGC      TO WRK-FILE-STATUS               
               MOVE WRK-ERRO-BRAD7100  TO ERR-TEXTO                     
               PERFORM 9999-PROCESSAR-ROTINA-ERRO                       
           END-IF.                                                      
      *                                                                 
       9060-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
       9070-TESTAR-FS-LISTACR.                                          
      *----------------------------------------------------------------*
           IF (WRK-FS-LISTACR          NOT EQUAL '00')                  
               MOVE 'LISTACR'          TO WRK-NOME-ARQ                  
               MOVE WRK-FS-LISTACR     TO WRK-FILE-STATUS               
               MOVE WRK-ERRO-BRAD7100  TO ERR-TEXTO                     
               PERFORM 9999-PROCESSAR-ROTINA-ERRO                       
           END-IF.                                                      
      *                                                                 
       9070-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO.                                      
      *----------------------------------------------------------------*
           MOVE 'CLLP0600'             TO ERR-PGM.                      
           MOVE 'APL'                  TO ERR-TIPO-ACESSO               
           CALL 'BRAD7100'             USING WRK-BATCH                  
                                          ERRO-AREA.                    
           GOBACK.                                                      
      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*

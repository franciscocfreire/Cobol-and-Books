000010*===============================================================* 
000020 IDENTIFICATION DIVISION.                                         
000030*===============================================================* 
000040                                                                  
000050 PROGRAM-ID. CLLP7930.                                            
000060 AUTHOR.     FERNANDA (FE).                                       
000070*===============================================================* 
000080*                   C P M   S I S T E M A S                     * 
000090*---------------------------------------------------------------* 
000100*                                                               * 
000110*      PROGRAMA     : CLLP7930                                  * 
000120*      PROGRAMADORA : FERNANDA NUNES RAMALHO  -  CPM            * 
000130*      SUPERVISORA  : VALUERIA                -  CPM            * 
000140*      ANALISTA     : HOMERO TOZETTO          -  GRP. 82        * 
000150*      DATA         : 28/11/1997                                * 
000160*                                                               * 
000170*      OBJETIVO     :                                           * 
000180*        ATUALIZAR CAMPOS DO CADASTRO CLLP ATRAVES MOVICLLP     * 
000190*        (FBNH).                                                * 
000200*                                                               * 
000210*      ARQUIVOS:                                                * 
000220*         DDNAME                           INCLUDE/BOOK         * 
000230*         MVCLLPAT (LRECL = 0500)            I#CLLPFB           * 
000240*         CADACLLP (LRECL = 0600)            I#TTLPAJ           * 
000250*         CADCLLP  (LRECL = 0600)               -               * 
000260*         RELATO   (LRECL = 0080)               -               * 
000270*                                                               * 
000280*      MODULOS CHAMADOS:                                        * 
000290*                                                               * 
000300*        ILBOABN0                                               * 
000310*                                                               * 
000320*===============================================================* 
      *                       A L T E R A C A O                       * 
      *---------------------------------------------------------------* 
      *    PROGRAMADOR.:   RICARDO PICCINI         - CPM PATO BRANCO  * 
      *    ANALISTA CPM:   CLEOMAR DA SILVA        - CPM PATO BRANCO  * 
      *    ANALISTA....:   LUIS AFONSO             - AMS / GP. 70     * 
      *    DATA........:   04/04/2005                                 * 
      *---------------------------------------------------------------* 
      *    OBJETIVO....:   SUBSTITUIR A INC I#CLLPGB PELA I#CLLP11,   * 
      *      ATUALIZAR LAYOUT DE 600 PARA 668 BYTES.                  * 
      *===============================================================* 
BRQ141***************************************************************** 
BRQ141* MAIO/2012 - (BRQ) - TRATAMENTO DE CARTEIRA  ALFANUMERICA      * 
BRQ141***************************************************************** 
BRQLEI*===============================================================* 
      *                       A L T E R A C A O                       * 
      *---------------------------------------------------------------* 
      *                                                               * 
      *   PROGRAMADOR :  ELSINO SILVA       - BRQ IT SERVICES         * 
      *   ANALISTA    :  FRANCISCO FREIRE   - BRQ IT SERVICES         * 
      *   DATA........:  NOVEMBRO / 2014                              * 
      *                                                               * 
      *   OBJETIVO....:  INCLUSAO DOS CAMPOS DA LEI DA TRANSPARENCIA  * 
      *                  NO LAYOUT DO ARQUIVOS MVCLLPAT, ALTERANDO O  * 
      *                  LRECL DE 500 PARA 600 (I#CLLP23)             * 
      *                                                               * 
      *===============================================================* 
BRQLEI***************************************************************** 
           EJECT                                                        
000340*===============================================================* 
000350 ENVIRONMENT DIVISION.                                            
000360*===============================================================* 
000370                                                                  
000380*---------------------------------------------------------------* 
000390 CONFIGURATION SECTION.                                           
000400*---------------------------------------------------------------* 
000410                                                                  
000420 SPECIAL-NAMES.                                                   
000430     DECIMAL-POINT IS COMMA.                                      
000440                                                                  
000450     EJECT                                                        
000460*---------------------------------------------------------------* 
000470 INPUT-OUTPUT SECTION.                                            
000480*---------------------------------------------------------------* 
000490                                                                  
000500 FILE-CONTROL.                                                    
000510                                                                  
000520     SELECT MVCLLPAT ASSIGN TO UT-S-MVCLLPAT                      
000530                FILE STATUS IS WRK-FS-MVCLLPAT.                   
000540                                                                  
000550     SELECT CADACLLP ASSIGN TO UT-S-CADACLLP                      
000560                FILE STATUS IS WRK-FS-CADACLLP.                   
000570                                                                  
000580     SELECT  CADCLLP ASSIGN TO UT-S-CADCLLP                       
000590                FILE STATUS IS WRK-FS-CADCLLP.                    
000600                                                                  
000610     SELECT   RELATO ASSIGN TO UT-S-RELATO                        
000620                FILE STATUS IS WRK-FS-RELATO.                     
000630                                                                  
000640*===============================================================* 
000650 DATA DIVISION.                                                   
000660*===============================================================* 
000670                                                                  
000680*---------------------------------------------------------------* 
000690 FILE SECTION.                                                    
000700*---------------------------------------------------------------* 
000710                                                                  
000720 FD  MVCLLPAT                                                     
000730     RECORDING MODE IS F                                          
000740     LABEL RECORD IS STANDARD                                     
000750     BLOCK CONTAINS 0 RECORDS.                                    
000760                                                                  
BRQ=E******-INC I#CLLPFB                                                
BRQ=I      COPY 'I#CLLP23'.
000780                                                                  
000790                                                                  
000800 FD  CADACLLP                                                     
000810     RECORDING MODE IS F                                          
000820     LABEL RECORD IS STANDARD                                     
000830     BLOCK CONTAINS 0 RECORDS.                                    
000840                                                                  
BRQ=I      COPY  'I#CLLPRU'.
000860                                                                  
000870     EJECT                                                        
000880                                                                  
000890                                                                  
000900 FD  CADCLLP                                                      
000910     RECORDING MODE IS F                                          
000920     LABEL RECORD IS STANDARD                                     
000930     BLOCK CONTAINS 0 RECORDS.                                    
000940                                                                  
000950 01  CAD-REGISTRO                PIC X(768).
000960                                                                  
000970                                                                  
000980 FD  RELATO                                                       
000990     RECORDING MODE IS F                                          
001000     LABEL RECORD IS STANDARD                                     
001010     BLOCK CONTAINS 0 RECORDS.                                    
001020                                                                  
001030 01  REG-RELATO                  PIC X(080).                      
001040                                                                  
001050     EJECT                                                        
001060                                                                  
001070*---------------------------------------------------------------* 
001080 WORKING-STORAGE SECTION.                                         
001090*---------------------------------------------------------------* 
001100                                                                  
001110 77  FILLER                      PIC X(32)        VALUE           
001120     '* INICIO DA WORKING CLLP7930 *'.                            
001130                                                                  
001140 77  WRK-ABEND                   PIC S9(04) COMP   VALUE +1111.   
001150 77  ACU-PAGINA                  PIC  9(03)        VALUE ZEROS.   
001160 77  ACU-LINHA                   PIC  9(03)        VALUE 80.      
001170                                                                  
001180 77  WRK-FIM-MVCLLPAT            PIC  X(01)       VALUE 'N'.      
001190 77  WRK-FIM-CADACLLP            PIC  X(01)       VALUE 'N'.      
001200                                                                  
001210*---------------------------------------------------------------* 
001220*                   AREAS DE FILE-STATUS                        * 
001230*---------------------------------------------------------------* 
001240                                                                  
001250 77  WRK-FS-MVCLLPAT             PIC  X(02)       VALUE SPACES.   
001260 77  WRK-FS-CADACLLP             PIC  X(02)       VALUE SPACES.   
001270 77  WRK-FS-CADCLLP              PIC  X(02)       VALUE SPACES.   
001280 77  WRK-FS-RELATO               PIC  X(02)       VALUE SPACES.   
001290                                                                  
001300*---------------------------------------------------------------* 
001310*               AREAS PARA MENSAGEM DE ERRO                     * 
001320*---------------------------------------------------------------* 
001330                                                                  
001340 77  WRK-GRAV                PIC  X(15)   VALUE 'NA  GRAVACAO'.   
001350 77  WRK-LEIT                PIC  X(15)   VALUE 'NA  LEITURA'.    
001360 77  WRK-ABRE                PIC  X(15)   VALUE 'NA ABERTURA'.    
001370 77  WRK-FECHA               PIC  X(15)   VALUE 'NO FECHAMENTO'.  
001380 77  WRK-MVCLLPAT            PIC  X(08)   VALUE 'MVCLLPAT'.       
001390 77  WRK-CADACLLP            PIC  X(08)   VALUE 'CADACLLP'.       
001400 77  WRK-CADCLLP             PIC  X(08)   VALUE 'CADCLLP'.        
001410 77  WRK-RELATO              PIC  X(08)   VALUE ' RELATO'.        
001420                                                                  
001430 01  WRK-MENSAGEM.                                                
001440     05  LINHA1                  PIC  X(60)        VALUE          
001450         '*********** C L L P 7 9 3 0 ************'.              
001460     05  LINHA2.                                                  
001470         10  FILLER              PIC  X(10)        VALUE          
001480         '*  ERRO  '.                                             
001490         10  WRK-ERRO            PIC  X(15)        VALUE SPACES.  
001500         10  FILLER              PIC  X(15)        VALUE          
001510             ' DO  ARQUIVO  *'.                                   
001520     05  LINHA3.                                                  
001530         10  FILLER              PIC  X(16)        VALUE          
001540             '*               '.                                  
001550         10  WRK-ARQ             PIC  X(08)        VALUE SPACES.  
001560         10  FILLER              PIC  X(16)        VALUE          
001570             '               *'.                                  
001580     05  LINHA4.                                                  
001590         10  FILLER              PIC  X(17)        VALUE          
001600             '*  FILE-STATUS = '.                                 
001610         10  WRK-FILE-STATUS     PIC  X(02)        VALUE SPACES.  
001620         10  FILLER              PIC  X(21)        VALUE          
001630             '                    *'.                             
001640                                                                  
001650*---------------------------------------------------------------* 
001660*                      AREAS AUXILIARES                         * 
001670*---------------------------------------------------------------* 
001680                                                                  
001690 01  WRK-ADVOG                   PIC 9(11) VALUE ZEROS.           
001700 01  FILLER REDEFINES WRK-ADVOG.                                  
001710     05  WRK-CPFNUM              PIC 9(09).                       
001720     05  WRK-CPFCTR              PIC 9(02).                       
001730                                                                  
001740 01  WRK-TAMANHO                 PIC 9(02) VALUE 15.              
001750                                                                  
001760 01  WRK-CONTROLE                PIC X(03) VALUE SPACES.          
001770                                                                  
001780 01  WRK-MOV-CHAVE.                                               
001790     05  WRK-MOV-AGENCIA         PIC 9(05) VALUE ZEROS.           
001800     05  WRK-MOV-CONTA           PIC 9(07) VALUE ZEROS.           
001810     05  WRK-MOV-CONTRATO        PIC 9(07) VALUE ZEROS.           
001820     05  WRK-MOV-VENCIMENTO      PIC 9(08) VALUE ZEROS.           
001830                                                                  
001840 01  WRK-CAD-CHAVE.                                               
001850     05  WRK-CAD-AGENCIA         PIC 9(05) VALUE ZEROS.           
001860     05  WRK-CAD-CONTA           PIC 9(07) VALUE ZEROS.           
001870     05  WRK-CAD-CONTRATO        PIC 9(07) VALUE ZEROS.           
001880     05  WRK-CAD-VENCIMENTO      PIC 9(08) VALUE ZEROS.           
001890                                                                  
001900*---------------------------------------------------------------* 
001910*          AREA PARA USO DA POOL7600 - DATA E HORA              * 
001920*---------------------------------------------------------------* 
001930                                                                  
001940 01  DATA-HORA.                                                   
001950     05  FILLER                  PIC X(07) VALUE SPACES.          
001960     05  DT-AAAAMMDD             PIC 9(09) COMP-3 VALUE ZEROS.    
001970     05  TI-HHMMSS               PIC 9(07) COMP-3 VALUE ZEROS.    
001980     05  FILLER                  PIC X(27) VALUE SPACES.          
001990                                                                  
002000 01  WRK-DATA                    PIC 9(08) VALUE ZEROS.           
002010 01  FILLER REDEFINES WRK-DATA.                                   
002020     05  WRK-ANO                 PIC 9(04).                       
002030     05  WRK-MES                 PIC 9(02).                       
002040     05  WRK-DIA                 PIC 9(02).                       
002050                                                                  
002060 01  WRK-DT                      PIC 9(08) VALUE ZEROS.           
002070 01  FILLER REDEFINES WRK-DT.                                     
002080     05  WRK-ANO-DT              PIC 9(04).                       
002090     05  WRK-MES-DT              PIC 9(02).                       
002100     05  WRK-DIA-DT              PIC 9(02).                       
002110                                                                  
002120 01  WRK-HORA                    PIC 9(07) VALUE ZEROS.           
002130 01  FILLER REDEFINES WRK-HORA.                                   
002140     05  WRK-ZERO                PIC 9(01).                       
002150     05  WRK-HH                  PIC 9(02).                       
002160     05  WRK-MM                  PIC 9(02).                       
002170     05  WRK-SS                  PIC 9(02).                       
002180                                                                  
002190*---------------------------------------------------------------* 
002200*                         CABECALHOS                            * 
002210*---------------------------------------------------------------* 
002220                                                                  
002230 01  CABEC1.                                                      
002240     03  CB1-CARRO     PIC X(01)  VALUE '1'.                      
002250     03  CB1-DIA       PIC 99     VALUE ZEROS.                    
002260     03  FILLER        PIC X(01)  VALUE '/'.                      
002270     03  CB1-MES       PIC 99     VALUE ZEROS.                    
002280     03  FILLER        PIC X(01)  VALUE '/'.                      
002290     03  CB1-ANO       PIC 9999   VALUE ZEROS.                    
002300     03  FILLER        PIC X(21)  VALUE SPACES.                   
002310     03  FILLER        PIC X(40)  VALUE 'BANCO BRADESCO S.A.'.    
002320     03  CB1-HH        PIC 99     VALUE ZEROS.                    
002330     03  FILLER        PIC X(01)  VALUE ':'.                      
002340     03  CB1-MM        PIC 99     VALUE ZEROS.                    
002350     03  FILLER        PIC X(01)  VALUE ':'.                      
002360     03  CB1-SS        PIC 99     VALUE ZEROS.                    
002370                                                                  
002380 01  CABEC2.                                                      
002390     03  CB2-CARRO     PIC X(01)  VALUE ' '.                      
002400     03  FILLER        PIC X(27)  VALUE 'CLLP7930'.               
002410     03  FILLER        PIC X(21)  VALUE 'RELATORIO DE INCONSIS'.  
002420     03  FILLER        PIC X(23)  VALUE 'TENCIA'.                 
002430     03  FILLER        PIC X(05)  VALUE 'PAG.'.                   
002440     03  CB2-PAG       PIC ZZ9    VALUE ZEROS.                    
002450                                                                  
002460 01  CABEC3.                                                      
002470     03  CB3-CARRO     PIC X(01)  VALUE '0'.                      
002480     03  FILLER        PIC X(21)  VALUE 'AGENCIA     CONTA   C'.  
002490     03  FILLER        PIC X(21)  VALUE 'ONTRATO   DT.VENCIMEN'.  
002500     03  FILLER        PIC X(15)  VALUE 'TO   OCORRENCIA'.        
002510                                                                  
002520 01  CABEC4.                                                      
002530     03  CB4-CARRO     PIC X(01)  VALUE ' '.                      
002540     03  FILLER        PIC X(21)  VALUE '-------  --------   -'.  
002550     03  FILLER        PIC X(21)  VALUE '-------   -----------'.  
002560     03  FILLER        PIC X(21)  VALUE '--   ----------------'.  
002570     03  FILLER        PIC X(16)  VALUE '----------------'.       
002580                                                                  
002590*---------------------------------------------------------------* 
002600*                        LINHAS DETALHE                         * 
002610*---------------------------------------------------------------* 
002620                                                                  
002630 01  LIN-DET1.                                                    
002640     03  LD1-CARRO     PIC X(01)  VALUE ' '.                      
002650     03  FILLER        PIC X(01)  VALUE SPACES.                   
002660     03  LD1-AGENC     PIC Z9999  VALUE ZEROS.                    
002670     03  FILLER        PIC X(04)  VALUE SPACES.                   
002680     03  LD1-CONTA     PIC Z999999 VALUE ZEROS.                   
002690     03  FILLER        PIC X(04)  VALUE SPACES.                   
002700     03  LD1-CONTR     PIC Z999999 VALUE ZEROS.                   
002710     03  FILLER        PIC X(04)  VALUE SPACES.                   
002720     03  LD1-DD-VC     PIC 99     VALUE ZEROS.                    
002730     03  FILLER        PIC X(01)  VALUE '/'.                      
002740     03  LD1-MM-VC     PIC 99     VALUE ZEROS.                    
002750     03  FILLER        PIC X(01)  VALUE '/'.                      
002760     03  LD1-AA-VC     PIC 9999   VALUE ZEROS.                    
002770     03  FILLER        PIC X(05)  VALUE SPACES.                   
002780     03  LD1-OCORR     PIC X(32)  VALUE SPACES.                   
002790                                                                  
002800 01  FILLER                      PIC X(32)        VALUE           
002810     '*  FIM DA WORKING CLLP7930 *'.                              
002820     EJECT                                                        
002830                                                                  
002840*===============================================================* 
002850 PROCEDURE DIVISION.                                              
002860*===============================================================* 
002870                                                                  
002880*---------------------------------------------------------------* 
002890 0000-00-CLLP7930 SECTION.                                        
002900*---------------------------------------------------------------* 
002910                                                                  
002920     PERFORM  1000-00-INICIO                                      
002930                                                                  
002940     PERFORM  2000-00-PROCESSA UNTIL WRK-FIM-MVCLLPAT = 'S'  AND  
002950                                     WRK-FIM-CADACLLP = 'S'       
002960                                                                  
002970     PERFORM  3000-00-FINALIZA.                                   
002980                                                                  
002990 0000-99-END.                EXIT.                                
003000*---------------------------------*                               
003010                                                                  
003020*---------------------------------------------------------------* 
003030 1000-00-INICIO SECTION.                                          
003040*---------------------------------------------------------------* 
003050                                                                  
003060     OPEN  INPUT  MVCLLPAT                                        
003070                  CADACLLP                                        
003080          OUTPUT  CADCLLP                                         
003090                  RELATO.                                         
003100                                                                  
003110     MOVE      WRK-ABRE       TO   WRK-ERRO                       
003120     PERFORM   1100-00-TESTAR-FILE-STATUS.                        
003130                                                                  
003140     CALL      'POOL7600'  USING   DATA-HORA                      
003150                                                                  
003160     MOVE      DT-AAAAMMDD    TO   WRK-DATA                       
003170     MOVE      TI-HHMMSS      TO   WRK-HORA.                      
003180                                                                  
003190     MOVE      WRK-DIA        TO   CB1-DIA                        
003200     MOVE      WRK-MES        TO   CB1-MES                        
003210     MOVE      WRK-ANO        TO   CB1-ANO.                       
003220                                                                  
003230     MOVE      WRK-HH         TO   CB1-HH                         
003240     MOVE      WRK-MM         TO   CB1-MM                         
003250     MOVE      WRK-SS         TO   CB1-SS.                        
003260                                                                  
003270     PERFORM   1200-00-LER-MVCLLPAT                               
003280                                                                  
003290     IF  WRK-FIM-MVCLLPAT   EQUAL  'S'                            
003300         IF  WRK-CAD-CHAVE  EQUAL  HIGH-VALUES                    
003310             PERFORM  3000-00-FINALIZA                            
003320         ELSE                                                     
003330             MOVE  HIGH-VALUES  TO  WRK-MOV-CHAVE.                
003340                                                                  
003350                                                                  
003360     PERFORM   1300-00-LER-CADACLLP                               
003370                                                                  
003380     IF  WRK-FIM-CADACLLP   EQUAL  'S'                            
003390         IF  WRK-MOV-CHAVE  EQUAL  HIGH-VALUES                    
003400             PERFORM  3000-00-FINALIZA                            
003410         ELSE                                                     
003420             MOVE  HIGH-VALUES  TO  WRK-CAD-CHAVE.                
003430                                                                  
003440 1000-99-END.                EXIT.                                
003450*---------------------------------*                               
003460                                                                  
003470*---------------------------------------------------------------* 
003480 1100-00-TESTAR-FILE-STATUS  SECTION.                             
003490*---------------------------------------------------------------* 
003500                                                                  
003510     PERFORM 1110-00-TESTAR-FS-MVCLLPAT                           
003520                                                                  
003530     PERFORM 1120-00-TESTAR-FS-CADACLLP                           
003540                                                                  
003550     PERFORM 1130-00-TESTAR-FS-CADCLLP                            
003560                                                                  
003570     PERFORM 1140-00-TESTAR-FS-RELATO.                            
003580                                                                  
003590 1100-99-END.                EXIT.                                
003600*---------------------------------*                               
003610                                                                  
003620*---------------------------------------------------------------* 
003630 1110-00-TESTAR-FS-MVCLLPAT  SECTION.                             
003640*---------------------------------------------------------------* 
003650                                                                  
003660     IF  WRK-FS-MVCLLPAT NOT EQUAL  '00'                          
003670         MOVE    WRK-MVCLLPAT        TO   WRK-ARQ                 
003680         MOVE    WRK-FS-MVCLLPAT     TO   WRK-FILE-STATUS         
003690         PERFORM 9000-00-ERRO-FILE-STATUS.                        
003700                                                                  
003710 1110-99-END.                EXIT.                                
003720*---------------------------------*                               
003730                                                                  
003740*---------------------------------------------------------------* 
003750 1120-00-TESTAR-FS-CADACLLP  SECTION.                             
003760*---------------------------------------------------------------* 
003770                                                                  
003780     IF  WRK-FS-CADACLLP NOT EQUAL  '00'                          
003790         MOVE    WRK-CADACLLP        TO   WRK-ARQ                 
003800         MOVE    WRK-FS-CADACLLP     TO   WRK-FILE-STATUS         
003810         PERFORM 9000-00-ERRO-FILE-STATUS.                        
003820                                                                  
003830 1120-99-END.                EXIT.                                
003840*---------------------------------*                               
003850                                                                  
003860*----------------------------------------------------------------*
003870 1130-00-TESTAR-FS-CADCLLP   SECTION.                             
003880*----------------------------------------------------------------*
003890                                                                  
003900     IF  WRK-FS-CADCLLP  NOT EQUAL  '00'                          
003910         MOVE    WRK-CADCLLP         TO   WRK-ARQ                 
003920         MOVE    WRK-FS-CADCLLP      TO   WRK-FILE-STATUS         
003930         PERFORM 9000-00-ERRO-FILE-STATUS.                        
003940                                                                  
003950 1130-99-END.                EXIT.                                
003960*---------------------------------*                               
003970                                                                  
003980*----------------------------------------------------------------*
003990 1140-00-TESTAR-FS-RELATO    SECTION.                             
004000*----------------------------------------------------------------*
004010                                                                  
004020     IF  WRK-FS-RELATO   NOT EQUAL  '00'                          
004030         MOVE    WRK-RELATO          TO   WRK-ARQ                 
004040         MOVE    WRK-FS-RELATO       TO   WRK-FILE-STATUS         
004050         PERFORM 9000-00-ERRO-FILE-STATUS.                        
004060                                                                  
004070 1140-99-END.                EXIT.                                
004080*---------------------------------*                               
004090                                                                  
004100*----------------------------------------------------------------*
004110 1200-00-LER-MVCLLPAT SECTION.                                    
004120*----------------------------------------------------------------*
004130                                                                  
004140     READ  MVCLLPAT                                               
004150                                                                  
004160     IF     WRK-FS-MVCLLPAT EQUAL '10'                            
004170            MOVE  'S'              TO    WRK-FIM-MVCLLPAT         
004180            MOVE  HIGH-VALUES      TO    WRK-MOV-CHAVE            
004190            GO                     TO    1200-99-END.             
004200                                                                  
004210     MOVE       WRK-LEIT           TO    WRK-ERRO                 
004220     PERFORM    1110-00-TESTAR-FS-MVCLLPAT.                       
004230                                                                  
004240     MOVE       MOV-AGENCIA        TO    WRK-MOV-AGENCIA          
004250     MOVE       MOV-CONTA          TO    WRK-MOV-CONTA            
004260     MOVE       MOV-CONTRATO       TO    WRK-MOV-CONTRATO         
004270     MOVE       MOV-DT-VECTO-INV   TO    WRK-MOV-VENCIMENTO.      
004280                                                                  
004290 1200-99-END.                EXIT.                                
004300*---------------------------------*                               
004310                                                                  
004320*----------------------------------------------------------------*
004330 1300-00-LER-CADACLLP SECTION.                                    
004340*----------------------------------------------------------------*
004350                                                                  
004360     READ  CADACLLP                                               
004370                                                                  
004380     IF     WRK-FS-CADACLLP EQUAL '10'                            
004390            MOVE  'S'          TO  WRK-FIM-CADACLLP               
004400            MOVE  HIGH-VALUES  TO  WRK-CAD-CHAVE                  
004410            GO                 TO  1300-99-END.                   
004420                                                                  
004430     MOVE       WRK-LEIT       TO  WRK-ERRO                       
004440     PERFORM    1120-00-TESTAR-FS-CADACLLP.                       
004450                                                                  
004460     MOVE       CAD-AGEN       TO  WRK-CAD-AGENCIA                
004470     MOVE       CAD-CC         TO  WRK-CAD-CONTA                  
004480     MOVE       CAD-CONTR      TO  WRK-CAD-CONTRATO               
004490     MOVE       CAD-DTPRECL    TO  WRK-CAD-VENCIMENTO.            
004500                                                                  
004510 1300-99-END.                EXIT.                                
004520*---------------------------------*                               
004530                                                                  
004540*----------------------------------------------------------------*
004550 2000-00-PROCESSA SECTION.                                        
004560*----------------------------------------------------------------*
004570                                                                  
004580     IF  WRK-MOV-CHAVE  EQUAL  WRK-CAD-CHAVE                      
004590         PERFORM  2100-00-GRAVA-CADCLLP                           
004600         PERFORM  1200-00-LER-MVCLLPAT                            
004610         PERFORM  1300-00-LER-CADACLLP                            
004620     ELSE                                                         
004630         IF  WRK-MOV-CHAVE  GREATER  WRK-CAD-CHAVE                
004640             MOVE    'CAD'       TO  WRK-CONTROLE                 
004650             PERFORM  2200-00-IMPRIME-RELATO                      
004660             PERFORM  1300-00-LER-CADACLLP                        
004670         ELSE                                                     
004680             MOVE    'MOV'       TO  WRK-CONTROLE                 
004690             PERFORM  2200-00-IMPRIME-RELATO                      
004700             PERFORM  1200-00-LER-MVCLLPAT.                       
004710                                                                  
004720 2000-99-END.                EXIT.                                
004730*---------------------------------*                               
004740                                                                  
004750*----------------------------------------------------------------*
004760 2100-00-GRAVA-CADCLLP SECTION.                                   
004770*----------------------------------------------------------------*
004780                                                                  
004790     MOVE    '7'                TO    MOV-DIGCL.                  
004800     CALL    'POOL0431'      USING    MOV-NUMCL                   
004810                                      MOV-DIGCL                   
004820                                      WRK-TAMANHO.                
004830     MOVE    MOV-EMPRESA        TO    CAD-EMP.                    
004840     MOVE    MOV-DIGCL          TO    CAD-DIG                     
004850     MOVE    MOV-NUMCL          TO    CAD-NUMCL.                  
004860     MOVE    MOV-DT-INCL-CL     TO    CAD-DTECL                   
004870                                      CAD-DTULT                   
004880                                      CAD-DTPRECL.                
004890     MOVE    MOV-VRCON          TO    CAD-VRCON.                  
004900     MOVE    MOV-VREVE          TO    CAD-VREVE.                  
004910     MOVE    MOV-DT-WRITEOFF    TO    CAD-DATA-TRANSLP.           
004920     MOVE    MOV-CPFNUM         TO    WRK-CPFNUM                  
004930     MOVE    MOV-CPFCTR         TO    WRK-CPFCTR                  
004940     MOVE    WRK-ADVOG          TO    CAD-ADVOG                   
004950     MOVE    01                 TO    CAD-CDULT                   
004960     MOVE    MOV-MARCA-PDD      TO    CAD-PRECL                   
004970     MOVE    076                TO    CAD-NATUREZA.               
004980     MOVE    WKCADCL            TO    CAD-REGISTRO                
004990                                                                  
005000     WRITE   CAD-REGISTRO.                                        
005010                                                                  
005020     MOVE    WRK-GRAV           TO    WRK-ERRO                    
005030     PERFORM 1130-00-TESTAR-FS-CADCLLP.                           
005040                                                                  
005050 2100-99-END.                EXIT.                                
005060*---------------------------------*                               
005070                                                                  
005080*----------------------------------------------------------------*
005090 2200-00-IMPRIME-RELATO SECTION.                                  
005100*----------------------------------------------------------------*
005110                                                                  
005120     IF  ACU-LINHA  GREATER       60                              
005130         MOVE       ZEROS         TO      ACU-LINHA               
005140         ADD        5             TO      ACU-LINHA               
005150         ADD        1             TO      ACU-PAGINA              
005160         MOVE       ACU-PAGINA    TO      CB2-PAG                 
005170         MOVE       WRK-GRAV      TO      WRK-ERRO                
005180         WRITE      REG-RELATO    FROM    CABEC1                  
005190         PERFORM    1140-00-TESTAR-FS-RELATO                      
005200         WRITE      REG-RELATO    FROM    CABEC2                  
005210         PERFORM    1140-00-TESTAR-FS-RELATO                      
005220         WRITE      REG-RELATO    FROM    CABEC3                  
005230         PERFORM    1140-00-TESTAR-FS-RELATO                      
005240         WRITE      REG-RELATO    FROM    CABEC4                  
005250         PERFORM    1140-00-TESTAR-FS-RELATO.                     
005260                                                                  
005270     IF  WRK-CONTROLE  EQUAL  'MOV'                               
005280                                                                  
005290         MOVE   WRK-MOV-AGENCIA     TO    LD1-AGENC               
005300         MOVE   WRK-MOV-CONTA       TO    LD1-CONTA               
005310         MOVE   WRK-MOV-CONTRATO    TO    LD1-CONTR               
005320         MOVE   WRK-MOV-VENCIMENTO  TO    WRK-DT                  
005330         MOVE   WRK-DIA-DT          TO    LD1-DD-VC               
005340         MOVE   WRK-MES-DT          TO    LD1-MM-VC               
005350         MOVE   WRK-ANO-DT          TO    LD1-AA-VC               
005360         MOVE   'SEM CORRESPONDENTE NO CADACLLP'                  
005370                                    TO    LD1-OCORR               
005380                                                                  
005390     ELSE                                                         
005400                                                                  
005410         MOVE   WRK-CAD-AGENCIA     TO    LD1-AGENC               
005420         MOVE   WRK-CAD-CONTA       TO    LD1-CONTA               
005430         MOVE   WRK-CAD-CONTRATO    TO    LD1-CONTR               
005440         MOVE   WRK-CAD-VENCIMENTO  TO    WRK-DT                  
005450         MOVE   WRK-DIA-DT          TO    LD1-DD-VC               
005460         MOVE   WRK-MES-DT          TO    LD1-MM-VC               
005470         MOVE   WRK-ANO-DT          TO    LD1-AA-VC               
005480         MOVE   'SEM CORRESPONDENTE NO MVCLLPAT'                  
005490                                    TO    LD1-OCORR.              
005500                                                                  
005510     WRITE      REG-RELATO        FROM    LIN-DET1.               
005520     PERFORM    1140-00-TESTAR-FS-RELATO.                         
005530                                                                  
005540 2200-99-END.                EXIT.                                
005550*---------------------------------*                               
005560                                                                  
005570*---------------------------------------------------------------* 
005580 3000-00-FINALIZA SECTION.                                        
005590*---------------------------------------------------------------* 
005600                                                                  
005610     CLOSE    MVCLLPAT                                            
005620              CADACLLP                                            
005630              CADCLLP                                             
005640              RELATO.                                             
005650                                                                  
005660     MOVE     WRK-FECHA      TO   WRK-ERRO                        
005670     PERFORM  1100-00-TESTAR-FILE-STATUS.                         
005680                                                                  
005690     STOP     RUN.                                                
005700                                                                  
005710 3000-99-END.                EXIT.                                
005720*---------------------------------*                               
005730                                                                  
005740*---------------------------------------------------------------* 
005750 9000-00-ERRO-FILE-STATUS    SECTION.                             
005760*---------------------------------------------------------------* 
005770                                                                  
005780     DISPLAY LINHA1                                               
005790     DISPLAY LINHA2                                               
005800     DISPLAY LINHA3                                               
005810     DISPLAY LINHA4                                               
005820     DISPLAY LINHA1                                               
005830     CALL    'ILBOABN0'  USING  WRK-ABEND.                        
005840                                                                  
005850 9000-99-END.                EXIT.                                
005860*---------------------------------*                               
005860*---------------------------------*                               

000010*===============================================================* 
000020 IDENTIFICATION DIVISION.                                         
000030*===============================================================* 
000040                                                                  
000050 PROGRAM-ID. CLLP7618.                                            
000060 AUTHOR. ARQUIMEDES SCHEUER DE CERQUEIRA                          
000070*===============================================================* 
000080*                   C P M   S I S T E M A S                     * 
000090*---------------------------------------------------------------* 
000100*                                                               * 
000110*      PROGRAMA     : CLLP7618                                  * 
000120*      PROGRAMADOR  : ARQUIMEDES S. DE CERQUEIRA - CPM/FPOLIS   * 
000130*      ANALISTA CPM : VILMAR DA SILVA RESENDE    - CPM/FPOLIS   * 
000140*      ANALISTA     : TOMOKO                     - GRUPO   82   * 
000150*      DATA         : 14/11/2000                                * 
000160*                                                               * 
000170*      OBJETIVO     :                                           * 
000180*        PESQUISA CADPENDE P/ OBTER RESTR. CADPENDE.            * 
000190*                                                               * 
000200*      ARQUIVOS:                                                * 
000210*         DDNAME                           INCLUDE/BOOK         * 
000220*         CADPENDE                           --------           * 
000230*         ARQLPCL                            I#CLLPCJ           * 
000240*         CADSELEC                           I#CLLPCJ           * 
000250*         TOTAIS                             --------           * 
000260*                                                               * 
000270*      MODULOS CHAMADOS:                                        * 
000280*        ILBOABN0 - MODULO DE CANCELAMENTO DE PGM BATCH.        * 
000290*        POOL0480 - CRIA E PESQUISA TABELAS.                    * 
000300*        POOL0760 - OBTER DATA E HORA CORRENTE.                 * 
000310*                                                               * 
000320*      ALTERACAO MAIO/2001                                      * 
000330*      ===================                                        
000340*      OBJETIVO     :                                           * 
000350*        ALTERADO CHAVE SERASA/SPC PARA CGCCPF/NATUREZA         * 
000310*                                                               * 
000320*      ALTERACAO MAIO/2001                                      * 
000330*      ===================                                      * 
      *      AUTOR:MENDONCA  - CPM                                    * 
      *      DATA:20/05/2003                                          * 
      *      OBJETIVO: MELHORIA DE PERFORMANCE                        * 
      *      SUBSTITUIR A POOL0480 POR BALANCE-LINE DOS               * 
      *      ARQUIVOS CADPENDE E ARQLPCL                              * 
      *                                                               * 
000360*===============================================================* 
BRQ141******************************************************************
BRQ141* MAIO/2012 - (BRQ) - TRATAMENTO DE CARTEIRA  ALFANUMERICA        
BRQ141******************************************************************
000370     EJECT                                                        
000380*===============================================================* 
000390 ENVIRONMENT DIVISION.                                            
000400*===============================================================* 
000410                                                                  
000420*---------------------------------------------------------------* 
000430 CONFIGURATION SECTION.                                           
000440*---------------------------------------------------------------* 
000450                                                                  
000460 SPECIAL-NAMES.                                                   
000470     DECIMAL-POINT IS COMMA.                                      
000480                                                                  
000490     EJECT                                                        
000500*---------------------------------------------------------------* 
000510 INPUT-OUTPUT SECTION.                                            
000520*---------------------------------------------------------------* 
000530                                                                  
000540 FILE-CONTROL.                                                    
000550                                                                  
000560     SELECT CADPENDE ASSIGN TO UT-S-CADPENDE                      
000570                FILE STATUS IS WRK-FS-CADPENDE.                   
000580                                                                  
000590     SELECT ARQLPCL  ASSIGN TO UT-S-ARQLPCL                       
000600                FILE STATUS IS WRK-FS-ARQLPCL.                    
000610                                                                  
000620     SELECT CADSELEC ASSIGN TO UT-S-CADSELEC                      
000630                FILE STATUS IS WRK-FS-CADSELEC.                   
000640                                                                  
000650     SELECT RELATO   ASSIGN TO UT-S-RELATO                        
000660                FILE STATUS IS WRK-FS-RELATO.                     
000670                                                                  
000680     SELECT TOTAIS   ASSIGN TO UT-S-TOTAIS                        
000690                FILE STATUS IS WRK-FS-TOTAIS.                     
000700                                                                  
000710     EJECT                                                        
000720*===============================================================* 
000730 DATA DIVISION.                                                   
000740*===============================================================* 
000750                                                                  
000760*---------------------------------------------------------------* 
000770 FILE SECTION.                                                    
000780*---------------------------------------------------------------* 
000790                                                                  
000800*---------------------------------------------------------------* 
000810*    INPUT:     ARQUIVO DE CADASTRO RESTRICOES                  * 
000820*               ORG. SEQUENCIAL   -   LRECL = 008               * 
000830*---------------------------------------------------------------* 
000840                                                                  
000850 FD  CADPENDE                                                     
000860     RECORDING MODE IS F                                          
000870     LABEL RECORD IS STANDARD                                     
000880     BLOCK CONTAINS 0 RECORDS.                                    
000890                                                                  
000900 01  REG-CADPENDE.                                                
000910     03  EMP-PENDE               PIC 9(05) COMP-3.                
BRQ=E******03  CARTEIRA-PENDE          PIC 9(03).                       
BRQ=I      03  CARTEIRA-PENDE          PIC X(03).                       
000930     03  RESTR-PENDE             PIC X(02).                       
000940                                                                  
000950*---------------------------------------------------------------* 
000960*    INPUT:     ARQUIVO DE SELEC. DEV. P/ SERASA/SPC            * 
000970*               ORG. SEQUENCIAL   -   LRECL = 285               * 
000980*---------------------------------------------------------------* 
000990                                                                  
001000 FD  ARQLPCL                                                      
001010     RECORDING MODE IS F                                          
001020     LABEL RECORD IS STANDARD                                     
001030     BLOCK CONTAINS 0 RECORDS.                                    
001040                                                                  
BRQ=E******-INC I#CLLPCJ                                                
-INC I#CLLP26                                                           
001060                                                                  
001070     EJECT                                                        
001080*---------------------------------------------------------------* 
001090*    OUTPUT:    ARQUIVO DE SELEC. DEV. P/ SERASA/SPC            * 
001100*               ORG. SEQUENCIAL   -   LRECL = 285               * 
001110*---------------------------------------------------------------* 
001120                                                                  
001130 FD  CADSELEC                                                     
001140     RECORDING MODE IS F                                          
001150     LABEL RECORD IS STANDARD                                     
001160     BLOCK CONTAINS 0 RECORDS.                                    
001170                                                                  
001180 01  REG-CADSELEC                PIC X(285).                      
001190                                                                  
001200*---------------------------------------------------------------* 
001210*    OUTPUT:    ARQUIVO DE RELATORIO DE CONTROLE                * 
001220*               ORG. SEQUENCIAL   -   LRECL = 081               * 
001230*---------------------------------------------------------------* 
001240                                                                  
001250 FD  TOTAIS                                                       
001260     RECORDING MODE IS F                                          
001270     LABEL RECORD IS STANDARD                                     
001280     BLOCK CONTAINS 0 RECORDS.                                    
001290                                                                  
001300 01  REG-TOTAIS                  PIC X(81).                       
001310                                                                  
001320*---------------------------------------------------------------* 
001330*    OUTPUT:    RELATORIO DESPREZADOS POR RESTRICAO CADPENDE    * 
001340*               ORG. SEQUENCIAL   -   LRECL = 081               * 
001350*---------------------------------------------------------------* 
001360                                                                  
001370 FD  RELATO                                                       
001380     RECORDING MODE IS F                                          
001390     LABEL RECORD IS STANDARD                                     
001400     BLOCK CONTAINS 0 RECORDS.                                    
001410                                                                  
001420 01  REG-RELATO                  PIC X(81).                       
001430                                                                  
001440     EJECT                                                        
001450*---------------------------------------------------------------* 
001460 WORKING-STORAGE SECTION.                                         
001470*---------------------------------------------------------------* 
001480                                                                  
001490*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -* 
001500 77  FILLER PIC X(32) VALUE  '*  INICIO DA WORKING CLLP7618  *'.  
001510*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -* 
001520                                                                  
001530*--- AREA P/ TESTE DE FILE STATUS ------------------------------* 
001540                                                                  
001550 77  WRK-FS-CADPENDE             PIC X(02) VALUE SPACES.          
001560 77  WRK-FS-ARQLPCL              PIC X(02) VALUE SPACES.          
001570 77  WRK-FS-CADSELEC             PIC X(02) VALUE SPACES.          
001580 77  WRK-FS-RELATO               PIC X(02) VALUE SPACES.          
001590 77  WRK-FS-TOTAIS               PIC X(02) VALUE SPACES.          
001600                                                                  
001610 77  WRK-OPERACAO                PIC X(13) VALUE SPACES.          
001620 77  WRK-ABERTURA                PIC X(13) VALUE 'NA ABERTURA'.   
001630 77  WRK-LEITURA                 PIC X(13) VALUE 'NA LEITURA'.    
001640 77  WRK-GRAVACAO                PIC X(13) VALUE 'NA GRAVACAO'.   
001650 77  WRK-FECHAMENTO              PIC X(13) VALUE 'NO FECHAMENTO'. 
001660 77  WRK-ABEND                   PIC S9(04) COMP VALUE +1111.     
001660 77  WRK-IND                     PIC 9(05) VALUE ZEROS.           
001670                                                                  
001680*--- AREA DE ACUMULADOES ---------------------------------------* 
001690                                                                  
001700 01  ACU-ACUMULADORES.                                            
001710     03  ACU-LIDOS               PIC 9(07) VALUE ZEROS.           
001720     03  ACU-DESPREZADOS         PIC 9(07) VALUE ZEROS.           
001730     03  ACU-GRAVADOS            PIC 9(07) VALUE ZEROS.           
001740     03  ACU-LINHA               PIC S9(05) VALUE +90.            
001740     03  ACU-PAGINA              PIC S9(05) VALUE +90.            
001750                                                                  
001760*--- AREA DE AUXILIARES ----------------------------------------* 
001770                                                                  
001780 01  WRK-RETURN-CODE             PIC 9(05) VALUE ZEROS.           
001790 01  WRK-CARTEIRA-PENDE.                                          
BRQ=E******03  WRK-CARTEIRA-PENDE-R    PIC 9(03) VALUE ZEROS.           
BRQ=I      03  WRK-CARTEIRA-PENDE-R    PIC X(03) VALUE SPACES.          
001810                                                                  
001820*--- AREA P/ CHAVE DE ARQUIVOS ---------------------------------* 
001830                                                                  
001840 01  WRK-CHAVE-CADPENDE.                                          
001850     03  WRK-CH-EMPRESA-PENDE    PIC 9(05) VALUE ZEROS.           
BRQ=E******03  WRK-CH-CARTEIRA-PENDE   PIC 9(03) VALUE ZEROS.           
BRQ=I      03  WRK-CH-CARTEIRA-PENDE   PIC X(03) VALUE SPACES.          
001870                                                                  
001840 01  WRK-CHAVE-ARQLPCL.                                           
001850     03  WRK-CH-EMPRESA-LPCL     PIC 9(05) VALUE ZEROS.           
BRQ=E******03  WRK-CH-CARTEIRA-LPCL    PIC 9(03) VALUE ZEROS.           
BRQ=I      03  WRK-CH-CARTEIRA-LPCL    PIC X(03) VALUE SPACES.          
001870                                                                  
001880*--- AREA P/ DATA E HORA ---------------------------------------* 
001890                                                                  
001900 01  WRK-DATA-AUX.                                                
001910     03  WRK-DT-AAAAA            PIC 9(04) VALUE ZEROS.           
001920     03  WRK-DT-MM               PIC 9(02) VALUE ZEROS.           
001930     03  WRK-DT-DD               PIC 9(02) VALUE ZEROS.           
001940 01  WRK-DATA-AUX-R REDEFINES    WRK-DATA-AUX PIC 9(08).          
001950                                                                  
001960 01  WRK-HORA-AUX.                                                
001970     03  WRK-HH-HORA             PIC 9(02) VALUE ZEROS.           
001980     03  WRK-HH-MM               PIC 9(02) VALUE ZEROS.           
001990     03  WRK-HH-SS               PIC 9(02) VALUE ZEROS.           
002000 01  WRK-HORA-AUX-R REDEFINES    WRK-HORA-AUX PIC 9(06).          
002010                                                                  
002020 01  WRK-DT-EDIT.                                                 
002030     03  WRK-DT-DD               PIC 9(02) VALUE ZEROS.           
002040     03  FILLER                  PIC X(01) VALUE '/'.             
002050     03  WRK-DT-MM               PIC 9(02) VALUE ZEROS.           
002060     03  FILLER                  PIC X(01) VALUE '/'.             
002070     03  WRK-DT-AAAAA            PIC 9(04) VALUE ZEROS.           
002080                                                                  
002090 01  WRK-HORA-EDIT.                                               
002100     03  WRK-HH-HORA             PIC 9(02) VALUE ZEROS.           
002110     03  FILLER                  PIC X(01) VALUE ':'.             
002120     03  WRK-HH-MM               PIC 9(02) VALUE ZEROS.           
002130     03  FILLER                  PIC X(01) VALUE ':'.             
002140     03  WRK-HH-SS               PIC 9(02) VALUE ZEROS.           
002150                                                                  
002160 01  WRK-DATA-HORA.                                               
002170     03  WRK-DT-JULIANA          PIC 9(05) COMP-3 VALUE ZEROS.    
002180     03  WRK-DT-AAMMDD           PIC 9(07) COMP-3 VALUE ZEROS.    
002190     03  WRK-DT-AAAAMMDD         PIC 9(09) COMP-3 VALUE ZEROS.    
002200     03  WRK-TI-HHMMSS           PIC 9(07) COMP-3 VALUE ZEROS.    
002210     03  WRK-TI-HHMMSSMMMMMM     PIC 9(13) COMP-3 VALUE ZEROS.    
002220     03  WRK-TIMESTAMP           PIC X(20) VALUE SPACES.          
002230                                                                  
002230                                                                  
002240*--- AREA DE CABECALHO -----------------------------------------* 
002250                                                                  
002260 01  CABEC1.                                                      
002270     03  CB1-CARRO               PIC X(01) VALUE '1'.             
002280     03  FILLER                  PIC X(23) VALUE 'CLLP7618'.      
002290     03  FILLER                  PIC X(22) VALUE                  
002300         'B A N C O   B R A D E'.                                 
002310     03  FILLER                  PIC X(26) VALUE                  
002320         'S C O   S / A.'.                                        
002330     03  FILLER                  PIC X(07) VALUE 'PAG.: '.        
002370     03  CB1-PAGINA              PIC ZZ9   VALUE ZEROS.           
002340                                                                  
002350 01  CABEC2.                                                      
002360     03  CB2-CARRO               PIC X(01) VALUE ' '.             
002370     03  CB2-DATA                PIC X(10) VALUE SPACES.          
002380     03  FILLER                  PIC X(21) VALUE                  
002390         '                    R'.                                 
002400     03  FILLER                  PIC X(41) VALUE                  
002410         'ELATORIO DE CONTROLE'.                                  
002420     03  CB2-HORA                PIC X(08) VALUE SPACES.          
002430                                                                  
002440 01  CABEC3.                                                      
002450     03  CB3-CARRO               PIC X(01) VALUE ' '.             
002460     03  CB3-LINHA-BRANCO        PIC X(80) VALUE SPACES.          
002470                                                                  
002480 01  CABEC4.                                                      
002490     03  CB4-CARRO               PIC X(01) VALUE ' '.             
002500     03  CB4-DATA                PIC X(10) VALUE SPACES.          
002510     03  FILLER                  PIC X(14) VALUE SPACES.          
002520     03  FILLER                  PIC X(34) VALUE                  
002530         'DESPREZADOS POR RESTRICAO CADPENDE'.                    
002540     03  FILLER                  PIC X(14) VALUE SPACES.          
002550     03  CB4-HORA                PIC X(08) VALUE SPACES.          
002560                                                                  
002570 01  CABEC5.                                                      
002580     03  CB5-CARRO               PIC X(01) VALUE '0'.             
002590     03  FILLER                  PIC X(20) VALUE                  
002600                                 'EMPR  CART  AGEN    '.          
002610     03  FILLER                  PIC X(27) VALUE                  
002620                                 'CONTA  CNPJ/CPF AVALISTA'.      
002630     03  FILLER                  PIC X(33) VALUE                  
002640                                 'NOME DO AVALISTA'.              
002650                                                                  
002660*--- AREA DE LINTOT --------------------------------------------* 
002670                                                                  
002680 01  LINTOT1.                                                     
002690     03  LT1-CARRO               PIC X(01) VALUE '-'.             
002700     03  FILLER                  PIC X(23) VALUE SPACES.          
002710     03  FILLER                  PIC X(21) VALUE                  
002720         'REGISTROS LIDOS ARQLP'.                                 
002730     03  FILLER                  PIC X(09) VALUE 'CL     = '.     
002740     03  LT1-LIDOS               PIC Z.ZZZ.ZZ9.                   
002750                                                                  
002760 01  LINTOT2.                                                     
002770     03  LT2-CARRO               PIC X(01) VALUE '0'.             
002780     03  FILLER                  PIC X(23) VALUE SPACES.          
002790     03  FILLER                  PIC X(28) VALUE                  
002800         'REGISTROS DESPREZADOS'.                                 
002810     03  FILLER                  PIC X(02) VALUE '= '.            
002820     03  LT2-DESPREZADOS         PIC Z.ZZZ.ZZ9.                   
002830                                                                  
002840 01  LINTOT3.                                                     
002850     03  LT3-CARRO               PIC X(01) VALUE '0'.             
002860     03  FILLER                  PIC X(23) VALUE SPACES.          
002870     03  FILLER                  PIC X(21) VALUE                  
002880         'REGISTROS GRAVADOS CA'.                                 
002890     03  FILLER                  PIC X(09) VALUE 'DSELEC = '.     
002900     03  LT3-GRAVADOS            PIC Z.ZZZ.ZZ9.                   
002910                                                                  
002920                                                                  
002930*--- AREA LINHA DE DETALHES ------------------------------------* 
002940                                                                  
002950 01  LINDET1.                                                     
002960     03  LD1-CARRO               PIC X(01) VALUE ' '.             
002970     03  LD1-EMPR                PIC 9(04).                       
002980     03  FILLER                  PIC X(03) VALUE SPACES.          
BRQ=E******03  LD1-CART                PIC 9(03).                       
BRQ=I      03  LD1-CART                PIC X(03).                       
003000     03  FILLER                  PIC X(02) VALUE SPACES.          
003010     03  LD1-AGEN                PIC 9(04).                       
003020     03  FILLER                  PIC X(02) VALUE SPACES.          
003030     03  LD1-CONTA               PIC ZZZZZZ9.                     
003040     03  FILLER                  PIC X(02) VALUE SPACES.          
003050     03  LD1-NUMCPF              PIC 9(09).                       
003060     03  FILLER                  PIC X(01) VALUE '/'.             
003070     03  LD1-FILCPF              PIC 9(04).                       
003080     03  FILLER                  PIC X(01) VALUE '-'.             
003090     03  LD1-CRTCPF              PIC 9(02).                       
003100     03  FILLER                  PIC X(02) VALUE SPACES.          
003110     03  LD1-NOME                PIC X(30).                       
003120                                                                  
003260*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -* 
003270 01  FILLER PIC X(32) VALUE  '*    FIM DA WORKING CLLP7618   *'.  
003280*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -* 
003290     EJECT                                                        
003300*===============================================================* 
003310 PROCEDURE DIVISION.                                              
003320*===============================================================* 
003330                                                                  
003340*---------------------------------------------------------------* 
003350 0000-ROTINA-PRINCIPAL SECTION.                                   
003360*---------------------------------------------------------------* 
003370                                                                  
003380     PERFORM 1000-INICIALIZAR.                                    
003390                                                                  
003400     PERFORM 2000-VERIFICAR-VAZIO.                                
003430                                                                  
003440     PERFORM 4000-PROCESSAR-CONTR-GERAL                           
003450                  UNTIL WRK-CHAVE-ARQLPCL EQUAL HIGH-VALUES.      
003460                                                                  
003470     PERFORM 5000-FINALIZAR.                                      
003480                                                                  
003490*---------------------------------------------------------------* 
003500 0000-99-FIM. EXIT.                                               
003510*---------------------------------------------------------------* 
003520                                                                  
003530*---------------------------------------------------------------* 
003540 1000-INICIALIZAR SECTION.                                        
003550*---------------------------------------------------------------* 
003560                                                                  
003570     OPEN INPUT  CADPENDE                                         
003580                 ARQLPCL                                          
003590          OUTPUT CADSELEC                                         
003600                 RELATO                                           
003610                 TOTAIS.                                          
003620                                                                  
003630     MOVE WRK-ABERTURA           TO WRK-OPERACAO.                 
003640     PERFORM 1100-TESTAR-FILE-STATUS.                             
003650                                                                  
003660     CALL 'POOL7600'             USING WRK-DATA-HORA.             
003670                                                                  
003680     MOVE WRK-DT-AAAAMMDD        TO WRK-DATA-AUX-R.               
003690     MOVE CORR WRK-DATA-AUX      TO WRK-DT-EDIT.                  
003700     MOVE WRK-DT-EDIT            TO CB2-DATA.                     
003710                                                                  
003720     MOVE WRK-TI-HHMMSS          TO WRK-HORA-AUX-R.               
003730     MOVE CORR WRK-HORA-AUX      TO WRK-HORA-EDIT.                
003740     MOVE WRK-HORA-EDIT          TO CB2-HORA.                     
003750                                                                  
003760*---------------------------------------------------------------* 
003770 1000-99-FIM. EXIT.                                               
003780*---------------------------------------------------------------* 
003790     EJECT                                                        
003800*---------------------------------------------------------------* 
003810 1100-TESTAR-FILE-STATUS SECTION.                                 
003820*---------------------------------------------------------------* 
003830                                                                  
003840     PERFORM 1110-TESTAR-FS-CADPENDE.                             
003850                                                                  
003860     PERFORM 1120-TESTAR-FS-ARQLPCL.                              
003870                                                                  
003880     PERFORM 1130-TESTAR-FS-CADSELEC.                             
003890                                                                  
003900     PERFORM 1140-TESTAR-FS-TOTAIS.                               
003910                                                                  
003920     PERFORM 1150-TESTAR-FS-RELATO.                               
003930                                                                  
003940*---------------------------------------------------------------* 
003950 1100-99-FIM. EXIT.                                               
003960*---------------------------------------------------------------* 
003970                                                                  
003980*---------------------------------------------------------------* 
003990 1110-TESTAR-FS-CADPENDE SECTION.                                 
004000*---------------------------------------------------------------* 
004010                                                                  
004020     IF WRK-FS-CADPENDE      NOT EQUAL '00'                       
004030        DISPLAY '************** CLLP7618 *************'           
004040        DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
004050        DISPLAY '*              CADPENDE             *'           
004060        DISPLAY '*         FILE STATUS =  ' WRK-FS-CADPENDE       
004070                                           '         *'           
004080        DISPLAY '************** CLLP7618 *************'           
004090        CALL 'ILBOABN0'          USING WRK-ABEND.                 
004100                                                                  
004110*---------------------------------------------------------------* 
004120 1110-99-FIM. EXIT.                                               
004130*---------------------------------------------------------------* 
004140                                                                  
004150*---------------------------------------------------------------* 
004160 1120-TESTAR-FS-ARQLPCL SECTION.                                  
004170*---------------------------------------------------------------* 
004180                                                                  
004190     IF WRK-FS-ARQLPCL       NOT EQUAL '00'                       
004200        DISPLAY '************** CLLP7618 *************'           
004210        DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
004220        DISPLAY '*              ARQLPCL              *'           
004230        DISPLAY '*         FILE STATUS =  ' WRK-FS-ARQLPCL        
004240                                           '         *'           
004250        DISPLAY '************** CLLP7618 *************'           
004260        CALL 'ILBOABN0'          USING WRK-ABEND.                 
004270                                                                  
004280*---------------------------------------------------------------* 
004290 1120-99-FIM. EXIT.                                               
004300*---------------------------------------------------------------* 
004310                                                                  
004320*---------------------------------------------------------------* 
004330 1130-TESTAR-FS-CADSELEC SECTION.                                 
004340*---------------------------------------------------------------* 
004350                                                                  
004360     IF WRK-FS-CADSELEC      NOT EQUAL '00'                       
004370        DISPLAY '************** CLLP7618 *************'           
004380        DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
004390        DISPLAY '*              CADSELEC             *'           
004400        DISPLAY '*         FILE STATUS =  ' WRK-FS-CADSELEC       
004410                                           '         *'           
004420        DISPLAY '************** CLLP7618 *************'           
004430        CALL 'ILBOABN0'          USING WRK-ABEND.                 
004440                                                                  
004450*---------------------------------------------------------------* 
004460 1130-99-FIM. EXIT.                                               
004470*---------------------------------------------------------------* 
004480     EJECT                                                        
004490*---------------------------------------------------------------* 
004500 1140-TESTAR-FS-TOTAIS SECTION.                                   
004510*---------------------------------------------------------------* 
004520                                                                  
004530     IF WRK-FS-TOTAIS        NOT EQUAL '00'                       
004540        DISPLAY '************** CLLP7618 *************'           
004550        DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
004560        DISPLAY '*              TOTAIS               *'           
004570        DISPLAY '*         FILE STATUS =  ' WRK-FS-TOTAIS         
004580                                           '         *'           
004590        DISPLAY '************** CLLP7618 *************'           
004600        CALL 'ILBOABN0'          USING WRK-ABEND.                 
004610                                                                  
004620*---------------------------------------------------------------* 
004630 1140-99-FIM. EXIT.                                               
004640*---------------------------------------------------------------* 
004650     EJECT                                                        
004660*---------------------------------------------------------------* 
004670 1150-TESTAR-FS-RELATO SECTION.                                   
004680*---------------------------------------------------------------* 
004690                                                                  
004700     IF WRK-FS-RELATO        NOT EQUAL '00'                       
004710        DISPLAY '************** CLLP7618 *************'           
004720        DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
004730        DISPLAY '*              RELATO               *'           
004740        DISPLAY '*         FILE STATUS =  ' WRK-FS-RELATO         
004750                                           '         *'           
004760        DISPLAY '************** CLLP7618 *************'           
004770        CALL 'ILBOABN0'          USING WRK-ABEND.                 
004780                                                                  
004790*---------------------------------------------------------------* 
004800 1150-99-FIM. EXIT.                                               
004810*---------------------------------------------------------------* 
004820                                                                  
004830*---------------------------------------------------------------* 
004840 2000-VERIFICAR-VAZIO SECTION.                                    
004850*---------------------------------------------------------------* 
004860                                                                  
004870     PERFORM 2100-LER-ARQLPCL.                                    
004880                                                                  
004890     IF WRK-FS-ARQLPCL           EQUAL '10'                       
004900        DISPLAY '********** CLLP7618 **********'                  
004910        DISPLAY '*   ARQUIVO ARQLPCL  VAZIO   *'                  
004920        DISPLAY '*     PROGRAMA ENCERRADO     *'                  
004930        DISPLAY '********** CLLP7618 **********'                  
              PERFORM 5000-FINALIZAR.                                   
004940                                                                  
004950     PERFORM 2200-LER-CADPENDE.                                   
004960                                                                  
004970     IF WRK-FS-CADPENDE          EQUAL '10'                       
004980        DISPLAY '********** CLLP7618 **********'                  
004990        DISPLAY '*   ARQUIVO CADPENDE VAZIO   *'                  
005000        DISPLAY '********** CLLP7618 **********'                  
              MOVE    HIGH-VALUES  TO WRK-CHAVE-CADPENDE.               
005080                                                                  
005090*---------------------------------------------------------------* 
005100 2000-99-FIM. EXIT.                                               
005110*---------------------------------------------------------------* 
005120     EJECT                                                        
005130*---------------------------------------------------------------* 
005140 2100-LER-ARQLPCL  SECTION.                                       
005150*---------------------------------------------------------------* 
005160                                                                  
005170     READ ARQLPCL.                                                
005180                                                                  
005190     IF WRK-FS-ARQLPCL           EQUAL '10'                       
005200        MOVE  HIGH-VALUES        TO WRK-CHAVE-ARQLPCL             
005210        GO  TO 2100-99-FIM.                                       
005220                                                                  
005230     MOVE WRK-LEITURA            TO WRK-OPERACAO.                 
005240     PERFORM 1120-TESTAR-FS-ARQLPCL.                              
005250                                                                  
005260     ADD 1                       TO ACU-LIDOS.                    
005270                                                                  
005280     MOVE EMPRESA-WOR            TO WRK-CH-EMPRESA-LPCL           
005290     MOVE CARTEIRA-WOR           TO WRK-CH-CARTEIRA-LPCL.         
005300                                                                  
005310*---------------------------------------------------------------* 
005320 2100-99-FIM. EXIT.                                               
005330*---------------------------------------------------------------* 
005340                                                                  
005350*---------------------------------------------------------------* 
005360 2200-LER-CADPENDE SECTION.                                       
005370*---------------------------------------------------------------* 
005380                                                                  
005390     READ CADPENDE.                                               
005400                                                                  
005410     IF WRK-FS-CADPENDE          EQUAL '10'                       
              MOVE  HIGH-VALUES        TO WRK-CHAVE-CADPENDE            
005420        GO  TO 2200-99-FIM.                                       
005430                                                                  
005440     MOVE WRK-LEITURA            TO WRK-OPERACAO.                 
005450     PERFORM 1110-TESTAR-FS-CADPENDE.                             
005460                                                                  
005470     MOVE EMP-PENDE              TO WRK-CH-EMPRESA-PENDE          
005480     MOVE CARTEIRA-PENDE         TO WRK-CH-CARTEIRA-PENDE.        
005510                                                                  
005520*---------------------------------------------------------------* 
005530 2200-99-FIM. EXIT.                                               
005540*---------------------------------------------------------------* 
005550                                                                  
006260*---------------------------------------------------------------* 
006270 4000-PROCESSAR-CONTR-GERAL  SECTION.                             
006280*---------------------------------------------------------------* 
006290                                                                  
006300     IF         WRK-CHAVE-ARQLPCL   EQUAL  WRK-CHAVE-CADPENDE     
006360        PERFORM 4200-GRAVAR-ARQLPCL                               
              PERFORM 2100-LER-ARQLPCL                                  
006310     ELSE                                                         
006300     IF         WRK-CHAVE-ARQLPCL   LESS   WRK-CHAVE-CADPENDE     
006330        ADD 1                    TO ACU-DESPREZADOS               
006340        PERFORM 4300-IMPRIME-RELATO                               
              PERFORM 2100-LER-ARQLPCL                                  
006350     ELSE                                                         
              PERFORM 2200-LER-CADPENDE.                                
006390                                                                  
006400*---------------------------------------------------------------* 
006410 4000-99-FIM.  EXIT.                                              
006420*---------------------------------------------------------------* 
006430                                                                  
006710*---------------------------------------------------------------* 
006720 4200-GRAVAR-ARQLPCL SECTION.                                     
006730*---------------------------------------------------------------* 
006740                                                                  
006750      MOVE RESTR-PENDE           TO NATUREZA-WOR.                 
006760      MOVE REG-WOR               TO REG-CADSELEC.                 
006770                                                                  
006780      WRITE REG-CADSELEC.                                         
006790      MOVE WRK-GRAVACAO          TO WRK-OPERACAO.                 
006800      PERFORM 1130-TESTAR-FS-CADSELEC.                            
006810                                                                  
006820      ADD 1                      TO ACU-GRAVADOS.                 
006830                                                                  
006840*---------------------------------------------------------------* 
006850 4200-99-FIM.  EXIT.                                              
006860*---------------------------------------------------------------* 
006870     EJECT                                                        
006880*---------------------------------------------------------------* 
006890 4300-IMPRIME-RELATO SECTION.                                     
006900*---------------------------------------------------------------* 
006910                                                                  
006920      IF   ACU-LINHA   GREATER   64                               
006930           MOVE   5    TO        ACU-LINHA                        
006930           ADD    1    TO        ACU-PAGINA                       
006930           MOVE   ACU-PAGINA TO  CB1-PAGINA                       
006940           WRITE REG-RELATO            FROM CABEC1                
006950           PERFORM 1150-TESTAR-FS-RELATO                          
006960           WRITE REG-RELATO            FROM CABEC4                
006970           PERFORM 1150-TESTAR-FS-RELATO                          
006980           WRITE REG-RELATO            FROM CABEC5                
006990           PERFORM 1150-TESTAR-FS-RELATO                          
007000           WRITE REG-RELATO            FROM CABEC3                
007010           PERFORM 1150-TESTAR-FS-RELATO.                         
007020                                                                  
007030      MOVE  EMPRESA-WOR      TO  LD1-EMPR                         
007040      MOVE  CARTEIRA-WOR     TO  LD1-CART                         
007050      MOVE  AGENCIA-WOR      TO  LD1-AGEN                         
007060      MOVE  CONTA-WOR        TO  LD1-CONTA                        
007070      MOVE  CAD-NCGC1-WOR    TO  LD1-NUMCPF                       
007080      MOVE  CAD-FIL1-WOR     TO  LD1-FILCPF                       
007090      MOVE  CAD-CTR1-WOR     TO  LD1-CRTCPF                       
007100      MOVE  CAD-NOME1-WOR    TO  LD1-NOME.                        
007110                                                                  
007120      WRITE REG-RELATO    FROM   LINDET1.                         
007130      PERFORM 1150-TESTAR-FS-RELATO.                              
007140                                                                  
007150      ADD 1                      TO ACU-LINHA.                    
007160                                                                  
007170*---------------------------------------------------------------* 
007180 4300-99-FIM.  EXIT.                                              
007190*---------------------------------------------------------------* 
007200                                                                  
007210*---------------------------------------------------------------* 
007220 5000-FINALIZAR SECTION.                                          
007230*---------------------------------------------------------------* 
007240                                                                  
007250     PERFORM 5100-IMPRIMIR-TOTAIS.                                
007260                                                                  
007270     CLOSE CADPENDE                                               
007280           ARQLPCL                                                
007290           CADSELEC                                               
007300           TOTAIS.                                                
007310                                                                  
007320     MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.                 
007330     PERFORM 1100-TESTAR-FILE-STATUS.                             
007340                                                                  
007350     MOVE ZEROS                  TO RETURN-CODE.                  
007360                                                                  
007370     GOBACK.                                                      
007380                                                                  
007390*---------------------------------------------------------------* 
007400 5000-99-FIM. EXIT.                                               
007410*---------------------------------------------------------------* 
007420     EJECT                                                        
007430*---------------------------------------------------------------* 
007440 5100-IMPRIMIR-TOTAIS SECTION.                                    
007450*---------------------------------------------------------------* 
007460                                                                  
007470     MOVE WRK-GRAVACAO           TO WRK-OPERACAO.                 
007480                                                                  
007490     PERFORM 5110-IMPRIMIR-CABEC.                                 
007500                                                                  
007510     MOVE ACU-LIDOS              TO LT1-LIDOS.                    
007520     WRITE REG-TOTAIS            FROM LINTOT1.                    
007530     PERFORM 1140-TESTAR-FS-TOTAIS.                               
007540                                                                  
007550     MOVE ACU-DESPREZADOS        TO LT2-DESPREZADOS.              
007560     WRITE REG-TOTAIS            FROM LINTOT2.                    
007570     PERFORM 1140-TESTAR-FS-TOTAIS.                               
007580                                                                  
007590     MOVE ACU-GRAVADOS           TO LT3-GRAVADOS.                 
007600     WRITE REG-TOTAIS            FROM LINTOT3.                    
007610     PERFORM 1140-TESTAR-FS-TOTAIS.                               
007620                                                                  
007630*---------------------------------------------------------------* 
007640 5100-99-FIM. EXIT.                                               
007650*---------------------------------------------------------------* 
007660                                                                  
007670*---------------------------------------------------------------* 
007680 5110-IMPRIMIR-CABEC SECTION.                                     
007690*---------------------------------------------------------------* 
007700                                                                  
007710     WRITE REG-TOTAIS            FROM CABEC1.                     
007720     PERFORM 1140-TESTAR-FS-TOTAIS.                               
007730                                                                  
007740     WRITE REG-TOTAIS            FROM CABEC2.                     
007750     PERFORM 1140-TESTAR-FS-TOTAIS.                               
007760                                                                  
007770     WRITE REG-TOTAIS            FROM CABEC3.                     
007780     PERFORM 1140-TESTAR-FS-TOTAIS.                               
007790                                                                  
007800*---------------------------------------------------------------* 
007810 5110-99-FIM. EXIT.                                               
007820*---------------------------------------------------------------* 

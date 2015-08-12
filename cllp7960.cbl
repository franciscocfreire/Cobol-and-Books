000010*===============================================================* 
000020 IDENTIFICATION DIVISION.                                         
000030*===============================================================* 
000040                                                                  
000050 PROGRAM-ID. CLLP7960.                                            
000060 AUTHOR. ADRIANO ROMERO.                                          
000070*===============================================================* 
000080*                   C P M   S I S T E M A S                     * 
000090*---------------------------------------------------------------* 
000100*                                                               * 
000110*      PROGRAMA     : CLLP7960                                  * 
000120*      PROGRAMADOR  : ADRIANO ROMERO                            * 
000130*      ANALISTA     : HOMERO                                    * 
000140*      DATA         : 27/11/1997                                * 
000150*                                                               * 
000160*      OBJETIVO     :                                           * 
000170*        ATUALIZAR CAMPOS ARQDB2.                               * 
000180*                                                               * 
000190*      ARQUIVOS:                                                * 
000200*         DDNAME                           INCLUDE/BOOK         * 
000210*         ARQDB2                                                * 
000220*         MVCLLPAT                           I#CLLPFB           * 
000230*         ARQDB2OK                                              * 
000240*         RELATO                                                * 
000250*                                                               * 
000260*===============================================================* 
000270*===============================================================* 
000280*                  ALTERACAO                                    * 
000290*---------------------------------------------------------------* 
000300*                                                               * 
000310*      PROGRAMADOR  : EDUARDO A. LOBO - CPM - FLORIANOPOLIS     * 
000320*      ANALISTA     : SERGIO NAVARINI - GRUPO - 97              * 
000330*      DATA         : 07/06/1999                                * 
000340*                                                               * 
000350*      OBJETIVO     : INCLUSAO DO CAMPO AGENCIA PARA RESP       * 
000360*                                                               * 
000370*===============================================================* 
BRQ141***************************************************************** 
BRQ141* MAIO/2012 - (BRQ) - TRATAMENTO DE CARTEIRA  ALFANUMERICA        
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
000380     EJECT                                                        
000390*===============================================================* 
000400 ENVIRONMENT DIVISION.                                            
000410*===============================================================* 
000420                                                                  
000430*---------------------------------------------------------------* 
000440 CONFIGURATION SECTION.                                           
000450*---------------------------------------------------------------* 
000460                                                                  
000470 SPECIAL-NAMES.                                                   
000480     DECIMAL-POINT IS COMMA.                                      
000490                                                                  
000500     EJECT                                                        
000510*---------------------------------------------------------------* 
000520 INPUT-OUTPUT SECTION.                                            
000530*---------------------------------------------------------------* 
000540                                                                  
000550 FILE-CONTROL.                                                    
000560                                                                  
000570     SELECT  ARQDB2 ASSIGN TO UT-S-ARQDB2                         
000580                FILE STATUS IS WRK-FS-ARQDB2.                     
000590                                                                  
000600     SELECT MVCLLPAT ASSIGN TO UT-S-MVCLLPAT                      
000610                FILE STATUS IS WRK-FS-MVCLLPAT.                   
000620                                                                  
000630     SELECT   RELATO ASSIGN TO UT-S-RELATO                        
000640                FILE STATUS IS WRK-FS-RELATO.                     
000650                                                                  
000660     SELECT   ARQDB2OK ASSIGN TO UT-S-ARQDB2OK                    
000670                FILE STATUS IS WRK-FS-ARQDB2OK.                   
000680                                                                  
000690     EJECT                                                        
000700*===============================================================* 
000710 DATA DIVISION.                                                   
000720*===============================================================* 
000730                                                                  
000740*---------------------------------------------------------------* 
000750 FILE SECTION.                                                    
000760*---------------------------------------------------------------* 
000770                                                                  
000780 FD  ARQDB2                                                       
000790     RECORDING MODE IS F                                          
000800     LABEL RECORD IS STANDARD                                     
000810     BLOCK CONTAINS 0 RECORDS.                                    
000820                                                                  
000830 01  REG-ARQDB2                   PIC X(590).                     
000840                                                                  
000850 FD  MVCLLPAT                                                     
000860     RECORDING MODE IS F                                          
000870     LABEL RECORD IS STANDARD                                     
000880     BLOCK CONTAINS 0 RECORDS.                                    
000890                                                                  
000900 01  REG-MVCLLPAT                     PIC X(500).                 
000910                                                                  
000920 FD  ARQDB2OK                                                     
000930     RECORDING MODE IS F                                          
000940     LABEL RECORD IS STANDARD                                     
000950     BLOCK CONTAINS 0 RECORDS.                                    
000960                                                                  
000970 01  DB2-REGISTRO                 PIC X(590).                     
000980                                                                  
000990                                                                  
001000 FD  RELATO                                                       
001010     RECORDING MODE IS F                                          
001020     LABEL RECORD IS STANDARD                                     
001030     BLOCK CONTAINS 0 RECORDS.                                    
001040                                                                  
001050 01  REG-RELATO.                                                  
001060     03  REG-REL                 PIC X(80).                       
001070                                                                  
001080     EJECT                                                        
001090*---------------------------------------------------------------* 
001100 WORKING-STORAGE SECTION.                                         
001110*---------------------------------------------------------------* 
001120                                                                  
001130*---------------------------------------------------------------* 
001140*    ARQUIVO DE TABELA DB2                                      * 
001150*---------------------------------------------------------------* 
001160 01   DB2-REGTO.                                                  
001170      02  DB2-CHAVE.                                              
001180          03  DB2-EMPRESA        PIC  9(05)  COMP-3 VALUE ZEROS.  
001190          03  DB2-AGENCIA        PIC  9(05)  COMP-3 VALUE ZEROS.  
001200          03  DB2-NUM-CL         PIC  9(15)  COMP-3 VALUE ZEROS.  
001210          03  DB2-DIG            PIC  X(01)         VALUE SPACES. 
001220      02  DB2-CON-CORR.                                           
001230          03  DB2-CONTA          PIC  9(07)  COMP-3 VALUE ZEROS.  
001240          03  DB2-DIG-CON        PIC  X(01)         VALUE SPACES. 
001250      02  DB2-CARTEIRA           PIC  X(03)         VALUE SPACES. 
001260      02  DB2-CONTRATO           PIC  9(07)  COMP-3 VALUE ZEROS.  
001270      02  DB2-VCTO               PIC  X(10)         VALUE SPACES. 
001280      02  DB2-ID                 PIC  X(02)         VALUE SPACES. 
001290      02  DB2-DATAS.                                              
001300          03  DB2-ENTRADAS       PIC  X(10)         VALUE SPACES. 
001310          03  DB2-VCTO-MORA      PIC  X(10)         VALUE SPACES. 
001320      02  DB2-MOEDA              PIC  X(02)         VALUE SPACES. 
001330      02  DB2-TIPO-GAR           PIC  X(02)         VALUE SPACES. 
001340      02  DB2-LOCAL              PIC  X(02)         VALUE SPACES. 
001350      02  DB2-TIPO-PEND          PIC  9(05)  COMP-3 VALUE ZEROS.  
001360      02  DB2-SIGLA              PIC  X(04)         VALUE SPACES. 
001370      02  DB2-TELEFONE           PIC  9(07)  COMP-3 VALUE ZEROS.  
001380      02  DB2-CPF-ADVOG.                                          
001390          03  DB2-NUMERO-ADV     PIC  9(09)  COMP-3 VALUE ZEROS.  
001400          03  DB2-CTR            PIC  9(02)         VALUE ZEROS.  
001410      02  DB2-ULT-OCORR.                                          
001420          03  DB2-CODIGO         PIC  9(03)  COMP-3 VALUE ZEROS.  
001430          03  DB2-DATA-OCORR     PIC  X(10)         VALUE SPACES. 
001440          03  DB2-STATUS         PIC  X(02)         VALUE SPACES. 
001450      02  DB2-DATA-VENC-CARENCIA PIC  X(10)         VALUE SPACES. 
001460      02  DB2-VALORES.                                            
001470          03  DB2-DEV-INIC       PIC  9(13)V99 COMP-3 VALUE ZEROS.
001480          03  DB2-PRINCIPAL      PIC  9(13)V99 COMP-3 VALUE ZEROS.
001490          03  DB2-CONTABIL       PIC  9(13)V99 COMP-3 VALUE ZEROS.
001500          03  DB2-LIQUIDO        PIC  9(13)V99 COMP-3 VALUE ZEROS.
001510          03  DB2-COBRANCA       PIC  9(13)V99 COMP-3 VALUE ZEROS.
001520          03  DB2-COBRANCA-INF   PIC  9(13)V99 COMP-3 VALUE ZEROS.
001530          03  DB2-JUROS-MORA     PIC  9(13)V99 COMP-3 VALUE ZEROS.
001540          03  DB2-CORR-MONET     PIC  9(13)V99 COMP-3 VALUE ZEROS.
001550          03  DB2-JUROS-12AA     PIC  9(13)V99 COMP-3 VALUE ZEROS.
001560          03  DB2-TOTTAL-CONTRATO PIC 9(13)V99 COMP-3 VALUE ZEROS.
001570      02  DB2-CARTEIRA-ORIGEM    PIC  X(03)         VALUE SPACES. 
001580      02  DB2-COD-NATUREZA-OPER  PIC  X(03)         VALUE SPACES. 
001590      02  DB2-NOME-DEVEDOR       PIC  X(40)         VALUE SPACES. 
001600      02  DB2-CPF-DEV.                                            
001610          03  DB2-NUMERO-DEV     PIC  9(09)  COMP-3 VALUE ZEROS.  
001620          03  DB2-FILIAL         PIC  9(05)  COMP-3 VALUE ZEROS.  
001630          03  DB2-CTR-DEV        PIC  9(02)         VALUE ZEROS.  
001640      02  DB2-NOME-AVALISTA      PIC  X(40)         VALUE SPACES. 
001650      02  DB2-CPF-AVAL.                                           
001660          03  DB2-NUMERO-AVAL    PIC  9(09)  COMP-3 VALUE ZEROS.  
001670          03  DB2-FILIAL-AVAL    PIC  9(05)  COMP-3 VALUE ZEROS.  
001680          03  DB2-CTR-AVAL       PIC  9(02)         VALUE ZEROS.  
001690      02  DB2-NOME-AVAL2         PIC  X(40)         VALUE SPACES. 
001700      02  DB2-CPF-AVAL2.                                          
001710          03  DB2-NUMERO-AVAL2   PIC  9(09)  COMP-3 VALUE ZEROS.  
001720          03  DB2-FILIAL-AVAL2   PIC  9(05)  COMP-3 VALUE ZEROS.  
001730          03  DB2-CTR-AVAL2      PIC  9(02)         VALUE ZEROS.  
001740      02  DB2-BLOQUEIO-TRANSF    PIC  X(01)         VALUE SPACES. 
001750      02  DB2-DIRETORIA.                                          
001760          03  DB2-EXEC           PIC  9(03)  COMP-3 VALUE ZEROS.  
001770          03  DB2-REG.                                            
001780              05  DB2-REGIONAL   PIC  9(03)  COMP-3 VALUE ZEROS.  
001790              05  DB2-COD-JUNCAO PIC  9(05)  COMP-3 VALUE ZEROS.  
001800      02  DB2-DEP-CAMBIO         PIC  9(05)  COMP-3 VALUE ZEROS.  
001810      02  DB2-COD-EMPR           PIC  9(02)         VALUE ZEROS.  
001820      02  DB2-OCORR.                                              
001830          03  DB2-OCORRENCIAS    OCCURS 50 TIMES.                 
001840              05  DB2-OCCORS     PIC  9(03) COMP-3 VALUE ZEROS.   
001850      02  DB2-OPER               PIC  X(10)        VALUE SPACES.  
001860      02  DB2-DT-PDD-180         PIC  X(10)        VALUE SPACES.  
001870      02  DB2-MARCA-PDD-180      PIC  X(01)        VALUE SPACES.  
001880      02  DB2-RAZAO-PRINCIP      PIC  9(05) COMP-3 VALUE ZEROS.   
001890      02  DB2-RAZAO-RENDAS       PIC  9(05) COMP-3 VALUE ZEROS.   
001900      02  DB2-VR-VENCIDOS        PIC  9(13)V99 COMP-3 VALUE ZEROS.
001910      02  DB2-VR-VINCENDOS       PIC  9(13)V99 COMP-3 VALUE ZEROS.
001920      02  DB2-MARCA-IMPE         PIC  X(01)         VALUE SPACES. 
001930      02  DB2-DATA-RETORNO-CL    PIC  X(10)         VALUE SPACES. 
001940      02  DB2-DATA-PRIM-TRANSF   PIC  X(10)         VALUE SPACES. 
001950      02  DB2-VR-ENCVEN-CONG     PIC 9(13)V99 COMP-3 VALUE ZEROS. 
001960      02  DB2-STATUS-REATIVACAO  PIC  X(01)         VALUE SPACES. 
001970      02  DB2-EMPF-PRODUTO       PIC  9(03) COMP-3 VALUE ZEROS.   
001980      02  DB2-EMPF-FAMILIA       PIC  9(01) COMP-3 VALUE ZEROS.   
001990      02  FILLER                 PIC  X(01)         VALUE SPACES. 
002000      02  DB2-DATA-AJUIZAMENTO   PIC  X(10)         VALUE SPACES. 
002010      02  DB2-IOF-NORMAL       PIC 9(13)V9(02) COMP-3 VALUE ZEROS.
002020      02  DB2-IOF-COMPL        PIC 9(13)V9(02) COMP-3 VALUE ZEROS.
002030      02  DB2-ALIQ-CONT-RECOL  PIC 9(03)V9(06) COMP-3 VALUE ZEROS.
002040      02  DB2-ALIQ-REC-TR-CL   PIC 9(03)V9(06) COMP-3 VALUE ZEROS.
002050      02  DB2-ALIQ-COMPL       PIC 9(03)V9(06) COMP-3 VALUE ZEROS.
002060      02  DB2-IOF-OPCAO          PIC  X(01)         VALUE SPACES. 
002070      02  DB2-DATA-SISTEL        PIC  X(10)         VALUE SPACES. 
002080      02  DB2-IND-SISTEL         PIC  X(01)         VALUE SPACES. 
002090      02  DB2-NOTIF-SISTEL       PIC  X(01)         VALUE SPACES. 
002100      02  DB2-AGRESP             PIC  9(05) COMP-3 VALUE ZEROS.   
002110                                                                  
BRQ=E******-INC I#CLLPFB                                                
-INC I#CLLP23                                                           
002130                                                                  
002140 77  FILLER                      PIC X(32)        VALUE           
002150     '* INICIO DA WORKING CLLP7960 *'.                            
002160                                                                  
002170 77  WRK-ABEND                   PIC S9(04) COMP  VALUE +1111.    
002180                                                                  
002190 77  WRK-FS-ARQDB2               PIC X(02)        VALUE  SPACES.  
002200 77  WRK-FS-MVCLLPAT             PIC X(02)        VALUE  SPACES.  
002210 77  WRK-FS-RELATO               PIC X(02)        VALUE  SPACES.  
002220 77  WRK-FS-ARQDB2OK             PIC X(02)        VALUE  SPACES.  
002230                                                                  
002240 77  WRK-OPERACAO                PIC X(13) VALUE  SPACES.         
002250 77  WRK-ABERTURA                PIC X(13) VALUE  'NA ABERTURA'.  
002260 77  WRK-LEITURA                 PIC X(13) VALUE  'NA LEITURA'.   
002270 77  WRK-GRAVACAO                PIC X(13) VALUE  'NA GRAVACAO'.  
002280 77  WRK-FECHAMENTO              PIC X(13) VALUE  'NO FECHAMENTO'.
002290                                                                  
002300 77  WRK-CONTROLE                PIC X(03) VALUE  SPACES.         
002310 77  WRK-TAMANHO                 PIC 9(02) VALUE  15.             
002320                                                                  
002330 77  ACU-LINHA                   PIC 9(02) VALUE  70.             
002340 77  ACU-PAG                     PIC 9(02) VALUE  ZEROS.          
002350                                                                  
002360 01  DATA-HORA.                                                   
002370     05  DT-JULIANA      PIC 9(05)  COMP-3 VALUE ZEROS.           
002380     05  DT-AAMMDD       PIC 9(07)  COMP-3 VALUE ZEROS.           
002390     05  DT-AAAAMMDD     PIC 9(09)  COMP-3 VALUE ZEROS.           
002400     05  TI-HHMMSS       PIC 9(07)  COMP-3 VALUE ZEROS.           
002410     05  TI-HHMMSSMMMMM  PIC 9(13)  COMP-3 VALUE ZEROS.           
002420     05  TIMESTAMP       PIC X(20)         VALUE SPACES.          
002430                                                                  
002440                                                                  
002450 01  DATA-X.                                                      
002460     03  AA-AUX          PIC 9(04) VALUE ZEROS.                   
002470     03  MM-AUX          PIC 9(02) VALUE ZEROS.                   
002480     03  DD-AUX          PIC 9(02) VALUE ZEROS.                   
002490 01  DATA-AUX  REDEFINES DATA-X  PIC 9(08).                       
002500                                                                  
002510 01  HORA-X.                                                      
002520     03  HOR-AUX          PIC 9(02) VALUE ZEROS.                  
002530     03  MIN-AUX          PIC 9(02) VALUE ZEROS.                  
002540     03  SEG-AUX          PIC 9(02) VALUE ZEROS.                  
002550 01  HORA-AUX  REDEFINES HORA-X  PIC 9(06).                       
002560                                                                  
002570 01  WRK-DATA-EDIT.                                               
002580     03  WRK-DIA-EDIT     PIC 9(02) VALUE ZEROS.                  
002590     03  FILLER           PIC X(01) VALUE SPACES.                 
002600     03  WRK-MES-EDIT     PIC 9(02) VALUE ZEROS.                  
002610     03  FILLER           PIC X(01) VALUE SPACES.                 
002620     03  WRK-ANO-EDIT     PIC 9(04) VALUE ZEROS.                  
002630                                                                  
002640 01  WRK-DB2-NUMERO            PIC 9(15) VALUE ZEROS.             
002650 01  FILLER  REDEFINES  WRK-DB2-NUMERO.                           
002660     03  WRK-DB2-NUMCL         PIC 9(12).                         
002670     03  WRK-DB2-PARCL         PIC 9(03).                         
002680                                                                  
002690 01  WRK-DB2-CHAVE.                                               
002700     03  WRK-DB2-AGENCIA       PIC 9(05) VALUE ZEROS.             
002710     03  WRK-DB2-CONTA         PIC 9(07) VALUE ZEROS.             
BRQ=E *    03  WRK-DB2-CARTEIRA      PIC 9(03).                         
BRQ=I      03  WRK-DB2-CARTEIRA      PIC X(03) VALUE SPACES.            
002730     03  WRK-DB2-CONTRATO      PIC 9(07) VALUE ZEROS.             
002740     03  WRK-DB2-VCTO          PIC 9(08) VALUE ZEROS.             
002750     03  FILLER  REDEFINES  WRK-DB2-VCTO.                         
002760         05  WRK-DB2-VCTO-ANO  PIC 9(04).                         
002770         05  WRK-DB2-VCTO-MES  PIC 9(02).                         
002780         05  WRK-DB2-VCTO-DIA  PIC 9(02).                         
002790                                                                  
002800 01  WRK-MOV-CHAVE.                                               
002810     03  WRK-MOV-AGENCIA       PIC 9(05) VALUE ZEROS.             
002820     03  WRK-MOV-CONTA         PIC 9(07) VALUE ZEROS.             
BRQ=E *    03  WRK-MOV-CARTEIRA      PIC 9(03).                         
BRQ=I      03  WRK-MOV-CARTEIRA      PIC X(03) VALUE SPACES.            
002840     03  WRK-MOV-CONTRATO      PIC 9(07) VALUE ZEROS.             
002850     03  WRK-MOV-VCTO          PIC 9(08) VALUE ZEROS.             
002860     03  FILLER  REDEFINES  WRK-MOV-VCTO.                         
002870         05  WRK-MOV-VCTO-ANO  PIC 9(04).                         
002880         05  WRK-MOV-VCTO-MES  PIC 9(02).                         
002890         05  WRK-MOV-VCTO-DIA  PIC 9(02).                         
002900                                                                  
002910 01  CABEC1.                                                      
002920     03  CB1-CARRO     PIC X(01)  VALUE '1'.                      
002930     03  DD1           PIC 99     VALUE ZEROS.                    
002940     03  FILLER        PIC X(01)  VALUE '/'.                      
002950     03  MM1           PIC 99     VALUE ZEROS.                    
002960     03  FILLER        PIC X(01)  VALUE '/'.                      
002970     03  SEC1          PIC 9999   VALUE ZEROS.                    
002980     03  FILLER        PIC X(21)  VALUE SPACES.                   
002990     03  FILLER        PIC X(40)  VALUE 'BANCO BRADESCO S.A.'.    
003000     03  HOR1          PIC 99     VALUE ZEROS.                    
003010     03  FILLER        PIC X(01)  VALUE ':'.                      
003020     03  MIN1          PIC 99     VALUE ZEROS.                    
003030     03  FILLER        PIC X(01)  VALUE ':'.                      
003040     03  SEG1          PIC 99     VALUE ZEROS.                    
003050                                                                  
003060 01  CABEC2.                                                      
003070     03  CB2-CARRO     PIC X(01)  VALUE ' '.                      
003080     03  FILLER        PIC X(27)  VALUE 'CLLP7960'.               
003090     03  FILLER        PIC X(21)  VALUE 'RELATORIO DE INCONSIS'.  
003100     03  FILLER        PIC X(23)  VALUE 'TENCIA'.                 
003110     03  FILLER        PIC X(05)  VALUE 'PAG.'.                   
003120     03  PAG           PIC ZZ9    VALUE ZEROS.                    
003130                                                                  
003140 01  CABEC3.                                                      
003150     03  CB3-CARRO     PIC X(01)  VALUE '0'.                      
003160     03  FILLER        PIC X(22)  VALUE 'AGENCIA   CONTA  CART '. 
003170     03  FILLER        PIC X(25)  VALUE 'CONTRATO   VENCIMENTO'.  
003180     03  FILLER        PIC X(10)  VALUE 'OCORRENCIA'.             
003190                                                                  
003200 01  CABEC4.                                                      
003210     03  CB4-CARRO     PIC X(01)  VALUE ' '.                      
003220     03  FILLER        PIC X(22)  VALUE '------- -------  ---- '. 
003230     03  FILLER        PIC X(25)  VALUE '--------   ----------'.  
003240     03  FILLER        PIC X(22)  VALUE '----------------------'. 
003250     03  FILLER        PIC X(10)  VALUE '----------'.             
003260                                                                  
003270 01  TOTAL1.                                                      
003280     03  LT1-CARRO     PIC X(01)  VALUE ' '.                      
003290     03  FILLER        PIC X(01)  VALUE SPACES.                   
003300     03  AGENCIA       PIC Z9999  VALUE ZEROS.                    
003310     03  FILLER        PIC X(02)  VALUE SPACES.                   
003320     03  CONTA         PIC ZZZ9999 VALUE ZEROS.                   
003330     03  FILLER        PIC X(03)  VALUE SPACES.                   
BRQ=E *    03  CARTEIRA      PIC 9(03).                                 
BRQ=I      03  CARTEIRA      PIC X(03)  VALUE SPACES.                   
003350     03  FILLER        PIC X(02)  VALUE SPACES.                   
003360     03  CONTRATO      PIC 9(07)  VALUE ZEROS.                    
003370     03  FILLER        PIC X(03)  VALUE SPACES.                   
003380     03  DIA-VCTO      PIC 9(02)/ VALUE ZEROS.                    
003390     03  MES-VCTO      PIC 9(02)/ VALUE ZEROS.                    
003400     03  ANO-VCTO      PIC 9(04)  VALUE ZEROS.                    
003410     03  FILLER        PIC X(04)  VALUE SPACES.                   
003420     03  OCORR         PIC X(32)  VALUE SPACES.                   
003430                                                                  
003440*===============================================================* 
003450 PROCEDURE DIVISION.                                              
003460*===============================================================* 
003470                                                                  
003480*---------------------------------------------------------------* 
003490 0000-INICIAR SECTION.                                            
003500*---------------------------------------------------------------* 
003510                                                                  
003520     OPEN INPUT   MVCLLPAT                                        
003530                  ARQDB2                                          
003540          OUTPUT  RELATO                                          
003550                  ARQDB2OK.                                       
003560                                                                  
003570     MOVE    WRK-ABERTURA            TO   WRK-OPERACAO.           
003580     PERFORM 1000-TESTAR-FILE-STATUS.                             
003590                                                                  
003600     PERFORM 1500-BUSCA-DATA-HORA.                                
003610                                                                  
003620     PERFORM 1700-LER-ARQDB2.                                     
003630                                                                  
003640     PERFORM 1800-LER-MVCLLPAT.                                   
003650                                                                  
003660     PERFORM  1900-PROCESSA UNTIL WRK-MOV-CHAVE  EQUAL HIGH-VALUE 
003670              AND  WRK-DB2-CHAVE  EQUAL HIGH-VALUE.               
003680                                                                  
003690 0000-FINALIZAR.                                                  
003700*---------------*                                                 
003710                                                                  
003720     CLOSE  MVCLLPAT                                              
003730            ARQDB2                                                
003740            RELATO                                                
003750            ARQDB2OK.                                             
003760                                                                  
003770     MOVE    WRK-FECHAMENTO          TO   WRK-OPERACAO.           
003780     PERFORM 1000-TESTAR-FILE-STATUS.                             
003790                                                                  
003800     STOP RUN.                                                    
003810                                                                  
003820*---------------------------------------------------------------* 
003830 0000-99-FIM. EXIT.                                               
003840*---------------------------------------------------------------* 
003850     EJECT                                                        
003860*---------------------------------------------------------------* 
003870 1000-TESTAR-FILE-STATUS SECTION.                                 
003880*---------------------------------------------------------------* 
003890                                                                  
003900     PERFORM 1100-TESTAR-FS-ARQDB2.                               
003910                                                                  
003920     PERFORM 1200-TESTAR-FS-MVCLLPAT.                             
003930                                                                  
003940     PERFORM 1300-TESTAR-FS-RELATO.                               
003950                                                                  
003960     PERFORM 1400-TESTAR-FS-ARQDB2OK.                             
003970                                                                  
003980*---------------------------------------------------------------* 
003990 1000-99-FIM. EXIT.                                               
004000*---------------------------------------------------------------* 
004010     EJECT                                                        
004020*---------------------------------------------------------------* 
004030 1100-TESTAR-FS-ARQDB2 SECTION.                                   
004040*---------------------------------------------------------------* 
004050                                                                  
004060     IF WRK-FS-ARQDB2 NOT EQUAL '00'                              
004070        DISPLAY '************** CLLP7960 **************'          
004080        DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO    *'      
004090        DISPLAY '*              ARQDB2                *'          
004100        DISPLAY '*         FILE STATUS =  ' WRK-FS-ARQDB2         
004110                                           '          *'          
004120        DISPLAY '************** CLLP7960 **************'          
004130        CALL 'ILBOABN0'     USING WRK-ABEND.                      
004140                                                                  
004150*---------------------------------------------------------------* 
004160 1100-99-FIM. EXIT.                                               
004170*---------------------------------------------------------------* 
004180     EJECT                                                        
004190*---------------------------------------------------------------* 
004200 1200-TESTAR-FS-MVCLLPAT SECTION.                                 
004210*---------------------------------------------------------------* 
004220                                                                  
004230     IF WRK-FS-MVCLLPAT NOT EQUAL '00'                            
004240        DISPLAY '************** CLLP7960 **************'          
004250        DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO    *'      
004260        DISPLAY '*              MVCLLPAT              *'          
004270        DISPLAY '*         FILE STATUS =  ' WRK-FS-MVCLLPAT       
004280                                           '          *'          
004290        DISPLAY '************** CLLP7960 **************'          
004300        CALL 'ILBOABN0'     USING WRK-ABEND.                      
004310                                                                  
004320*---------------------------------------------------------------* 
004330 1200-99-FIM. EXIT.                                               
004340*---------------------------------------------------------------* 
004350     EJECT                                                        
004360*---------------------------------------------------------------* 
004370 1300-TESTAR-FS-RELATO SECTION.                                   
004380*---------------------------------------------------------------* 
004390                                                                  
004400     IF WRK-FS-RELATO NOT EQUAL '00'                              
004410        DISPLAY '************** CLLP7960 **************'          
004420        DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO    *'      
004430        DISPLAY '*              RELATO                *'          
004440        DISPLAY '*         FILE STATUS =  ' WRK-FS-RELATO         
004450                                           '          *'          
004460        DISPLAY '************** CLLP7960 **************'          
004470        CALL 'ILBOABN0'     USING WRK-ABEND.                      
004480                                                                  
004490*---------------------------------------------------------------* 
004500 1300-99-FIM. EXIT.                                               
004510*---------------------------------------------------------------* 
004520     EJECT                                                        
004530*---------------------------------------------------------------* 
004540 1400-TESTAR-FS-ARQDB2OK SECTION.                                 
004550*---------------------------------------------------------------* 
004560                                                                  
004570     IF WRK-FS-ARQDB2OK   NOT EQUAL '00'                          
004580        DISPLAY '************** CLLP7960 **************'          
004590        DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO    *'      
004600        DISPLAY '*              ARQDB2OK              *'          
004610        DISPLAY '*         FILE STATUS =  ' WRK-FS-ARQDB2OK       
004620                                           '          *'          
004630        DISPLAY '************** CLLP7960 **************'          
004640        CALL 'ILBOABN0'     USING WRK-ABEND.                      
004650                                                                  
004660*---------------------------------------------------------------* 
004670 1400-99-FIM. EXIT.                                               
004680*---------------------------------------------------------------* 
004690     EJECT                                                        
004700*---------------------------------------------------------------* 
004710 1500-BUSCA-DATA-HORA SECTION.                                    
004720*---------------------------------------------------------------* 
004730                                                                  
004740     CALL  'POOL7600'  USING DATA-HORA.                           
004750                                                                  
004760                                                                  
004770     MOVE  DT-AAAAMMDD           TO      DATA-AUX.                
004780     MOVE  TI-HHMMSS             TO      HORA-AUX.                
004790                                                                  
004800     MOVE  AA-AUX                TO      SEC1.                    
004810     MOVE  MM-AUX                TO      MM1                      
004820     MOVE  DD-AUX                TO      DD1.                     
004830                                                                  
004840     MOVE  HOR-AUX               TO      HOR1                     
004850     MOVE  MIN-AUX               TO      MIN1                     
004860     MOVE  SEG-AUX               TO      SEG1.                    
004870                                                                  
004880*---------------------------------------------------------------* 
004890 1500-99-FIM. EXIT.                                               
004900*---------------------------------------------------------------* 
004910                                                                  
004920*---------------------------------------------------------------* 
004930 1700-LER-ARQDB2  SECTION.                                        
004940*---------------------------------------------------------------* 
004950                                                                  
004960     READ ARQDB2 INTO  DB2-REGTO.                                 
004970                                                                  
004980     IF  WRK-FS-ARQDB2 EQUAL '10'                                 
004990         MOVE  HIGH-VALUE        TO      WRK-DB2-CHAVE            
005000     ELSE                                                         
005010         MOVE WRK-LEITURA        TO      WRK-OPERACAO             
005020         PERFORM 1100-TESTAR-FS-ARQDB2                            
005030         IF DB2-EMPRESA       EQUAL      6500    OR               
005040            DB2-EMPRESA       EQUAL      6699                     
005050            MOVE DB2-NUM-CL      TO      WRK-DB2-NUMERO           
005060            IF WRK-DB2-PARCL  EQUAL      888     OR               
005070               DB2-ID         EQUAL      'MO'                     
005080               PERFORM  2000-GRAVA-ARQDB2                         
005090               GO                TO      1700-LER-ARQDB2          
005100            ELSE                                                  
005110               MOVE DB2-AGENCIA  TO      WRK-DB2-AGENCIA          
005120               MOVE DB2-CONTA    TO      WRK-DB2-CONTA            
005130               MOVE DB2-CARTEIRA TO      WRK-DB2-CARTEIRA         
005140               MOVE DB2-CONTRATO TO      WRK-DB2-CONTRATO         
005150               MOVE DB2-VCTO     TO      WRK-DATA-EDIT            
005160               MOVE WRK-DIA-EDIT TO      WRK-DB2-VCTO-DIA         
005170               MOVE WRK-MES-EDIT TO      WRK-DB2-VCTO-MES         
005180               MOVE WRK-ANO-EDIT TO      WRK-DB2-VCTO-ANO         
005190               IF   DB2-VCTO    EQUAL      '01.01.1999'           
005200                    MOVE '01.01.9999'                             
005210                                 TO      DB2-VCTO                 
005220                    MOVE 9999    TO      WRK-ANO-EDIT             
005230                                         WRK-DB2-VCTO-ANO         
005240               ELSE                                               
005250                    NEXT SENTENCE                                 
005260         ELSE                                                     
005270            PERFORM  2000-GRAVA-ARQDB2                            
005280            GO                   TO      1700-LER-ARQDB2.         
005290                                                                  
005300                                                                  
005310*---------------------------------------------------------------* 
005320 1700-99-FIM. EXIT.                                               
005330*---------------------------------------------------------------* 
005340     EJECT                                                        
005350*---------------------------------------------------------------* 
005360 1800-LER-MVCLLPAT SECTION.                                       
005370*---------------------------------------------------------------* 
005380                                                                  
005390     READ MVCLLPAT  INTO  MOV-REGISTRO.                           
005400                                                                  
005410     IF WRK-FS-MVCLLPAT EQUAL '10'                                
005420        MOVE  HIGH-VALUES        TO      WRK-MOV-CHAVE            
005430        GO                       TO      1800-99-FIM              
005440     ELSE                                                         
005450         MOVE   MOV-AGENCIA      TO      WRK-MOV-AGENCIA          
005460         MOVE   MOV-CONTA        TO      WRK-MOV-CONTA            
005470         MOVE   MOV-CARTEIRA     TO      WRK-MOV-CARTEIRA         
005480         MOVE   MOV-CONTRATO     TO      WRK-MOV-CONTRATO         
005490         MOVE   MOV-DT-VECTO-INV TO      WRK-MOV-VCTO.            
005500                                                                  
005510     MOVE WRK-LEITURA            TO      WRK-OPERACAO.            
005520     PERFORM 1200-TESTAR-FS-MVCLLPAT.                             
005530                                                                  
005540*---------------------------------------------------------------* 
005550 1800-99-FIM. EXIT.                                               
005560*---------------------------------------------------------------* 
005570     EJECT                                                        
005580*---------------------------------------------------------------* 
005590 1900-PROCESSA SECTION.                                           
005600*---------------------------------------------------------------* 
005610                                                                  
005620     IF  WRK-DB2-CHAVE   LESS   WRK-MOV-CHAVE                     
005630         PERFORM  2000-GRAVA-ARQDB2                               
005640         MOVE  'DB2'             TO      WRK-CONTROLE             
005650         PERFORM  2200-IMPRIME-RELATO                             
005660         PERFORM 1700-LER-ARQDB2                                  
005670     ELSE                                                         
005680        IF  WRK-DB2-CHAVE   EQUAL   WRK-MOV-CHAVE                 
005690            PERFORM  2100-ATUALIZA-CAMPO-DB2                      
005700            PERFORM  2000-GRAVA-ARQDB2                            
005710            PERFORM 1700-LER-ARQDB2                               
005720            PERFORM 1800-LER-MVCLLPAT                             
005730        ELSE                                                      
005740            MOVE  'MOV'          TO      WRK-CONTROLE             
005750            PERFORM  2200-IMPRIME-RELATO                          
005760            PERFORM 1800-LER-MVCLLPAT.                            
005770                                                                  
005780*---------------------------------------------------------------* 
005790 1900-99-FIM. EXIT.                                               
005800*---------------------------------------------------------------* 
005810                                                                  
005820*---------------------------------------------------------------* 
005830 2000-GRAVA-ARQDB2  SECTION.                                      
005840*---------------------------------------------------------------* 
005850                                                                  
005860     MOVE  DB2-REGTO             TO      DB2-REGISTRO             
005870                                                                  
005880     WRITE DB2-REGISTRO.                                          
005890                                                                  
005900     MOVE    WRK-GRAVACAO            TO   WRK-OPERACAO.           
005910     PERFORM 1400-TESTAR-FS-ARQDB2OK.                             
005920                                                                  
005930*---------------------------------------------------------------* 
005940 2000-99-FIM. EXIT.                                               
005950*---------------------------------------------------------------* 
005960                                                                  
005970*---------------------------------------------------------------* 
005980 2100-ATUALIZA-CAMPO-DB2 SECTION.                                 
005990*---------------------------------------------------------------* 
006000                                                                  
006010     MOVE   '7'                  TO      MOV-DIGCL.               
006020     CALL   'POOL0431'           USING   MOV-NUMCL                
006030                                         MOV-DIGCL                
006040                                         WRK-TAMANHO.             
006050     MOVE   MOV-NUMCL            TO      DB2-NUM-CL.              
006060     MOVE   MOV-DIGCL            TO      DB2-DIG.                 
006070     MOVE   MOV-VRTIT            TO      DB2-PRINCIPAL            
006080     MOVE   MOV-VRLIQ            TO      DB2-LIQUIDO              
006090     MOVE   MOV-VRCOB            TO      DB2-COBRANCA             
006100                                         DB2-COBRANCA-INF.        
006110                                                                  
006120     MOVE   MOV-VRJRM            TO      DB2-JUROS-MORA           
006130     MOVE   MOV-VRCOR            TO      DB2-CORR-MONET           
006140     MOVE   MOV-VRJ12            TO      DB2-JUROS-12AA.          
006150                                                                  
006160***  MOVE   '01.01.0001'         TO      DB2-DATA-AJUIZAMENTO.    
006170     MOVE   ZEROS                TO      DB2-IOF-NORMAL           
006180                                         DB2-IOF-COMPL            
006190                                         DB2-ALIQ-CONT-RECOL      
006200                                         DB2-ALIQ-REC-TR-CL       
006210                                         DB2-ALIQ-COMPL.          
006220     MOVE   'N'                  TO      DB2-IOF-OPCAO.           
006230                                                                  
006240*---------------------------------------------------------------* 
006250 2100-99-FIM. EXIT.                                               
006260*---------------------------------------------------------------* 
006270                                                                  
006280*---------------------------------------------------------------* 
006290 2200-IMPRIME-RELATO  SECTION.                                    
006300*---------------------------------------------------------------* 
006310     IF  ACU-LINHA  GREATER 65                                    
006320         PERFORM 2300-IMPRIME-CABEC.                              
006330                                                                  
006340     PERFORM  2400-IMPRIME-DETALHE.                               
006350                                                                  
006360*---------------------------------------------------------------* 
006370 2200-99-FIM. EXIT.                                               
006380*---------------------------------------------------------------* 
006390                                                                  
006400*---------------------------------------------------------------* 
006410 2300-IMPRIME-CABEC  SECTION.                                     
006420*---------------------------------------------------------------* 
006430                                                                  
006440     MOVE    WRK-GRAVACAO        TO      WRK-OPERACAO.            
006450     ADD     1                   TO      ACU-PAG.                 
006460     MOVE    ACU-PAG             TO      PAG.                     
006470                                                                  
006480     WRITE REG-RELATO            FROM    CABEC1.                  
006490     PERFORM 1300-TESTAR-FS-RELATO.                               
006500                                                                  
006510     WRITE REG-RELATO            FROM    CABEC2.                  
006520     PERFORM 1300-TESTAR-FS-RELATO.                               
006530                                                                  
006540     WRITE REG-RELATO            FROM    CABEC3.                  
006550     PERFORM 1300-TESTAR-FS-RELATO.                               
006560                                                                  
006570     WRITE REG-RELATO            FROM    CABEC4.                  
006580     PERFORM 1300-TESTAR-FS-RELATO.                               
006590                                                                  
006600     MOVE  ZEROS                 TO      ACU-LINHA.               
006610     ADD   5                     TO      ACU-LINHA.               
006620                                                                  
006630*---------------------------------------------------------------* 
006640 2300-99-FIM. EXIT.                                               
006650*---------------------------------------------------------------* 
006660                                                                  
006670*---------------------------------------------------------------* 
006680 2400-IMPRIME-DETALHE  SECTION.                                   
006690*---------------------------------------------------------------* 
006700                                                                  
006710     IF  WRK-CONTROLE            EQUAL   'DB2'                    
006720         MOVE   WRK-DB2-AGENCIA     TO   AGENCIA                  
006730         MOVE   WRK-DB2-CONTA       TO   CONTA                    
006740         MOVE   WRK-DB2-CARTEIRA    TO   CARTEIRA                 
006750         MOVE   WRK-DB2-CONTRATO    TO   CONTRATO                 
006760         MOVE   WRK-DB2-VCTO-DIA    TO   DIA-VCTO                 
006770         MOVE   WRK-DB2-VCTO-MES    TO   MES-VCTO                 
006780         MOVE   WRK-DB2-VCTO-ANO    TO   ANO-VCTO                 
006790         MOVE  'ARQDB2 S/CORRESPONDENTE MVCLLPAT'                 
006800                                    TO   OCORR                    
006810     ELSE                                                         
006820         MOVE   WRK-MOV-AGENCIA     TO   AGENCIA                  
006830         MOVE   WRK-MOV-CONTA       TO   CONTA                    
006840         MOVE   WRK-MOV-CARTEIRA    TO   CARTEIRA                 
006850         MOVE   WRK-MOV-CONTRATO    TO   CONTRATO                 
006860         MOVE   WRK-MOV-VCTO-DIA    TO   DIA-VCTO                 
006870         MOVE   WRK-MOV-VCTO-MES    TO   MES-VCTO                 
006880         MOVE   WRK-MOV-VCTO-ANO    TO   ANO-VCTO                 
006890         MOVE  'MVCLLPAT S/CORRESPONDENTE ARQDB2'                 
006900                                    TO   OCORR.                   
006910                                                                  
006920     WRITE REG-RELATO            FROM    TOTAL1                   
006930     PERFORM 1300-TESTAR-FS-RELATO.                               
006940                                                                  
006950     ADD   1                     TO      ACU-LINHA.               
006960                                                                  
006970*---------------------------------------------------------------* 
006980 2400-99-FIM. EXIT.                                               
006990*---------------------------------------------------------------* 
006990*---------------------------------------------------------------* 

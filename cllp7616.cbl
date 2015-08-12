000010*---------------------------------------------------------------* 
000020 ID DIVISION.                                                     
000030*---------------------------------------------------------------* 
000040 PROGRAM-ID.      CLLP7616.                                       
000050*---------------------------------------------------------------* 
000060                                                                  
000070************************************************                  
000080*                CPM - SISTEMAS                *                  
000090************************************************                  
000100*                                              *                  
000100*  ALTERACAO EM 22/09/2003: (MIGRACAO BBV)     *                  
000100*      - DESPREZAR BBV/FINANCEIRA (AG = 7881)  *                  
000100*      - DESPREZAR CONTA GRAFICA  (CART = 702) *                  
000100*                                              *                  
000100*  ALTERACAO EM 17/02/2004: (MIGRACAO BCN)     *                  
000100*      - DESPREZAR CIDADETRAN  (CART = 294/088)*                  
000100*      - DESPREZAR SECURITIZADAS ( CART = 278) *                  
000100*      - DESPREZAR GREENVILLE    ( CART = 084) *                  
000100*      - DESPREZAR BIBCN,FACTORING, E          *                  
000100*                  POTENZA DTVM  ( CART = 085) *                  
000100*      - DESPREZAR BANCO ZOGBI                 *                  
000100*                                              *                  
000100*                                              *                  
000110*  PROGRAMADOR  -  EDMILSON       - C.P.M.     *                  
000120*  SUPERVISOR   -  SIDNEI         - C.P.M.     *                  
000130*  ANALISTA     -  LOURIVAL SANTI - CPM/GP.82  *                  
000140*  DATA         -  10/10/1997                  *                  
000150*                                              *                  
000160*  OBJETIVOS    :                              *                  
000170*  ==============                              *                  
000180*                                              *                  
000190*  - SELECIONAR REGISTROS PARA A EMISSAO DE    *                  
000200*    CARTA COBRANCA.                           *                  
000210*                                              *                  
000220************************************************                  
000230*                                              *                  
000240*  ENTRADA.                                    *                  
000250*    PARMCLLP (REGISTRO 'PARM')                *                  
000260*    CADASTRO (CADASTRO 'LPCL')                *                  
000270*    CADPENDE (CADASTRO DE PENDENCIAS).        *                  
000280*    ARQESTAT (CREDITOS DE LIQUIDACAO).        *                  
000290*    ARQDATA  (ARQUIVO DE DATAS).              *                  
000300*                                              *                  
000310*  SAIDA.                                      *                  
000320*    CADSELEC (CADASTRO SELECIONADO)           *                  
000330*    TOTAIS   (RELATORIO DE TOTAIS).           *                  
000340*                                              *                  
000350*    (COPIA DO PGM. CLLP5001)                  *                  
000360************************************************                  
000370*                  ALTERACAO                   *                  
000380************************************************                  
000390*                                              *                  
000400*  PROGRAMADOR  -  KAZUO          - C.P.M.     *                  
000410*  SUPERVISOR   -  CARLOS         - C.P.M.     *                  
000420*  ANALISTA     -  TOMOKO         - CPM/GP.82  *                  
000430*  DATA         -  DEZEMBRO/1997               *                  
000440*                                              *                  
000450*  OBJETIVOS    :                              *                  
000460*  ==============                              *                  
000470*                                              *                  
000480*  - SUBSTITUIR A CONSIST. DO CAMPO MARCA-CAD  *                  
000490*    PELA CONSIST. PELO CAMPO DA TAB.LPCLB037  *                  
000500*                                              *                  
000510************************************************                  
000520*                    ALTERACAO                 *                  
000530************************************************                  
000540*                                              *                  
000550*  PROGRAMADOR  -  DIRCEU         - ORIGIN     *                  
000560*  ANALISTA     -  DIRCEU         - ORIGIN     *                  
000570*  RESPONSAVEL  -  ALVARO         - GP 82      *                  
000580*  DATA         -  AGOSTO/2000                 *                  
000590*                                              *                  
000600*  OBJETIVOS    :                              *                  
000610*  ==============                              *                  
000620*                                              *                  
000630*  - RETIRAR UTILIZACAO DA TABELA LPCLB037     *                  
000640*                                              *                  
000650************************************************                  
000660*                    ALTERACAO                 *                  
000670************************************************                  
000680*                                              *                  
000690*  PROGRAMADOR  -  MENDONCA (MEND01) - CPM     *                  
000700*  ANALISTA     -  MENDONCA          - CPM     *                  
000710*  DATA         -  05/09/2001                  *                  
000720*                                              *                  
000730*                                              *                  
000740*  ALTERACAO EM 12/04/2002 - LIMPAR OS DADOS   *                  
000750*                            DE AVALISTAS      *                  
000760*                                              *                  
000770*                                              *                  
000780*  OBJETIVOS    :                              *                  
000790*  ==============                              *                  
000800*                                              *                  
000810*  MELHORIA DE PERFORMANCE                     *                  
000820*   - PESQUISAR A TABELA INTERNA 'TABELA-PEND' *                  
000830*     COM SEARCH ALL.                          *                  
000840*   - SUBSTITUIR O 'INSPECT' POR PERFORM       *                  
000850*                                              *                  
000860************************************************                  
      *                    ALTERACAO                 *                  
      ************************************************                  
      *                                              *                  
      *  PROGRAMADORA -  MARIELI MUSIAL    - CPM     *                  
      *  ANALISTA CPM -  DALMIR TESTOLIN   - CPM     *                  
      *  ANALISTA     -  FERNANDO CAMILLO  - GP. 90  *                  
      *  DATA         -  02/04/2003                  *                  
      *                                              *                  
      *  OBJETIVOS    :                              *                  
      *  ==============                              *                  
      *                                              *                  
      *  CALCULAR DATA REFERENTE A DATA DE MOVIMENTO *                  
      *  E EFETUAR DESPREZA NA ROTINA CLASSIFICA.    *                  
      *                                              *                  
      ************************************************                  
BRQ141******************************************************************
BRQ141* MAIO/2012 - (BRQ) - TRATAMENTO DE CARTEIRA  ALFANUMERICA        
BRQ141******************************************************************
000870*---------------------------------------------------------------* 
000880 ENVIRONMENT DIVISION.                                            
000890 CONFIGURATION SECTION.                                           
000900 SPECIAL-NAMES.                                                   
000910     DECIMAL-POINT IS COMMA.                                      
000920 INPUT-OUTPUT SECTION.                                            
000930 FILE-CONTROL.                                                    
000940     SELECT PARMCLLP ASSIGN TO UT-S-PARMCLLP                      
000950                  FILE STATUS IS WRK-FS-PARMCLLP.                 
000960     SELECT CADPENDE  ASSIGN TO DA-S-CADPENDE.                    
000970     SELECT ARQDATA   ASSIGN TO DA-S-ARQDATA.                     
000980     SELECT CADSELEC  ASSIGN TO DA-S-CADSELEC.                    
000990     SELECT ARQSORT   ASSIGN TO DA-S-ARQSORT.                     
001000     SELECT TOTAIS    ASSIGN TO DA-S-TOTAIS.                      
001010*---------------------------------------------------------------* 
001020/                                                                 
001030*---------------------------------------------------------------* 
001040 DATA DIVISION.                                                   
001050 FILE SECTION.                                                    
001060*---------------------------------------------------------------* 
001070                                                                  
001080*---------------------------------------------------------------* 
001090*    INPUT:     ORG. SEQUENCIAL   -   LRECL = 250               * 
001100*---------------------------------------------------------------* 
001110                                                                  
001120 FD  PARMCLLP                                                     
001130     RECORDING MODE IS F                                          
001140     LABEL RECORD IS STANDARD                                     
001150     BLOCK CONTAINS 0 RECORDS.                                    
001160                                                                  
001170 01  REG-PARAMETRO.                                               
001180     05  PARM-TAREFA.                                             
001190         10  PARM-CCUSTO-1       PIC X(04).                       
001200         10  PARM-CODIGO-1       PIC 9(04).                       
001210         10  PARM-CCUSTO-2       PIC X(04).                       
001220         10  PARM-CODIGO-2       PIC 9(04).                       
001230     05  PARM-SEQUENCIA          PIC 9(07).                       
001240     05  PARM-NR-DIAS            PIC 9(03).                       
001250     05  PARM-VL-CORTE           PIC 9(15).                       
001260     05  FILLER                  PIC X(209).                      
001270                                                                  
001280*---------------------------------------------------------------* 
001290 FD  CADPENDE                                                     
001300     LABEL RECORDS STANDARD                                       
001310     RECORDING F                                                  
001320     BLOCK 0.                                                     
001330 01  REG-CADPENDE.                                                
001340     05 EMP-PE                   PIC 9(5)  COMP-3.                
001350     05 FILLER                   PIC X(03).                       
001360     05 CARTEIRA-PE              PIC X(03).                       
001370     05 FILLER                   PIC X(53).                       
001380     05 RESTR-PE                 PIC XX.                          
001390     05 FILLER                   PIC X(104).                      
001400                                                                  
001410*---------------------------------------------------------------* 
001420 FD  ARQDATA                                                      
001430     LABEL RECORDS STANDARD                                       
001440     RECORDING F                                                  
001450     BLOCK 0.                                                     
-INC I#CLLPGA                                                           
001470                                                                  
001480*---------------------------------------------------------------* 
001490 FD  CADSELEC                                                     
001500     LABEL RECORDS STANDARD                                       
001510     RECORDING F                                                  
001520     BLOCK 0.                                                     
001530 01  REG-CADSELEC                PIC X(362).
001540                                                                  
001550*---------------------------------------------------------------* 
001560 SD  ARQSORT.                                                     
001570 01  REG-SOR.                                                     
001580     05 CHAVE-SOR.                                                
001590        07  CHAVE-SOR-NAT.                                        
001600         09 NUM-CPF-SOR          PIC 9(09)    COMP-3.             
001610         09 FIL-CPF-SOR          PIC 9(05)    COMP-3.             
001620         09 CTR-CPF-SOR          PIC 9(03)    COMP-3.             
001630         09 NATUREZA-SOR         PIC X(02).                       
001640        07 DAT-VENCTO-SOR        PIC 9(09)    COMP-3.             
001650     05 VAL-RESG-SOR             PIC 9(11)V99 COMP-3.             
001660     05 VAL-ENC-VEN-SOR          PIC 9(11)V99 COMP-3.             
001670     05 VAL-ENC-VIN-SOR          PIC 9(11)V99 COMP-3.             
001680     05 CAMPOS-SOR               PIC X(150).                      
001690     05 NOME-3-AVAL-SOR          PIC X(30).                       
001700     05 CC-OP-SOR                PIC 9(07)    COMP-3.             
BRQ=E******05 CARTEIRA-SOR             PIC 9(03)    COMP-3.             
BRQ=I      05 CARTEIRA-SOR             PIC X(03).                       
001720     05 CONTRATO-SOR             PIC 9(07)    COMP-3.             
001730     05 CGC-CPF-SOR.                                              
001740        07 CGC-NRO-SOR           PIC 9(09)    COMP-3.             
001750        07 CGC-FIL-SOR           PIC 9(05)    COMP-3.             
001760        07 CGC-CTR-SOR           PIC 9(03)    COMP-3.             
001770     05 CHAVE-LP-CL-SOR.                                          
001780        07 COD-JUN-SOR           PIC 9(05)    COMP-3.             
001790        07 AGE-OP-SOR            PIC 9(05)    COMP-3.             
001800        07 NUMERO-CL-SOR         PIC 9(15)    COMP-3.             
001810     05 COD-NATUREZA-SOR         PIC 9(03).                       
BRQ=E******05 FILLER                   PIC X(07).                       
BRQ=I      05 FILLER                   PIC X(06).                       
           05 SOR-LT.
              10 SOR-TAXA-CONTRATO     PIC 9(02)V9(06) COMP-3.
              10 SOR-VR-REMUNERATORIO  PIC S9(13)V99   COMP-3.
              10 SOR-VALOR-MORATORIO   PIC S9(13)V99   COMP-3.
              10 SOR-VALOR-MULTA       PIC S9(13)V99   COMP-3.
              10 SOR-DESP-JUD-CUSTAS   PIC S9(11)V99   COMP-3.
              10 SOR-HONORARIOS        PIC S9(11)V99   COMP-3.
              10 SOR-VL-TOTAL-DIVIDA   PIC S9(15)V99   COMP-3.
              10 SOR-VL-TAXA-TARIFA    PIC S9(15)V99   COMP-3.
              10 FILLER                PIC  X(39).

001840*---------------------------------------------------------------*
001850 FD  TOTAIS
001860     LABEL RECORDS STANDARD
001870     RECORDING F
001880     BLOCK 0.
001890 01  REG-IMP                     PIC X(55).
001900
001910*---------------------------------------------------------------*
001920/
001930*---------------------------------------------------------------* 
001940 WORKING-STORAGE SECTION.                                         
001950*---------------------------------------------------------------* 
001960                                                                  
001970 77  WRK-OPERACAO                PIC X(13) VALUE  SPACES.         
001980 77  CD-PARM-DIA1                PIC 9(02)        VALUE ZEROS.    
001990 77  WRK-VR-BASE                 PIC 9(11)V99 VALUE ZERO COMP-3.  
002000 77  AC-LIDOS-CAD                PIC 9(7)     COMP-3 VALUE 0.     
002010 77  AC-DESPREZADO               PIC 9(7)     COMP-3 VALUE 0.     
002020 77  AC-SELEC                    PIC 9(7)     COMP-3 VALUE 0.     
002030 77  NUM-DIAS                    PIC S9(5)    COMP-3 VALUE 0.     
002040 77  WRK-RETURN-CODE             PIC S9(5)           VALUE +0.    
002050 77  FIM-SORT                    PIC X        VALUE  SPACES.      
002060 77  WRK-FS-PARMCLLP             PIC X(02)    VALUE  SPACES.      
002070 77  CHAVE-ERRO                  PIC X.                           
002080 77  WRK-ACHOU                   PIC X               VALUE ' '.   
002090 77  CHAVE-INCONS                PIC 9               VALUE 0.     
002100 77  CARTEIRA-AUX                PIC X(3) VALUE SPACES.           
002110 77  IND                         PIC 9(5) COMP-3     VALUE 0.     
002120 77  EMP-ANT                     PIC 9(5) COMP-3     VALUE 0.     
002130 77  FIM-CADPENDE                PIC 9               VALUE 0.     
002140 77  WRK-VL-CL-LIQ               PIC 9(15)V99 COMP-3 VALUE 0.     
002150 77  WRK-DESPREZ                 PIC 9(01)           VALUE ZEROS. 
002160 77  WRK-ON                      PIC 9(01)           VALUE 1.     
002170 77  CONTROLE-CALL               PIC 99.                          
002180 77  CADASTRO                    PIC X(8) VALUE 'CADASTRO'.       
002190 77  WRK-PRIM-AVISO              PIC X(1) VALUE SPACES.           
002200 77  WRK-SEG-AVISO               PIC X(1) VALUE SPACES.           
002210                                                                  
002220 01  WRK-NOME.                                                    
002230     05 WRK-NOMECAD              PIC  X(01)  OCCURS 40.           
002240                                                                  
002250 01  WRK-DIAS1                   PIC S9(03)      VALUE  ZEROS.    
002260 01  WRK-DIAS2                   PIC S9(03)      VALUE  ZEROS.    
002270 01  WRK-DIAS3                   PIC S9(03)      VALUE  ZEROS.    
002280 01  WRK-DIAS4                   PIC S9(03)      VALUE  ZEROS.    
002290                                                                  
002300 01  WRK-VL-CORTE1               PIC S9(15)      VALUE  ZEROS.    
002310 01  WRK-VL-CORTE2               PIC S9(15)      VALUE  ZEROS.    
002320 01  WRK-VL-CORTE3               PIC S9(15)      VALUE  ZEROS.    
002330 01  WRK-VL-CORTE4               PIC S9(15)      VALUE  ZEROS.    
002340                                                                  
002350*                                                                 
002360 01  CARTEIRA-NUM                PIC 9(3) VALUE ZEROS.            
002370 01  CARTEIRA-ALFA    REDEFINES  CARTEIRA-NUM PIC X(03).          
002380                                                                  
002390 01  REG-CADASTRO.                                                
002400     03  CHAVE-CAD.                                               
002410      05 EMP-CAD                 PIC  9(05)    COMP-3.            
002420      05 CAD-AGEN                PIC  9(05)    COMP-3.            
002430      05 CAD-NUMCL               PIC  9(15)    COMP-3.            
002440     03  CAD-DIG                 PIC  X(01).                      
002450     03  CAD-CC                  PIC  9(07)    COMP-3.            
002460     03  TIPO-PEND-CAD           PIC  9(05)    COMP-3.            
002470     03  CAD-SIGLA               PIC  X(04).                      
002480     03  CARTEIRA-CAD            PIC  9(03)    COMP-3.            
002490     03  CAD-CONTR               PIC  9(07)    COMP-3.            
002500     03  CAD-CART2               PIC  X(03).                      
002510     03  CAD-RZCL                PIC  9(05)    COMP-3.            
002520     03  CAD-CTCL                PIC  9(07)    COMP-3.            
002530     03  CAD-RZ1CL               PIC  9(05)    COMP-3.            
002540     03  CAD-CT1CL               PIC  9(07)    COMP-3.            
002550     03  DAT-VENCTO-CAD          PIC  9(09)    COMP-3.            
002560     03  CAD-DTECL               PIC  9(09)    COMP-3.            
002570     03  CAD-DTBCL               PIC  9(09)    COMP-3.            
002580     03  CAD-DTIOP               PIC  9(09)    COMP-3.            
002590     03  CAD-DTPRO               PIC  9(09)    COMP-3.            
002600     03  CAD-DTELP               PIC  9(09)    COMP-3.            
002610     03  CAD-DTBLP               PIC  9(09)    COMP-3.            
002620     03  CAD-DTPGTO              PIC  9(09)    COMP-3.            
002630     03  CAD-MOVTO               PIC  9(09)    COMP-3.            
002640     03  CAD-DTREX               PIC  9(09)    COMP-3.            
002650     03  CAD-DTVTR               PIC  9(09)    COMP-3.            
002660     03  CAD-VRDVI               PIC  9(13)V99 COMP-3.            
002670     03  VAL-RESG-CAD            PIC  9(13)V99 COMP-3.            
002680     03  CAD-VRBXA               PIC  9(13)V99 COMP-3.            
002690     03  CAD-VBASE               PIC  9(13)V99 COMP-3.            
002700     03  CAD-VRBBX               PIC  9(13)V99 COMP-3.            
002710     03  CAD-COBRM               PIC  9(13)V99 COMP-3.            
002720     03  CAD-COBRB               PIC  9(13)V99 COMP-3.            
002730     03  CAD-DEBCC               PIC  9(13)V99 COMP-3.            
002740     03  CAD-COBRD               PIC  9(13)V99 COMP-3.            
002750     03  VAL-ENC-VEN-CAD         PIC  9(13)V99 COMP-3.            
002760     03  VAL-ENC-VIN-CAD         PIC  9(13)V99 COMP-3.            
002770     03  CAD-VRINI               PIC  9(13)V99 COMP-3.            
002780     03  CAD-BXINI               PIC  9(13)V99 COMP-3.            
002790     03  CAD-VRIOF               PIC  9(13)V99 COMP-3.            
002800     03  CAD-BXIOF               PIC  9(13)V99 COMP-3.            
002810     03  CAD-ANTEX               PIC  9(13)V99 COMP-3.            
002820     03  CAD-MOEDA               PIC  X(02).                      
002830     03  CAD-MOEDA-ANT           PIC  X.                          
002840     03  CAD-TELEF               PIC  9(07)    COMP-3.            
002850     03  CAMPOS-CAD.                                              
002860      05 NOME-CAD.                                                
002870         07  NOME-CAD-39         PIC  X(39).                      
002880         07  FILLER              PIC  X(01).                      
002890      05 CAD-CGC.                                                 
002900         07  NUM-CGC-CAD         PIC  9(09)    COMP-3.            
002910         07  FIL-CGC-CAD         PIC  9(05)    COMP-3.            
002920         07  CONTROLE-CAD        PIC  9(03)    COMP-3.            
002930      05 CAD-NOME1               PIC  X(40).                      
002940      05 CAD-CGC1.                                                
002950         07  CAD-NCGC1           PIC  9(09)    COMP-3.            
002960         07  CAD-FIL1            PIC  9(05)    COMP-3.            
002970         07  CAD-CTR1            PIC  9(03)    COMP-3.            
002980      05 CAD-NOME2               PIC  X(40).                      
002990      05 CAD-CGC2.                                                
003000         07  CAD-NCGC2           PIC  9(09)    COMP-3.            
003010         07  CAD-FIL2            PIC  9(05)    COMP-3.            
003020         07  CAD-CTR2            PIC  9(03)    COMP-3.            
003030     03  CAD-TPGAR               PIC  X(02).                      
003040     03  CAD-NOGAR               PIC  X(32).                      
003050     03  FILLER                  PIC  X(33).                      
003060     03  CAD-ADVOG               PIC  9(11)    COMP-3.            
003070     03  LOC-PROCESS-CAD         PIC  XX.                         
003080     03  CAD-LOCA2               PIC  X.                          
003090     03  CAD-SUBSTAB             PIC  X.                          
003100     03  CAD-CDULT               PIC  9(03)    COMP-3.            
003110     03  CAD-DTULT               PIC  9(09)    COMP-3.            
003120     03  CAD-OCORRENCIAS.                                         
003130         05  CAD-OCORR           PIC  9(03)    COMP-3 OCCURS 50.  
003140     03  MARCA-CAD               PIC  X.                          
003150     03  CAD-IDCON               PIC  X.                          
003160     03  CAD-CARTA               PIC  X.                          
003170     03  CAD-TXPUN               PIC  X.                          
003180     03  CAD-AJUP                PIC  X.                          
003190     03  CAD-REGBX               PIC  X.                          
           03  CAD-TPTRANLP            PIC  X(02).                      
           03  CAD-NOVOREGI            PIC  X.                          
           03  CAD-TPBXCOBR            PIC  X.                          
           03  CAD-FALCONC             PIC  X.                          
           03  CAD-EMISSAO             PIC  X.                          
           03  CAD-PRELP               PIC  X.                          
           03  CAD-DTPRELP             PIC  9(09)    COMP-3.            
           03  CAD-ORIGEM              PIC  X(02).                      
           03  CAD-TEXPUR              PIC  X.                          
003210     03  CAD-NATUREZA            PIC  9(03)    COMP-3.            
003220     03  CAD-IDENT               PIC  X(02).                      
003230     03  CAD-TIPBX               PIC  X.                          
003240     03  BAIXA-CAD               PIC  9.                          
           03  CAD-LT.
               05 CAD-TAXA-CONTRATO            PIC 9(02)V9(06)  COMP-3.
               05 CAD-VR-REMUNERATORIO         PIC S9(13)V99    COMP-3.
               05 CAD-VALOR-MORATORIO          PIC S9(13)V99    COMP-3.
               05 CAD-VALOR-MULTA              PIC S9(13)V99    COMP-3.
               05 CAD-DESP-JUD-CUSTAS          PIC S9(11)V99    COMP-3.
               05 CAD-HONORARIOS               PIC S9(11)V99    COMP-3.
               05 CAD-VL-TOTAL-DIVIDA          PIC S9(15)V99    COMP-3.
               05 CAD-VL-TAXA-TARIFA           PIC S9(15)V99    COMP-3.
               05 FILLER                       PIC  X(39).

-INC I#CLLPDD
003270                                                                  
003280*---------------------------------------------------------------* 
003290 01  AREAS-AUXILIARES.                                            
003300*---------------------------------------------------------------* 
003310     05  SEPARA-CAMPO.                                            
003320         10  CPO-1               PIC X.                           
003330         10  CPO-2               PIC X.                           
003340*---------------------------------------------------------------* 
003350 05  CPFCGC-CALL.                                                 
003360     10    NUM-CPF-CALL          PIC 9(9).                        
003370     10    FILIAL-CALL           PIC 9(5).                        
003380*---------------------------------------------------------------* 
003390                                                                  
003400     05 DATA-SIST                PIC 9(8).                        
003410     05 DATA-SIST-R REDEFINES DATA-SIST.                          
003420        10 ANO-D                 PIC 9999.                        
003430        10 MES-D                 PIC 99.                          
003440        10 DIA-D                 PIC 99.                          
003450*---------------------------------------------------------------* 
003460     05 DAT-VENCTO-AUX            PIC 9(8)            VALUE 0.    
003470     05 DAT-VENCTO-AUXR  REDEFINES DAT-VENCTO-AUX.                
003480        10 DIA-AUX               PIC 99.                          
003490        10 MES-AUX               PIC 99.                          
003500        10 ANO-AUX               PIC 9999.                        
003510                                                                  
003520     05 DAT-VCTO-INV             PIC 9(8)            VALUE 0.     
003530     05 DAT-VCTO-INV-R  REDEFINES DAT-VCTO-INV.                   
003540        10 ANO-AUX-INV           PIC 9999.                        
003550        10 MES-AUX-INV           PIC 99.                          
003560        10 DIA-AUX-INV           PIC 99.                          
003570                                                                  
003580 05  DATA-VEN-INV.                                                
003590     10 ANO-VEN                  PIC 9999.                        
003600     10 MES-VEN                  PIC 99.                          
003610     10 DIA-VEN                  PIC 99.                          
003620 05  DATA-VEN-INV-R REDEFINES DATA-VEN-INV PIC 9(08).             
003630                                                                  
003640                                                                  
003650 05  DATA-CL-AUX                 PIC 9(08).                       
003660 05  DATA-CL-AUX-R REDEFINES DATA-CL-AUX.                         
003670     10 DIA-CL                   PIC 99.                          
003680     10 MES-CL                   PIC 99.                          
003690     10 ANO-CL                   PIC 9999.                        
003700                                                                  
003710 05  DATA-CL-INV.                                                 
003720     10 ANO-CL-INV               PIC 9(4).                        
003730     10 MES-CL-INV               PIC 99.                          
003740     10 DIA-CL-INV               PIC 99.                          
003750 05  DATA-CL-INV-R REDEFINES DATA-CL-INV PIC 9(08).               
003760                                                                  
003770 05  DATA-LIMITE-AUX             PIC 9(08).                       
003780                                                                  
003790 05  DATA-LIM-INV-1A-CARTA.                                       
003800     10 ANO-LI-INV-1A-CARTA            PIC 9(4).                  
003810     10 MES-LI-INV-1A-CARTA            PIC 99.                    
003820     10 DIA-LI-INV-1A-CARTA            PIC 99.                    
003830 05  DATA-LIM-INV-R-1A-CARTA                                      
003840                  REDEFINES DATA-LIM-INV-1A-CARTA PIC 9(08).      
003850                                                                  
003860 05  DATA-LIMITE-INV-08.                                          
003870     10 ANO-LI-INV-08            PIC 9(4).                        
003880     10 MES-LI-INV-08            PIC 99.                          
003890     10 DIA-LI-INV-08            PIC 99.                          
003900 05  DATA-LIM-INV-R-08 REDEFINES DATA-LIMITE-INV-08 PIC 9(08).    
003910                                                                  
003920 05  DATA-LIM-INV-2A-CARTA.                                       
003930     10 ANO-LI-INV-2A-CARTA            PIC 9(4).                  
003940     10 MES-LI-INV-2A-CARTA            PIC 99.                    
003950     10 DIA-LI-INV-2A-CARTA            PIC 99.                    
003960 05  DATA-LIM-INV-R-2A-CARTA                                      
003970             REDEFINES DATA-LIM-INV-2A-CARTA PIC 9(08).           
003980                                                                  
003990 05  DATA-LIMITE-INV-21.                                          
004000     10 ANO-LI-INV-21            PIC 9(4).                        
004010     10 MES-LI-INV-21            PIC 99.                          
004020     10 DIA-LI-INV-21            PIC 99.                          
004030 05  DATA-LIM-INV-R-21 REDEFINES DATA-LIMITE-INV-21 PIC 9(08).    
004040                                                                  
004050 05  DATA-LIM-INV-3A-CARTA.                                       
004060     10 ANO-LI-INV-3A-CARTA            PIC 9(4).                  
004070     10 MES-LI-INV-3A-CARTA            PIC 99.                    
004080     10 DIA-LI-INV-3A-CARTA            PIC 99.                    
004090 05  DATA-LIM-INV-R-3A-CARTA                                      
004100             REDEFINES DATA-LIM-INV-3A-CARTA PIC 9(08).           
004110                                                                  
004120 05  DATA-LIMITE-INV-41.                                          
004130     10 ANO-LI-INV-41            PIC 9(4).                        
004140     10 MES-LI-INV-41            PIC 99.                          
004150     10 DIA-LI-INV-41            PIC 99.                          
004160 05  DATA-LIM-INV-R-41 REDEFINES DATA-LIMITE-INV-41 PIC 9(08).    
004170                                                                  
       05  DATA-LIMITE-INV-10.                                          
           10 ANO-LI-INV-10            PIC 9(4).                        
           10 MES-LI-INV-10            PIC 99.                          
           10 DIA-LI-INV-10            PIC 99.                          
       05  DATA-LIM-INV-R-10 REDEFINES DATA-LIMITE-INV-10 PIC 9(08).    
                                                                        
004180*---------------------------------------------------------------* 
004190     05 CAB1.                                                     
004200        10 FILLER                PIC X(47) VALUE                  
004210     '1* CLLP7616 * TOTAIS DA SELECAO DO CADASTRO EM '.           
004220        10 DATA-CAB.                                              
004230           15 DIA-D              PIC 99.                          
004240           15 FILLER             PIC X VALUE '/'.                 
004250           15 MES-D              PIC 99.                          
004260           15 FILLER             PIC X VALUE '/'.                 
004270           15 ANO-D              PIC 9(4).                        
004280                                                                  
004290*---------------------------------------------------------------* 
004300     05 DET1.                                                     
004310        10 TEXTO-DET             PIC X(46).                       
004320        10 CAMPO-DET             PIC Z.ZZZ.ZZ9.                   
004330                                                                  
004340*---------------------------------------------------------------* 
004350     05 TABELA-PEND.                                              
004360           15 TAB-PEND  OCCURS 11000 TIMES                        
004370*                 ASCENDING KEY CARTEIRA-TAB                      
004380                  INDEXED BY IND-SEARCH.                          
004390              20 CARTEIRA-TAB    PIC X(03)    VALUE 'ZZZ'.        
004400              20 RESTR-TAB       PIC XX       VALUE SPACES.       
004410                                                                  
004420/---------------------------------------------------------------* 
004430*       AREAS  AUXILIARES                                         
004440*---------------------------------------------------------------* 
004450                                                                  
004460 01  WRK-AREA-POOL7600.                                           
004470     03  WRK-DT-JULIANA          PIC  9(05) COMP-3.               
004480     03  WRK-DT-AAMMDD           PIC  9(07) COMP-3.               
004490     03  WRK-DT-AAAAMMDD         PIC  9(09) COMP-3.               
004500     03  WRK-TI-HHMMSS           PIC  9(07) COMP-3.               
004510     03  WRK-TI-HHMMSSMMMMMM     PIC  9(13) COMP-3.               
004520     03  WRK-TIMESTAMP           PIC  X(20).                      
004530                                                                  
004540 01  WRK-AREA-POOL1285.                                           
004550     03  WRK-DT-ENTRADA          PIC  9(08) COMP-3 VALUE ZEROS.   
004560     03  WRK-NUM-DIAS            PIC S9(05) COMP-3 VALUE ZEROS.   
004570     03  WRK-DT-SAIDA            PIC  9(08) COMP-3 VALUE ZEROS.   
004580     03  WRK-MENSAGEM            PIC  X(50)        VALUE SPACES.  
004590                                                                  
004600 01  AUX-DT-INV-AAAAMMDD         PIC  9(08).                      
004610 01  FILLER  REDEFINES  AUX-DT-INV-AAAAMMDD.                      
004620     03  AUX-DT-INV-AA           PIC  9(04).                      
004630     03  AUX-DT-INV-MM           PIC  9(02).                      
004640     03  AUX-DT-INV-DD           PIC  9(02).                      

       01 WRK-CHAVE-SOR.
           05 WRK-CHAVE-SOR-NAT.
001600         07 WRK-NUM-CPF-SOR          PIC 9(09)    COMP-3.
001610         07 WRK-FIL-CPF-SOR          PIC 9(05)    COMP-3.
001620         07 WRK-CTR-CPF-SOR          PIC 9(03)    COMP-3.
001630         07 WRK-NATUREZA-SOR         PIC X(02).
001720     05 WRK-CONTRATO-SOR             PIC 9(07)    COMP-3.
001770     05 WRK-CHAVE-LP-CL-SOR.
001780        07 WRK-COD-JUN-SOR           PIC 9(05)    COMP-3.
001790        07 WRK-AGE-OP-SOR            PIC 9(05)    COMP-3.
001800*       07 WRK-NUMERO-CL-SOR         PIC 9(15)    COMP-3.
004650                                                                  
       01 WRK-CHAVE-REG.
           05 WRK-CHAVE-REG-NAT.
001600         07 WRK-NUM-CPF-REG          PIC 9(09)    COMP-3.
001610         07 WRK-FIL-CPF-REG          PIC 9(05)    COMP-3.
001620         07 WRK-CTR-CPF-REG          PIC 9(03)    COMP-3.
001630         07 WRK-NATUREZA-REG         PIC X(02).
001720     05 WRK-CONTRATO-REG             PIC 9(07)    COMP-3.
001770     05 WRK-CHAVE-LP-CL-REG.
001780        07 WRK-COD-JUN-REG           PIC 9(05)    COMP-3.
001790        07 WRK-AGE-OP-REG            PIC 9(05)    COMP-3.
001800*       07 WRK-NUMERO-CL-REG         PIC 9(15)    COMP-3.
004650
004660*---------------------------------------------------------------* 
004670*    AREA AUXILIAR                                              * 
004680*---------------------------------------------------------------* 
004690                                                                  
004700 01  FILLER                      PIC  X(030) VALUE                
004710     'AREA DE PARAMETROS DA POOL7100'.                            
-INC POL7100C                                                           
004730     EJECT                                                        
004740                                                                  
004750 01  WRK-RECURSO                 PIC  X(008) VALUE   'BATCH'.     
004760 01  WRK-ABEND                   PIC  S9(04) VALUE   +1111 COMP.  
004770                                                                  
004780     EJECT                                                        
004790                                                                  
004800 LINKAGE  SECTION.                                                
004810*-----------------*                                               
004820                                                                  
004830 01  LNK-DADOS.                                                   
004840     02  LNK-TAMANHO             PIC S9(04) COMP.                 
004850     02  LNK-VALOR               PIC 9(09).                       
004860                                                                  
004870/                                                                 
004880*---------------------------------------------------------------* 
004890 PROCEDURE DIVISION USING LNK-DADOS.                              
004900*---------------------------------------------------------------* 
004910                                                                  
004920 INICIO.                                                          
004930                                                                  
004940     OPEN  INPUT   PARMCLLP CADPENDE  ARQDATA                     
004950           OUTPUT  CADSELEC  TOTAIS.                              
004960                                                                  
004970                                                                  
004980     PERFORM   666666-LER-PARMCLLP.                               
004990                                                                  
005000     PERFORM   777777-TRATA-PARMCLLP                              
005010         UNTIL  WRK-FS-PARMCLLP   EQUAL   '10'.                   
005020                                                                  
005030                                                                  
005040     CALL   'POOL7600' USING WRK-AREA-POOL7600.                   
005050                                                                  
005060      READ  ARQDATA AT END                                        
005070                    DISPLAY 'CLLP7616 - ARQ DATA VAZIO.'          
005080                    GOBACK.                                       
005090                                                                  
005100     MOVE    DTMOVINV          TO     WRK-DT-ENTRADA              
005110     DISPLAY 'DATA DO MOVIMENTO = ' WRK-DT-ENTRADA                
005120                                                                  
005130     IF  LNK-VALOR NOT NUMERIC                                    
005140         MOVE ZEROS TO LNK-VALOR.                                 
005150     DISPLAY 'VALOR BASE = ' LNK-VALOR.                           
005160                                                                  
005170******************************************************            
005180*****  CALCULO DO PRIMEIRO AVISO:  DATA ATUAL  *******            
005190******************************************************            
005200                                                                  
005210     COMPUTE WRK-NUM-DIAS      =       WRK-DIAS1      - 1         
005220     COMPUTE WRK-NUM-DIAS      =     ( WRK-NUM-DIAS * -1 )        
005230*****MOVE    -06               TO     WRK-NUM-DIAS                
005240*****MOVE    -04               TO     WRK-NUM-DIAS                
005250     CALL   'POOL1285' USING WRK-AREA-POOL1285.                   
005260     IF RETURN-CODE NOT EQUAL ZEROS                               
005270        MOVE    RETURN-CODE   TO  WRK-RETURN-CODE                 
005280        DISPLAY '******************* CLLP7616 *******************'
005290        DISPLAY '*   ERRO  UTILIZANDO  A  POOL1285    '           
005300        DISPLAY '*                                    '           
005310        DISPLAY '*   DATA-ENTRADA     = ' WRK-DT-ENTRADA          
005320        DISPLAY '*   RETURN-CODE      = ' WRK-RETURN-CODE         
005330        DISPLAY '*   MENSAGEM DO ERRO = ' WRK-MENSAGEM            
005340        DISPLAY '*                                    '           
005350        DISPLAY '******************* CLLP7616 *******************'
005360     ELSE                                                         
005370********DISPLAY 'DATA DE -07 = ' WRK-DT-SAIDA                     
005380        DISPLAY 'DATA DE -05 = ' WRK-DT-SAIDA                     
005390        MOVE   WRK-DT-SAIDA       TO  AUX-DT-INV-AAAAMMDD         
005400        MOVE   AUX-DT-INV-AA      TO  ANO-LI-INV-1A-CARTA         
005410        MOVE   AUX-DT-INV-MM      TO  MES-LI-INV-1A-CARTA         
005420        MOVE   AUX-DT-INV-DD      TO  DIA-LI-INV-1A-CARTA.        
005430                                                                  
005440                                                                  
005450************************************************************      
005460*****  CALCULO DO PRIMEIRO AVISO:  DATA ATUAL - 6 DIAS *****      
005470************************************************************      
005480                                                                  
005490                                                                  
005500     MOVE    WRK-DIAS1         TO      WRK-NUM-DIAS               
005510     COMPUTE WRK-NUM-DIAS      =     ( WRK-NUM-DIAS * -1)         
005520**** MOVE    -07               TO     WRK-NUM-DIAS                
005530**** MOVE    -05               TO     WRK-NUM-DIAS                
005540     CALL   'POOL1285' USING WRK-AREA-POOL1285.                   
005550     IF RETURN-CODE NOT EQUAL ZEROS                               
005560        MOVE    RETURN-CODE   TO  WRK-RETURN-CODE                 
005570        DISPLAY '******************* CLLP7616 *******************'
005580        DISPLAY '*   ERRO  UTILIZANDO  A  POOL1285    '           
005590        DISPLAY '*                                    '           
005600        DISPLAY '*   DATA-ENTRADA     = ' WRK-DT-ENTRADA          
005610        DISPLAY '*   RETURN-CODE      = ' WRK-RETURN-CODE         
005620        DISPLAY '*   MENSAGEM DO ERRO = ' WRK-MENSAGEM            
005630        DISPLAY '*                                    '           
005640        DISPLAY '******************* CLLP7616 *******************'
005650     ELSE                                                         
005660********DISPLAY 'DATA DE -08 = ' WRK-DT-SAIDA                     
005670        DISPLAY 'DATA DE -06 = ' WRK-DT-SAIDA                     
005680        MOVE   WRK-DT-SAIDA       TO  AUX-DT-INV-AAAAMMDD         
005690        MOVE   AUX-DT-INV-AA      TO  ANO-LI-INV-08               
005700        MOVE   AUX-DT-INV-MM      TO  MES-LI-INV-08               
005710        MOVE   AUX-DT-INV-DD      TO  DIA-LI-INV-08.              
005720                                                                  
      ************************************************************      
      *****  CALCULO:  DATA ATUAL - 10 DIAS                  *****      
      ************************************************************      
                                                                        
           MOVE    -10               TO     WRK-NUM-DIAS                
           CALL   'POOL1285' USING WRK-AREA-POOL1285.                   
           IF RETURN-CODE NOT EQUAL ZEROS                               
              MOVE    RETURN-CODE   TO  WRK-RETURN-CODE                 
              DISPLAY '******************* CLLP7616 *******************'
              DISPLAY '*   ERRO  UTILIZANDO  A  POOL1285    '           
              DISPLAY '*                                    '           
              DISPLAY '*   DATA-ENTRADA     = ' WRK-DT-ENTRADA          
              DISPLAY '*   RETURN-CODE      = ' WRK-RETURN-CODE         
              DISPLAY '*   MENSAGEM DO ERRO = ' WRK-MENSAGEM            
              DISPLAY '*                                    '           
              DISPLAY '******************* CLLP7616 *******************'
           ELSE                                                         
              DISPLAY 'DATA DE -10 = ' WRK-DT-SAIDA                     
              MOVE   WRK-DT-SAIDA       TO  AUX-DT-INV-AAAAMMDD         
              MOVE   AUX-DT-INV-AA      TO  ANO-LI-INV-10               
              MOVE   AUX-DT-INV-MM      TO  MES-LI-INV-10               
              MOVE   AUX-DT-INV-DD      TO  DIA-LI-INV-10.              
005730                                                                  
005740**************************************************************    
005750*****  CALCULO DO SEGUNDO  AVISO:  DATA ATUAL - 20 DIAS******     
005760**************************************************************    
005770                                                                  
005780     COMPUTE WRK-NUM-DIAS      =       WRK-DIAS2      - 1         
005790     COMPUTE WRK-NUM-DIAS      =     ( WRK-NUM-DIAS * -1 )        
005800*****MOVE    -19               TO     WRK-NUM-DIAS                
005810*****MOVE     -9               TO     WRK-NUM-DIAS                
005820     CALL   'POOL1285' USING WRK-AREA-POOL1285.                   
005830     IF RETURN-CODE NOT EQUAL ZEROS                               
005840        MOVE    RETURN-CODE   TO  WRK-RETURN-CODE                 
005850        DISPLAY '******************* CLLP7616 *******************'
005860        DISPLAY '*   ERRO  UTILIZANDO  A  POOL1285    '           
005870        DISPLAY '*                                    '           
005880        DISPLAY '*   DATA-ENTRADA     = ' WRK-DT-ENTRADA          
005890        DISPLAY '*   RETURN-CODE      = ' WRK-RETURN-CODE         
005900        DISPLAY '*   MENSAGEM DO ERRO = ' WRK-MENSAGEM            
005910        DISPLAY '*                                    '           
005920        DISPLAY '******************* CLLP7616 *******************'
005930     ELSE                                                         
005940        DISPLAY 'DATA DE -20 = ' WRK-DT-SAIDA                     
005950********DISPLAY 'DATA DE -10 = ' WRK-DT-SAIDA                     
005960        MOVE   WRK-DT-SAIDA       TO  AUX-DT-INV-AAAAMMDD         
005970        MOVE   AUX-DT-INV-AA      TO  ANO-LI-INV-2A-CARTA         
005980        MOVE   AUX-DT-INV-MM      TO  MES-LI-INV-2A-CARTA         
005990        MOVE   AUX-DT-INV-DD      TO  DIA-LI-INV-2A-CARTA         
006000        MOVE   'S'                TO  WRK-SEG-AVISO.              
006010                                                                  
006020                                                                  
006030*****  CALCULO DO SEGUNDO  AVISO:  DATA ATUAL - 21 DIAS           
006040                                                                  
006050     MOVE    WRK-DIAS2         TO      WRK-NUM-DIAS               
006060     COMPUTE WRK-NUM-DIAS      =     ( WRK-NUM-DIAS * -1)         
006070*****MOVE    -20               TO     WRK-NUM-DIAS                
006080*****MOVE    -10               TO     WRK-NUM-DIAS                
006090     CALL   'POOL1285' USING WRK-AREA-POOL1285.                   
006100     IF RETURN-CODE NOT EQUAL ZEROS                               
006110        MOVE    RETURN-CODE   TO  WRK-RETURN-CODE                 
006120        DISPLAY '******************* CLLP7616 *******************'
006130        DISPLAY '*   ERRO  UTILIZANDO  A  POOL1285    '           
006140        DISPLAY '*                                    '           
006150        DISPLAY '*   DATA-ENTRADA     = ' WRK-DT-ENTRADA          
006160        DISPLAY '*   RETURN-CODE      = ' WRK-RETURN-CODE         
006170        DISPLAY '*   MENSAGEM DO ERRO = ' WRK-MENSAGEM            
006180        DISPLAY '*                                    '           
006190        DISPLAY '******************* CLLP7616 *******************'
006200     ELSE                                                         
006210        DISPLAY 'DATA DE -21 = ' WRK-DT-SAIDA                     
006220********DISPLAY 'DATA DE -11 = ' WRK-DT-SAIDA                     
006230        MOVE   WRK-DT-SAIDA       TO  AUX-DT-INV-AAAAMMDD         
006240        MOVE   AUX-DT-INV-AA      TO  ANO-LI-INV-21               
006250        MOVE   AUX-DT-INV-MM      TO  MES-LI-INV-21               
006260        MOVE   AUX-DT-INV-DD      TO  DIA-LI-INV-21.              
006270                                                                  
006280*****  CALCULO:  DATA ATUAL - 40 DIAS                             
006290                                                                  
006300     COMPUTE WRK-NUM-DIAS      =       WRK-DIAS3      - 1         
006310     COMPUTE WRK-NUM-DIAS      =     ( WRK-NUM-DIAS * -1 )        
006320*****MOVE    -39               TO     WRK-NUM-DIAS                
006330     CALL   'POOL1285' USING WRK-AREA-POOL1285.                   
006340     IF RETURN-CODE NOT EQUAL ZEROS                               
006350        MOVE    RETURN-CODE   TO  WRK-RETURN-CODE                 
006360        DISPLAY '******************* CLLP7616 *******************'
006370        DISPLAY '*   ERRO  UTILIZANDO  A  POOL1285    '           
006380        DISPLAY '*                                    '           
006390        DISPLAY '*   DATA-ENTRADA     = ' WRK-DT-ENTRADA          
006400        DISPLAY '*   RETURN-CODE      = ' WRK-RETURN-CODE         
006410        DISPLAY '*   MENSAGEM DO ERRO = ' WRK-MENSAGEM            
006420        DISPLAY '*                                    '           
006430        DISPLAY '******************* CLLP7616 *******************'
006440     ELSE                                                         
006450        DISPLAY 'DATA DE -40 = ' WRK-DT-SAIDA                     
006460        MOVE   WRK-DT-SAIDA       TO  AUX-DT-INV-AAAAMMDD         
006470        MOVE   AUX-DT-INV-AA      TO  ANO-LI-INV-3A-CARTA         
006480        MOVE   AUX-DT-INV-MM      TO  MES-LI-INV-3A-CARTA         
006490        MOVE   AUX-DT-INV-DD      TO  DIA-LI-INV-3A-CARTA.        
006500                                                                  
006510                                                                  
006520*****  CALCULO:  DATA ATUAL - 41 DIAS                             
006530                                                                  
006540                                                                  
006550     MOVE    WRK-DIAS3         TO      WRK-NUM-DIAS               
006560     COMPUTE WRK-NUM-DIAS      =     ( WRK-NUM-DIAS * -1)         
006570*****MOVE    -40               TO     WRK-NUM-DIAS                
006580     CALL   'POOL1285' USING WRK-AREA-POOL1285.                   
006590     IF RETURN-CODE NOT EQUAL ZEROS                               
006600        MOVE    RETURN-CODE   TO  WRK-RETURN-CODE                 
006610        DISPLAY '******************* CLLP7616 *******************'
006620        DISPLAY '*   ERRO  UTILIZANDO  A  POOL1285    '           
006630        DISPLAY '*                                    '           
006640        DISPLAY '*   DATA-ENTRADA     = ' WRK-DT-ENTRADA          
006650        DISPLAY '*   RETURN-CODE      = ' WRK-RETURN-CODE         
006660        DISPLAY '*   MENSAGEM DO ERRO = ' WRK-MENSAGEM            
006670        DISPLAY '*                                    '           
006680        DISPLAY '******************* CLLP7616 *******************'
006690     ELSE                                                         
006700        DISPLAY 'DATA DE -41 = ' WRK-DT-SAIDA                     
006710        MOVE   WRK-DT-SAIDA       TO  AUX-DT-INV-AAAAMMDD         
006720        MOVE   AUX-DT-INV-AA      TO  ANO-LI-INV-41               
006730        MOVE   AUX-DT-INV-MM      TO  MES-LI-INV-41               
006740        MOVE   AUX-DT-INV-DD      TO  DIA-LI-INV-41.              
006750                                                                  
006760                                                                  
006770     READ    CADPENDE AT END      GO TO FINALIZA.                 
006780     SORT    ARQSORT ASCENDING KEY CHAVE-SOR-NAT
                                         CONTRATO-SOR
                                         CHAVE-LP-CL-SOR
006790     INPUT   PROCEDURE   CLASSIFICA    THRU     FIM-CLASSIFICA    
006800     OUTPUT  PROCEDURE   CLASSIFICOU   THRU     FIM-CLASSIFICOU.  
006810     PERFORM FINALIZA                  THRU     FIM-FINALIZA.     
006820     CLOSE PARMCLLP CADPENDE  ARQDATA CADSELEC TOTAIS.            
006830     STOP RUN.                                                    
006840                                                                  
006850*---------------------------------------------------------------* 
006860/                                                                 
006870*---------------------------------------------------------------* 
006880                                                                  
006890*---------------------------------------------------------------* 
006900 666666-LER-PARMCLLP             SECTION.                         
006910*---------------------------------------------------------------* 
006920                                                                  
006930     READ   PARMCLLP.                                             
006940                                                                  
006950     MOVE  'LEITURA'     TO WRK-OPERACAO                          
006960     PERFORM  888888-TESTAR-FS-PARMCLLP                           
006970                                                                  
006980     IF  WRK-FS-PARMCLLP   EQUAL   '10'                           
006990         GO                TO      666666-99-FIM.                 
007000                                                                  
007010     DISPLAY 'PARM-CCUSTO-1 = '  PARM-CCUSTO-1.                   
007020     DISPLAY 'PARM-CODIGO-1 = '  PARM-CODIGO-1.                   
007030     DISPLAY 'PARM-CCUSTO-2 = '  PARM-CCUSTO-2.                   
007040     DISPLAY 'PARM-CODIGO-2 = '  PARM-CODIGO-2.                   
007050                                                                  
007060                                                                  
007070     IF  PARM-CCUSTO-1   NOT EQUAL   'CLLP'   OR                  
007080         PARM-CODIGO-1   NOT EQUAL   '0002'   OR                  
007090         PARM-CCUSTO-2   NOT EQUAL   'CLLP'   OR                  
007100         PARM-CODIGO-2   NOT EQUAL   '7615'                       
007110         DISPLAY '****************  CLLP7620  *******************'
007120         DISPLAY '*           ABEND 1111 - FORCADO              *'
007130         DISPLAY '*  PROGRAMA CANCELADO - PARAMETRO INVALIDO    *'
007140         DISPLAY '* '  PARM-CCUSTO-1 ' / ' PARM-CODIGO-1          
007150                 ' / ' PARM-CCUSTO-2 ' / ' PARM-CODIGO-2          
007160         DISPLAY '*   AVISAR ANALISTA RESPONSAVEL DA ROTINA     *'
007170         DISPLAY '*****************  CLLP7620  ******************'
007180         CALL  'ILBOABN0'  USING  WRK-ABEND.                      
007190                                                                  
007200*---------------------------------------------------------------* 
007210 666666-99-FIM.                  EXIT.                            
007220*---------------------------------------------------------------* 
007230                                                                  
007240*---------------------------------------------------------------* 
007250 777777-TRATA-PARMCLLP           SECTION.                         
007260*---------------------------------------------------------------* 
007270                                                                  
007280     IF  PARM-SEQUENCIA   EQUAL   1                               
007290         MOVE  PARM-NR-DIAS    TO   WRK-DIAS1                     
007300         MOVE  PARM-VL-CORTE   TO   WRK-VL-CORTE1                 
007310     ELSE                                                         
007320         IF  PARM-SEQUENCIA   EQUAL   2                           
007330             MOVE  PARM-NR-DIAS    TO   WRK-DIAS2                 
007340             MOVE  PARM-VL-CORTE   TO   WRK-VL-CORTE2             
007350         ELSE                                                     
007360             IF  PARM-SEQUENCIA   EQUAL   3                       
007370                 MOVE  PARM-NR-DIAS    TO   WRK-DIAS3             
007380                 MOVE  PARM-VL-CORTE   TO   WRK-VL-CORTE3         
007390             ELSE                                                 
007400                 IF  PARM-SEQUENCIA   EQUAL   4                   
007410                     MOVE  PARM-NR-DIAS    TO   WRK-DIAS4         
007420                     MOVE  PARM-VL-CORTE   TO   WRK-VL-CORTE4.    
007430                                                                  
007440     PERFORM   666666-LER-PARMCLLP.                               
007450                                                                  
007460*---------------------------------------------------------------* 
007470 777777-99-FIM.                  EXIT.                            
007480*---------------------------------------------------------------* 
007490                                                                  
007500*---------------------------------------------------------------* 
007510 888888-TESTAR-FS-PARMCLLP       SECTION.                         
007520*---------------------------------------------------------------* 
007530                                                                  
007540     IF  WRK-FS-PARMCLLP   NOT EQUAL   '00'  AND '10'             
007550         DISPLAY '************** CLLP7616 *************'          
007560         DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'      
007570         DISPLAY '*              PARMCLLP             *'          
007580         DISPLAY '*         FILE STATUS =  ' WRK-FS-PARMCLLP      
007590                                            '         *'          
007600         DISPLAY '************** CLLP7616 *************'          
007610         CALL  'ILBOABN0'   USING    WRK-ABEND.                   
007620                                                                  
007630*---------------------------------------------------------------* 
007640 888888-99-FIM.                  EXIT.                            
007650*---------------------------------------------------------------* 
007660                                                                  
007670*---------------------------------------------------------------* 
007680                                                                  
007690                                                                  
007700 CLASSIFICA SECTION.                                              
007710                                                                  
007720     CALL 'POOL0300' USING CADASTRO REG-CADASTRO.                 
007730     IF  RETURN-CODE EQUAL 4                                      
007740         GO TO FIM-CLASSIFICA.                                    
007750*                                                                 
007760**** IF  NUM-CGC-CAD LESS 70000000                                
007770****     GO TO CLASSIFICA.                                        
007780*                                                                 
007790                                                                  
007800     ADD 1 TO AC-LIDOS-CAD.                                       
007810     MOVE CAD-VRINI  TO  WRK-VL-CL-LIQ.                           
007820                                                                  
007830     MOVE ZEROS      TO  CAD-NCGC1                                
007840                         CAD-FIL1                                 
007850                         CAD-CTR1                                 
007860                         CAD-NCGC2                                
007870                         CAD-FIL2                                 
007880                         CAD-CTR2.                                
007890     MOVE SPACES     TO  CAD-NOME1                                
007900                         CAD-NOME2.                               
007910                                                                  
007920     MOVE DAT-VENCTO-CAD TO DAT-VENCTO-AUX.                       
007930     MOVE DIA-AUX   TO DIA-AUX-INV.                               
007940     MOVE MES-AUX   TO MES-AUX-INV.                               
007950     MOVE ANO-AUX   TO ANO-AUX-INV.                               
007960                                                                  
007970     MOVE  CAD-DTECL  TO  DATA-CL-AUX.                            
007980     MOVE  DIA-CL     TO  DIA-CL-INV.                             
007990     MOVE  MES-CL     TO  MES-CL-INV.                             
008000     MOVE  ANO-CL     TO  ANO-CL-INV.                             
008010                                                                  
008020     IF  (EMP-CAD        EQUAL   6500 OR 6699)  AND               
008030         (FIL-CGC-CAD    GREATER ZEROS)                           
008040         ADD 1 TO AC-DESPREZADO                                   
008050         GO  TO  CLASSIFICA.                                      
008060                                                                  
008020     IF  CAD-AGEN       EQUAL   7881                              
008040         ADD 1 TO AC-DESPREZADO                                   
008050         GO  TO  CLASSIFICA.                                      
008060                                                                  
008020     IF  CAD-EMISSAO    EQUAL   'Z'                               
008040         ADD 1 TO AC-DESPREZADO                                   
008050         GO  TO  CLASSIFICA.                                      
008060                                                                  
BRQ=E******IF  CARTEIRA-CAD   EQUAL   702  OR 088  OR 294  OR           
BRQ=E******                           278  OR 280  OR 281  OR           
BRQ=E******                           084  OR 085                       
BRQ=I      IF  CAD-CART2      EQUAL   '702'  OR '088'  OR '294'  OR     
BRQ=I                                 '278'  OR '280'  OR '281'  OR     
BRQ=I                                 '084'  OR '085'                   
008040         ADD 1 TO AC-DESPREZADO                                   
008050         GO  TO  CLASSIFICA.                                      
008060                                                                  
008070     IF BAIXA-CAD NOT = 1 AND 7                                   
008080        MOVE 1 TO CHAVE-INCONS                                    
008090     ELSE                                                         
008100           IF NUM-CGC-CAD = ZEROS                                 
008110             MOVE 1 TO CHAVE-INCONS                               
008120           ELSE                                                   
008130              MOVE NUM-CGC-CAD TO NUM-CPF-CALL                    
008140              MOVE FIL-CGC-CAD TO FILIAL-CALL                     
008150              CALL 'POOL0110' USING CPFCGC-CALL CONTROLE-CALL     
008160              IF  CONTROLE-CALL NOT NUMERIC                       
008170                  MOVE 1 TO CHAVE-INCONS                          
008180               ELSE                                               
008190               IF CONTROLE-CALL NOT = CONTROLE-CAD                
008200                  MOVE 1 TO CHAVE-INCONS                          
008210             ELSE                                                 
008220               IF NOME-CAD-39 = SPACES OR                         
008230                  NOME-CAD-39 = ALL '*'                           
008240                 MOVE 1 TO CHAVE-INCONS                           
008250               ELSE                                               
008260                 IF EMP-CAD = EMP-ANT                             
008270                    PERFORM PESQUISA-TAB                          
008280                 ELSE                                             
008290                   IF FIM-CADPENDE = 1                            
008300                     MOVE 1 TO CHAVE-INCONS                       
008310                   ELSE                                           
008320                     MOVE 0 TO IND                                
008330                     PERFORM CARGA-TAB THRU FIM-CARGA-TAB         
008340                     MOVE EMP-CAD TO EMP-ANT                      
008350                     PERFORM PESQUISA-TAB.                        
008360                                                                  
008370     MOVE  DIA-AUX    TO  DIA-VEN.                                
008380     MOVE  MES-AUX    TO  MES-VEN.                                
008390     MOVE  ANO-AUX    TO  ANO-VEN.                                
008400                                                                  
008410*    IF DAT-VENCTO-AUX  GREATER DATA-LIMITE-INV-R                 
008420*       MOVE  0  TO  CHAVE-INCONS                                 
008430*       ADD   1  TO  AC-DESPREZADO                                
008440*       GO       TO  CLASSIFICA.                                  
008450                                                                  
BRQ=E******IF CARTEIRA-CAD    EQUAL 373 OR 376                          
BRQ=I      IF CAD-CART2       EQUAL '373' OR '376'                      
              IF DAT-VCTO-INV GREATER DATA-LIMITE-INV-08                
                 MOVE 0 TO CHAVE-INCONS                                 
                 ADD  1 TO AC-DESPREZADO                                
                 GO     TO CLASSIFICA.                                  
                                                                        
008460     MOVE  DAT-VCTO-INV    TO  DAT-VENCTO-SOR.                    
008470                                                                  
008480     IF CHAVE-INCONS = 1                                          
008490        MOVE 0 TO CHAVE-INCONS                                    
008500        ADD  1 TO AC-DESPREZADO                                   
008510**      PERFORM ROT-DISPLAY                                       
008520        GO       TO  CLASSIFICA.                                  
008530*                                                                 
008540     IF NOME-CAD   =         '0123456789*'                        
008550        MOVE 0 TO CHAVE-INCONS                                    
008560        ADD  1 TO AC-DESPREZADO                                   
008570**      PERFORM ROT-DISPLAY                                       
008580        GO       TO  CLASSIFICA.                                  
008590*                                                                 
008600       MOVE      NOME-CAD          TO   WRK-NOME                  
008610                                                                  
008620       PERFORM   INSPECT-NOMECAD                                  
008630       VARYING   IND   FROM 1 BY 1                                
008640       UNTIL     IND   GREATER 40                                 
008650                                                                  
008660       MOVE      WRK-NOME           TO   NOME-CAD                 
008670                                                                  
008680       MOVE      CAD-CC             TO   CC-OP-SOR                
BRQ=E********MOVE      CARTEIRA-CAD       TO   CARTEIRA-SOR             
BRQ=I        MOVE      CAD-CART2          TO   CARTEIRA-SOR             
008700       MOVE      CAD-CONTR          TO   CONTRATO-SOR             
008710       MOVE      CHAVE-CAD          TO   CHAVE-LP-CL-SOR          
008720       MOVE      NUM-CGC-CAD        TO   NUM-CPF-SOR              
008730       MOVE      FIL-CGC-CAD        TO   FIL-CPF-SOR              
008740       MOVE      CONTROLE-CAD       TO   CTR-CPF-SOR              
008750       MOVE      WRK-VL-CL-LIQ      TO   VAL-RESG-SOR             
008760       MOVE      VAL-ENC-VEN-CAD    TO   VAL-ENC-VEN-SOR          
008770       MOVE      VAL-ENC-VIN-CAD    TO   VAL-ENC-VIN-SOR
008780       MOVE      CAMPOS-CAD         TO   CAMPOS-SOR               
008790                                                                  
008800       IF        CAD-NATUREZA NOT NUMERIC                         
008810                 MOVE ZEROS TO    CAD-NATUREZA.                   
008820                                                                  
008830       MOVE      CAD-NATUREZA       TO   COD-NATUREZA-SOR         
008840       MOVE      SPACES             TO   NOME-3-AVAL-SOR          
008850       MOVE      ZEROS              TO   CGC-NRO-SOR              
008860                                         CGC-FIL-SOR              
008870                                         CGC-CTR-SOR
             MOVE     CAD-LT               TO SOR-LT
008880       RELEASE   REG-SOR.                                         
008890                                                                  
008900     GO TO CLASSIFICA.                                            
008910                                                                  
008920 FIM-CLASSIFICA.                EXIT.                             
008930                                                                  
008940*---------------------------------------------------------------* 
008950/                                                                 
008960*---------------------------------------------------------------* 
008970                                                                  
008980 INSPECT-NOMECAD  SECTION.                                        
008990                                                                  
009000      IF    WRK-NOMECAD(IND)  EQUAL '0' OR '1' OR '2' OR '3' OR   
009010                                    '4' OR '5' OR '6' OR '7' OR   
009020                                    '8' OR '9' OR '*'             
009030          MOVE   SPACES   TO WRK-NOMECAD(IND).                    
009040                                                                  
009050 FIM-INSPECT. EXIT.                                               
009060                                                                  
009070*---------------------------------------------------------------* 
009080/                                                                 
009090*---------------------------------------------------------------* 
009100                                                                  
009110 LE-SORTE SECTION.                                                
009120                                                                  
009130                                                                  
009140     RETURN    ARQSORT       AT       END                         
009150         MOVE    'S'               TO      FIM-SORT               
009160         MOVE    HIGH-VALUES       TO      CHAVE-SOR.             
009170                                                                  
009180 FIM-LE-SORTE.                 EXIT.                              
009190                                                                  
009200*---------------------------------------------------------------* 
009210/                                                                 
009220*---------------------------------------------------------------* 
009230                                                                  
009240 CLASSIFICOU SECTION.                                             
009250                                                                  
009260         PERFORM   LE-SORTE         THRU        FIM-LE-SORTE      
009270         IF FIM-SORT EQUAL 'S'                                    
009280            GO TO FIM-CLASSIFICOU.                                
009290                                                                  
009300         MOVE      REG-SOR          TO          REG-WOR

009310         PERFORM   LE-SORTE         THRU        FIM-LE-SORTE.     
009320                                                                  
009330 CLASSIFICANDO.                                                   
009340
           MOVE CHAVE-SOR-NAT      TO WRK-CHAVE-SOR-NAT
           MOVE CONTRATO-SOR       TO WRK-CONTRATO-SOR
      *    MOVE CHAVE-LP-CL-SOR    TO WRK-CHAVE-LP-CL-SOR
           MOVE COD-JUN-SOR        TO WRK-COD-JUN-SOR
           MOVE AGE-OP-SOR         TO WRK-AGE-OP-SOR

           MOVE CHAVE-WOR-NAT      TO WRK-CHAVE-REG-NAT
           MOVE CONTRATO-WOR       TO WRK-CONTRATO-REG
      *    MOVE CHAVE-LP-CL-WOR    TO WRK-CHAVE-LP-CL-REG
           MOVE COD-JUN-WOR        TO WRK-COD-JUN-REG
           MOVE AGE-OP-WOR         TO WRK-AGE-OP-REG

009350     IF  WRK-CHAVE-SOR    EQUAL    WRK-CHAVE-REG
009360         ADD     VAL-RESG-SOR        TO          VAL-RESG-WOR     
009370         ADD     VAL-ENC-VEN-SOR     TO          VAL-ENC-VEN-WOR  
009380         ADD     VAL-ENC-VIN-SOR     TO          VAL-ENC-VIN-WOR

               DISPLAY 'CAMPOS LEI: '
               DISPLAY 'TAXA CONTRATO SOR: ' SOR-TAXA-CONTRATO
               DISPLAY 'TAXA CONTRATO WOR: ' WOR-TAXA-CONTRATO
               ADD SOR-TAXA-CONTRATO       TO WOR-TAXA-CONTRATO

               DISPLAY 'VR REMUNERATORIO : ' SOR-VR-REMUNERATORIO
               DISPLAY 'VR REMUNERATORIO : ' WOR-VR-REMUNERATORIO
               ADD SOR-VR-REMUNERATORIO    TO WOR-VR-REMUNERATORIO
               DISPLAY 'MORATORIO        : ' SOR-VALOR-MORATORIO
               DISPLAY 'MORATORIO        : ' WOR-VALOR-MORATORIO
               ADD SOR-VALOR-MORATORIO     TO WOR-VALOR-MORATORIO
               DISPLAY 'VR MULTA         : ' SOR-VALOR-MULTA
               DISPLAY 'VR MULTA         : ' WOR-VALOR-MULTA
               ADD SOR-VALOR-MULTA         TO WOR-VALOR-MULTA
               DISPLAY 'VR DESP JUD      : ' SOR-DESP-JUD-CUSTAS
               DISPLAY 'VR DESP JUD      : ' WOR-DESP-JUD-CUSTAS
               ADD SOR-DESP-JUD-CUSTAS     TO WOR-DESP-JUD-CUSTAS
               DISPLAY 'HONORARIOS       : ' SOR-HONORARIOS
               DISPLAY 'HONORARIOS       : ' WOR-HONORARIOS
               ADD SOR-HONORARIOS          TO WOR-HONORARIOS
               DISPLAY 'TOTAL DIVIDA     : ' SOR-VL-TOTAL-DIVIDA
               DISPLAY 'TOTAL DIVIDA     : ' WOR-VL-TOTAL-DIVIDA
               ADD SOR-VL-TOTAL-DIVIDA     TO WOR-VL-TOTAL-DIVIDA
               DISPLAY 'TAXA TARIFA      : ' SOR-VL-TAXA-TARIFA
               DISPLAY 'TAXA TARIFA      : ' WOR-VL-TAXA-TARIFA
               ADD SOR-VL-TAXA-TARIFA      TO WOR-VL-TAXA-TARIFA

009390         PERFORM LE-SORTE            THRU        FIM-LE-SORTE     
009400         GO  TO  CLASSIFICANDO                                    
009410     ELSE                                                         
009420****     PERFORM VERIFICA-DATAS  THRU    VERIFICOU                
009430         WRITE   REG-CADSELEC    FROM    REG-WOR                  
009440         ADD     1               TO      AC-SELEC                 
009450         IF  CHAVE-SOR    NOT EQUAL    HIGH-VALUES                
009460             MOVE   REG-SOR      TO      REG-WOR                  
009470             PERFORM  LE-SORTE  THRU   FIM-LE-SORTE               
009480             GO  TO   CLASSIFICANDO.                              
009490                                                                  
009500 FIM-CLASSIFICOU.               EXIT.                             
009510                                                                  
009520                                                                  
009530*---------------------------------------------------------------* 
009540 VERIFICA-DATAS SECTION.                                          
009550                                                                  
009560*****IF (DAT-VENCTO-WOR     GREATER DATA-LIM-INV-R-08 AND         
009570*****    DAT-VENCTO-WOR NOT GREATER DATA-LIM-INV-R-1A-CARTA    ) O
009580*****   (DAT-VENCTO-WOR     GREATER DATA-LIM-INV-R-21 AND         
009590*****    DAT-VENCTO-WOR NOT GREATER DATA-LIM-INV-R-2A-CARTA    ) O
009600*****   (DAT-VENCTO-WOR     GREATER DATA-LIM-INV-R-41 AND         
009610*****    DAT-VENCTO-WOR NOT GREATER DATA-LIM-INV-R-3A-CARTA    )  
009620*****   MOVE    DAT-VENCTO-WOR    TO  DATA-LIMITE-AUX             
009630*****   MOVE    11111111          TO  DAT-VENCTO-WOR              
009640*****   WRITE   REG-CADSELEC    FROM  REG-WOR                     
009650*****   ADD     1               TO      AC-SELEC                  
009660*****   MOVE    DATA-LIMITE-AUX   TO  DAT-VENCTO-WOR.             
009670                                                                  
009680                                                                  
009690***     IF    AGE-OP-WOR = 562 AND CC-OP-WOR = 285                
009700***           DISPLAY 'GRAVOU CHV = ' CHAVE-LP-CL-WOR             
009710***                   ' VALOR = ' VAL-RESG-WOR                    
009720***                   ' VCTO = '  DAT-VENCTO-WOR.                 
009730                                                                  
009740                                                                  
009750                                                                  
009760 VERIFICOU.  EXIT.                                                
009770/                                                                 
009780*---------------------------------------------------------------* 
009790                                                                  
009800 FINALIZA SECTION.                                                
009810                                                                  
009820     WRITE REG-IMP FROM CAB1.                                     
009830                                                                  
009840     MOVE '- TOTAL   LIDOS         DO   CADASTRO -CLLP-'          
009850       TO TEXTO-DET.                                              
009860     MOVE AC-LIDOS-CAD TO CAMPO-DET.                              
009870     WRITE REG-IMP FROM DET1.                                     
009880                                                                  
009890     MOVE '0 TOTAL   DESPREZADOS   DO   CADASTRO -CLLP-'          
009900       TO TEXTO-DET.                                              
009910     MOVE AC-DESPREZADO TO CAMPO-DET.                             
009920     WRITE REG-IMP FROM DET1.                                     
009930                                                                  
009940     MOVE '0 TOTAL   SELECIONADOS  DO   CADASTRO -CLLP-'          
009950       TO TEXTO-DET.                                              
009960     MOVE AC-SELEC TO CAMPO-DET.                                  
009970     WRITE REG-IMP FROM DET1.                                     
009980                                                                  
009990 FIM-FINALIZA.                 EXIT.                              
010000                                                                  
010010*---------------------------------------------------------------* 
010020/                                                                 
010030*---------------------------------------------------------------* 
010040                                                                  
010050 CARGA-TAB SECTION.                                               
010060                                                                  
010070     PERFORM  LIMPAR-TAB                                          
010080     VARYING  IND FROM 1 BY 1                                     
010090     UNTIL    IND GREATER 11000 OR CARTEIRA-TAB(IND) EQUAL 'ZZZ'  
010100                                                                  
010110     MOVE    ZEROS TO IND                                         
010120                                                                  
010130     GO  TO  FIM-LER-CADPENDE.                                    
010140                                                                  
010150 LER-CADPENDE.                                                    
010160                                                                  
010170     READ CADPENDE                                                
010180       AT END                                                     
010190         MOVE 1 TO FIM-CADPENDE                                   
010200         GO TO FIM-CARGA-TAB.                                     
010210                                                                  
010220 FIM-LER-CADPENDE.                                                
010230                                                                  
010240     IF EMP-PE LESS EMP-CAD                                       
010250       GO TO LER-CADPENDE                                         
010260     ELSE                                                         
010270       IF EMP-PE GREATER EMP-CAD                                  
010280         GO TO FIM-CARGA-TAB.                                     
010290                                                                  
010300     MOVE  RESTR-PE  TO  SEPARA-CAMPO.                            
010310                                                                  
010320     IF    (CPO-1  IS  ALPHABETIC   AND                           
010330            CPO-2  IS  ALPHABETIC)                                
010340            NEXT  SENTENCE                                        
010350     ELSE                                                         
010360            GO   TO   LER-CADPENDE.                               
010370                                                                  
010380     IF    CPO-1   EQUAL  SPACES   OR                             
010390           CPO-2   EQUAL  SPACES                                  
010400           GO   TO   LER-CADPENDE.                                
010410                                                                  
010420     IF    RESTR-PE = 'ZZ'                                        
010430           NEXT SENTENCE                                          
010440     ELSE                                                         
010450        IF  CARTEIRA-PE   EQUAL  CARTEIRA-AUX                     
010460            NEXT SENTENCE                                         
010470        ELSE                                                      
010480            ADD 1 TO IND                                          
010490            MOVE CARTEIRA-PE  TO CARTEIRA-TAB  (IND)              
010500            MOVE RESTR-PE     TO RESTR-TAB (IND)                  
010510            MOVE CARTEIRA-PE  TO CARTEIRA-AUX.                    
010520                                                                  
010530     GO TO LER-CADPENDE.                                          
010540                                                                  
010550 FIM-CARGA-TAB.                                                   
010560     EXIT.                                                        
010570                                                                  
010580*---------------------------------------------------------------* 
010590                                                                  
010600 PESQUISA-TAB SECTION.                                            
010610                                                                  
BRQ=E******MOVE CARTEIRA-CAD  TO CARTEIRA-NUM                           
BRQ=I      MOVE CAD-CART2     TO CARTEIRA-ALFA                          
010630                                                                  
010640     SET IND-SEARCH TO 1                                          
010650     SEARCH      TAB-PEND                                         
010660       AT END  MOVE    1   TO CHAVE-INCONS                        
010670               GO TO   FIM-PESQUISA-TAB                           
010680       WHEN    CARTEIRA-TAB(IND-SEARCH) EQUAL CARTEIRA-ALFA       
010690               MOVE    RESTR-TAB(IND-SEARCH)  TO NATUREZA-SOR     
010700       WHEN    CARTEIRA-TAB(IND-SEARCH) EQUAL 'ZZZ'               
010710               MOVE    1   TO CHAVE-INCONS.                       
010720                                                                  
010730 FIM-PESQUISA-TAB.                                                
010740     EXIT.                                                        
010750                                                                  
010760*---------------------------------------------------------------* 
010770                                                                  
010780 LIMPAR-TAB  SECTION.                                             
010790                                                                  
010800     MOVE   'ZZZ'        TO CARTEIRA-TAB(IND)                     
010810     MOVE   SPACES       TO RESTR-TAB(IND).                       
010820                                                                  
010830 FIM-LIMPAR-TAB. EXIT.                                            
010840                                                                  
010850*---------------------------------------------------------------* 
010860                                                                  

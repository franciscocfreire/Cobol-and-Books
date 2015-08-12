000010*===============================================================* 
000020 IDENTIFICATION DIVISION.                                         
000030*===============================================================* 
000040                                                                  
000050 PROGRAM-ID. CLLP7610.                                            
000060 AUTHOR. DANIELA RAMOS DA SILVA.                                  
000070*===============================================================* 
000080*                   C P M   S I S T E M A S                     * 
000090*---------------------------------------------------------------* 
000100*                                                               * 
000110*      PROGRAMA     : CLLP7610                                  * 
000120*      PROGRAMADOR  : DANIELA R.     ( CPM )                    * 
000130*      ANALISTA     : LOURIVAL SANTI GRP. 82                    * 
000140*      DATA         : 14/10/1997                                * 
000150*                                                               * 
000160*      OBJETIVO     :                                           * 
000170*        GERAR CACALPCL (TAMANHO 600 BYTES COMPRIMIDO) A PARTIR * 
000180*        DO ARQDB2 (IMAGEM DO DB2 LPCLB998).                    * 
000190*     ** COPIA DO PROGRAMA LW7520.                              * 
000200*                                                               * 
000210*      ARQUIVOS:                                                * 
000220*         DDNAME                           INCLUDE/BOOK         * 
000230*         ARQDB2                             I#CLLPD1           * 
000240*         CADALPCL (COMPRIMIDO)              I#LPCLAJ           * 
000250*         TOTAIS                                                * 
000260*                                                               * 
000270*                                                               * 
000280*===============================================================* 
000290*===============================================================* 
000300*===============================================================* 
000310*             U L T I M A   A L T E R A C A O                   * 
000320*===============================================================* 
000330*                                                               * 
000340*      PROGRAMA........: CLLP7610                               * 
000350*      PROGRAMADORA....: LUCIANA DE J. BRONZERRE   - (CPM)      * 
000360*      SUPERVISORA.....: BARTIRA                   - (CPM)      * 
000370*      ANALISTA........: SERGIO                    - GP.97      * 
000380*      OBJETIVO........: INCLUSAO DO CAMPO AGENCIA PARA RESP    * 
000390*                                                               * 
000400*===============================================================* 
BRQCAR*             U L T I M A   A L T E R A C A O                   * 
BRQCAR*===============================================================* 
BRQCAR*                                                               * 
BRQCAR*      PROGRAMA........: CLLP7610                               * 
BRQCAR*      ANALISTA........: DINIZ - BRQ INFORMATICA                * 
BRQCAR*      OBJETIVO........: PROJETO CARTEIRA ALFA                  * 
BRQCAR*                                                               * 
000400*===============================================================* 
000410     EJECT                                                        
000420*===============================================================* 
000430 ENVIRONMENT DIVISION.                                            
000440*===============================================================* 
000450                                                                  
000460*---------------------------------------------------------------* 
000470 CONFIGURATION SECTION.                                           
000480*---------------------------------------------------------------* 
000490                                                                  
000500 SPECIAL-NAMES.                                                   
000510     DECIMAL-POINT IS COMMA.                                      
000520                                                                  
000530     EJECT                                                        
000540*---------------------------------------------------------------* 
000550 INPUT-OUTPUT SECTION.                                            
000560*---------------------------------------------------------------* 
000570                                                                  
000580 FILE-CONTROL.                                                    
000590                                                                  
000600     SELECT  ARQDB2  ASSIGN TO UT-S-ARQDB2                        
000610                FILE STATUS IS WRK-FS-ARQDB2.                     
000620                                                                  
000630     SELECT   TOTAIS ASSIGN TO UT-S-TOTAIS                        
000640                FILE STATUS IS WRK-FS-TOTAIS.                     
000650     EJECT                                                        
000660*===============================================================* 
000670 DATA DIVISION.                                                   
000680*===============================================================* 
000690                                                                  
000700*---------------------------------------------------------------* 
000710 FILE SECTION.                                                    
000720*---------------------------------------------------------------* 
000730                                                                  
000740                                                                  
000750*---------------------------------------------------------------* 
000760*    INPUT:     ARQQUIVO DE TABELA DB2                          * 
000770*               ORG. SEQUENCIAL   -   LRECL = 690               *
000780*---------------------------------------------------------------* 
000790                                                                  
000800 FD  ARQDB2                                                       
000810     RECORDING MODE IS F                                          
000820     LABEL RECORD IS STANDARD                                     
000830     BLOCK CONTAINS 0 RECORDS.                                    
000840                                                                  
-INC I#CLLPFD
000860                                                                  
000870     EJECT                                                        
000880*---------------------------------------------------------------* 
000890*    OUTPUT:    RELATORIO DE TOTAIS                             * 
000900*               ORG. SEQUENCIAL   -   LRECL = 080               * 
000910*---------------------------------------------------------------* 
000920                                                                  
000930 FD  TOTAIS                                                       
000940     RECORDING MODE IS F                                          
000950     LABEL RECORD IS STANDARD                                     
000960     BLOCK CONTAINS 0 RECORDS.                                    
000970                                                                  
000980 01  REG-TOTAIS                  PIC X(80).                       
000990                                                                  
001000     EJECT                                                        
001010*---------------------------------------------------------------* 
001020 WORKING-STORAGE SECTION.                                         
001030*---------------------------------------------------------------* 
001040                                                                  
001050 77  FILLER                      PIC X(32)        VALUE           
001060     '* INICIO DA WORKING CLLP7610 *'.                            
001070                                                                  
001080 77  WRK-ABEND                   PIC S9(04) COMP  VALUE +1111.    
001090 77  ACU-LIDOS-ARQDB2            PIC  9(08) COMP-3 VALUE ZEROS.   
001100 77  ACU-GRAVA-CADALPCL          PIC  9(08) COMP-3 VALUE ZEROS.   
001110 77  WRK-FIM-ARQDB2              PIC  X(01)        VALUE  'N'.    
001120 77  WRK-FINALIZA                PIC  X(01)        VALUE  'N'.    
001130 77  WRK-IND                     PIC  9(03)        VALUE ZEROS.   
001140 77  WRK-CADALPCL                PIC  X(08) VALUE 'CADALPCL'.     
001150 77  WRK-RETURN-CODE             PIC S9(05)        VALUE ZEROS.   
001160                                                                  
001170 77  WRK-FS-ARQDB2               PIC  X(02)        VALUE  SPACES. 
001180 77  WRK-FS-TOTAIS               PIC  X(02)        VALUE  SPACES. 
001190                                                                  
001200 77  WRK-OPERACAO                PIC  X(13) VALUE SPACES.         
001210 77  WRK-ABERTURA                PIC  X(13) VALUE 'NA ABERTURA'.  
001220 77  WRK-LEITURA                 PIC  X(13) VALUE 'NA LEITURA'.   
001230 77  WRK-GRAVACAO                PIC  X(13) VALUE 'NA GRAVACAO'.  
001240 77  WRK-FECHAMENTO              PIC  X(13) VALUE 'NO FECHAMENTO'.
001250                                                                  
001260 01  WRK-CARTEIRA                PIC  X(03).                      
001270 01  FILLER  REDEFINES WRK-CARTEIRA.                              
001280     03  WRK-CART                PIC  9(03).                      
001290                                                                  
001300 01  WRK-COD-NATUREZA-OPER       PIC  X(03).                      
001310 01  FILLER  REDEFINES WRK-COD-NATUREZA-OPER.                     
001320     03  WRK-NATUREZA            PIC  9(03).                      
001330                                                                  
001340 01  WRK-VCTO-INV                PIC  9(08).                      
001350 01  FILLER  REDEFINES  WRK-VCTO-INV.                             
001360     03  WRK-VCTO-INV-AAAA       PIC  9(04).                      
001370     03  WRK-VCTO-INV-MM         PIC  9(02).                      
001380     03  WRK-VCTO-INV-DD         PIC  9(02).                      
001390                                                                  
001400 01  WRK-TI-SYS                  PIC  9(06).                      
001410 01  FILLER  REDEFINES  WRK-TI-SYS.                               
001420     03  WRK-TI-HOR              PIC  9(02).                      
001430     03  WRK-TI-MIN              PIC  9(02).                      
001440     03  WRK-TI-SEG              PIC  9(02).                      
001450                                                                  
001460 01  WRK-AREA-POOL7600.                                           
001470     03  WRK-DT-JULIANA          PIC  9(05) COMP-3.               
001480     03  WRK-DT-AAMMDD           PIC  9(07) COMP-3.               
001490     03  WRK-DT-AAAAMMDD         PIC  9(09) COMP-3.               
001500     03  WRK-TI-HHMMSS           PIC  9(07) COMP-3.               
001510     03  WRK-TI-HHMMSSMMMMMM     PIC  9(13) COMP-3.               
001520     03  WRK-TIMESTAMP           PIC  X(20).                      
001530                                                                  
001540 01  AUX-DT-INV-AAAAMMDD         PIC  9(08).                      
001550 01  FILLER  REDEFINES  AUX-DT-INV-AAAAMMDD.                      
001560     03  AUX-DT-INV-AAAA         PIC  9(04).                      
001570     03  AUX-DT-INV-MM           PIC  9(02).                      
001580     03  AUX-DT-INV-DD           PIC  9(02).                      
001590                                                                  
001600 01  AUX-DT-DDMMAA               PIC  9(08).                      
001610 01  FILLER  REDEFINES  AUX-DT-DDMMAA.                            
001620     03  AUX-DT-DD               PIC  9(02).                      
001630     03  AUX-DT-MM               PIC  9(02).                      
001640     03  AUX-DT-AA               PIC  9(04).                      
001650                                                                  
001660 01  AUX-DT-DB2.                                                  
001670     03  AUX-DT-DB2-DD           PIC  9(02).                      
001680     03  FILLER                  PIC  X(01).                      
001690     03  AUX-DT-DB2-MM           PIC  9(02).                      
001700     03  FILLER                  PIC  X(01).                      
001710     03  AUX-DT-DB2-AAAA         PIC  9(04).                      
001720     03  FILLER REDEFINES AUX-DT-DB2-AAAA.                        
001730         05  AUX-DT-DB2-SEC      PIC  9(02).                      
001740         05  AUX-DT-DB2-ANO      PIC  9(02).                      
001750                                                                  
001760                                                                  
001770*---------------------------------------------------------------* 
001780*               AREA DE GRAVACAO DO ARQUIVO CADALPCL            * 
001790*---------------------------------------------------------------* 
       01  WKCADCL.
           03  CAD-EMP                 PIC  9(005)        COMP-3.
           03  CAD-AGEN                PIC  9(005)        COMP-3.
           03  CAD-NUMCL               PIC  9(015)        COMP-3.
           03  CAD-DIG                 PIC  X(001).
           03  CAD-CC                  PIC  9(007)        COMP-3.
           03  CAD-PEND                PIC  9(005)        COMP-3.
           03  CAD-SIGLA               PIC  X(004).
           03  CAD-CART                PIC  9(003)        COMP-3.
           03  CAD-CONTR               PIC  9(007)        COMP-3.
           03  CAD-CART2               PIC  X(003).
           03  CAD-RZCL                PIC  9(005)        COMP-3.
           03  CAD-CTCL                PIC  9(007)        COMP-3.
           03  CAD-RZ1CL               PIC  9(005)        COMP-3.
           03  CAD-CT1CL               PIC  9(007)        COMP-3.
           03  CAD-VCTO                PIC  9(009)        COMP-3.
           03  CAD-DTECL               PIC  9(009)        COMP-3.
           03  CAD-DTBCL               PIC  9(009)        COMP-3.
           03  CAD-DTIOP               PIC  9(009)        COMP-3.
           03  CAD-DTPRO               PIC  9(009)        COMP-3.
           03  CAD-DTELP               PIC  9(009)        COMP-3.
           03  CAD-DTBLP               PIC  9(009)        COMP-3.
           03  CAD-DTPGTO              PIC  9(009)        COMP-3.
           03  CAD-MOVTO               PIC  9(009)        COMP-3.
           03  CAD-DTREX               PIC  9(009)        COMP-3.
           03  CAD-DTVTR               PIC  9(009)        COMP-3.
           03  CAD-VRDVI               PIC  9(013)V9(002) COMP-3.
           03  CAD-VRCON               PIC  9(013)V9(002) COMP-3.
           03  CAD-VRBXA               PIC  9(013)V9(002) COMP-3.
           03  CAD-VBASE               PIC  9(013)V9(002) COMP-3.
           03  CAD-VRBBX               PIC  9(013)V9(002) COMP-3.
           03  CAD-COBRM               PIC  9(013)V9(002) COMP-3.
           03  CAD-COBRB               PIC  9(013)V9(002) COMP-3.
           03  CAD-DEBCC               PIC  9(013)V9(002) COMP-3.
           03  CAD-COBRD               PIC  9(013)V9(002) COMP-3.
           03  CAD-VREVE               PIC  9(013)V9(002) COMP-3.
           03  CAD-VREVI               PIC  9(013)V9(002) COMP-3.
           03  CAD-VRINI               PIC  9(013)V9(002) COMP-3.
           03  CAD-BXINI               PIC  9(013)V9(002) COMP-3.
           03  CAD-VRIOF               PIC  9(013)V9(002) COMP-3.
           03  CAD-BXIOF               PIC  9(013)V9(002) COMP-3.
           03  CAD-ANTEX               PIC  9(013)V9(002) COMP-3.
           03  CAD-MOEDA               PIC  X(002).
           03  CAD-MOEDA-ANT           PIC  X(001).
           03  CAD-VAGO                PIC  X(001).
           03  CAD-AGRESP              PIC  9(005)        COMP-3.
           03  CAD-NOME                PIC  X(040).
           03  CAD-CGC.
               05  CAD-NCGC            PIC  9(009)        COMP-3.
               05  CAD-FIL             PIC  9(005)        COMP-3.
               05  CAD-CTR             PIC  9(003)        COMP-3.
           03  CAD-NOME1               PIC  X(040).
           03  CAD-CGC1.
               05  CAD-NCGC1           PIC  9(009)        COMP-3.
               05  CAD-FIL1            PIC  9(005)        COMP-3.
               05  CAD-CTR1            PIC  9(003)        COMP-3.
           03  CAD-NOME2               PIC  X(040).
           03  CAD-CGC2.
               05  CAD-NCGC2           PIC  9(009)        COMP-3.
               05  CAD-FIL2            PIC  9(005)        COMP-3.
               05  CAD-CTR2            PIC  9(003)        COMP-3.
           03  CAD-TPGAR               PIC  X(002).
           03  CAD-NOGAR               PIC  X(032).
           03  CAD-STARET              PIC  X(001).
           03  CAD-TRAG                PIC  X(004).
           03  CAD-TRAG-AGEN           PIC  X(004).
           03  CAD-TRAG-DATA           PIC  9(009)        COMP-3.
           03  CAD-DATA-RETORNO        PIC  9(009)        COMP-3.
           03  CAD-DATA-TRANSLP        PIC  9(009)        COMP-3.
           03  CAD-ENC-TRANSF          PIC  9(013)V9(002) COMP-3.
           03  CAD-COMPL-TPO-BAIXA     PIC  X(001).
           03  CAD-ADVOG               PIC  9(011)        COMP-3.
           03  CAD-LOCAL               PIC  X(002).
           03  CAD-LOCA2               PIC  X(001).
           03  CAD-SUBSTAB             PIC  X(001).
           03  CAD-CDULT               PIC  9(003)        COMP-3.
           03  CAD-DTULT               PIC  9(009)        COMP-3.
           03  CAD-OCORRENCIAS.
               05  CAD-OCORR           PIC  9(003)        COMP-3
                                       OCCURS 50 TIMES.
           03  CAD-MARCA               PIC  X(001).
           03  CAD-IDCON               PIC  X(001).
           03  CAD-CARTA               PIC  X(001).
           03  CAD-TXPUN               PIC  X(001).
           03  CAD-AJUP                PIC  X(001).
           03  CAD-REGBX               PIC  X(001).
           03  FILLER                  PIC  X(006).
           03  CAD-PRECL               PIC  X(001).
           03  CAD-DTPRECL             PIC  9(009)        COMP-3.
           03  CAD-ORIGEMCL            PIC  9(002).
           03  CAD-TPEXPUR             PIC  X(001).
           03  CAD-NATUREZA            PIC  9(003)        COMP-3.
           03  CAD-IDENT               PIC  X(002).
           03  CAD-TIPBX               PIC  X(001).
           03  CAD-TIPO                PIC  9(001).
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
001810                                                                  
001820                                                                  
001830*---------------------------------------------------------------* 
001840*                 DEFINICAO DO RELATORIO DE TOTAIS              * 
001850*---------------------------------------------------------------* 
001860                                                                  
001870 01  CABEC1.                                                      
001880     03  CB1-CARRO               PIC X(01) VALUE '1'.             
001890     03  FILLER                  PIC X(28) VALUE 'CLLP7610'.      
001900     03  FILLER                  PIC X(37) VALUE                  
001910         'BANCO   BRADESCO  S.A.'.                                
001920     03  FILLER                  PIC X(7)   VALUE 'FOLHA:'.       
001930     03  CB1-PAG                 PIC ZZZ.ZZ9.                     
001940                                                                  
001950 01  CABEC2.                                                      
001960     03  CB2-CARRO               PIC X(01) VALUE '0'.             
001970     03  FILLER.                                                  
001980         05  CB2-DTSYS-DD        PIC 9(2)/.                       
001990         05  CB2-DTSYS-MM        PIC 9(2)/.                       
002000         05  CB2-DTSYS-AAAA      PIC 9(4).                        
002010     03  FILLER                  PIC X(07) VALUE SPACES.          
002020     03  FILLER                  PIC X(37) VALUE                  
002030         'RELATORIO DE TOTAIS                 '.                  
002040     03  FILLER                  PIC X(17) VALUE SPACES.          
002050     03  FILLER.                                                  
002060         05  CB2-TIME-HOR        PIC 9(2).                        
002070         05  FILLER              PIC X(1) VALUE ':'.              
002080         05  CB2-TIME-MIN        PIC 9(2).                        
002090         05  FILLER              PIC X(1) VALUE ':'.              
002100         05  CB2-TIME-SEG        PIC 9(2).                        
002110                                                                  
002120                                                                  
002130 01  LINSPACE.                                                    
002140     03  LP1-CARRO               PIC X(01) VALUE '-'.             
002150     03  FILLER                  PIC X(79) VALUE SPACES.          
002160                                                                  
002170                                                                  
002180 01  LINTOT1.                                                     
002190     03  LT1-CARRO               PIC X(01) VALUE '0'.             
002200     03  FILLER                  PIC X(17) VALUE SPACES.          
002210     03  FILLER                  PIC X(32) VALUE                  
002220         'REGISTROS LIDOS - ARQDB2      = '.                      
002230     03  LT1-LIDOS-ARQDB2        PIC ZZ.ZZZ.ZZ9.                  
002240                                                                  
002250                                                                  
002260 01  LINTOT2.                                                     
002270     03  LT2-CARRO               PIC X(01) VALUE '0'.             
002280     03  FILLER                  PIC X(17) VALUE SPACES.          
002290     03  FILLER                  PIC X(32) VALUE                  
002300         'REGISTROS GRAVADOS - CADALPCL = '.                      
002310     03  LT2-GRAVA-CADALPCL      PIC ZZ.ZZZ.ZZ9.                  
002320                                                                  
002330                                                                  
002340 01  FILLER                      PIC X(32)        VALUE           
002350     '*  FIM DA WORKING CLLP7610 *'.                              
002360     EJECT                                                        
002370*===============================================================* 
002380 PROCEDURE DIVISION.                                              
002390*===============================================================* 
002400                                                                  
002410*---------------------------------------------------------------* 
002420 0000-INICIAR SECTION.                                            
002430*---------------------------------------------------------------* 
002440                                                                  
002450     OPEN INPUT   ARQDB2                                          
002460          OUTPUT  TOTAIS.                                         
002470                                                                  
002480     MOVE    WRK-ABERTURA            TO   WRK-OPERACAO.           
002490     PERFORM 1000-TESTAR-FILE-STATUS.                             
002500                                                                  
002510     PERFORM 2000-INICIALIZA.                                     
002520                                                                  
002530     IF  WRK-FINALIZA EQUAL 'S'                                   
002540         GO  TO  0000-FINALIZAR.                                  
002550                                                                  
002560     PERFORM 3000-PROCESSA-ARQDB2 UNTIL                           
002570                                  WRK-FIM-ARQDB2 EQUAL 'S'.       
002580                                                                  
002590     PERFORM  4000-IMPRIME-TOTAIS.                                
002600                                                                  
002610 0000-FINALIZAR.                                                  
002620*---------------*                                                 
002630                                                                  
002640     IF  ACU-GRAVA-CADALPCL GREATER ZEROS                         
002650         CALL  'FECHAARQ' USING  WRK-CADALPCL.                    
002660                                                                  
002670     CLOSE  ARQDB2                                                
002680            TOTAIS.                                               
002690                                                                  
002700     MOVE    WRK-FECHAMENTO          TO   WRK-OPERACAO.           
002710     PERFORM 1000-TESTAR-FILE-STATUS.                             
002720                                                                  
002730     STOP RUN.                                                    
002740                                                                  
002750*---------------------------------------------------------------* 
002760 0000-99-FIM. EXIT.                                               
002770*---------------------------------------------------------------* 
002780     EJECT                                                        
002790*---------------------------------------------------------------* 
002800 1000-TESTAR-FILE-STATUS SECTION.                                 
002810*---------------------------------------------------------------* 
002820                                                                  
002830     PERFORM 1100-TESTAR-FS-ARQDB2.                               
002840                                                                  
002850     PERFORM 1200-TESTAR-FS-TOTAIS.                               
002860                                                                  
002870*---------------------------------------------------------------* 
002880 1000-99-FIM. EXIT.                                               
002890*---------------------------------------------------------------* 
002900     EJECT                                                        
002910*---------------------------------------------------------------* 
002920 1100-TESTAR-FS-ARQDB2  SECTION.                                  
002930*---------------------------------------------------------------* 
002940                                                                  
002950     IF WRK-FS-ARQDB2 NOT EQUAL '00'                              
002960        DISPLAY '************** CLLP7610 *************'           
002970        DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
002980        DISPLAY '*              ARQDB2               *'           
002990        DISPLAY '*         FILE STATUS =  ' WRK-FS-ARQDB2         
003000                                           '         *'           
003010        DISPLAY '************** CLLP7610 *************'           
003020        CALL 'ILBOABN0'     USING WRK-ABEND.                      
003030                                                                  
003040*---------------------------------------------------------------* 
003050 1100-99-FIM. EXIT.                                               
003060*---------------------------------------------------------------* 
003070     EJECT                                                        
003080*---------------------------------------------------------------* 
003090 1200-TESTAR-FS-TOTAIS   SECTION.                                 
003100*---------------------------------------------------------------* 
003110                                                                  
003120     IF WRK-FS-TOTAIS NOT EQUAL '00'                              
003130        DISPLAY '************** CLLP7610 *************'           
003140        DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
003150        DISPLAY '*              TOTAIS               *'           
003160        DISPLAY '*         FILE STATUS =  ' WRK-FS-TOTAIS         
003170                                           '         *'           
003180        DISPLAY '************** CLLP7610 *************'           
003190        CALL 'ILBOABN0'     USING WRK-ABEND.                      
003200                                                                  
003210*---------------------------------------------------------------* 
003220 1200-99-FIM. EXIT.                                               
003230*---------------------------------------------------------------* 
003240     EJECT                                                        
003250*---------------------------------------------------------------* 
003260 2000-INICIALIZA SECTION.                                         
003270*---------------------------------------------------------------* 
003280                                                                  
003290                                                                  
003300     CALL   'POOL7600' USING WRK-AREA-POOL7600.                   
003310                                                                  
003320     MOVE   WRK-DT-AAAAMMDD       TO  AUX-DT-INV-AAAAMMDD.        
003330     MOVE   AUX-DT-INV-DD         TO  CB2-DTSYS-DD.               
003340     MOVE   AUX-DT-INV-MM         TO  CB2-DTSYS-MM.               
003350     MOVE   AUX-DT-INV-AAAA       TO  CB2-DTSYS-AAAA.             
003360                                                                  
003370     MOVE   WRK-TI-HHMMSS         TO  WRK-TI-SYS.                 
003380     MOVE   WRK-TI-HOR            TO  CB2-TIME-HOR.               
003390     MOVE   WRK-TI-MIN            TO  CB2-TIME-MIN.               
003400     MOVE   WRK-TI-SEG            TO  CB2-TIME-SEG.               
003410                                                                  
003420     PERFORM 2100-LER-ARQDB2.                                     
003440                                                                  
003450*---------------------------------------------------------------* 
003460 2000-99-FIM. EXIT.                                               
003470*---------------------------------------------------------------* 
003480     EJECT                                                        
003490*---------------------------------------------------------------* 
003500 2100-LER-ARQDB2   SECTION.                                       
003510*---------------------------------------------------------------* 
003520                                                                  
003540     READ ARQDB2.                                                 
003550                                                                  
003560     IF WRK-FS-ARQDB2 EQUAL '10'                                  
003570        MOVE  'S'        TO WRK-FIM-ARQDB2                        
003580        GO  TO 2100-99-FIM.                                       
003590                                                                  
003600     MOVE WRK-LEITURA             TO  WRK-OPERACAO.               
003610     PERFORM 1100-TESTAR-FS-ARQDB2.                               
003620                                                                  
003630     ADD 1                        TO  ACU-LIDOS-ARQDB2.           
003640                                                                  
003650     MOVE  DB2-VCTO               TO  AUX-DT-DB2.                 
003660     MOVE  AUX-DT-DB2-DD          TO  WRK-VCTO-INV-DD             
003670     MOVE  AUX-DT-DB2-MM          TO  WRK-VCTO-INV-MM             
003680     MOVE  AUX-DT-DB2-AAAA        TO  WRK-VCTO-INV-AAAA.          
003690                                                                  
003700     IF  DB2-EMPRESA           EQUAL  5000                        
003710         GO  TO  2100-LER-ARQDB2                                  
           ELSE                                                         
003700         IF  DB2-ID        NOT EQUAL 'MO'                         
003710             GO  TO  2100-LER-ARQDB2.                             
003720                                                                  
003730*---------------------------------------------------------------* 
003740 2100-99-FIM. EXIT.                                               
003750*---------------------------------------------------------------* 
003760     EJECT                                                        
003770*---------------------------------------------------------------* 
003780 3000-PROCESSA-ARQDB2    SECTION.                                 
003790*---------------------------------------------------------------* 
003800                                                                  
003810                                                                  
003820     IF  DB2-ID EQUAL 'MO'                                        
003830         IF  DB2-COD-EMPR EQUAL 01                                
003840             MOVE  04120  TO  DB2-EMPRESA                         
003850         ELSE                                                     
003860             IF  DB2-COD-EMPR EQUAL 03                            
003870                 MOVE  05240  TO  DB2-EMPRESA                     
003880             ELSE                                                 
003890                 IF  DB2-COD-EMPR EQUAL 04                        
003900                     MOVE  05150  TO  DB2-EMPRESA                 
003910                 ELSE                                             
003920                     IF  DB2-COD-EMPR EQUAL 05                    
003930                         MOVE  06500  TO  DB2-EMPRESA             
003940                     ELSE                                         
003950                         IF  DB2-COD-EMPR EQUAL 06                
003960                             MOVE  04900  TO  DB2-EMPRESA.        
003970                                                                  
003980     MOVE  DB2-EMPRESA           TO  CAD-EMP.                     
003990     MOVE  DB2-AGENCIA           TO  CAD-AGEN.                    
004000     MOVE  DB2-NUM-CL            TO  CAD-NUMCL.                   
004010     MOVE  DB2-DIG               TO  CAD-DIG.                     
004020     MOVE  DB2-CON-CORR          TO  CAD-CC.                      
004030     MOVE  DB2-TIPO-PEND         TO  CAD-PEND.                    
004040     MOVE  DB2-SIGLA             TO  CAD-SIGLA.                   
004050     MOVE  DB2-CARTEIRA          TO  WRK-CARTEIRA.                
BRQCAR     IF    WRK-CART IS NUMERIC                                    
BRQCAR           MOVE  WRK-CART        TO  CAD-CART                     
BRQCAR     ELSE                                                         
BRQCAR           MOVE  ZEROS           TO  CAD-CART                     
BRQCAR     END-IF.                                                      
004070     MOVE  DB2-CONTRATO          TO  CAD-CONTR.                   
004080     MOVE  DB2-CARTEIRA-ORIGEM   TO  CAD-CART2.                   
004090     MOVE  ZEROS                 TO  CAD-RZCL                     
004100                                     CAD-CTCL                     
004110                                     CAD-RZ1CL                    
004120                                     CAD-CT1CL.                   
004130     MOVE  WRK-VCTO-INV-DD       TO  AUX-DT-DD.                   
004140     MOVE  WRK-VCTO-INV-MM       TO  AUX-DT-MM.                   
004150     MOVE  WRK-VCTO-INV-AAAA     TO  AUX-DT-AA.                   
004160     MOVE  AUX-DT-DDMMAA         TO  CAD-VCTO.                    
004170     MOVE  DB2-ENTRADAS          TO  AUX-DT-DB2.                  
004180     MOVE  AUX-DT-DB2-DD         TO  AUX-DT-DD.                   
004190     MOVE  AUX-DT-DB2-MM         TO  AUX-DT-MM.                   
004200     MOVE  AUX-DT-DB2-AAAA       TO  AUX-DT-AA.                   
004210     MOVE  AUX-DT-DDMMAA         TO  CAD-DTECL.                   
004220     MOVE  ZEROS                 TO  CAD-DTBCL                    
004230                                     CAD-DTIOP                    
004240                                     CAD-DTPRO                    
004250                                     CAD-DTELP                    
004260                                     CAD-DTBLP                    
004270                                     CAD-DTPGTO                   
004280                                     CAD-MOVTO                    
004290                                     CAD-DTREX                    
004300                                     CAD-DTVTR                    
004310                                     CAD-VRDVI.                   
004320     MOVE  DB2-PRINCIPAL         TO  CAD-VRCON.                   
004330     MOVE  ZEROS                 TO  CAD-VRBXA                    
004340                                     CAD-VBASE                    
004350                                     CAD-VRBBX                    
004360                                     CAD-COBRM                    
004370                                     CAD-COBRB                    
004380                                     CAD-DEBCC                    
004390                                     CAD-COBRD                    
                                           CAD-AGRESP.                  
004400     MOVE  DB2-VR-VENCIDOS       TO  CAD-VREVE.                   
004410     MOVE  DB2-VR-VINCENDOS      TO  CAD-VREVI.                   
004420     MOVE  DB2-PRINCIPAL         TO  CAD-VRINI.                   
004430     MOVE  ZEROS                 TO  CAD-BXINI                    
004440                                     CAD-VRIOF                    
004450                                     CAD-BXIOF                    
004460                                     CAD-ANTEX.                   
004470     MOVE  DB2-MOEDA             TO  CAD-MOEDA.                   
004480     MOVE  SPACES                TO  CAD-MOEDA-ANT.               
004500     MOVE  DB2-NOME-DEVEDOR      TO  CAD-NOME.                    
004510     MOVE  DB2-NUMERO-DEV        TO  CAD-NCGC.                    
004520     MOVE  DB2-FILIAL            TO  CAD-FIL.                     
004530     MOVE  DB2-CTR-DEV           TO  CAD-CTR.                     
004540     MOVE  DB2-NOME-AVALISTA     TO  CAD-NOME1.                   
004550     MOVE  DB2-NUMERO-AVAL       TO  CAD-NCGC1.                   
004560     MOVE  DB2-FILIAL-AVAL       TO  CAD-FIL1.                    
004570     MOVE  DB2-CTR-AVAL          TO  CAD-CTR1.                    
004580     MOVE  DB2-NOME-AVAL2        TO  CAD-NOME2.                   
004590     MOVE  DB2-NUMERO-AVAL2      TO  CAD-NCGC2.                   
004600     MOVE  DB2-FILIAL-AVAL2      TO  CAD-FIL2.                    
004610     MOVE  DB2-CTR-AVAL2         TO  CAD-CTR2.                    
004620     MOVE  DB2-TIPO-GAR          TO  CAD-TPGAR.                   
004630     MOVE  SPACES                TO  CAD-NOGAR.                   
004640     MOVE  DB2-STATUS-REATIVACAO TO  CAD-STARET.                  
004650     MOVE  SPACES                TO  CAD-TRAG                     
004660                                     CAD-TRAG-AGEN.               
004670     MOVE  ZEROS                 TO  CAD-TRAG-DATA.               
004680     MOVE  ZEROS                 TO  CAD-DATA-RETORNO             
004690                                     CAD-DATA-TRANSLP             
004700                                     CAD-ENC-TRANSF.              
004710     MOVE  SPACES                TO  CAD-VAGO.                    
004720     MOVE  ZEROS                 TO  CAD-ADVOG.                   
004730     MOVE  SPACES                TO  CAD-LOCAL.                   
004740     MOVE  SPACES                TO  CAD-LOCA2                    
004750                                     CAD-SUBSTAB.                 
004760     MOVE  DB2-CODIGO            TO  CAD-CDULT.                   
004770     MOVE  DB2-DATA-OCORR        TO  AUX-DT-DB2.                  
004780     MOVE  AUX-DT-DB2-DD         TO  AUX-DT-DD.                   
004790     MOVE  AUX-DT-DB2-MM         TO  AUX-DT-MM.                   
004800     MOVE  AUX-DT-DB2-AAAA       TO  AUX-DT-AA.                   
004810     MOVE  AUX-DT-DDMMAA         TO  CAD-DTULT.                   
004820                                                                  
004830     MOVE  1                     TO  WRK-IND.                     
004840     PERFORM 3100-MOVE-OCORRENCIAS UNTIL WRK-IND GREATER 50.      
004850                                                                  
004860     MOVE  DB2-MARCA-IMPE        TO  CAD-MARCA                    
004870                                     CAD-IDCON                    
004880                                     CAD-CARTA                    
004890                                     CAD-TXPUN                    
004900                                     CAD-AJUP                     
004910                                     CAD-REGBX.                   
004920     MOVE  DB2-MARCA-PDD-180     TO  CAD-PRECL.                   
004930     MOVE  DB2-DT-PDD-180        TO  AUX-DT-DB2.                  
004940     MOVE  AUX-DT-DB2-DD         TO  AUX-DT-DD.                   
004950     MOVE  AUX-DT-DB2-MM         TO  AUX-DT-MM.                   
004960     MOVE  AUX-DT-DB2-AAAA       TO  AUX-DT-AA.                   
004970     MOVE  AUX-DT-DDMMAA         TO  CAD-DTPRECL                  
004980     MOVE  ZEROS                 TO  CAD-ORIGEMCL.                
004990     MOVE  SPACES                TO  CAD-TPEXPUR.                 
005000     MOVE  DB2-COD-NATUREZA-OPER TO  WRK-COD-NATUREZA-OPER.       
005010     MOVE  WRK-NATUREZA          TO  CAD-NATUREZA.                
005020     MOVE  DB2-ID                TO  CAD-IDENT.                   
005030     MOVE  SPACES                TO  CAD-TIPBX                    
                                           CAD-COMPL-TPO-BAIXA.         
005040     MOVE  '1'                   TO  CAD-TIPO.

005050     DISPLAY 'TAXA CONTRATO: ' DB2-TAXA-CONTRATO
           MOVE  DB2-TAXA-CONTRATO     TO  CAD-TAXA-CONTRATO
           MOVE  DB2-VR-REMUNERATORIO  TO  CAD-VR-REMUNERATORIO
           MOVE  DB2-VALOR-MORATORIO   TO  CAD-VALOR-MORATORIO
           MOVE  DB2-VALOR-MULTA       TO  CAD-VALOR-MULTA
           MOVE  DB2-DESP-JUD-CUSTAS   TO  CAD-DESP-JUD-CUSTAS
           MOVE  DB2-HONORARIOS        TO  CAD-HONORARIOS
           MOVE  DB2-VL-TOTAL-DIVIDA   TO  CAD-VL-TOTAL-DIVIDA
           MOVE  DB2-VL-TAXA-TARIFA    TO  CAD-VL-TAXA-TARIFA

005060     CALL  'POOL0315' USING WRK-CADALPCL WKCADCL.                 
005070                                                                  
005080     ADD   1                     TO  ACU-GRAVA-CADALPCL.          
005090                                                                  
005100     PERFORM  2100-LER-ARQDB2.                                    
005110                                                                  
005120*---------------------------------------------------------------* 
005130 3000-99-FIM.  EXIT.                                              
005140*---------------------------------------------------------------* 
005150     EJECT                                                        
005160*---------------------------------------------------------------* 
005170 3100-MOVE-OCORRENCIAS SECTION.                                   
005180*---------------------------------------------------------------* 
005190                                                                  
005200     MOVE  DB2-OCORRENCIAS(WRK-IND) TO  CAD-OCORR(WRK-IND).       
005210                                                                  
005220     ADD   1   TO   WRK-IND.                                      
005230                                                                  
005240*---------------------------------------------------------------* 
005250 3100-99-FIM. EXIT.                                               
005260*---------------------------------------------------------------* 
005270     EJECT                                                        
005280*---------------------------------------------------------------* 
005290 4000-IMPRIME-TOTAIS SECTION.                                     
005300*---------------------------------------------------------------* 
005310                                                                  
005320     MOVE ACU-LIDOS-ARQDB2           TO   LT1-LIDOS-ARQDB2.       
005330     MOVE ACU-GRAVA-CADALPCL         TO   LT2-GRAVA-CADALPCL.     
005340     MOVE WRK-GRAVACAO               TO   WRK-OPERACAO.           
005350                                                                  
005360     MOVE 1                          TO   CB1-PAG.                
005370                                                                  
005380     WRITE REG-TOTAIS                FROM CABEC1.                 
005390     PERFORM 1200-TESTAR-FS-TOTAIS.                               
005400                                                                  
005410     WRITE REG-TOTAIS                FROM CABEC2.                 
005420     PERFORM 1200-TESTAR-FS-TOTAIS.                               
005430                                                                  
005440     WRITE REG-TOTAIS                FROM LINSPACE.               
005450     PERFORM 1200-TESTAR-FS-TOTAIS.                               
005460                                                                  
005470     WRITE REG-TOTAIS                FROM LINTOT1.                
005480     PERFORM 1200-TESTAR-FS-TOTAIS.                               
005490                                                                  
005500     WRITE REG-TOTAIS                FROM LINTOT2.                
005510     PERFORM 1200-TESTAR-FS-TOTAIS.                               
005520                                                                  
005530*---------------------------------------------------------------* 
005540 4000-99-FIM. EXIT.                                               
005550*---------------------------------------------------------------* 

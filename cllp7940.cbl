000010*===============================================================* 
000020 IDENTIFICATION DIVISION.                                         
000030*===============================================================* 
000040                                                                  
000050 PROGRAM-ID. CLLP7940.                                            
000060 AUTHOR. FABIO.                                                   
000070*===============================================================* 
000080*                   C P M   S I S T E M A S                     * 
000090*---------------------------------------------------------------* 
000100*                                                               * 
000110*      PROGRAMA     : CLLP7940                                  * 
000120*      PROGRAMADOR  : FABIO AIRES     -  C.P.M.                 * 
000130*      SUPERVISOR   : REINALDO        -  C.P.M.                 * 
000140*      ANALISTA     : HOMERO          -  GP 82                  * 
000150*      DATA         : NOVEMBRO/1997                             * 
000160*                                                               * 
000170*      OBJETIVO     :                                           * 
000180*        ATUALIZAR OCORRENCIAS E ADVOGADO                       * 
000190*                                                               * 
000200*      ARQUIVOS:                                                * 
000210*         DDNAME                           INCLUDE/BOOK         * 
000220*         CADACLLP (DESCOMPRIMIDO)           I#TTLPAJ           * 
000230*         ARQDB2                             I#TTLWAP           * 
000240*         CADCLLP  (COMPRIMIDO)                                 * 
000250*                                                               * 
000260*===============================================================* 
000270*                       A L T E R A C A O                       * 
000280*---------------------------------------------------------------* 
000290*    PROGRAMADORA:   PATRICIA GUILHERMINO      - CPM            * 
000300*    ANALISTA....:   PATRICIA GUILHERMINO      - GRUPO 82       * 
000310*    DATA........:   28/02/2002                                 * 
000320*---------------------------------------------------------------* 
000330*    OBJETIVO....:   ATUALIZAR AVALISTAS 1 E 2, TENDO COMO BASE * 
000340*                    ARQUIVO VINDO DA DIARIA DO SISTEMA DCIR.   * 
      *                                                               * 
      *===============================================================* 
      *                       A L T E R A C A O                       * 
      *---------------------------------------------------------------* 
      *    PROGRAMADOR.:   MAICON BARBIERI          - CPM PATO BRANCO * 
      *    ANALISTA CPM:   KHARUZO INOCENCIO LEITE  - CPM PATO BRANCO * 
      *    ANALISTA....:   LUIZ AFONSO              - AMS / GP. 70    * 
      *    DATA........:   04/04/2005                                 * 
      *---------------------------------------------------------------* 
      *    OBJETIVO....:   ALTERAR O TAMANHO DO CAMPO CAD-REGISTRO DE * 
      *                    600 BYTES PARA 668 BYTES E O NOME DA INC   * 
      *                    DE I#CLLPGB PARA I#CLLP11.                 * 
000350***************************************************************** 
000360     EJECT                                                        
000370*===============================================================* 
000380 ENVIRONMENT DIVISION.                                            
000390*===============================================================* 
000400                                                                  
000410*---------------------------------------------------------------* 
000420 CONFIGURATION SECTION.                                           
000430*---------------------------------------------------------------* 
000440                                                                  
000450 SPECIAL-NAMES.                                                   
000460     DECIMAL-POINT IS COMMA.                                      
000470                                                                  
000480     EJECT                                                        
000490*---------------------------------------------------------------* 
000500 INPUT-OUTPUT SECTION.                                            
000510*---------------------------------------------------------------* 
000520                                                                  
000530 FILE-CONTROL.                                                    
000540                                                                  
000550     SELECT  CADACLLP ASSIGN TO UT-S-CADACLLP                     
000560                FILE STATUS IS WRK-FS-CADACLLP.                   
000570                                                                  
000580     SELECT  ARQDB2  ASSIGN TO UT-S-ARQDB2                        
000590                FILE STATUS IS WRK-FS-ARQDB2.                     
000600                                                                  
000610     EJECT                                                        
000620*===============================================================* 
000630 DATA DIVISION.                                                   
000640*===============================================================* 
000650                                                                  
000660*---------------------------------------------------------------* 
000670 FILE SECTION.                                                    
000680*---------------------------------------------------------------* 
000690                                                                  
000700 FD  CADACLLP                                                     
000710     RECORDING MODE IS F                                          
000720     LABEL RECORD IS STANDARD                                     
000730     BLOCK CONTAINS 0 RECORDS.                                    
000740                                                                  
000760 COPY 'I#CLLPRU'.

000770 FD  ARQDB2                                                       
000780     RECORDING MODE IS F                                          
000790     LABEL RECORD IS STANDARD                                     
000800     BLOCK CONTAINS 0 RECORDS.                                    
000810
       COPY 'I#CLLPIJ'.
000830                                                                  
000840     EJECT                                                        
000850*---------------------------------------------------------------* 
000860 WORKING-STORAGE SECTION.                                         
000870*---------------------------------------------------------------* 
000880                                                                  
000890 77  FILLER                      PIC X(32)        VALUE           
000900     '* INICIO DA WORKING-STORAGE  *'.                            
000910                                                                  
000920 77  WRK-ABEND                   PIC S9(04) COMP  VALUE +1111.    
000930                                                                  
000940 77  WRK-FS-CADACLLP             PIC X(02)        VALUE  SPACES.  
000950 77  WRK-FS-ARQDB2               PIC X(02)        VALUE  SPACES.  
000960                                                                  
000970 77  WRK-OPERACAO                PIC X(13) VALUE  SPACES.         
000980 77  WRK-ABERTURA                PIC X(13) VALUE  'NA ABERTURA'.  
000990 77  WRK-LEITURA                 PIC X(13) VALUE  'NA LEITURA'.   
001000 77  WRK-GRAVACAO                PIC X(13) VALUE  'NA GRAVACAO'.  
001010 77  WRK-FECHAMENTO              PIC X(13) VALUE  'NO FECHAMENTO'.
001020 77  WRK-SAIDA                   PIC X(08) VALUE  'CADCLLP'.      
001030                                                                  
       01  CAD-REGISTRO                PIC X(768).
001050                                                                  
001060 01  WRK-CAD-CHAVE.                                               
001070     03  WRK-CAD-EMPRESA         PIC 9(05) VALUE   ZEROS.         
001080     03  WRK-CAD-AGENCIA         PIC 9(05) VALUE   ZEROS.         
001090*    03  WRK-CAD-NUMCL           PIC 9(12) VALUE   ZEROS.         
001100     03  WRK-CAD-CONTR           PIC 9(07) VALUE   ZEROS.         
001110                                                                  
001120 01  WRK-DB2-CHAVE.                                               
001130     03  WRK-DB2-EMPRESA         PIC 9(05) VALUE   ZEROS.         
001140     03  WRK-DB2-AGENCIA         PIC 9(05) VALUE   ZEROS.         
001150*    03  WRK-DB2-NUMCL           PIC 9(12) VALUE   ZEROS.         
001160     03  WRK-DB2-CONTRATO        PIC 9(07) VALUE   ZEROS.         
001170                                                                  
001180*01  WRK-NUMERO-CL               PIC 9(15).                       
001190*01  FILLER REDEFINES WRK-NUMERO-CL.                              
001200*    03  WRK-NUMCL               PIC 9(12).                       
001210*    03  WRK-PARCL               PIC 9(03).                       
001220                                                                  
001230 01  WRK-DB2-DATA-OCORR.                                          
001240     03  WRK-DB2-DD-OCORR        PIC 9(02).                       
001250     03  FILLER                  PIC X(01).                       
001260     03  WRK-DB2-MM-OCORR        PIC 9(02).                       
001270     03  FILLER                  PIC X(01).                       
001280     03  WRK-DB2-AA-OCORR        PIC 9(04).                       
001290                                                                  
001300 01  WRK-CAD-DATA-OCORR          PIC 9(08) VALUE   ZEROS.         
001310 01  FILLER       REDEFINES      WRK-CAD-DATA-OCORR.              
001320     03  WRK-CAD-DD-OCORR        PIC 9(02).                       
001330     03  WRK-CAD-MM-OCORR        PIC 9(02).                       
001340     03  WRK-CAD-AA-OCORR        PIC 9(04).                       
001350                                                                  
001360 01  WRK-CAD-ADVOG               PIC 9(11) VALUE   ZEROS.         
001370 01  FILLER       REDEFINES      WRK-CAD-ADVOG.                   
001380     03  WRK-DB2-NUMERO-ADV      PIC 9(09).                       
001390     03  WRK-DB2-CTR             PIC 9(02).                       
001400                                                                  
001410 01  FILLER                      PIC X(32)        VALUE           
001420     '*  FIM DA WORKING-STORAGE  *'.                              
001430     EJECT                                                        
001440*===============================================================* 
001450 PROCEDURE DIVISION.                                              
001460*===============================================================* 
001470                                                                  
001480*---------------------------------------------------------------* 
001490 0000-INICIAR SECTION.                                            
001500*---------------------------------------------------------------* 
001510                                                                  
001520     OPEN INPUT   CADACLLP                                        
001530                  ARQDB2.                                         
001540                                                                  
001550     MOVE    WRK-ABERTURA     TO     WRK-OPERACAO.                
001560     PERFORM 1000-TESTAR-FILE-STATUS.                             
001570                                                                  
001580     PERFORM 2000-LER-CADACLLP.                                   
001590                                                                  
001600     PERFORM 3000-LER-ARQDB2.                                     
001610                                                                  
001620     PERFORM 4000-PROCESSAR-REGISTRO UNTIL                        
001630                                  WRK-CAD-CHAVE = HIGH-VALUES.    
001640                                                                  
001650     CLOSE  CADACLLP                                              
001660            ARQDB2.                                               
001670     CALL   'FECHAARQ'               USING   WRK-SAIDA.           
001680                                                                  
001690     MOVE    WRK-FECHAMENTO          TO   WRK-OPERACAO.           
001700     PERFORM 1000-TESTAR-FILE-STATUS.                             
001710                                                                  
001720     STOP RUN.                                                    
001730                                                                  
001740*---------------------------------------------------------------* 
001750 0000-99-FIM. EXIT.                                               
001760*---------------------------------------------------------------* 
001770     EJECT                                                        
001780*---------------------------------------------------------------* 
001790 1000-TESTAR-FILE-STATUS SECTION.                                 
001800*---------------------------------------------------------------* 
001810                                                                  
001820     PERFORM 1100-TESTAR-FS-CADACLLP.                             
001830                                                                  
001840     PERFORM 1200-TESTAR-FS-ARQDB2.                               
001850                                                                  
001860*---------------------------------------------------------------* 
001870 1000-99-FIM. EXIT.                                               
001880*---------------------------------------------------------------* 
001890     EJECT                                                        
001900*---------------------------------------------------------------* 
001910 1100-TESTAR-FS-CADACLLP SECTION.                                 
001920*---------------------------------------------------------------* 
001930                                                                  
001940     IF WRK-FS-CADACLLP NOT EQUAL '00'                            
001950        DISPLAY '************** CLLP7940 *************'           
001960        DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
001970        DISPLAY '*              CADACLLP             *'           
001980        DISPLAY '*         FILE STATUS =  ' WRK-FS-CADACLLP       
001990                                           '         *'           
002000        DISPLAY '************** CLLP7940 *************'           
002010        CALL 'ILBOABN0'     USING WRK-ABEND.                      
002020                                                                  
002030*---------------------------------------------------------------* 
002040 1100-99-FIM. EXIT.                                               
002050*---------------------------------------------------------------* 
002060     EJECT                                                        
002070*---------------------------------------------------------------* 
002080 1200-TESTAR-FS-ARQDB2   SECTION.                                 
002090*---------------------------------------------------------------* 
002100                                                                  
002110     IF WRK-FS-ARQDB2   NOT EQUAL '00'                            
002120        DISPLAY '************** CLLP7940 *************'           
002130        DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
002140        DISPLAY '*              ARQDB2               *'           
002150        DISPLAY '*         FILE STATUS =  ' WRK-FS-ARQDB2         
002160                                           '         *'           
002170        DISPLAY '************** CLLP7940 *************'           
002180        CALL 'ILBOABN0'     USING WRK-ABEND.                      
002190                                                                  
002200*---------------------------------------------------------------* 
002210 1200-99-FIM. EXIT.                                               
002220*---------------------------------------------------------------* 
002230     EJECT                                                        
002240*---------------------------------------------------------------* 
002250 2000-LER-CADACLLP    SECTION.                                    
002260*---------------------------------------------------------------* 
002270                                                                  
002280     READ CADACLLP.                                               
002290                                                                  
002300     IF WRK-FS-CADACLLP EQUAL '10'                                
002310        MOVE   HIGH-VALUES     TO     WRK-CAD-CHAVE               
002320        GO  TO 2000-99-FIM.                                       
002330                                                                  
002340     MOVE WRK-LEITURA                TO WRK-OPERACAO.             
002350     PERFORM 1100-TESTAR-FS-CADACLLP.                             
002360                                                                  
002370     MOVE    CAD-EMP     TO     WRK-CAD-EMPRESA.                  
002380     MOVE    CAD-AGEN    TO     WRK-CAD-AGENCIA.                  
002390*    MOVE    CAD-NUMCL   TO     WRK-NUMERO-CL.                    
002400*    MOVE    WRK-NUMCL   TO     WRK-CAD-NUMCL.                    
002410     MOVE    CAD-CONTR   TO     WRK-CAD-CONTR.                    
002420                                                                  
002430*---------------------------------------------------------------* 
002440 2000-99-FIM. EXIT.                                               
002450*---------------------------------------------------------------* 
002460     EJECT                                                        
002470*---------------------------------------------------------------* 
002480 3000-LER-ARQDB2   SECTION.                                       
002490*---------------------------------------------------------------* 
002500                                                                  
002510     READ ARQDB2.                                                 
002520                                                                  
002530     IF WRK-FS-ARQDB2   EQUAL '10'                                
002540        MOVE   HIGH-VALUES     TO     WRK-DB2-CHAVE               
002550        GO  TO 3000-99-FIM.                                       
002560                                                                  
002570     MOVE WRK-LEITURA                TO WRK-OPERACAO.             
002580     PERFORM 1200-TESTAR-FS-ARQDB2.                               
002590                                                                  
002600     MOVE    DB2-EMPRESA TO     WRK-DB2-EMPRESA.                  
002610     MOVE    DB2-AGENCIA TO     WRK-DB2-AGENCIA.                  
002620*    MOVE    DB2-NUM-CL  TO     WRK-NUMERO-CL.                    
002630*    MOVE    WRK-NUMCL   TO     WRK-DB2-NUMCL.                    
002640     MOVE    DB2-CONTRATO TO    WRK-DB2-CONTRATO.                 
002650                                                                  
002660*---------------------------------------------------------------* 
002670 3000-99-FIM. EXIT.                                               
002680*---------------------------------------------------------------* 
002690                                                                  
002700                                                                  
002710*---------------------------------------------------------------* 
002720 4000-PROCESSAR-REGISTRO  SECTION.                                
002730*---------------------------------------------------------------* 
002740                                                                  
002750     IF    WRK-CAD-CHAVE              <     WRK-DB2-CHAVE         
002760           MOVE    WKCADCL            TO    CAD-REGISTRO          
002770           CALL    'POOL0315'      USING    WRK-SAIDA             
002780                                            CAD-REGISTRO          
002790           PERFORM 2000-LER-CADACLLP                              
002800     ELSE                                                         
002810     IF    WRK-CAD-CHAVE              =     WRK-DB2-CHAVE         
002820           MOVE    DB2-OCORR          TO    CAD-OCORRENCIAS       
002830           MOVE    DB2-CODIGO         TO    CAD-CDULT             
002840           MOVE    DB2-DATA-OCORR     TO    WRK-DB2-DATA-OCORR    
002850           MOVE    WRK-DB2-DD-OCORR   TO    WRK-CAD-DD-OCORR      
002860           MOVE    WRK-DB2-MM-OCORR   TO    WRK-CAD-MM-OCORR      
002870           MOVE    WRK-DB2-AA-OCORR   TO    WRK-CAD-AA-OCORR      
002880           MOVE    WRK-CAD-DATA-OCORR TO    CAD-DTULT             
002890           MOVE    DB2-NUMERO-ADV     TO    WRK-DB2-NUMERO-ADV    
002900           MOVE    DB2-CTR            TO    WRK-DB2-CTR           
002910           MOVE    WRK-CAD-ADVOG      TO    CAD-ADVOG             
002920           MOVE    DB2-NOME-AVALISTA  TO    CAD-NOME1             
002930           MOVE    DB2-NUMERO-AVAL    TO    CAD-NCGC1             
002940           MOVE    DB2-FILIAL-AVAL    TO    CAD-FIL1              
002950           MOVE    DB2-CTR-AVAL       TO    CAD-CTR1              
002960           MOVE    DB2-NOME-AVAL2     TO    CAD-NOME2             
002970           MOVE    DB2-NUMERO-AVAL2   TO    CAD-NCGC2             
002980           MOVE    DB2-FILIAL-AVAL2   TO    CAD-FIL2              
002990           MOVE    DB2-CTR-AVAL2      TO    CAD-CTR2              
003000           MOVE    WKCADCL            TO    CAD-REGISTRO          
003010           CALL    'POOL0315'      USING    WRK-SAIDA             
003020                                            CAD-REGISTRO          
003030           PERFORM 2000-LER-CADACLLP                              
003040     ELSE                                                         
003050           PERFORM 3000-LER-ARQDB2.                               
003060                                                                  
003070*---------------------------------------------------------------* 
003080 4000-99-FIM. EXIT.                                               
003090*---------------------------------------------------------------* 
003100     EJECT                                                        

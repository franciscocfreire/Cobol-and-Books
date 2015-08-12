000010*===============================================================* 
000020 IDENTIFICATION DIVISION.                                         
000030*===============================================================* 
000040                                                                  
000050 PROGRAM-ID. CLLP7920.                                            
000060 AUTHOR. FABIO.                                                   
000070*===============================================================* 
000080*                   C P M   S I S T E M A S                     * 
000090*---------------------------------------------------------------* 
000100*                                                               * 
000110*      PROGRAMA     : CLLP7920                                  * 
000120*      PROGRAMADOR  : FABIO AIRES    -  C.P.M.                  * 
000130*      SUPERVISOR   : REINALDO       -  C.P.M.                  * 
000140*      ANALISTA     : HOMERO         -  GP 82                   * 
000150*      DATA         : NOVEMBRO/1997                             * 
000160*                                                               * 
000170*      OBJETIVO     :                                           * 
000180*        GRAVAR CADCLLP COM DT.VCTO INVERTIDA                   * 
000190*                                                               * 
000200*      ARQUIVOS:                                                * 
000210*         DDNAME                           INCLUDE/BOOK         * 
000220*         CADACLLP (COMPRIMIDO)              I#TTLPAJ           * 
000230*         CADCLLP  (DESCOMPRIMIDO)                              * 
000240*                                                               * 
000250*                                                               * 
000260*===============================================================* 
      *                     A L T E R A C A O                         * 
      *---------------------------------------------------------------* 
      *                                                               * 
      *    PROGRAMADORA : MIRELLI VENTURA             -  CPM/FPOLIS   * 
      *    ANALISTA CPM : MICHELLI VENTURA BORGES     -  CPM/FPOLIS   * 
      *    ANALISTA     : PORFIRIO S. MACHADO - AMS   -  GRUPO 70     * 
      *    DATA         : 23/03/2005                                  * 
      *                                                               * 
      *    OBJETIVO     :                                             * 
      *      SUBSTITUIR A INCLUDE I#CLLPGB PELA I#CLLP11 E EFETUAR    * 
      *      AJUSTES NAS MOVIMENTACOES DOS CAMPOS SE NECESSARIO.      * 
      *                                                               * 
      *===============================================================* 
000270     EJECT                                                        
000280*===============================================================* 
000290 ENVIRONMENT DIVISION.                                            
000300*===============================================================* 
000310                                                                  
000320*---------------------------------------------------------------* 
000330 CONFIGURATION SECTION.                                           
000340*---------------------------------------------------------------* 
000350                                                                  
000360 SPECIAL-NAMES.                                                   
000370     DECIMAL-POINT IS COMMA.                                      
000380                                                                  
000390     EJECT                                                        
000400*---------------------------------------------------------------* 
000410 INPUT-OUTPUT SECTION.                                            
000420*---------------------------------------------------------------* 
000430                                                                  
000440 FILE-CONTROL.                                                    
000450                                                                  
000460     SELECT CADCLLP  ASSIGN TO UT-S-CADCLLP                       
000470                FILE STATUS IS WRK-FS-CADCLLP.                    
000480                                                                  
000490     EJECT                                                        
000500*===============================================================* 
000510 DATA DIVISION.                                                   
000520*===============================================================* 
000530                                                                  
000540*---------------------------------------------------------------* 
000550 FILE SECTION.                                                    
000560*---------------------------------------------------------------* 
000570                                                                  
000580 FD  CADCLLP                                                      
000590     RECORDING MODE IS F                                          
000600     LABEL RECORD IS STANDARD                                     
000610     BLOCK CONTAINS 0 RECORDS.                                    
000620                                                                  
000630 01  CAD-REGISTRO                PIC X(768).
000640                                                                  
000650     EJECT                                                        
000660*---------------------------------------------------------------* 
000670 WORKING-STORAGE SECTION.                                         
000680*---------------------------------------------------------------* 
000690                                                                  
000700 77  FILLER                      PIC X(32)        VALUE           
000710     '* INICIO DA WORKING-STORAGE  *'.                            
000720                                                                  
000730 77  WRK-ABEND                   PIC S9(04) COMP  VALUE +1111.    
000740                                                                  
000750 77  WRK-FS-CADACLLP             PIC X(02)        VALUE  SPACES.  
000760 77  WRK-FS-CADCLLP              PIC X(02)        VALUE  SPACES.  
000770                                                                  
000780 77  WRK-OPERACAO                PIC X(13) VALUE  SPACES.         
000790 77  WRK-ABERTURA                PIC X(13) VALUE  'NA ABERTURA'.  
000800 77  WRK-LEITURA                 PIC X(13) VALUE  'NA LEITURA'.   
000810 77  WRK-GRAVACAO                PIC X(13) VALUE  'NA GRAVACAO'.  
000820 77  WRK-FECHAMENTO              PIC X(13) VALUE  'NO FECHAMENTO'.
000830 77  WRK-ENTRADA                 PIC X(08) VALUE  'CADACLLP'.     
000840                                                                  
000850 01  WRK-DT-VCTO                 PIC 9(08) VALUE  ZEROS.          
000860 01  FILLER          REDEFINES   WRK-DT-VCTO.                     
000870     03  WRK-DD-VCTO             PIC 9(02).                       
000880     03  WRK-MM-VCTO             PIC 9(02).                       
000890     03  WRK-AA-VCTO             PIC 9(04).                       
000900                                                                  
000910 01  WRK-DT-VCTO-INV             PIC 9(08) VALUE  ZEROS.          
000920 01  FILLER          REDEFINES   WRK-DT-VCTO-INV.                 
000930     03  WRK-AA-VCTO-INV         PIC 9(04).                       
000940     03  WRK-MM-VCTO-INV         PIC 9(02).                       
000950     03  WRK-DD-VCTO-INV         PIC 9(02).                       
000960                                                                  
       COPY 'I#CLLPRU'.
000980                                                                  
000990     EJECT                                                        
001000*===============================================================* 
001010 PROCEDURE DIVISION.                                              
001020*===============================================================* 
001030                                                                  
001040*---------------------------------------------------------------* 
001050 0000-INICIAR SECTION.                                            
001060*---------------------------------------------------------------* 
001070                                                                  
001080     OPEN OUTPUT  CADCLLP.                                        
001090                                                                  
001100     MOVE    WRK-ABERTURA            TO   WRK-OPERACAO.           
001110     PERFORM 1000-TESTAR-FILE-STATUS.                             
001120                                                                  
001130     PERFORM 2000-LER-CADACLLP.                                   
001140                                                                  
001150     PERFORM 3000-PROCESSAR-REGISTRO  UNTIL                       
001160                                      WRK-FS-CADACLLP = '10'.     
001170                                                                  
001180     CLOSE  CADCLLP.                                              
001190                                                                  
001200     MOVE    WRK-FECHAMENTO          TO   WRK-OPERACAO.           
001210     PERFORM 1000-TESTAR-FILE-STATUS.                             
001220                                                                  
001230     STOP RUN.                                                    
001240                                                                  
001250*---------------------------------------------------------------* 
001260 0000-99-FIM. EXIT.                                               
001270*---------------------------------------------------------------* 
001280     EJECT                                                        
001290*---------------------------------------------------------------* 
001300 1000-TESTAR-FILE-STATUS SECTION.                                 
001310*---------------------------------------------------------------* 
001320                                                                  
001330     PERFORM 1200-TESTAR-FS-CADCLLP.                              
001340                                                                  
001350*---------------------------------------------------------------* 
001360 1000-99-FIM. EXIT.                                               
001370*---------------------------------------------------------------* 
001380     EJECT                                                        
001390*---------------------------------------------------------------* 
001400 1200-TESTAR-FS-CADCLLP  SECTION.                                 
001410*---------------------------------------------------------------* 
001420                                                                  
001430     IF WRK-FS-CADCLLP  NOT EQUAL '00'                            
001440        DISPLAY '************** CLLP7920 *************'           
001450        DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
001460        DISPLAY '*              CADCLLP              *'           
001470        DISPLAY '*         FILE STATUS =  ' WRK-FS-CADCLLP        
001480                                           '         *'           
001490        DISPLAY '************** CLLP7920 *************'           
001500        CALL 'ILBOABN0'     USING WRK-ABEND.                      
001510                                                                  
001520*---------------------------------------------------------------* 
001530 1200-99-FIM. EXIT.                                               
001540*---------------------------------------------------------------* 
001550     EJECT                                                        
001560*---------------------------------------------------------------* 
001570 2000-LER-CADACLLP    SECTION.                                    
001580*---------------------------------------------------------------* 
001590                                                                  
001600     CALL 'POOL0300'              USING WRK-ENTRADA WKCADCL.      
001610                                                                  
001620     IF   RETURN-CODE             EQUAL 4                         
001630          MOVE '10'                  TO WRK-FS-CADACLLP.          
001640                                                                  
001650*---------------------------------------------------------------* 
001660 2000-99-FIM. EXIT.                                               
001670*---------------------------------------------------------------* 
001680     EJECT                                                        
001690*---------------------------------------------------------------* 
001700 3000-PROCESSAR-REGISTRO  SECTION.                                
001710*---------------------------------------------------------------* 
001720                                                                  
001730     MOVE     CAD-VCTO          TO        WRK-DT-VCTO.            
001740     MOVE     WRK-DD-VCTO       TO        WRK-DD-VCTO-INV.        
001750     MOVE     WRK-MM-VCTO       TO        WRK-MM-VCTO-INV.        
001760     MOVE     WRK-AA-VCTO       TO        WRK-AA-VCTO-INV.        
001770     MOVE     WRK-DT-VCTO-INV   TO        CAD-DTPRECL.            
001780     MOVE     WKCADCL           TO        CAD-REGISTRO.           
001790     WRITE    CAD-REGISTRO.                                       
001800     MOVE     WRK-GRAVACAO      TO        WRK-OPERACAO.           
001810     PERFORM  1200-TESTAR-FS-CADCLLP.                             
001820                                                                  
001830     PERFORM  2000-LER-CADACLLP.                                  
001840                                                                  
001850*---------------------------------------------------------------* 
001860 3000-99-FIM. EXIT.                                               
001870*---------------------------------------------------------------* 
001880                                                                  

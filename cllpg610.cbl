      *===============================================================* 
       IDENTIFICATION DIVISION.                                         
      *===============================================================* 
                                                                        
       PROGRAM-ID. CLLPG610.                                            
       AUTHOR. DANIELA RAMOS DA SILVA.                                  
      *===============================================================* 
      *                   C P M   S I S T E M A S                     * 
      *---------------------------------------------------------------* 
      *                                                               * 
      *      PROGRAMA     : CLLPG610                                  * 
      *      PROGRAMADOR  : DANIELA R.     ( CPM )                    * 
      *      ANALISTA     : LOURIVAL SANTI GRP. 82                    * 
      *      DATA         : 14/10/1997                                * 
      *                                                               * 
      *      OBJETIVO     :                                           * 
      *        GERAR CACALPCL (TAMANHO 600 BYTES COMPRIMIDO) A PARTIR * 
      *        DO ARQDB2 (IMAGEM DO DB2 LPCLB998).                    * 
      *     ** COPIA DO PROGRAMA LW7520.                              * 
      *                                                               * 
      *      ARQUIVOS:                                                * 
      *         DDNAME                           INCLUDE/BOOK         * 
      *         ARQDB2                             I#CLLPD1           * 
      *         CADALPCL (COMPRIMIDO)              I#LPCLAJ           * 
      *         TOTAIS                                                * 
      *                                                               * 
      *                                                               * 
      *===============================================================* 
      *===============================================================* 
      *===============================================================* 
      *             U L T I M A   A L T E R A C A O                   * 
      *===============================================================* 
      *                                                               * 
      *      PROGRAMA........: CLLPG610                               * 
      *      PROGRAMADORA....: LUCIANA DE J. BRONZERRE   - (CPM)      * 
      *      SUPERVISORA.....: BARTIRA                   - (CPM)      * 
      *      ANALISTA........: SERGIO                    - GP.97      * 
      *      OBJETIVO........: INCLUSAO DO CAMPO AGENCIA PARA RESP    * 
      *                                                               * 
      *===============================================================* 
      *             U L T I M A   A L T E R A C A O                   * 
      *===============================================================* 
      *                                                               * 
      *      PROGRAMA........: CLLPG610                               * 
      *      ANALISTA........: DINIZ - BRQ INFORMATICA                * 
      *      OBJETIVO........: PROJETO CARTEIRA ALFA                  * 
      *                                                               * 
      *===============================================================* 
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
                                                                        
           SELECT  ARQDB2  ASSIGN TO UT-S-ARQDB2                        
                      FILE STATUS IS WRK-FS-ARQDB2.                     
                                                                        
           SELECT   TOTAIS ASSIGN TO UT-S-TOTAIS                        
                      FILE STATUS IS WRK-FS-TOTAIS.                     
      *===============================================================* 
       DATA DIVISION.                                                   
      *===============================================================* 
                                                                        
      *---------------------------------------------------------------* 
       FILE SECTION.                                                    
      *---------------------------------------------------------------* 
                                                                        
                                                                        
      *---------------------------------------------------------------* 
      *    INPUT:     ARQQUIVO DE TABELA DB2                          * 
      *               ORG. SEQUENCIAL   -   LRECL = 690               * 
      *---------------------------------------------------------------* 
                                                                        
       FD  ARQDB2                                                       
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
           COPY 'I#CLLPFD'.                                             
                                                                        
      *---------------------------------------------------------------* 
      *    OUTPUT:    RELATORIO DE TOTAIS                             * 
      *               ORG. SEQUENCIAL   -   LRECL = 080               * 
      *---------------------------------------------------------------* 
                                                                        
       FD  TOTAIS                                                       
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  REG-TOTAIS                  PIC X(80).                       
                                                                        
      *---------------------------------------------------------------* 
       WORKING-STORAGE SECTION.                                         
      *---------------------------------------------------------------* 
                                                                        
       77  FILLER                      PIC X(32)        VALUE           
           '* INICIO DA WORKING CLLPG610 *'.                            
                                                                        
       77  WRK-ABEND                   PIC S9(04) COMP  VALUE +1111.    
       77  ACU-LIDOS-ARQDB2            PIC  9(08) COMP-3 VALUE ZEROS.   
       77  ACU-GRAVA-CADALPCL          PIC  9(08) COMP-3 VALUE ZEROS.   
       77  WRK-FIM-ARQDB2              PIC  X(01)        VALUE  'N'.    
       77  WRK-FINALIZA                PIC  X(01)        VALUE  'N'.    
       77  WRK-IND                     PIC  9(03)        VALUE ZEROS.   
       77  WRK-CADALPCL                PIC  X(08) VALUE 'CADALPCL'.     
       77  WRK-RETURN-CODE             PIC S9(05)        VALUE ZEROS.   
                                                                        
       77  WRK-FS-ARQDB2               PIC  X(02)        VALUE  SPACES. 
       77  WRK-FS-TOTAIS               PIC  X(02)        VALUE  SPACES. 
                                                                        
       77  WRK-OPERACAO                PIC  X(13) VALUE SPACES.         
       77  WRK-ABERTURA                PIC  X(13) VALUE 'NA ABERTURA'.  
       77  WRK-LEITURA                 PIC  X(13) VALUE 'NA LEITURA'.   
       77  WRK-GRAVACAO                PIC  X(13) VALUE 'NA GRAVACAO'.  
       77  WRK-FECHAMENTO              PIC  X(13) VALUE 'NO FECHAMENTO'.
                                                                        
       01  WRK-CARTEIRA                PIC  X(03).                      
       01  FILLER  REDEFINES WRK-CARTEIRA.                              
           03  WRK-CART                PIC  9(03).                      
                                                                        
       01  WRK-COD-NATUREZA-OPER       PIC  X(03).                      
       01  FILLER  REDEFINES WRK-COD-NATUREZA-OPER.                     
           03  WRK-NATUREZA            PIC  9(03).                      
                                                                        
       01  WRK-VCTO-INV                PIC  9(08).                      
       01  FILLER  REDEFINES  WRK-VCTO-INV.                             
           03  WRK-VCTO-INV-AAAA       PIC  9(04).                      
           03  WRK-VCTO-INV-MM         PIC  9(02).                      
           03  WRK-VCTO-INV-DD         PIC  9(02).                      
                                                                        
       01  WRK-TI-SYS                  PIC  9(06).                      
       01  FILLER  REDEFINES  WRK-TI-SYS.                               
           03  WRK-TI-HOR              PIC  9(02).                      
           03  WRK-TI-MIN              PIC  9(02).                      
           03  WRK-TI-SEG              PIC  9(02).                      
                                                                        
       01  WRK-AREA-POOL7600.                                           
           03  WRK-DT-JULIANA          PIC  9(05) COMP-3.               
           03  WRK-DT-AAMMDD           PIC  9(07) COMP-3.               
           03  WRK-DT-AAAAMMDD         PIC  9(09) COMP-3.               
           03  WRK-TI-HHMMSS           PIC  9(07) COMP-3.               
           03  WRK-TI-HHMMSSMMMMMM     PIC  9(13) COMP-3.               
           03  WRK-TIMESTAMP           PIC  X(20).                      
                                                                        
       01  AUX-DT-INV-AAAAMMDD         PIC  9(08).                      
       01  FILLER  REDEFINES  AUX-DT-INV-AAAAMMDD.                      
           03  AUX-DT-INV-AAAA         PIC  9(04).                      
           03  AUX-DT-INV-MM           PIC  9(02).                      
           03  AUX-DT-INV-DD           PIC  9(02).                      
                                                                        
       01  AUX-DT-DDMMAA               PIC  9(08).                      
       01  FILLER  REDEFINES  AUX-DT-DDMMAA.                            
           03  AUX-DT-DD               PIC  9(02).                      
           03  AUX-DT-MM               PIC  9(02).                      
           03  AUX-DT-AA               PIC  9(04).                      
                                                                        
       01  AUX-DT-DB2.                                                  
           03  AUX-DT-DB2-DD           PIC  9(02).                      
           03  FILLER                  PIC  X(01).                      
           03  AUX-DT-DB2-MM           PIC  9(02).                      
           03  FILLER                  PIC  X(01).                      
           03  AUX-DT-DB2-AAAA         PIC  9(04).                      
           03  FILLER REDEFINES AUX-DT-DB2-AAAA.                        
               05  AUX-DT-DB2-SEC      PIC  9(02).                      
               05  AUX-DT-DB2-ANO      PIC  9(02).                      
                                                                        
                                                                        
      *---------------------------------------------------------------* 
      *               AREA DE GRAVACAO DO ARQUIVO CADALPCL            * 
      *---------------------------------------------------------------* 
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
                                                                        
                                                                        
      *---------------------------------------------------------------* 
      *                 DEFINICAO DO RELATORIO DE TOTAIS              * 
      *---------------------------------------------------------------* 
                                                                        
       01  CABEC1.                                                      
           03  CB1-CARRO               PIC X(01) VALUE '1'.             
           03  FILLER                  PIC X(28) VALUE 'CLLPG610'.      
           03  FILLER                  PIC X(37) VALUE                  
               'BANCO   BRADESCO  S.A.'.                                
           03  FILLER                  PIC X(7)   VALUE 'FOLHA:'.       
           03  CB1-PAG                 PIC ZZZ.ZZ9.                     
                                                                        
       01  CABEC2.                                                      
           03  CB2-CARRO               PIC X(01) VALUE '0'.             
           03  FILLER.                                                  
               05  CB2-DTSYS-DD        PIC 9(2)/.                       
               05  CB2-DTSYS-MM        PIC 9(2)/.                       
               05  CB2-DTSYS-AAAA      PIC 9(4).                        
           03  FILLER                  PIC X(07) VALUE SPACES.          
           03  FILLER                  PIC X(37) VALUE                  
               'RELATORIO DE TOTAIS                 '.                  
           03  FILLER                  PIC X(17) VALUE SPACES.          
           03  FILLER.                                                  
               05  CB2-TIME-HOR        PIC 9(2).                        
               05  FILLER              PIC X(1) VALUE ':'.              
               05  CB2-TIME-MIN        PIC 9(2).                        
               05  FILLER              PIC X(1) VALUE ':'.              
               05  CB2-TIME-SEG        PIC 9(2).                        
                                                                        
                                                                        
       01  LINSPACE.                                                    
           03  LP1-CARRO               PIC X(01) VALUE '-'.             
           03  FILLER                  PIC X(79) VALUE SPACES.          
                                                                        
                                                                        
       01  LINTOT1.                                                     
           03  LT1-CARRO               PIC X(01) VALUE '0'.             
           03  FILLER                  PIC X(17) VALUE SPACES.          
           03  FILLER                  PIC X(32) VALUE                  
               'REGISTROS LIDOS - ARQDB2      = '.                      
           03  LT1-LIDOS-ARQDB2        PIC ZZ.ZZZ.ZZ9.                  
                                                                        
                                                                        
       01  LINTOT2.                                                     
           03  LT2-CARRO               PIC X(01) VALUE '0'.             
           03  FILLER                  PIC X(17) VALUE SPACES.          
           03  FILLER                  PIC X(32) VALUE                  
               'REGISTROS GRAVADOS - CADALPCL = '.                      
           03  LT2-GRAVA-CADALPCL      PIC ZZ.ZZZ.ZZ9.                  
                                                                        
                                                                        
       01  FILLER                      PIC X(32)        VALUE           
           '*  FIM DA WORKING CLLPG610 *'.                              
      *===============================================================* 
       PROCEDURE DIVISION.                                              
      *===============================================================* 
                                                                        
      *---------------------------------------------------------------* 
       0000-INICIAR SECTION.                                            
      *---------------------------------------------------------------* 
                                                                        
           OPEN INPUT   ARQDB2                                          
                OUTPUT  TOTAIS.                                         
                                                                        
           MOVE    WRK-ABERTURA            TO   WRK-OPERACAO.           
           PERFORM 1000-TESTAR-FILE-STATUS.                             
                                                                        
           PERFORM 2000-INICIALIZA.                                     
                                                                        
           IF  WRK-FINALIZA EQUAL 'S'                                   
               GO  TO  0000-FINALIZAR.                                  
                                                                        
           PERFORM 3000-PROCESSA-ARQDB2 UNTIL                           
                                        WRK-FIM-ARQDB2 EQUAL 'S'.       
                                                                        
           PERFORM  4000-IMPRIME-TOTAIS.                                
                                                                        
       0000-FINALIZAR.                                                  
      *---------------*                                                 
                                                                        
           IF  ACU-GRAVA-CADALPCL GREATER ZEROS                         
               CALL  'FECHAARQ' USING  WRK-CADALPCL.                    
                                                                        
           CLOSE  ARQDB2                                                
                  TOTAIS.                                               
                                                                        
           MOVE    WRK-FECHAMENTO          TO   WRK-OPERACAO.           
           PERFORM 1000-TESTAR-FILE-STATUS.                             
                                                                        
           STOP RUN.                                                    
                                                                        
      *---------------------------------------------------------------* 
       0000-99-FIM. EXIT.                                               
      *---------------------------------------------------------------* 
      *---------------------------------------------------------------* 
       1000-TESTAR-FILE-STATUS SECTION.                                 
      *---------------------------------------------------------------* 
                                                                        
           PERFORM 1100-TESTAR-FS-ARQDB2.                               
                                                                        
           PERFORM 1200-TESTAR-FS-TOTAIS.                               
                                                                        
      *---------------------------------------------------------------* 
       1000-99-FIM. EXIT.                                               
      *---------------------------------------------------------------* 
      *---------------------------------------------------------------* 
       1100-TESTAR-FS-ARQDB2  SECTION.                                  
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-FS-ARQDB2 NOT EQUAL '00'                              
              DISPLAY '************** CLLPG610 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              ARQDB2               *'           
              DISPLAY '*         FILE STATUS =  ' WRK-FS-ARQDB2         
                                                 '         *'           
              DISPLAY '************** CLLPG610 *************'           
              CALL 'ILBOABN0'     USING WRK-ABEND.                      
                                                                        
      *---------------------------------------------------------------* 
       1100-99-FIM. EXIT.                                               
      *---------------------------------------------------------------* 
      *---------------------------------------------------------------* 
       1200-TESTAR-FS-TOTAIS   SECTION.                                 
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-FS-TOTAIS NOT EQUAL '00'                              
              DISPLAY '************** CLLPG610 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              TOTAIS               *'           
              DISPLAY '*         FILE STATUS =  ' WRK-FS-TOTAIS         
                                                 '         *'           
              DISPLAY '************** CLLPG610 *************'           
              CALL 'ILBOABN0'     USING WRK-ABEND.                      
                                                                        
      *---------------------------------------------------------------* 
       1200-99-FIM. EXIT.                                               
      *---------------------------------------------------------------* 
      *---------------------------------------------------------------* 
       2000-INICIALIZA SECTION.                                         
      *---------------------------------------------------------------* 
                                                                        
                                                                        
           CALL   'POOL7600' USING WRK-AREA-POOL7600.                   
                                                                        
           MOVE   WRK-DT-AAAAMMDD       TO  AUX-DT-INV-AAAAMMDD.        
           MOVE   AUX-DT-INV-DD         TO  CB2-DTSYS-DD.               
           MOVE   AUX-DT-INV-MM         TO  CB2-DTSYS-MM.               
           MOVE   AUX-DT-INV-AAAA       TO  CB2-DTSYS-AAAA.             
                                                                        
           MOVE   WRK-TI-HHMMSS         TO  WRK-TI-SYS.                 
           MOVE   WRK-TI-HOR            TO  CB2-TIME-HOR.               
           MOVE   WRK-TI-MIN            TO  CB2-TIME-MIN.               
           MOVE   WRK-TI-SEG            TO  CB2-TIME-SEG.               
                                                                        
           PERFORM 2100-LER-ARQDB2.                                     
                                                                        
      *---------------------------------------------------------------* 
       2000-99-FIM. EXIT.                                               
      *---------------------------------------------------------------* 
      *---------------------------------------------------------------* 
       2100-LER-ARQDB2   SECTION.                                       
      *---------------------------------------------------------------* 
                                                                        
           READ ARQDB2.                                                 
                                                                        
           IF WRK-FS-ARQDB2 EQUAL '10'                                  
              MOVE  'S'        TO WRK-FIM-ARQDB2                        
              GO  TO 2100-99-FIM.                                       
                                                                        
           MOVE WRK-LEITURA             TO  WRK-OPERACAO.               
           PERFORM 1100-TESTAR-FS-ARQDB2.                               
                                                                        
           ADD 1                        TO  ACU-LIDOS-ARQDB2.           
                                                                        
           MOVE  DB2-VCTO               TO  AUX-DT-DB2.                 
           MOVE  AUX-DT-DB2-DD          TO  WRK-VCTO-INV-DD             
           MOVE  AUX-DT-DB2-MM          TO  WRK-VCTO-INV-MM             
           MOVE  AUX-DT-DB2-AAAA        TO  WRK-VCTO-INV-AAAA.          
                                                                        
           IF  DB2-EMPRESA           EQUAL  5000                        
               GO  TO  2100-LER-ARQDB2                                  
           ELSE                                                         
               IF  DB2-ID        NOT EQUAL 'MO'                         
                   GO  TO  2100-LER-ARQDB2.                             
                                                                        
      *---------------------------------------------------------------* 
       2100-99-FIM. EXIT.                                               
      *---------------------------------------------------------------* 
      *---------------------------------------------------------------* 
       3000-PROCESSA-ARQDB2    SECTION.                                 
      *---------------------------------------------------------------* 
                                                                        
                                                                        
           IF  DB2-ID EQUAL 'MO'                                        
               IF  DB2-COD-EMPR EQUAL 01                                
                   MOVE  04120  TO  DB2-EMPRESA                         
               ELSE                                                     
                   IF  DB2-COD-EMPR EQUAL 03                            
                       MOVE  05240  TO  DB2-EMPRESA                     
                   ELSE                                                 
                       IF  DB2-COD-EMPR EQUAL 04                        
                           MOVE  05150  TO  DB2-EMPRESA                 
                       ELSE                                             
                           IF  DB2-COD-EMPR EQUAL 05                    
                               MOVE  06500  TO  DB2-EMPRESA             
                           ELSE                                         
                               IF  DB2-COD-EMPR EQUAL 06                
                                   MOVE  04900  TO  DB2-EMPRESA.        
                                                                        
           MOVE  DB2-EMPRESA           TO  CAD-EMP.                     
           MOVE  DB2-AGENCIA           TO  CAD-AGEN.                    
           MOVE  DB2-NUM-CL            TO  CAD-NUMCL.                   
           MOVE  DB2-DIG               TO  CAD-DIG.                     
           MOVE  DB2-CON-CORR          TO  CAD-CC.                      
           MOVE  DB2-TIPO-PEND         TO  CAD-PEND.                    
           MOVE  DB2-SIGLA             TO  CAD-SIGLA.                   
           MOVE  DB2-CARTEIRA          TO  WRK-CARTEIRA.                
           IF    WRK-CART IS NUMERIC                                    
                 MOVE  WRK-CART        TO  CAD-CART                     
           ELSE                                                         
                 MOVE  ZEROS           TO  CAD-CART                     
           END-IF.                                                      
           MOVE  DB2-CONTRATO          TO  CAD-CONTR.                   
           MOVE  DB2-CARTEIRA-ORIGEM   TO  CAD-CART2.                   
           MOVE  ZEROS                 TO  CAD-RZCL                     
                                           CAD-CTCL                     
                                           CAD-RZ1CL                    
                                           CAD-CT1CL.                   
           MOVE  WRK-VCTO-INV-DD       TO  AUX-DT-DD.                   
           MOVE  WRK-VCTO-INV-MM       TO  AUX-DT-MM.                   
           MOVE  WRK-VCTO-INV-AAAA     TO  AUX-DT-AA.                   
           MOVE  AUX-DT-DDMMAA         TO  CAD-VCTO.                    
           MOVE  DB2-ENTRADAS          TO  AUX-DT-DB2.                  
           MOVE  AUX-DT-DB2-DD         TO  AUX-DT-DD.                   
           MOVE  AUX-DT-DB2-MM         TO  AUX-DT-MM.                   
           MOVE  AUX-DT-DB2-AAAA       TO  AUX-DT-AA.                   
           MOVE  AUX-DT-DDMMAA         TO  CAD-DTECL.                   
           MOVE  ZEROS                 TO  CAD-DTBCL                    
                                           CAD-DTIOP                    
                                           CAD-DTPRO                    
                                           CAD-DTELP                    
                                           CAD-DTBLP                    
                                           CAD-DTPGTO                   
                                           CAD-MOVTO                    
                                           CAD-DTREX                    
                                           CAD-DTVTR                    
                                           CAD-VRDVI.                   
           MOVE  DB2-PRINCIPAL         TO  CAD-VRCON.                   
           MOVE  ZEROS                 TO  CAD-VRBXA                    
                                           CAD-VBASE                    
                                           CAD-VRBBX                    
                                           CAD-COBRM                    
                                           CAD-COBRB                    
                                           CAD-DEBCC                    
                                           CAD-COBRD                    
                                           CAD-AGRESP.                  
           MOVE  DB2-VR-VENCIDOS       TO  CAD-VREVE.                   
           MOVE  DB2-VR-VINCENDOS      TO  CAD-VREVI.                   
           MOVE  DB2-PRINCIPAL         TO  CAD-VRINI.                   
           MOVE  ZEROS                 TO  CAD-BXINI                    
                                           CAD-VRIOF                    
                                           CAD-BXIOF                    
                                           CAD-ANTEX.                   
           MOVE  DB2-MOEDA             TO  CAD-MOEDA.                   
           MOVE  SPACES                TO  CAD-MOEDA-ANT.               
           MOVE  DB2-NOME-DEVEDOR      TO  CAD-NOME.                    
           MOVE  DB2-NUMERO-DEV        TO  CAD-NCGC.                    
           MOVE  DB2-FILIAL            TO  CAD-FIL.                     
           MOVE  DB2-CTR-DEV           TO  CAD-CTR.                     
           MOVE  DB2-NOME-AVALISTA     TO  CAD-NOME1.                   
           MOVE  DB2-NUMERO-AVAL       TO  CAD-NCGC1.                   
           MOVE  DB2-FILIAL-AVAL       TO  CAD-FIL1.                    
           MOVE  DB2-CTR-AVAL          TO  CAD-CTR1.                    
           MOVE  DB2-NOME-AVAL2        TO  CAD-NOME2.                   
           MOVE  DB2-NUMERO-AVAL2      TO  CAD-NCGC2.                   
           MOVE  DB2-FILIAL-AVAL2      TO  CAD-FIL2.                    
           MOVE  DB2-CTR-AVAL2         TO  CAD-CTR2.                    
           MOVE  DB2-TIPO-GAR          TO  CAD-TPGAR.                   
           MOVE  SPACES                TO  CAD-NOGAR.                   
           MOVE  DB2-STATUS-REATIVACAO TO  CAD-STARET.                  
           MOVE  SPACES                TO  CAD-TRAG                     
                                           CAD-TRAG-AGEN.               
           MOVE  ZEROS                 TO  CAD-TRAG-DATA.               
           MOVE  ZEROS                 TO  CAD-DATA-RETORNO             
                                           CAD-DATA-TRANSLP             
                                           CAD-ENC-TRANSF.              
           MOVE  SPACES                TO  CAD-VAGO.                    
           MOVE  ZEROS                 TO  CAD-ADVOG.                   
           MOVE  SPACES                TO  CAD-LOCAL.                   
           MOVE  SPACES                TO  CAD-LOCA2                    
                                           CAD-SUBSTAB.                 
           MOVE  DB2-CODIGO            TO  CAD-CDULT.                   
           MOVE  DB2-DATA-OCORR        TO  AUX-DT-DB2.                  
           MOVE  AUX-DT-DB2-DD         TO  AUX-DT-DD.                   
           MOVE  AUX-DT-DB2-MM         TO  AUX-DT-MM.                   
           MOVE  AUX-DT-DB2-AAAA       TO  AUX-DT-AA.                   
           MOVE  AUX-DT-DDMMAA         TO  CAD-DTULT.                   
                                                                        
           MOVE  1                     TO  WRK-IND.                     
           PERFORM 3100-MOVE-OCORRENCIAS UNTIL WRK-IND GREATER 50.      
                                                                        
           MOVE  DB2-MARCA-IMPE        TO  CAD-MARCA                    
                                           CAD-IDCON                    
                                           CAD-CARTA                    
                                           CAD-TXPUN                    
                                           CAD-AJUP                     
                                           CAD-REGBX.                   
           MOVE  DB2-MARCA-PDD-180     TO  CAD-PRECL.                   
           MOVE  DB2-DT-PDD-180        TO  AUX-DT-DB2.                  
           MOVE  AUX-DT-DB2-DD         TO  AUX-DT-DD.                   
           MOVE  AUX-DT-DB2-MM         TO  AUX-DT-MM.                   
           MOVE  AUX-DT-DB2-AAAA       TO  AUX-DT-AA.                   
           MOVE  AUX-DT-DDMMAA         TO  CAD-DTPRECL                  
           MOVE  ZEROS                 TO  CAD-ORIGEMCL.                
           MOVE  SPACES                TO  CAD-TPEXPUR.                 
           MOVE  DB2-COD-NATUREZA-OPER TO  WRK-COD-NATUREZA-OPER.       
           MOVE  WRK-NATUREZA          TO  CAD-NATUREZA.                
           MOVE  DB2-ID                TO  CAD-IDENT.                   
           MOVE  SPACES                TO  CAD-TIPBX                    
                                           CAD-COMPL-TPO-BAIXA.         
           MOVE  '1'                   TO  CAD-TIPO.                    
                                                                        
           MOVE  DB2-TAXA-CONTRATO     TO  CAD-TAXA-CONTRATO            
           MOVE  DB2-VR-REMUNERATORIO  TO  CAD-VR-REMUNERATORIO         
           MOVE  DB2-VALOR-MORATORIO   TO  CAD-VALOR-MORATORIO          
           MOVE  DB2-VALOR-MULTA       TO  CAD-VALOR-MULTA              
           MOVE  DB2-DESP-JUD-CUSTAS   TO  CAD-DESP-JUD-CUSTAS          
           MOVE  DB2-HONORARIOS        TO  CAD-HONORARIOS
           MOVE  DB2-VL-TAXA-TARIFA    TO  CAD-VL-TAXA-TARIFA

      *    VERIFICAR SE OS CAMPOS DA TRANSPARENCIA ESTAO PREENCHIDOS
      *    CASO NAO ESTIVEREM O TOTAL DA DIVIDA SERA O VALOR DE COBRANCA

           IF      DB2-VR-REMUNERATORIO EQUAL ZEROS
               AND DB2-VALOR-MORATORIO  EQUAL ZEROS
               AND DB2-VALOR-MULTA      EQUAL ZEROS
               AND DB2-DESP-JUD-CUSTAS  EQUAL ZEROS
               AND DB2-HONORARIOS       EQUAL ZEROS
               AND DB2-VL-TAXA-TARIFA   EQUAL ZEROS

               MOVE  DB2-COBRANCA          TO  CAD-VL-TOTAL-DIVIDA
           ELSE
               MOVE  DB2-VL-TOTAL-DIVIDA   TO  CAD-VL-TOTAL-DIVIDA
           END-IF
                                                                        
           CALL  'POOL0315' USING WRK-CADALPCL WKCADCL.                 
                                                                        
           ADD   1                     TO  ACU-GRAVA-CADALPCL.          
                                                                        
           PERFORM  2100-LER-ARQDB2.                                    
                                                                        
      *---------------------------------------------------------------* 
       3000-99-FIM.  EXIT.                                              
      *---------------------------------------------------------------*

      *---------------------------------------------------------------* 
       3100-MOVE-OCORRENCIAS SECTION.                                   
      *---------------------------------------------------------------* 
                                                                        
           MOVE  DB2-OCORRENCIAS(WRK-IND) TO  CAD-OCORR(WRK-IND).       
                                                                        
           ADD   1   TO   WRK-IND.                                      
                                                                        
      *---------------------------------------------------------------* 
       3100-99-FIM. EXIT.                                               
      *---------------------------------------------------------------* 
      *---------------------------------------------------------------* 
       4000-IMPRIME-TOTAIS SECTION.                                     
      *---------------------------------------------------------------* 
                                                                        
           MOVE ACU-LIDOS-ARQDB2           TO   LT1-LIDOS-ARQDB2.       
           MOVE ACU-GRAVA-CADALPCL         TO   LT2-GRAVA-CADALPCL.     
           MOVE WRK-GRAVACAO               TO   WRK-OPERACAO.           
                                                                        
           MOVE 1                          TO   CB1-PAG.                
                                                                        
           WRITE REG-TOTAIS                FROM CABEC1.                 
           PERFORM 1200-TESTAR-FS-TOTAIS.                               
                                                                        
           WRITE REG-TOTAIS                FROM CABEC2.                 
           PERFORM 1200-TESTAR-FS-TOTAIS.                               
                                                                        
           WRITE REG-TOTAIS                FROM LINSPACE.               
           PERFORM 1200-TESTAR-FS-TOTAIS.                               
                                                                        
           WRITE REG-TOTAIS                FROM LINTOT1.                
           PERFORM 1200-TESTAR-FS-TOTAIS.                               
                                                                        
           WRITE REG-TOTAIS                FROM LINTOT2.                
           PERFORM 1200-TESTAR-FS-TOTAIS.                               
                                                                        
      *---------------------------------------------------------------* 
       4000-99-FIM. EXIT.                                               
      *---------------------------------------------------------------* 

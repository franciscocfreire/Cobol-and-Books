      *===============================================================* 
       IDENTIFICATION DIVISION.                                         
      *===============================================================* 
                                                                        
       PROGRAM-ID. CLLP7623.                                            
       AUTHOR. GILBERTO.                                                
      *===============================================================* 
      *                   C P M   S I S T E M A S                     * 
      *---------------------------------------------------------------* 
      *                                                               * 
      *   PROGRAMA...: CLLP7623.                                      * 
      *   ANALISTA...: GILBERTO - CPM.                                * 
      *   DATA.......: JANEIRO/2005.                                  * 
      *                                                               * 
      *   OBJETIVO...: CRIAR O ARQUIVO CADLPCL PARA EMISSAO DE        * 
      *                AVISO DE COBRANCA PARA OS AVALISTAS.           * 
      *                                                               * 
      *      ARQUIVOS:                                                * 
      *         DDNAME                           INCLUDE/BOOK         * 
      *         ARQLPCL                            I#CLLPLC           * 
      *         CADLPCL                            I#CLLPLE           * 
      *         TOTAIS                                                * 
      *                                                               * 
      *===============================================================* 
CPMMRS*                        A L T E R A C A O                      * 
CPMMRS*---------------------------------------------------------------* 
CPMMRS*                                                               * 
CPMMRS*      PROGRAMADOR  : MAURICIO ROQUE DA SILVA - CPM             * 
CPMMRS*      ANALISTA     : MAURICIO ROQUE DA SILVA - 4196-0570       * 
CPMMRS*      DATA         : 30/05/2007                                * 
CPMMRS*                                                               * 
CPMMRS*      OBJETIVO     : INCLUSAO DO PARM VERIFICAR ENDERECO NA    * 
CPMMRS*                     LOCALIZADORA OU ENVIAR CARTA DE AVISO     * 
CPMMRS*                     0 - ENVIAR CARTA DE AVISO                 * 
CPMMRS*                     0 - ENVIAR PARA LOCALIZADORA              * 
CPMMRS*                                                               * 
CPMMRS*===============================================================* 
BRQ141******************************************************************
BRQ141* MAIO/2012 - (BRQ) - TRATAMENTO DE CARTEIRA  ALFANUMERICA        
BRQ141******************************************************************
      *               B R Q     I T     S E R V I C E S                *
      *================================================================*
      *                       A L T E R A C A O                        *
      *----------------------------------------------------------------*
      *    PROGRAMADOR.:  HENRIQUE GUIMARAES      - BRQ                *
      *    ANALISTA....:  HENRIQUE GUIMARAES      - BRQ                *
      *    DATA........:  09/04/2014                                   *
      *----------------------------------------------------------------*
      *    OBJETIVO....:  ADAPTACOES PARA A LEI DA TRANSPARENCIA       *
      *                   INCLUIDOS CAMPOS LT NA I#CLLPLC              *
      *                   TROCAR BOOK I#CLLPLE(600) POR I#CLLPLJ(1290) *
      *                   ALTERAR OCORRENCIAS DE 7 PARA 11             *
      *                   NAO ACUMULAR NATUREZA, CASO EXISTAM MAIS DE  *
      *                 11 OCORRENCIAS CRIAR N REGISTROS.              *
      *    PROJETO 13-0358                                             *
      *================================================================*
           EJECT                                                        
      *===============================================================* 
       ENVIRONMENT DIVISION.                                            
      *===============================================================* 
                                                                        
      *---------------------------------------------------------------* 
       CONFIGURATION SECTION.                                           
      *---------------------------------------------------------------* 
                                                                        
       SPECIAL-NAMES.                                                   
           DECIMAL-POINT IS COMMA.                                      
                                                                        
           EJECT                                                        
      *---------------------------------------------------------------* 
       INPUT-OUTPUT SECTION.                                            
      *---------------------------------------------------------------* 
                                                                        
       FILE-CONTROL.                                                    
                                                                        
           SELECT  ARQLPCL   ASSIGN  TO  UT-S-ARQLPCL                   
                   FILE      STATUS  IS  WRK-FS-ARQLPCL.                
                                                                        
           SELECT  CADLPCL   ASSIGN  TO  UT-S-CADLPCL                   
                   FILE      STATUS  IS  WRK-FS-CADLPCL.                
                                                                        
           SELECT  TOTAIS    ASSIGN  TO  UT-S-TOTAIS                    
                   FILE      STATUS  IS  WRK-FS-TOTAIS.                 
                                                                        
           SELECT  PARMCLLP  ASSIGN  TO  UT-S-PARMCLLP                  
                   FILE      STATUS  IS  WRK-FS-PARMCLLP.               
                                                                        
           SELECT  ARQDATA   ASSIGN  TO  UT-S-ARQDATA                   
                   FILE      STATUS  IS  WRK-FS-ARQDATA.                
                                                                        
           EJECT                                                        
      *===============================================================* 
       DATA DIVISION.                                                   
      *===============================================================* 
                                                                        
      *---------------------------------------------------------------* 
       FILE SECTION.                                                    
      *---------------------------------------------------------------* 
                                                                        
       FD  ARQLPCL                                                      
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
-INC I#CLLPLC                                                           
           EJECT                                                        
      *---------------------------------------------------------------* 
      *    OUTPUT:    ARQUIVO DE AVALISTAS PARA EMISSAO DE AVISO      * 
      *               ORG. SEQUENCIAL   -   LRECL = 1290              * 
      *---------------------------------------------------------------* 
                                                                        
       FD  CADLPCL                                                      
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
-INC I#CLLPLJ                                                           
                                                                        
           EJECT                                                        
      *---------------------------------------------------------------* 
      *    OUTPUT:    RELATORIO DE TOTAIS                             * 
      *               ORG. SEQUENCIAL   -   LRECL = 080               * 
      *---------------------------------------------------------------* 
                                                                        
       FD  TOTAIS                                                       
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  REG-TOTAIS                       PIC X(80).                  
                                                                        
      *---------------------------------------------------------------* 
      *    INPUT:     ORG. SEQUENCIAL   -   LRECL = 250               * 
      *---------------------------------------------------------------* 
                                                                        
       FD  PARMCLLP                                                     
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  REG-PARAMETRO.                                               
           05  PARM-TAREFA.                                             
               10  PARM-CCUSTO-1       PIC X(04).                       
               10  PARM-CODIGO-1       PIC 9(04).                       
               10  PARM-CCUSTO-2       PIC X(04).                       
               10  PARM-CODIGO-2       PIC 9(04).                       
           05  PARM-SEQUENCIA          PIC 9(07).                       
           05  PARM-NR-DIAS            PIC 9(03).                       
           05  PARM-VL-CORTE           PIC 9(15).                       
           05  FILLER                  PIC X(209).                      
                                                                        
      *---------------------------------------------------------------* 
      *    INPUT:     ORG. SEQUENCIAL   -                             * 
      *---------------------------------------------------------------* 
                                                                        
       FD  ARQDATA                                                      
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
-INC I#CLLPGA                                                           
           EJECT                                                        
      *---------------------------------------------------------------* 
       WORKING-STORAGE SECTION.                                         
      *---------------------------------------------------------------* 
                                                                        
       77  FILLER                      PIC X(32)        VALUE           
           '* INICIO DA WORKING CLLP7623 *'.                            
                                                                        
       77  WRK-ABEND                   PIC S9(04) COMP  VALUE +1111.    
       77  WRK-FIM-ARQLPCL             PIC  X(01)        VALUE  'N'.    
       77  WRK-FINALIZA                PIC  X(01)        VALUE  'N'.    
       77  WRK-RETURN-CODE             PIC S9(05)        VALUE ZEROS.   
       77  WRK-DESVIO                  PIC X(01)         VALUE 'N'.     
       77  WRK-IND                     PIC 9(02)         VALUE ZEROS.   
                                                                        
       77  WRK-FS-ARQLPCL              PIC  X(02)        VALUE  SPACES. 
       77  WRK-FS-CADLPCL              PIC  X(02)        VALUE  SPACES. 
       77  WRK-FS-TOTAIS               PIC  X(02)        VALUE  SPACES. 
       77  WRK-FS-PARMCLLP             PIC  X(02)        VALUE  SPACES. 
       77  WRK-FS-ARQDATA              PIC  X(02)        VALUE  SPACES. 
                                                                        
       77  WRK-OPERACAO                PIC  X(13) VALUE SPACES.         
       77  WRK-ABERTURA                PIC  X(13) VALUE 'NA ABERTURA'.  
       77  WRK-LEITURA                 PIC  X(13) VALUE 'NA LEITURA'.   
       77  WRK-GRAVACAO                PIC  X(13) VALUE 'NA GRAVACAO'.  
       77  WRK-FECHAMENTO              PIC  X(13) VALUE 'NO FECHAMENTO'.
                                                                        
       77  ACU-LIDOS-ARQLPCL           PIC  9(08) COMP-3 VALUE ZEROS.   
       77  ACU-GRAVA-CADLPCL           PIC  9(08) COMP-3 VALUE ZEROS.   
       77  ACU-GRAVA-ARQINCON          PIC  9(08) COMP-3 VALUE ZEROS.   
                                                                        
       77  WRK-QTD-DIA                 PIC  9(02) COMP-3 VALUE ZEROS.   
                                                                        
       01  WRK-PARM-NR-DIAS            PIC  9(03) COMP-3 VALUE ZEROS.   
                                                                        
       01  WRK-DIAS5                   PIC  9(03) COMP-3 VALUE ZEROS.   
                                                                        
       01  WRK-DT-VCTO                 PIC  9(08).                      
                                                                        
       01  WRK-DT-AVALISTA             PIC  9(08).                      
       01  FILLER  REDEFINES  WRK-DT-AVALISTA.                          
           03  WRK-DT-MOV-AAAA-1       PIC  9(04).                      
           03  WRK-DT-MOV-MM-1         PIC  9(02).                      
           03  WRK-DT-MOV-DD-1         PIC  9(02).                      
                                                                        
       01  WRK-DT-MENOS01D             PIC  9(08).                      
       01  FILLER  REDEFINES  WRK-DT-MENOS01D.                          
           03  WRK-DT01-MOV-AAAA       PIC  9(04).                      
           03  WRK-DT01-MOV-MM         PIC  9(02).                      
           03  WRK-DT01-MOV-DD         PIC  9(02).                      
                                                                        
       01  WRK-VCTO-INV                PIC  9(08).                      
       01  FILLER   REDEFINES   WRK-VCTO-INV.                           
           03  WRK-VCTO-INV-AAAA       PIC  9(04).                      
           03  WRK-VCTO-INV-MM         PIC  9(02).                      
           03  WRK-VCTO-INV-DD         PIC  9(02).                      
                                                                        
       01  WRK-DT-MOV                  PIC  9(08).                      
       01  FILLER   REDEFINES   WRK-DT-MOV.                             
           03  WRK-DT-MOV-AAAA         PIC  9(04).                      
           03  WRK-DT-MOV-MM           PIC  9(02).                      
           03  WRK-DT-MOV-DD           PIC  9(02).                      
                                                                        
       01  WRK-TI-SYS                  PIC  9(06).                      
       01  FILLER   REDEFINES   WRK-TI-SYS.                             
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
                                                                        
       01  WRK-AREA-POOL1285.                                           
           03  WRK-DT-ENTRADA          PIC  9(08) COMP-3 VALUE ZEROS.   
           03  WRK-NUM-DIAS            PIC S9(05) COMP-3 VALUE ZEROS.   
           03  WRK-DT-SAIDA            PIC  9(08) COMP-3 VALUE ZEROS.   
           03  WRK-MENSAGEM            PIC  X(50)        VALUE SPACES.  
                                                                        
       01  WRK-AREA-POOL0025.                                           
           03  WRK-DT-ABERT            PIC  9(09) COMP-3 VALUE ZEROS.   
           03  WRK-PRAZO               PIC S9(05) COMP-3 VALUE ZEROS.   
           03  WRK-DT-SAIDA25          PIC  9(09) COMP-3 VALUE ZEROS.   
                                                                        
       01  WRK-AREA-POOL0110.                                           
           03  WRK-POOL0110.                                            
               05  WRK-NUMERO-0110     PIC  9(09)        VALUE ZEROS.   
               05  WRK-FILIAL-0110     PIC  9(05)        VALUE ZEROS.   
           03  WRK-CONTROLE-0110       PIC  X(02)        VALUE SPACES.  
                                                                        
       01  WRK-CONTROLE-0110-AUX       PIC  9(03)   COMP-3.             
                                                                        
       01  AUX-DT-INV-AAAAMMDD         PIC  9(08).                      
       01  FILLER   REDEFINES   AUX-DT-INV-AAAAMMDD.                    
           03  AUX-DT-INV-AAAA         PIC  9(04).                      
           03  AUX-DT-INV-MM           PIC  9(02).                      
           03  AUX-DT-INV-DD           PIC  9(02).                      
                                                                        
       01  AUX-DT-DB2.                                                  
           03  AUX-DT-DB2-DD           PIC  9(02).                      
           03  FILLER                  PIC  X(01).                      
           03  AUX-DT-DB2-MM           PIC  9(02).                      
           03  FILLER                  PIC  X(01).                      
           03  AUX-DT-DB2-AAAA         PIC  9(04).                      
           03  FILLER   REDEFINES   AUX-DT-DB2-AAAA.                    
               05  AUX-DT-DB2-SEC      PIC  9(02).                      
               05  AUX-DT-DB2-ANO      PIC  9(02).                      
                                                                        
       01  WRK-CH-ARQLPCL-ATU.                                          
           03  WRK-CPF-NUM-AVA-ATU     PIC 9(09)   COMP-3 VALUE ZEROS.  
           03  WRK-CPF-FIL-AVA-ATU     PIC 9(05)   COMP-3 VALUE ZEROS.  
           03  WRK-CPF-CTR-AVA-ATU     PIC 9(03)   COMP-3 VALUE ZEROS.  
           03  WRK-AGENCIA-ATU         PIC 9(05)   COMP-3 VALUE ZEROS.  
           03  WRK-CONTA-ATU           PIC 9(07)   COMP-3 VALUE ZEROS.  
                                                                        
       01  WRK-CH-ARQLPCL-ANT.                                          
           03  WRK-CPF-NUM-AVA-ANT     PIC 9(09)   COMP-3 VALUE ZEROS.  
           03  WRK-CPF-FIL-AVA-ANT     PIC 9(05)   COMP-3 VALUE ZEROS.  
           03  WRK-CPF-CTR-AVA-ANT     PIC 9(03)   COMP-3 VALUE ZEROS.  
           03  WRK-AGENCIA-ANT         PIC 9(05)   COMP-3 VALUE ZEROS.  
           03  WRK-CONTA-ANT           PIC 9(07)   COMP-3 VALUE ZEROS.  
                                                                        
      *---------------------------------------------------------------* 
      *                 DEFINICAO DO RELATORIO DE TOTAIS              * 
      *---------------------------------------------------------------* 
                                                                        
       01  CABEC1.                                                      
           03  CB1-CARRO               PIC X(01) VALUE '1'.             
           03  FILLER                  PIC X(28) VALUE 'CLLP7623'.      
SID        03  CB1-NOME-BCO            PIC X(37) VALUE SPACES.          
SID************'BANCO   BRADESCO  S.A.'.                                
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
               'RELATORIO DE TOTAIS - DATA MOVIMENTO'.                  
           03  FILLER.                                                  
               05  CB2-DTMV-DD         PIC 9(2)/.                       
               05  CB2-DTMV-MM         PIC 9(2)/.                       
               05  CB2-DTMV-AAAA       PIC 9(4).                        
           03  FILLER                  PIC X(07) VALUE SPACES.          
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
               'REGISTROS LIDOS - ARQLPCL     = '.                      
           03  LT1-LIDOS-ARQLPCL       PIC ZZ.ZZZ.ZZ9.                  
                                                                        
       01  LINTOT2.                                                     
           03  LT2-CARRO               PIC X(01) VALUE '0'.             
           03  FILLER                  PIC X(17) VALUE SPACES.          
           03  FILLER                  PIC X(32) VALUE                  
               'REGISTROS GRAVADOS - CADLPCL  = '.                      
           03  LT2-GRAVA-CADLPCL       PIC ZZ.ZZZ.ZZ9.                  
                                                                        
       01  FILLER                      PIC X(32)        VALUE           
           '*  FIM DA WORKING CLLP7623 *'.                              
           EJECT                                                        
                                                                        
CPMMRS*---------------------------------------------------------------* 
CPMMRS LINKAGE                        SECTION.                          
CPMMRS*---------------------------------------------------------------* 
CPMMRS 01  LNK-PARM.                                                    
CPMMRS     05   LNK-TAMANHO      PIC S9(04) COMP.                       
CPMMRS     05   LNK-QTD-DIA      PIC  9(02).                            
           05   LNK-QTD-DIA-R REDEFINES                                 
                LNK-QTD-DIA      PIC  X(02).                            
SID        05   LNK-CCUSTO-PARM  PIC  X(04).                            
CPMMRS                                                                  
CPMMRS*===============================================================* 
CPMMRS PROCEDURE      DIVISION      USING       LNK-PARM.               
CPMMRS*---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       0000-INICIAR                    SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           OPEN  INPUT   ARQLPCL  PARMCLLP ARQDATA                      
                 OUTPUT  CADLPCL  TOTAIS.                               
                                                                        
           MOVE  WRK-ABERTURA          TO   WRK-OPERACAO.               
           PERFORM   1000-TESTAR-FILE-STATUS.                           
                                                                        
CPMMRS     IF   LNK-QTD-DIA-R  EQUAL SPACES OR LOW-VALUES               
CPMMRS          DISPLAY '************* CLLP7623 **************'         
CPMMRS          DISPLAY '*                                   *'         
CPMMRS          DISPLAY '*   PARM NAO FOI INFORMADO          *'         
CPMMRS          DISPLAY '*                                   *'         
CPMMRS          DISPLAY '************* CLLP7623 **************'         
CPMMRS          MOVE    4 TO RETURN-CODE                                
CPMMRS          PERFORM 0000-FINALIZAR                                  
           END-IF.                                                      
                                                                        
           MOVE LNK-QTD-DIA              TO WRK-QTD-DIA.                
CPMMRS                                                                  
CPMMRS     IF   LNK-QTD-DIA    NOT EQUAL 02 AND 00                      
CPMMRS          DISPLAY '************* CLLP7623 **************'         
CPMMRS          DISPLAY '*                                   *'         
CPMMRS          DISPLAY '*   PARM INFORMADO ESTA INVALIDO    *'         
CPMMRS          DISPLAY '*                                   *'         
CPMMRS          DISPLAY '************* CLLP7623 **************'         
CPMMRS          MOVE    4 TO RETURN-CODE                                
CPMMRS          PERFORM 0000-FINALIZAR                                  
           END-IF.                                                      
                                                                        
           PERFORM   2000-INICIALIZA.                                   
                                                                        
           IF  WRK-FINALIZA   EQUAL   'S'                               
               GO             TO      0000-FINALIZAR.                   
                                                                        
           PERFORM   3000-PROCESSA-ARQLPCL  UNTIL                       
                           WRK-FIM-ARQLPCL  EQUAL   'S'.                
                                                                        
           PERFORM   4000-IMPRIME-TOTAIS.                               
                                                                        
       0000-FINALIZAR.                                                  
      *---------------*                                                 
                                                                        
           CLOSE  ARQLPCL   CADLPCL   TOTAIS ARQDATA.                   
                                                                        
           MOVE  WRK-FECHAMENTO        TO   WRK-OPERACAO.               
           PERFORM   1000-TESTAR-FILE-STATUS.                           
                                                                        
           GOBACK.                                                      
                                                                        
      *---------------------------------------------------------------* 
       0000-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       1000-TESTAR-FILE-STATUS         SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           PERFORM   1100-TESTAR-FS-ARQLPCL.                            
                                                                        
           PERFORM   1200-TESTAR-FS-TOTAIS.                             
                                                                        
           PERFORM   1300-TESTAR-FS-CADLPCL.                            
                                                                        
           PERFORM   1400-TESTAR-FS-PARMCLLP.                           
                                                                        
           PERFORM   1500-TESTAR-FS-ARQDATA.                            
                                                                        
      *---------------------------------------------------------------* 
       1000-99-FIM. EXIT.                                               
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       1100-TESTAR-FS-ARQLPCL          SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           IF  WRK-FS-ARQLPCL  NOT EQUAL   '00'                         
               DISPLAY '************** CLLP7623 *************'          
               DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'      
               DISPLAY '*              ARQLPCL              *'          
               DISPLAY '*         FILE STATUS =  ' WRK-FS-ARQLPCL       
                                                  '         *'          
               DISPLAY '************** CLLP7623 *************'          
               CALL   'ILBOABN0'   USING   WRK-ABEND.                   
                                                                        
      *---------------------------------------------------------------* 
       1100-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       1200-TESTAR-FS-TOTAIS           SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           IF  WRK-FS-TOTAIS   NOT EQUAL   '00'                         
               DISPLAY '************** CLLP7623 *************'          
               DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'      
               DISPLAY '*              TOTAIS               *'          
               DISPLAY '*         FILE STATUS =  ' WRK-FS-TOTAIS        
                                                  '         *'          
               DISPLAY '************** CLLP7623 *************'          
               CALL   'ILBOABN0'   USING   WRK-ABEND.                   
                                                                        
      *---------------------------------------------------------------* 
       1200-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       1300-TESTAR-FS-CADLPCL          SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           IF  WRK-FS-CADLPCL  NOT EQUAL   '00'                         
               DISPLAY '************** CLLP7623 *************'          
               DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'      
               DISPLAY '*              CADLPCL              *'          
               DISPLAY '*         FILE STATUS =  ' WRK-FS-CADLPCL       
                                                  '         *'          
               DISPLAY '************** CLLP7623 *************'          
               CALL   'ILBOABN0'   USING   WRK-ABEND.                   
                                                                        
      *---------------------------------------------------------------* 
       1300-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       1400-TESTAR-FS-PARMCLLP         SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           IF  WRK-FS-PARMCLLP NOT EQUAL   '00'                         
               DISPLAY '************** CLLP7623 *************'          
               DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'      
               DISPLAY '*              PARMCLLP             *'          
               DISPLAY '*         FILE STATUS =  ' WRK-FS-PARMCLLP      
                                                  '         *'          
               DISPLAY '************** CLLP7623 *************'          
               CALL   'ILBOABN0'   USING   WRK-ABEND.                   
                                                                        
      *---------------------------------------------------------------* 
       1400-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       1500-TESTAR-FS-ARQDATA          SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           IF  WRK-FS-ARQDATA  NOT EQUAL   '00'                         
               DISPLAY '************** CLLP7623 *************'          
               DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'      
               DISPLAY '*              ARQDATA              *'          
               DISPLAY '*         FILE STATUS =  ' WRK-FS-ARQDATA       
                                                  '         *'          
               DISPLAY '************** CLLP7623 *************'          
               CALL   'ILBOABN0'   USING   WRK-ABEND.                   
                                                                        
      *---------------------------------------------------------------* 
       1500-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       2000-INICIALIZA                 SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           CALL   'POOL7600'   USING   WRK-AREA-POOL7600.               
                                                                        
           MOVE  WRK-DT-AAAAMMDD       TO   AUX-DT-INV-AAAAMMDD.        
           MOVE  AUX-DT-INV-DD         TO   CB2-DTSYS-DD.               
           MOVE  AUX-DT-INV-MM         TO   CB2-DTSYS-MM.               
           MOVE  AUX-DT-INV-AAAA       TO   CB2-DTSYS-AAAA.             
                                                                        
           MOVE  WRK-TI-HHMMSS         TO   WRK-TI-SYS.                 
           MOVE  WRK-TI-HOR            TO   CB2-TIME-HOR.               
           MOVE  WRK-TI-MIN            TO   CB2-TIME-MIN.               
           MOVE  WRK-TI-SEG            TO   CB2-TIME-SEG.               
                                                                        
           PERFORM   2100-LER-ARQLPCL.                                  
                                                                        
           PERFORM   2300-LER-ARQDATA.                                  
                                                                        
           IF  WRK-FS-ARQDATA      EQUAL   '10'                         
               DISPLAY '************** CLLP7623 *************'          
               DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'      
               DISPLAY '*       ARQUIVO ARQDATA  VAZIO      *'          
               DISPLAY '*         FILE STATUS =  ' WRK-FS-ARQDATA       
                                                  '         *'          
               DISPLAY '************** CLLP7623 *************'          
               CALL   'ILBOABN0'   USING   WRK-ABEND.                   
                                                                        
           PERFORM   2200-LER-PARMCLLP.                                 
                                                                        
           IF  WRK-FS-PARMCLLP     EQUAL   '10'                         
               DISPLAY '************** CLLP7623 *************'          
               DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'      
               DISPLAY '*       ARQUIVO PARMCLLP VAZIO      *'          
               DISPLAY '*         FILE STATUS =  ' WRK-FS-PARMCLLP      
                                                  '         *'          
               DISPLAY '************** CLLP7623 *************'          
               CALL   'ILBOABN0'   USING   WRK-ABEND.                   
                                                                        
           MOVE  PARM-NR-DIAS        TO WRK-PARM-NR-DIAS                
                                                                        
CPMMRS     COMPUTE  WRK-DIAS5 =  WRK-PARM-NR-DIAS - WRK-QTD-DIA         
                                                                        
           MOVE    DTMOVINV          TO     WRK-DT-ENTRADA              
                                                                        
           COMPUTE WRK-NUM-DIAS      =    ( WRK-DIAS5 - 1) .            
           COMPUTE WRK-NUM-DIAS      =    ( WRK-NUM-DIAS * -1) .        
                                                                        
REMOVE*    DISPLAY '--------- CALC.DTMOVINV 1O--------'                 
REMOVE*    DISPLAY 'DT.ENTRADA=' WRK-DT-ENTRADA.                        
REMOVE*    DISPLAY 'QTD.DIAS  =' WRK-NUM-DIAS                           
                                                                        
           CALL   'POOL1285' USING WRK-AREA-POOL1285.                   
           IF RETURN-CODE NOT EQUAL ZEROS                               
              MOVE    RETURN-CODE   TO  WRK-RETURN-CODE                 
              DISPLAY '******************* CLLP7623 *******************'
              DISPLAY '*   ERRO  UTILIZANDO  A  POOL1285    '           
              DISPLAY '*                                    '           
              DISPLAY '*   DATA-ENTRADA     = ' WRK-DT-ENTRADA          
              DISPLAY '*   RETURN-CODE      = ' WRK-RETURN-CODE         
              DISPLAY '*   MENSAGEM DO ERRO = ' WRK-MENSAGEM            
              DISPLAY '*                                    '           
              DISPLAY '******************* CLLP7623 *******************'
              MOVE  'S'  TO  WRK-FINALIZA                               
           ELSE                                                         
              MOVE   WRK-DT-SAIDA       TO  WRK-DT-AVALISTA.            
                                                                        
REMOVE*    DISPLAY 'DT.SAIDA  =' WRK-DT-SAIDA                           
                                                                        
           MOVE  DTMOVINV              TO   WRK-DT-MOV.                 
           MOVE  WRK-DT-MOV-DD         TO   CB2-DTMV-DD.                
           MOVE  WRK-DT-MOV-MM         TO   CB2-DTMV-MM.                
           MOVE  WRK-DT-MOV-AAAA       TO   CB2-DTMV-AAAA.              
                                                                        
           MOVE   WRK-DIAS5          TO     WRK-NUM-DIAS                
           COMPUTE WRK-NUM-DIAS      =    ( WRK-NUM-DIAS * -1)          
                                                                        
REMOVE*    DISPLAY '--------- CALC.DTMOVINV 2O--------'                 
REMOVE*    DISPLAY 'DT.ENTRADA=' WRK-DT-ENTRADA.                        
REMOVE*    DISPLAY 'QTD.DIAS  =' WRK-NUM-DIAS                           
           CALL   'POOL1285' USING WRK-AREA-POOL1285.                   
                                                                        
           IF RETURN-CODE NOT EQUAL ZEROS                               
              MOVE    RETURN-CODE   TO  WRK-RETURN-CODE                 
              DISPLAY '******************* CLLP7623 *******************'
              DISPLAY '*   ERRO  UTILIZANDO  A  POOL1285    '           
              DISPLAY '*                                    '           
              DISPLAY '*   DATA-ENTRADA     = ' WRK-DT-ENTRADA          
              DISPLAY '*   RETURN-CODE      = ' WRK-RETURN-CODE         
              DISPLAY '*   MENSAGEM DO ERRO = ' WRK-MENSAGEM            
              DISPLAY '*                                    '           
              DISPLAY '******************* CLLP7623 *******************'
              MOVE  'S'  TO  WRK-FINALIZA                               
           ELSE                                                         
              MOVE   WRK-DT-SAIDA       TO  WRK-DT-MENOS01D.            
                                                                        
REMOVE*    DISPLAY 'DT.SAIDA  =' WRK-DT-SAIDA.                          
                                                                        
      *---------------------------------------------------------------* 
       2000-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       2100-LER-ARQLPCL                SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           READ   ARQLPCL.                                              
                                                                        
           IF  WRK-FS-ARQLPCL  EQUAL   '10'                             
               MOVE  'S'               TO   WRK-FIM-ARQLPCL             
                                            WRK-FINALIZA                
               GO                      TO   2100-99-FIM.                
                                                                        
           MOVE  WRK-LEITURA           TO   WRK-OPERACAO.               
           PERFORM   1100-TESTAR-FS-ARQLPCL.                            
                                                                        
           ADD   1                     TO   ACU-LIDOS-ARQLPCL.          
                                                                        
           MOVE  CAD-NCGC1-CADLPCL     TO   WRK-CPF-NUM-AVA-ATU.        
           MOVE  CAD-FIL1-CADLPCL      TO   WRK-CPF-FIL-AVA-ATU.        
           MOVE  CAD-CTR1-CADLPCL      TO   WRK-CPF-CTR-AVA-ATU.        
           MOVE  AGENCIA-CADLPCL       TO   WRK-AGENCIA-ATU.            
           MOVE  CONTA-CADLPCL         TO   WRK-CONTA-ATU.              
                                                                        
      *---------------------------------------------------------------* 
       2100-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       2200-LER-PARMCLLP               SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           READ   PARMCLLP.                                             
                                                                        
           IF  WRK-FS-PARMCLLP EQUAL   '10'                             
               GO                      TO   2200-99-FIM.                
                                                                        
           MOVE  WRK-LEITURA           TO   WRK-OPERACAO.               
           PERFORM   1400-TESTAR-FS-PARMCLLP.                           
      *    DISPLAY 'PARM-CCUSTO-1 = '  PARM-CCUSTO-1.                   
      *    DISPLAY 'PARM-CODIGO-1 = '  PARM-CODIGO-1.                   
      *    DISPLAY 'PARM-CCUSTO-2 = '  PARM-CCUSTO-2.                   
      *    DISPLAY 'PARM-CODIGO-2 = '  PARM-CODIGO-2.                   
                                                                        
           IF  PARM-CCUSTO-1   NOT EQUAL   'CLLP'   OR                  
               PARM-CODIGO-1   NOT EQUAL   '0002'   OR                  
               PARM-CCUSTO-2   NOT EQUAL   'CLLP'   OR                  
               PARM-CODIGO-2   NOT EQUAL   '7615'                       
               DISPLAY '****************  CLLP7623  *******************'
               DISPLAY '*           ABEND 1111 - FORCADO              *'
               DISPLAY '*  PROGRAMA CANCELADO - PARAMETRO INVALIDO    *'
               DISPLAY '* '  PARM-CCUSTO-1 ' / ' PARM-CODIGO-1          
                       ' / ' PARM-CCUSTO-2 ' / ' PARM-CODIGO-2          
               DISPLAY '*   AVISAR ANALISTA RESPONSAVEL DA ROTINA     *'
               DISPLAY '*****************  CLLP7623  ******************'
               CALL  'ILBOABN0'  USING  WRK-ABEND.                      
                                                                        
           IF  PARM-SEQUENCIA   NOT  EQUAL   5                          
               GO               TO   2200-LER-PARMCLLP.                 
                                                                        
      *---------------------------------------------------------------* 
       2200-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       2300-LER-ARQDATA                SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           READ   ARQDATA.                                              
                                                                        
           IF  WRK-FS-ARQDATA  EQUAL   '10'                             
               GO                      TO   2300-99-FIM.                
                                                                        
           MOVE  WRK-LEITURA           TO   WRK-OPERACAO.               
           PERFORM   1500-TESTAR-FS-ARQDATA.                            
                                                                        
      *---------------------------------------------------------------* 
       2300-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       3000-PROCESSA-ARQLPCL           SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-CH-ARQLPCL-ATU       NOT EQUAL WRK-CH-ARQLPCL-ANT     
              MOVE DAT-VENCTO-CADLPCL  TO WRK-DT-VCTO                   
              IF (WRK-DT-VCTO          GREATER WRK-DT-MENOS01D AND      
                  WRK-DT-VCTO          NOT GREATER WRK-DT-AVALISTA)     
                  NEXT SENTENCE                                         
              ELSE                                                      
                  MOVE WRK-CH-ARQLPCL-ATU                               
                                       TO WRK-CH-ARQLPCL-ANT            
                  PERFORM   2100-LER-ARQLPCL UNTIL                      
                            WRK-CH-ARQLPCL-ATU NOT EQUAL                
                            WRK-CH-ARQLPCL-ANT OR                       
                            WRK-FIM-ARQLPCL EQUAL 'S'                   
                  GO                       TO        3000-99-FIM        
              END-IF                                                    
           END-IF.                                                      
                                                                        
           PERFORM   3200-MOVER-DADOS-CADLPCL.                          
                                                                        
      *---------------------------------------------------------------* 
       3000-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       3200-MOVER-DADOS-CADLPCL        SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           INITIALIZE REG-AVISO.                                        
                                                                        
           MOVE  CGCCPF-NUM-CADLPCL    TO   AVI-CGC.                    
           MOVE  CGCCPF-FIL-CADLPCL    TO   AVI-FIL.                    
           MOVE  CGCCPF-CTR-CADLPCL    TO   AVI-CTR.                    
           MOVE  EMPRESA-CADLPCL       TO   AVI-EMPRESA.                
           MOVE  AGENCIA-CADLPCL       TO   AVI-AGENCIA.                
           MOVE  CONTA-CADLPCL         TO   AVI-NUM-CC.                 
           MOVE  CAD-NOME1-CADLPCL     TO   AVI-NOME-AVAL.              
           MOVE  CAD-NCGC1-CADLPCL     TO   AVI-CGC-AVAL.               
           MOVE  CAD-FIL1-CADLPCL      TO   AVI-FIL-AVAL.               
           MOVE  CAD-CTR1-CADLPCL      TO   AVI-CTR-AVAL.               
           MOVE  CAD-IDENT-CADLPCL     TO   AVI-IDENT.                  
           MOVE  NOME-CAD-CADLPCL      TO   AVI-NOME-DEVEDOR.           
                                                                        
           MOVE  WRK-CH-ARQLPCL-ATU    TO   WRK-CH-ARQLPCL-ANT.         
                                                                        
           PERFORM 3250-ATUALIZA-OCORRENCIA VARYING                     
                   WRK-IND FROM 1 BY 1                                  
                   UNTIL WRK-IND GREATER 11   OR                        
                         WRK-CH-ARQLPCL-ATU   NOT EQUAL                 
                         WRK-CH-ARQLPCL-ANT   OR                        
                         WRK-FIM-ARQLPCL      EQUAL 'S'.                
                                                                        
           PERFORM   3300-GRAVAR-CADLPCL.                               
                                                                        
      *---------------------------------------------------------------* 
       3200-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       3250-ATUALIZA-OCORRENCIA        SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           IF   NATUREZA-CADLPCL       NOT EQUAL SPACES                 
                MOVE SPACES            TO AVI-POSSIBILIT (WRK-IND)      
                MOVE NATUREZA-CADLPCL  TO AVI-NATUREZA   (WRK-IND)      
                MOVE CARTEIRA-CADLPCL  TO AVI-CARTEIRA   (WRK-IND)      
                MOVE CONTRATO-CADLPCL  TO AVI-CONTRATO   (WRK-IND)      
                MOVE DAT-VENCTO-CADLPCL TO AVI-DAT-VENCTO (WRK-IND)     
                MOVE ZEROS             TO AVI-IOF-NORMAL (WRK-IND)      
                MOVE VAL-RESG-CADLPCL  TO AVI-RESGATE    (WRK-IND)      
                MOVE CADLPCL-VR-REMUNERATORIO                           
                                       TO AVI-VR-REMUNERATORIO          
                                                         (WRK-IND)      
                MOVE CADLPCL-VALOR-MORATORIO                            
                                       TO AVI-VALOR-MORATORIO           
                                                         (WRK-IND)      
                MOVE CADLPCL-VALOR-MULTA                                
                                       TO AVI-VALOR-MULTA               
                                                         (WRK-IND)      
                MOVE CADLPCL-DESP-JUD-CUSTAS                            
                                       TO AVI-DESP-JUD-CUSTAS           
                                                         (WRK-IND)      
                MOVE CADLPCL-HONORARIOS                                 
                                       TO AVI-HONORARIOS                
                                                         (WRK-IND)      
                MOVE CADLPCL-VL-TAXA-TARIFA                             
                                       TO AVI-VL-TAXA-TARIFA            
                                                         (WRK-IND)      
                MOVE CADLPCL-VL-TOTAL-DIVIDA                            
                                       TO AVI-VL-TOTAL-DIVIDA           
                                                         (WRK-IND)      
           END-IF.                                                      
                                                                        
           PERFORM   2100-LER-ARQLPCL.                                  
                                                                        
      *---------------------------------------------------------------* 
       3250-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       3300-GRAVAR-CADLPCL             SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           WRITE   REG-AVISO.                                           
                                                                        
           MOVE  WRK-GRAVACAO          TO   WRK-OPERACAO.               
           PERFORM   1300-TESTAR-FS-CADLPCL.                            
                                                                        
           ADD   1                     TO   ACU-GRAVA-CADLPCL.          
                                                                        
      *---------------------------------------------------------------* 
       3300-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       4000-IMPRIME-TOTAIS             SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           MOVE  ACU-LIDOS-ARQLPCL     TO   LT1-LIDOS-ARQLPCL.          
           MOVE  ACU-GRAVA-CADLPCL     TO   LT2-GRAVA-CADLPCL.          
           MOVE  WRK-GRAVACAO          TO   WRK-OPERACAO.               
                                                                        
           MOVE  1                     TO   CB1-PAG.                    
                                                                        
SID        IF  LNK-CCUSTO-PARM EQUAL 'LEAS'                             
SID            MOVE 'BRADESCO LEASING S/A' TO CB1-NOME-BCO              
SID        ELSE                                                         
SID            MOVE 'BANCO BRADESCO S.A  ' TO CB1-NOME-BCO              
SID        END-IF.                                                      
                                                                        
           WRITE   REG-TOTAIS          FROM   CABEC1.                   
           PERFORM   1200-TESTAR-FS-TOTAIS.                             
                                                                        
           WRITE   REG-TOTAIS          FROM   CABEC2.                   
           PERFORM   1200-TESTAR-FS-TOTAIS.                             
                                                                        
           WRITE   REG-TOTAIS          FROM   LINSPACE.                 
           PERFORM   1200-TESTAR-FS-TOTAIS.                             
                                                                        
           WRITE   REG-TOTAIS          FROM   LINTOT1.                  
           PERFORM   1200-TESTAR-FS-TOTAIS.                             
                                                                        
           WRITE   REG-TOTAIS          FROM   LINTOT2.                  
           PERFORM   1200-TESTAR-FS-TOTAIS.                             
                                                                        
      *---------------------------------------------------------------* 
       4000-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 

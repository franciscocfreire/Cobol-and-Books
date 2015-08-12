      *===============================================================* 
       IDENTIFICATION DIVISION.                                         
      *===============================================================* 
                                                                        
       PROGRAM-ID. CLLP7108.                                            
      *===============================================================* 
      *                   C P M   S I S T E M A S                     * 
      *---------------------------------------------------------------* 
      *                                                               * 
      *      PROGRAMADORA.: GISELE PAUL        - CPM/FPOLIS           * 
      *      ANALISTA CPM.: RESENDE            - CPM/FPOLIS           * 
      *      ANALISTA.....: TOMOKO             - GP. 82               * 
      *      DATA.........: 13/11/2000                                * 
      *                                                               * 
      *      OBJETIVO.....:                                           * 
      *        LER ARQUIVO ARQDB2 SUMARIZADO SOMENTE COM OS DADOS     * 
      *        UTILIZADOS NOS PROGRAMAS SEGUINTES.                    * 
      *        INCLUIDO AS CONSISTENCIAS DO PROGRAMA CLLP7640(VELHO)  * 
      *                                                               * 
STF   *===============================================================* 
STF   *                        A L T E R A C A O                      * 
STF   *---------------------------------------------------------------* 
STF   *                                                               * 
STF   *      PROGRAMADOR  : JOCILEIDE AP. MARIANO  - STEFANINI        * 
STF   *      SUPERVISOR   : RICARDO BA             - STEFANINI        * 
STF   *      ANALISTA     : ENEAS CARVALHO         - F.(11)4196-0994  * 
STF   *      DATA         : 04/09/2002                                * 
STF   *                                                               * 
STF   *      OBJETIVO     : ENVIAR O SEGUNDO AVALISTA PARA SPC/SERASA * 
STF   *                                                               * 
STF   *      OBS.: ESTE PROGRAMA EH COPIA DO PROGRAMA CLLP7516        * 
STF   *                                                               * 
STF   *===============================================================* 
CPM   *                        A L T E R A C A O                      * 
CPM   *---------------------------------------------------------------* 
CPM   *                                                               * 
CPM   *      PROGRAMADOR  : MAURICIO ROQUE         - CPM              * 
CPM   *      ANALISTA     : MAURICIO ROQUE         - F.(11)4196-0570  * 
CPM   *      DATA         : 23/05/2007                                * 
CPM   *                                                               * 
CPM   *      OBJETIVO     : PERMITIR ENVIO DE CARTA DE AVISO          * 
CPM   *                     PARA AVALISTA NAO CORRENTISTA             * 
CPM   *                                                               * 
CPM   *                                                               * 
CPM   *===============================================================* 
BRQ141******************************************************************
BRQ141* MAIO/2012 - (BRQ) - TRATAMENTO DE CARTEIRA  ALFANUMERICA        
BRQ141******************************************************************
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
                                                                        
           SELECT  ARQDB2  ASSIGN TO UT-S-ARQDB2                        
                      FILE STATUS IS WRK-FS-ARQDB2.                     
                                                                        
           SELECT   TOTAIS ASSIGN TO UT-S-TOTAIS                        
                      FILE STATUS IS WRK-FS-TOTAIS.                     
                                                                        
           SELECT  ARQLPCL ASSIGN TO UT-S-ARQLPCL                       
                      FILE STATUS IS WRK-FS-ARQLPCL.                    
                                                                        
           SELECT ARQINCON ASSIGN TO UT-S-ARQINCON                      
                      FILE STATUS IS WRK-FS-ARQINCON.                   
                                                                        
           SELECT PARMCLLP ASSIGN TO UT-S-PARMCLLP                      
                      FILE STATUS IS WRK-FS-PARMCLLP.                   
                                                                        
           SELECT ARQDATA  ASSIGN TO UT-S-ARQDATA                       
                      FILE STATUS IS WRK-FS-ARQDATA.                    
           EJECT                                                        
      *===============================================================* 
       DATA DIVISION.                                                   
      *===============================================================* 
                                                                        
      *---------------------------------------------------------------* 
       FILE SECTION.                                                    
      *---------------------------------------------------------------* 
                                                                        
                                                                        
      *---------------------------------------------------------------* 
      *    INPUT:     ARQQUIVO DE TABELA DB2                          * 
      *               ORG. SEQUENCIAL   -   LRECL = 213               * 
      *---------------------------------------------------------------* 
                                                                        
       FD  ARQDB2                                                       
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
      ******************************************************************
      *        ARQDB2 SUMARIZADO                          LRECL = 213  *
      *               CAMPO  DB2-VCTO = AAAAMMDD                       *
      ******************************************************************
       01  DB2-REGTO.                                                   
           03  DB2-CHAVE.                                               
               05  DB2-CPF-DEV.                                         
                07 DB2-NUMERO-DEV          PIC 9(09)    COMP-3.         
                07 DB2-FILIAL              PIC 9(05)    COMP-3.         
                07 DB2-CTR-DEV             PIC 9(02).                   
               05  DB2-COD-NATUREZA-OPER   PIC X(03).                   
               05  DB2-CONTRATO            PIC 9(07)    COMP-3.         
           03  DB2-VCTO                    PIC 9(08).                   
           03  DB2-EMPRESA                 PIC 9(05)    COMP-3.         
           03  DB2-AGENCIA                 PIC 9(05)    COMP-3.         
           03  DB2-CONTA                   PIC 9(07)    COMP-3.         
           03  DB2-CARTEIRA                PIC X(03).                   
           03  DB2-ID                      PIC X(02).                   
           03  DB2-MOEDA                   PIC X(02).                   
           03  DB2-TIPO-PEND               PIC 9(05)    COMP-3.         
           03  DB2-CODIGO                  PIC 9(03)    COMP-3.         
           03  DB2-PRINCIPAL               PIC 9(13)V99 COMP-3.         
           03  DB2-VR-VENCIDOS             PIC 9(13)V99 COMP-3.         
           03  DB2-VR-VINCENDOS            PIC 9(13)V99 COMP-3.         
           03  DB2-NOME-DEVEDOR.                                        
               05  DB2-NOME-20             PIC X(20).                   
               05  FILLER                  PIC X(20).                   
           03  DB2-NOME-AVALISTA           PIC X(40).                   
           03  DB2-CPF-AVAL.                                            
               05  DB2-NUMERO-AVAL         PIC 9(09)    COMP-3.         
               05  DB2-FILIAL-AVAL         PIC 9(05)    COMP-3.         
               05  DB2-CTR-AVAL            PIC 9(02).                   
           03  DB2-NOME-AVAL2              PIC X(40).                   
           03  DB2-CPF-AVAL2.                                           
               05  DB2-NUMERO-AVAL2        PIC 9(09)    COMP-3.         
               05  DB2-FILIAL-AVAL2        PIC 9(05)    COMP-3.         
               05  DB2-CTR-AVAL2           PIC 9(02).                   
           03  DB2-COD-EMPR                PIC 9(02).                   
                                                                        
           EJECT                                                        
      *---------------------------------------------------------------* 
      *    OUTPUT:    DEVEDORES SELECIONADOS P/ SERASA/SPC            * 
      *               ORG. SEQUENCIAL   -   LRECL = 285               * 
      *---------------------------------------------------------------* 
                                                                        
       FD  ARQLPCL                                                      
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  REG-ARQLPCL                  PIC X(285).                     
                                                                        
           EJECT                                                        
      *---------------------------------------------------------------* 
      *    OUTPUT:    DEVEDORES NAO SELECIONADOS P/ SERASA/SPC        * 
      *               ORG. SEQUENCIAL   -   LRECL = 215               * 
      *---------------------------------------------------------------* 
                                                                        
       FD  ARQINCON                                                     
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
-INC I#CLLPCQ                                                           
                                                                        
           EJECT                                                        
      *---------------------------------------------------------------* 
      *    OUTPUT:    RELATORIO DE TOTAIS                             * 
      *               ORG. SEQUENCIAL   -   LRECL = 133               * 
      *---------------------------------------------------------------* 
                                                                        
       FD  TOTAIS                                                       
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  REG-TOTAIS                  PIC X(133).                      
                                                                        
                                                                        
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
                                                                        
           EJECT                                                        
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
           '* INICIO DA WORKING CLLP7108 *'.                            
                                                                        
       77  WRK-ABEND                   PIC S9(04) COMP  VALUE +1111.    
       77  WRK-FIM-ARQDB2              PIC  X(01)        VALUE  'N'.    
       77  WRK-FINALIZA                PIC  X(01)        VALUE  'N'.    
       77  WRK-RETURN-CODE             PIC S9(05)        VALUE ZEROS.   
       77  WRK-DESVIO                  PIC X(01)         VALUE 'N'.     
                                                                        
       77  WRK-FS-ARQDB2               PIC  X(02)        VALUE  SPACES. 
       77  WRK-FS-TOTAIS               PIC  X(02)        VALUE  SPACES. 
       77  WRK-FS-ARQLPCL              PIC  X(02)        VALUE  SPACES. 
       77  WRK-FS-ARQINCON             PIC  X(02)        VALUE  SPACES. 
       77  WRK-FS-PARMCLLP             PIC  X(02)        VALUE  SPACES. 
       77  WRK-FS-ARQDATA              PIC  X(02)        VALUE  SPACES. 
                                                                        
       77  WRK-OPERACAO                PIC  X(13) VALUE SPACES.         
       77  WRK-ABERTURA                PIC  X(13) VALUE 'NA ABERTURA'.  
       77  WRK-LEITURA                 PIC  X(13) VALUE 'NA LEITURA'.   
       77  WRK-GRAVACAO                PIC  X(13) VALUE 'NA GRAVACAO'.  
       77  WRK-FECHAMENTO              PIC  X(13) VALUE 'NO FECHAMENTO'.
                                                                        
       77  ACU-LIDOS-ARQDB2            PIC  9(08) COMP-3 VALUE ZEROS.   
       77  ACU-GRAVA-ARQLPCL           PIC  9(08) COMP-3 VALUE ZEROS.   
       77  ACU-GRAVA-ARQINCON          PIC  9(08) COMP-3 VALUE ZEROS.   
       77  ACU-DESPREZA-AVAL           PIC  9(08) COMP-3 VALUE ZEROS.   
       77  ACU-DESPREZA-DATA           PIC  9(08) COMP-3 VALUE ZEROS.   
       77  ACU-VAL-RESG                PIC  9(11)V99     VALUE ZEROS.   
       77  ACU-ENC-VENC                PIC  9(11)V99     VALUE ZEROS.   
       77  ACU-ENC-VINC                PIC  9(11)V99     VALUE ZEROS.   
                                                                        
       01  WRK-DIAS5                   PIC S9(03)         VALUE  ZEROS. 
                                                                        
       01  WRK-VL-CORTE5               PIC S9(15)         VALUE  ZEROS. 
                                                                        
       01  WRK-DIAS-EDIT               PIC ZZZZZ9.                      
       01  WRK-DATA-EDIT               PIC 9999.99.99.                  
                                                                        
      *---------------------------------------------------------------* 
      *             DEFINICAO DAS DATAS DE VENCIMENTO                 * 
      *---------------------------------------------------------------* 
                                                                        
                                                                        
       01  WRK-DT-AVALISTA             PIC  9(08).                      
       01  FILLER  REDEFINES  WRK-DT-AVALISTA.                          
           03  WRK-DT-MOV-AAAA         PIC  9(04).                      
           03  WRK-DT-MOV-MM           PIC  9(02).                      
           03  WRK-DT-MOV-DD           PIC  9(02).                      
                                                                        
       01  WRK-DT-MENOS35D             PIC  9(08).                      
       01  FILLER  REDEFINES  WRK-DT-MENOS35D.                          
           03  WRK-DT35-MOV-AAAA       PIC  9(04).                      
           03  WRK-DT35-MOV-MM         PIC  9(02).                      
           03  WRK-DT35-MOV-DD         PIC  9(02).                      
                                                                        
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
                                                                        
       01  WRK-AREA-POOL1285.                                           
           03  WRK-DT-ENTRADA          PIC  9(08) COMP-3 VALUE ZEROS.   
           03  WRK-NUM-DIAS            PIC S9(05) COMP-3 VALUE ZEROS.   
           03  WRK-DT-SAIDA            PIC  9(08) COMP-3 VALUE ZEROS.   
           03  WRK-MENSAGEM            PIC  X(50)        VALUE SPACES.  
                                                                        
       01  WRK-AREA-POOL0025.                                           
           03  WRK-DT-ABERT            PIC  9(09) COMP-3 VALUE ZEROS.   
           03  WRK-PRAZO               PIC S9(05) COMP-3 VALUE ZEROS.   
           03  WRK-DT-LIMI5ANOS        PIC  9(09) COMP-3 VALUE ZEROS.   
                                                                        
       01  WRK1-AREA-POOL0025.                                          
           03  WRK1-DT-ABERT            PIC  9(09) COMP-3 VALUE ZEROS.  
           03  WRK1-PRAZO               PIC S9(05) COMP-3 VALUE ZEROS.  
           03  WRK1-DT-SAIDA            PIC  9(09) COMP-3 VALUE ZEROS.  
                                                                        
       01  WRK-AREA-POOL0110.                                           
           03  WRK-POOL0110.                                            
               05  WRK-NUMERO-0110     PIC  9(09)        VALUE ZEROS.   
               05  WRK-FILIAL-0110     PIC  9(05)        VALUE ZEROS.   
           03  WRK-CONTROLE-0110       PIC  X(02)        VALUE SPACES.  
                                                                        
       01  AUX-DT-INV-AAAAMMDD         PIC  9(08).                      
       01  FILLER  REDEFINES  AUX-DT-INV-AAAAMMDD.                      
           03  AUX-DT-INV-AAAA         PIC  9(04).                      
           03  AUX-DT-INV-MM           PIC  9(02).                      
           03  AUX-DT-INV-DD           PIC  9(02).                      
                                                                        
       01  AUX-DT-DB2.                                                  
           03  AUX-DT-DB2-DD           PIC  9(02).                      
           03  FILLER                  PIC  X(01).                      
           03  AUX-DT-DB2-MM           PIC  9(02).                      
           03  FILLER                  PIC  X(01).                      
           03  AUX-DT-DB2-AAAA         PIC  9(04).                      
           03  FILLER REDEFINES AUX-DT-DB2-AAAA.                        
               05  AUX-DT-DB2-SEC      PIC  9(02).                      
               05  AUX-DT-DB2-ANO      PIC  9(02).                      
                                                                        
       01  WRK-CH-ARQDB2-ATU.                                           
           03  WRK-CGCCPF-NUM-ATU      PIC 9(09) VALUE ZEROS.           
           03  WRK-CGCCPF-FIL-ATU      PIC 9(04) VALUE ZEROS.           
           03  WRK-CGCCPF-CTR-ATU      PIC 9(02) VALUE ZEROS.           
           03  WRK-AGENCIA-ATU         PIC 9(05) VALUE ZEROS.           
           03  WRK-CONTA-ATU           PIC 9(07) VALUE ZEROS.           
           03  WRK-NATUREZA-ATU        PIC X(03) VALUE SPACES.          
                                                                        
       01  WRK-CH-ARQDB2-ANT.                                           
           03  WRK-CGCCPF-NUM-ANT      PIC 9(09) VALUE ZEROS.           
           03  WRK-CGCCPF-FIL-ANT      PIC 9(04) VALUE ZEROS.           
           03  WRK-CGCCPF-CTR-ANT      PIC 9(02) VALUE ZEROS.           
           03  WRK-AGENCIA-ANT         PIC 9(05) VALUE ZEROS.           
           03  WRK-CONTA-ANT           PIC 9(07) VALUE ZEROS.           
           03  WRK-NATUREZA-ANT        PIC X(03) VALUE SPACES.          
                                                                        
      *---------------------------------------------------------------* 
      *                 DEFINICAO DO RELATORIO DE TOTAIS              * 
      *---------------------------------------------------------------* 
                                                                        
       01  CABEC1.                                                      
           03  CB1-CARRO               PIC X(01) VALUE '1'.             
           03  FILLER                  PIC X(28) VALUE 'CLLP7108'.      
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
                                                                        
       01  CABEC3.                                                      
           03  CB2-CARRO               PIC X(01) VALUE '0'.             
           03  FILLER                  PIC X(17) VALUE SPACES.          
           03  FILLER                  PIC X(37) VALUE                  
               'DATA PARAMETRO P/ NATUREZA = CARTAO '.                  
           03  FILLER.                                                  
               05  CB2-DTMV35-DD         PIC 9(2)/.                     
               05  CB2-DTMV35-MM         PIC 9(2)/.                     
               05  CB2-DTMV35-AAAA       PIC 9(4).                      
           03  FILLER                  PIC X(15) VALUE SPACES.          
                                                                        
                                                                        
       01  LINSPACE.                                                    
           03  LP1-CARRO               PIC X(01) VALUE '-'.             
           03  FILLER                  PIC X(79) VALUE SPACES.          
                                                                        
                                                                        
       01  LINDET1.                                                     
           03  LD1-CARRO               PIC X(01) VALUE ' '.             
           03  FILLER                  PIC X(02) VALUE SPACES.          
           03  LD1-CGC-DEV             PIC 999.999.999.                 
           03  FILLER                  PIC X(01) VALUE '/'.             
           03  LD1-FIL-DEV             PIC 9999.                        
           03  FILLER                  PIC X(01) VALUE '-'.             
           03  LD1-CTR-DEV             PIC 99.                          
           03  FILLER                  PIC X(02) VALUE SPACES.          
           03  LD1-CGC-AVA1            PIC 999.999.999.                 
           03  FILLER                  PIC X(01) VALUE '/'.             
           03  LD1-FIL-AVA1            PIC 9999.                        
           03  FILLER                  PIC X(01) VALUE '-'.             
           03  LD1-CTR-AVA1            PIC 99.                          
           03  FILLER                  PIC X(02) VALUE SPACES.          
           03  LD1-CGC-AVA2            PIC 999.999.999.                 
           03  FILLER                  PIC X(01) VALUE '/'.             
           03  LD1-FIL-AVA2            PIC 9999.                        
           03  FILLER                  PIC X(01) VALUE '-'.             
           03  LD1-CTR-AVA2            PIC 99.                          
           03  FILLER                  PIC X(02) VALUE SPACES.          
           03  LD1-AGENCIA             PIC ZZZZ9.                       
           03  FILLER                  PIC X(02) VALUE SPACES.          
           03  LD1-CONTA               PIC ZZZZZZ9.                     
           03  FILLER                  PIC X(02) VALUE SPACES.          
           03  LD1-CONTRATO            PIC ZZZZZZ9.                     
           03  FILLER                  PIC X(02) VALUE SPACES.          
           03  LD1-VCTO                PIC 9999.99.99.                  
                                                                        
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
               'REGISTROS GRAVADOS - ARQLPCL  = '.                      
           03  LT2-GRAVA-ARQLPCL       PIC ZZ.ZZZ.ZZ9.                  
                                                                        
       01  LINTOT3.                                                     
           03  LT3-CARRO               PIC X(01) VALUE '0'.             
           03  FILLER                  PIC X(17) VALUE SPACES.          
           03  FILLER                  PIC X(32) VALUE                  
               'REGISTROS GRAVADOS - ARQINCON = '.                      
           03  LT3-GRAVA-ARQINCON      PIC ZZ.ZZZ.ZZ9.                  
                                                                        
       01  LINTOT4.                                                     
           03  LT4-CARRO               PIC X(01) VALUE '0'.             
           03  FILLER                  PIC X(17) VALUE SPACES.          
           03  FILLER                  PIC X(32) VALUE                  
               'REGISTROS DESPREZADO - AVAL   = '.                      
           03  LT4-DESPR-AVALISTA      PIC ZZ.ZZZ.ZZ9.                  
                                                                        
       01  LINTOT5.                                                     
           03  LT5-CARRO               PIC X(01) VALUE '0'.             
           03  FILLER                  PIC X(17) VALUE SPACES.          
           03  FILLER                  PIC X(32) VALUE                  
               'REGISTROS DESPREZADO - AVAL   = '.                      
           03  LT5-DESPR-DATA          PIC ZZ.ZZZ.ZZ9.                  
                                                                        
BRQ141*****-INC I#CLLPCJ - SUBSTITUIDO PELO BOOK I#CLLP26               
-INC I#CLLP26                                                           
                                                                        
                                                                        
       01  FILLER                      PIC X(32)        VALUE           
           '*  FIM DA WORKING CLLP7108 *'.                              
           EJECT                                                        
                                                                        
      *===============================================================* 
       PROCEDURE DIVISION.                                              
      *===============================================================* 
                                                                        
      *---------------------------------------------------------------* 
       0000-INICIAR SECTION.                                            
      *---------------------------------------------------------------* 
                                                                        
           OPEN INPUT   ARQDB2                                          
                        PARMCLLP                                        
                        ARQDATA                                         
                OUTPUT  TOTAIS                                          
                        ARQLPCL                                         
                        ARQINCON.                                       
                                                                        
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
                                                                        
           CLOSE  ARQDB2                                                
                  TOTAIS                                                
                  ARQLPCL                                               
                  ARQINCON                                              
                  ARQDATA                                               
                  PARMCLLP.                                             
                                                                        
           MOVE    WRK-FECHAMENTO          TO   WRK-OPERACAO.           
           PERFORM 1000-TESTAR-FILE-STATUS.                             
                                                                        
           GOBACK.                                                      
                                                                        
      *---------------------------------------------------------------* 
       0000-99-FIM. EXIT.                                               
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       1000-TESTAR-FILE-STATUS SECTION.                                 
      *---------------------------------------------------------------* 
                                                                        
           PERFORM 1100-TESTAR-FS-ARQDB2.                               
                                                                        
           PERFORM 1200-TESTAR-FS-TOTAIS.                               
                                                                        
           PERFORM 1300-TESTAR-FS-ARQLPCL.                              
                                                                        
           PERFORM 1400-TESTAR-FS-ARQINCON.                             
                                                                        
           PERFORM 1500-TESTAR-FS-PARMCLLP.                             
                                                                        
           PERFORM 1600-TESTAR-FS-ARQDATA.                              
                                                                        
      *---------------------------------------------------------------* 
       1000-99-FIM. EXIT.                                               
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       1100-TESTAR-FS-ARQDB2  SECTION.                                  
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-FS-ARQDB2 NOT EQUAL '00'                              
              DISPLAY '************** CLLP7108 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              ARQDB2               *'           
              DISPLAY '*         FILE STATUS =  ' WRK-FS-ARQDB2         
                                                 '         *'           
              DISPLAY '************** CLLP7108 *************'           
              CALL 'ILBOABN0'     USING WRK-ABEND.                      
                                                                        
      *---------------------------------------------------------------* 
       1100-99-FIM. EXIT.                                               
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       1200-TESTAR-FS-TOTAIS   SECTION.                                 
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-FS-TOTAIS NOT EQUAL '00'                              
              DISPLAY '************** CLLP7108 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              TOTAIS               *'           
              DISPLAY '*         FILE STATUS =  ' WRK-FS-TOTAIS         
                                                 '         *'           
              DISPLAY '************** CLLP7108 *************'           
              CALL 'ILBOABN0'     USING WRK-ABEND.                      
                                                                        
      *---------------------------------------------------------------* 
       1200-99-FIM. EXIT.                                               
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       1300-TESTAR-FS-ARQLPCL  SECTION.                                 
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-FS-ARQLPCL NOT EQUAL '00'                             
              DISPLAY '************** CLLP7108 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              ARQLPCL              *'           
              DISPLAY '*         FILE STATUS =  ' WRK-FS-ARQLPCL        
                                                 '         *'           
              DISPLAY '************** CLLP7108 *************'           
              CALL 'ILBOABN0'     USING WRK-ABEND.                      
                                                                        
      *---------------------------------------------------------------* 
       1300-99-FIM. EXIT.                                               
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       1400-TESTAR-FS-ARQINCON SECTION.                                 
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-FS-ARQINCON NOT EQUAL '00'                            
              DISPLAY '************** CLLP7108 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              ARQINCON             *'           
              DISPLAY '*         FILE STATUS =  ' WRK-FS-ARQINCON       
                                                 '         *'           
              DISPLAY '************** CLLP7108 *************'           
              CALL 'ILBOABN0'     USING WRK-ABEND.                      
                                                                        
      *---------------------------------------------------------------* 
       1400-99-FIM. EXIT.                                               
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       1500-TESTAR-FS-PARMCLLP SECTION.                                 
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-FS-PARMCLLP NOT EQUAL '00'                            
              DISPLAY '************** CLLP7108 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              PARMCLLP             *'           
              DISPLAY '*         FILE STATUS =  ' WRK-FS-PARMCLLP       
                                                 '         *'           
              DISPLAY '************** CLLP7108 *************'           
              CALL 'ILBOABN0'     USING WRK-ABEND.                      
                                                                        
      *---------------------------------------------------------------* 
       1500-99-FIM. EXIT.                                               
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       1600-TESTAR-FS-ARQDATA  SECTION.                                 
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-FS-ARQDATA  NOT EQUAL '00'                            
              DISPLAY '************** CLLP7108 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              ARQDATA              *'           
              DISPLAY '*         FILE STATUS =  ' WRK-FS-ARQDATA        
                                                 '         *'           
              DISPLAY '************** CLLP7108 *************'           
              CALL 'ILBOABN0'     USING WRK-ABEND.                      
                                                                        
      *---------------------------------------------------------------* 
       1600-99-FIM. EXIT.                                               
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       2000-INICIALIZA SECTION.                                         
      *---------------------------------------------------------------* 
                                                                        
           PERFORM 2300-LER-ARQDATA.                                    
                                                                        
           IF  WRK-FS-ARQDATA    EQUAL   '10'                           
               DISPLAY '************** CLLP7108 *************'          
               DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'      
               DISPLAY '*              ARQDATA              *'          
               DISPLAY '*         FILE STATUS =  ' WRK-FS-ARQDATA       
                                                  '         *'          
               DISPLAY '************** CLLP7108 *************'          
               CALL 'ILBOABN0'     USING WRK-ABEND.                     
                                                                        
           PERFORM 2200-LER-PARMCLLP.                                   
                                                                        
           IF  WRK-FS-PARMCLLP   EQUAL   '10'                           
               DISPLAY '************** CLLP7108 *************'          
               DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'      
               DISPLAY '*              PARMCLLP             *'          
               DISPLAY '*         FILE STATUS =  ' WRK-FS-PARMCLLP      
                                                  '         *'          
               DISPLAY '************** CLLP7108 *************'          
               CALL 'ILBOABN0'     USING WRK-ABEND.                     
                                                                        
           MOVE  PARM-NR-DIAS    TO   WRK-DIAS5                         
           MOVE  PARM-VL-CORTE   TO   WRK-VL-CORTE5.                    
                                                                        
           CALL   'POOL7600' USING WRK-AREA-POOL7600.                   
                                                                        
           MOVE   WRK-DT-AAAAMMDD       TO  AUX-DT-INV-AAAAMMDD.        
           MOVE   AUX-DT-INV-DD         TO  CB2-DTSYS-DD.               
           MOVE   AUX-DT-INV-MM         TO  CB2-DTSYS-MM.               
           MOVE   AUX-DT-INV-AAAA       TO  CB2-DTSYS-AAAA.             
                                                                        
           MOVE   WRK-TI-HHMMSS         TO  WRK-TI-SYS.                 
           MOVE   WRK-TI-HOR            TO  CB2-TIME-HOR.               
           MOVE   WRK-TI-MIN            TO  CB2-TIME-MIN.               
           MOVE   WRK-TI-SEG            TO  CB2-TIME-SEG.               
                                                                        
           MOVE   WRK-DT-AAAAMMDD       TO  WRK-DT-ABERT.               
           MOVE   -1825                 TO  WRK-PRAZO.                  
                                                                        
           CALL 'POOL0025'      USING  WRK-DT-ABERT                     
                                       WRK-PRAZO                        
                                       WRK-DT-LIMI5ANOS                 
                                                                        
           IF RETURN-CODE NOT EQUAL ZEROS                               
              MOVE    RETURN-CODE   TO  WRK-RETURN-CODE                 
              DISPLAY '******************* CLLP7108 *******************'
              DISPLAY '*   ERRO  UTILIZANDO  A  POOL0025              *'
              DISPLAY '*                                              *'
              DISPLAY '*   DATA-ENTRADA (AAAAMMDD)    = '               
                            AUX-DT-INV-AAAAMMDD                '      *'
              DISPLAY '*   RETURN-CODE                = '               
                           WRK-RETURN-CODE                   '        *'
              DISPLAY '*                                              *'
              DISPLAY '******************* CLLP7108 *******************'
              CALL 'ILBOABN0'           USING WRK-ABEND.                
                                                                        
           MOVE   DTMOVINV              TO  WRK-DT-AVALISTA.            
                                                                        
           MOVE   WRK-DT-MOV-DD         TO  CB2-DTMV-DD.                
           MOVE   WRK-DT-MOV-MM         TO  CB2-DTMV-MM.                
           MOVE   WRK-DT-MOV-AAAA       TO  CB2-DTMV-AAAA.              
                                                                        
                                                                        
           MOVE    DTMOVINV          TO     WRK-DT-ENTRADA              
                                                                        
           COMPUTE WRK-NUM-DIAS      =    ( WRK-DIAS5 - 1)              
           COMPUTE WRK-NUM-DIAS      =    ( WRK-NUM-DIAS * -1)          
                                                                        
      *    MOVE WRK-DT-ENTRADA       TO  WRK-DATA-EDIT.                 
      *    MOVE WRK-NUM-DIAS         TO  WRK-DIAS-EDIT.                 
                                                                        
           DISPLAY 'DATA ENTRADA = ' WRK-DATA-EDIT.                     
           DISPLAY 'DIAS ENTRADA = ' WRK-DIAS-EDIT.                     
                                                                        
           CALL   'POOL1285' USING WRK-AREA-POOL1285.                   
                                                                        
           IF RETURN-CODE NOT EQUAL ZEROS                               
              MOVE    RETURN-CODE   TO  WRK-RETURN-CODE                 
              DISPLAY '******************* CLLP7108 *******************'
              DISPLAY '*   ERRO  UTILIZANDO  A  POOL1285    '           
              DISPLAY '*                                    '           
              DISPLAY '*   DATA-ENTRADA     = ' WRK-DT-ENTRADA          
              DISPLAY '*   RETURN-CODE      = ' WRK-RETURN-CODE         
              DISPLAY '*   MENSAGEM DO ERRO = ' WRK-MENSAGEM            
              DISPLAY '*                                    '           
              DISPLAY '******************* CLLP7108 *******************'
              MOVE  'S'  TO  WRK-FINALIZA                               
           ELSE                                                         
              MOVE   WRK-DT-SAIDA       TO  WRK-DT-AVALISTA.            
                                                                        
           MOVE WRK-DT-SAIDA         TO  WRK-DATA-EDIT.                 
                                                                        
      *    DISPLAY 'DATA SAIDA   = ' WRK-DATA-EDIT.                     
                                                                        
           PERFORM 2100-LER-ARQDB2.                                     
                                                                        
      *---------------------------------------------------------------* 
       2000-99-FIM. EXIT.                                               
      *---------------------------------------------------------------* 
           EJECT                                                        
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
                                                                        
           MOVE DB2-NUMERO-DEV          TO  WRK-CGCCPF-NUM-ATU          
           MOVE DB2-FILIAL              TO  WRK-CGCCPF-FIL-ATU          
           MOVE DB2-CTR-DEV             TO  WRK-CGCCPF-CTR-ATU          
           MOVE DB2-COD-NATUREZA-OPER   TO  WRK-NATUREZA-ATU            
           MOVE DB2-AGENCIA             TO  WRK-AGENCIA-ATU             
           MOVE DB2-CONTA               TO  WRK-CONTA-ATU               
                                                                        
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
                                                                        
      *---------------------------------------------------------------* 
       2100-99-FIM. EXIT.                                               
      *---------------------------------------------------------------* 
           EJECT                                                        
                                                                        
      *---------------------------------------------------------------* 
       2200-LER-PARMCLLP             SECTION.                           
      *---------------------------------------------------------------* 
                                                                        
           READ   PARMCLLP.                                             
                                                                        
           IF  WRK-FS-PARMCLLP   EQUAL   '10'                           
               GO                TO      2200-99-FIM.                   
                                                                        
           MOVE    WRK-LEITURA             TO   WRK-OPERACAO.           
           PERFORM 1500-TESTAR-FS-PARMCLLP.                             
                                                                        
      *    DISPLAY 'PARM-CCUSTO-1 = '  PARM-CCUSTO-1.                   
      *    DISPLAY 'PARM-CODIGO-1 = '  PARM-CODIGO-1.                   
      *    DISPLAY 'PARM-CCUSTO-2 = '  PARM-CCUSTO-2.                   
      *    DISPLAY 'PARM-CODIGO-2 = '  PARM-CODIGO-2.                   
                                                                        
           IF  PARM-CCUSTO-1   NOT EQUAL   'CLLP'   OR                  
               PARM-CODIGO-1   NOT EQUAL   '0002'   OR                  
               PARM-CCUSTO-2   NOT EQUAL   'CLLP'   OR                  
               PARM-CODIGO-2   NOT EQUAL   '7615'                       
               DISPLAY '****************  CLLP7108  *******************'
               DISPLAY '*           ABEND 1111 - FORCADO              *'
               DISPLAY '*  PROGRAMA CANCELADO - PARAMETRO INVALIDO    *'
               DISPLAY '* '  PARM-CCUSTO-1 ' / ' PARM-CODIGO-1          
                       ' / ' PARM-CCUSTO-2 ' / ' PARM-CODIGO-2          
               DISPLAY '*   AVISAR ANALISTA RESPONSAVEL DA ROTINA     *'
               DISPLAY '*****************  CLLP7108  ******************'
               CALL  'ILBOABN0'  USING  WRK-ABEND.                      
                                                                        
           IF  PARM-SEQUENCIA   NOT EQUAL   5                           
               GO               TO   2200-LER-PARMCLLP.                 
                                                                        
      *---------------------------------------------------------------* 
       2200-99-FIM.                  EXIT.                              
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       2300-LER-ARQDATA              SECTION.                           
      *---------------------------------------------------------------* 
                                                                        
           READ   ARQDATA.                                              
                                                                        
           IF  WRK-FS-ARQDATA    EQUAL   '10'                           
               GO                TO      2300-99-FIM.                   
                                                                        
           MOVE    WRK-LEITURA             TO   WRK-OPERACAO.           
           PERFORM 1600-TESTAR-FS-ARQDATA.                              
                                                                        
                                                                        
      *---------------------------------------------------------------* 
       2300-99-FIM.                  EXIT.                              
      *---------------------------------------------------------------* 
                                                                        
                                                                        
      *---------------------------------------------------------------* 
       3000-PROCESSA-ARQDB2    SECTION.                                 
      *---------------------------------------------------------------* 
                                                                        
           IF  DB2-CODIGO EQUAL 98  OR  22                              
               MOVE 10 TO INCON-COD-ERRO                                
               PERFORM 3100-GERAR-ARQINCON                              
               GO TO 3000-99-FIM.                                       
                                                                        
           IF  DB2-CODIGO EQUAL 97                                      
               MOVE 13 TO INCON-COD-ERRO                                
               PERFORM 3100-GERAR-ARQINCON                              
               GO TO 3000-99-FIM.                                       
                                                                        
           IF  DB2-TIPO-PEND EQUAL 4444                                 
               MOVE 02 TO INCON-COD-ERRO                                
               PERFORM 3100-GERAR-ARQINCON                              
               GO TO 3000-99-FIM.                                       
                                                                        
           IF  DB2-COD-NATUREZA-OPER EQUAL '32'                         
               MOVE 03 TO INCON-COD-ERRO                                
               PERFORM 3100-GERAR-ARQINCON                              
               GO TO 3000-99-FIM.                                       
                                                                        
           IF  DB2-CARTEIRA EQUAL '601' OR '702' OR                     
                                  '278' OR '084' OR                     
                                  '085'                                 
               MOVE 04 TO INCON-COD-ERRO                                
               PERFORM 3100-GERAR-ARQINCON                              
               GO TO 3000-99-FIM.                                       
                                                                        
           IF  (DB2-CARTEIRA EQUAL '281' OR '294')  AND                 
                DB2-CODIGO   EQUAL  37                                  
               MOVE 04 TO INCON-COD-ERRO                                
               PERFORM 3100-GERAR-ARQINCON                              
               GO TO 3000-99-FIM.                                       
                                                                        
           IF  (DB2-EMPRESA EQUAL 6500 OR 6699) AND                     
               (DB2-FILIAL  GREATER ZEROS)                              
               MOVE 05 TO INCON-COD-ERRO                                
               PERFORM 3100-GERAR-ARQINCON                              
               GO TO 3000-99-FIM.                                       
                                                                        
CPM*******************************************************************  
CPM********      ALTERACAO CONFORME S.S. 390.050             *********  
CPM******** LOCALIZAR ENDERECO DE AVALISTAS E ENVIAR AVISOS  *********  
CPM*******************************************************************  
CPM********IF  DB2-AGENCIA GREATER 3999                                 
CPM********    MOVE 16 TO INCON-COD-ERRO                                
CPM********    PERFORM 3100-GERAR-ARQINCON                              
CPM********    GO TO 3000-99-FIM.                                       
                                                                        
           IF  DB2-NUMERO-DEV EQUAL ZEROS                               
               MOVE 06 TO INCON-COD-ERRO                                
               PERFORM 3100-GERAR-ARQINCON                              
               GO TO 3000-99-FIM.                                       
                                                                        
           MOVE  DB2-NUMERO-DEV        TO WRK-NUMERO-0110.              
           MOVE  DB2-FILIAL            TO WRK-FILIAL-0110.              
           MOVE  SPACES                TO WRK-CONTROLE-0110.            
                                                                        
           CALL 'POOL0110'     USING   WRK-POOL0110                     
                                       WRK-CONTROLE-0110.               
                                                                        
           IF  RETURN-CODE NOT EQUAL ZEROS                              
               MOVE 07 TO INCON-COD-ERRO                                
               PERFORM 3100-GERAR-ARQINCON                              
               GO TO 3000-99-FIM.                                       
                                                                        
           IF  (WRK-CONTROLE-0110 IS NOT NUMERIC)  OR                   
               (WRK-CONTROLE-0110 NOT EQUAL DB2-CTR-DEV)                
               MOVE 07 TO INCON-COD-ERRO                                
               PERFORM 3100-GERAR-ARQINCON                              
               GO TO 3000-99-FIM.                                       
                                                                        
           IF  (DB2-NOME-20 EQUAL SPACES)  OR                           
               (DB2-NOME-20 EQUAL ALL '*')                              
               MOVE 08 TO INCON-COD-ERRO                                
               PERFORM 3100-GERAR-ARQINCON                              
               GO TO 3000-99-FIM.                                       
                                                                        
           IF  DB2-VCTO     LESS  WRK-DT-LIMI5ANOS                      
               MOVE 09 TO INCON-COD-ERRO                                
               PERFORM 3100-GERAR-ARQINCON                              
               GO TO 3000-99-FIM.                                       
                                                                        
           IF  DB2-NUMERO-DEV   EQUAL DB2-NUMERO-AVAL                   
               MOVE 11 TO INCON-COD-ERRO                                
               PERFORM 3150-GERAR-ARQINCON-AVALISTAS                    
               MOVE    SPACES   TO     DB2-NOME-AVALISTA                
               MOVE    ZEROS    TO     DB2-NUMERO-AVAL                  
               MOVE    ZEROS    TO     DB2-FILIAL-AVAL                  
               MOVE    ZEROS    TO     DB2-CTR-AVAL.                    
                                                                        
           IF  DB2-NUMERO-AVAL2  NOT EQUAL  ZEROS                       
               IF (DB2-NUMERO-DEV    EQUAL  DB2-NUMERO-AVAL2)    OR     
                  (DB2-NUMERO-AVAL   EQUAL  DB2-NUMERO-AVAL2)           
                   MOVE 12 TO INCON-COD-ERRO                            
                   PERFORM 3150-GERAR-ARQINCON-AVALISTAS                
                   MOVE    SPACES   TO     DB2-NOME-AVAL2               
                   MOVE    ZEROS    TO     DB2-NUMERO-AVAL2             
                   MOVE    ZEROS    TO     DB2-FILIAL-AVAL2             
                   MOVE    ZEROS    TO     DB2-CTR-AVAL2.               
                                                                        
           IF    DB2-NUMERO-AVAL            GREATER ZEROS               
                 MOVE  DB2-NUMERO-AVAL      TO WRK-NUMERO-0110          
                 MOVE  DB2-FILIAL-AVAL      TO WRK-FILIAL-0110          
                 MOVE  SPACES               TO WRK-CONTROLE-0110        
                                                                        
                 CALL 'POOL0110'     USING   WRK-POOL0110               
                                             WRK-CONTROLE-0110          
                                                                        
                 IF (RETURN-CODE NOT EQUAL ZEROS) OR                    
                    (WRK-CONTROLE-0110 IS NOT NUMERIC)  OR              
                    (WRK-CONTROLE-0110 NOT EQUAL DB2-CTR-AVAL)          
                     MOVE 14 TO INCON-COD-ERRO                          
                     PERFORM 3150-GERAR-ARQINCON-AVALISTAS              
                     MOVE    SPACES   TO     DB2-NOME-AVALISTA          
                     MOVE    ZEROS    TO     DB2-NUMERO-AVAL            
                     MOVE    ZEROS    TO     DB2-FILIAL-AVAL            
                     MOVE    ZEROS    TO     DB2-CTR-AVAL.              
                                                                        
                                                                        
           IF    DB2-NUMERO-AVAL2           GREATER ZEROS               
                 MOVE  DB2-NUMERO-AVAL2     TO WRK-NUMERO-0110          
                 MOVE  DB2-FILIAL-AVAL2     TO WRK-FILIAL-0110          
                 MOVE  SPACES               TO WRK-CONTROLE-0110        
                                                                        
                 CALL 'POOL0110'     USING   WRK-POOL0110               
                                             WRK-CONTROLE-0110          
                                                                        
                 IF (RETURN-CODE NOT EQUAL ZEROS) OR                    
                    (WRK-CONTROLE-0110 IS NOT NUMERIC)  OR              
                    (WRK-CONTROLE-0110 NOT EQUAL DB2-CTR-AVAL2)         
                     MOVE 15 TO INCON-COD-ERRO                          
                     PERFORM 3150-GERAR-ARQINCON-AVALISTAS              
                     MOVE    SPACES   TO     DB2-NOME-AVAL2             
                     MOVE    ZEROS    TO     DB2-NUMERO-AVAL2           
                     MOVE    ZEROS    TO     DB2-FILIAL-AVAL2           
                     MOVE    ZEROS    TO     DB2-CTR-AVAL2.             
                                                                        
***********PERFORM 3140-ZERAR-AVALISTAS.                                
                                                                        
           IF  DB2-NUMERO-AVAL         EQUAL ZEROS AND                  
               DB2-NUMERO-AVAL2        EQUAL ZEROS                      
               PERFORM 2100-LER-ARQDB2                                  
               GO TO 3000-99-FIM.                                       
                                                                        
           MOVE WRK-CGCCPF-NUM-ATU     TO WRK-CGCCPF-NUM-ANT            
           MOVE WRK-CGCCPF-FIL-ATU     TO WRK-CGCCPF-FIL-ANT            
           MOVE WRK-CGCCPF-CTR-ATU     TO WRK-CGCCPF-CTR-ANT            
           MOVE WRK-NATUREZA-ATU       TO WRK-NATUREZA-ANT.             
           MOVE WRK-AGENCIA-ATU        TO WRK-AGENCIA-ANT.              
           MOVE WRK-CONTA-ATU          TO WRK-CONTA-ANT.                
                                                                        
           PERFORM 3200-MOVER-DADOS-ARQLPCL.                            
                                                                        
      *---------------------------------------------------------------* 
       3000-99-FIM.  EXIT.                                              
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       3100-GERAR-ARQINCON  SECTION.                                    
      *---------------------------------------------------------------* 
                                                                        
           PERFORM 3110-MOVER-DADOS-INCONSIS.                           
                                                                        
           MOVE WRK-CGCCPF-NUM-ATU     TO WRK-CGCCPF-NUM-ANT            
           MOVE WRK-CGCCPF-FIL-ATU     TO WRK-CGCCPF-FIL-ANT            
           MOVE WRK-CGCCPF-CTR-ATU     TO WRK-CGCCPF-CTR-ANT            
           MOVE WRK-NATUREZA-ATU       TO WRK-NATUREZA-ANT.             
           MOVE WRK-AGENCIA-ATU        TO WRK-AGENCIA-ANT.              
           MOVE WRK-CONTA-ATU          TO WRK-CONTA-ANT.                
                                                                        
           PERFORM 3130-ACUMULAR-VALORES                                
                   UNTIL WRK-CH-ARQDB2-ATU NOT EQUAL                    
                         WRK-CH-ARQDB2-ANT OR                           
                         WRK-FIM-ARQDB2 EQUAL 'S'.                      
                                                                        
           MOVE ACU-VAL-RESG TO INCON-TOT-VR-RESGATE.                   
           MOVE ACU-ENC-VENC TO INCON-TOT-VR-VENCIDOS.                  
           MOVE ACU-ENC-VINC TO INCON-TOT-VR-VINCENDOS.                 
                                                                        
           PERFORM 3120-GRAVAR-ARQINCON.                                
           MOVE ZEROS                  TO ACU-VAL-RESG                  
                                          ACU-ENC-VENC                  
                                          ACU-ENC-VINC.                 
                                                                        
      *---------------------------------------------------------------* 
       3100-99-FIM. EXIT.                                               
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       3110-MOVER-DADOS-INCONSIS SECTION.                               
      *---------------------------------------------------------------* 
                                                                        
           MOVE DB2-AGENCIA            TO INCON-AGENCIA.                
           MOVE DB2-CONTA              TO INCON-CONTA.                  
                                                                        
           IF DB2-COD-NATUREZA-OPER    IS NUMERIC                       
              MOVE DB2-COD-NATUREZA-OPER                                
                                       TO INCON-COD-NATUREZA-OPER       
           ELSE                                                         
              MOVE ZEROS               TO INCON-COD-NATUREZA-OPER.      
                                                                        
           MOVE DB2-CONTRATO           TO INCON-CONTRATO.               
           MOVE DB2-EMPRESA            TO INCON-EMPRESA.                
                                                                        
BRQ141*    IF DB2-CARTEIRA             IS NUMERIC                       
BRQ141     IF DB2-CARTEIRA             NOT EQUAL SPACES AND             
BRQ141        DB2-CARTEIRA             NOT EQUAL LOW-VALUES             
              MOVE DB2-CARTEIRA        TO INCON-CARTEIRA                
           ELSE                                                         
              MOVE ZEROS               TO INCON-CARTEIRA.               
                                                                        
           MOVE DB2-VCTO               TO INCON-VCTO.                   
           MOVE DB2-ID                 TO INCON-ID.                     
           MOVE DB2-MOEDA              TO INCON-MOEDA.                  
           MOVE DB2-TIPO-PEND          TO INCON-TIPO-PEND.              
           MOVE DB2-CODIGO             TO INCON-COD-OCORR.              
           MOVE DB2-NOME-DEVEDOR       TO INCON-NOME-DEVEDOR.           
           MOVE DB2-NUMERO-DEV         TO INCON-NUMERO-DEV.             
           MOVE DB2-FILIAL             TO INCON-FILIAL.                 
           MOVE DB2-CTR-DEV            TO INCON-CTR-DEV.                
           MOVE DB2-NOME-AVALISTA      TO INCON-NOME-AVALISTA.          
           MOVE DB2-NUMERO-AVAL        TO INCON-NUMERO-AVAL.            
           MOVE DB2-FILIAL-AVAL        TO INCON-FILIAL-AVAL.            
           MOVE DB2-CTR-AVAL           TO INCON-CTR-AVAL.               
           MOVE DB2-COD-EMPR           TO INCON-COD-EMPR.               
STF        MOVE DB2-NOME-AVAL2         TO INCON-NOME-AVAL2.             
STF        MOVE DB2-NUMERO-AVAL2       TO INCON-NUMERO-AVAL2.           
STF        MOVE DB2-FILIAL-AVAL2       TO INCON-FILIAL-AVAL2.           
STF        MOVE DB2-CTR-AVAL2          TO INCON-CTR-AVAL2.              
                                                                        
      *---------------------------------------------------------------* 
       3110-99-FIM. EXIT.                                               
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       3120-GRAVAR-ARQINCON SECTION.                                    
      *---------------------------------------------------------------* 
                                                                        
           MOVE WRK-GRAVACAO TO WRK-OPERACAO.                           
                                                                        
           WRITE INCON-REGTO.                                           
           PERFORM 1400-TESTAR-FS-ARQINCON.                             
                                                                        
           ADD 1 TO ACU-GRAVA-ARQINCON.                                 
                                                                        
      *---------------------------------------------------------------* 
       3120-99-FIM. EXIT.                                               
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       3130-ACUMULAR-VALORES SECTION.                                   
      *---------------------------------------------------------------* 
                                                                        
            ADD DB2-PRINCIPAL       TO ACU-VAL-RESG                     
            PERFORM 2100-LER-ARQDB2.                                    
                                                                        
      *---------------------------------------------------------------* 
       3130-99-FIM. EXIT.                                               
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       3140-ZERAR-AVALISTAS  SECTION.                                   
      *---------------------------------------------------------------* 
                                                                        
           IF  DB2-NUMERO-DEV   EQUAL  DB2-NUMERO-AVAL                  
               MOVE    SPACES   TO     DB2-NOME-AVALISTA                
               MOVE    ZEROS    TO     DB2-NUMERO-AVAL                  
               MOVE    ZEROS    TO     DB2-FILIAL-AVAL                  
               MOVE    ZEROS    TO     DB2-CTR-AVAL.                    
                                                                        
           IF  DB2-NUMERO-AVAL2 EQUAL  ZEROS                            
               GO   TO   3140-99-FIM.                                   
                                                                        
           IF  DB2-NUMERO-DEV   EQUAL  DB2-NUMERO-AVAL2                 
               MOVE    SPACES   TO     DB2-NOME-AVAL2                   
               MOVE    ZEROS    TO     DB2-NUMERO-AVAL2                 
               MOVE    ZEROS    TO     DB2-FILIAL-AVAL2                 
               MOVE    ZEROS    TO     DB2-CTR-AVAL2.                   
                                                                        
           IF  DB2-NUMERO-AVAL  EQUAL  DB2-NUMERO-AVAL2                 
               MOVE    SPACES   TO     DB2-NOME-AVAL2                   
               MOVE    ZEROS    TO     DB2-NUMERO-AVAL2                 
               MOVE    ZEROS    TO     DB2-FILIAL-AVAL2                 
               MOVE    ZEROS    TO     DB2-CTR-AVAL2.                   
                                                                        
      *---------------------------------------------------------------* 
       3140-99-FIM. EXIT.                                               
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       3150-GERAR-ARQINCON-AVALISTAS  SECTION.                          
      *---------------------------------------------------------------* 
                                                                        
           PERFORM 3110-MOVER-DADOS-INCONSIS.                           
                                                                        
           MOVE ZEROS        TO INCON-TOT-VR-RESGATE.                   
           MOVE ZEROS        TO INCON-TOT-VR-VENCIDOS.                  
           MOVE ZEROS        TO INCON-TOT-VR-VINCENDOS.                 
                                                                        
           PERFORM 3120-GRAVAR-ARQINCON.                                
           MOVE ZEROS                  TO ACU-VAL-RESG                  
                                          ACU-ENC-VENC                  
                                          ACU-ENC-VINC.                 
                                                                        
      *---------------------------------------------------------------* 
       3150-99-FIM. EXIT.                                               
      *---------------------------------------------------------------* 
      *---------------------------------------------------------------* 
       3200-MOVER-DADOS-ARQLPCL SECTION.                                
      *---------------------------------------------------------------* 
                                                                        
           MOVE SPACES                 TO REG-WOR.                      
           INITIALIZE REG-WOR.                                          
                                                                        
           MOVE DB2-AGENCIA            TO AGENCIA-WOR.                  
           MOVE DB2-CONTA              TO CONTA-WOR.                    
                                                                        
           IF DB2-COD-NATUREZA-OPER IS NOT NUMERIC                      
              MOVE ZEROS               TO COD-NATUREZA-WOR              
           ELSE                                                         
              MOVE DB2-COD-NATUREZA-OPER                                
                                       TO COD-NATUREZA-WOR.             
                                                                        
           MOVE DB2-CONTRATO           TO CONTRATO-WOR.                 
           MOVE DB2-EMPRESA            TO EMPRESA-WOR.                  
                                                                        
BRQ141*    IF DB2-CARTEIRA          IS NOT NUMERIC                      
BRQ141     IF DB2-CARTEIRA          EQUAL  SPACES OR                    
BRQ141        DB2-CARTEIRA          EQUAL  LOW-VALUES                   
              MOVE ZEROS               TO CARTEIRA-WOR                  
           ELSE                                                         
              MOVE DB2-CARTEIRA        TO CARTEIRA-WOR.                 
                                                                        
           MOVE DB2-VCTO               TO DAT-VENCTO-WOR.               
           MOVE DB2-ID                 TO CAD-IDENT-WOR.                
           MOVE DB2-MOEDA              TO MOEDA-WOR.                    
           MOVE DB2-NOME-DEVEDOR       TO NOME-CAD-WOR.                 
           MOVE DB2-NUMERO-DEV         TO NUM-CGC-CAD-WOR               
                                          CGCCPF-NUM-WOR                
                                          CGC-NRO-WOR.                  
           MOVE DB2-FILIAL             TO FIL-CGC-CAD-WOR               
                                          CGCCPF-FIL-WOR                
                                          CGC-FIL-WOR.                  
           MOVE DB2-CTR-DEV            TO CONTROLE-CAD-WOR              
                                          CGCCPF-CTR-WOR                
                                          CGC-CTR-WOR.                  
           MOVE DB2-PRINCIPAL          TO VAL-RESG-WOR                  
           MOVE DB2-VR-VENCIDOS        TO VAL-ENC-VEN-WOR               
           MOVE DB2-VR-VINCENDOS       TO VAL-ENC-VIN-WOR.              
                                                                        
           IF   DB2-NUMERO-AVAL        GREATER ZEROS                    
                MOVE DB2-NOME-AVALISTA TO CAD-NOME1-WOR                 
                MOVE DB2-NUMERO-AVAL   TO CAD-NCGC1-WOR                 
                MOVE DB2-FILIAL-AVAL   TO CAD-FIL1-WOR                  
                MOVE DB2-CTR-AVAL      TO CAD-CTR1-WOR                  
                MOVE SPACES            TO CAD-NOME2-WOR                 
                MOVE ZEROS             TO CAD-NCGC2-WOR                 
                                          CAD-FIL2-WOR                  
                                          CAD-CTR2-WOR                  
                PERFORM 3300-GRAVAR-ARQLPCL.                            
                                                                        
           IF   DB2-NUMERO-AVAL2       GREATER ZEROS                    
                MOVE DB2-NOME-AVAL2    TO CAD-NOME1-WOR                 
                MOVE DB2-NUMERO-AVAL2  TO CAD-NCGC1-WOR                 
                MOVE DB2-FILIAL-AVAL2  TO CAD-FIL1-WOR                  
                MOVE DB2-CTR-AVAL2     TO CAD-CTR1-WOR                  
                MOVE SPACES            TO CAD-NOME2-WOR                 
                MOVE ZEROS             TO CAD-NCGC2-WOR                 
                                          CAD-FIL2-WOR                  
                                          CAD-CTR2-WOR                  
                PERFORM 3300-GRAVAR-ARQLPCL.                            
                                                                        
           PERFORM 2100-LER-ARQDB2.                                     
                                                                        
      *---------------------------------------------------------------* 
       3200-99-FIM.    EXIT.                                            
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       3300-GRAVAR-ARQLPCL SECTION.                                     
      *---------------------------------------------------------------* 
                                                                        
           IF CAD-NCGC1-WOR            EQUAL ZEROS                      
              ADD 1                    TO  ACU-DESPREZA-AVAL            
              GO                       TO  3300-99-FIM.                 
                                                                        
           IF (DB2-AGENCIA             EQUAL 2905 OR 2397) AND          
              (DB2-CONTA               EQUAL 5481 OR 17120)             
               MOVE DB2-NUMERO-DEV     TO   LD1-CGC-DEV                 
               MOVE DB2-FILIAL         TO   LD1-FIL-DEV                 
               MOVE DB2-CTR-DEV        TO   LD1-CTR-DEV                 
               MOVE DB2-AGENCIA        TO   LD1-AGENCIA                 
               MOVE DB2-CONTA          TO   LD1-CONTA                   
               MOVE DB2-CONTRATO       TO   LD1-CONTRATO                
               MOVE DB2-VCTO           TO   LD1-VCTO                    
               MOVE CAD-NCGC1-WOR      TO   LD1-CGC-AVA1                
               MOVE CAD-FIL1-WOR       TO   LD1-FIL-AVA1                
               MOVE CAD-CTR1-WOR       TO   LD1-CTR-AVA1                
               MOVE ZEROS              TO   LD1-CGC-AVA2                
               MOVE ZEROS              TO   LD1-FIL-AVA2                
               MOVE ZEROS              TO   LD1-CTR-AVA2                
               MOVE WRK-GRAVACAO       TO   WRK-OPERACAO                
               WRITE REG-TOTAIS        FROM LINDET1                     
               PERFORM 1200-TESTAR-FS-TOTAIS.                           
                                                                        
           WRITE REG-ARQLPCL           FROM REG-WOR.                    
                                                                        
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.                 
           PERFORM 1300-TESTAR-FS-ARQLPCL.                              
                                                                        
           ADD   1                     TO  ACU-GRAVA-ARQLPCL.           
                                                                        
      *---------------------------------------------------------------* 
       3300-99-FIM.    EXIT.                                            
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       4000-IMPRIME-TOTAIS SECTION.                                     
      *---------------------------------------------------------------* 
                                                                        
           MOVE ACU-LIDOS-ARQDB2           TO   LT1-LIDOS-ARQDB2.       
           MOVE ACU-GRAVA-ARQLPCL          TO   LT2-GRAVA-ARQLPCL.      
           MOVE ACU-GRAVA-ARQINCON         TO   LT3-GRAVA-ARQINCON.     
           MOVE ACU-DESPREZA-AVAL          TO   LT4-DESPR-AVALISTA.     
           MOVE ACU-DESPREZA-DATA          TO   LT5-DESPR-DATA.         
           MOVE WRK-GRAVACAO               TO   WRK-OPERACAO.           
                                                                        
           MOVE 1                          TO   CB1-PAG.                
                                                                        
           WRITE REG-TOTAIS                FROM CABEC1.                 
           PERFORM 1200-TESTAR-FS-TOTAIS.                               
                                                                        
           WRITE REG-TOTAIS                FROM CABEC2.                 
           PERFORM 1200-TESTAR-FS-TOTAIS.                               
                                                                        
           WRITE REG-TOTAIS                FROM CABEC3.                 
           PERFORM 1200-TESTAR-FS-TOTAIS.                               
                                                                        
           WRITE REG-TOTAIS                FROM LINSPACE.               
           PERFORM 1200-TESTAR-FS-TOTAIS.                               
                                                                        
           WRITE REG-TOTAIS                FROM LINTOT1.                
           PERFORM 1200-TESTAR-FS-TOTAIS.                               
                                                                        
           WRITE REG-TOTAIS                FROM LINTOT5.                
           PERFORM 1200-TESTAR-FS-TOTAIS.                               
                                                                        
           WRITE REG-TOTAIS                FROM LINTOT2.                
           PERFORM 1200-TESTAR-FS-TOTAIS.                               
                                                                        
           WRITE REG-TOTAIS                FROM LINTOT3.                
           PERFORM 1200-TESTAR-FS-TOTAIS.                               
                                                                        
           WRITE REG-TOTAIS                FROM LINTOT4.                
           PERFORM 1200-TESTAR-FS-TOTAIS.                               
                                                                        
      *---------------------------------------------------------------* 
       4000-99-FIM. EXIT.                                               
      *---------------------------------------------------------------* 
                                                                        
                                                                        

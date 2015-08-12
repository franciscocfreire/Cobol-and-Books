      *===============================================================* 
       IDENTIFICATION DIVISION.                                         
      *===============================================================* 
       PROGRAM-ID. CLLPG637.                                            
       AUTHOR.     ARTHUR EPISCOPO.                                     
      *===============================================================* 
      *                   C P M   S I S T E M A S                     * 
      *---------------------------------------------------------------* 
      *                                                               * 
      *      PROGRAMA     : CLLPG637                                  * 
      *      PROGRAMADOR  : ARTHUR EPISCOPO    CPM                    * 
      *      SUPERVISORA  : CONCEICAO          CPM                    * 
      *      ANALISTA     : MARISA             GRP.82                 * 
      *      DATA         : 21/01/1998                                * 
      *                                                               * 
      *      OBJETIVO     :                                           * 
      *        LER OS ARQUIVOS QUE EMITEM CARTAS E GERAR RELATO-      * 
      *        RIO POR CLIENTE.                                       * 
      *                                                               * 
      *      ARQUIVOS:                                                * 
      *         DDNAME                                                * 
      *         PARMCLLP............. 250                             * 
      *         LADO107 ............. 533                             * 
      *         LADO120 ............. 533                             * 
      *         LADO140 ............. 533                             * 
      *         LADO199 ............. 533                             * 
      *         LADO207 ............. 533                             * 
      *         LADO220 ............. 533                             * 
      *         LADO240 ............. 533                             * 
      *         LADO299 ............. 533                             * 
      *         LADA107 ............. 533                             * 
      *         LADA120 ............. 533                             * 
      *         LADA140 ............. 533                             * 
      *         LADA199 ............. 533                             * 
      *         LADA207 ............. 533                             * 
      *         LADA220 ............. 533                             * 
      *         LADA240 ............. 533                             * 
      *         LADA299 ............. 533                             * 
      *         RELATO  ............. 132                             * 
      *         SORTWK01............. 298                             * 
      *                                                               * 
      *===============================================================* 
      *      ANALISTA     : CARMEN SILVIA                             * 
      *      DATA         : 23/03/2007                                * 
      *                                                               * 
      *      OBJETIVO     :                                           * 
      *        INCLUIR CODIGO DE BARRA                                * 
      *                                                               * 
      *                                                               * 
      *===============================================================* 
VH0812*      ANALISTA     : VITAL HUNGARO                             * 
VH0812*      DATA         : 30/08/2012                                * 
VH0812*                                                               * 
VH0812*      OBJETIVO     :                                           * 
VH0812*        INCLUIR A GRAVACAO DO ARQUIVO I#CLLPX0, IMAGEM DO RELA-* 
VH0812*        TORIO RELATO                                           * 
VH0812*                                                               * 
VH0812*===============================================================* 
      *               B R Q     I T     S E R V I C E S                *
      *================================================================*
      *                       A L T E R A C A O                        *
      *----------------------------------------------------------------*
      *    PROGRAMADOR.:  HENRIQUE GUIMARAES      - BRQ                *
      *    ANALISTA....:  HENRIQUE GUIMARAES      - BRQ                *
      *    DATA........:  13/04/2014                                   *
      *----------------------------------------------------------------*
      *    OBJETIVO....:  ADAPTACOES PARA A LEI DA TRANSPARENCIA.      *
      *                   MANTER SOMENTE LADO1 OS DEMAIS EXCLUIR.      *
      *                   TROCA BOOK INTERNA POR I#CLLPPN.             *
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
                                                                        
                                                                        
           SELECT  PARMCLLP ASSIGN TO UT-S-PARMCLLP                     
                      FILE STATUS IS WRK-FS-PARMCLLP.                   
                                                                        
           SELECT  LADO107 ASSIGN TO UT-S-LADO107                       
                      FILE STATUS IS WRK-FS-LADO107.                    
                                                                        
VH0812     SELECT  IMAGAVIS ASSIGN TO UT-S-IMAGAVIS                     
VH0812                FILE  STATUS IS WRK-FS-IMAGAVIS.                  
                                                                        
           SELECT   RELATO ASSIGN TO UT-S-RELATO                        
                      FILE STATUS IS WRK-FS-RELATO.                     
                                                                        
           SELECT  SORTWK01 ASSIGN TO UT-S-SORTWK01.                    
                                                                        
           EJECT                                                        
      *===============================================================* 
       DATA DIVISION.                                                   
      *===============================================================* 
                                                                        
      *---------------------------------------------------------------* 
       FILE SECTION.                                                    
      *---------------------------------------------------------------* 
                                                                        
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
      *    INPUT:     ORG. SEQUENCIAL   -   LRECL = 1290              * 
      *---------------------------------------------------------------* 
       FD  LADO107                                                      
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       COPY 'I#CLLPPN'.                                                 
                                                                        
VH0812*---------------------------------------------------------------* 
VH0812*    INPUT:     ORG. SEQUENCIAL   -   LRECL = 116               * 
VH0812*---------------------------------------------------------------* 
VH0812 FD  IMAGAVIS                                                     
VH0812     RECORDING MODE IS F                                          
VH0812     LABEL RECORD IS STANDARD                                     
VH0812     BLOCK CONTAINS 0 RECORDS.                                    
VH0812                                                                  
VH0812 COPY 'I#CLLPX0'.                                                 
                                                                        
      *---------------------------------------------------------------* 
      *    OUTPUT:    ORG. SEQUENCIAL   -   LRECL = 244               *
      *---------------------------------------------------------------* 
       FD  RELATO                                                       
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  REG-RELATO                  PIC X(244).
                                                                        
      *---------------------------------------------------------------* 
      *    I-O:       ARQUIVO TEMPORARIO DE SORT                      * 
      *               ORG. SEQUENCIAL   -   LRECL = 298               * 
      *---------------------------------------------------------------* 
       SD  SORTWK01.                                                    
                                                                        
       01  SOR-REGTO.                                                   
           02  SOR-CHAVE.                                               
               03  SOR-IDENT           PIC 9(02).                       
               03  SOR-CGC.                                             
                   05  SOR-NUMCGC      PIC 9(09) COMP-3.                
                   05  SOR-FILCGC      PIC 9(05) COMP-3.                
                   05  SOR-DIGCGC      PIC 9(03) COMP-3.                
           02  SOR-AGENCIA             PIC 9(05) COMP-3.                
           02  SOR-CONTA               PIC 9(07) COMP-3.                
           02  SOR-NOME-RESP           PIC X(40).                       
           02  SOR-TIPO                PIC 9(01).                       
           02  SOR-TABELA.                                              
               03  SOR-OCORRENCIAS OCCURS 11 TIMES.                     
                   05  SOR-CART             PIC X(03).
                   05  SOR-NATUREZA         PIC X(02).
                   05  SOR-VCMTO            PIC 9(09)        COMP-3.
                   05  SOR-VALOR            PIC 9(11)V99     COMP-3.
                   05  SOR-IOF-NORMAL       PIC S9(13)V99    COMP-3.
                   05  SOR-VR-REMUNERATORIO PIC S9(13)V99    COMP-3.
                   05  SOR-VALOR-MORATORIO  PIC S9(13)V99    COMP-3.
                   05  SOR-VALOR-MULTA      PIC S9(13)V99    COMP-3.
                   05  SOR-DESP-JUD-CUSTAS  PIC S9(11)V99    COMP-3.
                   05  SOR-HONORARIOS       PIC S9(11)V99    COMP-3.
                   05  SOR-VL-TOTAL-DIVIDA  PIC S9(15)V99    COMP-3.
                   05  SOR-VL-TAXA-TARIFA   PIC S9(15)V99    COMP-3.
                                                                        
           EJECT                                                        
      *---------------------------------------------------------------* 
       WORKING-STORAGE SECTION.                                         
      *---------------------------------------------------------------* 
       77  FILLER                      PIC X(32)        VALUE           
           '* INICIO DA WORKING CLLPG637 *'.                            
                                                                        
      *---------------------------------------------------------------* 
      *                         VARIAVEIS                             * 
      *---------------------------------------------------------------* 
                                                                        
       77  WRK-VALOR-CORTE             PIC  9(15)V99      COMP-3.       
       77  WRK-SOMA                    PIC  9(15)V99    VALUE ZEROS.
       77  WRK-VL-CORTE1               PIC  9(15)V99.                   
       77  WRK-VL-CORTE2               PIC  9(15)V99.                   
       77  WRK-VL-CORTE3               PIC  9(15)V99.                   
       77  WRK-VL-CORTE4               PIC  9(15)V99.                   
       77  WRK-CORTE-ATUAL             PIC  9(15)V99.                   
       77  WRK-DIAS1                   PIC  9(02).                      
       77  WRK-DIAS2                   PIC  9(02).                      
       77  WRK-DIAS3                   PIC  9(02).                      
       77  WRK-DIAS4                   PIC  9(02).                      
       77  WRK-ABEND                   PIC S9(04) COMP  VALUE +1111.    
       77  IDENT-ANT                   PIC 9(02)        VALUE ZEROS.    
       77  ACU-VALOR-LT1               PIC 9(15)V99     VALUE ZEROS.    
       77  ACU-VALOR-LT2               PIC 9(15)V99     VALUE ZEROS.    
       77  ACU-CLIENTES                PIC 9(09)        VALUE ZEROS.    
       77  ACU-CLIENTES-GERAL          PIC 9(09)        VALUE ZEROS.    
       77  ACU-TOT-CLIENTE             PIC 9(09)V99     VALUE ZEROS.    
       77  IND                         PIC 9(02)        VALUE ZEROS.    
       77  WRK-IND                     PIC 9(03)        VALUE ZEROS.    
VH0812 77  WRK-IND1                    PIC 9(03) COMP-3 VALUE ZEROS.    
       77  CTA-LINHA                   PIC 9(03) COMP-3 VALUE 80.       
       77  PRIMEIRA-VEZ                PIC X(01)        VALUE  'S'.     
       77  WRK-ESTOURO-LIN             PIC X(01)        VALUE  'N'.     
       77  WRK-FIM-LADO107             PIC X(01)        VALUE  'N'.     
       77  WRK-FIM-SORT                PIC X(01)        VALUE  'N'.     
       77  WRK-SORT-RETURN             PIC 9(04)        VALUE  ZEROS.   
                                                                        
       77  WRK-FS-PARMCLLP             PIC X(02)        VALUE  SPACES.  
       77  WRK-FS-LADO107              PIC X(02)        VALUE  SPACES.  
VH0812 77  WRK-FS-IMAGAVIS             PIC X(02)        VALUE  SPACES.  
       77  WRK-FS-RELATO               PIC X(02)        VALUE  SPACES.  
                                                                        
       77  WRK-OPERACAO                PIC X(13) VALUE  SPACES.         
       77  WRK-ABERTURA                PIC X(13) VALUE  'NA ABERTURA'.  
       77  WRK-LEITURA                 PIC X(13) VALUE  'NA LEITURA'.   
       77  WRK-GRAVACAO                PIC X(13) VALUE  'NA GRAVACAO'.  
       77  WRK-FECHAMENTO              PIC X(13) VALUE  'NO FECHAMENTO'.
                                                                        
VH0812 01  FILLER.                                                      
VH0812     03  WRK-NOMECLI             PIC X(40) VALUE SPACES.          
VH0812     03  WRK-NUM-2-POS           PIC +9(03) VALUE ZEROS.          
VH0812     03  FILLER REDEFINES WRK-NUM-2-POS.                          
VH0812         05  FILLER              PIC X(02).                       
VH0812         05  WRK-NUM-2-POS-SS    PIC 9(02).                       
VH0812     03  WRK-NUM-5-POS           PIC +9(05) VALUE ZEROS.          
VH0812     03  FILLER REDEFINES WRK-NUM-5-POS.                          
VH0812         05  FILLER              PIC X(02).                       
VH0812         05  WRK-NUM-5-POS-SS    PIC 9(04).                       
       01  FILLER.                                                      
           03  CGC-ANT.                                                 
               05  ANT-NUMCGC      PIC 9(09) COMP-3 VALUE ZEROS.        
               05  ANT-FILCGC      PIC 9(05) COMP-3 VALUE ZEROS.        
               05  ANT-DIGCGC      PIC 9(03) COMP-3 VALUE ZEROS.        
                                                                        
                                                                        
       01  WRK-TAB-ANT.                                                 
           03  WRK-TAB  OCCURS  11  TIMES.                              
               05  TAB-AG          PIC 9(05)    COMP-3.                 
               05  TAB-CTA         PIC 9(07)    COMP-3.                 
               05  TAB-CART        PIC X(03).                           
               05  TAB-NOME        PIC X(40).                           
               05  TAB-TIPO        PIC X(01).                           
               05  TAB-VCTO        PIC 9(09)    COMP-3.                 
               05  TAB-VALOR       PIC 9(11)V99 COMP-3.                 
               05  TAB-NATUREZA    PIC X(02).                           
               05  TAB-DESC-NATUR  PIC X(20).                           
               05  TAB-IOF-NORMAL        PIC S9(13)V99 COMP-3.
               05  TAB-VR-REMUNERATORIO PIC S9(13)V99    COMP-3.
               05  TAB-VALOR-MORATORIO  PIC S9(13)V99    COMP-3.
               05  TAB-VALOR-MULTA      PIC S9(13)V99    COMP-3.
               05  TAB-DESP-JUD-CUSTAS  PIC S9(11)V99    COMP-3.
               05  TAB-HONORARIOS       PIC S9(11)V99    COMP-3.
               05  TAB-VL-TOTAL-DIVIDA  PIC S9(15)V99    COMP-3.
               05  TAB-VL-TAXA-TARIFA   PIC S9(15)V99    COMP-3.
                                                                        
       01  FILLER.                                                      
           03  DATA-AUXILIAR           PIC 9(08) VALUE ZEROS.           
           03  AUX-DATA REDEFINES DATA-AUXILIAR.                        
               05  AUX-AA                  PIC 9(4).                    
               05  AUX-MM                  PIC 9(2).                    
               05  AUX-DD                  PIC 9(2).                    
                                                                        
       01  DATA-HORA.                                                   
           03  DATA-JULIANA                PIC 9(05) COMP-3.            
           03  DATA-AAMMDD                 PIC 9(07) COMP-3.            
           03  DATA-AAAAMMDD               PIC 9(09) COMP-3.            
           03  TI-HHMMSS                   PIC 9(07) COMP-3.            
           03  TI-HHMMSSMMMMMM             PIC 9(13) COMP-3.            
           03  TIMESTAMP                   PIC X(20).                   
                                                                        
      *---------------------------------------------------------------* 
      *                            CABECALHOS                         * 
      *---------------------------------------------------------------* 
                                                                        
       01  CABEC1.                                                      
           03  CB1-CARRO               PIC X(01) VALUE '1'.             
           03  FILLER                  PIC X(50) VALUE 'CLLPG637'.      
SID        03  CB1-NOME-BCO            PIC X(20) VALUE SPACES.          
           03  FILLER                  PIC X(51) VALUE SPACES.          
                                                                        
       01  CABEC2.                                                      
           03  CB2-CARRO               PIC X(01) VALUE '0'.             
           03  FILLER.                                                  
               05  CB2-DATA-DD         PIC 9(2)/.                       
               05  CB2-DATA-MM         PIC 9(2)/.                       
               05  CB2-DATA-AA         PIC 9(4).                        
           03  FILLER                  PIC X(32) VALUE SPACES.          
           03  FILLER                  PIC X(39) VALUE                  
               'RELACAO DAS CARTAS DE COBRANCA EMITIDAS'.               
                                                                        
       01  CABEC3.                                                      
           03  CB3-CARRO               PIC X(01) VALUE '0'.             
           03  FILLER                  PIC X(50) VALUE SPACES.          
           03  CB3-TITULO              PIC X(10) VALUE 'IGUAL A '.      
           03  CB3-DIAS                PIC 9(02).                       
           03  CB3-TITULO1             PIC X(05) VALUE ' DIAS'.         
           03  FILLER                  PIC X(14) VALUE SPACES.          
                                                                        
       01  CABEC4.                                                      
           03  CB4-CARRO               PIC X(01) VALUE '0'.             
           03  FILLER                  PIC X(06) VALUE ' AG   '.        
           03  FILLER                  PIC X(11) VALUE 'CONTA'.         
           03  FILLER                  PIC X(15) VALUE 'CPF/CNPJ'.      
           03  FILLER                  PIC X(37) VALUE                  
               'N O M E   D O   C L I E N T E'.                         
           03  FILLER                  PIC X(04) VALUE 'CAR'.           
           03  FILLER                  PIC X(08) VALUE '  TIPO'.        
           03  FILLER                  PIC X(16) VALUE 'VENCIMENTO'.    
           03  FILLER                  PIC X(09) VALUE 'VALOR'.         
           03  FILLER                  PIC X(08) VALUE 'NATUREZA'.      
           03  FILLER                  PIC X(17) VALUE SPACES.
           03  FILLER                  PIC X(12) VALUE 'VR. REMUNET.'.
           03  FILLER                  PIC X(01) VALUE SPACES.
           03  FILLER                  PIC X(13) VALUE 'VR. MORATORIO'.
           03  FILLER                  PIC X(05) VALUE SPACES.
           03  FILLER                  PIC X(09) VALUE 'VR. MULTA'.
           03  FILLER                  PIC X(01) VALUE SPACES.
           03  FILLER                  PIC X(13) VALUE 'VR. DESP.JUD.'.
           03  FILLER                  PIC X(01) VALUE SPACES.
           03  FILLER                  PIC X(13) VALUE 'VR. HONORARIO'.
           03  FILLER                  PIC X(01) VALUE SPACES.
           03  FILLER                  PIC X(13) VALUE 'VR. TX.TARIFA'.
           03  FILLER                  PIC X(07) VALUE SPACES.
           03  FILLER                  PIC X(07) VALUE 'VR. IOF'.
           03  FILLER                  PIC X(05) VALUE SPACES.
           03  FILLER                  PIC X(09) VALUE 'VR. TOTAL'.
                                                                        
       01  CABEC5.                                                      
           03  CB5-CARRO               PIC X(01) VALUE ' '.             
           03  FILLER                  PIC X(05) VALUE ALL '='.         
           03  FILLER                  PIC X(01) VALUE SPACES.          
           03  FILLER                  PIC X(07) VALUE ALL '='.         
           03  FILLER                  PIC X(01) VALUE SPACES.          
           03  FILLER                  PIC X(17) VALUE ALL '='.         
           03  FILLER                  PIC X(01) VALUE SPACES.          
           03  FILLER                  PIC X(36) VALUE ALL '='.         
           03  FILLER                  PIC X(01) VALUE SPACES.          
           03  FILLER                  PIC X(03) VALUE ALL '='.         
           03  FILLER                  PIC X(01) VALUE SPACES.          
           03  FILLER                  PIC X(07) VALUE ALL '='.         
           03  FILLER                  PIC X(01) VALUE SPACES.          
           03  FILLER                  PIC X(10) VALUE ALL '='.         
           03  FILLER                  PIC X(01) VALUE SPACES.          
           03  FILLER                  PIC X(13) VALUE ALL '='.         
           03  FILLER                  PIC X(01) VALUE SPACES.          
           03  FILLER                  PIC X(23) VALUE ALL '='.         
           03  FILLER                  PIC X(01) VALUE SPACES.
           03  FILLER                  PIC X(13) VALUE ALL '='.
           03  FILLER                  PIC X(01) VALUE SPACES.
           03  FILLER                  PIC X(13) VALUE ALL '='.
           03  FILLER                  PIC X(01) VALUE SPACES.
           03  FILLER                  PIC X(13) VALUE ALL '='.
           03  FILLER                  PIC X(01) VALUE SPACES.
           03  FILLER                  PIC X(13) VALUE ALL '='.
           03  FILLER                  PIC X(01) VALUE SPACES.
           03  FILLER                  PIC X(13) VALUE ALL '='.
           03  FILLER                  PIC X(01) VALUE SPACES.
           03  FILLER                  PIC X(13) VALUE ALL '='.
           03  FILLER                  PIC X(01) VALUE SPACES.
           03  FILLER                  PIC X(13) VALUE ALL '='.
           03  FILLER                  PIC X(01) VALUE SPACES.
           03  FILLER                  PIC X(13) VALUE ALL '='.
                                                                        
      *---------------------------------------------------------------* 
      *                         LINHAS DETALHE                        * 
      *---------------------------------------------------------------* 
                                                                        
       01  LINDET1.                                                     
           03  LD1-CARRO               PIC X(1) VALUE ' '.              
           03  LD1-PRIM-PARTE.                                          
               05  LD1-AGENCIA         PIC 9(05)B.                      
               05  LD1-CONTA           PIC 9(07)B.                      
               05  LD1-CGC.                                             
                   07  LD1-NUMCGC      PIC 9(09)/.                      
                   07  LD1-FILCGC      PIC 9(04).                       
                   07  LD1-TRACO       PIC X(01) VALUE '-'.             
                   07  LD1-DIGCGC      PIC 9(02)B.                      
               05  LD1-NOMECLI         PIC X(36)B.                      
               05  LD1-CART            PIC X(03)B.                      
               05  LD1-TIPO            PIC X(07)B.                      
           03  LD1-VENCIMENTO.                                          
               05  LD1-DD              PIC 9(02)/.                      
               05  LD1-MM              PIC 9(02)/.                      
               05  LD1-AA              PIC 9(04)B.                      
           03  LD1-VALOR               PIC ZZZZZZZZZ9,99B.              
           03  LD1-NATUREZA            PIC X(02)B.                      
           03  LD1-DESCRICAO           PIC X(20).                       
           03  FILLER                  PIC X(01) VALUE SPACES.
           03  LD1-VR-REMUNERATORIO    PIC ZZZZZZZZZ9,99B.
           03  LD1-VALOR-MORATORIO     PIC ZZZZZZZZZ9,99B.
           03  LD1-VALOR-MULTA         PIC ZZZZZZZZZ9,99B.
           03  LD1-DESP-JUD-CUSTAS     PIC ZZZZZZZZZ9,99B.
           03  LD1-HONORARIOS          PIC ZZZZZZZZZ9,99B.
           03  LD1-VL-TAXA-TARIFA      PIC ZZZZZZZZZ9,99B.
           03  LD1-IOF-NORMAL          PIC ZZZZZZZZZ9,99B.
           03  LD1-VL-TOTAL-DIVIDA     PIC ZZZZZZZZZ9,99B.
                                                                        
       01  LINDET2.                                                     
           03  LD2-CARRO               PIC X(01) VALUE ' '.             
           03  FILLER                  PIC X(32) VALUE SPACES.          
           03  FILLER                  PIC X(16) VALUE                  
               'TOTAL DO CLIENTE'.                                      
           03  FILLER                  PIC X(180) VALUE SPACES.
           03  LD2-TOTAL-CLIENTE       PIC ZZZZZZZZZ9,99.               
      *---------------------------------------------------------------* 
      *                         LINHAS DE TOTAIS                      * 
      *---------------------------------------------------------------* 
                                                                        
       01  LINTOT1.                                                     
           03  LT1-CARRO               PIC X(01) VALUE '-'.             
           03  FILLER                  PIC X(16) VALUE SPACES.          
           03  FILLER                  PIC X(17) VALUE                  
               'TOTAL         :  '.                                     
           03  LT1-TOTAL               PIC ZZZ.ZZZ.ZZ9.                 
           03  FILLER                  PIC X(09) VALUE '   CARTAS'.     
           03  FILLER                  PIC X(175) VALUE SPACES.
           03  LT1-VALOR               PIC ZZZZZZZZZ9,99  .             
                                                                        
       01  LINTOT2.                                                     
           03  LT2-CARRO               PIC X(01) VALUE '0'.             
           03  FILLER                  PIC X(16) VALUE SPACES.          
           03  FILLER                  PIC X(17) VALUE                  
               'TOTAL  GERAL  :  '.                                     
           03  LT2-TOTAL-GERAL         PIC ZZZ.ZZZ.ZZ9.                 
           03  FILLER                  PIC X(09) VALUE '   CARTAS'.     
           03  FILLER                  PIC X(175) VALUE SPACES.
           03  LT2-VALOR               PIC ZZZZZZZZZ9,99  .             
                                                                        
      *---------------------------------------------------------------* 
      *                     LINHA DE ARQUIVOS VAZIOS                  * 
      *---------------------------------------------------------------* 
                                                                        
       01  VAZIO.                                                       
           03  LT1-CARRO               PIC X(01) VALUE '-'.             
           03  FILLER                  PIC X(40) VALUE SPACES.          
           03  FILLER                  PIC X(50) VALUE                  
               'PROGRAMA ENCERRADO TODOS OS ARQUIVOS ESTAO VAZIOS'.     
                                                                        
       01  FILLER                      PIC X(32)        VALUE           
           '*  FIM DA WORKING CLLPG637 *'.                              
                                                                        
      *================================================================*
       LINKAGE                         SECTION.                         
      *================================================================*
                                                                        
       01  LNK-CAMPOS-PARM.                                             
           03 LNK-TAMANHO-PARM              PIC S9(04)  COMP.           
           03 LNK-CCUSTO-PARM               PIC X(04).                  
                                                                        
           EJECT                                                        
      *===============================================================* 
       PROCEDURE DIVISION     USING       LNK-CAMPOS-PARM.              
      *===============================================================* 
                                                                        
      *---------------------------------------------------------------* 
       0000-INICIAR SECTION.                                            
      *---------------------------------------------------------------* 
                                                                        
           OPEN INPUT   PARMCLLP                                        
                        LADO107                                         
VH0812          OUTPUT  IMAGAVIS                                        
                OUTPUT  RELATO.                                         
                                                                        
           MOVE    WRK-ABERTURA            TO   WRK-OPERACAO.           
           PERFORM 1000-TESTAR-FILE-STATUS.                             
                                                                        
      *    CALL 'POOL7600' USING DATA-HORA.
           MOVE  DATA-AAAAMMDD TO DATA-AUXILIAR.                        
           MOVE AUX-AA TO CB2-DATA-AA.                                  
           MOVE AUX-MM TO CB2-DATA-MM.                                  
           MOVE AUX-DD TO CB2-DATA-DD.                                  
                                                                        
           SORT SORTWK01 ASCENDING KEY SOR-IDENT                        
                                       SOR-CGC                          
                INPUT  PROCEDURE  2000-ROT-INICIAL                      
                OUTPUT PROCEDURE  3000-ROT-FINAL.                       
                                                                        
           IF SORT-RETURN NOT EQUAL ZEROS                               
              MOVE SORT-RETURN           TO WRK-SORT-RETURN             
              DISPLAY '************* CLLPG637**************'            
              DISPLAY '*   ERRO NO PROCESSAMENTO DO SORT   *'           
              DISPLAY '*         SORT-RETURN  = ' WRK-SORT-RETURN       
                                                   '       *'           
              DISPLAY '************* CLLPG637**************'            
              CALL 'ILBOABN0'     USING WRK-ABEND.                      
                                                                        
           PERFORM 4000-FINALIZAR.                                      
                                                                        
      *---------------------------------------------------------------* 
       0000-99-FIM. EXIT.                                               
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       1000-TESTAR-FILE-STATUS SECTION.                                 
      *---------------------------------------------------------------* 
                                                                        
           PERFORM 1100-TESTAR-FS-LADO107.                              
VH0812     PERFORM 1850-TESTAR-FS-IMAGAVIS.                             
           PERFORM 1900-TESTAR-FS-RELATO.                               
                                                                        
      *---------------------------------------------------------------* 
       1000-99-FIM. EXIT.                                               
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       1100-TESTAR-FS-LADO107 SECTION.                                  
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-FS-LADO107 NOT EQUAL '00'                             
              DISPLAY '************* CLLPG637 **************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              LADO107              *'           
              DISPLAY '*         FILE STATUS =  ' WRK-FS-LADO107        
                                                 '         *'           
              DISPLAY '************* CLLPG637 **************'           
              CALL 'ILBOABN0'     USING WRK-ABEND.                      
                                                                        
      *---------------------------------------------------------------* 
       1100-99-FIM. EXIT.                                               
      *---------------------------------------------------------------* 
           EJECT                                                        
VH0812*---------------------------------------------------------------* 
VH0812 1850-TESTAR-FS-IMAGAVIS SECTION.                                 
VH0812*---------------------------------------------------------------* 
VH0812                                                                  
VH0812     IF WRK-FS-IMAGAVIS NOT EQUAL '00'                            
VH0812        DISPLAY '************* CLLPG637 **************'           
VH0812        DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
VH0812        DISPLAY '*              IMAGAVIS             *'           
VH0812        DISPLAY '*         FILE STATUS =  ' WRK-FS-IMAGAVIS       
VH0812                                           '         *'           
VH0812        DISPLAY '************* CLLPG637 **************'           
VH0812        CALL 'ILBOABN0'     USING WRK-ABEND                       
VH0812     END-IF.                                                      
VH0812                                                                  
VH0812*---------------------------------------------------------------* 
VH0812 1850-99-FIM. EXIT.                                               
VH0812*---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       1900-TESTAR-FS-RELATO SECTION.                                   
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-FS-RELATO NOT EQUAL '00'                              
              DISPLAY '************* CLLPG637 **************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              RELATO               *'           
              DISPLAY '*         FILE STATUS =  ' WRK-FS-RELATO         
                                                 '         *'           
              DISPLAY '************* CLLPG637 **************'           
              CALL 'ILBOABN0'     USING WRK-ABEND.                      
                                                                        
      *---------------------------------------------------------------* 
       1900-99-FIM. EXIT.                                               
      *---------------------------------------------------------------* 
           EJECT                                                        
      *===============================================================* 
       2000-ROT-INICIAL SECTION.                                        
      *===============================================================* 
                                                                        
                                                                        
           PERFORM 2105-LER-PARMCLLP.                                   
                                                                        
           IF  WRK-FS-PARMCLLP   EQUAL  '10'                            
               DISPLAY '****************  CLLPG637  *******************'
               DISPLAY '*           ABEND 1111 - FORCADO              *'
               DISPLAY '*      ARQUIVO DE PARAMETROS ESTA VAZIO       *'
               DISPLAY '*   AVISAR ANALISTA RESPONSAVEL DA ROTINA     *'
               DISPLAY '*****************  CLLPG637  ******************'
               CALL  'ILBOABN0'  USING  WRK-ABEND                       
           ELSE                                                         
               PERFORM 2106-TRATA-PARMCLLP                              
                   UNTIL WRK-FS-PARMCLLP  EQUAL  '10'.                  
                                                                        
           PERFORM 2100-LER-GRAVAR-LADO107                              
               UNTIL WRK-FIM-LADO107 EQUAL 'S'.                         
                                                                        
      *---------------------------------------------------------------* 
       2000-99-FIM. EXIT.                                               
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       2100-LER-GRAVAR-LADO107 SECTION.                                 
      *---------------------------------------------------------------* 
                                                                        
           PERFORM 2110-LER-LADO107.                                    
           IF  WRK-FIM-LADO107     NOT EQUAL   'S'                      
               PERFORM 2120-GRAVAR-SORT.                                
                                                                        
      *---------------------------------------------------------------* 
       2100-99-FIM. EXIT.                                               
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       2105-LER-PARMCLLP SECTION.                                       
      *---------------------------------------------------------------* 
                                                                        
           READ PARMCLLP.                                               
                                                                        
           IF  WRK-FS-PARMCLLP EQUAL '10'                               
               GO TO 2105-99-FIM.                                       
                                                                        
                                                                        
           IF  PARM-CCUSTO-1    NOT EQUAL 'CLLP'  OR                    
               PARM-CODIGO-1    NOT EQUAL '0002'  OR                    
               PARM-CCUSTO-2    NOT EQUAL 'CLLP'  OR                    
               PARM-CODIGO-2    NOT EQUAL '7615'                        
               DISPLAY '****************  CLLPG637  *******************'
               DISPLAY '*           ABEND 1111 - FORCADO              *'
               DISPLAY '*  PROGRAMA CANCELADO - PARAMETRO INVALIDO    *'
               DISPLAY '* '  PARM-CCUSTO-1 ' / ' PARM-CODIGO-1          
                       ' / ' PARM-CCUSTO-2 ' / ' PARM-CODIGO-2          
               DISPLAY '*   AVISAR ANALISTA RESPONSAVEL DA ROTINA     *'
               DISPLAY '*****************  CLLPG637  ******************'
               CALL  'ILBOABN0'  USING  WRK-ABEND.                      
                                                                        
                                                                        
      *---------------------------------------------------------------* 
       2105-99-FIM. EXIT.                                               
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       2106-TRATA-PARMCLLP SECTION.                                     
      *---------------------------------------------------------------* 
                                                                        
           IF  PARM-SEQUENCIA    EQUAL   1                              
               MOVE  PARM-NR-DIAS        TO   WRK-DIAS1                 
               MOVE  PARM-VL-CORTE       TO   WRK-VL-CORTE1             
           ELSE                                                         
               IF  PARM-SEQUENCIA    EQUAL   2                          
                   MOVE  PARM-NR-DIAS        TO   WRK-DIAS2             
                   MOVE  PARM-VL-CORTE       TO   WRK-VL-CORTE2         
               ELSE                                                     
                   IF  PARM-SEQUENCIA    EQUAL   3                      
                       MOVE  PARM-NR-DIAS        TO   WRK-DIAS3         
                       MOVE  PARM-VL-CORTE       TO   WRK-VL-CORTE3     
                   ELSE                                                 
                       IF  PARM-SEQUENCIA    EQUAL   4                  
                           MOVE  PARM-NR-DIAS      TO   WRK-DIAS4       
                           MOVE  PARM-VL-CORTE     TO   WRK-VL-CORTE4.  
                                                                        
           PERFORM 2105-LER-PARMCLLP.                                   
                                                                        
      *---------------------------------------------------------------* 
       2106-99-FIM. EXIT.                                               
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       2110-LER-LADO107  SECTION.                                       
      *---------------------------------------------------------------* 
                                                                        
           READ LADO107.                                                
                                                                        
           IF WRK-FS-LADO107 EQUAL '10'                                 
              MOVE  'S' TO WRK-FIM-LADO107                              
              GO  TO 2110-99-FIM.                                       
                                                                        
           MOVE    WRK-LEITURA     TO  WRK-OPERACAO.                    
           PERFORM 1100-TESTAR-FS-LADO107.                              
                                                                        
      *---------------------------------------------------------------* 
       2110-99-FIM. EXIT.                                               
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       2120-GRAVAR-SORT SECTION.                                        
      *---------------------------------------------------------------* 
                                                                        
           MOVE    SPC-IDENT         TO  SOR-IDENT.                     
           MOVE    SPC-CHAVE-CGC     TO  SOR-CGC.                       
           MOVE    SPC-AGENCIA       TO  SOR-AGENCIA.                   
           MOVE    SPC-NUM-CC        TO  SOR-CONTA.                     
           MOVE    SPC-NOMERESP      TO  SOR-NOME-RESP.                 
           MOVE    SPC-TIPO          TO  SOR-TIPO.                      
           PERFORM                                                      
               VARYING WRK-IND1        FROM 1 BY 1 UNTIL                
               WRK-IND1                GREATER 11                       
               MOVE SPC-NATUREZA  (WRK-IND1)                            
                                       TO SOR-NATUREZA(WRK-IND1)        
               MOVE SPC-DAT-VENCTO(WRK-IND1)                            
                                       TO SOR-VCMTO   (WRK-IND1)        
               MOVE SPC-CARTEIRA  (WRK-IND1)                            
                                       TO SOR-CART    (WRK-IND1)        
               MOVE SPC-RESGATE   (WRK-IND1)                            
                                       TO SOR-VALOR   (WRK-IND1)        
               MOVE SPC-IOF-NORMAL(WRK-IND1)
                                       TO SOR-IOF-NORMAL(WRK-IND1)
               MOVE SPC-VR-REMUNERATORIO(WRK-IND1)
                                       TO SOR-VR-REMUNERATORIO(WRK-IND1)
               MOVE  SPC-VALOR-MORATORIO(WRK-IND1)
                                       TO SOR-VALOR-MORATORIO(WRK-IND1)
               MOVE  SPC-VALOR-MULTA    (WRK-IND1)
                                       TO SOR-VALOR-MULTA(WRK-IND1)
               MOVE  SPC-DESP-JUD-CUSTAS(WRK-IND1)
                                       TO SOR-DESP-JUD-CUSTAS(WRK-IND1)
               MOVE  SPC-HONORARIOS     (WRK-IND1)
                                       TO SOR-HONORARIOS(WRK-IND1)
               MOVE  SPC-VL-TAXA-TARIFA (WRK-IND1)
                                       TO SOR-VL-TAXA-TARIFA(WRK-IND1)
               MOVE  SPC-VL-TOTAL-DIVIDA(WRK-IND1)
                                       TO SOR-VL-TOTAL-DIVIDA(WRK-IND1)
           END-PERFORM.                                                 
                                                                        
           RELEASE SOR-REGTO.                                           
                                                                        
      *---------------------------------------------------------------* 
       2120-99-FIM. EXIT.                                               
      *---------------------------------------------------------------* 
           EJECT                                                        
      *===============================================================* 
       3000-ROT-FINAL SECTION.                                          
      *===============================================================* 
                                                                        
           PERFORM 3100-ROTINA-FINAL UNTIL WRK-FIM-SORT EQUAL 'S'.      
                                                                        
      *---------------------------------------------------------------* 
       3000-99-FIM. EXIT.                                               
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       3100-ROTINA-FINAL SECTION.                                       
      *---------------------------------------------------------------* 
                                                                        
           PERFORM 3200-LER-SORTWK01.                                   
           MOVE    'S'     TO  WRK-ESTOURO-LIN.                         
                                                                        
           IF  WRK-FIM-SORT    NOT EQUAL   'S'                          
               PERFORM 3300-CONTROLA-IMPRESSAO                          
                                   VARYING IND FROM 1 BY 1              
                                   UNTIL IND GREATER 11.                
                                                                        
      *---------------------------------------------------------------* 
       3100-99-FIM. EXIT.                                               
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       3200-LER-SORTWK01  SECTION.                                      
      *---------------------------------------------------------------* 
                                                                        
           RETURN SORTWK01 AT END                                       
               MOVE 'S' TO WRK-FIM-SORT                                 
               GO TO 3200-99-FIM.                                       
                                                                        
           IF  PRIMEIRA-VEZ    EQUAL  'S'                               
               MOVE    'N'         TO  PRIMEIRA-VEZ                     
               MOVE    SOR-IDENT   TO  IDENT-ANT                        
               MOVE    SOR-NUMCGC  TO  ANT-NUMCGC                       
               MOVE    SOR-FILCGC  TO  ANT-FILCGC                       
               MOVE    SOR-DIGCGC  TO  ANT-DIGCGC                       
               MOVE    01          TO  WRK-IND                          
               PERFORM 3332-ZERA-TABELA                                 
               MOVE    01          TO  WRK-IND.                         
                                                                        
      *---------------------------------------------------------------* 
       3200-99-FIM. EXIT.                                               
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       3300-CONTROLA-IMPRESSAO SECTION.                                 
      *---------------------------------------------------------------* 
                                                                        
           IF  SOR-IDENT   EQUAL   IDENT-ANT                            
               PERFORM 3310-IMPRIME                                     
           ELSE                                                         
               PERFORM 3320-IMPRIME-TOTAL                               
               PERFORM 3310-IMPRIME.                                    
                                                                        
           MOVE    SOR-IDENT   TO  IDENT-ANT.                           
                                                                        
      *---------------------------------------------------------------* 
       3300-99-FIM. EXIT.                                               
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       3310-IMPRIME SECTION.                                            
      *---------------------------------------------------------------* 
                                                                        
           IF  SOR-VALOR(IND)  EQUAL   ZEROS                            
               GO TO 3310-99-FIM.                                       
                                                                        
           IF  SOR-CGC  EQUAL   CGC-ANT                                 
               PERFORM 3330-CARREGA-TABELA                              
               COMPUTE WRK-SOMA = WRK-SOMA + SOR-VL-TOTAL-DIVIDA(IND)
           ELSE                                                         
               PERFORM 3333-IDENTIFICA-VL-CORTE                         
               IF  WRK-SOMA  GREATER WRK-VALOR-CORTE                    
                   MOVE  01  TO  WRK-IND                                
VH1212             MOVE  01  TO  WRK-IND1                               
                   PERFORM 3331-DESCARREGA-TABELA                       
                   PERFORM 3315-IMPRIME-TOT-CLIENTE                     
                   MOVE    SOR-NUMCGC  TO  ANT-NUMCGC                   
                   MOVE    SOR-FILCGC  TO  ANT-FILCGC                   
                   MOVE    SOR-DIGCGC  TO  ANT-DIGCGC                   
                   MOVE  01  TO  WRK-IND                                
                   PERFORM 3332-ZERA-TABELA                             
                   MOVE  ZEROS  TO  WRK-SOMA                            
                   MOVE  01  TO  WRK-IND                                
                   PERFORM 3330-CARREGA-TABELA                          
                   COMPUTE WRK-SOMA = WRK-SOMA +
                                       SOR-VL-TOTAL-DIVIDA(IND)
               ELSE                                                     
                   MOVE    SOR-NUMCGC  TO  ANT-NUMCGC                   
                   MOVE    SOR-FILCGC  TO  ANT-FILCGC                   
                   MOVE    SOR-DIGCGC  TO  ANT-DIGCGC                   
                   MOVE  01  TO  WRK-IND                                
                   PERFORM 3332-ZERA-TABELA                             
                   MOVE  ZEROS  TO  WRK-SOMA                            
                   MOVE  01  TO  WRK-IND                                
                   PERFORM 3330-CARREGA-TABELA                          
                   COMPUTE WRK-SOMA = WRK-SOMA + SOR-VALOR(IND).        
                                                                        
      *---------------------------------------------------------------* 
       3310-99-FIM. EXIT.                                               
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       3311-VERIFICAR-QUEBRA-PAGINA SECTION.                            
      *---------------------------------------------------------------* 
                                                                        
           IF  CTA-LINHA GREATER 60                                     
               PERFORM 3312-IMPRIMIR-CABEC                              
               MOVE    'S'         TO  WRK-ESTOURO-LIN                  
               MOVE    08          TO  CTA-LINHA.                       
                                                                        
      *---------------------------------------------------------------* 
       3311-99-FIM. EXIT.                                               
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       3312-IMPRIMIR-CABEC  SECTION.                                    
      *---------------------------------------------------------------* 
                                                                        
           IF  SOR-IDENT   EQUAL   WRK-DIAS1                            
               MOVE    WRK-DIAS1           TO  CB3-DIAS                 
           ELSE                                                         
               IF  SOR-IDENT   EQUAL   WRK-DIAS2                        
                   MOVE    WRK-DIAS2            TO  CB3-DIAS            
               ELSE                                                     
                   IF  SOR-IDENT   EQUAL   WRK-DIAS3                    
                       MOVE    WRK-DIAS3            TO  CB3-DIAS        
                   ELSE                                                 
                       IF  SOR-IDENT   EQUAL   WRK-DIAS4                
                           MOVE    WRK-DIAS4             TO  CB3-DIAS.  
                                                                        
SID        IF  LNK-CCUSTO-PARM EQUAL 'LEAS'                             
SID            MOVE 'BRADESCO LEASING S/A' TO CB1-NOME-BCO              
SID        ELSE                                                         
SID            MOVE 'BANCO BRADESCO S.A  ' TO CB1-NOME-BCO              
SID        END-IF.                                                      
                                                                        
           WRITE   REG-RELATO  FROM    CABEC1.                          
           MOVE    WRK-GRAVACAO    TO  WRK-OPERACAO.                    
           PERFORM 1900-TESTAR-FS-RELATO.                               
                                                                        
           WRITE   REG-RELATO  FROM    CABEC2.                          
           PERFORM 1900-TESTAR-FS-RELATO.                               
                                                                        
           WRITE   REG-RELATO  FROM    CABEC3.                          
           PERFORM 1900-TESTAR-FS-RELATO.                               
                                                                        
           WRITE   REG-RELATO  FROM    CABEC4.                          
           PERFORM 1900-TESTAR-FS-RELATO.                               
                                                                        
           WRITE   REG-RELATO  FROM    CABEC5.                          
           PERFORM 1900-TESTAR-FS-RELATO.                               
                                                                        
      *---------------------------------------------------------------* 
       3312-99-FIM. EXIT.                                               
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       3313-DETERMINA-TIPO SECTION.                                     
      *---------------------------------------------------------------* 
                                                                        
           IF  TAB-TIPO(1)         EQUAL  1                             
               MOVE 'CORREIO'  TO  LD1-TIPO                             
           ELSE                                                         
               IF  TAB-TIPO(1)         EQUAL  2                         
                   MOVE 'AGENCIA'  TO  LD1-TIPO                         
               ELSE                                                     
                   MOVE 'AVALISTA' TO  LD1-TIPO.                        
                                                                        
      *---------------------------------------------------------------* 
       3313-99-FIM. EXIT.                                               
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       3315-IMPRIME-TOT-CLIENTE SECTION.                                
      *---------------------------------------------------------------* 
                                                                        
            IF WRK-SOMA         NOT EQUAL  ZEROS                        
               MOVE    WRK-SOMA        TO  LD2-TOTAL-CLIENTE            
               COMPUTE ACU-VALOR-LT1 = ACU-VALOR-LT1 + WRK-SOMA         
               WRITE   REG-RELATO  FROM    LINDET2                      
               MOVE    WRK-GRAVACAO    TO  WRK-OPERACAO                 
               PERFORM 1900-TESTAR-FS-RELATO                            
               MOVE    ZEROS           TO  WRK-SOMA                     
               ADD     1               TO  CTA-LINHA.                   
                                                                        
      *---------------------------------------------------------------* 
       3315-99-FIM. EXIT.                                               
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       3320-IMPRIME-TOTAL SECTION.                                      
      *---------------------------------------------------------------* 
                                                                        
           ADD     80              TO  CTA-LINHA.                       
                                                                        
           MOVE    ACU-CLIENTES    TO  LT1-TOTAL.                       
           ADD     ACU-CLIENTES    TO  ACU-CLIENTES-GERAL               
           MOVE    ZEROS           TO  ACU-CLIENTES.                    
                                                                        
           MOVE    ACU-VALOR-LT1   TO  LT1-VALOR.                       
           ADD     ACU-VALOR-LT1   TO  ACU-VALOR-LT2                    
           MOVE    ZEROS           TO  ACU-VALOR-LT1.                   
                                                                        
           WRITE   REG-RELATO      FROM    LINTOT1.                     
           MOVE    WRK-GRAVACAO    TO  WRK-OPERACAO.                    
           PERFORM 1900-TESTAR-FS-RELATO.                               
                                                                        
      *---------------------------------------------------------------* 
       3320-99-FIM. EXIT.                                               
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       3330-CARREGA-TABELA SECTION.                                     
      *---------------------------------------------------------------* 
                                                                        
           MOVE    SOR-AGENCIA     TO  TAB-AG(WRK-IND)                  
           MOVE    SOR-CONTA       TO  TAB-CTA(WRK-IND)                 
           MOVE    SOR-NOME-RESP   TO  TAB-NOME(WRK-IND)                
           MOVE    SOR-TIPO        TO  TAB-TIPO(WRK-IND)                
                                                                        
           MOVE    SOR-CART(IND)      TO  TAB-CART(WRK-IND)             
           MOVE    SOR-VCMTO(IND)     TO  TAB-VCTO(WRK-IND)             
           MOVE    SOR-NATUREZA(IND)  TO  TAB-NATUREZA(WRK-IND)         
           MOVE    SOR-VALOR(IND)     TO  TAB-VALOR(WRK-IND)
           MOVE    SOR-IOF-NORMAL(IND)
                                   TO TAB-IOF-NORMAL(WRK-IND)
           MOVE    SOR-VR-REMUNERATORIO(IND)
                                   TO TAB-VR-REMUNERATORIO(WRK-IND)
           MOVE    SOR-VALOR-MORATORIO(IND)
                                   TO TAB-VALOR-MORATORIO(WRK-IND)
           MOVE    SOR-VALOR-MULTA(IND)
                                   TO TAB-VALOR-MULTA(WRK-IND)
           MOVE    SOR-DESP-JUD-CUSTAS(IND)
                                   TO TAB-DESP-JUD-CUSTAS(WRK-IND)
           MOVE    SOR-HONORARIOS(IND)
                                   TO TAB-HONORARIOS(WRK-IND)
           MOVE    SOR-VL-TOTAL-DIVIDA(IND)
                                   TO TAB-VL-TOTAL-DIVIDA(WRK-IND)
           MOVE    SOR-VL-TAXA-TARIFA(IND)
                                   TO TAB-VL-TAXA-TARIFA(WRK-IND)

           EVALUATE SOR-NATUREZA(IND)                                   
           WHEN                                                         
            'CH'                                                        
              MOVE 'ADIANT. DEPOSITANTES' TO TAB-DESC-NATUR(WRK-IND)    
           WHEN                                                         
            'AD'                                                        
              MOVE 'AD. DEPOS. CLIENTES ' TO TAB-DESC-NATUR(WRK-IND)    
           WHEN                                                         
            'AR'                                                        
              MOVE 'ARRENDAMENTOS       ' TO TAB-DESC-NATUR(WRK-IND)    
           WHEN                                                         
            'FI'                                                        
              MOVE 'CREDITOS E FINANC.  ' TO TAB-DESC-NATUR(WRK-IND)    
           WHEN                                                         
            'EC'                                                        
              MOVE 'EMPRESTIMOS EM CONTA' TO TAB-DESC-NATUR(WRK-IND)    
           WHEN                                                         
            'AG'                                                        
              MOVE 'EMPR. AGRIC. E IND. ' TO TAB-DESC-NATUR(WRK-IND)    
           WHEN                                                         
            'CA'                                                        
              MOVE 'OPERACOES DE CAMBIO ' TO TAB-DESC-NATUR(WRK-IND)    
           WHEN                                                         
            'RE'                                                        
              MOVE 'OPERAC. DE REPASSES ' TO TAB-DESC-NATUR(WRK-IND)    
           WHEN                                                         
            'IM'                                                        
              MOVE 'OPERAC. IMOBILIARIAS' TO TAB-DESC-NATUR(WRK-IND)    
           WHEN                                                         
            'TD'                                                        
              MOVE 'TITULOS DESCONTADOS ' TO TAB-DESC-NATUR(WRK-IND)    
           WHEN                                                         
            'DC'                                                        
              MOVE 'DESCONTO DE CUEQUES ' TO TAB-DESC-NATUR(WRK-IND)    
           WHEN                                                         
            'CT'                                                        
              MOVE 'CARTAO DE CREDITO   ' TO TAB-DESC-NATUR(WRK-IND)    
           WHEN                                                         
            'OO'                                                        
              MOVE 'OUTRAS OPERACOES    ' TO TAB-DESC-NATUR(WRK-IND)    
           WHEN                                                         
            OTHER                                                       
              MOVE 'OPERACOES DIVERSAS  ' TO TAB-DESC-NATUR(WRK-IND)    
           END-EVALUATE.                                                
                                                                        
           ADD   01  TO  WRK-IND.                                       
                                                                        
      *---------------------------------------------------------------* 
       3330-99-FIM. EXIT.                                               
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       3331-DESCARREGA-TABELA SECTION.                                  
      *---------------------------------------------------------------* 
                                                                        
           IF  ( WRK-IND  GREATER  11 ) OR                              
               ( TAB-VALOR(WRK-IND)   EQUAL  ZEROS )                    
               GO TO 3331-99-FIM.                                       
                                                                        
           PERFORM 3311-VERIFICAR-QUEBRA-PAGINA.                        
                                                                        
           IF  ( WRK-IND EQUAL 01 ) OR ( WRK-ESTOURO-LIN EQUAL 'S' )    
               MOVE    TAB-AG(WRK-IND)   TO  LD1-AGENCIA                
               MOVE    TAB-CTA(WRK-IND)  TO  LD1-CONTA                  
               MOVE    ANT-NUMCGC        TO  LD1-NUMCGC                 
               MOVE    ANT-FILCGC        TO  LD1-FILCGC                 
               MOVE    '-'               TO  LD1-TRACO                  
               MOVE    ANT-DIGCGC        TO  LD1-DIGCGC                 
               MOVE    TAB-NOME(WRK-IND) TO  LD1-NOMECLI                
               MOVE    TAB-CART(WRK-IND) TO  LD1-CART                   
               PERFORM 3313-DETERMINA-TIPO                              
               MOVE  'N'  TO  WRK-ESTOURO-LIN                           
               ADD  1     TO  ACU-CLIENTES                              
           ELSE                                                         
               MOVE    SPACES          TO  LD1-PRIM-PARTE.              
                                                                        
           MOVE    TAB-VCTO(WRK-IND)   TO DATA-AUXILIAR                 
           MOVE    AUX-AA              TO LD1-AA                        
           MOVE    AUX-MM              TO LD1-MM                        
           MOVE    AUX-DD              TO LD1-DD                        
           MOVE    TAB-NATUREZA(WRK-IND)     TO LD1-NATUREZA            
           MOVE    TAB-VALOR(WRK-IND)        TO LD1-VALOR               
           MOVE    TAB-DESC-NATUR(WRK-IND)   TO LD1-DESCRICAO.          
           MOVE    TAB-IOF-NORMAL(WRK-IND)       TO LD1-IOF-NORMAL
           MOVE    TAB-VR-REMUNERATORIO(WRK-IND) TO LD1-VR-REMUNERATORIO
           MOVE    TAB-VALOR-MORATORIO(WRK-IND)  TO LD1-VALOR-MORATORIO
           MOVE    TAB-VALOR-MULTA(WRK-IND)      TO LD1-VALOR-MULTA
           MOVE    TAB-DESP-JUD-CUSTAS(WRK-IND)  TO LD1-DESP-JUD-CUSTAS
           MOVE    TAB-HONORARIOS(WRK-IND)       TO LD1-HONORARIOS
           MOVE    TAB-VL-TOTAL-DIVIDA(WRK-IND)  TO LD1-VL-TOTAL-DIVIDA
           MOVE    TAB-VL-TAXA-TARIFA(WRK-IND)   TO LD1-VL-TAXA-TARIFA
                                                                        
VH0812     PERFORM 3334-GRAVAR-IMAGAVIS                                 
                                                                        
           ADD     1   TO      CTA-LINHA                                
                                                                        
           WRITE   REG-RELATO  FROM    LINDET1.                         
           MOVE    WRK-GRAVACAO    TO  WRK-OPERACAO.                    
           PERFORM 1900-TESTAR-FS-RELATO.                               
                                                                        
           ADD     1   TO      WRK-IND.                                 
VH1212     ADD     1   TO      WRK-IND1.                                
           GO TO 3331-DESCARREGA-TABELA.                                
                                                                        
      *---------------------------------------------------------------* 
       3331-99-FIM. EXIT.                                               
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       3332-ZERA-TABELA SECTION.                                        
      *---------------------------------------------------------------* 
                                                                        
           IF  WRK-IND  GREATER  11                                     
               GO TO 3332-99-FIM.                                       
                                                                        
           MOVE  ZEROS  TO    TAB-AG(WRK-IND)                           
                              TAB-CTA(WRK-IND)                          
                              TAB-VCTO(WRK-IND)                         
                              TAB-VALOR(WRK-IND).                       
           MOVE  SPACES TO    TAB-NOME(WRK-IND)                         
                              TAB-TIPO(WRK-IND)                         
                              TAB-NATUREZA(WRK-IND)                     
                              TAB-CART(WRK-IND)                         
                              TAB-DESC-NATUR(WRK-IND).                  
                                                                        
           ADD  1  TO  WRK-IND.                                         
                                                                        
           GO TO 3332-ZERA-TABELA.                                      
                                                                        
      *---------------------------------------------------------------* 
       3332-99-FIM. EXIT.                                               
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       3333-IDENTIFICA-VL-CORTE SECTION.                                
      *---------------------------------------------------------------* 
                                                                        
             IF  SOR-IDENT   EQUAL   WRK-DIAS1                          
                 MOVE    WRK-VL-CORTE1       TO  WRK-VALOR-CORTE        
             ELSE                                                       
             IF  SOR-IDENT   EQUAL   WRK-DIAS2                          
                 MOVE    WRK-VL-CORTE2       TO  WRK-VALOR-CORTE        
             ELSE                                                       
             IF  SOR-IDENT   EQUAL   WRK-DIAS3                          
                 MOVE    WRK-VL-CORTE3       TO  WRK-VALOR-CORTE        
             ELSE                                                       
                 MOVE    WRK-VL-CORTE4       TO  WRK-VALOR-CORTE.       
                                                                        
      *---------------------------------------------------------------* 
       3333-99-FIM. EXIT.                                               
      *---------------------------------------------------------------* 
VH0812 EJECT                                                            
VH0812*---------------------------------------------------------------* 
VH0812 3334-GRAVAR-IMAGAVIS     SECTION.                                
VH0812*---------------------------------------------------------------* 
VH0812                                                                  
VH0812     MOVE    WRK-GRAVACAO            TO  WRK-OPERACAO             
VH0812     MOVE    TAB-AG(WRK-IND1)        TO  IMAGAVIS-AGENCIA         
VH0812     MOVE    TAB-CTA(WRK-IND1)       TO  IMAGAVIS-CONTA           
VH0812     MOVE    ANT-NUMCGC              TO  IMAGAVIS-NUMCGC          
VH0812     MOVE    ANT-FILCGC              TO  WRK-NUM-5-POS            
VH0812     MOVE    WRK-NUM-5-POS-SS        TO  IMAGAVIS-FILCGC          
VH0812     MOVE    ANT-DIGCGC              TO  WRK-NUM-2-POS            
VH0812     MOVE    WRK-NUM-2-POS-SS        TO  IMAGAVIS-DIGCGC          
VH0812     MOVE    TAB-NOME(WRK-IND1)      TO  WRK-NOMECLI              
VH0812     MOVE    WRK-NOMECLI(1:36)       TO  IMAGAVIS-NOMECLI         
VH0812     MOVE    TAB-CART(WRK-IND1)      TO  IMAGAVIS-CART            
VH0812     IF  TAB-TIPO(1) EQUAL '1'                                    
VH0812         MOVE 'CORREIO'              TO  IMAGAVIS-TIPO            
VH0812     ELSE                                                         
VH0812         IF  TAB-TIPO(1) EQUAL '2'                                
VH0812             MOVE 'AGENCIA'          TO  IMAGAVIS-TIPO            
VH0812         ELSE                                                     
VH0812             MOVE 'AVALIST'          TO  IMAGAVIS-TIPO            
VH0812         END-IF                                                   
VH0812     END-IF                                                       
VH0812     MOVE    AUX-AA                  TO  IMAGAVIS-AA              
VH0812     MOVE    AUX-MM                  TO  IMAGAVIS-MM              
VH0812     MOVE    AUX-DD                  TO  IMAGAVIS-DD              
VH0812     MOVE    TAB-NATUREZA(WRK-IND1)  TO  IMAGAVIS-NATUREZA        
VH0812     MOVE    TAB-VALOR(WRK-IND1)     TO  IMAGAVIS-VALOR           
VH0812     MOVE    TAB-DESC-NATUR(WRK-IND1) TO  IMAGAVIS-DESCRICAO      
VH0812                                                                  
VH0812     WRITE   IMAGAVIS-REGISTRO                                    
VH0812                                                                  
VH0812     PERFORM 1850-TESTAR-FS-IMAGAVIS.                             
VH0812                                                                  
VH0812*---------------------------------------------------------------* 
VH0812 3334-99-FIM. EXIT.                                               
VH0812*---------------------------------------------------------------* 
VH0812 EJECT                                                            
      *---------------------------------------------------------------* 
       4000-FINALIZAR SECTION.                                          
      *---------------------------------------------------------------* 
                                                                        
           PERFORM 4100-IMPRIME-TOTAL-GERAL.                            
                                                                        
           CLOSE  PARMCLLP                                              
                  LADO107                                               
VH0812            IMAGAVIS                                              
                  RELATO.                                               
                                                                        
           MOVE    WRK-FECHAMENTO  TO  WRK-OPERACAO.                    
           PERFORM 1000-TESTAR-FILE-STATUS.                             
                                                                        
           STOP RUN.                                                    
                                                                        
      *---------------------------------------------------------------* 
       4000-99-FIM. EXIT.                                               
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       4100-IMPRIME-TOTAL-GERAL SECTION.                                
      *---------------------------------------------------------------* 
                                                                        
           IF  ACU-CLIENTES        EQUAL ZEROS AND                      
               ACU-CLIENTES-GERAL  EQUAL ZEROS                          
               PERFORM 4110-ARQUIVOS-VAZIOS                             
               GO TO 4100-99-FIM.                                       
                                                                        
SID        MOVE  01  TO  WRK-IND.                                       
VH1212     MOVE  01  TO  WRK-IND1.                                      
SID        PERFORM 3331-DESCARREGA-TABELA.                              
SID        PERFORM 3315-IMPRIME-TOT-CLIENTE.                            
                                                                        
                                                                        
      **** PERFORM 3315-IMPRIME-TOT-CLIENTE                             
           PERFORM 3320-IMPRIME-TOTAL.                                  
                                                                        
           MOVE    ACU-CLIENTES-GERAL  TO  LT2-TOTAL-GERAL.             
           MOVE    ACU-VALOR-LT2       TO  LT2-VALOR.                   
                                                                        
           WRITE   REG-RELATO  FROM    LINTOT2.                         
           MOVE    WRK-GRAVACAO    TO  WRK-OPERACAO.                    
           PERFORM 1900-TESTAR-FS-RELATO.                               
                                                                        
      *---------------------------------------------------------------* 
       4100-99-FIM. EXIT.                                               
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       4110-ARQUIVOS-VAZIOS SECTION.                                    
      *---------------------------------------------------------------* 
                                                                        
           WRITE   REG-RELATO  FROM    CABEC1.                          
           MOVE    WRK-GRAVACAO    TO  WRK-OPERACAO.                    
           PERFORM 1900-TESTAR-FS-RELATO.                               
                                                                        
           WRITE   REG-RELATO  FROM    CABEC2.                          
           PERFORM 1900-TESTAR-FS-RELATO.                               
                                                                        
           WRITE   REG-RELATO  FROM    VAZIO.                           
           PERFORM 1900-TESTAR-FS-RELATO.                               
                                                                        
      *---------------------------------------------------------------* 
       4110-99-FIM. EXIT.                                               
      *---------------------------------------------------------------* 

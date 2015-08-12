      *===============================================================* 
       IDENTIFICATION                  DIVISION.                        
      *===============================================================* 
                                                                        
       PROGRAM-ID. RDAB0150.                                            
       AUTHOR.     RODRIGO WILLAIM.                                     
      *===============================================================* 
      *                   C P M   S I S T E M A S                     * 
      *---------------------------------------------------------------* 
      *                                                               * 
      *      PROGRAMA     : RDAB9150  -  COPIA  RDAB0150              * 
      *                                                               * 
      *      PROGRAMADOR  : RODRIGO WILLAIM      - CPM/FPOLIS         * 
      *      ANALISTA CPM : SANDRO               - CPM/FPOLIS         * 
      *      ANALISTA     : MAGALI DE FARIA LIMA - GRUPO 82           * 
      *      DATA         : 24/02/2003                                * 
      *                                                               * 
      *      OBJETIVO     :                                           * 
      *        A PARTIR DOS ARQUIVOS GERADOS NO PROCESSO DE OBTEN-    * 
      *        CAO DE DADOS DO FICA, GRAVAR ARQUIVOS IMAGEM PARA      * 
      *        CARGA DAS TABELAS DA BASE UNICA.                       * 
      *                                                               * 
      *      ARQUIVOS:                                                * 
      *         DDNAME                           INCLUDE/BOOK         * 
      *         TABASUNI                           I#RDAB05           * 
      *         ARQPESJD                           I#RDAB11           * 
      *         ARQPESFS                           I#RDAB12           * 
      *         ARQCTCOR                           I#RDAB13           * 
      *         ARQLOGDR                           I#RDAB14           * 
      *         ARQFONES                           I#RDAB15           * 
      *         REGTOTAL                           --------           * 
      *                                                               * 
      *     MODULOS CHAMADOS:                                         * 
      *        POOL7100  -  MODULO DE TRATAMENTO DE ERROS.            * 
      *        POOL7600  -  OBTER DATA E HORA DO SISTEMA.             * 
      *                                                               * 
      *===============================================================* 
RON   * 08/09/2004  -  ALTERACAO         -    LEANDRO  -  RONNAM      * 
RON   * OBJETIVO: INCLUIR CHAMADA AO MODULO CLIB2000 PARA ALIMENTAR   * 
RON   *           NOVOS CAMPOS DO LAYOUT I#RDAB12                     * 
RON   *        CLIB2000 - MODULO DE ACESSO A DADOS DO CLIE.           * 
RON   *===============================================================* 
      *                                                               * 
BRQ059* ============================================================= * 
      * |                      ALTERACAO                            | * 
      * ------------------------------------------------------------+ * 
      *  ANALISTA     : ROBSON VELASQUES / BRQ - IT SERVICES        | * 
      *  PROGRAMADOR  : CRISTIANO SOUZA  / BRQ - IT SERVICES        | * 
      *  DATA         : 02/2012                                     | * 
      *  OBJETIVO     : INSERIR UM DIGITO A MAIS AOS CAMPOS DE      | * 
      *                 TELEFONES                                   | * 
      *                 ********************************************|   
      *                 * RECOMPILACAO DO PROGRAMA PARA ADEQUAR AO *| * 
      *                 * NOVO PRADRAO DE 9 DIGITOS NO N. TELEFONE *| * 
      *                 ********************************************|   
      *  PROJETO      : 2012/0059 - ADEQUAR NRO TELEFONE(RES.553)   | * 
      * ============================================================+ * 
BRQ059***************************************************************** 
                                                                        
      *===============================================================* 
       ENVIRONMENT                     DIVISION.                        
      *===============================================================* 
                                                                        
      *---------------------------------------------------------------* 
       CONFIGURATION                   SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
       SPECIAL-NAMES.                                                   
           DECIMAL-POINT IS COMMA.                                      
                                                                        
      *---------------------------------------------------------------* 
       INPUT-OUTPUT                    SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
       FILE-CONTROL.                                                    
                                                                        
           SELECT TABASUNI ASSIGN      TO  UT-S-TABASUNI                
                      FILE STATUS      IS  WRK-FS-TABASUNI.             
                                                                        
           SELECT ARQPESJD ASSIGN      TO  UT-S-ARQPESJD                
                      FILE STATUS      IS  WRK-FS-ARQPESJD.             
                                                                        
           SELECT ARQPESFS ASSIGN      TO  UT-S-ARQPESFS                
                      FILE STATUS      IS  WRK-FS-ARQPESFS.             
                                                                        
           SELECT ARQCTCOR ASSIGN      TO  UT-S-ARQCTCOR                
                      FILE STATUS      IS  WRK-FS-ARQCTCOR.             
                                                                        
           SELECT ARQLOGDR ASSIGN      TO  UT-S-ARQLOGDR                
                      FILE STATUS      IS  WRK-FS-ARQLOGDR.             
                                                                        
           SELECT ARQFONES ASSIGN      TO  UT-S-ARQFONES                
                      FILE STATUS      IS  WRK-FS-ARQFONES.             
                                                                        
           SELECT REGTOTAL ASSIGN      TO  UT-S-REGTOTAL                
                      FILE STATUS      IS  WRK-FS-REGTOTAL.             
                                                                        
      *===============================================================* 
       DATA                            DIVISION.                        
      *===============================================================* 
                                                                        
      *---------------------------------------------------------------* 
       FILE                            SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
      *    INPUT:     ARQ PARA CARGA DAS TABELAS DA BASE UNICA        * 
      *               ORG. SEQUENCIAL   -   LRECL = 500               * 
      *---------------------------------------------------------------* 
                                                                        
       FD  TABASUNI                                                     
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
-INC I#RDAB05                                                           
                                                                        
      *---------------------------------------------------------------* 
      *   OUTPUT:     ARQ PARA CARGA TABELA DE PESSOA JURIDICA        * 
      *               ORG. SEQUENCIAL   -   LRECL = 081               * 
      *---------------------------------------------------------------* 
                                                                        
       FD  ARQPESJD                                                     
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
-INC I#RDAB11                                                           
                                                                        
      *---------------------------------------------------------------* 
      *   OUTPUT:     ARQ PARA CARGA TABELA DE PESSOA FISICA          * 
      *               ORG. SEQUENCIAL   -   LRECL = 270               * 
      *---------------------------------------------------------------* 
                                                                        
       FD  ARQPESFS                                                     
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
-INC I#RDAB12                                                           
                                                                        
      *---------------------------------------------------------------* 
      *   OUTPUT:     ARQ PARA CARGA TABELA DE CONTA CORRENTE         * 
      *               ORG. SEQUENCIAL   -   LRECL = 096               * 
      *---------------------------------------------------------------* 
                                                                        
       FD  ARQCTCOR                                                     
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
-INC I#RDAB13                                                           
                                                                        
      *---------------------------------------------------------------* 
      *   OUTPUT:     ARQ PARA CARGA TABELA DE LOGRADOURO             * 
      *               ORG. SEQUENCIAL   -   LRECL = 146               * 
      *---------------------------------------------------------------* 
                                                                        
       FD  ARQLOGDR                                                     
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
-INC I#RDAB14                                                           
                                                                        
      *---------------------------------------------------------------* 
      *   OUTPUT:     ARQ PARA CARGA TABELA DE TELEFONE               * 
      *               ORG. SEQUENCIAL   -   LRECL = 041               * 
      *---------------------------------------------------------------* 
                                                                        
       FD  ARQFONES                                                     
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
-INC I#RDAB15                                                           
                                                                        
      *---------------------------------------------------------------* 
      *   OUTPUT:     TOTALIZACAO DE QTDE DE LIDOS E GRAVADOS         * 
      *               ORG. SEQUENCIAL   -   LRECL = 081               * 
      *---------------------------------------------------------------* 
                                                                        
       FD  REGTOTAL                                                     
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
                                                                        
       01  REG-REGTOTAL                PIC  X(081).                     
                                                                        
      *---------------------------------------------------------------* 
       WORKING-STORAGE                 SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
       77  FILLER                      PIC  X(32)          VALUE        
           '*  INICIO DA WORKING RDAB0150  *'.                          
                                                                        
      *---- ACUMULADORES ---------------------------------------------* 
                                                                        
       77  ACU-LIDOS-TABASUNI          PIC  9(09) COMP-3   VALUE ZEROS. 
       77  ACU-GRAVADOS-ARQPESJD       PIC  9(09) COMP-3   VALUE ZEROS. 
       77  ACU-GRAVADOS-ARQPESFS       PIC  9(09) COMP-3   VALUE ZEROS. 
       77  ACU-GRAVADOS-ARQCTCOR       PIC  9(09) COMP-3   VALUE ZEROS. 
       77  ACU-GRAVADOS-ARQLOGDR       PIC  9(09) COMP-3   VALUE ZEROS. 
       77  ACU-GRAVADOS-ARQFONES       PIC  9(09) COMP-3   VALUE ZEROS. 
       77  ACU-SEQUENCIA               PIC  9(09) COMP-3   VALUE ZEROS. 
                                                                        
      *---- VARIAVEIS AUXILIARES -------------------------------------* 
                                                                        
       77  WRK-BATCH                   PIC  X(08)         VALUE 'BATCH'.
       77  WRK-EDITADO                 PIC  ZZZ.ZZZ.ZZ9.                
       77  WRK-TEM-FISI                PIC  X(01)         VALUE 'N'.    
       77  WRK-TEM-JURI                PIC  X(01)         VALUE 'N'.    
       77  WRK-TEM-FONE                PIC  X(01)         VALUE 'N'.    
       77  WRK-TEM-CCOR                PIC  X(01)         VALUE 'N'.    
       77  WRK-TEM-LOGR                PIC  X(01)         VALUE 'N'.    
       77  WRK-HATULZ                  PIC  X(10)         VALUE SPACES. 
                                                                        
RON    77  WRK-MODULO              PIC X(08) VALUE SPACES.              
                                                                        
      *---- AREA DE FILE STATUS --------------------------------------* 
                                                                        
       77  WRK-FS-TABASUNI             PIC  X(02)          VALUE SPACES.
       77  WRK-FS-ARQPESJD             PIC  X(02)          VALUE SPACES.
       77  WRK-FS-ARQPESFS             PIC  X(02)          VALUE SPACES.
       77  WRK-FS-ARQCTCOR             PIC  X(02)          VALUE SPACES.
       77  WRK-FS-ARQLOGDR             PIC  X(02)          VALUE SPACES.
       77  WRK-FS-ARQFONES             PIC  X(02)          VALUE SPACES.
       77  WRK-FS-REGTOTAL             PIC  X(02)          VALUE SPACES.
                                                                        
       77  WRK-ABERTURA                PIC  X(13)          VALUE        
           ' NA ABERTURA'.                                              
       77  WRK-LEITURA                 PIC  X(13)          VALUE        
           ' NA LEITURA'.                                               
       77  WRK-GRAVACAO                PIC  X(13)          VALUE        
           ' NA GRAVACAO'.                                              
       77  WRK-FECHAMENTO              PIC  X(13)          VALUE        
           'NO FECHAMENTO'.                                             
                                                                        
      *---- AREA DE CHAVES -------------------------------------------* 
                                                                        
       01  WRK-CHV-TABASUNI-ATU.                                        
           03  WRK-CHV-SEQ-ATU.                                         
               05  WRK-CHV-BANCO-ATU   PIC  9(03)          VALUE ZEROS. 
               05  WRK-CHV-AGENCIA-ATU PIC  9(05)          VALUE ZEROS. 
               05  WRK-CHV-CONTA-ATU   PIC  9(13)          VALUE ZEROS. 
                                                                        
       01  WRK-CHV-TABASUNI-ANT.                                        
           03  WRK-CHV-SEQ-ANT.                                         
               05  WRK-CHV-BANCO-ANT   PIC  9(03)          VALUE ZEROS. 
               05  WRK-CHV-AGENCIA-ANT PIC  9(05)          VALUE ZEROS. 
               05  WRK-CHV-CONTA-ANT   PIC  9(13)          VALUE ZEROS. 
                                                                        
      *---- AREA DA POOL7600 -----------------------------------------* 
                                                                        
       01  WRK-DATA-HORA.                                               
           03  WRK-DT-JULIANA          PIC  9(05) COMP-3   VALUE ZEROS. 
           03  WRK-DT-AAMMDD           PIC  9(07) COMP-3   VALUE ZEROS. 
           03  WRK-DT-AAAAMMDD         PIC  9(09) COMP-3   VALUE ZEROS. 
           03  WRK-TI-HHMMSS           PIC  9(07) COMP-3   VALUE ZEROS. 
           03  WRK-TI-HHMMSSMMMMMM     PIC  9(13) COMP-3   VALUE ZEROS. 
           03  WRK-TIMESTAMP           PIC  X(20)          VALUE SPACES.
                                                                        
       01  WRK-DATA                    PIC  9(08)          VALUE ZEROS. 
       01  FILLER REDEFINES WRK-DATA.                                   
           03  WRK-ANO                 PIC  9(04).                      
           03  WRK-MES                 PIC  9(02).                      
           03  WRK-DIA                 PIC  9(02).                      
                                                                        
      *---- AREA DE ERRO DE FILE STATUS ------------------------------* 
                                                                        
       01  WRK-ERRO-FS.                                                 
           03  FILLER                  PIC  X(05)          VALUE        
               'ERRO '.                                                 
           03  WRK-OPERACAO            PIC  X(13).                      
           03  FILLER                  PIC  X(12)          VALUE        
               ' DO ARQUIVO '.                                          
           03  WRK-ARQUIVO             PIC  X(08).                      
           03  FILLER                  PIC  X(17)          VALUE        
               ' - FILE STATUS = '.                                     
           03  WRK-FS                  PIC  X(02).                      
                                                                        
      *---- CABECALHOS -----------------------------------------------* 
                                                                        
       01  CABEC1.                                                      
           03  CB1-CARRO               PIC  X(01)          VALUE '1'.   
           03  FILLER                  PIC  X(22)          VALUE        
               'RDAB0150            B'.                                 
           03  FILLER                  PIC  X(21)          VALUE        
               'A N C O  B R A D E S'.                                  
           03  FILLER                  PIC  X(27)          VALUE        
               'C O  S / A'.                                            
           03  CB1-DIA                 PIC  9(02)          VALUE ZEROS. 
           03  FILLER                  PIC  X(01)          VALUE '/'.   
           03  CB1-MES                 PIC  9(02)          VALUE ZEROS. 
           03  FILLER                  PIC  X(01)          VALUE '/'.   
           03  CB1-ANO                 PIC  9(04)          VALUE ZEROS. 
                                                                        
       01  CABEC2.                                                      
           03  CB2-CARRO               PIC  X(01)          VALUE ' '.   
           03  FILLER                  PIC  X(22)          VALUE SPACES.
           03  FILLER                  PIC  X(21)          VALUE        
               'RELATORIO DE TOTALIZA'.                                 
           03  FILLER                  PIC  X(04)          VALUE 'COES'.
                                                                        
      *---- LINHAS DE TOTAIS -----------------------------------------* 
                                                                        
       01  LINTOT1.                                                     
           03  LT1-CARRO               PIC  X(01)          VALUE '0'.   
           03  FILLER                  PIC  X(21)          VALUE        
               '         QUANTIDADE D'.                                 
           03  FILLER                  PIC  X(21)          VALUE        
               'E REGISTROS LIDOS'.                                     
           03  FILLER                  PIC  X(15)          VALUE        
               'NO TABASUNI..:'.                                        
           03  LT1-TOTAL               PIC  ZZZ.ZZZ.ZZ9.                
                                                                        
       01  LINTOT2.                                                     
           03  LT2-CARRO               PIC  X(01)          VALUE ' '.   
           03  FILLER                  PIC  X(21)          VALUE        
               '         QUANTIDADE D'.                                 
           03  FILLER                  PIC  X(21)          VALUE        
               'E REGISTROS GRAVADOS'.                                  
           03  FILLER                  PIC  X(15)          VALUE        
               'NO ARQPESJD..:'.                                        
           03  LT2-TOTAL               PIC  ZZZ.ZZZ.ZZ9.                
                                                                        
       01  LINTOT3.                                                     
           03  LT3-CARRO               PIC  X(01)          VALUE ' '.   
           03  FILLER                  PIC  X(21)          VALUE        
               '         QUANTIDADE D'.                                 
           03  FILLER                  PIC  X(21)          VALUE        
               'E REGISTROS GRAVADOS'.                                  
           03  FILLER                  PIC  X(15)          VALUE        
               'NO ARQPESFS..:'.                                        
           03  LT3-TOTAL               PIC  ZZZ.ZZZ.ZZ9.                
                                                                        
       01  LINTOT4.                                                     
           03  LT4-CARRO               PIC  X(01)          VALUE ' '.   
           03  FILLER                  PIC  X(21)          VALUE        
               '         QUANTIDADE D'.                                 
           03  FILLER                  PIC  X(21)          VALUE        
               'E REGISTROS GRAVADOS'.                                  
           03  FILLER                  PIC  X(15)          VALUE        
               'NO ARQCTCOR..:'.                                        
           03  LT4-TOTAL               PIC  ZZZ.ZZZ.ZZ9.                
                                                                        
       01  LINTOT5.                                                     
           03  LT5-CARRO               PIC  X(01)          VALUE ' '.   
           03  FILLER                  PIC  X(21)          VALUE        
               '         QUANTIDADE D'.                                 
           03  FILLER                  PIC  X(21)          VALUE        
               'E REGISTROS GRAVADOS'.                                  
           03  FILLER                  PIC  X(15)          VALUE        
               'NO ARQLOGDR..:'.                                        
           03  LT5-TOTAL               PIC  ZZZ.ZZZ.ZZ9.                
                                                                        
       01  LINTOT6.                                                     
           03  LT6-CARRO               PIC  X(01)          VALUE ' '.   
           03  FILLER                  PIC  X(21)          VALUE        
               '         QUANTIDADE D'.                                 
           03  FILLER                  PIC  X(21)          VALUE        
               'E REGISTROS GRAVADOS'.                                  
           03  FILLER                  PIC  X(15)          VALUE        
               'NO ARQFONES..:'.                                        
           03  LT6-TOTAL               PIC  ZZZ.ZZZ.ZZ9.                
                                                                        
      *---- AREA DA POOL7100 -----------------------------------------* 
                                                                        
-INC POL7100C                                                           
                                                                        
                                                                        
RON   *----  AREA DE ACESSO AO MODULO CLIB2000 ------------------------*
RON                                                                     
RON    01  WRK-AREA-ENTRAD1.                                            
RON        03  WRK-TRANSACA1           PIC 9(05) COMP-3 VALUE ZEROS.    
RON        03  WRK-VERSA1              PIC X(06)        VALUE SPACES.   
RON        03  WRK-PRODUT1             PIC 9(05) COMP-3 VALUE ZEROS.    
RON        03  WRK-AREA-TRABALH1.                                       
RON            05 WRK-BANC1            PIC 9(05) COMP-3 VALUE ZEROS.    
RON            05 WRK-AGENCI1          PIC 9(05) COMP-3 VALUE ZEROS.    
RON            05 WRK-CONT1            PIC 9(13) COMP-3 VALUE ZEROS.    
RON        03  FILLER                  PIC X(95)        VALUE SPACES.   
RON                                                                     
RON    01  WRK-AREA-SAID1.                                              
RON        03  WRK-CODIGO-RETORN1      PIC 9(03) COMP-3 VALUE ZEROS.    
RON        03  WRK-MENSAGEM-SAID1      PIC X(998)       VALUE SPACES.   
RON                                                                     
RON   *----  INCLUDE PARA ACESSO AO MODULO CLIB2000 -------------------*
-INC I#CLIB21                                                           
                                                                        
       01  FILLER                      PIC  X(32)          VALUE        
           '*  FIM DA WORKING RDAB0150  *'.                             
                                                                        
      *===============================================================* 
       PROCEDURE                       DIVISION.                        
      *===============================================================* 
                                                                        
      *---------------------------------------------------------------* 
       0000-INICIAR                    SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           PERFORM 1000-INICIALIZAR.                                    
                                                                        
           PERFORM 2000-VERIFICAR-VAZIO.                                
                                                                        
           PERFORM 3000-PROCESSAR UNTIL WRK-FS-TABASUNI EQUAL '10'.     
                                                                        
           PERFORM 3100-VER-QUAL-GRAVAR                                 
                                                                        
           PERFORM 8000-FINALIZAR.                                      
                                                                        
      *---------------------------------------------------------------* 
       0000-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1000-INICIALIZAR                SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           OPEN INPUT TABASUNI                                          
               OUTPUT ARQPESJD                                          
                      ARQPESFS                                          
                      ARQCTCOR                                          
                      ARQLOGDR                                          
                      ARQFONES                                          
                      REGTOTAL.                                         
                                                                        
           MOVE WRK-ABERTURA           TO  WRK-OPERACAO.                
           PERFORM 1100-TESTAR-FILE-STATUS.                             
                                                                        
      *---------------------------------------------------------------* 
       1000-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1100-TESTAR-FILE-STATUS         SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           PERFORM 1110-TESTAR-FS-TABASUNI.                             
                                                                        
           PERFORM 1120-TESTAR-FS-ARQPESJD.                             
                                                                        
           PERFORM 1130-TESTAR-FS-ARQPESFS.                             
                                                                        
           PERFORM 1140-TESTAR-FS-ARQCTCOR.                             
                                                                        
           PERFORM 1150-TESTAR-FS-ARQLOGDR.                             
                                                                        
           PERFORM 1160-TESTAR-FS-ARQFONES.                             
                                                                        
           PERFORM 1170-TESTAR-FS-REGTOTAL.                             
                                                                        
      *---------------------------------------------------------------* 
       1100-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1110-TESTAR-FS-TABASUNI         SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           IF  WRK-FS-TABASUNI         NOT EQUAL '00'                   
               MOVE 'TABASUNI'         TO  WRK-ARQUIVO                  
               MOVE WRK-FS-TABASUNI    TO  WRK-FS                       
               MOVE 'APL'              TO  ERR-TIPO-ACESSO              
               MOVE WRK-ERRO-FS        TO  ERR-TEXTO                    
               PERFORM 9999-ROTINA-ERRO                                 
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       1110-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1120-TESTAR-FS-ARQPESJD         SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           IF  WRK-FS-ARQPESJD         NOT EQUAL '00'                   
               MOVE 'ARQPESJD'         TO  WRK-ARQUIVO                  
               MOVE WRK-FS-ARQPESJD    TO  WRK-FS                       
               MOVE 'APL'              TO  ERR-TIPO-ACESSO              
               MOVE WRK-ERRO-FS        TO  ERR-TEXTO                    
               PERFORM 9999-ROTINA-ERRO                                 
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       1120-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1130-TESTAR-FS-ARQPESFS         SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           IF  WRK-FS-ARQPESFS         NOT EQUAL '00'                   
               MOVE 'ARQPESFS'         TO  WRK-ARQUIVO                  
               MOVE WRK-FS-ARQPESFS    TO  WRK-FS                       
               MOVE 'APL'              TO  ERR-TIPO-ACESSO              
               MOVE WRK-ERRO-FS        TO  ERR-TEXTO                    
               PERFORM 9999-ROTINA-ERRO                                 
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       1130-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1140-TESTAR-FS-ARQCTCOR         SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           IF  WRK-FS-ARQCTCOR         NOT EQUAL '00'                   
               MOVE 'ARQCTCOR'         TO  WRK-ARQUIVO                  
               MOVE WRK-FS-ARQCTCOR    TO  WRK-FS                       
               MOVE 'APL'              TO  ERR-TIPO-ACESSO              
               MOVE WRK-ERRO-FS        TO  ERR-TEXTO                    
               PERFORM 9999-ROTINA-ERRO                                 
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       1140-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1150-TESTAR-FS-ARQLOGDR         SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           IF  WRK-FS-ARQLOGDR         NOT EQUAL '00'                   
               MOVE 'ARQLOGDR'         TO  WRK-ARQUIVO                  
               MOVE WRK-FS-ARQLOGDR    TO  WRK-FS                       
               MOVE 'APL'              TO  ERR-TIPO-ACESSO              
               MOVE WRK-ERRO-FS        TO  ERR-TEXTO                    
               PERFORM 9999-ROTINA-ERRO                                 
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       1150-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1160-TESTAR-FS-ARQFONES         SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           IF  WRK-FS-ARQFONES         NOT EQUAL '00'                   
               MOVE 'ARQFONES'         TO  WRK-ARQUIVO                  
               MOVE WRK-FS-ARQFONES    TO  WRK-FS                       
               MOVE 'APL'              TO  ERR-TIPO-ACESSO              
               MOVE WRK-ERRO-FS        TO  ERR-TEXTO                    
               PERFORM 9999-ROTINA-ERRO                                 
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       1160-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1170-TESTAR-FS-REGTOTAL         SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           IF  WRK-FS-REGTOTAL         NOT EQUAL '00'                   
               MOVE 'REGTOTAL'         TO  WRK-ARQUIVO                  
               MOVE WRK-FS-REGTOTAL    TO  WRK-FS                       
               MOVE 'APL'              TO  ERR-TIPO-ACESSO              
               MOVE WRK-ERRO-FS        TO  ERR-TEXTO                    
               PERFORM 9999-ROTINA-ERRO                                 
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       1170-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       2000-VERIFICAR-VAZIO            SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           PERFORM 2100-LER-TABASUNI.                                   
                                                                        
           IF  WRK-FS-TABASUNI         EQUAL '10'                       
               DISPLAY '*********** RDAB0150 ***********'               
               DISPLAY '*                              *'               
               DISPLAY '*    ARQUIVO TABASUNI VAZIO    *'               
               DISPLAY '*      PROGRAMA ENCERRADO      *'               
               DISPLAY '*                              *'               
               DISPLAY '*********** RDAB0150 ***********'               
               PERFORM 8000-FINALIZAR                                   
           END-IF.                                                      
                                                                        
           INITIALIZE ACC-REG.                                          
           INITIALIZE APJ-REG.                                          
           INITIALIZE APF-REG.                                          
           INITIALIZE ALG-REG.                                          
           INITIALIZE AFN-REG.                                          
                                                                        
           MOVE WRK-CHV-TABASUNI-ATU   TO  WRK-CHV-TABASUNI-ANT.        
                                                                        
      *---------------------------------------------------------------* 
       2000-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       2100-LER-TABASUNI               SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           READ TABASUNI.                                               
                                                                        
           IF  WRK-FS-TABASUNI         EQUAL '10'                       
               GO TO 2100-99-FIM                                        
           END-IF.                                                      
                                                                        
           MOVE WRK-LEITURA            TO  WRK-OPERACAO.                
           PERFORM 1110-TESTAR-FS-TABASUNI.                             
                                                                        
           MOVE CBCO                   TO  WRK-CHV-BANCO-ATU.           
           MOVE CAG-BCRIA              TO  WRK-CHV-AGENCIA-ATU.         
           MOVE CCTA-CORR              TO  WRK-CHV-CONTA-ATU.           
                                                                        
           IF  HATULZ          NOT EQUAL SPACES  AND                    
                               '0000000000'                             
               MOVE HATULZ            TO  WRK-HATULZ.                   
                                                                        
           ADD 1                      TO  ACU-LIDOS-TABASUNI.           
                                                                        
      *---------------------------------------------------------------* 
       2100-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       3000-PROCESSAR                  SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           IF  WRK-CHV-TABASUNI-ATU    NOT EQUAL  WRK-CHV-TABASUNI-ANT  
               PERFORM 3100-VER-QUAL-GRAVAR                             
               MOVE ZEROS          TO  ACU-SEQUENCIA                    
               MOVE HATULZ         TO  WRK-HATULZ                       
               END-IF.                                                  
                                                                        
           PERFORM  6800-MOVER-CONTA-CORRENTE                           
                                                                        
           PERFORM  7000-TESTAR-PESSOAS                                 
                                                                        
           PERFORM  7600-MOVER-LOGRADOURO.                              
                                                                        
           PERFORM  7800-MOVER-ARQFONES.                                
                                                                        
           PERFORM  2100-LER-TABASUNI.                                  
                                                                        
      *---------------------------------------------------------------* 
       3000-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       3100-VER-QUAL-GRAVAR            SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           IF   WRK-TEM-CCOR      EQUAL    'S'                          
                PERFORM           6400-GRAVAR-CONTA-CORRENTE.           
                                                                        
           IF   WRK-TEM-JURI      EQUAL    'S'                          
                PERFORM           6200-GRAVAR-PESSOA-JURIDICA.          
                                                                        
           IF   WRK-TEM-FISI      EQUAL    'S'                          
                PERFORM           6000-GRAVAR-PESSOA-FISICA.            
                                                                        
           MOVE   WRK-CHV-TABASUNI-ATU   TO   WRK-CHV-TABASUNI-ANT.     
                                                                        
           INITIALIZE ACC-REG.                                          
           INITIALIZE APJ-REG.                                          
           INITIALIZE APF-REG.                                          
           INITIALIZE ALG-REG.                                          
           INITIALIZE AFN-REG.                                          
           MOVE   'N'  TO   WRK-TEM-CCOR   WRK-TEM-FISI  WRK-TEM-JURI.  
                                                                        
      *---------------------------------------------------------------* 
       3100-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       5600-GRAVAR-ARQFONES           SECTION.                          
      *---------------------------------------------------------------* 
                                                                        
           WRITE AFN-REG.                                               
           MOVE WRK-GRAVACAO           TO  WRK-OPERACAO.                
           PERFORM 1160-TESTAR-FS-ARQFONES.                             
                                                                        
           ADD 1                       TO  ACU-GRAVADOS-ARQFONES.       
                                                                        
      *---------------------------------------------------------------* 
       5600-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       5800-GRAVAR-LOGRADOURO         SECTION.                          
      *---------------------------------------------------------------* 
                                                                        
           WRITE ALG-REG.                                               
           MOVE WRK-GRAVACAO           TO  WRK-OPERACAO.                
           PERFORM 1150-TESTAR-FS-ARQLOGDR.                             
                                                                        
           ADD 1                       TO  ACU-GRAVADOS-ARQLOGDR.       
                                                                        
      *---------------------------------------------------------------* 
       5800-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       6000-GRAVAR-PESSOA-FISICA      SECTION.                          
      *---------------------------------------------------------------* 
                                                                        
           IF  APF-ICONJG-CLI-RENEG    EQUAL SPACES                     
               MOVE '?'                TO  APF-ICONJG-NULL.             
                                                                        
           IF  APF-IPAI-CLI-RENEG      EQUAL SPACES                     
               MOVE '?'                TO  APF-IPAI-NULL.               
                                                                        
           IF  APF-IMAE-CLI-RENEG      EQUAL SPACES                     
               MOVE '?'                TO  APF-IMAE-NULL.               
                                                                        
           IF  APF-ICONJG-CLI-RENEG    EQUAL SPACES                     
               MOVE '?'                TO  APF-ICONJG-NULL.             
                                                                        
RON   *    PERFORM 8900-ACESSAR-ROTINA-CLIB2000
                                                                        
           WRITE APF-REG.                                               
           MOVE WRK-GRAVACAO           TO  WRK-OPERACAO.                
                                                                        
           PERFORM 1130-TESTAR-FS-ARQPESFS.                             
                                                                        
           ADD 1                       TO  ACU-GRAVADOS-ARQPESFS.       
                                                                        
      *---------------------------------------------------------------* 
       6000-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       6200-GRAVAR-PESSOA-JURIDICA    SECTION.                          
      *---------------------------------------------------------------* 
                                                                        
           WRITE APJ-REG.                                               
           MOVE WRK-GRAVACAO           TO  WRK-OPERACAO.                
                                                                        
           PERFORM 1120-TESTAR-FS-ARQPESJD.                             
                                                                        
           ADD 1                       TO  ACU-GRAVADOS-ARQPESJD.       
                                                                        
      *---------------------------------------------------------------* 
       6200-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       6400-GRAVAR-CONTA-CORRENTE     SECTION.                          
      *---------------------------------------------------------------* 
                                                                        
           IF  ACC-IOUTRO-TTLAR        EQUAL SPACES                     
               MOVE '?'                TO  ACC-IOUTRO-TTLAR-NULL.       
                                                                        
           WRITE ACC-REG.                                               
           MOVE WRK-GRAVACAO           TO  WRK-OPERACAO.                
           PERFORM 1140-TESTAR-FS-ARQCTCOR.                             
                                                                        
           ADD 1                       TO  ACU-GRAVADOS-ARQCTCOR.       
                                                                        
      *---------------------------------------------------------------* 
       6400-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       6800-MOVER-CONTA-CORRENTE      SECTION.                          
      *---------------------------------------------------------------* 
                                                                        
           IF  CBASE-CPF               NOT EQUAL ZEROS                  
               MOVE CBASE-CPF          TO  ACC-CBASE-CPF                
           END-IF.                                                      
                                                                        
           IF  CFILIAL-CNPJ            NOT EQUAL ZEROS                  
               MOVE CFILIAL-CNPJ       TO  ACC-CFLIAL-CNPJ              
           END-IF.                                                      
                                                                        
           IF  CCTRL-CNPJ-CPF          NOT EQUAL ZEROS                  
               MOVE CCTRL-CNPJ-CPF     TO  ACC-CCTRL-CNPJ-CPF           
           END-IF.                                                      
                                                                        
           IF  CBCO                    NOT EQUAL ZEROS                  
               MOVE CBCO               TO  ACC-CBCO                     
           END-IF.                                                      
                                                                        
           IF  CAG-BCRIA               NOT EQUAL ZEROS                  
               MOVE CAG-BCRIA          TO  ACC-CAG-BCRIA                
           END-IF.                                                      
                                                                        
           IF  CCTA-CORR               NOT EQUAL ZEROS                  
               MOVE   'S'              TO  WRK-TEM-CCOR                 
               MOVE CCTA-CORR          TO  ACC-CCTA-CORR                
           END-IF.                                                      
                                                                        
           IF  CIDTFD-TPO-PSSOA        NOT EQUAL SPACES                 
               MOVE CIDTFD-TPO-PSSOA   TO  ACC-CIDTFD-TPO-PSSOA         
           END-IF.                                                      
                                                                        
RON        IF  DABERT-CTA  OF  ADP-REGISTRO    NOT EQUAL SPACES         
                                               AND '0000000000'         
RON            MOVE DABERT-CTA OF  ADP-REGISTRO TO  ACC-DABERT-CTA-RENEG
           ELSE                                                         
               IF  ACC-DABERT-CTA-RENEG  NOT EQUAL  '01.01.0001'        
                                             AND    SPACES              
                                             AND    '0000000000'        
                   NEXT SENTENCE                                        
               ELSE                                                     
                   MOVE '01.01.0001'    TO   ACC-DABERT-CTA-RENEG       
               END-IF                                                   
           END-IF.                                                      
                                                                        
           IF  IOUTRO-TTLAR            NOT EQUAL SPACES                 
               MOVE IOUTRO-TTLAR       TO  ACC-IOUTRO-TTLAR             
           END-IF.                                                      
                                                                        
           IF  CINDCD-EXTR-BLOQ        NOT EQUAL SPACES                 
               MOVE CINDCD-EXTR-BLOQ   TO  ACC-CINDCD-EXTR-BLOQ         
           END-IF.                                                      
                                                                        
           IF  CINDCD-CTA-GERC         NOT EQUAL SPACES                 
               MOVE CINDCD-CTA-GERC    TO  ACC-CINDCD-CTA-GERC          
           END-IF.                                                      
                                                                        
           IF  CINDCD-PRVTE            NOT EQUAL SPACES                 
               MOVE CINDCD-PRVTE       TO  ACC-CINDCD-PRVTE             
           END-IF.                                                      
                                                                        
           IF  CLETRA-RTING            NOT EQUAL SPACES                 
               MOVE CLETRA-RTING       TO  ACC-CLETRA-RTING-RENEG       
           END-IF.                                                      
                                                                        
           IF  CRAMO-ATVDD             NOT EQUAL ZEROS                  
               MOVE CRAMO-ATVDD        TO  ACC-CRAMO-ATVDD-RENEG        
           END-IF.                                                      
                                                                        
           IF  WRK-HATULZ               NOT EQUAL SPACES  AND           
                                           '0000000000'                 
               MOVE WRK-HATULZ          TO  ACC-HATULZ                  
           ELSE                                                         
               IF  ACC-HATULZ  NOT EQUAL  '01.01.0001'                  
                                     AND    SPACES                      
                                     AND    '0000000000'                
                   NEXT SENTENCE                                        
               ELSE                                                     
                   MOVE '01.01.0001'       TO ACC-HATULZ                
               END-IF                                                   
           END-IF.                                                      
                                                                        
           IF  CPOSTO-SERVC            NOT EQUAL ZEROS                  
               MOVE CPOSTO-SERVC       TO  ACC-CPOSTO-SERVC             
           END-IF.                                                      
                                                                        
           MOVE ZEROS                  TO  ACC-CTPO-POSTO-SERVC.        
                                                                        
           IF  CSGMTO-CLI              NOT EQUAL ZEROS                  
               MOVE CSGMTO-CLI         TO  ACC-CSGMTO-CLI               
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       6800-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       7000-TESTAR-PESSOAS             SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           IF  CFILIAL-CNPJ            NOT EQUAL ZEROS                  
               PERFORM 7200-MOVER-PESSOA-JURIDICA                       
           ELSE                                                         
               PERFORM 7400-MOVER-PESSOA-FISICA                         
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       7000-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       7200-MOVER-PESSOA-JURIDICA     SECTION.                          
      *---------------------------------------------------------------* 
                                                                        
           IF  CBASE-CPF               NOT EQUAL ZEROS                  
               MOVE CBASE-CPF          TO  APJ-CBASE-CPF                
           END-IF.                                                      
                                                                        
           IF  CFILIAL-CNPJ            NOT EQUAL ZEROS                  
               MOVE CFILIAL-CNPJ       TO  APJ-CFLIAL-CNPJ              
           END-IF.                                                      
                                                                        
           IF  CCTRL-CNPJ-CPF          NOT EQUAL ZEROS                  
               MOVE CCTRL-CNPJ-CPF     TO  APJ-CCTRL-CNPJ-CPF           
           END-IF.                                                      
                                                                        
           IF  DFUNDC-EMPR             NOT EQUAL SPACES                 
                                             AND '0000000000'           
               MOVE DFUNDC-EMPR        TO  APJ-DFUNDC-EMPR-RENEG        
           ELSE                                                         
               IF APJ-DFUNDC-EMPR-RENEG  NOT EQUAL  '01.01.0001'        
                                               AND  SPACES              
                                               AND  '0000000000'        
                  NEXT SENTENCE                                         
               ELSE                                                     
                  MOVE '01.01.0001'       TO APJ-DFUNDC-EMPR-RENEG      
               END-IF                                                   
           END-IF.                                                      
                                                                        
           IF  VFATMT-EMPR             NOT EQUAL ZEROS                  
                MOVE VFATMT-EMPR       TO  APJ-VFATMT-EMPR-RENEG        
           END-IF.                                                      
                                                                        
           IF  IPRIM-TTLAR             NOT EQUAL SPACES                 
               MOVE IPRIM-TTLAR        TO  APJ-IPRIM-TTLAR-RENEG        
               MOVE   'S'              TO  WRK-TEM-JURI                 
           END-IF.                                                      
                                                                        
           IF  WRK-HATULZ              NOT EQUAL SPACES                 
                                                 AND '0000000000'       
               MOVE WRK-HATULZ         TO  APJ-HATULZ                   
           ELSE                                                         
               IF  APJ-HATULZ  NOT EQUAL  '01.01.0001'                  
                                    AND    SPACES                       
                                    AND    '0000000000'                 
                   NEXT SENTENCE                                        
               ELSE                                                     
                   MOVE '01.01.0001'       TO APJ-HATULZ                
               END-IF                                                   
           END-IF.                                                      
                                                                        
           MOVE '0'                    TO  APJ-CINDCD-COBR-RENEG        
                                           APJ-CINDCD-RENEG-BASE.       
                                                                        
      *---------------------------------------------------------------* 
       7200-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       7400-MOVER-PESSOA-FISICA       SECTION.                          
      *---------------------------------------------------------------* 
                                                                        
           IF  CBASE-CPF               NOT EQUAL ZEROS                  
               MOVE CBASE-CPF          TO  APF-CBASE-CPF                
           END-IF.                                                      
                                                                        
           IF  CCTRL-CNPJ-CPF          NOT EQUAL ZEROS                  
               MOVE CCTRL-CNPJ-CPF     TO  APF-CCTRL-CNPJ-CPF           
           END-IF.                                                      
                                                                        
           IF  DFUNDC-EMPR             NOT EQUAL SPACES                 
                                             AND '0000000000'           
               MOVE DFUNDC-EMPR        TO  APF-DNASC-CLI-RENEG          
           ELSE                                                         
              IF APF-DNASC-CLI-RENEG  NOT EQUAL '01.01.0001'            
                                            AND    SPACES               
                                            AND    '0000000000'         
                 NEXT SENTENCE                                          
              ELSE                                                      
                 MOVE '01.01.0001'       TO APF-DNASC-CLI-RENEG         
              END-IF                                                    
           END-IF.                                                      
                                                                        
           IF  VFATMT-EMPR             NOT EQUAL ZEROS                  
               MOVE VFATMT-EMPR        TO  APF-VRENDA-CLI-RENEG         
           END-IF.                                                      
                                                                        
           IF  IPAI-CLI                NOT EQUAL SPACES                 
               MOVE IPAI-CLI           TO  APF-IPAI-CLI-RENEG           
           END-IF.                                                      
                                                                        
           IF  IMAE-CLI                NOT EQUAL SPACES                 
               MOVE IMAE-CLI           TO  APF-IMAE-CLI-RENEG           
           END-IF.                                                      
                                                                        
           IF  ICONJG-CLI              NOT EQUAL SPACES                 
               MOVE ICONJG-CLI         TO  APF-ICONJG-CLI-RENEG         
           END-IF.                                                      
                                                                        
           IF  IPRIM-TTLAR             NOT EQUAL SPACES                 
               MOVE   'S'              TO  WRK-TEM-FISI                 
               MOVE IPRIM-TTLAR        TO  APF-IPRIM-TTLAR-RENEG        
           END-IF.                                                      
                                                                        
           IF  WRK-HATULZ              NOT EQUAL SPACES                 
                                             AND '0000000000'           
               MOVE WRK-HATULZ         TO  APF-HATULZ                   
           ELSE                                                         
               IF  APF-HATULZ  NOT EQUAL   '01.01.0001'                 
                                     AND    SPACES                      
                                     AND    '0000000000'                
                   NEXT SENTENCE                                        
               ELSE                                                     
                   MOVE '01.01.0001'       TO APF-HATULZ                
               END-IF                                                   
           END-IF.                                                      
                                                                        
           MOVE '0'                    TO  APF-CINDCD-COBR-RENEG        
                                           APF-CINDCD-RENEG-BASE.       
                                                                        
      *---------------------------------------------------------------* 
       7400-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       7600-MOVER-LOGRADOURO          SECTION.                          
      *---------------------------------------------------------------* 
                                                                        
           MOVE CBCO                   TO  ALG-CBCO.                    
           MOVE CAG-BCRIA              TO  ALG-CAG-BCRIA.               
           MOVE CCTA-CORR              TO  ALG-CCTA-CORR.               
                                                                        
           IF  CINDCD-ORIGE            NOT EQUAL SPACES                 
               MOVE CINDCD-ORIGE       TO  ALG-CINDCD-ORIGE-LOGDR       
           END-IF.                                                      
                                                                        
           IF  CINDCD-TPO-LOGDR        NOT EQUAL SPACES                 
               MOVE CINDCD-TPO-LOGDR   TO  ALG-CINDCD-TPO-LOGDR         
           END-IF.                                                      
                                                                        
           IF  ENUM-LOGDR              NOT EQUAL SPACES                 
               MOVE ENUM-LOGDR         TO  ALG-ENRO-LOGDR-RENEG         
           END-IF.                                                      
                                                                        
           IF  ECOMPL-LOGDR            NOT EQUAL SPACES                 
               MOVE ECOMPL-LOGDR       TO  ALG-ECOMPL-LOGDR-RENEG       
           END-IF.                                                      
                                                                        
           IF  EBAIRO-LOGDR            NOT EQUAL SPACES                 
               MOVE EBAIRO-LOGDR       TO  ALG-EBAIRO-LOGDR-RENEG       
           END-IF.                                                      
                                                                        
           IF  IMUN-IBGE               NOT EQUAL SPACES                 
               MOVE IMUN-IBGE          TO  ALG-IMUN-IBGE-RENEG          
           END-IF.                                                      
                                                                        
           IF  CCEP-CLI                NOT EQUAL ZEROS                  
               MOVE CCEP-CLI           TO  ALG-CCEP-CLI-RENEG           
           END-IF.                                                      
                                                                        
           IF  CCEP-COMPL              NOT EQUAL ZEROS                  
               MOVE CCEP-COMPL         TO  ALG-CCEP-COMPL-RENEG         
           END-IF.                                                      
                                                                        
           IF  CSGL-UF-CLI             NOT EQUAL SPACES                 
               MOVE CSGL-UF-CLI        TO  ALG-CSGL-UF-CLI-RENEG        
           END-IF.                                                      
                                                                        
           MOVE '2'                    TO  ALG-CINDCD-LOGDR             
                                           ALG-CINDCD-ATULZ.            
                                                                        
           IF  WRK-HATULZ              NOT EQUAL SPACES                 
                                             AND '0000000000'           
               MOVE WRK-HATULZ         TO  ALG-HATULZ                   
           ELSE                                                         
               IF  ALG-HATULZ  NOT EQUAL  '01.01.0001'                  
                                     AND    SPACES                      
                                     AND    '0000000000'                
                   NEXT SENTENCE                                        
               ELSE                                                     
                   MOVE '01.01.0001'       TO ALG-HATULZ                
               END-IF                                                   
           END-IF.                                                      
                                                                        
           IF  ELOGDR-CLI              NOT EQUAL SPACES                 
               MOVE ELOGDR-CLI         TO  ALG-ELOGDR-CLI-RENEG         
               PERFORM   5800-GRAVAR-LOGRADOURO                         
               INITIALIZE ALG-REG                                       
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       7600-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       7800-MOVER-ARQFONES            SECTION.                          
      *---------------------------------------------------------------* 
                                                                        
           MOVE CBCO                   TO  AFN-CBCO.                    
           MOVE CAG-BCRIA              TO  AFN-CAG-BCRIA.               
           MOVE CCTA-CORR              TO  AFN-CCTA-CORR.               
                                                                        
           IF  CINDCD-ORIGE            NOT EQUAL SPACES                 
               MOVE CINDCD-ORIGE       TO  AFN-CINDCD-ORIGE-FONE        
           END-IF.                                                      
                                                                        
           IF  CINDCD-TPO-FONE         NOT EQUAL SPACES                 
               MOVE CINDCD-TPO-FONE    TO  AFN-CINDCD-TPO-FONE          
           END-IF.                                                      
                                                                        
           IF  CDDD-CLI                NOT EQUAL SPACES                 
               MOVE CDDD-CLI           TO  AFN-CDDD-CLI-RENEG           
           END-IF.                                                      
                                                                        
           IF  WRK-HATULZ              NOT EQUAL SPACES                 
                                             AND '0000000000'           
               MOVE WRK-HATULZ         TO  AFN-HATULZ                   
           ELSE                                                         
               IF  AFN-HATULZ  NOT EQUAL  '01.01.0001'                  
                                     AND    SPACES                      
                                     AND    '0000000000'                
                   NEXT SENTENCE                                        
               ELSE                                                     
                   MOVE '01.01.0001'       TO AFN-HATULZ                
               END-IF                                                   
           END-IF.                                                      
                                                                        
           MOVE '2'                    TO  AFN-CINDCD-FONE              
                                           AFN-CINDCD-ATULZ.            
                                                                        
           IF  CFONE                   NOT EQUAL SPACES AND             
BRQ059         CFONE                   NOT EQUAL '00000000000'          
               ADD 1                   TO  ACU-SEQUENCIA                
               MOVE CFONE              TO  AFN-CFONE-RENEG              
               MOVE ACU-SEQUENCIA      TO  AFN-CSEQ-FONE                
               PERFORM   5600-GRAVAR-ARQFONES                           
               INITIALIZE AFN-REG                                       
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       7800-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       8000-FINALIZAR                  SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           PERFORM 8500-EMITIR-DISPLAY.                                 
           PERFORM 8800-IMPRIMIR-REGTOTAL.                              
                                                                        
           CLOSE TABASUNI                                               
                 ARQPESJD                                               
                 ARQPESFS                                               
                 ARQCTCOR                                               
                 ARQLOGDR                                               
                 ARQFONES                                               
                 REGTOTAL.                                              
                                                                        
           MOVE WRK-FECHAMENTO         TO  WRK-OPERACAO.                
           PERFORM 1100-TESTAR-FILE-STATUS.                             
                                                                        
           GOBACK.                                                      
                                                                        
      *---------------------------------------------------------------* 
       8000-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       8500-EMITIR-DISPLAY             SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           DISPLAY '***************** RDAB0150 ******************'.     
           DISPLAY '*                                           *'.     
           MOVE ACU-LIDOS-TABASUNI     TO  WRK-EDITADO.                 
           DISPLAY '*  LIDOS    NO ARQ TABASUNI..: ' WRK-EDITADO '  *'. 
           MOVE ACU-GRAVADOS-ARQPESJD  TO  WRK-EDITADO.                 
           DISPLAY '*  GRAVADOS NO ARQ ARQPESJD..: ' WRK-EDITADO '  *'. 
           MOVE ACU-GRAVADOS-ARQPESFS  TO  WRK-EDITADO.                 
           DISPLAY '*  GRAVADOS NO ARQ ARQPESFS..: ' WRK-EDITADO '  *'. 
           MOVE ACU-GRAVADOS-ARQCTCOR  TO  WRK-EDITADO.                 
           DISPLAY '*  GRAVADOS NO ARQ ARQCTCOR..: ' WRK-EDITADO '  *'. 
           MOVE ACU-GRAVADOS-ARQLOGDR  TO  WRK-EDITADO.                 
           DISPLAY '*  GRAVADOS NO ARQ ARQLOGDR..: ' WRK-EDITADO '  *'. 
           MOVE ACU-GRAVADOS-ARQFONES  TO  WRK-EDITADO.                 
           DISPLAY '*  GRAVADOS NO ARQ ARQFONES..: ' WRK-EDITADO '  *'. 
           DISPLAY '*                                           *'.     
           DISPLAY '***************** RDAB0150 ******************'.     
                                                                        
      *---------------------------------------------------------------* 
       8500-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       8800-IMPRIMIR-REGTOTAL          SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
      *    CALL 'POOL7600'             USING WRK-DATA-HORA.
                                                                        
           MOVE WRK-DT-AAAAMMDD        TO  WRK-DATA.                    
           MOVE WRK-DIA                TO  CB1-DIA.                     
           MOVE WRK-MES                TO  CB1-MES.                     
           MOVE WRK-ANO                TO  CB1-ANO.                     
                                                                        
           MOVE ACU-LIDOS-TABASUNI     TO  LT1-TOTAL.                   
           MOVE ACU-GRAVADOS-ARQPESJD  TO  LT2-TOTAL.                   
           MOVE ACU-GRAVADOS-ARQPESFS  TO  LT3-TOTAL.                   
           MOVE ACU-GRAVADOS-ARQCTCOR  TO  LT4-TOTAL.                   
           MOVE ACU-GRAVADOS-ARQLOGDR  TO  LT5-TOTAL.                   
           MOVE ACU-GRAVADOS-ARQFONES  TO  LT6-TOTAL.                   
                                                                        
           MOVE WRK-GRAVACAO           TO  WRK-OPERACAO.                
                                                                        
           WRITE REG-REGTOTAL          FROM CABEC1.                     
           PERFORM 1170-TESTAR-FS-REGTOTAL.                             
                                                                        
           WRITE REG-REGTOTAL          FROM CABEC2.                     
           PERFORM 1170-TESTAR-FS-REGTOTAL.                             
                                                                        
           WRITE REG-REGTOTAL          FROM LINTOT1.                    
           PERFORM 1170-TESTAR-FS-REGTOTAL.                             
                                                                        
           WRITE REG-REGTOTAL          FROM LINTOT2.                    
           PERFORM 1170-TESTAR-FS-REGTOTAL.                             
                                                                        
           WRITE REG-REGTOTAL          FROM LINTOT3.                    
           PERFORM 1170-TESTAR-FS-REGTOTAL.                             
                                                                        
           WRITE REG-REGTOTAL          FROM LINTOT4.                    
           PERFORM 1170-TESTAR-FS-REGTOTAL.                             
                                                                        
           WRITE REG-REGTOTAL          FROM LINTOT5.                    
           PERFORM 1170-TESTAR-FS-REGTOTAL.                             
                                                                        
           WRITE REG-REGTOTAL          FROM LINTOT6.                    
           PERFORM 1170-TESTAR-FS-REGTOTAL.                             
                                                                        
      *---------------------------------------------------------------* 
       8800-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
RON   *----------------------------------------------------------------*
RON    8900-ACESSAR-ROTINA-CLIB2000   SECTION.                          
RON   *----------------------------------------------------------------*
RON                                                                     
RON        MOVE    2               TO  WRK-TRANSACA1.                   
RON        MOVE    'VRS000'        TO  WRK-VERSA1.                      
RON        MOVE    1               TO  WRK-PRODUT1.                     
RON                                                                     
RON        MOVE    WRK-CHV-BANCO-ANT TO  WRK-BANC1.                     
RON        MOVE    WRK-CHV-AGENCIA-ANT TO  WRK-AGENCI1.                 
RON        MOVE    WRK-CHV-CONTA-ANT   TO  WRK-CONT1.                   
RON   *                                                                 
RON        MOVE    'CLIB2000'      TO  WRK-MODULO.                      
RON                                                                     
RON        CALL WRK-MODULO             USING WRK-AREA-ENTRAD1           
RON                                          WRK-AREA-SAID1.            
RON   *                                                                 
RON        IF  WRK-CODIGO-RETORN1  EQUAL   ZEROS                        
RON            MOVE    WRK-MENSAGEM-SAID1  TO  OBJBB011                 
RON            IF  NOT (CTPO-DOCTO-ID   OF  OBJBB011    EQUAL   'CPF'   
RON                     OR  SPACES)                                     
RON                MOVE    CTPO-DOCTO-ID   OF  OBJBB011     TO          
RON                        APF-CTPO-DOCTO-ID   OF  ARQPESFS             
RON                MOVE    CDOCTO-ID   OF  OBJBB011     TO              
RON                        APF-CDOCTO-ID   OF  ARQPESFS                 
RON                MOVE    IORG-EMISR-ID   OF  OBJBB011     TO          
RON                        APF-IORG-EMISR-ID   OF  ARQPESFS             
RON                MOVE    DEMIS   OF  OBJBB011     TO                  
RON                        APF-DEMIS   OF  ARQPESFS                     
RON                MOVE    ' ' TO                                       
RON                    APF-CTPO-DOCTO-ID-NULL  OF  ARQPESFS             
RON                    APF-CDOCTO-ID-NULL  OF  ARQPESFS                 
RON                    APF-IORG-EMISR-ID-NULL  OF  ARQPESFS             
RON                    APF-DEMIS-NULL  OF  ARQPESFS                     
RON            ELSE                                                     
RON                MOVE    SPACES  TO                                   
RON                    APF-CTPO-DOCTO-ID   OF  ARQPESFS                 
RON                    APF-CDOCTO-ID   OF  ARQPESFS                     
RON                    APF-IORG-EMISR-ID   OF  ARQPESFS                 
RON                    APF-DEMIS   OF  ARQPESFS                         
RON                MOVE    '?' TO                                       
RON                    APF-CTPO-DOCTO-ID-NULL  OF  ARQPESFS             
RON                    APF-CDOCTO-ID-NULL  OF  ARQPESFS                 
RON                    APF-IORG-EMISR-ID-NULL  OF  ARQPESFS             
RON                    APF-DEMIS-NULL  OF  ARQPESFS                     
RON            END-IF                                                   
RON        ELSE                                                         
RON            MOVE    SPACES  TO                                       
RON                APF-CTPO-DOCTO-ID   OF  ARQPESFS                     
RON                APF-CDOCTO-ID   OF  ARQPESFS                         
RON                APF-IORG-EMISR-ID   OF  ARQPESFS                     
RON                APF-DEMIS   OF  ARQPESFS                             
RON            MOVE    '?' TO                                           
RON                APF-CTPO-DOCTO-ID-NULL  OF  ARQPESFS                 
RON                APF-CDOCTO-ID-NULL  OF  ARQPESFS                     
RON                APF-IORG-EMISR-ID-NULL  OF  ARQPESFS                 
RON                APF-DEMIS-NULL  OF  ARQPESFS                         
RON        END-IF.                                                      
RON   *                                                                 
           IF  APF-DEMIS   OF  ARQPESFS    EQUAL  '01.01.0001'          
               MOVE SPACES TO  APF-DEMIS      OF   ARQPESFS             
               MOVE '?'    TO  APF-DEMIS-NULL OF   ARQPESFS             
           END-IF.                                                      
RON                                                                     
RON   *----------------------------------------------------------------*
RON    8900-99-FIM.                   EXIT.                             
RON   *----------------------------------------------------------------*
                                                                        
      *---------------------------------------------------------------* 
       9999-ROTINA-ERRO                SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           MOVE 'RDAB0150'             TO  ERR-PGM.                     
                                                                        
           CALL 'POOL7100'             USING WRK-BATCH                  
                                             ERRO-AREA.                 
                                                                        
           GOBACK.                                                      
                                                                        
      *---------------------------------------------------------------* 
       9999-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
                                                                        

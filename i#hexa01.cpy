HEXA  *---------------------------------------------------------------* 
HEXA  * AREA DE DECLARAÇ O PARA SUPORTE A MULTIPLAS FAIXAS DE AGENCIA * 
HEXA  *---------------------------------------------------------------* 
HEXA  *                                                                 
HEXA   77  FILLER                      PIC  X(090)         VALUE        
HEXA       'VARIAVEIS AUXILIARES PARA ANALISE DAS MULTIPLAS FAIXAS DE   
HEXA  -    'AGENCIA'.                                                   
HEXA  *                                                                 
HEXA   77  WRK-PRIMEIRA-AG             PIC S9(005) COMP-3  VALUE 1.     
HEXA   77  WRK-PRIMEIRA-AG-U           REDEFINES WRK-PRIMEIRA-AG        
HEXA                                   PIC  9(005) COMP-3.              
HEXA  *                                                                 
HEXA   77  WRK-ULTIMA-AG               PIC S9(005) COMP-3  VALUE 9998.  
HEXA   77  WRK-ULTIMA-AG-U             REDEFINES WRK-ULTIMA-AG          
HEXA                                   PIC  9(005) COMP-3.              
HEXA  *                                                                 
HEXA   77  WRK-HX01-AUX                PIC S9(005) COMP-3  VALUE 4999.  
HEXA   77  WRK-ULTIMO-DEP              REDEFINES WRK-HX01-AUX           
HEXA                                   PIC S9(005) COMP-3.              
HEXA   77  WRK-ULTIMO-DEP-U            REDEFINES WRK-ULTIMO-DEP         
HEXA                                   PIC  9(005) COMP-3.              
HEXA  *                                                                 
HEXA   77  WRK-ULTIMA-EMP-LIG          PIC S9(005) COMP-3  VALUE 5999.  
HEXA   77  WRK-ULTIMA-EMP-LIG-U        REDEFINES WRK-ULTIMA-EMP-LIG     
HEXA                                   PIC  9(005) COMP-3.              
HEXA  *                                                                 
HEXA   01  WRK-HX01-X-5                PIC  X(005)         VALUE SPACES.
HEXA   01  FILLER                      REDEFINES WRK-HX01-X-5.          
HEXA       03  FILLER                  PIC  X(001).                     
HEXA       03  WRK-HX01-X-4            PIC  X(004).                     
HEXA       03  FILLER                  REDEFINES WRK-HX01-X-4.          
HEXA         05  WRK-HX01-U-4          PIC  9(004).                     
HEXA   01  FILLER                      REDEFINES WRK-HX01-X-5.          
HEXA       03  WRK-HX01-U-5            PIC  9(005).                     
HEXA  *                                                                 
HEXA   77  WRK-HX01-9                  PIC S9(005) COMP-3  VALUE ZEROS. 
HEXA   77  WRK-HX01-U                  REDEFINES WRK-HX01-9             
HEXA                                   PIC  9(005) COMP-3.              
HEXA  *                                                                 
HEXA   01  WRK-HX01-AG-18              PIC S9(018)         VALUE ZEROS. 
HEXA   01  FILLER                      REDEFINES WRK-HX01-AG-18.        
HEXA       03  FILLER                  PIC  9(013).                     
HEXA       03  WRK-HX01-AG-5           PIC S9(005).                     
HEXA  *                                                                 
HEXA   77  WRK-CHV-SELECAO-AG          PIC  9(001)         VALUE ZEROS. 
HEXA     88  WRK-EH-UNTIL-AG                               VALUE 1.     
HEXA     88  WRK-NAO-EH-UNTIL-AG                           VALUE ZEROS. 
HEXA  *                                                                 
HEXA   77  FILLER                      PIC  X(090)         VALUE        
HEXA       'VARIAVEIS UTILIZADAS PELA ROTINA MESU DE ANALISE DAS        
HEXA  -    'MULTIPLAS FAIXAS DE AGENCIA'.                               
HEXA  *                                                                 
HEXA   77  WRK-MODULO-MESUX0           PIC  X(008)         VALUE SPACES.
HEXA  *                                                                 
HEXA   77  FILLER                      PIC  X(050)         VALUE        
HEXA       'VARIAVEIS PARA CHAMADA DA ROTINA DE ABEND'.                 
HEXA  *                                                                 
HEXA   77  WRK-ABEND-MESUX0            PIC  X(008)         VALUE SPACES.
HEXA   77  WRK-BATCH-MESUX0            PIC  X(005)         VALUE        
HEXA       'BATCH'.                                                     
HEXA  *                                                                 
HEXA   77  FILLER                      PIC  X(050)         VALUE        
HEXA       'VARIAVEIS PARA COMUNICACAO COM A ROTINA'.                   
HEXA  *                                                                 
HEXA  ***************************************************************** 
HEXA  *  1) AREA DE ENTRADA:                                          * 
HEXA  *        BANCO: 2 POSICOES COMPACTADAS:                         * 
HEXA  *           CONTEUDO VALIDO: NUMERICO E 237                     * 
HEXA  *                                                               * 
HEXA  *        CODIGO DE JUNCAO: 3 POSICOES COMPACTADAS               * 
HEXA  *           CONTEUDO VALIDO: NUMERICO                           * 
HEXA  *                                                               * 
HEXA  *  2) AREA DE SAIDA:                                            * 
HEXA  *        CODIGO DE RETORNO: 2 POSICOES NUMERICAS, SENDO:        * 
HEXA  *           A PRIMEIRA POSICAO = 0 (AGENCIA)                    * 
HEXA  *                                1 (DEPARTAMENTO)               * 
HEXA  *                                2 (EMPRESA LIGADA)             * 
HEXA  *                                9 (JUNCAO NAO ENCONTRADA)      * 
HEXA  *                                                               * 
HEXA  *           E A SEGUNDA POSICAO COMO QUALIFICADOR, PORTANTO:    * 
HEXA  *  00 - AGENCIA NORMAL (A)                                      * 
HEXA  *  01 - AGENCIA DE TESTE (?)                                    * 
HEXA  *  02 - AGENCIA DE SERVICO (?)                                  * 
HEXA  *  10 - DEPARTAMENTO MATRIZ (D)                                 * 
HEXA  *  11 - DEPARTAMENTO DIRETORIA REGIONAL (R)                     * 
HEXA  *  12 - DEPARTAMENTO GERENCIA REGIONAL (G)                      * 
HEXA  *  13 - DEPARTAMENTO POLO DE SERVICO (P)                        * 
HEXA  *  20 - EMPRESAS LIGADAS (L)                                    * 
HEXA  *  90 - JUNCAO NAO ENCONTRADA, POIS NAO EXISTE NA MESU          * 
HEXA  *  98 - JUNCAO NAO ENCONTRADA,                                  * 
HEXA  *       POIS PARAMETRO DE ENTRADA É INV LIDO                    * 
HEXA  *  99 - JUNCAO NAO ENCONTRADA,                                  * 
HEXA  *       POIS OCORREU ERRO NO MODULO CHAMADO, NESTE CASO         * 
HEXA  *       O PROGRAMA CHAMADOR DEVE ABENDAR                        * 
HEXA  ***************************************************************** 
HEXA  *                                                                 
HEXA   01  WRK-AREA-MESUX0.                                             
HEXA       03  WRK-MESUX0-ENTRADA.                                      
HEXA         05  WRK-MESUX0-BANCO      PIC  9(003) COMP-3  VALUE 237.   
HEXA         05  WRK-MESUX0-AGENCIA    PIC S9(005) COMP-3  VALUE ZEROS. 
HEXA       03  WRK-MESUX0-SAIDA.                                        
HEXA         05  WRK-MESUX0-RETORNO    PIC  9(002)         VALUE ZEROS. 
HEXA  *                                                                 
HEXA  **** CODIGOS PARA AGENCIAS                                        
HEXA  *  01 - AGENCIA DE TESTES                                         
HEXA  *  01 - AGENCIAS  TEIS                                            
HEXA           88  WRK-EH-AG-TESTE                         VALUE 01.    
HEXA           88  WRK-EH-FX-AG-UTIL                       VALUE 00, 02.
HEXA  *                                                                 
HEXA  **** CODIGOS PARA JUNCAO NAO ENCONTRADA                           
HEXA  *  90 - JUNCAO NAO ENCONTRADA, POIS NAO EXISTE NA MESU            
HEXA  *  98 - JUNCAO NAO ENCONTRADA, CONTEUDO INVALIDO.                 
HEXA  *  99 - JUNCAO NAO ENCONTRADA, ERRO NO MODULO. ABENDAR!           
HEXA           88  WRK-JUNCAO-NAO-EXISTE-MESU              VALUE 90.    
HEXA           88  WRK-EH-ERRO-MESU-CT-INV                 VALUE 98.    
HEXA           88  WRK-EH-ERRO-MESU-ABEND                  VALUE 99.    
HEXA  *                                                                 
HEXA       03  FILLER                  REDEFINES  WRK-MESUX0-SAIDA.     
HEXA         05  WRK-RETORNO-POSICAO1  PIC  9(001).                     
HEXA  *                                                                 
HEXA  **** CODIGOS PARA:                                                
HEXA  * 0 - AGENCIAS                                                    
HEXA           88  WRK-EH-FX-AG                            VALUE 0.     
HEXA           88  WRK-NAO-EH-FX-AG                        VALUE 1, 2,  
HEXA                                                             9.     
HEXA  * 1 - DEPARTAMENTOS                                               
HEXA           88  WRK-EH-FX-DEP                           VALUE 1.     
HEXA           88  WRK-NAO-EH-FX-DEP                       VALUE 0, 2,  
HEXA                                                             9.     
HEXA  * 2 - EMPRESAS LIGADAS                                            
HEXA           88  WRK-EH-FX-EMPR-LIG                      VALUE 2.     
HEXA           88  WRK-NAO-EH-FX-EMPR-LIG                  VALUE 0, 1,  
HEXA                                                             9.     
HEXA  * 9 - JUNCAO NAO ENCONTRADA                                       
HEXA           88  WRK-JUNCAO-NAO-ENC                      VALUE 9.     
HEXA  *                                                                 
HEXA  **** CODIGOS PARA COMBINACOES DE UTILIZACAO                       
HEXA  **** AGENCIAS / DEPARTAMENTOS / EMPRESAS LIGADAS                  
HEXA           88  WRK-EH-FX-AG-DEP                        VALUE 0, 1.  
HEXA           88  WRK-EH-FX-AG-DEP-EL                     VALUE 0, 1,  
HEXA                                                             2.     
HEXA           88  WRK-NAO-EH-FX-AG-DEP                    VALUE 2, 9.  
HEXA  *                                                                 
HEXA         05  WRK-RETORNO-POSICAO2  PIC  9(001).                     
                                                                        

      *===============================================================* 
       IDENTIFICATION DIVISION.                                         
      *===============================================================* 
       PROGRAM-ID. CLLP2601.                                            
       AUTHOR.     GUILHERME ALCANTARA.                                 
      *===============================================================* 
      *                   I N F O S E R V E R                         * 
      *---------------------------------------------------------------* 
      *                                                               * 
      *      PROGRAMA     : CLLP2601                                  * 
      *      ANALISTA     : GUILHERME ALCANTARA - INFOSERVER          * 
      *      PROGRAMADOR  : GUILHERME ALCANTARA - INFOSERVER          * 
      *      DATA         : 03/10/2013                                * 
      *                                                               * 
      *      OBJETIVO     : VERIFICAR RESTRICAO POR AVISO NA TABELA   * 
      *                     LPCLB037.                                 * 
      *                                                               * 
      *      ARQUIVOS:                                                * 
      *         DDNAME                           INCLUDE/BOOK         * 
      *         ARQDB22S                           I#CLLPKA.          * 
      *         TABELA37                                              * 
      *         BLQREST                                               * 
      *                                                               * 
      *===============================================================* 
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
                                                                        
           SELECT TABELA37 ASSIGN TO UT-S-TABELA37                      
                      FILE STATUS IS WRK-FS-TABELA37.                   
                                                                        
           SELECT ARQDB22S ASSIGN TO UT-S-ARQDB22S                      
                      FILE STATUS IS WRK-FS-ARQDB22S.                   
                                                                        
           SELECT BLQREST  ASSIGN TO UT-S-BLQREST                       
                      FILE STATUS IS WRK-FS-BLQREST.                    
                                                                        
           SELECT RELATO1  ASSIGN TO UT-S-RELATO1                       
                      FILE STATUS IS WRK-FS-RELATO1.                    
                                                                        
           SELECT RELATO2  ASSIGN TO UT-S-RELATO2                       
                      FILE STATUS IS WRK-FS-RELATO2.                    
                                                                        
      *===============================================================* 
       DATA DIVISION.                                                   
      *===============================================================* 
                                                                        
      *---------------------------------------------------------------* 
       FILE SECTION.                                                    
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
      *    INPUT:     ORG. SEQUENCIAL   -   LRECL = 43                * 
      *---------------------------------------------------------------* 
                                                                        
       FD  TABELA37                                                     
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  FD-TABELAL.                                                  
           10 FD-B037-CCGC-CPF          PIC S9(9)V USAGE COMP-3.        
           10 FD-B037-CFLIAL-CGC        PIC S9(5)V USAGE COMP-3.        
           10 FD-B037-CCTRL-CPF-CGC     PIC S9(2)V USAGE COMP-3.        
           10 FD-B037-CREST-BLOQ-OPER   PIC S9(1)V USAGE COMP-3.        
           10 FD-B037-CJUNC-DEPDC-BDSCO PIC S9(5)V USAGE COMP-3.        
           10 FD-B037-CCTA-CORR         PIC S9(7)V USAGE COMP-3.        
           10 FD-B037-CTPO-NATUZ-OPER   PIC S9(3)V USAGE COMP-3.        
           10 FD-B037-CCART             PIC X(3).                       
           10 FD-B037-CSGL-UF           PIC X(2).                       
           10 FD-B037-CINDCD-AVISO-COBR PIC X(1).                       
           10 FD-B037-CINDCD-IMPED-REST PIC X(1).                       
           10 FD-B037-CINDCD-SERASA-SPC PIC X(1).                       
           10 FD-B037-DULT-ATULZ        PIC X(10).                      
           10 FD-B037-CFUNC-BDSCO       PIC S9(9)V USAGE COMP-3.        
                                                                        
      *---------------------------------------------------------------* 
      *    INPUT:     ORG. SEQUENCIAL   -   LRECL = 592               * 
      *---------------------------------------------------------------* 
                                                                        
       FD  ARQDB22S                                                     
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       COPY 'I#CLLPKA'.
            02   FD-DB2-UF           PIC X(02) VALUE SPACES.            
                                                                        
      *---------------------------------------------------------------* 
      *    OUTPUT:    ORG. SEQUENCIAL   -   LRECL = 030               * 
      *---------------------------------------------------------------* 
                                                                        
       FD  BLQREST                                                      
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  FD-BLQREST.                                                  
           05  FD-EMPRESA   PIC 9(05)    COMP-3.                        
           05  FD-AGENCIA   PIC 9(05)    COMP-3.                        
           05  FD-CONTA     PIC 9(07)    COMP-3.                        
           05  FD-CARTEIRA  PIC X(03).                                  
           05  FD-CONTRATO  PIC 9(07)    COMP-3.                        
           05  FD-IND-BLOQ  PIC 9(02)    COMP-3.                        
           05  FD-INDICADOR PIC X(01).                                  
           05  FD-CPF       PIC 9(09)    COMP-3.                        
           05  FD-FILIAL    PIC 9(05)    COMP-3.                        
           05  FD-CTRL      PIC 9(03)    COMP-3.                        
           05  FILLER       PIC X(10).                                  
                                                                        
      *---------------------------------------------------------------* 
      *    OUTPUT:    RELATORIO DE BLOQUEADOS - DEVEDOR               * 
      *---------------------------------------------------------------* 
                                                                        
       FD  RELATO1                                                      
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  FD-RELATO1       PIC X(132).                                 
                                                                        
      *---------------------------------------------------------------* 
      *    OUTPUT:    RELATORIO DE BLOQUEADOS - AVALISTA              * 
      *---------------------------------------------------------------* 
                                                                        
       FD  RELATO2                                                      
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  FD-RELATO2       PIC X(132).                                 
                                                                        
      *---------------------------------------------------------------* 
       WORKING-STORAGE SECTION.                                         
      *---------------------------------------------------------------* 
                                                                        
       77  FILLER                      PIC X(32)        VALUE           
           '* INICIO DA WORKING CLLP2601 *'.                            
                                                                        
       77  ACU-LIDOS-ARQDB22S          PIC 9(07) COMP-3 VALUE ZEROS.    
       77  ACU-GRAV-BLQREST            PIC 9(07) COMP-3 VALUE ZEROS.    
       77  ACU-LINHAS-DEV              PIC 9(05) COMP-3 VALUE 99.       
       77  ACU-LINHAS-AVA              PIC 9(05) COMP-3 VALUE 99.       
       77  ACU-PAGINAS-DEV             PIC 9(05) COMP-3 VALUE ZEROS.    
       77  ACU-PAGINAS-AVA             PIC 9(05) COMP-3 VALUE ZEROS.    
                                                                        
       77  WRK-FIM-ARQDB22S            PIC X(01)        VALUE  'N'.     
                                                                        
       77  WRK-FS-ARQDB22S             PIC X(02)        VALUE  SPACES.  
       77  WRK-FS-BLQREST              PIC X(02)        VALUE  SPACES.  
       77  WRK-FS-RELATO1              PIC X(02)        VALUE  SPACES.  
       77  WRK-FS-RELATO2              PIC X(02)        VALUE  SPACES.  
       77  WRK-FS-TABELA37             PIC X(02)        VALUE  SPACES.  
                                                                        
       77  WRK-FECHAMENTO              PIC X(13) VALUE  'NO FECHAMENTO'.
                                                                        
       77  WRK-BLOQ-OPER           PIC 9(01)    VALUE ZEROS.            
       77  WRK-UF-ALFA             PIC X(02)    VALUE SPACES.           
       77  WRK-UF-NUM              PIC 9(02)    VALUE ZEROS.            
       77  WRK-PESQ-DEV            PIC 9(03) COMP-3   VALUE ZEROS.      
       77  WRK-PESQ-AVA            PIC 9(03) COMP-3   VALUE ZEROS.      
       77  WRK-PESQ-DEVEDOR        PIC 9(03)    VALUE 063.              
       77  WRK-PESQ-AVALISTA       PIC 9(03)    VALUE 063.              
                                                                        
       01  WRK-DATA-AUX-X10.                                            
           02  WRK-DIA                 PIC 9(02) VALUE ZEROS.           
           02  FILLER                  PIC X(01) VALUE SPACES.          
           02  WRK-MES                 PIC 9(02) VALUE ZEROS.           
           02  FILLER                  PIC X(01) VALUE SPACES.          
           02  WRK-ANO                 PIC 9(04) VALUE ZEROS.           
                                                                        
       01  WRK-DATA-AUX                PIC 9(08) VALUE ZEROS.           
       01  FILLER REDEFINES WRK-DATA-AUX.                               
           02  WRK-DT-DD               PIC 9(02).                       
           02  WRK-DT-MM               PIC 9(02).                       
           02  WRK-DT-AAAA             PIC 9(04).                       
                                                                        
       01  WRK-FILIAL-AUX              PIC 9(05) VALUE ZEROS.           
       01  FILLER REDEFINES WRK-FILIAL-AUX.                             
           02  FILLER                  PIC 9(01).                       
           02  WRK-FILIAL-AUX-RR       PIC 9(04).                       
                                                                        
       01  WRK-AGENCIA-AUX             PIC 9(05) VALUE ZEROS.           
       01  FILLER REDEFINES WRK-AGENCIA-AUX.                            
           02  FILLER                  PIC 9(01).                       
           02  WRK-AGENCIA-AUX-RR      PIC 9(04).                       
                                                                        
       01  WRK-CART-AUX                PIC X(03) VALUE SPACES.          
      *01  FILLER REDEFINES WRK-CART-AUX.
      *    02  WRK-CART-AUX-RR         PIC 9(03).
                                                                        
       01  WRK-NATU-AUX                PIC X(03) VALUE SPACES.          
       01  FILLER REDEFINES WRK-NATU-AUX.                               
           02  WRK-NATU-AUX-RR         PIC 9(03).                       
                                                                        
       01  WRK-SINAL-9                 PIC +9(09) VALUE ZEROS.          
       01  FILLER REDEFINES WRK-SINAL-9.                                
           02  FILLER                  PIC X(01).                       
           02  WRK-SEM-SINAL-9         PIC 9(09).                       
                                                                        
       01  WRK-SINAL-3                 PIC +9(03) VALUE ZEROS.          
       01  FILLER REDEFINES WRK-SINAL-3.                                
           02  FILLER                  PIC X(01).                       
           02  WRK-SEM-SINAL-3         PIC 9(03).                       
                                                                        
       01  WRK-SINAL-5                 PIC +9(05) VALUE ZEROS.          
       01  FILLER REDEFINES WRK-SINAL-5.                                
           02  FILLER                  PIC X(01).                       
           02  WRK-SEM-SINAL-5         PIC 9(05).                       
                                                                        
       01  WRK-SINAL-2                 PIC +9(02) VALUE ZEROS.          
       01  FILLER REDEFINES WRK-SINAL-2.                                
           02  FILLER                  PIC X(01).                       
           02  WRK-SEM-SINAL-2         PIC 9(02).                       
                                                                        
       01  WRK-SINAL-1                 PIC +9(01) VALUE ZEROS.          
       01  FILLER REDEFINES WRK-SINAL-1.                                
           02  FILLER                  PIC X(01).                       
           02  WRK-SEM-SINAL-1         PIC 9(01).                       
                                                                        
       01  WRK-SINAL-7                 PIC +9(07) VALUE ZEROS.          
       01  FILLER REDEFINES WRK-SINAL-7.                                
           02  FILLER                  PIC X(01).                       
           02  WRK-SEM-SINAL-7         PIC 9(07).                       
                                                                        
       01  WRK-TAB-MENSAGEM.                                            
           05  FILLER      PIC X(26) VALUE '001-UF'.                    
           05  FILLER      PIC X(26) VALUE '002-BLOQ-OPER'.             
           05  FILLER      PIC X(26) VALUE '003-UF/BLOQ-OPER'.          
           05  FILLER      PIC X(26) VALUE '004-CARTEIRA'.              
           05  FILLER      PIC X(26) VALUE '005-UF/CARTEIRA'.           
           05  FILLER      PIC X(26) VALUE '006-BLOQ/CARTEIRA'.         
           05  FILLER      PIC X(26) VALUE '007-UF/BLOQ/CARTEIRA'.      
           05  FILLER      PIC X(26) VALUE '008-NATUREZA'.              
           05  FILLER      PIC X(26) VALUE '009-UF/NATUREZA'.           
           05  FILLER      PIC X(26) VALUE '010-BLOQ/NATUREZA'.         
           05  FILLER      PIC X(26) VALUE '011-UF/BLOQ/NATUREZA'.      
           05  FILLER      PIC X(26) VALUE '012-CART/NATUREZA'.         
           05  FILLER      PIC X(26) VALUE '013-UF/CART/NATUREZA'.      
           05  FILLER      PIC X(26) VALUE '014-BLOQ/CART/NATUREZA'.    
           05  FILLER      PIC X(26) VALUE '015-UF/BLOQ/CART/NATUREZA'. 
           05  FILLER      PIC X(26) VALUE '016-AGENCIA/CONTA'.         
           05  FILLER      PIC X(26) VALUE '017-UF/AGENCIA/CONTA'.      
           05  FILLER      PIC X(26) VALUE '018-BLOQ/AGENCIA/CONTA'.    
           05  FILLER      PIC X(26) VALUE '019-UF/BLOQ/AGENCIA/CONTA'. 
           05  FILLER      PIC X(26) VALUE '020-AGENCIA/CONTA/CART'.    
           05  FILLER      PIC X(26) VALUE '021-AGENCIA/CONTA/CART/UF'. 
           05  FILLER      PIC X(26) VALUE '022-AGENC/CONTA/CART/BLOQ'. 
           05  FILLER      PIC X(26) VALUE '023-AGENC/C.C/CART/BLOQ/UF'.
           05  FILLER      PIC X(26) VALUE '024-AGENCIA/CONTA/NATUREZA'.
           05  FILLER      PIC X(26) VALUE '025-AGENC/C.C/NATUREZA/UF'. 
           05  FILLER      PIC X(26) VALUE '026-AGENC/C.C/NAT/BLOQ'.    
           05  FILLER      PIC X(26) VALUE '027-AGENC/C.C/NAT/BLOQ/UF'. 
           05  FILLER      PIC X(26) VALUE '028-AGENC/C.C/NAT/CART'.    
           05  FILLER      PIC X(26) VALUE '029-AGENC/C.C/NAT/CART/UF'. 
           05  FILLER      PIC X(26) VALUE '030-AGENC/C.C/NAT/CAR/BLOQ'.
           05  FILLER      PIC X(26) VALUE '031-AG/C.C/NAT/CAR/BLOQ/UF'.
           05  FILLER      PIC X(26) VALUE '032-CPF'.                   
           05  FILLER      PIC X(26) VALUE '033-CPF/UF'.                
           05  FILLER      PIC X(26) VALUE '034-CPF/BLOQ'.              
           05  FILLER      PIC X(26) VALUE '035-CPF/UF/BLOQ'.           
           05  FILLER      PIC X(26) VALUE '036-CPF/CARTEIRA'.          
           05  FILLER      PIC X(26) VALUE '037-CPF/UF/CARTEIRA'.       
           05  FILLER      PIC X(26) VALUE '038-CPF/CARTEIRA/BLOQ'.     
           05  FILLER      PIC X(26) VALUE '039-CPF/CARTEIRA/BLOQ/UF'.  
           05  FILLER      PIC X(26) VALUE '040-CPF/NATUREZA'.          
           05  FILLER      PIC X(26) VALUE '041-CPF/NATUREZA/UF'.       
           05  FILLER      PIC X(26) VALUE '042-CPF/NATUREZA/BLOQ'.     
           05  FILLER      PIC X(26) VALUE '043-CPF/NAT/BLOQ/UF'.       
           05  FILLER      PIC X(26) VALUE '044-CPF/NAT/CART'.          
           05  FILLER      PIC X(26) VALUE '045-CPF/NAT/CART/UF'.       
           05  FILLER      PIC X(26) VALUE '046-CPF/NAT/CART/BLOQ'.     
           05  FILLER      PIC X(26) VALUE '047-CPF/NAT/CART/BLOQ/UF'.  
           05  FILLER      PIC X(26) VALUE '048-CPF/AGENCIA/CONTA'.     
           05  FILLER      PIC X(26) VALUE '049-CPF/AGENCIA/CONTA/UF'.  
           05  FILLER      PIC X(26) VALUE '050-CPF/AGENCIA/CONTA/BLOQ'.
           05  FILLER      PIC X(26) VALUE '051-CPF/AGEN/C.C/BLOQ/UF'.  
           05  FILLER      PIC X(26) VALUE '052-CPF/AGEN/C.C/CART'.     
           05  FILLER      PIC X(26) VALUE '053-CPF/AGEN/C.C/CART/UF'.  
           05  FILLER      PIC X(26) VALUE '054-CPF/AG/C.C/CART/BLOQ'.  
           05  FILLER      PIC X(26) VALUE '055-CPF/AG/C.C/CART/BL/UF'. 
           05  FILLER      PIC X(26) VALUE '056-CPF/AG/C.C/NAT'.        
           05  FILLER      PIC X(26) VALUE '057-CPF/AG/C.C/NAT/UF'.     
           05  FILLER      PIC X(26) VALUE '058-CPF/AG/C.C/NAT/BLOQ'.   
           05  FILLER      PIC X(26) VALUE '059-CPF/AG/C.C/NAT/BLOQ/UF'.
           05  FILLER      PIC X(26) VALUE '060-CPF/AG/C.C/NAT/CART'.   
           05  FILLER      PIC X(26) VALUE '061-CPF/AG/C.C/NAT/CART/UF'.
           05  FILLER      PIC X(26) VALUE '062-CPF/AG/C.C/NAT/CART/BL'.
           05  FILLER      PIC X(26) VALUE '063-CPF/AG/C.C/NT/CA/BL/UF'.
                                                                        
       01  WRK-TAB-MENSG1        REDEFINES WRK-TAB-MENSAGEM.            
           05  WRK-MENSG1        OCCURS 63 TIMES.                       
               07 WRK-COD        PIC 9(03).                             
               07 FILLER         PIC X(01).                             
               07 WRK-MENSAG     PIC X(22).                             
                                                                        
      *---------------------------------------------------------------* 
      *                 CAMPOS UTILIZADOS NA POOL7600                 * 
      *---------------------------------------------------------------* 
                                                                        
       77  WRK-ABERTURA                PIC  X(13) VALUE 'NA ABERTURA'.  
       77  WRK-LEITURA                 PIC  X(13) VALUE 'NA LEITURA'.   
       77  WRK-GRAVACAO                PIC  X(13) VALUE 'NA GRAVACAO'.  
       77  WRK-BATCH                   PIC  X(08) VALUE 'BATCH'.        
                                                                        
       01  WRK-MSG-FS-ERRO.                                             
           03  FILLER                  PIC  X(10) VALUE SPACES.         
           03  FILLER                  PIC  X(05) VALUE 'ERRO '.        
           03  WRK-OPERACAO            PIC  X(13) VALUE SPACES.         
                                                                        
       COPY POL7100C.                                                   
                                                                        
       01  WRK-AREA-POOL7600.                                           
           02  WRK-DT-JULIANA          PIC 9(05) COMP-3 VALUE ZEROS.    
           02  WRK-DT-AAMMDD           PIC 9(07) COMP-3 VALUE ZEROS.    
           02  WRK-DT-AAAAMMDD         PIC 9(09) COMP-3 VALUE ZEROS.    
           02  WRK-TI-HHMMSS           PIC 9(07) COMP-3 VALUE ZEROS.    
           02  WRK-TI-HHMMSSMMMMMM     PIC 9(13) COMP-3 VALUE ZEROS.    
           02  WRK-TIMESTAMP           PIC X(20)        VALUE SPACES.   
                                                                        
       01  WRK-DT-MOV-AAAAMMDD             PIC 9(09) VALUE ZEROS.       
       01  FILLER REDEFINES WRK-DT-MOV-AAAAMMDD.                        
           03 FILLER                   PIC 9(01).                       
           03 WRK-DT-MOV-AAAA              PIC 9(04).                   
           03 WRK-DT-MOV-MM                PIC 9(02).                   
           03 WRK-DT-MOV-DD                PIC 9(02).                   
                                                                        
       01  WRK-DT-MOV-HHMMSS               PIC 9(07) VALUE ZEROS.       
       01  FILLER REDEFINES WRK-DT-MOV-HHMMSS.                          
           03 FILLER                   PIC 9(01).                       
           03 WRK-DT-MOV-HH                PIC 9(02).                   
           03 WRK-DT-MOV-MM1               PIC 9(02).                   
           03 WRK-DT-MOV-SS                PIC 9(02).                   
                                                                        
      *---------------------------------------------------------------* 
      * INC P/ COVERTER UF ALFA P/ NUMERICA P/ ENVIO A POOL0480       * 
      *---------------------------------------------------------------* 
                                                                        
       COPY 'I#LPCLUF'.
                                                                        
      *---------------------------------------------------------------* 
      *    AREA PARA POOL0480                                         * 
      *---------------------------------------------------------------* 
                                                                        
       77  WRK-480-CODIGO1             PIC 9(01) VALUE 1.               
       77  WRK-480-CODIGO2             PIC 9(01) VALUE 2.               
       77  WRK-480-CODIGO3             PIC 9(01) VALUE 3.               
       77  WRK-480-TAM-CHAVE           PIC 9(02) VALUE 37.              
       77  WRK-480-NOME-TABELA         PIC X(08) VALUE 'TABCONTR'.      
       77  WRK-480-TAM-OCORRENC        PIC 9(03) VALUE 37.              
                                                                        
       77  WRK-MODULO                  PIC X(08) VALUE 'POOL0482'.      
                                                                        
       01  WRK-480-REGISTRO.                                            
           03  WRK-480-CHAVE.                                           
               05 WRK-480-CCGC-CPF         PIC 9(09) VALUE ZEROS.       
               05 WRK-480-CFLIAL-CGC       PIC 9(05) VALUE ZEROS.       
               05 WRK-480-CCTRL-CGC        PIC 9(02) VALUE ZEROS.       
               05 WRK-480-AGENCIA          PIC 9(05) VALUE ZEROS.       
               05 WRK-480-CONTA            PIC 9(07) VALUE ZEROS.       
               05 WRK-480-NATUREZA         PIC 9(03) VALUE ZEROS.       
               05 WRK-480-CCART            PIC X(03) VALUE ZEROS.
               05 WRK-480-BLOQ-OPER        PIC 9(01) VALUE ZEROS.       
               05 WRK-480-UF               PIC 9(02) VALUE ZEROS.       
                                                                        
       01  WRK-480-RETORNO.                                             
           03  WRK-480-RETORNO-CHAVE.                                   
               05 WRK-480-RET-CCGC-CPF     PIC 9(09) VALUE ZEROS.       
               05 WRK-480-RET-CFLIAL-CGC   PIC 9(05) VALUE ZEROS.       
               05 WRK-480-RET-CCTRL-CGC    PIC 9(02) VALUE ZEROS.       
               05 WRK-480-RET-AGENCIA      PIC 9(05) VALUE ZEROS.       
               05 WRK-480-RET-CONTA        PIC 9(07) VALUE ZEROS.       
               05 WRK-480-RET-NATUREZA     PIC 9(03) VALUE ZEROS.       
               05 WRK-480-RET-CCART        PIC X(03) VALUE ZEROS.
               05 WRK-480-RET-BLOQ-OPER    PIC 9(01) VALUE ZEROS.       
               05 WRK-480-RET-UF           PIC 9(02) VALUE ZEROS.       
                                                                        
      *---------------------------------------------------------------* 
      *                            CABECALHOS                         * 
      *---------------------------------------------------------------* 
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER   PIC X(32) VALUE '*      AREA DE CABECALHOS      *'. 
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
       01  CABEC1A.                                                     
           03  CB1A-CARRO              PIC X(01) VALUE '1'.             
           03  FILLER                  PIC X(08) VALUE 'CLLP2601'.      
           03  FILLER                  PIC X(39) VALUE  SPACES.         
           03  CB1A-NOME-BCO           PIC X(40) VALUE  SPACES.         
           03  FILLER                  PIC X(32) VALUE  SPACES.         
           03  FILLER                  PIC X(06) VALUE 'PAG.: '.        
           03  CB1A-PAGINAS            PIC ZZ.ZZ9 VALUE ZEROS.          
                                                                        
       01  CABEC2A.                                                     
           03  CB2A-CARRO               PIC X(01) VALUE ' '.            
           03  CB2A-DATA-DIA            PIC 9(02)/ VALUE ZEROS.         
           03  CB2A-DATA-MES            PIC 9(02)/ VALUE ZEROS.         
           03  CB2A-DATA-ANO            PIC 9(04) VALUE ZEROS.          
           03  FILLER                   PIC X(29) VALUE SPACES.         
           03  CB2A-DESCRICAO           PIC X(52) VALUE SPACES.         
           03  FILLER                   PIC X(32) VALUE SPACES.         
           03  CB2A-HORA-HOR            PIC 9(02) VALUE ZEROS.          
           03  FILLER                   PIC X(01) VALUE ':'.            
           03  CB2A-HORA-MIN            PIC 9(02) VALUE ZEROS.          
           03  FILLER                   PIC X(01) VALUE ':'.            
           03  CB2A-HORA-SEG             PIC 9(02) VALUE ZEROS.         
                                                                        
       01  CABEC3A.                                                     
           03  CB3A-CARRO              PIC X(01) VALUE '0'.             
           03  FILLER                  PIC X(17) VALUE                  
                                       'CNPJ/CPF DEVEDOR'.              
           03  FILLER                  PIC X(01) VALUE  SPACES.         
           03  FILLER                  PIC X(04) VALUE 'AGEN'.          
           03  FILLER                  PIC X(01) VALUE  SPACES.         
           03  FILLER                  PIC X(07) VALUE 'NUM C/C'.       
           03  FILLER                  PIC X(01) VALUE  SPACES.         
           03  FILLER                  PIC X(12) VALUE ' CONTR  NAT'.   
           03  FILLER                  PIC X(05) VALUE 'CART'.          
           03  FILLER                  PIC X(07) VALUE 'DT.VCTO'.       
           03  FILLER                  PIC X(01) VALUE  SPACES.         
           03  FILLER                  PIC X(40) VALUE                  
                                       'NOME DO DEVEDOR'.               
           03  FILLER                  PIC X(01) VALUE  SPACES.         
           03  FILLER                  PIC X(04) VALUE 'BLOQ'.          
           03  FILLER                  PIC X(01) VALUE  SPACES.         
           03  FILLER                  PIC X(02) VALUE 'UF'.            
           03  FILLER                  PIC X(01) VALUE  SPACES.         
           03  FILLER                  PIC X(13) VALUE 'TIPO DE ERRO'.  
                                                                        
       01  CABEC3A1.                                                    
           03  CB3A1-CARRO             PIC X(01) VALUE '0'.             
           03  FILLER                  PIC X(17) VALUE                  
                                       'CNPJ/CPF AVALISTA'.             
           03  FILLER                  PIC X(01) VALUE  SPACES.         
           03  FILLER                  PIC X(04) VALUE 'AGEN'.          
           03  FILLER                  PIC X(01) VALUE  SPACES.         
           03  FILLER                  PIC X(07) VALUE 'NUM C/C'.       
           03  FILLER                  PIC X(01) VALUE  SPACES.         
           03  FILLER                  PIC X(12) VALUE ' CONTR  NAT'.   
           03  FILLER                  PIC X(05) VALUE 'CART'.          
           03  FILLER                  PIC X(07) VALUE 'DT.VCTO'.       
           03  FILLER                  PIC X(01) VALUE  SPACES.         
           03  FILLER                  PIC X(40) VALUE                  
                                       'NOME DO AVALISTA'.              
           03  FILLER                  PIC X(01) VALUE  SPACES.         
           03  FILLER                  PIC X(04) VALUE 'BLOQ'.          
           03  FILLER                  PIC X(01) VALUE  SPACES.         
           03  FILLER                  PIC X(02) VALUE 'UF'.            
           03  FILLER                  PIC X(01) VALUE  SPACES.         
           03  FILLER                  PIC X(13) VALUE 'TIPO DE ERRO'.  
                                                                        
       01  CABEC4A.                                                     
           03  CB4A-CARRO              PIC X(01) VALUE ' '.             
           03  FILLER                  PIC X(22) VALUE                  
                                       '================= ===='.        
           03  FILLER                  PIC X(01) VALUE  SPACES.         
           03  FILLER                  PIC X(07) VALUE '======='.       
           03  FILLER                  PIC X(01) VALUE  SPACES.         
           03  FILLER                  PIC X(11) VALUE '======= ==='.   
           03  FILLER                  PIC X(05) VALUE ' ==='.          
           03  FILLER                  PIC X(50) VALUE                  
               '======== ======================================== '.    
           03  FILLER                  PIC X(08) VALUE                  
               '==== =='.                                               
           03  FILLER                  PIC X(01) VALUE  SPACES.         
           03  FILLER                  PIC X(26) VALUE                  
                                       '=========================='.    
       01  CABEC5A.                                                     
           03  CB5A-CARRO              PIC X(01) VALUE ' '.             
           03  FILLER                  PIC X(131) VALUE SPACES.         
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER   PIC X(32) VALUE '*      AREA DE DETALHES        *'. 
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
       01  LINDET1A.                                                    
           03  LD1A-CARRO              PIC X(01)    VALUE ' '.          
           03  LD1A-CGC                PIC 999999999 VALUE ZEROS.       
           03  FILLER                  PIC X(01)    VALUE '/'.          
           03  LD1A-FIL                PIC 9999     VALUE ZEROS.        
           03  FILLER                  PIC X(01)    VALUE '-'.          
           03  LD1A-CTR                PIC 99       VALUE ZEROS.        
           03  FILLER                  PIC X(01)    VALUE SPACES.       
           03  LD1A-AGENCIA            PIC ZZZ9     VALUE ZEROS.        
           03  FILLER                  PIC X(01)    VALUE SPACES.       
           03  LD1A-CONTA              PIC ZZZZZZ9  VALUE ZEROS.        
           03  FILLER                  PIC X(01)    VALUE SPACES.       
           03  LD1A-CONTRATO           PIC ZZZZZZ9  VALUE ZEROS.        
           03  FILLER                  PIC X(01)    VALUE SPACES.       
           03  LD1A-NATUREZA           PIC X(03)    VALUE SPACES.       
           03  FILLER                  PIC X(01)    VALUE SPACES.       
           03  LD1A-CARTEIRA           PIC X(03)    VALUE ZEROS.        
           03  FILLER                  PIC X(01)    VALUE SPACES.       
           03  LD1A-DATA-VCTO          PIC 9(08)    VALUE ZEROS.        
           03  FILLER                  PIC X(01)    VALUE SPACES.       
           03  LD1A-NOME-DEVEDOR       PIC X(40)    VALUE SPACES.       
           03  FILLER                  PIC X(02)    VALUE SPACES.       
           03  LD1A-BLOQ               PIC X(02)    VALUE ZEROS.        
           03  FILLER                  PIC X(02)    VALUE SPACES.       
           03  LD1A-UF                 PIC X(02)    VALUE ZEROS.        
           03  FILLER                  PIC X(01)    VALUE SPACES.       
           03  LD1A-TIPO-ERRO          PIC X(22)    VALUE SPACES.       
                                                                        
       01  FILLER                      PIC X(32)        VALUE           
           '*  FIM DA WORKING CLLP2601 *'.                              
           EJECT                                                        
                                                                        
      *===============================================================* 
       PROCEDURE      DIVISION.                                         
      *===============================================================* 
                                                                        
      *---------------------------------------------------------------* 
       0900-INICIAR SECTION.                                            
      *---------------------------------------------------------------* 
                                                                        
           OPEN INPUT   TABELA37                                        
                        ARQDB22S                                        
                OUTPUT  BLQREST                                         
                        RELATO1                                         
                        RELATO2.                                        
                                                                        
           MOVE    WRK-ABERTURA TO WRK-OPERACAO.                        
                                                                        
           PERFORM 1000-TESTAR-FILE-STATUS.                             
                                                                        
           PERFORM 1700-LER-TABELA37.                                   
                                                                        
           IF  WRK-FS-TABELA37   EQUAL   '10'                           
               DISPLAY '************** CLLP2601 *************'          
               DISPLAY '*   TABELA DE RESTRICOES VAZI0          *'      
                                                  '         *'          
               DISPLAY '************** CLLP2601 *************'          
               PERFORM 9999-ROTINA-ERRO                                 
           END-IF.                                                      
                                                                        
           PERFORM 2000-CARREGA-POOL0480.                               
                                                                        
           CALL    'POOL7600'        USING  WRK-AREA-POOL7600           
                                                                        
           MOVE WRK-DT-AAAAMMDD        TO WRK-DT-MOV-AAAAMMDD.          
                                                                        
           MOVE WRK-DT-MOV-DD              TO CB2A-DATA-DIA.            
           MOVE WRK-DT-MOV-MM              TO CB2A-DATA-MES.            
           MOVE WRK-DT-MOV-AAAA            TO CB2A-DATA-ANO.            
                                                                        
           MOVE WRK-TI-HHMMSS          TO WRK-DT-MOV-HHMMSS.            
                                                                        
           MOVE WRK-DT-MOV-HH              TO CB2A-HORA-HOR.            
           MOVE WRK-DT-MOV-MM1             TO CB2A-HORA-MIN.            
           MOVE WRK-DT-MOV-SS              TO CB2A-HORA-SEG.            
                                                                        
           PERFORM 3100-LER-ARQDB22S UNTIL WRK-FIM-ARQDB22S EQUAL 'S'.  
                                                                        
           PERFORM 9000-FINALIZAR.                                      
                                                                        
      *---------------------------------------------------------------* 
       0900-99-FIM. EXIT.                                               
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1000-TESTAR-FILE-STATUS SECTION.                                 
      *---------------------------------------------------------------* 
                                                                        
           PERFORM 1100-TESTAR-FS-ARQDB22S.                             
                                                                        
           PERFORM 1001-TESTAR-FS-BLQREST.                              
                                                                        
           PERFORM 1800-TESTAR-FS-TABELA37.                             
                                                                        
      *---------------------------------------------------------------* 
       1000-99-FIM. EXIT.                                               
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1001-TESTAR-FS-BLQREST  SECTION.                                 
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-FS-BLQREST  NOT EQUAL '00'                            
              DISPLAY '************** CLLP2601 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              BLQREST              *'           
              DISPLAY '*         FILE STATUS =  ' WRK-FS-BLQREST        
                                                 '         *'           
              DISPLAY '************** CLLP2601 *************'           
              PERFORM 9999-ROTINA-ERRO                                  
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       1001-99-FIM. EXIT.                                               
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1002-GRAVA-BLQREST-DEV SECTION.                                  
      *---------------------------------------------------------------* 
                                                                        
           MOVE DB2-EMPRESA     TO FD-EMPRESA                           
           MOVE DB2-AGENCIA     TO FD-AGENCIA                           
           MOVE DB2-CONTA       TO FD-CONTA                             
           MOVE DB2-CARTEIRA    TO FD-CARTEIRA                          
           MOVE DB2-CONTRATO    TO FD-CONTRATO                          
           MOVE 02              TO FD-IND-BLOQ                          
           MOVE 'D'             TO FD-INDICADOR                         
           MOVE DB2-NUMERO-DEV  TO FD-CPF                               
           MOVE DB2-FILIAL      TO FD-FILIAL                            
           MOVE DB2-CTR-DEV     TO FD-CTRL                              
           WRITE FD-BLQREST                                             
           MOVE  WRK-GRAVACAO   TO  WRK-OPERACAO                        
           PERFORM 1001-TESTAR-FS-BLQREST                               
           ADD      1           TO  ACU-GRAV-BLQREST.                   
                                                                        
      *---------------------------------------------------------------* 
       1002-99-FIM. EXIT.                                               
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1003-GRAVA-BLQREST-AVA  SECTION.                                 
      *---------------------------------------------------------------* 
                                                                        
           MOVE DB2-EMPRESA      TO FD-EMPRESA                          
           MOVE DB2-AGENCIA      TO FD-AGENCIA                          
           MOVE DB2-CONTA        TO FD-CONTA                            
           MOVE DB2-CARTEIRA     TO FD-CARTEIRA                         
           MOVE DB2-CONTRATO     TO FD-CONTRATO                         
           MOVE 02               TO FD-IND-BLOQ                         
           MOVE 'A'              TO FD-INDICADOR                        
           MOVE DB2-NUMERO-AVAL  TO FD-CPF                              
           MOVE DB2-FILIAL-AVAL  TO FD-FILIAL                           
           MOVE DB2-CTR-AVAL     TO FD-CTRL                             
           WRITE FD-BLQREST                                             
           MOVE  WRK-GRAVACAO   TO  WRK-OPERACAO                        
           PERFORM 1001-TESTAR-FS-BLQREST                               
           ADD      1           TO  ACU-GRAV-BLQREST.                   
                                                                        
      *---------------------------------------------------------------* 
       1003-99-FIM. EXIT.                                               
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1100-TESTAR-FS-ARQDB22S SECTION.                                 
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-FS-ARQDB22S NOT EQUAL '00'                            
              DISPLAY '************** CLLP2601 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              ARQDB22S               *'         
              DISPLAY '*         FILE STATUS =  ' WRK-FS-ARQDB22S       
                                                 '         *'           
              DISPLAY '************** CLLP2601 *************'           
              PERFORM 9999-ROTINA-ERRO                                  
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       1100-99-FIM. EXIT.                                               
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1500-TESTAR-FS-RELATO1 SECTION.                                  
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-FS-RELATO1 NOT EQUAL '00'                             
              DISPLAY '************** CLLP2601 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              RELATO1              *'           
              DISPLAY '*         FILE STATUS =  ' WRK-FS-RELATO1        
                                                 '         *'           
              DISPLAY '************** CLLP2601 *************'           
              PERFORM 9999-ROTINA-ERRO                                  
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       1500-99-FIM. EXIT.                                               
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1501-TESTAR-FS-RELATO2 SECTION.                                  
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-FS-RELATO2 NOT EQUAL '00'                             
              DISPLAY '************** CLLP2601 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              RELATO2              *'           
              DISPLAY '*         FILE STATUS =  ' WRK-FS-RELATO2        
                                                 '         *'           
              DISPLAY '************** CLLP2601 *************'           
              PERFORM 9999-ROTINA-ERRO                                  
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       1501-99-FIM. EXIT.                                               
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1700-LER-TABELA37             SECTION.                           
      *---------------------------------------------------------------* 
                                                                        
           READ   TABELA37.                                             
                                                                        
           IF  WRK-FS-TABELA37   EQUAL   '10'                           
               NEXT              SENTENCE                               
           ELSE                                                         
               PERFORM 1800-TESTAR-FS-TABELA37                          
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       1700-99-FIM. EXIT.                                               
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1800-TESTAR-FS-TABELA37       SECTION.                           
      *---------------------------------------------------------------* 
                                                                        
           IF  WRK-FS-TABELA37   NOT EQUAL   '00'                       
               DISPLAY '************** CLLP2601 *************'          
               DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'      
               DISPLAY '*              TABELA37             *'          
               DISPLAY '*         FILE STATUS =  ' WRK-FS-TABELA37      
                                                  '         *'          
               DISPLAY '************** CLLP2601 *************'          
               PERFORM 9999-ROTINA-ERRO                                 
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       1800-99-FIM. EXIT.                                               
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       2000-CARREGA-POOL0480  SECTION.                                  
      *---------------------------------------------------------------* 
                                                                        
                                                                        
           PERFORM  2100-MONTAR-TABELA                                  
                    UNTIL              WRK-FS-TABELA37 = '10'.          
                                                                        
           CALL  WRK-MODULO         USING        WRK-480-CODIGO2        
                                                 WRK-480-NOME-TABELA.   
                                                                        
                                                                        
      *---------------------------------------------------------------* 
       2000-FIM-CARREGA-POOL0480. EXIT.                                 
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       2100-MONTAR-TABELA     SECTION.                                  
      *---------------------------------------------------------------* 
                                                                        
           MOVE   FD-B037-CSGL-UF           TO WRK-UF-ALFA.             
           PERFORM  2200-CONVERTE-UF.                                   
                                                                        
           MOVE   FD-B037-CCGC-CPF          TO WRK-SINAL-9.             
           MOVE   WRK-SEM-SINAL-9           TO WRK-480-CCGC-CPF.        
           MOVE   FD-B037-CFLIAL-CGC        TO WRK-SINAL-5.             
           MOVE   WRK-SEM-SINAL-5           TO WRK-480-CFLIAL-CGC.      
           MOVE   FD-B037-CCTRL-CPF-CGC     TO WRK-SINAL-2.             
           MOVE   WRK-SEM-SINAL-2           TO WRK-480-CCTRL-CGC        
           MOVE   FD-B037-CJUNC-DEPDC-BDSCO TO WRK-SINAL-5.             
           MOVE   WRK-SEM-SINAL-5           TO WRK-480-AGENCIA.         
           MOVE   FD-B037-CCTA-CORR         TO WRK-SINAL-7.             
           MOVE   WRK-SEM-SINAL-7           TO WRK-480-CONTA.           
           MOVE   FD-B037-CTPO-NATUZ-OPER   TO WRK-SINAL-3.             
           MOVE   WRK-SEM-SINAL-3           TO WRK-480-NATUREZA.        
           MOVE   FD-B037-CCART             TO WRK-480-CCART.
           MOVE   FD-B037-CREST-BLOQ-OPER   TO WRK-SINAL-1.             
           MOVE   WRK-SEM-SINAL-1           TO WRK-480-BLOQ-OPER.       
           MOVE   WRK-UF-NUM                TO WRK-480-UF.              
                                                                        
           CALL  WRK-MODULO         USING        WRK-480-CODIGO1        
                                                 WRK-480-NOME-TABELA    
                                                 WRK-480-TAM-CHAVE      
                                                 WRK-480-TAM-OCORRENC   
                                                 WRK-480-REGISTRO.      
                                                                        
           IF     RETURN-CODE       EQUAL    4                          
                  DISPLAY '**************   CLLP2601   *************'   
                  DISPLAY '*      ESTOUROU  A  REGION  POOL0482    *'   
                  DISPLAY '*  AO CARREGAR DADOS DA TABELA LPCLB037 *'   
                  DISPLAY '**************   CLLP2601   *************'   
                  PERFORM 9999-ROTINA-ERRO                              
           END-IF.                                                      
                                                                        
           PERFORM  1700-LER-TABELA37.                                  
                                                                        
      *---------------------------------------------------------------* 
       2100-FIM-MONTAR-TABELA. EXIT.                                    
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       2200-CONVERTE-UF                SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           MOVE   ZEROS   TO  WRK-UF-NUM.                               
                                                                        
           IF  WRK-UF-ALFA   EQUAL  ZEROS  OR  SPACES                   
               GO  TO   2200-FIM-CONVERTE-UF                            
           END-IF.                                                      
                                                                        
           SET LPCL-INDX  TO  1.                                        
                                                                        
           SEARCH  LPCL-OCC-UF  VARYING  LPCL-INDX                      
                   AT END   GO  TO   2200-FIM-CONVERTE-UF               
                   WHEN     LPCL-OCC-UF(LPCL-INDX) = WRK-UF-ALFA        
                            SET  WRK-UF-NUM  TO  LPCL-INDX.             
                                                                        
      *---------------------------------------------------------------* 
       2200-FIM-CONVERTE-UF. EXIT.                                      
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       3100-LER-ARQDB22S  SECTION.                                      
      *---------------------------------------------------------------* 
                                                                        
           READ ARQDB22S.                                               
                                                                        
           IF  WRK-FS-ARQDB22S EQUAL  '10'                              
               MOVE  'S'     TO     WRK-FIM-ARQDB22S                    
               GO            TO     3100-99-FIM                         
           END-IF.                                                      
                                                                        
           MOVE WRK-LEITURA                TO   WRK-OPERACAO.           
                                                                        
           PERFORM 1100-TESTAR-FS-ARQDB22S.                             
                                                                        
           ADD     1                       TO   ACU-LIDOS-ARQDB22S.     
                                                                        
           IF DB2-ID         EQUAL   'MO'                               
              MOVE    1 TO WRK-BLOQ-OPER                                
           ELSE                                                         
              IF DB2-ID      EQUAL   'CL' OR                            
                 DB2-ID      EQUAL   'LP'                               
                 MOVE 2 TO WRK-BLOQ-OPER                                
              ELSE                                                      
                 MOVE 0 TO WRK-BLOQ-OPER                                
              END-IF                                                    
           END-IF.                                                      
                                                                        
           MOVE    FD-DB2-UF TO WRK-UF-ALFA.                            
                                                                        
           PERFORM 2200-CONVERTE-UF.                                    
                                                                        
           IF DB2-NUMERO-DEV > 0                                        
              PERFORM 3150-PESQUISAR-DEVEDOR                            
                   VARYING  WRK-PESQ-DEV     FROM     1  BY  1          
                   UNTIL    WRK-PESQ-DEV     GREATER  WRK-PESQ-DEVEDOR  
           END-IF.                                                      
                                                                        
           IF DB2-NUMERO-AVAL > 0                                       
              PERFORM 3151-PESQUISAR-AVALISTA                           
                   VARYING  WRK-PESQ-AVA     FROM     1  BY  1          
                   UNTIL    WRK-PESQ-AVA     GREATER  WRK-PESQ-AVALISTA 
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       3100-99-FIM. EXIT.                                               
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       3150-PESQUISAR-DEVEDOR  SECTION.                                 
      *---------------------------------------------------------------* 
                                                                        
           MOVE  ZEROS                      TO  WRK-480-RETORNO         
                                                                        
           EVALUATE  WRK-PESQ-DEV                                       
                                                                        
           WHEN 001                                                     
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 002                                                     
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
           WHEN 003                                                     
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 004                                                     
                MOVE  DB2-CARTEIRA          TO  WRK-480-RET-CCART
           WHEN 005                                                     
                MOVE  DB2-CARTEIRA          TO  WRK-480-RET-CCART
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 006                                                     
                MOVE  DB2-CARTEIRA          TO  WRK-480-RET-CCART
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
           WHEN 007                                                     
                MOVE  DB2-CARTEIRA          TO  WRK-480-RET-CCART
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 008                                                     
                MOVE  DB2-COD-NATUREZA-OPER TO  WRK-NATU-AUX            
                MOVE  WRK-NATU-AUX-RR       TO  WRK-480-RET-NATUREZA    
           WHEN 009                                                     
                MOVE  DB2-COD-NATUREZA-OPER TO  WRK-NATU-AUX            
                MOVE  WRK-NATU-AUX-RR       TO  WRK-480-RET-NATUREZA    
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 010                                                     
                MOVE  DB2-COD-NATUREZA-OPER TO  WRK-NATU-AUX            
                MOVE  WRK-NATU-AUX-RR       TO  WRK-480-RET-NATUREZA    
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
           WHEN 011                                                     
                MOVE  DB2-COD-NATUREZA-OPER TO  WRK-NATU-AUX            
                MOVE  WRK-NATU-AUX-RR       TO  WRK-480-RET-NATUREZA    
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 012                                                     
                MOVE  DB2-COD-NATUREZA-OPER TO  WRK-NATU-AUX            
                MOVE  WRK-NATU-AUX-RR       TO  WRK-480-RET-NATUREZA    
                MOVE  DB2-CARTEIRA          TO  WRK-480-RET-CCART
           WHEN 013                                                     
                MOVE  DB2-COD-NATUREZA-OPER TO  WRK-NATU-AUX            
                MOVE  WRK-NATU-AUX-RR       TO  WRK-480-RET-NATUREZA    
                MOVE  DB2-CARTEIRA          TO  WRK-480-RET-CCART
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 014                                                     
                MOVE  DB2-COD-NATUREZA-OPER TO  WRK-NATU-AUX            
                MOVE  WRK-NATU-AUX-RR       TO  WRK-480-RET-NATUREZA    
                MOVE  DB2-CARTEIRA          TO  WRK-480-RET-CCART
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
           WHEN 015                                                     
                MOVE  DB2-COD-NATUREZA-OPER TO  WRK-NATU-AUX            
                MOVE  WRK-NATU-AUX-RR       TO  WRK-480-RET-NATUREZA    
                MOVE  DB2-CARTEIRA          TO  WRK-480-RET-CCART
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 016                                                     
                MOVE  DB2-AGENCIA           TO  WRK-480-RET-AGENCIA     
                MOVE  DB2-CONTA             TO  WRK-480-RET-CONTA       
           WHEN 017                                                     
                MOVE  DB2-AGENCIA           TO  WRK-480-RET-AGENCIA     
                MOVE  DB2-CONTA             TO  WRK-480-RET-CONTA       
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 018                                                     
                MOVE  DB2-AGENCIA           TO  WRK-480-RET-AGENCIA     
                MOVE  DB2-CONTA             TO  WRK-480-RET-CONTA       
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
           WHEN 019                                                     
                MOVE  DB2-AGENCIA           TO  WRK-480-RET-AGENCIA     
                MOVE  DB2-CONTA             TO  WRK-480-RET-CONTA       
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 020                                                     
                MOVE  DB2-AGENCIA           TO  WRK-480-RET-AGENCIA     
                MOVE  DB2-CONTA             TO  WRK-480-RET-CONTA       
                MOVE  DB2-CARTEIRA          TO  WRK-480-RET-CCART
           WHEN 021                                                     
                MOVE  DB2-AGENCIA           TO  WRK-480-RET-AGENCIA     
                MOVE  DB2-CONTA             TO  WRK-480-RET-CONTA       
                MOVE  DB2-CARTEIRA          TO  WRK-480-RET-CCART
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 022                                                     
                MOVE  DB2-AGENCIA           TO  WRK-480-RET-AGENCIA     
                MOVE  DB2-CONTA             TO  WRK-480-RET-CONTA       
                MOVE  DB2-CARTEIRA          TO  WRK-480-RET-CCART
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
           WHEN 023                                                     
                MOVE  DB2-AGENCIA           TO  WRK-480-RET-AGENCIA     
                MOVE  DB2-CONTA             TO  WRK-480-RET-CONTA       
                MOVE  DB2-CARTEIRA          TO  WRK-480-RET-CCART
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 024                                                     
                MOVE  DB2-AGENCIA           TO  WRK-480-RET-AGENCIA     
                MOVE  DB2-CONTA             TO  WRK-480-RET-CONTA       
                MOVE  DB2-COD-NATUREZA-OPER TO  WRK-NATU-AUX            
                MOVE  WRK-NATU-AUX-RR       TO  WRK-480-RET-NATUREZA    
           WHEN 025                                                     
                MOVE  DB2-AGENCIA           TO  WRK-480-RET-AGENCIA     
                MOVE  DB2-CONTA             TO  WRK-480-RET-CONTA       
                MOVE  DB2-COD-NATUREZA-OPER TO  WRK-NATU-AUX            
                MOVE  WRK-NATU-AUX-RR       TO  WRK-480-RET-NATUREZA    
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 026                                                     
                MOVE  DB2-AGENCIA           TO  WRK-480-RET-AGENCIA     
                MOVE  DB2-CONTA             TO  WRK-480-RET-CONTA       
                MOVE  DB2-COD-NATUREZA-OPER TO  WRK-NATU-AUX            
                MOVE  WRK-NATU-AUX-RR       TO  WRK-480-RET-NATUREZA    
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
           WHEN 027                                                     
                MOVE  DB2-AGENCIA           TO  WRK-480-RET-AGENCIA     
                MOVE  DB2-CONTA             TO  WRK-480-RET-CONTA       
                MOVE  DB2-COD-NATUREZA-OPER TO  WRK-NATU-AUX            
                MOVE  WRK-NATU-AUX-RR       TO  WRK-480-RET-NATUREZA    
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 028                                                     
                MOVE  DB2-AGENCIA           TO  WRK-480-RET-AGENCIA     
                MOVE  DB2-CONTA             TO  WRK-480-RET-CONTA       
                MOVE  DB2-COD-NATUREZA-OPER TO  WRK-NATU-AUX            
                MOVE  WRK-NATU-AUX-RR       TO  WRK-480-RET-NATUREZA    
                MOVE  DB2-CARTEIRA          TO  WRK-480-RET-CCART
           WHEN 029                                                     
                MOVE  DB2-AGENCIA           TO  WRK-480-RET-AGENCIA     
                MOVE  DB2-CONTA             TO  WRK-480-RET-CONTA       
                MOVE  DB2-COD-NATUREZA-OPER TO  WRK-NATU-AUX            
                MOVE  WRK-NATU-AUX-RR       TO  WRK-480-RET-NATUREZA    
                MOVE  DB2-CARTEIRA          TO  WRK-480-RET-CCART
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 030                                                     
                MOVE  DB2-AGENCIA           TO  WRK-480-RET-AGENCIA     
                MOVE  DB2-CONTA             TO  WRK-480-RET-CONTA       
                MOVE  DB2-COD-NATUREZA-OPER TO  WRK-NATU-AUX            
                MOVE  WRK-NATU-AUX-RR       TO  WRK-480-RET-NATUREZA    
                MOVE  DB2-CARTEIRA          TO  WRK-480-RET-CCART
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
           WHEN 031                                                     
                MOVE  DB2-AGENCIA           TO  WRK-480-RET-AGENCIA     
                MOVE  DB2-CONTA             TO  WRK-480-RET-CONTA       
                MOVE  DB2-COD-NATUREZA-OPER TO  WRK-NATU-AUX            
                MOVE  WRK-NATU-AUX-RR       TO  WRK-480-RET-NATUREZA    
                MOVE  DB2-CARTEIRA          TO  WRK-480-RET-CCART
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 032                                                     
                MOVE  DB2-NUMERO-DEV        TO  WRK-480-RET-CCGC-CPF    
                MOVE  DB2-FILIAL            TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  DB2-CTR-DEV           TO  WRK-480-RET-CCTRL-CGC   
           WHEN 033                                                     
                MOVE  DB2-NUMERO-DEV        TO  WRK-480-RET-CCGC-CPF    
                MOVE  DB2-FILIAL            TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  DB2-CTR-DEV           TO  WRK-480-RET-CCTRL-CGC   
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 034                                                     
                MOVE  DB2-NUMERO-DEV        TO  WRK-480-RET-CCGC-CPF    
                MOVE  DB2-FILIAL            TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  DB2-CTR-DEV           TO  WRK-480-RET-CCTRL-CGC   
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
           WHEN 035                                                     
                MOVE  DB2-NUMERO-DEV        TO  WRK-480-RET-CCGC-CPF    
                MOVE  DB2-FILIAL            TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  DB2-CTR-DEV           TO  WRK-480-RET-CCTRL-CGC   
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 036                                                     
                MOVE  DB2-NUMERO-DEV        TO  WRK-480-RET-CCGC-CPF    
                MOVE  DB2-FILIAL            TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  DB2-CTR-DEV           TO  WRK-480-RET-CCTRL-CGC   
                MOVE  DB2-CARTEIRA          TO  WRK-480-RET-CCART
           WHEN 037                                                     
                MOVE  DB2-NUMERO-DEV        TO  WRK-480-RET-CCGC-CPF    
                MOVE  DB2-FILIAL            TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  DB2-CTR-DEV           TO  WRK-480-RET-CCTRL-CGC   
                MOVE  DB2-CARTEIRA          TO  WRK-480-RET-CCART
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 038                                                     
                MOVE  DB2-NUMERO-DEV        TO  WRK-480-RET-CCGC-CPF    
                MOVE  DB2-FILIAL            TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  DB2-CTR-DEV           TO  WRK-480-RET-CCTRL-CGC   
                MOVE  DB2-CARTEIRA          TO  WRK-480-RET-CCART
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
           WHEN 039                                                     
                MOVE  DB2-NUMERO-DEV        TO  WRK-480-RET-CCGC-CPF    
                MOVE  DB2-FILIAL            TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  DB2-CTR-DEV           TO  WRK-480-RET-CCTRL-CGC   
                MOVE  DB2-CARTEIRA          TO  WRK-480-RET-CCART
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 040                                                     
                MOVE  DB2-NUMERO-DEV        TO  WRK-480-RET-CCGC-CPF    
                MOVE  DB2-FILIAL            TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  DB2-CTR-DEV           TO  WRK-480-RET-CCTRL-CGC   
                MOVE  DB2-COD-NATUREZA-OPER TO  WRK-NATU-AUX            
                MOVE  WRK-NATU-AUX-RR       TO  WRK-480-RET-NATUREZA    
           WHEN 041                                                     
                MOVE  DB2-NUMERO-DEV        TO  WRK-480-RET-CCGC-CPF    
                MOVE  DB2-FILIAL            TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  DB2-CTR-DEV           TO  WRK-480-RET-CCTRL-CGC   
                MOVE  DB2-COD-NATUREZA-OPER TO  WRK-NATU-AUX            
                MOVE  WRK-NATU-AUX-RR       TO  WRK-480-RET-NATUREZA    
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 042                                                     
                MOVE  DB2-NUMERO-DEV        TO  WRK-480-RET-CCGC-CPF    
                MOVE  DB2-FILIAL            TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  DB2-CTR-DEV           TO  WRK-480-RET-CCTRL-CGC   
                MOVE  DB2-COD-NATUREZA-OPER TO  WRK-NATU-AUX            
                MOVE  WRK-NATU-AUX-RR       TO  WRK-480-RET-NATUREZA    
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
           WHEN 043                                                     
                MOVE  DB2-NUMERO-DEV        TO  WRK-480-RET-CCGC-CPF    
                MOVE  DB2-FILIAL            TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  DB2-CTR-DEV           TO  WRK-480-RET-CCTRL-CGC   
                MOVE  DB2-COD-NATUREZA-OPER TO  WRK-NATU-AUX            
                MOVE  WRK-NATU-AUX-RR       TO  WRK-480-RET-NATUREZA    
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 044                                                     
                MOVE  DB2-NUMERO-DEV        TO  WRK-480-RET-CCGC-CPF    
                MOVE  DB2-FILIAL            TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  DB2-CTR-DEV           TO  WRK-480-RET-CCTRL-CGC   
                MOVE  DB2-COD-NATUREZA-OPER TO  WRK-NATU-AUX            
                MOVE  WRK-NATU-AUX-RR       TO  WRK-480-RET-NATUREZA    
                MOVE  DB2-CARTEIRA          TO  WRK-480-RET-CCART
           WHEN 045                                                     
                MOVE  DB2-NUMERO-DEV        TO  WRK-480-RET-CCGC-CPF    
                MOVE  DB2-FILIAL            TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  DB2-CTR-DEV           TO  WRK-480-RET-CCTRL-CGC   
                MOVE  DB2-COD-NATUREZA-OPER TO  WRK-NATU-AUX            
                MOVE  WRK-NATU-AUX-RR       TO  WRK-480-RET-NATUREZA    
                MOVE  DB2-CARTEIRA          TO  WRK-480-RET-CCART
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 046                                                     
                MOVE  DB2-NUMERO-DEV        TO  WRK-480-RET-CCGC-CPF    
                MOVE  DB2-FILIAL            TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  DB2-CTR-DEV           TO  WRK-480-RET-CCTRL-CGC   
                MOVE  DB2-COD-NATUREZA-OPER TO  WRK-NATU-AUX            
                MOVE  WRK-NATU-AUX-RR       TO  WRK-480-RET-NATUREZA    
                MOVE  DB2-CARTEIRA          TO  WRK-480-RET-CCART
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
           WHEN 047                                                     
                MOVE  DB2-NUMERO-DEV        TO  WRK-480-RET-CCGC-CPF    
                MOVE  DB2-FILIAL            TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  DB2-CTR-DEV           TO  WRK-480-RET-CCTRL-CGC   
                MOVE  DB2-COD-NATUREZA-OPER TO  WRK-NATU-AUX            
                MOVE  WRK-NATU-AUX-RR       TO  WRK-480-RET-NATUREZA    
                MOVE  DB2-CARTEIRA          TO  WRK-480-RET-CCART
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 048                                                     
                MOVE  DB2-NUMERO-DEV        TO  WRK-480-RET-CCGC-CPF    
                MOVE  DB2-FILIAL            TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  DB2-CTR-DEV           TO  WRK-480-RET-CCTRL-CGC   
                MOVE  DB2-AGENCIA           TO  WRK-480-RET-AGENCIA     
                MOVE  DB2-CONTA             TO  WRK-480-RET-CONTA       
           WHEN 049                                                     
                MOVE  DB2-NUMERO-DEV        TO  WRK-480-RET-CCGC-CPF    
                MOVE  DB2-FILIAL            TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  DB2-CTR-DEV           TO  WRK-480-RET-CCTRL-CGC   
                MOVE  DB2-AGENCIA           TO  WRK-480-RET-AGENCIA     
                MOVE  DB2-CONTA             TO  WRK-480-RET-CONTA       
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 050                                                     
                MOVE  DB2-NUMERO-DEV        TO  WRK-480-RET-CCGC-CPF    
                MOVE  DB2-FILIAL            TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  DB2-CTR-DEV           TO  WRK-480-RET-CCTRL-CGC   
                MOVE  DB2-AGENCIA           TO  WRK-480-RET-AGENCIA     
                MOVE  DB2-CONTA             TO  WRK-480-RET-CONTA       
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
           WHEN 051                                                     
                MOVE  DB2-NUMERO-DEV        TO  WRK-480-RET-CCGC-CPF    
                MOVE  DB2-FILIAL            TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  DB2-CTR-DEV           TO  WRK-480-RET-CCTRL-CGC   
                MOVE  DB2-AGENCIA           TO  WRK-480-RET-AGENCIA     
                MOVE  DB2-CONTA             TO  WRK-480-RET-CONTA       
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 052                                                     
                MOVE  DB2-NUMERO-DEV        TO  WRK-480-RET-CCGC-CPF    
                MOVE  DB2-FILIAL            TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  DB2-CTR-DEV           TO  WRK-480-RET-CCTRL-CGC   
                MOVE  DB2-AGENCIA           TO  WRK-480-RET-AGENCIA     
                MOVE  DB2-CONTA             TO  WRK-480-RET-CONTA       
                MOVE  DB2-CARTEIRA          TO  WRK-480-RET-CCART
           WHEN 053                                                     
                MOVE  DB2-NUMERO-DEV        TO  WRK-480-RET-CCGC-CPF    
                MOVE  DB2-FILIAL            TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  DB2-CTR-DEV           TO  WRK-480-RET-CCTRL-CGC   
                MOVE  DB2-AGENCIA           TO  WRK-480-RET-AGENCIA     
                MOVE  DB2-CONTA             TO  WRK-480-RET-CONTA       
                MOVE  DB2-CARTEIRA          TO  WRK-480-RET-CCART
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 054                                                     
                MOVE  DB2-NUMERO-DEV        TO  WRK-480-RET-CCGC-CPF    
                MOVE  DB2-FILIAL            TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  DB2-CTR-DEV           TO  WRK-480-RET-CCTRL-CGC   
                MOVE  DB2-AGENCIA           TO  WRK-480-RET-AGENCIA     
                MOVE  DB2-CONTA             TO  WRK-480-RET-CONTA       
                MOVE  DB2-CARTEIRA          TO  WRK-480-RET-CCART
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
           WHEN 055                                                     
                MOVE  DB2-NUMERO-DEV        TO  WRK-480-RET-CCGC-CPF    
                MOVE  DB2-FILIAL            TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  DB2-CTR-DEV           TO  WRK-480-RET-CCTRL-CGC   
                MOVE  DB2-AGENCIA           TO  WRK-480-RET-AGENCIA     
                MOVE  DB2-CONTA             TO  WRK-480-RET-CONTA       
                MOVE  DB2-CARTEIRA          TO  WRK-480-RET-CCART
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 056                                                     
                MOVE  DB2-NUMERO-DEV        TO  WRK-480-RET-CCGC-CPF    
                MOVE  DB2-FILIAL            TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  DB2-CTR-DEV           TO  WRK-480-RET-CCTRL-CGC   
                MOVE  DB2-AGENCIA           TO  WRK-480-RET-AGENCIA     
                MOVE  DB2-CONTA             TO  WRK-480-RET-CONTA       
                MOVE  DB2-COD-NATUREZA-OPER TO  WRK-NATU-AUX            
                MOVE  WRK-NATU-AUX-RR       TO  WRK-480-RET-NATUREZA    
           WHEN 057                                                     
                MOVE  DB2-NUMERO-DEV        TO  WRK-480-RET-CCGC-CPF    
                MOVE  DB2-FILIAL            TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  DB2-CTR-DEV           TO  WRK-480-RET-CCTRL-CGC   
                MOVE  DB2-AGENCIA           TO  WRK-480-RET-AGENCIA     
                MOVE  DB2-CONTA             TO  WRK-480-RET-CONTA       
                MOVE  DB2-COD-NATUREZA-OPER TO  WRK-NATU-AUX            
                MOVE  WRK-NATU-AUX-RR       TO  WRK-480-RET-NATUREZA    
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 058                                                     
                MOVE  DB2-NUMERO-DEV        TO  WRK-480-RET-CCGC-CPF    
                MOVE  DB2-FILIAL            TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  DB2-CTR-DEV           TO  WRK-480-RET-CCTRL-CGC   
                MOVE  DB2-AGENCIA           TO  WRK-480-RET-AGENCIA     
                MOVE  DB2-CONTA             TO  WRK-480-RET-CONTA       
                MOVE  DB2-COD-NATUREZA-OPER TO  WRK-NATU-AUX            
                MOVE  WRK-NATU-AUX-RR       TO  WRK-480-RET-NATUREZA    
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
           WHEN 059                                                     
                MOVE  DB2-NUMERO-DEV        TO  WRK-480-RET-CCGC-CPF    
                MOVE  DB2-FILIAL            TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  DB2-CTR-DEV           TO  WRK-480-RET-CCTRL-CGC   
                MOVE  DB2-AGENCIA           TO  WRK-480-RET-AGENCIA     
                MOVE  DB2-CONTA             TO  WRK-480-RET-CONTA       
                MOVE  DB2-COD-NATUREZA-OPER TO  WRK-NATU-AUX            
                MOVE  WRK-NATU-AUX-RR       TO  WRK-480-RET-NATUREZA    
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 060                                                     
                MOVE  DB2-NUMERO-DEV        TO  WRK-480-RET-CCGC-CPF    
                MOVE  DB2-FILIAL            TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  DB2-CTR-DEV           TO  WRK-480-RET-CCTRL-CGC   
                MOVE  DB2-AGENCIA           TO  WRK-480-RET-AGENCIA     
                MOVE  DB2-CONTA             TO  WRK-480-RET-CONTA       
                MOVE  DB2-COD-NATUREZA-OPER TO  WRK-NATU-AUX            
                MOVE  WRK-NATU-AUX-RR       TO  WRK-480-RET-NATUREZA    
                MOVE  DB2-CARTEIRA          TO  WRK-480-RET-CCART
           WHEN 061                                                     
                MOVE  DB2-NUMERO-DEV        TO  WRK-480-RET-CCGC-CPF    
                MOVE  DB2-FILIAL            TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  DB2-CTR-DEV           TO  WRK-480-RET-CCTRL-CGC   
                MOVE  DB2-AGENCIA           TO  WRK-480-RET-AGENCIA     
                MOVE  DB2-CONTA             TO  WRK-480-RET-CONTA       
                MOVE  DB2-COD-NATUREZA-OPER TO  WRK-NATU-AUX            
                MOVE  WRK-NATU-AUX-RR       TO  WRK-480-RET-NATUREZA    
                MOVE  DB2-CARTEIRA          TO  WRK-480-RET-CCART
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 062                                                     
                MOVE  DB2-NUMERO-DEV        TO  WRK-480-RET-CCGC-CPF    
                MOVE  DB2-FILIAL            TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  DB2-CTR-DEV           TO  WRK-480-RET-CCTRL-CGC   
                MOVE  DB2-AGENCIA           TO  WRK-480-RET-AGENCIA     
                MOVE  DB2-CONTA             TO  WRK-480-RET-CONTA       
                MOVE  DB2-COD-NATUREZA-OPER TO  WRK-NATU-AUX            
                MOVE  WRK-NATU-AUX-RR       TO  WRK-480-RET-NATUREZA    
                MOVE  DB2-CARTEIRA          TO  WRK-480-RET-CCART
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
           WHEN 063                                                     
                MOVE  DB2-NUMERO-DEV        TO  WRK-480-RET-CCGC-CPF    
                MOVE  DB2-FILIAL            TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  DB2-CTR-DEV           TO  WRK-480-RET-CCTRL-CGC   
                MOVE  DB2-AGENCIA           TO  WRK-480-RET-AGENCIA     
                MOVE  DB2-CONTA             TO  WRK-480-RET-CONTA       
                MOVE  DB2-COD-NATUREZA-OPER TO  WRK-NATU-AUX            
                MOVE  WRK-NATU-AUX-RR       TO  WRK-480-RET-NATUREZA    
                MOVE  DB2-CARTEIRA          TO  WRK-480-RET-CCART
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
                                                                        
           END-EVALUATE.                                                
                                                                        
           CALL   WRK-MODULO       USING        WRK-480-CODIGO3         
                                                WRK-480-NOME-TABELA     
                                                WRK-480-RETORNO-CHAVE   
                                                WRK-480-RETORNO.        
                                                                        
           IF   RETURN-CODE        EQUAL        ZEROS                   
                PERFORM 1002-GRAVA-BLQREST-DEV                          
                PERFORM 5000-GERAR-RELATORIO-DEV                        
                MOVE    099                 TO  WRK-PESQ-DEV            
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       3150-99-FIM. EXIT.                                               
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       3151-PESQUISAR-AVALISTA SECTION.                                 
      *---------------------------------------------------------------* 
                                                                        
           MOVE  ZEROS                      TO  WRK-480-RETORNO         
                                                                        
           EVALUATE  WRK-PESQ-AVA                                       
                                                                        
           WHEN 001                                                     
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 002                                                     
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
           WHEN 003                                                     
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 004                                                     
                MOVE  DB2-CARTEIRA          TO  WRK-480-RET-CCART
           WHEN 005                                                     
                MOVE  DB2-CARTEIRA          TO  WRK-480-RET-CCART
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 006                                                     
                MOVE  DB2-CARTEIRA          TO  WRK-480-RET-CCART
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
           WHEN 007                                                     
                MOVE  DB2-CARTEIRA          TO  WRK-480-RET-CCART
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 008                                                     
                MOVE  DB2-COD-NATUREZA-OPER TO  WRK-NATU-AUX            
                MOVE  WRK-NATU-AUX-RR       TO  WRK-480-RET-NATUREZA    
           WHEN 009                                                     
                MOVE  DB2-COD-NATUREZA-OPER TO  WRK-NATU-AUX            
                MOVE  WRK-NATU-AUX-RR       TO  WRK-480-RET-NATUREZA    
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 010                                                     
                MOVE  DB2-COD-NATUREZA-OPER TO  WRK-NATU-AUX            
                MOVE  WRK-NATU-AUX-RR       TO  WRK-480-RET-NATUREZA    
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
           WHEN 011                                                     
                MOVE  DB2-COD-NATUREZA-OPER TO  WRK-NATU-AUX            
                MOVE  WRK-NATU-AUX-RR       TO  WRK-480-RET-NATUREZA    
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 012                                                     
                MOVE  DB2-COD-NATUREZA-OPER TO  WRK-NATU-AUX            
                MOVE  WRK-NATU-AUX-RR       TO  WRK-480-RET-NATUREZA    
                MOVE  DB2-CARTEIRA          TO  WRK-480-RET-CCART
           WHEN 013                                                     
                MOVE  DB2-COD-NATUREZA-OPER TO  WRK-NATU-AUX            
                MOVE  WRK-NATU-AUX-RR       TO  WRK-480-RET-NATUREZA    
                MOVE  DB2-CARTEIRA          TO  WRK-480-RET-CCART
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 014                                                     
                MOVE  DB2-COD-NATUREZA-OPER TO  WRK-NATU-AUX            
                MOVE  WRK-NATU-AUX-RR       TO  WRK-480-RET-NATUREZA    
                MOVE  DB2-CARTEIRA          TO  WRK-480-RET-CCART
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
           WHEN 015                                                     
                MOVE  DB2-COD-NATUREZA-OPER TO  WRK-NATU-AUX            
                MOVE  WRK-NATU-AUX-RR       TO  WRK-480-RET-NATUREZA    
                MOVE  DB2-CARTEIRA          TO  WRK-480-RET-CCART
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 016                                                     
                MOVE  DB2-AGENCIA           TO  WRK-480-RET-AGENCIA     
                MOVE  DB2-CONTA             TO  WRK-480-RET-CONTA       
           WHEN 017                                                     
                MOVE  DB2-AGENCIA           TO  WRK-480-RET-AGENCIA     
                MOVE  DB2-CONTA             TO  WRK-480-RET-CONTA       
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 018                                                     
                MOVE  DB2-AGENCIA           TO  WRK-480-RET-AGENCIA     
                MOVE  DB2-CONTA             TO  WRK-480-RET-CONTA       
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
           WHEN 019                                                     
                MOVE  DB2-AGENCIA           TO  WRK-480-RET-AGENCIA     
                MOVE  DB2-CONTA             TO  WRK-480-RET-CONTA       
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 020                                                     
                MOVE  DB2-AGENCIA           TO  WRK-480-RET-AGENCIA     
                MOVE  DB2-CONTA             TO  WRK-480-RET-CONTA       
                MOVE  DB2-CARTEIRA          TO  WRK-480-RET-CCART
           WHEN 021                                                     
                MOVE  DB2-AGENCIA           TO  WRK-480-RET-AGENCIA     
                MOVE  DB2-CONTA             TO  WRK-480-RET-CONTA       
                MOVE  DB2-CARTEIRA          TO  WRK-480-RET-CCART
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 022                                                     
                MOVE  DB2-AGENCIA           TO  WRK-480-RET-AGENCIA     
                MOVE  DB2-CONTA             TO  WRK-480-RET-CONTA       
                MOVE  DB2-CARTEIRA          TO  WRK-480-RET-CCART
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
           WHEN 023                                                     
                MOVE  DB2-AGENCIA           TO  WRK-480-RET-AGENCIA     
                MOVE  DB2-CONTA             TO  WRK-480-RET-CONTA       
                MOVE  DB2-CARTEIRA          TO  WRK-480-RET-CCART
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 024                                                     
                MOVE  DB2-AGENCIA           TO  WRK-480-RET-AGENCIA     
                MOVE  DB2-CONTA             TO  WRK-480-RET-CONTA       
                MOVE  DB2-COD-NATUREZA-OPER TO  WRK-NATU-AUX            
                MOVE  WRK-NATU-AUX-RR       TO  WRK-480-RET-NATUREZA    
           WHEN 025                                                     
                MOVE  DB2-AGENCIA           TO  WRK-480-RET-AGENCIA     
                MOVE  DB2-CONTA             TO  WRK-480-RET-CONTA       
                MOVE  DB2-COD-NATUREZA-OPER TO  WRK-NATU-AUX            
                MOVE  WRK-NATU-AUX-RR       TO  WRK-480-RET-NATUREZA    
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 026                                                     
                MOVE  DB2-AGENCIA           TO  WRK-480-RET-AGENCIA     
                MOVE  DB2-CONTA             TO  WRK-480-RET-CONTA       
                MOVE  DB2-COD-NATUREZA-OPER TO  WRK-NATU-AUX            
                MOVE  WRK-NATU-AUX-RR       TO  WRK-480-RET-NATUREZA    
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
           WHEN 027                                                     
                MOVE  DB2-AGENCIA           TO  WRK-480-RET-AGENCIA     
                MOVE  DB2-CONTA             TO  WRK-480-RET-CONTA       
                MOVE  DB2-COD-NATUREZA-OPER TO  WRK-NATU-AUX            
                MOVE  WRK-NATU-AUX-RR       TO  WRK-480-RET-NATUREZA    
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 028                                                     
                MOVE  DB2-AGENCIA           TO  WRK-480-RET-AGENCIA     
                MOVE  DB2-CONTA             TO  WRK-480-RET-CONTA       
                MOVE  DB2-COD-NATUREZA-OPER TO  WRK-NATU-AUX            
                MOVE  WRK-NATU-AUX-RR       TO  WRK-480-RET-NATUREZA    
                MOVE  DB2-CARTEIRA          TO  WRK-480-RET-CCART
           WHEN 029                                                     
                MOVE  DB2-AGENCIA           TO  WRK-480-RET-AGENCIA     
                MOVE  DB2-CONTA             TO  WRK-480-RET-CONTA       
                MOVE  DB2-COD-NATUREZA-OPER TO  WRK-NATU-AUX            
                MOVE  WRK-NATU-AUX-RR       TO  WRK-480-RET-NATUREZA    
                MOVE  DB2-CARTEIRA          TO  WRK-480-RET-CCART
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 030                                                     
                MOVE  DB2-AGENCIA           TO  WRK-480-RET-AGENCIA     
                MOVE  DB2-CONTA             TO  WRK-480-RET-CONTA       
                MOVE  DB2-COD-NATUREZA-OPER TO  WRK-NATU-AUX            
                MOVE  WRK-NATU-AUX-RR       TO  WRK-480-RET-NATUREZA    
                MOVE  DB2-CARTEIRA          TO  WRK-480-RET-CCART
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
           WHEN 031                                                     
                MOVE  DB2-AGENCIA           TO  WRK-480-RET-AGENCIA     
                MOVE  DB2-CONTA             TO  WRK-480-RET-CONTA       
                MOVE  DB2-COD-NATUREZA-OPER TO  WRK-NATU-AUX            
                MOVE  WRK-NATU-AUX-RR       TO  WRK-480-RET-NATUREZA    
                MOVE  DB2-CARTEIRA          TO  WRK-480-RET-CCART
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 032                                                     
                MOVE  DB2-NUMERO-AVAL       TO  WRK-480-RET-CCGC-CPF    
                MOVE  DB2-FILIAL-AVAL       TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  DB2-CTR-AVAL          TO  WRK-480-RET-CCTRL-CGC   
           WHEN 033                                                     
                MOVE  DB2-NUMERO-AVAL       TO  WRK-480-RET-CCGC-CPF    
                MOVE  DB2-FILIAL-AVAL       TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  DB2-CTR-AVAL          TO  WRK-480-RET-CCTRL-CGC   
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 034                                                     
                MOVE  DB2-NUMERO-AVAL       TO  WRK-480-RET-CCGC-CPF    
                MOVE  DB2-FILIAL-AVAL       TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  DB2-CTR-AVAL          TO  WRK-480-RET-CCTRL-CGC   
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
           WHEN 035                                                     
                MOVE  DB2-NUMERO-AVAL       TO  WRK-480-RET-CCGC-CPF    
                MOVE  DB2-FILIAL-AVAL       TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  DB2-CTR-AVAL          TO  WRK-480-RET-CCTRL-CGC   
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 036                                                     
                MOVE  DB2-NUMERO-AVAL       TO  WRK-480-RET-CCGC-CPF    
                MOVE  DB2-FILIAL-AVAL       TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  DB2-CTR-AVAL          TO  WRK-480-RET-CCTRL-CGC   
                MOVE  DB2-CARTEIRA          TO  WRK-480-RET-CCART
           WHEN 037                                                     
                MOVE  DB2-NUMERO-AVAL       TO  WRK-480-RET-CCGC-CPF    
                MOVE  DB2-FILIAL-AVAL       TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  DB2-CTR-AVAL          TO  WRK-480-RET-CCTRL-CGC   
                MOVE  DB2-CARTEIRA          TO  WRK-480-RET-CCART
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 038                                                     
                MOVE  DB2-NUMERO-AVAL       TO  WRK-480-RET-CCGC-CPF    
                MOVE  DB2-FILIAL-AVAL       TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  DB2-CTR-AVAL          TO  WRK-480-RET-CCTRL-CGC   
                MOVE  DB2-CARTEIRA          TO  WRK-480-RET-CCART
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
           WHEN 039                                                     
                MOVE  DB2-NUMERO-AVAL       TO  WRK-480-RET-CCGC-CPF    
                MOVE  DB2-FILIAL-AVAL       TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  DB2-CTR-AVAL          TO  WRK-480-RET-CCTRL-CGC   
                MOVE  DB2-CARTEIRA          TO  WRK-480-RET-CCART
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 040                                                     
                MOVE  DB2-NUMERO-AVAL       TO  WRK-480-RET-CCGC-CPF    
                MOVE  DB2-FILIAL-AVAL       TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  DB2-CTR-AVAL          TO  WRK-480-RET-CCTRL-CGC   
                MOVE  DB2-COD-NATUREZA-OPER TO  WRK-NATU-AUX            
                MOVE  WRK-NATU-AUX-RR       TO  WRK-480-RET-NATUREZA    
           WHEN 041                                                     
                MOVE  DB2-NUMERO-AVAL       TO  WRK-480-RET-CCGC-CPF    
                MOVE  DB2-FILIAL-AVAL       TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  DB2-CTR-AVAL          TO  WRK-480-RET-CCTRL-CGC   
                MOVE  DB2-COD-NATUREZA-OPER TO  WRK-NATU-AUX            
                MOVE  WRK-NATU-AUX-RR       TO  WRK-480-RET-NATUREZA    
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 042                                                     
                MOVE  DB2-NUMERO-AVAL       TO  WRK-480-RET-CCGC-CPF    
                MOVE  DB2-FILIAL-AVAL       TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  DB2-CTR-AVAL          TO  WRK-480-RET-CCTRL-CGC   
                MOVE  DB2-COD-NATUREZA-OPER TO  WRK-NATU-AUX            
                MOVE  WRK-NATU-AUX-RR       TO  WRK-480-RET-NATUREZA    
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
           WHEN 043                                                     
                MOVE  DB2-NUMERO-AVAL       TO  WRK-480-RET-CCGC-CPF    
                MOVE  DB2-FILIAL-AVAL       TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  DB2-CTR-AVAL          TO  WRK-480-RET-CCTRL-CGC   
                MOVE  DB2-COD-NATUREZA-OPER TO  WRK-NATU-AUX            
                MOVE  WRK-NATU-AUX-RR       TO  WRK-480-RET-NATUREZA    
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 044                                                     
                MOVE  DB2-NUMERO-AVAL       TO  WRK-480-RET-CCGC-CPF    
                MOVE  DB2-FILIAL-AVAL       TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  DB2-CTR-AVAL          TO  WRK-480-RET-CCTRL-CGC   
                MOVE  DB2-COD-NATUREZA-OPER TO  WRK-NATU-AUX            
                MOVE  WRK-NATU-AUX-RR       TO  WRK-480-RET-NATUREZA    
                MOVE  DB2-CARTEIRA          TO  WRK-480-RET-CCART
           WHEN 045                                                     
                MOVE  DB2-NUMERO-AVAL       TO  WRK-480-RET-CCGC-CPF    
                MOVE  DB2-FILIAL-AVAL       TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  DB2-CTR-AVAL          TO  WRK-480-RET-CCTRL-CGC   
                MOVE  DB2-COD-NATUREZA-OPER TO  WRK-NATU-AUX            
                MOVE  WRK-NATU-AUX-RR       TO  WRK-480-RET-NATUREZA    
                MOVE  DB2-CARTEIRA          TO  WRK-480-RET-CCART
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 046                                                     
                MOVE  DB2-NUMERO-AVAL       TO  WRK-480-RET-CCGC-CPF    
                MOVE  DB2-FILIAL-AVAL       TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  DB2-CTR-AVAL          TO  WRK-480-RET-CCTRL-CGC   
                MOVE  DB2-COD-NATUREZA-OPER TO  WRK-NATU-AUX            
                MOVE  WRK-NATU-AUX-RR       TO  WRK-480-RET-NATUREZA    
                MOVE  DB2-CARTEIRA          TO  WRK-480-RET-CCART
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
           WHEN 047                                                     
                MOVE  DB2-NUMERO-AVAL       TO  WRK-480-RET-CCGC-CPF    
                MOVE  DB2-FILIAL-AVAL       TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  DB2-CTR-AVAL          TO  WRK-480-RET-CCTRL-CGC   
                MOVE  DB2-COD-NATUREZA-OPER TO  WRK-NATU-AUX            
                MOVE  WRK-NATU-AUX-RR       TO  WRK-480-RET-NATUREZA    
                MOVE  DB2-CARTEIRA          TO  WRK-480-RET-CCART
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 048                                                     
                MOVE  DB2-NUMERO-AVAL       TO  WRK-480-RET-CCGC-CPF    
                MOVE  DB2-FILIAL-AVAL       TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  DB2-CTR-AVAL          TO  WRK-480-RET-CCTRL-CGC   
                MOVE  DB2-AGENCIA           TO  WRK-480-RET-AGENCIA     
                MOVE  DB2-CONTA             TO  WRK-480-RET-CONTA       
           WHEN 049                                                     
                MOVE  DB2-NUMERO-AVAL       TO  WRK-480-RET-CCGC-CPF    
                MOVE  DB2-FILIAL-AVAL       TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  DB2-CTR-AVAL          TO  WRK-480-RET-CCTRL-CGC   
                MOVE  DB2-AGENCIA           TO  WRK-480-RET-AGENCIA     
                MOVE  DB2-CONTA             TO  WRK-480-RET-CONTA       
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 050                                                     
                MOVE  DB2-NUMERO-AVAL       TO  WRK-480-RET-CCGC-CPF    
                MOVE  DB2-FILIAL-AVAL       TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  DB2-CTR-AVAL          TO  WRK-480-RET-CCTRL-CGC   
                MOVE  DB2-AGENCIA           TO  WRK-480-RET-AGENCIA     
                MOVE  DB2-CONTA             TO  WRK-480-RET-CONTA       
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
           WHEN 051                                                     
                MOVE  DB2-NUMERO-AVAL       TO  WRK-480-RET-CCGC-CPF    
                MOVE  DB2-FILIAL-AVAL       TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  DB2-CTR-AVAL          TO  WRK-480-RET-CCTRL-CGC   
                MOVE  DB2-AGENCIA           TO  WRK-480-RET-AGENCIA     
                MOVE  DB2-CONTA             TO  WRK-480-RET-CONTA       
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 052                                                     
                MOVE  DB2-NUMERO-AVAL       TO  WRK-480-RET-CCGC-CPF    
                MOVE  DB2-FILIAL-AVAL       TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  DB2-CTR-AVAL          TO  WRK-480-RET-CCTRL-CGC   
                MOVE  DB2-AGENCIA           TO  WRK-480-RET-AGENCIA     
                MOVE  DB2-CONTA             TO  WRK-480-RET-CONTA       
                MOVE  DB2-CARTEIRA          TO  WRK-480-RET-CCART
           WHEN 053                                                     
                MOVE  DB2-NUMERO-AVAL       TO  WRK-480-RET-CCGC-CPF    
                MOVE  DB2-FILIAL-AVAL       TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  DB2-CTR-AVAL          TO  WRK-480-RET-CCTRL-CGC   
                MOVE  DB2-AGENCIA           TO  WRK-480-RET-AGENCIA     
                MOVE  DB2-CONTA             TO  WRK-480-RET-CONTA       
                MOVE  DB2-CARTEIRA          TO  WRK-480-RET-CCART
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 054                                                     
                MOVE  DB2-NUMERO-AVAL       TO  WRK-480-RET-CCGC-CPF    
                MOVE  DB2-FILIAL-AVAL       TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  DB2-CTR-AVAL          TO  WRK-480-RET-CCTRL-CGC   
                MOVE  DB2-AGENCIA           TO  WRK-480-RET-AGENCIA     
                MOVE  DB2-CONTA             TO  WRK-480-RET-CONTA       
                MOVE  DB2-CARTEIRA          TO  WRK-480-RET-CCART
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
           WHEN 055                                                     
                MOVE  DB2-NUMERO-AVAL       TO  WRK-480-RET-CCGC-CPF    
                MOVE  DB2-FILIAL-AVAL       TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  DB2-CTR-AVAL          TO  WRK-480-RET-CCTRL-CGC   
                MOVE  DB2-AGENCIA           TO  WRK-480-RET-AGENCIA     
                MOVE  DB2-CONTA             TO  WRK-480-RET-CONTA       
                MOVE  DB2-CARTEIRA          TO  WRK-480-RET-CCART
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 056                                                     
                MOVE  DB2-NUMERO-AVAL       TO  WRK-480-RET-CCGC-CPF    
                MOVE  DB2-FILIAL-AVAL       TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  DB2-CTR-AVAL          TO  WRK-480-RET-CCTRL-CGC   
                MOVE  DB2-AGENCIA           TO  WRK-480-RET-AGENCIA     
                MOVE  DB2-CONTA             TO  WRK-480-RET-CONTA       
                MOVE  DB2-COD-NATUREZA-OPER TO  WRK-NATU-AUX            
                MOVE  WRK-NATU-AUX-RR       TO  WRK-480-RET-NATUREZA    
           WHEN 057                                                     
                MOVE  DB2-NUMERO-AVAL       TO  WRK-480-RET-CCGC-CPF    
                MOVE  DB2-FILIAL-AVAL       TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  DB2-CTR-AVAL          TO  WRK-480-RET-CCTRL-CGC   
                MOVE  DB2-AGENCIA           TO  WRK-480-RET-AGENCIA     
                MOVE  DB2-CONTA             TO  WRK-480-RET-CONTA       
                MOVE  DB2-COD-NATUREZA-OPER TO  WRK-NATU-AUX            
                MOVE  WRK-NATU-AUX-RR       TO  WRK-480-RET-NATUREZA    
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 058                                                     
                MOVE  DB2-NUMERO-AVAL       TO  WRK-480-RET-CCGC-CPF    
                MOVE  DB2-FILIAL-AVAL       TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  DB2-CTR-AVAL          TO  WRK-480-RET-CCTRL-CGC   
                MOVE  DB2-AGENCIA           TO  WRK-480-RET-AGENCIA     
                MOVE  DB2-CONTA             TO  WRK-480-RET-CONTA       
                MOVE  DB2-COD-NATUREZA-OPER TO  WRK-NATU-AUX            
                MOVE  WRK-NATU-AUX-RR       TO  WRK-480-RET-NATUREZA    
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
           WHEN 059                                                     
                MOVE  DB2-NUMERO-AVAL       TO  WRK-480-RET-CCGC-CPF    
                MOVE  DB2-FILIAL-AVAL       TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  DB2-CTR-AVAL          TO  WRK-480-RET-CCTRL-CGC   
                MOVE  DB2-AGENCIA           TO  WRK-480-RET-AGENCIA     
                MOVE  DB2-CONTA             TO  WRK-480-RET-CONTA       
                MOVE  DB2-COD-NATUREZA-OPER TO  WRK-NATU-AUX            
                MOVE  WRK-NATU-AUX-RR       TO  WRK-480-RET-NATUREZA    
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 060                                                     
                MOVE  DB2-NUMERO-AVAL       TO  WRK-480-RET-CCGC-CPF    
                MOVE  DB2-FILIAL-AVAL       TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  DB2-CTR-AVAL          TO  WRK-480-RET-CCTRL-CGC   
                MOVE  DB2-AGENCIA           TO  WRK-480-RET-AGENCIA     
                MOVE  DB2-CONTA             TO  WRK-480-RET-CONTA       
                MOVE  DB2-COD-NATUREZA-OPER TO  WRK-NATU-AUX            
                MOVE  WRK-NATU-AUX-RR       TO  WRK-480-RET-NATUREZA    
                MOVE  DB2-CARTEIRA          TO  WRK-480-RET-CCART
           WHEN 061                                                     
                MOVE  DB2-NUMERO-AVAL       TO  WRK-480-RET-CCGC-CPF    
                MOVE  DB2-FILIAL-AVAL       TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  DB2-CTR-AVAL          TO  WRK-480-RET-CCTRL-CGC   
                MOVE  DB2-AGENCIA           TO  WRK-480-RET-AGENCIA     
                MOVE  DB2-CONTA             TO  WRK-480-RET-CONTA       
                MOVE  DB2-COD-NATUREZA-OPER TO  WRK-NATU-AUX            
                MOVE  WRK-NATU-AUX-RR       TO  WRK-480-RET-NATUREZA    
                MOVE  DB2-CARTEIRA          TO  WRK-480-RET-CCART
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 062                                                     
                MOVE  DB2-NUMERO-AVAL       TO  WRK-480-RET-CCGC-CPF    
                MOVE  DB2-FILIAL-AVAL       TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  DB2-CTR-AVAL          TO  WRK-480-RET-CCTRL-CGC   
                MOVE  DB2-AGENCIA           TO  WRK-480-RET-AGENCIA     
                MOVE  DB2-CONTA             TO  WRK-480-RET-CONTA       
                MOVE  DB2-COD-NATUREZA-OPER TO  WRK-NATU-AUX            
                MOVE  WRK-NATU-AUX-RR       TO  WRK-480-RET-NATUREZA    
                MOVE  DB2-CARTEIRA          TO  WRK-480-RET-CCART
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
           WHEN 063                                                     
                MOVE  DB2-NUMERO-AVAL       TO  WRK-480-RET-CCGC-CPF    
                MOVE  DB2-FILIAL-AVAL       TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  DB2-CTR-AVAL          TO  WRK-480-RET-CCTRL-CGC   
                MOVE  DB2-AGENCIA           TO  WRK-480-RET-AGENCIA     
                MOVE  DB2-CONTA             TO  WRK-480-RET-CONTA       
                MOVE  DB2-COD-NATUREZA-OPER TO  WRK-NATU-AUX            
                MOVE  WRK-NATU-AUX-RR       TO  WRK-480-RET-NATUREZA    
                MOVE  DB2-CARTEIRA          TO  WRK-480-RET-CCART
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
                                                                        
           END-EVALUATE.                                                
                                                                        
           CALL   WRK-MODULO       USING        WRK-480-CODIGO3         
                                                WRK-480-NOME-TABELA     
                                                WRK-480-RETORNO-CHAVE   
                                                WRK-480-RETORNO.        
                                                                        
           IF   RETURN-CODE        EQUAL        ZEROS                   
                PERFORM 1003-GRAVA-BLQREST-AVA                          
                PERFORM 5001-GERAR-RELATORIO-AVA                        
                MOVE    099                 TO  WRK-PESQ-AVA            
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       3151-99-FIM. EXIT.                                               
      *---------------------------------------------------------------* 
                                                                        
      *----------------------------------------------------------------*
       5000-GERAR-RELATORIO-DEV        SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE 'B A N C O   B R A D E S C O   S / A.    '  TO          
                                              CB1A-NOME-BCO.            
                                                                        
           IF ACU-LINHAS-DEV           GREATER 63                       
              MOVE 6                   TO ACU-LINHAS-DEV                
              ADD  1                   TO ACU-PAGINAS-DEV               
              MOVE ACU-PAGINAS-DEV     TO CB1A-PAGINAS                  
              WRITE FD-RELATO1         FROM CABEC1A                     
              MOVE WRK-GRAVACAO        TO WRK-OPERACAO                  
              PERFORM 1500-TESTAR-FS-RELATO1                            
              MOVE 'DEV COM RESTRICAO NA TAB LPCLB037 - AVISO' TO       
                                            CB2A-DESCRICAO              
              WRITE FD-RELATO1         FROM CABEC2A                     
              PERFORM 1500-TESTAR-FS-RELATO1                            
              WRITE FD-RELATO1         FROM CABEC3A                     
              PERFORM 1500-TESTAR-FS-RELATO1                            
              WRITE FD-RELATO1         FROM CABEC4A                     
              PERFORM 1500-TESTAR-FS-RELATO1                            
              WRITE FD-RELATO1         FROM CABEC5A                     
              PERFORM 1500-TESTAR-FS-RELATO1                            
           END-IF.                                                      
                                                                        
           MOVE DB2-NUMERO-DEV          TO LD1A-CGC.                    
           MOVE DB2-FILIAL              TO WRK-FILIAL-AUX.              
           MOVE WRK-FILIAL-AUX-RR       TO LD1A-FIL.                    
           MOVE DB2-CTR-DEV             TO LD1A-CTR.                    
                                                                        
           MOVE DB2-AGENCIA             TO WRK-AGENCIA-AUX.             
           MOVE WRK-AGENCIA-AUX-RR      TO LD1A-AGENCIA.                
           MOVE DB2-CONTA               TO LD1A-CONTA.                  
           MOVE DB2-CONTRATO            TO LD1A-CONTRATO.               
           MOVE DB2-COD-NATUREZA-OPER   TO LD1A-NATUREZA.               
           MOVE DB2-CARTEIRA            TO LD1A-CARTEIRA.               
           MOVE DB2-VCTO                TO WRK-DATA-AUX-X10.            
           MOVE WRK-DIA                 TO WRK-DT-DD.                   
           MOVE WRK-MES                 TO WRK-DT-MM.                   
           MOVE WRK-ANO                 TO WRK-DT-AAAA.                 
           MOVE WRK-DATA-AUX            TO LD1A-DATA-VCTO.              
                                                                        
           MOVE DB2-NOME-DEVEDOR        TO LD1A-NOME-DEVEDOR.           
           MOVE DB2-ID                  TO LD1A-BLOQ.                   
           MOVE FD-DB2-UF               TO LD1A-UF.                     
                                                                        
           MOVE WRK-MENSAG (WRK-PESQ-DEV)   TO LD1A-TIPO-ERRO.          
                                                                        
           WRITE FD-RELATO1            FROM LINDET1A.                   
                                                                        
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.                 
           PERFORM 1500-TESTAR-FS-RELATO1.                              
                                                                        
           ADD 1                       TO ACU-LINHAS-DEV.               
                                                                        
      *----------------------------------------------------------------*
       5000-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       5001-GERAR-RELATORIO-AVA        SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE 'B A N C O   B R A D E S C O   S / A.    '  TO          
                                              CB1A-NOME-BCO.            
                                                                        
           IF ACU-LINHAS-AVA           GREATER 63                       
              MOVE 6                   TO ACU-LINHAS-AVA                
              ADD  1                   TO ACU-PAGINAS-AVA               
              MOVE ACU-PAGINAS-AVA     TO CB1A-PAGINAS                  
              WRITE FD-RELATO2         FROM CABEC1A                     
              MOVE WRK-GRAVACAO        TO WRK-OPERACAO                  
              PERFORM 1501-TESTAR-FS-RELATO2                            
              MOVE 'AVA COM RESTRICAO NA TAB LPCLB037 - AVISO' TO       
                                            CB2A-DESCRICAO              
              WRITE FD-RELATO2         FROM CABEC2A                     
              PERFORM 1501-TESTAR-FS-RELATO2                            
              WRITE FD-RELATO2         FROM CABEC3A1                    
              PERFORM 1501-TESTAR-FS-RELATO2                            
              WRITE FD-RELATO2         FROM CABEC4A                     
              PERFORM 1501-TESTAR-FS-RELATO2                            
              WRITE FD-RELATO2         FROM CABEC5A                     
              PERFORM 1501-TESTAR-FS-RELATO2                            
           END-IF.                                                      
                                                                        
           MOVE DB2-NUMERO-AVAL         TO LD1A-CGC.                    
           MOVE DB2-FILIAL-AVAL         TO WRK-FILIAL-AUX.              
           MOVE WRK-FILIAL-AUX-RR       TO LD1A-FIL.                    
           MOVE DB2-CTR-AVAL            TO LD1A-CTR.                    
                                                                        
           MOVE DB2-AGENCIA             TO WRK-AGENCIA-AUX.             
           MOVE WRK-AGENCIA-AUX-RR      TO LD1A-AGENCIA.                
           MOVE DB2-CONTA               TO LD1A-CONTA.                  
           MOVE DB2-CONTRATO            TO LD1A-CONTRATO.               
           MOVE DB2-COD-NATUREZA-OPER   TO LD1A-NATUREZA.               
           MOVE DB2-CARTEIRA            TO LD1A-CARTEIRA.               
           MOVE DB2-VCTO                TO WRK-DATA-AUX-X10.            
           MOVE WRK-DIA                 TO WRK-DT-DD.                   
           MOVE WRK-MES                 TO WRK-DT-MM.                   
           MOVE WRK-ANO                 TO WRK-DT-AAAA.                 
           MOVE WRK-DATA-AUX            TO LD1A-DATA-VCTO.              
                                                                        
           MOVE DB2-NOME-AVALISTA       TO LD1A-NOME-DEVEDOR.           
           MOVE DB2-ID                  TO LD1A-BLOQ.                   
           MOVE FD-DB2-UF               TO LD1A-UF.                     
                                                                        
           MOVE WRK-MENSAG (WRK-PESQ-AVA)   TO LD1A-TIPO-ERRO.          
                                                                        
           WRITE FD-RELATO2            FROM LINDET1A.                   
                                                                        
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.                 
           PERFORM 1501-TESTAR-FS-RELATO2.                              
                                                                        
           ADD 1                       TO ACU-LINHAS-AVA.               
                                                                        
      *----------------------------------------------------------------*
       5001-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *---------------------------------------------------------------* 
       9000-FINALIZAR   SECTION.                                        
      *---------------------------------------------------------------* 
                                                                        
           CLOSE  TABELA37                                              
                  ARQDB22S                                              
                  BLQREST                                               
                  RELATO1                                               
                  RELATO2.                                              
                                                                        
           MOVE    WRK-FECHAMENTO    TO     WRK-OPERACAO.               
                                                                        
           PERFORM 1000-TESTAR-FILE-STATUS.                             
                                                                        
           MOVE    ZEROS             TO     RETURN-CODE.                
                                                                        
           STOP RUN.                                                    
                                                                        
      *---------------------------------------------------------------* 
       9000-99-FIM. EXIT.                                               
      *---------------------------------------------------------------* 
                                                                        
      *----------------------------------------------------------------*
       9999-ROTINA-ERRO SECTION.                                        
      *----------------------------------------------------------------*
                                                                        
           MOVE   'CLLP2601'           TO  ERR-PGM.                     
                                                                        
           CALL   'POOL7100'        USING  WRK-BATCH                    
                                           ERRO-AREA.                   
                                                                        
           GOBACK.                                                      
                                                                        
      *----------------------------------------------------------------*
       9999-99-FIM. EXIT.                                               
      *----------------------------------------------------------------*

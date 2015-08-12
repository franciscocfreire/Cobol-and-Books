      *================================================================*
       IDENTIFICATION                  DIVISION.                        
      *================================================================*
                                                                        
       PROGRAM-ID. PEAT1064.                                            
       AUTHOR.     CAIQUE.                                              
                                                                        
      *================================================================*
      *               B R Q   -   I T   S E R V I C E S                *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA     : PEAT1064                                     *
      *    PROGRAMADOR  : CAIQUE           - BRQ IT SERVICES.          *
      *    ANALISTA     : FERNANDA CARUSO  - BRQ IT SERVICES.          *
      *    FACILITADOR  : OSWALDO MATTOS   - GRUPO 45.                 *
      *    DATA         : 17/07/2012                                   *
      *    OBJETIVO     : FORMATAR NO ARQUIVO DE MOVIMENTO A CONTA     *
      *                   3 MILHOES (RAZAO 10.71) DE ACORDO COM A      *
      *                   DISPONIBILIDADE DE SALDO.                    *
      *                                                                *
      *                           ARQUIVOS                             *
      *           +---------------------------------------+            *
      *           |  DDNAME  |   I/O   |   BOOK   | LRECL |            *
      *           |----------|---------|----------|-------|            *
      *           | MVTODIAE | INPUT   | I#POUPN0 |  102  |            *
      *           | ARQSALDO | INPUT   | I#POUPSL |  050  |            *
      *           | MVTODIAS | OUTPUT  | I#POUPN0 |  102  |            *
      *           +---------------------------------------+            *
      *                                                                *
      *    MODULOS :                                                   *
      *                                                                *
      *    -> BRAD7100 - TRATAMENTO DE ERROS                           *
      *    -> BRAD0431 - CALCULO DO DIGITO DA CONTA                    *
      *    -> POUPXXXX - VERIFICA SALDO PARA ACERTO                    *
      *                                                                *
      *----------------------------------------------------------------*
                                                                        
      *================================================================*
       ENVIRONMENT                     DIVISION.                        
      *================================================================*
                                                                        
      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.                         
      *----------------------------------------------------------------*
                                                                        
       SPECIAL-NAMES.                                                   
           DECIMAL-POINT               IS COMMA.                        
                                                                        
      *----------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.                         
      *----------------------------------------------------------------*
                                                                        
       FILE-CONTROL.                                                    
                                                                        
           SELECT MVTODIAE ASSIGN      TO MVTODIAE                      
                      FILE STATUS      IS WRK-FS-MVTODIAE.              
                                                                        
           SELECT ARQSALDO ASSIGN      TO ARQSALDO                      
                      FILE STATUS      IS WRK-FS-ARQSALDO.              
                                                                        
           SELECT MVTODIAS ASSIGN      TO MVTODIAS                      
                      FILE STATUS      IS WRK-FS-MVTODIAS.              
                                                                        
      *================================================================*
       DATA                            DIVISION.                        
      *================================================================*
                                                                        
      *----------------------------------------------------------------*
       FILE                            SECTION.                         
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * INPUT : MOVIMENTO DIARIO.                                      *
      *                                                                *
      *                 ORG. SEQUENCIAL  -  LRECL 102                  *
      *----------------------------------------------------------------*
                                                                        
       FD  MVTODIAE                                                     
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  FD-MVTODIAE                 PIC  X(102).                     
                                                                        
      *----------------------------------------------------------------*
      * INPUT : SALDO DAS CONTAS DE PARA OBTIDO NA TABELA POUPV005 E   *
      *                                                   POUPV085     *
      *                 ORG. SEQUENCIAL  -  LRECL 050                  *
      *----------------------------------------------------------------*
                                                                        
       FD  ARQSALDO                                                     
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  FD-ARQSALDO                 PIC  X(050).                     
                                                                        
      *----------------------------------------------------------------*
      * OUTPUT : MOVIMENTO DIARIO ATUALIZADO.                          *
      *                                                                *
      *                 ORG. SEQUENCIAL  -  LRECL 102                  *
      *----------------------------------------------------------------*
                                                                        
       FD  MVTODIAS                                                     
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  FD-MVTODIAS                 PIC  X(102).                     
                                                                        
      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.                         
      *----------------------------------------------------------------*
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER PIC X(34) VALUE  '** INICIO DA WORKING PEAT1064 **  '.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER PIC X(34) VALUE  '** FILE-STATUS DOS ARQUIVOS **    '.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
       01  WRK-FS-MVTODIAE             PIC  X(002)  VALUE SPACES.       
       01  WRK-FS-ARQSALDO             PIC  X(002)  VALUE SPACES.       
       01  WRK-FS-MVTODIAS             PIC  X(002)  VALUE SPACES.       
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER PIC X(34) VALUE  '** BOOK DOS ARQUIVOS **           '.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
      *-->  MVTODIAE/MVTODIAS                                           
       01  WRK-MCTS-REG.                                                
           02  WRK-BANCO              PIC  9(003)  COMP-3 VALUE ZEROS.  
           02  WRK-CHAV.                                                
               05  WRK-AGENCIA        PIC  9(005)  COMP-3 VALUE ZEROS.  
               05  WRK-RAZAO.                                           
                   10  WRK-GRUPO      PIC  9(003)  COMP-3 VALUE ZEROS.  
                   10  WRK-SUBGP      PIC  9(003)  COMP-3 VALUE ZEROS.  
               05  WRK-CONTA          PIC  9(007)  COMP-3 VALUE ZEROS.  
               05  WRK-DIGITO         PIC  X(001)         VALUE SPACES. 
               05  WRK-DAT-MOV-AAAAMMDD                                 
                                       PIC  9(009)  COMP-3 VALUE ZEROS. 
           02  WRK-LANCAMENTO         PIC  9(005)  COMP-3 VALUE ZEROS.  
           02  WRK-NUMERO-DOC         PIC  9(007)  COMP-3 VALUE ZEROS.  
           02  WRK-DEB-CRED           PIC  X(001)         VALUE SPACES. 
           02  WRK-VALOR              PIC  9(013)V99                    
                                                    COMP-3 VALUE ZEROS. 
           02  WRK-DIA-VINCULO        PIC  9(003)  COMP-3 VALUE ZEROS.  
           02  WRK-CTRO-CUSTO         PIC  X(004)         VALUE SPACES. 
           02  WRK-SUBC-CRS           PIC  9(005)  COMP-3 VALUE ZEROS.  
           02  WRK-SERVICO            PIC  X(002)         VALUE SPACES. 
           02  WRK-TIPO-ENTRADA       PIC  X(001)         VALUE SPACES. 
           02  WRK-SN                 PIC  X(001)         VALUE SPACES. 
           02  WRK-ORIGEM             PIC  X(032)         VALUE SPACES. 
           02  WRK-DIA-LA             PIC  9(002)         VALUE ZEROS.  
           02  WRK-DATA-ORIGINAL      PIC  9(009)  COMP-3 VALUE ZEROS.  
           02  WRK-TRILHA.                                              
               05  WRK-PRODUTO        PIC  X(004)         VALUE SPACES. 
               05  WRK-OPERACAO       PIC  X(007)         VALUE SPACES. 
           02  WRK-NRO-SEQ-LA         PIC  X(003)         VALUE SPACES. 
           02  FILLER                  PIC  X(001)         VALUE SPACES.
                                                                        
      *-->  ARQSALDO                                                    
       COPY 'I#POUPSL'.                                                 
                                                                        
      *-->  BOOK USADO NA CHAMADA DO MODULO POUP1065                    
       COPY 'I#POUPM4'.                                                 
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER PIC X(34) VALUE  '** AREA DA BRAD7100 **            '.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
       COPY 'I#BRAD7C'.                                                 
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER PIC X(34) VALUE  '** AREA DA BRAD0431 **            '.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
       01  WRK-AREA-BRAD0431.                                           
           05  WRK-B0431-CONTA         PIC  9(007)         VALUE ZEROS. 
           05  WRK-B0431-DIGITO        PIC  X(001)         VALUE SPACES.
           05  WRK-B0431-TAMANHO       PIC  9(002)         VALUE ZEROS. 
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER PIC X(34) VALUE  '** ACUMULADORES **                '.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
       01  ACU-LIDOS-MVTODIAE          PIC  9(009)  COMP-3 VALUE ZEROS. 
       01  ACU-LIDOS-ARQSALDO          PIC  9(009)  COMP-3 VALUE ZEROS. 
       01  ACU-GRAVS-MVTODIAS          PIC  9(009)  COMP-3 VALUE ZEROS. 
       01  ACU-SLDO-INSUF-R1071        PIC  9(009)  COMP-3 VALUE ZEROS. 
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER PIC X(34) VALUE  '** AREA DE CHAVES **              '.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
       01  WRK-CHV-MOVTO.                                               
           05  WRK-CHV-MOV-AGE         PIC S9(005)         VALUE ZEROS. 
           05  WRK-CHV-MOV-CTA         PIC S9(007)         VALUE ZEROS. 
                                                                        
       01  WRK-CHV-ARQSALDO.                                            
           05  WRK-CHV-SLD-AGE         PIC S9(005)         VALUE ZEROS. 
           05  WRK-CHV-SLD-CTA         PIC S9(007)         VALUE ZEROS. 
                                                                        
       01  WRK-CHV-MOVTO-ANT           PIC  X(012)         VALUE SPACES.
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER PIC X(34) VALUE  '** VARIAVEIS AUXILIARES **        '.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
       01  WRK-SLD-CTA-ESTOQUE         PIC S9(013)V99                   
                                                    COMP-3 VALUE ZEROS. 
       01  WRK-SLD-CTA-MP              PIC S9(013)V99                   
                                                    COMP-3 VALUE ZEROS. 
       01  WRK-CREDT-DIA-MP            PIC S9(013)V99                   
                                                    COMP-3 VALUE ZEROS. 
       01  WRK-MOVTO-DEBITAR           PIC S9(013)V99                   
                                                    COMP-3 VALUE ZEROS. 
       01  WRK-VMOVTO                  PIC S9(013)V99                   
                                                    COMP-3 VALUE ZEROS. 
                                                                        
       01  WRK-DLCTO-CREDT-ANT         PIC  9(9)           VALUE ZEROS. 
                                                                        
       01  WRK-DLCTO-CREDT-DIA         PIC  9(9)           VALUE ZEROS. 
                                                                        
       01  WRK-AUX-TAM-07              PIC -9(7)           VALUE ZEROS. 
       01  FILLER REDEFINES WRK-AUX-TAM-07.                             
        03 FILLER                      PIC  X(1).                       
        03 WRK-AUX-TAM-07-R            PIC  9(7).                       
                                                                        
       01  WRK-AUX-TAM-05              PIC -9(5)           VALUE ZEROS. 
       01  FILLER REDEFINES WRK-AUX-TAM-05.                             
        03 FILLER                      PIC  X(1).                       
        03 WRK-AUX-TAM-05-R            PIC  9(5).                       
                                                                        
       01  WRK-AUX-TAM-15              PIC -9(13)V99       VALUE ZEROS. 
       01  FILLER REDEFINES WRK-AUX-TAM-15.                             
        03 FILLER                      PIC  X(1).                       
        03 WRK-AUX-TAM-15-R            PIC  9(13)V99.                   
                                                                        
       01  WRK-MODULO                  PIC  X(8)           VALUE SPACES.
                                                                        
      *                                                                 
      *--> "VARIAVEL PARA MASCARA DE DADOS"                             
      *                                                                 
                                                                        
       01  WRK-MASC-ACU                PIC  ZZZ.ZZZ.Z99  VALUE ZEROS.   
                                                                        
      *                                                                 
      *--> "LITERAIS UTILIZADAS PELO PROGRAMA"                          
      *                                                                 
                                                                        
       01  WRK-LIT-BATCH               PIC  X(008)  VALUE 'BATCH'.      
       01  WRK-LIT-PGM                 PIC  X(008)  VALUE 'PEAT1064'.   
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER PIC X(34) VALUE  '** MENSAGEM DE ERRO - BRAD7100 ** '.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
      *                                                                 
      *--> "MENSAGEM DE ERRO PARA ARQUIVOS"                             
      *                                                                 
                                                                        
       01  WRK-ERRO-ARQUIVO.                                            
                                                                        
           05  FILLER                  PIC X(008) VALUE '** ERRO '.     
                                                                        
           05  WRK-OPERACAO            PIC X(013) VALUE SPACES.         
             88  WRK-ABERTURA                     VALUE 'NA ABERTURA'.  
             88  WRK-LEITURA                      VALUE 'NA LEITURA'.   
             88  WRK-GRAVACAO                     VALUE 'NA GRAVACAO'.  
             88  WRK-FECHAMENTO                   VALUE 'NO FECHAMENTO'.
                                                                        
           05  FILLER                  PIC X(012) VALUE ' DO ARQUIVO '. 
                                                                        
           05  WRK-NOME-ARQUIVO        PIC X(008) VALUE SPACES.         
             88  WRK-MVTODIAE                     VALUE 'MVTODIAE'.     
             88  WRK-ARQSALDO                     VALUE 'ARQSALDO'.     
             88  WRK-MVTODIAS                     VALUE 'MVTODIAS'.     
                                                                        
           05  FILLER                  PIC X(017) VALUE                 
                                                  ' - FILE STATUS = '.  
                                                                        
           05  WRK-FILE-STATUS         PIC X(002) VALUE SPACES.         
                                                                        
           05  FILLER                  PIC X(003) VALUE ' **'.          
                                                                        
      *                                                                 
      *--> "MENSAGEM DE ERRO PARA MODULOS"                              
      *                                                                 
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER PIC X(34) VALUE  '** FIM DA WORKING PEAT1064 **     '.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
      *================================================================*
       PROCEDURE                       DIVISION.                        
      *================================================================*
                                                                        
      *----------------------------------------------------------------*
      * SECAO PRINCIPAL DE CONTROLE DO FLUXO DO PROGRAMA.              *
      *----------------------------------------------------------------*
       0000-PRINCIPAL                  SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           PERFORM 1000-INICIALIZAR                                     
                                                                        
           PERFORM 3000-PROCESSAR      UNTIL                            
                   WRK-FS-MVTODIAE     EQUAL  '10'                      
                                                                        
           PERFORM 9000-FINALIZAR.                                      
                                                                        
      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * PROCEDIMENTOS INICIAIS.                                        *
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.                         
      *----------------------------------------------------------------*
      *                                                                 
      *--> "ABERTURA DOS ARQUIVOS"                                      
      *                                                                 
           SET  WRK-ABERTURA           TO  TRUE                         
                                                                        
           OPEN INPUT  MVTODIAE                                         
                       ARQSALDO                                         
                OUTPUT MVTODIAS                                         
                                                                        
           PERFORM 1100-TESTAR-FILE-STATUS                              
      *                                                                 
      *--> "PRIMEIRA LEITURA"                                           
      *                                                                 
           PERFORM 2000-VERIFICAR-VAZIO.                                
                                                                        
      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * FAZER TESTE DE FILE-STATUS PARA OS ARQUIVOS.                   *
      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS         SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           PERFORM 1110-TESTAR-FS-MVTODIAE                              
           PERFORM 1120-TESTAR-FS-ARQSALDO                              
           PERFORM 1130-TESTAR-FS-MVTODIAS.                             
                                                                        
      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * TESTAR FILE-STATUS DO ARQUIVO DE ENTRADA MVTODIAE.             *
      *----------------------------------------------------------------*
       1110-TESTAR-FS-MVTODIAE         SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           IF WRK-FS-MVTODIAE          NOT EQUAL '00'                   
              SET   WRK-MVTODIAE       TO  TRUE                         
              MOVE  WRK-FS-MVTODIAE    TO  WRK-FILE-STATUS              
              MOVE  WRK-ERRO-ARQUIVO   TO  ERR-TEXTO                    
              MOVE  'APL'              TO  ERR-TIPO-ACESSO              
              PERFORM 9999-FINALIZAR-ERRO                               
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * TESTAR FILE-STATUS DO ARQUIVO DE ENTRADA ARQSALDO.             *
      *----------------------------------------------------------------*
       1120-TESTAR-FS-ARQSALDO         SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           IF WRK-FS-ARQSALDO          NOT EQUAL '00'                   
              SET   WRK-ARQSALDO       TO  TRUE                         
              MOVE  WRK-FS-ARQSALDO    TO  WRK-FILE-STATUS              
              MOVE  WRK-ERRO-ARQUIVO   TO  ERR-TEXTO                    
              MOVE  'APL'              TO  ERR-TIPO-ACESSO              
              PERFORM 9999-FINALIZAR-ERRO                               
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * TESTAR FILE-STATUS DO ARQUIVO DE SAIDA MVTODIAS.               *
      *----------------------------------------------------------------*
       1130-TESTAR-FS-MVTODIAS         SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           IF WRK-FS-MVTODIAS          NOT EQUAL '00'                   
              SET   WRK-MVTODIAS       TO  TRUE                         
              MOVE  WRK-FS-MVTODIAS    TO  WRK-FILE-STATUS              
              MOVE  WRK-ERRO-ARQUIVO   TO  ERR-TEXTO                    
              MOVE  'APL'              TO  ERR-TIPO-ACESSO              
              PERFORM 9999-FINALIZAR-ERRO                               
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       1130-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * VERIFICAR SE ARQUIVO DE ENTRADA ESTA VAZIO.                    *
      *----------------------------------------------------------------*
       2000-VERIFICAR-VAZIO            SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           PERFORM 2100-LER-MVTODIAE                                    
                                                                        
           IF WRK-FS-MVTODIAE          EQUAL '10'                       
              DISPLAY '***=========== PEAT1064 ===========***'          
              DISPLAY '*                                    *'          
              DISPLAY '*     ARQUIVO MVTODIAE VAZIO !!!     *'          
              DISPLAY '*                                    *'          
              DISPLAY '***================================***'          
           END-IF                                                       
                                                                        
           PERFORM 2200-LER-ARQSALDO                                    
                                                                        
           IF WRK-FS-ARQSALDO          EQUAL '10'                       
              DISPLAY '***=========== PEAT1064 ===========***'          
              DISPLAY '*                                    *'          
              DISPLAY '*     ARQUIVO ARQSALDO VAZIO !!!     *'          
              DISPLAY '*                                    *'          
              DISPLAY '***================================***'          
              MOVE  'ARQUIVO ARQSALDO VAZIO' TO  ERR-TEXTO              
              MOVE  'APL'              TO  ERR-TIPO-ACESSO              
              PERFORM  9999-FINALIZAR-ERRO                              
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * FAZER A LEITURA DO ARQUIVO MVTODIAE.                           *
      *----------------------------------------------------------------*
       2100-LER-MVTODIAE               SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           SET  WRK-LEITURA            TO   TRUE                        
           READ MVTODIAE               INTO WRK-MCTS-REG                
                                                                        
           IF WRK-FS-MVTODIAE          NOT EQUAL '10'                   
              PERFORM 1110-TESTAR-FS-MVTODIAE                           
              ADD   1                  TO  ACU-LIDOS-MVTODIAE           
              MOVE  WRK-AGENCIA       TO  WRK-CHV-MOV-AGE               
              MOVE  WRK-CONTA         TO  WRK-CHV-MOV-CTA               
           ELSE                                                         
              MOVE  HIGH-VALUES        TO  WRK-CHV-MOVTO                
           END-IF.                                                      
                                                                        
           IF  (WRK-CHV-ARQSALDO       EQUAL   HIGH-VALUES) AND         
               (WRK-FS-MVTODIAE    NOT EQUAL   '10') OR                 
               (WRK-GRUPO          NOT EQUAL 10) OR                     
               (WRK-SUBGP          NOT EQUAL 51)                        
               PERFORM 4000-GRAVAR-MVTODIAS                             
               GO TO   2100-LER-MVTODIAE                                
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
                                                                        
      *----------------------------------------------------------------*
      * FAZER A LEITURA DO ARQUIVO ARQSALDO.                           *
      *----------------------------------------------------------------*
       2200-LER-ARQSALDO               SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           SET  WRK-LEITURA            TO   TRUE                        
           READ ARQSALDO               INTO REG-POUPSL                  
                                                                        
           IF WRK-FS-ARQSALDO          NOT EQUAL '10'                   
              PERFORM 1120-TESTAR-FS-ARQSALDO                           
              ADD   1                  TO  ACU-LIDOS-ARQSALDO           
              MOVE  POUPSL-AGE-DE      TO  WRK-CHV-SLD-AGE              
              MOVE  POUPSL-CTA-DE      TO  WRK-CHV-SLD-CTA              
           ELSE                                                         
              MOVE  HIGH-VALUES        TO  WRK-CHV-ARQSALDO             
              MOVE  ZEROS              TO  POUPSL-SLD-DE                
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * PROCESSAMENTO PRINCIPAL.                                       *
      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.                         
      *----------------------------------------------------------------*
                                                                        
      *--> "TRATAR MOVIMENTO APOS NOVA REGRA DA POUPANCA"               
                                                                        
           MOVE POUPSL-SLD-DE          TO WRK-SLD-CTA-ESTOQUE           
           MOVE POUPSL-SLD-PARA        TO WRK-SLD-CTA-MP                
           MOVE ZEROS                  TO WRK-CREDT-DIA-MP              
           MOVE WRK-CHV-MOVTO          TO WRK-CHV-MOVTO-ANT             
                                                                        
           IF WRK-CHV-MOVTO              EQUAL WRK-CHV-ARQSALDO         
              PERFORM 3100-TRATAR-MOVTO-EQ-SALDO                        
                UNTIL WRK-CHV-MOVTO  NOT EQUAL WRK-CHV-MOVTO-ANT        
           ELSE                                                         
              IF WRK-CHV-MOVTO         GREATER WRK-CHV-ARQSALDO         
                 PERFORM 2200-LER-ARQSALDO                              
              ELSE                                                      
                 PERFORM 3700-MOVTODIAE-LT-ARQSALDO                     
              END-IF                                                    
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * PROCESSAR MOVIMENTOS >= 00/00/2012 (NOVA REGRA POUP) QUE       *
      * POSSUEM CONTA ABERTA NO RAZAO 10.71.                           *
      *----------------------------------------------------------------*
       3100-TRATAR-MOVTO-EQ-SALDO      SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           PERFORM 3800-IDENTIFICA-LANCAMENTO                           
                                                                        
           PERFORM 3200-VERIFICAR-ACERTO-DCO                            
                                                                        
           PERFORM 2100-LER-MVTODIAE.                                   
                                                                        
      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
      * VERIFICAR QUAL CONTAR USAR, ESTOQUE OU MP                      *
      *----------------------------------------------------------------*
       3200-VERIFICAR-ACERTO-DCO       SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           IF POUPM4-COD-RETORNO   NOT EQUAL   'N'                      
            PERFORM 3300-TRATAR-ACERTO-DCO                              
           ELSE                                                         
            IF WRK-DEB-CRED       NOT EQUAL  '1'                        
               PERFORM 3400-TRATAR-CREDITO                              
            ELSE                                                        
               PERFORM 3500-TRATAR-DEBITO                               
            END-IF                                                      
           END-IF.                                                      
                                                                        
       3200-FIM.                       EXIT.                            
      *----------------------------------------------------------------*
      * PROCESSAR MOVIMENTOS >= 00/00/2012 (NOVA REGRA POUP) QUE NAO   *
      * POSSUEM CONTA ABERTA NO RAZAO 10.71.                           *
      *----------------------------------------------------------------*
       3300-TRATAR-ACERTO-DCO          SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           IF POUPM4-COD-RETORNO       EQUAL   'M'                      
            MOVE POUPSL-AGE-PARA       TO  WRK-AUX-TAM-05               
            MOVE WRK-AUX-TAM-05-R      TO  WRK-AGENCIA                  
            MOVE POUPSL-CTA-PARA       TO  WRK-AUX-TAM-07               
            MOVE WRK-AUX-TAM-07-R      TO  WRK-CONTA                    
            PERFORM 3600-CALCULAR-DIGITO                                
            MOVE WRK-B0431-DIGITO      TO  WRK-DIGITO                   
            PERFORM 4000-GRAVAR-MVTODIAS                                
            MOVE  WRK-VALOR           TO  WRK-VMOVTO                    
            IF WRK-DEB-CRED           EQUAL  '2'                        
               COMPUTE WRK-SLD-CTA-MP  =   WRK-SLD-CTA-MP + WRK-VMOVTO  
            ELSE                                                        
               COMPUTE WRK-SLD-CTA-MP  =   WRK-SLD-CTA-MP - WRK-VMOVTO  
            END-IF                                                      
           ELSE                                                         
            PERFORM 4000-GRAVAR-MVTODIAS                                
            IF WRK-DEB-CRED           EQUAL  '2'                        
               COMPUTE WRK-SLD-CTA-ESTOQUE =                            
                                       WRK-SLD-CTA-ESTOQUE + WRK-VMOVTO 
            ELSE                                                        
               COMPUTE WRK-SLD-CTA-ESTOQUE =                            
                                       WRK-SLD-CTA-ESTOQUE - WRK-VMOVTO 
            END-IF                                                      
           END-IF.                                                      
                                                                        
       3300-FIM.                       EXIT.                            
      *----------------------------------------------------------------*
      * PROCESSAR MOVIMENTOS >= 00/00/2012 (NOVA REGRA POUP) QUE NAO   *
      * POSSUEM CONTA ABERTA NO RAZAO 10.71.                           *
      *----------------------------------------------------------------*
       3400-TRATAR-CREDITO             SECTION.                         
                                                                        
           MOVE POUPSL-AGE-PARA        TO  WRK-AUX-TAM-05               
           MOVE WRK-AUX-TAM-05-R       TO  WRK-AGENCIA                  
           MOVE POUPSL-CTA-PARA        TO  WRK-AUX-TAM-07               
           MOVE WRK-AUX-TAM-07-R       TO  WRK-CONTA                    
           PERFORM 3600-CALCULAR-DIGITO                                 
           MOVE WRK-B0431-DIGITO       TO  WRK-DIGITO                   
           PERFORM 4000-GRAVAR-MVTODIAS                                 
           MOVE WRK-VALOR             TO  WRK-VMOVTO                    
           COMPUTE WRK-SLD-CTA-MP      =   WRK-SLD-CTA-MP + WRK-VMOVTO  
           MOVE WRK-DAT-MOV-AAAAMMDD  TO  WRK-DLCTO-CREDT-DIA           
           IF   WRK-DAT-MOV-AAAAMMDD  NOT EQUAL   WRK-DLCTO-CREDT-ANT   
                MOVE WRK-VMOVTO        TO  WRK-CREDT-DIA-MP             
                MOVE WRK-DAT-MOV-AAAAMMDD TO  WRK-DLCTO-CREDT-ANT       
           ELSE                                                         
                COMPUTE WRK-CREDT-DIA-MP = WRK-CREDT-DIA-MP + WRK-VMOVTO
           END-IF.                                                      
                                                                        
       3400-FIM.                       EXIT.                            
      *----------------------------------------------------------------*
      * PROCESSAR MOVIMENTOS >= 00/00/2012 (NOVA REGRA POUP) QUE NAO   *
      * POSSUEM CONTA ABERTA NO RAZAO 10.71.                           *
      *----------------------------------------------------------------*
       3500-TRATAR-DEBITO              SECTION.                         
                                                                        
           MOVE    WRK-VALOR          TO  WRK-MOVTO-DEBITAR.            
                                                                        
           IF  (WRK-CREDT-DIA-MP       GREATER ZEROS) AND               
               (WRK-DAT-MOV-AAAAMMDD   EQUAL   WRK-DLCTO-CREDT-DIA)     
               PERFORM 3510-DEBITAR-CREDITO-DIA                         
               IF  WRK-MOVTO-DEBITAR   EQUAL   ZEROS                    
                   CONTINUE                                             
               ELSE                                                     
                   PERFORM 3515-DEBITA-CONTAS                           
               END-IF                                                   
           ELSE                                                         
               PERFORM 3515-DEBITA-CONTAS                               
           END-IF.                                                      
                                                                        
       3500-FIM.                       EXIT.                            
      *----------------------------------------------------------------*
      * TRATA DEBITO E CREDITO DO DIA                                  *
      *----------------------------------------------------------------*
       3510-DEBITAR-CREDITO-DIA        SECTION.                         
                                                                        
           MOVE POUPSL-AGE-PARA        TO  WRK-AUX-TAM-05               
           MOVE WRK-AUX-TAM-05-R       TO  WRK-AGENCIA                  
           MOVE POUPSL-CTA-PARA        TO  WRK-AUX-TAM-07               
           MOVE WRK-AUX-TAM-07-R       TO  WRK-CONTA                    
           PERFORM 3600-CALCULAR-DIGITO                                 
           MOVE WRK-B0431-DIGITO       TO  WRK-DIGITO                   
                                                                        
           IF  WRK-MOVTO-DEBITAR       GREATER WRK-CREDT-DIA-MP         
               MOVE    WRK-CREDT-DIA-MP TO WRK-AUX-TAM-15               
               MOVE    WRK-AUX-TAM-15-R TO WRK-VALOR                    
               PERFORM 4000-GRAVAR-MVTODIAS                             
               MOVE    WRK-VALOR      TO  WRK-VMOVTO                    
               COMPUTE WRK-MOVTO-DEBITAR=  WRK-MOVTO-DEBITAR +WRK-VMOVTO
               COMPUTE WRK-SLD-CTA-MP   =  WRK-SLD-CTA-MP    +WRK-VMOVTO
               MOVE    ZEROS           TO  WRK-CREDT-DIA-MP             
           ELSE                                                         
               PERFORM 4000-GRAVAR-MVTODIAS                             
               MOVE    WRK-VALOR      TO  WRK-VMOVTO                    
               COMPUTE WRK-CREDT-DIA-MP =  WRK-CREDT-DIA-MP + WRK-VMOVTO
               COMPUTE WRK-SLD-CTA-MP   =  WRK-SLD-CTA-MP   + WRK-VMOVTO
               MOVE    ZEROS           TO  WRK-MOVTO-DEBITAR            
           END-IF.                                                      
                                                                        
       3510-FIM.                       EXIT.                            
      *----------------------------------------------------------------*
      * TRATAMENTO A                                                   *
      *----------------------------------------------------------------*
       3515-DEBITA-CONTAS                          SECTION.             
                                                                        
           IF  POUPSL-OPCAO-CLIE       EQUAL   'E'                      
               PERFORM 3520-DEBITAR-CONTA-ESTOQUE                       
           ELSE                                                         
               PERFORM 3530-DEBITAR-CONTA-MP                            
           END-IF.                                                      
                                                                        
       3515-FIM.                       EXIT.                            
      *----------------------------------------------------------------*
      * DEBITAR DA CONTA ESTOQUE                                       *
      *----------------------------------------------------------------*
       3520-DEBITAR-CONTA-ESTOQUE      SECTION.                         
                                                                        
           IF  WRK-MOVTO-DEBITAR       GREATER WRK-SLD-CTA-ESTOQUE      
            IF WRK-SLD-CTA-ESTOQUE  NOT EQUAL ZEROS                     
               MOVE    WRK-SLD-CTA-ESTOQUE TO WRK-AUX-TAM-15            
               MOVE    WRK-AUX-TAM-15-R TO WRK-VALOR                    
               PERFORM 4000-GRAVAR-MVTODIAS                             
            ELSE                                                        
               MOVE ZEROS              TO  WRK-VALOR                    
            END-IF                                                      
            MOVE    ZEROS              TO  WRK-SLD-CTA-ESTOQUE          
            MOVE    WRK-VALOR          TO  WRK-VMOVTO                   
            COMPUTE WRK-MOVTO-DEBITAR =   WRK-MOVTO-DEBITAR - WRK-VMOVTO
            MOVE    WRK-MOVTO-DEBITAR  TO  WRK-AUX-TAM-15               
            MOVE    WRK-AUX-TAM-15-R   TO  WRK-VALOR                    
            MOVE    POUPSL-AGE-PARA    TO  WRK-AUX-TAM-05               
            MOVE    WRK-AUX-TAM-05-R   TO  WRK-AGENCIA                  
            MOVE    POUPSL-CTA-PARA    TO  WRK-AUX-TAM-07               
            MOVE    WRK-AUX-TAM-07-R   TO  WRK-CONTA                    
            PERFORM 3600-CALCULAR-DIGITO                                
            MOVE    WRK-B0431-DIGITO   TO  WRK-DIGITO                   
            PERFORM 4000-GRAVAR-MVTODIAS                                
           ELSE                                                         
            PERFORM 4000-GRAVAR-MVTODIAS                                
            MOVE    WRK-VALOR          TO  WRK-VMOVTO                   
            COMPUTE WRK-SLD-CTA-ESTOQUE = WRK-SLD-CTA-ESTOQUE           
                                                           - WRK-VMOVTO 
           END-IF.                                                      
                                                                        
       3520-FIM.                       EXIT.                            
      *----------------------------------------------------------------*
      * DEBITAR DA CONTA ESTOQUE                                       *
      *----------------------------------------------------------------*
       3530-DEBITAR-CONTA-MP           SECTION.                         
                                                                        
           IF  WRK-MOVTO-DEBITAR       GREATER WRK-SLD-CTA-MP           
            IF WRK-SLD-CTA-MP      NOT EQUAL   ZEROS                    
               MOVE POUPSL-AGE-PARA    TO  WRK-AUX-TAM-05               
               MOVE WRK-AUX-TAM-05-R   TO  WRK-AGENCIA                  
               MOVE POUPSL-CTA-PARA    TO  WRK-AUX-TAM-07               
               MOVE WRK-AUX-TAM-07-R   TO  WRK-CONTA                    
               PERFORM 3600-CALCULAR-DIGITO                             
               MOVE    WRK-B0431-DIGITO TO WRK-DIGITO                   
               MOVE    WRK-SLD-CTA-MP   TO  WRK-AUX-TAM-15              
               MOVE    WRK-AUX-TAM-15-R TO  WRK-VALOR                   
               PERFORM 4000-GRAVAR-MVTODIAS                             
            ELSE                                                        
               MOVE ZEROS              TO  WRK-VALOR                    
            END-IF                                                      
            MOVE   ZEROS               TO  WRK-SLD-CTA-MP               
            MOVE   WRK-VALOR           TO  WRK-VMOVTO                   
            COMPUTE WRK-MOVTO-DEBITAR   = WRK-MOVTO-DEBITAR - WRK-VMOVTO
            MOVE   WRK-MOVTO-DEBITAR   TO  WRK-AUX-TAM-15               
            MOVE   WRK-AUX-TAM-15-R    TO  WRK-VALOR                    
            MOVE   POUPSL-AGE-DE       TO  WRK-AUX-TAM-05               
            MOVE   WRK-AUX-TAM-05-R    TO  WRK-AGENCIA                  
            MOVE   POUPSL-CTA-DE       TO  WRK-AUX-TAM-07               
            MOVE   WRK-AUX-TAM-07-R    TO  WRK-CONTA                    
            PERFORM 3600-CALCULAR-DIGITO                                
            MOVE   WRK-B0431-DIGITO    TO  WRK-DIGITO                   
            PERFORM 4000-GRAVAR-MVTODIAS                                
           ELSE                                                         
            MOVE    POUPSL-AGE-PARA    TO  WRK-AUX-TAM-05               
            MOVE    WRK-AUX-TAM-05-R   TO  WRK-AGENCIA                  
            MOVE    POUPSL-CTA-PARA    TO  WRK-AUX-TAM-07               
            MOVE    WRK-AUX-TAM-07-R   TO  WRK-CONTA                    
            PERFORM 3600-CALCULAR-DIGITO                                
            MOVE    WRK-B0431-DIGITO   TO  WRK-DIGITO                   
            PERFORM 4000-GRAVAR-MVTODIAS                                
            MOVE    WRK-VALOR         TO  WRK-VMOVTO                    
            COMPUTE WRK-SLD-CTA-MP      =  WRK-SLD-CTA-MP - WRK-VMOVTO  
           END-IF.                                                      
                                                                        
       3530-FIM.                       EXIT.                            
      *----------------------------------------------------------------*
      * CALCULAR DIGITO DA NOVA CONTA RAZAO 10.71.                     *
      *----------------------------------------------------------------*
       3600-CALCULAR-DIGITO            SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE  WRK-AUX-TAM-07-R      TO  WRK-B0431-CONTA              
           MOVE  '7'                   TO  WRK-B0431-DIGITO             
           MOVE  +7                    TO  WRK-B0431-TAMANHO            
                                                                        
           CALL  'BRAD0431'            USING  WRK-B0431-CONTA           
                                              WRK-B0431-DIGITO          
                                              WRK-B0431-TAMANHO         
                                                                        
           IF WRK-B0431-DIGITO         EQUAL '.'                        
              DISPLAY '***=========== PEAT1064 ===========***'          
              DISPLAY '*                                    *'          
              DISPLAY '* ERRO NO CALCULO DO DIGITO DA NOVA  *'          
              DISPLAY '* CONTA RAZAO 10.71.                 *'          
              DISPLAY '*                                    *'          
              DISPLAY '* CONTA: ' POUPSL-CTA-PARA                       
                                      '                     *'          
              DISPLAY '*                                    *'          
              DISPLAY '***================================***'          
              MOVE  'BRAD0431 - ERRO NO CALCULO DO DIGITO DA CONTA'     
                                       TO  ERR-TEXTO                    
              MOVE  'APL'              TO  ERR-TIPO-ACESSO              
              PERFORM 9999-FINALIZAR-ERRO                               
           END-IF                                                       
                                                                        
           IF WRK-B0431-DIGITO         EQUAL 'P'                        
              MOVE ZEROS               TO WRK-B0431-DIGITO              
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       3600-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
       3700-MOVTODIAE-LT-ARQSALDO      SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           IF  WRK-DEB-CRED           EQUAL  '1'                        
               PERFORM 4000-GRAVAR-MVTODIAS                             
               PERFORM 2100-LER-MVTODIAE                                
           ELSE                                                         
               MOVE    WRK-CHV-MOV-AGE TO  WRK-AUX-TAM-05               
               MOVE    WRK-CHV-MOV-CTA TO  WRK-AUX-TAM-07               
               DISPLAY '***=========== PEAT1064 ===========***'         
               DISPLAY '*                                    *'         
               DISPLAY '*  CONTA NAO ABERTA NO RAZAO 10.71   *'         
               DISPLAY '* NAO ENCONTRADA NO ARQUIVO DE SALDO *'         
               DISPLAY '*                                    *'         
               DISPLAY '*  AGENCIA: ' WRK-AUX-TAM-05-R                  
                                        '                    *'         
               DISPLAY '*  CONTA..: ' WRK-AUX-TAM-07-R                  
                                          '                  *'         
               DISPLAY '*                                    *'         
               DISPLAY '***================================***'         
               MOVE  'CONTA NAO ENCONTRADA NO ARQUIVO DE SALDO'         
                                       TO  ERR-TEXTO                    
               MOVE  'APL'             TO  ERR-TIPO-ACESSO              
               PERFORM 9999-FINALIZAR-ERRO                              
           END-IF.                                                      
                                                                        
       3700-FIM.                       EXIT.                            
      *----------------------------------------------------------------*
       3800-IDENTIFICA-LANCAMENTO      SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE    WRK-NUMERO-DOC      TO  POUPM4-CDOCTO-LCTO           
           MOVE    WRK-LANCAMENTO      TO  POUPM4-CODLCTO-5POS          
           MOVE    SPACES              TO  POUPM4-OPER-TRL-CONTAB       
                                                                        
           MOVE    'POUP1065'          TO  WRK-MODULO                   
           CALL    WRK-MODULO          USING   REG-POUPM4.              
                                                                        
       3800-FIM.                       EXIT.                            
      *----------------------------------------------------------------*
      * GRAVAR REGISTRO NO ARQUIVO DE SAIDA MVTODIAS.                  *
      *----------------------------------------------------------------*
       4000-GRAVAR-MVTODIAS            SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           SET   WRK-GRAVACAO          TO  TRUE                         
                                                                        
           WRITE FD-MVTODIAS           FROM WRK-MCTS-REG                
                                                                        
           PERFORM 1130-TESTAR-FS-MVTODIAS                              
                                                                        
           ADD   1                     TO  ACU-GRAVS-MVTODIAS.          
                                                                        
      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * PROCEDIMENTOS FINAIS DO PROGRAMA.                              *
      *----------------------------------------------------------------*
       9000-FINALIZAR                  SECTION.                         
      *----------------------------------------------------------------*
                                                                        
      *                                                                 
      *--> "APRESENTAR O RESUMO DA EXECUCAO"                            
      *                                                                 
                                                                        
           PERFORM 9100-EXIBIR-ESTATISTICA                              
                                                                        
      *                                                                 
      *--> "FECHAR ARQUIVOS"                                            
      *                                                                 
                                                                        
           SET  WRK-FECHAMENTO         TO  TRUE                         
                                                                        
           CLOSE MVTODIAE                                               
                 ARQSALDO                                               
                 MVTODIAS                                               
                                                                        
           PERFORM 1100-TESTAR-FILE-STATUS                              
                                                                        
           STOP RUN.                                                    
                                                                        
      *----------------------------------------------------------------*
       9000-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * APRESENTAR O RESUMO DA EXECUCAO.                               *
      *----------------------------------------------------------------*
       9100-EXIBIR-ESTATISTICA         SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           DISPLAY '***************************************************'
           DISPLAY '**   ESTATISTICAS DE PROCESSAMENTO - PEAT1064    **'
           DISPLAY '**-----------------------------------------------**'
           DISPLAY '**  DDNAME  | I/O | DESCRICAO      |  QUANTIDADE **'
           DISPLAY '** -------- | --- | -------------- | ----------- **'
                                                                        
           MOVE    ACU-LIDOS-MVTODIAE                   TO WRK-MASC-ACU 
           DISPLAY '** MVTODIAE |  I  | TOTAL LIDOS    | ' WRK-MASC-ACU 
                                                                   ' **'
           MOVE    ACU-LIDOS-ARQSALDO                   TO WRK-MASC-ACU 
           DISPLAY '** ARQSALDO |  I  | TOTAL LIDOS    | ' WRK-MASC-ACU 
                                                                   ' **'
           MOVE    ACU-GRAVS-MVTODIAS                   TO WRK-MASC-ACU 
           DISPLAY '** MVTODIAS |  O  | TOTAL GRAVADOS | ' WRK-MASC-ACU 
                                                                   ' **'
           DISPLAY '**-----------------------------------------------**'
           MOVE    ACU-SLDO-INSUF-R1071                 TO WRK-MASC-ACU 
           DISPLAY '** SALDO INSUFICIENTE RAZAO 10.71  | ' WRK-MASC-ACU 
                                                                   ' **'
           DISPLAY '***************************************************'
           .                                                            
                                                                        
      *----------------------------------------------------------------*
       9100-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * FINALIZAR PROGRAMA COM ABEND.                                  *
      *----------------------------------------------------------------*
       9999-FINALIZAR-ERRO             SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE  WRK-LIT-PGM           TO  ERR-PGM                      
                                                                        
           CALL  'BRAD7100'            USING  WRK-LIT-BATCH             
                                              ERRO-AREA                 
                                                                        
           GOBACK.                                                      
                                                                        
      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*

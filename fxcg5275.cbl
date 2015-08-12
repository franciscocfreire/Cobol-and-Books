      *================================================================*
       IDENTIFICATION                  DIVISION.                        
      *================================================================*
                                                                        
       PROGRAM-ID. FXCG5275.                                            
       AUTHOR.     FERNANDA CARUSO.                                     
                                                                        
      *================================================================*
      *               B R Q   -   I T   S E R V I C E S                *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA     : FXCG5275                                     *
      *    ANALISTA     : WEI KANEGAE            - BRQ IT SERVICES.    *
      *    PROGRAMADOR  : FERNANDA CARUSO        - BRQ IT SERVICES.    *
      *    DATA         : 28/04/2011                                   *
      *                                                                *
      *    OBJETIVO     : GERAR ARQUIVO CONTENDO OS DADOS DO BLOCO     *
      *                   SEGMENTO DICIONARIO DE DADOS (PARAMETROS),   *
      *                   P/ FORMATAR OS E-MAILS QUE SERAO ENVIADOS    *
      *                   PELO SERVICO EMSG2000.                       *
      *                                                                *
      *                           ARQUIVOS                             *
      *           +---------------------------------------+            *
      *           |  DDNAME  |   I/O   |   BOOK   | LRECL |            *
      *           |----------|---------|----------|-------|            *
      *           | INPINCOK | INPUT   | FXCGWB94 |  080  |            *
      *           | SAIEDICD | OUTPUT  | FXCGWB74 | 25029 |            *
      *           +---------------------------------------+            *
      *                                                                *
      *           +---------------------------------------+            *
      *           | TABELA                    |  INCLUDE  |            *
      *           |---------------------------|-----------|            *
      *           | DB2PRD.TPRODT_SERVC_ADMTV | FXCGB044  |            *
      *           | DB2PRD.TPRODT_SERVC_CTBIL | FXCGB059  |            *
      *           +---------------------------------------+            *
      *                                                                *
      *    MODULOS :                                                   *
      *                                                                *
      *    -> BRAD0160 (    -   ) - OBTER JOBNAME                      *
      *    -> BRAD0450 (    -   ) - ABENDAR PROGRAMA                   *
      *    -> CKRS1000 (    -   ) - IDENTIFICA CONEXAO DB2             *
      *    -> CKRS1050 (    -   ) - CONEXAO DB2                        *
      *    -> CALE2000 (I#CALE01) - COMPONENTE CALENDARIO              *
      *    -> CDPS2027 (I#CDPSCZ) - OBTER DESCRICAO DO PRODUTO         *
      *    -> FRWK2999 (I#FRWKGE) - GRAV. DE ERRO NO COMPONENTE GLOG   *
      *                (I#FRWKHE) - AREA COMUM DE ERROS                *
      *                (I#FRWKAR) - INFORMACOES DE ERRO -> ARQUIVO     *
      *                (I#FRWKDB) - INFORMACOES DE ERRO -> DB2         *
      *                (I#FRWKMD) - INFORMACOES DE ERRO -> MODULO      *
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
                                                                        
           SELECT INPINCOK ASSIGN      TO INPINCOK                      
                      FILE STATUS      IS WRK-FS-INPINCOK.              
                                                                        
           SELECT SAIEDICD ASSIGN      TO SAIEDICD                      
                      FILE STATUS      IS WRK-FS-SAIEDICD.              
                                                                        
      *================================================================*
       DATA                            DIVISION.                        
      *================================================================*
                                                                        
      *----------------------------------------------------------------*
       FILE                            SECTION.                         
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * INPUT - ARQUIVO DOS PRODUTOS INCONSISTENTES (DESBATIDOS).      *
      *                                                                *
      *                 ORG. SEQUENCIAL  -  LRECL 080                  *
      *----------------------------------------------------------------*
                                                                        
       FD  INPINCOK                                                     
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  FD-INPINCOK                 PIC X(080).                      
                                                                        
      *----------------------------------------------------------------*
      * OUTPUT - INFORMACOES DO BLOCO SEGMENTO DICIONARIO DE DADOS.    *
      *                                                                *
      *                 ORG. SEQUENCIAL  -  LRECL 25029                *
      *----------------------------------------------------------------*
                                                                        
       FD  SAIEDICD                                                     
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  FD-SAIEDICD                 PIC X(25029).                    
                                                                        
      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.                         
      *----------------------------------------------------------------*
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER PIC X(34) VALUE  '** INICIO DA WORKING FXCG5275 **  '.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER PIC X(34) VALUE  '** FILE-STATUS DOS ARQUIVOS **    '.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
       01  WRK-FS-INPINCOK             PIC  X(002)  VALUE SPACES.       
       01  WRK-FS-SAIEDICD             PIC  X(002)  VALUE SPACES.       
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER PIC X(34) VALUE  '** BOOK DOS ARQUIVOS **           '.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
      *-->  INPINCOK                                                    
       COPY FXCGWB94.                                                   
                                                                        
      *-->  SAIEDICD                                                    
       COPY FXCGWB74.                                                   
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER PIC X(34) VALUE  '** AREA DA BRAD0160 **            '.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
       01  WRK-B0160-VALOR-FAC         PIC  9(005)  COMP-3 VALUE ZEROS. 
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER PIC X(34) VALUE  '** AREA DA BRAD0450 **            '.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
       01  WRK-B0450-ABEND             PIC S9(004)  COMP   VALUE +1111. 
       01  WRK-B0450-DUMP              PIC  X(001)         VALUE 'S'.   
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER PIC X(34) VALUE  '** AREA DA API - CALE2000 **      '.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
       COPY 'I#CALE01'.
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER PIC X(34) VALUE  '** AREA DA API - CDPS2027 **      '.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
       01  WRK-AREA-CDPS2027.                                           
           COPY 'I#CDPSCZ'.
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER PIC X(34) VALUE  '** AREA DA API - FRWK2999 **      '.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
       01  WRK-AREA-ERRO.                                               
           COPY 'I#FRWKGE'.
                                                                        
           05  WRK-BLOCO-INFO-ERRO.                                     
             10  WRK-CHAR-INFO-ERRO    PIC  X(001)                      
                                       OCCURS  0 TO 30000 TIMES         
                                       DEPENDING ON FRWKGHEA-TAM-DADOS. 
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER PIC X(34) VALUE  '** INFORMACOES DE ERRO - ARQ **   '.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
       01  WRK-AREA-ERRO-ARQUIVO.                                       
           COPY 'I#FRWKAR'.
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER PIC X(34) VALUE  '** INFORMACOES DE ERRO - DB2 **   '.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
       01  WRK-AREA-ERRO-DB2.                                           
           COPY 'I#FRWKDB'.
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER PIC X(34) VALUE  '** INFORMACOES DE ERRO - MODULO **'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
       01  WRK-AREA-ERRO-MODULO.                                        
           COPY 'I#FRWKMD'.
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER PIC X(34) VALUE  '** AREA PARA ESTATISTICA **       '.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
       COPY 'FXCGWB98'.
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER PIC X(34) VALUE  '** TABELA INTERNA **              '.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
       01  WRK-TAB-MENSAGEM.                                            
           05  WRK-TAB-DTMOVTO         PIC  X(010)  VALUE SPACES.       
           05  WRK-TAB-QTDE            PIC  9(003)  VALUE ZEROS.        
           05  WRK-TAB-LISTA-PRODT     OCCURS 231   TIMES.              
             10  WRK-TAB-PRODT         PIC  X(008).                     
             10  WRK-TAB-DESCR         PIC  X(100).                     
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER PIC X(34) VALUE  '** ACUMULADORES **                '.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
       01  ACU-LIDOS-INPINCOK          PIC  9(009)  COMP-3 VALUE ZEROS. 
       01  ACU-GRAVS-SAIEDICD          PIC  9(009)  COMP-3 VALUE ZEROS. 
       01  ACU-PRODT-ADMTV             PIC  9(009)  COMP-3 VALUE ZEROS. 
       01  ACU-PRODT-NEGOC             PIC  9(009)  COMP-3 VALUE ZEROS. 
       01  ACU-PRODT-SEM-DESCR         PIC  9(009)  COMP-3 VALUE ZEROS. 
       01  ACU-PRODT-DESPREZADOS       PIC  9(009)  COMP-3 VALUE ZEROS. 
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER PIC X(34) VALUE  '** AREA DE CHAVES **              '.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
       01  WRK-CHV-MSG.                                                 
           05  WRK-CHV-MSG-EMPR        PIC  9(010)  VALUE ZEROS.        
           05  WRK-CHV-MSG-SISTEMA     PIC  X(004)  VALUE SPACES.       
           05  WRK-CHV-MSG-DTMOVTO     PIC  X(010)  VALUE SPACES.       
                                                                        
       01  WRK-CHV-MSG-ANT.                                             
           05  WRK-CHV-MSG-EMPR-ANT    PIC  9(010)  VALUE ZEROS.        
           05  WRK-CHV-MSG-SIST-ANT    PIC  X(004)  VALUE SPACES.       
           05  WRK-CHV-MSG-DATA-ANT    PIC  X(010)  VALUE SPACES.       
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER PIC X(34) VALUE  '** VARIAVEIS AUXILIARES **        '.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
       01  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       01  WRK-TPO-PRODT               PIC  X(001)         VALUE SPACES.
       01  WRK-I                       PIC  9(003)  COMP-3 VALUE ZEROS. 
       01  WRK-TAMANHO                 PIC  9(005)  COMP-3 VALUE ZEROS. 
       01  WRK-CPRODUTO-ANT            PIC S9(008)         VALUE ZEROS. 
       01  WRK-CPRODT-SERVC-NULL       PIC S9(004)  COMP   VALUE ZEROS. 
       01  WRK-CPRODT-ADMTV-NULL       PIC S9(004)  COMP   VALUE ZEROS. 
       01  WRK-PRODT-DB2               PIC S9(008)  COMP-3 VALUE ZEROS. 
                                                                        
       01  WRK-PRODT                   PIC S9(008)  VALUE ZEROS.        
       01  WRK-PRODT-R                 REDEFINES  WRK-PRODT.            
           05  WRK-PRODT-SS            PIC  9(008).                     
                                                                        
       01  WRK-PRODT-X                 PIC  X(008) VALUE SPACES.        
       01  WRK-PRODT-N                 REDEFINES                        
           WRK-PRODT-X                 PIC  9(008).                     
                                                                        
       01  WRK-IND                     PIC S9(003)  COMP-3 VALUE ZEROS. 
       01  WRK-IND-R                   REDEFINES                        
           WRK-IND                     PIC  9(003)  COMP-3.             
                                                                        
HEX    01  WRK-PRODUTO                 PIC S9(008)  COMP-3 VALUE ZEROS. 
       01  WRK-PRODUTO-R                REDEFINES                       
           WRK-PRODUTO                 PIC  9(008).                     
      *                                                                 
      *--> "VARIAVEL PARA MASCARA DE DADOS"                             
      *                                                                 
                                                                        
       01  WRK-MASC-ACU                PIC  ZZZ.ZZZ.ZZZ.ZZ9             
                                                    VALUE ZEROS.        
                                                                        
      *                                                                 
      *--> "LITERAIS UTILIZADAS PELO PROGRAMA"                          
      *                                                                 
                                                                        
       01  WRK-LIT-PGM                 PIC  X(008)  VALUE 'FXCG5275'.   
       01  WRK-FRWK2999                PIC  X(008)  VALUE 'FRWK2999'.   
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER PIC X(34) VALUE  '** AREA DE INCLUDES PARA DB2 **   '.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
           EXEC SQL                                                     
              INCLUDE SQLCA                                             
           END-EXEC.                                                    
                                                                        
           EXEC SQL                                                     
              INCLUDE FXCGB044                                          
           END-EXEC.                                                    
                                                                        
           EXEC SQL                                                     
              INCLUDE FXCGB059                                          
           END-EXEC.                                                    
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER PIC X(34) VALUE  '** FIM DA WORKING FXCG5275 **     '.
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
                   WRK-FS-INPINCOK     EQUAL  '10'                      
                                                                        
           PERFORM 9000-FINALIZAR.                                      
                                                                        
      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * PROCEDIMENTOS INICIAIS.                                        *
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE  '1000-INICIALIZAR'    TO  FRWKGHEA-IDEN-PARAGRAFO      
                                                                        
           CALL  'CKRS1000'                                             
           CALL  'CKRS1050'                                             
                                                                        
           INITIALIZE  FRWKGERR-BLOCO-RETORNO                           
                       FRWKGHEA-REGISTRO                                
                       FRWKGARQ-REGISTRO                                
                       FRWKGDB2-REGISTRO                                
                       FRWKGMOD-REGISTRO                                
                                                                        
      *                                                                 
      *--> "ABERTURA DOS ARQUIVOS"                                      
      *                                                                 
                                                                        
           SET  ARQ-OPEN               TO  TRUE                         
                                                                        
           OPEN INPUT  INPINCOK                                         
                OUTPUT SAIEDICD                                         
                                                                        
           PERFORM 1100-TESTAR-FILE-STATUS                              
                                                                        
      *                                                                 
      *--> "OBTER DATA E HORA DO SISTEMA"                               
      *                                                                 
                                                                        
           PERFORM 1200-OBTER-DATA-HORA-SISTEMA                         
                                                                        
      *                                                                 
      *--> "OBTER JOBNAME"                                              
      *                                                                 
                                                                        
           PERFORM 1300-OBTER-JOBNAME                                   
                                                                        
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
                                                                        
           PERFORM 1110-TESTAR-FS-INPINCOK                              
           PERFORM 1120-TESTAR-FS-SAIEDICD.                             
                                                                        
      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * TESTAR FILE-STATUS DO ARQUIVO DE ENTRADA INPINCOK.             *
      *----------------------------------------------------------------*
       1110-TESTAR-FS-INPINCOK         SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           IF WRK-FS-INPINCOK          NOT EQUAL '00'                   
              SET   ERRO-ARQUIVO       TO  TRUE                         
              MOVE  WRK-FS-INPINCOK    TO  FRWKGARQ-FILE-STATUS         
              MOVE  'INPINCOK'         TO  FRWKGARQ-NOME-ARQUIVO        
              PERFORM 9999-FINALIZAR-ERRO                               
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * TESTAR FILE-STATUS DO ARQUIVO DE SAIDA SAIEDICD.               *
      *----------------------------------------------------------------*
       1120-TESTAR-FS-SAIEDICD         SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           IF WRK-FS-SAIEDICD          NOT EQUAL '00'                   
              SET   ERRO-ARQUIVO       TO  TRUE                         
              MOVE  WRK-FS-SAIEDICD    TO  FRWKGARQ-FILE-STATUS         
              MOVE  'SAIEDICD'         TO  FRWKGARQ-NOME-ARQUIVO        
              PERFORM 9999-FINALIZAR-ERRO                               
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * OBTER DATA E HORA DO SISTEMA ATRAVES DA API CALE2000.          *
      *----------------------------------------------------------------*
       1200-OBTER-DATA-HORA-SISTEMA    SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE  '1200-OBTER-DATA-HORA-SISTEMA'                         
                                       TO  FRWKGHEA-IDEN-PARAGRAFO      
                                                                        
           INITIALIZE  CALE01-ENTRADA                                   
                                                                        
      *--  F3     = CONSULTA                                            
      *--  SF3002 = FORNECER DATA E HORARIO ATUAL                       
                                                                        
           MOVE  'CALE2000'            TO  WRK-MODULO                   
           MOVE  'CALE0001'            TO  CALE01-ID-BLOCO              
           MOVE  1182                  TO  CALE01-TAM-BLOCO             
           MOVE  'F3'                  TO  CALE01-FUNCAO                
           MOVE  'SF3002'              TO  CALE01-SUB-FUNCAO            
                                                                        
           CALL  WRK-MODULO            USING  CALE01-REGISTRO           
                                                                        
           IF CALE01-COD-RETORNO       NOT EQUAL ZEROS                  
              SET   ERRO-MODULO        TO  TRUE                         
              MOVE  WRK-MODULO         TO  FRWKGMOD-NOME-MODULO         
              MOVE  CALE01-COD-RETORNO TO  FRWKGMOD-COD-RETORNO         
              MOVE  CALE01-COD-ERRO    TO  FRWKGMOD-COD-ERRO            
              MOVE  CALE01-COD-MENSAGEM-GMSG                            
                                       TO  FRWKGMOD-COD-MENSAGEM        
              PERFORM 9999-FINALIZAR-ERRO                               
           END-IF                                                       
                                                                        
           MOVE  CALE01-DT-GREGO-B-DDMMAAAA                             
                                       TO  WRK-DATAPROC                 
                                           WRK-DATAMOV                  
           MOVE  CALE01-HH-2PONTOS-HHMMSS                               
                                       TO  WRK-HORAPROC.                
                                                                        
      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * OBTER JOBNAME ATRAVES DA BRAD0160.                             *
      *----------------------------------------------------------------*
       1300-OBTER-JOBNAME              SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE   SPACES               TO     WRK-JOBNAME               
                                                                        
           CALL  'BRAD0160'            USING  WRK-JOBNAME               
                                              WRK-B0160-VALOR-FAC.      
                                                                        
      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * VERIFICAR SE ARQUIVOS DE ENTRADA ESTAO VAZIOS.                 *
      *----------------------------------------------------------------*
       2000-VERIFICAR-VAZIO            SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           PERFORM 2100-LER-INPINCOK                                    
                                                                        
           IF WRK-FS-INPINCOK          EQUAL '10'                       
              DISPLAY '***================================***'          
              DISPLAY '*              FXCG5275              *'          
              DISPLAY '*------------------------------------*'          
              DISPLAY '*                                    *'          
              DISPLAY '*     ARQUIVO INPINCOK VAZIO !!!     *'          
              DISPLAY '*                                    *'          
              DISPLAY '***================================***'          
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * FAZER A LEITURA DO ARQUIVO INPINCOK.                           *
      *----------------------------------------------------------------*
       2100-LER-INPINCOK               SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE  '2100-LER-INPINCOK'   TO  FRWKGHEA-IDEN-PARAGRAFO      
                                                                        
           SET  ARQ-READ               TO   TRUE                        
           READ INPINCOK               INTO FXCGWB94-REG                
                                                                        
           IF WRK-FS-INPINCOK          NOT EQUAL '10'                   
              PERFORM 1110-TESTAR-FS-INPINCOK                           
              ADD   1                  TO  ACU-LIDOS-INPINCOK           
              MOVE  FXCGWB94-CPSSOA-JURID                               
                                       TO  WRK-CHV-MSG-EMPR             
              MOVE  FXCGWB94-SISTEMA   TO  WRK-CHV-MSG-SISTEMA          
              MOVE  FXCGWB94-DTMOVTO   TO  WRK-CHV-MSG-DTMOVTO          
                                                                        
              IF (WRK-CHV-MSG          EQUAL WRK-CHV-MSG-ANT) AND       
                 (FXCGWB94-CPRODUTO    EQUAL WRK-CPRODUTO-ANT)          
                  ADD   1              TO  ACU-PRODT-DESPREZADOS        
                  GO TO 2100-LER-INPINCOK                               
              END-IF                                                    
                                                                        
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * PROCESSAMENTO PRINCIPAL.                                       *
      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           INITIALIZE  WRK-TAB-MENSAGEM                                 
           MOVE  WRK-CHV-MSG                  TO  WRK-CHV-MSG-ANT       
                                                                        
           PERFORM 3100-CARREGAR-LISTA-PRODTS VARYING   WRK-I           
                                              FROM      1 BY 1          
                      UNTIL  (WRK-I           GREATER   231)            
                         OR  (WRK-CHV-MSG     NOT EQUAL WRK-CHV-MSG-ANT)
                         OR  (WRK-FS-INPINCOK     EQUAL '10')           
                                                                        
           PERFORM 3500-CARREGAR-SAIEDICD                               
           PERFORM 3600-GRAVAR-SAIEDICD.                                
                                                                        
      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * CARREGAR A LISTA DOS PRODUTOS NA TABELA INTERNA.               *
      *----------------------------------------------------------------*
       3100-CARREGAR-LISTA-PRODTS      SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE  FXCGWB94-CPRODUTO     TO  WRK-PRODT                    
           MOVE  WRK-PRODT-SS          TO  WRK-PRODT-N                  
           MOVE  WRK-PRODT-X           TO  WRK-TAB-PRODT(WRK-I)         
                                                                        
           PERFORM 3200-VERIFICAR-TIPO-PRODT                            
                                                                        
           IF WRK-TPO-PRODT            EQUAL 'A'                        
              ADD     1                TO  ACU-PRODT-ADMTV              
              PERFORM 3300-OBTER-DESCR-PRODT-TAB44                      
           ELSE                                                         
              IF WRK-TPO-PRODT         EQUAL 'N'                        
                 ADD     1             TO  ACU-PRODT-NEGOC              
                 PERFORM 3400-OBTER-DESCR-PRODT-CDPS                    
              ELSE                                                      
                 MOVE  'DESCRICAO NAO ENCONTRADA - TIPO'                
                                       TO  WRK-TAB-DESCR(WRK-I)         
                 ADD    1              TO  ACU-PRODT-SEM-DESCR          
              END-IF                                                    
           END-IF                                                       
                                                                        
           MOVE  FXCGWB94-CPRODUTO     TO  WRK-CPRODUTO-ANT             
                                                                        
           PERFORM 2100-LER-INPINCOK.                                   
                                                                        
      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * VERIFICAR NA TABELA FXCGB059 SE PRODUTO EH ADMTV OU NEGOCIAVEL *
      *----------------------------------------------------------------*
       3200-VERIFICAR-TIPO-PRODT       SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE  '3200-VERIFICAR-TIPO-PRODT'                            
                                       TO  FRWKGHEA-IDEN-PARAGRAFO      
           MOVE  FXCGWB94-CPRODUTO     TO  WRK-PRODT-DB2                
                                                                        
           EXEC SQL                                                     
              SELECT                                                    
                     CPRODT_SERVC                                       
                    ,CPRODT_SERVC_ADMTV                                 
              INTO                                                      
                    :FXCGB059.CPRODT-SERVC                              
                       :WRK-CPRODT-SERVC-NULL,                          
                    :FXCGB059.CPRODT-SERVC-ADMTV                        
                       :WRK-CPRODT-ADMTV-NULL                           
              FROM                                                      
                     DB2PRD.TPRODT_SERVC_CTBIL                          
              WHERE                                                     
                    (CPRODT_SERVC       = :WRK-PRODT-DB2)               
                 OR (CPRODT_SERVC_ADMTV = :WRK-PRODT-DB2)               
                                                                        
                 FETCH FIRST 1 ROW ONLY                                 
                                                                        
           END-EXEC                                                     
                                                                        
           IF (SQLCODE                     NOT EQUAL ZEROS) OR          
              (SQLWARN0                    EQUAL 'W')                   
              IF (SQLCODE                     NOT EQUAL +100)           
                  SET     ERRO-DB2            TO  TRUE                  
                  MOVE   'TPRODT_SERVC_CTBIL' TO  FRWKGDB2-NOME-TABELA  
                  SET     DB2-SELECT          TO  TRUE                  
                  MOVE   '1'                  TO  FRWKGDB2-LOCAL        
                  PERFORM 9999-FINALIZAR-ERRO                           
              ELSE                                                      
                  MOVE    SPACES              TO  WRK-TPO-PRODT         
                  GO TO   3200-99-FIM                                   
              END-IF                                                    
           END-IF                                                       
                                                                        
           IF WRK-CPRODT-SERVC-NULL    LESS ZEROS                       
               MOVE  'A'                TO  WRK-TPO-PRODT               
           ELSE                                                         
              IF WRK-CPRODT-ADMTV-NULL LESS ZEROS                       
                 MOVE  'N'             TO  WRK-TPO-PRODT                
              END-IF                                                    
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * OBTER A DESCRICAO DO PRODUTO ADMINISTRATIVO NA TABELA FXCGB044 *
      *----------------------------------------------------------------*
       3300-OBTER-DESCR-PRODT-TAB44    SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE  '3300-OBTER-DESCR-PRODT-TAB44'                         
                                       TO  FRWKGHEA-IDEN-PARAGRAFO      
                                                                        
           MOVE   FXCGWB94-CPRODUTO    TO  CPRODT-SERVC-ADMTV           
                                       OF  FXCGB044                     
                                                                        
           EXEC SQL                                                     
              SELECT                                                    
                     RRSUMO_PRODT_SERVC                                 
              INTO                                                      
                    :FXCGB044.RRSUMO-PRODT-SERVC                        
              FROM                                                      
                     DB2PRD.TPRODT_SERVC_ADMTV                          
              WHERE                                                     
                    (CPRODT_SERVC_ADMTV = :FXCGB044.CPRODT-SERVC-ADMTV) 
           END-EXEC                                                     
                                                                        
           IF (SQLCODE                        NOT EQUAL ZEROS) OR       
              (SQLWARN0                       EQUAL 'W')                
              IF (SQLCODE                     NOT EQUAL +100)           
                  SET     ERRO-DB2            TO  TRUE                  
                  MOVE   'TPRODT_SERVC_ADMTV' TO  FRWKGDB2-NOME-TABELA  
                  SET     DB2-SELECT          TO  TRUE                  
                  MOVE   '2'                  TO  FRWKGDB2-LOCAL        
                  PERFORM 9999-FINALIZAR-ERRO                           
              ELSE                                                      
                  MOVE   'DESCRICAO NAO ENCONTRADA - ADMTV'             
                                              TO  WRK-TAB-DESCR(WRK-I)  
                  ADD     1                   TO  ACU-PRODT-SEM-DESCR   
              END-IF                                                    
           ELSE                                                         
              MOVE  RRSUMO-PRODT-SERVC        TO  WRK-TAB-DESCR(WRK-I)  
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       3300-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * OBTER A DESCRICAO DO PRODUTO NEGOCIAVEL ATRAVES DO CDPS2027.   *
      *----------------------------------------------------------------*
       3400-OBTER-DESCR-PRODT-CDPS     SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE  '3400-OBTER-DESCR-PRODT-CDPS'                          
                                       TO  FRWKGHEA-IDEN-PARAGRAFO      
                                                                        
HEX********KMOVE   FXCGWB94-CPRODUTO    TO  CDPSGLCZ-E-CPRODT-SERVC     
HEX        MOVE   FXCGWB94-CPRODUTO    TO  WRK-PRODUTO                  
HEX        MOVE   WRK-PRODUTO-R        TO  CDPSGLCZ-E-CPRODT-SERVC      
                                                                        
           MOVE  'CDPS2027'            TO  WRK-MODULO                   
                                                                        
           CALL  WRK-MODULO            USING  WRK-AREA-CDPS2027         
                                                                        
           IF CDPSGLCZ-COD-RETORNO     NOT EQUAL ZEROS AND 08           
              SET   ERRO-MODULO        TO  TRUE                         
              MOVE  WRK-MODULO         TO  FRWKGMOD-NOME-MODULO         
              MOVE  CDPSGLCZ-COD-RETORNO                                
                                       TO  FRWKGMOD-COD-RETORNO         
              MOVE  CDPSGLCZ-COD-ERRO  TO  FRWKGMOD-COD-ERRO            
              MOVE  CDPSGLCZ-COD-MENSAGEM                               
                                       TO  FRWKGMOD-COD-MENSAGEM        
              PERFORM 9999-FINALIZAR-ERRO                               
           END-IF                                                       
                                                                        
           IF CDPSGLCZ-COD-RETORNO     EQUAL 08                         
              MOVE  'DESCRICAO NAO ENCONTRADA - CDPS'                   
                                       TO  WRK-TAB-DESCR(WRK-I)         
              ADD   1                  TO  ACU-PRODT-SEM-DESCR          
           ELSE                                                         
              MOVE  CDPSGLCZ-S-RRSUMO-PRODT-SERVC                       
                                       TO  WRK-TAB-DESCR(WRK-I)         
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       3400-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * CARREGAR DADOS NO ARQUIVO DE SAIDA SAIEDICD.                   *
      *----------------------------------------------------------------*
       3500-CARREGAR-SAIEDICD          SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE  WRK-CHV-MSG-EMPR-ANT  TO  FXCGWB74-CPSSOA-JURID        
           MOVE  WRK-CHV-MSG-SIST-ANT  TO  FXCGWB74-SISTEMA             
           MOVE  WRK-CHV-MSG-DATA-ANT  TO  FXCGWB74-DTMOVTO             
                                           WRK-TAB-DTMOVTO              
                                                                        
           SUBTRACT  1                 FROM  WRK-I                      
           MOVE      WRK-I             TO    WRK-TAB-QTDE               
                                                                        
           MOVE  SPACES                TO  FXCGWB74-MENSAGEM            
                                                                        
           MOVE  WRK-TAB-MENSAGEM      TO  FXCGWB74-MENSAGEM            
                                                                        
           COMPUTE WRK-TAMANHO = (108 * WRK-I)                          
           ADD   13                    TO  WRK-TAMANHO                  
                                                                        
           PERFORM VARYING  WRK-IND    FROM  100 BY -1                  
                     UNTIL (WRK-IND    LESS  ZEROS)                     
                        OR (WRK-TAB-DESCR(WRK-I)(WRK-IND:1)             
                                       NOT EQUAL SPACES)                
           END-PERFORM                                                  
                                                                        
           IF WRK-IND                  LESS  ZEROS                      
              SUBTRACT  100            FROM  WRK-TAMANHO                
           ELSE                                                         
              COMPUTE WRK-IND-R   = (100 - WRK-IND-R)                   
              COMPUTE WRK-TAMANHO = (WRK-TAMANHO - WRK-IND-R)           
           END-IF                                                       
                                                                        
           MOVE    WRK-TAMANHO         TO  FXCGWB74-TAM-MSG.            
                                                                        
      *----------------------------------------------------------------*
       3500-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * GRAVAR ARQUIVO DE SAIDA SAIEDICD.                              *
      *----------------------------------------------------------------*
       3600-GRAVAR-SAIEDICD            SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE '3600-GRAVAR-SAIEDICD' TO  FRWKGHEA-IDEN-PARAGRAFO      
                                                                        
           SET   ARQ-WRITE             TO  TRUE                         
                                                                        
           WRITE FD-SAIEDICD           FROM FXCGWB74-REG                
                                                                        
           PERFORM 1120-TESTAR-FS-SAIEDICD                              
                                                                        
           ADD   1                     TO  ACU-GRAVS-SAIEDICD.          
                                                                        
      *----------------------------------------------------------------*
       3600-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * PROCEDIMENTOS FINAIS DO PROGRAMA.                              *
      *----------------------------------------------------------------*
       9000-FINALIZAR                  SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE  '9000-FINALIZAR'      TO  FRWKGHEA-IDEN-PARAGRAFO      
                                                                        
      *                                                                 
      *--> "APRESENTAR O RESUMO DA EXECUCAO"                            
      *                                                                 
                                                                        
           PERFORM 9100-EXIBIR-ESTATISTICA                              
                                                                        
      *                                                                 
      *--> "FECHAR ARQUIVOS"                                            
      *                                                                 
                                                                        
           SET  ARQ-CLOSE              TO  TRUE                         
                                                                        
           CLOSE  INPINCOK                                              
                  SAIEDICD                                              
                                                                        
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
                                                                        
           DISPLAY WRK-DISP1                                            
           DISPLAY WRK-DISP2                                            
           DISPLAY WRK-DISP3                                            
           DISPLAY WRK-DISP4                                            
           DISPLAY WRK-DISP5                                            
                                                                        
           MOVE    WRK-LIT-PGM                   TO  WRK-PROGNAME       
                                                                        
           DISPLAY WRK-DISP6                                            
           DISPLAY WRK-DISP3                                            
           DISPLAY WRK-DISP7                                            
           DISPLAY WRK-DISP8                                            
                                                                        
           MOVE   'INPINCOK'                     TO  WRK-DDNAME         
           MOVE   'I'                            TO  WRK-I-O            
           MOVE   'TOTAL LIDOS                '  TO  WRK-DESCARQ        
           MOVE    ACU-LIDOS-INPINCOK            TO  WRK-QTDEARQ        
           DISPLAY WRK-DISP9                                            
                                                                        
           MOVE   'SAIEDICD'                     TO  WRK-DDNAME         
           MOVE   'O'                            TO  WRK-I-O            
           MOVE   'TOTAL GRAVADOS             '  TO  WRK-DESCARQ        
           MOVE    ACU-GRAVS-SAIEDICD            TO  WRK-QTDEARQ        
           DISPLAY WRK-DISP9                                            
                                                                        
           MOVE    ACU-PRODT-ADMTV               TO  WRK-MASC-ACU       
           DISPLAY '** TOTAL DE PRODUTOS ADMINISTRATIVOS            |   
      -    ''      WRK-MASC-ACU ' **'                                   
                                                                        
           MOVE    ACU-PRODT-NEGOC               TO  WRK-MASC-ACU       
           DISPLAY '** TOTAL DE PRODUTOS NEGOCIAVEIS                |   
      -    ''      WRK-MASC-ACU ' **'                                   
                                                                        
           MOVE    ACU-PRODT-SEM-DESCR           TO  WRK-MASC-ACU       
           DISPLAY '** TOTAL DE PRODUTOS NAO ENCONTRADA A DESCRICAO |   
      -    ''      WRK-MASC-ACU ' **'                                   
                                                                        
           MOVE    ACU-PRODT-DESPREZADOS         TO  WRK-MASC-ACU       
           DISPLAY '** TOTAL DE PRODUTOS DESPREZADOS (IGUAIS)       |   
      -    ''      WRK-MASC-ACU ' **'                                   
                                                                        
           DISPLAY WRK-DISP1.                                           
                                                                        
      *----------------------------------------------------------------*
       9100-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * FINALIZAR PROGRAMA COM ABEND.                                  *
      *----------------------------------------------------------------*
       9999-FINALIZAR-ERRO             SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE  12                    TO  FRWKGERR-COD-RETORNO         
           MOVE  WRK-LIT-PGM           TO  FRWKGHEA-NOME-PROGRAMA       
                                                                        
           EVALUATE FRWKGHEA-TIPO-ERRO                                  
                                                                        
             WHEN  'AR'                                                 
                SET   FRWKGERR-FSTATUS-INCOR    TO  TRUE                
                MOVE  FRWKGARQ-TAM-LAYOUT       TO  FRWKGHEA-TAM-DADOS  
                MOVE  WRK-AREA-ERRO-ARQUIVO     TO  WRK-BLOCO-INFO-ERRO 
                                                                        
             WHEN  'DB'                                                 
                MOVE  SQLSTATE                  TO  FRWKGDB2-SQLSTATE   
                MOVE  SQLCODE                   TO  FRWKGDB2-SQLCODE    
                SET   FRWKGERR-SQLCODE-INCOR    TO  TRUE                
                MOVE  FRWKGDB2-TAM-LAYOUT       TO  FRWKGHEA-TAM-DADOS  
                MOVE  WRK-AREA-ERRO-DB2         TO  WRK-BLOCO-INFO-ERRO 
                                                                        
             WHEN  'MO'                                                 
                SET   FRWKGERR-COD-RETOR-INCOR  TO  TRUE                
                MOVE  FRWKGMOD-TAM-LAYOUT       TO  FRWKGHEA-TAM-DADOS  
                MOVE  WRK-AREA-ERRO-MODULO      TO  WRK-BLOCO-INFO-ERRO 
                                                                        
           END-EVALUATE                                                 
                                                                        
           CALL  WRK-FRWK2999          USING  WRK-AREA-ERRO             
                                                                        
           CALL  'BRAD0450'            USING  WRK-B0450-ABEND           
                                              WRK-B0450-DUMP.           
                                                                        
      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        

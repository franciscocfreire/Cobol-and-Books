      *================================================================*
       IDENTIFICATION                  DIVISION.                        
      *================================================================*
                                                                        
       PROGRAM-ID. RDAB0028.                                            
       AUTHOR.     MARCIO CRISTIANO MORAIS CUSTIN.                      
      *================================================================*
      *                    C P M  -  S I S T E M A S                   *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   RDAB0028                                    *
      *    PROGRAMADOR.:   MARCIO C. M. CUSTIN    - CPM                *
      *    ANALISTA CPM:   DANIEL AUGUSTO DANIELI - CPM                *
      *    ANALISTA....:   HENRY HIGA             - CPM / GRUPO 82     *
      *    DATA .......:   10/02/2003                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   OBTER O NOME DO CONJUGE DO ARQUIVO IMAGEM   *
      *      DA TABELA IRESV001, ATRAVES DE  UM  BALANCE-LINE  COM O   *
      *      ARQUIVO DE PARCELAS VENCIDAS/VINCENDAS.                   *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                    DDNAME                    INCLUDE/BOOK      *
      *                    ARQPARVV                    I#RDAB01        *
      *                    ARQUIRES                    I#IRES11        *
      *                    ARQDAPES                    I#RDAB05        *
      *                    RELNENCO                        -           *
      *                    RELTOTAL                        -           *
      *----------------------------------------------------------------*
      *    INC'S.......:                                               *
      *    POL7100C - AREA PARA TRATAMENTO DE ERRO PELA POOL7100       *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    POOL7100 - TRATAMENTO DE ERRO QUANDO PROGRAMA INVALIDO      *
      *    POOL7600 - OBTEM DATA E HORA DO SISTEMA                     *
      *================================================================*
      *                                                                *
BRQ059* =============================================================  *
      * |                      ALTERACAO                            |  *
      * ------------------------------------------------------------+  *
      *  ANALISTA     : ROBSON VELASQUES / BRQ - IT SERVICES        |  *
      *  PROGRAMADOR  : CRISTIANO SOUZA  / BRQ - IT SERVICES        |  *
      *  DATA         : 03/2012                                     |  *
      *  OBJETIVO     : INSERIR UM DIGITO A MAIS AOS CAMPOS DE      |  *
      *                 TELEFONES (I#RDAB05).                       |  *
      *  PROJETO      : 2012/0059 - ADEQUAR NRO TELEFONE(RES.553)   |  *
      * ============================================================+  *
BRQ059******************************************************************
           EJECT                                                        
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
                                                                        
           SELECT ARQPARVV ASSIGN      TO UT-S-ARQPARVV                 
           FILE STATUS                 IS WRK-FS-ARQPARVV.              
                                                                        
           SELECT ARQUIRES ASSIGN      TO UT-S-ARQUIRES                 
           FILE STATUS                 IS WRK-FS-ARQUIRES.              
                                                                        
           SELECT ARQDAPES ASSIGN      TO UT-S-ARQDAPES                 
           FILE STATUS                 IS WRK-FS-ARQDAPES.              
                                                                        
           SELECT RELNENCO ASSIGN      TO UT-S-RELNENCO                 
           FILE STATUS                 IS WRK-FS-RELNENCO.              
                                                                        
           SELECT RELTOTAL ASSIGN      TO UT-S-RELTOTAL                 
           FILE STATUS                 IS WRK-FS-RELTOTAL.              
                                                                        
           EJECT                                                        
      *================================================================*
       DATA                            DIVISION.                        
      *================================================================*
                                                                        
      *----------------------------------------------------------------*
       FILE                            SECTION.                         
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *    INPUT:  ARQUIVO DE PARCELAS VENCIDAS/VINCENDAS              *
      *            ORG. SEQUENCIAL     -   LRECL   =  150              *
      *----------------------------------------------------------------*
                                                                        
       FD  ARQPARVV                                                     
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    

       COPY 'I#RDAB01'.
                                                                        
      *----------------------------------------------------------------*
      *    INPUT:  ARQUIVO IMAGEM DA TABELA IRESV001                   *
      *            ORG. SEQUENCIAL     -   LRECL   =  169              *
      *----------------------------------------------------------------*
                                                                        
       FD  ARQUIRES                                                     
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    

       COPY 'I#RDABAR'.

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE DADOS DA PESSOA                          *
      *            ORG. SEQUENCIAL     -   LRECL   =  500              *
      *----------------------------------------------------------------*
                                                                        
       FD  ARQDAPES                                                     
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    

       COPY 'I#RDAB05'.

      *----------------------------------------------------------------*
      *    OUTPUT: RELATORIO DE REGISTROS NAO ENCONTRADOS NO ARQUIVO   *
      *            IMAGEM DA TABELA IRESV001                           *
      *            ORG. SEQUENCIAL     -   LRECL   =  132              *
      *----------------------------------------------------------------*
                                                                        
       FD  RELNENCO                                                     
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  REG-RELNENCO                PIC  X(132).                     
                                                                        
      *----------------------------------------------------------------*
      *    OUTPUT: RELATORIO DE TOTALIZACAO DE QTDE DE LIDOS           *
      *           (ARQPARVV, ARQUIRES), GRAVADOS (ARQDAPES)            *
      *            ORG. SEQUENCIAL     -   LRECL   =  080              *
      *----------------------------------------------------------------*
                                                                        
       FD  RELTOTAL                                                     
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  REG-RELTOTAL                PIC  X(080).                     
                                                                        
      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.                         
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE        
           '*** RDAB0028 - INICIO DA AREA DE WORKING ***'.              
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE        
           '*** AREA PARA ACUMULADORES ***'.                            
      *----------------------------------------------------------------*
                                                                        
       77  ACU-PAG                    PIC  9(007)    COMP-3 VALUE ZEROS.
       77  ACU-LINHAS                 PIC  9(002)           VALUE 99.   
       77  ACU-LIDOS-ARQUIRES         PIC  9(009)    COMP-3 VALUE ZEROS.
       77  ACU-LIDOS-ARQPARVV         PIC  9(009)    COMP-3 VALUE ZEROS.
       77  ACU-GRAVA-ARQDAPES         PIC  9(009)    COMP-3 VALUE ZEROS.
       77  ACU-IMPRE-RELNENCO         PIC  9(009)    COMP-3 VALUE ZEROS.
                                                                        
           EJECT                                                        
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE        
           '*** AREA PARA TESTE DE FILE-STATUS ***'.                    
      *----------------------------------------------------------------*
                                                                        
       77  WRK-FS-ARQPARVV             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-ARQUIRES             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-ARQDAPES             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-RELNENCO             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-RELTOTAL             PIC  X(002)         VALUE SPACES.
                                                                        
       77  WRK-ABERTURA                PIC  X(013)         VALUE        
           ' NA ABERTURA'.                                              
       77  WRK-LEITURA                 PIC  X(013)         VALUE        
           ' NA LEITURA'.                                               
       77  WRK-GRAVACAO                PIC  X(013)         VALUE        
           ' NA GRAVACAO'.                                              
       77  WRK-FECHAMENTO              PIC  X(013)         VALUE        
           'NO FECHAMENTO'.                                             
                                                                        
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE        
           '*** AREAS AUXILIARES ***'.                                  
      *----------------------------------------------------------------*
                                                                        
       77  WRK-BATCH                   PIC  X(008)        VALUE 'BATCH'.
       77  WRK-MASCARA                 PIC  ZZZ.ZZZ.ZZ9.                
                                                                        
       01  WRK-CGC-CPF.                                                 
           03  WRK-CPF                 PIC  999.999.999.                
           03  FILLER                  PIC  X(001)        VALUE '/'.    
           03  WRK-FILIAL              PIC  99999.                      
           03  FILLER                  PIC  X(001)        VALUE '-'.    
           03  WRK-CONTROLE            PIC  99.                         
                                                                        
       01  WRK-CGC-CPF-1.                                               
           03  FILLER                  PIC  X(003)        VALUE SPACES. 
           03  WRK-CPF-1               PIC  999.999.999.                
           03  FILLER                  PIC  X(001)        VALUE '-'.    
           03  WRK-CONTROLE-1          PIC  99.                         
                                                                        
       01  WRK-CHV-ARQUIRES.                                            
           03  WRK-CCLUB-ARQUIRES      PIC  9(010)        VALUE ZEROS.

       01  WRK-CHV-ARQPARVV.                                            
           03  WRK-CCLUB-ARQPARVV      PIC  9(010)        VALUE ZEROS.
                                                                        
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE        
           '*** AREA DA POOL7600 ***'.                                  
      *----------------------------------------------------------------*
                                                                        
       01  WRK-DATA-HORA.                                               
           03  WRK-DATA-JULIANA        PIC  9(005) COMP-3  VALUE ZEROS. 
           03  WRK-DATA-AAMMDD         PIC  9(007) COMP-3  VALUE ZEROS. 
           03  WRK-DATA-AAAAMMDD       PIC  9(009) COMP-3  VALUE ZEROS. 
           03  WRK-DATA-HHMMSS         PIC  9(007) COMP-3  VALUE ZEROS. 
           03  WRK-DATA-HHMMSSMMMMMM   PIC  9(013) COMP-3  VALUE ZEROS. 
           03  WRK-TIMESTAMP-7600.                                      
               05  WRK-ANO             PIC  9(004)         VALUE ZEROS. 
               05  WRK-MES             PIC  9(002)         VALUE ZEROS. 
               05  WRK-DIA             PIC  9(002)         VALUE ZEROS. 
               05  WRK-HORA            PIC  9(002)         VALUE ZEROS. 
               05  WRK-MINUTO          PIC  9(002)         VALUE ZEROS. 
               05  FILLER              PIC  X(008)         VALUE SPACES.
                                                                        
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE        
           '*** AREA ERRO DE FILE-STATUS ***'.                          
      *----------------------------------------------------------------*
                                                                        
       01  WRK-ERRO-ARQUIVO.                                            
           03  FILLER                  PIC  X(008)         VALUE        
               '** ERRO'.                                               
           03  WRK-OPERACAO            PIC  X(013)         VALUE SPACES.
           03  FILLER                  PIC  X(012)         VALUE        
               ' DO ARQUIVO'.                                           
           03  WRK-NOME-ARQUIVO        PIC  X(008)         VALUE SPACES.
           03  FILLER                  PIC  X(017)         VALUE        
               ' - FILE STATUS ='.                                      
           03  WRK-FILE-STATUS         PIC  X(002)         VALUE SPACES.
           03  FILLER                  PIC  X(003)         VALUE ' **'. 
                                                                        
           EJECT                                                        
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE        
           '*** AREA DO LAYOUT DO RELNENCO ***'.                        
      *----------------------------------------------------------------*
                                                                        
       01  CABEC1.                                                      
           03  FILLER                  PIC  X(001)         VALUE '1'.   
           03  FILLER                  PIC  X(044)         VALUE        
               'RDAB0028'.                                              
           03  FILLER                  PIC  X(011)         VALUE        
               'B A N C O'.                                             
           03  FILLER                  PIC  X(017)         VALUE        
               'B R A D E S C O'.                                       
           03  FILLER                  PIC  X(044)         VALUE        
               'S / A'.                                                 
           03  FILLER                  PIC  X(008)         VALUE        
               'PAGINA: '.                                              
           03  CB1-PAGINA              PIC  ZZZ.ZZ9.                    
                                                                        
       01  CABEC2.                                                      
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  CB2-DIA                 PIC  9(002)         VALUE ZEROS. 
           03  FILLER                  PIC  X(001)         VALUE '/'.   
           03  CB2-MES                 PIC  9(002)         VALUE ZEROS. 
           03  FILLER                  PIC  X(001)         VALUE '/'.   
           03  CB2-ANO                 PIC  9(004)         VALUE ZEROS. 
           03  FILLER                  PIC  X(015)         VALUE SPACES.
           03  FILLER                  PIC  X(054)         VALUE        
               'RELATORIO DE REGISTROS NAO ENCONTRADOS NO ARQUIVO IMAG'.
           03  FILLER                  PIC  X(047)         VALUE        
               'EM DA TABELA IRESV001'.                                 
           03  CB2-HORA                PIC  9(002)         VALUE ZEROS. 
           03  FILLER                  PIC  X(001)         VALUE ':'.   
           03  CB2-MINUTO              PIC  9(002)         VALUE ZEROS. 
                                                                        
       01  CABEC3.                                                      
           03  FILLER                  PIC  X(001)         VALUE '-'.   
           03  FILLER                  PIC  X(017)         VALUE        
               '   BANCO'.                                              
           03  FILLER                  PIC  X(016)         VALUE        
               'AGENCIA'.                                               
           03  FILLER                  PIC  X(026)         VALUE        
               'CONTA CORRENTE'.                                        
           03  FILLER                  PIC  X(026)         VALUE        
               'CARTEIRA'.                                              
           03  FILLER                  PIC  X(032)         VALUE        
               'CONTRATO'.                                              
           03  FILLER                  PIC  X(009)         VALUE        
               'DOCUMENTO'.                                             
                                                                        
       01  CABEC4.                                                      
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(017)         VALUE        
               '   ====='.                                              
           03  FILLER                  PIC  X(016)         VALUE        
               '======='.                                               
           03  FILLER                  PIC  X(026)         VALUE        
               '=============='.                                        
           03  FILLER                  PIC  X(022)         VALUE        
               '========'.                                              
           03  FILLER                  PIC  X(030)         VALUE        
               '================='.                                     
           03  FILLER                  PIC  X(020)         VALUE        
               ALL '='.                                                 
                                                                        
       01  LINDET1.                                                     
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(004)         VALUE SPACES.
           03  LD1-BANCO               PIC  9(003)         VALUE ZEROS. 
           03  FILLER                  PIC  X(011)         VALUE SPACES.
           03  LD1-AGENCIA             PIC  9(005)         VALUE ZEROS. 
           03  FILLER                  PIC  X(011)         VALUE SPACES.
           03  LD1-CONTA               PIC  ZZZZZZZZZZZZ9.              
           03  FILLER                  PIC  X(013)         VALUE SPACES.
           03  LD1-CARTEIRA.                                            
               05  LD1-CARTEIRA-N      PIC  9(005)         VALUE ZEROS. 
           03  FILLER                  PIC  X(016)         VALUE SPACES.
           03  LD1-CONTRATO            PIC  ZZZZZZZZZZZZZZZZ9.          
           03  FILLER                  PIC  X(013)         VALUE SPACES.
           03  LD1-DOCUMENTO           PIC  X(020)         VALUE SPACES.
                                                                        
           EJECT                                                        
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE        
           '*** AREA DO LAYOUT DO RELTOTAL ***'.                        
      *----------------------------------------------------------------*
                                                                        
       01  CABEC1-RELTOTAL.                                             
           03  FILLER                  PIC  X(001)         VALUE '1'.   
           03  FILLER                  PIC  X(020)         VALUE        
               'RDAB0028'.                                              
           03  FILLER                  PIC  X(011)         VALUE        
               'B A N C O'.                                             
           03  FILLER                  PIC  X(017)         VALUE        
               'B R A D E S C O'.                                       
           03  FILLER                  PIC  X(021)         VALUE        
               'S / A'.                                                 
           03  CB1-DIA-RELTOTAL        PIC  9(002)         VALUE ZEROS. 
           03  FILLER                  PIC  X(001)         VALUE '/'.   
           03  CB1-MES-RELTOTAL        PIC  9(002)         VALUE ZEROS. 
           03  FILLER                  PIC  X(001)         VALUE '/'.   
           03  CB1-ANO-RELTOTAL        PIC  9(004)         VALUE ZEROS. 
                                                                        
       01  CABEC2-RELTOTAL.                                             
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(025)         VALUE SPACES.
           03  FILLER                  PIC  X(025)         VALUE        
               'RELATORIO DE TOTALIZACOES'.                             
                                                                        
       01  LINTOT1-RELTOTAL.                                            
           03  FILLER                  PIC  X(001)         VALUE '0'.   
           03  FILLER                  PIC  X(009)         VALUE SPACES.
           03  FILLER                  PIC  X(033)         VALUE        
               'QUANTIDADE DE REGISTROS LIDOS'.                         
           03  FILLER                  PIC  X(015)         VALUE        
               'DO ARQPARVV..: '.                                       
           03  LT1-ARQPARVV-RELTOTAL   PIC  ZZZ.ZZZ.ZZ9.                
                                                                        
       01  LINTOT2-RELTOTAL.                                            
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(009)         VALUE SPACES.
           03  FILLER                  PIC  X(033)         VALUE        
               'QUANTIDADE DE REGISTROS LIDOS'.                         
           03  FILLER                  PIC  X(015)         VALUE        
               'DO ARQUIRES..: '.                                       
           03  LT2-ARQUIRES-RELTOTAL   PIC  ZZZ.ZZZ.ZZ9.                
                                                                        
       01  LINTOT3-RELTOTAL.                                            
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(009)         VALUE SPACES.
           03  FILLER                  PIC  X(048)         VALUE        
               'QUANTIDADE DE REGISTROS GRAVADOS NO ARQDAPES..: '.      
           03  LT3-ARQDAPES-RELTOTAL   PIC  ZZZ.ZZZ.ZZ9.                
                                                                        
           EJECT                                                        
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE        
           '*** AREA DA POOL7100 ***'.                                  
      *----------------------------------------------------------------*
                                                                        
-INC POL7100C                                                           
                                                                        
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE        
           '*** RDAB0028 - FIM DA AREA DE WORKING ***'.                 
      *----------------------------------------------------------------*
           EJECT                                                        
      *================================================================*
       PROCEDURE                       DIVISION.                        
      *================================================================*
                                                                        
      *----------------------------------------------------------------*
       000000-INICIAR                  SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           PERFORM 100000-INICIALIZAR.                                  
                                                                        
           PERFORM 200000-VERIFICAR-VAZIO.                              
                                                                        
           PERFORM 300000-PROCESSAR    UNTIL                            
                   WRK-CHV-ARQPARVV    EQUAL HIGH-VALUES.               
                                                                        
           PERFORM 400000-FINALIZAR.                                    
                                                                        
      *----------------------------------------------------------------*
       000000-99-FIM.                  EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       100000-INICIALIZAR              SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           OPEN INPUT  ARQUIRES                                         
                       ARQPARVV                                         
                OUTPUT ARQDAPES                                         
                       RELNENCO                                         
                       RELTOTAL.                                        
                                                                        
           MOVE WRK-ABERTURA           TO WRK-OPERACAO.                 
                                                                        
           PERFORM 110000-TESTAR-FILE-STATUS.                           
                                                                        
           PERFORM 120000-OBTER-DATA-HORA.                              
                                                                        
      *----------------------------------------------------------------*
       100000-99-FIM.                  EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       110000-TESTAR-FILE-STATUS       SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           PERFORM 111000-TESTAR-FS-ARQPARVV.                           
                                                                        
           PERFORM 112000-TESTAR-FS-ARQUIRES.                           
                                                                        
           PERFORM 113000-TESTAR-FS-ARQDAPES.                           
                                                                        
           PERFORM 114000-TESTAR-FS-RELNENCO.                           
                                                                        
           PERFORM 115000-TESTAR-FS-RELTOTAL.                           
                                                                        
      *----------------------------------------------------------------*
       110000-99-FIM.                  EXIT.                            
      *----------------------------------------------------------------*
           EJECT                                                        
      *----------------------------------------------------------------*
       111000-TESTAR-FS-ARQPARVV       SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           IF  WRK-FS-ARQPARVV         NOT EQUAL ZEROS                  
               MOVE WRK-FS-ARQPARVV    TO WRK-FILE-STATUS               
               MOVE 'ARQPARVV'         TO WRK-NOME-ARQUIVO              
               MOVE WRK-ERRO-ARQUIVO   TO ERR-TEXTO                     
               PERFORM 999999-PROCESSAR-ROTINA-ERRO                     
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       111000-99-FIM.                  EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       112000-TESTAR-FS-ARQUIRES       SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           IF  WRK-FS-ARQUIRES         NOT EQUAL ZEROS                  
               MOVE WRK-FS-ARQUIRES    TO WRK-FILE-STATUS               
               MOVE 'ARQUIRES'         TO WRK-NOME-ARQUIVO              
               MOVE WRK-ERRO-ARQUIVO   TO ERR-TEXTO                     
               PERFORM 999999-PROCESSAR-ROTINA-ERRO                     
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       112000-99-FIM.                  EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       113000-TESTAR-FS-ARQDAPES       SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           IF  WRK-FS-ARQDAPES         NOT EQUAL ZEROS                  
               MOVE WRK-FS-ARQDAPES    TO WRK-FILE-STATUS               
               MOVE 'ARQDAPES'         TO WRK-NOME-ARQUIVO              
               MOVE WRK-ERRO-ARQUIVO   TO ERR-TEXTO                     
               PERFORM 999999-PROCESSAR-ROTINA-ERRO                     
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       113000-99-FIM.                  EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       114000-TESTAR-FS-RELNENCO       SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           IF  WRK-FS-RELNENCO         NOT EQUAL ZEROS                  
               MOVE WRK-FS-RELNENCO    TO WRK-FILE-STATUS               
               MOVE 'RELNENCO'         TO WRK-NOME-ARQUIVO              
               MOVE WRK-ERRO-ARQUIVO   TO ERR-TEXTO                     
               PERFORM 999999-PROCESSAR-ROTINA-ERRO                     
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       114000-99-FIM.                  EXIT.                            
      *----------------------------------------------------------------*
           EJECT                                                        
      *----------------------------------------------------------------*
       115000-TESTAR-FS-RELTOTAL       SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           IF  WRK-FS-RELTOTAL         NOT EQUAL ZEROS                  
               MOVE WRK-FS-RELTOTAL    TO WRK-FILE-STATUS               
               MOVE 'RELTOTAL'         TO WRK-NOME-ARQUIVO              
               MOVE WRK-ERRO-ARQUIVO   TO ERR-TEXTO                     
               PERFORM 999999-PROCESSAR-ROTINA-ERRO                     
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       115000-99-FIM.                  EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       120000-OBTER-DATA-HORA          SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           CALL 'POOL7600'             USING WRK-DATA-HORA.             
                                                                        
           MOVE WRK-DIA                TO CB2-DIA                       
                                          CB1-DIA-RELTOTAL.             
           MOVE WRK-MES                TO CB2-MES                       
                                          CB1-MES-RELTOTAL.             
           MOVE WRK-ANO                TO CB2-ANO                       
                                          CB1-ANO-RELTOTAL.             
           MOVE WRK-HORA               TO CB2-HORA.                     
           MOVE WRK-MINUTO             TO CB2-MINUTO.                   
                                                                        
      *----------------------------------------------------------------*
       120000-99-FIM.                  EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       200000-VERIFICAR-VAZIO          SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           PERFORM 210000-LER-ARQPARVV.                                 
                                                                        
           IF  WRK-CHV-ARQPARVV        EQUAL HIGH-VALUES                
               DISPLAY '***************** RDAB0028 *****************'   
               DISPLAY '*                                          *'   
               DISPLAY '*          ARQUIVO ARQPARVV VAZIO          *'   
               DISPLAY '*                                          *'   
               DISPLAY '*         PROCESSAMENTO  ENCERRADO         *'   
               DISPLAY '*                                          *'   
               DISPLAY '***************** RDAB0028 *****************'   
               PERFORM 400000-FINALIZAR                                 
           END-IF.                                                      
                                                                        
           PERFORM 220000-LER-ARQUIRES.                                 
                                                                        
           IF  WRK-CHV-ARQUIRES        EQUAL HIGH-VALUES                
               DISPLAY '***************** RDAB0028 *****************'   
               DISPLAY '*                                          *'   
               DISPLAY '*          ARQUIVO ARQUIRES VAZIO          *'   
               DISPLAY '*                                          *'   
               DISPLAY '***************** RDAB0028 *****************'   
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       200000-99-FIM.                  EXIT.                            
      *----------------------------------------------------------------*
           EJECT                                                        
      *----------------------------------------------------------------*
       210000-LER-ARQPARVV             SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           READ ARQPARVV.                                               
                                                                        
           IF  WRK-FS-ARQPARVV         EQUAL '10'                       
               MOVE HIGH-VALUES        TO WRK-CHV-ARQPARVV              
               GO TO 210000-99-FIM                                      
           END-IF.                                                      
                                                                        
           MOVE WRK-LEITURA            TO WRK-OPERACAO.                 
                                                                        
           PERFORM 111000-TESTAR-FS-ARQPARVV.                           
                                                                        
           MOVE PVV-CCLUB              TO WRK-CCLUB-ARQPARVV.
                                                                        
           ADD 1                       TO ACU-LIDOS-ARQPARVV.           
                                                                        
      *----------------------------------------------------------------*
       210000-99-FIM.                  EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       220000-LER-ARQUIRES             SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           READ ARQUIRES.                                               
                                                                        
           IF  WRK-FS-ARQUIRES         EQUAL '10'                       
               MOVE HIGH-VALUES        TO WRK-CHV-ARQUIRES              
               GO TO 220000-99-FIM                                      
           END-IF.                                                      
                                                                        
           MOVE WRK-LEITURA            TO WRK-OPERACAO.                 
                                                                        
           PERFORM 112000-TESTAR-FS-ARQUIRES.                           
                                                                        
           MOVE CADUV001-CCLUB         TO WRK-CCLUB-ARQUIRES.
                                                                        
           ADD 1                       TO ACU-LIDOS-ARQUIRES.           
                                                                        
      *----------------------------------------------------------------*
       220000-99-FIM.                  EXIT.                            
      *----------------------------------------------------------------*
           EJECT                                                        
      *----------------------------------------------------------------*
       300000-PROCESSAR                SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           IF  WRK-CHV-ARQPARVV           GREATER WRK-CHV-ARQUIRES      
               PERFORM 220000-LER-ARQUIRES                              
           ELSE                                                         
               MOVE WRK-GRAVACAO          TO WRK-OPERACAO               
               INITIALIZE ADP-REGISTRO                                  
               IF  WRK-CHV-ARQPARVV       LESS WRK-CHV-ARQUIRES         
                   PERFORM 310000-EMITIR-RELNENCO                       
                   PERFORM 320000-GRAVAR-ARQDAPES                       
                   PERFORM 210000-LER-ARQPARVV                          
               ELSE
                   IF CADUV001-ICONJG-PSSOA NOT EQUAL SPACES
                       MOVE CADUV001-ICONJG-PSSOA(1:40) TO ICONJG-CLI
                       PERFORM 320000-GRAVAR-ARQDAPES
                   END-IF
                   PERFORM 210000-LER-ARQPARVV                          
                   PERFORM 220000-LER-ARQUIRES                          
               END-IF                                                   
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       300000-99-FIM.                  EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       310000-EMITIR-RELNENCO          SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           IF  ACU-LINHAS              GREATER 59                       
               PERFORM 311000-EMITIR-CABECALHOS                         
           END-IF.                                                      
                                                                        
           MOVE PVV-BANCO              TO LD1-BANCO.                    
           MOVE PVV-AGENCIA            TO LD1-AGENCIA.                  
           MOVE PVV-CONTA              TO LD1-CONTA.                    
           IF   PVV-CARTEIRA    NOT       NUMERIC                       
                MOVE  PVV-CARTEIRA-R   TO LD1-CARTEIRA                  
           ELSE                                                         
                MOVE  PVV-CARTEIRA     TO LD1-CARTEIRA-N.               
           MOVE PVV-CONTRATO           TO LD1-CONTRATO.                 
                                                                        
           IF  PVV-CGCFIL              EQUAL ZEROS                      
               MOVE PVV-CGCNUM         TO WRK-CPF-1                     
               MOVE PVV-CGCCTR         TO WRK-CONTROLE-1                
               MOVE WRK-CGC-CPF-1      TO LD1-DOCUMENTO                 
           ELSE                                                         
               MOVE PVV-CGCNUM         TO WRK-CPF                       
               MOVE PVV-CGCFIL         TO WRK-FILIAL                    
               MOVE PVV-CGCCTR         TO WRK-CONTROLE                  
               MOVE WRK-CGC-CPF        TO LD1-DOCUMENTO                 
           END-IF.                                                      
                                                                        
           WRITE REG-RELNENCO          FROM LINDET1.                    
                                                                        
           PERFORM 114000-TESTAR-FS-RELNENCO.                           
                                                                        
           ADD 1                       TO ACU-LINHAS                    
                                          ACU-IMPRE-RELNENCO.           
                                                                        
      *----------------------------------------------------------------*
       310000-99-FIM.                  EXIT.                            
      *----------------------------------------------------------------*
           EJECT                                                        
      *----------------------------------------------------------------*
       311000-EMITIR-CABECALHOS        SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           ADD 1                       TO ACU-PAG.                      
           MOVE ACU-PAG                TO CB1-PAGINA.                   
                                                                        
           WRITE REG-RELNENCO          FROM CABEC1.                     
                                                                        
           PERFORM 114000-TESTAR-FS-RELNENCO.                           
                                                                        
           WRITE REG-RELNENCO          FROM CABEC2.                     
                                                                        
           PERFORM 114000-TESTAR-FS-RELNENCO.                           
                                                                        
           WRITE REG-RELNENCO          FROM CABEC3.                     
                                                                        
           PERFORM 114000-TESTAR-FS-RELNENCO.                           
                                                                        
           WRITE REG-RELNENCO          FROM CABEC4.                     
                                                                        
           PERFORM 114000-TESTAR-FS-RELNENCO.                           
                                                                        
           MOVE 6                      TO ACU-LINHAS.                   
                                                                        
      *----------------------------------------------------------------*
       311000-99-FIM.                  EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       320000-GRAVAR-ARQDAPES          SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE PVV-BANCO              TO CBCO.                         
           MOVE PVV-CONTA              TO CCTA-CORR.                    
           MOVE PVV-AGENCIA            TO CAG-BCRIA.                    
           MOVE PVV-CGCNUM             TO CBASE-CPF.                    
           MOVE PVV-CGCFIL             TO CFILIAL-CNPJ.                 
           MOVE PVV-CGCCTR             TO CCTRL-CNPJ-CPF.               
           MOVE PVV-CPSSOA-CADTR       TO CPSSOA-CADTR.                 
           MOVE PVV-TPPESSOA           TO CIDTFD-TPO-PSSOA.             
           MOVE PVV-CPOSTO-SERVC       TO CPOSTO-SERVC.                 
           MOVE '1'                    TO CINDCD-ORIGE.                 
                                                                        
           WRITE ADP-REGISTRO.                                          
                                                                        
           PERFORM 113000-TESTAR-FS-ARQDAPES.                           
                                                                        
           ADD 1                       TO ACU-GRAVA-ARQDAPES.           
                                                                        
      *----------------------------------------------------------------*
       320000-99-FIM.                  EXIT.                            
      *----------------------------------------------------------------*
           EJECT                                                        
      *----------------------------------------------------------------*
       400000-FINALIZAR                SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           PERFORM 410000-EMITIR-DISPLAY.                               
                                                                        
           PERFORM 420000-EMITIR-RELTOTAL.                              
                                                                        
           CLOSE ARQUIRES                                               
                 ARQPARVV                                               
                 ARQDAPES                                               
                 RELNENCO                                               
                 RELTOTAL.                                              
                                                                        
           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.                 
                                                                        
           PERFORM 110000-TESTAR-FILE-STATUS.                           
                                                                        
           GOBACK.                                                      
                                                                        
      *----------------------------------------------------------------*
       400000-99-FIM.                  EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       410000-EMITIR-DISPLAY           SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           DISPLAY '***************** RDAB0028 *****************'.      
           DISPLAY '*                                          *'.      
                                                                        
           MOVE ACU-LIDOS-ARQPARVV     TO WRK-MASCARA.                  
                                                                        
           DISPLAY '*  REG. LIDOS    EM ARQPARVV: ' WRK-MASCARA '  *'.  
                                                                        
           MOVE ACU-LIDOS-ARQUIRES     TO WRK-MASCARA.                  
                                                                        
           DISPLAY '*  REG. LIDOS    EM ARQUIRES: ' WRK-MASCARA '  *'.  
                                                                        
           MOVE ACU-GRAVA-ARQDAPES     TO WRK-MASCARA.                  
                                                                        
           DISPLAY '*  REG. GRAVADOS EM ARQDAPES: ' WRK-MASCARA '  *'.  
                                                                        
           MOVE ACU-IMPRE-RELNENCO     TO WRK-MASCARA.                  
                                                                        
           DISPLAY '*  REG. GRAVADOS EM RELNENCO: ' WRK-MASCARA '  *'.  
           DISPLAY '*                                          *'.      
           DISPLAY '***************** RDAB0028 *****************'.      
                                                                        
      *----------------------------------------------------------------*
       410000-99-FIM.                  EXIT.                            
      *----------------------------------------------------------------*
           EJECT                                                        
      *----------------------------------------------------------------*
       420000-EMITIR-RELTOTAL          SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE ACU-LIDOS-ARQPARVV     TO LT1-ARQPARVV-RELTOTAL.        
           MOVE ACU-LIDOS-ARQUIRES     TO LT2-ARQUIRES-RELTOTAL.        
           MOVE ACU-GRAVA-ARQDAPES     TO LT3-ARQDAPES-RELTOTAL.        
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.                 
                                                                        
           WRITE REG-RELTOTAL          FROM CABEC1-RELTOTAL.            
                                                                        
           PERFORM 115000-TESTAR-FS-RELTOTAL.                           
                                                                        
           WRITE REG-RELTOTAL          FROM CABEC2-RELTOTAL.            
                                                                        
           PERFORM 115000-TESTAR-FS-RELTOTAL.                           
                                                                        
           WRITE REG-RELTOTAL          FROM LINTOT1-RELTOTAL.           
                                                                        
           PERFORM 115000-TESTAR-FS-RELTOTAL.                           
                                                                        
           WRITE REG-RELTOTAL          FROM LINTOT2-RELTOTAL.           
                                                                        
           PERFORM 115000-TESTAR-FS-RELTOTAL.                           
                                                                        
           WRITE REG-RELTOTAL          FROM LINTOT3-RELTOTAL.           
                                                                        
           PERFORM 115000-TESTAR-FS-RELTOTAL.                           
                                                                        
      *----------------------------------------------------------------*
       420000-99-FIM.                  EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       999999-PROCESSAR-ROTINA-ERRO    SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE 'APL'                  TO ERR-TIPO-ACESSO.              
           MOVE 'RDAB0028'             TO ERR-PGM.                      
                                                                        
           CALL 'POOL7100'             USING WRK-BATCH                  
                                             ERRO-AREA.                 
                                                                        
           GOBACK.                                                      
                                                                        
      *----------------------------------------------------------------*
       999999-99-FIM.                  EXIT.                            
      *----------------------------------------------------------------*
                                                                        

      *===============================================================* 
       IDENTIFICATION                  DIVISION.                        
      *===============================================================* 
                                                                        
       PROGRAM-ID. RDAB0259.                                            
       AUTHOR.     ELSINO SILVA.                                        
      *===============================================================* 
      *                    B R Q   I T   S E R V I C E S              * 
      *---------------------------------------------------------------* 
      *    PROGRAMA....:   RDAB0259                                   * 
      *    ANALISTA....:   ROBSON VELLASQUES         - BRQ            * 
      *    PROGRAMADOR.:   ELSINO SILVA              - BRQ            * 
      *    DATA........:   NOVEMBRO/2014                              * 
      *- -------------------------------------------------------------* 
      *    OBJETIVO....:   CORRIGIR TABELA RDABB005 COM O INFORMACOES * 
      *      DO CADASTRO DE TELEFONES.                                * 
      *---------------------------------------------------------------* 
      *    ARQUIVOS....:                                              * 
      *                                                               * 
      *   ---------------------------------------------------------   * 
      *   DDNAME   |I/O| DESCRICAO               | BOOK     | LRECL   * 
      *   ---------+---+-------------------------+----------+------   * 
      *   ARQTEL   | I | CADASTRO DE TELEFONE    | I#RDAB15 |   44    * 
      *   ARQUPDT  | 0 | REGS DE UPDATE NA TABELA| I#RDAB15 |   44    * 
      *   ---------------------------------------------------------   * 
      *                                                               * 
      *---------------------------------------------------------------* 
      *    BCO DE DADOS:                                              * 
      *                DB2                                            * 
      *                TABLE                           INCLUDE/BOOK   * 
      *                    DB2PRD.FONE_BASE_UNIC         RDABB005     * 
      *                                                               * 
      *---------------------------------------------------------------* 
      *    INC'S.......:                                              * 
      *    I#BRAD7C - AREA PARA TRATAMENTO DE ERROS PELA BRAD7100     * 
      *    I#FRWKGE - COMMAREA FRWK2999 (LOG DE ERRO).                * 
      *    I#FRWKMD - COMMAREA FRWK2999 (LOG DE ERROS MODULO).        * 
      *    I#CKRS01 - AREA PARA MODULO DE CHECKPOINT / RESTART.       * 
      *                                                               * 
      *---------------------------------------------------------------* 
      *    MODULOS.....:                                              * 
      *    POOL1050 - FAZ CONEXAO COM DB2                             * 
      *    BRAD7100 - TRATAMENTO DE ERROS                             * 
      *    POOL7300 - BUSCA NOME DO CJOB                              * 
      *    POOL7600 - OBTEM DATA E HORA DO SISTEMA                    * 
      *===============================================================* 
                                                                        
      *===============================================================* 
       ENVIRONMENT                     DIVISION.                        
      *===============================================================* 
                                                                        
      *---------------------------------------------------------------* 
       CONFIGURATION                   SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
       SPECIAL-NAMES.                                                   
           DECIMAL-POINT               IS COMMA.                        
                                                                        
      *---------------------------------------------------------------* 
       INPUT-OUTPUT                    SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
       FILE-CONTROL.                                                    
                                                                        
           SELECT ARQTEL ASSIGN        TO UT-S-ARQTEL                   
           FILE STATUS                 IS WRK-FS-ARQTEL.                
                                                                        
           SELECT ARQUPDT ASSIGN       TO UT-S-ARQUPDT                  
           FILE STATUS                 IS WRK-FS-ARQUPDT.               
                                                                        
      *===============================================================* 
       DATA                            DIVISION.                        
      *===============================================================* 
                                                                        
      *---------------------------------------------------------------* 
       FILE                            SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
      *    INPUT:  CADASTRO DE TELEFONES PARA CORRIGIR BASE RDABB005  * 
      *            ORG. SEQUENCIAL     -    LRECL   =   044           * 
      *---------------------------------------------------------------* 
                                                                        
       FD  ARQTEL                                                       
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  FD-REG-ARQTEL               PIC X(044).                      
                                                                        
                                                                        
      *---------------------------------------------------------------* 
      *   OUTPUT:  REGS. QUE ATUALIZARAM A TABELA RDABB005            * 
      *            ORG. SEQUENCIAL     -    LRECL   =   044           * 
      *---------------------------------------------------------------* 
                                                                        
       FD  ARQUPDT                                                      
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  FD-REG-ARQUPDT              PIC X(044).                      
                                                                        
      *---------------------------------------------------------------* 
       WORKING-STORAGE                 SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       77  FILLER                      PIC  X(050)         VALUE        
           '*** RDAB0259 - INICIO DA AREA DE WORKING ***'.              
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       77  FILLER                      PIC  X(050)         VALUE        
           '*** ACUMULADORES ***'.                                      
      *---------------------------------------------------------------* 
                                                                        
       77  ACU-LIDOS-ARQTEL         PIC  9(009) COMP-3  VALUE ZEROS.    
       77  ACU-GRAV-ARQUPDT         PIC  9(009) COMP-3  VALUE ZEROS.    
       77  ACU-ATUAL-CKRS01         PIC  9(009) COMP-3  VALUE ZEROS.    
       77  ACU-ATUAL-RDABB005       PIC  9(009) COMP-3  VALUE ZEROS.    
       77  ACU-COMMIT               PIC S9(005) COMP-3  VALUE ZEROS.    
                                                                        
      *---------------------------------------------------------------* 
       77  FILLER                      PIC  X(050)         VALUE        
           '*** AREA PARA TESTE DE FILE-STATUS ***'.                    
      *---------------------------------------------------------------* 
                                                                        
       77  WRK-FS-ARQTEL               PIC  X(002)         VALUE SPACES.
       77  WRK-FS-ARQUPDT              PIC  X(002)         VALUE SPACES.
                                                                        
      *---------------------------------------------------------------* 
       77  FILLER                      PIC  X(050)         VALUE        
           '*** VARIAVEIS AUXILIARES ***'.                              
      *---------------------------------------------------------------* 
                                                                        
       77  WRK-MASCARA                 PIC  ZZZ.ZZZ.ZZ9 VALUE ZEROS.    
       77  WRK-BATCH                   PIC  X(008)         VALUE        
           'BATCH'.                                                     
                                                                        
       77  WRK-ABERTURA                PIC  X(013)         VALUE        
           ' NA ABERTURA '.                                             
                                                                        
       77  WRK-LEITURA                 PIC  X(013)         VALUE        
           ' NA  LEITURA '.                                             
                                                                        
       77  WRK-GRAVACAO                PIC  X(013)         VALUE        
           ' NA GRAVACAO '.                                             
                                                                        
       77  WRK-FECHAMENTO              PIC  X(013)         VALUE        
           'NO FECHAMENTO'.                                             
                                                                        
       77  WRK-CKRS0100                PIC  X(008)         VALUE        
           'CKRS0100'.                                                  
                                                                        
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE        
           '*** AREA PARA CHECKPOINT/RESTART ***'.                      
      *----------------------------------------------------------------*
                                                                        
       COPY 'I#CKRS01'.                                                 
                                                                        
       01  WRK-AREA-RESTART.                                            
           05  WRK-LIDOS-ARQTEL         PIC  9(009) COMP-3  VALUE ZEROS.
           05  WRK-GRAV-ARQUPDT         PIC  9(009) COMP-3  VALUE ZEROS.
           05  WRK-ATUAL-CKRS01         PIC  9(009) COMP-3  VALUE ZEROS.
           05  WRK-ATUAL-RDABB005       PIC  9(009) COMP-3  VALUE ZEROS.
           05  WRK-COMMIT               PIC S9(005) COMP-3  VALUE ZEROS.
                                                                        
                                                                        
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE        
           '*** AREA DE COMUNICACAO DO PROGRAMA FRWK2999 ***'.          
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE        
           '*** AREA DE TRATAMENTO DE ERROS DE ARQUIVO ***'.            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE        
           '*** AREA DE TRATAMENTO DE ERROS DE MODULO ***'.             
      *----------------------------------------------------------------*
                                                                        
       01  WRK-AREA-ERRO-MOD.                                           
           COPY 'I#FRWKMD'.                                             
                                                                        
      *---------------------------------------------------------------* 
       01  FILLER                      PIC  X(050)         VALUE        
           '*** AREA DE MENSAGENS ***'.                                 
      *---------------------------------------------------------------* 
                                                                        
       01  WRK-ERRO-ARQUIVO.                                            
           03  FILLER                  PIC  X(008)         VALUE        
               '** ERRO '.                                              
           03  WRK-OPERACAO            PIC  X(013)         VALUE SPACES.
           03  FILLER                  PIC  X(012)         VALUE        
               ' DO ARQUIVO '.                                          
           03  WRK-ARQ-NOME            PIC  X(008)         VALUE SPACES.
           03  FILLER                  PIC  X(017)         VALUE        
               ' - FILE STATUS = '.                                     
           03  WRK-FILE-STATUS         PIC  X(002)         VALUE SPACES.
           03  FILLER                  PIC  X(003)         VALUE        
               ' **'.                                                   
                                                                        
      *---------------------------------------------------------------* 
       01  FILLER                      PIC  X(050)         VALUE        
           '*** AREA DE CHAVES ***'.                                    
      *---------------------------------------------------------------* 
                                                                        
       01  WRK-CH-ARQTEL.                                               
           05  WRK-CHV-BANCO           PIC  9(003) VALUE ZEROS.         
           05  WRK-CHV-AGENC           PIC  9(005) VALUE ZEROS.         
           05  WRK-CHV-CONTA           PIC  9(013) VALUE ZEROS.         
           05  WRK-CHV-ORIGE-FONE      PIC  X(001) VALUE SPACES.        
           05  WRK-CHV-TPO-FONE        PIC  X(001) VALUE SPACES.        
           05  WRK-CHV-CSEQ-FONE       PIC  9(002) VALUE ZEROS.         
                                                                        
       01  WRK-TEMP-ARQTEL.                                             
           05  WRK-TEMP-BANCO          PIC S9(003) VALUE ZEROS.         
           05  WRK-AUX-BANCO           REDEFINES   WRK-TEMP-BANCO       
                                       PIC  9(003).                     
           05  WRK-TEMP-AGENC          PIC S9(005) VALUE ZEROS.         
           05  WRK-AUX-AGENC           REDEFINES   WRK-TEMP-AGENC       
                                       PIC  9(005).                     
           05  WRK-TEMP-CONTA          PIC S9(013) VALUE ZEROS.         
           05  WRK-AUX-CONTA           REDEFINES   WRK-TEMP-CONTA       
                                       PIC  9(013).                     
           05  WRK-TEMP-CSEQ-FONE      PIC S9(002) VALUE ZEROS.         
           05  WRK-AUX-CSEQ-FONE       REDEFINES   WRK-TEMP-CSEQ-FONE   
                                       PIC  9(002).                     
                                                                        
       01  WRK-CINDCD-FONE             PIC  X(001) VALUE '2'.           
                                                                        
      *---------------------------------------------------------------* 
       01  FILLER                      PIC  X(032)         VALUE        
           '*       AREA DE BOOKS          *'.                          
      *----------------------------------------------------------------*
                                                                        
       COPY 'I#RDAB15'.                                                 
                                                                        
      *---------------------------------------------------------------* 
       01  FILLER                      PIC  X(050)         VALUE        
           '*** AREA DA BRAD7100 ***'.                                  
      *---------------------------------------------------------------* 
                                                                        
       COPY 'I#BRAD7C'.                                                 
                                                                        
      *---------------------------------------------------------------* 
       01  FILLER                      PIC  X(050)         VALUE        
           '*** AREA DE DB2 ***'.                                       
      *---------------------------------------------------------------* 
                                                                        
           EXEC SQL                                                     
             INCLUDE SQLCA                                              
           END-EXEC.                                                    
                                                                        
           EXEC SQL                                                     
             INCLUDE RDABB005                                           
           END-EXEC.                                                    
                                                                        
      *---------------------------------------------------------------* 
       01  FILLER                      PIC  X(050)         VALUE        
           '*** RDAB0259 - FIM DA AREA DE WORKING ***'.                 
      *---------------------------------------------------------------* 
      *===============================================================* 
       PROCEDURE                       DIVISION.                        
      *===============================================================* 
                                                                        
      *---------------------------------------------------------------* 
       0000-INICIAR                    SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           PERFORM 0500-INICIALIZAR-CHECK-RESTART                       
                                                                        
           IF  CK01-RESTART                                             
               DISPLAY '***************** RDAB0259 ****************'    
               DISPLAY '*                                         *'    
               DISPLAY '*        PROCESSAMENTO COM RESTART        *'    
               DISPLAY '*                                         *'    
               DISPLAY '***************** RDAB0259 ****************'    
               MOVE CK01-AREA-RESTART(1: LENGTH OF WRK-AREA-RESTART)    
                                       TO WRK-AREA-RESTART              
                                                                        
                                                                        
               MOVE WRK-LIDOS-ARQTEL   TO ACU-LIDOS-ARQTEL              
               MOVE WRK-GRAV-ARQUPDT   TO ACU-GRAV-ARQUPDT              
               MOVE WRK-ATUAL-CKRS01 TO ACU-ATUAL-CKRS01                
               MOVE WRK-ATUAL-RDABB005 TO ACU-ATUAL-RDABB005            
               MOVE WRK-COMMIT         TO ACU-COMMIT                    
           ELSE                                                         
               DISPLAY '***************** RDAB0259 ****************'    
               DISPLAY '*                                         *'    
               DISPLAY '*   PROCESSAMENTO NORMAL (SEM RESTART)    *'    
               DISPLAY '*                                         *'    
               DISPLAY '***************** RDAB0259 ****************'    
           END-IF.                                                      
                                                                        
                                                                        
           PERFORM 1000-INICIALIZAR.                                    
                                                                        
           PERFORM 2000-VERIFICAR-VAZIO.                                
                                                                        
           PERFORM 4000-PROCESSAR      UNTIL                            
                   WRK-FS-ARQTEL       EQUAL '10'.                      
                                                                        
           PERFORM 6000-FINALIZAR.                                      
                                                                        
      *---------------------------------------------------------------* 
       0000-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *----------------------------------------------------------------*
       0500-INICIALIZAR-CHECK-RESTART  SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           CALL 'CKRS1000'.                                             
                                                                        
           INITIALIZE                  CKRS01-INTERFACE.                
                                                                        
           MOVE SPACES                 TO CK01-PLAN.                    
           MOVE 'DB2'                  TO CK01-ID-DB2.                  
           MOVE 'I'                    TO CK01-FUNCAO.                  
                                                                        
           MOVE LENGTH                 OF WRK-AREA-RESTART              
                                       TO CK01-TAM-AREA-RESTART.        
           MOVE WRK-AREA-RESTART       TO CK01-AREA-RESTART             
                                       (1:LENGTH OF WRK-AREA-RESTART).  
           PERFORM 0510-CHAMAR-CHECKPOINT-RESTART.                      
                                                                        
      *----------------------------------------------------------------*
       0500-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      ******************************************************************
      * CHAMAR CHECKPOINT RESTART.                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       0510-CHAMAR-CHECKPOINT-RESTART  SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           CALL WRK-CKRS0100           USING CKRS01-INTERFACE.          
                                                                        
           IF  CK01-CODIGO-RETORNO     NOT EQUAL ZEROS                  
               MOVE 'CKRS0100'         TO FRWKGMOD-NOME-MODULO          
               MOVE CK01-CODIGO-RETORNO                                 
                                       TO FRWKGMOD-COD-RETORNO          
               MOVE SPACES             TO FRWKGMOD-COD-ERRO             
               MOVE CK01-CODIGO-MENSAGEM                                
                                       TO FRWKGMOD-COD-MENSAGEM         
               DISPLAY ' '                                              
                       ' INTERFACE = ' CKRS01-INTERFACE                 
               PERFORM 9999-PROCESSAR-ROTINA-ERRO                       
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       0510-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *---------------------------------------------------------------* 
       1000-INICIALIZAR                SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           OPEN INPUT  ARQTEL                                           
               OUTPUT  ARQUPDT.                                         
                                                                        
           MOVE WRK-ABERTURA           TO WRK-OPERACAO.                 
                                                                        
           PERFORM 1100-TESTAR-FS-ARQTEL.                               
           PERFORM 1200-TESTAR-FS-ARQUPDT.                              
                                                                        
      *---------------------------------------------------------------* 
       1000-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1100-TESTAR-FS-ARQTEL           SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           IF  WRK-FS-ARQTEL           NOT EQUAL '00'                   
               MOVE 'APL'              TO ERR-TIPO-ACESSO               
               MOVE 'ARQTEL'           TO WRK-ARQ-NOME                  
               MOVE  WRK-FS-ARQTEL     TO WRK-FILE-STATUS               
               MOVE  WRK-ERRO-ARQUIVO  TO ERR-TEXTO                     
               PERFORM 9999-PROCESSAR-ROTINA-ERRO                       
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       1100-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1200-TESTAR-FS-ARQUPDT          SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           IF  WRK-FS-ARQUPDT          NOT EQUAL '00'                   
               MOVE 'APL'              TO ERR-TIPO-ACESSO               
               MOVE 'ARQUPDT'          TO WRK-ARQ-NOME                  
               MOVE  WRK-FS-ARQUPDT    TO WRK-FILE-STATUS               
               MOVE  WRK-ERRO-ARQUIVO  TO ERR-TEXTO                     
               PERFORM 9999-PROCESSAR-ROTINA-ERRO                       
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       1200-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       2000-VERIFICAR-VAZIO            SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           PERFORM 2100-LER-ARQTEL.                                     
                                                                        
           IF  WRK-FS-ARQTEL           EQUAL '10'                       
               DISPLAY '***************** RDAB0259 *****************'   
               DISPLAY '*                                          *'   
               DISPLAY '*          ARQUIVO ARQTEL VAZIO            *'   
               DISPLAY '*                                          *'   
               DISPLAY '*         PROCESSAMENTO  ENCERRADO         *'   
               DISPLAY '*                                          *'   
               DISPLAY '***************** RDAB0259 *****************'   
               PERFORM 6000-FINALIZAR                                   
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       2000-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       2100-LER-ARQTEL                 SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           READ ARQTEL                 INTO  AFN-REG                    
                                                                        
           IF  WRK-FS-ARQTEL           EQUAL '10'                       
               MOVE HIGH-VALUES        TO WRK-CH-ARQTEL                 
               GO  TO  2100-99-FIM                                      
           END-IF.                                                      
                                                                        
           MOVE WRK-LEITURA            TO WRK-OPERACAO.                 
                                                                        
           PERFORM 1100-TESTAR-FS-ARQTEL.                               
                                                                        
           MOVE AFN-CBCO               TO  WRK-TEMP-BANCO               
           MOVE WRK-AUX-BANCO          TO  WRK-CHV-BANCO                
           MOVE AFN-CAG-BCRIA          TO  WRK-TEMP-AGENC               
           MOVE WRK-AUX-AGENC          TO  WRK-CHV-AGENC                
           MOVE AFN-CCTA-CORR          TO  WRK-TEMP-CONTA               
           MOVE WRK-AUX-CONTA          TO  WRK-CHV-CONTA                
           MOVE AFN-CINDCD-ORIGE-FONE  TO  WRK-CHV-ORIGE-FONE           
           MOVE AFN-CINDCD-TPO-FONE    TO  WRK-CHV-TPO-FONE             
           MOVE AFN-CSEQ-FONE          TO  WRK-TEMP-CSEQ-FONE           
           MOVE WRK-AUX-CSEQ-FONE      TO  WRK-CHV-CSEQ-FONE            
                                                                        
           ADD 1                       TO ACU-LIDOS-ARQTEL.             
                                                                        
      *---------------------------------------------------------------* 
       2100-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       4000-PROCESSAR                  SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           PERFORM 4100-ATUALIZAR-RDABB005                              
           PERFORM 8000-EFETIVAR-CHECK-RESTART                          
           PERFORM 2100-LER-ARQTEL.                                     
                                                                        
      *---------------------------------------------------------------* 
       4000-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       4100-ATUALIZAR-RDABB005         SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           MOVE AFN-CBCO               TO CBCO                          
           MOVE AFN-CAG-BCRIA          TO CAG-BCRIA                     
           MOVE AFN-CCTA-CORR          TO CCTA-BCRIA-CLI                
           MOVE AFN-CINDCD-ORIGE-FONE  TO CINDCD-ORIGE-FONE             
           MOVE AFN-CINDCD-TPO-FONE    TO CINDCD-TPO-FONE               
           MOVE AFN-CSEQ-FONE          TO CSEQ-FONE-BASE                
           MOVE WRK-CINDCD-FONE        TO CINDCD-FONE-INCOR             
                                                                        
           EXEC SQL                                                     
             UPDATE DB2PRD.FONE_BASE_UNIC                               
               SET                                                      
                   CINDCD_FONE_INCOR = :RDABB005.CINDCD-FONE-INCOR      
             WHERE                                                      
                   CBCO              = :RDABB005.CBCO                   
              AND  CAG_BCRIA         = :RDABB005.CAG-BCRIA              
              AND  CCTA_BCRIA_CLI    = :RDABB005.CCTA-BCRIA-CLI         
              AND  CINDCD_ORIGE_FONE = :RDABB005.CINDCD-ORIGE-FONE      
              AND  CINDCD_TPO_FONE   = :RDABB005.CINDCD-TPO-FONE        
              AND  CSEQ_FONE_BASE    = :RDABB005.CSEQ-FONE-BASE         
           END-EXEC.                                                    
                                                                        
           IF (SQLCODE               NOT  EQUAL +100 )                  
                                                                        
              IF (SQLCODE              NOT EQUAL ZEROS) OR              
                 (SQLWARN0             EQUAL 'W'      )                 
                  MOVE 'DB2'           TO ERR-TIPO-ACESSO               
                  MOVE 'FONE_BASE_UNIC' TO ERR-DBD-TAB                  
                  MOVE 'UPDATE'        TO ERR-FUN-COMANDO               
                  MOVE  SQLCODE        TO ERR-SQL-CODE                  
                  MOVE '0011'          TO ERR-LOCAL                     
                  MOVE  SPACES         TO ERR-SEGM                      
                  PERFORM 9999-PROCESSAR-ROTINA-ERRO                    
              END-IF                                                    
                                                                        
              ADD  1                   TO ACU-ATUAL-RDABB005            
                                          ACU-COMMIT                    
                                                                        
              PERFORM 4200-GRAVAR-ARQUPDT                               
                                                                        
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       4100-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       4200-GRAVAR-ARQUPDT             SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           WRITE  FD-REG-ARQUPDT       FROM    AFN-REG                  
           MOVE   WRK-GRAVACAO         TO      WRK-OPERACAO             
           PERFORM 1200-TESTAR-FS-ARQUPDT                               
                                                                        
           ADD 1                       TO   ACU-GRAV-ARQUPDT            
                                                                        
           .                                                            
      *---------------------------------------------------------------* 
       4200-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       6000-FINALIZAR                  SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           PERFORM 6100-EMITIR-DISPLAY.                                 
                                                                        
           CLOSE ARQTEL                                                 
                 ARQUPDT.                                               
                                                                        
           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.                 
                                                                        
           PERFORM 1100-TESTAR-FS-ARQTEL.                               
                                                                        
           PERFORM 9100-FINALIZAR-CHECK-RESTART                         
                                                                        
           GOBACK.                                                      
                                                                        
      *---------------------------------------------------------------* 
       6000-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       6100-EMITIR-DISPLAY             SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           DISPLAY '******************** RDAB0259 ********************'.
           DISPLAY '*                                                *'.
                                                                        
           MOVE ACU-LIDOS-ARQTEL       TO WRK-MASCARA.                  
                                                                        
           DISPLAY '* LIDOS NO ARQTEL..................: ' WRK-MASCARA  
                                                                   ' *'.
                                                                        
           MOVE ACU-ATUAL-RDABB005     TO WRK-MASCARA.                  
                                                                        
           DISPLAY '* ATUALIZADOS NA TAB. RDABB005.....: ' WRK-MASCARA  
                                                                   ' *'.
                                                                        
           MOVE ACU-GRAV-ARQUPDT      TO WRK-MASCARA.                   
                                                                        
           DISPLAY '* GRAVADOS NO ARQUIVO ARQUPDT......: ' WRK-MASCARA  
                                                                   ' *'.
           DISPLAY '*                                                *'.
           DISPLAY '******************** RDAB0259 ********************'.
                                                                        
      *---------------------------------------------------------------* 
       6100-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *----------------------------------------------------------------*
       8000-EFETIVAR-CHECK-RESTART     SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE ACU-LIDOS-ARQTEL   TO WRK-LIDOS-ARQTEL                  
           MOVE ACU-GRAV-ARQUPDT   TO WRK-GRAV-ARQUPDT                  
           MOVE ACU-ATUAL-CKRS01 TO WRK-ATUAL-CKRS01                    
           MOVE ACU-ATUAL-RDABB005 TO WRK-ATUAL-RDABB005                
           MOVE ACU-COMMIT         TO WRK-COMMIT                        
                                                                        
           MOVE SPACES                 TO CK01-PLAN.                    
           MOVE 'DB2'                  TO CK01-ID-DB2.                  
           MOVE 'P'                    TO CK01-FUNCAO.                  
                                                                        
           MOVE LENGTH OF WRK-AREA-RESTART                              
                                       TO CK01-TAM-AREA-RESTART.        
           MOVE WRK-AREA-RESTART       TO CK01-AREA-RESTART             
                                       (1:LENGTH OF WRK-AREA-RESTART).  
           PERFORM 0510-CHAMAR-CHECKPOINT-RESTART.                      
                                                                        
      *----------------------------------------------------------------*
       8000-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      ******************************************************************
      * FINALIZAR CHECKPOINT RESTART.                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       9100-FINALIZAR-CHECK-RESTART    SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE SPACES                 TO CK01-PLAN.                    
           MOVE 'DB2'                  TO CK01-ID-DB2.                  
           MOVE 'F'                    TO CK01-FUNCAO.                  
                                                                        
           MOVE LENGTH OF WRK-AREA-RESTART                              
                                       TO CK01-TAM-AREA-RESTART.        
           MOVE WRK-AREA-RESTART       TO CK01-AREA-RESTART             
                                       (1:LENGTH OF WRK-AREA-RESTART).  
                                                                        
           PERFORM 0510-CHAMAR-CHECKPOINT-RESTART.                      
                                                                        
      *----------------------------------------------------------------*
       9100-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *---------------------------------------------------------------* 
       9999-PROCESSAR-ROTINA-ERRO      SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           MOVE 'RDAB0259'             TO ERR-PGM.                      
                                                                        
           CALL 'BRAD7100'             USING WRK-BATCH                  
                                             ERRO-AREA                  
                                             SQLCA.                     
                                                                        
           GOBACK.                                                      
                                                                        
      *---------------------------------------------------------------* 
       9999-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
                                                                        

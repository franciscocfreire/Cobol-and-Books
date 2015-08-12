      *===============================================================* 
       IDENTIFICATION                  DIVISION.                        
      *===============================================================* 
                                                                        
       PROGRAM-ID. RDAB0274.                                            
       AUTHOR. ELSINO SILVA.                                            
                                                                        
      *===============================================================* 
      *                 B R Q    I T   S E R V I C E S                * 
      *---------------------------------------------------------------* 
      *                                                               * 
      *   PROGRAMA    : RDAB0274                                      * 
      *   PROGRAMADOR : ELSINO SILVA           - BRQ IT SERVICES      * 
      *   ANALISTA    : FRANCISCO FREIRE       - BRQ IT SERVICES      * 
      *   SUPERVISOR  : JUNIOR RIBAMAR         - GP. 70               * 
      *   DATA        : NOVEMBRO/2014                                 * 
      *                                                               * 
      *   OBJETIVO    : ATUALIZACAO DO TELEFONE DA BASE RDABB005 COM  * 
      *                 DADOS DO CADU                                 * 
      *   ARQUIVOS:                                                   * 
      *   ---------------------------------------------------------   * 
      *   DDNAME   |I/O| DESCRICAO               | BOOK     | LRECL   * 
      *   ---------+---+-------------------------+----------+------   * 
      *   TELCADU  | I | TELEFONE CADU           | I#RDABAP |   320   * 
      *   TELRDAB  | I | TELEFONE RDAB           | I#RDAB15 |   044   * 
      *   CADURDAB | O | TELEFONE RDAB ALTERADO  | I#RDAB15 |   044   * 
      *   DESPRDAB | O | TELEFONE RDAB DESPREZADO| I#RDAB15 |   044   * 
      *   ---------------------------------------------------------   * 
      *                                                               * 
      *===============================================================* 
                                                                        
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
                                                                        
           SELECT  TELCADU   ASSIGN    TO  UT-S-TELCADU                 
                   FILE      STATUS    IS  WRK-FS-TELCADU.              
                                                                        
           SELECT  TELRDAB   ASSIGN    TO  UT-S-TELRDAB                 
                   FILE      STATUS    IS  WRK-FS-TELRDAB.              
                                                                        
           SELECT  CADURDAB  ASSIGN    TO  UT-S-CADURDAB                
                   FILE      STATUS    IS  WRK-FS-CADURDAB.             
                                                                        
           SELECT  DESPRDAB  ASSIGN    TO  UT-S-DESPRDAB                
                   FILE      STATUS    IS  WRK-FS-DESPRDAB.             
                                                                        
      *===============================================================* 
       DATA                            DIVISION.                        
      *===============================================================* 
                                                                        
      *---------------------------------------------------------------* 
       FILE                            SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
      *  INPUT....: ARQ. TELEFONES CADU                               * 
      *             ORG. SEQUENCIAL    -  LRECL = 320 BYTES           * 
      *---------------------------------------------------------------* 
                                                                        
       FD  TELCADU                                                      
           LABEL       RECORD    STANDARD                               
           RECORDING   MODE      F                                      
           BLOCK       CONTAINS  0.                                     
                                                                        
       01  FD-REG-TELCADU              PIC  X(320)   VALUE SPACES.      
                                                                        
      *---------------------------------------------------------------* 
      *  INPUT....: ARQ. TELEFONES RDABB005                           * 
      *             ORG. SEQUENCIAL    -  LRECL = 044 BYTES           * 
      *---------------------------------------------------------------* 
                                                                        
       FD  TELRDAB                                                      
           LABEL       RECORD    STANDARD                               
           RECORDING   MODE      F                                      
           BLOCK       CONTAINS  0.                                     
                                                                        
       01  FD-REG-TELRDAB              PIC  X(044)   VALUE SPACES.      
                                                                        
      *---------------------------------------------------------------* 
      *  OUTPUT...: ARQ. DE TELEFONES DO RDAB ATUALIZADOS             * 
      *             ORG. SEQUENCIAL   -   LRECL = 044 BYTES           * 
      *---------------------------------------------------------------* 
                                                                        
       FD  CADURDAB                                                     
           LABEL       RECORD    STANDARD                               
           RECORDING   MODE      F                                      
           BLOCK       CONTAINS  0.                                     
                                                                        
       01  FD-REG-CADURDAB             PIC  X(044)   VALUE SPACES.      
                                                                        
      *---------------------------------------------------------------* 
      *  OUTPUT...: ARQ. DE TELEFONES DO RDAB DESPREZADOS             * 
      *             ORG. SEQUENCIAL   -   LRECL = 044 BYTES           * 
      *---------------------------------------------------------------* 
                                                                        
       FD  DESPRDAB                                                     
           LABEL       RECORD    STANDARD                               
           RECORDING   MODE      F                                      
           BLOCK       CONTAINS  0.                                     
                                                                        
       01  FD-REG-DESPRDAB             PIC  X(044)   VALUE SPACES.      
                                                                        
      *---------------------------------------------------------------* 
       WORKING-STORAGE                 SECTION.                         
      *---------------------------------------------------------------* 
       01   FILLER                     PIC  X(32) VALUE                 
            'INICIO DA WORKING RDAB0274'.                               
                                                                        
      *-- ------------------------------------------------------------* 
      *          CAMPOS UTILIZADOS PARA CANCELAR PROCESSAMENTO        * 
      *---------------------------------------------------------------* 
                                                                        
       01  FILLER.                                                      
           03  WRK-FS-TELCADU          PIC  X(02) VALUE SPACES.         
           03  WRK-FS-TELRDAB          PIC  X(02) VALUE SPACES.         
           03  WRK-FS-CADURDAB         PIC  X(02) VALUE SPACES.         
           03  WRK-FS-DESPRDAB         PIC  X(02) VALUE SPACES.         
           03  WRK-OPERACAO            PIC  X(13) VALUE SPACES.         
           03  WRK-ABERTURA            PIC  X(13) VALUE 'NA ABERTURA'.  
           03  WRK-LEITURA             PIC  X(13) VALUE 'NA LEITURA'.   
           03  WRK-GRAVACAO            PIC  X(13) VALUE 'NA GRAVACAO'.  
           03  WRK-FECHAMENTO          PIC  X(13) VALUE 'NO FECHAMENTO'.
           03  WRK-BATCH               PIC  X(08) VALUE 'BATCH'.        
      *---------------------------------------------------------------* 
      *--  ACUMULADORES.                                                
                                                                        
       01  FILLER.                                                      
           03  ACU-LDS-TELCADU         PIC 9(09) COMP-3    VALUE  ZEROS.
           03  ACU-LDS-TELRDAB         PIC 9(09) COMP-3    VALUE  ZEROS.
           03  ACU-GRV-CADURDAB        PIC 9(09) COMP-3    VALUE  ZEROS.
           03  ACU-GRV-DESPRDAB        PIC 9(09) COMP-3    VALUE  ZEROS.
           03  ACU-BRANCOS-CADU        PIC 9(09) COMP-3    VALUE  ZEROS.
           03  ACU-ENDRIGUAL-CADU      PIC 9(09) COMP-3    VALUE  ZEROS.
           03  ACU-NAOACHOU-CADU       PIC 9(09) COMP-3    VALUE  ZEROS.
      *                                                                 
      *----------------------------------------------------------------*
      *--  EDICAO.                                                      
                                                                        
       01  FILLER.                                                      
           03  WRK-EDIT01              PIC ZZZ.ZZZ.ZZ9     VALUE  ZEROS.
           03  WRK-EDIT02              PIC ZZZ.ZZZ.ZZ9     VALUE  ZEROS.
           03  WRK-EDIT03              PIC ZZZ.ZZZ.ZZ9     VALUE  ZEROS.
           03  WRK-EDIT04              PIC ZZZ.ZZZ.ZZ9     VALUE  ZEROS.
           03  WRK-EDIT05              PIC ZZZ.ZZZ.ZZ9     VALUE  ZEROS.
           03  WRK-EDIT06              PIC ZZZ.ZZZ.ZZ9     VALUE  ZEROS.
           03  WRK-EDIT07              PIC ZZZ.ZZZ.ZZ9     VALUE  ZEROS.
                                                                        
      *--  CHAVE TELCADU.                                               
       01  WRK-CHV-TELCADU.                                             
           03  WRK-CHV-CADU-AGE          PIC  9(005) VALUE ZEROS.       
           03  WRK-CHV-CADU-CCTA         PIC  9(013) VALUE ZEROS.       
                                                                        
      *--  CHAVE TELRDAB.                                               
       01  WRK-CHV-TELRDAB.                                             
           03  WRK-CHV-RDAB-AGE          PIC  9(005) VALUE ZEROS.       
           03  WRK-CHV-RDAB-CCTA         PIC  9(013) VALUE ZEROS.       
                                                                        
      *--  AUX. CHAVE TELRDAB.                                          
       01  WRK-TEMP-CH-TELRDAB.                                         
           03  WRK-TEMP-RDAB-AGE         PIC S9(005) VALUE ZEROS.       
           03  WRK-AUX-RDAB-AGE          REDEFINES WRK-TEMP-RDAB-AGE    
                                         PIC  9(005).                   
           03  WRK-TEMP-RDAB-CCTA        PIC S9(013) VALUE ZEROS.       
           03  WRK-AUX-RDAB-CCTA         REDEFINES WRK-TEMP-RDAB-CCTA   
                                         PIC  9(013).                   
                                                                        
      *--  AUX. TELEFONE CADU.                                          
       01  WRK-TEL-CADU.                                                
           05  WRK-CADU-CDDD             PIC  X(004) VALUE SPACES.      
           05  WRK-CADU-CFONE-NOVO       PIC  X(011) VALUE SPACES.      
                                                                        
      *--  AUX. TELEFONE RDAB.                                          
       01  WRK-TEL-RDAB.                                                
           03  WRK-RDAB-CDDD-CLI-RENEG   PIC  X(004) VALUE SPACES.      
           03  WRK-RDAB-CFONE-RENEG      PIC  X(011) VALUE SPACES.      
                                                                        
      *--  TEMP TELEFONE CADU                                           
       01  WRK-TEMP-TELCADU.                                            
           03  WRK-TEMP-CFONE-NOVO       PIC  9(011) VALUE ZEROS.       
           03  WRK-AUX-CFONE-NOVO        REDEFINES                      
               WRK-TEMP-CFONE-NOVO       PIC  X(011).                   
                                                                        
                                                                        
      *---------------------------------------------------------------* 
       01  FILLER                      PIC  X(032)         VALUE        
           '*       AREA DE BOOKS          *'.                          
      *----------------------------------------------------------------*
                                                                        
       COPY 'I#RDAB15'.                                                 
       COPY 'I#RDABAP'.                                                 
                                                                        
      *----------------------------------------------------------------*
       01  FILLER   PIC  X(32) VALUE '*       AREA DA BRAD7100       *'.
      *----------------------------------------------------------------*
                                                                        
       COPY 'I#BRAD7C'.                                                 
                                                                        
      *---------------------------------------------------------------* 
       01   FILLER                     PIC  X(32)    VALUE              
            'FIM DA WORKING RDAB0274'.                                  
                                                                        
      *===============================================================* 
       PROCEDURE                       DIVISION.                        
      *===============================================================* 
                                                                        
      *---------------------------------------------------------------* 
       0000-INICIAR                    SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           PERFORM 1000-INICIALIZAR.                                    
                                                                        
           PERFORM 2000-LER-TELCADU                                     
           IF      WRK-FS-TELCADU      EQUAL '10'                       
                   DISPLAY '**************** RDAB0274 ***************'  
                   DISPLAY '*                                       *'  
                   DISPLAY '* ARQUIVO DE ENTRADA - TELCADU - VAZIO *'   
                   DISPLAY '*        PROCESSAMENTO ENCERRADO        *'  
                   DISPLAY '*                                       *'  
                   DISPLAY '**************** RDAB0274 ***************'  
                   PERFORM  9000-FINALIZAR                              
           END-IF.                                                      
                                                                        
           PERFORM 3000-LER-TELRDAB                                     
           IF      WRK-FS-TELRDAB      EQUAL '10'                       
                   DISPLAY '**************** RDAB0274 ***************'  
                   DISPLAY '*                                       *'  
                   DISPLAY '* ARQUIVO DE ENTRADA - TELRDAB - VAZIO *'   
                   DISPLAY '*        PROCESSAMENTO ENCERRADO        *'  
                   DISPLAY '*                                       *'  
                   DISPLAY '**************** RDAB0274 ***************'  
           END-IF.                                                      
                                                                        
           PERFORM 4000-PROCESSAR     UNTIL                             
                  (WRK-FS-TELRDAB      EQUAL '10')                      
                                                                        
                                                                        
           PERFORM 7000-DISPLAY-TOTAIS                                  
                                                                        
           PERFORM 9000-FINALIZAR.                                      
                                                                        
      *---------------------------------------------------------------* 
       0000-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1000-INICIALIZAR                SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           OPEN  INPUT  TELCADU                                         
                        TELRDAB                                         
                 OUTPUT CADURDAB                                        
                        DESPRDAB                                        
                                                                        
           MOVE  WRK-ABERTURA          TO    WRK-OPERACAO               
           PERFORM  1100-TESTAR-FILE-STATUS.                            
                                                                        
      *---------------------------------------------------------------* 
       1000-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1100-TESTAR-FILE-STATUS         SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           PERFORM  1110-TESTAR-FS-TELCADU                              
           PERFORM  1120-TESTAR-FS-TELRDAB                              
           PERFORM  1150-TESTAR-FS-CADURDAB                             
           PERFORM  1160-TESTAR-FS-DESPRDAB.                            
                                                                        
      *---------------------------------------------------------------* 
       1100-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1110-TESTAR-FS-TELCADU          SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-FS-TELCADU          NOT EQUAL  '00'                   
              DISPLAY '************** RDAB0274 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              TELCADU             *'            
              DISPLAY '*         FILE STATUS =  ' WRK-FS-TELCADU        
                                                 '         *'           
              DISPLAY '************** RDAB0274 *************'           
              PERFORM 9999-ROTINA-ERRO                                  
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       1110-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1120-TESTAR-FS-TELRDAB          SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-FS-TELRDAB          NOT EQUAL  '00'                   
              DISPLAY '************** RDAB0274 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              TELRDAB             *'            
              DISPLAY '*         FILE STATUS =  ' WRK-FS-TELRDAB        
                                                 '         *'           
              DISPLAY '************** RDAB0274 *************'           
              PERFORM 9999-ROTINA-ERRO                                  
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       1120-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1150-TESTAR-FS-CADURDAB         SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-FS-CADURDAB           NOT EQUAL  '00'                 
              DISPLAY '************** RDAB0274 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              CADURDAB              *'          
              DISPLAY '*         FILE STATUS =  ' WRK-FS-CADURDAB       
                                                 '         *'           
              DISPLAY '************** RDAB0274 *************'           
              PERFORM 9999-ROTINA-ERRO                                  
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       1150-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1160-TESTAR-FS-DESPRDAB         SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-FS-DESPRDAB           NOT EQUAL  '00'                 
              DISPLAY '************** RDAB0274 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              DESPRDAB              *'          
              DISPLAY '*         FILE STATUS =  ' WRK-FS-DESPRDAB       
                                                 '         *'           
              DISPLAY '************** RDAB0274 *************'           
              PERFORM 9999-ROTINA-ERRO                                  
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       1160-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       2000-LER-TELCADU                SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           READ    TELCADU             INTO  REG-CADUV000               
                                                                        
           IF      WRK-FS-TELCADU      EQUAL  '10'                      
                   MOVE HIGH-VALUES    TO      WRK-CHV-TELCADU          
                   GO                  TO      2000-99-FIM              
           END-IF.                                                      
                                                                        
           MOVE    WRK-LEITURA         TO      WRK-OPERACAO             
           PERFORM 1110-TESTAR-FS-TELCADU                               
                                                                        
           MOVE  CADUV000-CAG-COBR-TARIF                                
                                       TO  WRK-CHV-CADU-AGE             
           MOVE  CADUV000-CCTA-COBR-TARIF                               
                                       TO  WRK-CHV-CADU-CCTA            
                                                                        
           MOVE  CADUV000-CDDD         TO  WRK-CADU-CDDD                
           MOVE  CADUV000-CFONE-NOVO   TO  WRK-TEMP-CFONE-NOVO          
           MOVE  WRK-AUX-CFONE-NOVO    TO  WRK-CADU-CFONE-NOVO          
                                                                        
           ADD     1                   TO  ACU-LDS-TELCADU.             
                                                                        
      *---------------------------------------------------------------* 
       2000-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       3000-LER-TELRDAB                SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           READ    TELRDAB             INTO  AFN-REG                    
                                                                        
           IF      WRK-FS-TELRDAB      EQUAL  '10'                      
                   MOVE HIGH-VALUES    TO      WRK-CHV-TELRDAB          
                   GO                  TO      3000-99-FIM              
           END-IF.                                                      
                                                                        
           MOVE    WRK-LEITURA         TO      WRK-OPERACAO             
           PERFORM 1120-TESTAR-FS-TELRDAB                               
                                                                        
           MOVE AFN-CAG-BCRIA          TO  WRK-TEMP-RDAB-AGE            
           MOVE WRK-AUX-RDAB-AGE       TO  WRK-CHV-RDAB-AGE             
           MOVE AFN-CCTA-CORR          TO  WRK-TEMP-RDAB-CCTA           
           MOVE WRK-AUX-RDAB-CCTA      TO  WRK-CHV-RDAB-CCTA            
                                                                        
           MOVE AFN-CDDD-CLI-RENEG     TO  WRK-RDAB-CDDD-CLI-RENEG      
           MOVE AFN-CFONE-RENEG        TO  WRK-RDAB-CFONE-RENEG         
                                                                        
           ADD     1                   TO  ACU-LDS-TELRDAB.             
                                                                        
      *---------------------------------------------------------------* 
       3000-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       4000-PROCESSAR                  SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           IF  WRK-CHV-TELCADU         GREATER  WRK-CHV-TELRDAB         
               ADD   1                 TO  ACU-NAOACHOU-CADU            
               PERFORM 5100-GRAVAR-DESPRDAB                             
               PERFORM 3000-LER-TELRDAB                                 
           ELSE                                                         
              IF   WRK-CHV-TELCADU     LESS   WRK-CHV-TELRDAB           
                   PERFORM 2000-LER-TELCADU                             
              ELSE                                                      
                 IF  WRK-TEL-CADU      EQUAL  WRK-TEL-RDAB              
                     ADD  1            TO  ACU-ENDRIGUAL-CADU           
                     PERFORM 5100-GRAVAR-DESPRDAB                       
                 ELSE                                                   
                    IF (WRK-TEL-CADU   EQUAL SPACES OR LOW-VALUES)      
                       ADD  1          TO  ACU-BRANCOS-CADU             
                       PERFORM 5100-GRAVAR-DESPRDAB                     
                    ELSE                                                
                       MOVE  CADUV000-CDDD                              
                                       TO  AFN-CDDD-CLI-RENEG           
                       MOVE  WRK-AUX-CFONE-NOVO                         
                                       TO  AFN-CFONE-RENEG              
                                                                        
                       PERFORM 5000-GRAVAR-CADURDAB                     
                    END-IF                                              
                 END-IF                                                 
                 PERFORM 3000-LER-TELRDAB                               
              END-IF                                                    
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       4000-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       5000-GRAVAR-CADURDAB            SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           WRITE  FD-REG-CADURDAB      FROM  AFN-REG                    
           MOVE   WRK-GRAVACAO         TO  WRK-OPERACAO                 
           PERFORM  1150-TESTAR-FS-CADURDAB                             
                                                                        
           ADD 1                       TO  ACU-GRV-CADURDAB.            
                                                                        
      *---------------------------------------------------------------* 
       5000-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       5100-GRAVAR-DESPRDAB            SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           WRITE  FD-REG-DESPRDAB      FROM  AFN-REG                    
           MOVE   WRK-GRAVACAO         TO  WRK-OPERACAO                 
           PERFORM  1160-TESTAR-FS-DESPRDAB                             
                                                                        
           ADD 1                       TO  ACU-GRV-DESPRDAB.            
                                                                        
      *---------------------------------------------------------------* 
       5100-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       7000-DISPLAY-TOTAIS             SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           MOVE     ACU-LDS-TELCADU    TO    WRK-EDIT01                 
           MOVE     ACU-LDS-TELRDAB    TO    WRK-EDIT02                 
           MOVE     ACU-BRANCOS-CADU   TO    WRK-EDIT03                 
           MOVE     ACU-ENDRIGUAL-CADU TO    WRK-EDIT04                 
           MOVE     ACU-GRV-CADURDAB   TO    WRK-EDIT05                 
           MOVE     ACU-GRV-DESPRDAB   TO    WRK-EDIT06                 
           MOVE     ACU-NAOACHOU-CADU  TO    WRK-EDIT07                 
                                                                        
           DISPLAY '******************** RDAB0274 ********************' 
           DISPLAY '*                                                *' 
           DISPLAY '*  TOTAL REG. LIDOS    -  TELCADU : ' WRK-EDIT01 '*'
           DISPLAY '*  TOTAL REG. LIDOS    -  TELRDAB : ' WRK-EDIT02 '*'
           DISPLAY '*  TOTAL REG. DESPREZ. ENDR BRANCO: ' WRK-EDIT03 '*'
           DISPLAY '*  TOTAL REG. DESPREZ. ENDR IGUAL : ' WRK-EDIT04 '*'
           DISPLAY '*  TOTAL REG. DESPR.NAO ACHOU CADU: ' WRK-EDIT07 '*'
           DISPLAY '*  TOTAL REG. GRAVADOS - CADURDAB : ' WRK-EDIT05 '*'
           DISPLAY '*  TOTAL REG. GRAVADOS - DESPRDAB : ' WRK-EDIT06 '*'
           DISPLAY '*                                                *' 
           DISPLAY '******************** RDAB0274 ********************'.
                                                                        
      *---------------------------------------------------------------* 
       7000-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       9000-FINALIZAR                  SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           CLOSE  TELCADU                                               
                  TELRDAB                                               
                  CADURDAB                                              
                  DESPRDAB.                                             
                                                                        
           MOVE   WRK-FECHAMENTO       TO      WRK-OPERACAO             
           PERFORM  1100-TESTAR-FILE-STATUS.                            
                                                                        
           STOP RUN.                                                    
                                                                        
      *---------------------------------------------------------------* 
       9000-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *----------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE   'RDAB0274'           TO  ERR-PGM.                     
           MOVE   'APL'                TO  ERR-TIPO-ACESSO.             
           CALL   'BRAD7100'        USING  WRK-BATCH                    
                                           ERRO-AREA.                   
                                                                        
           GOBACK.                                                      
                                                                        
      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        

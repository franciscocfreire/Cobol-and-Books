      *===============================================================* 
       IDENTIFICATION DIVISION.                                         
      *===============================================================* 
                                                                        
       PROGRAM-ID. RDAB0896.                                            
       AUTHOR. FRANCISCO.                                               
                                                                        
      *===============================================================* 
      *                 B R Q    I T   S E R V I C E S                * 
      *---------------------------------------------------------------* 
      *                                                               * 
      *   PROGRAMA    : RDAB0896                                      * 
      *   PROGRAMADOR : FRANCISCO FREIRE       - BRQ IT SERVICES      * 
      *   ANALISTA    : FRANCISCO FREIRE       - BRQ IT SERVICES      * 
      *   SUPERVISOR  : JUNIOR RIBAMAR         - GP. 70               * 
      *   DATA        : ABRIL/2014                                    * 
      *                                                               * 
      *   OBJETIVO    : BALANCE LINE DE PENDENCIAS RENEGOCIADAS COM   * 
      *                 INFORMACOES DO TELEFONE                       * 
      *   ARQUIVOS:                                                   * 
      *   ---------------------------------------------------------   * 
      *   DDNAME   |I/O| DESCRICAO               | BOOK     | LRECL   * 
      *   ---------+---+-------------------------+----------+------   * 
      *   REGPENDF | I | PENDENCIA RENEGOCIADAS  | I#RDABAS |   500   * 
      *   CADUB006 | I | TELEFONE                | PSDCW033 |   642   * 
      *   ARQSAIDA | O | DADOS PRINCIPAIS        | I#RDABAS |   500   * 
      *   ---------------------------------------------------------   * 
                                                                        
      *===============================================================* 
       ENVIRONMENT DIVISION.                                            
      *===============================================================* 
                                                                        
      *---------------------------------------------------------------* 
       CONFIGURATION SECTION.                                           
      *---------------------------------------------------------------* 
                                                                        
       SPECIAL-NAMES.                                                   
           DECIMAL-POINT IS COMMA.                                      
                                                                        
      *---------------------------------------------------------------* 
       INPUT-OUTPUT SECTION.                                            
      *---------------------------------------------------------------* 
                                                                        
       FILE-CONTROL.                                                    
                                                                        
           SELECT  REGPENDF  ASSIGN  TO  UT-S-REGPENDF                  
                   FILE      STATUS  IS  WRK-FS-REGPENDF.               
                                                                        
           SELECT  CADUB006  ASSIGN  TO  UT-S-CADUB006                  
                   FILE      STATUS  IS  WRK-FS-CADUB006.               
                                                                        
           SELECT  ARQSAIDA  ASSIGN  TO  UT-S-ARQSAIDA                  
                   FILE      STATUS  IS  WRK-FS-ARQSAIDA.               
                                                                        
      *===============================================================* 
       DATA DIVISION.                                                   
      *===============================================================* 
                                                                        
      *---------------------------------------------------------------* 
       FILE SECTION.                                                    
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
      *  INPUT....: ARQ. DADOS PRINCIPAIS                             * 
      *             ORG. SEQUENCIAL    -  LRECL = 500 BYTES           * 
      *---------------------------------------------------------------* 
                                                                        
       FD  REGPENDF                                                     
           LABEL       RECORD    STANDARD                               
           RECORDING   MODE      F                                      
           BLOCK       CONTAINS  0.                                     
                                                                        
       COPY 'I#RDABAS'.                                                 
      *---------------------------------------------------------------* 
      *  INPUT....: ARQ. ENDERECO                                     * 
      *             ORG. SEQUENCIAL    -  LRECL = 642 BYTES           * 
      *---------------------------------------------------------------* 
                                                                        
       FD  CADUB006                                                     
           LABEL       RECORD    STANDARD                               
           RECORDING   MODE      F                                      
           BLOCK       CONTAINS  0.                                     
                                                                        
       COPY 'PSDCW033'.                                                 
      *---------------------------------------------------------------* 
      *  OUTPUT...: ARQ. DE DADOS DA PESSOA                           * 
      *             ORG. SEQUENCIAL   -   LRECL = 500 BYTES           * 
      *---------------------------------------------------------------* 
                                                                        
       FD  ARQSAIDA                                                     
           LABEL       RECORD    STANDARD                               
           RECORDING   MODE      F                                      
           BLOCK       CONTAINS  0.                                     
                                                                        
                                                                        
       COPY 'I#RDABAS'.                                                 
                                                                        
      *---------------------------------------------------------------* 
       WORKING-STORAGE SECTION.                                         
      *---------------------------------------------------------------* 
       01   FILLER                     PIC  X(32) VALUE                 
            'INICIO DA WORKING RDAB0896'.                               
                                                                        
      *-- ------------------------------------------------------------* 
      *          CAMPOS UTILIZADOS PARA CANCELAR PROCESSAMENTO        * 
      *---------------------------------------------------------------* 
                                                                        
       01  FILLER.                                                      
           03  WRK-FS-REGPENDF         PIC  X(02) VALUE SPACES.         
           03  WRK-FS-CADUB006         PIC  X(02) VALUE SPACES.         
           03  WRK-FS-ARQSAIDA         PIC  X(02) VALUE SPACES.         
           03  WRK-OPERACAO            PIC  X(13) VALUE SPACES.         
           03  WRK-ABERTURA            PIC  X(13) VALUE 'NA ABERTURA'.  
           03  WRK-LEITURA             PIC  X(13) VALUE 'NA LEITURA'.   
           03  WRK-GRAVACAO            PIC  X(13) VALUE 'NA GRAVACAO'.  
           03  WRK-FECHAMENTO          PIC  X(13) VALUE 'NO FECHAMENTO'.
           03  WRK-BATCH               PIC  X(08) VALUE 'BATCH'.        
      *---------------------------------------------------------------* 
      *--  ACUMULADORES.                                                
                                                                        
       01  FILLER.                                                      
           03  ACU-LDS-REGPENDF        PIC 9(09) COMP-3    VALUE  ZEROS.
           03  ACU-LDS-CADUB006        PIC 9(09) COMP-3    VALUE  ZEROS.
           03  ACU-GRV-ARQSAIDA        PIC 9(09) COMP-3    VALUE  ZEROS.
      *                                                                 
      *----------------------------------------------------------------*
      *--  EDICAO.                                                      
                                                                        
       01  FILLER.                                                      
           03  WRK-EDIT01              PIC ZZZ.ZZZ.ZZ9     VALUE  ZEROS.
           03  WRK-EDIT02              PIC ZZZ.ZZZ.ZZ9     VALUE  ZEROS.
           03  WRK-EDIT05              PIC ZZZ.ZZZ.ZZ9     VALUE  ZEROS.
                                                                        
      *----------------------------------------------------------------*
      *--  AREA AUXILIAR.                                               
       01  WRK-AUX-NUM010              PIC 9(10) VALUE  ZEROS.          
       01  WRK-AUX-NUM010-S            REDEFINES                        
           WRK-AUX-NUM010              PIC S9(10).                      
                                                                        
       01  WRK-AUX-NUM003              PIC 9(03) VALUE  ZEROS.          
       01  WRK-AUX-NUM003-S            REDEFINES                        
           WRK-AUX-NUM003              PIC S9(03).                      
                                                                        
       01  WRK-AUX-RED-NUM011          PIC S9(11) VALUE  ZEROS.
       01  FILLER  REDEFINES WRK-AUX-RED-NUM011.                        
           03 FILLER                   PIC 9(02).                       
           03 WRK-AUX-RED-NUM009       PIC 9(09).                       
                                                                        
      *--  CHAVE REGPENDF.                                              
       01  WRK-CHV-REGPENDF.                                            
           03  WRK-CHV-RPF-CCLUB       PIC 9(10) VALUE  ZEROS.          
           03  WRK-CHV-RPF-CNTRL-FONE  PIC 9(03) VALUE  ZEROS.          
                                                                        
      *--  CHAVE CADUB006.                                              
       01  WRK-CHV-CADUB006.                                            
           03  WRK-CHV-006-CCLUB       PIC 9(10) VALUE  ZEROS.          
           03  WRK-CHV-006-CNTRL-FONE  PIC 9(03) VALUE  ZEROS.          
                                                                        
      *----------------------------------------------------------------*
       01  FILLER   PIC  X(32) VALUE '*       AREA DA BRAD7100       *'.
      *----------------------------------------------------------------*
                                                                        
       COPY 'I#BRAD7C'.                                                 
                                                                        
      *---------------------------------------------------------------* 
       01   FILLER                     PIC  X(32)    VALUE              
            'FIM DA WORKING RDAB0896'.                                  
                                                                        
      *===============================================================* 
       PROCEDURE DIVISION.                                              
      *===============================================================* 
                                                                        
      *---------------------------------------------------------------* 
       0000-INICIAR SECTION.                                            
      *---------------------------------------------------------------* 
                                                                        
           PERFORM 1000-INICIALIZAR.                                    
                                                                        
           PERFORM 2000-LER-REGPENDF                                    
           IF      WRK-FS-REGPENDF     EQUAL '10'                       
                   DISPLAY '**************** RDAB0896 ***************'  
                   DISPLAY '*                                       *'  
                   DISPLAY '* ARQUIVO DE ENTRADA - REGPENDF - VAZIO *'  
                   DISPLAY '*        PROCESSAMENTO ENCERRADO        *'  
                   DISPLAY '*                                       *'  
                   DISPLAY '**************** RDAB0896 ***************'  
                   PERFORM  9000-FINALIZAR                              
           END-IF.                                                      
                                                                        
           PERFORM 3000-LER-CADUB006                                    
           IF      WRK-FS-CADUB006     EQUAL '10'                       
                   DISPLAY '**************** RDAB0896 ***************'  
                   DISPLAY '*                                       *'  
                   DISPLAY '* ARQUIVO DE ENTRADA - CADUB006 - VAZIO *'  
                   DISPLAY '*        PROCESSAMENTO ENCERRADO        *'  
                   DISPLAY '*                                       *'  
                   DISPLAY '**************** RDAB0896 ***************'  
           END-IF                                                       
                                                                        
           PERFORM 4000-PROCESSAR     UNTIL                             
                  (WRK-FS-REGPENDF     EQUAL '10')                      
                                                                        
                                                                        
           PERFORM 7000-DISPLAY-TOTAIS                                  
                                                                        
           PERFORM 9000-FINALIZAR.                                      
                                                                        
      *---------------------------------------------------------------* 
       0000-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1000-INICIALIZAR              SECTION.                           
      *---------------------------------------------------------------* 
                                                                        
           OPEN  INPUT  REGPENDF                                        
                        CADUB006                                        
                 OUTPUT ARQSAIDA                                        
                                                                        
           MOVE  WRK-ABERTURA          TO    WRK-OPERACAO               
           PERFORM  1100-TESTAR-FILE-STATUS.                            
                                                                        
      *---------------------------------------------------------------* 
       1000-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1100-TESTAR-FILE-STATUS       SECTION.                           
      *---------------------------------------------------------------* 
                                                                        
           PERFORM  1110-TESTAR-FS-REGPENDF                             
           PERFORM  1120-TESTAR-FS-CADUB006                             
           PERFORM  1150-TESTAR-FS-ARQSAIDA.                            
                                                                        
      *---------------------------------------------------------------* 
       1100-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1110-TESTAR-FS-REGPENDF       SECTION.                           
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-FS-REGPENDF         NOT EQUAL  '00'                   
              DISPLAY '************** RDAB0896 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              REGPENDF             *'           
              DISPLAY '*         FILE STATUS =  ' WRK-FS-REGPENDF       
                                                 '         *'           
              DISPLAY '************** RDAB0896 *************'           
              PERFORM 9999-ROTINA-ERRO                                  
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       1110-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1120-TESTAR-FS-CADUB006       SECTION.                           
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-FS-CADUB006         NOT EQUAL  '00'                   
              DISPLAY '************** RDAB0896 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              CADUB006             *'           
              DISPLAY '*         FILE STATUS =  ' WRK-FS-CADUB006       
                                                 '         *'           
              DISPLAY '************** RDAB0896 *************'           
              PERFORM 9999-ROTINA-ERRO                                  
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       1120-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1150-TESTAR-FS-ARQSAIDA        SECTION.                          
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-FS-ARQSAIDA           NOT EQUAL  '00'                 
              DISPLAY '************** RDAB0896 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              ARQSAIDA              *'          
              DISPLAY '*         FILE STATUS =  ' WRK-FS-ARQSAIDA       
                                                 '         *'           
              DISPLAY '************** RDAB0896 *************'           
              PERFORM 9999-ROTINA-ERRO                                  
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       1150-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       2000-LER-REGPENDF            SECTION.                            
      *---------------------------------------------------------------* 
                                                                        
           READ    REGPENDF                                             
                                                                        
           IF      WRK-FS-REGPENDF     EQUAL  '10'                      
                   MOVE HIGH-VALUES    TO      WRK-CHV-REGPENDF         
                   GO                  TO      2000-99-FIM              
           END-IF.                                                      
                                                                        
           MOVE    WRK-LEITURA         TO      WRK-OPERACAO             
           PERFORM 1110-TESTAR-FS-REGPENDF                              
                                                                        
           MOVE RPF-CLUB OF REGPENDF      TO  WRK-CHV-RPF-CCLUB.        
           MOVE RPF-QREG-CNTRL-FONE OF REGPENDF                         
                                          TO  WRK-CHV-RPF-CNTRL-FONE.   
                                                                        
           ADD     1                      TO  ACU-LDS-REGPENDF.         
                                                                        
      *---------------------------------------------------------------* 
       2000-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       3000-LER-CADUB006            SECTION.                            
      *---------------------------------------------------------------* 
                                                                        
           READ    CADUB006                                             
                                                                        
           IF      WRK-FS-CADUB006     EQUAL  '10'                      
                   MOVE HIGH-VALUES    TO      WRK-CHV-CADUB006         
                   GO                  TO      3000-99-FIM              
           END-IF.                                                      
                                                                        
           MOVE    WRK-LEITURA         TO      WRK-OPERACAO             
           PERFORM 1120-TESTAR-FS-CADUB006                              
                                                                        
           MOVE PSDCW033-03-CCLUB OF CADUB006                           
                                       TO  WRK-AUX-NUM010-S             
           MOVE WRK-AUX-NUM010         TO  WRK-CHV-006-CCLUB.           
                                                                        
           MOVE PSDCW033-03-CSEQ-CNTRL-FONE OF CADUB006                 
                                       TO  WRK-AUX-NUM003-S             
           MOVE WRK-AUX-NUM003         TO  WRK-CHV-006-CNTRL-FONE       
                                                                        
           ADD     1                   TO      ACU-LDS-CADUB006.        
                                                                        
      *---------------------------------------------------------------* 
       3000-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       4000-PROCESSAR               SECTION.                            
      *---------------------------------------------------------------* 
                                                                        
           IF      WRK-CHV-REGPENDF   GREATER   WRK-CHV-CADUB006        
                   PERFORM 3000-LER-CADUB006                            
           ELSE                                                         
              IF      WRK-CHV-REGPENDF    LESS    WRK-CHV-CADUB006      
                                                                        
                      PERFORM 4100-CHAVES-MENOR                         
                      PERFORM 2000-LER-REGPENDF                         
              ELSE                                                      
                      PERFORM 4200-CHAVES-IGUAIS                        
                      PERFORM 2000-LER-REGPENDF                         
                      PERFORM 3000-LER-CADUB006                         
              END-IF                                                    
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       4000-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       4100-CHAVES-MENOR           SECTION.                             
      *---------------------------------------------------------------* 
                                                                        
           INITIALIZE REG-RDABAS OF ARQSAIDA.                           
                                                                        
           MOVE REG-RDABAS OF REGPENDF TO REG-RDABAS OF ARQSAIDA.       
                                                                        
           MOVE ZEROS                  TO RPF-TELEFONE-FICA OF ARQSAIDA.
                                                                        
           MOVE SPACES                 TO RPF-DDD-FICA OF ARQSAIDA.     
                                                                        
           PERFORM 5000-GRAVAR-ARQSAIDA.                                
                                                                        
      *---------------------------------------------------------------* 
       4100-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       4200-CHAVES-IGUAIS           SECTION.                            
      *---------------------------------------------------------------* 
                                                                        
           INITIALIZE REG-RDABAS OF ARQSAIDA.                           
                                                                        
           MOVE REG-RDABAS OF REGPENDF TO REG-RDABAS OF ARQSAIDA.       
                                                                        
           MOVE PSDCW033-03-NLIN-TFONI TO WRK-AUX-RED-NUM011.           
           MOVE WRK-AUX-RED-NUM009    TO RPF-TELEFONE-FICA OF ARQSAIDA. 
                                                                        
           MOVE PSDCW033-03-CDDDD      TO RPF-DDD-FICA OF ARQSAIDA.     
                                                                        
           PERFORM 5000-GRAVAR-ARQSAIDA.                                
                                                                        
      *---------------------------------------------------------------* 
       4200-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       5000-GRAVAR-ARQSAIDA          SECTION.                           
      *---------------------------------------------------------------* 
                                                                        
           WRITE  REG-RDABAS OF ARQSAIDA                                
           MOVE   WRK-GRAVACAO         TO      WRK-OPERACAO             
           PERFORM  1150-TESTAR-FS-ARQSAIDA                             
                                                                        
           ADD 1                      TO   ACU-GRV-ARQSAIDA.            
                                                                        
      *---------------------------------------------------------------* 
       5000-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       7000-DISPLAY-TOTAIS        SECTION.                              
      *---------------------------------------------------------------* 
                                                                        
           MOVE     ACU-LDS-REGPENDF   TO    WRK-EDIT01                 
           MOVE     ACU-LDS-CADUB006   TO    WRK-EDIT02                 
           MOVE     ACU-GRV-ARQSAIDA   TO    WRK-EDIT05                 
                                                                        
           DISPLAY '******************** RDAB0896 ********************' 
           DISPLAY '*                                                *' 
           DISPLAY '*  TOTAL REG. LIDOS    - REGPENDF : 'WRK-EDIT01'  *'
           DISPLAY '*  TOTAL REG. LIDOS    - CADUB006 : 'WRK-EDIT02'  *'
           DISPLAY '*  TOTAL REG. GRAVADOS - ARQSAIDA : 'WRK-EDIT05'  *'
           DISPLAY '*                                                *' 
           DISPLAY '******************** RDAB0896 ********************'.
                                                                        
      *---------------------------------------------------------------* 
       7000-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       9000-FINALIZAR             SECTION.                              
      *---------------------------------------------------------------* 
                                                                        
           CLOSE  REGPENDF                                              
                  CADUB006                                              
                  ARQSAIDA.                                             
                                                                        
           MOVE   WRK-FECHAMENTO       TO      WRK-OPERACAO             
           PERFORM  1100-TESTAR-FILE-STATUS.                            
                                                                        
           STOP RUN.                                                    
                                                                        
      *---------------------------------------------------------------* 
       9000-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *----------------------------------------------------------------*
       9999-ROTINA-ERRO SECTION.                                        
      *----------------------------------------------------------------*
                                                                        
           MOVE   'RDAB0896'           TO  ERR-PGM.                     
           MOVE   'APL'                TO ERR-TIPO-ACESSO.              
           CALL   'BRAD7100'        USING  WRK-BATCH                    
                                           ERRO-AREA.                   
                                                                        
           GOBACK.                                                      
                                                                        
      *----------------------------------------------------------------*
       9999-99-FIM. EXIT.                                               
      *----------------------------------------------------------------*
                                                                        

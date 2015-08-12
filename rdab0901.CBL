      *===============================================================* 
       IDENTIFICATION DIVISION.                                         
      *===============================================================* 
                                                                        
       PROGRAM-ID. RDAB0901.                                            
       AUTHOR. FRANCISCO.                                               
                                                                        
      *===============================================================* 
      *                 B R Q    I T   S E R V I C E S                * 
      *---------------------------------------------------------------* 
      *                                                               * 
      *   PROGRAMA    : RDAB0901                                      * 
      *   PROGRAMADOR : FRANCISCO FREIRE       - BRQ IT SERVICES      * 
      *   ANALISTA    : FRANCISCO FREIRE       - BRQ IT SERVICES      * 
      *   SUPERVISOR  : JUNIOR RIBAMAR         - GP. 70               * 
      *   DATA        : ABRIL/2014                                    * 
      *                                                               * 
      *   OBJETIVO    : BALANCE LINE DAS PENDENCIAS COM O ARQUIVO DO  * 
      *                 CADU COM INFORMACOES DO SOCIOS                * 
      *   ARQUIVOS:                                                   * 
      *   ---------------------------------------------------------   * 
      *   DDNAME   |I/O| DESCRICAO               | BOOK     | LRECL   * 
      *   ---------+---+-------------------------+----------+------   * 
      *   ARQPARVV | I | DADOS PRINCIPAIS        | I#RDAB01 |   223   * 
      *   CADUV001 | I | ENDERECO                | I#RDABAR |   570   * 
      *   ARQSAIDA | O | DADOS PRINCIPAIS        | I#RDAB17 |   223   * 
      *   ---------------------------------------------------------   * 
                                                                        
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
                                                                        
           SELECT  ARQPARVV  ASSIGN  TO  UT-S-ARQPARVV                  
                   FILE      STATUS  IS  WRK-FS-ARQPARVV.               
                                                                        
           SELECT  CADUV001  ASSIGN  TO  UT-S-CADUV001                  
                   FILE      STATUS  IS  WRK-FS-CADUV001.               
                                                                        
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
      *             ORG. SEQUENCIAL    -  LRECL = 150 BYTES           * 
      *---------------------------------------------------------------* 
                                                                        
       FD  ARQPARVV                                                     
           LABEL       RECORD    STANDARD                               
           RECORDING   MODE      F                                      
           BLOCK       CONTAINS  0.                                     
                                                                        
       COPY 'I#RDAB17'.                                                 
      *---------------------------------------------------------------* 
      *  INPUT....: ARQ. ENDERECO                                     * 
      *             ORG. SEQUENCIAL    -  LRECL = 642 BYTES           * 
      *---------------------------------------------------------------* 
                                                                        
       FD  CADUV001                                                     
           LABEL       RECORD    STANDARD                               
           RECORDING   MODE      F                                      
           BLOCK       CONTAINS  0.                                     
                                                                        
       COPY 'I#RDABAR'.                                                 
      *---------------------------------------------------------------* 
      *  OUTPUT...: ARQ. DE DADOS DA PESSOA                           * 
      *             ORG. SEQUENCIAL   -   LRECL = 223 BYTES           * 
      *---------------------------------------------------------------* 
                                                                        
       FD  ARQSAIDA                                                     
           LABEL       RECORD    STANDARD                               
           RECORDING   MODE      F                                      
           BLOCK       CONTAINS  0.                                     
                                                                        
                                                                        
       COPY 'I#RDAB17'.                                                 
                                                                        
      *---------------------------------------------------------------* 
       WORKING-STORAGE SECTION.                                         
      *---------------------------------------------------------------* 
       01   FILLER                     PIC  X(32) VALUE                 
            'INICIO DA WORKING RDAB0901'.                               
                                                                        
      *-- ------------------------------------------------------------* 
      *          CAMPOS UTILIZADOS PARA CANCELAR PROCESSAMENTO        * 
      *---------------------------------------------------------------* 
                                                                        
       01  FILLER.                                                      
           03  WRK-FS-ARQPARVV         PIC  X(02) VALUE SPACES.         
           03  WRK-FS-CADUV001         PIC  X(02) VALUE SPACES.         
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
           03  ACU-LDS-ARQPARVV        PIC 9(09) COMP-3    VALUE  ZEROS.
           03  ACU-LDS-CADUV001        PIC 9(09) COMP-3    VALUE  ZEROS.
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
BRQ059 01  WRK-CFONE-NOVO-9            PIC  9(11) VALUE ZEROS.          
BRQ059 01  WRK-CFONE-NOVO-X            REDEFINES                        
BRQ059     WRK-CFONE-NOVO-9            PIC  X(11).                      
                                                                        
      *--  CHAVE ARQPARVV.                                              
       01  WRK-CHV-ARQPARVV.                                            
           03  WRK-CHV-PAR-CPF         PIC 9(09) VALUE  ZEROS.          
           03  WRK-CHV-PAR-CFILIAL     PIC 9(05) VALUE  ZEROS.          
           03  WRK-CHV-PAR-CCTRL       PIC 9(03) VALUE  ZEROS.          
                                                                        
      *--  CHAVE CADUV001.                                              
       01  WRK-CHV-CADUV001.                                            
           03  WRK-CHV-001-CPF         PIC 9(09) VALUE  ZEROS.          
           03  WRK-CHV-001-CFILIAL     PIC 9(05) VALUE  ZEROS.          
           03  WRK-CHV-001-CCTRL       PIC 9(03) VALUE  ZEROS.          
                                                                        
      *----------------------------------------------------------------*
       01  FILLER   PIC  X(32) VALUE '*       AREA DA BRAD7100       *'.
      *----------------------------------------------------------------*
                                                                        
       COPY 'I#BRAD7C'.                                                 
                                                                        
      *---------------------------------------------------------------* 
       01   FILLER                     PIC  X(32)    VALUE              
            'FIM DA WORKING RDAB0901'.                                  
                                                                        
      *===============================================================* 
       PROCEDURE DIVISION.                                              
      *===============================================================* 
                                                                        
      *---------------------------------------------------------------* 
       0000-INICIAR SECTION.                                            
      *---------------------------------------------------------------* 
                                                                        
           PERFORM 1000-INICIALIZAR.                                    
                                                                        
           PERFORM 2000-LER-ARQPARVV                                    
           IF      WRK-FS-ARQPARVV     EQUAL '10'                       
                   DISPLAY '**************** RDAB0901 ***************'  
                   DISPLAY '*                                       *'  
                   DISPLAY '* ARQUIVO DE ENTRADA - ARQPARVV - VAZIO *'  
                   DISPLAY '*        PROCESSAMENTO ENCERRADO        *'  
                   DISPLAY '*                                       *'  
                   DISPLAY '**************** RDAB0901 ***************'  
                   PERFORM  9000-FINALIZAR                              
           END-IF.                                                      
                                                                        
           PERFORM 3000-LER-CADUV001                                    
           IF      WRK-FS-CADUV001     EQUAL '10'                       
                   DISPLAY '**************** RDAB0901 ***************'  
                   DISPLAY '*                                       *'  
                   DISPLAY '* ARQUIVO DE ENTRADA - CADUV001 - VAZIO *'  
                   DISPLAY '*        PROCESSAMENTO ENCERRADO        *'  
                   DISPLAY '*                                       *'  
                   DISPLAY '**************** RDAB0901 ***************'  
           END-IF                                                       
                                                                        
           PERFORM 4000-PROCESSAR     UNTIL                             
                  (WRK-FS-ARQPARVV     EQUAL '10')                      
                                                                        
                                                                        
           PERFORM 7000-DISPLAY-TOTAIS                                  
                                                                        
           PERFORM 9000-FINALIZAR.                                      
                                                                        
      *---------------------------------------------------------------* 
       0000-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1000-INICIALIZAR              SECTION.                           
      *---------------------------------------------------------------* 
                                                                        
           OPEN  INPUT  ARQPARVV                                        
                        CADUV001                                        
                 OUTPUT ARQSAIDA                                        
                                                                        
           MOVE  WRK-ABERTURA          TO    WRK-OPERACAO               
           PERFORM  1100-TESTAR-FILE-STATUS.                            
                                                                        
      *---------------------------------------------------------------* 
       1000-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1100-TESTAR-FILE-STATUS       SECTION.                           
      *---------------------------------------------------------------* 
                                                                        
           PERFORM  1110-TESTAR-FS-ARQPARVV                             
           PERFORM  1120-TESTAR-FS-CADUV001                             
           PERFORM  1150-TESTAR-FS-ARQSAIDA.                            
                                                                        
      *---------------------------------------------------------------* 
       1100-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1110-TESTAR-FS-ARQPARVV       SECTION.                           
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-FS-ARQPARVV         NOT EQUAL  '00'                   
              DISPLAY '************** RDAB0901 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              ARQPARVV             *'           
              DISPLAY '*         FILE STATUS =  ' WRK-FS-ARQPARVV       
                                                 '         *'           
              DISPLAY '************** RDAB0901 *************'           
              PERFORM 9999-ROTINA-ERRO                                  
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       1110-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1120-TESTAR-FS-CADUV001       SECTION.                           
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-FS-CADUV001         NOT EQUAL  '00'                   
              DISPLAY '************** RDAB0901 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              CADUV001             *'           
              DISPLAY '*         FILE STATUS =  ' WRK-FS-CADUV001       
                                                 '         *'           
              DISPLAY '************** RDAB0901 *************'           
              PERFORM 9999-ROTINA-ERRO                                  
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       1120-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1150-TESTAR-FS-ARQSAIDA        SECTION.                          
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-FS-ARQSAIDA           NOT EQUAL  '00'                 
              DISPLAY '************** RDAB0901 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              ARQSAIDA              *'          
              DISPLAY '*         FILE STATUS =  ' WRK-FS-ARQSAIDA       
                                                 '         *'           
              DISPLAY '************** RDAB0901 *************'           
              PERFORM 9999-ROTINA-ERRO                                  
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       1150-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       2000-LER-ARQPARVV            SECTION.                            
      *---------------------------------------------------------------* 
                                                                        
           READ    ARQPARVV                                             
                                                                        
           IF      WRK-FS-ARQPARVV     EQUAL  '10'                      
                   MOVE HIGH-VALUES    TO      WRK-CHV-ARQPARVV         
                   GO                  TO      2000-99-FIM              
           END-IF.                                                      
                                                                        
           MOVE    WRK-LEITURA         TO      WRK-OPERACAO             
           PERFORM 1110-TESTAR-FS-ARQPARVV                              
                                                                        
           MOVE SOC-CGCNUM OF ARQPARVV TO  WRK-CHV-PAR-CPF.             
           MOVE SOC-CGCFIL OF ARQPARVV TO  WRK-CHV-PAR-CFILIAL.         
           MOVE SOC-CGCCTR OF ARQPARVV TO  WRK-CHV-PAR-CCTRL.           
                                                                        
           ADD     1                      TO  ACU-LDS-ARQPARVV.         
                                                                        
      *---------------------------------------------------------------* 
       2000-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       3000-LER-CADUV001            SECTION.                            
      *---------------------------------------------------------------* 
                                                                        
           READ    CADUV001                                             
                                                                        
           IF      WRK-FS-CADUV001     EQUAL  '10'                      
                   MOVE HIGH-VALUES    TO      WRK-CHV-CADUV001         
                   GO                  TO      3000-99-FIM              
           END-IF.                                                      
                                                                        
           MOVE    WRK-LEITURA         TO      WRK-OPERACAO             
           PERFORM 1120-TESTAR-FS-CADUV001                              
                                                                        
                                                                        
           MOVE CADUV001-CCPF-CNPJ         TO  WRK-CHV-001-CPF.         
           MOVE CADUV001-CFLIAL-CPF-CNPJ   TO  WRK-CHV-001-CFILIAL.     
           MOVE CADUV001-CCTRL-CPF-CNPJ    TO  WRK-CHV-001-CCTRL.       
                                                                        
           ADD     1                   TO      ACU-LDS-CADUV001.        
                                                                        
      *---------------------------------------------------------------* 
       3000-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       4000-PROCESSAR               SECTION.                            
      *---------------------------------------------------------------* 
                                                                        
           IF      WRK-CHV-ARQPARVV   GREATER   WRK-CHV-CADUV001        
                   PERFORM 3000-LER-CADUV001                            
           ELSE                                                         
              IF      WRK-CHV-ARQPARVV    LESS    WRK-CHV-CADUV001      
                      MOVE SOC-REGISTRO OF ARQPARVV                     
                                    TO SOC-REGISTRO OF ARQSAIDA         
                      PERFORM 5000-GRAVAR-ARQSAIDA                      
                      PERFORM 2000-LER-ARQPARVV                         
              ELSE                                                      
                      PERFORM 4200-CHAVES-IGUAIS                        
                      PERFORM 3000-LER-CADUV001                         
                      PERFORM 2000-LER-ARQPARVV                         
              END-IF                                                    
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       4000-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       4200-CHAVES-IGUAIS           SECTION.                            
      *---------------------------------------------------------------* 
                                                                        
           INITIALIZE SOC-REGISTRO OF ARQSAIDA.                         
           MOVE SOC-REGISTRO       OF ARQPARVV                          
                        TO SOC-REGISTRO OF ARQSAIDA                     
           IF SOC-CGCFIL OF ARQPARVV EQUAL ZEROS                        
               PERFORM 4300-MOVER-ARQPARVV                              
           END-IF.                                                      
                                                                        
           PERFORM 5000-GRAVAR-ARQSAIDA.                                
                                                                        
      *---------------------------------------------------------------* 
       4200-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       4300-MOVER-ARQPARVV           SECTION.                           
      *---------------------------------------------------------------* 
                                                                        
               MOVE SPACES      TO  SOC-EBAIRO OF ARQSAIDA              
                                    SOC-CLETRA-RTING OF ARQSAIDA.       
               MOVE CADUV001-CCLUB        TO SOC-CCLUB-CADTR OF ARQSAIDA
               MOVE CADUV001-ELOGDR-PSSOA(1:40)                         
                                          TO SOC-ELOGDR OF ARQSAIDA     
               MOVE CADUV001-ELOGDR-NRO   TO SOC-ENRO-LOGDR OF ARQSAIDA 
               MOVE CADUV001-RCOMPL-ENDER(1:20)                         
                                         TO SOC-ECOMPL-LOGDR OF ARQSAIDA
               MOVE CADUV001-CSGL-UF      TO SOC-CSGL-UF OF ARQSAIDA    
               MOVE CADUV001-CCEP         TO SOC-CCEP OF ARQSAIDA       
               MOVE CADUV001-CCEP-COMPL   TO SOC-CCEP-COMPL OF ARQSAIDA 
               MOVE CADUV001-CDDD         TO SOC-CDDD OF ARQSAIDA       
               MOVE CADUV001-CFONE-NOVO   TO WRK-CFONE-NOVO-9.          
               MOVE WRK-CFONE-NOVO-X      TO SOC-CFONE OF ARQSAIDA.     
                                                                        
      *---------------------------------------------------------------* 
       4300-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       5000-GRAVAR-ARQSAIDA          SECTION.                           
      *---------------------------------------------------------------* 
                                                                        
           WRITE  SOC-REGISTRO OF ARQSAIDA                              
           MOVE   WRK-GRAVACAO         TO      WRK-OPERACAO             
           PERFORM  1150-TESTAR-FS-ARQSAIDA                             
                                                                        
           ADD 1                      TO   ACU-GRV-ARQSAIDA.            
                                                                        
      *---------------------------------------------------------------* 
       5000-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       7000-DISPLAY-TOTAIS        SECTION.                              
      *---------------------------------------------------------------* 
                                                                        
           MOVE     ACU-LDS-ARQPARVV   TO    WRK-EDIT01                 
           MOVE     ACU-LDS-CADUV001   TO    WRK-EDIT02                 
           MOVE     ACU-GRV-ARQSAIDA   TO    WRK-EDIT05                 
                                                                        
           DISPLAY '******************** RDAB0901 ********************' 
           DISPLAY '*                                                *' 
           DISPLAY '*  TOTAL REG. LIDOS    - ARQPARVV : 'WRK-EDIT01'  *'
           DISPLAY '*  TOTAL REG. LIDOS    - CADUV001 : 'WRK-EDIT02'  *'
           DISPLAY '*  TOTAL REG. GRAVADOS - ARQSAIDA : 'WRK-EDIT05'  *'
           DISPLAY '*                                                *' 
           DISPLAY '******************** RDAB0901 ********************'.
                                                                        
      *---------------------------------------------------------------* 
       7000-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       9000-FINALIZAR             SECTION.                              
      *---------------------------------------------------------------* 
                                                                        
           CLOSE  ARQPARVV                                              
                  CADUV001                                              
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
                                                                        
           MOVE   'RDAB0901'           TO  ERR-PGM.                     
           MOVE   'APL'                TO ERR-TIPO-ACESSO.              
           CALL   'BRAD7100'        USING  WRK-BATCH                    
                                           ERRO-AREA.                   
                                                                        
           GOBACK.                                                      
                                                                        
      *----------------------------------------------------------------*
       9999-99-FIM. EXIT.                                               
      *----------------------------------------------------------------*
                                                                        

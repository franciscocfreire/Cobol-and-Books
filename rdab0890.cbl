      *===============================================================* 
       IDENTIFICATION DIVISION.                                         
      *===============================================================* 
                                                                        
       PROGRAM-ID. RDAB0890.                                            
       AUTHOR. FRANCISCO.                                               
                                                                        
      *===============================================================* 
      *                 B R Q    I T   S E R V I C E S                * 
      *---------------------------------------------------------------* 
      *                                                               * 
      *   PROGRAMA    : RDAB0890                                      * 
      *   PROGRAMADOR : FRANCISCO FREIRE       - BRQ IT SERVICES      * 
      *   ANALISTA    : FRANCISCO FREIRE       - BRQ IT SERVICES      * 
      *   SUPERVISOR  : JUNIOR RIBAMAR         - GP. 70               * 
      *   DATA        : ABRIL/2014                                    * 
      *                                                               * 
      *   OBJETIVO    : BALANCE LINE DE DADOS PRINCIPAIS DO CADU COM  * 
      *                 INFORMACOES DO CADASTRO DE CLIENTES COM PENDE * 
      *                 NCIAS A SEREM RENEGOCIADAS                    * 
      *   ARQUIVOS:                                                   * 
      *   ---------------------------------------------------------   * 
      *   DDNAME   |I/O| DESCRICAO               | BOOK     | LRECL   * 
      *   ---------+---+-------------------------+----------+------   * 
      *   CADUB0A4 | I | DADOS PRINCIPAIS        | PSDCW033 |   642   * 
      *   REGPENDF | I | PENDENCIA RENEGOCIADAS  | I#CLLPRC |   480   * 
      *   ARQSAIDA | O | DADOS PEND+CLUB         | I#RDABAS |   500   * 
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
                                                                        
           SELECT  CADUB0A4  ASSIGN  TO  UT-S-CADUB0A4                  
                   FILE      STATUS  IS  WRK-FS-CADUB0A4.               
                                                                        
           SELECT  REGPENDF  ASSIGN  TO  UT-S-REGPENDF                  
                   FILE      STATUS  IS  WRK-FS-REGPENDF.               
                                                                        
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
      *             ORG. SEQUENCIAL    -  LRECL = 642 BYTES           * 
      *---------------------------------------------------------------* 
                                                                        
       FD  CADUB0A4                                                     
           LABEL       RECORD    STANDARD                               
           RECORDING   MODE      F                                      
           BLOCK       CONTAINS  0.                                     
                                                                        
       COPY 'PSDCW033'.                                                 
      *---------------------------------------------------------------* 
      *  INPUT....: ARQ. ENDERECO                                     * 
      *             ORG. SEQUENCIAL    -  LRECL = 480 BYTES           * 
      *---------------------------------------------------------------* 
                                                                        
       FD  REGPENDF                                                     
           LABEL       RECORD    STANDARD                               
           RECORDING   MODE      F                                      
           BLOCK       CONTAINS  0.                                     
                                                                        
       COPY 'I#CLLPRC'.                                                 
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
            'INICIO DA WORKING RDAB0890'.                               
                                                                        
      *-- ------------------------------------------------------------* 
      *          CAMPOS UTILIZADOS PARA CANCELAR PROCESSAMENTO        * 
      *---------------------------------------------------------------* 
                                                                        
       01  FILLER.                                                      
           03  WRK-FS-CADUB0A4         PIC  X(02) VALUE SPACES.         
           03  WRK-FS-REGPENDF         PIC  X(02) VALUE SPACES.         
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
           03  ACU-LDS-CADUB0A4        PIC 9(09) COMP-3    VALUE  ZEROS.
           03  ACU-LDS-REGPENDF        PIC 9(09) COMP-3    VALUE  ZEROS.
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
                                                                        
       01  WRK-AUX-NUM005              PIC 9(05) VALUE  ZEROS.          
       01  WRK-AUX-NUM005-S            REDEFINES                        
           WRK-AUX-NUM005              PIC S9(05).                      
                                                                        
       01  WRK-AUX-NUM013              PIC 9(13) VALUE  ZEROS.          
       01  WRK-AUX-NUM013-S            REDEFINES                        
           WRK-AUX-NUM013              PIC S9(13).                      
                                                                        
       01  WRK-DATA-DB2.                                                
           03  WRK-DD-DB2               PIC 9(02) VALUE ZEROS.          
           03  FILLER                   PIC X(01) VALUE SPACES.         
           03  WRK-MM-DB2               PIC 9(02) VALUE ZEROS.          
           03  FILLER                   PIC X(01) VALUE SPACES.         
           03  WRK-AA-DB2               PIC 9(04) VALUE ZEROS.          
                                                                        
       01  WRK-DATA-NUM                 PIC 9(08) VALUE ZEROS.          
       01  FILLER           REDEFINES   WRK-DATA-NUM.                   
           03  WRK-AA-NUM               PIC 9(04).                      
           03  WRK-MM-NUM               PIC 9(02).                      
           03  WRK-DD-NUM               PIC 9(02).                      
                                                                        
      *--  CHAVE CADUB0A4.                                              
       01  WRK-CHV-CADUB0A4.                                            
           03  WRK-CHV-0A4-CAG         PIC 9(05) VALUE  ZEROS.          
           03  WRK-CHV-0A4-CCTA        PIC 9(13) VALUE  ZEROS.          
                                                                        
                                                                        
      *--  CHAVE REGPENDF.                                              
       01  WRK-CHV-REGPENDF.                                            
           03  WRK-CHV-RPF-CAG         PIC 9(05) VALUE  ZEROS.          
           03  WRK-CHV-RPF-CCTA        PIC 9(13) VALUE  ZEROS.          
                                                                        
                                                                        
      *----------------------------------------------------------------*
       01  FILLER   PIC  X(32) VALUE '*       AREA DA BRAD7100       *'.
      *----------------------------------------------------------------*
                                                                        
       COPY 'I#BRAD7C'.                                                 
                                                                        
      *---------------------------------------------------------------* 
       01   FILLER                     PIC  X(32)    VALUE              
            'FIM DA WORKING RDAB0890'.                                  
                                                                        
      *===============================================================* 
       PROCEDURE DIVISION.                                              
      *===============================================================* 
                                                                        
      *---------------------------------------------------------------* 
       0000-INICIAR SECTION.                                            
      *---------------------------------------------------------------* 
                                                                        
           PERFORM 1000-INICIALIZAR.                                    
                                                                        
           PERFORM 2000-LER-CADUB0A4                                    
           IF      WRK-FS-CADUB0A4     EQUAL '10'                       
                   DISPLAY '**************** RDAB0890 ***************'  
                   DISPLAY '*                                       *'  
                   DISPLAY '* ARQUIVO DE ENTRADA - CADUB0A4 - VAZIO *'  
                   DISPLAY '*        PROCESSAMENTO ENCERRADO        *'  
                   DISPLAY '*                                       *'  
                   DISPLAY '**************** RDAB0890 ***************'  
           END-IF.                                                      
                                                                        
           PERFORM 3000-LER-REGPENDF                                    
           IF      WRK-FS-REGPENDF     EQUAL '10'                       
                   DISPLAY '**************** RDAB0890 ***************'  
                   DISPLAY '*                                       *'  
                   DISPLAY '* ARQUIVO DE ENTRADA - REGPENDF - VAZIO *'  
                   DISPLAY '*        PROCESSAMENTO ENCERRADO        *'  
                   DISPLAY '*                                       *'  
                   DISPLAY '**************** RDAB0890 ***************'  
                   PERFORM  9000-FINALIZAR                              
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
                                                                        
           OPEN  INPUT  CADUB0A4                                        
                        REGPENDF                                        
                 OUTPUT ARQSAIDA                                        
                                                                        
           MOVE  WRK-ABERTURA          TO    WRK-OPERACAO               
           PERFORM  1100-TESTAR-FILE-STATUS.                            
                                                                        
      *---------------------------------------------------------------* 
       1000-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1100-TESTAR-FILE-STATUS       SECTION.                           
      *---------------------------------------------------------------* 
                                                                        
           PERFORM  1110-TESTAR-FS-CADUB0A4                             
           PERFORM  1120-TESTAR-FS-REGPENDF                             
           PERFORM  1150-TESTAR-FS-ARQSAIDA.                            
                                                                        
      *---------------------------------------------------------------* 
       1100-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1110-TESTAR-FS-CADUB0A4       SECTION.                           
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-FS-CADUB0A4         NOT EQUAL  '00'                   
              DISPLAY '************** RDAB0890 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              CADUB0A4             *'           
              DISPLAY '*         FILE STATUS =  ' WRK-FS-CADUB0A4       
                                                 '         *'           
              DISPLAY '************** RDAB0890 *************'           
              PERFORM 9999-ROTINA-ERRO                                  
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       1110-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1120-TESTAR-FS-REGPENDF       SECTION.                           
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-FS-REGPENDF         NOT EQUAL  '00'                   
              DISPLAY '************** RDAB0890 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              REGPENDF             *'           
              DISPLAY '*         FILE STATUS =  ' WRK-FS-REGPENDF       
                                                 '         *'           
              DISPLAY '************** RDAB0890 *************'           
              PERFORM 9999-ROTINA-ERRO                                  
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       1120-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1150-TESTAR-FS-ARQSAIDA        SECTION.                          
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-FS-ARQSAIDA           NOT EQUAL  '00'                 
              DISPLAY '************** RDAB0890 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              ARQSAIDA              *'          
              DISPLAY '*         FILE STATUS =  ' WRK-FS-ARQSAIDA       
                                                 '         *'           
              DISPLAY '************** RDAB0890 *************'           
              PERFORM 9999-ROTINA-ERRO                                  
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       1150-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       2000-LER-CADUB0A4            SECTION.                            
      *---------------------------------------------------------------* 
                                                                        
           READ    CADUB0A4                                             
                                                                        
           IF      WRK-FS-CADUB0A4     EQUAL  '10'                      
                   MOVE HIGH-VALUES    TO      WRK-CHV-CADUB0A4         
                   GO                  TO      2000-99-FIM              
           END-IF.                                                      
                                                                        
           MOVE    WRK-LEITURA         TO      WRK-OPERACAO             
           PERFORM 1110-TESTAR-FS-CADUB0A4                              
                                                                        
           MOVE PSDCW033-01-CAG-COBR-TARIF OF CADUB0A4                  
                                       TO  WRK-AUX-NUM005-S.            
           MOVE WRK-AUX-NUM005         TO  WRK-CHV-0A4-CAG.             
                                                                        
           MOVE PSDCW033-01-CCTA-COBR-TARIF OF CADUB0A4                 
                                       TO  WRK-AUX-NUM013-S.            
           MOVE WRK-AUX-NUM013         TO  WRK-CHV-0A4-CCTA.            
                                                                        
           ADD     1                      TO  ACU-LDS-CADUB0A4.         
                                                                        
      *---------------------------------------------------------------* 
       2000-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       3000-LER-REGPENDF            SECTION.                            
      *---------------------------------------------------------------* 
                                                                        
           READ    REGPENDF                                             
                                                                        
           IF      WRK-FS-REGPENDF     EQUAL  '10'                      
                   MOVE HIGH-VALUES    TO      WRK-CHV-REGPENDF         
                   GO                  TO      3000-99-FIM              
           END-IF.                                                      
                                                                        
           MOVE    WRK-LEITURA         TO      WRK-OPERACAO             
           PERFORM 1120-TESTAR-FS-REGPENDF                              
                                                                        
           MOVE RF-AGENCIA             TO  WRK-CHV-RPF-CAG.             
           MOVE RF-CONTA               TO  WRK-CHV-RPF-CCTA.            
                                                                        
           ADD     1                   TO      ACU-LDS-REGPENDF.        
                                                                        
      *---------------------------------------------------------------* 
       3000-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       4000-PROCESSAR               SECTION.                            
      *---------------------------------------------------------------* 
                                                                        
           IF      WRK-CHV-CADUB0A4   GREATER   WRK-CHV-REGPENDF        
                   MOVE REG-PEND-FINAL TO  REG-RPF                      
                   MOVE ZEROS          TO  RPF-CLUB                     
                                           RPF-DT-NASC-FICA             
                                           RPF-QREG-ENDER               
                                           RPF-QREG-CNTRL-FONE          
                                           RPF-RG-FICA                  
                                                                        
                   PERFORM 5000-GRAVAR-ARQSAIDA                         
                   PERFORM 3000-LER-REGPENDF                            
           ELSE                                                         
              IF      WRK-CHV-CADUB0A4    LESS    WRK-CHV-REGPENDF      
                      PERFORM 2000-LER-CADUB0A4                         
              ELSE                                                      
                      PERFORM 4200-CHAVES-IGUAIS                        
                      PERFORM 2000-LER-CADUB0A4                         
                      PERFORM 3000-LER-REGPENDF                         
              END-IF                                                    
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       4000-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       4200-CHAVES-IGUAIS           SECTION.                            
      *---------------------------------------------------------------* 
                                                                        
           INITIALIZE REG-RDABAS.                                       
           MOVE REG-PEND-FINAL TO REG-RPF.                              
                                                                        
           MOVE PSDCW033-01-CCLUB          TO WRK-AUX-NUM010-S.         
           MOVE WRK-AUX-NUM010          TO RPF-CLUB.                    
                                                                        
           MOVE PSDCW033-01-DNASC-FUNDC    TO WRK-DATA-DB2.             
           MOVE WRK-DD-DB2          TO WRK-DD-NUM.                      
           MOVE WRK-MM-DB2          TO WRK-MM-NUM.                      
           MOVE WRK-AA-DB2          TO WRK-AA-NUM.                      
           MOVE WRK-DATA-NUM        TO RPF-DT-NASC-FICA.                
                                                                        
           MOVE PSDCW033-01-QREG-CNTRL-FONE TO WRK-AUX-NUM003-S.        
           MOVE WRK-AUX-NUM003 TO RPF-QREG-CNTRL-FONE.                  
                                                                        
           MOVE PSDCW033-01-QREG-ENDER      TO WRK-AUX-NUM003-S.        
           MOVE WRK-AUX-NUM003      TO RPF-QREG-ENDER.                  
                                                                        
           MOVE ZEROS               TO RPF-RG-FICA.                     
                                                                        
           PERFORM 5000-GRAVAR-ARQSAIDA.                                
                                                                        
      *---------------------------------------------------------------* 
       4200-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       5000-GRAVAR-ARQSAIDA          SECTION.                           
      *---------------------------------------------------------------* 
                                                                        
           WRITE  REG-RDABAS                                            
           MOVE   WRK-GRAVACAO         TO      WRK-OPERACAO             
           PERFORM  1150-TESTAR-FS-ARQSAIDA                             
                                                                        
           ADD 1                      TO   ACU-GRV-ARQSAIDA.            
                                                                        
      *---------------------------------------------------------------* 
       5000-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       7000-DISPLAY-TOTAIS        SECTION.                              
      *---------------------------------------------------------------* 
                                                                        
           MOVE     ACU-LDS-CADUB0A4   TO    WRK-EDIT01                 
           MOVE     ACU-LDS-REGPENDF   TO    WRK-EDIT02                 
           MOVE     ACU-GRV-ARQSAIDA   TO    WRK-EDIT05                 
                                                                        
           DISPLAY '******************** RDAB0890 ********************' 
           DISPLAY '*                                                *' 
           DISPLAY '*  TOTAL REG. LIDOS    - CADUB0A4 : 'WRK-EDIT01'  *'
           DISPLAY '*  TOTAL REG. LIDOS    - REGPENDF : 'WRK-EDIT02'  *'
           DISPLAY '*  TOTAL REG. GRAVADOS - ARQSAIDA : 'WRK-EDIT05'  *'
           DISPLAY '*                                                *' 
           DISPLAY '******************** RDAB0890 ********************'.
                                                                        
      *---------------------------------------------------------------* 
       7000-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       9000-FINALIZAR             SECTION.                              
      *---------------------------------------------------------------* 
                                                                        
           CLOSE  CADUB0A4                                              
                  REGPENDF                                              
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
                                                                        
           MOVE   'RDAB0890'           TO  ERR-PGM.                     
           MOVE   'APL'                TO ERR-TIPO-ACESSO.              
           CALL   'BRAD7100'        USING  WRK-BATCH                    
                                           ERRO-AREA.                   
                                                                        
           GOBACK.                                                      
                                                                        
      *----------------------------------------------------------------*
       9999-99-FIM. EXIT.                                               
      *----------------------------------------------------------------*
                                                                        

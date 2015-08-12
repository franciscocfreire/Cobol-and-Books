      *===============================================================* 
       IDENTIFICATION DIVISION.                                         
      *===============================================================* 
                                                                        
       PROGRAM-ID. RDAB0885.                                            
       AUTHOR. FRANCISCO.                                               
                                                                        
      *===============================================================* 
      *                 B R Q    I T   S E R V I C E S                * 
      *---------------------------------------------------------------* 
      *                                                               * 
      *   PROGRAMA    : RDAB0885                                      * 
      *   PROGRAMADOR : FRANCISCO FREIRE       - BRQ IT SERVICES      * 
      *   ANALISTA    : FRANCISCO FREIRE       - BRQ IT SERVICES      * 
      *   SUPERVISOR  : JUNIOR RIBAMAR         - GP. 70               * 
      *   DATA        : ABRIL/2014                                    * 
      *                                                               * 
      *   OBJETIVO    : COMPLEMENTA INFORMACOES PROFISSIONAL NO       * 
      *                 LAYOUT                                        * 
      *   ARQUIVOS:                                                   * 
      *   ---------------------------------------------------------   * 
      *   DDNAME   |I/O| DESCRICAO               | BOOK     | LRECL   * 
      *   ---------+---+-------------------------+----------+------   * 
      *   CADUV001 | I | CADUV001                | I#RDABAR |   480   * 
      *   CADUB067 | I | INFORMACOES FAMILIARES  | PSDCW033 |   642   * 
      *   ARQSAIDA | O | CADUV001 ATUALIZADO     | I#RDABAR |   480   * 
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
                                                                        
           SELECT  CADUV001  ASSIGN  TO  UT-S-CADUV001                  
                   FILE      STATUS  IS  WRK-FS-CADUV001.               
                                                                        
           SELECT  CADUB067  ASSIGN  TO  UT-S-CADUB067                  
                   FILE      STATUS  IS  WRK-FS-CADUB067.               
                                                                        
           SELECT  ARQSAIDA  ASSIGN  TO  UT-S-ARQSAIDA                  
                   FILE      STATUS  IS  WRK-FS-ARQSAIDA.               
                                                                        
      *===============================================================* 
       DATA DIVISION.                                                   
      *===============================================================* 
                                                                        
      *---------------------------------------------------------------* 
       FILE SECTION.                                                    
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
      *  INPUT....: ARQ. CADUV001                                     * 
      *             ORG. SEQUENCIAL    -  LRECL = 480 BYTES           * 
      *---------------------------------------------------------------* 
                                                                        
       FD  CADUV001                                                     
           LABEL       RECORD    STANDARD                               
           RECORDING   MODE      F                                      
           BLOCK       CONTAINS  0.                                     
                                                                        
       COPY 'I#RDABAR'.                                                 
      *---------------------------------------------------------------* 
      *  INPUT....: ARQ. TELEFONES                                    * 
      *             ORG. SEQUENCIAL    -  LRECL = 642 BYTES           * 
      *---------------------------------------------------------------* 
                                                                        
       FD  CADUB067                                                     
           LABEL       RECORD    STANDARD                               
           RECORDING   MODE      F                                      
           BLOCK       CONTAINS  0.                                     
                                                                        
       COPY 'PSDCW033'.                                                 
      *---------------------------------------------------------------* 
      *  OUTPUT...: ARQ. CADUV001 ATUALIZADO                          * 
      *             ORG. SEQUENCIAL   -   LRECL = 480 BYTES           * 
      *---------------------------------------------------------------* 
                                                                        
       FD  ARQSAIDA                                                     
           LABEL       RECORD    STANDARD                               
           RECORDING   MODE      F                                      
           BLOCK       CONTAINS  0.                                     
                                                                        
                                                                        
       COPY 'I#RDABAR'.                                                 
                                                                        
      *---------------------------------------------------------------* 
       WORKING-STORAGE SECTION.                                         
      *---------------------------------------------------------------* 
       01   FILLER                     PIC  X(32) VALUE                 
            'INICIO DA WORKING RDAB0885'.                               
                                                                        
      *-- ------------------------------------------------------------* 
      *          CAMPOS UTILIZADOS PARA CANCELAR PROCESSAMENTO        * 
      *---------------------------------------------------------------* 
                                                                        
       01  FILLER.                                                      
           03  WRK-FS-CADUV001         PIC  X(02) VALUE SPACES.         
           03  WRK-FS-CADUB067         PIC  X(02) VALUE SPACES.         
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
           03  ACU-LDS-CADUV001        PIC 9(09) COMP-3    VALUE  ZEROS.
           03  ACU-LDS-CADUB067        PIC 9(09) COMP-3    VALUE  ZEROS.
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
       01  WRK-AUX-CGCNUM              PIC 9(09) VALUE  ZEROS.          
       01  WRK-AUX-CGCNUM-S            REDEFINES                        
           WRK-AUX-CGCNUM              PIC S9(09).                      
                                                                        
       01  WRK-AUX-CGCFIL              PIC 9(05) VALUE  ZEROS.          
       01  WRK-AUX-CGCFIL-S            REDEFINES                        
           WRK-AUX-CGCFIL              PIC S9(05).                      
                                                                        
       01  WRK-AUX-CGCCTR              PIC 9(02) VALUE  ZEROS.          
       01  WRK-AUX-CGCCTR-S            REDEFINES                        
           WRK-AUX-CGCCTR              PIC S9(02).                      
                                                                        
       01  WRK-AUX-NUM010              PIC 9(10) VALUE  ZEROS.          
       01  WRK-AUX-NUM010-S            REDEFINES                        
           WRK-AUX-NUM010              PIC S9(10).                      
                                                                        
       01  WRK-AUX-NUM013V             PIC 9(13)V99 VALUE  ZEROS.       
       01  WRK-AUX-NUM013V-S           REDEFINES                        
           WRK-AUX-NUM013V             PIC S9(13)V99.                   
                                                                        
                                                                        
      *--  CHAVE CADUV001.                                              
       01  WRK-CHV-CADUV001.                                            
           03  WRK-CHV-0A4-CCLUB       PIC 9(10) VALUE  ZEROS.          
                                                                        
      *--  CHAVE CADUB067.                                              
       01  WRK-CHV-CADUB067.                                            
           03  WRK-CHV-018-CCLUB       PIC 9(10) VALUE  ZEROS.          
                                                                        
      *----------------------------------------------------------------*
       01  FILLER   PIC  X(32) VALUE '*       AREA DA BRAD7100       *'.
      *----------------------------------------------------------------*
                                                                        
       COPY 'I#BRAD7C'.                                                 
                                                                        
      *---------------------------------------------------------------* 
       01   FILLER                     PIC  X(32)    VALUE              
            'FIM DA WORKING RDAB0885'.                                  
                                                                        
      *===============================================================* 
       PROCEDURE DIVISION.                                              
      *===============================================================* 
                                                                        
      *---------------------------------------------------------------* 
       0000-INICIAR SECTION.                                            
      *---------------------------------------------------------------* 
                                                                        
           PERFORM 1000-INICIALIZAR.                                    
                                                                        
           PERFORM 2000-LER-CADUV001                                    
           IF      WRK-FS-CADUV001     EQUAL '10'                       
                   DISPLAY '**************** RDAB0885 ***************'  
                   DISPLAY '*                                       *'  
                   DISPLAY '* ARQUIVO DE ENTRADA - CADUV001 - VAZIO *'  
                   DISPLAY '*        PROCESSAMENTO ENCERRADO        *'  
                   DISPLAY '*                                       *'  
                   DISPLAY '**************** RDAB0885 ***************'  
                   PERFORM  9000-FINALIZAR                              
           END-IF.                                                      
                                                                        
           PERFORM 3000-LER-CADUB067                                    
           IF      WRK-FS-CADUB067     EQUAL '10'                       
                   DISPLAY '**************** RDAB0885 ***************'  
                   DISPLAY '*                                       *'  
                   DISPLAY '* ARQUIVO DE ENTRADA - CADUB067 - VAZIO *'  
                   DISPLAY '*        PROCESSAMENTO ENCERRADO        *'  
                   DISPLAY '*                                       *'  
                   DISPLAY '**************** RDAB0885 ***************'  
           END-IF                                                       
                                                                        
           PERFORM 4000-PROCESSAR     UNTIL                             
                  (WRK-FS-CADUV001     EQUAL '10')                      
                                                                        
                                                                        
           PERFORM 7000-DISPLAY-TOTAIS                                  
                                                                        
           PERFORM 9000-FINALIZAR.                                      
                                                                        
      *---------------------------------------------------------------* 
       0000-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1000-INICIALIZAR              SECTION.                           
      *---------------------------------------------------------------* 
                                                                        
           OPEN  INPUT  CADUV001                                        
                        CADUB067                                        
                 OUTPUT ARQSAIDA                                        
                                                                        
           MOVE  WRK-ABERTURA          TO    WRK-OPERACAO               
           PERFORM  1100-TESTAR-FILE-STATUS.                            
                                                                        
      *---------------------------------------------------------------* 
       1000-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1100-TESTAR-FILE-STATUS       SECTION.                           
      *---------------------------------------------------------------* 
                                                                        
           PERFORM  1110-TESTAR-FS-CADUV001                             
           PERFORM  1120-TESTAR-FS-CADUB067                             
           PERFORM  1150-TESTAR-FS-ARQSAIDA.                            
                                                                        
      *---------------------------------------------------------------* 
       1100-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1110-TESTAR-FS-CADUV001       SECTION.                           
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-FS-CADUV001         NOT EQUAL  '00'                   
              DISPLAY '************** RDAB0885 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              CADUV001             *'           
              DISPLAY '*         FILE STATUS =  ' WRK-FS-CADUV001       
                                                 '         *'           
              DISPLAY '************** RDAB0885 *************'           
              PERFORM 9999-ROTINA-ERRO                                  
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       1110-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1120-TESTAR-FS-CADUB067       SECTION.                           
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-FS-CADUB067         NOT EQUAL  '00'                   
              DISPLAY '************** RDAB0885 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              CADUB067             *'           
              DISPLAY '*         FILE STATUS =  ' WRK-FS-CADUB067       
                                                 '         *'           
              DISPLAY '************** RDAB0885 *************'           
              PERFORM 9999-ROTINA-ERRO                                  
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       1120-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1150-TESTAR-FS-ARQSAIDA        SECTION.                          
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-FS-ARQSAIDA           NOT EQUAL  '00'                 
              DISPLAY '************** RDAB0885 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              ARQSAIDA              *'          
              DISPLAY '*         FILE STATUS =  ' WRK-FS-ARQSAIDA       
                                                 '         *'           
              DISPLAY '************** RDAB0885 *************'           
              PERFORM 9999-ROTINA-ERRO                                  
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       1150-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       2000-LER-CADUV001            SECTION.                            
      *---------------------------------------------------------------* 
                                                                        
           READ    CADUV001                                             
                                                                        
           IF      WRK-FS-CADUV001     EQUAL  '10'                      
                   MOVE HIGH-VALUES    TO      WRK-CHV-CADUV001         
                   GO                  TO      2000-99-FIM              
           END-IF.                                                      
                                                                        
           MOVE    WRK-LEITURA         TO      WRK-OPERACAO             
           PERFORM 1110-TESTAR-FS-CADUV001                              
                                                                        
           MOVE CADUV001-CCLUB OF CADUV001                              
                                       TO  WRK-CHV-0A4-CCLUB.           
                                                                        
           ADD     1                      TO  ACU-LDS-CADUV001.         
                                                                        
      *---------------------------------------------------------------* 
       2000-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       3000-LER-CADUB067            SECTION.                            
      *---------------------------------------------------------------* 
                                                                        
           READ    CADUB067                                             
                                                                        
           IF      WRK-FS-CADUB067     EQUAL  '10'                      
                   MOVE HIGH-VALUES    TO      WRK-CHV-CADUB067         
                   GO                  TO      3000-99-FIM              
           END-IF.                                                      
                                                                        
           MOVE    WRK-LEITURA         TO      WRK-OPERACAO             
           PERFORM 1120-TESTAR-FS-CADUB067                              
                                                                        
           MOVE PSDCW033-01-CCLUB OF CADUB067                           
                                       TO  WRK-AUX-NUM010-S             
           MOVE WRK-AUX-NUM010         TO  WRK-CHV-018-CCLUB.           
                                                                        
           ADD     1                   TO      ACU-LDS-CADUB067.        
                                                                        
      *---------------------------------------------------------------* 
       3000-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       4000-PROCESSAR               SECTION.                            
      *---------------------------------------------------------------* 
                                                                        
           IF      WRK-CHV-CADUV001   GREATER   WRK-CHV-CADUB067        
                   PERFORM 3000-LER-CADUB067                            
           ELSE                                                         
              IF      WRK-CHV-CADUV001    LESS    WRK-CHV-CADUB067
                      MOVE REG-CADUV001 OF CADUV001
                                          TO REG-CADUV001 OF ARQSAIDA
                      PERFORM 5000-GRAVAR-ARQSAIDA
                      PERFORM 2000-LER-CADUV001                         
              ELSE                                                      
                      PERFORM 4200-CHAVES-IGUAIS                        
                      PERFORM 2000-LER-CADUV001                         
                      PERFORM 3000-LER-CADUB067                         
              END-IF                                                    
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       4000-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       4200-CHAVES-IGUAIS           SECTION.                            
      *---------------------------------------------------------------* 
                                                                        
           MOVE REG-CADUV001 OF CADUV001   TO REG-CADUV001 OF ARQSAIDA. 
                                                                        
           MOVE PSDCW033-08-VRENDA-MES     TO WRK-AUX-NUM013V-S         
           MOVE WRK-AUX-NUM013V     TO CADUV001-VRENDA-MES OF ARQSAIDA. 
                                                                        
           PERFORM 5000-GRAVAR-ARQSAIDA.                                
                                                                        
      *---------------------------------------------------------------* 
       4200-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       5000-GRAVAR-ARQSAIDA          SECTION.                           
      *---------------------------------------------------------------* 
                                                                        
           WRITE REG-CADUV001 OF ARQSAIDA                               
           MOVE   WRK-GRAVACAO         TO      WRK-OPERACAO             
           PERFORM  1150-TESTAR-FS-ARQSAIDA                             
                                                                        
           ADD 1                      TO   ACU-GRV-ARQSAIDA.            
                                                                        
      *---------------------------------------------------------------* 
       5000-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       7000-DISPLAY-TOTAIS        SECTION.                              
      *---------------------------------------------------------------* 
                                                                        
           MOVE     ACU-LDS-CADUV001   TO    WRK-EDIT01                 
           MOVE     ACU-LDS-CADUB067   TO    WRK-EDIT02                 
           MOVE     ACU-GRV-ARQSAIDA   TO    WRK-EDIT05                 
                                                                        
           DISPLAY '******************** RDAB0885 ********************' 
           DISPLAY '*                                                *' 
           DISPLAY '*  TOTAL REG. LIDOS    - CADUV001 : 'WRK-EDIT01'  *'
           DISPLAY '*  TOTAL REG. LIDOS    - CADUB067 : 'WRK-EDIT02'  *'
           DISPLAY '*  TOTAL REG. GRAVADOS - ARQSAIDA : 'WRK-EDIT05'  *'
           DISPLAY '*                                                *' 
           DISPLAY '******************** RDAB0885 ********************'.
                                                                        
      *---------------------------------------------------------------* 
       7000-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       9000-FINALIZAR             SECTION.                              
      *---------------------------------------------------------------* 
                                                                        
           CLOSE  CADUV001                                              
                  CADUB067                                              
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
                                                                        
           MOVE   'RDAB0885'           TO  ERR-PGM.                     
           MOVE   'APL'                TO ERR-TIPO-ACESSO.              
           CALL   'BRAD7100'        USING  WRK-BATCH                    
                                           ERRO-AREA.                   
                                                                        
           GOBACK.                                                      
                                                                        
      *----------------------------------------------------------------*
       9999-99-FIM. EXIT.                                               
      *----------------------------------------------------------------*

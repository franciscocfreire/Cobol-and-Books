      *===============================================================* 
       IDENTIFICATION DIVISION.                                         
      *===============================================================* 
                                                                        
       PROGRAM-ID. RDAB0856.                                            
       AUTHOR. FRANCISCO.                                               
                                                                        
      *===============================================================* 
      *                 B R Q    I T   S E R V I C E S                * 
      *---------------------------------------------------------------* 
      *                                                               * 
      *   PROGRAMA    : RDAB0856                                      * 
      *   PROGRAMADOR : FRANCISCO FREIRE       - BRQ IT SERVICES      * 
      *   ANALISTA    : FRANCISCO FREIRE       - BRQ IT SERVICES      * 
      *   SUPERVISOR  : JUNIOR RIBAMAR         - GP. 70               * 
      *   DATA        : ABRIL/2014                                    * 
      *                                                               * 
      *   OBJETIVO    : COMPLEMENTA INFORMACOES DE DATA DE ATUALIZACAO* 
      *   ARQUIVOS:                                                   * 
      *   ---------------------------------------------------------   * 
      *   DDNAME   |I/O| DESCRICAO               | BOOK     | LRECL   * 
      *   ---------+---+-------------------------+----------+------   * 
      *   CADUV000 | I | CADUV000                | I#RDABAP |   320   * 
      *   CADUB0M1 | I | EMPRESA PESSOA UNICA    | PSDCW033 |   642   * 
      *   ARQSAIDA | O | CADUV000 ATUALIZADO     | I#RDABAP |   320   * 
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
                                                                        
           SELECT  CADUV000  ASSIGN  TO  UT-S-CADUV000                  
                   FILE      STATUS  IS  WRK-FS-CADUV000.               
                                                                        
           SELECT  CADUB0M1  ASSIGN  TO  UT-S-CADUB0M1                  
                   FILE      STATUS  IS  WRK-FS-CADUB0M1.               
                                                                        
           SELECT  ARQSAIDA  ASSIGN  TO  UT-S-ARQSAIDA                  
                   FILE      STATUS  IS  WRK-FS-ARQSAIDA.               
                                                                        
      *===============================================================* 
       DATA DIVISION.                                                   
      *===============================================================* 
                                                                        
      *---------------------------------------------------------------* 
       FILE SECTION.                                                    
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
      *  INPUT....: ARQ. CADUV000                                     * 
      *             ORG. SEQUENCIAL    -  LRECL = 320 BYTES           * 
      *---------------------------------------------------------------* 
                                                                        
       FD  CADUV000                                                     
           LABEL       RECORD    STANDARD                               
           RECORDING   MODE      F                                      
           BLOCK       CONTAINS  0.                                     
                                                                        
       COPY 'I#RDABAP'.                                                 
      *---------------------------------------------------------------* 
      *  INPUT....: ARQ. TELEFONES                                    * 
      *             ORG. SEQUENCIAL    -  LRECL = 642 BYTES           * 
      *---------------------------------------------------------------* 
                                                                        
       FD  CADUB0M1                                                     
           LABEL       RECORD    STANDARD                               
           RECORDING   MODE      F                                      
           BLOCK       CONTAINS  0.                                     
                                                                        
       COPY 'PSDCW033'.                                                 
      *---------------------------------------------------------------* 
      *  OUTPUT...: ARQ. CADUV000 ATUALIZADO                          * 
      *             ORG. SEQUENCIAL   -   LRECL = 320 BYTES           * 
      *---------------------------------------------------------------* 
                                                                        
       FD  ARQSAIDA                                                     
           LABEL       RECORD    STANDARD                               
           RECORDING   MODE      F                                      
           BLOCK       CONTAINS  0.                                     
                                                                        
                                                                        
       COPY 'I#RDABAP'.                                                 
                                                                        
      *---------------------------------------------------------------* 
       WORKING-STORAGE SECTION.                                         
      *---------------------------------------------------------------* 
       01   FILLER                     PIC  X(32) VALUE                 
            'INICIO DA WORKING RDAB0856'.                               
                                                                        
      *-- ------------------------------------------------------------* 
      *          CAMPOS UTILIZADOS PARA CANCELAR PROCESSAMENTO        * 
      *---------------------------------------------------------------* 
                                                                        
       01  FILLER.                                                      
           03  WRK-FS-CADUV000         PIC  X(02) VALUE SPACES.         
           03  WRK-FS-CADUB0M1         PIC  X(02) VALUE SPACES.         
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
           03  ACU-LDS-CADUV000        PIC 9(09) COMP-3    VALUE  ZEROS.
           03  ACU-LDS-CADUB0M1        PIC 9(09) COMP-3    VALUE  ZEROS.
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
                                                                        
       01  WRK-AUX-NUM008              PIC 9(08) VALUE  ZEROS.          
       01  WRK-AUX-NUM008-S            REDEFINES                        
           WRK-AUX-NUM008              PIC S9(08).                      
                                                                        
       01  WRK-AUX-NUM003              PIC 9(03) VALUE  ZEROS.          
       01  WRK-AUX-NUM003-S            REDEFINES                        
           WRK-AUX-NUM003              PIC S9(03).                      
                                                                        
      *--  CHAVE CADUV000.                                              
       01  WRK-CHV-CADUV000.                                            
           03  WRK-CHV-0A4-CGCNUM      PIC 9(09) VALUE  ZEROS.          
           03  WRK-CHV-0A4-CGCFIL      PIC 9(05) VALUE  ZEROS.          
           03  WRK-CHV-0A4-CGCCTR      PIC 9(02) VALUE  ZEROS.          
           03  WRK-CHV-0A4-CCLUB       PIC 9(10) VALUE  ZEROS.          
                                                                        
      *--  CHAVE CADUB0M1.                                              
       01  WRK-CHV-CADUB0M1.                                            
           03  WRK-CHV-0M1-CGCNUM      PIC 9(09) VALUE  ZEROS.          
           03  WRK-CHV-0M1-CGCFIL      PIC 9(05) VALUE  ZEROS.          
           03  WRK-CHV-0M1-CGCCTR      PIC 9(02) VALUE  ZEROS.          
           03  WRK-CHV-0M1-CCLUB       PIC 9(10) VALUE  ZEROS.          
                                                                        
      *----------------------------------------------------------------*
       01  FILLER   PIC  X(32) VALUE '*       AREA DA BRAD7100       *'.
      *----------------------------------------------------------------*
                                                                        
       COPY 'I#BRAD7C'.                                                 
                                                                        
      *---------------------------------------------------------------* 
       01   FILLER                     PIC  X(32)    VALUE              
            'FIM DA WORKING RDAB0856'.                                  
                                                                        
      *===============================================================* 
       PROCEDURE DIVISION.                                              
      *===============================================================* 
                                                                        
      *---------------------------------------------------------------* 
       0000-INICIAR SECTION.                                            
      *---------------------------------------------------------------* 
                                                                        
           PERFORM 1000-INICIALIZAR.                                    
                                                                        
           PERFORM 2000-LER-CADUV000                                    
           IF      WRK-FS-CADUV000     EQUAL '10'                       
                   DISPLAY '**************** RDAB0856 ***************'  
                   DISPLAY '*                                       *'  
                   DISPLAY '* ARQUIVO DE ENTRADA - CADUV000 - VAZIO *'  
                   DISPLAY '*        PROCESSAMENTO ENCERRADO        *'  
                   DISPLAY '*                                       *'  
                   DISPLAY '**************** RDAB0856 ***************'  
                   PERFORM  9000-FINALIZAR                              
           END-IF.                                                      
                                                                        
           PERFORM 3000-LER-CADUB0M1                                    
           IF      WRK-FS-CADUB0M1     EQUAL '10'                       
                   DISPLAY '**************** RDAB0856 ***************'  
                   DISPLAY '*                                       *'  
                   DISPLAY '* ARQUIVO DE ENTRADA - CADUB0M1 - VAZIO *'  
                   DISPLAY '*        PROCESSAMENTO ENCERRADO        *'  
                   DISPLAY '*                                       *'  
                   DISPLAY '**************** RDAB0856 ***************'  
           END-IF                                                       
                                                                        
           PERFORM 4000-PROCESSAR     UNTIL                             
                  (WRK-FS-CADUV000     EQUAL '10')                      
                                                                        
                                                                        
           PERFORM 7000-DISPLAY-TOTAIS                                  
                                                                        
           PERFORM 9000-FINALIZAR.                                      
                                                                        
      *---------------------------------------------------------------* 
       0000-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1000-INICIALIZAR              SECTION.                           
      *---------------------------------------------------------------* 
                                                                        
           OPEN  INPUT  CADUV000                                        
                        CADUB0M1                                        
                 OUTPUT ARQSAIDA                                        
                                                                        
           MOVE  WRK-ABERTURA          TO    WRK-OPERACAO               
           PERFORM  1100-TESTAR-FILE-STATUS.                            
                                                                        
      *---------------------------------------------------------------* 
       1000-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1100-TESTAR-FILE-STATUS       SECTION.                           
      *---------------------------------------------------------------* 
                                                                        
           PERFORM  1110-TESTAR-FS-CADUV000                             
           PERFORM  1120-TESTAR-FS-CADUB0M1                             
           PERFORM  1150-TESTAR-FS-ARQSAIDA.                            
                                                                        
      *---------------------------------------------------------------* 
       1100-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1110-TESTAR-FS-CADUV000       SECTION.                           
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-FS-CADUV000         NOT EQUAL  '00'                   
              DISPLAY '************** RDAB0856 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              CADUV000             *'           
              DISPLAY '*         FILE STATUS =  ' WRK-FS-CADUV000       
                                                 '         *'           
              DISPLAY '************** RDAB0856 *************'           
              PERFORM 9999-ROTINA-ERRO                                  
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       1110-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1120-TESTAR-FS-CADUB0M1       SECTION.                           
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-FS-CADUB0M1         NOT EQUAL  '00'                   
              DISPLAY '************** RDAB0856 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              CADUB0M1             *'           
              DISPLAY '*         FILE STATUS =  ' WRK-FS-CADUB0M1       
                                                 '         *'           
              DISPLAY '************** RDAB0856 *************'           
              PERFORM 9999-ROTINA-ERRO                                  
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       1120-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1150-TESTAR-FS-ARQSAIDA        SECTION.                          
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-FS-ARQSAIDA           NOT EQUAL  '00'                 
              DISPLAY '************** RDAB0856 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              ARQSAIDA              *'          
              DISPLAY '*         FILE STATUS =  ' WRK-FS-ARQSAIDA       
                                                 '         *'           
              DISPLAY '************** RDAB0856 *************'           
              PERFORM 9999-ROTINA-ERRO                                  
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       1150-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       2000-LER-CADUV000            SECTION.                            
      *---------------------------------------------------------------* 
                                                                        
           READ    CADUV000                                             
                                                                        
           IF      WRK-FS-CADUV000     EQUAL  '10'                      
                   MOVE HIGH-VALUES    TO      WRK-CHV-CADUV000         
                   GO                  TO      2000-99-FIM              
           END-IF.                                                      
                                                                        
           MOVE    WRK-LEITURA         TO      WRK-OPERACAO             
           PERFORM 1110-TESTAR-FS-CADUV000                              
                                                                        
           MOVE CADUV000-CCPF-CNPJ OF CADUV000                          
                                       TO WRK-CHV-0A4-CGCNUM.           
                                                                        
           MOVE CADUV000-CFLIAL-CPF-CNPJ OF CADUV000                    
                                       TO  WRK-CHV-0A4-CGCFIL.          
                                                                        
           MOVE CADUV000-CCTRL-CPF-CNPJ OF CADUV000                     
                                       TO  WRK-CHV-0A4-CGCCTR.          
                                                                        
           MOVE CADUV000-CCLUB OF CADUV000                              
                                       TO  WRK-CHV-0A4-CCLUB.           
                                                                        
           ADD     1                      TO  ACU-LDS-CADUV000.         
                                                                        
      *---------------------------------------------------------------* 
       2000-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       3000-LER-CADUB0M1            SECTION.                            
      *---------------------------------------------------------------* 
                                                                        
           READ    CADUB0M1                                             
                                                                        
           IF      WRK-FS-CADUB0M1     EQUAL  '10'                      
                   MOVE HIGH-VALUES    TO      WRK-CHV-CADUB0M1         
                   GO                  TO      3000-99-FIM              
           END-IF.                                                      
                                                                        
           MOVE    WRK-LEITURA         TO      WRK-OPERACAO             
           PERFORM 1120-TESTAR-FS-CADUB0M1                              
                                                                        
           MOVE  PSDCW033-47-CPF-CNPJ-NRO OF CADUB0M1                   
                                       TO WRK-AUX-CGCNUM-S              
           MOVE  WRK-AUX-CGCNUM        TO WRK-CHV-0M1-CGCNUM.           
                                                                        
           MOVE PSDCW033-47-CPF-CNPJ-FIL OF CADUB0M1                    
                                       TO  WRK-AUX-CGCFIL-S             
           MOVE WRK-AUX-CGCFIL         TO  WRK-CHV-0M1-CGCFIL.          
                                                                        
           MOVE PSDCW033-47-CPF-CNPJ-CTR OF CADUB0M1                    
                                       TO  WRK-AUX-CGCCTR-S             
           MOVE WRK-AUX-CGCCTR         TO  WRK-CHV-0M1-CGCCTR.          
                                                                        
           MOVE PSDCW033-47-CCLUB OF CADUB0M1                           
                                       TO  WRK-AUX-NUM010-S             
           MOVE WRK-AUX-NUM010         TO  WRK-CHV-0M1-CCLUB.           
                                                                        
                                                                        
           ADD     1                   TO      ACU-LDS-CADUB0M1.        
                                                                        
      *---------------------------------------------------------------* 
       3000-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       4000-PROCESSAR               SECTION.                            
      *---------------------------------------------------------------* 
                                                                        
           IF      WRK-CHV-CADUV000   GREATER   WRK-CHV-CADUB0M1        
                   PERFORM 3000-LER-CADUB0M1                            
           ELSE                                                         
              IF      WRK-CHV-CADUV000    LESS    WRK-CHV-CADUB0M1      
                      MOVE REG-CADUV000 OF CADUV000                     
                                          TO REG-CADUV000 OF ARQSAIDA   
                      PERFORM 5000-GRAVAR-ARQSAIDA                      
                      PERFORM 2000-LER-CADUV000                         
              ELSE                                                      
                      PERFORM 4200-CHAVES-IGUAIS                        
                      PERFORM 2000-LER-CADUV000                         
                      PERFORM 3000-LER-CADUB0M1                         
              END-IF                                                    
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       4000-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       4200-CHAVES-IGUAIS           SECTION.                            
      *---------------------------------------------------------------* 
                                                                        
           MOVE REG-CADUV000 OF CADUV000 TO REG-CADUV000 OF ARQSAIDA.   
                                                                        
           IF PSDCW033-47-DULT-ATULZ-CAD NOT EQUAL SPACES               
             STRING  PSDCW033-47-DULT-ATULZ-CAD(1:2) '-'                
                     PSDCW033-47-DULT-ATULZ-CAD(4:2) '-/'               
                     PSDCW033-47-DULT-ATULZ-CAD(7:4) '-01.01.01.000001' 
             DELIMITED BY SIZE           INTO  CADUV000-HULT-ATULZ      
                                                   OF ARQSAIDA          
           ELSE                                                         
             MOVE '0001-01-01-01.01.01.000001' TO CADUV000-HULT-ATULZ   
                                                   OF ARQSAIDA          
           END-IF                                                       
                                                                        
                                                                        
           PERFORM 5000-GRAVAR-ARQSAIDA.                                
                                                                        
      *---------------------------------------------------------------* 
       4200-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       5000-GRAVAR-ARQSAIDA          SECTION.                           
      *---------------------------------------------------------------* 
                                                                        
           WRITE REG-CADUV000 OF ARQSAIDA                               
           MOVE   WRK-GRAVACAO         TO      WRK-OPERACAO             
           PERFORM  1150-TESTAR-FS-ARQSAIDA                             
                                                                        
           ADD 1                      TO   ACU-GRV-ARQSAIDA.            
                                                                        
      *---------------------------------------------------------------* 
       5000-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       7000-DISPLAY-TOTAIS        SECTION.                              
      *---------------------------------------------------------------* 
                                                                        
           MOVE     ACU-LDS-CADUV000   TO    WRK-EDIT01                 
           MOVE     ACU-LDS-CADUB0M1   TO    WRK-EDIT02                 
           MOVE     ACU-GRV-ARQSAIDA   TO    WRK-EDIT05                 
                                                                        
           DISPLAY '******************** RDAB0856 ********************' 
           DISPLAY '*                                                *' 
           DISPLAY '*  TOTAL REG. LIDOS    - CADUV000 : 'WRK-EDIT01'  *'
           DISPLAY '*  TOTAL REG. LIDOS    - CADUB0M1 : 'WRK-EDIT02'  *'
           DISPLAY '*  TOTAL REG. GRAVADOS - ARQSAIDA : 'WRK-EDIT05'  *'
           DISPLAY '*                                                *' 
           DISPLAY '******************** RDAB0856 ********************'.
                                                                        
      *---------------------------------------------------------------* 
       7000-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       9000-FINALIZAR             SECTION.                              
      *---------------------------------------------------------------* 
                                                                        
           CLOSE  CADUV000                                              
                  CADUB0M1                                              
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
                                                                        
           MOVE   'RDAB0856'           TO  ERR-PGM.                     
           MOVE   'APL'                TO ERR-TIPO-ACESSO.              
           CALL   'BRAD7100'        USING  WRK-BATCH                    
                                           ERRO-AREA.                   
                                                                        
           GOBACK.                                                      
                                                                        
      *----------------------------------------------------------------*
       9999-99-FIM. EXIT.                                               
      *----------------------------------------------------------------*
                                                                        

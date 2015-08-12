      *===============================================================* 
       IDENTIFICATION DIVISION.                                         
      *===============================================================* 
                                                                        
       PROGRAM-ID. RDAB0900.                                            
       AUTHOR. FRANCISCO.                                               
                                                                        
      *===============================================================* 
      *                 B R Q    I T   S E R V I C E S                * 
      *---------------------------------------------------------------* 
      *                                                               * 
      *   PROGRAMA    : RDAB0900                                      * 
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
      *   ARQPARVV | I | DADOS PRINCIPAIS        | I#RDAB01 |   150   * 
      *   CADUB0Q4 | I | ENDERECO                | PSDCW033 |   642   * 
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
                                                                        
           SELECT  CADUB0Q4  ASSIGN  TO  UT-S-CADUB0Q4                  
                   FILE      STATUS  IS  WRK-FS-CADUB0Q4.               
                                                                        
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
                                                                        
       COPY 'I#RDAB01'.                                                 
      *---------------------------------------------------------------* 
      *  INPUT....: ARQ. ENDERECO                                     * 
      *             ORG. SEQUENCIAL    -  LRECL = 642 BYTES           * 
      *---------------------------------------------------------------* 
                                                                        
       FD  CADUB0Q4                                                     
           LABEL       RECORD    STANDARD                               
           RECORDING   MODE      F                                      
           BLOCK       CONTAINS  0.                                     
                                                                        
       COPY 'PSDCW033'.                                                 
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
            'INICIO DA WORKING RDAB0900'.                               
                                                                        
      *-- ------------------------------------------------------------* 
      *          CAMPOS UTILIZADOS PARA CANCELAR PROCESSAMENTO        * 
      *---------------------------------------------------------------* 
                                                                        
       01  FILLER.                                                      
           03  WRK-FS-ARQPARVV         PIC  X(02) VALUE SPACES.         
           03  WRK-FS-CADUB0Q4         PIC  X(02) VALUE SPACES.         
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
           03  ACU-LDS-CADUB0Q4        PIC 9(09) COMP-3    VALUE  ZEROS.
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
                                                                        
       01  WRK-AUX-CGCCTR              PIC 9(02) VALUE  ZEROS.          
       01  WRK-AUX-CGCCTR-S            REDEFINES                        
           WRK-AUX-CGCCTR              PIC S9(02).                      
                                                                        
       01  WRK-AUX-NUM010              PIC 9(10) VALUE  ZEROS.          
       01  WRK-AUX-NUM010-S            REDEFINES                        
           WRK-AUX-NUM010              PIC S9(10).                      
                                                                        
       01  WRK-AUX-NUM003              PIC 9(03) VALUE  ZEROS.          
       01  WRK-AUX-NUM003-S            REDEFINES                        
           WRK-AUX-NUM003              PIC S9(03).                      
                                                                        
       01  WRK-AUX-NUM004              PIC 9(04) VALUE  ZEROS.          
       01  WRK-AUX-NUM004-S            REDEFINES                        
           WRK-AUX-NUM004              PIC S9(04).                      
                                                                        
      *--  CHAVE ARQPARVV.                                              
       01  WRK-CHV-ARQPARVV.                                            
           03  WRK-CHV-PAR-CCLUB       PIC 9(10) VALUE  ZEROS.          
                                                                        
      *--  CHAVE CADUB0Q4.                                              
       01  WRK-CHV-CADUB0Q4.                                            
           03  WRK-CHV-0Q4-CCLUB       PIC 9(10) VALUE  ZEROS.          
                                                                        
      *----------------------------------------------------------------*
       01  FILLER   PIC  X(32) VALUE '*       AREA DA BRAD7100       *'.
      *----------------------------------------------------------------*
                                                                        
       COPY 'I#BRAD7C'.                                                 
                                                                        
      *---------------------------------------------------------------* 
       01   FILLER                     PIC  X(32)    VALUE              
            'FIM DA WORKING RDAB0900'.                                  
                                                                        
      *===============================================================* 
       PROCEDURE DIVISION.                                              
      *===============================================================* 
                                                                        
      *---------------------------------------------------------------* 
       0000-INICIAR SECTION.                                            
      *---------------------------------------------------------------* 
                                                                        
           PERFORM 1000-INICIALIZAR.                                    
                                                                        
           PERFORM 2000-LER-ARQPARVV                                    
           IF      WRK-FS-ARQPARVV     EQUAL '10'                       
                   DISPLAY '**************** RDAB0900 ***************'  
                   DISPLAY '*                                       *'  
                   DISPLAY '* ARQUIVO DE ENTRADA - ARQPARVV - VAZIO *'  
                   DISPLAY '*        PROCESSAMENTO ENCERRADO        *'  
                   DISPLAY '*                                       *'  
                   DISPLAY '**************** RDAB0900 ***************'  
                   PERFORM  9000-FINALIZAR                              
           END-IF.                                                      
                                                                        
           PERFORM 3000-LER-CADUB0Q4                                    
           IF      WRK-FS-CADUB0Q4     EQUAL '10'                       
                   DISPLAY '**************** RDAB0900 ***************'  
                   DISPLAY '*                                       *'  
                   DISPLAY '* ARQUIVO DE ENTRADA - CADUB0Q4 - VAZIO *'  
                   DISPLAY '*        PROCESSAMENTO ENCERRADO        *'  
                   DISPLAY '*                                       *'  
                   DISPLAY '**************** RDAB0900 ***************'  
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
                        CADUB0Q4                                        
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
           PERFORM  1120-TESTAR-FS-CADUB0Q4                             
           PERFORM  1150-TESTAR-FS-ARQSAIDA.                            
                                                                        
      *---------------------------------------------------------------* 
       1100-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1110-TESTAR-FS-ARQPARVV       SECTION.                           
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-FS-ARQPARVV         NOT EQUAL  '00'                   
              DISPLAY '************** RDAB0900 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              ARQPARVV             *'           
              DISPLAY '*         FILE STATUS =  ' WRK-FS-ARQPARVV       
                                                 '         *'           
              DISPLAY '************** RDAB0900 *************'           
              PERFORM 9999-ROTINA-ERRO                                  
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       1110-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1120-TESTAR-FS-CADUB0Q4       SECTION.                           
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-FS-CADUB0Q4         NOT EQUAL  '00'                   
              DISPLAY '************** RDAB0900 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              CADUB0Q4             *'           
              DISPLAY '*         FILE STATUS =  ' WRK-FS-CADUB0Q4       
                                                 '         *'           
              DISPLAY '************** RDAB0900 *************'           
              PERFORM 9999-ROTINA-ERRO                                  
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       1120-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1150-TESTAR-FS-ARQSAIDA        SECTION.                          
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-FS-ARQSAIDA           NOT EQUAL  '00'                 
              DISPLAY '************** RDAB0900 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              ARQSAIDA              *'          
              DISPLAY '*         FILE STATUS =  ' WRK-FS-ARQSAIDA       
                                                 '         *'           
              DISPLAY '************** RDAB0900 *************'           
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
                                                                        
                                                                        
           MOVE PVV-CCLUB              TO  WRK-CHV-PAR-CCLUB.           
                                                                        
           ADD     1                      TO  ACU-LDS-ARQPARVV.         
                                                                        
      *---------------------------------------------------------------* 
       2000-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       3000-LER-CADUB0Q4            SECTION.                            
      *---------------------------------------------------------------* 
                                                                        
           READ    CADUB0Q4                                             
                                                                        
           IF      WRK-FS-CADUB0Q4     EQUAL  '10'                      
                   MOVE HIGH-VALUES    TO      WRK-CHV-CADUB0Q4         
                   GO                  TO      3000-99-FIM              
           END-IF.                                                      
                                                                        
           MOVE    WRK-LEITURA         TO      WRK-OPERACAO             
           PERFORM 1120-TESTAR-FS-CADUB0Q4                              
                                                                        
                                                                        
           MOVE PSDCW033-44-CCLUB OF CADUB0Q4                           
                                       TO  WRK-AUX-NUM010-S             
           MOVE WRK-AUX-NUM010         TO  WRK-CHV-0Q4-CCLUB.           
                                                                        
           ADD     1                   TO      ACU-LDS-CADUB0Q4.        
                                                                        
      *---------------------------------------------------------------* 
       3000-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       4000-PROCESSAR               SECTION.                            
      *---------------------------------------------------------------* 
                                                                        
           IF      WRK-CHV-ARQPARVV   GREATER   WRK-CHV-CADUB0Q4        
                   PERFORM 3000-LER-CADUB0Q4                            
           ELSE                                                         
              IF      WRK-CHV-ARQPARVV    LESS    WRK-CHV-CADUB0Q4      
                      PERFORM 2000-LER-ARQPARVV                         
              ELSE                                                      
                      PERFORM 4200-CHAVES-IGUAIS                        
                      PERFORM 3000-LER-CADUB0Q4                         
              END-IF                                                    
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       4000-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       4200-CHAVES-IGUAIS           SECTION.                            
      *---------------------------------------------------------------* 
                                                                        
           INITIALIZE SOC-REGISTRO.                                     
                                                                        
           PERFORM 4300-MOVER-ARQPARVV.                                 
                                                                        
           PERFORM 5000-GRAVAR-ARQSAIDA.                                
                                                                        
      *---------------------------------------------------------------* 
       4200-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       4300-MOVER-ARQPARVV           SECTION.                           
      *---------------------------------------------------------------* 
                                                                        
           MOVE PSDCW033-44-CCLUB OF CADUB0Q4  TO WRK-AUX-NUM010-S      
           MOVE WRK-AUX-NUM010                 TO SOC-CCLUB             
                                                                        
           MOVE PVV-CGCNUM                     TO SOC-CGCNUM-EMPRESA.   
           MOVE PVV-CGCFIL                     TO SOC-CGCFIL-EMPRESA.   
                                                                        
           MOVE  PSDCW033-44-CCPF-CNPJ OF CADUB0Q4                      
                                       TO WRK-AUX-CGCNUM-S              
           MOVE  WRK-AUX-CGCNUM        TO SOC-CGCNUM.                   
                                                                        
           MOVE PSDCW033-44-CFLIAL-CPF-CNPJ OF CADUB0Q4                 
                                           TO WRK-AUX-NUM004-S          
           MOVE WRK-AUX-NUM004             TO SOC-CGCFIL                
                                                                        
           MOVE PSDCW033-44-CCTRL-CPF-CNPJ OF CADUB0Q4                  
                                       TO  WRK-AUX-CGCCTR-S             
           MOVE WRK-AUX-CGCCTR         TO  SOC-CGCCTR.                  
                                                                        
           MOVE PSDCW033-44-IPARTC-EMPR(1:40)  TO SOC-NOME.
           MOVE ZEROS                          TO SOC-RAMO-ATVDD.       
                                                                        
      *---------------------------------------------------------------* 
       4300-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       5000-GRAVAR-ARQSAIDA          SECTION.                           
      *---------------------------------------------------------------* 
                                                                        
           WRITE  SOC-REGISTRO                                          
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
           MOVE     ACU-LDS-CADUB0Q4   TO    WRK-EDIT02                 
           MOVE     ACU-GRV-ARQSAIDA   TO    WRK-EDIT05                 
                                                                        
           DISPLAY '******************** RDAB0900 ********************' 
           DISPLAY '*                                                *' 
           DISPLAY '*  TOTAL REG. LIDOS    - ARQPARVV : 'WRK-EDIT01'  *'
           DISPLAY '*  TOTAL REG. LIDOS    - CADUB0Q4 : 'WRK-EDIT02'  *'
           DISPLAY '*  TOTAL REG. GRAVADOS - ARQSAIDA : 'WRK-EDIT05'  *'
           DISPLAY '*                                                *' 
           DISPLAY '******************** RDAB0900 ********************'.
                                                                        
      *---------------------------------------------------------------* 
       7000-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       9000-FINALIZAR             SECTION.                              
      *---------------------------------------------------------------* 
                                                                        
           CLOSE  ARQPARVV                                              
                  CADUB0Q4                                              
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
                                                                        
           MOVE   'RDAB0900'           TO  ERR-PGM.                     
           MOVE   'APL'                TO ERR-TIPO-ACESSO.              
           CALL   'BRAD7100'        USING  WRK-BATCH                    
                                           ERRO-AREA.                   
                                                                        
           GOBACK.                                                      
                                                                        
      *----------------------------------------------------------------*
       9999-99-FIM. EXIT.                                               
      *----------------------------------------------------------------*
                                                                        

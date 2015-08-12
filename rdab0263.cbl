      *===============================================================* 
       IDENTIFICATION DIVISION.                                         
      *===============================================================* 
                                                                        
       PROGRAM-ID. RDAB0263.                                            
       AUTHOR. FRANCISCO.                                               
                                                                        
      *===============================================================* 
      *                 B R Q    I T   S E R V I C E S                * 
      *---------------------------------------------------------------* 
      *                                                               * 
      *   PROGRAMA    : RDAB0263                                      * 
      *   PROGRAMADOR : FRANCISCO FREIRE       - BRQ IT SERVICES      * 
      *   ANALISTA    : FRANCISCO FREIRE       - BRQ IT SERVICES      * 
      *   SUPERVISOR  : JUNIOR RIBAMAR         - GP. 70               * 
      *   DATA        : ABRIL/2014                                    * 
      *                                                               * 
      *   OBJETIVO    : BALANCE LINE DE DADOS PRINCIPAIS DO CADU COM  * 
      *                 DADOS PRINCIPAIS DO RDAB PF (RDBAB008).       * 
      *   ARQUIVOS:                                                   * 
      *   ---------------------------------------------------------   * 
      *   DDNAME   |I/O| DESCRICAO               | BOOK     | LRECL   * 
      *   ---------+---+-------------------------+----------+------   * 
      *   NOMECADU | I | DADOS CADUV000          | I#RDABAP |   320   * 
      *   NOMERDAB | I | DADOS MOV LOGDR RDAB    | I#RDAB14 |   146   * 
      *   CADURDAB | O | DADOS MOV LOGDR RDAB AT | I#RDAB14 |   146   * 
      *   DESPRDAB | O | DESPR.MOV LOGDR         | I#RDAB14 |   146   * 
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
                                                                        
           SELECT  NOMERDAB  ASSIGN  TO  UT-S-NOMERDAB                  
                   FILE      STATUS  IS  WRK-FS-NOMERDAB.               
                                                                        
           SELECT  CADURDAB  ASSIGN  TO  UT-S-CADURDAB                  
                   FILE      STATUS  IS  WRK-FS-CADURDAB.               
                                                                        
           SELECT  DESPRDAB  ASSIGN  TO  UT-S-DESPRDAB                  
                   FILE      STATUS  IS  WRK-FS-DESPRDAB.               
                                                                        
      *===============================================================* 
       DATA DIVISION.                                                   
      *===============================================================* 
                                                                        
      *---------------------------------------------------------------* 
       FILE SECTION.                                                    
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
      *  INPUT....: ARQ. NOME PF CADU                                 * 
      *             ORG. SEQUENCIAL    -  LRECL = 320 BYTES           * 
      *---------------------------------------------------------------* 
                                                                        
       FD  CADUV000                                                     
           LABEL       RECORD    STANDARD                               
           RECORDING   MODE      F                                      
           BLOCK       CONTAINS  0.                                     
                                                                        
           COPY 'I#RDABAP'.                                             
                                                                        
      *---------------------------------------------------------------* 
      *  INPUT....: ARQ. NOME PF RDABB008                             * 
      *             ORG. SEQUENCIAL    -  LRECL = 050 BYTES           * 
      *---------------------------------------------------------------* 
                                                                        
       FD  NOMERDAB                                                     
           LABEL       RECORD    STANDARD                               
           RECORDING   MODE      F                                      
           BLOCK       CONTAINS  0.                                     
                                                                        
           COPY 'I#RDAB14'.                                             
      *---------------------------------------------------------------* 
      *  OUTPUT...: ARQ. DE DADOS DA PESSOA                           * 
      *             ORG. SEQUENCIAL   -   LRECL = 146 BYTES           * 
      *---------------------------------------------------------------* 
                                                                        
       FD  CADURDAB                                                     
           LABEL       RECORD    STANDARD                               
           RECORDING   MODE      F                                      
           BLOCK       CONTAINS  0.                                     
                                                                        
           COPY 'I#RDAB14'.                                             
                                                                        
                                                                        
      *---------------------------------------------------------------* 
      *  OUTPUT...: ARQ. DE DADOS DO RDAB (DESPREZADOS)               * 
      *             ORG. SEQUENCIAL   -   LRECL = 146 BYTES           * 
      *---------------------------------------------------------------* 
                                                                        
       FD  DESPRDAB                                                     
           LABEL       RECORD    STANDARD                               
           RECORDING   MODE      F                                      
           BLOCK       CONTAINS  0.                                     
                                                                        
           COPY 'I#RDAB14'.                                             
                                                                        
      *---------------------------------------------------------------* 
       WORKING-STORAGE SECTION.                                         
      *---------------------------------------------------------------* 
       01   FILLER                     PIC  X(32) VALUE                 
            'INICIO DA WORKING RDAB0263'.                               
                                                                        
      *-- ------------------------------------------------------------* 
      *          CAMPOS UTILIZADOS PARA CANCELAR PROCESSAMENTO        * 
      *---------------------------------------------------------------* 
                                                                        
       01  FILLER.                                                      
           03  WRK-FS-CADUV000         PIC  X(02) VALUE SPACES.         
           03  WRK-FS-NOMERDAB         PIC  X(02) VALUE SPACES.         
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
           03  ACU-LDS-CADUV000        PIC 9(09) COMP-3    VALUE  ZEROS.
           03  ACU-LDS-NOMERDAB        PIC 9(09) COMP-3    VALUE  ZEROS.
           03  ACU-GRV-CADURDAB        PIC 9(09) COMP-3    VALUE  ZEROS.
           03  ACU-GRV-DESPRDAB        PIC 9(09) COMP-3    VALUE  ZEROS.
           03  ACU-BRANCOS-CADU        PIC 9(09) COMP-3    VALUE  ZEROS.
           03  ACU-NOMEIGUAL-CADU      PIC 9(09) COMP-3    VALUE  ZEROS.
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
                                                                        
      *--  CHAVE CADUV000.                                              
       01  WRK-CHV-CADUV000.                                            
           03  WRK-CHV-CADU-AGEN     PIC  9(005) VALUE ZEROS.           
           03  WRK-CHV-CADU-CONTA    PIC  9(013) VALUE ZEROS.           
           03  WRK-CHV-CADU-IND      PIC  X(001) VALUE SPACES.          
                                                                        
      *--  CHAVE NOMERDAB.                                              
       01  WRK-CHV-NOMERDAB.                                            
           03  WRK-CHV-RDAB-AGEN     PIC  9(005) VALUE ZEROS.           
           03  WRK-CHV-RDAB-CONTA    PIC  9(013) VALUE ZEROS.           
           03  WRK-CHV-RDAB-IND      PIC  X(001) VALUE SPACES.          
                                                                        
      *--  AUX. CHAVE ENDRRDAB.                                         
       01  WRK-TEMP-CH-ENDRRDAB.                                        
           03  WRK-TEMP-RDAB-AGE         PIC S9(005) VALUE ZEROS.       
           03  WRK-AUX-RDAB-AGE          REDEFINES WRK-TEMP-RDAB-AGE    
                                         PIC  9(005).                   
           03  WRK-TEMP-RDAB-CCTA        PIC S9(013) VALUE ZEROS.       
           03  WRK-AUX-RDAB-CCTA         REDEFINES WRK-TEMP-RDAB-CCTA   
                                         PIC  9(013).                   
                                                                        
      *--  TEMP ENDERECO RDAB                                           
       01  WRK-TEMP-ENDRRDAB.                                           
           03  WRK-TEMP-CCEP-CLI-RENEG   PIC S9(005) VALUE ZEROS.       
           03  WRK-AUX-CCEP-CLI-RENEG    REDEFINES                      
               WRK-TEMP-CCEP-CLI-RENEG   PIC  9(005).                   
           03  WRK-TEMP-CCEP-COMPL-RENEG PIC S9(003) VALUE ZEROS.       
           03  WRK-AUX-CCEP-COMPL-RENEG  REDEFINES                      
               WRK-TEMP-CCEP-COMPL-RENEG PIC  9(003).                   
                                                                        
      *--  TEMP ENDERECO CADU                                           
       01  WRK-TEMP-ENDRCADU.                                           
           05  WRK-CADU-ELOGDR-PSSOA     PIC  X(040) VALUE SPACES.      
           05  WRK-CADU-RCOMPL-ENDER     PIC  X(020) VALUE SPACES.      
           05  WRK-CADU-ICIDDE-ENDER     PIC  X(025) VALUE SPACES.      
           03  WRK-TEMP-CCEP             PIC S9(005) VALUE ZEROS.       
           03  WRK-AUX-CCEP              REDEFINES   WRK-TEMP-CCEP      
                                         PIC  9(005).                   
           03  WRK-TEMP-CCEP-COMPL       PIC S9(003) VALUE ZEROS.       
           03  WRK-AUX-CCEP-COMPL        REDEFINES   WRK-TEMP-CCEP-COMPL
                                         PIC  9(003).                   
                                                                        
      *----------------------------------------------------------------*
       01  FILLER   PIC  X(32) VALUE '*       AREA DA BRAD7100       *'.
      *----------------------------------------------------------------*
                                                                        
       COPY 'I#BRAD7C'.                                                 
                                                                        
      *---------------------------------------------------------------* 
       01   FILLER                     PIC  X(32)    VALUE              
            'FIM DA WORKING RDAB0263'.                                  
                                                                        
      *===============================================================* 
       PROCEDURE DIVISION.                                              
      *===============================================================* 
                                                                        
      *---------------------------------------------------------------* 
       0000-INICIAR SECTION.                                            
      *---------------------------------------------------------------* 
                                                                        
           PERFORM 1000-INICIALIZAR.                                    
                                                                        
           PERFORM 2000-LER-CADUV000                                    
           IF      WRK-FS-CADUV000     EQUAL '10'                       
                   DISPLAY '**************** RDAB0263 ***************'  
                   DISPLAY '*                                       *'  
                   DISPLAY '* ARQUIVO DE ENTRADA - CADUV000 - VAZIO *'  
                   DISPLAY '*        PROCESSAMENTO ENCERRADO        *'  
                   DISPLAY '*                                       *'  
                   DISPLAY '**************** RDAB0263 ***************'  
                   PERFORM  9000-FINALIZAR                              
           END-IF.                                                      
                                                                        
           PERFORM 3000-LER-NOMERDAB                                    
           IF      WRK-FS-NOMERDAB     EQUAL '10'                       
                   DISPLAY '**************** RDAB0263 ***************'  
                   DISPLAY '*                                       *'  
                   DISPLAY '* ARQUIVO DE ENTRADA - NOMERDAB - VAZIO *'  
                   DISPLAY '*        PROCESSAMENTO ENCERRADO        *'  
                   DISPLAY '*                                       *'  
                   DISPLAY '**************** RDAB0263 ***************'  
           END-IF                                                       
                                                                        
           PERFORM 4000-PROCESSAR     UNTIL                             
                  (WRK-FS-NOMERDAB     EQUAL '10')                      
                                                                        
                                                                        
           PERFORM 7000-DISPLAY-TOTAIS                                  
                                                                        
           PERFORM 9000-FINALIZAR.                                      
                                                                        
      *---------------------------------------------------------------* 
       0000-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1000-INICIALIZAR              SECTION.                           
      *---------------------------------------------------------------* 
                                                                        
           OPEN  INPUT  CADUV000                                        
                        NOMERDAB                                        
                 OUTPUT CADURDAB                                        
                        DESPRDAB                                        
                                                                        
           MOVE  WRK-ABERTURA          TO    WRK-OPERACAO               
           PERFORM  1100-TESTAR-FILE-STATUS.                            
                                                                        
      *---------------------------------------------------------------* 
       1000-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1100-TESTAR-FILE-STATUS       SECTION.                           
      *---------------------------------------------------------------* 
                                                                        
           PERFORM  1110-TESTAR-FS-CADUV000                             
           PERFORM  1120-TESTAR-FS-NOMERDAB                             
           PERFORM  1150-TESTAR-FS-CADURDAB.                            
           PERFORM  1160-TESTAR-FS-DESPRDAB.                            
                                                                        
      *---------------------------------------------------------------* 
       1100-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1110-TESTAR-FS-CADUV000       SECTION.                           
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-FS-CADUV000         NOT EQUAL  '00'                   
              DISPLAY '************** RDAB0263 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              CADUV000             *'           
              DISPLAY '*         FILE STATUS =  ' WRK-FS-CADUV000       
                                                 '         *'           
              DISPLAY '************** RDAB0263 *************'           
              PERFORM 9999-ROTINA-ERRO                                  
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       1110-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1120-TESTAR-FS-NOMERDAB       SECTION.                           
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-FS-NOMERDAB         NOT EQUAL  '00'                   
              DISPLAY '************** RDAB0263 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              NOMERDAB             *'           
              DISPLAY '*         FILE STATUS =  ' WRK-FS-NOMERDAB       
                                                 '         *'           
              DISPLAY '************** RDAB0263 *************'           
              PERFORM 9999-ROTINA-ERRO                                  
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       1120-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1150-TESTAR-FS-CADURDAB        SECTION.                          
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-FS-CADURDAB           NOT EQUAL  '00'                 
              DISPLAY '************** RDAB0263 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              CADURDAB              *'          
              DISPLAY '*         FILE STATUS =  ' WRK-FS-CADURDAB       
                                                 '         *'           
              DISPLAY '************** RDAB0263 *************'           
              PERFORM 9999-ROTINA-ERRO                                  
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       1150-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1160-TESTAR-FS-DESPRDAB        SECTION.                          
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-FS-DESPRDAB           NOT EQUAL  '00'                 
              DISPLAY '************** RDAB0263 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              DESPRDAB              *'          
              DISPLAY '*         FILE STATUS =  ' WRK-FS-DESPRDAB       
                                                 '         *'           
              DISPLAY '************** RDAB0263 *************'           
              PERFORM 9999-ROTINA-ERRO                                  
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       1160-99-FIM.   EXIT.                                             
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
                                                                        
           MOVE  CADUV000-CAG-COBR-TARIF  TO WRK-CHV-CADU-AGEN          
           MOVE  CADUV000-CCTA-COBR-TARIF TO WRK-CHV-CADU-CONTA         
           MOVE  '1'                      TO WRK-CHV-CADU-IND           
                                                                        
                                                                        
           MOVE  CADUV000-ELOGDR-PSSOA OF  CADUV000(1:40)               
                                       TO  WRK-CADU-ELOGDR-PSSOA        
           MOVE  CADUV000-RCOMPL-ENDER OF  CADUV000(1:20)               
                                       TO  WRK-CADU-RCOMPL-ENDER        
           MOVE  CADUV000-ICIDDE-ENDER OF  CADUV000(1:25)               
                                       TO  WRK-CADU-ICIDDE-ENDER        
                                                                        
           MOVE CADUV000-CCEP          TO  WRK-AUX-CCEP                 
           MOVE CADUV000-CCEP-COMPL    TO  WRK-AUX-CCEP-COMPL           
                                                                        
           ADD     1                   TO  ACU-LDS-CADUV000.            
                                                                        
      *---------------------------------------------------------------* 
       2000-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       3000-LER-NOMERDAB            SECTION.                            
      *---------------------------------------------------------------* 
                                                                        
           READ    NOMERDAB                                             
                                                                        
           IF      WRK-FS-NOMERDAB     EQUAL  '10'                      
                   MOVE HIGH-VALUES    TO      WRK-CHV-NOMERDAB         
                   GO                  TO      3000-99-FIM              
           END-IF.                                                      
                                                                        
           MOVE    WRK-LEITURA         TO      WRK-OPERACAO             
           PERFORM 1120-TESTAR-FS-NOMERDAB                              
                                                                        
           MOVE ALG-CAG-BCRIA          OF  NOMERDAB                     
                                       TO  WRK-TEMP-RDAB-AGE            
           MOVE WRK-AUX-RDAB-AGE       TO  WRK-CHV-RDAB-AGEN            
           MOVE ALG-CCTA-CORR          OF  NOMERDAB                     
                                       TO  WRK-TEMP-RDAB-CCTA           
           MOVE WRK-AUX-RDAB-CCTA      TO  WRK-CHV-RDAB-CONTA           
           MOVE ALG-CINDCD-ORIGE-LOGDR OF  NOMERDAB                     
                                       TO  WRK-CHV-RDAB-IND             
                                                                        
                                                                        
           MOVE ALG-CCEP-CLI-RENEG     OF  NOMERDAB                     
                                       TO  WRK-TEMP-CCEP-CLI-RENEG      
           MOVE ALG-CCEP-COMPL-RENEG   OF  NOMERDAB                     
                                       TO  WRK-TEMP-CCEP-COMPL-RENEG    
                                                                        
           ADD     1                   TO      ACU-LDS-NOMERDAB.        
                                                                        
      *---------------------------------------------------------------* 
       3000-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       4000-PROCESSAR               SECTION.                            
      *---------------------------------------------------------------* 
                                                                        
           IF    WRK-CHV-CADUV000   GREATER   WRK-CHV-NOMERDAB          
                 INITIALIZE ALG-REG OF DESPRDAB                         
                                                                        
                 ADD   1            TO  ACU-NAOACHOU-CADU               
                 PERFORM 4310-MOVER-DESPRDAB                            
                 PERFORM 5100-GRAVAR-DESPRDAB                           
                 PERFORM 3000-LER-NOMERDAB                              
           ELSE                                                         
              IF   WRK-CHV-CADUV000   LESS    WRK-CHV-NOMERDAB          
                   PERFORM 2000-LER-CADUV000                            
              ELSE                                                      
                   INITIALIZE ALG-REG OF DESPRDAB                       
                                                                        
                   MOVE ALG-REG OF NOMERDAB TO ALG-REG OF DESPRDAB      
                                                                        
                   IF (WRK-CADU-ELOGDR-PSSOA                            
                                         EQUAL SPACES OR LOW-VALUES)    
                      ADD  1             TO  ACU-BRANCOS-CADU           
                                                                        
                      PERFORM 4310-MOVER-DESPRDAB                       
                      PERFORM 5100-GRAVAR-DESPRDAB                      
                   ELSE                                                 
                      IF (WRK-CADU-ELOGDR-PSSOA EQUAL                   
                          ALG-ELOGDR-CLI-RENEG  OF NOMERDAB) AND        
                                                                        
                         (CADUV000-ELOGDR-NRO   OF CADUV000 EQUAL       
                          ALG-ENRO-LOGDR-RENEG  OF NOMERDAB) AND        
                                                                        
                         (WRK-CADU-RCOMPL-ENDER EQUAL                   
                          ALG-ECOMPL-LOGDR-RENEG OF NOMERDAB) AND       
                                                                        
                         (WRK-CADU-ICIDDE-ENDER EQUAL                   
                          ALG-IMUN-IBGE-RENEG   OF NOMERDAB) AND        
                                                                        
                         (CADUV000-CSGL-UF      OF CADUV000 EQUAL       
                          ALG-CSGL-UF-CLI-RENEG OF NOMERDAB) AND        
                                                                        
                         (CADUV000-CCEP         OF CADUV000 EQUAL       
                          WRK-AUX-CCEP-CLI-RENEG)     AND               
                                                                        
                         (CADUV000-CCEP-COMPL   OF CADUV000 EQUAL       
                          WRK-AUX-CCEP-COMPL-RENEG)                     
                                                                        
                          ADD  1             TO  ACU-NOMEIGUAL-CADU     
                          PERFORM 4310-MOVER-DESPRDAB                   
                          PERFORM 5100-GRAVAR-DESPRDAB                  
                      ELSE                                              
                          PERFORM 4200-CHAVES-IGUAIS                    
                      END-IF                                            
                   END-IF                                               
                   PERFORM 3000-LER-NOMERDAB                            
                   PERFORM 2000-LER-CADUV000                            
              END-IF                                                    
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       4000-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       4200-CHAVES-IGUAIS           SECTION.                            
      *---------------------------------------------------------------* 
                                                                        
           INITIALIZE ALG-REG OF CADURDAB.                              
                                                                        
           MOVE  ALG-REG OF NOMERDAB TO ALG-REG OF CADURDAB             
                                                                        
           PERFORM 4300-MOVER-NOMERDAB.                                 
                                                                        
           PERFORM 5000-GRAVAR-CADURDAB.                                
                                                                        
      *---------------------------------------------------------------* 
       4200-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       4300-MOVER-NOMERDAB           SECTION.                           
      *---------------------------------------------------------------* 
                                                                        
           MOVE WRK-CADU-ELOGDR-PSSOA  TO                               
                                   ALG-ELOGDR-CLI-RENEG    OF CADURDAB  
           MOVE CADUV000-ELOGDR-NRO    TO                               
                                   ALG-ENRO-LOGDR-RENEG    OF CADURDAB  
           MOVE WRK-CADU-RCOMPL-ENDER  TO                               
                                   ALG-ECOMPL-LOGDR-RENEG  OF CADURDAB  
           MOVE WRK-CADU-ICIDDE-ENDER  TO                               
                                   ALG-IMUN-IBGE-RENEG     OF CADURDAB  
           MOVE CADUV000-CSGL-UF       TO                               
                                   ALG-CSGL-UF-CLI-RENEG   OF CADURDAB  
           MOVE WRK-TEMP-CCEP          TO                               
                                   ALG-CCEP-CLI-RENEG      OF CADURDAB  
           MOVE WRK-TEMP-CCEP-COMPL    TO                               
                                   ALG-CCEP-COMPL-RENEG    OF CADURDAB. 
                                                                        
      *---------------------------------------------------------------* 
       4300-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       4310-MOVER-DESPRDAB           SECTION.                           
      *---------------------------------------------------------------* 
                                                                        
           MOVE  ALG-REG OF NOMERDAB TO ALG-REG OF DESPRDAB.            
                                                                        
      *---------------------------------------------------------------* 
       4310-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       5000-GRAVAR-CADURDAB          SECTION.                           
      *---------------------------------------------------------------* 
                                                                        
           WRITE  ALG-REG OF CADURDAB                                   
           MOVE   WRK-GRAVACAO         TO      WRK-OPERACAO             
           PERFORM  1150-TESTAR-FS-CADURDAB                             
                                                                        
           ADD 1                      TO   ACU-GRV-CADURDAB.            
                                                                        
      *---------------------------------------------------------------* 
       5000-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       5100-GRAVAR-DESPRDAB          SECTION.                           
      *---------------------------------------------------------------* 
                                                                        
           WRITE  ALG-REG OF DESPRDAB                                   
           MOVE   WRK-GRAVACAO         TO      WRK-OPERACAO             
           PERFORM  1160-TESTAR-FS-DESPRDAB                             
                                                                        
           ADD 1                      TO   ACU-GRV-DESPRDAB.            
                                                                        
      *---------------------------------------------------------------* 
       5100-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       7000-DISPLAY-TOTAIS        SECTION.                              
      *---------------------------------------------------------------* 
                                                                        
           MOVE     ACU-LDS-CADUV000   TO    WRK-EDIT01                 
           MOVE     ACU-LDS-NOMERDAB   TO    WRK-EDIT02                 
           MOVE     ACU-BRANCOS-CADU   TO    WRK-EDIT03                 
           MOVE     ACU-NOMEIGUAL-CADU TO    WRK-EDIT04                 
           MOVE     ACU-GRV-CADURDAB   TO    WRK-EDIT05                 
           MOVE     ACU-GRV-DESPRDAB   TO    WRK-EDIT06                 
           MOVE     ACU-NAOACHOU-CADU  TO    WRK-EDIT07                 
                                                                        
           DISPLAY '******************** RDAB0263 ********************' 
           DISPLAY '*                                                *' 
           DISPLAY '*  TOTAL REG. LIDOS    - CADUV000 : ' WRK-EDIT01 '*'
           DISPLAY '*  TOTAL REG. LIDOS    - NOMERDAB : ' WRK-EDIT02 '*'
           DISPLAY '*  TOTAL REG. DESPREZ. NOME BRANCO: ' WRK-EDIT03 '*'
           DISPLAY '*  TOTAL REG. DESPREZ. NOME IGUAL : ' WRK-EDIT04 '*'
           DISPLAY '*  TOTAL REG. DESPR.NAO ACHOU CADU: ' WRK-EDIT07 '*'
           DISPLAY '*  TOTAL REG. GRAVADOS - CADURDAB : ' WRK-EDIT05 '*'
           DISPLAY '*  TOTAL REG. GRAVADOS - DESPRDAB : ' WRK-EDIT06 '*'
           DISPLAY '*                                                *' 
           DISPLAY '******************** RDAB0263 ********************'.
                                                                        
      *---------------------------------------------------------------* 
       7000-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       9000-FINALIZAR             SECTION.                              
      *---------------------------------------------------------------* 
                                                                        
           CLOSE  CADUV000                                              
                  NOMERDAB                                              
                  CADURDAB                                              
                  DESPRDAB.                                             
                                                                        
           MOVE   WRK-FECHAMENTO       TO      WRK-OPERACAO             
           PERFORM  1100-TESTAR-FILE-STATUS.                            
                                                                        
           STOP RUN.                                                    
                                                                        
      *---------------------------------------------------------------* 
       9000-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *----------------------------------------------------------------*
       9999-ROTINA-ERRO SECTION.                                        
      *----------------------------------------------------------------*
                                                                        
           MOVE   'RDAB0263'           TO  ERR-PGM.                     
           MOVE   'APL'                TO ERR-TIPO-ACESSO.              
           CALL   'BRAD7100'        USING  WRK-BATCH                    
                                           ERRO-AREA.                   
                                                                        
           GOBACK.                                                      
                                                                        
      *----------------------------------------------------------------*
       9999-99-FIM. EXIT.                                               
      *----------------------------------------------------------------*
                                                                        

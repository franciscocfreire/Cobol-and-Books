      *===============================================================* 
       IDENTIFICATION DIVISION.                                         
      *===============================================================* 
                                                                        
       PROGRAM-ID. RDAB0262.                                            
       AUTHOR. FRANCISCO.                                               
                                                                        
      *===============================================================* 
      *                 B R Q    I T   S E R V I C E S                * 
      *---------------------------------------------------------------* 
      *                                                               * 
      *   PROGRAMA    : RDAB0262                                      * 
      *   PROGRAMADOR : FRANCISCO FREIRE       - BRQ IT SERVICES      * 
      *   ANALISTA    : FRANCISCO FREIRE       - BRQ IT SERVICES      * 
      *   SUPERVISOR  : JUNIOR RIBAMAR         - GP. 70               * 
      *   DATA        : ABRIL/2014                                    * 
      *                                                               * 
      *   OBJETIVO    : BALANCE LINE MOVIMENTO DE CPFS X CADUV000     * 
      *                 PARA SELECIONAR REGISTROS ATUALIZADOS         * 
      *   ARQUIVOS:                                                   * 
      *   ---------------------------------------------------------   * 
      *   DDNAME   |I/O| DESCRICAO               | BOOK     | LRECL   * 
      *   ---------+---+-------------------------+----------+------   * 
      *   BKPRDAB  | I | DADOS PF RDAB           | I#RDAB01 |   150   * 
      *   HPURDABE | I | DADOS CADUV000          | I#RDABAP |   320   * 
      *   HPURDABS | O | DADOS CADUV000 ATUAL    | I#RDABAP |   320   * 
      *   ---------------------------------------------------------   * 
      *===============================================================* 
                                                                        
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
                                                                        
           SELECT  BKPRDAB   ASSIGN  TO  UT-S-BKPRDAB                   
                   FILE      STATUS  IS  WRK-FS-BKPRDAB.                
                                                                        
           SELECT  HPURDABE  ASSIGN  TO  UT-S-HPURDABE                  
                   FILE      STATUS  IS  WRK-FS-HPURDABE.               
                                                                        
           SELECT  HPURDABS  ASSIGN  TO  UT-S-HPURDABS                  
                   FILE      STATUS  IS  WRK-FS-HPURDABS.               
                                                                        
      *===============================================================* 
       DATA DIVISION.                                                   
      *===============================================================* 
                                                                        
      *---------------------------------------------------------------* 
       FILE SECTION.                                                    
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
      *  INPUT....: ARQ. NOME PF RDAB (BACKUP DO RDAB COM CPFS ATUAL.)* 
      *             ORG. SEQUENCIAL    -  LRECL = 150 BYTES           * 
      *---------------------------------------------------------------* 
                                                                        
       FD  BKPRDAB                                                      
           LABEL       RECORD    STANDARD                               
           RECORDING   MODE      F                                      
           BLOCK       CONTAINS  0.                                     
                                                                        
           COPY 'I#RDAB01'.                                             
                                                                        
      *---------------------------------------------------------------* 
      *  INPUT....: ARQ. NOME PF RDABB006 (HPU COMPLETO)              * 
      *             ORG. SEQUENCIAL    -  LRECL = 146 BYTES           * 
      *---------------------------------------------------------------* 
                                                                        
       FD  HPURDABE                                                     
           LABEL       RECORD    STANDARD                               
           RECORDING   MODE      F                                      
           BLOCK       CONTAINS  0.                                     
                                                                        
           COPY 'I#RDABAP'.                                             
                                                                        
      *---------------------------------------------------------------* 
      *  OUTPUT...: ARQ. DE DADOS RDABB006 (SOMENTE CASOS ATUALIZADOS)* 
      *             ORG. SEQUENCIAL   -   LRECL = 320 BYTES           * 
      *---------------------------------------------------------------* 
                                                                        
       FD  HPURDABS                                                     
           LABEL       RECORD    STANDARD                               
           RECORDING   MODE      F                                      
           BLOCK       CONTAINS  0.                                     
                                                                        
       01  FD-REG-HPURDABS           PIC X(320) VALUE SPACES.           
                                                                        
      *---------------------------------------------------------------* 
       WORKING-STORAGE SECTION.                                         
      *---------------------------------------------------------------* 
       01   FILLER                     PIC  X(32) VALUE                 
            'INICIO DA WORKING RDAB0262'.                               
                                                                        
      *---------------------------------------------------------------* 
      *          CAMPOS UTILIZADOS PARA CANCELAR PROCESSAMENTO        * 
      *---------------------------------------------------------------* 
                                                                        
       01  FILLER.                                                      
           03  WRK-FS-BKPRDAB          PIC  X(02) VALUE SPACES.         
           03  WRK-FS-HPURDABE         PIC  X(02) VALUE SPACES.         
           03  WRK-FS-HPURDABS         PIC  X(02) VALUE SPACES.         
           03  WRK-OPERACAO            PIC  X(13) VALUE SPACES.         
           03  WRK-ABERTURA            PIC  X(13) VALUE 'NA ABERTURA'.  
           03  WRK-LEITURA             PIC  X(13) VALUE 'NA LEITURA'.   
           03  WRK-GRAVACAO            PIC  X(13) VALUE 'NA GRAVACAO'.  
           03  WRK-FECHAMENTO          PIC  X(13) VALUE 'NO FECHAMENTO'.
           03  WRK-BATCH               PIC  X(08) VALUE 'BATCH'.        
      *---------------------------------------------------------------* 
      *--  ACUMULADORES.                                                
                                                                        
       01  FILLER.                                                      
           03  ACU-LDS-BKPRDAB         PIC 9(09) COMP-3    VALUE  ZEROS.
           03  ACU-LDS-HPURDABE        PIC 9(09) COMP-3    VALUE  ZEROS.
           03  ACU-GRV-HPURDABS        PIC 9(09) COMP-3    VALUE  ZEROS.
      *                                                                 
      *----------------------------------------------------------------*
      *--  EDICAO.                                                      
                                                                        
       01  FILLER.                                                      
           03  WRK-EDIT01              PIC ZZZ.ZZZ.ZZ9     VALUE  ZEROS.
           03  WRK-EDIT02              PIC ZZZ.ZZZ.ZZ9     VALUE  ZEROS.
           03  WRK-EDIT05              PIC ZZZ.ZZZ.ZZ9     VALUE  ZEROS.
                                                                        
      *--  CHAVE BKPRDAB.                                               
       01  WRK-CHV-BKPRDAB.                                             
           03  WRK-CHV-CADU-CPF      PIC  9(009) VALUE ZEROS.           
           03  WRK-CHV-CADU-FILIAL   PIC  9(005) VALUE ZEROS.           
                                                                        
                                                                        
      *--  CHAVE HPURDABE.                                              
       01  WRK-CHV-HPURDABE.                                            
           03  WRK-CHV-RDAB-CPF      PIC  9(009) VALUE ZEROS.           
           03  WRK-CHV-RDAB-FILIAL   PIC  9(005) VALUE ZEROS.           
                                                                        
      *----------------------------------------------------------------*
       01  FILLER   PIC  X(32) VALUE '*       AREA DA BRAD7100       *'.
      *----------------------------------------------------------------*
                                                                        
       COPY 'I#BRAD7C'.                                                 
                                                                        
      *---------------------------------------------------------------* 
       01   FILLER                     PIC  X(32)    VALUE              
            'FIM DA WORKING RDAB0262'.                                  
                                                                        
      *===============================================================* 
       PROCEDURE DIVISION.                                              
      *===============================================================* 
                                                                        
      *---------------------------------------------------------------* 
       0000-INICIAR SECTION.                                            
      *---------------------------------------------------------------* 
                                                                        
           PERFORM 1000-INICIALIZAR.                                    
                                                                        
           PERFORM 2000-LER-BKPRDAB                                     
           IF      WRK-FS-BKPRDAB      EQUAL '10'                       
                   DISPLAY '**************** RDAB0262 ***************'  
                   DISPLAY '*                                       *'  
                   DISPLAY '* ARQUIVO DE ENTRADA - BKPRDAB - VAZIO *'   
                   DISPLAY '*        PROCESSAMENTO ENCERRADO        *'  
                   DISPLAY '*                                       *'  
                   DISPLAY '**************** RDAB0262 ***************'  
                   PERFORM  9000-FINALIZAR                              
           END-IF.                                                      
                                                                        
           PERFORM 3000-LER-HPURDABE                                    
           IF      WRK-FS-HPURDABE     EQUAL '10'                       
                   DISPLAY '**************** RDAB0262 ***************'  
                   DISPLAY '*                                       *'  
                   DISPLAY '* ARQUIVO DE ENTRADA - HPURDABE - VAZIO *'  
                   DISPLAY '*        PROCESSAMENTO ENCERRADO        *'  
                   DISPLAY '*                                       *'  
                   DISPLAY '**************** RDAB0262 ***************'  
           END-IF                                                       
                                                                        
           PERFORM 4000-PROCESSAR     UNTIL                             
                  (WRK-FS-BKPRDAB     EQUAL '10')                       
                                                                        
                                                                        
           PERFORM 7000-DISPLAY-TOTAIS                                  
                                                                        
           PERFORM 9000-FINALIZAR.                                      
                                                                        
      *---------------------------------------------------------------* 
       0000-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1000-INICIALIZAR              SECTION.                           
      *---------------------------------------------------------------* 
                                                                        
           OPEN  INPUT  BKPRDAB                                         
                        HPURDABE                                        
                 OUTPUT HPURDABS                                        
                                                                        
           MOVE  WRK-ABERTURA          TO    WRK-OPERACAO               
           PERFORM  1100-TESTAR-FILE-STATUS.                            
                                                                        
      *---------------------------------------------------------------* 
       1000-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1100-TESTAR-FILE-STATUS       SECTION.                           
      *---------------------------------------------------------------* 
                                                                        
           PERFORM  1110-TESTAR-FS-BKPRDAB                              
           PERFORM  1120-TESTAR-FS-HPURDABE                             
           PERFORM  1150-TESTAR-FS-HPURDABS.                            
                                                                        
      *---------------------------------------------------------------* 
       1100-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1110-TESTAR-FS-BKPRDAB        SECTION.                           
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-FS-BKPRDAB          NOT EQUAL  '00'                   
              DISPLAY '************** RDAB0262 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              BKPRDAB             *'            
              DISPLAY '*         FILE STATUS =  ' WRK-FS-BKPRDAB        
                                                 '         *'           
              DISPLAY '************** RDAB0262 *************'           
              PERFORM 9999-ROTINA-ERRO                                  
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       1110-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1120-TESTAR-FS-HPURDABE       SECTION.                           
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-FS-HPURDABE         NOT EQUAL  '00'                   
              DISPLAY '************** RDAB0262 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              HPURDABE             *'           
              DISPLAY '*         FILE STATUS =  ' WRK-FS-HPURDABE       
                                                 '         *'           
              DISPLAY '************** RDAB0262 *************'           
              PERFORM 9999-ROTINA-ERRO                                  
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       1120-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1150-TESTAR-FS-HPURDABS        SECTION.                          
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-FS-HPURDABS           NOT EQUAL  '00'                 
              DISPLAY '************** RDAB0262 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              HPURDABS              *'          
              DISPLAY '*         FILE STATUS =  ' WRK-FS-HPURDABS       
                                                 '         *'           
              DISPLAY '************** RDAB0262 *************'           
              PERFORM 9999-ROTINA-ERRO                                  
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       1150-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       2000-LER-BKPRDAB             SECTION.                            
      *---------------------------------------------------------------* 
                                                                        
           READ    BKPRDAB                                              
                                                                        
           IF      WRK-FS-BKPRDAB      EQUAL  '10'                      
                   MOVE HIGH-VALUES    TO      WRK-CHV-BKPRDAB          
                   GO                  TO      2000-99-FIM              
           END-IF.                                                      
                                                                        
           MOVE    WRK-LEITURA         TO      WRK-OPERACAO             
           PERFORM 1110-TESTAR-FS-BKPRDAB                               
                                                                        
           MOVE PVV-CGCNUM             OF  BKPRDAB                      
                                       TO  WRK-CHV-CADU-CPF             
           MOVE PVV-CGCFIL             OF  BKPRDAB                      
                                       TO  WRK-CHV-CADU-FILIAL          
                                                                        
           ADD     1                   TO  ACU-LDS-BKPRDAB.             
                                                                        
      *---------------------------------------------------------------* 
       2000-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       3000-LER-HPURDABE            SECTION.                            
      *---------------------------------------------------------------* 
                                                                        
           READ    HPURDABE                                             
                                                                        
           IF      WRK-FS-HPURDABE     EQUAL  '10'                      
                   MOVE HIGH-VALUES    TO  WRK-CHV-HPURDABE             
                   GO                  TO  3000-99-FIM                  
           END-IF.                                                      
                                                                        
           MOVE    WRK-LEITURA         TO  WRK-OPERACAO                 
           PERFORM 1120-TESTAR-FS-HPURDABE                              
                                                                        
           MOVE CADUV000-CCPF-CNPJ         OF  HPURDABE                 
                                       TO  WRK-CHV-RDAB-CPF             
           MOVE CADUV000-CFLIAL-CPF-CNPJ   OF  HPURDABE                 
                                       TO  WRK-CHV-RDAB-FILIAL          
                                                                        
           ADD     1                   TO  ACU-LDS-HPURDABE.            
                                                                        
      *---------------------------------------------------------------* 
       3000-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       4000-PROCESSAR               SECTION.                            
      *---------------------------------------------------------------* 
                                                                        
           IF  WRK-CHV-BKPRDAB      GREATER   WRK-CHV-HPURDABE          
               PERFORM 3000-LER-HPURDABE                                
           ELSE                                                         
               IF  WRK-CHV-BKPRDAB  LESS      WRK-CHV-HPURDABE          
                   PERFORM 2000-LER-BKPRDAB                             
               ELSE                                                     
                   PERFORM 5000-GRAVAR-HPURDABS                         
                   PERFORM 3000-LER-HPURDABE                            
                   PERFORM 2000-LER-BKPRDAB                             
               END-IF                                                   
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       4000-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       5000-GRAVAR-HPURDABS          SECTION.                           
      *---------------------------------------------------------------* 
                                                                        
           WRITE  FD-REG-HPURDABS    FROM      REG-CADUV000.            
           MOVE   WRK-GRAVACAO         TO      WRK-OPERACAO             
           PERFORM  1150-TESTAR-FS-HPURDABS                             
                                                                        
           ADD 1                      TO   ACU-GRV-HPURDABS.            
                                                                        
      *---------------------------------------------------------------* 
       5000-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       7000-DISPLAY-TOTAIS        SECTION.                              
      *---------------------------------------------------------------* 
                                                                        
           MOVE     ACU-LDS-BKPRDAB    TO    WRK-EDIT01                 
           MOVE     ACU-LDS-HPURDABE   TO    WRK-EDIT02                 
           MOVE     ACU-GRV-HPURDABS   TO    WRK-EDIT05                 
                                                                        
           DISPLAY '******************** RDAB0262 ********************' 
           DISPLAY '*                                                *' 
           DISPLAY '*  TOTAL REG. LIDOS    - BKPRDAB  : 'WRK-EDIT01'  *'
           DISPLAY '*  TOTAL REG. LIDOS    - HPURDABE : 'WRK-EDIT02'  *'
           DISPLAY '*  TOTAL REG. GRAVADOS - HPURDABS : 'WRK-EDIT05'  *'
           DISPLAY '*                                                *' 
           DISPLAY '******************** RDAB0262 ********************'.
                                                                        
      *---------------------------------------------------------------* 
       7000-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       9000-FINALIZAR             SECTION.                              
      *---------------------------------------------------------------* 
                                                                        
           CLOSE  BKPRDAB                                               
                  HPURDABE                                              
                  HPURDABS.                                             
                                                                        
           MOVE   WRK-FECHAMENTO       TO      WRK-OPERACAO             
           PERFORM  1100-TESTAR-FILE-STATUS.                            
                                                                        
           STOP RUN.                                                    
                                                                        
      *---------------------------------------------------------------* 
       9000-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *----------------------------------------------------------------*
       9999-ROTINA-ERRO SECTION.                                        
      *----------------------------------------------------------------*
                                                                        
           MOVE   'RDAB0262'           TO  ERR-PGM.                     
           MOVE   'APL'                TO ERR-TIPO-ACESSO.              
           CALL   'BRAD7100'        USING  WRK-BATCH                    
                                           ERRO-AREA.                   
                                                                        
           GOBACK.                                                      
                                                                        
      *----------------------------------------------------------------*
       9999-99-FIM. EXIT.                                               
      *----------------------------------------------------------------*
                                                                        

      *===============================================================* 
       IDENTIFICATION DIVISION.                                         
      *===============================================================* 
                                                                        
       PROGRAM-ID. RDAB0258.                                            
       AUTHOR. GABRIEL.                                                 
                                                                        
      *===============================================================* 
      *                 B R Q    I T   S E R V I C E S                * 
      *---------------------------------------------------------------* 
      *                                                               * 
      *   PROGRAMA    : RDAB0258                                      * 
      *   PROGRAMADOR : GABRIEL MACORIN        - BRQ IT SERVICES      * 
      *   ANALISTA    : ROBSON VELLASQUES      - BRQ IT SERVICES      * 
      *   SUPERVISOR  : JUNIOR RIBAMAR         - GP. 70               * 
      *   DATA        : NOVEMBRO/2014                                 * 
      *                                                               * 
      *   OBJETIVO    : BALANCE LINE, SEPARA OS REGISTROS ACEITOS DOS * 
      *                 REJEITADOS                                    * 
      *   ARQUIVOS:                                                   * 
      *   ---------------------------------------------------------   * 
      *   DDNAME   |I/O| DESCRICAO               | BOOK     | LRECL   * 
      *   ---------+---+-------------------------+----------+------   * 
      *   ENTRAD1  | I | CADASTRO DE TELEFONE    | I#RDAB15 |   44    * 
      *   ENTRAD2  | I | PARC VENCIDAS/VINCENDAS | I#RDAB01 |   150   * 
      *   ACEITE   | O | REGISTROS ACEITOS       | I#RDAB15 |   44    * 
      *   DESPZD   | O | REGISTROS REJEITADOS    | I#RDAB15 |   44    * 
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
                                                                        
           SELECT  ENTRAD1  ASSIGN  TO  ENTRAD1                         
                   FILE      STATUS  IS  WRK-FS-ENTRAD1.                
                                                                        
           SELECT  ENTRAD2  ASSIGN  TO  ENTRAD2                         
                   FILE      STATUS  IS  WRK-FS-ENTRAD2.                
                                                                        
           SELECT  ACEITE    ASSIGN  TO  ACEITE                         
                   FILE      STATUS  IS  WRK-FS-ACEITE.                 
                                                                        
           SELECT  DESPZD    ASSIGN  TO  DESPZD                         
                   FILE      STATUS  IS  WRK-FS-DESPZD.                 
      *===============================================================* 
       DATA DIVISION.                                                   
      *===============================================================* 
                                                                        
      *---------------------------------------------------------------* 
       FILE SECTION.                                                    
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
      *  INPUT....: ENTRAD1                                             
      *             ORG. SEQUENCIAL    -  LRECL = 44 BYTES            * 
      *---------------------------------------------------------------* 
                                                                        
       FD  ENTRAD1                                                      
           LABEL       RECORD    STANDARD                               
           RECORDING   MODE      F                                      
           BLOCK       CONTAINS  0.                                     
                                                                        
       01  FD-REG-ENTRAD1              PIC X(044).                      
                                                                        
      *---------------------------------------------------------------* 
      *  INPUT....: ENTRAD2                                           * 
      *             ORG. SEQUENCIAL    -  LRECL = 150 BYTES           * 
      *---------------------------------------------------------------* 
                                                                        
       FD  ENTRAD2                                                      
           LABEL       RECORD    STANDARD                               
           RECORDING   MODE      F                                      
           BLOCK       CONTAINS  0.                                     
                                                                        
       01  FD-REG-ENTRAD2              PIC X(150).                      
                                                                        
      *---------------------------------------------------------------* 
      *  OUTPUT...: ACEITE                                            * 
      *             ORG. SEQUENCIAL   -   LRECL = 44 BYTES            * 
      *---------------------------------------------------------------* 
                                                                        
       FD  ACEITE                                                       
           LABEL       RECORD    STANDARD                               
           RECORDING   MODE      F                                      
           BLOCK       CONTAINS  0.                                     
                                                                        
       01  FD-REG-ACEITE               PIC X(044).                      
                                                                        
      *---------------------------------------------------------------* 
      *  OUTPUT...: DESPZD                                            * 
      *             ORG. SEQUENCIAL   -   LRECL = 44 BYTES            * 
      *---------------------------------------------------------------* 
                                                                        
       FD  DESPZD                                                       
           LABEL       RECORD    STANDARD                               
           RECORDING   MODE      F                                      
           BLOCK       CONTAINS  0.                                     
                                                                        
       01  FD-REG-DESPZD               PIC X(044).                      
                                                                        
      *---------------------------------------------------------------* 
       WORKING-STORAGE SECTION.                                         
      *---------------------------------------------------------------* 
       01   FILLER                     PIC  X(32) VALUE                 
            'INICIO DA WORKING RDAB0258'.                               
                                                                        
      *-- ------------------------------------------------------------* 
      *          CAMPOS PARA CONTROLE DE FILE STATUS                  * 
      *---------------------------------------------------------------* 
                                                                        
       01  FILLER.                                                      
           03  WRK-FS-ENTRAD1          PIC  X(02) VALUE SPACES.         
           03  WRK-FS-ENTRAD2          PIC  X(02) VALUE SPACES.         
           03  WRK-FS-ACEITE           PIC  X(02) VALUE SPACES.         
           03  WRK-FS-DESPZD           PIC  X(02) VALUE SPACES.         
           03  WRK-OPERACAO            PIC  X(13) VALUE SPACES.         
           03  WRK-ABERTURA            PIC  X(13) VALUE 'NA ABERTURA'.  
           03  WRK-LEITURA             PIC  X(13) VALUE 'NA LEITURA'.   
           03  WRK-FECHAMENTO          PIC  X(13) VALUE 'NO FECHAMENTO'.
           03  WRK-GRAVACAO            PIC  X(13) VALUE 'NA GRAVACAO'.  
           03  WRK-BATCH               PIC  X(08) VALUE 'BATCH'.        
      *                                                                 
      *---------------------------------------------------------------- 
      *--  ACUMULADORES.                                                
                                                                        
       01  FILLER.                                                      
          03  ACU-LDS-ENTRAD1         PIC 9(09) COMP-3    VALUE  ZEROS. 
          03  ACU-LDS-ENTRAD2         PIC 9(09) COMP-3    VALUE  ZEROS. 
          03  ACU-GRV-ACEITE          PIC 9(09) COMP-3    VALUE  ZEROS. 
          03  ACU-GRV-DESPZD          PIC 9(09) COMP-3    VALUE  ZEROS. 
      *---------------------------------------------------------------- 
      *--  EDICAO.                                                      
       01  WRK-FILLER.                                                  
          03  WRK-EDIT01              PIC ZZZ.ZZZ.ZZ9     VALUE  ZEROS. 
          03  WRK-EDIT02              PIC ZZZ.ZZZ.ZZ9     VALUE  ZEROS. 
          03  WRK-EDIT03              PIC ZZZ.ZZZ.ZZ9     VALUE  ZEROS. 
          03  WRK-EDIT04              PIC ZZZ.ZZZ.ZZ9     VALUE  ZEROS. 
      *---------------------------------------------------------------- 
      *--  CHAVE ENTRAD1.                                               
       01  WRK-CHV-ENTRAD1.                                             
           05  WRK-CHV-BANCO1            PIC  9(003) VALUE ZEROS.       
           05  WRK-CHV-AGENC1            PIC  9(005) VALUE ZEROS.       
           05  WRK-CHV-CONTA1            PIC  9(013) VALUE ZEROS.       
           05  WRK-CHV-CINDORI1          PIC  9(001) VALUE ZEROS.       
                                                                        
      *--  CHAVE ENTRAD2.                                               
       01  WRK-CHV-ENTRAD2.                                             
           05  WRK-CHV-BANCO2            PIC  9(003) VALUE ZEROS.       
           05  WRK-CHV-AGENC2            PIC  9(005) VALUE ZEROS.       
           05  WRK-CHV-CONTA2            PIC  9(013) VALUE ZEROS.       
           05  WRK-CHV-CINDORI2          PIC  9(001) VALUE ZEROS.       
                                                                        
      *--  CHAVE AUXILIAR                                               
       01  WRK-TEMP-ENTRAD1.                                            
          03  WRK-TEMP-CBCO            PIC S9(003) VALUE ZEROS.         
          03  WRK-AUX-CBCO             REDEFINES WRK-TEMP-CBCO          
                                       PIC  9(003).                     
          03  WRK-TEMP-CAG-BCRIA       PIC S9(005) VALUE ZEROS.         
          03  WRK-AUX-CAG-BCRIA        REDEFINES WRK-TEMP-CAG-BCRIA     
                                       PIC  9(005).                     
          03  WRK-TEMP-CCTA-CORR       PIC S9(013) VALUE ZEROS.         
          03  WRK-AUX-CCTA-CORR        REDEFINES WRK-TEMP-CCTA-CORR     
                                       PIC  9(013).                     
          03  WRK-AUX-CINDCD-ORIGE     PIC  9(001) VALUE ZEROS.         
          03  WRK-TEMP-CINDCD-ORIGE    REDEFINES WRK-AUX-CINDCD-ORIGE   
                                       PIC  X(001).                     
      *----------------------------------------------------------------*
       01  FILLER   PIC  X(32) VALUE '*       AREA DA BRAD7100       *'.
      *----------------------------------------------------------------*
                                                                        
       COPY 'I#BRAD7C'.                                                 
                                                                        
       01  FILLER   PIC  X(32) VALUE '*       AREA DE BOOKS          *'.
      *----------------------------------------------------------------*
                                                                        
       COPY 'I#RDAB01'.                                                 
       COPY 'I#RDAB15'.                                                 
                                                                        
      *---------------------------------------------------------------* 
                                                                        
       01   FILLER                     PIC  X(32)    VALUE              
            'FIM DA WORKING RDAB0258'.                                  
                                                                        
      *===============================================================* 
       PROCEDURE DIVISION.                                              
      *===============================================================* 
      *---------------------------------------------------------------* 
       0000-INICIAR SECTION.                                            
      *---------------------------------------------------------------* 
                                                                        
           PERFORM 1000-INICIALIZAR                                     
                                                                        
           PERFORM 2000-LER-ENTRAD1                                     
      *    TESTAR SE ARQUIVO 1 VAZIO                                    
           IF  WRK-FS-ENTRAD1     EQUAL '10'                            
               DISPLAY '**************** RDAB0258 ***************'      
               DISPLAY '*                                       *'      
               DISPLAY '* ARQUIVO DE ENTRADA - ENTRAD1 - VAZIO  *'      
               DISPLAY '*        PROCESSAMENTO ENCERRADO        *'      
               DISPLAY '*                                       *'      
               DISPLAY '**************** RDAB0258 ***************'      
           END-IF.                                                      
                                                                        
           PERFORM 3000-LER-ENTRAD2                                     
      *    TESTAR SE ARQUIVO 2 VAZIO                                    
           IF  WRK-FS-ENTRAD1     EQUAL '10'                            
               DISPLAY '**************** RDAB0258 ***************'      
               DISPLAY '*                                       *'      
               DISPLAY '* ARQUIVO DE ENTRADA - ENTRAD2  - VAZIO *'      
               DISPLAY '*        PROCESSAMENTO ENCERRADO        *'      
               DISPLAY '*                                       *'      
               DISPLAY '**************** RDAB0258 ***************'      
           END-IF.                                                      
                                                                        
           PERFORM 4000-PROCESSAR UNTIL                                 
             WRK-FS-ENTRAD1 EQUAL '10' AND WRK-FS-ENTRAD1 EQUAL '10'    
                                                                        
           PERFORM 9000-FINALIZAR.                                      
                                                                        
      *---------------------------------------------------------------* 
       0000-99-FIM.   EXIT.                                             
                                                                        
      *---------------------------------------------------------------* 
       1000-INICIALIZAR              SECTION.                           
      *---------------------------------------------------------------* 
                                                                        
           OPEN  INPUT  ENTRAD1                                         
                        ENTRAD2                                         
                 OUTPUT ACEITE                                          
                        DESPZD                                          
                                                                        
           MOVE  WRK-ABERTURA          TO    WRK-OPERACAO               
           PERFORM  1100-TESTAR-FILE-STATUS.                            
                                                                        
      *---------------------------------------------------------------* 
       1000-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1100-TESTAR-FILE-STATUS       SECTION.                           
                                                                        
           PERFORM  1110-TESTAR-FS-ENTRAD1                              
           PERFORM  1120-TESTAR-FS-ENTRAD2                              
           PERFORM  1150-TESTAR-FS-ACEITE                               
           PERFORM  1160-TESTAR-FS-DESPZD.                              
                                                                        
      *---------------------------------------------------------------* 
       1100-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1110-TESTAR-FS-ENTRAD1          SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-FS-ENTRAD1           NOT EQUAL  '00'                  
              DISPLAY '************** RDAB0258 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              ENTRAD1                  *'       
              DISPLAY '*         FILE STATUS =  ' WRK-FS-ENTRAD1        
              DISPLAY '************** RDAB0258 *************'           
              PERFORM 9999-ROTINA-ERRO                                  
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       1110-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1120-TESTAR-FS-ENTRAD2          SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-FS-ENTRAD2           NOT EQUAL  '00'                  
              DISPLAY '************** RDAB0258 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              ENTRAD2                  *'       
              DISPLAY '*         FILE STATUS =  ' WRK-FS-ENTRAD2        
                                                 '         *'           
              DISPLAY '************** RDAB0258 *************'           
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       1120-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1150-TESTAR-FS-ACEITE           SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-FS-ACEITE            NOT EQUAL  '00'                  
              DISPLAY '************** RDAB0258 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              ACEITE                   *'       
              DISPLAY '*         FILE STATUS =  ' WRK-FS-ACEITE         
                                                 '         *'           
              DISPLAY '************** RDAB0258 *************'           
              PERFORM 9999-ROTINA-ERRO                                  
           END-IF.                                                      
      *---------------------------------------------------------------* 
       1150-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1160-TESTAR-FS-DESPZD           SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-FS-DESPZD          NOT EQUAL  '00'                    
              DISPLAY '************** RDAB0258 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              DESPZD                   *'       
              DISPLAY '*         FILE STATUS =  ' WRK-FS-DESPZD         
                                                 '         *'           
              DISPLAY '************** RDAB0258 *************'           
              PERFORM 9999-ROTINA-ERRO                                  
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       1160-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       2000-LER-ENTRAD1                SECTION.                         
      *---------------------------------------------------------------* 
           READ    ENTRAD1             INTO  AFN-REG                    
           IF      WRK-FS-ENTRAD1      NOT EQUAL '10'                   
                                                                        
                   MOVE    WRK-LEITURA         TO      WRK-OPERACAO     
                   PERFORM 1110-TESTAR-FS-ENTRAD1                       
                                                                        
                   MOVE    AFN-CBCO            TO  WRK-TEMP-CBCO        
                   MOVE    AFN-CAG-BCRIA       TO  WRK-TEMP-CAG-BCRIA   
                   MOVE    AFN-CCTA-CORR       TO  WRK-TEMP-CCTA-CORR   
                   MOVE AFN-CINDCD-ORIGE-FONE  TO  WRK-TEMP-CINDCD-ORIGE
                                                                        
                   MOVE WRK-AUX-CBCO          TO  WRK-CHV-BANCO1        
                   MOVE WRK-AUX-CAG-BCRIA     TO  WRK-CHV-AGENC1        
                   MOVE WRK-AUX-CCTA-CORR     TO  WRK-CHV-CONTA1        
                   MOVE WRK-AUX-CINDCD-ORIGE  TO  WRK-CHV-CINDORI1      
                                                                        
                   ADD     1                   TO  ACU-LDS-ENTRAD1      
           END-IF.                                                      
      *---------------------------------------------------------------* 
       2000-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       3000-LER-ENTRAD2            SECTION.                             
      *---------------------------------------------------------------* 
           READ    ENTRAD2             INTO  PVV-REGISTRO               
           IF      WRK-FS-ENTRAD2      NOT EQUAL '10'                   
                                                                        
                   MOVE    WRK-LEITURA         TO      WRK-OPERACAO     
                   PERFORM 1120-TESTAR-FS-ENTRAD2                       
                                                                        
                   MOVE    PVV-BANCO           TO  WRK-CHV-BANCO2       
                   MOVE    PVV-AGENCIA         TO  WRK-CHV-AGENC2       
                   MOVE    PVV-CONTA           TO  WRK-CHV-CONTA2       
                   MOVE    1                   TO  WRK-CHV-CINDORI2     
                                                                        
                   ADD     1                   TO      ACU-LDS-ENTRAD2  
           END-IF.                                                      
      *---------------------------------------------------------------* 
       3000-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       4000-PROCESSAR               SECTION.                            
      *---------------------------------------------------------------* 
                                                                        
           IF    WRK-CHV-ENTRAD1   EQUAL   WRK-CHV-ENTRAD2              
                 PERFORM 5000-GRAVAR-ACEITE                             
                 PERFORM 2000-LER-ENTRAD1                               
           ELSE                                                         
               IF  WRK-CHV-ENTRAD1 LESS WRK-CHV-ENTRAD2                 
                   PERFORM 5100-GRAVAR-DESPZD                           
                   PERFORM 2000-LER-ENTRAD1                             
                ELSE                                                    
                   PERFORM 3000-LER-ENTRAD2                             
                END-IF                                                  
           END-IF.                                                      
                                                                        
       4000-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       5000-GRAVAR-ACEITE          SECTION.                             
      *---------------------------------------------------------------* 
           WRITE  FD-REG-ACEITE        FROM    AFN-REG                  
           MOVE   WRK-GRAVACAO         TO      WRK-OPERACAO             
           PERFORM  1150-TESTAR-FS-ACEITE                               
                                                                        
           ADD 1                      TO   ACU-GRV-ACEITE.              
                                                                        
      *---------------------------------------------------------------* 
       5000-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       5100-GRAVAR-DESPZD          SECTION.                             
      *---------------------------------------------------------------* 
                                                                        
           WRITE  FD-REG-DESPZD        FROM    AFN-REG                  
           MOVE   WRK-GRAVACAO         TO      WRK-OPERACAO             
           PERFORM  1160-TESTAR-FS-DESPZD                               
                                                                        
           ADD 1                      TO   ACU-GRV-DESPZD.              
      *---------------------------------------------------------------* 
       5100-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       7000-DISPLAY-TOTAIS        SECTION.                              
      *---------------------------------------------------------------* 
                                                                        
           MOVE     ACU-LDS-ENTRAD1    TO    WRK-EDIT01                 
           MOVE     ACU-LDS-ENTRAD2    TO    WRK-EDIT02                 
           MOVE     ACU-GRV-ACEITE     TO    WRK-EDIT03                 
           MOVE     ACU-GRV-DESPZD     TO    WRK-EDIT04                 
                                                                        
           DISPLAY '******************** RDAB0258 *******************'  
           DISPLAY '*                                               *'  
           DISPLAY '*  TOTAL REG. LIDOS    - ENTRAD1 : 'WRK-EDIT01'  *' 
           DISPLAY '*  TOTAL REG. LIDOS    - ENTRAD2 : 'WRK-EDIT02'  *' 
           DISPLAY '*  TOTAL REG. GRAVADOS - ACEITE  : 'WRK-EDIT03'  *' 
           DISPLAY '*  TOTAL REG. GRAVADOS - DESPZD  : 'WRK-EDIT04'  *' 
           DISPLAY '*                                               *'  
           DISPLAY '******************** RDAB0258 *******************'. 
                                                                        
      *---------------------------------------------------------------* 
       7000-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       9000-FINALIZAR             SECTION.                              
      *---------------------------------------------------------------* 
                                                                        
           CLOSE  ENTRAD1                                               
                  ENTRAD2                                               
                  ACEITE                                                
                  DESPZD                                                
                                                                        
           MOVE   WRK-FECHAMENTO       TO      WRK-OPERACAO             
           PERFORM  1100-TESTAR-FILE-STATUS.                            
                                                                        
           PERFORM 7000-DISPLAY-TOTAIS                                  
                                                                        
           STOP RUN.                                                    
                                                                        
      *---------------------------------------------------------------* 
       9000-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *----------------------------------------------------------------*
       9999-ROTINA-ERRO SECTION.                                        
      *----------------------------------------------------------------*
                                                                        
           MOVE   'RDAB0258'           TO  ERR-PGM.                     
           MOVE   'APL'                TO ERR-TIPO-ACESSO.              
           CALL   'BRAD7100'           USING  WRK-BATCH                 
                                              ERRO-AREA.                
                                                                        
                                                                        
      *----------------------------------------------------------------*
       9999-99-FIM. EXIT.                                               
      *----------------------------------------------------------------*

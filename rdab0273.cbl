      *===============================================================* 
       IDENTIFICATION                  DIVISION.                        
      *===============================================================* 
                                                                        
       PROGRAM-ID. RDAB0273.                                            
       AUTHOR. ELSINO SILVA.                                            
                                                                        
      *===============================================================* 
      *                 B R Q    I T   S E R V I C E S                * 
      *---------------------------------------------------------------* 
      *                                                               * 
      *   PROGRAMA    : RDAB0273                                      * 
      *   PROGRAMADOR : ELSINO SILVA           - BRQ IT SERVICES      * 
      *   ANALISTA    : FRANCISCO FREIRE       - BRQ IT SERVICES      * 
      *   SUPERVISOR  : JUNIOR RIBAMAR         - GP. 70               * 
      *   DATA        : NOVEMBRO/2014                                 * 
      *                                                               * 
      *   OBJETIVO    : BALANCE LINE DE DADOS PRINCIPAIS DO CADU COM  * 
      *                 DADOS PRINCIPAIS DO RDAB PF (RDBAB008).       * 
      *   ARQUIVOS:                                                   * 
      *   ---------------------------------------------------------   * 
      *   DDNAME   |I/O| DESCRICAO               | BOOK     | LRECL   * 
      *   ---------+---+-------------------------+----------+------   * 
      *   NOMECADU | I | DADOS PF CADU           | WORKING  |   100   * 
      *   NOMERDAB | I | DADOS PF RDAB           | I#RDAB12 |   270   * 
      *   CADURDAB | O | DADOS PF RDAB ALTERADOS | I#RDAB12 |   270   * 
      *   DESPRDAB | O | DADOS PF RDAB DESPREZADO| I#RDAB12 |   270   * 
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
                                                                        
           SELECT  NOMECADU  ASSIGN  TO  UT-S-NOMECADU                  
                   FILE      STATUS  IS  WRK-FS-NOMECADU.               
                                                                        
           SELECT  NOMERDAB  ASSIGN  TO  UT-S-NOMERDAB                  
                   FILE      STATUS  IS  WRK-FS-NOMERDAB.               
                                                                        
           SELECT  CADURDAB  ASSIGN  TO  UT-S-CADURDAB                  
                   FILE      STATUS  IS  WRK-FS-CADURDAB.               
                                                                        
           SELECT  ATULZPF   ASSIGN  TO  UT-S-ATULZPF                   
                   FILE      STATUS  IS  WRK-FS-ATULZPF.                
                                                                        
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
      *             ORG. SEQUENCIAL    -  LRECL = 130 BYTES           * 
      *---------------------------------------------------------------* 
                                                                        
       FD  NOMECADU                                                     
           LABEL       RECORD    STANDARD                               
           RECORDING   MODE      F                                      
           BLOCK       CONTAINS  0.                                     
                                                                        
       01  FD-REG-ARQATUAL.                                             
           03  FD-REG-CPF            PIC  9(009)         VALUE ZEROS.   
           03  FD-REG-FILIAL         PIC  9(005)         VALUE ZEROS.   
           03  FD-REG-CTRL           PIC  9(002)         VALUE ZEROS.   
           03  FD-REG-NOME-CADU      PIC  X(040)         VALUE SPACES.  
           03  FD-REG-NOME-RDAB      PIC  X(040)         VALUE SPACES.  
           03  FD-REG-CLUB           PIC  9(010)         VALUE ZEROS.   
           03  FILLER                PIC  X(024)         VALUE SPACES.  
                                                                        
      *---------------------------------------------------------------* 
      *  INPUT....: ARQ. NOME PF RDABB008                             * 
      *             ORG. SEQUENCIAL    -  LRECL = 270 BYTES           * 
      *---------------------------------------------------------------* 
                                                                        
       FD  NOMERDAB                                                     
           LABEL       RECORD    STANDARD                               
           RECORDING   MODE      F                                      
           BLOCK       CONTAINS  0.                                     
                                                                        
       01  FD-REG-NOMERDAB             PIC X(270).                      
      *---------------------------------------------------------------* 
      *  OUTPUT...: ARQ. DE DADOS DA PESSOA PRV                       * 
      *             ORG. SEQUENCIAL   -   LRECL = 270 BYTES           * 
      *---------------------------------------------------------------* 
                                                                        
       FD  CADURDAB                                                     
           LABEL       RECORD    STANDARD                               
           RECORDING   MODE      F                                      
           BLOCK       CONTAINS  0.                                     
                                                                        
       01  FD-REG-CADURDAB             PIC X(270).                      
      *---------------------------------------------------------------* 
      *  OUTPUT...: ARQ. DE DADOS DA PESSOA ATUALIZADOS               * 
      *             ORG. SEQUENCIAL   -   LRECL = 270 BYTES           * 
      *---------------------------------------------------------------* 
                                                                        
       FD  ATULZPF                                                      
           LABEL       RECORD    STANDARD                               
           RECORDING   MODE      F                                      
           BLOCK       CONTAINS  0.                                     
                                                                        
       01  FD-REG-ATULZPF              PIC X(081).                      
      *---------------------------------------------------------------* 
      *  OUTPUT...: ARQ. DE DADOS DO RDAB (DESPREZADOS)               * 
      *             ORG. SEQUENCIAL   -   LRECL = 130 BYTES           * 
      *---------------------------------------------------------------* 
                                                                        
       FD  DESPRDAB                                                     
           LABEL       RECORD    STANDARD                               
           RECORDING   MODE      F                                      
           BLOCK       CONTAINS  0.                                     
                                                                        
       01  FD-REG-DESPRDAB             PIC X(130).                      
      *---------------------------------------------------------------* 
       WORKING-STORAGE SECTION.                                         
      *---------------------------------------------------------------* 
       01   FILLER                     PIC  X(32) VALUE                 
            'INICIO DA WORKING RDAB0273'.                               
                                                                        
      *-- ------------------------------------------------------------* 
      *          CAMPOS UTILIZADOS PARA CANCELAR PROCESSAMENTO        * 
      *---------------------------------------------------------------* 
                                                                        
       01  FILLER.                                                      
           03  WRK-FS-NOMECADU         PIC  X(02) VALUE SPACES.         
           03  WRK-FS-NOMERDAB         PIC  X(02) VALUE SPACES.         
           03  WRK-FS-CADURDAB         PIC  X(02) VALUE SPACES.         
           03  WRK-FS-ATULZPF          PIC  X(02) VALUE SPACES.         
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
           03  ACU-LDS-NOMECADU        PIC 9(09) COMP-3    VALUE  ZEROS.
           03  ACU-LDS-NOMERDAB        PIC 9(09) COMP-3    VALUE  ZEROS.
           03  ACU-GRV-CADURDAB        PIC 9(09) COMP-3    VALUE  ZEROS.
           03  ACU-GRV-ATULZPF         PIC 9(09) COMP-3    VALUE  ZEROS.
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
                                                                        
      *--  CHAVE NOMECADU.                                              
       01  WRK-CHV-NOMECADU.                                            
           03  WRK-CHV-CADU-CPF      PIC  9(009) VALUE ZEROS.           
           03  WRK-CHV-CADU-FILIAL   PIC  9(005) VALUE ZEROS.           
                                                                        
      *--  CHAVE NOMERDAB.                                              
       01  WRK-CHV-NOMERDAB.                                            
           03  WRK-CHV-RDAB-CPF      PIC  9(009) VALUE ZEROS.           
           03  WRK-CHV-RDAB-FILIAL   PIC  9(005) VALUE ZEROS.           
                                                                        
                                                                        
      *--  CHAVE AUXILIAR                                               
       01  WRK-TEMP-NOMERDAB.                                           
          03  WRK-TEMP-RDAB-CPF         PIC S9(009) VALUE ZEROS.        
          03  WRK-AUX-RDAB-CPF          REDEFINES WRK-TEMP-RDAB-CPF     
                                        PIC  9(009).                    
          03  WRK-TEMP-RDAB-CTRL        PIC S9(002) VALUE ZEROS.        
          03  WRK-AUX-RDAB-CTRL         REDEFINES WRK-TEMP-RDAB-CTRL    
                                        PIC  9(002).                    
      *----------------------------------------------------------------*
       01  FILLER   PIC  X(32) VALUE '* AREA DA BRAD7100 E BOOKS     *'.
      *----------------------------------------------------------------*
                                                                        
       COPY 'I#BRAD7C'.                                                 
       COPY 'I#RDAB12'.                                                 
                                                                        
      *---------------------------------------------------------------* 
       01   FILLER                     PIC  X(32)    VALUE              
            'FIM DA WORKING RDAB0273'.                                  
                                                                        
      *===============================================================* 
       PROCEDURE DIVISION.                                              
      *===============================================================* 
                                                                        
      *---------------------------------------------------------------* 
       0000-INICIAR SECTION.                                            
      *---------------------------------------------------------------* 
           PERFORM 1000-INICIALIZAR.                                    
                                                                        
           PERFORM 2000-LER-NOMECADU                                    
           IF      WRK-FS-NOMECADU     EQUAL '10'                       
                   DISPLAY '**************** RDAB0273 ***************'  
                   DISPLAY '*                                       *'  
                   DISPLAY '* ARQUIVO DE ENTRADA - NOMECADU - VAZIO *'  
                   DISPLAY '*        PROCESSAMENTO ENCERRADO        *'  
                   DISPLAY '*                                       *'  
                   DISPLAY '**************** RDAB0273 ***************'  
                   PERFORM  9000-FINALIZAR                              
           END-IF.                                                      
                                                                        
           PERFORM 3000-LER-NOMERDAB                                    
           IF      WRK-FS-NOMERDAB     EQUAL '10'                       
                   DISPLAY '**************** RDAB0273 ***************'  
                   DISPLAY '*                                       *'  
                   DISPLAY '* ARQUIVO DE ENTRADA - NOMERDAB - VAZIO *'  
                   DISPLAY '*        PROCESSAMENTO ENCERRADO        *'  
                   DISPLAY '*                                       *'  
                   DISPLAY '**************** RDAB0273 ***************'  
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
           OPEN  INPUT  NOMECADU                                        
                        NOMERDAB                                        
                 OUTPUT CADURDAB                                        
                        DESPRDAB                                        
                        ATULZPF                                         
                                                                        
           MOVE  WRK-ABERTURA          TO    WRK-OPERACAO               
           PERFORM  1100-TESTAR-FILE-STATUS.                            
      *---------------------------------------------------------------* 
       1000-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1100-TESTAR-FILE-STATUS       SECTION.                           
      *---------------------------------------------------------------* 
           PERFORM  1110-TESTAR-FS-NOMECADU                             
           PERFORM  1120-TESTAR-FS-NOMERDAB                             
           PERFORM  1150-TESTAR-FS-CADURDAB.                            
           PERFORM  1160-TESTAR-FS-DESPRDAB.                            
           PERFORM  1170-TESTAR-FS-ATULZPF.                             
      *---------------------------------------------------------------* 
       1100-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1110-TESTAR-FS-NOMECADU       SECTION.                           
      *---------------------------------------------------------------* 
           IF WRK-FS-NOMECADU         NOT EQUAL  '00'                   
              DISPLAY '************** RDAB0273 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              NOMECADU             *'           
              DISPLAY '*         FILE STATUS =  ' WRK-FS-NOMECADU       
                                                 '         *'           
              DISPLAY '************** RDAB0273 *************'           
              PERFORM 9999-ROTINA-ERRO                                  
           END-IF.                                                      
      *---------------------------------------------------------------* 
       1110-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1120-TESTAR-FS-NOMERDAB       SECTION.                           
      *---------------------------------------------------------------* 
           IF WRK-FS-NOMERDAB         NOT EQUAL  '00'                   
              DISPLAY '************** RDAB0273 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              NOMERDAB             *'           
              DISPLAY '*         FILE STATUS =  ' WRK-FS-NOMERDAB       
                                                 '         *'           
              DISPLAY '************** RDAB0273 *************'           
              PERFORM 9999-ROTINA-ERRO                                  
           END-IF.                                                      
      *---------------------------------------------------------------* 
       1120-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1150-TESTAR-FS-CADURDAB        SECTION.                          
      *---------------------------------------------------------------* 
           IF WRK-FS-CADURDAB           NOT EQUAL  '00'                 
              DISPLAY '************** RDAB0273 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              CADURDAB              *'          
              DISPLAY '*         FILE STATUS =  ' WRK-FS-CADURDAB       
                                                 '         *'           
              DISPLAY '************** RDAB0273 *************'           
              PERFORM 9999-ROTINA-ERRO                                  
           END-IF.                                                      
      *---------------------------------------------------------------* 
       1150-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1160-TESTAR-FS-DESPRDAB        SECTION.                          
      *---------------------------------------------------------------* 
           IF WRK-FS-DESPRDAB           NOT EQUAL  '00'                 
              DISPLAY '************** RDAB0273 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              DESPRDAB              *'          
              DISPLAY '*         FILE STATUS =  ' WRK-FS-DESPRDAB       
                                                 '         *'           
              DISPLAY '************** RDAB0273 *************'           
              PERFORM 9999-ROTINA-ERRO                                  
           END-IF.                                                      
      *---------------------------------------------------------------* 
       1160-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1170-TESTAR-FS-ATULZPF         SECTION.                          
      *---------------------------------------------------------------* 
           IF WRK-FS-ATULZPF            NOT EQUAL  '00'                 
              DISPLAY '************** RDAB0273 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              ATULZPF               *'          
              DISPLAY '*         FILE STATUS =  ' WRK-FS-ATULZPF        
                                                 '         *'           
              DISPLAY '************** RDAB0273 *************'           
              PERFORM 9999-ROTINA-ERRO                                  
           END-IF.                                                      
      *---------------------------------------------------------------* 
       1170-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       2000-LER-NOMECADU            SECTION.                            
      *---------------------------------------------------------------* 
           READ    NOMECADU                                             
                                                                        
           IF      WRK-FS-NOMECADU     EQUAL  '10'                      
                   MOVE HIGH-VALUES    TO      WRK-CHV-NOMECADU         
                   GO                  TO      2000-99-FIM              
           END-IF.                                                      
                                                                        
           MOVE    WRK-LEITURA         TO      WRK-OPERACAO             
           PERFORM 1110-TESTAR-FS-NOMECADU                              
                                                                        
           MOVE FD-REG-CPF             TO  WRK-CHV-CADU-CPF             
           MOVE FD-REG-FILIAL          TO  WRK-CHV-CADU-FILIAL          
                                                                        
           ADD     1                   TO  ACU-LDS-NOMECADU.            
      *---------------------------------------------------------------* 
       2000-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       3000-LER-NOMERDAB            SECTION.                            
      *---------------------------------------------------------------* 
           READ    NOMERDAB            INTO   APF-REG                   
                                                                        
           IF      WRK-FS-NOMERDAB     EQUAL  '10'                      
                   MOVE HIGH-VALUES    TO      WRK-CHV-NOMERDAB         
                   GO                  TO      3000-99-FIM              
           END-IF.                                                      
                                                                        
           MOVE    WRK-LEITURA         TO      WRK-OPERACAO             
           PERFORM 1120-TESTAR-FS-NOMERDAB                              
                                                                        
           MOVE APF-CBASE-CPF          TO  WRK-TEMP-RDAB-CPF            
           MOVE APF-CCTRL-CNPJ-CPF     TO  WRK-TEMP-RDAB-CTRL           
                                                                        
           MOVE WRK-AUX-RDAB-CPF       TO  WRK-CHV-RDAB-CPF             
           MOVE WRK-AUX-RDAB-CTRL      TO  WRK-CHV-RDAB-FILIAL          
                                                                        
           ADD     1                   TO      ACU-LDS-NOMERDAB.        
      *---------------------------------------------------------------* 
       3000-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       4000-PROCESSAR               SECTION.                            
      *---------------------------------------------------------------* 
           IF    WRK-CHV-NOMECADU   GREATER   WRK-CHV-NOMERDAB          
                 ADD   1            TO  ACU-NAOACHOU-CADU               
                 PERFORM 5000-GRAVAR-CADURDAB                           
                 PERFORM 3000-LER-NOMERDAB                              
           ELSE                                                         
              IF   WRK-CHV-NOMECADU   LESS    WRK-CHV-NOMERDAB          
                   PERFORM 5100-GRAVAR-DESPRDAB                         
                   PERFORM 2000-LER-NOMECADU                            
              ELSE                                                      
                 IF  FD-REG-NOME-CADU  EQUAL  APF-IPRIM-TTLAR-RENEG     
                     ADD  1            TO  ACU-NOMEIGUAL-CADU           
                     PERFORM 5000-GRAVAR-CADURDAB                       
                     PERFORM 5100-GRAVAR-DESPRDAB                       
                 ELSE                                                   
                    IF (FD-REG-NOME-CADU                                
                                       EQUAL SPACES OR LOW-VALUES)      
                       ADD  1          TO  ACU-BRANCOS-CADU             
                       PERFORM 5000-GRAVAR-CADURDAB                     
                       PERFORM 5100-GRAVAR-DESPRDAB                     
                    ELSE                                                
                       MOVE  FD-REG-NOME-CADU                           
                                       TO  APF-IPRIM-TTLAR-RENEG        
                       PERFORM 5200-GRAVAR-ATULZPF                      
                       PERFORM 5000-GRAVAR-CADURDAB                     
                    END-IF                                              
                 END-IF                                                 
                 PERFORM 3000-LER-NOMERDAB                              
              END-IF                                                    
           END-IF.                                                      
      *---------------------------------------------------------------* 
       4000-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       5000-GRAVAR-CADURDAB          SECTION.                           
      *---------------------------------------------------------------* 
           WRITE  FD-REG-CADURDAB      FROM    APF-REG                  
           MOVE   WRK-GRAVACAO         TO      WRK-OPERACAO             
           PERFORM  1150-TESTAR-FS-CADURDAB                             
                                                                        
           ADD 1                      TO   ACU-GRV-CADURDAB.            
      *---------------------------------------------------------------* 
       5000-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       5100-GRAVAR-DESPRDAB          SECTION.                           
      *---------------------------------------------------------------* 
           WRITE  FD-REG-DESPRDAB      FROM    FD-REG-ARQATUAL          
           MOVE   WRK-GRAVACAO         TO      WRK-OPERACAO             
           PERFORM  1160-TESTAR-FS-DESPRDAB                             
                                                                        
           ADD 1                      TO   ACU-GRV-DESPRDAB.            
      *---------------------------------------------------------------* 
       5100-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       5200-GRAVAR-ATULZPF           SECTION.                           
      *---------------------------------------------------------------* 
           WRITE  FD-REG-ATULZPF       FROM    APF-REG                  
           MOVE   WRK-GRAVACAO         TO      WRK-OPERACAO             
           PERFORM  1170-TESTAR-FS-ATULZPF                              
                                                                        
           ADD 1                      TO   ACU-GRV-ATULZPF.             
      *---------------------------------------------------------------* 
       5200-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       7000-DISPLAY-TOTAIS        SECTION.                              
      *---------------------------------------------------------------* 
           MOVE     ACU-LDS-NOMECADU   TO    WRK-EDIT01                 
           MOVE     ACU-LDS-NOMERDAB   TO    WRK-EDIT02                 
           MOVE     ACU-BRANCOS-CADU   TO    WRK-EDIT03                 
           MOVE     ACU-NOMEIGUAL-CADU TO    WRK-EDIT04                 
           MOVE     ACU-GRV-CADURDAB   TO    WRK-EDIT05                 
           MOVE     ACU-GRV-DESPRDAB   TO    WRK-EDIT06                 
           MOVE     ACU-NAOACHOU-CADU  TO    WRK-EDIT07                 
                                                                        
           DISPLAY '******************** RDAB0273 ********************' 
           DISPLAY '*                                                *' 
           DISPLAY '*  TOTAL REG. LIDOS    - NOMECADU : ' WRK-EDIT01 '*'
           DISPLAY '*  TOTAL REG. LIDOS    - NOMERDAB : ' WRK-EDIT02 '*'
           DISPLAY '*  TOTAL REG. DESPREZ. NOME BRANCO: ' WRK-EDIT03 '*'
           DISPLAY '*  TOTAL REG. DESPREZ. NOME IGUAL : ' WRK-EDIT04 '*'
           DISPLAY '*  TOTAL REG. DESPR.NAO ACHOU CADU: ' WRK-EDIT07 '*'
           DISPLAY '*  TOTAL REG. GRAVADOS - CADURDAB : ' WRK-EDIT05 '*'
           DISPLAY '*  TOTAL REG. GRAVADOS - DESPRDAB : ' WRK-EDIT06 '*'
           DISPLAY '*                                                *' 
           DISPLAY '******************** RDAB0273 ********************'.
      *---------------------------------------------------------------* 
       7000-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       9000-FINALIZAR             SECTION.                              
      *---------------------------------------------------------------* 
           CLOSE  NOMECADU                                              
                  NOMERDAB                                              
                  CADURDAB                                              
                  DESPRDAB                                              
                  ATULZPF.                                              
                                                                        
           MOVE   WRK-FECHAMENTO       TO      WRK-OPERACAO             
           PERFORM  1100-TESTAR-FILE-STATUS.                            
                                                                        
           STOP RUN.                                                    
      *---------------------------------------------------------------* 
       9000-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *----------------------------------------------------------------*
       9999-ROTINA-ERRO SECTION.                                        
      *----------------------------------------------------------------*
                                                                        
           MOVE   'RDAB0273'           TO  ERR-PGM.                     
           MOVE   'APL'                TO ERR-TIPO-ACESSO.              
           CALL   'BRAD7100'        USING  WRK-BATCH                    
                                           ERRO-AREA.                   
           GOBACK.                                                      
      *----------------------------------------------------------------*
       9999-99-FIM. EXIT.                                               
      *----------------------------------------------------------------*

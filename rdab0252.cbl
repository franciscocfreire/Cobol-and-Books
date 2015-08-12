      *===============================================================* 
       IDENTIFICATION DIVISION.                                         
      *===============================================================* 
                                                                        
       PROGRAM-ID. RDAB0252.                                            
       AUTHOR. FRANCISCO.                                               
                                                                        
      *===============================================================* 
      *                 B R Q    I T   S E R V I C E S                * 
      *---------------------------------------------------------------* 
      *                                                               * 
      *   PROGRAMA    : RDAB0252                                      * 
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
      *   NOMECADU | I | DADOS PF CADU           | WORKING  |   100   * 
      *   NOMERDAB | I | DADOS PF RDAB           | WORKING  |   100   * 
      *   CADURDAB | O | DADOS PF CADU + RDAB    | WORKING  |   130   * 
      *   DESPRDAB | O | DESPR.PF CADU + RDAB    | WORKING  |   130   * 
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
                                                                        
           SELECT  NOMECADU  ASSIGN  TO  UT-S-NOMECADU                  
                   FILE      STATUS  IS  WRK-FS-NOMECADU.               
                                                                        
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
                                                                        
       FD  NOMECADU                                                     
           LABEL       RECORD    STANDARD                               
           RECORDING   MODE      F                                      
           BLOCK       CONTAINS  0.                                     
                                                                        
       01  FD-REG-NOMECADU.                                             
           03  FD-REG-CPF            PIC  9(009)         VALUE ZEROS.   
           03  FD-REG-FILIAL         PIC  9(005)         VALUE ZEROS.   
           03  FD-REG-CTRL           PIC  9(002)         VALUE ZEROS.   
           03  FD-REG-NOME-CADU      PIC  X(040)         VALUE SPACES.  
           03  FD-REG-HULT-ATULZ     PIC  X(026)         VALUE SPACES.  
           03  FD-REG-CLUB           PIC  9(010)         VALUE ZEROS.   
           03  FILLER                PIC  X(008)         VALUE SPACES.  
                                                                        
      *---------------------------------------------------------------* 
      *  INPUT....: ARQ. NOME PF RDABB008                             * 
      *             ORG. SEQUENCIAL    -  LRECL = 050 BYTES           * 
      *---------------------------------------------------------------* 
                                                                        
       FD  NOMERDAB                                                     
           LABEL       RECORD    STANDARD                               
           RECORDING   MODE      F                                      
           BLOCK       CONTAINS  0.                                     
                                                                        
       01  FD-REG-NOMERDAB.                                             
           03  FD-REG-CPF            PIC  9(009) COMP-3  VALUE ZEROS.   
           03  FD-REG-FILIAL         PIC  9(005) COMP-3  VALUE ZEROS.   
           03  FD-REG-CTRL           PIC  9(002) COMP-3  VALUE ZEROS.   
           03  FD-REG-NOME-RDAB      PIC  X(040)         VALUE SPACES.  
                                                                        
      *---------------------------------------------------------------* 
      *  OUTPUT...: ARQ. DE DADOS DA PESSOA                           * 
      *             ORG. SEQUENCIAL   -   LRECL = 130 BYTES           * 
      *---------------------------------------------------------------* 
                                                                        
       FD  CADURDAB                                                     
           LABEL       RECORD    STANDARD                               
           RECORDING   MODE      F                                      
           BLOCK       CONTAINS  0.                                     
                                                                        
       01  FD-REG-CADURDAB.                                             
           03  FD-REG-CPF            PIC  9(009)         VALUE ZEROS.   
           03  FD-REG-FILIAL         PIC  9(005)         VALUE ZEROS.   
           03  FD-REG-CTRL           PIC  9(002)         VALUE ZEROS.   
           03  FD-REG-NOME-CADU      PIC  X(040)         VALUE SPACES.  
           03  FD-REG-NOME-RDAB      PIC  X(040)         VALUE SPACES.  
           03  FD-REG-CLUB           PIC  9(010)         VALUE ZEROS.   
           03  FILLER                PIC  X(024)         VALUE SPACES.  
                                                                        
      *---------------------------------------------------------------* 
      *  OUTPUT...: ARQ. DE DADOS DO RDAB (DESPREZADOS)               * 
      *             ORG. SEQUENCIAL   -   LRECL = 130 BYTES           * 
      *---------------------------------------------------------------* 
                                                                        
       FD  DESPRDAB                                                     
           LABEL       RECORD    STANDARD                               
           RECORDING   MODE      F                                      
           BLOCK       CONTAINS  0.                                     
                                                                        
       01  FD-REG-DESPRDAB.                                             
           03  FD-REG-CPF            PIC  9(009)         VALUE ZEROS.   
           03  FD-REG-FILIAL         PIC  9(005)         VALUE ZEROS.   
           03  FD-REG-CTRL           PIC  9(002)         VALUE ZEROS.   
           03  FD-REG-NOME-CADU      PIC  X(040)         VALUE SPACES.  
           03  FD-REG-NOME-RDAB      PIC  X(040)         VALUE SPACES.  
           03  FD-REG-CLUB           PIC  9(010)         VALUE ZEROS.   
           03  FILLER                PIC  X(024)         VALUE SPACES.  
                                                                        
      *---------------------------------------------------------------* 
       WORKING-STORAGE SECTION.                                         
      *---------------------------------------------------------------* 
       01   FILLER                     PIC  X(32) VALUE                 
            'INICIO DA WORKING RDAB0252'.                               
                                                                        
      *-- ------------------------------------------------------------* 
      *          CAMPOS UTILIZADOS PARA CANCELAR PROCESSAMENTO        * 
      *---------------------------------------------------------------* 
                                                                        
       01  FILLER.                                                      
           03  WRK-FS-NOMECADU         PIC  X(02) VALUE SPACES.         
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
           03  ACU-LDS-NOMECADU        PIC 9(09) COMP-3    VALUE  ZEROS.
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
                                                                        
      *--  CHAVE NOMECADU.                                              
       01  WRK-REG-NOMECADU.                                            
           03  WRK-CHV-NOMECADU.                                        
               05  WRK-CHV-CADU-CPF      PIC  9(009) VALUE ZEROS.       
               05  WRK-CHV-CADU-FILIAL   PIC  9(005) VALUE ZEROS.       
           03  WRK-CHV-CADU-CTRL         PIC  9(002) VALUE ZEROS.       
           03  WRK-CHV-CADU-NOME         PIC  X(040) VALUE SPACES.      
                                                                        
       01  WRK-REG-NOMECADU-ANT.                                        
           03  WRK-CHV-NOMECADU-ANT.                                    
               05  WRK-CHV-CADU-CPF2     PIC  9(009) VALUE ZEROS.       
               05  WRK-CHV-CADU-FILIAL2  PIC  9(005) VALUE ZEROS.       
           03  WRK-CHV-CADU-CTRL2        PIC  9(002) VALUE ZEROS.       
           03  WRK-CHV-CADU-NOME         PIC  X(040) VALUE SPACES.      
                                                                        
      *--  CHAVE NOMERDAB.                                              
       01  WRK-CHV-NOMERDAB.                                            
           03  WRK-CHV-RDAB-CPF      PIC  9(009) VALUE ZEROS.           
           03  WRK-CHV-RDAB-FILIAL   PIC  9(005) VALUE ZEROS.           
                                                                        
      *----------------------------------------------------------------*
       01  FILLER   PIC  X(32) VALUE '*       AREA DA BRAD7100       *'.
      *----------------------------------------------------------------*
                                                                        
       COPY 'I#BRAD7C'.                                                 
                                                                        
      *---------------------------------------------------------------* 
       01   FILLER                     PIC  X(32)    VALUE              
            'FIM DA WORKING RDAB0252'.                                  
                                                                        
      *===============================================================* 
       PROCEDURE DIVISION.                                              
      *===============================================================* 
                                                                        
      *---------------------------------------------------------------* 
       0000-INICIAR SECTION.                                            
      *---------------------------------------------------------------* 
                                                                        
           PERFORM 1000-INICIALIZAR.                                    
                                                                        
           PERFORM 2000-LER-NOMECADU                                    
           IF      WRK-FS-NOMECADU     EQUAL '10'                       
                   DISPLAY '**************** RDAB0252 ***************'  
                   DISPLAY '*                                       *'  
                   DISPLAY '* ARQUIVO DE ENTRADA - NOMECADU - VAZIO *'  
                   DISPLAY '*        PROCESSAMENTO ENCERRADO        *'  
                   DISPLAY '*                                       *'  
                   DISPLAY '**************** RDAB0252 ***************'  
                   PERFORM  9000-FINALIZAR                              
           ELSE                                                         
               MOVE  WRK-REG-NOMECADU      TO  WRK-REG-NOMECADU-ANT     
           END-IF.                                                      
                                                                        
           PERFORM 3000-LER-NOMERDAB                                    
           IF      WRK-FS-NOMERDAB     EQUAL '10'                       
                   DISPLAY '**************** RDAB0252 ***************'  
                   DISPLAY '*                                       *'  
                   DISPLAY '* ARQUIVO DE ENTRADA - NOMERDAB - VAZIO *'  
                   DISPLAY '*        PROCESSAMENTO ENCERRADO        *'  
                   DISPLAY '*                                       *'  
                   DISPLAY '**************** RDAB0252 ***************'  
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
                                                                        
      *---------------------------------------------------------------* 
       1100-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1110-TESTAR-FS-NOMECADU       SECTION.                           
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-FS-NOMECADU         NOT EQUAL  '00'                   
              DISPLAY '************** RDAB0252 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              NOMECADU             *'           
              DISPLAY '*         FILE STATUS =  ' WRK-FS-NOMECADU       
                                                 '         *'           
              DISPLAY '************** RDAB0252 *************'           
              PERFORM 9999-ROTINA-ERRO                                  
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       1110-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1120-TESTAR-FS-NOMERDAB       SECTION.                           
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-FS-NOMERDAB         NOT EQUAL  '00'                   
              DISPLAY '************** RDAB0252 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              NOMERDAB             *'           
              DISPLAY '*         FILE STATUS =  ' WRK-FS-NOMERDAB       
                                                 '         *'           
              DISPLAY '************** RDAB0252 *************'           
              PERFORM 9999-ROTINA-ERRO                                  
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       1120-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1150-TESTAR-FS-CADURDAB        SECTION.                          
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-FS-CADURDAB           NOT EQUAL  '00'                 
              DISPLAY '************** RDAB0252 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              CADURDAB              *'          
              DISPLAY '*         FILE STATUS =  ' WRK-FS-CADURDAB       
                                                 '         *'           
              DISPLAY '************** RDAB0252 *************'           
              PERFORM 9999-ROTINA-ERRO                                  
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       1150-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1160-TESTAR-FS-DESPRDAB        SECTION.                          
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-FS-DESPRDAB           NOT EQUAL  '00'                 
              DISPLAY '************** RDAB0252 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              DESPRDAB              *'          
              DISPLAY '*         FILE STATUS =  ' WRK-FS-DESPRDAB       
                                                 '         *'           
              DISPLAY '************** RDAB0252 *************'           
              PERFORM 9999-ROTINA-ERRO                                  
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       1160-99-FIM.   EXIT.                                             
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
                                                                        
           MOVE  FD-REG-NOMECADU(1:56) TO  WRK-REG-NOMECADU.            
                                                                        
           ADD     1                   TO  ACU-LDS-NOMECADU.            
                                                                        
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
                                                                        
           MOVE FD-REG-CPF             OF  NOMERDAB                     
                                       TO  WRK-CHV-RDAB-CPF             
           MOVE FD-REG-FILIAL          OF  NOMERDAB                     
                                       TO  WRK-CHV-RDAB-FILIAL          
                                                                        
           ADD     1                   TO      ACU-LDS-NOMERDAB.        
                                                                        
      *---------------------------------------------------------------* 
       3000-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       4000-PROCESSAR               SECTION.                            
      *---------------------------------------------------------------* 
                                                                        
           IF    WRK-CHV-NOMECADU   GREATER   WRK-CHV-NOMERDAB          
                 INITIALIZE FD-REG-DESPRDAB                             
                                                                        
                 ADD   1            TO  ACU-NAOACHOU-CADU               
                 PERFORM 4310-MOVER-DESPRDAB                            
                 PERFORM 5100-GRAVAR-DESPRDAB                           
                 PERFORM 3000-LER-NOMERDAB                              
           ELSE                                                         
              IF   WRK-CHV-NOMECADU   LESS    WRK-CHV-NOMERDAB          
                   PERFORM 2000-LER-NOMECADU                            
              ELSE                                                      
                   INITIALIZE FD-REG-DESPRDAB                           
                                                                        
                   MOVE  FD-REG-NOME-CADU OF  NOMECADU TO               
                         FD-REG-NOME-CADU OF  DESPRDAB                  
                   MOVE  FD-REG-CLUB      OF  NOMECADU TO               
                         FD-REG-CLUB      OF  DESPRDAB                  
                                                                        
                   IF (FD-REG-NOME-CADU  OF NOMECADU                    
                                         EQUAL SPACES OR LOW-VALUES)    
                      ADD  1             TO  ACU-BRANCOS-CADU           
                                                                        
                      PERFORM 4310-MOVER-DESPRDAB                       
                      PERFORM 5100-GRAVAR-DESPRDAB                      
                   ELSE                                                 
                      IF (FD-REG-NOME-CADU   OF NOMECADU EQUAL          
                          FD-REG-NOME-RDAB   OF NOMERDAB)               
                          ADD  1             TO  ACU-NOMEIGUAL-CADU     
                          PERFORM 4310-MOVER-DESPRDAB                   
                          PERFORM 5100-GRAVAR-DESPRDAB                  
                      ELSE                                              
                          PERFORM 4200-CHAVES-IGUAIS                    
                      END-IF                                            
                   END-IF                                               
                                                                        
                   MOVE  WRK-REG-NOMECADU TO  WRK-REG-NOMECADU-ANT      
                                                                        
      ***   OBTER SOMENTE O PRIMEIRO CPF (TIMESTAMP MAIOR) E            
      ***   DESPREZAR O RESTO ATE QUEBRA DE CHAVE CPF                   
                   PERFORM 3000-LER-NOMERDAB                            
                   PERFORM 2000-LER-NOMECADU                            
                           UNTIL  WRK-CHV-NOMECADU NOT EQUAL            
                                  WRK-CHV-NOMECADU-ANT                  
              END-IF                                                    
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       4000-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       4200-CHAVES-IGUAIS           SECTION.                            
      *---------------------------------------------------------------* 
                                                                        
           INITIALIZE FD-REG-CADURDAB.                                  
                                                                        
           MOVE  FD-REG-NOME-CADU       OF  NOMECADU TO                 
                 FD-REG-NOME-CADU       OF  CADURDAB.                   
           MOVE  FD-REG-CLUB            OF  NOMECADU TO                 
                 FD-REG-CLUB            OF  CADURDAB.                   
                                                                        
           PERFORM 4300-MOVER-NOMERDAB.                                 
                                                                        
           PERFORM 5000-GRAVAR-CADURDAB.                                
                                                                        
      *---------------------------------------------------------------* 
       4200-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       4300-MOVER-NOMERDAB           SECTION.                           
      *---------------------------------------------------------------* 
                                                                        
           MOVE  FD-REG-CPF             OF  NOMERDAB TO                 
                 FD-REG-CPF             OF  CADURDAB                    
           MOVE  FD-REG-FILIAL          OF  NOMERDAB TO                 
                 FD-REG-FILIAL          OF  CADURDAB                    
           MOVE  FD-REG-CTRL            OF  NOMERDAB TO                 
                 FD-REG-CTRL            OF  CADURDAB                    
           MOVE  FD-REG-NOME-RDAB       OF  NOMERDAB TO                 
                 FD-REG-NOME-RDAB       OF  CADURDAB.                   
                                                                        
      *---------------------------------------------------------------* 
       4300-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       4310-MOVER-DESPRDAB           SECTION.                           
      *---------------------------------------------------------------* 
                                                                        
           MOVE  FD-REG-CPF             OF  NOMERDAB TO                 
                 FD-REG-CPF             OF  DESPRDAB                    
           MOVE  FD-REG-FILIAL          OF  NOMERDAB TO                 
                 FD-REG-FILIAL          OF  DESPRDAB                    
           MOVE  FD-REG-CTRL            OF  NOMERDAB TO                 
                 FD-REG-CTRL            OF  DESPRDAB                    
           MOVE  FD-REG-NOME-RDAB       OF  NOMERDAB TO                 
                 FD-REG-NOME-RDAB       OF  DESPRDAB.                   
                                                                        
      *---------------------------------------------------------------* 
       4310-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       5000-GRAVAR-CADURDAB          SECTION.                           
      *---------------------------------------------------------------* 
                                                                        
           WRITE  FD-REG-CADURDAB                                       
           MOVE   WRK-GRAVACAO         TO      WRK-OPERACAO             
           PERFORM  1150-TESTAR-FS-CADURDAB                             
                                                                        
           ADD 1                      TO   ACU-GRV-CADURDAB.            
                                                                        
      *---------------------------------------------------------------* 
       5000-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       5100-GRAVAR-DESPRDAB          SECTION.                           
      *---------------------------------------------------------------* 
                                                                        
           WRITE  FD-REG-DESPRDAB                                       
           MOVE   WRK-GRAVACAO         TO      WRK-OPERACAO             
           PERFORM  1160-TESTAR-FS-DESPRDAB                             
                                                                        
           ADD 1                      TO   ACU-GRV-DESPRDAB.            
                                                                        
      *---------------------------------------------------------------* 
       5100-99-FIM.   EXIT.                                             
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
                                                                        
           DISPLAY '******************** RDAB0252 ********************' 
           DISPLAY '*                                                *' 
           DISPLAY '*  TOTAL REG. LIDOS    - NOMECADU : 'WRK-EDIT01'  *'
           DISPLAY '*  TOTAL REG. LIDOS    - NOMERDAB : 'WRK-EDIT02'  *'
           DISPLAY '*  TOTAL REG. DESPREZ. NOME BRANCO: 'WRK-EDIT03'  *'
           DISPLAY '*  TOTAL REG. DESPREZ. NOME IGUAL : 'WRK-EDIT04'  *'
           DISPLAY '*  TOTAL REG. DESPR.NAO ACHOU CADU: 'WRK-EDIT07'  *'
           DISPLAY '*  TOTAL REG. GRAVADOS - CADURDAB : 'WRK-EDIT05'  *'
           DISPLAY '*  TOTAL REG. GRAVADOS - DESPRDAB : 'WRK-EDIT06'  *'
           DISPLAY '*                                                *' 
           DISPLAY '******************** RDAB0252 ********************'.
                                                                        
      *---------------------------------------------------------------* 
       7000-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       9000-FINALIZAR             SECTION.                              
      *---------------------------------------------------------------* 
                                                                        
           CLOSE  NOMECADU                                              
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
                                                                        
           MOVE   'RDAB0252'           TO  ERR-PGM.                     
           MOVE   'APL'                TO ERR-TIPO-ACESSO.              
           CALL   'BRAD7100'        USING  WRK-BATCH                    
                                           ERRO-AREA.                   
                                                                        
           GOBACK.                                                      
                                                                        
      *----------------------------------------------------------------*
       9999-99-FIM. EXIT.                                               
      *----------------------------------------------------------------*
                                                                        

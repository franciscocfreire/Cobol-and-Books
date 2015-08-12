      *===============================================================* 
       IDENTIFICATION DIVISION.                                         
      *===============================================================* 
                                                                        
       PROGRAM-ID. RDAB0860.                                            
       AUTHOR. FRANCISCO.                                               
                                                                        
      *===============================================================* 
      *                 B R Q    I T   S E R V I C E S                * 
      *---------------------------------------------------------------* 
      *                                                               * 
      *   PROGRAMA    : RDAB0860                                      * 
      *   PROGRAMADOR : FRANCISCO FREIRE       - BRQ IT SERVICES      * 
      *   ANALISTA    : FRANCISCO FREIRE       - BRQ IT SERVICES      * 
      *   SUPERVISOR  : JUNIOR RIBAMAR         - GP. 70               * 
      *   DATA        : ABRIL/2014                                    * 
      *                                                               * 
      *   OBJETIVO    : BALANCE LINE DE DADOS PRINCIPAIS DO CADU COM  * 
      *                 INFORMACOES DE DADOS BASICOS                  *
      *   ARQUIVOS:                                                   * 
      *   ---------------------------------------------------------   * 
      *   DDNAME   |I/O| DESCRICAO               | BOOK     | LRECL   * 
      *   ---------+---+-------------------------+----------+------   * 
      *   CADUB0A4 | I | DADOS PRINCIPAIS        | PSDCW033 |   642   *
      *   CADUB003 | I | DADOS BASICOS           | PSDCW033 |   642   *
      *   ARQSAIDA | O | DADOS PRINCIPAIS        | I#RDABAP |   165   * 
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
                                                                        
           SELECT  CADUB0A4  ASSIGN  TO  UT-S-CADUB0A4                  
                   FILE      STATUS  IS  WRK-FS-CADUB0A4.               
                                                                        
           SELECT  CADUB003  ASSIGN  TO  UT-S-CADUB003                  
                   FILE      STATUS  IS  WRK-FS-CADUB003.               
                                                                        
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
      *  INPUT....: ARQ DADOS BASICOS                                 * 
      *             ORG. SEQUENCIAL    -  LRECL = 642 BYTES           * 
      *---------------------------------------------------------------* 
                                                                        
       FD  CADUB003                                                     
           LABEL       RECORD    STANDARD                               
           RECORDING   MODE      F                                      
           BLOCK       CONTAINS  0.                                     
                                                                        
       COPY 'PSDCW033'.
      *---------------------------------------------------------------* 
      *  OUTPUT...: ARQ. DE DADOS DA PESSOA                           * 
      *             ORG. SEQUENCIAL   -   LRECL = 165 BYTES           * 
      *---------------------------------------------------------------* 
                                                                        
       FD  ARQSAIDA                                                     
           LABEL       RECORD    STANDARD                               
           RECORDING   MODE      F                                      
           BLOCK       CONTAINS  0.                                     
                                                                        
                                                                        
       COPY 'I#RDABAQ'.                                                 
                                                                        
      *---------------------------------------------------------------* 
       WORKING-STORAGE SECTION.                                         
      *---------------------------------------------------------------* 
       01   FILLER                     PIC  X(32) VALUE                 
            'INICIO DA WORKING RDAB0860'.                               
                                                                        
      *-- ------------------------------------------------------------* 
      *          CAMPOS UTILIZADOS PARA CANCELAR PROCESSAMENTO        * 
      *---------------------------------------------------------------* 
                                                                        
       01  FILLER.                                                      
           03  WRK-FS-CADUB0A4         PIC  X(02) VALUE SPACES.         
           03  WRK-FS-CADUB003         PIC  X(02) VALUE SPACES.         
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
           03  ACU-LDS-CADUB003        PIC 9(09) COMP-3    VALUE  ZEROS.
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
                                                                        
       01  WRK-AUX-NUM005              PIC 9(05) VALUE  ZEROS.          
       01  WRK-AUX-NUM005-S            REDEFINES                        
           WRK-AUX-NUM005              PIC S9(05).                      
                                                                        
       01  WRK-AUX-NUM013              PIC 9(13) VALUE  ZEROS.          
       01  WRK-AUX-NUM013-S            REDEFINES                        
           WRK-AUX-NUM013              PIC S9(13).                      
                                                                        
       01  WRK-AUX-NUM003              PIC 9(03) VALUE  ZEROS.          
       01  WRK-AUX-NUM003-S            REDEFINES                        
           WRK-AUX-NUM003              PIC S9(03).                      
                                                                        
       01  WRK-AUX-NUM004              PIC 9(04) VALUE  ZEROS.          
       01  WRK-AUX-NUM004-S            REDEFINES                        
           WRK-AUX-NUM004              PIC S9(04).                      
                                                                        
      *--  CHAVE CADUB0A4.                                              
       01  WRK-CHV-CADUB0A4.                                            
           03  WRK-CHV-0A4-CCLUB       PIC 9(10) VALUE  ZEROS.          
                                                                        
      *--  CHAVE CADUB003.                                              
       01  WRK-CHV-CADUB003.                                            
           03  WRK-CHV-018-CCLUB       PIC 9(10) VALUE  ZEROS.          
                                                                        
      *----------------------------------------------------------------*
       01  FILLER   PIC  X(32) VALUE '*       AREA DA BRAD7100       *'.
      *----------------------------------------------------------------*
                                                                        
       COPY 'I#BRAD7C'.                                                 
                                                                        
      *---------------------------------------------------------------* 
       01   FILLER                     PIC  X(32)    VALUE              
            'FIM DA WORKING RDAB0860'.                                  
                                                                        
      *===============================================================* 
       PROCEDURE DIVISION.                                              
      *===============================================================* 
                                                                        
      *---------------------------------------------------------------* 
       0000-INICIAR SECTION.                                            
      *---------------------------------------------------------------* 
                                                                        
           PERFORM 1000-INICIALIZAR.                                    
                                                                        
           PERFORM 2000-LER-CADUB0A4                                    
           IF      WRK-FS-CADUB0A4     EQUAL '10'                       
                   DISPLAY '**************** RDAB0860 ***************'  
                   DISPLAY '*                                       *'  
                   DISPLAY '* ARQUIVO DE ENTRADA - CADUB0A4 - VAZIO *'  
                   DISPLAY '*        PROCESSAMENTO ENCERRADO        *'  
                   DISPLAY '*                                       *'  
                   DISPLAY '**************** RDAB0860 ***************'  
                   PERFORM  9000-FINALIZAR                              
           END-IF.                                                      
                                                                        
           PERFORM 3000-LER-CADUB003                                    
           IF      WRK-FS-CADUB003     EQUAL '10'                       
                   DISPLAY '**************** RDAB0860 ***************'  
                   DISPLAY '*                                       *'  
                   DISPLAY '* ARQUIVO DE ENTRADA - CADUB003 - VAZIO *'  
                   DISPLAY '*        PROCESSAMENTO ENCERRADO        *'  
                   DISPLAY '*                                       *'  
                   DISPLAY '**************** RDAB0860 ***************'  
           END-IF                                                       
                                                                        
           PERFORM 4000-PROCESSAR     UNTIL                             
                  (WRK-FS-CADUB0A4     EQUAL '10')                      
                                                                        
                                                                        
           PERFORM 7000-DISPLAY-TOTAIS                                  
                                                                        
           PERFORM 9000-FINALIZAR.                                      
                                                                        
      *---------------------------------------------------------------* 
       0000-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1000-INICIALIZAR              SECTION.                           
      *---------------------------------------------------------------* 
                                                                        
           OPEN  INPUT  CADUB0A4                                        
                        CADUB003                                        
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
           PERFORM  1120-TESTAR-FS-CADUB003                             
           PERFORM  1150-TESTAR-FS-ARQSAIDA.                            
                                                                        
      *---------------------------------------------------------------* 
       1100-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1110-TESTAR-FS-CADUB0A4       SECTION.                           
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-FS-CADUB0A4         NOT EQUAL  '00'                   
              DISPLAY '************** RDAB0860 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              CADUB0A4             *'           
              DISPLAY '*         FILE STATUS =  ' WRK-FS-CADUB0A4       
                                                 '         *'           
              DISPLAY '************** RDAB0860 *************'           
              PERFORM 9999-ROTINA-ERRO                                  
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       1110-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1120-TESTAR-FS-CADUB003       SECTION.                           
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-FS-CADUB003         NOT EQUAL  '00'                   
              DISPLAY '************** RDAB0860 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              CADUB003             *'           
              DISPLAY '*         FILE STATUS =  ' WRK-FS-CADUB003       
                                                 '         *'           
              DISPLAY '************** RDAB0860 *************'           
              PERFORM 9999-ROTINA-ERRO                                  
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       1120-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1150-TESTAR-FS-ARQSAIDA        SECTION.                          
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-FS-ARQSAIDA           NOT EQUAL  '00'                 
              DISPLAY '************** RDAB0860 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              ARQSAIDA              *'          
              DISPLAY '*         FILE STATUS =  ' WRK-FS-ARQSAIDA       
                                                 '         *'           
              DISPLAY '************** RDAB0860 *************'           
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
                                                                        
           MOVE PSDCW033-01-CCLUB OF CADUB0A4
                                       TO  WRK-AUX-NUM010-S.            
           MOVE WRK-AUX-NUM010         TO  WRK-CHV-0A4-CCLUB.           
                                                                        
           ADD     1                      TO  ACU-LDS-CADUB0A4.         
                                                                        
      *---------------------------------------------------------------* 
       2000-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       3000-LER-CADUB003            SECTION.                            
      *---------------------------------------------------------------* 
                                                                        
           READ    CADUB003                                             
                                                                        
           IF      WRK-FS-CADUB003     EQUAL  '10'                      
                   MOVE HIGH-VALUES    TO      WRK-CHV-CADUB003         
                   GO                  TO      3000-99-FIM              
           END-IF.                                                      
                                                                        
           MOVE    WRK-LEITURA         TO      WRK-OPERACAO             
           PERFORM 1120-TESTAR-FS-CADUB003                              
                                                                        
           MOVE PSDCW033-16-CCLUB OF CADUB003
                                       TO  WRK-AUX-NUM010-S             
           MOVE WRK-AUX-NUM010         TO  WRK-CHV-018-CCLUB.           
                                                                        
           ADD     1                   TO      ACU-LDS-CADUB003.        
                                                                        
      *---------------------------------------------------------------* 
       3000-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       4000-PROCESSAR               SECTION.                            
      *---------------------------------------------------------------* 
                                                                        
           IF      WRK-CHV-CADUB0A4   GREATER   WRK-CHV-CADUB003        
                   PERFORM 3000-LER-CADUB003                            
           ELSE                                                         
              IF      WRK-CHV-CADUB0A4    LESS    WRK-CHV-CADUB003
                      PERFORM 4300-CHAVES-MENOR
                      PERFORM 2000-LER-CADUB0A4
              ELSE
                      PERFORM 4200-CHAVES-IGUAIS                        
                      PERFORM 2000-LER-CADUB0A4                         
                      PERFORM 3000-LER-CADUB003                         
              END-IF                                                    
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       4000-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 

      *---------------------------------------------------------------*
       4100-MOVE-CADUB0A4               SECTION.
      *---------------------------------------------------------------*

           MOVE PSDCW033-01-CCLUB OF CADUB0A4 TO WRK-AUX-NUM010-S
           MOVE WRK-AUX-NUM010             TO CADUV002-CCLUB

           MOVE PSDCW033-01-CID-TPO-PSSOA OF CADUB0A4
                                               TO CADUV002-CID-TPO-PSSOA

           MOVE PSDCW033-01-CPF-CNPJ-NRO OF CADUB0A4
                                       TO WRK-AUX-CGCNUM-S
           MOVE  WRK-AUX-CGCNUM        TO CADUV002-CCPF-CNPJ.

           MOVE PSDCW033-01-CPF-CNPJ-FIL OF CADUB0A4
                                           TO WRK-AUX-NUM004-S
           MOVE WRK-AUX-NUM004             TO CADUV002-CFLIAL-CPF-CNPJ
           MOVE PSDCW033-01-CPF-CNPJ-CTR OF CADUB0A4
                                       TO WRK-AUX-CGCCTR-S
           MOVE  WRK-AUX-CGCCTR        TO CADUV002-CCTRL-CPF-CNPJ.

           MOVE PSDCW033-01-DNASC-FUNDC OF CADUB0A4
                                           TO CADUV002-DNASC-FUNDC

           MOVE PSDCW033-01-CAG-COBR-TARIF OF CADUB0A4
                                           TO WRK-AUX-NUM005-S
           MOVE WRK-AUX-NUM005             TO CADUV002-CAG-COBR-TARIF

           MOVE PSDCW033-01-CCTA-COBR-TARIF OF CADUB0A4
                                           TO WRK-AUX-NUM013-S
           MOVE WRK-AUX-NUM013             TO CADUV002-CCTA-COBR-TARIF.
      ********************* QUAL CAMPO SERA PREENCHIDO****************
      *******VE TO CADUV002-HULT-ATULZ
      ****************************************************************
      *---------------------------------------------------------------*
       4100-99-FIM.   EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------* 
       4200-CHAVES-IGUAIS           SECTION.                            
      *---------------------------------------------------------------* 
                                                                        
           INITIALIZE REG-CADUV002.                                     

           PERFORM 4100-MOVE-CADUB0A4.
                                                                        
           MOVE PSDCW033-16-IFANTS-EMPR-COPLT OF CADUB003
                                           TO CADUV002-IFANTS-EMPR-COPLT
                                                                        
           PERFORM 5000-GRAVAR-ARQSAIDA.                                
                                                                        
      *---------------------------------------------------------------* 
       4200-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 

      *---------------------------------------------------------------*
       4300-CHAVES-MENOR           SECTION.
      *---------------------------------------------------------------*

           INITIALIZE REG-CADUV002.

           PERFORM 4100-MOVE-CADUB0A4.

           PERFORM 5000-GRAVAR-ARQSAIDA.

      *---------------------------------------------------------------*
       4300-99-FIM.   EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------* 
       5000-GRAVAR-ARQSAIDA          SECTION.                           
      *---------------------------------------------------------------* 
                                                                        
           WRITE  REG-CADUV002                                          
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
           MOVE     ACU-LDS-CADUB003   TO    WRK-EDIT02                 
           MOVE     ACU-GRV-ARQSAIDA   TO    WRK-EDIT05                 
                                                                        
           DISPLAY '******************** RDAB0860 ********************' 
           DISPLAY '*                                                *' 
           DISPLAY '*  TOTAL REG. LIDOS    - CADUB0A4 : 'WRK-EDIT01'  *'
           DISPLAY '*  TOTAL REG. LIDOS    - CADUB003 : 'WRK-EDIT02'  *'
           DISPLAY '*  TOTAL REG. GRAVADOS - ARQSAIDA : 'WRK-EDIT05'  *'
           DISPLAY '*                                                *' 
           DISPLAY '******************** RDAB0860 ********************'.
                                                                        
      *---------------------------------------------------------------* 
       7000-99-FIM.   EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       9000-FINALIZAR             SECTION.                              
      *---------------------------------------------------------------* 
                                                                        
           CLOSE  CADUB0A4                                              
                  CADUB003                                              
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
                                                                        
           MOVE   'RDAB0860'           TO  ERR-PGM.                     
           MOVE   'APL'                TO ERR-TIPO-ACESSO.              
           CALL   'BRAD7100'        USING  WRK-BATCH                    
                                           ERRO-AREA.                   
                                                                        
           GOBACK.                                                      
                                                                        
      *----------------------------------------------------------------*
       9999-99-FIM. EXIT.                                               
      *----------------------------------------------------------------*
                                                                        

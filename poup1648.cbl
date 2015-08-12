      *================================================================*
       IDENTIFICATION DIVISION.                                         
      *================================================================*
                                                                        
       PROGRAM-ID.      POUP1648.                                       
       AUTHOR.          FJ BUENO.                                       
      *================================================================*
      *                 STEFANINI  CONSULTORIA                         *
      *================================================================*
      *                                                                *
      *   PROGRAMADOR : LUCAS MONTEIRO            - BRQ                *
      *   ANALISTA    : ANGELO ALEXANDRE FOLTRAN  - BRQ                *
      *   DATA        : 08/2006                                        *
      *                                                                *
      *   OBJETIVO    : GERAR ARQUIVO DE EVENTO APROPRIACAO CCTA POUP. *
      *                                                                *
      *----------------------------------------------------------------*
      *                                                                *
      *   ARQUIVOS....:                                                *
      *                                                                *
      *   INPUT                                                        *
      *   -----                                                        *
      *                                                                *
      *   DDNAME        INCLUDE/BOOK                                   *
      *                                                                *
      *   ARQENT1     - I#POUP32                                       *
      *   ARQENT2     - I#POUP49                                       *
      *                                                                *
      *   OUTPUT                                                       *
      *   ------                                                       *
      *                                                                *
      *   ARQSAID     - I#POUP49                                       *
      *                                                                *
      *----------------------------------------------------------------*
      *                                                                *
      *   MODULOS CHAMADOS:                                            *
      *                                                                *
      *   BRAD7100 - TRATAMENTO DE ERRO DB2.                           *
      *   BRAD7600 - OBTEM DATA E HORA DO SISTEMA.                     *
      *                                                                *
      *================================================================*
                                                                        
      *================================================================*
       ENVIRONMENT DIVISION.                                            
      *================================================================*
                                                                        
      *----------------------------------------------------------------*
       CONFIGURATION SECTION.                                           
      *----------------------------------------------------------------*
                                                                        
       SPECIAL-NAMES.                                                   
           DECIMAL-POINT IS COMMA.                                      
                                                                        
      *----------------------------------------------------------------*
       INPUT-OUTPUT SECTION.                                            
      *----------------------------------------------------------------*
                                                                        
       FILE-CONTROL.                                                    
                                                                        
           SELECT  ARQENT1   ASSIGN  TO  UT-S-ARQENT1                   
                   FILE      STATUS  IS  WRK-FS-ARQENT1.                
                                                                        
           SELECT  ARQENT2   ASSIGN  TO  UT-S-ARQENT2                   
                   FILE      STATUS  IS  WRK-FS-ARQENT2.                
                                                                        
           SELECT  ARQSAID   ASSIGN  TO  UT-S-ARQSAID                   
                   FILE      STATUS  IS  WRK-FS-ARQSAID.                
                                                                        
      *================================================================*
       DATA DIVISION.                                                   
      *================================================================*
                                                                        
      *----------------------------------------------------------------*
       FILE SECTION.                                                    
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *  INPUT....: LAYOUT DO ARQUIVO ARQENT1 -                        *
      *             ORG. SEQUENCIAL    -  LRECL = 007 BYTES            *
      *----------------------------------------------------------------*
                                                                        
       FD  ARQENT1                                                      
           LABEL       RECORD    STANDARD                               
           RECORDING   MODE      F                                      
           BLOCK       CONTAINS  0.                                     
                                                                        
       COPY 'I#POUP32'.                                                 
                                                                        
      *----------------------------------------------------------------*
      *  INPUT....: LAYOUT DO ARQUIVO ARQENT2 -                        *
      *             ORG. SEQUENCIAL    -  LRECL = 168 BYTES            *
      *----------------------------------------------------------------*
                                                                        
       FD  ARQENT2                                                      
           LABEL       RECORD    STANDARD                               
           RECORDING   MODE      V                                      
           BLOCK       CONTAINS  0.                                     
                                                                        
       COPY 'I#POUP49'.                                                 
                                                                        
      *----------------------------------------------------------------*
      *  OUTPUT...: LAYOUT DO ARQSAID - ARQSAIDRIO ANALITICO, ABERTURA *
      *             DE CONTA CC/POUP.                                  *
      *             ORG. SEQUENCIAL    -  LRECL = 133 BYTES            *
      *----------------------------------------------------------------*
                                                                        
       FD  ARQSAID                                                      
           LABEL       RECORD    STANDARD                               
           RECORDING   MODE      V                                      
           BLOCK       CONTAINS  0.                                     
                                                                        
       COPY 'I#POUP49'.                                                 
                                                                        
      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.                                         
      *----------------------------------------------------------------*
                                                                        
       77  FILLER                      PIC  X(50) VALUE                 
           'INICIO DA WORKING STORAGE POUP1648'.                        
                                                                        
      *----------------------------------------------------------------*
                                                                        
       01  FILLER                      PIC  X(50) VALUE                 
           'AREAS AUXILIARES'.                                          
                                                                        
      *----------------------------------------------------------------*
                                                                        
       01  FILLER                      PIC  X(50) VALUE                 
           'AREAS ACUMULADORES'.                                        
                                                                        
       01  FILLER.                                                      
           05  ACU-LIDOS-ARQENT1         PIC  9(09) COMP-3 VALUE ZEROS. 
           05  ACU-LIDOS-ARQENT2         PIC  9(09) COMP-3 VALUE ZEROS. 
           05  ACU-GRAVA-ARQSAID         PIC  9(09) COMP-3 VALUE ZEROS. 
           05  ACU-DESPZ-ARQENT1-AGE     PIC  9(09) COMP-3 VALUE ZEROS. 
           05  ACU-DESPZ-ARQENT1-AGE-CTA PIC  9(09) COMP-3 VALUE ZEROS. 
                                                                        
      *----------------------------------------------------------------*
                                                                        
       01  FILLER                      PIC  X(50) VALUE                 
           'AREAS EDITADAS UTILIZADAS NO DISPLAY'.                      
                                                                        
       01  FILLER.                                                      
           05  WRK-EDIT1               PIC ZZZ.ZZZ.ZZ9 VALUE ZEROS.     
           05  WRK-EDIT2               PIC ZZZ.ZZZ.ZZ9 VALUE ZEROS.     
           05  WRK-EDIT3               PIC ZZZ.ZZZ.ZZ9 VALUE ZEROS.     
           05  WRK-EDIT4               PIC ZZZ.ZZZ.ZZ9 VALUE ZEROS.     
           05  WRK-EDIT5               PIC ZZZ.ZZZ.ZZ9 VALUE ZEROS.     
                                                                        
      *----------------------------------------------------------------*
                                                                        
       01  FILLER                      PIC  X(50) VALUE                 
           'AREAS PARA TESTE DE FILE-STATUS'.                           
                                                                        
       01  FILLER.                                                      
           05  WRK-FS-ARQENT1          PIC  X(02) VALUE SPACES.         
           05  WRK-FS-ARQENT2          PIC  X(02) VALUE SPACES.         
           05  WRK-FS-ARQSAID          PIC  X(02) VALUE SPACES.         
           05  WRK-ABERTURA            PIC  X(13) VALUE 'NA ABERTURA'.  
           05  WRK-LEITURA             PIC  X(13) VALUE 'NA LEITURA'.   
           05  WRK-GRAVACAO            PIC  X(13) VALUE 'NA GRAVACAO'.  
           05  WRK-FECHAMENTO          PIC  X(13) VALUE 'NO FECHAMENTO'.
           05  WRK-BATCH               PIC  X(08) VALUE 'BATCH'.        
                                                                        
       01  WRK-MENSAGEM-ERRO.                                           
           05  FILLER                  PIC  X(09) VALUE '*** ERRO '.    
           05  WRK-OPERACAO            PIC  X(13) VALUE SPACES.         
           05  FILLER                  PIC  X(12) VALUE ' DO ARQUIVO '. 
           05  WRK-NOME-ARQUIVO        PIC  X(08) VALUE SPACES.         
           05  FILLER                  PIC  X(17) VALUE                 
               ' - FILE STATUS = '.                                     
           05  WRK-FS                  PIC  X(02) VALUE SPACES.         
           05  FILLER                  PIC  X(04) VALUE ' ***'.         
           05  FILLER                  PIC  X(10) VALUE SPACES.         
                                                                        
       COPY 'I#BRAD7C'.                                                 
                                                                        
       01  WRK-TIPO-ACESSO             PIC  X(03) VALUE SPACES.         
                                                                        
       01  WRK-ERR-TEXTO               PIC  X(75) VALUE SPACES.         
                                                                        
      *----------------------------------------------------------------*
                                                                        
       01  FILLER                      PIC  X(50) VALUE                 
           'AREAS PARA OBTER DATA E HORA DO SISTEMA'.                   
                                                                        
       01  WRK-DATA-HORA.                                               
           05  WRK-DT-JULIANA          PIC  9(05) COMP-3 VALUE ZEROS.   
           05  WRK-DT-AAMMDD           PIC  9(07) COMP-3 VALUE ZEROS.   
           05  WRK-DT-AAAAMMDD         PIC  9(09) COMP-3 VALUE ZEROS.   
           05  WRK-TI-HHMMSS           PIC  9(07) COMP-3 VALUE ZEROS.   
           05  WRK-TI-HHMMSSMMMMMM     PIC  9(13) COMP-3 VALUE ZEROS.   
           05  WRK-TIMESTAMP           PIC  X(20) VALUE SPACES.         
                                                                        
      *----------------------------------------------------------------*
      *    CAMPOS AUXILIARES DE DATA                                   *
      *----------------------------------------------------------------*
                                                                        
       01  WRK-AAAAMMDD                PIC  9(09) VALUE ZEROS.          
       01  WRK-AAAAMMDD-R REDEFINES    WRK-AAAAMMDD.                    
           05  FILLER                  PIC  9(01).                      
           05  WRK-ANO-INV             PIC  9(04).                      
           05  WRK-MES-INV             PIC  9(02).                      
           05  WRK-DIA-INV             PIC  9(02).                      
                                                                        
       01  WRK-DDMMAAAA.                                                
           05  WRK-DIA-EDT             PIC  9(02) VALUE ZEROS.          
           05  FILLER                  PIC  X(01) VALUE '/'.            
           05  WRK-MES-EDT             PIC  9(02) VALUE ZEROS.          
           05  FILLER                  PIC  X(01) VALUE '/'.            
           05  WRK-ANO-EDT             PIC  9(04) VALUE ZEROS.          
       01  WRK-DDMMAAAA-R REDEFINES    WRK-DDMMAAAA                     
                                       PIC  X(10).                      
                                                                        
       01  WRK-HORA                    PIC  9(07) VALUE ZEROS.          
       01  WRK-HORA-R    REDEFINES     WRK-HORA.                        
           05  FILLER                  PIC  9(01).                      
           05  WRK-HH                  PIC  9(02).                      
           05  WRK-MM                  PIC  9(02).                      
           05  WRK-SS                  PIC  9(02).                      
                                                                        
       01  WRK-HORA-EDITADA.                                            
           05  WRK-HH-EDT              PIC  9(02) VALUE ZEROS.          
           05  FILLER                  PIC  X(01) VALUE ':'.            
           05  WRK-MM-EDT              PIC  9(02) VALUE ZEROS.          
           05  FILLER                  PIC  X(01) VALUE ':'.            
           05  WRK-SS-EDT              PIC  9(02) VALUE ZEROS.          
       01  WRK-HORA-EDITADA-R          REDEFINES  WRK-HORA-EDITADA      
                                       PIC  X(08).                      
                                                                        
      *----------------------------------------------------------------*
                                                                        
       01  FILLER                      PIC  X(50) VALUE                 
           'CHAVES UTILIZADAS NO BALANCE LINE'.                         
                                                                        
       01  WRK-CHV-ARQENT1.                                             
           05  WRK-CJUNC-ARQENT1       PIC  9(05) VALUE ZEROS.          
           05  WRK-CCTA-ARQENT1        PIC  9(07) VALUE ZEROS.          
                                                                        
       01  WRK-CHV-ARQENT2.                                             
           05  WRK-CJUNC-ARQENT2       PIC  9(05) VALUE ZEROS.          
           05  WRK-CCTA-ARQENT2        PIC  9(07) VALUE ZEROS.          
                                                                        
       01  FILLER.                                                      
           05  WRK-AGE                 PIC  9(05) VALUE ZEROS.          
           05  WRK-AGE-ARQENT2-ANT     PIC  9(05) VALUE ZEROS.          
           05  WRK-AGE-ARQENT2-ATU     PIC  9(05) VALUE ZEROS.          
                                                                        
      *----------------------------------------------------------------*
                                                                        
       01  FILLER                      PIC  X(50) VALUE                 
           'AREAS PARA REDEFINICAO DE CAMPOS PARA DRESS-CODE'.          
                                                                        
       01  WRK-CJUNC-DEPDC             PIC S9(05) VALUE ZEROS.          
       01  WRK-CJUNC-DEPDC-R   REDEFINES WRK-CJUNC-DEPDC                
                                       PIC  9(05).                      
                                                                        
       01  WRK-CCTA-POUP               PIC S9(07) VALUE ZEROS.          
       01  WRK-CCTA-POUP-R     REDEFINES WRK-CCTA-POUP                  
                                       PIC  9(07).                      
                                                                        
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50) VALUE                 
           'FIM DA WORKING POUP1648'.                                   
      *----------------------------------------------------------------*
                                                                        
      *================================================================*
       PROCEDURE                       DIVISION.                        
      *================================================================*
                                                                        
      *----------------------------------------------------------------*
      *  INICIO DO PROCESSAMENTO                                       *
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           PERFORM  1000-INICIALIZAR.                                   
                                                                        
           PERFORM  2000-VERIFICAR-VAZIO.                               
                                                                        
           PERFORM  3000-PROCESSAR                                      
             UNTIL  WRK-FS-ARQENT1  EQUAL   '10'    AND                 
                    WRK-FS-ARQENT2  EQUAL   '10'.                       
                                                                        
           PERFORM  6000-TOTALIZAR.                                     
                                                                        
           PERFORM  7000-FINALIZAR.                                     
                                                                        
      *----------------------------------------------------------------*
       0000-99-FIM.   EXIT.                                             
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *  ABRIR ARQUIVOS E TESTAR FILE-STATUS                           *
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           OPEN  INPUT  ARQENT1                                         
                        ARQENT2                                         
                 OUTPUT ARQSAID.                                        
                                                                        
           MOVE  WRK-ABERTURA          TO  WRK-OPERACAO.                
                                                                        
           PERFORM  1100-TESTAR-FILE-STATUS.                            
                                                                        
           PERFORM  1200-OBTER-DATA-HORA.                               
                                                                        
      *----------------------------------------------------------------*
       1000-99-FIM.   EXIT.                                             
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *  TESTAR FILE-STATUS DOS ARQUIVOS                               *
      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS         SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           PERFORM  1110-TESTAR-FS-ARQENT1.                             
                                                                        
           PERFORM  1120-TESTAR-FS-ARQENT2.                             
                                                                        
           PERFORM  1140-TESTAR-FS-ARQSAID.                             
                                                                        
      *----------------------------------------------------------------*
       1100-99-FIM.   EXIT.                                             
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *  TESTAR FILE-STATUS DO ARQUIVO ARQENT1 (ENTRADA)               *
      *----------------------------------------------------------------*
       1110-TESTAR-FS-ARQENT1          SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           IF WRK-FS-ARQENT1           NOT EQUAL   '00' AND '10'        
              DISPLAY '************** POUP1648 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              ARQENT1              *'           
              DISPLAY '*         FILE STATUS =  ' WRK-FS-ARQENT1        
                                                 '         *'           
              DISPLAY '************** POUP1648 *************'           
              MOVE  'APL'              TO  WRK-TIPO-ACESSO              
              MOVE  'ARQENT1 '         TO  WRK-NOME-ARQUIVO             
              MOVE  WRK-FS-ARQENT1     TO  WRK-FS                       
              MOVE  WRK-MENSAGEM-ERRO  TO  WRK-ERR-TEXTO                
              PERFORM  9999-ROTINA-ERRO                                 
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       1110-99-FIM.   EXIT.                                             
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *  TESTAR FILE-STATUS DO ARQUIVO ARQENT2 (ENTRADA)               *
      *----------------------------------------------------------------*
       1120-TESTAR-FS-ARQENT2          SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           IF WRK-FS-ARQENT2           NOT EQUAL   '00' AND '10'        
              DISPLAY '************** POUP1648 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              ARQENT2              *'           
              DISPLAY '*         FILE STATUS =  ' WRK-FS-ARQENT2        
                                                 '         *'           
              DISPLAY '************** POUP1648 *************'           
              MOVE  'APL'              TO  WRK-TIPO-ACESSO              
              MOVE  'ARQENT2 '         TO  WRK-NOME-ARQUIVO             
              MOVE  WRK-FS-ARQENT2     TO  WRK-FS                       
              MOVE  WRK-MENSAGEM-ERRO  TO  WRK-ERR-TEXTO                
              PERFORM  9999-ROTINA-ERRO                                 
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       1120-99-FIM.   EXIT.                                             
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *  TESTAR FILE-STATUS DO ARQUIVO ARQSAID (SAIDA)                 *
      *----------------------------------------------------------------*
       1140-TESTAR-FS-ARQSAID          SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           IF WRK-FS-ARQSAID           NOT EQUAL   '00'                 
              DISPLAY '************** POUP1648 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              ARQSAID               *'          
              DISPLAY '*         FILE STATUS =  ' WRK-FS-ARQSAID        
                                                 '         *'           
              DISPLAY '************** POUP1648 *************'           
              MOVE  'APL'              TO  WRK-TIPO-ACESSO              
              MOVE  'ARQSAID '         TO  WRK-NOME-ARQUIVO             
              MOVE  WRK-FS-ARQSAID     TO  WRK-FS                       
              MOVE  WRK-MENSAGEM-ERRO  TO  WRK-ERR-TEXTO                
              PERFORM  9999-ROTINA-ERRO                                 
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       1140-99-FIM.   EXIT.                                             
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *  OBTEM DATA E HORA DO SISTEMA                                  *
      *----------------------------------------------------------------*
       1200-OBTER-DATA-HORA            SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           CALL  'BRAD7600'            USING   WRK-DATA-HORA            
                                                                        
           MOVE  WRK-DT-AAAAMMDD       TO  WRK-AAAAMMDD.                
           MOVE  WRK-DIA-INV           TO  WRK-DIA-EDT.                 
           MOVE  WRK-MES-INV           TO  WRK-MES-EDT.                 
           MOVE  WRK-ANO-INV           TO  WRK-ANO-EDT.                 
                                                                        
           MOVE  WRK-TI-HHMMSS         TO  WRK-HORA.                    
           MOVE  WRK-HH                TO  WRK-HH-EDT.                  
           MOVE  WRK-MM                TO  WRK-MM-EDT.                  
           MOVE  WRK-SS                TO  WRK-SS-EDT.                  
                                                                        
      *----------------------------------------------------------------*
       1200-99-FIM. EXIT.                                               
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *  VERIFICAR ARQUIVOS VAZIOS                                     *
      *----------------------------------------------------------------*
       2000-VERIFICAR-VAZIO            SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           PERFORM  2100-LER-ARQENT1.                                   
                                                                        
           IF  WRK-FS-ARQENT1          EQUAL   '10'                     
               DISPLAY  '********* POUP1648 *********'                  
               DISPLAY  '*     ARQUIVO  ARQENT1     *'                  
               DISPLAY  '*          VAZIO,          *'                  
               DISPLAY  '* PROCESSAMENTO ENCERRADO  *'                  
               DISPLAY  '********* POUP1648 *********'                  
               PERFORM  6000-TOTALIZAR                                  
               PERFORM  7000-FINALIZAR                                  
           END-IF.                                                      
                                                                        
           PERFORM  2200-LER-ARQENT2.                                   
                                                                        
           IF  WRK-FS-ARQENT2          EQUAL   '10'                     
               DISPLAY  '********* POUP1648 *********'                  
               DISPLAY  '*     ARQUIVO  ARQENT2     *'                  
               DISPLAY  '*          VAZIO,          *'                  
               DISPLAY  '* PROCESSAMENTO ENCERRADO  *'                  
               DISPLAY  '********* POUP1648 *********'                  
               PERFORM  6000-TOTALIZAR                                  
               PERFORM  7000-FINALIZAR                                  
           ELSE                                                         
               MOVE  WRK-AGE-ARQENT2-ATU   TO  WRK-AGE-ARQENT2-ANT      
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       2000-99-FIM. EXIT.                                               
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *  LEITURA DO ARQUIVO ARQENT1                                    *
      *----------------------------------------------------------------*
       2100-LER-ARQENT1                SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           READ  ARQENT1.                                               
                                                                        
           IF  WRK-FS-ARQENT1          EQUAL   '10'                     
               MOVE  HIGH-VALUES       TO  WRK-CHV-ARQENT1              
               GO                      TO  2100-99-FIM                  
           END-IF.                                                      
                                                                        
           MOVE  WRK-LEITURA           TO  WRK-OPERACAO.                
           PERFORM  1110-TESTAR-FS-ARQENT1.                             
                                                                        
           ADD   1                     TO  ACU-LIDOS-ARQENT1.           
                                                                        
           IF  WRK-CCTA-ARQENT1        EQUAL   9999999                  
               IF  POUP32-CJUNC-DEPDC  EQUAL   WRK-CJUNC-ARQENT1        
                   GO          TO      2100-LER-ARQENT1                 
               END-IF                                                   
           END-IF.                                                      
                                                                        
           IF  POUP32-CCTA-POUP        EQUAL   ZEROS                    
               MOVE  9999999           TO  WRK-CCTA-ARQENT1             
           ELSE                                                         
               MOVE  POUP32-CCTA-POUP  TO  WRK-CCTA-ARQENT1             
           END-IF.                                                      
                                                                        
           MOVE  POUP32-CJUNC-DEPDC    TO  WRK-CJUNC-ARQENT1.           
                                                                        
      *----------------------------------------------------------------*
       2100-99-FIM.   EXIT.                                             
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *  LEITURA DO ARQUIVO ARQENT2                                    *
      *----------------------------------------------------------------*
       2200-LER-ARQENT2                SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           READ  ARQENT2.                                               
                                                                        
           IF  WRK-FS-ARQENT2          EQUAL   '10'                     
               MOVE  HIGH-VALUES       TO  WRK-CHV-ARQENT2              
               GO                      TO  2200-99-FIM                  
           END-IF.                                                      
                                                                        
           MOVE  WRK-LEITURA           TO  WRK-OPERACAO.                
           PERFORM  1120-TESTAR-FS-ARQENT2.                             
                                                                        
           ADD   1                     TO  ACU-LIDOS-ARQENT2.           
                                                                        
           MOVE  POUP49-CJUNC-DEPDC    OF  ARQENT2                      
                                       TO  WRK-CJUNC-DEPDC.             
                                                                        
           MOVE  POUP49-CCTA-POUP      OF  ARQENT2                      
                                       TO  WRK-CCTA-POUP.               
                                                                        
           MOVE  WRK-CJUNC-DEPDC-R     TO  WRK-CJUNC-ARQENT2            
                                           WRK-AGE-ARQENT2-ATU.         
           MOVE  WRK-CCTA-POUP-R       TO  WRK-CCTA-ARQENT2.            
                                                                        
      *----------------------------------------------------------------*
       2200-99-FIM.   EXIT.                                             
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *  PROCEDIMENTOS TRATAMENTO DE REGISTROS                         *
      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           IF  WRK-CHV-ARQENT1         EQUAL   WRK-CHV-ARQENT2          
               PERFORM  3100-TRATAR-CHAVES-IGUAIS                       
           ELSE                                                         
               IF  WRK-CHV-ARQENT1     LESS    WRK-CHV-ARQENT2          
                   PERFORM  3600-TRATAR-ARQENT1-MENOR                   
               ELSE                                                     
                   PERFORM  3700-TRATAR-ARQENT1-MAIOR                   
               END-IF                                                   
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       3000-99-FIM.   EXIT.                                             
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *  PROCEDIMENTOS ARQENT1 IGUAL ARQENT2                           *
      *----------------------------------------------------------------*
       3100-TRATAR-CHAVES-IGUAIS       SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           PERFORM  3300-GRAVA-ARQSAID.                                 
                                                                        
           PERFORM  2200-LER-ARQENT2.                                   
                                                                        
      *----------------------------------------------------------------*
       3100-99-FIM.   EXIT.                                             
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *  GRAVAR ARQUIVO DE SAIDA (ARQSAID)                             *
      *----------------------------------------------------------------*
       3300-GRAVA-ARQSAID              SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE  REG-RECDESP               OF ARQENT2                   
                                           TO REG-RECDESP OF  ARQSAID.  
                                                                        
           WRITE REG-RECDESP               OF  ARQSAID.                 
           MOVE  WRK-GRAVACAO              TO  WRK-OPERACAO.            
           PERFORM 1140-TESTAR-FS-ARQSAID.                              
                                                                        
           ADD   1                         TO  ACU-GRAVA-ARQSAID.       
                                                                        
      *----------------------------------------------------------------*
       3300-99-FIM.   EXIT.                                             
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *  PROCEDIMENTOS ARQENT1 MENOR QUE ARQENT2                       *
      *----------------------------------------------------------------*
       3600-TRATAR-ARQENT1-MENOR       SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           IF  WRK-CJUNC-ARQENT1   NOT EQUAL   WRK-AGE                  
               IF  WRK-CCTA-ARQENT1    EQUAL   9999999                  
                   ADD   1             TO  ACU-DESPZ-ARQENT1-AGE        
               ELSE                                                     
                   ADD   1             TO  ACU-DESPZ-ARQENT1-AGE-CTA    
               END-IF                                                   
           END-IF.                                                      
                                                                        
           PERFORM  2100-LER-ARQENT1.                                   
                                                                        
      *----------------------------------------------------------------*
       3600-99-FIM.   EXIT.                                             
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *  PROCEDIMENTOS ARQENT1 MAIOR QUE ARQENT2                       *
      *----------------------------------------------------------------*
       3700-TRATAR-ARQENT1-MAIOR       SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE POUP49-CJUNC-DEPDC     OF ARQENT2                       
                                       TO WRK-CJUNC-DEPDC.              
                                                                        
           IF (POUP32-CJUNC-DEPDC      EQUAL   WRK-CJUNC-DEPDC-R AND    
               WRK-CCTA-ARQENT1        EQUAL   9999999)                 
               PERFORM  3300-GRAVA-ARQSAID                              
               MOVE  WRK-CJUNC-ARQENT2 TO  WRK-AGE                      
           END-IF.                                                      
                                                                        
           PERFORM  2200-LER-ARQENT2.                                   
                                                                        
      *----------------------------------------------------------------*
       3700-99-FIM.   EXIT.                                             
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *  MOSTRAR ESTATISTICAS DE PROCESSAMENTO                         *
      *----------------------------------------------------------------*
       6000-TOTALIZAR                  SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE  ACU-LIDOS-ARQENT1         TO  WRK-EDIT1                
           MOVE  ACU-LIDOS-ARQENT2         TO  WRK-EDIT2.               
           MOVE  ACU-GRAVA-ARQSAID         TO  WRK-EDIT3.               
           MOVE  ACU-DESPZ-ARQENT1-AGE     TO  WRK-EDIT4.               
           MOVE  ACU-DESPZ-ARQENT1-AGE-CTA TO  WRK-EDIT5.               
                                                                        
           DISPLAY                                                      
           '*************************** POUP1648 ***********************
      -    '****'                                                       
           DISPLAY                                                      
           '*                                                           
      -    '   *'                                                       
           DISPLAY                                                      
           '* LIDOS       DO ARQUIVO ARQENT1                 = '        
            WRK-EDIT1  ' *'                                             
           DISPLAY                                                      
           '* LIDOS       DO ARQUIVO ARQENT2                 = '        
            WRK-EDIT2  ' *'                                             
           DISPLAY                                                      
           '* GRAVADOS    DO ARQUIVO ARQSAID                 = '        
            WRK-EDIT3  ' *'                                             
           DISPLAY                                                      
           '* DESPREZADOS DO ARQUIVO ARQENT1 AGENCIA         = '        
            WRK-EDIT4  ' *'                                             
           DISPLAY                                                      
           '* DESPREZADOS DO ARQUIVO ARQENT1 AGENCIA E CONTA = '        
            WRK-EDIT5  ' *'                                             
           DISPLAY                                                      
           '*                                                           
      -    '   *'                                                       
           DISPLAY                                                      
           '*************************** POUP1648 ***********************
      -    '****'.                                                      
                                                                        
      *----------------------------------------------------------------*
       6000-99-FIM.   EXIT.                                             
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *  FECHAR ARQUIVOS                                               *
      *----------------------------------------------------------------*
       7000-FINALIZAR                  SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           CLOSE  ARQENT1                                               
                  ARQENT2                                               
                  ARQSAID.                                              
                                                                        
           MOVE  WRK-FECHAMENTO        TO  WRK-OPERACAO                 
           PERFORM  1100-TESTAR-FILE-STATUS.                            
                                                                        
           STOP RUN.                                                    
                                                                        
      *----------------------------------------------------------------*
       7000-99-FIM.   EXIT.                                             
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *  ROTINA DE ERRO                                                *
      *----------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE  SPACES                TO  ERR-DADOS-SENHAS.            
           MOVE  'POUP1648'            TO  ERR-PGM.                     
           MOVE  WRK-TIPO-ACESSO       TO  ERR-TIPO-ACESSO.             
                                                                        
           MOVE  WRK-ERR-TEXTO         TO      ERR-TEXTO.               
                                                                        
           CALL  'BRAD7100'            USING   WRK-BATCH                
                                               ERRO-AREA.               
                                                                        
           GOBACK.                                                      
                                                                        
      *----------------------------------------------------------------*
       9999-99-FIM. EXIT.                                               
      *----------------------------------------------------------------*

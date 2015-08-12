      *================================================================*
       IDENTIFICATION                  DIVISION.                        
      *================================================================*
                                                                        
       PROGRAM-ID. POUP4040.                                            
       AUTHOR.     FRANCISCO FREIRE.                                    
                                                                        
      *================================================================*
      *               B R Q   -   I T   S E R V I C E S                *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA     : POUP4040                                     *
      *    ANALISTA     : FERNANDA CARUSO        - BRQ IT SERVICES.    *
      *    PROGRAMADOR  : FRANCISCO FREIRE       - BRQ IT SERVICES.    *
      *    DATA         : 10/08/2015                                   *
      *                                                                *
      *    OBJETIVO     : OBTER A RENDA SALARIAL DO CLIENTE E          *
      *                   INDENTIFICAR A FAIXA DE VALOR                *
      *                                                                *
      *                           ARQUIVOS                             *
      *           +---------------------------------------+            *
      *           |  DDNAME  |   I/O   |   BOOK   | LRECL |            *
      *           |----------|---------|----------|-------|            *
      *           | ARQRENDA | INPUT   | PSDCW033 | 0642  |            *
      *           | VMOVTCPF | INPUT   | I#POUP70 | 0050  |            *
      *           | FAIXREND | INPUT   | -------- | 0030  |            *
      *           | ARQBACEN | OUTPUT  | I#POUP71 | 0070  |            *
      *           | SEMRENDA | OUTPUT  | I#POUP70 | 0050  |            *
      *           +---------------------------------------+            *
      *                                                                *
      *    MODULOS :                                                   *
      *                                                                *
      *    -> BRAD7100 (    -   ) - TRATAMENTO DE ERRO DB2             *
      *    -> BRAD7600 (    -   ) - OBTEM DATA E HORA DO SISTEMA       *
      *                                                                *
      *----------------------------------------------------------------*
                                                                        
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
                                                                        
           SELECT  ARQRENDA   ASSIGN  TO  UT-S-ARQRENDA                 
                   FILE       STATUS  IS  WRK-FS-ARQRENDA.              
                                                                        
           SELECT  VMOVTCPF   ASSIGN  TO  UT-S-VMOVTCPF                 
                   FILE       STATUS  IS  WRK-FS-VMOVTCPF.              
                                                                        
           SELECT  FAIXREND   ASSIGN  TO  UT-S-FAIXREND                 
                   FILE       STATUS  IS  WRK-FS-FAIXREND.              
                                                                        
           SELECT  ARQBACEN   ASSIGN  TO  UT-S-ARQBACEN                 
                   FILE       STATUS  IS  WRK-FS-ARQBACEN.              
                                                                        
           SELECT  SEMRENDA   ASSIGN  TO  UT-S-SEMRENDA                 
                   FILE       STATUS  IS  WRK-FS-SEMRENDA.              
                                                                        
      *================================================================*
       DATA DIVISION.                                                   
      *================================================================*
                                                                        
      *----------------------------------------------------------------*
       FILE SECTION.                                                    
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *  INPUT....: LAYOUT DO ARQUIVO ARQRENDA                         *
      *             ORG. SEQUENCIAL    -  LRECL = 642 BYTES            *
      *----------------------------------------------------------------*
                                                                        
       FD  ARQRENDA                                                     
           LABEL       RECORD    STANDARD                               
           RECORDING   MODE      F                                      
           BLOCK       CONTAINS  0.                                     
                                                                        
       COPY 'PSDCW033'.                                                 
                                                                        
      *----------------------------------------------------------------*
      *  INPUT....: LAYOUT DO ARQUIVO VMOVTCPF                         *
      *             ORG. SEQUENCIAL    -  LRECL = 050 BYTES            *
      *----------------------------------------------------------------*
                                                                        
       FD  VMOVTCPF                                                     
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       COPY 'I#POUP70'.                                                 
                                                                        
      *----------------------------------------------------------------*
      * INPUT -                                                        *
      *                                                                *
      *                 ORG. SEQUENCIAL  -  LRECL 030                  *
      *----------------------------------------------------------------*
                                                                        
       FD  FAIXREND                                                     
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  FD-FAIXREND                 PIC X(030).                      
                                                                        
      *----------------------------------------------------------------*
      *  OUTPUT...: LAYOUT DO ARQBACEN                                 *
      *             ORG. SEQUENCIAL    -  LRECL = 070 BYTES            *
      *----------------------------------------------------------------*
                                                                        
       FD  ARQBACEN                                                     
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       COPY 'I#POUP71'.                                                 
                                                                        
      *----------------------------------------------------------------*
      *  OUTPUT...: LAYOUT DO SEMRENDA                                 *
      *             ORG. SEQUENCIAL    -  LRECL = 050 BYTES            *
      *----------------------------------------------------------------*
                                                                        
       FD  SEMRENDA                                                     
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  FD-SEMRENDA                 PIC X(050).                      
                                                                        
      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.                                         
      *----------------------------------------------------------------*
                                                                        
       77  FILLER                      PIC  X(50) VALUE                 
           'INICIO DA WORKING STORAGE POUP4040'.                        
                                                                        
      *----------------------------------------------------------------*
                                                                        
       01  FILLER                      PIC  X(50) VALUE                 
           'AREAS AUXILIARES'.                                          
                                                                        
       01  WRK-I                       PIC  9(09) COMP-3 VALUE ZEROS.   
       01  WRK-TABELA-LEN              PIC  9(09) COMP-3 VALUE ZEROS.   
       01  WRK-FAIXA-ENC               PIC  X(01) VALUE SPACES.         
                                                                        
      *----------------------------------------------------------------*
                                                                        
       01  FILLER                      PIC  X(50) VALUE                 
           'AREAS ACUMULADORES'.                                        
                                                                        
       01  FILLER.                                                      
           05  ACU-LIDOS-ARQRENDA         PIC  9(09) COMP-3 VALUE ZEROS.
           05  ACU-LIDOS-VMOVTCPF         PIC  9(09) COMP-3 VALUE ZEROS.
           05  ACU-LIDOS-FAIXREND         PIC  9(09) COMP-3 VALUE ZEROS.
           05  ACU-GRAVA-ARQBACEN         PIC  9(09) COMP-3 VALUE ZEROS.
           05  ACU-GRAVA-SEMRENDA         PIC  9(09) COMP-3 VALUE ZEROS.
           05  ACU-FAIXA-NAO-ENCONT       PIC  9(09) COMP-3 VALUE ZEROS.
                                                                        
      *----------------------------------------------------------------*
                                                                        
       01  FILLER                      PIC  X(50) VALUE                 
           'TABELA INTERNA'.                                            
                                                                        
       01  WRK-FAIXREND-REG.                                            
           05  WRK-FAIXREND-RENDA-DE   PIC 9(13)V99         VALUE ZEROS.
           05  WRK-FAIXREND-RENDA-ATE  PIC 9(13)V99         VALUE ZEROS.
                                                                        
       01  WRK-AUX-FAIXA-RENDA-DE      PIC 9(13)V99         VALUE ZEROS.
       01  WRK-AUX-FAIXA-RENDA-ATE     PIC 9(13)V99         VALUE ZEROS.
                                                                        
       01  TAB-FAIXA.                                                   
           05  WRK-FAIXA-RENDA         OCCURS    20 TIMES.              
               10  WRK-FAIXA-RENDA-DE  PIC 9(13)V99.
               10  WRK-FAIXA-RENDA-ATE PIC 9(13)V99.
                                                                        
       01  FILLER                      PIC  X(50) VALUE                 
           'AREAS EDITADAS UTILIZADAS NO DISPLAY'.                      
                                                                        
       01  FILLER.                                                      
           05  WRK-EDIT1               PIC ZZZ.ZZZ.ZZ9 VALUE ZEROS.     
           05  WRK-EDIT2               PIC ZZZ.ZZZ.ZZ9 VALUE ZEROS.     
           05  WRK-EDIT3               PIC ZZZ.ZZZ.ZZ9 VALUE ZEROS.     
           05  WRK-EDIT4               PIC ZZZ.ZZZ.ZZ9 VALUE ZEROS.     
           05  WRK-EDIT5               PIC ZZZ.ZZZ.ZZ9 VALUE ZEROS.     
           05  WRK-EDIT6               PIC ZZZ.ZZZ.ZZ9 VALUE ZEROS.     
                                                                        
      *----------------------------------------------------------------*
                                                                        
       01  FILLER                      PIC  X(50) VALUE                 
           'AREAS PARA TESTE DE FILE-STATUS'.                           
                                                                        
       01  FILLER.                                                      
           05  WRK-FS-ARQRENDA         PIC  X(02) VALUE SPACES.         
           05  WRK-FS-VMOVTCPF         PIC  X(02) VALUE SPACES.         
           05  WRK-FS-FAIXREND         PIC  X(02) VALUE SPACES.         
           05  WRK-FS-ARQBACEN         PIC  X(02) VALUE SPACES.         
           05  WRK-FS-SEMRENDA         PIC  X(02) VALUE SPACES.         
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
                                                                        
       01  WRK-CHV-ARQRENDA.                                            
           05 WRK-CPF-PRINC            PIC 9(09) VALUE ZEROS.           
                                                                        
       01  WRK-CHV-VMOVTCPF.                                            
           05 WRK-CPF-PRINC            PIC 9(09) VALUE ZEROS.           
                                                                        
      *----------------------------------------------------------------*
                                                                        
       01  FILLER                      PIC  X(50) VALUE                 
           'AREAS PARA REDEFINICAO DE CAMPOS PARA DRESS-CODE'.          
                                                                        
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50) VALUE                 
           'FIM DA WORKING POUP4040'.                                   
      *----------------------------------------------------------------*
                                                                        
       01  WRK-CPF-PRINC-S9            PIC S9(09) VALUE ZEROS.          
       01  WRK-CPF-PRINC-9     REDEFINES WRK-CPF-PRINC-S9               
                                       PIC  9(09).
                                                                        
       01  WRK-VRENDA-S13              PIC S9(13)V99 VALUE ZEROS.       
       01  WRK-VRENDA-13       REDEFINES WRK-VRENDA-S13                 
                                       PIC  9(13)V99.                   
                                                                        
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
                                                                        
           PERFORM  4000-CARREGAR-FAIXA-RENDA                           
                                                                        
           PERFORM  3000-PROCESSAR                                      
             UNTIL  WRK-FS-VMOVTCPF  EQUAL   '10'.                      
                                                                        
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
                                                                        
           OPEN  INPUT  ARQRENDA                                        
                        VMOVTCPF                                        
                        FAIXREND                                        
                 OUTPUT ARQBACEN                                        
                        SEMRENDA.                                       
                                                                        
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
                                                                        
           PERFORM  1110-TESTAR-FS-ARQRENDA.                            
                                                                        
           PERFORM  1120-TESTAR-FS-VMOVTCPF.                            
                                                                        
           PERFORM  1130-TESTAR-FS-FAIXREND.                            
                                                                        
           PERFORM  1140-TESTAR-FS-ARQBACEN.                            
                                                                        
           PERFORM  1150-TESTAR-FS-SEMRENDA.                            
                                                                        
      *----------------------------------------------------------------*
       1100-99-FIM.   EXIT.                                             
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *  TESTAR FILE-STATUS DO ARQUIVO ARQRENDA(ENTRADA)               *
      *----------------------------------------------------------------*
       1110-TESTAR-FS-ARQRENDA          SECTION.                        
      *----------------------------------------------------------------*
                                                                        
           IF WRK-FS-ARQRENDA           NOT EQUAL   '00' AND '10'       
              DISPLAY '************** POUP4040 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              ARQRENDA              *'          
              DISPLAY '*         FILE STATUS =  ' WRK-FS-ARQRENDA       
                                                 '         *'           
              DISPLAY '************** POUP4040 *************'           
              MOVE  'APL'              TO  WRK-TIPO-ACESSO              
              MOVE  'ARQRENDA'         TO  WRK-NOME-ARQUIVO             
              MOVE  WRK-FS-ARQRENDA    TO  WRK-FS                       
              MOVE  WRK-MENSAGEM-ERRO  TO  WRK-ERR-TEXTO                
              PERFORM  9999-ROTINA-ERRO                                 
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       1110-99-FIM.   EXIT.                                             
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *  TESTAR FILE-STATUS DO ARQUIVO VMOVTCPF(ENTRADA)               *
      *----------------------------------------------------------------*
       1120-TESTAR-FS-VMOVTCPF          SECTION.                        
      *----------------------------------------------------------------*
                                                                        
           IF WRK-FS-VMOVTCPF           NOT EQUAL   '00' AND '10'       
              DISPLAY '************** POUP4040 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              VMOVTCPF              *'          
              DISPLAY '*         FILE STATUS =  ' WRK-FS-VMOVTCPF       
                                                 '         *'           
              DISPLAY '************** POUP4040 *************'           
              MOVE  'APL'              TO  WRK-TIPO-ACESSO              
              MOVE  'VMOVTCPF'         TO  WRK-NOME-ARQUIVO             
              MOVE  WRK-FS-VMOVTCPF    TO  WRK-FS                       
              MOVE  WRK-MENSAGEM-ERRO  TO  WRK-ERR-TEXTO                
              PERFORM  9999-ROTINA-ERRO                                 
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       1120-99-FIM.   EXIT.                                             
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *  TESTAR FILE-STATUS DO ARQUIVO FAIXREND(ENTRADA)               *
      *----------------------------------------------------------------*
       1130-TESTAR-FS-FAIXREND          SECTION.                        
      *----------------------------------------------------------------*
                                                                        
           IF WRK-FS-FAIXREND           NOT EQUAL   '00' AND '10'       
              DISPLAY '************** POUP4040 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              FAIXREND              *'          
              DISPLAY '*         FILE STATUS =  ' WRK-FS-FAIXREND       
                                                 '         *'           
              DISPLAY '************** POUP4040 *************'           
              MOVE  'APL'              TO  WRK-TIPO-ACESSO              
              MOVE  'FAIXREND'         TO  WRK-NOME-ARQUIVO             
              MOVE  WRK-FS-FAIXREND    TO  WRK-FS                       
              MOVE  WRK-MENSAGEM-ERRO  TO  WRK-ERR-TEXTO                
              PERFORM  9999-ROTINA-ERRO                                 
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       1130-99-FIM.   EXIT.                                             
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *  TESTAR FILE-STATUS DO ARQUIVO ARQBACEN(SAIDA)                 *
      *----------------------------------------------------------------*
       1140-TESTAR-FS-ARQBACEN          SECTION.                        
      *----------------------------------------------------------------*
                                                                        
           IF WRK-FS-ARQBACEN           NOT EQUAL   '00'                
              DISPLAY '************** POUP4040 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              ARQBACEN               *'         
              DISPLAY '*         FILE STATUS =  ' WRK-FS-ARQBACEN       
                                                 '         *'           
              DISPLAY '************** POUP4040 *************'           
              MOVE  'APL'              TO  WRK-TIPO-ACESSO              
              MOVE  'ARQBACEN'         TO  WRK-NOME-ARQUIVO             
              MOVE  WRK-FS-ARQBACEN    TO  WRK-FS                       
              MOVE  WRK-MENSAGEM-ERRO  TO  WRK-ERR-TEXTO                
              PERFORM  9999-ROTINA-ERRO                                 
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       1140-99-FIM.   EXIT.                                             
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *  TESTAR FILE-STATUS DO ARQUIVO SEMRENDA(SAIDA)                 *
      *----------------------------------------------------------------*
       1150-TESTAR-FS-SEMRENDA          SECTION.                        
      *----------------------------------------------------------------*
                                                                        
           IF WRK-FS-SEMRENDA           NOT EQUAL   '00'                
              DISPLAY '************** POUP4040 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              SEMRENDA               *'         
              DISPLAY '*         FILE STATUS =  ' WRK-FS-SEMRENDA       
                                                 '         *'           
              DISPLAY '************** POUP4040 *************'           
              MOVE  'APL'              TO  WRK-TIPO-ACESSO              
              MOVE  'SEMRENDA'         TO  WRK-NOME-ARQUIVO             
              MOVE  WRK-FS-SEMRENDA    TO  WRK-FS                       
              MOVE  WRK-MENSAGEM-ERRO  TO  WRK-ERR-TEXTO                
              PERFORM  9999-ROTINA-ERRO                                 
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       1150-99-FIM.   EXIT.                                             
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
                                                                        
           PERFORM  2100-LER-ARQRENDA.                                  
                                                                        
           IF  WRK-FS-ARQRENDA          EQUAL   '10'                    
               DISPLAY  '********* POUP4040 *********'                  
               DISPLAY  '*     ARQUIVO  ARQRENDA    *'                  
               DISPLAY  '*          VAZIO,          *'                  
               DISPLAY  '* PROCESSAMENTO ENCERRADO  *'                  
               DISPLAY  '********* POUP4040 *********'                  
               PERFORM  6000-TOTALIZAR                                  
               PERFORM  9999-ROTINA-ERRO                                
           END-IF.                                                      
                                                                        
           PERFORM  2200-LER-VMOVTCPF.                                  
                                                                        
           IF  WRK-FS-VMOVTCPF          EQUAL   '10'                    
               DISPLAY  '********* POUP4040 *********'                  
               DISPLAY  '*     ARQUIVO  VMOVTCPF    *'                  
               DISPLAY  '*          VAZIO           *'                  
               DISPLAY  '********* POUP4040 *********'                  
               PERFORM  6000-TOTALIZAR                                  
               PERFORM  7000-FINALIZAR                                  
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       2000-99-FIM. EXIT.                                               
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *  LEITURA DO ARQUIVO ARQRENDA                                   *
      *----------------------------------------------------------------*
       2100-LER-ARQRENDA                SECTION.                        
      *----------------------------------------------------------------*
                                                                        
           READ  ARQRENDA.                                              
                                                                        
           IF  WRK-FS-ARQRENDA          EQUAL   '10'                    
               MOVE  HIGH-VALUES       TO  WRK-CHV-ARQRENDA             
               GO                      TO  2100-99-FIM                  
           END-IF.                                                      
                                                                        
           MOVE  WRK-LEITURA           TO  WRK-OPERACAO.                
           PERFORM  1110-TESTAR-FS-ARQRENDA.                            
                                                                        
           ADD   1                     TO  ACU-LIDOS-ARQRENDA.          
                                                                        
           MOVE PSDCW033-08-CPF-CNPJ-NRO                                
                               TO WRK-CPF-PRINC-S9.                     
           MOVE WRK-CPF-PRINC-9                                         
                               TO WRK-CPF-PRINC OF WRK-CHV-ARQRENDA.    
                                                                        
      *----------------------------------------------------------------*
       2100-99-FIM.   EXIT.                                             
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *  LEITURA DO ARQUIVO VMOVTCPF                                   *
      *----------------------------------------------------------------*
       2200-LER-VMOVTCPF                SECTION.                        
      *----------------------------------------------------------------*
                                                                        
           READ  VMOVTCPF.                                              
                                                                        
           IF  WRK-FS-VMOVTCPF          EQUAL   '10'                    
               MOVE  HIGH-VALUES       TO  WRK-CHV-VMOVTCPF             
               GO                      TO  2200-99-FIM                  
           END-IF.                                                      
                                                                        
           MOVE  WRK-LEITURA           TO  WRK-OPERACAO.                
           PERFORM  1120-TESTAR-FS-VMOVTCPF.                            
                                                                        
           ADD   1                     TO  ACU-LIDOS-VMOVTCPF.          
                                                                        
           MOVE POUP70-CPF-PRINC  TO  WRK-CPF-PRINC OF WRK-CHV-VMOVTCPF.
                                                                        
      *----------------------------------------------------------------*
       2200-99-FIM.   EXIT.                                             
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *  LEITURA DO ARQUIVO FAIXREND                                   *
      *----------------------------------------------------------------*
       2300-LER-FAIXREND                SECTION.                        
      *----------------------------------------------------------------*
                                                                        
           READ  FAIXREND              INTO WRK-FAIXREND-REG.           
                                                                        
           IF  WRK-FS-FAIXREND          EQUAL   '10'                    
               GO                      TO  2300-99-FIM                  
           END-IF.                                                      
                                                                        
           MOVE  WRK-LEITURA           TO  WRK-OPERACAO.                
           PERFORM  1130-TESTAR-FS-FAIXREND.                            
                                                                        
           ADD   1                     TO  ACU-LIDOS-FAIXREND.          
                                                                        
           MOVE WRK-FAIXREND-RENDA-DE  TO WRK-AUX-FAIXA-RENDA-DE.       
           MOVE WRK-FAIXREND-RENDA-ATE TO WRK-AUX-FAIXA-RENDA-ATE.      
                                                                        
      *----------------------------------------------------------------*
       2300-99-FIM.   EXIT.                                             
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *  PROCEDIMENTOS TRATAMENTO DE REGISTROS                         *
      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           IF  WRK-CHV-VMOVTCPF EQUAL   WRK-CHV-ARQRENDA                
               PERFORM  3100-TRATAR-CHAVES-IGUAIS                       
           ELSE                                                         
               IF  WRK-CHV-VMOVTCPF LESS WRK-CHV-ARQRENDA               
                   PERFORM  3600-TRATAR-ARQRENDA-MENOR                  
               ELSE                                                     
                   PERFORM  3700-TRATAR-ARQRENDA-MAIOR                  
               END-IF                                                   
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       3000-99-FIM.   EXIT.                                             
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *  PROCEDIMENTOS ARQRENDA IGUAL VMOVTCPF                         *
      *----------------------------------------------------------------*
       3100-TRATAR-CHAVES-IGUAIS       SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           INITIALIZE POUP71-REG.                                       
                                                                        
           MOVE POUP70-CPF-PRINC       TO POUP71-CPF-PRINC              
           MOVE POUP70-CPF-CTRL        TO POUP71-CPF-CTRL               
           MOVE POUP70-VMOVTO-DEP      TO POUP71-VMOVTO-DEP             
           MOVE POUP70-VMOVTO-SAQ      TO POUP71-VMOVTO-SAQ             
           MOVE PSDCW033-08-VRENDA-MES TO WRK-VRENDA-S13                
           MOVE WRK-VRENDA-13          TO POUP71-VRENDA                 
                                                                        
           DISPLAY 'PSDC-VRENDA: ' PSDCW033-08-VRENDA-MES               
           DISPLAY 'POUP-VRENDA: ' POUP71-VRENDA                        
           PERFORM 3200-IDENT-FAIXA-RENDA.                              
                                                                        
           IF WRK-FAIXA-ENC EQUAL 'S'                                   
               PERFORM  3300-GRAVA-ARQBACEN                             
           END-IF                                                       
                                                                        
           PERFORM  2100-LER-ARQRENDA.                                  
                                                                        
           PERFORM  2200-LER-VMOVTCPF.                                  
                                                                        
      *----------------------------------------------------------------*
       3100-99-FIM.   EXIT.                                             
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       3200-IDENT-FAIXA-RENDA           SECTION.                        
      *----------------------------------------------------------------*
                                                                        
           MOVE 'N' TO WRK-FAIXA-ENC                                    
                                                                        
           IF POUP71-VRENDA EQUAL ZEROS                                 
               MOVE ZEROS TO POUP71-FAIXA-RENDA-DE                      
               MOVE ZEROS TO POUP71-FAIXA-RENDA-ATE
               MOVE 'S' TO WRK-FAIXA-ENC
           ELSE                                                         
               PERFORM VARYING WRK-I FROM 1 BY 1                        
                           UNTIL WRK-I GREATER WRK-TABELA-LEN           
                   DISPLAY 'IND: ' WRK-I                                
                   DISPLAY 'WRK-FAIXA-RENDA-DE: '
                                   WRK-FAIXA-RENDA-DE(WRK-I)
                   DISPLAY 'WRK-FAIXA-RENDA-ATE: '
                                   WRK-FAIXA-RENDA-ATE(WRK-I)
                   IF POUP71-VRENDA >= WRK-FAIXA-RENDA-DE(WRK-I) AND    
                      POUP71-VRENDA <= WRK-FAIXA-RENDA-ATE(WRK-I)       
                       MOVE 'S' TO WRK-FAIXA-ENC                        
                       MOVE WRK-FAIXA-RENDA-DE(WRK-I)                   
                                           TO POUP71-FAIXA-RENDA-DE     
                       MOVE WRK-FAIXA-RENDA-ATE(WRK-I)                  
                                           TO POUP71-FAIXA-RENDA-ATE    
                       GO TO 3200-99-FIM                                
                   END-IF                                               
                                                                        
               END-PERFORM                                              
                                                                        
               IF WRK-FAIXA-ENC EQUAL 'N'                               
                   ADD 1 TO ACU-FAIXA-NAO-ENCONT                        
               END-IF                                                   
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       3200-99-FIM.   EXIT.                                             
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *  GRAVAR ARQUIVO DE SAIDA (ARQBACEN)                            *
      *----------------------------------------------------------------*
       3300-GRAVA-ARQBACEN              SECTION.                        
      *----------------------------------------------------------------*
                                                                        
           WRITE POUP71-REG.                                            
                                                                        
           MOVE  WRK-GRAVACAO              TO  WRK-OPERACAO.            
           PERFORM 1140-TESTAR-FS-ARQBACEN.                             
                                                                        
           ADD   1                         TO  ACU-GRAVA-ARQBACEN.      
                                                                        
      *----------------------------------------------------------------*
       3300-99-FIM.   EXIT.                                             
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *  GRAVAR ARQUIVO DE SAIDA (SEMRENDA)                            *
      *----------------------------------------------------------------*
       3400-GRAVA-SEMRENDA              SECTION.                        
      *----------------------------------------------------------------*
                                                                        
           WRITE FD-SEMRENDA            FROM POUP70-REG.                
                                                                        
           MOVE  WRK-GRAVACAO              TO  WRK-OPERACAO.            
           PERFORM 1150-TESTAR-FS-SEMRENDA.                             
                                                                        
           ADD   1                         TO  ACU-GRAVA-SEMRENDA.      
                                                                        
      *----------------------------------------------------------------*
       3400-99-FIM.   EXIT.                                             
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *  PROCEDIMENTOS ARQRENDA MENOR QUE VMOVTCPF                     *
      *----------------------------------------------------------------*
       3600-TRATAR-ARQRENDA-MENOR       SECTION.                        
      *----------------------------------------------------------------*
                                                                        
           PERFORM  3400-GRAVA-SEMRENDA.                                
                                                                        
           PERFORM  2200-LER-VMOVTCPF.                                  
                                                                        
      *----------------------------------------------------------------*
       3600-99-FIM.   EXIT.                                             
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *  PROCEDIMENTOS ARQRENDA MAIOR QUE VMOVTCPF                     *
      *----------------------------------------------------------------*
       3700-TRATAR-ARQRENDA-MAIOR       SECTION.                        
      *----------------------------------------------------------------*
                                                                        
           PERFORM  2100-LER-ARQRENDA.                                  
                                                                        
      *----------------------------------------------------------------*
       3700-99-FIM.   EXIT.                                             
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       4000-CARREGAR-FAIXA-RENDA       SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           PERFORM 2300-LER-FAIXREND                                    
                                                                        
           IF WRK-FS-FAIXREND EQUAL '10'                                
               DISPLAY  '********* POUP4040 *********'                  
               DISPLAY  '*     ARQUIVO  FAIXREND    *'                  
               DISPLAY  '*          VAZIO,          *'                  
               DISPLAY  '* PROCESSAMENTO ENCERRADO  *'                  
               DISPLAY  '********* POUP4040 *********'                  
               PERFORM  6000-TOTALIZAR                                  
               PERFORM  9999-ROTINA-ERRO                                
           END-IF                                                       
                                                                        
           PERFORM VARYING WRK-I FROM 1 BY 1                            
                       UNTIL WRK-I GREATER 20 OR                        
                               WRK-FS-FAIXREND EQUAL '10'               
                                                                        
               MOVE WRK-AUX-FAIXA-RENDA-DE                              
                                       TO WRK-FAIXA-RENDA-DE(WRK-I)     
               MOVE WRK-AUX-FAIXA-RENDA-ATE                             
                                       TO WRK-FAIXA-RENDA-ATE(WRK-I)    
               ADD 1                   TO WRK-TABELA-LEN                
               PERFORM 2300-LER-FAIXREND                                
                                                                        
           END-PERFORM.                                                 
                                                                        
      *----------------------------------------------------------------*
       4000-99-FIM.   EXIT.                                             
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *  MOSTRAR ESTATISTICAS DE PROCESSAMENTO                         *
      *----------------------------------------------------------------*
       6000-TOTALIZAR                  SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE  ACU-LIDOS-ARQRENDA         TO  WRK-EDIT1               
           MOVE  ACU-LIDOS-VMOVTCPF         TO  WRK-EDIT2.              
           MOVE  ACU-LIDOS-FAIXREND         TO  WRK-EDIT3.              
           MOVE  ACU-GRAVA-ARQBACEN         TO  WRK-EDIT4.              
           MOVE  ACU-GRAVA-SEMRENDA         TO  WRK-EDIT5.              
           MOVE  ACU-FAIXA-NAO-ENCONT       TO  WRK-EDIT6.              
                                                                        
           DISPLAY                                                      
           '*************************** POUP4040 ***********************
      -    '****'                                                       
           DISPLAY                                                      
           '*                                                           
      -    '   *'                                                       
           DISPLAY                                                      
           '* LIDOS       DO ARQUIVO ARQRENDA                 = '       
            WRK-EDIT1  ' *'                                             
           DISPLAY                                                      
           '* LIDOS       DO ARQUIVO VMOVTCPF                 = '       
            WRK-EDIT2  ' *'                                             
           DISPLAY                                                      
           '* LIDOS       DO ARQUIVO FAIXREND                 = '       
            WRK-EDIT3  ' *'                                             
           DISPLAY                                                      
           '* GRAVADOS    DO ARQUIVO ARQBACEN                 = '       
            WRK-EDIT4  ' *'                                             
           DISPLAY                                                      
           '* GRAVADOS    DO ARQUIVO SEMRENDA                 = '       
            WRK-EDIT5  ' *'                                             
           DISPLAY                                                      
           '* FAIXA NAO ENCONTRADA                            = '       
            WRK-EDIT6  ' *'                                             
           DISPLAY                                                      
           '*                                                           
      -    '   *'                                                       
           DISPLAY                                                      
           '*************************** POUP4040 ***********************
      -    '****'.                                                      
                                                                        
      *----------------------------------------------------------------*
       6000-99-FIM.   EXIT.                                             
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *  FECHAR ARQUIVOS                                               *
      *----------------------------------------------------------------*
       7000-FINALIZAR                  SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           CLOSE  ARQRENDA                                              
                  VMOVTCPF                                              
                  FAIXREND                                              
                  ARQBACEN                                              
                  SEMRENDA.                                             
                                                                        
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
           MOVE  'POUP4040'            TO  ERR-PGM.                     
           MOVE  WRK-TIPO-ACESSO       TO  ERR-TIPO-ACESSO.             
                                                                        
           MOVE  WRK-ERR-TEXTO         TO      ERR-TEXTO.               
                                                                        
           CALL  'BRAD7100'            USING   WRK-BATCH                
                                               ERRO-AREA.               
                                                                        
           GOBACK.                                                      
                                                                        
      *----------------------------------------------------------------*
       9999-99-FIM. EXIT.                                               
      *----------------------------------------------------------------*

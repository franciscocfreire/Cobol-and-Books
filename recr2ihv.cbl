      *================================================================*
       IDENTIFICATION                  DIVISION.                        
      *================================================================*
       PROGRAM-ID. RECR2IHV.                                            
       AUTHOR.     CESAR S PEREGO.                                      
      *================================================================*
      *             C P M  B R A X I S  -  S I S T E M A S             *
      *----------------------------------------------------------------*
      *    PROGRAMA....: RECR2IHV                                      *
      *    PROGRAMADOR.: CESAR S PEREGO           - CPM BRAXIS - ALPHA *
      *    ANALISTA....: CESAR S PEREGO           - CPM BRAXIS - ALPHA *
      *    DATA........: 25/07/2011                                    *
      *----------------------------------------------------------------*
      *             ALTERACAO DO SERVICO ECOR2911 PARA ECOR2912        *
      *----------------------------------------------------------------*
      *    PROGRAMADOR.: DIEGO CARDOSO            - CPM BRAXIS - ALPHA *
      *    DATA........: 22/08/2013                                    *
      *----------------------------------------------------------------*
      *    OBJETIVO....:                                               *
      *      ACESSAR O MODULO DO ECOR PARA ENVIAR O ARQUIVO DE AVISO   *
      *      DE COBRANCA.                                              *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *      DDNAME                                      INCLUDE/BOOK  *
      *      EMODLAVS                                      RECRWIJA    *
      *      SMODLAVS                                      RECRWIAA    *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *      RECRWIAA - AREA DOS ARQUIVOS EMODLAVS/SMODLAVS.           *
      *      ECORW392 - AREA DE ACESSO A API ECOR2912.                 *
      *      I#CKRS01 - AREA PARA CHECKPOINT/RESTART.                  *
      *      I#FRWKGE - BOOK DE COMUNICACAO COM FRWK2999.              *
      *      I#FRWKAR - COMMAREA FRWK2999 (LOG DE ERROS ARQUIVO).      *
      *      I#FRWKMD - COMMAREA FRWK2999 (LOG DE ERROS MODULO).       *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *      BRAD0450 - MODULO PARA ABENDAR PROGRAMA.                  *
      *      CKRS1000 - CONEXAO COM DB2.                               *
      *      CKRS0100 - CHECKPOINT/RESTART.                            *
      *      FRWK2999 - MODULO PARA TRATAMENTO DE ERROS.               *
      *================================================================*
                                                                        
      *================================================================*
       ENVIRONMENT                     DIVISION.                        
      *================================================================*
                                                                        
      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.                         
      *----------------------------------------------------------------*
                                                                        
       SPECIAL-NAMES.                                                   
           DECIMAL-POINT               IS COMMA.                        
                                                                        
      *----------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.                         
      *----------------------------------------------------------------*
                                                                        
       FILE-CONTROL.                                                    
                                                                        
           SELECT  EMODLAVS   ASSIGN   TO  EMODLAVS                     
                   FILE       STATUS   IS  WRK-FS-EMODLAVS.             
                                                                        
           SELECT  SMODLAVS   ASSIGN   TO  SMODLAVS                     
                   FILE       STATUS   IS  WRK-FS-SMODLAVS.             
                                                                        
      *================================================================*
       DATA                            DIVISION.                        
      *================================================================*
                                                                        
      *----------------------------------------------------------------*
       FILE                            SECTION.                         
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *    INPUT : ARQUIVO DE ENTRADA                                  *
      *            ORG. SEQUENCIAL     LRECL = 1500                    *
      *----------------------------------------------------------------*
                                                                        
       FD  EMODLAVS                                                     
           RECORDING MODE IS F                                          
           BLOCK     CONTAINS 0 RECORDS                                 
           LABEL     RECORD IS STANDARD.                                
                                                                        
       01  FD-EMODLAVS                 PIC  X(1500).                    
                                                                        
      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE SAIDA                                    *
      *            ORG. SEQUENCIAL     LRECL = 800                     *
      *----------------------------------------------------------------*
                                                                        
       FD  SMODLAVS                                                     
           RECORDING MODE IS F                                          
           BLOCK     CONTAINS 0 RECORDS                                 
           LABEL     RECORD IS STANDARD.                                
                                                                        
       01  FD-SMODLAVS                 PIC  X(800).                     
                                                                        
      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.                         
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(32)          VALUE        
           '*  INICIO DA WORKING RECR2IHV  *'.                          
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(32)          VALUE        
           '*     VARIAVEIS AUXILIARES     *'.                          
      *----------------------------------------------------------------*
                                                                        
       77  WRK-PROGRAMA                PIC  X(08)          VALUE        
           'RECR2IHV'.                                                  
                                                                        
       77  WRK-LOCAL                   PIC  X(04)          VALUE SPACES.
                                                                        
       77  WRK-MASCARA                 PIC  ZZZ.ZZZ.ZZ9    VALUE ZEROS. 
                                                                        
       77  WRK-MASCARA-SQLCODE         PIC  ZZZ.ZZZ.ZZ9-   VALUE ZEROS. 
                                                                        
       77  WRK-DATA-ATUAL              PIC  9(08)          VALUE ZEROS. 
                                                                        
       01  WRK-SGMTO-CLIENTE           PIC  9(05)          VALUE ZEROS. 
       01  FILLER                      REDEFINES WRK-SGMTO-CLIENTE.     
           05 FILLER                   PIC  9(02).                      
           05 WRK-SGMTO-CLIENTE-R      PIC  9(03).                      
                                                                        
       01  WRK-IDENT-CLIENTE.                                           
           05 WRK-ID-CPSSOA-JURID      PIC  9(10)          VALUE ZEROS. 
           05 WRK-ID-CTPO-CONTR        PIC  9(03)          VALUE ZEROS. 
           05 WRK-ID-NSEQ-CONTR        PIC  9(10)          VALUE ZEROS. 
           05 WRK-ID-CPF-CNPJ-NRO      PIC  9(09)          VALUE ZEROS. 
           05 WRK-ID-CPF-CNPJ-FIL      PIC  9(04)          VALUE ZEROS. 
           05 WRK-ID-CPF-CNPJ-CTR      PIC  9(02)          VALUE ZEROS. 
                                                                        
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)          VALUE        
           '* VARIAVES PARA FORMATACAO DE ENDERECO *'.                  
      *----------------------------------------------------------------*
       77  WRK-IND                     PIC  9(05)  COMP-3  VALUE ZEROS. 
       77  WRK-TAM-NUMERO              PIC  9(05)  COMP-3  VALUE ZEROS. 
       77  WRK-TAM-ENDERECO            PIC  9(05)  COMP-3  VALUE ZEROS. 
       77  WRK-TAM-STRING              PIC  9(05)  COMP-3  VALUE ZEROS. 
       77  WRK-TAM-COMPL               PIC  9(05)  COMP-3  VALUE ZEROS. 
                                                                        
       01  WRK-NUM-OK                  PIC  X(01)          VALUE SPACES.
       01  WRK-INIC-NUM                PIC  9(05)          VALUE ZEROS. 
       01  WRK-FNAL-NUM                PIC  9(05)          VALUE ZEROS. 
       01  WRK-FNAL-END                PIC  9(05)          VALUE ZEROS. 
       01  WRK-FNAL-COMPL              PIC  9(05)          VALUE ZEROS. 
                                                                        
       01  WRK-STRING                  PIC  X(40)          VALUE SPACES.
                                                                        
       01  FILLER.                                                      
           05  WRK-ENDERECO            PIC  X(40)          VALUE SPACES.
           05  FILLER REDEFINES        WRK-ENDERECO     OCCURS 40 TIMES.
               10  WRK-CHAR-E          PIC  X(01).                      
                                                                        
           05  WRK-NUMERO              PIC  X(07)          VALUE SPACES.
           05  FILLER REDEFINES        WRK-NUMERO       OCCURS 07 TIMES.
               10  WRK-CHAR-N          PIC  X(01).                      
                                                                        
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(32)          VALUE        
           '*         ACUMULADORES         *'.                          
      *----------------------------------------------------------------*
                                                                        
       77  ACU-LIDOS-EMODLAVS          PIC  9(09)  COMP-3  VALUE ZEROS. 
       77  ACU-GRAVADOS-SMODLAVS       PIC  9(09)  COMP-3  VALUE ZEROS. 
                                                                        
       77  ACU-SEQ-REG-ECOR            PIC  9(09)  COMP-3  VALUE ZEROS. 
                                                                        
       77  ACU-ACESSOS-ECOR            PIC  9(09)  COMP-3  VALUE ZEROS. 
                                                                        
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(32)          VALUE        
           '* AREA DE REGISTRO DE EMODLAVS *'.                          
      *----------------------------------------------------------------*
                                                                        
       01  WRK-AREA-RECRWIJA.                                           
           COPY 'RECRWIJA'.                                             
                                                                        
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(32)          VALUE        
           '* AREA DE REGISTRO DE SMODLAVS *'.                          
      *----------------------------------------------------------------*
                                                                        
       01  WRK-AREA-RECRWIAA.                                           
           COPY 'RECRWIAA'.                                             
                                                                        
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(32)          VALUE        
           '*     AREA PARA FILE-STATUS    *'.                          
      *----------------------------------------------------------------*
                                                                        
       01  WRK-FS-EMODLAVS             PIC  X(02)          VALUE SPACES.
       01  WRK-FS-SMODLAVS             PIC  X(02)          VALUE SPACES.
                                                                        
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(32)          VALUE        
           '*       AREA DA BRAD0450       *'.                          
      *----------------------------------------------------------------*
                                                                        
       77  WRK-ABEND-0450              PIC S9(04)  COMP    VALUE +1111. 
       77  WRK-DUMP-0450               PIC  X(01)          VALUE 'S'.   
       01  WRK-ABEND                   PIC  X(01)          VALUE SPACES.
           88  WRK-ABENDAR                                 VALUE 'S'.   
                                                                        
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)          VALUE        
           '* AREA DE COMUNICACAO DO PROGRAMA CKRS0105 *'.              
      *----------------------------------------------------------------*
                                                                        
       77  WRK-CKRS0100                PIC  X(08)          VALUE        
           'CKRS0100'.                                                  
                                                                        
       COPY 'I#CKRS01'.                                                 
                                                                        
       01  WRK-AREA-RESTART.                                            
           05 WRK-LIDOS-EMODLAVS-RST   PIC  9(09)  COMP-3  VALUE ZEROS. 
           05 WRK-GRAVA-SMODLAVS-RST   PIC  9(09)  COMP-3  VALUE ZEROS. 
                                                                        
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE        
           '* AREA DE COMUNICACAO COM O MODULO DE DATAS *'.             
      *----------------------------------------------------------------*
                                                                        
       77  WRK-RECR2DAO                PIC  X(08)          VALUE        
           'RECR2DAO'.                                                  
                                                                        
       01  WRK-AREA-RECR2DAO.                                           
           COPY RECRWDAA.                                               
                                                                        
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)          VALUE        
           '* AREA DE COMUNICACAO DO PROGRAMA ECOR2912 *'.              
      *----------------------------------------------------------------*
                                                                        
       77  WRK-ECOR2912                PIC  X(08)          VALUE        
           'ECOR2912'.                                                  
                                                                        
       01  WRK-AREA-ECOR2912.                                           
           COPY 'ECORW392'.                                             
                                                                        
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)          VALUE        
           '* AREA DE COMUNICACAO DO PROGRAMA FRWK2999 *'.              
      *----------------------------------------------------------------*
                                                                        
       77  WRK-FRWK2999                PIC  X(08)          VALUE        
           'FRWK2999'.                                                  
                                                                        
       01  WRK-AREA-ERRO.                                               
           COPY 'I#FRWKGE'.                                             
           05 WRK-BLOCO-INFO-ERRO      VALUE SPACES.                    
              10 WRK-CHAR-INFO-ERRO    PIC X(01) OCCURS 0 TO 526   TIMES
                                       DEPENDING ON FRWKGHEA-TAM-DADOS. 
                                                                        
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)          VALUE        
           '* AREA DE TRATAMENTO DE ERROS DE ARQUIVO *'.                
      *----------------------------------------------------------------*
                                                                        
       01  WRK-AREA-ERRO-ARQ.                                           
           COPY 'I#FRWKAR'.                                             
                                                                        
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)          VALUE        
           '* AREA DE TRATAMENTO DE ERROS DE MODULO *'.                 
      *----------------------------------------------------------------*
                                                                        
       01  WRK-AREA-ERRO-MOD.                                           
           COPY 'I#FRWKMD'.                                             
                                                                        
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)          VALUE        
           '* AREA DE TRATAMENTO DE ERRO DB2 *'.                        
      *----------------------------------------------------------------*
                                                                        
       01  WRK-AREA-ERRO-DB2.                                           
           COPY 'I#FRWKDB'.                                             
                                                                        
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(32)          VALUE        
           '*    FIM DA WORKING RECR2IHV   *'.                          
      *----------------------------------------------------------------*
                                                                        
      *================================================================*
       PROCEDURE                       DIVISION.                        
      *================================================================*
                                                                        
      *----------------------------------------------------------------*
      * ROTINA PRINCIPAL DO PROGRAMA                                   *
      *----------------------------------------------------------------*
       0000-PRINCIPAL                  SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           CALL 'CKRS1000'.                                             
                                                                        
           PERFORM 1000-INICIALIZAR.                                    
                                                                        
           PERFORM 2000-VERIFICAR-VAZIO.                                
                                                                        
           PERFORM 3000-PROCESSAR                                       
             UNTIL WRK-FS-EMODLAVS     EQUAL '10'.                      
                                                                        
           PERFORM 4000-FINALIZAR.                                      
                                                                        
      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * ROTINA DE INICIALIZACAO DO PROGRAMA                            *
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           INITIALIZE FRWKGHEA-REGISTRO                                 
                      FRWKGARQ-REGISTRO                                 
                      FRWKGMOD-REGISTRO.                                
                                                                        
           PERFORM 7000-CKRS-INICIALIZAR.                               
                                                                        
           MOVE '1000-INICIALIZAR'     TO  FRWKGHEA-IDEN-PARAGRAFO.     
           MOVE '0010'                 TO  WRK-LOCAL.                   
           MOVE 'OPEN'                 TO  FRWKGARQ-COMANDO.            
           SET   ARQ-OPEN              TO  TRUE.                        
                                                                        
           OPEN  INPUT  EMODLAVS                                        
                 OUTPUT SMODLAVS.                                       
                                                                        
           PERFORM 1100-TESTAR-FILE-STATUS.                             
                                                                        
           PERFORM 1200-OBTER-DATA-MOVIMENTO.                           
                                                                        
      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * ROTINA PARA TESTAR FILE STATUS DOS ARQUIVOS                     
      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS         SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           PERFORM 1110-TESTAR-FS-EMODLAVS.                             
                                                                        
           PERFORM 1120-TESTAR-FS-SMODLAVS.                             
                                                                        
      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * TESTAR FILE STATUS EMODLAVS                                    *
      *----------------------------------------------------------------*
       1110-TESTAR-FS-EMODLAVS         SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           IF (WRK-FS-EMODLAVS         NOT EQUAL '00' AND '10')         
               MOVE  WRK-FS-EMODLAVS   TO  FRWKGARQ-FILE-STATUS         
               MOVE 'EMODLAVS'         TO  FRWKGARQ-NOME-ARQUIVO        
               SET   WRK-ABENDAR       TO  TRUE                         
               PERFORM 9000-ERRO-ARQ                                    
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * TESTAR FILE STATUS SMODLAVS                                    *
      *----------------------------------------------------------------*
       1120-TESTAR-FS-SMODLAVS         SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           IF  WRK-FS-SMODLAVS         NOT EQUAL '00'                   
               MOVE  WRK-FS-SMODLAVS   TO  FRWKGARQ-FILE-STATUS         
               MOVE 'SMODLAVS'         TO  FRWKGARQ-NOME-ARQUIVO        
               SET   WRK-ABENDAR       TO  TRUE                         
               PERFORM 9000-ERRO-ARQ                                    
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * ROTINA PARA OBTER A DATA DO SISTEMA                             
      *----------------------------------------------------------------*
       1200-OBTER-DATA-MOVIMENTO       SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE '1200-OBTER-DATA-MOVIMENTO'                             
                                       TO  FRWKGHEA-IDEN-PARAGRAFO.     
                                                                        
           CALL  WRK-RECR2DAO       USING  WRK-AREA-RECR2DAO.           
                                                                        
           EVALUATE RECRWDAA-COD-RETORNO                                
             WHEN ZEROS                                                 
               CONTINUE                                                 
                                                                        
             WHEN 8                                                     
               MOVE  WRK-RECR2DAO      TO  FRWKGMOD-NOME-MODULO         
               MOVE  RECRWDAA-BLOCO-RETORNO                             
                                       TO  FRWKGMOD-BLOCO-RETORNO       
               PERFORM 9100-ERRO-MODULO                                 
                                                                        
             WHEN 12                                                    
               MOVE  WRK-RECR2DAO      TO  FRWKGHEA-NOME-PROGRAMA       
               MOVE  RECRWDAA-AREA-ERRO-DB2                             
                                       TO  WRK-AREA-ERRO-DB2            
               PERFORM 9200-ERRO-DB2                                    
                                                                        
           END-EVALUATE.                                                
                                                                        
           MOVE  RECRWDAA-DT-ACAO-AAAAMMDD                              
                                       TO  WRK-DATA-ATUAL.              
                                                                        
      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * ROTINA PARA VERIFICAR SE O ARQUIVO DE ENTRADA ESTA VAZIO       *
      *----------------------------------------------------------------*
       2000-VERIFICAR-VAZIO            SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           PERFORM 2100-LER-EMODLAVS.                                   
                                                                        
           IF  WRK-FS-EMODLAVS         EQUAL '10'                       
               DISPLAY '******** PROGRAMA RECR2IHV ********'            
               DISPLAY '*                                 *'            
               DISPLAY '*      ARQUIVO EMODLAVS VAZIO     *'            
               DISPLAY '*                                 *'            
               DISPLAY '*     PROCESSAMENTO ENCERRADO.    *'            
               DISPLAY '*                                 *'            
               DISPLAY '******** PROGRAMA RECR2IHV ********'            
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * LEITURA DO ARQUIVO EMODLAVS                                    *
      *----------------------------------------------------------------*
       2100-LER-EMODLAVS               SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE '2100-LER-EMODLAVS'    TO  FRWKGHEA-IDEN-PARAGRAFO.     
           MOVE '0020'                 TO  WRK-LOCAL.                   
           MOVE 'READ'                 TO  FRWKGARQ-COMANDO.            
           SET   ARQ-READ              TO  TRUE.                        
                                                                        
           READ  EMODLAVS            INTO  WRK-AREA-RECRWIJA.           
                                                                        
           PERFORM 1110-TESTAR-FS-EMODLAVS.                             
                                                                        
           IF  WRK-FS-EMODLAVS         EQUAL '00'                       
               ADD   1                 TO  ACU-LIDOS-EMODLAVS           
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * ROTINA DE PROCESSAMENTO DO PROGRAMA                            *
      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           PERFORM 3100-MOVER-CAMPOS-ECOR.                              
                                                                        
PEREGO     PERFORM 3200-ACESSAR-ECOR2912.                               
                                                                        
           PERFORM 7100-CKRS-CHECKPOINT.                                
                                                                        
           PERFORM 3300-GRAVAR-SMODLAVS.                                
                                                                        
           PERFORM 2100-LER-EMODLAVS.                                   
                                                                        
      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * ROTINA OS CAMPOS DO REGISTRO PARA O MODULO DO ECOR             *
      *----------------------------------------------------------------*
       3100-MOVER-CAMPOS-ECOR          SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE '3100-MOVER-CAMPOS-ECOR'                                
                                       TO  FRWKGHEA-IDEN-PARAGRAFO.     
                                                                        
DIEGOC     INITIALIZE ECORW392-REGISTRO.                                
*                                                                       
*          EVALUATE RECRWIJA-MODELO-AVISO                               
*            WHEN  'RC001   '                                           
*              MOVE 'RECRW0AA'         TO  ECORW392-COD-FORM            
*                                                                       
*            WHEN  'RC002   '                                           
*              MOVE 'RECRW0BA'         TO  ECORW392-COD-FORM            
*                                                                       
*            WHEN  'RC003   '                                           
*              MOVE 'RECRW0CA'         TO  ECORW392-COD-FORM            
*                                                                       
*            WHEN  'RC004   '                                           
*              MOVE 'RECRW0DA'         TO  ECORW392-COD-FORM            
*                                                                       
*            WHEN OTHER                                                 
*                 CONTINUE                                              
*          END-EVALUATE.                                                
*                                                                       
*          MOVE  RECRWIJA-MODELO-AVISO TO  ECORW392-COD-DOCTO.          
*          MOVE  999                   TO  ECORW392-CNL-ENVIO.          
*          MOVE  01                    TO  ECORW392-COD-IDIOMA.         
*                                                                       
*     *--> O CODIGO DA ESTRUTURA IDENTIFICA O CLIENTE                   
*     *    BOOK RECRW010                                                
*          MOVE  20569                 TO  ECORW392-COD-ESTRUT-DICD.    
*          MOVE  1                     TO  ECORW392-NUM-VERSAO-DICD.    
*                                                                       
*          SET   ECORW392-GRAVAR       TO  TRUE.                        
*          ADD   1                     TO  ACU-SEQ-REG-ECOR.            
*          MOVE  ACU-SEQ-REG-ECOR      TO  ECORW392-SEQ-REG.            
*          MOVE 'RECR'                 TO  ECORW392-SIS-ENVIO.          
*          MOVE  WRK-PROGRAMA          TO  ECORW392-COD-PROGRAMA.       
*          MOVE  WRK-DATA-ATUAL        TO  ECORW392-DAT-MOVI-APLIC-FUNCL
*          MOVE  ZEROS                 TO  ECORW392-DISTRIB-CORRESP     
*                                          ECORW392-INDIC-VINCULO-ORGNZ.
*          MOVE 'S'                    TO  ECORW392-INDIC-CADTO-CLIENTE 
*          MOVE  RECRWIJA-CCLUB        TO  ECORW392-CPSSOA-ENDER-PSTAL  
FS#01      MOVE  2269651               TO  RECRWIJA-CPSSOA-JURID-CONTR  
*          MOVE  RECRWIJA-CPSSOA-JURID-CONTR                            
*                                      TO  ECORW392-CPSSOA-JURID-PSTAL  
*          MOVE  1                     TO  ECORW392-CSEQ-ENDER-PSSOA    
*          MOVE  RECRWIJA-CPSSOA-JURID-CONTR                            
*                                      TO  ECORW392-CPSSOA-JURID-CONTR  
*          MOVE  RECRWIJA-CTPO-CONTR-NEGOC                              
*                                      TO  ECORW392-CTIPO-CONTR-NEGOC   
*          MOVE  RECRWIJA-NSEQ-CONTR-NEGOC                              
*                                      TO  ECORW392-NSEQ-CONTR-NEGOC    
*          MOVE  RECRWIJA-CPSSOA-JURID-CONTR                            
*                                      TO  WRK-ID-CPSSOA-JURID          
*          MOVE  RECRWIJA-CTPO-CONTR-NEGOC                              
*                                      TO  WRK-ID-CTPO-CONTR            
*          MOVE  RECRWIJA-NSEQ-CONTR-NEGOC                              
*                                      TO  WRK-ID-NSEQ-CONTR            
*          MOVE  RECRWIJA-CPF-CNPJ-NRO TO  WRK-ID-CPF-CNPJ-NRO.         
*          MOVE  RECRWIJA-CPF-CNPJ-FIL TO  WRK-ID-CPF-CNPJ-FIL.         
*          MOVE  RECRWIJA-CPF-CNPJ-CTR TO  WRK-ID-CPF-CNPJ-CTR.         
*          MOVE  WRK-IDENT-CLIENTE     TO  ECORW392-IDENTIF-CLI-DSTNO.  
                                                                        
*          MOVE  RECRWIJA-IPSSOA-COPLT(1:40)                            
*                                      TO  ECORW392-NOME-CLIENTE        
*                                                                       
*          IF  RECRWIJA-RCOMPL-ENDER   EQUAL SPACES                     
*              PERFORM  3110-FORMATAR-ENDER-SIMPLES                     
*          ELSE                                                         
*              PERFORM  3120-FORMATAR-ENDER-COMPLETO                    
*          END-IF.                                                      
*                                                                       
*          MOVE  WRK-STRING            TO  ECORW392-LOGRADOURO.         
*                                                                       
*          MOVE  RECRWIJA-EBAIRO-ENDER(1:20)                            
*                                      TO  ECORW392-BAIRRO              
*          MOVE  RECRWIJA-ICIDDE-ENDER(1:30)                            
*                                      TO  ECORW392-MUNICIPIO.          
*          MOVE  RECRWIJA-CSGL-UF      TO  ECORW392-COD-UF              
*          MOVE  RECRWIJA-CCEP         TO  ECORW392-COD-CEP             
*          MOVE  RECRWIJA-CCEP-COMPL   TO  ECORW392-COMPL-CEP           
*          MOVE  ZEROS                 TO  ECORW392-CFUNC-BDSCO         
*          MOVE 'N'                    TO  ECORW392-PROCESSO            
*          MOVE 'CADU'                 TO  ECORW392-CSIST-BASE-ENDER    
*          MOVE  RECRWIJA-COD-SEGM-CLIENTE                              
*                                      TO  WRK-SGMTO-CLIENTE            
*          MOVE  WRK-SGMTO-CLIENTE-R   TO  ECORW392-SGMTO-CLIENTE       
*          MOVE  1                     TO  ECORW392-SEQ-PAGINA.         
*          MOVE  1                     TO  ECORW392-QTD-PAGINAS.        
*          MOVE  RECRWIJA-AREA-DADOS-DICD                               
*                                      TO  ECORW392-PAGINA.             
*          MOVE  '0'                   TO  ECORW392-TIPO-ARQ            
*          MOVE  RECRWIJA-CPSSOA-JURID-ORGNZ                            
*                                      TO  ECORW392-CPSSOA-INTRN        
*                                          ECORW392-CPSSOA-JURID-REM    
*                                          ECORW392-CPSSOA-JURID-DEV    
*          MOVE  RECRWIJA-NSEQ-UND-ORGNZ                                
*                                      TO  ECORW392-NSEQ-INTRN          
*                                          ECORW392-NSEQ-UND-ORGNZ-REM  
*          MOVE  55640                 TO  ECORW392-NSEQ-UND-ORGNZ-DEV  
DIEGOC     .                                                            
      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * ROTINA RESPONSAVEL POR FORMATAR O ENDERECO SEM COMPLEMENTO     *
      *----------------------------------------------------------------*
       3110-FORMATAR-ENDER-SIMPLES     SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE ZEROS                  TO WRK-INIC-NUM                  
                                          WRK-FNAL-NUM                  
                                          WRK-FNAL-END                  
                                          WRK-TAM-NUMERO                
                                          WRK-TAM-STRING                
                                          WRK-TAM-ENDERECO.             
                                                                        
           MOVE SPACES                 TO WRK-STRING                    
                                          WRK-NUMERO                    
                                          WRK-ENDERECO.                 
                                                                        
           MOVE 'N'                    TO WRK-NUM-OK.                   
                                                                        
           MOVE RECRWIJA-ELOGDR-NRO    TO WRK-NUMERO.                   
                                                                        
           PERFORM VARYING WRK-IND     FROM 1  BY 1                     
                     UNTIL WRK-IND     GREATER 7                        
                                                                        
               EVALUATE WRK-CHAR-N(WRK-IND)                             
               WHEN '0'                                                 
                   IF  WRK-NUM-OK      EQUAL 'S'                        
                       ADD  1          TO WRK-TAM-NUMERO                
                   END-IF                                               
               WHEN '1' THRU '9'                                        
                   IF  WRK-NUM-OK      EQUAL 'N'                        
                       MOVE WRK-IND    TO WRK-INIC-NUM                  
                       MOVE 'S'        TO WRK-NUM-OK                    
                   END-IF                                               
                                                                        
                   ADD  1              TO WRK-TAM-NUMERO                
               END-EVALUATE                                             
           END-PERFORM.                                                 
                                                                        
           MOVE WRK-TAM-NUMERO         TO WRK-FNAL-NUM.                 
                                                                        
           MOVE RECRWIJA-ELOGDR-PSSOA(1:40)                             
                                       TO WRK-ENDERECO.                 
                                                                        
           MOVE 40                     TO WRK-IND.                      
                                                                        
           PERFORM UNTIL WRK-CHAR-E(WRK-IND)                            
                                       NOT EQUAL SPACES                 
               SUBTRACT 1              FROM WRK-IND                     
           END-PERFORM.                                                 
                                                                        
           ADD  1                      TO WRK-IND.                      
                                                                        
           MOVE WRK-IND                TO WRK-TAM-ENDERECO.             
                                                                        
           MOVE WRK-TAM-ENDERECO       TO WRK-FNAL-END.                 
                                                                        
           COMPUTE WRK-TAM-STRING = WRK-TAM-ENDERECO + WRK-TAM-NUMERO.  
                                                                        
           IF  WRK-TAM-STRING          NOT GREATER 40                   
                                                                        
               STRING WRK-ENDERECO(1:WRK-FNAL-END)                      
                      WRK-NUMERO  (WRK-INIC-NUM:WRK-FNAL-NUM)           
                      DELIMITED BY SIZE INTO WRK-STRING                 
               END-STRING                                               
                                                                        
           ELSE                                                         
                                                                        
               COMPUTE WRK-TAM-ENDERECO = 40 - WRK-TAM-NUMERO           
                                                                        
               SUBTRACT 1              FROM WRK-TAM-ENDERECO            
                                                                        
               MOVE WRK-TAM-ENDERECO   TO WRK-FNAL-END                  
                                                                        
               STRING WRK-ENDERECO(1:WRK-FNAL-END)                      
                      ' '                                               
                      WRK-NUMERO  (WRK-INIC-NUM:WRK-FNAL-NUM)           
                      DELIMITED BY SIZE INTO WRK-STRING                 
               END-STRING                                               
                                                                        
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       3110-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * ROTINA RESPONSAVEL POR FORMATAR O ENDERECO COM COMPLEMENTO     *
      *----------------------------------------------------------------*
       3120-FORMATAR-ENDER-COMPLETO    SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE ZEROS                  TO WRK-INIC-NUM                  
                                          WRK-FNAL-NUM                  
                                          WRK-FNAL-END                  
                                          WRK-FNAL-COMPL                
                                          WRK-TAM-COMPL                 
                                          WRK-TAM-NUMERO                
                                          WRK-TAM-STRING                
                                          WRK-TAM-ENDERECO.             
                                                                        
           MOVE SPACES                 TO WRK-STRING                    
                                          WRK-NUMERO                    
                                          WRK-ENDERECO.                 
                                                                        
           MOVE 'N'                    TO WRK-NUM-OK.                   
                                                                        
           MOVE RECRWIJA-ELOGDR-NRO    TO WRK-NUMERO.                   
                                                                        
           PERFORM VARYING WRK-IND     FROM 1  BY 1                     
                     UNTIL WRK-IND     GREATER 7                        
                                                                        
               EVALUATE WRK-CHAR-N(WRK-IND)                             
               WHEN '0'                                                 
                   IF  WRK-NUM-OK      EQUAL 'S'                        
                       ADD  1          TO WRK-TAM-NUMERO                
                   END-IF                                               
               WHEN '1' THRU '9'                                        
                   IF  WRK-NUM-OK      EQUAL 'N'                        
                       MOVE WRK-IND    TO WRK-INIC-NUM                  
                       MOVE 'S'        TO WRK-NUM-OK                    
                   END-IF                                               
                                                                        
                   ADD  1              TO WRK-TAM-NUMERO                
               END-EVALUATE                                             
           END-PERFORM.                                                 
                                                                        
           MOVE WRK-TAM-NUMERO         TO WRK-FNAL-NUM.                 
                                                                        
           MOVE RECRWIJA-ELOGDR-PSSOA(1:25)                             
                                       TO WRK-ENDERECO.                 
                                                                        
           MOVE 25                     TO WRK-IND.                      
                                                                        
           PERFORM UNTIL WRK-CHAR-E(WRK-IND)                            
                                       NOT EQUAL SPACES                 
               SUBTRACT 1              FROM WRK-IND                     
           END-PERFORM.                                                 
                                                                        
           ADD  1                      TO WRK-IND.                      
                                                                        
           MOVE SPACES                 TO WRK-CHAR-E(WRK-IND).          
                                                                        
           MOVE WRK-IND                TO WRK-TAM-ENDERECO.             
                                                                        
           MOVE WRK-TAM-ENDERECO       TO WRK-FNAL-END.                 
                                                                        
           COMPUTE WRK-TAM-COMPL =                                      
                              39 - (WRK-TAM-ENDERECO + WRK-TAM-NUMERO). 
                                                                        
           MOVE WRK-TAM-COMPL          TO WRK-FNAL-COMPL.               
                                                                        
           STRING WRK-ENDERECO         (1:WRK-FNAL-END)                 
                  WRK-NUMERO           (WRK-INIC-NUM:WRK-FNAL-NUM)      
                  ' '                                                   
                  RECRWIJA-RCOMPL-ENDER(1:WRK-FNAL-COMPL)               
                  DELIMITED BY SIZE INTO WRK-STRING                     
           END-STRING.                                                  
                                                                        
      *----------------------------------------------------------------*
       3120-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       3200-ACESSAR-ECOR2912           SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           CALL  WRK-ECOR2912       USING  WRK-AREA-ECOR2912.           
                                                                        
           EVALUATE ECORW392-COD-RETORNO                                
             WHEN ZEROS                                                 
             WHEN 4                                                     
               CONTINUE                                                 
                                                                        
             WHEN 12                                                    
             WHEN 16                                                    
               MOVE  WRK-PROGRAMA      TO  FRWKGMOD-NOME-MODULO         
               MOVE  ECORW392-COD-RETORNO                               
                                       TO  FRWKGMOD-COD-RETORNO         
               MOVE  ECORW392-COD-ERRO TO  FRWKGMOD-COD-ERRO            
               PERFORM 9100-ERRO-MODULO                                 
           END-EVALUATE.                                                
                                                                        
           ADD   1                     TO  ACU-ACESSOS-ECOR.            
                                                                        
      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * ROTINA PARA GRAVAR O ARQUIVO SMODLAVS                          *
      *----------------------------------------------------------------*
       3300-GRAVAR-SMODLAVS            SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE '3300-GRAVAR-SMODLAVS' TO  FRWKGHEA-IDEN-PARAGRAFO.     
           MOVE '0030'                 TO  WRK-LOCAL.                   
           MOVE 'WRITE'                TO  FRWKGARQ-COMANDO.            
           SET   ARQ-WRITE             TO  TRUE.                        
                                                                        
           MOVE  RECRWIJA-REGISTRO     TO  RECRWIAA-REGISTRO.           
                                                                        
           WRITE FD-SMODLAVS         FROM  WRK-AREA-RECRWIAA.           
                                                                        
           PERFORM 1120-TESTAR-FS-SMODLAVS.                             
                                                                        
           ADD   1                     TO  ACU-GRAVADOS-SMODLAVS.       
                                                                        
      *----------------------------------------------------------------*
       3300-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * ROTINA DE FINALIZACAO DO PROGRAMA                              *
      *----------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE '4000-FINALIZAR'       TO  FRWKGHEA-IDEN-PARAGRAFO.     
           MOVE '0040'                 TO  WRK-LOCAL.                   
           MOVE 'CLOSE'                TO  FRWKGARQ-COMANDO.            
           SET   ARQ-CLOSE             TO  TRUE.                        
                                                                        
           CLOSE EMODLAVS                                               
                 SMODLAVS.                                              
                                                                        
           PERFORM 1100-TESTAR-FILE-STATUS.                             
                                                                        
           PERFORM 4100-DISPLAYS-TOTAIS.                                
                                                                        
PEREGO     IF  ACU-SEQ-REG-ECOR        GREATER ZEROS                    
PEREGO         PERFORM 4200-FINALIZAR-ECOR2912                          
PEREGO     END-IF.                                                      
                                                                        
           PERFORM 7200-CKRS-FINALIZAR.                                 
                                                                        
           IF (ACU-LIDOS-EMODLAVS      GREATER ZEROS) AND               
              (ACU-GRAVADOS-SMODLAVS   GREATER ZEROS) AND               
              (ACU-ACESSOS-ECOR        GREATER ZEROS)                   
               DISPLAY '************* PROGRAMA RECR2IHV *************'  
               DISPLAY '*                                           *'  
               DISPLAY '*  PROGRAMA RECR2IHV ENCERRADO COM SUCESSO  *'  
               DISPLAY '*                                           *'  
               DISPLAY '************* PROGRAMA RECR2IHV *************'  
           END-IF.                                                      
                                                                        
           PERFORM 4300-ENCERRAR-STOP-RUN.                              
                                                                        
      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * ROTINA PARA APRESENTAR AS ESTATISTICAS DO PROGRAMA             *
      *----------------------------------------------------------------*
       4100-DISPLAYS-TOTAIS            SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           DISPLAY '************* PROGRAMA RECR2IHV *************'.     
           DISPLAY '*                                           *'.     
           MOVE  ACU-LIDOS-EMODLAVS    TO  WRK-MASCARA.                 
           DISPLAY '* REGISTROS LIDOS EMODLAVS...:  ' WRK-MASCARA ' *'. 
           DISPLAY '*                                           *'.     
           MOVE  ACU-GRAVADOS-SMODLAVS TO  WRK-MASCARA.                 
           DISPLAY '* REGISTROS GRAVADOS SMODLAVS:  ' WRK-MASCARA ' *'. 
           DISPLAY '*                                           *'.     
           MOVE  ACU-ACESSOS-ECOR      TO  WRK-MASCARA.                 
           DISPLAY '* ACESSOS REALIZADOS AO ECOR.:  ' WRK-MASCARA ' *'. 
           DISPLAY '*                                           *'.     
           DISPLAY '*********************************************'.     
                                                                        
      *----------------------------------------------------------------*
       4100-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * ROTINA PARA FINALIZAR O USO DA API DO ECOR                     *
      *----------------------------------------------------------------*
       4200-FINALIZAR-ECOR2912         SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE '4200-FINALIZAR-ECOR2912'                               
                                       TO  FRWKGHEA-IDEN-PARAGRAFO.     
                                                                        
           SET   ECORW392-FECHAR       TO  TRUE.                        
                                                                        
           MOVE  ACU-SEQ-REG-ECOR      TO  ECORW392-TOT-REGS.           
                                                                        
           PERFORM 3200-ACESSAR-ECOR2912.                               
                                                                        
      *----------------------------------------------------------------*
       4200-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * ROTINA DE FINALIZACAO DO PROGRAMA                              *
      *----------------------------------------------------------------*
       4300-ENCERRAR-STOP-RUN          SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           STOP RUN.                                                    
                                                                        
      *----------------------------------------------------------------*
       4300-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * ROTINA PARA INICIALIZAR O CKRS                                 *
      *----------------------------------------------------------------*
       7000-CKRS-INICIALIZAR           SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE '7000-CKRS-INICIALIZACAO'                               
                                       TO  FRWKGHEA-IDEN-PARAGRAFO.     
           MOVE '0050'                 TO  WRK-LOCAL.                   
                                                                        
           INITIALIZE  CKRS01-INTERFACE.                                
                                                                        
           MOVE  SPACES                TO  CK01-PLAN.                   
           MOVE 'DB2'                  TO  CK01-ID-DB2.                 
           SET   CK01-INICIALIZAR      TO  TRUE.                        
           MOVE LENGTH                 OF WRK-AREA-RESTART              
                                       TO CK01-TAM-AREA-RESTART.        
           MOVE WRK-AREA-RESTART       TO CK01-AREA-RESTART             
                                           (1:CK01-TAM-AREA-RESTART).   
                                                                        
           PERFORM 7300-CHAMAR-CKRS.                                    
                                                                        
           IF  CK01-RESTART                                             
               DISPLAY 'RECR2IHV - PROCESSAMENTO COM RESTART'           
               MOVE CK01-AREA-RESTART(1:CK01-TAM-AREA-RESTART)          
                                            TO WRK-AREA-RESTART         
               MOVE WRK-LIDOS-EMODLAVS-RST  TO ACU-LIDOS-EMODLAVS       
               MOVE WRK-GRAVA-SMODLAVS-RST  TO ACU-GRAVADOS-SMODLAVS    
           ELSE                                                         
               DISPLAY 'RECR2IHV - PROCESSAMENTO NORMAL (SEM RESTART)'  
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       7000-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * ROTINA PARA FAZER O CHECKPOINT DO CKRS                         *
      *----------------------------------------------------------------*
       7100-CKRS-CHECKPOINT            SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE '7100-CKRS-CHECKPOINT' TO  FRWKGHEA-IDEN-PARAGRAFO.     
           MOVE '0060'                 TO  WRK-LOCAL.                   
                                                                        
           INITIALIZE  CKRS01-INTERFACE.                                
                                                                        
           MOVE  SPACES                TO  CK01-PLAN.                   
           MOVE 'DB2'                  TO  CK01-ID-DB2.                 
           SET   CK01-CHECKPOINT       TO  TRUE.                        
           MOVE LENGTH                 OF WRK-AREA-RESTART              
                                       TO CK01-TAM-AREA-RESTART.        
                                                                        
           MOVE  ACU-LIDOS-EMODLAVS    TO  WRK-LIDOS-EMODLAVS-RST.      
           MOVE  ACU-GRAVADOS-SMODLAVS TO  WRK-GRAVA-SMODLAVS-RST.      
                                                                        
           MOVE WRK-AREA-RESTART       TO CK01-AREA-RESTART             
                                           (1:CK01-TAM-AREA-RESTART).   
                                                                        
           PERFORM 7300-CHAMAR-CKRS.                                    
                                                                        
      *----------------------------------------------------------------*
       7100-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * ROTINA PARA FINALIZAR O CKRS                                   *
      *----------------------------------------------------------------*
       7200-CKRS-FINALIZAR             SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE '7200-CKRS-FINALIZAR'  TO  FRWKGHEA-IDEN-PARAGRAFO.     
           MOVE '0070'                 TO  WRK-LOCAL.                   
                                                                        
           INITIALIZE  CKRS01-INTERFACE.                                
                                                                        
           MOVE  SPACES                TO  CK01-PLAN.                   
           MOVE 'DB2'                  TO  CK01-ID-DB2.                 
           SET   CK01-FINALIZAR        TO  TRUE.                        
           MOVE  WRK-AREA-RESTART      TO  CK01-AREA-RESTART.           
           MOVE  LENGTH OF WRK-AREA-RESTART                             
                                       TO  CK01-TAM-AREA-RESTART.       
                                                                        
           PERFORM 7300-CHAMAR-CKRS.                                    
                                                                        
      *----------------------------------------------------------------*
       7200-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * ROTINA PARA CHAMAR O MODULO CKRS                               *
      *----------------------------------------------------------------*
       7300-CHAMAR-CKRS                SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           CALL  WRK-CKRS0100          USING CKRS01-INTERFACE           
                                                                        
           IF (RETURN-CODE         NOT EQUAL ZEROS)  OR                 
              (CK01-CODIGO-RETORNO NOT EQUAL ZEROS)                     
               IF (RETURN-CODE         EQUAL 12)     OR                 
                  (CK01-CODIGO-RETORNO EQUAL 12)                        
                   GOBACK                                               
               ELSE                                                     
                   MOVE  12            TO  FRWKGMOD-COD-RETORNO         
                   MOVE  WRK-LOCAL     TO  FRWKGMOD-COD-ERRO            
                   MOVE  CK01-CODIGO-MENSAGEM                           
                                       TO  FRWKGMOD-COD-MENSAGEM        
                   MOVE  WRK-CKRS0100  TO  FRWKGMOD-NOME-MODULO         
                   DISPLAY                                              
                   '***************** RECR2IHV *****************'       
                   DISPLAY                                              
                   '*      ERRO NAO CONTROLADO - CKRS0100      *'       
                   DISPLAY                                              
                   '*                                          *'       
                   DISPLAY                                              
                   '* CK01-CODIGO-RETORNO : '                           
                            CK01-CODIGO-RETORNO '                 *'    
                   DISPLAY '* CK01-CODIGO-MENSAGEM: '                   
                            CK01-CODIGO-MENSAGEM '           *'         
                   DISPLAY                                              
                   '***************** RECR2IHV *****************'       
                   SET   WRK-ABENDAR   TO  TRUE                         
                   PERFORM 9100-ERRO-MODULO                             
               END-IF                                                   
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       7300-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * TRATAMENTO DE ERRO DE ARQUIVO                                  *
      *----------------------------------------------------------------*
       9000-ERRO-ARQ                   SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           SET   ERRO-ARQUIVO          TO  TRUE.                        
                                                                        
           MOVE  WRK-PROGRAMA          TO  FRWKGHEA-NOME-PROGRAMA.      
           MOVE  FRWKGARQ-TAM-LAYOUT   TO  FRWKGHEA-TAM-DADOS.          
           MOVE  WRK-AREA-ERRO-ARQ     TO  WRK-BLOCO-INFO-ERRO.         
                                                                        
           DISPLAY ' '.                                                 
           DISPLAY 'FRWKGARQ-FILE-STATUS   = ' FRWKGARQ-FILE-STATUS.    
           DISPLAY 'FRWKGARQ-NOME-ARQUIVO  = ' FRWKGARQ-NOME-ARQUIVO.   
           DISPLAY 'FRWKGARQ-COMANDO       = ' FRWKGARQ-COMANDO.        
           DISPLAY 'WRK-LOCAL              = ' WRK-LOCAL.               
                                                                        
           PERFORM 9990-API-ERROS.                                      
                                                                        
      *----------------------------------------------------------------*
       9000-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * PROCESSAMENTO DE ERRO DE MODULO                                *
      *----------------------------------------------------------------*
       9100-ERRO-MODULO                SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           SET   ERRO-MODULO           TO  TRUE                         
                                                                        
           MOVE  WRK-PROGRAMA          TO  FRWKGHEA-NOME-PROGRAMA       
           MOVE  FRWKGMOD-TAM-LAYOUT   TO  FRWKGHEA-TAM-DADOS           
           MOVE  WRK-AREA-ERRO-MOD     TO  WRK-BLOCO-INFO-ERRO          
                                                                        
           DISPLAY '** DADOS DE ERRO DE MODULO **'                      
           DISPLAY 'FRWKGMOD-NOME-MODULO  = ' FRWKGMOD-NOME-MODULO      
           DISPLAY 'FRWKGMOD-COD-RETORNO  = ' FRWKGMOD-COD-RETORNO      
           DISPLAY 'FRWKGMOD-COD-ERRO     = ' FRWKGMOD-COD-ERRO         
           DISPLAY 'FRWKGMOD-COD-MENSAGEM = ' FRWKGMOD-COD-MENSAGEM     
                                                                        
           PERFORM 9990-API-ERROS.                                      
                                                                        
      *----------------------------------------------------------------*
       9100-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * PROCESSAMENTO DE ERRO DE DB2                                   *
      *----------------------------------------------------------------*
       9200-ERRO-DB2                   SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           SET   ERRO-DB2              TO  TRUE.                        
                                                                        
           MOVE  FRWKGDB2-TAM-LAYOUT   TO  FRWKGHEA-TAM-DADOS.          
           MOVE  WRK-AREA-ERRO-DB2     TO  WRK-BLOCO-INFO-ERRO          
           MOVE  FRWKGDB2-SQLCODE      TO  WRK-MASCARA-SQLCODE.         
                                                                        
           DISPLAY ' '                                                  
           DISPLAY 'FRWKGDB2-NOME-TABELA    = ' FRWKGDB2-NOME-TABELA.   
           DISPLAY 'FRWKGDB2-LOCAL          = ' FRWKGDB2-LOCAL.         
           DISPLAY 'FRWKGDB2-SQLCODE        = ' WRK-MASCARA-SQLCODE.    
           DISPLAY 'FRWKGDB2-SQLSTATE       = ' FRWKGDB2-SQLSTATE.      
           DISPLAY 'FRWKGDB2-SQLCA          = ' FRWKGDB2-SQLCA.         
                                                                        
           PERFORM 9990-API-ERROS.                                      
                                                                        
      *----------------------------------------------------------------*
       9200-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * ROTINA PARA TRATAMENTO DE ERRO                                 *
      *----------------------------------------------------------------*
       9990-API-ERROS                  SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           PERFORM 4100-DISPLAYS-TOTAIS.                                
                                                                        
           PERFORM 9991-CHAMAR-FRWK2999.                                
                                                                        
           IF  WRK-ABENDAR                                              
               PERFORM 9992-CHAMAR-BRAD0450                             
           END-IF.                                                      
                                                                        
           PERFORM 4300-ENCERRAR-STOP-RUN.                              
                                                                        
      *----------------------------------------------------------------*
       9990-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * ROTINA PARA ACESSO AO FRWK                                     *
      *----------------------------------------------------------------*
       9991-CHAMAR-FRWK2999            SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           DISPLAY '*** FRWK2999 CHAMADO. SEGUEM SEUS PARAMETROS ***'.  
           DISPLAY ' '                                                  
           DISPLAY 'FRWKGHEA-TIPO-ERRO      = ' FRWKGHEA-TIPO-ERRO.     
           DISPLAY 'FRWKGHEA-NOME-PROGRAMA  = ' FRWKGHEA-NOME-PROGRAMA. 
           DISPLAY 'FRWKGHEA-IDEN-PARAGRAFO = ' FRWKGHEA-IDEN-PARAGRAFO.
           DISPLAY 'FRWKGHEA-TAM-DADOS      = ' FRWKGHEA-TAM-DADOS.     
                                                                        
           CALL  WRK-FRWK2999       USING  WRK-AREA-ERRO.               
                                                                        
           DISPLAY '*** PARAMETROS RETORNADOS PELO FRWK2999 ***'.       
           DISPLAY 'FRWKGERR-COD-RETORNO    = ' FRWKGERR-COD-RETORNO.   
           DISPLAY 'FRWKGERR-COD-ERRO       = ' FRWKGERR-COD-ERRO.      
           DISPLAY 'FRWKGERR-COD-MENSAGEM   = ' FRWKGERR-COD-MENSAGEM.  
                                                                        
      *----------------------------------------------------------------*
       9991-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * ROTINA PARA ABENDAR O SISTEMA                                  *
      *----------------------------------------------------------------*
       9992-CHAMAR-BRAD0450            SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           DISPLAY '*** BRAD0450 CHAMADO PARA ABENDAR O PROGRAMA ***'.  
           DISPLAY ' '.                                                 
                                                                        
           CALL 'BRAD0450'          USING  WRK-ABEND-0450               
                                           WRK-DUMP-0450.               
                                                                        
      *----------------------------------------------------------------*
       9992-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*

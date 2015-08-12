      *================================================================*
       IDENTIFICATION                  DIVISION.                        
      *================================================================*
       PROGRAM-ID. CLLP2955.                                            
       AUTHOR. FRANCISCO.                                               
      *===============================================================* 
      *                 B R Q    I T   S E R V I C E S                * 
      *---------------------------------------------------------------* 
      *                                                               * 
      *   PROGRAMA    : CLLP2955                                      * 
      *   PROGRAMADOR : FRANCISCO FREIRE       - BRQ IT SERVICES      * 
      *   ANALISTA    : FRANCISCO FREIRE       - BRQ IT SERVICES      * 
      *   SUPERVISOR  : JUNIOR RIBAMAR         - GP. 70               * 
      *   DATA        : AGOSTO/2014                                   * 
      *                                                               * 
      *   OBJETIVO    : ACESSAR O MODULO DO ECOR PARA ENVIAR O ARQUIVO* 
      *                 DE AVISO COBRANCA.                            * 
      *   ARQUIVOS:                                                   * 
      *   ---------------------------------------------------------   * 
      *   DDNAME   |I/O| DESCRICAO               | BOOK     | LRECL   * 
      *   ---------+---+-------------------------+----------+------   * 
      *   EMODLAVS | I | ENTRADA MOV AVISO       | I#CLLPLJ |  1290   * 
      *   SMODLAVS | I | SAIDA COPIA MOV AVISO   | I#CLLPLJ |  1290   * 
      *   ---------------------------------------------------------   * 
                                                                        
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
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
      *            ORG. SEQUENCIAL     LRECL = 1290                    *
      *----------------------------------------------------------------*
                                                                        
       FD  EMODLAVS                                                     
           RECORDING MODE IS F                                          
           BLOCK     CONTAINS 0 RECORDS                                 
           LABEL     RECORD IS STANDARD.                                
                                                                        
       01  FD-EMODLAVS                 PIC  X(1290).                    
                                                                        
      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE SAIDA                                    *
      *            ORG. SEQUENCIAL     LRECL = 1290                    *
      *----------------------------------------------------------------*
                                                                        
       FD  SMODLAVS                                                     
           RECORDING MODE IS F                                          
           BLOCK     CONTAINS 0 RECORDS                                 
           LABEL     RECORD IS STANDARD.                                
                                                                        
       01  FD-SMODLAVS                 PIC  X(1290).                    
                                                                        
      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.                         
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(32)          VALUE        
           '*  INICIO DA WORKING CLLP2955  *'.                          
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(32)          VALUE        
           '*     VARIAVEIS AUXILIARES     *'.                          
      *----------------------------------------------------------------*
                                                                        
       77  WRK-PROGRAMA                PIC  X(08)          VALUE        
           'CLLP2955'.                                                  
                                                                        
       77  WRK-LOCAL                   PIC  X(04)          VALUE SPACES.
                                                                        
       77  WRK-MASCARA                 PIC  ZZZ.ZZZ.ZZ9    VALUE ZEROS. 
                                                                        
       77  WRK-MASCARA-SQLCODE         PIC  ZZZ.ZZZ.ZZ9-   VALUE ZEROS. 
                                                                        
       77  WRK-DATA-ATUAL              PIC  9(08)          VALUE ZEROS. 
                                                                        
       77  WRK-LIMIT-PAG               PIC  9(09) COMP-3   VALUE 12.    
                                                                        
       01  WRK-DESCRIC                 PIC  X(020)         VALUE SPACES.
                                                                        
       01  WRK-DATA-COMP               PIC  9(09)          VALUE ZEROS. 
       01  FILLER                      REDEFINES WRK-DATA-COMP.         
           05 FILLER                   PIC  9(01).                      
           05 WRK-ANO-COMP             PIC  9(04).                      
           05 WRK-MES-COMP             PIC  9(02).                      
           05 WRK-DIA-COMP             PIC  9(02).                      
                                                                        
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
       01  WRK-AVI-NUM-CC              PIC  9(07)          VALUE ZEROS. 
       01  WRK-AVI-NUM-CC-X            REDEFINES                        
           WRK-AVI-NUM-CC              PIC  X(07).                      
                                                                        
       01  FILLER.                                                      
           05  WRK-ENDERECO            PIC  X(40)          VALUE SPACES.
           05  FILLER REDEFINES        WRK-ENDERECO     OCCURS 40 TIMES.
               10  WRK-CHAR-E          PIC  X(01).                      
                                                                        
           05  WRK-NUMERO              PIC  X(07)          VALUE SPACES.
           05  FILLER REDEFINES        WRK-NUMERO       OCCURS 07 TIMES.
               10  WRK-CHAR-N          PIC  X(01).                      
                                                                        
       01  WRK-PIC15V99                PIC  S9(15)V99  VALUE ZEROS.     
       01  FILLER                      REDEFINES WRK-PIC15V99.          
           05 FILLER                   PIC  9(02).                      
           05 WRK-PIC15                PIC  9(15).                      
       01  FILLER                      REDEFINES WRK-PIC15V99.          
           05 FILLER                   PIC  9(07).                      
           05 WRK-PIC10-T              PIC  9(10).                      
       01  FILLER                      REDEFINES WRK-PIC15V99.          
           05 FILLER                   PIC  9(05).                      
           05 WRK-PIC12-T              PIC  9(12).                      
                                                                        
       01  WRK-PIC11V99                PIC  S9(11)V99  VALUE ZEROS.     
       01  FILLER                      REDEFINES WRK-PIC11V99.          
           05 WRK-PIC13                PIC  9(13).                      
       01  FILLER                      REDEFINES WRK-PIC11V99.          
           05 FILLER                   PIC  9(02).                      
           05 WRK-PIC11                PIC  9(11).                      
       01  FILLER                      REDEFINES WRK-PIC11V99.          
           05 FILLER                   PIC  9(03).                      
           05 WRK-PIC10                PIC  9(10).                      
                                                                        
                                                                        
       01  WRK-PIC13V99                PIC  S9(13)V99  VALUE ZEROS.     
       01  FILLER                      REDEFINES WRK-PIC13V99.          
           05 WRK-PIC15-R              PIC  9(15).                      
       01  FILLER                      REDEFINES WRK-PIC13V99.          
           05 FILLER                   PIC  9(05).                      
           05 WRK-PIC10-R              PIC  9(10).                      
                                                                        
       01  WRK-PIC5                    PIC  9(05)     VALUE ZEROS.      
       01  FILLER                      REDEFINES WRK-PIC5.              
           05 FILLER                   PIC  9(01).                      
           05 WRK-PIC4                 PIC  9(04).                      
                                                                        
       01  WRK-PIC3                    PIC  9(03)     VALUE ZEROS.      
       01  FILLER                      REDEFINES WRK-PIC3.              
           05 FILLER                   PIC  9(01).                      
           05 WRK-PIC2                 PIC  9(02).                      
                                                                        
       01  WRK-IND-PIC9                PIC  9(09)     VALUE ZEROS.      
       01  FILLER                      REDEFINES WRK-IND-PIC9.          
           05 FILLER                   PIC  9(06).                      
           05 WRK-IND-PIC3             PIC  9(03).                      
                                                                        
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(32)          VALUE        
           '*         ACUMULADORES         *'.                          
      *----------------------------------------------------------------*
                                                                        
       77  ACU-LIDOS-EMODLAVS          PIC  9(09)  COMP-3  VALUE ZEROS. 
       77  ACU-GRAVADOS-SMODLAVS       PIC  9(09)  COMP-3  VALUE ZEROS. 
       77  ACU-DIVIDA-TOTAL            PIC S9(15)V99 COMP-3 VALUE ZEROS.
       77  WRK-DIVIDA                  PIC S9(15)V99 COMP-3 VALUE ZEROS.
       77  ACU-SEQ-REG-ECOR            PIC  9(09)  COMP-3  VALUE ZEROS. 
                                                                        
       77  ACU-ACESSOS-ECOR            PIC  9(09)  COMP-3  VALUE ZEROS. 
                                                                        
       77  WRK-IND-DICD                PIC  9(09)  COMP-3  VALUE ZEROS. 
                                                                        
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(32)          VALUE        
           '* AREA DE REGISTRO DE EMODLAVS *'.                          
      *----------------------------------------------------------------*
                                                                        
           COPY 'I#CLLPLJ'.                                             
                                                                        
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(32)          VALUE        
           '* AREA DE REGISTRO DE SMODLAVS *'.                          
      *----------------------------------------------------------------*
                                                                        
       01  WRK-AREA-RECRW810.                                           
           COPY 'CLLPW810'.                                             
                                                                        
       01  WRK-AREA-RECRW820.                                           
           COPY 'CLLPW820'.                                             
                                                                        
       01  WRK-AREA-RECRW830.                                           
           COPY 'CLLPW830'.                                             
                                                                        
       01  WRK-AREA-RECRW840.                                           
           COPY 'CLLPW840'.                                             
                                                                        
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
           '*    FIM DA WORKING CLLP2955   *'.                          
      *----------------------------------------------------------------*
                                                                        
       LINKAGE                         SECTION.                         
                                                                        
       01   LNK-PARM.                                                   
         05 LNK-TAMA-PARM             PIC 9(004) COMP VALUE ZEROS.      
         05 LNK-DIST-PARM             PIC 9(001)      VALUE ZEROS.      
         05 LNK-FORM-PARM             PIC X(005)      VALUE SPACES.     
                                                                        
      *================================================================*
       PROCEDURE                       DIVISION USING LNK-PARM.         
      *================================================================*
                                                                        
      *----------------------------------------------------------------*
      * ROTINA PRINCIPAL DO PROGRAMA                                   *
      *----------------------------------------------------------------*
       0000-PRINCIPAL                  SECTION.                         
      *----------------------------------------------------------------*
                                                                        
      *                                                                 
      *--> "CONEXAO COM O DB2"                                          
      *                                                                 
           CALL 'CKRS1000'                                              
                                                                        
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
                                                                        
           IF LNK-DIST-PARM NOT EQUAL ZEROS AND 1                       
               DISPLAY '******** PROGRAMA CLLP2955 ********'            
               DISPLAY '*                                 *'            
               DISPLAY '*   PARM DISTRIBUICAO INVALIDO    *'            
               DISPLAY '*                                 *'            
               DISPLAY '*     PROCESSAMENTO ENCERRADO     *'            
               DISPLAY '*                                 *'            
               DISPLAY '******** PROGRAMA CLLP2955 ********'            
               PERFORM 9992-CHAMAR-BRAD0450                             
           END-IF.                                                      
                                                                        
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
               DISPLAY '******** PROGRAMA CLLP2955 ********'            
               DISPLAY '*                                 *'            
               DISPLAY '*      ARQUIVO EMODLAVS VAZIO     *'            
               DISPLAY '*                                 *'            
               DISPLAY '*                                 *'            
               DISPLAY '******** PROGRAMA CLLP2955 ********'            
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
                                                                        
           READ  EMODLAVS            INTO  REG-AVISO.                   
                                                                        
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
                                                                        
           PERFORM 3105-MOVER-DADOS-DICD.                               
                                                                        
           PERFORM 3200-ACESSAR-ECOR2912.                               
                                                                        
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
                                                                        
           INITIALIZE ECORW392-REGISTRO.                                
                                                                        
           EVALUATE LNK-FORM-PARM                                       
             WHEN  'RC001'                                              
               MOVE 'RECRW810'         TO  ECORW392-COD-FORM            
                                                                        
             WHEN  'RC002'                                              
               MOVE 'RECRW820'         TO  ECORW392-COD-FORM            
                                                                        
             WHEN  'RC003'                                              
               MOVE 'RECRW830'         TO  ECORW392-COD-FORM            
                                                                        
             WHEN  'RC004'                                              
               MOVE 'RECRW840'         TO  ECORW392-COD-FORM            
                                                                        
             WHEN OTHER                                                 
                  CONTINUE                                              
           END-EVALUATE.                                                
                                                                        
           MOVE  LNK-FORM-PARM         TO  ECORW392-COD-DOCTO.          
           MOVE  999                   TO  ECORW392-CNL-ENVIO.          
           MOVE  01                    TO  ECORW392-COD-IDIOMA.         

      *--> O CODIGO DA ESTRUTURA IDENTIFICA O CLIENTE                   
      *    BOOK RECRW810                                                
           MOVE  20569                 TO  ECORW392-COD-ESTRUT-DICD.    
           MOVE  1                     TO  ECORW392-NUM-VERSAO-DICD.    
                                                                        
           SET   ECORW392-GRAVAR       TO  TRUE.                        
           ADD   1                     TO  ACU-SEQ-REG-ECOR.            
           MOVE  ACU-SEQ-REG-ECOR      TO  ECORW392-SEQ-REG.            
           MOVE 'RECR'                 TO  ECORW392-SIS-ENVIO.          
           MOVE  WRK-PROGRAMA          TO  ECORW392-COD-PROGRAMA.       
           MOVE  WRK-DATA-ATUAL        TO  ECORW392-DAT-MOVI-APLIC-FUNCL
           MOVE  LNK-DIST-PARM         TO  ECORW392-DISTRIB-CORRESP     
                                           ECORW392-INDIC-VINCULO-ORGNZ.
           MOVE 'S'                    TO  ECORW392-INDIC-CADTO-CLIENTE 
                                                                        
           MOVE  ZEROS                 TO  ECORW392-CPSSOA-ENDER-PSTAL  
                                                                        
           MOVE  ZEROS                 TO  ECORW392-CPSSOA-JURID-PSTAL  
           MOVE  ZEROS                 TO  ECORW392-CSEQ-ENDER-PSSOA    
           MOVE  ZEROS                 TO  ECORW392-CPSSOA-JURID-CONTR  
           MOVE  ZEROS                 TO  ECORW392-CTIPO-CONTR-NEGOC   
           MOVE  ZEROS                 TO  ECORW392-NSEQ-CONTR-NEGOC    
      *#################################################################
      *'ENCONTRAR COMO FORMATAR O IDENTIF-CLI-DSTNO                     
      *                                                                #
           MOVE  2269651               TO  WRK-ID-CPSSOA-JURID          
           MOVE  WRK-IDENT-CLIENTE     TO  ECORW392-IDENTIF-CLI-DSTNO.  
      *#################################################################
                                                                        
           MOVE  237                   TO  ECORW392-BANCO               
           MOVE  AVI-AGENCIA           TO  ECORW392-AGENCIA             
           MOVE  AVI-NUM-CC            TO  ECORW392-CONTA               

           MOVE AVI-NOME-AVAL          TO  ECORW392-NOME-CLIENTE
           IF AVI-FIL-AVAL EQUAL ZEROS                                  
              MOVE 'F' TO ECORW392-TIPO-PSSOA                           
           ELSE                                                         
              MOVE 'J' TO ECORW392-TIPO-PSSOA                           
           END-IF                                                       
                                                                        
      *#################################################################
      *    CAMPOS NAO MOSTRADOS NO AVISO. UTILIZAR O DA CHAVE OU DO AVA 
      *#################################################################
           MOVE AVI-CGC-AVAL TO ECORW392-CPF-CNPJ                       
                                                                        
           MOVE AVI-FIL-AVAL TO WRK-PIC5                                
           MOVE WRK-PIC4     TO ECORW392-CFLIL-CNPJ                     
                                                                        
           MOVE AVI-CTR-AVAL TO WRK-PIC3                                
           MOVE WRK-PIC2     TO ECORW392-CCTRL-CPF-CNPJ                 
                                                                        
           IF  AVI-COMPL               EQUAL SPACES                     
               PERFORM  3110-FORMATAR-ENDER-SIMPLES                     
           ELSE                                                         
               PERFORM  3120-FORMATAR-ENDER-COMPLETO                    
           END-IF.                                                      
                                                                        
           MOVE  WRK-STRING            TO  ECORW392-LOGRADOURO.         
                                                                        
           MOVE  AVI-BAIRRO            TO  ECORW392-BAIRRO              
           MOVE  AVI-CIDADE            TO  ECORW392-MUNICIPIO.          
           MOVE  AVI-UF                TO  ECORW392-COD-UF              
           MOVE  AVI-CCEP              TO  ECORW392-COD-CEP             
           MOVE  AVI-CCEP-COMPL        TO  ECORW392-COMPL-CEP           
           MOVE  ZEROS                 TO  ECORW392-CFUNC-BDSCO         
           MOVE 'N'                    TO  ECORW392-PROCESSO            
                                                                        
           MOVE 'CADU'                 TO  ECORW392-CSIST-BASE-ENDER    
           MOVE  ZEROS                 TO  ECORW392-SGMTO-CLIENTE       
           MOVE  1                     TO  ECORW392-SEQ-PAGINA.         
           MOVE  1                     TO  ECORW392-QTD-PAGINAS.        
           MOVE  '1'                   TO  ECORW392-TIPO-ARQ            
           MOVE  ZEROS                                                  
                                       TO  ECORW392-CPSSOA-INTRN        
                                           ECORW392-CPSSOA-JURID-REM    
                                           ECORW392-CPSSOA-JURID-DEV    
           MOVE  ZEROS                                                  
                                       TO  ECORW392-NSEQ-INTRN          
                                           ECORW392-NSEQ-UND-ORGNZ-REM  
           MOVE  ZEROS                 TO  ECORW392-NSEQ-UND-ORGNZ-DEV  
           .                                                            
      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
                                                                        
      *----------------------------------------------------------------*
      * PREENCHE DADOS DA ESTRUTURA PARA A CHAMADA DO ECOR             *
      *----------------------------------------------------------------*
       3105-MOVER-DADOS-DICD        SECTION.                            
      *----------------------------------------------------------------*
                                                                        
           EVALUATE LNK-FORM-PARM                                       
             WHEN  'RC001'                                              
               PERFORM 3106-MOVER-DICD-RECRW810                         
             WHEN  'RC002'                                              
               PERFORM 3107-MOVER-DICD-RECRW820                         
             WHEN  'RC003'                                              
               PERFORM 3108-MOVER-DICD-RECRW830                         
             WHEN  'RC004'                                              
               PERFORM 3109-MOVER-DICD-RECRW840                         
             WHEN OTHER                                                 
                  CONTINUE                                              
           END-EVALUATE.                                                
                                                                        
      *----------------------------------------------------------------*
       3105-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * PREENCHE DADOS DA ESTRUTURA PARA A CHAMADA DO ECOR             *
      *----------------------------------------------------------------*
       3106-MOVER-DICD-RECRW810        SECTION.                         
      *----------------------------------------------------------------*
                                                                        
      *#################################################################
      *' NOME DEVE SER DO DEVEDOR OU AVALISTA INFORMADO AO ECOR         
           MOVE AVI-NOME-DEVEDOR       TO RECRW810-NM-CLIENTE

      *#################################################################
                                                                        
           MOVE ZEROS                  TO ACU-DIVIDA-TOTAL              
                                                                        
           STRING AVI-DATA-MVTO(7:2) '.'                                
                  AVI-DATA-MVTO(5:2) '.'                                
                  AVI-DATA-MVTO(1:4)                                    
           DELIMITED BY SIZE INTO RECRW810-DT-BASE                      
                                                                        
           STRING AVI-MUNIC-AGENCIA ' '                                 
                  RECRW810-DT-BASE                                      
           DELIMITED BY SIZE INTO RECRW810-LOCAL-DATA                   
                                                                        
           MOVE 1 TO WRK-IND-DICD                                       
                                                                        
           PERFORM UNTIL AVI-NATUREZA(WRK-IND-DICD) EQUAL SPACES OR     
                          WRK-IND-DICD EQUAL WRK-LIMIT-PAG              
                                                                        
               PERFORM 3150-LER-TABELA-DESCNAT                          
               MOVE WRK-DESCRIC TO    RECRW810-DS-PRODUTO(WRK-IND-DICD) 
                                                                        
                                                                        
               MOVE AVI-DAT-VENCTO(WRK-IND-DICD) TO WRK-DATA-COMP       
                                                                        
               STRING WRK-DIA-COMP '.'                                  
                       WRK-MES-COMP '.'                                 
                       WRK-ANO-COMP                                     
               DELIMITED BY SIZE INTO                                   
                       RECRW810-DT-VENCTO-PARC-ATRS(WRK-IND-DICD)       
                                                                        
               MOVE AVI-CONTRATO(WRK-IND-DICD)                          
                                   TO RECRW810-NU-CONTRATO(WRK-IND-DICD)
                                                                        
      *##########   VALOR PRINCIPAL ####################################
               MOVE AVI-RESGATE(WRK-IND-DICD)                           
                                  TO WRK-PIC11V99                       
               MOVE WRK-PIC13     TO RECRW810-VR-PRINCIPAL(WRK-IND-DICD)
      *#################################################################
                                                                        
      *##########   VALOR REMUNERATORIO ################################
               MOVE AVI-VR-REMUNERATORIO(WRK-IND-DICD)                  
                         TO WRK-PIC13V99                                
               MOVE WRK-PIC15-R                                         
                         TO RECRW810-VR-JUROS-REMUNERATOR(WRK-IND-DICD) 
      *#################################################################
                                                                        
      *##########  VALOR MORATORIO #####################################
               MOVE AVI-VALOR-MORATORIO(WRK-IND-DICD)                   
                           TO WRK-PIC13V99                              
               MOVE WRK-PIC15-R                                         
                           TO RECRW810-VR-JUROS-MORATORIOS(WRK-IND-DICD)
      *#################################################################
                                                                        
      *############ IOF  ###############################################
               MOVE AVI-IOF-NORMAL(WRK-IND-DICD)                        
                                       TO WRK-PIC13V99                  
               MOVE WRK-PIC15-R        TO RECRW810-VR-IOF(WRK-IND-DICD) 
      *#################################################################
                                                                        
      *###################### MULTA ####################################
               MOVE AVI-VALOR-MULTA(WRK-IND-DICD)                       
                                   TO WRK-PIC13V99                      
               MOVE WRK-PIC15-R    TO RECRW810-VR-MULTA(WRK-IND-DICD)   
      *#################################################################
                                                                        
      *##################### DESPESAS JUDICIAIS ########################
               MOVE AVI-DESP-JUD-CUSTAS(WRK-IND-DICD)                   
                            TO WRK-PIC11V99                             
               MOVE WRK-PIC13                                           
                            TO RECRW810-VR-DESP-JUDICIAIS(WRK-IND-DICD) 
      *#################################################################
                                                                        
      *###################  HONORARIOS  ################################
               MOVE AVI-HONORARIOS (WRK-IND-DICD)                       
                           TO WRK-PIC11V99                              
               MOVE WRK-PIC13                                           
                           TO RECRW810-VR-HONORARIOS(WRK-IND-DICD)      
      *#################################################################
                                                                        
      *####################  VALOR TAXA TARIFA  ########################
               MOVE AVI-VL-TAXA-TARIFA(WRK-IND-DICD)                    
                           TO WRK-PIC15V99                              
               MOVE WRK-PIC15                                           
                           TO RECRW810-VR-TAXA-TARIFA(WRK-IND-DICD)     
      *#################################################################
                                                                        
      *####################  TOTAL DA DIVIDA  ##########################
               MOVE AVI-VL-TOTAL-DIVIDA(WRK-IND-DICD)                   
                           TO WRK-PIC15V99                              
                              WRK-DIVIDA                                
               MOVE WRK-PIC15                                           
                           TO RECRW810-VR-VENCIDO-CONTRATO(WRK-IND-DICD)
      *#################################################################
               COMPUTE ACU-DIVIDA-TOTAL = ACU-DIVIDA-TOTAL + WRK-DIVIDA 
               IF AVI-NATUREZA(WRK-IND-DICD) EQUAL 'AR'                 
                   MOVE 1                   TO  RECRW810-IMPRIME-EMPRESA
               ELSE                                                     
                   MOVE 2                   TO  RECRW810-IMPRIME-EMPRESA
               END-IF                                                   
                                                                        
               ADD 1 TO WRK-IND-DICD                                    
           END-PERFORM.                                                 
                                                                        
           SUBTRACT 1 FROM WRK-IND-DICD                                 
           MOVE ACU-DIVIDA-TOTAL   TO WRK-PIC15V99                      
           MOVE WRK-PIC15      TO RECRW810-VR-TOTAL-ACUMULADO           
                                                                        
           MOVE WRK-IND-DICD TO WRK-IND-PIC9                            
           MOVE WRK-IND-PIC3 TO RECRW810-QTDE-OCORRENCIAS               
           MOVE WRK-AREA-RECRW810 TO ECORW392-PAGINA.                   
                                                                        
      *----------------------------------------------------------------*
       3106-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * PREENCHE DADOS DA ESTRUTURA PARA A CHAMADA DO ECOR             *
      *----------------------------------------------------------------*
       3107-MOVER-DICD-RECRW820        SECTION.                         
      *----------------------------------------------------------------*
                                                                        
      *#################################################################
      *' NOME DEVE SER DO DEVEDOR OU AVALISTA INFORMADO AO ECOR         
           MOVE AVI-NOME-DEVEDOR       TO RECRW820-NM-CLIENTE
      *#################################################################
                                                                        
           MOVE AVI-NUM-CC             TO WRK-AVI-NUM-CC                
           MOVE WRK-AVI-NUM-CC-X       TO RECRW820-CONTA-BCRIA          
                                                                        
           MOVE AVI-NOME-AGENCIA       TO RECRW820-NOME-AGENCIA         
           MOVE AVI-END-AGENCIA        TO RECRW820-ENDE-AGENCIA         
           MOVE AVI-MUNIC-AGENCIA      TO RECRW820-CIDADE-AGENCIA       
                                                                        
           MOVE ZEROS                  TO ACU-DIVIDA-TOTAL              
                                                                        
           STRING AVI-DATA-MVTO(7:2) '.'                                
                  AVI-DATA-MVTO(5:2) '.'                                
                  AVI-DATA-MVTO(1:4)                                    
           DELIMITED BY SIZE INTO RECRW820-DT-BASE                      
                                                                        
           STRING AVI-MUNIC-AGENCIA ' '                                 
                  RECRW820-DT-BASE                                      
           DELIMITED BY SIZE INTO RECRW820-LOCAL-DATA                   
                                                                        
                                                                        
           MOVE 1 TO WRK-IND-DICD                                       
                                                                        
           PERFORM UNTIL AVI-NATUREZA(WRK-IND-DICD) EQUAL SPACES OR     
                          WRK-IND-DICD EQUAL WRK-LIMIT-PAG              
                                                                        
               PERFORM 3150-LER-TABELA-DESCNAT                          
               MOVE WRK-DESCRIC TO    RECRW820-DS-PRODUTO(WRK-IND-DICD) 
                                                                        
               MOVE AVI-DAT-VENCTO(WRK-IND-DICD) TO WRK-DATA-COMP       
                                                                        
               STRING WRK-DIA-COMP '.'                                  
                       WRK-MES-COMP '.'                                 
                       WRK-ANO-COMP                                     
               DELIMITED BY SIZE INTO                                   
                       RECRW820-DT-VENCTO-PARC-ATRS(WRK-IND-DICD)       
                                                                        
               MOVE AVI-CONTRATO(WRK-IND-DICD)                          
                                   TO RECRW820-NU-CONTRATO(WRK-IND-DICD)
                                                                        
      *##########   VALOR PRINCIPAL ####################################
               MOVE AVI-RESGATE(WRK-IND-DICD)                           
                                  TO WRK-PIC11V99                       
               MOVE WRK-PIC11     TO RECRW820-VR-PRINCIPAL(WRK-IND-DICD)
      *#################################################################
                                                                        
      *##########   VALOR REMUNERATORIO ################################
               MOVE AVI-VR-REMUNERATORIO(WRK-IND-DICD)                  
                         TO WRK-PIC13V99                                
               MOVE WRK-PIC10-R                                         
                         TO RECRW820-VR-JUROS-REMUNERATOR(WRK-IND-DICD) 
      *#################################################################
                                                                        
      *##########  VALOR MORATORIO #####################################
               MOVE AVI-VALOR-MORATORIO(WRK-IND-DICD)                   
                           TO WRK-PIC13V99                              
               MOVE WRK-PIC10-R                                         
                           TO RECRW820-VR-JUROS-MORATORIOS(WRK-IND-DICD)
      *#################################################################
                                                                        
      *############ IOF  ###############################################
               MOVE AVI-IOF-NORMAL(WRK-IND-DICD)                        
                                       TO WRK-PIC13V99                  
               MOVE WRK-PIC10-R        TO RECRW820-VR-IOF(WRK-IND-DICD) 
      *#################################################################
                                                                        
      *###################### MULTA ####################################
               MOVE AVI-VALOR-MULTA(WRK-IND-DICD)                       
                                   TO WRK-PIC13V99                      
               MOVE WRK-PIC10-R    TO RECRW820-VR-MULTA(WRK-IND-DICD)   
      *#################################################################
                                                                        
      *##################### DESPESAS JUDICIAIS ########################
               MOVE AVI-DESP-JUD-CUSTAS(WRK-IND-DICD)                   
                            TO WRK-PIC11V99                             
               MOVE WRK-PIC10                                           
                            TO RECRW820-VR-DESP-JUDICIAIS(WRK-IND-DICD) 
      *#################################################################
                                                                        
      *###################  HONORARIOS  ################################
               MOVE AVI-HONORARIOS (WRK-IND-DICD)                       
                           TO WRK-PIC11V99                              
               MOVE WRK-PIC10                                           
                           TO RECRW820-VR-HONORARIOS(WRK-IND-DICD)      
      *#################################################################
                                                                        
      *####################  VALOR TAXA TARIFA  ########################
               MOVE AVI-VL-TAXA-TARIFA(WRK-IND-DICD)                    
                           TO WRK-PIC15V99                              
               MOVE WRK-PIC10-T                                         
                           TO RECRW820-VR-TAXA-TARIFA(WRK-IND-DICD)     
      *#################################################################
                                                                        
      *####################  TOTAL DA DIVIDA  ##########################
               MOVE AVI-VL-TOTAL-DIVIDA(WRK-IND-DICD)                   
                           TO WRK-PIC15V99                              
                              WRK-DIVIDA                                
               MOVE WRK-PIC12-T                                         
                           TO RECRW820-VR-VENCIDO-CONTRATO(WRK-IND-DICD)
      *#################################################################
                                                                        
                                                                        
                                                                        
                                                                        
               COMPUTE ACU-DIVIDA-TOTAL = ACU-DIVIDA-TOTAL + WRK-DIVIDA 
                                                                        
               IF AVI-NATUREZA(WRK-IND-DICD) EQUAL 'AR'                 
                 MOVE 1                   TO RECRW820-IMPRIME-EMPRESA   
               ELSE                                                     
                 MOVE 2                   TO RECRW820-IMPRIME-EMPRESA   
               END-IF                                                   
                                                                        
                                                                        
               ADD 1 TO WRK-IND-DICD                                    
           END-PERFORM.                                                 
                                                                        
           SUBTRACT 1 FROM WRK-IND-DICD                                 
           MOVE ACU-DIVIDA-TOTAL   TO WRK-PIC15V99                      
           MOVE WRK-PIC15          TO RECRW820-VR-TOTAL-ACUMULADO       
                                                                        
           MOVE WRK-IND-DICD TO WRK-IND-PIC9                            
           MOVE WRK-IND-PIC3 TO RECRW820-QTDE-OCORRENCIAS               
           MOVE WRK-AREA-RECRW820 TO ECORW392-PAGINA.                   
                                                                        
      *----------------------------------------------------------------*
       3107-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * PREENCHE DADOS DA ESTRUTURA PARA A CHAMADA DO ECOR             *
      *----------------------------------------------------------------*
       3108-MOVER-DICD-RECRW830        SECTION.                         
      *----------------------------------------------------------------*
                                                                        
      *#################################################################
      *' NOME DEVE SER DO DEVEDOR OU AVALISTA INFORMADO AO ECOR         
      *    MOVE AVI-NOME-DEVEDOR       TO RECRW830-NM-CLIENTE
           MOVE AVI-NOME-AVAL          TO RECRW830-NM-CLIENTE
      *#################################################################
                                                                        
                                                                        
           MOVE AVI-NUM-CC             TO WRK-AVI-NUM-CC                
           MOVE WRK-AVI-NUM-CC-X       TO RECRW830-CONTA-BCRIA          
           MOVE AVI-NOME-AGENCIA       TO RECRW830-NOME-AGENCIA         
           MOVE AVI-END-AGENCIA        TO RECRW830-ENDE-AGENCIA         
           MOVE AVI-MUNIC-AGENCIA      TO RECRW830-CIDADE-AGENCIA       
           MOVE ZEROS                  TO RECRW830-IMPRIME-PRESTAMISTA  
                                                                        
           MOVE ZEROS                  TO ACU-DIVIDA-TOTAL              
                                                                        
           STRING AVI-DATA-MVTO(7:2) '.'                                
                  AVI-DATA-MVTO(5:2) '.'                                
                  AVI-DATA-MVTO(1:4)                                    
           DELIMITED BY SIZE INTO RECRW830-DT-BASE                      
                                                                        
           STRING AVI-MUNIC-AGENCIA ' '                                 
                  RECRW830-DT-BASE                                      
           DELIMITED BY SIZE INTO RECRW830-LOCAL-DATA                   
                                                                        
                                                                        
           MOVE 1 TO WRK-IND-DICD                                       
                                                                        
           PERFORM UNTIL AVI-NATUREZA(WRK-IND-DICD) EQUAL SPACES OR     
                          WRK-IND-DICD EQUAL WRK-LIMIT-PAG              
                                                                        
               PERFORM 3150-LER-TABELA-DESCNAT                          
               MOVE WRK-DESCRIC TO    RECRW830-DS-PRODUTO(WRK-IND-DICD) 
                                                                        
               MOVE AVI-DAT-VENCTO(WRK-IND-DICD) TO WRK-DATA-COMP       
                                                                        
               STRING WRK-DIA-COMP '.'                                  
                       WRK-MES-COMP '.'                                 
                       WRK-ANO-COMP                                     
               DELIMITED BY SIZE INTO                                   
                       RECRW830-DT-VENCTO-PARC-ATRS(WRK-IND-DICD)       
                                                                        
               MOVE AVI-CONTRATO(WRK-IND-DICD)                          
                                   TO RECRW830-NU-CONTRATO(WRK-IND-DICD)
                                                                        
      *##########   VALOR PRINCIPAL ####################################
               MOVE AVI-RESGATE(WRK-IND-DICD)                           
                                  TO WRK-PIC11V99                       
               MOVE WRK-PIC11     TO RECRW830-VR-PRINCIPAL(WRK-IND-DICD)
      *#################################################################
                                                                        
      *##########   VALOR REMUNERATORIO ################################
               MOVE AVI-VR-REMUNERATORIO(WRK-IND-DICD)                  
                         TO WRK-PIC13V99                                
               MOVE WRK-PIC10-R                                         
                         TO RECRW830-VR-JUROS-REMUNERATOR(WRK-IND-DICD) 
      *#################################################################
                                                                        
      *##########  VALOR MORATORIO #####################################
               MOVE AVI-VALOR-MORATORIO(WRK-IND-DICD)                   
                           TO WRK-PIC13V99                              
               MOVE WRK-PIC10-R                                         
                           TO RECRW830-VR-JUROS-MORATORIOS(WRK-IND-DICD)
      *#################################################################
                                                                        
      *############ IOF  ###############################################
               MOVE AVI-IOF-NORMAL(WRK-IND-DICD)                        
                                       TO WRK-PIC13V99                  
               MOVE WRK-PIC10-R        TO RECRW830-VR-IOF(WRK-IND-DICD) 
      *#################################################################
                                                                        
      *###################### MULTA ####################################
               MOVE AVI-VALOR-MULTA(WRK-IND-DICD)                       
                                   TO WRK-PIC13V99                      
               MOVE WRK-PIC10-R    TO RECRW830-VR-MULTA(WRK-IND-DICD)   
      *#################################################################
                                                                        
      *##################### DESPESAS JUDICIAIS ########################
               MOVE AVI-DESP-JUD-CUSTAS(WRK-IND-DICD)                   
                            TO WRK-PIC11V99                             
               MOVE WRK-PIC10                                           
                            TO RECRW830-VR-DESP-JUDICIAIS(WRK-IND-DICD) 
      *#################################################################
                                                                        
      *###################  HONORARIOS  ################################
               MOVE AVI-HONORARIOS (WRK-IND-DICD)                       
                           TO WRK-PIC11V99                              
               MOVE WRK-PIC10                                           
                           TO RECRW830-VR-HONORARIOS(WRK-IND-DICD)      
      *#################################################################
                                                                        
      *####################  VALOR TAXA TARIFA  ########################
               MOVE AVI-VL-TAXA-TARIFA(WRK-IND-DICD)                    
                           TO WRK-PIC15V99                              
               MOVE WRK-PIC10-T                                         
                           TO RECRW830-VR-TAXA-TARIFA(WRK-IND-DICD)     
      *#################################################################
                                                                        
      *####################  TOTAL DA DIVIDA  ##########################
               MOVE AVI-VL-TOTAL-DIVIDA(WRK-IND-DICD)                   
                           TO WRK-PIC15V99                              
                              WRK-DIVIDA                                
               MOVE WRK-PIC12-T                                         
                           TO RECRW830-VR-VENCIDO-CONTRATO(WRK-IND-DICD)
      *#################################################################
                                                                        
                                                                        
                                                                        
               COMPUTE ACU-DIVIDA-TOTAL = ACU-DIVIDA-TOTAL + WRK-DIVIDA 
                                                                        
               IF AVI-NATUREZA(WRK-IND-DICD)             EQUAL 'AR'     
                   MOVE 1                   TO  RECRW830-IMPRIME-EMPRESA
               ELSE                                                     
                   MOVE 2                   TO  RECRW830-IMPRIME-EMPRESA
               END-IF                                                   
                                                                        
               IF AVI-POSSIBILIT(WRK-IND-DICD)           EQUAL '*'      
                   MOVE 1    TO  RECRW830-IMPRIME-PRESTAMISTA           
               END-IF                                                   
                                                                        
               ADD 1 TO WRK-IND-DICD                                    
           END-PERFORM.                                                 
                                                                        
           SUBTRACT 1 FROM WRK-IND-DICD                                 
                                                                        
           MOVE ACU-DIVIDA-TOTAL    TO WRK-PIC15V99                     
           MOVE WRK-PIC15           TO RECRW830-VR-TOTAL-ACUMULADO      
                                                                        
           MOVE WRK-IND-DICD TO WRK-IND-PIC9                            
           MOVE WRK-IND-PIC3 TO RECRW830-QTDE-OCORRENCIAS               
           MOVE WRK-AREA-RECRW830 TO ECORW392-PAGINA.                   
                                                                        
      *----------------------------------------------------------------*
       3108-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * PREENCHE DADOS DA ESTRUTURA PARA A CHAMADA DO ECOR             *
      *----------------------------------------------------------------*
       3109-MOVER-DICD-RECRW840        SECTION.                         
      *----------------------------------------------------------------*
                                                                        
      *#################################################################
      *' NOME DEVE SER DO DEVEDOR OU AVALISTA INFORMADO AO ECOR         
      *    MOVE AVI-NOME-DEVEDOR       TO RECRW840-NM-CLIENTE
           MOVE AVI-NOME-AVAL          TO RECRW840-NM-CLIENTE
      *#################################################################
                                                                        
           MOVE ZEROS                  TO RECRW840-IMPRIME-PRESTAMISTA  
           MOVE ZEROS                  TO ACU-DIVIDA-TOTAL              
                                                                        
           STRING AVI-DATA-MVTO(7:2) '.'                                
                  AVI-DATA-MVTO(5:2) '.'                                
                  AVI-DATA-MVTO(1:4)                                    
           DELIMITED BY SIZE INTO RECRW840-DT-BASE                      
                                                                        
           STRING AVI-MUNIC-AGENCIA ' '                                 
                  RECRW840-DT-BASE                                      
           DELIMITED BY SIZE INTO RECRW840-LOCAL-DATA                   
                                                                        
           MOVE 1 TO WRK-IND-DICD                                       
                                                                        
           PERFORM UNTIL AVI-NATUREZA(WRK-IND-DICD) EQUAL SPACES OR     
                          WRK-IND-DICD EQUAL WRK-LIMIT-PAG              
                                                                        
               PERFORM 3150-LER-TABELA-DESCNAT                          
               MOVE WRK-DESCRIC TO    RECRW840-DS-PRODUTO(WRK-IND-DICD) 
                                                                        
               MOVE AVI-DAT-VENCTO(WRK-IND-DICD) TO WRK-DATA-COMP       
                                                                        
               STRING WRK-DIA-COMP '.'                                  
                       WRK-MES-COMP '.'                                 
                       WRK-ANO-COMP                                     
               DELIMITED BY SIZE INTO                                   
                       RECRW840-DT-VENCTO-PARC-ATRS(WRK-IND-DICD)       
                                                                        
               MOVE AVI-CONTRATO(WRK-IND-DICD)                          
                                   TO RECRW840-NU-CONTRATO(WRK-IND-DICD)
                                                                        
      *##########   VALOR PRINCIPAL ####################################
               MOVE AVI-RESGATE(WRK-IND-DICD)                           
                                  TO WRK-PIC11V99                       
               MOVE WRK-PIC11     TO RECRW840-VR-PRINCIPAL(WRK-IND-DICD)
      *#################################################################
                                                                        
      *##########   VALOR REMUNERATORIO ################################
               MOVE AVI-VR-REMUNERATORIO(WRK-IND-DICD)                  
                         TO WRK-PIC13V99                                
               MOVE WRK-PIC10-R                                         
                         TO RECRW840-VR-JUROS-REMUNERATOR(WRK-IND-DICD) 
      *#################################################################
                                                                        
      *##########  VALOR MORATORIO #####################################
               MOVE AVI-VALOR-MORATORIO(WRK-IND-DICD)                   
                           TO WRK-PIC13V99                              
               MOVE WRK-PIC10-R                                         
                           TO RECRW840-VR-JUROS-MORATORIOS(WRK-IND-DICD)
      *#################################################################
                                                                        
      *############ IOF  ###############################################
               MOVE AVI-IOF-NORMAL(WRK-IND-DICD)                        
                                       TO WRK-PIC13V99                  
               MOVE WRK-PIC10-R        TO RECRW840-VR-IOF(WRK-IND-DICD) 
      *#################################################################
                                                                        
      *###################### MULTA ####################################
               MOVE AVI-VALOR-MULTA(WRK-IND-DICD)                       
                                   TO WRK-PIC13V99                      
               MOVE WRK-PIC10-R    TO RECRW840-VR-MULTA(WRK-IND-DICD)   
      *#################################################################
                                                                        
      *##################### DESPESAS JUDICIAIS ########################
               MOVE AVI-DESP-JUD-CUSTAS(WRK-IND-DICD)                   
                            TO WRK-PIC11V99                             
               MOVE WRK-PIC10                                           
                            TO RECRW840-VR-DESP-JUDICIAIS(WRK-IND-DICD) 
      *#################################################################
                                                                        
      *###################  HONORARIOS  ################################
               MOVE AVI-HONORARIOS (WRK-IND-DICD)                       
                           TO WRK-PIC11V99                              
               MOVE WRK-PIC10                                           
                           TO RECRW840-VR-HONORARIOS(WRK-IND-DICD)      
      *#################################################################
                                                                        
      *####################  VALOR TAXA TARIFA  ########################
               MOVE AVI-VL-TAXA-TARIFA(WRK-IND-DICD)                    
                           TO WRK-PIC15V99                              
               MOVE WRK-PIC10-T                                         
                           TO RECRW840-VR-TAXA-TARIFA(WRK-IND-DICD)     
      *#################################################################
                                                                        
      *####################  TOTAL DA DIVIDA  ##########################
               MOVE AVI-VL-TOTAL-DIVIDA(WRK-IND-DICD)                   
                           TO WRK-PIC15V99                              
                              WRK-DIVIDA                                
               MOVE WRK-PIC12-T                                         
                           TO RECRW840-VR-VENCIDO-CONTRATO(WRK-IND-DICD)
      *#################################################################
                                                                        
                                                                        
               COMPUTE ACU-DIVIDA-TOTAL = ACU-DIVIDA-TOTAL + WRK-DIVIDA 
                                                                        
               IF AVI-NATUREZA(WRK-IND-DICD)             EQUAL 'AR'     
                   MOVE 1                   TO  RECRW840-IMPRIME-EMPRESA
               ELSE                                                     
                   MOVE 2                   TO  RECRW840-IMPRIME-EMPRESA
               END-IF                                                   
                                                                        
               IF AVI-POSSIBILIT(WRK-IND-DICD)           EQUAL '*'      
                   MOVE 1    TO  RECRW840-IMPRIME-PRESTAMISTA           
               END-IF                                                   
                                                                        
               ADD 1 TO WRK-IND-DICD                                    
           END-PERFORM.                                                 
                                                                        
           SUBTRACT 1 FROM WRK-IND-DICD                                 
                                                                        
           MOVE ACU-DIVIDA-TOTAL   TO WRK-PIC15V99                      
           MOVE WRK-PIC15          TO RECRW840-VR-TOTAL-ACUMULADO       
                                                                        
           MOVE WRK-IND-DICD TO WRK-IND-PIC9                            
           MOVE WRK-IND-PIC3 TO RECRW840-QTDE-OCORRENCIAS               
           MOVE WRK-AREA-RECRW840 TO ECORW392-PAGINA.                   
                                                                        
      *----------------------------------------------------------------*
       3109-99-FIM.                    EXIT.                            
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
                                                                        
           MOVE AVI-NRO    TO WRK-NUMERO.                               
                                                                        
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
                                                                        
           MOVE AVI-ENDER                                               
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
                                                                        
           MOVE AVI-NRO    TO WRK-NUMERO.                               
                                                                        
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
                                                                        
           MOVE AVI-ENDER(1:25)                                         
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
                  AVI-COMPL(1:WRK-FNAL-COMPL)                           
                  DELIMITED BY SIZE INTO WRK-STRING                     
           END-STRING.                                                  
                                                                        
      *----------------------------------------------------------------*
       3120-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *---------------------------------------------------------------* 
       3150-LER-TABELA-DESCNAT  SECTION.                                
      *---------------------------------------------------------------* 
                                                                        
           EVALUATE AVI-NATUREZA(WRK-IND-DICD)                          
               WHEN  'CH'                                               
              MOVE 'ADIANT. DEPOSITANTES' TO WRK-DESCRIC                
               WHEN  'AD'                                               
              MOVE 'AD. DEPOS. CLIENTES ' TO WRK-DESCRIC                
               WHEN  'AR'                                               
              MOVE 'ARRENDAMENTOS       ' TO WRK-DESCRIC                
               WHEN  'FI'                                               
              MOVE 'CREDITOS E FINANC.  ' TO WRK-DESCRIC                
               WHEN  'EC'                                               
              MOVE 'EMPRESTIMOS EM CONTA' TO WRK-DESCRIC                
               WHEN  'AG'                                               
              MOVE 'EMPR. AGRIC. E IND. ' TO WRK-DESCRIC                
               WHEN  'CA'                                               
              MOVE 'OPERACOES DE CAMBIO ' TO WRK-DESCRIC                
               WHEN  'RE'                                               
              MOVE 'OPERAC. DE REPASSES ' TO WRK-DESCRIC                
               WHEN  'IM'                                               
              MOVE 'OPERAC. IMOBILIARIAS' TO WRK-DESCRIC                
               WHEN  'TD'                                               
              MOVE 'TITULOS DESCONTADOS ' TO WRK-DESCRIC                
               WHEN  'DC'                                               
              MOVE 'DESCONTO DE CUEQUES ' TO WRK-DESCRIC                
               WHEN  'CT'                                               
              MOVE 'CARTAO DE CREDITO   ' TO WRK-DESCRIC                
               WHEN  'OO'                                               
              MOVE 'OUTRAS OPERACOES    ' TO WRK-DESCRIC                
               WHEN OTHER                                               
              MOVE 'OPERACOES DIVERSAS  ' TO WRK-DESCRIC                
           END-EVALUATE.                                                
                                                                        
      *---------------------------------------------------------------* 
       3150-99-FIM. EXIT.                                               
      *---------------------------------------------------------------* 
                                                                        
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
                                                                        
           MOVE  REG-AVISO             TO  FD-SMODLAVS.                 
                                                                        
           WRITE FD-SMODLAVS.                                           
                                                                        
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
               DISPLAY '************* PROGRAMA CLLP2955 *************'  
               DISPLAY '*                                           *'  
               DISPLAY '*  PROGRAMA CLLP2955 ENCERRADO COM SUCESSO  *'  
               DISPLAY '*                                           *'  
               DISPLAY '************* PROGRAMA CLLP2955 *************'  
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
                                                                        
           DISPLAY '************* PROGRAMA CLLP2955 *************'.     
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
               DISPLAY 'CLLP2955 - PROCESSAMENTO COM RESTART'           
               MOVE CK01-AREA-RESTART(1:CK01-TAM-AREA-RESTART)          
                                            TO WRK-AREA-RESTART         
               MOVE WRK-LIDOS-EMODLAVS-RST  TO ACU-LIDOS-EMODLAVS       
               MOVE WRK-GRAVA-SMODLAVS-RST  TO ACU-GRAVADOS-SMODLAVS    
           ELSE                                                         
               DISPLAY 'CLLP2955 - PROCESSAMENTO NORMAL (SEM RESTART)'  
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
                                                                        
           IF (CK01-CODIGO-RETORNO         NOT EQUAL ZEROS)             
               IF (CK01-CODIGO-RETORNO         EQUAL 12)                
                   GOBACK                                               
               ELSE                                                     
                   MOVE  12            TO  FRWKGMOD-COD-RETORNO         
                   MOVE  WRK-LOCAL     TO  FRWKGMOD-COD-ERRO            
                   MOVE  CK01-CODIGO-MENSAGEM                           
                                       TO  FRWKGMOD-COD-MENSAGEM        
                   MOVE  WRK-CKRS0100  TO  FRWKGMOD-NOME-MODULO         
                   DISPLAY                                              
                   '***************** CLLP2955 *****************'       
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
                   '***************** CLLP2955 *****************'       
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

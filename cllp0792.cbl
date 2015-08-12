      *================================================================*
       IDENTIFICATION                  DIVISION.                        
      *================================================================*
       PROGRAM-ID.        CLLP0792.                                     
       AUTHOR.            BSI.                                          
      *================================================================*
      *                           * B S I *                            *
      *----------------------------------------------------------------*
      *    PROGRAMA....:  CLLP0792                                     *
      *    DATA........:  ABRIL / 2013                                 *
      *    PROGRAMADOR.:  LUIS CARLOS KLENK                            *
      *    ANALISTA....:  MARCUS VINICIUS CURTO                        *
      *----------------------------------------------------------------*
      *    OBJETIVO....:  GERAR ARQUIVO DE NAOEMITE PARA CEDD          *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:  DDNAME    | E/S  |INCLUDE/BOOK | TAMANHO     *
      *                   AVISOENT     E      I#CLLPLE       600       *
      *                   AVISOSAI     S      I#CLLPLE       600       *
      *                   NAOEMITE     S      I#CLLPZF       350       *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:  DB2                                          *
      *                   TABLE                    INCLUDE/BOOK        *
      *                   NAO HA                                       *
      *----------------------------------------------------------------*
      *    MODULOS.....:  CEPN5060 - VALIDA ENDERECO                   *
      *                   FRWK2999 - MODULO PARA TRATAMENTO DE ERROS   *
      *                   BRAD0450 - MODULO PARA ABEND DO PROGRAMA     *
      *================================================================*
       ENVIRONMENT                     DIVISION.                        
      *================================================================*
      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.                         
      *----------------------------------------------------------------*
       SPECIAL-NAMES.                                                   
           DECIMAL-POINT IS COMMA.                                      
      *----------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.                         
      *----------------------------------------------------------------*
                                                                        
       FILE-CONTROL.                                                    
                                                                        
           SELECT  AVISOENT  ASSIGN    TO   UT-S-AVISOENT               
                      FILE   STATUS    IS   WRK-FS-AVISOENT.            
                                                                        
           SELECT  AVISOSAI  ASSIGN    TO   UT-S-AVISOSAI               
                      FILE   STATUS    IS   WRK-FS-AVISOSAI.            
                                                                        
           SELECT  NAOEMITE  ASSIGN    TO   UT-S-NAOEMITE               
                      FILE   STATUS    IS   WRK-FS-NAOEMITE.            
                                                                        
      *----------------------------------------------------------------*
      *================================================================ 
       DATA                            DIVISION.                        
      *================================================================*
      *----------------------------------------------------------------*
       FILE                            SECTION.                         
      *----------------------------------------------------------------*
      *  INPUT:  AVISOENT - ARQUIVO DE AVISOS              - LRECL=600  
      *                        -*- I#CLLPLE -*-                        *
      *----------------------------------------------------------------*
       FD  AVISOENT                                                     
           LABEL RECORD STANDARD                                        
           RECORDING MODE IS F                                          
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  FD-REG-AVISOENT                 PIC  X(600).                 
      *----------------------------------------------------------------*
      *  OUTPUT: AVISOSAI - ARQUIVO DE AVISOS OK           - LRECL=600 *
      *                        -*- I#CLLPLE -*-                        *
      *----------------------------------------------------------------*
       FD  AVISOSAI                                                     
           LABEL RECORD STANDARD                                        
           RECORDING MODE IS F                                          
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  FD-REG-AVISOSAI                 PIC  X(600).                 
      *----------------------------------------------------------------*
      *  OUTPUT: NAOEMITE- ARQUIVO DE NAO EMITIDOS         - LRECL=350 *
      *                        -*- I#CLLPZF -*-                        *
      *----------------------------------------------------------------*
       FD  NAOEMITE                                                     
           LABEL RECORD STANDARD                                        
           RECORDING MODE IS F                                          
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  FD-REG-NAOEMITE                 PIC  X(350).                 
      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.                         
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       77  FILLER PIC X(33) VALUE  '* CLLP0792 - INICIO DA WORKING  *'. 
      *----------------------------------------------------------------*
      *                                                                 
       77  WRK-FS-AVISOENT             PIC  X(002) VALUE SPACES.        
       77  WRK-FS-NAOEMITE             PIC  X(002) VALUE SPACES.        
       77  WRK-FS-AVISOSAI             PIC  X(002) VALUE SPACES.        
       77  WRK-REG-OK                  PIC  X(001) VALUE 'N'.           
       77  WRK-SIT-CORSP               PIC  9(003)         VALUE ZEROS. 
       77  ACU-LIDOS-AVISOENT          PIC  9(009) COMP-3  VALUE ZEROS. 
       77  ACU-GRV-NAOEMITE            PIC  9(009) COMP-3  VALUE ZEROS. 
       77  ACU-GRV-AVISOSAI            PIC  9(009) COMP-3  VALUE ZEROS. 
       77  WRK-PROGRAMA                PIC  X(008) VALUE 'CLLP0792'.    
       77  WRK-CEPN5060                PIC  X(008) VALUE 'CEPN5060'.    
       77  WRK-FRWK2999                PIC  X(008) VALUE 'FRWK2999'.    
       77  WRK-NOME-MODULO             PIC  X(008)         VALUE SPACES.
       77  WRK-DESCOMP-5               PIC  9(005)         VALUE ZEROS. 
       77  WRK-DESCOMP-3               PIC  9(003)         VALUE ZEROS. 
                                                                        
      *----------------------------------------------------------------*
      *               '* AREA PARA O ARQUIVO AVISOENT *'               *
      *                  '* LRECL = 600 BYTE(S) *'                     *
      *----------------------------------------------------------------*
       COPY 'I#CLLPLE'.                                                 
                                                                        
      *----------------------------------------------------------------*
      *               '* AREA PARA O ARQUIVO NAOEMITE *'               *
      *                  '* LRECL = 350 BYTE(S) *'                     *
      *----------------------------------------------------------------*
       COPY 'I#CLLPZF'.                                                 
                                                                        
      *----------------------------------------------------------------*
      *           '* AREA PARA ACESSO AO MODULO CEPN5060 *'            *
      *                  '* LRECL = NNN BYTE(S) *'                     *
      *----------------------------------------------------------------*
       COPY 'I#CEPN31'.                                                 
      *----------------------------------------------------------------*
      *                                                                 
       01  WRK-ERRO-AREA               PIC  X(107) VALUE SPACES.        
       01  WRK-SQLCA                   PIC  X(136) VALUE SPACES.        
                                                                        
       01 WRK-NRO-ENDE-X7              PIC X(007)  VALUE SPACES.        
       01 WRK-NRO-ENDE-R               REDEFINES   WRK-NRO-ENDE-X7.     
          03  FILLER                   PIC X(002).                      
          03  WRK-NRO-ENDE-X5          PIC X(005).                      
                                                                        
       01 WRK-NRO-ENDE-X               PIC X(005)  VALUE SPACES.        
       01 WRK-NRO-ENDE                 REDEFINES   WRK-NRO-ENDE-X       
                                       PIC 9(005).                      
                                                                        
       01  WRK-CTPO-LOGDR.                                              
           03  WRK-CTPO-LOGDR-COM2     PIC  X(002) VALUE SPACES.        
           03  FILLER                  PIC  X(003) VALUE SPACES.        
                                                                        
      *                                                                 
      *---------------------------------------------------------------* 
      *        AREA DE USO DA BRAD7600 (DATA-HORA  SISTEMA)           * 
      *---------------------------------------------------------------* 
       01  WRK-DATA-HORA.                                               
           03  WRK-DATA-JUL            PIC 9(05) COMP-3 VALUE ZEROS.    
           03  WRK-DATA-AAMMDD         PIC 9(07) COMP-3 VALUE ZEROS.    
           03  WRK-DATA-AAAAMMDD       PIC 9(09) COMP-3 VALUE ZEROS.    
           03  WRK-HORA-HHMMSS         PIC 9(07) COMP-3 VALUE ZEROS.    
           03  WRK-HORA-HHMMSSMMMMM    PIC 9(13) COMP-3 VALUE ZEROS.    
           03  WRK-TIMESTAMP           PIC X(20)        VALUE SPACES.   
                                                                        
       01  WRK-DATA-INV-C              PIC 9(09) COMP-3 VALUE ZEROS.    
                                                                        
       01  WRK-DATA-INV                PIC 9(09)        VALUE ZEROS.    
       01  FILLER  REDEFINES  WRK-DATA-INV.                             
           03  FILLER                  PIC X(01).                       
           03  WRK-ANO-INV             PIC 9(04).                       
           03  WRK-MES-INV             PIC 9(02).                       
           03  WRK-DIA-INV             PIC 9(02).                       
                                                                        
       01  WRK-DATA-DB2.                                                
           03  WRK-DIA-DB2             PIC 9(02)  VALUE ZEROS.          
           03  FILLER                  PIC X(01)  VALUE  '.'.           
           03  WRK-MES-DB2             PIC 9(02)  VALUE ZEROS.          
           03  FILLER                  PIC X(01)  VALUE  '.'.           
           03  WRK-ANO-DB2             PIC 9(04)  VALUE ZEROS.          
                                                                        
       01 FILLER    PIC X(32) VALUE '*    LINHAS DE DISPLAYS        *'. 
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
       01 WRK-MASCARA                  PIC BZ.ZZZ.ZZZ.ZZ9  VALUE ZEROS. 
                                                                        
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
       01 FILLER PIC X(32) VALUE '*      AREA DE MENSAGENS      *'.     
      *-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
                                                                        
       01  WRK-TRATAMENTO-ERROS.                                        
           05  WRK-BLOCO-RETORNO.                                       
             10  WRK-COD-RETORNO       PIC 9(002)      VALUE ZEROS.     
             10  WRK-COD-ERRO          PIC X(004)      VALUE SPACES.    
             10  WRK-COD-MENSAGEM      PIC X(008)      VALUE SPACES.    
                                                                        
       01  WRK-ABEND                   PIC  X(001)     VALUE SPACES.    
           88  WRK-ABENDAR                             VALUE 'S'.       
                                                                        
      *                                                                 
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)     VALUE            
           'AREA DE COMUNICACAO DO PROGRAMA FRWK2999'.                  
      *----------------------------------------------------------------*
                                                                        
       01  WRK-AREA-ERRO.                                               
           COPY 'I#FRWKGE'.                                             
           05  WRK-BLOCO-INFO-ERRO                     VALUE SPACES.    
             10  WRK-CHAR-INFO-ERRO    PIC X(001) OCCURS 0              
                                       TO 30000 TIMES                   
                                       DEPENDING ON FRWKGHEA-TAM-DADOS. 
                                                                        
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)     VALUE            
           'AREA PARA TRATAMENTO DE ERROS ARQUIVO'.                     
      *----------------------------------------------------------------*
       01  WRK-FRWKGARQ.                                                
           COPY 'I#FRWKAR'.                                             
                                                                        
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)     VALUE            
           '* AREA DE TRATAMENTO DE ERROS DE MODULO *'.                 
      *----------------------------------------------------------------*
                                                                        
       01  WRK-FRWKGMOD.                                                
           COPY 'I#FRWKMD'.                                             
                                                                        
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)     VALUE            
           'AREA DA BRAD0450'.                                          
      *----------------------------------------------------------------*
                                                                        
       01  WRK-ABEND-0450              PIC S9(004) COMP    VALUE +1111. 
       01  WRK-DUMP-0450               PIC  X(001)         VALUE 'S'.   
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER   PIC  X(32) VALUE '*   FIM DA WORKING CLLP0792    *'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
      *================================================================*
       PROCEDURE                       DIVISION.                        
      *================================================================*
                                                                        
      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           PERFORM 1000-INICIALIZAR.                                    
           PERFORM 2000-VERIFICAR-VAZIO.                                
                                                                        
           PERFORM 3000-PROCESSAR      UNTIL WRK-FS-AVISOENT            
                                       EQUAL '10'.                      
                                                                        
           PERFORM 5000-FINALIZAR.                                      
                                                                        
      *----------------------------------------------------------------*
       0000-99-EXIT.                   EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE '1000-INICIALIZAR'     TO FRWKGHEA-IDEN-PARAGRAFO.      
                                                                        
           CALL    'BRAD1050'.                                          
                                                                        
           INITIALIZE  FRWKGHEA-REGISTRO                                
                       FRWKGARQ-REGISTRO                                
                       FRWKGMOD-REGISTRO.                               
                                                                        
           OPEN INPUT  AVISOENT                                         
                OUTPUT AVISOSAI                                         
                       NAOEMITE.                                        
                                                                        
           INITIALIZE CLLPZF-AREA.                                      
                                                                        
           SET ARQ-OPEN                TO TRUE.                         
                                                                        
           PERFORM 1100-TESTAR-FILE-STATUS.                             
      *                                                                 
      *----------------------------------------------------------------*
       1000-99-EXIT.                   EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS         SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           PERFORM 1110-TESTAR-FS-AVISOENT.                             
                                                                        
           PERFORM 1120-TESTAR-FS-NAOEMITE.                             
                                                                        
           PERFORM 1130-TESTAR-FS-AVISOSAI.                             
                                                                        
      *----------------------------------------------------------------*
       1100-99-EXIT.                   EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       1110-TESTAR-FS-AVISOENT         SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           IF WRK-FS-AVISOENT          NOT EQUAL '00' AND '10'          
             MOVE WRK-FS-AVISOENT      TO FRWKGARQ-FILE-STATUS          
             MOVE 'AVISOENT'           TO FRWKGARQ-NOME-ARQUIVO         
             SET WRK-ABENDAR           TO TRUE                          
             PERFORM 9000-ERRO-ARQ                                      
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       1110-99-EXIT.                   EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       1120-TESTAR-FS-NAOEMITE         SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           IF WRK-FS-NAOEMITE          NOT EQUAL '00'                   
             MOVE WRK-FS-NAOEMITE      TO FRWKGARQ-FILE-STATUS          
             MOVE 'NAOEMITE'           TO FRWKGARQ-NOME-ARQUIVO         
             SET WRK-ABENDAR           TO TRUE                          
             PERFORM 9000-ERRO-ARQ                                      
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       1120-99-EXIT.                   EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       1130-TESTAR-FS-AVISOSAI         SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           IF WRK-FS-AVISOSAI          NOT EQUAL '00'                   
             MOVE WRK-FS-AVISOSAI      TO FRWKGARQ-FILE-STATUS          
             MOVE 'AVISOSAI'           TO FRWKGARQ-NOME-ARQUIVO         
             SET WRK-ABENDAR           TO TRUE                          
             PERFORM 9000-ERRO-ARQ                                      
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       1130-99-EXIT.                   EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       2000-VERIFICAR-VAZIO            SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           PERFORM 2100-LER-AVISOENT.                                   
                                                                        
           IF ACU-LIDOS-AVISOENT EQUAL ZEROS                            
             DISPLAY '*********** CLLP0792 ***********'                 
             DISPLAY '*                              *'                 
             DISPLAY '*   ARQUIVO AVISOENT  VAZIO    *'                 
             DISPLAY '*                              *'                 
             DISPLAY '*********** CLLP0792 ***********'                 
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       2000-99-EXIT.                   EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       2100-LER-AVISOENT               SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE '2100-LER-AVISOENT'    TO FRWKGHEA-IDEN-PARAGRAFO       
                                                                        
           READ AVISOENT.                                               
                                                                        
           SET ARQ-READ                TO TRUE.                         
           PERFORM 1110-TESTAR-FS-AVISOENT.                             
                                                                        
           IF WRK-FS-AVISOENT          EQUAL '00'                       
             MOVE FD-REG-AVISOENT      TO REG-AVISO                     
             ADD 1                     TO ACU-LIDOS-AVISOENT            
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       2100-99-EXIT.                   EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE '3000-PROCESSAR'       TO FRWKGHEA-IDEN-PARAGRAFO.      
                                                                        
           PERFORM 4000-TRATAR-REGISTRO.                                
                                                                        
           IF WRK-REG-OK               EQUAL 'S'                        
             PERFORM 3300-GRAVAR-AVISOSAI                               
           ELSE                                                         
             PERFORM 3100-MONTAR-NAOEMITE                               
             PERFORM 3200-GRAVAR-NAOEMITE                               
           END-IF.                                                      
                                                                        
           PERFORM 2100-LER-AVISOENT.                                   
                                                                        
      *----------------------------------------------------------------*
       3000-99-EXIT.                   EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       3100-MONTAR-NAOEMITE            SECTION.                         
      *----------------------------------------------------------------*
      *                                                                 
           MOVE SPACES                 TO CLLPZF-CINFO-FRANQ-CORSP      
                                          CLLPZF-DPOSTAGEM-CORSP.       
                                                                        
           INITIALIZE WRK-DATA-HORA.                                    
                                                                        
           CALL  'BRAD7600'  USING  WRK-DATA-HORA.                      
                                                                        
           MOVE WRK-DATA-AAAAMMDD      TO WRK-DATA-INV-C.               
           MOVE WRK-DATA-INV-C         TO WRK-DATA-INV.                 
           MOVE WRK-DIA-INV            TO WRK-DIA-DB2.                  
           MOVE WRK-MES-INV            TO WRK-MES-DB2.                  
           MOVE WRK-ANO-INV            TO WRK-ANO-DB2.                  
           MOVE WRK-DATA-DB2           TO CLLPZF-DFAC-CORSP.            
                                                                        
           MOVE 04120                  TO CLLPZF-GESTOR.                
           MOVE 'CLLP0793'             TO CLLPZF-ROTINA.                
           MOVE 237                    TO CLLPZF-CBCO-COBR-TARIF.       
           MOVE AVI-AGENCIA            TO CLLPZF-COD-AGE-CLI.           
           MOVE AVI-NUM-CC             TO CLLPZF-CONTA-COR-CLI.         
                                                                        
           IF CLLPZF-CD-FILIAL-CNPJ    EQUAL ZEROS                      
             MOVE 'F'                  TO CLLPZF-TIPO-CLIENTE           
           ELSE                                                         
             MOVE 'J'                  TO CLLPZF-TIPO-CLIENTE           
           END-IF.                                                      
                                                                        
           MOVE AVI-CGC-AVAL           TO CLLPZF-CD-CPF-CNPJ.           
           MOVE AVI-FIL-AVAL           TO WRK-DESCOMP-5.                
           MOVE WRK-DESCOMP-5(2:4)     TO CLLPZF-CD-FILIAL-CNPJ.        
           MOVE AVI-CTR-AVAL           TO WRK-DESCOMP-3.                
           MOVE WRK-DESCOMP-3(2:2)     TO CLLPZF-CONTROLE-CPF-CNPJ.     
                                                                        
           MOVE AVI-NOME-AVAL          TO CLLPZF-ICLI-CORSP .           
           MOVE AVI-ENDER              TO CLLPZF-ILOGDR-CLI-CORSP.      
           MOVE AVI-BAIRRO             TO CLLPZF-IBAIRO-CLI-CORSP.      
           MOVE AVI-CIDADE             TO CLLPZF-IMUN-CLI-CORSP.        
           MOVE AVI-UF                 TO CLLPZF-CUF-CLI-CORSP.         
           MOVE AVI-CCEP               TO CLLPZF-CCEP-CLI-CORSP.        
           MOVE AVI-CCEP-COMPL         TO CLLPZF-CCOMPL-CEP-CLI.        
                                                                        
           MOVE ZEROS                  TO CLLPZF-CLUB                   
                                          CLLPZF-SEQ-ENDER              
                                          CLLPZF-CPSSOA-JURID-CONTR     
                                          CLLPZF-CTPO-CONTR-NEGOC       
                                          CLLPZF-NSEQ-CONTR-NEGOC       
                                          CLLPZF-CIDTFD-DOCTO-GERDR.    
                                                                        
           MOVE 'CLIE'                 TO CLLPZF-BASE-ENDER.            
           MOVE 'CLLP'                 TO CLLPZF-CCUSTO-ORIGEM.         
                                                                        
           MOVE WRK-SIT-CORSP          TO CLLPZF-SIT-CORSP.             
      *                                                                 
      *----------------------------------------------------------------*
       3100-99-EXIT.                   EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       3200-GRAVAR-NAOEMITE            SECTION.                         
      *----------------------------------------------------------------*
      *                                                                 
           MOVE '3200-GRAVAR-NAOEMITE' TO FRWKGHEA-IDEN-PARAGRAFO       
                                                                        
           WRITE FD-REG-NAOEMITE       FROM CLLPZF-AREA.                
                                                                        
           SET ARQ-WRITE               TO TRUE.                         
                                                                        
           PERFORM 1120-TESTAR-FS-NAOEMITE.                             
                                                                        
           ADD 1                       TO ACU-GRV-NAOEMITE.             
      *                                                                 
      *----------------------------------------------------------------*
       3200-99-EXIT.                   EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       3300-GRAVAR-AVISOSAI            SECTION.                         
      *----------------------------------------------------------------*
      *                                                                 
           MOVE '3300-GRAVAR-AVISOSAI' TO FRWKGHEA-IDEN-PARAGRAFO       
                                                                        
           WRITE FD-REG-AVISOSAI       FROM REG-AVISO.                  
                                                                        
           SET ARQ-WRITE               TO TRUE.                         
                                                                        
           PERFORM 1130-TESTAR-FS-AVISOSAI.                             
                                                                        
           ADD 1                       TO ACU-GRV-AVISOSAI.             
      *                                                                 
      *----------------------------------------------------------------*
       3300-99-EXIT.                   EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       4000-TRATAR-REGISTRO            SECTION.                         
      *----------------------------------------------------------------*
      *                                                                 
           MOVE 'S'                    TO WRK-REG-OK.                   
                                                                        
      *                                                                 
           IF  AVI-NOME-AVAL           EQUAL  SPACES                    
             MOVE 'N'                  TO  WRK-REG-OK                   
             MOVE  61                  TO  WRK-SIT-CORSP                
           ELSE                                                         
             PERFORM 4200-CONSISTIR-ENDER                               
           END-IF.                                                      
                                                                        
      *                                                                 
      *----------------------------------------------------------------*
       4000-99-EXIT.                   EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       4200-CONSISTIR-ENDER            SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE '4200-CONSISTIR-ENDER' TO FRWKGHEA-IDEN-PARAGRAFO.      
                                                                        
           INITIALIZE                  CEPN31-DADOS-SAIDA.              
                                                                        
           MOVE AVI-CCEP               TO CEPN31-NUM-CEP.               
           MOVE AVI-CCEP-COMPL         TO CEPN31-COMPL-CEP.             
                                                                        
           MOVE AVI-ENDER(1:2)         TO WRK-CTPO-LOGDR-COM2.          
           MOVE WRK-CTPO-LOGDR         TO CEPN31-CTPO-LOGDR.            
           MOVE AVI-ENDER              TO CEPN31-ENDER.                 
                                                                        
           MOVE AVI-NRO                TO WRK-NRO-ENDE-X7.              
           MOVE WRK-NRO-ENDE-X5        TO WRK-NRO-ENDE-X.               
           IF  WRK-NRO-ENDE-X          IS NUMERIC                       
               MOVE WRK-NRO-ENDE       TO CEPN31-NUMERO                 
           ELSE                                                         
               MOVE ZEROS              TO CEPN31-NUMERO                 
           END-IF.                                                      
                                                                        
           IF  AVI-BAIRRO              EQUAL  SPACES                    
               MOVE 'CENTRO'           TO AVI-BAIRRO                    
           END-IF.                                                      
           MOVE AVI-BAIRRO             TO CEPN31-BAIRRO.                
                                                                        
           MOVE AVI-CIDADE             TO CEPN31-CIDADE.                
           MOVE AVI-UF                 TO CEPN31-UF.                    
           MOVE WRK-CEPN5060           TO WRK-NOME-MODULO.              
                                                                        
           CALL WRK-NOME-MODULO    USING CEPN31-AREA-CEPN5060           
                                         WRK-ERRO-AREA                  
                                         WRK-SQLCA.                     
                                                                        
           IF CEPN31-COD-RETORNO       NOT EQUAL   ZEROS  AND  4        
             DISPLAY '*********** CLLP0791 ************'                
             DISPLAY '*                               *'                
             DISPLAY '* ERRO ACESSO A ROTINA CEPN5060 *'                
             DISPLAY '*      PROGRAMA ENCERRADO       *'                
             DISPLAY '*                               *'                
             DISPLAY '*********** CLLP0791 ************'                
             MOVE  CEPN31-COD-RETORNO(2:2)                              
                                       TO WRK-COD-RETORNO               
             SET WRK-ABENDAR           TO TRUE                          
             PERFORM 9100-ERRO-MODULO                                   
           END-IF.                                                      
                                                                        
           IF CEPN31-COD-ERRO          NOT EQUAL  1  AND  2  AND  49    
             MOVE 'N'                  TO  WRK-REG-OK                   
             MOVE CEPN31-COD-ERRO      TO  WRK-SIT-CORSP                
           END-IF.                                                      
                                                                        
      *                                                                 
      *----------------------------------------------------------------*
       4200-99-EXIT.                   EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       5000-FINALIZAR                  SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE '5000-FINALIZAR'       TO FRWKGHEA-IDEN-PARAGRAFO       
                                                                        
           CLOSE AVISOENT                                               
                 AVISOSAI                                               
                 NAOEMITE.                                              
                                                                        
           SET ARQ-CLOSE               TO TRUE.                         
                                                                        
           PERFORM 1100-TESTAR-FILE-STATUS.                             
                                                                        
           PERFORM 5300-EMITIR-ESTATISTICA.                             
                                                                        
           MOVE ZEROS                  TO RETURN-CODE.                  
                                                                        
           DISPLAY '************ PROGRAMA CLLP0792 *************'.      
           DISPLAY '*                                          *'.      
           DISPLAY '* PROGRAMA CLLP0792 ENCERRADO COM SUCESSO  *'.      
           DISPLAY '*                                          *'.      
           DISPLAY '************ PROGRAMA CLLP0792 *************'.      
                                                                        
           PERFORM 5400-STOP-RUN.                                       
                                                                        
      *----------------------------------------------------------------*
       5000-99-EXIT.                    EXIT.                           
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       5300-EMITIR-ESTATISTICA         SECTION.                         
      *----------------------------------------------------------------*
      *                                                                 
           MOVE '5300-EMITIR-ESTATISTICA'                               
                                       TO FRWKGHEA-IDEN-PARAGRAFO       
                                                                        
           MOVE ACU-LIDOS-AVISOENT     TO WRK-MASCARA.                  
           DISPLAY                                                      
           '******************** CLLP0792 ********************'.        
           DISPLAY                                                      
           '*                                                *'.        
           DISPLAY                                                      
           '* TOTAL DE LIDOS AVISOENT........='        WRK-MASCARA ' *' 
           MOVE ACU-GRV-NAOEMITE       TO WRK-MASCARA.                  
           DISPLAY                                                      
           '* TOTAL REG. GRAVADOS NAOEMITE...='        WRK-MASCARA ' *'.
           MOVE ACU-GRV-AVISOSAI       TO WRK-MASCARA.                  
           DISPLAY                                                      
           '* TOTAL REG. GRAVADOS AVISOSAI...='        WRK-MASCARA ' *'.
           DISPLAY                                                      
           '*                                                *'.        
           DISPLAY                                                      
           '******************** CLLP0792 ********************'.        
      **                                                                
      *----------------------------------------------------------------*
       5300-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       5400-STOP-RUN                   SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           STOP RUN.                                                    
                                                                        
      *----------------------------------------------------------------*
       5400-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       8000-CHAMAR-FRWK2999            SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           DISPLAY '*** FRWK2999 CHAMADO. SEGUEM SEUS PARAMETROS ***'.  
           DISPLAY ' '.                                                 
           DISPLAY 'FRWKGHEA-TIPO-ERRO      = ' FRWKGHEA-TIPO-ERRO.     
           DISPLAY 'FRWKGHEA-NOME-PROGRAMA  = ' FRWKGHEA-NOME-PROGRAMA. 
           DISPLAY 'FRWKGHEA-IDEN-PARAGRAFO = ' FRWKGHEA-IDEN-PARAGRAFO.
           DISPLAY 'FRWKGHEA-TAM-DADOS      = ' FRWKGHEA-TAM-DADOS.     
                                                                        
           CALL WRK-FRWK2999   USING WRK-AREA-ERRO.                     
                                                                        
           DISPLAY '*** PARAMETROS RETORNADOS PELO FRWK2999 ***'.       
           DISPLAY 'FRWKGERR-COD-RETORNO    = ' FRWKGERR-COD-RETORNO.   
           DISPLAY 'FRWKGERR-COD-ERRO       = ' FRWKGERR-COD-ERRO.      
           DISPLAY 'FRWKGERR-COD-MENSAGEM   = ' FRWKGERR-COD-MENSAGEM.  
                                                                        
      *----------------------------------------------------------------*
       8000-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       8100-ROTINA-ABEND               SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           DISPLAY '*** BRAD0450 CHAMADO PARA ABENDAR O PROGRAMA ***'.  
           DISPLAY ' '                                                  
                                                                        
           CALL 'BRAD0450' USING WRK-ABEND-0450                         
                                 WRK-DUMP-0450.                         
      *----------------------------------------------------------------*
       8100-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       9000-ERRO-ARQ                   SECTION.                         
      *----------------------------------------------------------------*
      *                                                                 
           SET  ERRO-ARQUIVO           TO TRUE.                         
           MOVE WRK-PROGRAMA           TO FRWKGHEA-NOME-PROGRAMA.       
           MOVE FRWKGARQ-TAM-LAYOUT    TO FRWKGHEA-TAM-DADOS.           
           MOVE WRK-FRWKGARQ           TO WRK-BLOCO-INFO-ERRO           
                                          (1:FRWKGHEA-TAM-DADOS).       
                                                                        
           DISPLAY ' '.                                                 
           DISPLAY 'FRWKGARQ-FILE-STATUS   = '   FRWKGARQ-FILE-STATUS.  
           DISPLAY 'FRWKGARQ-NOME-ARQUIVO  = '   FRWKGARQ-NOME-ARQUIVO. 
           DISPLAY 'FRWKGARQ-COMANDO       = '   FRWKGARQ-COMANDO.      
                                                                        
           PERFORM 9999-API-ERROS.                                      
      *                                                                 
      *----------------------------------------------------------------*
       9000-99-EXIT.                   EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       9100-ERRO-MODULO                SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           SET  ERRO-MODULO            TO TRUE.                         
           MOVE WRK-PROGRAMA           TO FRWKGHEA-NOME-PROGRAMA.       
           MOVE FRWKGMOD-TAM-LAYOUT    TO FRWKGHEA-TAM-DADOS.           
           MOVE WRK-NOME-MODULO        TO FRWKGMOD-NOME-MODULO.         
           MOVE WRK-BLOCO-RETORNO      TO FRWKGMOD-BLOCO-RETORNO.       
           MOVE WRK-FRWKGMOD           TO WRK-BLOCO-INFO-ERRO           
                                          (1:FRWKGHEA-TAM-DADOS).       
           DISPLAY ' '                                                  
           DISPLAY 'FRWKGMOD-NOME-MODULO  = '    FRWKGMOD-NOME-MODULO.  
           DISPLAY 'FRWKGMOD-COD-RETORNO  = '    FRWKGMOD-COD-RETORNO.  
           DISPLAY 'FRWKGMOD-COD-ERRO     = '    FRWKGMOD-COD-ERRO.     
           DISPLAY 'FRWKGMOD-COD-MENSAGEM = '    FRWKGMOD-COD-MENSAGEM. 
                                                                        
           PERFORM 9999-API-ERROS.                                      
                                                                        
      *----------------------------------------------------------------*
       9100-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       9999-API-ERROS                  SECTION.                         
      *----------------------------------------------------------------*
      *                                                                 
           PERFORM 5300-EMITIR-ESTATISTICA.                             
                                                                        
           PERFORM 8000-CHAMAR-FRWK2999.                                
                                                                        
           IF WRK-ABENDAR                                               
             PERFORM 8100-ROTINA-ABEND                                  
           END-IF.                                                      
                                                                        
           PERFORM 5400-STOP-RUN.                                       
      *                                                                 
      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*

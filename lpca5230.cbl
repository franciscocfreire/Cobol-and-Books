      ******************************************************************
       IDENTIFICATION DIVISION.                                         
      ******************************************************************
       PROGRAM-ID.    LPCA5230.                                         
       AUTHOR.     RICARDO/BEHAR.                                       
      ***************************************************************** 
      *               A M S   I N F O R M A T I C A                   * 
      *                                                               * 
      *    PROGRAMA     - LPCA5230                                    * 
      *    SUPERVISOR   - MAURO ZAGO             - AMS                * 
      *    ANALISTAS    - RICARDO/BEHAR          - AMS                * 
      *    ANALISTA     - MARIA JOSE ZANCHIN     - GRUPO 70           * 
      *    DATA         - 18/04/2007                                  * 
      *    PROPOSTA     - AMS 06/281-01 SUBSTITUICAO DO PARM          * 
      *                                                               * 
      *    ***  BASEADO NO PROGRAMA ORIGINAL CLLP7350  ***            * 
      ***************************************************************** 
      *                                                                *
      *    OBJETIVO     - OBTER DADOS CADASTRAIS NA ROTINA FICA ATRAVES*
      *                   DAS TABELAS-DB2 FICAB000 E FICAB001 COM BASE *
      *                   NO ARQUIVO SEQUENCIAL CADASTRO (I#LPCA05) DE *
      *                   CLIENTES COM  PENDENCIAS SEREM  RENEGOCIADAS *
      *                   (CAMPANHA 13 SALARIO).                       *
      *                   GRAVAR ARQUIVO CLIEFICA QUANDO CHAVE (AGENCIA*
      *                   E CONTA) IGUAIS.                             *
      *                                                                *
      ******************************************************************
      *                    A R Q U I V O S / I N C S                   *
      *                                                                *
      * NOME         INC'S      DESCRICAO                              *
      *                                                                *
      * CADASTRO     I#LPCA05   ARQUIVO FINAL PARA RENEGOCIACAO.       *
      * CLIEFICA     I#LPCA05   ARQUIVO DE DADOS CADASTRAIS.           *
      *                                                                *
      ******************************************************************
      *                                                                *
      *                    T A B E L A S                               *
      *   NOME            NOME DA TABELA                               *
      *   FICAB000        DB2PRD.PESSOA_CADASTRADA                     *
      *   FICAB001        DB2PRD.PESSOA_FIS_CADTR                      *
      *                                                                *
      *----------------------------------------------------------------*
                                                                        
      *---------------------------------------------------------------* 
       ENVIRONMENT DIVISION.                                            
      *---------------------------------------------------------------* 
       CONFIGURATION SECTION.                                           
      *---------------------------------------------------------------* 
                                                                        
      *SOURCE-COMPUTER.                                                 
      *    IBM-370 WITH DEBUGGING MODE.                                 
       SPECIAL-NAMES.                                                   
           DECIMAL-POINT IS COMMA.                                      
                                                                        
      *---------------------------------------------------------------* 
                                                                        
      ******************************************************************
       INPUT-OUTPUT SECTION.                                            
      ******************************************************************
                                                                        
       FILE-CONTROL.                                                    
                                                                        
           SELECT CADASTRO ASSIGN      TO UT-S-CADASTRO                 
                           FILE STATUS IS WRK-FS-CADASTRO.              
                                                                        
           SELECT CLIEFICA ASSIGN      TO UT-S-CLIEFICA                 
                           FILE STATUS IS WRK-FS-CLIEFICA.              
                                                                        
      *---------------------------------------------------------------* 
       DATA DIVISION.                                                   
      *---------------------------------------------------------------* 
       FILE SECTION.                                                    
      *---------------------------------------------------------------* 
      * ARQ.INPUT - CADASTRO                               -LRECL=480 * 
      *---------------------------------------------------------------* 
       FD  CADASTRO                                                     
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       COPY 'I#LPCA05'.
                                                                        
      *---------------------------------------------------------------* 
      * ARQ.OUTPUT - CLIEFICA                              -LRECL=480 * 
      *---------------------------------------------------------------* 
       FD  CLIEFICA                                                     
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       COPY 'I#LPCA05'.
                                                                        
      *---------------------------------------------------------------* 
       WORKING-STORAGE SECTION.                                         
      *---------------------------------------------------------------* 
       77  FILLER                      PIC X(032)        VALUE          
           '** INICIO DA WORKING LPCA5230 **'.                          
                                                                        
      *---------------------------------------------------------------* 
      *    AREA PARA VARIAVEIS UTILIZADAS EM TESTES DE FILE-STATUS    * 
      *---------------------------------------------------------------* 
                                                                        
       01  WRK-FILE-STATUS.                                             
           03  WRK-ABEND               PIC S9(004) COMP VALUE +1111.    
           03  WRK-BATCH               PIC X(008) VALUE 'BATCH'.        
           03  WRK-FS-CADASTRO         PIC X(002) VALUE SPACES.         
           03  WRK-FS-CLIEFICA         PIC X(002) VALUE SPACES.         
           03  WRK-ABERTURA            PIC X(013) VALUE ' NA ABERTURA '.
           03  WRK-LEITURA             PIC X(013) VALUE ' NA LEITURA  '.
           03  WRK-GRAVACAO            PIC X(013) VALUE ' NA GRAVACAO '.
           03  WRK-FECHAMENTO          PIC X(013) VALUE 'NO FECHAMENTO'.
                                                                        
       01  WRK-MENSAGEM-ERRO.                                           
           03  FILLER                  PIC X(009) VALUE '*** ERRO '.    
           03  WRK-OPERACAO            PIC X(013) VALUE SPACES.         
           03  FILLER                  PIC X(019) VALUE 'DO ARQUIVO '.  
           03  FILLER                  PIC X(017) VALUE                 
               ' - FILE STATUS = '.                                     
           03  FILLER                  PIC X(006) VALUE ' ***'.         
                                                                        
      *---------------------------------------------------------------* 
      *                DEFINICAO DE CAMPOS ACUMULADORES               * 
      *---------------------------------------------------------------* 
                                                                        
       01  WRK-ACUMULADORES.                                            
           03  ACU-LIDOS-CADASTRO      PIC 9(009) COMP-3 VALUE ZEROS.   
           03  ACU-GRAVA-CLIEFICA      PIC 9(009) COMP-3 VALUE ZEROS.   
                                                                        
       01  FILLER.                                                      
           03  ACU-NAO-ENCONT-FICAB000 PIC 9(009) COMP-3 VALUE ZEROS.   
           03  ACU-NAO-ENCONT-FICAB001 PIC 9(009) COMP-3 VALUE ZEROS.   
                                                                        
      *---------------------------------------------------------------* 
      *                         AREA AUXILIAR                         * 
      *---------------------------------------------------------------* 
                                                                        
       01  FILLER.                                                      
           03 WRK-AGENCIA-NAO-ENCONT   PIC 9(005) VALUE ZEROS.          
           03 WRK-CONTA-NAO-ENCONT     PIC 9(007) VALUE ZEROS.          
                                                                        
      *---------------------------------------------------------------* 
      *                 DEFINICAO DE CAMPOS DE EDICAO                 * 
      *---------------------------------------------------------------* 
                                                                        
       01  WRK-EDICAO.                                                  
           03 WRK-ED-CADASTRO          PIC ZZZ.ZZZ.ZZ9.                 
           03 WRK-ED-CLIEFICA          PIC ZZZ.ZZZ.ZZ9.                 
           03 WRK-ED-NAO-FICAB00       PIC ZZZ.ZZZ.ZZ9.                 
           03 WRK-ED-NAO-FICAB01       PIC ZZZ.ZZZ.ZZ9.                 
                                                                        
      *---------------------------------------------------------------* 
      *    AREA  PARA TRATAMENTO DE DATAS                             * 
      *---------------------------------------------------------------* 
                                                                        
       01  WRK-DATA-DB2.                                                
           03  WRK-DD-DB2               PIC 9(02).                      
           03  FILLER                   PIC X(01).                      
           03  WRK-MM-DB2               PIC 9(02).                      
           03  FILLER                   PIC X(01).                      
           03  WRK-AA-DB2               PIC 9(04).                      
                                                                        
       01  WRK-DATA-NUM                 PIC 9(08).                      
       01  FILLER           REDEFINES   WRK-DATA-NUM.                   
           03  WRK-AA-NUM               PIC 9(04).                      
           03  WRK-MM-NUM               PIC 9(02).                      
           03  WRK-DD-NUM               PIC 9(02).                      
                                                                        
      *---------------------------------------------------------------* 
      *               DEFINICAO DAS VARIAVEIS INDICADORAS             * 
      *---------------------------------------------------------------* 
                                                                        
       01  FILLER.                                                      
           03 WRK-CCEP-COMPL-NULL      PIC S9(04) COMP.                 
           03 WRK-CSGL-UF-COML-NULL    PIC S9(04) COMP.                 
                                                                        
      *---------------------------------------------------------------* 
      *                AREAS UTILIZADAS PELA POOL7100                 * 
      *---------------------------------------------------------------* 
                                                                        
       COPY 'POL7100C'.
                                                                        
      *---------------------------------------------------------------* 
      *                AREAS UTILIZADAS PELAS TABELAS DB2             * 
      *---------------------------------------------------------------* 
                                                                        
           EXEC SQL                                                     
               INCLUDE SQLCA                                            
           END-EXEC.                                                    
                                                                        
           EXEC SQL                                                     
               INCLUDE FICAB000                                         
           END-EXEC.                                                    
                                                                        
           EXEC SQL                                                     
               INCLUDE FICAB001                                         
           END-EXEC.                                                    
                                                                        
      ******************************************************************
       01  FILLER                      PIC  X(032) VALUE                
           '** FINAL  DA WORKING LPCA5230 **'.                          
                                                                        
      ******************************************************************
       PROCEDURE                       DIVISION.                        
      ******************************************************************
      *DECLARATIVES.                                                    
      *COBOL-DEBUG SECTION.                                             
      *    USE FOR DEBUGGING ON ALL PROCEDURES.                         
      *COBOL-DEBUG-PARAGRAPH.                                           
      *    DISPLAY DEBUG-NAME.                                          
      *END DECLARATIVES.                                                
      *---------------------------------------------------------------* 
       0000-00-PRINCIPAL               SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           CALL    'POOL1050'.                                          
                                                                        
           PERFORM 1000-00-ABRIR-ARQUIVOS.                              
                                                                        
           PERFORM 2000-00-TESTAR-VAZIO.                                
                                                                        
           PERFORM 3000-00-PROCESSAR   UNTIL WRK-FS-CADASTRO EQUAL '10'.
                                                                        
           PERFORM 4000-00-EMITIR-DISPLAY.                              
                                                                        
           PERFORM 5000-00-FINALIZAR.                                   
                                                                        
      *---------------------------------------------------------------* 
       0000-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1000-00-ABRIR-ARQUIVOS          SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           OPEN INPUT   CADASTRO                                        
                OUTPUT  CLIEFICA.                                       
                                                                        
           MOVE WRK-ABERTURA           TO WRK-OPERACAO.                 
           PERFORM 1100-00-TESTAR-FS.                                   
                                                                        
      *---------------------------------------------------------------* 
       1000-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1100-00-TESTAR-FS               SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           PERFORM 1110-00-TESTAR-CADASTRO.                             
                                                                        
           PERFORM 1120-00-TESTAR-CLIEFICA.                             
                                                                        
      *---------------------------------------------------------------* 
       1100-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1110-00-TESTAR-CADASTRO         SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           IF  WRK-FS-CADASTRO         NOT EQUAL '00'                   
               DISPLAY '************* LPCA5230 **************'          
               DISPLAY '*   ERRO ' WRK-OPERACAO ' DO ARQUIVO   *'       
               DISPLAY '*             CADASTRO              *'          
               DISPLAY '*        FILE STATUS =  ' WRK-FS-CADASTRO       
                                                 '          *'          
               DISPLAY '************* LPCA5230 **************'          
               CALL 'ILBOABN0'         USING WRK-ABEND                  
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       1110-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1120-00-TESTAR-CLIEFICA         SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           IF  WRK-FS-CLIEFICA         NOT EQUAL '00'                   
               DISPLAY '************* LPCA5230 **************'          
               DISPLAY '*   ERRO ' WRK-OPERACAO ' DO ARQUIVO   *'       
               DISPLAY '*              CLIEFICA             *'          
               DISPLAY '*        FILE STATUS =  ' WRK-FS-CLIEFICA       
                                                 '          *'          
               DISPLAY '************* LPCA5230 **************'          
               CALL 'ILBOABN0'         USING WRK-ABEND                  
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       1120-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       2000-00-TESTAR-VAZIO            SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           PERFORM 2300-00-LER-CADASTRO                                 
           IF  WRK-FS-CADASTRO         EQUAL 10                         
               DISPLAY '************** LPCA5230 ************'           
               DISPLAY '*                                  *'           
               DISPLAY '*      ARQUIVO CADASTRO VAZIO      *'           
               DISPLAY '*                                  *'           
               DISPLAY '************** LPCA5230 ************'           
               PERFORM 5000-00-FINALIZAR                                
           ELSE                                                         
               IF LPCA05-H-CPFCNPJ-NUM OF CADASTRO EQUAL ZEROS  AND     
                  LPCA05-H-CPFCNPJ-FIL OF CADASTRO EQUAL ZEROS  AND     
                  LPCA05-H-AGENCIA     OF CADASTRO EQUAL ZEROS  AND     
                  LPCA05-H-CONTA       OF CADASTRO EQUAL ZEROS          
                  PERFORM  3500-00-GRAVA-HEADER                         
               ELSE                                                     
                  DISPLAY '************** LPCA5230 ***************'     
                  DISPLAY '*=                                   =*'     
                  DISPLAY '*= O ARQUIVO - CADATRO  - SEM HEADER =*'     
                  DISPLAY '*=                                   =*'     
                  DISPLAY '************** LPCA5230 ***************'     
                  MOVE 04               TO  RETURN-CODE                 
                  PERFORM  5000-00-FINALIZAR                            
               END-IF                                                   
           END-IF.                                                      
                                                                        
           PERFORM 2300-00-LER-CADASTRO                                 
                                                                        
           IF  WRK-FS-CADASTRO      EQUAL  '10'                         
               DISPLAY '************** LPCA5230 ****************'       
               DISPLAY '*=                                    =*'       
               DISPLAY '*= O ARQUIVO - CADATRO  - SEM DETALHE =*'       
               DISPLAY '*=                                    =*'       
               DISPLAY '************** LPCA5230 ****************'       
               MOVE 04                 TO   RETURN-CODE                 
               PERFORM  5000-00-FINALIZAR                               
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       2000-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       2300-00-LER-CADASTRO            SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           READ CADASTRO.                                               
                                                                        
           IF   WRK-FS-CADASTRO        EQUAL '10'                       
                GO TO 2300-99-LER-FIM.                                  
                                                                        
           MOVE WRK-LEITURA            TO  WRK-OPERACAO                 
           PERFORM 1110-00-TESTAR-CADASTRO.                             
                                                                        
           ADD  1                      TO  ACU-LIDOS-CADASTRO.          
                                                                        
      *---------------------------------------------------------------* 
       2300-99-LER-FIM.                EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       3000-00-PROCESSAR               SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           PERFORM 3100-00-SELECT-FICAB000.                             
                                                                        
           PERFORM 3200-00-SELECT-FICAB001.                             
                                                                        
           WRITE  REG-LPCA05                         OF CLIEFICA.       
           MOVE   WRK-GRAVACAO         TO WRK-OPERACAO.                 
           PERFORM 1120-00-TESTAR-CLIEFICA.                             
           ADD   1                     TO ACU-GRAVA-CLIEFICA.           
                                                                        
           PERFORM  2300-00-LER-CADASTRO.                               
                                                                        
      *---------------------------------------------------------------* 
       3000-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       3100-00-SELECT-FICAB000         SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           MOVE LPCA05-AGENCIA OF CADASTRO TO CJUNC-DEPDC OF FICAB000.  
           MOVE LPCA05-CONTA   OF CADASTRO TO CCTA-CORR   OF FICAB000.  
                                                                        
           EXEC SQL                                                     
            SELECT    CPSSOA_CADTR ,                                    
                      CJUNC_DEPDC ,                                     
                      CCTA_CORR ,                                       
                      ELOGDR ,                                          
                      ENRO_LOGDR ,                                      
                      ECOMPL_LOGDR ,                                    
                      CCEP ,                                            
                      CCEP_COMPL ,                                      
                      CSGL_UF_COML ,                                    
                      IMUN ,                                            
                      CDDD ,                                            
                      CFONE                                             
                                                                        
           INTO       :FICAB000.CPSSOA-CADTR ,                          
                      :FICAB000.CJUNC-DEPDC ,                           
                      :FICAB000.CCTA-CORR ,                             
                      :FICAB000.ELOGDR ,                                
                      :FICAB000.ENRO-LOGDR ,                            
                      :FICAB000.ECOMPL-LOGDR ,                          
                      :FICAB000.CCEP ,                                  
                      :FICAB000.CCEP-COMPL                              
                        :WRK-CCEP-COMPL-NULL,                           
                      :FICAB000.CSGL-UF-COML                            
                        :WRK-CSGL-UF-COML-NULL,                         
                      :FICAB000.IMUN ,                                  
                      :FICAB000.CDDD ,                                  
                      :FICAB000.CFONE                                   
                                                                        
             FROM   DB2PRD.PESSOA_CADASTRADA                            
             WHERE  CJUNC_DEPDC = :FICAB000.CJUNC-DEPDC                 
             AND    CCTA_CORR   = :FICAB000.CCTA-CORR                   
           END-EXEC.                                                    
                                                                        
           IF SQLCODE EQUAL +100                                        
              ADD 1                    TO  ACU-NAO-ENCONT-FICAB000      
              MOVE CJUNC-DEPDC         TO  WRK-AGENCIA-NAO-ENCONT       
              MOVE CCTA-CORR           TO  WRK-CONTA-NAO-ENCONT         
           ELSE                                                         
              IF ( SQLCODE NOT EQUAL ZEROS AND +100 AND -811 ) OR       
                 ( SQLWARN0    EQUAL 'W' )                              
                   MOVE 'DB2'                TO ERR-TIPO-ACESSO         
                   MOVE 'PESSOA_CADASTRADA ' TO ERR-DBD-TAB             
                   MOVE 'SELECT'             TO ERR-FUN-COMANDO         
                   MOVE  SQLCODE             TO ERR-SQL-CODE            
                   MOVE  0001                TO ERR-LOCAL               
                   MOVE  SPACES              TO ERR-SEGM                
                   PERFORM 99999-ROTINA-ERRO.                           
                                                                        
           PERFORM 3300-00-MOVER-DADOS-FICAB000.                        
                                                                        
      *---------------------------------------------------------------* 
       3100-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       3200-00-SELECT-FICAB001         SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           IF SQLCODE EQUAL +100                                        
              PERFORM 3400-00-MOVER-DADOS-FICAB001                      
              GO TO 3200-99-FIM.                                        
                                                                        
           MOVE CPSSOA-CADTR OF FICAB000 TO CPSSOA-CADTR OF FICAB001    
                                                                        
           EXEC SQL                                                     
            SELECT     DNASC ,                                          
                       IMAE ,                                           
                       IPAI,                                            
                       VSALRL_MES                                       
           INTO        :FICAB001.DNASC ,                                
                       :FICAB001.IMAE ,                                 
                       :FICAB001.IPAI,                                  
                       :FICAB001.VSALRL-MES                             
                                                                        
             FROM      DB2PRD.PESSOA_FIS_CADTR                          
             WHERE     CPSSOA_CADTR = :FICAB001.CPSSOA-CADTR            
           END-EXEC.                                                    
                                                                        
                                                                        
           IF SQLCODE EQUAL +100                                        
              ADD 1                    TO  ACU-NAO-ENCONT-FICAB001      
           ELSE                                                         
              IF ( SQLCODE NOT EQUAL ZEROS AND +100 AND -811 ) OR       
                 ( SQLWARN0    EQUAL 'W' )                              
                   MOVE 'DB2'                TO ERR-TIPO-ACESSO         
                   MOVE 'PESSOA_FIS_CADTR '  TO ERR-DBD-TAB             
                   MOVE 'SELECT'             TO ERR-FUN-COMANDO         
                   MOVE  SQLCODE             TO ERR-SQL-CODE            
                   MOVE  0002                TO ERR-LOCAL               
                   MOVE  SPACES              TO ERR-SEGM                
                   PERFORM 99999-ROTINA-ERRO.                           
                                                                        
           PERFORM 3400-00-MOVER-DADOS-FICAB001.                        
                                                                        
      *---------------------------------------------------------------* 
       3200-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       3300-00-MOVER-DADOS-FICAB000    SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           INITIALIZE REG-LPCA05       OF CLIEFICA.                     
                                                                        
           MOVE   REG-LPCA05       OF CADASTRO  TO                      
                  REG-LPCA05       OF CLIEFICA.                         
                                                                        
           IF SQLCODE EQUAL +100                                        
              MOVE SPACES         TO LPCA05-ENDERECO-FICA  OF CLIEFICA  
                                     LPCA05-COMPL-FICA     OF CLIEFICA  
                                     LPCA05-UF-FICA        OF CLIEFICA  
                                     LPCA05-CIDADE-FICA    OF CLIEFICA  
                                     LPCA05-DDD-FICA       OF CLIEFICA  
              MOVE ZEROS          TO LPCA05-NUMERO-FICA    OF CLIEFICA  
                                     LPCA05-CEP-FICA       OF CLIEFICA  
                                     LPCA05-CEP-COMPL-FICA OF CLIEFICA  
                                     LPCA05-TELEFONE-FICA  OF CLIEFICA  
                                     LPCA05-RG-FICA        OF CLIEFICA  
           ELSE                                                         
              MOVE ELOGDR                              OF FICAB000      
                                  TO LPCA05-ENDERECO-FICA  OF CLIEFICA  
              MOVE ECOMPL-LOGDR                        OF FICAB000      
                                  TO LPCA05-COMPL-FICA     OF CLIEFICA  
              MOVE CSGL-UF-COML                        OF FICAB000      
                                  TO LPCA05-UF-FICA        OF CLIEFICA  
              MOVE IMUN                                OF FICAB000      
                                  TO LPCA05-CIDADE-FICA    OF CLIEFICA  
              MOVE CDDD                                OF FICAB000      
                                  TO LPCA05-DDD-FICA       OF CLIEFICA  
              MOVE ENRO-LOGDR                          OF FICAB000      
                                  TO LPCA05-NUMERO-FICA    OF CLIEFICA  
              MOVE CCEP                                OF FICAB000      
                                  TO LPCA05-CEP-FICA       OF CLIEFICA  
              MOVE CCEP-COMPL                          OF FICAB000      
                                  TO LPCA05-CEP-COMPL-FICA OF CLIEFICA  
              MOVE CFONE                               OF FICAB000      
                                  TO LPCA05-TELEFONE-FICA  OF CLIEFICA  
              MOVE ZEROS          TO LPCA05-RG-FICA        OF CLIEFICA. 
                                                                        
      *---------------------------------------------------------------* 
       3300-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       3400-00-MOVER-DADOS-FICAB001    SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           IF SQLCODE                  EQUAL +100                       
              MOVE SPACES         TO LPCA05-NOME-PAI-FICA  OF CLIEFICA  
                                     LPCA05-NOME-MAE-FICA  OF CLIEFICA  
              MOVE ZEROS          TO LPCA05-RENDA-FICA     OF CLIEFICA  
                                     LPCA05-DT-NASC-FICA   OF CLIEFICA  
           ELSE                                                         
              MOVE IPAI  OF FICAB001   TO                               
                                     LPCA05-NOME-PAI-FICA  OF CLIEFICA  
              MOVE IMAE  OF FICAB001   TO                               
                                     LPCA05-NOME-MAE-FICA  OF CLIEFICA  
              MOVE DNASC OF FICAB001   TO WRK-DATA-DB2                  
              MOVE WRK-DD-DB2          TO WRK-DD-NUM                    
              MOVE WRK-MM-DB2          TO WRK-MM-NUM                    
              MOVE WRK-AA-DB2          TO WRK-AA-NUM                    
              MOVE WRK-DATA-NUM        TO                               
                                     LPCA05-DT-NASC-FICA   OF CLIEFICA  
              MOVE VSALRL-MES OF FICAB001 TO                            
                                     LPCA05-RENDA-FICA     OF CLIEFICA. 
                                                                        
      *---------------------------------------------------------------* 
       3400-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       3500-00-GRAVA-HEADER            SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           INITIALIZE REG-LPCA05       OF CLIEFICA.                     
                                                                        
           MOVE   REG-LPCA05       OF CADASTRO  TO                      
                  REG-LPCA05       OF CLIEFICA.                         
                                                                        
           WRITE  REG-LPCA05                         OF CLIEFICA.       
                                                                        
           MOVE   WRK-GRAVACAO         TO WRK-OPERACAO.                 
           PERFORM 1120-00-TESTAR-CLIEFICA.                             
           ADD   1                     TO ACU-GRAVA-CLIEFICA.           
                                                                        
      *---------------------------------------------------------------* 
       3500-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
      *---------------------------------------------------------------* 
       4000-00-EMITIR-DISPLAY          SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
                                                                        
           MOVE ACU-LIDOS-CADASTRO      TO WRK-ED-CADASTRO.             
           MOVE ACU-GRAVA-CLIEFICA      TO WRK-ED-CLIEFICA.             
           MOVE ACU-NAO-ENCONT-FICAB000 TO WRK-ED-NAO-FICAB00.          
           MOVE ACU-NAO-ENCONT-FICAB001 TO WRK-ED-NAO-FICAB01.          
                                                                        
           DISPLAY '****************** LPCA5230 *******************'.   
           DISPLAY '*                                             *'.   
           DISPLAY '*          PROCESSAMENTO ENCERRADO!           *'.   
           DISPLAY '*                                             *'.   
           DISPLAY '* TOTALIZACAO DOS REGISTROS:                  *'.   
           DISPLAY '* -------------------------                   *'.   
           DISPLAY '*                                             *'.   
           DISPLAY '* REGS. LIDOS CADASTRO:         'WRK-ED-CADASTRO'   
      -            '*'.                                                 
           DISPLAY '* REGS. GRAVADOS CLIEFICA:      'WRK-ED-CLIEFICA'   
      -            '*'.                                                 
           DISPLAY '* REGS. NAO ENCONTRADO FICAB000:'WRK-ED-NAO-FICAB00'
      -            '   *'.                                              
           DISPLAY '* REGS. NAO ENCONTRADO FICAB001:'WRK-ED-NAO-FICAB01'
      -            '   *'.                                              
           DISPLAY '*                                             *'.   
           DISPLAY '****************** LPCA5230 *******************'.   
                                                                        
      *---------------------------------------------------------------* 
       4000-99-FIM.                    EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       5000-00-FINALIZAR               SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           CLOSE   CADASTRO                                             
                   CLIEFICA                                             
                                                                        
           MOVE    WRK-FECHAMENTO      TO WRK-OPERACAO.                 
           PERFORM 1100-00-TESTAR-FS.                                   
                                                                        
           STOP RUN.                                                    
                                                                        
      *---------------------------------------------------------------* 
       5000-99-FINALIZAR-FIM.          EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       99999-ROTINA-ERRO               SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           MOVE   SPACES               TO   ERR-DADOS-SENHAS            
           MOVE  'LPCA5230'            TO   ERR-PGM                     
                                                                        
           CALL  'POOL7100'    USING  WRK-BATCH                         
                                      ERRO-AREA                         
                                      SQLCA.                            
                                                                        
           GOBACK.                                                      
                                                                        
      *---------------------------------------------------------------* 
       99999-99-FIM.                   EXIT.                            
      *---------------------------------------------------------------* 

      *================================================================*
       IDENTIFICATION DIVISION.                                         
      *================================================================*
      *                                                                *
       PROGRAM-ID.    SCSB2054.                                         
       AUTHOR.        ANDERSON.                                         
      *                                                                *
      *================================================================*
      *                          BRQ IT SERVICES                       *
      *================================================================*
      *                                                                *
      *  PROGRAMADOR.......: ANDERSON MARTINS                          *
      *  ANALISTA..........: FERNANDA CARUSO                           *
      *  DATA..............: 04/04/2013                                *
      *                                                                *
      *================================================================*
      *                                                                *
      *  OBJETIVO..........: CONSULTAR LISTA DAS RAS                   *
      *                                                                *
      *================================================================*
      *                                                                *
      * PROGRAMA : F U N C I O N A L                                   *
      *                                                                *
      *----------------------------------------------------------------*
      * I#FRWKGE COMMAREA FRWK1999 (LOG DE ERRO)                       *
      * I#FRWKHE AREA COMUM DE ERROS                                   *
      * I#FRWKMD COMMAREA FRWK1999 (LOG DE ERROS MODULO)               *
      * I#FRWKCI COMMAREA FRWK1999 (LOG DE ERROS CICS)                 *
      * SCSBY054 BLOCO DE ENTRADA E SAIDA - MODULO FUNCIONAL           *
      *                                                                *
140022*===============================================================* 
  -   *                       A L T E R A C A O                       * 
  -   *---------------------------------------------------------------* 
  -   *                                                               * 
  -   *    PROGRAMADOR..: LUCAS MONTEIRO        - BRQ IT SERVICES.    * 
  -   *    ANALISTA.....: FERNANDA CARUSO       - BRQ IT SERVICES.    * 
  -   *    DATA.........: 02/2015                                     * 
  -   *                                                               * 
  -   *    OBJETIVO.....: TRATAR O NOVO LAYOUT DE COMUNICACAO DO      * 
  -   *                   PGM.                                        * 
  -   *                                                               * 
140022*================================================================*
       ENVIRONMENT DIVISION.                                            
      *================================================================*
      *                                                                *
      *----------------------------------------------------------------*
       CONFIGURATION SECTION.                                           
      *----------------------------------------------------------------*
      *                                                                *
       SPECIAL-NAMES.                                                   
           DECIMAL-POINT IS COMMA.                                      
      *                                                                *
      *----------------------------------------------------------------*
       INPUT-OUTPUT SECTION.                                            
      *----------------------------------------------------------------*
      *                                                                *
       FILE-CONTROL.                                                    
      *                                                                *
      *================================================================*
       DATA DIVISION.                                                   
      *================================================================*
      *                                                                *
      *----------------------------------------------------------------*
       FILE SECTION.                                                    
      *----------------------------------------------------------------*
      *                                                                *
      *                                                                *
      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.                                         
      *----------------------------------------------------------------*
      *                                                                *
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                  PIC  X(080) VALUE                    
           'INICIO DA WORKING STORAGE'.                                 
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
       77  WRK-PROGRAMA            PIC  X(008) VALUE 'SCSB2054'.        
       77  WRK-FRWK1999            PIC  X(008) VALUE 'FRWK1999'.        
       77  WRK-SQLCODE             PIC S9(009) VALUE ZEROS COMP-3.      
       77  WRK-CALE1000            PIC  X(008) VALUE 'CALE1000'.        
       77  WRK-UORG1334            PIC  X(008) VALUE 'UORG1334'.        
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
      *  AREAS AUXILIARES                                              *
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
       01  WRK-I                   PIC  9(003) COMP-3 VALUE ZEROS.      
       01  WRK-QTDE-LIMITE         PIC  9(003) COMP-3 VALUE ZEROS.      
       01  WRK-DT-INCLUSAO         PIC  X(010)        VALUE SPACES.     
       01  WRK-MODULO              PIC  X(008) VALUE SPACES.            
                                                                        
       01  WRK-CPSSOA-JURID-ATU    PIC S9(010) COMP-3 VALUE ZEROS.      
       01  WRK-CPSSOA-JURID-ANT    PIC S9(010) COMP-3 VALUE ZEROS.      
                                                                        
       01  WRK-BLOCO-RETORNO.                                           
           05  WRK-COD-RETORNO     PIC  9(002) VALUE ZEROS.             
           05  WRK-COD-ERRO        PIC  X(004) VALUE SPACES.            
           05  WRK-COD-MENSAGEM    PIC  X(008) VALUE SPACES.            
                                                                        
       01  WRK-DANO-FIM            PIC S9(004) COMP-3 VALUE ZEROS.      
       01  WRK-NREG-FIM            PIC S9(009) COMP-3 VALUE ZEROS.      
       01  WRK-CSIT-INI            PIC  X(001) VALUE SPACES.            
       01  WRK-CPSSOA-JURID-INI    PIC S9(010) COMP-3 VALUE ZEROS.      
       01  WRK-CPSSOA-JURID-FIM    PIC S9(010) COMP-3 VALUE ZEROS.      
       01  WRK-CPSSOA-UND-INI      PIC S9(010) COMP-3 VALUE ZEROS.      
       01  WRK-CPSSOA-UND-FIM      PIC S9(010) COMP-3 VALUE ZEROS.      
       01  WRK-NSEQ-UND-INI        PIC S9(008) COMP-3 VALUE ZEROS.      
       01  WRK-NSEQ-UND-FIM        PIC S9(008) COMP-3 VALUE ZEROS.      
       01  WRK-CSIT-FIM            PIC  X(001) VALUE SPACES.            
       01  WRK-DATA-INI            PIC  X(026) VALUE SPACES.            
       01  WRK-DATA-FIM            PIC  X(026) VALUE SPACES.            
       01  WRK-CUSUAR-MANUT-INI    PIC  X(009) VALUE SPACES.            
       01  WRK-CUSUAR-MANUT-FIM    PIC  X(009) VALUE SPACES.            
                                                                        
       01  WRK-FIM-CSR             PIC  X(001) VALUE SPACES.            
       01  WRK-NOME-TABELA         PIC  X(032) VALUE SPACES.            
       01  WRK-STORED-PROC         PIC  X(008) VALUE SPACES.            
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       77  FILLER PIC X(34) VALUE  '** AREA DA API - CALE1000 **      '.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
           COPY CALEWAAC.                                               
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                  PIC  X(080) VALUE                    
           'AREA LOG DE ERRO - FRWK1999'.                               
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
       01  WRK-AREA-ERRO.                                               
           COPY I#FRWKGE.                                               
           05 WRK-BLOCO-INFO-ERRO.                                      
              10 WRK-CHAR-INFO-ERRO PIC  X(001) OCCURS 0 TO 256 TIMES   
                 DEPENDING ON FRWKGHEA-TAM-DADOS.                       
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                  PIC  X(080) VALUE                    
           'AREA TRATAMENTO DE ERRO - MODULO'.                          
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
       01  WRK-AREA-ERRO-MODULO.                                        
           COPY I#FRWKMD.                                               
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                  PIC  X(080) VALUE                    
           'AREA TRATAMENTO DE ERRO - CICS'.                            
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
       01  WRK-AREA-ERRO-CICS.                                          
           COPY I#FRWKCI.                                               
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                  PIC  X(080) VALUE                    
           'AREA TRATAMENTO DE ERRO - DB2'.                             
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
       01  WRK-AREA-ERRO-DB2.                                           
           COPY I#FRWKDB.                                               
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                        PIC X(080) VALUE               
           'AREA TRATAMENTO CHAMADA - UORG1334'.                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
       01  WRK-AREA-UORG1334.                                           
           COPY UORGW000.                                               
           COPY UORGW334.                                               
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                    PIC  X(008) VALUE 'AREA DB2'.      
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
           EXEC  SQL    INCLUDE      SQLCA      END-EXEC.               
                                                                        
           EXEC  SQL    INCLUDE      SCSBB013   END-EXEC.               
                                                                        
           EXEC  SQL    INCLUDE      SCSBB036   END-EXEC.               
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
      *   DECLARACAO DO CURSOR - CSR01-SCSBB013                        *
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
           EXEC SQL DECLARE CSR01-SCSBB013 CURSOR FOR SELECT            
                     DANO_COMPR_RECTA,                                  
                     NREG_COMPR_RECTA,                                  
                     CPSSOA_JURID    ,                                  
                     HINCL_REG                                          
                FROM DB2PRD.TREG_COMPR_RECTA                            
               WHERE ((DANO_COMPR_RECTA       >   :WRK-DANO-FIM)        
                   OR (DANO_COMPR_RECTA       =   :WRK-DANO-FIM         
                  AND  NREG_COMPR_RECTA       >   :WRK-NREG-FIM))       
                 AND (CPSSOA_JURID       BETWEEN :WRK-CPSSOA-JURID-INI  
                                            AND  :WRK-CPSSOA-JURID-FIM) 
                 AND (CPSSOA_UND_ORGNZ   BETWEEN :WRK-CPSSOA-UND-INI    
                                             AND :WRK-CPSSOA-UND-FIM)   
                 AND (NSEQ_UND_ORGNZ     BETWEEN :WRK-NSEQ-UND-INI      
                                             AND :WRK-NSEQ-UND-FIM)     
                 AND (HINCL_REG          BETWEEN :WRK-DATA-INI          
                                             AND :WRK-DATA-FIM)         
                 AND (CSIT_REG           BETWEEN :WRK-CSIT-INI          
                                             AND :WRK-CSIT-FIM)         
              ORDER BY DANO_COMPR_RECTA,                                
                       NREG_COMPR_RECTA,                                
                       CPSSOA_JURID    ,                                
                       HINCL_REG                                        
           END-EXEC.                                                    
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
      *   DECLARACAO DO CURSOR - CSR02-SCSBB013                        *
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
           EXEC SQL DECLARE CSR02-SCSBB013 CURSOR FOR SELECT            
                     DANO_COMPR_RECTA,                                  
                     NREG_COMPR_RECTA,                                  
                     CPSSOA_JURID    ,                                  
                     HINCL_REG                                          
                FROM DB2PRD.TREG_COMPR_RECTA                            
               WHERE ((DANO_COMPR_RECTA       <   :WRK-DANO-FIM)        
                   OR (DANO_COMPR_RECTA       =   :WRK-DANO-FIM         
                  AND  NREG_COMPR_RECTA       <   :WRK-NREG-FIM))       
                 AND (CPSSOA_JURID       BETWEEN :WRK-CPSSOA-JURID-INI  
                                            AND  :WRK-CPSSOA-JURID-FIM) 
                 AND (CPSSOA_UND_ORGNZ   BETWEEN :WRK-CPSSOA-UND-INI    
                                             AND :WRK-CPSSOA-UND-FIM)   
                 AND (NSEQ_UND_ORGNZ     BETWEEN :WRK-NSEQ-UND-INI      
                                             AND :WRK-NSEQ-UND-FIM)     
                 AND (HINCL_REG          BETWEEN :WRK-DATA-INI          
                                             AND :WRK-DATA-FIM)         
                 AND (CSIT_REG           BETWEEN :WRK-CSIT-INI          
                                             AND :WRK-CSIT-FIM)         
              ORDER BY DANO_COMPR_RECTA DESC,                           
                       NREG_COMPR_RECTA DESC,                           
                       CPSSOA_JURID     DESC,                           
                       HINCL_REG        DESC                            
           END-EXEC.                                                    
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                  PIC  X(080) VALUE                    
           'FINAL DA WORKING STORAGE'.                                  
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
      *----------------------------------------------------------------*
       LINKAGE SECTION.                                                 
      *----------------------------------------------------------------*
      *                                                                *
       01  DFHCOMMAREA.                                                 
           COPY SCSBY054.                                               
                                                                        
      *================================================================ 
       PROCEDURE DIVISION USING DFHCOMMAREA.                            
      *================================================================ 
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
      *   ROTINA PRINCIPAL                                             *
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
      *----------------------------------------------------------------*
       0000-ESTRUTURA                     SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           PERFORM 1000-INICIALIZAR                                     
                                                                        
           PERFORM 2000-PROCESSAR                                       
                                                                        
           PERFORM 3000-FINALIZAR.                                      
                                                                        
      *----------------------------------------------------------------*
       0000-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
      *   PROCEDIMENTOS INICIAIS DO PROGRAMA                           *
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
      *----------------------------------------------------------------*
       1000-INICIALIZAR                   SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           INITIALIZE   FRWKGERR-REGISTRO                               
                        FRWKGDB2-REGISTRO                               
                        FRWKGHEA-REGISTRO                               
                        FRWKGCIC-REGISTRO                               
                        FRWKGMOD-REGISTRO.                              
                                                                        
           PERFORM 1300-CONSISTIR-DADOS.                                
                                                                        
           MOVE ZEROS                     TO SCSBY054-COD-RETORNO       
           MOVE '0000'                    TO SCSBY054-COD-ERRO          
           MOVE 'SCSB0026'                TO SCSBY054-COD-MENSAGEM.     
                                                                        
      *----------------------------------------------------------------*
       1000-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
      *  FORMATA CHAVE DE PESQUISA PARA PAGINACAO                      *
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
      *----------------------------------------------------------------*
       1200-TRATAR-PAGINACAO              SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           MOVE 50                        TO WRK-QTDE-LIMITE            
                                                                        
           PERFORM 1201-FORMATA-CAMPOS                                  
                                                                        
           EVALUATE TRUE                                                
               WHEN SCSBY054-P-INICIAL                                  
               WHEN SCSBY054-P-PRIMEIRA                                 
                                                                        
                    CONTINUE                                            
                                                                        
               WHEN SCSBY054-P-ULTIMA                                   
                                                                        
                    MOVE SCSBY054-P-ULT-ANO   TO WRK-DANO-FIM           
                    MOVE SCSBY054-P-ULT-NREG  TO WRK-NREG-FIM           
                                                                        
               WHEN SCSBY054-P-SEGUINTE                                 
                                                                        
                    MOVE SCSBY054-P-ULT-ANO   TO WRK-DANO-FIM           
                    MOVE SCSBY054-P-ULT-NREG  TO WRK-NREG-FIM           
                                                                        
               WHEN SCSBY054-P-ANTERIOR                                 
                                                                        
                    MOVE SCSBY054-P-PRI-ANO   TO WRK-DANO-FIM           
                    MOVE SCSBY054-P-PRI-NREG  TO WRK-NREG-FIM           
                                                                        
           END-EVALUATE.                                                
                                                                        
      *----------------------------------------------------------------*
       1200-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       1201-FORMATA-CAMPOS                SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           IF SCSBY054-E-ANO               EQUAL ZEROS                  
              MOVE ZEROS                   TO WRK-DANO-FIM              
           END-IF                                                       
                                                                        
           IF SCSBY054-E-NREG              EQUAL ZEROS                  
              MOVE ZEROS                   TO WRK-NREG-FIM              
           END-IF                                                       
                                                                        
           IF SCSBY054-E-EMPR              EQUAL ZEROS                  
              MOVE ZEROS                   TO WRK-CPSSOA-JURID-INI      
              MOVE 9999999999              TO WRK-CPSSOA-JURID-FIM      
           ELSE                                                         
              MOVE SCSBY054-E-EMPR         TO WRK-CPSSOA-JURID-INI      
              MOVE SCSBY054-E-EMPR         TO WRK-CPSSOA-JURID-FIM      
           END-IF.                                                      
                                                                        
           IF SCSBY054-E-EMPR-DEPEND       EQUAL ZEROS                  
              MOVE ZEROS                   TO WRK-CPSSOA-UND-INI        
              MOVE 9999999999              TO WRK-CPSSOA-UND-FIM        
           ELSE                                                         
              MOVE SCSBY054-E-EMPR-DEPEND  TO WRK-CPSSOA-UND-INI        
              MOVE SCSBY054-E-EMPR-DEPEND  TO WRK-CPSSOA-UND-FIM        
           END-IF.                                                      
                                                                        
           IF SCSBY054-E-DEPEND            EQUAL ZEROS                  
              MOVE ZEROS                   TO WRK-NSEQ-UND-INI          
              MOVE 99999999                TO WRK-NSEQ-UND-FIM          
           ELSE                                                         
              MOVE SCSBY054-E-DEPEND       TO WRK-NSEQ-UND-INI          
              MOVE SCSBY054-E-DEPEND       TO WRK-NSEQ-UND-FIM          
           END-IF.                                                      
                                                                        
           IF SCSBY054-E-DATA-INI         EQUAL SPACES                  
              MOVE '0001-01-01-00.00.00.000000'                         
                                          TO WRK-DATA-INI               
              MOVE '9999-12-31-23.59.59.999999'                         
                                          TO WRK-DATA-FIM               
           ELSE                                                         
              STRING SCSBY054-E-DATA-INI (7:4) '-'                      
                     SCSBY054-E-DATA-INI (4:2) '-'                      
                     SCSBY054-E-DATA-INI (1:2) '-'                      
              DELIMITED BY SIZE         INTO WRK-DATA-INI(1:10)         
              MOVE '-00.00.00.000000'     TO WRK-DATA-INI(11:16)        
                                                                        
              STRING SCSBY054-E-DATA-FIM (7:4) '-'                      
                     SCSBY054-E-DATA-FIM (4:2) '-'                      
                     SCSBY054-E-DATA-FIM (1:2) '-'                      
              DELIMITED BY SIZE         INTO WRK-DATA-FIM(1:10)         
              MOVE '-23.59.59.999999'     TO WRK-DATA-FIM(11:16)        
           END-IF                                                       
                                                                        
           IF SCSBY054-E-SITUACAO         EQUAL SPACES                  
              MOVE LOW-VALUES             TO WRK-CSIT-INI               
              MOVE HIGH-VALUES            TO WRK-CSIT-FIM               
           ELSE                                                         
              MOVE SCSBY054-E-SITUACAO    TO WRK-CSIT-INI               
              MOVE SCSBY054-E-SITUACAO    TO WRK-CSIT-FIM               
           END-IF.                                                      
                                                                        
           IF SCSBY054-E-CUSUAR           EQUAL SPACES                  
              MOVE LOW-VALUES             TO WRK-CUSUAR-MANUT-INI       
              MOVE HIGH-VALUES            TO WRK-CUSUAR-MANUT-FIM       
           ELSE                                                         
              MOVE SCSBY054-E-CUSUAR      TO WRK-CUSUAR-MANUT-INI       
              MOVE SCSBY054-E-CUSUAR      TO WRK-CUSUAR-MANUT-FIM       
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       1201-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
      *   CONSISTIR DADOS DE ENTRADA - FUNCIONAL                       *
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
      *----------------------------------------------------------------*
       1300-CONSISTIR-DADOS               SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           IF SCSBY054-IND-PAGINACAO    NOT EQUAL 'I' AND 'P' AND 'S'   
                                                      AND 'A' AND 'U'   
              MOVE 08                     TO SCSBY054-COD-RETORNO       
              MOVE '0001'                 TO SCSBY054-COD-ERRO          
              MOVE 'SCSB0039'             TO SCSBY054-COD-MENSAGEM      
              PERFORM 3000-FINALIZAR                                    
           END-IF.                                                      
                                                                        
           IF SCSBY054-E-ANO             NOT NUMERIC                    
              MOVE 08                     TO SCSBY054-COD-RETORNO       
              MOVE '0002'                 TO SCSBY054-COD-ERRO          
              MOVE 'SCSB0037'             TO SCSBY054-COD-MENSAGEM      
              PERFORM 3000-FINALIZAR                                    
           END-IF.                                                      
                                                                        
           IF SCSBY054-E-NREG            NOT NUMERIC                    
              MOVE 08                     TO SCSBY054-COD-RETORNO       
              MOVE '0003'                 TO SCSBY054-COD-ERRO          
              MOVE 'SCSB0037'             TO SCSBY054-COD-MENSAGEM      
              PERFORM 3000-FINALIZAR                                    
           END-IF.                                                      
                                                                        
           IF SCSBY054-E-EMPR-DEPEND     NOT NUMERIC                    
              MOVE 08                     TO SCSBY054-COD-RETORNO       
              MOVE '0004'                 TO SCSBY054-COD-ERRO          
              MOVE 'SCSB0002'             TO SCSBY054-COD-MENSAGEM      
              PERFORM 3000-FINALIZAR                                    
           END-IF.                                                      
                                                                        
           IF SCSBY054-E-DEPEND          NOT NUMERIC                    
              MOVE 08                     TO SCSBY054-COD-RETORNO       
              MOVE '0005'                 TO SCSBY054-COD-ERRO          
              MOVE 'SCSB0002'             TO SCSBY054-COD-MENSAGEM      
              PERFORM 3000-FINALIZAR                                    
           END-IF.                                                      
                                                                        
           IF SCSBY054-E-NREG            EQUAL ZEROS AND                
              SCSBY054-E-ANO             EQUAL ZEROS                    
                                                                        
              IF SCSBY054-E-EMPR            NOT NUMERIC OR              
                 SCSBY054-E-EMPR            EQUAL ZEROS                 
                 MOVE 08                     TO SCSBY054-COD-RETORNO    
                 MOVE '0006'                 TO SCSBY054-COD-ERRO       
                 MOVE 'SCSB0001'             TO SCSBY054-COD-MENSAGEM   
                 PERFORM 3000-FINALIZAR                                 
              END-IF                                                    
                                                                        
              IF SCSBY054-E-DATA-INI        NOT EQUAL SPACES AND        
                 SCSBY054-E-DATA-INI            EQUAL SPACES            
                 MOVE 08                     TO SCSBY054-COD-RETORNO    
                 MOVE '0007'                 TO SCSBY054-COD-ERRO       
                 MOVE 'SCSB0031'             TO SCSBY054-COD-MENSAGEM   
                 PERFORM 3000-FINALIZAR                                 
              END-IF                                                    
                                                                        
              IF SCSBY054-E-DATA-FIM        NOT EQUAL SPACES AND        
                 SCSBY054-E-DATA-FIM            EQUAL SPACES            
                 MOVE 08                     TO SCSBY054-COD-RETORNO    
                 MOVE '0008'                 TO SCSBY054-COD-ERRO       
                 MOVE 'SCSB0031'             TO SCSBY054-COD-MENSAGEM   
                 PERFORM 3000-FINALIZAR                                 
              END-IF                                                    
           END-IF.                                                      
                                                                        
           IF SCSBY054-E-SITUACAO         NOT EQUAL SPACES              
              IF SCSBY054-E-SITUACAO      NOT EQUAL 'P' AND 'V' AND 'R' 
                                                AND 'G' AND 'E' AND 'T' 
                                                AND 'F'                 
                 MOVE 08                  TO SCSBY054-COD-RETORNO       
                 MOVE '0009'              TO SCSBY054-COD-ERRO          
                 MOVE 'SCSB0040'          TO SCSBY054-COD-MENSAGEM      
                 PERFORM 3000-FINALIZAR                                 
              END-IF                                                    
           END-IF.                                                      
                                                                        
      *---> VALIDA DATAS                                                
           IF SCSBY054-E-DATA-INI        NOT EQUAL SPACES               
              MOVE  007                      TO  CALEWAAC-FMT-ARGMTO    
              MOVE  SCSBY054-E-DATA-INI      TO  CALEWAAC-VLR-ARGMTO    
              PERFORM 4000-ACESSAR-CALE1000                             
                                                                        
              IF CALE-INCONSISTENTE                                     
                 MOVE 08                     TO SCSBY054-COD-RETORNO    
                 MOVE '0010'                 TO SCSBY054-COD-ERRO       
                 MOVE 'SCSB0041'             TO SCSBY054-COD-MENSAGEM   
                 PERFORM 3000-FINALIZAR                                 
              END-IF                                                    
           END-IF                                                       
                                                                        
           IF SCSBY054-E-DATA-FIM        NOT EQUAL SPACES               
              MOVE  007                      TO  CALEWAAC-FMT-ARGMTO    
              MOVE  SCSBY054-E-DATA-FIM      TO  CALEWAAC-VLR-ARGMTO    
              PERFORM 4000-ACESSAR-CALE1000                             
                                                                        
              IF CALE-INCONSISTENTE                                     
                 MOVE 08                     TO SCSBY054-COD-RETORNO    
                 MOVE '0011'                 TO SCSBY054-COD-ERRO       
                 MOVE 'SCSB0041'             TO SCSBY054-COD-MENSAGEM   
                 PERFORM 3000-FINALIZAR                                 
              END-IF                                                    
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       1300-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
      *   PROCESSAMENTO PRINCIPAL DO PROGRAMA                          *
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
      *----------------------------------------------------------------*
       2000-PROCESSAR                     SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           PERFORM 1200-TRATAR-PAGINACAO.                               
                                                                        
           IF SCSBY054-E-ANO           NOT EQUAL ZEROS AND              
              SCSBY054-E-NREG          NOT EQUAL ZEROS                  
              PERFORM 8000-SELECT-SCSBB013                              
              PERFORM 4100-ACESSAR-UORG1334                             
              PERFORM 2901-MONTAR-BL-SAIDA-UM                           
           ELSE                                                         
              MOVE 'N'                    TO WRK-FIM-CSR                
              MOVE ZERO                   TO WRK-I                      
                                                                        
              PERFORM 2110-ABRIR-CURSOR                                 
              PERFORM 2120-LER-CURSOR                                   
                                                                        
              PERFORM UNTIL SQLCODE EQUAL +100 OR                       
                      WRK-I GREATER WRK-QTDE-LIMITE - 1                 
                                                                        
                      IF WRK-CPSSOA-JURID-ATU                           
                                          NOT EQUAL WRK-CPSSOA-JURID-ANT
                         MOVE WRK-CPSSOA-JURID-ATU                      
                                          TO WRK-CPSSOA-JURID-ANT       
                         PERFORM 4100-ACESSAR-UORG1334                  
                      END-IF                                            
                                                                        
                      PERFORM 2900-MONTAR-BL-SAIDA-VARIOS               
                                                                        
                      PERFORM 2120-LER-CURSOR                           
                                                                        
              END-PERFORM                                               
                                                                        
              PERFORM 2130-FECHAR-CURSOR                                
                                                                        
              IF WRK-I                  EQUAL ZERO                      
                 MOVE 08                  TO SCSBY054-COD-RETORNO       
                 IF SCSBY054-IND-PAGINACAO       EQUAL 'I' OR 'P'       
                    MOVE '0012'           TO SCSBY054-COD-ERRO          
                    MOVE 'SCSB0029'       TO SCSBY054-COD-MENSAGEM      
                 ELSE                                                   
                    MOVE '0013'           TO SCSBY054-COD-ERRO          
                    MOVE 'SCSB0030'       TO SCSBY054-COD-MENSAGEM      
                 END-IF                                                 
                 GO TO 2000-99-FIM                                      
              END-IF                                                    
                                                                        
              IF WRK-FIM-CSR            EQUAL 'S'                       
                 MOVE 00                  TO SCSBY054-COD-RETORNO       
              ELSE                                                      
                 MOVE 01                  TO SCSBY054-COD-RETORNO       
              END-IF                                                    
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       2000-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       2110-ABRIR-CURSOR                  SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           IF SCSBY054-IND-PAGINACAO         EQUAL 'A' OR 'U'           
              PERFORM 7015-OPEN-CSR02-SCSBB013                          
           ELSE                                                         
              PERFORM 7000-OPEN-CSR01-SCSBB013                          
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       2110-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       2120-LER-CURSOR                    SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           IF SCSBY054-IND-PAGINACAO         EQUAL 'A' OR 'U'           
              PERFORM 7020-FETCH-CSR02-SCSBB013                         
           ELSE                                                         
              PERFORM 7005-FETCH-CSR01-SCSBB013                         
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       2120-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       2130-FECHAR-CURSOR                 SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           IF SCSBY054-IND-PAGINACAO         EQUAL 'A' OR 'U'           
              PERFORM 7025-CLOSE-CSR02-SCSBB013                         
           ELSE                                                         
              PERFORM 7010-CLOSE-CSR01-SCSBB013                         
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       2130-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
      *   FORMATA BL DE SAIDA PARA VARIOS REGISTROS                    *
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
      *----------------------------------------------------------------*
       2900-MONTAR-BL-SAIDA-VARIOS        SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           ADD  1                         TO WRK-I                      
                                                                        
           IF   WRK-I                  EQUAL 1                          
               IF SCSBY054-IND-PAGINACAO   EQUAL 'A' OR 'U'             
                  MOVE DANO-COMPR-RECTA   OF SCSBB013                   
                                          TO SCSBY054-P-ULT-ANO         
                  MOVE NREG-COMPR-RECTA   OF SCSBB013                   
                                          TO SCSBY054-P-ULT-NREG        
               ELSE                                                     
                  MOVE DANO-COMPR-RECTA   OF SCSBB013                   
                                          TO SCSBY054-P-PRI-ANO         
                  MOVE NREG-COMPR-RECTA   OF SCSBB013                   
                                          TO SCSBY054-P-PRI-NREG        
               END-IF                                                   
           END-IF                                                       
                                                                        
           MOVE DANO-COMPR-RECTA          OF SCSBB013                   
             TO SCSBY054-S-ANO            (WRK-I)                       
                                                                        
           MOVE NREG-COMPR-RECTA          OF SCSBB013                   
             TO SCSBY054-S-NREG           (WRK-I)                       
                                                                        
           MOVE UORGW334-S-CNPJ-CORPO                                   
             TO SCSBY054-S-CNPJ-PRINC     (WRK-I)                       
                                                                        
           MOVE UORGW334-S-CFLIAL-CNPJ                                  
             TO SCSBY054-S-CNPJ-FLIAL     (WRK-I)                       
                                                                        
           MOVE UORGW334-S-CCTRL-CNPJ                                   
             TO SCSBY054-S-CNPJ-CTRL      (WRK-I)                       
                                                                        
           MOVE UORGW334-S-IFANTS-EMPR                                  
             TO SCSBY054-S-DESC-EMPR      (WRK-I)                       
                                                                        
           STRING HINCL-REG (9:2) '.'                                   
                  HINCL-REG (6:2) '.'                                   
                  HINCL-REG (1:4) '.'                                   
           DELIMITED BY SIZE            INTO WRK-DT-INCLUSAO            
                                                                        
           MOVE WRK-DT-INCLUSAO                                         
             TO SCSBY054-S-DT-INCLUSAO    (WRK-I).                      
                                                                        
           IF SCSBY054-IND-PAGINACAO   EQUAL 'A' OR 'U'                 
              MOVE DANO-COMPR-RECTA   OF SCSBB013                       
                                      TO SCSBY054-P-PRI-ANO             
              MOVE NREG-COMPR-RECTA   OF SCSBB013                       
                                      TO SCSBY054-P-PRI-NREG            
           ELSE                                                         
              MOVE DANO-COMPR-RECTA   OF SCSBB013                       
                                      TO SCSBY054-P-ULT-ANO             
              MOVE NREG-COMPR-RECTA   OF SCSBB013                       
                                      TO SCSBY054-P-ULT-NREG            
           END-IF.                                                      
                                                                        
           MOVE WRK-I                     TO SCSBY054-S-QTD-REG.        
                                                                        
      *----------------------------------------------------------------*
       2900-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
      *   FORMATA BL DE SAIDA PARA UM REGISTRO                         *
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
      *----------------------------------------------------------------*
       2901-MONTAR-BL-SAIDA-UM            SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           MOVE 1                         TO WRK-I                      
                                                                        
           MOVE DANO-COMPR-RECTA          OF SCSBB013                   
             TO SCSBY054-S-ANO            (WRK-I)                       
                                                                        
           MOVE NREG-COMPR-RECTA          OF SCSBB013                   
             TO SCSBY054-S-NREG           (WRK-I)                       
                                                                        
           MOVE UORGW334-S-CNPJ-CORPO                                   
             TO SCSBY054-S-CNPJ-PRINC     (WRK-I)                       
                                                                        
           MOVE UORGW334-S-CFLIAL-CNPJ                                  
             TO SCSBY054-S-CNPJ-FLIAL     (WRK-I)                       
                                                                        
           MOVE UORGW334-S-CCTRL-CNPJ                                   
             TO SCSBY054-S-CNPJ-CTRL      (WRK-I)                       
                                                                        
           MOVE UORGW334-S-IFANTS-EMPR                                  
             TO SCSBY054-S-DESC-EMPR      (WRK-I)                       
                                                                        
           STRING HINCL-REG (9:2) '.'                                   
                  HINCL-REG (6:2) '.'                                   
                  HINCL-REG (1:4) '.'                                   
           DELIMITED BY SIZE            INTO WRK-DT-INCLUSAO            
                                                                        
           MOVE WRK-DT-INCLUSAO                                         
             TO SCSBY054-S-DT-INCLUSAO    (WRK-I)                       
                                                                        
           MOVE WRK-I                     TO SCSBY054-S-QTD-REG.        
                                                                        
      *----------------------------------------------------------------*
       2901-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
      *   PROCEDIMENTOS FINAIS DO PROGRAMA                             *
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
      *----------------------------------------------------------------*
       3000-FINALIZAR                     SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           EXEC  CICS  RETURN  END-EXEC.                                
                                                                        
      *----------------------------------------------------------------*
       3000-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * VALIDAR DATA                                                    
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       4000-ACESSAR-CALE1000              SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           MOVE  '4000-ACESSAR-CALE1000'  TO  FRWKGHEA-IDEN-PARAGRAFO   
                                                                        
           INITIALIZE CALEWAAC-BLOCO-RETORNO                            
                      CALEWAAC-SAIDA.                                   
                                                                        
           EXEC CICS LINK                                               
                PROGRAM  (WRK-CALE1000)                                 
                COMMAREA (CALEWAAC)                                     
                LENGTH   (LENGTH OF CALEWAAC)                           
           END-EXEC                                                     
                                                                        
           IF EIBRESP              NOT EQUAL DFHRESP(NORMAL)            
              MOVE '0014'                 TO SCSBY054-COD-ERRO          
              PERFORM 9997-ERRO-CICS                                    
           END-IF                                                       
                                                                        
           IF CALEWAAC-COD-RET              NOT EQUAL ZEROS             
              MOVE CALEWAAC-BLOCO-RETORNO   TO WRK-BLOCO-RETORNO        
                                               SCSBY054-BLOCO-RETORNO   
              MOVE WRK-CALE1000             TO WRK-MODULO               
              PERFORM 9998-ERRO-MODULO                                  
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       4000-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       4100-ACESSAR-UORG1334              SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           MOVE '4100-ACESSAR-UORG1334'   TO FRWKGHEA-IDEN-PARAGRAFO.   
                                                                        
           INITIALIZE UORGW334-BLOCO-SAIDA.                             
                                                                        
           MOVE ZEROS                     TO UORGW334-E-CNPJ-CORPO.     
           MOVE CPSSOA-JURID              TO UORGW334-E-CEMPR.          
                                                                        
           EXEC CICS LINK                                               
                PROGRAM   (WRK-UORG1334)                                
                COMMAREA  (WRK-AREA-UORG1334)                           
                LENGTH    (LENGTH OF WRK-AREA-UORG1334)                 
           END-EXEC.                                                    
                                                                        
           IF EIBRESP                     NOT EQUAL DFHRESP(NORMAL)     
              MOVE '0015'                 TO  SCSBY054-COD-ERRO         
              PERFORM 9997-ERRO-CICS                                    
           END-IF                                                       
                                                                        
           IF UORGW000-COD-RETORNO        NOT EQUAL ZEROS               
              MOVE UORGW000-BLOCO-RETORNO TO  WRK-BLOCO-RETORNO         
                                              SCSBY054-BLOCO-RETORNO    
              MOVE WRK-UORG1334           TO  WRK-MODULO                
              PERFORM 9998-ERRO-MODULO                                  
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       4100-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       7000-OPEN-CSR01-SCSBB013           SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           MOVE '7000-OPEN-CSR01-SCSBB013'    TO FRWKGHEA-IDEN-PARAGRAFO
                                                                        
           EXEC SQL                                                     
                OPEN CSR01-SCSBB013                                     
           END-EXEC.                                                    
                                                                        
           IF  (SQLCODE             NOT EQUAL ZEROS) OR                 
               (SQLWARN0                EQUAL 'W')                      
                SET   DB2-OPEN             TO TRUE                      
                MOVE '0016'                TO SCSBY054-COD-ERRO         
                MOVE 'SCSBB013'            TO WRK-STORED-PROC           
                MOVE 'TREG_COMPR_RECTA'    TO WRK-NOME-TABELA           
                PERFORM 9996-ERRO-DB2                                   
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       7000-99-FIM.                        EXIT.                        
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       7005-FETCH-CSR01-SCSBB013          SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           MOVE '7005-FETCH-CSR01-SCSBB013'   TO FRWKGHEA-IDEN-PARAGRAFO
                                                                        
           EXEC SQL FETCH CSR01-SCSBB013 INTO                           
                     :SCSBB013.DANO-COMPR-RECTA,                        
                     :SCSBB013.NREG-COMPR-RECTA,                        
                     :SCSBB013.CPSSOA-JURID    ,                        
                     :SCSBB013.HINCL-REG                                
           END-EXEC.                                                    
                                                                        
           IF  (SQLCODE             NOT EQUAL ZEROS AND +100) OR        
               (SQLWARN0                EQUAL 'W')                      
                SET   DB2-FETCH            TO TRUE                      
                MOVE '0017'                TO SCSBY054-COD-ERRO         
                MOVE 'SCSBB013'            TO WRK-STORED-PROC           
                MOVE 'TREG_COMPR_RECTA'    TO WRK-NOME-TABELA           
                PERFORM 9996-ERRO-DB2                                   
           END-IF.                                                      
                                                                        
           IF  SQLCODE                  EQUAL +100                      
               MOVE 'S'                    TO WRK-FIM-CSR               
           ELSE                                                         
               MOVE CPSSOA-JURID        TO WRK-CPSSOA-JURID-ATU         
               IF SCSBY054-E-CUSUAR     NOT EQUAL SPACES                
                  MOVE DANO-COMPR-RECTA OF SCSBB013                     
                                        TO DANO-COMPR-RECTA             
                                        OF SCSBB036                     
                  MOVE NREG-COMPR-RECTA OF SCSBB013                     
                                        TO NREG-COMPR-RECTA             
                                        OF SCSBB036                     
                  PERFORM 8100-SELECT-SCSBB036                          
                  IF SQLCODE            EQUAL +100                      
                     GO TO 7005-FETCH-CSR01-SCSBB013                    
                  END-IF                                                
               END-IF                                                   
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       7005-99-FIM.                        EXIT.                        
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       7010-CLOSE-CSR01-SCSBB013          SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           MOVE '7010-CLOSE-CSR01-SCSBB013'   TO FRWKGHEA-IDEN-PARAGRAFO
                                                                        
           EXEC SQL                                                     
                CLOSE CSR01-SCSBB013                                    
           END-EXEC.                                                    
                                                                        
           IF  (SQLCODE             NOT EQUAL ZEROS) OR                 
               (SQLWARN0                EQUAL 'W')                      
                SET   DB2-CLOSE            TO TRUE                      
                MOVE '0018'                TO SCSBY054-COD-ERRO         
                MOVE 'SCSBB013'            TO WRK-STORED-PROC           
                MOVE 'TREG_COMPR_RECTA'    TO WRK-NOME-TABELA           
                PERFORM 9996-ERRO-DB2                                   
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       7010-99-FIM.                        EXIT.                        
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       7015-OPEN-CSR02-SCSBB013           SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           MOVE '7015-OPEN-CSR02-SCSBB013'    TO FRWKGHEA-IDEN-PARAGRAFO
                                                                        
           EXEC SQL                                                     
                OPEN CSR02-SCSBB013                                     
           END-EXEC.                                                    
                                                                        
           IF  (SQLCODE             NOT EQUAL ZEROS) OR                 
               (SQLWARN0                EQUAL 'W')                      
                SET   DB2-OPEN             TO TRUE                      
                MOVE '0019'                TO SCSBY054-COD-ERRO         
                MOVE 'SCSBB013'            TO WRK-STORED-PROC           
                MOVE 'TREG_COMPR_RECTA'    TO WRK-NOME-TABELA           
                PERFORM 9996-ERRO-DB2                                   
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       7015-99-FIM.                        EXIT.                        
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       7020-FETCH-CSR02-SCSBB013          SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           MOVE '7020-FETCH-CSR02-SCSBB013'   TO FRWKGHEA-IDEN-PARAGRAFO
                                                                        
           EXEC SQL FETCH CSR02-SCSBB013 INTO                           
                     :SCSBB013.DANO-COMPR-RECTA,                        
                     :SCSBB013.NREG-COMPR-RECTA,                        
                     :SCSBB013.CPSSOA-JURID    ,                        
                     :SCSBB013.HINCL-REG                                
           END-EXEC.                                                    
                                                                        
           IF  (SQLCODE             NOT EQUAL ZEROS AND +100) OR        
               (SQLWARN0                EQUAL 'W')                      
                SET   DB2-FETCH            TO TRUE                      
                MOVE '0020'                TO SCSBY054-COD-ERRO         
                MOVE 'SCSBB013'            TO WRK-STORED-PROC           
                MOVE 'TREG_COMPR_RECTA'    TO WRK-NOME-TABELA           
                PERFORM 9996-ERRO-DB2                                   
           END-IF.                                                      
                                                                        
           IF  SQLCODE                  EQUAL +100                      
               MOVE 'S'                    TO WRK-FIM-CSR               
           ELSE                                                         
               MOVE CPSSOA-JURID        TO WRK-CPSSOA-JURID-ATU         
               IF SCSBY054-E-CUSUAR     NOT EQUAL SPACES                
                  MOVE DANO-COMPR-RECTA OF SCSBB013                     
                                        TO DANO-COMPR-RECTA             
                                        OF SCSBB036                     
                  MOVE NREG-COMPR-RECTA OF SCSBB013                     
                                        TO NREG-COMPR-RECTA             
                                        OF SCSBB036                     
                  PERFORM 8100-SELECT-SCSBB036                          
                  IF    SQLCODE        EQUAL +100                       
                     GO TO 7020-FETCH-CSR02-SCSBB013                    
                  END-IF                                                
               END-IF                                                   
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       7020-99-FIM.                        EXIT.                        
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       7025-CLOSE-CSR02-SCSBB013          SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           MOVE '7025-CLOSE-CSR02-SCSBB013'   TO FRWKGHEA-IDEN-PARAGRAFO
                                                                        
           EXEC SQL                                                     
                CLOSE CSR02-SCSBB013                                    
           END-EXEC.                                                    
                                                                        
           IF  (SQLCODE             NOT EQUAL ZEROS) OR                 
               (SQLWARN0                EQUAL 'W')                      
                SET   DB2-CLOSE            TO TRUE                      
                MOVE '0021'                TO SCSBY054-COD-ERRO         
                MOVE 'SCSBB013'            TO WRK-STORED-PROC           
                MOVE 'TREG_COMPR_RECTA'    TO WRK-NOME-TABELA           
                PERFORM 9996-ERRO-DB2                                   
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       7025-99-FIM.                        EXIT.                        
      *----------------------------------------------------------------*                                                                        
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
      * SELECIONA APENAS UM REGISTRO                                   *
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
      *----------------------------------------------------------------*
       8000-SELECT-SCSBB013               SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           MOVE '8000-SELECT-SCSBB013'    TO FRWKGHEA-IDEN-PARAGRAFO    
                                                                        
           MOVE SCSBY054-E-ANO            TO DANO-COMPR-RECTA           
                                          OF SCSBB013                   
           MOVE SCSBY054-E-NREG           TO NREG-COMPR-RECTA           
                                          OF SCSBB013                   
                                                                        
           EXEC SQL                                                     
                SELECT DANO_COMPR_RECTA,                                
                       NREG_COMPR_RECTA,                                
                       CPSSOA_JURID    ,                                
                       HINCL_REG                                        
              INTO                                                      
               :SCSBB013.DANO-COMPR-RECTA,                              
               :SCSBB013.NREG-COMPR-RECTA,                              
               :SCSBB013.CPSSOA-JURID    ,                              
               :SCSBB013.HINCL-REG                                      
              FROM DB2PRD.TREG_COMPR_RECTA                              
              WHERE  DANO_COMPR_RECTA       = :SCSBB013.DANO-COMPR-RECTA
                AND  NREG_COMPR_RECTA       = :SCSBB013.NREG-COMPR-RECTA
                AND (CPSSOA_JURID     BETWEEN :WRK-CPSSOA-JURID-INI     
                                          AND :WRK-CPSSOA-JURID-FIM)    
                AND (CPSSOA_UND_ORGNZ BETWEEN :WRK-CPSSOA-UND-INI       
                                          AND :WRK-CPSSOA-UND-FIM)      
                AND (NSEQ_UND_ORGNZ   BETWEEN :WRK-NSEQ-UND-INI         
                                          AND :WRK-NSEQ-UND-FIM)        
                AND (HINCL_REG        BETWEEN :WRK-DATA-INI             
                                          AND :WRK-DATA-FIM)            
                AND (CSIT_REG         BETWEEN :WRK-CSIT-INI             
                                          AND :WRK-CSIT-FIM)            
           END-EXEC.                                                    
                                                                        
           IF  (SQLCODE             NOT EQUAL ZEROS AND +100) OR        
               (SQLWARN0                EQUAL 'W')                      
                SET   DB2-SELECT           TO TRUE                      
                MOVE '0024'                TO SCSBY054-COD-ERRO         
                MOVE 'SCSBB013'            TO WRK-STORED-PROC           
                MOVE 'TREG_COMPR_RECTA'    TO WRK-NOME-TABELA           
                PERFORM 9996-ERRO-DB2                                   
           END-IF.                                                      
                                                                        
           IF  SQLCODE                  EQUAL +100                      
               MOVE 08                     TO SCSBY054-COD-RETORNO      
               MOVE '0025'                 TO SCSBY054-COD-ERRO         
               MOVE 'SCSB0029'             TO SCSBY054-COD-MENSAGEM     
               PERFORM 3000-FINALIZAR                                   
           END-IF.                                                      
                                                                        
           IF SCSBY054-E-CUSUAR     NOT EQUAL SPACES                    
              MOVE DANO-COMPR-RECTA OF SCSBB013                         
                                    TO DANO-COMPR-RECTA                 
                                    OF SCSBB036                         
              MOVE NREG-COMPR-RECTA OF SCSBB013                         
                                    TO NREG-COMPR-RECTA                 
                                    OF SCSBB036                         
              PERFORM 8100-SELECT-SCSBB036                              
                                                                        
              IF  SQLCODE             EQUAL +100                        
                  MOVE 08             TO SCSBY054-COD-RETORNO           
                  MOVE '0027'         TO SCSBY054-COD-ERRO              
                  MOVE 'SCSB0029'     TO SCSBY054-COD-MENSAGEM          
                  PERFORM 3000-FINALIZAR                                
              END-IF                                                    
                                                                        
           END-IF.                                                      
      *----------------------------------------------------------------*
       8000-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
      * SELECIONA NA TABELA DE LOG PARA VERIFICAR SE USUARIO EH VALIDO *
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
      *----------------------------------------------------------------*
       8100-SELECT-SCSBB036               SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           MOVE '8100-SELECT-SCSBB038'    TO FRWKGHEA-IDEN-PARAGRAFO.   
                                                                        
           MOVE SCSBY054-E-CUSUAR         TO CUSUAR-MANUT               
                                                                        
           EXEC SQL                                                     
                SELECT DANO_COMPR_RECTA                                 
              INTO                                                      
               :SCSBB036.DANO-COMPR-RECTA                               
              FROM DB2PRD.TLOG_COMPR_RECTA                              
              WHERE DANO_COMPR_RECTA = :SCSBB036.DANO-COMPR-RECTA       
                AND NREG_COMPR_RECTA = :SCSBB036.NREG-COMPR-RECTA       
                AND CUSUAR_MANUT     = :SCSBB036.CUSUAR-MANUT           
              FETCH FIRST ROW ONLY                                      
           END-EXEC.                                                    
                                                                        
           IF  (SQLCODE             NOT EQUAL ZEROS AND +100) OR        
               (SQLWARN0                EQUAL 'W')                      
                SET   DB2-SELECT           TO TRUE                      
                MOVE '0027'                TO SCSBY054-COD-ERRO         
                MOVE 'SCSBB036'            TO WRK-STORED-PROC           
                MOVE 'TLOG_COMPR_RECTA'    TO WRK-NOME-TABELA           
                PERFORM 9996-ERRO-DB2                                   
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       8100-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
      * TRATAMENTO DE ERRO DB2.                                        *
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
      *----------------------------------------------------------------*
       9996-ERRO-DB2                      SECTION.                      
      *----------------------------------------------------------------*
                                                                        
      *--> CARREGAR DADOS NA AREA DE COMUNICACAO                        
                                                                        
           MOVE  16                        TO  SCSBY054-COD-RETORNO     
           MOVE  'SCSB0032'                TO  SCSBY054-COD-MENSAGEM    
                                                                        
      *--> CARREGAR DADOS NA AREA COMUM DE ERROS (HEADER) - I#FRWKHE    
                                                                        
           SET   ERRO-DB2                  TO  TRUE                     
           MOVE  WRK-PROGRAMA              TO  FRWKGHEA-NOME-PROGRAMA   
           MOVE  FRWKGDB2-TAM-LAYOUT       TO  FRWKGHEA-TAM-DADOS       
                                                                        
      *--> CARREGAR DADOS NA AREA DE ERRO DB2 - I#FRWKDB                
                                                                        
           MOVE  WRK-NOME-TABELA           TO  FRWKGDB2-NOME-TABELA     
           MOVE  WRK-STORED-PROC           TO  FRWKGDB2-STORED-PROC     
           MOVE  FRWKGHEA-IDEN-PARAGRAFO(1:16)                          
                                           TO  FRWKGDB2-LOCAL           
           MOVE  SQLSTATE                  TO  FRWKGDB2-SQLSTATE        
           MOVE  WRK-SQLCODE               TO  FRWKGDB2-SQLCODE         
                                                                        
      *--> MOVER AREA DE ERRO DB2 P/ BLOCO DE INFORM ESPECIF DO ERRO    
                                                                        
           MOVE  WRK-AREA-ERRO-DB2         TO  WRK-BLOCO-INFO-ERRO      
                                                                        
           PERFORM 9999-API-ERROS.                                      
                                                                        
       9996-99-FIM.                       EXIT.                         
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
      * TRATAMENTO DE ERRO - CICS                                      *
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
      *----------------------------------------------------------------*
       9997-ERRO-CICS                     SECTION.                      
      *----------------------------------------------------------------*
                                                                        
      *--> CARREGAR DADOS NA AREA DE RETORNO P/ O FRAMEWORK - I#FRWKAQ  
                                                                        
           MOVE  16                       TO  SCSBY054-COD-RETORNO      
           MOVE  'SCSB0033'               TO  SCSBY054-COD-MENSAGEM     
                                                                        
      *--> CARREGAR DADOS NA AREA COMUM DE ERROS (HEADER) - I#FRWKHE    
                                                                        
           SET   ERRO-CICS                TO  TRUE                      
           MOVE  WRK-PROGRAMA             TO  FRWKGHEA-NOME-PROGRAMA    
           MOVE  FRWKGCIC-TAM-LAYOUT      TO  FRWKGHEA-TAM-DADOS        
                                                                        
      *--> CARREGAR DADOS NA AREA DE ERRO DE CICS - I#FRWKCI            
                                                                        
           MOVE  EIBFN                    TO  FRWKGCIC-EIBFN            
           MOVE  EIBRCODE                 TO  FRWKGCIC-EIBRCODE         
           MOVE  EIBRSRCE                 TO  FRWKGCIC-EIBRSRCE         
           MOVE  EIBRESP                  TO  FRWKGCIC-EIBRESP          
           MOVE  EIBRESP2                 TO  FRWKGCIC-EIBRESP2         
           MOVE  EIBTASKN                 TO  FRWKGCIC-EIBTASKN         
                                                                        
      *--> MOVER AREA DE ERRO CICS P/ BLOCO DE INFORM ESPECIF DO ERRO   
                                                                        
           MOVE  WRK-AREA-ERRO-CICS       TO  WRK-BLOCO-INFO-ERRO       
                                                                        
           PERFORM 9999-API-ERROS.                                      
                                                                        
       9997-99-FIM.                    EXIT.                            
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
      * TRATAMENTO DE ERRO DO MODULO.                                  *
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
      *----------------------------------------------------------------*
       9998-ERRO-MODULO                   SECTION.                      
      *----------------------------------------------------------------*
                                                                        
      *--> CARREGAR DADOS NA AREA COMUM DE ERROS (HEADER) - I#FRWKHE    
                                                                        
           SET   ERRO-MODULO              TO  TRUE                      
           MOVE  WRK-PROGRAMA             TO  FRWKGHEA-NOME-PROGRAMA    
           MOVE  FRWKGMOD-TAM-LAYOUT      TO  FRWKGHEA-TAM-DADOS        
                                                                        
      *--> CARREGAR DADOS NA AREA DE ERRO DO MODULO - I#FRWKMD          
                                                                        
           MOVE  WRK-MODULO               TO  FRWKGMOD-NOME-MODULO      
           MOVE  WRK-BLOCO-RETORNO        TO  FRWKGMOD-BLOCO-RETORNO    
                                                                        
      *--> MOVER AREA DE ERRO MODULO P/ BLOCO DE INFORM ESPECIF DO ERRO 
                                                                        
           MOVE  WRK-AREA-ERRO-MODULO     TO  WRK-BLOCO-INFO-ERRO       
                                                                        
           PERFORM 9999-API-ERROS.                                      
                                                                        
       9998-99-FIM.                       EXIT.                         
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
      * CHAMA O MODULO FRWK1999 - GRAVA LOG DE ERROS                   *
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
      *----------------------------------------------------------------*
       9999-API-ERROS                     SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           EXEC CICS LINK                                               
                PROGRAM  (WRK-FRWK1999)                                 
                COMMAREA (WRK-AREA-ERRO)                                
                LENGTH   (LENGTH OF WRK-AREA-ERRO)                      
                NOHANDLE                                                
           END-EXEC                                                     
                                                                        
           IF  EIBRESP             NOT EQUAL DFHRESP(NORMAL)            
               CONTINUE                                                 
           END-IF                                                       
                                                                        
           PERFORM 3000-FINALIZAR.                                      
                                                                        
       9999-99-FIM.                       EXIT.                         
                                                                        

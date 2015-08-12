      *================================================================*
       IDENTIFICATION DIVISION.                                         
      *================================================================*
      *                                                                *
       PROGRAM-ID.    SCSB2243.                                         
       AUTHOR.        ANDERSON.                                         
      *                                                                *
      *================================================================*
      *                          BRQ IT SERVICES                       *
      *================================================================*
      *                                                                *
      *  PROGRAMADOR.......: ANDERSON MARTINS                          *
      *  ANALISTA..........: JOSE AUGUSTO TOSI                         *
      *  DATA..............: 19/02/2015                                *
      *                                                                *
      *================================================================*
      *                                                                *
      *  OBJETIVO..........: CONSULTAR LISTA RAS PARA ALTERACAO DO     *
      *                      VALOR CONVERTIDO EM REAL                  *
      *================================================================*
      *                                                                *
      * PROGRAMA : F U N C I O N A L                                   *
      *                                                                *
      *----------------------------------------------------------------*
      * I#FRWKGE COMMAREA FRWK1999 (LOG DE ERRO)                       *
      * I#FRWKHE AREA COMUM DE ERROS                                   *
      * I#FRWKMD COMMAREA FRWK1999 (LOG DE ERROS MODULO)               *
      * I#FRWKCI COMMAREA FRWK1999 (LOG DE ERROS CICS)                 *
      * SCSBY243 BLOCO DE ENTRADA E SAIDA - MODULO FUNCIONAL           *
      *                                                                *
      *================================================================*
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
                                                                        
       77  WRK-PROGRAMA            PIC  X(008) VALUE 'SCSB2243'.        
       77  WRK-FRWK1999            PIC  X(008) VALUE 'FRWK1999'.        
       77  WRK-SQLCODE             PIC S9(009) VALUE ZEROS COMP-3.      
       77  WRK-CALE1000            PIC  X(008) VALUE 'CALE1000'.        
       77  WRK-UORG1334            PIC  X(008) VALUE 'UORG1334'.        
       77  WRK-SCSB2026            PIC  X(008) VALUE 'SCSB2026'.        
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
      *  AREAS AUXILIARES                                              *
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
       01  WRK-I                   PIC  9(003) COMP-3 VALUE ZEROS.      
       01  WRK-QTDE-LIMITE         PIC  9(003) COMP-3 VALUE ZEROS.      
       01  WRK-DT-INCLUSAO         PIC  X(010)        VALUE SPACES.     
       01  WRK-DT-VALIDACAO        PIC  X(010)        VALUE SPACES.     
       01  WRK-MODULO              PIC  X(008) VALUE SPACES.            
                                                                        
       01  WRK-BLOCO-RETORNO.                                           
           05  WRK-COD-RETORNO     PIC  9(002) VALUE ZEROS.             
           05  WRK-COD-ERRO        PIC  X(004) VALUE SPACES.            
           05  WRK-COD-MENSAGEM    PIC  X(008) VALUE SPACES.            
                                                                        
       01  WRK-DANO-PSQ            PIC S9(004) COMP-3 VALUE ZEROS.      
       01  WRK-NREG-PSQ            PIC S9(009) COMP-3 VALUE ZEROS.      
       01  WRK-EMPR-INI            PIC S9(010) COMP-3 VALUE ZEROS.      
       01  WRK-EMPR-FIM            PIC S9(010) COMP-3 VALUE ZEROS.      
       01  WRK-EMPR-DEP-INI        PIC S9(010) COMP-3 VALUE ZEROS.      
       01  WRK-EMPR-DEP-FIM        PIC S9(010) COMP-3 VALUE ZEROS.      
       01  WRK-DEPEND-INI          PIC S9(008) COMP-3 VALUE ZEROS.      
       01  WRK-DEPEND-FIM          PIC S9(008) COMP-3 VALUE ZEROS.      
       01  WRK-DATA-INI            PIC  X(026) VALUE SPACES.            
       01  WRK-DATA-FIM            PIC  X(026) VALUE SPACES.            
       01  WRK-CSIT-1              PIC  X(001) VALUE SPACES.            
       01  WRK-CSIT-2              PIC  X(001) VALUE SPACES.            
       01  WRK-CSIT-3              PIC  X(001) VALUE SPACES.            
       01  WRK-CSIT-4              PIC  X(001) VALUE SPACES.            
       01  WRK-CSIT-5              PIC  X(001) VALUE SPACES.            
       01  WRK-CSIT-6              PIC  X(001) VALUE SPACES.            
                                                                        
       01  WRK-FIM-CSR             PIC  X(001) VALUE SPACES.            
       01  WRK-NOME-TABELA         PIC  X(032) VALUE SPACES.            
       01  WRK-STORED-PROC         PIC  X(008) VALUE SPACES.            
       01  WRK-DESPREZA            PIC  X(001) VALUE SPACES.            
                                                                        
       01  WRK-EMPRESA-ATU         PIC  9(010)        VALUE ZEROS.      
       01  WRK-EMPRESA-ANT         PIC  9(010)        VALUE ZEROS.      
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       77  FILLER PIC X(34) VALUE  '** AREA DA API - CALE1000 **      '.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
           COPY CALEWAAC.                                               
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       77  FILLER PIC X(34) VALUE  '** AREA DA API - UORG1334 **      '.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
       01  WRK-AREA-UORG1334.                                           
           COPY 'UORGW000'.                                             
           COPY 'UORGW334'.                                             
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       77  FILLER PIC X(34) VALUE  '** AREA DA API - SCSB2026 **      '.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
       01  WRK-AREA-SCSB2026.                                           
           COPY 'SCSBY026'.                                             
                                                                        
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
                       CPSSOA_JURID,                                    
                       CSIT_REG,                                        
                       CTPO_REG_EXTER,                                  
                       HINCL_REG,                                       
                       CORIGE_REG_RECTA,                                
                       VREG_SERVC_RECTA,                                
                VALUE (HVALDC_REG_RECTA, '0001-01-01-00.00.00.000000')  
                FROM DB2PRD.TREG_COMPR_RECTA                            
               WHERE ((DANO_COMPR_RECTA       >   :WRK-DANO-PSQ)        
                  OR  (DANO_COMPR_RECTA       =   :WRK-DANO-PSQ         
                 AND   NREG_COMPR_RECTA       >   :WRK-NREG-PSQ))       
                 AND (CPSSOA_JURID       BETWEEN  :WRK-EMPR-INI         
                                            AND   :WRK-EMPR-FIM)        
                 AND (CPSSOA_UND_ORGNZ   BETWEEN  :WRK-EMPR-DEP-INI     
                                            AND   :WRK-EMPR-DEP-FIM)    
                 AND (NSEQ_UND_ORGNZ     BETWEEN  :WRK-DEPEND-INI       
                                            AND   :WRK-DEPEND-FIM)      
                 AND (HINCL_REG          BETWEEN  :WRK-DATA-INI         
                                             AND  :WRK-DATA-FIM)        
                 AND (CSIT_REG           IN (:WRK-CSIT-1, :WRK-CSIT-2,  
                                             :WRK-CSIT-3, :WRK-CSIT-4,  
                                             :WRK-CSIT-5, :WRK-CSIT-6)) 
                 AND (CTPO_REG_EXTER         =    1                     
                  OR  CTPO_REG_EXTER         =    3)                    
              ORDER BY DANO_COMPR_RECTA,                                
                       NREG_COMPR_RECTA                                 
      *       FETCH FIRST 51 ROWS ONLY                                  
           END-EXEC.                                                    
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
      *   DECLARACAO DO CURSOR - CSR02-SCSBB013                        *
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
           EXEC SQL DECLARE CSR02-SCSBB013 CURSOR FOR SELECT            
                       DANO_COMPR_RECTA,                                
                       NREG_COMPR_RECTA,                                
                       CPSSOA_JURID,                                    
                       CSIT_REG,                                        
                       CTPO_REG_EXTER,                                  
                       HINCL_REG,                                       
                       CORIGE_REG_RECTA,                                
                       VREG_SERVC_RECTA,                                
                VALUE (HVALDC_REG_RECTA, '0001-01-01-00.00.00.000000')  
                FROM DB2PRD.TREG_COMPR_RECTA                            
               WHERE ((DANO_COMPR_RECTA       <   :WRK-DANO-PSQ)        
                  OR  (DANO_COMPR_RECTA       =   :WRK-DANO-PSQ         
                 AND   NREG_COMPR_RECTA       <   :WRK-NREG-PSQ))       
                 AND (CPSSOA_JURID       BETWEEN  :WRK-EMPR-INI         
                                            AND   :WRK-EMPR-FIM)        
                 AND (CPSSOA_UND_ORGNZ   BETWEEN  :WRK-EMPR-DEP-INI     
                                            AND   :WRK-EMPR-DEP-FIM)    
                 AND (NSEQ_UND_ORGNZ     BETWEEN  :WRK-DEPEND-INI       
                                            AND   :WRK-DEPEND-FIM)      
                 AND (HINCL_REG          BETWEEN  :WRK-DATA-INI         
                                             AND  :WRK-DATA-FIM)        
                 AND (CSIT_REG            IN (:WRK-CSIT-1, :WRK-CSIT-2, 
                                              :WRK-CSIT-3, :WRK-CSIT-4, 
                                              :WRK-CSIT-5, :WRK-CSIT-6))
                 AND (CTPO_REG_EXTER         =    1                     
                  OR  CTPO_REG_EXTER         =    3)                    
              ORDER BY DANO_COMPR_RECTA DESC,                           
                       NREG_COMPR_RECTA DESC                            
      *       FETCH FIRST 51 ROWS ONLY                                  
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
           COPY SCSBY243.                                               
                                                                        
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
                                                                        
           MOVE ZEROS                     TO SCSBY243-COD-RETORNO       
           MOVE '0000'                    TO SCSBY243-COD-ERRO          
           MOVE 'SCSB0026'                TO SCSBY243-COD-MENSAGEM.     
                                                                        
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
               WHEN SCSBY243-P-INICIAL                                  
               WHEN SCSBY243-P-PRIMEIRA                                 
                                                                        
                    CONTINUE                                            
                                                                        
               WHEN SCSBY243-P-ULTIMA                                   
                                                                        
                    MOVE SCSBY243-P-ULT-ANO   TO WRK-DANO-PSQ           
                    MOVE SCSBY243-P-ULT-NREG  TO WRK-NREG-PSQ           
                                                                        
               WHEN SCSBY243-P-SEGUINTE                                 
                                                                        
                    MOVE SCSBY243-P-ULT-ANO   TO WRK-DANO-PSQ           
                    MOVE SCSBY243-P-ULT-NREG  TO WRK-NREG-PSQ           
                                                                        
               WHEN SCSBY243-P-ANTERIOR                                 
                                                                        
                    MOVE SCSBY243-P-PRI-ANO   TO WRK-DANO-PSQ           
                    MOVE SCSBY243-P-PRI-NREG  TO WRK-NREG-PSQ           
                                                                        
           END-EVALUATE.                                                
                                                                        
      *----------------------------------------------------------------*
       1200-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       1201-FORMATA-CAMPOS                SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           IF SCSBY243-E-EMPR             EQUAL ZEROS                   
              MOVE ZEROS                     TO WRK-EMPR-INI            
              MOVE 9999999999                TO WRK-EMPR-FIM            
           ELSE                                                         
              MOVE SCSBY243-E-EMPR           TO WRK-EMPR-INI            
              MOVE SCSBY243-E-EMPR           TO WRK-EMPR-FIM            
           END-IF                                                       
                                                                        
           IF SCSBY243-E-EMPR-DEPEND      EQUAL ZEROS                   
              MOVE ZEROS                     TO WRK-EMPR-DEP-INI        
              MOVE 9999999999                TO WRK-EMPR-DEP-FIM        
           ELSE                                                         
              MOVE SCSBY243-E-EMPR-DEPEND    TO WRK-EMPR-DEP-INI        
              MOVE SCSBY243-E-EMPR-DEPEND    TO WRK-EMPR-DEP-FIM        
           END-IF                                                       
                                                                        
           IF SCSBY243-E-DEPEND           EQUAL ZEROS                   
              MOVE ZEROS                     TO WRK-DEPEND-INI          
              MOVE 99999999                  TO WRK-DEPEND-FIM          
           ELSE                                                         
              MOVE SCSBY243-E-DEPEND         TO WRK-DEPEND-INI          
              MOVE SCSBY243-E-DEPEND         TO WRK-DEPEND-FIM          
           END-IF                                                       
                                                                        
           IF SCSBY243-E-DATA-INI         EQUAL SPACES                  
              MOVE '0001-01-01-00.00.00.000000'                         
                                          TO WRK-DATA-INI               
              MOVE '9999-12-31-23.59.59.999999'                         
                                          TO WRK-DATA-FIM               
           ELSE                                                         
              STRING SCSBY243-E-DATA-INI (7:4) '-'                      
                     SCSBY243-E-DATA-INI (4:2) '-'                      
                     SCSBY243-E-DATA-INI (1:2) '-'                      
              DELIMITED BY SIZE         INTO WRK-DATA-INI(1:10)         
              MOVE '-00.00.00.000000'     TO WRK-DATA-INI(11:16)        
                                                                        
              STRING SCSBY243-E-DATA-FIM (7:4) '-'                      
                     SCSBY243-E-DATA-FIM (4:2) '-'                      
                     SCSBY243-E-DATA-FIM (1:2) '-'                      
              DELIMITED BY SIZE         INTO WRK-DATA-FIM(1:10)         
              MOVE '-23.59.59.999999'     TO WRK-DATA-FIM(11:16)        
           END-IF                                                       
                                                                        
           IF SCSBY243-E-SITUACAO         NOT EQUAL SPACES              
              MOVE SCSBY243-E-SITUACAO    TO WRK-CSIT-1                 
                                             WRK-CSIT-2                 
                                             WRK-CSIT-3                 
                                             WRK-CSIT-4                 
                                             WRK-CSIT-5                 
                                             WRK-CSIT-6                 
           ELSE                                                         
              MOVE 'P'                    TO WRK-CSIT-1                 
              MOVE 'V'                    TO WRK-CSIT-2                 
              MOVE 'R'                    TO WRK-CSIT-3                 
              MOVE 'E'                    TO WRK-CSIT-4                 
              MOVE 'T'                    TO WRK-CSIT-5                 
              MOVE 'F'                    TO WRK-CSIT-6                 
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
                                                                        
           IF SCSBY243-IND-PAGINACAO    NOT EQUAL 'I' AND 'P' AND 'S'   
                                                      AND 'A' AND 'U'   
              MOVE 08                     TO SCSBY243-COD-RETORNO       
              MOVE '0001'                 TO SCSBY243-COD-ERRO          
              MOVE 'SCSB0039'             TO SCSBY243-COD-MENSAGEM      
              PERFORM 3000-FINALIZAR                                    
           END-IF.                                                      
                                                                        
           IF SCSBY243-E-ANO             NOT NUMERIC                    
              MOVE 08                     TO SCSBY243-COD-RETORNO       
              MOVE '0002'                 TO SCSBY243-COD-ERRO          
              MOVE 'SCSB0037'             TO SCSBY243-COD-MENSAGEM      
              PERFORM 3000-FINALIZAR                                    
           END-IF.                                                      
                                                                        
           IF SCSBY243-E-NREG            NOT NUMERIC                    
              MOVE 08                     TO SCSBY243-COD-RETORNO       
              MOVE '0003'                 TO SCSBY243-COD-ERRO          
              MOVE 'SCSB0037'             TO SCSBY243-COD-MENSAGEM      
              PERFORM 3000-FINALIZAR                                    
           END-IF.                                                      
                                                                        
           IF (SCSBY243-E-ANO            EQUAL   ZEROS) AND             
              (SCSBY243-E-NREG           EQUAL   ZEROS)                 
                                                                        
              IF (SCSBY243-E-EMPR        NOT NUMERIC) OR                
                 (SCSBY243-E-EMPR        NOT GREATER ZEROS)             
                  MOVE 08                 TO SCSBY243-COD-RETORNO       
                  MOVE '0004'             TO SCSBY243-COD-ERRO          
                  MOVE 'SCSB0001'         TO SCSBY243-COD-MENSAGEM      
                  PERFORM 3000-FINALIZAR                                
              END-IF                                                    
                                                                        
              IF (SCSBY243-E-EMPR-DEPEND NOT NUMERIC) OR                
                 (SCSBY243-E-EMPR-DEPEND NOT GREATER ZEROS)             
                  MOVE 08                 TO SCSBY243-COD-RETORNO       
                  MOVE '0005'             TO SCSBY243-COD-ERRO          
                  MOVE 'SCSB0001'         TO SCSBY243-COD-MENSAGEM      
                  PERFORM 3000-FINALIZAR                                
              END-IF                                                    
                                                                        
              IF (SCSBY243-E-DEPEND      NOT NUMERIC) OR                
                 (SCSBY243-E-DEPEND      NOT GREATER ZEROS)             
                  MOVE 08                 TO SCSBY243-COD-RETORNO       
                  MOVE '0006'             TO SCSBY243-COD-ERRO          
                  MOVE 'SCSB0002'         TO SCSBY243-COD-MENSAGEM      
                  PERFORM 3000-FINALIZAR                                
              END-IF                                                    
                                                                        
              IF (SCSBY243-E-DATA-INI    EQUAL SPACES OR LOW-VALUES)    
                  MOVE 08                 TO SCSBY243-COD-RETORNO       
                  MOVE '0007'             TO SCSBY243-COD-ERRO          
                  MOVE 'SCSB0031'         TO SCSBY243-COD-MENSAGEM      
                  PERFORM 3000-FINALIZAR                                
              END-IF                                                    
                                                                        
              IF (SCSBY243-E-DATA-FIM    EQUAL SPACES OR LOW-VALUES)    
                  MOVE 08                 TO SCSBY243-COD-RETORNO       
                  MOVE '0008'             TO SCSBY243-COD-ERRO          
                  MOVE 'SCSB0031'         TO SCSBY243-COD-MENSAGEM      
                  PERFORM 3000-FINALIZAR                                
              END-IF                                                    
                                                                        
      *---> VALIDA DATAS                                                
                                                                        
              MOVE  007                   TO CALEWAAC-FMT-ARGMTO        
              MOVE  SCSBY243-E-DATA-INI   TO CALEWAAC-VLR-ARGMTO        
              PERFORM 4000-ACESSAR-CALE1000                             
                                                                        
              IF CALE-INCONSISTENTE                                     
                 MOVE 08                   TO SCSBY243-COD-RETORNO      
                 MOVE '0009'               TO SCSBY243-COD-ERRO         
                 MOVE 'SCSB0041'           TO SCSBY243-COD-MENSAGEM     
                 PERFORM 3000-FINALIZAR                                 
              END-IF                                                    
                                                                        
              MOVE  007                   TO CALEWAAC-FMT-ARGMTO        
              MOVE  SCSBY243-E-DATA-FIM   TO CALEWAAC-VLR-ARGMTO        
              PERFORM 4000-ACESSAR-CALE1000                             
                                                                        
              IF CALE-INCONSISTENTE                                     
                 MOVE 08                   TO SCSBY243-COD-RETORNO      
                 MOVE '0010'               TO SCSBY243-COD-ERRO         
                 MOVE 'SCSB0041'           TO SCSBY243-COD-MENSAGEM     
                 PERFORM 3000-FINALIZAR                                 
              END-IF                                                    
                                                                        
           ELSE                                                         
                                                                        
              IF (SCSBY243-E-EMPR        NOT NUMERIC)                   
                  MOVE 08                 TO SCSBY243-COD-RETORNO       
                  MOVE '0011'             TO SCSBY243-COD-ERRO          
                  MOVE 'SCSB0001'         TO SCSBY243-COD-MENSAGEM      
                  PERFORM 3000-FINALIZAR                                
              END-IF                                                    
                                                                        
              IF (SCSBY243-E-EMPR-DEPEND NOT NUMERIC)                   
                  MOVE 08                 TO SCSBY243-COD-RETORNO       
                  MOVE '0012'             TO SCSBY243-COD-ERRO          
                  MOVE 'SCSB0001'         TO SCSBY243-COD-MENSAGEM      
                  PERFORM 3000-FINALIZAR                                
              END-IF                                                    
                                                                        
              IF (SCSBY243-E-DEPEND      NOT NUMERIC)                   
                  MOVE 08                 TO SCSBY243-COD-RETORNO       
                  MOVE '0013'             TO SCSBY243-COD-ERRO          
                  MOVE 'SCSB0002'         TO SCSBY243-COD-MENSAGEM      
                  PERFORM 3000-FINALIZAR                                
              END-IF                                                    
                                                                        
           END-IF.                                                      
                                                                        
           IF SCSBY243-E-SITUACAO        NOT EQUAL SPACES               
                                                                        
              IF SCSBY243-E-SITUACAO     NOT EQUAL 'P' AND 'V' AND 'R'  
                                               AND 'E' AND 'T' AND 'F'  
                 MOVE 08                  TO SCSBY243-COD-RETORNO       
                 MOVE '0014'              TO SCSBY243-COD-ERRO          
                 MOVE 'SCSB0060'          TO SCSBY243-COD-MENSAGEM      
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
                                                                        
           IF SCSBY243-E-ANO           NOT EQUAL ZEROS AND              
              SCSBY243-E-NREG          NOT EQUAL ZEROS                  
              PERFORM 8000-SELECT-SCSBB013                              
              PERFORM 2901-MONTAR-BL-SAIDA-UM                           
           ELSE                                                         
              PERFORM 1200-TRATAR-PAGINACAO                             
              MOVE 'N'                    TO WRK-FIM-CSR                
              MOVE ZERO                   TO WRK-I                      
                                                                        
              PERFORM 2110-ABRIR-CURSOR                                 
              PERFORM 2120-LER-CURSOR                                   
                                                                        
              PERFORM UNTIL SQLCODE EQUAL +100 OR                       
                      WRK-I GREATER WRK-QTDE-LIMITE - 1                 
                                                                        
                      PERFORM 2900-MONTAR-BL-SAIDA-VARIOS               
                                                                        
                      PERFORM 2120-LER-CURSOR                           
                                                                        
              END-PERFORM                                               
                                                                        
              PERFORM 2130-FECHAR-CURSOR                                
                                                                        
              IF WRK-I                  EQUAL ZERO                      
                 MOVE 08                  TO SCSBY243-COD-RETORNO       
                 IF SCSBY243-IND-PAGINACAO       EQUAL 'I' OR 'P'       
                    MOVE '0015'           TO SCSBY243-COD-ERRO          
                    MOVE 'SCSB0029'       TO SCSBY243-COD-MENSAGEM      
                 ELSE                                                   
                    MOVE '0016'           TO SCSBY243-COD-ERRO          
                    MOVE 'SCSB0030'       TO SCSBY243-COD-MENSAGEM      
                 END-IF                                                 
                 GO TO 2000-99-FIM                                      
              END-IF                                                    
                                                                        
              IF WRK-FIM-CSR            EQUAL 'S'                       
                 MOVE 00                  TO SCSBY243-COD-RETORNO       
              ELSE                                                      
                 MOVE 01                  TO SCSBY243-COD-RETORNO       
              END-IF                                                    
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       2000-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       2110-ABRIR-CURSOR                  SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           IF SCSBY243-IND-PAGINACAO         EQUAL 'A' OR 'U'           
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
                                                                        
           IF SCSBY243-IND-PAGINACAO         EQUAL 'A' OR 'U'           
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
                                                                        
           IF SCSBY243-IND-PAGINACAO         EQUAL 'A' OR 'U'           
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
                                                                        
           IF SCSBY243-E-CUSUAR           NOT EQUAL SPACES              
                                                                        
              PERFORM 8050-SELECT-SCSBB036                              
                                                                        
              IF WRK-DESPREZA             EQUAL 'S'                     
                 GO TO 2900-99-FIM                                      
              END-IF                                                    
                                                                        
           END-IF.                                                      
                                                                        
           ADD  1                         TO WRK-I                      
                                                                        
           IF   WRK-I                  EQUAL 1                          
               IF SCSBY243-IND-PAGINACAO   EQUAL 'A' OR 'U'             
                  MOVE DANO-COMPR-RECTA   OF SCSBB013                   
                    TO SCSBY243-P-ULT-ANO                               
                  MOVE NREG-COMPR-RECTA   OF SCSBB013                   
                    TO SCSBY243-P-ULT-NREG                              
               ELSE                                                     
                  MOVE DANO-COMPR-RECTA   OF SCSBB013                   
                    TO SCSBY243-P-PRI-ANO                               
                  MOVE NREG-COMPR-RECTA   OF SCSBB013                   
                    TO SCSBY243-P-PRI-NREG                              
               END-IF                                                   
           END-IF                                                       
                                                                        
           MOVE DANO-COMPR-RECTA          OF SCSBB013                   
             TO SCSBY243-S-ANO            (WRK-I)                       
                                                                        
           MOVE NREG-COMPR-RECTA          OF SCSBB013                   
             TO SCSBY243-S-NREG           (WRK-I)                       
                                                                        
           IF WRK-EMPRESA-ATU             NOT EQUAL WRK-EMPRESA-ANT     
              PERFORM 8100-ACESSAR-UORG1334                             
              MOVE WRK-EMPRESA-ATU        TO WRK-EMPRESA-ANT            
           END-IF.                                                      
                                                                        
           MOVE UORGW334-S-CNPJ-CORPO                                   
             TO SCSBY243-S-CNPJ-PRINC     (WRK-I)                       
                                                                        
           MOVE UORGW334-S-CFLIAL-CNPJ                                  
             TO SCSBY243-S-CNPJ-FLIAL     (WRK-I)                       
                                                                        
           MOVE UORGW334-S-CCTRL-CNPJ                                   
             TO SCSBY243-S-CNPJ-CTRL      (WRK-I)                       
                                                                        
           MOVE UORGW334-S-IFANTS-EMPR                                  
             TO SCSBY243-S-DESC-EMPR      (WRK-I)                       
                                                                        
           STRING HINCL-REG OF SCSBB013 (9:2) '.'                       
                  HINCL-REG OF SCSBB013 (6:2) '.'                       
                  HINCL-REG OF SCSBB013 (1:4) '.'                       
           DELIMITED BY SIZE            INTO WRK-DT-INCLUSAO            
                                                                        
           MOVE WRK-DT-INCLUSAO                                         
             TO SCSBY243-S-DT-INCLUSAO    (WRK-I)                       
                                                                        
           MOVE VREG-SERVC-RECTA          OF SCSBB013                   
             TO SCSBY243-S-VLR-CONV-REAL  (WRK-I)                       
                                                                        
           MOVE CSIT-REG                  OF SCSBB013                   
             TO SCSBY243-S-SIT-REG        (WRK-I)                       
                SCSBY026-E-CD-SITUACAO                                  
                                                                        
           PERFORM 8200-ACESSAR-SCSB2026                                
                                                                        
           MOVE SCSBY026-S-DS-SITUACAO(1)                               
             TO SCSBY243-S-DS-SIT-REG     (WRK-I)                       
                                                                        
           IF CORIGE-REG-RECTA OF SCSBB013 EQUAL 'U'                    
              MOVE 'INCLUIDO POR USUARIO'                               
                TO SCSBY243-S-ORIG-REG    (WRK-I)                       
           ELSE                                                         
              MOVE 'INCLUIDO VIA UPLOAD'                                
                TO SCSBY243-S-ORIG-REG    (WRK-I)                       
           END-IF.                                                      
                                                                        
           STRING HVALDC-REG-RECTA OF SCSBB013 (9:2) '.'                
                  HVALDC-REG-RECTA OF SCSBB013 (6:2) '.'                
                  HVALDC-REG-RECTA OF SCSBB013 (1:4) '.'                
           DELIMITED BY SIZE            INTO WRK-DT-VALIDACAO           
                                                                        
           MOVE WRK-DT-VALIDACAO                                        
             TO SCSBY243-S-DT-VALIDACAO   (WRK-I)                       
                                                                        
           MOVE CTPO-REG-EXTER            OF SCSBB013                   
             TO SCSBY243-S-TPO-REG        (WRK-I)                       
                                                                        
           IF SCSBY243-IND-PAGINACAO   EQUAL 'A' OR 'U'                 
              MOVE DANO-COMPR-RECTA   OF SCSBB013                       
                TO SCSBY243-P-PRI-ANO                                   
              MOVE NREG-COMPR-RECTA   OF SCSBB013                       
                TO SCSBY243-P-PRI-NREG                                  
           ELSE                                                         
              MOVE DANO-COMPR-RECTA   OF SCSBB013                       
                TO SCSBY243-P-ULT-ANO                                   
              MOVE NREG-COMPR-RECTA   OF SCSBB013                       
                TO SCSBY243-P-ULT-NREG                                  
           END-IF.                                                      
                                                                        
           MOVE WRK-I                     TO SCSBY243-S-QTD-REG.        
                                                                        
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
             TO SCSBY243-S-ANO            (WRK-I)                       
                                                                        
           MOVE NREG-COMPR-RECTA          OF SCSBB013                   
             TO SCSBY243-S-NREG           (WRK-I)                       
                                                                        
           PERFORM 8100-ACESSAR-UORG1334                                
                                                                        
           MOVE UORGW334-S-CNPJ-CORPO                                   
             TO SCSBY243-S-CNPJ-PRINC     (WRK-I)                       
                                                                        
           MOVE UORGW334-S-CFLIAL-CNPJ                                  
             TO SCSBY243-S-CNPJ-FLIAL     (WRK-I)                       
                                                                        
           MOVE UORGW334-S-CCTRL-CNPJ                                   
             TO SCSBY243-S-CNPJ-CTRL      (WRK-I)                       
                                                                        
           MOVE UORGW334-S-IFANTS-EMPR                                  
             TO SCSBY243-S-DESC-EMPR      (WRK-I)                       
                                                                        
           STRING HINCL-REG OF SCSBB013 (9:2) '.'                       
                  HINCL-REG OF SCSBB013 (6:2) '.'                       
                  HINCL-REG OF SCSBB013 (1:4) '.'                       
           DELIMITED BY SIZE            INTO WRK-DT-INCLUSAO            
                                                                        
           MOVE WRK-DT-INCLUSAO                                         
             TO SCSBY243-S-DT-INCLUSAO    (WRK-I)                       
                                                                        
           MOVE VREG-SERVC-RECTA          OF SCSBB013                   
             TO SCSBY243-S-VLR-CONV-REAL  (WRK-I)                       
                                                                        
           MOVE CSIT-REG                  OF SCSBB013                   
             TO SCSBY243-S-SIT-REG        (WRK-I)                       
                SCSBY026-E-CD-SITUACAO                                  
                                                                        
           PERFORM 8200-ACESSAR-SCSB2026                                
                                                                        
           MOVE SCSBY026-S-DS-SITUACAO(1)                               
             TO SCSBY243-S-DS-SIT-REG     (WRK-I)                       
                                                                        
           IF CORIGE-REG-RECTA OF SCSBB013 EQUAL 'U'                    
              MOVE 'INCLUIDO POR USUARIO'                               
                TO SCSBY243-S-ORIG-REG    (WRK-I)                       
           ELSE                                                         
              MOVE 'INCLUIDO VIA UPLOAD'                                
                TO SCSBY243-S-ORIG-REG    (WRK-I)                       
           END-IF.                                                      
                                                                        
           STRING HVALDC-REG-RECTA OF SCSBB013 (9:2) '.'                
                  HVALDC-REG-RECTA OF SCSBB013 (6:2) '.'                
                  HVALDC-REG-RECTA OF SCSBB013 (1:4) '.'                
           DELIMITED BY SIZE            INTO WRK-DT-VALIDACAO           
                                                                        
           MOVE WRK-DT-VALIDACAO                                        
             TO SCSBY243-S-DT-VALIDACAO   (WRK-I)                       
                                                                        
           MOVE CTPO-REG-EXTER            OF SCSBB013                   
             TO SCSBY243-S-TPO-REG        (WRK-I)                       
                                                                        
           MOVE WRK-I                     TO SCSBY243-S-QTD-REG.        
                                                                        
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
              MOVE '0017'                 TO SCSBY243-COD-ERRO          
              PERFORM 9997-ERRO-CICS                                    
           END-IF                                                       
                                                                        
           IF CALEWAAC-COD-RET              NOT EQUAL ZEROS             
              MOVE CALEWAAC-BLOCO-RETORNO   TO WRK-BLOCO-RETORNO        
                                               SCSBY243-BLOCO-RETORNO   
              MOVE WRK-CALE1000             TO WRK-MODULO               
              PERFORM 9998-ERRO-MODULO                                  
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       4000-99-FIM.                       EXIT.                         
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
                MOVE '0018'                TO SCSBY243-COD-ERRO         
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
                     :SCSBB013.CPSSOA-JURID,                            
                     :SCSBB013.CSIT-REG,                                
                     :SCSBB013.CTPO-REG-EXTER,                          
                     :SCSBB013.HINCL-REG,                               
                     :SCSBB013.CORIGE-REG-RECTA,                        
                     :SCSBB013.VREG-SERVC-RECTA,                        
                     :SCSBB013.HVALDC-REG-RECTA                         
           END-EXEC.                                                    
                                                                        
           IF  (SQLCODE             NOT EQUAL ZEROS AND +100) OR        
               (SQLWARN0                EQUAL 'W')                      
                SET   DB2-FETCH            TO TRUE                      
                MOVE '0019'                TO SCSBY243-COD-ERRO         
                MOVE 'SCSBB013'            TO WRK-STORED-PROC           
                MOVE 'TREG_COMPR_RECTA'    TO WRK-NOME-TABELA           
                PERFORM 9996-ERRO-DB2                                   
           END-IF.                                                      
                                                                        
           IF  SQLCODE                  EQUAL +100                      
               MOVE 'S'                    TO WRK-FIM-CSR               
           ELSE                                                         
               MOVE 'N'                    TO WRK-FIM-CSR               
           END-IF.                                                      
                                                                        
           MOVE CPSSOA-JURID               OF SCSBB013                  
             TO WRK-EMPRESA-ATU.                                        
                                                                        
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
                MOVE '0020'                TO SCSBY243-COD-ERRO         
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
                MOVE '0021'                TO SCSBY243-COD-ERRO         
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
                     :SCSBB013.CPSSOA-JURID,                            
                     :SCSBB013.CSIT-REG,                                
                     :SCSBB013.CTPO-REG-EXTER,                          
                     :SCSBB013.HINCL-REG,                               
                     :SCSBB013.CORIGE-REG-RECTA,                        
                     :SCSBB013.VREG-SERVC-RECTA,                        
                     :SCSBB013.HVALDC-REG-RECTA                         
           END-EXEC.                                                    
                                                                        
           IF  (SQLCODE             NOT EQUAL ZEROS AND +100) OR        
               (SQLWARN0                EQUAL 'W')                      
                SET   DB2-FETCH            TO TRUE                      
                MOVE '0022'                TO SCSBY243-COD-ERRO         
                MOVE 'SCSBB013'            TO WRK-STORED-PROC           
                MOVE 'TREG_COMPR_RECTA'    TO WRK-NOME-TABELA           
                PERFORM 9996-ERRO-DB2                                   
           END-IF.                                                      
                                                                        
           IF  SQLCODE                  EQUAL +100                      
               MOVE 'S'                    TO WRK-FIM-CSR               
           ELSE                                                         
               MOVE 'N'                    TO WRK-FIM-CSR               
           END-IF.                                                      
                                                                        
           MOVE CPSSOA-JURID               OF SCSBB013                  
             TO WRK-EMPRESA-ATU.                                        
                                                                        
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
                MOVE '0023'                TO SCSBY243-COD-ERRO         
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
                                                                        
           MOVE SCSBY243-E-ANO                                          
             TO DANO-COMPR-RECTA          OF SCSBB013                   
           MOVE SCSBY243-E-NREG                                         
             TO NREG-COMPR-RECTA          OF SCSBB013                   
                                                                        
           IF SCSBY243-E-SITUACAO         NOT EQUAL SPACES              
              MOVE SCSBY243-E-SITUACAO    TO WRK-CSIT-1                 
                                             WRK-CSIT-2                 
                                             WRK-CSIT-3                 
                                             WRK-CSIT-4                 
                                             WRK-CSIT-5                 
                                             WRK-CSIT-6                 
           ELSE                                                         
              MOVE 'P'                    TO WRK-CSIT-1                 
              MOVE 'V'                    TO WRK-CSIT-2                 
              MOVE 'R'                    TO WRK-CSIT-3                 
              MOVE 'E'                    TO WRK-CSIT-4                 
              MOVE 'T'                    TO WRK-CSIT-5                 
              MOVE 'F'                    TO WRK-CSIT-6                 
           END-IF.                                                      
                                                                        
           EXEC SQL                                                     
                SELECT DANO_COMPR_RECTA,                                
                       NREG_COMPR_RECTA,                                
                       CPSSOA_JURID,                                    
                       CSIT_REG,                                        
                       CTPO_REG_EXTER,                                  
                       HINCL_REG,                                       
                       CORIGE_REG_RECTA,                                
                       VREG_SERVC_RECTA,                                
                VALUE (HVALDC_REG_RECTA, '0001-01-01-00.00.00.000000')  
              INTO                                                      
               :SCSBB013.DANO-COMPR-RECTA,                              
               :SCSBB013.NREG-COMPR-RECTA,                              
               :SCSBB013.CPSSOA-JURID,                                  
               :SCSBB013.CSIT-REG,                                      
               :SCSBB013.CTPO-REG-EXTER,                                
               :SCSBB013.HINCL-REG,                                     
               :SCSBB013.CORIGE-REG-RECTA,                              
               :SCSBB013.VREG-SERVC-RECTA,                              
               :SCSBB013.HVALDC-REG-RECTA                               
              FROM DB2PRD.TREG_COMPR_RECTA                              
              WHERE  DANO_COMPR_RECTA   = :SCSBB013.DANO-COMPR-RECTA    
                AND  NREG_COMPR_RECTA   = :SCSBB013.NREG-COMPR-RECTA    
                AND (CSIT_REG         IN (:WRK-CSIT-1, :WRK-CSIT-2,     
                                          :WRK-CSIT-3, :WRK-CSIT-4,     
                                          :WRK-CSIT-5, :WRK-CSIT-6))    
                AND (CTPO_REG_EXTER     = 1                             
                 OR  CTPO_REG_EXTER     = 3)                            
           END-EXEC.                                                    
                                                                        
           IF  (SQLCODE             NOT EQUAL ZEROS AND +100) OR        
               (SQLWARN0                EQUAL 'W')                      
                SET   DB2-SELECT           TO TRUE                      
                MOVE '0026'                TO SCSBY243-COD-ERRO         
                MOVE 'SCSBB013'            TO WRK-STORED-PROC           
                MOVE 'TREG_COMPR_RECTA'    TO WRK-NOME-TABELA           
                PERFORM 9996-ERRO-DB2                                   
           END-IF.                                                      
                                                                        
           IF  SQLCODE                  EQUAL +100                      
               MOVE 08                     TO SCSBY243-COD-RETORNO      
               MOVE '0027'                 TO SCSBY243-COD-ERRO         
               MOVE 'SCSB0029'             TO SCSBY243-COD-MENSAGEM     
               PERFORM 3000-FINALIZAR                                   
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       8000-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
      * SELECIONA DADOS NA TABELA DE LOG                               *
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
      *----------------------------------------------------------------*
       8050-SELECT-SCSBB036               SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           MOVE '8050-SELECT-SCSBB036'    TO FRWKGHEA-IDEN-PARAGRAFO    
                                                                        
           MOVE DANO-COMPR-RECTA          OF SCSBB013                   
             TO DANO-COMPR-RECTA          OF SCSBB036                   
           MOVE NREG-COMPR-RECTA          OF SCSBB013                   
             TO NREG-COMPR-RECTA          OF SCSBB036                   
           MOVE SCSBY243-E-CUSUAR                                       
             TO CUSUAR-MANUT              OF SCSBB036                   
                                                                        
           EXEC SQL                                                     
                SELECT DANO_COMPR_RECTA,                                
                       NREG_COMPR_RECTA,                                
                       HMANUT_REG,                                      
                       CTPO_SERVC_LOG,                                  
                       CMANUT_LOG_RECTA,                                
                       CFUNCL_SERVC_EXTER,                              
                       CSIT_REG,                                        
                       CUSUAR_MANUT                                     
              INTO                                                      
               :SCSBB036.DANO-COMPR-RECTA,                              
               :SCSBB036.NREG-COMPR-RECTA,                              
               :SCSBB036.HMANUT-REG,                                    
               :SCSBB036.CTPO-SERVC-LOG,                                
               :SCSBB036.CMANUT-LOG-RECTA,                              
               :SCSBB036.CFUNCL-SERVC-EXTER,                            
               :SCSBB036.CSIT-REG,                                      
               :SCSBB036.CUSUAR-MANUT                                   
              FROM DB2PRD.TLOG_COMPR_RECTA                              
              WHERE DANO_COMPR_RECTA   = :SCSBB036.DANO-COMPR-RECTA     
                AND NREG_COMPR_RECTA   = :SCSBB036.NREG-COMPR-RECTA     
                AND CUSUAR_MANUT       = :SCSBB036.CUSUAR-MANUT         
                FETCH FIRST 1 ROWS ONLY                                 
           END-EXEC.                                                    
                                                                        
           IF  (SQLCODE             NOT EQUAL ZEROS AND +100) OR        
               (SQLWARN0                EQUAL 'W')                      
                SET   DB2-SELECT           TO TRUE                      
                MOVE '0028'                TO SCSBY243-COD-ERRO         
                MOVE 'SCSBB036'            TO WRK-STORED-PROC           
                MOVE 'TLOG_COMPR_RECTA'    TO WRK-NOME-TABELA           
                PERFORM 9996-ERRO-DB2                                   
           END-IF.                                                      
                                                                        
           IF  SQLCODE                  EQUAL +100                      
               MOVE 'S'                    TO WRK-DESPREZA              
           ELSE                                                         
               MOVE 'N'                    TO WRK-DESPREZA              
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       8050-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
      * OBTER NOME E CNPJ DA EMPRESA                                   *
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
      *----------------------------------------------------------------*
       8100-ACESSAR-UORG1334              SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           MOVE '8100-ACESSAR-UORG1334'   TO FRWKGHEA-IDEN-PARAGRAFO    
                                                                        
           MOVE  ZEROS                    TO  UORGW334-E-CNPJ-CORPO     
           MOVE  CPSSOA-JURID             OF SCSBB013                   
             TO  UORGW334-E-CEMPR                                       
                                                                        
           EXEC    CICS LINK                                            
                   PROGRAM   (WRK-UORG1334)                             
                   COMMAREA  (WRK-AREA-UORG1334)                        
                   LENGTH    (LENGTH OF WRK-AREA-UORG1334)              
                   NOHANDLE                                             
           END-EXEC.                                                    
                                                                        
           IF EIBRESP                NOT EQUAL DFHRESP(NORMAL)          
              MOVE '0029'                 TO SCSBY243-COD-ERRO          
              PERFORM 9997-ERRO-CICS                                    
           END-IF.                                                      
                                                                        
           IF UORGW000-COD-RETORNO        OF WRK-AREA-UORG1334          
                                          NOT EQUAL ZEROS AND 08        
              MOVE '0030'                 TO SCSBY243-COD-ERRO          
              MOVE UORGW000-BLOCO-RETORNO OF WRK-AREA-UORG1334          
                                          TO SCSBY243-BLOCO-RETORNO     
                                             WRK-BLOCO-RETORNO          
              MOVE WRK-UORG1334           TO WRK-MODULO                 
              PERFORM 9998-ERRO-MODULO                                  
           END-IF                                                       
                                                                        
           IF UORGW000-COD-RETORNO        OF WRK-AREA-UORG1334          
                                          EQUAL 08                      
              MOVE '0031'                 TO SCSBY243-COD-ERRO          
              MOVE 'SCSB0029'             TO SCSBY243-COD-MENSAGEM      
              PERFORM 3000-FINALIZAR                                    
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       8100-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
      * OBTER DESCRICAO DA SITUACAO DO REGISTRO                        *
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
      *----------------------------------------------------------------*
       8200-ACESSAR-SCSB2026              SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           MOVE '8200-ACESSAR-SCSB2026'   TO FRWKGHEA-IDEN-PARAGRAFO    
                                                                        
           EXEC    CICS LINK                                            
                   PROGRAM   (WRK-SCSB2026)                             
                   COMMAREA  (WRK-AREA-SCSB2026)                        
                   LENGTH    (LENGTH OF WRK-AREA-SCSB2026)              
                   NOHANDLE                                             
           END-EXEC.                                                    
                                                                        
           IF  EIBRESP                 NOT EQUAL DFHRESP(NORMAL)        
               MOVE '0032'             TO  SCSBY243-COD-ERRO            
               PERFORM 9997-ERRO-CICS                                   
           END-IF.                                                      
                                                                        
           IF  SCSBY026-COD-RETORNO    NOT EQUAL   ZEROS                
               PERFORM 9998-ERRO-MODULO                                 
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       8200-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
      * TRATAMENTO DE ERRO DB2.                                        *
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
      *----------------------------------------------------------------*
       9996-ERRO-DB2                      SECTION.                      
      *----------------------------------------------------------------*
                                                                        
      *--> CARREGAR DADOS NA AREA DE COMUNICACAO                        
                                                                        
           MOVE  16                        TO  SCSBY243-COD-RETORNO     
           MOVE  'SCSB0032'                TO  SCSBY243-COD-MENSAGEM    
                                                                        
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
                                                                        
      *----------------------------------------------------------------*
       9996-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
      * TRATAMENTO DE ERRO - CICS                                      *
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
      *----------------------------------------------------------------*
       9997-ERRO-CICS                     SECTION.                      
      *----------------------------------------------------------------*
                                                                        
      *--> CARREGAR DADOS NA AREA DE RETORNO P/ O FRAMEWORK - I#FRWKAQ  
                                                                        
           MOVE  16                       TO  SCSBY243-COD-RETORNO      
           MOVE  'SCSB0033'               TO  SCSBY243-COD-MENSAGEM     
                                                                        
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
                                                                        
      *----------------------------------------------------------------*
       9997-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
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
                                                                        
      *----------------------------------------------------------------*
       9998-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*
                                                                        
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
                                                                        
      *----------------------------------------------------------------*
       9999-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*

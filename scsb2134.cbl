      *================================================================*
       IDENTIFICATION DIVISION.                                         
      *================================================================*
      *                                                                *
       PROGRAM-ID.    SCSB2134.                                         
       AUTHOR.        ANDERSON.                                         
      *                                                                *
      *================================================================*
      *                          BRQ IT SERVICES                       *
      *================================================================*
      *                                                                *
      *  PROGRAMADOR.......: ANDERSON MARTINS                          *
      *  ANALISTA..........: FERNANDA CARUSO                           *
      *  DATA..............: 14/08/2013                                *
      *                                                                *
      *================================================================*
      *                                                                *
      *  OBJETIVO.......: GERAR RELATORIO DOS RAS NAO TRANSMITIDOS     *
      *                                                                *
      *================================================================*
      *                                                                *
      * PROGRAMA : F U N C I O N A L                                   *
      *                                                                *
      * ESPECIFICACOES TECNICAS: COBOLMVS / CICS                       *
      *----------------------------------------------------------------*
      * I#FRWKGE COMMAREA FRWK1999 (LOG DE ERRO)                       *
      * I#FRWKHE AREA COMUM DE ERROS                                   *
      * I#FRWKDB AREA DE ERROS DE DB2                                  *
      * SCSBY134 BLOCO DE ENTRADA E SAIDA - MODULO FUNCIONAL           *
      * SCSBY056 BLOCO DE ENTRADA E SAIDA - MODULO FUNCIONAL           *
      * SCSBY057 BLOCO DE ENTRADA E SAIDA - MODULO FUNCIONAL           *
      * UORGW000 BLOCO DE ENTRADA E SAIDA - MODULO FUNCIONAL           *
      * UORGW397 BLOCO DE ENTRADA E SAIDA - MODULO FUNCIONAL           *
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
      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.                                         
      *----------------------------------------------------------------*
      *                                                                *
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                  PIC  X(080) VALUE                    
           'INICIO DA WORKING STORAGE'.                                 
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
       77  WRK-PROGRAMA            PIC  X(008) VALUE 'SCSB2134'.        
       77  WRK-SCSB2056            PIC  X(008) VALUE 'SCSB2056'.        
       77  WRK-SCSB2057            PIC  X(008) VALUE 'SCSB2057'.        
       77  WRK-UORG1397            PIC  X(008) VALUE 'UORG1397'.        
       77  WRK-FRWK1999            PIC  X(008) VALUE 'FRWK1999'.        
       77  WRK-SQLCODE             PIC S9(009) VALUE ZEROS COMP-3.      
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
      *  AREAS AUXILIARES                                              *
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
       01  WRK-I                   PIC  9(003) COMP-3 VALUE ZEROS.      
       01  WRK-MODULO              PIC  X(008) VALUE SPACES.            
       01  WRK-QTDE-LIMITE         PIC  9(003) VALUE ZEROS.             
       01  WRK-STORED-PROC         PIC  X(008) VALUE SPACES.            
       01  WRK-NOME-TABELA         PIC  X(032) VALUE SPACES.            
       01  WRK-FIM-CSR             PIC  X(001) VALUE SPACES.            
                                                                        
       01  WRK-CPSSOA              PIC S9(010) COMP-3 VALUE ZEROS.      
       01  WRK-CTPO-13             PIC S9(001) COMP-3 VALUE ZEROS.      
       01  WRK-CTPO-09             PIC S9(001) COMP-3 VALUE ZEROS.      
       01  WRK-CTPO-23             PIC S9(001) COMP-3 VALUE ZEROS.      
                                                                        
       01  WRK-DANO-PSQ            PIC S9(004) COMP-3 VALUE ZEROS.      
       01  WRK-NREG-PSQ            PIC S9(009) COMP-3 VALUE ZEROS.      
       01  WRK-COPER-PSQ           PIC S9(010) COMP-3 VALUE ZEROS.      
       01  WRK-NPGTO-PSQ           PIC S9(010) COMP-3 VALUE ZEROS.      
                                                                        
       01  WRK-BLOCO-RETORNO.                                           
           05  WRK-COD-RETORNO     PIC  9(002) VALUE ZEROS.             
           05  WRK-COD-ERRO        PIC  X(004) VALUE SPACES.            
           05  WRK-COD-MENSAGEM    PIC  X(008) VALUE SPACES.            
                                                                        
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
           'AREA TRATAMENTO DE ERRO - DB2'.                             
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
       01  WRK-AREA-ERRO-DB2.                                           
           COPY I#FRWKDB.                                               
                                                                        
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
           'AREA TRATAMENTO CHAMADA - SCSB2056'.                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
       01  WRK-AREA-SCSB2056.                                           
           COPY SCSBY056.                                               
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                  PIC  X(080) VALUE                    
           'AREA TRATAMENTO CHAMADA - SCSB2057'.                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
       01  WRK-AREA-SCSB2057.                                           
           COPY SCSBY057.                                               
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                  PIC  X(080) VALUE                    
           'AREA TRATAMENTO CHAMADA - UORG1397'.                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
       01  WRK-AREA-UORG1397.                                           
           COPY 'UORGW000'.                                             
           COPY 'UORGW397'.                                             
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                    PIC  X(008) VALUE 'AREA DB2'.      
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
           EXEC  SQL    INCLUDE      SQLCA      END-EXEC.               
                                                                        
           EXEC  SQL    INCLUDE      SCSBB009   END-EXEC.               
                                                                        
           EXEC  SQL    INCLUDE      SCSBB012   END-EXEC.               
                                                                        
           EXEC  SQL    INCLUDE      SCSBB013   END-EXEC.               
                                                                        
           EXEC  SQL    INCLUDE      SCSBB023   END-EXEC.               
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
      *   DECLARACAO DO CURSOR - CSR01-SCSBB013-JN JOIN DAS TABELAS    *
      *                       SCSBB013-SCSBB009                        *
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
           EXEC SQL DECLARE CSR01-SCSBB013-JN CURSOR FOR SELECT         
                     A.DANO_COMPR_RECTA                                 
                    ,A.NREG_COMPR_RECTA                                 
                    ,B.COPER_COMPR_EXTER                                
               FROM DB2PRD.TREG_COMPR_RECTA A,                          
                    DB2PRD.TOPER_COMPR_EXTER B                          
              WHERE  (A.DANO_COMPR_RECTA  = B.DANO_COMPR_RECTA)         
                AND  (A.NREG_COMPR_RECTA  = B.NREG_COMPR_RECTA)         
                AND (A.CPSSOA_JURID      = :WRK-CPSSOA)               
                AND ((A.CSIT_REG          = 'P'  OR                     
                      A.CSIT_REG          = 'V'  OR                     
                      A.CSIT_REG          = 'G'  OR
                      A.CSIT_REG          = 'E') AND                    
                      (B.CSIT_REG         = 'P'  OR                     
                       B.CSIT_REG         = 'V'  OR                     
                       B.CSIT_REG         = 'G'  OR
                       B.CSIT_REG         = 'E'))                       
                AND ((A.CTPO_REG_EXTER    = :WRK-CTPO-13) OR            
                     (B.CTPO_REG_EXTER    = :WRK-CTPO-09))              
                AND ((A.DANO_COMPR_RECTA  > :WRK-DANO-PSQ)              
                 OR  (A.DANO_COMPR_RECTA  = :WRK-DANO-PSQ   AND         
                      A.NREG_COMPR_RECTA  > :WRK-NREG-PSQ)              
                 OR  (A.DANO_COMPR_RECTA  = :WRK-DANO-PSQ   AND         
                      A.NREG_COMPR_RECTA  = :WRK-NREG-PSQ  AND          
                      B.COPER_COMPR_EXTER > :WRK-COPER-PSQ))            
             ORDER BY A.DANO_COMPR_RECTA,                               
                      A.NREG_COMPR_RECTA,                               
                      B.COPER_COMPR_EXTER                               
           END-EXEC.                                                    
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
      *   DECLARACAO DO CURSOR - CSR02-SCSBB013-JN-JOIN DAS TABELAS    *
      *                  SCSBB012-SCSBB013-SCSBB023                    *
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
           EXEC SQL DECLARE CSR02-SCSBB013-JN CURSOR FOR SELECT         
                     A.DANO_COMPR_RECTA                                 
                    ,A.NREG_COMPR_RECTA                                 
                    ,B.COPER_COMPR_EXTER                                
                    ,C.NPGTO_COMPR_EXTER                                
                    ,C.DPGTO_COMPR_EXTER                                
               FROM DB2PRD.TREG_COMPR_RECTA A,                          
                    DB2PRD.TPGTO_OPER_EXTER B,                          
                    DB2PRD.TPGTO_COMPR_EXTER C                          
              WHERE  (C.DANO_COMPR_RECTA  = A.DANO_COMPR_RECTA)         
                AND  (C.NREG_COMPR_RECTA  = A.NREG_COMPR_RECTA)         
                AND  (C.DANO_COMPR_RECTA  = B.DANO_COMPR_PGTO)          
                AND  (C.NREG_COMPR_RECTA  = B.NREG_COMPR_PGTO)          
                AND  (C.NPGTO_COMPR_EXTER = B.NPGTO_COMPR_EXTER)        
                AND  (C.DANO_COMPR_RECTA  = B.DANO_COMPR_RECTA)         
                AND  (C.NREG_COMPR_RECTA  = B.NREG_COMPR_RECTA)         
                AND (A.CPSSOA_JURID      = :WRK-CPSSOA)               
                AND  (C.CSIT_REG          = 'P' OR                      
                      C.CSIT_REG          = 'V' OR                      
                      C.CSIT_REG          = 'G' OR
                      C.CSIT_REG          = 'E')                        
                AND  (C.CTPO_REG_EXTER    = :WRK-CTPO-23)               
                AND ((C.DANO_COMPR_RECTA  > :WRK-DANO-PSQ)              
                 OR  (C.DANO_COMPR_RECTA  = :WRK-DANO-PSQ     AND       
                      C.NREG_COMPR_RECTA  > :WRK-NREG-PSQ)              
                 OR  (C.DANO_COMPR_RECTA  = :WRK-DANO-PSQ     AND       
                      C.NREG_COMPR_RECTA  = :WRK-NREG-PSQ     AND       
                      B.COPER_COMPR_EXTER > :WRK-COPER-PSQ)             
                 OR  (C.DANO_COMPR_RECTA  = :WRK-DANO-PSQ     AND       
                      C.NREG_COMPR_RECTA  = :WRK-NREG-PSQ     AND       
                      B.COPER_COMPR_EXTER = :WRK-COPER-PSQ    AND       
                      C.NPGTO_COMPR_EXTER > :WRK-NPGTO-PSQ))            
             ORDER BY A.DANO_COMPR_RECTA,                               
                      A.NREG_COMPR_RECTA,                               
                      B.COPER_COMPR_EXTER,                              
                      C.NPGTO_COMPR_EXTER                               
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
           COPY SCSBY134.                                               
                                                                        
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
                        FRWKGHEA-REGISTRO                               
                        FRWKGCIC-REGISTRO                               
                        FRWKGMOD-REGISTRO                               
                        FRWKGDB2-REGISTRO.                              
                                                                        
           PERFORM 1200-CONSISTIR-DADOS.                                
                                                                        
           PERFORM 1300-TRATAR-PAGINACAO.                               
                                                                        
           MOVE ZEROS                     TO SCSBY134-COD-RETORNO       
           MOVE '0000'                    TO SCSBY134-COD-ERRO          
           MOVE 'SCSB0026'                TO SCSBY134-COD-MENSAGEM.     
                                                                        
      *----------------------------------------------------------------*
       1000-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
      *   CONSISTIR DADOS DE ENTRADA - FUNCIONAL                       *
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
      *----------------------------------------------------------------*
       1200-CONSISTIR-DADOS               SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           IF SCSBY134-IND-PAGINACAO  NOT EQUAL 'I' AND 'S'             
              MOVE 08                     TO SCSBY134-COD-RETORNO       
              MOVE '0001'                 TO SCSBY134-COD-ERRO          
              MOVE 'SCSB0039'             TO SCSBY134-COD-MENSAGEM      
              PERFORM 3000-FINALIZAR                                    
           END-IF.                                                      
                                                                        
           IF SCSBY134-E-EMPRESA         EQUAL SPACES                   
              MOVE 08                     TO SCSBY134-COD-RETORNO       
              MOVE '0002'                 TO SCSBY134-COD-ERRO          
              MOVE 'SCSB0031'             TO SCSBY134-COD-MENSAGEM      
              PERFORM 3000-FINALIZAR                                    
           END-IF.                                                      
                                                                        
           IF SCSBY134-E-CTPO-REG        NOT NUMERIC OR                 
              SCSBY134-E-CTPO-REG        EQUAL ZEROS                    
              MOVE 08                     TO SCSBY134-COD-RETORNO       
              MOVE '0003'                 TO SCSBY134-COD-ERRO          
              MOVE 'SCSB0040'             TO SCSBY134-COD-MENSAGEM      
              PERFORM 3000-FINALIZAR                                    
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       1200-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
      *  FORMATA CHAVE DE PESQUISA PARA PAGINACAO                      *
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
      *----------------------------------------------------------------*
       1300-TRATAR-PAGINACAO              SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           MOVE 40                        TO WRK-QTDE-LIMITE            
                                                                        
           EVALUATE TRUE                                                
                                                                        
               WHEN SCSBY134-P-INICIAL                                  
                                                                        
                    MOVE ZEROS                 TO WRK-DANO-PSQ          
                    MOVE ZEROS                 TO WRK-NREG-PSQ          
                    MOVE ZEROS                 TO WRK-COPER-PSQ         
                                                                        
               WHEN SCSBY134-P-SEGUINTE                                 
                                                                        
                    MOVE SCSBY134-P-ULT-ANO    TO WRK-DANO-PSQ          
                    MOVE SCSBY134-P-ULT-NREG   TO WRK-NREG-PSQ          
                    MOVE SCSBY134-P-ULT-COPER  TO WRK-COPER-PSQ         
                                                                        
               WHEN OTHER                                               
                                                                        
                    MOVE 16                    TO SCSBY134-COD-RETORNO  
                    MOVE '0004'                TO SCSBY134-COD-ERRO     
                    MOVE 'SCSB0039'            TO SCSBY134-COD-MENSAGEM 
                    PERFORM 3000-FINALIZAR                              
                                                                        
           END-EVALUATE.                                                
                                                                        
      *----------------------------------------------------------------*
       1300-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
      *   PROCESSAMENTO PRINCIPAL DO PROGRAMA                          *
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
      *----------------------------------------------------------------*
       2000-PROCESSAR                     SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           PERFORM 2050-ACESSAR-UORG1397                                
                                                                        
           EVALUATE SCSBY134-E-CTPO-REG                                 
                                                                        
              WHEN 1                                                    
                                                                        
                 MOVE 1                   TO WRK-CTPO-13                
                 MOVE ZEROS               TO WRK-CTPO-09                
                                                                        
              WHEN 2                                                    
                                                                        
                 MOVE 3                   TO WRK-CTPO-13                
                 MOVE ZEROS               TO WRK-CTPO-09                
                                                                        
              WHEN 3                                                    
                                                                        
                 MOVE 2                   TO WRK-CTPO-09                
                 MOVE ZEROS               TO WRK-CTPO-13                
                                                                        
              WHEN 4                                                    
                                                                        
                 MOVE 4                   TO WRK-CTPO-09                
                 MOVE ZEROS               TO WRK-CTPO-13                
                                                                        
              WHEN 5                                                    
                                                                        
                 MOVE 1                   TO WRK-CTPO-23                
                                                                        
              WHEN 6                                                    
                                                                        
                 MOVE 5                   TO WRK-CTPO-23                
                                                                        
           END-EVALUATE.                                                
                                                                        
           IF SCSBY134-E-CTPO-REG         EQUAL 1 OR 2 OR 3 OR 4        
                                                                        
              PERFORM 2100-TRATAR-RAS-ADITIVO                           
                                                                        
           ELSE                                                         
                                                                        
              PERFORM 2200-TRATAR-PAGAMENTO                             
                                                                        
           END-IF.                                                      
                                                                        
           IF  WRK-I                  EQUAL ZEROS                       
               MOVE 08                TO SCSBY134-COD-RETORNO           
               IF  SCSBY134-IND-PAGINACAO EQUAL 'I'                     
                   MOVE '0005'           TO SCSBY134-COD-ERRO           
                   MOVE 'SCSB0029'       TO SCSBY134-COD-MENSAGEM       
               ELSE                                                     
                   MOVE '0006'           TO SCSBY134-COD-ERRO           
                   MOVE 'SCSB0030'       TO SCSBY134-COD-MENSAGEM       
               END-IF                                                   
               GO                        TO 2000-99-FIM                 
           END-IF                                                       
                                                                        
           IF  WRK-FIM-CSR            EQUAL 'S'                         
               MOVE 00                   TO SCSBY134-COD-RETORNO        
           ELSE                                                         
               MOVE 01                   TO SCSBY134-COD-RETORNO        
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       2000-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
      *  ACESSAR UORG1397 PARA OBTER CODIGO DA EMPRESA                 *
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
      *----------------------------------------------------------------*
       2050-ACESSAR-UORG1397              SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           MOVE '2050-ACESSAR-UORG1397'   TO FRWKGHEA-IDEN-PARAGRAFO    
                                                                        
           MOVE  'D'                      TO  UORGW397-E-AMBIENTE.      
           MOVE  SCSBY134-E-EMPRESA(1:4)  TO  UORGW397-E-CEMPR-SAP.     
           MOVE  ZEROS                    TO  UORGW397-E-CPSSOA-JURID.  
                                                                        
           EXEC    CICS LINK                                            
                   PROGRAM   (WRK-UORG1397)                             
                   COMMAREA  (WRK-AREA-UORG1397)                        
                   LENGTH    (LENGTH OF WRK-AREA-UORG1397)              
                   NOHANDLE                                             
           END-EXEC.                                                    
                                                                        
           IF EIBRESP                NOT EQUAL DFHRESP(NORMAL)          
              MOVE '0007'                 TO SCSBY134-COD-ERRO          
              PERFORM 9997-ERRO-CICS                                    
           END-IF.                                                      
                                                                        
           IF UORGW000-COD-RETORNO        OF WRK-AREA-UORG1397          
                                          NOT EQUAL ZEROS AND 08        
              MOVE '0008'                 TO SCSBY134-COD-ERRO          
              MOVE UORGW000-BLOCO-RETORNO OF WRK-AREA-UORG1397          
                                          TO SCSBY134-BLOCO-RETORNO     
                                             WRK-BLOCO-RETORNO          
              MOVE WRK-UORG1397           TO WRK-MODULO                 
              PERFORM 9998-ERRO-MODULO                                  
           END-IF                                                       
                                                                        
           IF UORGW000-COD-RETORNO        OF WRK-AREA-UORG1397          
                                          EQUAL 08                      
              MOVE 999999999               TO WRK-CPSSOA                                
           ELSE                                                         
              MOVE UORGW397-S-CPSSOA-JURID TO WRK-CPSSOA                                
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       2050-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
      *  TRATA TIPO DE REGISTRO 01, 02, 03 OU 04                       *
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
      *----------------------------------------------------------------*
       2100-TRATAR-RAS-ADITIVO           SECTION.                       
      *----------------------------------------------------------------*
                                                                        
           PERFORM 7005-OPEN-CSR01-SCSBB013-JN                          
                                                                        
           PERFORM 7010-FETCH-CSR01-SCSBB013-JN                         
                                                                        
           PERFORM UNTIL SQLCODE EQUAL +100 OR                          
                   WRK-I   GREATER  WRK-QTDE-LIMITE - 1                 
                   PERFORM 2910-MONTAR-SAIDA-I                          
                   PERFORM 7010-FETCH-CSR01-SCSBB013-JN                 
           END-PERFORM                                                  
                                                                        
           PERFORM 7015-CLOSE-CSR01-SCSBB013-JN.                        
                                                                        
      *----------------------------------------------------------------*
       2100-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
      *  TRATA TIPO DE REGISTRO 05 OU 06                               *
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
      *----------------------------------------------------------------*
       2200-TRATAR-PAGAMENTO             SECTION.                       
      *----------------------------------------------------------------*
                                                                        
           PERFORM 8005-OPEN-CSR02-SCSBB013-JN                          
                                                                        
           PERFORM 8010-FETCH-CSR02-SCSBB013-JN                         
                                                                        
           PERFORM UNTIL SQLCODE EQUAL +100 OR                          
                   WRK-I   GREATER  WRK-QTDE-LIMITE - 1                 
                   PERFORM 2920-MONTAR-SAIDA-II                         
                   PERFORM 8010-FETCH-CSR02-SCSBB013-JN                 
           END-PERFORM                                                  
                                                                        
           PERFORM 8015-CLOSE-CSR02-SCSBB013-JN.                        
                                                                        
      *----------------------------------------------------------------*
       2200-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
      *  OBTER DADOS DO VENDEDOR E DO ADQUIRENTE DA RAS                *
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
      *----------------------------------------------------------------*
       2300-ACESSAR-SCSB2056              SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           MOVE '2300-ACESSAR-SCSB2056'   TO FRWKGHEA-IDEN-PARAGRAFO    
                                                                        
           EXEC    CICS LINK                                            
                   PROGRAM   (WRK-SCSB2056)                             
                   COMMAREA  (WRK-AREA-SCSB2056)                        
                   LENGTH    (LENGTH OF WRK-AREA-SCSB2056)              
                   NOHANDLE                                             
           END-EXEC.                                                    
                                                                        
           IF EIBRESP                NOT EQUAL DFHRESP(NORMAL)          
              MOVE '0009'                 TO SCSBY134-COD-ERRO          
              PERFORM 9997-ERRO-CICS                                    
           END-IF.                                                      
                                                                        
           MOVE SCSBY056-BLOCO-RETORNO    TO  SCSBY134-BLOCO-RETORNO    
                                              WRK-BLOCO-RETORNO         
                                                                        
           IF SCSBY056-COD-RETORNO   NOT EQUAL ZEROS AND 08 AND 01      
              MOVE '0010'                 TO SCSBY134-COD-ERRO          
              MOVE WRK-SCSB2056           TO WRK-MODULO                 
              MOVE SCSBY056-BLOCO-RETORNO TO WRK-BLOCO-RETORNO          
              PERFORM 9998-ERRO-MODULO                                  
           END-IF.                                                      
                                                                        
           IF SCSBY056-COD-RETORNO        EQUAL 08                      
              MOVE '0011'                 TO SCSBY134-COD-ERRO          
              MOVE 'SCSB0029'             TO SCSBY134-COD-MENSAGEM      
              PERFORM 3000-FINALIZAR                                    
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       2300-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
      *  OBTER DADOS DAS OPERACOES DA RAS                              *
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
      *----------------------------------------------------------------*
       2400-ACESSAR-SCSB2057              SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           MOVE '2400-ACESSAR-SCSB2057'   TO FRWKGHEA-IDEN-PARAGRAFO    
                                                                        
           EXEC    CICS LINK                                            
                   PROGRAM   (WRK-SCSB2057)                             
                   COMMAREA  (WRK-AREA-SCSB2057)                        
                   LENGTH    (LENGTH OF WRK-AREA-SCSB2057)              
                   NOHANDLE                                             
           END-EXEC.                                                    
                                                                        
           IF EIBRESP                NOT EQUAL DFHRESP(NORMAL)          
              MOVE '0012'                 TO SCSBY134-COD-ERRO          
              PERFORM 9997-ERRO-CICS                                    
           END-IF.                                                      
                                                                        
           MOVE SCSBY057-BLOCO-RETORNO    TO  SCSBY134-BLOCO-RETORNO    
                                              WRK-BLOCO-RETORNO         
                                                                        
           IF SCSBY057-COD-RETORNO   NOT EQUAL ZEROS AND 08 AND 01      
              MOVE '0013'                 TO SCSBY134-COD-ERRO          
              MOVE WRK-SCSB2057           TO WRK-MODULO                 
              MOVE SCSBY057-BLOCO-RETORNO TO WRK-BLOCO-RETORNO          
              PERFORM 9998-ERRO-MODULO                                  
           END-IF.                                                      
                                                                        
           IF SCSBY057-COD-RETORNO        EQUAL 08                      
              MOVE '0014'                 TO SCSBY134-COD-ERRO          
              MOVE 'SCSB0029'             TO SCSBY134-COD-MENSAGEM      
              PERFORM 3000-FINALIZAR                                    
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       2400-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
      *   FORMATA BL DE SAIDA(FUNCIONAL) - LISTA                       *
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
      *----------------------------------------------------------------*
       2910-MONTAR-SAIDA-I                SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           ADD 1                          TO WRK-I.                     
                                                                        
           MOVE DANO-COMPR-RECTA          OF SCSBB013                   
             TO SCSBY134-S-ANO            (WRK-I)                       
                SCSBY056-E-ANO                                          
                SCSBY057-E-ANO                                          
                SCSBY134-P-ULT-ANO.                                     
                                                                        
           MOVE NREG-COMPR-RECTA          OF SCSBB013                   
             TO SCSBY134-S-NREG           (WRK-I)                       
                SCSBY056-E-NREG                                         
                SCSBY057-E-NREG                                         
                SCSBY134-P-ULT-NREG.                                    
                                                                        
           PERFORM 2300-ACESSAR-SCSB2056                                
                                                                        
           MOVE SCSBY056-S-NOME-VEND                                    
             TO SCSBY134-S-NOME-VEND      (WRK-I).                      
                                                                        
           MOVE SCSBY056-S-CPF-CNPJ-PRINC                               
             TO SCSBY134-S-CPF-CNPJ-PRINC (WRK-I).                      
                                                                        
           MOVE SCSBY056-S-CPF-CNPJ-FLIAL                               
             TO SCSBY134-S-CPF-CNPJ-FLIAL (WRK-I).                      
                                                                        
           MOVE SCSBY056-S-CPF-CNPJ-CTRL                                
             TO SCSBY134-S-CPF-CNPJ-CTRL  (WRK-I).                      
                                                                        
           MOVE SCSBY056-S-NIF                                          
             TO SCSBY134-S-NIF            (WRK-I).                      
                                                                        
           MOVE SCSBY056-S-CD-MOEDA                                     
             TO SCSBY134-S-CD-MOEDA       (WRK-I).                      
                                                                        
           MOVE SCSBY056-S-DS-MOEDA                                     
             TO SCSBY134-S-DS-MOEDA       (WRK-I).                      
                                                                        
           MOVE COPER-COMPR-EXTER         OF SCSBB009                   
             TO SCSBY134-S-COPER          (WRK-I)                       
                SCSBY057-E-COPER.                                       
                                                                        
           PERFORM 2400-ACESSAR-SCSB2057                                
                                                                        
           MOVE SCSBY057-S-CD-NBS         (1)                           
             TO SCSBY134-S-CD-NBS         (WRK-I).                      
                                                                        
           MOVE SCSBY057-S-DS-NBS         (1)                           
             TO SCSBY134-S-DS-NBS         (WRK-I).                      
                                                                        
           MOVE SCSBY057-S-DT-INICIO      (1)                           
             TO SCSBY134-S-DT-INICIO      (WRK-I).                      
                                                                        
           MOVE SCSBY057-S-DT-CONCLUSAO   (1)                           
             TO SCSBY134-S-DT-CONCLUSAO   (WRK-I).                      
                                                                        
           MOVE SCSBY057-S-VALOR          (1)                           
             TO SCSBY134-S-VALOR          (WRK-I).                      
                                                                        
           MOVE ZEROS                                                   
             TO SCSBY134-S-NPGTO          (WRK-I).                      
                                                                        
           MOVE ZEROS                                                   
             TO SCSBY134-S-VLR-PAGO       (WRK-I).                      
                                                                        
           MOVE SPACES                                                  
             TO SCSBY134-S-DT-PGTO        (WRK-I).                      
                                                                        
           MOVE WRK-I                 TO SCSBY134-S-QTD-REG.            
                                                                        
      *----------------------------------------------------------------*
       2910-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
      * FORMATA BL DE SAIDA(FUNCIONAL) - LISTA COM INF. DE FATURAMENTO *
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
      *----------------------------------------------------------------*
       2920-MONTAR-SAIDA-II               SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           ADD 1                          TO WRK-I.                     
                                                                        
           MOVE DANO-COMPR-RECTA          OF SCSBB013                   
             TO SCSBY134-S-ANO            (WRK-I)                       
                SCSBY056-E-ANO                                          
                SCSBY057-E-ANO                                          
                SCSBY134-P-ULT-ANO.                                     
                                                                        
           MOVE NREG-COMPR-RECTA          OF SCSBB013                   
             TO SCSBY134-S-NREG           (WRK-I)                       
                SCSBY056-E-NREG                                         
                SCSBY057-E-NREG                                         
                SCSBY134-P-ULT-NREG.                                    
                                                                        
           PERFORM 2300-ACESSAR-SCSB2056                                
                                                                        
           MOVE SCSBY056-S-NOME-VEND                                    
             TO SCSBY134-S-NOME-VEND      (WRK-I).                      
                                                                        
           MOVE SCSBY056-S-CPF-CNPJ-PRINC                               
             TO SCSBY134-S-CPF-CNPJ-PRINC (WRK-I).                      
                                                                        
           MOVE SCSBY056-S-CPF-CNPJ-FLIAL                               
             TO SCSBY134-S-CPF-CNPJ-FLIAL (WRK-I).                      
                                                                        
           MOVE SCSBY056-S-CPF-CNPJ-CTRL                                
             TO SCSBY134-S-CPF-CNPJ-CTRL  (WRK-I).                      
                                                                        
           MOVE SCSBY056-S-NIF                                          
             TO SCSBY134-S-NIF            (WRK-I).                      
                                                                        
           MOVE SCSBY056-S-CD-MOEDA                                     
             TO SCSBY134-S-CD-MOEDA       (WRK-I).                      
                                                                        
           MOVE SCSBY056-S-DS-MOEDA                                     
             TO SCSBY134-S-DS-MOEDA       (WRK-I).                      
                                                                        
           MOVE COPER-COMPR-EXTER         OF SCSBB012                   
             TO SCSBY134-S-COPER          (WRK-I)                       
                SCSBY057-E-COPER.                                       
                                                                        
           PERFORM 2400-ACESSAR-SCSB2057                                
                                                                        
           MOVE SCSBY057-S-CD-NBS         (1)                           
             TO SCSBY134-S-CD-NBS         (WRK-I).                      
                                                                        
           MOVE SCSBY057-S-DS-NBS         (1)                           
             TO SCSBY134-S-DS-NBS         (WRK-I).                      
                                                                        
           MOVE SCSBY057-S-DT-INICIO      (1)                           
             TO SCSBY134-S-DT-INICIO      (WRK-I).                      
                                                                        
           MOVE SCSBY057-S-DT-CONCLUSAO   (1)                           
             TO SCSBY134-S-DT-CONCLUSAO   (WRK-I).                      
                                                                        
           MOVE SCSBY057-S-VALOR          (1)                           
             TO SCSBY134-S-VALOR          (WRK-I).                      
                                                                        
           MOVE NPGTO-COMPR-EXTER         OF SCSBB023                   
             TO SCSBY134-S-NPGTO          (WRK-I).                      
                                                                        
           MOVE SCSBY057-S-VALOR-PAGO     (1)                           
             TO SCSBY134-S-VLR-PAGO       (WRK-I).                      
                                                                        
           MOVE DPGTO-COMPR-EXTER         OF SCSBB023                   
             TO SCSBY134-S-DT-PGTO        (WRK-I).                      
                                                                        
           MOVE WRK-I                 TO SCSBY134-S-QTD-REG.            
                                                                        
      *----------------------------------------------------------------*
       2920-99-FIM.                       EXIT.                         
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
       7005-OPEN-CSR01-SCSBB013-JN        SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           MOVE '7005-OPEN-CSR01-SCSBB013-JN'                           
                                          TO FRWKGHEA-IDEN-PARAGRAFO    
                                                                        
           EXEC SQL                                                     
               OPEN  CSR01-SCSBB013-JN                                  
           END-EXEC.                                                    
                                                                        
           IF  (SQLCODE             NOT EQUAL ZEROS) OR                 
               (SQLWARN0                EQUAL 'W')                      
                SET   DB2-OPEN             TO TRUE                      
                MOVE '0015'                TO SCSBY134-COD-ERRO         
                MOVE 'SCSBB013'            TO WRK-STORED-PROC           
                MOVE 'TREG_COMPR_RECTA'    TO WRK-NOME-TABELA           
                PERFORM 9996-ERRO-DB2                                   
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       7005-99-FIM.                        EXIT.                        
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       7010-FETCH-CSR01-SCSBB013-JN        SECTION.                     
      *----------------------------------------------------------------*
                                                                        
           MOVE '7010-FETCH-CSR01-SCSBB013-JN'                          
                                             TO FRWKGHEA-IDEN-PARAGRAFO 
                                                                        
           EXEC SQL FETCH CSR01-SCSBB013-JN INTO                        
                    :SCSBB013.DANO-COMPR-RECTA                          
                   ,:SCSBB013.NREG-COMPR-RECTA                          
                   ,:SCSBB009.COPER-COMPR-EXTER                         
           END-EXEC.                                                    
                                                                        
           IF  (SQLCODE             NOT EQUAL ZEROS AND +100) OR        
               (SQLWARN0                EQUAL 'W')                      
                SET   DB2-FETCH            TO TRUE                      
                MOVE '0016'                TO SCSBY134-COD-ERRO         
                MOVE 'SCSBB013'            TO WRK-STORED-PROC           
                MOVE 'TREG_COMPR_RECTA'    TO WRK-NOME-TABELA           
                PERFORM 9996-ERRO-DB2                                   
           END-IF.                                                      
                                                                        
           IF   SQLCODE                 EQUAL +100                      
                MOVE 'S'                   TO WRK-FIM-CSR               
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       7010-99-FIM.                        EXIT.                        
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       7015-CLOSE-CSR01-SCSBB013-JN       SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           MOVE '7015-CLOSE-CSR01-SCSBB013-JN'                          
                                             TO FRWKGHEA-IDEN-PARAGRAFO 
                                                                        
           EXEC SQL                                                     
               CLOSE CSR01-SCSBB013-JN                                  
           END-EXEC.                                                    
                                                                        
           IF  (SQLCODE             NOT EQUAL ZEROS) OR                 
               (SQLWARN0                EQUAL 'W')                      
                SET   DB2-CLOSE            TO TRUE                      
                MOVE '0017'                TO SCSBY134-COD-ERRO         
                MOVE 'SCSBB013'            TO WRK-STORED-PROC           
                MOVE 'TREG_COMPR_RECTA'    TO WRK-NOME-TABELA           
                PERFORM 9996-ERRO-DB2                                   
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       7015-99-FIM.                        EXIT.                        
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       8005-OPEN-CSR02-SCSBB013-JN        SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           MOVE '8005-OPEN-CSR02-SCSBB013-JN'                           
                                          TO FRWKGHEA-IDEN-PARAGRAFO    
                                                                        
           EXEC SQL                                                     
               OPEN  CSR02-SCSBB013-JN                                  
           END-EXEC.                                                    
                                                                        
           IF  (SQLCODE             NOT EQUAL ZEROS) OR                 
               (SQLWARN0                EQUAL 'W')                      
                SET   DB2-OPEN             TO TRUE                      
                MOVE '0018'                TO SCSBY134-COD-ERRO         
                MOVE 'SCSBB013'            TO WRK-STORED-PROC           
                MOVE 'TREG_COMPR_RECTA'    TO WRK-NOME-TABELA           
                PERFORM 9996-ERRO-DB2                                   
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       8005-99-FIM.                        EXIT.                        
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       8010-FETCH-CSR02-SCSBB013-JN        SECTION.                     
      *----------------------------------------------------------------*
                                                                        
           MOVE '7010-FETCH-CSR02-SCSBB013-JN'                          
                                             TO FRWKGHEA-IDEN-PARAGRAFO 
                                                                        
           EXEC SQL FETCH CSR02-SCSBB013-JN INTO                        
                    :SCSBB013.DANO-COMPR-RECTA                          
                   ,:SCSBB013.NREG-COMPR-RECTA                          
                   ,:SCSBB012.COPER-COMPR-EXTER                         
                   ,:SCSBB023.NPGTO-COMPR-EXTER                         
                   ,:SCSBB023.DPGTO-COMPR-EXTER                         
           END-EXEC.                                                    
                                                                        
           IF  (SQLCODE             NOT EQUAL ZEROS AND +100) OR        
               (SQLWARN0                EQUAL 'W')                      
                SET   DB2-FETCH            TO TRUE                      
                MOVE '0019'                TO SCSBY134-COD-ERRO         
                MOVE 'SCSBB013'            TO WRK-STORED-PROC           
                MOVE 'TREG_COMPR_RECTA'    TO WRK-NOME-TABELA           
                PERFORM 9996-ERRO-DB2                                   
           END-IF.                                                      
                                                                        
           IF   SQLCODE                 EQUAL +100                      
                MOVE 'S'                   TO WRK-FIM-CSR               
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       8010-99-FIM.                        EXIT.                        
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       8015-CLOSE-CSR02-SCSBB013-JN       SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           MOVE '8015-CLOSE-CSR02-SCSBB013-JN'                          
                                             TO FRWKGHEA-IDEN-PARAGRAFO 
                                                                        
           EXEC SQL                                                     
               CLOSE CSR02-SCSBB013-JN                                  
           END-EXEC.                                                    
                                                                        
           IF  (SQLCODE             NOT EQUAL ZEROS) OR                 
               (SQLWARN0                EQUAL 'W')                      
                SET   DB2-CLOSE            TO TRUE                      
                MOVE '0020'                TO SCSBY134-COD-ERRO         
                MOVE 'SCSBB013'            TO WRK-STORED-PROC           
                MOVE 'TREG_COMPR_RECTA'    TO WRK-NOME-TABELA           
                PERFORM 9996-ERRO-DB2                                   
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       8015-99-FIM.                        EXIT.                        
      *----------------------------------------------------------------*
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
      * TRATAMENTO DE ERRO DB2.                                        *
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
      *----------------------------------------------------------------*
       9996-ERRO-DB2                      SECTION.                      
      *----------------------------------------------------------------*
                                                                        
      *--> CARREGAR DADOS NA AREA DE COMUNICACAO                        
                                                                        
           MOVE  16                        TO  SCSBY134-COD-RETORNO     
           MOVE  'SCSB0032'                TO  SCSBY134-COD-MENSAGEM    
                                                                        
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
                                                                        
           MOVE  16                       TO  SCSBY134-COD-RETORNO      
           MOVE  'SCSB0033'               TO  SCSBY134-COD-MENSAGEM     
                                                                        
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
                                                                        

      *================================================================*
       IDENTIFICATION DIVISION.                                         
      *================================================================*
      *                                                                *
       PROGRAM-ID.    SCSB2130.                                         
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
      *  OBJETIVO.......: GERAR RELATORIO DOS RVS NAO TRANSMITIDOS     *
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
      * SCSBY130 BLOCO DE ENTRADA E SAIDA - MODULO FUNCIONAL           *
      * SCSBY032 BLOCO DE ENTRADA E SAIDA - MODULO FUNCIONAL           *
      * SCSBY033 BLOCO DE ENTRADA E SAIDA - MODULO FUNCIONAL           *
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
                                                                        
       77  WRK-PROGRAMA            PIC  X(008) VALUE 'SCSB2130'.        
       77  WRK-SCSB2032            PIC  X(008) VALUE 'SCSB2032'.        
       77  WRK-SCSB2033            PIC  X(008) VALUE 'SCSB2033'.        
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
       01  WRK-CTPO-16             PIC S9(001) COMP-3 VALUE ZEROS.      
       01  WRK-CTPO-10             PIC S9(001) COMP-3 VALUE ZEROS.      
       01  WRK-CTPO-22             PIC S9(001) COMP-3 VALUE ZEROS.      
                                                                        
       01  WRK-DANO-PSQ            PIC S9(004) COMP-3 VALUE ZEROS.      
       01  WRK-NREG-PSQ            PIC S9(009) COMP-3 VALUE ZEROS.      
       01  WRK-COPER-PSQ           PIC S9(010) COMP-3 VALUE ZEROS.      
       01  WRK-NFATMT-PSQ          PIC S9(010) COMP-3 VALUE ZEROS.      
                                                                        
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
           'AREA TRATAMENTO CHAMADA - SCSB2032'.                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
       01  WRK-AREA-SCSB2032.                                           
           COPY SCSBY032.                                               
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                  PIC  X(080) VALUE                    
           'AREA TRATAMENTO CHAMADA - SCSB2033'.                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
       01  WRK-AREA-SCSB2033.                                           
           COPY SCSBY033.                                               
                                                                        
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
                                                                        
           EXEC  SQL    INCLUDE      SCSBB005   END-EXEC.               
                                                                        
           EXEC  SQL    INCLUDE      SCSBB010   END-EXEC.               
                                                                        
           EXEC  SQL    INCLUDE      SCSBB016   END-EXEC.               
                                                                        
           EXEC  SQL    INCLUDE      SCSBB022   END-EXEC.               
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
      *   DECLARACAO DO CURSOR - CSR01-SCSBB016-JN JOIN DAS TABELAS    *
      *                       SCSBB016-SCSBB010                        *
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
           EXEC SQL DECLARE CSR01-SCSBB016-JN CURSOR FOR SELECT         
                     A.DANO_VDA_RECTA                                   
                    ,A.NREG_VDA_RECTA                                   
                    ,B.COPER_VDA_EXTER                                  
               FROM DB2PRD.TREG_VDA_RECTA A,                            
                    DB2PRD.TOPER_VDA_EXTER B                            
              WHERE  (A.DANO_VDA_RECTA    = B.DANO_VDA_RECTA)           
                AND  (A.NREG_VDA_RECTA    = B.NREG_VDA_RECTA)           
                AND (A.CPSSOA_JURID      = :WRK-CPSSOA)               
                AND ((A.CSIT_REG          = 'P'  OR                     
                      A.CSIT_REG          = 'V'  OR                     
                      A.CSIT_REG          = 'G'  OR
                      A.CSIT_REG          = 'E') AND                    
                      (B.CSIT_REG         = 'P'  OR                     
                       B.CSIT_REG         = 'V'  OR                     
                       B.CSIT_REG         = 'G'  OR
                       B.CSIT_REG         = 'E'))                       
                AND ((A.CTPO_REG_EXTER    = :WRK-CTPO-16) OR            
                     (B.CTPO_REG_EXTER    = :WRK-CTPO-10))              
                AND ((A.DANO_VDA_RECTA    > :WRK-DANO-PSQ)              
                 OR  (A.DANO_VDA_RECTA    = :WRK-DANO-PSQ   AND         
                      A.NREG_VDA_RECTA    > :WRK-NREG-PSQ)              
                 OR  (A.DANO_VDA_RECTA    = :WRK-DANO-PSQ   AND         
                      A.NREG_VDA_RECTA    = :WRK-NREG-PSQ  AND          
                      B.COPER_VDA_EXTER   > :WRK-COPER-PSQ))            
             ORDER BY A.DANO_VDA_RECTA,                                 
                      A.NREG_VDA_RECTA,                                 
                      B.COPER_VDA_EXTER                                 
           END-EXEC.                                                    
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
      *   DECLARACAO DO CURSOR - CSR02-SCSBB016-JN-JOIN DAS TABELAS    *
      *                  SCSBB005-SCSBB016-SCSBB022                    *
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
           EXEC SQL DECLARE CSR02-SCSBB016-JN CURSOR FOR SELECT         
                     A.DANO_VDA_RECTA                                   
                    ,A.NREG_VDA_RECTA                                   
                    ,B.COPER_VDA_EXTER                                  
                    ,C.NFATMT_VDA_EXTER                                 
                    ,C.DFATMT_VDA_EXTER                                 
               FROM DB2PRD.TREG_VDA_RECTA A,                            
                    DB2PRD.TFATMT_OPER_VDA B,                           
                    DB2PRD.TFATMT_VDA_EXTER C                           
              WHERE  (C.DANO_VDA_RECTA    = A.DANO_VDA_RECTA)           
                AND  (C.NREG_VDA_RECTA    = A.NREG_VDA_RECTA)           
                AND  (C.DANO_VDA_RECTA    = B.DANO_VDA_FATMT)           
                AND  (C.NREG_VDA_RECTA    = B.NREG_VDA_FATMT)           
                AND  (C.NFATMT_VDA_EXTER  = B.NFATMT_VDA_EXTER)         
                AND  (C.DANO_VDA_RECTA    = B.DANO_VDA_RECTA)           
                AND  (C.NREG_VDA_RECTA    = B.NREG_VDA_RECTA)           
                AND (A.CPSSOA_JURID      = :WRK-CPSSOA)               
                AND  (C.CSIT_REG          = 'P' OR                      
                      C.CSIT_REG          = 'V' OR                      
                      C.CSIT_REG          = 'G' OR
                      C.CSIT_REG          = 'E')                        
                AND  (C.CTPO_REG_EXTER    = :WRK-CTPO-22)               
                AND ((C.DANO_VDA_RECTA    > :WRK-DANO-PSQ)              
                 OR  (C.DANO_VDA_RECTA    = :WRK-DANO-PSQ     AND       
                      C.NREG_VDA_RECTA    > :WRK-NREG-PSQ)              
                 OR  (C.DANO_VDA_RECTA    = :WRK-DANO-PSQ     AND       
                      C.NREG_VDA_RECTA    = :WRK-NREG-PSQ     AND       
                      B.COPER_VDA_EXTER   > :WRK-COPER-PSQ)             
                 OR  (C.DANO_VDA_RECTA    = :WRK-DANO-PSQ     AND       
                      C.NREG_VDA_RECTA    = :WRK-NREG-PSQ     AND       
                      B.COPER_VDA_EXTER   = :WRK-COPER-PSQ    AND       
                      C.NFATMT_VDA_EXTER  > :WRK-NFATMT-PSQ))           
             ORDER BY A.DANO_VDA_RECTA,                                 
                      A.NREG_VDA_RECTA,                                 
                      B.COPER_VDA_EXTER,                                
                      C.NFATMT_VDA_EXTER                                
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
           COPY SCSBY130.                                               
                                                                        
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
                                                                        
           MOVE ZEROS                     TO SCSBY130-COD-RETORNO       
           MOVE '0000'                    TO SCSBY130-COD-ERRO          
           MOVE 'SCSB0026'                TO SCSBY130-COD-MENSAGEM.     
                                                                        
      *----------------------------------------------------------------*
       1000-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
      *   CONSISTIR DADOS DE ENTRADA - FUNCIONAL                       *
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
      *----------------------------------------------------------------*
       1200-CONSISTIR-DADOS               SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           IF SCSBY130-IND-PAGINACAO  NOT EQUAL 'I' AND 'S'             
              MOVE 08                     TO SCSBY130-COD-RETORNO       
              MOVE '0001'                 TO SCSBY130-COD-ERRO          
              MOVE 'SCSB0039'             TO SCSBY130-COD-MENSAGEM      
              PERFORM 3000-FINALIZAR                                    
           END-IF.                                                      
                                                                        
           IF SCSBY130-E-EMPRESA         EQUAL SPACES                   
              MOVE 08                     TO SCSBY130-COD-RETORNO       
              MOVE '0002'                 TO SCSBY130-COD-ERRO          
              MOVE 'SCSB0031'             TO SCSBY130-COD-MENSAGEM      
              PERFORM 3000-FINALIZAR                                    
           END-IF.                                                      
                                                                        
           IF SCSBY130-E-CTPO-REG        NOT NUMERIC OR                 
              SCSBY130-E-CTPO-REG        EQUAL ZEROS                    
              MOVE 08                     TO SCSBY130-COD-RETORNO       
              MOVE '0003'                 TO SCSBY130-COD-ERRO          
              MOVE 'SCSB0040'             TO SCSBY130-COD-MENSAGEM      
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
                                                                        
               WHEN SCSBY130-P-INICIAL                                  
                                                                        
                    MOVE ZEROS                 TO WRK-DANO-PSQ          
                    MOVE ZEROS                 TO WRK-NREG-PSQ          
                    MOVE ZEROS                 TO WRK-COPER-PSQ         
                    MOVE ZEROS                 TO WRK-NFATMT-PSQ        
                                                                        
               WHEN SCSBY130-P-SEGUINTE                                 
                                                                        
                    MOVE SCSBY130-P-ULT-ANO    TO WRK-DANO-PSQ          
                    MOVE SCSBY130-P-ULT-NREG   TO WRK-NREG-PSQ          
                    MOVE SCSBY130-P-ULT-COPER  TO WRK-COPER-PSQ         
                    MOVE SCSBY130-P-ULT-NFATMT TO WRK-NFATMT-PSQ        
                                                                        
               WHEN OTHER                                               
                                                                        
                    MOVE 16                    TO SCSBY130-COD-RETORNO  
                    MOVE '0004'                TO SCSBY130-COD-ERRO     
                    MOVE 'SCSB0039'            TO SCSBY130-COD-MENSAGEM 
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
                                                                        
           EVALUATE SCSBY130-E-CTPO-REG                                 
                                                                        
              WHEN 1                                                    
                                                                        
                 MOVE 1                   TO WRK-CTPO-16                
                 MOVE ZEROS               TO WRK-CTPO-10                
                                                                        
              WHEN 2                                                    
                                                                        
                 MOVE 3                   TO WRK-CTPO-16                
                 MOVE ZEROS               TO WRK-CTPO-10                
                                                                        
              WHEN 3                                                    
                                                                        
                 MOVE 2                   TO WRK-CTPO-10                
                 MOVE ZEROS               TO WRK-CTPO-16                
                                                                        
              WHEN 4                                                    
                                                                        
                 MOVE 4                   TO WRK-CTPO-10                
                 MOVE ZEROS               TO WRK-CTPO-16                
                                                                        
              WHEN 5                                                    
                                                                        
                 MOVE 1                   TO WRK-CTPO-22                
                                                                        
              WHEN 6                                                    
                                                                        
                 MOVE 5                   TO WRK-CTPO-22                
                                                                        
           END-EVALUATE.                                                
                                                                        
           IF SCSBY130-E-CTPO-REG         EQUAL 1 OR 2 OR 3 OR 4        
                                                                        
              PERFORM 2100-TRATAR-RVS-ADITIVO                           
                                                                        
           ELSE                                                         
                                                                        
              PERFORM 2200-TRATAR-FATURAMENTO                           
                                                                        
           END-IF.                                                      
                                                                        
           IF  WRK-I                  EQUAL ZEROS                       
               MOVE 08                TO SCSBY130-COD-RETORNO           
               IF  SCSBY130-IND-PAGINACAO EQUAL 'I'                     
                   MOVE '0005'           TO SCSBY130-COD-ERRO           
                   MOVE 'SCSB0029'       TO SCSBY130-COD-MENSAGEM       
               ELSE                                                     
                   MOVE '0006'           TO SCSBY130-COD-ERRO           
                   MOVE 'SCSB0030'       TO SCSBY130-COD-MENSAGEM       
               END-IF                                                   
               GO                        TO 2000-99-FIM                 
           END-IF                                                       
                                                                        
           IF  WRK-FIM-CSR            EQUAL 'S'                         
               MOVE 00                   TO SCSBY130-COD-RETORNO        
           ELSE                                                         
               MOVE 01                   TO SCSBY130-COD-RETORNO        
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
           MOVE  SCSBY130-E-EMPRESA(1:4)  TO  UORGW397-E-CEMPR-SAP.     
           MOVE  ZEROS                    TO  UORGW397-E-CPSSOA-JURID.  
                                                                        
           EXEC    CICS LINK                                            
                   PROGRAM   (WRK-UORG1397)                             
                   COMMAREA  (WRK-AREA-UORG1397)                        
                   LENGTH    (LENGTH OF WRK-AREA-UORG1397)              
                   NOHANDLE                                             
           END-EXEC.                                                    
                                                                        
           IF EIBRESP                NOT EQUAL DFHRESP(NORMAL)          
              MOVE '0007'                 TO SCSBY130-COD-ERRO          
              PERFORM 9997-ERRO-CICS                                    
           END-IF.                                                      
                                                                        
           IF UORGW000-COD-RETORNO        OF WRK-AREA-UORG1397          
                                          NOT EQUAL ZEROS AND 08        
              MOVE '0008'                 TO SCSBY130-COD-ERRO          
              MOVE UORGW000-BLOCO-RETORNO OF WRK-AREA-UORG1397          
                                          TO SCSBY130-BLOCO-RETORNO     
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
       2100-TRATAR-RVS-ADITIVO           SECTION.                       
      *----------------------------------------------------------------*
                                                                        
           PERFORM 7005-OPEN-CSR01-SCSBB016-JN                          
                                                                        
           PERFORM 7010-FETCH-CSR01-SCSBB016-JN                         
                                                                        
           PERFORM UNTIL SQLCODE EQUAL +100 OR                          
                   WRK-I   GREATER  WRK-QTDE-LIMITE - 1                 
                   PERFORM 2910-MONTAR-SAIDA-I                          
                   PERFORM 7010-FETCH-CSR01-SCSBB016-JN                 
           END-PERFORM                                                  
                                                                        
           PERFORM 7015-CLOSE-CSR01-SCSBB016-JN.                        
                                                                        
      *----------------------------------------------------------------*
       2100-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
      *  TRATA TIPO DE REGISTRO 05 OU 06                               *
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
      *----------------------------------------------------------------*
       2200-TRATAR-FATURAMENTO           SECTION.                       
      *----------------------------------------------------------------*
                                                                        
           PERFORM 8005-OPEN-CSR02-SCSBB016-JN                          
                                                                        
           PERFORM 8010-FETCH-CSR02-SCSBB016-JN                         
                                                                        
           PERFORM UNTIL SQLCODE EQUAL +100 OR                          
                   WRK-I   GREATER  WRK-QTDE-LIMITE - 1                 
                   PERFORM 2920-MONTAR-SAIDA-II                         
                   PERFORM 8010-FETCH-CSR02-SCSBB016-JN                 
           END-PERFORM                                                  
                                                                        
           PERFORM 8015-CLOSE-CSR02-SCSBB016-JN.                        
                                                                        
      *----------------------------------------------------------------*
       2200-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
      *  OBTER DADOS DO VENDEDOR E DO ADQUIRENTE DA RVS                *
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
      *----------------------------------------------------------------*
       2300-ACESSAR-SCSB2032              SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           MOVE '2100-ACESSAR-SCSB2032'   TO FRWKGHEA-IDEN-PARAGRAFO    
                                                                        
           EXEC    CICS LINK                                            
                   PROGRAM   (WRK-SCSB2032)                             
                   COMMAREA  (WRK-AREA-SCSB2032)                        
                   LENGTH    (LENGTH OF WRK-AREA-SCSB2032)              
                   NOHANDLE                                             
           END-EXEC.                                                    
                                                                        
           IF EIBRESP                NOT EQUAL DFHRESP(NORMAL)          
              MOVE '0009'                 TO SCSBY130-COD-ERRO          
              PERFORM 9997-ERRO-CICS                                    
           END-IF.                                                      
                                                                        
           MOVE SCSBY032-BLOCO-RETORNO    TO  SCSBY130-BLOCO-RETORNO    
                                              WRK-BLOCO-RETORNO         
                                                                        
           IF SCSBY032-COD-RETORNO   NOT EQUAL ZEROS AND 08 AND 01      
              MOVE '0010'                 TO SCSBY130-COD-ERRO          
              MOVE WRK-SCSB2032           TO WRK-MODULO                 
              MOVE SCSBY032-BLOCO-RETORNO TO WRK-BLOCO-RETORNO          
              PERFORM 9998-ERRO-MODULO                                  
           END-IF.                                                      
                                                                        
           IF SCSBY032-COD-RETORNO        EQUAL 08                      
              MOVE '0011'                 TO SCSBY130-COD-ERRO          
              MOVE 'SCSB0029'             TO SCSBY130-COD-MENSAGEM      
              PERFORM 3000-FINALIZAR                                    
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       2300-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
      *  OBTER DADOS DAS OPERACOES DA RVS                              *
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
      *----------------------------------------------------------------*
       2400-ACESSAR-SCSB2033              SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           MOVE '2400-ACESSAR-SCSB2033'   TO FRWKGHEA-IDEN-PARAGRAFO    
                                                                        
           EXEC    CICS LINK                                            
                   PROGRAM   (WRK-SCSB2033)                             
                   COMMAREA  (WRK-AREA-SCSB2033)                        
                   LENGTH    (LENGTH OF WRK-AREA-SCSB2033)              
                   NOHANDLE                                             
           END-EXEC.                                                    
                                                                        
           IF EIBRESP                NOT EQUAL DFHRESP(NORMAL)          
              MOVE '0012'                 TO SCSBY130-COD-ERRO          
              PERFORM 9997-ERRO-CICS                                    
           END-IF.                                                      
                                                                        
           MOVE SCSBY033-BLOCO-RETORNO    TO  SCSBY130-BLOCO-RETORNO    
                                              WRK-BLOCO-RETORNO         
                                                                        
           IF SCSBY033-COD-RETORNO   NOT EQUAL ZEROS AND 08 AND 01      
              MOVE '0013'                 TO SCSBY130-COD-ERRO          
              MOVE WRK-SCSB2033           TO WRK-MODULO                 
              MOVE SCSBY033-BLOCO-RETORNO TO WRK-BLOCO-RETORNO          
              PERFORM 9998-ERRO-MODULO                                  
           END-IF.                                                      
                                                                        
           IF SCSBY033-COD-RETORNO        EQUAL 08                      
              MOVE '0014'                 TO SCSBY130-COD-ERRO          
              MOVE 'SCSB0029'             TO SCSBY130-COD-MENSAGEM      
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
                                                                        
           MOVE DANO-VDA-RECTA            OF SCSBB016                   
             TO SCSBY130-S-ANO            (WRK-I)                       
                SCSBY032-E-ANO                                          
                SCSBY033-E-ANO                                          
                SCSBY130-P-ULT-ANO.                                     
                                                                        
           MOVE NREG-VDA-RECTA            OF SCSBB016                   
             TO SCSBY130-S-NREG           (WRK-I)                       
                SCSBY032-E-NREG                                         
                SCSBY033-E-NREG                                         
                SCSBY130-P-ULT-NREG.                                    
                                                                        
           PERFORM 2300-ACESSAR-SCSB2032                                
                                                                        
           MOVE SCSBY032-S-NOME-ADQUIR                                  
             TO SCSBY130-S-NOME-ADQUIR    (WRK-I).                      
                                                                        
           MOVE SCSBY032-S-CPF-CNPJ-PRINC                               
             TO SCSBY130-S-CPF-CNPJ-PRINC (WRK-I).                      
                                                                        
           MOVE SCSBY032-S-CPF-CNPJ-FLIAL                               
             TO SCSBY130-S-CPF-CNPJ-FLIAL (WRK-I).                      
                                                                        
           MOVE SCSBY032-S-CPF-CNPJ-CTRL                                
             TO SCSBY130-S-CPF-CNPJ-CTRL  (WRK-I).                      
                                                                        
           MOVE SCSBY032-S-NIF                                          
             TO SCSBY130-S-NIF            (WRK-I).                      
                                                                        
           MOVE SCSBY032-S-CD-MOEDA                                     
             TO SCSBY130-S-CD-MOEDA       (WRK-I).                      
                                                                        
           MOVE SCSBY032-S-DS-MOEDA                                     
             TO SCSBY130-S-DS-MOEDA       (WRK-I).                      
                                                                        
           MOVE COPER-VDA-EXTER           OF SCSBB010                   
             TO SCSBY130-S-COPER          (WRK-I)                       
                SCSBY033-E-COPER.                                       
                                                                        
           PERFORM 2400-ACESSAR-SCSB2033                                
                                                                        
           MOVE SCSBY033-S-CD-NBS         (1)                           
             TO SCSBY130-S-CD-NBS         (WRK-I).                      
                                                                        
           MOVE SCSBY033-S-DS-NBS         (1)                           
             TO SCSBY130-S-DS-NBS         (WRK-I).                      
                                                                        
           MOVE SCSBY033-S-DT-INICIO      (1)                           
             TO SCSBY130-S-DT-INICIO      (WRK-I).                      
                                                                        
           MOVE SCSBY033-S-DT-CONCLUSAO   (1)                           
             TO SCSBY130-S-DT-CONCLUSAO   (WRK-I).                      
                                                                        
           MOVE SCSBY033-S-VALOR          (1)                           
             TO SCSBY130-S-VALOR          (WRK-I).                      
                                                                        
           MOVE ZEROS                                                   
             TO SCSBY130-S-NFATMT         (WRK-I).                      
                                                                        
           MOVE ZEROS                                                   
             TO SCSBY130-S-VLR-FATURADO   (WRK-I).                      
                                                                        
           MOVE SPACES                                                  
             TO SCSBY130-S-DT-FATMT       (WRK-I).                      
                                                                        
           MOVE WRK-I                 TO SCSBY130-S-QTD-REG.            
                                                                        
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
                                                                        
           MOVE DANO-VDA-RECTA            OF SCSBB016                   
             TO SCSBY130-S-ANO            (WRK-I)                       
                SCSBY032-E-ANO                                          
                SCSBY033-E-ANO                                          
                SCSBY130-P-ULT-ANO.                                     
                                                                        
           MOVE NREG-VDA-RECTA            OF SCSBB016                   
             TO SCSBY130-S-NREG           (WRK-I)                       
                SCSBY032-E-NREG                                         
                SCSBY033-E-NREG                                         
                SCSBY130-P-ULT-NREG.                                    
                                                                        
           PERFORM 2300-ACESSAR-SCSB2032                                
                                                                        
           MOVE SCSBY032-S-NOME-ADQUIR                                  
             TO SCSBY130-S-NOME-ADQUIR    (WRK-I).                      
                                                                        
           MOVE SCSBY032-S-CPF-CNPJ-PRINC                               
             TO SCSBY130-S-CPF-CNPJ-PRINC (WRK-I).                      
                                                                        
           MOVE SCSBY032-S-CPF-CNPJ-FLIAL                               
             TO SCSBY130-S-CPF-CNPJ-FLIAL (WRK-I).                      
                                                                        
           MOVE SCSBY032-S-CPF-CNPJ-CTRL                                
             TO SCSBY130-S-CPF-CNPJ-CTRL  (WRK-I).                      
                                                                        
           MOVE SCSBY032-S-NIF                                          
             TO SCSBY130-S-NIF            (WRK-I).                      
                                                                        
           MOVE SCSBY032-S-CD-MOEDA                                     
             TO SCSBY130-S-CD-MOEDA       (WRK-I).                      
                                                                        
           MOVE SCSBY032-S-DS-MOEDA                                     
             TO SCSBY130-S-DS-MOEDA       (WRK-I).                      
                                                                        
           MOVE COPER-VDA-EXTER           OF SCSBB005                   
             TO SCSBY130-S-COPER          (WRK-I)                       
                SCSBY033-E-COPER.                                       
                                                                        
           PERFORM 2400-ACESSAR-SCSB2033                                
                                                                        
           MOVE SCSBY033-S-CD-NBS         (1)                           
             TO SCSBY130-S-CD-NBS         (WRK-I).                      
                                                                        
           MOVE SCSBY033-S-DS-NBS         (1)                           
             TO SCSBY130-S-DS-NBS         (WRK-I).                      
                                                                        
           MOVE SCSBY033-S-DT-INICIO      (1)                           
             TO SCSBY130-S-DT-INICIO      (WRK-I).                      
                                                                        
           MOVE SCSBY033-S-DT-CONCLUSAO   (1)                           
             TO SCSBY130-S-DT-CONCLUSAO   (WRK-I).                      
                                                                        
           MOVE SCSBY033-S-VALOR          (1)                           
             TO SCSBY130-S-VALOR          (WRK-I).                      
                                                                        
           MOVE NFATMT-VDA-EXTER          OF SCSBB022                   
             TO SCSBY130-S-NFATMT         (WRK-I)                       
                SCSBY130-P-ULT-NFATMT.                                  
                                                                        
           MOVE SCSBY033-S-VALOR-FATURADO (1)                           
             TO SCSBY130-S-VLR-FATURADO   (WRK-I).                      
                                                                        
           MOVE DFATMT-VDA-EXTER          OF SCSBB022                   
             TO SCSBY130-S-DT-FATMT       (WRK-I).                      
                                                                        
           MOVE WRK-I                 TO SCSBY130-S-QTD-REG.            
                                                                        
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
       7005-OPEN-CSR01-SCSBB016-JN        SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           MOVE '7005-OPEN-CSR01-SCSBB016-JN'                           
                                          TO FRWKGHEA-IDEN-PARAGRAFO    
                                                                        
           EXEC SQL                                                     
               OPEN  CSR01-SCSBB016-JN                                  
           END-EXEC.                                                    
                                                                        
           IF  (SQLCODE             NOT EQUAL ZEROS) OR                 
               (SQLWARN0                EQUAL 'W')                      
                SET   DB2-OPEN             TO TRUE                      
                MOVE '0015'                TO SCSBY130-COD-ERRO         
                MOVE 'SCSBB016'            TO WRK-STORED-PROC           
                MOVE 'TREG_VDA_RECTA'      TO WRK-NOME-TABELA           
                PERFORM 9996-ERRO-DB2                                   
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       7005-99-FIM.                        EXIT.                        
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       7010-FETCH-CSR01-SCSBB016-JN        SECTION.                     
      *----------------------------------------------------------------*
                                                                        
           MOVE '7010-FETCH-CSR01-SCSBB016-JN'                          
                                             TO FRWKGHEA-IDEN-PARAGRAFO 
                                                                        
           EXEC SQL FETCH CSR01-SCSBB016-JN INTO                        
                    :SCSBB016.DANO-VDA-RECTA                            
                   ,:SCSBB016.NREG-VDA-RECTA                            
                   ,:SCSBB010.COPER-VDA-EXTER                           
           END-EXEC.                                                    
                                                                        
           IF  (SQLCODE             NOT EQUAL ZEROS AND +100) OR        
               (SQLWARN0                EQUAL 'W')                      
                SET   DB2-FETCH            TO TRUE                      
                MOVE '0016'                TO SCSBY130-COD-ERRO         
                MOVE 'SCSBB016'            TO WRK-STORED-PROC           
                MOVE 'TREG_VDA_RECTA'      TO WRK-NOME-TABELA           
                PERFORM 9996-ERRO-DB2                                   
           END-IF.                                                      
                                                                        
           IF   SQLCODE                 EQUAL +100                      
                MOVE 'S'                   TO WRK-FIM-CSR               
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       7010-99-FIM.                        EXIT.                        
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       7015-CLOSE-CSR01-SCSBB016-JN       SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           MOVE '7015-CLOSE-CSR01-SCSBB016-JN'                          
                                             TO FRWKGHEA-IDEN-PARAGRAFO 
                                                                        
           EXEC SQL                                                     
               CLOSE CSR01-SCSBB016-JN                                  
           END-EXEC.                                                    
                                                                        
           IF  (SQLCODE             NOT EQUAL ZEROS) OR                 
               (SQLWARN0                EQUAL 'W')                      
                SET   DB2-CLOSE            TO TRUE                      
                MOVE '0017'                TO SCSBY130-COD-ERRO         
                MOVE 'SCSBB016'            TO WRK-STORED-PROC           
                MOVE 'TREG_VDA_VDA'        TO WRK-NOME-TABELA           
                PERFORM 9996-ERRO-DB2                                   
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       7015-99-FIM.                        EXIT.                        
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       8005-OPEN-CSR02-SCSBB016-JN        SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           MOVE '8005-OPEN-CSR02-SCSBB016-JN'                           
                                          TO FRWKGHEA-IDEN-PARAGRAFO    
                                                                        
           EXEC SQL                                                     
               OPEN  CSR02-SCSBB016-JN                                  
           END-EXEC.                                                    
                                                                        
           IF  (SQLCODE             NOT EQUAL ZEROS) OR                 
               (SQLWARN0                EQUAL 'W')                      
                SET   DB2-OPEN             TO TRUE                      
                MOVE '0018'                TO SCSBY130-COD-ERRO         
                MOVE 'SCSBB016'            TO WRK-STORED-PROC           
                MOVE 'TREG_VDA_RECTA'      TO WRK-NOME-TABELA           
                PERFORM 9996-ERRO-DB2                                   
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       8005-99-FIM.                        EXIT.                        
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       8010-FETCH-CSR02-SCSBB016-JN        SECTION.                     
      *----------------------------------------------------------------*
                                                                        
           MOVE '7010-FETCH-CSR02-SCSBB016-JN'                          
                                             TO FRWKGHEA-IDEN-PARAGRAFO 
                                                                        
           EXEC SQL FETCH CSR02-SCSBB016-JN INTO                        
                    :SCSBB016.DANO-VDA-RECTA                            
                   ,:SCSBB016.NREG-VDA-RECTA                            
                   ,:SCSBB005.COPER-VDA-EXTER                           
                   ,:SCSBB022.NFATMT-VDA-EXTER                          
                   ,:SCSBB022.DFATMT-VDA-EXTER                          
           END-EXEC.                                                    
                                                                        
           IF  (SQLCODE             NOT EQUAL ZEROS AND +100) OR        
               (SQLWARN0                EQUAL 'W')                      
                SET   DB2-FETCH            TO TRUE                      
                MOVE '0019'                TO SCSBY130-COD-ERRO         
                MOVE 'SCSBB016'            TO WRK-STORED-PROC           
                MOVE 'TREG_VDA_RECTA'      TO WRK-NOME-TABELA           
                PERFORM 9996-ERRO-DB2                                   
           END-IF.                                                      
                                                                        
           IF   SQLCODE                 EQUAL +100                      
                MOVE 'S'                   TO WRK-FIM-CSR               
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       8010-99-FIM.                        EXIT.                        
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       8015-CLOSE-CSR02-SCSBB016-JN       SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           MOVE '8015-CLOSE-CSR02-SCSBB016-JN'                          
                                             TO FRWKGHEA-IDEN-PARAGRAFO 
                                                                        
           EXEC SQL                                                     
               CLOSE CSR02-SCSBB016-JN                                  
           END-EXEC.                                                    
                                                                        
           IF  (SQLCODE             NOT EQUAL ZEROS) OR                 
               (SQLWARN0                EQUAL 'W')                      
                SET   DB2-CLOSE            TO TRUE                      
                MOVE '0020'                TO SCSBY130-COD-ERRO         
                MOVE 'SCSBB016'            TO WRK-STORED-PROC           
                MOVE 'TREG_VDA_VDA'        TO WRK-NOME-TABELA           
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
                                                                        
           MOVE  16                        TO  SCSBY130-COD-RETORNO     
           MOVE  'SCSB0032'                TO  SCSBY130-COD-MENSAGEM    
                                                                        
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
                                                                        
           MOVE  16                       TO  SCSBY130-COD-RETORNO      
           MOVE  'SCSB0033'               TO  SCSBY130-COD-MENSAGEM     
                                                                        
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
                                                                        

      *================================================================*
       IDENTIFICATION DIVISION.                                         
      *================================================================*
      *                                                                *
       PROGRAM-ID.    SCSB2122.                                         
       AUTHOR.        ANDERSON.                                         
      *                                                                *
      *================================================================*
      *                          BRQ IT SERVICES                       *
      *================================================================*
      *                                                                *
      *  PROGRAMADOR.......: ANDERSON MARTINS                          *
      *  ANALISTA..........: FERNANDA CARUSO                           *
      *  DATA..............: 15/08/2013                                *
      *                                                                *
      *================================================================*
      *                                                                *
      *  OBJETIVO.......: OBTER A LISTA DAS EMPRESAS (SAP) CADASTRADAS *
      *                   NO SCSB DE ACORDO COM TIPO DE REGISTRO RVS   *
      *                   OU RAS                                       *
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
      * SCSBY122 BLOCO DE ENTRADA E SAIDA - MODULO FUNCIONAL           *
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
                                                                        
       77  WRK-PROGRAMA            PIC  X(008) VALUE 'SCSB2122'.        
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
           'AREA TRATAMENTO CHAMADA - UORG1397'.                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
       01  WRK-AREA-UORG1397.                                           
           COPY 'UORGW000'.                                             
           COPY 'UORGW397'.                                             
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                    PIC  X(008) VALUE 'AREA DB2'.      
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
           EXEC  SQL    INCLUDE      SQLCA      END-EXEC.               
                                                                        
           EXEC  SQL    INCLUDE      SCSBB013   END-EXEC.               
                                                                        
           EXEC  SQL    INCLUDE      SCSBB016   END-EXEC.               
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
      *  DECLARACAO DO CURSOR - CSR01-SCSBB016 - TABELA TREG_VDA_RECTA *
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
           EXEC SQL DECLARE CSR01-SCSBB016 CURSOR FOR SELECT            
                     CPSSOA_JURID                                       
               FROM DB2PRD.TREG_VDA_RECTA                               
              WHERE ((CPSSOA_JURID        > :WRK-CPSSOA)                
                 OR   CPSSOA_JURID        = :WRK-CPSSOA)                                
              GROUP BY                                                  
                  CPSSOA_JURID                                     
              ORDER BY                                                  
                  CPSSOA_JURID                                         
           END-EXEC.                                                    
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
      * DECLARACAO DO CURSOR - CSR02-SCSBB013 - TABELA TREG_COMPR_RECTA*
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
           EXEC SQL DECLARE CSR02-SCSBB013 CURSOR FOR SELECT            
                     CPSSOA_JURID                                       
               FROM DB2PRD.TREG_COMPR_RECTA                             
              WHERE ((CPSSOA_JURID        > :WRK-CPSSOA)                
                 OR   CPSSOA_JURID        = :WRK-CPSSOA)                
              GROUP BY                                                  
                  CPSSOA_JURID                                         
              ORDER BY                                                  
                  CPSSOA_JURID                                         
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
           COPY SCSBY122.                                               
                                                                        
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
                                                                        
           MOVE ZEROS                     TO SCSBY122-COD-RETORNO       
           MOVE '0000'                    TO SCSBY122-COD-ERRO          
           MOVE 'SCSB0026'                TO SCSBY122-COD-MENSAGEM.     
                                                                        
      *----------------------------------------------------------------*
       1000-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
      *   CONSISTIR DADOS DE ENTRADA - FUNCIONAL                       *
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
      *----------------------------------------------------------------*
       1200-CONSISTIR-DADOS               SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           IF SCSBY122-IND-PAGINACAO    NOT EQUAL 'I' AND 'S'           
              MOVE 08                     TO SCSBY122-COD-RETORNO       
              MOVE '0001'                 TO SCSBY122-COD-ERRO          
              MOVE 'SCSB0039'             TO SCSBY122-COD-MENSAGEM      
              PERFORM 3000-FINALIZAR                                    
           END-IF.                                                      
                                                                        
           IF SCSBY122-E-TPO-REG        NOT EQUAL 'RVS' AND 'RAS'       
              MOVE 08                     TO SCSBY122-COD-RETORNO       
              MOVE '0002'                 TO SCSBY122-COD-ERRO          
              MOVE 'SCSB0040'             TO SCSBY122-COD-MENSAGEM      
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
                                                                        
           MOVE 50                        TO WRK-QTDE-LIMITE            
                                                                        
           EVALUATE TRUE                                                
                                                                        
               WHEN SCSBY122-P-INICIAL                                  
                                                                        
                    MOVE ZEROS            TO WRK-CPSSOA                 
                                                                        
               WHEN SCSBY122-P-SEGUINTE                                 
                                                                        
                    MOVE SCSBY122-P-ULT-CPSSOA-JURID                    
                                           TO WRK-CPSSOA                
                                                                        
               WHEN OTHER                                               
                                                                        
                    MOVE 16                TO SCSBY122-COD-RETORNO      
                    MOVE '0003'            TO SCSBY122-COD-ERRO         
                    MOVE 'SCSB0039'        TO SCSBY122-COD-MENSAGEM     
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
                                                                        
           IF SCSBY122-E-TPO-REG         EQUAL 'RVS'                    
                                                                        
              PERFORM 7005-OPEN-CSR01-SCSBB016                          
              PERFORM 7010-FETCH-CSR01-SCSBB016                         
                                                                        
              PERFORM UNTIL SQLCODE EQUAL +100 OR                       
                      WRK-I   GREATER  WRK-QTDE-LIMITE - 1              
                      PERFORM 2910-MONTAR-COMBO-RVS                     
                      PERFORM 7010-FETCH-CSR01-SCSBB016                 
              END-PERFORM                                               
                                                                        
              PERFORM 7015-CLOSE-CSR01-SCSBB016                         
                                                                        
           ELSE                                                         
                                                                        
              PERFORM 7025-OPEN-CSR02-SCSBB013                          
              PERFORM 7030-FETCH-CSR02-SCSBB013                         
                                                                        
              PERFORM UNTIL SQLCODE EQUAL +100 OR                       
                      WRK-I   GREATER  WRK-QTDE-LIMITE - 1              
                      PERFORM 2920-MONTAR-COMBO-RAS                     
                      PERFORM 7030-FETCH-CSR02-SCSBB013                 
              END-PERFORM                                               
                                                                        
              PERFORM 7035-CLOSE-CSR02-SCSBB013                         
                                                                        
           END-IF.                                                      
                                                                        
           IF  WRK-I                  EQUAL ZEROS                       
               MOVE 08                TO SCSBY122-COD-RETORNO           
               IF  SCSBY122-IND-PAGINACAO EQUAL 'I'                     
                   MOVE '0004'           TO SCSBY122-COD-ERRO           
                   MOVE 'SCSB0029'       TO SCSBY122-COD-MENSAGEM       
               ELSE                                                     
                   MOVE '0005'           TO SCSBY122-COD-ERRO           
                   MOVE 'SCSB0030'       TO SCSBY122-COD-MENSAGEM       
               END-IF                                                   
               GO                        TO 2000-99-FIM                 
           END-IF                                                       
                                                                        
           IF  WRK-FIM-CSR            EQUAL 'S'                         
               MOVE 00                   TO SCSBY122-COD-RETORNO        
           ELSE                                                         
               MOVE 01                   TO SCSBY122-COD-RETORNO        
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       2000-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
      *  ACESSAR UORG1397 PARA OBTER CODIGO DA EMPRESA NO SAP          *
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
      *----------------------------------------------------------------*
       2100-ACESSAR-UORG1397              SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           MOVE '2100-ACESSAR-UORG1397'   TO FRWKGHEA-IDEN-PARAGRAFO    
                                                                        
           MOVE  'D'                      TO  UORGW397-E-AMBIENTE.      
           MOVE  SPACES                   TO  UORGW397-E-CEMPR-SAP.     
                                                                        
           EXEC    CICS LINK                                            
                   PROGRAM   (WRK-UORG1397)                             
                   COMMAREA  (WRK-AREA-UORG1397)                        
                   LENGTH    (LENGTH OF WRK-AREA-UORG1397)              
                   NOHANDLE                                             
           END-EXEC.                                                    
                                                                        
           IF EIBRESP                NOT EQUAL DFHRESP(NORMAL)          
              MOVE '0006'                 TO SCSBY122-COD-ERRO          
              PERFORM 9997-ERRO-CICS                                    
           END-IF.                                                      
                                                                        
           IF UORGW000-COD-RETORNO        OF WRK-AREA-UORG1397          
                                          NOT EQUAL ZEROS AND 08        
              MOVE '0007'                 TO SCSBY122-COD-ERRO          
              MOVE UORGW000-BLOCO-RETORNO OF WRK-AREA-UORG1397          
                                          TO SCSBY122-BLOCO-RETORNO     
                                             WRK-BLOCO-RETORNO          
              MOVE WRK-UORG1397           TO WRK-MODULO                 
              PERFORM 9998-ERRO-MODULO                                  
           END-IF                                                       
                                                                        
           IF UORGW000-COD-RETORNO        OF WRK-AREA-UORG1397          
                                          EQUAL 08                      
              MOVE '0008'                 TO SCSBY122-COD-ERRO          
              MOVE 'SCSB0029'             TO SCSBY122-COD-MENSAGEM      
              PERFORM 3000-FINALIZAR                                    
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       2100-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
      *   FORMATA BL DE SAIDA(FUNCIONAL) - LISTA RVS                   *
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
      *----------------------------------------------------------------*
       2910-MONTAR-COMBO-RVS              SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           ADD 1                          TO WRK-I.                     
                                                                        
           MOVE CPSSOA-JURID              OF SCSBB016                   
             TO SCSBY122-P-ULT-CPSSOA-JURID                             
                                                                        
                                                                        
           MOVE CPSSOA-JURID           OF SCSBB016                   
           TO UORGW397-E-CPSSOA-JURID                                
                                                                        
           PERFORM 2100-ACESSAR-UORG1397                             
                                                                        
           MOVE UORGW397-S-CEMPR-SAP   (1:4)                         
           TO SCSBY122-S-CFIRMA        (WRK-I)                       
              SCSBY122-P-ULT-CFIRMA                                                                                                        

                                                                        
           MOVE WRK-I                 TO SCSBY122-S-QTD-REG.            
                                                                        
      *----------------------------------------------------------------*
       2910-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
      *   FORMATA BL DE SAIDA(FUNCIONAL) - LISTA RAS                   *
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
      *----------------------------------------------------------------*
       2920-MONTAR-COMBO-RAS              SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           ADD 1                          TO WRK-I.                     
                                                                        
           MOVE CPSSOA-JURID              OF SCSBB013                   
             TO SCSBY122-P-ULT-CPSSOA-JURID                                                                                                    


           MOVE CPSSOA-JURID           OF SCSBB013                   
           TO UORGW397-E-CPSSOA-JURID                                
                                                                       
           PERFORM 2100-ACESSAR-UORG1397                             
                                                                        
           MOVE UORGW397-S-CEMPR-SAP   (1:4)                         
           TO SCSBY122-S-CFIRMA        (WRK-I)                       
              SCSBY122-P-ULT-CFIRMA                                  
                                                                        
                                                                        
           MOVE WRK-I                 TO SCSBY122-S-QTD-REG.            
                                                                        
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
       7005-OPEN-CSR01-SCSBB016           SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           MOVE '7005-OPEN-CSR01-SCSBB016'                              
                                          TO FRWKGHEA-IDEN-PARAGRAFO    
                                                                        
           MOVE WRK-CPSSOA                TO CPSSOA-JURID OF SCSBB016   
                                                                        
           EXEC SQL                                                     
               OPEN  CSR01-SCSBB016                                     
           END-EXEC.                                                    
                                                                        
           IF  (SQLCODE             NOT EQUAL ZEROS) OR                 
               (SQLWARN0                EQUAL 'W')                      
                SET   DB2-OPEN             TO TRUE                      
                MOVE '0009'                TO SCSBY122-COD-ERRO         
                MOVE 'SCSBB016'            TO WRK-STORED-PROC           
                MOVE 'TREG_VDA_RECTA'      TO WRK-NOME-TABELA           
                PERFORM 9996-ERRO-DB2                                   
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       7005-99-FIM.                        EXIT.                        
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       7010-FETCH-CSR01-SCSBB016           SECTION.                     
      *----------------------------------------------------------------*
                                                                        
           MOVE '7010-FETCH-CSR01-SCSBB016'                             
                                           TO FRWKGHEA-IDEN-PARAGRAFO   
                                                                        
           EXEC SQL FETCH CSR01-SCSBB016 INTO                           
                    :SCSBB016.CPSSOA-JURID                              
           END-EXEC.                                                    
                                                                        
           IF  (SQLCODE             NOT EQUAL ZEROS AND +100) OR        
               (SQLWARN0                EQUAL 'W')                      
                SET   DB2-FETCH            TO TRUE                      
                MOVE '0010'                TO SCSBY122-COD-ERRO         
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
       7015-CLOSE-CSR01-SCSBB016          SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           MOVE '7015-CLOSE-CSR01-SCSBB016'                             
                                          TO FRWKGHEA-IDEN-PARAGRAFO    
                                                                        
           EXEC SQL                                                     
               CLOSE CSR01-SCSBB016                                     
           END-EXEC.                                                    
                                                                        
           IF  (SQLCODE             NOT EQUAL ZEROS) OR                 
               (SQLWARN0                EQUAL 'W')                      
                SET   DB2-CLOSE            TO TRUE                      
                MOVE '0011'                TO SCSBY122-COD-ERRO         
                MOVE 'SCSBB016'            TO WRK-STORED-PROC           
                MOVE 'TREG_VDA_RECTA'      TO WRK-NOME-TABELA           
                PERFORM 9996-ERRO-DB2                                   
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       7015-99-FIM.                        EXIT.                        
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       7025-OPEN-CSR02-SCSBB013           SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           MOVE '7025-OPEN-CSR02-SCSBB013'                              
                                          TO FRWKGHEA-IDEN-PARAGRAFO    
                                                                        
           MOVE WRK-CPSSOA                TO CPSSOA-JURID OF SCSBB013   
                                                                        
           EXEC SQL                                                     
               OPEN  CSR02-SCSBB013                                     
           END-EXEC.                                                    
                                                                        
           IF  (SQLCODE             NOT EQUAL ZEROS) OR                 
               (SQLWARN0                EQUAL 'W')                      
                SET   DB2-OPEN             TO TRUE                      
                MOVE '0012'                TO SCSBY122-COD-ERRO         
                MOVE 'SCSBB013'            TO WRK-STORED-PROC           
                MOVE 'TREG_COMPR_RECTA'    TO WRK-NOME-TABELA           
                PERFORM 9996-ERRO-DB2                                   
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       7025-99-FIM.                        EXIT.                        
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       7030-FETCH-CSR02-SCSBB013           SECTION.                     
      *----------------------------------------------------------------*
                                                                        
           MOVE '7030-FETCH-CSR02-SCSBB013'                             
                                           TO FRWKGHEA-IDEN-PARAGRAFO   
                                                                        
           EXEC SQL FETCH CSR02-SCSBB013 INTO                           
                    :SCSBB013.CPSSOA-JURID                              
           END-EXEC.                                                    
                                                                        
           IF  (SQLCODE             NOT EQUAL ZEROS AND +100) OR        
               (SQLWARN0                EQUAL 'W')                      
                SET   DB2-FETCH            TO TRUE                      
                MOVE '0013'                TO SCSBY122-COD-ERRO         
                MOVE 'SCSBB013'            TO WRK-STORED-PROC           
                MOVE 'TREG_COMP_RECTA'     TO WRK-NOME-TABELA           
                PERFORM 9996-ERRO-DB2                                   
           END-IF.                                                      
                                                                        
           IF   SQLCODE                 EQUAL +100                      
                MOVE 'S'                   TO WRK-FIM-CSR               
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       7030-99-FIM.                        EXIT.                        
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       7035-CLOSE-CSR02-SCSBB013          SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           MOVE '7015-CLOSE-CSR02-SCSBB013'                             
                                          TO FRWKGHEA-IDEN-PARAGRAFO    
                                                                        
           EXEC SQL                                                     
               CLOSE CSR02-SCSBB013                                     
           END-EXEC.                                                    
                                                                        
           IF  (SQLCODE             NOT EQUAL ZEROS) OR                 
               (SQLWARN0                EQUAL 'W')                      
                SET   DB2-CLOSE            TO TRUE                      
                MOVE '0014'                TO SCSBY122-COD-ERRO         
                MOVE 'SCSBB013'            TO WRK-STORED-PROC           
                MOVE 'TREG_COMPR_RECTA'    TO WRK-NOME-TABELA           
                PERFORM 9996-ERRO-DB2                                   
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       7035-99-FIM.                        EXIT.                        
      *----------------------------------------------------------------*
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
      * TRATAMENTO DE ERRO DB2.                                        *
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
      *----------------------------------------------------------------*
       9996-ERRO-DB2                      SECTION.                      
      *----------------------------------------------------------------*
                                                                        
      *--> CARREGAR DADOS NA AREA DE COMUNICACAO                        
                                                                        
           MOVE  16                        TO  SCSBY122-COD-RETORNO     
           MOVE  'SCSB0032'                TO  SCSBY122-COD-MENSAGEM    
                                                                        
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
                                                                        
           MOVE  16                       TO  SCSBY122-COD-RETORNO      
           MOVE  'SCSB0033'               TO  SCSBY122-COD-MENSAGEM     
                                                                        
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
                                                                        

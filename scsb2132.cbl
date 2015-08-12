      *================================================================*
       IDENTIFICATION DIVISION.                                         
      *================================================================*
      *                                                                *
       PROGRAM-ID.    SCSB2132.                                         
       AUTHOR.        CRISTIANO SOARES DE SOUZA.                        
      *                                                                *
      *================================================================*
      *                          BRQ IT SERVICES                       *
      *================================================================*
      *                                                                *
      *  PROGRAMADOR.......: CRISTIANO SOARES DE SOUZA - BRQ           *
      *  ANALISTA..........: FERNANDA CARUSO           - BRQ           *
      *  DATA..............: AGOSTO/2013                               *
      *                                                                *
      *================================================================*
      *                                                                *
      *  OBJETIVO..........:                                           *
      *                                                                *
      *      GERAR INFORMACOES PARA O RELATORIO DAS RAS'S TRANSMITIDAS *
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
      * SCSBY132 BLOCO DE ENTRADA E SAIDA - MODULO FUNCIONAL           *
      *                                                                *
      *================================================================*
      *                            ALTERACAO                           *
      *----------------------------------------------------------------*
      *                                                                *
      *  PROGRAMADOR:  ANDERSON MARTINS - BRQ                          *
      *  ANALISTA...:  FERNANDA CARUSO  - BRQ                          *
      *  DATA.......:  29/10/2013                                      *
      *  OBJETIVO...:  INCLUSAO DO CAMPO NUMERO DE PROTOCOLO NA SAIDA  *
      *                'SCSBY132-S-CPROT-LOTE'                         *
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
                                                                        
       77  WRK-PROGRAMA            PIC  X(008) VALUE 'SCSB2132'.        
       77  WRK-SCSB2056            PIC  X(008) VALUE 'SCSB2056'.        
       77  WRK-SCSB2057            PIC  X(008) VALUE 'SCSB2057'.        
       77  WRK-FRWK1999            PIC  X(008) VALUE 'FRWK1999'.        
       77  WRK-UORG1397            PIC  X(008) VALUE 'UORG1397'.        
       77  WRK-SQLCODE             PIC S9(009) VALUE ZEROS COMP-3.      
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
      *  AREAS AUXILIARES                                              *
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
      *                                                                 
      *---> CHAVES PARA VERIFICAR QUEBRAS                               
      *                                                                 
                                                                        
      ***----------------- LOTE ATUAL                                   
       01  WRK-LOTE-ATU.                                                
           05  WRK-CHV-DANO-LOTE-ATU PIC  9(004) COMP-3 VALUE ZEROS.    
           05  WRK-CHV-CTPO-LOTE-ATU PIC  9(001) COMP-3 VALUE ZEROS.    
           05  WRK-NLOTE-ATU         PIC  9(009) COMP-3 VALUE ZEROS.    
                                                                        
      ***----------------- RAS ATUAL                                    
       01  WRK-RAS-ATU.                                                 
           05  WRK-CHV-DANO-COM-ATU  PIC  9(004) COMP-3 VALUE ZEROS.    
           05  WRK-CHV-NREG-COM-ATU  PIC  9(009) COMP-3 VALUE ZEROS.    
                                                                        
      ***----------------- PAGAMENTO ATUAL                              
       01  WRK-PAGTO-ATU.                                               
           05 WRK-CHV-DANO-PAGTO-ATU PIC  9(004) COMP-3 VALUE ZEROS.    
           05 WRK-CHV-NREG-PAGTO-ATU PIC  9(009) COMP-3 VALUE ZEROS.    
           05 WRK-CHV-NPAGTO-ATU     PIC  9(010) COMP-3 VALUE ZEROS.    
                                                                        
      ***----------------- LOTE ANTERIOR                                
       01  WRK-LOTE-ANT.                                                
           05  WRK-CHV-DANO-LOTE-ANT PIC  9(004) COMP-3 VALUE ZEROS.    
           05  WRK-CHV-CTPO-LOTE-ANT PIC  9(001) COMP-3 VALUE ZEROS.    
           05  WRK-NLOTE-ANT         PIC  9(009) COMP-3 VALUE ZEROS.    
                                                                        
      ***----------------- RAS ANTERIOR                                 
       01  WRK-RAS-ANT.                                                 
           05  WRK-CHV-DANO-COM-ANT  PIC  9(004) COMP-3 VALUE ZEROS.    
           05  WRK-CHV-NREG-COM-ANT  PIC  9(009) COMP-3 VALUE ZEROS.    
                                                                        
      ***----------------- PAGAMENTO ANTERIOR                           
       01  WRK-PAGTO-ANT.                                               
           05 WRK-CHV-DANO-PAGTO-ANT PIC  9(004) COMP-3 VALUE ZEROS.    
           05 WRK-CHV-NREG-PAGTO-ANT PIC  9(009) COMP-3 VALUE ZEROS.    
           05 WRK-CHV-NPAGTO-ANT     PIC  9(010) COMP-3 VALUE ZEROS.    
                                                                        
      *---> VARIAVEIS DE NULIDADE                                       
                                                                        
       01  WRK-DANO-COMPR-OPER-NULL   PIC S9(004) COMP   VALUE ZEROS.   
       01  WRK-NREG-COMPR-OPER-NULL   PIC S9(004) COMP   VALUE ZEROS.   
       01  WRK-COPER-COMPR-EXTER-NULL PIC S9(004) COMP   VALUE ZEROS.   
       01  WRK-DANO-COMPR-PGTO-NULL   PIC S9(004) COMP   VALUE ZEROS.   
       01  WRK-NREG-COMPR-PGTO-NULL   PIC S9(004) COMP   VALUE ZEROS.   
       01  WRK-NPGTO-COMPR-EXTER-NULL PIC S9(004) COMP   VALUE ZEROS.   
                                                                        
      *---> CHAVE ANTERIOR DE CONTROLE INTERNO                          
       01  WRK-CHAVE-ANT.                                               
           05  WRK-ANO-LOTE-ANT    PIC  9(004)        VALUE ZEROS.      
           05  WRK-CTPO-LOTE-ANT   PIC  9(001)        VALUE ZEROS.      
           05  WRK-NUMERO-LOT-ANT  PIC  9(009)        VALUE ZEROS.      
           05  WRK-ANO-RAS-ANT     PIC  9(004)        VALUE ZEROS.      
           05  WRK-NREG-ANT        PIC  9(009)        VALUE ZEROS.      
           05  WRK-NSEQ-ANT        PIC  9(009)        VALUE ZEROS.      
                                                                        
      *---> CHAVE ATUAL DE CONTROLE INTERNO                             
       01  WRK-CHAVE-ATU.                                               
           05  WRK-ANO-LOTE-ATU    PIC  9(004)        VALUE ZEROS.      
           05  WRK-CTPO-LOTE-ATU   PIC  9(001)        VALUE ZEROS.      
           05  WRK-NUMERO-LOT-ATU  PIC  9(009)        VALUE ZEROS.      
           05  WRK-ANO-RAS-ATU     PIC  9(004)        VALUE ZEROS.      
           05  WRK-NREG-ATU        PIC  9(009)        VALUE ZEROS.      
           05  WRK-NSEQ-ATU        PIC  9(009)        VALUE ZEROS.      
                                                                        
       01  WRK-ANO-LOTE-INI        PIC S9(004) COMP-3 VALUE ZEROS.      
       01  WRK-ANO-LOTE-FIM        PIC S9(004) COMP-3 VALUE ZEROS.      
       01  WRK-NLOTE-REG-INI       PIC S9(009) COMP-3 VALUE ZEROS.      
       01  WRK-NLOTE-REG-FIM       PIC S9(009) COMP-3 VALUE ZEROS.      
       01  WRK-DATA-INI            PIC  X(010)        VALUE SPACES.     
       01  WRK-DATA-FIM            PIC  X(010)        VALUE SPACES.     
       01  WRK-CPROT-LOTE-INI      PIC  X(012)        VALUE LOW-VALUES. 
       01  WRK-CPROT-LOTE-FIM      PIC  X(012)        VALUE HIGH-VALUES.
                                                                        
       01  WRK-I                   PIC  9(003) COMP-3 VALUE ZEROS.      
       01  WRK-II                  PIC  9(003) COMP-3 VALUE ZEROS.      
       01  WRK-QTDE-LIMITE         PIC  9(003) COMP-3 VALUE ZEROS.      
       01  WRK-MODULO              PIC  X(008)        VALUE SPACES.     
                                                                        
       01  WRK-FIM-CSR             PIC  X(001) VALUE SPACES.            
       01  WRK-FIM-CSR2            PIC  X(001) VALUE SPACES.            
       01  WRK-NOME-TABELA         PIC  X(032) VALUE SPACES.            
       01  WRK-STORED-PROC         PIC  X(008) VALUE SPACES.            
                                                                        
       01  WRK-TRATAMENTOS-ERROS.                                       
           05  WRK-BLOCO-RETORNO.                                       
               10 WRK-COD-RETORNO  PIC  9(002) VALUE ZEROS.             
               10 WRK-COD-ERRO     PIC  X(004) VALUE SPACES.            
               10 WRK-COD-MENSAGEM PIC  X(008) VALUE SPACES.            
                                                                        
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
       01  FILLER                  PIC  X(080) VALUE                    
           'AREA MODULO SCSB2056'.                                      
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
       01  WRK-AREA-SCSB2056.                                           
           COPY SCSBY056.                                               
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                  PIC  X(080) VALUE                    
           'AREA MODULO SCSB2057'.                                      
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
       01  WRK-AREA-SCSB2057.                                           
           COPY SCSBY057.                                               
                                                                        
      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050)  VALUE                
           'AREA DE INTERFACE COM SERVICO FUNCIONAL'.                   
      *----------------------------------------------------------------*
                                                                        
       01  WRK-AREA-UORG1397.                                           
           COPY 'UORGW000'.                                             
           COPY 'UORGW397'.                                             
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                    PIC  X(008) VALUE 'AREA DB2'.      
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
           EXEC SQL                                                     
                INCLUDE SQLCA                                           
           END-EXEC.                                                    
                                                                        
      *----> REGISTROS DE PAGAMENTOS                                    
           EXEC SQL                                                     
                INCLUDE SCSBB012                                        
           END-EXEC.                                                    
                                                                        
      *----> RAS                                                        
           EXEC SQL                                                     
                INCLUDE SCSBB013                                        
           END-EXEC.                                                    
                                                                        
      *----> DETALHE DO LOTE                                            
           EXEC SQL                                                     
                INCLUDE SCSBB018                                        
           END-EXEC.                                                    
                                                                        
      *----> LOTE                                                       
           EXEC SQL                                                     
                INCLUDE SCSBB020                                        
           END-EXEC.                                                    
                                                                        
      *----> PAGAMENTO                                                  
           EXEC SQL                                                     
                INCLUDE SCSBB023                                        
           END-EXEC.                                                    
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
      *   DECLARACAO DO CURSOR - CSR01 - SCSBB013 / 19 / 21             
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
           EXEC SQL DECLARE CSR01-SCSBB020-JN CURSOR FOR SELECT         
                    A.DLOTE_RECTA_FEDRL                                 
                   ,A.DANO_LOTE_RECTA                                   
                   ,A.CTPO_LOTE_COMPR                                   
                   ,A.NLOTE_REG_COMPR                                   
                   ,VALUE (A.CPROT_LOTE_RECTA, ' ')                     
                   ,B.DANO_COMPR_RECTA                                  
                   ,B.NREG_COMPR_RECTA                                  
                   ,B.NDETLH_LOTE_COMPR                                 
                   ,B.DANO_COMPR_OPER                                   
                   ,B.NREG_COMPR_OPER                                   
                   ,B.COPER_COMPR_EXTER                                 
                   ,B.DANO_COMPR_PGTO                                   
                   ,B.NREG_COMPR_PGTO                                   
                   ,B.NPGTO_COMPR_EXTER                                 
               FROM DB2PRD.TLOTE_REG_COMPR   A,                         
                    DB2PRD.TDETLH_LOTE_COMPR B,                         
                    DB2PRD.TREG_COMPR_RECTA  C                          
              WHERE  (A.DANO_LOTE_RECTA   = B.DANO_LOTE_RECTA           
                AND   A.CTPO_LOTE_COMPR   = B.CTPO_LOTE_COMPR           
                AND   A.NLOTE_REG_COMPR   = B.NLOTE_REG_COMPR           
                AND   B.DANO_COMPR_RECTA  = C.DANO_COMPR_RECTA          
                AND   B.NREG_COMPR_RECTA  = C.NREG_COMPR_RECTA          
                AND  (A.CPROT_LOTE_RECTA  BETWEEN :WRK-CPROT-LOTE-INI   
                AND                               :WRK-CPROT-LOTE-FIM)) 
                AND  (A.DLOTE_RECTA_FEDRL BETWEEN :WRK-DATA-INI         
                AND                               :WRK-DATA-FIM)        
                AND  (C.CPSSOA_JURID      = :SCSBB013.CPSSOA-JURID)     
                AND  (A.CTPO_LOTE_COMPR   = :SCSBB020.CTPO-LOTE-COMPR)  
                AND  (A.DANO_LOTE_RECTA   BETWEEN :WRK-ANO-LOTE-INI     
                AND                               :WRK-ANO-LOTE-FIM)    
                AND  (A.NLOTE_REG_COMPR   BETWEEN :WRK-NLOTE-REG-INI    
                AND                               :WRK-NLOTE-REG-FIM)   
                AND ((A.DANO_LOTE_RECTA   > :SCSBB020.DANO-LOTE-RECTA)  
                 OR  (A.DANO_LOTE_RECTA   = :SCSBB020.DANO-LOTE-RECTA   
                AND   A.NLOTE_REG_COMPR   > :SCSBB020.NLOTE-REG-COMPR)  
                 OR  (A.DANO_LOTE_RECTA   = :SCSBB020.DANO-LOTE-RECTA   
                AND   A.NLOTE_REG_COMPR   = :SCSBB020.NLOTE-REG-COMPR)  
                AND  (B.DANO_COMPR_RECTA  > :SCSBB018.DANO-COMPR-RECTA) 
                 OR  (B.DANO_COMPR_RECTA  = :SCSBB018.DANO-COMPR-RECTA  
                AND   B.NREG_COMPR_RECTA  > :SCSBB018.NREG-COMPR-RECTA) 
                 OR  (B.DANO_COMPR_RECTA  = :SCSBB018.DANO-COMPR-RECTA  
                AND   B.NREG_COMPR_RECTA  = :SCSBB018.NREG-COMPR-RECTA  
                AND   B.NDETLH_LOTE_COMPR >                             
                                           :SCSBB018.NDETLH-LOTE-COMPR))
            ORDER BY  DANO_LOTE_RECTA                                   
                     ,NLOTE_REG_COMPR                                   
                     ,DANO_COMPR_RECTA                                  
                     ,NREG_COMPR_RECTA                                  
                     ,NDETLH_LOTE_COMPR                                 
           END-EXEC.                                                    
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
      *   DECLARACAO DO CURSOR - CSR02 - SCSBB012 / 22                  
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
           EXEC SQL DECLARE CSR02-SCSBB012-JN CURSOR FOR SELECT         
                    A.COPER_COMPR_EXTER                                 
                   ,B.DPGTO_COMPR_EXTER                                 
               FROM DB2PRD.TPGTO_OPER_EXTER  A,                         
                    DB2PRD.TPGTO_COMPR_EXTER B                          
              WHERE  (A.DANO_COMPR_RECTA  = B.DANO_COMPR_RECTA          
                AND   A.NREG_COMPR_RECTA  = B.NREG_COMPR_RECTA          
                AND   A.NPGTO_COMPR_EXTER = B.NPGTO_COMPR_EXTER)        
                AND  (A.DANO_COMPR_RECTA  = :SCSBB012.DANO-COMPR-RECTA  
                AND   A.NREG_COMPR_RECTA  = :SCSBB012.NREG-COMPR-RECTA  
                AND   A.DANO_COMPR_PGTO   = :SCSBB012.DANO-COMPR-RECTA  
                AND   A.NREG_COMPR_PGTO   = :SCSBB012.NREG-COMPR-RECTA  
                AND   A.NPGTO_COMPR_EXTER = :SCSBB012.NPGTO-COMPR-EXTER)
                AND  (A.COPER_COMPR_EXTER > :SCSBB012.COPER-COMPR-EXTER)
            ORDER BY A.COPER_COMPR_EXTER                                
           END-EXEC.                                                    
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                  PIC  X(080) VALUE                    
           'FINAL DA WORKING STORAGE'.                                  
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
      *----------------------------------------------------------------*
       LINKAGE SECTION.                                                 
      *----------------------------------------------------------------*
       01  DFHCOMMAREA.                                                 
           COPY SCSBY132.                                               
      *----------------------------------------------------------------*
                                                                        
      *================================================================ 
       PROCEDURE DIVISION USING DFHCOMMAREA.                            
      *================================================================ 
                                                                        
      *----------------------------------------------------------------*
      *   ROTINA PRINCIPAL                                             *
      *----------------------------------------------------------------*
       0000-ESTRUTURA                     SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           PERFORM 1000-INICIALIZAR                                     
                                                                        
           PERFORM 2000-PROCESSAR                                       
                                                                        
           PERFORM 3000-FINALIZAR.                                      
                                                                        
      *----------------------------------------------------------------*
       0000-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *   PROCEDIMENTOS INICIAIS DO PROGRAMA                           *
      *----------------------------------------------------------------*
       1000-INICIALIZAR                   SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           INITIALIZE   FRWKGERR-REGISTRO                               
                        FRWKGCIC-REGISTRO                               
                        FRWKGDB2-REGISTRO                               
                        FRWKGMOD-REGISTRO                               
                        FRWKGHEA-REGISTRO.                              
                                                                        
           PERFORM 1300-CONSISTIR-DADOS.                                
                                                                        
           PERFORM 1200-TRATAR-PAGINACAO.                               
                                                                        
           MOVE ZEROS                     TO SCSBY132-COD-RETORNO       
           MOVE '0000'                    TO SCSBY132-COD-ERRO          
           MOVE 'SCSB0026'                TO SCSBY132-COD-MENSAGEM.     
                                                                        
      *----------------------------------------------------------------*
       1000-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *  FORMATA CHAVE DE PESQUISA PARA PAGINACAO                      *
      *----------------------------------------------------------------*
       1200-TRATAR-PAGINACAO              SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           MOVE 50                        TO WRK-QTDE-LIMITE            
                                                                        
           PERFORM 1210-TRATAR-CAMPOS                                   
                                                                        
           EVALUATE TRUE                                                
                                                                        
               WHEN SCSBY132-P-INICIAL                                  
                                                                        
                    MOVE ZEROS            TO DANO-LOTE-RECTA            
                                          OF SCSBB020                   
                    MOVE ZEROS            TO NLOTE-REG-COMPR            
                                          OF SCSBB020                   
                    MOVE ZEROS            TO DANO-COMPR-RECTA           
                                          OF SCSBB018                   
                    MOVE ZEROS            TO NREG-COMPR-RECTA           
                                          OF SCSBB018                   
                    MOVE ZEROS            TO NDETLH-LOTE-COMPR          
                                          OF SCSBB018                   
                                                                        
               WHEN SCSBY132-P-SEGUINTE                                 
                                                                        
                    MOVE SCSBY132-P-ULT-ANO-LOTE                        
                                          TO DANO-LOTE-RECTA            
                                          OF SCSBB020                   
                                             WRK-ANO-LOTE-ANT           
                    MOVE SCSBY132-P-ULT-CTPO-LOTE                       
                                          TO CTPO-LOTE-COMPR            
                                          OF SCSBB020                   
                                             WRK-CTPO-LOTE-ANT          
                    MOVE SCSBY132-P-ULT-NUMERO-LOT                      
                                          TO NLOTE-REG-COMPR            
                                          OF SCSBB020                   
                                             WRK-NUMERO-LOT-ANT         
                    MOVE SCSBY132-P-ULT-ANO-RAS                         
                                          TO DANO-COMPR-RECTA           
                                          OF SCSBB018                   
                                             WRK-ANO-RAS-ANT            
                    MOVE SCSBY132-P-ULT-NREG                            
                                          TO NREG-COMPR-RECTA           
                                          OF SCSBB018                   
                                             WRK-NREG-ANT               
                    MOVE SCSBY132-P-ULT-NSEQ                            
                                          TO NDETLH-LOTE-COMPR          
                                          OF SCSBB018                   
                                             WRK-NSEQ-ANT               
                                                                        
               WHEN OTHER                                               
                                                                        
                    MOVE 16                TO SCSBY132-COD-RETORNO      
                    MOVE '0001'            TO SCSBY132-COD-ERRO         
                    MOVE 'SCSB0040'        TO SCSBY132-COD-MENSAGEM     
                    PERFORM 3000-FINALIZAR                              
                                                                        
           END-EVALUATE.                                                
                                                                        
      *----------------------------------------------------------------*
       1200-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *     FORMATA CHV DE PESQ P/ PAG - CASO INFORMADO PELO USUARIO   *
      *----------------------------------------------------------------*
       1210-TRATAR-CAMPOS                 SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           IF  SCSBY132-E-ANO-LOTE        EQUAL ZEROS OR                
               SCSBY132-E-ANO-LOTE        NOT NUMERIC                   
               MOVE ZEROS                 TO WRK-ANO-LOTE-INI           
               MOVE 9999                  TO WRK-ANO-LOTE-FIM           
           ELSE                                                         
               MOVE SCSBY132-E-ANO-LOTE   TO WRK-ANO-LOTE-INI           
               MOVE SCSBY132-E-ANO-LOTE   TO WRK-ANO-LOTE-FIM           
           END-IF.                                                      
                                                                        
           IF  SCSBY132-E-NUMERO-LOTE     EQUAL ZEROS OR                
               SCSBY132-E-NUMERO-LOTE     NOT NUMERIC                   
               MOVE ZEROS                 TO WRK-NLOTE-REG-INI          
               MOVE 999999999             TO WRK-NLOTE-REG-FIM          
           ELSE                                                         
               MOVE SCSBY132-E-NUMERO-LOTE                              
                                          TO WRK-NLOTE-REG-INI          
               MOVE SCSBY132-E-NUMERO-LOTE                              
                                          TO WRK-NLOTE-REG-FIM          
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       1210-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *   CONSISTIR DADOS DE ENTRADA - FUNCIONAL                       *
      *----------------------------------------------------------------*
       1300-CONSISTIR-DADOS               SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           IF SCSBY132-IND-PAGINACAO      NOT EQUAL 'I' AND 'S'         
              MOVE 08                     TO SCSBY132-COD-RETORNO       
              MOVE '0002'                 TO SCSBY132-COD-ERRO          
              MOVE 'SCSB0040'             TO SCSBY132-COD-MENSAGEM      
              PERFORM 3000-FINALIZAR                                    
           END-IF.                                                      
                                                                        
           IF SCSBY132-E-EMPRESA          EQUAL SPACES                  
              MOVE 08                     TO SCSBY132-COD-RETORNO       
              MOVE '0003'                 TO SCSBY132-COD-ERRO          
              MOVE 'SCSB0001'             TO SCSBY132-COD-MENSAGEM      
              PERFORM 3000-FINALIZAR                                    
           END-IF.                                                      
                                                                        
           IF SCSBY132-E-DATA-INI         EQUAL SPACES                  
              MOVE 08                     TO SCSBY132-COD-RETORNO       
              MOVE '0004'                 TO SCSBY132-COD-ERRO          
              MOVE 'SCSB0041'             TO SCSBY132-COD-MENSAGEM      
              PERFORM 3000-FINALIZAR                                    
           END-IF.                                                      
                                                                        
           IF SCSBY132-E-DATA-FIM         EQUAL SPACES                  
              MOVE 08                     TO SCSBY132-COD-RETORNO       
              MOVE '0005'                 TO SCSBY132-COD-ERRO          
              MOVE 'SCSB0041'             TO SCSBY132-COD-MENSAGEM      
              PERFORM 3000-FINALIZAR                                    
           END-IF.                                                      
                                                                        
           IF SCSBY132-E-TP-REGISTRO      EQUAL ZEROS OR                
              SCSBY132-E-TP-REGISTRO      NOT NUMERIC                   
              MOVE 08                     TO SCSBY132-COD-RETORNO       
              MOVE '0006'                 TO SCSBY132-COD-ERRO          
              MOVE 'SCSB0051'             TO SCSBY132-COD-MENSAGEM      
              PERFORM 3000-FINALIZAR                                    
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       1300-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *   PROCESSAMENTO PRINCIPAL DO PROGRAMA                          *
      *----------------------------------------------------------------*
       2000-PROCESSAR                     SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           PERFORM 4200-ACESSAR-UORG1397.                               
                                                                        
           MOVE 'N'                       TO WRK-FIM-CSR                
                                             WRK-FIM-CSR2.              
           MOVE ZERO                      TO WRK-I.                     
                                                                        
           PERFORM 7000-OPEN-CSR01-SCSBB020-JN.                         
           PERFORM 7005-FETCH-CSR01-SCSBB020-JN.                        
                                                                        
           PERFORM UNTIL SQLCODE EQUAL +100 OR                          
                   WRK-I GREATER WRK-QTDE-LIMITE - 1 OR                 
                   WRK-II GREATER 50                                    
                                                                        
                   ADD 1                  TO WRK-II                     
                                                                        
                   IF  WRK-LOTE-ATU       NOT EQUAL WRK-LOTE-ANT        
                       PERFORM 2900-MONTAR-BL-SAIDA-RAS                 
                       MOVE WRK-LOTE-ATU  TO WRK-LOTE-ANT               
                       INITIALIZE            WRK-RAS-ANT                
                   END-IF                                               
                                                                        
                   IF  WRK-I          NOT GREATER WRK-QTDE-LIMITE - 1   
                                                                        
                       IF  WRK-RAS-ATU    NOT EQUAL WRK-RAS-ANT         
                           PERFORM 4000-ACESSAR-SCSB2056                
                           MOVE WRK-RAS-ATU                             
                                          TO WRK-RAS-ANT                
                       END-IF                                           
                                                                        
                       IF  WRK-I      NOT GREATER WRK-QTDE-LIMITE - 1   
                           MOVE NDETLH-LOTE-COMPR                       
                                          OF SCSBB018                   
                                          TO SCSBY132-P-ULT-NSEQ        
                                             WRK-NSEQ-ATU               
                                                                        
                           PERFORM 2200-OBTER-DADOS-OPER-PAG            
                       END-IF                                           
                   END-IF                                               
                                                                        
           END-PERFORM.                                                 
                                                                        
           IF  WRK-I                      GREATER 50                    
               MOVE 50                    TO SCSBY132-S-QTD-REG-RAS     
           ELSE                                                         
               MOVE WRK-I                 TO SCSBY132-S-QTD-REG-RAS     
           END-IF.                                                      
                                                                        
           PERFORM 7010-CLOSE-CSR01-SCSBB020-JN.                        
                                                                        
           IF  WRK-I                      EQUAL ZEROS                   
               MOVE 08                    TO SCSBY132-COD-RETORNO       
               IF  SCSBY132-IND-PAGINACAO EQUAL 'I'                     
                   MOVE '0007'            TO SCSBY132-COD-ERRO          
                   MOVE 'SCSB0029'        TO SCSBY132-COD-MENSAGEM      
               ELSE                                                     
                   MOVE '0008'            TO SCSBY132-COD-ERRO          
                   MOVE 'SCSB0030'        TO SCSBY132-COD-MENSAGEM      
               END-IF                                                   
               GO                         TO 2000-99-FIM                
           END-IF.                                                      
                                                                        
           IF WRK-FIM-CSR                 EQUAL 'S'                     
              MOVE 00                     TO SCSBY132-COD-RETORNO       
           ELSE                                                         
              MOVE 01                     TO SCSBY132-COD-RETORNO       
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       2000-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *   AJUSTE DE CHAVE PARA PAGINACAO                               *
      *----------------------------------------------------------------*
       2100-AJUSTAR-ULT-CHAVE             SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           MOVE WRK-ANO-LOTE-ANT          TO SCSBY132-P-ULT-ANO-LOTE.   
           MOVE WRK-CTPO-LOTE-ANT         TO SCSBY132-P-ULT-CTPO-LOTE.  
           MOVE WRK-NUMERO-LOT-ANT        TO SCSBY132-P-ULT-NUMERO-LOT. 
           MOVE WRK-ANO-RAS-ANT           TO SCSBY132-P-ULT-ANO-RAS.    
           MOVE WRK-NREG-ANT              TO SCSBY132-P-ULT-NREG.       
           MOVE WRK-NSEQ-ANT              TO SCSBY132-P-ULT-NSEQ.       
                                                                        
      *----------------------------------------------------------------*
       2100-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       2200-OBTER-DADOS-OPER-PAG          SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           IF  WRK-DANO-COMPR-OPER-NULL   EQUAL ZEROS AND               
               WRK-NREG-COMPR-OPER-NULL   EQUAL ZEROS AND               
               WRK-COPER-COMPR-EXTER-NULL EQUAL ZEROS                   
               MOVE DANO-COMPR-OPER       OF SCSBB018                   
                                          TO SCSBY057-E-ANO             
               MOVE NREG-COMPR-OPER       OF SCSBB018                   
                                          TO SCSBY057-E-NREG            
               MOVE COPER-COMPR-EXTER     OF SCSBB018                   
                                          TO SCSBY057-E-COPER           
               PERFORM 4100-ACESSAR-SCSB2057                            
               MOVE WRK-CHAVE-ATU         TO WRK-CHAVE-ANT              
               PERFORM 7005-FETCH-CSR01-SCSBB020-JN                     
           ELSE                                                         
               IF WRK-DANO-COMPR-PGTO-NULL EQUAL ZEROS AND              
                  WRK-NREG-COMPR-PGTO-NULL EQUAL ZEROS AND              
                  WRK-NPGTO-COMPR-EXTER-NULL                            
                                          EQUAL ZEROS                   
                  PERFORM 7100-TRATAR-PAGAMENTO                         
                  IF  WRK-FIM-CSR2        EQUAL 'S'                     
                      MOVE ZEROS          TO COPER-COMPR-EXTER          
                                          OF SCSBB012                   
                      MOVE 'N'            TO WRK-FIM-CSR2               
                      MOVE WRK-CHAVE-ATU  TO WRK-CHAVE-ANT              
                      PERFORM 7005-FETCH-CSR01-SCSBB020-JN              
                  ELSE                                                  
                      PERFORM 2100-AJUSTAR-ULT-CHAVE                    
                  END-IF                                                
               END-IF                                                   
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       2200-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *   FORMATA BL DE SAIDA(FUNCIONAL)                               *
      *----------------------------------------------------------------*
       2900-MONTAR-BL-SAIDA-RAS           SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           ADD  1                         TO WRK-I.                     
                                                                        
           MOVE 1                         TO SCSBY132-S-TPO-REG (WRK-I).
                                                                        
           MOVE DLOTE-RECTA-FEDRL         OF SCSBB020                   
                                          TO SCSBY132-S-DT-TRANSMISSAO  
                                                                (WRK-I).
           MOVE DANO-LOTE-RECTA           OF SCSBB020                   
                                          TO SCSBY132-S-ANO-LOTE(WRK-I) 
                                             SCSBY132-P-ULT-ANO-LOTE    
                                             WRK-ANO-LOTE-ATU.          
                                                                        
           MOVE CTPO-LOTE-COMPR           OF SCSBB020                   
                                          TO SCSBY132-S-CTPO-LOTE(WRK-I)
                                             SCSBY132-P-ULT-CTPO-LOTE   
                                             WRK-CTPO-LOTE-ATU.         
                                                                        
           MOVE NLOTE-REG-COMPR           OF SCSBB020                   
                                          TO SCSBY132-S-NUMERO-LOTE     
                                                                 (WRK-I)
                                             SCSBY132-P-ULT-NUMERO-LOT  
                                             WRK-NUMERO-LOT-ATU.        
                                                                        
           MOVE CPROT-LOTE-RECTA          OF SCSBB020                   
                                          TO SCSBY132-S-CPROT-LOTE      
                                                               (WRK-I). 
                                                                        
      *----------------------------------------------------------------*
       2900-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *   FORMATA BL DE SAIDA(FUNCIONAL)                               *
      *----------------------------------------------------------------*
       2910-MONTAR-BL-SAIDA-PAG           SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           ADD  1                         TO WRK-I.                     
                                                                        
           MOVE 4                         TO SCSBY132-S-TPO-REG (WRK-I).
                                                                        
           MOVE NPGTO-COMPR-EXTER         OF SCSBB012                   
                                          TO                            
                                            SCSBY132-S-NUM-PAGTO(WRK-I).
                                                                        
           MOVE DPGTO-COMPR-EXTER         OF SCSBB023                   
                                          TO SCSBY132-S-DT-PAGTO(WRK-I).
                                                                        
           MOVE COPER-COMPR-EXTER         OF SCSBB012                   
                                          TO SCSBY132-P-ULT-COPER.      
                                                                        
           PERFORM 2911-SELECT-SUM-B012.                                
                                                                        
           MOVE VPGTO-COMPR-EXTER         OF SCSBB012                   
                                          TO                            
                                            SCSBY132-S-VLR-PAGTO(WRK-I).
                                                                        
      *----------------------------------------------------------------*
       2910-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *  ACESSAR A TABELA SCSBB012 SUMARIZANDO O VLR DOS FATURAMENTOS  *
      *----------------------------------------------------------------*
       2911-SELECT-SUM-B012               SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           EXEC  SQL SELECT                                             
                 VALUE ( SUM(VPGTO_COMPR_EXTER), 0 )                    
                 INTO :SCSBB012.VPGTO-COMPR-EXTER                       
                 FROM DB2PRD.TPGTO_OPER_EXTER                           
                 WHERE (DANO_COMPR_RECTA  = :SCSBB012.DANO-COMPR-RECTA) 
                  AND  (NREG_COMPR_RECTA  = :SCSBB012.NREG-COMPR-RECTA) 
                  AND  (DANO_COMPR_PGTO   = :SCSBB012.DANO-COMPR-RECTA) 
                  AND  (NREG_COMPR_PGTO   = :SCSBB012.NREG-COMPR-RECTA) 
                  AND  (NPGTO_COMPR_EXTER = :SCSBB012.NPGTO-COMPR-EXTER)
           END-EXEC.                                                    
                                                                        
           IF (SQLCODE                    NOT EQUAL ZEROS) OR           
              (SQLWARN0                   EQUAL 'W')                    
               SET   DB2-SELECT           TO TRUE                       
               MOVE '08.1'                TO SCSBY132-COD-ERRO          
               MOVE 'TPGTO_OPER_EXTER'    TO WRK-NOME-TABELA            
               PERFORM 9996-ERRO-DB2                                    
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       2911-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       2920-MONTAR-BL-SAIDA-RAS2          SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           ADD  1                         TO WRK-I.                     
                                                                        
           MOVE 2                         TO SCSBY132-S-TPO-REG (WRK-I).
                                                                        
           MOVE DANO-COMPR-RECTA          OF SCSBB018                   
                                          TO SCSBY132-P-ULT-ANO-RAS     
                                             WRK-ANO-RAS-ATU.           
                                                                        
           MOVE NREG-COMPR-RECTA          OF SCSBB018                   
                                          TO SCSBY132-P-ULT-NREG        
                                             WRK-NREG-ATU.              
                                                                        
           MOVE DANO-COMPR-RECTA          OF SCSBB018                   
                                          TO SCSBY132-S-ANO     (WRK-I).
           MOVE NREG-COMPR-RECTA          OF SCSBB018                   
                                          TO SCSBY132-S-NREG    (WRK-I).
           MOVE SCSBY056-S-NOME-VEND      TO                            
                                          SCSBY132-S-NOME-VEND  (WRK-I).
           MOVE SCSBY056-S-CPF-CNPJ-PRINC TO                            
                                       SCSBY132-S-CPF-CNPJ-PRINC(WRK-I).
           MOVE SCSBY056-S-CPF-CNPJ-FLIAL TO                            
                                       SCSBY132-S-CPF-CNPJ-FLIAL(WRK-I).
           MOVE SCSBY056-S-CPF-CNPJ-CTRL  TO                            
                                        SCSBY132-S-CPF-CNPJ-CTRL(WRK-I).
           MOVE SCSBY056-S-NIF            TO SCSBY132-S-NIF     (WRK-I).
           MOVE SCSBY056-S-CD-MOEDA       TO SCSBY132-S-CD-MOEDA(WRK-I).
           MOVE SCSBY056-S-DS-MOEDA       TO SCSBY132-S-DS-MOEDA(WRK-I).
                                                                        
      *----------------------------------------------------------------*
       2920-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *   PROCEDIMENTOS FINAIS DO PROGRAMA                             *
      *----------------------------------------------------------------*
       3000-FINALIZAR                     SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           EXEC  CICS  RETURN  END-EXEC.                                
                                                                        
      *----------------------------------------------------------------*
       3000-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *   ACESSO AO MODULO                                             *
      *----------------------------------------------------------------*
       4000-ACESSAR-SCSB2056              SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           MOVE '4000-ACESSAR-SCSB2056'   TO FRWKGHEA-IDEN-PARAGRAFO    
                                                                        
           INITIALIZE  SCSBY056-BLOCO-RETORNO                           
                       SCSBY056-REGISTRO.                               
                                                                        
           MOVE  DANO-COMPR-RECTA         OF SCSBB018                   
                                          TO SCSBY056-E-ANO.            
           MOVE  NREG-COMPR-RECTA         OF SCSBB018                   
                                          TO SCSBY056-E-NREG.           
                                                                        
           EXEC CICS LINK                                               
                PROGRAM   (WRK-SCSB2056)                                
                COMMAREA  (WRK-AREA-SCSB2056)                           
                LENGTH    (LENGTH OF WRK-AREA-SCSB2056)                 
                NOHANDLE                                                
           END-EXEC.                                                    
                                                                        
           IF EIBRESP                     NOT EQUAL DFHRESP(NORMAL)     
              MOVE '0009'                 TO SCSBY132-COD-ERRO          
              PERFORM 9997-ERRO-CICS                                    
           END-IF                                                       
                                                                        
           MOVE SCSBY056-BLOCO-RETORNO    TO WRK-BLOCO-RETORNO          
                                             SCSBY132-BLOCO-RETORNO.    
                                                                        
           IF SCSBY056-COD-RETORNO        NOT EQUAL ZEROS AND 08        
              MOVE '0010'                 TO SCSBY132-COD-ERRO          
              MOVE WRK-SCSB2056           TO WRK-MODULO                 
              PERFORM 9998-ERRO-MODULO                                  
           END-IF.                                                      
                                                                        
           IF SCSBY056-COD-RETORNO        EQUAL 08                      
              MOVE '0011'                 TO SCSBY132-COD-ERRO          
              PERFORM 3000-FINALIZAR                                    
           END-IF.                                                      
                                                                        
           PERFORM 2920-MONTAR-BL-SAIDA-RAS2.                           
                                                                        
      *----------------------------------------------------------------*
       4000-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       4100-ACESSAR-SCSB2057              SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           MOVE '4100-ACESSAR-SCSB2057'   TO FRWKGHEA-IDEN-PARAGRAFO    
                                                                        
           INITIALIZE  SCSBY057-BLOCO-RETORNO.                          
                                                                        
           EXEC CICS LINK                                               
                PROGRAM   (WRK-SCSB2057)                                
                COMMAREA  (WRK-AREA-SCSB2057)                           
                LENGTH    (LENGTH OF WRK-AREA-SCSB2057)                 
                NOHANDLE                                                
           END-EXEC.                                                    
                                                                        
           IF EIBRESP                     NOT EQUAL DFHRESP(NORMAL)     
              MOVE '0012'                 TO SCSBY132-COD-ERRO          
              PERFORM 9997-ERRO-CICS                                    
           END-IF.                                                      
                                                                        
           MOVE SCSBY057-BLOCO-RETORNO    TO WRK-BLOCO-RETORNO          
                                             SCSBY132-BLOCO-RETORNO.    
                                                                        
           IF SCSBY057-COD-RETORNO        NOT EQUAL ZEROS AND 08        
              MOVE '0013'                 TO SCSBY132-COD-ERRO          
              MOVE WRK-SCSB2057           TO WRK-MODULO                 
              PERFORM 9998-ERRO-MODULO                                  
           END-IF.                                                      
                                                                        
           IF SCSBY057-COD-RETORNO        EQUAL 08                      
              MOVE '0014'                 TO SCSBY132-COD-ERRO          
              PERFORM 3000-FINALIZAR                                    
           END-IF.                                                      
                                                                        
           ADD  1                         TO WRK-I.                     
                                                                        
           MOVE 3                         TO SCSBY132-S-TPO-REG(WRK-I). 
           MOVE SCSBY057-S-CD-NBS(1)      TO SCSBY132-S-CD-NBS (WRK-I). 
           MOVE SCSBY057-S-DS-NBS(1)      TO SCSBY132-S-DS-NBS (WRK-I). 
           MOVE SCSBY057-S-DT-INICIO(1)   TO                            
                                           SCSBY132-S-DT-INICIO(WRK-I). 
           MOVE SCSBY057-S-DT-CONCLUSAO(1)                              
                                          TO                            
                                        SCSBY132-S-DT-CONCLUSAO(WRK-I). 
           MOVE SCSBY057-S-VALOR(1)       TO SCSBY132-S-VALOR  (WRK-I). 
           MOVE COPER-COMPR-EXTER         OF SCSBB018                   
                                          TO SCSBY132-S-COPER  (WRK-I). 
                                                                        
      *----------------------------------------------------------------*
       4100-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *     ACESSAR O UORG PARA CONVERTER A EMPRESA                    *
      *----------------------------------------------------------------*
       4200-ACESSAR-UORG1397              SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           MOVE    'D'                    TO  UORGW397-E-AMBIENTE.      
           MOVE    SCSBY132-E-EMPRESA     TO  UORGW397-E-CEMPR-SAP.     
           MOVE    ZEROS                  TO  UORGW397-E-CPSSOA-JURID.  
                                                                        
           EXEC CICS LINK                                               
                PROGRAM (WRK-UORG1397)                                  
                COMMAREA(WRK-AREA-UORG1397)                             
                LENGTH  (LENGTH OF WRK-AREA-UORG1397)                   
                NOHANDLE                                                
           END-EXEC.                                                    
                                                                        
           IF  EIBRESP                    NOT EQUAL DFHRESP(NORMAL)     
               MOVE '0015'                TO  SCSBY132-COD-ERRO         
               PERFORM 9997-ERRO-CICS                                   
           END-IF.                                                      
                                                                        
           IF  UORGW000-COD-RETORNO       NOT EQUAL ZEROS AND 08        
               MOVE UORGW000-BLOCO-RETORNO                              
                                          TO  WRK-BLOCO-RETORNO         
               MOVE WRK-UORG1397          TO  WRK-MODULO                
               PERFORM 9998-ERRO-MODULO                                 
           END-IF.                                                      
                                                                        
           IF  UORGW000-COD-RETORNO       EQUAL 08                      
               MOVE 9999999999            TO CPSSOA-JURID               
                                          OF SCSBB013                   
           ELSE                                                         
               MOVE UORGW397-S-CPSSOA-JURID                             
                                          TO CPSSOA-JURID               
                                          OF SCSBB013                   
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       4200-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       7000-OPEN-CSR01-SCSBB020-JN        SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           MOVE '7000-OPEN-CSR01-SCSBB020-JN'                           
                                          TO FRWKGHEA-IDEN-PARAGRAFO.   
                                                                        
           MOVE SCSBY132-E-DATA-INI       TO WRK-DATA-INI.              
           MOVE SCSBY132-E-DATA-FIM       TO WRK-DATA-FIM.              
                                                                        
           MOVE SCSBY132-E-TP-REGISTRO    TO CTPO-LOTE-COMPR            
                                          OF SCSBB020.                  
                                                                        
           EXEC SQL                                                     
                OPEN CSR01-SCSBB020-JN                                  
           END-EXEC.                                                    
                                                                        
           IF  (SQLCODE                   NOT EQUAL ZEROS) OR           
               (SQLWARN0                  EQUAL 'W')                    
                SET   DB2-OPEN            TO TRUE                       
                MOVE '0016'               TO SCSBY132-COD-ERRO          
                MOVE 'SCSBB020'           TO WRK-STORED-PROC            
                MOVE 'TLOTE_REG_COMPR'    TO WRK-NOME-TABELA            
                PERFORM 9996-ERRO-DB2                                   
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       7000-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       7005-FETCH-CSR01-SCSBB020-JN       SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           MOVE '7005-FETCH-CSR01-SCSBB020-JN'                          
                                          TO FRWKGHEA-IDEN-PARAGRAFO.   
                                                                        
           EXEC SQL FETCH CSR01-SCSBB020-JN INTO                        
                  :SCSBB020.DLOTE-RECTA-FEDRL                           
                 ,:SCSBB020.DANO-LOTE-RECTA                             
                 ,:SCSBB020.CTPO-LOTE-COMPR                             
                 ,:SCSBB020.NLOTE-REG-COMPR                             
                 ,:SCSBB020.CPROT-LOTE-RECTA                            
                 ,:SCSBB018.DANO-COMPR-RECTA                            
                 ,:SCSBB018.NREG-COMPR-RECTA                            
                 ,:SCSBB018.NDETLH-LOTE-COMPR                           
                 ,:SCSBB018.DANO-COMPR-OPER                             
                            :WRK-DANO-COMPR-OPER-NULL                   
                 ,:SCSBB018.NREG-COMPR-OPER                             
                            :WRK-NREG-COMPR-OPER-NULL                   
                 ,:SCSBB018.COPER-COMPR-EXTER                           
                            :WRK-COPER-COMPR-EXTER-NULL                 
                 ,:SCSBB018.DANO-COMPR-PGTO                             
                            :WRK-DANO-COMPR-PGTO-NULL                   
                 ,:SCSBB018.NREG-COMPR-PGTO                             
                            :WRK-NREG-COMPR-PGTO-NULL                   
                 ,:SCSBB018.NPGTO-COMPR-EXTER                           
                            :WRK-NPGTO-COMPR-EXTER-NULL                 
           END-EXEC.                                                    
                                                                        
           IF  (SQLCODE                   NOT EQUAL ZEROS AND +100) OR  
               (SQLWARN0                  EQUAL 'W')                    
                SET   DB2-FETCH           TO TRUE                       
                MOVE '0017'               TO SCSBY132-COD-ERRO          
                MOVE 'SCSBB020'           TO WRK-STORED-PROC            
                MOVE 'TLOTE_REG_COMPR'    TO WRK-NOME-TABELA            
                PERFORM 9996-ERRO-DB2                                   
           END-IF.                                                      
                                                                        
           IF  SQLCODE                    EQUAL +100                    
               MOVE 'S'                   TO WRK-FIM-CSR                
               GO TO 7005-99-FIM                                        
           END-IF.                                                      
                                                                        
      *----> CHAVE LOTE                                                 
           MOVE DANO-LOTE-RECTA           OF SCSBB020                   
                                          TO WRK-CHV-DANO-LOTE-ATU.     
           MOVE CTPO-LOTE-COMPR           OF SCSBB020                   
                                          TO WRK-CHV-CTPO-LOTE-ATU.     
           MOVE NLOTE-REG-COMPR           OF SCSBB020                   
                                          TO WRK-NLOTE-ATU.             
                                                                        
      *----> CHAVE RAS                                                  
           MOVE DANO-COMPR-RECTA          OF SCSBB018                   
                                          TO WRK-CHV-DANO-COM-ATU.      
           MOVE NREG-COMPR-RECTA          OF SCSBB018                   
                                          TO WRK-CHV-NREG-COM-ATU.      
                                                                        
      *----> CHAVE PAGAMENTO                                            
           MOVE DANO-COMPR-PGTO           OF SCSBB018                   
                                          TO WRK-CHV-DANO-PAGTO-ATU.    
           MOVE NREG-COMPR-PGTO           OF SCSBB018                   
                                          TO WRK-CHV-NREG-PAGTO-ATU.    
           MOVE NPGTO-COMPR-EXTER         OF SCSBB018                   
                                          TO WRK-CHV-NPAGTO-ATU.        
                                                                        
      *----------------------------------------------------------------*
       7005-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       7010-CLOSE-CSR01-SCSBB020-JN       SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           MOVE '7010-CLOSE-CSR01-SCSBB020-JN'                          
                                          TO FRWKGHEA-IDEN-PARAGRAFO    
                                                                        
           EXEC SQL                                                     
                CLOSE CSR01-SCSBB020-JN                                 
           END-EXEC.                                                    
                                                                        
           IF  (SQLCODE                   NOT EQUAL ZEROS) OR           
               (SQLWARN0                  EQUAL 'W')                    
                SET   DB2-CLOSE           TO TRUE                       
                MOVE '0018'               TO SCSBY132-COD-ERRO          
                MOVE 'SCSBB020'           TO WRK-STORED-PROC            
                MOVE 'TLOTE_REG_COMPR'    TO WRK-NOME-TABELA            
                PERFORM 9996-ERRO-DB2                                   
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       7010-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *       OBTER AS OPERACOES DE PAGAMENTO                          *
      *----------------------------------------------------------------*
       7100-TRATAR-PAGAMENTO              SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           PERFORM 7105-TRATAR-PAGINACAO.                               
                                                                        
           PERFORM 7110-OPEN-CSR02-SCSBB012-JN.                         
                                                                        
           PERFORM 7120-FETCH-CSR02-SCSBB012-JN.                        
                                                                        
           PERFORM UNTIL SQLCODE EQUAL +100 OR                          
                   WRK-I GREATER WRK-QTDE-LIMITE                        
                                                                        
                   IF  WRK-PAGTO-ATU      NOT EQUAL WRK-PAGTO-ANT       
                       PERFORM 2910-MONTAR-BL-SAIDA-PAG                 
                       MOVE WRK-PAGTO-ATU TO WRK-PAGTO-ANT              
                   END-IF                                               
                                                                        
                   MOVE DANO-COMPR-RECTA  OF SCSBB012                   
                                          TO SCSBY057-E-ANO             
                   MOVE NREG-COMPR-RECTA  OF SCSBB012                   
                                          TO SCSBY057-E-NREG            
                   MOVE COPER-COMPR-EXTER OF SCSBB012                   
                                          TO SCSBY057-E-COPER           
                                                                        
                   PERFORM 4100-ACESSAR-SCSB2057                        
                                                                        
                   PERFORM 7120-FETCH-CSR02-SCSBB012-JN                 
                                                                        
           END-PERFORM.                                                 
                                                                        
           PERFORM 7130-CLOSE-CSR02-SCSBB012-JN.                        
                                                                        
      *----------------------------------------------------------------*
       7100-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *  FORMATA CHAVE DE PESQUISA PARA PAGINACAO TABELA 05            *
      *----------------------------------------------------------------*
       7105-TRATAR-PAGINACAO              SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           EVALUATE TRUE                                                
                                                                        
               WHEN SCSBY132-P-INICIAL                                  
                                                                        
                    MOVE ZEROS            TO COPER-COMPR-EXTER          
                                          OF SCSBB012                   
                                                                        
               WHEN SCSBY132-P-SEGUINTE                                 
                                                                        
                    MOVE SCSBY132-P-ULT-COPER                           
                                          TO COPER-COMPR-EXTER          
                                          OF SCSBB012                   
                                                                        
               WHEN OTHER                                               
                                                                        
                    MOVE 16                TO SCSBY132-COD-RETORNO      
                    MOVE '0019'            TO SCSBY132-COD-ERRO         
                    MOVE 'SCSB0040'        TO SCSBY132-COD-MENSAGEM     
                    PERFORM 3000-FINALIZAR                              
                                                                        
           END-EVALUATE.                                                
                                                                        
      *----------------------------------------------------------------*
       7105-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       7110-OPEN-CSR02-SCSBB012-JN        SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           MOVE '7110-OPEN-CSR02-SCSBB012-JN'                           
                                          TO FRWKGHEA-IDEN-PARAGRAFO.   
                                                                        
           MOVE DANO-COMPR-PGTO           OF SCSBB018                   
                                          TO DANO-COMPR-RECTA           
                                          OF SCSBB012.                  
                                                                        
           MOVE NREG-COMPR-PGTO           OF SCSBB018                   
                                          TO NREG-COMPR-RECTA           
                                          OF SCSBB012.                  
                                                                        
           MOVE NPGTO-COMPR-EXTER         OF SCSBB018                   
                                          TO NPGTO-COMPR-EXTER          
                                          OF SCSBB012.                  
                                                                        
           EXEC SQL                                                     
                OPEN CSR02-SCSBB012-JN                                  
           END-EXEC.                                                    
                                                                        
           IF  (SQLCODE                   NOT EQUAL ZEROS) OR           
               (SQLWARN0                  EQUAL 'W')                    
                SET   DB2-OPEN            TO TRUE                       
                MOVE '0020'               TO SCSBY132-COD-ERRO          
                MOVE 'SCSBB012'           TO WRK-STORED-PROC            
                MOVE 'TPGTO_OPER_EXTER'   TO WRK-NOME-TABELA            
                PERFORM 9996-ERRO-DB2                                   
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       7110-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       7120-FETCH-CSR02-SCSBB012-JN       SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           MOVE '7120-FETCH-CSR02-SCSBB012-JN'                          
                                          TO FRWKGHEA-IDEN-PARAGRAFO.   
                                                                        
           EXEC SQL FETCH CSR02-SCSBB012-JN INTO                        
                  :SCSBB012.COPER-COMPR-EXTER                           
                 ,:SCSBB023.DPGTO-COMPR-EXTER                           
           END-EXEC.                                                    
                                                                        
           IF  (SQLCODE                   NOT EQUAL ZEROS AND +100) OR  
               (SQLWARN0                  EQUAL 'W')                    
                SET   DB2-FETCH           TO TRUE                       
                MOVE '0021'               TO SCSBY132-COD-ERRO          
                MOVE 'SCSBB012'           TO WRK-STORED-PROC            
                MOVE 'TPGTO_OPER_EXTER'   TO WRK-NOME-TABELA            
                PERFORM 9996-ERRO-DB2                                   
           END-IF.                                                      
                                                                        
           IF  SQLCODE                    EQUAL +100                    
               MOVE 'S'                   TO WRK-FIM-CSR2               
           END-IF.                                                      
                                                                        
           MOVE COPER-COMPR-EXTER         OF SCSBB012                   
                                          TO COPER-COMPR-EXTER          
                                          OF SCSBB018.                  
                                                                        
      *----------------------------------------------------------------*
       7120-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       7130-CLOSE-CSR02-SCSBB012-JN       SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           MOVE '7130-CLOSE-CSR02-SCSBB012-JN'                          
                                          TO FRWKGHEA-IDEN-PARAGRAFO    
                                                                        
           EXEC SQL                                                     
                CLOSE CSR02-SCSBB012-JN                                 
           END-EXEC.                                                    
                                                                        
           IF  (SQLCODE                   NOT EQUAL ZEROS) OR           
               (SQLWARN0                  EQUAL 'W')                    
                SET   DB2-CLOSE           TO TRUE                       
                MOVE '0022'               TO SCSBY132-COD-ERRO          
                MOVE 'SCSBB012'           TO WRK-STORED-PROC            
                MOVE 'TPGTO_OPER_EXTER'   TO WRK-NOME-TABELA            
                PERFORM 9996-ERRO-DB2                                   
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       7130-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * TRATAMENTO DE ERRO DB2.                                        *
      *----------------------------------------------------------------*
       9996-ERRO-DB2                      SECTION.                      
      *----------------------------------------------------------------*
                                                                        
      *--> CARREGAR DADOS NA AREA DE COMUNICACAO                        
                                                                        
           MOVE  16                       TO  SCSBY132-COD-RETORNO      
           MOVE  'SCSB0032'               TO  SCSBY132-COD-MENSAGEM     
                                                                        
      *--> CARREGAR DADOS NA AREA COMUM DE ERROS (HEADER) - I#FRWKHE    
                                                                        
           SET   ERRO-DB2                 TO  TRUE                      
           MOVE  WRK-PROGRAMA             TO  FRWKGHEA-NOME-PROGRAMA    
           MOVE  FRWKGDB2-TAM-LAYOUT      TO  FRWKGHEA-TAM-DADOS        
                                                                        
      *--> CARREGAR DADOS NA AREA DE ERRO DB2 - I#FRWKDB                
                                                                        
           MOVE  WRK-NOME-TABELA          TO  FRWKGDB2-NOME-TABELA      
           MOVE  WRK-STORED-PROC          TO  FRWKGDB2-STORED-PROC      
           MOVE  FRWKGHEA-IDEN-PARAGRAFO(1:16)                          
                                          TO  FRWKGDB2-LOCAL            
           MOVE  SQLSTATE                 TO  FRWKGDB2-SQLSTATE         
           MOVE  WRK-SQLCODE              TO  FRWKGDB2-SQLCODE          
                                                                        
      *--> MOVER AREA DE ERRO DB2 P/ BLOCO DE INFORM ESPECIF DO ERRO    
                                                                        
           MOVE  WRK-AREA-ERRO-DB2        TO  WRK-BLOCO-INFO-ERRO       
                                                                        
           PERFORM 9999-API-ERROS.                                      
                                                                        
      *----------------------------------------------------------------*
       9996-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * TRATAMENTO DE ERRO - CICS                                      *
      *----------------------------------------------------------------*
       9997-ERRO-CICS                     SECTION.                      
      *----------------------------------------------------------------*
                                                                        
      *--> CARREGAR DADOS NA AREA DE RETORNO P/ O FRAMEWORK - I#FRWKAQ  
                                                                        
           MOVE  16                       TO  SCSBY132-COD-RETORNO      
           MOVE  'SCSB0033'               TO  SCSBY132-COD-MENSAGEM     
                                                                        
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
       9997-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * TRATAMENTO DE ERRO DO MODULO.                                  *
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
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       9998-99-FIM.                       EXIT.                         
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
      *----------------------------------------------------------------*
      * CHAMA O MODULO FRWK1999 - GRAVA LOG DE ERROS                   *
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

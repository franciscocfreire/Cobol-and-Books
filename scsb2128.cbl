      *================================================================*
       IDENTIFICATION DIVISION.                                         
      *================================================================*
      *                                                                *
       PROGRAM-ID.    SCSB2128.                                         
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
      *      GERAR INFORMACOES PARA O RELATORIO DAS RVS'S TRANSMITIDAS *
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
      * SCSBY128 BLOCO DE ENTRADA E SAIDA - MODULO FUNCIONAL           *
      *                                                                *
      *----------------------------------------------------------------*
      *                       A L T E R A C A O                        *
      *----------------------------------------------------------------*
      *                                                                *
      *  PROGRAMADOR:  ANDERSON MARTINS - BRQ                          *
      *  ANALISTA...:  FERNANDA CARUSO  - BRQ                          *
      *  DATA.......:  28/10/2013                                      *
      *  OBJETIVO...:  INCLUSAO DO CAMPO NUMERO DE PROTOCOLO NA SAIDA  *
      *                'SCSBY128-S-CPROT-LOTE'.                        *
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
                                                                        
       77  WRK-PROGRAMA            PIC  X(008) VALUE 'SCSB2128'.        
       77  WRK-SCSB2032            PIC  X(008) VALUE 'SCSB2032'.        
       77  WRK-SCSB2033            PIC  X(008) VALUE 'SCSB2033'.        
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
                                                                        
      ***----------------- RVS ATUAL                                    
       01  WRK-RVS-ATU.                                                 
           05  WRK-CHV-DANO-VDA-ATU  PIC  9(004) COMP-3 VALUE ZEROS.    
           05  WRK-CHV-NREG-VDA-ATU  PIC  9(009) COMP-3 VALUE ZEROS.    
                                                                        
      ***----------------- FATURAMENTO ATUAL                            
       01  WRK-FATMT-ATU.                                               
           05 WRK-CHV-DANO-FATMT-ATU PIC  9(004) COMP-3 VALUE ZEROS.    
           05 WRK-CHV-NREG-FATMT-ATU PIC  9(009) COMP-3 VALUE ZEROS.    
           05 WRK-CHV-NFATMT-ATU     PIC  9(010) COMP-3 VALUE ZEROS.    
                                                                        
      ***----------------- LOTE ANTERIOR                                
       01  WRK-LOTE-ANT.                                                
           05  WRK-CHV-DANO-LOTE-ANT PIC  9(004) COMP-3 VALUE ZEROS.    
           05  WRK-CHV-CTPO-LOTE-ANT PIC  9(001) COMP-3 VALUE ZEROS.    
           05  WRK-NLOTE-ANT         PIC  9(009) COMP-3 VALUE ZEROS.    
                                                                        
      ***----------------- RVS ANTERIOR                                 
       01  WRK-RVS-ANT.                                                 
           05  WRK-CHV-DANO-VDA-ANT  PIC  9(004) COMP-3 VALUE ZEROS.    
           05  WRK-CHV-NREG-VDA-ANT  PIC  9(009) COMP-3 VALUE ZEROS.    
                                                                        
      ***----------------- FATURAMENTO ANTERIOR                         
       01  WRK-FATMT-ANT.                                               
           05 WRK-CHV-DANO-FATMT-ANT PIC  9(004) COMP-3 VALUE ZEROS.    
           05 WRK-CHV-NREG-FATMT-ANT PIC  9(009) COMP-3 VALUE ZEROS.    
           05 WRK-CHV-NFATMT-ANT     PIC  9(010) COMP-3 VALUE ZEROS.    
                                                                        
      *---> VARIAVEIS DE NULIDADE                                       
                                                                        
       01  WRK-DANO-VDA-OPER-NULL    PIC S9(004) COMP   VALUE ZEROS.    
       01  WRK-NREG-VDA-OPER-NULL    PIC S9(004) COMP   VALUE ZEROS.    
       01  WRK-COPER-VDA-EXTER-NULL  PIC S9(004) COMP   VALUE ZEROS.    
       01  WRK-DANO-VDA-FATMT-NULL   PIC S9(004) COMP   VALUE ZEROS.    
       01  WRK-NREG-VDA-FATMT-NULL   PIC S9(004) COMP   VALUE ZEROS.    
       01  WRK-NFATMT-VDA-EXTER-NULL PIC S9(004) COMP   VALUE ZEROS.    
                                                                        
      *---> CHAVE ANTERIOR DE CONTROLE INTERNO                          
       01  WRK-CHAVE-ANT.                                               
           05  WRK-ANO-LOTE-ANT    PIC  9(004)        VALUE ZEROS.      
           05  WRK-CTPO-LOTE-ANT   PIC  9(001)        VALUE ZEROS.      
           05  WRK-NUMERO-LOT-ANT  PIC  9(009)        VALUE ZEROS.      
           05  WRK-ANO-RVS-ANT     PIC  9(004)        VALUE ZEROS.      
           05  WRK-NREG-ANT        PIC  9(009)        VALUE ZEROS.      
           05  WRK-NSEQ-ANT        PIC  9(009)        VALUE ZEROS.      
                                                                        
      *---> CHAVE ATUAL DE CONTROLE INTERNO                             
       01  WRK-CHAVE-ATU.                                               
           05  WRK-ANO-LOTE-ATU    PIC  9(004)        VALUE ZEROS.      
           05  WRK-CTPO-LOTE-ATU   PIC  9(001)        VALUE ZEROS.      
           05  WRK-NUMERO-LOT-ATU  PIC  9(009)        VALUE ZEROS.      
           05  WRK-ANO-RVS-ATU     PIC  9(004)        VALUE ZEROS.      
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
           'AREA MODULO SCSB2032'.                                      
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
       01  WRK-AREA-SCSB2032.                                           
           COPY SCSBY032.                                               
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                  PIC  X(080) VALUE                    
           'AREA MODULO SCSB2033'.                                      
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
       01  WRK-AREA-SCSB2033.                                           
           COPY SCSBY033.                                               
                                                                        
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
                                                                        
      *----> REGISTROS DE FATURAMENTOS                                  
           EXEC SQL                                                     
                INCLUDE SCSBB005                                        
           END-EXEC.                                                    
                                                                        
      *----> RVS                                                        
           EXEC SQL                                                     
                INCLUDE SCSBB016                                        
           END-EXEC.                                                    
                                                                        
      *----> DETALHE DO LOTE                                            
           EXEC SQL                                                     
                INCLUDE SCSBB019                                        
           END-EXEC.                                                    
                                                                        
      *----> LOTE                                                       
           EXEC SQL                                                     
                INCLUDE SCSBB021                                        
           END-EXEC.                                                    
                                                                        
      *----> FATURAMENTO                                                
           EXEC SQL                                                     
                INCLUDE SCSBB022                                        
           END-EXEC.                                                    
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
      *   DECLARACAO DO CURSOR - CSR01 - SCSBB016 / 19 / 21             
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
           EXEC SQL DECLARE CSR01-SCSBB021-JN CURSOR FOR SELECT         
                    A.DLOTE_RECTA_FEDRL                                 
                   ,A.DANO_LOTE_RECTA                                   
                   ,A.CTPO_LOTE_VDA                                     
                   ,A.NLOTE_REG_VDA                                     
                   ,VALUE (A.CPROT_LOTE_RECTA, ' ')                     
                   ,B.DANO_VDA_RECTA                                    
                   ,B.NREG_VDA_RECTA                                    
                   ,B.NDETLH_LOTE_VDA                                   
                   ,B.DANO_VDA_OPER                                     
                   ,B.NREG_VDA_OPER                                     
                   ,B.COPER_VDA_EXTER                                   
                   ,B.DANO_VDA_FATMT                                    
                   ,B.NREG_VDA_FATMT                                    
                   ,B.NFATMT_VDA_EXTER                                  
               FROM DB2PRD.TLOTE_REG_VDA   A,                           
                    DB2PRD.TDETLH_LOTE_VDA B,                           
                    DB2PRD.TREG_VDA_RECTA  C                            
              WHERE  (A.DANO_LOTE_RECTA   = B.DANO_LOTE_RECTA           
                AND   A.CTPO_LOTE_VDA     = B.CTPO_LOTE_VDA             
                AND   A.NLOTE_REG_VDA     = B.NLOTE_REG_VDA             
                AND   B.DANO_VDA_RECTA    = C.DANO_VDA_RECTA            
                AND   B.NREG_VDA_RECTA    = C.NREG_VDA_RECTA            
                AND  (A.CPROT_LOTE_RECTA  BETWEEN :WRK-CPROT-LOTE-INI   
                AND                               :WRK-CPROT-LOTE-FIM)) 
                AND  (A.DLOTE_RECTA_FEDRL BETWEEN :WRK-DATA-INI         
                AND                               :WRK-DATA-FIM)        
                AND  (C.CPSSOA_JURID      = :SCSBB016.CPSSOA-JURID)     
                AND  (A.CTPO_LOTE_VDA     = :SCSBB021.CTPO-LOTE-VDA)    
                AND  (A.DANO_LOTE_RECTA   BETWEEN :WRK-ANO-LOTE-INI     
                AND                               :WRK-ANO-LOTE-FIM)    
                AND  (A.NLOTE_REG_VDA     BETWEEN :WRK-NLOTE-REG-INI    
                AND                               :WRK-NLOTE-REG-FIM)   
                AND ((A.DANO_LOTE_RECTA   > :SCSBB021.DANO-LOTE-RECTA)  
                 OR  (A.DANO_LOTE_RECTA   = :SCSBB021.DANO-LOTE-RECTA   
                AND   A.NLOTE_REG_VDA     > :SCSBB021.NLOTE-REG-VDA)    
                 OR  (A.DANO_LOTE_RECTA   = :SCSBB021.DANO-LOTE-RECTA   
                AND   A.NLOTE_REG_VDA     = :SCSBB021.NLOTE-REG-VDA)    
                AND  (B.DANO_VDA_RECTA    > :SCSBB019.DANO-VDA-RECTA)   
                 OR  (B.DANO_VDA_RECTA    = :SCSBB019.DANO-VDA-RECTA    
                AND   B.NREG_VDA_RECTA    > :SCSBB019.NREG-VDA-RECTA)   
                 OR  (B.DANO_VDA_RECTA    = :SCSBB019.DANO-VDA-RECTA    
                AND   B.NREG_VDA_RECTA    = :SCSBB019.NREG-VDA-RECTA    
                AND   B.NDETLH_LOTE_VDA   > :SCSBB019.NDETLH-LOTE-VDA)) 
            ORDER BY  DANO_LOTE_RECTA                                   
                     ,NLOTE_REG_VDA                                     
                     ,DANO_VDA_RECTA                                    
                     ,NREG_VDA_RECTA                                    
                     ,NDETLH_LOTE_VDA                                   
           END-EXEC.                                                    
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
      *   DECLARACAO DO CURSOR - CSR02 - SCSBB005 / 22                  
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
           EXEC SQL DECLARE CSR02-SCSBB005-JN CURSOR FOR SELECT         
                    A.COPER_VDA_EXTER                                   
                   ,B.DFATMT_VDA_EXTER                                  
               FROM DB2PRD.TFATMT_OPER_VDA  A,                          
                    DB2PRD.TFATMT_VDA_EXTER B                           
              WHERE  (A.DANO_VDA_RECTA    = B.DANO_VDA_RECTA            
                AND   A.NREG_VDA_RECTA    = B.NREG_VDA_RECTA            
                AND   A.NFATMT_VDA_EXTER  = B.NFATMT_VDA_EXTER)         
                AND  (A.DANO_VDA_RECTA    = :SCSBB005.DANO-VDA-RECTA    
                AND   A.NREG_VDA_RECTA    = :SCSBB005.NREG-VDA-RECTA    
                AND   A.DANO_VDA_FATMT    = :SCSBB005.DANO-VDA-RECTA    
                AND   A.NREG_VDA_FATMT    = :SCSBB005.NREG-VDA-RECTA    
                AND   A.NFATMT_VDA_EXTER  = :SCSBB005.NFATMT-VDA-EXTER) 
                AND  (A.COPER_VDA_EXTER   > :SCSBB005.COPER-VDA-EXTER)  
            ORDER BY A.COPER_VDA_EXTER                                  
           END-EXEC.                                                    
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                  PIC  X(080) VALUE                    
           'FINAL DA WORKING STORAGE'.                                  
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
      *----------------------------------------------------------------*
       LINKAGE SECTION.                                                 
      *----------------------------------------------------------------*
       01  DFHCOMMAREA.                                                 
           COPY SCSBY128.                                               
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
                                                                        
           MOVE ZEROS                     TO SCSBY128-COD-RETORNO       
           MOVE '0000'                    TO SCSBY128-COD-ERRO          
           MOVE 'SCSB0026'                TO SCSBY128-COD-MENSAGEM.     
                                                                        
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
                                                                        
               WHEN SCSBY128-P-INICIAL                                  
                                                                        
                    MOVE ZEROS            TO DANO-LOTE-RECTA            
                                          OF SCSBB021                   
                    MOVE ZEROS            TO NLOTE-REG-VDA              
                                          OF SCSBB021                   
                    MOVE ZEROS            TO DANO-VDA-RECTA             
                                          OF SCSBB019                   
                    MOVE ZEROS            TO NREG-VDA-RECTA             
                                          OF SCSBB019                   
                    MOVE ZEROS            TO NDETLH-LOTE-VDA            
                                          OF SCSBB019                   
                                                                        
               WHEN SCSBY128-P-SEGUINTE                                 
                                                                        
                    MOVE SCSBY128-P-ULT-ANO-LOTE                        
                                          TO DANO-LOTE-RECTA            
                                          OF SCSBB021                   
                                             WRK-ANO-LOTE-ANT           
                    MOVE SCSBY128-P-ULT-CTPO-LOTE                       
                                          TO CTPO-LOTE-VDA              
                                          OF SCSBB021                   
                                             WRK-CTPO-LOTE-ANT          
                    MOVE SCSBY128-P-ULT-NUMERO-LOT                      
                                          TO NLOTE-REG-VDA              
                                          OF SCSBB021                   
                                             WRK-NUMERO-LOT-ANT         
                    MOVE SCSBY128-P-ULT-ANO-RVS                         
                                          TO DANO-VDA-RECTA             
                                          OF SCSBB019                   
                                             WRK-ANO-RVS-ANT            
                    MOVE SCSBY128-P-ULT-NREG                            
                                          TO NREG-VDA-RECTA             
                                          OF SCSBB019                   
                                             WRK-NREG-ANT               
                    MOVE SCSBY128-P-ULT-NSEQ                            
                                          TO NDETLH-LOTE-VDA            
                                          OF SCSBB019                   
                                             WRK-NSEQ-ANT               
                                                                        
               WHEN OTHER                                               
                                                                        
                    MOVE 16                TO SCSBY128-COD-RETORNO      
                    MOVE '0001'            TO SCSBY128-COD-ERRO         
                    MOVE 'SCSB0040'        TO SCSBY128-COD-MENSAGEM     
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
                                                                        
           IF  SCSBY128-E-ANO-LOTE        EQUAL ZEROS OR                
               SCSBY128-E-ANO-LOTE        NOT NUMERIC                   
               MOVE ZEROS                 TO WRK-ANO-LOTE-INI           
               MOVE 9999                  TO WRK-ANO-LOTE-FIM           
           ELSE                                                         
               MOVE SCSBY128-E-ANO-LOTE   TO WRK-ANO-LOTE-INI           
               MOVE SCSBY128-E-ANO-LOTE   TO WRK-ANO-LOTE-FIM           
           END-IF.                                                      
                                                                        
           IF  SCSBY128-E-NUMERO-LOTE     EQUAL ZEROS OR                
               SCSBY128-E-NUMERO-LOTE     NOT NUMERIC                   
               MOVE ZEROS                 TO WRK-NLOTE-REG-INI          
               MOVE 999999999             TO WRK-NLOTE-REG-FIM          
           ELSE                                                         
               MOVE SCSBY128-E-NUMERO-LOTE                              
                                          TO WRK-NLOTE-REG-INI          
               MOVE SCSBY128-E-NUMERO-LOTE                              
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
                                                                        
           IF SCSBY128-IND-PAGINACAO      NOT EQUAL 'I' AND 'S'         
              MOVE 08                     TO SCSBY128-COD-RETORNO       
              MOVE '0002'                 TO SCSBY128-COD-ERRO          
              MOVE 'SCSB0040'             TO SCSBY128-COD-MENSAGEM      
              PERFORM 3000-FINALIZAR                                    
           END-IF.                                                      
                                                                        
           IF SCSBY128-E-EMPRESA          EQUAL SPACES                  
              MOVE 08                     TO SCSBY128-COD-RETORNO       
              MOVE '0003'                 TO SCSBY128-COD-ERRO          
              MOVE 'SCSB0001'             TO SCSBY128-COD-MENSAGEM      
              PERFORM 3000-FINALIZAR                                    
           END-IF.                                                      
                                                                        
           IF SCSBY128-E-DATA-INI         EQUAL SPACES                  
              MOVE 08                     TO SCSBY128-COD-RETORNO       
              MOVE '0004'                 TO SCSBY128-COD-ERRO          
              MOVE 'SCSB0041'             TO SCSBY128-COD-MENSAGEM      
              PERFORM 3000-FINALIZAR                                    
           END-IF.                                                      
                                                                        
           IF SCSBY128-E-DATA-FIM         EQUAL SPACES                  
              MOVE 08                     TO SCSBY128-COD-RETORNO       
              MOVE '0005'                 TO SCSBY128-COD-ERRO          
              MOVE 'SCSB0041'             TO SCSBY128-COD-MENSAGEM      
              PERFORM 3000-FINALIZAR                                    
           END-IF.                                                      
                                                                        
           IF SCSBY128-E-TP-REGISTRO      EQUAL ZEROS OR                
              SCSBY128-E-TP-REGISTRO      NOT NUMERIC                   
              MOVE 08                     TO SCSBY128-COD-RETORNO       
              MOVE '0006'                 TO SCSBY128-COD-ERRO          
              MOVE 'SCSB0051'             TO SCSBY128-COD-MENSAGEM      
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
                                                                        
           PERFORM 7000-OPEN-CSR01-SCSBB021-JN.                         
           PERFORM 7005-FETCH-CSR01-SCSBB021-JN.                        
                                                                        
           PERFORM UNTIL SQLCODE EQUAL +100 OR                          
                   WRK-I GREATER WRK-QTDE-LIMITE - 1 OR                 
                   WRK-II GREATER 50                                    
                                                                        
                   ADD 1                  TO WRK-II                     
                                                                        
                   IF  WRK-LOTE-ATU       NOT EQUAL WRK-LOTE-ANT        
                       PERFORM 2900-MONTAR-BL-SAIDA-RVS                 
                       MOVE WRK-LOTE-ATU  TO WRK-LOTE-ANT               
                       INITIALIZE            WRK-RVS-ANT                
                   END-IF                                               
                                                                        
                   IF  WRK-I          NOT GREATER WRK-QTDE-LIMITE - 1   
                                                                        
                       IF  WRK-RVS-ATU    NOT EQUAL WRK-RVS-ANT         
                           PERFORM 4000-ACESSAR-SCSB2032                
                           MOVE WRK-RVS-ATU                             
                                          TO WRK-RVS-ANT                
                       END-IF                                           
                                                                        
                       IF  WRK-I      NOT GREATER WRK-QTDE-LIMITE - 1   
                           MOVE NDETLH-LOTE-VDA                         
                                          OF SCSBB019                   
                                          TO SCSBY128-P-ULT-NSEQ        
                                             WRK-NSEQ-ATU               
                                                                        
                           PERFORM 2200-OBTER-DADOS-OPER-FAT            
                       END-IF                                           
                   END-IF                                               
                                                                        
           END-PERFORM.                                                 
                                                                        
           IF  WRK-I                      GREATER 50                    
               MOVE 50                    TO SCSBY128-S-QTD-REG-RVS     
           ELSE                                                         
               MOVE WRK-I                 TO SCSBY128-S-QTD-REG-RVS     
           END-IF.                                                      
                                                                        
           PERFORM 7010-CLOSE-CSR01-SCSBB021-JN.                        
                                                                        
           IF  WRK-I                      EQUAL ZEROS                   
               MOVE 08                    TO SCSBY128-COD-RETORNO       
               IF  SCSBY128-IND-PAGINACAO EQUAL 'I'                     
                   MOVE '0007'            TO SCSBY128-COD-ERRO          
                   MOVE 'SCSB0029'        TO SCSBY128-COD-MENSAGEM      
               ELSE                                                     
                   MOVE '0008'            TO SCSBY128-COD-ERRO          
                   MOVE 'SCSB0030'        TO SCSBY128-COD-MENSAGEM      
               END-IF                                                   
               GO                         TO 2000-99-FIM                
           END-IF.                                                      
                                                                        
           IF WRK-FIM-CSR                 EQUAL 'S'                     
              MOVE 00                     TO SCSBY128-COD-RETORNO       
           ELSE                                                         
              MOVE 01                     TO SCSBY128-COD-RETORNO       
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       2000-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *   AJUSTE DE CHAVE PARA PAGINACAO                               *
      *----------------------------------------------------------------*
       2100-AJUSTAR-ULT-CHAVE             SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           MOVE WRK-ANO-LOTE-ANT          TO SCSBY128-P-ULT-ANO-LOTE.   
           MOVE WRK-CTPO-LOTE-ANT         TO SCSBY128-P-ULT-CTPO-LOTE.  
           MOVE WRK-NUMERO-LOT-ANT        TO SCSBY128-P-ULT-NUMERO-LOT. 
           MOVE WRK-ANO-RVS-ANT           TO SCSBY128-P-ULT-ANO-RVS.    
           MOVE WRK-NREG-ANT              TO SCSBY128-P-ULT-NREG.       
           MOVE WRK-NSEQ-ANT              TO SCSBY128-P-ULT-NSEQ.       
                                                                        
      *----------------------------------------------------------------*
       2100-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       2200-OBTER-DADOS-OPER-FAT          SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           IF  WRK-DANO-VDA-OPER-NULL     EQUAL ZEROS AND               
               WRK-NREG-VDA-OPER-NULL     EQUAL ZEROS AND               
               WRK-COPER-VDA-EXTER-NULL   EQUAL ZEROS                   
               MOVE DANO-VDA-OPER         OF SCSBB019                   
                                          TO SCSBY033-E-ANO             
               MOVE NREG-VDA-OPER         OF SCSBB019                   
                                          TO SCSBY033-E-NREG            
               MOVE COPER-VDA-EXTER       OF SCSBB019                   
                                          TO SCSBY033-E-COPER           
               PERFORM 4100-ACESSAR-SCSB2033                            
               MOVE WRK-CHAVE-ATU         TO WRK-CHAVE-ANT              
               PERFORM 7005-FETCH-CSR01-SCSBB021-JN                     
           ELSE                                                         
               IF WRK-DANO-VDA-FATMT-NULL EQUAL ZEROS AND               
                  WRK-NREG-VDA-FATMT-NULL EQUAL ZEROS AND               
                  WRK-NFATMT-VDA-EXTER-NULL                             
                                          EQUAL ZEROS                   
                  PERFORM 7100-TRATAR-FATURAMENTO                       
                  IF  WRK-FIM-CSR2        EQUAL 'S'                     
                      MOVE ZEROS          TO COPER-VDA-EXTER            
                                          OF SCSBB005                   
                      MOVE 'N'            TO WRK-FIM-CSR2               
                      MOVE WRK-CHAVE-ATU  TO WRK-CHAVE-ANT              
                      PERFORM 7005-FETCH-CSR01-SCSBB021-JN              
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
       2900-MONTAR-BL-SAIDA-RVS           SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           ADD  1                         TO WRK-I.                     
                                                                        
           MOVE 1                         TO SCSBY128-S-TPO-REG (WRK-I).
                                                                        
           MOVE DLOTE-RECTA-FEDRL         OF SCSBB021                   
                                          TO                            
                                       SCSBY128-S-DT-TRANSMISSAO(WRK-I).
                                                                        
           MOVE DANO-LOTE-RECTA           OF SCSBB021                   
                                          TO SCSBY128-S-ANO-LOTE(WRK-I) 
                                             SCSBY128-P-ULT-ANO-LOTE    
                                             WRK-ANO-LOTE-ATU.          
                                                                        
           MOVE CTPO-LOTE-VDA             OF SCSBB021                   
                                          TO SCSBY128-S-CTPO-LOTE(WRK-I)
                                             SCSBY128-P-ULT-CTPO-LOTE   
                                             WRK-CTPO-LOTE-ATU.         
                                                                        
           MOVE NLOTE-REG-VDA             OF SCSBB021                   
                                          TO                            
                                           SCSBY128-S-NUMERO-LOTE(WRK-I)
                                             SCSBY128-P-ULT-NUMERO-LOT  
                                             WRK-NUMERO-LOT-ATU.        
                                                                        
           MOVE CPROT-LOTE-RECTA          OF SCSBB021                   
                                          TO                            
                                           SCSBY128-S-CPROT-LOTE(WRK-I).
                                                                        
      *----------------------------------------------------------------*
       2900-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *   FORMATA BL DE SAIDA(FUNCIONAL)                               *
      *----------------------------------------------------------------*
       2910-MONTAR-BL-SAIDA-FAT           SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           ADD  1                         TO WRK-I.                     
                                                                        
           MOVE 4                         TO SCSBY128-S-TPO-REG (WRK-I).
                                                                        
           MOVE NFATMT-VDA-EXTER          OF SCSBB005                   
                                          TO                            
                                            SCSBY128-S-NUM-FATMT(WRK-I).
                                                                        
           MOVE DFATMT-VDA-EXTER          OF SCSBB022                   
                                          TO SCSBY128-S-DT-FATMT(WRK-I).
                                                                        
           MOVE COPER-VDA-EXTER           OF SCSBB005                   
                                          TO SCSBY128-P-ULT-COPER.      
                                                                        
           PERFORM 2911-SELECT-SUM-B005.                                
                                                                        
           MOVE VFATMT-OPER-VDA           OF SCSBB005                   
                                          TO                            
                                            SCSBY128-S-VLR-FATMT(WRK-I).
                                                                        
      *----------------------------------------------------------------*
       2910-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *  ACESSAR A TABELA SCSBB005 SUMARIZANDO O VLR DOS FATURAMENTOS  *
      *----------------------------------------------------------------*
       2911-SELECT-SUM-B005               SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           EXEC  SQL SELECT                                             
                 VALUE ( SUM(VFATMT_OPER_VDA), 0 )                      
                 INTO :SCSBB005.VFATMT-OPER-VDA                         
                 FROM DB2PRD.TFATMT_OPER_VDA                            
                 WHERE (DANO_VDA_RECTA     = :SCSBB005.DANO-VDA-RECTA)  
                  AND  (NREG_VDA_RECTA     = :SCSBB005.NREG-VDA-RECTA)  
                  AND  (DANO_VDA_FATMT     = :SCSBB005.DANO-VDA-RECTA)  
                  AND  (NREG_VDA_FATMT     = :SCSBB005.NREG-VDA-RECTA)  
                  AND  (NFATMT_VDA_EXTER   = :SCSBB005.NFATMT-VDA-EXTER)
           END-EXEC.                                                    
                                                                        
           IF (SQLCODE                    NOT EQUAL ZEROS) OR           
              (SQLWARN0                   EQUAL 'W')                    
               SET   DB2-SELECT           TO TRUE                       
               MOVE '08.1'                TO SCSBY128-COD-ERRO          
               MOVE 'TFATMT_OPER_VDA'     TO WRK-NOME-TABELA            
               PERFORM 9996-ERRO-DB2                                    
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       2911-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       2920-MONTAR-BL-SAIDA-RVS2          SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           ADD  1                         TO WRK-I.                     
                                                                        
           MOVE 2                         TO SCSBY128-S-TPO-REG (WRK-I).
                                                                        
                                                                        
           MOVE DANO-VDA-RECTA            OF SCSBB019                   
                                          TO SCSBY128-S-ANO     (WRK-I) 
                                             SCSBY128-P-ULT-ANO-RVS     
                                             WRK-ANO-RVS-ATU.           
                                                                        
           MOVE NREG-VDA-RECTA            OF SCSBB019                   
                                          TO SCSBY128-S-NREG    (WRK-I) 
                                             SCSBY128-P-ULT-NREG        
                                             WRK-NREG-ATU.              
                                                                        
           MOVE SCSBY032-S-NOME-ADQUIR    TO                            
                                          SCSBY128-S-NOME-ADQUIR(WRK-I).
           MOVE SCSBY032-S-CPF-CNPJ-PRINC TO                            
                                       SCSBY128-S-CPF-CNPJ-PRINC(WRK-I).
           MOVE SCSBY032-S-CPF-CNPJ-FLIAL TO                            
                                       SCSBY128-S-CPF-CNPJ-FLIAL(WRK-I).
           MOVE SCSBY032-S-CPF-CNPJ-CTRL  TO                            
                                        SCSBY128-S-CPF-CNPJ-CTRL(WRK-I).
           MOVE SCSBY032-S-NIF            TO SCSBY128-S-NIF     (WRK-I).
           MOVE SCSBY032-S-CD-MOEDA       TO SCSBY128-S-CD-MOEDA(WRK-I).
           MOVE SCSBY032-S-DS-MOEDA       TO SCSBY128-S-DS-MOEDA(WRK-I).
                                                                        
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
       4000-ACESSAR-SCSB2032              SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           MOVE '4000-ACESSAR-SCSB2032'   TO FRWKGHEA-IDEN-PARAGRAFO    
                                                                        
           INITIALIZE  SCSBY032-BLOCO-RETORNO                           
                       SCSBY032-REGISTRO.                               
                                                                        
           MOVE  DANO-VDA-RECTA           OF SCSBB019                   
                                          TO SCSBY032-E-ANO.            
           MOVE  NREG-VDA-RECTA           OF SCSBB019                   
                                          TO SCSBY032-E-NREG.           
                                                                        
           EXEC CICS LINK                                               
                PROGRAM   (WRK-SCSB2032)                                
                COMMAREA  (WRK-AREA-SCSB2032)                           
                LENGTH    (LENGTH OF WRK-AREA-SCSB2032)                 
                NOHANDLE                                                
           END-EXEC.                                                    
                                                                        
           IF EIBRESP                     NOT EQUAL DFHRESP(NORMAL)     
              MOVE '0009'                 TO SCSBY128-COD-ERRO          
              PERFORM 9997-ERRO-CICS                                    
           END-IF                                                       
                                                                        
           MOVE SCSBY032-BLOCO-RETORNO    TO WRK-BLOCO-RETORNO          
                                             SCSBY128-BLOCO-RETORNO.    
                                                                        
           IF SCSBY032-COD-RETORNO        NOT EQUAL ZEROS AND 08        
              MOVE '0010'                 TO SCSBY128-COD-ERRO          
              MOVE WRK-SCSB2032           TO WRK-MODULO                 
              PERFORM 9998-ERRO-MODULO                                  
           END-IF.                                                      
                                                                        
           IF SCSBY032-COD-RETORNO        EQUAL 08                      
              MOVE '0011'                 TO SCSBY128-COD-ERRO          
              PERFORM 3000-FINALIZAR                                    
           END-IF.                                                      
                                                                        
           PERFORM 2920-MONTAR-BL-SAIDA-RVS2.                           
                                                                        
      *----------------------------------------------------------------*
       4000-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       4100-ACESSAR-SCSB2033              SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           MOVE '4100-ACESSAR-SCSB2033'   TO FRWKGHEA-IDEN-PARAGRAFO    
                                                                        
           INITIALIZE  SCSBY033-BLOCO-RETORNO.                          
                                                                        
           EXEC CICS LINK                                               
                PROGRAM   (WRK-SCSB2033)                                
                COMMAREA  (WRK-AREA-SCSB2033)                           
                LENGTH    (LENGTH OF WRK-AREA-SCSB2033)                 
                NOHANDLE                                                
           END-EXEC.                                                    
                                                                        
           IF EIBRESP                     NOT EQUAL DFHRESP(NORMAL)     
              MOVE '0012'                 TO SCSBY128-COD-ERRO          
              PERFORM 9997-ERRO-CICS                                    
           END-IF.                                                      
                                                                        
           MOVE SCSBY033-BLOCO-RETORNO    TO WRK-BLOCO-RETORNO          
                                             SCSBY128-BLOCO-RETORNO.    
                                                                        
           IF SCSBY033-COD-RETORNO        NOT EQUAL ZEROS AND 08        
              MOVE '0013'                 TO SCSBY128-COD-ERRO          
              MOVE WRK-SCSB2033           TO WRK-MODULO                 
              PERFORM 9998-ERRO-MODULO                                  
           END-IF.                                                      
                                                                        
           IF SCSBY033-COD-RETORNO        EQUAL 08                      
              MOVE '0014'                 TO SCSBY128-COD-ERRO          
              PERFORM 3000-FINALIZAR                                    
           END-IF.                                                      
                                                                        
           ADD  1                         TO WRK-I.                     
                                                                        
           MOVE 3                         TO SCSBY128-S-TPO-REG(WRK-I). 
           MOVE SCSBY033-S-CD-NBS(1)      TO SCSBY128-S-CD-NBS (WRK-I). 
           MOVE SCSBY033-S-DS-NBS(1)      TO SCSBY128-S-DS-NBS (WRK-I). 
           MOVE SCSBY033-S-DT-INICIO(1)   TO                            
                                           SCSBY128-S-DT-INICIO(WRK-I). 
           MOVE SCSBY033-S-DT-CONCLUSAO(1)                              
                                          TO                            
                                        SCSBY128-S-DT-CONCLUSAO(WRK-I). 
           MOVE SCSBY033-S-VALOR(1)       TO SCSBY128-S-VALOR  (WRK-I). 
           MOVE COPER-VDA-EXTER           OF SCSBB019                   
                                          TO SCSBY128-S-COPER  (WRK-I). 
                                                                        
      *----------------------------------------------------------------*
       4100-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *     ACESSAR O UORG PARA CONVERTER A EMPRESA                    *
      *----------------------------------------------------------------*
       4200-ACESSAR-UORG1397              SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           MOVE    'D'                    TO  UORGW397-E-AMBIENTE.      
           MOVE    SCSBY128-E-EMPRESA     TO  UORGW397-E-CEMPR-SAP.     
           MOVE    ZEROS                  TO  UORGW397-E-CPSSOA-JURID.  
                                                                        
           EXEC CICS LINK                                               
                PROGRAM (WRK-UORG1397)                                  
                COMMAREA(WRK-AREA-UORG1397)                             
                LENGTH  (LENGTH OF WRK-AREA-UORG1397)                   
                NOHANDLE                                                
           END-EXEC.                                                    
                                                                        
           IF  EIBRESP                    NOT EQUAL DFHRESP(NORMAL)     
               MOVE '0015'                TO  SCSBY128-COD-ERRO         
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
                                          OF SCSBB016                   
           ELSE                                                         
               MOVE UORGW397-S-CPSSOA-JURID                             
                                          TO CPSSOA-JURID               
                                          OF SCSBB016                   
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       4200-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       7000-OPEN-CSR01-SCSBB021-JN        SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           MOVE '7000-OPEN-CSR01-SCSBB021-JN'                           
                                          TO FRWKGHEA-IDEN-PARAGRAFO.   
                                                                        
           MOVE SCSBY128-E-DATA-INI       TO WRK-DATA-INI.              
           MOVE SCSBY128-E-DATA-FIM       TO WRK-DATA-FIM.              
                                                                        
           MOVE SCSBY128-E-TP-REGISTRO    TO CTPO-LOTE-VDA              
                                          OF SCSBB021.                  
                                                                        
           EXEC SQL                                                     
                OPEN CSR01-SCSBB021-JN                                  
           END-EXEC.                                                    
                                                                        
           IF  (SQLCODE                   NOT EQUAL ZEROS) OR           
               (SQLWARN0                  EQUAL 'W')                    
                SET   DB2-OPEN            TO TRUE                       
                MOVE '0016'               TO SCSBY128-COD-ERRO          
                MOVE 'SCSBB021'           TO WRK-STORED-PROC            
                MOVE 'TLOTE_REG_VDA'      TO WRK-NOME-TABELA            
                PERFORM 9996-ERRO-DB2                                   
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       7000-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       7005-FETCH-CSR01-SCSBB021-JN       SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           MOVE '7005-FETCH-CSR01-SCSBB021-JN'                          
                                          TO FRWKGHEA-IDEN-PARAGRAFO.   
                                                                        
           EXEC SQL FETCH CSR01-SCSBB021-JN INTO                        
                  :SCSBB021.DLOTE-RECTA-FEDRL                           
                 ,:SCSBB021.DANO-LOTE-RECTA                             
                 ,:SCSBB021.CTPO-LOTE-VDA                               
                 ,:SCSBB021.NLOTE-REG-VDA                               
                 ,:SCSBB021.CPROT-LOTE-RECTA                            
                 ,:SCSBB019.DANO-VDA-RECTA                              
                 ,:SCSBB019.NREG-VDA-RECTA                              
                 ,:SCSBB019.NDETLH-LOTE-VDA                             
                 ,:SCSBB019.DANO-VDA-OPER                               
                            :WRK-DANO-VDA-OPER-NULL                     
                 ,:SCSBB019.NREG-VDA-OPER                               
                            :WRK-NREG-VDA-OPER-NULL                     
                 ,:SCSBB019.COPER-VDA-EXTER                             
                            :WRK-COPER-VDA-EXTER-NULL                   
                 ,:SCSBB019.DANO-VDA-FATMT                              
                            :WRK-DANO-VDA-FATMT-NULL                    
                 ,:SCSBB019.NREG-VDA-FATMT                              
                            :WRK-NREG-VDA-FATMT-NULL                    
                 ,:SCSBB019.NFATMT-VDA-EXTER                            
                            :WRK-NFATMT-VDA-EXTER-NULL                  
           END-EXEC.                                                    
                                                                        
           IF  (SQLCODE                   NOT EQUAL ZEROS AND +100) OR  
               (SQLWARN0                  EQUAL 'W')                    
                SET   DB2-FETCH           TO TRUE                       
                MOVE '0017'               TO SCSBY128-COD-ERRO          
                MOVE 'SCSBB021'           TO WRK-STORED-PROC            
                MOVE 'TLOTE_REG_VDA'      TO WRK-NOME-TABELA            
                PERFORM 9996-ERRO-DB2                                   
           END-IF.                                                      
                                                                        
           IF  SQLCODE                    EQUAL +100                    
               MOVE 'S'                   TO WRK-FIM-CSR                
               GO TO 7005-99-FIM                                        
           END-IF.                                                      
                                                                        
      *----> CHAVE LOTE                                                 
           MOVE DANO-LOTE-RECTA           OF SCSBB021                   
                                          TO WRK-CHV-DANO-LOTE-ATU.     
           MOVE CTPO-LOTE-VDA             OF SCSBB021                   
                                          TO WRK-CHV-CTPO-LOTE-ATU.     
           MOVE NLOTE-REG-VDA             OF SCSBB021                   
                                          TO WRK-NLOTE-ATU.             
                                                                        
      *----> CHAVE RVS                                                  
           MOVE DANO-VDA-RECTA            OF SCSBB019                   
                                          TO WRK-CHV-DANO-VDA-ATU.      
           MOVE NREG-VDA-RECTA            OF SCSBB019                   
                                          TO WRK-CHV-NREG-VDA-ATU.      
                                                                        
      *----> CHAVE FATURAMENTO                                          
           MOVE DANO-VDA-FATMT            OF SCSBB019                   
                                          TO WRK-CHV-DANO-FATMT-ATU.    
           MOVE NREG-VDA-FATMT            OF SCSBB019                   
                                          TO WRK-CHV-NREG-FATMT-ATU.    
           MOVE NFATMT-VDA-EXTER          OF SCSBB019                   
                                          TO WRK-CHV-NFATMT-ATU.        
                                                                        
      *----------------------------------------------------------------*
       7005-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       7010-CLOSE-CSR01-SCSBB021-JN       SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           MOVE '7010-CLOSE-CSR01-SCSBB021-JN'                          
                                          TO FRWKGHEA-IDEN-PARAGRAFO    
                                                                        
           EXEC SQL                                                     
                CLOSE CSR01-SCSBB021-JN                                 
           END-EXEC.                                                    
                                                                        
           IF  (SQLCODE                   NOT EQUAL ZEROS) OR           
               (SQLWARN0                  EQUAL 'W')                    
                SET   DB2-CLOSE           TO TRUE                       
                MOVE '0018'               TO SCSBY128-COD-ERRO          
                MOVE 'SCSBB021'           TO WRK-STORED-PROC            
                MOVE 'TLOTE_REG_VDA'      TO WRK-NOME-TABELA            
                PERFORM 9996-ERRO-DB2                                   
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       7010-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *       OBTER AS OPERACOES DE FATURAMENTO                        *
      *----------------------------------------------------------------*
       7100-TRATAR-FATURAMENTO            SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           PERFORM 7105-TRATAR-PAGINACAO.                               
                                                                        
           PERFORM 7110-OPEN-CSR02-SCSBB005-JN.                         
                                                                        
           PERFORM 7120-FETCH-CSR02-SCSBB005-JN.                        
                                                                        
           PERFORM UNTIL SQLCODE EQUAL +100 OR                          
                   WRK-I GREATER WRK-QTDE-LIMITE                        
                                                                        
                   IF  WRK-FATMT-ATU      NOT EQUAL WRK-FATMT-ANT       
                       PERFORM 2910-MONTAR-BL-SAIDA-FAT                 
                       MOVE WRK-FATMT-ATU TO WRK-FATMT-ANT              
                   END-IF                                               
                                                                        
                   MOVE DANO-VDA-RECTA    OF SCSBB005                   
                                          TO SCSBY033-E-ANO             
                   MOVE NREG-VDA-RECTA    OF SCSBB005                   
                                          TO SCSBY033-E-NREG            
                   MOVE COPER-VDA-EXTER   OF SCSBB005                   
                                          TO SCSBY033-E-COPER           
                                                                        
                   PERFORM 4100-ACESSAR-SCSB2033                        
                                                                        
                   PERFORM 7120-FETCH-CSR02-SCSBB005-JN                 
                                                                        
           END-PERFORM.                                                 
                                                                        
           PERFORM 7130-CLOSE-CSR02-SCSBB005-JN.                        
                                                                        
      *----------------------------------------------------------------*
       7100-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *  FORMATA CHAVE DE PESQUISA PARA PAGINACAO TABELA 05            *
      *----------------------------------------------------------------*
       7105-TRATAR-PAGINACAO              SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           EVALUATE TRUE                                                
                                                                        
               WHEN SCSBY128-P-INICIAL                                  
                                                                        
                    MOVE ZEROS            TO COPER-VDA-EXTER            
                                          OF SCSBB005                   
                                                                        
               WHEN SCSBY128-P-SEGUINTE                                 
                                                                        
                    MOVE SCSBY128-P-ULT-COPER                           
                                          TO COPER-VDA-EXTER            
                                          OF SCSBB005                   
                                                                        
               WHEN OTHER                                               
                                                                        
                    MOVE 16                TO SCSBY128-COD-RETORNO      
                    MOVE '0019'            TO SCSBY128-COD-ERRO         
                    MOVE 'SCSB0040'        TO SCSBY128-COD-MENSAGEM     
                    PERFORM 3000-FINALIZAR                              
                                                                        
           END-EVALUATE.                                                
                                                                        
      *----------------------------------------------------------------*
       7105-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       7110-OPEN-CSR02-SCSBB005-JN        SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           MOVE '7110-OPEN-CSR02-SCSBB005-JN'                           
                                          TO FRWKGHEA-IDEN-PARAGRAFO.   
                                                                        
           MOVE DANO-VDA-FATMT            OF SCSBB019                   
                                          TO DANO-VDA-RECTA             
                                          OF SCSBB005.                  
                                                                        
           MOVE NREG-VDA-FATMT            OF SCSBB019                   
                                          TO NREG-VDA-RECTA             
                                          OF SCSBB005.                  
                                                                        
           MOVE NFATMT-VDA-EXTER          OF SCSBB019                   
                                          TO NFATMT-VDA-EXTER           
                                          OF SCSBB005.                  
                                                                        
           EXEC SQL                                                     
                OPEN CSR02-SCSBB005-JN                                  
           END-EXEC.                                                    
                                                                        
           IF  (SQLCODE                   NOT EQUAL ZEROS) OR           
               (SQLWARN0                  EQUAL 'W')                    
                SET   DB2-OPEN            TO TRUE                       
                MOVE '0020'               TO SCSBY128-COD-ERRO          
                MOVE 'SCSBB005'           TO WRK-STORED-PROC            
                MOVE 'TFATMT_OPER_VDA'    TO WRK-NOME-TABELA            
                PERFORM 9996-ERRO-DB2                                   
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       7110-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       7120-FETCH-CSR02-SCSBB005-JN       SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           MOVE '7120-FETCH-CSR02-SCSBB005-JN'                          
                                          TO FRWKGHEA-IDEN-PARAGRAFO.   
                                                                        
           EXEC SQL FETCH CSR02-SCSBB005-JN INTO                        
                  :SCSBB005.COPER-VDA-EXTER                             
                 ,:SCSBB022.DFATMT-VDA-EXTER                            
           END-EXEC.                                                    
                                                                        
           IF  (SQLCODE                   NOT EQUAL ZEROS AND +100) OR  
               (SQLWARN0                  EQUAL 'W')                    
                SET   DB2-FETCH           TO TRUE                       
                MOVE '0021'               TO SCSBY128-COD-ERRO          
                MOVE 'SCSBB005'           TO WRK-STORED-PROC            
                MOVE 'TFATMT_OPER_VDA'    TO WRK-NOME-TABELA            
                PERFORM 9996-ERRO-DB2                                   
           END-IF.                                                      
                                                                        
           IF  SQLCODE                    EQUAL +100                    
               MOVE 'S'                   TO WRK-FIM-CSR2               
           END-IF.                                                      
                                                                        
           MOVE COPER-VDA-EXTER           OF SCSBB005                   
                                          TO COPER-VDA-EXTER            
                                          OF SCSBB019.                  
                                                                        
      *----------------------------------------------------------------*
       7120-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       7130-CLOSE-CSR02-SCSBB005-JN       SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           MOVE '7130-CLOSE-CSR02-SCSBB005-JN'                          
                                          TO FRWKGHEA-IDEN-PARAGRAFO    
                                                                        
           EXEC SQL                                                     
                CLOSE CSR02-SCSBB005-JN                                 
           END-EXEC.                                                    
                                                                        
           IF  (SQLCODE                   NOT EQUAL ZEROS) OR           
               (SQLWARN0                  EQUAL 'W')                    
                SET   DB2-CLOSE           TO TRUE                       
                MOVE '0022'               TO SCSBY128-COD-ERRO          
                MOVE 'SCSBB005'           TO WRK-STORED-PROC            
                MOVE 'TFATMT_OPER_VDA'    TO WRK-NOME-TABELA            
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
                                                                        
           MOVE  16                       TO  SCSBY128-COD-RETORNO      
           MOVE  'SCSB0032'               TO  SCSBY128-COD-MENSAGEM     
                                                                        
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
                                                                        
           MOVE  16                       TO  SCSBY128-COD-RETORNO      
           MOVE  'SCSB0033'               TO  SCSBY128-COD-MENSAGEM     
                                                                        
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

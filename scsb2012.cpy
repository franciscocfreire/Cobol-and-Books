      *================================================================*
       IDENTIFICATION DIVISION.                                         
      *================================================================*
      *                                                                *
       PROGRAM-ID.    SCSB2012.                                         
       AUTHOR.        BRQ.                                              
      *                                                                *
      *================================================================*
      *                          BRQ IT SERVICES                       *
      *================================================================*
      *                                                                *
      *  PROGRAMADOR.......: RENATO - BRQ                              *
      *  ANALISTA..........: FERNANDA CARUSO                           *
      *  DATA..............: 28/03/2013                                *
      *                                                                *
      *================================================================*
      *                                                                *
      *  OBJETIVO..........:                                           *
      *                                                                *
      *  OBTER A EMPRESA, DEPENDENCIA E SETOR DO USUARIO NO SAP.       *
      *================================================================*
      *                                                                *
      * PROGRAMA : F U N C I O N A L                                   *
      *                                                                *
      *----------------------------------------------------------------*
      * I#FRWKGE COMMAREA FRWK1999 (LOG DE ERRO)                       *
      * I#FRWKHE AREA COMUM DE ERROS                                   *
      * SCSBY012 BLOCO DE ENTRADA E SAIDA - MODULO FUNCIONAL           *
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
                                                                        
       77  WRK-PROGRAMA            PIC  X(008) VALUE 'SCSB2012'.        
       77  WRK-FRWK1999            PIC  X(008) VALUE 'FRWK1999'.        
       77  WRK-MODULO-SARH9000     PIC  X(008) VALUE 'SARH9000'.        
       77  WRK-MODULO              PIC  X(008) VALUE SPACES.            
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
      *  AREAS AUXILIARES                                              *
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
       01  WRK-BLOCO-RETORNO.                                           
           05  WRK-COD-RETORNO     PIC  9(002) VALUE ZEROS.             
           05  WRK-COD-ERRO        PIC  X(004) VALUE SPACES.            
           05  WRK-COD-MENSAGEM    PIC  X(008) VALUE SPACES.            
                                                                        
       01  WRK-EMP-OK              PIC  X(001) VALUE SPACES.            
                                                                        
       01  WRK-I                   PIC  9(003)  COMP-3 VALUE ZEROS.     
                                                                        
       01  WRK-CUSUAR.                                                  
           05 WRK-CUSUAR-LETRA     PIC  X(001)         VALUE SPACES.    
           05 FILLER               PIC  X(008)         VALUE SPACES.    
                                                                        
       01  WRK-CUSUAR-N            REDEFINES                            
           WRK-CUSUAR              PIC  9(009).                         
                                                                        
       01  WRK-DEPEN-R             PIC  9(005) VALUE ZEROS.             
       01  WRK-SECAO-R             PIC  9(003) VALUE ZEROS.             
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                  PIC  X(080) VALUE                    
           'AREA LOG DE ERRO - FRWK1999'.                               
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
       01  WRK-AREA-ERRO.                                               
           COPY I#FRWKGE.                                               
           05 WRK-BLOCO-INFO-ERRO.                                      
              10 WRK-CHAR-INFO-ERRO PIC  X(001) OCCURS 0 TO 30000 TIMES 
                 DEPENDING ON FRWKGHEA-TAM-DADOS.                       
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                  PIC  X(080) VALUE                    
           'AREA TRATAMENTO DE ERRO - CICS'.                            
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
       01  WRK-AREA-ERRO-CICS.                                          
           COPY I#FRWKCI.                                               
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                  PIC  X(080) VALUE                    
           'AREA TRATAMENTO DE ERRO - MODULO'.                          
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
       01  WRK-AREA-ERRO-MODULO.                                        
           COPY I#FRWKMD.                                               
                                                                        
      *----------------------------------------------------------------*
      *AREA DE AREA DE INTERFACE COM O SARH8000                        *
      *================================================================*
                                                                        
       01  WRK-AREA-SARH9000.                                           
           03 WRK-ENTRADA-SARH9000.                                     
             05  WRK-ROTINA-ENT-9000       PIC X(004)  VALUE SPACES.    
             05  WRK-PESQUISA-ENT-9000     PIC 9(009)  VALUE ZEROS.     
             05  WRK-TAMANHO-ENT-9000      PIC 9(004)  VALUE ZEROS.     
             05  WRK-SOLICITA-ENT.                                      
                 07 FILLER OCCURS 255 TIMES.                            
                    10 WRK-CAMPO-ENT-9000  PIC 9(003)  VALUE ZEROS.     
           03 WRK-SAIDA-SARH9000.                                       
             05  WRK-COD-RETORNO-9000      PIC 9(002)  VALUE ZEROS.     
             05  WRK-TAMANHO-SAI-9000      PIC 9(004)  VALUE ZEROS.     
             05  WRK-ERRO-AREA             PIC X(107)  VALUE SPACES.    
             05  WRK-SQLCA                 PIC X(136)  VALUE SPACES.    
             05  WRK-REGISTRO-SAI-9000.                                 
                 07 FILLER OCCURS 0 TO 1000 TIMES                       
                           DEPENDING ON WRK-TAMANHO-SAI-9000.           
                    10 WRK-POSICAO-SAI     PIC X(001).                  
                                                                        
       01  WRK-RETORNO-SARH9000.                                        
           05 WRK-EMP                  PIC  X(004)         VALUE ZEROS. 
           05 WRK-DEPEN                PIC  9(005) COMP-3  VALUE ZEROS. 
           05 WRK-SECAO                PIC  9(003) COMP-3  VALUE ZEROS. 
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                  PIC  X(080) VALUE                    
           'FINAL DA WORKING STORAGE'.                                  
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
      *----------------------------------------------------------------*
       LINKAGE SECTION.                                                 
      *----------------------------------------------------------------*
      *                                                                *
       01  DFHCOMMAREA.                                                 
           COPY SCSBY012.                                               
                                                                        
      *================================================================*
       PROCEDURE DIVISION USING DFHCOMMAREA.                            
      *================================================================*
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
      *   ROTINA PRINCIPAL                                             *
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
      *----------------------------------------------------------------*
       0000-ESTRUTURA                     SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           PERFORM 1000-INICIALIZAR.                                    
                                                                        
           PERFORM 2000-PROCESSAR.                                      
                                                                        
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
                        WRK-EMP-OK.                                     
                                                                        
           PERFORM 1300-CONSISTIR-DADOS.                                
                                                                        
           MOVE ZEROS                      TO  SCSBY012-COD-RETORNO.    
           MOVE '0000'                     TO  SCSBY012-COD-ERRO.       
           MOVE 'SCSB0026'                 TO  SCSBY012-COD-MENSAGEM.   
                                                                        
      *----------------------------------------------------------------*
       1000-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
      *   CONSISTIR DADOS DE ENTRADA - FUNCIONAL                       *
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
      *----------------------------------------------------------------*
       1300-CONSISTIR-DADOS               SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           IF  SCSBY012-E-CD-USUAR     EQUAL SPACES OR LOW-VALUES       
               MOVE 08                 TO  SCSBY012-COD-RETORNO         
               MOVE '0001'             TO  SCSBY012-COD-ERRO            
               MOVE 'SCSB0040'         TO  SCSBY012-COD-MENSAGEM        
               PERFORM 3000-FINALIZAR                                   
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
                                                                        
           MOVE  SCSBY012-E-CD-USUAR   TO  WRK-CUSUAR.                  
                                                                        
           PERFORM 2100-FORMATA-CODIGO.                                 
                                                                        
           PERFORM 4000-ACESSA-SARH9000.                                
                                                                        
           MOVE  WRK-EMP               TO  SCSBY012-S-EMPRESA.          
                                                                        
           MOVE  WRK-DEPEN             TO  WRK-DEPEN-R                  
           MOVE  WRK-DEPEN-R           TO  SCSBY012-S-DEPEND.           
                                                                        
           MOVE  WRK-SECAO             TO  WRK-SECAO-R                  
           MOVE  WRK-SECAO-R           TO  SCSBY012-S-SECAO.            
                                                                        
      *----------------------------------------------------------------*
       2000-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * FORMATANDO CODIGO DO USUARIO PARA PESQUISA NO SARH8000         *
      *----------------------------------------------------------------*
       2100-FORMATA-CODIGO             SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           EVALUATE TRUE                                                
              WHEN WRK-CUSUAR-LETRA    EQUAL 'A' OR 'J' OR 'S'          
                  MOVE '1'             TO WRK-CUSUAR-LETRA              
                                                                        
              WHEN WRK-CUSUAR-LETRA    EQUAL 'B' OR 'K' OR 'T'          
                  MOVE '2'             TO WRK-CUSUAR-LETRA              
                                                                        
              WHEN WRK-CUSUAR-LETRA    EQUAL 'C' OR 'L' OR 'U'          
                  MOVE '3'             TO WRK-CUSUAR-LETRA              
                                                                        
              WHEN WRK-CUSUAR-LETRA    EQUAL 'D' OR 'M' OR 'V'          
                  MOVE '4'             TO WRK-CUSUAR-LETRA              
                                                                        
              WHEN WRK-CUSUAR-LETRA    EQUAL 'E' OR 'N' OR 'W'          
                  MOVE '5'             TO WRK-CUSUAR-LETRA              
                                                                        
              WHEN WRK-CUSUAR-LETRA    EQUAL 'F' OR 'O' OR 'X'          
                  MOVE '6'             TO WRK-CUSUAR-LETRA              
                                                                        
              WHEN WRK-CUSUAR-LETRA    EQUAL 'G' OR 'P' OR 'Y'          
                  MOVE '7'             TO WRK-CUSUAR-LETRA              
                                                                        
              WHEN WRK-CUSUAR-LETRA    EQUAL 'H' OR 'Q' OR 'Z'          
                  MOVE '8'             TO WRK-CUSUAR-LETRA              
                                                                        
              WHEN WRK-CUSUAR-LETRA    EQUAL 'I' OR 'R'                 
                  MOVE '9'             TO WRK-CUSUAR-LETRA              
           END-EVALUATE.                                                
                                                                        
      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.                            
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
       4000-ACESSA-SARH9000       SECTION.                              
      *----------------------------------------------------------------*
                                                                        
           MOVE '3500-OBTER-DEPTO-USUAR'                                
                                       TO  FRWKGHEA-IDEN-PARAGRAFO.     
                                                                        
           PERFORM VARYING WRK-I       FROM 1 BY 1                      
            UNTIL WRK-CUSUAR(WRK-I:1)  EQUAL SPACES OR                  
                  WRK-I                GREATER 9                        
           END-PERFORM                                                  
                                                                        
           SUBTRACT 1                  FROM WRK-I                       
                                                                        
           MOVE 'SCSB'                 TO  WRK-ROTINA-ENT-9000          
           MOVE WRK-CUSUAR-N(1:WRK-I)  TO  WRK-PESQUISA-ENT-9000        
                                                                        
           MOVE 15                     TO  WRK-TAMANHO-ENT-9000         
                                                                        
      *--> CAMPO DE PESQUISA                                            
           MOVE 006                    TO  WRK-CAMPO-ENT-9000(01)       
           MOVE 010                    TO  WRK-CAMPO-ENT-9000(02)       
           MOVE 012                    TO  WRK-CAMPO-ENT-9000(03)       
                                                                        
           EXEC  CICS  LINK                                             
                       PROGRAM (WRK-MODULO-SARH9000)                    
                       COMMAREA(WRK-AREA-SARH9000)                      
                       LENGTH  (LENGTH OF WRK-AREA-SARH9000)            
                       NOHANDLE                                         
           END-EXEC.                                                    
                                                                        
           IF  EIBRESP             NOT EQUAL DFHRESP(NORMAL)            
               MOVE '0003'             TO  SCSBY012-COD-ERRO            
               PERFORM 9997-ERRO-CICS                                   
           END-IF                                                       
                                                                        
           IF  WRK-COD-RETORNO-9000    NOT EQUAL   ZEROS                
               MOVE WRK-COD-RETORNO-9000   TO  WRK-COD-RETORNO          
               MOVE WRK-MODULO-SARH9000    TO  WRK-MODULO               
               PERFORM 9998-ERRO-MODULO                                 
           END-IF.                                                      
                                                                        
           MOVE WRK-REGISTRO-SAI-9000(1:WRK-TAMANHO-SAI-9000)           
                                       TO  WRK-RETORNO-SARH9000.        
                                                                        
      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       9997-ERRO-CICS                     SECTION.                      
      *----------------------------------------------------------------*
                                                                        
      *--> CARREGAR DADOS NA AREA DE RETORNO P/ O FRAMEWORK - I#FRWKAQ  
                                                                        
           MOVE  08                       TO  SCSBY012-COD-RETORNO      
           MOVE  'SCSB0033'               TO  SCSBY012-COD-MENSAGEM     
                                                                        
      *--> CARREGAR DADOS NA AREA COMUM DE ERROS (HEADER) - I#FRWKHE    
                                                                        
           SET   ERRO-CICS                TO  TRUE                      
           MOVE  WRK-PROGRAMA             TO  FRWKGHEA-NOME-PROGRAMA    
           MOVE  SCSBY012-TAM-LAYOUT      TO  FRWKGHEA-TAM-DADOS        
                                                                        
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
                                                                        

      *================================================================*
       IDENTIFICATION                  DIVISION.                        
      *================================================================*
                                                                        
       PROGRAM-ID. SCSB0100.                                            
       AUTHOR.     RICARDO BONFIM.                                      
                                                                        
      *================================================================*
      *               B R Q   -   I T   S E R V I C E S                *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA     : SCSB0100                                     *
      *    ANALISTA     : RICARDO BONFIM         - BRQ IT SERVICES.    *
      *    PROGRAMADOR  : RICARDO BONFIM         - BRQ IT SERVICES.    *
      *    DATA         : 20/05/2013                                   *
      *                                                                *
      *    OBJETIVO     : GERAR ARQUIVO DE INCLUSAO PARA RVS           *
      *                                                                *
      *           +---------------------------------------+            *
      *           |  DDNAME  |   I/O   |   BOOK   | LRECL |            *
      *           |----------|---------|----------|-------|            *
      *           | REGVENDA | INPUT   | I#SCSB16 | 5463  |            *
      *           | OPERACAO | INPUT   | I#SCSB10 | 0128  |            *
      *           | ENQUADRA | INPUT   | I#SCSB03 | 0029  |            *
      *           | REGEXPOR | INPUT   | I#SCSB14 | 0017  |            *
      *           | ARQBAIXA | OUTPUT  | I#SCSB51 | 5400  |            *
      *           +---------------------------------------+            *
      *                                                                *
      *    MODULOS :                                                   *
      *                                                                *
      *    -> BRAD0160 (    -   ) - OBTER JOBNAME                      *
      *    -> BRAD0450 (    -   ) - ABENDAR PROGRAMA                   *
      *    -> CKRS1000 (    -   ) - IDENTIFICA CONEXAO DB2             *
      *    -> CKRS1050 (    -   ) - CONEXAO DB2                        *
      *    -> CALE2000 (I#CALE01) - COMPONENTE CALENDARIO              *
      *    -> FRWK2999 (I#FRWKGE) - GRAV. DE ERRO NO COMPONENTE GLOG   *
      *                (I#FRWKHE) - AREA COMUM DE ERROS                *
      *                (I#FRWKDB) - AREA DE ERROS DE DB2               *
      *                (I#FRWKAR) - INFORMACOES DE ERRO -> ARQUIVO     *
      *                (I#FRWKMD) - INFORMACOES DE ERRO -> MODULO      *
      *                (I#FRWKLI) - INFORMACOES DE ERRO -> LIVRE       *
      *                                                                *
      *----------------------------------------------------------------*
                                                                        
      *================================================================*
      *                            ALTERACAO                           *
      *----------------------------------------------------------------*
      *                                                                *
      *  PROGRAMADOR: SANDRA PANSANI - BRQ                             *
      *  ANALISTA...: FERNANDA CARUSO - BRQ                            *
      *  DATA.......: JULHO/2013                                       *
      *  OBJETIVO...: UTILIZAR MODULOS PARA CONVERSAO DE MOEDA E PAIS  *
      *                                                                *
      *================================================================*
      *                            ALTERACAO                           *
      *----------------------------------------------------------------*
      *                                                                *
      *  PROGRAMADOR: ANDERSON MARTINS - BRQ                           *
      *  ANALISTA...: FERNANDA CARUSO  - BRQ                           *
      *  DATA.......: OUTUBRO/2013                                     *
      *  OBJETIVO...: TRATAR O CAMPO SCSB14-CTPO-REG-EXTER NA MONTAGEM *
      *               DO ARQUIVO ARQBAIXA.                             *
      *                                                                *
      *================================================================*
                                                                        
      *================================================================*
       ENVIRONMENT                     DIVISION.                        
      *================================================================*
                                                                        
      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.                         
      *----------------------------------------------------------------*
                                                                        
       SPECIAL-NAMES.                                                   
           DECIMAL-POINT               IS COMMA.                        
                                                                        
      *----------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.                         
      *----------------------------------------------------------------*
                                                                        
       FILE-CONTROL.                                                    
                                                                        
           SELECT REGVENDA ASSIGN      TO REGVENDA                      
                      FILE STATUS      IS WRK-FS-REGVENDA.              
                                                                        
           SELECT OPERACAO ASSIGN      TO OPERACAO                      
                      FILE STATUS      IS WRK-FS-OPERACAO.              
                                                                        
           SELECT ENQUADRA ASSIGN      TO ENQUADRA                      
                      FILE STATUS      IS WRK-FS-ENQUADRA.              
                                                                        
           SELECT REGEXPOR ASSIGN      TO REGEXPOR                      
                      FILE STATUS      IS WRK-FS-REGEXPOR.              
                                                                        
           SELECT ARQBAIXA ASSIGN      TO ARQBAIXA                      
                      FILE STATUS      IS WRK-FS-ARQBAIXA.              
                                                                        
      *================================================================*
       DATA                            DIVISION.                        
      *================================================================*
                                                                        
      *----------------------------------------------------------------*
       FILE                            SECTION.                         
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * INPUT - ARQUIVO COM AS RVS                                     *
      *                                                                *
      *                 ORG. SEQUENCIAL  -  LRECL 5463                 *
      *----------------------------------------------------------------*
                                                                        
       FD  REGVENDA                                                     
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  FD-REGVENDA                 PIC X(5463).                     
                                                                        
      *----------------------------------------------------------------*
      * INPUT - ARQUIVO COM AS OPERACOES                               *
      *                                                                *
      *                 ORG. SEQUENCIAL  -  LRECL 0128                 *
      *----------------------------------------------------------------*
                                                                        
       FD  OPERACAO                                                     
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  FD-OPERACAO                 PIC X(0128).                     
                                                                        
      *----------------------------------------------------------------*
      * INPUT - ARQUIVO COM OS ENQUADRAMENTOS                          *
      *                                                                *
      *                 ORG. SEQUENCIAL  -  LRECL 0029                 *
      *----------------------------------------------------------------*
                                                                        
       FD  ENQUADRA                                                     
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  FD-ENQUADRA                 PIC X(0029).                     
                                                                        
      *----------------------------------------------------------------*
      * INPUT - ARQUIVO COM OS REGISTROS                               *
      *                                                                *
      *                 ORG. SEQUENCIAL  -  LRECL 0017                 *
      *----------------------------------------------------------------*
                                                                        
       FD  REGEXPOR                                                     
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  FD-REGEXPOR                 PIC X(0017).                     
                                                                        
      *----------------------------------------------------------------*
      * OUTPUT - ARQUIVO COM OS LOTES ATUALIZADOS PARA BAIXA           *
      *                                                                *
      *                 ORG. SEQUENCIAL  -  LRECL 5400                 *
      *----------------------------------------------------------------*
                                                                        
       FD  ARQBAIXA                                                     
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  FD-ARQBAIXA                 PIC X(5400).                     
                                                                        
      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.                         
      *----------------------------------------------------------------*
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER PIC X(34) VALUE  '** INICIO DA WORKING SCSB0100 **  '.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER PIC X(34) VALUE  '** FILE-STATUS DOS ARQUIVOS **    '.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
       01  WRK-FS-REGVENDA             PIC  X(002)  VALUE SPACES.       
       01  WRK-FS-OPERACAO             PIC  X(002)  VALUE SPACES.       
       01  WRK-FS-ENQUADRA             PIC  X(002)  VALUE SPACES.       
       01  WRK-FS-REGEXPOR             PIC  X(002)  VALUE SPACES.       
       01  WRK-FS-ARQBAIXA             PIC  X(002)  VALUE SPACES.       
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER PIC X(34) VALUE  '** BOOK DOS ARQUIVOS **           '.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
      *            BOOK DO ARQUIVO DE RVS                              *
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
           COPY    I#SCSB16.                                            
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
      *            BOOK DO ARQUIVO DE OPERACOES                        *
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
           COPY    I#SCSB10.                                            
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
      *            BOOK DO ARQUIVO DE ENQUADRAMENTOS                   *
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
           COPY    I#SCSB03.                                            
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
      *            BOOK DO ARQUIVO DE REGISTROS                        *
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
           COPY    I#SCSB14.                                            
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
      *            BOOK DO ARQUIVO DE LOTES PARA BAIXA                 *
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
           COPY    I#SCSB51.                                            
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER PIC X(34) VALUE  '** AREA DA BRAD0160 **            '.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
       01  WRK-B0160-VALOR-FAC         PIC  9(005)  COMP-3 VALUE ZEROS. 
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER PIC X(34) VALUE  '** AREA DA BRAD0450 **            '.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
       01  WRK-B0450-ABEND             PIC S9(004)  COMP   VALUE +1111. 
       01  WRK-B0450-DUMP              PIC  X(001)         VALUE 'S'.   
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER PIC X(34) VALUE  '** AREA DA INEC2PPI **            '.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
       01  WRK-AREA-INECWPHI.                                           
           COPY    INECWPHI.                                            
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER PIC X(34) VALUE  '** AREA DA AGEO2000 **            '.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
           COPY    AGEOWAAI.                                            
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER PIC X(34) VALUE  '** AREA DA API - CALE2000 **      '.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
           COPY    I#CALE01.                                            
                                                                        
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
                                                                        
           EXEC  SQL    INCLUDE      SCSBB011   END-EXEC.               
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER PIC X(34) VALUE  '** AREA DA API - UORG2397 **      '.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
       01  WRK-AREA-UORG2397.                                           
           COPY    UORGWB97.                                            
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER PIC X(34) VALUE  '** AREA DA API - FRWK2999 **      '.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
       01  WRK-AREA-ERRO.                                               
           COPY I#FRWKGE.                                               
                                                                        
           05  WRK-BLOCO-INFO-ERRO.                                     
             10  WRK-CHAR-INFO-ERRO    PIC  X(001)                      
                                       OCCURS  0 TO 30000 TIMES         
                                       DEPENDING ON FRWKGHEA-TAM-DADOS. 
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER PIC X(34) VALUE  '** INFORMACOES DE ERRO - ARQ **   '.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
       01  WRK-AREA-ERRO-ARQUIVO.                                       
           COPY I#FRWKAR.                                               
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER PIC X(34) VALUE  '** INFORMACOES DE ERRO - MODULO **'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
       01  WRK-AREA-ERRO-MODULO.                                        
           COPY I#FRWKMD.                                               
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER PIC X(34) VALUE  '** INFORMACOES DE ERRO - LIVRE ** '.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
       01  WRK-AREA-ERRO-LIVRE.                                         
           COPY I#FRWKLI.                                               
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER PIC X(34) VALUE  '** AREA PARA ESTATISTICA **       '.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
           COPY SCSBWB99.                                               
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER PIC X(34) VALUE  '** ACUMULADORES **                '.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
       01  ACU-LIDOS-REGVENDA          PIC  9(012)  COMP-3 VALUE ZEROS. 
       01  ACU-LIDOS-OPERACAO          PIC  9(012)  COMP-3 VALUE ZEROS. 
       01  ACU-LIDOS-ENQUADRA          PIC  9(012)  COMP-3 VALUE ZEROS. 
       01  ACU-LIDOS-REGEXPOR          PIC  9(012)  COMP-3 VALUE ZEROS. 
       01  ACU-GRAVS-ARQBAIXA          PIC  9(012)  COMP-3 VALUE ZEROS. 
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER PIC X(34) VALUE  '** AREA DE CHAVES **              '.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
       01  WRK-CHV-REGVENDA.                                            
           05 WRK-CHV-DANO-REGVENDA    PIC  9(004)  COMP-3 VALUE ZEROS. 
           05 WRK-CHV-NREG-REGVENDA    PIC  9(009)  COMP-3 VALUE ZEROS. 
                                                                        
       01  WRK-CHV-OPERACAO.                                            
        03 WRK-CHV-RVSXOPER.                                            
           05 WRK-CHV-DANO-OPERACAO    PIC  9(004)  COMP-3 VALUE ZEROS. 
           05 WRK-CHV-NREG-OPERACAO    PIC  9(009)  COMP-3 VALUE ZEROS. 
        03    WRK-CHV-COPE-OPERACAO    PIC  9(010)  COMP-3 VALUE ZEROS. 
                                                                        
       01  WRK-CHV-ENQUADRA.                                            
           05 WRK-CHV-DANO-ENQUADRA    PIC  9(004)  COMP-3 VALUE ZEROS. 
           05 WRK-CHV-NREG-ENQUADRA    PIC  9(009)  COMP-3 VALUE ZEROS. 
           05 WRK-CHV-COPE-ENQUADRA    PIC  9(010)  COMP-3 VALUE ZEROS. 
                                                                        
       01  WRK-CHV-REGEXPOR.                                            
           05 WRK-CHV-DANO-REGEXPOR    PIC  9(004)  COMP-3 VALUE ZEROS. 
           05 WRK-CHV-NREG-REGEXPOR    PIC  9(009)  COMP-3 VALUE ZEROS. 
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER PIC X(34) VALUE  '** VARIAVEIS AUXILIARES **        '.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
                                                                        
       01  WRK-AUX-EMPRESA             PIC  X(004)         VALUE SPACES.
                                                                        
       01  WRK-AUX-MOEDA               PIC  X(005)         VALUE SPACES.
       01  WRK-AUX-MOEDA-NUM REDEFINES WRK-AUX-MOEDA                    
                                       PIC  9(005).                     
                                                                        
       01  WRK-IND                     PIC  9(002)  COMP-3 VALUE ZEROS. 
                                                                        
       01 WRK-E-CD-PAIS-AGEO       PIC X(10)  VALUE SPACES.             
       01 WRK-E-CD-PAIS-AGEO-R  REDEFINES WRK-E-CD-PAIS-AGEO.           
          05 FILLER                PIC X(07).                           
          05 WRK-CD-PAIS-AGEO-E    PIC 9(03).                           
                                                                        
       01 WRK-S-CD-PAIS-AGEO       PIC X(10)  VALUE SPACES.             
       01 WRK-S-CD-PAIS-AGEO-R  REDEFINES WRK-S-CD-PAIS-AGEO.           
          05 FILLER                PIC X(07).                           
          05 WRK-CD-PAIS-AGEO-S    PIC 9(03).                           
                                                                        
       01 WRK-CD-PAIS-SINAL        PIC S9(03) VALUE ZEROS.              
       01 WRK-CD-PAIS-R         REDEFINES WRK-CD-PAIS-SINAL.            
          05 WRK-CD-PAIS           PIC 9(03).                           
                                                                        
      *                                                                 
      *--> "LITERAIS UTILIZADAS PELO PROGRAMA"                          
                                                                        
       01  WRK-SCSB0100                PIC  X(008)  VALUE 'SCSB0100'.   
       01  WRK-FRWK2999                PIC  X(008)  VALUE 'FRWK2999'.   
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER PIC X(34) VALUE  '** FIM DA WORKING SCSB0100 **     '.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
      *================================================================*
       PROCEDURE                       DIVISION.                        
      *================================================================*
                                                                        
      *----------------------------------------------------------------*
      * SECAO PRINCIPAL DE CONTROLE DO FLUXO DO PROGRAMA.              *
      *----------------------------------------------------------------*
       0000-PRINCIPAL                  SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           PERFORM 1000-INICIALIZAR                                     
                                                                        
           PERFORM 3000-PROCESSAR      UNTIL                            
                   WRK-FS-REGVENDA     EQUAL  '10'                      
                                                                        
           PERFORM 5000-FINALIZAR.                                      
                                                                        
      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * PROCEDIMENTOS INICIAIS.                                        *
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE  '1000-INICIALIZAR'    TO  FRWKGHEA-IDEN-PARAGRAFO      
                                                                        
           CALL  'CKRS1000'                                             
           CALL  'CKRS1050'                                             
                                                                        
           INITIALIZE  FRWKGERR-REGISTRO                                
                       FRWKGHEA-REGISTRO                                
                       FRWKGARQ-REGISTRO                                
                       FRWKGMOD-REGISTRO                                
      *                                                                 
      *--> "ABERTURA DOS ARQUIVOS"                                      
      *                                                                 
                                                                        
           SET  ARQ-OPEN               TO  TRUE                         
                                                                        
           OPEN INPUT  REGVENDA                                         
                       OPERACAO                                         
                       ENQUADRA                                         
                       REGEXPOR                                         
                OUTPUT ARQBAIXA                                         
                                                                        
           PERFORM 1100-TESTAR-FILE-STATUS                              
                                                                        
      *                                                                 
      *--> "OBTER DATA E HORA DO SISTEMA"                               
      *                                                                 
                                                                        
           PERFORM 1200-OBTER-DATA-HORA-SISTEMA                         
                                                                        
      *                                                                 
      *--> "OBTER JOBNAME"                                              
      *                                                                 
                                                                        
           PERFORM 1400-OBTER-JOBNAME                                   
                                                                        
      *                                                                 
      *--> "PRIMEIRA LEITURA"                                           
      *                                                                 
                                                                        
           PERFORM 2000-VERIFICAR-VAZIO.                                
                                                                        
      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * FAZER TESTE DE FILE-STATUS PARA OS ARQUIVOS.                   *
      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS         SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           PERFORM 8000-TESTAR-FS-REGVENDA.                             
           PERFORM 8100-TESTAR-FS-OPERACAO.                             
           PERFORM 8200-TESTAR-FS-ENQUADRA.                             
           PERFORM 8300-TESTAR-FS-REGEXPOR.                             
           PERFORM 8400-TESTAR-FS-ARQBAIXA.                             
                                                                        
      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * OBTER DATA E HORA DO SISTEMA ATRAVES DA API CALE2000.          *
      *----------------------------------------------------------------*
       1200-OBTER-DATA-HORA-SISTEMA    SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE  '1200-OBTER-DATA-HORA-SISTEMA'                         
                                       TO  FRWKGHEA-IDEN-PARAGRAFO      
                                                                        
           INITIALIZE  CALE01-ENTRADA                                   
                                                                        
      *--  F3     = CONSULTA                                            
      *--  SF3002 = FORNECER DATA E HORARIO ATUAL                       
                                                                        
           MOVE  'CALE2000'            TO  WRK-MODULO                   
           MOVE  'CALE0001'            TO  CALE01-ID-BLOCO              
           MOVE  1182                  TO  CALE01-TAM-BLOCO             
           MOVE  'F3'                  TO  CALE01-FUNCAO                
           MOVE  'SF3002'              TO  CALE01-SUB-FUNCAO            
                                                                        
           CALL  WRK-MODULO            USING  CALE01-REGISTRO           
                                                                        
           IF CALE01-COD-RETORNO       NOT EQUAL ZEROS                  
              SET   ERRO-MODULO        TO  TRUE                         
              MOVE  WRK-MODULO         TO  FRWKGMOD-NOME-MODULO         
              MOVE  CALE01-COD-RETORNO TO  FRWKGMOD-COD-RETORNO         
              MOVE  CALE01-COD-ERRO    TO  FRWKGMOD-COD-ERRO            
              MOVE  CALE01-COD-MENSAGEM-GMSG                            
                                       TO  FRWKGMOD-COD-MENSAGEM        
              PERFORM 9999-FINALIZAR-ERRO                               
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * OBTER JOBNAME ATRAVES DA BRAD0160.                             *
      *----------------------------------------------------------------*
       1400-OBTER-JOBNAME              SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE   SPACES               TO     WRK-JOBNAME               
                                                                        
           CALL  'BRAD0160'            USING  WRK-JOBNAME               
                                              WRK-B0160-VALOR-FAC.      
                                                                        
      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * VERIFICAR SE ARQUIVOS DE ENTRADA ESTAO VAZIOS.                 *
      *----------------------------------------------------------------*
       2000-VERIFICAR-VAZIO            SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           PERFORM 6000-LER-REGVENDA                                    
                                                                        
           IF WRK-FS-REGVENDA          EQUAL '10'                       
              DISPLAY '***================================***'          
              DISPLAY '*              SCSB0100              *'          
              DISPLAY '*------------------------------------*'          
              DISPLAY '*                                    *'          
              DISPLAY '*     ARQUIVO REGVENDA VAZIO !!!     *'          
              DISPLAY '*                                    *'          
              DISPLAY '***================================***'          
           END-IF                                                       
      *---                                                              
           PERFORM 6100-LER-OPERACAO                                    
                                                                        
           IF WRK-FS-OPERACAO          EQUAL '10'                       
              DISPLAY '***================================***'          
              DISPLAY '*              SCSB0100              *'          
              DISPLAY '*------------------------------------*'          
              DISPLAY '*                                    *'          
              DISPLAY '*     ARQUIVO OPERACAO VAZIO !!!     *'          
              DISPLAY '*                                    *'          
              DISPLAY '***================================***'          
           END-IF                                                       
      *---                                                              
           PERFORM 6200-LER-ENQUADRA                                    
                                                                        
           IF WRK-FS-ENQUADRA          EQUAL '10'                       
              DISPLAY '***================================***'          
              DISPLAY '*              SCSB0100              *'          
              DISPLAY '*------------------------------------*'          
              DISPLAY '*                                    *'          
              DISPLAY '*     ARQUIVO ENQUADRA VAZIO !!!     *'          
              DISPLAY '*                                    *'          
              DISPLAY '***================================***'          
           END-IF.                                                      
      *---                                                              
           PERFORM 6300-LER-REGEXPOR                                    
                                                                        
           IF WRK-FS-REGEXPOR          EQUAL '10'                       
              DISPLAY '***================================***'          
              DISPLAY '*              SCSB0100              *'          
              DISPLAY '*------------------------------------*'          
              DISPLAY '*                                    *'          
              DISPLAY '*     ARQUIVO REGEXPOR VAZIO !!!     *'          
              DISPLAY '*                                    *'          
              DISPLAY '***================================***'          
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * PROCESSAMENTO PRINCIPAL.                                       *
      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           PERFORM 3100-TRATA-RVS.                                      
                                                                        
           IF      WRK-CHV-REGVENDA        EQUAL   WRK-CHV-RVSXOPER     
                   PERFORM 3200-TRATA-OPER UNTIL                        
                   WRK-CHV-REGVENDA NOT    EQUAL   WRK-CHV-RVSXOPER     
           END-IF.                                                      
                                                                        
           PERFORM 6300-LER-REGEXPOR       UNTIL                        
                   WRK-CHV-REGEXPOR        NOT LESS WRK-CHV-REGVENDA    
                                                                        
           IF      WRK-CHV-REGVENDA        EQUAL   WRK-CHV-REGEXPOR     
                   PERFORM 3300-TRATA-REGI UNTIL                        
                   WRK-CHV-REGVENDA NOT    EQUAL   WRK-CHV-REGEXPOR     
           END-IF.                                                      
                                                                        
           PERFORM 6000-LER-REGVENDA.                                   
                                                                        
      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *      TRATAR ARQUIVO RVS                                        *
      *----------------------------------------------------------------*
       3100-TRATA-RVS       SECTION.                                    
      *----------------------------------------------------------------*
           PERFORM 3110-MONTA-PRINCIPAL.                                
           PERFORM 6400-GRAVAR-ARQBAIXA.                                
                                                                        
      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *      MONTA DETALHE PRINCIPAL                                   *
      *----------------------------------------------------------------*
       3110-MONTA-PRINCIPAL    SECTION.                                 
      *----------------------------------------------------------------*
                                                                        
           INITIALIZE                              SCSB51-REG.          
                                                                        
           PERFORM 7100-CONVERTE-MOEDA.                                 
                                                                        
           MOVE    SCSB16-CPAIS            TO      WRK-CD-PAIS-AGEO-E.  
                                                                        
           PERFORM 7200-CONVERTE-PAIS.                                  
                                                                        
           MOVE    '1'                     TO      SCSB51-ID-REG.       
           MOVE    1                       TO      SCSB51-CTPO-LOTE.    
           MOVE    1                       TO      SCSB51-TPO-REG.      
           MOVE    SCSB16-DANO-VDA-RECTA   TO      SCSB51-DANO-RECTA.   
           MOVE    SCSB16-NREG-VDA-RECTA   TO      SCSB51-NREG-RECTA.   
                                                                        
           IF      SCSB16-CPSSOA-JURID     GREATER ZEROS                
                   PERFORM 7000-CALL-UORG2397                           
                   MOVE UORGWB97-S-CEMPR-SAP(1:4)                       
                                           TO      SCSB51-EMPRESA       
                                                   WRK-AUX-EMPRESA      
           ELSE                                                         
                   MOVE SCSB16-CFIRMA-EXTER                             
                                           TO      SCSB51-EMPRESA       
                                                   WRK-AUX-EMPRESA      
           END-IF.                                                      
                                                                        
           MOVE    SCSB16-ICOMPR-EXTER     TO      SCSB51-NOME.         
           MOVE    SCSB16-EEXTER-COMPR     TO      SCSB51-ENDERECO.     
************OVE    SCSB16-CPAIS            TO      SCSB51-CD-PAIS.      
           MOVE    WRK-CD-PAIS-AGEO-S      TO      SCSB51-CD-PAIS.      
           MOVE    SCSB16-CFSCAL-OPER-EXTER                             
                                           TO      SCSB51-NIF.          
************OVE    SCSB16-CINDCD-ECONM-MOEDA                            
************                               TO      SCSB51-CD-MOEDA.     
           MOVE    WRK-AUX-MOEDA-NUM(1:WRK-IND)                         
                                           TO      SCSB51-CD-MOEDA.     
           MOVE    WCOMPL-COMPR-VDA-TEXT   TO      SCSB51-INF-COMPLEM.  
                                                                        
      *----------------------------------------------------------------*
       3110-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *      TRATAR ARQUIVO DE OPERACOES                               *
      *----------------------------------------------------------------*
       3200-TRATA-OPER      SECTION.                                    
      *----------------------------------------------------------------*
                                                                        
           PERFORM 3210-MONTA-OPERACAO.                                 
           PERFORM 6400-GRAVAR-ARQBAIXA.                                
                                                                        
           PERFORM 6200-LER-ENQUADRA       UNTIL                        
                   WRK-CHV-ENQUADRA NOT    LESS    WRK-CHV-OPERACAO.    
                                                                        
           IF      WRK-CHV-OPERACAO        EQUAL   WRK-CHV-ENQUADRA     
                   PERFORM 3220-MONTA-ENQU UNTIL                        
                   WRK-CHV-OPERACAO NOT    EQUAL   WRK-CHV-ENQUADRA     
           END-IF.                                                      
                                                                        
           PERFORM 6100-LER-OPERACAO.                                   
                                                                        
      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *      MONTA DETALHE OPERACAO                                    *
      *----------------------------------------------------------------*
       3210-MONTA-OPERACAO     SECTION.                                 
      *----------------------------------------------------------------*
                                                                        
           INITIALIZE                              SCSB51-REG.          
                                                                        
           MOVE    SCSB10-CPAIS            TO      WRK-CD-PAIS-AGEO-E.  
                                                                        
           PERFORM 7200-CONVERTE-PAIS.                                  
                                                                        
           MOVE    '1'                     TO      SCSB51-ID-REG.       
           MOVE    1                       TO      SCSB51-CTPO-LOTE.    
           MOVE    WRK-AUX-EMPRESA         TO      SCSB51-EMPRESA.      
           MOVE    2                       TO      SCSB51-TPO-REG.      
           MOVE    SCSB10-DANO-VDA-RECTA   TO      SCSB51-DANO-RECTA.   
           MOVE    SCSB10-NREG-VDA-RECTA   TO      SCSB51-NREG-RECTA.   
           MOVE    SCSB10-COPER-VDA-EXTER  TO      SCSB51-COPER.        
           MOVE    SCSB10-CSERVC-VAR-PATRM TO      SCSB51-CD-NBS.       
************OVE    SCSB10-CPAIS            TO      SCSB51-CD-PAIS-DESTNO
           MOVE    WRK-CD-PAIS-AGEO-S      TO      SCSB51-CD-PAIS-DESTNO
           MOVE    SCSB10-CTPO-OPER-EXTER  TO      SCSB51-CD-MODO-PREST.
           MOVE    SCSB10-DINIC-OPER-EXTER TO      SCSB51-DT-INICIO.    
           MOVE    SCSB10-DFIM-OPER-EXTER  TO      SCSB51-DT-CONCLUSAO. 
           MOVE    SCSB10-VCOMPR-VDA-EXTER TO      SCSB51-VLR-OPER.     
                                                                        
      *----------------------------------------------------------------*
       3210-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *      MONTA DETALHE ENQUADRAMENTO                               *
      *----------------------------------------------------------------*
       3220-MONTA-ENQU         SECTION.                                 
      *----------------------------------------------------------------*
                                                                        
           INITIALIZE                              SCSB51-REG.          
                                                                        
           MOVE    '1'                     TO      SCSB51-ID-REG.       
           MOVE    1                       TO      SCSB51-CTPO-LOTE.    
           MOVE    WRK-AUX-EMPRESA         TO      SCSB51-EMPRESA.      
           MOVE    3                       TO      SCSB51-TPO-REG.      
           MOVE    SCSB03-DANO-VDA-RECTA   TO      SCSB51-DANO-RECTA.   
           MOVE    SCSB03-NREG-VDA-RECTA   TO      SCSB51-NREG-RECTA.   
           MOVE    SCSB03-COPER-VDA-EXTER  TO      SCSB51-COPER-ENQUA.  
           MOVE    SCSB03-CENQUA-VAR-PATRM TO      SCSB51-CD-ENQUA.     
           MOVE    SCSB03-CREG-CREDT-EXTER TO      SCSB51-RC.           
                                                                        
           IF      SCSB03-CTPO-REG-EXTER   EQUAL   1                    
                   MOVE 'I'                TO      SCSB51-ACAO-ENQUA    
            ELSE                                                        
                   MOVE 'E'                TO      SCSB51-ACAO-ENQUA    
           END-IF.                                                      
                                                                        
           PERFORM 6400-GRAVAR-ARQBAIXA.                                
                                                                        
           PERFORM 6200-LER-ENQUADRA.                                   
                                                                        
      *----------------------------------------------------------------*
       3220-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *      TRATAR ARQUIVO DE REGISTROS                               *
      *----------------------------------------------------------------*
       3300-TRATA-REGI      SECTION.                                    
      *----------------------------------------------------------------*
                                                                        
           INITIALIZE                              SCSB51-REG.          
                                                                        
           MOVE    '1'                     TO      SCSB51-ID-REG.       
           MOVE    1                       TO      SCSB51-CTPO-LOTE.    
           MOVE    WRK-AUX-EMPRESA         TO      SCSB51-EMPRESA.      
           MOVE    4                       TO      SCSB51-TPO-REG.      
           MOVE    SCSB16-DANO-VDA-RECTA   TO      SCSB51-DANO-RECTA.   
           MOVE    SCSB16-NREG-VDA-RECTA   TO      SCSB51-NREG-RECTA.   
           MOVE    SCSB14-CREG-EXPOR-SERVC TO      SCSB51-CD-RE.        
                                                                        
ANDER      IF      SCSB14-CTPO-REG-EXTER   EQUAL   1                    
                   MOVE 'I'                TO      SCSB51-ACAO-RE       
            ELSE                                                        
                   MOVE 'E'                TO      SCSB51-ACAO-RE       
ANDER      END-IF.                                                      
                                                                        
           PERFORM 6400-GRAVAR-ARQBAIXA.                                
                                                                        
           PERFORM 6300-LER-REGEXPOR.                                   
                                                                        
      *----------------------------------------------------------------*
       3300-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * PROCEDIMENTOS FINAIS DO PROGRAMA.                              *
      *----------------------------------------------------------------*
       5000-FINALIZAR                  SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE  '5000-FINALIZAR'      TO  FRWKGHEA-IDEN-PARAGRAFO      
                                                                        
      *                                                                 
      *--> "APRESENTAR O RESUMO DA EXECUCAO"                            
      *                                                                 
                                                                        
           PERFORM 5100-EXIBIR-ESTATISTICA                              
                                                                        
      *                                                                 
      *--> "FECHAR ARQUIVOS"                                            
      *                                                                 
                                                                        
           SET  ARQ-CLOSE              TO  TRUE                         
                                                                        
           CLOSE  REGVENDA                                              
                  OPERACAO                                              
                  ENQUADRA                                              
                  REGEXPOR                                              
                  ARQBAIXA.                                             
                                                                        
           PERFORM 1100-TESTAR-FILE-STATUS                              
                                                                        
           STOP RUN.                                                    
                                                                        
      *----------------------------------------------------------------*
       5000-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * APRESENTAR O RESUMO DA EXECUCAO.                               *
      *----------------------------------------------------------------*
       5100-EXIBIR-ESTATISTICA         SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE    CALE01-DT-GREGO-B-DDMMAAAA    TO  WRK-DATAPROC       
                                                     WRK-DATAMOV        
           MOVE    CALE01-HH-2PONTOS-HHMMSS      TO  WRK-HORAPROC       
                                                                        
           DISPLAY WRK-DISP1                                            
           DISPLAY WRK-DISP2                                            
           DISPLAY WRK-DISP3                                            
           DISPLAY WRK-DISP4                                            
           DISPLAY WRK-DISP5                                            
                                                                        
           MOVE    WRK-SCSB0100                  TO  WRK-PROGNAME       
                                                                        
           DISPLAY WRK-DISP6                                            
           DISPLAY WRK-DISP3                                            
           DISPLAY WRK-DISP7                                            
           DISPLAY WRK-DISP8                                            
                                                                        
           MOVE   'REGVENDA'                     TO  WRK-DDNAME         
           MOVE   'I'                            TO  WRK-I-O            
           MOVE   'TOTAL LIDOS                '  TO  WRK-DESCARQ        
           MOVE    ACU-LIDOS-REGVENDA            TO  WRK-QTDEARQ        
           DISPLAY WRK-DISP9                                            
                                                                        
           MOVE   'OPERACAO'                     TO  WRK-DDNAME         
           MOVE   'I'                            TO  WRK-I-O            
           MOVE   'TOTAL LIDOS                '  TO  WRK-DESCARQ        
           MOVE    ACU-LIDOS-OPERACAO            TO  WRK-QTDEARQ        
           DISPLAY WRK-DISP9                                            
                                                                        
           MOVE   'ENQUADRA'                     TO  WRK-DDNAME         
           MOVE   'I'                            TO  WRK-I-O            
           MOVE   'TOTAL LIDOS                '  TO  WRK-DESCARQ        
           MOVE    ACU-LIDOS-ENQUADRA            TO  WRK-QTDEARQ        
           DISPLAY WRK-DISP9                                            
                                                                        
           MOVE   'REGEXPOR'                     TO  WRK-DDNAME         
           MOVE   'I'                            TO  WRK-I-O            
           MOVE   'TOTAL LIDOS                '  TO  WRK-DESCARQ        
           MOVE    ACU-LIDOS-REGEXPOR            TO  WRK-QTDEARQ        
           DISPLAY WRK-DISP9                                            
                                                                        
           MOVE   'ARQBAIXA'                     TO  WRK-DDNAME         
           MOVE   'O'                            TO  WRK-I-O            
           MOVE   'TOTAL GRAVADOS             '  TO  WRK-DESCARQ        
           MOVE    ACU-GRAVS-ARQBAIXA            TO  WRK-QTDEARQ        
           DISPLAY WRK-DISP9                                            
                                                                        
           DISPLAY WRK-DISP1.                                           
                                                                        
      *----------------------------------------------------------------*
       5100-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * FAZER A LEITURA DO ARQUIVO REGVENDA.                           *
      *----------------------------------------------------------------*
       6000-LER-REGVENDA               SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE  '6000-LER-REGVENDA'   TO  FRWKGHEA-IDEN-PARAGRAFO      
                                                                        
           SET  ARQ-READ               TO   TRUE                        
           READ REGVENDA               INTO SCSBB016-REG                
                                                                        
           IF WRK-FS-REGVENDA          NOT EQUAL '10'                   
              PERFORM 8000-TESTAR-FS-REGVENDA                           
              ADD   1                  TO  ACU-LIDOS-REGVENDA           
              MOVE  SCSB16-DANO-VDA-RECTA                               
                                       TO  WRK-CHV-DANO-REGVENDA        
              MOVE  SCSB16-NREG-VDA-RECTA                               
                                       TO  WRK-CHV-NREG-REGVENDA        
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       6000-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * FAZER A LEITURA DO ARQUIVO OPERACAO.                           *
      *----------------------------------------------------------------*
       6100-LER-OPERACAO               SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE  '6100-LER-OPERACAO'   TO  FRWKGHEA-IDEN-PARAGRAFO      
                                                                        
           SET  ARQ-READ               TO   TRUE                        
           READ OPERACAO               INTO SCSBB010-REG                
                                                                        
           IF WRK-FS-OPERACAO          NOT EQUAL '10'                   
              PERFORM 8100-TESTAR-FS-OPERACAO                           
              ADD   1                  TO  ACU-LIDOS-OPERACAO           
              MOVE  SCSB10-DANO-VDA-RECTA                               
                                       TO  WRK-CHV-DANO-OPERACAO        
              MOVE  SCSB10-NREG-VDA-RECTA                               
                                       TO  WRK-CHV-NREG-OPERACAO        
              MOVE  SCSB10-COPER-VDA-EXTER                              
                                       TO  WRK-CHV-COPE-OPERACAO        
           ELSE                                                         
              MOVE  HIGH-VALUES        TO  WRK-CHV-OPERACAO             
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       6100-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * FAZER A LEITURA DO ARQUIVO ENQUADRA.                           *
      *----------------------------------------------------------------*
       6200-LER-ENQUADRA               SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE  '6200-LER-ENQUADRA'   TO  FRWKGHEA-IDEN-PARAGRAFO      
                                                                        
           SET  ARQ-READ               TO   TRUE                        
           READ ENQUADRA               INTO SCSBB003-REG                
                                                                        
           IF WRK-FS-ENQUADRA          NOT EQUAL '10'                   
              PERFORM 8200-TESTAR-FS-ENQUADRA                           
              ADD   1                  TO  ACU-LIDOS-ENQUADRA           
              MOVE  SCSB03-DANO-VDA-RECTA                               
                                       TO  WRK-CHV-DANO-ENQUADRA        
              MOVE  SCSB03-NREG-VDA-RECTA                               
                                       TO  WRK-CHV-NREG-ENQUADRA        
              MOVE  SCSB03-COPER-VDA-EXTER                              
                                       TO  WRK-CHV-COPE-ENQUADRA        
           ELSE                                                         
              MOVE  HIGH-VALUES        TO  WRK-CHV-ENQUADRA             
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       6200-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * FAZER A LEITURA DO ARQUIVO REGEXPOR.                           *
      *----------------------------------------------------------------*
       6300-LER-REGEXPOR               SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE  '6300-LER-REGEXPOR'   TO  FRWKGHEA-IDEN-PARAGRAFO      
                                                                        
           SET  ARQ-READ               TO   TRUE                        
           READ REGEXPOR               INTO SCSBB014-REG                
                                                                        
           IF WRK-FS-REGEXPOR          NOT EQUAL '10'                   
              PERFORM 8300-TESTAR-FS-REGEXPOR                           
              MOVE  SCSB14-DANO-VDA-RECTA                               
                                       TO  WRK-CHV-DANO-REGEXPOR        
              MOVE  SCSB14-NREG-VDA-RECTA                               
                                       TO  WRK-CHV-NREG-REGEXPOR        
              ADD   1                  TO  ACU-LIDOS-REGEXPOR           
           ELSE                                                         
              MOVE  HIGH-VALUES        TO  WRK-CHV-REGEXPOR             
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       6300-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * GRAVAR ARQUIVO DE SAIDA ARQBAIXA.                              *
      *----------------------------------------------------------------*
       6400-GRAVAR-ARQBAIXA            SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE '6400-GRAVAR-ARQBAIXA' TO  FRWKGHEA-IDEN-PARAGRAFO      
                                                                        
           SET   ARQ-WRITE             TO  TRUE                         
                                                                        
           WRITE FD-ARQBAIXA           FROM SCSB51-REG.                 
                                                                        
           PERFORM 8400-TESTAR-FS-ARQBAIXA                              
                                                                        
           ADD   1                     TO  ACU-GRAVS-ARQBAIXA.          
                                                                        
      *----------------------------------------------------------------*
       6400-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * BUSCA ENMPRESA SAP                                             *
      *----------------------------------------------------------------*
       7000-CALL-UORG2397              SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE  '7000-CALL-UORG2397'      TO                           
                                           FRWKGHEA-IDEN-PARAGRAFO.     
                                                                        
           INITIALIZE  UORGWB97-REGISTRO.                               
                                                                        
           MOVE    'D'                     TO      UORGWB97-E-AMBIENTE  
           MOVE    SCSB16-CPSSOA-JURID     TO                           
                                           UORGWB97-E-CPSSOA-JURID.     
           MOVE    'UORG2397'              TO      WRK-MODULO           
                                                                        
           CALL    WRK-MODULO              USING   WRK-AREA-UORG2397    
                                                                        
           IF      UORGWB97-COD-RETORNO  NOT EQUAL ZEROS                
                   SET  ERRO-MODULO        TO      TRUE                 
                   MOVE WRK-MODULO         TO      FRWKGMOD-NOME-MODULO 
                   MOVE UORGWB97-COD-RETORNO                            
                                           TO      FRWKGMOD-COD-RETORNO 
                   PERFORM 9999-FINALIZAR-ERRO                          
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       7000-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       7100-CONVERTE-MOEDA             SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE  '7100-CONVERTE-MOEDA'     TO                           
                                           FRWKGHEA-IDEN-PARAGRAFO.     
                                                                        
           INITIALIZE  WRK-AREA-INECWPHI.                               
                                                                        
           MOVE    SCSB16-CINDCD-ECONM-MOEDA                            
                                           TO     INECWPHI-CINDCD-ECONM.
                                                                        
           MOVE    'INEC2PPI'              TO      WRK-MODULO.          
                                                                        
           CALL    WRK-MODULO              USING   WRK-AREA-INECWPHI.   
                                                                        
           IF      INECWPHI-COD-RETORNO  NOT EQUAL ZEROS                
                   SET  ERRO-MODULO        TO      TRUE                 
                   MOVE WRK-MODULO         TO      FRWKGMOD-NOME-MODULO 
                   MOVE INECWPHI-COD-RETORNO                            
                                           TO      FRWKGMOD-COD-RETORNO 
                   PERFORM 9999-FINALIZAR-ERRO                          
           END-IF.                                                      
                                                                        
           MOVE INECWPHI-CINDCD-FONTE-CONS(1:5)                         
                                           TO      WRK-AUX-MOEDA.       
                                                                        
           PERFORM     VARYING WRK-IND FROM 1 BY 1                      
                       UNTIL WRK-IND GREATER 5  OR                      
                       (WRK-AUX-MOEDA(1:WRK-IND) IS NOT NUMERIC)        
           END-PERFORM.                                                 
                                                                        
           SUBTRACT  1   FROM WRK-IND.                                  
                                                                        
      *----------------------------------------------------------------*
       7100-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       7200-CONVERTE-PAIS              SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE  '7200-CONVERTE-PAIS'      TO                           
                                           FRWKGHEA-IDEN-PARAGRAFO.     
                                                                        
           MOVE    'AGEO2000'              TO      WRK-MODULO.          
                                                                        
           MOVE 'SF1007'                   TO AGEOWAAI-FUNCAO.          
           MOVE 001                        TO AGEOWAAI-VERSAO.          
           MOVE 'T'                        TO AGEOWAAI-TIP-TERRITORIO.  
           MOVE 'C'                        TO AGEOWAAI-IND-CLAS-LIST.   
           MOVE WRK-E-CD-PAIS-AGEO         TO AGEOWAAI-COD-PAIS-E.      
           MOVE 61                         TO AGEOWAAI-CIRECP-SAIDA.    
           MOVE 001                        TO AGEOWAAI-IDIOMA-SAIDA.    
           MOVE 01                         TO AGEOWAAI-QTDE-SOLIC.      
           MOVE 'I'                        TO AGEOWAAI-IND-PAGINA.      
                                                                        
           CALL    WRK-MODULO              USING   AGEOWAAI.            
                                                                        
           IF AGEOWAAI-COD-RET       NOT EQUAL ZEROS AND 01 AND 08      
              PERFORM 9999-FINALIZAR-ERRO                               
           END-IF.                                                      
                                                                        
           IF AGEOWAAI-COD-RET       EQUAL 08                           
              PERFORM 7300-SELECT-SCSBB011                              
           ELSE                                                         
              MOVE AGEOWAAI-CD-PAIS(1) TO WRK-S-CD-PAIS-AGEO            
              MOVE SPACES              TO AGEOWAAI-CTRL-PAG             
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       7200-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       7300-SELECT-SCSBB011               SECTION.                      
      *----------------------------------------------------------------*
                                                                        
           MOVE '7300-SELECT-SCSBB011'    TO FRWKGHEA-IDEN-PARAGRAFO    
                                                                        
           MOVE WRK-CD-PAIS-AGEO-E        TO CPAIS                      
                                                                        
           EXEC SQL                                                     
                SELECT CPAIS,                                           
                       CPAIS_RECTA_FEDRL,                               
                       IPAIS                                            
              INTO                                                      
               :SCSBB011.CPAIS,                                         
               :SCSBB011.CPAIS-RECTA-FEDRL,                             
               :SCSBB011.IPAIS                                          
              FROM DB2PRD.TPAIS_GEOGR_RECTA                             
              WHERE CPAIS        = :SCSBB011.CPAIS                      
           END-EXEC.                                                    
           IF                                                           
               (SQLCODE        NOT     EQUAL   ZEROS) OR                
               (SQLWARN0               EQUAL   'W')                     
                DISPLAY 'CPAIS ' CPAIS   OF      SCSBB011               
                SET ERRO-DB2             TO      TRUE                   
                SET DB2-UPDATE           TO      TRUE                   
                MOVE 'TPAIS_GEOGR_RECTA' TO      FRWKGDB2-NOME-TABELA   
                PERFORM 9999-FINALIZAR-ERRO                             
           END-IF.                                                      
                                                                        
           MOVE CPAIS-RECTA-FEDRL          TO WRK-CD-PAIS-SINAL.        
           MOVE WRK-CD-PAIS                TO WRK-CD-PAIS-AGEO-S.       
                                                                        
      *----------------------------------------------------------------*
       7300-99-FIM.                       EXIT.                         
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * TESTAR FILE-STATUS DO ARQUIVO DE ENTRADA REGVENDA.             *
      *----------------------------------------------------------------*
       8000-TESTAR-FS-REGVENDA         SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           IF WRK-FS-REGVENDA          NOT EQUAL '00'                   
              SET   ERRO-ARQUIVO       TO  TRUE                         
              MOVE  WRK-FS-REGVENDA    TO  FRWKGARQ-FILE-STATUS         
              MOVE  'REGVENDA'         TO  FRWKGARQ-NOME-ARQUIVO        
              PERFORM 9999-FINALIZAR-ERRO                               
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       8000-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * TESTAR FILE-STATUS DO ARQUIVO DE ENTRADA OPERACAO.             *
      *----------------------------------------------------------------*
       8100-TESTAR-FS-OPERACAO         SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           IF WRK-FS-OPERACAO          NOT EQUAL '00'                   
              SET   ERRO-ARQUIVO       TO  TRUE                         
              MOVE  WRK-FS-OPERACAO    TO  FRWKGARQ-FILE-STATUS         
              MOVE  'OPERACAO'         TO  FRWKGARQ-NOME-ARQUIVO        
              PERFORM 9999-FINALIZAR-ERRO                               
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       8100-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * TESTAR FILE-STATUS DO ARQUIVO DE ENTRADA ENQUADRA.             *
      *----------------------------------------------------------------*
       8200-TESTAR-FS-ENQUADRA         SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           IF WRK-FS-ENQUADRA          NOT EQUAL '00'                   
              SET   ERRO-ARQUIVO       TO  TRUE                         
              MOVE  WRK-FS-ENQUADRA    TO  FRWKGARQ-FILE-STATUS         
              MOVE  'ENQUADRA'         TO  FRWKGARQ-NOME-ARQUIVO        
              PERFORM 9999-FINALIZAR-ERRO                               
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       8200-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * TESTAR FILE-STATUS DO ARQUIVO DE SAIDA REGEXPOR.               *
      *----------------------------------------------------------------*
       8300-TESTAR-FS-REGEXPOR         SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           IF WRK-FS-REGEXPOR          NOT EQUAL '00'                   
              SET   ERRO-ARQUIVO       TO  TRUE                         
              MOVE  WRK-FS-REGEXPOR    TO  FRWKGARQ-FILE-STATUS         
              MOVE  'REGEXPOR'         TO  FRWKGARQ-NOME-ARQUIVO        
              PERFORM 9999-FINALIZAR-ERRO                               
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       8300-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * TESTAR FILE-STATUS DO ARQUIVO DE SAIDA ARQBAIXA.               *
      *----------------------------------------------------------------*
       8400-TESTAR-FS-ARQBAIXA         SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           IF WRK-FS-ARQBAIXA          NOT EQUAL '00'                   
              SET   ERRO-ARQUIVO       TO  TRUE                         
              MOVE  WRK-FS-ARQBAIXA    TO  FRWKGARQ-FILE-STATUS         
              MOVE  'ARQBAIXA'         TO  FRWKGARQ-NOME-ARQUIVO        
              PERFORM 9999-FINALIZAR-ERRO                               
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       8400-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
      * CHAMA O MODULO FRWK1999 - GRAVA LOG DE ERROS                   *
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
      *----------------------------------------------------------------*
      * FINALIZAR PROGRAMA COM ABEND.                                  *
      *----------------------------------------------------------------*
       9999-FINALIZAR-ERRO             SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE  12                    TO  FRWKGERR-COD-RETORNO         
           MOVE  WRK-SCSB0100          TO  FRWKGHEA-NOME-PROGRAMA       
                                                                        
           EVALUATE FRWKGHEA-TIPO-ERRO                                  
                                                                        
             WHEN  'AR'                                                 
                SET   FRWKGERR-FSTATUS-INCOR    TO  TRUE                
                MOVE  FRWKGARQ-TAM-LAYOUT       TO  FRWKGHEA-TAM-DADOS  
                MOVE  WRK-AREA-ERRO-ARQUIVO     TO  WRK-BLOCO-INFO-ERRO 
                                                                        
             WHEN  'MO'                                                 
                SET   FRWKGERR-COD-RETOR-INCOR  TO  TRUE                
                MOVE  FRWKGMOD-TAM-LAYOUT       TO  FRWKGHEA-TAM-DADOS  
                MOVE  WRK-AREA-ERRO-MODULO      TO  WRK-BLOCO-INFO-ERRO 
                                                                        
             WHEN  'LI'                                                 
                SET   FRWKGERR-TIPO-ERRO-INCOR  TO  TRUE                
                MOVE  FRWKGLIV-TAM-LAYOUT       TO  FRWKGHEA-TAM-DADOS  
                MOVE  WRK-AREA-ERRO-LIVRE       TO  WRK-BLOCO-INFO-ERRO 
                                                                        
             WHEN  'DB'                                                 
                MOVE  SQLSTATE                  TO  FRWKGDB2-SQLSTATE   
                MOVE  SQLCODE                   TO  FRWKGDB2-SQLCODE    
                SET   FRWKGERR-SQLCODE-INCOR    TO  TRUE                
                MOVE  FRWKGDB2-TAM-LAYOUT       TO  FRWKGHEA-TAM-DADOS  
                MOVE  WRK-AREA-ERRO-DB2         TO  WRK-BLOCO-INFO-ERRO 
           END-EVALUATE                                                 
                                                                        
           CALL  WRK-FRWK2999          USING  WRK-AREA-ERRO             
                                                                        
           CALL  'BRAD0450'            USING  WRK-B0450-ABEND           
                                              WRK-B0450-DUMP.           
                                                                        
      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*

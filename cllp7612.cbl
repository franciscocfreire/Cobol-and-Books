      *================================================================*
       IDENTIFICATION                  DIVISION.                        
      *================================================================*
                                                                        
       PROGRAM-ID. CLLP7612.                                            
       AUTHOR.     PATRIC ZAGO.                                         
      *================================================================*
      *                    C P M  -  S I S T E M A S                   *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   CLLP7612                                    *
      *    PROGRAMADOR.:   PATRIC ZAGO               - CPM/A           *
      *    ANALISTA....:   CIDINHA                   - DDS/GRUPO 70    *
      *    DATA........:   08/12/2005                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   GERAR ARQUIVO DE SAIDA APENAS COM OS REGIS- *
      *      TROS QUE  NAO  FORAM  ENCONTRADAS  RESTRICOES  NA  TABELA *
      *      LPCLB037.                                                 *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                    DDNAME                    INCLUDE/BOOK      *
      *                    ARQAVISO                    --------        *
      *                    TABELA                      --------        *
      *                    ARQSAIDA                    I#CLLPFD        *
      *                    ARQBLOQ                     I#CLLPFD        *
      *                    RELINCO                     --------        *
      *                    RELATO                      --------        *
      *----------------------------------------------------------------*
      *    INC'S.......:                                               *
      *    I#LPCLUF - TABELA P/CONSISTENCIA DE UNIDADES DA FEDERACAO   *
      *    POL7100C - AREA PARA TRATAMENTO DE ERRO PELA POOL7100       *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    POOL7100 - TRATAMENTO DE ERRO QUANDO PROGRAMA INVALIDO      *
      *    POOL7600 - OBTEM DATA E HORA CORRENTE                       *
      *================================================================*
                                                                        
BRQ141*----------------------------------------------------------------*
BRQ141*    ANALISTA    : TALLES AUGUSTO - BRQ                          *
BRQ141*    DATA        : 09/2012                                       *
BRQ141*    OBJETIVO    : PROJETO CARTEIRA ALFANUMERICA - BRQ141        *
BRQ141*                  CONVERSAO DE CARTEIRAS.                       *
BRQ141*----------------------------------------------------------------*
                                                                        
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
                                                                        
           SELECT ARQAVISO ASSIGN      TO UT-S-ARQAVISO                 
           FILE STATUS                 IS WRK-FS-ARQAVISO.              
                                                                        
           SELECT TABELA   ASSIGN      TO UT-S-TABELA                   
           FILE STATUS                 IS WRK-FS-TABELA.                
                                                                        
           SELECT ARQSAIDA ASSIGN      TO UT-S-ARQSAIDA                 
           FILE STATUS                 IS WRK-FS-ARQSAIDA.              
                                                                        
           SELECT ARQBLOQ  ASSIGN      TO UT-S-ARQBLOQ                  
           FILE STATUS                 IS WRK-FS-ARQBLOQ.               
                                                                        
           SELECT BLQREST  ASSIGN      TO UT-S-BLQREST                  
           FILE STATUS                 IS WRK-FS-BLQREST.               
                                                                        
           SELECT RELINCO  ASSIGN      TO UT-S-RELINCO                  
           FILE STATUS                 IS WRK-FS-RELINCO.               
                                                                        
           SELECT RELATO   ASSIGN      TO UT-S-RELATO                   
           FILE STATUS                 IS WRK-FS-RELATO.                
                                                                        
      *================================================================*
       DATA                            DIVISION.                        
      *================================================================*
                                                                        
      *----------------------------------------------------------------*
       FILE                            SECTION.                         
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *    INPUT:  ARQUIVO DE AVISOS DE ENTRADA                        *
      *            ORG. SEQUENCIAL     -   LRECL   =   692             *
      *----------------------------------------------------------------*
                                                                        
       FD  ARQAVISO                                                     
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
           COPY 'I#CLLPFD'.
                                                                        
           02  ENT-UF-ENDER-IMOVEL     PIC  X(002).                     
                                                                        
      *----------------------------------------------------------------*
      *    INPUT:  ARQUIVO DE ENTRADA                                  *
      *            ORG. SEQUENCIAL     -   LRECL   =   043             *
      *----------------------------------------------------------------*
                                                                        
       FD  TABELA                                                       
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  REG-TABELA.                                                  
           03  B037-CCGC-CPF           PIC S9(009) COMP-3.              
           03  B037-CFLIAL-CGC         PIC S9(005) COMP-3.              
           03  B037-CCTRL-CPF-CGC      PIC S9(002) COMP-3.              
           03  B037-CREST-BLOQ-OPER    PIC S9(001) COMP-3.              
           03  B037-CJUNC-DEPDC-BDSCO  PIC S9(005) COMP-3.              
           03  B037-CCTA-CORR          PIC S9(007) COMP-3.              
           03  B037-CTPO-NATUZ-OPER    PIC S9(003) COMP-3.              
           03  B037-CCART              PIC  X(003).                     
           03  B037-CSGL-UF            PIC  X(002).                     
           03  B037-CINDCD-AVISO-COBR  PIC  X(001).                     
           03  B037-CINDCD-IMPED-REST  PIC  X(001).                     
           03  B037-CINDCD-SERASA-SPC  PIC  X(001).                     
           03  B037-DULT-ATULZ         PIC  X(010).                     
           03  B037-CFUNC-BDSCO        PIC S9(009) COMP-3.              
                                                                        
      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE AVISOS DE SAIDA                          *
      *            ORG. SEQUENCIAL     -   LRECL   =   690             *
      *----------------------------------------------------------------*
                                                                        
       FD  ARQSAIDA                                                     
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  REG-ARQSAIDA                PIC  X(690).
                                                                        
      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE AVISOS BLOQUEADOS                        *
      *            ORG. SEQUENCIAL     -   LRECL   =   690             *
      *----------------------------------------------------------------*
                                                                        
       FD  ARQBLOQ                                                      
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  FD-ARQBLOQ                 PIC  X(692).
                                                                        
      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE AVISOS BLOQUEADOS                        *
      *            ORG. SEQUENCIAL     -   LRECL   =   690             *
      *----------------------------------------------------------------*
                                                                        
       FD  BLQREST                                                      
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  FD-BLQRESTR.                                                 
           05  FD-EMPRESA   PIC 9(05)    COMP-3.                        
           05  FD-AGENCIA   PIC 9(05)    COMP-3.                        
           05  FD-CONTA     PIC 9(07)    COMP-3.                        
           05  FD-CARTEIRA  PIC X(03).                                  
           05  FD-CONTRATO  PIC 9(07)    COMP-3.                        
           05  FD-IND-BLOQ  PIC 9(02)    COMP-3.                        
           05  FD-INDICADOR PIC X(01).                                  
           05  FD-CPF       PIC 9(09)    COMP-3.                        
           05  FD-FILIAL    PIC 9(05)    COMP-3.                        
           05  FD-CTRL      PIC 9(03)    COMP-3.                        
           05  FILLER       PIC X(10).                                  
                                                                        
      *----------------------------------------------------------------*
      *    OUTPUT: RELATORIO DE INCONSISTENCIAS                        *
      *            ORG. SEQUENCIAL     -   LRECL   =   133             *
      *----------------------------------------------------------------*
                                                                        
       FD  RELINCO                                                      
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  REG-RELINCO                 PIC  X(133).                     
                                                                        
      *----------------------------------------------------------------*
      *    OUTPUT: RELATORIO DE ENCONTRADOS                            *
      *            ORG. SEQUENCIAL     -   LRECL   =   133             *
      *----------------------------------------------------------------*
                                                                        
       FD  RELATO                                                       
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  REG-RELATO                  PIC  X(133).                     
                                                                        
      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.                         
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE        
           '*** CLLP7612 - INICIO DA AREA DE WORKING ***'.              
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE        
           '*** ACUMULADORES ***'.                                      
      *----------------------------------------------------------------*
                                                                        
       77  ACU-LIDOS-ARQAVISO          PIC  9(009) COMP-3  VALUE ZEROS. 
       77  ACU-LIDOS-TABELA            PIC  9(009) COMP-3  VALUE ZEROS. 
       77  ACU-GRAVA-ARQSAIDA          PIC  9(009) COMP-3  VALUE ZEROS. 
       77  ACU-GRAVA-ARQBLOQ           PIC  9(009) COMP-3  VALUE ZEROS. 
       77  ACU-GRAVA-BLQREST           PIC  9(009) COMP-3  VALUE ZEROS. 
       77  ACU-GRAVA-RELINCO           PIC  9(009) COMP-3  VALUE ZEROS. 
       77  ACU-GRAVA-RELATO            PIC  9(009) COMP-3  VALUE ZEROS. 
       77  ACU-EXCLUIDOS               PIC  9(009) COMP-3  VALUE ZEROS. 
                                                                        
       77  ACU-LINHAS-RELINCO          PIC  9(003) COMP-3  VALUE 99.    
       77  ACU-LINHAS-RELATO           PIC  9(003) COMP-3  VALUE 99.    
       77  ACU-PAGINA-RELINCO          PIC  9(003) COMP-3  VALUE ZEROS. 
       77  ACU-PAGINA-RELATO           PIC  9(003) COMP-3  VALUE ZEROS. 
                                                                        
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE        
           '*** AREA DE FILE STATUS ***'.                               
      *----------------------------------------------------------------*
                                                                        
       77  WRK-FS-ARQAVISO             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-TABELA               PIC  X(002)         VALUE SPACES.
       77  WRK-FS-ARQSAIDA             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-ARQBLOQ              PIC  X(002)         VALUE SPACES.
       77  WRK-FS-BLQREST              PIC  X(002)         VALUE SPACES.
       77  WRK-FS-RELINCO              PIC  X(002)         VALUE SPACES.
       77  WRK-FS-RELATO               PIC  X(002)         VALUE SPACES.
                                                                        
       77  WRK-ABERTURA                PIC  X(013)         VALUE        
           ' NA ABERTURA '.                                             
       77  WRK-LEITURA                 PIC  X(013)         VALUE        
           ' NA  LEITURA '.                                             
       77  WRK-GRAVACAO                PIC  X(013)         VALUE        
           ' NA GRAVACAO '.                                             
       77  WRK-FECHAMENTO              PIC  X(013)         VALUE        
           'NO FECHAMENTO'.                                             
                                                                        
       77  WRK-BATCH                   PIC  X(008)         VALUE        
           'BATCH'.                                                     
                                                                        
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE        
           '*** AREA DA POOL0480 ***'.                                  
      *----------------------------------------------------------------*
                                                                        
       77  WRK-0480-CODIGO1            PIC  9(001)         VALUE 1.     
       77  WRK-0480-CODIGO2            PIC  9(001)         VALUE 2.     
       77  WRK-0480-CODIGO3            PIC  9(001)         VALUE 3.     
       77  WRK-0480-TAM-CHAVE          PIC  9(002)         VALUE 8.     
       77  WRK-0480-NOME-TABELA        PIC  X(008)         VALUE        
           'TABUFNAT'.                                                  
       77  WRK-0480-TAM-OCORRENC       PIC  9(003)         VALUE 8.     
                                                                        
       01  WRK-0480-REGISTRO.                                           
           03  WRK-0480-CHAVE.                                          
               05  WRK-0480-UF             PIC  X(002)     VALUE SPACES.
               05  WRK-0480-NATUREZA.                                   
                   07  WRK-0480-NATUREZA-N PIC  9(003)     VALUE ZEROS. 
               05  WRK-0480-CARTEIRA       PIC  X(003)     VALUE SPACES.
                                                                        
                                                                        
       01  WRK-ERRO-POOL0480.                                           
           03  FILLER                  PIC  X(042)         VALUE        
               '** ERRO NO ACESSO A POOL0480 - RET.CODE = '.            
           03  WRK-0480-RETURN-CODE    PIC  9(002)         VALUE ZEROS. 
           03  FILLER                  PIC  X(011)         VALUE        
               ' - LOCAL = '.                                           
           03  WRK-0480-LOCAL          PIC  9(002)         VALUE ZEROS. 
           03  FILLER                  PIC  X(003)         VALUE        
               ' **'.                                                   
                                                                        
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE        
           '*** MENSAGEM DE ERRO DE FILE STATUS ***'.                   
      *----------------------------------------------------------------*
                                                                        
       01  WRK-ERRO-ARQUIVO.                                            
           03  FILLER                  PIC  X(008)         VALUE        
               '** ERRO '.                                              
           03  WRK-OPERACAO            PIC  X(013)         VALUE SPACES.
           03  FILLER                  PIC  X(012)         VALUE        
               ' DO ARQUIVO '.                                          
           03  WRK-NOME-ARQ            PIC  X(008)         VALUE SPACES.
           03  FILLER                  PIC  X(017)         VALUE        
               ' - FILE-STATUS = '.                                     
           03  WRK-FILE-STATUS         PIC  X(002)         VALUE SPACES.
           03  FILLER                  PIC  X(003)         VALUE ' **'. 
                                                                        
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE        
           '*** VARIAVEIS AUXILIARES ***'.                              
      *----------------------------------------------------------------*
                                                                        
       01  WRK-MASK-1                  PIC  ZZZ.ZZZ.ZZ9    VALUE ZEROS. 
       01  WRK-MASK-2                  PIC  ZZZ.ZZZ.ZZ9    VALUE ZEROS. 
       01  WRK-MASK-3                  PIC  ZZZ.ZZZ.ZZ9    VALUE ZEROS. 
       01  WRK-MASK-31                 PIC  ZZZ.ZZZ.ZZ9    VALUE ZEROS. 
       01  WRK-MASK-32                 PIC  ZZZ.ZZZ.ZZ9    VALUE ZEROS. 
       01  WRK-MASK-4                  PIC  ZZZ.ZZZ.ZZ9    VALUE ZEROS. 
       01  WRK-MASK-5                  PIC  ZZZ.ZZZ.ZZ9    VALUE ZEROS. 
                                                                        
       01  WRK-CPF.                                                     
           03  WRK-CPF-NUM             PIC  999.999.999    VALUE ZEROS. 
           03  FILLER                  PIC  X(001)         VALUE '-'.   
           03  WRK-CPF-CTR             PIC  99             VALUE ZEROS. 
                                                                        
       01  WRK-CGC.                                                     
           03  WRK-CGC-NUM             PIC  999.999.999    VALUE ZEROS. 
           03  FILLER                  PIC  X(001)         VALUE '/'.   
           03  WRK-CGC-FIL             PIC  9999           VALUE ZEROS. 
           03  FILLER                  PIC  X(001)         VALUE '-'.   
           03  WRK-CGC-CTR             PIC  99             VALUE ZEROS. 
                                                                        
       01  WRK-ACHOU-REGISTRO          PIC  X(001)         VALUE SPACES.
                                                                        
       01  WRK-CHAVE-AVISO.                                             
           03  WRK-CHV-DB2-AGENCIA     PIC  9(005)         VALUE ZEROS. 
           03  WRK-CHV-DB2-CONTA       PIC  9(007)         VALUE ZEROS. 
           03  WRK-CHV-DB2-CONTRATO    PIC  9(007)         VALUE ZEROS. 
                                                                        
       01  WRK-CHAVE-AVISO-ANT         PIC  X(019)         VALUE SPACES.
                                                                        
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE        
           '*** AREA DA POOL7600 ***'.                                  
      *----------------------------------------------------------------*
                                                                        
       01  WRK-DATA-HORA.                                               
           03  WRK-DATA-JULIANA        PIC  9(005) COMP-3  VALUE ZEROS. 
           03  WRK-DATA-AAMMDD         PIC  9(007) COMP-3  VALUE ZEROS. 
           03  WRK-DATA-AAAAMMDD       PIC  9(009) COMP-3  VALUE ZEROS. 
           03  WRK-HORA-HHMMSS         PIC  9(007) COMP-3  VALUE ZEROS. 
           03  WRK-HORA-HHMMSSMMMMMM   PIC  9(013) COMP-3  VALUE ZEROS. 
           03  WRK-TIMESTAMP.                                           
               05  WRK-ANO             PIC  9(004)         VALUE ZEROS. 
               05  WRK-MES             PIC  9(002)         VALUE ZEROS. 
               05  WRK-DIA             PIC  9(002)         VALUE ZEROS. 
               05  WRK-HOR             PIC  9(002)         VALUE ZEROS. 
               05  WRK-MIN             PIC  9(002)         VALUE ZEROS. 
               05  WRK-SEG             PIC  9(002)         VALUE ZEROS. 
               05  WRK-MIL             PIC  9(006)         VALUE ZEROS. 
                                                                        
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE        
           '*** AREA PARA RELATORIO ***'.                               
      *----------------------------------------------------------------*
                                                                        
       01  CABEC1.                                                      
           03  FILLER                  PIC  X(001)         VALUE '1'.   
           03  FILLER                  PIC  X(129)         VALUE        
               'CLLP7612                                          B A N 
      -        'C O   B R A D E S C O   S / A                           
      -        '            PAG:'.                                      
           03  CB1-PAGINA              PIC  ZZ9            VALUE ZEROS. 
                                                                        
       01  CABEC2.                                                      
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  CB2-DIA                 PIC  9(002)         VALUE ZEROS. 
           03  FILLER                  PIC  X(001)         VALUE '/'.   
           03  CB2-MES                 PIC  9(002)         VALUE ZEROS. 
           03  FILLER                  PIC  X(001)         VALUE '/'.   
           03  CB2-ANO                 PIC  9(004)         VALUE ZEROS. 
           03  FILLER                  PIC  X(114)         VALUE        
               '                           CLIENTES COM OPER. DE CREDITO
      -        ' IMOBILIARIO QUE NAO RECEBERAO AVISOS'.                 
           03  CB2-HOR                 PIC  9(002)         VALUE ZEROS. 
           03  FILLER                  PIC  X(001)         VALUE ':'.   
           03  CB2-MIN                 PIC  9(002)         VALUE ZEROS. 
           03  FILLER                  PIC  X(001)         VALUE ':'.   
           03  CB2-SEG                 PIC  9(002)         VALUE ZEROS. 
                                                                        
       01  CABEC2A.                                                     
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  CB2A-DIA                PIC  9(002)         VALUE ZEROS. 
           03  FILLER                  PIC  X(001)         VALUE '/'.   
           03  CB2A-MES                PIC  9(002)         VALUE ZEROS. 
           03  FILLER                  PIC  X(001)         VALUE '/'.   
           03  CB2A-ANO                PIC  9(004)         VALUE ZEROS. 
           03  FILLER                  PIC  X(114)         VALUE        
               '                                CLIENTES COM OPER. DE CR
      -        'EDITO IMOBILIARIO QUE NAO POSSUEM'.                     
           03  CB2A-HOR                PIC  9(002)         VALUE ZEROS. 
           03  FILLER                  PIC  X(001)         VALUE ':'.   
           03  CB2A-MIN                PIC  9(002)         VALUE ZEROS. 
           03  FILLER                  PIC  X(001)         VALUE ':'.   
           03  CB2A-SEG                PIC  9(002)         VALUE ZEROS. 
                                                                        
       01  CABEC3.                                                      
           03  FILLER                  PIC  X(133)         VALUE SPACES.
                                                                        
       01  CABEC4.                                                      
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(132)         VALUE        
               '   AGENCIA     CONTA   CART   CONTRATO   CPF/CGC        
      -        '       UF   CLIENTE                                    D
      -        'ATA DE VENCIMENTO'.                                     
                                                                        
       01  CABEC7.                                                      
           03  FILLER                  PIC  X(011)         VALUE SPACES.
           03  FILLER                  PIC  X(114)         VALUE        
               '                                        POR BLOQUEIO POR
      -        ' UF DO ENDERECO DO IMOVEL'.                             
                                                                        
       01  CABEC7A.                                                     
           03  FILLER                  PIC  X(011)         VALUE SPACES.
           03  FILLER                  PIC  X(117)         VALUE        
               '                                     BLOQUEIO DE AVISOS 
      -        'POR UF DO ENDERECO DO IMOVEL'.                          
                                                                        
       01  LINDET1.                                                     
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(005)         VALUE SPACES.
           03  LD1-AGENCIA             PIC  ZZZZ9          VALUE ZEROS. 
           03  FILLER                  PIC  X(003)         VALUE SPACES.
           03  LD1-CONTA               PIC  ZZZZZZ9        VALUE ZEROS. 
           03  FILLER                  PIC  X(004)         VALUE SPACES.
BRQ141     03  LD1-CARTEIRA            PIC  X(003)         VALUE SPACES.
BRQ141*........05..LD1-CARTEIRA-N......PIC..ZZ9............VALUE.ZEROS. 
           03  FILLER                  PIC  X(004)         VALUE SPACES.
           03  LD1-CONTRATO            PIC  ZZZZZZ9        VALUE ZEROS. 
           03  FILLER                  PIC  X(003)         VALUE SPACES.
           03  LD1-CGC-CPF             PIC  X(019)         VALUE SPACES.
           03  FILLER                  PIC  X(003)         VALUE SPACES.
           03  LD1-UF                  PIC  X(002)         VALUE SPACES.
           03  FILLER                  PIC  X(003)         VALUE SPACES.
           03  LD1-CLIENTE             PIC  X(040)         VALUE SPACES.
           03  FILLER                  PIC  X(007)         VALUE SPACES.
           03  LD1-DATA                PIC  X(010)         VALUE SPACES.
                                                                        
       01  LINTOT1.                                                     
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(033)         VALUE        
               '   TOTAL DE OPERACOES EXCLUIDAS :'.                     
           03  LT1-EXCLUIDOS           PIC  ZZZ.ZZZ.ZZ9    VALUE ZEROS. 
                                                                        
       01  LINTOT1A.                                                    
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(023)         VALUE        
               '   TOTAL DE CONTRATOS :'.                               
           03  LT1A-CONTRATOS          PIC  ZZZ.ZZZ.ZZ9    VALUE ZEROS. 
                                                                        
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE        
           '*** AREA DA POOL7100 ***'.                                  
      *----------------------------------------------------------------*
                                                                        
           COPY POL7100C.                                               
                                                                        
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE        
           '*** CLLP7612 - FIM DA AREA DE WORKING ***'.                 
      *----------------------------------------------------------------*
                                                                        
      *================================================================*
       PROCEDURE                       DIVISION.                        
      *================================================================*
                                                                        
      *----------------------------------------------------------------*
       0000-INICIAR                  SECTION.                           
      *----------------------------------------------------------------*
                                                                        
           PERFORM 1000-INICIALIZAR.                                    
                                                                        
           PERFORM 2000-VERIFICAR-VAZIO.                                
                                                                        
           PERFORM 3000-CARREGAR-TABELA.                                
                                                                        
           PERFORM 4000-PROCESSAR    UNTIL                              
                   WRK-FS-ARQAVISO     EQUAL '10'.                      
                                                                        
           PERFORM 5000-FINALIZAR.                                      
                                                                        
      *----------------------------------------------------------------*
       0000-99-FIM.                  EXIT.                              
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       1000-INICIALIZAR              SECTION.                           
      *----------------------------------------------------------------*
                                                                        
           OPEN INPUT  ARQAVISO                                         
                       TABELA                                           
                OUTPUT ARQSAIDA                                         
                       ARQBLOQ                                          
                       BLQREST                                          
                       RELINCO                                          
                       RELATO.                                          
                                                                        
           MOVE WRK-ABERTURA           TO WRK-OPERACAO.                 
                                                                        
           PERFORM 1100-TESTAR-FILE-STATUS.                             
                                                                        
           CALL 'POOL7600'             USING WRK-DATA-HORA.             
                                                                        
           MOVE WRK-DIA                TO CB2-DIA                       
                                          CB2A-DIA.                     
           MOVE WRK-MES                TO CB2-MES                       
                                          CB2A-MES.                     
           MOVE WRK-ANO                TO CB2-ANO                       
                                          CB2A-ANO.                     
           MOVE WRK-HOR                TO CB2-HOR                       
                                          CB2A-HOR.                     
           MOVE WRK-MIN                TO CB2-MIN                       
                                          CB2A-MIN.                     
           MOVE WRK-SEG                TO CB2-SEG                       
                                          CB2A-SEG.                     
                                                                        
      *----------------------------------------------------------------*
       1000-99-FIM.                  EXIT.                              
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS       SECTION.                           
      *----------------------------------------------------------------*
                                                                        
           PERFORM 1110-TESTAR-FS-ARQAVISO.                             
                                                                        
           PERFORM 1120-TESTAR-FS-TABELA.                               
                                                                        
           PERFORM 1130-TESTAR-FS-ARQSAIDA.                             
                                                                        
           PERFORM 1131-TESTAR-FS-ARQBLOQ.                              
                                                                        
           PERFORM 1132-TESTAR-FS-BLQREST.                              
                                                                        
           PERFORM 1140-TESTAR-FS-RELINCO.                              
                                                                        
           PERFORM 1150-TESTAR-FS-RELATO.                               
                                                                        
      *----------------------------------------------------------------*
       1100-99-FIM.                  EXIT.                              
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       1110-TESTAR-FS-ARQAVISO       SECTION.                           
      *----------------------------------------------------------------*
                                                                        
           IF  WRK-FS-ARQAVISO         NOT EQUAL ZEROS                  
               MOVE 'ARQAVISO'         TO WRK-NOME-ARQ                  
               MOVE WRK-FS-ARQAVISO    TO WRK-FILE-STATUS               
               MOVE WRK-ERRO-ARQUIVO   TO ERR-TEXTO                     
               PERFORM 9999-PROCESSAR-ROTINA-ERRO                       
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       1110-99-FIM.                  EXIT.                              
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       1120-TESTAR-FS-TABELA         SECTION.                           
      *----------------------------------------------------------------*
                                                                        
           IF  WRK-FS-TABELA           NOT EQUAL ZEROS                  
               MOVE 'TABELA  '         TO WRK-NOME-ARQ                  
               MOVE WRK-FS-TABELA      TO WRK-FILE-STATUS               
               MOVE WRK-ERRO-ARQUIVO   TO ERR-TEXTO                     
               PERFORM 9999-PROCESSAR-ROTINA-ERRO                       
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       1120-99-FIM.                  EXIT.                              
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       1130-TESTAR-FS-ARQSAIDA       SECTION.                           
      *----------------------------------------------------------------*
                                                                        
           IF  WRK-FS-ARQSAIDA         NOT EQUAL ZEROS                  
               MOVE 'ARQSAIDA'         TO WRK-NOME-ARQ                  
               MOVE WRK-FS-ARQSAIDA    TO WRK-FILE-STATUS               
               MOVE WRK-ERRO-ARQUIVO   TO ERR-TEXTO                     
               PERFORM 9999-PROCESSAR-ROTINA-ERRO                       
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       1130-99-FIM.                  EXIT.                              
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       1131-TESTAR-FS-ARQBLOQ        SECTION.                           
      *----------------------------------------------------------------*
                                                                        
           IF  WRK-FS-ARQBLOQ          NOT EQUAL ZEROS                  
               MOVE 'ARQBLOQ '         TO WRK-NOME-ARQ                  
               MOVE WRK-FS-ARQBLOQ     TO WRK-FILE-STATUS               
               MOVE WRK-ERRO-ARQUIVO   TO ERR-TEXTO                     
               PERFORM 9999-PROCESSAR-ROTINA-ERRO                       
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       1131-99-FIM.                  EXIT.                              
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       1132-TESTAR-FS-BLQREST        SECTION.                           
      *----------------------------------------------------------------*
                                                                        
           IF  WRK-FS-BLQREST          NOT EQUAL ZEROS                  
               MOVE 'BLQREST '         TO WRK-NOME-ARQ                  
               MOVE WRK-FS-BLQREST     TO WRK-FILE-STATUS               
               MOVE WRK-ERRO-ARQUIVO   TO ERR-TEXTO                     
               PERFORM 9999-PROCESSAR-ROTINA-ERRO                       
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       1132-99-FIM.                  EXIT.                              
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       1140-TESTAR-FS-RELINCO        SECTION.                           
      *----------------------------------------------------------------*
                                                                        
           IF  WRK-FS-RELINCO          NOT EQUAL ZEROS                  
               MOVE 'RELINCO '         TO WRK-NOME-ARQ                  
               MOVE WRK-FS-RELINCO     TO WRK-FILE-STATUS               
               MOVE WRK-ERRO-ARQUIVO   TO ERR-TEXTO                     
               PERFORM 9999-PROCESSAR-ROTINA-ERRO                       
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       1140-99-FIM.                  EXIT.                              
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       1150-TESTAR-FS-RELATO         SECTION.                           
      *----------------------------------------------------------------*
                                                                        
           IF  WRK-FS-RELATO           NOT EQUAL ZEROS                  
               MOVE 'RELATO  '         TO WRK-NOME-ARQ                  
               MOVE WRK-FS-RELATO      TO WRK-FILE-STATUS               
               MOVE WRK-ERRO-ARQUIVO   TO ERR-TEXTO                     
               PERFORM 9999-PROCESSAR-ROTINA-ERRO                       
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       1150-99-FIM.                  EXIT.                              
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       2000-VERIFICAR-VAZIO          SECTION.                           
      *----------------------------------------------------------------*
                                                                        
           PERFORM 2100-LER-ARQAVISO.                                   
                                                                        
           IF  ACU-LIDOS-ARQAVISO      EQUAL ZEROS                      
               DISPLAY '*************** CLLP7612 ***************'       
               DISPLAY '*                                      *'       
               DISPLAY '*       ARQUIVO ARQAVISO VAZIO         *'       
               DISPLAY '*      PROCESSAMENTO  ENCERRADO        *'       
               DISPLAY '*                                      *'       
               DISPLAY '*************** CLLP7612 ***************'       
               PERFORM 5000-FINALIZAR                                   
           END-IF.                                                      
                                                                        
           PERFORM 2200-LER-TABELA.                                     
                                                                        
           IF  ACU-LIDOS-TABELA        EQUAL ZEROS                      
               DISPLAY '*************** CLLP7612 ***************'       
               DISPLAY '*                                      *'       
               DISPLAY '*       ARQUIVO TABELA   VAZIO         *'       
               DISPLAY '*      PROCESSAMENTO  ENCERRADO        *'       
               DISPLAY '*                                      *'       
               DISPLAY '*************** CLLP7612 ***************'       
               PERFORM 5000-FINALIZAR                                   
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       2000-99-FIM.                  EXIT.                              
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       2100-LER-ARQAVISO             SECTION.                           
      *----------------------------------------------------------------*
                                                                        
           READ ARQAVISO.                                               
                                                                        
           IF  WRK-FS-ARQAVISO         EQUAL '10'                       
               MOVE HIGH-VALUES        TO WRK-CHAVE-AVISO               
               GO TO 2100-99-FIM                                        
           END-IF.                                                      
                                                                        
           MOVE WRK-LEITURA            TO WRK-OPERACAO.                 
                                                                        
           PERFORM 1110-TESTAR-FS-ARQAVISO.                             
                                                                        
           MOVE DB2-AGENCIA            TO WRK-CHV-DB2-AGENCIA.          
           MOVE DB2-CONTA              TO WRK-CHV-DB2-CONTA.            
           MOVE DB2-CONTRATO           TO WRK-CHV-DB2-CONTRATO.         
                                                                        
           ADD 1                       TO ACU-LIDOS-ARQAVISO.           
                                                                        
      *----------------------------------------------------------------*
       2100-99-FIM.                  EXIT.                              
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       2200-LER-TABELA               SECTION.                           
      *----------------------------------------------------------------*
                                                                        
           READ TABELA.                                                 
                                                                        
           IF  WRK-FS-TABELA           EQUAL '10'                       
               GO TO 2200-99-FIM                                        
           END-IF.                                                      
                                                                        
           MOVE WRK-LEITURA            TO WRK-OPERACAO.                 
                                                                        
           PERFORM 1120-TESTAR-FS-TABELA.                               
                                                                        
           ADD 1                       TO ACU-LIDOS-TABELA.             
                                                                        
      *----------------------------------------------------------------*
       2200-99-FIM.                  EXIT.                              
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       3000-CARREGAR-TABELA          SECTION.                           
      *----------------------------------------------------------------*
                                                                        
           PERFORM 3100-MONTAR-TABELA                                   
                   UNTIL WRK-FS-TABELA EQUAL '10'.                      
                                                                        
           CALL 'POOL0480'             USING WRK-0480-CODIGO2           
                                             WRK-0480-NOME-TABELA.      
                                                                        
           IF  RETURN-CODE             NOT EQUAL ZEROS                  
               MOVE RETURN-CODE        TO WRK-0480-RETURN-CODE          
               MOVE ZEROS              TO RETURN-CODE                   
               MOVE 01                 TO WRK-0480-LOCAL                
               MOVE WRK-ERRO-POOL0480  TO ERR-TEXTO                     
               PERFORM 9999-PROCESSAR-ROTINA-ERRO                       
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       3000-99-FIM.                  EXIT.                              
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       3100-MONTAR-TABELA            SECTION.                           
      *----------------------------------------------------------------*
                                                                        
           MOVE B037-CSGL-UF           TO WRK-0480-UF.                  
           MOVE B037-CTPO-NATUZ-OPER   TO WRK-0480-NATUREZA-N.          
           MOVE B037-CCART             TO WRK-0480-CARTEIRA.            
                                                                        
           CALL 'POOL0480'             USING WRK-0480-CODIGO1           
                                             WRK-0480-NOME-TABELA       
                                             WRK-0480-TAM-CHAVE         
                                             WRK-0480-TAM-OCORRENC      
                                             WRK-0480-REGISTRO.         
                                                                        
           IF  RETURN-CODE             NOT EQUAL ZEROS                  
               MOVE RETURN-CODE        TO WRK-0480-RETURN-CODE          
               MOVE ZEROS              TO RETURN-CODE                   
               MOVE 02                 TO WRK-0480-LOCAL                
               MOVE WRK-ERRO-POOL0480  TO ERR-TEXTO                     
               PERFORM 9999-PROCESSAR-ROTINA-ERRO                       
           END-IF.                                                      
                                                                        
           PERFORM 2200-LER-TABELA.                                     
                                                                        
      *----------------------------------------------------------------*
       3100-99-FIM.                  EXIT.                              
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       4000-PROCESSAR                SECTION.                           
      *----------------------------------------------------------------*
                                                                        
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.                 
           MOVE WRK-CHAVE-AVISO        TO WRK-CHAVE-AVISO-ANT.          
                                                                        
           PERFORM 4100-PESQUISAR-TABELA.                               
                                                                        
           IF  WRK-ACHOU-REGISTRO      EQUAL 'S'                        
               PERFORM 4200-EMITIR-RELINCO                              
               PERFORM 4309-GRAVAR-BLQREST                              
               PERFORM 4310-GRAVAR-ARQBLOQ                              
           ELSE                                                         
               PERFORM 4400-EMITIR-RELATO                               
               PERFORM 4300-GRAVAR-ARQSAIDA                             
                       UNTIL           WRK-CHAVE-AVISO                  
                       NOT EQUAL       WRK-CHAVE-AVISO-ANT              
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       4000-99-FIM.                  EXIT.                              
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       4100-PESQUISAR-TABELA         SECTION.                           
      *----------------------------------------------------------------*
                                                                        
           MOVE 'N'                    TO WRK-ACHOU-REGISTRO.           
           MOVE ENT-UF-ENDER-IMOVEL    TO WRK-0480-UF.                  
           MOVE ZEROS                  TO WRK-0480-NATUREZA-N           
                                          WRK-0480-CARTEIRA.            
                                                                        
           PERFORM 4110-ACESSAR-POOL0480.                               
                                                                        
           IF  RETURN-CODE             EQUAL ZEROS                      
               MOVE 'S'                TO WRK-ACHOU-REGISTRO            
               GO TO 4100-99-FIM                                        
           END-IF.                                                      
                                                                        
           MOVE ENT-UF-ENDER-IMOVEL    TO WRK-0480-UF.                  
           MOVE DB2-COD-NATUREZA-OPER  TO WRK-0480-NATUREZA.            
           MOVE ZEROS                  TO WRK-0480-CARTEIRA.            
                                                                        
           PERFORM 4110-ACESSAR-POOL0480.                               
                                                                        
           IF  RETURN-CODE             EQUAL ZEROS                      
               MOVE 'S'                TO WRK-ACHOU-REGISTRO            
               GO TO 4100-99-FIM                                        
           END-IF.                                                      
                                                                        
           MOVE ENT-UF-ENDER-IMOVEL    TO WRK-0480-UF.                  
           MOVE ZEROS                  TO WRK-0480-NATUREZA-N.          
           MOVE DB2-CARTEIRA           TO WRK-0480-CARTEIRA.            
                                                                        
           PERFORM 4110-ACESSAR-POOL0480.                               
                                                                        
           IF  RETURN-CODE             EQUAL ZEROS                      
               MOVE 'S'                TO WRK-ACHOU-REGISTRO            
               GO TO 4100-99-FIM                                        
           END-IF.                                                      
                                                                        
           MOVE ENT-UF-ENDER-IMOVEL    TO WRK-0480-UF.                  
           MOVE DB2-COD-NATUREZA-OPER  TO WRK-0480-NATUREZA.            
           MOVE DB2-CARTEIRA           TO WRK-0480-CARTEIRA.            
                                                                        
           PERFORM 4110-ACESSAR-POOL0480.                               
                                                                        
           IF  RETURN-CODE             EQUAL ZEROS                      
               MOVE 'S'                TO WRK-ACHOU-REGISTRO            
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       4100-99-FIM.                  EXIT.                              
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       4110-ACESSAR-POOL0480         SECTION.                           
      *----------------------------------------------------------------*
                                                                        
           CALL 'POOL0480'             USING WRK-0480-CODIGO3           
                                             WRK-0480-NOME-TABELA       
                                             WRK-0480-CHAVE             
                                             WRK-0480-REGISTRO.         
                                                                        
           IF  RETURN-CODE             NOT EQUAL ZEROS AND 12           
               MOVE RETURN-CODE        TO WRK-0480-RETURN-CODE          
               MOVE ZEROS              TO RETURN-CODE                   
               MOVE 03                 TO WRK-0480-LOCAL                
               MOVE WRK-ERRO-POOL0480  TO ERR-TEXTO                     
               PERFORM 9999-PROCESSAR-ROTINA-ERRO                       
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       4110-99-FIM.                  EXIT.                              
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       4200-EMITIR-RELINCO           SECTION.                           
      *----------------------------------------------------------------*
                                                                        
           IF  ACU-LINHAS-RELINCO      GREATER 56                       
               PERFORM 4210-EMITIR-CABECALHO                            
           END-IF.                                                      
                                                                        
           MOVE DB2-AGENCIA            TO LD1-AGENCIA.                  
           MOVE DB2-CONTA              TO LD1-CONTA.                    
BRQ141*....MOVE.DB2-CARTEIRA...........TO.LD1-CARTEIRA-N.               
BRQ141     MOVE DB2-CARTEIRA           TO LD1-CARTEIRA.                 
           MOVE DB2-CONTRATO           TO LD1-CONTRATO.                 
                                                                        
           IF  DB2-FILIAL              EQUAL ZEROS                      
               MOVE DB2-NUMERO-DEV     TO WRK-CPF-NUM                   
               MOVE DB2-CTR-DEV        TO WRK-CPF-CTR                   
               MOVE WRK-CPF            TO LD1-CGC-CPF                   
           ELSE                                                         
               MOVE DB2-NUMERO-DEV     TO WRK-CGC-NUM                   
               MOVE DB2-FILIAL         TO WRK-CGC-FIL                   
               MOVE DB2-CTR-DEV        TO WRK-CGC-CTR                   
               MOVE WRK-CGC            TO LD1-CGC-CPF                   
           END-IF.                                                      
                                                                        
           MOVE ENT-UF-ENDER-IMOVEL    TO LD1-UF.                       
           MOVE DB2-NOME-DEVEDOR       TO LD1-CLIENTE.                  
           MOVE DB2-VCTO               TO LD1-DATA.                     
           MOVE '/'                    TO LD1-DATA(3:1)                 
                                          LD1-DATA(6:1).                
                                                                        
           PERFORM 4220-EMITIR-DETALHE.                                 
                                                                        
           ADD 1                       TO ACU-EXCLUIDOS.                
                                                                        
      *----------------------------------------------------------------*
       4200-99-FIM.                  EXIT.                              
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       4210-EMITIR-CABECALHO         SECTION.                           
      *----------------------------------------------------------------*
                                                                        
           ADD 1                       TO ACU-PAGINA-RELINCO.           
                                                                        
           MOVE ACU-PAGINA-RELINCO     TO CB1-PAGINA.                   
                                                                        
           WRITE REG-RELINCO           FROM CABEC1.                     
                                                                        
           PERFORM 1140-TESTAR-FS-RELINCO.                              
                                                                        
           WRITE REG-RELINCO           FROM CABEC2.                     
                                                                        
           PERFORM 1140-TESTAR-FS-RELINCO.                              
                                                                        
           WRITE REG-RELINCO           FROM CABEC7.                     
                                                                        
           PERFORM 1140-TESTAR-FS-RELINCO.                              
                                                                        
           WRITE REG-RELINCO           FROM CABEC3.                     
                                                                        
           PERFORM 1140-TESTAR-FS-RELINCO.                              
                                                                        
           WRITE REG-RELINCO           FROM CABEC4.                     
                                                                        
           PERFORM 1140-TESTAR-FS-RELINCO.                              
                                                                        
           WRITE REG-RELINCO           FROM CABEC3.                     
                                                                        
           PERFORM 1140-TESTAR-FS-RELINCO.                              
                                                                        
           MOVE 6                      TO ACU-LINHAS-RELINCO.           
                                                                        
      *----------------------------------------------------------------*
       4210-99-FIM.                  EXIT.                              
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       4220-EMITIR-DETALHE           SECTION.                           
      *----------------------------------------------------------------*
                                                                        
           WRITE REG-RELINCO           FROM LINDET1.                    
                                                                        
           PERFORM 1140-TESTAR-FS-RELINCO.                              
                                                                        
           ADD 1                       TO ACU-LINHAS-RELINCO            
                                          ACU-GRAVA-RELINCO.            
                                                                        
      *----------------------------------------------------------------*
       4220-99-FIM.                  EXIT.                              
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       4300-GRAVAR-ARQSAIDA          SECTION.                           
      *----------------------------------------------------------------*
                                                                        
           MOVE DB2-REGTO              TO REG-ARQSAIDA.                 
                                                                        
           WRITE REG-ARQSAIDA.                                          
                                                                        
           PERFORM 1130-TESTAR-FS-ARQSAIDA.                             
                                                                        
           ADD 1                       TO ACU-GRAVA-ARQSAIDA.           
                                                                        
           PERFORM 2100-LER-ARQAVISO.                                   
                                                                        
      *----------------------------------------------------------------*
       4300-99-FIM.                  EXIT.                              
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       4309-GRAVAR-BLQREST           SECTION.                           
      *----------------------------------------------------------------*
                                                                        
           MOVE DB2-AGENCIA    TO  FD-AGENCIA.                          
           MOVE DB2-CONTA      TO  FD-CONTA.                            
           MOVE DB2-CONTRATO   TO  FD-CONTRATO.                         
           MOVE DB2-EMPRESA    TO  FD-EMPRESA.                          
           MOVE DB2-CARTEIRA   TO  FD-CARTEIRA.                         
           MOVE 02             TO  FD-IND-BLOQ.                         
           MOVE 'D'            TO  FD-INDICADOR.                        
           MOVE DB2-NUMERO-DEV TO  FD-CPF.                              
           MOVE DB2-FILIAL     TO  FD-FILIAL.                           
           MOVE DB2-CTR-DEV    TO  FD-CTRL.                             
                                                                        
           WRITE FD-BLQRESTR.                                           
                                                                        
           PERFORM 1132-TESTAR-FS-BLQREST.                              
                                                                        
           ADD 1                       TO ACU-GRAVA-BLQREST.            
                                                                        
      *----------------------------------------------------------------*
       4309-99-FIM.                  EXIT.                              
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       4310-GRAVAR-ARQBLOQ           SECTION.                           
      *----------------------------------------------------------------*
                                                                        
           MOVE DB2-REGTO              TO FD-ARQBLOQ.                   
                                                                        
           WRITE FD-ARQBLOQ.                                            
                                                                        
           PERFORM 1131-TESTAR-FS-ARQBLOQ.                              
                                                                        
           ADD 1                       TO ACU-GRAVA-ARQBLOQ.            
                                                                        
           PERFORM 2100-LER-ARQAVISO                                    
                   UNTIL               WRK-CHAVE-AVISO                  
                   NOT EQUAL           WRK-CHAVE-AVISO-ANT.             
                                                                        
      *----------------------------------------------------------------*
       4310-99-FIM.                  EXIT.                              
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       4400-EMITIR-RELATO            SECTION.                           
      *----------------------------------------------------------------*
                                                                        
           IF  ACU-LINHAS-RELATO       GREATER 56                       
               PERFORM 4410-EMITIR-CABECALHO                            
           END-IF.                                                      
                                                                        
           MOVE DB2-AGENCIA            TO LD1-AGENCIA.                  
           MOVE DB2-CONTA              TO LD1-CONTA.                    
BRQ141*....MOVE.DB2-CARTEIRA...........TO.LD1-CARTEIRA-N.               
BRQ141     MOVE DB2-CARTEIRA           TO LD1-CARTEIRA.                 
           MOVE DB2-CONTRATO           TO LD1-CONTRATO.                 
                                                                        
           IF  DB2-FILIAL              EQUAL ZEROS                      
               MOVE DB2-NUMERO-DEV     TO WRK-CPF-NUM                   
               MOVE DB2-CTR-DEV        TO WRK-CPF-CTR                   
               MOVE WRK-CPF            TO LD1-CGC-CPF                   
           ELSE                                                         
               MOVE DB2-NUMERO-DEV     TO WRK-CGC-NUM                   
               MOVE DB2-FILIAL         TO WRK-CGC-FIL                   
               MOVE DB2-CTR-DEV        TO WRK-CGC-CTR                   
               MOVE WRK-CGC            TO LD1-CGC-CPF                   
           END-IF.                                                      
                                                                        
           MOVE ENT-UF-ENDER-IMOVEL    TO LD1-UF.                       
           MOVE DB2-NOME-DEVEDOR       TO LD1-CLIENTE.                  
           MOVE DB2-VCTO               TO LD1-DATA.                     
           MOVE '/'                    TO LD1-DATA(3:1)                 
                                          LD1-DATA(6:1).                
                                                                        
           PERFORM 4420-EMITIR-DETALHE.                                 
                                                                        
      *----------------------------------------------------------------*
       4400-99-FIM.                  EXIT.                              
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       4410-EMITIR-CABECALHO         SECTION.                           
      *----------------------------------------------------------------*
                                                                        
           ADD 1                       TO ACU-PAGINA-RELATO.            
                                                                        
           MOVE ACU-PAGINA-RELATO      TO CB1-PAGINA.                   
           MOVE CABEC1                 TO REG-RELATO.                   
           MOVE 'A'                    TO REG-RELATO(10:1).             
                                                                        
           WRITE REG-RELATO.                                            
                                                                        
           PERFORM 1150-TESTAR-FS-RELATO.                               
                                                                        
           WRITE REG-RELATO            FROM CABEC2A.                    
                                                                        
           PERFORM 1150-TESTAR-FS-RELATO.                               
                                                                        
           WRITE REG-RELATO            FROM CABEC7A.                    
                                                                        
           PERFORM 1150-TESTAR-FS-RELATO.                               
                                                                        
           WRITE REG-RELATO            FROM CABEC3.                     
                                                                        
           PERFORM 1150-TESTAR-FS-RELATO.                               
                                                                        
           WRITE REG-RELATO            FROM CABEC4.                     
                                                                        
           PERFORM 1150-TESTAR-FS-RELATO.                               
                                                                        
           WRITE REG-RELATO            FROM CABEC3.                     
                                                                        
           PERFORM 1150-TESTAR-FS-RELATO.                               
                                                                        
           MOVE 6                      TO ACU-LINHAS-RELATO.            
                                                                        
      *----------------------------------------------------------------*
       4410-99-FIM.                  EXIT.                              
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       4420-EMITIR-DETALHE           SECTION.                           
      *----------------------------------------------------------------*
                                                                        
           WRITE REG-RELATO            FROM LINDET1.                    
                                                                        
           PERFORM 1150-TESTAR-FS-RELATO.                               
                                                                        
           ADD 1                       TO ACU-LINHAS-RELATO             
                                          ACU-GRAVA-RELATO.             
                                                                        
      *----------------------------------------------------------------*
       4420-99-FIM.                  EXIT.                              
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       5000-FINALIZAR                SECTION.                           
      *----------------------------------------------------------------*
                                                                        
           IF  ACU-GRAVA-RELINCO       GREATER ZEROS                    
               PERFORM 5100-EMITIR-TOTAIS                               
           END-IF.                                                      
                                                                        
           IF  ACU-GRAVA-RELATO        GREATER ZEROS                    
               PERFORM 5200-EMITIR-TOTAIS                               
           END-IF.                                                      
                                                                        
           PERFORM 5300-EXIBIR-DISPLAY-PROCESS.                         
                                                                        
           CLOSE ARQAVISO                                               
                 TABELA                                                 
                 ARQSAIDA                                               
                 ARQBLOQ                                                
                 BLQREST                                                
                 RELINCO                                                
                 RELATO.                                                
                                                                        
           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.                 
                                                                        
           PERFORM 1100-TESTAR-FILE-STATUS.                             
                                                                        
           GOBACK.                                                      
                                                                        
      *----------------------------------------------------------------*
       5000-99-FIM.                  EXIT.                              
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       5100-EMITIR-TOTAIS            SECTION.                           
      *----------------------------------------------------------------*
                                                                        
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.                 
           MOVE ACU-EXCLUIDOS          TO LT1-EXCLUIDOS.                
                                                                        
           WRITE REG-RELINCO           FROM CABEC3.                     
                                                                        
           PERFORM 1140-TESTAR-FS-RELINCO.                              
                                                                        
           WRITE REG-RELINCO           FROM LINTOT1.                    
                                                                        
           PERFORM 1140-TESTAR-FS-RELINCO.                              
                                                                        
      *----------------------------------------------------------------*
       5100-99-FIM.                  EXIT.                              
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       5200-EMITIR-TOTAIS            SECTION.                           
      *----------------------------------------------------------------*
                                                                        
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.                 
           MOVE ACU-GRAVA-RELATO       TO LT1A-CONTRATOS.               
                                                                        
           WRITE REG-RELATO            FROM CABEC3.                     
                                                                        
           PERFORM 1150-TESTAR-FS-RELATO.                               
                                                                        
           WRITE REG-RELATO            FROM LINTOT1A.                   
                                                                        
           PERFORM 1150-TESTAR-FS-RELATO.                               
                                                                        
      *----------------------------------------------------------------*
       5200-99-FIM.                  EXIT.                              
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       5300-EXIBIR-DISPLAY-PROCESS   SECTION.                           
      *----------------------------------------------------------------*
                                                                        
           MOVE ACU-LIDOS-ARQAVISO     TO WRK-MASK-1.                   
           MOVE ACU-LIDOS-TABELA       TO WRK-MASK-2.                   
           MOVE ACU-GRAVA-ARQSAIDA     TO WRK-MASK-3.                   
           MOVE ACU-GRAVA-ARQBLOQ      TO WRK-MASK-31.                  
           MOVE ACU-GRAVA-BLQREST      TO WRK-MASK-32.                  
           MOVE ACU-GRAVA-RELINCO      TO WRK-MASK-4.                   
           MOVE ACU-GRAVA-RELATO       TO WRK-MASK-5.                   
                                                                        
           DISPLAY '*************** CLLP7612 ***************'.          
           DISPLAY '*                                      *'.          
           DISPLAY '* TOT. LIDOS    ARQAVISO : ' WRK-MASK-1  ' *'.      
           DISPLAY '* TOT. LIDOS    TABELA   : ' WRK-MASK-2  ' *'.      
           DISPLAY '* TOT. GRAVADOS ARQSAIDA : ' WRK-MASK-3  ' *'.      
           DISPLAY '* TOT. GRAVADOS ARQBLOQ  : ' WRK-MASK-31 ' *'.      
           DISPLAY '* TOT. GRAVADOS BLQREST  : ' WRK-MASK-32 ' *'.      
           DISPLAY '* TOT. GRAVADOS RELINCO  : ' WRK-MASK-4  ' *'.      
           DISPLAY '* TOT. GRAVADOS RELATO   : ' WRK-MASK-5  ' *'.      
           DISPLAY '*                                      *'.          
           DISPLAY '*************** CLLP7612 ***************'.          
                                                                        
      *----------------------------------------------------------------*
       5300-99-FIM.                  EXIT.                              
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO    SECTION.                           
      *----------------------------------------------------------------*
                                                                        
           MOVE 'APL'                  TO ERR-TIPO-ACESSO.              
           MOVE 'CLLP7612'             TO ERR-PGM.                      
                                                                        
           CALL 'POOL7100'             USING WRK-BATCH                  
                                             ERRO-AREA.                 
                                                                        
           GOBACK.                                                      
                                                                        
      *----------------------------------------------------------------*
       9999-99-FIM.                  EXIT.                              
      *----------------------------------------------------------------*

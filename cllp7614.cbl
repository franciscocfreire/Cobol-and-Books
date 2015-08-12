      *================================================================*
       IDENTIFICATION                  DIVISION.                        
      *================================================================*
                                                                        
       PROGRAM-ID. CLLP7614.                                            
       AUTHOR.     PATRIC ZAGO.                                         
      *================================================================*
      *                    C P M  -  S I S T E M A S                   *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   CLLP7614                                    *
      *    PROGRAMADOR.:   PATRIC ZAGO               - CPM/A           *
      *    ANALISTA....:   CIDINHA                   - DDS/GRUPO 70    *
      *    DATA........:   07/12/2005                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   DEVERA SER FEITO BALANCE-LINE ENTRE O ARQUI-*
      *      VO DE ENDERECOS DO CREDITO IMOBILIARIO E O ARQUIVO DE AVI-*
      *      SOS GERANDO O ARQUIVO DE SAIDA QUANDO A CHAVE  FOR  ENCON-*
      *      TRADA E A UF ESTIVER OK. O ARQUIVO DE SAIDA DEVERA CONTER *
      *      NO FINAL A UF DO ARQUIVO DE ENTRADA. EMITIR RELATORIO LIS-*
      *      TANDO OS REGISTROS EXCLUIDOS.                             *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                    DDNAME                    INCLUDE/BOOK      *
      *                    ARQENT                      --------        *
      *                    ARQDB2                      I#CLLPKA        *
      *                    ARQSAIDA                    --------        *
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
BRQ141*    DATA        :    09/2012                                    *
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
                                                                        
           SELECT ARQENT   ASSIGN      TO UT-S-ARQENT                   
           FILE STATUS                 IS WRK-FS-ARQENT.                
                                                                        
           SELECT ARQDB2   ASSIGN      TO UT-S-ARQDB2                   
           FILE STATUS                 IS WRK-FS-ARQDB2.                
                                                                        
           SELECT ARQSAIDA ASSIGN      TO UT-S-ARQSAIDA                 
           FILE STATUS                 IS WRK-FS-ARQSAIDA.              
                                                                        
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
      *    INPUT:  ARQUIVO DE ENTRADA                                  *
      *            ORG. SEQUENCIAL     -   LRECL   =   130             *
      *----------------------------------------------------------------*
                                                                        
       FD  ARQENT                                                       
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  REG-ARQENT.                                                  
           03  ENT-AGENCIA             PIC  9(005) COMP-3.              
           03  ENT-CONTA               PIC  9(007) COMP-3.              
           03  ENT-CONTRATO            PIC  9(007) COMP-3.              
           03  ENT-LOGRADOURO          PIC  X(040).                     
           03  ENT-NRO-LOGDR           PIC  9(005) COMP-3.              
           03  ENT-COMPL-LOGDR         PIC  X(010).                     
           03  ENT-BAIRRO-LOGDR        PIC  X(020).                     
           03  ENT-MUNICIPIO           PIC  X(030).                     
           03  ENT-SGLUF               PIC  X(002).                     
           03  ENT-CEP                 PIC  9(005) COMP-3.              
           03  ENT-COMPLCEP            PIC  9(003) COMP-3.              
           03  ENT-IDENTIF             PIC  X(002).                     
           03  FILLER                  PIC  X(007).                     
                                                                        
      *----------------------------------------------------------------*
      *    INPUT:  ARQUIVO DE ENTRADA                                  *
      *            ORG. SEQUENCIAL     -   LRECL   =   690             *
      *----------------------------------------------------------------*
                                                                        
       FD  ARQDB2                                                       
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
           COPY 'I#CLLPFD'.
                                                                        
      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE AVISOS DE SAIDA                          *
      *            ORG. SEQUENCIAL     -   LRECL   =   692             *
      *----------------------------------------------------------------*
                                                                        
       FD  ARQSAIDA                                                     
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  REG-ARQSAIDA.                                                
           03  SAI-REGTO               PIC  X(690).
           03  SAI-UF-ENDER-IMOVEL     PIC  X(002).                     
                                                                        
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
           '*** CLLP7614 - INICIO DA AREA DE WORKING ***'.              
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE        
           '*** ACUMULADORES ***'.                                      
      *----------------------------------------------------------------*
                                                                        
       77  ACU-LIDOS-ARQENT            PIC  9(009) COMP-3  VALUE ZEROS. 
       77  ACU-LIDOS-ARQDB2            PIC  9(009) COMP-3  VALUE ZEROS. 
       77  ACU-GRAVA-ARQSAIDA          PIC  9(009) COMP-3  VALUE ZEROS. 
       77  ACU-GRAVA-RELATO            PIC  9(009) COMP-3  VALUE ZEROS. 
       77  ACU-GRAVA-RELINCO           PIC  9(009) COMP-3  VALUE ZEROS. 
       77  ACU-INCONSIS-UF             PIC  9(009) COMP-3  VALUE ZEROS. 
       77  ACU-INCONSIS-AVISOS         PIC  9(009) COMP-3  VALUE ZEROS. 
       77  ACU-INCONSIS-ENDERECOS      PIC  9(009) COMP-3  VALUE ZEROS. 
                                                                        
       77  ACU-LINHAS-RELINCO          PIC  9(003) COMP-3  VALUE 99.    
       77  ACU-LINHAS-RELATO           PIC  9(003) COMP-3  VALUE 99.    
       77  ACU-PAGINA-RELINCO          PIC  9(003) COMP-3  VALUE ZEROS. 
       77  ACU-PAGINA-RELATO           PIC  9(003) COMP-3  VALUE ZEROS. 
                                                                        
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE        
           '*** AREA DE FILE STATUS ***'.                               
      *----------------------------------------------------------------*
                                                                        
       77  WRK-FS-ARQENT               PIC  X(002)         VALUE SPACES.
       77  WRK-FS-ARQDB2               PIC  X(002)         VALUE SPACES.
       77  WRK-FS-ARQSAIDA             PIC  X(002)         VALUE SPACES.
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
           '*** CHAVES PARA BALANCE-LINE ***'.                          
      *----------------------------------------------------------------*
                                                                        
       01  WRK-CHAVE-ARQENT.                                            
           03  WRK-CHV-ENT-AGENCIA     PIC  9(005)         VALUE ZEROS. 
           03  WRK-CHV-ENT-CONTA       PIC  9(007)         VALUE ZEROS. 
           03  WRK-CHV-ENT-CONTRATO    PIC  9(007)         VALUE ZEROS. 
                                                                        
       01  WRK-CHAVE-ARQDB2.                                            
           03  WRK-CHV-DB2-AGENCIA     PIC  9(005)         VALUE ZEROS. 
           03  WRK-CHV-DB2-CONTA       PIC  9(007)         VALUE ZEROS. 
           03  WRK-CHV-DB2-CONTRATO    PIC  9(007)         VALUE ZEROS. 
                                                                        
       01  WRK-CHAVE-ARQDB2-ANT        PIC  X(019)         VALUE SPACES.
                                                                        
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
                                                                        
       01  WRK-MASK-1                  PIC  ZZZ.ZZZ.ZZ9.                
       01  WRK-MASK-2                  PIC  ZZZ.ZZZ.ZZ9.                
       01  WRK-MASK-3                  PIC  ZZZ.ZZZ.ZZ9.                
       01  WRK-MASK-4                  PIC  ZZZ.ZZZ.ZZ9.                
       01  WRK-MASK-5                  PIC  ZZZ.ZZZ.ZZ9.                
                                                                        
       01  WRK-CPF.                                                     
           03  WRK-CPF-NUM             PIC  999.999.999.                
           03  FILLER                  PIC  X(001)         VALUE '-'.   
           03  WRK-CPF-CTR             PIC  99.                         
                                                                        
       01  WRK-CGC.                                                     
           03  WRK-CGC-NUM             PIC  999.999.999.                
           03  FILLER                  PIC  X(001)         VALUE '/'.   
           03  WRK-CGC-FIL             PIC  9999.                       
           03  FILLER                  PIC  X(001)         VALUE '-'.   
           03  WRK-CGC-CTR             PIC  99.                         
                                                                        
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
               'CLLP7614                                         B A N C
      -        ' O   B R A D E S C O   S / A                            
      -        '            PAG:'.                                      
           03  CB1-PAGINA              PIC  ZZ9.                        
                                                                        
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
               '                            CLIENTES COM OPER. DE CREDIT
      -        'O IMOBILIARIO QUE NAO TIVERAM'.                         
           03  CB2A-HOR                PIC  9(002)         VALUE ZEROS. 
           03  FILLER                  PIC  X(001)         VALUE ':'.   
           03  CB2A-MIN                PIC  9(002)         VALUE ZEROS. 
           03  FILLER                  PIC  X(001)         VALUE ':'.   
           03  CB2A-SEG                PIC  9(002)         VALUE ZEROS. 
                                                                        
       01  CABEC7A.                                                     
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(132)         VALUE        
               '                                    INCONSISTENCIA NO CO
      -        'NFRONTO COM O ARQ. DE ENDERECO DO IMOVEL'.              
                                                                        
       01  CABEC7.                                                      
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(132)         VALUE        
               '                            POR INCONSISTENCIA NO CONFRO
      -        'NTO COM O ARQ. DE ENDERECO DO IMOVEL'.                  
                                                                        
       01  CABEC8.                                                      
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(063)         VALUE        
               '                                                     SIS
      -        'TEMA = '.                                               
           03  CB8-SISTEMA             PIC  X(004)         VALUE SPACES.
                                                                        
       01  CABEC3.                                                      
           03  FILLER                  PIC  X(133)         VALUE SPACES.
                                                                        
       01  CABEC4.                                                      
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(132)         VALUE        
               '       AGENCIA    CONTA  CART  CONTRATO                 
      -        ' CPF/CGC                 DATA DE VENCIMENTO'.           
                                                                        
       01  CABEC9.                                                      
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(132)         VALUE        
               '       CLIENTE'.                                        
                                                                        
       01  CABEC5.                                                      
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(132)         VALUE        
               '       ENDERECO                                 NUMERO  
      -        ' COMPLEMENTO    BAIRRO'.                                
                                                                        
       01  CABEC6.                                                      
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(132)         VALUE        
               '       CIDADE                                       UF  
      -        ' MOTIVO DA EXCLUSAO'.                                   
                                                                        
       01  LINDET1.                                                     
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(009)         VALUE SPACES.
           03  LD1-AGENCIA             PIC  ZZZZ9.                      
           03  FILLER                  PIC  X(002)         VALUE SPACES.
           03  LD1-CONTA               PIC  ZZZZZZ9.                    
           03  FILLER                  PIC  X(003)         VALUE SPACES.
           03  LD1-CARTEIRA            PIC  X(003)         VALUE SPACES.
           03  FILLER                  PIC  X(003)         VALUE SPACES.
           03  LD1-CONTRATO            PIC  ZZZZZZ9.                    
           03  FILLER                  PIC  X(018)         VALUE SPACES.
           03  LD1-CGC-CPF             PIC  X(019)         VALUE SPACES.
           03  FILLER                  PIC  X(009)         VALUE SPACES.
           03  LD1-DATA                PIC  X(010)         VALUE SPACES.
                                                                        
       01  LINDET2.                                                     
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(007)         VALUE SPACES.
           03  LD2-CLIENTE             PIC  X(040)         VALUE SPACES.
                                                                        
       01  LINDET3.                                                     
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(007)         VALUE SPACES.
           03  LD3-ENDERECO            PIC  X(040)         VALUE SPACES.
           03  FILLER                  PIC  X(002)         VALUE SPACES.
           03  LD3-NUMERO.                                              
               05  LD3-NUMERO-N        PIC  ZZZZ9.                      
           03  FILLER                  PIC  X(003)         VALUE SPACES.
           03  LD3-COMPLEMENTO         PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(005)         VALUE SPACES.
           03  LD3-BAIRRO              PIC  X(020)         VALUE SPACES.
                                                                        
       01  LINDET4.                                                     
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(007)         VALUE SPACES.
           03  LD4-CIDADE              PIC  X(030)         VALUE SPACES.
           03  FILLER                  PIC  X(015)         VALUE SPACES.
           03  LD4-UF                  PIC  X(002)         VALUE SPACES.
           03  FILLER                  PIC  X(003)         VALUE SPACES.
           03  LD4-MOTIVO              PIC  X(066)         VALUE SPACES.
                                                                        
       01  LINTOT1.                                                     
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(070)         VALUE        
               '       TOTAL DE CONTRATOS COM UF INCONSISTENTE          
      -        '          :'.                                           
           03  LT1-INCONSIS-UF         PIC  ZZZ.ZZZ.ZZ9.                
                                                                        
       01  LINTOT1A.                                                    
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(028)         VALUE        
               '       TOTAL DE CONTRATOS :'.                           
           03  LT1A-CONTRATOS          PIC  ZZZ.ZZZ.ZZ9.                
                                                                        
       01  LINTOT2.                                                     
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(070)         VALUE        
               '       TOTAL DE CONTRATOS NAO ENCONTRADOS NO ARQUIVO DE 
      -        'ENDERECOS :'.                                           
           03  LT2-INCONSIS-ENDERECOS  PIC  ZZZ.ZZZ.ZZ9.                
                                                                        
       01  LINTOT3.                                                     
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(070)         VALUE        
               '       TOTAL DE CONTRATOS NAO ENCONTRADOS NO ARQUIVO DE 
      -        'AVISOS    :'.                                           
           03  LT3-INCONSIS-AVISOS     PIC  ZZZ.ZZZ.ZZ9.                
                                                                        
       01  LINTOT4.                                                     
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(070)         VALUE        
               '       TOTAL DE CONTRATOS EXCLUIDOS                     
      -        '          :'.                                           
           03  LT4-EXCLUIDOS           PIC  ZZZ.ZZZ.ZZ9.                
                                                                        
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE        
           '*** TABELA DE UNIDADES DA FEDERACAO ***'.                   
      *----------------------------------------------------------------*
                                                                        
           COPY 'I#LPCLUF'.
                                                                        
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE        
           '*** AREA DA POOL7100 ***'.                                  
      *----------------------------------------------------------------*
                                                                        
           COPY POL7100C.                                               
                                                                        
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE        
           '*** CLLP7614 - FIM DA AREA DE WORKING ***'.                 
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       LINKAGE                         SECTION.                         
      *----------------------------------------------------------------*
                                                                        
       01  LKG-AREA.                                                    
           03  LKG-TAMANHO             PIC S9(004) COMP.                
           03  LKG-PARM                PIC  X(004).                     
                                                                        
      *================================================================*
       PROCEDURE DIVISION              USING LKG-AREA.                  
      *================================================================*
                                                                        
      *----------------------------------------------------------------*
       000000-INICIAR                  SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           PERFORM 100000-INICIALIZAR.                                  
                                                                        
           PERFORM 200000-VERIFICAR-VAZIO.                              
                                                                        
           PERFORM 300000-PROCESSAR    UNTIL                            
                   WRK-CHAVE-ARQENT    EQUAL HIGH-VALUES AND            
                   WRK-CHAVE-ARQDB2    EQUAL HIGH-VALUES.               
                                                                        
           PERFORM 400000-FINALIZAR.                                    
                                                                        
      *----------------------------------------------------------------*
       000000-99-FIM.                  EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       100000-INICIALIZAR              SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           OPEN INPUT  ARQENT                                           
                       ARQDB2                                           
                OUTPUT ARQSAIDA                                         
                       RELINCO                                          
                       RELATO.                                          
                                                                        
           MOVE WRK-ABERTURA           TO WRK-OPERACAO.                 
                                                                        
           PERFORM 110000-TESTAR-FILE-STATUS.                           
                                                                        
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
                                                                        
           MOVE LKG-PARM               TO CB8-SISTEMA.                  
                                                                        
      *----------------------------------------------------------------*
       100000-99-FIM.                  EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       110000-TESTAR-FILE-STATUS       SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           PERFORM 111000-TESTAR-FS-ARQENT.                             
                                                                        
           PERFORM 112000-TESTAR-FS-ARQDB2.                             
                                                                        
           PERFORM 113000-TESTAR-FS-ARQSAIDA.                           
                                                                        
           PERFORM 114000-TESTAR-FS-RELINCO.                            
                                                                        
           PERFORM 115000-TESTAR-FS-RELATO.                             
                                                                        
      *----------------------------------------------------------------*
       110000-99-FIM.                  EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       111000-TESTAR-FS-ARQENT         SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           IF  WRK-FS-ARQENT           NOT EQUAL ZEROS                  
               MOVE 'ARQENT  '         TO WRK-NOME-ARQ                  
               MOVE WRK-FS-ARQENT      TO WRK-FILE-STATUS               
               MOVE WRK-ERRO-ARQUIVO   TO ERR-TEXTO                     
               PERFORM 999999-PROCESSAR-ROTINA-ERRO                     
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       111000-99-FIM.                  EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       112000-TESTAR-FS-ARQDB2         SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           IF  WRK-FS-ARQDB2           NOT EQUAL ZEROS                  
               MOVE 'ARQDB2  '         TO WRK-NOME-ARQ                  
               MOVE WRK-FS-ARQDB2      TO WRK-FILE-STATUS               
               MOVE WRK-ERRO-ARQUIVO   TO ERR-TEXTO                     
               PERFORM 999999-PROCESSAR-ROTINA-ERRO                     
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       112000-99-FIM.                  EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       113000-TESTAR-FS-ARQSAIDA       SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           IF  WRK-FS-ARQSAIDA         NOT EQUAL ZEROS                  
               MOVE 'ARQSAIDA'         TO WRK-NOME-ARQ                  
               MOVE WRK-FS-ARQSAIDA    TO WRK-FILE-STATUS               
               MOVE WRK-ERRO-ARQUIVO   TO ERR-TEXTO                     
               PERFORM 999999-PROCESSAR-ROTINA-ERRO                     
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       113000-99-FIM.                  EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       114000-TESTAR-FS-RELINCO        SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           IF  WRK-FS-RELINCO          NOT EQUAL ZEROS                  
               MOVE 'RELINCO '         TO WRK-NOME-ARQ                  
               MOVE WRK-FS-RELINCO     TO WRK-FILE-STATUS               
               MOVE WRK-ERRO-ARQUIVO   TO ERR-TEXTO                     
               PERFORM 999999-PROCESSAR-ROTINA-ERRO                     
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       114000-99-FIM.                  EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       115000-TESTAR-FS-RELATO         SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           IF  WRK-FS-RELATO           NOT EQUAL ZEROS                  
               MOVE 'RELATO  '         TO WRK-NOME-ARQ                  
               MOVE WRK-FS-RELATO      TO WRK-FILE-STATUS               
               MOVE WRK-ERRO-ARQUIVO   TO ERR-TEXTO                     
               PERFORM 999999-PROCESSAR-ROTINA-ERRO                     
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       115000-99-FIM.                  EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       200000-VERIFICAR-VAZIO          SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           PERFORM 210000-LER-ARQENT.                                   
                                                                        
           IF  ACU-LIDOS-ARQENT        EQUAL ZEROS                      
               DISPLAY '*************** CLLP7614 ***************'       
               DISPLAY '*                                      *'       
               DISPLAY '*       ARQUIVO ARQENT   VAZIO         *'       
               DISPLAY '*      PROCESSAMENTO  ENCERRADO        *'       
               DISPLAY '*                                      *'       
               DISPLAY '*************** CLLP7614 ***************'       
               PERFORM 400000-FINALIZAR                                 
           END-IF.                                                      
                                                                        
           PERFORM 220000-LER-ARQDB2.                                   
                                                                        
           IF  ACU-LIDOS-ARQDB2        EQUAL ZEROS                      
               DISPLAY '*************** CLLP7614 ***************'       
               DISPLAY '*                                      *'       
               DISPLAY '*       ARQUIVO ARQDB2   VAZIO         *'       
               DISPLAY '*      PROCESSAMENTO  ENCERRADO        *'       
               DISPLAY '*                                      *'       
               DISPLAY '*************** CLLP7614 ***************'       
               PERFORM 400000-FINALIZAR                                 
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       200000-99-FIM.                  EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       210000-LER-ARQENT               SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           READ ARQENT.                                                 
                                                                        
           IF  WRK-FS-ARQENT           EQUAL '10'                       
               MOVE HIGH-VALUES        TO WRK-CHAVE-ARQENT              
               GO TO 210000-99-FIM                                      
           END-IF.                                                      
                                                                        
           MOVE WRK-LEITURA            TO WRK-OPERACAO.                 
                                                                        
           PERFORM 111000-TESTAR-FS-ARQENT.                             
                                                                        
           MOVE ENT-AGENCIA            TO WRK-CHV-ENT-AGENCIA.          
           MOVE ENT-CONTA              TO WRK-CHV-ENT-CONTA.            
           MOVE ENT-CONTRATO           TO WRK-CHV-ENT-CONTRATO.         
                                                                        
           ADD 1                       TO ACU-LIDOS-ARQENT.             
                                                                        
      *----------------------------------------------------------------*
       210000-99-FIM.                  EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       220000-LER-ARQDB2               SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           READ ARQDB2.                                                 
                                                                        
           IF  WRK-FS-ARQDB2           EQUAL '10'                       
               MOVE HIGH-VALUES        TO WRK-CHAVE-ARQDB2              
               GO TO 220000-99-FIM                                      
           END-IF.                                                      
                                                                        
           MOVE WRK-LEITURA            TO WRK-OPERACAO.                 
                                                                        
           PERFORM 112000-TESTAR-FS-ARQDB2.                             
                                                                        
           MOVE DB2-AGENCIA            TO WRK-CHV-DB2-AGENCIA.          
           MOVE DB2-CONTA              TO WRK-CHV-DB2-CONTA.            
           MOVE DB2-CONTRATO           TO WRK-CHV-DB2-CONTRATO.         
                                                                        
           ADD 1                       TO ACU-LIDOS-ARQDB2.             
                                                                        
      *----------------------------------------------------------------*
       220000-99-FIM.                  EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       300000-PROCESSAR                SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.                 
                                                                        
           IF  WRK-CHAVE-ARQENT        EQUAL WRK-CHAVE-ARQDB2           
               PERFORM 310000-TRATAR-CORRESPONDENTE                     
               PERFORM 220000-LER-ARQDB2                                
                       UNTIL           WRK-CHAVE-ARQDB2                 
                       NOT EQUAL       WRK-CHAVE-ARQENT                 
               PERFORM 210000-LER-ARQENT                                
           ELSE                                                         
               IF  WRK-CHAVE-ARQENT    LESS  WRK-CHAVE-ARQDB2           
                   PERFORM 320000-EMITIR-RELINCO-ARQENT                 
                   PERFORM 210000-LER-ARQENT                            
               ELSE                                                     
                   PERFORM 330000-EMITIR-RELINCO-ARQDB2                 
                   MOVE WRK-CHAVE-ARQDB2 TO WRK-CHAVE-ARQDB2-ANT        
                   PERFORM 220000-LER-ARQDB2                            
                           UNTIL       WRK-CHAVE-ARQDB2                 
                           NOT EQUAL   WRK-CHAVE-ARQDB2-ANT             
               END-IF                                                   
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       300000-99-FIM.                  EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       310000-TRATAR-CORRESPONDENTE    SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           SET LPCL-INDX               TO 1.                            
                                                                        
           SEARCH LPCL-OCC-UF                                           
                                                                        
             AT END                                                     
               PERFORM 311000-EMITIR-RELINCO                            
                                                                        
             WHEN                                                       
               LPCL-OCC-UF(LPCL-INDX)  EQUAL ENT-SGLUF                  
               PERFORM 313000-EMITIR-RELATO                             
               PERFORM 312000-GRAVAR-ARQSAIDA                           
                       UNTIL           WRK-CHAVE-ARQDB2                 
                       NOT EQUAL       WRK-CHAVE-ARQENT.                
                                                                        
      *----------------------------------------------------------------*
       311000-99-FIM.                  EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       311000-EMITIR-RELINCO           SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           IF  ACU-LINHAS-RELINCO      GREATER 56                       
               PERFORM 321000-EMITIR-CABECALHO                          
           END-IF.                                                      
                                                                        
           MOVE ENT-AGENCIA            TO LD1-AGENCIA.                  
           MOVE ENT-CONTA              TO LD1-CONTA.                    
BRQ141     MOVE DB2-CARTEIRA           TO LD1-CARTEIRA.                 
           MOVE ENT-CONTRATO           TO LD1-CONTRATO.                 
                                                                        
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
                                                                        
           MOVE DB2-VCTO               TO LD1-DATA.                     
           MOVE '/'                    TO LD1-DATA(3:1)                 
                                          LD1-DATA(6:1).                
           MOVE DB2-NOME-DEVEDOR       TO LD2-CLIENTE.                  
           MOVE ENT-LOGRADOURO         TO LD3-ENDERECO.                 
           MOVE ENT-NRO-LOGDR          TO LD3-NUMERO-N.                 
           MOVE ENT-COMPL-LOGDR        TO LD3-COMPLEMENTO.              
           MOVE ENT-BAIRRO-LOGDR       TO LD3-BAIRRO.                   
           MOVE ENT-MUNICIPIO          TO LD4-CIDADE.                   
           MOVE ENT-SGLUF              TO LD4-UF.                       
           MOVE 'UF DO ENDERECO DO IMOVEL INCONSISTENTE'                
                                       TO LD4-MOTIVO                    
                                                                        
           PERFORM 322000-EMITIR-DETALHE.                               
                                                                        
           ADD 1                       TO ACU-INCONSIS-UF.              
                                                                        
      *----------------------------------------------------------------*
       311000-99-FIM.                  EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       313000-EMITIR-RELATO            SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           IF  ACU-LINHAS-RELATO       GREATER 56                       
               PERFORM 313100-EMITIR-CABECALHO                          
           END-IF.                                                      
                                                                        
           MOVE ENT-AGENCIA            TO LD1-AGENCIA.                  
           MOVE ENT-CONTA              TO LD1-CONTA.                    
BRQ141     MOVE DB2-CARTEIRA           TO LD1-CARTEIRA.                 
           MOVE ENT-CONTRATO           TO LD1-CONTRATO.                 
                                                                        
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
                                                                        
           MOVE DB2-VCTO               TO LD1-DATA.                     
           MOVE '/'                    TO LD1-DATA(3:1)                 
                                          LD1-DATA(6:1).                
           MOVE DB2-NOME-DEVEDOR       TO LD2-CLIENTE.                  
           MOVE ENT-LOGRADOURO         TO LD3-ENDERECO.                 
           MOVE ENT-NRO-LOGDR          TO LD3-NUMERO-N.                 
           MOVE ENT-COMPL-LOGDR        TO LD3-COMPLEMENTO.              
           MOVE ENT-BAIRRO-LOGDR       TO LD3-BAIRRO.                   
           MOVE ENT-MUNICIPIO          TO LD4-CIDADE.                   
           MOVE ENT-SGLUF              TO LD4-UF.                       
           MOVE SPACES                 TO LD4-MOTIVO.                   
                                                                        
           PERFORM 313200-EMITIR-DETALHE.                               
                                                                        
      *----------------------------------------------------------------*
       313000-99-FIM.                  EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       313100-EMITIR-CABECALHO         SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           ADD 1                       TO ACU-PAGINA-RELATO.            
                                                                        
           MOVE ACU-PAGINA-RELATO      TO CB1-PAGINA.                   
           MOVE CABEC1                 TO REG-RELATO.                   
           MOVE 'A'                    TO REG-RELATO(10:1).             
                                                                        
           WRITE REG-RELATO.                                            
                                                                        
           PERFORM 115000-TESTAR-FS-RELATO.                             
                                                                        
           WRITE REG-RELATO            FROM CABEC2A.                    
                                                                        
           PERFORM 115000-TESTAR-FS-RELATO.                             
                                                                        
           WRITE REG-RELATO            FROM CABEC7A.                    
                                                                        
           PERFORM 115000-TESTAR-FS-RELATO.                             
                                                                        
           WRITE REG-RELATO            FROM CABEC8.                     
                                                                        
           PERFORM 115000-TESTAR-FS-RELATO.                             
                                                                        
           WRITE REG-RELATO            FROM CABEC3.                     
                                                                        
           PERFORM 115000-TESTAR-FS-RELATO.                             
                                                                        
           WRITE REG-RELATO            FROM CABEC4.                     
                                                                        
           PERFORM 115000-TESTAR-FS-RELATO.                             
                                                                        
           WRITE REG-RELATO            FROM CABEC9.                     
                                                                        
           PERFORM 115000-TESTAR-FS-RELATO.                             
                                                                        
           WRITE REG-RELATO            FROM CABEC5.                     
                                                                        
           PERFORM 115000-TESTAR-FS-RELATO.                             
                                                                        
           MOVE CABEC6                 TO REG-RELATO.                   
           MOVE SPACES                 TO REG-RELATO(59:18).            
                                                                        
           WRITE REG-RELATO.                                            
                                                                        
           PERFORM 115000-TESTAR-FS-RELATO.                             
                                                                        
           WRITE REG-RELATO            FROM CABEC3.                     
                                                                        
           PERFORM 115000-TESTAR-FS-RELATO.                             
                                                                        
           MOVE 10                     TO ACU-LINHAS-RELATO.            
                                                                        
      *----------------------------------------------------------------*
       313100-99-FIM.                  EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       313200-EMITIR-DETALHE           SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           WRITE REG-RELATO            FROM LINDET1.                    
                                                                        
           PERFORM 115000-TESTAR-FS-RELATO.                             
                                                                        
           WRITE REG-RELATO            FROM LINDET2.                    
                                                                        
           PERFORM 115000-TESTAR-FS-RELATO.                             
                                                                        
           WRITE REG-RELATO            FROM LINDET3.                    
                                                                        
           PERFORM 115000-TESTAR-FS-RELATO.                             
                                                                        
           WRITE REG-RELATO            FROM LINDET4.                    
                                                                        
           PERFORM 115000-TESTAR-FS-RELATO.                             
                                                                        
           WRITE REG-RELATO            FROM CABEC3.                     
                                                                        
           PERFORM 115000-TESTAR-FS-RELATO.                             
                                                                        
           ADD 5                       TO ACU-LINHAS-RELATO.            
           ADD 1                       TO ACU-GRAVA-RELATO.             
                                                                        
      *----------------------------------------------------------------*
       313200-99-FIM.                  EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       312000-GRAVAR-ARQSAIDA          SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE DB2-REGTO              TO SAI-REGTO.                    
           MOVE ENT-SGLUF              TO SAI-UF-ENDER-IMOVEL.          
                                                                        
           WRITE REG-ARQSAIDA.                                          
                                                                        
           PERFORM 113000-TESTAR-FS-ARQSAIDA.                           
                                                                        
           ADD 1                       TO ACU-GRAVA-ARQSAIDA.           
                                                                        
           PERFORM 220000-LER-ARQDB2.                                   
                                                                        
      *----------------------------------------------------------------*
       312000-99-FIM.                  EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       320000-EMITIR-RELINCO-ARQENT    SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           IF  ACU-LINHAS-RELINCO      GREATER 56                       
               PERFORM 321000-EMITIR-CABECALHO                          
           END-IF.                                                      
                                                                        
           MOVE ENT-AGENCIA            TO LD1-AGENCIA.                  
           MOVE ENT-CONTA              TO LD1-CONTA.                    
           MOVE SPACES                 TO LD1-CARTEIRA.                 
           MOVE ENT-CONTRATO           TO LD1-CONTRATO.                 
           MOVE SPACES                 TO LD1-CGC-CPF                   
                                          LD1-DATA                      
                                          LD2-CLIENTE.                  
           MOVE ENT-LOGRADOURO         TO LD3-ENDERECO.                 
           MOVE ENT-NRO-LOGDR          TO LD3-NUMERO-N.                 
           MOVE ENT-COMPL-LOGDR        TO LD3-COMPLEMENTO.              
           MOVE ENT-BAIRRO-LOGDR       TO LD3-BAIRRO.                   
           MOVE ENT-MUNICIPIO          TO LD4-CIDADE.                   
           MOVE ENT-SGLUF              TO LD4-UF.                       
           MOVE 'NAO FOI ENCONTRADO CORRESPONDENTE NO ARQUIVO DE AVISOS'
                                       TO LD4-MOTIVO.                   
                                                                        
           PERFORM 322000-EMITIR-DETALHE.                               
                                                                        
           ADD 1                       TO ACU-INCONSIS-AVISOS.          
                                                                        
      *----------------------------------------------------------------*
       320000-99-FIM.                  EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       321000-EMITIR-CABECALHO         SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           ADD 1                       TO ACU-PAGINA-RELINCO.           
                                                                        
           MOVE ACU-PAGINA-RELINCO     TO CB1-PAGINA.                   
                                                                        
           WRITE REG-RELINCO           FROM CABEC1.                     
                                                                        
           PERFORM 114000-TESTAR-FS-RELINCO.                            
                                                                        
           WRITE REG-RELINCO           FROM CABEC2.                     
                                                                        
           PERFORM 114000-TESTAR-FS-RELINCO.                            
                                                                        
           WRITE REG-RELINCO           FROM CABEC7.                     
                                                                        
           PERFORM 114000-TESTAR-FS-RELINCO.                            
                                                                        
           WRITE REG-RELINCO           FROM CABEC8.                     
                                                                        
           PERFORM 114000-TESTAR-FS-RELINCO.                            
                                                                        
           WRITE REG-RELINCO           FROM CABEC3.                     
                                                                        
           PERFORM 114000-TESTAR-FS-RELINCO.                            
                                                                        
           WRITE REG-RELINCO           FROM CABEC4.                     
                                                                        
           PERFORM 114000-TESTAR-FS-RELINCO.                            
                                                                        
           WRITE REG-RELINCO           FROM CABEC9.                     
                                                                        
           PERFORM 114000-TESTAR-FS-RELINCO.                            
                                                                        
           WRITE REG-RELINCO           FROM CABEC5.                     
                                                                        
           PERFORM 114000-TESTAR-FS-RELINCO.                            
                                                                        
           WRITE REG-RELINCO           FROM CABEC6.                     
                                                                        
           PERFORM 114000-TESTAR-FS-RELINCO.                            
                                                                        
           WRITE REG-RELINCO           FROM CABEC3.                     
                                                                        
           PERFORM 114000-TESTAR-FS-RELINCO.                            
                                                                        
           MOVE 10                     TO ACU-LINHAS-RELINCO.           
                                                                        
      *----------------------------------------------------------------*
       321000-99-FIM.                  EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       322000-EMITIR-DETALHE           SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           WRITE REG-RELINCO           FROM LINDET1.                    
                                                                        
           PERFORM 114000-TESTAR-FS-RELINCO.                            
                                                                        
           WRITE REG-RELINCO           FROM LINDET2.                    
                                                                        
           PERFORM 114000-TESTAR-FS-RELINCO.                            
                                                                        
           WRITE REG-RELINCO           FROM LINDET3.                    
                                                                        
           PERFORM 114000-TESTAR-FS-RELINCO.                            
                                                                        
           WRITE REG-RELINCO           FROM LINDET4.                    
                                                                        
           PERFORM 114000-TESTAR-FS-RELINCO.                            
                                                                        
           WRITE REG-RELINCO           FROM CABEC3.                     
                                                                        
           PERFORM 114000-TESTAR-FS-RELINCO.                            
                                                                        
           ADD 5                       TO ACU-LINHAS-RELINCO.           
           ADD 1                       TO ACU-GRAVA-RELINCO.            
                                                                        
      *----------------------------------------------------------------*
       322000-99-FIM.                  EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       330000-EMITIR-RELINCO-ARQDB2    SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           IF  ACU-LINHAS-RELINCO      GREATER 56                       
               PERFORM 321000-EMITIR-CABECALHO                          
           END-IF.                                                      
                                                                        
           MOVE DB2-AGENCIA            TO LD1-AGENCIA.                  
           MOVE DB2-CONTA              TO LD1-CONTA.                    
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
                                                                        
           MOVE DB2-VCTO               TO LD1-DATA.                     
           MOVE '/'                    TO LD1-DATA(3:1)                 
                                          LD1-DATA(6:1).                
           MOVE DB2-NOME-DEVEDOR       TO LD2-CLIENTE.                  
           MOVE SPACES                 TO LD3-ENDERECO                  
                                          LD3-NUMERO                    
                                          LD3-COMPLEMENTO               
                                          LD3-BAIRRO                    
                                          LD4-CIDADE                    
                                          LD4-UF.                       
           MOVE 'NAO FOI ENCONTRADO CORRESPONDENTE NO ARQUIVO DE ENDEREC
      -         'O DO IMOVEL'          TO LD4-MOTIVO.                   
                                                                        
           PERFORM 322000-EMITIR-DETALHE.                               
                                                                        
           ADD 1                       TO ACU-INCONSIS-ENDERECOS.       
                                                                        
      *----------------------------------------------------------------*
       330000-99-FIM.                  EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       400000-FINALIZAR                SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           IF  ACU-GRAVA-RELINCO       GREATER ZEROS                    
               PERFORM 410000-EMITIR-TOTAIS                             
           END-IF.                                                      
                                                                        
           IF  ACU-GRAVA-RELATO        GREATER ZEROS                    
               PERFORM 420000-EMITIR-TOTAIS                             
           END-IF.                                                      
                                                                        
           PERFORM 430000-EXIBIR-DISPLAY-PROCESS.                       
                                                                        
           CLOSE ARQENT                                                 
                 ARQDB2                                                 
                 ARQSAIDA                                               
                 RELINCO                                                
                 RELATO.                                                
                                                                        
           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.                 
                                                                        
           PERFORM 110000-TESTAR-FILE-STATUS.                           
                                                                        
           GOBACK.                                                      
                                                                        
      *----------------------------------------------------------------*
       400000-99-FIM.                  EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       410000-EMITIR-TOTAIS            SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.                 
           MOVE ACU-INCONSIS-UF        TO LT1-INCONSIS-UF.              
           MOVE ACU-INCONSIS-ENDERECOS TO LT2-INCONSIS-ENDERECOS.       
           MOVE ACU-INCONSIS-AVISOS    TO LT3-INCONSIS-AVISOS.          
                                                                        
           COMPUTE LT4-EXCLUIDOS       = ACU-INCONSIS-UF                
                                       + ACU-INCONSIS-ENDERECOS         
                                       + ACU-INCONSIS-AVISOS.           
                                                                        
           WRITE REG-RELINCO           FROM CABEC3.                     
                                                                        
           PERFORM 114000-TESTAR-FS-RELINCO.                            
                                                                        
           WRITE REG-RELINCO           FROM LINTOT1.                    
                                                                        
           PERFORM 114000-TESTAR-FS-RELINCO.                            
                                                                        
           WRITE REG-RELINCO           FROM LINTOT2.                    
                                                                        
           PERFORM 114000-TESTAR-FS-RELINCO.                            
                                                                        
           WRITE REG-RELINCO           FROM LINTOT3.                    
                                                                        
           PERFORM 114000-TESTAR-FS-RELINCO.                            
                                                                        
      *----------------------------------------------------------------*
       410000-99-FIM.                  EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       420000-EMITIR-TOTAIS            SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.                 
           MOVE ACU-GRAVA-RELATO       TO LT1A-CONTRATOS.               
                                                                        
           WRITE REG-RELATO            FROM CABEC3.                     
                                                                        
           PERFORM 115000-TESTAR-FS-RELATO.                             
                                                                        
           WRITE REG-RELATO            FROM LINTOT1A.                   
                                                                        
           PERFORM 115000-TESTAR-FS-RELATO.                             
                                                                        
      *----------------------------------------------------------------*
       420000-99-FIM.                  EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       430000-EXIBIR-DISPLAY-PROCESS   SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE ACU-LIDOS-ARQENT       TO WRK-MASK-1.                   
           MOVE ACU-LIDOS-ARQDB2       TO WRK-MASK-2.                   
           MOVE ACU-GRAVA-ARQSAIDA     TO WRK-MASK-3.                   
           MOVE ACU-GRAVA-RELINCO      TO WRK-MASK-4.                   
           MOVE ACU-GRAVA-RELATO       TO WRK-MASK-5.                   
                                                                        
           DISPLAY '*************** CLLP7614 ***************'.          
           DISPLAY '*                                      *'.          
           DISPLAY '*                 ' LKG-PARM '                 *'.  
           DISPLAY '*                                      *'.          
           DISPLAY '* TOT. LIDOS    ARQENT   : ' WRK-MASK-1  ' *'.      
           DISPLAY '* TOT. LIDOS    ARQDB2   : ' WRK-MASK-2  ' *'.      
           DISPLAY '* TOT. GRAVADOS ARQSAIDA : ' WRK-MASK-3  ' *'.      
           DISPLAY '* TOT. GRAVADOS RELINCO  : ' WRK-MASK-4  ' *'.      
           DISPLAY '* TOT. GRAVADOS RELATO   : ' WRK-MASK-5  ' *'.      
           DISPLAY '*                                      *'.          
           DISPLAY '*************** CLLP7614 ***************'.          
                                                                        
      *----------------------------------------------------------------*
       430000-99-FIM.                  EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       999999-PROCESSAR-ROTINA-ERRO    SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE 'APL'                  TO ERR-TIPO-ACESSO.              
           MOVE 'CLLP7614'             TO ERR-PGM.                      
                                                                        
           CALL 'POOL7100'             USING WRK-BATCH                  
                                             ERRO-AREA.                 
                                                                        
           GOBACK.                                                      
                                                                        
      *----------------------------------------------------------------*
       999999-99-FIM.                  EXIT.                            
      *----------------------------------------------------------------*

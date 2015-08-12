      *================================================================*
       IDENTIFICATION                  DIVISION.                        
      *================================================================*
                                                                        
       PROGRAM-ID.                     CLLP7103.                        
       AUTHOR.                         CPMACT 1.4.                      
                                                                        
      *REMARKS.                                                         
      *----------------------------------------------------------------*
      *    PROGRAMA ORIGINALMENTE CODIFICADO EM ASSEMBLER,              
      *    CONVERTIDO PARA COBOL ENTERPRISE PELO CONVERSOR:             
      *    CPMBRAXIS   "ACT 1.4 -  ASSEMBLER-COBOL TRANSLATOR".         
      *INSTALLATION.                                                    
      *DATE-WRITTEN.                                                    
      *DATE-COMPILED.                                                   
      *SECURITY.                                                        
                                                                        
      *----------------------------------------------------------------*
      *                                                                *
      *    CPM BRAXIS                                                  *
      *    EQUIPE DE CONVERSAO ASSEMBLER X COBOL ENTERPRISE            *
      *    DATA DE CONVERSAO: MAIO/2012                                *
      *    OBJETIVO: CONFRONTO ARQDB22 X LPCLB037 - CPF DEV/AVAL.      *
      *                                                                *
      *----------------------------------------------------------------*
                                                                        
      *================================================================*
       ENVIRONMENT                     DIVISION.                        
      *================================================================*
       CONFIGURATION                   SECTION.                         
      *================================================================*
                                                                        
       SPECIAL-NAMES.                                                   
                                                                        
           DECIMAL-POINT               IS  COMMA                        
           C01                         IS  CANAL1.                      
                                                                        
      *----------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.                         
      *----------------------------------------------------------------*
                                                                        
       FILE-CONTROL.                                                    
                                                                        
           SELECT ARQDB22  ASSIGN      TO  UT-S-ARQDB22                 
                  FILE STATUS          IS  WRK-FS-ARQDB22.              
                                                                        
           SELECT LPCLB037 ASSIGN      TO  UT-S-LPCLB037                
                  FILE STATUS          IS  WRK-FS-LPCLB037.             
                                                                        
           SELECT ARQATU   ASSIGN      TO  UT-S-ARQATU                  
                  FILE STATUS          IS  WRK-FS-ARQATU.               
                                                                        
           SELECT BLQREST  ASSIGN      TO  UT-S-BLQREST                 
                  FILE STATUS          IS  WRK-FS-BLQREST.              
                                                                        
           SELECT RELATO   ASSIGN      TO  UT-S-RELATO                  
                  FILE STATUS          IS  WRK-FS-RELATO.               
                                                                        
           SELECT RELATO2  ASSIGN      TO  UT-S-RELATO2                 
                  FILE STATUS          IS  WRK-FS-RELATO2.              
                                                                        
      *================================================================*
       DATA                            DIVISION.                        
      *================================================================*
                                                                        
       FILE                            SECTION.                         
                                                                        
      *----------------------------------------------------------------*
      *    INPUT : ARQUIVO ARQDB22                                     *
      *               ORG.          -  LRECL = 213                     *
      *----------------------------------------------------------------*
                                                                        
       FD  ARQDB22                                                      
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  FD-REG-ARQDB22              PIC  X(213).                     
                                                                        
      *----------------------------------------------------------------*
      *    INPUT : ARQUIVO LPCLB037                                    *
      *               ORG.          -  LRECL = 043                     *
      *----------------------------------------------------------------*
                                                                        
       FD  LPCLB037                                                     
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  FD-REG-LPCLB037             PIC  X(043).                     
                                                                        
      *----------------------------------------------------------------*
      *    OUTPUT : ARQUIVO ARQATU                                     *
      *               ORG.          -  LRECL = 213                     *
      *----------------------------------------------------------------*
                                                                        
       FD  ARQATU                                                       
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  FD-REG-ARQATU               PIC  X(213).                     
                                                                        
      *----------------------------------------------------------------*
      *    OUTPUT : ARQUIVO BLQREST                                    *
      *               ORG.          -  LRECL = 213                     *
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
      *    OUTPUT : ARQUIVO RELATO                                     *
      *               ORG.          -  LRECL = 133                     *
      *----------------------------------------------------------------*
                                                                        
       FD  RELATO                                                       
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  FD-REG-RELATO               PIC  X(133).                     
                                                                        
      *----------------------------------------------------------------*
      *    OUTPUT : ARQUIVO RELATO2                                    *
      *               ORG.          -  LRECL = 133                     *
      *----------------------------------------------------------------*
                                                                        
       FD  RELATO2                                                      
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  FD-REG-RELATO2              PIC  X(133).                     
                                                                        
      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.                         
                                                                        
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE        
           'INICIO DA WORKING STORAGE SECTION '.                        
      *----------------------------------------------------------------*
                                                                        
       77  WRK-BATCH                   PIC  X(008)         VALUE        
           'BATCH'.                                                     
                                                                        
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(020)         VALUE        
           'AREA DE FILE-STATUS'.                                       
      *----------------------------------------------------------------*
                                                                        
       77  WRK-FS-ARQDB22              PIC  X(002)         VALUE SPACES.
       77  WRK-FS-LPCLB037             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-ARQATU               PIC  X(002)         VALUE SPACES.
       77  WRK-FS-BLQREST              PIC  X(002)         VALUE SPACES.
       77  WRK-FS-RELATO               PIC  X(002)         VALUE SPACES.
       77  WRK-FS-RELATO2              PIC  X(002)         VALUE SPACES.
                                                                        
       77  WRK-ABERTURA                PIC  X(013)         VALUE        
           ' NA ABERTURA'.                                              
                                                                        
       77  WRK-LEITURA                 PIC  X(013)         VALUE        
           ' NA LEITURA '.                                              
                                                                        
       77  WRK-GRAVACAO                PIC  X(013)         VALUE        
           ' NA GRAVACAO'.                                              
                                                                        
       77  WRK-FECHAMENTO              PIC  X(013)         VALUE        
           'NO FECHAMENTO'.                                             
                                                                        
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(031)         VALUE        
           'MENSAGEM DE ERRO DE FILE-STATUS'.                           
      *----------------------------------------------------------------*
                                                                        
       01  WRK-ERRO-BRAD7100.                                           
           05 FILLER                   PIC  X(009)         VALUE        
           '*** ERRO '.                                                 
           05 WRK-OPERACAO             PIC  X(013)         VALUE SPACES.
           05 FILLER                   PIC  X(012)         VALUE        
           ' DO ARQUIVO '.                                              
           05 WRK-NOME-ARQ             PIC  X(008)         VALUE SPACES.
           05 FILLER                   PIC  X(017)         VALUE        
           ' - FILE-STATUS = '.                                         
           05 WRK-FILE-STATUS          PIC  X(002)         VALUE SPACES.
           05 FILLER                   PIC  X(004)         VALUE ' ***'.
           05 FILLER                   PIC  X(010)         VALUE SPACES.
                                                                        
      *----------------------------------------------------------------*
                                                                        
       01  WRK-WK.                                                      
           05 WRK-CPFNUM               PIC S9(009) COMP-3  VALUE ZEROS. 
CPMCAC     05 WRK-CPFNUM-R             REDEFINES           WRK-CPFNUM   
CPMCAC                                 PIC  X(005).                     
           05 WRK-CPFFIL               PIC S9(005) COMP-3  VALUE ZEROS. 
CPMCAC     05 WRK-CPFFIL-R             REDEFINES           WRK-CPFFIL   
CPMCAC                                 PIC  X(003).                     
           05 WRK-CPFCTR               PIC S9(003) COMP-3  VALUE ZEROS. 
CPMCAC     05 WRK-CPFCTR-R             REDEFINES           WRK-CPFCTR   
CPMCAC                                 PIC  X(002).                     
           05 WRK-NATU                 PIC  X(003)         VALUE SPACES.
           05 WRK-CONTR                PIC S9(007) COMP-3  VALUE ZEROS. 
           05 WRK-VCTO                 PIC  X(008)         VALUE SPACES.
           05 WRK-EMPRESA              PIC S9(005) COMP-3  VALUE ZEROS. 
           05 WRK-AGENCIA              PIC S9(005) COMP-3  VALUE ZEROS. 
           05 WRK-CONTA                PIC S9(007) COMP-3  VALUE ZEROS. 
           05 WRK-CART                 PIC  X(003)         VALUE SPACES.
           05 WRK-ID                   PIC  X(002)         VALUE SPACES.
           05 WRK-MOEDA                PIC  X(002)         VALUE SPACES.
           05 WRK-PEND                 PIC S9(005) COMP-3  VALUE ZEROS. 
           05 WRK-CODIGO               PIC S9(003) COMP-3  VALUE ZEROS. 
           05 WRK-VRPRINC              PIC S9(015) COMP-3  VALUE ZEROS. 
           05 WRK-VRENC                PIC S9(015) COMP-3  VALUE ZEROS. 
           05 WRK-VRENCVI              PIC S9(015) COMP-3  VALUE ZEROS. 
           05 WRK-NOME                 PIC  X(040)         VALUE SPACES.
           05 WRK-NOMEAVA1             PIC  X(040)         VALUE SPACES.
           05 WRK-CPFAVA1              PIC S9(009) COMP-3  VALUE ZEROS. 
CPMCAC     05 WRK-CPFAVA1-R            REDEFINES           WRK-CPFAVA1  
CPMCAC                                 PIC  X(005).                     
           05 WRK-CPFFIL1              PIC S9(005) COMP-3  VALUE ZEROS. 
CPMCAC     05 WRK-CPFFIL1-R            REDEFINES           WRK-CPFFIL1  
CPMCAC                                 PIC  X(003).                     
           05 WRK-CPFCTR1              PIC S9(003) COMP-3  VALUE ZEROS. 
CPMCAC     05 WRK-CPFCTR1-R            REDEFINES           WRK-CPFCTR1  
CPMCAC                                 PIC  X(002).                     
           05 WRK-NOMEAVA2             PIC  X(040)         VALUE SPACES.
           05 WRK-CPFAVA2              PIC S9(009) COMP-3  VALUE ZEROS. 
CPMCAC     05 WRK-CPFAVA2-R            REDEFINES           WRK-CPFAVA2  
CPMCAC                                 PIC  X(005).                     
           05 WRK-CPFFIL2              PIC S9(005) COMP-3  VALUE ZEROS. 
CPMCAC     05 WRK-CPFFIL2-R            REDEFINES           WRK-CPFFIL2  
CPMCAC                                 PIC  X(003).                     
           05 WRK-CPFCTR2              PIC S9(003) COMP-3  VALUE ZEROS. 
CPMCAC     05 WRK-CPFCTR2-R            REDEFINES           WRK-CPFCTR2  
CPMCAC                                 PIC  X(002).                     
           05 WRK-CODEMPR              PIC  X(002)         VALUE SPACES.
                                                                        
      *----------------------------------------------------------------*
                                                                        
       01  WRK-XAVE37                  PIC  X(010)         VALUE SPACES.
                                                                        
      *----------------------------------------------------------------*
                                                                        
       01  WRK-WK37.                                                    
           05 WRK-CPF37.                                                
              10 WRK-CPF37NUM          PIC S9(009) COMP-3  VALUE ZEROS. 
              10 WRK-CPF37FIL          PIC S9(005) COMP-3  VALUE ZEROS. 
              10 WRK-CPF37CTR          PIC S9(003) COMP-3  VALUE ZEROS. 
           05 WRK-CREST-BLOQ-OPER      PIC S9(001) COMP-3  VALUE ZEROS. 
           05 WRK-CJUNC-DEPDC-BDSCO    PIC S9(005) COMP-3  VALUE ZEROS. 
           05 WRK-CCTA-CORR            PIC S9(007) COMP-3  VALUE ZEROS. 
           05 WRK-CTPO-NATUZ-OPER      PIC S9(003) COMP-3  VALUE ZEROS. 
           05 WRK-CCART                PIC  X(003)         VALUE SPACES.
           05 WRK-CSGL-UF              PIC  X(002)         VALUE SPACES.
           05 WRK-CINDCD-AVISO-COBR    PIC  X(001)         VALUE SPACES.
           05 WRK-CINDCD-IMPED-REST    PIC  X(001)         VALUE SPACES.
           05 WRK-CINDCD-SERASA-SPC    PIC  X(001)         VALUE SPACES.
           05 WRK-DULT-ATULZ           PIC  X(010)         VALUE SPACES.
           05 WRK-CFUNC-BDSCO          PIC S9(009) COMP-3  VALUE ZEROS. 
                                                                        
      *----------------------------------------------------------------*
                                                                        
       01  WRK-DATAHORA.                                                
           05 WRK-JULIANA              PIC S9(005) COMP-3  VALUE ZEROS. 
           05 WRK-AAMMDD2              PIC S9(007) COMP-3  VALUE ZEROS. 
           05 WRK-AAAAMMDD             PIC S9(009) COMP-3  VALUE ZEROS. 
           05 WRK-HHMMSS               PIC S9(007) COMP-3  VALUE ZEROS. 
           05 WRK-HHMMSSMM             PIC S9(013) COMP-3  VALUE ZEROS. 
           05 WRK-TIMESTA              PIC  X(020)         VALUE SPACES.
                                                                        
       01  WRK-CPF-AUX                 PIC +9(09) VALUE ZEROS.          
       01  FILLER    REDEFINES WRK-CPF-AUX.                             
           05 FILLER                   PIC  X(001).                     
           05 WRK-CPF-AUX-RR           PIC  9(009).                     
                                                                        
       01  WRK-FIL-AUX                 PIC +9(05) VALUE ZEROS.          
       01  FILLER    REDEFINES WRK-FIL-AUX.                             
           05 FILLER                   PIC  X(001).                     
           05 WRK-FIL-AUX-RR           PIC  9(005).                     
                                                                        
       01  WRK-CTR-AUX                 PIC  X(02) VALUE SPACES.         
       01  FILLER    REDEFINES WRK-CTR-AUX.                             
           05 WRK-CTR-AUX-RR           PIC  9(02).                      
                                                                        
      *----------------------------------------------------------------*
                                                                        
       01  WRK-DATAZ.                                                   
           05 WRK-ANO                  PIC  X(004)         VALUE SPACES.
           05 WRK-MES                  PIC  X(002)         VALUE SPACES.
           05 WRK-DIA                  PIC  X(002)         VALUE SPACES.
                                                                        
      *----------------------------------------------------------------*
                                                                        
       01  WRK-CAB0.                                                    
           05 FILLER                   PIC  X(001)         VALUE X'89'. 
           05 FILLER                   PIC  X(132)         VALUE SPACES.
                                                                        
      *----------------------------------------------------------------*
                                                                        
       01  WRK-CAB1.                                                    
           05 FILLER                   PIC  X(001)         VALUE X'11'. 
           05 FILLER                   PIC  X(015)         VALUE        
           '*CLLP7103*'.                                                
           05 FILLER                   PIC  X(042)         VALUE        
           'CONFRONTO ARQDB22 COM TABELA LPCLB037'.                     
           05 FILLER                   PIC  X(003)         VALUE 'EM'.  
           05 WRK-LDATA.                                                
              10 WRK-LDIA              PIC  X(002)         VALUE SPACES.
              10 FILLER                PIC  X(001)         VALUE '/'.   
              10 WRK-LMES              PIC  X(002)         VALUE SPACES.
              10 FILLER                PIC  X(001)         VALUE '/'.   
              10 WRK-LANO              PIC  X(004)         VALUE SPACES.
           05 FILLER                   PIC  X(004)         VALUE '  AS'.
           05 WRK-HORAS.                                                
CPMCAC        10 FILLER                PIC  X(002)         VALUE SPACES.
CPMCAC        10 WRK-HORAS-HH          PIC  Z9             VALUE ZEROS. 
CPMCAC        10 FILLER                PIC  X(001)         VALUE ':'.   
CPMCAC        10 WRK-HORAS-MM          PIC  9(002)         VALUE ZEROS. 
CPMCAC        10 FILLER                PIC  X(001)         VALUE ':'.   
CPMCAC        10 WRK-HORAS-SS          PIC  9(002)         VALUE ZEROS. 
           05 FILLER                   PIC  X(005)         VALUE ' HS'. 
CPMCAC     05 FILLER                   PIC  X(043)         VALUE SPACES.
                                                                        
      *----------------------------------------------------------------*
                                                                        
       01  WRK-CAB2.                                                    
           05 FILLER                   PIC  X(001)         VALUE X'11'. 
           05 FILLER                   PIC  X(015)         VALUE SPACES.
           05 FILLER                   PIC  X(012)         VALUE        
           'EXCLUSAO DE '.                                              
           05 WRK-PARM                 PIC  X(009)         VALUE SPACES.
CPMCAC     05 FILLER                   PIC  X(096)         VALUE SPACES.
                                                                        
      *----------------------------------------------------------------*
                                                                        
       01  WRK-CAB3.                                                    
           05 FILLER                   PIC  X(001)         VALUE        
           X'11'.                                                       
           05 FILLER                   PIC  X(017)         VALUE        
           'CPF/CNPJ DEVEDOR'.                                          
           05 FILLER                   PIC  X(003)         VALUE SPACES.
           05 FILLER                   PIC  X(017)         VALUE        
           'CPF/CNPJ AVAL 1'.                                           
           05 FILLER                   PIC  X(003)         VALUE SPACES.
           05 FILLER                   PIC  X(017)         VALUE        
           'CPF/CNPJ AVAL 2'.                                           
           05 FILLER                   PIC  X(003)         VALUE SPACES.
           05 FILLER                   PIC  X(004)         VALUE        
           'AGEN'.                                                      
           05 FILLER                   PIC  X(010)         VALUE        
           '       C/C'.                                                
           05 FILLER                   PIC  X(003)         VALUE SPACES.
           05 FILLER                   PIC  X(004)         VALUE        
           'CART'.                                                      
           05 FILLER                   PIC  X(010)         VALUE        
           '  CONTRATO'.                                                
           05 FILLER                   PIC  X(003)         VALUE SPACES.
           05 FILLER                   PIC  X(008)         VALUE        
           ' VENCTO'.                                                   
           05 FILLER                   PIC  X(004)         VALUE SPACES.
           05 FILLER                   PIC  X(002)         VALUE        
           'ID'.                                                        
           05 FILLER                   PIC  X(003)         VALUE SPACES.
           05 FILLER                   PIC  X(008)         VALUE        
           'NATUREZA'.                                                  
CPMCAC     05 FILLER                   PIC  X(013)         VALUE SPACES.
                                                                        
      *----------------------------------------------------------------*
                                                                        
       01  WRK-LDET.                                                    
           05 FILLER                   PIC  X(001)         VALUE X'09'. 
           05 WRK-LCPFNUM              PIC  X(009)         VALUE SPACES.
CPMCAC     05 WRK-LCPFNUM-R            REDEFINES           WRK-LCPFNUM  
CPMCAC                                 PIC  9(009).                     
           05 FILLER                   PIC  X(001)         VALUE '/'.   
           05 WRK-LCPFFIL              PIC  X(004)         VALUE SPACES.
CPMCAC     05 WRK-LCPFFIL-R            REDEFINES           WRK-LCPFFIL  
CPMCAC                                 PIC  9(004).                     
           05 FILLER                   PIC  X(001)         VALUE '-'.   
           05 WRK-LCPFCTR              PIC  X(002)         VALUE SPACES.
           05 FILLER                   PIC  X(003)         VALUE SPACES.
           05 WRK-LCPFAVA1             PIC  X(009)         VALUE SPACES.
CPMCAC     05 WRK-LCPFAVA1-R           REDEFINES           WRK-LCPFAVA1 
CPMCAC                                 PIC  9(009).                     
           05 FILLER                   PIC  X(001)         VALUE '/'.   
           05 WRK-LCPFFIL1             PIC  X(004)         VALUE SPACES.
CPMCAC     05 WRK-LCPFFIL1-R           REDEFINES           WRK-LCPFFIL1 
CPMCAC                                 PIC  9(004).                     
           05 FILLER                   PIC  X(001)         VALUE '-'.   
           05 WRK-LCPFCTR1             PIC  X(002)         VALUE SPACES.
           05 FILLER                   PIC  X(003)         VALUE SPACES.
           05 WRK-LCPFAVA2             PIC  X(009)         VALUE SPACES.
CPMCAC     05 WRK-LCPFAVA2-R           REDEFINES           WRK-LCPFAVA2 
CPMCAC                                 PIC  9(009).                     
           05 FILLER                   PIC  X(001)         VALUE '/'.   
           05 WRK-LCPFFIL2             PIC  X(004)         VALUE SPACES.
CPMCAC     05 WRK-LCPFFIL2-R           REDEFINES           WRK-LCPFFIL2 
CPMCAC                                 PIC  9(004).                     
           05 FILLER                   PIC  X(001)         VALUE '-'.   
           05 WRK-LCPFCTR2             PIC  X(002)         VALUE SPACES.
           05 FILLER                   PIC  X(003)         VALUE SPACES.
           05 WRK-LAG                  PIC  X(004)         VALUE SPACES.
CPMCAC     05 WRK-LAG-R                REDEFINES           WRK-LAG      
CPMCAC                                 PIC  9(004).                     
           05 WRK-LCONTA               PIC  X(010)         VALUE SPACES.
           05 FILLER                   PIC  X(004)         VALUE SPACES.
           05 WRK-LCART                PIC  X(003)         VALUE SPACES.
           05 WRK-LCONTR               PIC  X(010)         VALUE SPACES.
           05 FILLER                   PIC  X(003)         VALUE SPACES.
           05 WRK-LVCTO                PIC  X(008)         VALUE SPACES.
           05 FILLER                   PIC  X(004)         VALUE SPACES.
           05 WRK-LID                  PIC  X(002)         VALUE SPACES.
           05 FILLER                   PIC  X(006)         VALUE SPACES.
           05 WRK-LNATU                PIC  X(003)         VALUE SPACES.
CPMCAC     05 FILLER                   PIC  X(015)         VALUE SPACES.
                                                                        
      *----------------------------------------------------------------*
                                                                        
       01  WRK-FOL                     PIC S9(003) COMP-3  VALUE 55.    
       01  WRK-FOL2                    PIC S9(003) COMP-3  VALUE 55.    
                                                                        
       01  WRK-PAD10                   PIC  BZ.ZZZ.ZZ9     VALUE SPACES.
CPMCAC 01  WRK-PAD10-R                 REDEFINES           WRK-PAD10    
CPMCAC                                 PIC  X(010).                     
                                                                        
       01  WRK-PAD12                   PIC  BZZZ.ZZZ.ZZ9   VALUE SPACES.
CPMCAC 01  WRK-PAD12-R                 REDEFINES           WRK-PAD12    
CPMCAC                                 PIC  X(012).                     
                                                                        
       01  WRK-QTDE                    PIC S9(009) COMP-3  VALUE ZEROS. 
       01  WRK-QTDELE                  PIC  9(009) COMP-3  VALUE ZEROS. 
       01  WRK-QTDEGR                  PIC  9(009) COMP-3  VALUE ZEROS. 
       01  WRK-QTDEGR-NOVO             PIC  9(009) COMP-3  VALUE ZEROS. 
       01  WRK-QTDEDEV                 PIC  9(009) COMP-3  VALUE ZEROS. 
       01  WRK-QTDEAVA1                PIC  9(009) COMP-3  VALUE ZEROS. 
       01  WRK-QTDEAVA2                PIC  9(009) COMP-3  VALUE ZEROS. 
                                                                        
      *----------------------------------------------------------------*
                                                                        
       01  WRK-CPFAUX.                                                  
           05 WRK-CPFAUXN              PIC S9(009) COMP-3  VALUE ZEROS. 
           05 WRK-CPFAUXF              PIC S9(005) COMP-3  VALUE ZEROS. 
           05 WRK-CPFAUXC              PIC S9(003) COMP-3  VALUE ZEROS. 
                                                                        
CPMCAC*----------------------------------------------------------------*
CPMCAC 01  FILLER                      PIC  X(050)         VALUE        
CPMCAC     'AREA PARA O CLLP8400'.                                      
CPMCAC*----------------------------------------------------------------*
                                                                        
CPMCAC 01  WRK-CLLP8400                PIC  X(008)         VALUE        
CPMCAC     'CLLP8400'.                                                  
                                                                        
CPMCAC 01  WRK-CLLP8400-TAMANHO        PIC S9(004) COMP    VALUE 1.     
CPMCAC 01  WRK-CLLP8400-OPERACAO-OI    PIC  X(002)         VALUE 'OI'.  
CPMCAC 01  WRK-CLLP8400-OPERANDO       PIC  X(001)         VALUE SPACES.
CPMCAC 01  WRK-CLLP8400-MASCARA1       PIC  X(001)         VALUE X'0F'. 
                                                                        
CPMCAC*----------------------------------------------------------------*
CPMCAC 01  FILLER                      PIC  X(050)         VALUE        
CPMCAC     'AREA PARA A BRAD0450'.                                      
CPMCAC*----------------------------------------------------------------*
                                                                        
CPMCAC 01  WRK-ABEND                   PIC S9(004) COMP    VALUE ZEROS. 
CPMCAC 01  WRK-DUMP                    PIC  X(001)         VALUE SPACES.
                                                                        
      *----------------------------------------------------------------*
                                                                        
CPMCAC 01  WRK-09N                     PIC +9(009)         VALUE ZEROS. 
CPMCAC 01  FILLER                      REDEFINES           WRK-09N.     
CPMCAC     05 FILLER                   PIC  X(002).                     
CPMCAC     05 WRK-09N-X                PIC  X(008).                     
CPMCAC 01  FILLER                      REDEFINES           WRK-09N.     
CPMCAC     05 FILLER                   PIC  X(001).                     
CPMCAC     05 WRK-09N-R                PIC  9(009).                     
                                                                        
CPMCAC 01  WRK-07N-SS                  PIC +9(007)         VALUE ZEROS. 
CPMCAC 01  FILLER                      REDEFINES           WRK-07N-SS.  
CPMCAC     05 FILLER                   PIC  X(001).                     
CPMCAC     05 WRK-07N-9                PIC  9(007).                     
                                                                        
CPMCAC 01  WRK-05N-SS                  PIC +9(005)         VALUE ZEROS. 
CPMCAC 01  FILLER                      REDEFINES           WRK-05N-SS.  
CPMCAC     05 FILLER                   PIC  X(002).                     
CPMCAC     05 WRK-05N                  PIC  9(004).                     
                                                                        
CPMCAC 01  WRK-03N-SS                  PIC +9(003)         VALUE ZEROS. 
CPMCAC 01  FILLER                      REDEFINES           WRK-03N-SS.  
CPMCAC     05 FILLER                   PIC  X(001).                     
CPMCAC     05 WRK-03N                  PIC  X(003).                     
                                                                        
CPMCAC 01  WRK-FLAG                    PIC  X(001)         VALUE 'N'.   
                                                                        
EGMF   01  WRK-AGENCIA-1                 PIC +9(05) VALUE ZEROS.        
EGMF   01  WRK-AGENCIA-1-R         REDEFINES  WRK-AGENCIA-1.            
EGMF       03  FILLER                    PIC  X(01).                    
EGMF       03  WRK-AGENCIA-1-RR          PIC  9(05).                    
EGMF                                                                    
EGMF   01  WRK-CONTA-1                 PIC +9(07) VALUE ZEROS.          
EGMF   01  WRK-CONTA-1-R         REDEFINES  WRK-CONTA-1.                
EGMF       03  FILLER                  PIC  X(01).                      
EGMF       03  WRK-CONTA-1-RR          PIC  9(07).                      
EGMF                                                                    
EGMF   01  WRK-CONTRATO-1              PIC +9(07) VALUE ZEROS.          
EGMF   01  WRK-CONTRATO-1-R      REDEFINES  WRK-CONTRATO-1.             
EGMF       03  FILLER                  PIC  X(01).                      
EGMF       03  WRK-CONTRATO-1-RR       PIC  9(07).                      
EGMF                                                                    
EGMF   01  WRK-EMPRESA-1               PIC +9(05) VALUE ZEROS.          
EGMF   01  WRK-EMPRESA-1-R      REDEFINES  WRK-EMPRESA-1.               
EGMF       03  FILLER                  PIC  X(01).                      
EGMF       03  WRK-EMPRESA-1-RR        PIC  9(05).                      
                                                                        
      *----------------------------------------------------------------*
                                                                        
           COPY    I#BRAD7C.                                            
                                                                        
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE        
           'FIM DA WORKING STORAGE SECTION '.                           
      *----------------------------------------------------------------*
                                                                        
CPMCAC*----------------------------------------------------------------*
CPMCAC LINKAGE                         SECTION.                         
CPMCAC*----------------------------------------------------------------*
                                                                        
CPMCAC 01  LNK-PARM-AREA.                                               
CPMCAC     05 LNK-TAM-PARM             PIC S9(004) COMP    VALUE ZEROS. 
CPMCAC     05 LNK-PARM-PARM            PIC  X(009)         VALUE SPACES.
                                                                        
      *================================================================*
       PROCEDURE DIVISION              USING LNK-PARM-AREA.             
      *================================================================*
                                                                        
           MOVE    LNK-PARM-PARM       TO  WRK-PARM                     
                                                                        
           CALL    'BRAD7600'          USING WRK-DATAHORA               
                                                                        
           MOVE    WRK-AAAAMMDD        TO  WRK-09N                      
           MOVE    WRK-09N-X           TO  WRK-DATAZ                    
           MOVE    WRK-ANO             TO  WRK-LANO                     
           MOVE    WRK-MES             TO  WRK-LMES                     
           MOVE    WRK-DIA             TO  WRK-LDIA                     
                                                                        
           MOVE    WRK-HHMMSS          TO  WRK-07N-SS                   
           MOVE    WRK-07N-9(2:2)      TO  WRK-HORAS-HH                 
           MOVE    WRK-07N-9(4:2)      TO  WRK-HORAS-MM                 
           MOVE    WRK-07N-9(6:2)      TO  WRK-HORAS-SS                 
                                                                        
           OPEN    INPUT  ARQDB22                                       
                          LPCLB037                                      
                                                                        
                   OUTPUT ARQATU                                        
                          BLQREST                                       
                          RELATO                                        
                          RELATO2                                       
                                                                        
           MOVE    WRK-ABERTURA        TO  WRK-OPERACAO                 
                                                                        
           PERFORM 9000-TESTAR-FILE-STATUS THRU 9000-99-FIM             
                                                                        
           PERFORM 1400-LETAB37            THRU 1500-99-FIM.            
                                                                        
      *----------------------------------------------------------------*
       0100-LER.                                                        
      *----------------------------------------------------------------*
                                                                        
           READ    ARQDB22             INTO WRK-WK END-READ             
                                                                        
           MOVE    WRK-LEITURA         TO  WRK-OPERACAO                 
                                                                        
           IF      WRK-FS-ARQDB22      EQUAL '10'                       
                   GO TO 1600-FIM                                       
           ELSE                                                         
                   PERFORM 9010-TESTAR-FS-ARQDB22 THRU 9010-99-FIM      
           END-IF                                                       
                                                                        
           ADD     1                   TO  WRK-QTDELE                   
                                                                        
      *    CPF DEVEDOR                                                  
                                                                        
           MOVE    WRK-CPFNUM-R        TO  WRK-CPFAUX(1:5)              
           MOVE    WRK-CPFFIL-R        TO  WRK-CPFAUX(6:3)              
           MOVE    WRK-CPFCTR-R        TO  WRK-CPFAUX(9:2)              
                                                                        
           IF      WRK-PARM            EQUAL 'DEVEDOR  '                
                   GO TO 0200-VSEZERO                                   
           END-IF                                                       
                                                                        
      *    CPF DO PRIMEIRO AVALISTA                                     
                                                                        
           MOVE    WRK-CPFAVA1-R       TO  WRK-CPFAUX(1:5)              
           MOVE    WRK-CPFFIL1-R       TO  WRK-CPFAUX(6:3)              
           MOVE    WRK-CPFCTR1-R       TO  WRK-CPFAUX(9:2)              
                                                                        
           IF      WRK-PARM            EQUAL 'AVALISTA1'                
                   GO TO 0200-VSEZERO                                   
           END-IF                                                       
                                                                        
      *    CPF DO SEGUNDO AVALISTA                                      
                                                                        
           MOVE    WRK-CPFAVA2-R       TO  WRK-CPFAUX(1:5)              
           MOVE    WRK-CPFFIL2-R       TO  WRK-CPFAUX(6:3)              
           MOVE    WRK-CPFCTR2-R       TO  WRK-CPFAUX(9:2).             
                                                                        
      *----------------------------------------------------------------*
       0200-VSEZERO.                                                    
      *----------------------------------------------------------------*
                                                                        
           MOVE    WRK-CPFAUX(5:1)     TO  WRK-CLLP8400-OPERANDO        
                                                                        
           CALL    WRK-CLLP8400        USING  WRK-CLLP8400-TAMANHO      
                                              WRK-CLLP8400-OPERACAO-OI  
                                              WRK-CLLP8400-OPERANDO     
                                              WRK-CLLP8400-MASCARA1     
                                                                        
           MOVE    WRK-CLLP8400-OPERANDO                                
                                       TO  WRK-CPFAUX(5:1)              
                                                                        
           MOVE    WRK-CPFAUX(8:1)     TO  WRK-CLLP8400-OPERANDO        
                                                                        
           CALL    WRK-CLLP8400        USING  WRK-CLLP8400-TAMANHO      
                                              WRK-CLLP8400-OPERACAO-OI  
                                              WRK-CLLP8400-OPERANDO     
                                              WRK-CLLP8400-MASCARA1     
                                                                        
           MOVE    WRK-CLLP8400-OPERANDO                                
                                       TO  WRK-CPFAUX(8:1)              
                                                                        
           IF      WRK-CPFAUXN         EQUAL   ZEROS                    
                   GO TO 1300-SOGRAVA                                   
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       0300-COMPARA.                                                    
      *----------------------------------------------------------------*
                                                                        
           IF      WRK-CPFAUX          EQUAL   WRK-XAVE37               
                   GO TO 1301-SOGRAVA-BLQREST                           
           END-IF                                                       
                                                                        
           IF      WRK-CPFAUX          LESS    WRK-XAVE37               
                   GO TO 1300-SOGRAVA                                   
           END-IF                                                       
                                                                        
           PERFORM 1400-LETAB37        THRU    1500-99-FIM              
                                                                        
           GO TO 0300-COMPARA.                                          
                                                                        
      *----------------------------------------------------------------*
       0400-ACHOU.                                                      
      *----------------------------------------------------------------*
                                                                        
           MOVE    WRK-CPFNUM          TO  WRK-09N                      
           MOVE    WRK-09N-R           TO  WRK-LCPFNUM-R                
                                                                        
           MOVE    WRK-CPFFIL          TO  WRK-05N-SS                   
           MOVE    WRK-05N             TO  WRK-LCPFFIL-R                
                                                                        
           MOVE    WRK-CPFCTR-R        TO  WRK-LCPFCTR                  
                                                                        
           MOVE    WRK-CPFAVA1         TO  WRK-09N                      
           MOVE    WRK-09N-R           TO  WRK-LCPFAVA1-R               
                                                                        
           MOVE    WRK-CPFFIL1         TO  WRK-05N-SS                   
           MOVE    WRK-05N             TO  WRK-LCPFFIL1-R               
                                                                        
           MOVE    WRK-CPFCTR1-R       TO  WRK-LCPFCTR1                 
                                                                        
           MOVE    WRK-CPFAVA2         TO  WRK-09N                      
           MOVE    WRK-09N-R           TO  WRK-LCPFAVA2-R               
                                                                        
           MOVE    WRK-CPFFIL2         TO  WRK-05N-SS                   
           MOVE    WRK-05N             TO  WRK-LCPFFIL2-R               
                                                                        
           MOVE    WRK-CPFCTR2-R       TO  WRK-LCPFCTR2                 
                                                                        
           MOVE    WRK-AGENCIA         TO  WRK-05N-SS                   
           MOVE    WRK-05N             TO  WRK-LAG-R                    
                                                                        
           MOVE    WRK-CONTA           TO  WRK-07N-SS                   
           MOVE    WRK-07N-9           TO  WRK-PAD10                    
           MOVE    WRK-PAD10-R         TO  WRK-LCONTA                   
                                                                        
           MOVE    WRK-CART            TO  WRK-LCART                    
                                                                        
           MOVE    WRK-CONTR           TO  WRK-07N-SS                   
           MOVE    WRK-07N-9           TO  WRK-PAD10                    
           MOVE    WRK-PAD10-R         TO  WRK-LCONTR                   
                                                                        
           MOVE    WRK-VCTO            TO  WRK-LVCTO                    
           MOVE    WRK-ID              TO  WRK-LID                      
           MOVE    WRK-NATU            TO  WRK-LNATU                    
                                                                        
           ADD     1                   TO  WRK-QTDE                     
                                                                        
           IF      WRK-QTDE            GREATER 270000                   
                   GO TO 0700-LISTA2                                    
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       0500-LISTA1.                                                     
      *----------------------------------------------------------------*
                                                                        
           IF      WRK-FOL             LESS 55                          
                   GO TO 0600-NQB                                       
           END-IF                                                       
                                                                        
           MOVE    ZEROS               TO  WRK-FOL                      
                                                                        
           WRITE   FD-REG-RELATO       FROM WRK-CAB0                    
                                                                        
           MOVE    WRK-GRAVACAO        TO  WRK-OPERACAO                 
                                                                        
           PERFORM 9040-TESTAR-FS-RELATO THRU 9040-99-FIM               
                                                                        
           WRITE   FD-REG-RELATO       FROM WRK-CAB1                    
                                                                        
           PERFORM 9040-TESTAR-FS-RELATO THRU 9040-99-FIM               
                                                                        
           WRITE   FD-REG-RELATO       FROM WRK-CAB2                    
                                                                        
           PERFORM 9040-TESTAR-FS-RELATO THRU 9040-99-FIM               
                                                                        
           WRITE   FD-REG-RELATO       FROM WRK-CAB3                    
                                                                        
           PERFORM 9040-TESTAR-FS-RELATO THRU 9040-99-FIM.              
                                                                        
      *----------------------------------------------------------------*
       0600-NQB.                                                        
      *----------------------------------------------------------------*
                                                                        
           ADD 1                       TO  WRK-FOL                      
                                                                        
           WRITE   FD-REG-RELATO       FROM WRK-LDET                    
                                                                        
           MOVE    WRK-GRAVACAO        TO  WRK-OPERACAO                 
                                                                        
           PERFORM 9040-TESTAR-FS-RELATO THRU 9040-99-FIM               
                                                                        
           GO TO 0900-JALISTOU.                                         
                                                                        
      *----------------------------------------------------------------*
       0700-LISTA2.                                                     
      *----------------------------------------------------------------*
                                                                        
           IF      WRK-FOL2            LESS 55                          
                   GO TO 0800-NQB2                                      
           END-IF                                                       
                                                                        
           MOVE    ZEROS               TO  WRK-FOL2                     
                                                                        
           WRITE   FD-REG-RELATO2      FROM WRK-CAB0                    
                                                                        
           MOVE    WRK-GRAVACAO        TO  WRK-OPERACAO                 
                                                                        
           PERFORM 9050-TESTAR-FS-RELATO2 THRU 9050-99-FIM              
                                                                        
           WRITE   FD-REG-RELATO2      FROM WRK-CAB1                    
                                                                        
           PERFORM 9050-TESTAR-FS-RELATO2 THRU 9050-99-FIM              
                                                                        
           WRITE   FD-REG-RELATO2      FROM WRK-CAB2                    
                                                                        
           PERFORM 9050-TESTAR-FS-RELATO2 THRU 9050-99-FIM              
                                                                        
           WRITE   FD-REG-RELATO2      FROM WRK-CAB3                    
                                                                        
           PERFORM 9050-TESTAR-FS-RELATO2 THRU 9050-99-FIM.             
                                                                        
      *----------------------------------------------------------------*
       0800-NQB2.                                                       
      *----------------------------------------------------------------*
                                                                        
           ADD     1                   TO    WRK-FOL2                   
                                                                        
           WRITE   FD-REG-RELATO2      FROM WRK-LDET                    
                                                                        
           MOVE    WRK-GRAVACAO        TO  WRK-OPERACAO                 
                                                                        
           PERFORM 9050-TESTAR-FS-RELATO2 THRU 9050-99-FIM.             
                                                                        
      *----------------------------------------------------------------*
       0900-JALISTOU.                                                   
      *----------------------------------------------------------------*
                                                                        
           IF      WRK-PARM            EQUAL 'AVALISTA1'                
                   GO TO 1100-ZERAR1                                    
           END-IF                                                       
                                                                        
           IF      WRK-PARM            EQUAL 'AVALISTA2'                
                   GO TO 1200-ZERAR2                                    
           END-IF                                                       
                                                                        
           ADD     1                   TO  WRK-QTDEDEV                  
                                                                        
           IF      WRK-PARM            EQUAL 'DEVEDOR  '                
                   GO TO 0100-LER                                       
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       1000-ABENDAR.                                                    
      *----------------------------------------------------------------*
                                                                        
           MOVE    2003                TO  WRK-ABEND                    
           MOVE    'S'                 TO  WRK-DUMP                     
                                                                        
           CALL    'BRAD0450'          USING WRK-ABEND                  
                                             WRK-DUMP.                  
                                                                        
      *----------------------------------------------------------------*
       1100-ZERAR1.                                                     
      *----------------------------------------------------------------*
                                                                        
           MOVE    X'000000000F'       TO  WRK-CPFAVA1-R                
           MOVE    X'00000F'           TO  WRK-CPFFIL1-R                
           MOVE    '00'                TO  WRK-CPFCTR1-R                
                                                                        
           ADD     1                   TO  WRK-QTDEAVA1                 
                                                                        
           GO TO 1300-SOGRAVA.                                          
                                                                        
      *----------------------------------------------------------------*
       1200-ZERAR2.                                                     
      *----------------------------------------------------------------*
                                                                        
           MOVE    X'000000000F'       TO  WRK-CPFAVA2-R                
           MOVE    X'00000F'           TO  WRK-CPFFIL2-R                
           MOVE    '00'                TO  WRK-CPFCTR2-R                
                                                                        
           ADD     1                   TO  WRK-QTDEAVA2                 
                                                                        
           GO TO 1300-SOGRAVA.                                          
                                                                        
      *----------------------------------------------------------------*
       1300-SOGRAVA.                                                    
      *----------------------------------------------------------------*
                                                                        
           WRITE   FD-REG-ARQATU       FROM WRK-WK                      
                                                                        
           MOVE    WRK-GRAVACAO        TO  WRK-OPERACAO                 
                                                                        
           PERFORM 9030-TESTAR-FS-ARQATU THRU 9030-99-FIM               
                                                                        
           ADD     1                   TO  WRK-QTDEGR                   
                                                                        
           GO TO 0100-LER.                                              
                                                                        
      *----------------------------------------------------------------*
       1301-SOGRAVA-BLQREST.                                            
      *----------------------------------------------------------------*
                                                                        
           IF WRK-CINDCD-SERASA-SPC EQUAL 'X'                           
EGMF          MOVE WRK-AGENCIA       TO WRK-AGENCIA-1                   
EGMF          MOVE WRK-AGENCIA-1-RR  TO FD-AGENCIA                      
EGMF          MOVE WRK-CONTA         TO WRK-CONTA-1                     
EGMF          MOVE WRK-CONTA-1-RR    TO FD-CONTA                        
EGMF          MOVE WRK-CONTR         TO WRK-CONTRATO-1                  
EGMF          MOVE WRK-CONTRATO-1-RR TO FD-CONTRATO                     
EGMF          MOVE WRK-EMPRESA       TO WRK-EMPRESA-1                   
EGMF          MOVE WRK-EMPRESA-1-RR  TO FD-EMPRESA                      
              MOVE WRK-CART          TO FD-CARTEIRA                     
              MOVE 00                TO FD-IND-BLOQ                     
              IF WRK-PARM            EQUAL 'DEVEDOR  '                  
                 MOVE 'D'            TO FD-INDICADOR                    
              ELSE                                                      
                 IF WRK-PARM         EQUAL 'AVALISTA1'                  
                    MOVE 'A'         TO FD-INDICADOR                    
                 ELSE                                                   
                    IF WRK-PARM      EQUAL 'AVALISTA2'                  
                       MOVE 'A'      TO FD-INDICADOR                    
                    END-IF                                              
                 END-IF                                                 
              END-IF                                                    
              MOVE  WRK-CPFNUM      TO WRK-CPF-AUX                      
              MOVE  WRK-CPF-AUX-RR  TO FD-CPF                           
              MOVE  WRK-CPFFIL      TO WRK-FIL-AUX                      
              MOVE  WRK-FIL-AUX-RR  TO FD-FILIAL                        
              MOVE  WRK-CPFCTR-R    TO WRK-CTR-AUX                      
              MOVE  WRK-CTR-AUX-RR  TO FD-CTRL                          
              WRITE FD-BLQRESTR                                         
           END-IF.                                                      
                                                                        
           IF WRK-CINDCD-IMPED-REST EQUAL 'X'                           
EGMF          MOVE WRK-AGENCIA       TO WRK-AGENCIA-1                   
EGMF          MOVE WRK-AGENCIA-1-RR  TO FD-AGENCIA                      
EGMF          MOVE WRK-CONTA         TO WRK-CONTA-1                     
EGMF          MOVE WRK-CONTA-1-RR    TO FD-CONTA                        
EGMF          MOVE WRK-CONTR         TO WRK-CONTRATO-1                  
EGMF          MOVE WRK-CONTRATO-1-RR TO FD-CONTRATO                     
EGMF          MOVE WRK-EMPRESA       TO WRK-EMPRESA-1                   
EGMF          MOVE WRK-EMPRESA-1-RR  TO FD-EMPRESA                      
              MOVE WRK-CART          TO FD-CARTEIRA                     
              MOVE 01                TO FD-IND-BLOQ                     
              IF WRK-PARM            EQUAL 'DEVEDOR  '                  
                 MOVE 'D'            TO FD-INDICADOR                    
              ELSE                                                      
                 IF WRK-PARM         EQUAL 'AVALISTA1'                  
                    MOVE 'A'         TO FD-INDICADOR                    
                 ELSE                                                   
                    IF WRK-PARM      EQUAL 'AVALISTA2'                  
                       MOVE 'A'      TO FD-INDICADOR                    
                    END-IF                                              
                 END-IF                                                 
              END-IF                                                    
              MOVE  WRK-CPFNUM      TO WRK-CPF-AUX                      
              MOVE  WRK-CPF-AUX-RR  TO FD-CPF                           
              MOVE  WRK-CPFFIL      TO WRK-FIL-AUX                      
              MOVE  WRK-FIL-AUX-RR  TO FD-FILIAL                        
              MOVE  WRK-CPFCTR-R    TO WRK-CTR-AUX                      
              MOVE  WRK-CTR-AUX-RR  TO FD-CTRL                          
              WRITE FD-BLQRESTR                                         
           END-IF.                                                      
                                                                        
           IF WRK-CINDCD-AVISO-COBR EQUAL 'X'                           
EGMF          MOVE WRK-AGENCIA       TO WRK-AGENCIA-1                   
EGMF          MOVE WRK-AGENCIA-1-RR  TO FD-AGENCIA                      
EGMF          MOVE WRK-CONTA         TO WRK-CONTA-1                     
EGMF          MOVE WRK-CONTA-1-RR    TO FD-CONTA                        
EGMF          MOVE WRK-CONTR         TO WRK-CONTRATO-1                  
EGMF          MOVE WRK-CONTRATO-1-RR TO FD-CONTRATO                     
EGMF          MOVE WRK-EMPRESA       TO WRK-EMPRESA-1                   
EGMF          MOVE WRK-EMPRESA-1-RR  TO FD-EMPRESA                      
              MOVE WRK-CART          TO FD-CARTEIRA                     
              MOVE 02                TO FD-IND-BLOQ                     
              IF WRK-PARM            EQUAL 'DEVEDOR  '                  
                 MOVE 'D'            TO FD-INDICADOR                    
              ELSE                                                      
                 IF WRK-PARM         EQUAL 'AVALISTA1'                  
                    MOVE 'A'         TO FD-INDICADOR                    
                 ELSE                                                   
                    IF WRK-PARM      EQUAL 'AVALISTA2'                  
                       MOVE 'A'      TO FD-INDICADOR                    
                    END-IF                                              
                 END-IF                                                 
              END-IF                                                    
              MOVE  WRK-CPFNUM      TO WRK-CPF-AUX                      
              MOVE  WRK-CPF-AUX-RR  TO FD-CPF                           
              MOVE  WRK-CPFFIL      TO WRK-FIL-AUX                      
              MOVE  WRK-FIL-AUX-RR  TO FD-FILIAL                        
              MOVE  WRK-CPFCTR-R    TO WRK-CTR-AUX                      
              MOVE  WRK-CTR-AUX-RR  TO FD-CTRL                          
              WRITE FD-BLQRESTR                                         
           END-IF.                                                      
                                                                        
           MOVE    WRK-GRAVACAO        TO  WRK-OPERACAO                 
                                                                        
           PERFORM 9031-TESTAR-FS-BLQREST THRU 9031-99-FIM              
                                                                        
           ADD     1                   TO  WRK-QTDEGR-NOVO              
                                                                        
           GO TO 0400-ACHOU.                                            
                                                                        
      *----------------------------------------------------------------*
       1400-LETAB37.                                                    
      *----------------------------------------------------------------*
                                                                        
           READ    LPCLB037            INTO WRK-WK37 END-READ           
                                                                        
           MOVE    WRK-LEITURA         TO  WRK-OPERACAO                 
                                                                        
           IF      WRK-FS-LPCLB037     EQUAL '10'                       
                   MOVE 'S'            TO  WRK-FLAG                     
                   GO TO 1500-FIM37                                     
           ELSE                                                         
                   PERFORM 9020-TESTAR-FS-LPCLB037 THRU 9020-99-FIM     
           END-IF                                                       
                                                                        
           MOVE    WRK-CPF37(5:1)      TO  WRK-CLLP8400-OPERANDO        
                                                                        
           CALL    WRK-CLLP8400        USING  WRK-CLLP8400-TAMANHO      
                                              WRK-CLLP8400-OPERACAO-OI  
                                              WRK-CLLP8400-OPERANDO     
                                              WRK-CLLP8400-MASCARA1     
           MOVE    WRK-CLLP8400-OPERANDO                                
                                       TO  WRK-CPF37(5:1)               
                                                                        
           MOVE    WRK-CPF37(8:1)      TO  WRK-CLLP8400-OPERANDO        
                                                                        
           CALL    WRK-CLLP8400        USING  WRK-CLLP8400-TAMANHO      
                                              WRK-CLLP8400-OPERACAO-OI  
                                              WRK-CLLP8400-OPERANDO     
                                              WRK-CLLP8400-MASCARA1     
           MOVE    WRK-CLLP8400-OPERANDO                                
                                       TO  WRK-CPF37(8:1)               
                                                                        
           MOVE    WRK-CPF37(10:1)     TO  WRK-CLLP8400-OPERANDO        
                                                                        
           CALL    WRK-CLLP8400        USING  WRK-CLLP8400-TAMANHO      
                                              WRK-CLLP8400-OPERACAO-OI  
                                              WRK-CLLP8400-OPERANDO     
                                              WRK-CLLP8400-MASCARA1     
           MOVE    WRK-CLLP8400-OPERANDO                                
                                       TO  WRK-CPF37(10:1)              
                                                                        
           MOVE    WRK-CPF37(1:8)      TO  WRK-XAVE37(1:8)              
           MOVE    WRK-CPF37CTR        TO  WRK-03N-SS                   
           MOVE    WRK-03N(2:2)        TO  WRK-XAVE37(9:2).             
                                                                        
      *----------------------------------------------------------------*
       1500-FIM37.                                                      
      *----------------------------------------------------------------*
                                                                        
           IF      WRK-FLAG            EQUAL 'S'                        
                   MOVE X'999999999F99999F999F'                         
                                       TO  WRK-XAVE37                   
           END-IF.                                                      
                                                                        
       1500-99-FIM.                    EXIT.                            
                                                                        
      *----------------------------------------------------------------*
       1600-FIM.                                                        
      *----------------------------------------------------------------*
                                                                        
           WRITE   FD-REG-RELATO       FROM WRK-CAB0                    
                                                                        
           MOVE    WRK-GRAVACAO        TO  WRK-OPERACAO                 
                                                                        
           PERFORM 9040-TESTAR-FS-RELATO THRU 9040-99-FIM               
                                                                        
           WRITE   FD-REG-RELATO       FROM WRK-CAB1                    
                                                                        
           PERFORM 9040-TESTAR-FS-RELATO THRU 9040-99-FIM               
                                                                        
           WRITE   FD-REG-RELATO       FROM WRK-CAB2                    
                                                                        
           PERFORM 9040-TESTAR-FS-RELATO THRU 9040-99-FIM               
                                                                        
           WRITE   FD-REG-RELATO2      FROM WRK-CAB0                    
                                                                        
           PERFORM 9050-TESTAR-FS-RELATO2 THRU 9050-99-FIM              
                                                                        
           WRITE   FD-REG-RELATO2      FROM WRK-CAB1                    
                                                                        
           PERFORM 9050-TESTAR-FS-RELATO2 THRU 9050-99-FIM              
                                                                        
           WRITE   FD-REG-RELATO2      FROM WRK-CAB2                    
                                                                        
           PERFORM 9050-TESTAR-FS-RELATO2 THRU 9050-99-FIM              
                                                                        
           MOVE    'TOTAIS DE REGISTROS -' TO  WRK-LDET(2:132)          
           MOVE    WRK-QTDELE          TO  WRK-PAD12                    
           MOVE    WRK-PAD12-R         TO  WRK-LDET(24:12)              
           MOVE    '-LIDOS'            TO  WRK-LDET(36:20)              
                                                                        
           WRITE   FD-REG-RELATO       FROM WRK-LDET                    
                                                                        
           PERFORM 9040-TESTAR-FS-RELATO THRU 9040-99-FIM               
                                                                        
           WRITE   FD-REG-RELATO2      FROM WRK-LDET                    
                                                                        
           PERFORM 9050-TESTAR-FS-RELATO2 THRU 9050-99-FIM              
                                                                        
           MOVE    ' '                 TO  WRK-LDET(2:132)              
           MOVE    WRK-QTDEDEV         TO  WRK-PAD12                    
           MOVE    WRK-PAD12-R         TO  WRK-LDET(24:12)              
           MOVE    '-EXCLUIDOS / DEVEDOR' TO  WRK-LDET(36:30)           
                                                                        
           WRITE   FD-REG-RELATO       FROM WRK-LDET                    
                                                                        
           PERFORM 9040-TESTAR-FS-RELATO THRU 9040-99-FIM               
                                                                        
           WRITE   FD-REG-RELATO2      FROM WRK-LDET                    
                                                                        
           PERFORM 9050-TESTAR-FS-RELATO2 THRU 9050-99-FIM              
                                                                        
           MOVE    WRK-QTDEAVA1        TO  WRK-PAD12                    
           MOVE    WRK-PAD12-R         TO  WRK-LDET(24:12)              
           MOVE    '-EXCLUIDOS / AVALISTA 1' TO  WRK-LDET(36:30)        
                                                                        
           WRITE   FD-REG-RELATO       FROM WRK-LDET                    
                                                                        
           MOVE    WRK-GRAVACAO        TO  WRK-OPERACAO                 
                                                                        
           PERFORM 9040-TESTAR-FS-RELATO THRU 9040-99-FIM               
                                                                        
           WRITE   FD-REG-RELATO2      FROM WRK-LDET                    
                                                                        
           PERFORM 9050-TESTAR-FS-RELATO2 THRU 9050-99-FIM              
                                                                        
           MOVE    WRK-QTDEAVA2        TO  WRK-PAD12                    
           MOVE    WRK-PAD12-R         TO  WRK-LDET(24:12)              
           MOVE    '-EXCLUIDOS / AVALISTA 2' TO  WRK-LDET(36:30)        
                                                                        
           WRITE   FD-REG-RELATO       FROM WRK-LDET                    
                                                                        
           PERFORM 9040-TESTAR-FS-RELATO THRU 9040-99-FIM               
                                                                        
           WRITE   FD-REG-RELATO2      FROM WRK-LDET                    
                                                                        
           PERFORM 9050-TESTAR-FS-RELATO2 THRU 9050-99-FIM              
                                                                        
           MOVE    WRK-QTDEGR          TO  WRK-PAD12                    
           MOVE    WRK-PAD12-R         TO  WRK-LDET(24:12)              
           MOVE    '-GRAVADOS'         TO  WRK-LDET(36:30)              
                                                                        
           WRITE   FD-REG-RELATO       FROM WRK-LDET                    
                                                                        
           PERFORM 9040-TESTAR-FS-RELATO THRU 9040-99-FIM               
                                                                        
           WRITE   FD-REG-RELATO2      FROM WRK-LDET                    
                                                                        
           PERFORM 9050-TESTAR-FS-RELATO2 THRU 9050-99-FIM              
                                                                        
           CLOSE   ARQDB22                                              
                   LPCLB037                                             
                   ARQATU                                               
                   BLQREST                                              
                   RELATO                                               
                   RELATO2                                              
                                                                        
           MOVE    WRK-FECHAMENTO      TO  WRK-OPERACAO                 
                                                                        
           PERFORM 9000-TESTAR-FILE-STATUS THRU 9000-99-FIM             
                                                                        
           GOBACK.                                                      
                                                                        
      *----------------------------------------------------------------*
       9000-TESTAR-FILE-STATUS.                                         
      *----------------------------------------------------------------*
                                                                        
           PERFORM 9010-TESTAR-FS-ARQDB22  THRU 9010-99-FIM             
           PERFORM 9020-TESTAR-FS-LPCLB037 THRU 9020-99-FIM             
           PERFORM 9030-TESTAR-FS-ARQATU   THRU 9030-99-FIM             
           PERFORM 9031-TESTAR-FS-BLQREST  THRU 9031-99-FIM             
           PERFORM 9040-TESTAR-FS-RELATO   THRU 9040-99-FIM             
           PERFORM 9050-TESTAR-FS-RELATO2  THRU 9050-99-FIM.            
                                                                        
       9000-99-FIM.                    EXIT.                            
                                                                        
      *----------------------------------------------------------------*
       9010-TESTAR-FS-ARQDB22.                                          
      *----------------------------------------------------------------*
                                                                        
           IF  (WRK-FS-ARQDB22         NOT EQUAL '00')                  
               MOVE 'ARQDB22'          TO WRK-NOME-ARQ                  
               MOVE WRK-FS-ARQDB22     TO WRK-FILE-STATUS               
               MOVE WRK-ERRO-BRAD7100  TO ERR-TEXTO                     
               PERFORM 9999-PROCESSAR-ROTINA-ERRO                       
           END-IF.                                                      
                                                                        
       9010-99-FIM.                    EXIT.                            
                                                                        
      *----------------------------------------------------------------*
       9020-TESTAR-FS-LPCLB037.                                         
      *----------------------------------------------------------------*
                                                                        
           IF  (WRK-FS-LPCLB037        NOT EQUAL '00')                  
               MOVE 'LPCLB037'         TO WRK-NOME-ARQ                  
               MOVE WRK-FS-LPCLB037    TO WRK-FILE-STATUS               
               MOVE WRK-ERRO-BRAD7100  TO ERR-TEXTO                     
               PERFORM 9999-PROCESSAR-ROTINA-ERRO                       
           END-IF.                                                      
                                                                        
       9020-99-FIM.                    EXIT.                            
                                                                        
      *----------------------------------------------------------------*
       9030-TESTAR-FS-ARQATU.                                           
      *----------------------------------------------------------------*
                                                                        
           IF  (WRK-FS-ARQATU          NOT EQUAL '00')                  
               MOVE 'ARQATU'           TO WRK-NOME-ARQ                  
               MOVE WRK-FS-ARQATU      TO WRK-FILE-STATUS               
               MOVE WRK-ERRO-BRAD7100  TO ERR-TEXTO                     
               PERFORM 9999-PROCESSAR-ROTINA-ERRO                       
           END-IF.                                                      
                                                                        
       9030-99-FIM.                    EXIT.                            
                                                                        
      *----------------------------------------------------------------*
       9031-TESTAR-FS-BLQREST.                                          
      *----------------------------------------------------------------*
                                                                        
           IF  (WRK-FS-BLQREST         NOT EQUAL '00')                  
               MOVE 'BLQREST'          TO WRK-NOME-ARQ                  
               MOVE WRK-FS-BLQREST     TO WRK-FILE-STATUS               
               MOVE WRK-ERRO-BRAD7100  TO ERR-TEXTO                     
               PERFORM 9999-PROCESSAR-ROTINA-ERRO                       
           END-IF.                                                      
                                                                        
       9031-99-FIM.                    EXIT.                            
                                                                        
      *----------------------------------------------------------------*
       9040-TESTAR-FS-RELATO.                                           
      *----------------------------------------------------------------*
                                                                        
           IF  (WRK-FS-RELATO          NOT EQUAL '00')                  
               MOVE 'RELATO'           TO WRK-NOME-ARQ                  
               MOVE WRK-FS-RELATO      TO WRK-FILE-STATUS               
               MOVE WRK-ERRO-BRAD7100  TO ERR-TEXTO                     
               PERFORM 9999-PROCESSAR-ROTINA-ERRO                       
           END-IF.                                                      
                                                                        
       9040-99-FIM.                    EXIT.                            
                                                                        
      *----------------------------------------------------------------*
       9050-TESTAR-FS-RELATO2.                                          
      *----------------------------------------------------------------*
                                                                        
           IF  (WRK-FS-RELATO2         NOT EQUAL '00')                  
               MOVE 'RELATO2'          TO WRK-NOME-ARQ                  
               MOVE WRK-FS-RELATO2     TO WRK-FILE-STATUS               
               MOVE WRK-ERRO-BRAD7100  TO ERR-TEXTO                     
               PERFORM 9999-PROCESSAR-ROTINA-ERRO                       
           END-IF.                                                      
                                                                        
       9050-99-FIM.                    EXIT.                            
                                                                        
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO.                                      
      *----------------------------------------------------------------*
                                                                        
           MOVE 'CLLP7103'             TO ERR-PGM                       
           MOVE 'APL'                  TO ERR-TIPO-ACESSO               
                                                                        
           CALL 'BRAD7100'          USING WRK-BATCH                     
                                          ERRO-AREA.                    
                                                                        
           GOBACK.                                                      
                                                                        
      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*

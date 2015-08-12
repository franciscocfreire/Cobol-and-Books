      *================================================================*
       IDENTIFICATION                  DIVISION.                        
      *================================================================*
       PROGRAM-ID. CLLPG647.                                            
       AUTHOR.     CPMACT 1.4.                                          
      *REMARKS.                                                         
      *    PROGRAMA ORIGINALMENTE CODIFICADO EM ASSEMBLER,              
      *    CONVERTIDO PARA COBOL ENTERPRISE PELO CONVERSOR:             
      *    CPMBRAXIS   "ACT 1.4 -  ASSEMBLER-COBOL TRANSLATOR".         
      *INSTALLATION.                                                    
      *DATE-WRITTEN.                                                    
      *DATE-COMPILED.                                                   
      *SECURITY.                                                        
      *=================================================================
      *                   U L T I M A   A L T E R A C A O               
      *-----------------------------------------------------------------
      *     PROGRAMADOR  :                                              
      *     SUPERVISOR   :                                              
      *     ANALISTA     :                                              
      *     DATA         :                                              
      *                                                                 
      *     OBJETIVO     :  LISTAGEM DOS AVALISTAS COM EMISSAO DE AVISO 
      *                                                                 
      *                                                                 
      *                                                                 
      *================================================================*
      *               B R Q     I T     S E R V I C E S                *
      *================================================================*
      *                       A L T E R A C A O                        *
      *----------------------------------------------------------------*
      *    PROGRAMADOR.:  HENRIQUE GUIMARAES      - BRQ                *
      *    ANALISTA....:  HENRIQUE GUIMARAES      - BRQ                *
      *    DATA........:  10/04/2014                                   *
      *----------------------------------------------------------------*
      *    OBJETIVO....:  ADAPTACOES PARA A LEI DA TRANSPARENCIA       *
      *    TROCA BOOK INTERNA  POR I#CLLPLJ.                           *
      *    CORRIGIR TAMANHO DE 600 PRA 1290.                           *
      *    PROJETO 13-0358                                             *
      *================================================================*
       ENVIRONMENT                     DIVISION.                        
      *================================================================*
       CONFIGURATION                   SECTION.                         
                                                                        
       SPECIAL-NAMES.                                                   
                                       DECIMAL-POINT IS COMMA           
                                       C01 IS CANAL1.                   
                                                                        
      *----------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.                         
      *----------------------------------------------------------------*
                                                                        
       FILE-CONTROL.                                                    
           SELECT ARQDATAS ASSIGN      TO  UT-S-ARQDATAS                
           FILE STATUS                 IS  WRK-FS-ARQDATAS.             
           SELECT ARQAVISO ASSIGN      TO  UT-S-ARQAVISO                
           FILE STATUS                 IS  WRK-FS-ARQAVISO.             
           SELECT RELATO ASSIGN        TO  UT-S-RELATO                  
           FILE STATUS                 IS  WRK-FS-RELATO.               
           SELECT TABULA ASSIGN        TO  UT-S-TABULA                  
           FILE STATUS                 IS  WRK-FS-TABULA.               
      *================================================================*
       DATA                            DIVISION.                        
      *================================================================*
       FILE                            SECTION.                         
      *----------------------------------------------------------------*
      *      INPUT  : ARQUIVO ARQDATAS                                 *
      *               ORG.           - LRECL = 060                     *
      *----------------------------------------------------------------*
       FD  ARQDATAS                                                     
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
       01  FD-REG-ARQDATAS             PIC  X(060).                     
      *----------------------------------------------------------------*
      *      INPUT  : ARQUIVO ARQAVISO                                 *
      *               ORG.           - LRECL = 1290                    *
      *----------------------------------------------------------------*
       FD  ARQAVISO                                                     
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
-INC I#CLLPLJ                                                           
      *----------------------------------------------------------------*
      *      OUTPUT : ARQUIVO RELATO                                   *
      *               ORG.           -  LRECL = 133                    *
      *----------------------------------------------------------------*
       FD  RELATO                                                       
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
       01  FD-REG-RELATO               PIC  X(133).                     
      *----------------------------------------------------------------*
      *      OUTPUT : ARQUIVO TABULA                                   *
      *               ORG.           -  LRECL = 080                    *
      *----------------------------------------------------------------*
       FD  TABULA                                                       
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
       01  FD-REG-TABULA               PIC  X(080).                     
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
       77  WRK-FS-ARQDATAS             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-ARQAVISO             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-RELATO               PIC  X(002)         VALUE SPACES.
       77  WRK-FS-TABULA               PIC  X(002)         VALUE SPACES.
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
                                                                        
CPMCAC*----------------------------------------------------------------*
CPMCAC 77  FILLER                      PIC  X(021)         VALUE        
CPMCAC     'AREA PARA INDEXADORES'.                                     
CPMCAC*----------------------------------------------------------------*
                                                                        
CPMCAC 77  IND-TABELA                  PIC  9(009) COMP-3  VALUE ZEROS. 
                                                                        
       01  WRK-WKDATA                  PIC  X(060)         VALUE SPACES.
                                                                        
       01  WRK-CAB0.                                                    
           05  FILLER                  PIC  X(001)         VALUE X'89'. 
           05  FILLER                  PIC  X(132)         VALUE ' '.   
                                                                        
       01  WRK-CABT.                                                    
           05  FILLER                  PIC  X(001)         VALUE X'09'. 
           05  FILLER                  PIC  X(028)         VALUE        
               '*CLLPG647*'.                                            
           05  WRK-LITECC1             PIC  X(020)         VALUE SPACES.
           05  FILLER                  PIC  X(084)         VALUE ' '.   
                                                                        
       01  WRK-CAB1.                                                    
           05  FILLER                  PIC  X(001)         VALUE X'09'. 
           05  FILLER                  PIC  X(015)         VALUE ' '.   
           05  FILLER                  PIC  X(117)         VALUE        
              'LISTAGEM DOS AVALISTAS COM EMISSAO DE AVISOS - CORRENTIST
      -       'A'.                                                      
                                                                        
       01  WRK-CAB1A.                                                   
           05  FILLER                  PIC  X(001)         VALUE X'09'. 
           05  FILLER                  PIC  X(015)         VALUE ' '.   
           05  FILLER                  PIC  X(117)         VALUE        
               'LISTAGEM DOS AVALISTAS COM EMISSAO DE AVISOS - NAO CORRE
      -        'NTISTA'.                                                
                                                                        
       01  WRK-CAB2.                                                    
           05  FILLER                  PIC  X(001)         VALUE X'11'. 
           05  FILLER                  PIC  X(015)         VALUE ' '.   
           05  FILLER                  PIC  X(006)         VALUE        
               'MOVTO-'.                                                
           05  WRK-LDATA               PIC  X(010)     VALUE LOW-VALUES.
           05  FILLER                  PIC  X(010)         VALUE ' '.   
           05  FILLER                  PIC  X(001)         VALUE '*'.   
           05  WRK-LDESTINO            PIC  X(007)         VALUE SPACES.
           05  FILLER                  PIC  X(001)         VALUE '*'.   
CPMCAC     05  FILLER                  PIC  X(082)         VALUE ' '.   
                                                                        
       01  WRK-CAB3.                                                    
           05  FILLER                  PIC  X(001)         VALUE X'11'. 
           05  FILLER                  PIC  X(017)         VALUE        
               '  CPF/CGC'.                                             
           05  FILLER                  PIC  X(003)         VALUE ' '.   
           05  FILLER                  PIC  X(004)         VALUE 'AGEN'.
           05  FILLER                  PIC  X(010)         VALUE        
               '       C/C'.                                            
           05  FILLER                  PIC  X(018)         VALUE        
               '             VALOR'.                                    
           05  FILLER                  PIC  X(003)         VALUE ' '.   
           05  FILLER                  PIC  X(040)         VALUE        
               'NOME DO AVALISTA'.                                      
CPMCAC     05  FILLER                  PIC  X(037)         VALUE ' '.   
                                                                        
       01  WRK-LDET.                                                    
           05  FILLER                  PIC  X(001)         VALUE X'09'. 
           05  WRK-LCPFNUM             PIC  X(009)         VALUE SPACES.
           05  WRK-LCPFNUM-REDCPMACT   REDEFINES  WRK-LCPFNUM           
                                       PIC  9(009).                     
           05  FILLER                  PIC  X(001)         VALUE '-'.   
           05  WRK-LCPFFIL             PIC  X(004)         VALUE SPACES.
           05  WRK-LCPFFIL-REDCPMACT   REDEFINES  WRK-LCPFFIL           
                                       PIC  9(004).                     
           05  FILLER                  PIC  X(001)         VALUE '-'.   
           05  WRK-LCPFCTR             PIC  X(002)         VALUE SPACES.
           05  WRK-LCPFCTR-REDCPMACT   REDEFINES  WRK-LCPFCTR           
                                       PIC  9(002).                     
           05  FILLER                  PIC  X(003)         VALUE ' '.   
           05  WRK-LAGEN               PIC  X(004)         VALUE SPACES.
           05  WRK-LAGEN-REDCPMACT     REDEFINES  WRK-LAGEN             
                                       PIC  9(004).                     
           05  WRK-LCONTA              PIC  X(010)         VALUE SPACES.
           05  WRK-LVALOR              PIC  X(018)         VALUE SPACES.
           05  FILLER                  PIC  X(003)         VALUE ' '.   
           05  WRK-LNOME               PIC  X(040)         VALUE SPACES.
CPMCAC     05  FILLER                  PIC  X(037) VALUE ' '.           
                                                                        
       01  WRK-PARM.                                                    
           05  WRK-DESTINO             PIC  X(007)         VALUE SPACES.
           05  WRK-CORRENT             PIC  X(001)         VALUE SPACES.
           05  WRK-CCUSTO              PIC  X(004)         VALUE SPACES.
                                                                        
       01  WRK-QTDELE                  PIC  9(007) COMP-3  VALUE ZEROS. 
       01  WRK-QTDESEL                 PIC  9(007) COMP-3  VALUE ZEROS. 
       01  WRK-TQTDE                   PIC S9(009) COMP-3  VALUE ZEROS. 
       01  WRK-IND1                    PIC 9(03) COMP-3 VALUE ZEROS.    
                                                                        
       01  WRK-TOTAL                   PIC S9(013) COMP-3  VALUE ZEROS. 
CPMCAC 01  WRK-TOTALV99                REDEFINES  WRK-TOTAL             
CPMCAC                                 PIC S9(011)V99 COMP-3.           
                                                                        
       01  WRK-FOL                     PIC  9(003) COMP-3  VALUE 55.    
                                                                        
       01  WRK-PAD18                   PIC BZZ.ZZZ.ZZZ.ZZ9,99           
                                                           VALUE SPACES.
CPMCAC 01  WRK-PAD18-R                 REDEFINES  WRK-PAD18             
CPMCAC                                 PIC  X(018).                     
                                                                        
       01  WRK-PAD10                   PIC BZ.ZZZ.ZZZ      VALUE SPACES.
CPMCAC 01  WRK-PAD10-R                 REDEFINES  WRK-PAD10             
CPMCAC                                 PIC  X(010).                     
                                                                        
       01  WRK-PAD12                   PIC BZZZ.ZZZ.ZZZ    VALUE SPACES.
CPMCAC 01  WRK-PAD12-R                 REDEFINES  WRK-PAD12             
CPMCAC                                 PIC  X(012).                     
                                                                        
       01  WRK-CAB02.                                                   
           05  FILLER                  PIC  X(001)         VALUE X'89'. 
           05  FILLER                  PIC  X(079)         VALUE ' '.   
                                                                        
       01  WRK-CAB12A.                                                  
           05  FILLER                  PIC  X(001)         VALUE X'11'. 
           05  FILLER                  PIC  X(028)         VALUE        
               '*CLLPG647*'.                                            
           05  WRK-LITECC2             PIC  X(020)         VALUE SPACES.
           05  FILLER                  PIC  X(031)         VALUE ' '.   
                                                                        
       01  WRK-CAB12B.                                                  
           05  FILLER                  PIC  X(001)         VALUE X'11'. 
           05  FILLER                  PIC  X(015)         VALUE ' '.   
           05  WRK-LITERAL             PIC  X(031)         VALUE SPACES.
           05  FILLER                  PIC  X(006)         VALUE        
               'MOVTO-'.                                                
           05  WRK-LDATA2              PIC  X(010)     VALUE LOW-VALUES.
           05  FILLER                  PIC  X(002)         VALUE ' '.   
           05  FILLER                  PIC  X(001)         VALUE '*'.   
           05  WRK-LDESTIN2            PIC  X(007)         VALUE SPACES.
           05  FILLER                  PIC  X(001)         VALUE '*'.   
CPMCAC     05  FILLER                  PIC  X(006)         VALUE ' '.   
                                                                        
       01  WRK-CAB22.                                                   
           05  FILLER                  PIC  X(001)         VALUE X'11'. 
           05  FILLER                  PIC  X(017)         VALUE ' '.   
           05  FILLER                  PIC  X(018)         VALUE        
               '      VALOR MINIMO'.                                    
           05  FILLER                  PIC  X(005)         VALUE        
               '  A  '.                                                 
           05  FILLER                  PIC  X(018)         VALUE        
               '      VALOR MAXIMO'.                                    
           05  FILLER                  PIC  X(003)         VALUE ' '.   
           05  FILLER                  PIC  X(012)         VALUE        
               '  QUANTIDADE'.                                          
CPMCAC     05  FILLER                  PIC  X(006)         VALUE ' '.   
                                                                        
       01  WRK-LTABU.                                                   
           15  FILLER                  PIC  X(001)         VALUE X'11'. 
           15  FILLER                  PIC  X(015)         VALUE ' '.   
           15  FILLER                  PIC  X(002)         VALUE 'DE'.  
           15  WRK-LVALOR1             PIC  X(018)         VALUE SPACES.
           15  FILLER                  PIC  X(005)         VALUE        
               '  A  '.                                                 
           15  WRK-LVALOR2             PIC  X(018)         VALUE SPACES.
           15  FILLER                  PIC  X(003)         VALUE ' '.   
           15  WRK-LQTDE               PIC  X(012)         VALUE SPACES.
CPMCAC     15  FILLER                  PIC  X(006)         VALUE ' '.   
                                                                        
       01  WRK-TABELA.                                                  
           05  FILLER                  PIC  X(007)         VALUE        
               X'0000000000001F'.                                       
           05  FILLER                  PIC  X(007)         VALUE        
               X'0000000000999F'.                                       
           05  FILLER                  PIC  X(005)         VALUE        
               X'000000000F'.                                           
           05  FILLER                  PIC  X(007)         VALUE        
               X'0000000001000F'.                                       
           05  FILLER                  PIC  X(007)         VALUE        
               X'0000000001499F'.                                       
           05  FILLER                  PIC  X(005)         VALUE        
               X'000000000F'.                                           
           05  FILLER                  PIC  X(007)         VALUE        
               X'0000000001500F'.                                       
           05  FILLER                  PIC  X(007)         VALUE        
               X'0000000001999F'.                                       
           05  FILLER                  PIC  X(005)         VALUE        
               X'000000000F'.                                           
           05  FILLER                  PIC  X(007)         VALUE        
               X'0000000002000F'.                                       
           05  FILLER                  PIC  X(007)         VALUE        
               X'0000000002999F'.                                       
           05  FILLER                  PIC  X(005)         VALUE        
               X'000000000F'.                                           
           05  FILLER                  PIC  X(007)         VALUE        
               X'0000000003000F'.                                       
           05  FILLER                  PIC  X(007)         VALUE        
               X'0000000003999F'.                                       
           05  FILLER                  PIC  X(005)         VALUE        
               X'000000000F'.                                           
           05  FILLER                  PIC  X(007)         VALUE        
               X'0000000004000F'.                                       
           05  FILLER                  PIC  X(007)         VALUE        
               X'0000000004999F'.                                       
           05  FILLER                  PIC  X(005)         VALUE        
               X'000000000F'.                                           
           05  FILLER                  PIC  X(007)         VALUE        
               X'0000000005000F'.                                       
           05  FILLER                  PIC  X(007)         VALUE        
               X'0000000009999F'.                                       
           05  FILLER                  PIC  X(005)         VALUE        
               X'000000000F'.                                           
           05  FILLER                  PIC  X(007)         VALUE        
               X'0000000010000F'.                                       
           05  FILLER                  PIC  X(007)         VALUE        
               X'0000000019999F'.                                       
           05  FILLER                  PIC  X(005)         VALUE        
               X'000000000F'.                                           
           05  FILLER                  PIC  X(007)         VALUE        
               X'0000000020000F'.                                       
           05  FILLER                  PIC  X(007)         VALUE        
               X'0000000029999F'.                                       
           05  FILLER                  PIC  X(005)         VALUE        
               X'000000000F'.                                           
           05  FILLER                  PIC  X(007)         VALUE        
               X'0000000030000F'.                                       
           05  FILLER                  PIC  X(007)         VALUE        
               X'0000000039999F'.                                       
           05  FILLER                  PIC  X(005)         VALUE        
               X'000000000F'.                                           
           05  FILLER                  PIC  X(007)         VALUE        
               X'0000000040000F'.                                       
           05  FILLER                  PIC  X(007)         VALUE        
               X'0000000049999F'.                                       
           05  FILLER                  PIC  X(005)         VALUE        
               X'000000000F'.                                           
           05  FILLER                  PIC  X(007)         VALUE        
               X'0000000050000F'.                                       
           05  FILLER                  PIC  X(007)         VALUE        
               X'0000000059999F'.                                       
           05  FILLER                  PIC  X(005)         VALUE        
               X'000000000F'.                                           
           05  FILLER                  PIC  X(007)         VALUE        
               X'0000000060000F'.                                       
           05  FILLER                  PIC  X(007)         VALUE        
               X'0000000069999F'.                                       
           05  FILLER                  PIC  X(005)         VALUE        
               X'000000000F'.                                           
           05  FILLER                  PIC  X(007)         VALUE        
               X'0000000070000F'.                                       
           05  FILLER                  PIC  X(007)         VALUE        
               X'0000000079999F'.                                       
           05  FILLER                  PIC  X(005)         VALUE        
               X'000000000F'.                                           
           05  FILLER                  PIC  X(007)         VALUE        
               X'0000000080000F'.                                       
           05  FILLER                  PIC  X(007)         VALUE        
               X'0000000089999F'.                                       
           05  FILLER                  PIC  X(005)         VALUE        
               X'000000000F'.                                           
           05  FILLER                  PIC  X(007)         VALUE        
               X'0000000090000F'.                                       
           05  FILLER                  PIC  X(007)         VALUE        
               X'0000000099999F'.                                       
           05  FILLER                  PIC  X(005)         VALUE        
               X'000000000F'.                                           
           05  FILLER                  PIC  X(007)         VALUE        
               X'0000000100000F'.                                       
           05  FILLER                  PIC  X(007)         VALUE        
               X'9999999999999F'.                                       
           05  FILLER                  PIC  X(005)         VALUE        
               X'000000000F'.                                           
           05  FILLER                  PIC  X(019)         VALUE '*'.   
           05  FILLER                  PIC  X(100)         VALUE ' '.   
                                                                        
CPMCAC 01  WRK-AUX                     PIC  X(007)         VALUE SPACES.
CPMCAC 01  WRK-AUX-R                   REDEFINES  WRK-AUX               
CPMCAC                                 PIC S9(013) COMP-3.              
CPMCAC 01  WRK-AUX-11V99               REDEFINES  WRK-AUX               
CPMCAC                                 PIC S9(011)V99 COMP-3.           
CPMCAC 01  FILLER                      REDEFINES  WRK-AUX.              
CPMCAC     05  WRK-AUX-9-9             PIC S9(009) COMP-3.              
CPMCAC     05  FILLER                  PIC  X(002).                     
                                                                        
CPMCAC 01  WRK-9-9-NUM                 PIC S9(009) COMP-3  VALUE ZEROS. 
CPMCAC 01  WRK-X-5-ALP                 REDEFINES  WRK-9-9-NUM           
CPMCAC                                 PIC  X(005).                     
                                                                        
CPMCAC 01  WRK-9-5                     PIC  9(005)         VALUE ZEROS. 
CPMCAC 01  FILLER                      REDEFINES  WRK-9-5.              
CPMCAC     05  FILLER                  PIC  9(001).                     
CPMCAC     05  WRK-9-4                 PIC  9(004).                     
                                                                        
CPMCAC 01  WRK-9-3                     PIC  9(003)         VALUE ZEROS. 
CPMCAC 01  FILLER                      REDEFINES  WRK-9-3.              
CPMCAC     05  FILLER                  PIC  9(001).                     
CPMCAC     05  WRK-9-2                 PIC  99.                         
                                                                        
CPMCAC 01  WRK-S9-11V99                PIC +9(011)V99      VALUE ZEROS. 
CPMCAC 01  FILLER                      REDEFINES WRK-S9-11V99.          
CPMCAC     05 FILLER                   PIC  X(001).                     
CPMCAC     05 WRK-9-11V99              PIC  9(011)V99.                  
                                                                        
CPMCAC 01  WRK-S9-9                    PIC +9(009)         VALUE ZEROS. 
CPMCAC 01  FILLER                      REDEFINES WRK-S9-9.              
CPMCAC     05 FILLER                   PIC  X(001).                     
CPMCAC     05 WRK-9-9                  PIC  9(009).                     
                                                                        
           COPY 'I#BRAD7C'.
                                                                        
      *----------------------------------------------------------------*
       01  FILLER                     PIC  X(050)          VALUE        
           'FIM DA WORKING STORAGE SECTION '.                           
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
CPMCAC LINKAGE                         SECTION.                         
      *----------------------------------------------------------------*
                                                                        
CPMCAC 01  LNK-PARM-AREA.                                               
CPMCAC     05 LNK-TAM-PARM             PIC S9(004) COMP    VALUE ZEROS. 
CPMCAC     05 LNK-PARM                 PIC  X(012)         VALUE SPACES.
                                                                        
      *================================================================*
       PROCEDURE                       DIVISION USING LNK-PARM-AREA.    
      *================================================================*
                                                                        
           MOVE  LNK-PARM              TO  WRK-PARM                     
           MOVE  WRK-DESTINO           TO  WRK-LDESTINO                 
           MOVE  WRK-DESTINO           TO  WRK-LDESTIN2                 
      *                                                                 
           MOVE  'AVISO COBRANCA-NAO CORRENTISTA '                      
                                       TO  WRK-LITERAL                  
                                                                        
           IF  WRK-CORRENT      NOT EQUAL  'C'                          
               GO TO 0010-VECCUST                                       
           END-IF                                                       
      *                                                                 
           MOVE  'AVISO DE COBRANCA-CORRENTISTA  '                      
                                       TO  WRK-LITERAL.                 
      *                                                                 
      *----------------------------------------------------------------*
       0010-VECCUST.                                                    
      *----------------------------------------------------------------*
                                                                        
           MOVE  'BANCO BRADESCO S/A  ' TO  WRK-LITECC1                 
           MOVE  WRK-LITECC1           TO  WRK-LITECC2                  
                                                                        
           IF  WRK-CCUSTO       NOT EQUAL  'LEAS'                       
               GO TO 0020-INICIO                                        
           END-IF                                                       
      *                                                                 
           MOVE 'BRADESCO LEASING S/A' TO  WRK-LITECC1                  
           MOVE  WRK-LITECC1           TO  WRK-LITECC2.                 
      *                                                                 
      *----------------------------------------------------------------*
       0020-INICIO.                                                     
      *----------------------------------------------------------------*
                                                                        
           OPEN INPUT ARQDATAS                                          
                                                                        
           MOVE  WRK-ABERTURA          TO  WRK-OPERACAO                 
                                                                        
           PERFORM 9010-TESTAR-FS-ARQDATAS THRU 9010-99-FIM.            
      *                                                                 
      *----------------------------------------------------------------*
       0030-LEDATA.                                                     
      *----------------------------------------------------------------*
                                                                        
           READ ARQDATAS             INTO  WRK-WKDATA END-READ          
                                                                        
           MOVE  WRK-LEITURA           TO  WRK-OPERACAO                 
                                                                        
           IF  WRK-FS-ARQDATAS      EQUAL  '10'                         
               GO TO 0040-FIMDATA                                       
           ELSE                                                         
               PERFORM 9010-TESTAR-FS-ARQDATAS THRU 9010-99-FIM         
           END-IF                                                       
      *                                                                 
           MOVE  WRK-WKDATA(1:10)      TO  WRK-LDATA                    
           MOVE  WRK-WKDATA(1:10)      TO  WRK-LDATA2.                  
                                                                        
      *----------------------------------------------------------------*
       0040-FIMDATA.                                                    
      *----------------------------------------------------------------*
                                                                        
           CLOSE ARQDATAS                                               
                                                                        
           MOVE  WRK-FECHAMENTO        TO  WRK-OPERACAO                 
                                                                        
           PERFORM 9010-TESTAR-FS-ARQDATAS THRU 9010-99-FIM             
      *                                                                 
           OPEN  INPUT  ARQAVISO                                        
                OUTPUT  RELATO                                          
                        TABULA                                          
                                                                        
           MOVE  WRK-ABERTURA          TO  WRK-OPERACAO                 
                                                                        
           PERFORM 9020-TESTAR-FS-ARQAVISO THRU 9020-99-FIM             
           PERFORM 9030-TESTAR-FS-RELATO   THRU 9030-99-FIM             
           PERFORM 9040-TESTAR-FS-TABULA   THRU 9040-99-FIM.            
      *                                                                 
      *----------------------------------------------------------------*
       0050-LER.                                                        
      *----------------------------------------------------------------*
                                                                        
           READ ARQAVISO                          END-READ              
                                                                        
           MOVE  WRK-LEITURA           TO  WRK-OPERACAO                 
                                                                        
           IF  WRK-FS-ARQAVISO      EQUAL  '10'                         
               GO TO 0110-FIM                                           
           ELSE                                                         
               PERFORM 9020-TESTAR-FS-ARQAVISO THRU 9020-99-FIM         
           END-IF                                                       
      *                                                                 
           ADD 1                       TO  WRK-QTDELE                   
                                                                        
                                                                        
           MOVE ZEROS                  TO WRK-TOTAL                     
           PERFORM                                                      
               VARYING WRK-IND1        FROM 1 BY 1 UNTIL                
               WRK-IND1                GREATER 11                       
               ADD AVI-RESGATE (WRK-IND1)                               
                                       TO WRK-TOTALV99
           END-PERFORM.                                                 
                                                                        
           MOVE 1                      TO  IND-TABELA.                  
                                                                        
      *----------------------------------------------------------------*
       0060-COMPARA.                                                    
      *----------------------------------------------------------------*
                                                                        
CPMCAC     MOVE WRK-TABELA(IND-TABELA + 7:7)                            
CPMCAC                                 TO WRK-AUX                       
                                                                        
CPMCAC     IF  WRK-TOTAL               NOT GREATER WRK-AUX-R            
CPMCAC         GO TO 0070-SOMAR                                         
CPMCAC     END-IF                                                       
                                                                        
           ADD  19                     TO  IND-TABELA                   
                                                                        
CPMCAC     MOVE WRK-TABELA(IND-TABELA:7)                                
CPMCAC                                 TO WRK-AUX                       
                                                                        
CPMCAC     IF  WRK-AUX-R               GREATER 100000                   
               GO TO 0070-SOMAR                                         
           END-IF                                                       
                                                                        
           GO TO 0060-COMPARA.                                          
                                                                        
      *----------------------------------------------------------------*
       0070-SOMAR.                                                      
      *----------------------------------------------------------------*
                                                                        
CPMCAC     MOVE  WRK-TABELA(IND-TABELA + 14:5)                          
CPMCAC                                 TO  WRK-X-5-ALP                  
                                                                        
CPMCAC     ADD 1                       TO  WRK-9-9-NUM                  
                                                                        
CPMCAC     MOVE  WRK-X-5-ALP           TO  WRK-TABELA(IND-TABELA + 14:5)
      *                                                                 
           ADD 1                       TO  WRK-QTDESEL                  
      *                                                                 
           MOVE  AVI-CGC-AVAL          TO  WRK-LCPFNUM-REDCPMACT        
CPMCAC     MOVE  AVI-FIL-AVAL          TO  WRK-9-5                      
CPMCAC     MOVE  WRK-9-4               TO  WRK-LCPFFIL-REDCPMACT        
CPMCAC     MOVE  AVI-CTR-AVAL          TO  WRK-9-3                      
CPMCAC     MOVE  WRK-9-2               TO  WRK-LCPFCTR-REDCPMACT        
CPMCAC     MOVE  AVI-AGENCIA           TO  WRK-9-5                      
CPMCAC     MOVE  WRK-9-4               TO  WRK-LAGEN-REDCPMACT          
           MOVE  AVI-NUM-CC            TO  WRK-PAD10                    
           MOVE  WRK-PAD10-R           TO  WRK-LCONTA                   
           MOVE  AVI-NOME-AVAL         TO  WRK-LNOME                    
                                                                        
CPMCAC     MOVE  WRK-TOTALV99          TO  WRK-S9-11V99                 
CPMCAC     MOVE  WRK-9-11V99           TO  WRK-PAD18                    
CPMCAC     MOVE  WRK-PAD18-R           TO  WRK-LVALOR                   
                                                                        
           IF  WRK-FOL               LESS  55                           
               GO TO 0100-NQB                                           
           END-IF                                                       
                                                                        
           MOVE  ZEROS                 TO  WRK-FOL                      
                                                                        
           WRITE FD-REG-RELATO       FROM  WRK-CAB0                     
                                                                        
           MOVE  WRK-GRAVACAO          TO  WRK-OPERACAO                 
                                                                        
           PERFORM 9030-TESTAR-FS-RELATO THRU 9030-99-FIM               
      *                                                                 
           WRITE FD-REG-RELATO       FROM  WRK-CABT                     
                                                                        
           MOVE  WRK-GRAVACAO          TO  WRK-OPERACAO                 
                                                                        
           PERFORM 9030-TESTAR-FS-RELATO THRU 9030-99-FIM               
      *                                                                 
           IF  WRK-CORRENT      NOT EQUAL  'C'                          
               GO TO 0080-PUTNAO1                                       
           END-IF                                                       
                                                                        
           WRITE FD-REG-RELATO       FROM  WRK-CAB1                     
                                                                        
           MOVE  WRK-GRAVACAO          TO  WRK-OPERACAO                 
                                                                        
           PERFORM 9030-TESTAR-FS-RELATO THRU 9030-99-FIM               
      *                                                                 
           GO TO 0090-PUTCOR1.                                          
                                                                        
      *----------------------------------------------------------------*
       0080-PUTNAO1.                                                    
      *----------------------------------------------------------------*
                                                                        
           WRITE FD-REG-RELATO       FROM  WRK-CAB1A                    
                                                                        
           MOVE  WRK-GRAVACAO          TO  WRK-OPERACAO                 
                                                                        
           PERFORM 9030-TESTAR-FS-RELATO THRU 9030-99-FIM.              
      *                                                                 
      *----------------------------------------------------------------*
       0090-PUTCOR1.                                                    
      *----------------------------------------------------------------*
                                                                        
           WRITE FD-REG-RELATO       FROM  WRK-CAB2                     
                                                                        
           MOVE  WRK-GRAVACAO          TO  WRK-OPERACAO                 
                                                                        
           PERFORM 9030-TESTAR-FS-RELATO THRU 9030-99-FIM               
      *                                                                 
           WRITE FD-REG-RELATO       FROM  WRK-CAB3                     
                                                                        
           MOVE  WRK-GRAVACAO          TO  WRK-OPERACAO                 
                                                                        
           PERFORM 9030-TESTAR-FS-RELATO THRU 9030-99-FIM.              
      *                                                                 
      *----------------------------------------------------------------*
       0100-NQB.                                                        
      *----------------------------------------------------------------*
                                                                        
           ADD 1                       TO  WRK-FOL                      
                                                                        
           WRITE FD-REG-RELATO       FROM  WRK-LDET                     
                                                                        
           MOVE  WRK-GRAVACAO          TO  WRK-OPERACAO                 
                                                                        
           PERFORM 9030-TESTAR-FS-RELATO THRU 9030-99-FIM               
      *                                                                 
           GO TO 0050-LER.                                              
                                                                        
      *----------------------------------------------------------------*
       0110-FIM.                                                        
      *----------------------------------------------------------------*
                                                                        
           IF  WRK-QTDELE       NOT EQUAL  0                            
               GO TO 0140-LISTTOT                                       
           END-IF                                                       
                                                                        
           WRITE FD-REG-RELATO       FROM  WRK-CAB0                     
                                                                        
           MOVE  WRK-GRAVACAO          TO  WRK-OPERACAO                 
                                                                        
           PERFORM 9030-TESTAR-FS-RELATO THRU 9030-99-FIM               
      *                                                                 
           WRITE FD-REG-RELATO       FROM  WRK-CABT                     
                                                                        
           MOVE  WRK-GRAVACAO          TO  WRK-OPERACAO                 
                                                                        
           PERFORM 9030-TESTAR-FS-RELATO THRU 9030-99-FIM               
      *                                                                 
           IF  WRK-CORRENT      NOT EQUAL  'C'                          
               GO TO 0120-PUTNAO2                                       
           END-IF                                                       
                                                                        
           WRITE FD-REG-RELATO       FROM  WRK-CAB1                     
                                                                        
           MOVE  WRK-GRAVACAO          TO  WRK-OPERACAO                 
                                                                        
           PERFORM 9030-TESTAR-FS-RELATO THRU 9030-99-FIM               
      *                                                                 
           GO TO 0130-PUTCOR2.                                          
                                                                        
      *----------------------------------------------------------------*
       0120-PUTNAO2.                                                    
      *----------------------------------------------------------------*
                                                                        
           WRITE FD-REG-RELATO       FROM  WRK-CAB1A                    
                                                                        
           MOVE  WRK-GRAVACAO          TO  WRK-OPERACAO                 
                                                                        
           PERFORM 9030-TESTAR-FS-RELATO THRU 9030-99-FIM.              
      *                                                                 
      *----------------------------------------------------------------*
       0130-PUTCOR2.                                                    
      *----------------------------------------------------------------*
                                                                        
           WRITE FD-REG-RELATO       FROM  WRK-CAB2                     
                                                                        
           MOVE  WRK-GRAVACAO          TO  WRK-OPERACAO                 
                                                                        
           PERFORM 9030-TESTAR-FS-RELATO THRU 9030-99-FIM               
      *                                                                 
           WRITE FD-REG-RELATO       FROM  WRK-CAB3                     
                                                                        
           MOVE  WRK-GRAVACAO          TO  WRK-OPERACAO                 
                                                                        
           PERFORM 9030-TESTAR-FS-RELATO THRU 9030-99-FIM.              
      *                                                                 
      *----------------------------------------------------------------*
       0140-LISTTOT.                                                    
      *----------------------------------------------------------------*
                                                                        
           MOVE  ' '                   TO  WRK-LDET(2:132)              
      *                                                                 
           MOVE 'QTDE DE REGISTROS -'  TO  WRK-LDET(2:132)              
           MOVE  WRK-QTDELE            TO  WRK-PAD10                    
           MOVE  WRK-PAD10-R           TO  WRK-LDET(22:10)              
           MOVE  '-LIDOS'              TO  WRK-LDET(32:6)               
           MOVE  WRK-PAD10-R           TO  WRK-LDET(38:10)              
      *                                                                 
           WRITE FD-REG-RELATO       FROM  WRK-LDET                     
                                                                        
           MOVE  WRK-GRAVACAO          TO  WRK-OPERACAO                 
                                                                        
           PERFORM 9030-TESTAR-FS-RELATO THRU 9030-99-FIM               
      *                                                                 
           WRITE FD-REG-TABULA       FROM  WRK-CAB02                    
                                                                        
           MOVE  WRK-GRAVACAO          TO  WRK-OPERACAO                 
                                                                        
           PERFORM 9040-TESTAR-FS-TABULA THRU 9040-99-FIM               
      *                                                                 
           WRITE FD-REG-TABULA       FROM  WRK-CAB12A                   
                                                                        
           MOVE  WRK-GRAVACAO          TO  WRK-OPERACAO                 
                                                                        
           PERFORM 9040-TESTAR-FS-TABULA THRU 9040-99-FIM               
      *                                                                 
           WRITE FD-REG-TABULA       FROM  WRK-CAB12B                   
                                                                        
           MOVE  WRK-GRAVACAO          TO  WRK-OPERACAO                 
                                                                        
           PERFORM 9040-TESTAR-FS-TABULA THRU 9040-99-FIM               
      *                                                                 
           WRITE FD-REG-TABULA       FROM  WRK-CAB22                    
                                                                        
           MOVE  WRK-GRAVACAO          TO  WRK-OPERACAO                 
                                                                        
           PERFORM 9040-TESTAR-FS-TABULA THRU 9040-99-FIM               
      *                                                                 
           MOVE 1                      TO  IND-TABELA.                  
                                                                        
      *----------------------------------------------------------------*
       0150-LISTABU.                                                    
      *----------------------------------------------------------------*
                                                                        
           MOVE  WRK-TABELA(IND-TABELA:7)                               
                                       TO  WRK-AUX                      
           MOVE  WRK-AUX-11V99         TO  WRK-S9-11V99                 
           MOVE  WRK-9-11V99           TO  WRK-PAD18                    
           MOVE  WRK-PAD18-R           TO  WRK-LVALOR1                  
                                                                        
           MOVE  WRK-TABELA(IND-TABELA + 7:7)                           
                                       TO  WRK-AUX                      
           MOVE  WRK-AUX-11V99         TO  WRK-S9-11V99                 
           MOVE  WRK-9-11V99           TO  WRK-PAD18                    
           MOVE  WRK-PAD18-R           TO  WRK-LVALOR2                  
                                                                        
           MOVE  WRK-TABELA(IND-TABELA + 14:5)                          
                                       TO  WRK-AUX                      
           MOVE  WRK-AUX-9-9           TO  WRK-S9-9                     
           MOVE  WRK-9-9               TO  WRK-PAD12                    
           MOVE  WRK-PAD12-R           TO  WRK-LQTDE                    
                                                                        
           MOVE  WRK-TABELA(IND-TABELA + 14:5)                          
                                       TO  WRK-X-5-ALP                  
           ADD   WRK-9-9-NUM           TO  WRK-TQTDE                    
                                                                        
           WRITE FD-REG-TABULA       FROM  WRK-LTABU                    
                                                                        
           MOVE  WRK-GRAVACAO          TO  WRK-OPERACAO                 
                                                                        
           PERFORM 9040-TESTAR-FS-TABULA THRU 9040-99-FIM               
      *                                                                 
           ADD  19                     TO  IND-TABELA                   
                                                                        
           IF  WRK-TABELA(IND-TABELA:1) NOT EQUAL '*'                   
               GO TO 0150-LISTABU                                       
           END-IF                                                       
      *                                                                 
           MOVE 'TOTAL DE REGISTROS -' TO  WRK-LTABU(2:79)              
CPMCAC     MOVE  WRK-TQTDE             TO  WRK-S9-9                     
CPMCAC     MOVE  WRK-9-9               TO  WRK-PAD12                    
           MOVE  WRK-PAD12-R           TO  WRK-LQTDE                    
                                                                        
           WRITE FD-REG-TABULA       FROM  WRK-LTABU                    
                                                                        
           MOVE  WRK-GRAVACAO          TO  WRK-OPERACAO                 
                                                                        
           PERFORM 9040-TESTAR-FS-TABULA THRU 9040-99-FIM               
      *                                                                 
           CLOSE ARQAVISO                                               
                 RELATO                                                 
                 TABULA                                                 
                                                                        
           MOVE  WRK-FECHAMENTO        TO  WRK-OPERACAO                 
                                                                        
           PERFORM 9020-TESTAR-FS-ARQAVISO THRU  9020-99-FIM            
           PERFORM 9030-TESTAR-FS-RELATO   THRU  9030-99-FIM            
           PERFORM 9040-TESTAR-FS-TABULA   THRU  9040-99-FIM            
      *                                                                 
           GOBACK.                                                      
      *                                                                 
      *----------------------------------------------------------------*
       9000-TESTAR-FILE-STATUS.                                         
      *----------------------------------------------------------------*
                                                                        
           PERFORM 9010-TESTAR-FS-ARQDATAS THRU  9010-99-FIM            
           PERFORM 9020-TESTAR-FS-ARQAVISO THRU  9020-99-FIM            
           PERFORM 9030-TESTAR-FS-RELATO   THRU  9030-99-FIM            
           PERFORM 9040-TESTAR-FS-TABULA   THRU  9040-99-FIM.           
      *                                                                 
      *----------------------------------------------------------------*
       9000-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
      *                                                                 
      *----------------------------------------------------------------*
       9010-TESTAR-FS-ARQDATAS.                                         
      *----------------------------------------------------------------*
                                                                        
           IF (WRK-FS-ARQDATAS  NOT EQUAL  '00')                        
               MOVE 'ARQDATAS'         TO  WRK-NOME-ARQ                 
               MOVE WRK-FS-ARQDATAS    TO  WRK-FILE-STATUS              
               MOVE WRK-ERRO-BRAD7100  TO  ERR-TEXTO                    
               PERFORM 9999-PROCESSAR-ROTINA-ERRO                       
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       9010-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       9020-TESTAR-FS-ARQAVISO.                                         
      *----------------------------------------------------------------*
                                                                        
           IF (WRK-FS-ARQAVISO  NOT EQUAL  '00')                        
               MOVE 'ARQAVISO'         TO  WRK-NOME-ARQ                 
               MOVE WRK-FS-ARQAVISO    TO  WRK-FILE-STATUS              
               MOVE WRK-ERRO-BRAD7100  TO  ERR-TEXTO                    
               PERFORM 9999-PROCESSAR-ROTINA-ERRO                       
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       9020-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       9030-TESTAR-FS-RELATO.                                           
      *----------------------------------------------------------------*
                                                                        
           IF (WRK-FS-RELATO    NOT EQUAL  '00')                        
               MOVE 'RELATO'           TO  WRK-NOME-ARQ                 
               MOVE WRK-FS-RELATO      TO  WRK-FILE-STATUS              
               MOVE WRK-ERRO-BRAD7100  TO  ERR-TEXTO                    
               PERFORM 9999-PROCESSAR-ROTINA-ERRO                       
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       9030-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       9040-TESTAR-FS-TABULA.                                           
      *----------------------------------------------------------------*
                                                                        
           IF (WRK-FS-TABULA    NOT EQUAL  '00')                        
               MOVE 'TABULA'           TO  WRK-NOME-ARQ                 
               MOVE WRK-FS-TABULA      TO  WRK-FILE-STATUS              
               MOVE WRK-ERRO-BRAD7100  TO  ERR-TEXTO                    
               PERFORM 9999-PROCESSAR-ROTINA-ERRO                       
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       9040-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO.                                      
      *----------------------------------------------------------------*
                                                                        
           MOVE 'CLLPG647'             TO  ERR-PGM.                     
           MOVE 'APL'                  TO  ERR-TIPO-ACESSO              
                                                                        
           CALL 'BRAD7100'          USING  WRK-BATCH                    
                                           ERRO-AREA.                   
                                                                        
           GOBACK.                                                      
      *                                                                 
      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*

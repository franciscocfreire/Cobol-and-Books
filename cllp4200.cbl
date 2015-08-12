       PROCESS ARITH(EXTEND)                                            
      *================================================================*
       IDENTIFICATION                  DIVISION.                        
      *================================================================*
                                                                        
       PROGRAM-ID.                     CLLP4200.                        
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
      *    DATA DE CONVERSAO: NOV/2012                                 *
      *    OBJETIVO:'*CLLP4200* CRIA ARQUIVO DE BAIXAS REF. A CONSULTA *
      *               DE SALDO EM C/C'                                 *
      *----------------------------------------------------------------*
                                                                        
      *                         ____________                            
      ************************ *  CLLP4200  * **************************
      *                         ------------                           *
      *             U L T I M A     A L T E R A C A O                  *
      *             ===========     =================                  *
      *                                                                *
      *      PROGRAMADOR.......... MARCOS NOGUEIRA  - CPM              *
      *      ANALISTA............. VIVIAN         - GP. 91             *
      *      DATA................. SETEMBRO / 1996.                    *
      *                                                                *
      *----------------------------------------------------------------*
      *                                                                *
      *      OBJETIVO............. ATUALIZAR CAMPO POS. 569 COM 'N'.   *
      *                                                                *
      ******************************************************************
                                                                        
      *================================================================*
       ENVIRONMENT                     DIVISION.                        
      *================================================================*
                                                                        
       CONFIGURATION                   SECTION.                         
                                                                        
       SPECIAL-NAMES.                                                   
                                                                        
           DECIMAL-POINT               IS COMMA                         
           C01                         IS CANAL1                        
           CONSOLE                     IS OPERADOR.                     
                                                                        
      *----------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.                         
      *----------------------------------------------------------------*
                                                                        
       FILE-CONTROL.                                                    
                                                                        
           SELECT ARQDATA ASSIGN       TO UT-S-ARQDATA                  
                  FILE STATUS          IS WRK-FS-ARQDATA.               
                                                                        
           SELECT BXMORA ASSIGN        TO UT-S-BXMORA                   
                  FILE STATUS          IS WRK-FS-BXMORA.                
                                                                        
           SELECT SUBCPEN ASSIGN       TO UT-S-SUBCPEN                  
                  FILE STATUS          IS WRK-FS-SUBCPEN.               
                                                                        
           SELECT LPCLIN ASSIGN        TO UT-S-LPCLIN                   
                  FILE STATUS          IS WRK-FS-LPCLIN.                
                                                                        
           SELECT LPCLOUT ASSIGN       TO UT-S-LPCLOUT                  
                  FILE STATUS          IS WRK-FS-LPCLOUT.               
                                                                        
           SELECT BAIXALP ASSIGN       TO UT-S-BAIXALP                  
                  FILE STATUS          IS WRK-FS-BAIXALP.               
                                                                        
           SELECT RELATO ASSIGN        TO UT-S-RELATO                   
                  FILE STATUS          IS WRK-FS-RELATO.                
                                                                        
           SELECT ARQSORT ASSIGN       TO UT-S-ARQSORT.                 
                                                                        
      *================================================================*
       DATA                            DIVISION.                        
      *================================================================*
                                                                        
      *----------------------------------------------------------------*
      *        D E F I N I C A O   D A S   D C B ' S                   *
      *----------------------------------------------------------------*
                                                                        
       FILE                            SECTION.                         
                                                                        
      *----------------------------------------------------------------*
      *    INPUT : ARQUIVO ARQDATA                                     *
      *            ORG.           -    LRECL = 060                     *
      *----------------------------------------------------------------*
                                                                        
       FD  ARQDATA                                                      
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  FD-REG-ARQDATA              PIC X(060).                      
                                                                        
      *----------------------------------------------------------------*
      *    INPUT : ARQUIVO BXMORA                                      *
      *            ORG.           -    LRECL = 264                     *
      *----------------------------------------------------------------*
                                                                        
       FD  BXMORA                                                       
           RECORDING MODE IS V                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  FD-REG-BXMORA               PIC X(264).                      
                                                                        
      *----------------------------------------------------------------*
      *    INPUT : ARQUIVO SUBCPEN                                     *
      *            ORG.           -    LRECL = 262                     *
      *----------------------------------------------------------------*
                                                                        
       FD  SUBCPEN                                                      
           RECORDING MODE IS V                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  FD-REG-SUBCPEN              PIC X(262).                      
                                                                        
      *----------------------------------------------------------------*
      *    INPUT : ARQUIVO LPCLIN                                      *
      *            ORG.           -    LRECL = 131                     *
      *----------------------------------------------------------------*
                                                                        
       FD  LPCLIN                                                       
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  FD-REG-LPCLIN               PIC X(131).                      
                                                                        
      *----------------------------------------------------------------*
      *    OUTPUT : ARQUIVO LPCLOUT                                    *
      *             ORG.           -   LRECL = 131                     *
      *----------------------------------------------------------------*
                                                                        
       FD  LPCLOUT                                                      
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  FD-REG-LPCLOUT              PIC X(131).                      
                                                                        
      *----------------------------------------------------------------*
      *    OUTPUT : ARQUIVO BAIXALP                                    *
      *             ORG.           -   LRECL = 575                     *
      *----------------------------------------------------------------*
                                                                        
       FD  BAIXALP                                                      
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  FD-REG-BAIXALP              PIC X(575).                      
                                                                        
      *----------------------------------------------------------------*
      *    OUTPUT : ARQUIVO RELATO                                     *
      *             ORG.           -   LRECL = 150                     *
      *----------------------------------------------------------------*
                                                                        
       FD  RELATO                                                       
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  FD-REG-RELATO               PIC X(150).                      
                                                                        
      *----------------------------------------------------------------*
       SD  ARQSORT.                                                     
                                                                        
       01  SD-ARQSORT.                                                  
           05 SD-STAGEN                PIC S9(005) COMP-3  VALUE ZEROS. 
           05 SD-STNROCON              PIC S9(007) COMP-3  VALUE ZEROS. 
           05 SD-STDIGCON              PIC  X(001)         VALUE SPACES.
           05 SD-STCART                PIC S9(003) COMP-3  VALUE ZEROS. 
           05 FILLER                   PIC  X(007).                     
                                                                        
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
                                                                        
       77  WRK-FS-ARQDATA              PIC  X(002)         VALUE SPACES.
       77  WRK-FS-BXMORA               PIC  X(002)         VALUE SPACES.
       77  WRK-FS-SUBCPEN              PIC  X(002)         VALUE SPACES.
       77  WRK-FS-LPCLIN               PIC  X(002)         VALUE SPACES.
       77  WRK-FS-LPCLOUT              PIC  X(002)         VALUE SPACES.
       77  WRK-FS-BAIXALP              PIC  X(002)         VALUE SPACES.
       77  WRK-FS-RELATO               PIC  X(002)         VALUE SPACES.
                                                                        
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
      *        DEFINICOES DAS AREAS DE ENTRADA E SAIDA                 *
      *----------------------------------------------------------------*
                                                                        
       01  WRK-REGLPCL.                                                 
           05 WRK-INNROCON             PIC S9(007) COMP-3  VALUE ZEROS. 
CPMCAC     05 WRK-INNROCON-R           REDEFINES           WRK-INNROCON 
CPMCAC                                 PIC  X(004).                     
CPMCAC     05 WRK-INNROCON-RR          REDEFINES           WRK-INNROCON 
CPMCAC                                 PIC  9(007) COMP-3.              
           05 WRK-INDIGCON             PIC  X(001)         VALUE SPACES.
           05 WRK-INCART               PIC S9(003) COMP-3  VALUE ZEROS. 
CPMCAC     05 WRK-INCART-R             REDEFINES           WRK-INCART   
CPMCAC                                 PIC  X(002).                     
CPMCAC     05 WRK-INCART-RR            REDEFINES           WRK-INCART   
CPMCAC                                 PIC  9(003) COMP-3.              
           05 WRK-INEMPR               PIC S9(005) COMP-3  VALUE ZEROS. 
CPMCAC     05 WRK-INEMPR-R             REDEFINES           WRK-INEMPR   
CPMCAC                                 PIC  X(003).                     
CPMCAC     05 WRK-INEMPR-RR            REDEFINES           WRK-INEMPR   
CPMCAC                                 PIC  9(005) COMP-3.              
           05 WRK-INAGEN               PIC S9(005) COMP-3  VALUE ZEROS. 
CPMCAC     05 WRK-INAGEN-R             REDEFINES           WRK-INAGEN   
CPMCAC                                 PIC  X(003).                     
CPMCAC     05 WRK-INAGEN-RR            REDEFINES           WRK-INAGEN   
CPMCAC                                 PIC  9(005) COMP-3.              
           05 WRK-INNRO                PIC S9(015) COMP-3  VALUE ZEROS. 
CPMCAC     05 WRK-INNRO-RR             REDEFINES           WRK-INNRO    
CPMCAC                                 PIC  9(015) COMP-3.              
           05 WRK-INDIG                PIC  X(001)         VALUE SPACES.
           05 WRK-INIDENT              PIC  X(002)         VALUE SPACES.
           05 WRK-INNROCC              PIC S9(007) COMP-3  VALUE ZEROS. 
           05 WRK-INDIGCC              PIC  X(001)         VALUE SPACES.
           05 WRK-INVCTO               PIC  9(009) COMP-3  VALUE ZEROS. 
           05 WRK-INTPPND              PIC S9(005) COMP-3  VALUE ZEROS. 
           05 WRK-INCARTA              PIC  X(003)         VALUE SPACES.
           05 WRK-INDTCLP              PIC S9(009) COMP-3  VALUE ZEROS. 
           05 WRK-INCODULT             PIC  X(002)         VALUE SPACES.
           05 WRK-INDTENT              PIC S9(009) COMP-3  VALUE ZEROS. 
           05 WRK-INVRCONT             PIC S9(015) COMP-3  VALUE ZEROS. 
CPMCAC     05 WRK-INVRCONT-R           REDEFINES           WRK-INVRCONT 
CPMCAC                                 PIC  X(008).                     
           05 WRK-INVRVEN              PIC S9(015) COMP-3  VALUE ZEROS. 
           05 WRK-INVRCOBR             PIC S9(015) COMP-3  VALUE ZEROS. 
CPMCAC     05 FILLER                   REDEFINES           WRK-INVRCOBR.
CPMCAC        10 FILLER                PIC  X(001).                     
CPMCAC        10 WRK-INVRCOBR-R        PIC S9(013) COMP-3.              
           05 WRK-INVRVIN              PIC S9(015) COMP-3  VALUE ZEROS. 
           05 WRK-INVRCONS             PIC S9(015) COMP-3  VALUE ZEROS. 
           05 WRK-INPRELP              PIC  X(001)         VALUE SPACES.
           05 WRK-INCONTR              PIC  X(001)         VALUE SPACES.
           05 WRK-INVRIOF              PIC S9(017) COMP-3  VALUE ZEROS. 
CPMCAC     05 WRK-INVRIOF-R            REDEFINES           WRK-INVRIOF  
CPMCAC                                 PIC  X(009).                     
           05 WRK-INALIQR              PIC S9(009) COMP-3  VALUE ZEROS. 
           05 WRK-INALIQT              PIC S9(009) COMP-3  VALUE ZEROS. 
           05 WRK-INALIQC              PIC S9(009) COMP-3  VALUE ZEROS. 
      *    VALOR BASE DE IOF                                            
           05 WRK-INVBAIOF             PIC S9(017) COMP-3  VALUE ZEROS. 
CPMCAC     05 WRK-INVBAIOF-R           REDEFINES           WRK-INVBAIOF 
CPMCAC                                 PIC  X(009).                     
      *    CONTRATO ORIGINAL                                            
           05 WRK-INCONTOR             PIC S9(007) COMP-3  VALUE ZEROS. 
                                                                        
      *----------------------------------------------------------------*
       01  WRK-WKDATA.                                                  
      *    **** DATA COM BARRAS                                         
           05 WRK-DATACBAR             PIC  X(010)         VALUE SPACES.
      *    **** DATA SEM BARRAS                                         
           05 WRK-DATASBAR             PIC  X(008)         VALUE SPACES.
           05 FILLER                   PIC  X(042)         VALUE SPACES.
                                                                        
      *----------------------------------------------------------------*
       01  WRK-REGBXA.                                                  
           05 FILLER                   PIC  X(011)         VALUE SPACES.
           05 WRK-BXAGEN               PIC S9(005) COMP-3  VALUE ZEROS. 
CPMCAC     05 WRK-BXAGEN-R             REDEFINES           WRK-BXAGEN   
CPMCAC                                 PIC  X(003).                     
           05 FILLER                   PIC  X(008)         VALUE SPACES.
           05 WRK-BXCART               PIC S9(005) COMP-3  VALUE ZEROS. 
CPMCAC     05 WRK-BXCART-R             REDEFINES           WRK-BXCART   
CPMCAC                                 PIC  X(003).                     
           05 FILLER                   PIC S9(003) COMP-3  VALUE ZEROS. 
           05 WRK-BXCONTR              PIC S9(007) COMP-3  VALUE ZEROS. 
CPMCAC     05 WRK-BXCONTR-R            REDEFINES           WRK-BXCONTR  
CPMCAC                                 PIC  X(004).                     
           05 WRK-BXDIG                PIC  X(001)         VALUE SPACES.
           05 FILLER                   PIC  X(021)         VALUE SPACES.
      *    VR. DEBITADO PRINCIPAL + ENCARGOS                            
           05 WRK-BXVRDEB              PIC S9(013) COMP-3  VALUE ZEROS. 
CPMCAC     05 WRK-BXVRDEB-R            REDEFINES           WRK-BXVRDEB  
CPMCAC                                 PIC  X(007).                     
           05 WRK-BXEB                 PIC  X(001)         VALUE SPACES.
           05 FILLER                   PIC  X(001)         VALUE SPACES.
           05 WRK-BXIND                PIC  X(002)         VALUE SPACES.
           05 FILLER                   PIC  X(003)         VALUE SPACES.
      *    VR. DEBITADO PRINCIPAL                                       
           05 WRK-BXVALOR              PIC S9(013) COMP-3  VALUE ZEROS. 
           05 FILLER                   PIC  X(190)         VALUE SPACES.
                                                                        
      *----------------------------------------------------------------*
       01  WRK-REGSUBC.                                                 
           05 WRK-SUBCBXA.                                              
              10 FILLER                PIC  X(022)         VALUE SPACES.
              10 WRK-SUBCBXA-SUBC      PIC S9(005) COMP-3  VALUE ZEROS. 
              10 FILLER                PIC  X(237)         VALUE SPACES.
                                                                        
      *----------------------------------------------------------------*
       01  WRK-REGBXALP.                                                
           05 WRK-LPFIXO               PIC  X(001)         VALUE SPACES.
           05 WRK-LPEMPR               PIC  X(005)         VALUE SPACES.
CPMCAC     05 WRK-LPEMPR-R             REDEFINES           WRK-LPEMPR   
CPMCAC                                 PIC  9(005).                     
           05 WRK-LPAGEN               PIC  X(005)         VALUE SPACES.
CPMCAC     05 WRK-LPAGEN-R             REDEFINES           WRK-LPAGEN   
CPMCAC                                 PIC  9(005).                     
           05 WRK-LPNRO                PIC  9(015)         VALUE ZEROS. 
           05 WRK-LPDIG                PIC  X(001)         VALUE SPACES.
           05 WRK-LPRAZCL              PIC  X(005)         VALUE        
           '00000'.                                                     
           05 WRK-LPCTACL              PIC  X(007)         VALUE        
           '0000000'.                                                   
           05 WRK-LPRAZOR              PIC  X(005)         VALUE        
           '00000'.                                                     
           05 WRK-LPCTAOR              PIC  X(007)         VALUE        
           '0000000'.                                                   
           05 WRK-LPNRCC               PIC  X(007)         VALUE        
           '0000000'.                                                   
           05 WRK-LPDTVCTO             PIC  9(008)         VALUE ZEROS. 
           05 WRK-LPDTECL              PIC  X(008)         VALUE        
           '00000000'.                                                  
           05 WRK-LPDTCL               PIC  X(008)         VALUE SPACES.
           05 WRK-LPDTIOP              PIC  X(008)         VALUE        
           '00000000'.                                                  
           05 WRK-LPDTPRO              PIC  X(008)         VALUE        
           '00000000'.                                                  
           05 WRK-LPDTELP              PIC  X(008)         VALUE        
           '00000000'.                                                  
           05 WRK-LPDTPL               PIC  X(008)         VALUE SPACES.
           05 WRK-LPDTMVTO             PIC  X(008)         VALUE        
           '00000000'.                                                  
           05 WRK-LPDTTRAS             PIC  X(008)         VALUE        
           '00000000'.                                                  
           05 WRK-LPSIGLA              PIC  X(004)         VALUE SPACES.
           05 WRK-LPCARCL              PIC  X(003)         VALUE        
           '000'.                                                       
           05 WRK-LPNRCONT             PIC  X(007)         VALUE        
           '0000000'.                                                   
           05 WRK-LPVROPER             PIC  X(015)         VALUE        
           '000000000000000'.                                           
           05 WRK-LPVRCONT             PIC  X(015)         VALUE        
           '000000000000000'.                                           
           05 WRK-LPMOEDA              PIC  X(002)         VALUE SPACES.
           05 WRK-LPLOCAL              PIC  X(002)         VALUE SPACES.
           05 WRK-LPIDCON              PIC  X(001)         VALUE '0'.   
           05 WRK-LPTXPUN              PIC  X(001)         VALUE '0'.   
           05 WRK-LPIDEN               PIC  X(002)         VALUE SPACES.
           05 WRK-LPNOMRD              PIC  X(040)         VALUE SPACES.
           05 WRK-LPCPFDEV             PIC  X(009)         VALUE        
           '000000000'.                                                 
           05 WRK-LPFILDEV             PIC  X(004)         VALUE        
           '0000'.                                                      
           05 WRK-LPCTRDEV             PIC  X(002)         VALUE        
           '00'.                                                        
           05 WRK-LPCOMPSA             PIC  X(001)         VALUE '0'.   
           05 WRK-LPNONAV1             PIC  X(040)         VALUE SPACES.
           05 WRK-LPCPFAV1             PIC  X(009)         VALUE        
           '000000000'.                                                 
           05 WRK-LPFILAV1             PIC  X(004)         VALUE        
           '0000'.                                                      
           05 WRK-LPCTRAV1             PIC  X(002)         VALUE        
           '00'.                                                        
           05 WRK-LPNONAV2             PIC  X(040)         VALUE SPACES.
           05 WRK-LPCPFAV2             PIC  X(009)         VALUE        
           '000000000'.                                                 
           05 WRK-LPFILAV2             PIC  X(004)         VALUE        
           '0000'.                                                      
           05 WRK-LPCTRAV2             PIC  X(002)         VALUE        
           '00'.                                                        
           05 WRK-LPVRBASE             PIC  X(015)         VALUE        
           '000000000000000'.                                           
CPMCAC     05 WRK-LPVRBASE-R           REDEFINES           WRK-LPVRBASE 
CPMCAC                                 PIC  9(015).                     
           05 WRK-LPDTEXP              PIC  X(008)         VALUE        
           '00000000'.                                                  
           05 WRK-LPSUBSTB             PIC  X(001)         VALUE SPACES.
           05 WRK-LPCPFADV             PIC  X(009)         VALUE        
           '000000000'.                                                 
           05 WRK-LPCTRADV             PIC  X(002)         VALUE        
           '00'.                                                        
           05 WRK-LPCAROR              PIC  X(003)         VALUE        
           '000'.                                                       
           05 WRK-LPTIPGAR             PIC  X(002)         VALUE        
           '00'.                                                        
           05 WRK-LPTPEND              PIC  X(004)         VALUE        
           '0000'.                                                      
           05 WRK-LPMARCA              PIC  X(001)         VALUE SPACES.
           05 WRK-LPCODGER             PIC  X(006)         VALUE        
           '000000'.                                                    
           05 WRK-LPCOMPBX             PIC  X(001)         VALUE SPACES.
      *    VALOR BASE IOF                                               
           05 WRK-LPBASIOF             PIC  X(015)         VALUE        
                                                           LOW-VALUES.  
           05 WRK-LPGARANT             PIC  X(032)         VALUE SPACES.
           05 WRK-LPTIPBXA             PIC  X(001)         VALUE SPACES.
           05 WRK-LPVRVEN              PIC  X(015)         VALUE SPACES.
CPMCAC     05 WRK-LPVRVEN-R            REDEFINES           WRK-LPVRVEN  
CPMCAC                                 PIC  9(015).                     
           05 WRK-LPVRVIN              PIC  X(015)         VALUE        
           '000000000000000'.                                           
CPMCAC     05 WRK-LPVRVIN-R            REDEFINES           WRK-LPVRVIN  
CPMCAC                                 PIC  9(015).                     
           05 WRK-LPVRCON              PIC  X(015)         VALUE SPACES.
CPMCAC     05 WRK-LPVRCON-R            REDEFINES           WRK-LPVRCON  
CPMCAC                                 PIC  9(015).                     
           05 WRK-LPVRINI              PIC  X(015)         VALUE        
           '000000000000000'.                                           
CPMCAC     05 WRK-LPVRINI-R            REDEFINES           WRK-LPVRINI  
CPMCAC                                 PIC  9(015).                     
           05 WRK-LPVRIOF              PIC  X(015)         VALUE        
           '000000000000000'.                                           
CPMCAC     05 WRK-LPVRIOF-R            REDEFINES           WRK-LPVRIOF  
CPMCAC                                 PIC  9(015).                     
           05 WRK-LPVRDEB              PIC  X(015)         VALUE SPACES.
CPMCAC     05 WRK-LPVRDEB-R            REDEFINES           WRK-LPVRDEB  
CPMCAC                                 PIC  9(015).                     
           05 WRK-LPVRCOBR             PIC  X(015)         VALUE SPACES.
CPMCAC     05 WRK-LPVRCOBR-R           REDEFINES           WRK-LPVRCOBR 
CPMCAC                                 PIC  9(015).                     
           05 WRK-LPCDUOCO             PIC  X(002)         VALUE        
           '00'.                                                        
           05 WRK-LPBQTRAN             PIC  X(001)         VALUE 'S'.   
           05 WRK-LPDTPGTO             PIC  X(008)         VALUE SPACES.
      *    ACERTO = 3 (FIXO)                                            
           05 WRK-LPACERTO             PIC  X(001)         VALUE        
                                                           LOW-VALUES.  
      *    ORIGEM = 'DA'                                                
           05 WRK-LPACDADE             PIC  X(002)         VALUE        
                                                           LOW-VALUES.  
           05 FILLER                   PIC  X(001)         VALUE        
                                                           LOW-VALUES.  
      *    CAMPO FIXO 'N'                                               
           05 WRK-LPFIXON              PIC  X(001)         VALUE SPACES.
           05 FILLER                   PIC  X(002)         VALUE        
                                                           LOW-VALUES.  
           05 WRK-LPSOL                PIC  X(004)         VALUE        
           '0000'.                                                      
                                                                        
      *----------------------------------------------------------------*
       01  WRK-CHVLPCL.                                                 
           05 WRK-CHVAGEN              PIC  X(003)         VALUE        
           X'00000F'.                                                   
           05 WRK-CHVNROCO             PIC  X(004)         VALUE        
           X'0000000F'.                                                 
           05 WRK-CHVDIG               PIC  X(001)         VALUE SPACES.
BRQ=A      05 WRK-CHVCART              PIC  X(003)         VALUE
BRQ=A      X'00000F'.
                                                                        
      *----------------------------------------------------------------*
       01  WRK-AREASORT.                                                
           05 WRK-STCHAVE.                                              
              10 WRK-STAGEN            PIC S9(005) COMP-3  VALUE ZEROS. 
CPMCAC        10 WRK-STAGEN-R          REDEFINES           WRK-STAGEN   
CPMCAC                                 PIC  X(003).                     
              10 WRK-STNROCON          PIC S9(007) COMP-3  VALUE ZEROS. 
CPMCAC        10 WRK-STNROCON-R        REDEFINES           WRK-STNROCON 
CPMCAC                                 PIC  X(004).                     
CPMCAC        10 WRK-STNROCON-RR       REDEFINES           WRK-STNROCON 
CPMCAC                                 PIC  9(007) COMP-3.              
              10 WRK-STDIGCON          PIC  X(001)         VALUE SPACES.
              10 WRK-STCART            PIC S9(003) COMP-3  VALUE ZEROS. 
CPMCAC        10 WRK-STCART-R          REDEFINES           WRK-STCART   
CPMCAC                                 PIC  X(002).                     
CPMCAC        10 WRK-STCART-RR         REDEFINES           WRK-STCART   
CPMCAC                                 PIC  9(003) COMP-3.              
           05  WRK-STVRDEB             PIC S9(013) COMP-3  VALUE ZEROS. 
CPMCAC     05  WRK-STVRDEB-R           REDEFINES           WRK-STVRDEB  
CPMCAC                                 PIC  X(007).                     
                                                                        
      *----------------------------------------------------------------*
      *        DEFINICOES DE ACUMULADORES                              *
      *----------------------------------------------------------------*
                                                                        
       01  WRK-FOL                     PIC S9(003) COMP-3  VALUE 55.    
       01  WRK-WFOL                    PIC S9(005) COMP-3  VALUE ZEROS. 
       01  WRK-AUXTPBX                 PIC  X(007)         VALUE '0'.   
       01  WRK-WKDATMOV                PIC  X(008)         VALUE SPACES.
       01  WRK-WSUBCPEN                PIC  X(001)         VALUE SPACES.
       01  WRK-WTEMSORT                PIC  X(001)         VALUE SPACES.
       01  WRK-WTEMLPIN                PIC  X(001)         VALUE SPACES.
      *                                                                 
       01  WRK-WKDTAUX                 PIC  X(008)         VALUE SPACES.
CPMCAC 01  WRK-WKDTAUX-R               REDEFINES           WRK-WKDTAUX  
CPMCAC                                 PIC  9(008).                     
                                                                        
       01  WRK-ENCARGOS                PIC S9(013) COMP-3  VALUE ZEROS. 
       01  WRK-WTOTCON                 PIC S9(013) COMP-3  VALUE ZEROS. 
       01  WRK-WTOTVEN                 PIC S9(013) COMP-3  VALUE ZEROS. 
       01  WRK-WTOTCONS                PIC S9(013) COMP-3  VALUE ZEROS. 
                                                                        
       01  WRK-WVRCON                  PIC S9(013) COMP-3  VALUE ZEROS. 
CPMCAC 01  WRK-WVRCON-R                REDEFINES           WRK-WVRCON   
CPMCAC                                 PIC  X(007).                     
                                                                        
       01  WRK-WVRIOF                  PIC S9(017) COMP-3  VALUE ZEROS. 
CPMCAC 01  WRK-WVRIOF-R                REDEFINES           WRK-WVRIOF   
CPMCAC                                 PIC  X(009).                     
                                                                        
       01  WRK-WVBASIOF                PIC S9(017) COMP-3  VALUE ZEROS. 
CPMCAC 01  WRK-WVBASIOF-R              REDEFINES           WRK-WVBASIOF 
CPMCAC                                 PIC  X(009).                     
                                                                        
       01  WRK-WVRVEN                  PIC S9(013) COMP-3  VALUE ZEROS. 
       01  WRK-WVRCOBR                 PIC S9(013) COMP-3  VALUE ZEROS. 
       01  WRK-WVRCORR                 PIC S9(013) COMP-3  VALUE ZEROS. 
       01  WRK-WVRVIN                  PIC S9(015) COMP-3  VALUE ZEROS. 
       01  WRK-WVVENAUX                PIC S9(013) COMP-3  VALUE ZEROS. 
                                                                        
       01  WRK-VRAUX                   PIC S9(017) COMP-3  VALUE ZEROS.
CPMCAC 01  FILLER                      REDEFINES           WRK-VRAUX.   
CPMCAC     05 FILLER                   PIC  X(008).                     
CPMCAC     05 WRK-VRAUX-RR             PIC S9(013) COMP-3.              
                                                                        
      *----------------------------------------------------------------*
       01  WRK-AUXPRELP                PIC  X(001)         VALUE SPACES.
                                                                        
       01  WRK-PADRAO                  PIC BZZ.ZZZ.ZZZ.ZZ9,99           
                                                           VALUE SPACES.
CPMCAC 01  WRK-PADRAO-R                REDEFINES           WRK-PADRAO   
CPMCAC                                 PIC  X(018).                     
                                                                        
       01  WRK-PADRAO1                 PIC BZZZZZZZZ.ZZ9,99             
                                                           VALUE SPACES.
CPMCAC 01  WRK-PADRAO1-R               REDEFINES           WRK-PADRAO1  
CPMCAC                                 PIC  X(016).                     
                                                                        
       01  WRK-PAD10                   PIC BZ.ZZZ.ZZ9      VALUE SPACES.
CPMCAC 01  WRK-PAD10-R                 REDEFINES           WRK-PAD10    
CPMCAC                                 PIC  X(010).                     
                                                                        
       01  WRK-WTOTQTDE                PIC S9(009) COMP-3  VALUE ZEROS. 
                                                                        
      *----------------------------------------------------------------*
       01  WRK-DATAHORA.                                                
           05 WRK-JULIANA              PIC S9(005) COMP-3  VALUE ZEROS. 
           05 WRK-AAMMDD2              PIC S9(007) COMP-3  VALUE ZEROS. 
           05 WRK-AAAAMMDD             PIC S9(009) COMP-3  VALUE ZEROS. 
           05 WRK-HHMMSS               PIC S9(007) COMP-3  VALUE ZEROS. 
           05 WRK-HHMMSSMM             PIC S9(013) COMP-3  VALUE ZEROS. 
           05 WRK-TIMESTA              PIC  X(020)         VALUE SPACES.
                                                                        
      *----------------------------------------------------------------*
       01  WRK-DATAZ.                                                   
           05 WRK-ANOZ                 PIC  X(004)         VALUE SPACES.
           05 WRK-MESZ                 PIC  X(002)         VALUE SPACES.
           05 WRK-DIAZ                 PIC  X(002)         VALUE SPACES.
                                                                        
      *----------------------------------------------------------------*
      *     CAMPOS UTILIZADOS NO ACESSO AO MODULO   F E R I 1 0 1 0    *
      *----------------------------------------------------------------*
                                                                        
       01  WRK-BRAD1205                PIC  X(008)         VALUE        
           'BRAD1205'.                                                  
                                                                        
      *----------------------------------------------------------------*
      *ROTINAS DE DATAS                                                 
       01  WRK-ROTDATAS.                                                
      *    DATA ENVIADA AO MODULO                                       
           05 WRK-DTENVIA              PIC  9(009) COMP-3  VALUE ZEROS. 
      *    RECEBE STATUS RETORNO                                        
           05 WRK-RESULFUI             PIC  X(001)         VALUE SPACES.
           05 FILLER                   PIC  X(004)         VALUE SPACES.
      *    DATA COM BARRA                                               
           05 WRK-DTBARRA              PIC  X(010)         VALUE SPACES.
           05 FILLER                   PIC  X(046)         VALUE SPACES.
                                                                        
       01  WRK-WKMENS                  PIC  X(050)         VALUE SPACES.
                                                                        
      *----------------------------------------------------------------*
      *                   DIFINICOES  DO RELATORIO                     *
      *----------------------------------------------------------------*
                                                                        
       01  WRK-SAIDA.                                                   
           05 FILLER                   PIC  X(001)         VALUE X'11'. 
           05 FILLER                   PIC  X(149)         VALUE SPACES.
                                                                        
      *----------------------------------------------------------------*
       01  WRK-CANAL.                                                   
           05  FILLER                  PIC  X(001)         VALUE X'8B'. 
           05  WRK-CAB1.                                                
            10 FILLER                  PIC  X(001)         VALUE X'19'. 
            10 FILLER                  PIC  X(088)         VALUE        
           '*CLLP4200*               RELACAO DOS REGISTROS BAIXADOS ATRA
      -    'VES DA CONSULTA DE SALDO EM '.                              
            10 WRK-CABDATA             PIC  X(010)         VALUE SPACES.
            10 FILLER                  PIC  X(024)         VALUE SPACES.
            10 FILLER                  PIC  X(003)         VALUE 'FL.'. 
            10 WRK-CBFOL               PIC  X(005)         VALUE        
                                                           LOW-VALUES.  
CPMCAC      10 WRK-CBFOL-R             REDEFINES           WRK-CBFOL    
CPMCAC                                 PIC  9(005).                     
            10 FILLER                  PIC  X(019)         VALUE SPACES.
                                                                        
      *----------------------------------------------------------------*
       01  WRK-CAB2.                                                    
           05 FILLER                   PIC  X(001)         VALUE X'11'. 
           05 FILLER                   PIC  X(149)         VALUE        
           '     C H A V E   LPCL       T.PEND NM.SEQ. CART ID     C/C  
      -    'VENCIMENTO  VR. CONTABIL BX      ENC.VENCIDOS    ENC.VINCEN/
      -    'IOF  VR. DEBITADO C/C TIPO BX'.                             
                                                                        
      *----------------------------------------------------------------*
       01  WRK-CAB3.                                                    
           05 FILLER                   PIC  X(001)         VALUE X'11'. 
           05 FILLER                   PIC  X(079)         VALUE        
           '*CLLP4200*     REGISTROS DESPREZADOS NA MONTAGEM DA TABELA D
      -    'E PENDENCIAS    EM '.                                       
           05  WRK-LDATA.                                               
               10 WRK-LDIA             PIC  X(002)         VALUE SPACES.
               10 FILLER               PIC  X(001)         VALUE '/'.   
               10 WRK-LMES             PIC  X(002)         VALUE SPACES.
               10 FILLER               PIC  X(001)         VALUE '/'.   
               10 WRK-LANO             PIC  X(004)         VALUE SPACES.
           05  FILLER                  PIC  X(043)         VALUE SPACES.
CPMCAC     05  FILLER                  PIC  X(017)         VALUE SPACES.
                                                                        
      *----------------------------------------------------------------*
       01  WRK-TABACUMX.                                                
           05 FILLER                   PIC  X(003)         VALUE        
           X'04025F'.                                                   
           05 FILLER                   PIC  X(008)         VALUE        
           X'000000000000000F'.                                         
           05 FILLER                   PIC  X(008)         VALUE        
           X'000000000000000F'.                                         
           05 FILLER                   PIC  X(008)         VALUE        
           X'000000000000000F'.                                         
           05 FILLER                   PIC  X(004)         VALUE        
           X'0000000F'.                                                 
           05 FILLER                   PIC  X(003)         VALUE        
           X'04120F'.                                                   
           05 FILLER                   PIC  X(008)         VALUE        
           X'000000000000000F'.                                         
           05 FILLER                   PIC  X(008)         VALUE        
           X'000000000000000F'.                                         
           05 FILLER                   PIC  X(008)         VALUE        
           X'000000000000000F'.                                         
           05 FILLER                   PIC  X(004)         VALUE        
           X'0000000F'.                                                 
           05 FILLER                   PIC  X(003)         VALUE        
           X'05150F'.                                                   
           05 FILLER                   PIC  X(008)         VALUE        
           X'000000000000000F'.                                         
           05 FILLER                   PIC  X(008)         VALUE        
           X'000000000000000F'.                                         
           05 FILLER                   PIC  X(008)         VALUE        
           X'000000000000000F'.                                         
           05 FILLER                   PIC  X(004)         VALUE        
           X'0000000F'.                                                 
           05 FILLER                   PIC  X(003)         VALUE        
           X'05240F'.                                                   
           05 FILLER                   PIC  X(008)         VALUE        
           X'000000000000000F'.                                         
           05 FILLER                   PIC  X(008)         VALUE        
           X'000000000000000F'.                                         
           05 FILLER                   PIC  X(008)         VALUE        
           X'000000000000000F'.                                         
           05 FILLER                   PIC  X(004)         VALUE        
           X'0000000F'.                                                 
           05 FILLER                   PIC  X(003)         VALUE        
           X'07000F'.                                                   
           05 FILLER                   PIC  X(008)         VALUE        
           X'000000000000000F'.                                         
           05 FILLER                   PIC  X(008)         VALUE        
           X'000000000000000F'.                                         
           05 FILLER                   PIC  X(008)         VALUE        
           X'000000000000000F'.                                         
           05 FILLER                   PIC  X(004)         VALUE        
           X'0000000F'.                                                 
           05 FILLER                   PIC  X(003)         VALUE        
           X'09999F'.                                                   
           05 FILLER                   PIC  X(008)         VALUE        
           X'000000000000000F'.                                         
           05 FILLER                   PIC  X(008)         VALUE        
           X'000000000000000F'.                                         
           05 FILLER                   PIC  X(008)         VALUE        
           X'000000000000000F'.                                         
           05 FILLER                   PIC  X(004)         VALUE        
           X'0000000F'.                                                 
                                                                        
CPMCAC*----------------------------------------------------------------*
CPMCAC 01  FILLER                      PIC  X(050)         VALUE        
CPMCAC     'AREA PARA O CLLP8400'.                                      
CPMCAC*----------------------------------------------------------------*
                                                                        
CPMCAC 01  WRK-CLLP8400                PIC  X(008)         VALUE        
CPMCAC     'CLLP8400'.                                                  
                                                                        
CPMCAC 01  WRK-CLLP8400-TAMANHO        PIC S9(004) COMP    VALUE ZEROS. 
CPMCAC 01  WRK-CLLP8400-OPERACAO       PIC  X(003)         VALUE SPACES.
                                                                        
CPMCAC*----------------------------------------------------------------*
CPMCAC 01  FILLER                      PIC  X(050)         VALUE        
CPMCAC     'AREA PARA A BRAD0450'.                                      
CPMCAC*----------------------------------------------------------------*
                                                                        
CPMCAC 01  WRK-ABEND                   PIC S9(004) COMP    VALUE ZEROS. 
CPMCAC 01  WRK-DUMP                    PIC  X(001)         VALUE SPACES.
                                                                        
CPMCAC*----------------------------------------------------------------*
CPMCAC 01  FILLER                      PIC  X(050)         VALUE        
CPMCAC     'AREA PARA VARIAVEIS AUXILIARES'.                            
CPMCAC*----------------------------------------------------------------*
                                                                        
CPMCAC 01  WRK-IND-R6                  PIC  9(005) COMP-3  VALUE ZEROS. 
CPMCAC 01  WRK-RESTO                   PIC S9(015) COMP-3  VALUE ZEROS. 
CPMCAC 01  WRK-FLAG                    PIC  X(001)         VALUE 'N'.   
CPMCAC 01  WRK-FLAG1                   PIC  X(001)         VALUE 'N'.   
CPMCAC 01  WRK-FLAG2                   PIC  X(001)         VALUE 'N'.   
                                                                        
CPMCAC 01  WRK-03                      PIC S9(003)         VALUE ZEROS. 
CPMCAC 01  WRK-03-RED                  REDEFINES           WRK-03       
CPMCAC                                 PIC  9(003).                     
                                                                        
CPMCAC 01  WRK-03N                     PIC  9(003)         VALUE ZEROS. 
CPMCAC 01  WRK-03N-X                   REDEFINES           WRK-03N      
CPMCAC                                 PIC  X(003).                     
                                                                        
CPMCAC 01  WRK-05                      PIC S9(005)         VALUE ZEROS. 
CPMCAC 01  WRK-05-RED                  REDEFINES           WRK-05       
CPMCAC                                 PIC  9(005).                     
                                                                        
CPMCAC 01  WRK-05-COMP                 PIC S9(005) COMP-3  VALUE ZEROS. 
CPMCAC 01  WRK-05-COMP-R               REDEFINES           WRK-05-COMP  
CPMCAC                                 PIC  X(003).                     
                                                                        
CPMCAC 01  WRK-05-N-COMP               PIC  9(005) COMP-3  VALUE ZEROS. 
CPMCAC 01  WRK-05-COMP-X               REDEFINES           WRK-05-N-COMP
CPMCAC                                 PIC  X(003).                     
                                                                        
CPMCAC 01  WRK-05N                     PIC  9(005)         VALUE ZEROS. 
CPMCAC 01  WRK-05N-X                   REDEFINES           WRK-05N      
CPMCAC                                 PIC  X(005).                     
                                                                        
CPMCAC 01  WRK-07                      PIC S9(007)         VALUE ZEROS. 
CPMCAC 01  WRK-07-RED                  REDEFINES           WRK-07       
CPMCAC                                 PIC  9(007).                     
                                                                        
CPMCAC 01  WRK-07N                     PIC  9(007)         VALUE ZEROS. 
CPMCAC 01  WRK-07N-X                   REDEFINES           WRK-07N
CPMCAC                                 PIC  X(007).                     
                                                                        
CPMCAC 01  WRK-07-C2                   PIC  9(007) COMP-3  VALUE ZEROS. 
CPMCAC 01  WRK-07-X                    REDEFINES           WRK-07-C2    
CPMCAC                                 PIC  X(004).                     
                                                                        
CPMCAC 01  WRK-07COMP-N                PIC S9(009)         VALUE ZEROS. 
CPMCAC 01  FILLER                      REDEFINES           WRK-07COMP-N.
CPMCAC     05 FILLER                   PIC  9(002).                     
CPMCAC     05 WRK-07COMP-R             PIC  9(007).                     
                                                                        
CPMCAC 01  WRK-07-COMP                 PIC S9(007) COMP-3  VALUE ZEROS. 
CPMCAC 01  WRK-07-COMP-R               REDEFINES           WRK-07-COMP  
CPMCAC                                 PIC  X(004).                     
                                                                        
CPMCAC 01  WRK-9-CS                    PIC S9(009)         VALUE ZEROS. 
CPMCAC 01  WRK-9-CS-R                  REDEFINES           WRK-9-CS     
CPMCAC                                 PIC  9(009).                     
CPMCAC 01  WRK-9-X                     REDEFINES           WRK-9-CS-R   
CPMCAC                                 PIC  X(009).                     
                                                                        
CPMCAC 01  WRK-9-9-D                   PIC  9(009)         VALUE ZEROS. 
CPMCAC 01  FILLER                      REDEFINES           WRK-9-9-D.   
CPMCAC     05  FILLER                  PIC  9(001).                     
CPMCAC     05  WRK-9-8-D               PIC  9(008).                     
                                                                        
CPMCAC 01  WRK-PAD12                   PIC BZZZ/ZZ/ZZZZ    VALUE SPACES.
CPMCAC 01  WRK-PAD12-R                 REDEFINES           WRK-PAD12    
CPMCAC                                 PIC  X(012).                     
                                                                        
CPMCAC 01  WRK-13-CS                   PIC S9(013)         VALUE ZEROS. 
CPMCAC 01  WRK-13-CS-R                 REDEFINES           WRK-13-CS    
CPMCAC                                 PIC  9(013).                     
                                                                        
CPMCAC 01  WRK-13-C2                   PIC S9(013) COMP-3  VALUE ZEROS. 
CPMCAC 01  WRK-13-X                    REDEFINES           WRK-13-C2    
CPMCAC                                 PIC  X(007).                     
                                                                        
CPMCAC 01  WRK-013                     PIC S9(013)         VALUE ZEROS. 
CPMCAC 01  WRK-013-R                   REDEFINES           WRK-013      
CPMCAC                                 PIC  9(011)V99.                  
                                                                        
CPMCAC 01  WRK-15                      PIC S9(015)         VALUE ZEROS. 
CPMCAC 01  WRK-15-RED                  REDEFINES           WRK-15       
CPMCAC                                 PIC  9(015).                     
                                                                        
CPMCAC 01  WRK-015N                    PIC  9(015)         VALUE ZEROS. 
CPMCAC 01  WRK-015N-X                  REDEFINES           WRK-015N     
CPMCAC                                 PIC  X(015).                     
                                                                        
CPMCAC 01  WRK-015                     PIC S9(015)         VALUE ZEROS. 
CPMCAC 01  FILLER                      REDEFINES           WRK-015.     
CPMCAC     05 WRK-015-R                PIC  9(011)V99.                  
CPMCAC     05 FILLER                   PIC  9(002).                     
                                                                        
CPMCAC 01  WRK-015-COMP                PIC S9(015) COMP-3  VALUE ZEROS. 
CPMCAC 01  WRK-015-COMP-R              REDEFINES           WRK-015-COMP 
CPMCAC                                 PIC  X(008).                     
                                                                        
CPMCAC 01  WRK-15-X                    PIC  X(015)         VALUE ZEROS. 
CPMCAC 01  FILLER                      REDEFINES           WRK-15-X.    
CPMCAC     05 FILLER                   PIC  9(002).                     
CPMCAC     05 WRK-15-N                 PIC  9(013).                     
CPMCAC 01  FILLER                      REDEFINES           WRK-15-X.    
CPMCAC     05 FILLER                   PIC  9(001).                     
CPMCAC     05 WRK-14-N                 PIC  9(014).                     
                                                                        
CPMCAC 01  WRK-17                      PIC S9(017)         VALUE ZEROS. 
CPMCAC 01  WRK-17-RED                  REDEFINES           WRK-17       
CPMCAC                                 PIC  9(017).                     
                                                                        
CPMCAC 01  WRK-17N                     PIC  9(017)         VALUE ZEROS. 
CPMCAC 01  WRK-17N-X                   REDEFINES           WRK-17N      
CPMCAC                                 PIC  X(017).                     
                                                                        
CPMCAC 01  WRK-S-9-17-D                PIC +9(017)         VALUE ZEROS. 
CPMCAC 01  FILLER                      REDEFINES           WRK-S-9-17-D.
CPMCAC     05 FILLER                   PIC  X(005).                     
CPMCAC     05 WRK-9-11-V-99-D          PIC  9(011)V99.                  
                                                                        
      *----------------------------------------------------------------*
           COPY    'I#BRAD7C'.
                                                                        
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE        
           'FIM DA WORKING STORAGE SECTION '.                           
      *----------------------------------------------------------------*
                                                                        
      *================================================================*
       PROCEDURE                       DIVISION.                        
      *================================================================*
                                                                        
           OPEN    INPUT   LPCLIN                                       
                   OUTPUT  LPCLOUT                                      
                           BAIXALP                                      
                                                                        
           MOVE    WRK-ABERTURA        TO  WRK-OPERACAO                 
                                                                        
           PERFORM 9040-TESTAR-FS-LPCLIN   THRU 9040-99-FIM             
           PERFORM 9050-TESTAR-FS-LPCLOUT  THRU 9050-99-FIM             
           PERFORM 9060-TESTAR-FS-BAIXALP  THRU 9060-99-FIM             
                                                                        
           PERFORM 0550-LERLPLIN       THRU 0590-99-FIM                 
                                                                        
           CALL    'BRAD7600'          USING WRK-DATAHORA               
                                                                        
           MOVE    WRK-AAAAMMDD        TO  WRK-9-CS                     
           MOVE    WRK-9-X(2:8)        TO  WRK-DATAZ                    
           MOVE    WRK-ANOZ            TO  WRK-LANO                     
           MOVE    WRK-MESZ            TO  WRK-LMES                     
           MOVE    WRK-DIAZ            TO  WRK-LDIA                     
                                                                        
           OPEN    INPUT   SUBCPEN                                      
                                                                        
           MOVE    WRK-ABERTURA        TO  WRK-OPERACAO                 
                                                                        
           PERFORM 9030-TESTAR-FS-SUBCPEN  THRU 9030-99-FIM.            
                                                                        
      *----------------------------------------------------------------*
      *          *** SELECIONA REGISTROS DE  "SUBCPEN" ***             *
      *----------------------------------------------------------------*
                                                                        
      *================================================================*
       0010-LERSUBC.                                                    
      *================================================================*
                                                                        
           READ    SUBCPEN             INTO WRK-REGSUBC END-READ        
                                                                        
           MOVE    WRK-LEITURA         TO  WRK-OPERACAO                 
                                                                        
           IF      WRK-FS-SUBCPEN      EQUAL '10'                       
                   GO TO 0030-FIMSUBC                                   
           ELSE                                                         
                   PERFORM 9030-TESTAR-FS-SUBCPEN THRU 9030-99-FIM      
           END-IF                                                       
                                                                        
      *          (* CL / 4120)                                          
                                                                        
           IF      WRK-SUBCBXA-SUBC    EQUAL 0010                       
                   GO TO 0020-MOVEAST                                   
           END-IF                                                       
                                                                        
      *    **    (* CL / 4025)                                          
      *    **                                                           
                                                                        
           IF      WRK-SUBCBXA-SUBC    EQUAL 3000                       
                   GO TO 0020-MOVEAST                                   
           END-IF                                                       
                                                                        
      *    **    (* LP / 4025)                                          
      *    **                                                           
                                                                        
           IF      WRK-SUBCBXA-SUBC    EQUAL 6000                       
                   GO TO 0020-MOVEAST                                   
           END-IF                                                       
                                                                        
      *          (* CL / 5150)                                          
                                                                        
           IF      WRK-SUBCBXA-SUBC    EQUAL 3010                       
                   GO TO 0020-MOVEAST                                   
           END-IF                                                       
                                                                        
      *          (* CL / 7000)                                          
                                                                        
           IF      WRK-SUBCBXA-SUBC    EQUAL 0040                       
                   GO TO 0020-MOVEAST                                   
           END-IF                                                       
                                                                        
      *          (* CL / 5240)                                          
                                                                        
           IF      WRK-SUBCBXA-SUBC    EQUAL 0070                       
                   GO TO 0020-MOVEAST                                   
           END-IF                                                       
                                                                        
      *          (* LP / 4120)                                          
                                                                        
           IF      WRK-SUBCBXA-SUBC    EQUAL 3100                       
                   GO TO 0020-MOVEAST                                   
           END-IF                                                       
                                                                        
      *          (* LP / 7000)                                          
                                                                        
           IF      WRK-SUBCBXA-SUBC    EQUAL 4000                       
                   GO TO 0020-MOVEAST                                   
           END-IF                                                       
                                                                        
      *          (* LP / 6000)                                          
                                                                        
           IF      WRK-SUBCBXA-SUBC    EQUAL 3000                       
                   GO TO 0020-MOVEAST                                   
           END-IF                                                       
                                                                        
      *          (* LP / 5150)                                          
                                                                        
           IF      WRK-SUBCBXA-SUBC    EQUAL 4530                       
                   GO TO 0020-MOVEAST                                   
           END-IF                                                       
                                                                        
      *          (* LP / 5240)                                          
                                                                        
           IF      WRK-SUBCBXA-SUBC    NOT EQUAL 8500                   
                   GO TO 0010-LERSUBC                                   
           END-IF.                                                      
                                                                        
      *================================================================*
       0020-MOVEAST.                                                    
      *================================================================*
                                                                        
           MOVE    '*'                 TO  WRK-WSUBCPEN.                
                                                                        
      *================================================================*
       0030-FIMSUBC.                                                    
      *================================================================*
                                                                        
           CLOSE   SUBCPEN                                              
                                                                        
           MOVE    WRK-FECHAMENTO      TO  WRK-OPERACAO                 
                                                                        
           PERFORM 9030-TESTAR-FS-SUBCPEN THRU 9030-99-FIM              
                                                                        
           OPEN    INPUT   ARQDATA                                      
                                                                        
           MOVE    WRK-ABERTURA        TO  WRK-OPERACAO                 
                                                                        
           PERFORM 9010-TESTAR-FS-ARQDATA THRU 9010-99-FIM              
                                                                        
           READ    ARQDATA             INTO WRK-WKDATA END-READ         
                                                                        
           MOVE    WRK-LEITURA         TO  WRK-OPERACAO                 
                                                                        
           IF      WRK-FS-ARQDATA      EQUAL '10'                       
                   GO TO 0040-FIMDATA                                   
           ELSE                                                         
                   PERFORM 9010-TESTAR-FS-ARQDATA THRU 9010-99-FIM      
           END-IF                                                       
                                                                        
           MOVE    WRK-DATACBAR        TO  WRK-CABDATA                  
                                                                        
           CLOSE   ARQDATA                                              
                                                                        
           MOVE    WRK-FECHAMENTO      TO  WRK-OPERACAO                 
                                                                        
           PERFORM 9010-TESTAR-FS-ARQDATA THRU 9010-99-FIM              
                                                                        
           GO TO   0060-OPENCADA.                                       
                                                                        
      *================================================================*
       0040-FIMDATA.                                                    
      *================================================================*
                                                                        
           DISPLAY ' *** CLLP4200 - ARQUIVO DE DATAS VAZIO ***'         
           UPON OPERADOR.                                               
                                                                        
      *================================================================*
       0050-TECLDATA.                                                   
      *================================================================*
                                                                        
           DISPLAY                                                      
           'CLLP4200-TECLAR DATA DO MOVIMENTO NO FORMATO DDMMAAAA'      
           WRK-WKDATMOV                                                 
           UPON OPERADOR                                                
                                                                        
           ACCEPT  WRK-WKDATMOV        FROM CONSOLE                     
                                                                        
           MOVE    WRK-WKDATMOV(5:4)   TO  WRK-WKDTAUX(1:4)             
           MOVE    WRK-WKDATMOV(3:2)   TO  WRK-WKDTAUX(5:2)             
           MOVE    WRK-WKDATMOV(1:2)   TO  WRK-WKDTAUX(7:2)             
                                                                        
           MOVE    WRK-WKDTAUX-R       TO  WRK-DTENVIA                  
           MOVE    SPACES              TO  WRK-RESULFUI                 
                                                                        
           CALL    'BRAD0080'          USING WRK-BRAD1205               
                                             WRK-ROTDATAS               
                                             WRK-WKMENS                 
                                                                        
           IF      WRK-RESULFUI        EQUAL 'I'                        
                   GO TO 0050-TECLDATA                                  
           END-IF                                                       
                                                                        
           MOVE    WRK-DTBARRA         TO  WRK-CABDATA                  
                                                                        
CPMCAC     CLOSE   ARQDATA                                              
                                                                        
CPMCAC     MOVE    WRK-FECHAMENTO      TO  WRK-OPERACAO                 
                                                                        
CPMCAC     PERFORM 9010-TESTAR-FS-ARQDATA THRU 9010-99-FIM.             
                                                                        
      *----------------------------------------------------------------*
      *                 C O R P O   P R I N C I P A L                  *
      *----------------------------------------------------------------*
                                                                        
      *================================================================*
       0060-OPENCADA.                                                   
      *================================================================*
                                                                        
           OPEN    INPUT   BXMORA                                       
                   OUTPUT  RELATO                                       
                                                                        
           MOVE    WRK-ABERTURA        TO  WRK-OPERACAO                 
                                                                        
           PERFORM 9020-TESTAR-FS-BXMORA THRU 9020-99-FIM               
           PERFORM 9070-TESTAR-FS-RELATO THRU 9070-99-FIM               
                                                                        
           SORT    ARQSORT ASCENDING   KEY SD-STAGEN                    
                                           SD-STNROCON                  
                                           SD-STDIGCON                  
                                           SD-STCART                    
           INPUT   PROCEDURE 0090-MODULOE THRU 0120-99-FIM              
           OUTPUT  PROCEDURE 0130-MODULOS THRU 0360-99-FIM              
                                                                        
           IF  SORT-RETURN             NOT EQUAL ZEROS                  
               DISPLAY 'PGM-CLLP4200 - PROBLEMA NO SORT'                
                                       UPON OPERADOR                    
               GO TO 0080-FECHARQ                                       
           END-IF.                                                      
                                                                        
      *================================================================*
       0070-OKSORT.                                                     
      *================================================================*
                                                                        
      *    LISTAR TOTAIS DAS EMPRESAS                                   
                                                                        
           PERFORM 0460-IMPTOT         THRU 0480-99-FIM                 
           PERFORM 0490-IMPTOTGE       THRU 0490-99-FIM.                
                                                                        
      *================================================================*
       0080-FECHARQ.                                                    
      *================================================================*
                                                                        
           CLOSE   LPCLIN                                               
                   BXMORA                                               
                   LPCLOUT                                              
                   BAIXALP                                              
                   RELATO                                               
                                                                        
           MOVE    WRK-FECHAMENTO      TO  WRK-OPERACAO                 
                                                                        
           PERFORM 9040-TESTAR-FS-LPCLIN   THRU 9040-99-FIM             
           PERFORM 9020-TESTAR-FS-BXMORA   THRU 9020-99-FIM             
           PERFORM 9050-TESTAR-FS-LPCLOUT  THRU 9050-99-FIM             
           PERFORM 9060-TESTAR-FS-BAIXALP  THRU 9060-99-FIM             
           PERFORM 9070-TESTAR-FS-RELATO   THRU 9070-99-FIM             
                                                                        
           GOBACK.                                                      
                                                                        
      *----------------------------------------------------------------*
      *                      MODULO DE ENTRADA                         *
      *----------------------------------------------------------------*
                                                                        
      *================================================================*
       0090-MODULOE.                                                    
      *================================================================*
                                                                        
      *            *** SELECIONA REGISTROS DE  "BXMORA" ***             
                                                                        
           CONTINUE.                                                    
                                                                        
      *================================================================*
       0100-LERBXMOR.                                                   
      *================================================================*
                                                                        
           READ    BXMORA              INTO WRK-REGBXA END-READ         
                                                                        
           MOVE    WRK-LEITURA         TO  WRK-OPERACAO                 
                                                                        
           IF      WRK-FS-BXMORA       EQUAL '10'                       
                   GO TO 0120-FIMBXA                                    
           ELSE                                                         
                   PERFORM 9020-TESTAR-FS-BXMORA THRU 9020-99-FIM       
           END-IF                                                       
                                                                        
      *    (* CL / 4120)                                                
                                                                        
           IF      WRK-BXCART          EQUAL 0010                       
                   GO TO 0110-VECOD                                     
           END-IF                                                       
                                                                        
      *    (* CL / 4025)                                                
                                                                        
           IF      WRK-BXCART          EQUAL 3000                       
                   GO TO 0110-VECOD                                     
           END-IF                                                       
                                                                        
      *    (* LP / 4025)                                                
                                                                        
           IF      WRK-BXCART          EQUAL 6000                       
                   GO TO 0110-VECOD                                     
           END-IF                                                       
                                                                        
      *    (* CL / 5150)                                                
                                                                        
           IF      WRK-BXCART          EQUAL 3010                       
                   GO TO 0110-VECOD                                     
           END-IF                                                       
                                                                        
      *    (* CL / 7000)                                                
                                                                        
           IF      WRK-BXCART          EQUAL 0040                       
                   GO TO 0110-VECOD                                     
           END-IF                                                       
                                                                        
      *    (* CL / 5240)                                                
                                                                        
           IF      WRK-BXCART          EQUAL 0070                       
                   GO TO 0110-VECOD                                     
           END-IF                                                       
                                                                        
      *    (* LP / 4120)                                                
                                                                        
           IF      WRK-BXCART          EQUAL 3100                       
                   GO TO 0110-VECOD                                     
           END-IF                                                       
                                                                        
      *    (* LP / 7000)                                                
                                                                        
           IF      WRK-BXCART          EQUAL 4000                       
                   GO TO 0110-VECOD                                     
           END-IF                                                       
                                                                        
      *    (* LP / 6000)                                                
                                                                        
           IF      WRK-BXCART          EQUAL 6000                       
                   GO TO 0110-VECOD                                     
           END-IF                                                       
                                                                        
      *    (* LP / 5150)                                                
                                                                        
           IF      WRK-BXCART          EQUAL 4530                       
                   GO TO 0110-VECOD                                     
           END-IF                                                       
                                                                        
      *    (* LP / 5240)                                                
                                                                        
           IF      WRK-BXCART          NOT EQUAL 8500                   
                   GO TO 0100-LERBXMOR                                  
           END-IF.                                                      
                                                                        
      *================================================================*
       0110-VECOD.                                                      
      *================================================================*
                                                                        
           IF      WRK-BXEB            NOT EQUAL '2'                    
                   GO TO 0100-LERBXMOR                                  
           END-IF                                                       
                                                                        
           MOVE    WRK-BXAGEN-R        TO  WRK-STAGEN-R                 
           MOVE    WRK-BXCONTR-R       TO  WRK-STNROCON-R               
           MOVE    WRK-BXDIG           TO  WRK-STDIGCON                 
                                                                        
CPMCAC     MOVE    2                   TO  WRK-CLLP8400-TAMANHO         
CPMCAC     MOVE    'MVO'               TO  WRK-CLLP8400-OPERACAO        
                                                                        
CPMCAC     CALL    WRK-CLLP8400        USING  WRK-CLLP8400-TAMANHO      
CPMCAC                                        WRK-CLLP8400-OPERACAO     
CPMCAC                                        WRK-STCART-R              
CPMCAC                                        WRK-BXCART-R(1:2)         
CPMCAC                                        WRK-CLLP8400-TAMANHO      
                                                                        
           MOVE    WRK-STCART-RR       TO  WRK-STCART-RR                
                                                                        
      *    ** VR DE COBRANCA DEBITADO EM C/C                            
                                                                        
           MOVE    WRK-BXVRDEB-R       TO  WRK-STVRDEB-R                
           MOVE    '*'                 TO  WRK-WTEMSORT                 
                                                                        
           MOVE    WRK-AREASORT        TO  SD-ARQSORT                   
                                                                        
           RELEASE SD-ARQSORT                                           
                                                                        
           GO TO   0090-MODULOE.                                        
                                                                        
      *================================================================*
       0120-FIMBXA.                                                     
      *================================================================*
                                                                        
           CONTINUE.                                                    
                                                                        
       0120-99-FIM.               EXIT.                                 
                                                                        
      *----------------------------------------------------------------*
      *                       MODULO DE SAIDA                          *
      *----------------------------------------------------------------*
                                                                        
      *================================================================*
       0130-MODULOS.                                                    
      *================================================================*
                                                                        
           RETURN  ARQSORT             AT END                           
                   GO TO   0500-FIMSORT                                 
           END-RETURN                                                   
                                                                        
           MOVE    SD-ARQSORT          TO  WRK-AREASORT                 
CPMCAC     MOVE    WRK-STNROCON-RR     TO  WRK-STNROCON-RR.             
                                                                        
      *================================================================*
       0140-CONFRONT.                                                   
      *================================================================*
                                                                        
           IF      WRK-CHVLPCL         GREATER WRK-STCHAVE              
                   GO TO 0350-LERSORT                                   
           END-IF                                                       
                                                                        
           IF      WRK-CHVLPCL         EQUAL WRK-STCHAVE                
                   GO TO 0190-IGUAL                                     
           END-IF.                                                      
                                                                        
      *================================================================*
       0150-VESUBCPE.                                                   
      *================================================================*
                                                                        
           IF      WRK-WSUBCPEN        NOT EQUAL '*'                    
                   GO TO 0180-LERIN                                     
           END-IF.                                                      
                                                                        
      *================================================================*
       0160-GRAVOUT.                                                    
      *================================================================*
                                                                        
           IF      WRK-FLAG            EQUAL 'S'                        
                   GO TO 0170-SOGRAVA                                   
           END-IF                                                       
                                                                        
           MOVE    'S'                 TO  WRK-FLAG                     
           MOVE    'S'                 TO  WRK-FLAG1                    
           MOVE    X'FF'               TO  WRK-INCONTR.                 
                                                                        
      *================================================================*
       0170-SOGRAVA.                                                    
      *================================================================*
                                                                        
           WRITE   FD-REG-LPCLOUT      FROM WRK-REGLPCL                 
                                                                        
           MOVE    WRK-GRAVACAO        TO  WRK-OPERACAO                 
                                                                        
           PERFORM 9050-TESTAR-FS-LPCLOUT THRU 9050-99-FIM.             
                                                                        
      *================================================================*
       0180-LERIN.                                                      
      *================================================================*
                                                                        
           PERFORM 0550-LERLPLIN       THRU 0590-99-FIM                 
                                                                        
           GO TO   0140-CONFRONT.                                       
                                                                        
      *================================================================*
       0190-IGUAL.                                                      
      *================================================================*
                                                                        
           IF      WRK-STCHAVE         EQUAL X'99999F9999999FF9999F'    
                   GO TO 0360-RETORNAR                                  
           END-IF.                                                      
                                                                        
      *================================================================*
       0200-VEBAIXA.                                                    
      *================================================================*
                                                                        
           IF      WRK-FLAG1           EQUAL 'S'                        
                   GO TO 0210-CRIABXA                                   
           END-IF                                                       
                                                                        
           IF      WRK-WSUBCPEN        NOT EQUAL '*'                    
                   GO TO 0210-CRIABXA                                   
           END-IF                                                       
                                                                        
           MOVE    'S'                 TO  WRK-FLAG1                    
           MOVE    'S'                 TO  WRK-FLAG                     
           MOVE    X'FF'               TO  WRK-INCONTR                  
                                                                        
           WRITE   FD-REG-LPCLOUT      FROM WRK-REGLPCL                 
                                                                        
           MOVE    WRK-GRAVACAO        TO  WRK-OPERACAO                 
                                                                        
           PERFORM 9050-TESTAR-FS-LPCLOUT THRU 9050-99-FIM.             
                                                                        
      *================================================================*
       0210-CRIABXA.                                                    
      *================================================================*
                                                                        
      *    ** TIPO=1 ALT. 24/09/93                                      
                                                                        
           MOVE    '1'                 TO  WRK-LPFIXO                   
           MOVE    '000000000000000'   TO  WRK-LPVROPER                 
                                                                        
           MOVE    WRK-INEMPR-RR       TO  WRK-LPEMPR-R                 
           MOVE    WRK-INAGEN-RR       TO  WRK-LPAGEN-R                 
           MOVE    WRK-INNRO-RR        TO  WRK-LPNRO                    
                                                                        
           MOVE    WRK-INDIG           TO  WRK-LPDIG                    
                                                                        
      *    ** IDENTIFICACAO CL/LP                                       
                                                                        
           MOVE    WRK-INIDENT         TO  WRK-LPIDEN                   
                                                                        
      *    ** DATA DO VENCIMENTO                                        
                                                                        
           MOVE    WRK-INVCTO          TO  WRK-9-9-D                    
           MOVE    WRK-9-8-D           TO  WRK-LPDTVCTO                 
                                                                        
      *    ** NUMERO C/C                                                
                                                                        
           MOVE    WRK-INNROCC         TO  WRK-07                       
           MOVE    WRK-07-RED          TO  WRK-07N                      
           MOVE    WRK-07N-X           TO  WRK-LPNRCC                   
                                                                        
      *    ** CARTEIRA DE ORIGEM                                        
                                                                        
           MOVE    WRK-INCARTA         TO  WRK-LPCARCL                  
                                                                        
      *    ** NUMERO DO CONTRATO                                        
                                                                        
           MOVE    WRK-INCONTOR        TO  WRK-07                       
           MOVE    WRK-07-RED          TO  WRK-07N                      
           MOVE    WRK-07N-X           TO  WRK-LPNRCONT                 
                                                                        
      *    ** VALOR DE COBRANCA                                         
                                                                        
           MOVE    WRK-INVRCOBR        TO  WRK-15                       
           MOVE    WRK-15-RED          TO  WRK-015N                     
           MOVE    WRK-015N-X          TO  WRK-LPVRCONT                 
                                                                        
           IF      WRK-INIDENT         NOT EQUAL 'CL'                   
               GO TO 0220-REGLP                                         
           END-IF                                                       
                                                                        
           MOVE    WRK-DATASBAR        TO  WRK-LPDTCL                   
                                                                        
      *    ** DATA PAGAMENTO CL                                         
                                                                        
           MOVE    WRK-LPDTCL          TO  WRK-LPDTPGTO                 
           MOVE    '000000'            TO  WRK-LPDTPL(1:6)              
                                                                        
           GO TO   0290-REGCL.                                          
                                                                        
      *                                                                 
      *          R E G I S T R O     D E       (   L   P   )            
      *                                                                 
                                                                        
      *================================================================*
       0220-REGLP.                                                      
      *================================================================*
                                                                        
           MOVE    '00000000'          TO  WRK-LPDTCL                   
           MOVE    WRK-DATASBAR        TO  WRK-LPDTPL                   
                                                                        
      *    ** DATA DO PAGAMENTO LP                                      
                                                                        
           MOVE    WRK-LPDTPL          TO  WRK-LPDTPGTO                 
                                                                        
      *    ** CONFRONTA VALOR DEBITADO                                  
      *            COM  VALOR INFORMATIVO                               
                                                                        
           IF      WRK-STVRDEB         LESS WRK-INVRCOBR-R              
                   GO TO 0230-BXPARLP                                   
           END-IF                                                       
                                                                        
           GO TO   0260-BXTOTLP.                                        
                                                                        
      ******************************************************************
      *                     BAIXA PARCIAL  ( L P )                     *
      ******************************************************************
                                                                        
      *================================================================*
       0230-BXPARLP.                                                    
      *================================================================*
                                                                        
           MOVE    'PARCIAL'           TO  WRK-AUXTPBX                  
           MOVE    '4'                 TO  WRK-LPTIPBXA                 
           MOVE    '3'                 TO  WRK-LPACERTO                 
           MOVE    'DA'                TO  WRK-LPACDADE                 
           MOVE    ZEROS               TO  WRK-WVRCORR                  
                                                                        
      *                                                                 
      *   ** CALCULA VR. CONTABIL                                       
                                                                        
           MOVE     WRK-INVRCONT       TO     WRK-VRAUX                 
                                                                        
           MULTIPLY WRK-STVRDEB        BY     WRK-VRAUX                 
                                                                        
           DIVIDE   WRK-VRAUX          BY        WRK-INVRCOBR           
                                       GIVING    WRK-VRAUX              
                                       REMAINDER WRK-RESTO              
                                                                        
           MOVE     WRK-VRAUX-RR       TO     WRK-WVRCON                
                                                                        
           MOVE    ZEROS               TO  WRK-WVRIOF                   
                                                                        
      *    ** CALCULA VR. IOF COMPLEMENTAR                              
                                                                        
           IF      WRK-INVRIOF         EQUAL ZEROS                      
                   GO TO 0240-VECOBR                                    
           END-IF                                                       
                                                                        
           MOVE     WRK-INVRIOF        TO     WRK-VRAUX                 
                                                                        
           MULTIPLY WRK-STVRDEB        BY     WRK-VRAUX                 
                                                                        
           DIVIDE   WRK-VRAUX          BY        WRK-INVRCOBR           
                                       GIVING    WRK-VRAUX              
                                       REMAINDER WRK-RESTO              
                                                                        
           MOVE     WRK-VRAUX-RR       TO     WRK-WVRIOF                
      *                                                                 
           MOVE    ZEROS               TO  WRK-WVBASIOF                 
                                                                        
      *    ** CALCULA VR. BASE IOF                                      
                                                                        
           IF      WRK-INVBAIOF        EQUAL ZEROS                      
                   GO TO 0240-VECOBR                                    
           END-IF                                                       
                                                                        
           MOVE     WRK-INVBAIOF       TO     WRK-VRAUX                 
                                                                        
           MULTIPLY WRK-STVRDEB        BY     WRK-VRAUX                 
                                                                        
           DIVIDE   WRK-VRAUX          BY        WRK-INVRCOBR           
                                       GIVING    WRK-VRAUX              
                                       REMAINDER WRK-RESTO              
                                                                        
           MOVE     WRK-VRAUX-RR       TO     WRK-WVBASIOF.             
                                                                        
      *================================================================*
       0240-VECOBR.                                                     
      *================================================================*
                                                                        
      *    ** VALOR DE COBRANCA                                         
                                                                        
           MOVE    WRK-STVRDEB         TO  WRK-WVRCOBR                  
      *                                                                 
           IF      WRK-INVRVIN         EQUAL ZEROS                      
                   GO TO 0250-VEDIF                                     
           END-IF                                                       
                                                                        
           IF      WRK-INEMPR          NOT EQUAL 5240                   
                   GO TO 0250-VEDIF                                     
           END-IF                                                       
                                                                        
      *    ** CALCULA VR.ENC.VINC. - EMPRESA 5240                       
                                                                        
           MOVE     WRK-INVRVIN        TO     WRK-VRAUX                 
                                                                        
           MULTIPLY WRK-STVRDEB        BY     WRK-VRAUX                 
                                                                        
           DIVIDE   WRK-VRAUX          BY        WRK-INVRCOBR           
                                       GIVING    WRK-VRAUX              
                                       REMAINDER WRK-RESTO              
                                                                        
           MOVE     WRK-VRAUX-RR       TO     WRK-WVRCORR.              
                                                                        
      *================================================================*
       0250-VEDIF.                                                      
      *================================================================*
                                                                        
      *    ** CALCULA VR.ENCARGOS DEBITADO                              
                                                                        
           MOVE    WRK-STVRDEB         TO  WRK-ENCARGOS                 
                                                                        
           SUBTRACT WRK-WVRCON         FROM  WRK-ENCARGOS               
      *                                                                 
           MOVE    '000000000000000'   TO  WRK-LPVRVEN                  
                                                                        
           MOVE    WRK-WVRCON          TO  WRK-13-CS                    
           MOVE    WRK-13-CS-R         TO  WRK-LPVRCON-R                
                                                                        
           MOVE    WRK-WVRIOF          TO  WRK-17                       
           MOVE    WRK-17-RED          TO  WRK-17N                      
           MOVE    WRK-17N-X(3:15)     TO  WRK-LPVROPER                 
                                                                        
           MOVE    WRK-WVBASIOF        TO  WRK-17                       
           MOVE    WRK-17-RED          TO  WRK-17N                      
           MOVE    WRK-17N-X(3:15)     TO  WRK-LPBASIOF                 
                                                                        
           MOVE    WRK-STVRDEB         TO  WRK-13-CS                    
           MOVE    WRK-13-CS-R         TO  WRK-LPVRDEB-R                
                                                                        
           MOVE    WRK-WVRCOBR         TO  WRK-13-CS                    
           MOVE    WRK-13-CS-R         TO  WRK-LPVRCOBR-R               
                                                                        
           MOVE    WRK-WVRCORR         TO  WRK-13-CS                    
           MOVE    WRK-13-CS-R         TO  WRK-LPVRVIN-R                
      *                                                                 
           GO TO   0340-GVBXA.                                          
                                                                        
      ******************************************************************
      *                    BAIXA  TOTAL   ( L P )                      *
      ******************************************************************
                                                                        
      *================================================================*
       0260-BXTOTLP.                                                    
      *================================================================*
                                                                        
           MOVE    'TOTAL'             TO  WRK-AUXTPBX                  
           MOVE    '2'                 TO  WRK-LPTIPBXA                 
           MOVE    '3'                 TO  WRK-LPACERTO                 
           MOVE    'DA'                TO  WRK-LPACDADE                 
                                                                        
           MOVE    WRK-INVRCONT        TO  WRK-15                       
           MOVE    WRK-15-RED          TO  WRK-015N                     
           MOVE    WRK-015N-X          TO  WRK-LPVRCON                  
                                                                        
           MOVE    WRK-STVRDEB         TO  WRK-13-CS                    
           MOVE    WRK-13-CS-R         TO  WRK-LPVRDEB-R                
                                                                        
           MOVE    WRK-INVRCOBR        TO  WRK-15                       
           MOVE    WRK-15-RED          TO  WRK-015N                     
           MOVE    WRK-015N-X          TO  WRK-LPVRCOBR                 
                                                                        
           MOVE    '000000000000000'   TO  WRK-LPVRVEN                  
                                                                        
           MOVE    WRK-INVRCONT-R(2:7) TO  WRK-WVRCON-R                 
           MOVE    WRK-INVRIOF-R       TO  WRK-WVRIOF-R                 
           MOVE    WRK-INVBAIOF-R      TO  WRK-WVBASIOF-R               
                                                                        
           MOVE    WRK-STVRDEB         TO  WRK-ENCARGOS                 
                                                                        
           SUBTRACT WRK-INVRCONT       FROM  WRK-ENCARGOS               
      *                                                                 
           IF      WRK-INEMPR          EQUAL 5240                       
                   GO TO 0270-MOVECPO                                   
           END-IF                                                       
                                                                        
           MOVE    '000000000000000'   TO  WRK-LPVRVIN                  
                                                                        
           GO TO   0280-GRVFT4.                                         
                                                                        
      *================================================================*
       0270-MOVECPO.                                                    
      *================================================================*
                                                                        
           MOVE    WRK-INVRVIN         TO  WRK-15                       
           MOVE    WRK-15-RED          TO  WRK-015N                     
           MOVE    WRK-015N-X          TO  WRK-LPVRVIN.                 
                                                                        
      *================================================================*
       0280-GRVFT4.                                                     
      *================================================================*
                                                                        
           GO TO   0340-GVBXA.                                          
                                                                        
      ******************************************************************
      *          R E G I S T R O     D E       (   C   L   )           *
      ******************************************************************
                                                                        
      *================================================================*
       0290-REGCL.                                                      
      *================================================================*
                                                                        
           IF      WRK-STVRDEB         LESS WRK-INVRCOBR-R              
                   GO TO 0300-BXPARCL                                   
           END-IF                                                       
                                                                        
      *    * IGUAL OU MAIOR = BAIXA TOTAL                               
                                                                        
           GO TO   0330-BXTOTCL.                                        
                                                                        
      ******************************************************************
      *                    BAIXA PARCIAL  ( C L )                      *
      ******************************************************************
                                                                        
      *================================================================*
       0300-BXPARCL.                                                    
      *================================================================*
                                                                        
           MOVE    'PARCIAL'           TO  WRK-AUXTPBX                  
           MOVE    '4'                 TO  WRK-LPTIPBXA                 
           MOVE    '3'                 TO  WRK-LPACERTO                 
           MOVE    'DA'                TO  WRK-LPACDADE                 
           MOVE    '000000000000000'   TO  WRK-LPVRVEN                  
           MOVE    ZEROS               TO  WRK-WVVENAUX                 
           MOVE    ZEROS               TO  WRK-WVRCORR                  
      *                                                                 
      *    ** CALCULA VALOR CONTABIL                                    
                                                                        
           MOVE     WRK-INVRCONT       TO     WRK-VRAUX                 
                                                                        
           MULTIPLY WRK-STVRDEB        BY     WRK-VRAUX                 
                                                                        
           DIVIDE   WRK-VRAUX          BY        WRK-INVRCOBR           
                                       GIVING    WRK-VRAUX              
                                       REMAINDER WRK-RESTO              
                                                                        
           MOVE     WRK-VRAUX-RR       TO     WRK-WVRCON                
           MOVE     WRK-STVRDEB        TO     WRK-WVRCOBR               
      *                                                                 
           MOVE     ZEROS              TO     WRK-WVRIOF                
                                                                        
      *    ** CALCULA VR. IOF COMPLEMENTAR                              
                                                                        
           IF      WRK-INVRIOF         EQUAL ZEROS                      
                   GO TO 0310-VEVEN                                     
           END-IF                                                       
                                                                        
           MOVE     WRK-INVRIOF        TO     WRK-VRAUX                 
                                                                        
           MULTIPLY WRK-STVRDEB        BY     WRK-VRAUX                 
                                                                        
           DIVIDE   WRK-VRAUX          BY        WRK-INVRCOBR           
                                       GIVING    WRK-VRAUX              
                                       REMAINDER WRK-RESTO              
                                                                        
           MOVE     WRK-VRAUX-RR       TO     WRK-WVRIOF                
      *                                                                 
           MOVE     ZEROS              TO     WRK-WVBASIOF              
                                                                        
      *    ** CALCULA VR. BASE IOF                                      
                                                                        
           IF      WRK-INVBAIOF        EQUAL ZEROS                      
                   GO TO 0310-VEVEN                                     
           END-IF                                                       
                                                                        
           MOVE     WRK-INVBAIOF       TO     WRK-VRAUX                 
                                                                        
           MULTIPLY WRK-STVRDEB        BY     WRK-VRAUX                 
                                                                        
           DIVIDE   WRK-VRAUX          BY        WRK-INVRCOBR           
                                       GIVING    WRK-VRAUX              
                                       REMAINDER WRK-RESTO              
                                                                        
           MOVE    WRK-VRAUX-RR        TO  WRK-WVBASIOF.                
                                                                        
      *================================================================*
       0310-VEVEN.                                                      
      *================================================================*
                                                                        
           IF      WRK-INVRVEN         EQUAL ZEROS                      
                   GO TO 0320-VEENC                                     
           END-IF                                                       
                                                                        
      *    ** CALCULA VALOR DE ENCARGOS                                 
                                                                        
           MOVE     WRK-INVRVEN        TO     WRK-VRAUX                 
                                                                        
           MULTIPLY WRK-STVRDEB        BY     WRK-VRAUX                 
                                                                        
           DIVIDE   WRK-VRAUX          BY        WRK-INVRCOBR           
                                       GIVING    WRK-VRAUX              
                                       REMAINDER WRK-RESTO              
                                                                        
           MOVE     WRK-VRAUX-RR       TO     WRK-WVVENAUX.             
                                                                        
      *================================================================*
       0320-VEENC.                                                      
      *================================================================*
                                                                        
      *    * CALCULA ENCARGOS                                           
                                                                        
           MOVE    WRK-STVRDEB         TO  WRK-ENCARGOS                 
                                                                        
           SUBTRACT WRK-WVRCON         FROM  WRK-ENCARGOS               
                                                                        
           MOVE    WRK-WVVENAUX        TO  WRK-13-CS                    
           MOVE    WRK-13-CS-R         TO  WRK-LPVRVEN-R                
                                                                        
           MOVE    WRK-STVRDEB         TO  WRK-13-CS                    
           MOVE    WRK-13-CS-R         TO  WRK-LPVRDEB-R                
                                                                        
           MOVE    WRK-WVRCON          TO  WRK-13-CS                    
           MOVE    WRK-13-CS-R         TO  WRK-LPVRCON-R                
                                                                        
           MOVE    WRK-WVRIOF          TO  WRK-17                       
           MOVE    WRK-17-RED          TO  WRK-17N                      
           MOVE    WRK-17N-X(3:15)     TO  WRK-LPVROPER                 
                                                                        
           MOVE    WRK-WVBASIOF        TO  WRK-17                       
           MOVE    WRK-17-RED          TO  WRK-17N                      
           MOVE    WRK-17N-X(3:15)     TO  WRK-LPBASIOF                 
                                                                        
           MOVE    WRK-WVRCOBR         TO  WRK-13-CS                    
           MOVE    WRK-13-CS-R         TO  WRK-LPVRCOBR-R               
                                                                        
           MOVE    '000000000000000'   TO  WRK-LPVRVIN                  
                                                                        
           GO TO   0340-GVBXA.                                          
                                                                        
      ******************************************************************
      *                    BAIXA  TOTAL   ( C L )                      *
      ******************************************************************
                                                                        
      *================================================================*
       0330-BXTOTCL.                                                    
      *================================================================*
                                                                        
           MOVE    'TOTAL'             TO  WRK-AUXTPBX                  
           MOVE    '2'                 TO  WRK-LPTIPBXA                 
           MOVE    '3'                 TO  WRK-LPACERTO                 
           MOVE    'DA'                TO  WRK-LPACDADE                 
                                                                        
           MOVE    WRK-INVRCONT        TO  WRK-15                       
           MOVE    WRK-15-RED          TO  WRK-015N                     
           MOVE    WRK-015N-X          TO  WRK-LPVRCON                  
                                                                        
           MOVE    WRK-INVRVEN         TO  WRK-15                       
           MOVE    WRK-15-RED          TO  WRK-015N                     
           MOVE    WRK-015N-X          TO  WRK-LPVRVEN                  
                                                                        
           MOVE    WRK-STVRDEB         TO  WRK-13-CS                    
           MOVE    WRK-13-CS-R         TO  WRK-LPVRDEB-R                
                                                                        
           MOVE    WRK-INVRCOBR        TO  WRK-15                       
           MOVE    WRK-15-RED          TO  WRK-015N                     
           MOVE    WRK-015N-X          TO  WRK-LPVRCOBR                 
                                                                        
           MOVE    WRK-STVRDEB         TO  WRK-ENCARGOS                 
                                                                        
           SUBTRACT WRK-INVRCONT       FROM  WRK-ENCARGOS               
                                                                        
           MOVE    '000000000000000'   TO  WRK-LPVRVIN.                 
                                                                        
      *================================================================*
       0340-GVBXA.                                                      
      *================================================================*
                                                                        
           PERFORM 0370-LISTABXA       THRU 0450-99-FIM                 
                                                                        
           MOVE    'N'                 TO  WRK-LPFIXON                  
                                                                        
           MOVE    WRK-INALIQR         TO  WRK-9-CS                     
           MOVE    WRK-9-CS-R          TO  WRK-LPVRBASE-R               
                                                                        
           MOVE    WRK-INALIQT         TO  WRK-9-CS                     
           MOVE    WRK-9-CS-R          TO  WRK-LPVRINI-R                
                                                                        
           MOVE    WRK-INALIQC         TO  WRK-9-CS                     
           MOVE    WRK-9-CS-R          TO  WRK-LPVRIOF-R                
                                                                        
           WRITE   FD-REG-BAIXALP      FROM WRK-REGBXALP                
                                                                        
           MOVE    WRK-GRAVACAO        TO  WRK-OPERACAO                 
                                                                        
           PERFORM 9060-TESTAR-FS-BAIXALP THRU 9060-99-FIM              
                                                                        
           MOVE    ZEROS               TO  WRK-LPVROPER                 
           MOVE    ZEROS               TO  WRK-LPBASIOF                 
           MOVE    ZEROS               TO  WRK-LPVRBASE                 
           MOVE    ZEROS               TO  WRK-LPVRINI                  
           MOVE    ZEROS               TO  WRK-LPVRIOF                  
                                                                        
           PERFORM 0550-LERLPLIN       THRU 0590-99-FIM.                
                                                                        
      *================================================================*
       0350-LERSORT.                                                    
      *================================================================*
                                                                        
           GO  TO  0130-MODULOS.                                        
                                                                        
      *================================================================*
       0360-RETORNAR.                                                   
      *================================================================*
                                                                        
           CONTINUE.                                                    
                                                                        
       0360-99-FIM.               EXIT.                                 
                                                                        
      *----------------------------------------------------------------*
      *                       ROTINAS DE IMPRESSAO                     *
      *----------------------------------------------------------------*
                                                                        
      *================================================================*
       0370-LISTABXA.                                                   
      *================================================================*
                                                                        
           MOVE    LOW-VALUES          TO  WRK-SAIDA(2:149).            
                                                                        
      *================================================================*
       0380-IMPDET.                                                     
      *================================================================*
                                                                        
           MOVE    WRK-LPEMPR(2:4)     TO  WRK-SAIDA(2:4)               
           MOVE    '-'                 TO  WRK-SAIDA(6:1)               
           MOVE    WRK-LPAGEN          TO  WRK-SAIDA(7:5)               
           MOVE    '-'                 TO  WRK-SAIDA(12:1)              
                                                                        
           MOVE    WRK-INNRO           TO  WRK-15                       
           MOVE    WRK-15-RED          TO  WRK-015N                     
           MOVE    WRK-015N-X          TO  WRK-SAIDA(13:15)             
                                                                        
           MOVE    '/'                 TO  WRK-SAIDA(28:1)              
           MOVE    WRK-INDIG           TO  WRK-SAIDA(29:1)              
                                                                        
           MOVE    WRK-INTPPND         TO  WRK-05                       
           MOVE    WRK-05-RED          TO  WRK-05N                      
           MOVE    WRK-05N-X(2:4)      TO  WRK-SAIDA(32:4)              
                                                                        
           MOVE    WRK-INNROCON        TO  WRK-07                       
           MOVE    WRK-07-RED          TO  WRK-07N                      
           MOVE    WRK-07N-X           TO  WRK-SAIDA(37:7)              
                                                                        
BRQ=E******MOVE    WRK-INCART          TO  WRK-03
BRQ=E******MOVE    WRK-03-RED          TO  WRK-03N
BRQ=E******MOVE    WRK-03N-X           TO  WRK-SAIDA(46:3)
           MOVE    WRK-INCARTA         TO  WRK-SAIDA(46:3)
                                                                        
           MOVE    WRK-INIDENT         TO  WRK-SAIDA(50:2)              
                                                                        
           MOVE    WRK-INNROCC         TO  WRK-07                       
           MOVE    WRK-07-RED          TO  WRK-07N                      
           MOVE    WRK-07N-X           TO  WRK-SAIDA(53:7)              
                                                                        
           MOVE    WRK-INVCTO          TO  WRK-PAD12                    
           MOVE    WRK-PAD12-R         TO  WRK-SAIDA(60:12)             
                                                                        
           MOVE    WRK-AUXPRELP        TO  WRK-SAIDA(72:1)              
                                                                        
           MOVE    WRK-LPVRCON(2:14)   TO  WRK-15-X(2:14)               
           MOVE    WRK-15-N            TO  WRK-WVRCON                   
                                                                        
           MOVE    WRK-LPVRVEN(2:14)   TO  WRK-15-X(2:14)               
           MOVE    WRK-15-N            TO  WRK-WVRVEN                   
                                                                        
           MOVE    WRK-WVRCON          TO  WRK-013                      
           MOVE    WRK-013-R           TO  WRK-PADRAO1                  
           MOVE    WRK-PADRAO1-R       TO  WRK-SAIDA(73:16)             
                                                                        
           MOVE    WRK-WVRVEN          TO  WRK-013                      
           MOVE    WRK-013-R           TO  WRK-PADRAO                   
           MOVE    WRK-PADRAO-R        TO  WRK-SAIDA(89:18)             
                                                                        
           IF      WRK-LPEMPR          NOT EQUAL '05240'                
                   GO TO 0390-EMP5240                                   
           END-IF                                                       
                                                                        
           MOVE    WRK-WVRVIN          TO  WRK-015                      
           MOVE    WRK-015-R           TO  WRK-PADRAO                   
           MOVE    WRK-PADRAO-R        TO  WRK-SAIDA(107:18)            
                                                                        
           GO TO 0400-PULAEMP.                                          
                                                                        
      *================================================================*
       0390-EMP5240.                                                    
      *================================================================*
                                                                        
           MOVE    WRK-WVRIOF          TO  WRK-S-9-17-D                 
           MOVE    WRK-9-11-V-99-D     TO  WRK-PADRAO                   
           MOVE    WRK-PADRAO-R        TO  WRK-SAIDA(107:18).           
                                                                        
      *================================================================*
       0400-PULAEMP.                                                    
      *================================================================*
                                                                        
           MOVE    WRK-LPVRVIN(2:14)   TO  WRK-15-X(2:14)               
           MOVE    WRK-14-N            TO  WRK-WVRVIN                   
                                                                        
           MOVE    WRK-STVRDEB         TO  WRK-013                      
           MOVE    WRK-013-R           TO  WRK-PADRAO                   
           MOVE    WRK-PADRAO-R        TO  WRK-SAIDA(125:18)            
                                                                        
           MOVE    WRK-AUXTPBX         TO  WRK-SAIDA(144:7)             
      *                                                                 
           MOVE    1                   TO  WRK-IND-R6.                  
                                                                        
      *================================================================*
       0410-COMPEMP.                                                    
      *================================================================*
                                                                        
           MOVE    WRK-TABACUMX(WRK-IND-R6:3) TO WRK-05-COMP-R          
                                                                        
           IF      WRK-INEMPR          EQUAL WRK-05-COMP                
                   GO TO 0430-SOMAR                                     
           END-IF                                                       
                                                                        
           IF      WRK-INEMPR          LESS WRK-05-COMP                 
                   GO TO 0420-ERROTAB                                   
           END-IF                                                       
                                                                        
           ADD     31                  TO  WRK-IND-R6                   
                                                                        
           GO TO   0410-COMPEMP.                                        
                                                                        
      *================================================================*
       0420-ERROTAB.                                                    
      *================================================================*
                                                                        
           MOVE    2002                TO  WRK-ABEND                    
           MOVE    'S'                 TO  WRK-DUMP                     
                                                                        
           CALL    'BRAD0450'          USING WRK-ABEND                  
                                             WRK-DUMP.                  
                                                                        
      *================================================================*
       0430-SOMAR.                                                      
      *================================================================*
                                                                        
           MOVE    WRK-TABACUMX(WRK-IND-R6 + 3:8) TO WRK-015-COMP-R     
                                                                        
           ADD     WRK-WVRCON         TO  WRK-015-COMP                  
                                                                        
           MOVE    WRK-015-COMP-R     TO  WRK-TABACUMX(WRK-IND-R6 + 3:8)
                                                                        
           MOVE    WRK-TABACUMX(WRK-IND-R6 + 11:8) TO WRK-015-COMP-R    
                                                                        
           ADD     WRK-WVRVEN         TO  WRK-015-COMP                  
                                                                        
           MOVE    WRK-015-COMP-R     TO                                
                                         WRK-TABACUMX(WRK-IND-R6 + 11:8)
                                                                        
           MOVE    WRK-TABACUMX(WRK-IND-R6 + 19:8) TO WRK-015-COMP-R    
                                                                        
           ADD     WRK-STVRDEB        TO  WRK-015-COMP                  
                                                                        
           MOVE    WRK-015-COMP-R     TO                                
                                         WRK-TABACUMX(WRK-IND-R6 + 19:8)
                                                                        
           MOVE    WRK-TABACUMX(WRK-IND-R6 + 27:4) TO WRK-07-COMP-R     
                                                                        
           ADD     1                  TO  WRK-07-COMP                   
                                                                        
           MOVE    WRK-07-COMP-R      TO                                
                                         WRK-TABACUMX(WRK-IND-R6 + 27:4)
                                                                        
           ADD     WRK-WVRCON          TO  WRK-WTOTCON                  
           ADD     WRK-WVRVEN          TO  WRK-WTOTVEN                  
           ADD     WRK-STVRDEB         TO  WRK-WTOTCONS                 
           ADD     1                   TO  WRK-WTOTQTDE.                
                                                                        
      *================================================================*
       0440-LISTAR.                                                     
      *================================================================*
                                                                        
           IF      WRK-FOL             LESS 55                          
                   GO TO 0450-IMP                                       
           END-IF                                                       
                                                                        
           MOVE    ZEROS               TO  WRK-FOL                      
                                                                        
           WRITE   FD-REG-RELATO       FROM WRK-CANAL                   
                                                                        
           MOVE    WRK-GRAVACAO        TO  WRK-OPERACAO                 
                                                                        
           PERFORM 9070-TESTAR-FS-RELATO THRU 9070-99-FIM               
                                                                        
           ADD     1                   TO  WRK-WFOL                     
                                                                        
           MOVE    WRK-WFOL            TO  WRK-05                       
           MOVE    WRK-05-RED          TO  WRK-CBFOL-R                  
                                                                        
           WRITE   FD-REG-RELATO       FROM WRK-CAB1                    
                                                                        
           MOVE    WRK-GRAVACAO        TO  WRK-OPERACAO                 
                                                                        
           PERFORM 9070-TESTAR-FS-RELATO THRU 9070-99-FIM               
                                                                        
           WRITE   FD-REG-RELATO       FROM WRK-CAB2                    
                                                                        
           PERFORM 9070-TESTAR-FS-RELATO THRU 9070-99-FIM.              
                                                                        
      *================================================================*
       0450-IMP.                                                        
      *================================================================*
                                                                        
           WRITE   FD-REG-RELATO       FROM WRK-SAIDA                   
                                                                        
           MOVE    WRK-GRAVACAO        TO  WRK-OPERACAO                 
                                                                        
           PERFORM 9070-TESTAR-FS-RELATO THRU 9070-99-FIM               
                                                                        
           ADD     1                   TO  WRK-FOL.                     
                                                                        
       0450-99-FIM.                    EXIT.                            
                                                                        
      *================================================================*
       0460-IMPTOT.                                                     
      *================================================================*
                                                                        
           MOVE    1                   TO WRK-IND-R6.                   
                                                                        
      *================================================================*
       0470-IMPRIME.                                                    
      *================================================================*
                                                                        
           MOVE    LOW-VALUES          TO  WRK-SAIDA(2:149)             
                                                                        
           WRITE   FD-REG-RELATO       FROM WRK-SAIDA                   
                                                                        
           MOVE    WRK-GRAVACAO        TO  WRK-OPERACAO                 
                                                                        
           PERFORM 9070-TESTAR-FS-RELATO THRU 9070-99-FIM               
                                                                        
           MOVE    'TOTAL DA EMPRESA'  TO  WRK-SAIDA(2:17).             
                                                                        
      *================================================================*
       0480-VOLTAR.                                                     
      *================================================================*
                                                                        
           MOVE    WRK-TABACUMX(WRK-IND-R6:3) TO WRK-05-COMP-X          
           MOVE    WRK-05-N-COMP       TO  WRK-05N                      
           MOVE    WRK-05N-X(2:4)      TO  WRK-SAIDA(21:4)              
                                                                        
           MOVE    '-'                 TO  WRK-SAIDA(28:1)              
                                                                        
           MOVE    WRK-TABACUMX(WRK-IND-R6 + 27:4) TO WRK-07-X          
           MOVE    WRK-07-C2           TO  WRK-PAD10                    
           MOVE    WRK-PAD10-R         TO  WRK-SAIDA(31:10)             
                                                                        
           MOVE    '-QTDE'             TO  WRK-SAIDA(41:10)             
                                                                        
           MOVE    WRK-TABACUMX(WRK-IND-R6 + 4:7)  TO WRK-13-X          
           MOVE    WRK-13-C2           TO  WRK-013                      
           MOVE    WRK-013-R           TO  WRK-PADRAO                   
           MOVE    WRK-PADRAO-R        TO  WRK-SAIDA(71:18)             
                                                                        
           MOVE    WRK-TABACUMX(WRK-IND-R6 + 12:7) TO WRK-13-X          
           MOVE    WRK-13-C2           TO  WRK-013                      
           MOVE    WRK-013-R           TO  WRK-PADRAO                   
           MOVE    WRK-PADRAO-R        TO  WRK-SAIDA(89:18)             
                                                                        
           MOVE    WRK-TABACUMX(WRK-IND-R6 + 20:7) TO WRK-13-X          
           MOVE    WRK-13-C2           TO  WRK-013                      
           MOVE    WRK-013-R           TO  WRK-PADRAO                   
           MOVE    WRK-PADRAO-R        TO  WRK-SAIDA(125:18)            
                                                                        
           WRITE   FD-REG-RELATO       FROM WRK-SAIDA                   
                                                                        
           MOVE    WRK-GRAVACAO        TO  WRK-OPERACAO                 
                                                                        
           PERFORM 9070-TESTAR-FS-RELATO THRU 9070-99-FIM               
                                                                        
           MOVE    SPACES              TO  WRK-SAIDA(2:17)              
                                                                        
           ADD     31                  TO WRK-IND-R6                    
                                                                        
           MOVE    WRK-TABACUMX(WRK-IND-R6:3) TO WRK-05-COMP-R          
                                                                        
           IF      WRK-05-COMP         NOT EQUAL 9999                   
                   GO TO 0480-VOLTAR                                    
           END-IF.                                                      
                                                                        
       0480-99-FIM.                    EXIT.                            
                                                                        
      *================================================================*
       0490-IMPTOTGE.                                                   
      *================================================================*
                                                                        
           MOVE    LOW-VALUES          TO  WRK-SAIDA(2:149)             
                                                                        
           WRITE   FD-REG-RELATO       FROM WRK-SAIDA                   
                                                                        
           MOVE    WRK-GRAVACAO        TO  WRK-OPERACAO                 
                                                                        
           PERFORM 9070-TESTAR-FS-RELATO THRU 9070-99-FIM               
                                                                        
           MOVE    'TOTAL GERAL'       TO  WRK-SAIDA(2:11)              
                                                                        
           MOVE    WRK-WTOTQTDE        TO  WRK-07COMP-N                 
           MOVE    WRK-07COMP-R        TO  WRK-PAD10                    
           MOVE    WRK-PAD10-R         TO  WRK-SAIDA(31:10)             
                                                                        
           MOVE    '-QTDE'             TO  WRK-SAIDA(41:10)             
                                                                        
           MOVE    WRK-WTOTCON         TO  WRK-013                      
           MOVE    WRK-013-R           TO  WRK-PADRAO                   
           MOVE    WRK-PADRAO-R        TO  WRK-SAIDA(71:18)             
                                                                        
           MOVE    WRK-WTOTVEN         TO  WRK-013                      
           MOVE    WRK-013-R           TO  WRK-PADRAO                   
           MOVE    WRK-PADRAO-R        TO  WRK-SAIDA(89:18)             
                                                                        
           MOVE    WRK-WTOTCONS        TO  WRK-013                      
           MOVE    WRK-013-R           TO  WRK-PADRAO                   
           MOVE    WRK-PADRAO-R        TO  WRK-SAIDA(125:18)            
                                                                        
           WRITE   FD-REG-RELATO       FROM WRK-SAIDA                   
                                                                        
           PERFORM 9070-TESTAR-FS-RELATO THRU 9070-99-FIM.              
                                                                        
       0490-99-FIM.                    EXIT.                            
                                                                        
      *----------------------------------------------------------------*
      *                    ROTINAS DE FIM E LEITURA                    *
      *----------------------------------------------------------------*
                                                                        
      *================================================================*
       0500-FIMSORT.                                                    
      *================================================================*
                                                                        
           IF      WRK-WTEMSORT        NOT EQUAL '*'                    
                   GO TO 0510-TESTE                                     
           END-IF                                                       
                                                                        
           MOVE    X'99999F9999999FF9999F' TO  WRK-STCHAVE              
                                                                        
           GO TO   0140-CONFRONT.                                       
                                                                        
      *================================================================*
       0510-TESTE.                                                      
      *================================================================*
                                                                        
           IF      WRK-CHVLPCL         EQUAL X'99999F9999999FF9999F'    
                   GO TO 0360-RETORNAR                                  
           END-IF                                                       
                                                                        
           IF      WRK-WSUBCPEN        NOT EQUAL '*'                    
                   GO TO 0540-VEPOS80                                   
           END-IF.                                                      
                                                                        
      *================================================================*
       0520-NOP2.                                                       
      *================================================================*
                                                                        
           IF      WRK-FLAG2           EQUAL 'S'                        
                   GO TO 0530-GVREST                                    
           END-IF                                                       
                                                                        
           MOVE    'S'                 TO  WRK-FLAG2                    
           MOVE    X'FF'               TO  WRK-INCONTR.                 
                                                                        
      *================================================================*
       0530-GVREST.                                                     
      *================================================================*
                                                                        
           WRITE   FD-REG-LPCLOUT      FROM WRK-REGLPCL                 
                                                                        
           MOVE    WRK-GRAVACAO        TO  WRK-OPERACAO                 
                                                                        
           PERFORM 9050-TESTAR-FS-LPCLOUT THRU 9050-99-FIM              
                                                                        
           PERFORM 0550-LERLPLIN       THRU 0590-99-FIM                 
                                                                        
           GO TO   0510-TESTE.                                          
                                                                        
      *================================================================*
       0540-VEPOS80.                                                    
      *================================================================*
                                                                        
           IF      WRK-INCONTR         NOT EQUAL SPACES                 
                   GO TO 0360-RETORNAR                                  
           END-IF                                                       
                                                                        
           GO TO   0530-GVREST.                                         
                                                                        
      *                                                                 
      *           LEITURA DO ARQUIVO  "LPCLIN"                          
      *                                                                 
                                                                        
      *================================================================*
       0550-LERLPLIN.                                                   
      *================================================================*
                                                                        
           READ    LPCLIN              INTO WRK-REGLPCL END-READ        
                                                                        
           MOVE    WRK-LEITURA         TO  WRK-OPERACAO                 
                                                                        
           IF      WRK-FS-LPCLIN       EQUAL '10'                       
                   GO TO 0580-FIMLPCL                                   
           ELSE                                                         
                   PERFORM 9040-TESTAR-FS-LPCLIN THRU 9040-99-FIM       
           END-IF                                                       
                                                                        
CPMCAC     MOVE    WRK-INEMPR-RR       TO  WRK-INEMPR-RR                
           MOVE    WRK-INPRELP         TO  WRK-AUXPRELP                 
                                                                        
           IF      WRK-INIDENT         EQUAL 'LP'                       
                   GO TO 0560-LIMPAXX                                   
           END-IF                                                       
                                                                        
           IF      WRK-INEMPR-R        NOT EQUAL X'05240F'              
                   GO TO 0570-PULXXX                                    
           END-IF.                                                      
                                                                        
      *================================================================*
       0560-LIMPAXX.                                                    
      *================================================================*
                                                                        
           MOVE    SPACES              TO  WRK-INPRELP.                 
                                                                        
      *================================================================*
       0570-PULXXX.                                                     
      *================================================================*
                                                                        
CPMCAC     MOVE    WRK-INNROCON-RR     TO  WRK-INNROCON-RR              
CPMCAC     MOVE    WRK-INCART-RR       TO  WRK-INCART-RR                
CPMCAC     MOVE    WRK-INAGEN-RR       TO  WRK-INAGEN-RR                
           MOVE    WRK-INAGEN-R        TO  WRK-CHVLPCL(1:3)             
           MOVE    WRK-INNROCON-R      TO  WRK-CHVLPCL(4:4)             
           MOVE    WRK-INDIGCON        TO  WRK-CHVLPCL(8:1)             
BRQ=A      MOVE    WRK-INCARTA         TO  WRK-CHVLPCL(9:3)
                                                                        
           MOVE    '*'                 TO  WRK-WTEMLPIN                 
                                                                        
           IF      WRK-INCARTA         NOT EQUAL '440'                  
                   GO TO 0590-99-FIM                                    
           END-IF                                                       
                                                                        
           MOVE    '422'               TO  WRK-INCARTA                  
                                                                        
           GO TO   0590-99-FIM.                                         
                                                                        
      *================================================================*
       0580-FIMLPCL.                                                    
      *================================================================*
                                                                        
           IF      WRK-WTEMLPIN        EQUAL '*'                        
                   GO TO 0590-OK                                        
           END-IF                                                       
                                                                        
           CLOSE   LPCLIN                                               
                   LPCLOUT                                              
                   BAIXALP                                              
                                                                        
           MOVE    WRK-FECHAMENTO      TO  WRK-OPERACAO                 
                                                                        
           PERFORM 9040-TESTAR-FS-LPCLIN  THRU 9040-99-FIM              
           PERFORM 9050-TESTAR-FS-LPCLOUT THRU 9050-99-FIM              
           PERFORM 9060-TESTAR-FS-BAIXALP THRU 9060-99-FIM              
                                                                        
           MOVE    004                 TO  RETURN-CODE                  
                                                                        
           GOBACK.                                                      
                                                                        
      *================================================================*
       0590-OK.                                                         
      *================================================================*
                                                                        
           MOVE    X'99999F9999999FF9999F' TO  WRK-CHVLPCL.             
                                                                        
       0590-99-FIM.                    EXIT.                            
                                                                        
      *----------------------------------------------------------------*
       9000-TESTAR-FILE-STATUS.                                         
      *----------------------------------------------------------------*
                                                                        
           PERFORM 9010-TESTAR-FS-ARQDATA  THRU 9010-99-FIM             
           PERFORM 9020-TESTAR-FS-BXMORA   THRU 9020-99-FIM             
           PERFORM 9030-TESTAR-FS-SUBCPEN  THRU 9030-99-FIM             
           PERFORM 9040-TESTAR-FS-LPCLIN   THRU 9040-99-FIM             
           PERFORM 9050-TESTAR-FS-LPCLOUT  THRU 9050-99-FIM             
           PERFORM 9060-TESTAR-FS-BAIXALP  THRU 9060-99-FIM             
           PERFORM 9070-TESTAR-FS-RELATO   THRU 9070-99-FIM.            
                                                                        
       9000-99-FIM.                    EXIT.                            
                                                                        
      *----------------------------------------------------------------*
       9010-TESTAR-FS-ARQDATA.                                          
      *----------------------------------------------------------------*
                                                                        
           IF (WRK-FS-ARQDATA          NOT EQUAL '00')                  
               MOVE 'ARQDATA'          TO WRK-NOME-ARQ                  
               MOVE WRK-FS-ARQDATA     TO WRK-FILE-STATUS               
               MOVE WRK-ERRO-BRAD7100  TO ERR-TEXTO                     
               PERFORM 9999-PROCESSAR-ROTINA-ERRO                       
           END-IF.                                                      
                                                                        
       9010-99-FIM.                    EXIT.                            
                                                                        
      *----------------------------------------------------------------*
       9020-TESTAR-FS-BXMORA.                                           
      *----------------------------------------------------------------*
                                                                        
           IF (WRK-FS-BXMORA           NOT EQUAL '00' AND '04')         
               MOVE 'BXMORA'           TO WRK-NOME-ARQ                  
               MOVE WRK-FS-BXMORA      TO WRK-FILE-STATUS               
               MOVE WRK-ERRO-BRAD7100  TO ERR-TEXTO                     
               PERFORM 9999-PROCESSAR-ROTINA-ERRO                       
           END-IF.                                                      
                                                                        
       9020-99-FIM.                    EXIT.                            
                                                                        
      *----------------------------------------------------------------*
       9030-TESTAR-FS-SUBCPEN.                                          
      *----------------------------------------------------------------*
                                                                        
           IF (WRK-FS-SUBCPEN          NOT EQUAL '00' AND '04')         
               MOVE 'SUBCPEN'          TO WRK-NOME-ARQ                  
               MOVE WRK-FS-SUBCPEN     TO WRK-FILE-STATUS               
               MOVE WRK-ERRO-BRAD7100  TO ERR-TEXTO                     
               PERFORM 9999-PROCESSAR-ROTINA-ERRO                       
           END-IF.                                                      
                                                                        
       9030-99-FIM.                    EXIT.                            
                                                                        
      *----------------------------------------------------------------*
       9040-TESTAR-FS-LPCLIN.                                           
      *----------------------------------------------------------------*
                                                                        
           IF (WRK-FS-LPCLIN           NOT EQUAL '00')                  
               MOVE 'LPCLIN'           TO WRK-NOME-ARQ                  
               MOVE WRK-FS-LPCLIN      TO WRK-FILE-STATUS               
               MOVE WRK-ERRO-BRAD7100  TO ERR-TEXTO                     
               PERFORM 9999-PROCESSAR-ROTINA-ERRO                       
           END-IF.                                                      
                                                                        
       9040-99-FIM.                    EXIT.                            
                                                                        
      *----------------------------------------------------------------*
       9050-TESTAR-FS-LPCLOUT.                                          
      *----------------------------------------------------------------*
                                                                        
           IF (WRK-FS-LPCLOUT          NOT EQUAL '00')                  
               MOVE 'LPCLOUT'          TO WRK-NOME-ARQ                  
               MOVE WRK-FS-LPCLOUT     TO WRK-FILE-STATUS               
               MOVE WRK-ERRO-BRAD7100  TO ERR-TEXTO                     
               PERFORM 9999-PROCESSAR-ROTINA-ERRO                       
           END-IF.                                                      
                                                                        
       9050-99-FIM.                    EXIT.                            
                                                                        
      *----------------------------------------------------------------*
       9060-TESTAR-FS-BAIXALP.                                          
      *----------------------------------------------------------------*
                                                                        
           IF (WRK-FS-BAIXALP          NOT EQUAL '00')                  
               MOVE 'BAIXALP'          TO WRK-NOME-ARQ                  
               MOVE WRK-FS-BAIXALP     TO WRK-FILE-STATUS               
               MOVE WRK-ERRO-BRAD7100  TO ERR-TEXTO                     
               PERFORM 9999-PROCESSAR-ROTINA-ERRO                       
           END-IF.                                                      
                                                                        
       9060-99-FIM.                    EXIT.                            
                                                                        
      *----------------------------------------------------------------*
       9070-TESTAR-FS-RELATO.                                           
      *----------------------------------------------------------------*
                                                                        
           IF (WRK-FS-RELATO           NOT EQUAL '00')                  
               MOVE 'RELATO'           TO WRK-NOME-ARQ                  
               MOVE WRK-FS-RELATO      TO WRK-FILE-STATUS               
               MOVE WRK-ERRO-BRAD7100  TO ERR-TEXTO                     
               PERFORM 9999-PROCESSAR-ROTINA-ERRO                       
           END-IF.                                                      
                                                                        
       9070-99-FIM.                    EXIT.                            
                                                                        
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO.                                      
      *----------------------------------------------------------------*
                                                                        
           MOVE 'CLLP4200'             TO ERR-PGM                       
           MOVE 'APL'                  TO ERR-TIPO-ACESSO               
                                                                        
           CALL 'BRAD7100'             USING WRK-BATCH                  
                                             ERRO-AREA.                 
                                                                        
           GOBACK.                                                      
                                                                        
      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*

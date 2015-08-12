      *===============================================================* 
       IDENTIFICATION DIVISION.                                         
      *===============================================================* 
                                                                        
       PROGRAM-ID. CLLP7626.                                            
       AUTHOR.     ALINE DE NORONHA ABRAO.                              
                                                                        
      *===============================================================* 
      *                   C P M   S I S T E M A S                     * 
      *---------------------------------------------------------------* 
      *                                                               * 
      *      PROGRAMA     : CLLP7626                                  * 
      *      PROGRAMADOR  : ALINE DE NORONHA ABRAO                    * 
      *      SUPERVISOR   : SIDNEI                                    * 
      *      ANALISTA     : LOURIVAL         GP. 82                   * 
      *      DATA         : 13/10/1997                                * 
      *                                                               * 
      *      OBJETIVO     :                                           * 
      *        LER ARQUIVO DE SPC E CRIAR ARQUIVOS DE AGENCIA E       * 
      *        CORREIO                                                * 
      *                                                               * 
      *                                                               * 
      *      ARQUIVOS:                                                * 
      *         DDNAME                           INCLUDE/BOOK         * 
      *         CADCARTA                           I#CLLPLE           * 
      *         CONTMVTO                           I#CLLPGP           * 
      *         CADCEP                             --------           * 
      *         AGENC01                            --------           * 
      *         CORRE01                            --------           * 
      *         RELATO                             --------           * 
      *                                                               * 
      *---------------------------------------------------------------* 
CPM   *                        A L T E R A C A O                      * 
CPM   *---------------------------------------------------------------* 
CPM   *                                                               * 
CPM   *      PROGRAMADOR  : MAURICIO ROQUE         - CPM              * 
CPM   *      ANALISTA     : MAURICIO ROQUE         - F.(11)4196-0570  * 
CPM   *      DATA         : 11/06/2007                                * 
CPM   *                                                               * 
CPM   *      OBJETIVO     : VERIFICAR SE AVALISTA EH CORRENTISTA E    * 
CPM   *                     NAO CORRENTISTA, INCLUSAO DO ARQUIVO      * 
CPM   *                     CONTROLE DE MOVIMENTO.                    * 
CPM   *                                                               * 
CPM   *===============================================================* 
HEXA  *                                                                 
HEXA  *----------------------------------------------------------------*
HEXA  * ***  HEXASOLUTION - 07/2009 - CONVERSAO FAIXA DE AGENCIAS  *** *
HEXA  *----------------------------------------------------------------*
HEXA  *                                                                 
HEXA  *----------------------------------------------------------------*
HEXA  *                       TEMPLATE_OCCURS                          *
HEXA  *----------------------------------------------------------------*
HEXA  *  TABELA: WRK-CAMPOS                                            *
HEXA  *  AREA DE EXPANSAO: NAO                                         *
HEXA  *  INDEXADOR: NAO-EXISTE                                         *
HEXA  *  SUBSCRITOR: AUX-AGENCIA/WRK-IND                               *
HEXA  *                                                                 
      *================================================================*
      *               B R Q     I T     S E R V I C E S                *
      *================================================================*
      *                       A L T E R A C A O                        *
      *----------------------------------------------------------------*
      *    PROGRAMADOR.:  HENRIQUE GUIMARAES      - BRQ                *
      *    ANALISTA....:  HENRIQUE GUIMARAES      - BRQ                *
      *    DATA........:  09/04/2014                                   *
      *----------------------------------------------------------------*
      *    OBJETIVO....:  ADAPTACOES PARA A LEI DA TRANSPARENCIA       *
      *                   TROCA DA BOOK I#CLLPLE POR I#CLLPLJ.         *
      *                   AUMENTO DAS SAIDAS DE 600 PARA 1290.         *
      *    PROJETO 13-0358                                             *
      *================================================================*
                                                                        
      *===============================================================* 
       ENVIRONMENT DIVISION.                                            
      *===============================================================* 
                                                                        
      *---------------------------------------------------------------* 
       CONFIGURATION  SECTION.                                          
      *---------------------------------------------------------------* 
                                                                        
       SPECIAL-NAMES.                                                   
           DECIMAL-POINT IS COMMA.                                      
                                                                        
           EJECT                                                        
                                                                        
      *---------------------------------------------------------------* 
       INPUT-OUTPUT SECTION.                                            
      *---------------------------------------------------------------* 
                                                                        
       FILE-CONTROL.                                                    
                                                                        
                                                                        
           SELECT    CADCARTA ASSIGN TO UT-S-CADCARTA                   
                     FILE STATUS IS WRK-FS-CADCARTA.                    
                                                                        
           SELECT    CADCEP ASSIGN TO UT-S-CADCEP                       
                     FILE STATUS IS WRK-FS-CADCEP.                      
                                                                        
           SELECT   ARQDATA  ASSIGN TO UT-S-ARQDATA                     
                      FILE STATUS IS WRK-FS-ARQDATA.                    
                                                                        
           SELECT    AGENC01 ASSIGN TO UT-S-AGENC01                     
                     FILE STATUS IS WRK-FS-AGENC01.                     
                                                                        
           SELECT    CORRE01 ASSIGN TO UT-S-CORRE01                     
                     FILE STATUS IS WRK-FS-CORRE01.                     
                                                                        
           SELECT    AGENDIA ASSIGN TO UT-S-AGENDIA                     
                     FILE STATUS IS WRK-FS-AGENDIA.                     
                                                                        
           SELECT    MVAVISO ASSIGN TO UT-S-MVAVISO                     
                     FILE STATUS IS WRK-FS-MVAVISO.                     
                                                                        
CPMMRS     SELECT    CONTMVTO ASSIGN TO UT-S-CONTMVTO                   
CPMMRS               FILE STATUS IS WRK-FS-CONTMVTO.                    
                                                                        
           SELECT    RELATO  ASSIGN TO UT-S-RELATO                      
                     FILE STATUS IS WRK-FS-RELATO.                      
                                                                        
           EJECT                                                        
      *===============================================================* 
       DATA DIVISION.                                                   
      *===============================================================* 
                                                                        
      *---------------------------------------------------------------* 
       FILE SECTION.                                                    
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
      *    INPUT:  ORGANIZACAO SEQUENCIAL  LRECL: 1290                * 
      *---------------------------------------------------------------* 
                                                                        
       FD  CADCARTA                                                     
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
-INC I#CLLPLJ                                                           
                                                                        
           EJECT                                                        
      *---------------------------------------------------------------* 
      *   INPUT :  ARQUIVO DE CEPS      LRECL : 200                   * 
      *---------------------------------------------------------------* 
                                                                        
       FD  CADCEP                                                       
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  REG-CADCEP.                                                  
           03 CAD-CEP                  PIC 9(05)  COMP-3.               
           03 CAD-SUFIXO               PIC 9(03).                       
           03 FILLER                   PIC X(63).                       
           03 CAD-NOME-CID             PIC X(30).                       
           03 CAD-UF                   PIC X(02).                       
           03 CAD-COD-ENTR             PIC 9(01).                       
           03 FILLER                   PIC X(07).                       
           03 CAD-CDD                  PIC X(35).                       
           03 CAD-COD-DEP              PIC 9(07)  COMP-3.               
           03 CAD-NOME-DEP             PIC X(20).                       
           03 FILLER                   PIC X(32).                       
                                                                        
           EJECT                                                        
      *---------------------------------------------------------------* 
      *    INPUT:    ARQUIVO DATA DO MOVIMENTO                        * 
      *               ORG. SEQUENCIAL   -   LRECL = 060               * 
      *---------------------------------------------------------------* 
                                                                        
       FD          ARQDATA                                              
                   RECORDING MODE     IS F                              
                   LABEL     RECORD   IS STANDARD                       
                   BLOCK     CONTAINS 0  RECORDS.                       
                                                                        
-INC I#CLLPGA                                                           
                                                                        
           EJECT                                                        
CPMMRS*---------------------------------------------------------------* 
      *    OUTPUT:   CONTROLE DO MOVIMENTO ENVIADO PARA BMK           * 
      *               ORG. SEQUENCIAL   -   LRECL = 100               * 
      *---------------------------------------------------------------* 
                                                                        
       FD          CONTMVTO                                             
                   RECORDING MODE     IS F                              
                   LABEL     RECORD   IS STANDARD                       
                   BLOCK     CONTAINS 0  RECORDS.                       
                                                                        
-INC I#CLLPPG                                                           
                                                                        
           EJECT                                                        
                                                                        
      *---------------------------------------------------------------* 
      *    OUTPUT:  ORGANIZACAO SEQUENCIAL  LRECL :  1290             * 
      *---------------------------------------------------------------* 
                                                                        
       FD  AGENC01                                                      
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  REG-AGENC01             PIC X(1290).                         
                                                                        
           EJECT                                                        
                                                                        
      *---------------------------------------------------------------* 
      *    OUTPUT:  ORGANIZACAO SEQUENCIAL  LRECL :  1290             * 
      *---------------------------------------------------------------* 
                                                                        
       FD  CORRE01                                                      
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  REG-CORRE01             PIC X(1290).                         
                                                                        
           EJECT                                                        
                                                                        
      *---------------------------------------------------------------* 
      *    OUTPUT:  ORGANIZACAO SEQUENCIAL  LRECL :  050              * 
      *---------------------------------------------------------------* 
                                                                        
       FD  AGENDIA                                                      
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  REG-AGENDIA.                                                 
           03  REG-AGENDIA-AGENCIA     PIC 9(05) COMP-3.                
           03  REG-AGENDIA-QTDEAVISOS  PIC 9(07) COMP-3.                
           03  REG-AGENDIA-TOTALAVISOS PIC 9(15)V99 COMP-3.             
           03  REG-AGENDIA-MOVIMENTO   PIC 9(09) COMP-3.                
           03  REG-AGENDIA-FILLER      PIC X(29).                       
           EJECT                                                        
                                                                        
003320*---------------------------------------------------------------* 
003330*    OUTPUT:  ORGANIZACAO SEQUENCIAL  LRECL :  096              * 
003340*---------------------------------------------------------------* 
003350                                                                  
003360 FD  MVAVISO                                                      
003370     RECORDING MODE IS F                                          
003380     LABEL RECORD IS STANDARD                                     
003390     BLOCK CONTAINS 0 RECORDS.                                    
003400                                                                  
-INC I#CLLPA3                                                           
003490                                                                  
                                                                        
      *---------------------------------------------------------------* 
      *    OUTPUT:  RELATORIO DE TOTAIS     LRECL :  132              * 
      *---------------------------------------------------------------* 
                                                                        
       FD  RELATO                                                       
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  REG-RELATO              PIC X(132).                          
                                                                        
           EJECT                                                        
                                                                        
      *---------------------------------------------------------------* 
      *    I-O:       ARQUIVO TEMPORARIO DE SORT                      * 
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       WORKING-STORAGE SECTION.                                         
      *---------------------------------------------------------------* 
                                                                        
       77  FILLER                      PIC X(32)        VALUE           
           '* INICIO DA WORKING CLLP7626 *'.                            
                                                                        
       77  WRK-ABEND                   PIC S9(04) COMP  VALUE +1111.    
       77  WRK-AGENCIA-ANT             PIC 9(05) COMP-3 VALUE ZEROS.    
                                                                        
      *---------------------------------------------------------------* 
       01  ACUMULADORES.                                                
           03  ACU-LIDOS               PIC 9(09) COMP-3 VALUE ZEROS.    
           03  ACU-GRV-AGENC01         PIC 9(09) COMP-3 VALUE ZEROS.    
           03  ACU-NGRV-AGENC01        PIC 9(09) COMP-3 VALUE ZEROS.    
           03  ACU-GRV-AGENDIA         PIC 9(09) COMP-3 VALUE ZEROS.    
           03  ACU-GRV-MVAVISO         PIC 9(09) COMP-3 VALUE ZEROS.    
           03  ACU-GRV-CORRE01         PIC 9(09) COMP-3 VALUE ZEROS.    
CPMMRS     03  ACU-GRV-CONTMVTO        PIC 9(09) COMP-3 VALUE ZEROS.    
           03  ACU-NGRV-CORRE01        PIC 9(09) COMP-3 VALUE ZEROS.    
                                                                        
      *---------------------------------------------------------------* 
       01  CPOS-FILE-STATUS.                                            
           03  WRK-FS-CADCARTA         PIC X(02) VALUE  SPACES.         
           03  WRK-FS-CADCEP           PIC X(02) VALUE  SPACES.         
           03  WRK-FS-ARQDATA          PIC X(02)  VALUE SPACES.         
           03  WRK-FS-AGENC01          PIC X(02) VALUE  SPACES.         
           03  WRK-FS-AGENDIA          PIC X(02) VALUE  SPACES.         
           03  WRK-FS-MVAVISO          PIC X(02) VALUE  SPACES.         
CPMMRS     03  WRK-FS-CONTMVTO         PIC X(02) VALUE  SPACES.         
           03  WRK-FS-CORRE01          PIC X(02) VALUE  SPACES.         
           03  WRK-FS-RELATO           PIC X(02) VALUE  SPACES.         
           03  WRK-OPERACAO            PIC X(13) VALUE  SPACES.         
           03  WRK-ABERTURA            PIC X(13) VALUE 'NA ABERTURA'.   
           03  WRK-LEITURA             PIC X(13) VALUE 'NA LEITURA'.    
           03  WRK-GRAVACAO            PIC X(13) VALUE 'NA GRAVACAO'.   
           03  WRK-FECHAMENTO          PIC X(13) VALUE 'NO FECHAMENTO'. 
                                                                        
      *---------------------------------------------------------------* 
       01  WRK-POOL7600.                                                
           03  FILLER                  PIC 9(05)  COMP-3  VALUE  ZEROS. 
           03  FILLER                  PIC 9(07)  COMP-3  VALUE  ZEROS. 
           03  WRK-DATA                PIC 9(09)  COMP-3  VALUE  ZEROS. 
           03  FILLER                  PIC 9(07)  COMP-3  VALUE  ZEROS. 
           03  FILLER                  PIC 9(13)  COMP-3  VALUE  ZEROS. 
           03  FILLER                  PIC X(20)          VALUE  SPACES.
                                                                        
       01  WRK-FLIAL-CGCCPF            PIC 9(05)          VALUE  ZEROS. 
       01  WRK-FLIAL-CGCCPF-R  REDEFINES   WRK-FLIAL-CGCCPF.            
           03  FILLER                  PIC 9(01).                       
           03  WRK-FLIAL-CGCCPF-4      PIC 9(04).                       
                                                                        
       01  WRK-CTR-CGCCPF              PIC 9(03)          VALUE  ZEROS. 
       01  WRK-CTR-CGCCPF-R  REDEFINES   WRK-CTR-CGCCPF.                
           03  FILLER                  PIC 9(01).                       
           03  WRK-CTR-CGCCPF-2        PIC 9(02).                       
                                                                        
       01  WRK-DATA-AUX                PIC 9(08)          VALUE  ZEROS. 
       01  WRK-DATA-AUX-R  REDEFINES   WRK-DATA-AUX.                    
           03  CB2-ANO                 PIC 9(04).                       
           03  CB2-MES                 PIC 9(02).                       
           03  CB2-DIA                 PIC 9(02).                       
                                                                        
       01  WRK-AUX-DTMOV               PIC 9(08)          VALUE  ZEROS. 
       01  WRK-AUX-DTMOV-R  REDEFINES   WRK-AUX-DTMOV.                  
           03  WRK-DTMOV-DIA           PIC 9(02).                       
           03  WRK-DTMOV-MES           PIC 9(02).                       
           03  WRK-DTMOV-ANO           PIC 9(04).                       
                                                                        
       01  WRK-DATA-AVISO.                                              
           03  WRK-DIA-AVISO           PIC 9(02).                       
           03  FILLER                  PIC X(01) VALUE '.'.             
           03  WRK-MES-AVISO           PIC 9(02).                       
           03  FILLER                  PIC X(01) VALUE '.'.             
           03  WRK-ANO-AVISO           PIC 9(04).                       
                                                                        
       01  WRK-ARQ-AGENDIA.                                             
HEXA  **********   INICIO      TEMPLATE_OCCURS                          
HEXA  ***  ALTERADO PARA SUPORTE A MULTIPLAS FAIXAS DE AGENCIA          
HEXA  *                                                                 
HEXA  *    03  WRK-CAMPOS  OCCURS 5000 TIMES.                           
HEXA  *                                                                 
HEXA       03  WRK-CAMPOS  OCCURS 9999 TIMES.                           
HEXA  **********   FIM         TEMPLATE_OCCURS                          
               05  WRK-AGENDIA-QTDEAVISOS  PIC 9(07) COMP-3 VALUE ZEROS.
               05  WRK-AGENDIA-TOTALAVISOS                              
                                        PIC 9(15)V99 COMP-3 VALUE ZEROS.
                                                                        
       01  WRK-IND                    PIC 9(04) VALUE ZEROS.            
       01  WRK-IND1                   PIC 9(03) COMP-3 VALUE ZEROS.     
       01  WRK-VALOR-RESGATE          PIC 9(15)V99 COMP-3 VALUE ZEROS.  
       01  WRK-SAI                    PIC X(01) VALUE SPACES.           
                                                                        
           EJECT                                                        
                                                                        
       01  AUX-CHAVE-CGC.                                               
           05  FILLER      PIC 9(09)    COMP-3  VALUE  ZEROS.           
           05  FILLER      PIC 9(05)    COMP-3  VALUE  ZEROS.           
           05  FILLER      PIC 9(03)    COMP-3  VALUE  ZEROS.           
                                                                        
       01  AUX-AGENCIA     PIC 9(05)    COMP-3  VALUE  ZEROS.           
                                                                        
       01  WRK-CGC.                                                     
           02 WRK-NUMCGC                  PIC 9(09).                    
           02 WRK-FILCGC                  PIC 9(05).                    
           02 WRK-CTRCGC                  PIC 9(03).                    
       01  WRK-CGC-N                      REDEFINES WRK-CGC PIC 9(17).  
      *---------------------------------------------------------------* 
      *                     LINHAS DE CABECALHOS                      * 
      *---------------------------------------------------------------* 
                                                                        
       01  CABEC1.                                                      
           03  CB1-CARRO     PIC X(01)  VALUE '-'.                      
           03  FILLER        PIC X(35)  VALUE 'CLLP7626'.               
           03  FILLER        PIC X(19)  VALUE 'BANCO BRADESCO S.A.'.    
                                                                        
       01  CABEC2.                                                      
           03  CB2-CARRO     PIC X(01)  VALUE '-'.                      
           03  CB2-DATA.                                                
               05  CB2-DIA   PIC 9(02)/.                                
               05  CB2-MES   PIC 9(02)/.                                
               05  CB2-ANO   PIC 9(04).                                 
           03  FILLER        PIC X(02)  VALUE SPACES.                   
           03  FILLER        PIC X(23) VALUE 'SELECAO PARA A EMISSAO '. 
           03  FILLER        PIC X(24) VALUE 'DE AVISOS DE COBRANCA A '.
           03  FILLER        PIC X(29) VALUE                            
                                      'SEREM ENVIADOS AOS AVALISTAS '.  
           03  FILLER        PIC X(20)  VALUE '- DATA DE MOVIMENTO '.   
           03  CB2-DIA-MOV   PIC 9(02).                                 
           03  FILLER        PIC X(01)  VALUE '/'.                      
           03  CB2-MES-MOV   PIC 9(02).                                 
           03  FILLER        PIC X(01)  VALUE '/'.                      
           03  CB2-ANO-MOV   PIC 9(04).                                 
                                                                        
       01  CABEC3.                                                      
           03  CB3-CARRO     PIC X(01)  VALUE '-'.                      
           03  FILLER        PIC X(31)  VALUE SPACES.                   
           03  FILLER        PIC X(22)  VALUE 'T O T A L I Z A D O R'.  
           03  FILLER        PIC X(03)  VALUE 'E S'.                    
                                                                        
      *---------------------------------------------------------------* 
      *                        LINHAS DE TOTAIS                       * 
      *---------------------------------------------------------------* 
                                                                        
       01  LINTOT1.                                                     
           03  LT1-CARRO     PIC X(01)  VALUE '-'.                      
           03  FILLER        PIC X(25)  VALUE SPACES.                   
           03  FILLER        PIC X(21)  VALUE 'TOTAL DE REGISTROS LI'.  
           03  FILLER        PIC X(23)  VALUE 'DOS                   :'.
           03  LT1-TOT-LIDOS PIC ZZZ.ZZZ.ZZ9.                           
                                                                        
       01  LINTOT2.                                                     
           03  LT2-CARRO     PIC X(01)  VALUE '-'.                      
           03  FILLER        PIC X(25)  VALUE SPACES.                   
           03  FILLER        PIC X(21)  VALUE 'TOTAL DE REGISTROS GR'.  
           03  FILLER        PIC X(23)  VALUE 'AVADOS AGENCIA        :'.
           03  LT2-AGENC01   PIC ZZZ.ZZZ.ZZ9.                           
                                                                        
       01  LINTOT3.                                                     
           03  LT3-CARRO     PIC X(01)  VALUE '-'.                      
           03  FILLER        PIC X(25)  VALUE SPACES.                   
           03  FILLER        PIC X(21)  VALUE 'TOTAL DE REGISTROS NA'.  
           03  FILLER        PIC X(23)  VALUE 'O GRAVADOS AGENCIA    :'.
           03  LT3-AGENC01   PIC ZZZ.ZZZ.ZZ9.                           
                                                                        
       01  LINTOT5.                                                     
           03  LT5-CARRO     PIC X(01)  VALUE '-'.                      
           03  FILLER        PIC X(25)  VALUE SPACES.                   
           03  FILLER        PIC X(21)  VALUE 'TOTAL DE REGISTROS GR'.  
           03  FILLER        PIC X(23)  VALUE 'AVADOS CORREIO        :'.
           03  LT5-CORRE01   PIC ZZZ.ZZZ.ZZ9.                           
                                                                        
       01  LINTOT6.                                                     
           03  LT6-CARRO     PIC X(01)  VALUE '-'.                      
           03  FILLER        PIC X(25)  VALUE SPACES.                   
           03  FILLER        PIC X(21)  VALUE 'TOTAL DE REGISTROS NA'.  
           03  FILLER        PIC X(23)  VALUE 'O GRAVADOS CORREIO    :'.
           03  LT6-CORRE01   PIC ZZZ.ZZZ.ZZ9.                           
                                                                        
       01  LINTOT7.                                                     
           03  LT7-CARRO     PIC X(01)  VALUE '-'.                      
           03  FILLER        PIC X(25)  VALUE SPACES.                   
           03  FILLER        PIC X(21)  VALUE 'TOTAL DE REGISTROS GR'.  
           03  FILLER        PIC X(23)  VALUE 'ADOS PARA EMPRESA BMK :'.
           03  LT7-CORRE01   PIC ZZZ.ZZZ.ZZ9.                           
HEXA  *                                                                 
HEXA  *---------------------------------------------------------------* 
HEXA  * AREA DE DECLARACAO PARA SUPORTE A MULTIPLAS FAIXAS DE AGENCIA * 
HEXA  *---------------------------------------------------------------* 
HEXA  *                                                                 
HEXA       COPY I#HEXA01.                                               
HEXA  *                                                                 
HEXA       COPY I#BRAD7C.                                               
HEXA  *                                                                 
                                                                        
           EJECT                                                        
                                                                        
       01  FILLER                      PIC X(32)        VALUE           
           '*  FIM DA WORKING CLLP7626 *'.                              
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       PROCEDURE DIVISION.                                              
      *===============================================================* 
                                                                        
      *---------------------------------------------------------------* 
       00000-INICIAR SECTION.                                           
      *---------------------------------------------------------------* 
HEXA  *                                                                 
HEXA  *---------------------------------------------------------------* 
HEXA  * AREA DE MOVIMENTACAO DA ROTINA MESU E DE ABEND                * 
HEXA  *---------------------------------------------------------------* 
HEXA  *                                                                 
HEXA       MOVE        'MESU9410'  TO          WRK-MODULO-MESUX0.       
HEXA       MOVE        'BRAD7100'  TO          WRK-ABEND-MESUX0.        
HEXA  *                                                                 
                                                                        
           OPEN     INPUT    CADCARTA                                   
                             CADCEP                                     
                             ARQDATA                                    
                    OUTPUT   AGENC01                                    
                             AGENDIA                                    
                             MVAVISO                                    
CPMMRS                       CONTMVTO                                   
                             CORRE01                                    
                             RELATO.                                    
                                                                        
           INITIALIZE WRK-ARQ-AGENDIA.                                  
                                                                        
           MOVE     WRK-ABERTURA         TO   WRK-OPERACAO              
           PERFORM  10000-TESTAR-FILE-STATUS                            
                                                                        
           PERFORM  20000-VERIFICAR-VAZIO                               
                                                                        
           IF       WRK-FS-CADCARTA  EQUAL  '10'                        
                    PERFORM          80000-FINALIZAR.                   
                                                                        
           PERFORM  25000-LER-ARQDATA                                   
                                                                        
           MOVE     DTMOV              TO        WRK-AUX-DTMOV          
                                                                        
                                                                        
           MOVE     WRK-DTMOV-DIA      TO        CB2-DIA-MOV            
                                                 WRK-DIA-AVISO          
           MOVE     WRK-DTMOV-MES      TO        CB2-MES-MOV            
                                                 WRK-MES-AVISO          
           MOVE     WRK-DTMOV-ANO      TO        CB2-ANO-MOV            
                                                 WRK-ANO-AVISO          
                                                                        
           PERFORM  52000-LER-CADCEP.                                   
                                                                        
           PERFORM  30000-PROCESSA  UNTIL WRK-FS-CADCARTA  EQUAL  '10'. 
                                                                        
           PERFORM  70000-IMPRIMIR-RELATO                               
                                                                        
           PERFORM  80000-FINALIZAR.                                    
                                                                        
                                                                        
       FIM-00000-INICIAR.                                        EXIT.  
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       10000-TESTAR-FILE-STATUS SECTION.                                
      *---------------------------------------------------------------* 
                                                                        
           PERFORM 11000-TESTAR-FS-CADCARTA                             
                                                                        
           PERFORM 13000-TESTAR-FS-CADCEP                               
                                                                        
           PERFORM 13500-TESTAR-FS-ARQDATA                              
                                                                        
           PERFORM 14000-TESTAR-FS-AGENC01                              
                                                                        
           PERFORM 16200-TESTAR-FS-AGENDIA.                             
                                                                        
           PERFORM 16210-TESTAR-FS-MVAVISO.                             
                                                                        
           PERFORM 17000-TESTAR-FS-CORRE01                              
                                                                        
           PERFORM 19000-A-TESTAR-FS-RELATO.                            
                                                                        
           PERFORM 19500-A-TESTAR-FS-CONTMVTO.                          
                                                                        
       FIM-10000-TESTAR-FS.                                      EXIT.  
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       11000-TESTAR-FS-CADCARTA SECTION.                                
      *---------------------------------------------------------------* 
                                                                        
           IF    WRK-FS-CADCARTA  NOT EQUAL '00'                        
                 DISPLAY '************** CLLP7626 *************'        
                 DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'    
                 DISPLAY '*              CADCARTA             *'        
                 DISPLAY '*         FILE STATUS =  ' WRK-FS-CADCARTA    
                                                    '         *'        
                 DISPLAY '************** CLLP7626 *************'        
                 CALL    'ILBOABN0'      USING   WRK-ABEND.             
                                                                        
                                                                        
       FIM-11000-FS-CADCARTA.                                    EXIT.  
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       13000-TESTAR-FS-CADCEP SECTION.                                  
      *---------------------------------------------------------------* 
                                                                        
           IF    WRK-FS-CADCEP  NOT EQUAL '00'                          
                 DISPLAY '************** CLLP7626 *************'        
                 DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'    
                 DISPLAY '*              CADCEP               *'        
                 DISPLAY '*         FILE STATUS =  ' WRK-FS-CADCEP      
                                                    '         *'        
                 DISPLAY '************** CLLP7626 *************'        
                 CALL    'ILBOABN0'      USING   WRK-ABEND.             
                                                                        
                                                                        
       FIM-13000-FS-CADCEP.                                      EXIT.  
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
      *      TESTA FILE ESTATUS DO ARQDATA                              
      *---------------------------------------------------------------* 
      *                                                                 
       13500-TESTAR-FS-ARQDATA  SECTION.                                
                                                                        
           IF    WRK-FS-ARQDATA    NOT EQUAL '00'                       
                 DISPLAY '************** CLLP7626 **************'       
                 DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO    *'   
                 DISPLAY '*              ARQDATA               *'       
                 DISPLAY '*         FILE STATUS =  ' WRK-FS-ARQDATA     
                                                    '          *'       
                 DISPLAY '************** CLLP7626 **************'       
                 CALL 'ILBOABN0'       USING WRK-ABEND.                 
                                                                        
       13500-EXIT. EXIT.                                                
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       14000-TESTAR-FS-AGENC01  SECTION.                                
      *---------------------------------------------------------------* 
                                                                        
           IF    WRK-FS-AGENC01  NOT EQUAL '00' AND '34'                
                 DISPLAY '************** CLLP7626 *************'        
                 DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'    
                 DISPLAY '*              AGENC01              *'        
                 DISPLAY '*         FILE STATUS =  ' WRK-FS-AGENC01     
                                                    '         *'        
                 DISPLAY '************** CLLP7626 *************'        
                 CALL    'ILBOABN0'      USING   WRK-ABEND.             
                                                                        
                                                                        
       FIM-14000-FS-AGENC01.                                     EXIT.  
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       14010-TESTAR-FS-MVAVISO  SECTION.                                
      *---------------------------------------------------------------* 
                                                                        
           IF    WRK-FS-MVAVISO  NOT EQUAL '00'                         
                 DISPLAY '************** CLLP7626 *************'        
                 DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'    
                 DISPLAY '*              MVAVISO              *'        
                 DISPLAY '*         FILE STATUS =  ' WRK-FS-MVAVISO     
                                                    '         *'        
                 DISPLAY '************** CLLP7626 *************'        
                 CALL    'ILBOABN0'      USING   WRK-ABEND.             
                                                                        
                                                                        
       FIM-14010-FS-MVAVISO.                                     EXIT.  
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       16200-TESTAR-FS-AGENDIA SECTION.                                 
      *---------------------------------------------------------------* 
                                                                        
           IF    WRK-FS-AGENDIA NOT EQUAL '00'                          
                 DISPLAY '************** CLLP7626 *************'        
                 DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'    
                 DISPLAY '*              AGENDIA              *'        
                 DISPLAY '*         FILE STATUS =  ' WRK-FS-AGENDIA     
                                                    '         *'        
                 DISPLAY '************** CLLP7626 *************'        
                 CALL    'ILBOABN0'      USING   WRK-ABEND.             
                                                                        
                                                                        
       FIM-16200-FS-AGENDIA.                                     EXIT.  
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       16210-TESTAR-FS-MVAVISO SECTION.                                 
      *---------------------------------------------------------------* 
                                                                        
           IF    WRK-FS-MVAVISO NOT EQUAL '00'                          
                 DISPLAY '************** CLLP7626 *************'        
                 DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'    
                 DISPLAY '*              MVAVISO              *'        
                 DISPLAY '*         FILE STATUS =  ' WRK-FS-MVAVISO     
                                                    '         *'        
                 DISPLAY '************** CLLP7626 *************'        
                 CALL    'ILBOABN0'      USING   WRK-ABEND.             
                                                                        
                                                                        
       FIM-16200-FS-MVAVISO.                                     EXIT.  
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       17000-TESTAR-FS-CORRE01 SECTION.                                 
      *---------------------------------------------------------------* 
                                                                        
           IF    WRK-FS-CORRE01 NOT EQUAL '00' AND '34'                 
                 DISPLAY '************** CLLP7626 *************'        
                 DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'    
                 DISPLAY '*              CORRE01              *'        
                 DISPLAY '*         FILE STATUS =  ' WRK-FS-CORRE01     
                                                    '         *'        
                 DISPLAY '************** CLLP7626 *************'        
                 CALL    'ILBOABN0'      USING   WRK-ABEND.             
                                                                        
       FIM-17000-FS-CORRE01.                                     EXIT.  
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       19000-A-TESTAR-FS-RELATO SECTION.                                
      *---------------------------------------------------------------* 
                                                                        
           IF    WRK-FS-RELATO  NOT EQUAL '00'                          
                 DISPLAY '************** CLLP7626 *************'        
                 DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'    
                 DISPLAY '*              RELATO               *'        
                 DISPLAY '*         FILE STATUS =  ' WRK-FS-RELATO      
                                                    '         *'        
                 DISPLAY '************** CLLP7626 *************'        
                 CALL    'ILBOABN0'      USING   WRK-ABEND.             
                                                                        
                                                                        
       FIM-19000-A-FS-RELATO.                                    EXIT.  
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       19500-A-TESTAR-FS-CONTMVTO  SECTION.                             
      *---------------------------------------------------------------* 
                                                                        
           IF    WRK-FS-CONTMVTO   NOT EQUAL '00'                       
                 DISPLAY '************** CLLP7626 *************'        
                 DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'    
                 DISPLAY '*              CONTMVTO             *'        
                 DISPLAY '*         FILE STATUS =  ' WRK-FS-CONTMVTO    
                                                    '         *'        
                 DISPLAY '************** CLLP7626 *************'        
                 CALL    'ILBOABN0'      USING   WRK-ABEND.             
                                                                        
                                                                        
       FIM-19500-A-FS-CONTMVTO.                                  EXIT.  
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       20000-VERIFICAR-VAZIO SECTION.                                   
      *---------------------------------------------------------------* 
                                                                        
           PERFORM   21000-LER-CADCARTA                                 
                                                                        
           IF  WRK-FS-CADCARTA EQUAL '10'                               
               DISPLAY '*******************************'  UPON CONSOLE  
               DISPLAY '*         SR. OPERADOR        *'  UPON CONSOLE  
               DISPLAY '*       ARQUIVO CADCARTA      *'  UPON CONSOLE  
               DISPLAY '*            VAZIO            *'  UPON CONSOLE  
               DISPLAY '*   PROCESSAMENTO ENCERRADO   *'  UPON CONSOLE  
               DISPLAY '*******************************'  UPON CONSOLE. 
                                                                        
      *---------------------------------------------------------------* 
       FIM-20000-VERIF-VAZIO.                                    EXIT.  
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       21000-LER-CADCARTA SECTION.                                      
      *---------------------------------------------------------------* 
                                                                        
           READ    CADCARTA                                             
                                                                        
           IF      WRK-FS-CADCARTA  EQUAL   '10'                        
                   GO               TO      FIM-21000-LER-CADCARTA.     
                                                                        
           MOVE    WRK-LEITURA      TO      WRK-OPERACAO                
           PERFORM 11000-TESTAR-FS-CADCARTA                             
                                                                        
           ADD         1            TO      ACU-LIDOS.                  
                                                                        
      *---------------------------------------------------------------* 
       FIM-21000-LER-CADCARTA.                                   EXIT.  
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
      *        LEITURA DO ARQUIVO DATA DE MOVIMENTO                     
      *---------------------------------------------------------------* 
                                                                        
       25000-LER-ARQDATA   SECTION.                                     
                                                                        
           READ ARQDATA.                                                
                                                                        
           MOVE WRK-LEITURA            TO     WRK-OPERACAO              
           PERFORM 13500-TESTAR-FS-ARQDATA.                             
                                                                        
       FIM-25000-LER-ARQDATA. EXIT.                                     
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       30000-PROCESSA      SECTION.                                     
      *---------------------------------------------------------------* 
                                                                        
           IF    AVI-CCEP            LESS 1001                          
                 MOVE      2         TO   AVI-TIPO                      
                 MOVE   ZEROS        TO   AVI-COD-ENTR                  
CPMMRS           MOVE   DTMOV        TO   AVI-DATA-MVTO                 
                 PERFORM             51120-GRAVAR-AGENC01               
                 PERFORM             30010-MONTAR-MVAVISO               
                 PERFORM             30020-GRAVAR-MVAVISO               
                 PERFORM             21000-LER-CADCARTA                 
                 GO  TO        FIM-30000-PROCESSA.                      
                                                                        
           IF  AVI-CCEP          NOT EQUAL CAD-CEP                      
               PERFORM        52000-LER-CADCEP  UNTIL                   
                              WRK-FS-CADCEP     EQUAL  '10'             
                        OR    AVI-CCEP          EQUAL   CAD-CEP         
                        OR    CAD-CEP           GREATER AVI-CCEP.       
                                                                        
           IF    CAD-CEP     EQUAL     AVI-CCEP                         
                 PERFORM               53100-CHAVES-IGUAIS              
           ELSE                                                         
                 MOVE  CAD-COD-ENTR TO AVI-COD-ENTR                     
                 MOVE    2       TO    AVI-TIPO                         
CPMMRS           MOVE   DTMOV    TO    AVI-DATA-MVTO                    
                 PERFORM               51120-GRAVAR-AGENC01.            
                                                                        
           PERFORM                     30010-MONTAR-MVAVISO             
                                                                        
           PERFORM                     30020-GRAVAR-MVAVISO             
                                                                        
           PERFORM                     21000-LER-CADCARTA.              
                                                                        
      *---------------------------------------------------------------* 
       FIM-30000-PROCESSA.                          EXIT.               
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       30010-MONTAR-MVAVISO SECTION.                                    
      *---------------------------------------------------------------* 
                                                                        
      *                                                                 
      *    MONTAR MVAVISO                                               
      *                                                                 
           INITIALIZE REG-CLLP.                                         
                                                                        
           MOVE WRK-CGC-N              TO  CLLP-CPF.                    
           MOVE AVI-NOME-DEVEDOR       TO  CLLP-NOME-DEVEDOR.           
           MOVE WRK-DATA-AVISO         TO  CLLP-DATA-EMISSAO-AVISO.     
           MOVE AVI-NATUREZA(01)       TO  CLLP-NATUREZA                
           MOVE AVI-CARTEIRA(01)       TO  CLLP-CARTEIRA.               
           MOVE AVI-CGC-AVAL           TO  WRK-NUMCGC.                  
           MOVE AVI-FIL-AVAL           TO  WRK-FILCGC.                  
           MOVE AVI-CTR-AVAL           TO  WRK-CTRCGC.                  
                                                                        
HEXA  *----------------------------------------------------------------*
HEXA  *            INICIO     DO     TEMPLATE_IF                        
HEXA  *----------------------------------------------------------------*
HEXA  ***  ALTERADO PARA SUPORTE A MULTIPLAS FAIXAS DE AGENCIA          
HEXA  *                                                                 
HEXA  * HX_IF AVI-AGENCIA-AVAL      LESS 4000                           
HEXA  *                                                                 
HEXA       MOVE AVI-AGENCIA-AVAL       TO  WRK-MESUX0-AGENCIA           
HEXA       PERFORM 9990-10-CHAMA-ROTINA-FX-AG                           
HEXA  *                                                                 
HEXA       IF WRK-EH-FX-AG                                              
HEXA  *                                                                 
HEXA  *----------------------------------------------------------------*
HEXA  *            TERMINO    DO     TEMPLATE_IF                        
HEXA  *----------------------------------------------------------------*
HEXA  *                                                                 
CPMMRS         MOVE WRK-CGC-N       TO  CLLP-AVALISTA1                  
CPMMRS         MOVE 'S'             TO  CLLP-AVALISTA1-CORRENTISTA      
CPMMRS         MOVE ZEROS           TO  CLLP-AVALISTA2                  
CPMMRS         MOVE 'S'             TO  CLLP-AVALISTA2-CORRENTISTA      
CPMMRS     ELSE                                                         
CPMMRS         MOVE WRK-CGC-N       TO  CLLP-AVALISTA1                  
CPMMRS         MOVE 'N'             TO  CLLP-AVALISTA1-CORRENTISTA      
CPMMRS         MOVE ZEROS           TO  CLLP-AVALISTA2                  
CPMMRS         MOVE 'N'             TO  CLLP-AVALISTA2-CORRENTISTA.     
                                                                        
           ADD   1                  TO  CLLP-QTDADE-PRI-AVISO-ATUAL.    
                                                                        
      *---------------------------------------------------------------* 
       FIM-30010-MONTAR-MVAVISO.                                        
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       30020-GRAVAR-MVAVISO SECTION.                                    
      *---------------------------------------------------------------* 
                                                                        
           WRITE    REG-CLLP.                                           
                                                                        
           MOVE     WRK-GRAVACAO  TO     WRK-OPERACAO                   
                                                                        
           PERFORM  14010-TESTAR-FS-MVAVISO                             
                                                                        
           ADD      1       TO     ACU-GRV-MVAVISO.                     
                                                                        
       FIM-30020-GRAVAR-MVAVISO.                                 EXIT.  
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       51120-GRAVAR-AGENC01 SECTION.                                    
      *---------------------------------------------------------------* 
                                                                        
           WRITE    REG-AGENC01   FROM   REG-AVISO.                     
                                                                        
           MOVE     WRK-GRAVACAO  TO     WRK-OPERACAO                   
           PERFORM  14000-TESTAR-FS-AGENC01                             
                                                                        
           ADD      1       TO     ACU-GRV-AGENC01.                     
                                                                        
       FIM-51121-GRAVAR-AGENC01.                                 EXIT.  
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       52000-LER-CADCEP SECTION.                                        
      *---------------------------------------------------------------* 
                                                                        
           READ     CADCEP                                              
                                                                        
           IF       WRK-FS-CADCEP     EQUAL  '10'                       
                    MOVE    99999      TO      CAD-CEP                  
                    GO                 TO      FIM-52000-LER-CADCEP.    
                                                                        
           MOVE     WRK-LEITURA        TO      WRK-OPERACAO             
           PERFORM  13000-TESTAR-FS-CADCEP.                             
                                                                        
                                                                        
       FIM-52000-LER-CADCEP.                                     EXIT.  
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       53100-CHAVES-IGUAIS SECTION.                                     
      *---------------------------------------------------------------* 
                                                                        
           IF   CAD-COD-ENTR   EQUAL  1  OR  3                          
                MOVE    2      TO    AVI-TIPO                           
                MOVE   CAD-COD-ENTR  TO  AVI-COD-ENTR                   
CPMMRS          MOVE   DTMOV         TO  AVI-DATA-MVTO                  
                PERFORM              51120-GRAVAR-AGENC01               
           ELSE                                                         
                MOVE    1      TO    AVI-TIPO                           
CPMMRS          MOVE   DTMOV         TO  AVI-DATA-MVTO                  
                MOVE   CAD-COD-ENTR  TO  AVI-COD-ENTR                   
                PERFORM              53110-GRAVAR-MAIOR.                
                                                                        
                                                                        
       FIM-53100-CHAVES-IGUAIS.                                  EXIT.  
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       53110-GRAVAR-MAIOR SECTION.                                      
      *---------------------------------------------------------------* 
                                                                        
                PERFORM 53111-GRAVAR-CONTMVTO                           
                PERFORM 53112-GRAVAR-CORRE01                            
                PERFORM 53115-ACUMULAR-AGENDIA.                         
                                                                        
       FIM-53110-GRAVAR-MAIOR.                                   EXIT.  
      *---------------------------------------------------------------* 
           EJECT                                                        
CPMMRS*---------------------------------------------------------------* 
CPMMRS 53111-GRAVAR-CONTMVTO   SECTION.                                 
CPMMRS*---------------------------------------------------------------* 
CPMMRS     INITIALIZE                 REG-HISTAVIS                      
CPMMRS                                                                  
CPMMRS     MOVE AVI-CGC-AVAL           TO  HIST-NUMCPF.                 
CPMMRS     MOVE AVI-FIL-AVAL           TO  HIST-FILCPF.                 
CPMMRS     MOVE AVI-CTR-AVAL           TO  HIST-CTRCPF.                 
CRUZ       MOVE AVI-CGC                TO  HIST-NUMCPF-DEV.             
CRUZ       MOVE AVI-FIL                TO  WRK-FLIAL-CGCCPF.            
CRUZ       MOVE WRK-FLIAL-CGCCPF-4     TO  HIST-FILCPF-DEV.             
CRUZ       MOVE AVI-CTR                TO  WRK-CTR-CGCCPF.              
CRUZ       MOVE WRK-CTR-CGCCPF-2       TO  HIST-CTRCPF-DEV.             
CPMMRS     MOVE DTMOV                  TO  HIST-DT-MOVTO.               
CPMMRS     MOVE AVI-AGENCIA            TO  HIST-AGENCIA.                
CPMMRS     MOVE 2                      TO  HIST-TPO-CLIENTE.            
CPMMRS     MOVE AVI-NUM-CC             TO  HIST-CTA-CORRENT.            
CPMMRS     MOVE AVI-TPO-LOGRA          TO  HIST-TPO-ENDEREC.            
CPMMRS     MOVE AVI-DTULTATU           TO  HIST-DT-ATULZ.               
           MOVE AVI-CRET-LOGRA         TO  HIST-SIT-LOCALIZ.            
CPMMRS     MOVE AVI-ORIGEM-END         TO  HIST-ORIG-ENDEREC.           
CPMMRS     MOVE AVI-AGENCIA-AVAL       TO  HIST-AGENCIA-AVAL            
CPMMRS     MOVE AVI-CONTA-AVAL         TO  HIST-CTA-CORRENT-AVAL.       
CPMMRS                                                                  
CPMMRS     WRITE    REG-HISTAVIS.                                       
CPMMRS                                                                  
CPMMRS     MOVE     WRK-GRAVACAO       TO  WRK-OPERACAO                 
CPMMRS     PERFORM  19500-A-TESTAR-FS-CONTMVTO.                         
CPMMRS                                                                  
CPMMRS     ADD           1             TO  ACU-GRV-CONTMVTO.            
CPMMRS                                                                  
CPMMRS FIM-53111-GRAVAR-CONTMVTO.                             EXIT.     
CPMMRS*---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       53112-GRAVAR-CORRE01 SECTION.                                    
      *---------------------------------------------------------------* 
                                                                        
           WRITE    REG-CORRE01   FROM   REG-AVISO.                     
                                                                        
           MOVE     WRK-GRAVACAO  TO     WRK-OPERACAO                   
           PERFORM  17000-TESTAR-FS-CORRE01                             
                                                                        
           ADD      1       TO     ACU-GRV-CORRE01.                     
                                                                        
       FIM-53112-GRAVAR-CORRE01.                                 EXIT.  
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       53115-ACUMULAR-AGENDIA SECTION.                                  
      *---------------------------------------------------------------* 
                                                                        
HEXA  *----------------------------------------------------------------*
HEXA  *            INICIO     DO     TEMPLATE_IF                        
HEXA  *----------------------------------------------------------------*
HEXA  ***  ALTERADO PARA SUPORTE A MULTIPLAS FAIXAS DE AGENCIA          
HEXA  *                                                                 
HEXA  * HX_IF  (AVI-AGENCIA  GREATER 5000)                              
HEXA  *                                                                 
HEXA       MOVE     AVI-AGENCIA   TO     WRK-MESUX0-AGENCIA             
HEXA       PERFORM  9990-10-CHAMA-ROTINA-FX-AG                          
HEXA  *                                                                 
HEXA       IF  (WRK-NAO-EH-FX-AG-DEP     )                              
HEXA  *                                                                 
HEXA  *----------------------------------------------------------------*
HEXA  *            TERMINO    DO     TEMPLATE_IF                        
HEXA  *----------------------------------------------------------------*
HEXA  *                                                                 
               GO  TO  FIM-53115-ACU.                                   
                                                                        
           IF  (AUX-CHAVE-CGC  EQUAL  ZEROS)                            
               MOVE    AVI-AGENCIA  TO  AUX-AGENCIA                     
               MOVE  AVI-CHAVE-CGC  TO  AUX-CHAVE-CGC                   
               COMPUTE WRK-AGENDIA-QTDEAVISOS (AUX-AGENCIA) =           
                       WRK-AGENDIA-QTDEAVISOS (AUX-AGENCIA) + 1         
           ELSE                                                         
               IF  (AUX-CHAVE-CGC  NOT  EQUAL  AVI-CHAVE-CGC)           
                    MOVE  AVI-CHAVE-CGC   TO  AUX-CHAVE-CGC             
                    MOVE    AVI-AGENCIA   TO  AUX-AGENCIA               
                    COMPUTE WRK-AGENDIA-QTDEAVISOS (AUX-AGENCIA) =      
                           WRK-AGENDIA-QTDEAVISOS (AUX-AGENCIA) + 1.    
                                                                        
           MOVE ZEROS                  TO WRK-VALOR-RESGATE.            
           PERFORM                                                      
               VARYING WRK-IND1        FROM 1 BY 1 UNTIL                
               WRK-IND1                GREATER 11                       
               ADD AVI-RESGATE(WRK-IND1)                                
                                       TO WRK-VALOR-RESGATE             
           END-PERFORM.                                                 
                                                                        
                                                                        
           COMPUTE WRK-AGENDIA-TOTALAVISOS (AUX-AGENCIA) =              
                   WRK-AGENDIA-TOTALAVISOS (AUX-AGENCIA) +              
                   WRK-VALOR-RESGATE.                                   
                                                                        
       FIM-53115-ACU.                                 EXIT.             
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       70000-IMPRIMIR-RELATO SECTION.                                   
      *---------------------------------------------------------------* 
                                                                        
           PERFORM 71000-TRATAR-DATA                                    
                                                                        
           MOVE    WRK-GRAVACAO     TO     WRK-OPERACAO                 
                                                                        
           MOVE    ACU-LIDOS        TO     LT1-TOT-LIDOS                
           MOVE    ACU-GRV-AGENC01  TO     LT2-AGENC01                  
           MOVE    ACU-GRV-CORRE01  TO     LT5-CORRE01                  
           MOVE    ACU-NGRV-AGENC01 TO     LT3-AGENC01                  
           MOVE    ACU-NGRV-CORRE01 TO     LT6-CORRE01                  
           MOVE    ACU-GRV-CONTMVTO TO     LT7-CORRE01                  
           MOVE    WRK-GRAVACAO     TO     WRK-OPERACAO                 
                                                                        
           WRITE   REG-RELATO   FROM   CABEC1                           
           PERFORM 19000-A-TESTAR-FS-RELATO                             
                                                                        
           WRITE   REG-RELATO   FROM   CABEC2                           
           PERFORM 19000-A-TESTAR-FS-RELATO                             
                                                                        
           PERFORM 72000-PULA-LINHA    3  TIMES                         
                                                                        
           WRITE   REG-RELATO   FROM   CABEC3                           
           PERFORM 19000-A-TESTAR-FS-RELATO                             
                                                                        
           WRITE   REG-RELATO   FROM   LINTOT1                          
           PERFORM 19000-A-TESTAR-FS-RELATO                             
                                                                        
           WRITE   REG-RELATO   FROM   LINTOT2                          
           PERFORM 19000-A-TESTAR-FS-RELATO                             
                                                                        
           WRITE   REG-RELATO   FROM   LINTOT5                          
           PERFORM 19000-A-TESTAR-FS-RELATO.                            
                                                                        
           WRITE   REG-RELATO   FROM   LINTOT7                          
           PERFORM 19000-A-TESTAR-FS-RELATO.                            
                                                                        
       FIM-70000-IMPRIMIR-RELATO.                                EXIT.  
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       71000-TRATAR-DATA SECTION.                                       
      *---------------------------------------------------------------* 
                                                                        
           CALL     'POOL7600'       USING    WRK-POOL7600              
           MOVE      WRK-DATA        TO       WRK-DATA-AUX              
           MOVE CORR WRK-DATA-AUX-R  TO       CB2-DATA.                 
                                                                        
                                                                        
       FIM-71000-TRATAR-DATA.                                    EXIT.  
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       72000-PULA-LINHA SECTION.                                        
      *---------------------------------------------------------------* 
                                                                        
           MOVE      SPACES        TO  REG-RELATO                       
           WRITE     REG-RELATO                                         
                                                                        
           MOVE      WRK-GRAVACAO  TO  WRK-OPERACAO                     
           PERFORM   19000-A-TESTAR-FS-RELATO.                          
                                                                        
                                                                        
       FIM-72000-PULA-LINHA.                                     EXIT.  
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       74000-GRAVAR-AGENDIA SECTION.                                    
      *---------------------------------------------------------------* 
HEXA  *                                                                 
HEXA  *----------------------------------------------------------------*
HEXA  *            INICIO     DO     TEMPLATE_PERFORM -   BLOCO  2      
HEXA  *----------------------------------------------------------------*
HEXA  *                                                                 
HEXA       MOVE    WRK-IND     TO          WRK-MESUX0-AGENCIA           
HEXA       PERFORM 9990-10-CHAMA-ROTINA-FX-AG                           
HEXA  *                                                                 
HEXA       IF      WRK-NAO-EH-FX-AG-DEP                                 
HEXA           GO TO 74000-NAO-EH-FX-AG-DEP                             
HEXA       END-IF.                                                      
HEXA  *                                                                 
HEXA  *----------------------------------------------------------------*
HEXA  *            TERMINO    DO     TEMPLATE_PERFORM  -   BLOCO  2     
HEXA  *----------------------------------------------------------------*
HEXA  *                                                                 
                                                                        
             IF  WRK-AGENDIA-QTDEAVISOS(WRK-IND) NOT EQUAL ZEROS        
                 MOVE  WRK-IND TO REG-AGENDIA-AGENCIA                   
                 MOVE  WRK-AGENDIA-QTDEAVISOS(WRK-IND)  TO              
                       REG-AGENDIA-QTDEAVISOS                           
                 MOVE  WRK-AGENDIA-TOTALAVISOS(WRK-IND) TO              
                       REG-AGENDIA-TOTALAVISOS                          
                 MOVE  WRK-DATA         TO  REG-AGENDIA-MOVIMENTO       
                 MOVE  SPACES           TO  REG-AGENDIA-FILLER          
                 WRITE REG-AGENDIA                                      
                 MOVE  WRK-GRAVACAO     TO  WRK-OPERACAO                
                 PERFORM 16200-TESTAR-FS-AGENDIA                        
                 ADD     1  TO ACU-GRV-AGENDIA.                         
HEXA  *                                                                 
HEXA  * PARAGRAFO PARA SUPORTE A MULTIPLAS FAIXAS DE AGENCIA            
HEXA   74000-NAO-EH-FX-AG-DEP.                                          
HEXA  *                                                                 
                                                                        
                                                                        
       FIM-74000-GRAVA.                                     EXIT.       
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       80000-FINALIZAR SECTION.                                         
      *---------------------------------------------------------------* 
                                                                        
HEXA  *----------------------------------------------------------------*
HEXA  *            INICIO     DO     TEMPLATE_PERFORM -   BLOCO  1      
HEXA  *----------------------------------------------------------------*
HEXA  ***  ALTERADO PARA SUPORTE A MULTIPLAS FAIXAS DE AGENCIA          
HEXA  *                                                                 
HEXA  * HX_PERFORM 74000-GRAVAR-AGENDIA VARYING WRK-IND FROM 1 BY 1     
HEXA  *            UNTIL WRK-IND GREATER 5000.                          
HEXA  *                                                                 
HEXA       MOVE        WRK-ULTIMA-AG-U TO      WRK-HX01-U-5             
HEXA  *                                                                 
HEXA       PERFORM 74000-GRAVAR-AGENDIA VARYING WRK-IND FROM 1 BY 1     
HEXA               UNTIL WRK-IND GREATER WRK-HX01-U-4.                  
HEXA  *                                                                 
HEXA  *----------------------------------------------------------------*
HEXA  *            TERMINO    DO     TEMPLATE_PERFORM -   BLOCO  1      
HEXA  *----------------------------------------------------------------*
HEXA  *                                                                 
                                                                        
                                                                        
           CLOSE        CADCARTA                                        
                        CADCEP                                          
                        ARQDATA                                         
                        AGENC01                                         
                        AGENDIA                                         
                        MVAVISO                                         
CPMMRS                  CONTMVTO                                        
                        CORRE01                                         
                        RELATO                                          
                                                                        
           MOVE         WRK-FECHAMENTO    TO  WRK-OPERACAO              
           PERFORM      10000-TESTAR-FILE-STATUS                        
           STOP RUN.                                                    
                                                                        
                                                                        
       FIM-80000-FINALIZAR.                                      EXIT.  
      *---------------------------------------------------------------* 
           EJECT                                                        
      ******************************************************************
      *                   FIM  DO  PROGRAMA  CLLP7626                  *
      ******************************************************************
HEXA  *----------------------------------------------------------------*
HEXA  * SECTION PARA CHAMADA DA ROTINA DE CONVERSAO DE AGENCIA          
HEXA  *----------------------------------------------------------------*
HEXA  *                                                                 
HEXA  *----------------------------------------------------------------*
HEXA   9990-10-CHAMA-ROTINA-FX-AG SECTION.                              
HEXA  *----------------------------------------------------------------*
HEXA  *                                                                 
HEXA       CALL        WRK-MODULO-MESUX0 USING WRK-AREA-MESUX0.         
HEXA  *                                                                 
HEXA       IF          WRK-EH-ERRO-MESU-ABEND                           
HEXA           MOVE    'APL'       TO          ERR-TIPO-ACESSO          
HEXA           MOVE    'ERRO NA CHAMADA DA ROTINA DE FAIXAS DE AGENCIA' 
HEXA             TO    ERR-TEXTO                                        
HEXA           PERFORM 9991-10-CHAMA-ROTINA-ABEND                       
HEXA       ELSE                                                         
HEXA           MOVE    ZEROS       TO          WRK-MESUX0-AGENCIA       
HEXA       END-IF.                                                      
HEXA  *                                                                 
HEXA  *----------------------------------------------------------------*
HEXA   9990-90-EXIT. EXIT.                                              
HEXA  *----------------------------------------------------------------*
HEXA  *                                                                 
HEXA  *----------------------------------------------------------------*
HEXA  * SECTION PARA CHAMADA DA ROTINA DE ABEND                         
HEXA  *----------------------------------------------------------------*
HEXA  *                                                                 
HEXA  *----------------------------------------------------------------*
HEXA   9991-10-CHAMA-ROTINA-ABEND SECTION.                              
HEXA  *----------------------------------------------------------------*
HEXA  *                                                                 
HEXA       MOVE    'CLLP7626'      TO          ERR-PGM.                 
HEXA  *                                                                 
HEXA       CALL    WRK-ABEND-MESUX0 USING      WRK-BATCH-MESUX0         
HEXA                                           ERRO-AREA.               
HEXA  *                                                                 
HEXA       GOBACK.                                                      
HEXA  *                                                                 
HEXA  *----------------------------------------------------------------*
HEXA   9991-90-EXIT. EXIT.                                              
HEXA  *----------------------------------------------------------------*
HEXA  *                                                                 

      *===============================================================* 
       IDENTIFICATION DIVISION.                                         
      *===============================================================* 
                                                                        
                                                                        
       PROGRAM-ID. CLLP7625.                                            
       AUTHOR.     ALINE DE NORONHA ABRAO.                              
                                                                        
      *===============================================================* 
      *                ATUC P M   S I S T E M A S                     * 
      *---------------------------------------------------------------* 
      *                                                               * 
      *      PROGRAMA     : CLLP7625                                  * 
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
      *         PARMCLLP                           --------           * 
      *         CADCARTA                           --------           * 
      *         MESTRE                             --------           * 
      *         CADCEP                             --------           * 
      *         AGENC01                            --------           * 
      *         AGENC02                            --------           * 
      *         AGENC03                            --------           * 
      *         AGENC04                            --------           * 
      *         CORRE01                            --------           * 
      *         CORRE02                            --------           * 
      *         CORRE03                            --------           * 
      *         CORRE04                            --------           * 
      *         RELATO                             --------           * 
      *         SORTWK01                           --------           * 
      *                                                               * 
      *                                                               * 
      *---------------------------------------------------------------* 
      *                 ULTIMA ALTERACAO                              * 
      *---------------------------------------------------------------* 
      *                                                               * 
      *      PROGRAMA     : CLLP7625                                  * 
      *      PROGRAMADOR  : ANDREA RUNTE                              * 
      *      SUPERVISOR   : RICARDO BA                                * 
      *      ANALISTA     : JOSE CARLOS      G&P                      * 
      *      DATA         : 13/10/1997                                * 
      *                                                               * 
      *      OBJETIVO     :                                           * 
      *        INCLUIR ARQUIVO PARA GRAVAR INFORMA��ES POR AGENCIA    * 
      *---------------------------------------------------------------* 
      *                 ULTIMA ALTERACAO                              * 
      *---------------------------------------------------------------* 
      *                                                               * 
      *      PROGRAMA     : CLLP7625                                  * 
      *      PROGRAMADOR  : CPM                                       * 
      *      SUPERVISOR   : CPM                                       * 
      *      ANALISTA     : JULIANO GOUVEIA  - CPM                    * 
      *      DATA         : 12/03/2007                                * 
      *                                                               * 
      *      OBJETIVO     :                                           * 
      *                                                               * 
      *        INCLUIR ARQUIVO DE CONTROLE DE LOTE.                   * 
      *                                                               * 
      *        INCLUIR GRAVACAO DE ARQUIVO CONTROLE DE ENVIO AO COR-  * 
      *        REIO, GRAVADO A PARTIR DE REGISTROS COM ENDERECO  DE   * 
      *        ORIGEM DA LOCALIZADORA.                                * 
      *                                                               * 
      *        GERAR ARQUIVO CODIGO DE BARRAS PARA OS ARQUIVOS DE A-  * 
      *        VISOS QUE SERAO ENVIADOS PARA O CORREIO, ALTERANDO O   * 
      *        LEIOUTE DO REG-SAIDA.                                  * 
      *                                                               * 
      *---------------------------------------------------------------* 
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
      *    DATA........:  12/04/2014                                   *
      *----------------------------------------------------------------*
      *    OBJETIVO....:  ADAPTACOES PARA A LEI DA TRANSPARENCIA       *
      *                   TROCAR BOOK INTERNA (840) POR I#CLLPPL(1530) *
      *                   TROCAR BOOK INTERNA (558) POR I#CLLPPN(1290) *
      *    PROJETO 13-0358                                             *
      *================================================================*
           EJECT                                                        
                                                                        
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
                                                                        
                                                                        
           SELECT    PARMCLLP ASSIGN TO UT-S-PARMCLLP                   
                     FILE STATUS IS WRK-FS-PARMCLLP.                    
                                                                        
           SELECT    CADCARTA ASSIGN TO UT-S-CADCARTA                   
                     FILE STATUS IS WRK-FS-CADCARTA.                    
                                                                        
           SELECT    MESTRE ASSIGN TO UT-S-MESTRE                       
                     FILE STATUS IS WRK-FS-MESTRE.                      
                                                                        
           SELECT    CADCEP ASSIGN TO UT-S-CADCEP                       
                     FILE STATUS IS WRK-FS-CADCEP.                      
                                                                        
           SELECT   ARQDATA  ASSIGN TO UT-S-ARQDATA                     
                      FILE STATUS IS WRK-FS-ARQDATA.                    
                                                                        
           SELECT    AGENC01 ASSIGN TO UT-S-AGENC01                     
                     FILE STATUS IS WRK-FS-AGENC01.                     
                                                                        
           SELECT    AGENC02 ASSIGN TO UT-S-AGENC02                     
                     FILE STATUS IS WRK-FS-AGENC02.                     
                                                                        
           SELECT    AGENC03 ASSIGN TO UT-S-AGENC03                     
                     FILE STATUS IS WRK-FS-AGENC03.                     
                                                                        
           SELECT    AGENC04 ASSIGN TO UT-S-AGENC04                     
                     FILE STATUS IS WRK-FS-AGENC04.                     
                                                                        
           SELECT    CORRE01 ASSIGN TO UT-S-CORRE01                     
                     FILE STATUS IS WRK-FS-CORRE01.                     
                                                                        
           SELECT    CORRE02 ASSIGN TO UT-S-CORRE02                     
                     FILE STATUS IS WRK-FS-CORRE02.                     
                                                                        
           SELECT    CORRE03 ASSIGN TO UT-S-CORRE03                     
                     FILE STATUS IS WRK-FS-CORRE03.                     
                                                                        
           SELECT    CORRE04 ASSIGN TO UT-S-CORRE04                     
                     FILE STATUS IS WRK-FS-CORRE04.                     
                                                                        
           SELECT    AGENDIA ASSIGN TO UT-S-AGENDIA                     
                     FILE STATUS IS WRK-FS-AGENDIA.                     
                                                                        
           SELECT    MVAVISO ASSIGN TO UT-S-MVAVISO                     
                     FILE STATUS IS WRK-FS-MVAVISO.                     
                                                                        
           SELECT    RELATO  ASSIGN TO UT-S-RELATO                      
                     FILE STATUS IS WRK-FS-RELATO.                      
                                                                        
JGA        SELECT    CTRLLOTE ASSIGN TO UT-S-CTRLLOTE                   
JGA                  FILE STATUS IS WRK-FS-CTRLLOTE.                    
                                                                        
JGA        SELECT    CTRLCORR ASSIGN TO UT-S-CTRLCORR                   
JGA                  FILE STATUS IS WRK-FS-CTRLCORR.                    
                                                                        
           SELECT    SORTWK01  ASSIGN TO UT-S-SORTKW01.                 
                                                                        
                                                                        
           EJECT                                                        
                                                                        
      *===============================================================* 
       DATA DIVISION.                                                   
      *===============================================================* 
                                                                        
      *---------------------------------------------------------------* 
       FILE SECTION.                                                    
      *---------------------------------------------------------------* 
      *----------------------------------------------------------------*
      *        PARMCLLP        -   INPUT       -       LRECL = 250     *
      *----------------------------------------------------------------*
                                                                        
       FD  PARMCLLP                                                     
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  REG-PARAMETRO.                                               
           05  PARM-TAREFA.                                             
               10  PARM-CCUSTO-1       PIC X(04).                       
               10  PARM-CODIGO-1       PIC 9(04).                       
               10  PARM-CCUSTO-2       PIC X(04).                       
               10  PARM-CODIGO-2       PIC 9(04).                       
           05  PARM-SEQUENCIA          PIC 9(07).                       
           05  PARM-NR-DIAS            PIC 9(03).                       
           05  PARM-VL-CORTE           PIC 9(15).                       
           05  FILLER                  PIC X(209).                      
                                                                        
                                                                        
      *---------------------------------------------------------------* 
      *    INPUT:  ORGANIZACAO SEQUENCIAL  LRECL: 1530                * 
      *---------------------------------------------------------------* 
                                                                        
       FD  CADCARTA                                                     
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  REG-CADCARTA.                                                
           03 FILLER                   PIC X(13).                       
           03 CARTA-AGENCIA            PIC 9(05)   COMP-3.              
           03 FILLER                   PIC X(1514).                     
                                                                        
      *---------------------------------------------------------------* 
      *    INPUT:  ARQUIVO MESTRE DE AGENCIAS      LRECL = 80         * 
      *---------------------------------------------------------------* 
                                                                        
       FD  MESTRE                                                       
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  REG-MESTRE.                                                  
           03 MESTRE-AGENCIA           PIC 9(05)   COMP-3.              
           03 MESTRE-NOME              PIC X(20).                       
           03 MESTRE-ENDER             PIC X(25).                       
           03 MESTRE-CEP               PIC 9(09)   COMP-3.              
           03 MESTRE-SIGLA             PIC X(02).                       
           03 MESTRE-MUNICIPIO         PIC X(14).                       
           03 MESTRE-CD                PIC 9(05)   COMP-3.              
           03 FILLER                   PIC X(08).                       
                                                                        
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
                                                                        
      *---------------------------------------------------------------* 
      *    INPUT:    ARQUIVO CONTROLE DE LOTE                         * 
      *               ORG. SEQUENCIAL   -   LRECL = 050               * 
      *---------------------------------------------------------------* 
                                                                        
       FD          CTRLLOTE                                             
                   RECORDING MODE     IS F                              
                   LABEL     RECORD   IS STANDARD                       
                   BLOCK     CONTAINS 0  RECORDS.                       
                                                                        
       COPY 'I#CLLPPI'.
                                                                        
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
                                                                        
       FD  AGENC02                                                      
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  REG-AGENC02             PIC X(1290).                         
                                                                        
           EJECT                                                        
                                                                        
      *---------------------------------------------------------------* 
      *    OUTPUT:  ORGANIZACAO SEQUENCIAL  LRECL :  1290             * 
      *---------------------------------------------------------------* 
                                                                        
       FD  AGENC03                                                      
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  REG-AGENC03             PIC X(1290).                         
                                                                        
           EJECT                                                        
                                                                        
      *---------------------------------------------------------------* 
      *    OUTPUT:  ORGANIZACAO SEQUENCIAL  LRECL :  1290             * 
      *---------------------------------------------------------------* 
                                                                        
       FD  AGENC04                                                      
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  REG-AGENC04             PIC X(1290).                         
                                                                        
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
      *    OUTPUT:  ORGANIZACAO SEQUENCIAL  LRECL :  1290             * 
      *---------------------------------------------------------------* 
                                                                        
       FD  CORRE02                                                      
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  REG-CORRE02             PIC X(1290).                         
                                                                        
           EJECT                                                        
                                                                        
      *---------------------------------------------------------------* 
      *    OUTPUT:  ORGANIZACAO SEQUENCIAL  LRECL :  1290             * 
      *---------------------------------------------------------------* 
                                                                        
       FD  CORRE03                                                      
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  REG-CORRE03             PIC X(1290).                         
                                                                        
      *---------------------------------------------------------------* 
      *    OUTPUT:  ORGANIZACAO SEQUENCIAL  LRECL :  1290             * 
      *---------------------------------------------------------------* 
                                                                        
       FD  CORRE04                                                      
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  REG-CORRE04             PIC X(1290).                         
                                                                        
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
                                                                        
      *---------------------------------------------------------------* 
      *    OUTPUT:  ORGANIZACAO SEQUENCIAL  LRECL :  096              * 
      *---------------------------------------------------------------* 
                                                                        
       FD  MVAVISO                                                      
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
-INC I#CLLPA3                                                           
                                                                        
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
      *    OUTPUT:  ORGANIZACAO SEQUENCIAL  LRECL :  100              * 
      *---------------------------------------------------------------* 
                                                                        
       FD  CTRLCORR                                                     
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       COPY 'I#CLLPPG'.
                                                                        
           EJECT                                                        
      *---------------------------------------------------------------* 
      *    I-O:       ARQUIVO TEMPORARIO DE SORT                      * 
      *---------------------------------------------------------------* 
                                                                        
       SD  SORTWK01.                                                    
                                                                        
-INC I#CLLPPL                                                           
           02  SOR-NOME-AGENCIA    PIC X(20).                           
           02  SOR-END-AGENCIA     PIC X(25).                           
           02  SOR-CEP-AGENCIA     PIC 9(09)    COMP-3.                 
           02  SOR-SIGLA-AGENCIA   PIC X(02).                           
           02  SOR-MUNIC-AGENCIA   PIC X(14).                           
           02  SOR-CD-AGENCIA      PIC 9(05)    COMP-3.                 
                                                                        
           EJECT                                                        
                                                                        
      *---------------------------------------------------------------* 
       WORKING-STORAGE SECTION.                                         
      *---------------------------------------------------------------* 
                                                                        
       77  FILLER                      PIC X(32)        VALUE           
           '* INICIO DA WORKING CLLP7625 *'.                            
                                                                        
           EJECT                                                        
                                                                        
       77  WRK-ABEND                   PIC S9(04) COMP  VALUE +1111.    
                                                                        
           EJECT                                                        
                                                                        
      *---------------------------------------------------------------* 
      * AREA DOS ARQUIVOS DE SAIDA                                    * 
      *---------------------------------------------------------------* 
                                                                        
-INC I#CLLPPN                                                           
                                                                        
      *---------------------------------------------------------------* 
      * AREAS AUXILIARES                                              * 
      *---------------------------------------------------------------* 
                                                                        
       01  WRK-AGENCIA-ANT             PIC 9(05) COMP-3 VALUE ZEROS.    
       01  WRK-FS-PARMCLLP             PIC X(02)        VALUE  SPACES.  
       01  WRK-FIM-SORT                PIC X(01)        VALUE SPACES.   
       01  WRK-CGC.                                                     
           02 WRK-NUMCGC                  PIC 9(09).                    
           02 WRK-FILCGC                  PIC 9(05).                    
           02 WRK-CTRCGC                  PIC 9(03).                    
       01  WRK-CGC-N                      REDEFINES WRK-CGC PIC 9(17).  
                                                                        
       01  WRK-CGC-CORR.                                                
           02 WRK-NUMCGC-CORR             PIC 9(09)  COMP-3.            
           02 WRK-FILCGC-CORR             PIC 9(05)  COMP-3.            
           02 WRK-CTRCGC-CORR             PIC 9(03)  COMP-3.            
           02 FILLER                      PIC X(07).                    
       01  WRK-CGC-CORR-N       REDEFINES WRK-CGC-CORR PIC 9(17).       
                                                                        
           EJECT                                                        
                                                                        
       01  ACUMULADORES.                                                
           03  ACU-LIDOS               PIC 9(09) COMP-3 VALUE ZEROS.    
           03  ACU-GRV-AGENC01         PIC 9(09) COMP-3 VALUE ZEROS.    
           03  ACU-GRV-AGENC02         PIC 9(09) COMP-3 VALUE ZEROS.    
           03  ACU-GRV-AGENC03         PIC 9(09) COMP-3 VALUE ZEROS.    
           03  ACU-GRV-AGENC04         PIC 9(09) COMP-3 VALUE ZEROS.    
           03  ACU-GRV-AGENDIA         PIC 9(09) COMP-3 VALUE ZEROS.    
           03  ACU-GRV-MVAVISO         PIC 9(09) COMP-3 VALUE ZEROS.    
           03  ACU-GRV-CORRE01         PIC 9(09) COMP-3 VALUE ZEROS.    
           03  ACU-GRV-CORRE02         PIC 9(09) COMP-3 VALUE ZEROS.    
           03  ACU-GRV-CORRE03         PIC 9(09) COMP-3 VALUE ZEROS.    
           03  ACU-GRV-CORRE04         PIC 9(09) COMP-3 VALUE ZEROS.    
           03  ACU-GRV-CTRLCORR        PIC 9(09) COMP-3 VALUE ZEROS.    
                                                                        
           EJECT                                                        
                                                                        
       01  CPOS-FILE-STATUS.                                            
           03  WRK-FS-CADCARTA         PIC X(02) VALUE  SPACES.         
           03  WRK-FS-MESTRE           PIC X(02) VALUE  SPACES.         
           03  WRK-FS-CADCEP           PIC X(02) VALUE  SPACES.         
           03  WRK-FS-ARQDATA          PIC X(02)  VALUE SPACES.         
           03  WRK-FS-AGENC01          PIC X(02) VALUE  SPACES.         
           03  WRK-FS-AGENC02          PIC X(02) VALUE  SPACES.         
           03  WRK-FS-AGENC03          PIC X(02) VALUE  SPACES.         
           03  WRK-FS-AGENC04          PIC X(02) VALUE  SPACES.         
           03  WRK-FS-AGENDIA          PIC X(02) VALUE  SPACES.         
           03  WRK-FS-MVAVISO          PIC X(02) VALUE  SPACES.         
           03  WRK-FS-CORRE01          PIC X(02) VALUE  SPACES.         
           03  WRK-FS-CORRE02          PIC X(02) VALUE  SPACES.         
           03  WRK-FS-CORRE03          PIC X(02) VALUE  SPACES.         
           03  WRK-FS-CORRE04          PIC X(02) VALUE  SPACES.         
           03  WRK-FS-RELATO           PIC X(02) VALUE  SPACES.         
           03  WRK-FS-CTRLLOTE         PIC X(02) VALUE  SPACES.         
           03  WRK-FS-CTRLCORR         PIC X(02) VALUE  SPACES.         
           03  WRK-SORT-RETURN         PIC 9(04) VALUE  ZEROS.          
           03  WRK-OPERACAO            PIC X(13) VALUE  SPACES.         
           03  WRK-ABERTURA            PIC X(13) VALUE 'NA ABERTURA'.   
           03  WRK-LEITURA             PIC X(13) VALUE 'NA LEITURA'.    
           03  WRK-GRAVACAO            PIC X(13) VALUE 'NA GRAVACAO'.   
           03  WRK-FECHAMENTO          PIC X(13) VALUE 'NO FECHAMENTO'. 
                                                                        
           EJECT                                                        
                                                                        
       01  WRK-POOL7600.                                                
           03  FILLER                  PIC 9(05)  COMP-3  VALUE  ZEROS. 
           03  FILLER                  PIC 9(07)  COMP-3  VALUE  ZEROS. 
           03  WRK-DATA                PIC 9(09)  COMP-3  VALUE  ZEROS. 
           03  FILLER                  PIC 9(07)  COMP-3  VALUE  ZEROS. 
           03  FILLER                  PIC 9(13)  COMP-3  VALUE  ZEROS. 
           03  FILLER                  PIC X(20)          VALUE  SPACES.
                                                                        
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
                                                                        
       01  WRK-DIAS1                   PIC 9(03)          VALUE  ZEROS. 
       01  WRK-DIAS2                   PIC 9(03)          VALUE  ZEROS. 
       01  WRK-DIAS3                   PIC 9(03)          VALUE  ZEROS. 
       01  WRK-DIAS4                   PIC 9(03)          VALUE  ZEROS. 
                                                                        
       01  WRK-VL-CORTE1               PIC 9(15)          VALUE  ZEROS. 
       01  WRK-VL-CORTE2               PIC 9(15)          VALUE  ZEROS. 
       01  WRK-VL-CORTE3               PIC 9(15)          VALUE  ZEROS. 
       01  WRK-VL-CORTE4               PIC 9(15)          VALUE  ZEROS. 
                                                                        
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
       01  WRK-IND1                   PIC 9(04) VALUE ZEROS.            
       01  WRK-SAI                    PIC X(01) VALUE SPACES.           
       01  WRK-VALOR-RESGATE          PIC  9(15)V99 COMP-3 VALUE ZEROS. 
                                                                        
       01  AUX-CHAVE-CGC.                                               
           05  FILLER      PIC 9(09)    COMP-3  VALUE  ZEROS.           
           05  FILLER      PIC 9(05)    COMP-3  VALUE  ZEROS.           
           05  FILLER      PIC 9(03)    COMP-3  VALUE  ZEROS.           
                                                                        
       01  AUX-AGENCIA     PIC 9(05)    COMP-3  VALUE  ZEROS.           
                                                                        
      *---------------------------------------------------------------* 
      *                     LINHAS DE CABECALHOS                      * 
      *---------------------------------------------------------------* 
                                                                        
       01  CABEC1.                                                      
           03  CB1-CARRO     PIC X(01)  VALUE '-'.                      
           03  FILLER        PIC X(35)  VALUE 'CLLP7625'.               
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
           03  FILLER        PIC X(28) VALUE                            
                                      'SEREM ENVIADOS AOS CLIENTES '.   
           03  FILLER        PIC X(20) VALUE '- DATA DE MOVIMENTO '.    
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
                                                                        
           EJECT                                                        
                                                                        
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
           03  FILLER        PIC X(23)  VALUE 'AVADOS AGENC 1A. CARTA:'.
           03  LT2-AGENC01   PIC ZZZ.ZZZ.ZZ9.                           
                                                                        
       01  LINTOT3.                                                     
           03  LT3-CARRO     PIC X(01)  VALUE '-'.                      
           03  FILLER        PIC X(25)  VALUE SPACES.                   
           03  FILLER        PIC X(21)  VALUE 'TOTAL DE REGISTROS GR'.  
           03  FILLER        PIC X(23)  VALUE 'AVADOS AGENC 2A. CARTA:'.
           03  LT3-AGENC02   PIC ZZZ.ZZZ.ZZ9.                           
                                                                        
       01  LINTOT4.                                                     
           03  LT4-CARRO     PIC X(01)  VALUE '-'.                      
           03  FILLER        PIC X(25)  VALUE SPACES.                   
           03  FILLER        PIC X(21)  VALUE 'TOTAL DE REGISTROS GR'.  
           03  FILLER        PIC X(23)  VALUE 'AVADOS AGENC 3A. CARTA:'.
           03  LT4-AGENC03   PIC ZZZ.ZZZ.ZZ9.                           
                                                                        
       01  LINTOT5.                                                     
           03  LT5-CARRO     PIC X(01)  VALUE '-'.                      
           03  FILLER        PIC X(25)  VALUE SPACES.                   
           03  FILLER        PIC X(21)  VALUE 'TOTAL DE REGISTROS GR'.  
           03  FILLER        PIC X(23)  VALUE 'AVADOS CORRE 1A. CARTA:'.
           03  LT5-CORRE01   PIC ZZZ.ZZZ.ZZ9.                           
                                                                        
       01  LINTOT6.                                                     
           03  LT6-CARRO     PIC X(01)  VALUE '-'.                      
           03  FILLER        PIC X(25)  VALUE SPACES.                   
           03  FILLER        PIC X(21)  VALUE 'TOTAL DE REGISTROS GR'.  
           03  FILLER        PIC X(23)  VALUE 'AVADOS CORRE 2A. CARTA:'.
           03  LT6-CORRE02   PIC ZZZ.ZZZ.ZZ9.                           
                                                                        
       01  LINTOT7.                                                     
           03  LT7-CARRO     PIC X(01)  VALUE '-'.                      
           03  FILLER        PIC X(25)  VALUE SPACES.                   
           03  FILLER        PIC X(21)  VALUE 'TOTAL DE REGISTROS GR'.  
           03  FILLER        PIC X(23)  VALUE 'AVADOS CORRE 3A. CARTA:'.
           03  LT7-CORRE03   PIC ZZZ.ZZZ.ZZ9.                           
                                                                        
       01  LINTOT8.                                                     
           03  LT8-CARRO     PIC X(01)  VALUE '-'.                      
           03  FILLER        PIC X(25)  VALUE SPACES.                   
           03  FILLER        PIC X(21)  VALUE 'TOTAL DE REGISTROS GR'.  
           03  FILLER        PIC X(23)  VALUE 'AVADOS AGENC 4A. CARTA:'.
           03  LT8-AGENC04   PIC ZZZ.ZZZ.ZZ9.                           
                                                                        
       01  LINTOT9.                                                     
           03  LT9-CARRO     PIC X(01)  VALUE '-'.                      
           03  FILLER        PIC X(25)  VALUE SPACES.                   
           03  FILLER        PIC X(21)  VALUE 'TOTAL DE REGISTROS GR'.  
           03  FILLER        PIC X(23)  VALUE 'AVADOS CORRE 4A. CARTA:'.
           03  LT9-CORRE04   PIC ZZZ.ZZZ.ZZ9.                           
HEXA  *                                                                 
HEXA  *---------------------------------------------------------------* 
HEXA  * AREA DE DECLARACAO PARA SUPORTE A MULTIPLAS FAIXAS DE AGENCIA * 
HEXA  *---------------------------------------------------------------* 
HEXA  *                                                                 
HEXA       COPY 'I#HEXA01'.
HEXA  *                                                                 
HEXA       COPY 'I#BRAD7C'.
HEXA  *                                                                 
                                                                        
           EJECT                                                        
                                                                        
       01  FILLER                      PIC X(32)        VALUE           
           '*  FIM DA WORKING CLLP7625 *'.                              
                                                                        
           EJECT                                                        
                                                                        
      *===============================================================* 
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
                                                                        
           OPEN     INPUT    PARMCLLP                                   
                             CADCARTA                                   
                             MESTRE                                     
                             CADCEP                                     
                             ARQDATA                                    
JGA                          CTRLLOTE                                   
                    OUTPUT   AGENC01                                    
                             AGENC02                                    
                             AGENC03                                    
                             AGENC04                                    
                             AGENDIA                                    
                             MVAVISO                                    
                             CORRE01                                    
                             CORRE02                                    
                             CORRE03                                    
                             CORRE04                                    
JGA                          CTRLCORR                                   
                             RELATO                                     
                                                                        
           INITIALIZE WRK-ARQ-AGENDIA.                                  
                                                                        
           MOVE     WRK-ABERTURA         TO   WRK-OPERACAO              
           PERFORM  10000-TESTAR-FILE-STATUS                            
                                                                        
           PERFORM  20000-VERIFICAR-VAZIO                               
                                                                        
           IF       WRK-FS-CADCARTA  EQUAL  '10'                        
                    PERFORM          80000-FINALIZAR.                   
                                                                        
                                                                        
           PERFORM 20500-LER-PARMCLLP.                                  
                                                                        
           IF  WRK-FS-PARMCLLP   EQUAL  '10'                            
               DISPLAY '****************  CLLP7625  *******************'
               DISPLAY '*           ABEND 1111 - FORCADO              *'
               DISPLAY '*      ARQUIVO DE PARAMETROS ESTA VAZIO       *'
               DISPLAY '*   AVISAR ANALISTA RESPONSAVEL DA ROTINA     *'
               DISPLAY '*****************  CLLP7625  ******************'
               CALL  'ILBOABN0'  USING  WRK-ABEND                       
           ELSE                                                         
               PERFORM 20600-TRATA-PARMCLLP                             
                   UNTIL WRK-FS-PARMCLLP  EQUAL  '10'.                  
                                                                        
           PERFORM  25000-LER-ARQDATA                                   
                                                                        
                                                                        
           MOVE     DTMOV              TO        WRK-AUX-DTMOV          
                                                                        
                                                                        
           MOVE     WRK-DTMOV-DIA      TO        CB2-DIA-MOV            
                                                 WRK-DIA-AVISO          
           MOVE     WRK-DTMOV-MES      TO        CB2-MES-MOV            
                                                 WRK-MES-AVISO          
           MOVE     WRK-DTMOV-ANO      TO        CB2-ANO-MOV            
                                                 WRK-ANO-AVISO          
                                                                        
JGA        PERFORM 20700-LER-CTRLLOTE.                                  
JGA                                                                     
           PERFORM  30000-LER-MESTRE                                    
                                                                        
           SORT     SORTWK01 ASCENDING KEY                              
                                            SELE-DEV-CCEP               
                    INPUT    PROCEDURE      40000-ENTRADA-SORT          
                    OUTPUT   PROCEDURE      50000-SAIDA-SORT            
                                                                        
           PERFORM  60000-TESTAR-RC-SORT                                
                                                                        
           PERFORM  70000-IMPRIMIR-RELATO                               
                                                                        
           PERFORM  80000-FINALIZAR.                                    
                                                                        
                                                                        
       FIM-00000-INICIAR.                                        EXIT.  
      *---------------------------------------------------------------* 
                                                                        
           EJECT                                                        
                                                                        
      *---------------------------------------------------------------* 
       10000-TESTAR-FILE-STATUS SECTION.                                
      *---------------------------------------------------------------* 
                                                                        
           PERFORM 11000-TESTAR-FS-CADCARTA                             
                                                                        
           PERFORM 12000-TESTAR-FS-MESTRE                               
                                                                        
           PERFORM 13000-TESTAR-FS-CADCEP                               
                                                                        
           PERFORM 13500-TESTAR-FS-ARQDATA                              
                                                                        
           PERFORM 14000-TESTAR-FS-AGENC01                              
                                                                        
           PERFORM 15000-TESTAR-FS-AGENC02                              
                                                                        
           PERFORM 16000-TESTAR-FS-AGENC03                              
                                                                        
           PERFORM 16100-TESTAR-FS-AGENC04                              
                                                                        
           PERFORM 16200-TESTAR-FS-AGENDIA.                             
                                                                        
           PERFORM 16210-TESTAR-FS-MVAVISO.                             
                                                                        
           PERFORM 17000-TESTAR-FS-CORRE01                              
                                                                        
           PERFORM 18000-TESTAR-FS-CORRE02                              
                                                                        
           PERFORM 19000-TESTAR-FS-CORRE03                              
                                                                        
           PERFORM 19100-TESTAR-FS-CORRE04                              
                                                                        
           PERFORM 19000-A-TESTAR-FS-RELATO.                            
                                                                        
JGA        PERFORM 19200-TESTAR-FS-CTRLLOTE                             
                                                                        
JGA        PERFORM 19300-TESTAR-FS-CTRLCORR.                            
                                                                        
       FIM-10000-TESTAR-FS.                                      EXIT.  
      *---------------------------------------------------------------* 
                                                                        
           EJECT                                                        
                                                                        
      *---------------------------------------------------------------* 
       11000-TESTAR-FS-CADCARTA SECTION.                                
      *---------------------------------------------------------------* 
                                                                        
           IF    WRK-FS-CADCARTA  NOT EQUAL '00'                        
                 DISPLAY '************** CLLP7625 *************'        
                 DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'    
                 DISPLAY '*              CADCARTA             *'        
                 DISPLAY '*         FILE STATUS =  ' WRK-FS-CADCARTA    
                                                    '         *'        
                 DISPLAY '************** CLLP7625 *************'        
                 CALL    'ILBOABN0'      USING   WRK-ABEND.             
                                                                        
                                                                        
       FIM-11000-FS-CADCARTA.                                    EXIT.  
      *---------------------------------------------------------------* 
                                                                        
           EJECT                                                        
                                                                        
      *---------------------------------------------------------------* 
       12000-TESTAR-FS-MESTRE SECTION.                                  
      *---------------------------------------------------------------* 
                                                                        
           IF    WRK-FS-MESTRE  NOT EQUAL '00'                          
                 DISPLAY '************** CLLP7625 *************'        
                 DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'    
                 DISPLAY '*              MESTRE               *'        
                 DISPLAY '*         FILE STATUS =  ' WRK-FS-MESTRE      
                                                    '         *'        
                 DISPLAY '************** CLLP7625 *************'        
                 CALL    'ILBOABN0'      USING   WRK-ABEND.             
                                                                        
                                                                        
       FIM-12000-FS-MESTRE.                                      EXIT.  
      *---------------------------------------------------------------* 
                                                                        
           EJECT                                                        
                                                                        
      *---------------------------------------------------------------* 
       13000-TESTAR-FS-CADCEP SECTION.                                  
      *---------------------------------------------------------------* 
                                                                        
           IF    WRK-FS-CADCEP  NOT EQUAL '00'                          
                 DISPLAY '************** CLLP7625 *************'        
                 DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'    
                 DISPLAY '*              CADCEP               *'        
                 DISPLAY '*         FILE STATUS =  ' WRK-FS-CADCEP      
                                                    '         *'        
                 DISPLAY '************** CLLP7625 *************'        
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
                 DISPLAY '************** CLLP7625 **************'       
                 DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO    *'   
                 DISPLAY '*              ARQDATA               *'       
                 DISPLAY '*         FILE STATUS =  ' WRK-FS-ARQDATA     
                                                    '          *'       
                 DISPLAY '************** CLLP7625 **************'       
                 CALL 'ILBOABN0'       USING WRK-ABEND.                 
                                                                        
       13500-EXIT. EXIT.                                                
      *                                                                 
           EJECT                                                        
                                                                        
      *---------------------------------------------------------------* 
       14000-TESTAR-FS-AGENC01  SECTION.                                
      *---------------------------------------------------------------* 
                                                                        
           IF    WRK-FS-AGENC01  NOT EQUAL '00'                         
                 DISPLAY '************** CLLP7625 *************'        
                 DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'    
                 DISPLAY '*              AGENC01              *'        
                 DISPLAY '*         FILE STATUS =  ' WRK-FS-AGENC01     
                                                    '         *'        
                 DISPLAY '************** CLLP7625 *************'        
                 CALL    'ILBOABN0'      USING   WRK-ABEND.             
                                                                        
                                                                        
       FIM-14000-FS-AGENC01.                                     EXIT.  
      *---------------------------------------------------------------* 
                                                                        
           EJECT                                                        
      *---------------------------------------------------------------* 
       15000-TESTAR-FS-AGENC02 SECTION.                                 
      *---------------------------------------------------------------* 
                                                                        
           IF    WRK-FS-AGENC02  NOT EQUAL '00'                         
                 DISPLAY '************** CLLP7625 *************'        
                 DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'    
                 DISPLAY '*              AGENC02              *'        
                 DISPLAY '*         FILE STATUS =  ' WRK-FS-AGENC02     
                                                    '         *'        
                 DISPLAY '************** CLLP7625 *************'        
                 CALL    'ILBOABN0'      USING   WRK-ABEND.             
                                                                        
                                                                        
       FIM-15000-FS-AGENC02.                                     EXIT.  
      *---------------------------------------------------------------* 
                                                                        
           EJECT                                                        
                                                                        
      *---------------------------------------------------------------* 
       16000-TESTAR-FS-AGENC03 SECTION.                                 
      *---------------------------------------------------------------* 
                                                                        
           IF    WRK-FS-AGENC03 NOT EQUAL '00'                          
                 DISPLAY '************** CLLP7625 *************'        
                 DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'    
                 DISPLAY '*              AGENC03              *'        
                 DISPLAY '*         FILE STATUS =  ' WRK-FS-AGENC03     
                                                    '         *'        
                 DISPLAY '************** CLLP7625 *************'        
                 CALL    'ILBOABN0'      USING   WRK-ABEND.             
                                                                        
                                                                        
       FIM-16000-FS-AGENC03.                                     EXIT.  
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       16100-TESTAR-FS-AGENC04 SECTION.                                 
      *---------------------------------------------------------------* 
                                                                        
           IF    WRK-FS-AGENC04 NOT EQUAL '00'                          
                 DISPLAY '************** CLLP7625 *************'        
                 DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'    
                 DISPLAY '*              AGENC04              *'        
                 DISPLAY '*         FILE STATUS =  ' WRK-FS-AGENC04     
                                                    '         *'        
                 DISPLAY '************** CLLP7625 *************'        
                 CALL    'ILBOABN0'      USING   WRK-ABEND.             
                                                                        
                                                                        
       FIM-16100-FS-AGENC04.                                     EXIT.  
      *---------------------------------------------------------------* 
                                                                        
           EJECT                                                        
      *---------------------------------------------------------------* 
       16200-TESTAR-FS-AGENDIA SECTION.                                 
      *---------------------------------------------------------------* 
                                                                        
           IF    WRK-FS-AGENDIA NOT EQUAL '00'                          
                 DISPLAY '************** CLLP7625 *************'        
                 DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'    
                 DISPLAY '*              AGENDIA              *'        
                 DISPLAY '*         FILE STATUS =  ' WRK-FS-AGENDIA     
                                                    '         *'        
                 DISPLAY '************** CLLP7625 *************'        
                 CALL    'ILBOABN0'      USING   WRK-ABEND.             
                                                                        
                                                                        
       FIM-16200-FS-AGENDIA.                                     EXIT.  
      *---------------------------------------------------------------* 
                                                                        
           EJECT                                                        
      *---------------------------------------------------------------* 
       16210-TESTAR-FS-MVAVISO SECTION.                                 
      *---------------------------------------------------------------* 
                                                                        
           IF    WRK-FS-MVAVISO NOT EQUAL '00'                          
                 DISPLAY '************** CLLP7625 *************'        
                 DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'    
                 DISPLAY '*              MVAVISO              *'        
                 DISPLAY '*         FILE STATUS =  ' WRK-FS-MVAVISO     
                                                    '         *'        
                 DISPLAY '************** CLLP7625 *************'        
                 CALL    'ILBOABN0'      USING   WRK-ABEND.             
                                                                        
                                                                        
       FIM-16200-FS-MVAVISO.                                     EXIT.  
      *---------------------------------------------------------------* 
                                                                        
           EJECT                                                        
                                                                        
      *---------------------------------------------------------------* 
       17000-TESTAR-FS-CORRE01 SECTION.                                 
      *---------------------------------------------------------------* 
                                                                        
           IF    WRK-FS-CORRE01 NOT EQUAL '00'                          
                 DISPLAY '************** CLLP7625 *************'        
                 DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'    
                 DISPLAY '*              CORRE01              *'        
                 DISPLAY '*         FILE STATUS =  ' WRK-FS-CORRE01     
                                                    '         *'        
                 DISPLAY '************** CLLP7625 *************'        
                 CALL    'ILBOABN0'      USING   WRK-ABEND.             
                                                                        
       FIM-17000-FS-CORRE01.                                     EXIT.  
                                                                        
           EJECT                                                        
                                                                        
      *---------------------------------------------------------------* 
       18000-TESTAR-FS-CORRE02 SECTION.                                 
      *---------------------------------------------------------------* 
                                                                        
           IF    WRK-FS-CORRE02 NOT EQUAL '00'                          
                 DISPLAY '************** CLLP7625 *************'        
                 DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'    
                 DISPLAY '*              CORRE02              *'        
                 DISPLAY '*         FILE STATUS =  ' WRK-FS-CORRE02     
                                                    '         *'        
                 DISPLAY '************** CLLP7625 *************'        
                 CALL    'ILBOABN0'      USING   WRK-ABEND.             
                                                                        
                                                                        
       FIM-18000-FS-CORRE02.                                     EXIT.  
      *---------------------------------------------------------------* 
                                                                        
           EJECT                                                        
                                                                        
      *---------------------------------------------------------------* 
       19000-TESTAR-FS-CORRE03 SECTION.                                 
      *---------------------------------------------------------------* 
                                                                        
           IF    WRK-FS-CORRE03 NOT EQUAL '00'                          
                 DISPLAY '************** CLLP7625 *************'        
                 DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'    
                 DISPLAY '*              CORRE03              *'        
                 DISPLAY '*         FILE STATUS =  ' WRK-FS-CORRE03     
                                                    '         *'        
                 DISPLAY '************** CLLP7625 *************'        
                 CALL    'ILBOABN0'      USING   WRK-ABEND.             
                                                                        
                                                                        
       FIM-19000-FS-CORRE03.                                     EXIT.  
                                                                        
      *---------------------------------------------------------------* 
       19100-TESTAR-FS-CORRE04 SECTION.                                 
      *---------------------------------------------------------------* 
                                                                        
           IF    WRK-FS-CORRE04 NOT EQUAL '00'                          
                 DISPLAY '************** CLLP7625 *************'        
                 DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'    
                 DISPLAY '*              CORRE04              *'        
                 DISPLAY '*         FILE STATUS =  ' WRK-FS-CORRE04     
                                                    '         *'        
                 DISPLAY '************** CLLP7625 *************'        
                 CALL    'ILBOABN0'      USING   WRK-ABEND.             
                                                                        
       FIM-19100-FS-CORRE04.                                     EXIT.  
      *---------------------------------------------------------------* 
                                                                        
           EJECT                                                        
      *---------------------------------------------------------------* 
       19200-TESTAR-FS-CTRLLOTE SECTION.                                
      *---------------------------------------------------------------* 
                                                                        
           IF    WRK-FS-CTRLLOTE NOT EQUAL '00'                         
                 DISPLAY '************** CLLP7625 *************'        
                 DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'    
                 DISPLAY '*              CTRLLOTE             *'        
                 DISPLAY '*         FILE STATUS =  ' WRK-FS-CTRLLOTE    
                                                    '         *'        
                 DISPLAY '************** CLLP7625 *************'        
                 CALL    'ILBOABN0'      USING   WRK-ABEND.             
                                                                        
       FIM-19200-FS-CTRLLOTE.                                    EXIT.  
      *---------------------------------------------------------------* 
                                                                        
           EJECT                                                        
      *---------------------------------------------------------------* 
       19300-TESTAR-FS-CTRLCORR SECTION.                                
      *---------------------------------------------------------------* 
                                                                        
           IF    WRK-FS-CTRLCORR NOT EQUAL '00'                         
                 DISPLAY '************** CLLP7625 *************'        
                 DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'    
                 DISPLAY '*              CTRLCORR             *'        
                 DISPLAY '*         FILE STATUS =  ' WRK-FS-CTRLCORR    
                                                    '         *'        
                 DISPLAY '************** CLLP7625 *************'        
                 CALL    'ILBOABN0'      USING   WRK-ABEND.             
                                                                        
       FIM-19300-FS-CTRLCORR.                                    EXIT.  
      *---------------------------------------------------------------* 
                                                                        
           EJECT                                                        
                                                                        
      *---------------------------------------------------------------* 
       19000-A-TESTAR-FS-RELATO SECTION.                                
      *---------------------------------------------------------------* 
                                                                        
           IF    WRK-FS-RELATO  NOT EQUAL '00'                          
                 DISPLAY '************** CLLP7625 *************'        
                 DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'    
                 DISPLAY '*              RELATO               *'        
                 DISPLAY '*         FILE STATUS =  ' WRK-FS-RELATO      
                                                    '         *'        
                 DISPLAY '************** CLLP7625 *************'        
                 CALL    'ILBOABN0'      USING   WRK-ABEND.             
                                                                        
                                                                        
       FIM-19000-A-FS-RELATO.                                    EXIT.  
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
                                                                        
                                                                        
      *----------------------------------------------------------------*
       20500-LER-PARMCLLP SECTION.                                      
      *----------------------------------------------------------------*
                                                                        
           READ PARMCLLP.                                               
                                                                        
           IF  WRK-FS-PARMCLLP EQUAL '10'                               
               GO TO 20500-99-FIM.                                      
                                                                        
                                                                        
           IF  PARM-CCUSTO-1    NOT EQUAL 'CLLP'  OR                    
               PARM-CODIGO-1    NOT EQUAL '0002'  OR                    
               PARM-CCUSTO-2    NOT EQUAL 'CLLP'  OR                    
               PARM-CODIGO-2    NOT EQUAL '7615'                        
               DISPLAY '****************  CLLP7625  *******************'
               DISPLAY '*           ABEND 1111 - FORCADO              *'
               DISPLAY '*  PROGRAMA CANCELADO - PARAMETRO INVALIDO    *'
               DISPLAY '* '  PARM-CCUSTO-1 ' / ' PARM-CODIGO-1          
                       ' / ' PARM-CCUSTO-2 ' / ' PARM-CODIGO-2          
               DISPLAY '*   AVISAR ANALISTA RESPONSAVEL DA ROTINA     *'
               DISPLAY '*****************  CLLP7625  ******************'
               CALL  'ILBOABN0'  USING  WRK-ABEND.                      
                                                                        
      *----------------------------------------------------------------*
       20500-99-FIM.   EXIT.                                            
      *----------------------------------------------------------------*
                                                                        
                                                                        
      *----------------------------------------------------------------*
       20600-TRATA-PARMCLLP SECTION.                                    
      *----------------------------------------------------------------*
                                                                        
           IF  PARM-SEQUENCIA    EQUAL   1                              
               MOVE  PARM-NR-DIAS        TO   WRK-DIAS1                 
               MOVE  PARM-VL-CORTE       TO   WRK-VL-CORTE1             
           ELSE                                                         
               IF  PARM-SEQUENCIA    EQUAL   2                          
                   MOVE  PARM-NR-DIAS        TO   WRK-DIAS2             
                   MOVE  PARM-VL-CORTE       TO   WRK-VL-CORTE2         
               ELSE                                                     
                   IF  PARM-SEQUENCIA    EQUAL   3                      
                       MOVE  PARM-NR-DIAS        TO   WRK-DIAS3         
                       MOVE  PARM-VL-CORTE       TO   WRK-VL-CORTE3     
                   ELSE                                                 
                       IF  PARM-SEQUENCIA    EQUAL   4                  
                           MOVE  PARM-NR-DIAS        TO   WRK-DIAS4     
                           MOVE  PARM-VL-CORTE       TO   WRK-VL-CORTE4.
                                                                        
           PERFORM 20500-LER-PARMCLLP.                                  
                                                                        
      *----------------------------------------------------------------*
       20600-99-FIM.   EXIT.                                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
JGA    20700-LER-CTRLLOTE SECTION.                                      
JGA   *----------------------------------------------------------------*
JGA                                                                     
JGA        READ CTRLLOTE.                                               
JGA                                                                     
JGA        IF  WRK-FS-CTRLLOTE EQUAL '10'                               
JGA            DISPLAY '****************  CLLP7625  *******************'
JGA            DISPLAY '*           ABEND 1111 - FORCADO              *'
JGA            DISPLAY '*    ARQUIVO DE CONTROLE DE LOTES VAZIO       *'
JGA            DISPLAY '*   AVISAR ANALISTA RESPONSAVEL DA ROTINA     *'
JGA            DISPLAY '*****************  CLLP7625  ******************'
JGA            CALL  'ILBOABN0'  USING  WRK-ABEND.                      
JGA                                                                     
JGA        DISPLAY '****************  CLLP7625  *******************'    
JGA        DISPLAY '*                                             *'    
JGA        DISPLAY '*  CONTROLE DE LOTES  -  LOTE ATUAL           *'    
JGA        DISPLAY '*                                             *'    
JGA        DISPLAY '* NUMERO DO LOTE.....: ' CLLPPI-NLOTE               
JGA                                                 '             *'    
JGA        DISPLAY '* DATA DE MOVIMENTO..: ' CLLPPI-DTMOV               
JGA                                                '              *'    
JGA        DISPLAY '*                                             *'    
JGA        DISPLAY '*****************  CLLP7625  ******************'.   
                                                                        
      *----------------------------------------------------------------*
       20500-99-FIM.   EXIT.                                            
      *----------------------------------------------------------------*
                                                                        
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
                                                                        
                                                                        
      *---------------------------------------------------------------* 
      *        LEITURA DO ARQUIVO DATA DE MOVIMENTO                     
      *---------------------------------------------------------------* 
                                                                        
       25000-LER-ARQDATA   SECTION.                                     
                                                                        
           READ ARQDATA.                                                
                                                                        
           MOVE WRK-LEITURA            TO     WRK-OPERACAO              
           PERFORM 13500-TESTAR-FS-ARQDATA.                             
                                                                        
       FIM-25000-LER-ARQDATA. EXIT.                                     
      *                                                                 
                                                                        
           EJECT                                                        
                                                                        
      *---------------------------------------------------------------* 
       30000-LER-MESTRE SECTION.                                        
      *---------------------------------------------------------------* 
                                                                        
           READ   MESTRE                                                
                                                                        
           IF      WRK-FS-MESTRE     EQUAL   '10'                       
                   MOVE  99999       TO      MESTRE-AGENCIA             
                   GO                TO      FIM-30000-LER-MESTRE.      
                                                                        
           MOVE    WRK-LEITURA       TO      WRK-OPERACAO               
           PERFORM 12000-TESTAR-FS-MESTRE.                              
                                                                        
                                                                        
                                                                        
       FIM-30000-LER-MESTRE.                                     EXIT.  
      *---------------------------------------------------------------* 
                                                                        
           EJECT                                                        
                                                                        
      *---------------------------------------------------------------* 
       40000-ENTRADA-SORT SECTION.                                      
      *---------------------------------------------------------------* 
                                                                        
           PERFORM 31000-MONTAR-SORT                                    
                   UNTIL WRK-FS-CADCARTA  EQUAL  '10'.                  
                                                                        
                                                                        
       FIM-30000-ENTRADA-SORT.                                   EXIT.  
      *---------------------------------------------------------------* 
                                                                        
           EJECT                                                        
                                                                        
      *---------------------------------------------------------------* 
       31000-MONTAR-SORT SECTION.                                       
      *---------------------------------------------------------------* 
                                                                        
           IF  CARTA-AGENCIA  NOT  EQUAL  WRK-AGENCIA-ANT               
               PERFORM        30000-LER-MESTRE UNTIL                    
                              WRK-FS-MESTRE    EQUAL  '10'           OR 
                              CARTA-AGENCIA    EQUAL   MESTRE-AGENCIA OR
                              MESTRE-AGENCIA   GREATER CARTA-AGENCIA.   
                                                                        
           MOVE  REG-CADCARTA     TO REG-ARQSELE.                       
                                                                        
           IF    MESTRE-AGENCIA    GREATER      CARTA-AGENCIA           
                 MOVE   SPACES          TO      SOR-NOME-AGENCIA        
                                                SOR-END-AGENCIA         
                                                SOR-SIGLA-AGENCIA       
                                                SOR-MUNIC-AGENCIA       
                 MOVE   ZEROS           TO      SOR-CEP-AGENCIA         
                                                SOR-CD-AGENCIA          
           ELSE                                                         
                 MOVE  MESTRE-NOME      TO      SOR-NOME-AGENCIA        
                 MOVE  MESTRE-ENDER     TO      SOR-END-AGENCIA         
                 MOVE  MESTRE-CEP       TO      SOR-CEP-AGENCIA         
                 MOVE  MESTRE-SIGLA     TO      SOR-SIGLA-AGENCIA       
                 MOVE  MESTRE-MUNICIPIO TO      SOR-MUNIC-AGENCIA       
                 MOVE  MESTRE-CD        TO      SOR-CD-AGENCIA.         
                                                                        
           RELEASE     REG-ARQSELE                                      
                                                                        
           MOVE        CARTA-AGENCIA    TO      WRK-AGENCIA-ANT         
                                                                        
           PERFORM     21000-LER-CADCARTA.                              
                                                                        
                                                                        
       FIM-31000-MONTAR-SORT.                                    EXIT.  
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       50000-SAIDA-SORT SECTION.                                        
      *---------------------------------------------------------------* 
                                                                        
           PERFORM    51000-LER-SORTWK01                                
                                                                        
           PERFORM    52000-LER-CADCEP                                  
                                                                        
           PERFORM    53000-PROCESSAR-SORT   UNTIL                      
                      WRK-FIM-SORT           EQUAL  'S'.                
                                                                        
                                                                        
                                                                        
       FIM-50000-SAIDA-SORT.                                     EXIT.  
      *---------------------------------------------------------------* 
                                                                        
           EJECT                                                        
                                                                        
      *---------------------------------------------------------------* 
       51000-LER-SORTWK01 SECTION.                                      
      *---------------------------------------------------------------* 
                                                                        
           RETURN    SORTWK01   AT   END                                
                     MOVE  'S'  TO   WRK-FIM-SORT                       
                     GO         TO   FIM-51000-LER-SORTWK01.            
                                                                        
                                                                        
       FIM-51000-LER-SORTWK01.                                   EXIT.  
      *---------------------------------------------------------------* 
                                                                        
           EJECT                                                        
                                                                        
      *---------------------------------------------------------------* 
AQUI   51110-DEVEDOR SECTION.                                           
      *--------------------------------------------------------------*  
                                                                        
           MOVE   SELE-CHAVE              TO   SPC-CHAVE                
           MOVE   SELE-EMPRESA            TO   SPC-EMPRESA              
           MOVE   SELE-AGENCIA            TO   SPC-AGENCIA              
           MOVE   SELE-NUM-CC             TO   SPC-NUM-CC               
           MOVE   SELE-NOMERESP           TO   SPC-NOMERESP             
           MOVE   SELE-CGCCPF-RESP        TO   SPC-CGCCPFRESP           
           MOVE   SELE-DEV-ENDER          TO   SPC-ENDER                
           MOVE   SELE-DEV-NRO            TO   SPC-NRO                  
           MOVE   SELE-DEV-COMPL          TO   SPC-COMPL                
           MOVE   SELE-DEV-BAIRRO         TO   SPC-BAIRRO               
           MOVE   SELE-DEV-CIDADE         TO   SPC-CIDADE               
           MOVE   SELE-DEV-UF             TO   SPC-UF                   
           MOVE   SELE-DEV-CCEP           TO   SPC-CCEP                 
           MOVE   SELE-DEV-CCEP-COMPL     TO   SPC-CCEP-COMPL           
           MOVE   SELE-IDENT              TO   SPC-IDENT                
           PERFORM                                                      
               VARYING WRK-IND1        FROM 1 BY 1 UNTIL                
               WRK-IND1                GREATER 11                       
               MOVE SELE-OCORRENCIAS (WRK-IND1)                         
                                          TO SPC-OCORRENCIAS (WRK-IND1) 
           END-PERFORM.                                                 
           MOVE   SOR-NOME-AGENCIA        TO SPC-NOME-AGENCIA           
           MOVE   SOR-END-AGENCIA         TO SPC-END-AGENCIA            
           MOVE   SOR-CEP-AGENCIA         TO SPC-CEP-AGENCIA            
           MOVE   SOR-SIGLA-AGENCIA       TO SPC-SIGLA-AGENCIA          
           MOVE   SOR-MUNIC-AGENCIA       TO SPC-MUNIC-AGENCIA          
           MOVE   SOR-CD-AGENCIA          TO SPC-CD-AGENCIA.            
      *                                                                 
      *    MONTAR MVAVISO                                               
      *                                                                 
           MOVE SELE-NUMCGC               TO   WRK-NUMCGC.              
           MOVE SELE-FILCGC               TO   WRK-FILCGC.              
           MOVE SELE-CTRCGC               TO   WRK-CTRCGC.              
           MOVE WRK-CGC-N                 TO   CLLP-CPF.                
           MOVE SELE-NOMERESP             TO   CLLP-NOME-DEVEDOR.       
           MOVE WRK-DATA-AVISO            TO   CLLP-DATA-EMISSAO-AVISO. 
           MOVE SELE-NATUREZA(1)          TO   CLLP-NATUREZA            
           MOVE SELE-CARTEIRA(1)          TO   CLLP-CARTEIRA.           
                                                                        
                                                                        
                                                                        
       FIM-51110-DEVEDOR.                                        EXIT.  
      *---------------------------------------------------------------* 
                                                                        
           EJECT                                                        
                                                                        
      *---------------------------------------------------------------* 
       51120-GRAVAR-MENOR SECTION.                                      
      *---------------------------------------------------------------* 
                                                                        
           IF   SELE-IDENT  =   WRK-DIAS1                               
                ADD   1     TO  CLLP-QTDADE-PRI-AVISO-ATUAL             
                PERFORM         51121-GRAVAR-AGENC01                    
           ELSE                                                         
           IF   SELE-IDENT  =   WRK-DIAS2                               
                ADD   1     TO  CLLP-QTDADE-SEG-AVISO-ATUAL             
                PERFORM         51122-GRAVAR-AGENC02                    
           ELSE                                                         
           IF   SELE-IDENT  =   WRK-DIAS3                               
                PERFORM         51123-GRAVAR-AGENC03                    
           ELSE                                                         
                PERFORM         51124-GRAVAR-AGENC04.                   
                                                                        
                                                                        
       FIM-51120-GRAVAR-MENOR.                                   EXIT.  
      *---------------------------------------------------------------* 
                                                                        
           EJECT                                                        
                                                                        
      *---------------------------------------------------------------* 
       51121-GRAVAR-AGENC01 SECTION.                                    
      *---------------------------------------------------------------* 
                                                                        
           WRITE    REG-AGENC01   FROM   REG-SPC                        
                                                                        
           MOVE     WRK-GRAVACAO  TO     WRK-OPERACAO                   
           PERFORM  14000-TESTAR-FS-AGENC01                             
                                                                        
           ADD            1       TO     ACU-GRV-AGENC01.               
                                                                        
                                                                        
       FIM-51121-GRAVAR-AGENC01.                                 EXIT.  
      *---------------------------------------------------------------* 
                                                                        
           EJECT                                                        
                                                                        
      *---------------------------------------------------------------* 
       51122-GRAVAR-AGENC02 SECTION.                                    
      *---------------------------------------------------------------* 
                                                                        
           WRITE    REG-AGENC02   FROM   REG-SPC                        
                                                                        
           MOVE     WRK-GRAVACAO  TO     WRK-OPERACAO                   
           PERFORM  15000-TESTAR-FS-AGENC02                             
                                                                        
           ADD            1       TO     ACU-GRV-AGENC02.               
                                                                        
                                                                        
       FIM-51122-GRAVAR-AGENC02.                                 EXIT.  
      *---------------------------------------------------------------* 
                                                                        
           EJECT                                                        
                                                                        
      *---------------------------------------------------------------* 
       51123-GRAVAR-AGENC03 SECTION.                                    
      *---------------------------------------------------------------* 
                                                                        
           WRITE    REG-AGENC03   FROM   REG-SPC                        
                                                                        
           MOVE     WRK-GRAVACAO  TO     WRK-OPERACAO                   
           PERFORM  16000-TESTAR-FS-AGENC03                             
                                                                        
           ADD            1       TO     ACU-GRV-AGENC03.               
                                                                        
                                                                        
           EJECT                                                        
                                                                        
      *---------------------------------------------------------------* 
       51124-GRAVAR-AGENC04 SECTION.                                    
      *---------------------------------------------------------------* 
                                                                        
           WRITE    REG-AGENC04   FROM   REG-SPC                        
                                                                        
           MOVE     WRK-GRAVACAO  TO     WRK-OPERACAO                   
           PERFORM  16100-TESTAR-FS-AGENC04                             
                                                                        
           ADD            1       TO     ACU-GRV-AGENC04.               
                                                                        
                                                                        
       FIM-51124-GRAVAR-AGENC04.                                 EXIT.  
      *---------------------------------------------------------------* 
                                                                        
           EJECT                                                        
                                                                        
      *---------------------------------------------------------------* 
       51130-VERIFICAR-AVALISTA SECTION.                                
      *---------------------------------------------------------------* 
                                                                        
           IF   SELE-CGCCPF-AVAL1  GREATER  ZEROS                       
                PERFORM            51131-AVALISTA-1                     
                MOVE     2         TO   SPC-TIPO                        
                MOVE     ZEROS     TO   SPC-COD-ENTR                    
                PERFORM            51120-GRAVAR-MENOR.                  
                                                                        
           IF   SELE-CGCCPF-AVAL1  GREATER  ZEROS                       
                PERFORM            51132-AVALISTA-2                     
                MOVE     ZEROS     TO   SPC-COD-ENTR                    
                MOVE     2    TO   SPC-TIPO                             
                PERFORM            51120-GRAVAR-MENOR.                  
                                                                        
                                                                        
       FIM-51130-VERIF-AVALISTA.                                 EXIT.  
      *---------------------------------------------------------------* 
                                                                        
           EJECT                                                        
                                                                        
      *---------------------------------------------------------------* 
       51131-AVALISTA-1 SECTION.                                        
      *---------------------------------------------------------------* 
                                                                        
           MOVE    SELE-CHAVE              TO     SPC-CHAVE             
           MOVE    SELE-EMPRESA            TO     SPC-EMPRESA           
           MOVE    SELE-AGENCIA            TO     SPC-AGENCIA           
           MOVE    SELE-NUM-CC             TO     SPC-NUM-CC            
           MOVE    SELE-NOMEAVAL1          TO     SPC-NOMERESP          
           MOVE    SELE-CGC-CPF-AVAL1      TO     SPC-CGCCPFRESP        
           MOVE    SELE-AVA-ENDER          TO     SPC-ENDER             
           MOVE    SELE-AVA-NRO            TO     SPC-NRO               
           MOVE    SELE-AVA-COMPL          TO     SPC-COMPL             
           MOVE    SELE-AVA-BAIRRO         TO     SPC-BAIRRO            
           MOVE    SELE-AVA-CIDADE         TO     SPC-CIDADE            
           MOVE    SELE-AVA-UF             TO     SPC-UF                
           MOVE    SELE-AVA-CCEP           TO     SPC-CCEP              
           MOVE    SELE-AVA-CCEP-COMPL TO         SPC-CCEP-COMPL        
                                                                        
           MOVE    SELE-IDENT              TO     SPC-IDENT             
           PERFORM                                                      
               VARYING WRK-IND1        FROM 1 BY 1 UNTIL                
               WRK-IND1                GREATER 11                       
               MOVE SELE-OCORRENCIAS (WRK-IND1)                         
                                          TO SPC-OCORRENCIAS (WRK-IND1) 
           END-PERFORM.                                                 
           MOVE    SOR-NOME-AGENCIA        TO     SPC-NOME-AGENCIA      
           MOVE    SOR-END-AGENCIA         TO     SPC-END-AGENCIA       
           MOVE    SOR-CEP-AGENCIA         TO     SPC-CEP-AGENCIA       
           MOVE    SOR-SIGLA-AGENCIA       TO     SPC-SIGLA-AGENCIA     
           MOVE    SOR-MUNIC-AGENCIA       TO     SPC-MUNIC-AGENCIA     
           MOVE    SOR-CD-AGENCIA          TO     SPC-CD-AGENCIA.       
                                                                        
                                                                        
       FIM-51131-AVALISTA-1.                                     EXIT.  
      *---------------------------------------------------------------* 
                                                                        
           EJECT                                                        
                                                                        
      *---------------------------------------------------------------* 
       51132-AVALISTA-2 SECTION.                                        
      *---------------------------------------------------------------* 
                                                                        
           MOVE   SELE-CHAVE          TO    SPC-CHAVE                   
           MOVE   SELE-EMPRESA        TO    SPC-EMPRESA                 
           MOVE   SELE-AGENCIA        TO    SPC-AGENCIA                 
           MOVE   SELE-NUM-CC         TO    SPC-NUM-CC                  
           MOVE   SELE-NOMEAVAL2      TO    SPC-NOMERESP                
           MOVE   SELE-CGC-CPF-AVAL   TO    SPC-CGCCPFRESP              
           MOVE   SOR-NOME-AGENCIA    TO    SPC-NOME-AGENCIA            
           MOVE   SOR-END-AGENCIA     TO    SPC-END-AGENCIA             
           MOVE   SOR-CEP-AGENCIA     TO    SPC-CEP-AGENCIA             
           MOVE   SOR-SIGLA-AGENCIA   TO    SPC-SIGLA-AGENCIA           
           MOVE   SOR-MUNIC-AGENCIA   TO    SPC-MUNIC-AGENCIA           
           MOVE   SOR-CD-AGENCIA      TO    SPC-CD-AGENCIA              
           MOVE   SELE-IDENT          TO    SPC-IDENT                   
           PERFORM                                                      
               VARYING WRK-IND1        FROM 1 BY 1 UNTIL                
               WRK-IND1                GREATER 11                       
               MOVE SELE-OCORRENCIAS (WRK-IND1)                         
                                          TO SPC-OCORRENCIAS (WRK-IND1) 
           END-PERFORM.                                                 
           MOVE   SPACES              TO    SPC-ENDER                   
                                            SPC-NRO                     
                                            SPC-COMPL                   
                                            SPC-BAIRRO                  
                                            SPC-CIDADE                  
                                            SPC-UF                      
           MOVE   ZEROS               TO    SPC-CCEP                    
                                            SPC-CCEP-COMPL.             
                                                                        
                                                                        
       FIM-51132-AVALISTA-2.                                     EXIT.  
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
       53000-PROCESSAR-SORT SECTION.                                    
      *---------------------------------------------------------------* 
                                                                        
           INITIALIZE REG-CLLP.                                         
                                                                        
           IF    SELE-DEV-CCEP       LESS 1001                          
                 PERFORM             51110-DEVEDOR                      
                 MOVE      2    TO   SPC-TIPO                           
                 MOVE   ZEROS   TO   SPC-COD-ENTR                       
                 PERFORM             51120-GRAVAR-MENOR                 
                 PERFORM             51130-VERIFICAR-AVALISTA           
                 PERFORM             51000-LER-SORTWK01                 
                 PERFORM             53010-GRAVAR-MVAVISO               
                 GO  TO        FIM-53000-PROCESSAR-SORT.                
                                                                        
                                                                        
           IF  SELE-DEV-CCEP NOT EQUAL CAD-CEP                          
               PERFORM        52000-LER-CADCEP  UNTIL                   
                              WRK-FS-CADCEP     EQUAL  '10'             
                        OR    SELE-DEV-CCEP EQUAL   CAD-CEP             
                        OR    CAD-CEP       GREATER SELE-DEV-CCEP.      
                                                                        
           IF    CAD-CEP     EQUAL     SELE-DEV-CCEP                    
                 PERFORM               53100-CHAVES-IGUAIS              
           ELSE                                                         
                 PERFORM               51110-DEVEDOR                    
                 MOVE  CAD-COD-ENTR TO SPC-COD-ENTR                     
                 MOVE    2    TO       SPC-TIPO                         
                 PERFORM               51120-GRAVAR-MENOR               
                 PERFORM               51130-VERIFICAR-AVALISTA.        
                                                                        
           PERFORM                     53010-GRAVAR-MVAVISO.            
                                                                        
           PERFORM                     51000-LER-SORTWK01.              
                                                                        
                                                                        
       FIM-53000-PROCESSAR-SORT.                          EXIT.         
      *---------------------------------------------------------------* 
                                                                        
           EJECT                                                        
                                                                        
       53010-GRAVAR-MVAVISO SECTION.                                    
      *---------------------------------------------------------------* 
           WRITE REG-CLLP.                                              
                                                                        
           MOVE     WRK-GRAVACAO  TO     WRK-OPERACAO                   
           PERFORM  16210-TESTAR-FS-MVAVISO                             
                                                                        
           ADD            1       TO     ACU-GRV-MVAVISO.               
                                                                        
       FIM-53010-GRAVAR-MVAVISO.                          EXIT.         
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       53100-CHAVES-IGUAIS SECTION.                                     
      *---------------------------------------------------------------* 
                                                                        
           PERFORM                   51110-DEVEDOR                      
                                                                        
           IF   CAD-COD-ENTR   EQUAL  1  OR  3                          
                MOVE    2      TO    SPC-TIPO                           
                MOVE   CAD-COD-ENTR  TO  SPC-COD-ENTR                   
                PERFORM              51120-GRAVAR-MENOR                 
           ELSE                                                         
                MOVE    1      TO    SPC-TIPO                           
                MOVE   CAD-COD-ENTR  TO  SPC-COD-ENTR                   
                PERFORM              53110-GRAVAR-MAIOR.                
                                                                        
           PERFORM                   51130-VERIFICAR-AVALISTA.          
                                                                        
                                                                        
       FIM-53100-CHAVES-IGUAIS.                                  EXIT.  
      *---------------------------------------------------------------* 
                                                                        
           EJECT                                                        
                                                                        
      *---------------------------------------------------------------* 
       53110-GRAVAR-MAIOR SECTION.                                      
      *---------------------------------------------------------------* 
                                                                        
JGA   ***------------------------------------------------------------***
JGA   ***   MONTAGEM DO CAMPO CODIGO DE BARRAS PARA ENDERECOS DE     ***
JGA   ***   ORIGEM = 5 (LOCALIZADORA) E CLIENTES TIPO = 1 (DEVE-     ***
JGA   ***   DORES).                                                  ***
JGA   ***   GRAVACAO DO ARQUIVO DE CONTROLE DE ENVIO PARA CORREIO    ***
JGA   ***   DESTES MESMOS TIPOS.                                     ***
JGA   ***------------------------------------------------------------***
                                                                        
JGA        MOVE SELE-CHAVE            TO  WRK-CGC-CORR                  
JGA        MOVE WRK-NUMCGC-CORR       TO  SPC-CB-NUMCPF                 
JGA        MOVE WRK-FILCGC-CORR       TO  SPC-CB-FILCPF                 
JGA        MOVE WRK-CTRCGC-CORR       TO  SPC-CB-CTRCPF                 
JGA        MOVE CLLPPI-DTMOV          TO  SPC-CB-DT-MOVTO               
JGA        MOVE SELE-AGENCIA          TO  SPC-CB-AGENCIA                
JGA        MOVE SELE-TPO-CLIENTE      TO  SPC-CB-TPO-CLIENTE            
JGA        PERFORM                    53116-GRAVAR-CTRLCORR.            
                                                                        
           IF   SELE-IDENT       EQUAL    WRK-DIAS1                     
                ADD   1          TO       CLLP-QTDADE-PRI-AVISO-ATUAL   
                PERFORM                   53111-GRAVAR-CORRE01          
                PERFORM 53115-ACUMULAR-AGENDIA                          
           ELSE                                                         
           IF   SELE-IDENT       EQUAL    WRK-DIAS2                     
                ADD   1          TO       CLLP-QTDADE-SEG-AVISO-ATUAL   
                PERFORM                   53112-GRAVAR-CORRE02          
                PERFORM 53115-ACUMULAR-AGENDIA                          
           ELSE                                                         
           IF   SELE-IDENT       EQUAL    WRK-DIAS3                     
                PERFORM                   53113-GRAVAR-CORRE03          
           ELSE                                                         
                PERFORM                   53114-GRAVAR-CORRE04.         
                                                                        
                                                                        
       FIM-53110-GRAVAR-MAIOR.                                   EXIT.  
      *---------------------------------------------------------------* 
                                                                        
           EJECT                                                        
                                                                        
      *---------------------------------------------------------------* 
       53111-GRAVAR-CORRE01 SECTION.                                    
      *---------------------------------------------------------------* 
                                                                        
           WRITE    REG-CORRE01   FROM   REG-SPC                        
                                                                        
           MOVE     WRK-GRAVACAO  TO     WRK-OPERACAO                   
           PERFORM  16210-TESTAR-FS-MVAVISO.                            
                                                                        
           ADD            1       TO     ACU-GRV-CORRE01.               
                                                                        
                                                                        
       FIM-53111-GRAVAR-CORRE01.                                 EXIT.  
      *---------------------------------------------------------------* 
                                                                        
           EJECT                                                        
                                                                        
      *---------------------------------------------------------------* 
       53112-GRAVAR-CORRE02 SECTION.                                    
      *---------------------------------------------------------------* 
                                                                        
           WRITE    REG-CORRE02   FROM   REG-SPC                        
                                                                        
           MOVE     WRK-GRAVACAO  TO     WRK-OPERACAO                   
           PERFORM  18000-TESTAR-FS-CORRE02                             
                                                                        
           ADD            1       TO     ACU-GRV-CORRE02.               
                                                                        
                                                                        
       FIM-53112-GRAVAR-CORRE02.                                 EXIT.  
      *---------------------------------------------------------------* 
                                                                        
           EJECT                                                        
                                                                        
      *---------------------------------------------------------------* 
       53113-GRAVAR-CORRE03 SECTION.                                    
      *---------------------------------------------------------------* 
                                                                        
           WRITE    REG-CORRE03   FROM   REG-SPC                        
                                                                        
           MOVE     WRK-GRAVACAO  TO     WRK-OPERACAO                   
           PERFORM  19000-TESTAR-FS-CORRE03                             
                                                                        
           ADD           1        TO     ACU-GRV-CORRE03.               
                                                                        
                                                                        
       FIM-53113-GRAVAR-CORRE03.                                 EXIT.  
      *---------------------------------------------------------------* 
                                                                        
           EJECT                                                        
                                                                        
      *---------------------------------------------------------------* 
       53114-GRAVAR-CORRE04 SECTION.                                    
      *---------------------------------------------------------------* 
                                                                        
           WRITE    REG-CORRE04   FROM   REG-SPC                        
                                                                        
           MOVE     WRK-GRAVACAO  TO     WRK-OPERACAO                   
           PERFORM  19100-TESTAR-FS-CORRE04                             
                                                                        
           ADD           1        TO     ACU-GRV-CORRE04.               
                                                                        
                                                                        
       FIM-53114-GRAVAR-CORRE04.                                 EXIT.  
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
HEXA  * HX_IF  (WRK-AGENCIA  GREATER 5000)                              
HEXA  *                                                                 
           MOVE     CARTA-AGENCIA    TO WRK-MESUX0-AGENCIA              
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
               MOVE  CARTA-AGENCIA  TO  AUX-AGENCIA                     
               MOVE  SPC-CHAVE-CGC  TO  AUX-CHAVE-CGC                   
               COMPUTE WRK-AGENDIA-QTDEAVISOS (AUX-AGENCIA) =           
                       WRK-AGENDIA-QTDEAVISOS (AUX-AGENCIA) + 1         
           ELSE                                                         
               IF  (AUX-CHAVE-CGC  NOT  EQUAL  SPC-CHAVE-CGC)           
                    MOVE  SPC-CHAVE-CGC   TO  AUX-CHAVE-CGC             
                    MOVE  CARTA-AGENCIA   TO AUX-AGENCIA                
                    COMPUTE WRK-AGENDIA-QTDEAVISOS (AUX-AGENCIA) =      
                           WRK-AGENDIA-QTDEAVISOS (AUX-AGENCIA) + 1.    
                                                                        
           MOVE ZEROS                  TO WRK-VALOR-RESGATE.            
                                                                        
           PERFORM                                                      
               VARYING WRK-IND1        FROM 1 BY 1 UNTIL                
               WRK-IND1                GREATER 11                       
               ADD SELE-RESGATE (WRK-IND1)                              
                                       TO WRK-VALOR-RESGATE             
           END-PERFORM.                                                 
                                                                        
           COMPUTE WRK-AGENDIA-TOTALAVISOS (AUX-AGENCIA) =              
                   WRK-AGENDIA-TOTALAVISOS (AUX-AGENCIA) +              
                   WRK-VALOR-RESGATE.                                   
                                                                        
       FIM-53115-ACU.                                 EXIT.             
      *---------------------------------------------------------------* 
                                                                        
           EJECT                                                        
      *---------------------------------------------------------------* 
JGA    53116-GRAVAR-CTRLCORR  SECTION.                                  
      *---------------------------------------------------------------* 
                                                                        
JGA        MOVE SELE-CHAVE             TO  WRK-CGC-CORR.                
JGA        MOVE WRK-NUMCGC-CORR        TO  HIST-NUMCPF.                 
JGA        MOVE WRK-FILCGC-CORR        TO  HIST-FILCPF.                 
JGA        MOVE WRK-CTRCGC-CORR        TO  HIST-CTRCPF.                 
CRUZ       MOVE ZEROS                  TO  HIST-NUMCPF-DEV              
CRUZ                                       HIST-FILCPF-DEV              
CRUZ                                       HIST-CTRCPF-DEV.             
JGA        MOVE CLLPPI-DTMOV           TO  HIST-DT-MOVTO.               
JGA        MOVE SELE-AGENCIA           TO  HIST-AGENCIA.                
JGA        MOVE SELE-TPO-CLIENTE       TO  HIST-TPO-CLIENTE.            
JGA        MOVE SELE-NUM-CC            TO  HIST-CTA-CORRENT.            
SAN        MOVE SELE-TPO-LOGRA         TO  HIST-TPO-ENDEREC.            
JGA        MOVE SELE-ORIG-ENDER        TO  HIST-ORIG-ENDEREC.           
                                                                        
JGA        MOVE SELE-DATA-OCORR        TO  WRK-AUX-DTMOV.               
JGA        MOVE WRK-DTMOV-DIA          TO  HIST-DT-ATULZ(01:02).        
JGA        MOVE WRK-DTMOV-MES          TO  HIST-DT-ATULZ(04:02).        
JGA        MOVE WRK-DTMOV-ANO          TO  HIST-DT-ATULZ(07:04).        
JGA        MOVE '.'                    TO  HIST-DT-ATULZ(03:01)         
JGA                                        HIST-DT-ATULZ(06:01).        
                                                                        
SAN        MOVE SELE-CSIT-LOCALIZ      TO  HIST-SIT-LOCALIZ.            
JGA        MOVE SPACES                 TO  HIST-RESERVA.                
                                                                        
JGA        WRITE    REG-HISTAVIS.                                       
                                                                        
JGA        MOVE     WRK-GRAVACAO       TO  WRK-OPERACAO                 
JGA        PERFORM  19300-TESTAR-FS-CTRLCORR.                           
                                                                        
JGA        ADD           1             TO  ACU-GRV-CTRLCORR.            
                                                                        
JGA    FIM-53116-GRAVAR-CTRLCORR.                                EXIT.  
      *---------------------------------------------------------------* 
                                                                        
           EJECT                                                        
                                                                        
      *---------------------------------------------------------------* 
       60000-TESTAR-RC-SORT SECTION.                                    
      *---------------------------------------------------------------* 
                                                                        
           IF    SORT-RETURN NOT EQUAL ZEROS                            
                 MOVE SORT-RETURN           TO WRK-SORT-RETURN          
                 DISPLAY '************** CLLP7625 *************'        
                 DISPLAY '*   ERRO NO PROCESSAMENTO DO SORT   *'        
                 DISPLAY '*         SORT-RETURN  = ' WRK-SORT-RETURN    
                                                      '       *'        
                 DISPLAY '************** CLLP7625 *************'        
                 CALL    'ILBOABN0'      USING   WRK-ABEND.             
                                                                        
                                                                        
       FIM-60000-TESTAR-RC-SORT.                                 EXIT.  
      *---------------------------------------------------------------* 
                                                                        
           EJECT                                                        
                                                                        
      *---------------------------------------------------------------* 
       70000-IMPRIMIR-RELATO SECTION.                                   
      *---------------------------------------------------------------* 
                                                                        
           PERFORM 71000-TRATAR-DATA                                    
                                                                        
           MOVE    WRK-GRAVACAO     TO     WRK-OPERACAO                 
                                                                        
           MOVE    ACU-LIDOS        TO     LT1-TOT-LIDOS                
           MOVE    ACU-GRV-AGENC01  TO     LT2-AGENC01                  
           MOVE    ACU-GRV-AGENC02  TO     LT3-AGENC02                  
           MOVE    ACU-GRV-AGENC03  TO     LT4-AGENC03                  
           MOVE    ACU-GRV-AGENC04  TO     LT8-AGENC04                  
           MOVE    ACU-GRV-CORRE01  TO     LT5-CORRE01                  
           MOVE    ACU-GRV-CORRE02  TO     LT6-CORRE02                  
           MOVE    ACU-GRV-CORRE03  TO     LT7-CORRE03                  
           MOVE    ACU-GRV-CORRE04  TO     LT9-CORRE04                  
                                                                        
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
                                                                        
           WRITE   REG-RELATO   FROM   LINTOT3                          
           PERFORM 19000-A-TESTAR-FS-RELATO                             
                                                                        
      **   WRITE   REG-RELATO   FROM   LINTOT4                          
      **   PERFORM 19000-A-TESTAR-FS-RELATO                             
                                                                        
      **   WRITE   REG-RELATO   FROM   LINTOT8                          
      **   PERFORM 19000-A-TESTAR-FS-RELATO                             
                                                                        
           WRITE   REG-RELATO   FROM   LINTOT5                          
           PERFORM 19000-A-TESTAR-FS-RELATO                             
                                                                        
           WRITE   REG-RELATO   FROM   LINTOT6                          
           PERFORM 19000-A-TESTAR-FS-RELATO.                            
                                                                        
      **   WRITE   REG-RELATO   FROM   LINTOT7                          
      **   PERFORM 19000-A-TESTAR-FS-RELATO                             
                                                                        
      **   WRITE   REG-RELATO   FROM   LINTOT9                          
      **   PERFORM 19000-A-TESTAR-FS-RELATO.                            
                                                                        
                                                                        
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
                                                                        
JGA        DISPLAY '****************  CLLP7625  *******************'    
JGA        DISPLAY '*                                             *'    
JGA        DISPLAY '*  CONTROLE DE LOTES  -  ENVIO CORREIO        *'    
JGA        DISPLAY '*                                             *'    
JGA        DISPLAY '* TOTAL DE DEVEDORES COM ENDERECO ORIGEM 5    *'    
JGA        DISPLAY '* GRAVADOS ..........: ' ACU-GRV-CTRLCORR           
JGA                                                 '             *'    
JGA        DISPLAY '*                                             *'    
JGA        DISPLAY '*****************  CLLP7625  ******************'.   
                                                                        
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
                                                                        
                                                                        
                                                                        
           CLOSE        PARMCLLP                                        
                        CADCARTA                                        
                        MESTRE                                          
                        CADCEP                                          
                        ARQDATA                                         
                        AGENC01                                         
                        AGENC02                                         
                        AGENC03                                         
                        AGENC04                                         
                        AGENDIA                                         
                        MVAVISO                                         
                        CORRE01                                         
                        CORRE02                                         
                        CORRE03                                         
                        CORRE04                                         
                        RELATO                                          
                                                                        
           MOVE         WRK-FECHAMENTO    TO  WRK-OPERACAO              
           PERFORM      10000-TESTAR-FILE-STATUS                        
           STOP RUN.                                                    
                                                                        
                                                                        
       FIM-80000-FINALIZAR.                                      EXIT.  
      *---------------------------------------------------------------* 
                                                                        
           EJECT                                                        
                                                                        
      ******************************************************************
      *                   FIM  DO  PROGRAMA  CLLP7625                  *
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
HEXA       MOVE    'CLLP7625'      TO          ERR-PGM.                 
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

      *===============================================================* 
       IDENTIFICATION DIVISION.                                         
      *===============================================================* 
      *                                                                 
       PROGRAM-ID. CLLPG656.                                            
       AUTHOR.     TARITA.                                              
      *===============================================================* 
      *                   C P M   S I S T E M A S                     * 
      *---------------------------------------------------------------* 
      *                                                               * 
      *      PROGRAMA     : CLLPG656                                  * 
      *      PROGRAMADORA : TARITA          -    CPM                  * 
      *      ANALISTA     : LOURIVAL        -    GP.82                * 
      *      SUPERVISORA  : CRIS            -    CPM                  * 
      *      DATA         : 10/10/1997                                * 
      *                                                               * 
      *      OBJETIVO     :                                           * 
      *        LER ARQUIVO DE SPC E SELECIONAR REGISTROS COM TEMPO    * 
      *            DE VENCIDO IGUAL A 40 OU 20 OU 5 DIAS UTEIS.       * 
      *                                                               * 
      *                                                               * 
      *      ARQUIVOS:                                                * 
      *         DDNAME                           INCLUDE/BOOK         * 
      *         TABELAL                                               * 
      *         CADSPC                             I#CLLPCI           * 
      *         PARMCLLP                                              * 
      *         ARQDATA                                               * 
      *         CADCARTA                                              * 
      *         RELATO                                                * 
      *         SORTWK01                                              * 
      *                                                               * 
      *      BANCO DE DADOS:                                          * 
      *         DB2                                                   * 
      *           TABLE                          INCLUDE/BOOK         * 
      *           DB2PRD.V01LPCL_NATUREZ           LPCLV009           * 
      *                                                               * 
      *                                                               * 
      *      MODULOS CHAMADOS:                                        * 
      *                                                               * 
      *        POOL7600                                               * 
      *                                                               * 
      *        POOL1050                                               * 
      *                                                               * 
      *        POOL1285                                               * 
      *                                                               * 
      ************************************************                  
      *                    ALTERACAO                 *                  
      ************************************************                  
      *                                              *                  
      *  PROGRAMADORA -  DENISE         - C.P.M.     *                  
      *  SUPERVISOR   -  TESTA          - C.P.M.     *                  
      *  ANALISTA     -  TOMOKO         - CPM/GP.82  *                  
      *  DATA         -  MARCO   /1998               *                  
      *                                              *                  
      *  OBJETIVOS    :                              *                  
      *  ==============                              *                  
      *                                              *                  
      *  - CONTROLE DE NEGATIVACOES                  *                  
      *    INCLUIR O PROCESSAMENTO DA TABELA LPCLB037*                  
      *                                              *                  
      ************************************************                  
      *                 ALTERACAO                    *                  
      ************************************************                  
      *                                              *                  
      *  PROGRAMADORA -  ANDREA  - STEFANINI         *                  
      *  SUPERVISOR   -  RICARDO - STEFANINI         *                  
      *  ANALISTA     -  VALDIR  - ORIGIN            *                  
      *  DATA         -  MAIO    /2000               *                  
      *                                              *                  
      *  OBJETIVOS    :                              *                  
      *  ==============                              *                  
      *                                              *                  
      *  - ADEQUACAO A TABELA LPCLB037               *                  
      *                                              *                  
      ************************************************                  
      *                                                                 
      *** ==================== ULTIMA  ALTERACAO ================== *** 
      *                                                               * 
      *   PROGRAMADOR.: MAURICIO - CPM.                               * 
      *   ANALISTAS...: ARMANDO/MAURICIO - CPM.                       * 
      *   DATA........: MAIO/2001.                                    * 
      *                                                               * 
      *   OBJETIVO....: ESTA SENDO INCLUIDO O ARQUIVO PARMCLLP.       * 
      *                 PARA PARAMETRIZACAO DO SISTEMA.               * 
      *                                                               * 
      *===============================================================* 
BRQ141*---------------------------------------------------------------* 
BRQ141* MAIO/2012 - (BRQ) - TRATAMENTO DE CARTEIRA  ALFANUMERICA      * 
BRQ141*---------------------------------------------------------------* 
      *================================================================*
      *               B R Q     I T     S E R V I C E S                *
      *================================================================*
      *                       A L T E R A C A O                        *
      *----------------------------------------------------------------*
      *    PROGRAMADOR.:  HENRIQUE GUIMARAES      - BRQ                *
      *    ANALISTA....:  HENRIQUE GUIMARAES      - BRQ                *
      *    DATA........:  15/04/2014                                   *
      *----------------------------------------------------------------*
      *    OBJETIVO....:  ADAPTACOES PARA A LEI DA TRANSPARENCIA       *
      *    INCLUIDOS CAMPOS LT NA I#CLLPC0                             *
      *    TROCA BOOK I#CLLPCO(600) POR I#CLLPC0(700)                  *
      *    TROCA BOOK INTERNA(840) POR I#CLLPPL(1530)                  *
      *    AJUSTE AREA SORT DE 600 PRA 700.                            *
      *    ALTERAR OCORRENCIAS DE 7 PARA 11                            *
      *    NAO ACUMULAR NATUREZA, CASO EXISTAM MAIS DE                 *
      *                 11 OCORRENCIAS CRIAR NOVO REGISTROS.           *
      *    RETIRADA DA TABELA DB2 LPCLV009.                            *
      *    PROJETO 13-0358                                             *
      *================================================================*
                                                                        
           EJECT                                                        
      *===============================================================* 
       ENVIRONMENT DIVISION.                                            
      *===============================================================* 
                                                                        
      *---------------------------------------------------------------* 
       CONFIGURATION SECTION.                                           
      *---------------------------------------------------------------* 
                                                                        
       SPECIAL-NAMES.                                                   
           DECIMAL-POINT IS COMMA.                                      
                                                                        
           EJECT                                                        
      *---------------------------------------------------------------* 
       INPUT-OUTPUT SECTION.                                            
      *---------------------------------------------------------------* 
                                                                        
       FILE-CONTROL.                                                    
                                                                        
           SELECT  TABELAL ASSIGN TO UT-S-TABELAL                       
                      FILE STATUS IS WRK-FS-TABELAL.                    
                                                                        
           SELECT PARMCLLP ASSIGN TO UT-S-PARMCLLP                      
                      FILE STATUS IS WRK-FS-PARMCLLP.                   
                                                                        
           SELECT CADSPC   ASSIGN TO UT-S-CADSPC                        
                      FILE STATUS IS WRK-FS-CADSPC.                     
                                                                        
           SELECT ARQDATA  ASSIGN TO UT-S-ARQDATA                       
                      FILE STATUS IS WRK-FS-ARQDATA.                    
                                                                        
           SELECT CADCARTA ASSIGN TO UT-S-CADCARTA                      
                      FILE STATUS IS WRK-FS-CADCARTA.                   
                                                                        
           SELECT BLQREST  ASSIGN TO UT-S-BLQREST                       
                      FILE STATUS IS WRK-FS-BLQREST.                    
                                                                        
           SELECT   RELATO ASSIGN TO UT-S-RELATO                        
                      FILE STATUS IS WRK-FS-RELATO.                     
                                                                        
                                                                        
           SELECT SORTWK01 ASSIGN TO UT-S-SORTWK01.                     
           EJECT                                                        
      *===============================================================* 
       DATA DIVISION.                                                   
      *===============================================================* 
                                                                        
      *---------------------------------------------------------------* 
       FILE SECTION.                                                    
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
      *    INPUT   :  ARQUIVO DE ENTRADA TABELAL                      * 
      *               ORG. SEQUENCIAL   -   LRECL  =  043             * 
      *---------------------------------------------------------------* 
                                                                        
       FD  TABELAL                                                      
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  REG-TABELAL.                                                 
           03  B037-CCGC-CPF           PIC S9(09) COMP-3.               
           03  B037-CFLIAL-CGC         PIC S9(05) COMP-3.               
           03  B037-CCTRL-CPF-CGC      PIC S9(02) COMP-3.               
           03  B037-CREST-BLOQ-OPER    PIC S9(01) COMP-3.               
           03  B037-CJUNC-DEPDC-BDSCO  PIC S9(05) COMP-3.               
           03  B037-CCTA-CORR          PIC S9(07) COMP-3.               
           03  B037-CTPO-NATUZ-OPER    PIC S9(03) COMP-3.               
           03  B037-CCART              PIC  X(03).                      
           03  B037-CSGL-UF            PIC  X(02).                      
           03  B037-CINDCD-AVISO-COBR  PIC  X(01).                      
           03  B037-CINDCD-IMPED-REST  PIC  X(01).                      
           03  B037-CINDCD-SERASA-SPC  PIC  X(01).                      
           03  B037-DULT-ATULZ         PIC  X(10).                      
           03  B037-CFUNC-BDSCO        PIC S9(09) COMP-3.               
                                                                        
      *---------------------------------------------------------------* 
      *    INPUT:     ORG. SEQUENCIAL   -   LRECL = 250               * 
      *---------------------------------------------------------------* 
                                                                        
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
      *    INPUT:     ORG. SEQUENCIAL   -   LRECL = 700               * 
      *---------------------------------------------------------------* 
                                                                        
       FD  CADSPC                                                       
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
           COPY 'I#CLLPC0'.
                                                                        
      *---------------------------------------------------------------* 
      *    INPUT:     ORG. SEQUENCIAL   -   LRECL = 060               * 
      *---------------------------------------------------------------* 
                                                                        
       FD  ARQDATA                                                      
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
           COPY 'I#CLLPGA'.                                             
                                                                        
      *---------------------------------------------------------------* 
      *    OUTPUT:    ORG. SEQUENCIAL   -   LRECL = 1530              * 
      *---------------------------------------------------------------* 
                                                                        
       FD  CADCARTA                                                     
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
           COPY 'I#CLLPPL'.                                             
                                                                        
      *---------------------------------------------------------------* 
      *    OUTPUT:    ORG. SEQUENCIAL   -   LRECL = 040               * 
      *---------------------------------------------------------------* 
                                                                        
       FD  BLQREST                                                      
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  FD-BLQREST.                                                  
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
                                                                        
      *---------------------------------------------------------------* 
      *    OUTPUT:    ORG. SEQUENCIAL   -   LRECL = 132               * 
      *---------------------------------------------------------------* 
                                                                        
       FD  RELATO                                                       
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  REG-REL                     PIC X(132).                      
                                                                        
                                                                        
      *---------------------------------------------------------------* 
      *    I-O:       ARQUIVO TEMPORARIO DE SORT                      * 
      *               ORG. SEQUENCIAL   -   LRECL = 700               * 
      *---------------------------------------------------------------* 
                                                                        
       SD  SORTWK01.                                                    
                                                                        
      *---------------------------------------------------------------* 
      *    COPIA DA BOOK I#CLLPC0                                     * 
      *                  *** FORMATO DAS DATAS AAAAMMDD ***           * 
      *---------------------------------------------------------------* 
       01  REG-SORT.                                                    
           02  SOR-CGC.                                                 
               03  SOR-NUMCGC                  PIC 9(09)       COMP-3.  
               03  SOR-FILCGC                  PIC 9(05)       COMP-3.  
               03  SOR-DIGCGC                  PIC 9(03)       COMP-3.  
           02  SOR-NATURE                      PIC X(02).               
           02  SOR-VCMTO                       PIC 9(09)       COMP-3.  
           02  SOR-EMPRESA-CATU                PIC 9(05)       COMP-3.  
           02  SOR-AGENCIA-CATU                PIC 9(05)       COMP-3.  
           02  SOR-NUM-CC-CATU                 PIC 9(07)       COMP-3.  
           02  SOR-CARTEIRA-CATU               PIC X(03).               
           02  SOR-CONTRATO-CATU               PIC 9(07)       COMP-3.  
           02  SOR-RESGATE                     PIC 9(11)V99    COMP-3.  
           02  SOR-VALOR-VENCIDOS-CATU         PIC 9(11)V99    COMP-3.  
           02  SOR-VALOR-VINCENDOS-CATU        PIC 9(11)V99    COMP-3.  
           02  SOR-NOMERESP-CATU               PIC X(40).               
           02  SOR-CGCCPFRESP-CATU.                                     
               03  SOR-NUMCGC-CATU             PIC 9(09)       COMP-3.  
               03  SOR-FILCGC-CATU             PIC 9(05)       COMP-3.  
               03  SOR-CTRCGC-CATU             PIC 9(03)       COMP-3.  
           02  SOR-DATA-NASCIMEN-CATU          PIC 9(09)       COMP-3.  
           02  SOR-NATURALIDADE-CATU           PIC X(30).               
           02  SOR-UF-ORIGEM-CATU              PIC X(02).               
           02  SOR-NOME-CONJUGE-CATU           PIC X(40).               
           02  SOR-DEV-ENDER-CATU              PIC X(40).               
           02  SOR-DEV-NRO-CATU                PIC X(07).               
           02  SOR-DEV-COMPL-CATU              PIC X(20).               
           02  SOR-DEV-BAIRRO-CATU             PIC X(20).               
           02  SOR-DEV-CIDADE-CATU             PIC X(30).               
           02  SOR-DEV-UF-CATU                 PIC X(02).               
           02  SOR-DEV-CCEP-CATU               PIC 9(05).               
           02  SOR-DEV-CCEP-COMPL-CATU         PIC 9(03).               
           02  SOR-NOMEAVAL1-CATU              PIC X(40).               
           02  SOR-CGC-CPF-AVAL1-CATU.                                  
               03 SOR-CGCCPFAVAL1-CATU         PIC 9(09)       COMP-3.  
               03 SOR-FILCGC-AVAL1-CATU        PIC 9(05)       COMP-3.  
               03 SOR-CTRCGC-AVAL1-CATU        PIC 9(03)       COMP-3.  
           02  SOR-AVA-ENDER-CATU              PIC X(40).               
           02  SOR-AVA-NRO-CATU                PIC X(07).               
           02  SOR-AVA-COMPL-CATU              PIC X(20).               
           02  SOR-AVA-BAIRRO-CATU             PIC X(20).               
           02  SOR-AVA-CIDADE-CATU             PIC X(30).               
           02  SOR-AVA-UF-CATU                 PIC X(02).               
           02  SOR-AVA-CCEP-CATU               PIC 9(05).               
           02  SOR-AVA-CCEP-COMPL-CATU         PIC 9(03).               
           02  SOR-NOMEAVAL2-CATU              PIC X(40).               
           02  SOR-CGC-CPF-AVAL2-CATU.                                  
               03 SOR-CGCCPFAVAL2-CATU         PIC 9(09)       COMP-3.  
               03 SOR-FILCGC-AVAL2-CATU        PIC 9(05)       COMP-3.  
               03 SOR-CTRCGC-AVAL2-CATU        PIC 9(03)       COMP-3.  
           02  SOR-CHAVE-LP-CL-CATU.                                    
               03  SOR-NATUREZA-OPER-CATU      PIC X(02).               
               03  FILLER                      PIC 9(10).               
           02  SOR-NUMERO-CL-CATU              PIC 9(15).               
           02  SOR-DATA-OCORR-CATU             PIC 9(09)       COMP-3.  
           02  SOR-COD-NATUREZA-CATU           PIC 9(03).               
           02  SOR-DTECL-CATU                  PIC 9(09)       COMP-3.  
           02  SOR-DTBCL-CATU                  PIC 9(09)       COMP-3.  
           02  SOR-DTELP-CATU                  PIC 9(09)       COMP-3.  
           02  SOR-DTBLP-CATU                  PIC 9(09)       COMP-3.  
           02  SOR-ID-LPCL-CATU                PIC X(02).               
           02  SOR-TIPBX-CATU                  PIC X(01).               
           02  SOR-TIPO-CATU                   PIC 9(01).               
           02  SOR-TPO-CLIENTE                 PIC X(01).               
           02  SOR-ORIG-ENDER                  PIC X(01).               
           02  SOR-TPO-LOGRA                   PIC X(01).               
           02  SOR-SIT-LOCALIZ                 PIC X(01).               
           02  FILLER                          PIC X(01).               
           02  SOR-LT.                                                  
AQUI          03 SOR-TAXA-CONTRATO            PIC 9(02)V9(06) COMP-3.   
              03 SOR-VR-REMUNERATORIO          PIC S9(13)V99   COMP-3.  
              03 SOR-VALOR-MORATORIO           PIC S9(13)V99   COMP-3.  
              03 SOR-VALOR-MULTA               PIC S9(13)V99   COMP-3.  
              03 SOR-DESP-JUD-CUSTAS           PIC S9(11)V99   COMP-3.  
              03 SOR-HONORARIOS                PIC S9(11)V99   COMP-3.  
              03 SOR-VL-TOTAL-DIVIDA           PIC S9(15)V99   COMP-3.  
              03 SOR-VL-TAXA-TARIFA            PIC S9(15)V99   COMP-3.  
              03 FILLER                        PIC  X(39).              
                                                                        
      *---------------------------------------------------------------* 
       WORKING-STORAGE SECTION.                                         
      *---------------------------------------------------------------* 
                                                                        
       77  FILLER                      PIC X(32)        VALUE           
           '* INICIO DA WORKING CLLPG656 *'.                            
                                                                        
       77  WRK-IND                     PIC 9(02)   VALUE ZEROS.         
                                                                        
       77  WRK-BATCH                   PIC X(08)        VALUE 'BATCH'.  
       77  WRK-ABEND                   PIC S9(04) COMP  VALUE +1111.    
                                                                        
       77  CD-PARM-DIA1                PIC 9(02)        VALUE ZEROS.    
       77  WRK-DIA-AUX                 PIC 9(02)        VALUE ZEROS.    
       77  ACU-LIDOS-CADSPC            PIC 9(07) COMP-3 VALUE ZEROS.    
       77  ACU-LIDOS-TABELAL           PIC 9(07) COMP-3 VALUE ZEROS.    
       77  ACU-GRAV-CADCARTA           PIC 9(07) COMP-3 VALUE ZEROS.    
       77  ACU-GRAV-BLQREST            PIC 9(07) COMP-3 VALUE ZEROS.    
       77  ACU-EXCLUIDOS               PIC 9(07) COMP-3 VALUE ZEROS.    
       77  ACU-DESPREZADO              PIC 9(07) COMP-3 VALUE ZEROS.    
       77  WRK-CONTA-99                PIC 9(07) COMP-3 VALUE ZEROS.    
       77  WRK-DIA-PARM                PIC 9(03)        VALUE ZEROS.    
                                                                        
       01  WRK-IDENT                   PIC S9(03)       VALUE ZEROS.    
       01  WRK-IDENT-R REDEFINES                                        
           WRK-IDENT.                                                   
           03  FILLER                  PIC  X(01).                      
           03  WRK-IDENT-N             PIC  9(02).                      
                                                                        
       77  WRK-FIM-CADSPC              PIC X(01)        VALUE  'N'.     
       77  WRK-FIM-ARQSORT             PIC X(01)        VALUE  'N'.     
                                                                        
       77  WRK-PRIMEIRA-VEZ            PIC X(01)        VALUE  'S'.     
       77  WRK-MOV-REG                 PIC X(01)        VALUE  'N'.     
       77  WRK-TEM-05-20-40            PIC X(01)        VALUE  'N'.     
                                                                        
       77  WRK-SORT-RETURN             PIC 9(04)        VALUE  ZEROS.   
       77  WRK-FS-TABELAL              PIC X(02)        VALUE  SPACES.  
       77  WRK-FS-CADSPC               PIC X(02)        VALUE  SPACES.  
       77  WRK-FS-ARQDATA              PIC X(02)        VALUE  SPACES.  
       77  WRK-FS-CADCARTA             PIC X(02)        VALUE  SPACES.  
       77  WRK-FS-BLQREST              PIC X(02)        VALUE  SPACES.  
       77  WRK-FS-RELATO               PIC X(02)        VALUE  SPACES.  
       77  WRK-FS-PARMCLLP             PIC X(02)        VALUE  SPACES.  
                                                                        
       77  WRK-OPERACAO                PIC X(13) VALUE  SPACES.         
       77  WRK-ABERTURA                PIC X(13) VALUE  'NA ABERTURA'.  
       77  WRK-LEITURA                 PIC X(13) VALUE  'NA LEITURA'.   
       77  WRK-GRAVACAO                PIC X(13) VALUE  'NA GRAVACAO'.  
       77  WRK-FECHAMENTO              PIC X(13) VALUE  'NO FECHAMENTO'.
                                                                        
       77  WRK-BLOQ-OPER           PIC 9(01)    VALUE ZEROS.            
       77  WRK-UF-ALFA             PIC X(02)    VALUE SPACES.           
       77  WRK-UF-NUM              PIC 9(02)    VALUE ZEROS.            
       77  WRK-PESQ                PIC 9(03)    VALUE ZEROS.            
       77  WRK-PESQ-DEVEDOR        PIC 9(03)    VALUE 063.              
                                                                        
       01  WRK-DIAS1                   PIC S9(03)         VALUE  ZEROS. 
       01  WRK-DIAS2                   PIC S9(03)         VALUE  ZEROS. 
       01  WRK-DIAS3                   PIC S9(03)         VALUE  ZEROS. 
       01  WRK-DIAS4                   PIC S9(03)         VALUE  ZEROS. 
                                                                        
       01  WRK-VL-CORTE1               PIC S9(15)         VALUE  ZEROS. 
       01  WRK-VL-CORTE2               PIC S9(15)         VALUE  ZEROS. 
       01  WRK-VL-CORTE3               PIC S9(15)         VALUE  ZEROS. 
       01  WRK-VL-CORTE4               PIC S9(15)         VALUE  ZEROS. 
                                                                        
                                                                        
       01  WRK-MENSAGEM-AUX            PIC X(21) VALUE SPACES.

       01  WRK-SORT.
           02  WRK-CGC.
               03  WRK-NUMCGC                  PIC 9(09)       COMP-3.
               03  WRK-FILCGC                  PIC 9(05)       COMP-3.
               03  WRK-DIGCGC                  PIC 9(03)       COMP-3.
           02  WRK-NATURE                      PIC X(02).
           02  WRK-VCMTO                       PIC 9(09)       COMP-3.
           02  WRK-EMPRESA-CATU                PIC 9(05)       COMP-3.
           02  WRK-AGENCIA-CATU                PIC 9(05)       COMP-3.
           02  WRK-NUM-CC-CATU                 PIC 9(07)       COMP-3.
           02  WRK-CARTEIRA-CATU               PIC X(03).
           02  WRK-CONTRATO-CATU               PIC 9(07)       COMP-3.
           02  WRK-RESGATE                     PIC 9(11)V99    COMP-3.
           02  WRK-VALOR-VENCIDOS-CATU         PIC 9(11)V99    COMP-3.
           02  WRK-VALOR-VINCENDOS-CATU        PIC 9(11)V99    COMP-3.
           02  WRK-NOMERESP-CATU               PIC X(40).
           02  WRK-CGCCPFRESP-CATU.
               03  WRK-NUMCGC-CATU             PIC 9(09)       COMP-3.
               03  WRK-FILCGC-CATU             PIC 9(05)       COMP-3.
               03  WRK-CTRCGC-CATU             PIC 9(03)       COMP-3.
           02  WRK-DATA-NASCIMEN-CATU          PIC 9(09)       COMP-3.
           02  WRK-NATURALIDADE-CATU           PIC X(30).
           02  WRK-UF-ORIGEM-CATU              PIC X(02).
           02  WRK-NOME-CONJUGE-CATU           PIC X(40).
           02  WRK-DEV-ENDER-CATU              PIC X(40).
           02  WRK-DEV-NRO-CATU                PIC X(07).
           02  WRK-DEV-COMPL-CATU              PIC X(20).
           02  WRK-DEV-BAIRRO-CATU             PIC X(20).
           02  WRK-DEV-CIDADE-CATU             PIC X(30).
           02  WRK-DEV-UF-CATU                 PIC X(02).
           02  WRK-DEV-CCEP-CATU               PIC 9(05).
           02  WRK-DEV-CCEP-COMPL-CATU         PIC 9(03).
           02  WRK-NOMEAVAL1-CATU              PIC X(40).
           02  WRK-CGC-CPF-AVAL1-CATU.
               03 WRK-CGCCPFAVAL1-CATU         PIC 9(09)       COMP-3.
               03 WRK-FILCGC-AVAL1-CATU        PIC 9(05)       COMP-3.
               03 WRK-CTRCGC-AVAL1-CATU        PIC 9(03)       COMP-3.
           02  WRK-AVA-ENDER-CATU              PIC X(40).
           02  WRK-AVA-NRO-CATU                PIC X(07).
           02  WRK-AVA-COMPL-CATU              PIC X(20).
           02  WRK-AVA-BAIRRO-CATU             PIC X(20).
           02  WRK-AVA-CIDADE-CATU             PIC X(30).
           02  WRK-AVA-UF-CATU                 PIC X(02).
           02  WRK-AVA-CCEP-CATU               PIC 9(05).
           02  WRK-AVA-CCEP-COMPL-CATU         PIC 9(03).
           02  WRK-NOMEAVAL2-CATU              PIC X(40).
           02  WRK-CGC-CPF-AVAL2-CATU.
               03 WRK-CGCCPFAVAL2-CATU         PIC 9(09)       COMP-3.
               03 WRK-FILCGC-AVAL2-CATU        PIC 9(05)       COMP-3.
               03 WRK-CTRCGC-AVAL2-CATU        PIC 9(03)       COMP-3.
           02  WRK-CHAVE-LP-CL-CATU.
               03  WRK-NATUREZA-OPER-CATU      PIC X(02).
               03  FILLER                      PIC 9(10).
           02  WRK-NUMERO-CL-CATU              PIC 9(15).
           02  WRK-DATA-OCORR-CATU             PIC 9(09)       COMP-3.
           02  WRK-COD-NATUREZA-CATU           PIC 9(03).
           02  WRK-DTECL-CATU                  PIC 9(09)       COMP-3.
           02  WRK-DTBCL-CATU                  PIC 9(09)       COMP-3.
           02  WRK-DTELP-CATU                  PIC 9(09)       COMP-3.
           02  WRK-DTBLP-CATU                  PIC 9(09)       COMP-3.
           02  WRK-ID-LPCL-CATU                PIC X(02).
           02  WRK-TIPBX-CATU                  PIC X(01).
           02  WRK-TIPO-CATU                   PIC 9(01).
           02  WRK-TPO-CLIENTE                 PIC X(01).
           02  WRK-ORIG-ENDER                  PIC X(01).
           02  WRK-TPO-LOGRA                   PIC X(01).
           02  WRK-SIT-LOCALIZ                 PIC X(01).
           02  FILLER                          PIC X(01).
           02  WRK-LT.
AQUI          03 WRK-TAXA-CONTRATO            PIC 9(02)V9(06) COMP-3.
              03 WRK-VR-REMUNERATORIO          PIC S9(13)V99   COMP-3.
              03 WRK-VALOR-MORATORIO           PIC S9(13)V99   COMP-3.
              03 WRK-VALOR-MULTA               PIC S9(13)V99   COMP-3.
              03 WRK-DESP-JUD-CUSTAS           PIC S9(11)V99   COMP-3.
              03 WRK-HONORARIOS                PIC S9(11)V99   COMP-3.
              03 WRK-VL-TOTAL-DIVIDA           PIC S9(15)V99   COMP-3.
              03 WRK-VL-TAXA-TARIFA            PIC S9(15)V99   COMP-3.
              03 FILLER                        PIC  X(39).

                                                                        
       01  WRK-NATURE.                                                  
           02  FILLER                  PIC X(01) VALUE  '0'.            
           02  WRK-NATURE-NUMERIC      PIC X(02).                       
                                                                        
       01  WRK-DATA-AUX                PIC 9(08).                       
       01  FILLER REDEFINES WRK-DATA-AUX.                               
           02  WRK-DT-AAAA             PIC 9(04).                       
           02  WRK-DT-MM               PIC 9(02).                       
           02  WRK-DT-DD               PIC 9(02).                       
                                                                        
       01  SORT-CGC-ATU.                                                
           02  SORT-NUMCGC-ATU         PIC 9(09) COMP-3.                
           02  SORT-FILCGC-ATU         PIC 9(05) COMP-3.                
           02  SORT-DIGCGC-ATU         PIC 9(02).                       
                                                                        
       01  SORT-CGC-ANT.                                                
           02  SORT-NUMCGC-ANT         PIC 9(09) COMP-3.                
           02  SORT-FILCGC-ANT         PIC 9(05) COMP-3.                
           02  SORT-DIGCGC-ANT         PIC 9(02).                       
                                                                        
       01  WS-CHAVE-480-ATU.                                            
           02  WS-480-NUMCGC-ATU       PIC 9(09) COMP-3    VALUE ZEROS. 
           02  WS-480-FILCGC-ATU       PIC 9(05) COMP-3    VALUE ZEROS. 
           02  WS-480-CTRCGC-ATU       PIC 9(02)           VALUE ZEROS. 
           02  WS-480-AGEN-ATU         PIC 9(05) COMP-3    VALUE ZEROS. 
           02  WS-480-CTA-ATU          PIC 9(07) COMP-3    VALUE ZEROS. 
           02  WS-480-NAT-ATU          PIC 9(03)           VALUE ZEROS. 
                                                                        
       01  WS-CHAVE-480-ANT.                                            
           02  WS-480-NUMCGC-ANT       PIC 9(09) COMP-3    VALUE ZEROS. 
           02  WS-480-FILCGC-ANT       PIC 9(05) COMP-3    VALUE ZEROS. 
           02  WS-480-CTRCGC-ANT       PIC 9(02)           VALUE ZEROS. 
           02  WS-480-AGEN-ANT         PIC 9(05) COMP-3    VALUE ZEROS. 
           02  WS-480-CTA-ANT          PIC 9(07) COMP-3    VALUE ZEROS. 
           02  WS-480-NAT-ANT          PIC 9(03)           VALUE ZEROS. 
                                                                        
      *---------------------------------------------------------------* 
      *                 CAMPOS UTILIZADOS NA POOL7600                 * 
      *---------------------------------------------------------------* 
                                                                        
       01  WRK-AREA-POOL7600.                                           
           02  WRK-DT-JULIANA          PIC 9(05) COMP-3.                
           02  WRK-DT-AAMMDD           PIC 9(07) COMP-3.                
           02  WRK-DT-AAAAMMDD         PIC 9(09) COMP-3.                
           02  WRK-TI-HHMMSS           PIC 9(07) COMP-3.                
           02  WRK-TI-HHMMSSMMMMMM     PIC 9(13) COMP-3.                
           02  WRK-TIMESTAMP           PIC X(20).                       
                                                                        
      *---------------------------------------------------------------* 
      *                 CAMPOS UTILIZADOS NA POOL1285                 * 
      *---------------------------------------------------------------* 
                                                                        
       01  WRK-AREA-POOL1285.                                           
           02  WRK-DT-ENTRADA          PIC  9(08) COMP-3 VALUE ZEROS.   
           02  WRK-NUM-DIAS            PIC S9(05) COMP-3 VALUE ZEROS.   
           02  WRK-DATA-SAIDA          PIC  9(08) COMP-3 VALUE ZEROS.   
           02  WRK-MENSAGEM            PIC  X(50)        VALUE SPACES.  
                                                                        
       01  WRK-RETURN-CODE             PIC S9(05)        VALUE ZEROS.   
                                                                        
                                                                        
      *---------------------------------------------------------------* 
      *             DEFINICAO DAS DATAS DE VENCIMENTO                 * 
      *---------------------------------------------------------------* 
                                                                        
       01  WRK-DATA-1A-CARTA           PIC 9(09).                       
       01  WRK-DATA-08                 PIC 9(09).                       
       01  WRK-DATA-2A-CARTA           PIC 9(09).                       
       01  WRK-DATA-21                 PIC 9(09).                       
       01  WRK-DATA-3A-CARTA           PIC 9(09).                       
       01  WRK-DATA-41                 PIC 9(09).                       
                                                                        
       01  DT-MOV-AAAAMMDD             PIC 9(08).                       
       01  FILLER REDEFINES DT-MOV-AAAAMMDD.                            
        03 DT-MOV-SS                   PIC 9(02).                       
        03 DT-MOV-AA                   PIC 9(02).                       
       01  FILLER REDEFINES DT-MOV-AAAAMMDD.                            
        03 FILLER                      PIC 9(02).                       
        03 DT-MOV-AAMMDD               PIC 9(06).                       
                                                                        
      *---------------------------------------------------------------* 
      * INC P/ COVERTER UF ALFA P/ NUMERICA P/ ENVIO A POOL0480       * 
      *---------------------------------------------------------------* 
                                                                        
           COPY 'I#LPCLUF'.                                             
                                                                        
      *---------------------------------------------------------------* 
      *    AREA PARA POOL0480                                         * 
      *---------------------------------------------------------------* 
                                                                        
       77  WRK-480-CODIGO1             PIC 9(01) VALUE 1.               
       77  WRK-480-CODIGO2             PIC 9(01) VALUE 2.               
       77  WRK-480-CODIGO3             PIC 9(01) VALUE 3.               
       77  WRK-480-TAM-CHAVE           PIC 9(02) VALUE 37.              
       77  WRK-480-NOME-TABELA         PIC X(08) VALUE 'TABCONTR'.      
       77  WRK-480-TAM-OCORRENC        PIC 9(03) VALUE 37.              
                                                                        
       77  WRK-MODULO                  PIC X(08) VALUE 'POOL0482'.      
                                                                        
       01  WRK-480-REGISTRO.                                            
           03  WRK-480-CHAVE.                                           
               05 WRK-480-CCGC-CPF         PIC 9(09).                   
               05 WRK-480-CFLIAL-CGC       PIC 9(05).                   
               05 WRK-480-CCTRL-CGC        PIC 9(02).                   
               05 WRK-480-AGENCIA          PIC 9(05).                   
               05 WRK-480-CONTA            PIC 9(07).                   
               05 WRK-480-NATUREZA         PIC 9(03).                   
BRQ141*........05.WRK-480-CCART............PIC.9(03).                   
BRQ141         05 WRK-480-CCART            PIC X(03).                   
               05 WRK-480-BLOQ-OPER        PIC 9(01).                   
               05 WRK-480-UF               PIC 9(02).                   
                                                                        
       01  WRK-480-RETORNO.                                             
           03  WRK-480-RETORNO-CHAVE.                                   
               05 WRK-480-RET-CCGC-CPF     PIC 9(09).                   
               05 WRK-480-RET-CFLIAL-CGC   PIC 9(05).                   
               05 WRK-480-RET-CCTRL-CGC    PIC 9(02).                   
               05 WRK-480-RET-AGENCIA      PIC 9(05).                   
               05 WRK-480-RET-CONTA        PIC 9(07).                   
               05 WRK-480-RET-NATUREZA     PIC 9(03).                   
BRQ141*........05.WRK-480-RET-CCART........PIC.9(03).                   
BRQ141         05 WRK-480-RET-CCART        PIC X(03).                   
               05 WRK-480-RET-BLOQ-OPER    PIC 9(01).                   
               05 WRK-480-RET-UF           PIC 9(02).                   
                                                                        
      *---------------------------------------------------------------* 
      *                            CABECALHOS                         * 
      *---------------------------------------------------------------* 
                                                                        
       01  CB-CARRO                    PIC X(132) VALUE SPACES.         
                                                                        
       01  CABEC1.                                                      
           03  CB1-CARRO               PIC X(01) VALUE '1'.             
           03  FILLER                  PIC X(36) VALUE 'CLLPG656'.      
           03  FILLER                  PIC X(41) VALUE                  
               'BANCO  BRADESCO S.A.'.                                  
                                                                        
       01  CABEC2.                                                      
           03  CB2-CARRO               PIC X(01) VALUE '0'.             
           03  FILLER.                                                  
               05  CB2-DATA-DD         PIC 9(02)/.                      
               05  CB2-DATA-MM         PIC 9(02)/.                      
               05  CB2-DATA-AAAA       PIC 9(04).                       
           03  FILLER                  PIC X(16) VALUE SPACES.          
           03  FILLER                  PIC X(50) VALUE                  
               'SELECAO PARA A EMISSAO DE CARTAS COBRANCA'.             
                                                                        
       01  CABEC3.                                                      
           03  CB3-CARRO               PIC X(01) VALUE '-'.             
           03  FILLER                  PIC X(33) VALUE SPACES.          
           03  FILLER                  PIC X(80) VALUE                  
               'T O T A L I Z A D O R E S'.                             
                                                                        
      *---------------------------------------------------------------* 
      *                         LINHAS DETALHE                        * 
      *---------------------------------------------------------------* 
                                                                        
       01  LINDET1.                                                     
           03  FILLER                  PIC X(01) VALUE '0'.             
           03  FILLER                  PIC X(25) VALUE SPACES.          
           03  FILLER                  PIC X(33) VALUE                  
               'TOTAL DE REGISTROS LIDOS      :'.                       
           03  LD1-TOT-LIDOS           PIC ZZZ.ZZZ.ZZ9.                 
                                                                        
       01  LINDET2.                                                     
           03  FILLER                  PIC X(01) VALUE '0'.             
           03  FILLER                  PIC X(25) VALUE SPACES.          
           03  FILLER                  PIC X(33) VALUE                  
               'TOTAL DE REGISTROS EXCLUIDOS  :'.                       
           03  LD2-TOT-EXCL            PIC ZZZ.ZZZ.ZZ9.                 
                                                                        
       01  LINDET3.                                                     
           03  FILLER                  PIC X(01) VALUE '0'.             
           03  FILLER                  PIC X(25) VALUE SPACES.          
           03  FILLER                  PIC X(33) VALUE                  
               'TOTAL DE REGISTROS GRAVADOS   :'.                       
           03  LD3-TOT-GRAV            PIC ZZZ.ZZZ.ZZ9.                 
                                                                        
       01  LINDET4.                                                     
           03  FILLER                  PIC X(01) VALUE '0'.             
           03  FILLER                  PIC X(25) VALUE SPACES.          
           03  FILLER                  PIC X(33) VALUE                  
               'TOTAL DE REG.DESPR.BLOQ.RESTR.:'.                       
           03  LD4-TOT-DESP            PIC ZZZ.ZZZ.ZZ9.                 
                                                                        
                                                                        
      *---------------------------------------------------------------* 
      *   PARAMETRO A SER PASSADO P/ A POOL7100 (MODULO TRATA/ ERRO)  * 
      *                    (-INC POL7100C)                            * 
      *---------------------------------------------------------------* 
       01  ERRO-AREA.                                                   
           05  ERR-TIPO-ACESSO        PIC X(03).                        
           05  ERR-PGM                PIC X(08).                        
           05  ERR-DADOS-IMS-DB2.                                       
               07  ERR-DBD-TAB        PIC X(18).                        
               07  ERR-FUN-COMANDO    PIC X(10).                        
               07  ERR-STA-CODE       PIC X(04).                        
               07  ERR-SQL-CODE  REDEFINES                              
                   ERR-STA-CODE       PIC S9(09) COMP-4.                
               07  ERR-LOCAL          PIC X(04).                        
               07  ERR-SEGM           PIC X(08).                        
               07  FILLER             PIC X(31).                        
                                                                        
           05  ERR-TEXTO  REDEFINES  ERR-DADOS-IMS-DB2                  
                                      PIC X(75).                        
      *---------------------------------------------------------------* 
      *  OS CPOS ABAIXO DEVEM SER INICIALIZADOS SO' QDO O APLICATIVO  * 
      *  USAR O SISTEMA DE SENHAS (POOL1000).                         * 
      *---------------------------------------------------------------* 
           05  ERR-DADOS-SENHAS.                                        
               07  ERR-COD-USER       PIC X(07).                        
               07  ERR-COD-DEPTO      PIC X(06).                        
               07  ERR-MODULO         PIC X(08)  VALUE SPACES.          
       01  FILLER                      PIC X(32)        VALUE           
           '*  FIM DA WORKING CLLPG656 *'.                              
           EJECT                                                        
      *===============================================================* 
       PROCEDURE DIVISION.                                              
      *===============================================================* 
                                                                        
      *---------------------------------------------------------------* 
       00000-INICIAR SECTION.                                           
      *---------------------------------------------------------------* 
                                                                        
           CALL 'POOL1050'.                                             
                                                                        
           OPEN INPUT   TABELAL                                         
                        PARMCLLP                                        
                        CADSPC                                          
                        ARQDATA                                         
                OUTPUT  CADCARTA                                        
                        BLQREST                                         
                        RELATO.                                         
                                                                        
           MOVE    WRK-ABERTURA            TO   WRK-OPERACAO.           
           PERFORM 100000-TESTAR-FILE-STATUS.                           
                                                                        
BRQ358     PERFORM 0001-ZERAR-11-OCORRENCIAS                            
BRQ358                  VARYING WRK-IND FROM 1 BY 1 UNTIL               
BRQ358                          WRK-IND GREATER 11.                     
                                                                        
BRQ358     MOVE  ZEROS TO WRK-IND.                                      
                                                                        
           PERFORM CARREGA-POOL0480    THRU  FIM-CARREGA-POOL0480.      
                                                                        
           PERFORM 666666-LER-PARMCLLP.                                 
                                                                        
           PERFORM   777777-TRATA-PARMCLLP                              
               UNTIL  WRK-FS-PARMCLLP   EQUAL   '10'                    
                                                                        
           CALL    'POOL7600'        USING  WRK-AREA-POOL7600           
                                                                        
           DISPLAY 'CLLPG656 - DATA DE HOJE = ' WRK-DT-AAAAMMDD.        
                                                                        
           PERFORM 150000-CALC-1A-2A-3A-CARTA.                          
                                                                        
                                                                        
           SORT SORTWK01  ASCENDING   KEY  SOR-NUMCGC                   
                                           SOR-FILCGC                   
                                           SOR-DIGCGC                   
                                           SOR-VCMTO                    
                INPUT  PROCEDURE  300000-ENTRADA-DO-SORT                
                OUTPUT PROCEDURE  400000-SAIDA-DO-SORT.                 
                                                                        
           IF SORT-RETURN   NOT      EQUAL   ZEROS                      
              MOVE    SORT-RETURN    TO      WRK-SORT-RETURN            
              DISPLAY '************** CLLPG656 *************'           
              DISPLAY '*   ERRO NO PROCESSAMENTO DO SORT   *'           
              DISPLAY '*         SORT-RETURN  = ' WRK-SORT-RETURN       
                                                   '       *'           
              DISPLAY '************** CLLPG656 *************'           
              CALL    'ILBOABN0'     USING   WRK-ABEND.                 
                                                                        
                                                                        
       000000-FINALIZAR.                                                
      *---------------*                                                 
                                                                        
           CLOSE  TABELAL                                               
                  PARMCLLP                                              
                  CADSPC                                                
                  ARQDATA                                               
                  CADCARTA                                              
                  BLQREST                                               
                  RELATO.                                               
                                                                        
           MOVE    WRK-FECHAMENTO    TO     WRK-OPERACAO.               
           PERFORM 100000-TESTAR-FILE-STATUS.                           
                                                                        
           MOVE    ZEROS             TO     RETURN-CODE.                
                                                                        
           STOP RUN.                                                    
                                                                        
      *---------------------------------------------------------------* 
       000000-99-FIM. EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
BRQ358 0001-ZERAR-11-OCORRENCIAS  SECTION.                              
      *---------------------------------------------------------------* 
                                                                        
           MOVE  SPACES           TO  SELE-POSSIBILITI (WRK-IND)        
                                      SELE-NATUREZA (WRK-IND)           
                                      SELE-CARTEIRA (WRK-IND)           
                                                                        
           MOVE  ZEROS            TO  SELE-DAT-VENCTO (WRK-IND)         
                                      SELE-CONTRATO (WRK-IND)           
                                      SELE-IOF-NORMAL (WRK-IND)         
                                      SELE-RESGATE (WRK-IND)            
                                      SELE-VR-REMUNERATORIO (WRK-IND)   
                                      SELE-VALOR-MORATORIO  (WRK-IND)   
                                      SELE-VALOR-MULTA  (WRK-IND)       
                                      SELE-DESP-JUD-CUSTAS  (WRK-IND)   
                                      SELE-HONORARIOS  (WRK-IND)        
                                      SELE-VL-TAXA-TARIFA  (WRK-IND)    
                                      SELE-VL-TOTAL-DIVIDA  (WRK-IND).  
                                                                        
      *---------------------------------------------------------------* 
       0001-99-FIM. EXIT.                                               
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1001-TESTAR-FS-BLQREST  SECTION.                                 
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-FS-BLQREST  NOT EQUAL '00'                            
              DISPLAY '************** CLLPG656 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              BLQREST              *'           
              DISPLAY '*         FILE STATUS =  ' WRK-FS-BLQREST        
                                                 '         *'           
              DISPLAY '************** CLLPG656 *************'           
              CALL 'ILBOABN0'     USING WRK-ABEND                       
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       1001-99-FIM. EXIT.                                               
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1002-GRAVA-BLQREST  SECTION.                                     
      *---------------------------------------------------------------* 
                                                                        
           MOVE SPC-EMPRESA-CATU   TO FD-EMPRESA.                       
           MOVE SPC-AGENCIA-CATU   TO FD-AGENCIA.                       
           MOVE SPC-NUM-CC-CATU    TO FD-CONTA.                         
           MOVE SPC-CARTEIRA-CATU  TO FD-CARTEIRA.                      
           MOVE SPC-CONTRATO-CATU  TO FD-CONTRATO.                      
           MOVE 2                  TO FD-IND-BLOQ.                      
           MOVE 'D'                TO FD-INDICADOR.                     
           MOVE SPC-NUMCGC-CATU    TO FD-CPF.                           
           MOVE SPC-FILCGC-CATU    TO FD-FILIAL.                        
           MOVE SPC-CTRCGC-CATU    TO FD-CTRL.                          
                                                                        
           WRITE  FD-BLQREST.                                           
                                                                        
           MOVE    WRK-GRAVACAO  TO  WRK-OPERACAO                       
           PERFORM 1001-TESTAR-FS-BLQREST.                              
                                                                        
           ADD      1      TO   ACU-GRAV-BLQREST.                       
                                                                        
      *---------------------------------------------------------------* 
       1002-99-FIM. EXIT.                                               
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       100000-TESTAR-FILE-STATUS SECTION.                               
      *---------------------------------------------------------------* 
                                                                        
           PERFORM 110000-TESTAR-FS-CADSPC.                             
                                                                        
           PERFORM 120000-TESTAR-FS-CADCARTA.                           
                                                                        
           PERFORM 1001-TESTAR-FS-BLQREST.                              
                                                                        
           PERFORM 130000-TESTAR-FS-RELATO.                             
                                                                        
           PERFORM 140000-TESTAR-FS-ARQDATA.                            
                                                                        
           PERFORM 170000-TESTAR-FS-TABELAL.                            
                                                                        
           PERFORM 888888-TESTAR-FS-PARMCLLP.                           
                                                                        
      *---------------------------------------------------------------* 
       100000-99-FIM. EXIT.                                             
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       110000-TESTAR-FS-CADSPC SECTION.                                 
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-FS-CADSPC NOT EQUAL '00'                              
              DISPLAY '************** CLLPG656 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              CADSPC               *'           
              DISPLAY '*         FILE STATUS =  ' WRK-FS-CADSPC         
                                                 '         *'           
              DISPLAY '************** CLLPG656 *************'           
              CALL 'ILBOABN0'     USING WRK-ABEND.                      
                                                                        
      *---------------------------------------------------------------* 
       110000-99-FIM. EXIT.                                             
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       120000-TESTAR-FS-CADCARTA SECTION.                               
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-FS-CADCARTA NOT EQUAL '00'                            
              DISPLAY '************** CLLPG656 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              CADCARTA             *'           
              DISPLAY '*         FILE STATUS =  ' WRK-FS-CADCARTA       
                                                 '         *'           
              DISPLAY '************** CLLPG656 *************'           
              CALL 'ILBOABN0'     USING WRK-ABEND.                      
                                                                        
      *---------------------------------------------------------------* 
       120000-99-FIM. EXIT.                                             
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       130000-TESTAR-FS-RELATO SECTION.                                 
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-FS-RELATO NOT EQUAL '00'                              
              DISPLAY '************** CLLPG656 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              RELATO               *'           
              DISPLAY '*         FILE STATUS =  ' WRK-FS-RELATO         
                                                 '         *'           
              DISPLAY '************** CLLPG656 *************'           
              CALL 'ILBOABN0'     USING WRK-ABEND.                      
                                                                        
      *---------------------------------------------------------------* 
       130000-99-FIM. EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       140000-TESTAR-FS-ARQDATA SECTION.                                
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-FS-ARQDATA NOT EQUAL '00'                             
              DISPLAY '************** CLLPG656 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              ARQDATA              *'           
              DISPLAY '*         FILE STATUS =  ' WRK-FS-ARQDATA        
                                                 '         *'           
              DISPLAY '************** CLLPG656 *************'           
              CALL 'ILBOABN0'     USING WRK-ABEND.                      
                                                                        
      *---------------------------------------------------------------* 
       140000-99-FIM. EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       170000-TESTAR-FS-TABELAL SECTION.                                
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-FS-TABELAL NOT EQUAL '00'                             
              DISPLAY '************** CLLPG656 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              TABELAL              *'           
              DISPLAY '*         FILE STATUS =  ' WRK-FS-TABELAL        
                                                 '         *'           
              DISPLAY '************** CLLPG656 *************'           
              CALL 'ILBOABN0'     USING WRK-ABEND.                      
                                                                        
      *---------------------------------------------------------------* 
       170000-99-FIM. EXIT.                                             
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       150000-CALC-1A-2A-3A-CARTA  SECTION.                             
      *---------------------------------------------------------------* 
                                                                        
           PERFORM 160000-LER-ARQDATA.                                  
                                                                        
           MOVE    DTMOVINV          TO     DT-MOV-AAAAMMDD.            
                                                                        
           DISPLAY 'DATA DO MOVIMENTO ' DT-MOV-AAAAMMDD.                
           MOVE    DT-MOV-AAAAMMDD   TO     WRK-DT-ENTRADA.             
                                                                        
           COMPUTE WRK-NUM-DIAS      =       WRK-DIAS1      - 1         
           COMPUTE WRK-NUM-DIAS      =     ( WRK-NUM-DIAS * -1)         
      ***  MOVE    -04               TO      WRK-NUM-DIAS               
           PERFORM 200000-POOL1285                                      
           DISPLAY 'DATA DE -05 = ' WRK-DATA-SAIDA                      
           MOVE    WRK-DATA-SAIDA    TO      WRK-DATA-1A-CARTA          
                                                                        
                                                                        
           MOVE    WRK-DIAS1         TO      WRK-NUM-DIAS               
           COMPUTE WRK-NUM-DIAS      =     ( WRK-NUM-DIAS * -1)         
      ***  MOVE    -05               TO      WRK-NUM-DIAS               
           PERFORM 200000-POOL1285                                      
           DISPLAY 'DATA DE -06= '  WRK-DATA-SAIDA                      
           MOVE    WRK-DATA-SAIDA    TO      WRK-DATA-08                
                                                                        
                                                                        
           COMPUTE WRK-NUM-DIAS      =      WRK-DIAS2    - 1            
           COMPUTE WRK-NUM-DIAS      =    ( WRK-NUM-DIAS * -1)          
      ***  MOVE    -19               TO     WRK-NUM-DIAS                
           PERFORM 200000-POOL1285                                      
           DISPLAY 'DATA DE -20 = ' WRK-DATA-SAIDA                      
           MOVE   WRK-DATA-SAIDA     TO     WRK-DATA-2A-CARTA           
                                                                        
                                                                        
           MOVE    WRK-DIAS2         TO      WRK-NUM-DIAS               
           COMPUTE WRK-NUM-DIAS      =     ( WRK-NUM-DIAS * -1)         
      ***  MOVE    -20               TO      WRK-NUM-DIAS               
           PERFORM 200000-POOL1285                                      
           DISPLAY 'DATA DE -21 = ' WRK-DATA-SAIDA                      
           MOVE   WRK-DATA-SAIDA     TO     WRK-DATA-21                 
                                                                        
                                                                        
           COMPUTE WRK-NUM-DIAS      =       WRK-DIAS3    -  1          
           COMPUTE WRK-NUM-DIAS      =     ( WRK-NUM-DIAS * -1)         
      ***  MOVE   -39                TO      WRK-NUM-DIAS               
           PERFORM 200000-POOL1285                                      
           DISPLAY 'DATA DE -40 = ' WRK-DATA-SAIDA                      
           MOVE   WRK-DATA-SAIDA     TO     WRK-DATA-3A-CARTA.          
                                                                        
                                                                        
           MOVE    WRK-DIAS3         TO     WRK-NUM-DIAS                
           COMPUTE WRK-NUM-DIAS      =    ( WRK-NUM-DIAS * -1)          
      ***  MOVE   -40                TO     WRK-NUM-DIAS                
           PERFORM 200000-POOL1285                                      
           DISPLAY 'DATA DE -41 = ' WRK-DATA-SAIDA                      
           MOVE   WRK-DATA-SAIDA     TO     WRK-DATA-41.                
                                                                        
      *---------------------------------------------------------------* 
       150000-99-FIM. EXIT.                                             
      *---------------------------------------------------------------* 
       160000-LER-ARQDATA SECTION.                                      
      *---------------------------------------------------------------* 
                                                                        
           READ ARQDATA.                                                
                                                                        
           MOVE WRK-LEITURA                TO   WRK-OPERACAO.           
           PERFORM 140000-TESTAR-FS-ARQDATA.                            
      *---------------------------------------------------------------* 
       160000-99-FIM. EXIT.                                             
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       200000-POOL1285 SECTION.                                         
      *---------------------------------------------------------------* 
                                                                        
            CALL  'POOL1285' USING WRK-AREA-POOL1285                    
                                                                        
            IF  RETURN-CODE  NOT  EQUAL  ZEROS                          
                MOVE  RETURN-CODE  TO   WRK-RETURN-CODE                 
                MOVE  WRK-MENSAGEM TO   WRK-MENSAGEM-AUX                
                DISPLAY                                                 
                      '******************* CLLPG656 *******************'
                DISPLAY                                                 
                      '*   ERRO  UTILIZANDO  A  POOL1285              *'
                DISPLAY                                                 
                      '*   DATA-ENTRADA     =   ' WRK-DT-ENTRADA '      
      -               '        *'                                       
                DISPLAY                                                 
                      '*   RETURN-CODE      =   ' WRK-RETURN-CODE '     
      -               '           *'                                    
                DISPLAY                                                 
                      '*   MENSAGEM DO ERRO =   ' WRK-MENSAGEM-AUX ' *' 
                DISPLAY                                                 
                      '******************* CLLPG656 *******************'
                GO   TO   000000-FINALIZAR.                             
                                                                        
      *---------------------------------------------------------------* 
       200000-99-FIM. EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       300000-ENTRADA-DO-SORT SECTION.                                  
      *---------------------------------------------------------------* 
                                                                        
           PERFORM 310000-LER-CADSPC                                    
                                                                        
           IF  WRK-FS-CADSPC EQUAL '10'                                 
               DISPLAY '*****************************'                  
               DISPLAY '**      SR. OPERADOR       **'                  
               DISPLAY '**     ARQUIVO CADSPC      **'                  
               DISPLAY '**         VAZIO           **'                  
               DISPLAY '** PROCESSAMENTO ENCERRADO **'                  
               DISPLAY '*****************************'                  
               MOVE 'S'  TO WRK-FIM-CADSPC.                             
                                                                        
                                                                        
           PERFORM 320000-MOVIMENTA-REGISTRO UNTIL                      
                                            WRK-FIM-CADSPC  EQUAL  'S'. 
                                                                        
      *---------------------------------------------------------------* 
       300000-99-FIM. EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       310000-LER-CADSPC  SECTION.                                      
      *---------------------------------------------------------------* 
                                                                        
       310000-10-LE.                                                    
                                                                        
           READ CADSPC.                                                 
                                                                        
           IF  WRK-FS-CADSPC EQUAL  '10'                                
               MOVE  'S'     TO     WRK-FIM-CADSPC                      
               GO            TO     310000-99-FIM.                      
                                                                        
           MOVE WRK-LEITURA                TO   WRK-OPERACAO.           
           PERFORM 110000-TESTAR-FS-CADSPC.                             
                                                                        
           ADD     1                       TO   ACU-LIDOS-CADSPC.       
                                                                        
           IF SPC-ID-LPCL-CATU EQUAL 'MO'                               
              MOVE 1 TO WRK-BLOQ-OPER                                   
           ELSE                                                         
              IF SPC-ID-LPCL-CATU EQUAL 'CL' OR                         
                 SPC-ID-LPCL-CATU EQUAL 'LP'                            
                 MOVE 2 TO WRK-BLOQ-OPER                                
              ELSE                                                      
                 MOVE 0 TO WRK-BLOQ-OPER.                               
                                                                        
      * ***                                                             
      * *** CREDITO IMOBILIARIO  DEVERA FAZER O FILTRO PELA             
      * *** UF DO END. DO  IMOVEL E NAO DO ENDERECO CLIENTE             
      * *** ESTA SELECAO  ESTA  SENDO  FEITA  NO  INICIO DA             
      * *** NET CLLP1400                                                
      * ***                                                             
           IF  SPC-CARTEIRA-CATU  EQUAL  '502' OR '532' OR '561'        
               OR  '572'  OR  '590'                                     
               MOVE  ZEROS              TO    WRK-UF-ALFA               
           ELSE                                                         
               MOVE  SPC-DEV-UF-CATU    TO    WRK-UF-ALFA.              
      * ***                                                             
           PERFORM CONVERTE-UF.                                         
                                                                        
           PERFORM 315000-PESQUISAR-DEVEDOR                             
                   VARYING  WRK-PESQ         FROM     1  BY  1          
                   UNTIL    WRK-PESQ         GREATER  WRK-PESQ-DEVEDOR. 
                                                                        
           IF   RETURN-CODE           EQUAL   ZEROS                     
                PERFORM 1002-GRAVA-BLQREST                              
                ADD   1               TO      ACU-DESPREZADO            
                GO    TO              310000-10-LE.                     
                                                                        
      *---------------------------------------------------------------* 
       310000-99-FIM. EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       315000-PESQUISAR-DEVEDOR  SECTION.                               
      *---------------------------------------------------------------* 
                                                                        
           MOVE  ZEROS                      TO  WRK-480-RETORNO         
                                                                        
           EVALUATE  WRK-PESQ                                           
                                                                        
           WHEN 001                                                     
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 002                                                     
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
           WHEN 003                                                     
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 004                                                     
                MOVE  SPC-CARTEIRA-CATU     TO  WRK-480-RET-CCART       
           WHEN 005                                                     
                MOVE  SPC-CARTEIRA-CATU     TO  WRK-480-RET-CCART       
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 006                                                     
                MOVE  SPC-CARTEIRA-CATU     TO  WRK-480-RET-CCART       
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
           WHEN 007                                                     
                MOVE  SPC-CARTEIRA-CATU     TO  WRK-480-RET-CCART       
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 008                                                     
                MOVE  SPC-COD-NATUREZA-CATU TO  WRK-480-RET-NATUREZA    
           WHEN 009                                                     
                MOVE  SPC-COD-NATUREZA-CATU TO  WRK-480-RET-NATUREZA    
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 010                                                     
                MOVE  SPC-COD-NATUREZA-CATU TO  WRK-480-RET-NATUREZA    
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
           WHEN 011                                                     
                MOVE  SPC-COD-NATUREZA-CATU TO  WRK-480-RET-NATUREZA    
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 012                                                     
                MOVE  SPC-COD-NATUREZA-CATU TO  WRK-480-RET-NATUREZA    
                MOVE  SPC-CARTEIRA-CATU     TO  WRK-480-RET-CCART       
           WHEN 013                                                     
                MOVE  SPC-COD-NATUREZA-CATU TO  WRK-480-RET-NATUREZA    
                MOVE  SPC-CARTEIRA-CATU     TO  WRK-480-RET-CCART       
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 014                                                     
                MOVE  SPC-COD-NATUREZA-CATU TO  WRK-480-RET-NATUREZA    
                MOVE  SPC-CARTEIRA-CATU     TO  WRK-480-RET-CCART       
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
           WHEN 015                                                     
                MOVE  SPC-COD-NATUREZA-CATU TO  WRK-480-RET-NATUREZA    
                MOVE  SPC-CARTEIRA-CATU     TO  WRK-480-RET-CCART       
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 016                                                     
                MOVE  SPC-AGENCIA-CATU      TO  WRK-480-RET-AGENCIA     
                MOVE  SPC-NUM-CC-CATU       TO  WRK-480-RET-CONTA       
           WHEN 017                                                     
                MOVE  SPC-AGENCIA-CATU      TO  WRK-480-RET-AGENCIA     
                MOVE  SPC-NUM-CC-CATU       TO  WRK-480-RET-CONTA       
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 018                                                     
                MOVE  SPC-AGENCIA-CATU      TO  WRK-480-RET-AGENCIA     
                MOVE  SPC-NUM-CC-CATU       TO  WRK-480-RET-CONTA       
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
           WHEN 019                                                     
                MOVE  SPC-AGENCIA-CATU      TO  WRK-480-RET-AGENCIA     
                MOVE  SPC-NUM-CC-CATU       TO  WRK-480-RET-CONTA       
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 020                                                     
                MOVE  SPC-AGENCIA-CATU      TO  WRK-480-RET-AGENCIA     
                MOVE  SPC-NUM-CC-CATU       TO  WRK-480-RET-CONTA       
                MOVE  SPC-CARTEIRA-CATU     TO  WRK-480-RET-CCART       
           WHEN 021                                                     
                MOVE  SPC-AGENCIA-CATU      TO  WRK-480-RET-AGENCIA     
                MOVE  SPC-NUM-CC-CATU       TO  WRK-480-RET-CONTA       
                MOVE  SPC-CARTEIRA-CATU     TO  WRK-480-RET-CCART       
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 022                                                     
                MOVE  SPC-AGENCIA-CATU      TO  WRK-480-RET-AGENCIA     
                MOVE  SPC-NUM-CC-CATU       TO  WRK-480-RET-CONTA       
                MOVE  SPC-CARTEIRA-CATU     TO  WRK-480-RET-CCART       
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
           WHEN 023                                                     
                MOVE  SPC-AGENCIA-CATU      TO  WRK-480-RET-AGENCIA     
                MOVE  SPC-NUM-CC-CATU       TO  WRK-480-RET-CONTA       
                MOVE  SPC-CARTEIRA-CATU     TO  WRK-480-RET-CCART       
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 024                                                     
                MOVE  SPC-AGENCIA-CATU      TO  WRK-480-RET-AGENCIA     
                MOVE  SPC-NUM-CC-CATU       TO  WRK-480-RET-CONTA       
                MOVE  SPC-COD-NATUREZA-CATU TO  WRK-480-RET-NATUREZA    
           WHEN 025                                                     
                MOVE  SPC-AGENCIA-CATU      TO  WRK-480-RET-AGENCIA     
                MOVE  SPC-NUM-CC-CATU       TO  WRK-480-RET-CONTA       
                MOVE  SPC-COD-NATUREZA-CATU TO  WRK-480-RET-NATUREZA    
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 026                                                     
                MOVE  SPC-AGENCIA-CATU      TO  WRK-480-RET-AGENCIA     
                MOVE  SPC-NUM-CC-CATU       TO  WRK-480-RET-CONTA       
                MOVE  SPC-COD-NATUREZA-CATU TO  WRK-480-RET-NATUREZA    
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
           WHEN 027                                                     
                MOVE  SPC-AGENCIA-CATU      TO  WRK-480-RET-AGENCIA     
                MOVE  SPC-NUM-CC-CATU       TO  WRK-480-RET-CONTA       
                MOVE  SPC-COD-NATUREZA-CATU TO  WRK-480-RET-NATUREZA    
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 028                                                     
                MOVE  SPC-AGENCIA-CATU      TO  WRK-480-RET-AGENCIA     
                MOVE  SPC-NUM-CC-CATU       TO  WRK-480-RET-CONTA       
                MOVE  SPC-COD-NATUREZA-CATU TO  WRK-480-RET-NATUREZA    
                MOVE  SPC-CARTEIRA-CATU     TO  WRK-480-RET-CCART       
           WHEN 029                                                     
                MOVE  SPC-AGENCIA-CATU      TO  WRK-480-RET-AGENCIA     
                MOVE  SPC-NUM-CC-CATU       TO  WRK-480-RET-CONTA       
                MOVE  SPC-COD-NATUREZA-CATU TO  WRK-480-RET-NATUREZA    
                MOVE  SPC-CARTEIRA-CATU     TO  WRK-480-RET-CCART       
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 030                                                     
                MOVE  SPC-AGENCIA-CATU      TO  WRK-480-RET-AGENCIA     
                MOVE  SPC-NUM-CC-CATU       TO  WRK-480-RET-CONTA       
                MOVE  SPC-COD-NATUREZA-CATU TO  WRK-480-RET-NATUREZA    
                MOVE  SPC-CARTEIRA-CATU     TO  WRK-480-RET-CCART       
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
           WHEN 031                                                     
                MOVE  SPC-AGENCIA-CATU      TO  WRK-480-RET-AGENCIA     
                MOVE  SPC-NUM-CC-CATU       TO  WRK-480-RET-CONTA       
                MOVE  SPC-COD-NATUREZA-CATU TO  WRK-480-RET-NATUREZA    
                MOVE  SPC-CARTEIRA-CATU     TO  WRK-480-RET-CCART       
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 032                                                     
                MOVE  SPC-NUMCGC-CATU       TO  WRK-480-RET-CCGC-CPF    
                MOVE  SPC-FILCGC-CATU       TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  SPC-CTRCGC-CATU       TO  WRK-480-RET-CCTRL-CGC   
           WHEN 033                                                     
                MOVE  SPC-NUMCGC-CATU       TO  WRK-480-RET-CCGC-CPF    
                MOVE  SPC-FILCGC-CATU       TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  SPC-CTRCGC-CATU       TO  WRK-480-RET-CCTRL-CGC   
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 034                                                     
                MOVE  SPC-NUMCGC-CATU       TO  WRK-480-RET-CCGC-CPF    
                MOVE  SPC-FILCGC-CATU       TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  SPC-CTRCGC-CATU       TO  WRK-480-RET-CCTRL-CGC   
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
           WHEN 035                                                     
                MOVE  SPC-NUMCGC-CATU       TO  WRK-480-RET-CCGC-CPF    
                MOVE  SPC-FILCGC-CATU       TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  SPC-CTRCGC-CATU       TO  WRK-480-RET-CCTRL-CGC   
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 036                                                     
                MOVE  SPC-NUMCGC-CATU       TO  WRK-480-RET-CCGC-CPF    
                MOVE  SPC-FILCGC-CATU       TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  SPC-CTRCGC-CATU       TO  WRK-480-RET-CCTRL-CGC   
                MOVE  SPC-CARTEIRA-CATU     TO  WRK-480-RET-CCART       
           WHEN 037                                                     
                MOVE  SPC-NUMCGC-CATU       TO  WRK-480-RET-CCGC-CPF    
                MOVE  SPC-FILCGC-CATU       TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  SPC-CTRCGC-CATU       TO  WRK-480-RET-CCTRL-CGC   
                MOVE  SPC-CARTEIRA-CATU     TO  WRK-480-RET-CCART       
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 038                                                     
                MOVE  SPC-NUMCGC-CATU       TO  WRK-480-RET-CCGC-CPF    
                MOVE  SPC-FILCGC-CATU       TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  SPC-CTRCGC-CATU       TO  WRK-480-RET-CCTRL-CGC   
                MOVE  SPC-CARTEIRA-CATU     TO  WRK-480-RET-CCART       
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
           WHEN 039                                                     
                MOVE  SPC-NUMCGC-CATU       TO  WRK-480-RET-CCGC-CPF    
                MOVE  SPC-FILCGC-CATU       TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  SPC-CTRCGC-CATU       TO  WRK-480-RET-CCTRL-CGC   
                MOVE  SPC-CARTEIRA-CATU     TO  WRK-480-RET-CCART       
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 040                                                     
                MOVE  SPC-NUMCGC-CATU       TO  WRK-480-RET-CCGC-CPF    
                MOVE  SPC-FILCGC-CATU       TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  SPC-CTRCGC-CATU       TO  WRK-480-RET-CCTRL-CGC   
                MOVE  SPC-COD-NATUREZA-CATU TO  WRK-480-RET-NATUREZA    
           WHEN 041                                                     
                MOVE  SPC-NUMCGC-CATU       TO  WRK-480-RET-CCGC-CPF    
                MOVE  SPC-FILCGC-CATU       TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  SPC-CTRCGC-CATU       TO  WRK-480-RET-CCTRL-CGC   
                MOVE  SPC-COD-NATUREZA-CATU TO  WRK-480-RET-NATUREZA    
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 042                                                     
                MOVE  SPC-NUMCGC-CATU       TO  WRK-480-RET-CCGC-CPF    
                MOVE  SPC-FILCGC-CATU       TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  SPC-CTRCGC-CATU       TO  WRK-480-RET-CCTRL-CGC   
                MOVE  SPC-COD-NATUREZA-CATU TO  WRK-480-RET-NATUREZA    
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
           WHEN 043                                                     
                MOVE  SPC-NUMCGC-CATU       TO  WRK-480-RET-CCGC-CPF    
                MOVE  SPC-FILCGC-CATU       TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  SPC-CTRCGC-CATU       TO  WRK-480-RET-CCTRL-CGC   
                MOVE  SPC-COD-NATUREZA-CATU TO  WRK-480-RET-NATUREZA    
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 044                                                     
                MOVE  SPC-NUMCGC-CATU       TO  WRK-480-RET-CCGC-CPF    
                MOVE  SPC-FILCGC-CATU       TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  SPC-CTRCGC-CATU       TO  WRK-480-RET-CCTRL-CGC   
                MOVE  SPC-COD-NATUREZA-CATU TO  WRK-480-RET-NATUREZA    
                MOVE  SPC-CARTEIRA-CATU     TO  WRK-480-RET-CCART       
           WHEN 045                                                     
                MOVE  SPC-NUMCGC-CATU       TO  WRK-480-RET-CCGC-CPF    
                MOVE  SPC-FILCGC-CATU       TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  SPC-CTRCGC-CATU       TO  WRK-480-RET-CCTRL-CGC   
                MOVE  SPC-COD-NATUREZA-CATU TO  WRK-480-RET-NATUREZA    
                MOVE  SPC-CARTEIRA-CATU     TO  WRK-480-RET-CCART       
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 046                                                     
                MOVE  SPC-NUMCGC-CATU       TO  WRK-480-RET-CCGC-CPF    
                MOVE  SPC-FILCGC-CATU       TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  SPC-CTRCGC-CATU       TO  WRK-480-RET-CCTRL-CGC   
                MOVE  SPC-COD-NATUREZA-CATU TO  WRK-480-RET-NATUREZA    
                MOVE  SPC-CARTEIRA-CATU     TO  WRK-480-RET-CCART       
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
           WHEN 047                                                     
                MOVE  SPC-NUMCGC-CATU       TO  WRK-480-RET-CCGC-CPF    
                MOVE  SPC-FILCGC-CATU       TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  SPC-CTRCGC-CATU       TO  WRK-480-RET-CCTRL-CGC   
                MOVE  SPC-COD-NATUREZA-CATU TO  WRK-480-RET-NATUREZA    
                MOVE  SPC-CARTEIRA-CATU     TO  WRK-480-RET-CCART       
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 048                                                     
                MOVE  SPC-NUMCGC-CATU       TO  WRK-480-RET-CCGC-CPF    
                MOVE  SPC-FILCGC-CATU       TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  SPC-CTRCGC-CATU       TO  WRK-480-RET-CCTRL-CGC   
                MOVE  SPC-AGENCIA-CATU      TO  WRK-480-RET-AGENCIA     
                MOVE  SPC-NUM-CC-CATU       TO  WRK-480-RET-CONTA       
           WHEN 049                                                     
                MOVE  SPC-NUMCGC-CATU       TO  WRK-480-RET-CCGC-CPF    
                MOVE  SPC-FILCGC-CATU       TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  SPC-CTRCGC-CATU       TO  WRK-480-RET-CCTRL-CGC   
                MOVE  SPC-AGENCIA-CATU      TO  WRK-480-RET-AGENCIA     
                MOVE  SPC-NUM-CC-CATU       TO  WRK-480-RET-CONTA       
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 050                                                     
                MOVE  SPC-NUMCGC-CATU       TO  WRK-480-RET-CCGC-CPF    
                MOVE  SPC-FILCGC-CATU       TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  SPC-CTRCGC-CATU       TO  WRK-480-RET-CCTRL-CGC   
                MOVE  SPC-AGENCIA-CATU      TO  WRK-480-RET-AGENCIA     
                MOVE  SPC-NUM-CC-CATU       TO  WRK-480-RET-CONTA       
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
           WHEN 051                                                     
                MOVE  SPC-NUMCGC-CATU       TO  WRK-480-RET-CCGC-CPF    
                MOVE  SPC-FILCGC-CATU       TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  SPC-CTRCGC-CATU       TO  WRK-480-RET-CCTRL-CGC   
                MOVE  SPC-AGENCIA-CATU      TO  WRK-480-RET-AGENCIA     
                MOVE  SPC-NUM-CC-CATU       TO  WRK-480-RET-CONTA       
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 052                                                     
                MOVE  SPC-NUMCGC-CATU       TO  WRK-480-RET-CCGC-CPF    
                MOVE  SPC-FILCGC-CATU       TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  SPC-CTRCGC-CATU       TO  WRK-480-RET-CCTRL-CGC   
                MOVE  SPC-AGENCIA-CATU      TO  WRK-480-RET-AGENCIA     
                MOVE  SPC-NUM-CC-CATU       TO  WRK-480-RET-CONTA       
                MOVE  SPC-CARTEIRA-CATU     TO  WRK-480-RET-CCART       
           WHEN 053                                                     
                MOVE  SPC-NUMCGC-CATU       TO  WRK-480-RET-CCGC-CPF    
                MOVE  SPC-FILCGC-CATU       TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  SPC-CTRCGC-CATU       TO  WRK-480-RET-CCTRL-CGC   
                MOVE  SPC-AGENCIA-CATU      TO  WRK-480-RET-AGENCIA     
                MOVE  SPC-NUM-CC-CATU       TO  WRK-480-RET-CONTA       
                MOVE  SPC-CARTEIRA-CATU     TO  WRK-480-RET-CCART       
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 054                                                     
                MOVE  SPC-NUMCGC-CATU       TO  WRK-480-RET-CCGC-CPF    
                MOVE  SPC-FILCGC-CATU       TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  SPC-CTRCGC-CATU       TO  WRK-480-RET-CCTRL-CGC   
                MOVE  SPC-AGENCIA-CATU      TO  WRK-480-RET-AGENCIA     
                MOVE  SPC-NUM-CC-CATU       TO  WRK-480-RET-CONTA       
                MOVE  SPC-CARTEIRA-CATU     TO  WRK-480-RET-CCART       
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
           WHEN 055                                                     
                MOVE  SPC-NUMCGC-CATU       TO  WRK-480-RET-CCGC-CPF    
                MOVE  SPC-FILCGC-CATU       TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  SPC-CTRCGC-CATU       TO  WRK-480-RET-CCTRL-CGC   
                MOVE  SPC-AGENCIA-CATU      TO  WRK-480-RET-AGENCIA     
                MOVE  SPC-NUM-CC-CATU       TO  WRK-480-RET-CONTA       
                MOVE  SPC-CARTEIRA-CATU     TO  WRK-480-RET-CCART       
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 056                                                     
                MOVE  SPC-NUMCGC-CATU       TO  WRK-480-RET-CCGC-CPF    
                MOVE  SPC-FILCGC-CATU       TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  SPC-CTRCGC-CATU       TO  WRK-480-RET-CCTRL-CGC   
                MOVE  SPC-AGENCIA-CATU      TO  WRK-480-RET-AGENCIA     
                MOVE  SPC-NUM-CC-CATU       TO  WRK-480-RET-CONTA       
                MOVE  SPC-COD-NATUREZA-CATU TO  WRK-480-RET-NATUREZA    
           WHEN 057                                                     
                MOVE  SPC-NUMCGC-CATU       TO  WRK-480-RET-CCGC-CPF    
                MOVE  SPC-FILCGC-CATU       TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  SPC-CTRCGC-CATU       TO  WRK-480-RET-CCTRL-CGC   
                MOVE  SPC-AGENCIA-CATU      TO  WRK-480-RET-AGENCIA     
                MOVE  SPC-NUM-CC-CATU       TO  WRK-480-RET-CONTA       
                MOVE  SPC-COD-NATUREZA-CATU TO  WRK-480-RET-NATUREZA    
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 058                                                     
                MOVE  SPC-NUMCGC-CATU       TO  WRK-480-RET-CCGC-CPF    
                MOVE  SPC-FILCGC-CATU       TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  SPC-CTRCGC-CATU       TO  WRK-480-RET-CCTRL-CGC   
                MOVE  SPC-AGENCIA-CATU      TO  WRK-480-RET-AGENCIA     
                MOVE  SPC-NUM-CC-CATU       TO  WRK-480-RET-CONTA       
                MOVE  SPC-COD-NATUREZA-CATU TO  WRK-480-RET-NATUREZA    
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
           WHEN 059                                                     
                MOVE  SPC-NUMCGC-CATU       TO  WRK-480-RET-CCGC-CPF    
                MOVE  SPC-FILCGC-CATU       TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  SPC-CTRCGC-CATU       TO  WRK-480-RET-CCTRL-CGC   
                MOVE  SPC-AGENCIA-CATU      TO  WRK-480-RET-AGENCIA     
                MOVE  SPC-NUM-CC-CATU       TO  WRK-480-RET-CONTA       
                MOVE  SPC-COD-NATUREZA-CATU TO  WRK-480-RET-NATUREZA    
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 060                                                     
                MOVE  SPC-NUMCGC-CATU       TO  WRK-480-RET-CCGC-CPF    
                MOVE  SPC-FILCGC-CATU       TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  SPC-CTRCGC-CATU       TO  WRK-480-RET-CCTRL-CGC   
                MOVE  SPC-AGENCIA-CATU      TO  WRK-480-RET-AGENCIA     
                MOVE  SPC-NUM-CC-CATU       TO  WRK-480-RET-CONTA       
                MOVE  SPC-COD-NATUREZA-CATU TO  WRK-480-RET-NATUREZA    
                MOVE  SPC-CARTEIRA-CATU     TO  WRK-480-RET-CCART       
           WHEN 061                                                     
                MOVE  SPC-NUMCGC-CATU       TO  WRK-480-RET-CCGC-CPF    
                MOVE  SPC-FILCGC-CATU       TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  SPC-CTRCGC-CATU       TO  WRK-480-RET-CCTRL-CGC   
                MOVE  SPC-AGENCIA-CATU      TO  WRK-480-RET-AGENCIA     
                MOVE  SPC-NUM-CC-CATU       TO  WRK-480-RET-CONTA       
                MOVE  SPC-COD-NATUREZA-CATU TO  WRK-480-RET-NATUREZA    
                MOVE  SPC-CARTEIRA-CATU     TO  WRK-480-RET-CCART       
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
           WHEN 062                                                     
                MOVE  SPC-NUMCGC-CATU       TO  WRK-480-RET-CCGC-CPF    
                MOVE  SPC-FILCGC-CATU       TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  SPC-CTRCGC-CATU       TO  WRK-480-RET-CCTRL-CGC   
                MOVE  SPC-AGENCIA-CATU      TO  WRK-480-RET-AGENCIA     
                MOVE  SPC-NUM-CC-CATU       TO  WRK-480-RET-CONTA       
                MOVE  SPC-COD-NATUREZA-CATU TO  WRK-480-RET-NATUREZA    
                MOVE  SPC-CARTEIRA-CATU     TO  WRK-480-RET-CCART       
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
           WHEN 063                                                     
                MOVE  SPC-NUMCGC-CATU       TO  WRK-480-RET-CCGC-CPF    
                MOVE  SPC-FILCGC-CATU       TO  WRK-480-RET-CFLIAL-CGC  
                MOVE  SPC-CTRCGC-CATU       TO  WRK-480-RET-CCTRL-CGC   
                MOVE  SPC-AGENCIA-CATU      TO  WRK-480-RET-AGENCIA     
                MOVE  SPC-NUM-CC-CATU       TO  WRK-480-RET-CONTA       
                MOVE  SPC-COD-NATUREZA-CATU TO  WRK-480-RET-NATUREZA    
                MOVE  SPC-CARTEIRA-CATU     TO  WRK-480-RET-CCART       
                MOVE  WRK-BLOQ-OPER         TO  WRK-480-RET-BLOQ-OPER   
                MOVE  WRK-UF-NUM            TO  WRK-480-RET-UF          
                                                                        
           END-EVALUATE.                                                
                                                                        
           CALL   WRK-MODULO       USING        WRK-480-CODIGO3         
                                                WRK-480-NOME-TABELA     
                                                WRK-480-RETORNO-CHAVE   
                                                WRK-480-RETORNO.        
                                                                        
           IF   RETURN-CODE        EQUAL        ZEROS                   
                MOVE    099                 TO  WRK-PESQ.               
                                                                        
      *---------------------------------------------------------------* 
       315000-99-FIM. EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       320000-MOVIMENTA-REGISTRO SECTION.                               
      *---------------------------------------------------------------* 
                                                                        
           MOVE    REG-CADSEATU   TO   REG-SORT                         
                                                                        
           RELEASE REG-SORT                                             
                                                                        
           PERFORM 310000-LER-CADSPC.                                   
                                                                        
      *---------------------------------------------------------------* 
       320000-99-FIM. EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       400000-SAIDA-DO-SORT SECTION.                                    
      *---------------------------------------------------------------* 
                                                                        
           PERFORM 410000-LER-ARQSORT                                   
                                                                        
           IF     WRK-FIM-ARQSORT NOT EQUAL    'S'                      
                  MOVE   SORT-CGC-ATU  TO  SORT-CGC-ANT.                
                                                                        
           PERFORM 420000-PROCESSA-SORT  UNTIL                          
                                         WRK-FIM-ARQSORT  EQUAL  'S'.   
                                                                        
           PERFORM 430000-GERAR-RELATORIO.                              
                                                                        
      *---------------------------------------------------------------* 
       400000-99-FIM. EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       410000-LER-ARQSORT SECTION.                                      
      *---------------------------------------------------------------* 
                                                                        
           RETURN   SORTWK01    AT    END                               
                    MOVE  'S'          TO   WRK-FIM-ARQSORT             
                    MOVE  HIGH-VALUES  TO   SORT-CGC-ATU                
                    GO                 TO   410000-99-FIM.              
                                                                        
           IF     SOR-VCMTO     LESS   20000914                         
                  GO      TO    410000-LER-ARQSORT.                     
                                                                        
           MOVE   SOR-CGC       TO   SORT-CGC-ATU.                      
                                                                        
      *---------------------------------------------------------------* 
       410000-99-FIM. EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       420000-PROCESSA-SORT SECTION.                                    
      *---------------------------------------------------------------* 
                                                                        
           PERFORM 421000-QUEBRA-CGC  UNTIL                             
                                      SORT-CGC-ATU NOT EQUAL            
                                      SORT-CGC-ANT.                     
           IF WRK-MOV-REG    EQUAL     'S'                              
              IF WRK-IDENT   EQUAL     '99' AND                         
                 WRK-TEM-05-20-40   NOT EQUAL 'S'                       
                 ADD  WRK-CONTA-99  TO ACU-EXCLUIDOS                    
              ELSE                                                      
                 PERFORM 422000-GRAVA-CADCARTA.                         
                                                                        
           MOVE  'S'   TO  WRK-PRIMEIRA-VEZ                             
           MOVE  'N'   TO  WRK-MOV-REG                                  
                           WRK-TEM-05-20-40.                            
                                                                        
           MOVE  SORT-CGC-ATU  TO  SORT-CGC-ANT.                        
                                                                        
      *---------------------------------------------------------------* 
       420000-99-FIM. EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       421000-QUEBRA-CGC SECTION.                                       
      *---------------------------------------------------------------* 
                                                                        
           IF  WRK-PRIMEIRA-VEZ     EQUAL  'S'
               MOVE   'N'    TO     WRK-PRIMEIRA-VEZ                    
               MOVE  ZEROS   TO     WRK-IND                             
               MOVE  1       TO     WRK-CONTA-99                        
               IF  (SOR-VCMTO      GREATER WRK-DATA-08 AND              
                    SOR-VCMTO NOT GREATER WRK-DATA-1A-CARTA ) OR        
                   (SOR-VCMTO      GREATER WRK-DATA-21 AND              
                    SOR-VCMTO NOT GREATER WRK-DATA-2A-CARTA ) OR        
                   (SOR-VCMTO      GREATER WRK-DATA-41 AND              
                    SOR-VCMTO NOT GREATER WRK-DATA-3A-CARTA ) OR        
                    SOR-VCMTO NOT GREATER WRK-DATA-41                   
                   PERFORM 421200-MONTA-CADCARTA                        
                   MOVE   'S'       TO     WRK-MOV-REG                  
                   PERFORM 421100-ACUMULAR-OCORR                        
                  IF (SOR-VCMTO      GREATER WRK-DATA-08 AND            
                     SOR-VCMTO NOT GREATER WRK-DATA-1A-CARTA )          
                      MOVE  WRK-DIAS1   TO   WRK-IDENT                  
                  ELSE                                                  
                  IF (SOR-VCMTO      GREATER WRK-DATA-21 AND            
                     SOR-VCMTO NOT GREATER WRK-DATA-2A-CARTA )          
                      MOVE  WRK-DIAS2   TO   WRK-IDENT                  
                  ELSE                                                  
                  IF (SOR-VCMTO      GREATER WRK-DATA-41 AND            
                      SOR-VCMTO NOT GREATER WRK-DATA-3A-CARTA )         
                      MOVE  WRK-DIAS3   TO   WRK-IDENT                  
                  ELSE                                                  
                      MOVE  WRK-DIAS4   TO   WRK-IDENT                  
               ELSE                                                     
                   ADD  1   TO   ACU-EXCLUIDOS                          
           ELSE                                                         
               IF  WRK-MOV-REG  EQUAL  'S'                              
                   ADD       1      TO     WRK-CONTA-99
                   PERFORM 421300-MONTA-CADCARTA-2VEZ
                   PERFORM 421100-ACUMULAR-OCORR                        
                   IF (SOR-VCMTO      GREATER WRK-DATA-08 AND           
                       SOR-VCMTO NOT GREATER WRK-DATA-1A-CARTA ) OR     
                      (SOR-VCMTO      GREATER WRK-DATA-21 AND           
                       SOR-VCMTO NOT GREATER WRK-DATA-2A-CARTA ) OR     
                      (SOR-VCMTO      GREATER WRK-DATA-41 AND           
                       SOR-VCMTO NOT GREATER WRK-DATA-3A-CARTA ) OR     
                       SOR-VCMTO NOT GREATER WRK-DATA-41                
                       MOVE  'S'    TO     WRK-TEM-05-20-40             
                   ELSE                                                 
                       NEXT SENTENCE                                    
                   END-IF                                               
               ELSE                                                     
                   ADD  1   TO   ACU-EXCLUIDOS                          
               END-IF                                                   
           END-IF.                                                      
                                                                        
           IF  WRK-IND                 LESS 12                          
               MOVE    SORT-CGC-ATU    TO  SORT-CGC-ANT                 
               PERFORM 410000-LER-ARQSORT                               
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       421000-99-FIM. EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       421100-ACUMULAR-OCORR SECTION.
      *---------------------------------------------------------------* 
                                                                        
           COMPUTE WRK-IND             EQUAL WRK-IND + 1                
           IF  WRK-IND                 GREATER 11                       
               MOVE   'S'              TO WRK-PRIMEIRA-VEZ
               PERFORM 422000-GRAVA-CADCARTA                            
               GO   TO  421100-99-FIM                                   
           END-IF.                                                      
                                                                        
           MOVE SPACES               TO SELE-POSSIBILITI     (WRK-IND). 
           MOVE SOR-NATURE           TO SELE-NATUREZA        (WRK-IND). 
           MOVE SOR-VCMTO            TO SELE-DAT-VENCTO      (WRK-IND). 
           MOVE SOR-CONTRATO-CATU    TO SELE-CONTRATO        (WRK-IND). 
           MOVE SOR-CARTEIRA-CATU    TO SELE-CARTEIRA        (WRK-IND). 
           MOVE ZEROS                TO SELE-IOF-NORMAL      (WRK-IND). 
           MOVE SOR-RESGATE          TO SELE-RESGATE         (WRK-IND). 
           MOVE SOR-VR-REMUNERATORIO TO SELE-VR-REMUNERATORIO (WRK-IND) 
           MOVE SOR-VALOR-MORATORIO  TO SELE-VALOR-MORATORIO (WRK-IND). 
           MOVE SOR-VALOR-MULTA      TO SELE-VALOR-MULTA     (WRK-IND). 
           MOVE SOR-DESP-JUD-CUSTAS  TO SELE-DESP-JUD-CUSTAS (WRK-IND). 
           MOVE SOR-HONORARIOS       TO SELE-HONORARIOS      (WRK-IND). 
           MOVE SOR-VL-TOTAL-DIVIDA  TO SELE-VL-TOTAL-DIVIDA (WRK-IND). 
           MOVE SOR-VL-TAXA-TARIFA   TO SELE-VL-TAXA-TARIFA  (WRK-IND). 
                                                                        
      *---------------------------------------------------------------* 
       421100-99-FIM. EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       421200-MONTA-CADCARTA SECTION.                                   
      *---------------------------------------------------------------* 

           MOVE REG-SORT                 TO WRK-SORT
           MOVE SOR-NUMCGC               TO SELE-NUMCPF.                
           MOVE SOR-FILCGC               TO SELE-FILCPF.                
           MOVE SOR-DIGCGC               TO SELE-CTRCPF.                
           MOVE SOR-EMPRESA-CATU         TO SELE-EMPRESA.               
           MOVE SOR-AGENCIA-CATU         TO SELE-AGENCIA.               
           MOVE SOR-NUM-CC-CATU          TO SELE-NUM-CC.                
           MOVE SOR-VALOR-VENCIDOS-CATU  TO SELE-VALOR-VENCIDOS.        
           MOVE SOR-VALOR-VINCENDOS-CATU TO SELE-VALOR-VINCENDOS.       
           MOVE SOR-NOMERESP-CATU        TO SELE-NOMERESP.              
           MOVE SOR-NUMCGC-CATU          TO SELE-NUMCGC.                
           MOVE SOR-FILCGC-CATU          TO SELE-FILCGC.                
           MOVE SOR-CTRCGC-CATU          TO SELE-CTRCGC.                
           MOVE SOR-DATA-NASCIMEN-CATU   TO SELE-DATA-NASCIMEN.         
           MOVE SOR-NATURALIDADE-CATU    TO SELE-NATURALIDADE.          
           MOVE SOR-UF-ORIGEM-CATU       TO SELE-UF-ORIGEM.             
           MOVE SOR-NOME-CONJUGE-CATU    TO SELE-NOME-CONJUGE.          
           MOVE SOR-DEV-ENDER-CATU       TO SELE-DEV-ENDER.             
           MOVE SOR-DEV-NRO-CATU         TO SELE-DEV-NRO.               
           MOVE SOR-DEV-COMPL-CATU       TO SELE-DEV-COMPL.             
           MOVE SOR-DEV-BAIRRO-CATU      TO SELE-DEV-BAIRRO.            
           MOVE SOR-DEV-CIDADE-CATU      TO SELE-DEV-CIDADE.            
           MOVE SOR-DEV-UF-CATU          TO SELE-DEV-UF.                
           MOVE SOR-DEV-CCEP-CATU        TO SELE-DEV-CCEP.              
           MOVE SOR-DEV-CCEP-COMPL-CATU  TO SELE-DEV-CCEP-COMPL.        
           MOVE SOR-NOMEAVAL1-CATU       TO SELE-NOMEAVAL1.             
           MOVE SOR-CGCCPFAVAL1-CATU     TO SELE-CGCCPF-AVAL1.          
           MOVE SOR-FILCGC-AVAL1-CATU    TO SELE-FILCGC-AVAL1.          
           MOVE SOR-CTRCGC-AVAL1-CATU    TO SELE-CTRCGC-AVAL1.          
           MOVE SOR-AVA-ENDER-CATU       TO SELE-AVA-ENDER.             
           MOVE SOR-AVA-NRO-CATU         TO SELE-AVA-NRO.               
           MOVE SOR-AVA-COMPL-CATU       TO SELE-AVA-COMPL.             
           MOVE SOR-AVA-BAIRRO-CATU      TO SELE-AVA-BAIRRO.            
           MOVE SOR-AVA-CIDADE-CATU      TO SELE-AVA-CIDADE.            
           MOVE SOR-AVA-UF-CATU          TO SELE-AVA-UF.                
           MOVE SOR-AVA-CCEP-CATU        TO SELE-AVA-CCEP.              
           MOVE SOR-AVA-CCEP-COMPL-CATU  TO SELE-AVA-CCEP-COMPL.        
           MOVE SOR-NOMEAVAL2-CATU       TO SELE-NOMEAVAL2.             
           MOVE SOR-CGCCPFAVAL2-CATU     TO SELE-CGCCPF-AVAL2.          
           MOVE SOR-FILCGC-AVAL2-CATU    TO SELE-FILCGC-AVAL2.          
           MOVE SOR-CTRCGC-AVAL2-CATU    TO SELE-CTRCGC-AVAL2.          
           MOVE SOR-NATUREZA-OPER-CATU   TO SELE-NATUREZA-OPER.         
           MOVE SOR-NUMERO-CL-CATU       TO SELE-NUMERO-CL.             
           MOVE SOR-DATA-OCORR-CATU      TO SELE-DATA-OCORR.            
           MOVE SOR-COD-NATUREZA-CATU    TO SELE-COD-NATUREZA.          
           MOVE SOR-DTECL-CATU           TO SELE-DTECL.                 
           MOVE SOR-DTBCL-CATU           TO SELE-DTBCL.                 
           MOVE SOR-DTELP-CATU           TO SELE-DTELP.                 
           MOVE SOR-DTBLP-CATU           TO SELE-DTBLP.                 
           MOVE SOR-ID-LPCL-CATU         TO SELE-ID-LPCL.               
           MOVE SOR-TIPBX-CATU           TO SELE-TIPBX.                 
           MOVE SOR-TIPO-CATU            TO SELE-TIPO.                  
           MOVE SOR-TPO-CLIENTE          TO SELE-TPO-CLIENTE.           
           MOVE SOR-ORIG-ENDER           TO SELE-ORIG-ENDER.            
           MOVE SOR-TPO-LOGRA            TO SELE-TPO-LOGRA.             
           MOVE SOR-SIT-LOCALIZ          TO SELE-CSIT-LOCALIZ.          
                                                                        
      *---------------------------------------------------------------* 
       421200-99-FIM. EXIT.                                             
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       421300-MONTA-CADCARTA-2VEZ        SECTION.
      *---------------------------------------------------------------*

           MOVE WRK-NUMCGC               TO SELE-NUMCPF.
           MOVE WRK-FILCGC               TO SELE-FILCPF.
           MOVE WRK-DIGCGC               TO SELE-CTRCPF.
           MOVE WRK-EMPRESA-CATU         TO SELE-EMPRESA.
           MOVE WRK-AGENCIA-CATU         TO SELE-AGENCIA.
           MOVE WRK-NUM-CC-CATU          TO SELE-NUM-CC.
           MOVE WRK-VALOR-VENCIDOS-CATU  TO SELE-VALOR-VENCIDOS.
           MOVE WRK-VALOR-VINCENDOS-CATU TO SELE-VALOR-VINCENDOS.
           MOVE WRK-NOMERESP-CATU        TO SELE-NOMERESP.
           MOVE WRK-NUMCGC-CATU          TO SELE-NUMCGC.
           MOVE WRK-FILCGC-CATU          TO SELE-FILCGC.
           MOVE WRK-CTRCGC-CATU          TO SELE-CTRCGC.
           MOVE WRK-DATA-NASCIMEN-CATU   TO SELE-DATA-NASCIMEN.
           MOVE WRK-NATURALIDADE-CATU    TO SELE-NATURALIDADE.
           MOVE WRK-UF-ORIGEM-CATU       TO SELE-UF-ORIGEM.
           MOVE WRK-NOME-CONJUGE-CATU    TO SELE-NOME-CONJUGE.
           MOVE WRK-DEV-ENDER-CATU       TO SELE-DEV-ENDER.
           MOVE WRK-DEV-NRO-CATU         TO SELE-DEV-NRO.
           MOVE WRK-DEV-COMPL-CATU       TO SELE-DEV-COMPL.
           MOVE WRK-DEV-BAIRRO-CATU      TO SELE-DEV-BAIRRO.
           MOVE WRK-DEV-CIDADE-CATU      TO SELE-DEV-CIDADE.
           MOVE WRK-DEV-UF-CATU          TO SELE-DEV-UF.
           MOVE WRK-DEV-CCEP-CATU        TO SELE-DEV-CCEP.
           MOVE WRK-DEV-CCEP-COMPL-CATU  TO SELE-DEV-CCEP-COMPL.
           MOVE WRK-NOMEAVAL1-CATU       TO SELE-NOMEAVAL1.
           MOVE WRK-CGCCPFAVAL1-CATU     TO SELE-CGCCPF-AVAL1.
           MOVE WRK-FILCGC-AVAL1-CATU    TO SELE-FILCGC-AVAL1.
           MOVE WRK-CTRCGC-AVAL1-CATU    TO SELE-CTRCGC-AVAL1.
           MOVE WRK-AVA-ENDER-CATU       TO SELE-AVA-ENDER.
           MOVE WRK-AVA-NRO-CATU         TO SELE-AVA-NRO.
           MOVE WRK-AVA-COMPL-CATU       TO SELE-AVA-COMPL.
           MOVE WRK-AVA-BAIRRO-CATU      TO SELE-AVA-BAIRRO.
           MOVE WRK-AVA-CIDADE-CATU      TO SELE-AVA-CIDADE.
           MOVE WRK-AVA-UF-CATU          TO SELE-AVA-UF.
           MOVE WRK-AVA-CCEP-CATU        TO SELE-AVA-CCEP.
           MOVE WRK-AVA-CCEP-COMPL-CATU  TO SELE-AVA-CCEP-COMPL.
           MOVE WRK-NOMEAVAL2-CATU       TO SELE-NOMEAVAL2.
           MOVE WRK-CGCCPFAVAL2-CATU     TO SELE-CGCCPF-AVAL2.
           MOVE WRK-FILCGC-AVAL2-CATU    TO SELE-FILCGC-AVAL2.
           MOVE WRK-CTRCGC-AVAL2-CATU    TO SELE-CTRCGC-AVAL2.
           MOVE WRK-NATUREZA-OPER-CATU   TO SELE-NATUREZA-OPER.
           MOVE WRK-NUMERO-CL-CATU       TO SELE-NUMERO-CL.
           MOVE WRK-DATA-OCORR-CATU      TO SELE-DATA-OCORR.
           MOVE WRK-COD-NATUREZA-CATU    TO SELE-COD-NATUREZA.
           MOVE WRK-DTECL-CATU           TO SELE-DTECL.
           MOVE WRK-DTBCL-CATU           TO SELE-DTBCL.
           MOVE WRK-DTELP-CATU           TO SELE-DTELP.
           MOVE WRK-DTBLP-CATU           TO SELE-DTBLP.
           MOVE WRK-ID-LPCL-CATU         TO SELE-ID-LPCL.
           MOVE WRK-TIPBX-CATU           TO SELE-TIPBX.
           MOVE WRK-TIPO-CATU            TO SELE-TIPO.
           MOVE WRK-TPO-CLIENTE          TO SELE-TPO-CLIENTE.
           MOVE WRK-ORIG-ENDER           TO SELE-ORIG-ENDER.
           MOVE WRK-TPO-LOGRA            TO SELE-TPO-LOGRA.
           MOVE WRK-SIT-LOCALIZ          TO SELE-CSIT-LOCALIZ.

      *---------------------------------------------------------------*
       421300-99-FIM. EXIT.
      *---------------------------------------------------------------*
                                                                        
      *---------------------------------------------------------------* 
       422000-GRAVA-CADCARTA SECTION.                                   
      *---------------------------------------------------------------* 
                                                                        
           MOVE WRK-IDENT-N              TO SELE-IDENT.                 
           WRITE  REG-ARQSELE.                                          
                                                                        
           MOVE    WRK-GRAVACAO  TO  WRK-OPERACAO                       
           PERFORM 120000-TESTAR-FS-CADCARTA.                           
                                                                        
BRQ358     PERFORM 0001-ZERAR-11-OCORRENCIAS                            
BRQ358                  VARYING WRK-IND FROM 1 BY 1 UNTIL               
BRQ358                          WRK-IND GREATER 11.                     
                                                                        
           ADD      1      TO   ACU-GRAV-CADCARTA.                      
                                                                        
      *---------------------------------------------------------------* 
       422000-99-FIM. EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       430000-GERAR-RELATORIO SECTION.                                  
      *---------------------------------------------------------------* 
                                                                        
           MOVE    WRK-DT-AAAAMMDD   TO     WRK-DATA-AUX                
           MOVE    WRK-DT-AAAA       TO     CB2-DATA-AAAA               
           MOVE    WRK-DT-MM         TO     CB2-DATA-MM                 
           MOVE    WRK-DT-DD         TO     CB2-DATA-DD                 
                                                                        
           MOVE    WRK-GRAVACAO      TO     WRK-OPERACAO                
                                                                        
           WRITE   REG-REL           FROM   CABEC1                      
           PERFORM 130000-TESTAR-FS-RELATO                              
                                                                        
           WRITE   REG-REL           FROM   CABEC2                      
           PERFORM 130000-TESTAR-FS-RELATO                              
                                                                        
           WRITE   REG-REL           FROM   CB-CARRO                    
           PERFORM 130000-TESTAR-FS-RELATO                              
                                                                        
           WRITE   REG-REL           FROM   CABEC3                      
           PERFORM 130000-TESTAR-FS-RELATO                              
                                                                        
           MOVE    ACU-LIDOS-CADSPC  TO     LD1-TOT-LIDOS               
           MOVE    ACU-GRAV-CADCARTA TO     LD3-TOT-GRAV                
           MOVE    ACU-EXCLUIDOS     TO     LD2-TOT-EXCL                
           MOVE    ACU-DESPREZADO    TO     LD4-TOT-DESP                
                                                                        
           WRITE   REG-REL           FROM   LINDET1                     
           PERFORM 130000-TESTAR-FS-RELATO                              
                                                                        
           DISPLAY       LINDET1                                        
           DISPLAY       LINDET4                                        
           DISPLAY       LINDET2                                        
           DISPLAY       LINDET3                                        
                                                                        
           WRITE   REG-REL           FROM   LINDET4.                    
           PERFORM 130000-TESTAR-FS-RELATO.                             
                                                                        
           WRITE   REG-REL           FROM   LINDET3.                    
           PERFORM 130000-TESTAR-FS-RELATO.                             
                                                                        
      *---------------------------------------------------------------* 
       430000-99-FIM. EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
      *         CARREGA    POOL0480                                   * 
      *---------------------------------------------------------------* 
                                                                        
       CARREGA-POOL0480  SECTION.                                       
                                                                        
           PERFORM  LER-TABELAL                THRU   FIM-LER-TABELAL.  
                                                                        
           IF       WRK-FS-TABELAL EQUAL '10'                           
                    GO   TO   FIM-CARREGA-POOL0480.                     
                                                                        
           PERFORM  MONTAR-TABELA            THRU     FIM-MONTAR-TABELA 
                    UNTIL                    WRK-FS-TABELAL EQUAL '10'. 
                                                                        
           CALL  WRK-MODULO         USING        WRK-480-CODIGO2        
                                                 WRK-480-NOME-TABELA.   
                                                                        
       FIM-CARREGA-POOL0480.                                            
           EXIT.                                                        
                                                                        
      *---------------------------------------------------------------* 
                                                                        
       LER-TABELAL SECTION.                                             
                                                                        
           READ    TABELAL.                                             
                                                                        
           IF  WRK-FS-TABELAL EQUAL '10'                                
               GO               TO  FIM-LER-TABELAL.                    
                                                                        
ATEND      ADD   1      TO   ACU-LIDOS-TABELAL.                         
ATEND                                                                   
ATEND.*....IF..B037-CCART.NOT.NUMERIC                                   
BRQ141     IF  B037-CCART EQUAL SPACES OR LOW-VALUES                    
ATEND          DISPLAY '*** B037-CCART = ' B037-CCART '***'             
ATEND          GO               TO  LER-TABELAL                         
ATEND      END-IF.                                                      
ATEND                                                                   
           MOVE    WRK-LEITURA  TO  WRK-OPERACAO.                       
           PERFORM 170000-TESTAR-FS-TABELAL.                            
                                                                        
       FIM-LER-TABELAL.                                                 
           EXIT.                                                        
                                                                        
      *---------------------------------------------------------------* 
                                                                        
       MONTAR-TABELA     SECTION.                                       
                                                                        
           MOVE   B037-CSGL-UF                   TO WRK-UF-ALFA.        
           PERFORM  CONVERTE-UF.                                        
                                                                        
           MOVE   B037-CCGC-CPF                  TO WRK-480-CCGC-CPF.   
           MOVE   B037-CFLIAL-CGC                TO WRK-480-CFLIAL-CGC. 
           MOVE   B037-CCTRL-CPF-CGC             TO WRK-480-CCTRL-CGC   
           MOVE   B037-CJUNC-DEPDC-BDSCO         TO WRK-480-AGENCIA.    
           MOVE   B037-CCTA-CORR                 TO WRK-480-CONTA.      
           MOVE   B037-CTPO-NATUZ-OPER           TO WRK-480-NATUREZA.   
           MOVE   B037-CCART                     TO WRK-480-CCART.      
           MOVE   B037-CREST-BLOQ-OPER           TO WRK-480-BLOQ-OPER.  
           MOVE   WRK-UF-NUM                     TO WRK-480-UF.         
                                                                        
           CALL  WRK-MODULO         USING        WRK-480-CODIGO1        
                                                 WRK-480-NOME-TABELA    
                                                 WRK-480-TAM-CHAVE      
                                                 WRK-480-TAM-OCORRENC   
                                                 WRK-480-REGISTRO.      
                                                                        
           IF     RETURN-CODE       EQUAL    4                          
                  DISPLAY '**************   CLLPG656   *************'   
                  DISPLAY '*      ESTOUROU  A  REGION  POOL0482    *'   
                  DISPLAY '*  AO CARREGAR DADOS DA TABELA LPCLB037 *'   
                  DISPLAY '**************   CLLPG656   *************'   
                  CALL  'ILBOABN0'  USING        WRK-ABEND.             
                                                                        
           PERFORM  LER-TABELAL        THRU      FIM-LER-TABELAL.       
                                                                        
       FIM-MONTAR-TABELA.                                               
           EXIT.                                                        
                                                                        
      *---------------------------------------------------------------* 
                                                                        
       CONVERTE-UF                SECTION.                              
                                                                        
           MOVE   ZEROS   TO  WRK-UF-NUM.                               
                                                                        
           IF  WRK-UF-ALFA   EQUAL  ZEROS  OR  SPACES                   
               GO  TO   FIM-CONVERTE-UF                                 
           END-IF.                                                      
                                                                        
           SET LPCL-INDX  TO  1.                                        
                                                                        
           SEARCH  LPCL-OCC-UF  VARYING  LPCL-INDX                      
                   AT END   GO  TO   FIM-CONVERTE-UF                    
                   WHEN     LPCL-OCC-UF(LPCL-INDX) = WRK-UF-ALFA        
                            SET  WRK-UF-NUM  TO  LPCL-INDX.             
                                                                        
       FIM-CONVERTE-UF.                                                 
           EXIT.                                                        
                                                                        
      *---------------------------------------------------------------* 
       666666-LER-PARMCLLP             SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           READ   PARMCLLP.                                             
                                                                        
           IF  WRK-FS-PARMCLLP   EQUAL   '10'                           
               GO                TO      666666-99-FIM.                 
                                                                        
           DISPLAY 'PARM-CCUSTO-1 = '  PARM-CCUSTO-1.                   
           DISPLAY 'PARM-CODIGO-1 = '  PARM-CODIGO-1.                   
           DISPLAY 'PARM-CCUSTO-2 = '  PARM-CCUSTO-2.                   
           DISPLAY 'PARM-CODIGO-2 = '  PARM-CODIGO-2.                   
                                                                        
                                                                        
           IF  PARM-CCUSTO-1   NOT EQUAL   'CLLP'   OR                  
               PARM-CODIGO-1   NOT EQUAL   '0002'   OR                  
               PARM-CCUSTO-2   NOT EQUAL   'CLLP'   OR                  
               PARM-CODIGO-2   NOT EQUAL   '7615'                       
               DISPLAY '****************  CLLPG656  *******************'
               DISPLAY '*           ABEND 1111 - FORCADO              *'
               DISPLAY '*  PROGRAMA CANCELADO - PARAMETRO INVALIDO    *'
               DISPLAY '* '  PARM-CCUSTO-1 ' / ' PARM-CODIGO-1          
                       ' / ' PARM-CCUSTO-2 ' / ' PARM-CODIGO-2          
               DISPLAY '*   AVISAR ANALISTA RESPONSAVEL DA ROTINA     *'
               DISPLAY '*****************  CLLPG656  ******************'
               CALL  'ILBOABN0'  USING  WRK-ABEND.                      
                                                                        
      *---------------------------------------------------------------* 
       666666-99-FIM.                  EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       777777-TRATA-PARMCLLP           SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           IF  PARM-SEQUENCIA   EQUAL   1                               
               MOVE  PARM-NR-DIAS    TO   WRK-DIAS1                     
               MOVE  PARM-VL-CORTE   TO   WRK-VL-CORTE1                 
           ELSE                                                         
               IF  PARM-SEQUENCIA   EQUAL   2                           
                   MOVE  PARM-NR-DIAS    TO   WRK-DIAS2                 
                   MOVE  PARM-VL-CORTE   TO   WRK-VL-CORTE2             
               ELSE                                                     
                   IF  PARM-SEQUENCIA   EQUAL   3                       
                       MOVE  PARM-NR-DIAS    TO   WRK-DIAS3             
                       MOVE  PARM-VL-CORTE   TO   WRK-VL-CORTE3         
                   ELSE                                                 
                       IF  PARM-SEQUENCIA   EQUAL   4                   
                           MOVE  PARM-NR-DIAS    TO   WRK-DIAS4         
                           MOVE  PARM-VL-CORTE   TO   WRK-VL-CORTE4.    
                                                                        
           PERFORM   666666-LER-PARMCLLP.                               
                                                                        
      *---------------------------------------------------------------* 
       777777-99-FIM.                  EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       888888-TESTAR-FS-PARMCLLP       SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           IF  WRK-FS-PARMCLLP   NOT EQUAL   '00'                       
               DISPLAY '************** CLLPG656 *************'          
               DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'      
               DISPLAY '*              PARMCLLP             *'          
               DISPLAY '*         FILE STATUS =  ' WRK-FS-PARMCLLP      
                                                  '         *'          
               DISPLAY '************** CLLPG656 *************'          
               CALL  'ILBOABN0'   USING    WRK-ABEND.                   
                                                                        
      *---------------------------------------------------------------* 
       888888-99-FIM.                  EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       999999-ROTINA-ERRO SECTION.                                      
      *---------------------------------------------------------------* 
                                                                        
           MOVE 'CLLPG656'              TO   ERR-PGM.                   
                                                                        
           CALL 'POOL7100'   USING WRK-BATCH ERRO-AREA.                 
                                                                        
           GOBACK.                                                      
                                                                        
      *---------------------------------------------------------------* 
       999999-99-FIM. EXIT.                                             
      *---------------------------------------------------------------* 
                                                                        
      *******TESTE***************                                       
                                                                        
       ROT-DISPLAY SECTION.                                             
            DISPLAY      'CGC = '                                       
                   SOR-NUMCGC ' '                                       
                   SOR-FILCGC ' '                                       
                   SOR-DIGCGC ' NAT. '                                  
                   SOR-NATURE ' VCTO '                                  
                   SOR-VCMTO   ' VALOR '                                
                   SOR-RESGATE.                                         

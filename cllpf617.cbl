       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID. CLLPF617.                                            
       AUTHOR. CROCETTI.                                                
                                                                        
      *===============================================================* 
      *                   C P M   S I S T E M A S                     * 
      *---------------------------------------------------------------* 
      *---------------------------------------------------------------* 
      *                                                               * 
      *      PROGRAMA     : CLLPF617                                  * 
      *      PROGRAMADOR  : ANTONIO CROCETTI DE MELO     -  CPM       * 
      *      SUPERVISOR   : SIDNEI                       -  CPM       * 
      *      ANALISTA     : LOURIVAL                     -  CPM       * 
      *      DATA         : 29/09/1997                                * 
      *                                                               * 
      *      OBJETIVO     :                                           * 
      *        COMPLEMENTAR OS CAMPOS REFERENTES A ENDERECO PARA      * 
      *        CORRESPONDENCIA E DADOS PESSOAIS, NA INCLUSAO  DE      * 
      *        CLIENTES (PESSOA FISICA) PARA ENVIO AO S.P.C.          * 
      *                                                               * 
      *      ARQUIVOS:                                                * 
      *         DDNAME                           INCLUDE/BOOK         * 
      *         PARMCLLP       LRECL = 250                            * 
      *         CADSEDIA       LRECL = 260         I#CLLPCJ           * 
      *         CDSPCATU       LRECL = 600         I#CLLPCI           * 
JL2003*         CDSPCLEA       LRECL = 600         I#CLLPCI           * 
      *         RELINCON       LRECL = 132                            * 
      *         RELCONTR       LRECL = 080                            * 
      *         ARQDATA        LRECL = 047         I#LPCLAH           * 
      *                                                               * 
      *      BANCO DE DADOS:                                          * 
      *         DB2                                                   * 
      *           TABLE                          INCLUDE/BOOK         * 
      *           DB2PRD.V01CTA_CORRENTE           CLIEV008           * 
      *           DB2PRD.V01CLIENTE_AGENCIA        CLIEV004           * 
      *           DB2PRD.V01CLIENTE_AG_PF          CLIEV006           * 
      *           DB2PRD.V01FAMILIA_CLI_PF         CLIEV011           * 
      *           DB2PRD.V01CEP                    CEPNV003           * 
      *                                                               * 
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
      *    DATA........:  11/04/2014                                   *
      *----------------------------------------------------------------*
      *    OBJETIVO....:  ADAPTACOES PARA A LEI DA TRANSPARENCIA       *
      *                   TROCAR BOOK I#CLLPCN(262) POR I#CLLPDD(362)  *
      *                   TROCAR BOOK I#CLLPCO(600) POR I#CLLPC0(700)  *
      *    PROJETO 13-0358                                             *
      *================================================================*
                                                                        
       ENVIRONMENT DIVISION.                                            
       CONFIGURATION SECTION.                                           
       SPECIAL-NAMES.                                                   
           DECIMAL-POINT IS COMMA.                                      
                                                                        
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    
                                                                        
                                                                        
           SELECT  PARMCLLP ASSIGN TO UT-S-PARMCLLP                     
                      FILE STATUS IS WRK-FS-PARMCLLP.                   
                                                                        
           SELECT  CADSEDIA ASSIGN TO UT-S-CADSEDIA                     
                      FILE STATUS IS WRK-FS-CADSEDIA.                   
                                                                        
           SELECT   ARQDATA  ASSIGN TO UT-S-ARQDATA                     
                      FILE STATUS IS WRK-FS-ARQDATA.                    
                                                                        
           SELECT CDSPCATU ASSIGN TO UT-S-CDSPCATU                      
                      FILE STATUS IS WRK-FS-CDSPCATU.                   
                                                                        
           SELECT   RELINCON ASSIGN TO UT-S-RELINCON                    
                      FILE STATUS IS WRK-FS-RELINCON.                   
                                                                        
           SELECT   RELCONTR ASSIGN TO UT-S-RELCONTR                    
                      FILE STATUS IS WRK-FS-RELCONTR.                   
                                                                        
JL2003     SELECT CDSPCLEA ASSIGN TO UT-S-CDSPCLEA                      
JL2003                FILE STATUS IS WRK-FS-CDSPCLEA.                   
                                                                        
                                                                        
       DATA DIVISION.                                                   
       FILE SECTION.                                                    
                                                                        
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
      *    INPUT:    CADASTRO SELECIONADO                             * 
      *               ORG. SEQUENCIAL   -   LRECL = 362               * 
      *---------------------------------------------------------------* 
                                                                        
       FD          CADSEDIA                                             
                   RECORDING MODE     IS F                              
                   LABEL     RECORD   IS STANDARD                       
                   BLOCK     CONTAINS 0  RECORDS.                       
                                                                        
        COPY 'I#CLLPDD'.                                                
                                                                        
                                                                        
      *---------------------------------------------------------------* 
      *    INPUT:    ARQUIVO DATA DO MOVIMENTO                        * 
      *               ORG. SEQUENCIAL   -   LRECL = 060               * 
      *---------------------------------------------------------------* 
                                                                        
       FD          ARQDATA                                              
                   RECORDING MODE     IS F                              
                   LABEL     RECORD   IS STANDARD                       
                   BLOCK     CONTAINS 0  RECORDS.                       
                                                                        
JL2003  COPY 'I#CLLPGA'.                                                
                                                                        
                                                                        
      *---------------------------------------------------------------* 
JL2003*    OUTPUT:    CADASTRO DE S.P.C  ATUAL - BRADESCO             * 
      *               ORG. SEQUENCIAL   -   LRECL = 700               * 
      *---------------------------------------------------------------* 
                                                                        
       FD          CDSPCATU                                             
                   RECORDING MODE   IS F                                
                   LABEL     RECORD IS STANDARD                         
                   BLOCK     CONTAINS 0  RECORDS.                       
                                                                        
       01          FD-CDSPCATU         PIC  X(0700).                    
                                                                        
      *---------------------------------------------------------------* 
      *    OUTPUT:    RELINCON RELATORIO DE INCONSISTENCIA              
      *               ORG. SEQUENCIAL   -   LRECL = 90                * 
      *---------------------------------------------------------------* 
                                                                        
       FD          RELINCON                                             
                   RECORDING MODE     IS F                              
                   LABEL     RECORD   IS STANDARD                       
                   BLOCK     CONTAINS 0  RECORDS.                       
                                                                        
       01          REG-RELINCON        PIC X(132).                      
                                                                        
                                                                        
      *---------------------------------------------------------------* 
      *    OUTPUT:    RELCONTR RELATORIO DE CONTROLE                    
      *               ORG. SEQUENCIAL   -   LRECL = 80                * 
      *---------------------------------------------------------------* 
                                                                        
       FD          RELCONTR                                             
                   RECORDING MODE     IS F                              
                   LABEL     RECORD   IS STANDARD                       
                   BLOCK     CONTAINS 0  RECORDS.                       
                                                                        
       01          REG-RELCONTR        PIC X(80).                       
                                                                        
JL2003*---------------------------------------------------------------* 
JL2003*    OUTPUT:    CADASTRO DE S.P.C  ATUAL - LEASING              * 
      *               ORG. SEQUENCIAL   -   LRECL = 700               * 
JL2003*---------------------------------------------------------------* 
                                                                        
JL2003 FD          CDSPCLEA                                             
JL2003             RECORDING MODE   IS F                                
JL2003             LABEL     RECORD IS STANDARD                         
JL2003             BLOCK     CONTAINS 0  RECORDS.                       
                                                                        
       01          FD-CDSPCLEA         PIC  X(0700).                    
      *                                                                 
      *---------------------------------------------------------------* 
       WORKING-STORAGE SECTION.                                         
      *---------------------------------------------------------------* 
                                                                        
       77      FILLER                  PIC X(32)        VALUE           
               '* INICIO DA WORKING CLLPF617 *'.                        
                                                                        
       77      WRK-BATCH               PIC X(08)        VALUE 'BATCH'.  
       77      WRK-ABEND               PIC S9(04) COMP  VALUE +1111.    
                                                                        
       77      WRK-NORMAIS             PIC X(01)  VALUE SPACES.         
       77      WRK-EXTRAS              PIC X(01)  VALUE SPACES.         
                                                                        
       77      WRK-DIAS1               PIC S9(03) VALUE  ZEROS.         
       77      WRK-DIAS2               PIC S9(03) VALUE  ZEROS.         
       77      WRK-DIAS3               PIC S9(03) VALUE  ZEROS.         
       77      WRK-DIAS4               PIC S9(03) VALUE  ZEROS.         
                                                                        
       77      WRK-VL-CORTE1           PIC S9(15) VALUE  ZEROS.         
       77      WRK-VL-CORTE2           PIC S9(15) VALUE  ZEROS.         
       77      WRK-VL-CORTE3           PIC S9(15) VALUE  ZEROS.         
       77      WRK-VL-CORTE4           PIC S9(15) VALUE  ZEROS.         
                                                                        
                                                                        
       77      WRK-FS-PARMCLLP         PIC X(02)  VALUE SPACES.         
       77      WRK-FS-CADSEDIA         PIC X(02)  VALUE SPACES.         
       77      WRK-FS-ARQDATA          PIC X(02)  VALUE SPACES.         
       77      WRK-FS-RELCONTR         PIC X(02)  VALUE SPACES.         
       77      WRK-FS-RELINCON         PIC X(02)  VALUE SPACES.         
       77      WRK-FS-CDSPCATU         PIC X(02)  VALUE SPACES.         
JL2003 77      WRK-FS-CDSPCLEA         PIC X(02)  VALUE SPACES.         
                                                                        
       77      WRK-OPERACAO            PIC X(13)  VALUE SPACES.         
       77      WRK-ABERTURA            PIC X(13)  VALUE 'NA ABERTURA'.  
       77      WRK-LEITURA             PIC X(13)  VALUE 'NA LEITURA'.   
       77      WRK-IMPRESSAO           PIC X(13)  VALUE 'NA IMPRESSAO'. 
       77      WRK-GRAVACAO            PIC X(13)  VALUE 'NA GRAVACAO'.  
       77      WRK-FECHAMENTO          PIC X(13)  VALUE 'NO FECHAMENTO'.
                                                                        
       01         WRK-CPFNUM.                                           
         05       WRK-CPF1             PIC 9(03).                       
         05       WRK-CPF2-CC          PIC 9(06).                       
       01         WRK-CPFNUM-R    REDEFINES   WRK-CPFNUM                
                                       PIC 9(09).                       
                                                                        
       01         WRK-AGEN-CART614.                                     
         05       FILLER               PIC 9(01)   VALUE  9.            
         05       WRK-AGEN-614         PIC 9(03)   VALUE  ZEROS.        
       01         WRK-AGEN-CART614-R   REDEFINES   WRK-AGEN-CART614     
                                       PIC 9(04).                       
                                                                        
       01      WRK-CPF-AUX             PIC 9(17) VALUE ZEROS.           
       01      WRK-CPF                 PIC 9(17) VALUE ZEROS.           
       01      WRK-CPF-R REDEFINES     WRK-CPF.                         
               03 WRK-NUM-CPF          PIC 9(09).                       
               03 WRK-FIL-CPF          PIC 9(05).                       
               03 WRK-CTR-CPF          PIC 9(03).                       
                                                                        
       01      WRK-DATA-CLIEV006       PIC 9(09)  VALUE ZEROS.          
       01      WRK-DATA-CLIEV006-R     REDEFINES  WRK-DATA-CLIEV006.    
           05  FILLER                  PIC X(01).                       
           05  WRK-ANO-CLIEV006        PIC 9(04).                       
           05  WRK-MES-CLIEV006        PIC 9(02).                       
           05  WRK-DIA-CLIEV006        PIC 9(02).                       
                                                                        
       01      WRK-DATA-NASCIMENTO     PIC X(10)  VALUE SPACES.         
       01      WRK-DATA-NASCIMENTO-R   REDEFINES  WRK-DATA-NASCIMENTO.  
           05  WRK-DIA-NASCIMENTO      PIC 9(02).                       
           05  FILLER                  PIC X(01).                       
           05  WRK-MES-NASCIMENTO      PIC 9(02).                       
           05  FILLER                  PIC X(01).                       
           05  WRK-ANO-NASCIMENTO      PIC 9(04).                       
      *                                                                 
       01      WRK-CHAVE-CADSEDIA.                                      
           05  WRK-NUM-CADSEDIA        PIC 9(09) COMP-3.                
           05  WRK-FIL-CADSEDIA        PIC 9(05) COMP-3.                
           05  WRK-COM-CADSEDIA        PIC 9(03) COMP-3.                
           05  WRK-NAT-CADSEDIA        PIC X(02).                       
           05  WRK-DAT-CADSEDIA        PIC 9(09) COMP-3.                
       01      WRK-CHAVE-CADSEDIA-R    REDEFINES WRK-CHAVE-CADSEDIA.    
           05  FILLER                  PIC X(17).                       
                                                                        
       01      WRK-VENCTO-CADSEDIA     PIC 9(09)  VALUE ZEROS.          
       01      WRK-VENCTO-CADSEDIA-R   REDEFINES  WRK-VENCTO-CADSEDIA.  
           05  FILLER                  PIC X(01).                       
           05  WRK-SEC-VCTO-CADSEDIA   PIC 9(02).                       
           05  WRK-ANO-VCTO-CADSEDIA   PIC 9(02).                       
           05  WRK-MES-VCTO-CADSEDIA   PIC 9(02).                       
           05  WRK-DIA-VCTO-CADSEDIA   PIC 9(02).                       
                                                                        
       01      WRK-CHAVE-AGCONTA-ANT.                                   
           05  WRK-AGOP-ANT            PIC 9(05) COMP-3.                
           05  WRK-CCOP-ANT            PIC 9(07) COMP-3.                
       01      WRK-CHAVE-AGCONTA-ANT-R REDEFINES WRK-CHAVE-AGCONTA-ANT. 
           05  FILLER                  PIC X(07).                       
                                                                        
       01      WRK-CHAVE-AGCONTA-ATU.                                   
           05  WRK-AGOP-ATU            PIC 9(05) COMP-3.                
           05  WRK-CCOP-ATU            PIC 9(07) COMP-3.                
       01      WRK-CHAVE-AGCONTA-ATU-R REDEFINES WRK-CHAVE-AGCONTA-ATU. 
           05  FILLER                  PIC X(07).                       
                                                                        
       01      WRK-DADOS-CWOR.                                          
           05  SPC-REG-COM-AVALISTA            PIC X(01) VALUE  SPACES. 
           05  SPC-DATA-NASCIMEN-CWOR          PIC 9(09)     COMP-3.    
           05  SPC-NATURALIDADE-CWOR           PIC X(30).               
           05  SPC-UF-ORIGEM-CWOR              PIC X(02).               
           05  SPC-NOME-CONJUGE-CWOR           PIC X(40).               
           05  SPC-DEV-ENDER-CWOR              PIC X(40).               
           05  SPC-DEV-NRO-CWOR                PIC X(07).               
           05  SPC-DEV-COMPL-CWOR              PIC X(20).               
           05  SPC-DEV-BAIRRO-CWOR             PIC X(20).               
           05  SPC-DEV-CIDADE-CWOR             PIC X(30).               
           05  SPC-DEV-UF-CWOR                 PIC X(02).               
           05  SPC-DEV-CCEP-CWOR               PIC 9(05).               
           05  SPC-DEV-CCEP-COMPL-CWOR         PIC 9(03).               
           05  SPC-AVA-ENDER-CWOR              PIC X(40).               
           05  SPC-AVA-NRO-CWOR                PIC X(07).               
           05  SPC-AVA-COMPL-CWOR              PIC X(20).               
           05  SPC-AVA-BAIRRO-CWOR             PIC X(20).               
           05  SPC-AVA-CIDADE-CWOR             PIC X(30).               
           05  SPC-AVA-UF-CWOR                 PIC X(02).               
           05  SPC-AVA-CCEP-CWOR               PIC 9(05).               
           05  SPC-AVA-CCEP-COMPL-CWOR         PIC 9(03).               
           05  CGC-CPF-AVAL1-CWOR.                                      
               07 CGCCPFAVAL1-CWOR             PIC 9(09)     COMP-3.    
               07  FILLER                      PIC 9(05)     COMP-3.    
               07  FILLER                      PIC 9(03)     COMP-3.    
           05  NOMEAVAL1-CWOR                  PIC X(40).               
           05  CGC-CPF-AVAL2-CWOR.                                      
               07 CGCCPFAVAL2-CWOR             PIC 9(09)     COMP-3.    
               07  FILLER                      PIC 9(05)     COMP-3.    
               07  FILLER                      PIC 9(03)     COMP-3.    
           05  NOMEAVAL2-CWOR                  PIC X(40).               
                                                                        
       01      WRK-ACUMULADORES.                                        
           05  ACU-DTFORALIM           PIC 9(07)  VALUE ZEROS.          
           05  ACU-DEVEDOR             PIC 9(08)  VALUE ZEROS.          
           05  ACU-AVALISTA            PIC 9(08)  VALUE ZEROS.          
           05  ACU-TOTAL               PIC 9(08)  VALUE ZEROS.          
           05  ACU-SPCDIA              PIC 9(08)  VALUE ZEROS.          
           05  ACU-INCONS              PIC 9(08)  VALUE ZEROS.          
           05  ACU-DTNAOFXA            PIC 9(08)  VALUE ZEROS.          
           05  ACU-LIN                 PIC 9(02)  VALUE 80.             
           05  ACU-PAG                 PIC 9(06)  VALUE ZEROS.          
           05  WRK-SIM-AVALISTA        PIC X(01)  VALUE SPACES.         
           05  WRK-SIM-DEVEDOR         PIC X(01)  VALUE SPACES.         
                                                                        
JL2003     05  ACU-SPCATU              PIC S9(08) COMP-3                
JL2003                                            VALUE ZEROS.          
JL2003     05  ACU-SPCLEA              PIC S9(08) COMP-3                
JL2003                                            VALUE ZEROS.          
                                                                        
JL2003     05  ACU-SPC-SINAL           PIC +9(08) VALUE ZEROS.          
JL2003     05  FILLER REDEFINES        ACU-SPC-SINAL.                   
JL2003      10 FILLER                  PIC X(01).                       
JL2003      10 ACU-SPC-ZD              PIC 9(08).                       
      *                                                                 
      *----------------------------------------------------------------*
      *        CAMPOS DE DATAS DO SISTEMA                               
      *----------------------------------------------------------------*
      *                                                                 
       01      WRK-DATA-SIS            PIC 9(09)   VALUE ZEROS.         
       01      WRK-DATA-SIS-R          REDEFINES   WRK-DATA-SIS.        
           05  FILLER                  PIC X(01).                       
           05  WRK-AA-SIS              PIC 9(04).                       
           05  WRK-MM-SIS              PIC 9(02).                       
           05  WRK-DD-SIS              PIC 9(02).                       
      *                                                                 
      *----------------------------------------------------------------*
      *        CAMPOS DE HORA DO SISTEMA                                
      *----------------------------------------------------------------*
      *                                                                 
       01      WRK-HORA-SIS            PIC 9(07)   VALUE ZEROS.         
       01      WRK-HORA-SIS-R          REDEFINES   WRK-HORA-SIS.        
           05  FILLER                  PIC X(01).                       
           05  WRK-HHH-SIS             PIC 9(02).                       
           05  WRK-MMM-SIS             PIC 9(02).                       
           05  WRK-SSS-SIS             PIC 9(02).                       
                                                                        
      *                                                                 
      *---------------------------------------------------------------* 
      *                            CABECALHOS                         * 
      *---------------------------------------------------------------* 
      *                                                                 
       01      CABEC01.                                                 
           05  FILLER                  PIC X(57)   VALUE ' CLLPF617 '.  
           05  FILLER                  PIC X(60)   VALUE                
           'BANCO BRADESCO S.A. '.                                      
           05  FILLER                  PIC X(07)   VALUE 'FOLHA: '.     
           05  CB1-PAG                 PIC ZZZ.ZZ9.                     
                                                                        
       01      CABEC02.                                                 
           05  FILLER                  PIC X(01)   VALUE SPACES.        
           05  CB2-DIA                 PIC 9(02)   VALUE ZEROS.         
           05  FILLER                  PIC X(01)   VALUE '/'.           
           05  CB2-MES                 PIC 9(02)   VALUE ZEROS.         
           05  FILLER                  PIC X(01)   VALUE '/'.           
           05  CB2-ANO                 PIC 9(04)   VALUE ZEROS.         
           05  FILLER                  PIC X(29)   VALUE SPACES.        
           05  FILLER                  PIC X(46)   VALUE                
           'CLIENTES NAO CADASTRADOS NO CLIE - MOVIMENTO: '.            
           05  CB2-DIA-MOV             PIC 9(02)   VALUE ZEROS.         
           05  FILLER                  PIC X(01)   VALUE '/'.           
           05  CB2-MES-MOV             PIC 9(02)   VALUE ZEROS.         
           05  FILLER                  PIC X(01)   VALUE '/'.           
           05  CB2-ANO-MOV             PIC 9(04)   VALUE ZEROS.         
           05  FILLER                  PIC X(05)   VALUE SPACES.        
           05  CB2-HH                  PIC 9(02)   VALUE ZEROS.         
           05  FILLER                  PIC X(01)   VALUE ':'.           
           05  CB2-MM                  PIC 9(02)   VALUE ZEROS.         
           05  FILLER                  PIC X(01)   VALUE ':'.           
           05  CB2-SS                  PIC 9(02)   VALUE ZEROS.         
                                                                        
       01      CABEC03.                                                 
           05  FILLER                  PIC X(03)   VALUE SPACES.        
           05  FILLER                  PIC X(18)   VALUE                
           'CPF DEVEDOR  '.                                             
           05  FILLER                  PIC X(49)   VALUE                
           'AGENCIA CART NAT CONTRATO NUM C/C  CPF AVALISTA '.          
           05  FILLER                  PIC X(24)   VALUE                
           'NOME DO DEVEDOR/AVALISTA'.                                  
                                                                        
       01      CABEC04.                                                 
           05  FILLER                  PIC X(02)   VALUE SPACES.        
           05  FILLER                  PIC X(05)   VALUE                
           '====='.                                                     
           05  FILLER                  PIC X(31)   VALUE                
           '============  ======= ==== ===='.                           
           05  FILLER                  PIC X(33)   VALUE                
           ' ======== =======  ============'.                           
           05  FILLER                  PIC X(24)   VALUE                
           '========================'.                                  
                                                                        
       01      CABEC05.                                                 
           05  FILLER                  PIC X(28)   VALUE '  CLLPF617 '. 
           05  FILLER                  PIC X(37)   VALUE                
           'BANCO   BRADESCO  S.A. '.                                   
           05  FILLER                  PIC X(07)   VALUE 'FOLHA: '.     
           05  CB5-PAG                 PIC ZZZ.ZZ9.                     
                                                                        
       01      CABEC06.                                                 
           05  FILLER                  PIC X(01)   VALUE SPACES.        
           05  CB6-DIA                 PIC 9(02)   VALUE ZEROS.         
           05  FILLER                  PIC X(01)   VALUE '/'.           
           05  CB6-MES                 PIC 9(02)   VALUE ZEROS.         
           05  FILLER                  PIC X(01)   VALUE '/'.           
           05  CB6-ANO                 PIC 9(04)   VALUE ZEROS.         
           05  FILLER                  PIC X(08)   VALUE SPACES.        
           05  FILLER                  PIC X(32)   VALUE                
           'RELATORIO DE TOTAIS - MOVIMENTO '.                          
           05  CB6-DIA-MOV             PIC 9(02)   VALUE ZEROS.         
           05  FILLER                  PIC X(01)   VALUE '/'.           
           05  CB6-MES-MOV             PIC 9(02)   VALUE ZEROS.         
           05  FILLER                  PIC X(01)   VALUE '/'.           
           05  CB6-ANO-MOV             PIC 9(04)   VALUE ZEROS.         
           05  FILLER                  PIC X(10)   VALUE SPACES.        
           05  CB6-HH                  PIC 9(02)   VALUE ZEROS.         
           05  FILLER                  PIC X(01)   VALUE ':'.           
           05  CB6-MM                  PIC 9(02)   VALUE ZEROS.         
           05  FILLER                  PIC X(01)   VALUE ':'.           
           05  CB6-SS                  PIC 9(02)   VALUE ZEROS.         
                                                                        
       01      CABEC07.                                                 
           05  FILLER                  PIC X(17)   VALUE SPACES.        
           05  FILLER                  PIC X(40)   VALUE                
           'TOTAIS DE REGISTROS INCONSISTENTES '.                       
                                                                        
       01      DET01.                                                   
           05  FILLER                  PIC X(02)   VALUE SPACES.        
           05  DET1-CPF-DEV            PIC X(09)   VALUE SPACES.        
           05  DET1-CPF-DEV-IFEM       PIC X(01)   VALUE SPACES.        
           05  DET1-CPF-FIL            PIC X(04)   VALUE SPACES.        
           05  DET1-CPF-DEV-IFEM1      PIC X(01)   VALUE SPACES.        
           05  DET1-CPF-CTR-DEV        PIC X(02)   VALUE SPACES.        
           05  DET1-CPF-CTR-DEV-R      REDEFINES   DET1-CPF-CTR-DEV.    
               10  DET1-CPFCTR-DEV     PIC 9(02).                       
           05  FILLER                  PIC X(03)   VALUE SPACES.        
           05  DET1-AGENCIA            PIC ZZZZ9.                       
           05  FILLER                  PIC X(02)   VALUE SPACES.        
BRQ141*....05..DET1-CARTEIRA...........PIC.9(03)...VALUE.ZEROS.         
BRQ141     05  DET1-CARTEIRA           PIC X(03)   VALUE ZEROS.         
           05  FILLER                  PIC X(02)   VALUE SPACES.        
           05  DET1-NATUREZA           PIC X(03)   VALUE SPACES.        
           05  FILLER                  PIC X(03)   VALUE SPACES.        
           05  DET1-CONTRATO           PIC ZZZZZZ9.                     
           05  FILLER                  PIC X(01)   VALUE SPACES.        
           05  DET1-CCTA               PIC ZZZZZZ9.                     
           05  FILLER                  PIC X(02)   VALUE SPACES.        
           05  DET1-CPF-AVA            PIC X(09)   VALUE SPACES.        
           05  DET1-CPF-AVA-IFEM       PIC X(01)   VALUE SPACES.        
           05  DET1-CPF-CTR-AVA        PIC X(02)   VALUE SPACES.        
           05  DET1-CPF-CTR-AVA-R      REDEFINES   DET1-CPF-CTR-AVA.    
               10  DET1-CPFCTR-AVA     PIC 9(02).                       
           05  FILLER                  PIC X(02)   VALUE SPACES.        
           05  DET1-NOME               PIC X(24)   VALUE SPACES.        
           05  FILLER                  PIC X(10)   VALUE SPACES.        
           05  DET1-OCORRENCIA         PIC X(20)   VALUE SPACES.        
                                                                        
       01      TOTAL01.                                                 
           05  FILLER                  PIC X(27)   VALUE SPACES.        
           05  FILLER                  PIC X(02)   VALUE '- '.          
           05  TOT1-CAMPO              PIC X(09)   VALUE SPACES.        
           05  FILLER                  PIC X(02)   VALUE '= '.          
           05  TOT1-TOTAL              PIC ZZ.ZZZ.ZZ9.                  
                                                                        
       01      TOTAL02.                                                 
           05  FILLER                  PIC X(17)   VALUE SPACES.        
           05  TOT2-CAMPO              PIC X(30)   VALUE SPACES.        
           05  FILLER                  PIC X(02)   VALUE '= '.          
           05  TOT2-TOTAL              PIC ZZ.ZZZ.ZZ9.                  
                                                                        
       01      TOTAL03.                                                 
           05  FILLER                  PIC X(36)   VALUE SPACES.        
           05  FILLER                  PIC X(02)   VALUE '- '.          
           05  TOT3-CAMPO              PIC X(09)   VALUE SPACES.        
           05  FILLER                  PIC X(02)   VALUE '= '.          
           05  TOT3-TOTAL              PIC ZZ.ZZZ.ZZ9.                  
      *                                                                 
      *----------------------------------------------------------------*
      *    CAMPOS DA POOL7600                                           
      *----------------------------------------------------------------*
      *                                                                 
       01      DATA-HORA.                                               
           05  DT-JULIANA              PIC 9(05)   COMP-3.              
           05  DT-AAMMDD               PIC 9(07)   COMP-3.              
           05  DT-AAAAMMDD             PIC 9(09)   COMP-3.              
           05  TI-HHMMSS               PIC 9(07)   COMP-3.              
           05  TI-HHMMSSMMMMMM         PIC 9(13)   COMP-3.              
           05  TIMESTAMP               PIC X(20).                       
                                                                        
      *---------------------------------------------------------------* 
      *                 CAMPOS UTILIZADOS NA POOL1285                 * 
      *---------------------------------------------------------------* 
                                                                        
       01  WRK-AREA-POOL1285.                                           
           02  WRK-DT-ENTRADA          PIC  9(08) COMP-3 VALUE ZEROS.   
           02  WRK-NUM-DIAS            PIC S9(05) COMP-3 VALUE ZEROS.   
           02  WRK-DATA-SAIDA          PIC  9(08) COMP-3 VALUE ZEROS.   
           02  WRK-MENSAGEM            PIC  X(50)        VALUE SPACES.  
                                                                        
       01  WRK-RETURN-CODE             PIC S9(05)        VALUE ZEROS.   
                                                                        
       01  WRK-MENSAGEM-AUX            PIC X(21) VALUE SPACES.          
                                                                        
      *---------------------------------------------------------------* 
      *             DEFINICAO DAS DATAS DE VENCIMENTO                 * 
      *---------------------------------------------------------------* 
                                                                        
       01  WRK-DATA-08                 PIC 9(08).                       
       01  WRK-DATA-1A-CARTA           PIC 9(08).                       
       01  WRK-DATA-21                 PIC 9(08).                       
       01  WRK-DATA-2A-CARTA           PIC 9(08).                       
       01  WRK-DATA-41                 PIC 9(08).                       
       01  WRK-DATA-3A-CARTA           PIC 9(08).                       
      *                                                                 
JL2003*----------------------------------------------------------------*
JL2003*    BOOK DE SAIDA DOS ARQUIVOS CDSPCATU E CDSPCLEA               
JL2003*----------------------------------------------------------------*
JL2003*                                                                 
        COPY 'I#CLLPC0'.                                                
                                                                        
      *                                                                 
      *----------------------------------------------------------------*
      *        DEFINICAL DA POL7100C                                    
      *----------------------------------------------------------------*
      *                                                                 
JL2003  COPY 'POL7100C'.                                                
                                                                        
           EXEC SQL                                                     
                INCLUDE SQLCA                                           
           END-EXEC.                                                    
                                                                        
           EXEC SQL                                                     
                INCLUDE CLIEV008                                        
           END-EXEC.                                                    
                                                                        
           EXEC SQL                                                     
                INCLUDE CLIEV004                                        
           END-EXEC.                                                    
                                                                        
           EXEC SQL                                                     
                INCLUDE CLIEV006                                        
           END-EXEC.                                                    
                                                                        
           EXEC SQL                                                     
                INCLUDE CLIEV011                                        
           END-EXEC.                                                    
                                                                        
           EXEC SQL                                                     
                INCLUDE CEPNV003                                        
           END-EXEC.                                                    
                                                                        
       01  FILLER                      PIC X(32)        VALUE           
           '*  FIM DA WORKING CLLPF617 *'.                              
                                                                        
       PROCEDURE DIVISION.                                              
       0000-00-INICIAR SECTION.                                         
                                                                        
           CALL    'POOL1050'.                                          
                                                                        
           CALL    'POOL7600'            USING   DATA-HORA              
                                                                        
           MOVE    DT-AAAAMMDD           TO      WRK-DATA-SIS           
           MOVE    TI-HHMMSS             TO      WRK-HORA-SIS           
                                                                        
           MOVE    WRK-DD-SIS            TO      CB2-DIA  CB2-DIA-MOV   
                                                 CB6-DIA  CB6-DIA-MOV   
           MOVE    WRK-MM-SIS            TO      CB2-MES  CB2-MES-MOV   
                                                 CB6-MES  CB6-MES-MOV   
           MOVE    WRK-AA-SIS            TO      CB2-ANO  CB2-ANO-MOV   
                                                 CB6-ANO  CB6-ANO-MOV   
                                                                        
           MOVE    WRK-HHH-SIS           TO      CB2-HH   CB6-HH        
           MOVE    WRK-MMM-SIS           TO      CB2-MM   CB6-MM        
           MOVE    WRK-SSS-SIS           TO      CB2-SS   CB6-SS        
                                                                        
                                                                        
           DISPLAY 'CLLPF617 - DATA DE HOJE = ' DT-AAAAMMDD.            
                                                                        
           OPEN    INPUT   PARMCLLP                                     
                           CADSEDIA                                     
                           ARQDATA                                      
                   OUTPUT  RELINCON                                     
                           RELCONTR                                     
                           CDSPCATU                                     
JL2003                     CDSPCLEA.                                    
                                                                        
           MOVE    WRK-ABERTURA        TO      WRK-OPERACAO             
           PERFORM 1000-00-TESTAR-FILE-STATUS                           
                                                                        
           PERFORM 666666-LER-PARMCLLP.                                 
                                                                        
           PERFORM   777777-TRATA-PARMCLLP                              
               UNTIL  WRK-FS-PARMCLLP   EQUAL   '10'.                   
                                                                        
                                                                        
           PERFORM 2500-00-CALC-1A-2A-3A-CARTA                          
                                                                        
           PERFORM 2100-00-LER-CADSEDIA                                 
                                                                        
           PERFORM 3000-00-PROCESSAR.                                   
                                                                        
       0000-10-FINALIZAR.                                               
                                                                        
           PERFORM 5300-00-CONTROLE                                     
                                                                        
           CLOSE   PARMCLLP                                             
                   CADSEDIA                                             
                   ARQDATA                                              
                   RELINCON                                             
                   RELCONTR                                             
                   CDSPCATU                                             
JL2003             CDSPCLEA.                                            
                                                                        
       0000-20-FINALIZAR.                                               
           MOVE    WRK-FECHAMENTO          TO   WRK-OPERACAO.           
           PERFORM 1000-00-TESTAR-FILE-STATUS.                          
                                                                        
           STOP    RUN.                                                 
                                                                        
       0000-99-EXIT. EXIT.                                              
      *                                                                 
      *---------------------------------------------------------------* 
      *       ROTINA DE TILE STATUSO DE TODOS OS ARQUIVOS               
      *---------------------------------------------------------------* 
      *                                                                 
       1000-00-TESTAR-FILE-STATUS SECTION.                              
                                                                        
           PERFORM 1100-00-TESTAR-FS-CADSEDIA                           
                                                                        
           PERFORM 1300-00-TESTAR-FS-CDSPCATU                           
                                                                        
JL2003     PERFORM 1350-00-TESTAR-FS-CDSPCLEA                           
                                                                        
           PERFORM 1400-00-TESTAR-FS-RELINCON                           
                                                                        
           PERFORM 1500-00-TESTAR-FS-RELCONTR.                          
                                                                        
           PERFORM 1600-00-TESTAR-FS-ARQDATA.                           
                                                                        
           PERFORM 888888-TESTAR-FS-PARMCLLP.                           
                                                                        
       1000-99-EXIT. EXIT.                                              
                                                                        
      *---------------------------------------------------------------* 
      *      TESTA FILE ESTATUS DO CADSEDIA                             
      *---------------------------------------------------------------* 
      *                                                                 
       1100-00-TESTAR-FS-CADSEDIA SECTION.                              
                                                                        
           IF    WRK-FS-CADSEDIA   NOT EQUAL '00'                       
                 DISPLAY '************** CLLPF617 **************'       
                 DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO    *'   
                 DISPLAY '*              CADSEDIA              *'       
                 DISPLAY '*         FILE STATUS =  ' WRK-FS-CADSEDIA    
                                                    '          *'       
                 DISPLAY '************** CLLPF617 **************'       
                 CALL 'ILBOABN0'       USING WRK-ABEND.                 
                                                                        
       1100-99-EXIT. EXIT.                                              
      *                                                                 
                                                                        
      *---------------------------------------------------------------* 
       1300-00-TESTAR-FS-CDSPCATU SECTION.                              
      *---------------------------------------------------------------* 
                                                                        
           IF    WRK-FS-CDSPCATU   NOT EQUAL '00'                       
                 DISPLAY '************** CLLPF617 **************'       
                 DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO    *'   
                 DISPLAY '*              CDSPCATU              *'       
                 DISPLAY '*         FILE STATUS =  ' WRK-FS-CDSPCATU    
                                                    '          *'       
                 DISPLAY '************** CLLPF617 **************'       
                 CALL 'ILBOABN0'     USING WRK-ABEND.                   
                                                                        
       1300-99-EXIT. EXIT.                                              
      *                                                                 
JL2003*---------------------------------------------------------------* 
JL2003 1350-00-TESTAR-FS-CDSPCLEA SECTION.                              
JL2003*---------------------------------------------------------------* 
JL2003                                                                  
JL2003     IF    WRK-FS-CDSPCLEA   NOT EQUAL '00'                       
JL2003           DISPLAY '************** CLLPF617 **************'       
JL2003           DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO    *'   
JL2003           DISPLAY '*              CDSPCLEA              *'       
JL2003           DISPLAY '*         FILE STATUS =  ' WRK-FS-CDSPCLEA    
JL2003                                              '          *'       
JL2003           DISPLAY '************** CLLPF617 **************'       
JL2003           STOP RUN                                               
JL2003     END-IF.                                                      
JL2003                                                                  
JL2003 1350-99-EXIT. EXIT.                                              
      *                                                                 
      *---------------------------------------------------------------* 
      *        TESTA FILE STATUS DO RELINCON                            
      *---------------------------------------------------------------* 
      *                                                                 
       1400-00-TESTAR-FS-RELINCON SECTION.                              
                                                                        
           IF    WRK-FS-RELINCON   NOT EQUAL '00'                       
                 DISPLAY '************** CLLPF617 **************'       
                 DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO    *'   
                 DISPLAY '*              RELINCON              *'       
                 DISPLAY '*         FILE STATUS =  ' WRK-FS-RELINCON    
                                                    '          *'       
                 DISPLAY '************** CLLPF617 **************'       
                 CALL 'ILBOABN0'     USING WRK-ABEND.                   
                                                                        
       1400-99-EXIT. EXIT.                                              
      *                                                                 
      *---------------------------------------------------------------* 
      *       TESTA FIULE STATUS DO RELCONTR                            
      *---------------------------------------------------------------* 
      *                                                                 
       1500-00-TESTAR-FS-RELCONTR SECTION.                              
                                                                        
           IF    WRK-FS-RELCONTR   NOT EQUAL '00'                       
                 DISPLAY '************** CLLPF617 **************'       
                 DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO    *'   
                 DISPLAY '*              RELINCON              *'       
                 DISPLAY '*         FILE STATUS =  ' WRK-FS-RELCONTR    
                                                    '          *'       
                 DISPLAY '************** CLLPF617 **************'       
                 CALL 'ILBOABN0'     USING WRK-ABEND.                   
                                                                        
       1500-99-EXIT. EXIT.                                              
                                                                        
      *---------------------------------------------------------------* 
      *      TESTA FILE ESTATUS DO ARQDATA                              
      *---------------------------------------------------------------* 
      *                                                                 
       1600-00-TESTAR-FS-ARQDATA  SECTION.                              
                                                                        
           IF    WRK-FS-ARQDATA    NOT EQUAL '00'                       
                 DISPLAY '************** CLLPF617 **************'       
                 DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO    *'   
                 DISPLAY '*              ARQDATA               *'       
                 DISPLAY '*         FILE STATUS =  ' WRK-FS-ARQDATA     
                                                    '          *'       
                 DISPLAY '************** CLLPF617 **************'       
                 CALL 'ILBOABN0'       USING WRK-ABEND.                 
                                                                        
       1600-99-EXIT. EXIT.                                              
                                                                        
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
               DISPLAY '****************  CLLPF617  *******************'
               DISPLAY '*           ABEND 1111 - FORCADO              *'
               DISPLAY '*  PROGRAMA CANCELADO - PARAMETRO INVALIDO    *'
               DISPLAY '* '  PARM-CCUSTO-1 ' / ' PARM-CODIGO-1          
                       ' / ' PARM-CCUSTO-2 ' / ' PARM-CODIGO-2          
               DISPLAY '*   AVISAR ANALISTA RESPONSAVEL DA ROTINA     *'
               DISPLAY '*****************  CLLPF617  ******************'
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
               DISPLAY '************** CLLPF617 *************'          
               DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'      
               DISPLAY '*              PARMCLLP             *'          
               DISPLAY '*         FILE STATUS =  ' WRK-FS-PARMCLLP      
                                                  '         *'          
               DISPLAY '************** CLLPF617 *************'          
               CALL  'ILBOABN0'   USING    WRK-ABEND.                   
                                                                        
      *---------------------------------------------------------------* 
       888888-99-FIM.                  EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *                                                                 
      *---------------------------------------------------------------* 
      *        LEITURA DO CADASTRO SELECIONADOS DO DIA                  
      *---------------------------------------------------------------* 
      *                                                                 
       2100-00-LER-CADSEDIA  SECTION.                                   
                                                                        
           READ CADSEDIA.                                               
                                                                        
           IF   WRK-FS-CADSEDIA        EQUAL  '10'                      
                MOVE  HIGH-VALUES      TO     WRK-CHAVE-CADSEDIA-R      
                GO                     TO     2100-99-EXIT.             
                                                                        
                                                                        
           MOVE WRK-LEITURA            TO     WRK-OPERACAO              
           PERFORM 1100-00-TESTAR-FS-CADSEDIA                           
                                                                        
           ADD 1                       TO     ACU-SPCDIA                
                                                                        
           MOVE NUM-CPF-WOR   TO    WRK-NUM-CPF                         
           MOVE FIL-CPF-WOR   TO    WRK-FIL-CPF                         
           MOVE CTR-CPF-WOR   TO    WRK-CTR-CPF                         
           IF   WRK-CPF      NOT    EQUAL  WRK-CPF-AUX                  
                MOVE WRK-CPF  TO    WRK-CPF-AUX                         
                MOVE SPACES   TO    WRK-NORMAIS                         
            IF  DAT-VENCTO-WOR LESS 19990409                            
                ADD       1         TO    ACU-DTFORALIM                 
                GO TO 2100-00-LER-CADSEDIA                              
            ELSE                                                        
              IF(DAT-VENCTO-WOR     GREATER WRK-DATA-08 AND             
                 DAT-VENCTO-WOR NOT GREATER WRK-DATA-1A-CARTA ) OR      
                (DAT-VENCTO-WOR     GREATER WRK-DATA-21 AND             
                 DAT-VENCTO-WOR NOT GREATER WRK-DATA-2A-CARTA ) OR      
                (DAT-VENCTO-WOR     GREATER WRK-DATA-41 AND             
                 DAT-VENCTO-WOR NOT GREATER WRK-DATA-3A-CARTA )         
                 MOVE 'S'      TO    WRK-NORMAIS                        
                 MOVE SPACES   TO    WRK-EXTRAS                         
                ELSE                                                    
                IF    DAT-VENCTO-WOR =    11111111                      
                      MOVE SPACES   TO    WRK-NORMAIS                   
                      MOVE 'S'      TO    WRK-EXTRAS                    
                      ADD 1         TO    ACU-DTNAOFXA                  
                      GO TO 2100-00-LER-CADSEDIA                        
                ELSE                                                    
                      MOVE SPACES   TO    WRK-NORMAIS                   
                      MOVE SPACES   TO    WRK-EXTRAS                    
                      ADD 1         TO    ACU-DTNAOFXA                  
                      GO TO 2100-00-LER-CADSEDIA                        
           ELSE                                                         
           IF   DAT-VENCTO-WOR =    11111111                            
                ADD       1         TO    ACU-DTNAOFXA                  
                GO TO 2100-00-LER-CADSEDIA                              
           ELSE                                                         
           IF   WRK-NORMAIS = 'S'                                       
                NEXT SENTENCE                                           
           ELSE                                                         
           IF   WRK-EXTRAS  = 'S'                                       
            IF  DAT-VENCTO-WOR LESS 19990409                            
                ADD       1         TO    ACU-DTFORALIM                 
                GO TO 2100-00-LER-CADSEDIA                              
            ELSE                                                        
             IF (DAT-VENCTO-WOR     GREATER WRK-DATA-08 AND             
                 DAT-VENCTO-WOR NOT GREATER WRK-DATA-1A-CARTA ) OR      
                (DAT-VENCTO-WOR     GREATER WRK-DATA-21 AND             
                 DAT-VENCTO-WOR NOT GREATER WRK-DATA-2A-CARTA ) OR      
                (DAT-VENCTO-WOR     GREATER WRK-DATA-41 AND             
                 DAT-VENCTO-WOR NOT GREATER WRK-DATA-3A-CARTA )         
                 MOVE 'S'         TO  WRK-NORMAIS                       
                 MOVE SPACES      TO  WRK-EXTRAS                        
             ELSE                                                       
             IF DAT-VENCTO-WOR  NOT GREATER WRK-DATA-41                 
                MOVE 'S'         TO  WRK-NORMAIS                        
             ELSE                                                       
                MOVE SPACES   TO    WRK-NORMAIS                         
                MOVE SPACES   TO    WRK-EXTRAS                          
                ADD  1        TO    ACU-DTNAOFXA                        
                GO TO 2100-00-LER-CADSEDIA                              
           ELSE                                                         
                ADD       1         TO    ACU-DTNAOFXA                  
                GO TO 2100-00-LER-CADSEDIA.                             
                                                                        
           MOVE NUM-CPF-WOR            TO     WRK-NUM-CADSEDIA          
           MOVE FIL-CPF-WOR            TO     WRK-FIL-CADSEDIA          
           MOVE CTR-CPF-WOR            TO     WRK-COM-CADSEDIA          
           MOVE NATUREZA-WOR           TO     WRK-NAT-CADSEDIA          
           MOVE DAT-VENCTO-WOR         TO     WRK-VENCTO-CADSEDIA       
           MOVE WRK-VENCTO-CADSEDIA    TO     WRK-DAT-CADSEDIA.         
           MOVE    AGE-OP-WOR          TO     WRK-AGOP-ATU.             
           MOVE     CC-OP-WOR          TO     WRK-CCOP-ATU.             
                                                                        
       2100-99-EXIT. EXIT.                                              
      *                                                                 
      *---------------------------------------------------------------* 
      *        LEITURA DO ARQUIVO DATA DE MOVIMENTO                     
      *---------------------------------------------------------------* 
      *                                                                 
       2200-00-LER-ARQDATA   SECTION.                                   
                                                                        
           READ ARQDATA.                                                
                                                                        
           MOVE WRK-LEITURA            TO     WRK-OPERACAO              
           PERFORM 1600-00-TESTAR-FS-ARQDATA.                           
                                                                        
       2200-99-EXIT. EXIT.                                              
      *                                                                 
      *---------------------------------------------------------------* 
      *        CALCULAR DATAS -5, -20 E -40                             
      *---------------------------------------------------------------* 
      *                                                                 
       2500-00-CALC-1A-2A-3A-CARTA SECTION.                             
                                                                        
           PERFORM 2200-00-LER-ARQDATA.                                 
                                                                        
           MOVE    DTMOVINV          TO     WRK-DT-ENTRADA              
           DISPLAY 'DATA DO MOVIMENTO = ' WRK-DT-ENTRADA                
                                                                        
                                                                        
           COMPUTE WRK-NUM-DIAS      =       WRK-DIAS1      - 1         
           COMPUTE WRK-NUM-DIAS      =     ( WRK-NUM-DIAS * -1)         
      **   MOVE    -04               TO      WRK-NUM-DIAS               
           PERFORM 200000-POOL1285                                      
           DISPLAY 'DATA DE -05 = ' WRK-DATA-SAIDA                      
           MOVE    WRK-DATA-SAIDA    TO     WRK-DATA-1A-CARTA           
                                                                        
                                                                        
           MOVE    WRK-DIAS1         TO      WRK-NUM-DIAS               
           COMPUTE WRK-NUM-DIAS      =     ( WRK-NUM-DIAS * -1)         
      **   MOVE    -05               TO     WRK-NUM-DIAS                
           PERFORM 200000-POOL1285                                      
           DISPLAY 'DATA DE -06 = ' WRK-DATA-SAIDA                      
           MOVE    WRK-DATA-SAIDA    TO     WRK-DATA-08                 
                                                                        
                                                                        
           COMPUTE WRK-NUM-DIAS      =       WRK-DIAS2      - 1         
           COMPUTE WRK-NUM-DIAS      =     ( WRK-NUM-DIAS * -1)         
      **   MOVE    -19               TO     WRK-NUM-DIAS                
           PERFORM 200000-POOL1285                                      
           DISPLAY 'DATA DE -20 = ' WRK-DATA-SAIDA                      
           MOVE   WRK-DATA-SAIDA     TO     WRK-DATA-2A-CARTA           
                                                                        
                                                                        
           MOVE    WRK-DIAS2         TO      WRK-NUM-DIAS               
           COMPUTE WRK-NUM-DIAS      =     ( WRK-NUM-DIAS * -1)         
      **   MOVE    -20               TO      WRK-NUM-DIAS               
           PERFORM 200000-POOL1285                                      
           DISPLAY 'DATA DE -21 = ' WRK-DATA-SAIDA                      
           MOVE   WRK-DATA-SAIDA     TO     WRK-DATA-21                 
                                                                        
                                                                        
           COMPUTE WRK-NUM-DIAS      =       WRK-DIAS3      - 1         
           COMPUTE WRK-NUM-DIAS      =     ( WRK-NUM-DIAS * -1)         
      **   MOVE   -39                TO     WRK-NUM-DIAS                
           PERFORM 200000-POOL1285                                      
           DISPLAY 'DATA DE -40 = ' WRK-DATA-SAIDA                      
           MOVE   WRK-DATA-SAIDA     TO     WRK-DATA-3A-CARTA.          
                                                                        
                                                                        
           MOVE    WRK-DIAS3         TO      WRK-NUM-DIAS               
           COMPUTE WRK-NUM-DIAS      =     ( WRK-NUM-DIAS * -1)         
      **   MOVE   -40                TO     WRK-NUM-DIAS                
           PERFORM 200000-POOL1285                                      
           DISPLAY 'DATA DE -41 = ' WRK-DATA-SAIDA                      
           MOVE   WRK-DATA-SAIDA     TO     WRK-DATA-41.                
                                                                        
                                                                        
                                                                        
       2500-99-EXIT. EXIT.                                              
      *                                                                 
      *---------------------------------------------------------------* 
      *   ROTINA DE PROCESSAMENTO                                       
      *---------------------------------------------------------------* 
      *                                                                 
       3000-00-PROCESSAR SECTION.                                       
                                                                        
           IF       WRK-CHAVE-CADSEDIA-R EQUAL  HIGH-VALUES             
                    GO                 TO     3000-99-EXIT.             
                                                                        
            PERFORM 3100-00-PESQUISA                                    
            PERFORM 2100-00-LER-CADSEDIA                                
            GO      TO   3000-00-PROCESSAR.                             
                                                                        
       3000-99-EXIT.  EXIT.                                             
      *                                                                 
      *----------------------------------------------------------------*
      *    ROTINA DE PESQUISAS DE TABELAS                               
      *----------------------------------------------------------------*
      *                                                                 
       3100-00-PESQUISA    SECTION.                                     
                                                                        
CPM-> *    IF      NOME-CAD-WOR          EQUAL  SPACES                  
CPM-> *            MOVE 'NOME INVALIDO  ' TO  DET1-OCORRENCIA           
CPM-> *            PERFORM 5000-00-IMPRIMIR-DEVEDOR                     
CPM-> *            GO             TO   3100-99-EXIT.                    
                                                                        
           IF   AGE-OP-WOR NOT NUMERIC OR                               
                CC-OP-WOR  NOT NUMERIC                                  
                DISPLAY 'AG/CONTA INVALIDA - CPF/CGC = '                
                       NUM-CPF-WOR '/'                                  
                       FIL-CPF-WOR '-'                                  
                       CTR-CPF-WOR                                      
                GO             TO   3100-99-EXIT.                       
                                                                        
           IF  WRK-CHAVE-AGCONTA-ATU-R  EQUAL  WRK-CHAVE-AGCONTA-ANT-R  
CPM-> *        IF  WRK-SIM-DEVEDOR     EQUAL 'S'                        
CPM-> *            MOVE  SPACES           TO  DET1-OCORRENCIA           
CPM-> *            PERFORM 5000-00-IMPRIMIR-DEVEDOR                     
CPM-> *            GO             TO   3100-99-EXIT                     
CPM-> *        ELSE                                                     
                   IF  WRK-SIM-AVALISTA  EQUAL 'S'                      
                     IF  CAD-NCGC1-WOR  EQUAL ZEROS                     
                         PERFORM 3400-00-GRAVA-SEM-AVALISTA             
                         GO             TO   3100-99-EXIT               
                     ELSE                                               
                         MOVE  SPACES   TO   DET1-OCORRENCIA            
CPM-> *                  PERFORM 5100-00-IMPRIMIR-AVALISTA              
                         PERFORM 3400-00-GRAVA-SEM-AVALISTA             
                         GO             TO   3100-99-EXIT               
                   ELSE                                                 
                       PERFORM 3500-00-GRAVA-COM-AVALISTA               
                       GO             TO   3100-99-EXIT.                
                                                                        
           MOVE  WRK-CHAVE-AGCONTA-ATU-R  TO  WRK-CHAVE-AGCONTA-ANT-R.  
           MOVE  SPACES                   TO  WRK-SIM-DEVEDOR           
                                              WRK-SIM-AVALISTA.         
                                                                        
BRQ141*....IF......CARTEIRA-WOR.......EQUAL..614.OR.615.OR.616.OR.617   
BRQ141     IF      CARTEIRA-WOR EQUAL '614' OR '615' OR '616' OR '617'  
                   MOVE  NUM-CPF-WOR      TO   WRK-CPFNUM-R             
AQUI               MOVE  WRK-CPF1         TO   WRK-AGEN-614             
                   MOVE  WRK-AGEN-CART614-R TO  CJUNC-DEPDC  OF CLIEV008
                   MOVE  WRK-CPF2-CC      TO  CCTA-CLI     OF CLIEV008  
                   PERFORM                4000-00-PESQUISA-CLIEV008     
           ELSE                                                         
                   MOVE  AGE-OP-WOR   TO  CJUNC-DEPDC  OF CLIEV008      
                   MOVE  CC-OP-WOR    TO  CCTA-CLI     OF CLIEV008      
                   PERFORM            4000-00-PESQUISA-CLIEV008.        
                                                                        
CPM-> *    IF      WRK-SIM-DEVEDOR       EQUAL 'S'                      
CPM-> *            MOVE 'CLIEV008 - DEV.' TO  DET1-OCORRENCIA           
CPM-> *            PERFORM 5000-00-IMPRIMIR-DEVEDOR                     
CPM-> *            GO             TO   3100-99-EXIT.                    
                                                                        
           MOVE    ZEROS               TO    SPC-DATA-NASCIMEN-CWOR     
           MOVE    SPACES              TO    SPC-NATURALIDADE-CWOR      
           MOVE    SPACES              TO    SPC-UF-ORIGEM-CWOR.        
           MOVE    SPACES              TO    SPC-NOME-CONJUGE-CWOR.     
                                                                        
      **   PERFORM 4100-00-PESQUISA-CLIEV006.                           
      **   IF      WRK-SIM-DEVEDOR     EQUAL 'S'                        
      **           MOVE 'CLIEV006 - DEV.' TO  DET1-OCORRENCIA           
      **           PERFORM 5000-00-IMPRIMIR-DEVEDOR                     
      **           GO             TO   3100-99-EXIT.                    
                                                                        
           IF  CCEP-NOVO OF  CLIEV008  >  1000                          
               PERFORM 4300-00-PESQUISA-CEPNV003                        
           ELSE                                                         
               MOVE  'S'          TO   WRK-SIM-DEVEDOR.                 
CPM-> *    IF      WRK-SIM-DEVEDOR     EQUAL 'S'                        
CPM-> *            MOVE 'CEPNV003 - DEV.' TO  DET1-OCORRENCIA           
CPM-> *            PERFORM 5000-00-IMPRIMIR-DEVEDOR                     
CPM-> *            GO             TO   3100-99-EXIT.                    
                                                                        
      ********** NAO IMPRIMIR CARTA PARA AVALISTA ****************      
      **   IF      CAD-NCGC1-WOR       EQUAL ZEROS                      
              MOVE 'S'       TO   WRK-SIM-AVALISTA                      
              GO             TO   3100-10-SEGUE.                        
                                                                        
           PERFORM 4400-00-PESQUISA-AVALISTA.                           
CPM-> *    IF      WRK-SIM-AVALISTA      EQUAL 'S'                      
CPM-> *            MOVE 'CLIEV004 - AVAL.' TO  DET1-OCORRENCIA          
CPM-> *            PERFORM 5100-00-IMPRIMIR-AVALISTA                    
CPM-> *            GO             TO    3100-10-SEGUE.                  
      *                                                                 
      *    MOVE    CID-CLI OF CLIEV004  TO  CID-CLI OF CLIEV008         
      *                                                                 
      *    PERFORM 4600-00-PESQUISA-CLIEV008-AVA.                       
      *    IF      WRK-SIM-AVALISTA      EQUAL 'S'                      
      *            MOVE 'CLIEV008 - AVAL.' TO  DET1-OCORRENCIA          
      *            PERFORM 5100-00-IMPRIMIR-AVALISTA                    
      *            GO             TO    3100-10-SEGUE.                  
                                                                        
           IF  CCEP  OF  CLIEV004  >  1000                              
               PERFORM 4700-00-PESQUISA-CEPNV003-AVA                    
           ELSE                                                         
               MOVE  'S'          TO   WRK-SIM-AVALISTA.                
                                                                        
CPM-> *    IF      WRK-SIM-AVALISTA      EQUAL 'S'                      
CPM-> *            MOVE 'CEPNV003 - AVAL.' TO  DET1-OCORRENCIA          
CPM-> *            PERFORM 5100-00-IMPRIMIR-AVALISTA.                   
                                                                        
       3100-10-SEGUE.                                                   
                                                                        
           IF      WRK-SIM-AVALISTA    EQUAL 'S'                        
                   PERFORM 3400-00-GRAVA-SEM-AVALISTA                   
           ELSE                                                         
                   PERFORM 3500-00-GRAVA-COM-AVALISTA.                  
                                                                        
       3100-99-EXIT.         EXIT.                                      
      *                                                                 
      *----------------------------------------------------------------*
      *     ROTINA DE GRAVACAO INCLUSAO DE CADSEDIA                     
      *----------------------------------------------------------------*
      *                                                                 
      *                                                                 
      *----------------------------------------------------------------*
      *     GRAVA ATUALIZACAO SEM AVALISTA                              
      *----------------------------------------------------------------*
      *                                                                 
       3400-00-GRAVA-SEM-AVALISTA   SECTION.                            
                                                                        
           MOVE    WRK-CHAVE-CADSEDIA  TO    CHAVE-CATU                 
           MOVE    COD-JUN-WOR         TO    SPC-EMPRESA-CATU           
           MOVE    AGE-OP-WOR          TO    SPC-AGENCIA-CATU           
           MOVE    CC-OP-WOR           TO    SPC-NUM-CC-CATU            
           MOVE    CARTEIRA-WOR        TO    SPC-CARTEIRA-CATU          
           MOVE    CONTRATO-WOR        TO    SPC-CONTRATO-CATU          
           MOVE    VAL-RESG-WOR        TO    SPC-VALOR-RESGATE-CATU     
           MOVE    VAL-ENC-VEN-WOR     TO    SPC-VALOR-VENCIDOS-CATU    
           MOVE    VAL-ENC-VIN-WOR     TO    SPC-VALOR-VINCENDOS-CATU   
           MOVE    NOME-CAD-WOR        TO    SPC-NOMERESP-CATU          
           MOVE    CAD-CGC-WOR         TO    SPC-CGCCPFRESP-CATU        
           MOVE    CAD-NOME1-WOR       TO    SPC-NOMEAVAL1-CATU         
           MOVE    CAD-CGC1-WOR        TO    SPC-CGC-CPF-AVAL1-CATU     
           MOVE    ZEROS               TO    SPC-AVA-CCEP-CATU          
                                             SPC-AVA-CCEP-COMPL-CATU    
           MOVE    SPACES              TO    SPC-AVA-ENDER-CATU         
                                             SPC-AVA-NRO-CATU           
                                             SPC-AVA-COMPL-CATU         
                                             SPC-AVA-BAIRRO-CATU        
                                             SPC-AVA-CIDADE-CATU        
                                             SPC-AVA-UF-CATU            
           MOVE    CAD-NOME2-WOR       TO    SPC-NOMEAVAL2-CATU         
           MOVE    CAD-CGC2-WOR        TO    SPC-CGC-CPF-AVAL2-CATU     
           MOVE    NATUREZA-WOR        TO    SPC-NATUREZA-OPER-CATU     
           MOVE    NUMERO-CL-WOR       TO    SPC-NUMERO-CL-CATU         
           MOVE    COD-NATUREZA-WOR    TO    SPC-COD-NATUREZA-CATU      
           MOVE    ZEROS               TO    SPC-DATA-OCORR-CATU        
                                                                        
           MOVE    SPC-DATA-NASCIMEN-CWOR    TO                         
                   SPC-DATA-NASCIMEN-CATU.                              
           MOVE    SPC-NATURALIDADE-CWOR     TO                         
                   SPC-NATURALIDADE-CATU.                               
           MOVE    SPC-UF-ORIGEM-CWOR        TO                         
                   SPC-UF-ORIGEM-CATU.                                  
           MOVE    SPC-NOME-CONJUGE-CWOR     TO                         
                   SPC-NOME-CONJUGE-CATU.                               
           MOVE    SPC-DEV-ENDER-CWOR        TO                         
                   SPC-DEV-ENDER-CATU.                                  
           MOVE    SPC-DEV-NRO-CWOR          TO                         
                   SPC-DEV-NRO-CATU.                                    
           MOVE    SPC-DEV-COMPL-CWOR        TO                         
                   SPC-DEV-COMPL-CATU.                                  
           MOVE    SPC-DEV-BAIRRO-CWOR       TO                         
                   SPC-DEV-BAIRRO-CATU.                                 
           MOVE    SPC-DEV-CIDADE-CWOR       TO                         
                   SPC-DEV-CIDADE-CATU.                                 
           MOVE    SPC-DEV-UF-CWOR           TO                         
                   SPC-DEV-UF-CATU.                                     
           MOVE    SPC-DEV-CCEP-CWOR         TO                         
                   SPC-DEV-CCEP-CATU.                                   
           MOVE    SPC-DEV-CCEP-COMPL-CWOR   TO                         
                   SPC-DEV-CCEP-COMPL-CATU.                             
           MOVE    WOR-LT                    TO SPC-LT.                 
                                                                        
JL2003     PERFORM 3600-00-GRAVA-CADASTRO-SPC.                          
                                                                        
       3400-99-EXIT.        EXIT.                                       
      *                                                                 
      *----------------------------------------------------------------*
      *     GRAVA ATUALIZACAO COM AVALISTA                              
      *----------------------------------------------------------------*
      *                                                                 
       3500-00-GRAVA-COM-AVALISTA   SECTION.                            
                                                                        
           MOVE    WRK-CHAVE-CADSEDIA  TO    CHAVE-CATU                 
           MOVE    COD-JUN-WOR         TO    SPC-EMPRESA-CATU           
           MOVE    AGE-OP-WOR          TO    SPC-AGENCIA-CATU           
           MOVE    CC-OP-WOR           TO    SPC-NUM-CC-CATU            
           MOVE    CARTEIRA-WOR        TO    SPC-CARTEIRA-CATU          
           MOVE    CONTRATO-WOR        TO    SPC-CONTRATO-CATU          
           MOVE    VAL-RESG-WOR        TO    SPC-VALOR-RESGATE-CATU     
           MOVE    VAL-ENC-VEN-WOR     TO    SPC-VALOR-VENCIDOS-CATU    
           MOVE    VAL-ENC-VIN-WOR     TO    SPC-VALOR-VINCENDOS-CATU   
           MOVE    NOME-CAD-WOR        TO    SPC-NOMERESP-CATU          
           MOVE    CAD-CGC-WOR         TO    SPC-CGCCPFRESP-CATU        
           MOVE    CAD-NOME1-WOR       TO    SPC-NOMEAVAL1-CATU         
           MOVE    CAD-NOME2-WOR       TO    SPC-NOMEAVAL2-CATU         
           MOVE    NATUREZA-WOR        TO    SPC-NATUREZA-OPER-CATU     
           MOVE    NUMERO-CL-WOR       TO    SPC-NUMERO-CL-CATU         
           MOVE    COD-NATUREZA-WOR    TO    SPC-COD-NATUREZA-CATU      
           MOVE    ZEROS               TO    SPC-DATA-OCORR-CATU        
                                                                        
           MOVE    CGC-CPF-AVAL1-CWOR  TO    SPC-CGC-CPF-AVAL1-CATU     
           MOVE    NOMEAVAL1-CWOR      TO    SPC-NOMEAVAL1-CATU         
           MOVE    CGC-CPF-AVAL2-CWOR  TO    SPC-CGC-CPF-AVAL2-CATU     
           MOVE    NOMEAVAL2-CWOR      TO    SPC-NOMEAVAL2-CATU         
                                                                        
           MOVE    SPC-DATA-NASCIMEN-CWOR    TO                         
                   SPC-DATA-NASCIMEN-CATU.                              
           MOVE    SPC-NATURALIDADE-CWOR     TO                         
                   SPC-NATURALIDADE-CATU.                               
           MOVE    SPC-UF-ORIGEM-CWOR        TO                         
                   SPC-UF-ORIGEM-CATU.                                  
           MOVE    SPC-NOME-CONJUGE-CWOR     TO                         
                   SPC-NOME-CONJUGE-CATU.                               
           MOVE    SPC-DEV-ENDER-CWOR        TO                         
                   SPC-DEV-ENDER-CATU.                                  
           MOVE    SPC-DEV-NRO-CWOR          TO                         
                   SPC-DEV-NRO-CATU.                                    
           MOVE    SPC-DEV-COMPL-CWOR        TO                         
                   SPC-DEV-COMPL-CATU.                                  
           MOVE    SPC-DEV-BAIRRO-CWOR       TO                         
                   SPC-DEV-BAIRRO-CATU.                                 
           MOVE    SPC-DEV-CIDADE-CWOR       TO                         
                   SPC-DEV-CIDADE-CATU.                                 
           MOVE    SPC-DEV-UF-CWOR           TO                         
                   SPC-DEV-UF-CATU.                                     
           MOVE    SPC-DEV-CCEP-CWOR         TO                         
                   SPC-DEV-CCEP-CATU.                                   
           MOVE    SPC-DEV-CCEP-COMPL-CWOR   TO                         
                   SPC-DEV-CCEP-COMPL-CATU.                             
           MOVE    SPC-AVA-ENDER-CWOR        TO                         
                   SPC-AVA-ENDER-CATU.                                  
           MOVE    SPC-AVA-NRO-CWOR          TO                         
                   SPC-AVA-NRO-CATU.                                    
           MOVE    SPC-AVA-COMPL-CWOR        TO                         
                   SPC-AVA-COMPL-CATU.                                  
           MOVE    SPC-AVA-BAIRRO-CWOR       TO                         
                   SPC-AVA-BAIRRO-CATU.                                 
           MOVE    SPC-AVA-CIDADE-CWOR       TO                         
                   SPC-AVA-CIDADE-CATU                                  
           MOVE    SPC-AVA-UF-CWOR           TO                         
                   SPC-AVA-UF-CATU                                      
           MOVE    SPC-AVA-CCEP-CWOR         TO                         
                   SPC-AVA-CCEP-CATU                                    
           MOVE    SPC-AVA-CCEP-COMPL-CWOR   TO                         
                   SPC-AVA-CCEP-COMPL-CATU.                             
           MOVE    WOR-LT                    TO SPC-LT.                 
                                                                        
JL2003     PERFORM 3600-00-GRAVA-CADASTRO-SPC.                          
                                                                        
       3500-99-EXIT.        EXIT.                                       
JL2003*                                                                 
JL2003*----------------------------------------------------------------*
JL2003* GRAVA SAIDA DE ACORDO COM A NATUREZA (BRADESCO OU ARRENDAMENTO)*
JL2003*----------------------------------------------------------------*
JL2003*                                                                 
JL2003 3600-00-GRAVA-CADASTRO-SPC    SECTION.                           
JL2003                                                                  
JL2003     MOVE     WRK-GRAVACAO       TO      WRK-OPERACAO             
JL2003                                                                  
JL2003     IF SPC-NATUREZA-CATU       EQUAL    'AR'                     
JL2003        WRITE    FD-CDSPCLEA     FROM   REG-CADSEATU              
JL2003        PERFORM  1350-00-TESTAR-FS-CDSPCLEA                       
JL2003        ADD      1               TO      ACU-SPCLEA               
JL2003     ELSE                                                         
JL2003        WRITE    FD-CDSPCATU     FROM   REG-CADSEATU              
JL2003        PERFORM  1300-00-TESTAR-FS-CDSPCATU                       
JL2003        ADD      1               TO      ACU-SPCATU               
JL2003     END-IF.                                                      
JL2003                                                                  
JL2003 3600-99-EXIT.        EXIT.                                       
      *                                                                 
      *----------------------------------------------------------------*
      *     PESQUISA TABELA CLIEV008                                    
      *----------------------------------------------------------------*
      *                                                                 
       4000-00-PESQUISA-CLIEV008     SECTION.                           
                                                                        
           EXEC SQL                                                     
             SELECT                                                     
                   CJUNC_DEPDC,                                         
                   CCTA_CLI,                                            
                   EBAIRO_LOGDR,                                        
                   CID_CLI,                                             
                   ELOGDR_NRO_COMPL,                                    
                   CCEP_NOVO,                                           
                   CCEP_NOVO_COMPL                                      
             INTO                                                       
                   :CLIEV008.CJUNC-DEPDC,                               
                   :CLIEV008.CCTA-CLI,                                  
                   :CLIEV008.EBAIRO-LOGDR,                              
                   :CLIEV008.CID-CLI,                                   
                   :CLIEV008.ELOGDR-NRO-COMPL,                          
                   :CLIEV008.CCEP-NOVO,                                 
                   :CLIEV008.CCEP-NOVO-COMPL                            
             FROM   DB2PRD.V01CTA_CORRENTE                              
             WHERE                                                      
                   CJUNC_DEPDC      = :CLIEV008.CJUNC-DEPDC      AND    
                   CCTA_CLI         = :CLIEV008.CCTA-CLI                
           END-EXEC.                                                    
                                                                        
           IF    ( SQLCODE   NOT  =    ZEROS AND +100 )   OR            
                 ( SQLWARN0       =    'W'            )                 
                   MOVE 'DB2'                TO   ERR-TIPO-ACESSO       
                   MOVE 'V01CTA_CORRENTE   ' TO   ERR-DBD-TAB           
                   MOVE 'SELECT  '           TO   ERR-FUN-COMANDO       
                   MOVE SQLCODE              TO   ERR-SQL-CODE          
                   MOVE 0010                 TO   ERR-LOCAL             
                   MOVE SPACES               TO   ERR-SEGM              
                   PERFORM   9000-00-ROTINA-ERRO.                       
                                                                        
           IF      SQLCODE             EQUAL    +100                    
                   MOVE  0             TO  CCEP-NOVO       OF CLIEV008  
                                           CCEP-NOVO-COMPL OF CLIEV008  
                   MOVE 'S'            TO     WRK-SIM-DEVEDOR           
                   GO                  TO     4000-99-EXIT.             
                                                                        
           MOVE    CCEP-NOVO        OF CLIEV008                         
                                       TO    SPC-DEV-CCEP-CWOR          
           MOVE    CCEP-NOVO-COMPL  OF CLIEV008                         
                                       TO    SPC-DEV-CCEP-COMPL-CWOR    
           MOVE    ELOGDR-NRO-COMPL OF CLIEV008                         
                                       TO    SPC-DEV-ENDER-CWOR         
           MOVE    SPACES              TO    SPC-DEV-NRO-CWOR           
           MOVE    SPACES              TO    SPC-DEV-COMPL-CWOR         
           MOVE    EBAIRO-LOGDR     OF CLIEV008                         
                                       TO    SPC-DEV-BAIRRO-CWOR.       
                                                                        
       4000-99-EXIT.         EXIT.                                      
      *                                                                 
      *----------------------------------------------------------------*
      *      PESQUISA  TABELA CLIEV006                                  
      *----------------------------------------------------------------*
      *                                                                 
       4100-00-PESQUISA-CLIEV006    SECTION.                            
                                                                        
             MOVE  CID-CLI OF CLIEV008  TO    CID-CLI OF CLIEV006       
                                                                        
           EXEC SQL                                                     
             SELECT                                                     
                   CID_CLI,                                             
                   DNASC,                                               
                   INAT,                                                
                   CSGL_UF_ORIGE,                                       
                   CEST_CVIL                                            
             INTO                                                       
                   :CLIEV006.CID-CLI,                                   
                   :CLIEV006.DNASC,                                     
                   :CLIEV006.INAT,                                      
                   :CLIEV006.CSGL-UF-ORIGE,                             
                   :CLIEV006.CEST-CVIL                                  
             FROM   DB2PRD.V01CLIENTE_AG_PF                             
             WHERE                                                      
                   CID_CLI            = :CLIEV006.CID-CLI               
                                                                        
           END-EXEC.                                                    
                                                                        
           IF    ( SQLCODE   NOT  =    ZEROS AND +100 )   OR            
                 ( SQLWARN0       =    'W'            )                 
                   MOVE 'DB2'                TO   ERR-TIPO-ACESSO       
                   MOVE 'V01CLIENTE_AG_PF  ' TO   ERR-DBD-TAB           
                   MOVE 'SELECT  '           TO   ERR-FUN-COMANDO       
                   MOVE SQLCODE              TO   ERR-SQL-CODE          
                   MOVE 0020                 TO   ERR-LOCAL             
                   MOVE SPACES               TO   ERR-SEGM              
                   PERFORM   9000-00-ROTINA-ERRO.                       
                                                                        
            IF     SQLCODE                  EQUAL +100                  
                   MOVE 'S'                 TO   WRK-SIM-DEVEDOR        
                   GO                       TO   4100-99-EXIT.          
                                                                        
           MOVE    DNASC            OF CLIEV006                         
                                       TO    WRK-DATA-NASCIMENTO        
           MOVE    WRK-DIA-NASCIMENTO  TO    WRK-DIA-CLIEV006           
           MOVE    WRK-MES-NASCIMENTO  TO    WRK-MES-CLIEV006           
           MOVE    WRK-ANO-NASCIMENTO  TO    WRK-ANO-CLIEV006           
           MOVE    WRK-DATA-CLIEV006   TO    SPC-DATA-NASCIMEN-CWOR     
                                                                        
           MOVE    INAT             OF CLIEV006                         
                                       TO    SPC-NATURALIDADE-CWOR      
           MOVE    CSGL-UF-ORIGE    OF CLIEV006                         
                                       TO    SPC-UF-ORIGEM-CWOR.        
                                                                        
           IF      CEST-CVIL    OF CLIEV006 EQUAL 2                     
                   PERFORM 4200-00-PESQUISA-CLIEV011                    
           ELSE                                                         
                   MOVE SPACES         TO        SPC-NOME-CONJUGE-CWOR. 
                                                                        
                                                                        
       4100-99-EXIT.         EXIT.                                      
      *                                                                 
      *----------------------------------------------------------------*
      *       PESQUISA TABELA CLIEV011                                  
      *----------------------------------------------------------------*
      *                                                                 
       4200-00-PESQUISA-CLIEV011   SECTION.                             
                                                                        
           MOVE  CID-CLI  OF  CLIEV006  TO CID-CLI OF CLIEV011          
                                                                        
           EXEC SQL                                                     
             SELECT                                                     
                   CID_CLI,                                             
                   CPARNT,                                              
                   IPSSOA_ABREV                                         
             INTO                                                       
                   :CLIEV011.CID-CLI,                                   
                   :CLIEV011.CPARNT,                                    
                   :CLIEV011.IPSSOA-ABREV                               
             FROM   DB2PRD.V01FAMILIA_CLI_PF                            
             WHERE                                                      
                   CID_CLI      = :CLIEV011.CID-CLI  AND                
                   CPARNT       = '3'                                   
           END-EXEC.                                                    
                                                                        
           IF    ( SQLCODE   NOT  =    ZEROS AND +100 )   OR            
                 ( SQLWARN0       =    'W'            )                 
                   MOVE 'DB2'                    TO   ERR-TIPO-ACESSO   
                   MOVE 'V01FAMILIA_CLI_PF '     TO   ERR-DBD-TAB       
                   MOVE 'SELECT  '               TO   ERR-FUN-COMANDO   
                   MOVE SQLCODE                  TO   ERR-SQL-CODE      
                   MOVE 0030                     TO   ERR-LOCAL         
                   MOVE SPACES                   TO   ERR-SEGM          
                   PERFORM   9000-00-ROTINA-ERRO.                       
                                                                        
           IF      SQLCODE             EQUAL     ZEROS                  
                   MOVE IPSSOA-ABREV OF CLIEV011                        
                                       TO        SPC-NOME-CONJUGE-CWOR  
           ELSE                                                         
                   MOVE SPACES         TO        SPC-NOME-CONJUGE-CWOR. 
                                                                        
       4200-99-EXIT.         EXIT.                                      
      *                                                                 
      *----------------------------------------------------------------*
      *      PESQUISA TABELA  CLIEV003                                  
      *----------------------------------------------------------------*
      *                                                                 
       4300-00-PESQUISA-CEPNV003    SECTION.                            
                                                                        
           MOVE  CCEP-NOVO       OF CLIEV008 TO  CCEP       OF CEPNV003 
           MOVE  CCEP-NOVO-COMPL OF CLIEV008 TO  CCEP-COMPL OF CEPNV003 
                                                                        
           EXEC SQL                                                     
             SELECT                                                     
                   CCEP,                                                
                   CCEP_COMPL,                                          
                   CSGL_UF,                                             
                   IMUN                                                 
             INTO                                                       
                   :CEPNV003.CCEP,                                      
                   :CEPNV003.CCEP-COMPL,                                
                   :CEPNV003.CSGL-UF,                                   
                   :CEPNV003.IMUN                                       
             FROM   DB2PRD.V01CEP                                       
             WHERE                                                      
                   CCEP               = :CEPNV003.CCEP       AND        
                   CCEP_COMPL         = :CEPNV003.CCEP-COMPL            
           END-EXEC.                                                    
                                                                        
           IF    ( SQLCODE   NOT  =    ZEROS AND +100 )   OR            
                 ( SQLWARN0       =    'W'   )                          
                   MOVE 'DB2'                    TO   ERR-TIPO-ACESSO   
                   MOVE 'V01CEP            '     TO   ERR-DBD-TAB       
                   MOVE 'SELECT  '               TO   ERR-FUN-COMANDO   
                   MOVE SQLCODE                  TO   ERR-SQL-CODE      
                   MOVE 0040                     TO   ERR-LOCAL         
                   MOVE SPACES                   TO   ERR-SEGM          
                   PERFORM   9000-00-ROTINA-ERRO.                       
                                                                        
           IF      SQLCODE             EQUAL +100                       
                   MOVE 'S'            TO    WRK-SIM-DEVEDOR            
                   GO                  TO    4300-99-EXIT.              
           MOVE    CSGL-UF          OF CEPNV003                         
                                       TO    SPC-DEV-UF-CWOR            
           MOVE    IMUN             OF CEPNV003                         
                                       TO    SPC-DEV-CIDADE-CWOR.       
                                                                        
       4300-99-EXIT.           EXIT.                                    
      *                                                                 
      *----------------------------------------------------------------*
      *    ROTINA DE AVALISTA                                           
      *----------------------------------------------------------------*
      *                                                                 
       4400-00-PESQUISA-AVALISTA    SECTION.                            
                                                                        
           MOVE    CAD-NCGC1-WOR       TO   CCGC-CPF OF CLIEV004        
           MOVE    CAD-FIL1-WOR        TO   CFLIAL-CGC OF CLIEV004      
           MOVE    CAD-CTR1-WOR        TO   CCTRL-CPF-CGC OF CLIEV004   
           PERFORM 4500-00-PESQUISA-CLIEV004.                           
                                                                        
           IF      SQLCODE             EQUAL ZEROS OR -811              
                   MOVE CAD-CGC1-WOR   TO    CGC-CPF-AVAL1-CWOR         
                   MOVE CAD-CGC2-WOR   TO    CGC-CPF-AVAL2-CWOR         
           ELSE                                                         
             IF    CAD-NCGC2-WOR   NOT EQUAL ZEROS                      
                   MOVE CAD-NCGC2-WOR  TO CCGC-CPF OF CLIEV004          
                   MOVE CAD-FIL2-WOR   TO CFLIAL-CGC OF CLIEV004        
                   MOVE CAD-CTR2-WOR   TO CCTRL-CPF-CGC OF CLIEV004     
                   PERFORM 4500-00-PESQUISA-CLIEV004                    
                   IF  SQLCODE     EQUAL ZEROS OR -811                  
                       MOVE CAD-CGC2-WOR  TO CGC-CPF-AVAL1-CWOR         
                       MOVE CAD-NOME2-WOR TO NOMEAVAL1-CWOR             
                       MOVE CAD-CGC1-WOR  TO CGC-CPF-AVAL2-CWOR         
                       MOVE CAD-NOME1-WOR TO NOMEAVAL2-CWOR             
                   ELSE                                                 
                       MOVE 'S'          TO  WRK-SIM-AVALISTA           
             ELSE                                                       
                       MOVE 'S'          TO  WRK-SIM-AVALISTA.          
                                                                        
       4400-99-EXIT.         EXIT.                                      
      *                                                                 
      *----------------------------------------------------------------*
      *    ROTINA DE PESQUISA CLIEV004                                  
      *----------------------------------------------------------------*
      *                                                                 
       4500-00-PESQUISA-CLIEV004  SECTION.                              
                                                                        
           EXEC SQL                                                     
             SELECT                                                     
                   CID_CLI,                                             
                   CCGC_CPF,                                            
                   CFLIAL_CGC,                                          
                   CCTRL_CPF_CGC,                                       
                   ELOGDR,                                              
                   ENRO_IMOV,                                           
                   ECOMPL_NRO_LOGDR,                                    
                   EBAIRO_LOGDR,                                        
                   CCEP,                                                
                   CCEP_COMPL                                           
             INTO                                                       
                   :CLIEV004.CID-CLI,                                   
                   :CLIEV004.CCGC-CPF,                                  
                   :CLIEV004.CFLIAL-CGC,                                
                   :CLIEV004.CCTRL-CPF-CGC,                             
                   :CLIEV004.ELOGDR,                                    
                   :CLIEV004.ENRO-IMOV,                                 
                   :CLIEV004.ECOMPL-NRO-LOGDR,                          
                   :CLIEV004.EBAIRO-LOGDR,                              
                   :CLIEV004.CCEP,                                      
                   :CLIEV004.CCEP-COMPL                                 
             FROM   DB2PRD.V01CLIENTE_AGENCIA                           
             WHERE                                                      
                   CCGC_CPF           = :CLIEV004.CCGC-CPF      AND     
                   CFLIAL_CGC         = :CLIEV004.CFLIAL-CGC    AND     
                   CCTRL_CPF_CGC      = :CLIEV004.CCTRL-CPF-CGC         
           END-EXEC.                                                    
                                                                        
           IF    ( SQLCODE   NOT  =    ZEROS AND +100 AND -811 )   OR   
                 ( SQLWARN0       =    'W'            )                 
                   MOVE 'DB2'                    TO   ERR-TIPO-ACESSO   
                   MOVE 'V01CLIENTE_AGENCIA'     TO   ERR-DBD-TAB       
                   MOVE 'SELECT  '               TO   ERR-FUN-COMANDO   
                   MOVE SQLCODE                  TO   ERR-SQL-CODE      
                   MOVE 0050                     TO   ERR-LOCAL         
                   MOVE SPACES                   TO   ERR-SEGM          
                   PERFORM   9000-00-ROTINA-ERRO.                       
                                                                        
           IF      SQLCODE             EQUAL    +100                    
                   MOVE 'S'            TO       WRK-SIM-AVALISTA        
                   GO                  TO       4500-99-EXIT.           
                                                                        
           MOVE    CCEP             OF CLIEV004                         
                                       TO    SPC-AVA-CCEP-CWOR          
           MOVE    CCEP-COMPL       OF CLIEV004                         
                                       TO    SPC-AVA-CCEP-COMPL-CWOR    
           MOVE    ELOGDR           OF CLIEV004                         
                                       TO    SPC-AVA-ENDER-CWOR         
           MOVE    ENRO-IMOV        OF CLIEV004                         
                                       TO    SPC-AVA-NRO-CWOR           
           MOVE    ECOMPL-NRO-LOGDR OF CLIEV004                         
                                       TO    SPC-AVA-COMPL-CWOR         
           MOVE    EBAIRO-LOGDR     OF CLIEV004                         
                                       TO    SPC-AVA-BAIRRO-CWOR.       
                                                                        
       4500-99-EXIT.      EXIT.                                         
      *                                                                 
      *----------------------------------------------------------------*
      *     PESQUISA TABELA CLIEV008                                    
      *----------------------------------------------------------------*
      *                                                                 
       4600-00-PESQUISA-CLIEV008-AVA     SECTION.                       
                                                                        
           EXEC SQL                                                     
             SELECT                                                     
                   CJUNC_DEPDC,                                         
                   CCTA_CLI,                                            
                   EBAIRO_LOGDR,                                        
                   CID_CLI,                                             
                   ELOGDR_NRO_COMPL,                                    
                   CCEP_NOVO,                                           
                   CCEP_NOVO_COMPL                                      
             INTO                                                       
                   :CLIEV008.CJUNC-DEPDC,                               
                   :CLIEV008.CCTA-CLI,                                  
                   :CLIEV008.EBAIRO-LOGDR,                              
                   :CLIEV008.CID-CLI,                                   
                   :CLIEV008.ELOGDR-NRO-COMPL,                          
                   :CLIEV008.CCEP-NOVO,                                 
                   :CLIEV008.CCEP-NOVO-COMPL                            
             FROM   DB2PRD.V01CTA_CORRENTE                              
             WHERE                                                      
                   CID_CLI          = :CLIEV008.CID-CLI                 
           END-EXEC.                                                    
                                                                        
           IF    ( SQLCODE   NOT  =    ZEROS AND +100 AND -811 )   OR   
                 ( SQLWARN0       =    'W'            )                 
                   MOVE 'DB2'                TO   ERR-TIPO-ACESSO       
                   MOVE 'V01CTA_CORRENTE   ' TO   ERR-DBD-TAB           
                   MOVE 'SELECT  '           TO   ERR-FUN-COMANDO       
                   MOVE SQLCODE              TO   ERR-SQL-CODE          
                   MOVE 0060                 TO   ERR-LOCAL             
                   MOVE SPACES               TO   ERR-SEGM              
                   PERFORM   9000-00-ROTINA-ERRO.                       
                                                                        
           IF      SQLCODE             EQUAL    +100                    
                   MOVE 'S'            TO       WRK-SIM-AVALISTA        
                   GO                  TO       4600-99-EXIT.           
                                                                        
           MOVE    CCEP-NOVO        OF CLIEV008                         
                                       TO    SPC-AVA-CCEP-CWOR          
           MOVE    CCEP-NOVO-COMPL  OF CLIEV008                         
                                       TO    SPC-AVA-CCEP-COMPL-CWOR    
           MOVE    ELOGDR-NRO-COMPL OF CLIEV008                         
                                       TO    SPC-AVA-ENDER-CWOR         
           MOVE    SPACES              TO    SPC-AVA-NRO-CWOR           
           MOVE    SPACES              TO    SPC-AVA-COMPL-CWOR         
           MOVE    EBAIRO-LOGDR     OF CLIEV008                         
                                       TO    SPC-AVA-BAIRRO-CWOR.       
                                                                        
       4600-99-EXIT.          EXIT.                                     
      *                                                                 
      *----------------------------------------------------------------*
      *      PESQUISA TABELA  CLIEV003                                  
      *----------------------------------------------------------------*
      *                                                                 
       4700-00-PESQUISA-CEPNV003-AVA    SECTION.                        
                                                                        
           MOVE  CCEP       OF CLIEV004 TO  CCEP       OF CEPNV003      
           MOVE  CCEP-COMPL OF CLIEV004 TO  CCEP-COMPL OF CEPNV003      
                                                                        
           EXEC SQL                                                     
             SELECT                                                     
                   CCEP,                                                
                   CCEP_COMPL,                                          
                   CSGL_UF,                                             
                   IMUN                                                 
             INTO                                                       
                   :CEPNV003.CCEP,                                      
                   :CEPNV003.CCEP-COMPL,                                
                   :CEPNV003.CSGL-UF,                                   
                   :CEPNV003.IMUN                                       
             FROM   DB2PRD.V01CEP                                       
             WHERE                                                      
                   CCEP               = :CEPNV003.CCEP       AND        
                   CCEP_COMPL         = :CEPNV003.CCEP-COMPL            
           END-EXEC.                                                    
                                                                        
           IF    ( SQLCODE   NOT  =    ZEROS AND +100 )   OR            
                 ( SQLWARN0       =    'W'   )                          
                   MOVE 'DB2'                    TO   ERR-TIPO-ACESSO   
                   MOVE 'V01CEP            '     TO   ERR-DBD-TAB       
                   MOVE 'SELECT  '               TO   ERR-FUN-COMANDO   
                   MOVE SQLCODE                  TO   ERR-SQL-CODE      
                   MOVE 0070                     TO   ERR-LOCAL         
                   MOVE SPACES                   TO   ERR-SEGM          
                   PERFORM   9000-00-ROTINA-ERRO.                       
                                                                        
           IF      SQLCODE             EQUAL +100                       
                   MOVE 'S'            TO    WRK-SIM-AVALISTA           
                   GO                  TO    4700-99-EXIT.              
                                                                        
           MOVE    IMUN             OF CEPNV003                         
                                       TO    SPC-AVA-CIDADE-CWOR        
           MOVE    CSGL-UF          OF CEPNV003                         
                                       TO    SPC-AVA-UF-CWOR.           
                                                                        
       4700-99-EXIT.          EXIT.                                     
      *                                                                 
CPM-> *----------------------------------------------------------------*
CPM-> *    ROTINA DE IMPRESSAO DE INCONSISTENCIA DEVEDOR                
CPM-> *----------------------------------------------------------------*
CPM-> *                                                                 
CPM-> *5000-00-IMPRIMIR-DEVEDOR  SECTION.                               
CPM-> *                                                                 
CPM-> *    IF    ACU-LIN               GREATER  55                      
CPM-> *          PERFORM 5200-00-CABECALHO.                             
CPM-> *    MOVE  NATUREZA-WOR          TO       DET1-NATUREZA           
CPM-> *    MOVE  NUM-CGC-CAD-WOR       TO       DET1-CPF-DEV            
CPM-> *    MOVE  '/'                   TO       DET1-CPF-DEV-IFEM       
CPM-> *    MOVE  FIL-CGC-CAD-WOR       TO       DET1-CPF-FIL            
CPM-> *    MOVE  '-'                   TO       DET1-CPF-DEV-IFEM1      
CPM-> *    MOVE  CONTROLE-CAD-WOR      TO       DET1-CPFCTR-DEV         
CPM-> *    MOVE  AGE-OP-WOR            TO       DET1-AGENCIA            
CPM-> *    MOVE  CARTEIRA-WOR          TO       DET1-CARTEIRA           
CPM-> *    MOVE  CONTRATO-WOR          TO       DET1-CONTRATO           
CPM-> *    MOVE  CC-OP-WOR             TO       DET1-CCTA               
CPM-> *    MOVE  SPACES                TO       DET1-CPF-AVA            
CPM-> *                                         DET1-CPF-AVA-IFEM       
CPM-> *                                         DET1-CPF-CTR-AVA        
CPM-> *    MOVE  NOME-CAD-WOR          TO       DET1-NOME               
CPM-> *                                                                 
      ***************  TESTE *******************                        
      *    IF    AGE-OP-WOR        LESS 1 OR                            
      *          AGE-OP-WOR        GREATER 20                           
      *          NEXT SENTENCE                                          
      *    ELSE                                                         
CPM-> *     WRITE REG-RELINCON  FROM DET01 AFTER 1                      
CPM-> *                                                                 
CPM-> *     MOVE  WRK-IMPRESSAO         TO       WRK-OPERACAO           
CPM-> *     PERFORM 1400-00-TESTAR-FS-RELINCON                          
CPM-> *                                                                 
CPM-> *     ADD     1                   TO       ACU-LIN.               
CPM-> *    ADD     1                   TO       ACU-DEVEDOR             
CPM-> *                                         ACU-TOTAL.              
CPM-> *                                         ACU-INCONS.             
CPM-> *5000-99-EXIT.       EXIT.                                        
CPM-> *                                                                 
CPM-> *-----------------------------------------------------------------
CPM-> *      ROTINA DE IMPRESSAO DE AVALISTA                            
CPM-> *-----------------------------------------------------------------
CPM-> *                                                                 
CPM-> *5100-00-IMPRIMIR-AVALISTA  SECTION.                              
CPM-> *                                                                 
CPM-> *    IF    ACU-LIN               GREATER  55                      
CPM-> *          PERFORM 5200-00-CABECALHO.                             
CPM-> *                                                                 
CPM-> *    MOVE  NATUREZA-WOR          TO       DET1-NATUREZA           
CPM-> *    MOVE  NUM-CGC-CAD-WOR       TO       DET1-CPF-DEV            
CPM-> *    MOVE  '/'                   TO       DET1-CPF-DEV-IFEM       
CPM-> *    MOVE  FIL-CGC-CAD-WOR       TO       DET1-CPF-FIL            
CPM-> *    MOVE  '-'                   TO       DET1-CPF-DEV-IFEM1      
CPM-> *    MOVE  CONTROLE-CAD-WOR      TO       DET1-CPFCTR-DEV         
CPM-> *    MOVE  AGE-OP-WOR            TO       DET1-AGENCIA            
CPM-> *    MOVE  CARTEIRA-WOR          TO       DET1-CARTEIRA           
CPM-> *    MOVE  CONTRATO-WOR          TO       DET1-CONTRATO           
CPM-> *    MOVE  CC-OP-WOR             TO       DET1-CCTA               
CPM-> *    MOVE  CAD-NCGC1-WOR         TO       DET1-CPF-AVA            
CPM-> *    MOVE  '/'                   TO       DET1-CPF-AVA-IFEM       
CPM-> *    MOVE  CAD-CTR1-WOR          TO       DET1-CPFCTR-AVA         
CPM-> *    MOVE  CAD-NOME1-WOR         TO       DET1-NOME               
CPM-> *                                                                 
      ***************  TESTE *******************                        
      *    IF    AGE-OP-WOR        LESS 1 OR                            
      *          AGE-OP-WOR        GREATER 20                           
      *          NEXT SENTENCE                                          
      *    ELSE                                                         
CPM-> *    WRITE REG-RELINCON  FROM DET01 AFTER 1                       
CPM-> *                                                                 
CPM-> *    MOVE  WRK-IMPRESSAO         TO       WRK-OPERACAO            
CPM-> *    PERFORM 1400-00-TESTAR-FS-RELINCON                           
CPM-> *                                                                 
CPM-> *    ADD     1                   TO       ACU-LIN.                
CPM-> *    ADD     1                   TO       ACU-AVALISTA            
CPM-> *                                         ACU-TOTAL               
CPM-> *                                         ACU-INCONS.             
CPM-> *5100-99-EXIT.         EXIT.                                      
CPM-> *                                                                 
CPM-> *----------------------------------------------------------------*
CPM-> *       ROTINA DE CABECALHO                                       
CPM-> *----------------------------------------------------------------*
CPM-> *                                                                 
CPM-> *5200-00-CABECALHO SECTION.                                       
CPM-> *                                                                 
CPM-> *    MOVE   ZEROS                 TO      ACU-LIN                 
CPM-> *    ADD    1                     TO      ACU-PAG                 
CPM-> *    MOVE   ACU-PAG               TO      CB1-PAG                 
CPM-> *                                                                 
CPM-> *    WRITE  REG-RELINCON  FROM CABEC01 AFTER PAGE                 
CPM-> *    MOVE  WRK-IMPRESSAO         TO       WRK-OPERACAO            
CPM-> *    PERFORM 1400-00-TESTAR-FS-RELINCON                           
CPM-> *                                                                 
CPM-> *    WRITE  REG-RELINCON  FROM CABEC02 AFTER 2                    
CPM-> *    MOVE  WRK-IMPRESSAO         TO       WRK-OPERACAO            
CPM-> *    PERFORM 1400-00-TESTAR-FS-RELINCON                           
CPM-> *                                                                 
CPM-> *    WRITE  REG-RELINCON  FROM CABEC03 AFTER 2                    
CPM-> *    MOVE  WRK-IMPRESSAO         TO       WRK-OPERACAO            
CPM-> *    PERFORM 1400-00-TESTAR-FS-RELINCON                           
CPM-> *                                                                 
CPM-> *    WRITE  REG-RELINCON  FROM CABEC04 AFTER 1                    
CPM-> *    MOVE  WRK-IMPRESSAO         TO       WRK-OPERACAO            
CPM-> *    PERFORM 1400-00-TESTAR-FS-RELINCON                           
CPM-> *                                                                 
CPM-> *    ADD   6                     TO        ACU-LIN.               
CPM-> *                                                                 
CPM-> *5200-99-EXIT.         EXIT.                                      
CPM-> *                                                                 
      *-----------------------------------------------------------------
      *     ROTINA DE CONTROLE                                          
      *----------------------------------------------------------------*
      *                                                                 
       5300-00-CONTROLE SECTION.                                        
                                                                        
CPM-> ******************************************************            
CPM-> *    NAO IMPRIMIR RELATORIO INCONSISTENTE                         
CPM-> ******************************************************            
CPM-> *    WRITE   REG-RELINCON  FROM  CABEC07  AFTER 3                 
CPM-> *    MOVE    WRK-IMPRESSAO       TO       WRK-OPERACAO            
CPM-> *    PERFORM 1400-00-TESTAR-FS-RELINCON                           
CPM-> *                                                                 
CPM-> *    MOVE    'DEVEDOR '          TO       TOT1-CAMPO              
CPM-> *    MOVE    ACU-DEVEDOR         TO       TOT1-TOTAL              
CPM-> *                                                                 
CPM-> *    WRITE   REG-RELINCON  FROM  TOTAL01  AFTER 2                 
CPM-> *    MOVE    WRK-IMPRESSAO       TO       WRK-OPERACAO            
CPM-> *    PERFORM 1400-00-TESTAR-FS-RELINCON                           
                                                                        
      ********** NAO IMPRIMIR CARTA PARA AVALISTA ****************      
      ***  MOVE    'AVALISTA '         TO       TOT1-CAMPO              
      ***  MOVE    ACU-AVALISTA        TO       TOT1-TOTAL              
      ***                                                               
      ***  WRITE   REG-RELINCON  FROM  TOTAL01  AFTER 2                 
      ***  MOVE    WRK-IMPRESSAO       TO       WRK-OPERACAO            
      ***  PERFORM 1400-00-TESTAR-FS-RELINCON                           
                                                                        
CPM-> *    MOVE    'TOTAL '            TO       TOT1-CAMPO              
CPM-> *    MOVE    ACU-TOTAL           TO       TOT1-TOTAL              
CPM-> *                                                                 
CPM-> *    WRITE   REG-RELINCON  FROM  TOTAL01  AFTER 2                 
CPM-> *    MOVE    WRK-IMPRESSAO       TO       WRK-OPERACAO            
CPM-> *    PERFORM 1400-00-TESTAR-FS-RELINCON                           
                                                                        
           MOVE    ZEROS               TO      ACU-LIN                  
           MOVE    1                   TO      ACU-PAG                  
           MOVE    ACU-PAG             TO      CB5-PAG                  
                                                                        
           WRITE   REG-RELCONTR  FROM CABEC05 AFTER PAGE                
           MOVE    WRK-IMPRESSAO       TO       WRK-OPERACAO            
           PERFORM 1500-00-TESTAR-FS-RELCONTR                           
                                                                        
           WRITE   REG-RELCONTR  FROM CABEC06 AFTER 2                   
           MOVE    WRK-IMPRESSAO       TO       WRK-OPERACAO            
           PERFORM 1500-00-TESTAR-FS-RELCONTR                           
                                                                        
           MOVE    'REGISTROS LIDOS - CADSPCDIA '                       
                                       TO        TOT2-CAMPO             
           MOVE    ACU-SPCDIA          TO        TOT2-TOTAL             
                                                                        
           WRITE   REG-RELCONTR  FROM TOTAL02 AFTER 3                   
           MOVE    WRK-IMPRESSAO       TO       WRK-OPERACAO            
           PERFORM 1500-00-TESTAR-FS-RELCONTR                           
                                                                        
JL2003     MOVE    'GRAVADOS BRADESCO - CADSPCAT '                      
                                       TO        TOT2-CAMPO             
                                                                        
JL2003     MOVE    ACU-SPCATU          TO        ACU-SPC-SINAL.         
JL2003     MOVE    ACU-SPC-ZD          TO        TOT2-TOTAL             
                                                                        
           WRITE   REG-RELCONTR  FROM TOTAL02 AFTER 2                   
           MOVE    WRK-IMPRESSAO       TO       WRK-OPERACAO            
           PERFORM 1500-00-TESTAR-FS-RELCONTR                           
                                                                        
JL2003     MOVE    'GRAVADOS LEASING  - CADSPLEA '                      
JL2003                                 TO        TOT2-CAMPO             
                                                                        
JL2003     MOVE    ACU-SPCLEA          TO        ACU-SPC-SINAL.         
JL2003     MOVE    ACU-SPC-ZD          TO        TOT2-TOTAL             
JL2003                                                                  
JL2003     WRITE   REG-RELCONTR  FROM TOTAL02 AFTER 2                   
JL2003     MOVE    WRK-IMPRESSAO       TO       WRK-OPERACAO            
JL2003     PERFORM 1500-00-TESTAR-FS-RELCONTR                           
                                                                        
           MOVE    'REGISTROS DATA FORA DA FAIXA'                       
                                       TO        TOT2-CAMPO             
           MOVE    ACU-DTNAOFXA        TO        TOT2-TOTAL             
                                                                        
           WRITE   REG-RELCONTR  FROM TOTAL02 AFTER 2                   
           MOVE    WRK-IMPRESSAO       TO       WRK-OPERACAO            
           PERFORM 1500-00-TESTAR-FS-RELCONTR                           
                                                                        
           MOVE    'REG.DT.VCTO.MENOR QUE LIMITE'                       
                                       TO        TOT2-CAMPO             
           MOVE    ACU-DTFORALIM       TO        TOT2-TOTAL             
                                                                        
           WRITE   REG-RELCONTR  FROM TOTAL02 AFTER 2                   
           MOVE    WRK-IMPRESSAO       TO       WRK-OPERACAO            
           PERFORM 1500-00-TESTAR-FS-RELCONTR.                          
                                                                        
CPM-> ******************************************************            
CPM-> *    NAO IMPRIMIR REGISTROS INCONSISTENTES                        
CPM-> ******************************************************            
CPM-> *    MOVE    'REGISTROS INCONSISTENTES '                          
CPM-> *                                TO        TOT2-CAMPO             
CPM-> *    MOVE    ACU-INCONS          TO        TOT2-TOTAL             
CPM-> *                                                                 
CPM-> *    WRITE   REG-RELCONTR  FROM TOTAL02 AFTER 2                   
CPM-> *    MOVE    WRK-IMPRESSAO       TO       WRK-OPERACAO            
CPM-> *    PERFORM 1500-00-TESTAR-FS-RELCONTR                           
CPM-> *                                                                 
CPM-> *    MOVE    'DEVEDOR '                                           
CPM-> *                                TO        TOT3-CAMPO             
CPM-> *    MOVE    ACU-DEVEDOR         TO        TOT3-TOTAL             
CPM-> *                                                                 
CPM-> *    WRITE   REG-RELCONTR  FROM TOTAL03 AFTER 2                   
CPM-> *    MOVE    WRK-IMPRESSAO       TO       WRK-OPERACAO            
CPM-> *    PERFORM 1500-00-TESTAR-FS-RELCONTR.                          
                                                                        
      ********** NAO IMPRIMIR CARTA PARA AVALISTA ****************      
      ***  MOVE    'AVALISTA '                                          
      ***                              TO        TOT3-CAMPO             
      ***  MOVE    ACU-AVALISTA        TO        TOT3-TOTAL             
      ***                                                               
      ***  WRITE   REG-RELCONTR  FROM TOTAL03 AFTER 2                   
      ***  MOVE    WRK-IMPRESSAO       TO       WRK-OPERACAO            
      ***  PERFORM 1500-00-TESTAR-FS-RELCONTR.                          
                                                                        
       5300-99-EXIT.         EXIT.                                      
                                                                        
      *---------------------------------------------------------------* 
       200000-POOL1285 SECTION.                                         
      *---------------------------------------------------------------* 
                                                                        
            CALL  'POOL1285' USING WRK-AREA-POOL1285                    
                                                                        
            IF  RETURN-CODE  NOT  EQUAL  ZEROS                          
                MOVE  RETURN-CODE  TO   WRK-RETURN-CODE                 
                MOVE  WRK-MENSAGEM TO   WRK-MENSAGEM-AUX                
                DISPLAY                                                 
                      '******************* CLLPF617 *******************'
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
                      '******************* CLLPF617 *******************'
                GO   TO   0000-20-FINALIZAR.                            
                                                                        
      *---------------------------------------------------------------* 
       200000-99-FIM. EXIT.                                             
      *---------------------------------------------------------------* 
      *                                                                 
      *----------------------------------------------------------------*
      *    ROTINA DE ERRO DAS TABELAS                                   
      *----------------------------------------------------------------*
      *                                                                 
       9000-00-ROTINA-ERRO SECTION.                                     
                                                                        
           MOVE 'CLLPF617'             TO   ERR-PGM.                    
                                                                        
           CALL 'POOL7100'   USING WRK-BATCH ERRO-AREA SQLCA.           
                                                                        
           GOBACK.                                                      
                                                                        
       9000-99-EXIT. EXIT.                                              
                                                                        

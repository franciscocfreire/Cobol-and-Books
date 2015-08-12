      *================================================================*
       IDENTIFICATION                  DIVISION.                        
      *================================================================*
                                                                        
       PROGRAM-ID. CLLP6620.                                            
       AUTHOR.     CPMACT 1.4.                                          
                                                                        
      *REMARKS.                                                         
      *    PROGRAMA ORIGINALMENTE CODIFICADO EM ASSEMBLER,              
      *    CONVERTIDO PARA COBOL ENTERPRISE PELO CONVERSOR:             
      *    CPMBRAXIS   "ACT 1.4 -  ASSEMBLER-COBOL TRANSLATOR".         
      *INSTALLATION.                                                    
      *DATE-WRITTEN.                                                    
      *DATE-COMPILED.                                                   
      *SECURITY.                                                        
                                                                        
      *----------------------------------------------------------------*
      *                                                                *
      *                    POOL DE PROGRAMACAO                         *
      *                                                                *
      *             CONSISTENCIA FISICA DO MOVIMENTO                   *
      *                                                                *
      *                     PROGRAMA  CLLP6620                         *
      *                                                                *
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *        ESTE PROGRAMA FOI FEITO COM BASE NO PROGRAMA CLLP5620.  *
      *----------------------------------------------------------------*
      *                                                                *
      *        PROGRAMA ........: CLLP6620                             *
      *                                                                *
      *        ANALISTA ........: IEDA             -  GP.90            *
      *        PROGRAMADOR .....: MARCELO SANTORO  -  CPM              *
      *        DATA ............: 22/06/1999                           *
      *                                                                *
      *----------------------------------------------------------------*
      *                                                                *
      *        OBJETIVO :                                              *
      *                                                                *
      *         CONSISTENCIA DO MOVIMENTO DE ENTRADA EM CL (INCLUSAO). *
      *                                                                *
      *----------------------------------------------------------------*
      *                                                                *
      *        INPUT  - CADLOCAL                                       *
      *                 CADASTRO DE ESCRITORIOS DE ADVOGADOS           *
      *               - CADPEND                                        *
      *                 CADASTRO DE PENDENCIAS                         *
      *               - MOVTO                                          *
      *                 ARQUIVO MOVIMENTO PARA ACERTOS                 *
      *                                                                *
      *        OUTPUT - ACERTOS                                        *
      *                 ARQUIVO COM MOVIMENTO  CONSISTIDO E OK         *
      *               - RELATO                                         *
      *                 RELATORIO DE INCONSISTENCIA                    *
      *                                                                *
      *----------------------------------------------------------------*
      *   R3, R4, R6, RA      - BASE                                   *
      *   R5                  - BAL                                    *
      *   R7                  - TABELAS                                *
      *   R8,R9               - TABELA DE ERROS                        *
      *                                                                *
      *----------------------------------------------------------------*
      *        ULTIMA ALTERACAO                                        *
      *----------------------------------------------------------------*
      *                                                                *
      *        ANALISTA ........: RENATO           -  CPM              *
      *        PROGRAMADOR .....: EDUARDO          -  CPM              *
      *        SUPERVISOR ......: ZULU             -  CPM              *
      *        DATA ............: 03/11/1999                           *
      *                                                                *
      *----------------------------------------------------------------*
      *                                                                *
      *        OBJETIVO :                                              *
      *                                                                *
      *              CORRECAO DA CONSISTENCIA DAS DATAS                *
      *                                                                *
      *----------------------------------------------------------------*
BRQ141******************************************************************
BRQ141* MAIO/2012 - (BRQ) - TRATAMENTO DE CARTEIRA  ALFANUMERICA        
BRQ141******************************************************************
                                                                        
      *================================================================*
       ENVIRONMENT                     DIVISION.                        
      *================================================================*
                                                                        
       CONFIGURATION                   SECTION.                         
                                                                        
       SPECIAL-NAMES.                                                   
                                                                        
           DECIMAL-POINT               IS COMMA                         
           C01                         IS CANAL1.                       
                                                                        
      *----------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.                         
      *----------------------------------------------------------------*
                                                                        
       FILE-CONTROL.                                                    
                                                                        
           SELECT ARQDATA  ASSIGN      TO UT-S-ARQDATA                  
           FILE STATUS                 IS WRK-FS-ARQDATA.               
                                                                        
           SELECT CADLOCAL ASSIGN      TO UT-S-CADLOCAL                 
           FILE STATUS                 IS WRK-FS-CADLOCAL.              
                                                                        
           SELECT CADPEND  ASSIGN      TO UT-S-CADPEND                  
           FILE STATUS                 IS WRK-FS-CADPEND.               
                                                                        
           SELECT MOVTO    ASSIGN      TO UT-S-MOVTO                    
           FILE STATUS                 IS WRK-FS-MOVTO.                 
                                                                        
           SELECT CERTO    ASSIGN      TO UT-S-CERTO                    
           FILE STATUS                 IS WRK-FS-CERTO.                 
                                                                        
           SELECT RELATO   ASSIGN      TO UT-S-RELATO                   
           FILE STATUS                 IS WRK-FS-RELATO.                
                                                                        
      *================================================================*
       DATA                            DIVISION.                        
      *================================================================*
                                                                        
       FILE                            SECTION.                         
                                                                        
      *----------------------------------------------------------------*
      *      INPUT  : ARQUIVO ARQDATA                                  *
      *               ORG.           -  LRECL = 060                    *
      *----------------------------------------------------------------*
                                                                        
       FD  ARQDATA                                                      
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  FD-REG-ARQDATA              PIC  X(060).                     
                                                                        
      *----------------------------------------------------------------*
      *      INPUT  : ARQUIVO CADLOCAL                                 *
      *               ORG.           -  LRECL = 040                    *
      *----------------------------------------------------------------*
                                                                        
       FD  CADLOCAL                                                     
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  FD-REG-CADLOCAL             PIC  X(040).                     
                                                                        
      *----------------------------------------------------------------*
      *      INPUT  : ARQUIVO CADPEND                                  *
      *               ORG.           -  LRECL = 168                    *
      *----------------------------------------------------------------*
                                                                        
       FD  CADPEND                                                      
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  FD-REG-CADPEND              PIC  X(168).                     
                                                                        
      *----------------------------------------------------------------*
      *      INPUT : ARQUIVO MOVTO                                     *
      *               ORG.           -  LRECL = 575                    *
      *----------------------------------------------------------------*
                                                                        
       FD  MOVTO                                                        
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  FD-REG-MOVTO                PIC  X(575).                     
                                                                        
      *----------------------------------------------------------------*
      *      OUTPUT : ARQUIVO CERTO                                    *
      *               ORG.           -  LRECL = 575                    *
      *----------------------------------------------------------------*
                                                                        
       FD  CERTO                                                        
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  FD-REG-CERTO                PIC  X(575).                     
                                                                        
      *----------------------------------------------------------------*
      *      OUTPUT : ARQUIVO RELATO                                   *
      *               ORG.           -  LRECL = 133                    *
      *----------------------------------------------------------------*
                                                                        
       FD  RELATO                                                       
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  FD-REG-RELATO               PIC  X(133).                     
                                                                        
       WORKING-STORAGE                 SECTION.                         
                                                                        
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE        
           'INICIO DA WORKING STORAGE SECTION '.                        
      *----------------------------------------------------------------*
                                                                        
       01  WRK-DATA05                  PIC  X(001)         VALUE SPACES.
                                                                        
       01  WRK-DATA06.                                                  
           05 WRK-DATA07               PIC  X(002)         VALUE SPACES.
           05 WRK-DATA08               PIC  X(002)         VALUE SPACES.
           05 WRK-DATA09               PIC  X(004)         VALUE SPACES.
CPMCAC 01  WRK-DATA06-R                REDEFINES WRK-DATA06             
CPMCAC                                 PIC  9(008).                     
                                                                        
       01  WRK-LOCAL01                 PIC  X(002)         VALUE SPACES.
       01  WRK-LOCAL05                 PIC  X(001)         VALUE SPACES.
                                                                        
       01  WRK-PADRAO1                 PIC BZZ.ZZ9         VALUE SPACES.
CPMCAC 01  WRK-PADRAO1-R               REDEFINES WRK-PADRAO1            
CPMCAC                                 PIC  X(007).                     
      *     ' ZZ.ZZ9'                                                   
                                                                        
       01  WRK-PADRAO2                 PIC BZ.ZZZ.ZZ9      VALUE SPACES.
CPMCAC 01  WRK-PADRAO2-R               REDEFINES WRK-PADRAO2            
CPMCAC                                 PIC  X(010).                     
      *     ' Z.ZZZ.ZZ9'                                                
                                                                        
      *----------------------------------------------------------------*
      *              DEFINICAO DO ARQUIVO  CERTO                       *
      *             PARA ENTRADA NO PGM LPCL1001                       *
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *  ++ INC I#CLLPG0 (ATUAL)     LPAF050A  (ANTERIOR)    11.05.1999*
      *                                                                *
      *  AREA DE REGISTRO MOVIMENTO  PARA ENTRADA NO PGM LPCL1001      *
      *                                                                *
      *----------------------------------------------------------------*
                                                                        
       01  WRK-ACERAUX.                                                 
      *             001  575         * REG CONSISTITDO                  
           05 WRK-ACCHAVE.                                              
      *             001  027         *                                  
              10 WRK-ACTIPO            PIC  X(001)         VALUE SPACES.
      *             001  001         *                                  
              10 WRK-ACFIRMA           PIC  X(005)         VALUE SPACES.
      *             002  006         *                                  
              10 WRK-ACAGENC           PIC  X(005)         VALUE SPACES.
      *             007  011         *                                  
              10 WRK-ACNROCL.                                           
      *             012  027         * NRO DO LP/CL                     
                 15 WRK-ACNUMER        PIC  X(012)         VALUE SPACES.
      *             012  023         *                                  
                 15 WRK-ACPARC         PIC  X(003)         VALUE SPACES.
      *             024  026         * NRO DA PARCELA                   
                 15 WRK-ACDIDAC        PIC  X(001)         VALUE SPACES.
      *             027  027         *                                  
           05 WRK-ACRAZCL              PIC  X(005)         VALUE SPACES.
      *             028  032         *                                  
           05 WRK-ACCTACL              PIC  X(007)         VALUE SPACES.
      *             033  039         *                                  
           05 WRK-ACRAZOR              PIC  X(005)         VALUE SPACES.
      *             040  044         *                                  
           05 WRK-ACCTAOR              PIC  X(007)         VALUE SPACES.
      *             045  051         *                                  
           05 WRK-ACCORRE              PIC  X(007)         VALUE SPACES.
      *             052  058         * NRO CONTA CORRENTE               
           05 WRK-ACVENCI              PIC  X(008)         VALUE SPACES.
      *             059  066         * DATA VENCOT                      
           05 WRK-ACENTCL              PIC  X(008)         VALUE SPACES.
      *             067  074         * DATA ENTRADA CL                  
           05 WRK-ACBAICL              PIC  X(008)         VALUE SPACES.
      *             075  082         * DATA BAIXA CL                    
           05 WRK-ACINICI              PIC  X(008)         VALUE SPACES.
      *             083  090         * DATA INICIO OPERACAO             
           05 WRK-ACPROTE              PIC  X(008)         VALUE SPACES.
      *             091  098         * DATA DE PROTESTO                 
           05 WRK-ACENTLP              PIC  X(008)         VALUE SPACES.
      *             099  106         * DATA ENTRADA LP                  
           05 WRK-ACBAILP              PIC  X(008)         VALUE SPACES.
      *             107  114         * DATA BAIXA LP                    
           05 WRK-ACMOVTO              PIC  X(008)         VALUE SPACES.
      *             115  122         * DATA DE MOVIMENTO                
           05 WRK-ACVTOTRN             PIC  X(008)         VALUE SPACES.
      *             123  129         * DATA VCTO.P/ EFEITO TRANS        
           05 WRK-ACSIGBA              PIC  X(004)         VALUE SPACES.
      *             131  134         * SIGLA DA NATUREZA OPER.          
           05 WRK-ACOPANO              PIC  X(003)         VALUE SPACES.
      *             135  137         * CARTEIRA CL DA NAT. OPER.        
           05 WRK-ACOPNUM              PIC  X(007)         VALUE SPACES.
      *             138  144         * NRO. NATUREZA DA OPER.           
           05 WRK-ACVALOP              PIC  X(015)         VALUE SPACES.
      *             145  159         * VALOR DA OPERACAO                
           05 WRK-ACVALR               PIC  X(015)         VALUE SPACES.
      *             160  174         * VALOR CONTABIL                   
           05 WRK-ACMOEDA              PIC  X(002)         VALUE SPACES.
      *             175  176         *                                  
           05 WRK-ACLOCAL              PIC  X(002)         VALUE SPACES.
      *             177  178         *                                  
           05 WRK-ACIDCON              PIC  X(001)         VALUE SPACES.
      *             179  179         * IDENTIFICACAO CONTABIL           
           05 WRK-ACTXPUN              PIC  X(001)         VALUE SPACES.
      *             180  180         * TAXA PUNITIVA                    
           05 WRK-ACICLLP              PIC  X(002)         VALUE SPACES.
      *             181  182         * ID. CL/LP                        
           05 WRK-ACNDEVE              PIC  X(040)         VALUE SPACES.
      *             183  222         * NOME DO DEVEDOR                  
           05 WRK-ACCGC                PIC  X(015)         VALUE SPACES.
      *             223  237         * CGC/CPF DO DEVEDOR               
           05 WRK-ACCOMP1              PIC  X(001)         VALUE SPACES.
      *             238  238         * COMPENSA                         
           05 WRK-ACNAVA1              PIC  X(040)         VALUE SPACES.
      *             239  278         * NOME 1. AVALISTA                 
           05 WRK-ACCGC1               PIC  X(015)         VALUE SPACES.
      *             279  293         * CGC/CPF 1. AVALISTA              
           05 WRK-ACNAVA2              PIC  X(040)         VALUE SPACES.
      *             294  333         * NOME DO 2. AVALISTA              
           05 WRK-ACCGC2               PIC  X(015)         VALUE SPACES.
      *             334  348         * CGC/CPF 2. AVALISTA              
           05 WRK-ACVRBASE             PIC  X(015)         VALUE SPACES.
      *             349  363         * VALOR BASE PARA CALCULO          
           05 WRK-ACDTREC              PIC  X(008)         VALUE SPACES.
      *             364  371         * DT. REC. EXPURGO                 
           05 WRK-ACSUBSTA             PIC  X(001)         VALUE SPACES.
      *             372  372         *                                  
           05 WRK-ACCPFADV             PIC  X(009)         VALUE SPACES.
      *             373  381         * NRO CPF DO ADVOGADO              
           05 WRK-ACCTRADV             PIC  X(002)         VALUE SPACES.
      *             382  383         * CTR CPF DO ADVOGADO              
           05 WRK-ACCART               PIC  X(003)         VALUE SPACES.
      *             384  386         * CARTEIRA ORIGEM                  
           05 WRK-ACTPGARA             PIC  X(002)         VALUE SPACES.
      *             387  388         * TIPO GARANTIA                    
           05 WRK-ACIDENT              PIC  X(004)         VALUE SPACES.
      *             389  392         * TIPO PENDENCIA                   
           05 WRK-ACMARCA              PIC  X(001)         VALUE SPACES.
      *             393  393         *                                  
           05 WRK-ACCODG.                                               
      *             394  400         * CODIGO DO GERENTE                
              10 WRK-ACSTREAT          PIC  X(001)         VALUE SPACES.
      *             394  394         * STATUS REATIV.CONTRATO           
              10 WRK-ACAGRESP          PIC  X(004)         VALUE SPACES.
      *             395  398         * AGENCIA PARA RESP                
              10 WRK-ACFILL            PIC  X(001)         VALUE SPACES.
      *             399  399         * FILLER                           
              10 WRK-ACCOMPBX          PIC  X(001)         VALUE SPACES.
      *             400  400         * COMPLEMENTO TIPO DE BAIXA        
           05 WRK-ACVRBIOF             PIC  X(015)         VALUE SPACES.
      *             401  415         * VR BASE CACL IOF COMPL           
           05 WRK-ACGARA               PIC  X(032)         VALUE SPACES.
      *             416  447         * GARANTIA                         
           05 WRK-ACCODACE             PIC  X(001)         VALUE SPACES.
      *             448  448         * COD. ACERTO                      
           05 WRK-ACVREVE              PIC  X(015)         VALUE SPACES.
      *             449  463         * VR. ENCARGOS VENCIDOS            
           05 WRK-ACVREVI              PIC  X(015)         VALUE SPACES.
      *             464  478         * VR. ENCARGOS VINCENDOS           
           05 WRK-ACVRBXP              PIC  X(015)         VALUE SPACES.
      *             479  493         * VR. BAIXA                        
           05 WRK-ACVRINIC             PIC  X(015)         VALUE SPACES.
      *             494  508         * VR. DEVEDOR INICIAL              
           05 WRK-ACVRIOF              PIC  X(015)         VALUE SPACES.
      *             509  523         * VR. IOF                          
           05 WRK-ACVRDEB              PIC  X(015)         VALUE SPACES.
      *             524  538         * VR. DEBITO C/C                   
           05 WRK-ACVRCOBR             PIC  X(015)         VALUE SPACES.
      *             539  553         * VR. COBRANCA                     
           05 WRK-ACCDOCOR             PIC  X(002)         VALUE SPACES.
      *             554  555         * COD. ULTIMA OCORRENCIA           
           05 WRK-ACBQTRAN             PIC  X(001)         VALUE SPACES.
      *             556  556         * BLOQUEIO P/ TRANFERENCIA         
           05 WRK-ACDTPGTO             PIC  X(008)         VALUE SPACES.
      *             557  564         * DATA DE PAGAMENTO                
           05 WRK-ACERTOS              PIC  X(001)         VALUE SPACES.
      *             565  565         * ACERTO                           
           05 WRK-ACDEDA               PIC  X(002)         VALUE SPACES.
      *             566  567         * IDENT.ORIGEM DA BAIXA            
      *                              *       DA = CONSULTA              
      *                              *       DE = MANUAL/ONLINE         
           05 WRK-ACPRELP              PIC  X(001)         VALUE SPACES.
      *             568  568         * NO REG TRANSF PARA LP            
      *                              *    - S  (CL COM MARCA P )        
      *                              *    - T  (CL COM MARCA Q )        
           05 WRK-ACDEBCC              PIC  X(001)         VALUE SPACES.
      *             569  569         * DEBITO EM C/C                    
      *                              *    - S  (COM DEBITO C/C)         
      *                              *    - N  (SEM DEBITO C/C)         
           05 WRK-ACORIGEM             PIC  X(002)         VALUE SPACES.
      *             570  571         * ORIGEM DO REGISTRO               
           05 WRK-SOL8273              PIC  X(004)         VALUE SPACES.
      *             572  575         *                                  
                                                                        
      *----------------------------------------------------------------*
      *              DEFINICAO DO ARQUIVO  *CADPEND*                   *
      *            INPUT  LRECL=168    DDNAME=CADPEND                  *
      *----------------------------------------------------------------*
                                                                        
       01  WRK-MESREGIS.                                                
           05 WRK-MESEMPRE             PIC S9(005) COMP-3  VALUE ZEROS. 
           05 WRK-MESEMPRE-R           REDEFINES           WRK-MESEMPRE 
                                       PIC  X(003).                     
      *           EMPRESA                                               
           05 WRK-MESTPPEN             PIC S9(005) COMP-3  VALUE ZEROS. 
           05 WRK-MESTPPEN-R           REDEFINES           WRK-MESTPPEN 
                                       PIC  X(003).                     
                                                                        
      *           TIPO PENDENCIA                                        
           05 WRK-MESCARTE             PIC  X(003)         VALUE SPACES.
      *           CARTEIRA                                              
           05 WRK-MESSIGLA             PIC  X(004)         VALUE SPACES.
      *           SIGLA                                                 
           05 FILLER                   PIC  X(031)         VALUE SPACES.
           05 WRK-MESENTRA.                                             
      *           ENTRADAS                                              
              10 WRK-MESENDRZ          PIC S9(005) COMP-3  VALUE ZEROS. 
      *              DEBITO - RAZAO                                     
              10 WRK-MESENDCT          PIC S9(007) COMP-3  VALUE ZEROS. 
      *              DEBITO - CONTA                                     
              10 WRK-MESENCRZ          PIC S9(005) COMP-3  VALUE ZEROS. 
      *              CREDITO- RAZAO                                     
              10 WRK-MESENCCT          PIC S9(007) COMP-3  VALUE ZEROS. 
      *              CREDITO- CONTA                                     
           05 WRK-MESCONTA             PIC  X(001)         VALUE SPACES.
      *           CONTAB/ENTRADA                                        
           05 WRK-MESAGENT             PIC S9(005) COMP-3  VALUE ZEROS. 
           05 WRK-MESAGENT-R           REDEFINES           WRK-MESAGENT 
                                       PIC  X(003).                     
      *           AGENCIA ENTRADA                                       
           05 WRK-MESRESTR             PIC  X(002)         VALUE SPACES.
      *           RESTR                                                 
CPMCAC     05 WRK-MESBXCL.                                              
      *           BAIXA DE CL                                           
              10 WRK-MESCLDRZ          PIC S9(005) COMP-3  VALUE ZEROS. 
      *              DEBITO - RAZAO                                     
              10 WRK-MESCLDCT          PIC S9(007) COMP-3  VALUE ZEROS. 
      *              DEBITO - CONTA                                     
              10 WRK-MESCLCRZ          PIC S9(005) COMP-3  VALUE ZEROS. 
      *              CREDITO- RAZAO                                     
              10 WRK-MESCLCCT          PIC S9(007) COMP-3  VALUE ZEROS. 
      *              CREDITO- CONTA                                     
CPMCAC     05 WRK-MESBXLP.                                              
      *           BAIXA DE LP                                           
              10 WRK-MES1PDRZ          PIC S9(005) COMP-3  VALUE ZEROS. 
      *              DEBITO - RAZAO                                     
              10 WRK-MESLPDCT          PIC S9(007) COMP-3  VALUE ZEROS. 
      *              DEBITO - CONTA                                     
              10 WRK-MESLPCPZ          PIC S9(005) COMP-3  VALUE ZEROS. 
      *              CREDITO- RAZAO                                     
              10 WRK-MESLPCCT          PIC S9(007) COMP-3  VALUE ZEROS. 
      *              CREDITO- CONTA                                     
CPMCAC     05 WRK-MESBXRE.                                              
      *           BAIXA RENDAS A APROPRIAR                              
              10 WRK-MESREDRZ          PIC S9(005) COMP-3  VALUE ZEROS. 
      *              DEBITO - RAZAO                                     
              10 WRK-MESREDCT          PIC S9(007) COMP-3  VALUE ZEROS. 
      *              DEBITO - CONTA                                     
              10 WRK-MESRECRZ          PIC S9(005) COMP-3  VALUE ZEROS. 
      *              CREDITO- RAZAO                                     
              10 WRK-MESRECCT          PIC S9(007) COMP-3  VALUE ZEROS. 
      *              CREDITO- CONTA                                     
CPMCAC     05 WRK-MESRCLP.                                              
      *           RECEITA EM LP                                         
              10 WRK-MESRCCRZ          PIC S9(005) COMP-3  VALUE ZEROS. 
      *              CREDITO - RAZAO                                    
              10 WRK-MESRCCCT          PIC S9(007) COMP-3  VALUE ZEROS. 
      *              CREDITO - CONTA                                    
           05 WRK-MESAGBAI             PIC S9(005) COMP-3  VALUE ZEROS. 
      *           AGENCIA BAIXA                                         
           05 FILLER                   PIC  X(052)         VALUE SPACES.
                                                                        
      *----------------------------------------------------------------*
      *                   DEFENICAO   DO ARQUIVO MOVTO                 *
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *  ++ INC I#CLLPM8                                               *
      *                                                                *
      *  AREA DE REGISTRO MOVIMENTO  PARA SAIDA DO PGM CLLP5600        *
      *                                                                *
      *----------------------------------------------------------------*
                                                                        
       01  WRK-REGMOVTO.                                                
      *          001  575                                               
           05 WRK-ACCHAVEX.                                             
      *          001  027         *                                     
              10 WRK-ACTIPOX           PIC  X(001)         VALUE '1'.   
      *          001  001         * TIPO DE REGISTRO                    
              10 WRK-FIRMA             PIC  X(005)         VALUE        
                 '00000'.                                               
CPMCAC        10 WRK-FIRMA-R           REDEFINES WRK-FIRMA              
CPMCAC                                 PIC  9(005).                     
      *          002  006         * ENPRESA                             
              10 WRK-AGENCIA           PIC  X(005)         VALUE        
                 '00000'.                                               
CPMCAC        10 WRK-AGENCIA-R         REDEFINES WRK-AGENCIA            
CPMCAC                                 PIC  9(005).                     
      *          007  011         * AGENCIA                             
              10 WRK-NUMCL             PIC  X(015)         VALUE        
                 '000000000000000'.                                     
      *          012  027         * NRO DO LP/CL                        
              10 WRK-DIGCL             PIC  X(001)         VALUE SPACES.
      *          027  027         * DIGITO DO NUM.DO CL                 
           05 WRK-CONT.                                                 
      *          028  051         * CONT. CL - CONT. ORIGEM             
              10 WRK-RAZCL             PIC  X(005)         VALUE        
                 '00000'.                                               
CPMCAC        10 WRK-RAZCL-R           REDEFINES WRK-RAZCL              
CPMCAC                                 PIC  9(005).                     
      *          028  032         * RAZAO - CL                          
              10 WRK-CTACL             PIC  X(007)         VALUE        
                 '0000000'.                                             
CPMCAC        10 WRK-CTACL-R           REDEFINES WRK-CTACL              
CPMCAC                                 PIC  9(007).                     
      *          033  039         * CONTA - CL                          
              10 WRK-RAZAZOR           PIC  X(005)         VALUE        
                 '00000'.                                               
CPMCAC        10 WRK-RAZAZOR-R         REDEFINES WRK-RAZAZOR            
CPMCAC                                 PIC  9(005).                     
      *          040  044         * RAZAO - ORIGEM                      
              10 WRK-CTAOR             PIC  X(007)         VALUE        
                 '0000000'.                                             
CPMCAC        10 WRK-CTAOR-R           REDEFINES WRK-CTAOR              
CPMCAC                                 PIC  9(007).                     
      *          045  051         * CONTA - ORIGEM                      
           05 WRK-CONTA                PIC  X(007)         VALUE        
              '0000000'.                                                
CPMCAC     05 WRK-CONTA-R              REDEFINES WRK-CONTA              
CPMCAC                                 PIC  9(007).                     
      *          052  058         * NRO CONTA CORRENTE                  
           05 WRK-DATAS.                                                
      *          059  122         * DATAS FORMATO DDMMAAAA              
              10 WRK-VENCIM            PIC  X(008)         VALUE        
                 '00000000'.                                            
      *          059  066         * VENCIMENTO                          
              10 WRK-ENTRADA           PIC  X(008)         VALUE        
                 '00000000'.                                            
      *          067  074         * ENTRADA EM CL                       
              10 WRK-BAIXACL           PIC  X(008)         VALUE        
                 '00000000'.                                            
      *          075  082         * BAIXA DE CL                         
CPMCAC        10 WRK-OPERACAOO         PIC  X(008)         VALUE        
                 '00000000'.                                            
      *          083  090         * INICIO DE OPERACAO                  
              10 WRK-AJUIZAM           PIC  X(008)         VALUE        
                 '00000000'.                                            
      *          091  098         * AJUIZAMENTO                         
              10 WRK-ENTRALP           PIC  X(008)         VALUE        
                 '00000000'.                                            
      *          099  106         * ENTRADA EM LP                       
              10 WRK-BAIXALP           PIC  X(008)         VALUE        
                 '00000000'.                                            
      *          107  114         * BAIXA DE LP                         
              10 WRK-DTAMOVTO          PIC  X(008)         VALUE        
                 '00000000'.                                            
      *          115  122         * MOVIMENTO                           
           05 WRK-DTTRANS              PIC  X(008)         VALUE        
              '00000000'.                                               
      *          123  129         * DT VCTO.P/ EFEITO TR                
           05 WRK-SIGLA                PIC  X(004)         VALUE SPACES.
      *          131  134         * SIGLA DA NATUREZA OP.               
           05 WRK-ANOOP                PIC  X(003)         VALUE '000'. 
CPMCAC     05 WRK-ANOOP-R              REDEFINES WRK-ANOOP              
CPMCAC                                 PIC  9(003).                     
      *          135  137         * CART CL DA NAT. OPER.               
           05 WRK-NUMOP                PIC  X(007)         VALUE        
              '0000000'.                                                
CPMCAC     05 WRK-NUMOP-R              REDEFINES WRK-NUMOP              
CPMCAC                                 PIC  9(007).                     
      *          138  144         * NRO. NATUREZA DA OPER.              
           05 WRK-VALOP                PIC  X(015)         VALUE        
              '000000000000000'.                                        
CPMCAC     05 WRK-VALOP-R              REDEFINES WRK-VALOP              
CPMCAC                                 PIC  9(015).                     
      *          145  159         * VALOR DA OPERACAO                   
           05 WRK-VALRES               PIC  X(015)         VALUE        
              '000000000000000'.                                        
CPMCAC     05 WRK-VALRES-R             REDEFINES WRK-VALRES             
CPMCAC                                 PIC  9(015).                     
      *          160  174         * VALOR CONTABIL                      
           05 WRK-MOEDA                PIC  X(002)         VALUE SPACES.
      *          175  176         * TIPO DE MOEDA                       
           05 WRK-LOCAL                PIC  X(002)         VALUE 'FO'.  
      *          177  178         * LOCAL                               
           05 WRK-CONTAB               PIC  X(001)         VALUE SPACES.
      *          179  179         * IDENTIFICACAO CONTABIL              
           05 WRK-TPUNIT               PIC  X(001)         VALUE SPACES.
      *          180  180         * TAXA PUNITIVA                       
           05 WRK-ID                   PIC  X(002)         VALUE 'CL'.  
      *          181  182         * ID. CL/LP                           
           05 WRK-NOMED                PIC  X(040)         VALUE SPACES.
      *          183  222         * NOME DO DEVEDOR                     
           05 WRK-CGC                  PIC  X(015)         VALUE SPACES.
      *          223  237         * CGC/CPF DO DEVEDOR                  
           05 WRK-COMP1                PIC  X(001)         VALUE SPACES.
      *          238  238         * COMPENSA                            
           05 WRK-COOB0                PIC  X(040)         VALUE SPACES.
      *          239  278         * NOME 1. AVALISTA                    
           05 WRK-CGC1                 PIC  X(015)         VALUE        
              '000000000000000'.                                        
      *          279  293         * CGC/CPF 1. AVALISTA                 
           05 WRK-COOB1                PIC  X(040)         VALUE SPACES.
      *          294  333         * NOME DO 2. AVALISTA                 
           05 WRK-CGC2                 PIC  X(015)         VALUE        
              '000000000000000'.                                        
      *          334  348         * CGC/CPF 2. AVALISTA                 
           05 WRK-VRBASE               PIC  X(015)         VALUE        
              '000000000000000'.                                        
CPMCAC     05 WRK-VRBASE-R             REDEFINES WRK-VRBASE             
CPMCAC                                 PIC  9(015).                     
      *          349  363         * VALOR BASE P/ CALCULO               
           05 WRK-DTRECEXP             PIC  X(008)         VALUE        
              '00000000'.                                               
      *          364  371         * DT. REC. EXPURGO                    
           05 WRK-SUBSTA               PIC  X(001)         VALUE SPACES.
      *          372  372         * SUBSTABELECIDO                      
           05 WRK-CPFADV               PIC  X(009)         VALUE        
              '000000000'.                                              
      *          373  381         * NRO CPF DO ADVOGADO                 
           05 WRK-CTRADV               PIC  X(002)         VALUE '00'.  
      *          382  383         * CTR CPF DO ADVOGADO                 
           05 WRK-CART                 PIC  X(003)         VALUE '000'. 
      *          384  386         * CARTEIRA ORIGEM                     
           05 WRK-TIPOGAR              PIC  X(002)         VALUE '00'.  
      *          387  388         * TIPO GARANTIA                       
           05 WRK-IDENT                PIC  X(004)         VALUE '0000'.
CPMCAC     05 WRK-IDENT-R              REDEFINES WRK-IDENT              
CPMCAC                                 PIC  9(004).                     
      *          389  392         * TIPO PENDENCIA                      
           05 WRK-MARCA                PIC  X(001)         VALUE SPACES.
      *          393  393         * MARCA (SERASA/CARTA)                
           05 WRK-STAREAT              PIC  X(001)         VALUE SPACES.
      *          394  394         * STATUS REATIV.CONTRATO              
           05 WRK-AGRESP               PIC  X(004)         VALUE '0000'.
CPMCAC     05 WRK-AGRESP-R             REDEFINES WRK-AGRESP             
CPMCAC                                 PIC  9(004).                     
      *          395  398         * AGENCIA PARA RESP                   
           05 FILLER                   PIC  X(002)         VALUE SPACES.
      *          399  400         * RESERVA                             
           05 WRK-VRBIOF               PIC  X(015)         VALUE        
              '000000000000000'.                                        
      *          401  415         * VR BASE CACL IOF COMPL              
           05 WRK-GARANTIA             PIC  X(032)         VALUE SPACES.
      *          416  447         * GARANTIA                            
           05 WRK-CODACER              PIC  X(001)         VALUE '1'.   
      *          448  448         * COD. ACERTO                         
           05 WRK-VALVENC              PIC  X(015)         VALUE        
              '000000000000000'.                                        
CPMCAC     05 WRK-VALVENC-R            REDEFINES WRK-VALVENC            
CPMCAC                                 PIC  9(015).                     
      *          449  463         * VR. ENCARGOS VENCIDOS               
           05 WRK-VALVINC              PIC  X(015)         VALUE        
              '000000000000000'.                                        
CPMCAC     05 WRK-VALVINC-R            REDEFINES WRK-VALVINC            
CPMCAC                                 PIC  9(015).                     
      *          464  478         * VR. ENCARGOS VINCENDOS              
           05 WRK-VALBXA               PIC  X(015)         VALUE        
              '000000000000000'.                                        
      *          479  493         * VR. BAIXA                           
           05 WRK-VALINIC              PIC  X(015)         VALUE        
              '000000000000000'.                                        
CPMCAC     05 WRK-VALINIC-R            REDEFINES WRK-VALINIC            
CPMCAC                                 PIC  9(015).                     
      *          494  508         * VR. DEVEDOR INICIAL                 
           05 WRK-VALORIOF             PIC  X(015)         VALUE        
              '000000000000000'.                                        
CPMCAC     05 WRK-VALORIOF-R           REDEFINES WRK-VALORIOF           
CPMCAC                                 PIC  9(015).                     
      *          509  523         * VR. IOF                             
           05 WRK-VRDEBCC              PIC  X(015)         VALUE        
              '000000000000000'.                                        
      *          524  538         * VR. DEBITO C/C                      
           05 WRK-VRCOBR               PIC  X(015)         VALUE        
              '000000000000000'.                                        
      *          539  553         * VR. COBRANCA                        
           05 WRK-CDOCOR               PIC  X(002)         VALUE '00'.  
      *          554  555         * COD. ULTIMA OCORR                   
           05 WRK-BLOQTR               PIC  X(001)         VALUE SPACES.
      *          556  556         * BLOQUEIO P/ TRANF                   
           05 WRK-DTPGTO               PIC  X(008)         VALUE        
              '00000000'.                                               
      *          557  564         * DATA DE PAGAMENTO                   
           05 WRK-ACERTOSX             PIC  X(001)         VALUE '1'.   
      *          565  565         * ACERTO                              
           05 WRK-ORIGBXA              PIC  X(002)         VALUE SPACES.
      *          566  567         * IDENT.ORIGEM DA BAIXA               
      *                           *    DA = CONSULTA                    
      *                           *    DE = MANUAL/ONLINE               
           05 WRK-TRANSST              PIC  X(001)         VALUE SPACES.
      *          568  568         * NO REG TRANSF PARA LP               
      *                           *  - S  (CL COM MARCA P)              
      *                           *  - T  (CL COM MARCA Q)              
           05 WRK-DEBCC                PIC  X(001)         VALUE SPACES.
      *          569  569         * DEBITO EM C/C                       
      *                           *  - S  (COM DEBITO C/C)              
      *                           *  - N  (SEM DEBITO C/C)              
           05 WRK-FIXO                 PIC  X(002)         VALUE '00'.  
      *          570  571         * ORIGEM DO REGISTRO                  
           05 WRK-SOL8273X             PIC  X(004)         VALUE SPACES.
      *          572  575         * SOLICITACAO 8273                    
                                                                        
       01  WRK-WKMOVTO                 PIC  X(575)         VALUE SPACES.
      *     AREA P/GUARDAR REGISTRO MOVTO DURANTE CONSISTENCIA          
                                                                        
      *----------------------------------------------------------------*
      *                   DEFENICAO   DO ARQUIVO CADLOCAL              *
      *----------------------------------------------------------------*
                                                                        
       01  WRK-REGLOCAL                PIC  X(040)         VALUE SPACES.
                                                                        
      ******************************************************************
      *  CAMPOS AUXILIARES                                             *
      ******************************************************************
                                                                        
       01  WRK-DATAHORA.                                                
           05 WRK-JULIANA              PIC  9(005) COMP-3  VALUE ZEROS. 
           05 WRK-AAMMDD2              PIC  9(007) COMP-3  VALUE ZEROS. 
           05 WRK-AAAAMMDD             PIC  9(009) COMP-3  VALUE ZEROS. 
           05 WRK-HHMMSS               PIC  9(007) COMP-3  VALUE ZEROS. 
           05 WRK-HHMMSSMM             PIC  9(013) COMP-3  VALUE ZEROS. 
           05 WRK-TIMESTA              PIC  X(020)         VALUE SPACES.
                                                                        
       01  WRK-DATAZ.                                                   
           05 WRK-ANO2                 PIC  X(004)         VALUE SPACES.
           05 WRK-MES2                 PIC  X(002)         VALUE SPACES.
           05 WRK-DIA2                 PIC  X(002)         VALUE SPACES.
                                                                        
       01  WRK-PADVAL19                PIC BZZZ.ZZZ.ZZZ.Z99,99          
                                                           VALUE SPACES.
CPMCAC 01  WRK-PADVAL19-R              REDEFINES WRK-PADVAL19           
CPMCAC                                 PIC  X(019).                     
                                                                        
       01  WRK-PADVAL20                PIC BZZZZ.ZZZ.ZZZ.ZZ9,99         
                                                           VALUE SPACES.
CPMCAC 01  WRK-PADVAL20-R              REDEFINES WRK-PADVAL20           
CPMCAC                                 PIC  X(020).                     
                                                                        
       01  WRK-AUXDATA                 PIC  X(008)         VALUE        
           '00000000'.                                                  
CPMCAC 01  WRK-AUXDATA-REDCPMACT       REDEFINES WRK-AUXDATA            
CPMCAC                                 PIC  9(008).                     
                                                                        
       01  WRK-AUXVALOR                PIC  9(015) COMP-3  VALUE ZEROS. 
CPMCAC 01  WRK-AUXVALOR-R              REDEFINES WRK-AUXVALOR           
CPMCAC                                 PIC  9(013)V99                   
CPMCAC                                             COMP-3.              
                                                                        
       01  WRK-CGCAUX.                                                  
           05 WRK-NUMCGC               PIC  X(009)         VALUE SPACES.
           05 WRK-FILZERO              PIC  X(001)         VALUE '0'.   
           05 WRK-FILCGC               PIC  X(004)         VALUE SPACES.
                                                                        
       01  WRK-TABCL4                  PIC  X(3402)        VALUE SPACES.
       01  WRK-DIGTAUX                 PIC  X(002)         VALUE ' '.   
       01  WRK-MASCARA                 PIC  X(006)         VALUE        
           X'00000F00000F'.                                             
       01  WRK-MASC2                   PIC  X(014)         VALUE        
           X'00000F0000000F00000F0000000F'.                             
       01  WRK-BRANCOS                 PIC  X(250)         VALUE ' '.   
       01  WRK-ZEROSZ                  PIC  X(250)         VALUE        
           ALL '0'.                                                     
                                                                        
      *----------------------------------------------------------------*
      * DEFINICAO DA TABELA DE ACUMULACAO DE CODIGOS DE INCONSISTENCIAS*
      *----------------------------------------------------------------*
                                                                        
       01  WRK-TABINCON                PIC  X(027)         VALUE SPACES.
                                                                        
       01  FILLER                      PIC  X(012)         VALUE        
           'ACUMULADORES'.                                              
       01  WRK-QTDERROS                PIC S9(003) COMP-3  VALUE ZEROS. 
       01  WRK-QTDLIDO                 PIC  9(007) COMP-3  VALUE ZEROS. 
       01  WRK-QTDCERRO                PIC  9(007) COMP-3  VALUE ZEROS. 
       01  WRK-QTDREJE                 PIC  9(007) COMP-3  VALUE ZEROS. 
       01  WRK-QTDGRAV                 PIC  9(007) COMP-3  VALUE ZEROS. 
       01  WRK-ACPAG                   PIC  9(005) COMP-3  VALUE ZEROS. 
       01  WRK-ACLIN                   PIC S9(003) COMP-3  VALUE 65.    
                                                                        
      ******************************************************************
      *        CABECALHO   DE   IMPRESSAO                              *
      ******************************************************************
                                                                        
       01  WRK-CAB1.                                                    
           05 FILLER                   PIC  X(001)         VALUE '1'.   
           05 FILLER                   PIC  X(010)         VALUE        
              '*CLLP6620*'.                                             
           05 FILLER                   PIC  X(030)         VALUE ' '.   
           05 FILLER                   PIC  X(054)         VALUE        
              'CRITICA DOS MOVIMENTOS DE ATUALIZACAO DO CADASTRO LPCL'. 
           05 FILLER                   PIC  X(027)         VALUE ' '.   
           05 FILLER                   PIC  X(004)         VALUE 'FOL.'.
           05 WRK-CB1PAG               PIC  X(007)         VALUE        
              '     '.                                                  
      *        BZZ.Z99                                                  
                                                                        
       01  WRK-CAB2.                                                    
           05 FILLER                   PIC  X(001)         VALUE ' '.   
           05 FILLER                   PIC  X(052)         VALUE ' '.   
           05 FILLER                   PIC  X(016)         VALUE        
              'MOVIMENTO DE :  '.                                       
           05 WRK-CB2DTMOV             PIC  X(010)         VALUE        
              '          '.                                             
      *        99/99/9999                                               
           05 FILLER                   PIC  X(039)         VALUE ' '.   
           05 FILLER                   PIC  X(005)         VALUE        
              'DATA '.                                                  
           05 WRK-CB2DATA              PIC  X(010)         VALUE        
              '          '.                                             
      *        99/99/9999                                               
                                                                        
       01  WRK-CAB3.                                                    
           05 FILLER                   PIC  X(133)         VALUE ' '.   
                                                                        
       01  WRK-CAB4.                                                    
           05 FILLER                   PIC  X(001)         VALUE '-'.   
           05 FILLER                   PIC  X(045)         VALUE ' '.   
           05 FILLER                   PIC  X(038)         VALUE        
              'T A B E L A   D E   C O D.   E R R O S'.                 
           05 FILLER                   PIC  X(049)         VALUE ' '.   
                                                                        
       01  WRK-CAB5.                                                    
           05 FILLER                   PIC  X(001)         VALUE ' '.   
           05 FILLER                   PIC  X(045)         VALUE ' '.   
           05 FILLER                   PIC  X(038)         VALUE        
              '--------------------------------------'.                 
           05 FILLER                   PIC  X(049)         VALUE ' '.   
                                                                        
       01  WRK-CAB6.                                                    
           05 FILLER                   PIC  X(001)         VALUE '-'.   
           05 FILLER                   PIC  X(013)         VALUE ' '.   
           05 FILLER                   PIC  X(004)         VALUE 'COD.'.
           05 FILLER                   PIC  X(009)         VALUE ' '.   
           05 FILLER                   PIC  X(017)         VALUE        
              'D E S C R I C A O'.                                      
           05 FILLER                   PIC  X(035)         VALUE ' '.   
           05 FILLER                   PIC  X(004)         VALUE 'COD.'.
           05 FILLER                   PIC  X(009)         VALUE ' '.   
           05 FILLER                   PIC  X(017)         VALUE        
              'D E S C R I C A O'.                                      
           05 FILLER                   PIC  X(024)         VALUE ' '.   
                                                                        
       01  WRK-CAB8.                                                    
           05 FILLER                   PIC  X(001)         VALUE '-'.   
           05 FILLER                   PIC  X(062)         VALUE ' '.   
           05 FILLER                   PIC  X(018)         VALUE        
              'TOTAIS DE CONTROLE'.                                     
           05 FILLER                   PIC  X(052)         VALUE ' '.   
                                                                        
       01  WRK-DET1.                                                    
           05 FILLER                   PIC  X(001)         VALUE ' '.   
           05 FILLER                   PIC  X(014)         VALUE ' '.   
           05 WRK-DT1COD1              PIC  X(002)         VALUE '00'.  
           05 FILLER                   PIC  X(002)         VALUE ' '.   
           05 WRK-DT1DESC1             PIC  X(035)         VALUE ' '.   
           05 FILLER                   PIC  X(026)         VALUE ' '.   
           05 WRK-DT1COD2              PIC  X(002)         VALUE '00'.  
           05 FILLER                   PIC  X(002)         VALUE ' '.   
           05 WRK-DT1DESC2             PIC  X(035)         VALUE ' '.   
CPMCAC     05 FILLER                   PIC  X(014)         VALUE ' '.   
                                                                        
       01  WRK-DET4.                                                    
           05 FILLER                   PIC  X(001)         VALUE ' '.   
           05 FILLER                   PIC  X(016)         VALUE ' '.   
           05 FILLER                   PIC  X(019)         VALUE        
              'REG. LIDOS       - '.                                    
           05 WRK-DT4QTD               PIC  X(010)         VALUE        
              '          '.                                             
      *        9.999.999                                                
           05 FILLER                   PIC  X(087)         VALUE ' '.   
                                                                        
       01  WRK-DET5.                                                    
           05 FILLER                   PIC  X(001)         VALUE ' '.   
           05 FILLER                   PIC  X(016)         VALUE ' '.   
           05 FILLER                   PIC  X(019)         VALUE        
              'REG. COM ERRO    - '.                                    
           05 WRK-DT5QTD               PIC  X(010)         VALUE        
              '          '.                                             
      *        9.999.999                                                
           05 FILLER                   PIC  X(087)         VALUE ' '.   
                                                                        
       01  WRK-DET6.                                                    
           05 FILLER                   PIC  X(001)         VALUE ' '.   
           05 FILLER                   PIC  X(016)         VALUE ' '.   
           05 FILLER                   PIC  X(019)         VALUE        
              'REG. REJEITADOS  - '.                                    
           05 WRK-DT6QTD               PIC  X(010)         VALUE        
              '          '.                                             
      *        9.999.999                                                
           05 FILLER                   PIC  X(087)         VALUE ' '.   
                                                                        
       01  WRK-DET7.                                                    
           05 FILLER                   PIC  X(001)         VALUE ' '.   
           05 FILLER                   PIC  X(016)         VALUE ' '.   
           05 FILLER                   PIC  X(019)         VALUE        
              'REG. GRAVADOS    - '.                                    
           05 WRK-DT7QTD               PIC  X(010)         VALUE        
              '          '.                                             
      *        9.999.999                                                
           05 FILLER                   PIC  X(087)         VALUE ' '.   
                                                                        
       01  WRK-CABCRIT1.                                                
           05 FILLER                   PIC  X(001)         VALUE '0'.   
           05 FILLER                   PIC  X(040)         VALUE        
              '  EMP/AG/CC/CART/CONTRATO/VENCTO'.                       
           05 FILLER                   PIC  X(001)         VALUE ' '.   
           05 FILLER                   PIC  X(040)         VALUE        
              'NOME DO DEVEDOR'.                                        
           05 FILLER                   PIC  X(001)         VALUE ' '.   
           05 FILLER                   PIC  X(017)         VALUE        
              '     CGC/CPF'.                                           
           05 FILLER                   PIC  X(001)         VALUE ' '.   
           05 FILLER                   PIC  X(005)         VALUE        
              'RZ/CT'.                                                  
           05 FILLER                   PIC  X(001)         VALUE ' '.   
           05 FILLER                   PIC  X(007)         VALUE        
              'ENT. CL'.                                                
           05 FILLER                   PIC  X(001)         VALUE ' '.   
           05 FILLER                   PIC  X(018)         VALUE        
              '    VR.DA OPERACAO'.                                     
                                                                        
       01  WRK-CABCRIT2.                                                
           05 FILLER                   PIC  X(001)         VALUE ' '.   
           05 FILLER                   PIC  X(019)         VALUE        
              '     VR.DEV.INICIAL'.                                    
           05 FILLER                   PIC  X(002)         VALUE ' '.   
           05 FILLER                   PIC  X(019)         VALUE        
              '     VALOR CONTABIL'.                                    
           05 FILLER                   PIC  X(002)         VALUE ' '.   
           05 FILLER                   PIC  X(019)         VALUE        
              '       VALOR DO IOF'.                                    
           05 FILLER                   PIC  X(002)         VALUE ' '.   
           05 FILLER                   PIC  X(019)         VALUE        
              '    VR.ENC.VENCIDOS'.                                    
           05 FILLER                   PIC  X(002)         VALUE ' '.   
           05 FILLER                   PIC  X(019)         VALUE        
              '   VR.ENC.VINCENDOS'.                                    
           05 FILLER                   PIC  X(002)         VALUE ' '.   
           05 FILLER                   PIC  X(019)         VALUE        
              '    VR.BASE CALCULO'.                                    
           05 FILLER                   PIC  X(004)         VALUE ' '.   
           05 FILLER                   PIC  X(002)         VALUE 'ID'.  
           05 FILLER                   PIC  X(002)         VALUE ' '.   
                                                                        
       01  WRK-CABCRIT3.                                                
           05 FILLER                   PIC  X(001)         VALUE ' '.   
           05 FILLER                   PIC  X(059)         VALUE        
              'NOME / CGC CPF AVALISTA 1'.                              
           05 FILLER                   PIC  X(004)         VALUE ' '.   
           05 FILLER                   PIC  X(059)         VALUE        
              'NOME / CGC CPF AVALISTA 2'.                              
           05 FILLER                   PIC  X(003)         VALUE ' '.   
           05 FILLER                   PIC  X(007)         VALUE        
              'AGRESP.'.                                                
                                                                        
       01  WRK-CABCRIT4.                                                
           05 FILLER                   PIC  X(001)         VALUE ' '.   
           05 FILLER                   PIC  X(033)         VALUE        
              'TIPO/NOME DA GARANTIA'.                                  
           05 FILLER                   PIC  X(001)         VALUE ' '.   
           05 FILLER                   PIC  X(006)         VALUE        
              'T.PEND'.                                                 
           05 FILLER                   PIC  X(002)         VALUE ' '.   
           05 FILLER                   PIC  X(008)         VALUE        
              'INIC.OP'.                                                
           05 FILLER                   PIC  X(002)         VALUE ' '.   
           05 FILLER                   PIC  X(008)         VALUE        
              'ENT.CL'.                                                 
           05 FILLER                   PIC  X(002)         VALUE ' '.   
           05 FILLER                   PIC  X(008)         VALUE        
              'DT.MOV'.                                                 
           05 FILLER                   PIC  X(002)         VALUE ' '.   
           05 FILLER                   PIC  X(008)         VALUE        
              'DT.AJUIZ'.                                               
           05 FILLER                   PIC  X(001)         VALUE ' '.   
           05 FILLER                   PIC  X(003)         VALUE        
              'LOC'.                                                    
           05 FILLER                   PIC  X(002)         VALUE ' '.   
           05 FILLER                   PIC  X(005)         VALUE        
              'MARCA'.                                                  
           05 FILLER                   PIC  X(002)         VALUE ' '.   
           05 FILLER                   PIC  X(008)         VALUE ' '.   
      *        NATUREZA                                                 
           05 FILLER                   PIC  X(004)         VALUE ' '.   
           05 FILLER                   PIC  X(023)         VALUE        
              'COD. DE ERROS'.                                          
           05 FILLER                   PIC  X(004)         VALUE ' '.   
                                                                        
       01  WRK-DETCRIT1.                                                
           05 FILLER                   PIC  X(001)         VALUE '0'.   
           05 WRK-DETEMP               PIC  X(005)         VALUE ' '.   
      *        99999                                                    
           05 FILLER                   PIC  X(001)         VALUE '/'.   
           05 WRK-DETAG                PIC  X(005)         VALUE ' '.   
      *        99999                                                    
           05 FILLER                   PIC  X(001)         VALUE '/'.   
           05 WRK-DETCC                PIC  X(007)         VALUE ' '.   
      *        9999999                                                  
           05 FILLER                   PIC  X(001)         VALUE '/'.   
           05 WRK-DETCART              PIC  X(003)         VALUE ' '.   
      *        999                                                      
           05 FILLER                   PIC  X(001)         VALUE '/'.   
           05 WRK-DETCONTR             PIC  X(007)         VALUE ' '.   
      *        9999999                                                  
           05 FILLER                   PIC  X(001)         VALUE '/'.   
           05 WRK-DETVENCT             PIC  X(008)         VALUE ' '.   
      *        DDMMAAAA                                                 
           05 FILLER                   PIC  X(001)         VALUE ' '.   
           05 WRK-DETNMDEV             PIC  X(040)         VALUE ' '.   
      *        NOME DO DEVEDOR                                          
           05 FILLER                   PIC  X(001)         VALUE ' '.   
           05 WRK-DETCGCDV             PIC  X(009)         VALUE ' '.   
      *        999999999                                                
           05 FILLER                   PIC  X(001)         VALUE ' '.   
           05 WRK-DETFILDV             PIC  X(004)         VALUE ' '.   
      *        9999                                                     
           05 FILLER                   PIC  X(001)         VALUE ' '.   
           05 WRK-DETCNTDV             PIC  X(002)         VALUE ' '.   
      *        99                                                       
           05 FILLER                   PIC  X(001)         VALUE ' '.   
           05 WRK-DETRAZAO             PIC  X(005)         VALUE ' '.   
      *        99999                                                    
           05 FILLER                   PIC  X(001)         VALUE ' '.   
           05 WRK-DETENTCL             PIC  X(007)         VALUE ' '.   
      *        9999999                                                  
           05 FILLER                   PIC  X(001)         VALUE ' '.   
           05 WRK-DETVROPE             PIC  X(018)         VALUE ' '.   
      *        ZZZ.ZZZ.ZZZ.Z99,99                                       
                                                                        
       01  WRK-DETCRIT2.                                                
            05 FILLER                  PIC  X(001)         VALUE ' '.   
            05 WRK-DETVRINI            PIC  X(019)         VALUE ' '.   
      *         ZZZZ.ZZZ.ZZZ.ZZ9,99                                     
            05 FILLER                  PIC  X(002)         VALUE ' '.   
            05 WRK-DETVRCON            PIC  X(019)         VALUE ' '.   
      *         ZZZZ.ZZZ.ZZZ.ZZ9,99                                     
            05 FILLER                  PIC  X(002)         VALUE ' '.   
            05 WRK-DETVRIOF            PIC  X(019)         VALUE ' '.   
      *         ZZZZ.ZZZ.ZZZ.ZZ9,99                                     
            05 FILLER                  PIC  X(002)         VALUE ' '.   
            05 WRK-DETVRVEN            PIC  X(019)         VALUE ' '.   
      *         ZZZZ.ZZZ.ZZZ.ZZ9,99                                     
            05 FILLER                  PIC  X(002)         VALUE ' '.   
            05 WRK-DETVRVIN            PIC  X(019)         VALUE ' '.   
      *         ZZZZ.ZZZ.ZZZ.ZZ9,99                                     
            05 FILLER                  PIC  X(002)         VALUE ' '.   
            05 WRK-DETVRBAS            PIC  X(019)         VALUE ' '.   
      *         ZZZZ.ZZZ.ZZZ.ZZ9,99                                     
            05 FILLER                  PIC  X(004)         VALUE ' '.   
            05 WRK-DETID               PIC  X(002)         VALUE ' '.   
            05 FILLER                  PIC  X(002)         VALUE ' '.   
                                                                        
       01  WRK-DETCRIT3.                                                
           05 FILLER                   PIC  X(001)         VALUE ' '.   
           05 WRK-DETNMAV1             PIC  X(040)         VALUE ' '.   
      *        NOME DO AVALISTA 1                                       
           05 FILLER                   PIC  X(002)         VALUE '  '.  
           05 WRK-DETCGCA1             PIC  X(009)         VALUE ' '.   
      *        999999999                                                
           05 FILLER                   PIC  X(001)         VALUE ' '.   
           05 WRK-DETFILA1             PIC  X(004)         VALUE ' '.   
      *        9999                                                     
           05 FILLER                   PIC  X(001)         VALUE ' '.   
           05 WRK-DETCNTA1             PIC  X(002)         VALUE ' '.   
      *        99                                                       
           05 FILLER                   PIC  X(004)         VALUE ' '.   
           05 WRK-DETNMAV2             PIC  X(040)         VALUE ' '.   
      *        NOME DO AVALISTA 2                                       
           05 FILLER                   PIC  X(002)         VALUE '  '.  
           05 WRK-DETCGCA2             PIC  X(009)         VALUE ' '.   
      *        999999999                                                
           05 FILLER                   PIC  X(001)         VALUE ' '.   
           05 WRK-DETFILA2             PIC  X(004)         VALUE ' '.   
      *        9999                                                     
           05 FILLER                   PIC  X(001)         VALUE ' '.   
           05 WRK-DETCNTA2             PIC  X(002)         VALUE ' '.   
      *        99                                                       
           05 FILLER                   PIC  X(004)         VALUE ' '.   
           05 WRK-DETAGRES             PIC  X(005)         VALUE ' '.   
      *        99999                                                    
           05 FILLER                   PIC  X(001)         VALUE ' '.   
                                                                        
       01  WRK-DETCRIT4.                                                
           05 FILLER                   PIC  X(001)         VALUE ' '.   
           05 WRK-DETTPGAR             PIC  X(002)         VALUE ' '.   
      *        99                                                       
           05 FILLER                   PIC  X(001)         VALUE ' '.   
           05 WRK-DETGARAN             PIC  X(030)         VALUE ' '.   
      *        GARANTIA                                                 
           05 FILLER                   PIC  X(002)         VALUE ' '.   
           05 WRK-DETPEND              PIC  X(004)         VALUE ' '.   
      *        9999                                                     
           05 FILLER                   PIC  X(003)         VALUE ' '.   
           05 WRK-DETDTINI             PIC  X(008)         VALUE ' '.   
      *        DDMMAAAA                                                 
           05 FILLER                   PIC  X(002)         VALUE ' '.   
           05 WRK-DETDTENT             PIC  X(008)         VALUE ' '.   
      *        DDMMAAAA                                                 
           05 FILLER                   PIC  X(002)         VALUE ' '.   
           05 WRK-DETDTMOV             PIC  X(008)         VALUE ' '.   
      *        DDMMAAAA                                                 
           05 FILLER                   PIC  X(002)         VALUE ' '.   
           05 WRK-DETDTAJU             PIC  X(008)         VALUE ' '.   
      *        DDMMAAAA                                                 
           05 FILLER                   PIC  X(002)         VALUE ' '.   
           05 WRK-DETLOC               PIC  X(002)         VALUE ' '.   
           05 FILLER                   PIC  X(005)         VALUE ' '.   
           05 WRK-DETMARCA             PIC  X(001)         VALUE ' '.   
           05 FILLER                   PIC  X(006)         VALUE ' '.   
           05 FILLER                   PIC  X(002)         VALUE ' '.   
      *        99 - NATUREZA                                            
           05 FILLER                   PIC  X(007)         VALUE ' '.   
           05 WRK-DETCDERR             PIC  X(027)         VALUE ' '.   
                                                                        
      ******************************************************************
      *  AREA PARA REGISTRO DE DATA  - ARQUIVO - ARQDATA               *
      ******************************************************************
                                                                        
       01  WRK-WDATA.                                                   
      *        DD/MM/AAAA                                               
           05 WRK-DT                   PIC  X(010)         VALUE        
              '31/01/1991'.                                             
      *        DDMMAAAA                                                 
           05 WRK-DTMOV                PIC  X(008)         VALUE        
              '31011991'.                                               
      *        AAAAMMDD                                                 
           05 WRK-DTMOVINV             PIC  X(008)         VALUE        
              '19910131'.                                               
      *        DDMMAAAA                                                 
           05 WRK-DTMOVMOD             PIC  X(005)         VALUE        
              X'031011991F'.                                            
      *        AAAAMMDD                                                 
           05 WRK-DTMOVMOI             PIC  X(005)         VALUE        
              X'019910131F'.                                            
      *        AAAADDD                                                  
           05 WRK-CODMOV               PIC  X(004)         VALUE        
              X'1991031F'.                                              
      *        DDMMAAAA                                                 
           05 WRK-DTULDIA              PIC  X(010)         VALUE        
              '31/01/1991'.                                             
      *        DDMMAAAA                                                 
           05 WRK-DTULTDIP             PIC  X(005)         VALUE        
              X'031011991F'.                                            
      *        DDMMAAAA                                                 
           05 WRK-DTDIA20              PIC  X(005)         VALUE        
              X'018011991F'.                                            
                                                                        
      *----------------------------------------------------------------*
      *        AREAS USADA NA POOL1205                                 *
      *----------------------------------------------------------------*
                                                                        
       01  WRK-LSTCAMPO.                                                
           05 WRK-DTENVIAD             PIC  9(008) COMP-3  VALUE ZEROS. 
CPMCAC     05 WRK-DTENVIAD-R           REDEFINES WRK-DTENVIAD           
CPMCAC                                 PIC  X(005).                     
           05 WRK-OPCAO                PIC  X(001)         VALUE SPACES.
           05 WRK-DTJULIAN             PIC  9(007) COMP-3  VALUE ZEROS. 
           05 WRK-DTEDITAD             PIC  X(010)         VALUE SPACES.
           05 WRK-DTGREGOR             PIC  X(008)         VALUE SPACES.
           05 WRK-DIASEMAN             PIC  X(013)         VALUE SPACES.
           05 WRK-MES                  PIC  X(009)         VALUE SPACES.
           05 WRK-DUTIANTE             PIC  X(008)         VALUE SPACES.
           05 WRK-DUTIPOST             PIC  X(008)         VALUE SPACES.
                                                                        
       01  WRK-MENSAGEM                PIC  X(050)         VALUE SPACES.
                                                                        
      *----------------------------------------------------------------*
      *        AREAS USADA NA POOL0480                                 *
      *----------------------------------------------------------------*
                                                                        
       01  WRK-COD1                    PIC  X(001)         VALUE '1'.   
       01  WRK-COD2                    PIC  X(001)         VALUE '2'.   
       01  WRK-COD3                    PIC  X(001)         VALUE '3'.   
       01  WRK-TABCL                   PIC  X(008)         VALUE        
           'TABCL'.                                                     
CPMCAC 01  WRK-TAMCHCL                 PIC  9(002)         VALUE 09.    
CPMCAC 01  WRK-TAMCL                   PIC  9(003)         VALUE 030.   
                                                                        
       01  WRK-CAMPOANT                PIC  X(009)         VALUE ' '.   
      *     GUARDA CAMPO AUX ANTERIOR                                   
                                                                        
       01  WRK-CAMPOAUX.                                                
           05 WRK-AGAUX                PIC S9(005) COMP-3  VALUE ZEROS. 
           05 WRK-IDENTAUX             PIC S9(005) COMP-3  VALUE ZEROS. 
           05 WRK-CARTAUX              PIC  X(003)         VALUE ' '.   
                                                                        
       01  WRK-TABELACL.                                                
           05 WRK-CHAVECL.                                              
      *           CHAVE                                                 
              10 WRK-EMPRESCL          PIC S9(005) COMP-3  VALUE ZEROS. 
              10 WRK-EMPRESCL-R        REDEFINES           WRK-EMPRESCL 
                                       PIC  X(003).                     
      *              EMPRESA                                            
              10 WRK-TIPENDCL          PIC S9(005) COMP-3  VALUE ZEROS. 
              10 WRK-TIPENDCL-R        REDEFINES           WRK-TIPENDCL 
                                       PIC  X(003).                     
      *              TIPO PENDENCIA                                     
              10 WRK-CARTEICL          PIC  X(003)         VALUE SPACES.
      *              CARTEIRA                                           
           05 WRK-SIGLACL              PIC  X(004)         VALUE SPACES.
      *           SIGLA                                                 
           05 WRK-AGEENTCL             PIC S9(005) COMP-3  VALUE ZEROS. 
           05 WRK-AGEENTCL-R           REDEFINES           WRK-AGEENTCL 
                                       PIC  X(003).                     
      *              AGENCIA ENTRADA                                    
           05 WRK-ENTRACL.                                              
      *           ENTRADA EM CL                                         
              10 WRK-ENDRZCL           PIC S9(005) COMP-3  VALUE ZEROS. 
      *              DEBITO - RAZAO                                     
              10 WRK-ENDCTCL           PIC S9(007) COMP-3  VALUE ZEROS. 
      *              DEBITO - CONTA                                     
              10 WRK-ENCRZCL           PIC S9(005) COMP-3  VALUE ZEROS. 
      *              CREDITO- RAZAO                                     
              10 WRK-ENCCTCL           PIC S9(007) COMP-3  VALUE ZEROS. 
      *              CREDITO- CONTA                                     
                                                                        
      ******************************************************************
      *        T A B E L A  DE CODIGO DE ERROS                         *
      ******************************************************************
                                                                        
       01  WRK-TABERRO.                                                 
           05 FILLER                   PIC  X(002)         VALUE '01'.  
           05 FILLER                   PIC  X(035)         VALUE        
              'EMPRESA INVALIDA                   '.                    
           05 FILLER                   PIC  X(002)         VALUE '02'.  
           05 FILLER                   PIC  X(035)         VALUE        
              'AGENCIA INVALIDA                   '.                    
           05 FILLER                   PIC  X(002)         VALUE '03'.  
           05 FILLER                   PIC  X(035)         VALUE        
              'NUMERO DO CL COM ERRO              '.                    
           05 FILLER                   PIC  X(002)         VALUE '04'.  
           05 FILLER                   PIC  X(035)         VALUE        
              'DIGITO NUMERO CL INVALIDO          '.                    
           05 FILLER                   PIC  X(002)         VALUE '05'.  
           05 FILLER                   PIC  X(035)         VALUE        
              'NUM. RAZAO CONTA. CL INVALIDO      '.                    
           05 FILLER                   PIC  X(002)         VALUE '06'.  
           05 FILLER                   PIC  X(035)         VALUE        
              'NUM. CONTA CONTAB CL INVALIDO      '.                    
           05 FILLER                   PIC  X(002)         VALUE '07'.  
           05 FILLER                   PIC  X(035)         VALUE        
              'NUM RAZAO CONTB ORIGEM INVALIDO    '.                    
           05 FILLER                   PIC  X(002)         VALUE '08'.  
           05 FILLER                   PIC  X(035)         VALUE        
              'NUM CONTA CONTAB ORIGEM INVALIDO   '.                    
           05 FILLER                   PIC  X(002)         VALUE '09'.  
           05 FILLER                   PIC  X(035)         VALUE        
              'NUM. C/CORRENTE INVALIDO           '.                    
           05 FILLER                   PIC  X(002)         VALUE '10'.  
           05 FILLER                   PIC  X(035)         VALUE        
              'DATA VENCIMENTO INVALIDO           '.                    
           05 FILLER                   PIC  X(002)         VALUE '11'.  
           05 FILLER                   PIC  X(035)         VALUE        
              'DATA ENTRADA CL INVALIDO           '.                    
           05 FILLER                   PIC  X(002)         VALUE '12'.  
           05 FILLER                   PIC  X(035)         VALUE        
              'DATA BAIXA CL INVALIDO             '.                    
           05 FILLER                   PIC  X(002)         VALUE '13'.  
           05 FILLER                   PIC  X(035)         VALUE        
              'DAT INCI. OPERACAO INVLIDO         '.                    
           05 FILLER                   PIC  X(002)         VALUE '14'.  
           05 FILLER                   PIC  X(035)         VALUE        
              'DATA PROTESTO INVALIDO             '.                    
           05 FILLER                   PIC  X(002)         VALUE '15'.  
           05 FILLER                   PIC  X(035)         VALUE        
              'DATA ENTRADA LP INVALIDO           '.                    
           05 FILLER                   PIC  X(002)         VALUE '16'.  
           05 FILLER                   PIC  X(035)         VALUE        
              'DATA BAIXA LP INVALIDO             '.                    
           05 FILLER                   PIC  X(002)         VALUE '17'.  
           05 FILLER                   PIC  X(035)         VALUE        
              'DAT MOVIMENTO INVALIDO             '.                    
           05 FILLER                   PIC  X(002)         VALUE '18'.  
           05 FILLER                   PIC  X(035)         VALUE        
              'SIGLA NATUREZA OPERACAO C/. ERRO   '.                    
           05 FILLER                   PIC  X(002)         VALUE '19'.  
           05 FILLER                   PIC  X(035)         VALUE        
              'CARTEIRA CL NAT. OPERACAO C/ ERRO  '.                    
           05 FILLER                   PIC  X(002)         VALUE '20'.  
           05 FILLER                   PIC  X(035)         VALUE        
              'VALOR DA OPERACAO INVALIDO         '.                    
           05 FILLER                   PIC  X(002)         VALUE '21'.  
           05 FILLER                   PIC  X(035)         VALUE        
              'VALOR CONTABIL INVALIDO            '.                    
           05 FILLER                   PIC  X(002)         VALUE '22'.  
           05 FILLER                   PIC  X(035)         VALUE        
              'COD. MOEDA INVALIDO                '.                    
           05 FILLER                   PIC  X(002)         VALUE '23'.  
           05 FILLER                   PIC  X(035)         VALUE        
              'LOCAL INVALIDO                     '.                    
           05 FILLER                   PIC  X(002)         VALUE '24'.  
           05 FILLER                   PIC  X(035)         VALUE        
              'IDENTIFICACAO CONTABIL C/ ERRO     '.                    
           05 FILLER                   PIC  X(002)         VALUE '25'.  
           05 FILLER                   PIC  X(035)         VALUE        
              'TAXA PUNIT. C ERRO                 '.                    
           05 FILLER                   PIC  X(002)         VALUE '26'.  
           05 FILLER                   PIC  X(035)         VALUE        
              'IDENTIFICADOR CL/LP INVALIDO       '.                    
           05 FILLER                   PIC  X(002)         VALUE '27'.  
           05 FILLER                   PIC  X(035)         VALUE        
              'CGC/CPF DEVEDOR INVALIDO           '.                    
           05 FILLER                   PIC  X(002)         VALUE '28'.  
           05 FILLER                   PIC  X(035)         VALUE        
              'COMPENSA INVALIDO                  '.                    
           05 FILLER                   PIC  X(002)         VALUE '29'.  
           05 FILLER                   PIC  X(035)         VALUE        
              'COD. ACERTO INVALIDO               '.                    
           05 FILLER                   PIC  X(002)         VALUE '30'.  
           05 FILLER                   PIC  X(035)         VALUE        
              'NOME DO 1. AVALISTA INEXISTENTE    '.                    
           05 FILLER                   PIC  X(002)         VALUE '31'.  
           05 FILLER                   PIC  X(035)         VALUE        
              'CGC/CPF DO 1. AVALISTA INVALIDO    '.                    
           05 FILLER                   PIC  X(002)         VALUE '32'.  
           05 FILLER                   PIC  X(035)         VALUE        
              'NOME DO 2. AVALISTA INEXIXTENTE    '.                    
           05 FILLER                   PIC  X(002)         VALUE '33'.  
           05 FILLER                   PIC  X(035)         VALUE        
              'CGC/CPF DO 2. AVALISTA INVALIDO    '.                    
           05 FILLER                   PIC  X(002)         VALUE '34'.  
           05 FILLER                   PIC  X(035)         VALUE        
              'VALOR BASE P/ CALCULO INVLIDO      '.                    
           05 FILLER                   PIC  X(002)         VALUE '35'.  
           05 FILLER                   PIC  X(035)         VALUE        
              'DATA REC. EXPURGO INVALIDO         '.                    
           05 FILLER                   PIC  X(002)         VALUE '36'.  
           05 FILLER                   PIC  X(035)         VALUE        
              'SUB. EXIAB. INVLIDO                '.                    
           05 FILLER                   PIC  X(002)         VALUE '37'.  
           05 FILLER                   PIC  X(035)         VALUE        
              'CPF DO ADVOGADO INVALIDO           '.                    
           05 FILLER                   PIC  X(002)         VALUE '38'.  
           05 FILLER                   PIC  X(035)         VALUE        
              'CARTEIRA ORIGEM C/ ERRO            '.                    
           05 FILLER                   PIC  X(002)         VALUE '39'.  
           05 FILLER                   PIC  X(035)         VALUE        
              'TIPOGARANT. C/ ERRO                '.                    
           05 FILLER                   PIC  X(002)         VALUE '40'.  
           05 FILLER                   PIC  X(035)         VALUE        
              'TIPO DE PENDENCIA C/ ERRO          '.                    
           05 FILLER                   PIC  X(002)         VALUE '41'.  
           05 FILLER                   PIC  X(035)         VALUE        
              'MARCA INVALIDODA                   '.                    
           05 FILLER                   PIC  X(002)         VALUE '42'.  
           05 FILLER                   PIC  X(035)         VALUE        
              'DATA VCTO P/ EFEITO DE TRANSF. INV.'.                    
           05 FILLER                   PIC  X(002)         VALUE '43'.  
           05 FILLER                   PIC  X(035)         VALUE        
              'REGISTRO EM BRANCO                 '.                    
           05 FILLER                   PIC  X(002)         VALUE '44'.  
           05 FILLER                   PIC  X(035)         VALUE        
              'COD. FUNC. GERENTE INVALIDO        '.                    
           05 FILLER                   PIC  X(002)         VALUE '45'.  
           05 FILLER                   PIC  X(035)         VALUE        
              'NOME DO GERENTE INEXISTENTE OU INV.'.                    
           05 FILLER                   PIC  X(002)         VALUE '46'.  
           05 FILLER                   PIC  X(035)         VALUE        
              'GARATIA INEXISTENTE OU INVALIDO    '.                    
           05 FILLER                   PIC  X(002)         VALUE '47'.  
           05 FILLER                   PIC  X(035)         VALUE        
              'VALOR ENCARGOS VENCIDOS INVALIDO   '.                    
           05 FILLER                   PIC  X(002)         VALUE '48'.  
           05 FILLER                   PIC  X(035)         VALUE        
              'VALOR DEVDOR INICIAL INVALIDO      '.                    
           05 FILLER                   PIC  X(002)         VALUE '49'.  
           05 FILLER                   PIC  X(035)         VALUE        
              'VALOR IOF INVALIDO                 '.                    
           05 FILLER                   PIC  X(002)         VALUE '50'.  
           05 FILLER                   PIC  X(035)         VALUE        
              'IDENT. CRED. LIQUIDO INVALIDO      '.                    
           05 FILLER                   PIC  X(002)         VALUE '51'.  
           05 FILLER                   PIC  X(035)         VALUE        
              'DATA BAIXA CL INVALIDO             '.                    
           05 FILLER                   PIC  X(002)         VALUE '52'.  
           05 FILLER                   PIC  X(035)         VALUE        
              'DATA BAIXA LP INVALIDO             '.                    
           05 FILLER                   PIC  X(002)         VALUE '53'.  
           05 FILLER                   PIC  X(035)         VALUE        
              'VALOR DEBITADO C/C INVALIDO        '.                    
           05 FILLER                   PIC  X(002)         VALUE '54'.  
           05 FILLER                   PIC  X(035)         VALUE        
              'VALOR CONTABIL INVALIDO            '.                    
           05 FILLER                   PIC  X(002)         VALUE '55'.  
           05 FILLER                   PIC  X(035)         VALUE        
              'VALOR DE COBRANCA INVALIDO         '.                    
           05 FILLER                   PIC  X(002)         VALUE '56'.  
           05 FILLER                   PIC  X(035)         VALUE        
              'AVLOR ENCARGOS VINCENDOS INVALIDO  '.                    
           05 FILLER                   PIC  X(002)         VALUE '57'.  
           05 FILLER                   PIC  X(035)         VALUE        
              'LOCAL INVALIDO                     '.                    
           05 FILLER                   PIC  X(002)         VALUE '58'.  
           05 FILLER                   PIC  X(035)         VALUE        
              'OCORR. INVALIDO                    '.                    
           05 FILLER                   PIC  X(002)         VALUE '59'.  
           05 FILLER                   PIC  X(035)         VALUE        
              'VALOR BAIXA INVALIDO               '.                    
           05 FILLER                   PIC  X(002)         VALUE '60'.  
           05 FILLER                   PIC  X(035)         VALUE        
              'TIPO REG. ERRADO BX TOT            '.                    
           05 FILLER                   PIC  X(002)         VALUE '61'.  
           05 FILLER                   PIC  X(035)         VALUE        
              'TIPO DE REGISTRO DIFERENTE DE 1    '.                    
           05 FILLER                   PIC  X(002)         VALUE '62'.  
           05 FILLER                   PIC  X(035)         VALUE        
              'FALTA REG. TIPO 3     P. INCLUSAO  '.                    
           05 FILLER                   PIC  X(002)         VALUE '63'.  
           05 FILLER                   PIC  X(035)         VALUE        
              'DATA PAGAMENTO INVALIDA            '.                    
           05 FILLER                   PIC  X(002)         VALUE '64'.  
           05 FILLER                   PIC  X(035)         VALUE        
              'TIPO REG. ERRADO BX PAR.           '.                    
           05 FILLER                   PIC  X(002)         VALUE '65'.  
           05 FILLER                   PIC  X(035)         VALUE        
              'TIPO REG. ERRADO TRANSF.           '.                    
           05 FILLER                   PIC  X(002)         VALUE '66'.  
           05 FILLER                   PIC  X(035)         VALUE        
              'TIPO REG. ERRADO RET/CAD           '.                    
           05 FILLER                   PIC  X(002)         VALUE '67'.  
           05 FILLER                   PIC  X(035)         VALUE        
              'NUMERO NATUREZA OPER. INVALIDO     '.                    
           05 FILLER                   PIC  X(002)         VALUE '68'.  
           05 FILLER                   PIC  X(035)         VALUE        
              'NOME DEVEDOR INVALIDO              '.                    
           05 FILLER                   PIC  X(002)         VALUE '69'.  
           05 FILLER                   PIC  X(035)         VALUE        
              'COD. DE ACERTO ERRADO              '.                    
           05 FILLER                   PIC  X(002)         VALUE '70'.  
           05 FILLER                   PIC  X(035)         VALUE        
              'CHAVE INVALIDA                     '.                    
           05 FILLER                   PIC  X(002)         VALUE '71'.  
           05 FILLER                   PIC  X(035)         VALUE        
              'TIPO REG. INVALIDO P. SUBSTITUICAO '.                    
           05 FILLER                   PIC  X(002)         VALUE '72'.  
           05 FILLER                   PIC  X(035)         VALUE        
              'AGENCIA PARA RESP INVALIDA         '.                    
           05 FILLER                   PIC  X(002)         VALUE '99'.  
           05 FILLER                   PIC  X(035)         VALUE        
              'REG. INVALIDO (+ DE 8 ERROS)       '.                    
           05 FILLER                   PIC  X(002)         VALUE '**'.  
                                                                        
       77  WRK-BATCH                   PIC  X(008)         VALUE        
           'BATCH'.                                                     
                                                                        
CPMCAC*----------------------------------------------------------------*
CPMCAC 77  FILLER                      PIC  X(021)         VALUE        
CPMCAC     'AREA PARA INDEXADORES'.                                     
CPMCAC*----------------------------------------------------------------*
                                                                        
CPMCAC 77  IND-TABCL4                  PIC  9(009) COMP-3  VALUE ZEROS. 
CPMCAC 77  IND-TABINCON                PIC  9(009) COMP-3  VALUE ZEROS. 
CPMCAC 77  IND-TABERRO                 PIC  9(009) COMP-3  VALUE ZEROS. 
                                                                        
CPMCAC*----------------------------------------------------------------*
CPMCAC 01  FILLER                      PIC  X(050)         VALUE        
CPMCAC     'AREA PARA A CLLP8400'.                                      
CPMCAC*----------------------------------------------------------------*
                                                                        
CPMCAC 01  WRK-CLLP8400                PIC  X(008)         VALUE        
CPMCAC     'CLLP8400'.                                                  
CPMCAC 01  WRK-CLLP8400-TAMANHO        PIC S9(004) COMP    VALUE ZEROS. 
CPMCAC 01  WRK-CLLP8400-OPERACAO       PIC  X(002)         VALUE SPACES.
CPMCAC 01  WRK-CLLP8400-OPERANDO1      PIC  X(001)         VALUE SPACES.
CPMCAC 01  WRK-CLLP8400-OPERANDO6      PIC  X(006)         VALUE SPACES.
CPMCAC 01  WRK-CLLP8400-OPERANDO14     PIC  X(014)         VALUE SPACES.
CPMCAC 01  WRK-CLLP8400-MASCARA1       PIC  X(001)         VALUE SPACES.
CPMCAC 01  WRK-CLLP8400-MASCARA6       PIC  X(006)         VALUE SPACES.
CPMCAC 01  WRK-CLLP8400-MASCARA14      PIC  X(014)         VALUE SPACES.
                                                                        
CPMCAC*----------------------------------------------------------------*
CPMCAC 01  FILLER                      PIC  X(050)         VALUE        
CPMCAC     'AREA PARA VARIAVEIS AUXILIARES'.                            
CPMCAC*----------------------------------------------------------------*
                                                                        
CPMCAC 01  WRK-S9-9-D                  PIC +9(009)         VALUE ZEROS. 
CPMCAC 01  FILLER                      REDEFINES WRK-S9-9-D.            
CPMCAC     05 FILLER                   PIC  X(002).                     
CPMCAC     05 WRK-X-8                  PIC  X(008).                     
                                                                        
CPMCAC 01  WRK-X-13                    PIC  X(013)         VALUE SPACES.
CPMCAC 01  WRK-9-13                    REDEFINES WRK-X-13               
CPMCAC                                 PIC  9(013).                     
                                                                        
CPMCAC 01  WRK-9-13V99-D               PIC  9(013)V99      VALUE ZEROS. 
CPMCAC 01  FILLER                      REDEFINES WRK-9-13V99-D.         
CPMCAC     05 FILLER                   PIC  X(001).                     
CPMCAC     05 WRK-9-12V99-D            PIC  9(012)V99.                  
                                                                        
CPMCAC 01  WRK-05-CS                   PIC S9(005)         VALUE ZEROS. 
CPMCAC 01  WRK-05-SS                   REDEFINES           WRK-05-CS    
CPMCAC                                 PIC  9(005).                     
                                                                        
CPMCAC 01  WRK-07-CS                   PIC S9(007)         VALUE ZEROS. 
CPMCAC 01  WRK-07-SS                   REDEFINES           WRK-07-CS    
CPMCAC                                 PIC  9(007).                     
                                                                        
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(020)         VALUE        
           'AREA DE FILE-STATUS'.                                       
      *----------------------------------------------------------------*
                                                                        
       77  WRK-FS-ARQDATA              PIC  X(002)         VALUE SPACES.
       77  WRK-FS-CADLOCAL             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-CADPEND              PIC  X(002)         VALUE SPACES.
       77  WRK-FS-MOVTO                PIC  X(002)         VALUE SPACES.
       77  WRK-FS-CERTO                PIC  X(002)         VALUE SPACES.
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
       01  FILLER                      PIC  X(050)         VALUE        
           'AREA PARA BRAD7100'.                                        
      *----------------------------------------------------------------*
                                                                        
           COPY I#BRAD7C.                                               
                                                                        
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE        
           'FIM DA WORKING STORAGE SECTION '.                           
      *----------------------------------------------------------------*
                                                                        
      *================================================================*
       PROCEDURE                       DIVISION.                        
      *================================================================*
                                                                        
      *----------------------------------------------------------------*
      * ABRINDO ARQUIVOS                                               *
      *----------------------------------------------------------------*
                                                                        
           OPEN INPUT  CADLOCAL                                         
                       CADPEND                                          
                       MOVTO                                            
                       ARQDATA                                          
                OUTPUT CERTO                                            
                       RELATO                                           
                                                                        
           MOVE WRK-ABERTURA           TO WRK-OPERACAO                  
                                                                        
           PERFORM 9000-TESTAR-FILE-STATUS THRU 9000-99-FIM             
                                                                        
           READ ARQDATA                INTO WRK-WDATA END-READ          
                                                                        
           MOVE WRK-LEITURA            TO WRK-OPERACAO                  
                                                                        
           IF  WRK-FS-ARQDATA          EQUAL '10'                       
CPMCAC         NEXT SENTENCE                                            
           ELSE                                                         
               PERFORM 9010-TESTAR-FS-ARQDATA THRU 9010-99-FIM          
           END-IF                                                       
                                                                        
           MOVE WRK-DT                 TO WRK-CB2DTMOV                  
                                                                        
           CLOSE ARQDATA                                                
                                                                        
           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO                  
                                                                        
           PERFORM 9010-TESTAR-FS-ARQDATA THRU 9010-99-FIM              
                                                                        
CPMCAC     CALL 'BRAD7600'             USING WRK-DATAHORA               
                                                                        
CPMCAC     MOVE WRK-AAAAMMDD           TO WRK-S9-9-D                    
CPMCAC     MOVE WRK-X-8                TO WRK-DATAZ                     
           MOVE WRK-ANO2               TO WRK-DT(7:4)                   
           MOVE WRK-MES2               TO WRK-DT(4:2)                   
           MOVE WRK-DIA2               TO WRK-DT(1:2)                   
           MOVE WRK-DT                 TO WRK-CB2DATA.                  
                                                                        
      *----------------------------------------------------------------*
      * ROTINA PARA CARREGAR A TABELA COM CAD. LOCAL  : TABCL4         *
      *----------------------------------------------------------------*
                                                                        
           MOVE 1                      TO IND-TABCL4.                   
                                                                        
      *----------------------------------------------------------------*
       0100-GETLOCAL.                                                   
      *----------------------------------------------------------------*
                                                                        
           READ CADLOCAL               INTO WRK-REGLOCAL END-READ       
                                                                        
           MOVE WRK-LEITURA            TO WRK-OPERACAO                  
                                                                        
           IF  WRK-FS-CADLOCAL         EQUAL '10'                       
               GO TO 0110-FIMLOC                                        
           ELSE                                                         
               PERFORM 9020-TESTAR-FS-CADLOCAL THRU 9020-99-FIM         
           END-IF                                                       
                                                                        
           MOVE WRK-REGLOCAL(1:34)     TO WRK-TABCL4(IND-TABCL4:34)     
                                                                        
           ADD 34                      TO IND-TABCL4                    
                                                                        
           GO TO 0100-GETLOCAL.                                         
                                                                        
      *----------------------------------------------------------------*
       0110-FIMLOC.                                                     
      *----------------------------------------------------------------*
                                                                        
           MOVE '**'                   TO WRK-TABCL4(IND-TABCL4:2)      
                                                                        
           CLOSE CADLOCAL                                               
                                                                        
           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO                  
                                                                        
           PERFORM 9020-TESTAR-FS-CADLOCAL THRU 9020-99-FIM.            
                                                                        
      *----------------------------------------------------------------*
      * ROTINA PARA CARREGAR A TABELA COM MESTRE DE AGENCIA :          *
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       0120-LERMESTR.                                                   
      *----------------------------------------------------------------*
                                                                        
           READ CADPEND                INTO WRK-MESREGIS END-READ       
                                                                        
           MOVE WRK-LEITURA            TO WRK-OPERACAO                  
                                                                        
           IF  WRK-FS-CADPEND          EQUAL '10'                       
               GO TO 0130-FIMEST                                        
           ELSE                                                         
               PERFORM 9030-TESTAR-FS-CADPEND THRU 9030-99-FIM          
           END-IF                                                       
                                                                        
           IF  WRK-MESEMPRE            EQUAL 99999                      
               GO TO 0130-FIMEST                                        
           END-IF                                                       
                                                                        
      *     MONTA TABELA  TABCL                                         
                                                                        
           MOVE WRK-MESEMPRE-R         TO WRK-EMPRESCL-R                
           MOVE WRK-MESTPPEN-R         TO WRK-TIPENDCL-R                
           MOVE WRK-MESCARTE           TO WRK-CARTEICL                  
           MOVE WRK-MESSIGLA           TO WRK-SIGLACL                   
           MOVE WRK-MESAGENT-R         TO WRK-AGEENTCL-R                
           MOVE WRK-MESENTRA           TO WRK-ENTRACL                   
                                                                        
CPMCAC     MOVE 6                      TO WRK-CLLP8400-TAMANHO          
CPMCAC     MOVE 'OC'                   TO WRK-CLLP8400-OPERACAO         
CPMCAC     MOVE WRK-MASCARA            TO WRK-CLLP8400-MASCARA6         
CPMCAC     MOVE WRK-CHAVECL(1:6)       TO WRK-CLLP8400-OPERANDO6        
                                                                        
CPMCAC     CALL WRK-CLLP8400           USING WRK-CLLP8400-TAMANHO       
CPMCAC                                       WRK-CLLP8400-OPERACAO      
CPMCAC                                       WRK-CLLP8400-OPERANDO6     
CPMCAC                                       WRK-CLLP8400-MASCARA6      
                                                                        
CPMCAC     MOVE WRK-CLLP8400-OPERANDO6 TO WRK-CHAVECL(1:6)              
                                                                        
CPMCAC     MOVE 14                     TO WRK-CLLP8400-TAMANHO          
CPMCAC     MOVE 'OC'                   TO WRK-CLLP8400-OPERACAO         
CPMCAC     MOVE WRK-MASC2              TO WRK-CLLP8400-MASCARA14        
CPMCAC     MOVE WRK-ENTRACL            TO WRK-CLLP8400-OPERANDO14       
                                                                        
CPMCAC     CALL WRK-CLLP8400           USING WRK-CLLP8400-TAMANHO       
CPMCAC                                       WRK-CLLP8400-OPERACAO      
CPMCAC                                       WRK-CLLP8400-OPERANDO14    
CPMCAC                                       WRK-CLLP8400-MASCARA14     
                                                                        
CPMCAC     MOVE WRK-CLLP8400-OPERANDO14                                 
CPMCAC                                 TO WRK-ENTRACL                   
                                                                        
           CALL 'BRAD0480'             USING WRK-COD1                   
                                             WRK-TABCL                  
                                             WRK-TAMCHCL                
                                             WRK-TAMCL                  
                                             WRK-TABELACL               
                                                                        
           GO TO 0120-LERMESTR.                                         
                                                                        
      *----------------------------------------------------------------*
       0130-FIMEST.                                                     
      *----------------------------------------------------------------*
                                                                        
           CLOSE CADPEND                                                
                                                                        
           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO                  
                                                                        
           PERFORM 9030-TESTAR-FS-CADPEND THRU 9030-99-FIM              
                                                                        
           CALL 'BRAD0480'             USING WRK-COD2                   
                                             WRK-TABCL.                 
                                                                        
      *----------------------------------------------------------------*
      * ROTINA DE INICIO DE CONSISTENCIAS                              *
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       0140-INICIO.                                                     
      *----------------------------------------------------------------*
                                                                        
           MOVE  ALL '   '             TO WRK-TABINCON                  
           MOVE  1                     TO IND-TABINCON                  
                                                                        
           PERFORM 0690-LERMOVTO THRU 0740-99-FIM.                      
                                                                        
      *----------------------------------------------------------------*
       0150-BEGIN.                                                      
      *----------------------------------------------------------------*
                                                                        
      *     INCLUSAO                                                    
                                                                        
           IF  WRK-CODACER             EQUAL '1'                        
               GO TO 0160-MODULO02                                      
           END-IF                                                       
                                                                        
           PERFORM 1540-INCONS69 THRU 1600-99-FIM.                      
                                                                        
      *----------------------------------------------------------------*
      * ROTINA DE INCLUSAO - CONSISTENCIA                              *
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       0160-MODULO02.                                                   
      *----------------------------------------------------------------*
                                                                        
      *     TIPO DE REGISTRO                                            
                                                                        
           IF  WRK-ACTIPOX             EQUAL '1'                        
               GO TO 0170-CONT07                                        
           END-IF                                                       
                                                                        
           PERFORM 1460-INCONS61 THRU 1600-99-FIM.                      
                                                                        
      *----------------------------------------------------------------*
       0170-CONT07.                                                     
      *----------------------------------------------------------------*
                                                                        
           PERFORM 0190-CONSINCL THRU 0680-99-FIM                       
                                                                        
           IF  WRK-QTDERROS            NOT EQUAL 0                      
               GO TO 0180-CONT12                                        
           END-IF                                                       
                                                                        
           PERFORM 0850-GRAVAOK THRU 0850-99-FIM                        
                                                                        
           ADD 1                       TO WRK-QTDGRAV                   
                                                                        
           GO TO 0140-INICIO.                                           
                                                                        
      *----------------------------------------------------------------*
       0180-CONT12.                                                     
      *----------------------------------------------------------------*
                                                                        
           PERFORM 1630-IMPDET THRU 1780-99-FIM                         
                                                                        
           ADD 1                       TO WRK-QTDCERRO                  
                                                                        
           GO TO 0140-INICIO.                                           
                                                                        
      *----------------------------------------------------------------*
      * CONSISTENCIA DOS DADOS DE INCLUSAO                             *
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       0190-CONSINCL.                                                   
      *----------------------------------------------------------------*
                                                                        
CPMCAC     CONTINUE.                                                    
                                                                        
      *----------------------------------------------------------------*
       0200-MVCRAZAO.                                                   
      *----------------------------------------------------------------*
                                                                        
      *     ZERANDO CONT.CL E ORIGEM                                    
                                                                        
CPMCAC     MOVE WRK-ZEROSZ(1:24)       TO WRK-CONT                      
                                                                        
      *     NRO C/C INVALIDO                                            
                                                                        
CPMCAC     IF  WRK-CONTA-R             NOT NUMERIC                      
               GO TO 0210-CONT101                                       
           END-IF                                                       
                                                                        
           IF  WRK-CONTA               EQUAL '9999999'                  
               GO TO 0210-CONT101                                       
           END-IF                                                       
                                                                        
           GO TO 0220-CONT102.                                          
                                                                        
      *----------------------------------------------------------------*
       0210-CONT101.                                                    
      *----------------------------------------------------------------*
                                                                        
           PERFORM 0940-INCONS09 THRU 1600-99-FIM.                      
                                                                        
      *----------------------------------------------------------------*
       0220-CONT102.                                                    
      *----------------------------------------------------------------*
                                                                        
      *     DATA VENCIMENTO                                             
                                                                        
           MOVE WRK-VENCIM             TO WRK-DATA06                    
                                                                        
           PERFORM 0760-DATA00 THRU 0790-99-FIM                         
                                                                        
           IF  WRK-DATA05              EQUAL 'S'                        
               GO TO 0230-CONT103                                       
           END-IF                                                       
                                                                        
           PERFORM 0960-INCONS11 THRU 1600-99-FIM.                      
                                                                        
      *----------------------------------------------------------------*
       0230-CONT103.                                                    
      *----------------------------------------------------------------*
                                                                        
      *     DATA ENTRADA CL                                             
                                                                        
           MOVE WRK-ENTRADA            TO WRK-DATA06                    
                                                                        
           PERFORM 0750-DATA01 THRU 0790-99-FIM                         
                                                                        
           IF  WRK-DATA05              EQUAL 'S'                        
               GO TO 0240-CONT104                                       
           END-IF                                                       
                                                                        
           PERFORM 0970-INCONS12 THRU 1600-99-FIM.                      
                                                                        
      *----------------------------------------------------------------*
       0240-CONT104.                                                    
      *----------------------------------------------------------------*
                                                                        
      *     ZERANDO DT. BAIXA CL                                        
                                                                        
CPMCAC      MOVE WRK-ZEROSZ(1:8)        TO WRK-BAIXACL                  
                                                                        
      *     DATA INICIO OPER.                                           
                                                                        
CPMCAC     IF  WRK-OPERACAOO           EQUAL WRK-ZEROSZ(1:8)            
               GO TO 0250-CONT105                                       
           END-IF                                                       
                                                                        
CPMCAC     MOVE WRK-OPERACAOO          TO WRK-DATA06                    
                                                                        
           PERFORM 0750-DATA01 THRU 0790-99-FIM                         
                                                                        
           IF  WRK-DATA05              EQUAL 'S'                        
               GO TO 0250-CONT105                                       
           END-IF                                                       
                                                                        
           PERFORM 0980-INCONS13 THRU 1600-99-FIM.                      
                                                                        
      *----------------------------------------------------------------*
       0250-CONT105.                                                    
      *----------------------------------------------------------------*
                                                                        
      *     DATA PROTESTO                                               
                                                                        
CPMCAC     IF  WRK-AJUIZAM             NOT EQUAL WRK-ZEROSZ(1:8)        
               GO TO 0260-CONT106                                       
           END-IF                                                       
                                                                        
           GO TO 0270-CONT107.                                          
                                                                        
      *----------------------------------------------------------------*
       0260-CONT106.                                                    
      *----------------------------------------------------------------*
                                                                        
           MOVE WRK-AJUIZAM            TO WRK-DATA06                    
                                                                        
           PERFORM 0750-DATA01 THRU 0790-99-FIM                         
                                                                        
           IF  WRK-DATA05              EQUAL 'S'                        
               GO TO 0270-CONT107                                       
           END-IF                                                       
                                                                        
           PERFORM 0990-INCONS14 THRU 1600-99-FIM.                      
                                                                        
      *----------------------------------------------------------------*
       0270-CONT107.                                                    
      *----------------------------------------------------------------*
                                                                        
      *     ZERANDO DT. ENTRADA LP                                      
                                                                        
CPMCAC     MOVE WRK-ZEROSZ(1:8)        TO WRK-ENTRALP                   
                                                                        
      *     ZERANDO DT. BAIXA LP                                        
                                                                        
CPMCAC     MOVE WRK-ZEROSZ(1:8)        TO WRK-BAIXALP                   
                                                                        
      *     DATA MOVIMENTO                                              
                                                                        
           MOVE WRK-DTAMOVTO           TO WRK-DATA06                    
                                                                        
           PERFORM 0750-DATA01 THRU 0790-99-FIM                         
                                                                        
           IF  WRK-DATA05              EQUAL 'S'                        
               GO TO 0280-CONT108                                       
           END-IF                                                       
                                                                        
           PERFORM 1020-INCONS17 THRU 1600-99-FIM.                      
                                                                        
      *----------------------------------------------------------------*
       0280-CONT108.                                                    
      *----------------------------------------------------------------*
                                                                        
      *     NRO NATUREZA OPERACAO                                       
                                                                        
CPMCAC     IF  WRK-NUMOP-R             NOT NUMERIC                      
               GO TO 0290-CONT109                                       
           END-IF                                                       
                                                                        
CPMCAC     IF  WRK-NUMOP               EQUAL WRK-ZEROSZ(1:7)            
               GO TO 0290-CONT109                                       
           END-IF                                                       
                                                                        
           GO TO 0300-CONT110.                                          
                                                                        
      *----------------------------------------------------------------*
       0290-CONT109.                                                    
      *----------------------------------------------------------------*
                                                                        
           PERFORM 1520-INCONS67 THRU 1600-99-FIM.                      
                                                                        
      *----------------------------------------------------------------*
       0300-CONT110.                                                    
      *----------------------------------------------------------------*
                                                                        
      *     VALOR DA OPERACAO                                           
                                                                        
CPMCAC     IF  WRK-VALOP-R             NUMERIC                          
               GO TO 0310-CONT111                                       
           END-IF                                                       
                                                                        
           MOVE '*'                    TO WRK-DETVROPE                  
                                                                        
           PERFORM 1050-INCONS20 THRU 1600-99-FIM.                      
                                                                        
      *----------------------------------------------------------------*
       0310-CONT111.                                                    
      *----------------------------------------------------------------*
                                                                        
      *     VALOR CONTABIL                                              
                                                                        
CPMCAC     IF  WRK-VALRES-R            NOT NUMERIC                      
               GO TO 0320-CONT112                                       
           END-IF                                                       
                                                                        
CPMCAC     IF  WRK-VALRES              EQUAL WRK-ZEROSZ(1:15)           
               GO TO 0320-CONT112                                       
           END-IF                                                       
                                                                        
           GO TO 0330-CONT113.                                          
                                                                        
      *----------------------------------------------------------------*
       0320-CONT112.                                                    
      *----------------------------------------------------------------*
                                                                        
           MOVE '*'                    TO WRK-DETVRCON                  
                                                                        
           PERFORM 1060-INCONS21 THRU 1600-99-FIM.                      
                                                                        
      *----------------------------------------------------------------*
       0330-CONT113.                                                    
      *----------------------------------------------------------------*
                                                                        
      *     MOEDA CRUZEIRO                                              
                                                                        
           MOVE 'CR'                   TO WRK-MOEDA                     
                                                                        
      *     LOCAL                                                       
                                                                        
CPMCAC     IF  WRK-LOCAL               EQUAL WRK-ZEROSZ(1:2)            
               GO TO 0340-CONT114                                       
           END-IF                                                       
                                                                        
      *     LOCAL                                                       
                                                                        
CPMCAC     IF  WRK-LOCAL               EQUAL WRK-BRANCOS(1:2)           
               GO TO 0340-CONT114                                       
           END-IF                                                       
                                                                        
           GO TO 0350-CONT115.                                          
                                                                        
      *----------------------------------------------------------------*
       0340-CONT114.                                                    
      *----------------------------------------------------------------*
                                                                        
           MOVE 'F0'                   TO WRK-LOCAL.                    
                                                                        
      *----------------------------------------------------------------*
       0350-CONT115.                                                    
      *----------------------------------------------------------------*
                                                                        
           MOVE WRK-LOCAL              TO WRK-LOCAL01                   
                                                                        
           PERFORM 0800-PQLOCAL THRU 0830-99-FIM                        
                                                                        
           IF  WRK-LOCAL05             EQUAL 'S'                        
               GO TO 0360-CONT116                                       
           END-IF                                                       
                                                                        
           PERFORM 1080-INCONS23 THRU 1600-99-FIM.                      
                                                                        
      *----------------------------------------------------------------*
       0360-CONT116.                                                    
      *----------------------------------------------------------------*
                                                                        
      *     IDENTIFICACAO CONTAB.                                       
                                                                        
           IF  WRK-CONTAB              NOT EQUAL ' '                    
               GO TO 0370-CONT117                                       
           END-IF                                                       
                                                                        
           MOVE '0'                    TO WRK-CONTAB                    
                                                                        
           GO TO 0390-CONT119.                                          
                                                                        
      *----------------------------------------------------------------*
       0370-CONT117.                                                    
      *----------------------------------------------------------------*
                                                                        
           IF  WRK-CONTAB              EQUAL '*'                        
               GO TO 0390-CONT119                                       
           END-IF                                                       
                                                                        
           IF  WRK-CONTAB              LESS '0'                         
               GO TO 0380-CONT118                                       
           END-IF                                                       
                                                                        
           IF  WRK-CONTAB              GREATER '6'                      
               GO TO 0380-CONT118                                       
           END-IF                                                       
                                                                        
           GO TO 0390-CONT119.                                          
                                                                        
      *----------------------------------------------------------------*
       0380-CONT118.                                                    
      *----------------------------------------------------------------*
                                                                        
           PERFORM 1090-INCONS24 THRU 1600-99-FIM.                      
                                                                        
      *----------------------------------------------------------------*
       0390-CONT119.                                                    
      *----------------------------------------------------------------*
                                                                        
      *     TX. PUNITIVA                                                
                                                                        
           IF  WRK-TPUNIT              EQUAL ' '                        
               GO TO 0410-CONT121                                       
           END-IF                                                       
                                                                        
           IF  WRK-TPUNIT              EQUAL '1'                        
               GO TO 0400-CONT120                                       
           END-IF                                                       
                                                                        
           PERFORM 1100-INCONS25 THRU 1600-99-FIM.                      
                                                                        
      *----------------------------------------------------------------*
       0400-CONT120.                                                    
      *----------------------------------------------------------------*
                                                                        
      *     IDENTIFICACAO LP/CL                                         
                                                                        
           IF  WRK-ID                  EQUAL 'CL'                       
               GO TO 0410-CONT121                                       
           END-IF                                                       
                                                                        
           PERFORM 1110-INCONS26 THRU 1600-99-FIM.                      
                                                                        
      *----------------------------------------------------------------*
       0410-CONT121.                                                    
      *----------------------------------------------------------------*
                                                                        
      *     NOME DEVEDOR                                                
                                                                        
           IF  WRK-NOMED(1:1)          NOT EQUAL ' '                    
               GO TO 0440-CONT124                                       
           END-IF                                                       
                                                                        
BRQ=E******IF  WRK-ANOOP-R             NOT NUMERIC                      
BRQ=I      IF  WRK-ANOOP   EQUAL SPACES OR LOW-VALUES                   
               GO TO 0420-CONT122                                       
           END-IF                                                       
                                                                        
           IF  WRK-ANOOP               EQUAL '392'                      
               GO TO 0440-CONT124                                       
           END-IF                                                       
                                                                        
           IF  WRK-ANOOP               EQUAL '393'                      
               GO TO 0440-CONT124                                       
           END-IF                                                       
                                                                        
           IF  WRK-ANOOP               EQUAL '396'                      
               GO TO 0440-CONT124                                       
           END-IF                                                       
                                                                        
           IF  WRK-ANOOP               EQUAL '397'                      
               GO TO 0440-CONT124                                       
           END-IF                                                       
                                                                        
           IF  WRK-ANOOP               EQUAL '398'                      
               GO TO 0440-CONT124                                       
           END-IF                                                       
                                                                        
           IF  WRK-ANOOP               EQUAL '399'                      
               GO TO 0440-CONT124                                       
           END-IF                                                       
                                                                        
           GO TO 0430-CONT123.                                          
                                                                        
      *----------------------------------------------------------------*
       0420-CONT122.                                                    
      *----------------------------------------------------------------*
                                                                        
           PERFORM 1040-INCONS19 THRU 1600-99-FIM.                      
                                                                        
      *----------------------------------------------------------------*
       0430-CONT123.                                                    
      *----------------------------------------------------------------*
                                                                        
           PERFORM 1530-INCONS68 THRU 1600-99-FIM.                      
                                                                        
      *----------------------------------------------------------------*
       0440-CONT124.                                                    
      *----------------------------------------------------------------*
                                                                        
      *     CGC/CPF DEVEDOR                                             
                                                                        
CPMCAC     IF  WRK-CGC(1:13)           NOT EQUAL WRK-BRANCOS(1:13)      
               GO TO 0450-CONT125                                       
           END-IF                                                       
                                                                        
CPMCAC     MOVE WRK-ZEROSZ(1:13)       TO WRK-CGC(1:13)                 
                                                                        
           GO TO 0470-CONT127.                                          
                                                                        
      *----------------------------------------------------------------*
       0450-CONT125.                                                    
      *----------------------------------------------------------------*
                                                                        
CPMCAC     IF  WRK-CGC(1:13)           EQUAL WRK-ZEROSZ(1:13)           
               GO TO 0470-CONT127                                       
           END-IF                                                       
                                                                        
CPMCAC     MOVE WRK-CGC(1:13)          TO WRK-X-13                      
                                                                        
CPMCAC     IF  WRK-9-13                NOT NUMERIC                      
               GO TO 0460-CONT126                                       
           END-IF                                                       
                                                                        
           MOVE WRK-CGC(1:9)           TO WRK-NUMCGC                    
           MOVE WRK-CGC(10:4)          TO WRK-FILCGC                    
                                                                        
           PERFORM 0840-CALCDIG THRU 0840-99-FIM                        
                                                                        
           IF  WRK-DIGTAUX             NOT EQUAL WRK-CGC(14:2)          
               GO TO 0460-CONT126                                       
           END-IF                                                       
                                                                        
           GO TO 0470-CONT127.                                          
                                                                        
      *----------------------------------------------------------------*
       0460-CONT126.                                                    
      *----------------------------------------------------------------*
                                                                        
           PERFORM 1120-INCONS27 THRU 1600-99-FIM.                      
                                                                        
      *----------------------------------------------------------------*
       0470-CONT127.                                                    
      *----------------------------------------------------------------*
                                                                        
      *     COMPENSA                                                    
                                                                        
           IF  WRK-COMP1               EQUAL '1'                        
               GO TO 0480-CONT128                                       
           END-IF                                                       
                                                                        
           MOVE '0'                    TO WRK-COMP1.                    
                                                                        
      *----------------------------------------------------------------*
       0480-CONT128.                                                    
      *----------------------------------------------------------------*
                                                                        
      *     BLOQUEIO P. TRANS.                                          
                                                                        
           IF  WRK-BLOQTR              EQUAL 'B'                        
               GO TO 0490-CONT200                                       
           END-IF                                                       
                                                                        
           MOVE 'N'                    TO WRK-BLOQTR.                   
                                                                        
      *----------------------------------------------------------------*
       0490-CONT200.                                                    
      *----------------------------------------------------------------*
                                                                        
      *     NOME 1. AVALISTA                                            
                                                                        
CPMCAC     IF  WRK-COOB0               EQUAL WRK-BRANCOS(1:40)          
               GO TO 0500-CONT201                                       
           END-IF                                                       
                                                                        
           IF  WRK-COOB0(1:1)          NOT EQUAL ' '                    
               GO TO 0500-CONT201                                       
           END-IF                                                       
                                                                        
           PERFORM 1150-INCONS30 THRU 1600-99-FIM.                      
                                                                        
      *----------------------------------------------------------------*
       0500-CONT201.                                                    
      *----------------------------------------------------------------*
                                                                        
      *     CGC/CPF 1. AVALISTA                                         
                                                                        
CPMCAC     IF  WRK-CGC1(1:13)          NOT EQUAL WRK-BRANCOS(1:13)      
               GO TO 0510-CONT202                                       
           END-IF                                                       
                                                                        
CPMCAC     MOVE WRK-ZEROSZ(1:13)       TO WRK-CGC1(1:13).               
                                                                        
      *----------------------------------------------------------------*
       0510-CONT202.                                                    
      *----------------------------------------------------------------*
                                                                        
      *     CGC/CPF 1. AVALISTA                                         
                                                                        
CPMCAC     IF  WRK-CGC1(1:13)          EQUAL WRK-ZEROSZ(1:13)           
               GO TO 0520-CONT203                                       
           END-IF                                                       
                                                                        
           MOVE WRK-CGC1(1:9)          TO WRK-NUMCGC                    
           MOVE WRK-CGC1(10:4)         TO WRK-FILCGC                    
                                                                        
           PERFORM 0840-CALCDIG THRU 0840-99-FIM                        
                                                                        
           IF  WRK-DIGTAUX             EQUAL WRK-CGC1(14:2)             
               GO TO 0520-CONT203                                       
           END-IF                                                       
                                                                        
           PERFORM 1160-INCONS31 THRU 1600-99-FIM.                      
                                                                        
      *----------------------------------------------------------------*
       0520-CONT203.                                                    
      *----------------------------------------------------------------*
                                                                        
      *         NOME 2. AVALISTA                                        
                                                                        
CPMCAC     IF  WRK-COOB1               EQUAL WRK-BRANCOS(1:40)          
               GO TO 0530-CONT204                                       
           END-IF                                                       
                                                                        
           IF  WRK-COOB1(1:1)          NOT EQUAL ' '                    
               GO TO 0530-CONT204                                       
           END-IF                                                       
                                                                        
           PERFORM 1170-INCONS32 THRU 1600-99-FIM.                      
                                                                        
      *----------------------------------------------------------------*
       0530-CONT204.                                                    
      *----------------------------------------------------------------*
                                                                        
      *     CGC/CPF 2. AVALISTA                                         
                                                                        
CPMCAC     IF  WRK-CGC2(1:13)          NOT EQUAL WRK-BRANCOS(1:13)      
               GO TO 0540-CONT205                                       
           END-IF                                                       
                                                                        
CPMCAC     MOVE WRK-ZEROSZ(1:13)       TO WRK-CGC2(1:13).               
                                                                        
      *----------------------------------------------------------------*
       0540-CONT205.                                                    
      *----------------------------------------------------------------*
                                                                        
      *     CGC/CPF 2. AVALISTA                                         
                                                                        
CPMCAC     IF  WRK-CGC2(1:13)          EQUAL WRK-ZEROSZ(1:13)           
               GO TO 0550-CONT206                                       
           END-IF                                                       
                                                                        
           MOVE WRK-CGC2(1:9)          TO WRK-NUMCGC                    
           MOVE WRK-CGC2(10:4)         TO WRK-FILCGC                    
                                                                        
           PERFORM 0840-CALCDIG THRU 0840-99-FIM                        
                                                                        
           IF  WRK-DIGTAUX             EQUAL WRK-CGC2(14:2)             
               GO TO 0550-CONT206                                       
           END-IF                                                       
                                                                        
           PERFORM 1180-INCONS33 THRU 1600-99-FIM.                      
                                                                        
      *----------------------------------------------------------------*
       0550-CONT206.                                                    
      *----------------------------------------------------------------*
                                                                        
      *     VALOR BASE                                                  
                                                                        
CPMCAC     IF  WRK-VRBASE-R            NUMERIC                          
               GO TO 0560-CONT207                                       
           END-IF                                                       
                                                                        
      *     NOT NUMERIC                                                 
                                                                        
CPMCAC     MOVE WRK-ZEROSZ(1:15)       TO WRK-VRBASE.                   
                                                                        
      *----------------------------------------------------------------*
       0560-CONT207.                                                    
      *----------------------------------------------------------------*
                                                                        
      *     ZERANDO DT. EXPURGO                                         
                                                                        
CPMCAC     MOVE WRK-ZEROSZ(1:8)        TO  WRK-DTRECEXP                 
                                                                        
      *     TIPO DE GARANTIA                                            
                                                                        
           IF  WRK-TIPOGAR             LESS '00'                        
               GO TO 0570-CONT208                                       
           END-IF                                                       
                                                                        
           IF  WRK-TIPOGAR             NOT GREATER '99'                 
               GO TO 0580-CONT209                                       
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       0570-CONT208.                                                    
      *----------------------------------------------------------------*
                                                                        
           MOVE '99'                   TO WRK-TIPOGAR.                  
                                                                        
      *----------------------------------------------------------------*
       0580-CONT209.                                                    
      *----------------------------------------------------------------*
                                                                        
      *     TIPO DE PENDENCIA                                           
                                                                        
CPMCAC     IF  WRK-IDENT-R             NUMERIC                          
               GO TO 0590-CONT210                                       
           END-IF                                                       
                                                                        
           GO TO 0600-CONT211.                                          
                                                                        
      *----------------------------------------------------------------*
       0590-CONT210.                                                    
      *----------------------------------------------------------------*
                                                                        
CPMCAC     IF  WRK-IDENT               EQUAL WRK-ZEROSZ(1:4)            
               GO TO 0600-CONT211                                       
           END-IF                                                       
                                                                        
           PERFORM 1610-PESQTBCL THRU 1620-99-FIM                       
                                                                        
           IF RETURN-CODE              EQUAL 12                         
               GO TO 0600-CONT211                                       
           END-IF                                                       
                                                                        
           MOVE WRK-SIGLACL            TO WRK-SIGLA                     
                                                                        
CPMCAC     MOVE WRK-ENDRZCL            TO WRK-05-CS                     
CPMCAC     MOVE WRK-05-SS              TO WRK-RAZCL-R                   
CPMCAC     MOVE WRK-ENDCTCL            TO WRK-07-CS                     
CPMCAC     MOVE WRK-07-SS              TO WRK-CTACL-R                   
CPMCAC     MOVE WRK-ENCRZCL            TO WRK-05-CS                     
CPMCAC     MOVE WRK-05-SS              TO WRK-RAZAZOR-R                 
CPMCAC     MOVE WRK-ENCCTCL            TO WRK-07-CS                     
CPMCAC     MOVE WRK-07-SS              TO WRK-CTAOR-R                   
                                                                        
           GO TO 0610-CONT212.                                          
                                                                        
      *----------------------------------------------------------------*
       0600-CONT211.                                                    
      *----------------------------------------------------------------*
                                                                        
           PERFORM 1250-INCONS40 THRU 1600-99-FIM.                      
                                                                        
      *----------------------------------------------------------------*
       0610-CONT212.                                                    
      *----------------------------------------------------------------*
                                                                        
      *     DT VCT EFEITO TRANS                                         
                                                                        
CPMCAC     IF  WRK-DTTRANS             EQUAL WRK-ZEROSZ(1:8)            
               GO TO 0620-CONT300                                       
           END-IF                                                       
                                                                        
           MOVE WRK-DTTRANS            TO WRK-DATA06                    
                                                                        
           PERFORM 0750-DATA01 THRU 0790-99-FIM                         
                                                                        
           IF  WRK-DATA05              EQUAL 'S'                        
               GO TO 0620-CONT300                                       
           END-IF                                                       
                                                                        
           PERFORM 1270-INCONS42 THRU 1600-99-FIM.                      
                                                                        
      *----------------------------------------------------------------*
       0620-CONT300.                                                    
      *----------------------------------------------------------------*
                                                                        
      ******************************************************************
      * EXCLUSAO DA CONSISTENCIA DO CODIGO E NOME DO GERENTE - 06/06/97*
      ******************************************************************
                                                                        
      *     VR. ENC. VENCIDOS                                           
                                                                        
CPMCAC     IF  WRK-VALVENC-R           NUMERIC                          
               GO TO 0630-CONT303                                       
           END-IF                                                       
                                                                        
           MOVE '*'                    TO WRK-DETVRVEN                  
                                                                        
           PERFORM 1320-INCONS47 THRU 1600-99-FIM.                      
                                                                        
      *----------------------------------------------------------------*
       0630-CONT303.                                                    
      *----------------------------------------------------------------*
                                                                        
      *     VR. ENC. VINCENDOS                                          
                                                                        
CPMCAC     IF  WRK-VALVINC-R           NUMERIC                          
               GO TO 0640-CONT304                                       
           END-IF                                                       
                                                                        
           MOVE '*'                    TO WRK-DETVRVIN                  
                                                                        
           PERFORM 1410-INCONS56 THRU 1600-99-FIM.                      
                                                                        
      *----------------------------------------------------------------*
       0640-CONT304.                                                    
      *----------------------------------------------------------------*
                                                                        
      *     VR. DEV. INICIAL                                            
                                                                        
CPMCAC     IF  WRK-VALINIC-R           NUMERIC                          
               GO TO 0650-CONT305                                       
           END-IF                                                       
                                                                        
           MOVE '*'                    TO WRK-DETVRINI                  
                                                                        
           PERFORM 1330-INCONS48 THRU 1600-99-FIM.                      
                                                                        
      *----------------------------------------------------------------*
       0650-CONT305.                                                    
      *----------------------------------------------------------------*
                                                                        
      *     VR. IOF                                                     
                                                                        
CPMCAC     IF  WRK-VALORIOF-R          NUMERIC                          
               GO TO 0660-CONT306                                       
           END-IF                                                       
                                                                        
           MOVE '*'                    TO WRK-DETVRIOF                  
                                                                        
           PERFORM 1340-INCONS49 THRU 1600-99-FIM.                      
                                                                        
      *----------------------------------------------------------------*
       0660-CONT306.                                                    
      *----------------------------------------------------------------*
                                                                        
      *     * AGENCIA PARA RESP                                         
                                                                        
CPMCAC     IF  WRK-AGRESP-R            NOT NUMERIC                      
               GO TO 0670-CONT307                                       
           END-IF                                                       
                                                                        
CPMCAC     IF  WRK-AGRESP              GREATER WRK-ZEROSZ(1:4)          
               GO TO 0680-CONT308                                       
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       0670-CONT307.                                                    
      *----------------------------------------------------------------*
                                                                        
           PERFORM 1570-INCONS72 THRU 1600-99-FIM.                      
                                                                        
      *----------------------------------------------------------------*
       0680-CONT308.                                                    
      *----------------------------------------------------------------*
                                                                        
CPMCAC     CONTINUE.                                                    
                                                                        
       0680-99-FIM.                    EXIT.                            
                                                                        
      *----------------------------------------------------------------*
      * ROTINA DE LEITURA DO MOVIMENTO                                 *
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       0690-LERMOVTO.                                                   
      *----------------------------------------------------------------*
                                                                        
           READ MOVTO INTO             WRK-REGMOVTO END-READ            
                                                                        
           MOVE WRK-LEITURA            TO WRK-OPERACAO                  
                                                                        
           IF  WRK-FS-MOVTO            EQUAL '10'                       
               GO TO 1810-FIMPGM                                        
           ELSE                                                         
               PERFORM 9040-TESTAR-FS-MOVTO THRU 9040-99-FIM            
           END-IF                                                       
                                                                        
           ADD 1                       TO WRK-QTDLIDO                   
                                                                        
      *     GUARDA REG. ORIGINAL                                        
      *     PARA SER USADO EM CASO                                      
      *     DE INCONSISTENCIA.                                          
                                                                        
           MOVE WRK-REGMOVTO(1:250)    TO WRK-WKMOVTO(1:250)            
           MOVE WRK-REGMOVTO(251:250)  TO WRK-WKMOVTO(251:250)          
           MOVE WRK-REGMOVTO(501:75)   TO WRK-WKMOVTO(501:75)           
           MOVE ZEROS                  TO WRK-QTDERROS.                 
                                                                        
      *----------------------------------------------------------------*
       0700-CONSEMPR.                                                   
      *----------------------------------------------------------------*
                                                                        
      *     EMPRESA NAO NUMERICA                                        
                                                                        
CPMCAC     IF  WRK-FIRMA-R             NOT NUMERIC                      
               GO TO 0710-EMPRINCO                                      
           END-IF                                                       
                                                                        
      *     EMPRESA INVALIDA                                            
                                                                        
           IF  WRK-FIRMA               NOT GREATER '04000'              
               GO TO 0710-EMPRINCO                                      
           END-IF                                                       
                                                                        
           GO TO 0720-CONSAGEN.                                         
                                                                        
      *----------------------------------------------------------------*
       0710-EMPRINCO.                                                   
      *----------------------------------------------------------------*
                                                                        
           PERFORM 0860-INCONS01 THRU 1600-99-FIM                       
                                                                        
           GO TO 0720-CONSAGEN.                                         
                                                                        
      *----------------------------------------------------------------*
       0720-CONSAGEN.                                                   
      *----------------------------------------------------------------*
                                                                        
      *     AGENCIA NAO NUMERICA                                        
                                                                        
CPMCAC     IF  WRK-AGENCIA-R           NOT NUMERIC                      
               GO TO 0730-AGENINCO                                      
           END-IF                                                       
                                                                        
      *     AGENCIA ZERADA                                              
                                                                        
CPMCAC     IF  WRK-AGENCIA             EQUAL WRK-ZEROSZ(1:5)            
               GO TO 0730-AGENINCO                                      
           END-IF                                                       
                                                                        
      *     AGENCIA INVALIDA                                            
                                                                        
           IF  WRK-AGENCIA             GREATER '09999'                  
               GO TO 0730-AGENINCO                                      
           END-IF                                                       
                                                                        
           GO TO 0740-FIMLER.                                           
                                                                        
      *----------------------------------------------------------------*
       0730-AGENINCO.                                                   
      *----------------------------------------------------------------*
                                                                        
           PERFORM 0870-INCONS02 THRU 1600-99-FIM.                      
                                                                        
      *----------------------------------------------------------------*
       0740-FIMLER.                                                     
      *----------------------------------------------------------------*
                                                                        
CPMCAC     CONTINUE.                                                    
                                                                        
       0740-99-FIM.                    EXIT.                            
                                                                        
      *-----------------------------------------------------------------
      * ROTINA DE CONSISTENCIA DE DATA                                  
      *-----------------------------------------------------------------
                                                                        
      *----------------------------------------------------------------*
       0750-DATA01.                                                     
      *----------------------------------------------------------------*
                                                                        
      *     ANO INVALIDO                                                
                                                                        
           IF  WRK-DATA09              LESS '1970'                      
               GO TO 0770-DATA02                                        
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       0760-DATA00.                                                     
      *----------------------------------------------------------------*
                                                                        
      *     DATA NAO NUMERICA                                           
                                                                        
CPMCAC     IF  WRK-DATA06-R            NOT NUMERIC                      
               GO TO 0770-DATA02                                        
           END-IF                                                       
                                                                        
      *     ANO                                                         
                                                                        
           MOVE WRK-DATA09             TO WRK-AUXDATA(1:4)              
                                                                        
      *     MES                                                         
                                                                        
           MOVE WRK-DATA08             TO WRK-AUXDATA(5:2)              
                                                                        
      *     DIA                                                         
                                                                        
           MOVE WRK-DATA07             TO WRK-AUXDATA(7:2)              
           MOVE WRK-AUXDATA-REDCPMACT  TO WRK-DTENVIAD                  
                                                                        
CPMCAC     MOVE 1                      TO WRK-CLLP8400-TAMANHO          
CPMCAC     MOVE 'OI'                   TO WRK-CLLP8400-OPERACAO         
CPMCAC     MOVE X'0F'                  TO WRK-CLLP8400-MASCARA1         
CPMCAC     MOVE WRK-DTENVIAD-R(5:1)    TO WRK-CLLP8400-OPERANDO1        
                                                                        
CPMCAC     CALL WRK-CLLP8400           USING WRK-CLLP8400-TAMANHO       
CPMCAC                                       WRK-CLLP8400-OPERACAO      
CPMCAC                                       WRK-CLLP8400-OPERANDO1     
CPMCAC                                       WRK-CLLP8400-MASCARA1      
                                                                        
CPMCAC     MOVE WRK-CLLP8400-OPERANDO1 TO WRK-DTENVIAD-R(5:1)           
                                                                        
           CALL 'BRAD1205'             USING WRK-LSTCAMPO               
                                             WRK-MENSAGEM               
                                                                        
           IF  RETURN-CODE             EQUAL ZEROS                      
               GO TO 0780-DATA03                                        
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       0770-DATA02.                                                     
      *----------------------------------------------------------------*
                                                                        
      *     DATA INVALIDA                                               
                                                                        
           MOVE 'N'                    TO WRK-DATA05                    
                                                                        
           GO TO 0790-DATA04.                                           
                                                                        
      *----------------------------------------------------------------*
       0780-DATA03.                                                     
      *----------------------------------------------------------------*
                                                                        
      *     DATA VALIDA                                                 
                                                                        
           MOVE 'S'                    TO WRK-DATA05.                   
                                                                        
      *----------------------------------------------------------------*
       0790-DATA04.                                                     
      *----------------------------------------------------------------*
                                                                        
      *     RETORNE                                                     
                                                                        
CPMCAC      CONTINUE.                                                   
                                                                        
       0790-99-FIM.                    EXIT.                            
                                                                        
      *----------------------------------------------------------------*
      * ROTINA DE PESQUISA NA TABELA DE ESCRITORIOS                    *
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       0800-PQLOCAL.                                                    
      *----------------------------------------------------------------*
                                                                        
           MOVE 1            TO IND-TABCL4.                             
                                                                        
      *----------------------------------------------------------------*
       0810-LOCAL02.                                                    
      *----------------------------------------------------------------*
                                                                        
           IF  WRK-LOCAL01             EQUAL WRK-TABCL4(IND-TABCL4:2)   
               GO TO 0820-LOCAL03                                       
           END-IF                                                       
                                                                        
           IF  WRK-LOCAL01             EQUAL                            
                                       WRK-TABCL4(IND-TABCL4 + 2:2)     
               GO TO 0820-LOCAL03                                       
           END-IF                                                       
                                                                        
           ADD 34                      TO IND-TABCL4                    
                                                                        
           IF  WRK-TABCL4(IND-TABCL4:2)                                 
                                       EQUAL '**'                       
               GO TO 0830-LOCAL04                                       
           END-IF                                                       
                                                                        
           GO TO 0810-LOCAL02.                                          
                                                                        
      *----------------------------------------------------------------*
       0820-LOCAL03.                                                    
      *----------------------------------------------------------------*
                                                                        
           MOVE 'S'                    TO WRK-LOCAL05                   
                                                                        
           GO TO 0830-99-FIM.                                           
                                                                        
      *----------------------------------------------------------------*
       0830-LOCAL04.                                                    
      *----------------------------------------------------------------*
                                                                        
           MOVE 'N'                    TO WRK-LOCAL05.                  
                                                                        
       0830-99-FIM.                    EXIT.                            
                                                                        
      ******************************************************************
      * ROTINA DE CALCULO DO DIGITO CGC                                *
      ******************************************************************
                                                                        
      *----------------------------------------------------------------*
       0840-CALCDIG.                                                    
      *----------------------------------------------------------------*
                                                                        
           MOVE '0'                    TO WRK-FILZERO                   
                                                                        
           CALL 'BRAD0110'             USING WRK-CGCAUX                 
                                             WRK-DIGTAUX.               
                                                                        
       0840-99-FIM.                    EXIT.                            
                                                                        
      ******************************************************************
      * GRAVACAO DO ARQUIVO DE SAIDA                                   *
      ******************************************************************
                                                                        
      *----------------------------------------------------------------*
       0850-GRAVAOK.                                                    
      *----------------------------------------------------------------*
                                                                        
           MOVE WRK-REGMOVTO(1:250)    TO WRK-ACERAUX(1:250)            
           MOVE WRK-REGMOVTO(251:250)  TO WRK-ACERAUX(251:250)          
           MOVE WRK-REGMOVTO(501:75)   TO WRK-ACERAUX(501:75)           
           MOVE '1'                    TO WRK-ACERTOS                   
           MOVE '1'                    TO WRK-ACTIPO                    
           MOVE 'N'                    TO WRK-ACDEBCC                   
                                                                        
           WRITE FD-REG-CERTO          FROM WRK-ACERAUX                 
                                                                        
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO                  
                                                                        
           PERFORM 9050-TESTAR-FS-CERTO THRU 9050-99-FIM.               
                                                                        
       0850-99-FIM.                    EXIT.                            
                                                                        
      ******************************************************************
      *        MOVIMENTO DOS ERROS                                     *
      ******************************************************************
                                                                        
      *----------------------------------------------------------------*
       0860-INCONS01.                                                   
      *----------------------------------------------------------------*
                                                                        
           MOVE '01 '                  TO WRK-TABINCON(IND-TABINCON:3)  
                                                                        
           GO TO 1590-INCONS.                                           
                                                                        
      *----------------------------------------------------------------*
       0870-INCONS02.                                                   
      *----------------------------------------------------------------*
                                                                        
           MOVE '02 '                  TO WRK-TABINCON(IND-TABINCON:3)  
                                                                        
           GO TO 1590-INCONS.                                           
                                                                        
      *----------------------------------------------------------------*
       0880-INCONS03.                                                   
      *----------------------------------------------------------------*
                                                                        
           MOVE '03 '                  TO WRK-TABINCON(IND-TABINCON:3)  
                                                                        
           GO TO 1590-INCONS.                                           
                                                                        
      *----------------------------------------------------------------*
       0890-INCONS04.                                                   
      *----------------------------------------------------------------*
                                                                        
           MOVE '04 '                  TO WRK-TABINCON(IND-TABINCON:3)  
                                                                        
           GO TO 1590-INCONS.                                           
                                                                        
      *----------------------------------------------------------------*
       0900-INCONS05.                                                   
      *----------------------------------------------------------------*
                                                                        
           MOVE '05 '                  TO WRK-TABINCON(IND-TABINCON:3)  
                                                                        
           GO TO 1590-INCONS.                                           
                                                                        
      *----------------------------------------------------------------*
       0910-INCONS06.                                                   
      *----------------------------------------------------------------*
                                                                        
           MOVE '06 '                  TO WRK-TABINCON(IND-TABINCON:3)  
                                                                        
           GO TO 1590-INCONS.                                           
                                                                        
      *----------------------------------------------------------------*
       0920-INCONS07.                                                   
      *----------------------------------------------------------------*
                                                                        
           MOVE '07 '                  TO WRK-TABINCON(IND-TABINCON:3)  
                                                                        
           GO TO 1590-INCONS.                                           
                                                                        
      *----------------------------------------------------------------*
       0930-INCONS08.                                                   
      *----------------------------------------------------------------*
                                                                        
           MOVE '08 '                  TO WRK-TABINCON(IND-TABINCON:3)  
                                                                        
           GO TO 1590-INCONS.                                           
                                                                        
      *----------------------------------------------------------------*
       0940-INCONS09.                                                   
      *----------------------------------------------------------------*
                                                                        
           MOVE '09 '                  TO WRK-TABINCON(IND-TABINCON:3)  
                                                                        
           GO TO 1590-INCONS.                                           
                                                                        
      *----------------------------------------------------------------*
       0950-INCONS10.                                                   
      *----------------------------------------------------------------*
                                                                        
           MOVE '10 '                  TO WRK-TABINCON(IND-TABINCON:3)  
                                                                        
           GO TO 1590-INCONS.                                           
                                                                        
      *----------------------------------------------------------------*
       0960-INCONS11.                                                   
      *----------------------------------------------------------------*
                                                                        
           MOVE '11 '                  TO WRK-TABINCON(IND-TABINCON:3)  
                                                                        
           GO TO 1590-INCONS.                                           
                                                                        
      *----------------------------------------------------------------*
       0970-INCONS12.                                                   
      *----------------------------------------------------------------*
                                                                        
           MOVE '12 '                  TO WRK-TABINCON(IND-TABINCON:3)  
                                                                        
           GO TO 1590-INCONS.                                           
                                                                        
      *----------------------------------------------------------------*
       0980-INCONS13.                                                   
      *----------------------------------------------------------------*
                                                                        
           MOVE '13 '                  TO WRK-TABINCON(IND-TABINCON:3)  
                                                                        
           GO TO 1590-INCONS.                                           
                                                                        
      *----------------------------------------------------------------*
       0990-INCONS14.                                                   
      *----------------------------------------------------------------*
                                                                        
           MOVE '14 '                  TO WRK-TABINCON(IND-TABINCON:3)  
                                                                        
           GO TO 1590-INCONS.                                           
                                                                        
      *----------------------------------------------------------------*
       1000-INCONS15.                                                   
      *----------------------------------------------------------------*
                                                                        
           MOVE '15 '                  TO WRK-TABINCON(IND-TABINCON:3)  
                                                                        
           GO TO 1590-INCONS.                                           
                                                                        
      *----------------------------------------------------------------*
       1010-INCONS16.                                                   
      *----------------------------------------------------------------*
                                                                        
           MOVE '16 '                  TO WRK-TABINCON(IND-TABINCON:3)  
                                                                        
           GO TO 1590-INCONS.                                           
                                                                        
      *----------------------------------------------------------------*
       1020-INCONS17.                                                   
      *----------------------------------------------------------------*
                                                                        
           MOVE '17 '                  TO WRK-TABINCON(IND-TABINCON:3)  
                                                                        
           GO TO 1590-INCONS.                                           
                                                                        
      *----------------------------------------------------------------*
       1030-INCONS18.                                                   
      *----------------------------------------------------------------*
                                                                        
           MOVE '18 '                  TO WRK-TABINCON(IND-TABINCON:3)  
                                                                        
           GO TO 1590-INCONS.                                           
                                                                        
      *----------------------------------------------------------------*
       1040-INCONS19.                                                   
      *----------------------------------------------------------------*
                                                                        
           MOVE '19 '                  TO WRK-TABINCON(IND-TABINCON:3)  
                                                                        
           GO TO 1590-INCONS.                                           
                                                                        
      *----------------------------------------------------------------*
       1050-INCONS20.                                                   
      *----------------------------------------------------------------*
                                                                        
           MOVE '20 '                  TO WRK-TABINCON(IND-TABINCON:3)  
                                                                        
           GO TO 1590-INCONS.                                           
                                                                        
      *----------------------------------------------------------------*
       1060-INCONS21.                                                   
      *----------------------------------------------------------------*
                                                                        
           MOVE '21 '                  TO WRK-TABINCON(IND-TABINCON:3)  
                                                                        
           GO TO 1590-INCONS.                                           
                                                                        
      *----------------------------------------------------------------*
       1070-INCONS22.                                                   
      *----------------------------------------------------------------*
                                                                        
           MOVE '22 '                  TO WRK-TABINCON(IND-TABINCON:3)  
                                                                        
           GO TO 1590-INCONS.                                           
                                                                        
      *----------------------------------------------------------------*
       1080-INCONS23.                                                   
      *----------------------------------------------------------------*
                                                                        
           MOVE '23 '                  TO WRK-TABINCON(IND-TABINCON:3)  
                                                                        
           GO TO 1590-INCONS.                                           
                                                                        
      *----------------------------------------------------------------*
       1090-INCONS24.                                                   
      *----------------------------------------------------------------*
                                                                        
           MOVE '24 '                  TO WRK-TABINCON(IND-TABINCON:3)  
                                                                        
           GO TO 1590-INCONS.                                           
                                                                        
      *----------------------------------------------------------------*
       1100-INCONS25.                                                   
      *----------------------------------------------------------------*
                                                                        
           MOVE '25 '                  TO WRK-TABINCON(IND-TABINCON:3)  
                                                                        
           GO TO 1590-INCONS.                                           
                                                                        
      *----------------------------------------------------------------*
       1110-INCONS26.                                                   
      *----------------------------------------------------------------*
                                                                        
           MOVE '26 '                  TO WRK-TABINCON(IND-TABINCON:3)  
                                                                        
           GO TO 1590-INCONS.                                           
                                                                        
      *----------------------------------------------------------------*
       1120-INCONS27.                                                   
      *----------------------------------------------------------------*
                                                                        
           MOVE '27 '                  TO WRK-TABINCON(IND-TABINCON:3)  
                                                                        
           GO TO 1590-INCONS.                                           
                                                                        
      *----------------------------------------------------------------*
       1130-INCONS28.                                                   
      *----------------------------------------------------------------*
                                                                        
           MOVE '28 '                  TO WRK-TABINCON(IND-TABINCON:3)  
                                                                        
           GO TO 1590-INCONS.                                           
                                                                        
      *----------------------------------------------------------------*
       1140-INCONS29.                                                   
      *----------------------------------------------------------------*
                                                                        
           MOVE '29 '                  TO WRK-TABINCON(IND-TABINCON:3)  
                                                                        
           GO TO 1590-INCONS.                                           
                                                                        
      *----------------------------------------------------------------*
       1150-INCONS30.                                                   
      *----------------------------------------------------------------*
                                                                        
           MOVE '30 '                  TO WRK-TABINCON(IND-TABINCON:3)  
                                                                        
           GO TO 1590-INCONS.                                           
                                                                        
      *----------------------------------------------------------------*
       1160-INCONS31.                                                   
      *----------------------------------------------------------------*
                                                                        
           MOVE '31 '                  TO WRK-TABINCON(IND-TABINCON:3)  
                                                                        
           GO TO 1590-INCONS.                                           
                                                                        
      *----------------------------------------------------------------*
       1170-INCONS32.                                                   
      *----------------------------------------------------------------*
                                                                        
           MOVE '32 '                  TO WRK-TABINCON(IND-TABINCON:3)  
                                                                        
           GO TO 1590-INCONS.                                           
                                                                        
      *----------------------------------------------------------------*
       1180-INCONS33.                                                   
      *----------------------------------------------------------------*
                                                                        
           MOVE '33 '                  TO WRK-TABINCON(IND-TABINCON:3)  
                                                                        
           GO TO 1590-INCONS.                                           
                                                                        
      *----------------------------------------------------------------*
       1190-INCONS34.                                                   
      *----------------------------------------------------------------*
                                                                        
           MOVE '34 '                  TO WRK-TABINCON(IND-TABINCON:3)  
                                                                        
           GO TO 1590-INCONS.                                           
                                                                        
      *----------------------------------------------------------------*
       1200-INCONS35.                                                   
      *----------------------------------------------------------------*
                                                                        
           MOVE '35 '                  TO WRK-TABINCON(IND-TABINCON:3)  
                                                                        
           GO TO 1590-INCONS.                                           
                                                                        
      *----------------------------------------------------------------*
       1210-INCONS36.                                                   
      *----------------------------------------------------------------*
                                                                        
           MOVE '36 '                  TO WRK-TABINCON(IND-TABINCON:3)  
                                                                        
           GO TO 1590-INCONS.                                           
                                                                        
      *----------------------------------------------------------------*
       1220-INCONS37.                                                   
      *----------------------------------------------------------------*
                                                                        
           MOVE '37 '                  TO WRK-TABINCON(IND-TABINCON:3)  
                                                                        
           GO TO 1590-INCONS.                                           
                                                                        
      *----------------------------------------------------------------*
       1230-INCONS38.                                                   
      *----------------------------------------------------------------*
                                                                        
           MOVE '38 '                  TO WRK-TABINCON(IND-TABINCON:3)  
                                                                        
           GO TO 1590-INCONS.                                           
                                                                        
      *----------------------------------------------------------------*
       1240-INCONS39.                                                   
      *----------------------------------------------------------------*
                                                                        
           MOVE '39 '                  TO WRK-TABINCON(IND-TABINCON:3)  
                                                                        
           GO TO 1590-INCONS.                                           
                                                                        
      *----------------------------------------------------------------*
       1250-INCONS40.                                                   
      *----------------------------------------------------------------*
                                                                        
           MOVE '40 '                  TO WRK-TABINCON(IND-TABINCON:3)  
                                                                        
           GO TO 1590-INCONS.                                           
                                                                        
      *----------------------------------------------------------------*
       1260-INCONS41.                                                   
      *----------------------------------------------------------------*
                                                                        
           MOVE '41 '                  TO WRK-TABINCON(IND-TABINCON:3)  
                                                                        
           GO TO 1590-INCONS.                                           
                                                                        
      *----------------------------------------------------------------*
       1270-INCONS42.                                                   
      *----------------------------------------------------------------*
                                                                        
           MOVE '42 '                  TO WRK-TABINCON(IND-TABINCON:3)  
                                                                        
           GO TO 1590-INCONS.                                           
                                                                        
      *----------------------------------------------------------------*
       1280-INCONS43.                                                   
      *----------------------------------------------------------------*
                                                                        
           MOVE '43 '                  TO WRK-TABINCON(IND-TABINCON:3)  
                                                                        
           GO TO 1590-INCONS.                                           
                                                                        
      *----------------------------------------------------------------*
       1290-INCONS44.                                                   
      *----------------------------------------------------------------*
                                                                        
           MOVE '44 '                  TO WRK-TABINCON(IND-TABINCON:3)  
                                                                        
           GO TO 1590-INCONS.                                           
                                                                        
      *----------------------------------------------------------------*
       1300-INCONS45.                                                   
      *----------------------------------------------------------------*
                                                                        
           MOVE '45 '                  TO WRK-TABINCON(IND-TABINCON:3)  
                                                                        
           GO TO 1590-INCONS.                                           
                                                                        
      *----------------------------------------------------------------*
       1310-INCONS46.                                                   
      *----------------------------------------------------------------*
                                                                        
           MOVE '46 '                  TO WRK-TABINCON(IND-TABINCON:3)  
                                                                        
           GO TO 1590-INCONS.                                           
                                                                        
      *----------------------------------------------------------------*
       1320-INCONS47.                                                   
      *----------------------------------------------------------------*
                                                                        
           MOVE '47 '                  TO WRK-TABINCON(IND-TABINCON:3)  
                                                                        
           GO TO 1590-INCONS.                                           
                                                                        
      *----------------------------------------------------------------*
       1330-INCONS48.                                                   
      *----------------------------------------------------------------*
                                                                        
           MOVE '48 '                  TO WRK-TABINCON(IND-TABINCON:3)  
                                                                        
           GO TO 1590-INCONS.                                           
                                                                        
      *----------------------------------------------------------------*
       1340-INCONS49.                                                   
      *----------------------------------------------------------------*
                                                                        
           MOVE '49 '                  TO WRK-TABINCON(IND-TABINCON:3)  
                                                                        
           GO TO 1590-INCONS.                                           
                                                                        
      *----------------------------------------------------------------*
       1350-INCONS50.                                                   
      *----------------------------------------------------------------*
                                                                        
           MOVE '50 '                  TO WRK-TABINCON(IND-TABINCON:3)  
                                                                        
           GO TO 1590-INCONS.                                           
                                                                        
      *----------------------------------------------------------------*
       1360-INCONS51.                                                   
      *----------------------------------------------------------------*
                                                                        
           MOVE '51 '                  TO WRK-TABINCON(IND-TABINCON:3)  
                                                                        
           GO TO 1590-INCONS.                                           
                                                                        
      *----------------------------------------------------------------*
       1370-INCONS52.                                                   
      *----------------------------------------------------------------*
                                                                        
           MOVE '52 '                  TO WRK-TABINCON(IND-TABINCON:3)  
                                                                        
           GO TO 1590-INCONS.                                           
                                                                        
      *----------------------------------------------------------------*
       1380-INCONS53.                                                   
      *----------------------------------------------------------------*
                                                                        
           MOVE '53 '                  TO WRK-TABINCON(IND-TABINCON:3)  
                                                                        
           GO TO 1590-INCONS.                                           
                                                                        
      *----------------------------------------------------------------*
       1390-INCONS54.                                                   
      *----------------------------------------------------------------*
                                                                        
           MOVE '54 '                  TO WRK-TABINCON(IND-TABINCON:3)  
                                                                        
           GO TO 1590-INCONS.                                           
                                                                        
      *----------------------------------------------------------------*
       1400-INCONS55.                                                   
      *----------------------------------------------------------------*
                                                                        
           MOVE '55 '                  TO WRK-TABINCON(IND-TABINCON:3)  
                                                                        
           GO TO 1590-INCONS.                                           
                                                                        
      *----------------------------------------------------------------*
       1410-INCONS56.                                                   
      *----------------------------------------------------------------*
                                                                        
           MOVE '56 '                  TO WRK-TABINCON(IND-TABINCON:3)  
                                                                        
           GO TO 1590-INCONS.                                           
                                                                        
      *----------------------------------------------------------------*
       1420-INCONS57.                                                   
      *----------------------------------------------------------------*
                                                                        
           MOVE '57 '                  TO WRK-TABINCON(IND-TABINCON:3)  
                                                                        
           GO TO 1590-INCONS.                                           
                                                                        
      *----------------------------------------------------------------*
       1430-INCONS58.                                                   
      *----------------------------------------------------------------*
                                                                        
           MOVE '58 '                  TO WRK-TABINCON(IND-TABINCON:3)  
                                                                        
           GO TO 1590-INCONS.                                           
                                                                        
      *----------------------------------------------------------------*
       1440-INCONS59.                                                   
      *----------------------------------------------------------------*
                                                                        
           MOVE '59 '                  TO WRK-TABINCON(IND-TABINCON:3)  
                                                                        
           GO TO 1590-INCONS.                                           
                                                                        
      *----------------------------------------------------------------*
       1450-INCONS60.                                                   
      *----------------------------------------------------------------*
                                                                        
           MOVE '60 '                  TO WRK-TABINCON(IND-TABINCON:3)  
                                                                        
           GO TO 1590-INCONS.                                           
                                                                        
      *----------------------------------------------------------------*
       1460-INCONS61.                                                   
      *----------------------------------------------------------------*
                                                                        
           MOVE '61 '                  TO WRK-TABINCON(IND-TABINCON:3)  
                                                                        
           GO TO 1590-INCONS.                                           
                                                                        
      *----------------------------------------------------------------*
       1470-INCONS62.                                                   
      *----------------------------------------------------------------*
                                                                        
           MOVE '62 '                  TO WRK-TABINCON(IND-TABINCON:3)  
                                                                        
           GO TO 1590-INCONS.                                           
                                                                        
      *----------------------------------------------------------------*
       1480-INCONS63.                                                   
      *----------------------------------------------------------------*
                                                                        
           MOVE '63 '                  TO WRK-TABINCON(IND-TABINCON:3)  
                                                                        
           GO TO 1590-INCONS.                                           
                                                                        
      *----------------------------------------------------------------*
       1490-INCONS64.                                                   
      *----------------------------------------------------------------*
                                                                        
           MOVE '64 '                  TO WRK-TABINCON(IND-TABINCON:3)  
                                                                        
           GO TO 1590-INCONS.                                           
                                                                        
      *----------------------------------------------------------------*
       1500-INCONS65.                                                   
      *----------------------------------------------------------------*
                                                                        
           MOVE '65 '                  TO WRK-TABINCON(IND-TABINCON:3)  
                                                                        
           GO TO 1590-INCONS.                                           
                                                                        
      *----------------------------------------------------------------*
       1510-INCONS66.                                                   
      *----------------------------------------------------------------*
                                                                        
           MOVE '66 '                  TO WRK-TABINCON(IND-TABINCON:3)  
                                                                        
           GO TO 1590-INCONS.                                           
                                                                        
      *----------------------------------------------------------------*
       1520-INCONS67.                                                   
      *----------------------------------------------------------------*
                                                                        
           MOVE '67 '                  TO WRK-TABINCON(IND-TABINCON:3)  
                                                                        
           GO TO 1590-INCONS.                                           
                                                                        
      *----------------------------------------------------------------*
       1530-INCONS68.                                                   
      *----------------------------------------------------------------*
                                                                        
           MOVE '68 '                  TO WRK-TABINCON(IND-TABINCON:3)  
                                                                        
           GO TO 1590-INCONS.                                           
                                                                        
      *----------------------------------------------------------------*
       1540-INCONS69.                                                   
      *----------------------------------------------------------------*
                                                                        
           MOVE '69 '                  TO WRK-TABINCON(IND-TABINCON:3)  
                                                                        
           GO TO 1590-INCONS.                                           
                                                                        
      *----------------------------------------------------------------*
       1550-INCONS70.                                                   
      *----------------------------------------------------------------*
                                                                        
           MOVE '70 '                  TO WRK-TABINCON(IND-TABINCON:3)  
                                                                        
           GO TO 1590-INCONS.                                           
                                                                        
      *----------------------------------------------------------------*
       1560-INCONS71.                                                   
      *----------------------------------------------------------------*
                                                                        
           MOVE '71 '                  TO WRK-TABINCON(IND-TABINCON:3)  
                                                                        
           GO TO 1590-INCONS.                                           
                                                                        
      *----------------------------------------------------------------*
       1570-INCONS72.                                                   
      *----------------------------------------------------------------*
                                                                        
           MOVE '72 '                  TO WRK-TABINCON(IND-TABINCON:3)  
                                                                        
           GO TO 1590-INCONS.                                           
                                                                        
      *----------------------------------------------------------------*
       1580-INCONS99.                                                   
      *----------------------------------------------------------------*
                                                                        
           MOVE '99 '                  TO WRK-TABINCON(IND-TABINCON:3). 
                                                                        
      *----------------------------------------------------------------*
       1590-INCONS.                                                     
      *----------------------------------------------------------------*
                                                                        
           ADD 1                       TO WRK-QTDERROS                  
                                                                        
           IF  WRK-QTDERROS            GREATER 8                        
               GO TO 1600-NOVERRO                                       
           END-IF                                                       
                                                                        
           ADD 3                       TO IND-TABINCON                  
                                                                        
           GO TO 1600-99-FIM.                                           
                                                                        
      *----------------------------------------------------------------*
       1600-NOVERRO.                                                    
      *----------------------------------------------------------------*
                                                                        
           MOVE '99'                   TO WRK-TABINCON(IND-TABINCON:2). 
                                                                        
       1600-99-FIM.                    EXIT.                            
                                                                        
      *----------------------------------------------------------------*
       1610-PESQTBCL.                                                   
      *----------------------------------------------------------------*
                                                                        
CPMCAC     MOVE WRK-FIRMA-R            TO WRK-AGAUX                     
CPMCAC     MOVE WRK-IDENT-R            TO WRK-IDENTAUX                  
           MOVE WRK-ANOOP              TO WRK-CARTAUX                   
                                                                        
CPMCAC     MOVE 6                      TO WRK-CLLP8400-TAMANHO          
CPMCAC     MOVE 'OC'                   TO WRK-CLLP8400-OPERACAO         
CPMCAC     MOVE WRK-MASCARA            TO WRK-CLLP8400-MASCARA6         
CPMCAC     MOVE WRK-CAMPOAUX(1:6)      TO WRK-CLLP8400-OPERANDO6        
                                                                        
CPMCAC     CALL WRK-CLLP8400           USING WRK-CLLP8400-TAMANHO       
CPMCAC                                       WRK-CLLP8400-OPERACAO      
CPMCAC                                       WRK-CLLP8400-OPERANDO6     
CPMCAC                                       WRK-CLLP8400-MASCARA6      
                                                                        
CPMCAC     MOVE WRK-CLLP8400-OPERANDO6 TO WRK-CAMPOAUX(1:6)             
                                                                        
           IF  WRK-CAMPOANT            EQUAL WRK-CAMPOAUX               
               GO TO 1620-BACKPESQ                                      
           END-IF                                                       
                                                                        
           CALL 'BRAD0480'             USING WRK-COD3                   
                                             WRK-TABCL                  
                                             WRK-CAMPOAUX               
                                             WRK-TABELACL               
                                                                        
           MOVE WRK-CAMPOAUX           TO WRK-CAMPOANT.                 
                                                                        
      *----------------------------------------------------------------*
       1620-BACKPESQ.                                                   
      *----------------------------------------------------------------*
                                                                        
CPMCAC     CONTINUE.                                                    
                                                                        
       1620-99-FIM.                    EXIT.                            
                                                                        
      ******************************************************************
      *   ROTINA DE IMPRESSAO DE DETALHE                               *
      ******************************************************************
                                                                        
      *----------------------------------------------------------------*
       1630-IMPDET.                                                     
      *----------------------------------------------------------------*
                                                                        
      *     RESTAURA REG. ORIGINAL                                      
      *     QUE SOFREU ALTERACAO                                        
      *     DURANTE CONSISTENCIA.                                       
                                                                        
           MOVE WRK-WKMOVTO(1:250)     TO WRK-REGMOVTO(1:250)           
           MOVE WRK-WKMOVTO(251:250)   TO WRK-REGMOVTO(251:250)         
           MOVE WRK-WKMOVTO(501:75)    TO WRK-REGMOVTO(501:75)          
                                                                        
      *     EMPRESA                                                     
                                                                        
           MOVE WRK-FIRMA              TO WRK-DETEMP                    
CPMCAC     MOVE '/'                    TO WRK-DETCRIT1(7:1)             
                                                                        
      *     AGENCIA                                                     
                                                                        
           MOVE WRK-AGENCIA            TO WRK-DETAG                     
CPMCAC     MOVE '/'                    TO WRK-DETCRIT1(13:1)            
                                                                        
      *     CC                                                          
                                                                        
           MOVE WRK-CONTA              TO WRK-DETCC                     
CPMCAC     MOVE '/'                    TO WRK-DETCRIT1(21:1)            
                                                                        
      *     CART                                                        
                                                                        
           MOVE WRK-CART               TO WRK-DETCART                   
CPMCAC     MOVE '/'                    TO WRK-DETCRIT1(25:1)            
                                                                        
      *     CONTRATO                                                    
                                                                        
           MOVE WRK-NUMOP              TO WRK-DETCONTR                  
CPMCAC     MOVE '/'                    TO WRK-DETCRIT1(33:1)            
                                                                        
      *     VENCTO                                                      
                                                                        
           MOVE WRK-VENCIM             TO WRK-DETVENCT                  
                                                                        
      *     NOME DO DEVEDOR                                             
                                                                        
           MOVE WRK-NOMED              TO WRK-DETNMDEV                  
                                                                        
      *     CGC DO DEVEDOR                                              
                                                                        
           MOVE WRK-CGC(1:9)           TO WRK-DETCGCDV                  
           MOVE WRK-CGC(10:4)          TO WRK-DETFILDV                  
           MOVE WRK-CGC(14:2)          TO WRK-DETCNTDV                  
                                                                        
      *     RZ/CT                                                       
                                                                        
           MOVE WRK-RAZCL              TO WRK-DETRAZAO                  
                                                                        
      *     ENT. CL                                                     
                                                                        
           MOVE WRK-CTACL              TO WRK-DETENTCL                  
                                                                        
      *     VALOR DA OPERACAO                                           
                                                                        
           IF  WRK-DETVROPE            NOT EQUAL '*'                    
               GO TO 1640-EDVROPE                                       
           END-IF                                                       
                                                                        
           MOVE WRK-VALOP              TO WRK-DETVROPE(1:15)            
                                                                        
           GO TO 1650-IMPVRINI.                                         
                                                                        
      *----------------------------------------------------------------*
       1640-EDVROPE.                                                    
      *----------------------------------------------------------------*
                                                                        
      *     ZZZ.ZZZ.ZZZ.Z99,99                                          
                                                                        
CPMCAC     MOVE WRK-VALOP-R            TO WRK-AUXVALOR                  
CPMCAC     MOVE WRK-AUXVALOR-R         TO WRK-9-13V99-D                 
CPMCAC     MOVE WRK-9-12V99-D          TO WRK-PADVAL19                  
           MOVE WRK-PADVAL19-R(2:18)   TO WRK-DETVROPE.                 
                                                                        
      *----------------------------------------------------------------*
       1650-IMPVRINI.                                                   
      *----------------------------------------------------------------*
                                                                        
      *     VALOR INICIAL                                               
                                                                        
           IF  WRK-DETVRINI            NOT EQUAL '*'                    
               GO TO 1660-EDVRINI                                       
           END-IF                                                       
                                                                        
           MOVE WRK-VALINIC            TO WRK-DETVRINI(1:15)            
                                                                        
           GO TO 1670-IMPVRCON.                                         
                                                                        
      *----------------------------------------------------------------*
       1660-EDVRINI.                                                    
      *----------------------------------------------------------------*
                                                                        
      *     ZZZZ.ZZZ.ZZZ.ZZ9,99                                         
                                                                        
CPMCAC     MOVE WRK-VALINIC-R          TO WRK-AUXVALOR                  
CPMCAC     MOVE WRK-AUXVALOR-R         TO WRK-PADVAL20                  
CPMCAC     MOVE WRK-PADVAL20-R(2:19)   TO WRK-DETVRINI.                 
                                                                        
      *----------------------------------------------------------------*
       1670-IMPVRCON.                                                   
      *----------------------------------------------------------------*
                                                                        
      *     VALOR CONTABIL                                              
                                                                        
           IF  WRK-DETVRCON            NOT EQUAL '*'                    
               GO TO 1680-EDVRCON                                       
           END-IF                                                       
                                                                        
           MOVE WRK-VALRES             TO WRK-DETVRCON(1:15)            
                                                                        
           GO TO 1690-IMPVRIOF.                                         
                                                                        
      *----------------------------------------------------------------*
       1680-EDVRCON.                                                    
      *----------------------------------------------------------------*
                                                                        
      *     ZZZZ.ZZZ.ZZZ.ZZ9,99                                         
                                                                        
CPMCAC     MOVE WRK-VALRES-R           TO WRK-AUXVALOR                  
CPMCAC     MOVE WRK-AUXVALOR-R         TO WRK-PADVAL20                  
CPMCAC     MOVE WRK-PADVAL20-R(2:19)   TO WRK-DETVRCON.                 
                                                                        
      *----------------------------------------------------------------*
       1690-IMPVRIOF.                                                   
      *----------------------------------------------------------------*
                                                                        
      *     VALOR DO IOF                                                
                                                                        
           IF  WRK-DETVRIOF            NOT EQUAL '*'                    
               GO TO 1700-EDVRIOF                                       
           END-IF                                                       
                                                                        
           MOVE WRK-VALORIOF           TO WRK-DETVRIOF(1:15)            
                                                                        
           GO TO 1710-IMPVRVEN.                                         
                                                                        
      *----------------------------------------------------------------*
       1700-EDVRIOF.                                                    
      *----------------------------------------------------------------*
                                                                        
      *     ZZZZ.ZZZ.ZZZ.ZZ9,99                                         
                                                                        
CPMCAC     MOVE WRK-VALORIOF-R         TO WRK-AUXVALOR                  
CPMCAC     MOVE WRK-AUXVALOR-R         TO WRK-PADVAL20                  
CPMCAC     MOVE WRK-PADVAL20-R(2:19)   TO WRK-DETVRIOF.                 
                                                                        
      *----------------------------------------------------------------*
       1710-IMPVRVEN.                                                   
      *----------------------------------------------------------------*
                                                                        
      *     VALOR ENCARGOS VENCIDOS                                     
                                                                        
           IF  WRK-DETVRVEN            NOT EQUAL '*'                    
               GO TO 1720-EDVRVEN                                       
           END-IF                                                       
                                                                        
           MOVE WRK-VALVENC            TO WRK-DETVRVEN(1:15)            
                                                                        
           GO TO 1730-IMPVRVIN.                                         
                                                                        
      *----------------------------------------------------------------*
       1720-EDVRVEN.                                                    
      *----------------------------------------------------------------*
                                                                        
      *     ZZZZ.ZZZ.ZZZ.ZZ9,99                                         
                                                                        
CPMCAC     MOVE WRK-VALVENC-R          TO WRK-AUXVALOR                  
CPMCAC     MOVE WRK-AUXVALOR-R         TO WRK-PADVAL20                  
CPMCAC     MOVE WRK-PADVAL20-R(2:19)   TO WRK-DETVRVEN.                 
                                                                        
      *----------------------------------------------------------------*
       1730-IMPVRVIN.                                                   
      *----------------------------------------------------------------*
                                                                        
      *     VALOR ENCARGOS VINCENDOS                                    
                                                                        
           IF  WRK-DETVRVIN            NOT EQUAL '*'                    
               GO TO 1740-EDVRVIN                                       
           END-IF                                                       
                                                                        
           MOVE WRK-VALVINC            TO WRK-DETVRVIN(1:15)            
                                                                        
           GO TO 1750-IMPVRBAS.                                         
                                                                        
      *----------------------------------------------------------------*
       1740-EDVRVIN.                                                    
      *----------------------------------------------------------------*
                                                                        
      *     ZZZZ.ZZZ.ZZZ.ZZ9,99                                         
                                                                        
CPMCAC     MOVE WRK-VALVINC-R          TO WRK-AUXVALOR                  
CPMCAC     MOVE WRK-AUXVALOR-R         TO WRK-PADVAL20                  
CPMCAC     MOVE WRK-PADVAL20-R(2:19)   TO WRK-DETVRVIN.                 
                                                                        
      *----------------------------------------------------------------*
       1750-IMPVRBAS.                                                   
      *----------------------------------------------------------------*
                                                                        
      *     VALOR BASE DE CALCULO                                       
                                                                        
           IF  WRK-DETVRBAS            NOT EQUAL '*'                    
               GO TO 1760-EDVRBAS                                       
           END-IF                                                       
                                                                        
           MOVE WRK-VRBASE             TO WRK-DETVRBAS(1:15)            
                                                                        
           GO TO 1770-IMPID.                                            
                                                                        
      *----------------------------------------------------------------*
       1760-EDVRBAS.                                                    
      *----------------------------------------------------------------*
                                                                        
      *     ZZZZ.ZZZ.ZZZ.ZZ9,99                                         
                                                                        
CPMCAC     MOVE WRK-VRBASE-R           TO WRK-AUXVALOR                  
CPMCAC     MOVE WRK-AUXVALOR-R         TO WRK-PADVAL20                  
CPMCAC     MOVE WRK-PADVAL20-R(2:19)   TO WRK-DETVRBAS.                 
                                                                        
      *----------------------------------------------------------------*
       1770-IMPID.                                                      
      *----------------------------------------------------------------*
                                                                        
      *     ID. CL/LP                                                   
                                                                        
           MOVE WRK-ID                 TO WRK-DETID                     
                                                                        
      *     NOME PRIMEIRO AVALISTA                                      
                                                                        
           MOVE WRK-COOB0              TO WRK-DETNMAV1                  
                                                                        
      *     CGC PRIMEIRO AVALISTA                                       
                                                                        
           MOVE WRK-CGC1(1:9)          TO WRK-DETCGCA1                  
           MOVE WRK-CGC1(10:4)         TO WRK-DETFILA1                  
           MOVE WRK-CGC(14:2)          TO WRK-DETCNTA1                  
                                                                        
      *     NOME SEGUNDO AVALISTA                                       
                                                                        
           MOVE WRK-COOB1              TO WRK-DETNMAV2                  
                                                                        
      *     CGC SEGUNDO AVALISTA                                        
                                                                        
           MOVE WRK-CGC2(1:9)          TO WRK-DETCGCA2                  
           MOVE WRK-CGC2(10:4)         TO WRK-DETFILA2                  
           MOVE WRK-CGC2(14:2)         TO WRK-DETCNTA2                  
                                                                        
      *     AGENCIA PARA RESP                                           
                                                                        
           MOVE WRK-AGRESP             TO WRK-DETAGRES                  
                                                                        
      *     TIPO DE GARANTIA                                            
                                                                        
           MOVE WRK-TIPOGAR            TO WRK-DETTPGAR                  
                                                                        
      *     NOME DA GARANTIA                                            
                                                                        
CPMCAC     MOVE WRK-GARANTIA(1:30)     TO WRK-DETGARAN                  
                                                                        
      *     TIPO PENDENCIA                                              
                                                                        
           MOVE WRK-IDENT              TO WRK-DETPEND                   
                                                                        
      *     DATA DE INICIO OPERACAO                                     
                                                                        
CPMCAC     MOVE WRK-OPERACAOO          TO WRK-DETDTINI                  
                                                                        
      *     DATA DE ENTRADA EM CL                                       
                                                                        
           MOVE WRK-ENTRADA            TO WRK-DETDTENT                  
                                                                        
      *     DATA DO MOVTO.                                              
                                                                        
           MOVE WRK-DTAMOVTO           TO WRK-DETDTMOV                  
                                                                        
      *     DATA DE AJUIZAMENTO                                         
                                                                        
           MOVE WRK-AJUIZAM            TO WRK-DETDTAJU                  
                                                                        
      *     LOCAL                                                       
                                                                        
           MOVE WRK-LOCAL              TO WRK-DETLOC                    
                                                                        
      *     MARCA                                                       
                                                                        
           MOVE WRK-MARCA              TO WRK-DETMARCA                  
                                                                        
      *     COD. DAS INCONSISTENCIAS                                    
                                                                        
           MOVE WRK-TABINCON           TO WRK-DETCDERR                  
                                                                        
           IF  WRK-ACLIN               LESS 50                          
               GO TO 1780-DETIMP                                        
           END-IF                                                       
                                                                        
           PERFORM 1790-IMPCAB THRU 1800-99-FIM                         
                                                                        
           WRITE FD-REG-RELATO         FROM WRK-CABCRIT1                
                                                                        
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO                  
                                                                        
           PERFORM 9060-TESTAR-FS-RELATO THRU 9060-99-FIM               
                                                                        
           WRITE FD-REG-RELATO         FROM WRK-CABCRIT2                
                                                                        
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO                  
                                                                        
           PERFORM 9060-TESTAR-FS-RELATO THRU 9060-99-FIM               
                                                                        
           WRITE FD-REG-RELATO         FROM WRK-CABCRIT3                
                                                                        
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO                  
                                                                        
           PERFORM 9060-TESTAR-FS-RELATO THRU 9060-99-FIM               
                                                                        
           WRITE FD-REG-RELATO         FROM WRK-CABCRIT4                
                                                                        
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO                  
                                                                        
           PERFORM 9060-TESTAR-FS-RELATO THRU 9060-99-FIM               
                                                                        
           ADD 4                       TO WRK-ACLIN.                    
                                                                        
      *----------------------------------------------------------------*
       1780-DETIMP.                                                     
      *----------------------------------------------------------------*
                                                                        
           WRITE FD-REG-RELATO         FROM WRK-DETCRIT1                
                                                                        
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO                  
                                                                        
           PERFORM 9060-TESTAR-FS-RELATO THRU 9060-99-FIM               
                                                                        
           WRITE FD-REG-RELATO         FROM WRK-DETCRIT2                
                                                                        
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO                  
                                                                        
           PERFORM 9060-TESTAR-FS-RELATO THRU 9060-99-FIM               
                                                                        
           WRITE FD-REG-RELATO         FROM WRK-DETCRIT3                
                                                                        
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO                  
                                                                        
           PERFORM 9060-TESTAR-FS-RELATO THRU 9060-99-FIM               
                                                                        
           WRITE FD-REG-RELATO         FROM WRK-DETCRIT4                
                                                                        
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO                  
                                                                        
           PERFORM 9060-TESTAR-FS-RELATO THRU 9060-99-FIM               
                                                                        
      *     LIMPA LINHAS DO REL.                                        
      *     DE INCONSISTENCIA                                           
      *     PARA PROXIMO REGISTRO                                       
      *     A SER CONSISTIDO                                            
                                                                        
           MOVE WRK-BRANCOS(1:132)     TO WRK-DETCRIT1(2:132)           
           MOVE WRK-BRANCOS(1:132)     TO WRK-DETCRIT2(2:132)           
           MOVE WRK-BRANCOS(1:132)     TO WRK-DETCRIT3(2:132)           
           MOVE WRK-BRANCOS(1:132)     TO WRK-DETCRIT4(2:132)           
                                                                        
           ADD 5                       TO WRK-ACLIN                     
           ADD 1                       TO WRK-QTDREJE.                  
                                                                        
       1780-99-FIM.                    EXIT.                            
                                                                        
      ******************************************************************
      *   ROTINA DE IMPRESSAO DE CABECALHO                             *
      ******************************************************************
                                                                        
      *----------------------------------------------------------------*
       1790-IMPCAB.                                                     
      *----------------------------------------------------------------*
                                                                        
           ADD 1                       TO WRK-ACPAG                     
                                                                        
CPMCAC     MOVE WRK-ACPAG              TO WRK-PADRAO1                   
CPMCAC     MOVE WRK-PADRAO1-R          TO WRK-CB1PAG                    
                                                                        
           WRITE FD-REG-RELATO         FROM WRK-CAB1                    
                                                                        
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO                  
                                                                        
           PERFORM 9060-TESTAR-FS-RELATO THRU 9060-99-FIM               
                                                                        
           WRITE FD-REG-RELATO         FROM WRK-CAB2                    
                                                                        
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO                  
                                                                        
           PERFORM 9060-TESTAR-FS-RELATO THRU 9060-99-FIM.              
                                                                        
      *----------------------------------------------------------------*
       1800-FIMPCAB.                                                    
      *----------------------------------------------------------------*
                                                                        
           MOVE WRK-BRANCOS(1:133)     TO WRK-DET1                      
                                                                        
           WRITE FD-REG-RELATO         FROM WRK-DET1                    
                                                                        
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO                  
                                                                        
           PERFORM 9060-TESTAR-FS-RELATO THRU 9060-99-FIM               
                                                                        
           MOVE 3                      TO WRK-ACLIN.                    
                                                                        
       1800-99-FIM.                    EXIT.                            
                                                                        
      ******************************************************************
      *   ROTINA DE IMPRESSAO DOS CODIGOS DE ERRO                      *
      ******************************************************************
                                                                        
      *----------------------------------------------------------------*
       1810-FIMPGM.                                                     
      *----------------------------------------------------------------*
                                                                        
CPMCAC     CONTINUE.                                                    
                                                                        
      *----------------------------------------------------------------*
       1820-IMPCOD.                                                     
      *----------------------------------------------------------------*
                                                                        
           PERFORM 1790-IMPCAB THRU 1800-99-FIM                         
                                                                        
           WRITE FD-REG-RELATO         FROM WRK-CAB4                    
                                                                        
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO                  
                                                                        
           PERFORM 9060-TESTAR-FS-RELATO THRU 9060-99-FIM               
                                                                        
           WRITE FD-REG-RELATO         FROM WRK-CAB5                    
                                                                        
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO                  
                                                                        
           PERFORM 9060-TESTAR-FS-RELATO THRU 9060-99-FIM               
                                                                        
           WRITE FD-REG-RELATO         FROM WRK-CAB6                    
                                                                        
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO                  
                                                                        
           PERFORM 9060-TESTAR-FS-RELATO THRU 9060-99-FIM               
                                                                        
           MOVE 1                      TO IND-TABERRO.                  
                                                                        
      *----------------------------------------------------------------*
       1830-LOOPCOD.                                                    
      *----------------------------------------------------------------*
                                                                        
           IF  WRK-TABERRO(IND-TABERRO:2)                               
                                       EQUAL '**'                       
               GO TO 1850-FIMPCOD2                                      
           END-IF                                                       
                                                                        
           MOVE WRK-TABERRO(IND-TABERRO:2)                              
                                       TO WRK-DT1COD1                   
           MOVE WRK-TABERRO(IND-TABERRO + 2:35)                         
                                       TO WRK-DT1DESC1                  
                                                                        
           ADD 37                      TO IND-TABERRO                   
                                                                        
           IF  WRK-TABERRO(IND-TABERRO:2)                               
                                       EQUAL '**'                       
               GO TO 1840-FIMPCOD1                                      
           END-IF                                                       
                                                                        
           MOVE WRK-TABERRO(IND-TABERRO:2)                              
                                       TO WRK-DT1COD2                   
           MOVE WRK-TABERRO(IND-TABERRO + 2:35)                         
                                       TO WRK-DT1DESC2                  
                                                                        
           ADD 37                      TO IND-TABERRO                   
                                                                        
           WRITE FD-REG-RELATO         FROM WRK-DET1                    
                                                                        
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO                  
                                                                        
           PERFORM 9060-TESTAR-FS-RELATO THRU 9060-99-FIM               
                                                                        
CPMCAC     MOVE WRK-BRANCOS(1:133)     TO WRK-DET1                      
                                                                        
           GO TO 1830-LOOPCOD.                                          
                                                                        
      *----------------------------------------------------------------*
       1840-FIMPCOD1.                                                   
      *----------------------------------------------------------------*
                                                                        
           WRITE FD-REG-RELATO         FROM WRK-DET1                    
                                                                        
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO                  
                                                                        
           PERFORM 9060-TESTAR-FS-RELATO THRU 9060-99-FIM.              
                                                                        
      *----------------------------------------------------------------*
       1850-FIMPCOD2.                                                   
      *----------------------------------------------------------------*
                                                                        
           ADD 1                       TO WRK-ACPAG                     
                                                                        
CPMCAC     MOVE WRK-ACPAG              TO WRK-PADRAO1                   
CPMCAC     MOVE WRK-PADRAO1-R          TO WRK-CB1PAG                    
                                                                        
           WRITE FD-REG-RELATO         FROM WRK-CAB1                    
                                                                        
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO                  
                                                                        
           PERFORM 9060-TESTAR-FS-RELATO THRU 9060-99-FIM               
                                                                        
           WRITE FD-REG-RELATO         FROM WRK-CAB2                    
                                                                        
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO                  
                                                                        
           PERFORM 9060-TESTAR-FS-RELATO THRU 9060-99-FIM               
                                                                        
           WRITE FD-REG-RELATO         FROM WRK-CAB3                    
                                                                        
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO                  
                                                                        
           PERFORM 9060-TESTAR-FS-RELATO THRU 9060-99-FIM               
                                                                        
           WRITE FD-REG-RELATO         FROM WRK-CAB8                    
                                                                        
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO                  
                                                                        
           PERFORM 9060-TESTAR-FS-RELATO THRU 9060-99-FIM               
                                                                        
           WRITE FD-REG-RELATO         FROM WRK-CAB3                    
                                                                        
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO                  
                                                                        
           PERFORM 9060-TESTAR-FS-RELATO THRU 9060-99-FIM               
                                                                        
           WRITE FD-REG-RELATO         FROM WRK-CAB3                    
                                                                        
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO                  
                                                                        
           PERFORM 9060-TESTAR-FS-RELATO THRU 9060-99-FIM               
                                                                        
CPMCAC     MOVE WRK-QTDLIDO            TO WRK-PADRAO2                   
CPMCAC     MOVE WRK-PADRAO2-R          TO WRK-DT4QTD                    
                                                                        
CPMCAC     MOVE WRK-QTDCERRO           TO WRK-PADRAO2                   
CPMCAC     MOVE WRK-PADRAO2-R          TO WRK-DT5QTD                    
                                                                        
CPMCAC     MOVE WRK-QTDREJE            TO WRK-PADRAO2                   
CPMCAC     MOVE WRK-PADRAO2-R          TO WRK-DT6QTD                    
                                                                        
CPMCAC     MOVE WRK-QTDGRAV            TO WRK-PADRAO2                   
CPMCAC     MOVE WRK-PADRAO2-R          TO WRK-DT7QTD                    
                                                                        
           WRITE FD-REG-RELATO         FROM WRK-DET4                    
                                                                        
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO                  
                                                                        
           PERFORM 9060-TESTAR-FS-RELATO THRU 9060-99-FIM               
                                                                        
           WRITE FD-REG-RELATO         FROM WRK-CAB3                    
                                                                        
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO                  
                                                                        
           PERFORM 9060-TESTAR-FS-RELATO THRU 9060-99-FIM               
                                                                        
           WRITE FD-REG-RELATO         FROM WRK-DET5                    
                                                                        
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO                  
                                                                        
           PERFORM 9060-TESTAR-FS-RELATO THRU 9060-99-FIM               
                                                                        
           WRITE FD-REG-RELATO         FROM WRK-CAB3                    
                                                                        
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO                  
                                                                        
           PERFORM 9060-TESTAR-FS-RELATO THRU 9060-99-FIM               
                                                                        
           WRITE FD-REG-RELATO         FROM WRK-DET6                    
                                                                        
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO                  
                                                                        
           PERFORM 9060-TESTAR-FS-RELATO THRU 9060-99-FIM               
                                                                        
           WRITE FD-REG-RELATO         FROM WRK-CAB3                    
                                                                        
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO                  
                                                                        
           PERFORM 9060-TESTAR-FS-RELATO THRU 9060-99-FIM               
                                                                        
           WRITE FD-REG-RELATO         FROM WRK-DET7                    
                                                                        
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO                  
                                                                        
           PERFORM 9060-TESTAR-FS-RELATO THRU 9060-99-FIM               
                                                                        
           CLOSE MOVTO                                                  
                 CERTO                                                  
                 RELATO                                                 
                                                                        
           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO                  
                                                                        
           PERFORM 9040-TESTAR-FS-MOVTO  THRU 9040-99-FIM               
                                                                        
           PERFORM 9050-TESTAR-FS-CERTO  THRU 9050-99-FIM               
                                                                        
           PERFORM 9060-TESTAR-FS-RELATO THRU 9060-99-FIM               
                                                                        
           MOVE 0                      TO RETURN-CODE                   
                                                                        
           GOBACK.                                                      
                                                                        
      *----------------------------------------------------------------*
       9000-TESTAR-FILE-STATUS.                                         
      *----------------------------------------------------------------*
                                                                        
           PERFORM 9010-TESTAR-FS-ARQDATA  THRU 9010-99-FIM             
           PERFORM 9020-TESTAR-FS-CADLOCAL THRU 9020-99-FIM             
           PERFORM 9030-TESTAR-FS-CADPEND  THRU 9030-99-FIM             
           PERFORM 9040-TESTAR-FS-MOVTO    THRU 9040-99-FIM             
           PERFORM 9050-TESTAR-FS-CERTO    THRU 9050-99-FIM             
           PERFORM 9060-TESTAR-FS-RELATO   THRU 9060-99-FIM.            
                                                                        
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
       9020-TESTAR-FS-CADLOCAL.                                         
      *----------------------------------------------------------------*
                                                                        
           IF (WRK-FS-CADLOCAL         NOT EQUAL '00')                  
               MOVE 'CADLOCAL'         TO WRK-NOME-ARQ                  
               MOVE WRK-FS-CADLOCAL    TO WRK-FILE-STATUS               
               MOVE WRK-ERRO-BRAD7100  TO ERR-TEXTO                     
               PERFORM 9999-PROCESSAR-ROTINA-ERRO                       
           END-IF.                                                      
                                                                        
       9020-99-FIM.                    EXIT.                            
                                                                        
      *----------------------------------------------------------------*
       9030-TESTAR-FS-CADPEND.                                          
      *----------------------------------------------------------------*
                                                                        
           IF (WRK-FS-CADPEND          NOT EQUAL '00')                  
               MOVE 'CADPEND'          TO WRK-NOME-ARQ                  
               MOVE WRK-FS-CADPEND     TO WRK-FILE-STATUS               
               MOVE WRK-ERRO-BRAD7100  TO ERR-TEXTO                     
               PERFORM 9999-PROCESSAR-ROTINA-ERRO                       
           END-IF.                                                      
                                                                        
       9030-99-FIM.                    EXIT.                            
                                                                        
      *----------------------------------------------------------------*
       9040-TESTAR-FS-MOVTO.                                            
      *----------------------------------------------------------------*
                                                                        
           IF (WRK-FS-MOVTO            NOT EQUAL '00')                  
               MOVE 'MOVTO'            TO WRK-NOME-ARQ                  
               MOVE WRK-FS-MOVTO       TO WRK-FILE-STATUS               
               MOVE WRK-ERRO-BRAD7100  TO ERR-TEXTO                     
               PERFORM 9999-PROCESSAR-ROTINA-ERRO                       
           END-IF.                                                      
                                                                        
       9040-99-FIM.                    EXIT.                            
                                                                        
      *----------------------------------------------------------------*
       9050-TESTAR-FS-CERTO.                                            
      *----------------------------------------------------------------*
                                                                        
           IF (WRK-FS-CERTO            NOT EQUAL '00')                  
               MOVE 'CERTO'            TO WRK-NOME-ARQ                  
               MOVE WRK-FS-CERTO       TO WRK-FILE-STATUS               
               MOVE WRK-ERRO-BRAD7100  TO ERR-TEXTO                     
               PERFORM 9999-PROCESSAR-ROTINA-ERRO                       
           END-IF.                                                      
                                                                        
       9050-99-FIM.                    EXIT.                            
                                                                        
      *----------------------------------------------------------------*
       9060-TESTAR-FS-RELATO.                                           
      *----------------------------------------------------------------*
                                                                        
           IF (WRK-FS-RELATO           NOT EQUAL '00')                  
               MOVE 'RELATO'           TO WRK-NOME-ARQ                  
               MOVE WRK-FS-RELATO      TO WRK-FILE-STATUS               
               MOVE WRK-ERRO-BRAD7100  TO ERR-TEXTO                     
               PERFORM 9999-PROCESSAR-ROTINA-ERRO                       
           END-IF.                                                      
                                                                        
       9060-99-FIM.                    EXIT.                            
                                                                        
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO.                                      
      *----------------------------------------------------------------*
                                                                        
           MOVE 'CLLP6620'             TO ERR-PGM                       
           MOVE 'APL'                  TO ERR-TIPO-ACESSO               
                                                                        
           CALL 'BRAD7100'             USING WRK-BATCH                  
                                             ERRO-AREA.                 
                                                                        
           GOBACK.                                                      
                                                                        
      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*

      ******************************************************************
      *                                                                *
      * NOME INC: PSDCW018                                             *
      *                                                                *
      * DESCRICAO: DISPONIBILIZA INFORMACOES CADASTRAIS POR TIPO DE    *
      *            REGISTROS A PARTIR DOS BLOCOS CADASTRAIS EXISTENTE  *
      *            NO MODELO CADU                                      *
      *            (INFORMACOES A SEREM UTILIZADAS NA WORKING-STORAGE) *
      *            TAMANHO: 660 BYTES                                  *
      *                                                                *
      ******************************************************************
CPMALP*                       A L T E R A C A O                        *
.     *----------------------------------------------------------------*
.     * ANALISTA.....: ANA LUIZA PEZZINI         - CPMBRAXIS           *
.     * DATA.........: 31/10/2011                                      *
.     *----------------------------------------------------------------*
.     * OBJETIVO.....: INCLUIR CAMPOS NOVOS DAS TABELAS NO LAYOUT      *
CPMALP******************************************************************
CPMTEL*                       A L T E R A C A O                        *
.     *----------------------------------------------------------------*
.     * ANALISTA.....: ANA LUIZA PEZZINI         - CPMBRAXIS           *
.     * DATA.........: 19/03/2012                                      *
.     *----------------------------------------------------------------*
.     * OBJETIVO.....: INCLUIR CAMPO TELEFONE NO LAYOUT DAS TABELAS    *
.     *                CADUB003-CADUB006-CADUB0A1-CADUB0A8-CADUB0M3    *
.     *             ---INCLUIR LAYOUT DAS TABELAS CADUB0F9 E CADUB0M1  *
CPMTEL******************************************************************
      ******************************************************************
      *                       A L T E R A C A O                        *
      *----------------------------------------------------------------*
      * PROGRAMADOR..: ALAN RICARTE SALOMAO      - CPMBRAXIS           *
      * DATA.........: 12/09/2012                                      *
      *----------------------------------------------------------------*
      * OBJETIVO.....: INCLUIR 3 NOVOS CAMPOS NO LAYOUT CADUB018 E UM  *
      *                NOVO CAMPO NO LAYOUT CADUB003                   *
      ******************************************************************
                                                                        
       01  PSDCW018-REGISTRO.                                           
                                                                        
      ******************************************************************
      **** TIPO REG.: 001 - CADUB0A4 - PESSOA-UNICA                     
      ******************************************************************
                                                                        
           03  PSDCW018-01-PSSOA-UNICA.                                 
                05  PSDCW018-01-CPF-CNPJ.                               
                    07  PSDCW018-01-CPF-CNPJ-NRO     PIC S9(009) COMP-3.
                    07  PSDCW018-01-CPF-CNPJ-CTR     PIC S9(002) COMP-3.
                    07  PSDCW018-01-CPF-CNPJ-FIL     PIC S9(004) COMP-3.
                05  PSDCW018-01-CCLUB                PIC S9(010) COMP-3.
                05  PSDCW018-01-TP-REG               PIC  X(003).       
                05  PSDCW018-01-CPTCAO-ESPAC-TBELA   PIC S9(002) COMP-3.
                05  PSDCW018-01-CCLUB-VINC           PIC S9(010) COMP-3.
                05  PSDCW018-01-CEMPR-INC            PIC S9(005) COMP-3.
                05  PSDCW018-01-CCTRO-CUSTO          PIC  X(004).       
                05  PSDCW018-01-CSIT-RLCTO-PSSOA     PIC S9(002) COMP-3.
                05  PSDCW018-01-CDEPDC               PIC S9(005) COMP-3.
                05  PSDCW018-01-CTPO-CLI-CREDT       PIC S9(003) COMP-3.
                05  PSDCW018-01-CID-TPO-PSSOA        PIC  X(001).       
                05  PSDCW018-01-CSGMTO-CLI           PIC S9(003) COMP-3.
                05  PSDCW018-01-CSSGTO-CLI           PIC S9(003) COMP-3.
                05  PSDCW018-01-CGER-RESP            PIC S9(009) COMP-3.
                05  PSDCW018-01-CINDCD-ENQUA-SGMTO   PIC  X(001).       
                05  PSDCW018-01-CCLUB-ORIGE-PRSCT    PIC S9(009) COMP-3.
                05  PSDCW018-01-RCOMEN-LIVRE-GER     PIC  X(300).       
                05  PSDCW018-01-CINDCD-CONS-BACEN    PIC  X(001).       
                05  PSDCW018-01-CINDCD-CPO-FALTT     PIC  X(001).       
                05  PSDCW018-01-CISENC-CPF-CNPJ      PIC  X(001).       
                05  PSDCW018-01-CMOTVO-ISENC-CPF     PIC S9(001) COMP-3.
                05  PSDCW018-01-CCPF-CNPJ            PIC S9(009) COMP-3.
                05  PSDCW018-01-CCTRL-CPF-CNPJ       PIC S9(002) COMP-3.
                05  PSDCW018-01-CFLIAL-CPF-CNPJ      PIC S9(004) COMP-3.
                05  PSDCW018-01-DNASC-FUNDC          PIC  X(010).       
                05  PSDCW018-01-CAGPTO-PSSOA         PIC  X(045).       
                05  PSDCW018-01-DCLUB-VINC           PIC  X(010).       
                05  PSDCW018-01-DENQUA-PSSOA         PIC  X(010).       
                05  PSDCW018-01-DSIT-CAD             PIC  X(010).       
                05  PSDCW018-01-DCAD-PSSOA           PIC  X(010).       
                05  PSDCW018-01-DULT-RENOV           PIC  X(010).       
                05  PSDCW018-01-DPROX-RENOV          PIC  X(010).       
                05  PSDCW018-01-DCLI-DESDE           PIC  X(010).       
                05  PSDCW018-01-CSIT-CAD-PSSOA       PIC S9(001) COMP-3.
                05  PSDCW018-01-DINIC-VALDD-BACEN    PIC  X(010).       
                05  PSDCW018-01-DFIM-VALDD-BACEN     PIC  X(010).       
                05  PSDCW018-01-CCLASS-CAD           PIC S9(003) COMP-3.
                05  PSDCW018-01-PCOBR-TARIF       PIC S9(003)V99 COMP-3.
                05  PSDCW018-01-CBCO-COBR-TARIF      PIC S9(003) COMP-3.
                05  PSDCW018-01-CAG-COBR-TARIF       PIC S9(005) COMP-3.
                05  PSDCW018-01-CCTA-COBR-TARIF      PIC S9(013) COMP-3.
                05  PSDCW018-01-CFUNC-BDSCO-LIBR     PIC S9(009) COMP-3.
                05  PSDCW018-01-QREG-CNTRL-FONE      PIC S9(003) COMP-3.
                05  PSDCW018-01-QREG-CLI             PIC S9(003) COMP-3.
                05  PSDCW018-01-CINDCD-COMPL         PIC  X(001).       
                05  PSDCW018-01-QREG-DEPDT           PIC S9(003) COMP-3.
                05  PSDCW018-01-QREG-ENDER           PIC S9(003) COMP-3.
                05  PSDCW018-01-CINDCD-EXPOR         PIC  X(001).       
                05  PSDCW018-01-CINDCD-FAMLR         PIC  X(001).       
                05  PSDCW018-01-CINDCD-FINCR         PIC  X(001).       
                05  PSDCW018-01-QREG-FORNC           PIC S9(003) COMP-3.
                05  PSDCW018-01-CINDCD-IMPOR         PIC  X(001).       
                05  PSDCW018-01-QREG-LSNG            PIC S9(003) COMP-3.
                05  PSDCW018-01-QREG-PSSOA-OPER      PIC S9(003) COMP-3.
                05  PSDCW018-01-QREG-OUTRA-DESP      PIC S9(003) COMP-3.
                05  PSDCW018-01-QREG-OUTRA-RENDA     PIC S9(003) COMP-3.
                05  PSDCW018-01-CINDCD-FROTA-VEIC    PIC  X(001).       
                05  PSDCW018-01-QREG-PATRM-IMOV      PIC S9(003) COMP-3.
                05  PSDCW018-01-QREG-OUTRO-BEM       PIC S9(003) COMP-3.
                05  PSDCW018-01-QREG-PATRM-VEIC      PIC S9(003) COMP-3.
                05  PSDCW018-01-QREG-PRODT-SERVC     PIC S9(003) COMP-3.
                05  PSDCW018-01-CINDCD-PROFS         PIC  X(001).       
                05  PSDCW018-01-QREG-PRTCP-EMPR      PIC S9(003) COMP-3.
                05  PSDCW018-01-QREG-PRTCP-SCIO      PIC S9(003) COMP-3.
                05  PSDCW018-01-QREG-QUADR-ADMTV     PIC S9(003) COMP-3.
                05  PSDCW018-01-QREG-QUADR-COSLH     PIC S9(003) COMP-3.
                05  PSDCW018-01-QREG-QUADR-SCIAL     PIC S9(003) COMP-3.
                05  PSDCW018-01-QREG-REFT-BCRIA      PIC S9(003) COMP-3.
                05  PSDCW018-01-QREG-REFT-CATAO      PIC S9(003) COMP-3.
                05  PSDCW018-01-QREG-REFT-COML       PIC S9(003) COMP-3.
                05  PSDCW018-01-QREG-REFT-PSSOA      PIC S9(003) COMP-3.
                05  PSDCW018-01-QREG-SEGUR           PIC S9(003) COMP-3.
CPMALP********  CAMPOS NOVOS  ******************************************
.               05  PSDCW018-01-QREG-DEFIC-PSSOA     PIC S9(003) COMP-3.
.               05  PSDCW018-01-CEMPR-ORIGE-PSSOA    PIC S9(010) COMP-3.
.               05  PSDCW018-01-QREG-DOCTO-PSSOA     PIC S9(003) COMP-3.
.               05  PSDCW018-01-QREG-EMPRG-PSSOA     PIC S9(003) COMP-3.
.               05  PSDCW018-01-QREG-ATVDD-PSSOA     PIC S9(003) COMP-3.
.               05  PSDCW018-01-QREG-MAQNA-EQPMT     PIC S9(003) COMP-3.
.               05  PSDCW018-01-QREG-PATRM-AERNV     PIC S9(003) COMP-3.
.               05  PSDCW018-01-QREG-PATRM-EMBCA     PIC S9(003) COMP-3.
.               05  PSDCW018-01-NSEQ-ENDER-ELETR     PIC S9(003) COMP-3.
CPMALP          05  PSDCW018-01-RESERVA              PIC  X(028).       
                                                                        
      ******************************************************************
      **** TIPO REG.: 002 - CADUB018 - ENDERECO-PESSOA                  
      ******************************************************************
                                                                        
           03   PSDCW018-02-ENDER-PSSOA  REDEFINES                      
                                          PSDCW018-01-PSSOA-UNICA.      
                05  PSDCW018-02-CPF-CNPJ.                               
                    07  PSDCW018-02-CPF-CNPJ-NRO     PIC S9(009) COMP-3.
                    07  PSDCW018-02-CPF-CNPJ-CTR     PIC S9(002) COMP-3.
                    07  PSDCW018-02-CPF-CNPJ-FIL     PIC S9(004) COMP-3.
                05  PSDCW018-02-CCLUB                PIC S9(010) COMP-3.
                05  PSDCW018-02-TP-REG               PIC  X(003).       
                05  PSDCW018-02-CSEQ-ENDER-PSSOA     PIC S9(005) COMP-3.
                05  PSDCW018-02-CPTCAO-ESPAC-TBELA   PIC S9(002) COMP-3.
                05  PSDCW018-02-CTPO-ENDER           PIC S9(001) COMP-3.
                05  PSDCW018-02-ELOGDR-PSSOA         PIC  X(070).       
                05  PSDCW018-02-ELOGDR-NRO           PIC  X(007).       
                05  PSDCW018-02-RCOMPL-ENDER         PIC  X(030).       
                05  PSDCW018-02-EBAIRO-ENDER         PIC  X(040).       
                05  PSDCW018-02-ICIDDE-ENDER         PIC  X(040).       
                05  PSDCW018-02-CSGL-UF              PIC  X(002).       
                05  PSDCW018-02-CPAIS                PIC S9(005) COMP-3.
                05  PSDCW018-02-CCEP                 PIC S9(005) COMP-3.
                05  PSDCW018-02-CCEP-COMPL           PIC S9(003) COMP-3.
                05  PSDCW018-02-CPSTAL-EXTER-ZIP     PIC  X(010).       
                05  PSDCW018-02-CCX-PSTAL            PIC S9(005) COMP-3.
                05  PSDCW018-02-CCEP-CX-PSTAL        PIC S9(005) COMP-3.
                05  PSDCW018-02-CCEP-PSTAL-COMPL     PIC S9(003) COMP-3.
                05  PSDCW018-02-CCX-PSTAL-AG         PIC S9(005) COMP-3.
                05  PSDCW018-02-CCX-PSTAL-EXTER      PIC  X(010).       
                05  PSDCW018-02-CCONF-ENDER          PIC S9(003) COMP-3.
                05  PSDCW018-02-CINDCD-ENDER-INVLD   PIC  X(001).       
CPMALP********  CAMPOS NOVOS  ******************************************
.               05  PSDCW018-02-CEMPR                PIC S9(010) COMP-3.
.               05  PSDCW018-02-CESPCE-ENDER         PIC S9(001) COMP-3.
.               05  PSDCW018-02-CNVEL-CONFD-ENDER    PIC S9(003) COMP-3.
.               05  PSDCW018-02-CCOMPV-ENDER-PSSOA   PIC S9(003) COMP-3.
.               05  PSDCW018-02-DINIC-UTILZ-ENDER    PIC  X(010).       
.               05  PSDCW018-02-DFIM-UTILZ-ENDER     PIC  X(010).       
.               05  PSDCW018-02-IPSSOA-CNTAT-ENDER   PIC  X(040).       
.               05  PSDCW018-02-CENDER-PDRAO-PSSOA   PIC  X(001).       
.               05  PSDCW018-02-CUSO-ENDER-PSSOA     PIC S9(003) COMP-3.
.               05  PSDCW018-02-DMES-ANO-RESID       PIC S9(006) COMP-3.
.               05  PSDCW018-02-RENDER-REDZD-CRRIO   PIC  X(040).       
.               05  PSDCW018-02-CCATEG-ENDER         PIC S9(003) COMP-3.
.               05  PSDCW018-02-CMUN                 PIC S9(010) COMP-3.
.               05  PSDCW018-02-CUF                  PIC S9(006) COMP-3.
CPMALP          05  PSDCW018-02-CPAIS-GEOGR          PIC S9(003) COMP-3.
ALAN            05  PSDCW018-02-CSIT-ENDER-PSSOA     PIC S9(001) COMP-3.
ALAN            05  PSDCW018-02-CMOTVO-DEVLC-CORSP   PIC S9(003) COMP-3.
ALAN            05  PSDCW018-02-DBLOQ-ENDER-PSSOA    PIC  X(010).       
                                                                        
      ******************************************************************
      **** TIPO REG.: 003 - CADUB006 - CENTRAL-TELEFONE                 
      ******************************************************************
                                                                        
           03   PSDCW018-03-CNTRL-FONE   REDEFINES                      
                                         PSDCW018-01-PSSOA-UNICA.       
                05  PSDCW018-03-CPF-CNPJ.                               
                    07  PSDCW018-03-CPF-CNPJ-NRO     PIC S9(009) COMP-3.
                    07  PSDCW018-03-CPF-CNPJ-CTR     PIC S9(002) COMP-3.
                    07  PSDCW018-03-CPF-CNPJ-FIL     PIC S9(004) COMP-3.
                05  PSDCW018-03-CCLUB                PIC S9(010) COMP-3.
                05  PSDCW018-03-TP-REG               PIC  X(003).       
                05  PSDCW018-03-CSEQ-CNTRL-FONE      PIC S9(003) COMP-3.
                05  PSDCW018-03-CPTCAO-ESPAC-TBELA   PIC S9(002) COMP-3.
                05  PSDCW018-03-CTPO-FONE-CLI        PIC S9(003) COMP-3.
                05  PSDCW018-03-CTPO-LIN-TFONI       PIC S9(003) COMP-3.
                05  PSDCW018-03-CINDCD-FONE-INVLD    PIC  X(001).       
                05  PSDCW018-03-CFONE                PIC S9(008) COMP-3.
                05  PSDCW018-03-CID-RMAL-BIP-PAGER   PIC  X(015).       
                05  PSDCW018-03-ICNTAT-PSSOA         PIC  X(020).       
                05  PSDCW018-03-CDDDI                PIC  X(004).       
                05  PSDCW018-03-CDDDD                PIC  X(004).       
CPMALP********  CAMPOS NOVOS  ******************************************
.               05  PSDCW018-03-CEMPR                PIC S9(010) COMP-3.
.               05  PSDCW018-03-CAUTRZ-ENVIO-SMS     PIC  X(001).       
.               05  PSDCW018-03-CESPCE-ENDER         PIC S9(001) COMP-3.
.               05  PSDCW018-03-CTPO-FONE            PIC S9(002) COMP-3.
.               05  PSDCW018-03-CNVEL-CONFD-ENDER    PIC S9(003) COMP-3.
.               05  PSDCW018-03-CCOMPV-ENDER-PSSOA   PIC S9(003) COMP-3.
.               05  PSDCW018-03-DINIC-UTILZ-ENDER    PIC  X(010).       
.               05  PSDCW018-03-DFIM-UTILZ-ENDER     PIC  X(010).       
.               05  PSDCW018-03-IPSSOA-CNTAT-ENDER   PIC  X(040).       
.               05  PSDCW018-03-CENDER-PDRAO-PSSOA   PIC  X(001).       
.               05  PSDCW018-03-CUSO-ENDER-PSSOA     PIC S9(003) COMP-3.
.               05  PSDCW018-03-ICARGO-CNTAT         PIC  X(040).       
.               05  PSDCW018-03-CCLUB-ENDER-PSSOA    PIC S9(010) COMP-3.
.               05  PSDCW018-03-CEMPR-ENDER-PSSOA    PIC S9(010) COMP-3.
.               05  PSDCW018-03-CSEQ-ENDER-PSSOA     PIC S9(005) COMP-3.
CPMALP          05  PSDCW018-03-CCARGO-CLI-PF        PIC S9(005) COMP-3.
CPMTEL          05  PSDCW018-03-NLIN-TFONI           PIC S9(011) COMP-3.
                                                                        
      ******************************************************************
      **** TIPO REG.: 004 - CADUB0A8 - RFERENCIA-BANCARIA               
      ******************************************************************
                                                                        
           03   PSDCW018-04-REFT-BCRIA   REDEFINES                      
                                         PSDCW018-01-PSSOA-UNICA.       
                05  PSDCW018-04-CPF-CNPJ.                               
                    07  PSDCW018-04-CPF-CNPJ-NRO     PIC S9(009) COMP-3.
                    07  PSDCW018-04-CPF-CNPJ-CTR     PIC S9(002) COMP-3.
                    07  PSDCW018-04-CPF-CNPJ-FIL     PIC S9(004) COMP-3.
                05  PSDCW018-04-CCLUB                PIC S9(010) COMP-3.
                05  PSDCW018-04-TP-REG               PIC  X(003).       
                05  PSDCW018-04-CSEQ-BCRIA           PIC S9(003) COMP-3.
                05  PSDCW018-04-CPTCAO-ESPAC-TBELA   PIC S9(002) COMP-3.
                05  PSDCW018-04-CBCO-REFT            PIC S9(003) COMP-3.
                05  PSDCW018-04-EAG-REFT             PIC  X(040).       
                05  PSDCW018-04-CCTA-CORR-REFT       PIC S9(007) COMP-3.
                05  PSDCW018-04-CDIG-CTA-REFT        PIC  X(001).       
                05  PSDCW018-04-CMUN-REFT            PIC S9(009) COMP-3.
                05  PSDCW018-04-CSGL-UF              PIC  X(002).       
                05  PSDCW018-04-CDDD-FONE-REFT       PIC  X(004).       
                05  PSDCW018-04-CFONE-REFT           PIC S9(008) COMP-3.
                05  PSDCW018-04-CINDCD-OPER-CREDT    PIC  X(001).       
                05  PSDCW018-04-VLIM-CREDT-REFT   PIC S9(013)V99 COMP-3.
                05  PSDCW018-04-CINDCD-CONF-REFT     PIC  X(001).       
CPMALP********  CAMPOS NOVOS  ******************************************
.               05  PSDCW018-04-CEMPR                PIC S9(010) COMP-3.
.               05  PSDCW018-04-CAG-BCRIA            PIC S9(005) COMP-3.
.               05  PSDCW018-04-DABERT-CTA-REFT      PIC  X(010).       
.               05  PSDCW018-04-CMUN                 PIC S9(010) COMP-3.
CPMALP          05  PSDCW018-04-CUF                  PIC S9(006) COMP-3.
CPMTEL          05  PSDCW018-04-NLIN-TFONI           PIC S9(011) COMP-3.
                                                                        
      ******************************************************************
      **** TIPO REG.: 005 - CADUB0B6 - SEGURO-PESSOA                    
      ******************************************************************
                                                                        
           03   PSDCW018-05-SEGUR-PSSOA  REDEFINES                      
                                          PSDCW018-01-PSSOA-UNICA.      
                05  PSDCW018-05-CPF-CNPJ.                               
                    07  PSDCW018-05-CPF-CNPJ-NRO     PIC S9(009) COMP-3.
                    07  PSDCW018-05-CPF-CNPJ-CTR     PIC S9(002) COMP-3.
                    07  PSDCW018-05-CPF-CNPJ-FIL     PIC S9(004) COMP-3.
                05  PSDCW018-05-CCLUB                PIC S9(010) COMP-3.
                05  PSDCW018-05-TP-REG               PIC  X(003).       
                05  PSDCW018-05-CSEQ-SEGUR           PIC S9(004) COMP-3.
                05  PSDCW018-05-CPTCAO-ESPAC-TBELA   PIC S9(002) COMP-3.
                05  PSDCW018-05-CSEGDR               PIC S9(004) COMP-3.
                05  PSDCW018-05-CMODLD-RAMO-SEGUR    PIC S9(002) COMP-3.
                05  PSDCW018-05-DANO-MES-SEGUR       PIC S9(006) COMP-3.
                05  PSDCW018-05-VTOT-SEGUR-PSSOA  PIC S9(013)V99 COMP-3.
CPMALP********  CAMPOS NOVOS  ******************************************
.               05  PSDCW018-05-CEMPR                PIC S9(010) COMP-3.
CPMALP          05  PSDCW018-05-CMODLD-SEGUR         PIC S9(003) COMP-3.
                                                                        
      ******************************************************************
      **** TIPO REG.: 006 - CADUB069 - INFORMACAO-PESSOAL               
      ******************************************************************
                                                                        
           03   PSDCW018-06-INFO-PSOA    REDEFINES                      
                                        PSDCW018-01-PSSOA-UNICA.        
                05  PSDCW018-06-CPF-CNPJ.                               
                    07  PSDCW018-06-CPF-CNPJ-NRO     PIC S9(009) COMP-3.
                    07  PSDCW018-06-CPF-CNPJ-CTR     PIC S9(002) COMP-3.
                    07  PSDCW018-06-CPF-CNPJ-FIL     PIC S9(004) COMP-3.
                05  PSDCW018-06-CCLUB                PIC S9(010) COMP-3.
                05  PSDCW018-06-TP-REG               PIC  X(003).       
                05  PSDCW018-06-CPTCAO-ESPAC-TBELA   PIC S9(002) COMP-3.
                05  PSDCW018-06-IPSSOA-COPLT         PIC  X(070).       
                05  PSDCW018-06-CNAC                 PIC S9(003) COMP-3.
                05  PSDCW018-06-CMUN-NASC            PIC S9(009) COMP-3.
                05  PSDCW018-06-CSGL-UF              PIC  X(002).       
                05  PSDCW018-06-CTPO-TTLAR-CPF       PIC S9(002) COMP-3.
                05  PSDCW018-06-CTPO-DOCTO           PIC S9(003) COMP-3.
                05  PSDCW018-06-CDOCTO               PIC  X(015).       
                05  PSDCW018-06-CSEXO                PIC  X(001).       
                05  PSDCW018-06-DEMIS-DOCTO          PIC  X(010).       
                05  PSDCW018-06-DVCTO-DOCTO          PIC  X(010).       
                05  PSDCW018-06-CORG-EMISR           PIC  X(004).       
                05  PSDCW018-06-CUF-ORG-EMISR        PIC  X(002).       
                05  PSDCW018-06-CGRAU-FORMC          PIC S9(002) COMP-3.
                05  PSDCW018-06-CSIT-FORMC           PIC S9(001) COMP-3.
                05  PSDCW018-06-DESPOL               PIC  X(010).       
                05  PSDCW018-06-CTPO-FONTE-INF       PIC S9(003) COMP-3.
                05  PSDCW018-06-CTPO-SEDE-RESID      PIC S9(003) COMP-3.
                05  PSDCW018-06-CID-TPO-PSSOA        PIC  X(001).       
                05  PSDCW018-06-DMES-ANO-RESID       PIC S9(006) COMP-3.
                05  PSDCW018-06-QANO-RESID-ANTER     PIC S9(002) COMP-3.
                05  PSDCW018-06-QMES-RESID-ANTER     PIC S9(002) COMP-3.
                05  PSDCW018-06-EEMAIL-PSSOA         PIC  X(070).       
                05  PSDCW018-06-EHPAGE-PSSOA         PIC  X(070).       
                05  PSDCW018-06-CEMAIL-INVLD         PIC  X(001).       
                05  PSDCW018-06-IMUN-NAT-ESTRG       PIC  X(040).       
CPMALP********  CAMPOS NOVOS  ******************************************
.               05  PSDCW018-06-CEMPR                PIC S9(010) COMP-3.
.               05  PSDCW018-06-CPAIS                PIC S9(003) COMP-3.
.               05  PSDCW018-06-DINIC-FORMC-PSSOA    PIC  X(010).       
.               05  PSDCW018-06-DFNAL-FORMC-PSSOA    PIC  X(010).       
.               05  PSDCW018-06-CCPF-INFO-OBITO      PIC S9(009) COMP-3.
.               05  PSDCW018-06-CCTRL-CPF-OBITO      PIC S9(002) COMP-3.
.               05  PSDCW018-06-CSMEST-CURSO         PIC S9(002) COMP-3.
.               05  PSDCW018-06-CRESDT-DOMCL-UNIC    PIC  X(010).       
.               05  PSDCW018-06-IAPLDO-PSSOA         PIC  X(040).       
.               05  PSDCW018-06-CUF-INSTC-ENSNO      PIC S9(006) COMP-3.
.               05  PSDCW018-06-CMUN                 PIC S9(010) COMP-3.
.               05  PSDCW018-06-CUF                  PIC S9(006) COMP-3.
.               05  PSDCW018-06-NCURSO-INSTC-ENSNO   PIC S9(005) COMP-3.
.               05  PSDCW018-06-NINSTC-ENSNO         PIC S9(005) COMP-3.
CPMALP          05  PSDCW018-06-CCAPAC-CVIL          PIC S9(002) COMP-3.
                                                                        
      ******************************************************************
      **** TIPO REG.: 007 - CADUB065 - INFORMACAO-FAMILIAR              
      ******************************************************************
                                                                        
           03   PSDCW018-07-INFO-FAMRL   REDEFINES                      
                                         PSDCW018-01-PSSOA-UNICA.       
                05  PSDCW018-07-CPF-CNPJ.                               
                    07  PSDCW018-07-CPF-CNPJ-NRO     PIC S9(009) COMP-3.
                    07  PSDCW018-07-CPF-CNPJ-CTR     PIC S9(002) COMP-3.
                    07  PSDCW018-07-CPF-CNPJ-FIL     PIC S9(004) COMP-3.
                05  PSDCW018-07-CCLUB                PIC S9(010) COMP-3.
                05  PSDCW018-07-TP-REG               PIC  X(003).       
                05  PSDCW018-07-CPTCAO-ESPAC-TBELA   PIC S9(002) COMP-3.
                05  PSDCW018-07-IPAI-PSSOA-COPLT     PIC  X(070).       
                05  PSDCW018-07-IMAE-PSSOA-COPLT     PIC  X(070).       
                05  PSDCW018-07-CEST-CVIL-RGIME      PIC S9(002) COMP-3.
                05  PSDCW018-07-ICONJG-PSSOA         PIC  X(070).       
                05  PSDCW018-07-CCPF-CONJG           PIC S9(009) COMP-3.
                05  PSDCW018-07-CCTRL-CPF-CONJG      PIC S9(002) COMP-3.
                05  PSDCW018-07-CISENC-CPF-CNPJ      PIC  X(001).       
                05  PSDCW018-07-CMOTVO-ISENC-CPF     PIC S9(001) COMP-3.
                05  PSDCW018-07-DNASC-CONJG          PIC  X(010).       
                05  PSDCW018-07-CPROFS-CONJG         PIC S9(005) COMP-3.
                05  PSDCW018-07-CCATEG-PROFS-CONJG   PIC S9(005) COMP-3.
                05  PSDCW018-07-DINIC-VALDD          PIC  X(010).       
                05  PSDCW018-07-IEMPR-CONJG          PIC  X(070).       
CPMALP********  CAMPOS NOVOS  ******************************************
.               05  PSDCW018-07-CEMPR                PIC S9(010) COMP-3.
.               05  PSDCW018-07-CMOTVO-AUSEN-MAE     PIC S9(001) COMP-3.
.               05  PSDCW018-07-CMOTVO-AUSEN-PAI     PIC S9(001) COMP-3.
CPMALP          05  PSDCW018-07-QDEPDT               PIC S9(003) COMP-3.
                                                                        
      ******************************************************************
      **** TIPO REG.: 008 - CADUB067 - INFORMACAO-PROFISSIONAL          
      ******************************************************************
                                                                        
           03   PSDCW018-08-INFO-PROFS   REDEFINES                      
                                         PSDCW018-01-PSSOA-UNICA.       
                05  PSDCW018-08-CPF-CNPJ.                               
                    07  PSDCW018-08-CPF-CNPJ-NRO     PIC S9(009) COMP-3.
                    07  PSDCW018-08-CPF-CNPJ-CTR     PIC S9(002) COMP-3.
                    07  PSDCW018-08-CPF-CNPJ-FIL     PIC S9(004) COMP-3.
                05  PSDCW018-08-CCLUB                PIC S9(010) COMP-3.
                05  PSDCW018-08-TP-REG               PIC  X(003).       
                05  PSDCW018-08-CPTCAO-ESPAC-TBELA   PIC S9(002) COMP-3.
                05  PSDCW018-08-IRZ-SCIAL            PIC  X(070).       
                05  PSDCW018-08-CCNPJ-EMPR           PIC S9(009) COMP-3.
                05  PSDCW018-08-CCTRL-CNPJ-EMPR      PIC S9(002) COMP-3.
                05  PSDCW018-08-CFLIAL-CNPJ-EMPR     PIC S9(004) COMP-3.
                05  PSDCW018-08-CPORTE-EMPR          PIC S9(003) COMP-3.
                05  PSDCW018-08-CCOMPS-EMPR          PIC S9(001) COMP-3.
                05  PSDCW018-08-CPROFS-PF            PIC S9(005) COMP-3.
                05  PSDCW018-08-CCATEG-PROFS-CREDT   PIC S9(005) COMP-3.
                05  PSDCW018-08-DINIC-VALDD          PIC  X(010).       
                05  PSDCW018-08-CCARGO-CLI-PF        PIC S9(005) COMP-3.
                05  PSDCW018-08-DMES-ANO-ATVDD       PIC S9(006) COMP-3.
                05  PSDCW018-08-DADMIS               PIC  X(010).       
                05  PSDCW018-08-CCNPJ-ANTER-CLI      PIC S9(009) COMP-3.
                05  PSDCW018-08-CCTRL-CNPJ-ANTER     PIC S9(002) COMP-3.
                05  PSDCW018-08-CFLIAL-CNPJ-ANTER    PIC S9(004) COMP-3.
                05  PSDCW018-08-IRZ-SCIAL-ANTER      PIC  X(070).       
                05  PSDCW018-08-QANO-EMPR-ANTER      PIC S9(002) COMP-3.
                05  PSDCW018-08-QMES-EMPR-ANTER      PIC S9(002) COMP-3.
                05  PSDCW018-08-VRENDA-MES        PIC S9(013)V99 COMP-3.
                05  PSDCW018-08-VRENDA-INFRD      PIC S9(013)V99 COMP-3.
                05  PSDCW018-08-VRENDA-FAMLR-MES  PIC S9(013)V99 COMP-3.
                05  PSDCW018-08-CID-COMPV-RENDA      PIC  X(001).       
                05  PSDCW018-08-RDEPTO-ALOC          PIC  X(040).       
                05  PSDCW018-08-CTPO-COMPV-RENDA     PIC S9(002) COMP-3.
CPMALP********  CAMPOS NOVOS  ******************************************
CPMALP          05  PSDCW018-08-CEMPR                PIC S9(010) COMP-3.
                                                                        
      ******************************************************************
      **** TIPO REG.: 009 - CADUB089 - PATRIMONIO-IMOVEL                
      ******************************************************************
                                                                        
           03   PSDCW018-09-PATRM-IMOV   REDEFINES                      
                                         PSDCW018-01-PSSOA-UNICA.       
                05  PSDCW018-09-CPF-CNPJ.                               
                    07  PSDCW018-09-CPF-CNPJ-NRO     PIC S9(009) COMP-3.
                    07  PSDCW018-09-CPF-CNPJ-CTR     PIC S9(002) COMP-3.
                    07  PSDCW018-09-CPF-CNPJ-FIL     PIC S9(004) COMP-3.
                05  PSDCW018-09-CCLUB                PIC S9(010) COMP-3.
                05  PSDCW018-09-TP-REG               PIC  X(003).       
                05  PSDCW018-09-CSEQ-PATRM-IMOV      PIC S9(005) COMP-3.
                05  PSDCW018-09-CPTCAO-ESPAC-TBELA   PIC S9(002) COMP-3.
                05  PSDCW018-09-CESPEC-IMOV          PIC S9(003) COMP-3.
                05  PSDCW018-09-CTPO-ESCRT           PIC  X(001).       
                05  PSDCW018-09-CSIT-IMOV            PIC S9(001) COMP-3.
                05  PSDCW018-09-CINDCD-IMOV-EXPDO    PIC  X(001).       
                05  PSDCW018-09-CINDCD-IMOV-IMPNH    PIC  X(001).       
                05  PSDCW018-09-CINDCD-IMOV-RESID    PIC  X(001).       
                05  PSDCW018-09-CMUN-IMOV            PIC S9(009) COMP-3.
                05  PSDCW018-09-DAQUIS-IMOV          PIC  X(010).       
                05  PSDCW018-09-CSGL-UF              PIC  X(002).       
                05  PSDCW018-09-DREG-IMOV-CARTR      PIC  X(010).       
                05  PSDCW018-09-CCARTR-REG-IMOV      PIC S9(002) COMP-3.
                05  PSDCW018-09-CMATR-REG-IMOV       PIC S9(007) COMP-3.
                05  PSDCW018-09-CLIVRO-REG-IMOV      PIC  X(004).       
                05  PSDCW018-09-CFL-REG-IMOV         PIC S9(004) COMP-3.
                05  PSDCW018-09-MAREA-TOT-IMOV    PIC S9(006)V99 COMP-3.
                05  PSDCW018-09-MAREA-CONSD-IMOV  PIC S9(006)V99 COMP-3.
                05  PSDCW018-09-CTPO-UND-MEDD        PIC S9(001) COMP-3.
                05  PSDCW018-09-VATUAL-IMOV       PIC S9(013)V99 COMP-3.
                05  PSDCW018-09-ICREDR-HIPOT-FINAN   PIC  X(070).       
                05  PSDCW018-09-DVCTO-HIPOT-FINAN    PIC  X(010).       
                05  PSDCW018-09-VHIPOT-FINAN      PIC S9(013)V99 COMP-3.
                05  PSDCW018-09-CCARTR-IMOV-REG      PIC  X(010).       
                05  PSDCW018-09-CMATR-IMOV-REG       PIC  X(010).       
                05  PSDCW018-09-NFL-IMOV-REG         PIC  X(007).       
CPMALP********  CAMPOS NOVOS  ******************************************
.               05  PSDCW018-09-CEMPR                PIC S9(010) COMP-3.
.               05  PSDCW018-09-CTPO-BEM-IMOV        PIC S9(003) COMP-3.
.               05  PSDCW018-09-CTPO-FNALD-IMOV      PIC S9(003) COMP-3.
.               05  PSDCW018-09-CINDCD-ECONM-MOEDA   PIC S9(005) COMP-3.
.               05  PSDCW018-09-CIDTFD-IMOV-COMPV    PIC  X(001).       
.               05  PSDCW018-09-CCLUB-ENDER-PSSOA    PIC S9(010) COMP-3.
.               05  PSDCW018-09-CEMPR-ENDER-PSSOA    PIC S9(010) COMP-3.
.               05  PSDCW018-09-CSEQ-ENDER-PSSOA     PIC S9(005) COMP-3.
.               05  PSDCW018-09-CMUN                 PIC S9(010) COMP-3.
.               05  PSDCW018-09-CUF                  PIC S9(006) COMP-3.
CPMALP          05  PSDCW018-09-CDOCTO               PIC S9(005) COMP-3.
                                                                        
      ******************************************************************
      **** TIPO REG.: 010 - CADUB087 - PATRIMONIO-OUTRO-BEM             
      ******************************************************************
                                                                        
           03   PSDCW018-10-PATRM-BEM    REDEFINES                      
                                        PSDCW018-01-PSSOA-UNICA.        
                05  PSDCW018-10-CPF-CNPJ.                               
                    07  PSDCW018-10-CPF-CNPJ-NRO     PIC S9(009) COMP-3.
                    07  PSDCW018-10-CPF-CNPJ-CTR     PIC S9(002) COMP-3.
                    07  PSDCW018-10-CPF-CNPJ-FIL     PIC S9(004) COMP-3.
                05  PSDCW018-10-CCLUB                PIC S9(010) COMP-3.
                05  PSDCW018-10-TP-REG               PIC  X(003).       
                05  PSDCW018-10-CSEQ-PATRM-BEM       PIC S9(005) COMP-3.
                05  PSDCW018-10-CPTCAO-ESPAC-TBELA   PIC S9(002) COMP-3.
                05  PSDCW018-10-CTPO-BEM-CLI         PIC S9(003) COMP-3.
                05  PSDCW018-10-CSIT-BEM             PIC  X(001).       
                05  PSDCW018-10-VATUAL-PATRM-BEM  PIC S9(013)V99 COMP-3.
                05  PSDCW018-10-CINDCD-ONUS-BEM      PIC  X(001).       
                05  PSDCW018-10-VONUS-BEM         PIC S9(013)V99 COMP-3.
                05  PSDCW018-10-CINDCD-ALIEN         PIC  X(001).       
                05  PSDCW018-10-DMES-ANO-ALIEN       PIC S9(006) COMP-3.
CPMALP********  CAMPOS NOVOS  ******************************************
CPMALP          05  PSDCW018-10-CEMPR                PIC S9(010) COMP-3.
                                                                        
      ******************************************************************
      **** TIPO REG.: 011 - CADUB091 - PATRIMONIO-VEICULO               
      ******************************************************************
                                                                        
           03   PSDCW018-11-PATRM-VEIC   REDEFINES                      
                                         PSDCW018-01-PSSOA-UNICA.       
                05  PSDCW018-11-CPF-CNPJ.                               
                    07  PSDCW018-11-CPF-CNPJ-NRO     PIC S9(009) COMP-3.
                    07  PSDCW018-11-CPF-CNPJ-CTR     PIC S9(002) COMP-3.
                    07  PSDCW018-11-CPF-CNPJ-FIL     PIC S9(004) COMP-3.
                05  PSDCW018-11-CCLUB                PIC S9(010) COMP-3.
                05  PSDCW018-11-TP-REG               PIC  X(003).       
                05  PSDCW018-11-CSEQ-PATRM-VEIC      PIC S9(004) COMP-3.
                05  PSDCW018-11-CPTCAO-ESPAC-TBELA   PIC S9(002) COMP-3.
                05  PSDCW018-11-CTPO-VEIC            PIC S9(001) COMP-3.
                05  PSDCW018-11-CMARCA-VEIC          PIC S9(003) COMP-3.
                05  PSDCW018-11-CMOD-VEIC            PIC S9(005) COMP-3.
                05  PSDCW018-11-CCOMBS               PIC  X(001).       
                05  PSDCW018-11-DANO-VEIC            PIC S9(004) COMP-3.
                05  PSDCW018-11-CPLACA-VEIC          PIC  X(008).       
                05  PSDCW018-11-VATUAL-VEIC       PIC S9(013)V99 COMP-3.
                05  PSDCW018-11-CINDCD-ONUS-BEM      PIC  X(001).       
                05  PSDCW018-11-VONUS-BEM         PIC S9(013)V99 COMP-3.
                05  PSDCW018-11-CINDCD-ALIEN         PIC  X(001).       
                05  PSDCW018-11-DMES-ANO-ALIEN       PIC S9(006) COMP-3.
CPMALP********  CAMPOS NOVOS  ******************************************
.               05  PSDCW018-11-CEMPR                PIC S9(010) COMP-3.
.               05  PSDCW018-11-DANO-MOD-VEIC        PIC S9(004) COMP-3.
.               05  PSDCW018-11-CTPO-ESPCE-VEIC      PIC S9(003) COMP-3.
.               05  PSDCW018-11-CINDCD-ECONM-MOEDA   PIC S9(005) COMP-3.
.               05  PSDCW018-11-CTPO-AVALC-VEIC      PIC S9(001) COMP-3.
.               05  PSDCW018-11-VPCELA-VEIC       PIC S9(013)V99 COMP-3.
.               05  PSDCW018-11-QPCELA               PIC S9(005) COMP-3.
.               05  PSDCW018-11-QPCELA-PG            PIC S9(005) COMP-3.
.               05  PSDCW018-11-NINSTC-FINCR-PATRM   PIC S9(004) COMP-3.
.               05  PSDCW018-11-CFORMA-ALIEN-PATRM   PIC S9(002) COMP-3.
.               05  PSDCW018-11-VTOT-CONTR-ALIEN  PIC S9(013)V99 COMP-3.
CPMALP          05  PSDCW018-11-CTPO-COMBS           PIC S9(003) COMP-3.
                                                                        
      ******************************************************************
      **** TIPO REG.: 012 - CADUB0B0 - REFERENCIA-CARTAO-CREDITO        
      ******************************************************************
                                                                        
           03   PSDCW018-12-REFT-CATAO-CREDT   REDEFINES                
                                               PSDCW018-01-PSSOA-UNICA. 
                05  PSDCW018-12-CPF-CNPJ.                               
                    07  PSDCW018-12-CPF-CNPJ-NRO     PIC S9(009) COMP-3.
                    07  PSDCW018-12-CPF-CNPJ-CTR     PIC S9(002) COMP-3.
                    07  PSDCW018-12-CPF-CNPJ-FIL     PIC S9(004) COMP-3.
                05  PSDCW018-12-CCLUB                PIC S9(010) COMP-3.
                05  PSDCW018-12-TP-REG               PIC  X(003).       
                05  PSDCW018-12-CSEQ-CATAO-CREDT     PIC S9(003) COMP-3.
                05  PSDCW018-12-CPTCAO-ESPAC-TBELA   PIC S9(002) COMP-3.
                05  PSDCW018-12-CTPO-UTILZ-CATAO     PIC S9(001) COMP-3.
                05  PSDCW018-12-CBANDE-CATAO-CREDT   PIC S9(002) COMP-3.
                05  PSDCW018-12-CINDCD-CATAO-UTLZD   PIC  X(001).       
                05  PSDCW018-12-CBCO-CATAO-CREDT     PIC S9(003) COMP-3.
                05  PSDCW018-12-IADM-CATAO-CREDT     PIC  X(070).       
CPMALP********  CAMPOS NOVOS  ******************************************
.               05  PSDCW018-12-CEMPR                PIC S9(010) COMP-3.
.               05  PSDCW018-12-VMED-CATAO-CREDT  PIC S9(013)V99 COMP-3.
.               05  PSDCW018-12-CADM-CATAO-CREDT     PIC S9(004) COMP-3.
CPMALP          05  PSDCW018-12-NTPO-CATAO-CREDT     PIC S9(002) COMP-3.
                                                                        
      ******************************************************************
      **** TIPO REG.: 013 - CADUB0B4 - RENDA-DESPESA-PESSOA             
      ******************************************************************
                                                                        
           03   PSDCW018-13-RENDA-DESP   REDEFINES                      
                                         PSDCW018-01-PSSOA-UNICA.       
                05  PSDCW018-13-CPF-CNPJ.                               
                    07  PSDCW018-13-CPF-CNPJ-NRO     PIC S9(009) COMP-3.
                    07  PSDCW018-13-CPF-CNPJ-CTR     PIC S9(002) COMP-3.
                    07  PSDCW018-13-CPF-CNPJ-FIL     PIC S9(004) COMP-3.
                05  PSDCW018-13-CCLUB                PIC S9(010) COMP-3.
                05  PSDCW018-13-TP-REG               PIC  X(003).       
                05  PSDCW018-13-CTPO-RENDA-DESP      PIC S9(005) COMP-3.
                05  PSDCW018-13-CINDCD-RENDA-DESP    PIC S9(001) COMP-3.
                05  PSDCW018-13-CSEQ-RENDA-DESP      PIC S9(003) COMP-3.
                05  PSDCW018-13-CPTCAO-ESPAC-TBELA   PIC S9(002) COMP-3.
                05  PSDCW018-13-VRENDA-DESP-CLI   PIC S9(013)V99 COMP-3.
CPMALP********  CAMPOS NOVOS  ******************************************
.               05  PSDCW018-13-CEMPR                PIC S9(010) COMP-3.
.               05  PSDCW018-13-DMES-RENDA-DESP      PIC S9(006) COMP-3.
CPMALP          05  PSDCW018-13-CPERDC-RENDA-DESP    PIC S9(002) COMP-3.
                                                                        
      ******************************************************************
      **** TIPO REG.: 014 - CADUB097 - PARTICIPACAO-EMPRESA-PF          
      ******************************************************************
                                                                        
           03   PSDCW018-14-PRTCP-PF     REDEFINES                      
                                       PSDCW018-01-PSSOA-UNICA.         
                05  PSDCW018-14-CPF-CNPJ.                               
                    07  PSDCW018-14-CPF-CNPJ-NRO     PIC S9(009) COMP-3.
                    07  PSDCW018-14-CPF-CNPJ-CTR     PIC S9(002) COMP-3.
                    07  PSDCW018-14-CPF-CNPJ-FIL     PIC S9(004) COMP-3.
                05  PSDCW018-14-CCLUB                PIC S9(010) COMP-3.
                05  PSDCW018-14-TP-REG               PIC  X(003).       
                05  PSDCW018-14-CSEQ-PRTCP-EMPR      PIC S9(003) COMP-3.
                05  PSDCW018-14-CPTCAO-ESPAC-TBELA   PIC S9(002) COMP-3.
                05  PSDCW018-14-IRZ-SCIAL            PIC  X(070).       
                05  PSDCW018-14-CISENC-CPF-CNPJ      PIC  X(001).       
                05  PSDCW018-14-CMOTVO-ISENC-CPF     PIC S9(001) COMP-3.
                05  PSDCW018-14-CCNPJ-EMPR           PIC S9(009) COMP-3.
                05  PSDCW018-14-CFLIAL-CNPJ          PIC S9(004) COMP-3.
                05  PSDCW018-14-CCTRL-CNPJ-EMPR      PIC S9(002) COMP-3.
                05  PSDCW018-14-CCLASS-ATVDD-ECONC   PIC  X(002).       
                05  PSDCW018-14-CRAMO-ATVDD-ECONC    PIC S9(003) COMP-3.
                05  PSDCW018-14-CSRAMO-ATVDD-ECONC   PIC S9(003) COMP-3.
                05  PSDCW018-14-CATVDD-ECONC         PIC S9(003) COMP-3.
                05  PSDCW018-14-PPRTCP-EMPR       PIC S9(003)V99 COMP-3.
                05  PSDCW018-14-DMES-ANO-PRTCP       PIC S9(006) COMP-3.
                05  PSDCW018-14-DMES-ANO-FIM-EMPR    PIC S9(006) COMP-3.
                05  PSDCW018-14-DFUNDC-EMPR          PIC  X(010).       
                05  PSDCW018-14-CPORTE-EMPR          PIC S9(003) COMP-3.
                05  PSDCW018-14-CTPO-SEDE-RESID      PIC S9(003) COMP-3.
                05  PSDCW018-14-CID-TPO-PSSOA        PIC  X(001).       
                05  PSDCW018-14-QFUNC-EMPR           PIC S9(005) COMP-3.
                05  PSDCW018-14-VCAPTL-SCIAL      PIC S9(013)V99 COMP-3.
                05  PSDCW018-14-VFATMT-LIQ-ANO    PIC S9(013)V99 COMP-3.
CPMALP********  CAMPOS NOVOS  ******************************************
.               05  PSDCW018-14-CEMPR                PIC S9(010) COMP-3.
CPMALP          05  PSDCW018-14-CCLASF-PORTE-EMPR    PIC S9(002) COMP-3.
                                                                        
      ******************************************************************
      **** TIPO REG.: 015 - CADUB0A1 - REFERENCIA-PESSOAL-COMERCIAL     
      ******************************************************************
                                                                        
           03   PSDCW018-15-PSSOA-COML   REDEFINES                      
                                         PSDCW018-01-PSSOA-UNICA.       
                05  PSDCW018-15-CPF-CNPJ.                               
                    07  PSDCW018-15-CPF-CNPJ-NRO     PIC S9(009) COMP-3.
                    07  PSDCW018-15-CPF-CNPJ-CTR     PIC S9(002) COMP-3.
                    07  PSDCW018-15-CPF-CNPJ-FIL     PIC S9(004) COMP-3.
                05  PSDCW018-15-CCLUB                PIC S9(010) COMP-3.
                05  PSDCW018-15-TP-REG               PIC  X(003).       
                05  PSDCW018-15-CSEQ-PSSOA-COML      PIC S9(002) COMP-3.
                05  PSDCW018-15-CPTCAO-ESPAC-TBELA   PIC S9(002) COMP-3.
                05  PSDCW018-15-CTPO-REFT            PIC S9(001) COMP-3.
                05  PSDCW018-15-IPSSOA-REFT          PIC  X(040).       
                05  PSDCW018-15-CMUN-REFT            PIC S9(009) COMP-3.
                05  PSDCW018-15-CSGL-UF              PIC  X(002).       
                05  PSDCW018-15-CDDD-FONE-REFT       PIC  X(004).       
                05  PSDCW018-15-CFONE-REFT           PIC S9(008) COMP-3.
                05  PSDCW018-15-CINDCD-CONF-REFT     PIC  X(001).       
CPMALP********  CAMPOS NOVOS  ******************************************
.               05  PSDCW018-15-CEMPR                PIC S9(010) COMP-3.
.               05  PSDCW018-15-CMUN                 PIC S9(010) COMP-3.
CPMALP          05  PSDCW018-15-CUF                  PIC S9(006) COMP-3.
CPMTEL          05  PSDCW018-15-NLIN-TFONI           PIC S9(011) COMP-3.
                                                                        
      ******************************************************************
      **** TIPO REG.: 016 - CADUB003 - BASICO-PJ                        
      ******************************************************************
                                                                        
           03   PSDCW018-16-BSICO-PJ     REDEFINES                      
                                       PSDCW018-01-PSSOA-UNICA.         
                05  PSDCW018-16-CPF-CNPJ.                               
                    07  PSDCW018-16-CPF-CNPJ-NRO     PIC S9(009) COMP-3.
                    07  PSDCW018-16-CPF-CNPJ-CTR     PIC S9(002) COMP-3.
                    07  PSDCW018-16-CPF-CNPJ-FIL     PIC S9(004) COMP-3.
                05  PSDCW018-16-CCLUB                PIC S9(010) COMP-3.
                05  PSDCW018-16-TP-REG               PIC  X(003).       
                05  PSDCW018-16-CPTCAO-ESPAC-TBELA   PIC S9(002) COMP-3.
                05  PSDCW018-16-IRZ-SCIAL            PIC  X(070).       
                05  PSDCW018-16-IFANTS-EMPR-COPLT    PIC  X(070).       
                05  PSDCW018-16-CCLASS-ATVDD-ECONC   PIC  X(002).       
                05  PSDCW018-16-CRAMO-ATVDD-ECONC    PIC S9(003) COMP-3.
                05  PSDCW018-16-CSRAMO-ATVDD-ECONC   PIC S9(003) COMP-3.
                05  PSDCW018-16-CATVDD-ECONC         PIC S9(003) COMP-3.
                05  PSDCW018-16-CISENC-CPF-CNPJ      PIC  X(001).       
                05  PSDCW018-16-CCNPJ-INCPD          PIC S9(009) COMP-3.
                05  PSDCW018-16-CCTRL-CNPJ-INCPD     PIC S9(002) COMP-3.
                05  PSDCW018-16-CFLIAL-CNPJ-INCPD    PIC S9(004) COMP-3.
                05  PSDCW018-16-IRZ-SCIAL-INCPD      PIC  X(070).       
                05  PSDCW018-16-DINCPC               PIC  X(010).       
                05  PSDCW018-16-CTPO-FONTE-INFO      PIC S9(003) COMP-3.
                05  PSDCW018-16-DMES-ANO-FATMT       PIC S9(006) COMP-3.
                05  PSDCW018-16-VFATMT-LIQ-ANO    PIC S9(013)V99 COMP-3.
                05  PSDCW018-16-CTPO-CAPTL-EMPR      PIC S9(003) COMP-3.
                05  PSDCW018-16-CPAIS-CMBIO          PIC S9(005) COMP-3.
                05  PSDCW018-16-CCNSTT-EMPR-CREDT    PIC S9(003) COMP-3.
                05  PSDCW018-16-CPORTE-EMPR          PIC S9(003) COMP-3.
                05  PSDCW018-16-DINIC-SEDE           PIC S9(006) COMP-3.
                05  PSDCW018-16-CTPO-SEDE-RESID      PIC S9(003) COMP-3.
                05  PSDCW018-16-CID-TPO-PSSOA        PIC  X(001).       
                05  PSDCW018-16-EHPAGE-PSSOA         PIC  X(070).       
                05  PSDCW018-16-EEMAIL-PSSOA         PIC  X(070).       
                05  PSDCW018-16-CEMAIL-INVLD         PIC  X(001).       
                05  PSDCW018-16-CDDD-CNTAT-EMPR      PIC  X(004).       
                05  PSDCW018-16-CFONE-CNTAT-EMPR     PIC S9(008) COMP-3.
                05  PSDCW018-16-CRMAL-CNTAT-EMPR     PIC  X(006).       
                05  PSDCW018-16-ICNTAT-EMPR          PIC  X(050).       
                05  PSDCW018-16-CCARGO-CLI-PF        PIC S9(005) COMP-3.
CPMALP********  CAMPOS NOVOS  ******************************************
.               05  PSDCW018-16-CEMPR                PIC S9(010) COMP-3.
.               05  PSDCW018-16-CNATUZ-JURID         PIC S9(003) COMP-3.
.               05  PSDCW018-16-CRGIME-TRIBT-PSSOA   PIC S9(001) COMP-3.
.               05  PSDCW018-16-DENCRR-ATVDD-EMPR    PIC  X(010).       
CPMALP          05  PSDCW018-16-CPAIS                PIC S9(003) COMP-3.
CPMTEL          05  PSDCW018-16-NLIN-TFONI           PIC S9(011) COMP-3.
ALAN            05  PSDCW018-16-CAGPTO-ATVDD-CONC    PIC S9(009) COMP-3.
                                                                        
      ******************************************************************
      **** TIPO REG.: 017 - CADUB008 - COMPLEMENTAR-PJ                  
      ******************************************************************
                                                                        
           03   PSDCW018-17-COMPL-PJ     REDEFINES                      
                                       PSDCW018-01-PSSOA-UNICA.         
                05  PSDCW018-17-CPF-CNPJ.                               
                    07  PSDCW018-17-CPF-CNPJ-NRO     PIC S9(009) COMP-3.
                    07  PSDCW018-17-CPF-CNPJ-CTR     PIC S9(002) COMP-3.
                    07  PSDCW018-17-CPF-CNPJ-FIL     PIC S9(004) COMP-3.
                05  PSDCW018-17-CCLUB                PIC S9(010) COMP-3.
                05  PSDCW018-17-TP-REG               PIC  X(003).       
                05  PSDCW018-17-CPTCAO-ESPAC-TBELA   PIC S9(002) COMP-3.
                05  PSDCW018-17-CMUN-MTRIZ           PIC S9(009) COMP-3.
                05  PSDCW018-17-CUF-MTRIZ            PIC  X(002).       
                05  PSDCW018-17-CREG-JUNTA-COML      PIC S9(009) COMP-3.
                05  PSDCW018-17-DREG-JUNTA-COML      PIC  X(010).       
                05  PSDCW018-17-CINSCR-MUN           PIC  X(030).       
                05  PSDCW018-17-CINSCR-EST-EMPR      PIC  X(020).       
                05  PSDCW018-17-VCAPTL-REGTD      PIC S9(013)V99 COMP-3.
                05  PSDCW018-17-CCAPTL-ABERT         PIC  X(001).       
                05  PSDCW018-17-QTOT-ACAO-ORDNR      PIC S9(009) COMP-3.
                05  PSDCW018-17-QTOT-ACAO-PREFC      PIC S9(009) COMP-3.
                05  PSDCW018-17-VCAPTL-AUTRZ      PIC S9(013)V99 COMP-3.
                05  PSDCW018-17-VCAPTL-REALZ      PIC S9(013)V99 COMP-3.
                05  PSDCW018-17-CPRTCP-CAPTL-EST     PIC  X(001).       
                05  PSDCW018-17-PPRTCP-CAPTL-EST  PIC S9(003)V99 COMP-3.
                05  PSDCW018-17-CCAPTL-ESTRG         PIC  X(001).       
                05  PSDCW018-17-PPRTCP-CAPTL-ESTR PIC S9(003)V99 COMP-3.
                05  PSDCW018-17-NREG-JUNTA-COML      PIC S9(011) COMP-3.
CPMALP********  CAMPOS NOVOS  ******************************************
.               05  PSDCW018-17-CEMPR                PIC S9(010) COMP-3.
.               05  PSDCW018-17-CCPF-CNPJ            PIC S9(009) COMP-3.
.               05  PSDCW018-17-CFLIAL-CPF-CNPJ      PIC S9(004) COMP-3.
.               05  PSDCW018-17-CCTRL-CPF-CNPJ       PIC S9(002) COMP-3.
.               05  PSDCW018-17-VRENDA-BRUTA-AGROP                      
.                                                 PIC S9(013)V99 COMP-3.
.               05  PSDCW018-17-PCAPTL-SCIAL      PIC S9(003)V99 COMP-3.
.               05  PSDCW018-17-DULT-ATULZ-SCIAL     PIC  X(010).       
.               05  PSDCW018-17-PCAPTL-PBLIC-NACIO                      
.                                                 PIC S9(003)V99 COMP-3.
.               05  PSDCW018-17-PCAPTL-PBLIC-ESTRG                      
.                                                 PIC S9(003)V99 COMP-3.
.               05  PSDCW018-17-PCAPTL-PRIVD-NACIO                      
.                                                 PIC S9(003)V99 COMP-3.
.               05  PSDCW018-17-PCAPTL-PRIVD-ESTRG                      
.                                                 PIC S9(003)V99 COMP-3.
.               05  PSDCW018-17-QFLIAL-EMPR          PIC S9(004) COMP-3.
.               05  PSDCW018-17-CMUN                 PIC S9(010) COMP-3.
CPMALP          05  PSDCW018-17-CUF                  PIC S9(006) COMP-3.
                                                                        
      ******************************************************************
      **** TIPO REG.: 018 - CADUB026 - EXPORTACAO-IMPORTACAO-EMPRESA    
      ******************************************************************
                                                                        
           03   PSDCW018-18-EXPOR-IMPOR  REDEFINES                      
                                          PSDCW018-01-PSSOA-UNICA.      
                05  PSDCW018-18-CPF-CNPJ.                               
                    07  PSDCW018-18-CPF-CNPJ-NRO     PIC S9(009) COMP-3.
                    07  PSDCW018-18-CPF-CNPJ-CTR     PIC S9(002) COMP-3.
                    07  PSDCW018-18-CPF-CNPJ-FIL     PIC S9(004) COMP-3.
                05  PSDCW018-18-CCLUB                PIC S9(010) COMP-3.
                05  PSDCW018-18-TP-REG               PIC  X(003).       
                05  PSDCW018-18-CINDCD-EXPOR-IMPOR   PIC S9(001) COMP-3.
                05  PSDCW018-18-CPTCAO-ESPAC-TBELA   PIC S9(002) COMP-3.
                05  PSDCW018-18-PEXPOR-IMPOR      PIC S9(003)V99 COMP-3.
                05  PSDCW018-18-VPREVT-EXPOR-IMPO PIC S9(013)V99 COMP-3.
                05  PSDCW018-18-VEXPOR-IMPOR-CONT PIC S9(013)V99 COMP-3.
CPMALP********  CAMPOS NOVOS  ******************************************
.               05  PSDCW018-18-CEMPR                PIC S9(010) COMP-3.
.               05  PSDCW018-18-DMES-EXPOR-EMPR      PIC S9(006) COMP-3.
CPMALP          05  PSDCW018-18-CINDCD-ECONM-MOEDA   PIC S9(005) COMP-3.
                                                                        
      ******************************************************************
      **** TIPO REG.: 019 - CADUB028 - FINANCEIRA-EMPRESA               
      ******************************************************************
                                                                        
           03   PSDCW018-19-FINCR-EMPR   REDEFINES                      
                                         PSDCW018-01-PSSOA-UNICA.       
                05  PSDCW018-19-CPF-CNPJ.                               
                    07  PSDCW018-19-CPF-CNPJ-NRO     PIC S9(009) COMP-3.
                    07  PSDCW018-19-CPF-CNPJ-CTR     PIC S9(002) COMP-3.
                    07  PSDCW018-19-CPF-CNPJ-FIL     PIC S9(004) COMP-3.
                05  PSDCW018-19-CCLUB                PIC S9(010) COMP-3.
                05  PSDCW018-19-TP-REG               PIC  X(003).       
                05  PSDCW018-19-CPTCAO-ESPAC-TBELA   PIC S9(002) COMP-3.
                05  PSDCW018-19-QFUNC-EMPR           PIC S9(005) COMP-3.
                05  PSDCW018-19-VFL-PGTO          PIC S9(013)V99 COMP-3.
                05  PSDCW018-19-DMES-ANO-FL-PGTO     PIC S9(006) COMP-3.
                05  PSDCW018-19-DMES-ANO-VDA         PIC S9(006) COMP-3.
                05  PSDCW018-19-VVDA-RENDA-MES    PIC S9(013)V99 COMP-3.
                05  PSDCW018-19-DMES-ANO-ULT-FATMT   PIC S9(006) COMP-3.
                05  PSDCW018-19-VPREVT-FATMT-ANO  PIC S9(013)V99 COMP-3.
                05  PSDCW018-19-VFATMT-ULT-MES    PIC S9(013)V99 COMP-3.
                05  PSDCW018-19-VPDIDO-CART       PIC S9(013)V99 COMP-3.
                05  PSDCW018-19-DMES-ANO-PDIDO       PIC S9(006) COMP-3.
                05  PSDCW018-19-VESTOQ-ATUAL      PIC S9(013)V99 COMP-3.
                05  PSDCW018-19-DMES-ANO-ESTOQ       PIC S9(006) COMP-3.
                05  PSDCW018-19-DMES-ANO-VDA-ULT     PIC S9(006) COMP-3.
                05  PSDCW018-19-VVDA-ULT-ANO      PIC S9(013)V99 COMP-3.
                05  PSDCW018-19-DMES-ANO-COMPR-ULT   PIC S9(006) COMP-3.
                05  PSDCW018-19-VCOMPR-ULT-ANO    PIC S9(013)V99 COMP-3.
CPMALP********  CAMPOS NOVOS  ******************************************
.               05  PSDCW018-19-CEMPR                PIC S9(010) COMP-3.
.               05  PSDCW018-19-TMED-VDA-OBJET       PIC S9(005) COMP-3.
.               05  PSDCW018-19-TMED-COMPR-OBJET     PIC S9(005) COMP-3.
.               05  PSDCW018-19-CUND-TEMPO-VDA       PIC S9(003) COMP-3.
.               05  PSDCW018-19-CUND-TEMPO-COMPR     PIC S9(003) COMP-3.
.               05  PSDCW018-19-VCAPTL-ABERT      PIC S9(013)V99 COMP-3.
CPMALP          05  PSDCW018-19-CCLASF-PORTE-EMPR    PIC S9(002) COMP-3.
                                                                        
      ******************************************************************
      **** TIPO REG.: 020 - CADUB031 - FORNECEDOR-CLIENTE               
      ******************************************************************
                                                                        
           03   PSDCW018-20-FORNC-CLI    REDEFINES                      
                                        PSDCW018-01-PSSOA-UNICA.        
                05  PSDCW018-20-CPF-CNPJ.                               
                    07  PSDCW018-20-CPF-CNPJ-NRO     PIC S9(009) COMP-3.
                    07  PSDCW018-20-CPF-CNPJ-CTR     PIC S9(002) COMP-3.
                    07  PSDCW018-20-CPF-CNPJ-FIL     PIC S9(004) COMP-3.
                05  PSDCW018-20-CCLUB                PIC S9(010) COMP-3.
                05  PSDCW018-20-TP-REG               PIC  X(003).       
                05  PSDCW018-20-CSEQ-FORNC-CLI       PIC S9(004) COMP-3.
                05  PSDCW018-20-CPTCAO-ESPAC-TBELA   PIC S9(002) COMP-3.
                05  PSDCW018-20-CID-RELAC-COML       PIC  X(001).       
                05  PSDCW018-20-CCNPJ                PIC S9(009) COMP-3.
                05  PSDCW018-20-CFLIAL-CNPJ          PIC S9(004) COMP-3.
                05  PSDCW018-20-CCTRL-CNPJ           PIC S9(002) COMP-3.
                05  PSDCW018-20-IRZ-SCIAL            PIC  X(070).       
                05  PSDCW018-20-PCOCEN-COMPR-VDA  PIC S9(003)V99 COMP-3.
                05  PSDCW018-20-CMUN-RURAL           PIC S9(009) COMP-3.
                05  PSDCW018-20-CSGL-UF              PIC  X(002).       
CPMALP********  CAMPOS NOVOS  ******************************************
.               05  PSDCW018-20-CEMPR                PIC S9(010) COMP-3.
.               05  PSDCW018-20-CMUN                 PIC S9(010) COMP-3.
CPMALP          05  PSDCW018-20-CUF                  PIC S9(006) COMP-3.
                                                                        
      ******************************************************************
      **** TIPO REG.: 021 - CADUB033 - PATRIMONIO-FROTA-VEICULO         
      ******************************************************************
                                                                        
           03   PSDCW018-21-FROTA-VEIC   REDEFINES                      
                                         PSDCW018-01-PSSOA-UNICA.       
                05  PSDCW018-21-CPF-CNPJ.                               
                    07  PSDCW018-21-CPF-CNPJ-NRO     PIC S9(009) COMP-3.
                    07  PSDCW018-21-CPF-CNPJ-CTR     PIC S9(002) COMP-3.
                    07  PSDCW018-21-CPF-CNPJ-FIL     PIC S9(004) COMP-3.
                05  PSDCW018-21-CCLUB                PIC S9(010) COMP-3.
                05  PSDCW018-21-TP-REG               PIC  X(003).       
                05  PSDCW018-21-CPTCAO-ESPAC-TBELA   PIC S9(002) COMP-3.
                05  PSDCW018-21-QANO-MED-FROTA       PIC S9(002) COMP-3.
                05  PSDCW018-21-QFROTA-VEIC          PIC S9(006) COMP-3.
                05  PSDCW018-21-VMED-MERCD-FROTA  PIC S9(013)V99 COMP-3.
                05  PSDCW018-21-QALIEN-FROTA-VEIC    PIC S9(006) COMP-3.
                05  PSDCW018-21-VLIVRE-ALIEN-FROT PIC S9(013)V99 COMP-3.
                05  PSDCW018-21-VTOT-VEIC-ALIEN   PIC S9(013)V99 COMP-3.
                05  PSDCW018-21-CINDCD-OBRIG-FROTA   PIC  X(001).       
                05  PSDCW018-21-VTOT-PCELA-OBRIG  PIC S9(013)V99 COMP-3.
                05  PSDCW018-21-QPCELA-PG-OBRIG      PIC S9(003) COMP-3.
                05  PSDCW018-21-QMES-OPER-OBRIG      PIC S9(003) COMP-3.
CPMALP********  CAMPOS NOVOS  ******************************************
.               05  PSDCW018-21-CEMPR                PIC S9(010) COMP-3.
.               05  PSDCW018-21-NPATRM-FROTA-VEIC    PIC S9(005) COMP-3.
.               05  PSDCW018-21-TTOT-CONTR-OPER      PIC S9(005) COMP-3.
.               05  PSDCW018-21-CINDCD-ECONM-MOEDA   PIC S9(005) COMP-3.
.               05  PSDCW018-21-CTPO-FROTA           PIC S9(003) COMP-3.
CPMALP          05  PSDCW018-21-NINSTC-FINCR-PATRM   PIC S9(004) COMP-3.
                                                                        
      ******************************************************************
      **** TIPO REG.: 022 - CADUB076 - LEASING-PESSOA                   
      ******************************************************************
                                                                        
           03   PSDCW018-22-LSNG-PSSOA   REDEFINES                      
                                         PSDCW018-01-PSSOA-UNICA.       
                05  PSDCW018-22-CPF-CNPJ.                               
                    07  PSDCW018-22-CPF-CNPJ-NRO     PIC S9(009) COMP-3.
                    07  PSDCW018-22-CPF-CNPJ-CTR     PIC S9(002) COMP-3.
                    07  PSDCW018-22-CPF-CNPJ-FIL     PIC S9(004) COMP-3.
                05  PSDCW018-22-CCLUB                PIC S9(010) COMP-3.
                05  PSDCW018-22-TP-REG               PIC  X(003).       
                05  PSDCW018-22-CSEQ-LSNG            PIC S9(004) COMP-3.
                05  PSDCW018-22-CPTCAO-ESPAC-TBELA   PIC S9(002) COMP-3.
                05  PSDCW018-22-CTPO-BEM-CLI         PIC S9(003) COMP-3.
                05  PSDCW018-22-DINIC-PRIM-CONTR     PIC  X(010).       
                05  PSDCW018-22-DFIM-ULT-CONTR       PIC  X(010).       
                05  PSDCW018-22-QBEM-LSNG            PIC S9(005) COMP-3.
                05  PSDCW018-22-VCONTR-LSNG       PIC S9(013)V99 COMP-3.
                05  PSDCW018-22-IEMPR-LSNG-COPLT     PIC  X(070).       
                05  PSDCW018-22-VPCELA-MES        PIC S9(013)V99 COMP-3.
                05  PSDCW018-22-CINDCD-ONUS-BEM      PIC  X(001).       
                05  PSDCW018-22-VONUS-BEM         PIC S9(013)V99 COMP-3.
CPMALP********  CAMPOS NOVOS  ******************************************
CPMALP          05  PSDCW018-22-CEMPR                PIC S9(010) COMP-3.
                                                                        
      ******************************************************************
      **** TIPO REG.: 023 - CADUB0A6 - QUADRO-SOCIAL-ADM-COSLH          
      ******************************************************************
                                                                        
           03   PSDCW018-23-QUADR-SCIAL  REDEFINES                      
                                          PSDCW018-01-PSSOA-UNICA.      
                05  PSDCW018-23-CPF-CNPJ.                               
                    07  PSDCW018-23-CPF-CNPJ-NRO     PIC S9(009) COMP-3.
                    07  PSDCW018-23-CPF-CNPJ-CTR     PIC S9(002) COMP-3.
                    07  PSDCW018-23-CPF-CNPJ-FIL     PIC S9(004) COMP-3.
                05  PSDCW018-23-CCLUB                PIC S9(010) COMP-3.
                05  PSDCW018-23-TP-REG               PIC  X(003).       
                05  PSDCW018-23-CINDCD-ORIGE-PARTC   PIC S9(001) COMP-3.
                05  PSDCW018-23-CSEQ-QUADR-EMPR      PIC S9(005) COMP-3.
                05  PSDCW018-23-CPTCAO-ESPAC-TBELA   PIC S9(002) COMP-3.
                05  PSDCW018-23-DMES-ANO-PRTCP       PIC S9(006) COMP-3.
                05  PSDCW018-23-DMES-ANO-FIM-EMPR    PIC S9(006) COMP-3.
                05  PSDCW018-23-CCPF-CNPJ            PIC S9(009) COMP-3.
                05  PSDCW018-23-CCTRL-CPF-CNPJ       PIC S9(002) COMP-3.
                05  PSDCW018-23-CFLIAL-CPF-CNPJ      PIC S9(004) COMP-3.
                05  PSDCW018-23-CISENC-CPF-CNPJ      PIC  X(001).       
                05  PSDCW018-23-CMOTVO-ISENC-CPF     PIC S9(001) COMP-3.
                05  PSDCW018-23-CNAC                 PIC S9(003) COMP-3.
                05  PSDCW018-23-IPARTC-EMPR          PIC  X(070).       
                05  PSDCW018-23-CCARGO-CLI-PF        PIC S9(005) COMP-3.
                05  PSDCW018-23-PCAPTL-TOT        PIC S9(003)V99 COMP-3.
CPMALP********  CAMPOS NOVOS  ******************************************
CPMALP          05  PSDCW018-23-CEMPR                PIC S9(010) COMP-3.
                                                                        
      ******************************************************************
      **** TIPO REG.: 024 - CADUB095 - PARTICIPACAO-OUTRA-EMPRESA       
      ******************************************************************
                                                                        
           03   PSDCW018-24-PRTCP-OUTRA-EMPR   REDEFINES                
                                               PSDCW018-01-PSSOA-UNICA. 
                05  PSDCW018-24-CPF-CNPJ.                               
                    07  PSDCW018-24-CPF-CNPJ-NRO     PIC S9(009) COMP-3.
                    07  PSDCW018-24-CPF-CNPJ-CTR     PIC S9(002) COMP-3.
                    07  PSDCW018-24-CPF-CNPJ-FIL     PIC S9(004) COMP-3.
                05  PSDCW018-24-CCLUB                PIC S9(010) COMP-3.
                05  PSDCW018-24-TP-REG               PIC  X(003).       
                05  PSDCW018-24-CINDCD-ORIGE-PARTC   PIC S9(001) COMP-3.
                05  PSDCW018-24-CSEQ-QUADR-EMPR      PIC S9(005) COMP-3.
                05  PSDCW018-24-CSEQ-PRTCP-OUTRA     PIC S9(003) COMP-3.
                05  PSDCW018-24-CPTCAO-ESPAC-TBELA   PIC S9(002) COMP-3.
                05  PSDCW018-24-CCARGO-CLI-PF        PIC S9(005) COMP-3.
                05  PSDCW018-24-CISENC-CPF-CNPJ      PIC  X(001).       
                05  PSDCW018-24-CMOTVO-ISENC-CPF     PIC S9(001) COMP-3.
                05  PSDCW018-24-CCNPJ-EMPR           PIC S9(009) COMP-3.
                05  PSDCW018-24-CFLIAL-CNPJ-EMPR     PIC S9(004) COMP-3.
                05  PSDCW018-24-CCTRL-CNPJ-EMPR      PIC S9(002) COMP-3.
                05  PSDCW018-24-IRZ-SCIAL            PIC  X(070).       
                05  PSDCW018-24-PPRTCP-EMPR       PIC S9(003)V99 COMP-3.
                05  PSDCW018-24-DMES-ANO-PRTCP       PIC S9(006) COMP-3.
                05  PSDCW018-24-DMES-ANO-FIM-EMPR    PIC S9(006) COMP-3.
CPMALP********  CAMPOS NOVOS  ******************************************
CPMALP          05  PSDCW018-24-CEMPR                PIC S9(010) COMP-3.
                                                                        
      ******************************************************************
      **** TIPO REG.: 025 - CADUB099 - PARTICIPACAO-EMPRESA-PJ          
      ******************************************************************
                                                                        
           03   PSDCW018-25-PRTCP-PJ     REDEFINES                      
                                       PSDCW018-01-PSSOA-UNICA.         
                05  PSDCW018-25-CPF-CNPJ.                               
                    07  PSDCW018-25-CPF-CNPJ-NRO     PIC S9(009) COMP-3.
                    07  PSDCW018-25-CPF-CNPJ-CTR     PIC S9(002) COMP-3.
                    07  PSDCW018-25-CPF-CNPJ-FIL     PIC S9(004) COMP-3.
                05  PSDCW018-25-CCLUB                PIC S9(010) COMP-3.
                05  PSDCW018-25-TP-REG               PIC  X(003).       
                05  PSDCW018-25-CSEQ-PRTCP-EMPR      PIC S9(003) COMP-3.
                05  PSDCW018-25-CPTCAO-ESPAC-TBELA   PIC S9(002) COMP-3.
                05  PSDCW018-25-IRZ-SCIAL            PIC  X(070).       
                05  PSDCW018-25-CMUN-RURAL           PIC S9(009) COMP-3.
                05  PSDCW018-25-CUF-EMPR             PIC  X(002).       
                05  PSDCW018-25-CISENC-CPF-CNPJ      PIC  X(001).       
                05  PSDCW018-25-CMOTVO-ISENC-CPF     PIC S9(001) COMP-3.
                05  PSDCW018-25-CCNPJ-EMPR           PIC S9(009) COMP-3.
                05  PSDCW018-25-CFLIAL-CNPJ-EMPR     PIC S9(004) COMP-3.
                05  PSDCW018-25-CCTRL-CNPJ-EMPR      PIC S9(002) COMP-3.
                05  PSDCW018-25-CCLASS-ATVDD-ECONC   PIC  X(002).       
                05  PSDCW018-25-CRAMO-ATVDD-ECONC    PIC S9(003) COMP-3.
                05  PSDCW018-25-CSRAMO-ATVDD-ECONC   PIC S9(003) COMP-3.
                05  PSDCW018-25-CATVDD-ECONC         PIC S9(003) COMP-3.
                05  PSDCW018-25-PPRTCP-EMPR       PIC S9(003)V99 COMP-3.
                05  PSDCW018-25-DMES-ANO-PRTCP       PIC S9(006) COMP-3.
                05  PSDCW018-25-DMES-ANO-FIM-EMPR    PIC S9(006) COMP-3.
CPMALP********  CAMPOS NOVOS  ******************************************
.               05  PSDCW018-25-CEMPR                PIC S9(010) COMP-3.
.               05  PSDCW018-25-CMUN-GEOGR           PIC S9(010) COMP-3.
CPMALP          05  PSDCW018-25-CUF                  PIC S9(006) COMP-3.
                                                                        
      ******************************************************************
      **** TIPO REG.: 026 - CADUB014 - DEPENDENTE-PESSOA                
      ******************************************************************
                                                                        
           03   PSDCW018-26-DEPDT-PSSOA  REDEFINES                      
                                          PSDCW018-01-PSSOA-UNICA.      
                05  PSDCW018-26-CPF-CNPJ.                               
                    07  PSDCW018-26-CPF-CNPJ-NRO     PIC S9(009) COMP-3.
                    07  PSDCW018-26-CPF-CNPJ-CTR     PIC S9(002) COMP-3.
                    07  PSDCW018-26-CPF-CNPJ-FIL     PIC S9(004) COMP-3.
                05  PSDCW018-26-CCLUB                PIC S9(010) COMP-3.
                05  PSDCW018-26-TP-REG               PIC  X(003).       
                05  PSDCW018-26-CSEQ-DEPDT           PIC S9(002) COMP-3.
                05  PSDCW018-26-CPTCAO-ESPAC-TBELA   PIC S9(002) COMP-3.
                05  PSDCW018-26-IDEPDT               PIC  X(070).       
                05  PSDCW018-26-CGRAU-PARNT          PIC S9(001) COMP-3.
                05  PSDCW018-26-CSEXO                PIC  X(001).       
                05  PSDCW018-26-DNASC-DEPDT          PIC  X(010).       
CPMALP********  CAMPOS NOVOS  ******************************************
.               05  PSDCW018-26-CEMPR                PIC S9(010) COMP-3.
.               05  PSDCW018-26-CCPF-CNPJ            PIC S9(009) COMP-3.
.               05  PSDCW018-26-CFLIAL-CPF-CNPJ      PIC S9(004) COMP-3.
CPMALP          05  PSDCW018-26-CCTRL-CPF-CNPJ       PIC S9(002) COMP-3.
                                                                        
      ******************************************************************
      **** TIPO REG.: 027 - CADUB072 - LINHA-PRODUTO-EMPRESA            
      ******************************************************************
                                                                        
           03   PSDCW018-27-LIN-PRODT    REDEFINES                      
                                        PSDCW018-01-PSSOA-UNICA.        
                05  PSDCW018-27-CPF-CNPJ.                               
                    07  PSDCW018-27-CPF-CNPJ-NRO     PIC S9(009) COMP-3.
                    07  PSDCW018-27-CPF-CNPJ-CTR     PIC S9(002) COMP-3.
                    07  PSDCW018-27-CPF-CNPJ-FIL     PIC S9(004) COMP-3.
                05  PSDCW018-27-CCLUB                PIC S9(010) COMP-3.
                05  PSDCW018-27-TP-REG               PIC  X(003).       
                05  PSDCW018-27-CSEQ-PRODT-EMPR      PIC S9(005) COMP-3.
                05  PSDCW018-27-CPTCAO-ESPAC-TBELA   PIC S9(002) COMP-3.
                05  PSDCW018-27-IPRODT-EMPR          PIC  X(040).       
                05  PSDCW018-27-PPRODT-FATMT-EMPR PIC S9(003)V99 COMP-3.
CPMALP********  CAMPOS NOVOS  ******************************************
CPMALP          05  PSDCW018-27-CEMPR                PIC S9(010) COMP-3.
                                                                        
      ******************************************************************
      **** TIPO REG.: 028 - CADUB016 - EMPRESA-ANTECESSORA              
      ******************************************************************
                                                                        
           03   PSDCW018-28-EMPR-ANTCS   REDEFINES                      
                                         PSDCW018-01-PSSOA-UNICA.       
                05  PSDCW018-28-CPF-CNPJ.                               
                    07  PSDCW018-28-CPF-CNPJ-NRO     PIC S9(009) COMP-3.
                    07  PSDCW018-28-CPF-CNPJ-CTR     PIC S9(002) COMP-3.
                    07  PSDCW018-28-CPF-CNPJ-FIL     PIC S9(004) COMP-3.
                05  PSDCW018-28-CCLUB                PIC S9(010) COMP-3.
                05  PSDCW018-28-TP-REG               PIC  X(003).       
                05  PSDCW018-28-CSEQ-EMPR-ANTCS      PIC S9(003) COMP-3.
                05  PSDCW018-28-CPTCAO-ESPAC-TBELA   PIC S9(002) COMP-3.
                05  PSDCW018-28-IEMPR-ANTCS          PIC  X(070).       
                05  PSDCW018-28-DSUCSS-EMPR-ANTCS    PIC  X(010).       
CPMALP********  CAMPOS NOVOS  ******************************************
CPMALP          05  PSDCW018-28-CEMPR                PIC S9(010) COMP-3.
                                                                        
      ******************************************************************
      **** TIPO REG.: 029 - CADUB0A3 - PESSOA-OPERACAO                  
      ******************************************************************
                                                                        
           03   PSDCW018-29-PSSOA-OPER   REDEFINES                      
                                         PSDCW018-01-PSSOA-UNICA.       
                05  PSDCW018-29-CPF-CNPJ.                               
                    07  PSDCW018-29-CPF-CNPJ-NRO     PIC S9(009) COMP-3.
                    07  PSDCW018-29-CPF-CNPJ-CTR     PIC S9(002) COMP-3.
                    07  PSDCW018-29-CPF-CNPJ-FIL     PIC S9(004) COMP-3.
                05  PSDCW018-29-CCLUB                PIC S9(010) COMP-3.
                05  PSDCW018-29-TP-REG               PIC  X(003).       
                05  PSDCW018-29-CID-OPER             PIC  X(026).       
                05  PSDCW018-29-CEMPR-INC            PIC S9(005) COMP-3.
                05  PSDCW018-29-CDEPDC               PIC S9(005) COMP-3.
                05  PSDCW018-29-CCTRO-CUSTO          PIC  X(004).       
                05  PSDCW018-29-CTPO-OBRIG           PIC S9(001) COMP-3.
                05  PSDCW018-29-CPRODT-CREDT         PIC S9(003) COMP-3.
                05  PSDCW018-29-CSPROD-CREDT         PIC S9(004) COMP-3.
                05  PSDCW018-29-CSEQ-TPO-OBRIG       PIC S9(003) COMP-3.
                05  PSDCW018-29-CPTCAO-ESPAC-TBELA   PIC S9(002) COMP-3.
                05  PSDCW018-29-CSIT-OPER            PIC S9(001) COMP-3.
                05  PSDCW018-29-CINDCD-TRAG-OPER     PIC S9(001) COMP-3.
                05  PSDCW018-29-CCATEG-CTA           PIC S9(001) COMP-3.
                05  PSDCW018-29-DCELEB-OPER          PIC  X(010).       
                05  PSDCW018-29-DFIM-OPER            PIC  X(010).       
                05  PSDCW018-29-DENCRR-OPER          PIC  X(010).       
                05  PSDCW018-29-CPOSTO-SERVC         PIC S9(005) COMP-3.
                05  PSDCW018-29-CSGMTO-CTA           PIC S9(003) COMP-3.
                05  PSDCW018-29-QPCELA-ORIGN         PIC S9(003) COMP-3.
                05  PSDCW018-29-QPCELA-ABERT         PIC S9(003) COMP-3.
                05  PSDCW018-29-QPCELA-ATRSO         PIC S9(003) COMP-3.
                05  PSDCW018-29-QDIA-PCELA-ATRSO     PIC S9(004) COMP-3.
                05  PSDCW018-29-CSEQ-ENDER-PSSOA     PIC S9(005) COMP-3.
                05  PSDCW018-29-CDISTR-CORSP         PIC S9(002) COMP-3.
CPMALP********  CAMPOS NOVOS  ******************************************
.               05  PSDCW018-29-CEMPR                PIC S9(010) COMP-3.
.                                                                       
.     ******************************************************************
.     **** TIPO REG.: 032 - CADUB0F9 - ATIVIDADE-PESSOA-UNICA           
.     ******************************************************************
.                                                                       
.          03   PSDCW018-32-TATVDD-PSSOA-UNIC REDEFINES                 
.                                             PSDCW018-01-PSSOA-UNICA.  
.               05 PSDCW018-32-CPF-CNPJ.                                
.                   07  PSDCW018-32-CPF-CNPJ-NRO    PIC S9(009) COMP-3. 
.                   07  PSDCW018-32-CPF-CNPJ-CTR    PIC S9(002) COMP-3. 
.                   07  PSDCW018-32-CPF-CNPJ-FIL    PIC S9(004) COMP-3. 
.               05 PSDCW018-32-CCLUB                PIC S9(010) COMP-3. 
.               05 PSDCW018-32-TP-REG               PIC  X(003).        
CPMTEL          05 PSDCW018-32-CAGPTO-ATVDD-ECONC   PIC S9(009) COMP-3. 
.               05 PSDCW018-32-CEMPR                PIC S9(010) COMP-3. 
.                                                                       
.     ******************************************************************
.     **** TIPO REG.: 033 - CADUB0E9 - DEFICIENCIA-PESSOA-UNICA         
.     ******************************************************************
.                                                                       
.          03   PSDCW018-33-TDEFIC-PSSOA-UNIC REDEFINES                 
.                                             PSDCW018-01-PSSOA-UNICA.  
.               05 PSDCW018-33-CPF-CNPJ.                                
.                   07  PSDCW018-33-CPF-CNPJ-NRO    PIC S9(009) COMP-3. 
.                   07  PSDCW018-33-CPF-CNPJ-CTR    PIC S9(002) COMP-3. 
.                   07  PSDCW018-33-CPF-CNPJ-FIL    PIC S9(004) COMP-3. 
.               05 PSDCW018-33-CCLUB                PIC S9(010) COMP-3. 
.               05 PSDCW018-33-TP-REG               PIC  X(003).        
.               05 PSDCW018-33-CEMPR                PIC S9(010) COMP-3. 
.               05 PSDCW018-33-NDEFIC-PSSOA-UNIC    PIC S9(002) COMP-3. 
.               05 PSDCW018-33-CDEFIC-PSSOA-UNIC    PIC S9(002) COMP-3. 
.                                                                       
.     ******************************************************************
.     **** TIPO REG.: 034 - CADUB0L7 - DOCUMENTO-PESSOA-UNICA           
.     ******************************************************************
.                                                                       
.          03   PSDCW018-34-TDOCTO-PSSOA-UNIC REDEFINES                 
.                                             PSDCW018-01-PSSOA-UNICA.  
.               05 PSDCW018-34-CPF-CNPJ.                                
.                   07  PSDCW018-34-CPF-CNPJ-NRO    PIC S9(009) COMP-3. 
.                   07  PSDCW018-34-CPF-CNPJ-CTR    PIC S9(002) COMP-3. 
.                   07  PSDCW018-34-CPF-CNPJ-FIL    PIC S9(004) COMP-3. 
.               05 PSDCW018-34-CCLUB                PIC S9(010) COMP-3. 
.               05 PSDCW018-34-TP-REG               PIC  X(003).        
.               05 PSDCW018-34-CEMPR                PIC S9(010) COMP-3. 
.               05 PSDCW018-34-CDOCTO               PIC S9(005) COMP-3. 
.               05 PSDCW018-34-CPTCAO-ESPAC-TBELA   PIC S9(002) COMP-3. 
.               05 PSDCW018-34-CDOCTO-EMPR          PIC  X(030).        
.               05 PSDCW018-34-CDOCTO-PSSOA         PIC  X(015).        
.               05 PSDCW018-34-CORG-EMISR           PIC  X(004).        
.               05 PSDCW018-34-CUF                  PIC S9(006) COMP-3. 
.               05 PSDCW018-34-CPAIS                PIC S9(003) COMP-3. 
.               05 PSDCW018-34-DEMIS-DOCTO          PIC  X(010).        
.               05 PSDCW018-34-DVCTO-DOCTO          PIC  X(010).        
.                                                                       
.     ******************************************************************
.     **** TIPO REG.: 037 - PSCEB004 - ENDERECO-COMPLEMENTAR-PESSOA     
.     ******************************************************************
.                                                                       
.          03   PSDCW018-37-TENDER-COMPL-PSSOA REDEFINES                
.                                              PSDCW018-01-PSSOA-UNICA. 
.               05 PSDCW018-37-CPF-CNPJ.                                
.                   07  PSDCW018-37-CPF-CNPJ-NRO    PIC S9(009) COMP-3. 
.                   07  PSDCW018-37-CPF-CNPJ-CTR    PIC S9(002) COMP-3. 
.                   07  PSDCW018-37-CPF-CNPJ-FIL    PIC S9(004) COMP-3. 
.               05 PSDCW018-37-CPSSOA               PIC S9(010) COMP-3. 
.               05 PSDCW018-37-TP-REG               PIC  X(003).        
.               05 PSDCW018-37-CPSSOA-JURID         PIC S9(010) COMP-3. 
.               05 PSDCW018-37-NSEQ-ENDER-COMPL     PIC S9(005) COMP-3. 
.               05 PSDCW018-37-CPTCAO-TBELA         PIC S9(002) COMP-3. 
.               05 PSDCW018-37-CESPCE-ENDER         PIC S9(001) COMP-3. 
.               05 PSDCW018-37-CCATEG-ENDER         PIC S9(003) COMP-3. 
.               05 PSDCW018-37-CNVEL-CONFD-ENDER    PIC S9(003) COMP-3. 
.               05 PSDCW018-37-CCOMPV-ENDER-PSSOA   PIC S9(003) COMP-3. 
.               05 PSDCW018-37-DINIC-UTILZ-ENDER    PIC  X(010).        
.               05 PSDCW018-37-DFIM-UTILZ-ENDER     PIC  X(010).        
.               05 PSDCW018-37-IPSSOA-CNTAT-ENDER   PIC  X(040).        
.               05 PSDCW018-37-CCARGO-PSSOA-CNTAT   PIC S9(005) COMP-3. 
.               05 PSDCW018-37-CENDER-PDRAO-PSSOA   PIC S9(001) COMP-3. 
.               05 PSDCW018-37-CUSO-ENDER-PSSOA     PIC S9(003) COMP-3. 
.               05 PSDCW018-37-RENDER-ELETR         PIC  X(070).        
.               05 PSDCW018-37-CTPO-ENDER-ELETR     PIC S9(003) COMP-3. 
.               05 PSDCW018-37-CAUTRZ-ENVIO-EMAIL   PIC S9(001) COMP-3. 
.               05 PSDCW018-37-CCATEG-ENDER-INTRN   PIC S9(001) COMP-3. 
.               05 PSDCW018-37-CTPO-UND-ORGNZ       PIC S9(003) COMP-3. 
.               05 PSDCW018-37-EINTRN-ORGNZ-COMPL   PIC  X(020).        
.               05 PSDCW018-37-CIDTFD-ENDER-EXIST   PIC S9(001) COMP-3. 
.                                                                       
.     ******************************************************************
.     **** TIPO REG.: 038 - CADUB0M3 - EMPREGADOR-PESSOA                
.     ******************************************************************
.                                                                       
.          03   PSDCW018-38-TEMPRG-PSSOA REDEFINES                      
.                                        PSDCW018-01-PSSOA-UNICA.       
.               05 PSDCW018-38-CPF-CNPJ.                                
.                   07  PSDCW018-38-CPF-CNPJ-NRO    PIC S9(009) COMP-3. 
.                   07  PSDCW018-38-CPF-CNPJ-CTR    PIC S9(002) COMP-3. 
.                   07  PSDCW018-38-CPF-CNPJ-FIL    PIC S9(004) COMP-3. 
.               05 PSDCW018-38-CCLUB                PIC S9(010) COMP-3. 
.               05 PSDCW018-38-TP-REG               PIC  X(003).        
.               05 PSDCW018-38-CEMPR                PIC S9(010) COMP-3. 
.               05 PSDCW018-38-NEMPRG-PJ            PIC S9(003) COMP-3. 
.               05 PSDCW018-38-CPTCAO-ESPAC-TBELA   PIC S9(002) COMP-3. 
.               05 PSDCW018-38-CCPF-CNPJ            PIC S9(009) COMP-3. 
.               05 PSDCW018-38-CFLIAL-CPF-CNPJ      PIC S9(004) COMP-3. 
.               05 PSDCW018-38-CCTRL-CPF-CNPJ       PIC S9(002) COMP-3. 
.               05 PSDCW018-38-IRZ-SCIAL            PIC  X(070).        
.               05 PSDCW018-38-CCARGO-CLI-PF        PIC S9(005) COMP-3. 
.               05 PSDCW018-38-CCOMPS-EMPR          PIC S9(001) COMP-3. 
.               05 PSDCW018-38-CPORTE-EMPR          PIC S9(003) COMP-3. 
.               05 PSDCW018-38-CPROFS-PF            PIC S9(005) COMP-3. 
.               05 PSDCW018-38-CCATEG-PROFS-CREDT   PIC S9(005) COMP-3. 
.               05 PSDCW018-38-ROBS-EMPRG-PSSOA     PIC  X(040).        
.               05 PSDCW018-38-DINIC-PSSOA-CARGO    PIC  X(010).        
.               05 PSDCW018-38-DADMIS-EMPR          PIC  X(010).        
.               05 PSDCW018-38-VRENDA-PSSOA      PIC S9(013)V99 COMP-3. 
.               05 PSDCW018-38-CTPO-COMPV-RENDA     PIC S9(002) COMP-3. 
.               05 PSDCW018-38-CPERDC-RENDA-EMPRG   PIC S9(002) COMP-3. 
.               05 PSDCW018-38-CINDCD-ECONM-MOEDA   PIC S9(005) COMP-3. 
.               05 PSDCW018-38-CCLUB-ENDER-PSSOA    PIC S9(010) COMP-3. 
.               05 PSDCW018-38-CEMPR-ENDER-PSSOA    PIC S9(010) COMP-3. 
.               05 PSDCW018-38-CSEQ-ENDER-PSSOA     PIC S9(005) COMP-3. 
.               05 PSDCW018-38-CCLUB-CNTRL-FONE     PIC S9(010) COMP-3. 
.               05 PSDCW018-38-CEMPR-CNTRL-FONE     PIC S9(010) COMP-3. 
.               05 PSDCW018-38-CSEQ-CNTRL-FONE      PIC S9(003) COMP-3. 
.               05 PSDCW018-38-CPSSOA-ENDER-COMPL   PIC S9(010) COMP-3. 
.               05 PSDCW018-38-CPSSOA-JURID-COMPL   PIC S9(010) COMP-3. 
.               05 PSDCW018-38-NSEQ-ENDER-COMPL     PIC S9(005) COMP-3. 
.               05 PSDCW018-38-CDDD-EMPRG-ANTER     PIC  X(004).        
.               05 PSDCW018-38-CFONE-EMPR-ANTER     PIC S9(008) COMP-3. 
.               05 PSDCW018-38-DDMISS-EMPRG-ANTER   PIC  X(010).        
.               05 PSDCW018-38-CID-COMPV-RENDA      PIC  X(001).        
.               05 PSDCW018-38-DMES-ANO-ATVDD       PIC S9(006) COMP-3. 
CPMTEL          05 PSDCW018-38-NLIN-TFONI           PIC S9(011) COMP-3. 
.                                                                       
.     ******************************************************************
.     **** TIPO REG.: 039 - CADUB0G4 - FILIAL-PESSOA-UNICA              
.     ******************************************************************
.                                                                       
.          03   PSDCW018-39-TFLIAL-PSSOA-UNIC REDEFINES                 
.                                             PSDCW018-01-PSSOA-UNICA.  
.               05 PSDCW018-39-CPF-CNPJ.                                
.                   07  PSDCW018-39-CPF-CNPJ-NRO    PIC S9(009) COMP-3. 
.                   07  PSDCW018-39-CPF-CNPJ-CTR    PIC S9(002) COMP-3. 
.                   07  PSDCW018-39-CPF-CNPJ-FIL    PIC S9(004) COMP-3. 
.               05 PSDCW018-39-CCLUB                PIC S9(010) COMP-3. 
.               05 PSDCW018-39-TP-REG               PIC  X(003).        
.               05 PSDCW018-39-NFLIAL-PSSOA-JURID   PIC S9(002) COMP-3. 
.               05 PSDCW018-39-CCPF-CNPJ            PIC S9(009) COMP-3. 
.               05 PSDCW018-39-CFLIAL-CPF-CNPJ      PIC S9(004) COMP-3. 
.               05 PSDCW018-39-CCTRL-CPF-CNPJ       PIC S9(002) COMP-3. 
.               05 PSDCW018-39-CUF                  PIC S9(006) COMP-3. 
.               05 PSDCW018-39-CMUN                 PIC S9(010) COMP-3. 
.               05 PSDCW018-39-IRZ-SCIAL            PIC  X(070).        
.               05 PSDCW018-39-CEMPR                PIC S9(010) COMP-3. 
.                                                                       
.     ******************************************************************
.     **** TIPO REG.: 040 - CADUB0L9 - PATRIMONIO-BEM-AERONAVE          
.     ******************************************************************
.                                                                       
.          03   PSDCW018-40-TPATRM-BEM-AERNV REDEFINES                  
.                                            PSDCW018-01-PSSOA-UNICA.   
.               05 PSDCW018-40-CPF-CNPJ.                                
.                   07  PSDCW018-40-CPF-CNPJ-NRO    PIC S9(009) COMP-3. 
.                   07  PSDCW018-40-CPF-CNPJ-CTR    PIC S9(002) COMP-3. 
.                   07  PSDCW018-40-CPF-CNPJ-FIL    PIC S9(004) COMP-3. 
.               05 PSDCW018-40-CCLUB                PIC S9(010) COMP-3. 
.               05 PSDCW018-40-TP-REG               PIC  X(003).        
.               05 PSDCW018-40-CEMPR                PIC S9(010) COMP-3. 
.               05 PSDCW018-40-NPATRM-BEM-AERNV     PIC S9(005) COMP-3. 
.               05 PSDCW018-40-CTPO-AERNV           PIC S9(003) COMP-3. 
.               05 PSDCW018-40-DANO-FABRI-BEM       PIC S9(004) COMP-3. 
.               05 PSDCW018-40-CTPO-COMBS           PIC S9(003) COMP-3. 
.               05 PSDCW018-40-CPREFX-AERNV         PIC  X(010).        
.               05 PSDCW018-40-VMERCD-BEM-AERNV  PIC S9(013)V99 COMP-3. 
.               05 PSDCW018-40-CINDCD-ECONM-MOEDA   PIC S9(005) COMP-3. 
.               05 PSDCW018-40-CMARCA-AERNV         PIC S9(003) COMP-3. 
.               05 PSDCW018-40-CMOD-AERNV           PIC S9(005) COMP-3. 
.               05 PSDCW018-40-CCLUB-OUTRO-BEM      PIC S9(010) COMP-3. 
.               05 PSDCW018-40-CEMPR-OUTRO-BEM      PIC S9(010) COMP-3. 
.               05 PSDCW018-40-CSEQ-PATRM-BEM       PIC S9(005) COMP-3. 
.               05 PSDCW018-40-NINSTC-FINCR-PATRM   PIC S9(004) COMP-3. 
.               05 PSDCW018-40-CFORMA-ALIEN-PATRM   PIC S9(002) COMP-3. 
.               05 PSDCW018-40-VTOT-CONTR-ALIEN  PIC S9(013)V99 COMP-3. 
.               05 PSDCW018-40-VONUS-ALIEN-PATRM PIC S9(013)V99 COMP-3. 
.               05 PSDCW018-40-DFIM-ALIEN-PATRM     PIC  X(010).        
.               05 PSDCW018-40-VPCELA-ALIEN-PATRM PIC S9(013)V99 COMP-3.
.               05 PSDCW018-40-QPCELA-ALIEN-PATRM   PIC S9(003) COMP-3. 
.               05 PSDCW018-40-QPCELA-PG-ALIEN      PIC S9(003) COMP-3. 
.                                                                       
.     ******************************************************************
.     **** TIPO REG.: 041 - CADUB0L2 - PATRIMONIO-BEM-EMBARCACAO        
.     ******************************************************************
.                                                                       
.          03   PSDCW018-41-TPATRM-BEM-EMBCA REDEFINES                  
.                                            PSDCW018-01-PSSOA-UNICA.   
.               05 PSDCW018-41-CPF-CNPJ.                                
.                   07  PSDCW018-41-CPF-CNPJ-NRO    PIC S9(009) COMP-3. 
.                   07  PSDCW018-41-CPF-CNPJ-CTR    PIC S9(002) COMP-3. 
.                   07  PSDCW018-41-CPF-CNPJ-FIL    PIC S9(004) COMP-3. 
.               05 PSDCW018-41-CCLUB                PIC S9(010) COMP-3. 
.               05 PSDCW018-41-TP-REG               PIC  X(003).        
.               05 PSDCW018-41-CEMPR                PIC S9(010) COMP-3. 
.               05 PSDCW018-41-NPATRM-VEIC-EMBCA    PIC S9(005) COMP-3. 
.               05 PSDCW018-41-CTPO-ESPCE-VEIC      PIC S9(003) COMP-3. 
.               05 PSDCW018-41-CTPO-EMBCA           PIC S9(003) COMP-3. 
.               05 PSDCW018-41-DANO-FABRI-BEM       PIC S9(004) COMP-3. 
.               05 PSDCW018-41-CTPO-COMBS           PIC S9(003) COMP-3. 
.               05 PSDCW018-41-CREG-PATRM-EMBCA     PIC S9(005) COMP-3. 
.               05 PSDCW018-41-VMERCD-BEM-EMBCA  PIC S9(013)V99 COMP-3. 
.               05 PSDCW018-41-IBEM-EMBCA           PIC  X(020).        
.               05 PSDCW018-41-CINDCD-ECONM-MOEDA   PIC S9(005) COMP-3. 
.               05 PSDCW018-41-CMARCA-EMBCA         PIC S9(003) COMP-3. 
.               05 PSDCW018-41-CMOD-EMBCA           PIC S9(005) COMP-3. 
.               05 PSDCW018-41-CCLUB-OUTRO-BEM      PIC S9(010) COMP-3. 
.               05 PSDCW018-41-CEMPR-OUTRO-BEM      PIC S9(010) COMP-3. 
.               05 PSDCW018-41-CSEQ-PATRM-BEM       PIC S9(005) COMP-3. 
.               05 PSDCW018-41-NINSTC-FINCR-PATRM   PIC S9(004) COMP-3. 
.               05 PSDCW018-41-CFORMA-ALIEN-PATRM   PIC S9(002) COMP-3. 
.               05 PSDCW018-41-VTOT-CONTR-ALIEN  PIC S9(013)V99 COMP-3. 
.               05 PSDCW018-41-VONUS-ALIEN-PATRM PIC S9(013)V99 COMP-3. 
.               05 PSDCW018-41-DFIM-ALIEN-PATRM     PIC  X(010).        
.               05 PSDCW018-41-VPCELA-ALIEN-PATRM PIC S9(013)V99 COMP-3.
.               05 PSDCW018-41-QPCELA-ALIEN-PATRM   PIC S9(003) COMP-3. 
.               05 PSDCW018-41-QPCELA-PG-ALIEN      PIC S9(003) COMP-3. 
.                                                                       
.     ******************************************************************
.     **** TIPO REG.: 042 - CADUB0K9 - PATRIMONIO-BEM-EQUIPAMENTO       
.     ******************************************************************
.                                                                       
.          03   PSDCW018-42-TPATRM-MAQNA-EQPMT REDEFINES                
.                                              PSDCW018-01-PSSOA-UNICA. 
.               05 PSDCW018-42-CPF-CNPJ.                                
.                   07  PSDCW018-42-CPF-CNPJ-NRO    PIC S9(009) COMP-3. 
.                   07  PSDCW018-42-CPF-CNPJ-CTR    PIC S9(002) COMP-3. 
.                   07  PSDCW018-42-CPF-CNPJ-FIL    PIC S9(004) COMP-3. 
.               05 PSDCW018-42-CCLUB                PIC S9(010) COMP-3. 
.               05 PSDCW018-42-TP-REG               PIC  X(003).        
.               05 PSDCW018-42-CEMPR                PIC S9(010) COMP-3. 
.               05 PSDCW018-42-NPATRM-MAQNA-EQPMT   PIC S9(005) COMP-3. 
.               05 PSDCW018-42-CCATEG-MAQNA-OUTRO   PIC S9(003) COMP-3. 
.               05 PSDCW018-42-CTPO-MAQNA-EQPMT     PIC S9(003) COMP-3. 
.               05 PSDCW018-42-RMAQNA-EQPMT         PIC  X(080).        
.               05 PSDCW018-42-VMERCD-MAQNA-EQPMT PIC S9(013)V99 COMP-3.
.               05 PSDCW018-42-CINDCD-ECONM-MOEDA   PIC S9(005) COMP-3. 
.               05 PSDCW018-42-CMARCA-MAQNA-EQPMT   PIC S9(003) COMP-3. 
.               05 PSDCW018-42-CMOD-MAQNA-EQPMT     PIC S9(005) COMP-3. 
.               05 PSDCW018-42-DANO-FABRI-BEM       PIC S9(004) COMP-3. 
.               05 PSDCW018-42-DANO-MOD-MAQNA       PIC S9(004) COMP-3. 
.               05 PSDCW018-42-CTPO-COMBS           PIC S9(003) COMP-3. 
.               05 PSDCW018-42-VONUS-MAQNA-EQPMT  PIC S9(013)V99 COMP-3.
.               05 PSDCW018-42-CIDTFD-ALIEN-MAQNA   PIC  X(001).        
.               05 PSDCW018-42-DFIM-ALIEN-MAQNA     PIC  X(010).        
.               05 PSDCW018-42-VPCELA-MAQNA-EQPMT PIC S9(013)V99 COMP-3.
.               05 PSDCW018-42-QPCELA-MAQNA-EQPMT   PIC S9(005) COMP-3. 
.               05 PSDCW018-42-QPCELA-PG-MAQNA      PIC S9(005) COMP-3. 
.               05 PSDCW018-42-CSIT-MAQNA-EQPMT     PIC S9(003) COMP-3. 
.               05 PSDCW018-42-CCLUB-OUTRO-BEM      PIC S9(010) COMP-3. 
.               05 PSDCW018-42-CEMPR-OUTRO-BEM      PIC S9(010) COMP-3. 
.               05 PSDCW018-42-CSEQ-PATRM-BEM       PIC S9(005) COMP-3. 
.               05 PSDCW018-42-NINSTC-FINCR-PATRM   PIC S9(004) COMP-3. 
.               05 PSDCW018-42-CFORMA-ALIEN-PATRM   PIC S9(002) COMP-3. 
.               05 PSDCW018-42-VTOT-CONTR-ALIEN  PIC S9(013)V99 COMP-3. 
.               05 PSDCW018-42-CSIT-BEM             PIC  X(001).        
.                                                                       
.     ******************************************************************
.     **** TIPO REG.: 043 - CADUB0Q2 - PARTICICAPAO-PESSOA              
.     ******************************************************************
.                                                                       
.          03   PSDCW018-43-TPRTCP-PSSOA REDEFINES                      
.                                        PSDCW018-01-PSSOA-UNICA.       
.               05 PSDCW018-43-CPF-CNPJ.                                
.                   07  PSDCW018-43-CPF-CNPJ-NRO    PIC S9(009) COMP-3. 
.                   07  PSDCW018-43-CPF-CNPJ-CTR    PIC S9(002) COMP-3. 
.                   07  PSDCW018-43-CPF-CNPJ-FIL    PIC S9(004) COMP-3. 
.               05 PSDCW018-43-CCLUB                PIC S9(010) COMP-3. 
.               05 PSDCW018-43-TP-REG               PIC  X(003).        
.               05 PSDCW018-43-CEMPR                PIC S9(010) COMP-3. 
.               05 PSDCW018-43-CSEQ-QUADR-EMPR      PIC S9(005) COMP-3. 
.               05 PSDCW018-43-CPRTCP-OUTRA-PSSOA   PIC S9(005) COMP-3. 
.               05 PSDCW018-43-CPTCAO-ESPAC-TBELA   PIC S9(002) COMP-3. 
.               05 PSDCW018-43-CCNPJ-EMPR           PIC S9(009) COMP-3. 
.               05 PSDCW018-43-CFLIAL-CNPJ-EMPR     PIC S9(004) COMP-3. 
.               05 PSDCW018-43-CCTRL-CNPJ-EMPR      PIC S9(002) COMP-3. 
.               05 PSDCW018-43-IRZ-SCIAL            PIC  X(070).        
.               05 PSDCW018-43-CINDCD-QUADR-SCIAL   PIC  X(001).        
.               05 PSDCW018-43-PPRTCP-EMPR       PIC S9(003)V99 COMP-3. 
.               05 PSDCW018-43-DINIC-PRTCP-SCIAL    PIC  X(010).        
.               05 PSDCW018-43-DFIM-PRTCP-SCIAL     PIC  X(010).        
.               05 PSDCW018-43-CINDCD-QUADR-ADMTV   PIC  X(001).        
.               05 PSDCW018-43-CCARGO-QUADR-ADMTV   PIC S9(005) COMP-3. 
.               05 PSDCW018-43-DINIC-PRTCP-ADMTV    PIC  X(010).        
.               05 PSDCW018-43-DFIM-PRTCP-ADMTV     PIC  X(010).        
.               05 PSDCW018-43-CINDCD-COSLH-ADMTV   PIC  X(001).        
.               05 PSDCW018-43-CCARGO-COSLH-ADMTV   PIC S9(005) COMP-3. 
.               05 PSDCW018-43-DINIC-PRTCP-COSLH    PIC  X(010).        
.               05 PSDCW018-43-DFIM-PRTCP-COSLH     PIC  X(010).        
.                                                                       
.     ******************************************************************
.     **** TIPO REG.: 044 - CADUB0Q4 - QUADRO-PESSOA                    
.     ******************************************************************
.                                                                       
.          03   PSDCW018-44-TQUADR-PSSOA REDEFINES                      
.                                        PSDCW018-01-PSSOA-UNICA.       
.               05 PSDCW018-44-CPF-CNPJ.                                
.                   07  PSDCW018-44-CPF-CNPJ-NRO    PIC S9(009) COMP-3. 
.                   07  PSDCW018-44-CPF-CNPJ-CTR    PIC S9(002) COMP-3. 
.                   07  PSDCW018-44-CPF-CNPJ-FIL    PIC S9(004) COMP-3. 
.               05 PSDCW018-44-CCLUB                PIC S9(010) COMP-3. 
.               05 PSDCW018-44-TP-REG               PIC  X(003).        
.               05 PSDCW018-44-CEMPR                PIC S9(010) COMP-3. 
.               05 PSDCW018-44-CSEQ-QUADR-EMPR      PIC S9(005) COMP-3. 
.               05 PSDCW018-44-CPTCAO-ESPAC-TBELA   PIC S9(002) COMP-3. 
.               05 PSDCW018-44-CCPF-CNPJ            PIC S9(009) COMP-3. 
.               05 PSDCW018-44-CFLIAL-CPF-CNPJ      PIC S9(004) COMP-3. 
.               05 PSDCW018-44-CCTRL-CPF-CNPJ       PIC S9(002) COMP-3. 
.               05 PSDCW018-44-IPARTC-EMPR          PIC  X(070).        
.               05 PSDCW018-44-CINDCD-QUADR-SCIAL   PIC  X(001).        
.               05 PSDCW018-44-PCAPTL-TOT        PIC S9(003)V99 COMP-3. 
.               05 PSDCW018-44-DINIC-PRTCP-SCIAL    PIC  X(010).        
.               05 PSDCW018-44-DFIM-PRTCP-SCIAL     PIC  X(010).        
.               05 PSDCW018-44-CINDCD-QUADR-ADMTV   PIC  X(001).        
.               05 PSDCW018-44-CCARGO-QUADR-ADMTV   PIC S9(005) COMP-3. 
.               05 PSDCW018-44-DINIC-PRTCP-ADMTV    PIC  X(010).        
.               05 PSDCW018-44-DFIM-PRTCP-ADMTV     PIC  X(010).        
.               05 PSDCW018-44-CINDCD-COSLH-ADMTV   PIC  X(001).        
.               05 PSDCW018-44-CCARGO-COSLH-ADMTV   PIC S9(005) COMP-3. 
.               05 PSDCW018-44-DINIC-PRTCP-COSLH    PIC  X(010).        
.               05 PSDCW018-44-DFIM-PRTCP-COSLH     PIC  X(010).        
.               05 PSDCW018-44-CPSSOA-PRTCP-EMPR    PIC  X(001).        
.               05 PSDCW018-44-CFOTGF-PSSOA         PIC S9(009) COMP-3. 
.                                                                       
.     ******************************************************************
.     **** TIPO REG.: 045 - CADUB0K6 - RECEITA-PESSOA                   
.     ******************************************************************
.                                                                       
.          03   PSDCW018-45-TRECTA-PSSOA REDEFINES                      
.                                        PSDCW018-01-PSSOA-UNICA.       
.               05 PSDCW018-45-CPF-CNPJ.                                
.                   07  PSDCW018-45-CPF-CNPJ-NRO    PIC S9(009) COMP-3. 
.                   07  PSDCW018-45-CPF-CNPJ-CTR    PIC S9(002) COMP-3. 
.                   07  PSDCW018-45-CPF-CNPJ-FIL    PIC S9(004) COMP-3. 
.               05 PSDCW018-45-CCLUB                PIC S9(010) COMP-3. 
.               05 PSDCW018-45-TP-REG               PIC  X(003).        
.               05 PSDCW018-45-CEMPR                PIC S9(010) COMP-3. 
.               05 PSDCW018-45-CTPO-RECTA           PIC S9(002) COMP-3. 
.               05 PSDCW018-45-DMES-ANO-RECTA       PIC S9(006) COMP-3. 
.               05 PSDCW018-45-CPTCAO-ESPAC-TBELA   PIC S9(002) COMP-3. 
.               05 PSDCW018-45-VRECTA-PSSOA      PIC S9(013)V99 COMP-3. 
.                                                                       
.     ******************************************************************
.     **** TIPO REG.: 046 - CADUB0M5 - RENDA-PESSOA-UNICA               
.     ******************************************************************
.                                                                       
.          03   PSDCW018-46-TRENDA-PSSOA-UNIC REDEFINES                 
.                                             PSDCW018-01-PSSOA-UNICA.  
.               05 PSDCW018-46-CPF-CNPJ.                                
.                   07  PSDCW018-46-CPF-CNPJ-NRO    PIC S9(009) COMP-3. 
.                   07  PSDCW018-46-CPF-CNPJ-CTR    PIC S9(002) COMP-3. 
.                   07  PSDCW018-46-CPF-CNPJ-FIL    PIC S9(004) COMP-3. 
.               05 PSDCW018-46-CCLUB                PIC S9(010) COMP-3. 
.               05 PSDCW018-46-TP-REG               PIC  X(003).        
.               05 PSDCW018-46-CEMPR                PIC S9(010) COMP-3. 
.               05 PSDCW018-46-NEMPRG-PJ            PIC S9(003) COMP-3. 
.               05 PSDCW018-46-DMES-ANO-RENDA       PIC S9(006) COMP-3. 
.               05 PSDCW018-46-CPTCAO-ESPAC-TBELA   PIC S9(002) COMP-3. 
CPMALP          05 PSDCW018-46-VRENDA-PSSOA      PIC S9(013)V99 COMP-3. 
CPMTEL******************************************************************
.     **** TIPO REG.: 047 - CADUB0M1 - EMPRESA-PESSOA-UNICA             
.     ******************************************************************
.                                                                       
.          03   PSDCW018-47-TEMPR-PSSOA-UNIC REDEFINES                  
.                                             PSDCW018-01-PSSOA-UNICA.  
.               05 PSDCW018-47-CPF-CNPJ.                                
.                   07  PSDCW018-47-CPF-CNPJ-NRO    PIC S9(009) COMP-3. 
.                   07  PSDCW018-47-CPF-CNPJ-CTR    PIC S9(002) COMP-3. 
.                   07  PSDCW018-47-CPF-CNPJ-FIL    PIC S9(004) COMP-3. 
.               05 PSDCW018-47-CCLUB                PIC S9(010) COMP-3. 
.               05 PSDCW018-47-TP-REG               PIC  X(003).        
.               05 PSDCW018-47-CEMPR                PIC S9(010) COMP-3. 
.               05 PSDCW018-47-CPTCAO-ESPAC-TBELA   PIC S9(02) COMP-3.  
.               05 PSDCW018-47-CSIT-RLCTO-PSSOA     PIC S9(02) COMP-3.  
.               05 PSDCW018-47-CPSSOA-JURID         PIC S9(10) COMP-3.  
.               05 PSDCW018-47-NSEQ-UND-ORGNZ       PIC S9(08) COMP-3.  
.               05 PSDCW018-47-CGER-RESP            PIC S9(09) COMP-3.  
.               05 PSDCW018-47-RCOMEN-LIVRE-GER     PIC  X(300).        
.               05 PSDCW018-47-CCPO-FALTT           PIC  X(001).        
.               05 PSDCW018-47-CISENT-CPF-CNPJ      PIC  X(001).        
.               05 PSDCW018-47-CMOTVO-ISENC-CPF     PIC S9(001) COMP-3. 
.               05 PSDCW018-47-DSIT-CAD             PIC  X(010).        
.               05 PSDCW018-47-DCAD-PSSOA           PIC  X(010).        
.               05 PSDCW018-47-DULT-RENOV           PIC  X(010).        
.               05 PSDCW018-47-DPROX-RENOV          PIC  X(010).        
.               05 PSDCW018-47-DCLI-DESDE           PIC  X(010).        
.               05 PSDCW018-47-CSIT-CAD-PSSOA       PIC S9(001) COMP-3. 
.               05 PSDCW018-47-CREG-COMPL-JURID     PIC  X(001).        
.               05 PSDCW018-47-CREG-BLOCO-EXPOR     PIC  X(001).        
.               05 PSDCW018-47-CREG-BLOCO-FAMLR     PIC  X(001).        
.               05 PSDCW018-47-CREG-BLOCO-FINCR     PIC  X(001).        
.               05 PSDCW018-47-CREG-BLOCO-IMPOR     PIC  X(001).        
.               05 PSDCW018-47-CREG-FROTA-VEIC      PIC  X(001).        
.               05 PSDCW018-47-CREG-BLOCO-PROFS     PIC  X(001).        
.               05 PSDCW018-47-CCLI-AUTRZ-INFO      PIC  X(001).        
.               05 PSDCW018-47-DCLI-AUTRZ-BACEN     PIC  X(010).        
.               05 PSDCW018-47-CCLI-AUTRZ-BALAN     PIC  X(001).        
.               05 PSDCW018-47-DCLI-AUTRZ-SERASA    PIC  X(010).        
.               05 PSDCW018-47-CPSSOA-POLTC-EXPTO   PIC  X(001).        
.               05 PSDCW018-47-DCLI-PSSOA-EXPTO     PIC  X(010).        
.               05 PSDCW018-47-CRLCTO-PSSOA-EXPTO   PIC  X(001).        
.               05 PSDCW018-47-DCLI-RLCTO-PSSOA     PIC  X(010).        
.               05 PSDCW018-47-CCPF-CNPJ            PIC S9(009) COMP-3. 
.               05 PSDCW018-47-CFLIAL-CPF-CNPJ      PIC S9(004) COMP-3. 
.               05 PSDCW018-47-CCTRL-CPF-CNPJ       PIC S9(002) COMP-3. 
.               05 PSDCW018-47-CPSSOA-UNIC-DEFIC    PIC  X(001).        
.               05 PSDCW018-47-CPSSOA-UNIC-DEPDT    PIC  X(001).        
.               05 PSDCW018-47-CPSSOA-PRTCP-EMPR    PIC  X(001).        
.               05 PSDCW018-47-CREG-BLOCO-FLIAL     PIC  X(001).        
.               05 PSDCW018-47-CREG-BLOCO-DEPDT     PIC  X(001).        
.               05 PSDCW018-47-CBLOCO-PRTCP-EMPR    PIC  X(001).        
.               05 PSDCW018-47-CPPRIE-FLIAL         PIC  X(001).        
.               05 PSDCW018-47-CPPRIE-FROTA         PIC  X(001).        
.               05 PSDCW018-47-CPPRIE-IMOV          PIC  X(001).        
.               05 PSDCW018-47-CPPRIE-MAQNA-EQPMT   PIC  X(001).        
.               05 PSDCW018-47-CPPRIE-VEIC          PIC  X(001).        
.               05 PSDCW018-47-DULT-ATULZ-CAD       PIC  X(010).        
.               05 PSDCW018-47-CCPF-POLTC-DECLD     PIC S9(009) COMP-3. 
.               05 PSDCW018-47-CCTRL-CPF-POLTC      PIC S9(002) COMP-3. 
.               05 PSDCW018-47-CCLASS-CAD-PSSOA     PIC S9(003) COMP-3. 
CPMTEL          05 PSDCW018-47-CVRSAO-PRFIL-CLASS   PIC S9(003) COMP-3. 

      *================================================================*
       IDENTIFICATION                  DIVISION.                        
      *================================================================*
       PROGRAM-ID. RDAB7130.                                            
       AUTHOR. HENRIQUE.                                                
      *================================================================*
      *                        C P M   SISTEMAS                        *
      *================================================================*
      *                                                                *
      *    PROGRAMA     :  RDAB7130                                    *
      *    ANALISTA     :  HENRIQUE      - DDS                         *
      *    DATA         :  29/08/2004                                  *
      *                                                                *
      *    OBJETIVO     :                                              *
      *        CRIAR A BASE LOGRADOURO TELEFONE.                       *
      **                                                               *
      *    ARQUIVOS     :                                              *
      *        DDNAME                          INCLUDE/BOOK            *
      *        ARQLOGRA                          I#RDAB14              *
      *        ARQTELEL                          I#RDAB15              *
      *        ARQENTEL                          I#RDABS7              *
      *                                                                *
      *    MODULOS CHAMADOS:                                           *
      *        POOL7100  -  MODULO DE TRATAMENTO DE ERROS              *
      *                                                                *
      *================================================================*
      *                   A L T E R A C O E S                         * 
      *---------------------------------------------------------------* 
      *                                                               * 
      *      ANALISTA     : LUMENA               - GRUPO 45           * 
      *      DATA         : 15/03/2007                                * 
      *                                                               * 
      *      OBJETIVO     :                                           * 
      *        ALTERAR LRECL INC I#RDAB15 DE 40 PARA 41.              * 
      *        ALTERAR LRECL INC I#RDAB14 DE 145 PARA 146.            * 
      *---------------------------------------------------------------* 
      *                                                               * 
      *      ANALISTA     : RICARDO CRUZ         - GRUPO 71           * 
      *      DATA         : 27/08/2008                                * 
      *                                                               * 
      *      OBJETIVO     :                                           * 
      *        PASSAR A APRESENTAR O TELEFONE DA AFINDER NA POSICAO 2 * 
      *===============================================================* 
PCS023*----------------------------------------------------------------*
PCS023* PROGRAMADOR : PAULINO CARLOS DE SOUZA - BRQ                    *
PCS023* ANALISTA    : PAULINO CARLOS DE SOUZA - BRQ                    *
PCS023*             : 2009/08-0023                                     *
PCS023* DATA        : 24/08/2009                                       *
PCS023* MOTIVO      : ALTEARAR LOGICA PARA TELEFONE, DEIXAR APENAS     *
PCS023*             : LOCALIZADORA E TELEFONES NAO DUPLICADOS *         
PCS023*----------------------------------------------------------------*
PCS023*----------------------------------------------------------------*
PCS023* PROGRAMADOR : PAULINO CARLOS DE SOUZA - BRQ                    *
PCS023* ANALISTA    : PAULINO CARLOS DE SOUZA - BRQ                    *
PCS023*             : 2009/08-0023                                     *
PCS023* DATA        : 07/10/2009                                       *
PCS023* MOTIVO      : DESPREZAR OS REG. ONDE O CAMPO AFN-CINDCD-FONE   *
PCS023*             : IGUAL "1"                                        *
PCS023*----------------------------------------------------------------*
      *                                                                *
BRQ059* =============================================================  *
      * |                      ALTERACAO                            |  *
      * ------------------------------------------------------------+  *
      *  ANALISTA     : ROBSON VELASQUES / BRQ - IT SERVICES        |  *
      *  PROGRAMADOR  : CRISTIANO SOUZA  / BRQ - IT SERVICES        |  *
      *  DATA         : 04/2012                                     |  *
      *  OBJETIVO     : INSERIR UM DIGITO A MAIS AOS CAMPOS DE      |  *
      *                 TELEFONES                                   |  *
      *  PROJETO      : 2012/0059 - ADEQUAR NRO TELEFONE(RES.553)   |  *
      * ============================================================+  *
BRQ059******************************************************************
                                                                        
      *================================================================*
       ENVIRONMENT                     DIVISION.                        
      *================================================================*
                                                                        
      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.                         
      *----------------------------------------------------------------*
                                                                        
                                                                        
       SPECIAL-NAMES.                                                   
           DECIMAL-POINT               IS  COMMA.                       
                                                                        
      *----------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.                         
      *----------------------------------------------------------------*
                                                                        
       FILE-CONTROL.                                                    
                                                                        
           SELECT  ARQLOGRA  ASSIGN    TO  UT-S-ARQLOGRA                
                       FILE  STATUS    IS  WRK-FS-ARQLOGRA.             
                                                                        
           SELECT  ARQTELEL  ASSIGN    TO  UT-S-ARQTELEL                
                       FILE  STATUS    IS  WRK-FS-ARQTELEL.             
                                                                        
           SELECT  ARQENTEL  ASSIGN    TO  UT-S-ARQENTEL                
                       FILE  STATUS    IS  WRK-FS-ARQENTEL.             
                                                                        
      *================================================================*
       DATA                            DIVISION.                        
      *================================================================*
                                                                        
      *----------------------------------------------------------------*
       FILE                            SECTION.                         
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *    INPUT  :  ARQUIVO DE ENDERECOS                              *
1503  *              ORG. SEQUENCIAL   -   LRECL  =  146               *
      *----------------------------------------------------------------*
                                                                        
       FD  ARQLOGRA                                                     
           RECORDING MODE IS F                                          
           LABEL RECORDS IS STANDARD                                    
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       COPY 'I#RDAB14'.
                                                                        
      *----------------------------------------------------------------*
      *    INPUT  :  ARQUIVO DE TELEFONES                              *
BRQ059*              ORG. SEQUENCIAL  -   LRECL  =  044                *
      *----------------------------------------------------------------*
                                                                        
       FD  ARQTELEL                                                     
           RECORDING MODE IS F                                          
           LABEL RECORDS IS STANDARD                                    
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       COPY 'I#RDAB15'.
                                                                        
      *----------------------------------------------------------------*
      *    OUTPUT :  ARQUIVO BASE ENDERECO TELEFONE                    *
      *              ORG. SEQUENCIAL  -   LRECL  =  340                *
      *----------------------------------------------------------------*
                                                                        
       FD  ARQENTEL                                                     
           RECORDING MODE IS F                                          
           LABEL RECORDS IS STANDARD                                    
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       COPY 'I#RDABS7'.
                                                                        
      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.                         
      *----------------------------------------------------------------*
                                                                        
       77  FILLER PIC X(32)    VALUE '*  INICIO DA WORKING  RDAB7130 *'.
                                                                        
      *----------------------------------------------------------------*
      *    ACUMULADORES                                                *
      *----------------------------------------------------------------*
                                                                        
       77  WAC-F                    PIC 9(02)        VALUE ZEROS.       
       77  WAC-ENDERECOS               PIC 9(02)        VALUE ZEROS.    
       77  WAC-LID-FON                 PIC 9(15) COMP-3 VALUE ZEROS.    
       77  WAC-LID-END                 PIC 9(15) COMP-3 VALUE ZEROS.    
       77  WAC-GRV-CAD                 PIC 9(15) COMP-3 VALUE ZEROS.    
       77  WRK-KEY-GRV-ANT             PIC X(12)        VALUE SPACES.   
                                                                        
      *----------------------------------------------------------------*
      *    AREA PARA TESTE DE FILE STATUS                              *
      *----------------------------------------------------------------*
                                                                        
       77  WRK-FS-ARQLOGRA             PIC X(02)  VALUE SPACES.         
       77  WRK-FS-ARQTELEL             PIC X(02)  VALUE SPACES.         
       77  WRK-FS-ARQENTEL             PIC X(02)  VALUE SPACES.         
       77  WRK-ABERTURA                PIC X(13)  VALUE 'NA ABERTURA'.  
       77  WRK-LEITURA                 PIC X(13)  VALUE 'NA LEITURA'.   
       77  WRK-GRAVACAO                PIC X(13)  VALUE 'NA GRAVACAO'.  
       77  WRK-FECHAMENTO              PIC X(13)  VALUE 'NO FECHAMENTO'.
                                                                        
      *----------------------------------------------------------------*
      *    AREAS AUXILIARES                                            *
      *----------------------------------------------------------------*
                                                                        
       77  WRK-BATCH                   PIC X(08)  VALUE SPACES.         
       77  WQTDE                       PIC 9(02)  COMP-3 VALUE 0.       
       77  WIND                        PIC 9(02)  COMP-3 VALUE 0.       
       77  WLOC                        PIC 9(02)  VALUE ZEROS.          
       77  WREF                        PIC 9(02)  VALUE ZEROS.          
       77  WBASE                       PIC 9(02)  COMP-3 VALUE 0.       
       77  WDDDT                       PIC 9(02)  VALUE ZEROS.          
       77  WDDDN                       PIC 9(02)  VALUE ZEROS.          
       77  WFINAL                      PIC 9(02)  COMP-3 VALUE 0.       
       77  WFONT                       PIC 9(02)  VALUE ZEROS.          
       77  WDEST                       PIC 9(02)  VALUE ZEROS.          
       77  WLOCLZD-SM                  PIC 9(02)  VALUE ZEROS.          
       77  WLOCLZD-AF                  PIC 9(02)  VALUE ZEROS.          
PCS023 77  IND-DUP                     PIC 9(02)  COMP-3 VALUE 0.       
       77  WRK-ENDER-VAL-COM           PIC X(01)  VALUE SPACES.         
       77  WRK-ENDER-VAL-RES           PIC X(01)  VALUE SPACES.         
       77  WRK-ARQLOGRA-AUX            PIC X(155) VALUE ZEROS.          
       77  WRK-ENDER-RES               PIC X(01)  VALUE SPACES.         
       77  WRK-ENDER-LOC               PIC X(01)  VALUE SPACES.         
       77  WRK-ENDER-COM               PIC X(01)  VALUE SPACES.         
PCS023**7  WRK-CINDCD-FONE              IC X(01)  VALUE SPACES.         
       77  WAC-PROCESSOU-FONE          PIC X(01)  VALUE SPACES.         
                                                                        
       01  WRK-DATA-AAAAMMDD.                                           
           03  WRK-ANO-AAAAMMDD        PIC 9(04)  VALUE ZEROS.          
           03  WRK-MES-AAAAMMDD        PIC 9(02)  VALUE ZEROS.          
           03  WRK-DIA-AAAAMMDD        PIC 9(02)  VALUE ZEROS.          
                                                                        
       01  WRK-DATA-DB2.                                                
           03  WRK-DIA-DB2             PIC 9(02)  VALUE ZEROS.          
           03  FILLER                  PIC X(01)  VALUE SPACES.         
           03  WRK-MES-DB2             PIC 9(02)  VALUE ZEROS.          
           03  FILLER                  PIC X(01)  VALUE SPACES.         
           03  WRK-ANO-DB2             PIC 9(04)  VALUE ZEROS.          
                                                                        
      *----------------------------------------------------------------*
      *    AREAS PARA EDICAO CORRETA DO DDD (POSICIONAR 00NN)          *
      *----------------------------------------------------------------*
                                                                        
       01  WRK-DDD.                                                     
           03  WRK-DDD-DIG             OCCURS  4  TIMES                 
                                       PIC 9(01).                       
                                                                        
       01  WRK-DDDN                    PIC 9(04).                       
       01  FILLER                      REDEFINES  WRK-DDDN.             
           03  WRK-DDDN-DIG            OCCURS  4  TIMES                 
                                       PIC 9(01).                       
                                                                        
BRQ059 01  WRK-FONE-ESP.                                                
  -        03  WRK-FESP-FN0.                                            
  -            05  WRK-FESP-DIG1       PIC X(01) VALUE SPACES.          
  -            05  WRK-FESP-DIG2       PIC X(01) VALUE SPACES.          
  -            05  WRK-FESP-DIG3       PIC X(01) VALUE SPACES.          
  -        03  WRK-FESP-FN1            PIC X(01) VALUE SPACES.          
  -        03  WRK-FESP-FN8.                                            
  -            05  WRK-FESP-FN2        PIC X(01) VALUE SPACES.          
  -            05  WRK-FESP-FN6        PIC X(06) VALUE SPACES.          
  -            05  FILLER              PIC X(01) VALUE SPACES.          
  -                                                                     
  -    01  WRK-FONE-ESP-R.                                              
  -        03  WRK-FESP-FN1-R          PIC X(01) VALUE SPACES.          
  -        03  WRK-FESP-FN8-R.                                          
  -            05  WRK-FESP-FN2-R      PIC X(01) VALUE SPACES.          
  -            05  WRK-FESP-FN6-R      PIC X(06) VALUE SPACES.          
  -            05  FILLER              PIC X(01) VALUE SPACES.          
  -                                                                     
  -    01  WRK-FONE-INVALIDO.                                           
  -        03  WRK-FINV-FN2            PIC X(05) VALUE SPACES.          
BRQ059     03  WRK-FINV-FN8            PIC X(06) VALUE SPACES.          
                                                                        
      *----------------------------------------------------------------*
      *    AREAS PARA GUARDAR REGISTROS SELECIONADOS                   *
      *----------------------------------------------------------------*
                                                                        
       01  WRK-ENDERECO-RES.                                            
           03  WRK-ELOGDR-RES          PIC  X(40)        VALUE SPACES.  
           03  WRK-ENRO-LOGDR-RES      PIC  X(07)        VALUE SPACES.  
           03  WRK-ECOMPL-LOGDR-RES    PIC  X(20)        VALUE SPACES.  
           03  WRK-EBAIRO-LOGDR-RES    PIC  X(20)        VALUE SPACES.  
           03  WRK-IMUN-IBGE-RES       PIC  X(25)        VALUE SPACES.  
           03  WRK-CCEP-CLI-RES        PIC S9(05) COMP-3 VALUE ZEROS.   
           03  WRK-CCEP-COMPL-RES      PIC S9(03) COMP-3 VALUE ZEROS.   
           03  WRK-CSGL-UF-CLI-RES     PIC  X(02)        VALUE SPACES.  
           03  WRK-CINDCD-LOGDR-RES    PIC  X(01)        VALUE SPACES.  
                                                                        
       01  WRK-ENDERECO-COM.                                            
           03  WRK-ELOGDR-COM          PIC  X(40)        VALUE SPACES.  
           03  WRK-ENRO-LOGDR-COM      PIC  X(07)        VALUE SPACES.  
           03  WRK-ECOMPL-LOGDR-COM    PIC  X(20)        VALUE SPACES.  
           03  WRK-EBAIRO-LOGDR-COM    PIC  X(20)        VALUE SPACES.  
           03  WRK-IMUN-IBGE-COM       PIC  X(25)        VALUE SPACES.  
           03  WRK-CCEP-CLI-COM        PIC S9(05) COMP-3 VALUE ZEROS.   
           03  WRK-CCEP-COMPL-COM      PIC S9(03) COMP-3 VALUE ZEROS.   
           03  WRK-CSGL-UF-CLI-COM     PIC  X(02)        VALUE SPACES.  
           03  WRK-CINDCD-LOGDR-COM    PIC  X(01)        VALUE SPACES.  
                                                                        
      *----------------------------------------------------------------*
      *    CHAVES PARA ARQUIVOS                                        *
      *----------------------------------------------------------------*
                                                                        
       01  WRK-CHV-ARQLOGRA-ATU.                                        
           03  WRK-CBCO-LOGRA-ATU      PIC 9(03)  VALUE ZEROS.          
           03  WRK-CAG-BCRIA-LOGRA-ATU PIC 9(05)  VALUE ZEROS.          
           03  WRK-CCTA-CORR-LOGRA-ATU PIC 9(13)  VALUE ZEROS.          
                                                                        
       01  WRK-CHV-ARQLOGRA-ANT.                                        
           03  WRK-CBCO-LOGRA-ANT      PIC 9(03)  VALUE ZEROS.          
           03  WRK-CAG-BCRIA-LOGRA-ANT PIC 9(05)  VALUE ZEROS.          
           03  WRK-CCTA-CORR-LOGRA-ANT PIC 9(13)  VALUE ZEROS.          
                                                                        
       01  WRK-CHV-ARQTELEL-ATU.                                        
           03  WRK-CBCO-TELEL-ATU      PIC 9(03)  VALUE ZEROS.          
           03  WRK-CAG-BCRIA-TELEL-ATU PIC 9(05)  VALUE ZEROS.          
           03  WRK-CCTA-CORR-TELEL-ATU PIC 9(13)  VALUE ZEROS.          
                                                                        
       01  WRK-CHV-ARQTELEL-ANT.                                        
           03  WRK-CBCO-TELEL-ANT      PIC 9(03)  VALUE ZEROS.          
           03  WRK-CAG-BCRIA-TELEL-ANT PIC 9(05)  VALUE ZEROS.          
           03  WRK-CCTA-CORR-TELEL-ANT PIC 9(13)  VALUE ZEROS.          
                                                                        
      *----------------------------------------------------------------*
      *    AREA DE MENSAGENS                                           *
      *----------------------------------------------------------------*
                                                                        
       01  WRK-ERRO.                                                    
           03  FILLER                  PIC X(05)  VALUE 'ERRO'.         
           03  WRK-OPERACAO            PIC X(13)  VALUE SPACES.         
           03  FILLER                  PIC X(12)  VALUE ' DO ARQUIVO '. 
           03  WRK-ARQUIVO             PIC X(09)  VALUE SPACES.         
           03  FILLER                  PIC X(15)  VALUE                 
               ' FILE STATUS = '.                                       
           03  WRK-FILE-STATUS         PIC X(02)  VALUE SPACES.         
                                                                        
       01  WRK-MENSAGENS.                                               
           03  WRK-MENS01              PIC X(75)  VALUE                 
               'ARQUIVO ARQLOGRA ESTA VAZIO'.                           
           03  WRK-MENS02              PIC X(75)  VALUE                 
               'ARQUIVO ARQTELEL ESTA VAZIO'.                           
                                                                        
      *----------------------------------------------------------------*
      *    TABELA INTERNA P/ REGISTROS DA MESMA CHAVE - ARQLOGRA       *
      *----------------------------------------------------------------*
                                                                        
       01  TAB-ARQLOGRA-X.                                              
           03  TAB-ARQLOGRA            OCCURS 30 TIMES.                 
               05  TAB-CBCO            PIC 9(03)  VALUE ZEROS.          
               05  TAB-CAG-BCRIA       PIC 9(05)  VALUE ZEROS.          
               05  TAB-CCTA-CORR       PIC 9(13)  VALUE ZEROS.          
               05  TAB-ORIGE-LOGDR     PIC X(01)  VALUE SPACES.         
               05  TAB-TPO-LOGDR       PIC X(01)  VALUE SPACES.         
               05  TAB-ELOGDR-CLI      PIC X(40)  VALUE SPACES.         
               05  TAB-ENRO-LOGDR      PIC X(07)  VALUE SPACES.         
               05  TAB-ECOMPL-LOGDR    PIC X(20)  VALUE SPACES.         
               05  TAB-EBAIRO-LOGDR    PIC X(20)  VALUE SPACES.         
               05  TAB-IMUN-IBGE       PIC X(25)  VALUE SPACES.         
               05  TAB-CCEP-CLI        PIC 9(05)  VALUE ZEROS.          
               05  TAB-CCEP-COMPL      PIC 9(03)  VALUE ZEROS.          
               05  TAB-CSGL-UF-CLI     PIC X(02)  VALUE SPACES.         
               05  TAB-CINDCD-LOGDR    PIC X(01)  VALUE SPACES.         
               05  TAB-CINDCD-ATULZ-LOGRA                               
                                       PIC X(01)  VALUE SPACES.         
               05  TAB-DATA-LOGRA      PIC 9(08)  VALUE ZEROS.          
                                                                        
                                                                        
      *----------------------------------------------------------------*
      *    TABELA INTERNA P/ REGISTROS DA MESMA CHAVE - ARQTELEL       *
      *----------------------------------------------------------------*
                                                                        
       01  TAB-ARQTELEL-X.                                              
           03  TAB-ARQTELEL            OCCURS 20 TIMES.                 
               05  TAB-CBCO-FONE       PIC 9(03) COMP-3 VALUE ZEROS.    
               05  TAB-CAG-BCRIA-FONE  PIC 9(05) COMP-3 VALUE ZEROS.    
               05  TAB-CCTA-CORR-FONE  PIC 9(13) COMP-3 VALUE ZEROS.    
               05  TAB-CINDCD-ORIGE-FONE                                
                                       PIC X(01)        VALUE SPACES.   
               05  TAB-TPO-FONE-TELEL  PIC X(01)        VALUE SPACES.   
               05  TAB-CSEQ-FONE       PIC 9(03) COMP-3 VALUE ZEROS.    
               05  TAB-CDDD-CLI-TELEL  PIC X(04)        VALUE SPACES.   
BRQ059         05  TAB-CFONE-TELEL     PIC X(11)        VALUE SPACES.   
               05  TAB-CINDCD-FONE-TELEL                                
                                       PIC X(01)        VALUE SPACES.   
               05  TAB-CINDCD-ATULZ-TELEL                               
                                       PIC X(01)        VALUE SPACES.   
               05  TAB-DATA-TELEL      PIC 9(08)        VALUE ZEROS.    
                                                                        
      *----------------------------------------------------------------*
      *    TABELA INTERNA P/ OS TELEFONES VALIDOS MAIS ATUAIS          *
      *----------------------------------------------------------------*
                                                                        
       01  TAB-FONE-X.                                                  
           03  TAB-FONE                OCCURS 20 TIMES.                 
               05  TAB-CINDCD-TPO-FONE PIC X(01) VALUE SPACES.          
               05  TAB-CINDCD-ORI-FONE PIC X(01) VALUE SPACES.          
               05  TAB-CDDD-CLI-RENEG  PIC X(04) VALUE SPACES.          
BRQ059         05  TAB-CFONE-RENEG     PIC X(11) VALUE SPACES.          
               05  TAB-CINDCD-FONE     PIC X(01) VALUE SPACES.          
                                                                        
      *----------------------------------------------------------------*
      *    TABELA INTERNA P/ OS TELEFONES VALIDOS MAIS ATUAIS APOS A   *
      *    VERIFICACAO DA LOCALIZADORA-FONE                             
      *----------------------------------------------------------------*
                                                                        
       01  TAB-FONE-L.                                                  
           03  TAB-FONE-LL               OCCURS 6 TIMES.                
               05  TAB-CINDCD-TPO-FONE-L PIC X(01) VALUE SPACES.        
               05  TAB-CINDCD-ORI-FONE-L PIC X(01) VALUE SPACES.        
               05  TAB-CDDD-CLI-RENEG-L  PIC X(04) VALUE SPACES.        
BRQ059         05  TAB-CFONE-RENEG-L     PIC X(11) VALUE SPACES.        
               05  TAB-CINDCD-FONE-L     PIC X(01) VALUE SPACES.        
                                                                        
       01  W-AET-TAB-FONE-X.                                            
           05   W-AET-TAB-FONE             OCCURS 6 TIMES.              
                07 W-AET-CINDCD-TPO-FONE   PIC  X(001) VALUE SPACES.    
                07 W-AET-CDDD-CLI-RENEG    PIC  9(004) VALUE ZEROS.     
BRQ059          07 W-AET-CFONE-RENEG       PIC  9(011) VALUE ZEROS.     
                07 W-AET-CINDCD-FONE       PIC  X(001) VALUE SPACES.    
      *----------------------------------------------------------------*
      *    AREA PARA CHAMADA DA POOL7100                               *
      *----------------------------------------------------------------*
                                                                        
       COPY POL7100C.                                                   
                                                                        
       01  FILLER      PIC X(32)  VALUE '*  FIM DA WORKING RDAB7130 *'. 
                                                                        
      *================================================================*
       PROCEDURE                       DIVISION.                        
      *================================================================*
                                                                        
       00000-ROTINA-PRINCIPAL          SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           PERFORM 10000-INICIALIZAR                                    
                                                                        
           PERFORM 20000-VERIFICAR-VAZIO.                               
                                                                        
           PERFORM 30000-PROCESSAR UNTIL                                
               WRK-CHV-ARQLOGRA-ATU    EQUAL HIGH-VALUES.               
                                                                        
           PERFORM 40000-FINALIZAR.                                     
                                                                        
      *----------------------------------------------------------------*
       00000-99-FIM.                   EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       10000-INICIALIZAR               SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           OPEN  INPUT  ARQLOGRA                                        
                        ARQTELEL                                        
                 OUTPUT ARQENTEL.                                       
                                                                        
           MOVE WRK-ABERTURA           TO WRK-OPERACAO.                 
           PERFORM  11000-TESTAR-FILE-STATUS.                           
                                                                        
      *----------------------------------------------------------------*
       10000-99-FIM.                   EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       11000-TESTAR-FILE-STATUS        SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           PERFORM 11100-TESTAR-FS-ARQLOGRA.                            
                                                                        
           PERFORM 11200-TESTAR-FS-ARQTELEL.                            
                                                                        
           PERFORM 11300-TESTAR-FS-ARQENTEL.                            
                                                                        
      *----------------------------------------------------------------*
       11000-99-FIM.                   EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       11100-TESTAR-FS-ARQLOGRA        SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           IF  WRK-FS-ARQLOGRA         NOT EQUAL ZEROS                  
               MOVE 'ARQLOGRA'         TO WRK-ARQUIVO                   
               MOVE WRK-FS-ARQLOGRA    TO WRK-FILE-STATUS               
               MOVE WRK-ERRO           TO ERR-TEXTO                     
               PERFORM 99999-ROTINA-ERRO                                
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       11100-99-FIM.                   EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       11200-TESTAR-FS-ARQTELEL        SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           IF  WRK-FS-ARQTELEL         NOT EQUAL ZEROS                  
               MOVE 'ARQTELEL'         TO WRK-ARQUIVO                   
               MOVE WRK-FS-ARQTELEL    TO WRK-FILE-STATUS               
               MOVE WRK-ERRO           TO ERR-TEXTO                     
               PERFORM 99999-ROTINA-ERRO                                
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       11200-99-FIM.                   EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       11300-TESTAR-FS-ARQENTEL        SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           IF  WRK-FS-ARQENTEL         NOT EQUAL ZEROS                  
               MOVE 'ARQENTEL'         TO WRK-ARQUIVO                   
               MOVE WRK-FS-ARQENTEL    TO WRK-FILE-STATUS               
               MOVE WRK-ERRO           TO ERR-TEXTO                     
               PERFORM 99999-ROTINA-ERRO                                
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       11300-99-FIM.                   EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       20000-VERIFICAR-VAZIO           SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           PERFORM 21000-LER-ARQLOGRA.                                  
                                                                        
           IF  WRK-CHV-ARQLOGRA-ATU    EQUAL HIGH-VALUES                
               DISPLAY '*************** RDAB7130 ***************'       
               DISPLAY '*                                      *'       
               DISPLAY '*       ARQUIVO ARQLOGRA VAZIO         *'       
               DISPLAY '*                                      *'       
               DISPLAY '*      PROCESSAMENTO  CANCELADO        *'       
               DISPLAY '*                                      *'       
               DISPLAY '*************** RDAB7130 ***************'       
               MOVE WRK-MENS01         TO ERR-TEXTO                     
               PERFORM 99999-ROTINA-ERRO                                
           END-IF.                                                      
                                                                        
           MOVE WRK-CHV-ARQLOGRA-ATU   TO WRK-CHV-ARQLOGRA-ANT.         
                                                                        
           PERFORM 22000-LER-ARQTELEL.                                  
                                                                        
           IF  WRK-CHV-ARQTELEL-ATU      EQUAL HIGH-VALUES              
               DISPLAY '*************** RDAB7130 ***************'       
               DISPLAY '*                                      *'       
               DISPLAY '*       ARQUIVO ARQTELEL VAZIO         *'       
               DISPLAY '*                                      *'       
               DISPLAY '*      PROCESSAMENTO  CANCELADO        *'       
               DISPLAY '*                                      *'       
               DISPLAY '*************** RDAB7130 ***************'       
               MOVE WRK-MENS02         TO ERR-TEXTO                     
               PERFORM 99999-ROTINA-ERRO                                
           END-IF.                                                      
                                                                        
           MOVE WRK-CHV-ARQTELEL-ATU   TO WRK-CHV-ARQTELEL-ANT.         
                                                                        
      *----------------------------------------------------------------*
       20000-99-FIM.                   EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       21000-LER-ARQLOGRA              SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           READ ARQLOGRA.                                               
                                                                        
           IF  WRK-FS-ARQLOGRA         EQUAL '10'                       
               MOVE HIGH-VALUES        TO WRK-CHV-ARQLOGRA-ATU          
               GO                      TO 21000-99-FIM                  
           END-IF.                                                      
                                                                        
           ADD 1                       TO WAC-LID-END.                  
                                                                        
           MOVE WRK-LEITURA            TO WRK-OPERACAO.                 
           PERFORM 11100-TESTAR-FS-ARQLOGRA.                            
                                                                        
           MOVE ALG-CBCO               TO WRK-CBCO-LOGRA-ATU.           
           MOVE ALG-CAG-BCRIA          TO WRK-CAG-BCRIA-LOGRA-ATU.      
           MOVE ALG-CCTA-CORR          TO WRK-CCTA-CORR-LOGRA-ATU.      
                                                                        
      *----------------------------------------------------------------*
       21000-99-FIM.                   EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       22000-LER-ARQTELEL              SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           READ ARQTELEL.                                               
                                                                        
           IF  WRK-FS-ARQTELEL         EQUAL '10'                       
               MOVE HIGH-VALUES        TO WRK-CHV-ARQTELEL-ATU          
               GO                      TO 22000-99-FIM                  
           END-IF.                                                      
                                                                        
           ADD 1                       TO WAC-LID-FON.                  
           MOVE WRK-LEITURA            TO WRK-OPERACAO.                 
           PERFORM 11200-TESTAR-FS-ARQTELEL.                            
                                                                        
BRQ********IF   AFN-CINDCD-FONE             EQUAL 1                     
BRQ*************GO                     TO 22000-LER-ARQTELEL            
BRQ********END-IF.
           IF AFN-CCTA-CORR EQUAL 14646
               CONTINUE
           END-IF.

           MOVE AFN-CBCO               TO WRK-CBCO-TELEL-ATU.           
           MOVE AFN-CAG-BCRIA          TO WRK-CAG-BCRIA-TELEL-ATU.      
           MOVE AFN-CCTA-CORR          TO WRK-CCTA-CORR-TELEL-ATU.      
                                                                        
      *----------------------------------------------------------------*
       22000-99-FIM.                   EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       30000-PROCESSAR                 SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           INITIALIZE TAB-ARQTELEL-X                                    
                      TAB-FONE-X                                        
                      TAB-FONE-L                                        
                      WAC-F                                             
                      WAC-PROCESSOU-FONE                                
                      WLOCLZD-SM                                        
                      WLOCLZD-AF                                        
                      TAB-ARQLOGRA-X                                    
                      WAC-ENDERECOS                                     
                      WRK-ENDER-VAL-RES                                 
                      WRK-ENDER-VAL-COM                                 
                      WRK-ENDER-RES                                     
                      WRK-ENDER-LOC                                     
                      WRK-ENDER-COM                                     
                      WRK-ENDERECO-RES                                  
                      WRK-ENDERECO-COM                                  
                      W-AET-TAB-FONE-X.                                 
                                                                        
           IF  WRK-CHV-ARQLOGRA-ATU    EQUAL WRK-CHV-ARQTELEL-ATU       
               PERFORM 31000-PROCESSAR-ARQLOGRA                         
               PERFORM 32000-SELECIONAR-TELEFONES                       
               PERFORM 33000-GRAVAR-ARQENTEL                            
           ELSE                                                         
               IF  WRK-CHV-ARQLOGRA-ATU GREATER WRK-CHV-ARQTELEL-ATU    
                   PERFORM 22000-LER-ARQTELEL UNTIL                     
                     WRK-CHV-ARQTELEL-ATU NOT LESS WRK-CHV-ARQLOGRA-ATU 
               ELSE                                                     
                   PERFORM 31000-PROCESSAR-ARQLOGRA                     
                   PERFORM 33000-GRAVAR-ARQENTEL                        
               END-IF                                                   
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       30000-99-FIM.                   EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       31000-PROCESSAR-ARQLOGRA        SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE WRK-CHV-ARQLOGRA-ATU   TO WRK-CHV-ARQLOGRA-ANT.         
                                                                        
           PERFORM VARYING WIND        FROM 1 BY 1 UNTIL                
                  WIND                 GREATER 30 OR                    
                  WRK-CHV-ARQLOGRA-ATU NOT EQUAL WRK-CHV-ARQLOGRA-ANT   
               ADD 1                   TO WAC-ENDERECOS                 
               IF  ALG-CINDCD-LOGDR    EQUAL '1'                        
                   IF  ALG-CINDCD-TPO-LOGDR     EQUAL '1' OR '3' OR '5' 
                       MOVE 'S'            TO WRK-ENDER-VAL-RES         
                   ELSE                                                 
                       IF  ALG-CINDCD-TPO-LOGDR EQUAL '2'               
                           MOVE 'S'    TO WRK-ENDER-VAL-COM             
                       END-IF                                           
                   END-IF                                               
               END-IF                                                   
               PERFORM 31100-CARREGAR-TAB-ARQLOGRA                      
               PERFORM 21000-LER-ARQLOGRA                               
           END-PERFORM.                                                 
                                                                        
           IF  WRK-CHV-ARQLOGRA-ATU    EQUAL WRK-CHV-ARQLOGRA-ANT       
               PERFORM 21000-LER-ARQLOGRA UNTIL                         
                  WRK-CHV-ARQLOGRA-ATU NOT EQUAL WRK-CHV-ARQLOGRA-ANT   
           END-IF.                                                      
                                                                        
           PERFORM 31200-CLASSIFICAR-TAB-ARQLOGRA.                      
                                                                        
           PERFORM 31300-SELECIONAR-ENDERECO-LOC                        
                   VARYING WIND        FROM 1 BY 1                      
                   UNTIL WIND          GREATER WAC-ENDERECOS OR         
                         WRK-ENDER-LOC EQUAL 'S'.                       
                                                                        
           IF WRK-ENDER-LOC = SPACES                                    
              PERFORM 31300-SELECIONAR-ENDERECO-RES                     
                      VARYING WIND        FROM 1 BY 1                   
                      UNTIL WIND          GREATER WAC-ENDERECOS OR      
                            WRK-ENDER-RES EQUAL 'S'.                    
                                                                        
           PERFORM 31400-SELECIONAR-ENDERECO-COM                        
                   VARYING WIND        FROM 1 BY 1                      
                   UNTIL WIND          GREATER WAC-ENDERECOS OR         
                         WRK-ENDER-COM EQUAL 'S'.                       
                                                                        
      *----------------------------------------------------------------*
       31000-99-FIM.                   EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       31100-CARREGAR-TAB-ARQLOGRA     SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE ALG-CBCO               TO TAB-CBCO(WIND).               
           MOVE ALG-CAG-BCRIA          TO TAB-CAG-BCRIA(WIND).          
           MOVE ALG-CCTA-CORR          TO TAB-CCTA-CORR(WIND).          
           MOVE ALG-CINDCD-ORIGE-LOGDR TO TAB-ORIGE-LOGDR(WIND).        
           MOVE ALG-CINDCD-TPO-LOGDR   TO TAB-TPO-LOGDR(WIND).          
           MOVE ALG-ELOGDR-CLI-RENEG   TO TAB-ELOGDR-CLI(WIND).         
           MOVE ALG-ENRO-LOGDR-RENEG   TO TAB-ENRO-LOGDR(WIND).         
           MOVE ALG-ECOMPL-LOGDR-RENEG TO TAB-ECOMPL-LOGDR(WIND).       
           MOVE ALG-EBAIRO-LOGDR-RENEG TO TAB-EBAIRO-LOGDR(WIND).       
           MOVE ALG-IMUN-IBGE-RENEG    TO TAB-IMUN-IBGE(WIND).          
           MOVE ALG-CCEP-CLI-RENEG     TO TAB-CCEP-CLI(WIND).           
           MOVE ALG-CCEP-COMPL-RENEG   TO TAB-CCEP-COMPL(WIND).         
           MOVE ALG-CSGL-UF-CLI-RENEG  TO TAB-CSGL-UF-CLI(WIND).        
           MOVE ALG-CINDCD-LOGDR       TO TAB-CINDCD-LOGDR(WIND).       
           MOVE ALG-CINDCD-ATULZ       TO TAB-CINDCD-ATULZ-LOGRA(WIND). 
           MOVE ALG-HATULZ             TO WRK-DATA-DB2.                 
           MOVE WRK-DIA-DB2            TO WRK-DIA-AAAAMMDD.             
           MOVE WRK-MES-DB2            TO WRK-MES-AAAAMMDD.             
           MOVE WRK-ANO-DB2            TO WRK-ANO-AAAAMMDD.             
           MOVE WRK-DATA-AAAAMMDD      TO TAB-DATA-LOGRA(WIND).         
                                                                        
      *----------------------------------------------------------------*
       31100-99-FIM.                   EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       31200-CLASSIFICAR-TAB-ARQLOGRA  SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           PERFORM VARYING WIND          FROM 1 BY 1 UNTIL              
                   WIND                  GREATER 30 OR                  
                   TAB-DATA-LOGRA(WIND)  EQUAL ZEROS                    
                   COMPUTE WBASE         = WIND + 1                     
               PERFORM VARYING WREF     FROM WBASE BY 1 UNTIL           
                       WREF             GREATER 30 OR                   
                       TAB-DATA-LOGRA(WREF) EQUAL ZEROS                 
                   IF  TAB-DATA-LOGRA(WIND) LESS TAB-DATA-LOGRA(WREF)   
                       MOVE TAB-ARQLOGRA(WIND) TO WRK-ARQLOGRA-AUX      
                       MOVE TAB-ARQLOGRA(WREF) TO TAB-ARQLOGRA(WIND)    
                       MOVE WRK-ARQLOGRA-AUX  TO TAB-ARQLOGRA(WREF)     
                   END-IF                                               
               END-PERFORM                                              
           END-PERFORM.                                                 
                                                                        
      *----------------------------------------------------------------*
       31200-99-FIM.                   EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       31300-SELECIONAR-ENDERECO-LOC    SECTION.                        
      *----------------------------------------------------------------*
                                                                        
           IF  TAB-TPO-LOGDR(WIND)          EQUAL '5'                   
               IF  (TAB-CINDCD-LOGDR(WIND)  EQUAL '1') OR               
                   (TAB-CINDCD-LOGDR(WIND)  EQUAL '2' AND               
                   WRK-ENDER-VAL-RES        EQUAL SPACES)               
                   PERFORM 31310-GUARDAR-ENDERECO-RES                   
                   MOVE 'S'            TO WRK-ENDER-LOC                 
               END-IF                                                   
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       31300-99-FIM.                   EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       31300-SELECIONAR-ENDERECO-RES   SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           IF  TAB-TPO-LOGDR(WIND)          EQUAL '1' OR '3'            
               IF  (TAB-CINDCD-LOGDR(WIND)  EQUAL '1') OR               
                   (TAB-CINDCD-LOGDR(WIND)  EQUAL '2' AND               
                   WRK-ENDER-VAL-RES        EQUAL SPACES)               
                   PERFORM 31310-GUARDAR-ENDERECO-RES                   
                   MOVE 'S'            TO WRK-ENDER-RES                 
               END-IF                                                   
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       31300-99-FIM.                   EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       31310-GUARDAR-ENDERECO-RES      SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE TAB-ELOGDR-CLI(WIND)   TO WRK-ELOGDR-RES.               
           MOVE TAB-ENRO-LOGDR(WIND)   TO WRK-ENRO-LOGDR-RES.           
           MOVE TAB-ECOMPL-LOGDR(WIND) TO  WRK-ECOMPL-LOGDR-RES.        
           MOVE TAB-EBAIRO-LOGDR(WIND) TO WRK-EBAIRO-LOGDR-RES.         
           MOVE TAB-IMUN-IBGE(WIND)    TO WRK-IMUN-IBGE-RES.            
           MOVE TAB-CCEP-CLI(WIND)     TO WRK-CCEP-CLI-RES.             
           MOVE TAB-CCEP-COMPL(WIND)   TO WRK-CCEP-COMPL-RES.           
           MOVE TAB-CSGL-UF-CLI(WIND)  TO WRK-CSGL-UF-CLI-RES.          
           MOVE TAB-CINDCD-LOGDR(WIND) TO WRK-CINDCD-LOGDR-RES.         
                                                                        
      *----------------------------------------------------------------*
       31310-99-FIM.                   EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       31400-SELECIONAR-ENDERECO-COM   SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           IF  TAB-TPO-LOGDR(WIND)         EQUAL '2'                    
               IF  (TAB-CINDCD-LOGDR(WIND) EQUAL '1') OR                
                   (TAB-CINDCD-LOGDR(WIND) EQUAL '2' AND                
                   WRK-ENDER-VAL-COM   EQUAL SPACES)                    
                   PERFORM 31410-GUARDAR-ENDERECO-COM                   
                   MOVE 'S'            TO WRK-ENDER-COM                 
               END-IF                                                   
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       31400-99-FIM.                   EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       31410-GUARDAR-ENDERECO-COM      SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE TAB-ELOGDR-CLI(WIND)   TO WRK-ELOGDR-COM.               
           MOVE TAB-ENRO-LOGDR(WIND)   TO WRK-ENRO-LOGDR-COM.           
           MOVE TAB-ECOMPL-LOGDR(WIND) TO WRK-ECOMPL-LOGDR-COM.         
           MOVE TAB-EBAIRO-LOGDR(WIND) TO WRK-EBAIRO-LOGDR-COM.         
           MOVE TAB-IMUN-IBGE(WIND)    TO WRK-IMUN-IBGE-COM.            
           MOVE TAB-CCEP-CLI(WIND)     TO WRK-CCEP-CLI-COM.             
           MOVE TAB-CCEP-COMPL(WIND)   TO WRK-CCEP-COMPL-COM.           
           MOVE TAB-CSGL-UF-CLI(WIND)  TO WRK-CSGL-UF-CLI-COM.          
           MOVE TAB-CINDCD-LOGDR(WIND) TO WRK-CINDCD-LOGDR-COM.         
                                                                        
      *----------------------------------------------------------------*
       31410-99-FIM.                   EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       32000-SELECIONAR-TELEFONES      SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE WRK-CHV-ARQTELEL-ATU   TO WRK-CHV-ARQTELEL-ANT.         
                                                                        
           PERFORM VARYING WIND        FROM 1 BY 1 UNTIL                
                  WIND                 GREATER 20 OR                    
                  WRK-CHV-ARQTELEL-ATU NOT EQUAL WRK-CHV-ARQTELEL-ANT   
               PERFORM 32100-CARREGAR-TAB-ARQTELEL                      
               PERFORM 22000-LER-ARQTELEL                               
           END-PERFORM.                                                 
                                                                        
           IF  WRK-CHV-ARQTELEL-ATU    EQUAL WRK-CHV-ARQTELEL-ANT       
               PERFORM 22000-LER-ARQTELEL UNTIL                         
                  WRK-CHV-ARQTELEL-ATU NOT EQUAL WRK-CHV-ARQTELEL-ANT   
           END-IF.                                                      
                                                                        
           PERFORM 32200-CLASSIFICAR-TAB-ARQTELEL.                      
                                                                        
      *----------------------------------------------------------------*
       32000-99-FIM.                   EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       32100-CARREGAR-TAB-ARQTELEL     SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE AFN-CBCO               TO TAB-CBCO-FONE(WIND).          
           MOVE AFN-CAG-BCRIA          TO TAB-CAG-BCRIA-FONE(WIND).     
           MOVE AFN-CCTA-CORR          TO TAB-CCTA-CORR-FONE(WIND).     
           MOVE AFN-CINDCD-ORIGE-FONE  TO TAB-CINDCD-ORIGE-FONE(WIND)   
           MOVE AFN-CINDCD-TPO-FONE    TO TAB-TPO-FONE-TELEL(WIND).     
           MOVE AFN-CSEQ-FONE          TO TAB-CSEQ-FONE(WIND).          
                                                                        
           MOVE AFN-CDDD-CLI-RENEG     TO WRK-DDD                       
           MOVE ZEROS                  TO WRK-DDDN                      
           MOVE 4                      TO WDDDT                         
                                          WDDDN.                        
                                                                        
           PERFORM 35000-EDIT-DDD                                       
                   UNTIL WDDDT       LESS 1.                            
                                                                        
           IF      WRK-DDDN             LESS  100                       
                   GO                     TO  32100-01-SEGUE.           
                                                                        
           MOVE    AFN-CFONE-RENEG        TO  WRK-FONE-ESP              
                                                                        
BRQ059     IF  WRK-FESP-FN0            EQUAL  '000' AND                 
  -            WRK-FESP-FN1            EQUAL  '0'   AND                 
  -            WRK-FESP-FN2            EQUAL  '0'                       
  -            MOVE WRK-FESP-FN8          TO  WRK-FONE-ESP-R            
  -        END-IF.                                                      
  -                                                                     
  -        IF  WRK-FESP-FN0            EQUAL  '000' AND                 
  -            WRK-FESP-FN1-R          EQUAL  '0'   AND                 
  -            WRK-FESP-FN2-R          EQUAL  '0'                       
  -            MOVE WRK-FESP-FN8-R        TO  WRK-FONE-ESP-R            
  -        END-IF.                                                      
  -                                                                     
  -        IF  WRK-FESP-FN0            EQUAL  '000' AND                 
  -            WRK-FESP-FN1-R          EQUAL  '0'                       
  -            MOVE WRK-DDDN-DIG(4)       TO  WRK-FESP-FN1-R            
  -            MOVE WRK-FONE-ESP-R        TO  AFN-CFONE-RENEG           
  -            COMPUTE WRK-DDDN            =  WRK-DDDN / 10             
  -        ELSE                                                         
  -            MOVE AFN-CFONE-RENEG       TO  WRK-FONE-ESP              
  -            MOVE WRK-DDDN-DIG(4)       TO  WRK-FESP-DIG3             
  -            MOVE WRK-FONE-ESP          TO  AFN-CFONE-RENEG           
  -            COMPUTE WRK-DDDN            =  WRK-DDDN / 10             
BRQ059     END-IF.                                                      
                                                                        
       32100-01-SEGUE.                                                  
                                                                        
           MOVE WRK-DDDN               TO TAB-CDDD-CLI-TELEL(WIND).     
           MOVE AFN-CFONE-RENEG        TO TAB-CFONE-TELEL(WIND).        
                                                                        
                                                                        
CSN********IF AFN-CINDCD-ORIGE-FONE EQUAL 5                             
CPM-07********IF AFN-CSIT-RETOR-FONE   EQUAL 3                          
CPM-07************MOVE 'X'             TO TAB-CINDCD-FONE-TELEL(WIND)   
CPM-07********ELSE                                                      
CPM-07************IF AFN-CINDCD-FONE   EQUAL 1                          
CPM-07****************MOVE 'X'         TO TAB-CINDCD-FONE-TELEL(WIND)   
CPM-07************ELSE                                                  
CPM-07****************MOVE 'V'         TO TAB-CINDCD-FONE-TELEL(WIND)   
CPM-07************END-IF                                                
CPM-07********END-IF                                                    
CSN********ELSE                                                         
              IF AFN-CINDCD-FONE       EQUAL 1                          
                  MOVE 'X'         TO TAB-CINDCD-FONE-TELEL(WIND)       
              ELSE                                                      
                  MOVE 'V'         TO TAB-CINDCD-FONE-TELEL(WIND).      
                                                                        
           MOVE AFN-HATULZ             TO WRK-DATA-DB2.                 
           MOVE WRK-DIA-DB2            TO WRK-DIA-AAAAMMDD.             
           MOVE WRK-MES-DB2            TO WRK-MES-AAAAMMDD.             
           MOVE WRK-ANO-DB2            TO WRK-ANO-AAAAMMDD.             
           MOVE WRK-DATA-AAAAMMDD      TO TAB-DATA-TELEL(WIND).         
                                                                        
PCS023     MOVE AFN-CFONE-RENEG        TO WRK-FONE-INVALIDO.            
PCS023     IF   WRK-FINV-FN2                EQUAL '00000'               
PCS023***************************************************************** 
PCS023*****     VOU DESPREZAR SE OS DOIS PRIMEIROS NUMEROS DO ********* 
PCS023*****     TELEFONE FOR IGUAL ZEROS                      ********* 
PCS023***************************************************************** 
PCS023          PERFORM 3216-LIMPA-ATUAL                                
PCS023     ELSE                                                         
PCS023          MOVE 1                  TO IND-DUP                      
PCS023          PERFORM 3215-VERIFICA-DUPLICADO                         
PCS023            UNTIL IND-DUP         GREATER 20                      
PCS023     END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       32100-99-FIM.                   EXIT.                            
      *----------------------------------------------------------------*
                                                                        
PCS023*----------------------------------------------------------------*
PCS023 3215-VERIFICA-DUPLICADO            SECTION.                      
PCS023*----------------------------------------------------------------*
=                                                                       
=          IF WIND              NOT EQUAL  IND-DUP                      
=             IF TAB-CFONE-TELEL(WIND)                                  
BRQ                                 EQUAL TAB-CFONE-TELEL(IND-DUP) AND
BRQ              TAB-CINDCD-FONE-TELEL(WIND)
BRQ                                NOT EQUAL 'X'
=                PERFORM 3215-TRATA-DUPLICADO                           
=             END-IF                                                    
=          END-IF.                                                      
=                                                                       
=          ADD 1                       TO IND-DUP.                      
=                                                                       
PCS023*----------------------------------------------------------------*
PCS023 3215-99-FIM.                    EXIT.                            
PCS023*----------------------------------------------------------------*
=                                                                       
=     ***************************************************************** 
=     *                 SE O QUE ESTA ENTRANDO E LOCALIZADORA         * 
=     *             DEVE PREVALECER LOCALIZADORA PARA DUPLICADO       * 
=     *                 SE O QUE ESTA ENTRANDO NAO E LOCALIZADORA     * 
=     *             DEVE ZERAR CAMPOS WIND E VOLTAR A LER             * 
=     ***************************************************************** 
PCS023*----------------------------------------------------------------*
PCS023 3215-TRATA-DUPLICADO            SECTION.                         
PCS023*----------------------------------------------------------------*
=                                                                       
=          IF (TAB-CINDCD-ORIGE-FONE(WIND)                              
=                                   EQUAL '5' OR '9')                   
=             IF (TAB-CINDCD-ORIGE-FONE(IND-DUP)                        
=                               NOT EQUAL '5' AND '9')                  
=                MOVE TAB-ARQTELEL(WIND)  TO TAB-ARQTELEL(IND-DUP)      
=             END-IF                                                    
=          END-IF.                                                      
=                                                                       
=          PERFORM 3216-LIMPA-ATUAL.                                    
=                                                                       
PCS023*----------------------------------------------------------------*
PCS023 3215-99-FIM.                    EXIT.                            
PCS023*----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       3216-LIMPA-ATUAL                SECTION.                         
      *----------------------------------------------------------------*
=          MOVE  0              TO TAB-CBCO-FONE(WIND)                  
=                                  TAB-CAG-BCRIA-FONE(WIND)             
=                                  TAB-CCTA-CORR-FONE(WIND)             
=                                  TAB-CSEQ-FONE(WIND)                  
=                                  TAB-DATA-TELEL(WIND)                 
=          MOVE SPACES          TO TAB-CINDCD-ORIGE-FONE(WIND)          
=                                  TAB-TPO-FONE-TELEL(WIND)             
=                                  TAB-CDDD-CLI-TELEL(WIND)             
=                                  TAB-CFONE-TELEL(WIND)                
=                                  TAB-CINDCD-FONE-TELEL(WIND)          
=                                  TAB-CINDCD-ATULZ-TELEL(WIND)         
=          MOVE 20               TO IND-DUP                             
=          COMPUTE WIND = WIND - 1.                                     
      *----------------------------------------------------------------*
       3216-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       32200-CLASSIFICAR-TAB-ARQTELEL  SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE ZEROS                  TO WAC-F                         
                                          WLOCLZD-SM                    
                                          WLOCLZD-AF.                   
                                                                        
      ******OVE 'V'                    TO WRK-CINDCD-FONE.              
                                                                        
           MOVE SPACES                 TO WAC-PROCESSOU-FONE.           
                                                                        
           PERFORM 32210-CARREGAR-TAB-FONE UNTIL                        
                   WAC-PROCESSOU-FONE EQUAL 'S'.                        
                                                                        
           MOVE ZEROS              TO    WLOC                           
                                         WLOCLZD-SM                     
                                         WLOCLZD-AF.                    
                                                                        
           PERFORM 32220-VERIFICAR-LOCALIZADORA UNTIL                   
                   WLOC                EQUAL 20.                        
                                                                        
           MOVE 1                  TO    WFONT.                         
                                                                        
           IF ( WLOCLZD-SM > 1 ) OR ( WLOCLZD-AF > 1 )                  
              MOVE    1    TO     WDEST                                 
              PERFORM 32230-CARREGAR-LOCALIZADORA UNTIL                 
                      WFONT             EQUAL 6                         
              MOVE TAB-FONE-L          TO TAB-FONE-X                    
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       32200-99-FIM.                   EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       32210-CARREGAR-TAB-FONE         SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           ADD 1                   TO      WAC-F.                       
                                                                        
           IF  WAC-F            GREATER     20                          
               MOVE 'S'            TO      WAC-PROCESSOU-FONE           
               GO                  TO      32210-99-FIM.                
                                                                        
           IF TAB-DATA-TELEL(WAC-F) EQUAL   ZEROS                       
               MOVE 'S'            TO      WAC-PROCESSOU-FONE           
               GO                  TO      32210-99-FIM.                
                                                                        
                                                                        
           MOVE TAB-TPO-FONE-TELEL(WAC-F)                               
                                   TO TAB-CINDCD-TPO-FONE(WAC-F).       
           MOVE TAB-CINDCD-ORIGE-FONE(WAC-F)                            
                                   TO TAB-CINDCD-ORI-FONE(WAC-F).       
           MOVE TAB-CDDD-CLI-TELEL(WAC-F)                               
                                   TO TAB-CDDD-CLI-RENEG(WAC-F).        
           MOVE TAB-CFONE-TELEL(WAC-F)                                  
                                   TO TAB-CFONE-RENEG(WAC-F).           
           MOVE TAB-CINDCD-FONE-TELEL(WAC-F)                            
                                   TO TAB-CINDCD-FONE(WAC-F).           
                                                                        
      *----------------------------------------------------------------*
       32210-99-FIM.                   EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       32220-VERIFICAR-LOCALIZADORA         SECTION.                    
      *----------------------------------------------------------------*
                                                                        
           ADD     1          TO    WLOC.                               
                                                                        
           IF  TAB-CINDCD-ORI-FONE (WLOC) EQUAL '5'  AND                
               WLOCLZD-SM                 EQUAL  0                      
               MOVE WLOC           TO WLOCLZD-SM                        
           END-IF.                                                      
                                                                        
CRUZ       IF  TAB-CINDCD-ORI-FONE (WLOC) EQUAL '9'  AND                
CRUZ           WLOCLZD-AF                 EQUAL  0                      
CRUZ           MOVE WLOC           TO WLOCLZD-AF                        
CRUZ       END-IF.                                                      
                                                                        
                                                                        
      *----------------------------------------------------------------*
       32220-99-FIM.                   EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       32230-CARREGAR-LOCALIZADORA          SECTION.                    
      *----------------------------------------------------------------*
                                                                        
           IF  WDEST  =  1                                              
              IF  WLOCLZD-SM  >  0                                      
                 MOVE TAB-FONE (WLOCLZD-SM)  TO TAB-FONE-LL (WDEST)     
                 ADD  1  TO  WDEST                                      
              END-IF                                                    
              IF  WLOCLZD-AF  >  0                                      
                 MOVE TAB-FONE (WLOCLZD-AF)  TO TAB-FONE-LL (WDEST)     
                 ADD  1  TO  WDEST                                      
              END-IF                                                    
           END-IF.                                                      
                                                                        
           IF (WFONT = WLOCLZD-SM) OR (WFONT = WLOCLZD-AF)              
              ADD  1  TO  WFONT                                         
           ELSE                                                         
              MOVE TAB-FONE (WFONT)           TO TAB-FONE-LL (WDEST)    
              ADD  1  TO  WFONT                                         
              ADD  1  TO  WDEST                                         
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       32230-99-FIM.                   EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       33000-GRAVAR-ARQENTEL           SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           INITIALIZE ALG-REG           OF  ARQENTEL.                   
                                                                        
           MOVE WRK-CBCO-LOGRA-ANT      TO  AET-CBCO                    
           MOVE WRK-CAG-BCRIA-LOGRA-ANT TO  AET-CAG-BCRIA               
           MOVE WRK-CCTA-CORR-LOGRA-ANT TO  AET-CCTA-CORR               
           MOVE WRK-ELOGDR-RES          TO  AET-ELOGDR-RES              
           MOVE WRK-ENRO-LOGDR-RES      TO  AET-ENRO-LOGDR-RES          
           MOVE WRK-ECOMPL-LOGDR-RES    TO  AET-ECOMPL-LOGDR-RES        
           MOVE WRK-EBAIRO-LOGDR-RES    TO  AET-EBAIRO-LOGDR-RES        
           MOVE WRK-IMUN-IBGE-RES       TO  AET-IMUN-IBGE-RES           
           MOVE WRK-CCEP-CLI-RES        TO  AET-CCEP-CLI-RES            
           MOVE WRK-CCEP-COMPL-RES      TO  AET-CCEP-COMPL-RES          
           MOVE WRK-CSGL-UF-CLI-RES     TO  AET-CSGL-UF-CLI-RES         
           MOVE WRK-CINDCD-LOGDR-RES    TO  AET-CINDCD-LOGDR-RES        
           MOVE WRK-ELOGDR-COM          TO  AET-ELOGDR-COM              
           MOVE WRK-ENRO-LOGDR-COM      TO  AET-ENRO-LOGDR-COM          
           MOVE WRK-ECOMPL-LOGDR-COM    TO  AET-ECOMPL-LOGDR-COM        
           MOVE WRK-EBAIRO-LOGDR-COM    TO  AET-EBAIRO-LOGDR-COM        
           MOVE WRK-IMUN-IBGE-COM       TO  AET-IMUN-IBGE-COM           
           MOVE WRK-CCEP-CLI-COM        TO  AET-CCEP-CLI-COM            
           MOVE WRK-CCEP-COMPL-COM      TO  AET-CCEP-COMPL-COM          
           MOVE WRK-CSGL-UF-CLI-COM     TO  AET-CSGL-UF-CLI-COM         
           MOVE WRK-CINDCD-LOGDR-COM    TO  AET-CINDCD-LOGDR-COM        
           MOVE AET-CHAVE               TO  WRK-KEY-GRV-ANT             
           MOVE 1                       TO  WIND.                       
                                                                        
           PERFORM 34000-MOVER-FONES                                    
                   UNTIL  WIND     GREATER  6.                          
                                                                        
           MOVE ZEROS                   TO  WIND.                       
                                                                        
           PERFORM 34008-INICIALIZA-TABELA                              
                   UNTIL  WIND     EQUAL    6.                          
                                                                        
           MOVE 1                       TO  WIND                        
                                            WFINAL.                     
                                                                        
           PERFORM 34010-MOVER-FONES-DIF-ZEROS                          
                   UNTIL WFINAL GREATER  6.                             
                                                                        
           WRITE ALG-REG                OF  ARQENTEL.                   
                                                                        
           ADD 1                        TO  WAC-GRV-CAD.                
           MOVE WRK-GRAVACAO            TO  WRK-OPERACAO.               
           PERFORM 11300-TESTAR-FS-ARQENTEL.                            
                                                                        
      *----------------------------------------------------------------*
       33000-99-FIM.                   EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       34000-MOVER-FONES               SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           IF   TAB-CINDCD-TPO-FONE(WIND)  EQUAL  '1' OR '3' OR '5'     
                MOVE  'P'                  TO AET-CINDCD-TPO-FONE(WIND) 
           ELSE                                                         
                MOVE  'B'                  TO AET-CINDCD-TPO-FONE(WIND) 
           END-IF.                                                      
           MOVE TAB-CINDCD-FONE(WIND)      TO AET-CINDCD-FONE(WIND).    
                                                                        
                                                                        
           IF   TAB-CDDD-CLI-RENEG(WIND)   EQUAL  SPACES OR '0000'      
                MOVE  '0000'               TO AET-CDDD-CLI-RENEG(WIND)  
                MOVE  'X'                  TO AET-CINDCD-FONE(WIND)     
           ELSE                                                         
                MOVE TAB-CDDD-CLI-RENEG(WIND)                           
                                           TO AET-CDDD-CLI-RENEG(WIND)  
           END-IF.                                                      
BRQ059     IF   TAB-CFONE-RENEG(WIND)      EQUAL SPACES OR '00000000000'
BRQ059          MOVE  '00000000000'        TO AET-CFONE-RENEG(WIND)     
                MOVE  'X'                  TO AET-CINDCD-FONE(WIND)     
           ELSE                                                         
                MOVE TAB-CFONE-RENEG(WIND) TO AET-CFONE-RENEG(WIND)     
           END-IF.                                                      
                                                                        
BRQ        IF TAB-CINDCD-FONE(WIND) EQUAL 'X'
BRQ             MOVE  'B'                  TO AET-CINDCD-TPO-FONE(WIND) 
BRQ             MOVE  'X'                  TO AET-CINDCD-FONE(WIND)     
BRQ        END-IF.                                                      
                                                                        
           ADD  1                          TO WIND.                     
                                                                        
      *----------------------------------------------------------------*
       34000-99-FIM.                   EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       34008-INICIALIZA-TABELA SECTION.                                 
      *----------------------------------------------------------------*
                                                                        
           ADD  1           TO   WIND.                                  
                                                                        
           MOVE 'B'         TO   W-AET-CINDCD-TPO-FONE (WIND).          
           MOVE 'X'         TO   W-AET-CINDCD-FONE (WIND).              
           MOVE ZEROS       TO   W-AET-CDDD-CLI-RENEG(WIND).            
           MOVE ZEROS       TO   W-AET-CFONE-RENEG(WIND).               
                                                                        
      *----------------------------------------------------------------*
       34008-99-FIM.                   EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       34010-MOVER-FONES-DIF-ZEROS SECTION.                             
      *----------------------------------------------------------------*
                                                                        
           IF  WFINAL         =  1  AND                                 
               WLOCLZD-SM   = 0                                         
               MOVE ZEROS            TO  W-AET-CDDD-CLI-RENEG(WFINAL)   
               MOVE ZEROS            TO  W-AET-CFONE-RENEG(WFINAL)      
               MOVE 'B'              TO  W-AET-CINDCD-TPO-FONE(WFINAL)  
               MOVE 'X'              TO  W-AET-CINDCD-FONE(WFINAL)      
               ADD   1               TO  WFINAL                         
           END-IF.                                                      
                                                                        
CRUZ       IF  WFINAL         =  2  AND                                 
CRUZ           WLOCLZD-AF   = 0                                         
CRUZ           MOVE ZEROS            TO  W-AET-CDDD-CLI-RENEG(WFINAL)   
CRUZ           MOVE ZEROS            TO  W-AET-CFONE-RENEG(WFINAL)      
CRUZ           MOVE 'B'              TO  W-AET-CINDCD-TPO-FONE(WFINAL)  
CRUZ           MOVE 'X'              TO  W-AET-CINDCD-FONE(WFINAL)      
CRUZ           ADD   1               TO  WFINAL                         
CRUZ       END-IF.                                                      
                                                                        
           IF  AET-CDDD-CLI-RENEG(WIND) = ZEROS  AND                    
               AET-CFONE-RENEG(WIND)    = ZEROS                         
               ADD  1                 TO WIND                           
           ELSE                                                         
               MOVE AET-CDDD-CLI-RENEG(WIND)                            
                                      TO W-AET-CDDD-CLI-RENEG(WFINAL)   
               MOVE AET-CFONE-RENEG(WIND)                               
                                       TO W-AET-CFONE-RENEG(WFINAL)     
               MOVE AET-CINDCD-TPO-FONE(WIND)                           
                                      TO W-AET-CINDCD-TPO-FONE(WFINAL)  
               MOVE AET-CINDCD-FONE(WIND)                               
                                      TO W-AET-CINDCD-FONE(WFINAL)      
               ADD  1                 TO WIND                           
               ADD  1                 TO WFINAL                         
           END-IF.                                                      
                                                                        
           IF  WFINAL GREATER 6    OR    WIND GREATER 6                 
              IF  WFINAL <  WIND                                        
                 COMPUTE WQTDE  =  WIND  -  WFINAL                      
                 PERFORM  WQTDE  TIMES                                  
                    MOVE ZEROS     TO  W-AET-CDDD-CLI-RENEG(WFINAL)     
                    MOVE ZEROS     TO  W-AET-CFONE-RENEG(WFINAL)        
                    MOVE 'B'       TO  W-AET-CINDCD-TPO-FONE(WFINAL)    
                    MOVE 'X'       TO  W-AET-CINDCD-FONE(WFINAL)        
                    ADD   1        TO  WFINAL                           
                 END-PERFORM                                            
              END-IF                                                    
PCS023        IF W-AET-CFONE-RENEG(2) EQUAL ZEROS                       
PCS023           MOVE W-AET-TAB-FONE (1) TO AET-TAB-FONE (1)            
PCS023           MOVE W-AET-TAB-FONE (3) TO AET-TAB-FONE (2)            
PCS023           MOVE W-AET-TAB-FONE (4) TO AET-TAB-FONE (4)            
PCS023           MOVE W-AET-TAB-FONE (5) TO AET-TAB-FONE (5)            
PCS023        ELSE                                                      
                 MOVE W-AET-TAB-FONE (1) TO AET-TAB-FONE (1)            
                 MOVE W-AET-TAB-FONE (2) TO AET-TAB-FONE (2)            
                 MOVE W-AET-TAB-FONE (3) TO AET-TAB-FONE (4)            
                 MOVE W-AET-TAB-FONE (4) TO AET-TAB-FONE (5)            
PCS023        END-IF                                                    
              MOVE ZEROS              TO AET-CDDD-CLI-RENEG (3)         
              MOVE ZEROS              TO AET-CFONE-RENEG (3)            
              MOVE 'B'                TO AET-CINDCD-TPO-FONE (3)        
              MOVE 'X'                TO AET-CINDCD-FONE (3)            
              MOVE ZEROS              TO AET-CDDD-CLI-RENEG (6)         
              MOVE ZEROS              TO AET-CFONE-RENEG (6)            
              MOVE 'B'                TO AET-CINDCD-TPO-FONE (6)        
              MOVE 'X'                TO AET-CINDCD-FONE (6)            
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       34010-99-FIM.                   EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       35000-EDIT-DDD                  SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           IF       WRK-DDD-DIG(WDDDT)       IS    NUMERIC              
                    MOVE WRK-DDD-DIG(WDDDT)  TO    WRK-DDDN-DIG(WDDDN)  
                    SUBTRACT 1             FROM    WDDDN.               
                                                                        
           SUBTRACT 1                      FROM    WDDDT.               
                                                                        
      *----------------------------------------------------------------*
       35000-99-FIM.                   EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       40000-FINALIZAR                 SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           CLOSE ARQLOGRA                                               
                 ARQTELEL                                               
                 ARQENTEL.                                              
                                                                        
           DISPLAY '***************** RDAB7130 ********************'    
           DISPLAY '** MATCH BASE ENDERECOS COM BASE TELEFONES   **'    
           DISPLAY '**          FIM DO PROCESSAMENTO             **'    
           DISPLAY '**  LIDOS BASE TELEFONES  = '  WAC-LID-FON  '  **'  
           DISPLAY '**  LIDOS BASE ENDERECOS  = '  WAC-LID-END  '  **'  
           DISPLAY '**  GRAVADOS CADASTROS    = '  WAC-GRV-CAD  '  **'  
           DISPLAY '***************** RDAB7130 ********************'.   
                                                                        
           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.                 
           PERFORM 11000-TESTAR-FILE-STATUS.                            
                                                                        
           GOBACK.                                                      
                                                                        
      *----------------------------------------------------------------*
       40000-99-FIM.                   EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       99999-ROTINA-ERRO               SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE 'APL'                  TO ERR-TIPO-ACESSO.              
           MOVE 'RDAB7130'             TO ERR-PGM.                      
                                                                        
           CALL 'POOL7100'             USING WRK-BATCH                  
                                             ERRO-AREA.                 
                                                                        
           GOBACK.                                                      
                                                                        
      *----------------------------------------------------------------*
       99999-99-FIM.                   EXIT.                            
      *----------------------------------------------------------------*

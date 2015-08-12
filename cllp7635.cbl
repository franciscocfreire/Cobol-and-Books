      ***************************************************************** 
       ID  DIVISION.                                                    
      ***************************************************************** 
       PROGRAM-ID.                 CLLP7635.                            
      ***************************************************************** 
      *                                                               * 
      *        PROGRAMA    -   CLLP7635                               * 
      *        PROGRAMADOR -   RONALD M.S.                            * 
      *        SUPERVISOR  -   TESTA                                  * 
      *        ANALISTA    -   LOURIVAL SANTI       GRP = 82          * 
      *        DATA        -   15/10/1997                             * 
      *                                                               * 
      *---------------------------------------------------------------* 
      *        OBJETIVOS  DO   PROGRAMA:                              * 
      *                                                               * 
      *        - EMITIR TRES TIPOS DE CARTA COBRANCA:                 * 
      *                                                               * 
      *        * CARTA PARA  5 DIAS DE ATRAZO.                        * 
      *        * CARTA PARA 20 DIAS DE ATRAZO.                        * 
      *        * CARTA PARA 40 DIAS DE ATRAZO.                        * 
      *                                                               * 
      *---------------------------------------------------------------* 
      *                   -   A R Q U I V O S   -                     * 
      *                                                               * 
      *        ENTRADA:                                               * 
      *          LADO1     -  CADASTRO DO LPCL - TIPOS 1,2 E 3.       * 
      *                       LISTA LADO1 DAS CARTAS.                 * 
      *                                                               * 
      *          LADO2     -  CADASTRO DO LPCL - TIPOS 1,2 E 3.       * 
      *                       LISTA LADO2 DAS CARTAS.                 * 
      *                                                               * 
      *          DATATECL  -  ARQUIVO DE DATAS.                       * 
      *                                                               * 
      *        SAIDA:                                                 * 
      *          LISTA     -  AVISOS DE COBRANCA.                     * 
      *          LISTA1    -  TOTAL PARA AS AGENCIAS.                 * 
      *                                                               * 
      *---------------------------------------------------------------* 
      *                     ULTIMA ALTERACAO                          * 
      *                                                               * 
      *        PROGRAMADOR -   TESTA                                  * 
      *        ANALISTA    -   TESTA                G&P               * 
      *        DATA        -   MARCO 2003                             * 
      *                                                               * 
      *        OBJETIVOS:                                             * 
      *        - IMCLUR COD. DE BARRAS CIF E POSTNET                  * 
      *---------------------------------------------------------------* 
      *                     ULTIMA ALTERACAO                          * 
      *                                                               * 
      *        PROGRAMADOR -   TESTA                                  * 
      *        ANALISTA    -   MAGDA                CPM               * 
      *        DATA        -   MAIO  2005                             * 
      *                                                               * 
      *        OBJETIVOS:                                             * 
      *        - ALTERACAO NO ENDERECO DO CLIENTE                     * 
      *---------------------------------------------------------------* 
      *                     ULTIMA ALTERACAO                          * 
      *---------------------------------------------------------------* 
      *                                                               * 
      *  PROGRAMADOR -   TESTA - CPM                                  * 
      *  DATA        -   ABRIL 2007                                   * 
      *  OBJETIVO    -   IMCLUIR CODIGO DE BARRA                      * 
      *                                                               * 
      *---------------------------------------------------------------* 
      ***************************************************************** 
      /                                                                 
      ***************************************************************** 
       ENVIRONMENT DIVISION.                                            
      ***************************************************************** 
       CONFIGURATION SECTION.                                           
                                                                        
       SPECIAL-NAMES.                                                   
           DECIMAL-POINT IS COMMA.                                      
                                                                        
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    
                                                                        
           SELECT  ARQPARM   ASSIGN  TO  UT-S-ARQPARM                   
                   FILE      STATUS  IS  WRK-FS-PARM.                   
                                                                        
           SELECT  LADO1     ASSIGN  TO  UT-S-LADO1                     
                   FILE      STATUS  IS  WRK-FS-LADO1.                  
                                                                        
           SELECT  LADO2     ASSIGN  TO  UT-S-LADO2                     
                   FILE      STATUS  IS  WRK-FS-LADO2.                  
                                                                        
           SELECT  DATATECL  ASSIGN  TO  UT-S-DATATECL                  
                   FILE      STATUS  IS  WRK-FS-DATATECL.               
                                                                        
           SELECT  LISTA     ASSIGN  TO  UT-S-LISTA                     
                   FILE      STATUS  IS  WRK-FS-LISTA.                  
                                                                        
           SELECT  LISTA1    ASSIGN  TO  UT-S-LISTA1                    
                   FILE      STATUS  IS  WRK-FS-LISTA1.                 
                                                                        
           SELECT  ARQECT    ASSIGN  TO  UT-S-ARQECT                    
                   FILE      STATUS IS WRK-FS-ARQECT.                   
                                                                        
      ***************************************************************** 
       DATA DIVISION.                                                   
      ***************************************************************** 
       FILE SECTION.                                                    
                                                                        
      *---------------------------------------------------------------* 
      * OUTPUT:       ARQUIVO COM DADOS PARA ENVIO AO CORREIO         * 
      *               ORG. SEQUENCIAL   -   LRECL = 25                * 
      *---------------------------------------------------------------* 
                                                                        
       FD  ARQECT                                                       
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
-INC I#ROTIZA                                                           
                                                                        
                                                                        
      *---------------------------------------------------------------* 
      *      CADASTRO DO LPCL - ARQ. DE ENTRADA - LRECL : 0562        * 
      *---------------------------------------------------------------* 
                                                                        
       FD  LADO1       BLOCK 0                                          
                       RECORDING F                                      
                       LABEL RECORD STANDARD.                           
                                                                        
       01  LD1-REGTO.                                                   
           03  FILLER          PIC X(562).                              
                                                                        
                                                                        
      *---------------------------------------------------------------* 
      *      CADASTRO DO LPCL - ARQ. DE ENTRADA - LRECL : 0562        * 
      *---------------------------------------------------------------* 
                                                                        
       FD  LADO2       BLOCK 0                                          
                       RECORDING F                                      
                       LABEL RECORD STANDARD.                           
                                                                        
       01  LD2-REGTO.                                                   
           03  FILLER          PIC X(562).                              
                                                                        
                                                                        
      *---------------------------------------------------------------* 
      *      ARQUIVO DE DATAS - ARQ. DE ENTRADA - LRECL : 0006        * 
      *---------------------------------------------------------------* 
                                                                        
       FD  DATATECL    BLOCK 0                                          
                       RECORDING F                                      
                       LABEL RECORD STANDARD.                           
                                                                        
       01  DAT-REGTO.                                                   
           03  DAT-DATA.                                                
               05  DAT-DIA     PIC 9(02).                               
               05  DAT-MES     PIC 9(02).                               
               05  DAT-ANO     PIC 9(04).                               
                                                                        
      /                                                                 
                                                                        
      *---------------------------------------------------------------* 
      *      ARQUIVO DE PARAMETROS              - LRECL : 0006        * 
      *---------------------------------------------------------------* 
                                                                        
       FD  ARQPARM     BLOCK 0                                          
                       RECORDING F                                      
                       LABEL RECORD STANDARD.                           
                                                                        
       01  REG-PARAMETRO.                                               
           03  PARAMETROS.                                              
               05  PARM-TAREFA     PIC X(08).                           
               05  PARM-PROGRAMA   PIC X(08).                           
               05  FILLER          PIC X(06).                           
               05  PARM-SEQUENCIA  PIC 9(01).                           
               05  FILLER          PIC X(01).                           
               05  PARM-NR-DIAS    PIC 9(02).                           
               05  PARM-VALOR      PIC 9(15).                           
               05  FILLER          PIC X(209).                          
                                                                        
      *---------------------------------------------------------------* 
      *      R E L A T O R I O  -  ARQ. DE SAIDA - LRECL : 0175       * 
      *---------------------------------------------------------------* 
                                                                        
       FD  LISTA       BLOCK 0                                          
                       RECORDING F                                      
                       LABEL RECORD STANDARD.                           
                                                                        
       01  LIS-REGTO.                                                   
           03  LIS-CARRO       PIC X(001).                              
           03  FILLER          PIC X(191).                              
                                                                        
                                                                        
      *---------------------------------------------------------------* 
      *      R E L A T O R I O  -  ARQ. DE SAIDA - LRECL : 0080       * 
      *---------------------------------------------------------------* 
                                                                        
       FD  LISTA1      BLOCK 0                                          
                       RECORDING F                                      
                       LABEL RECORD STANDARD.                           
                                                                        
       01  LT1-REGTO.                                                   
           03  LT1-CARRO       PIC X(01).                               
           03  FILLER          PIC X(79).                               
                                                                        
      ***************************************************************** 
       WORKING-STORAGE SECTION.                                         
      ***************************************************************** 
       77  FILLER              PIC X(41)         VALUE                  
           '*** INICIO DA WORKING-STORAGE SECTION ***'.                 
                                                                        
      *---------------------------------------------------------------* 
      *    AUXILIARES (FILE - STATUS)                                   
      *---------------------------------------------------------------* 
       77  WRK-FS-ARQECT       PIC X(02)         VALUE SPACES.          
       77  WRK-FS-PARM         PIC X(02)         VALUE SPACES.          
       77  WRK-FS-LADO1        PIC X(02)         VALUE SPACES.          
       77  WRK-FS-LADO2        PIC X(02)         VALUE SPACES.          
       77  WRK-FS-DATATECL     PIC X(02)         VALUE SPACES.          
       77  WRK-FS-LISTA        PIC X(02)         VALUE SPACES.          
       77  WRK-FS-LISTA1       PIC X(02)         VALUE SPACES.          
       77  WRK-FILE-STATUS     PIC X(02)         VALUE SPACES.          
       77  WRK-FUNCAO          PIC X(05)         VALUE SPACES.          
       77  WRK-NOME-ARQ        PIC X(08)         VALUE SPACES.          
       77  WRK-FAIXA01         PIC 9(03)         VALUE ZEROS.           
       77  WRK-FAIXA02         PIC 9(03)         VALUE ZEROS.           
       77  WRK-FAIXA03         PIC 9(03)         VALUE ZEROS.           
       77  WRK-FLAG-9002       PIC  9(01)        VALUE ZERO.            
       77  WRK-NUM-EXTRATO     PIC  9(11)        VALUE ZEROS.           
       77  WRK-LPCL5011        PIC  X(08)        VALUE 'LPCL5011'.      
                                                                        
      *---------------------------------------------------------------* 
      *    AUXILIAR   (ILBOABN0)                                        
      *---------------------------------------------------------------* 
       77  WRK-ABEND           PIC S9(04) COMP   VALUE +1111.           
                                                                        
      *---------------------------------------------------------------* 
      *    AUXILIARES (FIM - DE - ARQUIVO)                              
      *---------------------------------------------------------------* 
       77  WRK-FIM-PARM        PIC X(01)         VALUE SPACES.          
       77  WRK-FIM-LADO1       PIC X(01)         VALUE SPACES.          
       77  WRK-FIM-LADO2       PIC X(01)         VALUE SPACES.          
                                                                        
      *---------------------------------------------------------------* 
      *    TRATA CEP AGENCIA                                            
      *---------------------------------------------------------------* 
       01  WRK-TRATA-CEP.                                               
           03 WRK-TR-CEP       PIC 9(05)              VALUE ZEROS.      
           03 WRK-TR-CEP-SUF   PIC 9(03)              VALUE ZEROS.      
       01  WRK-TRATA-CEP-R REDEFINES WRK-TRATA-CEP PIC 9(08).           
                                                                        
      *---------------------------------------------------------------* 
      *                                                                 
      *---------------------------------------------------------------* 
       77  WRK-AREA-AUX        PIC X(34)         VALUE SPACES.          
                                                                        
       77  WRK-POOL0380                PIC  X(08) VALUE 'POOL0380'.     
       77  WRK-TAMANHO-380             PIC  9(02) COMP  VALUE 40.       
                                                                        
      *---------------------------------------------------------------* 
      *    AUXILIARES (INDEXADORES)                                     
      *---------------------------------------------------------------* 
       77  WRK-IND1            PIC 9(02)         VALUE ZEROS.           
       77  WRK-IND2            PIC 9(02)         VALUE ZEROS.           
       77  WRK-IND-AUX         PIC 9(02)         VALUE ZEROS.           
                                                                        
      *---------------------------------------------------------------* 
      *    ACUMULADORES                                                 
      *---------------------------------------------------------------* 
       77  WRK-VALOR-A         PIC 9(13)V99      VALUE ZEROS.           
       77  WRK-VALOR-B         PIC 9(13)V99      VALUE ZEROS.           
                                                                        
      *---------------------------------------------------------------* 
      *    DATA NO FORMATO SSAAMMDD                                     
      *---------------------------------------------------------------* 
                                                                        
       01  WRK-DT-SSAAMMDD.                                             
           03 WRK-ANO-SAMD     PIC 9(04).                               
           03 WRK-MES-SAMD     PIC 9(02).                               
           03 WRK-DIA-SAMD     PIC 9(02).                               
       01  WRK-DT-SSAAMMDD-R  REDEFINES  WRK-DT-SSAAMMDD PIC 9(08).     
                                                                        
       77  WRK-DIGITO          PIC X(01).                               
       77  WRK-TAMANHO         PIC 9(02)         VALUE ZEROS.           
                                                                        
       77  ACU-CARTAS          PIC 9(05)  COMP-3 VALUE ZEROS.           
       77  WRK-RESTART         PIC 9(05)         VALUE ZEROS.           
                                                                        
       77  WK-JOBNAME          PIC X(08).                               
       77  WK-VALORFAC         PIC 9(07) COMP-3.                        
                                                                        
       01  DATA-HORA.                                                   
           03  DT-JULIANA              PIC 9(05)  COMP-3.               
           03  DT-AAMMDD               PIC 9(07)  COMP-3.               
           03  DT-AAAAMMDD             PIC 9(09)  COMP-3.               
           03  TI-HHMMSS               PIC 9(07)  COMP-3.               
           03  TI-HHMMSSMMMMMM         PIC 9(13)  COMP-3.               
           03  TIMESTAMP               PIC X(20).                       
                                                                        
                                                                        
       01  WRK-DATA-SIST.                                               
           03  WRK-ANO-SIST    PIC 9(04).                               
           03  WRK-MES-SIST    PIC 9(02).                               
           03  WRK-DIA-SIST    PIC 9(02).                               
       01  WRK-DT-SIST-R   REDEFINES   WRK-DATA-SIST PIC 9(08).         
                                                                        
                                                                        
       01  WRK-DATA-DESC       PIC 9(08).                               
       01  WRK-DT-DESC   REDEFINES   WRK-DATA-DESC.                     
           03  WRK-DIA-DESC    PIC 9(02).                               
           03  WRK-MES-DESC    PIC 9(02).                               
           03  WRK-ANO-DESC    PIC 9(04).                               
                                                                        
       01  WRK-ANO-POST        PIC 9(04).                               
       01  FILLER        REDEFINES   WRK-ANO-POST.                      
           03  WRK-SS-POST     PIC 9(02).                               
           03  WRK-AA-POST     PIC 9(02).                               
                                                                        
       01  WRK-NRO-REG             PIC 9(10).                           
       01  WRK-NRO-REG-R   REDEFINES  WRK-NRO-REG.                      
           03  WRK-NRO-REG3        PIC 9(02).                           
           03  WRK-NRO-REG4        PIC 9(03).                           
           03  WRK-NRO-REG5        PIC 9(05).                           
                                                                        
                                                                        
       01  AUX-CONTROLE            PIC 9(03).                           
       01  AUX-CONTROLE-RD REDEFINES  AUX-CONTROLE.                     
           03  FILLER              PIC X(01).                           
           03  AUX-CTR             PIC 9(02).                           
                                                                        
      *---------------------------------------------------------------* 
      *   CAMPOS UTILIZADOS PARA EXECUCAO DO CODIGO DE BARRAS         * 
      *---------------------------------------------------------------* 
                                                                        
       01  WRK-ROTI9003                     PIC X(08) VALUE 'ROTI9003'. 
                                                                        
       01  WRK-LINKAGE.                                                 
            10  WRK-VERSAO-9003             PIC X(06).                  
            10  WRK-MENSAGEM-9003           PIC X(83).                  
            10  WRK-DATA-POSTAGEM-9003      PIC 9(08).                  
            10  WRK-NUMERO-LOTE-9003        PIC 9(05).                  
                                                                        
       01  WRK-AREA-BRAD9002.                                           
           03  WRK-VERSAO-9002             PIC X(06)  VALUE  'VRS001'.  
           03  WRK-MENSAGEM-9002           PIC X(83)  VALUE  SPACES.    
           03  WRK-DATA-POSTAGEM-9002      PIC 9(08).                   
           03  FILLER  REDEFINES  WRK-DATA-POSTAGEM-9002.               
               05  WRK-DIA-POSTAGEM-9002   PIC 9(02).                   
               05  WRK-MES-POSTAGEM-9002   PIC 9(02).                   
               05  WRK-SEC-POSTAGEM-9002   PIC 9(02).                   
               05  WRK-ANO-POSTAGEM-9002   PIC 9(02).                   
           03  WRK-NUMERO-LOTE-9002        PIC 9(05).                   
                                                                        
       01  WRK-AREA-BRAD9011.                                           
           03  WRK-VERSAO-9011             PIC X(06)  VALUE  'VRS001'.  
           03  WRK-MENSAGEM-9011           PIC X(83)  VALUE  SPACES.    
           03  WRK-CEP-POSTNET-9011        PIC 9(08).                   
           03  FILLER  REDEFINES  WRK-CEP-POSTNET-9011.                 
               05  WRK-NUMCEP-POSTNET-9011 PIC 9(05).                   
               05  WRK-CPLCEP-POSTNET-9011 PIC 9(03).                   
           03  WRK-RETORNO-POSTNET-9011    PIC X(11).                   
                                                                        
       01  WRK-AREA-BRAD9010.                                           
           03  WRK-VERSAO-9010             PIC X(06)  VALUE  'VRS001'.  
           03  WRK-MENSAGEM-9010           PIC X(83)  VALUE  SPACES.    
           03  WRK-LINHA-NUMERO-9010       PIC X(34).                   
           03  FILLER  REDEFINES  WRK-LINHA-NUMERO-9010.                
               05  WRK-CODIGO-DR-POSTAGEM-9010  PIC 9(02).              
               05  WRK-CODIGO-ADM-CONTR-9010    PIC 9(08).              
               05  WRK-NUMERO-LOTE-9010         PIC 9(05).              
               05  WRK-NUMERO-SEQ-OBJETO-9010   PIC 9(11).              
               05  WRK-CODIGO-DESTINO-9010      PIC 9(01).              
               05  WRK-CODIGO-RESERVA-9010      PIC 9(01).              
               05  WRK-DATA-POSTAGEM-9010       PIC 9(06).              
               05  FILLER  REDEFINES  WRK-DATA-POSTAGEM-9010.           
                   07  WRK-DIA-POSTAGEM-9010    PIC 9(02).              
                   07  WRK-MES-POSTAGEM-9010    PIC 9(02).              
                   07  WRK-ANO-POSTAGEM-9010    PIC 9(02).              
                                                                        
           03  WRK-LINHA-RETORNO-9010           PIC X(20).              
           03  FILLER  REDEFINES  WRK-LINHA-RETORNO-9010.               
               05  WRK-START-9010               PIC X(01).              
               05  WRK-NUMERO-RETORNO-9010      PIC X(18).              
               05  WRK-STOP-9010                PIC X(01).              
                                                                        
      *---------------------------------------------------------------* 
      *    C A R T O E S     -   (  D J D E )   -                       
      *---------------------------------------------------------------* 
       01  WRK-DJDE-INI.                                                
           03  FILLER             PIC X(186)     VALUE                  
           '1DJDE JDL=LWX5W3,JDE=LW7635,END;'.                          
           03  INI-FONTINDEX       PIC X(01)     VALUE '1'.             
           03  INI-RESTART         PIC 9(05)     VALUE 00001.           
                                                                        
       01  WRK-DJDE-INT.                                                
           03  WRK-FORMS           PIC X(186).                          
           03  INT-FONTINDEX       PIC X(01)     VALUE '1'.             
           03  INT-RESTART         PIC 9(05)     VALUE ZEROS.           
                                                                        
       01  WRK-DJDE-EXT.                                                
           03  FILLER              PIC X(186)    VALUE                  
           '1DJDE FORMS=CLLP10,END;'.                                   
           03  EXT-FONTINDEX       PIC X(01)     VALUE '1'.             
           03  EXT-RESTART         PIC 9(05)     VALUE ZEROS.           
                                                                        
       01  WRK-DJDE-EXT1.                                               
           03  FILLER              PIC X(186)    VALUE                  
           '1DJDE FORMS=CLLP09,END;'.                                   
           03  EXT1-FONTINDEX      PIC X(01)     VALUE '1'.             
           03  EXT1-RESTART        PIC 9(05)     VALUE ZEROS.           
                                                                        
       01  WRK-DJDE-FIM.                                                
           03  FILLER               PIC X(186)    VALUE                 
           '1DJDE JDL=DFAULT,JDE=F163S,END;'.                           
           03  FIM-FONTINDEX       PIC X(01)     VALUE '1'.             
           03  FIM-RESTART         PIC 9(05)     VALUE 99999.           
                                                                        
      *---------------------------------------------------------------* 
      *     AREA PARA CONTER O ARQ. LADO1 E LADO2 - LRECL : 0533      * 
      *---------------------------------------------------------------* 
                                                                        
       01  WRK-REGTO.                                                   
           02  WRK-CHAVE.                                               
               03  FILLER          PIC 9(09)     COMP-3.                
               03  FILLER          PIC 9(05)     COMP-3.                
               03  FILLER          PIC 9(03)     COMP-3.                
               03  FILLER          PIC 9(02)     COMP-3.                
               03  FILLER          PIC 9(09)     COMP-3.                
           02  WRK-EMPRESA         PIC 9(05)     COMP-3.                
           02  WRK-AGENCIA         PIC 9(05)     COMP-3.                
           02  WRK-CONTA           PIC 9(07)     COMP-3.                
           02  WRK-CART            PIC X(03).                           
           02  WRK-CONTRATO        PIC 9(07)     COMP-3.                
           02  FILLER              PIC X(07).                           
           02  WRK-NOME-RESP       PIC X(40).                           
           02  WRK-CGCPFRESP.                                           
               03  WRK-CGCPF       PIC 9(09)     COMP-3.                
               03  WRK-FILIAL      PIC 9(05)     COMP-3.                
               03  WRK-CTR         PIC 9(03)     COMP-3.                
           02  WRK-END-RESP        PIC X(40).                           
           02  WRK-DEV-NRO         PIC X(07).                           
           02  WRK-DEV-COMPL       PIC X(20).                           
           02  WRK-DEV-BAIRRO      PIC X(20).                           
           02  WRK-CIDADE          PIC X(30).                           
           02  WRK-EST             PIC X(02).                           
           02  WRK-CEP             PIC 9(05).                           
           02  WRK-CEP-SUFIXO      PIC 9(03).                           
           02  WRK-IDENT           PIC 9(02).                           
           02  WRK-OCORRENCIAS OCCURS 7 TIMES.                          
               03  WRK-NATUREZA    PIC X(02).                           
               03  WRK-VCMTO       PIC 9(09)     COMP-3.                
               03  WRK-VALOR       PIC 9(11)V99  COMP-3.                
               03  WRK-DESCRIC     PIC X(20).                           
           02  WRK-TIPO            PIC 9(01).                           
           02  WRK-COD-ENT         PIC 9(01).                           
           02  WRK-NOME-AGE        PIC X(20).                           
           02  WRK-END-AGE         PIC X(25).                           
           02  WRK-CEP-AG          PIC 9(09)     COMP-3.                
           02  WRK-EST-AG          PIC X(02).                           
           02  WRK-MUNICIPIO       PIC X(14).                           
           02  WRK-CD              PIC 9(05)     COMP-3.                
           02  WRK-COD-BARRA       PIC X(29).                           
           02  WRK-NRO-SEQ         PIC 9(07)     COMP-3.                
                                                                        
                                                                        
       01  FILLER                  PIC X(38)     VALUE                  
           '*** FIM DA WORKING-STORAGE SECTION ***'.                    
                                                                        
      *---------------------------------------------------------------* 
      *    C A R T A  (LADO1 E LADO2 - PARTE EXTERNA)                 * 
      *---------------------------------------------------------------* 
       01  CABECDD.                                                     
           03  FILLER                PIC X(02)     VALUE '3'.           
           03  CBCD-LADO1.                                              
               05  FILLER            PIC X(38)     VALUE SPACES.        
               05  CBCD-FIXO1.                                          
                   07  CBCD1-BRANCO1 PIC X(19).                         
                   07  CBCD1-CD      PIC X(03).                         
                   07  CBCD1-NRO-CD  PIC 9(05).                         
           03  FILLER                PIC X(33)   VALUE SPACES.          
           03  CBCD-LADO2.                                              
               05  FILLER            PIC X(38)   VALUE SPACES.          
               05  CBCD-FIXO2.                                          
                   07  CBCD2-BRANCO1 PIC X(19).                         
                   07  CBCD2-CD      PIC X(03).                         
                   07  CBCD2-NRO-CD  PIC 9(05).                         
           03  FILLER                PIC X(21)   VALUE SPACES.          
           03  CBCD-FONTINDEX        PIC X(01)   VALUE '1'.             
           03  CBCD-RESTART          PIC 9(05)   VALUE ZEROS.           
                                                                        
      /                                                                 
       01  CABEC1.                                                      
           03  FILLER                PIC X(02)     VALUE '5'.           
           03  CB1A-LADO1.                                              
               05  CB1A-END-AG       PIC X(26).                         
               05  FILLER            PIC X(12)     VALUE SPACES.        
               05  CB1A-FIXO1.                                          
                   07  CB1A-ECT      PIC X(27).                         
               05  CB1A-FIXO2   REDEFINES   CB1A-FIXO1.                 
                   07  CB1A-BRANCO1  PIC X(19).                         
                   07  CB1A-CD       PIC X(03).                         
                   07  CB1A-NRO-CD   PIC 9(05).                         
               05  CB1A-FIXO3  REDEFINES    CB1A-FIXO2.                 
                   07  CB1A-BRANCO2  PIC X(27).                         
               05  CB1A-FIXO4  REDEFINES    CB1A-FIXO3.                 
                   07  CB1A-BRANCO3  PIC X(04).                         
                   07  CB1A-FRANQUIA PIC X(23).                         
               05  CB1A-FIXO5  REDEFINES    CB1A-FIXO4.                 
                   07  CB1A-CEP-IR   PIC X(27).                         
           03  FILLER                PIC X(33)   VALUE SPACES.          
           03  CB1B-LADO2.                                              
               05  CB1B-END-AG       PIC X(26).                         
               05  FILLER            PIC X(12)   VALUE SPACES.          
               05  CB1B-FIXO1.                                          
                   07  CB1B-ECT      PIC X(27).                         
               05  CB1B-FIXO2  REDEFINES  CB1B-FIXO1.                   
                   07  CB1B-BRANCO1  PIC X(19).                         
                   07  CB1B-CD       PIC X(03).                         
                   07  CB1B-NRO-CD   PIC 9(05).                         
               05  CB1B-FIXO3  REDEFINES  CB1B-FIXO2.                   
                   07  CB1B-BRANCO2  PIC X(27).                         
               05  CB1B-FIXO4  REDEFINES  CB1B-FIXO3.                   
                   07  CB1B-BRANCO3  PIC X(04).                         
                   07  CB1B-FRANQUIA PIC X(23).                         
               05  CB1B-FIXO5  REDEFINES  CB1B-FIXO4.                   
                   07  CB1B-CEP-IR   PIC X(27).                         
           03  FILLER                PIC X(21)   VALUE SPACES.          
           03  CB1B-FONTINDEX        PIC X(01)   VALUE '1'.             
           03  CB1B-RESTART          PIC 9(05)   VALUE ZEROS.           
                                                                        
      /                                                                 
       01  CABEC2.                                                      
           03  CB2-CARRO             PIC X(02)   VALUE ' '.             
           03  CB2A-LADO1.                                              
               05  CB2A-CEP          PIC 9(05).                         
               05  CB2A-HIFEN        PIC X(01)   VALUE SPACES.          
               05  CB2A-AG-SUFIXO    PIC 9(03).                         
               05  FILLER            PIC X(01)   VALUE SPACES.          
               05  CB2A-MUNICIPIO    PIC X(13).                         
               05  FILLER            PIC X(01)   VALUE SPACES.          
               05  CB2A-SIGLA        PIC X(02).                         
               05  FILLER            PIC X(12)   VALUE SPACES.          
               05  CB2A-CEP-IR       PIC X(27).                         
           03  FILLER                PIC X(33)   VALUE SPACES.          
           03  CB2B-LADO2.                                              
               05  CB2B-CEP          PIC 9(05).                         
               05  CB2B-HIFEN        PIC X(01)   VALUE SPACES.          
               05  CB2B-AG-SUFIXO    PIC 9(03).                         
               05  FILLER            PIC X(01)   VALUE SPACES.          
               05  CB2B-MUNICIPIO    PIC X(13).                         
               05  FILLER            PIC X(01)   VALUE SPACES.          
               05  CB2B-SIGLA        PIC X(02).                         
               05  FILLER            PIC X(12)   VALUE SPACES.          
               05  CB2B-CEP-IR       PIC X(27)   VALUE SPACES.          
           03  FILLER                PIC X(21)   VALUE SPACES.          
           03  CB2B-FONTINDEX        PIC X(01)   VALUE '1'.             
           03  CB2B-RESTART          PIC 9(05)   VALUE ZEROS.           
                                                                        
       01  CABEC3.                                                      
           03  CB3-CARRO             PIC X(02)   VALUE '0'.             
           03  CB3A-LADO1.                                              
               05  CB3A-AGEN         PIC 9(05).                         
               05  CB3A-HIFEN        PIC X(01).                         
               05  CB3A-DIG-AG       PIC X(01).                         
               05  CB3A-BARRA        PIC X(01).                         
               05  CB3A-NOME-AG      PIC X(20).                         
               05  FILLER            PIC X(13)   VALUE SPACES.          
           03  CB3A-ROTINA           PIC X(24)   VALUE SPACES.          
           03  CB3A-NRO-SEQ          PIC 9(07).                         
           03  FILLER                PIC X(26)   VALUE SPACES.          
           03  CB3B-LADO2.                                              
               05  CB3B-AGEN         PIC 9(05).                         
               05  CB3B-HIFEN        PIC X(01).                         
               05  CB3B-DIG-AG       PIC X(01).                         
               05  CB3B-BARRA        PIC X(01).                         
               05  CB3B-NOME-AG      PIC X(20).                         
               05  FILLER            PIC X(13)   VALUE SPACES.          
           03  CB3B-ROTINA           PIC X(24)   VALUE SPACES.          
           03  CB3B-NRO-SEQ          PIC 9(07).                         
           03  FILLER                PIC X(14)   VALUE SPACES.          
           03  CB3B-FONTINDEX        PIC X(01)   VALUE '1'.             
           03  CB3B-RESTART          PIC 9(05)   VALUE ZEROS.           
                                                                        
       01  LINHA-POSTNET1.                                              
           05  LPOS1-CARRO             PIC  X(01)  VALUE '6'.           
           05 POSTESQ1.                                                 
              07  LPOS1-FILLER         PIC  X(58)  VALUE  SPACES.       
              07  LPOS1-FILLER         PIC  X(74)  VALUE  SPACES.       
           05  LPOS1-FIILER            PIC  X(53) VALUE  SPACES.        
           05  LPOS1-FONTINDEX         PIC  X(01)  VALUE '6'.           
           05  LPOS1-SEQ-RESTART       PIC  9(05)  VALUE ZEROS.         
                                                                        
       01  LINHA-POSTNET2.                                              
           03  LPOS2-CARRO             PIC  X(01)  VALUE '0'.           
           03 POSTESQ2.                                                 
              05  LPOS2-FILLER         PIC  X(15)  VALUE  SPACES.       
              05  LPOS2-AREA-POSTNET-E PIC  X(11)  VALUE  SPACES.       
              05  LPOS2-FILLER         PIC  X(69)  VALUE  SPACES.       
           03 POSTDIR2.                                                 
              05  LPOS2-AREA-POSTNET-D PIC  X(11)  VALUE  SPACES.       
           03  LPOS2-FILLER            PIC  X(79)  VALUE  SPACES.       
           03  LPOS2-FONTINDEX         PIC  X(01)  VALUE '3'.           
           03  LPOS2-SEQ-RESTART       PIC  9(05)  VALUE ZEROS.         
                                                                        
       01  LINHA-CIF1.                                                  
           03  LCIF1-CARRO             PIC  X(01)  VALUE ' '.           
           03 CIFESQ1.                                                  
              05  LCIF1-FILLER         PIC  X(07)  VALUE  ALL '{'.      
              05  LCIF1-AREA-E         PIC  X(20).                      
           03 CIFDIR1.                                                  
              05  LCIF1-FILLER         PIC  X(31)  VALUE  ALL '{'.      
              05  LCIF1-AREA-D         PIC  X(20).                      
           03  LCIF1-FILLER            PIC  X(107) VALUE  SPACES.       
           03  LCIF1-FONTINDEX         PIC  X(01)  VALUE '4'.           
           03  LCIF1-SEQ-RESTART       PIC  9(05)  VALUE ZEROS.         
                                                                        
       01  LINHA-CIF2.                                                  
           03  LCIF2-CARRO             PIC  X(01)  VALUE '-'.           
           03 CIFESQ2.                                                  
              05  LCIF2-FILLER         PIC  X(11)  VALUE  SPACES.       
              05  LCIF2-AREA-E         PIC  X(34).                      
              05  LCIF2-FILLER         PIC  X(51) VALUE  SPACES.        
           03 CIFDIR2.                                                  
              05  LCIF2-AREA-D         PIC  X(34).                      
              05  LCIF2-FILLER         PIC  X(55) VALUE  SPACES.        
           03  LCIF2-FONTINDEX         PIC  X(01)  VALUE '5'.           
           03  LCIF2-SEQ-RESTART       PIC  9(05)  VALUE ZEROS.         
                                                                        
       01  CABEC4.                                                      
           03  CB4-CARRO           PIC X(01)     VALUE '0'.             
           03  CB4A-LADO1.                                              
               05  FILLER          PIC X(11)     VALUE SPACES.          
               05  CB4A-NOME-RES   PIC X(40).                           
           03  FILLER              PIC X(45)     VALUE SPACES.          
           03  CB4B-LADO2.                                              
               05  CB4B-NOME-RES   PIC X(40).                           
           03  FILLER              PIC X(49)     VALUE SPACES.          
           03  CB4B-FONTINDEX      PIC X(01)     VALUE '5'.             
           03  CB4B-RESTART        PIC 9(05)     VALUE ZEROS.           
                                                                        
       01  CABEC5.                                                      
           03  CB5-CARRO           PIC X(01)     VALUE ' '.             
           03  CB5A-LADO1.                                              
               05  FILLER          PIC X(11)     VALUE SPACES.          
               05  CB5A-END-RES    PIC X(49).                           
               05  FILLER          PIC X(23)     VALUE SPACES.          
           03  FILLER              PIC X(13)     VALUE SPACES.          
           03  CB5B-LADO2.                                              
               05  CB5B-END-RES    PIC X(49).                           
               05  FILLER          PIC X(23)     VALUE SPACES.          
           03  FILLER              PIC X(17)     VALUE SPACES.          
           03  CB5B-FONTINDEX      PIC X(01)     VALUE '5'.             
           03  CB5B-RESTART        PIC 9(05)     VALUE ZEROS.           
                                                                        
       01  CABEC5A.                                                     
           03  CB5A-CARRO           PIC X(01)     VALUE ' '.            
           03  CB5AA-LADO1.                                             
               05  FILLER           PIC X(83)     VALUE SPACES.         
           03  FILLER               PIC X(13)     VALUE SPACES.         
           03  CB5BB-LADO2.                                             
               05  FILLER           PIC X(72)     VALUE SPACES.         
           03  FILLER               PIC X(17)     VALUE SPACES.         
           03  CB5BB-FONTINDEX      PIC X(01)     VALUE '5'.            
           03  CB5BB-RESTART        PIC 9(05)     VALUE ZEROS.          
                                                                        
       01  WRKAA-LADO1.                                                 
           03  FILLER          PIC X(11)     VALUE SPACES.              
           03  WRKA-BAI-RES    PIC X(20).                               
           03  FILLER          PIC X(01)     VALUE SPACES.              
           03  WRKA-COM-RES    PIC X(20).                               
           03  FILLER          PIC X(01)     VALUE SPACES.              
           03  FILLER          PIC X(19).                               
           03  FILLER          PIC X(11)     VALUE SPACES.              
                                                                        
       01  WRKBB-LADO2.                                                 
           03  WRKB-BAI-RES    PIC X(20).                               
           03  FILLER          PIC X(01)     VALUE SPACES.              
           03  WRKB-COM-RES    PIC X(20).                               
           03  FILLER          PIC X(01)     VALUE SPACES.              
           03  FILLER          PIC X(19).                               
           03  FILLER          PIC X(11)     VALUE SPACES.              
      /                                                                 
       01  CABEC6.                                                      
           03  CB6-CARRO           PIC X(01)     VALUE ' '.             
           03  CB6A-LADO1.                                              
               05  FILLER          PIC X(54)     VALUE SPACES.          
           03  FILLER              PIC X(42)     VALUE SPACES.          
           03  CB6B-LADO2.                                              
               05  FILLER          PIC X(43)     VALUE SPACES.          
           03  FILLER              PIC X(46)     VALUE SPACES.          
           03  CB6B-FONTINDEX      PIC X(01)     VALUE '5'.             
           03  CB6B-RESTART        PIC 9(05)     VALUE ZEROS.           
                                                                        
       01  WRKA-LADO1.                                                  
           03  FILLER          PIC X(11)     VALUE SPACES.              
           03  WRKA-CEP        PIC 9(05).                               
           03  WRKA-HIFEN      PIC X(01)     VALUE SPACES.              
           03  WRKA-CEP-SUFIXO PIC 9(03).                               
           03  FILLER          PIC X(01)     VALUE SPACES.              
           03  WRKA-CIDADE     PIC X(33).                               
                                                                        
       01  WRKB-LADO2.                                                  
           03  WRKB-CEP        PIC 9(05).                               
           03  WRKB-HIFEN      PIC X(01)     VALUE SPACES.              
           03  WRKB-CEP-SUFIXO PIC 9(03).                               
           03  FILLER          PIC X(01)     VALUE SPACES.              
           03  WRKB-CIDADE     PIC X(33).                               
      /                                                                 
       01  CABEC7.                                                      
           03  CB7-CARRO           PIC X(01)     VALUE ' '.             
           03  CB7A-LADO1.                                              
               05  FILLER          PIC X(50)     VALUE SPACES.          
               05  CB7A-NRO-SEQ    PIC 9(07).                           
           03  FILLER              PIC X(12)     VALUE SPACES.          
           03  CB7-AVISO-LADO1     PIC X(06)     VALUE 'CLLP'.          
           03  FILLER              PIC X(27)     VALUE SPACES.          
           03  CB7B-LADO2.                                              
               05  FILLER          PIC X(47)     VALUE SPACES.          
               05  CB7B-NRO-SEQ    PIC 9(07).                           
           03  FILLER              PIC X(12)     VALUE SPACES.          
           03  CB7-AVISO-LADO2     PIC X(06)     VALUE 'CLLP'.          
           03  FILLER              PIC X(11)     VALUE SPACES.          
           03  CB7B-FONTINDEX      PIC X(01)     VALUE '1'.             
           03  CB7B-RESTART        PIC 9(05)     VALUE ZEROS.           
                                                                        
       01  CABEC8.                                                      
           03  CB8-CARRO           PIC X(01)     VALUE '7'.             
           03  FILLER              PIC X(18)     VALUE SPACES.          
           03  CB8-LADO1.                                               
               05  CB8A-LINHA-DIG  PIC X(29).                           
           03  FILLER              PIC X(55)     VALUE SPACES.          
           03  CB8-LADO2.                                               
               05  CB8B-LINHA-DIG  PIC X(29).                           
           03  FILLER              PIC X(13)     VALUE SPACES.          
           03  FILLER              PIC X(41)     VALUE SPACES.          
           03  CB8-FONTINDEX       PIC X(01)     VALUE '7'.             
           03  CB8-RESTART         PIC 9(05)     VALUE ZEROS.           
                                                                        
       01  CABEC9.                                                      
           03  CB9-CARRO           PIC X(01)     VALUE '7'.             
           03  FILLER              PIC X(07)     VALUE ALL '@'.         
           03  FILLER              PIC X(01)     VALUE '*'.             
           03  CB9-LADO1.                                               
               05  CB9A-COD-BARRA  PIC X(29).                           
           03  FILLER              PIC X(01)     VALUE '*'.             
           03  FILLER              PIC X(27)     VALUE ALL '@'.         
           03  FILLER              PIC X(01)     VALUE '*'.             
           03  CB9-LADO2.                                               
               05  CB9B-COD-BARRA  PIC X(29).                           
           03  FILLER              PIC X(01)     VALUE '*'.             
           03  FILLER              PIC X(89)     VALUE SPACES.          
           03  CB9-FONTINDEX       PIC X(01)     VALUE '8'.             
           03  CB9-RESTART         PIC 9(05)     VALUE ZEROS.           
                                                                        
                                                                        
      *---------------------------------------------------------------* 
      *    C A R T A  (LADOA E LADOB - PARTE INTERNA)                 * 
      *---------------------------------------------------------------* 
       01  LINHA1.                                                      
           03 LD1-CARRO            PIC X(01)     VALUE '2'.             
           03 LD1-LADOA.                                                
              05 FILLER            PIC X(02)     VALUE SPACES.          
              05 LD1A-NOME-RES     PIC X(40).                           
           03 FILLER               PIC X(59)     VALUE SPACES.          
           03 LD1-LADOB.                                                
              05 LD1B-NOME-RES     PIC X(40).                           
           03 FILLER               PIC X(44)     VALUE SPACES.          
           03 LD1-FONTINDEX        PIC X(01)     VALUE '1'.             
           03 LD1-RESTART          PIC 9(05)     VALUE ZEROS.           
                                                                        
       01  LINHA2.                                                      
           03 LD2-CARRO            PIC X(01)     VALUE '-'.             
           03 LD2-LADOA.                                                
              05 FILLER            PIC X(12)     VALUE SPACES.          
              05 LD2A-AGENCIA      PIC 9(05).                           
              05 LD2A-HIFEN        PIC X(03).                           
              05 LD2A-NOME-AGE     PIC X(20).                           
           03 FILLER               PIC X(71)     VALUE SPACES.          
           03 LD2-LADOB.                                                
              05 LD2B-AGENCIA      PIC 9(05).                           
              05 LD2B-HIFEN        PIC X(03).                           
              05 LD2B-NOME-AGE     PIC X(20).                           
           03 FILLER               PIC X(46)     VALUE SPACES.          
           03 LD2-FONTINDEX        PIC X(01)     VALUE '1'.             
           03 LD2-RESTART          PIC 9(05)     VALUE ZEROS.           
                                                                        
       01  LINHA3.                                                      
           03 LD3-CARRO            PIC X(01)     VALUE '0'.             
           03 LD3-LADOA.                                                
              05 FILLER            PIC X(12)     VALUE SPACES.          
              05 LD3A-CONTA        PIC ZZZ.ZZZ.                         
              05 LD3A-HIFEN        PIC X(01).                           
              05 LD3A-DIG-CTA      PIC X(01).                           
              05 FILLER            PIC X(28)     VALUE SPACES.          
              05 LD3A-DT-AVISO.                                         
                 07 LD3A-DIA       PIC 9(02).                           
                 07 LD3A-BARRA1    PIC X(01).                           
                 07 LD3A-MES       PIC 9(02).                           
                 07 LD3A-BARRA2    PIC X(01).                           
                 07 LD3A-ANO       PIC 9(04).                           
           03 FILLER               PIC X(52)     VALUE SPACES.          
           03 LD3-LADOB.                                                
              05 LD3B-CONTA        PIC ZZZ.ZZZ.                         
              05 LD3B-HIFEN        PIC X(01).                           
              05 LD3B-DIG-CTA      PIC X(01).                           
              05 FILLER            PIC X(28)     VALUE SPACES.          
              05 LD3B-DT-AVISO.                                         
                 07 LD3B-DIA       PIC 9(02).                           
                 07 LD3B-BARRA1    PIC X(01).                           
                 07 LD3B-MES       PIC 9(02).                           
                 07 LD3B-BARRA2    PIC X(01).                           
                 07 LD3B-ANO       PIC 9(04).                           
           03 FILLER               PIC X(27)     VALUE SPACES.          
           03 LD3-FONTINDEX        PIC X(01)     VALUE '1'.             
           03 LD3-RESTART          PIC 9(05)     VALUE ZEROS.           
                                                                        
       01  LINHA4.                                                      
           03 LD4-LINHA OCCURS 7 TIMES.                                 
              05 LD4-CARRO         PIC X(01).                           
              05 LD4-LADOA.                                             
                 07 FILLER         PIC X(02).                           
                 07 LD4A-COD-NAT   PIC X(02).                           
                 07 LD4A-HIFEN     PIC X(03).                           
                 07 LD4A-DESCRIC   PIC X(20).                           
                 07 FILLER         PIC X(74).                           
              05 LD4-LADOB.                                             
                 07 LD4B-COD-NAT   PIC X(02).                           
                 07 LD4B-HIFEN     PIC X(03).                           
                 07 LD4B-DESCRIC   PIC X(20).                           
                 07 FILLER         PIC X(59).                           
              05 LD4-FONTINDEX     PIC X(01).                           
              05 LD4-RESTART       PIC 9(05).                           
                                                                        
                                                                        
       01  LINHA5.                                                      
           03 LD5-LINHA OCCURS 7 TIMES.                                 
              05 LD5-CARRO          PIC X(01).                          
              05 LD5-LADOA1.                                            
                 07 FILLER          PIC X(02).                          
                 07 LD5A-LIT1       PIC X(19).                          
                 07 LD5A-DATA1.                                         
                    10 LD5A-DIA     PIC 9(02).                          
                    10 LD5A-BAR1    PIC X(01).                          
                    10 LD5A-MES     PIC 9(02).                          
                    10 LD5A-BAR2    PIC X(01).                          
                    10 LD5A-ANO     PIC 9(04).                          
                 07 FILLER          PIC X(01).                          
                 07 LD5A-LIT2       PIC X(26).                          
                 07 LD5A-VALOR      PIC ZZZ.ZZZ.ZZ9,99.                 
                 07 FILLER          PIC X(29).                          
              05 LD5-LADOB.                                             
                 07 LD5B-LIT1       PIC X(19).                          
                 07 LD5B-DATA1.                                         
                    10 LD5B-DIA     PIC 9(02).                          
                    10 LD5B-BAR1    PIC X(01).                          
                    10 LD5B-MES     PIC 9(02).                          
                    10 LD5B-BAR2    PIC X(01).                          
                    10 LD5B-ANO     PIC 9(04).                          
                 07 FILLER          PIC X(01).                          
                 07 LD5B-LIT2       PIC X(26).                          
                 07 LD5B-VALOR      PIC ZZZ.ZZZ.ZZ9,99.                 
                 07 FILLER          PIC X(14).                          
              05 LD5-FONTINDEX      PIC X(01).                          
              05 LD5-RESTART        PIC 9(05).                          
                                                                        
       01  LINHA7.                                                      
           03 LD7-CARRO            PIC X(01)     VALUE '-'.             
           03 LD7-LADOA.                                                
              05 FILLER            PIC X(16)     VALUE SPACES.          
              05 LD7A-TOT-LIT      PIC X(40).                           
              05 LD7A-TOT-DIVIDA PIC Z.ZZZ.ZZZ.ZZ9,99 BLANK WHEN ZEROS. 
           03 FILLER               PIC X(43)     VALUE SPACES.          
           03 LD7-LADOB.                                                
              05 LD7B-TOT-LIT      PIC X(40).                           
              05 LD7B-TOT-DIVIDA PIC Z.ZZZ.ZZZ.ZZ9,99 BLANK WHEN ZEROS. 
           03 FILLER               PIC X(14)     VALUE SPACES.          
           03 LD7-FONTINDEX        PIC X(01)     VALUE '1'.             
           03 LD7-RESTART          PIC 9(05)     VALUE ZEROS.           
                                                                        
       01  LINHA8.                                                      
           03 LD8-CARRO            PIC X(01)     VALUE '-'.             
           03 LD8-LADOA.                                                
              05 FILLER            PIC X(02)     VALUE SPACES.          
              05 LD8A-NRO-SEQ      PIC 9(07)     VALUE ZEROS.           
              05 FILLER            PIC X(92)     VALUE SPACES.          
           03 LD8-LADOB.                                                
              05 LD8B-NRO-SEQ      PIC 9(07)     VALUE ZEROS.           
              05 FILLER            PIC X(77)     VALUE SPACES.          
           03 LD8-FONTINDEX        PIC X(01)     VALUE '1'.             
           03 LD8-RESTART          PIC 9(05)     VALUE ZEROS.           
                                                                        
       01  LINBRA1.                                                     
           03  LDBRC-CARRO         PIC X(01)     VALUE '1'.             
           03  FILLER              PIC X(185)    VALUE SPACES.          
           03  LIN1-FONTINDEX      PIC X(01)     VALUE '1'.             
           03  LIN1-RESTART        PIC 9(05)     VALUE ZEROS.           
                                                                        
       01  LINBRA2.                                                     
           03  LDBR2-CARRO         PIC X(01)     VALUE ' '.             
           03  FILLER              PIC X(185)    VALUE SPACES.          
           03  LIN2-FONTINDEX      PIC X(01)     VALUE '1'.             
           03  LIN2-RESTART        PIC 9(05)     VALUE ZEROS.           
                                                                        
      *---------------------------------------------------------------* 
      *       R E L A T O R I O   -   LISTA1   -   LRECL: 0080        * 
      *---------------------------------------------------------------* 
       01  CABEC1-L.                                                    
           03  FILLER              PIC X(01)     VALUE '1'.             
SID        03  FILLER              PIC X(14)     VALUE SPACES.          
SID        03  CB1-NOME-BCO        PIC X(20)     VALUE SPACES.          
SID        03  FILLER              PIC X(24)     VALUE SPACES.          
           03  FILLER              PIC X(08)     VALUE 'LPCL3635'.      
                                                                        
       01  LINTOT1.                                                     
           03  FILLER              PIC X(01)     VALUE '0'.             
           03  FILLER              PIC X(58)     VALUE                  
           'TOTAIS DA MONTAGEM DE CARTAS DE COBRANCA VIA AGENCIA .....'.
           03  LT1-CARTAS          PIC ZZZ99.                           
                                                                        
SID   *---------------------------------------------------------------* 
SID    LINKAGE                        SECTION.                          
SID   *---------------------------------------------------------------* 
SID                                                                     
SID    01  LNK-CAMPOS-PARM.                                             
SID        03 LNK-TAMANHO-PARM              PIC S9(04)  COMP.           
SID        03 LNK-CCUSTO-PARM               PIC X(04).                  
                                                                        
      /                                                                 
      ***************************************************************** 
SID    PROCEDURE DIVISION        USING       LNK-CAMPOS-PARM.           
      ***************************************************************** 
       0000-INICIO                SECTION.                              
                                                                        
           OPEN  INPUT  ARQPARM  LADO1  LADO2  DATATECL                 
                OUTPUT  LISTA  LISTA1  ARQECT.                          
                                                                        
           MOVE 'OPEN'              TO        WRK-FUNCAO.               
                                                                        
           MOVE WRK-FS-PARM         TO        WRK-FILE-STATUS.          
           MOVE 'ARQPARM'           TO        WRK-NOME-ARQ.             
           PERFORM 0000-TESTA-FILE-STATUS.                              
                                                                        
           MOVE WRK-FS-LADO1        TO        WRK-FILE-STATUS.          
           MOVE 'LADO1'             TO        WRK-NOME-ARQ.             
           PERFORM 0000-TESTA-FILE-STATUS.                              
                                                                        
           MOVE WRK-FS-LADO2        TO        WRK-FILE-STATUS.          
           MOVE 'LADO2'             TO        WRK-NOME-ARQ.             
      *    PERFORM 0000-TESTA-FILE-STATUS.                              
                                                                        
           MOVE WRK-FS-DATATECL     TO        WRK-FILE-STATUS.          
           MOVE 'DATATECL'          TO        WRK-NOME-ARQ.             
           PERFORM 0000-TESTA-FILE-STATUS.                              
                                                                        
           MOVE WRK-FS-LISTA        TO        WRK-FILE-STATUS.          
           MOVE 'LISTA'             TO        WRK-NOME-ARQ.             
           PERFORM 0000-TESTA-FILE-STATUS.                              
                                                                        
           MOVE WRK-FS-LISTA1       TO        WRK-FILE-STATUS.          
           MOVE 'LISTA1'            TO        WRK-NOME-ARQ.             
           PERFORM 0000-TESTA-FILE-STATUS.                              
                                                                        
           MOVE WRK-FS-ARQECT       TO        WRK-FILE-STATUS.          
           MOVE 'ARQECT'            TO        WRK-NOME-ARQ.             
           PERFORM 0000-TESTA-FILE-STATUS.                              
                                                                        
           WRITE LIS-REGTO          FROM      WRK-DJDE-INI.             
                                                                        
           MOVE 'WRITE'             TO        WRK-FUNCAO.               
           MOVE WRK-FS-LISTA        TO        WRK-FILE-STATUS.          
           MOVE 'LISTA'             TO        WRK-NOME-ARQ.             
           PERFORM 0000-TESTA-FILE-STATUS.                              
                                                                        
           CALL  'POOL7600'  USING  DATA-HORA.                          
           MOVE  DT-AAAAMMDD  TO  WRK-DT-SIST-R.                        
                                                                        
           PERFORM 0000-LER-PARM.                                       
                                                                        
           READ    DATATECL.                                            
                                                                        
           MOVE 'READ'              TO        WRK-FUNCAO.               
           MOVE WRK-FS-DATATECL     TO        WRK-FILE-STATUS.          
           MOVE 'DATATECL'          TO        WRK-NOME-ARQ.             
           PERFORM 0000-TESTA-FILE-STATUS.                              
                                                                        
           PERFORM 0000-LER-LADO1.                                      
                                                                        
           IF WRK-FIM-LADO1  NOT =  'S'                                 
      *       IF WRK-IDENT  EQUAL  05                                   
              IF WRK-IDENT  EQUAL  WRK-FAIXA01                          
                 MOVE '1DJDE FORMS=CLLP03,END;' TO WRK-FORMS            
                 MOVE 'CLLP03' TO  CB3A-ROTINA CB3B-ROTINA              
              ELSE                                                      
      *          IF WRK-IDENT  EQUAL  20                                
                 IF WRK-IDENT  EQUAL  WRK-FAIXA02                       
                    MOVE '1DJDE FORMS=CLLP04,END;' TO WRK-FORMS         
                    MOVE 'CLLP04' TO  CB3A-ROTINA CB3B-ROTINA           
                 ELSE                                                   
      *             IF WRK-IDENT  EQUAL  40                             
                    IF WRK-IDENT  EQUAL  WRK-FAIXA03                    
                       MOVE '1DJDE FORMS=CLLP05,END;' TO WRK-FORMS      
                       MOVE 'CLLP05' TO  CB3A-ROTINA CB3B-ROTINA        
                    ELSE                                                
      *             -- WRK-IDENT = 99                                   
                      MOVE '1DJDE FORMS=CLLP06,END;' TO WRK-FORMS       
                      MOVE 'CLLP06' TO  CB3A-ROTINA CB3B-ROTINA.        
                                                                        
           PERFORM 0000-PROCESSA UNTIL WRK-FIM-LADO1 EQUAL 'S'.         
                                                                        
           MOVE    ACU-CARTAS       TO        LT1-CARTAS.               
                                                                        
           WRITE LIS-REGTO          FROM      WRK-DJDE-FIM.             
           MOVE 'WRITE'             TO        WRK-FUNCAO.               
           MOVE 'LISTA'             TO        WRK-NOME-ARQ.             
           MOVE WRK-FS-LISTA        TO        WRK-FILE-STATUS.          
           PERFORM 0000-TESTA-FILE-STATUS.                              
                                                                        
           IF  LNK-CCUSTO-PARM   EQUAL 'LEAS'                           
               MOVE 'BRADESCO LEASING S/A' TO CB1-NOME-BCO              
           ELSE                                                         
               MOVE 'BANCO BRADESCO S.A  ' TO CB1-NOME-BCO              
           END-IF.                                                      
                                                                        
           WRITE LT1-REGTO          FROM      CABEC1-L.                 
           MOVE WRK-FS-LISTA1       TO        WRK-FILE-STATUS.          
           MOVE 'LISTA1'            TO        WRK-NOME-ARQ.             
           PERFORM 0000-TESTA-FILE-STATUS.                              
                                                                        
           WRITE LT1-REGTO          FROM      LINTOT1.                  
           MOVE WRK-FS-LISTA1       TO        WRK-FILE-STATUS.          
           PERFORM 0000-TESTA-FILE-STATUS.                              
                                                                        
           CLOSE  LADO1  LADO2  DATATECL  LISTA  LISTA1  ARQECT         
                  ARQPARM.                                              
                                                                        
           MOVE 'CLOSE'             TO        WRK-FUNCAO.               
                                                                        
           MOVE WRK-FS-LADO1        TO        WRK-FILE-STATUS.          
           MOVE 'LADO1'             TO        WRK-NOME-ARQ.             
           PERFORM 0000-TESTA-FILE-STATUS.                              
                                                                        
           MOVE WRK-FS-LADO2        TO        WRK-FILE-STATUS.          
           MOVE 'LADO2'             TO        WRK-NOME-ARQ.             
           PERFORM 0000-TESTA-FILE-STATUS.                              
                                                                        
           MOVE WRK-FS-DATATECL     TO        WRK-FILE-STATUS.          
           MOVE 'DATATECL'          TO        WRK-NOME-ARQ.             
           PERFORM 0000-TESTA-FILE-STATUS.                              
                                                                        
           MOVE WRK-FS-LISTA        TO        WRK-FILE-STATUS.          
           MOVE 'LISTA'             TO        WRK-NOME-ARQ.             
           PERFORM 0000-TESTA-FILE-STATUS.                              
                                                                        
           MOVE WRK-FS-LISTA1       TO        WRK-FILE-STATUS.          
           MOVE 'LISTA1'            TO        WRK-NOME-ARQ.             
           PERFORM 0000-TESTA-FILE-STATUS.                              
                                                                        
           GOBACK.                                                      
                                                                        
       0000-INICIO-FIM.  EXIT.                                          
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       0000-TESTA-FILE-STATUS     SECTION.                              
                                                                        
           IF WRK-FILE-STATUS  NOT  EQUAL  '00' AND '10'                
              DISPLAY '***************** CLLP7635 ****************'     
              DISPLAY '*                                         *'     
              DISPLAY '* ERRO NO ' WRK-FUNCAO '                         
      -       '   *'                                                    
              DISPLAY '*                                         *'     
              DISPLAY '* DO ARQUIVO ' WRK-NOME-ARQ '                    
      -       '   *'                                                    
              DISPLAY '*                                         *'     
              DISPLAY '* FILE STATUS = ' WRK-FILE-STATUS '              
      -       '        *'                                               
              DISPLAY '*                                         *'     
              DISPLAY '***************** CLLP7635 ****************'     
              CALL 'ILBOABN0'         USING       WRK-ABEND             
           ELSE                                                         
              IF       WRK-FILE-STATUS  EQUAL  '10'                     
                 IF    WRK-NOME-ARQ  EQUAL  'LADO1'                     
                       MOVE 'S'             TO          WRK-FIM-LADO1   
                 ELSE                                                   
                   IF  WRK-NOME-ARQ  EQUAL  'LADO2'                     
                       MOVE 'S'             TO          WRK-FIM-LADO2   
                 ELSE                                                   
                   IF  WRK-NOME-ARQ  EQUAL  'ARQPARM'                   
                       MOVE 'S'             TO          WRK-FIM-PARM.   
                                                                        
       0000-TESTA-FILE-STATUS-FIM.  EXIT.                               
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       0000-LER-LADO1             SECTION.                              
                                                                        
           READ LADO1 INTO WRK-REGTO.                                   
                                                                        
           MOVE 'READ'              TO        WRK-FUNCAO.               
           MOVE 'LADO1'             TO        WRK-NOME-ARQ.             
           MOVE WRK-FS-LADO1        TO        WRK-FILE-STATUS.          
           PERFORM 0000-TESTA-FILE-STATUS.                              
                                                                        
       0000-LER-LADO1-FIM.  EXIT.                                       
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       0000-LER-LADO2             SECTION.                              
                                                                        
           READ LADO2 INTO WRK-REGTO.                                   
                                                                        
           MOVE 'READ'              TO        WRK-FUNCAO.               
           MOVE 'LADO2'             TO        WRK-NOME-ARQ.             
           MOVE WRK-FS-LADO2        TO        WRK-FILE-STATUS.          
           PERFORM 0000-TESTA-FILE-STATUS.                              
                                                                        
       0000-LER-LADO2-FIM.  EXIT.                                       
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       0000-LER-PARM              SECTION.                              
                                                                        
           READ ARQPARM.                                                
                                                                        
           MOVE 'READ'              TO        WRK-FUNCAO.               
           MOVE 'ARQPARM'           TO        WRK-NOME-ARQ.             
           MOVE WRK-FS-PARM         TO        WRK-FILE-STATUS.          
           PERFORM 0000-TESTA-FILE-STATUS.                              
                                                                        
           IF WRK-FIM-PARM   NOT =  'S'                                 
              IF PARM-SEQUENCIA = 1                                     
                 MOVE PARM-NR-DIAS TO WRK-FAIXA01                       
                 GO TO 0000-LER-PARM                                    
              ELSE                                                      
                 IF PARM-SEQUENCIA = 2                                  
                    MOVE PARM-NR-DIAS TO WRK-FAIXA02                    
                    GO TO 0000-LER-PARM                                 
                 ELSE                                                   
                    IF PARM-SEQUENCIA = 3                               
                       MOVE PARM-NR-DIAS TO WRK-FAIXA03                 
                       GO TO 0000-LER-PARM.                             
                                                                        
       0000-LER-PARM-FIM.  EXIT.                                        
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       0000-PROCESSA    SECTION.                                        
                                                                        
           PERFORM 0000-RESTART.                                        
                                                                        
           PERFORM 0000-LIMPA-CAMPOS                                    
                                                                        
           PERFORM 0000-MONTA-LADO1.                                    
                                                                        
           PERFORM 0000-LER-LADO2.                                      
                                                                        
           PERFORM 0000-MONTA-LADO2.                                    
                                                                        
           PERFORM 0000-IMPRI-INTERNO.                                  
                                                                        
           PERFORM 0000-IMPRI-EXTERNO.                                  
                                                                        
           PERFORM 0000-LER-LADO1.                                      
                                                                        
       0000-PROCESSA-FIM.  EXIT.                                        
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       0000-RESTART               SECTION.                              
                                                                        
           ADD     1                  TO         WRK-RESTART.           
                                                                        
           MOVE    WRK-RESTART        TO         INT-RESTART            
                                                 EXT-RESTART            
                                                 EXT1-RESTART           
                                                 CBCD-RESTART           
                                                 CB1B-RESTART           
                                                 CB2B-RESTART           
                                                 CB3B-RESTART           
                                                 LPOS1-SEQ-RESTART      
                                                 LPOS2-SEQ-RESTART      
                                                 LCIF1-SEQ-RESTART      
                                                 LCIF2-SEQ-RESTART      
                                                 CB4B-RESTART           
                                                 CB5B-RESTART           
                                                 CB5BB-RESTART          
                                                 CB6B-RESTART           
                                                 CB7B-RESTART           
                                                 CB8-RESTART            
                                                 CB9-RESTART            
                                                  LD1-RESTART           
                                                  LD2-RESTART           
                                                  LD3-RESTART           
                                                  LD7-RESTART           
                                                  LD8-RESTART           
                                                  LIN2-RESTART          
                                                  LIN1-RESTART.         
                                                                        
                                                                        
       0000-RESTART-FIM.  EXIT.                                         
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       0000-LIMPA-CAMPOS          SECTION.                              
                                                                        
           MOVE SPACES                    TO           CBCD-LADO1       
                                                       CB1A-LADO1       
                                                       CB2A-LADO1       
                                                       CB3A-LADO1       
                                                       CB4A-LADO1       
                                                       CB5A-LADO1       
                                                       CB5AA-LADO1      
                                                       WRKAA-LADO1      
                                                       CB6A-LADO1       
                                                       WRKA-LADO1       
                                                       CB7A-LADO1.      
                                                                        
           MOVE SPACES                    TO           CBCD-LADO2       
                                                       CB1B-LADO2       
                                                       CB2B-LADO2       
                                                       CB3B-LADO2       
                                                       CB4B-LADO2       
                                                       CB5B-LADO2       
                                                       CB5BB-LADO2      
                                                       WRKBB-LADO2      
                                                       CB6B-LADO2       
                                                       WRKB-LADO2       
                                                       CB7B-LADO2.      
                                                                        
           MOVE SPACES                    TO           LD1-LADOA        
                                                       LD2-LADOA        
                                                       LD3-LADOA        
                                                       LD7-LADOA        
                                                       LD8-LADOA.       
                                                                        
           MOVE SPACES                    TO           LD1-LADOB        
                                                       LD2-LADOB        
                                                       LD3-LADOB        
                                                       LD7-LADOB        
                                                       LD8-LADOB.       
                                                                        
           MOVE SPACES                    TO           LD4-LINHA (1).   
           MOVE SPACES                    TO           LD4-LINHA (2).   
           MOVE SPACES                    TO           LD4-LINHA (3).   
           MOVE SPACES                    TO           LD4-LINHA (4).   
           MOVE SPACES                    TO           LD4-LINHA (5).   
           MOVE SPACES                    TO           LD4-LINHA (6).   
           MOVE SPACES                    TO           LD4-LINHA (7).   
                                                                        
           MOVE SPACES                    TO           LD5-LINHA (1).   
           MOVE SPACES                    TO           LD5-LINHA (2).   
           MOVE SPACES                    TO           LD5-LINHA (3).   
           MOVE SPACES                    TO           LD5-LINHA (4).   
           MOVE SPACES                    TO           LD5-LINHA (5).   
           MOVE SPACES                    TO           LD5-LINHA (6).   
           MOVE SPACES                    TO           LD5-LINHA (7).   
                                                                        
       0000-LIMPA-CAMPOS-FIM. EXIT.                                     
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       0000-MONTA-LADO1           SECTION.                              
                                                                        
           PERFORM 0000-MONT-INT-LD1.                                   
                                                                        
           PERFORM 0000-MONT-EXT-LD1.                                   
                                                                        
       0000-MONTA-LADO1-FIM.  EXIT.                                     
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       0000-MONT-INT-LD1          SECTION.                              
                                                                        
           MOVE    WRK-NOME-RESP     TO    LD1A-NOME-RES                
                                                                        
           MOVE    WRK-AGENCIA       TO    LD2A-AGENCIA                 
           MOVE    ' - '             TO    LD2A-HIFEN                   
           MOVE    WRK-NOME-AGE      TO    LD2A-NOME-AGE                
                                                                        
           MOVE    WRK-CONTA         TO    LD3A-CONTA                   
           MOVE    '-'               TO    LD3A-HIFEN                   
                                                                        
           MOVE    '7'               TO    WRK-DIGITO                   
           MOVE     4                TO    WRK-TAMANHO                  
           CALL    'POOL0431'     USING    WRK-CONTA                    
                                           WRK-DIGITO                   
                                           WRK-TAMANHO                  
                                                                        
           IF  WRK-DIGITO  EQUAL  'P'                                   
               MOVE    '0'           TO    LD3A-DIG-CTA                 
           ELSE                                                         
               MOVE    WRK-DIGITO    TO    LD3A-DIG-CTA.                
                                                                        
           MOVE    '/'               TO    LD3A-BARRA1                  
                                           LD3A-BARRA2                  
           MOVE    WRK-DIA-SIST      TO    LD3A-DIA                     
           MOVE    WRK-MES-SIST      TO    LD3A-MES                     
           MOVE    WRK-ANO-SIST      TO    LD3A-ANO                     
                                                                        
           PERFORM  0000-LIMP-ARQA VARYING WRK-IND1 FROM 1 BY 1 UNTIL   
                                           WRK-IND1 GREATER 7.          
                                                                        
           MOVE    7                 TO    WRK-IND-AUX.                 
           PERFORM  0000-SORT-A VARYING WRK-IND1 FROM 1 BY 1 UNTIL      
                                        WRK-IND1 GREATER WRK-IND-AUX.   
                                                                        
           PERFORM  0000-CARRE-DET-A VARYING WRK-IND1 FROM 1 BY 1 UNTIL 
                                             WRK-IND1 GREATER 7.        
                                                                        
       0000-MONT-INT-LD1-FIM.  EXIT.                                    
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       0000-LIMP-ARQA             SECTION.                              
                                                                        
              IF WRK-VCMTO(WRK-IND1)  NOT NUMERIC                       
                 MOVE ZEROS           TO      WRK-VCMTO(WRK-IND1).      
                                                                        
       0000-LIMP-ARQA-FIM.  EXIT.                                       
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       0000-SORT-A                SECTION.                              
                                                                        
           MOVE 2                   TO       WRK-IND2.                  
           PERFORM  0000-SORT-A1 VARYING WRK-IND1 FROM 1 BY 1 UNTIL     
                                         WRK-IND1 GREATER WRK-IND-AUX.  
                                                                        
           COMPUTE WRK-IND-AUX = WRK-IND-AUX - 1.                       
           MOVE    1                TO       WRK-IND1.                  
                                                                        
       0000-SORT-A-FIM. EXIT.                                           
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       0000-SORT-A1               SECTION.                              
                                                                        
           IF WRK-IND2   GREATER  7                                     
              NEXT SENTENCE                                             
           ELSE                                                         
              IF WRK-VCMTO(WRK-IND1)  LESS  WRK-VCMTO(WRK-IND2)         
                 MOVE WRK-OCORRENCIAS(WRK-IND1) TO WRK-AREA-AUX         
                 MOVE WRK-OCORRENCIAS(WRK-IND2) TO                      
                                               WRK-OCORRENCIAS(WRK-IND1)
                 MOVE WRK-AREA-AUX    TO WRK-OCORRENCIAS(WRK-IND2).     
                                                                        
           ADD   1                        TO WRK-IND2.                  
                                                                        
       0000-SORT-A1-FIM.  EXIT.                                         
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       0000-CARRE-DET-A           SECTION.                              
                                                                        
           IF WRK-VCMTO (WRK-IND1)  EQUAL ZEROS                         
              MOVE 8                      TO WRK-IND1                   
              GO TO 0000-CARRE-DET-A-FIM.                               
                                                                        
           MOVE WRK-NATUREZA (WRK-IND1)   TO LD4A-COD-NAT  (WRK-IND1).  
           MOVE ' - '                     TO LD4A-HIFEN    (WRK-IND1).  
           MOVE WRK-DESCRIC (WRK-IND1)    TO LD4A-DESCRIC  (WRK-IND1).  
                                                                        
           MOVE WRK-VCMTO    (WRK-IND1)   TO WRK-DT-SSAAMMDD-R.         
           MOVE WRK-DIA-SAMD              TO LD5A-DIA      (WRK-IND1).  
           MOVE '/'                       TO LD5A-BAR1     (WRK-IND1).  
           MOVE WRK-MES-SAMD              TO LD5A-MES      (WRK-IND1).  
           MOVE '/'                       TO LD5A-BAR2     (WRK-IND1).  
           MOVE WRK-ANO-SAMD              TO LD5A-ANO      (WRK-IND1).  
           MOVE 'VENCTO MAIS ANTIGO:'     TO LD5A-LIT1     (WRK-IND1).  
           MOVE WRK-VALOR    (WRK-IND1)   TO LD5A-VALOR    (WRK-IND1).  
           MOVE 'TOTAL DA DIVIDA NO VENCTO:' TO LD5A-LIT2  (WRK-IND1).  
                                                                        
           ADD  WRK-VALOR    (WRK-IND1)   TO WRK-VALOR-A.               
                                                                        
           MOVE 'TOTAL DA RESPONSABILIDADE NO VENCIMENTO:'              
                                          TO  LD7A-TOT-LIT.             
                                                                        
           MOVE    WRK-NRO-SEQ            TO  LD8A-NRO-SEQ.             
                                                                        
       0000-CARRE-DET-A-FIM. EXIT.                                      
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       0000-MONT-EXT-LD1          SECTION.                              
                                                                        
           MOVE    WRK-AGENCIA       TO    CB3A-AGEN.                   
           MOVE    '-'               TO    CB3A-HIFEN.                  
           MOVE    '/'               TO    CB3A-BARRA.                  
           MOVE    WRK-NOME-AGE      TO    CB3A-NOME-AG.                
                                                                        
           MOVE    '7'               TO    WRK-DIGITO.                  
           MOVE     3                TO    WRK-TAMANHO.                 
                                                                        
           CALL  'POOL0431'       USING    WRK-AGENCIA                  
                                           WRK-DIGITO                   
                                           WRK-TAMANHO.                 
           IF WRK-DIGITO  EQUAL  'P'                                    
              MOVE '0'               TO    CB3A-DIG-AG                  
           ELSE                                                         
              MOVE WRK-DIGITO        TO    CB3A-DIG-AG.                 
                                                                        
           MOVE    WRK-NOME-RESP     TO    CB4A-NOME-RES                
                                                                        
           CALL    WRK-LPCL5011      USING WRK-END-RESP.                
                                                                        
           MOVE  SPACES              TO    CB5A-END-RES.                
           STRING WRK-END-RESP ' ' WRK-DEV-NRO                          
           DELIMITED BY '  ' INTO CB5A-END-RES.                         
                                                                        
           MOVE    WRK-DEV-COMPL     TO    WRKA-COM-RES                 
           MOVE    WRK-DEV-BAIRRO    TO    WRKA-BAI-RES                 
                                                                        
           MOVE    WRK-CEP           TO    WRKA-CEP                     
           MOVE    '-'               TO    WRKA-HIFEN                   
           MOVE    WRK-CEP-SUFIXO    TO    WRKA-CEP-SUFIXO              
                                                                        
           MOVE  SPACES              TO    WRKA-CIDADE                  
           STRING WRK-CIDADE  '-' WRK-EST                               
           DELIMITED BY '  ' INTO WRKA-CIDADE                           
                                                                        
           MOVE    WRK-NRO-SEQ       TO    CB3A-NRO-SEQ                 
                                                                        
           IF  WRK-TIPO    EQUAL   1                                    
                                                                        
               MOVE    WRK-COD-BARRA TO     CB9A-COD-BARRA              
                                                                        
               MOVE  29              TO     WRK-TAMANHO-380             
               CALL  'POOL0380'      USING  CB9-LADO1                   
                                            WRK-TAMANHO-380             
                                                                        
               PERFORM  0000-CARTA-CORREIO-A                            
           ELSE                                                         
               ADD  1                TO     ACU-CARTAS                  
               IF  WRK-TIPO    EQUAL   2                                
                   MOVE  '*999999999*'  TO  LPOS2-AREA-POSTNET-E        
                   MOVE  ALL '9'        TO  LCIF1-AREA-E                
                                            LCIF2-AREA-E                
                   PERFORM 0000-CARTA-AGENCIA-A                         
               ELSE                                                     
                   MOVE  '*999999999*'  TO  LPOS2-AREA-POSTNET-E        
                   MOVE  ALL '9'        TO  LCIF1-AREA-E                
                                            LCIF2-AREA-E                
                   PERFORM 0000-CARTA-AVALISTA-A.                       
                                                                        
       0000-MONT-EXT-LD1-FIM. EXIT.                                     
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       0000-CARTA-CORREIO-A       SECTION.                              
                                                                        
           MOVE    WRK-END-AGE       TO    CB1A-END-AG.                 
           MOVE    SPACES            TO    CB1A-ECT.                    
           MOVE    WRK-CEP-AG        TO    WRK-TRATA-CEP-R.             
           MOVE    WRK-TR-CEP        TO    CB2A-CEP.                    
           MOVE   '-'                TO    CB2A-HIFEN.                  
           MOVE    WRK-TR-CEP-SUF    TO    CB2A-AG-SUFIXO.              
           MOVE    WRK-MUNICIPIO     TO    CB2A-MUNICIPIO.              
           MOVE    WRK-EST-AG        TO    CB2A-SIGLA.                  
                                                                        
      *================================================================*
      *===> COM A IMPRESSAO DO CODIGO DE BARRAS PARA A EMPRESA ADDRESS *
      *===> O ENDERECO DE RETORNO DE CORRESPONDENCIA PARA A AGENCIA    *
      *===> NAO DEVE MAIS SER IMPRESSO                                 *
      *================================================================*
                                                                        
SAN        MOVE    SPACES            TO    CB1A-END-AG.                 
SAN        MOVE    SPACES            TO    CB1A-LADO1.                  
                                                                        
           MOVE WRK-CEP              TO    WRK-NUMCEP-POSTNET-9011.     
           MOVE WRK-CEP-SUFIXO       TO    WRK-CPLCEP-POSTNET-9011.     
                                                                        
           PERFORM  0000-OBTEM-BARRA-POSTNET  THRU                      
                    0000-OBTEM-BARRA-POSTNET-FIM.                       
           MOVE  WRK-RETORNO-POSTNET-9011 TO LPOS2-AREA-POSTNET-E.      
           PERFORM  0000-MONTA-BARRA-CIF      THRU                      
                    0000-MONTA-BARRA-CIF-FIM.                           
           MOVE  WRK-LINHA-RETORNO-9010  TO   LCIF1-AREA-E.             
           MOVE  WRK-LINHA-NUMERO-9010   TO   LCIF2-AREA-E.             
                                                                        
       0000-CARTA-CORREIO-A-FIM.  EXIT.                                 
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       0000-CARTA-AGENCIA-A       SECTION.                              
                                                                        
           MOVE    SPACES        TO    CBCD1-BRANCO1.                   
           MOVE   'CD'           TO    CBCD1-CD.                        
           MOVE    WRK-CD        TO    CBCD1-NRO-CD.                    
                                                                        
           IF  WRK-COD-ENT     EQUAL     3                              
               MOVE   ' '  TO  CB2-CARRO                                
               MOVE   '0'  TO  CB3-CARRO                                
               MOVE   'CEP IRREGULAR - CONSULTE  O'  TO   CB1A-CEP-IR   
               MOVE   'GUIA POSTAL OU CDC REGIONAL'  TO   CB2A-CEP-IR   
           ELSE                                                         
               MOVE   ' '  TO  CB2-CARRO                                
               MOVE   '-'  TO  CB3-CARRO                                
               MOVE SPACES TO  CB1A-BRANCO3                             
               MOVE   'FRANQUIA  CANCELADA'          TO   CB1A-FRANQUIA.
                                                                        
       0000-CARTA-AGENCIA-A-FIM. EXIT.                                  
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       0000-CARTA-AVALISTA-A      SECTION.                              
                                                                        
           MOVE    SPACES            TO    CBCD1-BRANCO1.               
           MOVE   'CD'               TO    CBCD1-CD.                    
           MOVE    WRK-CD            TO    CBCD1-NRO-CD.                
           MOVE   ' '                TO    CB2-CARRO.                   
           MOVE   '-'                TO    CB3-CARRO.                   
           MOVE   SPACES             TO    CB1A-BRANCO3.                
           MOVE   'FRANQUIA  CANCELADA'    TO    CB1A-FRANQUIA.         
                                                                        
       0000-CARTA-AVALISTA-A-FIM. EXIT.                                 
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       0000-MONTA-LADO2           SECTION.                              
           IF WRK-FIM-LADO2  EQUAL 'S'                                  
              GO TO 0000-MONTA-LADO2-FIM.                               
                                                                        
           PERFORM 0000-MONT-INT-LD2.                                   
                                                                        
           PERFORM 0000-MONT-EXT-LD2.                                   
                                                                        
       0000-MONTA-LADO2-FIM.  EXIT.                                     
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       0000-MONT-INT-LD2          SECTION.                              
                                                                        
           MOVE    WRK-NOME-RESP     TO    LD1B-NOME-RES                
                                                                        
           MOVE    WRK-AGENCIA       TO    LD2B-AGENCIA                 
           MOVE    ' - '             TO    LD2B-HIFEN                   
           MOVE    WRK-NOME-AGE      TO    LD2B-NOME-AGE                
                                                                        
           MOVE    WRK-CONTA         TO    LD3B-CONTA                   
           MOVE    '-'               TO    LD3B-HIFEN                   
                                                                        
           MOVE    '7'               TO    WRK-DIGITO                   
           MOVE     4                TO    WRK-TAMANHO                  
           CALL    'POOL0431'     USING    WRK-CONTA                    
                                           WRK-DIGITO                   
                                           WRK-TAMANHO                  
                                                                        
           IF  WRK-DIGITO  EQUAL  'P'                                   
               MOVE    '0'           TO    LD3B-DIG-CTA                 
           ELSE                                                         
               MOVE    WRK-DIGITO    TO    LD3B-DIG-CTA.                
                                                                        
           MOVE    '/'               TO    LD3B-BARRA1                  
                                           LD3B-BARRA2                  
           MOVE    WRK-DIA-SIST      TO    LD3B-DIA                     
           MOVE    WRK-MES-SIST      TO    LD3B-MES                     
           MOVE    WRK-ANO-SIST      TO    LD3B-ANO                     
                                                                        
           PERFORM  0000-LIMP-ARQB VARYING WRK-IND1 FROM 1 BY 1 UNTIL   
                                           WRK-IND1 GREATER 7.          
                                                                        
           MOVE    7                 TO    WRK-IND-AUX.                 
           PERFORM  0000-SORT-B1 VARYING WRK-IND1 FROM 1 BY 1 UNTIL     
                                         WRK-IND1 GREATER WRK-IND-AUX.  
                                                                        
           PERFORM  0000-CARRE-DET-B1 VARYING WRK-IND1 FROM 1 BY 1 UNTIL
                                              WRK-IND1 GREATER 7.       
                                                                        
       0000-MONT-INT-LD2-FIM.  EXIT.                                    
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       0000-LIMP-ARQB             SECTION.                              
                                                                        
              IF WRK-VCMTO(WRK-IND1)  NOT NUMERIC                       
                 MOVE ZEROS           TO      WRK-VCMTO(WRK-IND1).      
                                                                        
       0000-LIMP-ARQB-FIM.  EXIT.                                       
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       0000-SORT-B1               SECTION.                              
                                                                        
           MOVE 2                   TO       WRK-IND2.                  
           PERFORM  0000-SORT-B2 VARYING WRK-IND1 FROM 1 BY 1 UNTIL     
                                         WRK-IND1 GREATER WRK-IND-AUX.  
                                                                        
           COMPUTE WRK-IND-AUX = WRK-IND-AUX - 1.                       
           MOVE    1                TO       WRK-IND1.                  
                                                                        
       0000-SORT-B1-FIM. EXIT.                                          
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       0000-SORT-B2                 SECTION.                            
                                                                        
           IF WRK-IND2   GREATER  7                                     
              NEXT SENTENCE                                             
           ELSE                                                         
              IF WRK-VCMTO(WRK-IND1)  LESS  WRK-VCMTO(WRK-IND2)         
                 MOVE WRK-OCORRENCIAS(WRK-IND1) TO WRK-AREA-AUX         
                 MOVE WRK-OCORRENCIAS(WRK-IND2) TO                      
                                            WRK-OCORRENCIAS(WRK-IND1)   
                 MOVE WRK-AREA-AUX   TO WRK-OCORRENCIAS(WRK-IND2).      
                                                                        
           ADD   1                        TO WRK-IND2.                  
                                                                        
       0000-SORT-B2-FIM.  EXIT.                                         
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       0000-CARRE-DET-B1          SECTION.                              
                                                                        
           IF WRK-VCMTO (WRK-IND1)  EQUAL ZEROS                         
              MOVE 8                      TO WRK-IND1                   
              GO TO 0000-CARRE-DET-B1-FIM.                              
                                                                        
           MOVE WRK-NATUREZA (WRK-IND1)   TO LD4B-COD-NAT  (WRK-IND1).  
           MOVE ' - '                     TO LD4B-HIFEN    (WRK-IND1).  
           MOVE WRK-DESCRIC  (WRK-IND1)   TO LD4B-DESCRIC  (WRK-IND1).  
                                                                        
           MOVE WRK-VCMTO    (WRK-IND1)   TO WRK-DT-SSAAMMDD-R.         
           MOVE WRK-DIA-SAMD              TO LD5B-DIA      (WRK-IND1).  
           MOVE '/'                       TO LD5B-BAR1     (WRK-IND1).  
           MOVE WRK-MES-SAMD              TO LD5B-MES      (WRK-IND1).  
           MOVE '/'                       TO LD5B-BAR2     (WRK-IND1).  
           MOVE WRK-ANO-SAMD              TO LD5B-ANO      (WRK-IND1).  
           MOVE 'VENCTO MAIS ANTIGO:'     TO LD5B-LIT1     (WRK-IND1).  
           MOVE WRK-VALOR    (WRK-IND1)   TO LD5B-VALOR    (WRK-IND1).  
           MOVE 'TOTAL DA DIVIDA NO VENCTO:' TO LD5B-LIT2  (WRK-IND1).  
                                                                        
           ADD  WRK-VALOR    (WRK-IND1)   TO WRK-VALOR-B.               
                                                                        
           MOVE 'TOTAL DA RESPONSABILIDADE NO VENCIMENTO:'              
                                          TO  LD7B-TOT-LIT.             
                                                                        
           MOVE    WRK-NRO-SEQ            TO  LD8B-NRO-SEQ.             
                                                                        
       0000-CARRE-DET-B1-FIM. EXIT.                                     
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       0000-MONT-EXT-LD2          SECTION.                              
                                                                        
           MOVE    WRK-AGENCIA       TO    CB3B-AGEN.                   
           MOVE    '-'               TO    CB3B-HIFEN.                  
           MOVE    '/'               TO    CB3B-BARRA.                  
           MOVE    WRK-NOME-AGE      TO    CB3B-NOME-AG.                
                                                                        
           MOVE    '7'               TO    WRK-DIGITO.                  
           MOVE     3                TO    WRK-TAMANHO.                 
                                                                        
           CALL  'POOL0431'       USING    WRK-AGENCIA                  
                                           WRK-DIGITO                   
                                           WRK-TAMANHO.                 
           IF WRK-DIGITO  EQUAL  'P'                                    
              MOVE '0'               TO    CB3B-DIG-AG                  
           ELSE                                                         
              MOVE WRK-DIGITO        TO    CB3B-DIG-AG.                 
                                                                        
           MOVE    WRK-NOME-RESP     TO    CB4B-NOME-RES                
                                                                        
           CALL    WRK-LPCL5011      USING WRK-END-RESP.                
                                                                        
           MOVE  SPACES              TO    CB5B-END-RES.                
           STRING WRK-END-RESP ' ' WRK-DEV-NRO                          
           DELIMITED BY '  ' INTO CB5B-END-RES.                         
                                                                        
           MOVE    WRK-DEV-COMPL     TO    WRKB-COM-RES                 
           MOVE    WRK-DEV-BAIRRO    TO    WRKB-BAI-RES                 
                                                                        
           MOVE    WRK-CEP           TO    WRKB-CEP                     
           MOVE    '-'               TO    WRKB-HIFEN                   
           MOVE    WRK-CEP-SUFIXO    TO    WRKB-CEP-SUFIXO              
                                                                        
           MOVE  SPACES              TO    WRKB-CIDADE                  
           STRING WRK-CIDADE  '-' WRK-EST                               
           DELIMITED BY '  ' INTO WRKB-CIDADE                           
                                                                        
           MOVE    WRK-NRO-SEQ       TO    CB3B-NRO-SEQ                 
                                                                        
           IF  WRK-TIPO   EQUAL   1                                     
                                                                        
               MOVE    WRK-COD-BARRA TO    CB9B-COD-BARRA               
                                                                        
               MOVE  29              TO    WRK-TAMANHO-380              
               CALL  'POOL0380'      USING CB9-LADO2                    
                                           WRK-TAMANHO-380              
                                                                        
               PERFORM 0000-CARTA-CORREIO-B                             
           ELSE                                                         
               ADD 1                 TO    ACU-CARTAS                   
               IF  WRK-TIPO   EQUAL   2                                 
                   MOVE  '*999999999*'  TO  LPOS2-AREA-POSTNET-D        
                   MOVE  ALL '9'        TO  LCIF1-AREA-D                
                                            LCIF2-AREA-D                
                   PERFORM 0000-CARTA-AGENCIA-B                         
               ELSE                                                     
                   MOVE  '*999999999*'  TO  LPOS2-AREA-POSTNET-D        
                   MOVE  ALL '9'        TO  LCIF1-AREA-D                
                                            LCIF2-AREA-D                
                   PERFORM 0000-CARTA-AVALISTA-B.                       
                                                                        
       0000-MONT-EXT-LD2-FIM. EXIT.                                     
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       0000-CARTA-CORREIO-B       SECTION.                              
                                                                        
           MOVE    WRK-END-AGE       TO    CB1B-END-AG.                 
           MOVE   SPACES             TO    CB1B-ECT.                    
           MOVE    WRK-CEP-AG        TO    WRK-TRATA-CEP-R.             
           MOVE    WRK-TR-CEP        TO    CB2B-CEP.                    
           MOVE   '-'                TO    CB2B-HIFEN.                  
           MOVE    WRK-TR-CEP-SUF    TO    CB2B-AG-SUFIXO.              
           MOVE    WRK-MUNICIPIO     TO    CB2B-MUNICIPIO.              
           MOVE    WRK-EST-AG        TO    CB2B-SIGLA.                  
                                                                        
      *================================================================*
      *===> COM A IMPRESSAO DO CODIGO DE BARRAS PARA A EMPRESA ADDRESS *
      *===> O ENDERECO DE RETORNO DE CORRESPONDENCIA PARA A AGENCIA    *
      *===> NAO DEVE MAIS SER IMPRESSO                                 *
      *================================================================*
                                                                        
SAN        MOVE    SPACES            TO    CB1B-END-AG.                 
SAN        MOVE    SPACES            TO    CB1B-LADO2.                  
                                                                        
           MOVE WRK-CEP              TO    WRK-NUMCEP-POSTNET-9011.     
           MOVE WRK-CEP-SUFIXO       TO    WRK-CPLCEP-POSTNET-9011.     
           PERFORM  0000-OBTEM-BARRA-POSTNET  THRU                      
                    0000-OBTEM-BARRA-POSTNET-FIM.                       
           MOVE  WRK-RETORNO-POSTNET-9011 TO LPOS2-AREA-POSTNET-D.      
           PERFORM  0000-MONTA-BARRA-CIF      THRU                      
                    0000-MONTA-BARRA-CIF-FIM.                           
           MOVE  WRK-LINHA-RETORNO-9010  TO   LCIF1-AREA-D.             
           MOVE  WRK-LINHA-NUMERO-9010   TO   LCIF2-AREA-D.             
                                                                        
       0000-CARTA-CORREIO-B-FIM. EXIT.                                  
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       0000-CARTA-AGENCIA-B       SECTION.                              
                                                                        
           MOVE    SPACES          TO    CBCD2-BRANCO1.                 
           MOVE    'CD'            TO    CBCD2-CD.                      
           MOVE    WRK-CD          TO    CBCD2-NRO-CD.                  
                                                                        
           IF  WRK-COD-ENT    EQUAL    3                                
               MOVE   ' '  TO  CB2-CARRO                                
               MOVE   '0'  TO  CB3-CARRO                                
               MOVE    'CEP IRREGULAR - CONSULTE  O' TO    CB1B-CEP-IR  
               MOVE    'GUIA POSTAL OU CDC REGIONAL' TO    CB2B-CEP-IR  
           ELSE                                                         
               MOVE   ' '  TO  CB2-CARRO                                
               MOVE   '-'  TO  CB3-CARRO                                
               MOVE SPACES TO  CB1B-BRANCO3                             
               MOVE    'FRANQUIA CANCELADA'  TO    CB1B-FRANQUIA.       
                                                                        
       0000-CARTA-AGENCIA-B-FIM.  EXIT.                                 
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       0000-CARTA-AVALISTA-B      SECTION.                              
                                                                        
           MOVE    SPACES            TO    CBCD2-BRANCO1.               
           MOVE    'CD'              TO    CBCD2-CD.                    
           MOVE    WRK-CD            TO    CBCD2-NRO-CD.                
           MOVE    ' '               TO    CB2-CARRO.                   
           MOVE    '-'               TO    CB3-CARRO.                   
           MOVE    SPACES            TO    CB1B-BRANCO3.                
           MOVE    'FRANQUIA  CANCELADA'   TO    CB1B-FRANQUIA.         
                                                                        
       0000-CARTA-AVALISTA-B-FIM. EXIT.                                 
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       0000-IMPRI-INTERNO         SECTION.                              
                                                                        
           WRITE LIS-REGTO           FROM       WRK-DJDE-INT.           
           MOVE 'WRITE'              TO         WRK-FUNCAO.             
           MOVE 'LISTA'              TO         WRK-NOME-ARQ.           
           MOVE WRK-FS-LISTA         TO         WRK-FILE-STATUS.        
           PERFORM 0000-TESTA-FILE-STATUS.                              
                                                                        
           WRITE LIS-REGTO           FROM       LINBRA1.                
           MOVE WRK-FS-LISTA         TO         WRK-FILE-STATUS.        
           PERFORM 0000-TESTA-FILE-STATUS.                              
                                                                        
           WRITE LIS-REGTO           FROM       LINHA1.                 
           MOVE WRK-FS-LISTA         TO         WRK-FILE-STATUS.        
           PERFORM 0000-TESTA-FILE-STATUS.                              
                                                                        
           WRITE LIS-REGTO           FROM       LINHA2.                 
           MOVE WRK-FS-LISTA         TO         WRK-FILE-STATUS.        
           PERFORM 0000-TESTA-FILE-STATUS.                              
                                                                        
           WRITE LIS-REGTO           FROM       LINHA3.                 
           MOVE WRK-FS-LISTA         TO         WRK-FILE-STATUS.        
           PERFORM 0000-TESTA-FILE-STATUS.                              
                                                                        
           PERFORM 0000-IMPR-TAB VARYING WRK-IND1 FROM 1 BY 1           
                                         UNTIL WRK-IND1 GREATER 7.      
                                                                        
           MOVE  WRK-VALOR-A         TO         LD7A-TOT-DIVIDA.        
           MOVE  WRK-VALOR-B         TO         LD7B-TOT-DIVIDA.        
                                                                        
           WRITE LIS-REGTO           FROM       LINHA7.                 
           MOVE WRK-FS-LISTA         TO         WRK-FILE-STATUS.        
           PERFORM 0000-TESTA-FILE-STATUS.                              
                                                                        
           WRITE LIS-REGTO           FROM       LINHA8.                 
           MOVE WRK-FS-LISTA         TO         WRK-FILE-STATUS.        
           PERFORM 0000-TESTA-FILE-STATUS.                              
                                                                        
           MOVE  ZEROS               TO         WRK-VALOR-A             
                                                WRK-VALOR-B.            
                                                                        
       0000-IMPRI-INTERNO-FIM.  EXIT.                                   
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       0000-IMPR-TAB              SECTION.                              
                                                                        
           MOVE WRK-RESTART          TO        LD4-RESTART(WRK-IND1)    
                                               LD5-RESTART(WRK-IND1).   
                                                                        
           MOVE '1'                  TO        LD4-FONTINDEX(WRK-IND1)  
                                               LD5-FONTINDEX(WRK-IND1). 
                                                                        
           MOVE  '-'                 TO        LD4-CARRO(WRK-IND1).     
                                                                        
           WRITE LIS-REGTO           FROM      LD4-LINHA(WRK-IND1).     
           MOVE WRK-FS-LISTA         TO        WRK-FILE-STATUS.         
           PERFORM 0000-TESTA-FILE-STATUS.                              
                                                                        
           WRITE LIS-REGTO           FROM      LD5-LINHA(WRK-IND1).     
           MOVE WRK-FS-LISTA         TO        WRK-FILE-STATUS.         
           PERFORM 0000-TESTA-FILE-STATUS.                              
                                                                        
       0000-IMPR-TAB-FIM.  EXIT.                                        
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       0000-IMPRI-EXTERNO         SECTION.                              
                                                                        
           IF  WRK-TIPO   EQUAL   1                                     
               WRITE LIS-REGTO           FROM      WRK-DJDE-EXT1        
               MOVE WRK-FS-LISTA         TO        WRK-FILE-STATUS      
               PERFORM 0000-TESTA-FILE-STATUS                           
           ELSE                                                         
               WRITE LIS-REGTO           FROM      WRK-DJDE-EXT         
               MOVE WRK-FS-LISTA         TO        WRK-FILE-STATUS      
               PERFORM 0000-TESTA-FILE-STATUS.                          
                                                                        
           WRITE LIS-REGTO           FROM      LINBRA1.                 
           MOVE WRK-FS-LISTA         TO        WRK-FILE-STATUS.         
           PERFORM 0000-TESTA-FILE-STATUS.                              
                                                                        
           IF  WRK-TIPO  NOT EQUAL  1                                   
               WRITE LIS-REGTO       FROM      CABECDD                  
               MOVE WRK-FS-LISTA     TO        WRK-FILE-STATUS          
               PERFORM 0000-TESTA-FILE-STATUS.                          
                                                                        
           WRITE LIS-REGTO           FROM      CABEC1.                  
           MOVE WRK-FS-LISTA         TO        WRK-FILE-STATUS.         
           PERFORM 0000-TESTA-FILE-STATUS.                              
                                                                        
           WRITE LIS-REGTO           FROM      CABEC2.                  
           MOVE WRK-FS-LISTA         TO        WRK-FILE-STATUS.         
           PERFORM 0000-TESTA-FILE-STATUS.                              
                                                                        
           WRITE LIS-REGTO           FROM      CABEC3.                  
           MOVE WRK-FS-LISTA         TO        WRK-FILE-STATUS.         
           PERFORM 0000-TESTA-FILE-STATUS.                              
                                                                        
           WRITE LIS-REGTO           FROM      LINHA-POSTNET1.          
           MOVE WRK-FS-LISTA         TO        WRK-FILE-STATUS.         
           PERFORM 0000-TESTA-FILE-STATUS.                              
                                                                        
           WRITE LIS-REGTO           FROM      LINHA-POSTNET2.          
           MOVE WRK-FS-LISTA         TO        WRK-FILE-STATUS.         
           PERFORM 0000-TESTA-FILE-STATUS.                              
                                                                        
           WRITE LIS-REGTO           FROM      CABEC4.                  
           MOVE WRK-FS-LISTA         TO        WRK-FILE-STATUS.         
           PERFORM 0000-TESTA-FILE-STATUS.                              
                                                                        
           WRITE LIS-REGTO           FROM      CABEC5.                  
           MOVE WRK-FS-LISTA         TO        WRK-FILE-STATUS.         
           PERFORM 0000-TESTA-FILE-STATUS.                              
                                                                        
           IF  WRKA-BAI-RES    EQUAL   SPACES                           
               MOVE  WRKA-COM-RES        TO        WRKA-BAI-RES         
               MOVE  SPACES              TO        WRKA-COM-RES.        
                                                                        
           IF  WRKB-BAI-RES    EQUAL   SPACES                           
               MOVE  WRKB-COM-RES        TO        WRKB-BAI-RES         
               MOVE  SPACES              TO        WRKB-COM-RES.        
                                                                        
           IF    ( WRKAA-LADO1      EQUAL   SPACES ) AND                
                 ( WRKBB-LADO2      EQUAL   SPACES )                    
                 MOVE  WRKA-LADO1          TO        CB6A-LADO1         
                 MOVE  WRKB-LADO2          TO        CB6B-LADO2         
                 WRITE LIS-REGTO       FROM      CABEC6                 
                 MOVE WRK-FS-LISTA     TO        WRK-FILE-STATUS        
                 PERFORM 0000-TESTA-FILE-STATUS                         
                 MOVE  ' '             TO      LDBR2-CARRO              
                 WRITE LIS-REGTO       FROM      LINBRA2                
           ELSE                                                         
           IF    ( WRKAA-LADO1      EQUAL   SPACES )                    
                 MOVE  WRKA-LADO1          TO        CB5AA-LADO1        
                 MOVE  WRKBB-LADO2         TO        CB5BB-LADO2        
                 WRITE LIS-REGTO           FROM      CABEC5A            
                 MOVE WRK-FS-LISTA         TO        WRK-FILE-STATUS    
                 PERFORM 0000-TESTA-FILE-STATUS                         
                 MOVE  SPACES              TO        CB6A-LADO1         
                 MOVE  WRKB-LADO2          TO        CB6B-LADO2         
                 WRITE LIS-REGTO           FROM      CABEC6             
                 MOVE WRK-FS-LISTA         TO        WRK-FILE-STATUS    
                 PERFORM 0000-TESTA-FILE-STATUS                         
           ELSE                                                         
           IF    ( WRKBB-LADO2      EQUAL   SPACES )                    
                 MOVE  WRKAA-LADO1         TO        CB5AA-LADO1        
                 MOVE  WRKB-LADO2          TO        CB5BB-LADO2        
                 WRITE LIS-REGTO           FROM      CABEC5A            
                 MOVE WRK-FS-LISTA         TO        WRK-FILE-STATUS    
                 PERFORM 0000-TESTA-FILE-STATUS                         
                 MOVE  SPACES              TO        CB6B-LADO2         
                 MOVE  WRKA-LADO1          TO        CB6A-LADO1         
                 WRITE LIS-REGTO           FROM      CABEC6             
                 MOVE WRK-FS-LISTA         TO        WRK-FILE-STATUS    
                 PERFORM 0000-TESTA-FILE-STATUS                         
           ELSE                                                         
                 MOVE  WRKAA-LADO1         TO        CB5AA-LADO1        
                 MOVE  WRKBB-LADO2         TO        CB5BB-LADO2        
                 WRITE LIS-REGTO           FROM      CABEC5A            
                 MOVE WRK-FS-LISTA         TO        WRK-FILE-STATUS    
                 PERFORM 0000-TESTA-FILE-STATUS                         
                 MOVE  WRKA-LADO1          TO        CB6A-LADO1         
                 MOVE  WRKB-LADO2          TO        CB6B-LADO2         
                 WRITE LIS-REGTO           FROM      CABEC6             
                 MOVE WRK-FS-LISTA         TO        WRK-FILE-STATUS    
                 PERFORM 0000-TESTA-FILE-STATUS.                        
                                                                        
           MOVE  '0'                   TO      LDBR2-CARRO.             
           WRITE LIS-REGTO           FROM      LINBRA2.                 
                                                                        
           MOVE  '0'                 TO        LCIF1-CARRO.             
           WRITE LIS-REGTO           FROM      LINHA-CIF1.              
           MOVE WRK-FS-LISTA         TO        WRK-FILE-STATUS.         
           PERFORM 0000-TESTA-FILE-STATUS.                              
                                                                        
           MOVE  ' '                 TO        LCIF1-CARRO.             
           WRITE LIS-REGTO           FROM      LINHA-CIF1.              
           MOVE WRK-FS-LISTA         TO        WRK-FILE-STATUS.         
           PERFORM 0000-TESTA-FILE-STATUS.                              
                                                                        
           WRITE LIS-REGTO           FROM      LINHA-CIF2.              
           MOVE WRK-FS-LISTA         TO        WRK-FILE-STATUS.         
           PERFORM 0000-TESTA-FILE-STATUS.                              
                                                                        
      *    WRITE LIS-REGTO           FROM      CABEC7.                  
      *    MOVE WRK-FS-LISTA         TO        WRK-FILE-STATUS.         
      *    PERFORM 0000-TESTA-FILE-STATUS.                              
                                                                        
           IF  WRK-TIPO   EQUAL   1                                     
               WRITE LIS-REGTO       FROM      CABEC9                   
               MOVE WRK-FS-LISTA     TO        WRK-FILE-STATUS          
               PERFORM 0000-TESTA-FILE-STATUS.                          
                                                                        
           MOVE  ' '                   TO      LDBR2-CARRO.             
           WRITE LIS-REGTO           FROM      LINBRA2.                 
                                                                        
       0000-IMPRI-EXTERNO-FIM.  EXIT.                                   
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       0000-GRAVA-ARQECT SECTION.                                       
                                                                        
           MOVE    WRK-NUMERO-LOTE-9003     TO    ECT-NUM-LOTE          
           MOVE    WRK-NUM-EXTRATO          TO    ECT-NUM-EXTRATO       
           MOVE    WRK-NUMCEP-POSTNET-9011  TO    ECT-NUM-CEP           
           MOVE    WRK-CPLCEP-POSTNET-9011  TO    ECT-SUF-CEP.          
           MOVE        'P'                  TO    ECT-TIPO-EXTRATO.     
                                                                        
           CALL 'POOL0160' USING WK-JOBNAME WK-VALORFAC                 
           MOVE WK-JOBNAME                  TO    ECT-JOBNAME           
           MOVE WRK-DATA-POSTAGEM-9002      TO    ECT-DT-FRANQ          
                                                                        
           WRITE REG-ARQECT.                                            
           MOVE WRK-FS-ARQECT         TO        WRK-FILE-STATUS.        
           PERFORM 0000-TESTA-FILE-STATUS.                              
                                                                        
       0000-GRAVA-ARQECT-FIM. EXIT.                                     
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       0000-MONTA-BARRA-CIF SECTION.                                    
                                                                        
           MOVE   'VRS001'               TO WRK-VERSAO-9010             
           MOVE   SPACES                 TO WRK-MENSAGEM-9010           
           MOVE       72                 TO WRK-CODIGO-DR-POSTAGEM-9010.
           MOVE     1953                 TO WRK-CODIGO-ADM-CONTR-9010.  
                                                                        
           IF  WRK-FLAG-9002  EQUAL  ZEROS                              
               PERFORM  0000-OBTEM-NUMERO-LOTE  THRU                    
                        0000-OBTEM-NUMERO-LOTE-FIM                      
               MOVE          1           TO WRK-FLAG-9002.              
                                                                        
           MOVE    WRK-NUMERO-LOTE-9003  TO WRK-NUMERO-LOTE-9010.       
           ADD          1                TO WRK-NUM-EXTRATO.            
           MOVE    WRK-NUM-EXTRATO       TO WRK-NUMERO-SEQ-OBJETO-9010. 
                                                                        
           IF  WRK-NUMCEP-POSTNET-9011  GREATER   999  AND              
               WRK-NUMCEP-POSTNET-9011     LESS 10000                   
               MOVE           1          TO WRK-CODIGO-DESTINO-9010     
           ELSE                                                         
               IF  WRK-NUMCEP-POSTNET-9011      GREATER  9999  AND      
                   WRK-NUMCEP-POSTNET-9011 NOT  GREATER 19999           
                   MOVE       2          TO WRK-CODIGO-DESTINO-9010     
               ELSE                                                     
                   MOVE       3          TO WRK-CODIGO-DESTINO-9010.    
                                                                        
           MOVE   ZEROS            TO WRK-CODIGO-RESERVA-9010.          
           MOVE DAT-DIA            TO WRK-DIA-POSTAGEM-9010.            
           MOVE DAT-MES            TO WRK-MES-POSTAGEM-9010.            
           MOVE DAT-ANO            TO WRK-ANO-POSTAGEM-9010.            
           MOVE     SPACES         TO WRK-LINHA-RETORNO-9010.           
                                                                        
           CALL   'BRAD9010'   USING   WRK-VERSAO-9010                  
                                       WRK-MENSAGEM-9010                
                                       WRK-LINHA-NUMERO-9010            
                                       WRK-LINHA-RETORNO-9010.          
                                                                        
           IF  RETURN-CODE  EQUAL  ZEROS                                
               NEXT SENTENCE                                            
           ELSE                                                         
               DISPLAY '*********** PTEX1152 **********'                
               DISPLAY '* PTEX1152-ERRO NO ACESSO AO  *'                
               DISPLAY '* MODULO BRAD9010.            *'                
               DISPLAY WRK-MENSAGEM-9010                                
               DISPLAY '*********** PTEX1152 **********'                
               CALL 'ILBOABN0'     USING WRK-ABEND.                     
                                                                        
           PERFORM  0000-GRAVA-ARQECT.                                  
                                                                        
       0000-MONTA-BARRA-CIF-FIM. EXIT.                                  
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       0000-OBTEM-NUMERO-LOTE SECTION.                                  
                                                                        
           MOVE 'VRS001'               TO WRK-VERSAO-9003.              
           MOVE SPACES                 TO WRK-MENSAGEM-9003.            
           MOVE DAT-DIA                TO WRK-DIA-POSTAGEM-9002.        
           MOVE DAT-MES                TO WRK-MES-POSTAGEM-9002.        
           MOVE DAT-ANO                TO WRK-ANO-POST.                 
           MOVE WRK-SS-POST            TO WRK-SEC-POSTAGEM-9002.        
           MOVE WRK-AA-POST            TO WRK-ANO-POSTAGEM-9002.        
           MOVE WRK-DATA-POSTAGEM-9002 TO WRK-DATA-POSTAGEM-9003        
           MOVE     ZEROS              TO WRK-NUMERO-LOTE-9003.         
                                                                        
           CALL WRK-ROTI9003   USING   WRK-VERSAO-9003                  
                                       WRK-MENSAGEM-9003                
                                       WRK-DATA-POSTAGEM-9003           
                                       WRK-NUMERO-LOTE-9003.            
                                                                        
           IF  RETURN-CODE  EQUAL  ZEROS                                
               NEXT SENTENCE                                            
           ELSE                                                         
               DISPLAY '*********** PTEX1152 **********'                
               DISPLAY '* PTEX1152-ERRO NO ACESSO AO  *'                
               DISPLAY '* MODULO ROTI9003.            *'                
               DISPLAY WRK-MENSAGEM-9003                                
               DISPLAY '*********** PTEX1152 **********'                
               CALL 'ILBOABN0'     USING WRK-ABEND.                     
                                                                        
       0000-OBTEM-NUMERO-LOTE-FIM. EXIT.                                
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       0000-OBTEM-BARRA-POSTNET SECTION.                                
                                                                        
           MOVE   'VRS001'               TO WRK-VERSAO-9011.            
           MOVE   SPACES                 TO WRK-MENSAGEM-9011.          
                                                                        
           CALL   'BRAD9011'   USING  WRK-VERSAO-9011                   
                                      WRK-MENSAGEM-9011                 
                                      WRK-CEP-POSTNET-9011              
                                      WRK-RETORNO-POSTNET-9011.         
                                                                        
           IF  RETURN-CODE  EQUAL  ZEROS                                
               NEXT SENTENCE                                            
           ELSE                                                         
               DISPLAY '*********** PTEX1152 **********'                
               DISPLAY '* PTEX1152-ERRO NO ACESSO AO  *'                
               DISPLAY '* MODULO BRAD9011.            *'                
               DISPLAY WRK-MENSAGEM-9011                                
               DISPLAY '*********** PTEX1152 **********'                
               CALL 'ILBOABN0'     USING WRK-ABEND.                     
                                                                        
       0000-OBTEM-BARRA-POSTNET-FIM. EXIT.                              
      *---------------------------------------------------------------* 
      *---------------------------------------------------------------* 

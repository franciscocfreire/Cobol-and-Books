      ***************************************************************** 
       ID  DIVISION.                                                    
      ***************************************************************** 
       PROGRAM-ID.                 CLLP0782.                            
      ***************************************************************** 
      *                                                               * 
      *  PROGRAMA    -   CLLP0782                                     * 
      *  ANALISTA    -   RENATO - EGM                                 * 
      *  DATA        -   MAIO/2010                                    * 
      *  OBJETIVO    -   EMITIR AVISO UNICO DE COBRANCA - LEASING     * 
      *                                                               * 
      *      ARQUIVOS:                                                * 
      *         DDNAME                           INCLUDE/BOOK         * 
      *         LADO1                                                 * 
      *         LADO2                                                 * 
      *         ARQPARM                                               * 
      *         DATATECL                                              * 
      *         LISTA                                                 * 
      *         LISTA1                                                * 
      *         ARQECT                           I#ROTIZA             * 
      *                                                               * 
      *---------------------------------------------------------------* 
RST   *                       A L T E R A C A O                       * 
RST   ***************************************************************** 
RST   *    ANALISTA    :    FABRICA STEFANINI       - STEFANINI       * 
RST   *    DATA        :    OUTUBRO, 2011                             * 
RST   *    OBJETIVO    :    TRATAR ALTERACAO DE CONTRATO DO FAC COM OS* 
RST   *                     COREIOS                                   * 
RST   *                                                               * 
RST   ***************************************************************** 
BSI   ***************************************************************** 
BSI   *                       A L T E R A C A O                       * 
BSI   ***************************************************************** 
BSI   *    ANALISTA    :    MARCUS VINIUS CURTO     - BSI             * 
BSI   *    DATA        :    ABRIL, 2013                               * 
BSI   *    OBJETIVO    :    INCLUIR GERACAO ARQUIVO CEDD              * 
BSI   *                                                               * 
BSI   ***************************************************************** 
       ENVIRONMENT DIVISION.                                            
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
                                                                        
BSI        SELECT  ARQCEDD   ASSIGN  TO  UT-S-ARQCEDD                   
BSI                FILE      STATUS IS WRK-FS-ARQCEDD.                  
                                                                        
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
                                                                        
       COPY I#ROTIZA.                                                   
                                                                        
      *---------------------------------------------------------------* 
      *      CADASTRO DO LPCL - ARQ. DE ENTRADA - LRECL : 0569        * 
      *---------------------------------------------------------------* 
                                                                        
       FD  LADO1       BLOCK 0                                          
                       RECORDING F                                      
                       LABEL RECORD STANDARD.                           
                                                                        
       01  FILLER          PIC X(569).                                  
                                                                        
      *---------------------------------------------------------------* 
      *      CADASTRO DO LPCL - ARQ. DE ENTRADA - LRECL : 0569        * 
      *---------------------------------------------------------------* 
                                                                        
       FD  LADO2       BLOCK 0                                          
                       RECORDING F                                      
                       LABEL RECORD STANDARD.                           
                                                                        
       01  FILLER          PIC X(569).                                  
                                                                        
      *---------------------------------------------------------------* 
      *      ARQUIVO DE DATAS - ARQ. DE ENTRADA - LRECL : 0006        * 
      *---------------------------------------------------------------* 
                                                                        
       FD  DATATECL    BLOCK 0                                          
                       RECORDING F                                      
                       LABEL RECORD STANDARD.                           
                                                                        
       01  FD-DAT-REGTO.                                                
           03  FD-DAT-DATA.                                             
               05  FD-DAT-DIA     PIC 9(02).                            
               05  FD-DAT-MES     PIC 9(02).                            
               05  FD-DAT-ANO     PIC 9(04).                            
                                                                        
      /                                                                 
                                                                        
      *---------------------------------------------------------------* 
      *      ARQUIVO DE PARAMETROS              - LRECL : 0006        * 
      *---------------------------------------------------------------* 
                                                                        
       FD  ARQPARM     BLOCK 0                                          
                       RECORDING F                                      
                       LABEL RECORD STANDARD.                           
                                                                        
       01  FD-REG-PARAMETRO.                                            
           03  FD-PARAMETROS.                                           
               05  FD-PARM-TAREFA     PIC X(08).                        
               05  FD-PARM-PROGRAMA   PIC X(08).                        
               05  FILLER             PIC X(06).                        
               05  FD-PARM-SEQUENCIA  PIC 9(01).                        
               05  FILLER             PIC X(01).                        
               05  FD-PARM-NR-DIAS    PIC 9(02).                        
               05  FD-PARM-VALOR      PIC 9(15).                        
               05  FILLER             PIC X(209).                       
                                                                        
      *---------------------------------------------------------------* 
      *      R E L A T O R I O  -  ARQ. DE SAIDA - LRECL : 0175       * 
      *---------------------------------------------------------------* 
                                                                        
       FD  LISTA       BLOCK 0                                          
                       RECORDING F                                      
                       LABEL RECORD STANDARD.                           
                                                                        
       01  FD-LIS-REGTO.                                                
           03  FILLER             PIC X(001).                           
           03  FILLER             PIC X(191).                           
                                                                        
                                                                        
      *---------------------------------------------------------------* 
      *      R E L A T O R I O  -  ARQ. DE SAIDA - LRECL : 0080       * 
      *---------------------------------------------------------------* 
                                                                        
       FD  LISTA1      BLOCK 0                                          
                       RECORDING F                                      
                       LABEL RECORD STANDARD.                           
                                                                        
       01  FD-LT1-REGTO.                                                
           03  FILLER             PIC X(01).                            
           03  FILLER             PIC X(79).                            
BSI                                                                     
BSI   *----------------------------------------------------------------*
BSI   * OUTPUT:       ARQUIVO PARA CEDD                                *
BSI   *               ORG. SEQUENCIAL   -   LRECL = 350                *
BSI   *               BOOK I#CLLPZF                                    *
BSI   *----------------------------------------------------------------*
BSI    FD  ARQCEDD                                                      
BSI        BLOCK 0                                                      
BSI        RECORDING F                                                  
BSI        LABEL RECORD STANDARD.                                       
BSI                                                                     
BSI        COPY I#CLLPZF.                                               
                                                                        
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
BSI    77  WRK-FS-ARQCEDD      PIC X(02)         VALUE SPACES.          
       77  WRK-FILE-STATUS     PIC X(02)         VALUE SPACES.          
       77  WRK-FUNCAO          PIC X(05)         VALUE SPACES.          
       77  WRK-NOME-ARQ        PIC X(08)         VALUE SPACES.          
       77  WRK-FAIXA01         PIC 9(03)         VALUE ZEROS.           
       77  WRK-FAIXA02         PIC 9(03)         VALUE ZEROS.           
       77  WRK-FAIXA03         PIC 9(03)         VALUE ZEROS.           
       77  WRK-FLAG-9002       PIC  9(01)        VALUE ZERO.            
       77  WRK-NUM-EXTRATO     PIC  9(11) COMP-3 VALUE ZEROS.           
       77  WRK-LPCL5011        PIC  X(08)        VALUE 'LPCL5011'.      
       77  WRK-NOME-CORREIO    PIC  X(58)        VALUE                  
           'TOTAIS DA MONTAGEM DE CARTAS DE COBRANCA - CORREIO NAO COR'.
       77  WRK-NOME-AGENCIA    PIC  X(58)        VALUE                  
           'TOTAIS DA MONTAGEM DE CARTAS DE COBRANCA - AGENCIA NAO COR'.
                                                                        
BSI    77  WRK-ENDENUM             PIC X(49)        VALUE SPACES.       
BSI    77  WRK-DESCOMP-5           PIC 9(05)        VALUE ZEROS.        
BSI    77  WRK-DESCOMP-3           PIC 9(03)        VALUE ZEROS.        
                                                                        
      *---------------------------------------------------------------* 
      *    AUXILIAR   (ILBOABN0)                                        
      *---------------------------------------------------------------* 
       77  WRK-ABEND           PIC S9(04) COMP   VALUE +1111.           
                                                                        
      *---------------------------------------------------------------* 
      *    AUXILIARES (FIM - DE - ARQUIVO)                              
      *---------------------------------------------------------------* 
       77  WRK-WRK-FIM-PARM        PIC X(01)         VALUE SPACES.      
       77  WRK-WRK-FIM-LADO1       PIC X(01)         VALUE SPACES.      
       77  WRK-WRK-FIM-LADO2       PIC X(01)         VALUE SPACES.      
RST                                                                     
RST   *---------------------------------------------------------------* 
RST   *   AREA DE COMINICACAO COM MODULO ECTS9010                     * 
RST   *---------------------------------------------------------------* 
RST                                                                     
RST    COPY ECTSW001.                                                   
RST                                                                     
RST    01  WRK-ECTS9010                     PIC X(08) VALUE 'ECTS9010'. 
                                                                        
       01  WRK-TRATA-CEP.                                               
           03 FILLER           PIC X(01)              VALUE SPACES.     
           03 WRK-TR-CEP       PIC 9(05)              VALUE ZEROS.      
           03 WRK-TR-CEP-SUF   PIC 9(03)              VALUE ZEROS.      
       01  WRK-TRATA-CEP-R REDEFINES WRK-TRATA-CEP PIC 9(09).           
                                                                        
      *---------------------------------------------------------------* 
      *                                                                 
      *---------------------------------------------------------------* 
       01  WRK-LITERAL1.                                                
           03  FILLER              PIC  X(42)        VALUE              
           '*Possibilidade de enquadramento no seguro:'.                
                                                                        
       01  WRK-LITERAL2.                                                
           03  FILLER              PIC  X(44)        VALUE              
           ' Morte/Invalidez/Desemprego. Consulte sua Ag'.              
           03  FILLER              PIC  X(01)        VALUE X'41'.       
           03  FILLER              PIC  X(04)        VALUE 'ncia'.      
                                                                        
       77  WRK-FLAG-ESQ        PIC X(01)         VALUE 'N'.             
       77  WRK-FLAG-DIR        PIC X(01)         VALUE 'N'.             
                                                                        
       77  WRK-AREA-AUX        PIC X(35)         VALUE SPACES.          
                                                                        
       77  WRK-TAMANHO-380             PIC  9(02) COMP  VALUE 40.       
                                                                        
      *---------------------------------------------------------------* 
      *    AUXILIARES (INDEXADORES)                                     
      *---------------------------------------------------------------* 
       77  WRK-IND1            PIC 9(02) COMP-3  VALUE ZEROS.           
       77  WRK-IND2            PIC 9(02) COMP-3  VALUE ZEROS.           
       77  WRK-IND-AUX         PIC 9(02) COMP-3  VALUE ZEROS.           
                                                                        
      *---------------------------------------------------------------* 
      *    ACUMULADORES                                                 
      *---------------------------------------------------------------* 
       77  WRK-VALOR-A         PIC 9(11)V99 COMP-3 VALUE ZEROS.         
       77  WRK-VALOR-B         PIC 9(11)V99 COMP-3 VALUE ZEROS.         
                                                                        
      *---------------------------------------------------------------* 
      *    DATA NO FORMATO SSAAMMDD                                     
      *---------------------------------------------------------------* 
                                                                        
       01  WRK-DT-SSAAMMDD     PIC 9(09) VALUE ZEROS.                   
       01  WRK-DT-SSAAMMDD-R  REDEFINES  WRK-DT-SSAAMMDD.               
           03 FILLER           PIC X(01).                               
           03 WRK-ANO-SAMD     PIC 9(04).                               
           03 WRK-MES-SAMD     PIC 9(02).                               
           03 WRK-DIA-SAMD     PIC 9(02).                               
                                                                        
       77  ACU-CARTAS          PIC 9(05)  COMP-3 VALUE ZEROS.           
       77  WRK-RESTART         PIC 9(05)  COMP-3 VALUE ZEROS.           
                                                                        
       77  WRK-JOBNAME          PIC X(08)         VALUE SPACES.         
       77  WRK-VALORFAC         PIC 9(07) COMP-3  VALUE ZEROS.          
                                                                        
       01  WRK-DATA-HORA.                                               
           03  FILLER           PIC X(07)         VALUE SPACES.         
           03  WRK-DT-AAAAMMDD  PIC 9(09)  COMP-3 VALUE ZEROS.          
           03  FILLER           PIC X(31)         VALUE SPACES.         
                                                                        
       01  WRK-DT-SIST         PIC 9(09) VALUE ZEROS.                   
       01  WRK-DT-SIST-R   REDEFINES   WRK-DT-SIST.                     
           03  FILLER          PIC X(01).                               
           03  WRK-ANO-SIST    PIC 9(04).                               
           03  WRK-MES-SIST    PIC 9(02).                               
           03  WRK-DIA-SIST    PIC 9(02).                               
                                                                        
       01  WRK-ANO-POST        PIC 9(04) VALUE ZEROS.                   
       01  FILLER        REDEFINES   WRK-ANO-POST.                      
           03  WRK-SS-POST     PIC 9(02).                               
           03  WRK-AA-POST     PIC 9(02).                               
                                                                        
BSI    01  WRK-DATA-FAC.                                                
BSI        03  WRK-DIA-FAC             PIC 9(02)  VALUE ZEROS.          
BSI        03  FILLER                  PIC X(01)  VALUE  '.'.           
BSI        03  WRK-MES-FAC             PIC 9(02)  VALUE ZEROS.          
BSI        03  FILLER                  PIC X(01)  VALUE  '.'.           
BSI        03  WRK-ANO-FAC             PIC 9(04)  VALUE ZEROS.          
BSI                                                                     
BSI    01  WRK-DATA-DB2.                                                
BSI        03  WRK-DIA-DB2             PIC 9(02)  VALUE ZEROS.          
BSI        03  FILLER                  PIC X(01)  VALUE  '.'.           
BSI        03  WRK-MES-DB2             PIC 9(02)  VALUE ZEROS.          
BSI        03  FILLER                  PIC X(01)  VALUE  '.'.           
BSI        03  WRK-ANO-DB2             PIC 9(04)  VALUE ZEROS.          
                                                                        
      *---------------------------------------------------------------* 
      *   CAMPOS UTILIZADOS PARA EXECUCAO DO CODIGO DE BARRAS         * 
      *---------------------------------------------------------------* 
                                                                        
       01  WRK-ROTI9003                     PIC X(08) VALUE 'ROTI9003'. 
                                                                        
       01  WRK-LINKAGE.                                                 
            10  WRK-VERSAO-9003             PIC X(06) VALUE SPACES.     
            10  WRK-MENSAGEM-9003           PIC X(83) VALUE SPACES.     
            10  WRK-DATA-POSTAGEM-9003      PIC 9(08) VALUE ZEROS.      
            10  WRK-NUMERO-LOTE-9003        PIC 9(05) VALUE ZEROS.      
                                                                        
       01  WRK-AREA-BRAD9002.                                           
           03  WRK-DATA-POSTAGEM-9002      PIC 9(08)  VALUE  ZEROS.     
           03  FILLER  REDEFINES  WRK-DATA-POSTAGEM-9002.               
               05  WRK-DIA-POSTAGEM-9002   PIC 9(02).                   
               05  WRK-MES-POSTAGEM-9002   PIC 9(02).                   
               05  WRK-SEC-POSTAGEM-9002   PIC 9(02).                   
               05  WRK-ANO-POSTAGEM-9002   PIC 9(02).                   
                                                                        
       01  WRK-AREA-BRAD9011.                                           
           03  WRK-VERSAO-9011             PIC X(06)  VALUE  'VRS001'.  
           03  WRK-MENSAGEM-9011           PIC X(83)  VALUE  SPACES.    
           03  WRK-CEP-POSTNET-9011        PIC 9(08)  VALUE  ZEROS.     
           03  FILLER  REDEFINES  WRK-CEP-POSTNET-9011.                 
               05  WRK-NUMCEP-POSTNET-9011 PIC 9(05).                   
               05  WRK-CPLCEP-POSTNET-9011 PIC 9(03).                   
           03  WRK-RETORNO-POSTNET-9011    PIC X(11)  VALUE SPACES.     
                                                                        
       01  WRK-AREA-BRAD9010.                                           
           03  WRK-VERSAO-9010             PIC X(06)  VALUE  'VRS001'.  
           03  WRK-MENSAGEM-9010           PIC X(83)  VALUE  SPACES.    
           03  WRK-CABEC-NUMERO-9010       PIC X(34)  VALUE SPACES.     
           03  FILLER  REDEFINES  WRK-CABEC-NUMERO-9010.                
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
                                                                        
           03  WRK-CABEC-RETORNO-9010           PIC X(20) VALUE SPACES. 
           03  FILLER  REDEFINES  WRK-CABEC-RETORNO-9010.               
               05  WRK-START-9010               PIC X(01).              
               05  WRK-NUMERO-RETORNO-9010      PIC X(18).              
               05  WRK-STOP-9010                PIC X(01).              
                                                                        
      *---------------------------------------------------------------* 
      *    C A R T O E S     -   (  D J D E )   -                       
      *---------------------------------------------------------------* 
       01  WRK-DJDE-INI.                                                
           03  FILLER             PIC X(186)     VALUE                  
           '1DJDE JDL=LWX0LQ,JDE=LW0782,END;'.                          
           03  FILLER              PIC X(01)     VALUE '1'.             
           03  WRK-INI-RESTART     PIC 9(05)     VALUE 00001.           
                                                                        
       01  WRK-DJDE-INT.                                                
           03  WRK-FORMS           PIC X(186)    VALUE SPACES.          
           03  FILLER              PIC X(01)     VALUE '1'.             
           03  WRK-INT-RESTART     PIC 9(05)     VALUE ZEROS.           
                                                                        
       01  WRK-DJDE-EXT.                                                
           03  FILLER              PIC X(186)    VALUE                  
           '1DJDE FORMS=CLLP01,END;'.                                   
           03  FILLER              PIC X(01)     VALUE '1'.             
           03  WRK-EXT-RESTART     PIC 9(05)     VALUE ZEROS.           
                                                                        
       01  WRK-DJDE-EXT1.                                               
           03  FILLER              PIC X(186)    VALUE                  
           '1DJDE FORMS=CLLP02,END;'.                                   
           03  FILLER              PIC X(01)     VALUE '1'.             
           03  WRK-EXT1-RESTART    PIC 9(05)     VALUE ZEROS.           
                                                                        
       01  WRK-DJDE-FIM.                                                
           03  FILLER               PIC X(186)    VALUE                 
           '1DJDE JDL=DFAULT,JDE=F163S,END;'.                           
           03  FILLER              PIC X(01)     VALUE '1'.             
           03  WRK-FIM-RESTART     PIC 9(05)     VALUE 99999.           
                                                                        
      *---------------------------------------------------------------* 
      *     AREA PARA CONTER O ARQ. LADO1 E LADO2 - LRECL : 0569      * 
      *---------------------------------------------------------------* 
                                                                        
       01  WRK-REGTO.                                                   
           02  WRK-CHAVE.                                               
               03  FILLER          PIC 9(09)     COMP-3 VALUE ZEROS.    
               03  FILLER          PIC 9(05)     COMP-3 VALUE ZEROS.    
               03  FILLER          PIC 9(03)     COMP-3 VALUE ZEROS.    
               03  FILLER          PIC 9(02)     COMP-3 VALUE ZEROS.    
               03  FILLER          PIC 9(09)     COMP-3 VALUE ZEROS.    
           02  WRK-EMPRESA         PIC 9(05)     COMP-3 VALUE ZEROS.    
           02  WRK-AGENCIA         PIC 9(05)     COMP-3 VALUE ZEROS.    
           02  WRK-CONTA           PIC 9(07)     COMP-3 VALUE ZEROS.    
           02  WRK-CART            PIC X(03)            VALUE SPACES.   
           02  WRK-CONTRATO        PIC 9(07)     COMP-3 VALUE ZEROS.    
           02  FILLER              PIC X(07)            VALUE SPACES.   
           02  WRK-NOME-RESP       PIC X(40)            VALUE SPACES.   
           02  WRK-CGCPFRESP.                                           
               03  WRK-CGCPF       PIC 9(09)     COMP-3 VALUE ZEROS.    
               03  WRK-FILIAL      PIC 9(05)     COMP-3 VALUE ZEROS.    
               03  WRK-CTR         PIC 9(03)     COMP-3 VALUE ZEROS.    
           02  WRK-END-RESP        PIC X(40)            VALUE SPACES.   
           02  WRK-DEV-NRO         PIC X(07)            VALUE SPACES.   
           02  WRK-DEV-COMPL       PIC X(20)            VALUE SPACES.   
           02  WRK-DEV-BAIRRO      PIC X(20)            VALUE SPACES.   
           02  WRK-CIDADE          PIC X(30)            VALUE SPACES.   
           02  WRK-EST             PIC X(02)            VALUE SPACES.   
           02  WRK-CEP             PIC 9(05)            VALUE ZEROS.    
           02  WRK-CEP-SUFIXO      PIC 9(03)            VALUE ZEROS.    
           02  WRK-IDENT           PIC 9(02)            VALUE ZEROS.    
           02  WRK-OCORRENCIAS OCCURS 7 TIMES.                          
               03  WRK-POSSIBIL    PIC X(01).                           
               03  WRK-NATUREZA    PIC X(02).                           
               03  WRK-VCMTO       PIC 9(09)     COMP-3.                
               03  WRK-VALOR       PIC 9(11)V99  COMP-3.                
               03  WRK-DESCRIC     PIC X(20).                           
           02  WRK-TIPO            PIC 9(01)            VALUE ZERO.     
           02  WRK-COD-ENT         PIC 9(01)            VALUE ZERO.     
           02  WRK-NOME-AGE        PIC X(20)            VALUE SPACES.   
           02  WRK-END-AGE         PIC X(25)            VALUE SPACES.   
           02  WRK-CEP-AG          PIC 9(09)     COMP-3 VALUE ZEROS.    
           02  WRK-EST-AG          PIC X(02)            VALUE SPACES.   
           02  WRK-MUNICIPIO       PIC X(14)            VALUE SPACES.   
           02  WRK-CD              PIC 9(05)     COMP-3 VALUE ZEROS.    
           02  WRK-COD-BARRA       PIC X(29)            VALUE SPACES.   
           02  WRK-NRO-SEQ         PIC 9(07)     COMP-3 VALUE ZEROS.    
                                                                        
                                                                        
       01  FILLER                  PIC X(38)     VALUE                  
           '*** FIM DA WORKING-STORAGE SECTION ***'.                    
                                                                        
      *---------------------------------------------------------------* 
      *    C A R T A  (LADO1 E LADO2 - PARTE EXTERNA)                 * 
      *---------------------------------------------------------------* 
       01  CABEC-DD.                                                    
           03  FILLER                 PIC X(02)   VALUE '3'.            
           03  CB-CD-LADO1.                                             
               05  FILLER             PIC X(41)   VALUE SPACES.         
               05  CB-CD-FIXO1.                                         
                   07  CB-CD1-BRANCO1 PIC X(19)   VALUE SPACES.         
                   07  CB-CD1-CD      PIC X(03)   VALUE SPACES.         
                   07  CB-CD1-NRO-CD  PIC 9(05)   VALUE ZEROS.          
           03  FILLER                 PIC X(33)   VALUE SPACES.         
           03  CB-CD-LADO2.                                             
               05  FILLER             PIC X(42)   VALUE SPACES.         
               05  CB-CD-FIXO2.                                         
                   07  CB-CD2-BRANCO1 PIC X(19)   VALUE SPACES.         
                   07  CB-CD2-CD      PIC X(03)   VALUE SPACES.         
                   07  CB-CD2-NRO-CD  PIC 9(05)   VALUE ZEROS.          
           03  FILLER                 PIC X(14)   VALUE SPACES.         
           03  FILLER                 PIC X(01)   VALUE '1'.            
           03  CB-CD-RESTART          PIC 9(05)   VALUE ZEROS.          
                                                                        
      /                                                                 
       01  CABEC-1.                                                     
           03  FILLER                 PIC X(02)     VALUE '5'.          
           03  CB-1A-LADO1.                                             
               05  CB-1A-END-AG       PIC X(26)     VALUE SPACES.       
               05  FILLER             PIC X(19)     VALUE SPACES.       
               05  CB-1A-FIXO1.                                         
                   07  CB-1A-ECT      PIC X(27)     VALUE SPACES.       
               05  CB-1A-FIXO2   REDEFINES   CB-1A-FIXO1.               
                   07  CB-1A-BRANCO1  PIC X(19).                        
                   07  CB-1A-CD       PIC X(03).                        
                   07  CB-1A-NRO-CD   PIC 9(05).                        
               05  CB-1A-FIXO3  REDEFINES    CB-1A-FIXO2.               
                   07  CB-1A-BRANCO2  PIC X(27).                        
               05  CB-1A-FIXO4  REDEFINES    CB-1A-FIXO3.               
                   07  CB-1A-BRANCO3  PIC X(04).                        
                   07  CB-1A-FRANQUIA PIC X(23).                        
               05  CB-1A-FIXO5  REDEFINES    CB-1A-FIXO4.               
                   07  CB-1A-CEP-IR   PIC X(27).                        
           03  FILLER                 PIC X(31)   VALUE SPACES.         
           03  CB-1B-LADO2.                                             
               05  CB-1B-END-AG       PIC X(26)  VALUE SPACES.          
               05  FILLER             PIC X(19)  VALUE SPACES.          
               05  CB-1B-FIXO1.                                         
                   07  CB-1B-ECT      PIC X(27)  VALUE SPACES.          
               05  CB-1B-FIXO2  REDEFINES  CB-1B-FIXO1.                 
                   07  CB-1B-BRANCO1  PIC X(19).                        
                   07  CB-1B-CD       PIC X(03).                        
                   07  CB-1B-NRO-CD   PIC 9(05).                        
               05  CB-1B-FIXO3  REDEFINES  CB-1B-FIXO2.                 
                   07  CB-1B-BRANCO2  PIC X(27).                        
               05  CB-1B-FIXO4  REDEFINES  CB-1B-FIXO3.                 
                   07  CB-1B-BRANCO3  PIC X(04).                        
                   07  CB-1B-FRANQUIA PIC X(23).                        
               05  CB-1B-FIXO5  REDEFINES  CB-1B-FIXO4.                 
                   07  CB-1B-CEP-IR   PIC X(27).                        
           03  FILLER                 PIC X(09)   VALUE SPACES.         
           03  FILLER                 PIC X(01)   VALUE '1'.            
           03  CB-1B-RESTART          PIC 9(05)  VALUE ZEROS.           
                                                                        
      /                                                                 
       01  CABEC-2.                                                     
           03  CB-2-CARRO             PIC X(02)   VALUE ' '.            
           03  CB-2A-LADO1.                                             
               05  CB-2A-CEP          PIC 9(05)   VALUE ZEROS.          
               05  CB-2A-HIFEN        PIC X(01)   VALUE SPACES.         
               05  CB-2A-AG-SUFIXO    PIC 9(03)   VALUE ZEROS.          
               05  FILLER             PIC X(01)   VALUE SPACES.         
               05  CB-2A-MUNICIPIO    PIC X(13)   VALUE SPACES.         
               05  FILLER             PIC X(01)   VALUE SPACES.         
               05  CB-2A-SIGLA        PIC X(02)   VALUE SPACES.         
               05  FILLER             PIC X(13)   VALUE SPACES.         
               05  CB-2A-CEP-IR       PIC X(27)   VALUE SPACES.         
           03  FILLER                 PIC X(37)   VALUE SPACES.         
           03  CB-2B-LADO2.                                             
               05  CB-2B-CEP          PIC 9(05)   VALUE ZEROS.          
               05  CB-2B-HIFEN        PIC X(01)   VALUE SPACES.         
               05  CB-2B-AG-SUFIXO    PIC 9(03)   VALUE ZEROS.          
               05  FILLER             PIC X(01)   VALUE SPACES.         
               05  CB-2B-MUNICIPIO    PIC X(13)   VALUE SPACES.         
               05  FILLER             PIC X(01)   VALUE SPACES.         
               05  CB-2B-SIGLA        PIC X(02)   VALUE SPACES.         
               05  FILLER             PIC X(13)   VALUE SPACES.         
               05  CB-2B-CEP-IR       PIC X(27)   VALUE SPACES.         
           03  FILLER                 PIC X(15)   VALUE SPACES.         
           03  FILLER                 PIC X(01)   VALUE '1'.            
           03  CB-2B-RESTART          PIC 9(05)   VALUE ZEROS.          
                                                                        
       01  CABEC-3.                                                     
           03  CB-3-CARRO             PIC X(02)   VALUE '0'.            
           03  CB-3A-LADO1.                                             
               05  CB-3A-AGEN         PIC 9(04)   VALUE ZEROS.          
               05  CB-3A-HIFEN        PIC X(01)   VALUE SPACE.          
               05  CB-3A-NOME-AG      PIC X(20)   VALUE SPACES.         
               05  FILLER             PIC X(16)   VALUE SPACES.         
           03  CB-3A-ROTINA           PIC X(24)   VALUE SPACES.         
           03  CB-3A-NRO-SEQ          PIC 9(07)   VALUE ZEROS.          
           03  FILLER                 PIC X(31)   VALUE SPACES.         
           03  CB-3B-LADO2.                                             
               05  CB-3B-AGEN         PIC 9(04)   VALUE ZEROS.          
               05  CB-3B-HIFEN        PIC X(01)   VALUE SPACE.          
               05  CB-3B-NOME-AG      PIC X(20)   VALUE SPACES.         
               05  FILLER             PIC X(16)   VALUE SPACES.         
           03  CB-3B-ROTINA           PIC X(24)   VALUE SPACES.         
           03  CB-3B-NRO-SEQ          PIC 9(07)   VALUE ZEROS.          
           03  FILLER                 PIC X(09)   VALUE SPACES.         
           03  FILLER                 PIC X(01)   VALUE '1'.            
           03  CB-3B-RESTART          PIC 9(05)   VALUE ZEROS.          
                                                                        
       01  CABEC-POSTNET1.                                              
           05  FILLER                  PIC  X(01)  VALUE '6'.           
           05  FILLER                  PIC  X(58)  VALUE  SPACES.       
           05  FILLER                  PIC  X(74)  VALUE  SPACES.       
           05  FILLER                  PIC  X(53) VALUE  SPACES.        
           05  FILLER                  PIC  X(01)  VALUE '6'.           
           05  CB-LPOS1-SEQ-RESTART    PIC  9(05)  VALUE ZEROS.         
                                                                        
       01  CABEC-POSTNET2.                                              
           03  FILLER                  PIC  X(01)  VALUE '0'.           
           03 FILLER.                                                   
              05  FILLER                  PIC  X(18)  VALUE  SPACES.    
              05  CB-LPOS2-AREA-POSTNET-E PIC  X(11)  VALUE  SPACES.    
              05  FILLER                  PIC  X(73)  VALUE  SPACES.    
           03 FILLER.                                                   
              05  CB-LPOS2-AREA-POSTNET-D PIC  X(11)  VALUE  SPACES.    
           03  FILLER                     PIC  X(72)  VALUE  SPACES.    
           03  FILLER                     PIC  X(01)  VALUE '3'.        
           03  CB-LPOS2-SEQ-RESTART       PIC  9(05)  VALUE ZEROS.      
                                                                        
       01  CABEC-CIF1.                                                  
           03  CB-LCIF1-CARRO          PIC  X(01)  VALUE ' '.           
           03 FILLER.                                                   
              05  FILLER               PIC  X(08)  VALUE  ALL '{'.      
              05  CB-LCIF1-AREA-E      PIC  X(20)  VALUE SPACES.        
           03 FILLER.                                                   
              05  FILLER               PIC  X(33)  VALUE  ALL '{'.      
              05  CB-LCIF1-AREA-D      PIC  X(20)  VALUE SPACES.        
           03  FILLER                  PIC  X(104) VALUE  SPACES.       
           03  FILLER                  PIC  X(01)  VALUE '4'.           
           03  CB-LCIF1-SEQ-RESTART    PIC  9(05)  VALUE ZEROS.         
                                                                        
       01  CABEC-CIF2.                                                  
           03  FILLER                  PIC  X(01)  VALUE '-'.           
           03 FILLER.                                                   
              05  FILLER               PIC  X(13)  VALUE  SPACES.       
              05  CB-LCIF2-AREA-E      PIC  X(34) VALUE SPACES.         
              05  FILLER               PIC  X(54) VALUE  SPACES.        
           03 FILLER.                                                   
              05  CB-LCIF2-AREA-D      PIC  X(34) VALUE SPACES.         
              05  FILLER               PIC  X(50) VALUE  SPACES.        
           03  FILLER                  PIC  X(01)  VALUE '5'.           
           03  CB-LCIF2-SEQ-RESTART    PIC  9(05)  VALUE ZEROS.         
                                                                        
       01  CABEC-4.                                                     
           03  FILLER              PIC X(01)     VALUE '0'.             
           03  CB-4A-LADO1.                                             
               05  FILLER          PIC X(12)     VALUE SPACES.          
               05  CB-4A-NOME-RES  PIC X(40)     VALUE SPACES.          
           03  FILLER              PIC X(40)     VALUE SPACES.          
           03  CB-4B-LADO2.                                             
               05  CB-4B-NOME-RES  PIC X(40)     VALUE SPACES.          
           03  FILLER              PIC X(53)     VALUE SPACES.          
           03  FILLER              PIC X(01)     VALUE X'FA'.           
           03  CB-4B-RESTART       PIC 9(05)     VALUE ZEROS.           
                                                                        
       01  CABEC-5.                                                     
           03  FILLER              PIC X(01)     VALUE ' '.             
           03  CB-5A-LADO1.                                             
               05  FILLER          PIC X(12)     VALUE SPACES.          
               05  CB-5A-END-RES   PIC X(49)     VALUE SPACES.          
               05  FILLER          PIC X(23)     VALUE SPACES.          
           03  FILLER              PIC X(08)     VALUE SPACES.          
           03  CB-5B-LADO2.                                             
               05  CB-5B-END-RES   PIC X(49)     VALUE SPACES.          
               05  FILLER          PIC X(23)     VALUE SPACES.          
           03  FILLER              PIC X(21)     VALUE SPACES.          
           03  FILLER              PIC X(01)     VALUE X'FA'.           
           03  CB-5B-RESTART       PIC 9(05)     VALUE ZEROS.           
                                                                        
       01  CABEC-5A.                                                    
           03  FILLER              PIC X(01)     VALUE ' '.             
           03  CB-5AA-LADO1.                                            
               05  FILLER          PIC X(12)     VALUE SPACES.          
               05  CB-5A-BAI-RES   PIC X(20)     VALUE SPACES.          
               05  FILLER          PIC X(01)     VALUE SPACES.          
               05  CB-5A-COM-RES   PIC X(20)     VALUE SPACES.          
               05  FILLER          PIC X(01)     VALUE SPACES.          
               05  FILLER          PIC X(19)     VALUE SPACES.          
               05  FILLER          PIC X(11)     VALUE SPACES.          
           03  FILLER              PIC X(08)     VALUE SPACES.          
           03  CB-5BB-LADO2.                                            
               05  CB-5B-BAI-RES   PIC X(20)     VALUE SPACES.          
               05  FILLER          PIC X(01)     VALUE SPACES.          
               05  CB-5B-COM-RES   PIC X(20)     VALUE SPACES.          
               05  FILLER          PIC X(01)     VALUE SPACES.          
               05  FILLER          PIC X(19)     VALUE SPACES.          
               05  FILLER          PIC X(15)     VALUE SPACES.          
           03  FILLER              PIC X(17)     VALUE SPACES.          
           03  FILLER              PIC X(01)     VALUE X'FA'.           
           03  CB-5BB-RESTART      PIC 9(05)     VALUE ZEROS.           
                                                                        
      /                                                                 
                                                                        
       01  WRK-AA-LADO1.                                                
           03  FILLER          PIC X(12)     VALUE SPACES.              
           03  WRK-A-BAI-RES   PIC X(20)     VALUE SPACES.              
           03  FILLER          PIC X(01)     VALUE SPACES.              
           03  WRK-A-COM-RES   PIC X(20)     VALUE SPACES.              
           03  FILLER          PIC X(01)     VALUE SPACES.              
           03  FILLER          PIC X(19)     VALUE SPACES.              
           03  FILLER          PIC X(10)     VALUE SPACES.              
                                                                        
       01  WRK-BB-LADO2.                                                
           03  WRK-B-BAI-RES   PIC X(20)     VALUE SPACES.              
           03  FILLER          PIC X(01)     VALUE SPACES.              
           03  WRK-B-COM-RES   PIC X(20)     VALUE SPACES.              
           03  FILLER          PIC X(01)     VALUE SPACES.              
           03  FILLER          PIC X(19)     VALUE SPACES.              
           03  FILLER          PIC X(11)     VALUE SPACES.              
      /                                                                 
                                                                        
      /                                                                 
       01  CABEC-6.                                                     
           03  FILLER               PIC X(01)     VALUE ' '.            
           03  CB-6A-LADO1.                                             
               05  FILLER           PIC X(12)     VALUE SPACES.         
               05  CB-6A-CEP        PIC 9(05)     VALUE ZEROS.          
               05  CB-6A-HIFEN      PIC X(01)     VALUE SPACES.         
               05  CB-6A-CEP-SUFIXO PIC 9(03)     VALUE ZEROS.          
               05  FILLER           PIC X(01)     VALUE SPACES.         
               05  CB-6A-CIDADE     PIC X(33)     VALUE SPACES.         
           03  FILLER               PIC X(37)     VALUE SPACES.         
           03  CB-6B-LADO2.                                             
               05  CB-6B-CEP        PIC 9(05)     VALUE ZEROS.          
               05  CB-6B-HIFEN      PIC X(01)     VALUE SPACES.         
               05  CB-6B-CEP-SUFIXO PIC 9(03)     VALUE ZEROS.          
               05  FILLER           PIC X(01)     VALUE SPACES.         
               05  CB-6B-CIDADE     PIC X(33)     VALUE SPACES.         
           03  FILLER               PIC X(50)     VALUE SPACES.         
           03  FILLER               PIC X(01)     VALUE X'FA'.          
           03  CB-6B-RESTART        PIC 9(05)     VALUE ZEROS.          
                                                                        
       01  WRK-A-LADO1.                                                 
           03  FILLER           PIC X(12)     VALUE SPACES.             
           03  WRK-A-CEP        PIC 9(05)     VALUE ZEROS.              
           03  WRK-A-HIFEN      PIC X(01)     VALUE SPACES.             
           03  WRK-A-CEP-SUFIXO PIC 9(03)     VALUE ZEROS.              
           03  FILLER           PIC X(01)     VALUE SPACES.             
           03  WRK-A-CIDADE     PIC X(33)     VALUE SPACES.             
                                                                        
       01  WRK-B-LADO2.                                                 
           03  WRK-B-CEP        PIC 9(05)     VALUE ZEROS.              
           03  WRK-B-HIFEN      PIC X(01)     VALUE SPACES.             
           03  WRK-B-CEP-SUFIXO PIC 9(03)     VALUE ZEROS.              
           03  FILLER           PIC X(01)     VALUE SPACES.             
           03  WRK-B-CIDADE     PIC X(33)     VALUE SPACES.             
      /                                                                 
       01  CABEC-7.                                                     
           03  FILLER              PIC X(01)     VALUE ' '.             
           03  CB-7A-LADO1.                                             
               05  FILLER          PIC X(28)     VALUE SPACES.          
               05  CB-7A-DD-POST   PIC 9(02)     VALUE ZEROS.           
               05  CB-7A-BARRA1    PIC X(01)     VALUE SPACES.          
               05  CB-7A-MM-POST   PIC 9(02)     VALUE ZEROS.           
               05  CB-7A-BARRA2    PIC X(01)     VALUE SPACES.          
               05  CB-7A-AA-POST   PIC 9(04)     VALUE ZEROS.           
           03  FILLER              PIC X(28)     VALUE SPACES.          
           03  CB-7B-LADO2.                                             
               05  FILLER          PIC X(50)     VALUE SPACES.          
               05  CB-7B-DD-POST   PIC 9(02)     VALUE ZEROS.           
               05  CB-7B-BARRA1    PIC X(01)     VALUE SPACES.          
               05  CB-7B-MM-POST   PIC 9(02)     VALUE ZEROS.           
               05  CB-7B-BARRA2    PIC X(01)     VALUE SPACES.          
               05  CB-7B-AA-POST   PIC 9(04)     VALUE ZEROS.           
           03  FILLER              PIC X(43)     VALUE SPACES.          
           03  FILLER              PIC X(16)     VALUE SPACES.          
           03  FILLER              PIC X(01)     VALUE X'FB'.           
           03  CB-7B-RESTART       PIC 9(05)     VALUE ZEROS.           
                                                                        
      *---------------------------------------------------------------* 
      *    C A R T A  (LADOA E LADOB - PARTE INTERNA)                 * 
      *---------------------------------------------------------------* 
       01  CABEC-IN1.                                                   
           03 FILLER               PIC X(01)     VALUE '2'.             
           03 CB-LD1-LADOA.                                             
              05 FILLER            PIC X(02)     VALUE SPACES.          
              05 CB-LD1A-NOME-RES  PIC X(40)     VALUE SPACES.          
              05 FILLER            PIC X(30)     VALUE SPACES.          
           03 FILLER               PIC X(34)     VALUE SPACES.          
           03 CB-LD1-LADOB.                                             
              05 CB-LD1B-NOME-RES  PIC X(40)     VALUE SPACES.          
              05 FILLER            PIC X(21)     VALUE SPACES.          
           03 FILLER               PIC X(18)     VALUE SPACES.          
           03 FILLER               PIC X(01)     VALUE '1'.             
           03 CB-LD1-RESTART       PIC 9(05)     VALUE ZEROS.           
                                                                        
                                                                        
       01  CABEC-IN2.                                                   
           03 CB-LD2-CABEC OCCURS 7 TIMES.                              
              05 CB-LD2-CARRO         PIC X(01).                        
              05 CB-LD2-LADOA.                                          
                 07 FILLER            PIC X(02).                        
                 07 CB-LD2A-POSSIBIL  PIC X(01).                        
                 07 FILLER            PIC X(01).                        
                 07 CB-LD2A-COD-NAT   PIC X(02).                        
                 07 CB-LD2A-HIFEN     PIC X(03).                        
                 07 CB-LD2A-DESCRIC   PIC X(20)B(10).                   
                 07 CB-LD2A-DATA1.                                      
                    10 CB-LD2A-DIA     PIC 9(02).                       
                    10 CB-LD2A-BAR1    PIC X(01).                       
                    10 CB-LD2A-MES     PIC 9(02).                       
                    10 CB-LD2A-BAR2    PIC X(01).                       
                    10 CB-LD2A-ANO     PIC 9(04)B(08).                  
                 07 CB-LD2A-VALOR      PIC ZZZZZ.ZZZ.ZZ9,99.            
                 07 FILLER             PIC X(33).                       
              05 CB-LD2-LADOB.                                          
                 07 CB-LD2B-POSSIBIL   PIC X(01).                       
                 07 FILLER             PIC X(01).                       
                 07 CB-LD2B-COD-NAT    PIC X(02).                       
                 07 CB-LD2B-HIFEN      PIC X(03).                       
                 07 CB-LD2B-DESCRIC    PIC X(20)B(10).                  
                 07 CB-LD2B-DATA1.                                      
                    10 CB-LD2B-DIA     PIC 9(02).                       
                    10 CB-LD2B-BAR1    PIC X(01).                       
                    10 CB-LD2B-MES     PIC 9(02).                       
                    10 CB-LD2B-BAR2    PIC X(01).                       
                    10 CB-LD2B-ANO     PIC 9(04)B(08).                  
                 07 CB-LD2B-VALOR      PIC ZZZZZ.ZZZ.ZZ9,99.            
                 07 FILLER             PIC X(08).                       
              05 CB-CB-LD2-FONTINDEX   PIC X(01).                       
              05 CB-LD2-RESTART        PIC 9(05).                       
                                                                        
       01  CABEC-IN3.                                                   
           03 FILLER                PIC X(01)     VALUE '7'.            
           03 CB-LD3-LADOA.                                             
              05 FILLER             PIC X(56)     VALUE SPACES.         
              05 CB-LD3A-TOT-DIVIDA PIC ZZ.ZZZ.ZZZ.ZZ9,99               
                                                  VALUE ZEROS.          
           03 FILLER                PIC X(87)     VALUE SPACES.         
           03 CB-LD3-LADOB.                                             
              05 CB-LD3B-TOT-DIVIDA PIC ZZ.ZZZ.ZZZ.ZZ9,99               
                                                  VALUE ZEROS.          
           03 FILLER                PIC X(08)     VALUE SPACES.         
           03 FILLER                PIC X(01)     VALUE '1'.            
           03 CB-LD3-RESTART        PIC 9(05)     VALUE ZEROS.          
                                                                        
                                                                        
                                                                        
       01  CABEC-IN4.                                                   
           03 FILLER               PIC X(01)     VALUE '8'.             
           03 CB-LD4-LADOA.                                             
              05 FILLER            PIC X(02)     VALUE SPACES.          
              05 CB-LD4A-CID-AGE   PIC X(14)     VALUE SPACES.          
              05 FILLER            PIC X(06)     VALUE SPACES.          
              05 CB-LD4A-DIA       PIC 9(02)     VALUE ZEROS.           
              05 CB-LD4A-BARRA1    PIC X(01)     VALUE '/'.             
              05 CB-LD4A-MES       PIC 9(02)     VALUE ZEROS.           
              05 CB-LD4A-BARRA2    PIC X(01)     VALUE '/'.             
              05 CB-LD4A-ANO       PIC 9(04)     VALUE ZEROS.           
           03 FILLER               PIC X(74)     VALUE SPACES.          
           03 CB-LD4-LADOB.                                             
              05 CB-LD4B-CID-AGE   PIC X(14)     VALUE SPACES.          
              05 FILLER            PIC X(06)     VALUE SPACES.          
              05 CB-LD4B-DIA       PIC 9(02)     VALUE ZEROS.           
              05 CB-LD4B-BARRA1    PIC X(01)     VALUE '/'.             
              05 CB-LD4B-MES       PIC 9(02)     VALUE ZEROS.           
              05 CB-LD4B-BARRA2    PIC X(01)     VALUE '/'.             
              05 CB-LD4B-ANO       PIC 9(04)     VALUE ZEROS.           
           03 FILLER               PIC X(49)     VALUE SPACES.          
           03 FILLER               PIC X(01)     VALUE '1'.             
           03 CB-LD4-RESTART       PIC 9(05)     VALUE ZEROS.           
                                                                        
       01  CABEC-IN6.                                                   
           03 FILLER               PIC X(01)     VALUE ' '.             
           03 CB-LD6-LADOA.                                             
              05 FILLER            PIC X(02)     VALUE SPACES.          
              05 CB-LD6A-NRO-SEQ   PIC 9(07)     VALUE ZEROS.           
              05 FILLER            PIC X(97)     VALUE SPACES.          
           03 CB-LD6-LADOB.                                             
              05 CB-LD6B-NRO-SEQ   PIC 9(07)     VALUE ZEROS.           
              05 FILLER            PIC X(72)     VALUE SPACES.          
           03 FILLER               PIC X(01)     VALUE '1'.             
           03 CB-LD6-RESTART       PIC 9(05)     VALUE ZEROS.           
                                                                        
       01  CABEC-IN7.                                                   
           03 FILLER               PIC X(01)     VALUE '0'.             
           03 CB-LD7-LADOA.                                             
              05 FILLER            PIC X(02)     VALUE SPACES.          
              05 CB-LD7A-LITERAL1  PIC X(42)     VALUE SPACES.          
              05 FILLER            PIC X(47)     VALUE SPACES.          
           03 CB-LD7-LADOB.                                             
              05 CB-LD7B-LITERAL1  PIC X(42)     VALUE SPACES.          
              05 FILLER            PIC X(52)     VALUE SPACES.          
           03 FILLER               PIC X(01)     VALUE '9'.             
           03 CB-LD7-RESTART       PIC 9(05)     VALUE ZEROS.           
                                                                        
       01  CABEC-IN8.                                                   
           03 FILLER               PIC X(01)     VALUE ' '.             
           03 CB-LD8-LADOA.                                             
              05 FILLER            PIC X(02)     VALUE SPACES.          
              05 CB-LD8A-LITERAL2  PIC X(49)     VALUE SPACES.          
              05 FILLER            PIC X(40)     VALUE SPACES.          
           03 CB-LD8-LADOB.                                             
              05 CB-LD8B-LITERAL2  PIC X(49)     VALUE SPACES.          
              05 FILLER            PIC X(45)     VALUE SPACES.          
           03 FILLER               PIC X(01)     VALUE '9'.             
           03 CB-LD8-RESTART       PIC 9(05)     VALUE ZEROS.           
                                                                        
       01  CABEC-LBR1.                                                  
           03  FILLER              PIC X(01)     VALUE '1'.             
           03  FILLER              PIC X(185)    VALUE SPACES.          
           03  FILLER              PIC X(01)     VALUE '1'.             
           03  CB-LIN1-RESTART     PIC 9(05)     VALUE ZEROS.           
                                                                        
       01  CABEC-LBR2.                                                  
           03  CB-LDBR2-CARRO      PIC X(01)     VALUE ' '.             
           03  FILLER              PIC X(185)    VALUE SPACES.          
           03  FILLER              PIC X(01)     VALUE '1'.             
           03  CB-LIN2-RESTART     PIC 9(05)     VALUE ZEROS.           
                                                                        
       01  CB-LINBRA3.                                                  
           03  FILLER              PIC X(01)     VALUE '+'.             
           03  FILLER              PIC X(185)    VALUE SPACES.          
           03  FILLER              PIC X(01)     VALUE X'FB'.           
           03  CB-LIN3-RESTART     PIC 9(05)     VALUE ZEROS.           
                                                                        
      *---------------------------------------------------------------* 
      *       R E L A T O R I O   -   LISTA1   -   LRECL: 0080        * 
      *---------------------------------------------------------------* 
       01  CABEC-1-L.                                                   
           03  FILLER              PIC X(01)     VALUE '1'.             
           03  FILLER              PIC X(08)     VALUE 'LPCL9636'.      
                                                                        
       01  CABEC-TOT1.                                                  
           03  FILLER              PIC X(01)     VALUE '0'.             
           03  CB-LT1-NOME         PIC X(58)     VALUE SPACES.          
           03  FILLER              PIC X(06)     VALUE                  
           ' .....'.                                                    
           03  CB-LT1-CARTAS       PIC ZZZ99     VALUE ZEROS.           
                                                                        
      /                                                                 
      ***************************************************************** 
       PROCEDURE DIVISION.                                              
      ***************************************************************** 
       0000-INICIO                SECTION.                              
                                                                        
           OPEN  INPUT  ARQPARM  LADO1  LADO2  DATATECL                 
BSI             OUTPUT  LISTA  LISTA1  ARQECT                           
BSI                     ARQCEDD.                                        
                                                                        
           MOVE 'OPEN'              TO        WRK-FUNCAO.               
                                                                        
           MOVE WRK-FS-PARM         TO        WRK-FILE-STATUS.          
           MOVE 'ARQPARM'           TO        WRK-NOME-ARQ.             
           PERFORM 0100-TESTA-FILE-STATUS.                              
                                                                        
           MOVE WRK-FS-LADO1        TO        WRK-FILE-STATUS.          
           MOVE 'LADO1'             TO        WRK-NOME-ARQ.             
           PERFORM 0100-TESTA-FILE-STATUS.                              
                                                                        
           MOVE WRK-FS-LADO2        TO        WRK-FILE-STATUS.          
           MOVE 'LADO2'             TO        WRK-NOME-ARQ.             
                                                                        
           MOVE WRK-FS-DATATECL     TO        WRK-FILE-STATUS.          
           MOVE 'DATATECL'          TO        WRK-NOME-ARQ.             
           PERFORM 0100-TESTA-FILE-STATUS.                              
                                                                        
           MOVE WRK-FS-LISTA        TO        WRK-FILE-STATUS.          
           MOVE 'LISTA'             TO        WRK-NOME-ARQ.             
           PERFORM 0100-TESTA-FILE-STATUS.                              
                                                                        
           MOVE WRK-FS-LISTA1       TO        WRK-FILE-STATUS.          
           MOVE 'LISTA1'            TO        WRK-NOME-ARQ.             
           PERFORM 0100-TESTA-FILE-STATUS.                              
                                                                        
           MOVE WRK-FS-ARQECT       TO        WRK-FILE-STATUS.          
           MOVE 'ARQECT'            TO        WRK-NOME-ARQ.             
           PERFORM 0100-TESTA-FILE-STATUS.                              
                                                                        
BSI        MOVE WRK-FS-ARQCEDD      TO        WRK-FILE-STATUS.          
BSI        MOVE 'ARQCEDD'           TO        WRK-NOME-ARQ.             
BSI        PERFORM 0100-TESTA-FILE-STATUS.                              
                                                                        
           WRITE FD-LIS-REGTO          FROM      WRK-DJDE-INI.          
                                                                        
           MOVE 'WRITE'             TO        WRK-FUNCAO.               
           MOVE WRK-FS-LISTA        TO        WRK-FILE-STATUS.          
           MOVE 'LISTA'             TO        WRK-NOME-ARQ.             
           PERFORM 0100-TESTA-FILE-STATUS.                              
                                                                        
RST        CALL 'POOL0160' USING WRK-JOBNAME WRK-VALORFAC               
RST        MOVE WRK-JOBNAME                 TO    ECTSW001-JOBNAME      
                                                                        
           CALL  'POOL7600'  USING  WRK-DATA-HORA.                      
           MOVE  WRK-DT-AAAAMMDD  TO  WRK-DT-SIST.                      
                                                                        
BSI        MOVE      WRK-DIA-SIST      TO   WRK-DIA-FAC.                
BSI        MOVE      WRK-MES-SIST      TO   WRK-MES-FAC.                
BSI        MOVE      WRK-ANO-SIST      TO   WRK-ANO-FAC.                
                                                                        
           PERFORM 0400-LER-PARM.                                       
                                                                        
           READ    DATATECL.                                            
                                                                        
           MOVE 'READ'              TO        WRK-FUNCAO.               
           MOVE WRK-FS-DATATECL     TO        WRK-FILE-STATUS.          
           MOVE 'DATATECL'          TO        WRK-NOME-ARQ.             
           PERFORM 0100-TESTA-FILE-STATUS.                              
                                                                        
           PERFORM 0200-LER-LADO1.                                      
                                                                        
           IF WRK-WRK-FIM-LADO1  NOT =  'S'                             
              MOVE '1DJDE FORMS=CLLP18,END;' TO WRK-FORMS               
              MOVE 'CLLP18' TO  CB-3A-ROTINA CB-3B-ROTINA               
           END-IF.                                                      
                                                                        
           PERFORM 0500-PROCESSA UNTIL WRK-WRK-FIM-LADO1 EQUAL 'S'.     
                                                                        
           MOVE    ACU-CARTAS       TO        CB-LT1-CARTAS.            
                                                                        
           WRITE FD-LIS-REGTO          FROM      WRK-DJDE-FIM.          
           MOVE 'WRITE'             TO        WRK-FUNCAO.               
           MOVE 'LISTA'             TO        WRK-NOME-ARQ.             
           MOVE WRK-FS-LISTA        TO        WRK-FILE-STATUS.          
           PERFORM 0100-TESTA-FILE-STATUS.                              
           IF  ACU-CARTAS  NOT = ZEROS                                  
               WRITE FD-LT1-REGTO          FROM      CABEC-1-L          
               MOVE WRK-FS-LISTA1       TO        WRK-FILE-STATUS       
               MOVE 'LISTA1'            TO        WRK-NOME-ARQ          
               PERFORM 0100-TESTA-FILE-STATUS                           
               WRITE FD-LT1-REGTO          FROM      CABEC-TOT1         
               MOVE WRK-FS-LISTA1       TO        WRK-FILE-STATUS       
               PERFORM 0100-TESTA-FILE-STATUS                           
           END-IF.                                                      
                                                                        
           CLOSE  LADO1  LADO2  DATATECL  LISTA  LISTA1  ARQECT         
BSI               ARQPARM                                               
BSI               ARQCEDD.                                              
                                                                        
           MOVE 'CLOSE'             TO        WRK-FUNCAO.               
                                                                        
           MOVE WRK-FS-LADO1        TO        WRK-FILE-STATUS.          
           MOVE 'LADO1'             TO        WRK-NOME-ARQ.             
           PERFORM 0100-TESTA-FILE-STATUS.                              
                                                                        
           MOVE WRK-FS-LADO2        TO        WRK-FILE-STATUS.          
           MOVE 'LADO2'             TO        WRK-NOME-ARQ.             
           PERFORM 0100-TESTA-FILE-STATUS.                              
                                                                        
           MOVE WRK-FS-DATATECL     TO        WRK-FILE-STATUS.          
           MOVE 'DATATECL'          TO        WRK-NOME-ARQ.             
           PERFORM 0100-TESTA-FILE-STATUS.                              
                                                                        
           MOVE WRK-FS-LISTA        TO        WRK-FILE-STATUS.          
           MOVE 'LISTA'             TO        WRK-NOME-ARQ.             
           PERFORM 0100-TESTA-FILE-STATUS.                              
                                                                        
           MOVE WRK-FS-LISTA1       TO        WRK-FILE-STATUS.          
           MOVE 'LISTA1'            TO        WRK-NOME-ARQ.             
           PERFORM 0100-TESTA-FILE-STATUS.                              
                                                                        
BSI        MOVE WRK-FS-ARQCEDD      TO        WRK-FILE-STATUS.          
BSI        MOVE 'ARQCEDD'           TO        WRK-NOME-ARQ.             
BSI        PERFORM 0100-TESTA-FILE-STATUS.                              
                                                                        
           GOBACK.                                                      
                                                                        
      *---------------------------------------------------------------* 
       0000-INICIO-FIM.  EXIT.                                          
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       0100-TESTA-FILE-STATUS     SECTION.                              
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-FILE-STATUS  NOT  EQUAL  '00' AND '10'                
              DISPLAY '***************** CLLP0782 ****************'     
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
              DISPLAY '***************** CLLP0782 ****************'     
              CALL 'ILBOABN0'         USING       WRK-ABEND             
           END-IF.                                                      
                                                                        
           IF WRK-FILE-STATUS  EQUAL  '10'                              
             IF WRK-NOME-ARQ  EQUAL  'LADO1'                            
                MOVE 'S'             TO          WRK-WRK-FIM-LADO1      
              ELSE                                                      
                IF WRK-NOME-ARQ  EQUAL  'LADO2'                         
                   MOVE 'S'          TO          WRK-WRK-FIM-LADO2      
                ELSE                                                    
                   IF WRK-NOME-ARQ  EQUAL  'ARQPARM'                    
                      MOVE 'S'       TO          WRK-WRK-FIM-PARM       
                   END-IF                                               
                END-IF                                                  
             END-IF                                                     
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       0100-TESTA-FILE-STATUS-FIM.  EXIT.                               
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       0200-LER-LADO1             SECTION.                              
      *---------------------------------------------------------------* 
                                                                        
           READ LADO1 INTO WRK-REGTO.                                   
                                                                        
           MOVE 'READ'              TO        WRK-FUNCAO.               
           MOVE 'LADO1'             TO        WRK-NOME-ARQ.             
           MOVE WRK-FS-LADO1        TO        WRK-FILE-STATUS.          
           PERFORM 0100-TESTA-FILE-STATUS.                              
                                                                        
      *---------------------------------------------------------------* 
       0200-LER-LADO1-FIM.  EXIT.                                       
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       0300-LER-LADO2             SECTION.                              
      *---------------------------------------------------------------* 
                                                                        
           READ LADO2 INTO WRK-REGTO.                                   
                                                                        
           MOVE 'READ'              TO        WRK-FUNCAO.               
           MOVE 'LADO2'             TO        WRK-NOME-ARQ.             
           MOVE WRK-FS-LADO2        TO        WRK-FILE-STATUS.          
           PERFORM 0100-TESTA-FILE-STATUS.                              
                                                                        
      *---------------------------------------------------------------* 
       0300-LER-LADO2-FIM.  EXIT.                                       
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       0400-LER-PARM              SECTION.                              
      *---------------------------------------------------------------* 
                                                                        
           READ ARQPARM.                                                
                                                                        
           MOVE 'READ'              TO        WRK-FUNCAO.               
           MOVE 'ARQPARM'           TO        WRK-NOME-ARQ.             
           MOVE WRK-FS-PARM         TO        WRK-FILE-STATUS.          
           PERFORM 0100-TESTA-FILE-STATUS.                              
                                                                        
           IF WRK-WRK-FIM-PARM   NOT =  'S'                             
              IF FD-PARM-SEQUENCIA = 1                                  
                 MOVE FD-PARM-NR-DIAS TO WRK-FAIXA01                    
                 GO TO 0400-LER-PARM                                    
              ELSE                                                      
                 IF FD-PARM-SEQUENCIA = 2                               
                    MOVE FD-PARM-NR-DIAS TO WRK-FAIXA02                 
                    GO TO 0400-LER-PARM                                 
                 ELSE                                                   
                    IF FD-PARM-SEQUENCIA = 3                            
                       MOVE FD-PARM-NR-DIAS TO WRK-FAIXA03              
                       GO TO 0400-LER-PARM                              
                    END-IF                                              
                 END-IF                                                 
              END-IF                                                    
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       0400-LER-FD-PARM-FIM.  EXIT.                                     
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       0500-PROCESSA    SECTION.                                        
      *---------------------------------------------------------------* 
                                                                        
           PERFORM 0600-RESTART.                                        
                                                                        
           PERFORM 0700-LIMPA-CAMPOS                                    
                                                                        
           PERFORM 0800-MONTA-LADO1.                                    
                                                                        
           PERFORM 0300-LER-LADO2.                                      
                                                                        
           PERFORM 1800-MONTA-LADO2.                                    
                                                                        
           PERFORM 2800-IMPRI-INTERNO.                                  
                                                                        
           PERFORM 3000-IMPRI-EXTERNO.                                  
                                                                        
           PERFORM 0200-LER-LADO1.                                      
                                                                        
      *---------------------------------------------------------------* 
       0500-PROCESSA-FIM.  EXIT.                                        
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       0600-RESTART               SECTION.                              
      *---------------------------------------------------------------* 
                                                                        
           ADD     1                  TO         WRK-RESTART.           
                                                                        
           MOVE    WRK-RESTART        TO         WRK-INT-RESTART        
                                                 WRK-EXT-RESTART        
                                                 WRK-EXT1-RESTART       
                                                 CB-CD-RESTART          
                                                 CB-1B-RESTART          
                                                 CB-2B-RESTART          
                                                 CB-3B-RESTART          
                                                 CB-LPOS1-SEQ-RESTART   
                                                 CB-LPOS2-SEQ-RESTART   
                                                 CB-LCIF1-SEQ-RESTART   
                                                 CB-LCIF2-SEQ-RESTART   
                                                 CB-4B-RESTART          
                                                 CB-5B-RESTART          
                                                 CB-5BB-RESTART         
                                                 CB-6B-RESTART          
                                                 CB-7B-RESTART          
                                                  CB-LD1-RESTART        
                                                  CB-LD3-RESTART        
                                                  CB-LD4-RESTART        
                                                  CB-LD6-RESTART        
                                                  CB-LD7-RESTART        
                                                  CB-LD8-RESTART        
                                                  CB-LIN2-RESTART       
                                                  CB-LIN3-RESTART       
                                                  CB-LIN1-RESTART.      
                                                                        
      *---------------------------------------------------------------* 
       0600-RESTART-FIM.  EXIT.                                         
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       0700-LIMPA-CAMPOS          SECTION.                              
      *---------------------------------------------------------------* 
                                                                        
           MOVE SPACES                    TO           CB-CD-LADO1      
                                                       CB-1A-LADO1      
                                                       CB-2A-LADO1      
                                                       CB-3A-LADO1      
                                                       CB-4A-LADO1      
                                                       CB-5A-LADO1      
                                                       CB-5AA-LADO1     
                                                       WRK-AA-LADO1     
                                                       WRK-A-LADO1      
                                                       CB-6A-LADO1      
                                                       CB-7A-LADO1.     
                                                                        
           MOVE SPACES                    TO           CB-CD-LADO2      
                                                       CB-1B-LADO2      
                                                       CB-2B-LADO2      
                                                       CB-3B-LADO2      
                                                       CB-4B-LADO2      
                                                       CB-5B-LADO2      
                                                       CB-5BB-LADO2     
                                                       WRK-BB-LADO2     
                                                       WRK-B-LADO2      
                                                       CB-6B-LADO2      
                                                       CB-7B-LADO2.     
                                                                        
           MOVE SPACES                    TO           CB-LD1-LADOA     
                                                       CB-LD3-LADOA     
                                                       CB-LD4-LADOA     
                                                       CB-LD6-LADOA     
                                                       CB-LD7-LADOA     
                                                       CB-LD8-LADOA.    
                                                                        
           MOVE SPACES                    TO           CB-LD1-LADOB     
                                                       CB-LD3-LADOB     
                                                       CB-LD4-LADOB     
                                                       CB-LD6-LADOB     
                                                       CB-LD7-LADOB     
                                                       CB-LD8-LADOB.    
                                                                        
           MOVE SPACES                    TO           CB-LD2-CABEC (1).
           MOVE SPACES                    TO           CB-LD2-CABEC (2).
           MOVE SPACES                    TO           CB-LD2-CABEC (3).
           MOVE SPACES                    TO           CB-LD2-CABEC (4).
           MOVE SPACES                    TO           CB-LD2-CABEC (5).
           MOVE SPACES                    TO           CB-LD2-CABEC (6).
           MOVE SPACES                    TO           CB-LD2-CABEC (7).
                                                                        
                                                                        
      *---------------------------------------------------------------* 
       0700-LIMPA-CAMPOS-FIM. EXIT.                                     
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       0800-MONTA-LADO1           SECTION.                              
      *---------------------------------------------------------------* 
                                                                        
           PERFORM 0900-MONT-INT-LD1.                                   
                                                                        
           PERFORM 1400-MONT-EXT-LD1.                                   
                                                                        
      *---------------------------------------------------------------* 
       0800-MONTA-LADO1-FIM.  EXIT.                                     
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       0900-MONT-INT-LD1          SECTION.                              
      *---------------------------------------------------------------* 
                                                                        
      *--- CABEC-IN 1 ---*                                              
                                                                        
           MOVE    WRK-NOME-RESP     TO    CB-LD1A-NOME-RES             
                                                                        
      *--- CABEC-IN 2 ---*                                              
                                                                        
           PERFORM  1000-LIMP-ARQA VARYING WRK-IND1 FROM 1 BY 1 UNTIL   
                                           WRK-IND1 GREATER 7.          
                                                                        
           MOVE    7                 TO    WRK-IND-AUX.                 
           PERFORM  1100-SORT-A VARYING WRK-IND1 FROM 1 BY 1 UNTIL      
                                        WRK-IND1 GREATER WRK-IND-AUX.   
                                                                        
           PERFORM  1300-CARRE-DET-A VARYING WRK-IND1 FROM 1 BY 1 UNTIL 
                                             WRK-IND1 GREATER 7.        
                                                                        
      *--- CABEC-IN 4 ---*                                              
                                                                        
           MOVE  'OSASCO'        TO      CB-LD4A-CID-AGE                
           MOVE  WRK-DIA-SIST    TO      CB-LD4A-DIA                    
           MOVE  WRK-MES-SIST    TO      CB-LD4A-MES                    
           MOVE  WRK-ANO-SIST    TO      CB-LD4A-ANO.                   
           MOVE  '/'             TO      CB-LD4A-BARRA1                 
                                         CB-LD4A-BARRA2.                
                                                                        
      *--- CABEC-IN 7/8 -*                                              
                                                                        
           IF  WRK-FLAG-ESQ            EQUAL 'S'                        
               MOVE 'N'                   TO WRK-FLAG-ESQ               
               MOVE WRK-LITERAL1          TO CB-LD7A-LITERAL1           
               MOVE WRK-LITERAL2          TO CB-LD8A-LITERAL2           
           ELSE                                                         
               MOVE SPACES                TO CB-LD7A-LITERAL1           
                                             CB-LD8A-LITERAL2           
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       0900-MONT-INT-LD1-FIM.  EXIT.                                    
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1000-LIMP-ARQA             SECTION.                              
      *---------------------------------------------------------------* 
                                                                        
              IF WRK-VCMTO(WRK-IND1)  NOT NUMERIC                       
                 MOVE ZEROS           TO      WRK-VCMTO(WRK-IND1)       
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       1000-LIMP-ARQA-FIM.  EXIT.                                       
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1100-SORT-A                SECTION.                              
      *---------------------------------------------------------------* 
                                                                        
           MOVE 2                   TO       WRK-IND2.                  
           PERFORM  1200-SORT-A1 VARYING WRK-IND1 FROM 1 BY 1 UNTIL     
                                         WRK-IND1 GREATER WRK-IND-AUX.  
                                                                        
           COMPUTE WRK-IND-AUX = WRK-IND-AUX - 1.                       
           MOVE    1                TO       WRK-IND1.                  
                                                                        
      *---------------------------------------------------------------* 
       1100-SORT-A-FIM. EXIT.                                           
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1200-SORT-A1               SECTION.                              
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-IND2   GREATER  7                                     
              NEXT SENTENCE                                             
           ELSE                                                         
              IF WRK-VCMTO(WRK-IND1)  LESS  WRK-VCMTO(WRK-IND2)         
                 MOVE WRK-OCORRENCIAS(WRK-IND1) TO WRK-AREA-AUX         
                 MOVE WRK-OCORRENCIAS(WRK-IND2) TO                      
                                            WRK-OCORRENCIAS(WRK-IND1)   
                 MOVE WRK-AREA-AUX    TO WRK-OCORRENCIAS(WRK-IND2)      
              END-IF                                                    
           END-IF.                                                      
                                                                        
           ADD   1                        TO WRK-IND2.                  
                                                                        
      *---------------------------------------------------------------* 
       1200-SORT-A1-FIM.  EXIT.                                         
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1300-CARRE-DET-A           SECTION.                              
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-VCMTO (WRK-IND1)  EQUAL ZEROS                         
              MOVE 8                      TO WRK-IND1                   
              GO TO 1300-CARRE-DET-A-FIM                                
           END-IF.                                                      
                                                                        
           MOVE WRK-POSSIBIL (WRK-IND1) TO CB-LD2A-POSSIBIL (WRK-IND1). 
           MOVE WRK-NATUREZA (WRK-IND1) TO CB-LD2A-COD-NAT  (WRK-IND1). 
           MOVE ' - '                   TO CB-LD2A-HIFEN    (WRK-IND1). 
           MOVE WRK-DESCRIC (WRK-IND1)  TO CB-LD2A-DESCRIC  (WRK-IND1). 
                                                                        
           MOVE WRK-VCMTO    (WRK-IND1) TO WRK-DT-SSAAMMDD.             
           MOVE WRK-DIA-SAMD            TO CB-LD2A-DIA      (WRK-IND1). 
           MOVE '/'                     TO CB-LD2A-BAR1     (WRK-IND1). 
           MOVE WRK-MES-SAMD            TO CB-LD2A-MES      (WRK-IND1). 
           MOVE '/'                     TO CB-LD2A-BAR2     (WRK-IND1). 
           MOVE WRK-ANO-SAMD            TO CB-LD2A-ANO      (WRK-IND1). 
           MOVE WRK-VALOR    (WRK-IND1) TO CB-LD2A-VALOR    (WRK-IND1). 
                                                                        
           IF  WRK-POSSIBIL(WRK-IND1)  EQUAL '*'                        
               MOVE  'S'                  TO WRK-FLAG-ESQ               
           END-IF.                                                      
                                                                        
           ADD  WRK-VALOR    (WRK-IND1)   TO WRK-VALOR-A.               
                                                                        
           MOVE    WRK-NRO-SEQ            TO  CB-LD6A-NRO-SEQ.          
                                                                        
      *---------------------------------------------------------------* 
       1300-CARRE-DET-A-FIM. EXIT.                                      
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1400-MONT-EXT-LD1          SECTION.                              
      *---------------------------------------------------------------* 
                                                                        
           MOVE    4120              TO    CB-3A-AGEN.                  
           MOVE    '-'               TO    CB-3A-HIFEN.                 
           MOVE    'DRC/MATRIZ'      TO    CB-3A-NOME-AG.               
                                                                        
                                                                        
           MOVE    WRK-NOME-RESP     TO    CB-4A-NOME-RES               
                                                                        
           CALL    WRK-LPCL5011      USING WRK-END-RESP.                
                                                                        
           MOVE  SPACES              TO    CB-5A-END-RES.               
           STRING WRK-END-RESP ' ' WRK-DEV-NRO                          
           DELIMITED BY '  ' INTO CB-5A-END-RES.                        
                                                                        
           MOVE    WRK-DEV-COMPL     TO    WRK-A-COM-RES                
           MOVE    WRK-DEV-BAIRRO    TO    WRK-A-BAI-RES                
                                                                        
           MOVE    WRK-CEP           TO    WRK-A-CEP                    
           MOVE    '-'               TO    WRK-A-HIFEN                  
           MOVE    WRK-CEP-SUFIXO    TO    WRK-A-CEP-SUFIXO             
                                                                        
           MOVE  SPACES              TO    WRK-A-CIDADE                 
           STRING WRK-CIDADE  '-' WRK-EST                               
           DELIMITED BY '  ' INTO WRK-A-CIDADE                          
                                                                        
           MOVE    WRK-NRO-SEQ       TO    CB-3A-NRO-SEQ                
                                                                        
           IF  WRK-TIPO    EQUAL   1                                    
               ADD  1                TO     ACU-CARTAS                  
               MOVE  WRK-NOME-CORREIO  TO  CB-LT1-NOME                  
                                                                        
               PERFORM  1500-CARTA-CORREIO-A                            
                                                                        
               MOVE FD-DAT-DIA       TO    CB-7A-DD-POST                
               MOVE FD-DAT-MES       TO    CB-7A-MM-POST                
               MOVE WRK-ANO-POST     TO    CB-7A-AA-POST                
               MOVE '/'              TO    CB-7A-BARRA1                 
                                           CB-7A-BARRA2                 
           ELSE                                                         
               ADD  1                  TO     ACU-CARTAS                
               MOVE  WRK-NOME-AGENCIA  TO  CB-LT1-NOME                  
               IF  WRK-TIPO    EQUAL   2                                
                   MOVE  '*999999999*'  TO  CB-LPOS2-AREA-POSTNET-E     
                   MOVE  ALL '9'        TO  CB-LCIF1-AREA-E             
                                            CB-LCIF2-AREA-E             
                   PERFORM 1600-CARTA-AGENCIA-A                         
               ELSE                                                     
                   MOVE  '*999999999*'  TO  CB-LPOS2-AREA-POSTNET-E     
                   MOVE  ALL '9'        TO  CB-LCIF1-AREA-E             
                                            CB-LCIF2-AREA-E             
                   PERFORM 1700-CARTA-AVALISTA-A                        
               END-IF                                                   
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       1400-MONT-EXT-LD1-FIM. EXIT.                                     
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1500-CARTA-CORREIO-A       SECTION.                              
      *---------------------------------------------------------------* 
                                                                        
      *================================================================*
      *===> COM A IMPRESSAO DO CODIGO DE BARRAS PARA A EMPRESA ADDRESS *
      *===> O ENDERECO DE RETORNO DE CORRESPONDENCIA PARA A AGENCIA    *
      *===> NAO DEVE MAIS SER IMPRESSO                                 *
      *================================================================*
                                                                        
JGA        MOVE    SPACES            TO    CB-1A-END-AG.                
           MOVE    SPACES            TO    CB-1A-ECT.                   
           MOVE    WRK-CEP-AG        TO    WRK-TRATA-CEP-R.             
                                                                        
           MOVE WRK-CEP              TO    WRK-NUMCEP-POSTNET-9011      
RST                                        ECTSW001-NUMCEP-FAC.         
           MOVE WRK-CEP-SUFIXO       TO    WRK-CPLCEP-POSTNET-9011      
RST                                        ECTSW001-CPLCEP-FAC.         
                                                                        
RST        PERFORM  3500-OBTEM-DADOS-FAC      THRU                      
RST                 3500-OBTEM-DADOS-FAC-FIM.                           
           PERFORM  3400-OBTEM-BARRA-POSTNET  THRU                      
                    3400-OBTEM-BARRA-POSTNET-FIM.                       
           MOVE  WRK-RETORNO-POSTNET-9011 TO CB-LPOS2-AREA-POSTNET-E.   
           PERFORM  3200-MONTA-BARRA-CIF      THRU                      
                    3200-MONTA-BARRA-CIF-FIM.                           
           MOVE  WRK-CABEC-RETORNO-9010  TO   CB-LCIF1-AREA-E.          
           MOVE  WRK-CABEC-NUMERO-9010   TO   CB-LCIF2-AREA-E.          
BSI        PERFORM  3600-GRAVA-ARQCEDD        THRU                      
BSI                 3600-GRAVA-ARQCEDD-FIM.                             
                                                                        
      *---------------------------------------------------------------* 
       1500-CARTA-CORREIO-A-FIM.  EXIT.                                 
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1600-CARTA-AGENCIA-A       SECTION.                              
      *---------------------------------------------------------------* 
                                                                        
           MOVE    SPACES        TO    CB-CD1-BRANCO1.                  
           MOVE   'CD'           TO    CB-CD1-CD.                       
           MOVE    WRK-CD        TO    CB-CD1-NRO-CD.                   
                                                                        
           IF  WRK-COD-ENT     EQUAL     3                              
               MOVE   ' '  TO  CB-2-CARRO                               
               MOVE   '0'  TO  CB-3-CARRO                               
               MOVE   'CEP IRREGULAR - CONSULTE  O'  TO  CB-1A-CEP-IR   
               MOVE   'GUIA POSTAL OU CDC REGIONAL'  TO  CB-2A-CEP-IR   
           ELSE                                                         
               MOVE   ' '  TO  CB-2-CARRO                               
               MOVE   '0'  TO  CB-3-CARRO                               
               MOVE SPACES TO  CB-1A-BRANCO3                            
               MOVE   'FRANQUIA  CANCELADA'          TO  CB-1A-FRANQUIA 
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       1600-CARTA-AGENCIA-A-FIM. EXIT.                                  
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1700-CARTA-AVALISTA-A      SECTION.                              
      *---------------------------------------------------------------* 
                                                                        
           MOVE    SPACES            TO    CB-CD1-BRANCO1.              
           MOVE   'CD'               TO    CB-CD1-CD.                   
           MOVE    WRK-CD            TO    CB-CD1-NRO-CD.               
           MOVE   ' '                TO    CB-2-CARRO.                  
           MOVE   '0'                TO    CB-3-CARRO.                  
           MOVE   SPACES             TO    CB-1A-BRANCO3.               
           MOVE   'FRANQUIA  CANCELADA'    TO    CB-1A-FRANQUIA.        
                                                                        
      *---------------------------------------------------------------* 
       1700-CARTA-AVALISTA-A-FIM. EXIT.                                 
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1800-MONTA-LADO2           SECTION.                              
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-WRK-FIM-LADO2  EQUAL 'S'                              
              GO TO 1800-MONTA-LADO2-FIM                                
           END-IF.                                                      
                                                                        
           PERFORM 1900-MONT-INT-LD2.                                   
                                                                        
           PERFORM 2400-MONT-EXT-LD2.                                   
                                                                        
      *---------------------------------------------------------------* 
       1800-MONTA-LADO2-FIM.  EXIT.                                     
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       1900-MONT-INT-LD2          SECTION.                              
      *---------------------------------------------------------------* 
                                                                        
                                                                        
      *--- CABEC-IN 1 ---*                                              
                                                                        
           MOVE    WRK-NOME-RESP     TO    CB-LD1B-NOME-RES             
                                                                        
      *--- CABEC-IN 2 ---*                                              
                                                                        
           PERFORM  2000-LIMP-ARQB VARYING WRK-IND1 FROM 1 BY 1 UNTIL   
                                           WRK-IND1 GREATER 7.          
                                                                        
           MOVE    7                 TO    WRK-IND-AUX.                 
           PERFORM  2100-SORT-B1 VARYING WRK-IND1 FROM 1 BY 1 UNTIL     
                                         WRK-IND1 GREATER WRK-IND-AUX.  
                                                                        
           PERFORM  2300-CARRE-DET-B1 VARYING WRK-IND1 FROM 1 BY 1 UNTIL
                                              WRK-IND1 GREATER 7.       
                                                                        
      *--- CABEC-IN 4 ---*                                              
                                                                        
           MOVE  'OSASCO'       TO      CB-LD4B-CID-AGE                 
           MOVE  WRK-DIA-SIST   TO      CB-LD4B-DIA                     
           MOVE  WRK-MES-SIST   TO      CB-LD4B-MES                     
           MOVE  WRK-ANO-SIST   TO      CB-LD4B-ANO.                    
           MOVE  '/'             TO      CB-LD4B-BARRA1                 
                                         CB-LD4B-BARRA2.                
                                                                        
      *--- CABEC-IN 7/8 -*                                              
                                                                        
           IF  WRK-FLAG-DIR            EQUAL 'S'                        
               MOVE 'N'                   TO WRK-FLAG-DIR               
               MOVE WRK-LITERAL1          TO CB-LD7B-LITERAL1           
               MOVE WRK-LITERAL2          TO CB-LD8B-LITERAL2           
           ELSE                                                         
               MOVE SPACES                TO CB-LD7B-LITERAL1           
                                             CB-LD8B-LITERAL2           
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       1900-MONT-INT-LD2-FIM.  EXIT.                                    
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       2000-LIMP-ARQB             SECTION.                              
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-VCMTO(WRK-IND1)  NOT NUMERIC                          
              MOVE ZEROS           TO      WRK-VCMTO(WRK-IND1)          
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       2000-LIMP-ARQB-FIM.  EXIT.                                       
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       2100-SORT-B1               SECTION.                              
      *---------------------------------------------------------------* 
                                                                        
           MOVE 2                   TO       WRK-IND2.                  
           PERFORM  2200-SORT-B2 VARYING WRK-IND1 FROM 1 BY 1 UNTIL     
                                         WRK-IND1 GREATER WRK-IND-AUX.  
                                                                        
           COMPUTE WRK-IND-AUX = WRK-IND-AUX - 1.                       
           MOVE    1                TO       WRK-IND1.                  
                                                                        
      *---------------------------------------------------------------* 
       2100-SORT-B1-FIM. EXIT.                                          
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       2200-SORT-B2                 SECTION.                            
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-IND2   GREATER  7                                     
              NEXT SENTENCE                                             
           ELSE                                                         
              IF WRK-VCMTO(WRK-IND1)  LESS  WRK-VCMTO(WRK-IND2)         
                 MOVE WRK-OCORRENCIAS(WRK-IND1) TO WRK-AREA-AUX         
                 MOVE WRK-OCORRENCIAS(WRK-IND2) TO                      
                                            WRK-OCORRENCIAS(WRK-IND1)   
                 MOVE WRK-AREA-AUX   TO WRK-OCORRENCIAS(WRK-IND2)       
              END-IF                                                    
           END-IF.                                                      
                                                                        
           ADD   1                        TO WRK-IND2.                  
                                                                        
      *---------------------------------------------------------------* 
       2200-SORT-B2-FIM.  EXIT.                                         
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       2300-CARRE-DET-B1          SECTION.                              
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-VCMTO (WRK-IND1)  EQUAL ZEROS                         
              MOVE 8                      TO WRK-IND1                   
              GO TO 2300-CARRE-DET-B1-FIM                               
           END-IF.                                                      
                                                                        
           MOVE WRK-POSSIBIL (WRK-IND1)   TO CB-LD2B-POSSIBIL (WRK-IND1)
           MOVE WRK-NATUREZA (WRK-IND1)   TO CB-LD2B-COD-NAT  (WRK-IND1)
           MOVE ' - '                     TO CB-LD2B-HIFEN    (WRK-IND1)
           MOVE WRK-DESCRIC  (WRK-IND1)   TO CB-LD2B-DESCRIC  (WRK-IND1)
                                                                        
           MOVE WRK-VCMTO    (WRK-IND1)   TO WRK-DT-SSAAMMDD.           
           MOVE WRK-DIA-SAMD              TO CB-LD2B-DIA      (WRK-IND1)
           MOVE '/'                       TO CB-LD2B-BAR1     (WRK-IND1)
           MOVE WRK-MES-SAMD              TO CB-LD2B-MES      (WRK-IND1)
           MOVE '/'                       TO CB-LD2B-BAR2     (WRK-IND1)
           MOVE WRK-ANO-SAMD              TO CB-LD2B-ANO      (WRK-IND1)
           MOVE WRK-VALOR    (WRK-IND1)   TO CB-LD2B-VALOR    (WRK-IND1)
                                                                        
                                                                        
           IF  WRK-POSSIBIL(WRK-IND1)  EQUAL '*'                        
               MOVE  'S'                  TO WRK-FLAG-DIR               
           END-IF.                                                      
                                                                        
           ADD  WRK-VALOR    (WRK-IND1)   TO WRK-VALOR-B.               
                                                                        
           MOVE    WRK-NRO-SEQ            TO  CB-LD6B-NRO-SEQ.          
                                                                        
      *---------------------------------------------------------------* 
       2300-CARRE-DET-B1-FIM. EXIT.                                     
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       2400-MONT-EXT-LD2          SECTION.                              
      *---------------------------------------------------------------* 
                                                                        
           MOVE    4120              TO    CB-3B-AGEN.                  
           MOVE    '-'               TO    CB-3B-HIFEN.                 
           MOVE    'DRC/MATRIZ'      TO    CB-3B-NOME-AG.               
                                                                        
                                                                        
           MOVE    WRK-NOME-RESP     TO    CB-4B-NOME-RES               
                                                                        
           CALL    WRK-LPCL5011      USING WRK-END-RESP.                
                                                                        
           MOVE  SPACES              TO    CB-5B-END-RES.               
           STRING WRK-END-RESP ' ' WRK-DEV-NRO                          
           DELIMITED BY '  ' INTO CB-5B-END-RES.                        
                                                                        
           MOVE    WRK-DEV-COMPL     TO    WRK-B-COM-RES                
           MOVE    WRK-DEV-BAIRRO    TO    WRK-B-BAI-RES                
                                                                        
           MOVE    WRK-CEP           TO    WRK-B-CEP                    
           MOVE    '-'               TO    WRK-B-HIFEN                  
           MOVE    WRK-CEP-SUFIXO    TO    WRK-B-CEP-SUFIXO             
                                                                        
           MOVE  SPACES              TO    WRK-B-CIDADE                 
           STRING WRK-CIDADE  '-' WRK-EST                               
           DELIMITED BY '  ' INTO WRK-B-CIDADE                          
                                                                        
           MOVE    WRK-NRO-SEQ       TO    CB-3B-NRO-SEQ                
                                                                        
           IF  WRK-TIPO   EQUAL   1                                     
               ADD 1                 TO    ACU-CARTAS                   
               MOVE  WRK-NOME-CORREIO  TO  CB-LT1-NOME                  
                                                                        
               PERFORM 2500-CARTA-CORREIO-B                             
                                                                        
               MOVE FD-DAT-DIA       TO    CB-7B-DD-POST                
               MOVE FD-DAT-MES       TO    CB-7B-MM-POST                
               MOVE WRK-ANO-POST     TO    CB-7B-AA-POST                
               MOVE '/'              TO    CB-7B-BARRA1                 
                                           CB-7B-BARRA2                 
           ELSE                                                         
               ADD 1                 TO    ACU-CARTAS                   
               MOVE  WRK-NOME-AGENCIA  TO  CB-LT1-NOME                  
               IF  WRK-TIPO   EQUAL   2                                 
                   MOVE  '*999999999*'  TO  CB-LPOS2-AREA-POSTNET-D     
                   MOVE  ALL '9'        TO  CB-LCIF1-AREA-D             
                                            CB-LCIF2-AREA-D             
                   PERFORM 2600-CARTA-AGENCIA-B                         
               ELSE                                                     
                   MOVE  '*999999999*'  TO  CB-LPOS2-AREA-POSTNET-D     
                   MOVE  ALL '9'        TO  CB-LCIF1-AREA-D             
                                            CB-LCIF2-AREA-D             
                   PERFORM 2700-CARTA-AVALISTA-B                        
               END-IF                                                   
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       2400-MONT-EXT-LD2-FIM. EXIT.                                     
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       2500-CARTA-CORREIO-B       SECTION.                              
      *================================================================*
      *===> COM A IMPRESSAO DO CODIGO DE BARRAS PARA A EMPRESA ADDRESS *
      *===> O ENDERECO DE RETORNO DE CORRESPONDENCIA PARA A AGENCIA    *
      *===> NAO DEVE MAIS SER IMPRESSO                                 *
      *================================================================*
JGA        MOVE    SPACES            TO    CB-1B-END-AG.                
           MOVE   SPACES             TO    CB-1B-ECT.                   
           MOVE    WRK-CEP-AG        TO    WRK-TRATA-CEP-R.             
                                                                        
           MOVE WRK-CEP              TO    WRK-NUMCEP-POSTNET-9011      
RST                                              ECTSW001-NUMCEP-FAC.   
           MOVE WRK-CEP-SUFIXO       TO    WRK-CPLCEP-POSTNET-9011      
RST                                              ECTSW001-CPLCEP-FAC    
RST        PERFORM  3500-OBTEM-DADOS-FAC      THRU                      
RST                 3500-OBTEM-DADOS-FAC-FIM.                           
           PERFORM  3400-OBTEM-BARRA-POSTNET  THRU                      
                    3400-OBTEM-BARRA-POSTNET-FIM.                       
           MOVE  WRK-RETORNO-POSTNET-9011 TO CB-LPOS2-AREA-POSTNET-D.   
           PERFORM  3200-MONTA-BARRA-CIF      THRU                      
                    3200-MONTA-BARRA-CIF-FIM.                           
           MOVE  WRK-CABEC-RETORNO-9010  TO   CB-LCIF1-AREA-D.          
           MOVE  WRK-CABEC-NUMERO-9010   TO   CB-LCIF2-AREA-D.          
BSI        PERFORM  3600-GRAVA-ARQCEDD        THRU                      
BSI                 3600-GRAVA-ARQCEDD-FIM.                             
                                                                        
      *---------------------------------------------------------------* 
       2500-CARTA-CORREIO-B-FIM. EXIT.                                  
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       2600-CARTA-AGENCIA-B       SECTION.                              
      *---------------------------------------------------------------* 
                                                                        
           MOVE    SPACES          TO    CB-CD2-BRANCO1.                
           MOVE    'CD'            TO    CB-CD2-CD.                     
           MOVE    WRK-CD          TO    CB-CD2-NRO-CD.                 
                                                                        
           IF  WRK-COD-ENT    EQUAL    3                                
               MOVE   ' '  TO  CB-2-CARRO                               
               MOVE   '0'  TO  CB-3-CARRO                               
               MOVE    'CEP IRREGULAR - CONSULTE  O' TO    CB-1B-CEP-IR 
               MOVE    'GUIA POSTAL OU CDC REGIONAL' TO    CB-2B-CEP-IR 
           ELSE                                                         
               MOVE   ' '  TO  CB-2-CARRO                               
               MOVE   '0'  TO  CB-3-CARRO                               
               MOVE SPACES TO  CB-1B-BRANCO3                            
               MOVE    'FRANQUIA CANCELADA'  TO    CB-1B-FRANQUIA       
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       2600-CARTA-AGENCIA-B-FIM.  EXIT.                                 
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       2700-CARTA-AVALISTA-B      SECTION.                              
      *---------------------------------------------------------------* 
                                                                        
           MOVE    SPACES            TO    CB-CD2-BRANCO1.              
           MOVE    'CD'              TO    CB-CD2-CD.                   
           MOVE    WRK-CD            TO    CB-CD2-NRO-CD.               
           MOVE    ' '               TO    CB-2-CARRO.                  
           MOVE    '0'               TO    CB-3-CARRO.                  
           MOVE    SPACES            TO    CB-1B-BRANCO3.               
           MOVE    'FRANQUIA  CANCELADA'   TO    CB-1B-FRANQUIA.        
                                                                        
      *---------------------------------------------------------------* 
       2700-CARTA-AVALISTA-B-FIM. EXIT.                                 
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       2800-IMPRI-INTERNO         SECTION.                              
      *---------------------------------------------------------------* 
                                                                        
           WRITE FD-LIS-REGTO           FROM       WRK-DJDE-INT.        
           MOVE 'WRITE'              TO         WRK-FUNCAO.             
           MOVE 'LISTA'              TO         WRK-NOME-ARQ.           
           MOVE WRK-FS-LISTA         TO         WRK-FILE-STATUS.        
           PERFORM 0100-TESTA-FILE-STATUS.                              
                                                                        
           WRITE FD-LIS-REGTO           FROM       CABEC-LBR1.          
           MOVE WRK-FS-LISTA         TO         WRK-FILE-STATUS.        
           PERFORM 0100-TESTA-FILE-STATUS.                              
                                                                        
           WRITE FD-LIS-REGTO           FROM       CABEC-IN1.           
           MOVE WRK-FS-LISTA         TO         WRK-FILE-STATUS.        
           PERFORM 0100-TESTA-FILE-STATUS.                              
                                                                        
           MOVE  ' '                   TO      CB-LDBR2-CARRO.          
           WRITE FD-LIS-REGTO           FROM      CABEC-LBR2.           
           PERFORM 0100-TESTA-FILE-STATUS.                              
                                                                        
           PERFORM 2900-IMPR-TAB VARYING WRK-IND1 FROM 1 BY 1           
                                         UNTIL WRK-IND1 GREATER 7.      
                                                                        
                                                                        
           MOVE  WRK-VALOR-A         TO         CB-LD3A-TOT-DIVIDA.     
           MOVE  WRK-VALOR-B         TO         CB-LD3B-TOT-DIVIDA.     
           WRITE FD-LIS-REGTO           FROM       CABEC-IN3.           
           MOVE WRK-FS-LISTA         TO         WRK-FILE-STATUS.        
           PERFORM 0100-TESTA-FILE-STATUS.                              
                                                                        
           WRITE FD-LIS-REGTO           FROM       CABEC-IN7.           
           MOVE WRK-FS-LISTA         TO         WRK-FILE-STATUS.        
           PERFORM 0100-TESTA-FILE-STATUS.                              
                                                                        
           WRITE FD-LIS-REGTO           FROM       CABEC-IN8.           
           MOVE WRK-FS-LISTA         TO         WRK-FILE-STATUS.        
           PERFORM 0100-TESTA-FILE-STATUS.                              
                                                                        
           WRITE FD-LIS-REGTO           FROM       CABEC-IN4.           
           MOVE WRK-FS-LISTA         TO         WRK-FILE-STATUS.        
           PERFORM 0100-TESTA-FILE-STATUS.                              
                                                                        
           WRITE FD-LIS-REGTO           FROM       CABEC-IN6.           
           MOVE WRK-FS-LISTA         TO         WRK-FILE-STATUS.        
           PERFORM 0100-TESTA-FILE-STATUS.                              
                                                                        
                                                                        
           MOVE  ZEROS               TO         WRK-VALOR-A             
                                                WRK-VALOR-B.            
                                                                        
      *---------------------------------------------------------------* 
       2800-IMPRI-INTERNO-FIM.  EXIT.                                   
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       2900-IMPR-TAB              SECTION.                              
      *---------------------------------------------------------------* 
                                                                        
           MOVE WRK-RESTART          TO   CB-LD2-RESTART(WRK-IND1)      
                                                                        
           MOVE '1'                  TO   CB-CB-LD2-FONTINDEX(WRK-IND1) 
           IF  WRK-IND1  =  1                                           
               MOVE  '-'             TO   CB-LD2-CARRO(WRK-IND1)        
           ELSE                                                         
               MOVE  ' '             TO   CB-LD2-CARRO(WRK-IND1)        
           END-IF.                                                      
                                                                        
           WRITE FD-LIS-REGTO      FROM   CB-LD2-CABEC(WRK-IND1).       
           MOVE WRK-FS-LISTA         TO   WRK-FILE-STATUS.              
           PERFORM 0100-TESTA-FILE-STATUS.                              
                                                                        
      *---------------------------------------------------------------* 
       2900-IMPR-TAB-FIM.  EXIT.                                        
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       3000-IMPRI-EXTERNO         SECTION.                              
      *---------------------------------------------------------------* 
                                                                        
           IF  WRK-TIPO   EQUAL   1                                     
               WRITE FD-LIS-REGTO           FROM      WRK-DJDE-EXT1     
               MOVE WRK-FS-LISTA         TO        WRK-FILE-STATUS      
               PERFORM 0100-TESTA-FILE-STATUS                           
           ELSE                                                         
               WRITE FD-LIS-REGTO           FROM      WRK-DJDE-EXT      
               MOVE WRK-FS-LISTA         TO        WRK-FILE-STATUS      
               PERFORM 0100-TESTA-FILE-STATUS                           
           END-IF.                                                      
                                                                        
           WRITE FD-LIS-REGTO           FROM      CABEC-LBR1.           
           MOVE WRK-FS-LISTA         TO        WRK-FILE-STATUS.         
           PERFORM 0100-TESTA-FILE-STATUS.                              
                                                                        
           IF  WRK-TIPO  NOT EQUAL  1                                   
               WRITE FD-LIS-REGTO       FROM      CABEC-DD              
               MOVE WRK-FS-LISTA     TO        WRK-FILE-STATUS          
               PERFORM 0100-TESTA-FILE-STATUS                           
           END-IF.                                                      
                                                                        
           WRITE FD-LIS-REGTO           FROM      CABEC-1.              
           MOVE WRK-FS-LISTA         TO        WRK-FILE-STATUS.         
           PERFORM 0100-TESTA-FILE-STATUS.                              
                                                                        
           WRITE FD-LIS-REGTO           FROM      CABEC-2.              
           MOVE WRK-FS-LISTA         TO        WRK-FILE-STATUS.         
           PERFORM 0100-TESTA-FILE-STATUS.                              
                                                                        
           IF  WRK-TIPO      EQUAL  1                                   
               MOVE SPACES           TO        CB-3A-LADO1              
                                               CB-3B-LADO2              
           END-IF.                                                      
                                                                        
           WRITE FD-LIS-REGTO           FROM      CABEC-3.              
           MOVE WRK-FS-LISTA         TO        WRK-FILE-STATUS.         
           PERFORM 0100-TESTA-FILE-STATUS.                              
                                                                        
           WRITE FD-LIS-REGTO           FROM      CABEC-POSTNET1.       
           MOVE WRK-FS-LISTA         TO        WRK-FILE-STATUS.         
           PERFORM 0100-TESTA-FILE-STATUS.                              
                                                                        
           WRITE FD-LIS-REGTO           FROM      CABEC-POSTNET2.       
           MOVE WRK-FS-LISTA         TO        WRK-FILE-STATUS.         
           PERFORM 0100-TESTA-FILE-STATUS.                              
                                                                        
           WRITE FD-LIS-REGTO           FROM      CABEC-4.              
           MOVE WRK-FS-LISTA         TO        WRK-FILE-STATUS.         
           PERFORM 0100-TESTA-FILE-STATUS.                              
                                                                        
           WRITE FD-LIS-REGTO           FROM      CABEC-5.              
           MOVE WRK-FS-LISTA         TO        WRK-FILE-STATUS.         
           PERFORM 0100-TESTA-FILE-STATUS.                              
                                                                        
           IF  WRK-A-BAI-RES    EQUAL   SPACES                          
               MOVE  WRK-A-COM-RES        TO        WRK-A-BAI-RES       
               MOVE  SPACES              TO        WRK-A-COM-RES        
           END-IF.                                                      
                                                                        
           IF  WRK-B-BAI-RES    EQUAL   SPACES                          
               MOVE  WRK-B-COM-RES        TO        WRK-B-BAI-RES       
               MOVE  SPACES              TO        WRK-B-COM-RES        
           END-IF.                                                      
                                                                        
           IF    ( WRK-AA-LADO1      EQUAL   SPACES ) AND               
                 ( WRK-BB-LADO2      EQUAL   SPACES )                   
                 MOVE  WRK-A-LADO1          TO        CB-6A-LADO1       
                 MOVE  WRK-B-LADO2          TO        CB-6B-LADO2       
                 WRITE FD-LIS-REGTO       FROM      CABEC-6             
                 MOVE WRK-FS-LISTA     TO        WRK-FILE-STATUS        
                 PERFORM 0100-TESTA-FILE-STATUS                         
                 MOVE  ' '             TO      CB-LDBR2-CARRO           
                 WRITE FD-LIS-REGTO       FROM      CABEC-LBR2          
           ELSE                                                         
           IF    ( WRK-AA-LADO1      EQUAL   SPACES )                   
                 MOVE  WRK-A-LADO1          TO        CB-5AA-LADO1      
                 MOVE  WRK-BB-LADO2         TO        CB-5BB-LADO2      
                 WRITE FD-LIS-REGTO           FROM      CABEC-5A        
                 MOVE WRK-FS-LISTA         TO        WRK-FILE-STATUS    
                 PERFORM 0100-TESTA-FILE-STATUS                         
                 MOVE  SPACES              TO        CB-6A-LADO1        
                 MOVE  WRK-B-LADO2          TO        CB-6B-LADO2       
                 WRITE FD-LIS-REGTO           FROM      CABEC-6         
                 MOVE WRK-FS-LISTA         TO        WRK-FILE-STATUS    
                 PERFORM 0100-TESTA-FILE-STATUS                         
           ELSE                                                         
           IF    ( WRK-BB-LADO2      EQUAL   SPACES )                   
                 MOVE  WRK-AA-LADO1         TO        CB-5AA-LADO1      
                 MOVE  WRK-B-LADO2          TO        CB-5BB-LADO2      
                 WRITE FD-LIS-REGTO           FROM      CABEC-5A        
                 MOVE WRK-FS-LISTA         TO        WRK-FILE-STATUS    
                 PERFORM 0100-TESTA-FILE-STATUS                         
                 MOVE  SPACES              TO        CB-6B-LADO2        
                 MOVE  WRK-A-LADO1          TO        CB-6A-LADO1       
                 WRITE FD-LIS-REGTO           FROM      CABEC-6         
                 MOVE WRK-FS-LISTA         TO        WRK-FILE-STATUS    
                 PERFORM 0100-TESTA-FILE-STATUS                         
           ELSE                                                         
                 MOVE  WRK-AA-LADO1         TO        CB-5AA-LADO1      
                 MOVE  WRK-BB-LADO2         TO        CB-5BB-LADO2      
                 WRITE FD-LIS-REGTO           FROM      CABEC-5A        
                 MOVE WRK-FS-LISTA         TO        WRK-FILE-STATUS    
                 PERFORM 0100-TESTA-FILE-STATUS                         
                 MOVE  WRK-A-LADO1          TO        CB-6A-LADO1       
                 MOVE  WRK-B-LADO2          TO        CB-6B-LADO2       
                 WRITE FD-LIS-REGTO           FROM      CABEC-6         
                 MOVE WRK-FS-LISTA         TO        WRK-FILE-STATUS    
                 PERFORM 0100-TESTA-FILE-STATUS                         
           END-IF                                                       
           END-IF                                                       
           END-IF.                                                      
                                                                        
           MOVE  '0'                   TO      CB-LDBR2-CARRO.          
           WRITE FD-LIS-REGTO           FROM      CABEC-LBR2.           
                                                                        
           MOVE  ' '                 TO        CB-LCIF1-CARRO.          
           WRITE FD-LIS-REGTO           FROM      CABEC-CIF1.           
           MOVE WRK-FS-LISTA         TO        WRK-FILE-STATUS.         
           PERFORM 0100-TESTA-FILE-STATUS.                              
                                                                        
           MOVE  ' '                 TO        CB-LCIF1-CARRO.          
           WRITE FD-LIS-REGTO           FROM      CABEC-CIF1.           
           MOVE WRK-FS-LISTA         TO        WRK-FILE-STATUS.         
           PERFORM 0100-TESTA-FILE-STATUS.                              
                                                                        
           WRITE FD-LIS-REGTO           FROM      CABEC-CIF2.           
           MOVE WRK-FS-LISTA         TO        WRK-FILE-STATUS.         
           PERFORM 0100-TESTA-FILE-STATUS.                              
                                                                        
           IF  WRK-TIPO   EQUAL   1                                     
               WRITE FD-LIS-REGTO       FROM      CB-LINBRA3            
               MOVE WRK-FS-LISTA          TO      WRK-FILE-STATUS       
               PERFORM 0100-TESTA-FILE-STATUS                           
               WRITE FD-LIS-REGTO       FROM      CABEC-7               
               MOVE WRK-FS-LISTA          TO      WRK-FILE-STATUS       
               PERFORM 0100-TESTA-FILE-STATUS                           
           END-IF.                                                      
                                                                        
           MOVE  ' '                   TO      CB-LDBR2-CARRO.          
           WRITE FD-LIS-REGTO           FROM      CABEC-LBR2.           
                                                                        
      *---------------------------------------------------------------* 
       3000-IMPRI-EXTERNO-FIM.  EXIT.                                   
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       3100-GRAVA-ARQECT SECTION.                                       
      *---------------------------------------------------------------* 
                                                                        
           MOVE    WRK-NUMERO-LOTE-9003     TO    ECT-NUM-LOTE          
           MOVE    WRK-NUM-EXTRATO          TO    ECT-NUM-EXTRATO       
           MOVE    WRK-NUMCEP-POSTNET-9011  TO    ECT-NUM-CEP           
           MOVE    WRK-CPLCEP-POSTNET-9011  TO    ECT-SUF-CEP.          
           MOVE        'P'                  TO    ECT-TIPO-EXTRATO.     
           MOVE WRK-JOBNAME                 TO    ECT-JOBNAME           
           MOVE WRK-DATA-POSTAGEM-9002      TO    ECT-DT-FRANQ          
                                                                        
RST        MOVE    'S'                      TO    ECT-CONT-FAC.         
                                                                        
           WRITE REG-ARQECT.                                            
           MOVE WRK-FS-ARQECT         TO        WRK-FILE-STATUS.        
           PERFORM 0100-TESTA-FILE-STATUS.                              
                                                                        
      *---------------------------------------------------------------* 
       3100-GRAVA-ARQECT-FIM. EXIT.                                     
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       3200-MONTA-BARRA-CIF SECTION.                                    
      *---------------------------------------------------------------* 
                                                                        
           MOVE   'VRS001'               TO WRK-VERSAO-9010             
           MOVE   SPACES                 TO WRK-MENSAGEM-9010           
RST        MOVE   ECTSW001-DR-FAC        TO WRK-CODIGO-DR-POSTAGEM-9010.
RST        MOVE   ECTSW001-CD-ADM-FAC    TO WRK-CODIGO-ADM-CONTR-9010.  
RST        MOVE   ECTSW001-DEST-FAC      TO WRK-CODIGO-DESTINO-9010.    
                                                                        
           IF  WRK-FLAG-9002  EQUAL  ZEROS                              
               PERFORM  3300-OBTEM-NUMERO-LOTE  THRU                    
                        3300-OBTEM-NUMERO-LOTE-FIM                      
               MOVE          1           TO WRK-FLAG-9002               
           END-IF.                                                      
                                                                        
           MOVE    WRK-NUMERO-LOTE-9003  TO WRK-NUMERO-LOTE-9010.       
           ADD          1                TO WRK-NUM-EXTRATO.            
           MOVE    WRK-NUM-EXTRATO       TO WRK-NUMERO-SEQ-OBJETO-9010. 
                                                                        
           MOVE FD-DAT-ANO            TO WRK-ANO-POST.                  
           MOVE   ZEROS               TO WRK-CODIGO-RESERVA-9010.       
           MOVE FD-DAT-DIA            TO WRK-DIA-POSTAGEM-9010.         
           MOVE FD-DAT-MES            TO WRK-MES-POSTAGEM-9010.         
           MOVE WRK-AA-POST           TO WRK-ANO-POSTAGEM-9010.         
           MOVE     SPACES         TO WRK-CABEC-RETORNO-9010.           
                                                                        
           CALL   'BRAD9010'   USING   WRK-VERSAO-9010                  
                                       WRK-MENSAGEM-9010                
                                       WRK-CABEC-NUMERO-9010            
                                       WRK-CABEC-RETORNO-9010.          
                                                                        
           IF  RETURN-CODE  EQUAL  ZEROS                                
               NEXT SENTENCE                                            
           ELSE                                                         
               DISPLAY '*********** CLLP0782 **********'                
               DISPLAY '* CLLP0782-ERRO NO ACESSO AO  *'                
               DISPLAY '* MODULO BRAD9010.            *'                
               DISPLAY WRK-MENSAGEM-9010                                
               DISPLAY '*********** CLLP0782 **********'                
               CALL 'ILBOABN0'     USING WRK-ABEND                      
           END-IF.                                                      
                                                                        
           PERFORM  3100-GRAVA-ARQECT.                                  
                                                                        
      *---------------------------------------------------------------* 
       3200-MONTA-BARRA-CIF-FIM. EXIT.                                  
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       3300-OBTEM-NUMERO-LOTE SECTION.                                  
      *---------------------------------------------------------------* 
                                                                        
           MOVE 'VRS001'               TO WRK-VERSAO-9003.              
           MOVE SPACES                 TO WRK-MENSAGEM-9003.            
           MOVE FD-DAT-DIA                TO WRK-DIA-POSTAGEM-9002.     
           MOVE FD-DAT-MES                TO WRK-MES-POSTAGEM-9002.     
           MOVE FD-DAT-ANO                TO WRK-ANO-POST.              
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
               DISPLAY '*********** CLLP0782 **********'                
               DISPLAY '* CLLP0782-ERRO NO ACESSO AO  *'                
               DISPLAY '* MODULO ROTI9003.            *'                
               DISPLAY WRK-MENSAGEM-9003                                
               DISPLAY '*********** CLLP0782 **********'                
               CALL 'ILBOABN0'     USING WRK-ABEND                      
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       3300-OBTEM-NUMERO-LOTE-FIM. EXIT.                                
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       3400-OBTEM-BARRA-POSTNET SECTION.                                
      *---------------------------------------------------------------* 
                                                                        
           MOVE   'VRS001'               TO WRK-VERSAO-9011.            
           MOVE   SPACES                 TO WRK-MENSAGEM-9011.          
                                                                        
           CALL   'BRAD9011'   USING  WRK-VERSAO-9011                   
                                      WRK-MENSAGEM-9011                 
                                      WRK-CEP-POSTNET-9011              
                                      WRK-RETORNO-POSTNET-9011.         
                                                                        
           IF  RETURN-CODE  EQUAL  ZEROS                                
               NEXT SENTENCE                                            
           ELSE                                                         
               DISPLAY '*********** CLLP0782 **********'                
               DISPLAY '* CLLP0782-ERRO NO ACESSO AO  *'                
               DISPLAY '* MODULO BRAD9011.            *'                
               DISPLAY WRK-MENSAGEM-9011                                
               DISPLAY '*********** CLLP0782 **********'                
               CALL 'ILBOABN0'     USING WRK-ABEND                      
           END-IF.                                                      
                                                                        
      *---------------------------------------------------------------* 
       3400-OBTEM-BARRA-POSTNET-FIM. EXIT.                              
      *---------------------------------------------------------------* 
RST        EJECT                                                        
RST   *---------------------------------------------------------------* 
RST    3500-OBTEM-DADOS-FAC         SECTION.                            
RST   *---------------------------------------------------------------* 
RST                                                                     
RST        MOVE   'VRS001'               TO ECTSW001-VERSAO-FAC.        
RST        MOVE   SPACES                 TO ECTSW001-MENSAGEM-FAC.      
RST                                                                     
RST        CALL WRK-ECTS9010   USING  ECTSW001-VERSAO-FAC               
RST                                   ECTSW001-MENSAGEM-FAC             
RST                                   ECTSW001-CEP-FAC                  
RST                                   ECTSW001-JOBNAME                  
RST                                   ECTSW001-RETORNO-FAC.             
RST                                                                     
RST        IF  RETURN-CODE  NOT EQUAL  ZEROS                            
RST            DISPLAY '*********** CLLP0782 **********'                
RST            DISPLAY '* CLLP0782-ERRO NO ACESSO AO  *'                
RST            DISPLAY '* MODULO ECTS9010.            *'                
RST            DISPLAY ECTSW001-MENSAGEM-FAC                            
RST            DISPLAY '*********** CLLP0782 **********'                
RST            CALL 'ILBOABN0'     USING WRK-ABEND                      
RST        END-IF.                                                      
RST                                                                     
RST   *---------------------------------------------------------------* 
RST    3500-OBTEM-DADOS-FAC-FIM. EXIT.                                  
RST   *---------------------------------------------------------------* 
                                                                        
BSI   *---------------------------------------------------------------* 
BSI    3600-GRAVA-ARQCEDD   SECTION.                                    
BSI   *---------------------------------------------------------------* 
BSI                                                                     
BSI        MOVE WRK-CABEC-NUMERO-9010    TO CLLPZF-CINFO-FRANQ-CORSP.   
BSI        MOVE WRK-DATA-POSTAGEM-9002(1:2)  TO WRK-DIA-DB2             
BSI        MOVE WRK-DATA-POSTAGEM-9002(3:2)  TO WRK-MES-DB2.            
BSI        MOVE WRK-DATA-POSTAGEM-9002(5:4)  TO WRK-ANO-DB2.            
BSI        MOVE WRK-DATA-DB2             TO CLLPZF-DFAC-CORSP.          
BSI        MOVE WRK-DATA-FAC             TO CLLPZF-DPOSTAGEM-CORSP.     
BSI        MOVE 04120                    TO CLLPZF-GESTOR.              
BSI        MOVE 'CLLP0793'               TO CLLPZF-ROTINA.              
BSI        MOVE 237                      TO CLLPZF-CBCO-COBR-TARIF.     
BSI        MOVE WRK-AGENCIA              TO CLLPZF-COD-AGE-CLI.         
BSI        MOVE WRK-CONTA                TO CLLPZF-CONTA-COR-CLI.       
BSI        MOVE WRK-NOME-RESP            TO CLLPZF-ICLI-CORSP.          
BSI        MOVE WRK-CGCPF                TO CLLPZF-CD-CPF-CNPJ.         
BSI        MOVE WRK-FILIAL               TO WRK-DESCOMP-5.              
BSI        MOVE WRK-DESCOMP-5(2:4)       TO CLLPZF-CD-FILIAL-CNPJ.      
BSI        MOVE WRK-CTR                  TO WRK-DESCOMP-3.              
BSI        MOVE WRK-DESCOMP-3(2:2)       TO CLLPZF-CONTROLE-CPF-CNPJ.   
BSI                                                                     
BSI        MOVE  SPACES                  TO WRK-ENDENUM.                
BSI        STRING WRK-END-RESP ' ' WRK-DEV-NRO                          
BSI        DELIMITED BY '  '             INTO WRK-ENDENUM.              
BSI                                                                     
BSI        MOVE WRK-ENDENUM (1:40)       TO CLLPZF-ILOGDR-CLI-CORSP.    
BSI        MOVE WRK-DEV-BAIRRO           TO CLLPZF-IBAIRO-CLI-CORSP.    
BSI        MOVE WRK-CIDADE               TO CLLPZF-IMUN-CLI-CORSP.      
BSI        MOVE WRK-EST                  TO CLLPZF-CUF-CLI-CORSP.       
BSI        MOVE WRK-CEP                  TO CLLPZF-CCEP-CLI-CORSP.      
BSI        MOVE WRK-CEP-SUFIXO           TO CLLPZF-CCOMPL-CEP-CLI.      
BSI        MOVE ZEROS                    TO CLLPZF-CLUB.                
BSI        MOVE ZEROS                    TO CLLPZF-SEQ-ENDER            
BSI                                         CLLPZF-CPSSOA-JURID-CONTR   
BSI                                         CLLPZF-CTPO-CONTR-NEGOC     
BSI                                         CLLPZF-NSEQ-CONTR-NEGOC     
BSI                                         CLLPZF-CIDTFD-DOCTO-GERDR.  
BSI                                                                     
BSI        MOVE 'CLIE'                   TO CLLPZF-BASE-ENDER.          
BSI        MOVE 'CLLP'                   TO CLLPZF-CCUSTO-ORIGEM.       
BSI                                                                     
BSI        IF WRK-DEV-BAIRRO             EQUAL SPACES                   
BSI          MOVE 002                    TO CLLPZF-SIT-CORSP            
BSI        ELSE                                                         
BSI          MOVE 001                    TO CLLPZF-SIT-CORSP            
BSI        END-IF.                                                      
BSI                                                                     
BSI        IF WRK-FILIAL                 EQUAL ZEROS                    
BSI          MOVE 'F'                    TO CLLPZF-TIPO-CLIENTE         
BSI        ELSE                                                         
BSI          MOVE 'J'                    TO CLLPZF-TIPO-CLIENTE         
BSI        END-IF.                                                      
BSI                                                                     
BSI        WRITE CLLPZF-AREA.                                           
BSI        MOVE 'WRITE'             TO        WRK-FUNCAO.               
BSI        MOVE 'ARQCEDD'           TO        WRK-NOME-ARQ.             
BSI        MOVE WRK-FS-ARQCEDD      TO        WRK-FILE-STATUS.          
BSI        PERFORM 0100-TESTA-FILE-STATUS.                              
BSI                                                                     
BSI        INITIALIZE  CLLPZF-AREA.                                     
BSI                                                                     
BSI    3600-GRAVA-ARQCEDD-FIM. EXIT.                                    
BSI   *---------------------------------------------------------------* 

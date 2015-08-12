      *===============================================================* 
       IDENTIFICATION DIVISION.                                         
      *===============================================================* 
                                                                        
       PROGRAM-ID. RDAB0005.                                            
       AUTHOR. JOCILEIDE.                                               
                                                                        
      *----------------------------------------------------------------*
      *                        S T E F A N I N I                       *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMADOR...: JOCILEIDE AP. MARIANO   STEFANINI           *
      *    SUPERVISOR....: RICARDO PINHO         - STEFANINI           *
      *    ANALISTA......: HENRY HIGA            - CPM F. 4196-0698    *
      *    DATA..........: 08/01/2003                                  *
      *                                                                *
      *----------------------------------------------------------------*
      *    OBJETIVO......: 2002-1341-5-00 - CARGA DA BASE UNIFICADA.   *
      *                    OBTER DADOS DE PESSOAS JURIDICAS E CPSSOA-CA*
      *                    DTR, ATRAVES DO CPSSOA.                     *
      *----------------------------------------------------------------*
      *                    I N C ' S   /   A R Q U I V O S             *
      *    NOME     DESCRICAO                                          *
      *    I#FICAC6 PENDFICA - IMAGEM DA TABELA FICAV000               *
      *    I#RDAB01 ARQPARVV - ARQUIVO DE PARCELAS VENCIDAS/VINCENDAS  *
      *    I#RDAB05 ARQDAPES - ARQUIVO DE DADOS DA PESSOA              *
      *----------------------------------------------------------------*
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
                                                                        
           SELECT  ARQPARVV  ASSIGN  TO  UT-S-ARQPARVV                  
                   FILE      STATUS  IS  WRK-FS-ARQPARVV.               
                                                                        
           SELECT  PENDFICA  ASSIGN  TO  UT-S-PENDFICA                  
                   FILE      STATUS  IS  WRK-FS-PENDFICA.               
                                                                        
           SELECT  ARQDAPES  ASSIGN  TO  UT-S-ARQDAPES                  
                   FILE      STATUS  IS  WRK-FS-ARQDAPES.               
                                                                        
           SELECT  RELVAZIO  ASSIGN  TO  UT-S-RELVAZIO                  
                   FILE      STATUS  IS  WRK-FS-RELVAZIO.               
                                                                        
           SELECT  RELNENCO  ASSIGN  TO  UT-S-RELNENCO                  
                   FILE      STATUS  IS  WRK-FS-RELNENCO.               
                                                                        
           SELECT  RELTOTAL  ASSIGN  TO  UT-S-RELTOTAL                  
                   FILE      STATUS  IS  WRK-FS-RELTOTAL.               
                                                                        
      *===============================================================* 
       DATA DIVISION.                                                   
      *===============================================================* 
                                                                        
      *---------------------------------------------------------------* 
       FILE SECTION.                                                    
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
      *  INPUT....: ARQ. PARCELAS VENCIDAS/VINCENDAS                  * 
      *             ORG. SEQUENCIAL    -  LRECL = 150 BYTES           * 
      *---------------------------------------------------------------* 
                                                                        
       FD  ARQPARVV                                                     
           LABEL       RECORD    STANDARD                               
           RECORDING   MODE      F                                      
           BLOCK       CONTAINS  0.                                     
                                                                        
       COPY 'I#RDAB01'.                                                 
      *---------------------------------------------------------------* 
      *  INPUT....: ARQ RECEBIDO DO CADU - CADUV000                   * 
      *             ORG. SEQUENCIAL    -  LRECL = 320 BYTES           * 
      *---------------------------------------------------------------* 
                                                                        
       FD  PENDFICA                                                     
           LABEL       RECORD    STANDARD                               
           RECORDING   MODE      F                                      
           BLOCK       CONTAINS  0.                                     
                                                                        
      *COPY 'I#FICAC6'.                                                 
       COPY 'I#RDABAP'.                                                 
      *---------------------------------------------------------------* 
      *  OUTPUT...: ARQ. DE DADOS DA PESSOA                           * 
      *             ORG. SEQUENCIAL   -   LRECL = 500 BYTES           * 
      *---------------------------------------------------------------* 
                                                                        
       FD  ARQDAPES                                                     
           LABEL       RECORD    STANDARD                               
           RECORDING   MODE      F                                      
           BLOCK       CONTAINS  0.                                     
                                                                        
                                                                        
       COPY 'I#RDAB05'.                                                 
      *---------------------------------------------------------------* 
      *  OUTPUT...: RELATORIO DE ARQUIVO VAZIO.                       * 
      *             ORG. SEQUENCIAL   -   LRECL = 81 BYTES           *  
      *---------------------------------------------------------------* 
                                                                        
       FD  RELVAZIO                                                     
           LABEL       RECORD    STANDARD                               
           RECORDING   MODE      F                                      
           BLOCK       CONTAINS  0.                                     
                                                                        
       01  REG-RELVAZIO                   PIC  X(81).                   
                                                                        
      *---------------------------------------------------------------* 
      *  OUTPUT...: RELATORIO DE REGISTROS NAO ENCONTRADOS NO ARQUIVO * 
      *             IMAGEM DA TABELA FICAV000.                        * 
      *             ORG. SEQUENCIAL   -   LRECL = 132 BYTES           * 
      *---------------------------------------------------------------* 
                                                                        
       FD  RELNENCO                                                     
           LABEL       RECORD    STANDARD                               
           RECORDING   MODE      F                                      
           BLOCK       CONTAINS  0.                                     
                                                                        
       01  REG-RELNENCO                   PIC  X(132).                  
                                                                        
      *---------------------------------------------------------------* 
      *  OUTPUT...: RELATORIO DE TOTALIZACAO DE QTDE DE LIDOS         * 
      *             ORG. SEQUENCIAL   -   LRECL = 81 BYTES            * 
      *---------------------------------------------------------------* 
                                                                        
       FD  RELTOTAL                                                     
           LABEL       RECORD    STANDARD                               
           RECORDING   MODE      F                                      
           BLOCK       CONTAINS  0.                                     
                                                                        
       01  REG-RELTOTAL                  PIC  X(81).                    
                                                                        
      *---------------------------------------------------------------* 
       WORKING-STORAGE SECTION.                                         
      *---------------------------------------------------------------* 
      *COPY 'I#RDABAP'.                                                 
       01   FILLER                     PIC  X(32) VALUE                 
            'INICIO DA WORKING RDAB0005'.                               
                                                                        
      *-- ------------------------------------------------------------* 
      *          CAMPOS UTILIZADOS PARA CANCELAR PROCESSAMENTO        * 
      *---------------------------------------------------------------* 
                                                                        
       01  FILLER.                                                      
           03  WRK-FS-ARQPARVV         PIC  X(02) VALUE SPACES.         
           03  WRK-FS-PENDFICA         PIC  X(02) VALUE SPACES.         
           03  WRK-FS-ARQDAPES         PIC  X(02) VALUE SPACES.         
           03  WRK-FS-RELVAZIO         PIC  X(02) VALUE SPACES.         
           03  WRK-FS-RELNENCO         PIC  X(02) VALUE SPACES.         
           03  WRK-FS-RELTOTAL         PIC  X(02) VALUE SPACES.         
           03  WRK-OPERACAO            PIC  X(13) VALUE SPACES.         
           03  WRK-ABERTURA            PIC  X(13) VALUE 'NA ABERTURA'.  
           03  WRK-LEITURA             PIC  X(13) VALUE 'NA LEITURA'.   
           03  WRK-GRAVACAO            PIC  X(13) VALUE 'NA GRAVACAO'.  
           03  WRK-FECHAMENTO          PIC  X(13) VALUE 'NO FECHAMENTO'.
           03  WRK-ABEND               PIC S9(04) VALUE +1111  COMP.    
           03  WRK-BATCH               PIC  X(08) VALUE 'BATCH'.        
                                                                        
      *---------------------------------------------------------------* 
      *--  Acumuladores.                                                
                                                                        
       01  FILLER.                                                      
           03  ACU-LDS-ARQPARVV        PIC 9(09)  VALUE  ZEROS.         
           03  ACU-LDS-PENDFICA        PIC 9(09)  VALUE  ZEROS.         
           03  ACU-GRV-ARQDAPES        PIC 9(09)  VALUE  ZEROS.         
      *                                                                 
      *--  RELTOTAL.                                                    
           03  ACU-LINHAS              PIC 9(02)  VALUE  90.            
           03  ACU-PAGINAS             PIC 9(06)  VALUE  ZEROS.         
                                                                        
      *----------------------------------------------------------------*
      *--  Edicao.                                                      
                                                                        
       01  FILLER.                                                      
           03  WRK-EDIT01              PIC ZZZ.ZZZ.ZZ9.                 
           03  WRK-EDIT02              PIC ZZZ.ZZZ.ZZ9.                 
           03  WRK-EDIT03              PIC ZZZ.ZZZ.ZZ9.                 
           03  WRK-EDIT04              PIC ZZZ.ZZZ.ZZ9.                 
           03  WRK-EDIT05              PIC ZZZ.ZZZ.ZZ9.                 
                                                                        
      *----------------------------------------------------------------*
                                                                        
      *--  CHAVE ARQPARVV.                                              
       01  WRK-CHV-ARQPARVV.                                            
           03  WRK-CHV-PVV-CGCNUM      PIC 9(09) VALUE  ZEROS.
           03  WRK-CHV-PVV-CGCFIL      PIC 9(05) VALUE  ZEROS.          
           03  WRK-CHV-PVV-CGCCTR      PIC 9(02) VALUE  ZEROS.          
                                                                        
      *--  CHAVE PENDIRES.                                              
       01  WRK-CHV-PENDFICA.                                            
           03  WRK-CHV-PEN-CGCNUM      PIC 9(09) VALUE  ZEROS.
           03  WRK-CHV-PEN-CGCFIL      PIC 9(05) VALUE  ZEROS.          
           03  WRK-CHV-PEN-CGCCTR      PIC 9(02) VALUE  ZEROS.          
                                                                        
                                                                        
       01  WRK-CPF.                                                     
           03  FILLER                  PIC X(03) VALUE  SPACES.         
           03  WRK-CPF-NUM             PIC 999.999.999.                 
           03  FILLER                  PIC X(01) VALUE '-'.             
           03  WRK-CPF-CTR             PIC 99.                          
           03  FILLER                  PIC X(03) VALUE  SPACES.         
                                                                        
       01  WRK-CGC.                                                     
           03  WRK-CGC-NUM             PIC 999.999.999.                 
           03  FILLER                  PIC X(01) VALUE '/'.             
           03  WRK-CGC-FIL             PIC 99999.                       
           03  FILLER                  PIC X(01) VALUE '-'.             
           03  WRK-CGC-CTR             PIC 99.                          
                                                                        
BRQ059 01  WRK-CFONE-NOVO              PIC S9(11) COMP-3 VALUE ZEROS.   
BRQ059 01  WRK-CFONE-NOVO-R            REDEFINES                        
BRQ059     WRK-CFONE-NOVO              PIC  9(11) COMP-3.               
                                                                        
BRQ059 01  WRK-CFONE-NOVO-9            PIC  9(11) VALUE ZEROS.          
BRQ059 01  WRK-CFONE-NOVO-X            REDEFINES                        
BRQ059     WRK-CFONE-NOVO-9            PIC  X(11).                      
                                                                        
      *---------------------------------------------------------------* 
      *  CAMPO UTILIZADO PELA POOL7600 - OBTEM DATA E HORA DO SISTEMA * 
      *---------------------------------------------------------------* 
                                                                        
       01  WRK-7600-DATA-HORA.                                          
           03  DT-JULIANA-7600         PIC  9(05) COMP-3.               
           03  DT-AAMMDD-7600          PIC  9(07) COMP-3.               
           03  DT-AAAAMMDD-7600        PIC  9(09) COMP-3.               
           03  TI-HHMMSS-7600          PIC  9(07) COMP-3.               
           03  TI-HHMMSSMMMMMM-7600    PIC  9(13) COMP-3.               
           03  TIMESTAMP-7600.                                          
               05  ANO-7600            PIC  9(04).                      
               05  MES-7600            PIC  9(02).                      
               05  DIA-7600            PIC  9(02).                      
               05  HORA-7600           PIC  9(02).                      
               05  MINUTOS-7600        PIC  9(02).                      
               05  SEGUNDOS-7600       PIC  9(02).                      
               05  MICROSEGUNDOS-7600  PIC  9(06).                      
                                                                        
      *---------------------------------------------------------------* 
      *               CAMPOS AUXILIARES DE DATA E HORA                * 
      *---------------------------------------------------------------* 
                                                                        
       01  WRK-AAAAMMDD                PIC  9(08) VALUE ZEROS.          
       01  WRK-AAAAMMDD-R REDEFINES    WRK-AAAAMMDD.                    
           03  WRK-ANO                 PIC  9(04).                      
           03  WRK-MES                 PIC  9(02).                      
           03  WRK-DIA                 PIC  9(02).                      
                                                                        
       01  WRK-DDMMAAAA.                                                
           03  WRK-DIA                 PIC  9(02)  VALUE ZEROS.         
           03  FILLER                  PIC  X(01)  VALUE '/'.           
           03  WRK-MES                 PIC  9(02)  VALUE ZEROS.         
           03  FILLER                  PIC  X(01)  VALUE '/'.           
           03  WRK-ANO                 PIC  9(04)  VALUE ZEROS.         
       01  WRK-DDMMAAAA-R REDEFINES    WRK-DDMMAAAA                     
                                       PIC  X(10).                      
                                                                        
       01  WRK-HORA                    PIC  9(06)  VALUE ZEROS.         
       01  WRK-HORA-R    REDEFINES     WRK-HORA.                        
           03  WRK-HH                  PIC  9(02).                      
           03  WRK-MM                  PIC  9(02).                      
           03  WRK-SS                  PIC  9(02).                      
                                                                        
       01  WRK-HORA-EDITADA.                                            
           03  WRK-HH                  PIC  9(02).                      
           03  FILLER                  PIC  X(01)  VALUE ':'.           
           03  WRK-MM                  PIC  9(02).                      
           03  FILLER                  PIC  X(01)  VALUE ':'.           
           03  WRK-SS                  PIC  9(02).                      
       01  WRK-HORA-EDITADA-R          REDEFINES   WRK-HORA-EDITADA     
                                       PIC  X(08).                      
      *---------------------------------------------------------------* 
      *                      LAY-OUT DO RELATORIO                     * 
      *---------------------------------------------------------------* 
                                                                        
      *----------------------------------------------------------------*
      *--  RELVAZIO.                                                    
                                                                        
       01  CAB01-01.                                                    
           03  FILLER                  PIC X(01)  VALUE '1'.            
           03  FILLER                  PIC X(08)  VALUE 'RDAB0005'.     
           03  FILLER                  PIC X(12)  VALUE SPACES.         
           03  FILLER                  PIC X(33)  VALUE                 
               'B A N C O  B R A D E S C O  S / A'.                     
           03  FILLER                  PIC X(17)  VALUE SPACES.         
           03  CB1-01-DATA             PIC X(10) VALUE SPACES.          
                                                                        
       01  CAB02-01.                                                    
           03  FILLER                  PIC X(01)  VALUE ' '.            
           03  FILLER                  PIC X(23)  VALUE SPACES.         
           03  FILLER                  PIC X(26)  VALUE                 
               'RELATORIO DE ARQUIVO VAZIO'.                            
                                                                        
       01  CAB03-01.                                                    
           03  FILLER                  PIC X(01)  VALUE '-'.            
           03  FILLER                  PIC X(20)  VALUE SPACES.         
           03  FILLER                  PIC X(33)  VALUE                 
               'NOME DO PROGRAMA.......: RDAB0005'.                     
                                                                        
       01  LINDET1-1.                                                   
           03  FILLER                  PIC X(01)  VALUE ' '.            
           03  FILLER                  PIC X(20)  VALUE SPACES.         
           03  FILLER                  PIC X(24)  VALUE                 
               'NOME DO ARQUIVO VAZIO..:'.                              
           03  FILLER                  PIC X(01)  VALUE SPACES.         
           03  LD1-1-ARQUIVO           PIC X(08).                       
                                                                        
      *----------------------------------------------------------------*
      *--  RELTOTAL.                                                    
                                                                        
       01  CAB01-02.                                                    
           03  FILLER                  PIC X(01)  VALUE '1'.            
           03  FILLER                  PIC X(08)  VALUE 'RDAB0005'.     
           03  FILLER                  PIC X(12)  VALUE SPACES.         
           03  FILLER                  PIC X(33)  VALUE                 
               'B A N C O  B R A D E S C O  S / A'.                     
           03  FILLER                  PIC X(17)  VALUE SPACES.         
           03  CB1-02-DATA             PIC X(10) VALUE SPACES.          
                                                                        
       01  CAB02-02.                                                    
           03  FILLER                  PIC X(01)  VALUE ' '.            
           03  FILLER                  PIC X(25)  VALUE SPACES.         
           03  FILLER                  PIC X(25)  VALUE                 
               'RELATORIO DE TOTALIZACOES'.                             
           03  FILLER                  PIC X(29)  VALUE SPACES.         
                                                                        
       01  LINDET1-2.                                                   
           03  FILLER                  PIC X(01)  VALUE '0'.            
           03  FILLER                  PIC X(06)  VALUE SPACES.         
           03  FILLER                  PIC X(47)  VALUE                 
               'QUANTIDADE DE REGISTROS LIDOS    DO ARQPARVV..:'.       
           03  FILLER                  PIC X(01)  VALUE SPACES.         
           03  LD1-2-ARQPARVV          PIC ZZZ.ZZZ.ZZ9.                 
                                                                        
       01  LINDET2-2.                                                   
           03  FILLER                  PIC X(01)  VALUE ' '.            
           03  FILLER                  PIC X(06)  VALUE SPACES.         
           03  FILLER                  PIC X(47)  VALUE                 
               'QUANTIDADE DE REGISTROS LIDOS    DO PENDFICA..:'.       
           03  FILLER                  PIC X(01)  VALUE SPACES.         
           03  LD2-2-PENDFICA          PIC ZZZ.ZZZ.ZZ9.                 
                                                                        
       01  LINDET5-2.                                                   
           03  FILLER                  PIC X(01)  VALUE ' '.            
           03  FILLER                  PIC X(06)  VALUE SPACES.         
           03  FILLER                  PIC X(47)  VALUE                 
               'QUANTIDADE DE REGISTROS GRAVADOS DO ARQDAPES..:'.       
           03  FILLER                  PIC X(01)  VALUE SPACES.         
           03  LD5-2-ARQDAPES          PIC ZZZ.ZZZ.ZZ9.                 
                                                                        
      *----------------------------------------------------------------*
      *--  RELNENCO.                                                    
                                                                        
       01  CAB01-03.                                                    
           03  FILLER                  PIC X(01)  VALUE '1'.            
           03  FILLER                  PIC X(08)  VALUE 'RDAB0005'.     
           03  FILLER                  PIC X(36)  VALUE SPACES.         
           03  FILLER                  PIC X(33)  VALUE                 
               'B A N C O  B R A D E S C O  S / A'.                     
           03  FILLER                  PIC X(39)  VALUE SPACES.         
           03  FILLER                  PIC X(08)  VALUE 'PAGINA:'.      
           03  CB1-03-PAGINA           PIC ZZZ.ZZ9.                     
                                                                        
       01  CAB02-03.                                                    
           03  FILLER                  PIC X(01)  VALUE ' '.            
           03  CB1-03-DATA             PIC X(10) VALUE SPACES.          
           03  FILLER                  PIC X(15)  VALUE SPACES.         
           03  FILLER                  PIC X(75)  VALUE                 
               'RELATORIO DE REGISTROS NAO ENCONTRADOS NO ARQUIVO IMAGEM
      -        ' DA TABELA FICAV000'.                                   
           03  FILLER                  PIC X(26)  VALUE SPACES.         
           03  CB1-2-HORA              PIC X(08) VALUE SPACES.          
                                                                        
       01  CAB03-03.                                                    
           03  FILLER                  PIC X(01)  VALUE '-'.            
           03  FILLER                  PIC X(03)  VALUE SPACES.         
           03  FILLER                  PIC X(05)  VALUE 'BANCO'.        
           03  FILLER                  PIC X(09)  VALUE SPACES.         
           03  FILLER                  PIC X(07)  VALUE 'AGENCIA'.      
           03  FILLER                  PIC X(09)  VALUE SPACES.         
           03  FILLER                  PIC X(14)  VALUE                 
               'CONTA CORRENTE'.                                        
           03  FILLER                  PIC X(12)  VALUE SPACES.         
           03  FILLER                  PIC X(08)  VALUE 'CARTEIRA'.     
           03  FILLER                  PIC X(18)  VALUE SPACES.         
           03  FILLER                  PIC X(08)  VALUE 'CONTRATO'.     
           03  FILLER                  PIC X(24)  VALUE SPACES.         
           03  FILLER                  PIC X(09)  VALUE 'DOCUMENTO'.    
           03  FILLER                  PIC X(05)  VALUE SPACES.         
                                                                        
       01  CAB04-03.                                                    
           03  FILLER                  PIC X(01)  VALUE ' '.            
           03  FILLER                  PIC X(03)  VALUE SPACES.         
           03  FILLER                  PIC X(05)  VALUE '====='.        
           03  FILLER                  PIC X(09)  VALUE SPACES.         
           03  FILLER                  PIC X(07)  VALUE '======='.      
           03  FILLER                  PIC X(09)  VALUE SPACES.         
           03  FILLER                  PIC X(14)  VALUE                 
               '=============='.                                        
           03  FILLER                  PIC X(12)  VALUE SPACES.         
           03  FILLER                  PIC X(08)  VALUE '========'.     
           03  FILLER                  PIC X(14)  VALUE SPACES.         
           03  FILLER                  PIC X(17)  VALUE                 
               '================='.                                     
           03  FILLER                  PIC X(13)  VALUE SPACES.         
           03  FILLER                  PIC X(20)  VALUE                 
               '===================='.                                  
                                                                        
       01  LINDET1-3.                                                   
           03  FILLER                  PIC X(01)  VALUE ' '.            
           03  FILLER                  PIC X(04)  VALUE SPACES.         
           03  LD1-3-BANCO             PIC 9(03).                       
           03  FILLER                  PIC X(11)  VALUE SPACES.         
           03  LD2-3-AGENCIA           PIC 9(05).                       
           03  FILLER                  PIC X(11)  VALUE SPACES.         
           03  LD3-3-CONTA             PIC ZZZZZZZZZZZZ9.               
           03  FILLER                  PIC X(13)  VALUE SPACES.         
           03  LD4-3-CARTEIRA          PIC ZZ9.                         
           03  LD4-3-CARTEIRA-R REDEFINES LD4-3-CARTEIRA                
                                       PIC X(03).                       
           03  FILLER                  PIC X(16)  VALUE SPACES.         
           03  LD5-3-CONTRATO          PIC ZZZZZZZZZZZZZZZZ9.           
           03  FILLER                  PIC X(13)  VALUE SPACES.         
           03  LD6-3-DOCUMENTO         PIC X(20)  VALUE SPACES.         
                                                                        
      *---------------------------------------------------------------* 
       01   FILLER                     PIC  X(32)    VALUE              
            'FIM DA WORKING RDAB0005'.                                  
                                                                        
      *===============================================================* 
       PROCEDURE DIVISION.                                              
      *===============================================================* 
                                                                        
      *---------------------------------------------------------------* 
       00000-INICIAR SECTION.                                           
      *---------------------------------------------------------------* 
                                                                        
           PERFORM 10000-INICIALIZAR.                                   
                                                                        
           PERFORM 12000-FORMATAR-DATA-HORA.                            
                                                                        
           PERFORM 20000-LER-ARQPARVV                                   
           IF      WRK-FS-ARQPARVV     EQUAL '10'                       
                   DISPLAY '**************** RDAB0005 ***************'  
                   DISPLAY '*                                       *'  
                   DISPLAY '* ARQUIVO DE ENTRADA - ARQPARVV - VAZIO *'  
                   DISPLAY '*        PROCESSAMENTO ENCERRADO        *'  
                   DISPLAY '*                                       *'  
                   DISPLAY '**************** RDAB0005 ***************'  
                   MOVE    'ARQPARVV'  TO     LD1-1-ARQUIVO             
                   PERFORM  60000-IMPRIMIR-RELVAZIO                     
                   PERFORM  90000-FINALIZAR.                            
                                                                        
           PERFORM 30000-LER-PENDFICA                                   
           IF      WRK-FS-PENDFICA     EQUAL '10'                       
                   DISPLAY '**************** RDAB0005 ***************'  
                   DISPLAY '*                                       *'  
                   DISPLAY '* ARQUIVO DE ENTRADA - PENDFICA - VAZIO *'  
                   DISPLAY '*        PROCESSAMENTO ENCERRADO        *'  
                   DISPLAY '*                                       *'  
                   DISPLAY '**************** RDAB0005 ***************'  
                   MOVE    'PENDFICA'  TO     LD1-1-ARQUIVO             
                   PERFORM  60000-IMPRIMIR-RELVAZIO.                    
                                                                        
           PERFORM 40000-PROCESSAR     UNTIL                            
                  (WRK-FS-ARQPARVV     EQUAL '10')                      
                                                                        
           PERFORM 62000-IMPRIMIR-RELTOTAL                              
                                                                        
           PERFORM 70000-DISPLAY-TOTAIS                                 
                                                                        
           PERFORM 90000-FINALIZAR.                                     
                                                                        
      *---------------------------------------------------------------* 
       00000-99-FIM.   EXIT.                                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       10000-INICIALIZAR              SECTION.                          
      *---------------------------------------------------------------* 
                                                                        
           OPEN  INPUT  ARQPARVV                                        
                        PENDFICA                                        
                 OUTPUT ARQDAPES                                        
                        RELVAZIO                                        
                        RELTOTAL                                        
                        RELNENCO                                        
                                                                        
           MOVE  WRK-ABERTURA          TO    WRK-OPERACAO               
           PERFORM  11000-TESTAR-FILE-STATUS.                           
                                                                        
      *---------------------------------------------------------------* 
       10000-99-FIM.   EXIT.                                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       11000-TESTAR-FILE-STATUS       SECTION.                          
      *---------------------------------------------------------------* 
                                                                        
           PERFORM  11100-TESTAR-FS-ARQPARVV                            
           PERFORM  11200-TESTAR-FS-PENDFICA                            
           PERFORM  11500-TESTAR-FS-ARQDAPES.                           
           PERFORM  11600-TESTAR-FS-RELVAZIO.                           
           PERFORM  11700-TESTAR-FS-RELNENCO.                           
           PERFORM  11800-TESTAR-FS-RELTOTAL.                           
                                                                        
      *---------------------------------------------------------------* 
       11000-99-FIM.   EXIT.                                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       11100-TESTAR-FS-ARQPARVV       SECTION.                          
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-FS-ARQPARVV         NOT EQUAL  '00'                   
              DISPLAY '************** RDAB0005 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              ARQPARVV             *'           
              DISPLAY '*         FILE STATUS =  ' WRK-FS-ARQPARVV       
                                                 '         *'           
              DISPLAY '************** RDAB0005 *************'           
              CALL    'ILBOABN0'      USING    WRK-ABEND.               
                                                                        
      *---------------------------------------------------------------* 
       11100-99-FIM.   EXIT.                                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       11200-TESTAR-FS-PENDFICA       SECTION.                          
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-FS-PENDFICA         NOT EQUAL  '00'                   
              DISPLAY '************** RDAB0005 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              PENDFICA             *'           
              DISPLAY '*         FILE STATUS =  ' WRK-FS-PENDFICA       
                                                 '         *'           
              DISPLAY '************** RDAB0005 *************'           
              CALL    'ILBOABN0'      USING    WRK-ABEND.               
                                                                        
      *---------------------------------------------------------------* 
       11200-99-FIM.   EXIT.                                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       11500-TESTAR-FS-ARQDAPES        SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-FS-ARQDAPES           NOT EQUAL  '00'                 
              DISPLAY '************** RDAB0005 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              ARQDAPES              *'          
              DISPLAY '*         FILE STATUS =  ' WRK-FS-ARQDAPES       
                                                 '         *'           
              DISPLAY '************** RDAB0005 *************'           
              CALL    'ILBOABN0'      USING    WRK-ABEND.               
                                                                        
      *---------------------------------------------------------------* 
       11500-99-FIM.   EXIT.                                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       11600-TESTAR-FS-RELVAZIO        SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-FS-RELVAZIO           NOT EQUAL  '00'                 
              DISPLAY '************** RDAB0005 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              RELVAZIO              *'          
              DISPLAY '*         FILE STATUS =  ' WRK-FS-RELVAZIO       
                                                 '         *'           
              DISPLAY '************** RDAB0005 *************'           
              CALL    'ILBOABN0'      USING    WRK-ABEND.               
                                                                        
      *---------------------------------------------------------------* 
       11600-99-FIM.   EXIT.                                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       11700-TESTAR-FS-RELNENCO        SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-FS-RELNENCO           NOT EQUAL  '00'                 
              DISPLAY '************** RDAB0005 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              RELNENCO              *'          
              DISPLAY '*         FILE STATUS =  ' WRK-FS-RELNENCO       
                                                 '         *'           
              DISPLAY '************** RDAB0005 *************'           
              CALL    'ILBOABN0'      USING    WRK-ABEND.               
                                                                        
      *---------------------------------------------------------------* 
       11700-99-FIM.   EXIT.                                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       11800-TESTAR-FS-RELTOTAL        SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-FS-RELTOTAL           NOT EQUAL  '00'                 
              DISPLAY '************** RDAB0005 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              RELTOTAL              *'          
              DISPLAY '*         FILE STATUS =  ' WRK-FS-RELTOTAL       
                                                 '         *'           
              DISPLAY '************** RDAB0005 *************'           
              CALL    'ILBOABN0'      USING    WRK-ABEND.               
                                                                        
      *---------------------------------------------------------------* 
       11800-99-FIM.   EXIT.                                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       12000-FORMATAR-DATA-HORA SECTION.                                
      *---------------------------------------------------------------* 
                                                                        
           CALL  'POOL7600'            USING WRK-7600-DATA-HORA         
                                                                        
           MOVE   DT-AAAAMMDD-7600     TO    WRK-AAAAMMDD               
           MOVE CORR WRK-AAAAMMDD-R    TO    WRK-DDMMAAAA               
           MOVE   WRK-DDMMAAAA-R       TO    CB1-01-DATA                
                                             CB1-02-DATA                
                                             CB1-03-DATA.               
                                                                        
           MOVE   TI-HHMMSS-7600       TO    WRK-HORA                   
           MOVE CORR WRK-HORA-R        TO    WRK-HORA-EDITADA           
           MOVE   WRK-HORA-EDITADA-R   TO    CB1-2-HORA.                
                                                                        
      *---------------------------------------------------------------* 
       12000-99-FIM. EXIT.                                              
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       20000-LER-ARQPARVV            SECTION.                           
      *---------------------------------------------------------------* 
                                                                        
           READ    ARQPARVV                                             
                                                                        
           IF      WRK-FS-ARQPARVV     EQUAL  '10'                      
                   MOVE HIGH-VALUES    TO      WRK-CHV-ARQPARVV         
                   GO                  TO      20000-99-FIM.            
                                                                        
           MOVE    WRK-LEITURA         TO      WRK-OPERACAO             
           PERFORM 11100-TESTAR-FS-ARQPARVV                             
                                                                        
BRQ=I      MOVE    PVV-CGCNUM OF ARQPARVV TO  WRK-CHV-PVV-CGCNUM.
BRQ=I      MOVE    PVV-CGCFIL OF ARQPARVV TO  WRK-CHV-PVV-CGCFIL.       
BRQ=I      MOVE    PVV-CGCCTR OF ARQPARVV TO  WRK-CHV-PVV-CGCCTR.       
                                                                        
           ADD     1                      TO  ACU-LDS-ARQPARVV.         
                                                                        
      *---------------------------------------------------------------* 
       20000-99-FIM.   EXIT.                                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       30000-LER-PENDFICA            SECTION.                           
      *---------------------------------------------------------------* 
                                                                        
           READ    PENDFICA                                             
                                                                        
           IF      WRK-FS-PENDFICA     EQUAL  '10'                      
                   MOVE HIGH-VALUES    TO      WRK-CHV-PENDFICA         
                   GO                  TO      30000-99-FIM.            
                                                                        
           MOVE    WRK-LEITURA         TO      WRK-OPERACAO             
           PERFORM 11200-TESTAR-FS-PENDFICA                             
                                                                        
BRQ=I      MOVE    CADUV000-CCPF-CNPJ         TO  WRK-CHV-PEN-CGCNUM.
BRQ=I      MOVE    CADUV000-CFLIAL-CPF-CNPJ   TO  WRK-CHV-PEN-CGCFIL.   
BRQ=I      MOVE    CADUV000-CCTRL-CPF-CNPJ    TO  WRK-CHV-PEN-CGCCTR.   
                                                                        
           ADD     1                   TO      ACU-LDS-PENDFICA.        
                                                                        
      *---------------------------------------------------------------* 
       30000-99-FIM.   EXIT.                                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       40000-PROCESSAR               SECTION.                           
      *---------------------------------------------------------------* 
                                                                        
           IF      WRK-CHV-ARQPARVV   GREATER   WRK-CHV-PENDFICA        
                   PERFORM 30000-LER-PENDFICA                           
           ELSE                                                         
              IF      WRK-CHV-ARQPARVV    LESS    WRK-CHV-PENDFICA      
                      PERFORM 41000-ARQPARVV-MENOR                      
                      PERFORM 20000-LER-ARQPARVV                        
              ELSE                                                      
                      PERFORM 42000-CHAVES-IGUAIS                       
                      PERFORM 20000-LER-ARQPARVV                        
                      PERFORM 30000-LER-PENDFICA.                       
                                                                        
      *---------------------------------------------------------------* 
       40000-99-FIM.   EXIT.                                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       41000-ARQPARVV-MENOR          SECTION.                           
      *---------------------------------------------------------------* 
                                                                        
           INITIALIZE ADP-REGISTRO OF ARQDAPES                          
                                                                        
           MOVE  PVV-BANCO         OF ARQPARVV TO CBCO                  
           MOVE  PVV-CONTA         OF ARQPARVV TO CCTA-CORR             
           MOVE  PVV-AGENCIA       OF ARQPARVV TO CAG-BCRIA             
           MOVE  PVV-CGCNUM        OF ARQPARVV TO CBASE-CPF             
           MOVE  PVV-CGCFIL        OF ARQPARVV TO CFILIAL-CNPJ          
           MOVE  PVV-CGCCTR        OF ARQPARVV TO CCTRL-CNPJ-CPF        
           MOVE  PVV-NOME-PESSOA   OF ARQPARVV TO IPRIM-TTLAR           
           MOVE  PVV-CPOSTO-SERVC  OF ARQPARVV TO CPOSTO-SERVC          
           MOVE 'ZZ'                           TO STATUS-FICHA          
           MOVE SPACES             TO PVV-CPSSOA-CADTR OF ARQPARVV      
                                      PVV-CPSSOA-CDCLI OF ARQPARVV      
                                                                        
           PERFORM  50000-GRAVAR-ARQDAPES                               
                                                                        
           PERFORM  61000-IMPRIMIR-RELNENCO.                            
                                                                        
      *---------------------------------------------------------------* 
       41000-99-FIM.   EXIT.                                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       42000-CHAVES-IGUAIS           SECTION.                           
      *---------------------------------------------------------------* 
                                                                        
           INITIALIZE ADP-REGISTRO.                                     
                                                                        
           IF CADUV000-HULT-ATULZ EQUAL SPACES                          
             MOVE '01.01.0001'         TO  HATULZ                       
           ELSE                                                         
             STRING  CADUV000-HULT-ATULZ(9:2) '.'                       
                     CADUV000-HULT-ATULZ(6:2) '.'                       
                     CADUV000-HULT-ATULZ(1:4)                           
             DELIMITED BY SIZE           INTO  HATULZ                   
                                                                        
           END-IF.                                                      
                                                                        
           IF   CADUV000-CID-TPO-PSSOA    EQUAL  'J'                    
                MOVE '2'               TO CINDCD-TPO-LOGDR              
                MOVE CADUV000-ELOGDR-PSSOA(1:40)     TO ELOGDR-CLI      
                MOVE CADUV000-ELOGDR-NRO       TO ENUM-LOGDR            
                MOVE CADUV000-RCOMPL-ENDER(1:20)     TO ECOMPL-LOGDR    
                MOVE CADUV000-ICIDDE-ENDER(1:25)     TO IMUN-IBGE       
                MOVE CADUV000-CCEP             TO CCEP-CLI              
                MOVE CADUV000-CCEP-COMPL   TO CCEP-COMPL  OF ARQDAPES   
                MOVE CADUV000-CSGL-UF          TO CSGL-UF-CLI           
                MOVE '1'                       TO CINDCD-ORIGE          
                MOVE CADUV000-CDDD             TO CDDD-CLI              
                                                                        
BRQ059          IF   CADUV000-CFONE-NOVO  NOT NUMERIC                   
BRQ059               MOVE ZEROS           TO WRK-CFONE-NOVO             
BRQ059          ELSE                                                    
BRQ059               MOVE CADUV000-CFONE-NOVO  TO WRK-CFONE-NOVO        
BRQ059          END-IF                                                  
BRQ059                                                                  
BRQ059          MOVE WRK-CFONE-NOVO-R     TO WRK-CFONE-NOVO-9           
BRQ059          MOVE WRK-CFONE-NOVO-X     TO CFONE  OF ARQDAPES         
                MOVE '2'                  TO CINDCD-TPO-FONE            
                MOVE '2'                TO CIDTFD-TPO-PSSOA OF ARQDAPES 
           ELSE                                                         
                MOVE '1'                TO CIDTFD-TPO-PSSOA OF ARQDAPES 
                MOVE SPACES             TO CFONE OF ARQDAPES            
           END-IF                                                       
                                                                        
           MOVE PVV-BANCO        OF ARQPARVV TO CBCO                    
           MOVE PVV-CONTA        OF ARQPARVV TO CCTA-CORR               
           MOVE PVV-AGENCIA      OF ARQPARVV TO CAG-BCRIA               
           MOVE PVV-CGCNUM       OF ARQPARVV TO CBASE-CPF               
           MOVE PVV-CGCFIL       OF ARQPARVV TO CFILIAL-CNPJ            
           MOVE PVV-CGCCTR       OF ARQPARVV TO CCTRL-CNPJ-CPF          
           MOVE PVV-NOME-PESSOA  OF ARQPARVV TO IPRIM-TTLAR             
BRQ=I      IF PVV-CARTEIRA     OF ARQPARVV NOT NUMERIC                  
BRQ=I         MOVE PVV-CARTEIRA-R   OF ARQPARVV TO CARTEIRA-R           
BRQ=I      ELSE                                                         
BRQ=I         MOVE PVV-CARTEIRA     OF ARQPARVV TO CARTEIRA             
BRQ=I      END-IF                                                       
           MOVE PVV-CONTRATO     OF ARQPARVV TO CONTRATO                
           MOVE PVV-CPOSTO-SERVC OF ARQPARVV TO CPOSTO-SERVC            
BRQ=I      MOVE SPACES            TO CPSSOA-CADTR OF ARQDAPES           
BRQ=I      MOVE CADUV000-CCLUB            TO CCLUB        OF ARQDAPES   
                                                                        
BRQ=I      MOVE 'EF'                                TO  STATUS-FICHA    
           MOVE '1'                         TO CINDCD-ORIGE.            
                                                                        
           PERFORM 50000-GRAVAR-ARQDAPES.                               
                                                                        
      *---------------------------------------------------------------* 
       42000-99-FIM.   EXIT.                                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       50000-GRAVAR-ARQDAPES          SECTION.                          
      *---------------------------------------------------------------* 
                                                                        
           WRITE  ADP-REGISTRO                                          
           MOVE   WRK-GRAVACAO         TO      WRK-OPERACAO             
           PERFORM  11500-TESTAR-FS-ARQDAPES                            
                                                                        
           ADD 1                      TO   ACU-GRV-ARQDAPES.            
                                                                        
      *---------------------------------------------------------------* 
       50000-99-FIM.   EXIT.                                            
      *---------------------------------------------------------------* 
                                                                        
      *----------------------------------------------------------------*
       60000-IMPRIMIR-RELVAZIO          SECTION.                        
      *----------------------------------------------------------------*
                                                                        
           MOVE   WRK-GRAVACAO         TO      WRK-OPERACAO             
                                                                        
           PERFORM  60100-IMPRIMIR-CABECS.                              
                                                                        
           WRITE   REG-RELVAZIO        FROM  LINDET1-1                  
           PERFORM 11600-TESTAR-FS-RELVAZIO.                            
                                                                        
      *----------------------------------------------------------------*
       60000-99-FIM.                   EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       60100-IMPRIMIR-CABECS            SECTION.                        
      *----------------------------------------------------------------*
                                                                        
           MOVE   WRK-GRAVACAO         TO    WRK-OPERACAO               
           WRITE  REG-RELVAZIO        FROM    CAB01-01                  
           PERFORM  11600-TESTAR-FS-RELVAZIO.                           
                                                                        
           WRITE  REG-RELVAZIO        FROM    CAB02-01                  
           PERFORM  11600-TESTAR-FS-RELVAZIO.                           
                                                                        
           WRITE  REG-RELVAZIO        FROM    CAB03-01                  
           PERFORM  11600-TESTAR-FS-RELVAZIO.                           
                                                                        
      *----------------------------------------------------------------*
       60100-99-FIM.                   EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       61000-IMPRIMIR-RELNENCO          SECTION.                        
      *----------------------------------------------------------------*
                                                                        
           MOVE   WRK-GRAVACAO         TO      WRK-OPERACAO             
                                                                        
           IF      ACU-LINHAS          GREATER 59                       
                    PERFORM 61100-IMPRIMIR-CABECS.                      
                                                                        
           MOVE    PVV-BANCO     OF ARQPARVV  TO     LD1-3-BANCO        
           MOVE    PVV-AGENCIA   OF ARQPARVV  TO     LD2-3-AGENCIA      
           MOVE    PVV-CONTA     OF ARQPARVV  TO     LD3-3-CONTA        
                                                                        
           IF   PVV-CARTEIRA  OF ARQPARVV  NOT NUMERIC                  
                MOVE  PVV-CARTEIRA-R OF ARQPARVV  TO  LD4-3-CARTEIRA-R  
           ELSE                                                         
                MOVE    PVV-CARTEIRA  OF ARQPARVV  TO  LD4-3-CARTEIRA.  
                                                                        
           MOVE    PVV-CONTRATO  OF ARQPARVV  TO     LD5-3-CONTRATO     
                                                                        
           IF      PVV-CGCFIL OF ARQPARVV  EQUAL  ZEROS                 
                   MOVE  PVV-CGCNUM OF ARQPARVV  TO  WRK-CPF-NUM        
                   MOVE  PVV-CGCCTR OF ARQPARVV  TO  WRK-CPF-CTR        
                   MOVE  WRK-CPF                 TO  LD6-3-DOCUMENTO    
           ELSE                                                         
                   MOVE  PVV-CGCNUM OF ARQPARVV  TO  WRK-CGC-NUM        
                   MOVE  PVV-CGCFIL OF ARQPARVV  TO  WRK-CGC-FIL        
                   MOVE  PVV-CGCCTR OF ARQPARVV  TO  WRK-CGC-CTR        
                   MOVE  WRK-CGC                 TO  LD6-3-DOCUMENTO.   
                                                                        
           WRITE   REG-RELNENCO        FROM   LINDET1-3                 
           MOVE    WRK-GRAVACAO        TO     WRK-OPERACAO              
           PERFORM 11700-TESTAR-FS-RELNENCO                             
                                                                        
           ADD     01                  TO     ACU-LINHAS.               
                                                                        
      *----------------------------------------------------------------*
       61000-99-FIM.                   EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       61100-IMPRIMIR-CABECS            SECTION.                        
      *----------------------------------------------------------------*
                                                                        
           MOVE   WRK-GRAVACAO         TO    WRK-OPERACAO               
                                                                        
           ADD     1                   TO    ACU-PAGINAS                
           MOVE    ACU-PAGINAS         TO    CB1-03-PAGINA              
                                                                        
           WRITE  REG-RELNENCO        FROM    CAB01-03                  
           PERFORM  11700-TESTAR-FS-RELNENCO.                           
                                                                        
           WRITE  REG-RELNENCO        FROM    CAB02-03                  
           PERFORM  11700-TESTAR-FS-RELNENCO.                           
                                                                        
           WRITE  REG-RELNENCO        FROM    CAB03-03                  
           PERFORM  11700-TESTAR-FS-RELNENCO.                           
                                                                        
           WRITE  REG-RELNENCO        FROM    CAB04-03                  
           PERFORM  11700-TESTAR-FS-RELNENCO.                           
                                                                        
           MOVE    6                    TO      ACU-LINHAS.             
                                                                        
      *----------------------------------------------------------------*
       61100-99-FIM.                   EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       62000-IMPRIMIR-RELTOTAL          SECTION.                        
      *----------------------------------------------------------------*
                                                                        
           MOVE   WRK-GRAVACAO         TO      WRK-OPERACAO             
                                                                        
           PERFORM  62100-IMPRIMIR-CABECS.                              
                                                                        
                                                                        
           MOVE  ACU-LDS-ARQPARVV      TO   LD1-2-ARQPARVV              
           MOVE  ACU-LDS-PENDFICA      TO   LD2-2-PENDFICA              
           MOVE  ACU-GRV-ARQDAPES      TO   LD5-2-ARQDAPES              
                                                                        
           WRITE   REG-RELTOTAL       FROM  LINDET1-2                   
           PERFORM 11800-TESTAR-FS-RELTOTAL.                            
                                                                        
           WRITE   REG-RELTOTAL       FROM  LINDET2-2                   
           PERFORM 11800-TESTAR-FS-RELTOTAL.                            
                                                                        
           WRITE   REG-RELTOTAL       FROM  LINDET5-2                   
           PERFORM 11800-TESTAR-FS-RELTOTAL.                            
                                                                        
      *----------------------------------------------------------------*
       62000-99-FIM.                   EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       62100-IMPRIMIR-CABECS            SECTION.                        
      *----------------------------------------------------------------*
                                                                        
           MOVE   WRK-GRAVACAO         TO    WRK-OPERACAO               
                                                                        
           WRITE  REG-RELTOTAL        FROM    CAB01-02                  
           PERFORM  11800-TESTAR-FS-RELTOTAL.                           
                                                                        
           WRITE  REG-RELTOTAL        FROM    CAB02-02                  
           PERFORM  11800-TESTAR-FS-RELTOTAL.                           
                                                                        
      *----------------------------------------------------------------*
       62100-99-FIM.                   EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *---------------------------------------------------------------* 
       70000-DISPLAY-TOTAIS        SECTION.                             
      *---------------------------------------------------------------* 
                                                                        
           MOVE     ACU-LDS-ARQPARVV   TO    WRK-EDIT01                 
           MOVE     ACU-LDS-PENDFICA   TO    WRK-EDIT02                 
           MOVE     ACU-GRV-ARQDAPES   TO    WRK-EDIT05                 
                                                                        
           DISPLAY '******************** RDAB0005 ********************' 
           DISPLAY '*                                                *' 
           DISPLAY '*  TOTAL REG. LIDOS    - ARQPARVV : 'WRK-EDIT01'  *'
           DISPLAY '*  TOTAL REG. LIDOS    - PENDFICA : 'WRK-EDIT02'  *'
           DISPLAY '*  TOTAL REG. GRAVADOS - ARQDAPES : 'WRK-EDIT05'  *'
           DISPLAY '*                                                *' 
           DISPLAY '******************** RDAB0005 ********************'.
                                                                        
      *---------------------------------------------------------------* 
       70000-99-FIM.   EXIT.                                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       90000-FINALIZAR             SECTION.                             
      *---------------------------------------------------------------* 
                                                                        
           CLOSE  ARQPARVV                                              
                  PENDFICA                                              
                  ARQDAPES                                              
                  RELVAZIO                                              
                  RELNENCO                                              
                  RELTOTAL.                                             
                                                                        
           MOVE   WRK-FECHAMENTO       TO      WRK-OPERACAO             
           PERFORM  11000-TESTAR-FILE-STATUS.                           
                                                                        
           STOP RUN.                                                    
                                                                        
      *---------------------------------------------------------------* 
       90000-99-FIM.   EXIT.                                            
      *---------------------------------------------------------------* 
                                                                        

      *----------------------------------------------------------------*
       IDENTIFICATION                  DIVISION.                        
      *----------------------------------------------------------------*
                                                                        
       PROGRAM-ID.         RDAB0015.                                    
       AUTHOR.             WERNER.                                      
                                                                        
      *----------------------------------------------------------------*
      *                        S T E F A N I N I                       *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMADOR...: WERNER CJ DENZIN             - STEFANINI    *
      *    SUPERVISOR....: RICARDO PINHO                - STEFANINI    *
      *    ANALISTA......: HENRY HIGA                   - CPM          *
      *                                                                *
      *    DATA .........: 09/01/2003                                  *
      *    OBJETIVOS.....: OBTER DADOS DE PESSOA FISICA NO ARQ. IMAGEM *
      *                    DA TABELA FICAV001 ATRAVES DO CPSSOA.       *
      *                                                                *
      *----------------------------------------------------------------*
      *                                                               * 
BRQ059* ============================================================= * 
      * |                      ALTERACAO                            | * 
      * ------------------------------------------------------------+ * 
      *  ANALISTA     : ROBSON VELASQUES / BRQ - IT SERVICES        | * 
      *  PROGRAMADOR  : CRISTIANO SOUZA  / BRQ - IT SERVICES        | * 
      *  DATA         : 04/2012                                     | * 
      *  OBJETIVO     : INSERIR UM DIGITO A MAIS AOS CAMPOS DE      | * 
      *                 TELEFONES                                   | * 
      *  PROJETO      : 2012/0059 - ADEQUAR NRO TELEFONE(RES.553)   | * 
      * ============================================================+ * 
BRQ059***************************************************************** 
                                                                        
      *----------------------------------------------------------------*
       ENVIRONMENT                     DIVISION.                        
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.                         
      *----------------------------------------------------------------*
                                                                        
       SPECIAL-NAMES.                                                   
           DECIMAL-POINT IS COMMA.                                      
                                                                        
      *----------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.                         
      *----------------------------------------------------------------*
                                                                        
       FILE-CONTROL.                                                    
                                                                        
           SELECT  ARQPARVV   ASSIGN   TO  UT-S-ARQPARVV                
                   FILE       STATUS   IS  WRK-FS-ARQPARVV.             
                                                                        
           SELECT  PENDFICA   ASSIGN   TO  UT-S-PENDFICA                
                   FILE       STATUS   IS  WRK-FS-PENDFICA.             
                                                                        
           SELECT  ARQDAPES   ASSIGN   TO  UT-S-ARQDAPES                
                   FILE       STATUS   IS  WRK-FS-ARQDAPES.             
                                                                        
           SELECT  RELVAZIO   ASSIGN   TO  UT-S-RELVAZIO                
                   FILE       STATUS   IS  WRK-FS-RELVAZIO.             
                                                                        
           SELECT  RELTOTAL   ASSIGN   TO  UT-S-RELTOTAL                
                   FILE       STATUS   IS  WRK-FS-RELTOTAL.             
                                                                        
           SELECT  RELNENCO   ASSIGN   TO  UT-S-RELNENCO                
                   FILE       STATUS   IS  WRK-FS-RELNENCO.             
                                                                        
      *----------------------------------------------------------------*
       DATA                            DIVISION.                        
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       FILE                            SECTION.                         
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *--  INPUT ....: org. sequencial - LRECL = 0150 bytes.            
                                                                        
       FD  ARQPARVV                                                     
           RECORDING F                                                  
           BLOCK 0                                                      
           LABEL RECORD STANDARD.                                       
                                                                        
       COPY 'I#RDAB01'.                                                 
                                                                        
      *----------------------------------------------------------------*
      *--  INPUT ....: org. sequencial - LRECL = 0516 bytes.            
                                                                        
       FD  PENDFICA                                                     
           RECORDING F                                                  
           BLOCK 0                                                      
           LABEL RECORD STANDARD.                                       
                                                                        
      *COPY 'I#FICAC8'.                                                 
       COPY 'I#RDABAR'.                                                 
                                                                        
      *----------------------------------------------------------------*
      *--  OUTPUT....: org. sequencial - LRECL = 0500 bytes.            
                                                                        
       FD  ARQDAPES                                                     
           RECORDING F                                                  
           BLOCK 0                                                      
           LABEL RECORD STANDARD.                                       
                                                                        
       COPY 'I#RDAB05'.                                                 
                                                                        
      *----------------------------------------------------------------*
      *--  OUTPUT....: org. sequencial - LRECL = 0081 bytes.            
      *--  Relatorio de arquivo vazio                                   
                                                                        
       FD  RELVAZIO                                                     
           RECORDING F                                                  
           BLOCK 0                                                      
           LABEL RECORD STANDARD.                                       
                                                                        
       01  REG-RELVAZIO                PIC X(081).                      
                                                                        
      *----------------------------------------------------------------*
      *--  OUTPUT....: org. sequencial - LRECL = 0081 bytes.            
      *--  Relatorio de totalizacoes                                    
                                                                        
       FD  RELTOTAL                                                     
           RECORDING F                                                  
           BLOCK 0                                                      
           LABEL RECORD STANDARD.                                       
                                                                        
       01  REG-RELTOTAL                PIC X(081).                      
                                                                        
      *----------------------------------------------------------------*
      *--  OUTPUT....: org. sequencial - LRECL = 0132 bytes.            
      *--  Relatorio de regs n encontrados no arq imagem da tab FICAV001
                                                                        
       FD  RELNENCO                                                     
           RECORDING F                                                  
           BLOCK 0                                                      
           LABEL RECORD STANDARD.                                       
                                                                        
       01  REG-RELNENCO                PIC X(132).                      
                                                                        
      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.                         
      *----------------------------------------------------------------*
                                                                        
       01  FILLER                      PIC X(25)  VALUE                 
           'INICIO DA WORKING-STORAGE'.                                 
                                                                        
      *----------------------------------------------------------------*
      *--  Area de teste de FILE-STATUS.                                
                                                                        
       01  FILLER.                                                      
           03  WRK-FS-ARQPARVV         PIC X(02)  VALUE  SPACES.        
           03  WRK-FS-PENDFICA         PIC X(02)  VALUE  SPACES.        
           03  WRK-FS-ARQDAPES         PIC X(02)  VALUE  SPACES.        
           03  WRK-FS-RELVAZIO         PIC X(02)  VALUE  SPACES.        
           03  WRK-FS-RELTOTAL         PIC X(02)  VALUE  SPACES.        
           03  WRK-FS-RELNENCO         PIC X(02)  VALUE  SPACES.        
           03  WRK-OPERACAO            PIC X(13)  VALUE  SPACES.        
           03  WRK-ABERTURA            PIC X(13)  VALUE 'NA ABERTURA'.  
           03  WRK-LEITURA             PIC X(13)  VALUE 'NA LEITURA'.   
           03  WRK-GRAVACAO            PIC X(13)  VALUE 'NA GRAVACAO'.  
           03  WRK-FECHAMENTO          PIC X(13)  VALUE 'NO FECHAMENTO'.
           03  WRK-ABEND               PIC S9(04) VALUE +1111 COMP.     
                                                                        
      *----------------------------------------------------------------*
      *--  Acumuladores.                                                
                                                                        
       01  FILLER.                                                      
           03  ACU-LINHAS              PIC 9(02)  VALUE  90.            
           03  ACU-PAGINAS             PIC 9(06)  VALUE  ZEROS.         
           03  ACU-LDS-ARQPARVV        PIC 9(09)  VALUE  ZEROS.         
           03  ACU-LDS-PENDFICA        PIC 9(09)  VALUE  ZEROS.         
           03  ACU-GRV-ARQDAPES        PIC 9(09)  VALUE  ZEROS.         
                                                                        
      *----------------------------------------------------------------*
      *--  Edicao.                                                      
                                                                        
       01  FILLER.                                                      
           03  WRK-EDIT1               PIC ZZZ.ZZZ.ZZ9 VALUE ZEROS.
           03  WRK-EDIT2               PIC ZZZ.ZZZ.ZZ9 VALUE ZEROS.
           03  WRK-EDIT3               PIC ZZZ.ZZZ.ZZ9 VALUE ZEROS.
                                                                        
      *----------------------------------------------------------------*
      *--  Area auxiliar.                                               
                                                                        
BRQ059 01  WRK-CFONE                   PIC S9(11) VALUE ZEROS.          
BRQ059 01  WRK-CFONE-R                 REDEFINES                        
BRQ059     WRK-CFONE                   PIC  9(11).                      
                                                                        
BRQ059 01  WRK-CFONE-X.                                                 
BRQ059     05  WRK-CFONE-9             PIC  9(11) VALUE ZEROS.          
                                                                        
                                                                        
       01  FILLER.                                                      
           03  WRK-RELATO              PIC 9(02) VALUE  ZEROS.          
                                                                        
       01  WRK-CPF.                                                     
           03  FILLER                  PIC X(03) VALUE  SPACES.         
           03  WRK-CPF-NUM             PIC 999.999.999 VALUE ZEROS.
           03  FILLER                  PIC X(01) VALUE '-'.             
           03  WRK-CPF-CTR             PIC 99    VALUE ZEROS.
           03  FILLER                  PIC X(03) VALUE  SPACES.         
                                                                        
       01  WRK-CGC.                                                     
           03  WRK-CGC-NUM             PIC 999.999.999 VALUE ZEROS.
           03  FILLER                  PIC X(01) VALUE '/'.             
           03  WRK-CGC-FIL             PIC 99999 VALUE ZEROS.
           03  FILLER                  PIC X(01) VALUE '-'.             
           03  WRK-CGC-CTR             PIC 99 VALUE ZEROS.
                                                                        
      *--  CHAVE ARQPARVV.                                              
       01  WRK-CHV-ARQPARVV.                                            
           03  WRK-CHV-PAR-CCLUB       PIC 9(10) VALUE  ZEROS.          
                                                                        
                                                                        
      *--  CHAVE PENDFICA.                                              
       01  WRK-CHV-PENDFICA.                                            
           03  WRK-CHV-PEN-CCLUB       PIC 9(10) VALUE  ZEROS.          
                                                                        
      *----------------------------------------------------------------*
      *--  POOL7600.                                                    
                                                                        
       01  WRK-7600.                                                    
           03  WRK-7600-DT-JULIANA     PIC 9(05) COMP-3 VALUE ZEROS.
           03  WRK-7600-DT-AAMMDD      PIC 9(07) COMP-3 VALUE ZEROS.
           03  WRK-7600-DT-AAAAMMDD    PIC 9(09) COMP-3 VALUE ZEROS.
           03  WRK-7600-HHMMSS         PIC 9(07) COMP-3 VALUE ZEROS.
           03  WRK-7600-HHMMSSMMMMMM   PIC 9(13) COMP-3 VALUE ZEROS.
           03  WRK-7600-TIMESTAMP.                                      
               05  WRK-7600-ANO        PIC 9(04) VALUE ZEROS.
               05  WRK-7600-MES        PIC 9(02) VALUE ZEROS.
               05  WRK-7600-DIA        PIC 9(02) VALUE ZEROS.
               05  WRK-7600-HOR        PIC 9(02) VALUE ZEROS.
               05  WRK-7600-MIN        PIC 9(02) VALUE ZEROS.
               05  WRK-7600-SEG        PIC 9(02) VALUE ZEROS.
               05  WRK-7600-NAN        PIC 9(06) VALUE ZEROS.
                                                                        
      *----------------------------------------------------------------*
      *--  RELVAZIO.                                                    
                                                                        
       01  CAB01-01.                                                    
           03  CAB01-01-CARRO          PIC X(01)  VALUE '1'.            
           03  FILLER                  PIC X(08)  VALUE 'RDAB0015'.     
           03  FILLER                  PIC X(12)  VALUE SPACES.         
           03  FILLER                  PIC X(33)  VALUE                 
               'B A N C O  B R A D E S C O  S / A'.                     
           03  FILLER                  PIC X(17)  VALUE SPACES.         
           03  CAB01-01-DATA.                                           
               05  CAB01-01-DIA        PIC 9(02)  VALUE ZEROS.
               05  FILLER              PIC X(01)  VALUE '/'.            
               05  CAB01-01-MES        PIC 9(02)  VALUE ZEROS.
               05  FILLER              PIC X(01)  VALUE '/'.            
               05  CAB01-01-ANO        PIC 9(04)  VALUE ZEROS.
                                                                        
       01  CAB02-01.                                                    
           03  CAB02-01-CARRO          PIC X(01)  VALUE ' '.            
           03  FILLER                  PIC X(24)  VALUE SPACES.         
           03  FILLER                  PIC X(26)  VALUE                 
               'RELATORIO DE ARQUIVO VAZIO'.                            
           03  FILLER                  PIC X(30)  VALUE SPACES.         
                                                                        
       01  CAB03-01.                                                    
           03  CAB03-01-CARRO          PIC X(01)  VALUE '-'.            
           03  FILLER                  PIC X(20)  VALUE SPACES.         
           03  FILLER                  PIC X(33)  VALUE                 
               'NOME DO PROGRAMA.......: RDAB0015'.                     
           03  FILLER                  PIC X(27)  VALUE SPACES.         
                                                                        
       01  DET01-01.                                                    
           03  DET01-01-CARRO          PIC X(01)  VALUE ' '.            
           03  FILLER                  PIC X(20)  VALUE SPACES.         
           03  FILLER                  PIC X(24)  VALUE                 
               'NOME DO ARQUIVO VAZIO..:'.                              
           03  FILLER                  PIC X(01)  VALUE SPACES.         
           03  DET01-01-ARQUIVO        PIC X(08)  VALUE ZEROS.
           03  FILLER                  PIC X(27)  VALUE SPACES.         
                                                                        
      *----------------------------------------------------------------*
      *--  RELTOTAL.                                                    
                                                                        
       01  CAB01-02.                                                    
           03  CAB01-02-CARRO          PIC X(01)  VALUE '1'.            
           03  FILLER                  PIC X(08)  VALUE 'RDAB0015'.     
           03  FILLER                  PIC X(12)  VALUE SPACES.         
           03  FILLER                  PIC X(33)  VALUE                 
               'B A N C O  B R A D E S C O  S / A'.                     
           03  FILLER                  PIC X(17)  VALUE SPACES.         
           03  CAB01-02-DATA.                                           
               05  CAB01-02-DIA        PIC 9(02)  VALUE ZEROS.
               05  FILLER              PIC X(01)  VALUE '/'.            
               05  CAB01-02-MES        PIC 9(02)  VALUE ZEROS.
               05  FILLER              PIC X(01)  VALUE '/'.            
               05  CAB01-02-ANO        PIC 9(04)  VALUE ZEROS.
                                                                        
       01  CAB02-02.                                                    
           03  CAB02-02-CARRO          PIC X(01)  VALUE ' '.            
           03  FILLER                  PIC X(24)  VALUE SPACES.         
           03  FILLER                  PIC X(25)  VALUE                 
               'RELATORIO DE TOTALIZACOES'.                             
           03  FILLER                  PIC X(31)  VALUE SPACES.         
                                                                        
       01  DET01-02.                                                    
           03  DET01-02-CARRO          PIC X(01)  VALUE '-'.            
           03  FILLER                  PIC X(06)  VALUE SPACES.         
           03  FILLER                  PIC X(47)  VALUE                 
               'QUANTIDADE DE REGISTROS LIDOS    DO ARQPARVV..:'.       
           03  FILLER                  PIC X(01)  VALUE SPACES.         
           03  DET01-02-ARQPARVV       PIC ZZZ.ZZZ.ZZ9 VALUE ZEROS.
           03  FILLER                  PIC X(15)  VALUE SPACES.         
                                                                        
       01  DET02-02.                                                    
           03  DET02-02-CARRO          PIC X(01)  VALUE ' '.            
           03  FILLER                  PIC X(06)  VALUE SPACES.         
           03  FILLER                  PIC X(47)  VALUE                 
               'QUANTIDADE DE REGISTROS LIDOS    DO PENDFICA..:'.       
           03  FILLER                  PIC X(01)  VALUE SPACES.         
           03  DET02-02-PENDFICA       PIC ZZZ.ZZZ.ZZ9 VALUE ZEROS.
           03  FILLER                  PIC X(15)  VALUE SPACES.         
                                                                        
       01  DET03-02.                                                    
           03  DET03-02-CARRO          PIC X(01)  VALUE ' '.            
           03  FILLER                  PIC X(06)  VALUE SPACES.         
           03  FILLER                  PIC X(47)  VALUE                 
               'QUANTIDADE DE REGISTROS GRAVADOS DO ARQDAPES..:'.       
           03  FILLER                  PIC X(01)  VALUE SPACES.         
           03  DET03-02-ARQDAPES       PIC ZZZ.ZZZ.ZZ9 VALUE ZEROS.
           03  FILLER                  PIC X(15)  VALUE SPACES.         
                                                                        
      *----------------------------------------------------------------*
      *--  RELNENCO.                                                    
                                                                        
       01  CAB01-03.                                                    
           03  CAB01-03-CARRO          PIC X(01)  VALUE '1'.            
           03  FILLER                  PIC X(08)  VALUE 'RDAB0015'.     
           03  FILLER                  PIC X(36)  VALUE SPACES.         
           03  FILLER                  PIC X(33)  VALUE                 
               'B A N C O  B R A D E S C O  S / A'.                     
           03  FILLER                  PIC X(39)  VALUE SPACES.         
           03  FILLER                  PIC X(08)  VALUE 'PAGINA:'.      
           03  CAB01-03-PAGINA         PIC ZZZ.ZZ9 VALUE ZEROS.
                                                                        
       01  CAB02-03.                                                    
           03  CAB02-03-CARRO          PIC X(01)  VALUE ' '.            
           03  CAB02-03-DATA.                                           
               05  CAB02-03-DIA        PIC 9(02)  VALUE ZEROS.
               05  FILLER              PIC X(01)  VALUE '/'.            
               05  CAB02-03-MES        PIC 9(02)  VALUE ZEROS.
               05  FILLER              PIC X(01)  VALUE '/'.            
               05  CAB02-03-ANO        PIC 9(04)  VALUE ZEROS.
           03  FILLER                  PIC X(15)  VALUE SPACES.         
           03  FILLER                  PIC X(75)  VALUE                 
               'RELATORIO DE REGISTROS NAO ENCONTRADOS NO ARQUIVO IMAGEM
      -        ' DA TABELA FICAV001'.                                   
           03  FILLER                  PIC X(26)  VALUE SPACES.         
           03  CAB02-03-HORA.                                           
               05  CAB02-03-HOR        PIC 9(02)  VALUE ZEROS.
               05  FILLER              PIC X(01)  VALUE ':'.            
               05  CAB02-03-MIN        PIC 9(02)  VALUE ZEROS.
                                                                        
       01  CAB03-03.                                                    
           03  CAB03-03-CARRO          PIC X(01)  VALUE '-'.            
           03  FILLER                  PIC X(03)  VALUE SPACES.         
           03  FILLER                  PIC X(05)  VALUE 'BANCO'.        
           03  FILLER                  PIC X(04)  VALUE SPACES.         
           03  FILLER                  PIC X(07)  VALUE 'AGENCIA'.      
           03  FILLER                  PIC X(05)  VALUE SPACES.         
           03  FILLER                  PIC X(14)  VALUE                 
               'CONTA CORRENTE'.                                        
           03  FILLER                  PIC X(05)  VALUE SPACES.         
           03  FILLER                  PIC X(08)  VALUE 'CARTEIRA'.     
           03  FILLER                  PIC X(08)  VALUE SPACES.         
           03  FILLER                  PIC X(08)  VALUE 'CONTRATO'.     
           03  FILLER                  PIC X(14)  VALUE SPACES.         
           03  FILLER                  PIC X(09)  VALUE 'DOCUMENTO'.    
           03  FILLER                  PIC X(19)  VALUE SPACES.         
           03  FILLER                  PIC X(12)  VALUE 'CPSSOA-CADTR'. 
           03  FILLER                  PIC X(10)  VALUE SPACES.         
                                                                        
       01  CAB04-03.                                                    
           03  CAB04-03-CARRO          PIC X(01)  VALUE ' '.            
           03  FILLER                  PIC X(03)  VALUE SPACES.         
           03  FILLER                  PIC X(05)  VALUE '====='.        
           03  FILLER                  PIC X(04)  VALUE SPACES.         
           03  FILLER                  PIC X(07)  VALUE '======='.      
           03  FILLER                  PIC X(05)  VALUE SPACES.         
           03  FILLER                  PIC X(14)  VALUE                 
               '=============='.                                        
           03  FILLER                  PIC X(05)  VALUE SPACES.         
           03  FILLER                  PIC X(08)  VALUE '========'.     
           03  FILLER                  PIC X(04)  VALUE SPACES.         
           03  FILLER                  PIC X(17)  VALUE                 
               '================='.                                     
           03  FILLER                  PIC X(04)  VALUE SPACES.         
           03  FILLER                  PIC X(20)  VALUE                 
               '===================='.                                  
           03  FILLER                  PIC X(06)  VALUE SPACES.         
           03  FILLER                  PIC X(26)  VALUE                 
               '=========================='.                            
           03  FILLER                  PIC X(03)  VALUE SPACES.         
                                                                        
       01  DET01-03.                                                    
           03  DET01-03-CARRO          PIC X(01)  VALUE ' '.            
           03  FILLER                  PIC X(04)  VALUE SPACES.         
           03  DET01-03-BANCO          PIC 9(03)  VALUE ZEROS.
           03  FILLER                  PIC X(06)  VALUE SPACES.         
           03  DET01-03-AGENCIA        PIC 9(05)  VALUE ZEROS.
           03  FILLER                  PIC X(07)  VALUE SPACES.         
           03  DET01-03-CONTA          PIC ZZZZZZZZZZZZ9  VALUE ZEROS.
           03  FILLER                  PIC X(06)  VALUE SPACES.         
           03  DET01-03-CARTEIRA.                                       
            05 DET01-03-CARTEIRA-N     PIC ZZZZ9  VALUE ZEROS.
           03  FILLER                  PIC X(06)  VALUE SPACES.         
           03  DET01-03-CONTRATO       PIC ZZZZZZZZZZZZZZZZ9
                                                            VALUE ZEROS.
           03  FILLER                  PIC X(04)  VALUE SPACES.         
           03  DET01-03-DOCUMENTO      PIC X(20)  VALUE ZEROS.
           03  FILLER                  PIC X(06)  VALUE SPACES.         
           03  DET01-03-CPSSOA-CADTR   PIC X(26)  VALUE ZEROS.
           03  FILLER                  PIC X(03)  VALUE SPACES.         
                                                                        
      *----------------------------------------------------------------*
                                                                        
       01  FILLER                      PIC X(22)  VALUE                 
           'FIM DA WORKING-STORAGE'.                                    
                                                                        
      *----------------------------------------------------------------*
       PROCEDURE                       DIVISION.                        
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       00000-INICIAR                   SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           PERFORM 10000-INICIALIZAR                                    
                                                                        
           PERFORM 20000-LER-ARQPARVV                                   
           IF      WRK-FS-ARQPARVV     EQUAL '10'                       
                   DISPLAY '**************** RDAB0015 ***************'  
                   DISPLAY '*                                       *'  
                   DISPLAY '* ARQUIVO DE ENTRADA - ARQPARVV - VAZIO *'  
                   DISPLAY '*        PROCESSAMENTO ENCERRADO        *'  
                   DISPLAY '*                                       *'  
                   DISPLAY '**************** RDAB0015 ***************'  
                   MOVE     01         TO     WRK-RELATO                
                   MOVE    'ARQPARVV'  TO     DET01-01-ARQUIVO          
                   PERFORM  30000-IMPRIMIR                              
                   PERFORM  70000-FINALIZAR.                            
                                                                        
           PERFORM 40000-LER-PENDFICA                                   
           IF      WRK-FS-PENDFICA     EQUAL '10'                       
                   DISPLAY '**************** RDAB0015 ***************'  
                   DISPLAY '*                                       *'  
                   DISPLAY '* ARQUIVO DE ENTRADA - PENDFICA - VAZIO *'  
                   DISPLAY '*        PROCESSAMENTO ENCERRADO        *'  
                   DISPLAY '*                                       *'  
                   DISPLAY '**************** RDAB0015 ***************'  
                   MOVE     01         TO     WRK-RELATO                
                   MOVE    'PENDFICA'  TO     DET01-01-ARQUIVO          
                   PERFORM  30000-IMPRIMIR.                             
                                                                        
           PERFORM 50000-PROCESSAR     UNTIL                            
                  (WRK-FS-ARQPARVV     EQUAL '10')                      
                                                                        
           PERFORM 60000-EMITIR-TOTAIS                                  
           PERFORM 70000-FINALIZAR.                                     
                                                                        
      *----------------------------------------------------------------*
       00000-99-FIM.                   EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       10000-INICIALIZAR               SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           OPEN    INPUT  ARQPARVV                                      
                          PENDFICA                                      
                   OUTPUT ARQDAPES                                      
                          RELVAZIO                                      
                          RELTOTAL                                      
                          RELNENCO                                      
                                                                        
           MOVE    WRK-ABERTURA        TO    WRK-OPERACAO               
           PERFORM 11000-TESTAR-FILE-STATUS                             
           PERFORM 12000-CARREGAR-7600.                                 
                                                                        
      *----------------------------------------------------------------*
       10000-99-FIM.                   EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       11000-TESTAR-FILE-STATUS        SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           PERFORM  11100-TESTAR-FS-ARQPARVV                            
           PERFORM  11200-TESTAR-FS-PENDFICA                            
           PERFORM  11300-TESTAR-FS-ARQDAPES                            
           PERFORM  11400-TESTAR-FS-RELVAZIO                            
           PERFORM  11500-TESTAR-FS-RELTOTAL                            
           PERFORM  11600-TESTAR-FS-RELNENCO.                           
                                                                        
      *----------------------------------------------------------------*
       11000-99-FIM.                   EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       11100-TESTAR-FS-ARQPARVV        SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           IF    WRK-FS-ARQPARVV       NOT EQUAL   '00'                 
                 DISPLAY '************** RDAB0015 *************'        
                 DISPLAY '*   ERRO 'WRK-OPERACAO                        
                                               ' DO ARQUIVO   *'        
                 DISPLAY '*              ARQPARVV             *'        
                 DISPLAY '*         FILE STATUS =  '                    
                                    WRK-FS-ARQPARVV '         *'        
                 DISPLAY '************** RDAB0015 *************'        
                 CALL    'ILBOABN0'    USING         WRK-ABEND.         
                                                                        
      *----------------------------------------------------------------*
       11100-99-FIM.                   EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       11200-TESTAR-FS-PENDFICA        SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           IF    WRK-FS-PENDFICA       NOT EQUAL   '00'                 
                 DISPLAY '************** RDAB0015 *************'        
                 DISPLAY '*   ERRO 'WRK-OPERACAO                        
                                               ' DO ARQUIVO   *'        
                 DISPLAY '*              PENDFICA             *'        
                 DISPLAY '*         FILE STATUS =  '                    
                                    WRK-FS-PENDFICA '         *'        
                 DISPLAY '************** RDAB0015 *************'        
                 CALL    'ILBOABN0'    USING         WRK-ABEND.         
                                                                        
      *----------------------------------------------------------------*
       11200-99-FIM.                   EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       11300-TESTAR-FS-ARQDAPES        SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           IF    WRK-FS-ARQDAPES       NOT EQUAL   '00'                 
                 DISPLAY '************** RDAB0015 *************'        
                 DISPLAY '*   ERRO 'WRK-OPERACAO                        
                                               ' DO ARQUIVO   *'        
                 DISPLAY '*              ARQDAPES             *'        
                 DISPLAY '*         FILE STATUS =  '                    
                                    WRK-FS-ARQDAPES '         *'        
                 DISPLAY '************** RDAB0015 *************'        
                 CALL    'ILBOABN0'    USING         WRK-ABEND.         
                                                                        
      *----------------------------------------------------------------*
       11300-99-FIM.                   EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       11400-TESTAR-FS-RELVAZIO        SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           IF    WRK-FS-RELVAZIO       NOT EQUAL   '00'                 
                 DISPLAY '************** RDAB0015 *************'        
                 DISPLAY '*   ERRO 'WRK-OPERACAO                        
                                               ' DO ARQUIVO   *'        
                 DISPLAY '*              RELVAZIO             *'        
                 DISPLAY '*         FILE STATUS =  '                    
                                    WRK-FS-RELVAZIO '         *'        
                 DISPLAY '************** RDAB0015 *************'        
                 CALL    'ILBOABN0'    USING         WRK-ABEND.         
                                                                        
      *----------------------------------------------------------------*
       11400-99-FIM.                   EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       11500-TESTAR-FS-RELTOTAL        SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           IF    WRK-FS-RELTOTAL       NOT EQUAL   '00'                 
                 DISPLAY '************** RDAB0015 *************'        
                 DISPLAY '*   ERRO 'WRK-OPERACAO                        
                                               ' DO ARQUIVO   *'        
                 DISPLAY '*              RELTOTAL             *'        
                 DISPLAY '*         FILE STATUS =  '                    
                                    WRK-FS-RELTOTAL '         *'        
                 DISPLAY '************** RDAB0015 *************'        
                 CALL    'ILBOABN0'    USING         WRK-ABEND.         
                                                                        
      *----------------------------------------------------------------*
       11500-99-FIM.                   EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       11600-TESTAR-FS-RELNENCO        SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           IF    WRK-FS-RELNENCO       NOT EQUAL   '00'                 
                 DISPLAY '************** RDAB0015 *************'        
                 DISPLAY '*   ERRO 'WRK-OPERACAO                        
                                               ' DO ARQUIVO   *'        
                 DISPLAY '*              RELNENCO             *'        
                 DISPLAY '*         FILE STATUS =  '                    
                                    WRK-FS-RELNENCO '         *'        
                 DISPLAY '************** RDAB0015 *************'        
                 CALL    'ILBOABN0'    USING         WRK-ABEND.         
                                                                        
      *----------------------------------------------------------------*
       11600-99-FIM.                   EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       12000-CARREGAR-7600             SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           CALL   'POOL7600'           USING WRK-7600                   
                                                                        
           MOVE    WRK-7600-DIA        TO    CAB01-01-DIA               
                                             CAB01-02-DIA               
                                             CAB02-03-DIA               
                                                                        
           MOVE    WRK-7600-MES        TO    CAB01-01-MES               
                                             CAB01-02-MES               
                                             CAB02-03-MES               
                                                                        
           MOVE    WRK-7600-ANO        TO    CAB01-01-ANO               
                                             CAB01-02-ANO               
                                             CAB02-03-ANO               
                                                                        
           MOVE    WRK-7600-HOR        TO    CAB02-03-HOR               
           MOVE    WRK-7600-MIN        TO    CAB02-03-MIN.              
                                                                        
      *----------------------------------------------------------------*
       12000-99-FIM.                   EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       20000-LER-ARQPARVV              SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           READ    ARQPARVV                                             
                                                                        
           IF      WRK-FS-ARQPARVV     EQUAL  '10'                      
                   GO                  TO      20000-99-FIM.            
                                                                        
           MOVE    WRK-LEITURA         TO      WRK-OPERACAO             
           PERFORM 11100-TESTAR-FS-ARQPARVV                             
                                                                        
BRQ=I      MOVE    PVV-CCLUB  OF ARQPARVV TO  WRK-CHV-PAR-CCLUB.        
                                                                        
           ADD     1                   TO      ACU-LDS-ARQPARVV.        
                                                                        
      *----------------------------------------------------------------*
       20000-99-FIM.                   EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       30000-IMPRIMIR                  SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           EVALUATE WRK-RELATO                                          
              WHEN  01  PERFORM 31000-IMPRIMIR-DET                      
              WHEN  02  PERFORM 32000-IMPRIMIR-DET                      
              WHEN  03  PERFORM 33000-IMPRIMIR-DET                      
           END-EVALUATE.                                                
                                                                        
      *----------------------------------------------------------------*
       30000-99-FIM.                   EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       31000-IMPRIMIR-DET              SECTION.                         
      *----------------------------------------------------------------*
                                                                        
      *--  Procedimento para impressao de RELVAZIO.                     
      *--  Relatorio de arquivo vazio                                   
                                                                        
           MOVE    WRK-GRAVACAO        TO    WRK-OPERACAO               
                                                                        
           WRITE   REG-RELVAZIO        FROM  CAB01-01                   
           PERFORM 11400-TESTAR-FS-RELVAZIO                             
                                                                        
           WRITE   REG-RELVAZIO        FROM  CAB02-01                   
           PERFORM 11400-TESTAR-FS-RELVAZIO                             
                                                                        
           WRITE   REG-RELVAZIO        FROM  CAB03-01                   
           PERFORM 11400-TESTAR-FS-RELVAZIO                             
                                                                        
           WRITE   REG-RELVAZIO        FROM  DET01-01                   
           PERFORM 11400-TESTAR-FS-RELVAZIO.                            
                                                                        
      *----------------------------------------------------------------*
       31000-99-FIM.                   EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       32000-IMPRIMIR-DET              SECTION.                         
      *----------------------------------------------------------------*
                                                                        
      *--  Procedimento para impressao de RELTOTAL.                     
      *--  Relatorio de totalizacoes                                    
                                                                        
           MOVE    WRK-GRAVACAO        TO    WRK-OPERACAO               
                                                                        
           WRITE   REG-RELTOTAL        FROM  CAB01-02                   
           PERFORM 11500-TESTAR-FS-RELTOTAL                             
                                                                        
           WRITE   REG-RELTOTAL        FROM  CAB02-02                   
           PERFORM 11500-TESTAR-FS-RELTOTAL                             
                                                                        
           WRITE   REG-RELTOTAL        FROM  DET01-02                   
           PERFORM 11500-TESTAR-FS-RELTOTAL                             
                                                                        
           WRITE   REG-RELTOTAL        FROM  DET02-02                   
           PERFORM 11500-TESTAR-FS-RELTOTAL                             
                                                                        
           WRITE   REG-RELTOTAL        FROM  DET03-02                   
           PERFORM 11500-TESTAR-FS-RELTOTAL.                            
                                                                        
      *----------------------------------------------------------------*
       32000-99-FIM.                   EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       33000-IMPRIMIR-DET              SECTION.                         
      *----------------------------------------------------------------*
                                                                        
      *--  Procedimento para impressao de RELNENCO.                     
      *--  Relatorio de regs n encontrados no arq imagem da tab FICAV001
                                                                        
           IF      ACU-LINHAS         GREATER 59                        
                   PERFORM 33100-IMPRIMIR-CAB.                          
                                                                        
           MOVE    PVV-BANCO           TO     DET01-03-BANCO            
           MOVE    PVV-AGENCIA         TO     DET01-03-AGENCIA          
           MOVE    PVV-CONTA           TO     DET01-03-CONTA            
           IF      PVV-CARTEIRA    NOT        NUMERIC                   
                   MOVE PVV-CARTEIRA-R TO     DET01-03-CARTEIRA         
           ELSE                                                         
                   MOVE PVV-CARTEIRA   TO     DET01-03-CARTEIRA-N.      
           MOVE    PVV-CONTRATO        TO     DET01-03-CONTRATO         
                                                                        
           IF      PVV-CGCFIL          EQUAL  ZEROS                     
                   MOVE  PVV-CGCNUM    TO     WRK-CPF-NUM               
                   MOVE  PVV-CGCCTR    TO     WRK-CPF-CTR               
                   MOVE  WRK-CPF       TO     DET01-03-DOCUMENTO        
           ELSE                                                         
                   MOVE  PVV-CGCNUM    TO     WRK-CGC-NUM               
                   MOVE  PVV-CGCFIL    TO     WRK-CGC-FIL               
                   MOVE  PVV-CGCCTR    TO     WRK-CGC-CTR               
                   MOVE  WRK-CGC       TO     DET01-03-DOCUMENTO.       
                                                                        
           MOVE    PVV-CPSSOA-CADTR    TO     DET01-03-CPSSOA-CADTR     
                                                                        
           WRITE   REG-RELNENCO        FROM   DET01-03                  
           MOVE    WRK-GRAVACAO        TO     WRK-OPERACAO              
           PERFORM 11600-TESTAR-FS-RELNENCO                             
                                                                        
           ADD     01                  TO     ACU-LINHAS.               
                                                                        
      *----------------------------------------------------------------*
       33000-99-FIM.                   EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       33100-IMPRIMIR-CAB              SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           ADD     1                   TO    ACU-PAGINAS                
           MOVE    ACU-PAGINAS         TO    CAB01-03-PAGINA            
                                                                        
           MOVE    WRK-GRAVACAO        TO    WRK-OPERACAO               
                                                                        
           WRITE   REG-RELNENCO        FROM  CAB01-03                   
           PERFORM 11600-TESTAR-FS-RELNENCO                             
                                                                        
           WRITE   REG-RELNENCO        FROM  CAB02-03                   
           PERFORM 11600-TESTAR-FS-RELNENCO                             
                                                                        
           WRITE   REG-RELNENCO        FROM  CAB03-03                   
           PERFORM 11600-TESTAR-FS-RELNENCO                             
                                                                        
           WRITE   REG-RELNENCO        FROM  CAB04-03                   
           PERFORM 11600-TESTAR-FS-RELNENCO                             
                                                                        
           MOVE    06                  TO    ACU-LINHAS.                
                                                                        
      *----------------------------------------------------------------*
       33100-99-FIM.                   EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       40000-LER-PENDFICA              SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           READ    PENDFICA                                             
                                                                        
           IF      WRK-FS-PENDFICA     EQUAL  '10'                      
                   MOVE HIGH-VALUES    TO      WRK-CHV-PENDFICA         
                   GO                  TO      40000-99-FIM.            
                                                                        
           MOVE    WRK-LEITURA         TO      WRK-OPERACAO             
           PERFORM 11200-TESTAR-FS-PENDFICA                             
                                                                        
BRQ=I      MOVE    CADUV001-CCLUB             TO  WRK-CHV-PEN-CCLUB.    
                                                                        
           ADD     1                   TO      ACU-LDS-PENDFICA.        
                                                                        
      *----------------------------------------------------------------*
       40000-99-FIM.                   EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       50000-PROCESSAR                 SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           IF      WRK-CHV-ARQPARVV    EQUAL   WRK-CHV-PENDFICA         
                   PERFORM 51000-CHAVE-EQUAL                            
                   PERFORM 20000-LER-ARQPARVV                           
                   PERFORM 40000-LER-PENDFICA                           
           ELSE                                                         
           IF      WRK-CHV-ARQPARVV    LESS    WRK-CHV-PENDFICA         
                   PERFORM 52000-CHAVE-LESS                             
                   PERFORM 20000-LER-ARQPARVV                           
           ELSE                                                         
                   PERFORM 40000-LER-PENDFICA.                          
                                                                        
      *----------------------------------------------------------------*
       50000-99-FIM.                   EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       51000-CHAVE-EQUAL               SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           INITIALIZE ADP-REGISTRO.                                     
           MOVE     CADUV001-DNASC-FUNDC            TO DFUNDC-EMPR      
           MOVE     CADUV001-VRENDA-MES             TO VFATMT-EMPR      
           MOVE     CADUV001-IPAI-PSSOA-COPLT(1:40) TO IPAI-CLI         
           MOVE     CADUV001-IMAE-PSSOA-COPLT(1:40) TO IMAE-CLI         
           MOVE     CADUV001-ELOGDR-PSSOA(1:40)     TO ELOGDR-CLI
           MOVE     CADUV001-ELOGDR-NRO             TO ENUM-LOGDR       
           MOVE     CADUV001-RCOMPL-ENDER(1:20)     TO ECOMPL-LOGDR     
           MOVE     CADUV001-ICIDDE-ENDER(1:25)  TO IMUN-IBGE
           MOVE     CADUV001-CCEP        TO CCEP-CLI                    
                                                                        
           MOVE  CADUV001-CCEP-COMPL  TO CCEP-COMPL.                    
                                                                        
           MOVE CADUV001-CSGL-UF TO CSGL-UF-CLI.                        
                                                                        
           MOVE     1                        TO CINDCD-TPO-FONE         
                                                CINDCD-TPO-LOGDR        
           MOVE     CADUV001-CDDD        TO CDDD-CLI                    
                                                                        
           IF (CADUV001-CFONE-NOVO  IS NOT NUMERIC)
              MOVE ZEROS                TO  CFONE                       
           ELSE                                                         
BRQ059        MOVE     CADUV001-CFONE-NOVO  TO WRK-CFONE
BRQ059        MOVE     WRK-CFONE-R              TO WRK-CFONE-9          
BRQ059        MOVE     WRK-CFONE-X              TO CFONE                
           END-IF.                                                      
                                                                        
           MOVE     PVV-BANCO                TO CBCO                    
           MOVE     PVV-CONTA                TO CCTA-CORR               
           MOVE     PVV-AGENCIA              TO CAG-BCRIA               
           MOVE     PVV-CGCNUM               TO CBASE-CPF               
           MOVE     PVV-CGCFIL               TO CFILIAL-CNPJ            
           MOVE     PVV-CGCCTR               TO CCTRL-CNPJ-CPF          
           MOVE     PVV-CPSSOA-CADTR         TO CPSSOA-CADTR            
           MOVE     PVV-TPPESSOA             TO CIDTFD-TPO-PSSOA        
           MOVE     PVV-CPOSTO-SERVC         TO CPOSTO-SERVC            
BRQ=I      MOVE     PVV-CCLUB                TO CCLUB                   
           MOVE     1                        TO CINDCD-ORIGE            
           WRITE    ADP-REGISTRO                                        
           MOVE     WRK-GRAVACAO       TO       WRK-OPERACAO            
           PERFORM  11300-TESTAR-FS-ARQDAPES                            
           ADD      1                  TO       ACU-GRV-ARQDAPES.       
                                                                        
      *----------------------------------------------------------------*
       51000-99-FIM.                   EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       52000-CHAVE-LESS                SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE     03                 TO     WRK-RELATO                
           PERFORM  30000-IMPRIMIR.                                     
                                                                        
      *----------------------------------------------------------------*
       52000-99-FIM.                   EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       60000-EMITIR-TOTAIS             SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE     ACU-LDS-ARQPARVV   TO    DET01-02-ARQPARVV WRK-EDIT1
           MOVE     ACU-LDS-PENDFICA   TO    DET02-02-PENDFICA WRK-EDIT2
           MOVE     ACU-GRV-ARQDAPES   TO    DET03-02-ARQDAPES WRK-EDIT3
                                                                        
      *--  Imprimir totais de processamento.                            
                                                                        
           MOVE     02                 TO    WRK-RELATO                 
           PERFORM  30000-IMPRIMIR                                      
                                                                        
      *--- Emitir totais de processamento.                              
                                                                        
           DISPLAY '******************** RDAB0015 ********************' 
           DISPLAY '*                                                *' 
           DISPLAY '*  TOTAL REG. LIDOS    - ARQPARVV : 'WRK-EDIT1'  *' 
           DISPLAY '*  TOTAL REG. LIDOS    - PENDFICA : 'WRK-EDIT2'  *' 
           DISPLAY '*  TOTAL REG. GRAVADOS - ARQDAPES : 'WRK-EDIT3'  *' 
           DISPLAY '*                                                *' 
           DISPLAY '******************** RDAB0015 ********************'.
                                                                        
      *----------------------------------------------------------------*
       60000-99-FIM.                   EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       70000-FINALIZAR                 SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           CLOSE   ARQPARVV                                             
                   PENDFICA                                             
                   ARQDAPES                                             
                   RELVAZIO                                             
                   RELTOTAL                                             
                   RELNENCO                                             
                                                                        
           MOVE    WRK-FECHAMENTO      TO   WRK-OPERACAO                
           PERFORM 11000-TESTAR-FILE-STATUS                             
                                                                        
           STOP    RUN.                                                 
                                                                        
      *----------------------------------------------------------------*
       70000-99-FIM.                   EXIT.                            
      *----------------------------------------------------------------*
                                                                        

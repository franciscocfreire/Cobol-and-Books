      *---------------------------------------------------------------* 
       IDENTIFICATION DIVISION.                                         
      *---------------------------------------------------------------* 
                                                                        
       PROGRAM-ID. RDAB0024.                                            
       AUTHOR. LUCIANA.                                                 
                                                                        
      *---------------------------------------------------------------* 
      *         S T E F A N I N I    C O N S U L T O R I A            * 
      *---------------------------------------------------------------* 
      *                                                               * 
      *    PROGRAMA......: RDAB0024                                   * 
      *                                                               * 
      *    PROGRAMADORA..: LUCIANA SAIS         - STEFANINI / JGR     * 
      *    SUPERVISORA...: ANDREA MATTOS        - STEFANINI / JGR     * 
      *    ACQ......... .: RICARDO PINHO        - STEFANINI / JGR     * 
      *    ANALISTA......: HENRY HIGA           - CPM - GRUPO: 82     * 
      *    DATA........ .: 11/02/2003                                 * 
      *                                                               * 
      *    OBJETIVO......: CARGA DA BASE UNIFICADA                    * 
      *                    RECUPERAR DO ARQUIVO IMAGEM DA TABELA      * 
      *                    FICAV004 O CAMPO V004-S-CGRAU-LIG IGUAL    * 
      *                    A 1.                                       * 
      *                                                               * 
      *    ARQUIVOS......:                                            * 
      *        INC'S     DDNAME    DESCRICAO                          * 
      *    (I)  I#RDAB01  PENDENCI  ARQUIVO DE PARCELAS VENCIDAS/     * 
      *                             VINCENDAS.                        * 
      *    (I)  I#FICACD  ARQIFICA  ARQUIVO IMAGEM DA TABELA FICA.    * 
      *    (O)  I#RDAB01  PENDFICA  ARQUIVO DE PARCELAS VENCIDAS/     * 
      *                             VINCENDAS.                        * 
      *    (O)            RELNENCO  RELATORIO DE REGISTROS NAO ENCON- * 
      *                             TRADOS NO ARQUIVO IMAGEM DA TABE- * 
      *                             LA FICAV004.                      * 
      *    (O)            RELTOTAL  RELATORIO DE TOTALIZACAO DE QTDE. * 
      *                             DE LIDOS E GRAVADOS.              * 
      *                                                               * 
      *    MODULOS/POOL'S:                                            * 
      *        POOL7600 - OBTER DATA E HORA DO SISTEMA                * 
      *        ILBOABN0 - MODULO DE CANCELAMENTO PGM BATCH            * 
      *                                                               * 
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       ENVIRONMENT                     DIVISION.                        
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       CONFIGURATION                   SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
       SPECIAL-NAMES.                                                   
           DECIMAL-POINT IS COMMA.                                      
                                                                        
      *---------------------------------------------------------------* 
       INPUT-OUTPUT                    SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
       FILE-CONTROL.                                                    
                                                                        
           SELECT  PENDENCI   ASSIGN   TO UT-S-PENDENCI                 
                   FILE       STATUS   IS  WRK-FS-PENDENCI.             
                                                                        
           SELECT  ARQIFICA   ASSIGN   TO UT-S-ARQIFICA                 
                   FILE       STATUS   IS  WRK-FS-ARQIFICA.             
                                                                        
           SELECT  PENDFICA   ASSIGN   TO UT-S-PENDFICA                 
                   FILE       STATUS   IS  WRK-FS-PENDFICA.             
                                                                        
           SELECT  RELNENCO   ASSIGN   TO UT-S-RELNENCO                 
                   FILE       STATUS   IS  WRK-FS-RELNENCO.             
                                                                        
           SELECT  RELTOTAL   ASSIGN   TO UT-S-RELTOTAL                 
                   FILE       STATUS   IS  WRK-FS-RELTOTAL.             
                                                                        
      *===============================================================* 
       DATA DIVISION.                                                   
      *===============================================================* 
                                                                        
      *---------------------------------------------------------------* 
       FILE                            SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
      *  INPUT....: PENDENCI - ARQUIVO DE PARCELAS VENCIDAS/VINCENDAS * 
      *             ORG. SEQUENCIAL    -  LRECL =  150 BYTES          * 
      *---------------------------------------------------------------* 
                                                                        
       FD  PENDENCI                                                     
           RECORDING F                                                  
           BLOCK 0                                                      
           LABEL RECORD STANDARD.                                       
                                                                        
       COPY 'I#RDAB01'.

      *---------------------------------------------------------------* 
      *  INPUT....: ARQIFICA - ARQUIVO IMAGEM DA TABELA FICA.         * 
      *             ORG. SEQUENCIAL    -  LRECL =  154 BYTES          * 
      *---------------------------------------------------------------* 
                                                                        
       FD  ARQIFICA                                                     
           RECORDING F                                                  
           BLOCK 0                                                      
           LABEL RECORD STANDARD.                                       
                                                                        
       COPY 'I#FICACD'.
                                                                        
      *---------------------------------------------------------------* 
      *  OUTPUT...: PENDFICA - ARQUIVO DE PARCELAS VENCIDAS/VINCENDAS * 
      *             ORG. SEQUENCIAL    -  LRECL =  150 BYTES          * 
      *---------------------------------------------------------------* 
                                                                        
       FD  PENDFICA                                                     
           RECORDING F                                                  
           BLOCK 0                                                      
           LABEL RECORD STANDARD.                                       
                                                                        
       COPY 'I#RDAB01'.
                                                                        
      *---------------------------------------------------------------* 
      *  OUTPUT...: RELNENCO - RELATORIO DE REGISTROS NAO ENCONTRADOS * 
      *                        NO ARQUIVO IMAGEM DA TABELA FICAV004.  * 
      *             ORG. SEQUENCIAL    -  LRECL =  132 BYTES          * 
      *---------------------------------------------------------------* 
                                                                        
       FD  RELNENCO                                                     
           RECORDING F                                                  
           BLOCK 0                                                      
           LABEL RECORD STANDARD.                                       
                                                                        
       01  REG-RELNENCO                PIC  X(132).                     
                                                                        
      *---------------------------------------------------------------* 
      *  OUTPUT...: RELTOTAL - RELATORIO DE TOTALIZACAO DE QTDE. DE   * 
      *                        LIDOS E GRAVADOS                       * 
      *             ORG. SEQUENCIAL    -  LRECL =  081 BYTES          * 
      *---------------------------------------------------------------* 
                                                                        
       FD  RELTOTAL                                                     
           RECORDING F                                                  
           BLOCK 0                                                      
           LABEL RECORD STANDARD.                                       
                                                                        
       01  REG-RELTOTAL                PIC  X(81).                      
                                                                        
      *---------------------------------------------------------------* 
       WORKING-STORAGE                 SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
       01  FILLER                      PIC  X(25)  VALUE                
           'INICIO DA WORKING-STORAGE'.                                 
                                                                        
      *---------------------------------------------------------------* 
      *    AREA P/ VARIAVEIS DE FILE STATUS                           * 
      *---------------------------------------------------------------* 
                                                                        
       01  FILLER.                                                      
           03  WRK-FS-PENDENCI         PIC  X(02)  VALUE SPACES.        
           03  WRK-FS-ARQIFICA         PIC  X(02)  VALUE SPACES.        
           03  WRK-FS-PENDFICA         PIC  X(02)  VALUE SPACES.        
           03  WRK-FS-RELTOTAL         PIC  X(02)  VALUE SPACES.        
           03  WRK-FS-RELNENCO         PIC  X(02)  VALUE SPACES.        
           03  WRK-OPERACAO            PIC  X(13)  VALUE SPACES.        
           03  WRK-ABERTURA            PIC  X(13)  VALUE 'NA ABERTURA'. 
           03  WRK-LEITURA             PIC  X(13)  VALUE 'NA LEITURA'.  
           03  WRK-GRAVACAO            PIC  X(13)  VALUE 'NA GRAVACAO'. 
           03  WRK-FECHAMENTO          PIC  X(13) VALUE 'NO FECHAMENTO'.
           03  WRK-ABEND               PIC S9(04)  VALUE +1111 COMP.    
           03  WRK-BATCH               PIC  X(08)  VALUE 'BATCH'.       
                                                                        
      *---------------------------------------------------------------* 
      *    ACUMULADORES                                               * 
      *---------------------------------------------------------------* 
                                                                        
       01  FILLER.                                                      
           03  ACU-LINHAS              PIC  9(02)  VALUE 60.            
           03  ACU-PAGINAS             PIC  9(06)  VALUE ZEROS.         
           03  ACU-LDS-PENDENCI        PIC  9(09)  VALUE ZEROS.         
           03  ACU-LDS-ARQIFICA        PIC  9(09)  VALUE ZEROS.         
           03  ACU-GRV-PENDFICA        PIC  9(09)  VALUE ZEROS.         
                                                                        
      *---------------------------------------------------------------* 
      *    MASCARAS DE EDICAO                                         * 
      *---------------------------------------------------------------* 
                                                                        
       01  FILLER.                                                      
           03  WRK-MASC-LDS-PENDENCI   PIC  ZZZ.ZZZ.ZZ9.                
           03  WRK-MASC-LDS-ARQIFICA   PIC  ZZZ.ZZZ.ZZ9.                
           03  WRK-MASC-GRV-PENDFICA   PIC  ZZZ.ZZZ.ZZ9.                
                                                                        
       01  WRK-CPF.                                                     
           03  FILLER                  PIC  X(03)  VALUE SPACES.        
           03  WRK-CPF-NUM             PIC  999.999.999.                
           03  FILLER                  PIC  X(01)  VALUE '-'.           
           03  WRK-CPF-CTR             PIC  99.                         
           03  FILLER                  PIC  X(03)  VALUE SPACES.        
                                                                        
       01  WRK-CGC.                                                     
           03  WRK-CGC-NUM             PIC  999.999.999.                
           03  FILLER                  PIC  X(01)  VALUE '/'.           
           03  WRK-CGC-FIL             PIC  99999.                      
           03  FILLER                  PIC  X(01)  VALUE '-'.           
           03  WRK-CGC-CTR             PIC  99.                         
                                                                        
      *---------------------------------------------------------------* 
      *    AREA P/ VARIAVEIS AUXILIARES                               * 
      *---------------------------------------------------------------* 
                                                                        
       01  WRK-CHAVE-PENDENCI.                                          
           03 WRK-CHV-CPSSOA-PENDENCI      PIC  X(26).                  
                                                                        
       01  WRK-CHAVE-ARQIFICA-ANT.                                      
           03 WRK-CHV-CPSSOA-ARQIFICA-ANT  PIC  X(26).                  
                                                                        
       01  WRK-CHAVE-ARQIFICA-ATU.                                      
           03 WRK-CHV-CPSSOA-ARQIFICA-ATU  PIC  X(26).                  
                                                                        
       01  FILLER.                                                      
           03  WRK-ENCONTRADO          PIC  X(01)  VALUE 'S'.           
                                                                        
      *---------------------------------------------------------------* 
      *    AREA DA POOL7600                                           * 
      *---------------------------------------------------------------* 
                                                                        
       01  WRK-7600.                                                    
           03  WRK-7600-DT-JULIANA     PIC  9(05) COMP-3.               
           03  WRK-7600-DT-AAMMDD      PIC  9(07) COMP-3.               
           03  WRK-7600-DT-AAAAMMDD    PIC  9(09) COMP-3.               
           03  WRK-7600-HHMMSS         PIC  9(07) COMP-3.               
           03  WRK-7600-HHMMSSMMMMMM   PIC  9(13) COMP-3.               
           03  WRK-7600-TIMESTAMP.                                      
               05  WRK-7600-ANO        PIC  9(04).                      
               05  WRK-7600-MES        PIC  9(02).                      
               05  WRK-7600-DIA        PIC  9(02).                      
               05  WRK-7600-HOR        PIC  9(02).                      
               05  WRK-7600-MIN        PIC  9(02).                      
               05  WRK-7600-SEG        PIC  9(02).                      
               05  WRK-7600-NAN        PIC  9(06).                      
                                                                        
      *---------------------------------------------------------------* 
      *    DEFINICAO DE RELATORIOS                                    * 
      *---------------------------------------------------------------* 
      *---------------------------------------------------------------* 
      *    DEFINICAO DO RELATORIO: RELNENCO                           * 
      *---------------------------------------------------------------* 
                                                                        
       01  CAB01-01.                                                    
           03  CAB01-01-CARRO          PIC  X(01)  VALUE '1'.           
           03  FILLER                  PIC  X(08)  VALUE 'RDAB0024'.    
           03  FILLER                  PIC  X(36)  VALUE SPACES.        
           03  FILLER                  PIC  X(33)  VALUE                
               'B A N C O  B R A D E S C O  S / A'.                     
           03  FILLER                  PIC  X(39)  VALUE SPACES.        
           03  FILLER                  PIC  X(08)  VALUE 'PAGINA:'.     
           03  CAB01-01-PAGINA         PIC  ZZZ.ZZ9.                    
                                                                        
       01  CAB02-01.                                                    
           03  CAB02-01-CARRO          PIC  X(01)  VALUE ' '.           
           03  CAB02-01-DATA.                                           
               05  CAB02-01-DIA        PIC  9(02).                      
               05  FILLER              PIC  X(01)  VALUE '/'.           
               05  CAB02-01-MES        PIC  9(02).                      
               05  FILLER              PIC  X(01)  VALUE '/'.           
               05  CAB02-01-ANO        PIC  9(04).                      
           03  FILLER                  PIC  X(15)  VALUE SPACES.        
           03  FILLER                  PIC  X(75)  VALUE                
               'RELATORIO DE REGISTROS NAO ENCONTRADOS NO ARQUIVO IMAGEM
      -        ' DA TABELA FICAV004'.                                   
           03  FILLER                  PIC  X(26)  VALUE SPACES.        
           03  CAB02-01-HORA.                                           
               05  CAB02-01-HOR        PIC  9(02).                      
               05  FILLER              PIC  X(01)  VALUE ':'.           
               05  CAB02-01-MIN        PIC  9(02).                      
                                                                        
       01  CAB03-01.                                                    
           03  CAB03-01-CARRO          PIC  X(01)  VALUE '-'.           
           03  FILLER                  PIC  X(03)  VALUE SPACES.        
           03  FILLER                  PIC  X(05)  VALUE 'BANCO'.       
           03  FILLER                  PIC  X(09)  VALUE SPACES.        
           03  FILLER                  PIC  X(07)  VALUE 'AGENCIA'.     
           03  FILLER                  PIC  X(09)  VALUE SPACES.        
           03  FILLER                  PIC  X(14)  VALUE                
               'CONTA CORRENTE'.                                        
           03  FILLER                  PIC  X(12)  VALUE SPACES.        
           03  FILLER                  PIC  X(08)  VALUE 'CARTEIRA'.    
           03  FILLER                  PIC  X(18)  VALUE SPACES.        
           03  FILLER                  PIC  X(08)  VALUE 'CONTRATO'.    
           03  FILLER                  PIC  X(24)  VALUE SPACES.        
           03  FILLER                  PIC  X(09)  VALUE 'DOCUMENTO'.   
                                                                        
       01  CAB04-01.                                                    
           03  CAB04-01-CARRO          PIC  X(01)  VALUE ' '.           
           03  FILLER                  PIC  X(03)  VALUE SPACES.        
           03  FILLER                  PIC  X(05)  VALUE '====='.       
           03  FILLER                  PIC  X(09)  VALUE SPACES.        
           03  FILLER                  PIC  X(07)  VALUE '======='.     
           03  FILLER                  PIC  X(09)  VALUE SPACES.        
           03  FILLER                  PIC  X(14)  VALUE                
               '=============='.                                        
           03  FILLER                  PIC  X(12)  VALUE SPACES.        
           03  FILLER                  PIC  X(08)  VALUE '========'.    
           03  FILLER                  PIC  X(14)  VALUE SPACES.        
           03  FILLER                  PIC  X(17)  VALUE                
               '================='.                                     
           03  FILLER                  PIC  X(13)  VALUE SPACES.        
           03  FILLER                  PIC  X(20)  VALUE                
               '===================='.                                  
                                                                        
       01  DET01-01.                                                    
           03  DET01-01-CARRO          PIC  X(01)  VALUE ' '.           
           03  FILLER                  PIC  X(04)  VALUE SPACES.        
           03  DET01-01-BANCO          PIC  9(03).                      
           03  FILLER                  PIC  X(11)  VALUE SPACES.        
           03  DET01-01-AGENCIA        PIC  9(05).                      
           03  FILLER                  PIC  X(11)  VALUE SPACES.        
           03  DET01-01-CONTA          PIC  ZZZZZZZZZZZZ9.              
           03  FILLER                  PIC  X(13)  VALUE SPACES.        
           03  DET01-01-CARTEIRA.                                       
            05 DET01-01-CARTEIRA-N     PIC  ZZZZ9.                      
           03  FILLER                  PIC  X(16)  VALUE SPACES.        
           03  DET01-01-CONTRATO       PIC  ZZZZZZZZZZZZZZZZ9.          
           03  FILLER                  PIC  X(13)  VALUE SPACES.        
           03  DET01-01-DOCUMENTO      PIC  X(20).                      
                                                                        
      *---------------------------------------------------------------* 
      *    DEFINICAO DO RELATORIO: RELTOTAL                           * 
      *---------------------------------------------------------------* 
                                                                        
       01  CAB01-02.                                                    
           03  CAB01-02-CARRO          PIC  X(01)  VALUE '1'.           
           03  FILLER                  PIC  X(08)  VALUE 'RDAB0024'.    
           03  FILLER                  PIC  X(12)  VALUE SPACES.        
           03  FILLER                  PIC  X(33)  VALUE                
               'B A N C O  B R A D E S C O  S / A'.                     
           03  FILLER                  PIC  X(17)  VALUE SPACES.        
           03  CAB01-02-DATA.                                           
               05  CAB01-02-DIA        PIC  9(02).                      
               05  FILLER              PIC  X(01)  VALUE '/'.           
               05  CAB01-02-MES        PIC  9(02).                      
               05  FILLER              PIC  X(01)  VALUE '/'.           
               05  CAB01-02-ANO        PIC  9(04).                      
                                                                        
       01  CAB02-02.                                                    
           03  CAB02-02-CARRO          PIC  X(01)  VALUE ' '.           
           03  FILLER                  PIC  X(24)  VALUE SPACES.        
           03  FILLER                  PIC  X(25)  VALUE                
               'RELATORIO DE TOTALIZACOES'.                             
                                                                        
       01  DET01-02.                                                    
           03  DET01-02-CARRO          PIC  X(01)  VALUE '0'.           
           03  FILLER                  PIC  X(09)  VALUE SPACES.        
           03  FILLER                  PIC  X(47)  VALUE                
               'QUANTIDADE DE REGISTROS LIDOS    DO PENDENCI..:'.       
           03  FILLER                  PIC  X(01)  VALUE SPACES.        
           03  DET01-02-PENDENCI       PIC  ZZZ.ZZZ.ZZ9.                
                                                                        
       01  DET02-02.                                                    
           03  DET02-02-CARRO          PIC  X(01)  VALUE ' '.           
           03  FILLER                  PIC  X(09)  VALUE SPACES.        
           03  FILLER                  PIC  X(47)  VALUE                
               'QUANTIDADE DE REGISTROS LIDOS    DO ARQIFICA..:'.       
           03  FILLER                  PIC  X(01)  VALUE SPACES.        
           03  DET02-02-ARQIFICA       PIC  ZZZ.ZZZ.ZZ9.                
                                                                        
       01  DET03-02.                                                    
           03  DET03-02-CARRO          PIC  X(01)  VALUE ' '.           
           03  FILLER                  PIC  X(09)  VALUE SPACES.        
           03  FILLER                  PIC  X(47)  VALUE                
               'QUANTIDADE DE REGISTROS GRAVADOS DO PENDFICA..:'.       
           03  FILLER                  PIC  X(01)  VALUE SPACES.        
           03  DET03-02-PENDFICA       PIC  ZZZ.ZZZ.ZZ9.                
                                                                        
      *---------------------------------------------------------------* 
       01  FILLER                      PIC  X(22)  VALUE                
           'FIM DA WORKING-STORAGE'.                                    
      *---------------------------------------------------------------* 
       PROCEDURE                       DIVISION.                        
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       00000-INICIAR                   SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           PERFORM 10000-INICIALIZAR                                    
                                                                        
           PERFORM 20000-PROCESSAR     UNTIL WRK-FS-PENDENCI EQUAL '10' 
                                                                        
           PERFORM 30000-FINALIZAR.                                     
                                                                        
      *---------------------------------------------------------------* 
       00000-99-FIM.                   EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       10000-INICIALIZAR               SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           OPEN    INPUT  PENDENCI                                      
                          ARQIFICA                                      
                   OUTPUT PENDFICA                                      
                          RELNENCO                                      
                          RELTOTAL                                      
                                                                        
           MOVE WRK-ABERTURA           TO WRK-OPERACAO                  
           PERFORM 11000-TESTAR-FILE-STATUS                             
                                                                        
           PERFORM 12000-OBTER-DATA-HORA                                
                                                                        
           INITIALIZE WRK-CHAVE-PENDENCI                                
                      WRK-CHAVE-ARQIFICA-ANT                            
                      WRK-CHAVE-ARQIFICA-ATU                            
                                                                        
           PERFORM 13000-LER-PENDENCI                                   
                                                                        
           IF WRK-FS-PENDENCI          EQUAL '10'                       
              DISPLAY '**************** RDAB0024 ***************'       
              DISPLAY '*                                       *'       
              DISPLAY '* ARQUIVO DE ENTRADA - PENDENCI - VAZIO *'       
              DISPLAY '*        PROCESSAMENTO ENCERRADO        *'       
              DISPLAY '*                                       *'       
              DISPLAY '**************** RDAB0024 ***************'       
              PERFORM 30000-FINALIZAR.                                  
                                                                        
           PERFORM 14000-LER-ARQIFICA                                   
                                                                        
           IF WRK-FS-ARQIFICA          EQUAL '10'                       
              DISPLAY '**************** RDAB0024 ***************'       
              DISPLAY '*                                       *'       
              DISPLAY '* ARQUIVO DE ENTRADA - ARQIFICA - VAZIO *'       
              DISPLAY '*                                       *'       
              DISPLAY '**************** RDAB0024 ***************'.      
                                                                        
           MOVE WRK-CHV-CPSSOA-ARQIFICA-ATU                             
                                       TO WRK-CHV-CPSSOA-ARQIFICA-ANT.  
                                                                        
      *---------------------------------------------------------------* 
       10000-99-FIM.                   EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       11000-TESTAR-FILE-STATUS        SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           PERFORM 11100-TESTAR-FS-PENDENCI                             
           PERFORM 11200-TESTAR-FS-ARQIFICA                             
           PERFORM 11300-TESTAR-FS-PENDFICA                             
           PERFORM 11400-TESTAR-FS-RELNENCO                             
           PERFORM 11500-TESTAR-FS-RELTOTAL.                            
                                                                        
      *---------------------------------------------------------------* 
       11000-99-FIM.                   EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       11100-TESTAR-FS-PENDENCI        SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-FS-PENDENCI          NOT EQUAL   '00'                 
              DISPLAY '************** RDAB0024 *************'           
              DISPLAY '*   ERRO 'WRK-OPERACAO                           
                                            ' DO ARQUIVO   *'           
              DISPLAY '*              PENDENCI             *'           
              DISPLAY '*         FILE STATUS =  '                       
                                 WRK-FS-PENDENCI '         *'           
              DISPLAY '************** RDAB0024 *************'           
              CALL 'ILBOABN0'          USING WRK-ABEND.                 
                                                                        
      *---------------------------------------------------------------* 
       11100-99-FIM.                   EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       11200-TESTAR-FS-ARQIFICA        SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-FS-ARQIFICA          NOT EQUAL   '00'                 
              DISPLAY '************** RDAB0024 *************'           
              DISPLAY '*   ERRO 'WRK-OPERACAO                           
                                            ' DO ARQUIVO   *'           
              DISPLAY '*              ARQIFICA             *'           
              DISPLAY '*         FILE STATUS =  '                       
                                 WRK-FS-ARQIFICA '         *'           
              DISPLAY '************** RDAB0024 *************'           
              CALL 'ILBOABN0'          USING WRK-ABEND.                 
                                                                        
      *---------------------------------------------------------------* 
       11200-99-FIM.                   EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       11300-TESTAR-FS-PENDFICA        SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-FS-PENDFICA          NOT EQUAL   '00'                 
              DISPLAY '************** RDAB0024 *************'           
              DISPLAY '*   ERRO 'WRK-OPERACAO                           
                                            ' DO ARQUIVO   *'           
              DISPLAY '*              PENDFICA             *'           
              DISPLAY '*         FILE STATUS =  '                       
                                 WRK-FS-PENDFICA '         *'           
              DISPLAY '************** RDAB0024 *************'           
              CALL 'ILBOABN0'          USING WRK-ABEND.                 
                                                                        
      *---------------------------------------------------------------* 
       11300-99-FIM.                   EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       11400-TESTAR-FS-RELNENCO        SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-FS-RELNENCO          NOT EQUAL   '00'                 
              DISPLAY '************** RDAB0024 *************'           
              DISPLAY '*   ERRO 'WRK-OPERACAO                           
                                            ' DO ARQUIVO   *'           
              DISPLAY '*              RELNENCO             *'           
              DISPLAY '*         FILE STATUS =  '                       
                                 WRK-FS-RELNENCO '         *'           
              DISPLAY '************** RDAB0024 *************'           
              CALL 'ILBOABN0'          USING WRK-ABEND.                 
                                                                        
      *---------------------------------------------------------------* 
       11400-99-FIM.                   EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       11500-TESTAR-FS-RELTOTAL        SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-FS-RELTOTAL          NOT EQUAL   '00'                 
              DISPLAY '************** RDAB0024 *************'           
              DISPLAY '*   ERRO 'WRK-OPERACAO                           
                                            ' DO ARQUIVO   *'           
              DISPLAY '*              RELTOTAL             *'           
              DISPLAY '*         FILE STATUS =  '                       
                                 WRK-FS-RELTOTAL '         *'           
              DISPLAY '************** RDAB0024 *************'           
              CALL 'ILBOABN0'          USING WRK-ABEND.                 
                                                                        
      *---------------------------------------------------------------* 
       11500-99-FIM.                   EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       12000-OBTER-DATA-HORA             SECTION.                       
      *---------------------------------------------------------------* 
                                                                        
           CALL 'POOL7600'             USING WRK-7600                   
                                                                        
           MOVE WRK-7600-DIA           TO CAB01-02-DIA                  
                                          CAB02-01-DIA                  
                                                                        
           MOVE WRK-7600-MES           TO CAB01-02-MES                  
                                          CAB02-01-MES                  
                                                                        
           MOVE WRK-7600-ANO           TO CAB01-02-ANO                  
                                          CAB02-01-ANO                  
                                                                        
           MOVE WRK-7600-HOR           TO CAB02-01-HOR                  
           MOVE WRK-7600-MIN           TO CAB02-01-MIN.                 
                                                                        
      *---------------------------------------------------------------* 
       12000-99-FIM.                   EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       13000-LER-PENDENCI              SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           READ PENDENCI                                                
                                                                        
           IF WRK-FS-PENDENCI          EQUAL  '10'                      
              MOVE HIGH-VALUES         TO WRK-CHAVE-PENDENCI            
              GO                       TO 13000-99-FIM.                 
                                                                        
           MOVE WRK-LEITURA            TO WRK-OPERACAO                  
           PERFORM 11100-TESTAR-FS-PENDENCI                             
                                                                        
           MOVE PVV-CPSSOA-CADTR       OF PENDENCI                      
                                       TO WRK-CHV-CPSSOA-PENDENCI       
                                                                        
           ADD     1                   TO ACU-LDS-PENDENCI.             
                                                                        
      *---------------------------------------------------------------* 
       13000-99-FIM.                   EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       14000-LER-ARQIFICA              SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           READ ARQIFICA                                                
                                                                        
           IF WRK-FS-ARQIFICA          EQUAL  '10'                      
              MOVE HIGH-VALUES         TO WRK-CHAVE-ARQIFICA-ATU        
              GO                       TO 14000-99-FIM.                 
                                                                        
           MOVE WRK-LEITURA            TO WRK-OPERACAO                  
           PERFORM 11200-TESTAR-FS-ARQIFICA                             
                                                                        
           MOVE V004-S-CPSSOA-CADTR    OF ARQIFICA                      
                                       TO WRK-CHV-CPSSOA-ARQIFICA-ATU   
                                                                        
           ADD     1                   TO ACU-LDS-ARQIFICA.             
                                                                        
      *---------------------------------------------------------------* 
       14000-99-FIM.                   EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       20000-PROCESSAR                 SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-CHAVE-PENDENCI       GREATER WRK-CHAVE-ARQIFICA-ATU   
              PERFORM 21000-CHAVE-MAIOR                                 
           ELSE                                                         
           IF WRK-CHAVE-PENDENCI       LESS WRK-CHAVE-ARQIFICA-ATU      
              PERFORM 22000-CHAVE-MENOR                                 
           ELSE                                                         
              PERFORM 23000-CHAVES-IGUAIS.                              
                                                                        
      *---------------------------------------------------------------* 
       20000-99-FIM.                   EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       21000-CHAVE-MAIOR               SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
                                                                        
           PERFORM 14000-LER-ARQIFICA  UNTIL WRK-CHV-CPSSOA-ARQIFICA-ATU
                                   NOT EQUAL WRK-CHV-CPSSOA-ARQIFICA-ANT
                                                                        
           MOVE WRK-CHV-CPSSOA-ARQIFICA-ATU                             
                                       TO WRK-CHV-CPSSOA-ARQIFICA-ANT.  
                                                                        
      *---------------------------------------------------------------* 
       21000-99-FIM.                   EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       22000-CHAVE-MENOR               SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
      *    IF WRK-ENCONTRADO           EQUAL 'S'                        
      *       MOVE 'N'                 TO WRK-ENCONTRADO                
      *    ELSE                                                         
              PERFORM 22100-IMPRIMIR-NAO-ENCONTRADOS.                   
                                                                        
           PERFORM 13000-LER-PENDENCI.                                  
                                                                        
      *---------------------------------------------------------------* 
       22000-99-FIM.                   EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       22100-IMPRIMIR-NAO-ENCONTRADOS  SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           IF ACU-LINHAS               GREATER 59                       
              PERFORM 22110-IMPRIMIR-CAB-NENCO.                         
                                                                        
           MOVE PVV-BANCO              OF PENDENCI                      
                                       TO DET01-01-BANCO                
           MOVE PVV-AGENCIA            OF PENDENCI                      
                                       TO DET01-01-AGENCIA              
           MOVE PVV-CONTA              OF PENDENCI                      
                                       TO DET01-01-CONTA                
           IF   PVV-CARTEIRA OF PENDENCI  NOT  NUMERIC                  
                MOVE  PVV-CARTEIRA-R   OF PENDENCI                      
                                       TO DET01-01-CARTEIRA             
           ELSE                                                         
                MOVE  PVV-CARTEIRA     OF PENDENCI                      
                                       TO DET01-01-CARTEIRA-N.          
           MOVE PVV-CONTRATO           OF PENDENCI                      
                                       TO DET01-01-CONTRATO             
                                                                        
           IF PVV-CGCFIL OF PENDENCI   EQUAL  ZEROS                     
              MOVE PVV-CGCNUM          OF PENDENCI                      
                                       TO WRK-CPF-NUM                   
              MOVE PVV-CGCCTR          OF PENDENCI                      
                                       TO WRK-CPF-CTR                   
              MOVE WRK-CPF             TO DET01-01-DOCUMENTO            
           ELSE                                                         
              MOVE PVV-CGCNUM          OF PENDENCI                      
                                       TO WRK-CGC-NUM                   
              MOVE PVV-CGCFIL          OF PENDENCI                      
                                       TO WRK-CGC-FIL                   
              MOVE PVV-CGCCTR          OF PENDENCI                      
                                       TO WRK-CGC-CTR                   
              MOVE WRK-CGC             TO DET01-01-DOCUMENTO.           
                                                                        
           WRITE   REG-RELNENCO        FROM   DET01-01                  
                                                                        
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO                  
           PERFORM 11400-TESTAR-FS-RELNENCO                             
                                                                        
           ADD     01                  TO ACU-LINHAS.                   
                                                                        
      *---------------------------------------------------------------* 
       22100-99-FIM.                   EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       22110-IMPRIMIR-CAB-NENCO        SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           ADD     1                   TO ACU-PAGINAS                   
           MOVE ACU-PAGINAS            TO CAB01-01-PAGINA               
                                                                        
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO                  
                                                                        
           WRITE   REG-RELNENCO        FROM  CAB01-01                   
           PERFORM 11400-TESTAR-FS-RELNENCO                             
                                                                        
           WRITE   REG-RELNENCO        FROM  CAB02-01                   
           PERFORM 11400-TESTAR-FS-RELNENCO                             
                                                                        
           WRITE   REG-RELNENCO        FROM  CAB03-01                   
           PERFORM 11400-TESTAR-FS-RELNENCO                             
                                                                        
           WRITE   REG-RELNENCO        FROM  CAB04-01                   
           PERFORM 11400-TESTAR-FS-RELNENCO                             
                                                                        
           MOVE 06                     TO ACU-LINHAS.                   
                                                                        
      *---------------------------------------------------------------* 
       22110-99-FIM.                   EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       23000-CHAVES-IGUAIS             SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           IF V004-S-CGRAU-LIG         OF ARQIFICA EQUAL 1              
              PERFORM 23100-GRAVAR-PENDFICA.                            
                                                                        
           PERFORM 13000-LER-PENDENCI.                                  
           PERFORM 14000-LER-ARQIFICA.                                  
                                                                        
      *---------------------------------------------------------------* 
       23000-99-FIM.                   EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       23100-GRAVAR-PENDFICA           SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           MOVE PVV-REGISTRO           OF PENDENCI                      
                                       TO PVV-REGISTRO      OF PENDFICA 
           MOVE V004-S-CPSSOA          TO PVV-CPSSOA-LIGADA OF PENDFICA 
                                                                        
           WRITE PVV-REGISTRO          OF PENDFICA                      
                                                                        
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO                  
           PERFORM 11300-TESTAR-FS-PENDFICA                             
                                                                        
           MOVE 'S'                    TO WRK-ENCONTRADO                
                                                                        
           ADD      1                  TO ACU-GRV-PENDFICA.             
                                                                        
      *---------------------------------------------------------------* 
       23100-99-FIM.                   EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       30000-FINALIZAR                 SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           PERFORM 31000-EMITIR-TOTAIS                                  
                                                                        
           CLOSE PENDENCI                                               
                 ARQIFICA                                               
                 PENDFICA                                               
                 RELNENCO                                               
                 RELTOTAL                                               
                                                                        
           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO                  
           PERFORM 11000-TESTAR-FILE-STATUS                             
                                                                        
           STOP RUN.                                                    
                                                                        
      *---------------------------------------------------------------* 
       30000-99-FIM.                   EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       31000-EMITIR-TOTAIS             SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           MOVE ACU-LDS-PENDENCI       TO DET01-02-PENDENCI             
                                          WRK-MASC-LDS-PENDENCI         
           MOVE ACU-LDS-ARQIFICA       TO DET02-02-ARQIFICA             
                                          WRK-MASC-LDS-ARQIFICA         
           MOVE ACU-GRV-PENDFICA       TO DET03-02-PENDFICA             
                                          WRK-MASC-GRV-PENDFICA         
                                                                        
           PERFORM 31100-IMPRIMIR-TOTAIS                                
                                                                        
           DISPLAY '******************** RDAB0024 ********************' 
           DISPLAY '*                                                *' 
           DISPLAY '*  TOTAL REG. LIDOS    - PENDENCI : '               
                                                 WRK-MASC-LDS-PENDENCI  
           '  *'                                                        
           DISPLAY '*  TOTAL REG. LIDOS    - ARQIFICA : '               
                                                 WRK-MASC-LDS-ARQIFICA  
           '  *'                                                        
           DISPLAY '*  TOTAL REG. GRAVADOS - PENDFICA : '               
                                                 WRK-MASC-GRV-PENDFICA  
           '  *'                                                        
           DISPLAY '*                                                *' 
           DISPLAY '******************** RDAB0024 ********************'.
                                                                        
      *---------------------------------------------------------------* 
       31000-99-FIM.                   EXIT.                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       31100-IMPRIMIR-TOTAIS           SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO                  
                                                                        
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
                                                                        
      *---------------------------------------------------------------* 
       31100-99-FIM.                   EXIT.                            
      *---------------------------------------------------------------* 
                                                                        

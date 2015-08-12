      *===============================================================* 
       IDENTIFICATION DIVISION.                                         
      *===============================================================* 
                                                                        
       PROGRAM-ID. RDAB0007.                                            
       AUTHOR. MAGALI LIMA.                                             
      *----------------------------------------------------------------*
      *                        S T E F A N I N I                       *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMADOR...: MAGALI DE FARIA LIMA    CPM S/A             *
      *    SUPERVISOR....: MAGALI DE FARIA LIMA  - CPM S/A             *
      *    ANALISTA......: MAGALI DE FARIA LIMA  - CPM S/A             *
      *    DATA..........: 21/05/2003                                  *
      *                                                                *
      *----------------------------------------------------------------*
      *    OBJETIVO......: ELIMINAR REGISTROS DUPLICADOS PARA UM MESMO *
      *                    DOCUMENTO. PREVALECERA, O REGISTRO CUJA FI- *
      *                    TIVER STATUS "EF", SENDO QUE EXISTINDO DUAS *
      *                    SELECIONAR A MAIS ATUAL E NA FALTA DESTA A  *
      *                    FICHA COM STATUS "HS".                      *
      *----------------------------------------------------------------*
      *                    I N C ' S   /   A R Q U I V O S             *
      *    NOME     DESCRICAO                                          *
      *    I#RDAB01 ARQPARVV - ARQUIVO DE PARCELAS VENCIDAS/VINCENDAS  *
      *    I#RDAB01 ARQPEFIS - ARQUIVO DE PARCELAS VENCIDAS/VINCENDAS  *
      *                        DE PESSOA FISICA.                       *
      *    I#RDAB01 ARQPEJUR - ARQUIVO DE PARCELAS VENCIDAS/VINCENDAS  *
      *                        DE PESSOA FISICA.                       *
      *    I#RDAB05 ARQDAPES - ARQUIVO DE DADOS DA PESSOA              *
      *----------------------------------------------------------------*
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
                                                                        
           SELECT  ARQPEFIS  ASSIGN  TO  UT-S-ARQPEFIS                  
                   FILE      STATUS  IS  WRK-FS-ARQPEFIS.               
                                                                        
           SELECT  ARQPEJUR  ASSIGN  TO  UT-S-ARQPEJUR                  
                   FILE      STATUS  IS  WRK-FS-ARQPEJUR.               
                                                                        
           SELECT  ARQDAPES  ASSIGN  TO  UT-S-ARQDAPES                  
                   FILE      STATUS  IS  WRK-FS-ARQDAPES.               
                                                                        
           SELECT  RELVAZIO  ASSIGN  TO  UT-S-RELVAZIO                  
                   FILE      STATUS  IS  WRK-FS-RELVAZIO.               
                                                                        
           SELECT  RELTOTAL  ASSIGN  TO  UT-S-RELTOTAL                  
                   FILE      STATUS  IS  WRK-FS-RELTOTAL.               
                                                                        
                                                                        
      *===============================================================* 
       DATA DIVISION.                                                   
      *===============================================================* 
                                                                        
      *---------------------------------------------------------------* 
       FILE SECTION.                                                    
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
      *  INPUT... : ARQ. PARCELAS VENCIDAS/VINCENDAS                  * 
      *             ORG. SEQUENCIAL    -  LRECL = 500 BYTES           * 
      *---------------------------------------------------------------* 
                                                                        
       FD  ARQPARVV                                                     
           LABEL       RECORD    STANDARD                               
           RECORDING   MODE      F                                      
           BLOCK       CONTAINS  0.                                     
                                                                        
-INC I#RDAB05                                                           
                                                                        
      *---------------------------------------------------------------* 
      *  OUTPUT...: ARQ. DE PARCELAS VENCS/VINCENDAS DE PESSOA FISICA * 
      *             ORG. SEQUENCIAL   -   LRECL = 150 BYTES           * 
      *---------------------------------------------------------------* 
                                                                        
       FD  ARQPEFIS                                                     
           LABEL       RECORD    STANDARD                               
           RECORDING   MODE      F                                      
           BLOCK       CONTAINS  0.                                     
                                                                        
                                                                        
-INC I#RDAB01                                                           
                                                                        
      *---------------------------------------------------------------* 
      *  OUTPUT...: ARQ. DE PARCELAS VENC/VINCENDAS DE PESSOA JURIDICA* 
      *             ORG. SEQUENCIAL   -   LRECL = 150 BYTES           * 
      *---------------------------------------------------------------* 
                                                                        
       FD  ARQPEJUR                                                     
           LABEL       RECORD    STANDARD                               
           RECORDING   MODE      F                                      
           BLOCK       CONTAINS  0.                                     
                                                                        
-INC I#RDAB01                                                           
                                                                        
      *---------------------------------------------------------------* 
      *  OUTPUT...: ARQ. DE DADOS DA PESSOA                           * 
      *             ORG. SEQUENCIAL   -   LRECL = 500 BYTES           * 
      *---------------------------------------------------------------* 
                                                                        
       FD  ARQDAPES                                                     
           LABEL       RECORD    STANDARD                               
           RECORDING   MODE      F                                      
           BLOCK       CONTAINS  0.                                     
                                                                        
       01  REG-ARQDAPES                   PIC  X(500).                  
                                                                        
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
       01   FILLER                     PIC  X(32) VALUE                 
            'INICIO DA WORKING RDAB0007'.                               
                                                                        
      *---------------------------------------------------------------* 
      *          CAMPOS UTILIZADOS PARA CANCELAR PROCESSAMENTO        * 
      *---------------------------------------------------------------* 
                                                                        
       01  FILLER.                                                      
           03  WRK-FS-ARQPARVV         PIC  X(02) VALUE SPACES.         
           03  WRK-FS-ARQPEFIS         PIC  X(02) VALUE SPACES.         
           03  WRK-FS-ARQPEJUR         PIC  X(02) VALUE SPACES.         
           03  WRK-FS-ARQDAPES         PIC  X(02) VALUE SPACES.         
           03  WRK-FS-RELVAZIO         PIC  X(02) VALUE SPACES.         
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
           03  ACU-GRV-ARQPEFIS        PIC 9(09)  VALUE  ZEROS.         
           03  ACU-GRV-ARQPEJUR        PIC 9(09)  VALUE  ZEROS.         
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
      *--  Area auxiliar.                                               
                                                                        
       01  WRK-CPF.                                                     
           03  FILLER                  PIC X(03) VALUE  SPACES.         
           03  WRK-CPF-NUM             PIC 999.999.999.                 
           03  FILLER                  PIC X(01) VALUE '-'.             
           03  WRK-CPF-CTR             PIC 99.                          
           03  FILLER                  PIC X(03) VALUE  SPACES.         
                                                                        
      *--  CHAVE ARQPARVV.                                              
       01  WRK-CHV-ATUAL.                                               
           03  WRK-CHV-CPF-ATU         PIC 9(09) VALUE  ZEROS.          
           03  WRK-CHV-CFILIAL-ATU     PIC 9(05) VALUE  ZEROS.          
           03  WRK-CHV-CCTRL-ATU       PIC 9(03) VALUE  ZEROS.          
                                                                        
       01  WRK-CHV-ANT.                                                 
           03  WRK-CHV-CPF-ANT         PIC 9(09) VALUE  ZEROS.          
           03  WRK-CHV-CFILIAL-ANT     PIC 9(05) VALUE  ZEROS.          
           03  WRK-CHV-CCTRL-ANT       PIC 9(03) VALUE  ZEROS.          
                                                                        
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
           03  FILLER                  PIC X(08)  VALUE 'RDAB0007'.     
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
               'NOME DO PROGRAMA.......: RDAB0007'.                     
                                                                        
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
           03  FILLER                  PIC X(08)  VALUE 'RDAB0007'.     
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
                                                                        
       01  LINDET3-2.                                                   
           03  FILLER                  PIC X(01)  VALUE ' '.            
           03  FILLER                  PIC X(06)  VALUE SPACES.         
           03  FILLER                  PIC X(47)  VALUE                 
               'QUANTIDADE DE REGISTROS GRAVADOS DO ARQPEFIS..:'.       
           03  FILLER                  PIC X(01)  VALUE SPACES.         
           03  LD3-2-ARQPEFIS          PIC ZZZ.ZZZ.ZZ9.                 
                                                                        
       01  LINDET4-2.                                                   
           03  FILLER                  PIC X(01)  VALUE ' '.            
           03  FILLER                  PIC X(06)  VALUE SPACES.         
           03  FILLER                  PIC X(47)  VALUE                 
               'QUANTIDADE DE REGISTROS GRAVADOS DO ARQPEJUR..:'.       
           03  FILLER                  PIC X(01)  VALUE SPACES.         
           03  LD4-2-ARQPEJUR          PIC ZZZ.ZZZ.ZZ9.                 
                                                                        
       01  LINDET5-2.                                                   
           03  FILLER                  PIC X(01)  VALUE ' '.            
           03  FILLER                  PIC X(06)  VALUE SPACES.         
           03  FILLER                  PIC X(47)  VALUE                 
               'QUANTIDADE DE REGISTROS GRAVADOS DO ARQDAPES..:'.       
           03  FILLER                  PIC X(01)  VALUE SPACES.         
           03  LD5-2-ARQDAPES          PIC ZZZ.ZZZ.ZZ9.                 
                                                                        
      *---------------------------------------------------------------* 
       01   FILLER                     PIC  X(32)    VALUE              
            'FIM DA WORKING RDAB0007'.                                  
                                                                        
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
                   DISPLAY '**************** RDAB0007 ***************'  
                   DISPLAY '*                                       *'  
                   DISPLAY '* ARQUIVO DE ENTRADA - ARQPARVV - VAZIO *'  
                   DISPLAY '*        PROCESSAMENTO ENCERRADO        *'  
                   DISPLAY '*                                       *'  
                   DISPLAY '**************** RDAB0007 ***************'  
                   MOVE    'ARQPARVV'  TO     LD1-1-ARQUIVO             
                   PERFORM  60000-IMPRIMIR-RELVAZIO                     
                   PERFORM  90000-FINALIZAR.                            
                                                                        
           PERFORM 40000-PROCESSAR     UNTIL                            
                  (WRK-FS-ARQPARVV     EQUAL '10').                     
                                                                        
           PERFORM 62000-IMPRIMIR-RELTOTAL.                             
                                                                        
           PERFORM 70000-DISPLAY-TOTAIS.                                
                                                                        
           PERFORM 90000-FINALIZAR.                                     
                                                                        
      *---------------------------------------------------------------* 
       00000-99-FIM.   EXIT.                                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       10000-INICIALIZAR              SECTION.                          
      *---------------------------------------------------------------* 
                                                                        
           OPEN  INPUT  ARQPARVV                                        
                 OUTPUT ARQPEFIS                                        
                        ARQPEJUR                                        
                        ARQDAPES                                        
                        RELVAZIO                                        
                        RELTOTAL.                                       
                                                                        
           MOVE  WRK-ABERTURA          TO    WRK-OPERACAO               
           PERFORM  11000-TESTAR-FILE-STATUS.                           
                                                                        
      *---------------------------------------------------------------* 
       10000-99-FIM.   EXIT.                                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       11000-TESTAR-FILE-STATUS       SECTION.                          
      *---------------------------------------------------------------* 
                                                                        
           PERFORM  11100-TESTAR-FS-ARQPARVV                            
           PERFORM  11300-TESTAR-FS-ARQPEFIS.                           
           PERFORM  11400-TESTAR-FS-ARQPEJUR.                           
           PERFORM  11500-TESTAR-FS-ARQDAPES.                           
           PERFORM  11600-TESTAR-FS-RELVAZIO.                           
           PERFORM  11800-TESTAR-FS-RELTOTAL.                           
                                                                        
      *---------------------------------------------------------------* 
       11000-99-FIM.   EXIT.                                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       11100-TESTAR-FS-ARQPARVV       SECTION.                          
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-FS-ARQPARVV         NOT EQUAL  '00'                   
              DISPLAY '************** RDAB0007 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              ARQPARVV             *'           
              DISPLAY '*         FILE STATUS =  ' WRK-FS-ARQPARVV       
                                                 '         *'           
              DISPLAY '************** RDAB0007 *************'           
              CALL    'ILBOABN0'      USING    WRK-ABEND.               
                                                                        
      *---------------------------------------------------------------* 
       11100-99-FIM.   EXIT.                                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       11300-TESTAR-FS-ARQPEFIS        SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-FS-ARQPEFIS          NOT EQUAL  '00'                  
              DISPLAY '************** RDAB0007 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              ARQPEFIS              *'          
              DISPLAY '*         FILE STATUS =  ' WRK-FS-ARQPEFIS       
                                                 '         *'           
              DISPLAY '************** RDAB0007 *************'           
              CALL    'ILBOABN0'      USING    WRK-ABEND.               
                                                                        
      *---------------------------------------------------------------* 
       11300-99-FIM.   EXIT.                                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       11400-TESTAR-FS-ARQPEJUR        SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-FS-ARQPEJUR           NOT EQUAL  '00'                 
              DISPLAY '************** RDAB0007 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              ARQPEJUR              *'          
              DISPLAY '*         FILE STATUS =  ' WRK-FS-ARQPEJUR       
                                                 '         *'           
              DISPLAY '************** RDAB0007 *************'           
              CALL    'ILBOABN0'      USING    WRK-ABEND.               
                                                                        
      *---------------------------------------------------------------* 
       11400-99-FIM.   EXIT.                                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       11500-TESTAR-FS-ARQDAPES        SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-FS-ARQDAPES           NOT EQUAL  '00'                 
              DISPLAY '************** RDAB0007 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              ARQDAPES              *'          
              DISPLAY '*         FILE STATUS =  ' WRK-FS-ARQDAPES       
                                                 '         *'           
              DISPLAY '************** RDAB0007 *************'           
              CALL    'ILBOABN0'      USING    WRK-ABEND.               
                                                                        
      *---------------------------------------------------------------* 
       11500-99-FIM.   EXIT.                                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       11600-TESTAR-FS-RELVAZIO        SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-FS-RELVAZIO           NOT EQUAL  '00'                 
              DISPLAY '************** RDAB0007 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              RELVAZIO              *'          
              DISPLAY '*         FILE STATUS =  ' WRK-FS-RELVAZIO       
                                                 '         *'           
              DISPLAY '************** RDAB0007 *************'           
              CALL    'ILBOABN0'      USING    WRK-ABEND.               
                                                                        
      *---------------------------------------------------------------* 
       11600-99-FIM.   EXIT.                                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       11800-TESTAR-FS-RELTOTAL        SECTION.                         
      *---------------------------------------------------------------* 
                                                                        
           IF WRK-FS-RELTOTAL           NOT EQUAL  '00'                 
              DISPLAY '************** RDAB0007 *************'           
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'       
              DISPLAY '*              RELTOTAL              *'          
              DISPLAY '*         FILE STATUS =  ' WRK-FS-RELTOTAL       
                                                 '         *'           
              DISPLAY '************** RDAB0007 *************'           
              CALL    'ILBOABN0'      USING    WRK-ABEND.               
                                                                        
      *---------------------------------------------------------------* 
       11800-99-FIM.   EXIT.                                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       12000-FORMATAR-DATA-HORA SECTION.                                
      *---------------------------------------------------------------* 
                                                                        
           CALL  'POOL7600'            USING WRK-7600-DATA-HORA         
                                                                        
           MOVE   DT-AAAAMMDD-7600     TO    WRK-AAAAMMDD.              
           MOVE CORR WRK-AAAAMMDD-R    TO    WRK-DDMMAAAA.              
           MOVE   WRK-DDMMAAAA-R       TO    CB1-01-DATA.               
                                                                        
      *---------------------------------------------------------------* 
       12000-99-FIM. EXIT.                                              
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       20000-LER-ARQPARVV            SECTION.                           
      *---------------------------------------------------------------* 
                                                                        
           READ    ARQPARVV                                             
                                                                        
           IF      WRK-FS-ARQPARVV     EQUAL  '10'                      
                   MOVE HIGH-VALUES    TO      WRK-CHV-ATUAL            
                   GO                  TO      20000-99-FIM.            
                                                                        
           MOVE    WRK-LEITURA         TO      WRK-OPERACAO.            
           PERFORM 11100-TESTAR-FS-ARQPARVV.                            
                                                                        
           MOVE    CBASE-CPF           TO      WRK-CHV-CPF-ATU.         
           MOVE    CFILIAL-CNPJ        TO      WRK-CHV-CFILIAL-ATU.     
           MOVE    CCTRL-CNPJ-CPF      TO      WRK-CHV-CCTRL-ATU.       
                                                                        
           ADD     1                   TO      ACU-LDS-ARQPARVV.        
                                                                        
      *---------------------------------------------------------------* 
       20000-99-FIM.   EXIT.                                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       40000-PROCESSAR               SECTION.                           
      *---------------------------------------------------------------* 
                                                                        
           IF  WRK-CHV-ATUAL  NOT EQUAL    WRK-CHV-ANT                  
               PERFORM 42000-CHAVES-IGUAIS                              
               MOVE WRK-CHV-ATUAL   TO  WRK-CHV-ANT.                    
           PERFORM  20000-LER-ARQPARVV.                                 
                                                                        
      *---------------------------------------------------------------* 
       40000-99-FIM.   EXIT.                                            
      *---------------------------------------------------------------* 
                                                                        
                                                                        
      *---------------------------------------------------------------* 
       42000-CHAVES-IGUAIS           SECTION.                           
      *---------------------------------------------------------------* 
                                                                        
           INITIALIZE PVV-REGISTRO OF ARQPEFIS                          
                      PVV-REGISTRO OF ARQPEJUR.                         
                                                                        
           PERFORM  50000-GRAVAR-ARQDAPES.                              
                                                                        
           IF STATUS-FICHA          EQUAL  'ZZ'                         
              NEXT SENTENCE                                             
           ELSE                                                         
              IF CIDTFD-TPO-PSSOA   EQUAL  '2'                          
                 MOVE CBCO             TO PVV-BANCO        OF ARQPEJUR  
                 MOVE CAG-BCRIA        TO PVV-AGENCIA      OF ARQPEJUR  
                 MOVE CCTA-CORR        TO PVV-CONTA        OF ARQPEJUR

                 IF CARTEIRA IS NUMERIC
                    MOVE CARTEIRA      TO PVV-CARTEIRA     OF ARQPEJUR
                 ELSE
                    MOVE CARTEIRA-R    TO PVV-CARTEIRA-R   OF ARQPEJUR
                 END-IF

                 MOVE CONTRATO         TO PVV-CONTRATO     OF ARQPEJUR  
                 MOVE CBASE-CPF        TO PVV-CGCNUM       OF ARQPEJUR  
                 MOVE CFILIAL-CNPJ     TO PVV-CGCFIL       OF ARQPEJUR  
                 MOVE CCTRL-CNPJ-CPF   TO PVV-CGCCTR       OF ARQPEJUR  
                 MOVE CIDTFD-TPO-PSSOA TO PVV-TPPESSOA     OF ARQPEJUR  
                 MOVE CPSSOA-CADTR     TO PVV-CPSSOA-CADTR OF ARQPEJUR  
                 MOVE CD-CLI           TO PVV-CPSSOA-CDCLI OF ARQPEJUR  
                 MOVE CPOSTO-SERVC     TO PVV-CPOSTO-SERVC OF ARQPEJUR  
BRQ=I            MOVE CCLUB            TO PVV-CCLUB        OF ARQPEJUR  
                 PERFORM 51000-GRAVAR-ARQPEJUR                          
              ELSE                                                      
                 MOVE CBCO             TO PVV-BANCO        OF ARQPEFIS  
                 MOVE CAG-BCRIA        TO PVV-AGENCIA      OF ARQPEFIS  
                 MOVE CCTA-CORR        TO PVV-CONTA        OF ARQPEFIS  
                 IF CARTEIRA IS NUMERIC
                    MOVE CARTEIRA      TO PVV-CARTEIRA     OF ARQPEFIS
                 ELSE
                    MOVE CARTEIRA-R    TO PVV-CARTEIRA-R   OF ARQPEFIS
                 END-IF
                 MOVE CONTRATO         TO PVV-CONTRATO     OF ARQPEFIS  
                 MOVE CBASE-CPF        TO PVV-CGCNUM       OF ARQPEFIS  
                 MOVE CFILIAL-CNPJ     TO PVV-CGCFIL       OF ARQPEFIS  
                 MOVE CCTRL-CNPJ-CPF   TO PVV-CGCCTR       OF ARQPEFIS  
                 MOVE CIDTFD-TPO-PSSOA TO PVV-TPPESSOA     OF ARQPEFIS  
                 MOVE CPSSOA-CADTR     TO PVV-CPSSOA-CADTR OF ARQPEFIS  
                 MOVE CD-CLI           TO PVV-CPSSOA-CDCLI OF ARQPEFIS  
                 MOVE CPOSTO-SERVC     TO PVV-CPOSTO-SERVC OF ARQPEFIS  
BRQ=I            MOVE CCLUB            TO PVV-CCLUB        OF ARQPEFIS  
                 PERFORM 52000-GRAVAR-ARQPEFIS.                         
                                                                        
      *---------------------------------------------------------------* 
       42000-99-FIM.   EXIT.                                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       50000-GRAVAR-ARQDAPES          SECTION.                          
      *---------------------------------------------------------------* 
                                                                        
           INITIALIZE REG-ARQDAPES.                                     
           MOVE   ADP-REGISTRO         TO     REG-ARQDAPES.             
           WRITE  REG-ARQDAPES.                                         
           MOVE   WRK-GRAVACAO         TO      WRK-OPERACAO             
           PERFORM  11500-TESTAR-FS-ARQDAPES                            
                                                                        
           ADD 1                      TO   ACU-GRV-ARQDAPES.            
                                                                        
      *---------------------------------------------------------------* 
       50000-99-FIM.   EXIT.                                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       51000-GRAVAR-ARQPEJUR          SECTION.                          
      *---------------------------------------------------------------* 
                                                                        
           WRITE  PVV-REGISTRO  OF ARQPEJUR                             
           MOVE   WRK-GRAVACAO         TO      WRK-OPERACAO             
           PERFORM  11400-TESTAR-FS-ARQPEJUR                            
                                                                        
           ADD 1                      TO   ACU-GRV-ARQPEJUR.            
                                                                        
      *---------------------------------------------------------------* 
       51000-99-FIM.   EXIT.                                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       52000-GRAVAR-ARQPEFIS          SECTION.                          
      *---------------------------------------------------------------* 
                                                                        
           WRITE  PVV-REGISTRO  OF ARQPEFIS                             
           MOVE   WRK-GRAVACAO         TO      WRK-OPERACAO             
           PERFORM  11300-TESTAR-FS-ARQPEFIS                            
                                                                        
           ADD 1                      TO   ACU-GRV-ARQPEFIS.            
                                                                        
      *---------------------------------------------------------------* 
       52000-99-FIM.   EXIT.                                            
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
       62000-IMPRIMIR-RELTOTAL          SECTION.                        
      *----------------------------------------------------------------*
                                                                        
           MOVE   WRK-GRAVACAO         TO      WRK-OPERACAO             
                                                                        
           PERFORM  62100-IMPRIMIR-CABECS.                              
                                                                        
                                                                        
           MOVE  ACU-LDS-ARQPARVV      TO   LD1-2-ARQPARVV              
           MOVE  ACU-GRV-ARQPEFIS      TO   LD3-2-ARQPEFIS              
           MOVE  ACU-GRV-ARQPEJUR      TO   LD4-2-ARQPEJUR              
           MOVE  ACU-GRV-ARQDAPES      TO   LD5-2-ARQDAPES              
                                                                        
           WRITE   REG-RELTOTAL       FROM  LINDET1-2                   
           PERFORM 11800-TESTAR-FS-RELTOTAL.                            
                                                                        
           WRITE   REG-RELTOTAL       FROM  LINDET3-2                   
           PERFORM 11800-TESTAR-FS-RELTOTAL.                            
                                                                        
           WRITE   REG-RELTOTAL       FROM  LINDET4-2                   
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
           MOVE     ACU-GRV-ARQPEFIS   TO    WRK-EDIT03                 
           MOVE     ACU-GRV-ARQPEJUR   TO    WRK-EDIT04                 
           MOVE     ACU-GRV-ARQDAPES   TO    WRK-EDIT05                 
                                                                        
           DISPLAY '******************** RDAB0007 ********************' 
           DISPLAY '*                                                *' 
           DISPLAY '*  TOTAL REG. LIDOS    - ARQPARVV : 'WRK-EDIT01'  *'
           DISPLAY '*  TOTAL REG. GRAVADOS - ARQPEFIS : 'WRK-EDIT03'  *'
           DISPLAY '*  TOTAL REG. GRAVADOS - ARQPEJUR : 'WRK-EDIT04'  *'
           DISPLAY '*  TOTAL REG. GRAVADOS - ARQDAPES : 'WRK-EDIT05'  *'
           DISPLAY '*                                                *' 
           DISPLAY '******************** RDAB0007 ********************'.
                                                                        
      *---------------------------------------------------------------* 
       70000-99-FIM.   EXIT.                                            
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
       90000-FINALIZAR             SECTION.                             
      *---------------------------------------------------------------* 
                                                                        
           CLOSE  ARQPARVV                                              
                  ARQPEFIS                                              
                  ARQPEJUR                                              
                  ARQDAPES                                              
                  RELVAZIO                                              
                  RELTOTAL.                                             
                                                                        
           MOVE   WRK-FECHAMENTO       TO      WRK-OPERACAO             
           PERFORM  11000-TESTAR-FILE-STATUS.                           
                                                                        
           STOP RUN.                                                    
                                                                        
      *---------------------------------------------------------------* 
       90000-99-FIM.   EXIT.                                            
      *---------------------------------------------------------------* 
                                                                        

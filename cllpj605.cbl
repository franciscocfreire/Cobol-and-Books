       ID DIVISION.                                                     
       PROGRAM-ID. CLLPJ605.                                            
       AUTHOR. KLEBER.                                                  
       REMARKS.                                                         
      *------------------------- CLLPJ605 ----------------------------* 
      *              PROGRAMADOR......KLEBER    DIV. 04               * 
      *              SUPERVISOR.......CESAR                           * 
      *              ANALISTA.........ALVARO                          * 
      *              DATA.............JULHO/92                        * 
      *                                                               * 
      *         OBJETIVO.                                             * 
      *                  OBTER  CGC/CPF  ATRAVES DO CAD. DE CHEQUES   * 
      *                                                               * 
      *         ENTRADA.                       SAIDA.                 * 
      *           -ARQANT                        -ARQATU              * 
      *           -CADACHEQ                      -RELATO              * 
      *---------------------------------------------------------------* 
      *                   ULTIMA ALTERACAO                            * 
      *           PROGRAMADOR.........ALESSANDRO FUZINATO / DIV. 09   * 
      *           SUPERVISOR..........CESAR                           * 
      *           ANALISTA............ALVARO / GPO. 57                * 
      *           DATA................FEVEREIRO / 93                  * 
      *                                                               * 
      *           OBJETIVO.                                           * 
      *              -ALTERACAO P/ OBTER O NOME DO CLIENTE JUNTAMENTE * 
      *  COM O NUMERO DO CGC/CPF DA TABELA DB2 DE CLIENTES.           * 
      *---------------------------------------------------------------* 
BRQ141******************************************************************
BRQ141* MAIO/2012 - (BRQ) - TRATAMENTO DE CARTEIRA  ALFANUMERICA        
BRQ141******************************************************************
           EJECT                                                        
      ***************************************************************** 
       ENVIRONMENT DIVISION.                                            
      ***************************************************************** 
                                                                        
       CONFIGURATION SECTION.                                           
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.                           
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    
           SELECT ARQANT   ASSIGN TO UT-S-ARQANT.                       
           SELECT ARQATU   ASSIGN TO UT-S-ARQATU.                       
           SELECT RELATO   ASSIGN TO UT-S-RELATO.                       
           EJECT                                                        
                                                                        
      ***************************************************************** 
       DATA DIVISION.                                                   
      ***************************************************************** 
      ***************************************************************** 
      *               INPUT:  ARQUIVO  ARQANT                         * 
      *           ORGANIZACAO SEQUENCIAL  -  LRECL = 301              * 
      ***************************************************************** 
                                                                        
       FILE SECTION.                                                    
       FD  ARQANT                                                       
           RECORDING F                                                  
           BLOCK 0                                                      
           LABEL RECORDS STANDARD.                                      
                                                                        
       COPY 'I#CLLPRQ'.                                                 
                                                                        
                                                                        
      ***************************************************************** 
      *              OUTPUT:  RELATO                                  * 
      *              RELATORIO       -  LRECL = 133                   * 
      ***************************************************************** 
       FD  RELATO                                                       
           RECORDING F                                                  
           BLOCK 0                                                      
           LABEL RECORDS STANDARD.                                      
                                                                        
       01  REG-RELATO                  PIC X(150).                      
                                                                        
                                                                        
      ***************************************************************** 
      *               OUTPUT: ARQATU                                  * 
      *           ORGANIZACAO SEQUENCIAL  -  LRECL = 401              * 
      ***************************************************************** 
       FD  ARQATU                                                       
           RECORDING F                                                  
           BLOCK 0                                                      
           LABEL RECORDS STANDARD.                                      
                                                                        
       COPY 'I#CLLPRR'.                                                 
                                                                        
      ***************************************************************** 
       WORKING-STORAGE SECTION.                                         
      ***************************************************************** 
                                                                        
       77  FILLER                   PIC X(17) VALUE 'INICIO DA WORKING'.
                                                                        
       77  WRK-CADACHEQ             PIC X(08) VALUE 'CADACHEQ'.         
       77  WRK-CONTROLE             PIC X(02) VALUE SPACES.             
       77  XAV-FIM-ARQANT           PIC X(01) VALUE SPACES.             
       77  ACUPAG                   PIC 9(05) VALUE ZEROS.              
       77  ACULIN                   PIC 9(02) VALUE 99.                 
       77  ACULIDOS                 PIC 9(06) VALUE ZEROS.              
       77  ACU-GRAVADOS             PIC 9(06) VALUE ZEROS.              
       77  ACU-RECUPERADOS          PIC 9(07) COMP-3 VALUE ZEROS.       
       77  MODULO                   PIC X(08) VALUE 'LPCL5011'.         
       77  WRK-FUNCAO               PIC 9(01) VALUE 2.                  
                                                                        
                                                                        
       77  WRK-BATCH                PIC X(08) VALUE 'BATCH'.            
                                                                        
                                                                        
       01  AUX-MSG-ERRO.                                                
           03  FILLER               PIC X(43) VALUE 'PROBLEMA NO ACESSO 
      -    'AO MODULO OBJE0050 RC ='.                                   
           03  AUX-RET-COD          PIC 9(02) VALUE ZEROS.              
           03  FILLER               PIC X(28) VALUE ' AVISAR O ANALISTA 
      -    'DA ROTINA'.                                                 
                                                                        
-INC POL7100C                                                           
                                                                        
                                                                        
       01  WRK-MSG-RET-COD.                                             
           03  WRK-RT-COD-POOL      PIC 9(02) VALUE ZEROS.              
           03  WRK-MENS-POOL        PIC X(14) VALUE '-INCONSISTENTE'.   
                                                                        
       01  WRK-NUM-FIL.                                                 
           03  WRK-NUM              PIC 9(09) VALUE ZEROS.              
           03  WRK-FIL              PIC 9(05) VALUE ZEROS.              
                                                                        
       01  DATA-HORA.                                                   
           03  DT-JULIANA              PIC 9(05)  COMP-3.               
           03  DT-AAMMDD               PIC 9(07)  COMP-3.               
           03  DT-AAAAMMDD             PIC 9(09)  COMP-3.               
           03  TI-HHMMSS               PIC 9(07)  COMP-3.               
           03  TI-HHMMSSMMMMMM         PIC 9(13)  COMP-3.               
           03  TIMESTAMP               PIC X(20).                       
                                                                        
                                                                        
       01  AUX-DATA-SIST.                                               
           03  AUX-AA-SIST          PIC 9999.                           
           03  AUX-MM-SIST          PIC 99.                             
           03  AUX-DD-SIST          PIC 99.                             
       01  AUX-DATA-SIST-R          REDEFINES  AUX-DATA-SIST            
                                    PIC  9(008).                        
                                                                        
       01  NOME-WK.                                                     
           02  NOMEESPA             PIC X(04).                          
           02  NOMECALL             PIC X(36).                          
                                                                        
      /---------------------------------------------------------------* 
      *  DEFINICAO DOS  CABECALHOS DO RELATORIO                       * 
      *---------------------------------------------------------------* 
                                                                        
       01  CAB1-REL.                                                    
           05  FILLER                 PIC X(01) VALUE '1'.              
           05  FILLER                 PIC X(25) VALUE '*CLLPJ605*'.     
           05  FILLER                  PIC X(76) VALUE 'TRANSFERENCIA DA
      -        'S PENDENCIAS EM MORA PARA CL  - ATUALIZACAO DE CGC/CPF'.
           05  FILLER                 PIC X(03) VALUE 'EM '.            
           05  CAB1-DIA-REL           PIC X(02).                        
           05  FILLER                 PIC X(01) VALUE '/'.              
           05  CAB1-MES-REL           PIC X(02).                        
           05  FILLER                 PIC X(01) VALUE '/'.              
           05  CAB1-ANO-REL           PIC X(04).                        
           05  FILLER                 PIC X(13) VALUE '       FOLHA '.  
           05  CAB1-PAG-REL           PIC ZZZZ9.                        
                                                                        
       01  CAB2-REL.                                                    
           05  FILLER                 PIC X(01) VALUE '0'.              
           05  FILLER                 PIC X(11) VALUE 'AGENCIA'.        
           05  FILLER                 PIC X(07) VALUE 'CONTA'.          
           05  FILLER                 PIC X(06) VALUE 'CART'.           
           05  FILLER                 PIC X(11) VALUE 'CONTRATO'.       
           05  FILLER                 PIC X(06) VALUE 'VCTO'.           
           05  FILLER                 PIC X(09) VALUE 'DT.OPER.'.       
           05  FILLER                 PIC X(45) VALUE 'NOME DO DEVEDOR'.
           05  FILLER                 PIC X(21) VALUE 'VR. DO TITULO'.  
           05  FILLER                 PIC X(15) VALUE 'CGC / CPF'.      
           05  FILLER                 PIC X(08) VALUE 'MENSAGEM'.       
                                                                        
      /---------------------------------------------------------------* 
      *  DEFINICAO DA LINHA DETALHE  DO RELATORIO                     * 
      *---------------------------------------------------------------* 
       01  DET-REL.                                                     
           05  DET-CARRO               PIC X(01) VALUE SPACES.          
           05  FILLER                  PIC X(02).                       
           05  DET-AGENCIA             PIC 9(04)B(03).                  
           05  DET-CONTA               PIC 9(07)B(03).                  
BRQ=E******05  DET-CART                PIC 9(03)B(03).                  
BRQ=I      05  DET-CART                PIC X(03).                       
BRQ=I      05  FILLER                  PIC X(03) VALUE SPACES.          
           05  DET-CONTRATO            PIC 9(07)B(01).                  
           05  DET-VCTO                PIC 9(08)B(01).                  
           05  DET-DT-OPER             PIC 9(08)B(01).                  
           05  DET-NOME                PIC X(39)B(01).                  
           05  DET-VLR-TITULO          PIC Z.ZZZ.ZZZ.ZZZ.ZZZ,ZZ.        
           05  FILLER                  PIC X(04) VALUE SPACES.          
           05  DET-CGC-NUM             PIC X(09).                       
           05  FILLER                  PIC X(01) VALUE '/'.             
           05  DET-CGC-FIL             PIC X(04).                       
           05  FILLER                  PIC X(01) VALUE '/'.             
           05  DET-CGC-CTR             PIC X(02)B(02).                  
           05  DET-MENSAGEM            PIC X(18) VALUE SPACES.          
                                                                        
      /---------------------------------------------------------------* 
      *  AREA PARA SALVAR CAMPOS PARA RELATORIO                       * 
      *---------------------------------------------------------------* 
       01  WRK-REL.                                                     
           05  WRK-AGENCIA             PIC 9(04) VALUE ZEROS.           
           05  WRK-CONTA               PIC 9(07) VALUE ZEROS.           
BRQ=E******05  WRK-CART                PIC 9(03) VALUE ZEROS.           
BRQ=I      05  WRK-CART                PIC X(03) VALUE SPACES.          
           05  WRK-CONTRATO            PIC 9(07) VALUE ZEROS.           
           05  WRK-VCTO                PIC 9(08) VALUE ZEROS.           
           05  WRK-DT-OPER             PIC 9(08) VALUE ZEROS.           
           05  WRK-NOME                PIC X(40) VALUE SPACES.          
           05  WRK-VLR-TITULO          PIC 9(13)V9(02) VALUE ZEROS.     
      *---------------------------------------------------------------* 
      *  DEFINICAO DA LINHA DE  TOTAL                                 * 
      *---------------------------------------------------------------* 
                                                                        
       01  LIN-TOT.                                                     
           05  FILLER                  PIC X(01) VALUE '-'.             
           05  FILLER                  PIC X(21) VALUE                  
               'TOTAIS DE REGISTROS'.                                   
           05  TOT-LIDOS               PIC Z.ZZZ.ZZ9.                   
           05  FILLER                  PIC X(07) VALUE '-LIDOS '.       
           05  TOT-GRAVADOS            PIC Z.ZZZ.ZZ9.                   
           05  FILLER                  PIC X(10) VALUE '-GRAVADOS '.    
           05  TOT-RECUPERADOS         PIC Z.ZZZ.ZZ9.                   
           05  FILLER                  PIC X(12) VALUE '-RECUPERADOS'.  
                                                                        
           EXEC SQL                                                     
                INCLUDE SQLCA                                           
           END-EXEC.                                                    
                                                                        
           EXEC SQL                                                     
                INCLUDE OBJEB000                                        
           END-EXEC.                                                    
                                                                        
                                                                        
       01  FILLER     PIC X(16)  VALUE 'FIM  DA  WORKING'.              
           EJECT                                                        
      *---------------------------------------------------------------* 
       PROCEDURE DIVISION.                                              
      *---------------------------------------------------------------* 
      *---------------------------------------------------------------* 
       000-00-INICIO SECTION.                                           
      *---------------------------------------------------------------* 
                                                                        
           CALL  'POOL1050'.                                            
                                                                        
           OPEN INPUT  ARQANT                                           
                OUTPUT ARQATU  RELATO.                                  
                                                                        
                                                                        
           CALL  'POOL7600'  USING  DATA-HORA.                          
           MOVE  DT-AAAAMMDD  TO  AUX-DATA-SIST-R.                      
                                                                        
                                                                        
           MOVE   AUX-DD-SIST     TO     CAB1-DIA-REL.                  
           MOVE   AUX-MM-SIST     TO     CAB1-MES-REL.                  
           MOVE   AUX-AA-SIST     TO     CAB1-ANO-REL.                  
                                                                        
           PERFORM 050-00-LER-ARQANT.                                   
                                                                        
           IF   XAV-FIM-ARQANT   EQUAL 'S'    AND                       
                ACULIDOS         EQUAL ZEROS                            
                                                                        
                DISPLAY  '***********CLLPJ605***********' UPON CONSOLE  
                DISPLAY  '*   "ARQUIVO  ARQANT  ESTA   *' UPON CONSOLE  
                DISPLAY  '*            VAZIO"          *' UPON CONSOLE  
                DISPLAY  '*                            *' UPON CONSOLE  
                DISPLAY  '* "FAVOR AVISAR O ANALISTA   *' UPON CONSOLE  
                DISPLAY  '*          DA ROTINA"        *' UPON CONSOLE  
                DISPLAY  '******************************' UPON CONSOLE  
                GO TO 010-00-FECHA-ARQ.                                 
                                                                        
           PERFORM 200-00-PESQUISA  UNTIL XAV-FIM-ARQANT   EQUAL  'S'.  
                                                                        
           MOVE  ACULIDOS        TO    TOT-LIDOS.                       
           MOVE  ACU-GRAVADOS    TO    TOT-GRAVADOS.                    
           MOVE  ACU-RECUPERADOS TO    TOT-RECUPERADOS.                 
                                                                        
           WRITE REG-RELATO FROM LIN-TOT.                               
                                                                        
       010-00-FECHA-ARQ.                                                
      *-----------------*                                               
                                                                        
           CLOSE ARQANT                                                 
                 ARQATU                                                 
                 RELATO.                                                
           STOP RUN.                                                    
                                                                        
      *---------------------------------------------------------------* 
       000-99-FIM-INICIO. EXIT.                                         
      *---------------------------------------------------------------* 
                                                                        
      /---------------------------------------------------------------* 
       050-00-LER-ARQANT  SECTION.                                      
      *---------------------------------------------------------------* 
                                                                        
           READ ARQANT   AT END                                         
                MOVE     'S'         TO     XAV-FIM-ARQANT              
                GO TO 050-99-FIM-LER-ARQANT.                            
                                                                        
           ADD        1              TO    ACULIDOS.                    
           IF         ANT-FIXO       EQUAL 13             AND           
                      ANT-CGCCPF NOT EQUAL ZEROS                        
                      PERFORM        250-00-SALVA                       
                      PERFORM        300-00-REGRAVA                     
                      GO   TO        050-00-LER-ARQANT.                 
                                                                        
           IF     ANT-NATOPER    EQUAL 064                              
                  IF  ANT-AGEN = 5150 OR 5570                           
                      MOVE 'FRAUDE - CARTOES'  TO  ANT-NOME             
                      PERFORM        250-00-SALVA                       
                      PERFORM        300-00-REGRAVA                     
                      GO   TO        050-00-LER-ARQANT                  
                  ELSE                                                  
                     IF  ANT-AGEN = 5151 OR 5571                        
                         MOVE 'FURTO/EXTRAVIOS - CARTOES' TO ANT-NOME   
                         PERFORM        250-00-SALVA                    
                         PERFORM        300-00-REGRAVA                  
                         GO   TO        050-00-LER-ARQANT.              
                                                                        
           MOVE       ANT-AGEN       TO    AGENCIA        OF OBJEB000.  
           MOVE       ANT-CONTA      TO    CONTA-CORRENTE OF OBJEB000.  
                                                                        
                                                                        
      *---------------------------------------------------------------* 
       050-99-FIM-LER-ARQANT. EXIT.                                     
      *---------------------------------------------------------------* 
                                                                        
      /---------------------------------------------------------------* 
       200-00-PESQUISA SECTION.                                         
      *---------------------------------------------------------------* 
                                                                        
           PERFORM  210-00-ACESSA-TAB-OBJEB000.                         
                                                                        
                                                                        
           IF  SQLCODE     EQUAL  +100                                  
               MOVE  SPACES                TO  DET-CGC-NUM              
                                               DET-CGC-FIL              
                                               DET-CGC-CTR              
               MOVE 'SEM CORRESPONDENTE'   TO  DET-MENSAGEM             
               PERFORM 250-00-SALVA                                     
               PERFORM 300-00-REGRAVA                                   
           ELSE                                                         
               MOVE SPACES    TO DET-MENSAGEM                           
               PERFORM   250-00-SALVA                                   
               PERFORM   400-00-POOL0110.                               
                                                                        
           PERFORM 500-00-LISTA-CAB.                                    
           PERFORM 550-00-IMP-DET.                                      
           PERFORM 050-00-LER-ARQANT.                                   
                                                                        
      *---------------------------------------------------------------* 
       200-99-FIM-PESQUISA. EXIT.                                       
      *---------------------------------------------------------------* 
                                                                        
      /---------------------------------------------------------------* 
       210-00-ACESSA-TAB-OBJEB000  SECTION.                             
      *---------------------------------------------------------------* 
                                                                        
           EXEC  SQL                                                    
               SELECT                                                   
                        NOME,                                           
                        NUM_CGCCPF,                                     
                        FILIAL_CGCCPF,                                  
                        CONTR_CGCCPF                                    
               INTO                                                     
                       :OBJEB000.NOME,                                  
                       :OBJEB000.NUM-CGCCPF,                            
                       :OBJEB000.FILIAL-CGCCPF,                         
                       :OBJEB000.CONTR-CGCCPF                           
               FROM     DB2PRD.OBJE_CLIENTES                            
               WHERE    AGENCIA             =  :OBJEB000.AGENCIA    AND 
                        CONTA_CORRENTE      =  :OBJEB000.CONTA-CORRENTE 
           END-EXEC.                                                    
                                                                        
              IF SQLCODE  NOT EQUAL  +100  AND  ZEROS                   
                 MOVE 'DB2'              TO ERR-TIPO-ACESSO             
                 MOVE 'OBJE_CLIENTES  '  TO ERR-DBD-TAB                 
                 MOVE 'SELECT'           TO ERR-FUN-COMANDO             
                 MOVE SQLCODE            TO ERR-SQL-CODE                
                 MOVE '0001'             TO ERR-LOCAL                   
                 MOVE SPACES             TO ERR-SEGM                    
                 PERFORM 900-ROTINA-ERRO.                               
                                                                        
                                                                        
      *---------------------------------------------------------------* 
       210-99-FIM-ACESSA.   EXIT.                                       
      *---------------------------------------------------------------* 
                                                                        
                                                                        
      /---------------------------------------------------------------* 
       250-00-SALVA   SECTION.                                          
      *---------------------------------------------------------------* 
                                                                        
           MOVE   ANT-AGEN      TO    WRK-AGENCIA.                      
           MOVE   ANT-CONTA     TO    WRK-CONTA.                        
           MOVE   ANT-CART      TO    WRK-CART.                         
           MOVE   ANT-NUMERO    TO    WRK-CONTRATO.                     
           MOVE   ANT-VCTO      TO    WRK-VCTO.                         
           MOVE   ANT-DTINIOP   TO    WRK-DT-OPER.                      
      *    MOVE   ANT-NOME      TO    WRK-NOME.                         
           MOVE   ANT-VRTITULO  TO    WRK-VLR-TITULO.                   
                                                                        
      *---------------------------------------------------------------* 
       250-99-FIM-SALVA.   EXIT.                                        
      *---------------------------------------------------------------* 
      /---------------------------------------------------------------* 
       300-00-REGRAVA   SECTION.                                        
      *---------------------------------------------------------------* 
                                                                        
           MOVE  ANT-REG      TO      ATU-REG.                          
           IF    ANT-CGCNUM NUMERIC   AND                               
                 ANT-CGCFIL NUMERIC   AND                               
                 ANT-CGCCTR NUMERIC                                     
                 MOVE  ANT-CGCNUM   TO      DET-CGC-NUM                 
                 MOVE  ANT-CGCFIL   TO      DET-CGC-FIL                 
                 MOVE  ANT-CGCCTR   TO      DET-CGC-CTR                 
           ELSE                                                         
                 MOVE  ZEROS        TO      ATU-CGCNUM                  
                                            ATU-CGCFIL                  
                                            ATU-CGCCTR.                 
                                                                        
           MOVE  ATU-NOME     TO      NOME-WK.                          
           CALL  'POOL0080'   USING   MODULO  NOME-WK.                  
           MOVE  NOME-WK      TO      ATU-NOME.                         
                                                                        
           WRITE ATU-REG.                                               
           ADD        1       TO      ACU-GRAVADOS.                     
                                                                        
      *---------------------------------------------------------------* 
       300-00-FIM-REGRAVA. EXIT.                                        
      *---------------------------------------------------------------* 
                                                                        
      /---------------------------------------------------------------* 
       400-00-POOL0110   SECTION.                                       
      *---------------------------------------------------------------* 
                                                                        
           MOVE   NUM-CGCCPF    OF OBJEB000  TO     WRK-NUM.            
           MOVE   FILIAL-CGCCPF OF OBJEB000  TO     WRK-FIL.            
                                                                        
           CALL  'POOL0110'  USING    WRK-NUM-FIL                       
                                      WRK-CONTROLE.                     
                                                                        
           IF  (RETURN-CODE                NOT EQUAL  00) AND           
               (CONTR-CGCCPF OF OBJEB000   NOT EQUAL  WRK-CONTROLE)     
                                                                        
               PERFORM 600-00-PROCESSA-INCONS                           
           ELSE                                                         
               PERFORM 700-00-PROCESSA-CONSIST.                         
                                                                        
      *---------------------------------------------------------------* 
       400-99-FIM-POOL0110. EXIT.                                       
      *---------------------------------------------------------------* 
                                                                        
      /---------------------------------------------------------------* 
       500-00-LISTA-CAB SECTION.                                        
      *---------------------------------------------------------------* 
                                                                        
           IF  ACULIN    GREATER    54                                  
               MOVE  ZEROS       TO        ACULIN                       
               ADD     1         TO        ACUPAG                       
               MOVE    ACUPAG    TO        CAB1-PAG-REL                 
               WRITE REG-RELATO FROM  CAB1-REL                          
               WRITE REG-RELATO FROM  CAB2-REL                          
               MOVE    SPACES    TO   REG-RELATO                        
               WRITE REG-RELATO.                                        
                                                                        
      *---------------------------------------------------------------* 
       500-99-FIM-LISTA-CAB. EXIT.                                      
      *---------------------------------------------------------------* 
                                                                        
      /---------------------------------------------------------------* 
       550-00-IMP-DET SECTION.                                          
      *---------------------------------------------------------------* 
                                                                        
           MOVE   WRK-AGENCIA    TO    DET-AGENCIA.                     
           MOVE   WRK-CONTA      TO    DET-CONTA.                       
           MOVE   WRK-CART       TO    DET-CART.                        
           MOVE   WRK-CONTRATO   TO    DET-CONTRATO.                    
           MOVE   WRK-VCTO       TO    DET-VCTO.                        
           MOVE   WRK-DT-OPER    TO    DET-DT-OPER.                     
           MOVE   WRK-NOME       TO    DET-NOME.                        
           MOVE   WRK-VLR-TITULO TO    DET-VLR-TITULO.                  
           WRITE   REG-RELATO FROM   DET-REL.                           
           ADD      1           TO    ACULIN.                           
                                                                        
      *---------------------------------------------------------------* 
       550-99-FIM-IMP-DET. EXIT.                                        
      *---------------------------------------------------------------* 
                                                                        
      /---------------------------------------------------------------* 
       600-00-PROCESSA-INCONS SECTION.                                  
      *---------------------------------------------------------------* 
                                                                        
           MOVE  RETURN-CODE       TO      WRK-RT-COD-POOL.             
           MOVE  WRK-MSG-RET-COD   TO      DET-MENSAGEM.                
           MOVE  NUM-CGCCPF    OF OBJEB000   TO    DET-CGC-NUM.         
           MOVE  FILIAL-CGCCPF OF OBJEB000   TO    DET-CGC-FIL.         
           MOVE  CONTR-CGCCPF  OF OBJEB000   TO    DET-CGC-CTR.         
           MOVE  ANT-REG                     TO    ATU-REG.             
           IF    ANT-CGCNUM NUMERIC   AND                               
                 ANT-CGCFIL NUMERIC   AND                               
                 ANT-CGCCTR NUMERIC                                     
                 MOVE  ANT-CGCNUM            TO      DET-CGC-NUM        
                 MOVE  ANT-CGCFIL            TO      DET-CGC-FIL        
                 MOVE  ANT-CGCCTR            TO      DET-CGC-CTR        
           ELSE                                                         
                 MOVE  ZEROS        TO      ATU-CGCNUM                  
                                            ATU-CGCFIL                  
                                            ATU-CGCCTR.                 
                                                                        
           MOVE  ATU-NOME     TO      NOME-WK.                          
           CALL  'POOL0080'   USING   MODULO  NOME-WK.                  
           MOVE  NOME-WK                     TO      ATU-NOME           
                                                     WRK-NOME.          
                                                                        
           WRITE ATU-REG.                                               
           ADD        1            TO      ACU-GRAVADOS.                
                                                                        
      *---------------------------------------------------------------* 
       600-99-FIM-PROCESSA-INCONS. EXIT.                                
      *---------------------------------------------------------------* 
                                                                        
      /---------------------------------------------------------------* 
       700-00-PROCESSA-CONSIST  SECTION.                                
      *---------------------------------------------------------------* 
                                                                        
           MOVE     ANT-REG                     TO    ATU-REG.          
           MOVE     NUM-CGCCPF    OF OBJEB000   TO    ATU-CGCNUM        
                                                      DET-CGC-NUM.      
           MOVE     FILIAL-CGCCPF OF OBJEB000   TO    ATU-CGCFIL        
                                                      DET-CGC-FIL.      
           MOVE     CONTR-CGCCPF  OF OBJEB000   TO    ATU-CGCCTR        
                                                      DET-CGC-CTR.      
                                                                        
           MOVE     ATU-NOME                 TO      NOME-WK.           
           CALL  'POOL0080'   USING   MODULO  NOME-WK.                  
           MOVE     NOME-WK                  TO      ATU-NOME           
                                                     ANT-NOME.          
                                                                        
           IF  ANT-NOME6  EQUAL  SPACES  OR  '*'                        
               MOVE  NOME OF OBJEB000  TO  ATU-NOME                     
                                           DET-NOME                     
           ELSE                                                         
               MOVE  ATU-NOME          TO  WRK-NOME.                    
                                                                        
           WRITE ATU-REG.                                               
           ADD        1               TO       ACU-GRAVADOS             
                                               ACU-RECUPERADOS.         
                                                                        
      *---------------------------------------------------------------* 
       700-99-FIM-PROCESSA-CONSIST. EXIT.                               
      *---------------------------------------------------------------* 
                                                                        
      /---------------------------------------------------------------* 
       900-ROTINA-ERRO  SECTION.                                        
      *---------------------------------------------------------------* 
                                                                        
           MOVE  'CLLPJ605'  TO  ERR-PGM                                
                                                                        
           CALL  'POOL7100'  USING  WRK-BATCH                           
                                    ERRO-AREA                           
                                    SQLCA.                              
                                                                        
           GOBACK.                                                      
                                                                        
                                                                        
      *---------------------------------------------------------------* 
       900-99-FIM-POOL7100. EXIT.                                       
      *---------------------------------------------------------------* 

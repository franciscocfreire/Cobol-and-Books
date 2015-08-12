      ****************************************************************  
      *                                                              *  
      * NOME BOOK : ECORW392                                         *  
      * DESCRICAO : AREA DE COMUNICACAO - API PARA GRAVACAO DE       *  
      *             INTERFACE -NOVA ARQUITETURA  VERSAO 04           *  
      *             INCLUSAO INDICADOR ORIGEM ENDERECO               *  
      *                                                              *  
      * DATA      : 28/08/2013                                       *  
      * AUTOR     : DEBORA                                           *  
      * EMPRESA   : CPMBRAXIS                                        *  
      * GRUPO     : COMPONENTES TECNICOS GP-97                       *  
      * COMPONENTE: ENVIO DE CORRESPONDENCIA AOS CLIENTES            *  
      *                                                              *  
      ****************************************************************  
      *                                                              *  
      * ECORW392-HEADER                                              *  
      *  ECORW392-COD-LAYOUT = CODIGO DESSE LAYOUT. (NAO MODIFICAR)  *  
      *  ECORW392-TAM-LAYOUT = TAMANHO DO REGISTRO  (NAO MODIFICAR)  *  
      *                                                              *  
      * ECORW392-REGISTRO                                            *  
      *                                                              *  
      *  ECORW392-SAIDA. (NAO MODIFICAR, MONTADA PELA API) ********* *  
      *                                                              *  
      *    ECORW392-BLOCO-RETORNO                                    *  
      *                                                              *  
      *     ECORW392-COD-RETORNO  = RESULTADO DA ACAO                *  
      *                             00 - GRAVACAO OK                 *  
      *                             04 - ATENCAO : LOTE GERADO COM   *  
      *                                  BLOQUEIO (DOCUMENTO BLOQUEA-*  
      *                             08 - ERRO NOS DADOS DE ENTRADA   *  
      *                             16 - ERRO NO ACESSO AOS DADOS    *  
      *     ECORW392-MENSAGEM     = TEXTO DA MENSAGEM EXTRAIDO DO    *  
      *                             GMSG                             *  
      *                                                              *  
      *    ECORW392-BLOCO-LOTE.                                      *  
      *     ECORW392-NUM-LOTE     = NUMERO ATRIBUIDO AO LOTE         *  
      *     ECORW392-DAT-GERAC    = DATA DA GERACAO DO LOTE          *  
      *    ECORW392-BLOCO-DATAS.                                     *  
      *     ECORW392-DAT-PREV-IMPR= DATA PREVISTA PARA A IMPRESSAO   *  
      *     ECORW392-DAT-ARMAZ    = DATA DE ARMAZENDOMENTO DO        *  
      *                             DOCUMENTO                        *  
      *     ECORW392-DAT-FIM-DISP = DATA DE FIM DE DISPONIBILIDADE   *  
      *                                                              *  
      *                                                              *  
      *  ECORW392-ENTRADA ********************************************  
      *                                                              *  
      *   ECORW392-ACAO         = ACAO A SER EXECUTADA NA CHAMADA    *  
      *                           G - GRAVAR UMA LINHA               *  
      *                           F - FECHAR O RELATORIO             *  
      *                                                              *  
      *   ECORW392-DETALHE                                           *  
      *                                                              *  
      *     ECORW392-SEQ-REG      = SEQUENCIA CRESCENTE DE CADA      *  
      *                             PAGINA                           *  
      *    ECORW392-BLOCO-ORIGEM                                     *  
      *     ECORW392-SIS-ENVIO    = CODIGO DO SISTEMA                *  
      *     ECORW392-COD-PROGRAMA = CODIGO DO PROGRAMA               *  
      *     ECORW392-DAT-MOVI-APLIC-FUNCL = DATA MOVIMENTO DA        *  
      *                                    APLICACAO FUNCIONAL       *  
      *                                                              *  
      *                                                              *  
      *    ECORW392-BLOCO-CLIENTE                                    *  
      *     ECORW392-DISTRIB-CORRESP = TIPO DISTRIBUICAO CORRESPON-  *  
      *                                DENCIA                        *  
      *                                0-CORREIO,                    *  
      *                                1-INTERNA                     *  
      *     ECORW392-INDIC-VINCULO-ORGNZ = INDICADOR DO VINCULO DO   *  
      *                                    CLIENTE COM A ORGANIZACAO *  
      *                                    0-CLIENTE-COMUM,          *  
      *                                    1-CLIENTE-FUNCIONARIO,    *  
      *                                    2-CLIENTE-DIRETOR,        *  
      *                                    3-CLIENTE-PARENTE-DIRETOR *  
      *     ECORW392-INDIC-CADTO-CLIENTE = INDICADOR DO CADASTRAMENTO*  
      *                                    DO CLIENTE NA ORGANIZACAO *  
      *                                    S-CLIENTE COM CADASTRO    *  
      *                                    N-CLIENTE SEM CADASTRO    *  
      *     ECORW392-CPSSOA-ENDER        = CLUB                      *  
      *     ECORW392-CPSSOA-JURID-ENDER  = EMPRESA                   *  
      *     ECORW392-CSEQ-ENDER-PSSOA    = SEQUENCIA ENDERECO        *  
      *     ECORW392-CFUNC-BDSCO         = COD. FUNCIONARIO          *  
      *     ECORW392-COD-ESTRUT-DICD = CODIGO ESTRUTURA IDENTIFICA-  *  
      *                                AGRUPAMENTO CLIENTE NO DICD   *  
      *     ECORW392-NUM-VERSAO-DICD = NUMERO VERSAO ESTRUTURA IDEN- *  
      *                                TIFICADOR AGRUPAMENTO CLIENTE *  
      *                                NO DICD                       *  
      *     ECORW392-IDENTIF-CLI-DSTNO = IDENTIFICADOR CLIENTE PARA  *  
      *                                  AGRUPAMENTO                 *  
      *     ECORW392-NOME-CLIENTE = NOME DO CLIENTE PARA ENDERECAMEN-*  
      *                             TO DA CORRESPONDENCIA            *  
      *     ECORW392-LOGRADOURO = NOME DO LOGRADOURO PARA ENDERECA-  *  
      *                           MENTO DA CORRESPONDENCIA           *  
      *     ECORW392-BAIRRO = NOME DO BAIRRO PARA ENDERECAMENTO DA   *  
      *                       CORRESPONDENCIA                        *  
      *     ECORW392-MUNICIPIO = NOME DO MUNICIPIO PARA ENDERECAMENTO*  
      *                          DA CORRESPONDENCIA                  *  
      *     ECORW392-COD-UF = CODIGO UNIDADE DA FEDERACAO PARA ENDE- *  
      *                       RECAMENTO DA CORRESPONDENCIA           *  
      *     ECORW392-COD-CEP = CODIGO CEP PARA ENDERECAMENTO DA COR- *  
      *                        RESPONDENCIA                          *  
      *     ECORW392-COMPL-CEP = CODIGO COMPLEMENTO CEP PARA ENDERE- *  
      *                       CAMENTO DA CORRESPONDENCIA             *  
      *     ECORW392-SGMTO-CLIENTE = SEGMENTACAO DE CLIENTE UTILIZA- *  
      *                       DA PARA SEPARACAO DA CORRESPONDENCIA   *  
      *                                                              *  
      *                                                              *  
      *    ECORW392-BLOCO-DEFNC-ECOR                                 *  
      *     ECORW392-COD-DOCTO    = CODIGO DO DOCUMENTO              *  
      *     ECORW392-COD-FORM     = CODIGO DO FORMULARIO ASSOCIADO   *  
      *                             AO DOCUMENTO                     *  
      *     ECORW392-CNL-ENVIO    = CANAL DE ENVIO (BAT E EML)       *  
      *     ECORW392-COD-IDIOMA   = CODIGO IDIOMA CORRESPONDENCIA    *  
      *                                                              *  
      *                                                              *  
      *    ECORW392-BLOCO-PAGINACAO-DOCTO                            *  
      *     ECORW392-SEQ-PAGINA   = SEQUENCIA CRESCENTE DE CADA      *  
      *                             PAGINA DA CORRESPONDENCIA        *  
      *     ECORW392-QTD-PAGINAS  = QUANTIDADE DE PAGINAS DA         *  
      *                             CORRESPONDENCIA                  *  
      *    ECORW392-BANCO         = BANCO CLIENTE                    *  
      *    ECORW392-AGENCIA       = AGENCIA CLIENTE                  *  
      *    ECORW392-CONTA         = CONTA CLIENTE                    *  
      *    ECORW392-TIPO-PSSOA    = TIPO PESSOA FISICA OU JURIDICA   *  
      *      ECORW392-FISICA      = VALOR 'F' FISICA                 *  
      *      ECORW392-JURIDICA    = VALOR 'J' JURIDICA               *  
      *    ECORW392-CPF-CNPJ      = NUMERO CPF  OU CNPJ              *  
      *    ECORW392-CCTRL-CPF     = NUMERO DE CONTROLE CPF           *  
      *    ECORW392-CFLIL-CNPJ    = NUMERO DA FILIAL CNPJ            *  
      *    ECORW392-CPSSOA-JURID-CONTR = CONTRADO PESSOA JURIDICA    *  
      *    ECORW392-CTIPO-CONTR-NEGOC  = TIPO DE CONTRATO            *  
      *    ECORW392-NSEQ-CONTR-NEGOC   = NUMERO DE SEQUENCIA CONTRATO*  
      *    ECORW392-TIPO-ARQ      = INDICADOR DA ARQUITETURA LEGADO  *  
      *                             OU NOVA                          *  
      *      ECORW392-ARQ-NOVA    = VALOR '0' ARQUITETURA NOVA       *  
      *      ECORW392-ARQ-LEGAD   = VALOR '1' ARQUITETURA LEGADO     *  
      *                                                              *  
      *    ECORW392-DADOS-UORG    = INFORMACOES DE DEPTO/DEPENDENCIA *  
      *      ECORW392-CPSSOA-INTRN = PESSOA JURIDICA DISTRIB.INTERNA *  
      *      ECORW392-NSEQ-INTRN  = NUMERO DE SEQ.  DISTRIB.INTERNA  *  
      *      ECORW392-CPSSOA-JURID-REM = PESSOA JURIDICA REMETENTE   *  
      *      ECORW392-NSEQ-UND-ORGNZ-REM = NUMERO DE SEQ. REMETENTE  *  
      *      ECORW392-CPSSOA-JURID-DEV = PESSOA JURIDICA DEVOLUCAO   *  
      *      ECORW392-NSEQ-UND-ORGNZ-DEV = NUMERO DE SEQ. DEVOLUCAO  *  
      *                                                              *  
      *    ECORW392-CSIST-BASE-ENDER=CENTRO DE CUSTO ORIGEM ENDERECO *  
      *                                                              *  
      *   ECORW392-FECHAMENTO (REDEFINE DETALHE NA POSICAO DE SEQ    *  
      *    ECORW392-TOT-REGS = TOTAL DE REGISTROS ENVIADOS PELA      *  
      *                        APLICACAO,INFORMADO SO NO FECHAMENTO  *  
      *                                                              *  
      *   ECORW392-BLOCO-IMPRESSAO                                   *  
      *    ECORW392-PAGINA   = DADOS PARA PREENCHIMENTO DO TEMPLATE  *  
      *                        DA CORRESPONDENCIA, NAO INFORMAR NO   *  
      *                        FECHAMENTO                            *  
      *                                                              *  
      ****************************************************************  
           05 ECORW392-HEADER.                                          
              10 ECORW392-COD-LAYOUT                    PIC X(08)  VALUE
                 'ECORW392'.                                            
                 88 ECORW392-COD-LAYOUT-API-V000 VALUE 'ECORW092'.      
                 88 ECORW392-COD-LAYOUT-API-V001 VALUE 'ECORW192'.      
                 88 ECORW392-COD-LAYOUT-API-V002 VALUE 'ECORW292'.      
                 88 ECORW392-COD-LAYOUT-API-V003 VALUE 'ECORW392'.      
                 88 ECORW392-COD-LAYOUT-API-VOK  VALUE 'ECORW092'       
                                                       'ECORW192'       
12345                                                  'ECORW292'       
12345                                                  'ECORW392'.      
              10 ECORW392-TAM-LAYOUT                    PIC 9(05)  VALUE
                10000.                                                  
      *                                                                 
           05 ECORW392-REGISTRO.                                        
              10 ECORW392-SAIDA.                                        
                 15 ECORW392-BLOCO-RETORNO.                             
                    20 ECORW392-COD-RETORNO   PIC 9(002).               
                    20 ECORW392-COD-ERRO      PIC X(004).               
                    20 ECORW392-MENSAGEM      PIC X(080).               
      *                                                                 
                 15 ECORW392-BLOCO-LOTE.                                
                    20 ECORW392-NUM-LOTE      PIC S9(015) COMP-3.       
                    20 ECORW392-DAT-GERAC     PIC 9(008)  COMP-3.       
      *                                                                 
                 15 ECORW392-BLOCO-DATAS.                               
                    20 ECORW392-DAT-PREV-IMPR PIC 9(008)  COMP-3.       
                    20 ECORW392-DAT-ARMAZ     PIC 9(008)  COMP-3.       
                    20 ECORW392-DAT-FIM-DISP  PIC 9(008)  COMP-3.       
      *                                                                 
              10 ECORW392-ENTRADA.                                      
                 15 ECORW392-ACAO             PIC X(001).               
                    88 ECORW392-GRAVAR    VALUE 'G'.                    
                    88 ECORW392-FECHAR    VALUE 'F'.                    
      *                                                                 
                 15 ECORW392-DETALHE.                                   
                    20 ECORW392-SEQ-REG       PIC S9(009) COMP-3.       
                    20 ECORW392-BLOCO-ORIGEM.                           
                       25 ECORW392-SIS-ENVIO  PIC X(004).               
                       25 ECORW392-COD-PROGRAMA                         
                                              PIC X(008).               
                       25 ECORW392-DAT-MOVI-APLIC-FUNCL                 
                                              PIC S9(008) COMP-3.       
      *                                                                 
      *          DATA DA APLICACAO FUNCIONAL QUE REFERENCIA O LOTE      
      *          DE CORRESPONDENCIAS GERADO COMO INTERFACE ENTRE        
      *          A APLICACAO FUNCIONAL E O ECOR NO FORMATO AAAAMMDD.    
      *                                                                 
                    20 ECORW392-BLOCO-CLIENTE.                          
                       25 ECORW392-DISTRIB-CORRESP                      
                                              PIC S9(001) COMP-3.       
                          88 ECORW392-DIST-CORREIO         VALUE +0.    
                          88 ECORW392-DIST-INTERNA         VALUE +1.    
      *                                                                 
      * INDICADOR DO VINCULO DO CLIENTE COM A ORGANIZACAO               
      *                                                                 
                       25 ECORW392-INDIC-VINCULO-ORGNZ                  
                                              PIC S9(001) COMP-3.       
                          88 ECORW392-CLIENTE-COMUM        VALUE +0.    
                          88 ECORW392-CLIENTE-FUNCIONARIO  VALUE +1.    
                          88 ECORW392-CLIENTE-DIRETOR      VALUE +2.    
                          88 ECORW392-CLIENTE-PARENTE-DIR  VALUE +3.    
      *                                                                 
      * INDICADOR DO CADASTRAMENTO DO CLIENTE NA ORGANIZACAO            
      *                                                                 
                       25 ECORW392-INDIC-CADTO-CLIENTE                  
                                              PIC X(001).               
                          88 ECORW392-CLIENTE-COM-CADTO    VALUE 'S'.   
                          88 ECORW392-CLIENTE-SEM-CADTO    VALUE 'N'.   
      *                                                                 
      * SE CLIENTE CADASTRADO-ATRIBUTOS PARA RECUPERACAO DO NOME E      
      * ENDERECO DO CLIENTE NO CADU PARA REIMPRESSAO BATCH              
      *                                                                 
                       25 ECORW392-CPSSOA-ENDER-PSTAL                   
                                              PIC S9(010) COMP-3.       
                       25 ECORW392-CPSSOA-JURID-PSTAL                   
                                              PIC S9(010) COMP-3.       
                       25 ECORW392-CSEQ-ENDER-PSSOA                     
                                              PIC S9(005) COMP-3.       
      *                                                                 
      * IDENTIFICADOR PARA AGRUPAMENTO DE CORRESPONDENCIA               
      *                                                                 
                       25 ECORW392-COD-ESTRUT-DICD                      
                                              PIC S9(009) COMP.         
                       25 ECORW392-NUM-VERSAO-DICD                      
                                              PIC S9(009) COMP.         
                       25 ECORW392-IDENTIF-CLI-DSTNO                    
                                              PIC X(040).               
      *                                                                 
      * ENDERECAMENTO DA CORRESPONDENCIA                                
      *                                                                 
                       25 ECORW392-NOME-CLIENTE                         
                                              PIC X(040).               
                       25 ECORW392-LOGRADOURO PIC X(040).               
                       25 ECORW392-BAIRRO     PIC X(020).               
                       25 ECORW392-MUNICIPIO  PIC X(030).               
                       25 ECORW392-COD-UF     PIC X(002).               
                       25 ECORW392-COD-CEP    PIC S9(005) COMP-3.       
                       25 ECORW392-COMPL-CEP  PIC S9(003) COMP-3.       
      *                                                                 
      * SE FUNCIONARIO ATRIBUTO PARA RECUPERACAO DE NOME E ENDERECO     
      * INTERNO DO FUNCIONARIO NO SARAH PARA REIMPRESSAO BATCH          
      *                                                                 
                       25 ECORW392-CFUNC-BDSCO                          
                                              PIC S9(009) COMP-3.       
      *                                                                 
      * CARACTERIZACAO DO CLIENTE COM RELACAO AO NEGOCIO                
      *                                                                 
                       25 ECORW392-SGMTO-CLIENTE                        
                                              PIC S9(003) COMP-3.       
      *                                                                 
      * PARAMETRIZACAO DA CORRESPONDENCIA NO ECOR                       
      *                                                                 
                    20 ECORW392-BLOCO-DEFNC-ECOR.                       
                       25 ECORW392-COD-DOCTO  PIC X(008).               
                       25 ECORW392-COD-FORM   PIC X(008).               
                       25 ECORW392-CNL-ENVIO  PIC S9(003) COMP-3.       
                       25 ECORW392-COD-IDIOMA PIC S9(003) COMP-3.       
      *                                                                 
                    20 ECORW392-BLOCO-PAGINACAO-DOCTO.                  
                       25 ECORW392-SEQ-PAGINA PIC S9(007) COMP-3.       
                       25 ECORW392-QTD-PAGINAS                          
                                              PIC S9(007) COMP-3.       
                                                                        
                    20 ECORW392-PROCESSO      PIC X(001).               
                       88 ECORW392-NORMAL    VALUE 'N'.                 
                       88 ECORW392-RESTART   VALUE 'R'.                 
                       88 ECORW392-REINICIO  VALUE 'I'.                 
                    20 ECORW392-AREA-RESTART  PIC X(800).               
      *                                                                 
      * DADOS DO CLIENTE:   AGENCIA, BANCO, CONTA                       
      *                                                                 
                    20 ECORW392-BANCO         PIC S9(003) COMP-3.       
      *                                                                 
                    20 ECORW392-AGENCIA       PIC S9(005) COMP-3.       
      *                                                                 
                    20 ECORW392-CONTA         PIC S9(013) COMP-3.       
      *                                                                 
      * IDENTIFICADOR TIPO PESSOA FISICA OU JURIDICA                    
      *                                                                 
                    20 ECORW392-TIPO-PSSOA    PIC X(001).               
                       88 ECORW392-FISICA    VALUE 'F'.                 
                       88 ECORW392-JURIDICA  VALUE 'J'.                 
      *                                                                 
      * DADOS DO CLIENTE:   CNPJ, CPF                                   
      *                                                                 
                    20 ECORW392-DADOS-CPF-CNPJ.                         
                       25 ECORW392-CPF-CNPJ   PIC S9(009) COMP-3.       
                       25 ECORW392-CCTRL-CPF-CNPJ                       
                                              PIC S9(002) COMP-3.       
                       25 ECORW392-CFLIL-CNPJ PIC S9(004) COMP-3.       
      *                                                                 
      * DADOS DO CONTRATO                                               
      *                                                                 
                    20 ECORW392-DADOS-CONTRATO.                         
                       25 ECORW392-CPSSOA-JURID-CONTR                   
                                              PIC S9(010) COMP-3.       
                       25 ECORW392-CTIPO-CONTR-NEGOC                    
                                              PIC S9(003) COMP-3.       
                       25 ECORW392-NSEQ-CONTR-NEGOC                     
                                              PIC S9(010) COMP-3.       
      *                                                                 
      * IDENTIFICADOR  DE ARQUITETURA    LEGADO OU NOVA                 
      *                                                                 
                    20 ECORW392-TIPO-ARQ      PIC X(001).               
                       88 ECORW392-ARQ-NOVA  VALUE '0'.                 
                       88 ECORW392-ARQ-LEGAD VALUE '1'.                 
                                                                        
      *                                                                 
      * DADOS IDENDIFICACAO - UORG                                      
      *                                                                 
                    20 ECORW392-DADOS-UORG.                             
                       25 ECORW392-CPSSOA-INTRN                         
                                              PIC S9(010) COMP-3.       
                       25 ECORW392-NSEQ-INTRN                           
                                              PIC S9(008) COMP-3.       
      *                                                                 
      * DADOS IDENDIFICACAO - UORG AGENCIA DE DEVOLUCAO E REMETENTE     
      *                                                                 
                    20 ECORW392-DADOS-UORG-AGENCIA.                     
                       25 ECORW392-CPSSOA-JURID-REM                     
                                                   PIC S9(010) COMP-3.  
                       25 ECORW392-NSEQ-UND-ORGNZ-REM                   
                                                   PIC S9(008) COMP-3.  
                       25 ECORW392-CPSSOA-JURID-DEV                     
                                                   PIC S9(010) COMP-3.  
                       25 ECORW392-NSEQ-UND-ORGNZ-DEV                   
                                                   PIC S9(008) COMP-3.  
                                                                        
                    20 ECORW392-CSIST-BASE-ENDER   PIC X(004).          
                                                                        
      *          RESERVA PARA EXPANSAO                                  
                    20 FILLER                 PIC X(544).               
                                                                        
                 15 ECORW392-FECHAMENTO REDEFINES ECORW392-DETALHE.     
                    20 ECORW392-TOT-REGS      PIC S9(009) COMP-3.       
                    20 FILLER                 PIC X(1675).              
                 15 ECORW392-BLOCO-IMPRESSAO.                           
                    20 ECORW392-PAGINA        PIC X(8192).              

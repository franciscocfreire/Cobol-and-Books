      **** I#CLLPZF ****************************************************
      *                                                                *
      *    BOOK PARA ENVIO DE DADOS PARA MODULO CEDD2030               *
      *    TAMANHO : 350 BYTES                                         *
      *                                                                *
      *    CONFECCAO: KLENK / VINICIUS - BSI - JUNHO/2013              *
      *                                                                *
      ******************************************************************
       01           CLLPZF-AREA.                                        
      *             AREA TOTAL DA SUBROTINA                             
           05       CLLPZF-CINFO-FRANQ-CORSP      PIC  X(34).           
      *             CODIGO CIF DA CORSP - BRANCO PARA REJEITADAS        
           05       CLLPZF-DPOSTAGEM-CORSP        PIC  X(10).           
      *             DATA DA POSTAGEM - BRANCO PARA REJEITADAS           
           05       CLLPZF-DFAC-CORSP             PIC  X(10).           
      *             DATA DA FAC DA CORSP                                
           05       CLLPZF-GESTOR                 PIC  9(05).           
      *             CODIGO DO GESTOR RESPONSAVEL PELA CORSP             
           05       CLLPZF-ROTINA                 PIC  X(08).           
      *             CODIGO DA ROTINA EMISSORA DA CORSP                  
           05       CLLPZF-CBCO-COBR-TARIF        PIC  9(03).           
      *             CODIGO DO BANCO DO CLIENTE                          
           05       CLLPZF-COD-AGE-CLI            PIC  9(05).           
      *             CODIGO DE AGENCIA DO CLIENTE                        
           05       CLLPZF-CONTA-COR-CLI          PIC  9(13).           
      *             CODIGO DA CONTA CORRENTE DO CLIENTE                 
           05       CLLPZF-TIPO-CLIENTE           PIC  X(01).           
      *             INDICADOR DO TIPO DE PESSOA DO CLIENTE:             
      *                    F = FISICA                                   
      *                    J = JURIDICA                                 
           05       CLLPZF-CD-CPF-CNPJ            PIC  9(09).           
      *             CNPJ OU CPF DO CLIENTE                              
           05       CLLPZF-CD-FILIAL-CNPJ         PIC  9(04).           
      *             CODIGO FILIA DO CNPJ -  ZEROS QUANDO PESSOA FISICA  
           05       CLLPZF-CONTROLE-CPF-CNPJ      PIC  9(02).           
      *             CODIGO DE CONTROLE DO CNPJ OU DV DO CPF             
           05       CLLPZF-ICLI-CORSP             PIC  X(40).           
      *             NOME DO CLIENTE                                     
           05       CLLPZF-ILOGDR-CLI-CORSP       PIC  X(40).           
      *             ENDERECO DO CLIENTE                                 
           05       CLLPZF-IBAIRO-CLI-CORSP       PIC  X(20).           
      *             BAIRRO DO CLIENTE                                   
           05       CLLPZF-IMUN-CLI-CORSP         PIC  X(30).           
      *             CIDADE DO CLIENTE                                   
           05       CLLPZF-CUF-CLI-CORSP          PIC  X(02).           
      *             CODIGO DA UF DO CLIENTE                             
           05       CLLPZF-CCEP-CLI-CORSP         PIC  9(05).           
      *             CEP DO CLIENTE                                      
           05       CLLPZF-CCOMPL-CEP-CLI         PIC  9(03).           
      *             COMPLEMENTO DO CEP DO CLIENTE                       
           05       CLLPZF-CLUB                   PIC  9(10).           
      *             CODIGO CLUB (CODIGO UNICO DO CLIENTE)               
           05       CLLPZF-SEQ-ENDER              PIC  9(05).           
      *             SEQUENCIA DO ENDERECO                               
           05       CLLPZF-CPSSOA-JURID-CONTR     PIC  9(10).           
      *             CODIGO DO CONTRATO DE PESSOA JURIDICA               
           05       CLLPZF-CTPO-CONTR-NEGOC       PIC  9(03).           
      *             CODIGO DO TIPO DE CONTRATO DE PESSOA JURIDICA       
           05       CLLPZF-NSEQ-CONTR-NEGOC       PIC  9(10).           
      *             CODIGO DE SEQUENCIA DO CONTRATO DE PESSOA JURIDICA  
           05       CLLPZF-CIDTFD-DOCTO-GERDR     PIC  9(15).           
      *             ID DO DOCUMENTO - ZEROS PARA LEGADO OU REJEITADAS   
           05       CLLPZF-BASE-ENDER             PIC  X(04).           
      *             BASE DO ENDERECO                                    
           05       CLLPZF-SIT-CORSP              PIC  9(03).           
      *             CODIGO DE SITUACAO DA CORSP:                        
      *                    01 - POSTADA                                 
      *                    02 - POSTADA COM BAIRRO EM BRANCO(WARNING)   
      *                    49 - POSTADA COM OUTRO WARNING               
      *                    50 - REJEITADA POR FALTA DE RUA              
      *                    51 - REJEITADA POR FALTA DE NUMERO           
      *                    52 - REJEITADA POR FALTA DE CEP              
      *                    53 - REJEITADA POR CEP NAO CONSTAR NAS BASES 
      *                    99 - REJEITADA POR OUTROS MOTIVOS            
           05       CLLPZF-CCUSTO-ORIGEM          PIC  X(04).           
      *             CODIGO DO SISTEMA GERADOR DA INFORMACAO             
           05       FILLER                        PIC  X(42).           
      *             AREA RESERVADA                                      
      **** I#CLLPZF ****************************************************

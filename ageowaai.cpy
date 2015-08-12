      *================================================================*
      * COPY .....: AGEOWAAI  - ( 15981 BYTES )                        *
      *                                                                *
      * DESCRICAO.: AREA DE COMUNICACAO DOS SISTEMAS DO BRADESCO COM O *
      *       COMPONENTE ESTRUTURAL AREA GEOGRAFICA DA NOVA ARQUITETURA*
      *       DE SISTEMAS PARA TRATAMENTO DA FUNCIONALIDADE:           *
      *                                                                *
      * SF1007 .: LISTA PAISES                        VERSAO .: 001    *
      *                                                                *
      * GRUPO...: DDS-GP-34          COMPONENTE AGEO - AREA GEOGRAFICA *
      *================================================================*
      ***  DADOS DE IDENTIFICACAO DA ESTRUTURA DA SF1007             ***
      *================================================================*
       01  AGEOWAAI.                                                    
           03  FILLER                  PIC  X(008) VALUE 'AGEOWAAI'.    
           03  FILLER                  PIC  9(005) VALUE  15981.        
           03  AGEOWAAI-FUNCAO         PIC  X(006) VALUE 'SF1007'.      
               88  CALE-SF1007                     VALUE 'SF1007'.      
           03  AGEOWAAI-VERSAO         PIC  9(003) VALUE  001.          
      *================================================================*
      ***  DADOS PARA CONTROLE DE PROCESSAMENTO DA API               ***
      *================================================================*
           03  AGEOWAAI-BLOCO-RETORNO.                                  
               05  AGEOWAAI-COD-RET    PIC  9(002) VALUE  ZEROS.        
               05  AGEOWAAI-COD-ERRO   PIC  X(004) VALUE  ZEROS.        
               05  AGEOWAAI-COD-MSGEM  PIC  X(008) VALUE  SPACES.       
      *================================================================*
      ***  DADOS DE ENTRADA PARA TRATAMENTO DA FUNCIONALIDADE        ***
      *================================================================*
           03  AGEOWAAI-ENTRADA.                                        
               05  AGEOWAAI-CIRECP-CNTTE   PIC  9(005) VALUE  ZEROS.    
               05  AGEOWAAI-COD-CNTTE-E    PIC  X(010) VALUE  ZEROS.    
               05  AGEOWAAI-CIRECP-PAIS    PIC  9(005) VALUE  ZEROS.    
               05  AGEOWAAI-COD-PAIS-E     PIC  X(010) VALUE  ZEROS.    
               05  AGEOWAAI-TIP-TERRITORIO PIC  X(001) VALUE  SPACES.   
                   88  AGEOWAAI-TODOS           VALUE 'T'.              
                   88  AGEOWAAI-CONTINENTAL     VALUE 'C'.              
                   88  AGEOWAAI-INSULAR         VALUE 'I'.              
               05  AGEOWAAI-CIRECP-SAIDA   PIC  9(005) VALUE  ZEROS.    
               05  AGEOWAAI-IDIOMA-SAIDA   PIC  9(003) VALUE  ZEROS.    
               05  AGEOWAAI-IND-CLAS-LIST  PIC  X(001) VALUE  SPACES.   
                   88  AGEOWAAI-NOME            VALUE 'N'.              
                   88  AGEOWAAI-CODIGO          VALUE 'C'.              
                   88  AGEOWAAI-SIGLA           VALUE 'S'.              
      *================================================================*
      ***  DADOS DE ENTRADA/SAIDA - CONTROLE PAGINACAO DA LISTA      ***
      *================================================================*
           03  AGEOWAAI-CTRL-PAG.                                       
               05  AGEOWAAI-QTDE-SOLIC     PIC  9(002) VALUE  ZEROS.    
               05  AGEOWAAI-IND-PAGINA     PIC  X(001) VALUE  SPACES.   
                   88  AGEOWAAI-INICIAL         VALUE 'I'.              
                   88  AGEOWAAI-ANTERIOR        VALUE 'A'.              
                   88  AGEOWAAI-POSTERIOR       VALUE 'P'.              
                   88  AGEOWAAI-ULTIMA          VALUE 'U'.              
                   88  AGEOWAAI-FIM-LISTA       VALUE 'S'.              
               05  AGEOWAAI-CTRL-LISTA     PIC  X(050) VALUE  SPACES.   
      *================================================================*
      ***    DADOS DE SAIDA PARA FUNCIONALIDADE SF1007               ***
      *================================================================*
           03  AGEOWAAI-SAIDA.                                          
               05  AGEOWAAI-QTDE-OCORR     PIC  9(002) VALUE  ZEROS.    
               05  AGEOWAAI-TABELA   OCCURS   50   TIMES.               
                   07  AGEOWAAI-CD-CNTTE       PIC  X(010) VALUE SPACES.
                   07  AGEOWAAI-NOME-CNTTE     PIC  X(030) VALUE SPACES.
                   07  AGEOWAAI-CD-PAIS        PIC  X(010) VALUE SPACES.
                   07  AGEOWAAI-NOME-PAIS      PIC  X(060) VALUE SPACES.
                   07  AGEOWAAI-FMT-PDR-DATA   PIC  9(003) VALUE  ZEROS.
                   07  AGEOWAAI-FMT-PDR-HORA   PIC  9(003) VALUE  ZEROS.
                   07  AGEOWAAI-TIPO-CALEN     PIC  9(003) VALUE  ZEROS.
                   07  AGEOWAAI-SGL-PAIS-ISO   PIC  X(002) VALUE SPACES.
                   07  AGEOWAAI-SGL-PAIS       PIC  X(005) VALUE SPACES.
                   07  AGEOWAAI-NAC-MASC       PIC  X(020) VALUE SPACES.
                   07  AGEOWAAI-NAC-FEMI       PIC  X(020) VALUE SPACES.
                   07  AGEOWAAI-MUN-HORA-OFIC  PIC  X(040) VALUE SPACES.
                   07  AGEOWAAI-MUN-LAT-LONG   PIC  X(040) VALUE SPACES.
                   07  AGEOWAAI-IND-INSULAR    PIC  X(001) VALUE SPACES.
                   07  AGEOWAAI-SIGLA-PARTCP   PIC  X(010) VALUE SPACES.
                   07  AGEOWAAI-NPAIS-PARTCP   PIC  X(060) VALUE SPACES.
      *                                                                 
      *================================================================*

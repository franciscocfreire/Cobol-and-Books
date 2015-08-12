      *================================================================*
      * NOME BOOK  : SCSBW314                                          *
      * DESCRICAO  : CONSULTAR NO HISTORICO RAS ANTERIOR               *
      * COMUNICACAO: FRAMEWORK X COORDENADOR                           *
      * DATA       : 05/02/2015                                        *
      * AUTOR      : FERNANDA CARUSO                                   *
      * EMPRESA    : BRQ IT SERVICES                                   *
      * GRUPO      : 17 - CONTABILIDADE E IMPOSTOS                     *
      * COMPONENTE : SCSB - SISCOSERV BRADESCO                         *
      *================================================================*
      *                                                                *
      * SCSBW314-HEADER.                                               *
      *   SCSBW314-COD-LAYOUT            = CODIGO DO LAYOUT            *
      *   SCSBW314-TAM-LAYOUT            = TAMANHO DO LAYOUT           *
      * SCSBW314-REGISTRO.                                             *
      *   SCSBW314-BLOCO-ENTRADA.                                      *
      *     SCSBW314-E-ANO               = ANO RAS                     *
      *     SCSBW314-E-NREG              = NUMERO DO REGISTRO RAS      *
      *   SCSBW314-BLOCO-SAIDA.                                        *
      *     SCSBW314-S-CNPJ-PRINC        = CODIGO DO CNPJ (CORPO)      *
      *     SCSBW314-S-CNPJ-FLIAL        = CODIGO DA FILIAL DO CNPJ    *
      *     SCSBW314-S-CNPJ-CTRL         = CODIGO DO CONTROLE DO CNPJ  *
      *     SCSBW314-S-DS-EMPR           = DESCRICAO DA EMPRESA        *
      *     SCSBW314-S-CD-DEPEND-SAP     = CODIGO DA DEPENDENCIA SAP   *
      *     SCSBW314-S-DS-DEPEND         = DESCRICAO DA DEPENDENCIA    *
      *     SCSBW314-S-NOME-VEND         = NOME DO VENDEDOR            *
      *     SCSBW314-S-END-VEND          = ENDERECO DO VENDEDOR        *
      *     SCSBW314-S-CD-PAIS-VEND      = CODIGO DO PAIS VENDEDOR     *
      *     SCSBW314-S-DS-PAIS-VEND      = DESCRICAO PAIS VENDEDOR     *
      *     SCSBW314-S-NIF               = NUMERO IDENTIFICACAO FISCAL *
      *     SCSBW314-S-CD-MOEDA          = CODIGO DA MOEDA             *
      *     SCSBW314-S-DS-MOEDA          = DESCRICAO DA MOEDA          *
      *     SCSBW314-S-VLR-CONV-REAL     = VALOR TOTAL CONVERTIDO REAL *
      *     SCSBW314-S-DS-SIT-REG        = DESCRICAO SITUACAO REGISTRO *
      *     SCSBW314-S-ORIG-REG          = ORIGEM DO REGISTRO          *
      *     SCSBW314-S-DT-INCLUSAO       = DATA DA INCLUSAO            *
      *     SCSBW314-S-DT-VALIDACAO      = DATA DA VALIDACAO           *
      *     SCSBW314-S-CD-USUARIO        = CODIGO DO USUARIO           *
      *     SCSBW314-S-NOME-USUARIO      = NOME DO USUARIO             *
      *     SCSBW314-S-DT-GERACAO-ARQ    = DATA DE GERACAO DO ARQUIVO  *
      *     SCSBW314-S-DT-TRANSMIS-ARQ   = DATA TRANSMISSAO DO ARQUIVO *
      *     SCSBW314-S-NUM-PROTOCOLO     = NUMERO DO PROTOCOLO         *
      *     SCSBW314-S-INF-COMPLEM       = INFORMACOES COMPLEMENTARES  *
      *     SCSBW314-S-HINCL-HIST        = TIMESTAMP INCLUSAO HISTORICO*
      *     SCSBW314-S-QTD-REG           = QTDE REGISTROS DEVOLVIDOS   *
      *     SCSBW314-S-LISTA.                                          *
      *       SCSBW314-S-COPER           = NUMERO DA OPERACAO          *
      *       SCSBW314-S-CD-NBS          = CODIGO NBS                  *
      *       SCSBW314-S-DS-NBS          = DESCRICAO NBS               *
      *       SCSBW314-S-CD-PAIS-DESTNO  = CODIGO PAIS DE DESTINO      *
      *       SCSBW314-S-DS-PAIS-DESTNO  = DESCRICAO PAIS DE DESTINO   *
      *       SCSBW314-S-CD-MODO-PREST   = CODIGO MODO DE PRESTACAO    *
      *       SCSBW314-S-DS-MODO-PREST   = DESCRICAO MODO DE PRESTACAO *
      *       SCSBW314-S-DT-INICIO       = DATA DE INICIO              *
      *       SCSBW314-S-DT-CONCLUSAO    = DATA DE CONCLUSAO           *
      *       SCSBW314-S-VALOR           = VALOR DA OPERACAO           *
      *       SCSBW314-S-VALOR-FATURADO  = VALOR FATURADO              *
      *       SCSBW314-S-IND-ADITIVO     = INDICADOR ADITIVO (S OU N)  *
      *       SCSBW314-S-SIT-REG         = SITUACAO DO REGISTRO        *
      *       SCSBW314-S-DS-SIT-OPER     = DESCRICAO SITUACAO OPERACAO *
      *       SCSBW314-S-TPO-REG         = TIPO DO REGISTRO            *
      *                                    1 = INCLUSAO                *
      *                                    2 = INCLUSAO DE ADITIVO     *
      *                                    3 = RETIFICACAO             *
      *                                    4 = RETIFICACAO DE ADITIVO  *
      *                                    5 = CANCELAMENTO            *
      *                                    6 = EXCLUSAO                *
      *----------------------------------------------------------------*
      * DATA       AUTOR             ALTERACAO                         *
      * ---------- ----------------- --------------------------------- *
      * 99/99/9999                   XXXXXXXXXXXXXXXXXXXXXXX           *
      *================================================================*
                                                                        
          05 SCSBW314-HEADER.                                           
             10 SCSBW314-COD-LAYOUT         PIC X(008) VALUE 'SCSBW314'.
             10 SCSBW314-TAM-LAYOUT         PIC 9(005) VALUE 20489.     
          05 SCSBW314-REGISTRO.                                         
             10 SCSBW314-BLOCO-ENTRADA.                                 
                15 SCSBW314-E-ANO                PIC 9(004).            
                15 SCSBW314-E-NREG               PIC 9(009).            
             10 SCSBW314-BLOCO-SAIDA.                                   
                15 SCSBW314-S-CNPJ-PRINC         PIC 9(009).            
                15 SCSBW314-S-CNPJ-FLIAL         PIC 9(004).            
                15 SCSBW314-S-CNPJ-CTRL          PIC 9(002).            
                15 SCSBW314-S-DS-EMPR            PIC X(070).            
                15 SCSBW314-S-CD-DEPEND-SAP      PIC 9(005).            
                15 SCSBW314-S-DS-DEPEND          PIC X(060).            
                15 SCSBW314-S-NOME-VEND          PIC X(150).            
                15 SCSBW314-S-END-VEND           PIC X(150).            
                15 SCSBW314-S-CD-PAIS-VEND       PIC 9(003).            
                15 SCSBW314-S-DS-PAIS-VEND       PIC X(060).            
                15 SCSBW314-S-NIF                PIC X(040).            
                15 SCSBW314-S-CD-MOEDA           PIC 9(005).            
                15 SCSBW314-S-DS-MOEDA           PIC X(040).            
                15 SCSBW314-S-VLR-CONV-REAL      PIC 9(013)V99.         
                15 SCSBW314-S-DS-SIT-REG         PIC X(050).            
                15 SCSBW314-S-ORIG-REG           PIC X(050).            
                15 SCSBW314-S-DT-INCLUSAO        PIC X(010).            
                15 SCSBW314-S-DT-VALIDACAO       PIC X(010).            
                15 SCSBW314-S-CD-USUARIO         PIC X(009).            
                15 SCSBW314-S-NOME-USUARIO       PIC X(080).            
                15 SCSBW314-S-DT-GERACAO-ARQ     PIC X(010).            
                15 SCSBW314-S-DT-TRANSMIS-ARQ    PIC X(010).            
                15 SCSBW314-S-NUM-PROTOCOLO      PIC X(012).            
                15 SCSBW314-S-INF-COMPLEM        PIC X(5000).           
                15 SCSBW314-S-HINCL-HIST         PIC X(026).            
                15 SCSBW314-S-QTD-REG            PIC 9(003).            
                15 SCSBW314-S-LISTA              OCCURS 0 TO 030 TIMES  
                      DEPENDING ON SCSBW314-S-QTD-REG.                  
                   20 SCSBW314-S-COPER           PIC 9(010).            
                   20 SCSBW314-S-CD-NBS          PIC X(009).            
                   20 SCSBW314-S-DS-NBS          PIC X(250).            
                   20 SCSBW314-S-CD-PAIS-DESTNO  PIC 9(003).            
                   20 SCSBW314-S-DS-PAIS-DESTNO  PIC X(060).            
                   20 SCSBW314-S-CD-MODO-PREST   PIC 9(001).            
                   20 SCSBW314-S-DS-MODO-PREST   PIC X(050).            
                   20 SCSBW314-S-DT-INICIO       PIC X(010).            
                   20 SCSBW314-S-DT-CONCLUSAO    PIC X(010).            
                   20 SCSBW314-S-VALOR           PIC 9(013)V99.         
                   20 SCSBW314-S-VALOR-FATURADO  PIC 9(013)V99.         
                   20 SCSBW314-S-IND-ADITIVO     PIC X(001).            
                   20 SCSBW314-S-SIT-REG         PIC X(001).            
                   20 SCSBW314-S-DS-SIT-OPER     PIC X(050).            
                   20 SCSBW314-S-TPO-REG         PIC 9(001).            

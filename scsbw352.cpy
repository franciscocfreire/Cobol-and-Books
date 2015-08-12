      *================================================================*
      * NOME BOOK  : SCSBW352                                          *
      * DESCRICAO  : CONSULTA PARA ALTERAR ADITIVO/RETIFICACAO ADITIVO *
      * COMUNICACAO: FRAMEWORK X COORDENADOR                           *
      * DATA       : 10/02/2015                                        *
      * AUTOR      : FERNANDA CARUSO                                   *
      * EMPRESA    : BRQ IT SERVICES                                   *
      * GRUPO      : 17 - CONTABILIDADE E IMPOSTOS                     *
      * COMPONENTE : SCSB - SISCOSERV BRADESCO                         *
      *================================================================*
      *                                                                *
      * SCSBW352-HEADER.                                               *
      *   SCSBW352-COD-LAYOUT            = CODIGO DO LAYOUT            *
      *   SCSBW352-TAM-LAYOUT            = TAMANHO DO LAYOUT           *
      * SCSBW352-REGISTRO.                                             *
      *   SCSBW352-BLOCO-ENTRADA.                                      *
      *     SCSBW352-E-ANO               = ANO RAS                     *
      *     SCSBW352-E-NREG              = NUMERO DO REGISTRO RAS      *
      *     SCSBW352-E-COPER-ADITIVO     = NUMERO OPERACAO DE ADITIVO  *
      *     SCSBW352-E-TPO-REG           = TIPO DE REGISTRO            *
      *                                    2 - INCLUSAO DE ADITIVO     *
      *                                    4 - RETIFICACAO DE ADITIVO  *
      *   SCSBW352-BLOCO-SAIDA.                                        *
      *     SCSBW352-S-CNPJ-PRINC        = CODIGO DO CNPJ (CORPO)      *
      *     SCSBW352-S-CNPJ-FLIAL        = CODIGO DA FILIAL DO CNPJ    *
      *     SCSBW352-S-CNPJ-CTRL         = CODIGO DO CONTROLE DO CNPJ  *
      *     SCSBW352-S-DS-EMPR           = DESCRICAO DA EMPRESA        *
      *     SCSBW352-S-CD-DEPEND-SAP     = CODIGO DA DEPENDENCIA SAP   *
      *     SCSBW352-S-DS-DEPEND         = DESCRICAO DA DEPENDENCIA    *
      *     SCSBW352-S-NOME-VEND         = NOME DO VENDEDOR            *
      *     SCSBW352-S-END-VEND          = ENDERECO DO VENDEDOR        *
      *     SCSBW352-S-CD-PAIS-VEND      = CODIGO DO PAIS VENDEDOR     *
      *     SCSBW352-S-DS-PAIS-VEND      = DESCRICAO PAIS VENDEDOR     *
      *     SCSBW352-S-NIF               = NUMERO IDENTIFICACAO FISCAL *
      *     SCSBW352-S-CD-MOEDA          = CODIGO DA MOEDA             *
      *     SCSBW352-S-DS-MOEDA          = DESCRICAO DA MOEDA          *
      *     SCSBW352-S-VLR-CONV-REAL     = VALOR TOTAL CONVERTIDO REAL *
      *     SCSBW352-S-SIT-REG           = SITUACAO DO REGISTRO        *
      *     SCSBW352-S-DS-SIT-REG        = DESCRICAO SITUACAO REGISTRO *
      *     SCSBW352-S-ORIG-REG          = ORIGEM DO REGISTRO          *
      *     SCSBW352-S-DT-INCLUSAO       = DATA DA INCLUSAO            *
      *     SCSBW352-S-DT-VALIDACAO      = DATA DA VALIDACAO           *
      *     SCSBW352-S-CD-USUARIO        = CODIGO DO USUARIO           *
      *     SCSBW352-S-NOME-USUARIO      = NOME DO USUARIO             *
      *     SCSBW352-S-DT-GERACAO-ARQ    = DATA DE GERACAO DO ARQUIVO  *
      *     SCSBW352-S-DT-TRANSMIS-ARQ   = DATA TRANSMISSAO DO ARQUIVO  
      *     SCSBW352-S-NUM-PROTOCOLO     = NUMERO DO PROTOCOLO          
      *     SCSBW352-S-INF-COMPLEM       = INFORMACOES COMPLEMENTARES  *
      *     SCSBW352-S-QTD-REG           = QTDE REGISTROS DEVOLVIDOS   *
      *     SCSBW352-S-LISTA.                                          *
      *       SCSBW352-S-COPER           = NUMERO DA OPERACAO          *
      *       SCSBW352-S-CD-NBS          = CODIGO NBS                  *
      *       SCSBW352-S-DS-NBS          = DESCRICAO NBS               *
      *       SCSBW352-S-CD-PAIS-DESTNO  = CODIGO PAIS DE DESTINO      *
      *       SCSBW352-S-DS-PAIS-DESTNO  = DESCRICAO PAIS DE DESTINO   *
      *       SCSBW352-S-CD-MODO-PREST   = CODIGO MODO DE PRESTACAO    *
      *       SCSBW352-S-DS-MODO-PREST   = DESCRICAO MODO DE PRESTACAO *
      *       SCSBW352-S-DT-INICIO       = DATA DE INICIO              *
      *       SCSBW352-S-DT-CONCLUSAO    = DATA DE CONCLUSAO           *
      *       SCSBW352-S-VALOR           = VALOR DA OPERACAO           *
      *       SCSBW352-S-VALOR-PAGO      = VALOR PAGO                  *
      *       SCSBW352-S-IND-ADITIVO     = INDICADOR ADITIVO (S OU N)  *
      *       SCSBW352-S-SIT-OPER        = SITUACAO DA OPERACAO        *
      *       SCSBW352-S-DS-SIT-OPER     = DESCRICAO SITUACAO OPERACAO *
      *       SCSBW352-S-COMBO-NBS       = A (ATIVO) OU I (INATIVO)    *
      *                                                                *
      *----------------------------------------------------------------*
      * DATA       AUTOR             ALTERACAO                         *
      * ---------- ----------------- --------------------------------- *
      * 99/99/9999                   XXXXXXXXXXXXXXXXXXXXXXX           *
      *================================================================*
                                                                        
          05 SCSBW352-HEADER.                                           
             10 SCSBW352-COD-LAYOUT         PIC X(008) VALUE 'SCSBW352'.
             10 SCSBW352-TAM-LAYOUT         PIC 9(005) VALUE 20475.     
          05 SCSBW352-REGISTRO.                                         
             10 SCSBW352-BLOCO-ENTRADA.                                 
                15 SCSBW352-E-ANO                PIC 9(004).            
                15 SCSBW352-E-NREG               PIC 9(009).            
                15 SCSBW352-E-COPER-ADITIVO      PIC 9(010).            
                15 SCSBW352-E-TPO-REG            PIC 9(001).            
             10 SCSBW352-BLOCO-SAIDA.                                   
                15 SCSBW352-S-CNPJ-PRINC         PIC 9(009).            
                15 SCSBW352-S-CNPJ-FLIAL         PIC 9(004).            
                15 SCSBW352-S-CNPJ-CTRL          PIC 9(002).            
                15 SCSBW352-S-DS-EMPR            PIC X(070).            
                15 SCSBW352-S-CD-DEPEND-SAP      PIC 9(005).            
                15 SCSBW352-S-DS-DEPEND          PIC X(060).            
                15 SCSBW352-S-NOME-VEND          PIC X(150).            
                15 SCSBW352-S-END-VEND           PIC X(150).            
                15 SCSBW352-S-CD-PAIS-VEND       PIC 9(003).            
                15 SCSBW352-S-DS-PAIS-VEND       PIC X(060).            
                15 SCSBW352-S-NIF                PIC X(040).            
                15 SCSBW352-S-CD-MOEDA           PIC 9(005).            
                15 SCSBW352-S-DS-MOEDA           PIC X(040).            
                15 SCSBW352-S-VLR-CONV-REAL      PIC 9(013)V99.         
                15 SCSBW352-S-SIT-REG            PIC X(001).            
                15 SCSBW352-S-DS-SIT-REG         PIC X(050).            
                15 SCSBW352-S-ORIG-REG           PIC X(050).            
                15 SCSBW352-S-DT-INCLUSAO        PIC X(010).            
                15 SCSBW352-S-DT-VALIDACAO       PIC X(010).            
                15 SCSBW352-S-CD-USUARIO         PIC X(009).            
                15 SCSBW352-S-NOME-USUARIO       PIC X(080).            
                15 SCSBW352-S-DT-GERACAO-ARQ     PIC X(010).            
                15 SCSBW352-S-DT-TRANSMIS-ARQ    PIC X(010).            
                15 SCSBW352-S-NUM-PROTOCOLO      PIC X(012).            
                15 SCSBW352-S-INF-COMPLEM        PIC X(5000).           
                15 SCSBW352-S-QTD-REG            PIC 9(003).            
                15 SCSBW352-S-LISTA              OCCURS 0 TO 030 TIMES  
                      DEPENDING ON SCSBW352-S-QTD-REG.                  
                   20 SCSBW352-S-COPER           PIC 9(010).            
                   20 SCSBW352-S-CD-NBS          PIC X(009).            
                   20 SCSBW352-S-DS-NBS          PIC X(250).            
                   20 SCSBW352-S-CD-PAIS-DESTNO  PIC 9(003).            
                   20 SCSBW352-S-DS-PAIS-DESTNO  PIC X(060).            
                   20 SCSBW352-S-CD-MODO-PREST   PIC 9(001).            
                   20 SCSBW352-S-DS-MODO-PREST   PIC X(050).            
                   20 SCSBW352-S-DT-INICIO       PIC X(010).            
                   20 SCSBW352-S-DT-CONCLUSAO    PIC X(010).            
                   20 SCSBW352-S-VALOR           PIC 9(013)V99.         
                   20 SCSBW352-S-VALOR-PAGO      PIC 9(013)V99.         
                   20 SCSBW352-S-IND-ADITIVO     PIC X(001).            
                   20 SCSBW352-S-SIT-OPER        PIC X(001).            
                   20 SCSBW352-S-DS-SIT-OPER     PIC X(050).            
                   20 SCSBW352-S-COMBO-NBS       PIC X(001).            

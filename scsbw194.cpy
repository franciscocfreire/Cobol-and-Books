      *================================================================*
      * NOME BOOK  : SCSBW194                                          *
      * DESCRICAO  : CONSULTA PARA ALTERAR ADITIVO/RETIFICACAO ADITIVO *
      * COMUNICACAO: FRAMEWORK X COORDENADOR                           *
      * DATA       : 28/01/2015                                        *
      * AUTOR      : FERNANDA CARUSO                                   *
      * EMPRESA    : BRQ IT SERVICES                                   *
      * GRUPO      : 17 - CONTABILIDADE E IMPOSTOS                     *
      * COMPONENTE : SCSB - SISCOSERV BRADESCO                         *
      *================================================================*
      *                                                                *
      * SCSBW194-HEADER.                                               *
      *   SCSBW194-COD-LAYOUT            = CODIGO DO LAYOUT            *
      *   SCSBW194-TAM-LAYOUT            = TAMANHO DO LAYOUT           *
      * SCSBW194-REGISTRO.                                             *
      *   SCSBW194-BLOCO-ENTRADA.                                      *
      *     SCSBW194-E-ANO               = ANO RVS                     *
      *     SCSBW194-E-NREG              = NUMERO DO REGISTRO RVS      *
      *     SCSBW194-E-COPER-ADITIVO     = NUMERO OPERACAO DE ADITIVO  *
      *     SCSBW194-E-TPO-REG           = TIPO DE REGISTRO            *
      *                                    2 - INCLUSAO DE ADITIVO     *
      *                                    4 - RETIFICACAO DE ADITIVO  *
      *   SCSBW194-BLOCO-SAIDA.                                        *
      *     SCSBW194-S-CNPJ-PRINC        = CODIGO DO CNPJ (CORPO)      *
      *     SCSBW194-S-CNPJ-FLIAL        = CODIGO DA FILIAL DO CNPJ    *
      *     SCSBW194-S-CNPJ-CTRL         = CODIGO DO CONTROLE DO CNPJ  *
      *     SCSBW194-S-DS-EMPR           = DESCRICAO DA EMPRESA        *
      *     SCSBW194-S-CD-DEPEND-SAP     = CODIGO DA DEPENDENCIA SAP   *
      *     SCSBW194-S-DS-DEPEND         = DESCRICAO DA DEPENDENCIA    *
      *     SCSBW194-S-NOME-ADQUIR       = NOME DO ADQUIRENTE          *
      *     SCSBW194-S-END-ADQUIR        = ENDERECO DO ADQUIRENTE      *
      *     SCSBW194-S-CD-PAIS-ADQUIR    = CODIGO DO PAIS ADQUIRENTE   *
      *     SCSBW194-S-DS-PAIS-ADQUIR    = DESCRICAO PAIS ADQUIRENTE   *
      *     SCSBW194-S-NIF               = NUMERO IDENTIFICACAO FISCAL *
      *     SCSBW194-S-CD-MOEDA          = CODIGO DA MOEDA             *
      *     SCSBW194-S-DS-MOEDA          = DESCRICAO DA MOEDA          *
      *     SCSBW194-S-VLR-CONV-REAL     = VALOR TOTAL CONVERTIDO REAL *
      *     SCSBW194-S-SIT-REG           = SITUACAO DO REGISTRO        *
      *     SCSBW194-S-DS-SIT-REG        = DESCRICAO SITUACAO REGISTRO *
      *     SCSBW194-S-ORIG-REG          = ORIGEM DO REGISTRO          *
      *     SCSBW194-S-DT-INCLUSAO       = DATA DA INCLUSAO            *
      *     SCSBW194-S-DT-VALIDACAO      = DATA DA VALIDACAO           *
      *     SCSBW194-S-CD-USUARIO        = CODIGO DO USUARIO           *
      *     SCSBW194-S-NOME-USUARIO      = NOME DO USUARIO             *
      *     SCSBW194-S-DT-GERACAO-ARQ    = DATA DE GERACAO DO ARQUIVO  *
      *     SCSBW194-S-DT-TRANSMIS-ARQ   = DATA TRANSMISSAO DO ARQUIVO  
      *     SCSBW194-S-NUM-PROTOCOLO     = NUMERO DO PROTOCOLO          
      *     SCSBW194-S-INF-COMPLEM       = INFORMACOES COMPLEMENTARES  *
      *     SCSBW194-S-QTD-REG           = QTDE REGISTROS DEVOLVIDOS   *
      *     SCSBW194-S-LISTA.                                          *
      *       SCSBW194-S-COPER           = NUMERO DA OPERACAO          *
      *       SCSBW194-S-CD-NBS          = CODIGO NBS                  *
      *       SCSBW194-S-DS-NBS          = DESCRICAO NBS               *
      *       SCSBW194-S-CD-PAIS-DESTNO  = CODIGO PAIS DE DESTINO      *
      *       SCSBW194-S-DS-PAIS-DESTNO  = DESCRICAO PAIS DE DESTINO   *
      *       SCSBW194-S-CD-MODO-PREST   = CODIGO MODO DE PRESTACAO    *
      *       SCSBW194-S-DS-MODO-PREST   = DESCRICAO MODO DE PRESTACAO *
      *       SCSBW194-S-DT-INICIO       = DATA DE INICIO              *
      *       SCSBW194-S-DT-CONCLUSAO    = DATA DE CONCLUSAO           *
      *       SCSBW194-S-VALOR           = VALOR DA OPERACAO           *
      *       SCSBW194-S-VALOR-FATURADO  = VALOR FATURADO              *
      *       SCSBW194-S-IND-ADITIVO     = INDICADOR ADITIVO (S OU N)  *
      *       SCSBW194-S-SIT-OPER        = SITUACAO DA OPERACAO        *
      *       SCSBW194-S-DS-SIT-OPER     = DESCRICAO SITUACAO OPERACAO *
      *       SCSBW194-S-COMBO-NBS       = A (ATIVO) OU I (INATIVO)    *
      *                                                                *
      *----------------------------------------------------------------*
      * DATA       AUTOR             ALTERACAO                         *
      * ---------- ----------------- --------------------------------- *
      * 99/99/9999                   XXXXXXXXXXXXXXXXXXXXXXX           *
      *================================================================*
                                                                        
          05 SCSBW194-HEADER.                                           
             10 SCSBW194-COD-LAYOUT         PIC X(008) VALUE 'SCSBW194'.
             10 SCSBW194-TAM-LAYOUT         PIC 9(005) VALUE 20475.     
          05 SCSBW194-REGISTRO.                                         
             10 SCSBW194-BLOCO-ENTRADA.                                 
                15 SCSBW194-E-ANO                PIC 9(004).            
                15 SCSBW194-E-NREG               PIC 9(009).            
                15 SCSBW194-E-COPER-ADITIVO      PIC 9(010).            
                15 SCSBW194-E-TPO-REG            PIC 9(001).            
             10 SCSBW194-BLOCO-SAIDA.                                   
                15 SCSBW194-S-CNPJ-PRINC         PIC 9(009).            
                15 SCSBW194-S-CNPJ-FLIAL         PIC 9(004).            
                15 SCSBW194-S-CNPJ-CTRL          PIC 9(002).            
                15 SCSBW194-S-DS-EMPR            PIC X(070).            
                15 SCSBW194-S-CD-DEPEND-SAP      PIC 9(005).            
                15 SCSBW194-S-DS-DEPEND          PIC X(060).            
                15 SCSBW194-S-NOME-ADQUIR        PIC X(150).            
                15 SCSBW194-S-END-ADQUIR         PIC X(150).            
                15 SCSBW194-S-CD-PAIS-ADQUIR     PIC 9(003).            
                15 SCSBW194-S-DS-PAIS-ADQUIR     PIC X(060).            
                15 SCSBW194-S-NIF                PIC X(040).            
                15 SCSBW194-S-CD-MOEDA           PIC 9(005).            
                15 SCSBW194-S-DS-MOEDA           PIC X(040).            
                15 SCSBW194-S-VLR-CONV-REAL      PIC 9(013)V99.         
                15 SCSBW194-S-SIT-REG            PIC X(001).            
                15 SCSBW194-S-DS-SIT-REG         PIC X(050).            
                15 SCSBW194-S-ORIG-REG           PIC X(050).            
                15 SCSBW194-S-DT-INCLUSAO        PIC X(010).            
                15 SCSBW194-S-DT-VALIDACAO       PIC X(010).            
                15 SCSBW194-S-CD-USUARIO         PIC X(009).            
                15 SCSBW194-S-NOME-USUARIO       PIC X(080).            
                15 SCSBW194-S-DT-GERACAO-ARQ     PIC X(010).            
                15 SCSBW194-S-DT-TRANSMIS-ARQ    PIC X(010).            
                15 SCSBW194-S-NUM-PROTOCOLO      PIC X(012).            
                15 SCSBW194-S-INF-COMPLEM        PIC X(5000).           
                15 SCSBW194-S-QTD-REG            PIC 9(003).            
                15 SCSBW194-S-LISTA              OCCURS 0 TO 030 TIMES  
                      DEPENDING ON SCSBW194-S-QTD-REG.                  
                   20 SCSBW194-S-COPER           PIC 9(010).            
                   20 SCSBW194-S-CD-NBS          PIC X(009).            
                   20 SCSBW194-S-DS-NBS          PIC X(250).            
                   20 SCSBW194-S-CD-PAIS-DESTNO  PIC 9(003).            
                   20 SCSBW194-S-DS-PAIS-DESTNO  PIC X(060).            
                   20 SCSBW194-S-CD-MODO-PREST   PIC 9(001).            
                   20 SCSBW194-S-DS-MODO-PREST   PIC X(050).            
                   20 SCSBW194-S-DT-INICIO       PIC X(010).            
                   20 SCSBW194-S-DT-CONCLUSAO    PIC X(010).            
                   20 SCSBW194-S-VALOR           PIC 9(013)V99.         
                   20 SCSBW194-S-VALOR-FATURADO  PIC 9(013)V99.         
                   20 SCSBW194-S-IND-ADITIVO     PIC X(001).            
                   20 SCSBW194-S-SIT-OPER        PIC X(001).            
                   20 SCSBW194-S-DS-SIT-OPER     PIC X(050).            
                   20 SCSBW194-S-COMBO-NBS       PIC X(001).            

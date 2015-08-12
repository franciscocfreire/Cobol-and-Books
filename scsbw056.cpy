      *================================================================*
      * NOME BOOK  : SCSBW056                                          *
      * DESCRICAO  : CONSULTAR RAS OU ADITIVO                          *
      * COMUNICACAO: FRAMEWORK X COORDENADOR                           *
      * DATA       : 02/03/2013                                        *
      * AUTOR      : FERNANDA CARUSO                                   *
      * EMPRESA    : BRQ IT SERVICES                                   *
      * GRUPO      : 17 - CONTABILIDADE E IMPOSTOS                     *
      * COMPONENTE : SCSB - SISCOSERV BRADESCO                         *
      *================================================================*
      *                                                                *
      * SCSBW056-HEADER.                                               *
      *   SCSBW056-COD-LAYOUT            = CODIGO DO LAYOUT            *
      *   SCSBW056-TAM-LAYOUT            = TAMANHO DO LAYOUT           *
      * SCSBW056-REGISTRO.                                             *
      *   SCSBW056-BLOCO-ENTRADA.                                      *
      *     SCSBW056-E-ANO               = ANO RAS                     *
      *     SCSBW056-E-NREG              = NUMERO DO REGISTRO RAS      *
      *     SCSBW056-E-COPER-ADITIVO     = NUMERO OPERACAO DE ADITIVO  *
      *   SCSBW056-BLOCO-SAIDA.                                        *
      *     SCSBW056-S-CPF-CNPJ-PRINC    = CPF/CNPJ PRINCIPAL          *
      *     SCSBW056-S-CPF-CNPJ-FLIAL    = CPF/CNPJ FILIAL             *
      *     SCSBW056-S-CPF-CNPJ-CTRL     = CPF/CNPJ CONTROLE           *
      *     SCSBW056-S-DS-EMPR           = DESCRICAO DA EMPRESA        *
      *     SCSBW056-S-CD-DEPEND-SAP     = CODIGO DA DEPENDENCIA SAP   *
      *     SCSBW056-S-DS-DEPEND         = DESCRICAO DA DEPENDENCIA    *
      *     SCSBW056-S-NOME-VEND         = NOME DO VENDEDOR            *
      *     SCSBW056-S-END-VEND          = ENDERECO DO VENDEDOR        *
      *     SCSBW056-S-CD-PAIS-VEND      = CODIGO DO PAIS DO VENDEDOR  *
      *     SCSBW056-S-DS-PAIS-VEND      = DESCRICAO PAIS DO VENDEDOR  *
      *     SCSBW056-S-NIF               = NUMERO IDENTIFICACAO FISCAL *
      *     SCSBW056-S-CD-MOEDA          = CODIGO DA MOEDA             *
      *     SCSBW056-S-DS-MOEDA          = DESCRICAO DA MOEDA          *
      *     SCSBW056-S-VLR-CONV-REAL     = VALOR TOTAL CONVERTIDO REAL *
      *     SCSBW056-S-SIT-REG           = SITUACAO DO REGISTRO        *
      *     SCSBW056-S-DS-SIT-REG        = DESCRICAO SITUACAO REGISTRO *
      *     SCSBW056-S-ORIG-REG          = ORIGEM DO REGISTRO          *
      *     SCSBW056-S-DT-INCLUSAO       = DATA DA INCLUSAO            *
      *     SCSBW056-S-DT-VALIDACAO      = DATA DA VALIDACAO           *
      *     SCSBW056-S-CD-USUARIO        = CODIGO DO USUARIO           *
      *     SCSBW056-S-NOME-USUARIO      = NOME DO USUARIO             *
      *     SCSBW056-S-DT-GERACAO-ARQ    = DATA DE GERACAO DO ARQUIVO  *
      *     SCSBW056-S-DT-TRANSMIS-ARQ   = DATA TRANSMISSAO DO ARQUIVO *
      *     SCSBW056-S-NUM-PROTOCOLO     = NUMERO DO PROTOCOLO         *
      *     SCSBW056-S-INF-COMPLEM       = INFORMACOES COMPLEMENTARES  *
      *     SCSBW056-S-QTD-REG           = QTDE REGISTROS DEVOLVIDOS   *
      *     SCSBW056-S-LISTA.                                          *
      *       SCSBW056-S-COPER           = NUMERO DA OPERACAO          *
      *       SCSBW056-S-CD-NBS          = CODIGO NBS                  *
      *       SCSBW056-S-DS-NBS          = DESCRICAO NBS               *
      *       SCSBW056-S-CD-PAIS-DESTNO  = CODIGO PAIS DE DESTINO      *
      *       SCSBW056-S-DS-PAIS-DESTNO  = DESCRICAO PAIS DE DESTINO   *
      *       SCSBW056-S-CD-MODO-PREST   = CODIGO MODO DE PRESTACAO    *
      *       SCSBW056-S-DS-MODO-PREST   = DESCRICAO MODO DE PRESTACAO *
      *       SCSBW056-S-DT-INICIO       = DATA DE INICIO              *
      *       SCSBW056-S-DT-CONCLUSAO    = DATA DE CONCLUSAO           *
      *       SCSBW056-S-VALOR           = VALOR DA OPERACAO           *
      *       SCSBW056-S-VALOR-PAGO      = VALOR PAGO                  *
      *       SCSBW056-S-IND-ADITIVO     = INDICADOR ADITIVO (S OU N)  *
      *       SCSBW056-S-SIT-OPER        = SITUACAO DA OPERACAO        *
      *       SCSBW056-S-DS-SIT-OPER     = DESCRICAO SITUACAO OPERACAO *
      *       SCSBW056-S-TPO-REG         = TIPO DO REGISTRO            *
      *                                    1 = INCLUSAO                *
      *                                    2 = INCLUSAO DE ADITIVO     *
      *                                    3 = RETIFICACAO             *
      *                                    4 = RETIFICACAO DE ADITIVO  *
      *                                    5 = CANCELAMENTO            *
      *                                    6 = EXCLUSAO                *
      *                                                                *
      *----------------------------------------------------------------*
      * DATA       AUTOR             ALTERACAO                         *
      * ---------- ----------------- --------------------------------- *
      * 04/02/2015 FERNANDA CARUSO   INCLUSAO DOS CAMPOS SCSBW056-S-   *
      *                              DS-EMPR/CD-DEPEND-SAP/DS-DEPEND   *
      *                              VLR-CONV-REAL/SIT-REG/DS-SIT-REG  *
      *                              ORIG-REG/DT-INCLUSAO/DT-VALIDACAO *
      *                              CD-USUARIO/NOME-USUARIO           *
      *                              DS-SIT-OPER/DT-GERACAO-ARQ        *
      *                              DT-TRANSMIS-ARQ/NUM-PROTOCOLO.    *
      *                              ALTERACAO DO CAMPO SCSBW056-S-    *
      *                              SIT-REG PARA SIT-OPER.            *
      *================================================================*

          05 SCSBW056-HEADER.
             10 SCSBW056-COD-LAYOUT         PIC X(008) VALUE 'SCSBW056'.
             10 SCSBW056-TAM-LAYOUT         PIC 9(005) VALUE 20475.
          05 SCSBW056-REGISTRO.
             10 SCSBW056-BLOCO-ENTRADA.
                15 SCSBW056-E-ANO                PIC 9(004).
                15 SCSBW056-E-NREG               PIC 9(009).
                15 SCSBW056-E-COPER-ADITIVO      PIC 9(010).
             10 SCSBW056-BLOCO-SAIDA.
                15 SCSBW056-S-CPF-CNPJ-PRINC     PIC 9(009).
                15 SCSBW056-S-CPF-CNPJ-FLIAL     PIC 9(005).
                15 SCSBW056-S-CPF-CNPJ-CTRL      PIC 9(002).
                15 SCSBW056-S-DS-EMPR            PIC X(070).
                15 SCSBW056-S-CD-DEPEND-SAP      PIC 9(005).
                15 SCSBW056-S-DS-DEPEND          PIC X(060).
                15 SCSBW056-S-NOME-VEND          PIC X(150).
                15 SCSBW056-S-END-VEND           PIC X(150).
                15 SCSBW056-S-CD-PAIS-VEND       PIC 9(003).
                15 SCSBW056-S-DS-PAIS-VEND       PIC X(060).
                15 SCSBW056-S-NIF                PIC X(040).
                15 SCSBW056-S-CD-MOEDA           PIC 9(005).
                15 SCSBW056-S-DS-MOEDA           PIC X(040).
                15 SCSBW056-S-VLR-CONV-REAL      PIC 9(013)V99.
                15 SCSBW056-S-SIT-REG            PIC X(001).
                15 SCSBW056-S-DS-SIT-REG         PIC X(050).
                15 SCSBW056-S-ORIG-REG           PIC X(050).
                15 SCSBW056-S-DT-INCLUSAO        PIC X(010).
                15 SCSBW056-S-DT-VALIDACAO       PIC X(010).
                15 SCSBW056-S-CD-USUARIO         PIC X(009).
                15 SCSBW056-S-NOME-USUARIO       PIC X(080).
                15 SCSBW056-S-DT-GERACAO-ARQ     PIC X(010).
                15 SCSBW056-S-DT-TRANSMIS-ARQ    PIC X(010).
                15 SCSBW056-S-NUM-PROTOCOLO      PIC X(012).
                15 SCSBW056-S-INF-COMPLEM        PIC X(5000).
                15 SCSBW056-S-QTD-REG            PIC 9(003).
                15 SCSBW056-S-LISTA              OCCURS 0 TO 030 TIMES
                      DEPENDING ON SCSBW056-S-QTD-REG.
                   20 SCSBW056-S-COPER           PIC 9(010).
                   20 SCSBW056-S-CD-NBS          PIC X(009).
                   20 SCSBW056-S-DS-NBS          PIC X(250).
                   20 SCSBW056-S-CD-PAIS-DESTNO  PIC 9(003).
                   20 SCSBW056-S-DS-PAIS-DESTNO  PIC X(060).
                   20 SCSBW056-S-CD-MODO-PREST   PIC 9(001).
                   20 SCSBW056-S-DS-MODO-PREST   PIC X(050).
                   20 SCSBW056-S-DT-INICIO       PIC X(010).
                   20 SCSBW056-S-DT-CONCLUSAO    PIC X(010).
                   20 SCSBW056-S-VALOR           PIC 9(013)V99.
                   20 SCSBW056-S-VALOR-PAGO      PIC 9(013)V99.
                   20 SCSBW056-S-IND-ADITIVO     PIC X(001).
                   20 SCSBW056-S-SIT-OPER        PIC X(001).
                   20 SCSBW056-S-DS-SIT-OPER     PIC X(050).
                   20 SCSBW056-S-TPO-REG         PIC 9(001).

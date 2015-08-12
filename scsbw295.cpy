      *================================================================*
      * NOME BOOK  : SCSBW295                                          *
      * DESCRICAO  : INCLUIR SOLICITACAO DE ARQUIVO DE HISTORICO       *
      *              DE SERVICOS.                                      *
      * COMUNICACAO: FRAMEWORK X COORDENADOR                           *
      * DATA       : 19/06/2015                                        *
      * AUTOR      : TOSI                                              *
      * EMPRESA    : BRQ IT SERVICES                                   *
      * GRUPO      : 17 - CONTABILIDADE E IMPOSTOS                     *
      * COMPONENTE : SCSB - SISCOSERV BRADESCO                         *
      *================================================================*
      *                                                                *
      * SCSBW295-HEADER.                                               *
      *   SCSBW295-COD-LAYOUT            = CODIGO DO LAYOUT            *
      *   SCSBW295-TAM-LAYOUT            = TAMANHO DO LAYOUT           *
      * SCSBW295-REGISTRO.                                             *
      *   SCSBW295-BLOCO-ENTRADA.                                      *
      *     SCSBW295-E-TPO-REG           = TIPO DE REGISTRO            *
      *     SCSBW295-E-ANO               = ANO DO SERVICO              *
      *     SCSBW295-E-NREG              = NUMERO DO REGISTRO SERVICO  *
      *     SCSBW295-E-EMPR-DEP          = EMPRESA DA DEPENDENCIA      *
      *     SCSBW295-E-COD-DEP           = CODIGO DA DEPENDENCIA       *
      *     SCSBW295-E-EMPR-SERVICO      = EMPRESA DO SERVICO          *
      *     SCSBW295-E-DATA-DE           = PERIODO DE                  *
      *     SCSBW295-E-DATA-ATE          = PERIODO ATE                 *
      *     SCSBW295-E-CD-SIT-REG        = SITUACAO DO REGISTRO        *
      *     SCSBW295-E-NOME-ARQ          = NOME DO ARQUIVO A GERAR     *
      *     SCSBW295-E-DIRET-ARQ         = DIRETORIO DO ARQUIVO        *
      *                                                                *
      *----------------------------------------------------------------*
      * DATA       AUTOR             ALTERACAO                         *
      * ---------- ----------------- --------------------------------- *
      * 99/99/9999 XXXX              ----                              *
      *================================================================*

          05 SCSBW295-HEADER.
             10 SCSBW295-COD-LAYOUT         PIC X(008) VALUE 'SCSBW295'.
             10 SCSBW295-TAM-LAYOUT         PIC 9(005) VALUE   156.
          05 SCSBW295-REGISTRO.
             10 SCSBW295-BLOCO-ENTRADA.
                15 SCSBW295-E-TPO-REG            PIC 9(001).
                15 SCSBW295-E-ANO                PIC 9(004).
                15 SCSBW295-E-NREG               PIC 9(009).
                15 SCSBW295-E-EMPR-DEP           PIC 9(010).
                15 SCSBW295-E-COD-DEP            PIC 9(008).
                15 SCSBW295-E-EMPR-SERVICO       PIC 9(010).
                15 SCSBW295-E-DATA-DE            PIC X(010).
                15 SCSBW295-E-DATA-ATE           PIC X(010).
                15 SCSBW295-E-CD-SIT-REG         PIC X(001).
                15 SCSBW295-E-NOME-ARQ           PIC X(030).
                15 SCSBW295-E-DIRET-ARQ          PIC X(050).
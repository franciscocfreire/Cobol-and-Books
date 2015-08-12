      *================================================================*
      * NOME BOOK  : SCSBW376                                          *
      * DESCRICAO  : OBTER A DEPENDENCIA RESPONSAVEL PELA GERACAO DE   *
      *              ARQUIVOS DE TODAS AS EMPRESAS.                    *
      * COMUNICACAO: FRAMEWORK X COORDENADOR                           *
      * DATA       : 23/03/2015                                        *
      * AUTOR      : FERNANDA CARUSO                                   *
      * EMPRESA    : BRQ IT SERVICES                                   *
      * GRUPO      : 17 - CONTABILIDADE E IMPOSTOS                     *
      * COMPONENTE : SCSB - SISCOSERV BRADESCO                         *
      *================================================================*
      *                                                                *
      * SCSBW376-HEADER.                                               *
      *   SCSBW376-COD-LAYOUT          = CODIGO DO LAYOUT              *
      *   SCSBW376-TAM-LAYOUT          = TAMANHO DO LAYOUT             *
      * SCSBW376-REGISTRO.                                             *
      *   SCSBW376-BLOCO-SAIDA.                                        *
      *     SCSBW376-S-EMPR-DEPEND     = CODIGO DA EMPRESA DEPEND UORG *
      *     SCSBW376-S-DEPEND-UORG     = CODIGO DA DEPENDENCIA UORG    *
      *     SCSBW376-S-DEPEND-SAP      = CODIGO DA DEPENDENCIA SAP     *
      *     SCSBW376-S-DESC-DEPEND     = DESCRICAO DA DEPENDENCIA      *
      *                                                                *
      *----------------------------------------------------------------*
      * DATA       AUTOR             ALTERACAO                         *
      * ---------- ----------------- --------------------------------- *
      * 99/99/9999                   XXXXXXXXXXXXXXXXXXXXXXX           *
      *================================================================*
                                                                        
          05 SCSBW376-HEADER.                                           
             10 SCSBW376-COD-LAYOUT         PIC X(008) VALUE 'SCSBW376'.
             10 SCSBW376-TAM-LAYOUT         PIC 9(005) VALUE 96.        
          05 SCSBW376-REGISTRO.                                         
             10 SCSBW376-BLOCO-SAIDA.                                   
                15 SCSBW376-S-EMPR-DEPEND   PIC 9(010).                 
                15 SCSBW376-S-DEPEND-UORG   PIC 9(008).                 
                15 SCSBW376-S-DEPEND-SAP    PIC 9(005).                 
                15 SCSBW376-S-DESC-DEPEND   PIC X(060).                 

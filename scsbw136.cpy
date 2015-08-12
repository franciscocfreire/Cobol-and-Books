      *================================================================*
      * NOME BOOK  : SCSBW136                                          *
      * DESCRICAO  : VALIDAR DEPENDENCIA SAP NO UORG                   *
      * COMUNICACAO: FRAMEWORK X COORDENADOR                           *
      * DATA       : 03/12/2014                                        *
      * AUTOR      : FERNANDA CARUSO                                   *
      * EMPRESA    : BRQ IT SERVICES                                   *
      * GRUPO      : 17 - CONTABILIDADE E IMPOSTOS                     *
      * COMPONENTE : SCSB - SISCOSERV BRADESCO                         *
      *================================================================*
      *                                                                *
      * SCSBW136-HEADER.                                               *
      *   SCSBW136-COD-LAYOUT          = CODIGO DO LAYOUT              *
      *   SCSBW136-TAM-LAYOUT          = TAMANHO DO LAYOUT             *
      * SCSBW136-REGISTRO.                                             *
      *   SCSBW136-BLOCO-ENTRADA.                                      *
      *     SCSBW136-E-EMPR-SAP        = CODIGO DA EMPRESA SAP         *
      *     SCSBW136-E-DEPEND-SAP      = CODIGO DA DEPENDENCIA SAP     *
      *   SCSBW136-BLOCO-SAIDA.                                        *
      *     SCSBW136-S-EMPR-UORG       = CODIGO DA EMPRESA UORG        *
      *     SCSBW136-S-DEPEND-UORG     = CODIGO DA DEPENDENCIA UORG    *
      *     SCSBW136-S-DESC-DEPEND     = DESCRICAO DA DEPENDENCIA      *
      *                                                                *
      *----------------------------------------------------------------*
      * DATA       AUTOR             ALTERACAO                         *
      * ---------- ----------------- --------------------------------- *
      * 99/99/9999                   XXXXXXXXXXXXXXXXXXXXXXX           *
      *================================================================*
                                                                        
          05 SCSBW136-HEADER.                                           
             10 SCSBW136-COD-LAYOUT         PIC X(008) VALUE 'SCSBW136'.
             10 SCSBW136-TAM-LAYOUT         PIC 9(005) VALUE 101.       
          05 SCSBW136-REGISTRO.                                         
             10 SCSBW136-BLOCO-ENTRADA.                                 
                15 SCSBW136-E-EMPR-SAP      PIC X(005).                 
                15 SCSBW136-E-DEPEND-SAP    PIC 9(005).                 
             10 SCSBW136-BLOCO-SAIDA.                                   
                15 SCSBW136-S-EMPR-UORG     PIC 9(010).                 
                15 SCSBW136-S-DEPEND-UORG   PIC 9(008).                 
                15 SCSBW136-S-DESC-DEPEND   PIC X(060).                 

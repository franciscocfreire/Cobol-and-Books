      *================================================================*
      * NOME BOOK  : SCSBW152                                          *
      * DESCRICAO  : COMBO DEPENDENCIA DO SCSB                         *
      * COMUNICACAO: FRAMEWORK X COORDENADOR                           *
      * DATA       : 17/12/2014                                        *
      * AUTOR      : FERNANDA CARUSO                                   *
      * EMPRESA    : BRQ IT SERVICES                                   *
      * GRUPO      : 17 - CONTABILIDADE E IMPOSTOS                     *
      * COMPONENTE : SCSB - SISCOSERV BRADESCO                         *
      *================================================================*
      *                                                                *
      * SCSBW152-HEADER.                                               *
      *   SCSBW152-COD-LAYOUT          = CODIGO DO LAYOUT              *
      *   SCSBW152-TAM-LAYOUT          = TAMANHO DO LAYOUT             *
      * SCSBW152-REGISTRO.                                             *
      *   SCSBW152-BLOCO-ENTRADA.                                      *
      *     SCSBW152-E-TPO-REG         = TIPO DE REGISTRO              *
      *                                  1 -> RVS                      *
      *                                  2 -> RAS                      *
      *                                  3 -> RPC                      *
      *   SCSBW152-BLOCO-SAIDA.                                        *
      *     SCSBW152-S-QTD-REG         = QUANTIDADE DE REGISTROS       *
      *     SCSBW152-S-LISTA.                                          *
      *       SCSBW152-S-DEPEND-SAP    = CODIGO DA DEPENDENCIA SAP     *
      *       SCSBW152-S-EMPR-UORG     = CODIGO DA EMPRESA UORG        *
      *       SCSBW152-S-DEPEND-UORG   = CODIGO DA DEPENDENCIA UORG    *
      *       SCSBW152-S-DESC-DEPEND   = DESCRICAO DA DEPENDENCIA      *
      *                                                                *
      *----------------------------------------------------------------*
      * DATA       AUTOR             ALTERACAO                         *
      * ---------- ----------------- --------------------------------- *
      * 99/99/9999                   XXXXXXXXXXXXXXXXXXXXXXX           *
      *================================================================*
                                                                        
          05 SCSBW152-HEADER.                                           
             10 SCSBW152-COD-LAYOUT         PIC X(008) VALUE 'SCSBW152'.
             10 SCSBW152-TAM-LAYOUT         PIC 9(005) VALUE 4167.      
          05 SCSBW152-REGISTRO.                                         
             10 SCSBW152-BLOCO-ENTRADA.                                 
                15 SCSBW152-E-TPO-REG         PIC 9(001).               
             10 SCSBW152-BLOCO-SAIDA.                                   
                15 SCSBW152-S-QTD-REG         PIC 9(003).               
                15 SCSBW152-S-LISTA           OCCURS 0 TO 050 TIMES     
                      DEPENDING ON SCSBW152-S-QTD-REG.                  
                   20 SCSBW152-S-DEPEND-SAP   PIC 9(005).               
                   20 SCSBW152-S-EMPR-UORG    PIC 9(010).               
                   20 SCSBW152-S-DEPEND-UORG  PIC 9(008).               
                   20 SCSBW152-S-DESC-DEPEND  PIC X(060).               

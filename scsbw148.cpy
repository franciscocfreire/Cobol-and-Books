      *================================================================*
      * NOME BOOK  : SCSBW148                                          *
      * DESCRICAO  : COMBO USUARIO DO SCSB                             *
      * COMUNICACAO: FRAMEWORK X COORDENADOR                           *
      * DATA       : 16/12/2014                                        *
      * AUTOR      : FERNANDA CARUSO                                   *
      * EMPRESA    : BRQ IT SERVICES                                   *
      * GRUPO      : 17 - CONTABILIDADE E IMPOSTOS                     *
      * COMPONENTE : SCSB - SISCOSERV BRADESCO                         *
      *================================================================*
      *                                                                *
      * SCSBW148-HEADER.                                               *
      *   SCSBW148-COD-LAYOUT          = CODIGO DO LAYOUT              *
      *   SCSBW148-TAM-LAYOUT          = TAMANHO DO LAYOUT             *
      * SCSBW148-REGISTRO.                                             *
      *   SCSBW148-BLOCO-ENTRADA.                                      *
      *     SCSBW148-E-TPO-REG         = TIPO DE REGISTRO              *
      *                                  1 -> RVS                      *
      *                                  2 -> RAS                      *
      *                                  3 -> RPC                      *
      *     SCSBW148-E-EMPR-DEPEND     = COD EMPRESA DA DEPENDENCIA    *
      *     SCSBW148-E-DEPEND          = CODIGO DA DEPENDENCIA         *
      *   SCSBW148-BLOCO-SAIDA.                                        *
      *     SCSBW148-S-QTD-REG         = QUANTIDADE DE REGISTROS       *
      *     SCSBW148-S-LISTA.                                          *
      *       SCSBW148-S-CD-USUAR      = CODIGO DO USUARIO             *
      *       SCSBW148-S-NOME-USUAR    = NOME DO USUARIO               *
      *                                                                *
      *----------------------------------------------------------------*
      * DATA       AUTOR             ALTERACAO                         *
      * ---------- ----------------- --------------------------------- *
      * 99/99/9999                   XXXXXXXXXXXXXXXXXXXXXXX           *
      *================================================================*
                                                                        
          05 SCSBW148-HEADER.                                           
             10 SCSBW148-COD-LAYOUT         PIC X(008) VALUE 'SCSBW148'.
             10 SCSBW148-TAM-LAYOUT         PIC 9(005) VALUE 4485.      
          05 SCSBW148-REGISTRO.                                         
             10 SCSBW148-BLOCO-ENTRADA.                                 
                15 SCSBW148-E-TPO-REG       PIC 9(001).                 
                15 SCSBW148-E-EMPR-DEPEND   PIC 9(010).                 
                15 SCSBW148-E-DEPEND        PIC 9(008).                 
             10 SCSBW148-BLOCO-SAIDA.                                   
                15 SCSBW148-S-QTD-REG       PIC 9(003).                 
                15 SCSBW148-S-LISTA         OCCURS 0 TO 050 TIMES       
                      DEPENDING ON SCSBW148-S-QTD-REG.                  
                   20 SCSBW148-S-CD-USUAR   PIC X(009).                 
                   20 SCSBW148-S-NOME-USUAR PIC X(080).                 

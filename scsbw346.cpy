      *================================================================*
      * NOME BOOK  : SCSBW346                                          *
      * DESCRICAO  : ALTERAR RETIFICACAO DOS DI'S DA RAS               *
      * COMUNICACAO: FRAMEWORK X COORDENADOR                           *
      * DATA       : 09/02/2015                                        *
      * AUTOR      : FERNANDA CARUSO                                   *
      * EMPRESA    : BRQ IT SERVICES                                   *
      * GRUPO      : 17 - CONTABILIDADE E IMPOSTOS                     *
      * COMPONENTE : SCSB - SISCOSERV BRADESCO                         *
      *================================================================*
      *                                                                *
      * SCSBW346-HEADER.                                               *
      *   SCSBW346-COD-LAYOUT          = CODIGO DO LAYOUT              *
      *   SCSBW346-TAM-LAYOUT          = TAMANHO DO LAYOUT             *
      * SCSBW346-REGISTRO.                                             *
      *   SCSBW346-BLOCO-ENTRADA.                                      *
      *     SCSBW346-E-ANO             = ANO RAS                       *
      *     SCSBW346-E-NREG            = NUMERO DO REGISTRO RAS        *
      *     SCSBW346-E-QTD-REG         = QUANTIDADE DE REGISTROS       *
      *     SCSBW346-E-LISTA.                                          *
      *       SCSBW346-E-DI            = NUMERO DO DI                  *
      *                                                                *
      *----------------------------------------------------------------*
      * DATA       AUTOR             ALTERACAO                         *
      * ---------- ----------------- --------------------------------- *
      * 99/99/9999                   XXXXXXXXXXXXXXXXXXXXXXX           *
      *================================================================*
                                                                        
          05 SCSBW346-HEADER.                                           
             10 SCSBW346-COD-LAYOUT         PIC X(008) VALUE 'SCSBW346'.
             10 SCSBW346-TAM-LAYOUT         PIC 9(005) VALUE 529.       
          05 SCSBW346-REGISTRO.                                         
             10 SCSBW346-BLOCO-ENTRADA.                                 
                15 SCSBW346-E-ANO           PIC 9(004).                 
                15 SCSBW346-E-NREG          PIC 9(009).                 
                15 SCSBW346-E-QTD-REG       PIC 9(003).                 
                15 SCSBW346-E-LISTA         OCCURS 0 TO 050 TIMES       
                      DEPENDING ON SCSBW346-E-QTD-REG.                  
                   20 SCSBW346-E-DI         PIC 9(010).                 

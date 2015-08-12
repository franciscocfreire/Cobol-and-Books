      *================================================================*
      * NOME BOOK  : SCSBW336                                          *
      * DESCRICAO  : ALTERAR DI - EXCLUIR TODOS DI'S DA RAS INFORMADA  *
      *              E INCLUIR OS NOVOS.                               *
      * COMUNICACAO: FRAMEWORK X COORDENADOR                           *
      * DATA       : 09/02/2015                                        *
      * AUTOR      : FERNANDA CARUSO                                   *
      * EMPRESA    : BRQ IT SERVICES                                   *
      * GRUPO      : 17 - CONTABILIDADE E IMPOSTOS                     *
      * COMPONENTE : SCSB - SISCOSERV BRADESCO                         *
      *================================================================*
      *                                                                *
      * SCSBW336-HEADER.                                               *
      *   SCSBW336-COD-LAYOUT          = CODIGO DO LAYOUT              *
      *   SCSBW336-TAM-LAYOUT          = TAMANHO DO LAYOUT             *
      * SCSBW336-REGISTRO.                                             *
      *   SCSBW336-BLOCO-ENTRADA.                                      *
      *     SCSBW336-E-ANO             = ANO RAS                       *
      *     SCSBW336-E-NREG            = NUMERO DO REGISTRO RAS        *
      *     SCSBW336-E-QTD-REG.        = QUANTIDADE DE REGISTROS       *
      *     SCSBW336-E-LISTA.                                          *
      *       SCSBW336-E-DI            = NUMERO DO DI (OPCIONAL)       *
      *                                                                *
      *----------------------------------------------------------------*
      * DATA       AUTOR             ALTERACAO                         *
      * ---------- ----------------- --------------------------------- *
      * 99/99/9999                   XXXXXXXXXXXXXXXXXXXXXXX           *
      *================================================================*
                                                                        
          05 SCSBW336-HEADER.                                           
             10 SCSBW336-COD-LAYOUT         PIC X(008) VALUE 'SCSBW336'.
             10 SCSBW336-TAM-LAYOUT         PIC 9(005) VALUE 529.       
          05 SCSBW336-REGISTRO.                                         
             10 SCSBW336-BLOCO-ENTRADA.                                 
                15 SCSBW336-E-ANO           PIC 9(004).                 
                15 SCSBW336-E-NREG          PIC 9(009).                 
                15 SCSBW336-E-QTD-REG       PIC 9(003).                 
                15 SCSBW336-E-LISTA         OCCURS 0 TO 050 TIMES       
                      DEPENDING ON SCSBW336-E-QTD-REG.                  
                   20 SCSBW336-E-DI         PIC 9(010).                 

      *================================================================*
      * NOME BOOK  : SCSBW188                                          *
      * DESCRICAO  : ALTERAR RETIFICACAO DOS RE'S DA RVS               *
      * COMUNICACAO: FRAMEWORK X COORDENADOR                           *
      * DATA       : 26/01/2015                                        *
      * AUTOR      : FERNANDA CARUSO                                   *
      * EMPRESA    : BRQ IT SERVICES                                   *
      * GRUPO      : 17 - CONTABILIDADE E IMPOSTOS                     *
      * COMPONENTE : SCSB - SISCOSERV BRADESCO                         *
      *================================================================*
      *                                                                *
      * SCSBW188-HEADER.                                               *
      *   SCSBW188-COD-LAYOUT          = CODIGO DO LAYOUT              *
      *   SCSBW188-TAM-LAYOUT          = TAMANHO DO LAYOUT             *
      * SCSBW188-REGISTRO.                                             *
      *   SCSBW188-BLOCO-ENTRADA.                                      *
      *     SCSBW188-E-ANO             = ANO RVS                       *
      *     SCSBW188-E-NREG            = NUMERO DO REGISTRO RVS        *
      *     SCSBW188-E-QTD-REG         = QUANTIDADE DE REGISTROS       *
      *     SCSBW188-E-LISTA.                                          *
      *       SCSBW188-E-RE            = NUMERO DO RE                  *
      *                                                                *
      *----------------------------------------------------------------*
      * DATA       AUTOR             ALTERACAO                         *
      * ---------- ----------------- --------------------------------- *
      * 99/99/9999                   XXXXXXXXXXXXXXXXXXXXXXX           *
      *================================================================*
                                                                        
          05 SCSBW188-HEADER.                                           
             10 SCSBW188-COD-LAYOUT         PIC X(008) VALUE 'SCSBW188'.
             10 SCSBW188-TAM-LAYOUT         PIC 9(005) VALUE 629.       
          05 SCSBW188-REGISTRO.                                         
             10 SCSBW188-BLOCO-ENTRADA.                                 
                15 SCSBW188-E-ANO           PIC 9(004).                 
                15 SCSBW188-E-NREG          PIC 9(009).                 
                15 SCSBW188-E-QTD-REG       PIC 9(003).                 
                15 SCSBW188-E-LISTA         OCCURS 0 TO 050 TIMES       
                      DEPENDING ON SCSBW188-E-QTD-REG.                  
                   20 SCSBW188-E-RE         PIC 9(012).                 

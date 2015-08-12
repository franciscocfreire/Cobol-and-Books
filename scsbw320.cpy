      *================================================================*
      * NOME BOOK  : SCSBW320                                          *
      * DESCRICAO  : CONSULTAR NO HISTORICO OS REGISTROS IMPORTACOES   *
      *              DE BENS (DI) DA RAS.                              *
      * COMUNICACAO: FRAMEWORK X COORDENADOR                           *
      * DATA       : 05/02/2015                                        *
      * AUTOR      : FERNANDA CARUSO                                   *
      * EMPRESA    : BRQ IT SERVICES                                   *
      * GRUPO      : 17 - CONTABILIDADE E IMPOSTOS                     *
      * COMPONENTE : SCSB - SISCOSERV BRADESCO                         *
      *================================================================*
      *                                                                *
      * SCSBW320-HEADER.                                               *
      *   SCSBW320-COD-LAYOUT          = CODIGO DO LAYOUT              *
      *   SCSBW320-TAM-LAYOUT          = TAMANHO DO LAYOUT             *
      * SCSBW320-REGISTRO.                                             *
      *   SCSBW320-BLOCO-ENTRADA.                                      *
      *     SCSBW320-E-ANO             = ANO RAS                       *
      *     SCSBW320-E-NREG            = NUMERO DO REGISTRO RAS        *
      *     SCSBW320-E-HINCL-HIST      = TIMESTAMP INCLUSAO HISTORICO  *
      *   SCSBW320-BLOCO-SAIDA.                                        *
      *     SCSBW320-S-QTD-REG         = QTDE REGISTROS DEVOLVIDOS     *
      *     SCSBW320-S-LISTA.                                          *
      *       SCSBW320-S-DI            = REGISTRO IMPORTACAO DE BENS   *
      *                                                                *
      *----------------------------------------------------------------*
      * DATA       AUTOR             ALTERACAO                         *
      * ---------- ----------------- --------------------------------- *
      * 99/99/9999                   XXXXXXXXXXXXXXXXXXXXXXX           *
      *================================================================*
                                                                        
          05 SCSBW320-HEADER.                                           
             10 SCSBW320-COD-LAYOUT         PIC X(008) VALUE 'SCSBW320'.
             10 SCSBW320-TAM-LAYOUT         PIC 9(005) VALUE 555.       
          05 SCSBW320-REGISTRO.                                         
             10 SCSBW320-BLOCO-ENTRADA.                                 
                15 SCSBW320-E-ANO           PIC 9(004).                 
                15 SCSBW320-E-NREG          PIC 9(009).                 
                15 SCSBW320-E-HINCL-HIST    PIC X(026).                 
             10 SCSBW320-BLOCO-SAIDA.                                   
                15 SCSBW320-S-QTD-REG       PIC 9(003).                 
                15 SCSBW320-S-LISTA         OCCURS 0 TO 050 TIMES       
                      DEPENDING ON SCSBW320-S-QTD-REG.                  
                   20 SCSBW320-S-DI         PIC 9(010).                 

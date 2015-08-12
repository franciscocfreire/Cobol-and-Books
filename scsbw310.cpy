      *================================================================*
      * NOME BOOK  : SCSBW310                                          *
      * DESCRICAO  : ATUALIZAR RE'S OU DI'S REFERENTE AO FATURAMENTO   *
      * COMUNICACAO: FRAMEWORK X COORDENADOR                           *
      * DATA       : 03/02/2015                                        *
      * AUTOR      : FERNANDA CARUSO                                   *
      * EMPRESA    : BRQ IT SERVICES                                   *
      * GRUPO      : 17 - CONTABILIDADE E IMPOSTOS                     *
      * COMPONENTE : SCSB - SISCOSERV BRADESCO                         *
      *================================================================*
      *                                                                *
      * SCSBW310-HEADER.                                               *
      *   SCSBW310-COD-LAYOUT            = CODIGO DO LAYOUT            *
      *   SCSBW310-TAM-LAYOUT            = TAMANHO DO LAYOUT           *
      * SCSBW310-REGISTRO.                                             *
      *   SCSBW310-BLOCO-ENTRADA.                                      *
      *     SCSBW310-E-ANO               = ANO RVS                     *
      *     SCSBW310-E-NREG              = NUMERO DO REGISTRO RVS      *
      *     SCSBW310-E-NFATMT            = NUMERO DE FATURAMENTO       *
      *     SCSBW310-E-FUNCAO            = FUNCAO -> 1-RE / 2-DI       *
      *     SCSBW310-E-QTD-REG           = QUANTIDADE DE REGISTROS     *
      *     SCSBW310-E-LISTA.                                          *
      *       SCSBW310-E-CD-REG          = COD DO REGISTRO (RE OU DI)  *
      *                                                                *
      *----------------------------------------------------------------*
      * DATA       AUTOR             ALTERACAO                         *
      * ---------- ----------------- --------------------------------- *
      * 99/99/9999                   XXXXXXXXXXXXXXXXXXXXXXX           *
      *================================================================*
                                                                        
          05 SCSBW310-HEADER.                                           
             10 SCSBW310-COD-LAYOUT         PIC X(008) VALUE 'SCSBW310'.
             10 SCSBW310-TAM-LAYOUT         PIC 9(005) VALUE 640.       
          05 SCSBW310-REGISTRO.                                         
             10 SCSBW310-BLOCO-ENTRADA.                                 
                15 SCSBW310-E-ANO               PIC 9(004).             
                15 SCSBW310-E-NREG              PIC 9(009).             
                15 SCSBW310-E-NFATMT            PIC 9(010).             
                15 SCSBW310-E-FUNCAO            PIC 9(001).             
                15 SCSBW310-E-QTD-REG           PIC 9(003).             
                15 SCSBW310-E-LISTA             OCCURS 0 TO 050 TIMES   
                      DEPENDING ON SCSBW310-E-QTD-REG.                  
                   20 SCSBW310-E-CD-REG         PIC 9(012).             

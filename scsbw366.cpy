      *================================================================*
      * NOME BOOK  : SCSBW366                                          *
      * DESCRICAO  : ATUALIZAR RE'S OU DI'S REFERENTE AO PAGAMENTO     *
      * COMUNICACAO: FRAMEWORK X COORDENADOR                           *
      * DATA       : 11/02/2015                                        *
      * AUTOR      : FERNANDA CARUSO                                   *
      * EMPRESA    : BRQ IT SERVICES                                   *
      * GRUPO      : 17 - CONTABILIDADE E IMPOSTOS                     *
      * COMPONENTE : SCSB - SISCOSERV BRADESCO                         *
      *================================================================*
      *                                                                *
      * SCSBW366-HEADER.                                               *
      *   SCSBW366-COD-LAYOUT            = CODIGO DO LAYOUT            *
      *   SCSBW366-TAM-LAYOUT            = TAMANHO DO LAYOUT           *
      * SCSBW366-REGISTRO.                                             *
      *   SCSBW366-BLOCO-ENTRADA.                                      *
      *     SCSBW366-E-ANO               = ANO RAS                     *
      *     SCSBW366-E-NREG              = NUMERO DO REGISTRO RAS      *
      *     SCSBW366-E-NPGTO             = NUMERO DO PAGAMENTO         *
      *     SCSBW366-E-FUNCAO            = FUNCAO -> 1-RE / 2-DI       *
      *     SCSBW366-E-QTD-REG           = QUANTIDADE DE REGISTROS     *
      *     SCSBW366-E-LISTA.                                          *
      *       SCSBW366-E-CD-REG          = COD DO REGISTRO (RE OU DI)  *
      *                                                                *
      *----------------------------------------------------------------*
      * DATA       AUTOR             ALTERACAO                         *
      * ---------- ----------------- --------------------------------- *
      * 99/99/9999                   XXXXXXXXXXXXXXXXXXXXXXX           *
      *================================================================*
                                                                        
          05 SCSBW366-HEADER.                                           
             10 SCSBW366-COD-LAYOUT         PIC X(008) VALUE 'SCSBW366'.
             10 SCSBW366-TAM-LAYOUT         PIC 9(005) VALUE 640.       
          05 SCSBW366-REGISTRO.                                         
             10 SCSBW366-BLOCO-ENTRADA.                                 
                15 SCSBW366-E-ANO               PIC 9(004).             
                15 SCSBW366-E-NREG              PIC 9(009).             
                15 SCSBW366-E-NPGTO             PIC 9(010).             
                15 SCSBW366-E-FUNCAO            PIC 9(001).             
                15 SCSBW366-E-QTD-REG           PIC 9(003).             
                15 SCSBW366-E-LISTA             OCCURS 0 TO 050 TIMES   
                      DEPENDING ON SCSBW366-E-QTD-REG.                  
                   20 SCSBW366-E-CD-REG         PIC 9(012).             

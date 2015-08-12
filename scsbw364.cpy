      *================================================================*
      * NOME BOOK  : SCSBW364                                          *
      * DESCRICAO  : ALTERAR REGISTRO DE PAGAMENTO                     *
      * COMUNICACAO: FRAMEWORK X COORDENADOR                           *
      * DATA       : 05/05/2015                                        *
      * AUTOR      : FERNANDA CARUSO                                   *
      * EMPRESA    : BRQ IT SERVICES                                   *
      * GRUPO      : 17 - CONTABILIDADE E IMPOSTOS                     *
      * COMPONENTE : SCSB - SISCOSERV BRADESCO                         *
      *================================================================*
      *                                                                *
      * SCSBW364-HEADER.                                               *
      *   SCSBW364-COD-LAYOUT            = CODIGO DO LAYOUT            *
      *   SCSBW364-TAM-LAYOUT            = TAMANHO DO LAYOUT           *
      * SCSBW364-REGISTRO.                                             *
      *   SCSBW364-BLOCO-ENTRADA.                                      *
      *     SCSBW364-E-ANO               = ANO RAS                     *
      *     SCSBW364-E-NREG              = NUMERO DO REGISTRO RAS      *
      *     SCSBW364-E-NPGTO             = NUMERO DE PAGAMENTO         *
      *     SCSBW364-E-DATA              = DATA DO PAGAMENTO           *
      *     SCSBW364-E-NF                = NUMERO DA NOTA FISCAL       *
      *     SCSBW364-E-QTD-REG           = QUANTIDADE DE REGISTROS     *
      *     SCSBW364-E-LISTA.                                          *
      *       SCSBW364-E-COPER           = NUMERO DA OPERACAO          *
      *       SCSBW364-E-VLR-PAGO        = VALOR PAGO                  *
      *       SCSBW364-E-VLR-MANT-EXTER  = VALOR MANTIDO NO EXTERIOR   *
      *                                                                *
      *----------------------------------------------------------------*
      * DATA       AUTOR             ALTERACAO                         *
      * ---------- ----------------- --------------------------------- *
      * 99/99/9999                   XXXXXXXXXXXXXXXXXXXXXXX           *
      *================================================================*
                                                                        
          05 SCSBW364-HEADER.                                           
             10 SCSBW364-COD-LAYOUT         PIC X(008) VALUE 'SCSBW364'.
             10 SCSBW364-TAM-LAYOUT         PIC 9(005) VALUE 2109.      
          05 SCSBW364-REGISTRO.                                         
             10 SCSBW364-BLOCO-ENTRADA.                                 
                15 SCSBW364-E-ANO               PIC 9(004).             
                15 SCSBW364-E-NREG              PIC 9(009).             
                15 SCSBW364-E-NPGTO             PIC 9(010).             
                15 SCSBW364-E-DATA              PIC X(010).             
                15 SCSBW364-E-NF                PIC X(060).             
                15 SCSBW364-E-QTD-REG           PIC 9(003).             
                15 SCSBW364-E-LISTA             OCCURS 0 TO 050 TIMES   
                      DEPENDING ON SCSBW364-E-QTD-REG.                  
                   20 SCSBW364-E-COPER          PIC 9(010).             
                   20 SCSBW364-E-VLR-PAGO       PIC 9(013)V99.          
                   20 SCSBW364-E-VLR-MANT-EXTER PIC 9(013)V99.          

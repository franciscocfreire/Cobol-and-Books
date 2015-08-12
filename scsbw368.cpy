      *================================================================*
      * NOME BOOK  : SCSBW368                                          *
      * DESCRICAO  : EXCLUIR PAGAMENTO                                 *
      * COMUNICACAO: FRAMEWORK X COORDENADOR                           *
      * DATA       : 12/02/2015                                        *
      * AUTOR      : FERNANDA CARUSO                                   *
      * EMPRESA    : BRQ IT SERVICES                                   *
      * GRUPO      : 17 - CONTABILIDADE E IMPOSTOS                     *
      * COMPONENTE : SCSB - SISCOSERV BRADESCO                         *
      *================================================================*
      *                                                                *
      * SCSBW368-HEADER.                                               *
      *   SCSBW368-COD-LAYOUT            = CODIGO DO LAYOUT            *
      *   SCSBW368-TAM-LAYOUT            = TAMANHO DO LAYOUT           *
      * SCSBW368-REGISTRO.                                             *
      *   SCSBW368-BLOCO-ENTRADA.                                      *
      *     SCSBW368-E-ANO               = ANO RAS                     *
      *     SCSBW368-E-NREG              = NUMERO DO REGISTRO RAS      *
      *     SCSBW368-E-NPGTO             = NUMERO DE PAGAMENTO         *
      *     SCSBW368-E-SIT-PAG           = SITUACAO DO PAGAMENTO       *
      *                                                                *
      *----------------------------------------------------------------*
      * DATA       AUTOR             ALTERACAO                         *
      * ---------- ----------------- --------------------------------- *
      * 99/99/9999                   XXXXXXXXXXXXXXXXXXXXXXX           *
      *================================================================*
                                                                        
          05 SCSBW368-HEADER.                                           
             10 SCSBW368-COD-LAYOUT         PIC X(008) VALUE 'SCSBW368'.
             10 SCSBW368-TAM-LAYOUT         PIC 9(005) VALUE 37.        
          05 SCSBW368-REGISTRO.                                         
             10 SCSBW368-BLOCO-ENTRADA.                                 
                15 SCSBW368-E-ANO           PIC 9(004).                 
                15 SCSBW368-E-NREG          PIC 9(009).                 
                15 SCSBW368-E-NPGTO         PIC 9(010).                 
                15 SCSBW368-E-SIT-PAG       PIC X(001).                 

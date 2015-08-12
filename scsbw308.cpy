      *================================================================*
      * NOME BOOK  : SCSBW308                                          *
      * DESCRICAO  : ALTERAR REGISTRO DE FATURAMENTO                   *
      * COMUNICACAO: FRAMEWORK X COORDENADOR                           *
      * DATA       : 02/02/2015                                        *
      * AUTOR      : FERNANDA CARUSO                                   *
      * EMPRESA    : BRQ IT SERVICES                                   *
      * GRUPO      : 17 - CONTABILIDADE E IMPOSTOS                     *
      * COMPONENTE : SCSB - SISCOSERV BRADESCO                         *
      *================================================================*
      *                                                                *
      * SCSBW308-HEADER.                                               *
      *   SCSBW308-COD-LAYOUT            = CODIGO DO LAYOUT            *
      *   SCSBW308-TAM-LAYOUT            = TAMANHO DO LAYOUT           *
      * SCSBW308-REGISTRO.                                             *
      *   SCSBW308-BLOCO-ENTRADA.                                      *
      *     SCSBW308-E-ANO               = ANO RVS                     *
      *     SCSBW308-E-NREG              = NUMERO DO REGISTRO RVS      *
      *     SCSBW308-E-NFATMT            = NUMERO DE FATURAMENTO       *
      *     SCSBW308-E-DATA              = DATA DO FATURAMENTO         *
      *     SCSBW308-E-NF                = NUMERO DA NOTA FISCAL       *
      *     SCSBW308-E-QTD-REG           = QUANTIDADE DE REGISTROS     *
      *     SCSBW308-E-LISTA.                                          *
      *       SCSBW308-E-COPER           = NUMERO DA OPERACAO          *
      *       SCSBW308-E-VLR-FATURADO    = VALOR FATURADO              *
      *       SCSBW308-E-VLR-MANT-EXTER  = VALOR MANTIDO NO EXTERIOR   *
      *                                                                *
      *----------------------------------------------------------------*
      * DATA       AUTOR             ALTERACAO                         *
      * ---------- ----------------- --------------------------------- *
      * 99/99/9999                   XXXXXXXXXXXXXXXXXXXXXXX           *
      *================================================================*
                                                                        
          05 SCSBW308-HEADER.                                           
             10 SCSBW308-COD-LAYOUT         PIC X(008) VALUE 'SCSBW308'.
             10 SCSBW308-TAM-LAYOUT         PIC 9(005) VALUE 2109.      
          05 SCSBW308-REGISTRO.                                         
             10 SCSBW308-BLOCO-ENTRADA.                                 
                15 SCSBW308-E-ANO               PIC 9(004).             
                15 SCSBW308-E-NREG              PIC 9(009).             
                15 SCSBW308-E-NFATMT            PIC 9(010).             
                15 SCSBW308-E-DATA              PIC X(010).             
                15 SCSBW308-E-NF                PIC X(060).             
                15 SCSBW308-E-QTD-REG           PIC 9(003).             
                15 SCSBW308-E-LISTA             OCCURS 0 TO 050 TIMES   
                      DEPENDING ON SCSBW308-E-QTD-REG.                  
                   20 SCSBW308-E-COPER          PIC 9(010).             
                   20 SCSBW308-E-VLR-FATURADO   PIC 9(013)V99.          
                   20 SCSBW308-E-VLR-MANT-EXTER PIC 9(013)V99.          

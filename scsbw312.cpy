      *================================================================*
      * NOME BOOK  : SCSBW312                                          *
      * DESCRICAO  : EXCLUIR FATURAMENTO                               *
      * COMUNICACAO: FRAMEWORK X COORDENADOR                           *
      * DATA       : 03/02/2015                                        *
      * AUTOR      : FERNANDA CARUSO                                   *
      * EMPRESA    : BRQ IT SERVICES                                   *
      * GRUPO      : 17 - CONTABILIDADE E IMPOSTOS                     *
      * COMPONENTE : SCSB - SISCOSERV BRADESCO                         *
      *================================================================*
      *                                                                *
      * SCSBW312-HEADER.                                               *
      *   SCSBW312-COD-LAYOUT            = CODIGO DO LAYOUT            *
      *   SCSBW312-TAM-LAYOUT            = TAMANHO DO LAYOUT           *
      * SCSBW312-REGISTRO.                                             *
      *   SCSBW312-BLOCO-ENTRADA.                                      *
      *     SCSBW312-E-ANO               = ANO RVS                     *
      *     SCSBW312-E-NREG              = NUMERO DO REGISTRO RVS      *
      *     SCSBW312-E-NFATMT            = NUMERO DE FATURAMENTO       *
      *     SCSBW312-E-SIT-FAT           = SITUACAO DO FATURAMENTO     *
      *                                                                *
      *----------------------------------------------------------------*
      * DATA       AUTOR             ALTERACAO                         *
      * ---------- ----------------- --------------------------------- *
      * 99/99/9999                   XXXXXXXXXXXXXXXXXXXXXXX           *
      *================================================================*
                                                                        
          05 SCSBW312-HEADER.                                           
             10 SCSBW312-COD-LAYOUT         PIC X(008) VALUE 'SCSBW312'.
             10 SCSBW312-TAM-LAYOUT         PIC 9(005) VALUE 37.        
          05 SCSBW312-REGISTRO.                                         
             10 SCSBW312-BLOCO-ENTRADA.                                 
                15 SCSBW312-E-ANO           PIC 9(004).                 
                15 SCSBW312-E-NREG          PIC 9(009).                 
                15 SCSBW312-E-NFATMT        PIC 9(010).                 
                15 SCSBW312-E-SIT-FAT       PIC X(001).                 

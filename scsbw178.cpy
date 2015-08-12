      *================================================================*
      * NOME BOOK  : SCSBW178                                          *
      * DESCRICAO  : EXCLUIR RVS EM TODAS AS TABELAS                   *
      * COMUNICACAO: FRAMEWORK X COORDENADOR                           *
      * DATA       : 21/01/2015                                        *
      * AUTOR      : FERNANDA CARUSO                                   *
      * EMPRESA    : BRQ IT SERVICES                                   *
      * GRUPO      : 17 - CONTABILIDADE E IMPOSTOS                     *
      * COMPONENTE : SCSB - SISCOSERV BRADESCO                         *
      *================================================================*
      *                                                                *
      * SCSBW178-HEADER.                                               *
      *   SCSBW178-COD-LAYOUT          = CODIGO DO LAYOUT              *
      *   SCSBW178-TAM-LAYOUT          = TAMANHO DO LAYOUT             *
      * SCSBW178-REGISTRO.                                             *
      *   SCSBW178-BLOCO-ENTRADA.                                      *
      *     SCSBW178-E-ANO             = ANO RVS                       *
      *     SCSBW178-E-NREG            = NUMERO DO REGISTRO RVS        *
      *     SCSBW178-E-SIT-REG         = SITUACAO DO REGISTRO          *
      *                                                                *
      *----------------------------------------------------------------*
      * DATA       AUTOR             ALTERACAO                         *
      * ---------- ----------------- --------------------------------- *
      * 99/99/9999                   XXXXXXXXXXXXXXXXXXXXXXX           *
      *================================================================*
                                                                        
          05 SCSBW178-HEADER.                                           
             10 SCSBW178-COD-LAYOUT         PIC X(008) VALUE 'SCSBW178'.
             10 SCSBW178-TAM-LAYOUT         PIC 9(005) VALUE 27.        
          05 SCSBW178-REGISTRO.                                         
             10 SCSBW178-BLOCO-ENTRADA.                                 
                15 SCSBW178-E-ANO           PIC 9(004).                 
                15 SCSBW178-E-NREG          PIC 9(009).                 
                15 SCSBW178-E-SIT-REG       PIC X(001).                 

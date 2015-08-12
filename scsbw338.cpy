      *================================================================*
      * NOME BOOK  : SCSBW338                                          *
      * DESCRICAO  : EXCLUIR RAS EM TODAS AS TABELAS                   *
      * COMUNICACAO: FRAMEWORK X COORDENADOR                           *
      * DATA       : 09/02/2015                                        *
      * AUTOR      : FERNANDA CARUSO                                   *
      * EMPRESA    : BRQ IT SERVICES                                   *
      * GRUPO      : 17 - CONTABILIDADE E IMPOSTOS                     *
      * COMPONENTE : SCSB - SISCOSERV BRADESCO                         *
      *================================================================*
      *                                                                *
      * SCSBW338-HEADER.                                               *
      *   SCSBW338-COD-LAYOUT          = CODIGO DO LAYOUT              *
      *   SCSBW338-TAM-LAYOUT          = TAMANHO DO LAYOUT             *
      * SCSBW338-REGISTRO.                                             *
      *   SCSBW338-BLOCO-ENTRADA.                                      *
      *     SCSBW338-E-ANO             = ANO RAS                       *
      *     SCSBW338-E-NREG            = NUMERO DO REGISTRO RAS        *
      *     SCSBW338-E-SIT-REG         = SITUACAO DO REGISTRO          *
      *                                                                *
      *----------------------------------------------------------------*
      * DATA       AUTOR             ALTERACAO                         *
      * ---------- ----------------- --------------------------------- *
      * 99/99/9999                   XXXXXXXXXXXXXXXXXXXXXXX           *
      *================================================================*
                                                                        
          05 SCSBW338-HEADER.                                           
             10 SCSBW338-COD-LAYOUT         PIC X(008) VALUE 'SCSBW338'.
             10 SCSBW338-TAM-LAYOUT         PIC 9(005) VALUE 27.        
          05 SCSBW338-REGISTRO.                                         
             10 SCSBW338-BLOCO-ENTRADA.                                 
                15 SCSBW338-E-ANO           PIC 9(004).                 
                15 SCSBW338-E-NREG          PIC 9(009).                 
                15 SCSBW338-E-SIT-REG       PIC X(001).                 

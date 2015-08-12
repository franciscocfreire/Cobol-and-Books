      *================================================================*
      * NOME BOOK  : SCSBW190                                          *
      * DESCRICAO  : EXCLUIR RETIFICACAO DE RVS EM TODAS AS TABELAS    *
      * COMUNICACAO: FRAMEWORK X COORDENADOR                           *
      * DATA       : 26/01/2015                                        *
      * AUTOR      : FERNANDA CARUSO                                   *
      * EMPRESA    : BRQ IT SERVICES                                   *
      * GRUPO      : 17 - CONTABILIDADE E IMPOSTOS                     *
      * COMPONENTE : SCSB - SISCOSERV BRADESCO                         *
      *================================================================*
      *                                                                *
      * SCSBW190-HEADER.                                               *
      *   SCSBW190-COD-LAYOUT          = CODIGO DO LAYOUT              *
      *   SCSBW190-TAM-LAYOUT          = TAMANHO DO LAYOUT             *
      * SCSBW190-REGISTRO.                                             *
      *   SCSBW190-BLOCO-ENTRADA.                                      *
      *     SCSBW190-E-ANO             = ANO RVS                       *
      *     SCSBW190-E-NREG            = NUMERO DO REGISTRO RVS        *
      *     SCSBW190-E-SIT-REG         = SITUACAO DO REGISTRO          *
      *                                                                *
      *----------------------------------------------------------------*
      * DATA       AUTOR             ALTERACAO                         *
      * ---------- ----------------- --------------------------------- *
      * 99/99/9999                   XXXXXXXXXXXXXXXXXXXXXXX           *
      *================================================================*
                                                                        
          05 SCSBW190-HEADER.                                           
             10 SCSBW190-COD-LAYOUT         PIC X(008) VALUE 'SCSBW190'.
             10 SCSBW190-TAM-LAYOUT         PIC 9(005) VALUE 27.        
          05 SCSBW190-REGISTRO.                                         
             10 SCSBW190-BLOCO-ENTRADA.                                 
                15 SCSBW190-E-ANO           PIC 9(004).                 
                15 SCSBW190-E-NREG          PIC 9(009).                 
                15 SCSBW190-E-SIT-REG       PIC X(001).                 

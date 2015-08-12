      *================================================================*
      * NOME BOOK  : SCSBW348                                          *
      * DESCRICAO  : EXCLUIR RETIFICACAO DE RAS EM TODAS AS TABELAS    *
      * COMUNICACAO: FRAMEWORK X COORDENADOR                           *
      * DATA       : 09/02/2015                                        *
      * AUTOR      : FERNANDA CARUSO                                   *
      * EMPRESA    : BRQ IT SERVICES                                   *
      * GRUPO      : 17 - CONTABILIDADE E IMPOSTOS                     *
      * COMPONENTE : SCSB - SISCOSERV BRADESCO                         *
      *================================================================*
      *                                                                *
      * SCSBW348-HEADER.                                               *
      *   SCSBW348-COD-LAYOUT          = CODIGO DO LAYOUT              *
      *   SCSBW348-TAM-LAYOUT          = TAMANHO DO LAYOUT             *
      * SCSBW348-REGISTRO.                                             *
      *   SCSBW348-BLOCO-ENTRADA.                                      *
      *     SCSBW348-E-ANO             = ANO RAS                       *
      *     SCSBW348-E-NREG            = NUMERO DO REGISTRO RAS        *
      *     SCSBW348-E-SIT-REG         = SITUACAO DO REGISTRO          *
      *                                                                *
      *----------------------------------------------------------------*
      * DATA       AUTOR             ALTERACAO                         *
      * ---------- ----------------- --------------------------------- *
      * 99/99/9999                   XXXXXXXXXXXXXXXXXXXXXXX           *
      *================================================================*
                                                                        
          05 SCSBW348-HEADER.                                           
             10 SCSBW348-COD-LAYOUT         PIC X(008) VALUE 'SCSBW348'.
             10 SCSBW348-TAM-LAYOUT         PIC 9(005) VALUE 27.        
          05 SCSBW348-REGISTRO.                                         
             10 SCSBW348-BLOCO-ENTRADA.                                 
                15 SCSBW348-E-ANO           PIC 9(004).                 
                15 SCSBW348-E-NREG          PIC 9(009).                 
                15 SCSBW348-E-SIT-REG       PIC X(001).                 

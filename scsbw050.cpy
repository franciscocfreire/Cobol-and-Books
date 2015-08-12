      *================================================================*
      * NOME BOOK  : SCSBW050                                          *
      * DESCRICAO  : CANCELAR REGISTRO DE FATURAMENTO                  *
      * COMUNICACAO: FRAMEWORK X COORDENADOR                           *
      * DATA       : 02/03/2013                                        *
      * AUTOR      : FERNANDA CARUSO                                   *
      * EMPRESA    : BRQ IT SERVICES                                   *
      * GRUPO      : 17 - CONTABILIDADE E IMPOSTOS                     *
      * COMPONENTE : SCSB - SISCOSERV BRADESCO                         *
      *================================================================*
      *                                                                *
      * SCSBW050-HEADER.                                               *
      *   SCSBW050-COD-LAYOUT          = CODIGO DO LAYOUT              *
      *   SCSBW050-TAM-LAYOUT          = TAMANHO DO LAYOUT             *
      * SCSBW050-REGISTRO.                                             *
      *   SCSBW050-BLOCO-ENTRADA.                                      *
      *     SCSBW050-E-ANO             = ANO RVS                       *
      *     SCSBW050-E-NREG            = NUMERO DO REGISTRO RVS        *
      *     SCSBW050-E-CFATMT          = NUMERO DO FATURAMENTO         *
      *                                                                *
      *----------------------------------------------------------------*
      * DATA       AUTOR             ALTERACAO                         *
      * ---------- ----------------- --------------------------------- *
      * 99/99/9999                   XXXXXXXXXXXXXXXXXXXXXXX           *
      *================================================================*
                                                                        
          05 SCSBW050-HEADER.                                           
             10 SCSBW050-COD-LAYOUT         PIC X(008) VALUE 'SCSBW050'.
             10 SCSBW050-TAM-LAYOUT         PIC 9(005) VALUE 36.        
          05 SCSBW050-REGISTRO.                                         
             10 SCSBW050-BLOCO-ENTRADA.                                 
                15 SCSBW050-E-ANO           PIC 9(004).                 
                15 SCSBW050-E-NREG          PIC 9(009).                 
                15 SCSBW050-E-CFATMT        PIC 9(010).                 

      *================================================================*
      * NOME BOOK  : SCSBW322                                          *
      * DESCRICAO  : CONSULTAR DADOS DO ULTIMO LOTE QUE O ADITIVO DA   *
      *              RAS FOI ENVIADO.                                  *
      * COMUNICACAO: FRAMEWORK X COORDENADOR                           *
      * DATA       : 06/02/2015                                        *
      * AUTOR      : FERNANDA CARUSO                                   *
      * EMPRESA    : BRQ IT SERVICES                                   *
      * GRUPO      : 17 - CONTABILIDADE E IMPOSTOS                     *
      * COMPONENTE : SCSB - SISCOSERV BRADESCO                         *
      *================================================================*
      *                                                                *
      * SCSBW322-HEADER.                                               *
      *   SCSBW322-COD-LAYOUT          = CODIGO DO LAYOUT              *
      *   SCSBW322-TAM-LAYOUT          = TAMANHO DO LAYOUT             *
      * SCSBW322-REGISTRO.                                             *
      *   SCSBW322-BLOCO-ENTRADA.                                      *
      *     SCSBW322-E-ANO             = ANO RAS                       *
      *     SCSBW322-E-NREG            = NUMERO DO REGISTRO RAS        *
      *     SCSBW322-E-COPER           = NUMERO DA OPERACAO            *
      *   SCSBW322-BLOCO-SAIDA.                                        *
      *     SCSBW322-S-DT-GERACAO      = DATA DE GERACAO DO LOTE       *
      *     SCSBW322-S-DT-TRANSMIS     = DATA TRANSMISSAO DO LOTE      *
      *     SCSBW322-S-DT-PROTOCOLO    = DATA INCLUSAO DO PROTOCOLO    *
      *     SCSBW322-S-NUM-PROTOCOLO   = NUMERO DO PROTOCOLO           *
      *                                                                *
      *----------------------------------------------------------------*
      * DATA       AUTOR             ALTERACAO                         *
      * ---------- ----------------- --------------------------------- *
      * 99/99/9999                   XXXXXXXXXXXXXXXXXXXXXXX           *
      *================================================================*
                                                                        
          05 SCSBW322-HEADER.                                           
             10 SCSBW322-COD-LAYOUT         PIC X(008) VALUE 'SCSBW322'.
             10 SCSBW322-TAM-LAYOUT         PIC 9(005) VALUE 78.        
          05 SCSBW322-REGISTRO.                                         
             10 SCSBW322-BLOCO-ENTRADA.                                 
                15 SCSBW322-E-ANO           PIC 9(004).                 
                15 SCSBW322-E-NREG          PIC 9(009).                 
                15 SCSBW322-E-COPER         PIC 9(010).                 
             10 SCSBW322-BLOCO-SAIDA.                                   
                15 SCSBW322-S-DT-GERACAO    PIC X(010).                 
                15 SCSBW322-S-DT-TRANSMIS   PIC X(010).                 
                15 SCSBW322-S-DT-PROTOCOLO  PIC X(010).                 
                15 SCSBW322-S-NUM-PROTOCOLO PIC X(012).                 

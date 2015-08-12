      *================================================================*
      * NOME BOOK  : SCSBW324                                          *
      * DESCRICAO  : CONSULTAR NO HISTORICO ADITIVO ANTERIOR DA RAS    *
      * COMUNICACAO: FRAMEWORK X COORDENADOR                           *
      * DATA       : 06/02/2015                                        *
      * AUTOR      : FERNANDA CARUSO                                   *
      * EMPRESA    : BRQ IT SERVICES                                   *
      * GRUPO      : 17 - CONTABILIDADE E IMPOSTOS                     *
      * COMPONENTE : SCSB - SISCOSERV BRADESCO                         *
      *================================================================*
      *                                                                *
      * SCSBW324-HEADER.                                               *
      *   SCSBW324-COD-LAYOUT          = CODIGO DO LAYOUT              *
      *   SCSBW324-TAM-LAYOUT          = TAMANHO DO LAYOUT             *
      * SCSBW324-REGISTRO.                                             *
      *   SCSBW324-BLOCO-ENTRADA.                                      *
      *     SCSBW324-E-ANO             = ANO RAS                       *
      *     SCSBW324-E-NREG            = NUMERO DO REGISTRO RAS        *
      *     SCSBW324-E-COPER           = NUMERO DA OPERACAO            *
      *   SCSBW324-BLOCO-SAIDA.                                        *
      *     SCSBW324-S-CD-NBS          = CODIGO NBS                    *
      *     SCSBW324-S-DS-NBS          = DESCRICAO NBS                 *
      *     SCSBW324-S-CD-PAIS-DESTNO  = CODIGO PAIS DE DESTINO        *
      *     SCSBW324-S-DS-PAIS-DESTNO  = DESCRICAO PAIS DE DESTINO     *
      *     SCSBW324-S-CD-MODO-PREST   = CODIGO MODO DE PRESTACAO      *
      *     SCSBW324-S-DS-MODO-PREST   = DESCRICAO MODO DE PRESTACAO   *
      *     SCSBW324-S-DT-INICIO       = DATA DE INICIO                *
      *     SCSBW324-S-DT-CONCLUSAO    = DATA DE CONCLUSAO             *
      *     SCSBW324-S-VALOR           = VALOR DA OPERACAO             *
      *     SCSBW324-S-VALOR-PAGO      = VALOR PAGO                    *
      *     SCSBW324-S-IND-ADITIVO     = INDICADOR ADITIVO (S OU N)    *
      *     SCSBW324-S-SIT-OPER        = SITUACAO DA OPERACAO          *
      *     SCSBW324-S-DS-SIT-OPER     = DESCRICAO SITUACAO OPERACAO   *
      *     SCSBW324-S-TPO-REG         = TIPO DO REGISTRO              *
      *     SCSBW324-S-DT-GERACAO-ARQ  = DATA DE GERACAO DO ARQUIVO    *
      *     SCSBW324-S-DT-TRANSMIS-ARQ = DATA TRANSMISSAO DO ARQUIVO   *
      *     SCSBW324-S-NUM-PROTOCOLO   = NUMERO DO PROTOCOLO           *
      *     SCSBW324-S-QTD-REG         = QTDE REGISTROS DEVOLVIDOS     *
      *     SCSBW324-S-LISTA.                                          *
      *       SCSBW324-S-CD-ENQUA      = CODIGO DO ENQUADRAMENTO       *
      *       SCSBW324-S-DS-ENQUA      = DESCRICAO DO ENQUADRAMENTO    *
      *                                                                *
      *----------------------------------------------------------------*
      * DATA       AUTOR             ALTERACAO                         *
      * ---------- ----------------- --------------------------------- *
      * 99/99/9999                   XXXXXXXXXXXXXXXXXXXXXXX           *
      *================================================================*
                                                                        
          05 SCSBW324-HEADER.                                           
             10 SCSBW324-COD-LAYOUT         PIC X(008) VALUE 'SCSBW324'.
             10 SCSBW324-TAM-LAYOUT         PIC 9(005) VALUE 6047.      
          05 SCSBW324-REGISTRO.                                         
             10 SCSBW324-BLOCO-ENTRADA.                                 
                15 SCSBW324-E-ANO                PIC 9(004).            
                15 SCSBW324-E-NREG               PIC 9(009).            
                15 SCSBW324-E-COPER              PIC 9(010).            
             10 SCSBW324-BLOCO-SAIDA.                                   
                15 SCSBW324-S-CD-NBS             PIC X(009).            
                15 SCSBW324-S-DS-NBS             PIC X(250).            
                15 SCSBW324-S-CD-PAIS-DESTNO     PIC 9(003).            
                15 SCSBW324-S-DS-PAIS-DESTNO     PIC X(060).            
                15 SCSBW324-S-CD-MODO-PREST      PIC 9(001).            
                15 SCSBW324-S-DS-MODO-PREST      PIC X(050).            
                15 SCSBW324-S-DT-INICIO          PIC X(010).            
                15 SCSBW324-S-DT-CONCLUSAO       PIC X(010).            
                15 SCSBW324-S-VALOR              PIC 9(013)V99.         
                15 SCSBW324-S-VALOR-PAGO         PIC 9(013)V99.         
                15 SCSBW324-S-IND-ADITIVO        PIC X(001).            
                15 SCSBW324-S-SIT-OPER           PIC X(001).            
                15 SCSBW324-S-DS-SIT-OPER        PIC X(050).            
                15 SCSBW324-S-TPO-REG            PIC 9(001).            
                15 SCSBW324-S-DT-GERACAO-ARQ     PIC X(010).            
                15 SCSBW324-S-DT-TRANSMIS-ARQ    PIC X(010).            
                15 SCSBW324-S-NUM-PROTOCOLO      PIC X(012).            
                15 SCSBW324-S-QTD-REG            PIC 9(003).            
                15 SCSBW324-S-LISTA              OCCURS 0 TO 050 TIMES  
                      DEPENDING ON SCSBW324-S-QTD-REG.                  
                   20 SCSBW324-S-CD-ENQUA        PIC 9(010).            
                   20 SCSBW324-S-DS-ENQUA        PIC X(100).            

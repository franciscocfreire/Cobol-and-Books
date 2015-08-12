      *================================================================*
      * NOME BOOK  : SCSBW304                                          *
      * DESCRICAO  : CONSULTAR NO HISTORICO ADITIVO ANTERIOR DA RVS    *
      * COMUNICACAO: FRAMEWORK X COORDENADOR                           *
      * DATA       : 30/01/2015                                        *
      * AUTOR      : FERNANDA CARUSO                                   *
      * EMPRESA    : BRQ IT SERVICES                                   *
      * GRUPO      : 17 - CONTABILIDADE E IMPOSTOS                     *
      * COMPONENTE : SCSB - SISCOSERV BRADESCO                         *
      *================================================================*
      *                                                                *
      * SCSBW304-HEADER.                                               *
      *   SCSBW304-COD-LAYOUT          = CODIGO DO LAYOUT              *
      *   SCSBW304-TAM-LAYOUT          = TAMANHO DO LAYOUT             *
      * SCSBW304-REGISTRO.                                             *
      *   SCSBW304-BLOCO-ENTRADA.                                      *
      *     SCSBW304-E-ANO             = ANO RVS                       *
      *     SCSBW304-E-NREG            = NUMERO DO REGISTRO RVS        *
      *     SCSBW304-E-COPER           = NUMERO DA OPERACAO            *
      *   SCSBW304-BLOCO-SAIDA.                                        *
      *     SCSBW304-S-CD-NBS          = CODIGO NBS                    *
      *     SCSBW304-S-DS-NBS          = DESCRICAO NBS                 *
      *     SCSBW304-S-CD-PAIS-DESTNO  = CODIGO PAIS DE DESTINO        *
      *     SCSBW304-S-DS-PAIS-DESTNO  = DESCRICAO PAIS DE DESTINO     *
      *     SCSBW304-S-CD-MODO-PREST   = CODIGO MODO DE PRESTACAO      *
      *     SCSBW304-S-DS-MODO-PREST   = DESCRICAO MODO DE PRESTACAO   *
      *     SCSBW304-S-DT-INICIO       = DATA DE INICIO                *
      *     SCSBW304-S-DT-CONCLUSAO    = DATA DE CONCLUSAO             *
      *     SCSBW304-S-VALOR           = VALOR DA OPERACAO             *
      *     SCSBW304-S-VALOR-FATURADO  = VALOR FATURADO                *
      *     SCSBW304-S-IND-ADITIVO     = INDICADOR ADITIVO (S OU N)    *
      *     SCSBW304-S-SIT-OPER        = SITUACAO DO REGISTRO          *
      *     SCSBW304-S-DS-SIT-OPER     = DESCRICAO SITUACAO OPERACAO   *
      *     SCSBW304-S-TPO-REG         = TIPO DO REGISTRO              *
      *     SCSBW304-S-DT-GERACAO-ARQ  = DATA DE GERACAO DO ARQUIVO    *
      *     SCSBW304-S-DT-TRANSMIS-ARQ = DATA TRANSMISSAO DO ARQUIVO   *
      *     SCSBW304-S-NUM-PROTOCOLO   = NUMERO DO PROTOCOLO           *
      *     SCSBW034-S-QTD-REG         = QTDE REGISTROS DEVOLVIDOS     *
      *     SCSBW034-S-LISTA.                                          *
      *       SCSBW034-S-CD-ENQUA      = CODIGO DO ENQUADRAMENTO       *
      *       SCSBW034-S-DS-ENQUA      = DESCRICAO DO ENQUADRAMENTO    *
      *       SCSBW034-S-REG-CREDT     = REGISTRO DE CREDITO (RC)      *
      *                                                                *
      *----------------------------------------------------------------*
      * DATA       AUTOR             ALTERACAO                         *
      * ---------- ----------------- --------------------------------- *
      * 99/99/9999                   XXXXXXXXXXXXXXXXXXXXXXX           *
      *================================================================*
                                                                        
          05 SCSBW304-HEADER.                                           
             10 SCSBW304-COD-LAYOUT         PIC X(008) VALUE 'SCSBW304'.
             10 SCSBW304-TAM-LAYOUT         PIC 9(005) VALUE 6447.      
          05 SCSBW304-REGISTRO.                                         
             10 SCSBW304-BLOCO-ENTRADA.                                 
                15 SCSBW304-E-ANO                PIC 9(004).            
                15 SCSBW304-E-NREG               PIC 9(009).            
                15 SCSBW304-E-COPER              PIC 9(010).            
             10 SCSBW304-BLOCO-SAIDA.                                   
                15 SCSBW304-S-CD-NBS             PIC X(009).            
                15 SCSBW304-S-DS-NBS             PIC X(250).            
                15 SCSBW304-S-CD-PAIS-DESTNO     PIC 9(003).            
                15 SCSBW304-S-DS-PAIS-DESTNO     PIC X(060).            
                15 SCSBW304-S-CD-MODO-PREST      PIC 9(001).            
                15 SCSBW304-S-DS-MODO-PREST      PIC X(050).            
                15 SCSBW304-S-DT-INICIO          PIC X(010).            
                15 SCSBW304-S-DT-CONCLUSAO       PIC X(010).            
                15 SCSBW304-S-VALOR              PIC 9(013)V99.         
                15 SCSBW304-S-VALOR-FATURADO     PIC 9(013)V99.         
                15 SCSBW304-S-IND-ADITIVO        PIC X(001).            
                15 SCSBW304-S-SIT-REG            PIC X(001).            
                15 SCSBW304-S-DS-SIT-OPER        PIC X(050).            
                15 SCSBW304-S-TPO-REG            PIC 9(001).            
                15 SCSBW304-S-DT-GERACAO-ARQ     PIC X(010).            
                15 SCSBW304-S-DT-TRANSMIS-ARQ    PIC X(010).            
                15 SCSBW304-S-NUM-PROTOCOLO      PIC X(012).            
                15 SCSBW304-S-QTD-REG            PIC 9(003).            
                15 SCSBW304-S-LISTA              OCCURS 0 TO 050 TIMES  
                      DEPENDING ON SCSBW304-S-QTD-REG.                  
                   20 SCSBW304-S-CD-ENQUA        PIC 9(010).            
                   20 SCSBW304-S-DS-ENQUA        PIC X(100).            
                   20 SCSBW304-S-REG-CREDT       PIC 9(008).            

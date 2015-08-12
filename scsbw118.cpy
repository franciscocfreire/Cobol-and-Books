      *================================================================*
      * NOME BOOK  : SCSBW118                                          *
      * DESCRICAO  : RETIFICAR ADITIVO RVS                             *
      * COMUNICACAO: FRAMEWORK X COORDENADOR                           *
      * DATA       : 14/06/2013                                        *
      * AUTOR      : FERNANDA CARUSO                                   *
      * EMPRESA    : BRQ IT SERVICES                                   *
      * GRUPO      : 17 - CONTABILIDADE E IMPOSTOS                     *
      * COMPONENTE : SCSB - SISCOSERV BRADESCO                         *
      *================================================================*
      *                                                                *
      * SCSBW118-HEADER.                                               *
      *   SCSBW118-COD-LAYOUT            = CODIGO DO LAYOUT            *
      *   SCSBW118-TAM-LAYOUT            = TAMANHO DO LAYOUT           *
      * SCSBW118-REGISTRO.                                             *
      *   SCSBW118-BLOCO-ENTRADA.                                      *
      *     SCSBW118-E-ANO               = ANO RVS                     *
      *     SCSBW118-E-NREG              = NUMERO DO REGISTRO RVS      *
      *     SCSBW118-E-INF-COMPLEM       = INFORMACOES COMPLEMENTARES  *
      *     SCSBW118-E-QTD-REG-OPER      = QTDE REGISTROS DE OPERACOES *
      *     SCSBW118-E-LISTA-OPER.                                     *
      *       SCSBW118-E-COPER-ADITIVO   = NUMERO OPERACAO DE ADITIVO  *
      *       SCSBW118-E-CD-NBS          = CODIGO NBS                  *
      *       SCSBW118-E-CD-PAIS-DESTNO  = CODIGO PAIS DE DESTINO      *
      *       SCSBW118-E-CD-MODO-PREST   = CODIGO MODO DE PRESTACAO    *
      *       SCSBW118-E-DT-INICIO       = DATA DE INICIO              *
      *       SCSBW118-E-DT-CONCLUSAO    = DATA DE CONCLUSAO           *
      *       SCSBW118-E-VALOR           = VALOR DA OPERACAO           *
      *       SCSBW118-E-QTD-REG-ENQUA   = QTDE REG'S DE ENQUADRAMENTO *
      *       SCSBW118-E-LISTA-ENQUA.                                  *
      *         SCSBW118-E-ACAO          = ACAO (I=INCLUIR / E=EXCLUIR)*
      *         SCSBW118-E-CD-ENQUA      = CODIGO DO ENQUADRAMENTO     *
      *         SCSBW118-E-RC            = REGISTRO DE CREDITO         *
      *                                                                *
      *----------------------------------------------------------------*
      * DATA       AUTOR             ALTERACAO                         *
      * ---------- ----------------- --------------------------------- *
      * 29/01/2015 FERNANDA CARUSO   ALTERACAO DO CAMPO SCSBW118-E-RC  *
      *                              DE X(06) PARA 9(08).              *
      *================================================================*
                                                                        
          05 SCSBW118-HEADER.                                           
             10 SCSBW118-COD-LAYOUT         PIC X(008) VALUE 'SCSBW118'.
             10 SCSBW118-TAM-LAYOUT         PIC 9(005) VALUE 25249.     
          05 SCSBW118-REGISTRO.                                         
             10 SCSBW118-BLOCO-ENTRADA.                                 
                15 SCSBW118-E-ANO                PIC 9(004).            
                15 SCSBW118-E-NREG               PIC 9(009).            
                15 SCSBW118-E-INF-COMPLEM        PIC X(5000).           
                15 SCSBW118-E-QTD-REG-OPER       PIC 9(003).            
                15 SCSBW118-E-LISTA-OPER         OCCURS 020 TIMES.      
                   20 SCSBW118-E-COPER-ADITIVO   PIC 9(010).            
                   20 SCSBW118-E-CD-NBS          PIC X(009).            
                   20 SCSBW118-E-CD-PAIS-DESTNO  PIC 9(003).            
                   20 SCSBW118-E-CD-MODO-PREST   PIC 9(001).            
                   20 SCSBW118-E-DT-INICIO       PIC X(010).            
                   20 SCSBW118-E-DT-CONCLUSAO    PIC X(010).            
                   20 SCSBW118-E-VALOR           PIC 9(013)V99.         
                   20 SCSBW118-E-QTD-REG-ENQUA   PIC 9(003).            
                   20 SCSBW118-E-LISTA-ENQUA     OCCURS 050 TIMES.      
                      25 SCSBW118-E-ACAO         PIC X(001).            
                      25 SCSBW118-E-CD-ENQUA     PIC 9(010).            
                      25 SCSBW118-E-RC           PIC 9(008).            

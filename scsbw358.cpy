      *================================================================*
      * NOME BOOK  : SCSBW358                                          *
      * DESCRICAO  : ALTERAR RETIFICACAO DE ADITIVO DE RAS             *
      * COMUNICACAO: FRAMEWORK X COORDENADOR                           *
      * DATA       : 11/02/2015                                        *
      * AUTOR      : FERNANDA CARUSO                                   *
      * EMPRESA    : BRQ IT SERVICES                                   *
      * GRUPO      : 17 - CONTABILIDADE E IMPOSTOS                     *
      * COMPONENTE : SCSB - SISCOSERV BRADESCO                         *
      *================================================================*
      *                                                                *
      * SCSBW358-HEADER.                                               *
      *   SCSBW358-COD-LAYOUT            = CODIGO DO LAYOUT            *
      *   SCSBW358-TAM-LAYOUT            = TAMANHO DO LAYOUT           *
      * SCSBW358-REGISTRO.                                             *
      *   SCSBW358-BLOCO-ENTRADA.                                      *
      *     SCSBW358-E-ANO               = ANO RAS                     *
      *     SCSBW358-E-NREG              = NUMERO DO REGISTRO RAS      *
      *     SCSBW358-E-INF-COMPLEM       = INFORMACOES COMPLEMENTARES  *
      *     SCSBW358-E-QTD-REG-OPER      = QTDE REGISTROS DE OPERACOES *
      *     SCSBW358-E-LISTA-OPER.                                     *
      *       SCSBW358-E-COPER-ADITIVO   = NUMERO OPERACAO DE ADITIVO  *
      *       SCSBW358-E-CD-NBS          = CODIGO NBS                  *
      *       SCSBW358-E-CD-PAIS-DESTNO  = CODIGO PAIS DE DESTINO      *
      *       SCSBW358-E-CD-MODO-PREST   = CODIGO MODO DE PRESTACAO    *
      *       SCSBW358-E-DT-INICIO       = DATA DE INICIO              *
      *       SCSBW358-E-DT-CONCLUSAO    = DATA DE CONCLUSAO           *
      *       SCSBW358-E-VALOR           = VALOR DA OPERACAO           *
      *       SCSBW358-E-QTD-REG-ENQUA   = QTDE REG'S DE ENQUADRAMENTO *
      *       SCSBW358-E-LISTA-ENQUA.                                  *
      *         SCSBW358-E-CD-ENQUA      = CODIGO DO ENQUADRAMENTO     *
      *                                                                *
      *----------------------------------------------------------------*
      * DATA       AUTOR             ALTERACAO                         *
      * ---------- ----------------- --------------------------------- *
      * 99/99/9999                   XXXXXXXXXXXXXXXXXXXXXXX           *
      *================================================================*
                                                                        
          05 SCSBW358-HEADER.                                           
             10 SCSBW358-COD-LAYOUT         PIC X(008) VALUE 'SCSBW358'.
             10 SCSBW358-TAM-LAYOUT         PIC 9(005) VALUE 16249.     
          05 SCSBW358-REGISTRO.                                         
             10 SCSBW358-BLOCO-ENTRADA.                                 
                15 SCSBW358-E-ANO                PIC 9(004).            
                15 SCSBW358-E-NREG               PIC 9(009).            
                15 SCSBW358-E-INF-COMPLEM        PIC X(5000).           
                15 SCSBW358-E-QTD-REG-OPER       PIC 9(003).            
                15 SCSBW358-E-LISTA-OPER         OCCURS 020 TIMES.      
                   20 SCSBW358-E-COPER-ADITIVO   PIC 9(010).            
                   20 SCSBW358-E-CD-NBS          PIC X(009).            
                   20 SCSBW358-E-CD-PAIS-DESTNO  PIC 9(003).            
                   20 SCSBW358-E-CD-MODO-PREST   PIC 9(001).            
                   20 SCSBW358-E-DT-INICIO       PIC X(010).            
                   20 SCSBW358-E-DT-CONCLUSAO    PIC X(010).            
                   20 SCSBW358-E-VALOR           PIC 9(013)V99.         
                   20 SCSBW358-E-QTD-REG-ENQUA   PIC 9(003).            
                   20 SCSBW358-E-LISTA-ENQUA     OCCURS 050 TIMES.      
                      25 SCSBW358-E-CD-ENQUA     PIC 9(010).            

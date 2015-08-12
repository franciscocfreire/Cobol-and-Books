      *================================================================*
      * NOME BOOK  : SCSBW196                                          *
      * DESCRICAO  : ALTERAR ADITIVO DE RVS                            *
      * COMUNICACAO: FRAMEWORK X COORDENADOR                           *
      * DATA       : 28/01/2015                                        *
      * AUTOR      : FERNANDA CARUSO                                   *
      * EMPRESA    : BRQ IT SERVICES                                   *
      * GRUPO      : 17 - CONTABILIDADE E IMPOSTOS                     *
      * COMPONENTE : SCSB - SISCOSERV BRADESCO                         *
      *================================================================*
      *                                                                *
      * SCSBW196-HEADER.                                               *
      *   SCSBW196-COD-LAYOUT            = CODIGO DO LAYOUT            *
      *   SCSBW196-TAM-LAYOUT            = TAMANHO DO LAYOUT           *
      * SCSBW196-REGISTRO.                                             *
      *   SCSBW196-BLOCO-ENTRADA.                                      *
      *     SCSBW196-E-ANO               = ANO RVS                     *
      *     SCSBW196-E-NREG              = NUMERO DO REGISTRO RVS      *
      *     SCSBW196-E-QTD-REG-OPER      = QTDE REGISTROS DE OPERACOES *
      *     SCSBW196-E-LISTA-OPER.                                     *
      *       SCSBW196-E-COPER-ADITIVO   = NUMERO OPERACAO DE ADITIVO  *
      *       SCSBW196-E-CD-NBS          = CODIGO NBS                  *
      *       SCSBW196-E-CD-PAIS-DESTNO  = CODIGO PAIS DE DESTINO      *
      *       SCSBW196-E-CD-MODO-PREST   = CODIGO MODO DE PRESTACAO    *
      *       SCSBW196-E-DT-INICIO       = DATA DE INICIO              *
      *       SCSBW196-E-DT-CONCLUSAO    = DATA DE CONCLUSAO           *
      *       SCSBW196-E-VALOR           = VALOR DA OPERACAO           *
      *       SCSBW196-E-QTD-REG-ENQUA   = QTDE REG'S DE ENQUADRAMENTO *
      *       SCSBW196-E-LISTA-ENQUA.                                  *
      *         SCSBW196-E-CD-ENQUA      = CODIGO DO ENQUADRAMENTO     *
      *         SCSBW196-E-RC            = REGISTRO DE CREDITO         *
      *                                                                *
      *----------------------------------------------------------------*
      * DATA       AUTOR             ALTERACAO                         *
      * ---------- ----------------- --------------------------------- *
      * 99/99/9999                   XXXXXXXXXXXXXXXXXXXXXXX           *
      *================================================================*
                                                                        
          05 SCSBW196-HEADER.                                           
             10 SCSBW196-COD-LAYOUT         PIC X(008) VALUE 'SCSBW196'.
             10 SCSBW196-TAM-LAYOUT         PIC 9(005) VALUE 19249.     
          05 SCSBW196-REGISTRO.                                         
             10 SCSBW196-BLOCO-ENTRADA.                                 
                15 SCSBW196-E-ANO                PIC 9(004).            
                15 SCSBW196-E-NREG               PIC 9(009).            
                15 SCSBW196-E-QTD-REG-OPER       PIC 9(003).            
                15 SCSBW196-E-LISTA-OPER         OCCURS 020 TIMES.      
                   20 SCSBW196-E-COPER-ADITIVO   PIC 9(010).            
                   20 SCSBW196-E-CD-NBS          PIC X(009).            
                   20 SCSBW196-E-CD-PAIS-DESTNO  PIC 9(003).            
                   20 SCSBW196-E-CD-MODO-PREST   PIC 9(001).            
                   20 SCSBW196-E-DT-INICIO       PIC X(010).            
                   20 SCSBW196-E-DT-CONCLUSAO    PIC X(010).            
                   20 SCSBW196-E-VALOR           PIC 9(013)V99.         
                   20 SCSBW196-E-QTD-REG-ENQUA   PIC 9(003).            
                   20 SCSBW196-E-LISTA-ENQUA     OCCURS 050 TIMES.      
                      25 SCSBW196-E-CD-ENQUA     PIC 9(010).            
                      25 SCSBW196-E-RC           PIC 9(008).            

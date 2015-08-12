      *================================================================*
      * NOME BOOK  : SCSBW354                                          *
      * DESCRICAO  : ALTERAR ADITIVO DE RAS                            *
      * COMUNICACAO: FRAMEWORK X COORDENADOR                           *
      * DATA       : 10/02/2015                                        *
      * AUTOR      : FERNANDA CARUSO                                   *
      * EMPRESA    : BRQ IT SERVICES                                   *
      * GRUPO      : 17 - CONTABILIDADE E IMPOSTOS                     *
      * COMPONENTE : SCSB - SISCOSERV BRADESCO                         *
      *================================================================*
      *                                                                *
      * SCSBW354-HEADER.                                               *
      *   SCSBW354-COD-LAYOUT            = CODIGO DO LAYOUT            *
      *   SCSBW354-TAM-LAYOUT            = TAMANHO DO LAYOUT           *
      * SCSBW354-REGISTRO.                                             *
      *   SCSBW354-BLOCO-ENTRADA.                                      *
      *     SCSBW354-E-ANO               = ANO RAS                     *
      *     SCSBW354-E-NREG              = NUMERO DO REGISTRO RAS      *
      *     SCSBW354-E-QTD-REG-OPER      = QTDE REGISTROS DE OPERACOES *
      *     SCSBW354-E-LISTA-OPER.                                     *
      *       SCSBW354-E-COPER-ADITIVO   = NUMERO OPERACAO DE ADITIVO  *
      *       SCSBW354-E-CD-NBS          = CODIGO NBS                  *
      *       SCSBW354-E-CD-PAIS-DESTNO  = CODIGO PAIS DE DESTINO      *
      *       SCSBW354-E-CD-MODO-PREST   = CODIGO MODO DE PRESTACAO    *
      *       SCSBW354-E-DT-INICIO       = DATA DE INICIO              *
      *       SCSBW354-E-DT-CONCLUSAO    = DATA DE CONCLUSAO           *
      *       SCSBW354-E-VALOR           = VALOR DA OPERACAO           *
      *       SCSBW354-E-QTD-REG-ENQUA   = QTDE REG'S DE ENQUADRAMENTO *
      *       SCSBW354-E-LISTA-ENQUA.                                  *
      *         SCSBW354-E-CD-ENQUA      = CODIGO DO ENQUADRAMENTO     *
      *                                                                *
      *----------------------------------------------------------------*
      * DATA       AUTOR             ALTERACAO                         *
      * ---------- ----------------- --------------------------------- *
      * 99/99/9999                   XXXXXXXXXXXXXXXXXXXXXXX           *
      *================================================================*
                                                                        
          05 SCSBW354-HEADER.                                           
             10 SCSBW354-COD-LAYOUT         PIC X(008) VALUE 'SCSBW354'.
             10 SCSBW354-TAM-LAYOUT         PIC 9(005) VALUE 11249.     
          05 SCSBW354-REGISTRO.                                         
             10 SCSBW354-BLOCO-ENTRADA.                                 
                15 SCSBW354-E-ANO                PIC 9(004).            
                15 SCSBW354-E-NREG               PIC 9(009).            
                15 SCSBW354-E-QTD-REG-OPER       PIC 9(003).            
                15 SCSBW354-E-LISTA-OPER         OCCURS 020 TIMES.      
                   20 SCSBW354-E-COPER-ADITIVO   PIC 9(010).            
                   20 SCSBW354-E-CD-NBS          PIC X(009).            
                   20 SCSBW354-E-CD-PAIS-DESTNO  PIC 9(003).            
                   20 SCSBW354-E-CD-MODO-PREST   PIC 9(001).            
                   20 SCSBW354-E-DT-INICIO       PIC X(010).            
                   20 SCSBW354-E-DT-CONCLUSAO    PIC X(010).            
                   20 SCSBW354-E-VALOR           PIC 9(013)V99.         
                   20 SCSBW354-E-QTD-REG-ENQUA   PIC 9(003).            
                   20 SCSBW354-E-LISTA-ENQUA     OCCURS 050 TIMES.      
                      25 SCSBW354-E-CD-ENQUA     PIC 9(010).            

      *================================================================*
      * NOME BOOK  : SCSBW300                                          *
      * DESCRICAO  : ALTERAR RETIFICACAO DE ADITIVO DE RVS             *
      * COMUNICACAO: FRAMEWORK X COORDENADOR                           *
      * DATA       : 30/01/2015                                        *
      * AUTOR      : FERNANDA CARUSO                                   *
      * EMPRESA    : BRQ IT SERVICES                                   *
      * GRUPO      : 17 - CONTABILIDADE E IMPOSTOS                     *
      * COMPONENTE : SCSB - SISCOSERV BRADESCO                         *
      *================================================================*
      *                                                                *
      * SCSBW300-HEADER.                                               *
      *   SCSBW300-COD-LAYOUT            = CODIGO DO LAYOUT            *
      *   SCSBW300-TAM-LAYOUT            = TAMANHO DO LAYOUT           *
      * SCSBW300-REGISTRO.                                             *
      *   SCSBW300-BLOCO-ENTRADA.                                      *
      *     SCSBW300-E-ANO               = ANO RVS                     *
      *     SCSBW300-E-NREG              = NUMERO DO REGISTRO RVS      *
      *     SCSBW300-E-INF-COMPLEM       = INFORMACOES COMPLEMENTARES  *
      *     SCSBW300-E-QTD-REG-OPER      = QTDE REGISTROS DE OPERACOES *
      *     SCSBW300-E-LISTA-OPER.                                     *
      *       SCSBW300-E-COPER-ADITIVO   = NUMERO OPERACAO DE ADITIVO  *
      *       SCSBW300-E-CD-NBS          = CODIGO NBS                  *
      *       SCSBW300-E-CD-PAIS-DESTNO  = CODIGO PAIS DE DESTINO      *
      *       SCSBW300-E-CD-MODO-PREST   = CODIGO MODO DE PRESTACAO    *
      *       SCSBW300-E-DT-INICIO       = DATA DE INICIO              *
      *       SCSBW300-E-DT-CONCLUSAO    = DATA DE CONCLUSAO           *
      *       SCSBW300-E-VALOR           = VALOR DA OPERACAO           *
      *       SCSBW300-E-QTD-REG-ENQUA   = QTDE REG'S DE ENQUADRAMENTO *
      *       SCSBW300-E-LISTA-ENQUA.                                  *
      *         SCSBW300-E-CD-ENQUA      = CODIGO DO ENQUADRAMENTO     *
      *         SCSBW300-E-RC            = REGISTRO DE CREDITO         *
      *                                                                *
      *----------------------------------------------------------------*
      * DATA       AUTOR             ALTERACAO                         *
      * ---------- ----------------- --------------------------------- *
      * 99/99/9999                   XXXXXXXXXXXXXXXXXXXXXXX           *
      *================================================================*
                                                                        
          05 SCSBW300-HEADER.                                           
             10 SCSBW300-COD-LAYOUT         PIC X(008) VALUE 'SCSBW300'.
             10 SCSBW300-TAM-LAYOUT         PIC 9(005) VALUE 24249.     
          05 SCSBW300-REGISTRO.                                         
             10 SCSBW300-BLOCO-ENTRADA.                                 
                15 SCSBW300-E-ANO                PIC 9(004).            
                15 SCSBW300-E-NREG               PIC 9(009).            
                15 SCSBW300-E-INF-COMPLEM        PIC X(5000).           
                15 SCSBW300-E-QTD-REG-OPER       PIC 9(003).            
                15 SCSBW300-E-LISTA-OPER         OCCURS 020 TIMES.      
                   20 SCSBW300-E-COPER-ADITIVO   PIC 9(010).            
                   20 SCSBW300-E-CD-NBS          PIC X(009).            
                   20 SCSBW300-E-CD-PAIS-DESTNO  PIC 9(003).            
                   20 SCSBW300-E-CD-MODO-PREST   PIC 9(001).            
                   20 SCSBW300-E-DT-INICIO       PIC X(010).            
                   20 SCSBW300-E-DT-CONCLUSAO    PIC X(010).            
                   20 SCSBW300-E-VALOR           PIC 9(013)V99.         
                   20 SCSBW300-E-QTD-REG-ENQUA   PIC 9(003).            
                   20 SCSBW300-E-LISTA-ENQUA     OCCURS 050 TIMES.      
                      25 SCSBW300-E-CD-ENQUA     PIC 9(010).            
                      25 SCSBW300-E-RC           PIC 9(008).            

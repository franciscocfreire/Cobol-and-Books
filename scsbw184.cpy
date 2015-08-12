      *================================================================*
      * NOME BOOK  : SCSBW184                                          *
      * DESCRICAO  : ALTERAR RETIFICACAO DA RVS                        *
      * COMUNICACAO: FRAMEWORK X COORDENADOR                           *
      * DATA       : 24/01/2015                                        *
      * AUTOR      : FERNANDA CARUSO                                   *
      * EMPRESA    : BRQ IT SERVICES                                   *
      * GRUPO      : 17 - CONTABILIDADE E IMPOSTOS                     *
      * COMPONENTE : SCSB - SISCOSERV BRADESCO                         *
      *================================================================*
      *                                                                *
      * SCSBW184-HEADER.                                               *
      *   SCSBW184-COD-LAYOUT            = CODIGO DO LAYOUT            *
      *   SCSBW184-TAM-LAYOUT            = TAMANHO DO LAYOUT           *
      * SCSBW184-REGISTRO.                                             *
      *   SCSBW184-BLOCO-ENTRADA.                                      *
      *     SCSBW184-E-ANO               = ANO RVS                     *
      *     SCSBW184-E-NREG              = NUMERO DO REGISTRO RVS      *
      *     SCSBW184-E-EMPR-RVS          = EMPRESA DO REGISTRO RVS     *
      *     SCSBW184-E-NOME-ADQUIR       = NOME DO ADQUIRENTE          *
      *     SCSBW184-E-END-ADQUIR        = ENDERECO DO ADQUIRENTE      *
      *     SCSBW184-E-CD-PAIS-ADQUIR    = CODIGO DO PAIS ADQUIRENTE   *
      *     SCSBW184-E-NIF               = NUMERO IDENTIFICACAO FISCAL *
      *     SCSBW184-E-CD-MOEDA          = CODIGO DA MOEDA             *
      *     SCSBW184-E-VLR-TOT-CONVERT   = VALOR TOTAL CONVERTIDO REAL *
      *     SCSBW184-E-INF-COMPLEM       = INFORMACOES COMPLEMENTARES  *
      *     SCSBW184-E-QTD-REG-OPER      = QTDE REGISTROS DE OPERACOES *
      *     SCSBW184-E-LISTA-OPER.                                     *
      *       SCSBW184-E-COPER           = CODIGO DA OPERACAO          *
      *       SCSBW184-E-CD-NBS          = CODIGO NBS                  *
      *       SCSBW184-E-CD-PAIS-DESTNO  = CODIGO PAIS DE DESTINO      *
      *       SCSBW184-E-CD-MODO-PREST   = CODIGO MODO DE PRESTACAO    *
      *       SCSBW184-E-DT-INICIO       = DATA DE INICIO              *
      *       SCSBW184-E-DT-CONCLUSAO    = DATA DE CONCLUSAO           *
      *       SCSBW184-E-VALOR           = VALOR DA OPERACAO           *
      *       SCSBW184-E-QTD-REG-ENQUA   = QTDE REG'S DE ENQUADRAMENTO *
      *       SCSBW184-E-LISTA-ENQUA.                                  *
      *         SCSBW184-E-CD-ENQUA      = CODIGO DO ENQUADRAMENTO     *
      *         SCSBW184-E-RC            = REGISTRO DE CREDITO         *
      *                                                                *
      *----------------------------------------------------------------*
      * DATA       AUTOR             ALTERACAO                         *
      * ---------- ----------------- --------------------------------- *
      * 99/99/9999                   XXXXXXXXXXXXXXXXXXXXXXX           *
      *================================================================*
                                                                        
          05 SCSBW184-HEADER.                                           
             10 SCSBW184-COD-LAYOUT         PIC X(008) VALUE 'SCSBW184'.
             10 SCSBW184-TAM-LAYOUT         PIC 9(005) VALUE 24622.     
          05 SCSBW184-REGISTRO.                                         
             10 SCSBW184-BLOCO-ENTRADA.                                 
                15 SCSBW184-E-ANO                PIC 9(004).            
                15 SCSBW184-E-NREG               PIC 9(009).            
                15 SCSBW184-E-EMPR-RVS           PIC 9(010).            
                15 SCSBW184-E-NOME-ADQUIR        PIC X(150).            
                15 SCSBW184-E-END-ADQUIR         PIC X(150).            
                15 SCSBW184-E-CD-PAIS-ADQUIR     PIC 9(003).            
                15 SCSBW184-E-NIF                PIC X(040).            
                15 SCSBW184-E-CD-MOEDA           PIC 9(005).            
                15 SCSBW184-E-VLR-TOT-CONVERT    PIC 9(013)V99.         
                15 SCSBW184-E-INF-COMPLEM        PIC X(5000).           
                15 SCSBW184-E-QTD-REG-OPER       PIC 9(003).            
                15 SCSBW184-E-LISTA-OPER         OCCURS 020 TIMES.      
                   20 SCSBW184-E-COPER           PIC 9(010).            
                   20 SCSBW184-E-CD-NBS          PIC X(009).            
                   20 SCSBW184-E-CD-PAIS-DESTNO  PIC 9(003).            
                   20 SCSBW184-E-CD-MODO-PREST   PIC 9(001).            
                   20 SCSBW184-E-DT-INICIO       PIC X(010).            
                   20 SCSBW184-E-DT-CONCLUSAO    PIC X(010).            
                   20 SCSBW184-E-VALOR           PIC 9(013)V99.         
                   20 SCSBW184-E-QTD-REG-ENQUA   PIC 9(003).            
                   20 SCSBW184-E-LISTA-ENQUA     OCCURS 050 TIMES.      
                      25 SCSBW184-E-CD-ENQUA     PIC 9(010).            
                      25 SCSBW184-E-RC           PIC 9(008).            

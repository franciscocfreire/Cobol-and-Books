      *================================================================*
      * NOME BOOK  : SCSBW168                                          *
      * DESCRICAO  : ALTERAR RVS                                       *
      * COMUNICACAO: FRAMEWORK X COORDENADOR                           *
      * DATA       : 15/01/2015                                        *
      * AUTOR      : FERNANDA CARUSO                                   *
      * EMPRESA    : BRQ IT SERVICES                                   *
      * GRUPO      : 17 - CONTABILIDADE E IMPOSTOS                     *
      * COMPONENTE : SCSB - SISCOSERV BRADESCO                         *
      *================================================================*
      *                                                                *
      * SCSBW168-HEADER.                                               *
      *   SCSBW168-COD-LAYOUT            = CODIGO DO LAYOUT            *
      *   SCSBW168-TAM-LAYOUT            = TAMANHO DO LAYOUT           *
      * SCSBW168-REGISTRO.                                             *
      *   SCSBW168-BLOCO-ENTRADA.                                      *
      *     SCSBW168-E-ANO               = ANO RVS                     *
      *     SCSBW168-E-NREG              = NUMERO DO REGISTRO RVS      *
      *     SCSBW168-E-EMPR-RVS          = EMPRESA DO REGISTRO RVS     *
      *     SCSBW168-E-NOME-ADQUIR       = NOME DO ADQUIRENTE          *
      *     SCSBW168-E-END-ADQUIR        = ENDERECO DO ADQUIRENTE      *
      *     SCSBW168-E-CD-PAIS-ADQUIR    = CODIGO DO PAIS ADQUIRENTE   *
      *     SCSBW168-E-NIF               = NUMERO IDENTIFICACAO FISCAL *
      *     SCSBW168-E-CD-MOEDA          = CODIGO DA MOEDA             *
      *     SCSBW168-E-VLR-TOT-CONVERT   = VALOR TOTAL CONVERTIDO REAL *
      *     SCSBW168-E-SIT-REG           = SITUACAO DO REGISTRO        *
      *     SCSBW168-E-INF-COMPLEM       = INFORMACOES COMPLEMENTARES  *
      *     SCSBW168-E-QTD-REG-OPER      = QTDE REGISTROS DE OPERACOES *
      *     SCSBW168-E-LISTA-OPER.                                     *
      *       SCSBW168-E-ACAO            = ACAO                        *
      *                                    A -> ALTERAR                *
      *                                    E -> EXCLUIR                *
      *                                    I -> INCLUIR                *
      *       SCSBW168-E-COPER           = CODIGO DA OPERACAO          *
      *       SCSBW168-E-CD-NBS          = CODIGO NBS                  *
      *       SCSBW168-E-CD-PAIS-DESTNO  = CODIGO PAIS DE DESTINO      *
      *       SCSBW168-E-CD-MODO-PREST   = CODIGO MODO DE PRESTACAO    *
      *       SCSBW168-E-DT-INICIO       = DATA DE INICIO              *
      *       SCSBW168-E-DT-CONCLUSAO    = DATA DE CONCLUSAO           *
      *       SCSBW168-E-VALOR           = VALOR DA OPERACAO           *
      *       SCSBW168-E-QTD-REG-ENQUA   = QTDE REG'S DE ENQUADRAMENTO *
      *       SCSBW168-E-LISTA-ENQUA.                                  *
      *         SCSBW168-E-CD-ENQUA      = CODIGO DO ENQUADRAMENTO     *
      *         SCSBW168-E-RC            = REGISTRO DE CREDITO         *
      *                                                                *
      *----------------------------------------------------------------*
      * DATA       AUTOR             ALTERACAO                         *
      * ---------- ----------------- --------------------------------- *
      * 99/99/9999                   XXXXXXXXXXXXXXXXXXXXXXX           *
      *================================================================*
                                                                        
          05 SCSBW168-HEADER.                                           
             10 SCSBW168-COD-LAYOUT         PIC X(008) VALUE 'SCSBW168'.
             10 SCSBW168-TAM-LAYOUT         PIC 9(005) VALUE 24643.     
          05 SCSBW168-REGISTRO.                                         
             10 SCSBW168-BLOCO-ENTRADA.                                 
                15 SCSBW168-E-ANO                PIC 9(004).            
                15 SCSBW168-E-NREG               PIC 9(009).            
                15 SCSBW168-E-EMPR-RVS           PIC 9(010).            
                15 SCSBW168-E-NOME-ADQUIR        PIC X(150).            
                15 SCSBW168-E-END-ADQUIR         PIC X(150).            
                15 SCSBW168-E-CD-PAIS-ADQUIR     PIC 9(003).            
                15 SCSBW168-E-NIF                PIC X(040).            
                15 SCSBW168-E-CD-MOEDA           PIC 9(005).            
                15 SCSBW168-E-VLR-TOT-CONVERT    PIC 9(013)V99.         
                15 SCSBW168-E-SIT-REG            PIC X(001).            
                15 SCSBW168-E-INF-COMPLEM        PIC X(5000).           
                15 SCSBW168-E-QTD-REG-OPER       PIC 9(003).            
                15 SCSBW168-E-LISTA-OPER         OCCURS 020 TIMES.      
                   20 SCSBW168-E-ACAO            PIC X(001).            
                   20 SCSBW168-E-COPER           PIC 9(010).            
                   20 SCSBW168-E-CD-NBS          PIC X(009).            
                   20 SCSBW168-E-CD-PAIS-DESTNO  PIC 9(003).            
                   20 SCSBW168-E-CD-MODO-PREST   PIC 9(001).            
                   20 SCSBW168-E-DT-INICIO       PIC X(010).            
                   20 SCSBW168-E-DT-CONCLUSAO    PIC X(010).            
                   20 SCSBW168-E-VALOR           PIC 9(013)V99.         
                   20 SCSBW168-E-QTD-REG-ENQUA   PIC 9(003).            
                   20 SCSBW168-E-LISTA-ENQUA     OCCURS 050 TIMES.      
                      25 SCSBW168-E-CD-ENQUA     PIC 9(010).            
                      25 SCSBW168-E-RC           PIC 9(008).            

      *================================================================*
      * NOME BOOK  : SCSBW342                                          *
      * DESCRICAO  : ALTERAR RETIFICACAO DA RAS                        *
      * COMUNICACAO: FRAMEWORK X COORDENADOR                           *
      * DATA       : 09/02/2015                                        *
      * AUTOR      : FERNANDA CARUSO                                   *
      * EMPRESA    : BRQ IT SERVICES                                   *
      * GRUPO      : 17 - CONTABILIDADE E IMPOSTOS                     *
      * COMPONENTE : SCSB - SISCOSERV BRADESCO                         *
      *================================================================*
      *                                                                *
      * SCSBW342-HEADER.                                               *
      *   SCSBW342-COD-LAYOUT            = CODIGO DO LAYOUT            *
      *   SCSBW342-TAM-LAYOUT            = TAMANHO DO LAYOUT           *
      * SCSBW342-REGISTRO.                                             *
      *   SCSBW342-BLOCO-ENTRADA.                                      *
      *     SCSBW342-E-ANO               = ANO RAS                     *
      *     SCSBW342-E-NREG              = NUMERO DO REGISTRO RAS      *
      *     SCSBW342-E-EMPR-RAS          = EMPRESA DO REGISTRO RAS     *
      *     SCSBW342-E-NOME-VEND         = NOME DO VENDEDOR            *
      *     SCSBW342-E-END-VEND          = ENDERECO DO VENDEDOR        *
      *     SCSBW342-E-CD-PAIS-VEND      = CODIGO DO PAIS VENDEDOR     *
      *     SCSBW342-E-NIF               = NUMERO IDENTIFICACAO FISCAL *
      *     SCSBW342-E-CD-MOEDA          = CODIGO DA MOEDA             *
      *     SCSBW342-E-VLR-TOT-CONVERT   = VALOR TOTAL CONVERTIDO REAL *
      *     SCSBW342-E-INF-COMPLEM       = INFORMACOES COMPLEMENTARES  *
      *     SCSBW342-E-QTD-REG-OPER      = QTDE REGISTROS DE OPERACOES *
      *     SCSBW342-E-LISTA-OPER.                                     *
      *       SCSBW342-E-COPER           = CODIGO DA OPERACAO          *
      *       SCSBW342-E-CD-NBS          = CODIGO NBS                  *
      *       SCSBW342-E-CD-PAIS-DESTNO  = CODIGO PAIS DE DESTINO      *
      *       SCSBW342-E-CD-MODO-PREST   = CODIGO MODO DE PRESTACAO    *
      *       SCSBW342-E-DT-INICIO       = DATA DE INICIO              *
      *       SCSBW342-E-DT-CONCLUSAO    = DATA DE CONCLUSAO           *
      *       SCSBW342-E-VALOR           = VALOR DA OPERACAO           *
      *       SCSBW342-E-QTD-REG-ENQUA   = QTDE REG'S DE ENQUADRAMENTO *
      *       SCSBW342-E-LISTA-ENQUA.                                  *
      *         SCSBW342-E-CD-ENQUA      = CODIGO DO ENQUADRAMENTO     *
      *                                                                *
      *----------------------------------------------------------------*
      * DATA       AUTOR             ALTERACAO                         *
      * ---------- ----------------- --------------------------------- *
      * 99/99/9999                   XXXXXXXXXXXXXXXXXXXXXXX           *
      *================================================================*
                                                                        
          05 SCSBW342-HEADER.                                           
             10 SCSBW342-COD-LAYOUT         PIC X(008) VALUE 'SCSBW342'.
             10 SCSBW342-TAM-LAYOUT         PIC 9(005) VALUE 16622.     
          05 SCSBW342-REGISTRO.                                         
             10 SCSBW342-BLOCO-ENTRADA.                                 
                15 SCSBW342-E-ANO                PIC 9(004).            
                15 SCSBW342-E-NREG               PIC 9(009).            
                15 SCSBW342-E-EMPR-RAS           PIC 9(010).            
                15 SCSBW342-E-NOME-VEND          PIC X(150).            
                15 SCSBW342-E-END-VEND           PIC X(150).            
                15 SCSBW342-E-CD-PAIS-VEND       PIC 9(003).            
                15 SCSBW342-E-NIF                PIC X(040).            
                15 SCSBW342-E-CD-MOEDA           PIC 9(005).            
                15 SCSBW342-E-VLR-TOT-CONVERT    PIC 9(013)V99.         
                15 SCSBW342-E-INF-COMPLEM        PIC X(5000).           
                15 SCSBW342-E-QTD-REG-OPER       PIC 9(003).            
                15 SCSBW342-E-LISTA-OPER         OCCURS 020 TIMES.      
                   20 SCSBW342-E-COPER           PIC 9(010).            
                   20 SCSBW342-E-CD-NBS          PIC X(009).            
                   20 SCSBW342-E-CD-PAIS-DESTNO  PIC 9(003).            
                   20 SCSBW342-E-CD-MODO-PREST   PIC 9(001).            
                   20 SCSBW342-E-DT-INICIO       PIC X(010).            
                   20 SCSBW342-E-DT-CONCLUSAO    PIC X(010).            
                   20 SCSBW342-E-VALOR           PIC 9(013)V99.         
                   20 SCSBW342-E-QTD-REG-ENQUA   PIC 9(003).            
                   20 SCSBW342-E-LISTA-ENQUA     OCCURS 050 TIMES.      
                      25 SCSBW342-E-CD-ENQUA     PIC 9(010).            

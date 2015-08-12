      *================================================================*
      * NOME BOOK  : SCSBW112                                          *
      * DESCRICAO  : RETIFICAR RAS                                     *
      * COMUNICACAO: FRAMEWORK X COORDENADOR                           *
      * DATA       : 14/06/2013                                        *
      * AUTOR      : FERNANDA CARUSO                                   *
      * EMPRESA    : BRQ IT SERVICES                                   *
      * GRUPO      : 17 - CONTABILIDADE E IMPOSTOS                     *
      * COMPONENTE : SCSB - SISCOSERV BRADESCO                         *
      *================================================================*
      *                                                                *
      * SCSBW112-HEADER.                                               *
      *   SCSBW112-COD-LAYOUT            = CODIGO DO LAYOUT            *
      *   SCSBW112-TAM-LAYOUT            = TAMANHO DO LAYOUT           *
      * SCSBW112-REGISTRO.                                             *
      *   SCSBW112-BLOCO-ENTRADA.                                      *
      *     SCSBW112-E-ANO               = ANO RAS                     *
      *     SCSBW112-E-NREG              = NUMERO DO REGISTRO RAS      *
      *     SCSBW112-E-EMPR-RAS          = EMPRESA DO REGISTRO RAS     *
      *     SCSBW112-E-NOME-VEND         = NOME DO VENDEDOR            *
      *     SCSBW112-E-END-VEND          = ENDERECO DO VENDEDOR        *
      *     SCSBW112-E-CD-PAIS-VEND      = CODIGO DO PAIS DO VENDEDOR  *
      *     SCSBW112-E-NIF               = NUMERO IDENTIFICACAO FISCAL *
      *     SCSBW112-E-CD-MOEDA          = CODIGO DA MOEDA             *
      *     SCSBW112-E-VLR-TOT-CONVERT   = VALOR TOTAL CONVERTIDO REAL *
      *     SCSBW112-E-INF-COMPLEM       = INFORMACOES COMPLEMENTARES  *
      *     SCSBW112-E-QTD-REG-OPER      = QTDE REGISTROS DE OPERACOES *
      *     SCSBW112-E-LISTA-OPER.                                     *
      *       SCSBW112-E-COPER           = CODIGO DA OPERACAO          *
      *       SCSBW112-E-CD-NBS          = CODIGO NBS                  *
      *       SCSBW112-E-CD-PAIS-DESTNO  = CODIGO PAIS DE DESTINO      *
      *       SCSBW112-E-CD-MODO-PREST   = CODIGO MODO DE PRESTACAO    *
      *       SCSBW112-E-DT-INICIO       = DATA DE INICIO              *
      *       SCSBW112-E-DT-CONCLUSAO    = DATA DE CONCLUSAO           *
      *       SCSBW112-E-VALOR           = VALOR DA OPERACAO           *
      *       SCSBW112-E-QTD-REG-ENQUA   = QTDE REG'S DE ENQUADRAMENTO *
      *       SCSBW112-E-LISTA-ENQUA.                                  *
      *         SCSBW112-E-ACAO          = ACAO (I=INCLUIR / E=EXCLUIR)*
      *         SCSBW112-E-CD-ENQUA      = CODIGO DO ENQUADRAMENTO     *
      *                                                                *
      *----------------------------------------------------------------*
      * DATA       AUTOR             ALTERACAO                         *
      * ---------- ----------------- --------------------------------- *
      * 09/02/2015 FERNANDA CARUSO   EXCLUSAO DOS CAMPOS SCSBW112-E-   *
      *                              CPF-CNPJ-PRINC/FLIAL/CTRL.        *
      *                              INCLUSAO DOS CAMPOS SCSBW112-E-   *
      *                              EMPR-RAS/VLR-TOT-CONVERT.         *
      *================================================================*
                                                                        
          05 SCSBW112-HEADER.                                           
             10 SCSBW112-COD-LAYOUT         PIC X(008) VALUE 'SCSBW112'.
             10 SCSBW112-TAM-LAYOUT         PIC 9(005) VALUE 17622.     
          05 SCSBW112-REGISTRO.                                         
             10 SCSBW112-BLOCO-ENTRADA.                                 
                15 SCSBW112-E-ANO                PIC 9(004).            
                15 SCSBW112-E-NREG               PIC 9(009).            
                15 SCSBW112-E-EMPR-RAS           PIC 9(010).            
                15 SCSBW112-E-NOME-VEND          PIC X(150).            
                15 SCSBW112-E-END-VEND           PIC X(150).            
                15 SCSBW112-E-CD-PAIS-VEND       PIC 9(003).            
                15 SCSBW112-E-NIF                PIC X(040).            
                15 SCSBW112-E-CD-MOEDA           PIC 9(005).            
                15 SCSBW112-E-VLR-TOT-CONVERT    PIC 9(013)V99.         
                15 SCSBW112-E-INF-COMPLEM        PIC X(5000).           
                15 SCSBW112-E-QTD-REG-OPER       PIC 9(003).            
                15 SCSBW112-E-LISTA-OPER         OCCURS 020 TIMES.      
                   20 SCSBW112-E-COPER           PIC 9(010).            
                   20 SCSBW112-E-CD-NBS          PIC X(009).            
                   20 SCSBW112-E-CD-PAIS-DESTNO  PIC 9(003).            
                   20 SCSBW112-E-CD-MODO-PREST   PIC 9(001).            
                   20 SCSBW112-E-DT-INICIO       PIC X(010).            
                   20 SCSBW112-E-DT-CONCLUSAO    PIC X(010).            
                   20 SCSBW112-E-VALOR           PIC 9(013)V99.         
                   20 SCSBW112-E-QTD-REG-ENQUA   PIC 9(003).            
                   20 SCSBW112-E-LISTA-ENQUA     OCCURS 050 TIMES.      
                      25 SCSBW112-E-ACAO         PIC X(001).            
                      25 SCSBW112-E-CD-ENQUA     PIC 9(010).            

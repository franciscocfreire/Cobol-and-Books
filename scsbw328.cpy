      *================================================================*
      * NOME BOOK  : SCSBW328                                          *
      * DESCRICAO  : ALTERAR RAS                                       *
      * COMUNICACAO: FRAMEWORK X COORDENADOR                           *
      * DATA       : 06/02/2015                                        *
      * AUTOR      : FERNANDA CARUSO                                   *
      * EMPRESA    : BRQ IT SERVICES                                   *
      * GRUPO      : 17 - CONTABILIDADE E IMPOSTOS                     *
      * COMPONENTE : SCSB - SISCOSERV BRADESCO                         *
      *================================================================*
      *                                                                *
      * SCSBW328-HEADER.                                               *
      *   SCSBW328-COD-LAYOUT            = CODIGO DO LAYOUT            *
      *   SCSBW328-TAM-LAYOUT            = TAMANHO DO LAYOUT           *
      * SCSBW328-REGISTRO.                                             *
      *   SCSBW328-BLOCO-ENTRADA.                                      *
      *     SCSBW328-E-ANO               = ANO RAS                     *
      *     SCSBW328-E-NREG              = NUMERO DO REGISTRO RAS      *
      *     SCSBW328-E-EMPR-RAS          = EMPRESA DO REGISTRO RAS     *
      *     SCSBW328-E-NOME-VEND         = NOME DO VENDEDOR            *
      *     SCSBW328-E-END-VEND          = ENDERECO DO VENDEDOR        *
      *     SCSBW328-E-CD-PAIS-VEND      = CODIGO DO PAIS VENDEDOR     *
      *     SCSBW328-E-NIF               = NUMERO IDENTIFICACAO FISCAL *
      *     SCSBW328-E-CD-MOEDA          = CODIGO DA MOEDA             *
      *     SCSBW328-E-VLR-TOT-CONVERT   = VALOR TOTAL CONVERTIDO REAL *
      *     SCSBW328-E-SIT-REG           = SITUACAO DO REGISTRO        *
      *     SCSBW328-E-INF-COMPLEM       = INFORMACOES COMPLEMENTARES  *
      *     SCSBW328-E-QTD-REG-OPER      = QTDE REGISTROS DE OPERACOES *
      *     SCSBW328-E-LISTA-OPER.                                     *
      *       SCSBW328-E-ACAO            = ACAO                        *
      *                                    A -> ALTERAR                *
      *                                    E -> EXCLUIR                *
      *                                    I -> INCLUIR                *
      *       SCSBW328-E-COPER           = CODIGO DA OPERACAO          *
      *       SCSBW328-E-CD-NBS          = CODIGO NBS                  *
      *       SCSBW328-E-CD-PAIS-DESTNO  = CODIGO PAIS DE DESTINO      *
      *       SCSBW328-E-CD-MODO-PREST   = CODIGO MODO DE PRESTACAO    *
      *       SCSBW328-E-DT-INICIO       = DATA DE INICIO              *
      *       SCSBW328-E-DT-CONCLUSAO    = DATA DE CONCLUSAO           *
      *       SCSBW328-E-VALOR           = VALOR DA OPERACAO           *
      *       SCSBW328-E-QTD-REG-ENQUA   = QTDE REG'S DE ENQUADRAMENTO *
      *       SCSBW328-E-LISTA-ENQUA.                                  *
      *         SCSBW328-E-CD-ENQUA      = CODIGO DO ENQUADRAMENTO     *
      *                                                                *
      *----------------------------------------------------------------*
      * DATA       AUTOR             ALTERACAO                         *
      * ---------- ----------------- --------------------------------- *
      * 99/99/9999                   XXXXXXXXXXXXXXXXXXXXXXX           *
      *================================================================*
                                                                        
          05 SCSBW328-HEADER.                                           
             10 SCSBW328-COD-LAYOUT         PIC X(008) VALUE 'SCSBW328'.
             10 SCSBW328-TAM-LAYOUT         PIC 9(005) VALUE 16643.     
          05 SCSBW328-REGISTRO.                                         
             10 SCSBW328-BLOCO-ENTRADA.                                 
                15 SCSBW328-E-ANO                PIC 9(004).            
                15 SCSBW328-E-NREG               PIC 9(009).            
                15 SCSBW328-E-EMPR-RAS           PIC 9(010).            
                15 SCSBW328-E-NOME-VEND          PIC X(150).            
                15 SCSBW328-E-END-VEND           PIC X(150).            
                15 SCSBW328-E-CD-PAIS-VEND       PIC 9(003).            
                15 SCSBW328-E-NIF                PIC X(040).            
                15 SCSBW328-E-CD-MOEDA           PIC 9(005).            
                15 SCSBW328-E-VLR-TOT-CONVERT    PIC 9(013)V99.         
                15 SCSBW328-E-SIT-REG            PIC X(001).            
                15 SCSBW328-E-INF-COMPLEM        PIC X(5000).           
                15 SCSBW328-E-QTD-REG-OPER       PIC 9(003).            
                15 SCSBW328-E-LISTA-OPER         OCCURS 020 TIMES.      
                   20 SCSBW328-E-ACAO            PIC X(001).            
                   20 SCSBW328-E-COPER           PIC 9(010).            
                   20 SCSBW328-E-CD-NBS          PIC X(009).            
                   20 SCSBW328-E-CD-PAIS-DESTNO  PIC 9(003).            
                   20 SCSBW328-E-CD-MODO-PREST   PIC 9(001).            
                   20 SCSBW328-E-DT-INICIO       PIC X(010).            
                   20 SCSBW328-E-DT-CONCLUSAO    PIC X(010).            
                   20 SCSBW328-E-VALOR           PIC 9(013)V99.         
                   20 SCSBW328-E-QTD-REG-ENQUA   PIC 9(003).            
                   20 SCSBW328-E-LISTA-ENQUA     OCCURS 050 TIMES.      
                      25 SCSBW328-E-CD-ENQUA     PIC 9(010).            

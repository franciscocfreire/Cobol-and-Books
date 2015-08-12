      *================================================================*
      * NOME BOOK  : SCSBW052                                          *
      * DESCRICAO  : INCLUIR RAS                                       *
      * COMUNICACAO: FRAMEWORK X COORDENADOR                           *
      * DATA       : 02/03/2013                                        *
      * AUTOR      : FERNANDA CARUSO                                   *
      * EMPRESA    : BRQ IT SERVICES                                   *
      * GRUPO      : 17 - CONTABILIDADE E IMPOSTOS                     *
      * COMPONENTE : SCSB - SISCOSERV BRADESCO                         *
      *================================================================*
      *                                                                *
      * SCSBW052-HEADER.                                               *
      *   SCSBW052-COD-LAYOUT            = CODIGO DO LAYOUT            *
      *   SCSBW052-TAM-LAYOUT            = TAMANHO DO LAYOUT           *
      * SCSBW052-REGISTRO.                                             *
      *   SCSBW052-BLOCO-ENTRADA.                                      *
      *     SCSBW052-E-ANO               = ANO RAS                     *
      *     SCSBW052-E-NREG              = NUMERO DO REGISTRO RAS      *
      *     -> CAMPOS ACIMA NAO OBRIGATORIOS, SOMENTE SERAO            *
      *        PREENCHIDOS QUANDO EXISTIREM MAIS DO QUE 20 OPERACOES   *
      *     SCSBW052-E-EMPR-USUAR        = EMPRESA DO USUARIO          *
      *     SCSBW052-E-DEPEND-USUAR      = DEPENDENCIA DO USUARIO      *
      *     SCSBW052-E-EMPR-RAS          = EMPRESA DO REGISTRO RAS     *
      *     SCSBW052-E-NOME-VEND         = NOME DO VENDEDOR            *
      *     SCSBW052-E-END-VEND          = ENDERECO DO VENDEDOR        *
      *     SCSBW052-E-CD-PAIS-VEND      = CODIGO DO PAIS DO VENDEDOR  *
      *     SCSBW052-E-NIF               = NUMERO IDENTIFICACAO FISCAL *
      *     SCSBW052-E-CD-MOEDA          = CODIGO DA MOEDA             *
      *     SCSBW052-E-VLR-TOT-CONVERT   = VALOR TOTAL CONVERTIDO REAL *
      *     SCSBW052-E-INF-COMPLEM       = INFORMACOES COMPLEMENTARES  *
      *     SCSBW052-E-QTD-REG-OPER      = QTDE REGISTROS DE OPERACOES *
      *     SCSBW052-E-LISTA-OPER.                                     *
      *       SCSBW052-E-CD-NBS          = CODIGO NBS                  *
      *       SCSBW052-E-CD-PAIS-DESTNO  = CODIGO PAIS DE DESTINO      *
      *       SCSBW052-E-CD-MODO-PREST   = CODIGO MODO DE PRESTACAO    *
      *       SCSBW052-E-DT-INICIO       = DATA DE INICIO              *
      *       SCSBW052-E-DT-CONCLUSAO    = DATA DE CONCLUSAO           *
      *       SCSBW052-E-VALOR           = VALOR DA OPERACAO           *
      *       SCSBW052-E-QTD-REG-ENQUA   = QTDE REG'S DE ENQUADRAMENTO *
      *       SCSBW052-E-LISTA-ENQUA.                                  *
      *         SCSBW052-E-CD-ENQUA      = CODIGO DO ENQUADRAMENTO     *
      *   SCSBW052-BLOCO-SAIDA.                                        *
      *     SCSBW052-S-ANO               = ANO RAS                     *
      *     SCSBW052-S-NREG              = NUMERO DO REGISTRO RAS      *
      *                                                                *
      *----------------------------------------------------------------*
      * DATA       AUTOR             ALTERACAO                         *
      * ---------- ----------------- --------------------------------- *
      * 04/02/2015 FERNANDA CARUSO   EXCLUSAO DOS CAMPOS SCSBW052-E-   *
      *                              EMPRESA/DEPEND/SECAO/CPF-CNPJ.    *
      *                              INCLUSAO DOS CAMPOS SCSBW052-E-   *
      *                              EMPR-USUAR/DEPEND-USUAR/EMPR-RAS/ *
      *                              VLR-TOT-CONVERT.                  *
      *================================================================*
                                                                        
          05 SCSBW052-HEADER.                                           
             10 SCSBW052-COD-LAYOUT         PIC X(008) VALUE 'SCSBW052'.
             10 SCSBW052-TAM-LAYOUT         PIC 9(005) VALUE 16453.     
          05 SCSBW052-REGISTRO.                                         
             10 SCSBW052-BLOCO-ENTRADA.                                 
                15 SCSBW052-E-ANO                PIC 9(004).            
                15 SCSBW052-E-NREG               PIC 9(009).            
                15 SCSBW052-E-EMPR-USUAR         PIC 9(010).            
                15 SCSBW052-E-DEPEND-USUAR       PIC 9(008).            
                15 SCSBW052-E-EMPR-RAS           PIC 9(010).            
                15 SCSBW052-E-NOME-VEND          PIC X(150).            
                15 SCSBW052-E-END-VEND           PIC X(150).            
                15 SCSBW052-E-CD-PAIS-VEND       PIC 9(003).            
                15 SCSBW052-E-NIF                PIC X(040).            
                15 SCSBW052-E-CD-MOEDA           PIC 9(005).            
                15 SCSBW052-E-VLR-TOT-CONVERT    PIC 9(013)V99.         
                15 SCSBW052-E-INF-COMPLEM        PIC X(5000).           
                15 SCSBW052-E-QTD-REG-OPER       PIC 9(003).            
                15 SCSBW052-E-LISTA-OPER         OCCURS 020 TIMES.      
                   20 SCSBW052-E-CD-NBS          PIC X(009).            
                   20 SCSBW052-E-CD-PAIS-DESTNO  PIC 9(003).            
                   20 SCSBW052-E-CD-MODO-PREST   PIC 9(001).            
                   20 SCSBW052-E-DT-INICIO       PIC X(010).            
                   20 SCSBW052-E-DT-CONCLUSAO    PIC X(010).            
                   20 SCSBW052-E-VALOR           PIC 9(013)V99.         
                   20 SCSBW052-E-QTD-REG-ENQUA   PIC 9(003).            
                   20 SCSBW052-E-LISTA-ENQUA     OCCURS 050 TIMES.      
                      25 SCSBW052-E-CD-ENQUA     PIC 9(010).            
             10 SCSBW052-BLOCO-SAIDA.                                   
                15 SCSBW052-S-ANO                PIC 9(004).            
                15 SCSBW052-S-NREG               PIC 9(009).            

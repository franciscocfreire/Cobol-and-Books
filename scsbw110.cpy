      *================================================================*
      * NOME BOOK  : SCSBW110                                          *
      * DESCRICAO  : RETIFICAR RVS                                     *
      * COMUNICACAO: FRAMEWORK X COORDENADOR                           *
      * DATA       : 14/06/2013                                        *
      * AUTOR      : FERNANDA CARUSO                                   *
      * EMPRESA    : BRQ IT SERVICES                                   *
      * GRUPO      : 17 - CONTABILIDADE E IMPOSTOS                     *
      * COMPONENTE : SCSB - SISCOSERV BRADESCO                         *
      *================================================================*
      *                                                                *
      * SCSBW110-HEADER.                                               *
      *   SCSBW110-COD-LAYOUT            = CODIGO DO LAYOUT            *
      *   SCSBW110-TAM-LAYOUT            = TAMANHO DO LAYOUT           *
      * SCSBW110-REGISTRO.                                             *
      *   SCSBW110-BLOCO-ENTRADA.                                      *
      *     SCSBW110-E-ANO               = ANO RVS                     *
      *     SCSBW110-E-NREG              = NUMERO DO REGISTRO RVS      *
      *     SCSBW110-E-EMPR-RVS          = EMPRESA DO REGISTRO RVS     *
      *     SCSBW110-E-NOME-ADQUIR       = NOME DO ADQUIRENTE          *
      *     SCSBW110-E-END-ADQUIR        = ENDERECO DO ADQUIRENTE      *
      *     SCSBW110-E-CD-PAIS-ADQUIR    = CODIGO DO PAIS ADQUIRENTE   *
      *     SCSBW110-E-NIF               = NUMERO IDENTIFICACAO FISCAL *
      *     SCSBW110-E-CD-MOEDA          = CODIGO DA MOEDA             *
      *     SCSBW110-E-VLR-TOT-CONVERT   = VALOR TOTAL CONVERTIDO REAL *
      *     SCSBW110-E-INF-COMPLEM       = INFORMACOES COMPLEMENTARES  *
      *     SCSBW110-E-QTD-REG-OPER      = QTDE REGISTROS DE OPERACOES *
      *     SCSBW110-E-LISTA-OPER.                                     *
      *       SCSBW110-E-COPER           = CODIGO DA OPERACAO          *
      *       SCSBW110-E-CD-NBS          = CODIGO NBS                  *
      *       SCSBW110-E-CD-PAIS-DESTNO  = CODIGO PAIS DE DESTINO      *
      *       SCSBW110-E-CD-MODO-PREST   = CODIGO MODO DE PRESTACAO    *
      *       SCSBW110-E-DT-INICIO       = DATA DE INICIO              *
      *       SCSBW110-E-DT-CONCLUSAO    = DATA DE CONCLUSAO           *
      *       SCSBW110-E-VALOR           = VALOR DA OPERACAO           *
      *       SCSBW110-E-QTD-REG-ENQUA   = QTDE REG'S DE ENQUADRAMENTO *
      *       SCSBW110-E-LISTA-ENQUA.                                  *
      *         SCSBW110-E-ACAO          = ACAO (I=INCLUIR / E=EXCLUIR)*
      *         SCSBW110-E-CD-ENQUA      = CODIGO DO ENQUADRAMENTO     *
      *         SCSBW110-E-RC            = REGISTRO DE CREDITO         *
      *                                                                *
      *----------------------------------------------------------------*
      * DATA       AUTOR             ALTERACAO                         *
      * ---------- ----------------- --------------------------------- *
      * 21/01/2015 FERNANDA CARUSO   EXCLUSAO DOS CAMPOS SCSBW110-E-   *
      *                              CPF-CNPJ-PRINC/FLIAL/CTRL.        *
      *                              ALTERACAO DO CAMPO SCSBW110-E-RC  *
      *                              DE X(06) PARA 9(08).              *
      *                              INCLUSAO DOS CAMPOS SCSBW110-E-   *
      *                              EMPR-RVS/VLR-TOT-CONVERT.         *
      *================================================================*
                                                                        
          05 SCSBW110-HEADER.                                           
             10 SCSBW110-COD-LAYOUT         PIC X(008) VALUE 'SCSBW110'.
             10 SCSBW110-TAM-LAYOUT         PIC 9(005) VALUE 20567.     
          05 SCSBW110-REGISTRO.                                         
             10 SCSBW110-BLOCO-ENTRADA.                                 
                15 SCSBW110-E-ANO                PIC 9(004).            
                15 SCSBW110-E-NREG               PIC 9(009).            
                15 SCSBW110-E-EMPR-RVS           PIC 9(010).            
                15 SCSBW110-E-NOME-ADQUIR        PIC X(150).            
                15 SCSBW110-E-END-ADQUIR         PIC X(150).            
                15 SCSBW110-E-CD-PAIS-ADQUIR     PIC 9(003).            
                15 SCSBW110-E-NIF                PIC X(040).            
                15 SCSBW110-E-CD-MOEDA           PIC 9(005).            
                15 SCSBW110-E-VLR-TOT-CONVERT    PIC 9(013)V99.         
                15 SCSBW110-E-INF-COMPLEM        PIC X(5000).           
                15 SCSBW110-E-QTD-REG-OPER       PIC 9(003).            
                15 SCSBW110-E-LISTA-OPER         OCCURS 015 TIMES.      
                   20 SCSBW110-E-COPER           PIC 9(010).            
                   20 SCSBW110-E-CD-NBS          PIC X(009).            
                   20 SCSBW110-E-CD-PAIS-DESTNO  PIC 9(003).            
                   20 SCSBW110-E-CD-MODO-PREST   PIC 9(001).            
                   20 SCSBW110-E-DT-INICIO       PIC X(010).            
                   20 SCSBW110-E-DT-CONCLUSAO    PIC X(010).            
                   20 SCSBW110-E-VALOR           PIC 9(013)V99.         
                   20 SCSBW110-E-QTD-REG-ENQUA   PIC 9(003).            
                   20 SCSBW110-E-LISTA-ENQUA     OCCURS 050 TIMES.      
                      25 SCSBW110-E-ACAO         PIC X(001).            
                      25 SCSBW110-E-CD-ENQUA     PIC 9(010).            
                      25 SCSBW110-E-RC           PIC 9(008).            

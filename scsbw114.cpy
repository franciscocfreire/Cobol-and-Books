      *================================================================*
      * NOME BOOK  : SCSBW114                                          *
      * DESCRICAO  : INCLUIR ADITIVO RVS                               *
      * COMUNICACAO: FRAMEWORK X COORDENADOR                           *
      * DATA       : 14/06/2013                                        *
      * AUTOR      : FERNANDA CARUSO                                   *
      * EMPRESA    : BRQ IT SERVICES                                   *
      * GRUPO      : 17 - CONTABILIDADE E IMPOSTOS                     *
      * COMPONENTE : SCSB - SISCOSERV BRADESCO                         *
      *================================================================*
      *                                                                *
      * SCSBW114-HEADER.                                               *
      *   SCSBW114-COD-LAYOUT            = CODIGO DO LAYOUT            *
      *   SCSBW114-TAM-LAYOUT            = TAMANHO DO LAYOUT           *
      * SCSBW114-REGISTRO.                                             *
      *   SCSBW114-BLOCO-ENTRADA.                                      *
      *     SCSBW114-E-ANO               = ANO RVS                     *
      *     SCSBW114-E-NREG              = NUMERO DO REGISTRO RVS      *
      *     SCSBW114-E-QTD-REG-OPER      = QTDE REGISTROS DE OPERACOES *
      *     SCSBW114-E-LISTA-OPER.                                     *
      *       SCSBW114-E-CD-NBS          = CODIGO NBS                  *
      *       SCSBW114-E-CD-PAIS-DESTNO  = CODIGO PAIS DE DESTINO      *
      *       SCSBW114-E-CD-MODO-PREST   = CODIGO MODO DE PRESTACAO    *
      *       SCSBW114-E-DT-INICIO       = DATA DE INICIO              *
      *       SCSBW114-E-DT-CONCLUSAO    = DATA DE CONCLUSAO           *
      *       SCSBW114-E-VALOR           = VALOR DA OPERACAO           *
      *       SCSBW114-E-QTD-REG-ENQUA   = QTDE REG'S DE ENQUADRAMENTO *
      *       SCSBW114-E-LISTA-ENQUA.                                  *
      *         SCSBW114-E-CD-ENQUA      = CODIGO DO ENQUADRAMENTO     *
      *         SCSBW114-E-RC            = REGISTRO DE CREDITO         *
      *                                                                *
      *----------------------------------------------------------------*
      * DATA       AUTOR             ALTERACAO                         *
      * ---------- ----------------- --------------------------------- *
      * 26/01/2015 FERNANDA CARUSO   TAMANHO/FORMATO DO CAMPO SCSBW114 *
      *                              -E-RC DE X(06) PARA 9(08).        *
      *================================================================*
                                                                        
          05 SCSBW114-HEADER.                                           
             10 SCSBW114-COD-LAYOUT         PIC X(008) VALUE 'SCSBW114'.
             10 SCSBW114-TAM-LAYOUT         PIC 9(005) VALUE 19049.     
          05 SCSBW114-REGISTRO.                                         
             10 SCSBW114-BLOCO-ENTRADA.                                 
                15 SCSBW114-E-ANO                PIC 9(004).            
                15 SCSBW114-E-NREG               PIC 9(009).            
                15 SCSBW114-E-QTD-REG-OPER       PIC 9(003).            
                15 SCSBW114-E-LISTA-OPER         OCCURS 020 TIMES.      
                   20 SCSBW114-E-CD-NBS          PIC X(009).            
                   20 SCSBW114-E-CD-PAIS-DESTNO  PIC 9(003).            
                   20 SCSBW114-E-CD-MODO-PREST   PIC 9(001).            
                   20 SCSBW114-E-DT-INICIO       PIC X(010).            
                   20 SCSBW114-E-DT-CONCLUSAO    PIC X(010).            
                   20 SCSBW114-E-VALOR           PIC 9(013)V99.         
                   20 SCSBW114-E-QTD-REG-ENQUA   PIC 9(003).            
                   20 SCSBW114-E-LISTA-ENQUA     OCCURS 050 TIMES.      
                      25 SCSBW114-E-CD-ENQUA     PIC 9(010).            
                      25 SCSBW114-E-RC           PIC 9(008).            

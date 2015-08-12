      *================================================================*
      * NOME BOOK  : SCSBW092                                          *
      * DESCRICAO  : GERAR ARQUIVOS COM OS REGISTROS VALIDADOS         *
      * COMUNICACAO: FRAMEWORK X COORDENADOR                           *
      * DATA       : 27/05/2013                                        *
      * AUTOR      : FERNANDA CARUSO                                   *
      * EMPRESA    : BRQ IT SERVICES                                   *
      * GRUPO      : 17 - CONTABILIDADE E IMPOSTOS                     *
      * COMPONENTE : SCSB - SISCOSERV BRADESCO                         *
      *================================================================*
      *                                                                *
      * SCSBW092-HEADER.                                               *
      *   SCSBW092-COD-LAYOUT          = CODIGO DO LAYOUT              *
      *   SCSBW092-TAM-LAYOUT          = TAMANHO DO LAYOUT             *
      * SCSBW092-REGISTRO.                                             *
      *   SCSBW092-BLOCO-ENTRADA.                                      *
      *     SCSBW092-E-TPO-REG         = TIPO DE REGISTRO (1=RVS/2=RAS)*
      *     SCSBW092-E-QTD-REG         = QUANTIDADE DE REGISTROS       *
      *     SCSBW092-E-LISTA.                                          *
      *       SCSBW092-E-ANO           = ANO DO SERVICO                *
      *       SCSBW092-E-NREG          = NUMERO DO SERVICO             *
      *       SCSBW092-E-TPO-SERVC     = TIPO DE SERVICO               *
      *                                                                *
      *----------------------------------------------------------------*
      * DATA       AUTOR             ALTERACAO                         *
      * ---------- ----------------- --------------------------------- *
      * 01/04/2015 FERNANDA CARUSO   INCLUSAO DOS CAMPOS SCSBW092-E-   *
      *                              TPO-REG/QTDE-REG/LISTA/ANO/       *
      *                              NREG/TPO-SERVC.                   *
      *                              EXCLUSAO DO CAMPO SCSBW092-E-     *
      *                              TPO-ARQ.                          *
      *================================================================*
                                                                        
          05 SCSBW092-HEADER.                                           
             10 SCSBW092-COD-LAYOUT         PIC X(008) VALUE 'SCSBW092'.
             10 SCSBW092-TAM-LAYOUT         PIC 9(005) VALUE 767.       
          05 SCSBW092-REGISTRO.                                         
             10 SCSBW092-BLOCO-ENTRADA.                                 
                15 SCSBW092-E-TPO-REG       PIC 9(001).                 
                15 SCSBW092-E-QTD-REG       PIC 9(003).                 
                15 SCSBW092-E-LISTA         OCCURS 0 TO 050 TIMES       
                      DEPENDING ON SCSBW092-E-QTD-REG.                  
                   20 SCSBW092-E-ANO        PIC 9(004).                 
                   20 SCSBW092-E-NREG       PIC 9(009).                 
                   20 SCSBW092-E-TPO-SERVC  PIC 9(002).                 

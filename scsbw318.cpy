      *================================================================*
      * NOME BOOK  : SCSBW318                                          *
      * DESCRICAO  : CONSULTAR NO HISTORICO OS ENQUADRAMENTOS DA       *
      *              RAS/OPERACAO.                                     *
      * COMUNICACAO: FRAMEWORK X COORDENADOR                           *
      * DATA       : 05/02/2015                                        *
      * AUTOR      : FERNANDA CARUSO                                   *
      * EMPRESA    : BRQ IT SERVICES                                   *
      * GRUPO      : 17 - CONTABILIDADE E IMPOSTOS                     *
      * COMPONENTE : SCSB - SISCOSERV BRADESCO                         *
      *================================================================*
      *                                                                *
      * SCSBW318-HEADER.                                               *
      *   SCSBW318-COD-LAYOUT          = CODIGO DO LAYOUT              *
      *   SCSBW318-TAM-LAYOUT          = TAMANHO DO LAYOUT             *
      * SCSBW318-REGISTRO.                                             *
      *   SCSBW318-BLOCO-ENTRADA.                                      *
      *     SCSBW318-E-ANO             = ANO RAS                       *
      *     SCSBW318-E-NREG            = NUMERO DO REGISTRO RAS        *
      *     SCSBW318-E-COPER           = NUMERO DA OPERACAO            *
      *     SCSBW318-E-HINCL-HIST      = TIMESTAMP INCLUSAO HISTORICO  *
      *   SCSBW318-BLOCO-SAIDA.                                        *
      *     SCSBW318-S-QTD-REG         = QTDE REGISTROS DEVOLVIDOS     *
      *     SCSBW318-S-LISTA.                                          *
      *       SCSBW318-S-CD-ENQUA      = CODIGO DO ENQUADRAMENTO       *
      *       SCSBW318-S-DS-ENQUA      = DESCRICAO DO ENQUADRAMENTO    *
      *                                                                *
      *----------------------------------------------------------------*
      * DATA       AUTOR             ALTERACAO                         *
      * ---------- ----------------- --------------------------------- *
      * 99/99/9999                   XXXXXXXXXXXXXXXXXXXXXXX           *
      *================================================================*
                                                                        
          05 SCSBW318-HEADER.                                           
             10 SCSBW318-COD-LAYOUT         PIC X(008) VALUE 'SCSBW318'.
             10 SCSBW318-TAM-LAYOUT         PIC 9(005) VALUE 5565.      
          05 SCSBW318-REGISTRO.                                         
             10 SCSBW318-BLOCO-ENTRADA.                                 
                15 SCSBW318-E-ANO               PIC 9(004).             
                15 SCSBW318-E-NREG              PIC 9(009).             
                15 SCSBW318-E-COPER             PIC 9(010).             
                15 SCSBW318-E-HINCL-HIST        PIC X(026).             
             10 SCSBW318-BLOCO-SAIDA.                                   
                15 SCSBW318-S-QTD-REG           PIC 9(003).             
                15 SCSBW318-S-LISTA             OCCURS 0 TO 050 TIMES   
                      DEPENDING ON SCSBW318-S-QTD-REG.                  
                   20 SCSBW318-S-CD-ENQUA       PIC 9(010).             
                   20 SCSBW318-S-DS-ENQUA       PIC X(100).             

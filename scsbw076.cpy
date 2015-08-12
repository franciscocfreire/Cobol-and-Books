      *================================================================*
      * NOME BOOK  : SCSBW076                                          *
      * DESCRICAO  : INCLUIR/EXCLUIR OS RE'S DA RVS                    *
      * COMUNICACAO: FRAMEWORK X COORDENADOR                           *
      * DATA       : 02/03/2013                                        *
      * AUTOR      : FERNANDA CARUSO                                   *
      * EMPRESA    : BRQ IT SERVICES                                   *
      * GRUPO      : 17 - CONTABILIDADE E IMPOSTOS                     *
      * COMPONENTE : SCSB - SISCOSERV BRADESCO                         *
      *================================================================*
      *                                                                *
      * SCSBW076-HEADER.                                               *
      *   SCSBW076-COD-LAYOUT            = CODIGO DO LAYOUT            *
      *   SCSBW076-TAM-LAYOUT            = TAMANHO DO LAYOUT           *
      * SCSBW076-REGISTRO.                                             *
      *   SCSBW076-BLOCO-ENTRADA.                                      *
      *     SCSBW076-E-ANO               = ANO RVS                     *
      *     SCSBW076-E-NREG              = NUMERO DO REGISTRO RVS      *
      *     SCSBW076-E-QTD-REG.          = QUANTIDADE DE REGISTROS     *
      *     SCSBW076-E-LISTA.                                          *
      *       SCSBW076-E-TPO-REG         = TIPO DE REGISTRO            *
      *                                    1 -> INCLUSAO               *
      *                                    6 -> EXCLUSAO               *
      *       SCSBW076-E-CD-REG          = CODIGO DO REGISTRO          *
      *                                                                *
      *----------------------------------------------------------------*
      * DATA       AUTOR             ALTERACAO                         *
      * ---------- ----------------- --------------------------------- *
      * 99/99/9999                   XXXXXXXXXXXXXXXXXXXXXXX           *
      *================================================================*
                                                                        
          05 SCSBW076-HEADER.                                           
             10 SCSBW076-COD-LAYOUT         PIC X(008) VALUE 'SCSBW076'.
             10 SCSBW076-TAM-LAYOUT         PIC 9(005) VALUE 679.       
          05 SCSBW076-REGISTRO.                                         
             10 SCSBW076-BLOCO-ENTRADA.                                 
                15 SCSBW076-E-ANO           PIC 9(004).                 
                15 SCSBW076-E-NREG          PIC 9(009).                 
                15 SCSBW076-E-QTD-REG       PIC 9(003).                 
                15 SCSBW076-E-LISTA         OCCURS 0 TO 050 TIMES       
                      DEPENDING ON SCSBW076-E-QTD-REG.                  
                   20 SCSBW076-E-TPO-REG    PIC 9(001).                 
                   20 SCSBW076-E-CD-REG     PIC 9(012).                 

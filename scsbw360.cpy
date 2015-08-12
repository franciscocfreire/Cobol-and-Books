      *================================================================*
      * NOME BOOK  : SCSBW360                                          *
      * DESCRICAO  : EXCLUIR RETIFICACAO DE ADITIVO DE RAS             *
      * COMUNICACAO: FRAMEWORK X COORDENADOR                           *
      * DATA       : 11/02/2015                                        *
      * AUTOR      : FERNANDA CARUSO                                   *
      * EMPRESA    : BRQ IT SERVICES                                   *
      * GRUPO      : 17 - CONTABILIDADE E IMPOSTOS                     *
      * COMPONENTE : SCSB - SISCOSERV BRADESCO                         *
      *================================================================*
      *                                                                *
      * SCSBW360-HEADER.                                               *
      *   SCSBW360-COD-LAYOUT          = CODIGO DO LAYOUT              *
      *   SCSBW360-TAM-LAYOUT          = TAMANHO DO LAYOUT             *
      * SCSBW360-REGISTRO.                                             *
      *   SCSBW360-BLOCO-ENTRADA.                                      *
      *     SCSBW360-E-ANO             = ANO RAS                       *
      *     SCSBW360-E-NREG            = NUMERO DO REGISTRO RAS        *
      *     SCSBW360-E-QTD-REG         = QUANTIDADE DE REGISTROS       *
      *     SCSBW360-E-LISTA.                                          *
      *       SCSBW360-E-COPER-ADITIVO = CODIGO DA OPERACAO DE ADITIVO *
      *       SCSBW360-E-SIT-OPER      = SITUACAO DA OPERACAO          *
      *                                                                *
      *----------------------------------------------------------------*
      * DATA       AUTOR             ALTERACAO                         *
      * ---------- ----------------- --------------------------------- *
      * 99/99/9999                   XXXXXXXXXXXXXXXXXXXXXXX           *
      *================================================================*
                                                                        
          05 SCSBW360-HEADER.                                           
             10 SCSBW360-COD-LAYOUT         PIC X(008) VALUE 'SCSBW360'.
             10 SCSBW360-TAM-LAYOUT         PIC 9(005) VALUE 579.       
          05 SCSBW360-REGISTRO.                                         
             10 SCSBW360-BLOCO-ENTRADA.                                 
                15 SCSBW360-E-ANO               PIC 9(004).             
                15 SCSBW360-E-NREG              PIC 9(009).             
                15 SCSBW360-E-QTD-REG           PIC 9(003).             
                15 SCSBW360-E-LISTA             OCCURS 0 TO 050 TIMES   
                      DEPENDING ON SCSBW360-E-QTD-REG.                  
                   20 SCSBW360-E-COPER-ADITIVO  PIC 9(010).             
                   20 SCSBW360-E-SIT-OPER       PIC X(001).             

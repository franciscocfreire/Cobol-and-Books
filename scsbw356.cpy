      *================================================================*
      * NOME BOOK  : SCSBW356                                          *
      * DESCRICAO  : EXCLUIR ADITIVO DE RAS                            *
      * COMUNICACAO: FRAMEWORK X COORDENADOR                           *
      * DATA       : 11/02/2015                                        *
      * AUTOR      : FERNANDA CARUSO                                   *
      * EMPRESA    : BRQ IT SERVICES                                   *
      * GRUPO      : 17 - CONTABILIDADE E IMPOSTOS                     *
      * COMPONENTE : SCSB - SISCOSERV BRADESCO                         *
      *================================================================*
      *                                                                *
      * SCSBW356-HEADER.                                               *
      *   SCSBW356-COD-LAYOUT          = CODIGO DO LAYOUT              *
      *   SCSBW356-TAM-LAYOUT          = TAMANHO DO LAYOUT             *
      * SCSBW356-REGISTRO.                                             *
      *   SCSBW356-BLOCO-ENTRADA.                                      *
      *     SCSBW356-E-ANO             = ANO RAS                       *
      *     SCSBW356-E-NREG            = NUMERO DO REGISTRO RAS        *
      *     SCSBW356-E-QTD-REG         = QUANTIDADE DE REGISTROS       *
      *     SCSBW356-E-LISTA.                                          *
      *       SCSBW356-E-COPER-ADITIVO = CODIGO DA OPERACAO DE ADITIVO *
      *       SCSBW356-E-SIT-OPER      = SITUACAO DA OPERACAO          *
      *                                                                *
      *----------------------------------------------------------------*
      * DATA       AUTOR             ALTERACAO                         *
      * ---------- ----------------- --------------------------------- *
      * 99/99/9999                   XXXXXXXXXXXXXXXXXXXXXXX           *
      *================================================================*
                                                                        
          05 SCSBW356-HEADER.                                           
             10 SCSBW356-COD-LAYOUT         PIC X(008) VALUE 'SCSBW356'.
             10 SCSBW356-TAM-LAYOUT         PIC 9(005) VALUE 579.       
          05 SCSBW356-REGISTRO.                                         
             10 SCSBW356-BLOCO-ENTRADA.                                 
                15 SCSBW356-E-ANO               PIC 9(004).             
                15 SCSBW356-E-NREG              PIC 9(009).             
                15 SCSBW356-E-QTD-REG           PIC 9(003).             
                15 SCSBW356-E-LISTA             OCCURS 0 TO 050 TIMES   
                      DEPENDING ON SCSBW356-E-QTD-REG.                  
                   20 SCSBW356-E-COPER-ADITIVO  PIC 9(010).             
                   20 SCSBW356-E-SIT-OPER       PIC X(001).             

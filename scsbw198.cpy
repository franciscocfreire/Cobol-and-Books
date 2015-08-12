      *================================================================*
      * NOME BOOK  : SCSBW198                                          *
      * DESCRICAO  : EXCLUIR ADITIVO DE RVS                            *
      * COMUNICACAO: FRAMEWORK X COORDENADOR                           *
      * DATA       : 29/01/2015                                        *
      * AUTOR      : FERNANDA CARUSO                                   *
      * EMPRESA    : BRQ IT SERVICES                                   *
      * GRUPO      : 17 - CONTABILIDADE E IMPOSTOS                     *
      * COMPONENTE : SCSB - SISCOSERV BRADESCO                         *
      *================================================================*
      *                                                                *
      * SCSBW198-HEADER.                                               *
      *   SCSBW198-COD-LAYOUT          = CODIGO DO LAYOUT              *
      *   SCSBW198-TAM-LAYOUT          = TAMANHO DO LAYOUT             *
      * SCSBW198-REGISTRO.                                             *
      *   SCSBW198-BLOCO-ENTRADA.                                      *
      *     SCSBW198-E-ANO             = ANO RVS                       *
      *     SCSBW198-E-NREG            = NUMERO DO REGISTRO RVS        *
      *     SCSBW198-E-QTD-REG         = QUANTIDADE DE REGISTROS       *
      *     SCSBW198-E-LISTA.                                          *
      *       SCSBW198-E-COPER-ADITIVO = CODIGO DA OPERACAO DE ADITIVO *
      *       SCSBW198-E-SIT-OPER      = SITUACAO DA OPERACAO          *
      *                                                                *
      *----------------------------------------------------------------*
      * DATA       AUTOR             ALTERACAO                         *
      * ---------- ----------------- --------------------------------- *
      * 99/99/9999                   XXXXXXXXXXXXXXXXXXXXXXX           *
      *================================================================*
                                                                        
          05 SCSBW198-HEADER.                                           
             10 SCSBW198-COD-LAYOUT         PIC X(008) VALUE 'SCSBW198'.
             10 SCSBW198-TAM-LAYOUT         PIC 9(005) VALUE 579.       
          05 SCSBW198-REGISTRO.                                         
             10 SCSBW198-BLOCO-ENTRADA.                                 
                15 SCSBW198-E-ANO               PIC 9(004).             
                15 SCSBW198-E-NREG              PIC 9(009).             
                15 SCSBW198-E-QTD-REG           PIC 9(003).             
                15 SCSBW198-E-LISTA             OCCURS 0 TO 050 TIMES   
                      DEPENDING ON SCSBW198-E-QTD-REG.                  
                   20 SCSBW198-E-COPER-ADITIVO  PIC 9(010).             
                   20 SCSBW198-E-SIT-OPER       PIC X(001).             

      *================================================================*
      * NOME BOOK  : SCSBW302                                          *
      * DESCRICAO  : EXCLUIR RETIFICACAO DE ADITIVO DE RVS             *
      * COMUNICACAO: FRAMEWORK X COORDENADOR                           *
      * DATA       : 30/01/2015                                        *
      * AUTOR      : FERNANDA CARUSO                                   *
      * EMPRESA    : BRQ IT SERVICES                                   *
      * GRUPO      : 17 - CONTABILIDADE E IMPOSTOS                     *
      * COMPONENTE : SCSB - SISCOSERV BRADESCO                         *
      *================================================================*
      *                                                                *
      * SCSBW302-HEADER.                                               *
      *   SCSBW302-COD-LAYOUT          = CODIGO DO LAYOUT              *
      *   SCSBW302-TAM-LAYOUT          = TAMANHO DO LAYOUT             *
      * SCSBW302-REGISTRO.                                             *
      *   SCSBW302-BLOCO-ENTRADA.                                      *
      *     SCSBW302-E-ANO             = ANO RVS                       *
      *     SCSBW302-E-NREG            = NUMERO DO REGISTRO RVS        *
      *     SCSBW302-E-QTD-REG         = QUANTIDADE DE REGISTROS       *
      *     SCSBW302-E-LISTA.                                          *
      *       SCSBW302-E-COPER-ADITIVO = CODIGO DA OPERACAO DE ADITIVO *
      *       SCSBW302-E-SIT-OPER      = SITUACAO DA OPERACAO          *
      *                                                                *
      *----------------------------------------------------------------*
      * DATA       AUTOR             ALTERACAO                         *
      * ---------- ----------------- --------------------------------- *
      * 99/99/9999                   XXXXXXXXXXXXXXXXXXXXXXX           *
      *================================================================*
                                                                        
          05 SCSBW302-HEADER.                                           
             10 SCSBW302-COD-LAYOUT         PIC X(008) VALUE 'SCSBW302'.
             10 SCSBW302-TAM-LAYOUT         PIC 9(005) VALUE 579.       
          05 SCSBW302-REGISTRO.                                         
             10 SCSBW302-BLOCO-ENTRADA.                                 
                15 SCSBW302-E-ANO               PIC 9(004).             
                15 SCSBW302-E-NREG              PIC 9(009).             
                15 SCSBW302-E-QTD-REG           PIC 9(003).             
                15 SCSBW302-E-LISTA             OCCURS 0 TO 050 TIMES   
                      DEPENDING ON SCSBW302-E-QTD-REG.                  
                   20 SCSBW302-E-COPER-ADITIVO  PIC 9(010).             
                   20 SCSBW302-E-SIT-OPER       PIC X(001).             

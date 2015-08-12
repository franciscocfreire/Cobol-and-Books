      *================================================================*
      * NOME BOOK  : SCSBW026                                          *
      * DESCRICAO  : COMBO SITUACAO DO REGISTRO                        *
      * COMUNICACAO: FRAMEWORK X COORDENADOR                           *
      * DATA       : 02/03/2013                                        *
      * AUTOR      : FERNANDA CARUSO                                   *
      * EMPRESA    : BRQ IT SERVICES                                   *
      * GRUPO      : 17 - CONTABILIDADE E IMPOSTOS                     *
      * COMPONENTE : SCSB - SISCOSERV BRADESCO                         *
      *================================================================*
      *                                                                *
      * SCSBW026-HEADER.                                               *
      *   SCSBW026-COD-LAYOUT          = CODIGO DO LAYOUT              *
      *   SCSBW026-TAM-LAYOUT          = TAMANHO DO LAYOUT             *
      * SCSBW026-REGISTRO.                                             *
      *   SCSBW026-BLOCO-ENTRADA.                                      *
      *     SCSBW026-E-TPO-REG         = TIPO DO REGISTRO              *
      *                                  1 -> RVS                      *
      *                                  2 -> RAS                      *
      *                                  3 -> RPC                      *
      *     SCSBW026-E-TPO-ACAO        = TIPO DE ACAO                  *
      *                                  C -> CONSULTA                 *
      *                                  A -> ALTERACAO                *
      *                                  E -> EXCLUSAO                 *
      *                                  V -> VALIDACAO                *
      *     SCSBW026-E-CD-SITUACAO     = CODIGO DA SITUACAO (OPCIONAL) *
      *   SCSBW026-BLOCO-SAIDA.                                        *
      *     SCSBW026-S-QTD-REG         = QTDE REGISTROS DEVOLVIDOS     *
      *     SCSBW026-S-LISTA.                                          *
      *       SCSBW026-S-CD-SITUACAO   = CODIGO DA SITUACAO            *
      *       SCSBW026-S-DS-SITUACAO   = DESCRICAO DA SITUACAO         *
      *                                                                *
      *----------------------------------------------------------------*
      * DATA       AUTOR             ALTERACAO                         *
      * ---------- ----------------- --------------------------------- *
      * 17/12/2014 FERNANDA CARUSO   INCLUSAO DOS CAMPOS DE ENTRADA:   *
      *                              SCSBW026-E-TPO-REG                *
      *                              SCSBW026-E-TPO-ACAO               *
      *================================================================*
                                                                        
          05 SCSBW026-HEADER.                                           
             10 SCSBW026-COD-LAYOUT         PIC X(008) VALUE 'SCSBW026'.
             10 SCSBW026-TAM-LAYOUT         PIC 9(005) VALUE 2569.      
          05 SCSBW026-REGISTRO.                                         
             10 SCSBW026-BLOCO-ENTRADA.                                 
                15 SCSBW026-E-TPO-REG         PIC 9(001).               
                15 SCSBW026-E-TPO-ACAO        PIC X(001).               
                15 SCSBW026-E-CD-SITUACAO     PIC X(001).               
             10 SCSBW026-BLOCO-SAIDA.                                   
                15 SCSBW026-S-QTD-REG         PIC 9(003).               
                15 SCSBW026-S-LISTA           OCCURS 0 TO 050 TIMES     
                      DEPENDING ON SCSBW026-S-QTD-REG.                  
                   20 SCSBW026-S-CD-SITUACAO  PIC X(001).               
                   20 SCSBW026-S-DS-SITUACAO  PIC X(050).               

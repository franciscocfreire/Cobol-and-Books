      *================================================================*
      * NOME BOOK  : SCSBW378                                          *
      * DESCRICAO  : PARAMETRIZAR DEPENDENCIA RESPONSAVEL PELA         *
      *              GERACAO DE ARQUIVOS.                              *
      * COMUNICACAO: FRAMEWORK X COORDENADOR                           *
      * DATA       : 23/03/2015                                        *
      * AUTOR      : FERNANDA CARUSO                                   *
      * EMPRESA    : BRQ IT SERVICES                                   *
      * GRUPO      : 17 - CONTABILIDADE E IMPOSTOS                     *
      * COMPONENTE : SCSB - SISCOSERV BRADESCO                         *
      *================================================================*
      *                                                                *
      * SCSBW378-HEADER.                                               *
      *   SCSBW378-COD-LAYOUT            = CODIGO DO LAYOUT            *
      *   SCSBW378-TAM-LAYOUT            = TAMANHO DO LAYOUT           *
      * SCSBW378-REGISTRO.                                             *
      *   SCSBW378-BLOCO-ENTRADA.                                      *
      *     SCSBW378-E-FUNCAO            = FUNCAO                      *
      *                                    I -> INCLUIR                *
      *                                    E -> EXCLUIR                *
      *     SCSBW378-E-EMPR-DEPEND       = COD EMPRESA DA DEPENDENCIA  *
      *     SCSBW378-E-DEPEND            = CODIGO DA DEPENDENCIA       *
      *     SCSBW378-E-IND-TODAS-EMPR    = IND RESPONS TODAS EMPRESAS  *
      *                                    0 -> NAO SELECIONADO        *
      *                                    1 -> SELECIONADO            *
      *     SCSBW378-E-QTD-REG           = QUANTIDADE DE REGISTROS     *
      *     SCSBW378-E-LISTA.                                          *
      *       SCSBW378-E-EMPR            = CODIGO DA EMPRESA           *
      *                                                                *
      *----------------------------------------------------------------*
      * DATA       AUTOR             ALTERACAO                         *
      * ---------- ----------------- --------------------------------- *
      * 99/99/9999                   XXXXXXXXXXXXXXXXXXXXXXX           *
      *================================================================*
                                                                        
          05 SCSBW378-HEADER.                                           
             10 SCSBW378-COD-LAYOUT         PIC X(008) VALUE 'SCSBW378'.
             10 SCSBW378-TAM-LAYOUT         PIC 9(005) VALUE 536.       
          05 SCSBW378-REGISTRO.                                         
             10 SCSBW378-BLOCO-ENTRADA.                                 
                15 SCSBW378-E-FUNCAO              PIC X(001).           
                15 SCSBW378-E-EMPR-DEPEND         PIC 9(010).           
                15 SCSBW378-E-DEPEND              PIC 9(008).           
                15 SCSBW378-E-IND-TODAS-EMPR      PIC 9(001).           
                15 SCSBW378-E-QTD-REG             PIC 9(003).           
                15 SCSBW378-E-LISTA               OCCURS 0 TO 050 TIMES 
                      DEPENDING ON SCSBW378-E-QTD-REG.                  
                   20 SCSBW378-E-EMPR             PIC 9(010).           

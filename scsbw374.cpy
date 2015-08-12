      *================================================================*
      * NOME BOOK  : SCSBW374                                          *
      * DESCRICAO  : COMBO EMPRESA - RESPONSAVEL PELA GERACAO DE ARQ'S *
      * COMUNICACAO: FRAMEWORK X COORDENADOR                           *
      * DATA       : 23/03/2015                                        *
      * AUTOR      : FERNANDA CARUSO                                   *
      * EMPRESA    : BRQ IT SERVICES                                   *
      * GRUPO      : 17 - CONTABILIDADE E IMPOSTOS                     *
      * COMPONENTE : SCSB - SISCOSERV BRADESCO                         *
      *================================================================*
      *                                                                *
      * SCSBW374-HEADER.                                               *
      *   SCSBW374-COD-LAYOUT            = CODIGO DO LAYOUT            *
      *   SCSBW374-TAM-LAYOUT            = TAMANHO DO LAYOUT           *
      * SCSBW374-REGISTRO.                                             *
      *   SCSBW374-BLOCO-ENTRADA.                                      *
      *     SCSBW374-E-EMPR-DEPEND       = COD EMPRESA DA DEPENDENCIA  *
      *     SCSBW374-E-DEPEND            = CODIGO DA DEPENDENCIA       *
      *   SCSBW374-BLOCO-SAIDA.                                        *
      *     SCSBW374-S-IND-TODAS-EMPR    = IND RESPONS TODAS EMPRESAS  *
      *                                    0 -> NAO SELECIONADO        *
      *                                    1 -> SELECIONADO            *
      *     SCSBW374-S-QTD-REG           = QUANTIDADE DE REGISTROS     *
      *     SCSBW374-S-LISTA.                                          *
      *       SCSBW374-S-EMPR            = CODIGO DA EMPRESA           *
      *       SCSBW374-S-CNPJ-PRINC      = CODIGO DO CNPJ (CORPO)      *
      *       SCSBW374-S-CNPJ-FLIAL      = CODIGO DA FILIAL DO CNPJ    *
      *       SCSBW374-S-CNPJ-CTRL       = CODIGO DO CONTROLE DO CNPJ  *
      *       SCSBW374-S-DESC-EMPR       = DESCRICAO DA EMPRESA        *
      *                                                                *
      *----------------------------------------------------------------*
      * DATA       AUTOR             ALTERACAO                         *
      * ---------- ----------------- --------------------------------- *
      * 99/99/9999                   XXXXXXXXXXXXXXXXXXXXXXX           *
      *================================================================*
                                                                        
          05 SCSBW374-HEADER.                                           
             10 SCSBW374-COD-LAYOUT         PIC X(008) VALUE 'SCSBW374'.
             10 SCSBW374-TAM-LAYOUT         PIC 9(005) VALUE 4785.      
          05 SCSBW374-REGISTRO.                                         
             10 SCSBW374-BLOCO-ENTRADA.                                 
                15 SCSBW374-E-EMPR-DEPEND         PIC 9(010).           
                15 SCSBW374-E-DEPEND              PIC 9(008).           
             10 SCSBW374-BLOCO-SAIDA.                                   
                15 SCSBW374-S-IND-TODAS-EMPR      PIC 9(001).           
                15 SCSBW374-S-QTD-REG             PIC 9(003).           
                15 SCSBW374-S-LISTA               OCCURS 0 TO 050 TIMES 
                      DEPENDING ON SCSBW374-S-QTD-REG.                  
                   20 SCSBW374-S-EMPR             PIC 9(010).           
                   20 SCSBW374-S-CNPJ-PRINC       PIC 9(009).           
                   20 SCSBW374-S-CNPJ-FLIAL       PIC 9(004).           
                   20 SCSBW374-S-CNPJ-CTRL        PIC 9(002).           
                   20 SCSBW374-S-DESC-EMPR        PIC X(070).           

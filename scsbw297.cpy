      *================================================================*
      * NOME BOOK  : SCSBW297                                          *
      * DESCRICAO  : COMBO EMPRESA DO SCSB                             *
      * COMUNICACAO: FRAMEWORK X COORDENADOR                           *
      * DATA       : 19/06/2015                                        *
      * AUTOR      : JOSE TOSI                                         *
      * EMPRESA    : BRQ IT SERVICES                                   *
      * GRUPO      : 17 - CONTABILIDADE E IMPOSTOS                     *
      * COMPONENTE : SCSB - SISCOSERV BRADESCO                         *
      *================================================================*
      *                                                                *
      * SCSBW297-HEADER.                                               *
      *   SCSBW297-COD-LAYOUT          = CODIGO DO LAYOUT              *
      *   SCSBW297-TAM-LAYOUT          = TAMANHO DO LAYOUT             *
      * SCSBW297-REGISTRO.                                             *
      *   SCSBW297-BLOCO-ENTRADA.                                      *
      *     SCSBW297-E-TPO-REG         = TIPO DE REGISTRO              *
      *                                  1 -> RVS                      *
      *                                  2 -> RAS                      *
      *   SCSBW297-BLOCO-SAIDA.                                        *
      *     SCSBW297-S-QTD-REG         = QUANTIDADE DE REGISTROS       *
      *     SCSBW297-S-LISTA.                                          *
      *       SCSBW297-S-CPF-CNPJ-PRINC = CPF/CNPJ PRINCIPAL          * 
      *       SCSBW297-S-CPF-CNPJ-FLIAL = CPF/CNPJ FILIAL              *
      *       SCSBW297-S-CPF-CNPJ-CTRL  = CPF/CNPJ CONTROLE            *
      *       SCSBW297-S-CD-EMPR        = CODIGO DA EMPRESA            *
      *       SCSBW297-S-DS-EMPR        = DESCRICAO DA EMPRESA         *
      *                                                                *
      *----------------------------------------------------------------*
      * DATA       AUTOR             ALTERACAO                         *
      * ---------- ----------------- --------------------------------- *
      * 99/99/9999                   XXXXXXXXXXXXXXXXXXXXXXX           *
      *================================================================*
                                                                        
          05 SCSBW297-HEADER.                                           
             10 SCSBW297-COD-LAYOUT         PIC X(008) VALUE 'SCSBW297'.
             10 SCSBW297-TAM-LAYOUT         PIC 9(005) VALUE 4767.      
          05 SCSBW297-REGISTRO.                                         
             10 SCSBW297-BLOCO-ENTRADA.                                 
                15 SCSBW297-E-TPO-REG         PIC 9(001).               
             10 SCSBW297-BLOCO-SAIDA.                                   
                15 SCSBW297-S-QTD-REG         PIC 9(003).               
                15 SCSBW297-S-LISTA           OCCURS 0 TO 050 TIMES     
                      DEPENDING ON SCSBW297-S-QTD-REG.                  
                   20 SCSBW297-S-CPF-CNPJ-PRINC PIC 9(009).             
                   20 SCSBW297-S-CPF-CNPJ-FLIAL PIC 9(004).             
                   20 SCSBW297-S-CPF-CNPJ-CTRL  PIC 9(002).             
                   20 SCSBW297-S-CD-EMPR        PIC 9(010).             
                   20 SCSBW297-S-DS-EMPR        PIC X(070).             

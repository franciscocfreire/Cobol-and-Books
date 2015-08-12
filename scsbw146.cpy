      *================================================================*
      * NOME BOOK  : SCSBW146                                          *
      * DESCRICAO  : OBTER O PERFIL FUNCIONAL DO USUARIO               *
      * COMUNICACAO: FRAMEWORK X COORDENADOR                           *
      * DATA       : 12/12/2014                                        *
      * AUTOR      : FERNANDA CARUSO                                   *
      * EMPRESA    : BRQ IT SERVICES                                   *
      * GRUPO      : 17 - CONTABILIDADE E IMPOSTOS                     *
      * COMPONENTE : SCSB - SISCOSERV BRADESCO                         *
      *================================================================*
      *                                                                *
      * SCSBW146-HEADER.                                               *
      *   SCSBW146-COD-LAYOUT          = CODIGO DO LAYOUT              *
      *   SCSBW146-TAM-LAYOUT          = TAMANHO DO LAYOUT             *
      * SCSBW146-REGISTRO.                                             *
      *   SCSBW146-BLOCO-SAIDA.                                        *
      *     SCSBW146-S-PERFIL          = CODIGO DO PERFIL FUNCIONAL    *
      *                                  C - USUARIO COMUM             *
      *                                  G - GESTOR CORPORATIVO        *
      *                                  M - MASTER                    *
      *                                  T - GEST CORP TODAS EMPRESAS  *
      *     SCSBW146-S-EMPR            = CODIGO DA EMPRESA UORG        *
      *     SCSBW146-S-CNPJ-PRINC      = CODIGO DO CNPJ (CORPO)        *
      *     SCSBW146-S-CNPJ-FLIAL      = CODIGO DA FILIAL DO CNPJ      *
      *     SCSBW146-S-CNPJ-CTRL       = CODIGO DO CONTROLE DO CNPJ    *
      *     SCSBW146-S-DESC-EMPR       = DESCRICAO DA EMPRESA          *
      *     SCSBW146-S-DEPEND-SAP      = CODIGO DA DEPENDENCIA SAP     *
      *     SCSBW146-S-DEPEND-UORG     = CODIGO DA DEPENDENCIA UORG    *
      *     SCSBW146-S-DESC-DEPEND     = DESCRICAO DA DEPENDENCIA      *
      *                                                                *
      *----------------------------------------------------------------*
      * DATA       AUTOR             ALTERACAO                         *
      * ---------- ----------------- --------------------------------- *
      * 99/99/9999                   XXXXXXXXXXXXXXXXXXXXXXX           *
      *================================================================*
                                                                        
          05 SCSBW146-HEADER.                                           
             10 SCSBW146-COD-LAYOUT         PIC X(008) VALUE 'SCSBW146'.
             10 SCSBW146-TAM-LAYOUT         PIC 9(005) VALUE 182.       
          05 SCSBW146-REGISTRO.                                         
             10 SCSBW146-BLOCO-SAIDA.                                   
                15 SCSBW146-S-PERFIL        PIC X(001).                 
                15 SCSBW146-S-EMPR          PIC 9(010).                 
                15 SCSBW146-S-CNPJ-PRINC    PIC 9(009).                 
                15 SCSBW146-S-CNPJ-FLIAL    PIC 9(004).                 
                15 SCSBW146-S-CNPJ-CTRL     PIC 9(002).                 
                15 SCSBW146-S-DESC-EMPR     PIC X(070).                 
                15 SCSBW146-S-DEPEND-SAP    PIC 9(005).                 
                15 SCSBW146-S-DEPEND-UORG   PIC 9(008).                 
                15 SCSBW146-S-DESC-DEPEND   PIC X(060).                 

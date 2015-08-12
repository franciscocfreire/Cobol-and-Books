      *================================================================*
      * NOME BOOK  : SCSBW362                                          *
      * DESCRICAO  : CONSULTAR A LISTA DOS REGISTROS DE PAGAMENTO      *
      *              PARA ALTERACAO.                                   *
      * COMUNICACAO: FRAMEWORK X COORDENADOR                           *
      * DATA       : 11/02/2015                                        *
      * AUTOR      : FERNANDA CARUSO                                   *
      * EMPRESA    : BRQ IT SERVICES                                   *
      * GRUPO      : 17 - CONTABILIDADE E IMPOSTOS                     *
      * COMPONENTE : SCSB - SISCOSERV BRADESCO                         *
      *================================================================*
      *                                                                *
      * SCSBW362-HEADER.                                               *
      *   SCSBW362-COD-LAYOUT            = CODIGO DO LAYOUT            *
      *   SCSBW362-TAM-LAYOUT            = TAMANHO DO LAYOUT           *
      * SCSBW362-REGISTRO.                                             *
      *   SCSBW362-BLOCO-ENTRADA.                                      *
      *     SCSBW362-E-ANO               = ANO RAS                     *
      *     SCSBW362-E-NREG              = NUMERO DO REGISTRO RAS      *
      *     SCSBW362-E-EMPR              = CODIGO DA EMPRESA           *
      *     SCSBW362-E-EMPR-DEPEND       = COD EMPRESA DA DEPENDENCIA  *
      *     SCSBW362-E-DEPEND            = CODIGO DA DEPENDENCIA       *
      *     SCSBW362-E-CUSUAR            = CODIGO DO USUARIO           *
      *     SCSBW362-E-DATA-INI          = DATA INICIAL                *
      *     SCSBW362-E-DATA-FIM          = DATA FINAL                  *
      *     SCSBW362-E-SITUACAO          = SIT. DO REG. DE PAGAMENTO   *
      *   SCSBW362-BLOCO-SAIDA.                                        *
      *     SCSBW362-S-QTD-REG           = QTDE REGISTROS DEVOLVIDOS   *
      *     SCSBW362-S-LISTA.                                          *
      *       SCSBW362-S-ANO             = ANO RAS                     *
      *       SCSBW362-S-NREG            = NUMERO DO REGISTRO RAS      *
      *       SCSBW362-S-NPGTO           = NUMERO DO PAGAMENTO         *
      *       SCSBW362-S-CNPJ-PRINC      = CODIGO DO CNPJ (CORPO)      *
      *       SCSBW362-S-CNPJ-FLIAL      = CODIGO DA FILIAL DO CNPJ    *
      *       SCSBW362-S-CNPJ-CTRL       = CODIGO DO CONTROLE DO CNPJ  *
      *       SCSBW362-S-DESC-EMPR       = DESCRICAO DA EMPRESA        *
      *       SCSBW362-S-DT-INCLUSAO     = DATA DE INCLUSAO (CRIACAO)  *
      *                                                                *
      *----------------------------------------------------------------*
      * DATA       AUTOR             ALTERACAO                         *
      * ---------- ----------------- --------------------------------- *
      * 99/99/9999                   XXXXXXXXXXXXXXXXXXXXXXX           *
      *================================================================*
                                                                        
          05 SCSBW362-HEADER.                                           
             10 SCSBW362-COD-LAYOUT         PIC X(008) VALUE 'SCSBW362'.
             10 SCSBW362-TAM-LAYOUT         PIC 9(005) VALUE 6037.      
          05 SCSBW362-REGISTRO.                                         
             10 SCSBW362-BLOCO-ENTRADA.                                 
                15 SCSBW362-E-ANO                PIC 9(004).            
                15 SCSBW362-E-NREG               PIC 9(009).            
                15 SCSBW362-E-EMPR               PIC 9(010).            
                15 SCSBW362-E-EMPR-DEPEND        PIC 9(010).            
                15 SCSBW362-E-DEPEND             PIC 9(008).            
                15 SCSBW362-E-CUSUAR             PIC X(009).            
                15 SCSBW362-E-DATA-INI           PIC X(010).            
                15 SCSBW362-E-DATA-FIM           PIC X(010).            
                15 SCSBW362-E-SITUACAO           PIC X(001).            
             10 SCSBW362-BLOCO-SAIDA.                                   
                15 SCSBW362-S-QTD-REG            PIC 9(003).            
                15 SCSBW362-S-LISTA              OCCURS 0 TO 050 TIMES  
                      DEPENDING ON SCSBW362-S-QTD-REG.                  
                   20 SCSBW362-S-ANO             PIC 9(004).            
                   20 SCSBW362-S-NREG            PIC 9(009).            
                   20 SCSBW362-S-NPGTO           PIC 9(010).            
                   20 SCSBW362-S-CNPJ-PRINC      PIC 9(009).            
                   20 SCSBW362-S-CNPJ-FLIAL      PIC 9(005).            
                   20 SCSBW362-S-CNPJ-CTRL       PIC 9(002).            
                   20 SCSBW362-S-DESC-EMPR       PIC X(070).            
                   20 SCSBW362-S-DT-INCLUSAO     PIC X(010).            

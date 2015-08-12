      *================================================================*
      * NOME BOOK  : SCSBW306                                          *
      * DESCRICAO  : CONSULTAR A LISTA DOS REGISTROS DE FATURAMENTO    *
      *              PARA ALTERACAO.                                   *
      * COMUNICACAO: FRAMEWORK X COORDENADOR                           *
      * DATA       : 02/02/2015                                        *
      * AUTOR      : FERNANDA CARUSO                                   *
      * EMPRESA    : BRQ IT SERVICES                                   *
      * GRUPO      : 17 - CONTABILIDADE E IMPOSTOS                     *
      * COMPONENTE : SCSB - SISCOSERV BRADESCO                         *
      *================================================================*
      *                                                                *
      * SCSBW306-HEADER.                                               *
      *   SCSBW306-COD-LAYOUT            = CODIGO DO LAYOUT            *
      *   SCSBW306-TAM-LAYOUT            = TAMANHO DO LAYOUT           *
      * SCSBW306-REGISTRO.                                             *
      *   SCSBW306-BLOCO-ENTRADA.                                      *
      *     SCSBW306-E-ANO               = ANO RVS                     *
      *     SCSBW306-E-NREG              = NUMERO DO REGISTRO RVS      *
      *     SCSBW306-E-EMPR              = CODIGO DA EMPRESA           *
      *     SCSBW306-E-EMPR-DEPEND       = COD EMPRESA DA DEPENDENCIA  *
      *     SCSBW306-E-DEPEND            = CODIGO DA DEPENDENCIA       *
      *     SCSBW306-E-CUSUAR            = CODIGO DO USUARIO           *
      *     SCSBW306-E-DATA-INI          = DATA INICIAL                *
      *     SCSBW306-E-DATA-FIM          = DATA FINAL                  *
      *     SCSBW306-E-SITUACAO          = SIT. DO REG. DE FATURAMENTO *
      *   SCSBW306-BLOCO-SAIDA.                                        *
      *     SCSBW306-S-QTD-REG           = QTDE REGISTROS DEVOLVIDOS   *
      *     SCSBW306-S-LISTA.                                          *
      *       SCSBW306-S-ANO             = ANO RVS                     *
      *       SCSBW306-S-NREG            = NUMERO DO REGISTRO RVS      *
      *       SCSBW306-S-CFATMT          = NUMERO DO FATURAMENTO       *
      *       SCSBW306-S-CNPJ-PRINC      = CODIGO DO CNPJ (CORPO)      *
      *       SCSBW306-S-CNPJ-FLIAL      = CODIGO DA FILIAL DO CNPJ    *
      *       SCSBW306-S-CNPJ-CTRL       = CODIGO DO CONTROLE DO CNPJ  *
      *       SCSBW306-S-DESC-EMPR       = DESCRICAO DA EMPRESA        *
      *       SCSBW306-S-DT-INCLUSAO     = DATA DE INCLUSAO (CRIACAO)  *
      *                                                                *
      *----------------------------------------------------------------*
      * DATA       AUTOR             ALTERACAO                         *
      * ---------- ----------------- --------------------------------- *
      * 99/99/9999                   XXXXXXXXXXXXXXXXXXXXXXX           *
      *================================================================*
                                                                        
          05 SCSBW306-HEADER.                                           
             10 SCSBW306-COD-LAYOUT         PIC X(008) VALUE 'SCSBW306'.
             10 SCSBW306-TAM-LAYOUT         PIC 9(005) VALUE 6037.      
          05 SCSBW306-REGISTRO.                                         
             10 SCSBW306-BLOCO-ENTRADA.                                 
                15 SCSBW306-E-ANO                PIC 9(004).            
                15 SCSBW306-E-NREG               PIC 9(009).            
                15 SCSBW306-E-EMPR               PIC 9(010).            
                15 SCSBW306-E-EMPR-DEPEND        PIC 9(010).            
                15 SCSBW306-E-DEPEND             PIC 9(008).            
                15 SCSBW306-E-CUSUAR             PIC X(009).            
                15 SCSBW306-E-DATA-INI           PIC X(010).            
                15 SCSBW306-E-DATA-FIM           PIC X(010).            
                15 SCSBW306-E-SITUACAO           PIC X(001).            
             10 SCSBW306-BLOCO-SAIDA.                                   
                15 SCSBW306-S-QTD-REG            PIC 9(003).            
                15 SCSBW306-S-LISTA              OCCURS 0 TO 050 TIMES  
                      DEPENDING ON SCSBW306-S-QTD-REG.                  
                   20 SCSBW306-S-ANO             PIC 9(004).            
                   20 SCSBW306-S-NREG            PIC 9(009).            
                   20 SCSBW306-S-CFATMT          PIC 9(010).            
                   20 SCSBW306-S-CNPJ-PRINC      PIC 9(009).            
                   20 SCSBW306-S-CNPJ-FLIAL      PIC 9(005).            
                   20 SCSBW306-S-CNPJ-CTRL       PIC 9(002).            
                   20 SCSBW306-S-DESC-EMPR       PIC X(070).            
                   20 SCSBW306-S-DT-INCLUSAO     PIC X(010).            

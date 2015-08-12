      *================================================================*
      * NOME BOOK  : SCSBW192                                          *
      * DESCRICAO  : CONSULTAR LISTA DAS RVS RELACIONADAS A UM ADITIVO *
      *              PARA ALTERACAO.                                   *
      * COMUNICACAO: FRAMEWORK X COORDENADOR                           *
      * DATA       : 27/01/2015                                        *
      * AUTOR      : FERNANDA CARUSO                                   *
      * EMPRESA    : BRQ IT SERVICES                                   *
      * GRUPO      : 17 - CONTABILIDADE E IMPOSTOS                     *
      * COMPONENTE : SCSB - SISCOSERV BRADESCO                         *
      *================================================================*
      *                                                                *
      * SCSBW192-HEADER.                                               *
      *   SCSBW192-COD-LAYOUT            = CODIGO DO LAYOUT            *
      *   SCSBW192-TAM-LAYOUT            = TAMANHO DO LAYOUT           *
      * SCSBW192-REGISTRO.                                             *
      *   SCSBW192-BLOCO-ENTRADA.                                      *
      *     SCSBW192-E-ANO               = ANO RVS                     *
      *     SCSBW192-E-NREG              = NUMERO DO REGISTRO RVS      *
      *     SCSBW192-E-COPER-ADITIVO     = NUMERO OPERACAO DE ADITIVO  *
      *     SCSBW192-E-EMPR              = CODIGO DA EMPRESA           *
      *     SCSBW192-E-EMPR-DEPEND       = COD EMPRESA DA DEPENDENCIA  *
      *     SCSBW192-E-DEPEND            = CODIGO DA DEPENDENCIA       *
      *     SCSBW192-E-CUSUAR            = CODIGO DO USUARIO           *
      *     SCSBW192-E-DATA-INI          = DATA INICIAL                *
      *     SCSBW192-E-DATA-FIM          = DATA FINAL                  *
      *     SCSBW192-E-SITUACAO          = SITUACAO DO REGISTRO        *
      *                                    P -> PENDENTE DE VALIDACAO  *
      *                                    R -> REJEITADO VALIDADOR    *
      *                                    F -> REJEITADO REC FEDERAL  *
      *     SCSBW192-E-TPO-REG           = TIPO DO REGISTRO            *
      *                                    2 -> INCLUSAO DE ADITIVO    *
      *                                    4 -> RETIFICACAO DE ADITIVO *
      *   SCSBW192-BLOCO-SAIDA.                                        *
      *     SCSBW192-S-QTD-REG           = QTDE REGISTROS DEVOLVIDOS   *
      *     SCSBW192-S-LISTA.                                          *
      *       SCSBW192-S-ANO             = ANO RVS                     *
      *       SCSBW192-S-NREG            = NUMERO DO REGISTRO RVS      *
      *       SCSBW192-S-CNPJ-PRINC      = CODIGO DO CNPJ (CORPO)      *
      *       SCSBW192-S-CNPJ-FLIAL      = CODIGO DA FILIAL DO CNPJ    *
      *       SCSBW192-S-CNPJ-CTRL       = CODIGO DO CONTROLE DO CNPJ  *
      *       SCSBW192-S-DESC-EMPR       = DESCRICAO DA EMPRESA        *
      *       SCSBW192-S-DT-INCLUSAO     = DATA DE INCLUSAO (CRIACAO)  *
      *                                                                *
      *----------------------------------------------------------------*
      * DATA       AUTOR             ALTERACAO                         *
      * ---------- ----------------- --------------------------------- *
      * 99/99/9999                   XXXXXXXXXXXXXXXXXXXXXXX           *
      *================================================================*
                                                                        
          05 SCSBW192-HEADER.                                           
             10 SCSBW192-COD-LAYOUT         PIC X(008) VALUE 'SCSBW192'.
             10 SCSBW192-TAM-LAYOUT         PIC 9(005) VALUE 5548.      
          05 SCSBW192-REGISTRO.                                         
             10 SCSBW192-BLOCO-ENTRADA.                                 
                15 SCSBW192-E-ANO                PIC 9(004).            
                15 SCSBW192-E-NREG               PIC 9(009).            
                15 SCSBW192-E-COPER-ADITIVO      PIC 9(010).            
                15 SCSBW192-E-EMPR               PIC 9(010).            
                15 SCSBW192-E-EMPR-DEPEND        PIC 9(010).            
                15 SCSBW192-E-DEPEND             PIC 9(008).            
                15 SCSBW192-E-CUSUAR             PIC X(009).            
                15 SCSBW192-E-DATA-INI           PIC X(010).            
                15 SCSBW192-E-DATA-FIM           PIC X(010).            
                15 SCSBW192-E-SITUACAO           PIC X(001).            
                15 SCSBW192-E-TPO-REG            PIC 9(001).            
             10 SCSBW192-BLOCO-SAIDA.                                   
                15 SCSBW192-S-QTD-REG            PIC 9(003).            
                15 SCSBW192-S-LISTA              OCCURS 0 TO 050 TIMES  
                      DEPENDING ON SCSBW192-S-QTD-REG.                  
                   20 SCSBW192-S-ANO             PIC 9(004).            
                   20 SCSBW192-S-NREG            PIC 9(009).            
                   20 SCSBW192-S-CNPJ-PRINC      PIC 9(009).            
                   20 SCSBW192-S-CNPJ-FLIAL      PIC 9(005).            
                   20 SCSBW192-S-CNPJ-CTRL       PIC 9(002).            
                   20 SCSBW192-S-DESC-EMPR       PIC X(070).            
                   20 SCSBW192-S-DT-INCLUSAO     PIC X(010).            

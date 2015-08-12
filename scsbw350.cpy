      *================================================================*
      * NOME BOOK  : SCSBW350                                          *
      * DESCRICAO  : CONSULTAR LISTA DAS RAS RELACIONADAS A UM ADITIVO *
      *              PARA ALTERACAO.                                   *
      * COMUNICACAO: FRAMEWORK X COORDENADOR                           *
      * DATA       : 10/02/2015                                        *
      * AUTOR      : FERNANDA CARUSO                                   *
      * EMPRESA    : BRQ IT SERVICES                                   *
      * GRUPO      : 17 - CONTABILIDADE E IMPOSTOS                     *
      * COMPONENTE : SCSB - SISCOSERV BRADESCO                         *
      *================================================================*
      *                                                                *
      * SCSBW350-HEADER.                                               *
      *   SCSBW350-COD-LAYOUT            = CODIGO DO LAYOUT            *
      *   SCSBW350-TAM-LAYOUT            = TAMANHO DO LAYOUT           *
      * SCSBW350-REGISTRO.                                             *
      *   SCSBW350-BLOCO-ENTRADA.                                      *
      *     SCSBW350-E-ANO               = ANO RAS                     *
      *     SCSBW350-E-NREG              = NUMERO DO REGISTRO RAS      *
      *     SCSBW350-E-COPER-ADITIVO     = NUMERO OPERACAO DE ADITIVO  *
      *     SCSBW350-E-EMPR              = CODIGO DA EMPRESA           *
      *     SCSBW350-E-EMPR-DEPEND       = COD EMPRESA DA DEPENDENCIA  *
      *     SCSBW350-E-DEPEND            = CODIGO DA DEPENDENCIA       *
      *     SCSBW350-E-CUSUAR            = CODIGO DO USUARIO           *
      *     SCSBW350-E-DATA-INI          = DATA INICIAL                *
      *     SCSBW350-E-DATA-FIM          = DATA FINAL                  *
      *     SCSBW350-E-SITUACAO          = SITUACAO DO REGISTRO        *
      *                                    P -> PENDENTE DE VALIDACAO  *
      *                                    R -> REJEITADO VALIDADOR    *
      *                                    F -> REJEITADO REC FEDERAL  *
      *     SCSBW350-E-TPO-REG           = TIPO DO REGISTRO            *
      *                                    2 -> INCLUSAO DE ADITIVO    *
      *                                    4 -> RETIFICACAO DE ADITIVO *
      *   SCSBW350-BLOCO-SAIDA.                                        *
      *     SCSBW350-S-QTD-REG           = QTDE REGISTROS DEVOLVIDOS   *
      *     SCSBW350-S-LISTA.                                          *
      *       SCSBW350-S-ANO             = ANO RAS                     *
      *       SCSBW350-S-NREG            = NUMERO DO REGISTRO RAS      *
      *       SCSBW350-S-CNPJ-PRINC      = CODIGO DO CNPJ (CORPO)      *
      *       SCSBW350-S-CNPJ-FLIAL      = CODIGO DA FILIAL DO CNPJ    *
      *       SCSBW350-S-CNPJ-CTRL       = CODIGO DO CONTROLE DO CNPJ  *
      *       SCSBW350-S-DESC-EMPR       = DESCRICAO DA EMPRESA        *
      *       SCSBW350-S-DT-INCLUSAO     = DATA DE INCLUSAO (CRIACAO)  *
      *                                                                *
      *----------------------------------------------------------------*
      * DATA       AUTOR             ALTERACAO                         *
      * ---------- ----------------- --------------------------------- *
      * 99/99/9999                   XXXXXXXXXXXXXXXXXXXXXXX           *
      *================================================================*
                                                                        
          05 SCSBW350-HEADER.                                           
             10 SCSBW350-COD-LAYOUT         PIC X(008) VALUE 'SCSBW350'.
             10 SCSBW350-TAM-LAYOUT         PIC 9(005) VALUE 5548.      
          05 SCSBW350-REGISTRO.                                         
             10 SCSBW350-BLOCO-ENTRADA.                                 
                15 SCSBW350-E-ANO                PIC 9(004).            
                15 SCSBW350-E-NREG               PIC 9(009).            
                15 SCSBW350-E-COPER-ADITIVO      PIC 9(010).            
                15 SCSBW350-E-EMPR               PIC 9(010).            
                15 SCSBW350-E-EMPR-DEPEND        PIC 9(010).            
                15 SCSBW350-E-DEPEND             PIC 9(008).            
                15 SCSBW350-E-CUSUAR             PIC X(009).            
                15 SCSBW350-E-DATA-INI           PIC X(010).            
                15 SCSBW350-E-DATA-FIM           PIC X(010).            
                15 SCSBW350-E-SITUACAO           PIC X(001).            
                15 SCSBW350-E-TPO-REG            PIC 9(001).            
             10 SCSBW350-BLOCO-SAIDA.                                   
                15 SCSBW350-S-QTD-REG            PIC 9(003).            
                15 SCSBW350-S-LISTA              OCCURS 0 TO 050 TIMES  
                      DEPENDING ON SCSBW350-S-QTD-REG.                  
                   20 SCSBW350-S-ANO             PIC 9(004).            
                   20 SCSBW350-S-NREG            PIC 9(009).            
                   20 SCSBW350-S-CNPJ-PRINC      PIC 9(009).            
                   20 SCSBW350-S-CNPJ-FLIAL      PIC 9(005).            
                   20 SCSBW350-S-CNPJ-CTRL       PIC 9(002).            
                   20 SCSBW350-S-DESC-EMPR       PIC X(070).            
                   20 SCSBW350-S-DT-INCLUSAO     PIC X(010).            

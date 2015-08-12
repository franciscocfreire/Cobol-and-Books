      *================================================================*
      * NOME BOOK  : SCSBW326                                          *
      * DESCRICAO  : CONSULTAR A LISTA DAS RAS PARA ALTERACAO          *
      * COMUNICACAO: FRAMEWORK X COORDENADOR                           *
      * DATA       : 06/02/2015                                        *
      * AUTOR      : FERNANDA CARUSO                                   *
      * EMPRESA    : BRQ IT SERVICES                                   *
      * GRUPO      : 17 - CONTABILIDADE E IMPOSTOS                     *
      * COMPONENTE : SCSB - SISCOSERV BRADESCO                         *
      *================================================================*
      *                                                                *
      * SCSBW326-HEADER.                                               *
      *   SCSBW326-COD-LAYOUT            = CODIGO DO LAYOUT            *
      *   SCSBW326-TAM-LAYOUT            = TAMANHO DO LAYOUT           *
      * SCSBW326-REGISTRO.                                             *
      *   SCSBW326-BLOCO-ENTRADA.                                      *
      *     SCSBW326-E-ANO               = ANO RAS                     *
      *     SCSBW326-E-NREG              = NUMERO DO REGISTRO RAS      *
      *     SCSBW326-E-EMPR              = CODIGO DA EMPRESA           *
      *     SCSBW326-E-EMPR-DEPEND       = COD EMPRESA DA DEPENDENCIA  *
      *     SCSBW326-E-DEPEND            = CODIGO DA DEPENDENCIA       *
      *     SCSBW326-E-CUSUAR            = CODIGO DO USUARIO           *
      *     SCSBW326-E-DATA-INI          = DATA INICIAL                *
      *     SCSBW326-E-DATA-FIM          = DATA FINAL                  *
      *     SCSBW326-E-SITUACAO          = SITUACAO DO REGISTRO        *
      *                                    P -> PENDENTE DE VALIDACAO  *
      *                                    R -> REJEITADO VALIDADOR    *
      *                                    F -> REJEITADO REC FEDERAL  *
      *     SCSBW326-E-TPO-REG           = TIPO DO REGISTRO            *
      *                                    1 -> INCLUSAO DE RAS        *
      *                                    3 -> RETIFICACAO DE RAS     *
      *   SCSBW326-BLOCO-SAIDA.                                        *
      *     SCSBW326-S-QTD-REG           = QTDE REGISTROS DEVOLVIDOS   *
      *     SCSBW326-S-LISTA.                                          *
      *       SCSBW326-S-ANO             = ANO RAS                     *
      *       SCSBW326-S-NREG            = NUMERO DO REGISTRO RAS      *
      *       SCSBW326-S-CNPJ-PRINC      = CODIGO DO CNPJ (CORPO)      *
      *       SCSBW326-S-CNPJ-FLIAL      = CODIGO DA FILIAL DO CNPJ    *
      *       SCSBW326-S-CNPJ-CTRL       = CODIGO DO CONTROLE DO CNPJ  *
      *       SCSBW326-S-DESC-EMPR       = DESCRICAO DA EMPRESA        *
      *       SCSBW326-S-DT-INCLUSAO     = DATA DE INCLUSAO (CRIACAO)  *
      *                                                                *
      *----------------------------------------------------------------*
      * DATA       AUTOR             ALTERACAO                         *
      * ---------- ----------------- --------------------------------- *
      * 99/99/9999                   XXXXXXXXXXXXXXXXXXXXXXX           *
      *================================================================*
                                                                        
          05 SCSBW326-HEADER.                                           
             10 SCSBW326-COD-LAYOUT         PIC X(008) VALUE 'SCSBW326'.
             10 SCSBW326-TAM-LAYOUT         PIC 9(005) VALUE 5488.      
          05 SCSBW326-REGISTRO.                                         
             10 SCSBW326-BLOCO-ENTRADA.                                 
                15 SCSBW326-E-ANO                PIC 9(004).            
                15 SCSBW326-E-NREG               PIC 9(009).            
                15 SCSBW326-E-EMPR               PIC 9(010).            
                15 SCSBW326-E-EMPR-DEPEND        PIC 9(010).            
                15 SCSBW326-E-DEPEND             PIC 9(008).            
                15 SCSBW326-E-CUSUAR             PIC X(009).            
                15 SCSBW326-E-DATA-INI           PIC X(010).            
                15 SCSBW326-E-DATA-FIM           PIC X(010).            
                15 SCSBW326-E-SITUACAO           PIC X(001).            
                15 SCSBW326-E-TPO-REG            PIC 9(001).            
             10 SCSBW326-BLOCO-SAIDA.                                   
                15 SCSBW326-S-QTD-REG            PIC 9(003).            
                15 SCSBW326-S-LISTA              OCCURS 0 TO 050 TIMES  
                      DEPENDING ON SCSBW326-S-QTD-REG.                  
                   20 SCSBW326-S-ANO             PIC 9(004).            
                   20 SCSBW326-S-NREG            PIC 9(009).            
                   20 SCSBW326-S-CNPJ-PRINC      PIC 9(009).            
                   20 SCSBW326-S-CNPJ-FLIAL      PIC 9(004).            
                   20 SCSBW326-S-CNPJ-CTRL       PIC 9(002).            
                   20 SCSBW326-S-DESC-EMPR       PIC X(070).            
                   20 SCSBW326-S-DT-INCLUSAO     PIC X(010).            

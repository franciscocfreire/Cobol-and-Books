      *================================================================*
      * NOME BOOK  : SCSBW104                                          *
      * DESCRICAO  : CONSULTAR LISTA DAS RAS RELACIONADAS A UM ADITIVO *
      * COMUNICACAO: FRAMEWORK X COORDENADOR                           *
      * DATA       : 14/06/2013                                        *
      * AUTOR      : FERNANDA CARUSO                                   *
      * EMPRESA    : BRQ IT SERVICES                                   *
      * GRUPO      : 17 - CONTABILIDADE E IMPOSTOS                     *
      * COMPONENTE : SCSB - SISCOSERV BRADESCO                         *
      *================================================================*
      *                                                                *
      * SCSBW104-HEADER.                                               *
      *   SCSBW104-COD-LAYOUT            = CODIGO DO LAYOUT            *
      *   SCSBW104-TAM-LAYOUT            = TAMANHO DO LAYOUT           *
      * SCSBW104-REGISTRO.                                             *
      *   SCSBW104-BLOCO-ENTRADA.                                      *
      *     SCSBW104-E-ANO               = ANO RAS                     *
      *     SCSBW104-E-NREG              = NUMERO DO REGISTRO RAS      *
      *     SCSBW104-E-COPER-ADITIVO     = NUMERO OPERACAO DE ADITIVO  *
      *     SCSBW104-E-EMPR              = CODIGO DA EMPRESA           *
      *     SCSBW104-E-EMPR-DEPEND       = COD EMPRESA DA DEPENDENCIA  *
      *     SCSBW104-E-DEPEND            = CODIGO DA DEPENDENCIA       *
      *     SCSBW104-E-CUSUAR            = CODIGO DO USUARIO           *
      *     SCSBW104-E-DATA-INI          = DATA INICIAL                *
      *     SCSBW104-E-DATA-FIM          = DATA FINAL                  *
      *     SCSBW104-E-SITUACAO          = SITUACAO DO REGISTRO        *
      *   SCSBW104-BLOCO-SAIDA.                                        *
      *     SCSBW104-S-QTD-REG           = QTDE REGISTROS DEVOLVIDOS   *
      *     SCSBW104-S-LISTA.                                          *
      *       SCSBW104-S-ANO             = ANO RAS                     *
      *       SCSBW104-S-NREG            = NUMERO DO REGISTRO RAS      *
      *       SCSBW104-S-CNPJ-PRINC      = CODIGO DO CNPJ (CORPO)      *
      *       SCSBW104-S-CNPJ-FLIAL      = CODIGO DA FILIAL DO CNPJ    *
      *       SCSBW104-S-CNPJ-CTRL       = CODIGO DO CONTROLE DO CNPJ  *
      *       SCSBW104-S-DESC-EMPR       = DESCRICAO DA EMPRESA        *
      *       SCSBW104-S-DT-INCLUSAO     = DATA DE INCLUSAO (CRIACAO)  *
      *                                                                *
      *----------------------------------------------------------------*
      * DATA       AUTOR             ALTERACAO                         *
      * ---------- ----------------- --------------------------------- *
      * 10/02/2015 FERNANDA CARUSO   EXCLUSAO DOS CAMPOS SCSBW104-E-   *
      *                              CPF-CNPJ-PRINC/FLIAL/CTRL.        *
      *                              INCLUSAO DOS CAMPOS SCSBW104-E-   *
      *                              EMPR/EMPR-DEPEND/DEPEND/CUSUAR/   *
      *                              SCSBW104-S-DESC-EMPR.             *
      *                              ALTERACAO DO NOME DOS CAMPOS      *
      *                              SCSBW104-S-CPF-CNPJ-PRINC/FLIAL/  *
      *                              CTRL.                             *
      *================================================================*
                                                                        
          05 SCSBW104-HEADER.                                           
             10 SCSBW104-COD-LAYOUT         PIC X(008) VALUE 'SCSBW104'.
             10 SCSBW104-TAM-LAYOUT         PIC 9(005) VALUE 5547.      
          05 SCSBW104-REGISTRO.                                         
             10 SCSBW104-BLOCO-ENTRADA.                                 
                15 SCSBW104-E-ANO                PIC 9(004).            
                15 SCSBW104-E-NREG               PIC 9(009).            
                15 SCSBW104-E-COPER-ADITIVO      PIC 9(010).            
                15 SCSBW104-E-EMPR               PIC 9(010).            
                15 SCSBW104-E-EMPR-DEPEND        PIC 9(010).            
                15 SCSBW104-E-DEPEND             PIC 9(008).            
                15 SCSBW104-E-CUSUAR             PIC X(009).            
                15 SCSBW104-E-DATA-INI           PIC X(010).            
                15 SCSBW104-E-DATA-FIM           PIC X(010).            
                15 SCSBW104-E-SITUACAO           PIC X(001).            
             10 SCSBW104-BLOCO-SAIDA.                                   
                15 SCSBW104-S-QTD-REG            PIC 9(003).            
                15 SCSBW104-S-LISTA              OCCURS 0 TO 050 TIMES  
                      DEPENDING ON SCSBW104-S-QTD-REG.                  
                   20 SCSBW104-S-ANO             PIC 9(004).            
                   20 SCSBW104-S-NREG            PIC 9(009).            
                   20 SCSBW104-S-CNPJ-PRINC      PIC 9(009).            
                   20 SCSBW104-S-CNPJ-FLIAL      PIC 9(005).            
                   20 SCSBW104-S-CNPJ-CTRL       PIC 9(002).            
                   20 SCSBW104-S-DESC-EMPR       PIC X(070).            
                   20 SCSBW104-S-DT-INCLUSAO     PIC X(010).            

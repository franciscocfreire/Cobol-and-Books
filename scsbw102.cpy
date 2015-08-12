      *================================================================*
      * NOME BOOK  : SCSBW102                                          *
      * DESCRICAO  : CONSULTAR LISTA DAS RVS RELACIONADAS A UM ADITIVO *
      * COMUNICACAO: FRAMEWORK X COORDENADOR                           *
      * DATA       : 13/06/2013                                        *
      * AUTOR      : FERNANDA CARUSO                                   *
      * EMPRESA    : BRQ IT SERVICES                                   *
      * GRUPO      : 17 - CONTABILIDADE E IMPOSTOS                     *
      * COMPONENTE : SCSB - SISCOSERV BRADESCO                         *
      *================================================================*
      *                                                                *
      * SCSBW102-HEADER.                                               *
      *   SCSBW102-COD-LAYOUT            = CODIGO DO LAYOUT            *
      *   SCSBW102-TAM-LAYOUT            = TAMANHO DO LAYOUT           *
      * SCSBW102-REGISTRO.                                             *
      *   SCSBW102-BLOCO-ENTRADA.                                      *
      *     SCSBW102-E-ANO               = ANO RVS                     *
      *     SCSBW102-E-NREG              = NUMERO DO REGISTRO RVS      *
      *     SCSBW102-E-COPER-ADITIVO     = NUMERO OPERACAO DE ADITIVO  *
      *     SCSBW102-E-EMPR              = CODIGO DA EMPRESA           *
      *     SCSBW102-E-EMPR-DEPEND       = COD EMPRESA DA DEPENDENCIA  *
      *     SCSBW102-E-DEPEND            = CODIGO DA DEPENDENCIA       *
      *     SCSBW102-E-CUSUAR            = CODIGO DO USUARIO           *
      *     SCSBW102-E-DATA-INI          = DATA INICIAL                *
      *     SCSBW102-E-DATA-FIM          = DATA FINAL                  *
      *     SCSBW102-E-SITUACAO          = SITUACAO DO REGISTRO        *
      *   SCSBW102-BLOCO-SAIDA.                                        *
      *     SCSBW102-S-QTD-REG           = QTDE REGISTROS DEVOLVIDOS   *
      *     SCSBW102-S-LISTA.                                          *
      *       SCSBW102-S-ANO             = ANO RVS                     *
      *       SCSBW102-S-NREG            = NUMERO DO REGISTRO RVS      *
      *       SCSBW102-S-CNPJ-PRINC      = CODIGO DO CNPJ (CORPO)      *
      *       SCSBW102-S-CNPJ-FLIAL      = CODIGO DA FILIAL DO CNPJ    *
      *       SCSBW102-S-CNPJ-CTRL       = CODIGO DO CONTROLE DO CNPJ  *
      *       SCSBW102-S-DESC-EMPR       = DESCRICAO DA EMPRESA        *
      *       SCSBW102-S-DT-INCLUSAO     = DATA DE INCLUSAO (CRIACAO)  *
      *                                                                *
      *----------------------------------------------------------------*
      * DATA       AUTOR             ALTERACAO                         *
      * ---------- ----------------- --------------------------------- *
      * 27/01/2015 FERNANDA CARUSO   EXCLUSAO DOS CAMPOS SCSBW102-E-   *
      *                              CPF-CNPJ-PRINC/FLIAL/CTRL.        *
      *                              INCLUSAO DOS CAMPOS SCSBW102-E-   *
      *                              EMPR/EMPR-DEPEND/DEPEND/CUSUAR/   *
      *                              SCSBW102-S-DESC-EMPR.             *
      *                              ALTERACAO DO NOME DOS CAMPOS      *
      *                              SCSBW102-S-CPF-CNPJ-PRINC/FLIAL/  *
      *                              CTRL.                             *
      *================================================================*
                                                                        
          05 SCSBW102-HEADER.                                           
             10 SCSBW102-COD-LAYOUT         PIC X(008) VALUE 'SCSBW102'.
             10 SCSBW102-TAM-LAYOUT         PIC 9(005) VALUE 5547.      
          05 SCSBW102-REGISTRO.                                         
             10 SCSBW102-BLOCO-ENTRADA.                                 
                15 SCSBW102-E-ANO                PIC 9(004).            
                15 SCSBW102-E-NREG               PIC 9(009).            
                15 SCSBW102-E-COPER-ADITIVO      PIC 9(010).            
                15 SCSBW102-E-EMPR               PIC 9(010).            
                15 SCSBW102-E-EMPR-DEPEND        PIC 9(010).            
                15 SCSBW102-E-DEPEND             PIC 9(008).            
                15 SCSBW102-E-CUSUAR             PIC X(009).            
                15 SCSBW102-E-DATA-INI           PIC X(010).            
                15 SCSBW102-E-DATA-FIM           PIC X(010).            
                15 SCSBW102-E-SITUACAO           PIC X(001).            
             10 SCSBW102-BLOCO-SAIDA.                                   
                15 SCSBW102-S-QTD-REG            PIC 9(003).            
                15 SCSBW102-S-LISTA              OCCURS 0 TO 050 TIMES  
                      DEPENDING ON SCSBW102-S-QTD-REG.                  
                   20 SCSBW102-S-ANO             PIC 9(004).            
                   20 SCSBW102-S-NREG            PIC 9(009).            
                   20 SCSBW102-S-CNPJ-PRINC      PIC 9(009).            
                   20 SCSBW102-S-CNPJ-FLIAL      PIC 9(005).            
                   20 SCSBW102-S-CNPJ-CTRL       PIC 9(002).            
                   20 SCSBW102-S-DESC-EMPR       PIC X(070).            
                   20 SCSBW102-S-DT-INCLUSAO     PIC X(010).            

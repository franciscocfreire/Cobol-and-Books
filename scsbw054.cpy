      *================================================================*
      * NOME BOOK  : SCSBW054                                          *
      * DESCRICAO  : CONSULTAR A LISTA DAS RAS                         *
      * COMUNICACAO: FRAMEWORK X COORDENADOR                           *
      * DATA       : 02/03/2013                                        *
      * AUTOR      : FERNANDA CARUSO                                   *
      * EMPRESA    : BRQ IT SERVICES                                   *
      * GRUPO      : 17 - CONTABILIDADE E IMPOSTOS                     *
      * COMPONENTE : SCSB - SISCOSERV BRADESCO                         *
      *================================================================*
      *                                                                *
      * SCSBW054-HEADER.                                               *
      *   SCSBW054-COD-LAYOUT            = CODIGO DO LAYOUT            *
      *   SCSBW054-TAM-LAYOUT            = TAMANHO DO LAYOUT           *
      * SCSBW054-REGISTRO.                                             *
      *   SCSBW054-BLOCO-ENTRADA.                                      *
      *     SCSBW054-E-ANO               = ANO RAS                     *
      *     SCSBW054-E-NREG              = NUMERO DO REGISTRO RAS      *
      *     SCSBW054-E-EMPR              = CODIGO DA EMPRESA           *
      *     SCSBW054-E-EMPR-DEPEND       = COD EMPRESA DA DEPENDENCIA  *
      *     SCSBW054-E-DEPEND            = CODIGO DA DEPENDENCIA       *
      *     SCSBW054-E-CUSUAR            = CODIGO DO USUARIO           *
      *     SCSBW054-E-DATA-INI          = DATA INICIAL                *
      *     SCSBW054-E-DATA-FIM          = DATA FINAL                  *
      *     SCSBW054-E-SITUACAO          = SITUACAO DO REGISTRO        *
      *   SCSBW054-BLOCO-SAIDA.                                        *
      *     SCSBW054-S-QTD-REG           = QTDE REGISTROS DEVOLVIDOS   *
      *     SCSBW054-S-LISTA.                                          *
      *       SCSBW054-S-ANO             = ANO RAS                     *
      *       SCSBW054-S-NREG            = NUMERO DO REGISTRO RAS      *
      *       SCSBW054-S-CNPJ-PRINC      = CODIGO DO CNPJ (CORPO)      *
      *       SCSBW054-S-CNPJ-FLIAL      = CODIGO DA FILIAL DO CNPJ    *
      *       SCSBW054-S-CNPJ-CTRL       = CODIGO DO CONTROLE DO CNPJ  *
      *       SCSBW054-S-DESC-EMPR       = DESCRICAO DA EMPRESA        *
      *       SCSBW054-S-DT-INCLUSAO     = DATA DE INCLUSAO (CRIACAO)  *
      *                                                                *
      *----------------------------------------------------------------*
      * DATA       AUTOR             ALTERACAO                         *
      * ---------- ----------------- --------------------------------- *
      * 04/02/2015 FERNANDA CARUSO   EXCLUSAO DOS CAMPOS SCSBW054-E-   *
      *                              CPF-CNPJ-PRINC/FLIAL/CTRL.        *
      *                              INCLUSAO DOS CAMPOS SCSBW054-E-   *
      *                              EMPR/EMPR-DEPEND/DEPEND/CUSUAR/   *
      *                              SCSBW054-S-DESC-EMPR.             *
      *                              ALTERACAO DO NOME DOS CAMPOS      *
      *                              SCSBW054-S-CPF-CNPJ-PRINC/FLIAL/  *
      *                              CTRL.                             *
      *================================================================*
                                                                        
          05 SCSBW054-HEADER.                                           
             10 SCSBW054-COD-LAYOUT         PIC X(008) VALUE 'SCSBW054'.
             10 SCSBW054-TAM-LAYOUT         PIC 9(005) VALUE 5487.      
          05 SCSBW054-REGISTRO.                                         
             10 SCSBW054-BLOCO-ENTRADA.                                 
                15 SCSBW054-E-ANO                PIC 9(004).            
                15 SCSBW054-E-NREG               PIC 9(009).            
                15 SCSBW054-E-EMPR               PIC 9(010).            
                15 SCSBW054-E-EMPR-DEPEND        PIC 9(010).            
                15 SCSBW054-E-DEPEND             PIC 9(008).            
                15 SCSBW054-E-CUSUAR             PIC X(009).            
                15 SCSBW054-E-DATA-INI           PIC X(010).            
                15 SCSBW054-E-DATA-FIM           PIC X(010).            
                15 SCSBW054-E-SITUACAO           PIC X(001).            
             10 SCSBW054-BLOCO-SAIDA.                                   
                15 SCSBW054-S-QTD-REG            PIC 9(003).            
                15 SCSBW054-S-LISTA              OCCURS 0 TO 050 TIMES  
                      DEPENDING ON SCSBW054-S-QTD-REG.                  
                   20 SCSBW054-S-ANO             PIC 9(004).            
                   20 SCSBW054-S-NREG            PIC 9(009).            
                   20 SCSBW054-S-CNPJ-PRINC      PIC 9(009).            
                   20 SCSBW054-S-CNPJ-FLIAL      PIC 9(004).            
                   20 SCSBW054-S-CNPJ-CTRL       PIC 9(002).            
                   20 SCSBW054-S-DESC-EMPR       PIC X(070).            
                   20 SCSBW054-S-DT-INCLUSAO     PIC X(010).            

      *================================================================*
      * NOME BOOK  : SCSBW066                                          *
      * DESCRICAO  : CONSULTAR A LISTA DOS REGISTROS DE PAGAMENTO      *
      * COMUNICACAO: FRAMEWORK X COORDENADOR                           *
      * DATA       : 02/03/2013                                        *
      * AUTOR      : FERNANDA CARUSO                                   *
      * EMPRESA    : BRQ IT SERVICES                                   *
      * GRUPO      : 17 - CONTABILIDADE E IMPOSTOS                     *
      * COMPONENTE : SCSB - SISCOSERV BRADESCO                         *
      *================================================================*
      *                                                                *
      * SCSBW066-HEADER.                                               *
      *   SCSBW066-COD-LAYOUT            = CODIGO DO LAYOUT            *
      *   SCSBW066-TAM-LAYOUT            = TAMANHO DO LAYOUT           *
      * SCSBW066-REGISTRO.                                             *
      *   SCSBW066-BLOCO-ENTRADA.                                      *
      *     SCSBW066-E-ANO               = ANO RAS                     *
      *     SCSBW066-E-NREG              = NUMERO DO REGISTRO RAS      *
      *     SCSBW066-E-EMPR              = CODIGO DA EMPRESA           *
      *     SCSBW066-E-EMPR-DEPEND       = COD EMPRESA DA DEPENDENCIA  *
      *     SCSBW066-E-DEPEND            = CODIGO DA DEPENDENCIA       *
      *     SCSBW066-E-CUSUAR            = CODIGO DO USUARIO           *
      *     SCSBW066-E-DATA-INI          = DATA INICIAL                *
      *     SCSBW066-E-DATA-FIM          = DATA FINAL                  *
      *     SCSBW066-E-SITUACAO          = SIT. DO REG. DE PAGAMENTO   *
      *   SCSBW066-BLOCO-SAIDA.                                        *
      *     SCSBW066-S-QTD-REG           = QTDE REGISTROS DEVOLVIDOS   *
      *     SCSBW066-S-LISTA.                                          *
      *       SCSBW066-S-ANO             = ANO RAS                     *
      *       SCSBW066-S-NREG            = NUMERO DO REGISTRO RAS      *
      *       SCSBW066-S-CPAGTO          = NUMERO DO PAGAMENTO         *
      *       SCSBW066-S-CNPJ-PRINC      = CODIGO DO CNPJ (CORPO)      *
      *       SCSBW066-S-CNPJ-FLIAL      = CODIGO DA FILIAL DO CNPJ    *
      *       SCSBW066-S-CNPJ-CTRL       = CODIGO DO CONTROLE DO CNPJ  *
      *       SCSBW066-S-DESC-EMPR       = DESCRICAO DA EMPRESA        *
      *       SCSBW066-S-DT-INCLUSAO     = DATA DE INCLUSAO (CRIACAO)  *
      *       SCSBW066-S-DT-CANCEL       = DATA DE CANCELAMENTO        *
      *                                                                *
      *----------------------------------------------------------------*
      * DATA       AUTOR             ALTERACAO                         *
      * ---------- ----------------- --------------------------------- *
      * 11/02/2015 FERNANDA CARUSO   EXCLUSAO DOS CAMPOS SCSBW066-E-   *
      *                              CPF-CNPJ-PRINC/FLIAL/CTRL.        *
      *                              INCLUSAO DOS CAMPOS SCSBW066-E-   *
      *                              EMPR/EMPR-DEPEND/DEPEND/CUSUAR/   *
      *                              SCSBW066-S-DESC-EMPR.             *
      *                              ALTERACAO DO NOME DOS CAMPOS      *
      *                              SCSBW066-S-CPF-CNPJ-PRINC/FLIAL/  *
      *                              CTRL.                             *
      *================================================================*
                                                                        
          05 SCSBW066-HEADER.                                           
             10 SCSBW066-COD-LAYOUT         PIC X(008) VALUE 'SCSBW066'.
             10 SCSBW066-TAM-LAYOUT         PIC 9(005) VALUE 6537.      
          05 SCSBW066-REGISTRO.                                         
             10 SCSBW066-BLOCO-ENTRADA.                                 
                15 SCSBW066-E-ANO                PIC 9(004).            
                15 SCSBW066-E-NREG               PIC 9(009).            
                15 SCSBW066-E-EMPR               PIC 9(010).            
                15 SCSBW066-E-EMPR-DEPEND        PIC 9(010).            
                15 SCSBW066-E-DEPEND             PIC 9(008).            
                15 SCSBW066-E-CUSUAR             PIC X(009).            
                15 SCSBW066-E-DATA-INI           PIC X(010).            
                15 SCSBW066-E-DATA-FIM           PIC X(010).            
                15 SCSBW066-E-SITUACAO           PIC X(001).            
             10 SCSBW066-BLOCO-SAIDA.                                   
                15 SCSBW066-S-QTD-REG            PIC 9(003).            
                15 SCSBW066-S-LISTA              OCCURS 0 TO 050 TIMES  
                      DEPENDING ON SCSBW066-S-QTD-REG.                  
                   20 SCSBW066-S-ANO             PIC 9(004).            
                   20 SCSBW066-S-NREG            PIC 9(009).            
                   20 SCSBW066-S-CPAGTO          PIC 9(010).            
                   20 SCSBW066-S-CNPJ-PRINC      PIC 9(009).            
                   20 SCSBW066-S-CNPJ-FLIAL      PIC 9(005).            
                   20 SCSBW066-S-CNPJ-CTRL       PIC 9(002).            
                   20 SCSBW066-S-DESC-EMPR       PIC X(070).            
                   20 SCSBW066-S-DT-INCLUSAO     PIC X(010).            
                   20 SCSBW066-S-DT-CANCEL       PIC X(010).            

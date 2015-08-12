      *================================================================*
      * NOME BOOK  : SCSBW042                                          *
      * DESCRICAO  : CONSULTAR A LISTA DOS REGISTROS DE FATURAMENTO    *
      * COMUNICACAO: FRAMEWORK X COORDENADOR                           *
      * DATA       : 02/03/2013                                        *
      * AUTOR      : FERNANDA CARUSO                                   *
      * EMPRESA    : BRQ IT SERVICES                                   *
      * GRUPO      : 17 - CONTABILIDADE E IMPOSTOS                     *
      * COMPONENTE : SCSB - SISCOSERV BRADESCO                         *
      *================================================================*
      *                                                                *
      * SCSBW042-HEADER.                                               *
      *   SCSBW042-COD-LAYOUT            = CODIGO DO LAYOUT            *
      *   SCSBW042-TAM-LAYOUT            = TAMANHO DO LAYOUT           *
      * SCSBW042-REGISTRO.                                             *
      *   SCSBW042-BLOCO-ENTRADA.                                      *
      *     SCSBW042-E-ANO               = ANO RVS                     *
      *     SCSBW042-E-NREG              = NUMERO DO REGISTRO RVS      *
      *     SCSBW042-E-EMPR              = CODIGO DA EMPRESA           *
      *     SCSBW042-E-EMPR-DEPEND       = COD EMPRESA DA DEPENDENCIA  *
      *     SCSBW042-E-DEPEND            = CODIGO DA DEPENDENCIA       *
      *     SCSBW042-E-CUSUAR            = CODIGO DO USUARIO           *
      *     SCSBW042-E-DATA-INI          = DATA INICIAL                *
      *     SCSBW042-E-DATA-FIM          = DATA FINAL                  *
      *     SCSBW042-E-SITUACAO          = SIT. DO REG. DE FATURAMENTO *
      *   SCSBW042-BLOCO-SAIDA.                                        *
      *     SCSBW042-S-QTD-REG           = QTDE REGISTROS DEVOLVIDOS   *
      *     SCSBW042-S-LISTA.                                          *
      *       SCSBW042-S-ANO             = ANO RVS                     *
      *       SCSBW042-S-NREG            = NUMERO DO REGISTRO RVS      *
      *       SCSBW042-S-CFATMT          = NUMERO DO FATURAMENTO       *
      *       SCSBW042-S-CNPJ-PRINC      = CODIGO DO CNPJ (CORPO)      *
      *       SCSBW042-S-CNPJ-FLIAL      = CODIGO DA FILIAL DO CNPJ    *
      *       SCSBW042-S-CNPJ-CTRL       = CODIGO DO CONTROLE DO CNPJ  *
      *       SCSBW042-S-DESC-EMPR       = DESCRICAO DA EMPRESA        *
      *       SCSBW042-S-DT-INCLUSAO     = DATA DE INCLUSAO (CRIACAO)  *
      *       SCSBW042-S-DT-CANCEL       = DATA DE CANCELAMENTO        *
      *                                                                *
      *----------------------------------------------------------------*
      * DATA       AUTOR             ALTERACAO                         *
      * ---------- ----------------- --------------------------------- *
      * 02/02/2015 FERNANDA CARUSO   EXCLUSAO DOS CAMPOS SCSBW042-E-   *
      *                              CPF-CNPJ-PRINC/FLIAL/CTRL.        *
      *                              INCLUSAO DOS CAMPOS SCSBW042-E-   *
      *                              EMPR/EMPR-DEPEND/DEPEND/CUSUAR/   *
      *                              SCSBW042-S-DESC-EMPR.             *
      *                              ALTERACAO DO NOME DOS CAMPOS      *
      *                              SCSBW042-S-CPF-CNPJ-PRINC/FLIAL/  *
      *                              CTRL.                             *
      *================================================================*
                                                                        
          05 SCSBW042-HEADER.                                           
             10 SCSBW042-COD-LAYOUT         PIC X(008) VALUE 'SCSBW042'.
             10 SCSBW042-TAM-LAYOUT         PIC 9(005) VALUE 6537.      
          05 SCSBW042-REGISTRO.                                         
             10 SCSBW042-BLOCO-ENTRADA.                                 
                15 SCSBW042-E-ANO                PIC 9(004).            
                15 SCSBW042-E-NREG               PIC 9(009).            
                15 SCSBW042-E-EMPR               PIC 9(010).            
                15 SCSBW042-E-EMPR-DEPEND        PIC 9(010).            
                15 SCSBW042-E-DEPEND             PIC 9(008).            
                15 SCSBW042-E-CUSUAR             PIC X(009).            
                15 SCSBW042-E-DATA-INI           PIC X(010).            
                15 SCSBW042-E-DATA-FIM           PIC X(010).            
                15 SCSBW042-E-SITUACAO           PIC X(001).            
             10 SCSBW042-BLOCO-SAIDA.                                   
                15 SCSBW042-S-QTD-REG            PIC 9(003).            
                15 SCSBW042-S-LISTA              OCCURS 0 TO 050 TIMES  
                      DEPENDING ON SCSBW042-S-QTD-REG.                  
                   20 SCSBW042-S-ANO             PIC 9(004).            
                   20 SCSBW042-S-NREG            PIC 9(009).            
                   20 SCSBW042-S-CFATMT          PIC 9(010).            
                   20 SCSBW042-S-CNPJ-PRINC      PIC 9(009).            
                   20 SCSBW042-S-CNPJ-FLIAL      PIC 9(005).            
                   20 SCSBW042-S-CNPJ-CTRL       PIC 9(002).            
                   20 SCSBW042-S-DESC-EMPR       PIC X(070).            
                   20 SCSBW042-S-DT-INCLUSAO     PIC X(010).            
                   20 SCSBW042-S-DT-CANCEL       PIC X(010).            

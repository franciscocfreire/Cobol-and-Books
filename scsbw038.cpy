      *================================================================*
      * NOME BOOK  : SCSBW038                                          *
      * DESCRICAO  : CONSULTAR RVS P/ INCLUIR REGISTRO DE FATURAMENTO  *
      * COMUNICACAO: FRAMEWORK X COORDENADOR                           *
      * DATA       : 02/03/2013                                        *
      * AUTOR      : FERNANDA CARUSO                                   *
      * EMPRESA    : BRQ IT SERVICES                                   *
      * GRUPO      : 17 - CONTABILIDADE E IMPOSTOS                     *
      * COMPONENTE : SCSB - SISCOSERV BRADESCO                         *
      *================================================================*
      *                                                                *
      * SCSBW038-HEADER.                                               *
      *   SCSBW038-COD-LAYOUT            = CODIGO DO LAYOUT            *
      *   SCSBW038-TAM-LAYOUT            = TAMANHO DO LAYOUT           *
      * SCSBW038-REGISTRO.                                             *
      *   SCSBW038-BLOCO-ENTRADA.                                      *
      *     SCSBW038-E-ANO               = ANO RVS                     *
      *     SCSBW038-E-NREG              = NUMERO DO REGISTRO RVS      *
      *   SCSBW038-BLOCO-SAIDA.                                        *
      *     SCSBW038-S-CPF-CNPJ-PRINC    = CPF/CNPJ PRINCIPAL          *
      *     SCSBW038-S-CPF-CNPJ-FLIAL    = CPF/CNPJ FILIAL             *
      *     SCSBW038-S-CPF-CNPJ-CTRL     = CPF/CNPJ CONTROLE           *
      *     SCSBW038-S-DS-EMPR           = DESCRICAO DA EMPRESA        *
      *     SCSBW038-S-CD-DEPEND-SAP     = CODIGO DA DEPENDENCIA SAP   *
      *     SCSBW038-S-DS-DEPEND         = DESCRICAO DA DEPENDENCIA    *
      *     SCSBW038-S-NOME-ADQUIR       = NOME DO ADQUIRENTE          *
      *     SCSBW038-S-END-ADQUIR        = ENDERECO DO ADQUIRENTE      *
      *     SCSBW038-S-CD-PAIS-ADQUIR    = CODIGO DO PAIS ADQUIRENTE   *
      *     SCSBW038-S-DS-PAIS-ADQUIR    = DESCRICAO PAIS ADQUIRENTE   *
      *     SCSBW038-S-CD-MOEDA          = CODIGO DA MOEDA             *
      *     SCSBW038-S-DS-MOEDA          = DESCRICAO DA MOEDA          *
      *     SCSBW038-S-VLR-CONV-REAL     = VALOR TOTAL CONVERTIDO REAL *
      *     SCSBW038-S-QTD-REG           = QTDE REGISTROS DEVOLVIDOS   *
      *     SCSBW038-S-LISTA.                                          *
      *       SCSBW038-S-COPER           = NUMERO DA OPERACAO          *
      *       SCSBW038-S-CD-NBS          = CODIGO NBS                  *
      *       SCSBW038-S-DS-NBS          = DESCRICAO NBS               *
      *       SCSBW038-S-CD-PAIS-DESTNO  = CODIGO PAIS DE DESTINO      *
      *       SCSBW038-S-DS-PAIS-DESTNO  = DESCRICAO PAIS DE DESTINO   *
      *       SCSBW038-S-CD-MODO-PREST   = CODIGO MODO DE PRESTACAO    *
      *       SCSBW038-S-DS-MODO-PREST   = DESCRICAO MODO DE PRESTACAO *
      *       SCSBW038-S-DT-INICIO       = DATA DE INICIO              *
      *       SCSBW038-S-DT-CONCLUSAO    = DATA DE CONCLUSAO           *
      *       SCSBW038-S-VALOR           = VALOR DA OPERACAO           *
      *       SCSBW038-S-SALDO-FATURAR   = SALDO A FATURAR             *
      *       SCSBW038-S-OPER-BAIXA      = OPERACAO BAIXA PLATAFORMA   *
      *                                    V (VISUALIZAR) / E (EDITAR) *
      *                                                                *
      *----------------------------------------------------------------*
      * DATA       AUTOR             ALTERACAO                         *
      * ---------- ----------------- --------------------------------- *
      * 02/02/2015 FERNANDA CARUSO   INCLUSAO DOS CAMPOS SCSBW038-S-   *
      *                              DS-EMPR/CD-DEPEND-SAP/DS-DEPEND   *
      *                              VLR-CONV-REAL.                    *
      *================================================================*
                                                                        
          05 SCSBW038-HEADER.                                           
             10 SCSBW038-COD-LAYOUT         PIC X(008) VALUE 'SCSBW038'.
             10 SCSBW038-TAM-LAYOUT         PIC 9(005) VALUE 17963.     
          05 SCSBW038-REGISTRO.                                         
             10 SCSBW038-BLOCO-ENTRADA.                                 
                15 SCSBW038-E-ANO                PIC 9(004).            
                15 SCSBW038-E-NREG               PIC 9(009).            
             10 SCSBW038-BLOCO-SAIDA.                                   
                15 SCSBW038-S-CPF-CNPJ-PRINC     PIC 9(009).            
                15 SCSBW038-S-CPF-CNPJ-FLIAL     PIC 9(005).            
                15 SCSBW038-S-CPF-CNPJ-CTRL      PIC 9(002).            
                15 SCSBW038-S-DS-EMPR            PIC X(070).            
                15 SCSBW038-S-CD-DEPEND-SAP      PIC 9(005).            
                15 SCSBW038-S-DS-DEPEND          PIC X(060).            
                15 SCSBW038-S-NOME-ADQUIR        PIC X(150).            
                15 SCSBW038-S-END-ADQUIR         PIC X(150).            
                15 SCSBW038-S-CD-PAIS-ADQUIR     PIC 9(003).            
                15 SCSBW038-S-DS-PAIS-ADQUIR     PIC X(060).            
                15 SCSBW038-S-CD-MOEDA           PIC 9(005).            
                15 SCSBW038-S-DS-MOEDA           PIC X(040).            
                15 SCSBW038-S-VLR-CONV-REAL      PIC 9(013)V99.         
                15 SCSBW038-S-QTD-REG            PIC 9(003).            
                15 SCSBW038-S-LISTA              OCCURS 0 TO 040 TIMES  
                      DEPENDING ON SCSBW038-S-QTD-REG.                  
                   20 SCSBW038-S-COPER           PIC 9(010).            
                   20 SCSBW038-S-CD-NBS          PIC X(009).            
                   20 SCSBW038-S-DS-NBS          PIC X(250).            
                   20 SCSBW038-S-CD-PAIS-DESTNO  PIC 9(003).            
                   20 SCSBW038-S-DS-PAIS-DESTNO  PIC X(060).            
                   20 SCSBW038-S-CD-MODO-PREST   PIC 9(001).            
                   20 SCSBW038-S-DS-MODO-PREST   PIC X(050).            
                   20 SCSBW038-S-DT-INICIO       PIC X(010).            
                   20 SCSBW038-S-DT-CONCLUSAO    PIC X(010).            
                   20 SCSBW038-S-VALOR           PIC 9(013)V99.         
                   20 SCSBW038-S-SALDO-FATURAR   PIC 9(013)V99.         
                   20 SCSBW038-S-OPER-BAIXA      PIC X(001).            

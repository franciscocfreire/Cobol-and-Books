      *================================================================*
      * NOME BOOK  : SCSBW062                                          *
      * DESCRICAO  : CONSULTAR RAS P/ INCLUIR REGISTRO DE PAGAMENTO    *
      * COMUNICACAO: FRAMEWORK X COORDENADOR                           *
      * DATA       : 02/03/2013                                        *
      * AUTOR      : FERNANDA CARUSO                                   *
      * EMPRESA    : BRQ IT SERVICES                                   *
      * GRUPO      : 17 - CONTABILIDADE E IMPOSTOS                     *
      * COMPONENTE : SCSB - SISCOSERV BRADESCO                         *
      *================================================================*
      *                                                                *
      * SCSBW062-HEADER.                                               *
      *   SCSBW062-COD-LAYOUT            = CODIGO DO LAYOUT            *
      *   SCSBW062-TAM-LAYOUT            = TAMANHO DO LAYOUT           *
      * SCSBW062-REGISTRO.                                             *
      *   SCSBW062-BLOCO-ENTRADA.                                      *
      *     SCSBW062-E-ANO               = ANO RAS                     *
      *     SCSBW062-E-NREG              = NUMERO DO REGISTRO RAS      *
      *   SCSBW062-BLOCO-SAIDA.                                        *
      *     SCSBW062-S-CPF-CNPJ-PRINC    = CPF/CNPJ PRINCIPAL          *
      *     SCSBW062-S-CPF-CNPJ-FLIAL    = CPF/CNPJ FILIAL             *
      *     SCSBW062-S-CPF-CNPJ-CTRL     = CPF/CNPJ CONTROLE           *
      *     SCSBW062-S-DS-EMPR           = DESCRICAO DA EMPRESA        *
      *     SCSBW062-S-CD-DEPEND-SAP     = CODIGO DA DEPENDENCIA SAP   *
      *     SCSBW062-S-DS-DEPEND         = DESCRICAO DA DEPENDENCIA    *
      *     SCSBW062-S-NOME-VEND         = NOME DO VENDEDOR            *
      *     SCSBW062-S-END-VEND          = ENDERECO DO VENDEDOR        *
      *     SCSBW062-S-CD-PAIS-VEND      = CODIGO DO PAIS DO VENDEDOR  *
      *     SCSBW062-S-DS-PAIS-VEND      = DESCRICAO PAIS DO VENDEDOR  *
      *     SCSBW062-S-CD-MOEDA          = CODIGO DA MOEDA             *
      *     SCSBW062-S-DS-MOEDA          = DESCRICAO DA MOEDA          *
      *     SCSBW062-S-VLR-CONV-REAL     = VALOR TOTAL CONVERTIDO REAL *
      *     SCSBW062-S-QTD-REG           = QTDE REGISTROS DEVOLVIDOS   *
      *     SCSBW062-S-LISTA.                                          *
      *       SCSBW062-S-COPER           = NUMERO DA OPERACAO          *
      *       SCSBW062-S-CD-NBS          = CODIGO NBS                  *
      *       SCSBW062-S-DS-NBS          = DESCRICAO NBS               *
      *       SCSBW062-S-CD-PAIS-DESTNO  = CODIGO PAIS DE DESTINO      *
      *       SCSBW062-S-DS-PAIS-DESTNO  = DESCRICAO PAIS DE DESTINO   *
      *       SCSBW062-S-CD-MODO-PREST   = CODIGO MODO DE PRESTACAO    *
      *       SCSBW062-S-DS-MODO-PREST   = DESCRICAO MODO DE PRESTACAO *
      *       SCSBW062-S-DT-INICIO       = DATA DE INICIO              *
      *       SCSBW062-S-DT-CONCLUSAO    = DATA DE CONCLUSAO           *
      *       SCSBW062-S-VALOR           = VALOR DA OPERACAO           *
      *       SCSBW062-S-SALDO-A-PAGAR   = SALDO A PAGAR               *
      *       SCSBW062-S-OPER-BAIXA      = OPERACAO BAIXA PLATAFORMA   *
      *                                    V (VISUALIZAR) / E (EDITAR) *
      *                                                                *
      *----------------------------------------------------------------*
      * DATA       AUTOR             ALTERACAO                         *
      * ---------- ----------------- --------------------------------- *
      * 11/02/2015 FERNANDA CARUSO   INCLUSAO DOS CAMPOS SCSBW062-S-   *
      *                              DS-EMPR/CD-DEPEND-SAP/DS-DEPEND   *
      *                              VLR-CONV-REAL.                    *
      *================================================================*
                                                                        
          05 SCSBW062-HEADER.                                           
             10 SCSBW062-COD-LAYOUT         PIC X(008) VALUE 'SCSBW062'.
             10 SCSBW062-TAM-LAYOUT         PIC 9(005) VALUE 17963.     
          05 SCSBW062-REGISTRO.                                         
             10 SCSBW062-BLOCO-ENTRADA.                                 
                15 SCSBW062-E-ANO                PIC 9(004).            
                15 SCSBW062-E-NREG               PIC 9(009).            
             10 SCSBW062-BLOCO-SAIDA.                                   
                15 SCSBW062-S-CPF-CNPJ-PRINC     PIC 9(009).            
                15 SCSBW062-S-CPF-CNPJ-FLIAL     PIC 9(005).            
                15 SCSBW062-S-CPF-CNPJ-CTRL      PIC 9(002).            
                15 SCSBW062-S-DS-EMPR            PIC X(070).            
                15 SCSBW062-S-CD-DEPEND-SAP      PIC 9(005).            
                15 SCSBW062-S-DS-DEPEND          PIC X(060).            
                15 SCSBW062-S-NOME-VEND          PIC X(150).            
                15 SCSBW062-S-END-VEND           PIC X(150).            
                15 SCSBW062-S-CD-PAIS-VEND       PIC 9(003).            
                15 SCSBW062-S-DS-PAIS-VEND       PIC X(060).            
                15 SCSBW062-S-CD-MOEDA           PIC 9(005).            
                15 SCSBW062-S-DS-MOEDA           PIC X(040).            
                15 SCSBW062-S-VLR-CONV-REAL      PIC 9(013)V99.         
                15 SCSBW062-S-QTD-REG            PIC 9(003).            
                15 SCSBW062-S-LISTA              OCCURS 0 TO 040 TIMES  
                      DEPENDING ON SCSBW062-S-QTD-REG.                  
                   20 SCSBW062-S-COPER           PIC 9(010).            
                   20 SCSBW062-S-CD-NBS          PIC X(009).            
                   20 SCSBW062-S-DS-NBS          PIC X(250).            
                   20 SCSBW062-S-CD-PAIS-DESTNO  PIC 9(003).            
                   20 SCSBW062-S-DS-PAIS-DESTNO  PIC X(060).            
                   20 SCSBW062-S-CD-MODO-PREST   PIC 9(001).            
                   20 SCSBW062-S-DS-MODO-PREST   PIC X(050).            
                   20 SCSBW062-S-DT-INICIO       PIC X(010).            
                   20 SCSBW062-S-DT-CONCLUSAO    PIC X(010).            
                   20 SCSBW062-S-VALOR           PIC 9(013)V99.         
                   20 SCSBW062-S-SALDO-A-PAGAR   PIC 9(013)V99.         
                   20 SCSBW062-S-OPER-BAIXA      PIC X(001).            

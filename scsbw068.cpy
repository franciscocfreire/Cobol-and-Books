      *================================================================*
      * NOME BOOK  : SCSBW068                                          *
      * DESCRICAO  : CONSULTAR DADOS DO PAGAMENTO                      *
      * COMUNICACAO: FRAMEWORK X COORDENADOR                           *
      * DATA       : 02/03/2013                                        *
      * AUTOR      : FERNANDA CARUSO                                   *
      * EMPRESA    : BRQ IT SERVICES                                   *
      * GRUPO      : 17 - CONTABILIDADE E IMPOSTOS                     *
      * COMPONENTE : SCSB - SISCOSERV BRADESCO                         *
      *================================================================*
      *                                                                *
      * SCSBW068-HEADER.                                               *
      *   SCSBW068-COD-LAYOUT            = CODIGO DO LAYOUT            *
      *   SCSBW068-TAM-LAYOUT            = TAMANHO DO LAYOUT           *
      * SCSBW068-REGISTRO.                                             *
      *   SCSBW068-BLOCO-ENTRADA.                                      *
      *     SCSBW068-E-ANO               = ANO RAS                     *
      *     SCSBW068-E-NREG              = NUMERO DO REGISTRO RAS      *
      *     SCSBW068-E-CPAGTO            = NUMERO DO PAGAMENTO         *
      *   SCSBW068-BLOCO-SAIDA.                                        *
      *     SCSBW068-S-CPF-CNPJ-PRINC    = CPF/CNPJ PRINCIPAL          *
      *     SCSBW068-S-CPF-CNPJ-FLIAL    = CPF/CNPJ FILIAL             *
      *     SCSBW068-S-CPF-CNPJ-CTRL     = CPF/CNPJ CONTROLE           *
      *     SCSBW068-S-DS-EMPR           = DESCRICAO DA EMPRESA        *
      *     SCSBW068-S-CD-DEPEND-SAP     = CODIGO DA DEPENDENCIA SAP   *
      *     SCSBW068-S-DS-DEPEND         = DESCRICAO DA DEPENDENCIA    *
      *     SCSBW068-S-NOME-VEND         = NOME DO VENDEDOR            *
      *     SCSBW068-S-END-VEND          = ENDERECO DO VENDEDOR        *
      *     SCSBW068-S-CD-PAIS-VEND      = CODIGO DO PAIS DO VENDEDOR  *
      *     SCSBW068-S-DS-PAIS-VEND      = DESCRICAO PAIS DO VENDEDOR  *
      *     SCSBW068-S-CD-MOEDA          = CODIGO DA MOEDA             *
      *     SCSBW068-S-DS-MOEDA          = DESCRICAO DA MOEDA          *
      *     SCSBW068-S-NF                = NUMERO DA NOTA FISCAL       *
      *     SCSBW068-S-DT-PAGAMENTO      = DATA DO PAGAMENTO           *
      *     SCSBW068-S-DT-INCLUSAO       = DATA DA INCLUSAO            *
      *     SCSBW068-S-DT-VALIDACAO      = DATA DA VALIDACAO           *
      *     SCSBW068-S-ORIG-REG          = ORIGEM DO REGISTRO          *
      *     SCSBW068-S-SIT-REG           = SIT. DO REG. DE PAGAMENTO   *
      *     SCSBW068-S-DESC-SIT-REG      = DESCRICAO DA SITUACAO       *
      *     SCSBW068-S-CD-USUARIO        = CODIGO DO USUARIO           *
      *     SCSBW068-S-NOME-USUARIO      = NOME DO USUARIO             *
      *     SCSBW068-S-DT-GERACAO-ARQ    = DATA DE GERACAO DO ARQUIVO  *
      *     SCSBW068-S-DT-TRANSMIS-ARQ   = DATA TRANSMISSAO DO ARQUIVO *
      *     SCSBW068-S-NUM-PROTOCOLO     = NUMERO DO PROTOCOLO         *
      *     SCSBW068-S-DT-CANCEL         = DATA DE CANCELAMENTO        *
      *     SCSBW068-S-DT-GERACAO-CANC   = DT GERACAO DO ARQ CANCEL    *
      *     SCSBW068-S-DT-TRANSMIS-CANC  = DT TRANSMISSAO ARQ CANCEL   *
      *     SCSBW068-S-NUM-PROTOC-CANC   = NUMERO DO PROTOCOLO CANCEL  *
      *     SCSBW068-S-QTD-REG           = QTDE REGISTROS DEVOLVIDOS   *
      *     SCSBW068-S-LISTA.                                          *
      *       SCSBW068-S-COPER           = NUMERO DA OPERACAO          *
      *       SCSBW068-S-CD-NBS          = CODIGO NBS                  *
      *       SCSBW068-S-DS-NBS          = DESCRICAO NBS               *
      *       SCSBW068-S-CD-PAIS-DESTNO  = CODIGO PAIS DE DESTINO      *
      *       SCSBW068-S-DS-PAIS-DESTNO  = DESCRICAO PAIS DE DESTINO   *
      *       SCSBW068-S-VLR-OPERACAO    = VALOR DA OPERACAO           *
      *       SCSBW068-S-VLR-PAGAMENTO   = VALOR DO PAGAMENTO          *
      *       SCSBW068-S-VLR-MANT-EXTER  = VALOR MANTIDO NO EXTERIOR   *
      *                                                                *
      *----------------------------------------------------------------*
      * DATA       AUTOR             ALTERACAO                         *
      * ---------- ----------------- --------------------------------- *
      * 06/02/2015 FERNANDA CARUSO   INCLUSAO DOS CAMPOS SCSBW068-S-   *
      *                              DS-EMPR/CD-DEPEND-SAP/DS-DEPEND   *
      *                              CD-USUARIO/NOME-USUARIO           *
      *                              DT-INCLUSAO/DT-VALIDACAO/ORIG-REG *
      *                              DESC-SIT-REG/DT-GERACAO-ARQ       *
      *                              DT-TRANSMIS-ARQ/NUM-PROTOCOLO     *
      *                              DT-CANCEL/DT-GERACAO-CANC         *
      *                              DT-TRANSMIS-CANC/NUM-PROTOC-CANC. *
      *================================================================*
                                                                        
          05 SCSBW068-HEADER.                                           
             10 SCSBW068-COD-LAYOUT         PIC X(008) VALUE 'SCSBW068'.
             10 SCSBW068-TAM-LAYOUT         PIC 9(005) VALUE 16032.     
          05 SCSBW068-REGISTRO.                                         
             10 SCSBW068-BLOCO-ENTRADA.                                 
                15 SCSBW068-E-ANO                PIC 9(004).            
                15 SCSBW068-E-NREG               PIC 9(009).            
                15 SCSBW068-E-CPAGTO             PIC 9(010).            
             10 SCSBW068-BLOCO-SAIDA.                                   
                15 SCSBW068-S-CPF-CNPJ-PRINC     PIC 9(009).            
                15 SCSBW068-S-CPF-CNPJ-FLIAL     PIC 9(005).            
                15 SCSBW068-S-CPF-CNPJ-CTRL      PIC 9(002).            
                15 SCSBW068-S-DS-EMPR            PIC X(070).            
                15 SCSBW068-S-CD-DEPEND-SAP      PIC 9(005).            
                15 SCSBW068-S-DS-DEPEND          PIC X(060).            
                15 SCSBW068-S-NOME-VEND          PIC X(150).            
                15 SCSBW068-S-END-VEND           PIC X(150).            
                15 SCSBW068-S-CD-PAIS-VEND       PIC 9(003).            
                15 SCSBW068-S-DS-PAIS-VEND       PIC X(060).            
                15 SCSBW068-S-CD-MOEDA           PIC 9(005).            
                15 SCSBW068-S-DS-MOEDA           PIC X(040).            
                15 SCSBW068-S-NF                 PIC X(060).            
                15 SCSBW068-S-DT-PAGAMENTO       PIC X(010).            
                15 SCSBW068-S-DT-INCLUSAO        PIC X(010).            
                15 SCSBW068-S-DT-VALIDACAO       PIC X(010).            
                15 SCSBW068-S-ORIG-REG           PIC X(050).            
                15 SCSBW068-S-SIT-REG            PIC X(001).            
                15 SCSBW068-S-DESC-SIT-REG       PIC X(050).            
                15 SCSBW068-S-CD-USUARIO         PIC X(009).            
                15 SCSBW068-S-NOME-USUARIO       PIC X(080).            
                15 SCSBW068-S-DT-GERACAO-ARQ     PIC X(010).            
                15 SCSBW068-S-DT-TRANSMIS-ARQ    PIC X(010).            
                15 SCSBW068-S-NUM-PROTOCOLO      PIC X(012).            
                15 SCSBW068-S-DT-CANCEL          PIC X(010).            
                15 SCSBW068-S-DT-GERACAO-CANC    PIC X(010).            
                15 SCSBW068-S-DT-TRANSMIS-CANC   PIC X(010).            
                15 SCSBW068-S-NUM-PROTOC-CANC    PIC X(012).            
                15 SCSBW068-S-QTD-REG            PIC 9(003).            
                15 SCSBW068-S-LISTA              OCCURS 0 TO 040 TIMES  
                      DEPENDING ON SCSBW068-S-QTD-REG.                  
                   20 SCSBW068-S-COPER           PIC 9(010).            
                   20 SCSBW068-S-CD-NBS          PIC X(009).            
                   20 SCSBW068-S-DS-NBS          PIC X(250).            
                   20 SCSBW068-S-CD-PAIS-DESTNO  PIC 9(003).            
                   20 SCSBW068-S-DS-PAIS-DESTNO  PIC X(060).            
                   20 SCSBW068-S-VLR-OPERACAO    PIC 9(013)V99.         
                   20 SCSBW068-S-VLR-PAGAMENTO   PIC 9(013)V99.         
                   20 SCSBW068-S-VLR-MANT-EXTER  PIC 9(013)V99.         

      *================================================================*
      * NOME BOOK  : SCSBW108                                          *
      * DESCRICAO  : CONSULTA PARA RETIFICAR RAS OU ADITIVO            *
      * COMUNICACAO: FRAMEWORK X COORDENADOR                           *
      * DATA       : 14/06/2013                                        *
      * AUTOR      : FERNANDA CARUSO                                   *
      * EMPRESA    : BRQ IT SERVICES                                   *
      * GRUPO      : 17 - CONTABILIDADE E IMPOSTOS                     *
      * COMPONENTE : SCSB - SISCOSERV BRADESCO                         *
      *================================================================*
      *                                                                *
      * SCSBW108-HEADER.                                               *
      *   SCSBW108-COD-LAYOUT            = CODIGO DO LAYOUT            *
      *   SCSBW108-TAM-LAYOUT            = TAMANHO DO LAYOUT           *
      * SCSBW108-REGISTRO.                                             *
      *   SCSBW108-BLOCO-ENTRADA.                                      *
      *     SCSBW108-E-ANO               = ANO RAS                     *
      *     SCSBW108-E-NREG              = NUMERO DO REGISTRO RAS      *
      *     SCSBW108-E-COPER-ADITIVO     = NUMERO OPERACAO DE ADITIVO  *
      *     SCSBW108-E-IND-CONSUL        = INDICADOR DE CONSULTA       *
      *                                    A - ALTERACAO RETIFICACAO   *
      *                                    I - INCLUSAO RETIFICACAO    *
      *   SCSBW108-BLOCO-SAIDA.                                        *
      *     SCSBW108-S-CPF-CNPJ-PRINC    = CPF/CNPJ PRINCIPAL          *
      *     SCSBW108-S-CPF-CNPJ-FLIAL    = CPF/CNPJ FILIAL             *
      *     SCSBW108-S-CPF-CNPJ-CTRL     = CPF/CNPJ CONTROLE           *
      *     SCSBW108-S-DS-EMPR           = DESCRICAO DA EMPRESA        *
      *     SCSBW108-S-CD-DEPEND-SAP     = CODIGO DA DEPENDENCIA SAP   *
      *     SCSBW108-S-DS-DEPEND         = DESCRICAO DA DEPENDENCIA    *
      *     SCSBW108-S-NOME-VEND         = NOME DO VENDEDOR            *
      *     SCSBW108-S-END-VEND          = ENDERECO DO VENDEDOR        *
      *     SCSBW108-S-CD-PAIS-VEND      = CODIGO DO PAIS DO VENDEDOR  *
      *     SCSBW108-S-DS-PAIS-VEND      = DESCRICAO PAIS DO VENDEDOR  *
      *     SCSBW108-S-NIF               = NUMERO IDENTIFICACAO FISCAL *
      *     SCSBW108-S-CD-MOEDA          = CODIGO DA MOEDA             *
      *     SCSBW108-S-DS-MOEDA          = DESCRICAO DA MOEDA          *
      *     SCSBW108-S-COMBO-MOEDA       = A (ATIVO) OU I (INATIVO)    *
      *     SCSBW108-S-VLR-CONV-REAL     = VALOR TOTAL CONVERTIDO REAL *
      *     SCSBW108-S-SIT-REG           = SITUACAO DO REGISTRO        *
      *     SCSBW108-S-DS-SIT-REG        = DESCRICAO SITUACAO REGISTRO *
      *     SCSBW108-S-ORIG-REG          = ORIGEM DO REGISTRO          *
      *     SCSBW108-S-DT-INCLUSAO       = DATA DA INCLUSAO            *
      *     SCSBW108-S-DT-VALIDACAO      = DATA DA VALIDACAO           *
      *     SCSBW108-S-CD-USUARIO        = CODIGO DO USUARIO           *
      *     SCSBW108-S-NOME-USUARIO      = NOME DO USUARIO             *
      *     SCSBW108-S-DT-GERACAO-ARQ    = DATA DE GERACAO DO ARQUIVO  *
      *     SCSBW108-S-DT-TRANSMIS-ARQ   = DATA TRANSMISSAO DO ARQUIVO *
      *     SCSBW108-S-NUM-PROTOCOLO     = NUMERO DO PROTOCOLO         *
      *     SCSBW108-S-INF-COMPLEM       = INFORMACOES COMPLEMENTARES  *
      *     SCSBW108-S-QTD-REG           = QTDE REGISTROS DEVOLVIDOS   *
      *     SCSBW108-S-LISTA.                                          *
      *       SCSBW108-S-COPER           = NUMERO DA OPERACAO          *
      *       SCSBW108-S-CD-NBS          = CODIGO NBS                  *
      *       SCSBW108-S-DS-NBS          = DESCRICAO NBS               *
      *       SCSBW108-S-CD-PAIS-DESTNO  = CODIGO PAIS DE DESTINO      *
      *       SCSBW108-S-DS-PAIS-DESTNO  = DESCRICAO PAIS DE DESTINO   *
      *       SCSBW108-S-CD-MODO-PREST   = CODIGO MODO DE PRESTACAO    *
      *       SCSBW108-S-DS-MODO-PREST   = DESCRICAO MODO DE PRESTACAO *
      *       SCSBW108-S-DT-INICIO       = DATA DE INICIO              *
      *       SCSBW108-S-DT-CONCLUSAO    = DATA DE CONCLUSAO           *
      *       SCSBW108-S-VALOR           = VALOR DA OPERACAO           *
      *       SCSBW108-S-VALOR-PAGO      = VALOR PAGO                  *
      *       SCSBW108-S-IND-ADITIVO     = INDICADOR ADITIVO (S OU N)  *
      *       SCSBW108-S-SIT-OPER        = SITUACAO DA OPERACAO        *
      *       SCSBW108-S-DS-SIT-OPER     = DESCRICAO SITUACAO OPERACAO *
      *       SCSBW108-S-COMBO-NBS       = A (ATIVO) OU I (INATIVO)    *
      *       SCSBW108-S-OPER-BAIXA      = OPERACAO BAIXA PLATAFORMA   *
      *                                    V (VISUALIZAR) / E (EDITAR) *
      *                                                                *
      *----------------------------------------------------------------*
      * DATA       AUTOR             ALTERACAO                         *
      * ---------- ----------------- --------------------------------- *
      * 09/02/2015 FERNANDA CARUSO   INCLUSAO DOS CAMPOS SCSBW108-E-   *
      *                              IND-CONSUL/SCSBW108-S-            *
      *                              DS-EMPR/CD-DEPEND-SAP/DS-DEPEND   *
      *                              VLR-CONV-REAL/SIT-REG/DS-SIT-REG  *
      *                              ORIG-REG/DT-INCLUSAO/DT-VALIDACAO *
      *                              CD-USUARIO/NOME-USUARIO           *
      *                              DT-GERACAO-ARQ/DT-TRANSMIS-ARQ    *
      *                              NUM-PROTOCOLO/SIT-OPER            *
      *                              DS-SIT-OPER.                      *
      *================================================================*
                                                                        
          05 SCSBW108-HEADER.                                           
             10 SCSBW108-COD-LAYOUT         PIC X(008) VALUE 'SCSBW108'.
             10 SCSBW108-TAM-LAYOUT         PIC 9(005) VALUE 20507.     
          05 SCSBW108-REGISTRO.                                         
             10 SCSBW108-BLOCO-ENTRADA.                                 
                15 SCSBW108-E-ANO                PIC 9(004).            
                15 SCSBW108-E-NREG               PIC 9(009).            
                15 SCSBW108-E-COPER-ADITIVO      PIC 9(010).            
                15 SCSBW108-E-IND-CONSUL         PIC X(001).            
             10 SCSBW108-BLOCO-SAIDA.                                   
                15 SCSBW108-S-CPF-CNPJ-PRINC     PIC 9(009).            
                15 SCSBW108-S-CPF-CNPJ-FLIAL     PIC 9(005).            
                15 SCSBW108-S-CPF-CNPJ-CTRL      PIC 9(002).            
                15 SCSBW108-S-DS-EMPR            PIC X(070).            
                15 SCSBW108-S-CD-DEPEND-SAP      PIC 9(005).            
                15 SCSBW108-S-DS-DEPEND          PIC X(060).            
                15 SCSBW108-S-NOME-VEND          PIC X(150).            
                15 SCSBW108-S-END-VEND           PIC X(150).            
                15 SCSBW108-S-CD-PAIS-VEND       PIC 9(003).            
                15 SCSBW108-S-DS-PAIS-VEND       PIC X(060).            
                15 SCSBW108-S-NIF                PIC X(040).            
                15 SCSBW108-S-CD-MOEDA           PIC 9(005).            
                15 SCSBW108-S-DS-MOEDA           PIC X(040).            
                15 SCSBW108-S-COMBO-MOEDA        PIC X(001).            
                15 SCSBW108-S-VLR-CONV-REAL      PIC 9(013)V99.         
                15 SCSBW108-S-SIT-REG            PIC X(001).            
                15 SCSBW108-S-DS-SIT-REG         PIC X(050).            
                15 SCSBW108-S-ORIG-REG           PIC X(050).            
                15 SCSBW108-S-DT-INCLUSAO        PIC X(010).            
                15 SCSBW108-S-DT-VALIDACAO       PIC X(010).            
                15 SCSBW108-S-CD-USUARIO         PIC X(009).            
                15 SCSBW108-S-NOME-USUARIO       PIC X(080).            
                15 SCSBW108-S-DT-GERACAO-ARQ     PIC X(010).            
                15 SCSBW108-S-DT-TRANSMIS-ARQ    PIC X(010).            
                15 SCSBW108-S-NUM-PROTOCOLO      PIC X(012).            
                15 SCSBW108-S-INF-COMPLEM        PIC X(5000).           
                15 SCSBW108-S-QTD-REG            PIC 9(003).            
                15 SCSBW108-S-LISTA              OCCURS 0 TO 030 TIMES  
                      DEPENDING ON SCSBW108-S-QTD-REG.                  
                   20 SCSBW108-S-COPER           PIC 9(010).            
                   20 SCSBW108-S-CD-NBS          PIC X(009).            
                   20 SCSBW108-S-DS-NBS          PIC X(250).            
                   20 SCSBW108-S-CD-PAIS-DESTNO  PIC 9(003).            
                   20 SCSBW108-S-DS-PAIS-DESTNO  PIC X(060).            
                   20 SCSBW108-S-CD-MODO-PREST   PIC 9(001).            
                   20 SCSBW108-S-DS-MODO-PREST   PIC X(050).            
                   20 SCSBW108-S-DT-INICIO       PIC X(010).            
                   20 SCSBW108-S-DT-CONCLUSAO    PIC X(010).            
                   20 SCSBW108-S-VALOR           PIC 9(013)V99.         
                   20 SCSBW108-S-VALOR-PAGO      PIC 9(013)V99.         
                   20 SCSBW108-S-IND-ADITIVO     PIC X(001).            
                   20 SCSBW108-S-SIT-OPER        PIC X(001).            
                   20 SCSBW108-S-DS-SIT-OPER     PIC X(050).            
                   20 SCSBW108-S-COMBO-NBS       PIC X(001).            
                   20 SCSBW108-S-OPER-BAIXA      PIC X(001).            

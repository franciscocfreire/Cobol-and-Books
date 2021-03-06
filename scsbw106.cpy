      *================================================================*
      * NOME BOOK  : SCSBW106                                          *
      * DESCRICAO  : CONSULTA PARA RETIFICAR RVS OU ADITIVO            *
      * COMUNICACAO: FRAMEWORK X COORDENADOR                           *
      * DATA       : 13/06/2013                                        *
      * AUTOR      : FERNANDA CARUSO                                   *
      * EMPRESA    : BRQ IT SERVICES                                   *
      * GRUPO      : 17 - CONTABILIDADE E IMPOSTOS                     *
      * COMPONENTE : SCSB - SISCOSERV BRADESCO                         *
      *================================================================*
      *                                                                *
      * SCSBW106-HEADER.                                               *
      *   SCSBW106-COD-LAYOUT            = CODIGO DO LAYOUT            *
      *   SCSBW106-TAM-LAYOUT            = TAMANHO DO LAYOUT           *
      * SCSBW106-REGISTRO.                                             *
      *   SCSBW106-BLOCO-ENTRADA.                                      *
      *     SCSBW106-E-ANO               = ANO RVS                     *
      *     SCSBW106-E-NREG              = NUMERO DO REGISTRO RVS      *
      *     SCSBW106-E-COPER-ADITIVO     = NUMERO OPERACAO DE ADITIVO  *
      *     SCSBW106-E-IND-CONSUL        = INDICADOR DE CONSULTA       *
      *                                    A - ALTERACAO RETIFICACAO   *
      *                                    I - INCLUSAO RETIFICACAO    *
      *   SCSBW106-BLOCO-SAIDA.                                        *
      *     SCSBW106-S-CPF-CNPJ-PRINC    = CPF/CNPJ PRINCIPAL          *
      *     SCSBW106-S-CPF-CNPJ-FLIAL    = CPF/CNPJ FILIAL             *
      *     SCSBW106-S-CPF-CNPJ-CTRL     = CPF/CNPJ CONTROLE           *
      *     SCSBW106-S-DS-EMPR           = DESCRICAO DA EMPRESA        *
      *     SCSBW106-S-CD-DEPEND-SAP     = CODIGO DA DEPENDENCIA SAP   *
      *     SCSBW106-S-DS-DEPEND         = DESCRICAO DA DEPENDENCIA    *
      *     SCSBW106-S-NOME-ADQUIR       = NOME DO ADQUIRENTE          *
      *     SCSBW106-S-END-ADQUIR        = ENDERECO DO ADQUIRENTE      *
      *     SCSBW106-S-CD-PAIS-ADQUIR    = CODIGO DO PAIS ADQUIRENTE   *
      *     SCSBW106-S-DS-PAIS-ADQUIR    = DESCRICAO PAIS ADQUIRENTE   *
      *     SCSBW106-S-NIF               = NUMERO IDENTIFICACAO FISCAL *
      *     SCSBW106-S-CD-MOEDA          = CODIGO DA MOEDA             *
      *     SCSBW106-S-DS-MOEDA          = DESCRICAO DA MOEDA          *
      *     SCSBW106-S-COMBO-MOEDA       = A (ATIVO) OU I (INATIVO)    *
      *     SCSBW106-S-VLR-CONV-REAL     = VALOR TOTAL CONVERTIDO REAL *
      *     SCSBW106-S-SIT-REG           = SITUACAO DO REGISTRO        *
      *     SCSBW106-S-DS-SIT-REG        = DESCRICAO SITUACAO REGISTRO *
      *     SCSBW106-S-ORIG-REG          = ORIGEM DO REGISTRO          *
      *     SCSBW106-S-DT-INCLUSAO       = DATA DA INCLUSAO            *
      *     SCSBW106-S-DT-VALIDACAO      = DATA DA VALIDACAO           *
      *     SCSBW106-S-CD-USUARIO        = CODIGO DO USUARIO           *
      *     SCSBW106-S-NOME-USUARIO      = NOME DO USUARIO             *
      *     SCSBW106-S-DT-GERACAO-ARQ    = DATA DE GERACAO DO ARQUIVO  *
      *     SCSBW106-S-DT-TRANSMIS-ARQ   = DATA TRANSMISSAO DO ARQUIVO *
      *     SCSBW106-S-NUM-PROTOCOLO     = NUMERO DO PROTOCOLO         *
      *     SCSBW106-S-INF-COMPLEM       = INFORMACOES COMPLEMENTARES  *
      *     SCSBW106-S-QTD-REG           = QTDE REGISTROS DEVOLVIDOS   *
      *     SCSBW106-S-LISTA.                                          *
      *       SCSBW106-S-COPER           = NUMERO DA OPERACAO          *
      *       SCSBW106-S-CD-NBS          = CODIGO NBS                  *
      *       SCSBW106-S-DS-NBS          = DESCRICAO NBS               *
      *       SCSBW106-S-CD-PAIS-DESTNO  = CODIGO PAIS DE DESTINO      *
      *       SCSBW106-S-DS-PAIS-DESTNO  = DESCRICAO PAIS DE DESTINO   *
      *       SCSBW106-S-CD-MODO-PREST   = CODIGO MODO DE PRESTACAO    *
      *       SCSBW106-S-DS-MODO-PREST   = DESCRICAO MODO DE PRESTACAO *
      *       SCSBW106-S-DT-INICIO       = DATA DE INICIO              *
      *       SCSBW106-S-DT-CONCLUSAO    = DATA DE CONCLUSAO           *
      *       SCSBW106-S-VALOR           = VALOR DA OPERACAO           *
      *       SCSBW106-S-VALOR-FATURADO  = VALOR FATURADO              *
      *       SCSBW106-S-IND-ADITIVO     = INDICADOR ADITIVO (S OU N)  *
      *       SCSBW106-S-SIT-OPER        = SITUACAO DA OPERACAO        *
      *       SCSBW106-S-DS-SIT-OPER     = DESCRICAO SITUACAO OPERACAO *
      *       SCSBW106-S-COMBO-NBS       = A (ATIVO) OU I (INATIVO)    *
      *       SCSBW106-S-OPER-BAIXA      = OPERACAO BAIXA PLATAFORMA   *
      *                                    V (VISUALIZAR) / E (EDITAR) *
      *                                                                *
      *----------------------------------------------------------------*
      * DATA       AUTOR             ALTERACAO                         *
      * ---------- ----------------- --------------------------------- *
      * 21/01/2015 FERNANDA CARUSO   INCLUSAO DOS CAMPOS SCSBW106-E-   *
      *                              IND-CONSUL/SCSBW106-S-            *
      *                              DS-EMPR/CD-DEPEND-SAP/DS-DEPEND   *
      *                              VLR-CONV-REAL/SIT-REG/DS-SIT-REG  *
      *                              ORIG-REG/DT-INCLUSAO/DT-VALIDACAO *
      *                              CD-USUARIO/NOME-USUARIO           *
      *                              DT-GERACAO-ARQ/DT-TRANSMIS-ARQ    *
      *                              NUM-PROTOCOLO/SIT-OPER            *
      *                              DS-SIT-OPER.                      *
      *================================================================*
                                                                        
          05 SCSBW106-HEADER.                                           
             10 SCSBW106-COD-LAYOUT         PIC X(008) VALUE 'SCSBW106'.
             10 SCSBW106-TAM-LAYOUT         PIC 9(005) VALUE 20507.     
          05 SCSBW106-REGISTRO.                                         
             10 SCSBW106-BLOCO-ENTRADA.                                 
                15 SCSBW106-E-ANO                PIC 9(004).            
                15 SCSBW106-E-NREG               PIC 9(009).            
                15 SCSBW106-E-COPER-ADITIVO      PIC 9(010).            
                15 SCSBW106-E-IND-CONSUL         PIC X(001).            
             10 SCSBW106-BLOCO-SAIDA.                                   
                15 SCSBW106-S-CPF-CNPJ-PRINC     PIC 9(009).            
                15 SCSBW106-S-CPF-CNPJ-FLIAL     PIC 9(005).            
                15 SCSBW106-S-CPF-CNPJ-CTRL      PIC 9(002).            
                15 SCSBW106-S-DS-EMPR            PIC X(070).            
                15 SCSBW106-S-CD-DEPEND-SAP      PIC 9(005).            
                15 SCSBW106-S-DS-DEPEND          PIC X(060).            
                15 SCSBW106-S-NOME-ADQUIR        PIC X(150).            
                15 SCSBW106-S-END-ADQUIR         PIC X(150).            
                15 SCSBW106-S-CD-PAIS-ADQUIR     PIC 9(003).            
                15 SCSBW106-S-DS-PAIS-ADQUIR     PIC X(060).            
                15 SCSBW106-S-NIF                PIC X(040).            
                15 SCSBW106-S-CD-MOEDA           PIC 9(005).            
                15 SCSBW106-S-DS-MOEDA           PIC X(040).            
                15 SCSBW106-S-COMBO-MOEDA        PIC X(001).            
                15 SCSBW106-S-VLR-CONV-REAL      PIC 9(013)V99.         
                15 SCSBW106-S-SIT-REG            PIC X(001).            
                15 SCSBW106-S-DS-SIT-REG         PIC X(050).            
                15 SCSBW106-S-ORIG-REG           PIC X(050).            
                15 SCSBW106-S-DT-INCLUSAO        PIC X(010).            
                15 SCSBW106-S-DT-VALIDACAO       PIC X(010).            
                15 SCSBW106-S-CD-USUARIO         PIC X(009).            
                15 SCSBW106-S-NOME-USUARIO       PIC X(080).            
                15 SCSBW106-S-DT-GERACAO-ARQ     PIC X(010).            
                15 SCSBW106-S-DT-TRANSMIS-ARQ    PIC X(010).            
                15 SCSBW106-S-NUM-PROTOCOLO      PIC X(012).            
                15 SCSBW106-S-INF-COMPLEM        PIC X(5000).           
                15 SCSBW106-S-QTD-REG            PIC 9(003).            
                15 SCSBW106-S-LISTA              OCCURS 0 TO 030 TIMES  
                      DEPENDING ON SCSBW106-S-QTD-REG.                  
                   20 SCSBW106-S-COPER           PIC 9(010).            
                   20 SCSBW106-S-CD-NBS          PIC X(009).            
                   20 SCSBW106-S-DS-NBS          PIC X(250).            
                   20 SCSBW106-S-CD-PAIS-DESTNO  PIC 9(003).            
                   20 SCSBW106-S-DS-PAIS-DESTNO  PIC X(060).            
                   20 SCSBW106-S-CD-MODO-PREST   PIC 9(001).            
                   20 SCSBW106-S-DS-MODO-PREST   PIC X(050).            
                   20 SCSBW106-S-DT-INICIO       PIC X(010).            
                   20 SCSBW106-S-DT-CONCLUSAO    PIC X(010).            
                   20 SCSBW106-S-VALOR           PIC 9(013)V99.         
                   20 SCSBW106-S-VALOR-FATURADO  PIC 9(013)V99.         
                   20 SCSBW106-S-IND-ADITIVO     PIC X(001).            
                   20 SCSBW106-S-SIT-OPER        PIC X(001).            
                   20 SCSBW106-S-DS-SIT-OPER     PIC X(050).            
                   20 SCSBW106-S-COMBO-NBS       PIC X(001).            
                   20 SCSBW106-S-OPER-BAIXA      PIC X(001).            

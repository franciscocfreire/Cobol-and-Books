      *================================================================*
      * NOME BOOK  : SCSBW382                                          *
      * DESCRICAO  : CONSULTAR LISTA DOS SERVICOS VALIDADOS PARA       *
      *              GERAR ARQUIVO PARA TRANSMISSAO.                   *
      * COMUNICACAO: FRAMEWORK X COORDENADOR                           *
      * DATA       : 31/03/2015                                        *
      * AUTOR      : FERNANDA CARUSO                                   *
      * EMPRESA    : BRQ IT SERVICES                                   *
      * GRUPO      : 17 - CONTABILIDADE E IMPOSTOS                     *
      * COMPONENTE : SCSB - SISCOSERV BRADESCO                         *
      *================================================================*
      *                                                                *
      * SCSBW382-HEADER.                                               *
      *   SCSBW382-COD-LAYOUT            = CODIGO DO LAYOUT            *
      *   SCSBW382-TAM-LAYOUT            = TAMANHO DO LAYOUT           *
      * SCSBW382-REGISTRO.                                             *
      *   SCSBW382-BLOCO-ENTRADA.                                      *
      *     SCSBW382-E-TPO-REG           = TIPO DE REGISTRO            *
      *     SCSBW382-E-EMPR              = CODIGO DA EMPRESA           *
      *     SCSBW382-E-DATA-INI          = DATA INICIAL                *
      *     SCSBW382-E-DATA-FIM          = DATA FINAL                  *
      *     SCSBW382-E-TPO-SERVC         = TIPO DE SERVICO             *
      *   SCSBW382-BLOCO-SAIDA.                                        *
      *     SCSBW382-S-QTD-SERVC         = QTDE MAXIMA SERVICO POR ARQ *
      *     SCSBW382-S-QTD-REG           = QUANTIDADE DE REGISTROS     *
      *     SCSBW382-S-LISTA.                                          *
      *       SCSBW382-S-ANO             = ANO DO SERVICO              *
      *       SCSBW382-S-NREG            = NUMERO DO SERVICO           *
      *       SCSBW382-S-CNPJ-PRINC      = CODIGO DO CNPJ (CORPO)      *
      *       SCSBW382-S-CNPJ-FLIAL      = CODIGO DA FILIAL DO CNPJ    *
      *       SCSBW382-S-CNPJ-CTRL       = CODIGO DO CONTROLE DO CNPJ  *
      *       SCSBW382-S-DESC-EMPR       = DESCRICAO DA EMPRESA        *
      *       SCSBW382-S-DT-INCLUSAO     = DATA DE INCLUSAO (CRIACAO)  *
      *       SCSBW382-S-TPO-SERVC       = TIPO DE SERVICO             *
      *       SCSBW382-S-DES-SERVC       = DESCRICAO TIPO DE SERVICO   *
      *                         01 - RVS                               *
      *                         02 - RETIFICACAO DE RVS                *
      *                         03 - ADITIVO DE RVS                    *
      *                         04 - RETIFICACAO DE ADITIVO DE RVS     *
      *                         05 - REGISTRO DE FATURAMENTO           *
      *                         06 - CANCELAMENTO DO FATURAMENTO       *
      *                         07 - RAS                               *
      *                         08 - RETIFICACAO DE RAS                *
      *                         09 - ADITIVO DE RAS                    *
      *                         10 - RETIFICACAO DE ADITIVO DE RAS     *
      *                         11 - REGISTRO DE PAGAMENTO             *
      *                         12 - CANCELAMENTO DO PAGAMENTO         *
      *                                                                *
      *----------------------------------------------------------------*
      * DATA       AUTOR             ALTERACAO                         *
      * ---------- ----------------- --------------------------------- *
      * 99/99/9999                   XXXXXXXXXXXXXXXXXXXXXXX           *
      *================================================================*
                                                                        
          05 SCSBW382-HEADER.                                           
             10 SCSBW382-COD-LAYOUT         PIC X(008) VALUE 'SCSBW382'.
             10 SCSBW382-TAM-LAYOUT         PIC 9(005) VALUE 8052.      
          05 SCSBW382-REGISTRO.                                         
             10 SCSBW382-BLOCO-ENTRADA.                                 
                15 SCSBW382-E-TPO-REG            PIC 9(001).            
                15 SCSBW382-E-EMPR               PIC 9(010).            
                15 SCSBW382-E-DATA-INI           PIC X(010).            
                15 SCSBW382-E-DATA-FIM           PIC X(010).            
                15 SCSBW382-E-TPO-SERVC          PIC 9(002).            
             10 SCSBW382-BLOCO-SAIDA.                                   
                15 SCSBW382-S-QTD-SERVC          PIC 9(003).            
                15 SCSBW382-S-QTD-REG            PIC 9(003).            
                15 SCSBW382-S-LISTA              OCCURS 0 TO 050 TIMES  
                      DEPENDING ON SCSBW382-S-QTD-REG.                  
                   20 SCSBW382-S-ANO             PIC 9(004).            
                   20 SCSBW382-S-NREG            PIC 9(009).            
                   20 SCSBW382-S-CNPJ-PRINC      PIC 9(009).            
                   20 SCSBW382-S-CNPJ-FLIAL      PIC 9(004).            
                   20 SCSBW382-S-CNPJ-CTRL       PIC 9(002).            
                   20 SCSBW382-S-DESC-EMPR       PIC X(070).            
                   20 SCSBW382-S-DT-INCLUSAO     PIC X(010).            
                   20 SCSBW382-S-TPO-SERVC       PIC 9(002).            
                   20 SCSBW382-S-DES-SERVC       PIC X(050).            

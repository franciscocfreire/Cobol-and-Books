      ****************************************************************  
      * NOME BOOK : UORGWB39                                         *  
      *                                                              *  
      * DESCRICAO : FUNCIONAL BATCH UORG2340 PARA CONSULTA DE CODIGO *  
      *             DA DEPENDENCIA DA NOVA ARQUITETURA E SEQUENCIAL  *  
      *             DA DEP/FILIAL.                                   *  
      *                                                              *  
      * DATA      : JAN/2011                                         *  
      * AUTOR     : SERGIO F. SOUSA                                  *  
      * EMPRESA   : STEFANINI                                        *  
      * GRUPO     : TI MELHORIAS                                     *  
      * COMPONENTE: UORG - UNIDADE ORGANIZACIONAL                    *  
      * TAMANHO   : FIXO - TAMANHO LAYOUT                            *  
      ****************************************************************  
      * DATA       AUTOR             DESCRICAO / MANUTENCAO          *  
      ****************************************************************  
      * XX/XX/XXXX XXXXXXXXXXXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  *  
      ****************************************************************  
      * BLOCO DE IDENTIFICACAO                                       *  
      * UORGWB39-COD-LAYOUT         = CODIGO DESTE LAYOUT            *  
      * UORGWB39-TAM-LAYOUT         = TAMANHO DO REGISTRO            *  
      *                                                              *  
      * BLOCO DE ENTRADA                                             *  
      * UORGWB39-E-CPSSOA-JURID     = CODIGO DA PESSOA JURIDICA      *  
      * UORGWB39-E-CTPO_UND_ORGNZ   = NUMERO SEQ DA UNIDADE ORGNZ    *  
      * UORGWB39-E-CUND_ORGNZ       = CODIGO DA UNIDADE ORGNZ        *  
      *                                                              *  
      * BLOCO DE SAIDA                                               *  
      * UORGWB39-S-CPSSOA-JUR       = CODIGO DA DEPENDENCIA TI       *  
      * UORGWB39-S-NSEQ-ORG-TI      = NUMERO SEQ DEP/FILIAL          *  
      * UORGWB39-S-CUND_ORGNZ       = CODIGO DA UNIDADE ORGNZ        *  
      ****************************************************************  
           05 UORGWB39-HEADER.                                          
              10 UORGWB39-COD-LAYOUT      PIC X(008) VALUE 'UORGWB39'.  
              10 UORGWB39-TAM-LAYOUT      PIC 9(005) VALUE      00056.  
              10 UORGWB39-BLOCO-ENTRADA.                                
                 15 UORGWB39-E-CPSSOA-JURID        PIC 9(10).           
                 15 UORGWB39-E-CTPO-UND-ORGNZ      PIC 9(03).           
                 15 UORGWB39-E-CUND-ORGNZ          PIC 9(06).           
              10 UORGWB39-BLOCO-SAIDA.                                  
                 15 UORGWB39-S-CPSSOA-JUR          PIC 9(10).           
                 15 UORGWB39-S-NSEQ-ORG            PIC 9(08).           
                 15 UORGWB39-S-CUND-ORGNZ          PIC 9(06).           

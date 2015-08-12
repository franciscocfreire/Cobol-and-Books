      ******************************************************************
      * NOME BOOK : UORGWB97                                           *
      * DESCRICAO : INTERFACE DO SERVICO FUNCIONAL BATCH UORG2397.     *
      *             (CONSULTA CODIGO DA EMPRESA CADASTRADA NO SAP).    *
      * DATA      : 03/01/2011                                         *
      * AUTOR     : ALINE A. M. MARIANO                                *
      * EMPRESA   : STEFANINI                                          *
      * GRUPO     : TI MELHORIAS                                       *
      * COMPONENTE: UORG - UNIDADE ORGANIZACIONAL                      *
      * TAMANHO   : UORGWB97-TAM-LAYOUT                                *
      ******************************************************************
      * DATA       AUTOR       DESCRICAO / MANUTENCAO                  *
      ******************************************************************
      * XX/XX/XXXX XXXXXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *
      ******************************************************************
      *                                                                *
      * BLOCO DE IDENTIFICACAO                                         *
      * UORGWB97-COD-LAYOUT       = CODIGO DESTE LAYOUT                *
      * UORGWB97-TAM-LAYOUT       = TAMANHO DO REGISTRO                *
      *                                                                *
      * BLOCO DE ENTRADA                                               *
      *                                                                *
      * UORGWB97-E-AMBIENTE       = AMBIENTE DE EXECUCAO (D OU P)      *
      * UORGWB97-E-CEMPR-SAP      = CODIGO DA EMPRESA (SAP)            *
      * UORGWB97-E-CPSSOA-JURID   = CODIGO PESSOA JURIDICA (EMPRESA)   *
      *                                                                *
      * BLOCO DE SAIDA                                                 *
      *                                                                *
      * UORGWB97-S-CEMPR-SAP      = CODIGO DA EMPRESA (SAP)            *
      * UORGWB97-S-CPSSOA-JURID   = CODIGO PESSOA JURIDICA (EMPRESA)   *
      *                                                                *
      ******************************************************************
           05 UORGWB97-HEADER.                                          
              10 UORGWB97-COD-LAYOUT      PIC X(008) VALUE 'UORGWB97'.  
              10 UORGWB97-TAM-LAYOUT      PIC 9(005) VALUE      00058.  
           05 UORGWB97-REGISTRO.                                        
              10 UORGWB97-BLOCO-RETORNO.                                
                 15 UORGWB97-COD-RETORNO           PIC  9(002).         
                 15 UORGWB97-COD-ERRO              PIC  X(004).         
                 15 UORGWB97-COD-MENSAGEM          PIC  X(008).         
              10 UORGWB97-BLOCO-ENTRADA.                                
                 15 UORGWB97-E-AMBIENTE            PIC  X(001).         
                 15 UORGWB97-E-CEMPR-SAP           PIC  X(005).         
                 15 UORGWB97-E-CPSSOA-JURID        PIC  9(010).         
              10 UORGWB97-BLOCO-SAIDA.                                  
                 15 UORGWB97-S-CEMPR-SAP           PIC  X(005).         
                 15 UORGWB97-S-CPSSOA-JURID        PIC  9(010).         

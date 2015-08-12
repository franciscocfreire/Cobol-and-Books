      ******************************************************************
      * NOME BOOK : UORGWB88                                           *
      * DESCRICAO : INTERFACE DO SERVICO FUNCIONAL BATCH UORG2389.     *
      *             (CONSULTA DE/PARA DO CODIGO DO UORG E O CODIGO     *
      *             ATUAL NO LEGADO).                                  *
      * DATA      : 23/07/2012                                         *
      * AUTOR     : CRISTIANO FERNANDES                                *
      * EMPRESA   : BSITECNOLOGIA                                      *
      * GRUPO     : TI MELHORIAS                                       *
      * COMPONENTE: UORG - UNIDADE ORGANIZACIONAL                      *
      * TAMANHO   : UORGWB88-TAM-LAYOUT                                *
      ******************************************************************
      * DATA       AUTOR       DESCRICAO / MANUTENCAO                  *
      ******************************************************************
      * XX/XX/XXXX XXXXXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *
      ******************************************************************
      *                                                                *
      * UORGWB88-COD-LAYOUT         = CODIGO DESTE LAYOUT              *
      * UORGWB88-TAM-LAYOUT         = TAMANHO DO REGISTRO              *
      *                                                                *
      * BLOCO DE ENTRADA                                               *
      * UORGWB88-E-CTPO-PESQUISA    = TP. PESQUISA 1 OU 2 (OBRIGATORIO)*
      * UORGWB88-E-CPSSOA-JURID     = CODIGO DA EMPRESA (OBRIGATORIO)  *
      * UORGWB88-E-NSEQ-UND-ORGNZ   = NUMERO SEQUENCIAL DA UNIDADE     *
      *                               (OBRIGATORIO PARA TIPO 1)        *
      * UORGWB88-E-CIDTFD-TPO-UND   = IDENTIFICADOR TIPO UNIDADE       *
      *                               (OBRIGATORIO)                    *
      * UORGWB88-E-CRCONH-UND       = CODIGO RECONHECIMENTO UNIDADE    *
      *                               (OBRIGATORIO PARA TIPO 2)        *
      *                                                                *
      * BLOCO DE SAIDA                                                 *
      * UORGWB88-S-CPSSOA-JURID     =  COD. DA PESSOA JURIDICA         *
      * UORGWB88-S-NSEQ-UND-ORGNZ   =  NUMERO SEQUENCIAL DA UNIDADE    *
      * UORGWB88-S-CIDTFD-TPO-UND   =  IDENTIFICADOR TIPO UNIDADE      *
      * UORGWB88-S-CRCONH-UND       =  CODIGO RECONHECIMENTO UNIDADE   *
      *                                                                *
      ******************************************************************
           05 UORGWB88-HEADER.                                          
              10 UORGWB88-COD-LAYOUT      PIC X(008) VALUE 'UORGWB88'.  
              10 UORGWB88-TAM-LAYOUT      PIC 9(005) VALUE      00148.  
           05 UORGWB88-REGISTRO.                                        
              10 UORGWB88-BLOCO-RETORNO.                                
                 15 UORGWB88-COD-RETORNO           PIC  9(002).         
                 15 UORGWB88-COD-ERRO              PIC  X(004).         
                 15 UORGWB88-COD-MENSAGEM          PIC  X(008).         
              10 UORGWB88-BLOCO-ENTRADA.                                
                 15 UORGWB88-E-CTPO-PESQUISA       PIC  X(001).         
                 15 UORGWB88-E-CPSSOA-JURID        PIC  9(010).         
                 15 UORGWB88-E-NSEQ-UND-ORGNZ      PIC  9(008).         
                 15 UORGWB88-E-CIDTFD-TPO-UND      PIC  9(002).         
                 15 UORGWB88-E-CRCONH-UND          PIC  X(040).         
              10 UORGWB88-BLOCO-SAIDA.                                  
                 15 UORGWB88-S-CPSSOA-JURID        PIC  9(010).         
                 15 UORGWB88-S-NSEQ-UND-ORGNZ      PIC  9(008).         
                 15 UORGWB88-S-CIDTFD-TPO-UND      PIC  9(002).         
                 15 UORGWB88-S-CRCONH-UND          PIC  X(040).         

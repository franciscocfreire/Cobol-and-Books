      ******************************************************************
      * NOME BOOK : UORGWB25                                           *
      * DESCRICAO : INTERFACE DO SERVICO FUNCIONAL BATCH UORG2334.     *
      *             (CONSULTA DADOS EMPRESAS COLIGADAS AO GRUPO BRAD.) *
      * DATA      : 25/08/2010                                         *
      * AUTOR     : CRISTIANO FERNANDES                                *
      * EMPRESA   : STEFANINI                                          *
      * GRUPO     : TI MELHORIAS                                       *
      * COMPONENTE: UORG - UNIDADE ORGANIZACIONAL                      *
      * TAMANHO   : UORGWB25-TAM-LAYOUT                                *
      ******************************************************************
      * DATA       AUTOR       DESCRICAO / MANUTENCAO                  *
      ******************************************************************
      * 09/11/2010 CRISTIANO   INCLUSAO DE CAMPOS NA ENT/SAIDA         *
      ******************************************************************
      * BLOCO DE IDENTIFICACAO                                         *
      * UORGWB25-COD-LAYOUT       = CODIGO DESTE LAYOUT                *
      * UORGWB25-TAM-LAYOUT       = TAMANHO DO REGISTRO                *
      *                                                                *
      * BLOCO DE ENTRADA                                               *
      * UORGWB25-E-CNPJ-CORPO     = CODIGO DO CNPJ (CORPO)             *
      *                             (OPCIONAL)                         *
      * UORGWB25-E-CEMPR          = CODIGO DA EMPRESA                  *
      *                             (OPCIONAL)                         *
      *                                                                *
      * BLOCO DE SAIDA                                                 *
      * UORGWB25-S-CEMPR          = CODIGO DA EMPRESA                  *
      * UORGWB25-S-CNPJ-CORPO     = CODIGO DO CNPJ (CORPO)             *
      * UORGWB25-S-CFLIAL-CNPJ    = CODIGO DA FILIAL DO CNPJ           *
      * UORGWB25-S-CCTRL-CNPJ     = CODIGO DO CONTROLE DO CNPJ         *
      * UORGWB25-S-IDENT-COLIGAD  = IDENTIFICADOR SE A EMPRESA E COLI- *
      *                             GADA AO GRUPO BRADESCO.            *
      *                             S = SIM / N = NAO                  *
      * UORGWB25-S-IRZ-SCIAL      = NOME DA RAZAO SOCIAL EMPRESA       *
      * UORGWB25-S-IFANTS-EMPR    = NOME FANTASIA EMPRESA              *
      *                                                                *
      ******************************************************************
           05 UORGWB25-HEADER.                                          
              10 UORGWB25-COD-LAYOUT      PIC X(008) VALUE 'UORGWB25'.  
              10 UORGWB25-TAM-LAYOUT      PIC 9(005) VALUE      00212.  
           05 UORGWB25-REGISTRO.                                        
              10 UORGWB25-BLOCO-RETORNO.                                
                 15 UORGWB25-COD-RETORNO           PIC  9(002).         
                 15 UORGWB25-COD-ERRO              PIC  X(004).         
                 15 UORGWB25-COD-MENSAGEM          PIC  X(008).         
              10 UORGWB25-BLOCO-ENTRADA.                                
                 15 UORGWB25-E-CNPJ-CORPO          PIC  9(009).         
                 15 UORGWB25-E-CEMPR               PIC  9(010).         
              10 UORGWB25-BLOCO-SAIDA.                                  
                 15 UORGWB25-S-CEMPR               PIC  9(010).         
                 15 UORGWB25-S-CNPJ-CORPO          PIC  9(009).         
                 15 UORGWB25-S-CFLIAL-CNPJ         PIC  9(004).         
                 15 UORGWB25-S-CCTRL-CNPJ          PIC  9(002).         
                 15 UORGWB25-S-IDENT-COLIGAD       PIC  X(001).         
                 15 UORGWB25-S-IRZ-SCIAL           PIC  X(070).         
                 15 UORGWB25-S-IFANTS-EMPR         PIC  X(070).         

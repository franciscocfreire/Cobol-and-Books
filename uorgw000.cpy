      ***************************************************************   
      * NOME BOOK : UORGW000 - INTERFACE ENTRE PROGRAMAS            *   
      *             MODULO DE OBTENCAO DE CODIGO DE ERROS           *   
      * DESCRICAO : AREA DE USO COMUM PARA RETORNO DE DADOS         *   
      * DATA      : 16/11/2007                                      *   
      * AUTOR     : YAYOI                                           *   
      * EMPRESA   : CPM                                             *   
      * GRUPO     : TI MELHORIAS                                    *   
      * COMPONENTE: UORG - UNIDADE ORGANIZACIONAL                   *   
      * TAMANHO    : 27    BYTES                                    *   
      ***************************************************************   
      * DATA       AUTOR             DESCRICAO / MANUTENCAO         *   
      ***************************************************************   
      * XX/XX/XXXX XXXXXXXXXXXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *   
      ****************************************************************  
      *                                                              *  
      * UORGW000-COD-LAYOUT         = CODIGO DE LAYOUT               *  
      * UORGW000-COR-TAM-LAYOUT     = TAMANHO DO LAYOUT              *  
      * UORGW000-COD-RETORNO        = CODIGO DE RETORNO              *  
      * UORGW000-COR-ERRO           = CODIGO DE ERRO                 *  
      * UORGW000-COD-MENSAGEM       = CODIGO DE MENSAGEM             *  
      ****************************************************************  
           05 UORGW000-HEADER.                                          
              10 UORGW000-COD-LAYOUT       PIC X(008) VALUE 'UORGW000'. 
              10 UORGW000-TAM-LAYOUT       PIC 9(005) VALUE 27.         
           05 UORGW000-BLOCO-RETORNO.                                   
              10 UORGW000-COD-RETORNO      PIC 9(002).                  
              10 UORGW000-COD-ERRO         PIC X(004).                  
              10 UORGW000-COD-MENSAGEM     PIC X(008).                  

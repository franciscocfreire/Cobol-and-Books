      ***************************************************************   
      * NOME BOOK : I#CDPSCZ    - INTERFACE BATCH                   *   
      * DESCRICAO : CONSULTAR                                       *   
      *             CARACTERISTICAS FIXAS DE UM PRODUTO ESPECIFICO  *   
      * DATA      : 10/06/2010                                      *   
      * ANALISTA  : MARCEL CRIS VENDRAMINI MORENO - GRUPO 52        *   
      * AUTOR     : DORIVAL                                         *   
      * EMPRESA   : CPMBRAXIS                                       *   
      * GRUPO     : 52                                              *   
      * COMPONENTE: SISTEMA CADASTRO DE PRODUTOS E SERVICOS         *   
      * TAMANHO   : 334 BYTES                                       *   
      ***************************************************************   
      *                                                             *   
      ***************************************************************   
      * DATA       AUTOR             DESCRICAO / MANUTENCAO         *   
      ***************************************************************   
      * XX/XX/XXXX XXXXXXXXXXXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *   
      ***************************************************************   
          05 CDPSGLCZ-HEADER.                                           
             10 CDPSGLCZ-COD-LAYOUT       PIC X(08) VALUE 'CDPSGLCZ'.   
             10 CDPSGLCZ-TAM-LAYOUT       PIC 9(05) VALUE  334.         
          05 CDPSGLCZ-REGISTRO.                                         
             10 CDPSGLCZ-ENTRADA.                                       
                15 CDPSGLCZ-E-CPRODT-SERVC         PIC 9(008).          
             10 CDPSGLCZ-BLOCO-RETORNO.                                 
                15 CDPSGLCZ-COD-RETORNO            PIC 9(002).          
                15 CDPSGLCZ-COD-ERRO               PIC X(004).          
                15 CDPSGLCZ-COD-MENSAGEM           PIC X(008).          
                15 CDPSGLCZ-DESC-MENSAGEM          PIC X(050).          
             10 CDPSGLCZ-SAIDA.                                         
                15 CDPSGLCZ-S-RPRODT-SERVC         PIC X(100).          
                15 CDPSGLCZ-S-RRSUMO-PRODT-SERVC   PIC X(030).          
                15 CDPSGLCZ-S-CTPO-PRODT-SERVC     PIC 9(001).          
                15 CDPSGLCZ-S-CCLASF-PRODT-SERVC   PIC 9(003).          
                15 CDPSGLCZ-S-CPSSOA-JURID-GTORA   PIC 9(010).          
                15 CDPSGLCZ-S-NSEQ-UND-GTORA       PIC 9(008).          
                15 CDPSGLCZ-S-CSIST                PIC X(004).          
                15 CDPSGLCZ-S-DINIC-PRODT-SERVC    PIC X(010).          
                15 CDPSGLCZ-S-DFIM-VGCIA           PIC X(010).          
                15 CDPSGLCZ-S-CINDCD-OFERT-PRODT   PIC X(001).          
                15 CDPSGLCZ-S-CINDCD-PRODT-LGADO   PIC X(001).          
                15 CDPSGLCZ-S-CINSTC-RCONH-PRODT   PIC 9(005).          
                15 CDPSGLCZ-S-CPRODT-INSTC-RCONH   PIC 9(005).          
                15 CDPSGLCZ-S-RPRODT-INSTC-RCONH   PIC X(060).          
                15 CDPSGLCZ-S-CSIT-PRODT-OPER      PIC 9(001).          

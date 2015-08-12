      ******************************************************            
      *                                                    *            
      * NOME BOOK : FRWKGMOD                               *            
      * DESCRICAO : ESTRUTURA COMUN PARA ERROS.            *            
      * DATA      : 29/08/2006                             *            
      * AUTOR     : CARLA PARALS SENDIN                    *            
      * EMPRESA   : CPM/GFT                                *            
      * GRUPO     : TI MELHORIAS                           *            
      * COMPONENTE: FRAMEWORK ONLINE                       *            
      *                                                    *            
      ******************************************************            
      *                                                    *            
      * FRWKGMOD-HEADER.                                   *            
      *   FRWKGMOD-COD-LAYOUT     = CODIGO DE LAYOUT       *            
      *   FRWKGMOD-TAM-LAYOUT     = TAMANHO DO LAYOUT      *            
      * FRWKGMOD-REGISTRO.                                 *            
      *   FRWKGMOD-NOME-MODULO    = NOME DO PGM QUE CHAMADO*            
      *   FRWKGMOD-BLOCO-RETORNO.                          *            
      *     FRWKGMOD-COD-RETORNO  = COD DE RETORNO DO MOD  *            
      *     FRWKGMOD-COD-ERRO     = COD DE ERRO DO MODULO  *            
      *     FRWKGMOD-COD-MENSAGEM = MSG RETORNADA PELO MOD *            
      *                                                    *            
      ******************************************************            
      * DATA       AUTOR        MODIFICACAO                *            
      * --------   ---------    -------------------------- *            
      * DD/MM/AAAA JNNNNNN      XXXXXXXXXXXXXXXXXXXXXXXXXX *            
      ******************************************************            
       05 FRWKGMOD-HEADER.                                              
          07 FRWKGMOD-COD-LAYOUT PIC X(008) VALUE 'FRWKGMOD'.           
          07 FRWKGMOD-TAM-LAYOUT PIC 9(005) VALUE 35.                   
       05 FRWKGMOD-REGISTRO.                                            
          07 FRWKGMOD-NOME-MODULO           PIC X(08).                  
          07 FRWKGMOD-BLOCO-RETORNO.                                    
             09 FRWKGMOD-COD-RETORNO        PIC 9(02).                  
             09 FRWKGMOD-COD-ERRO           PIC X(04).                  
             09 FRWKGMOD-COD-MENSAGEM       PIC X(08).                  

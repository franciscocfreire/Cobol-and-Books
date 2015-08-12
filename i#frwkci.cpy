      ******************************************************            
      *                                                    *            
      * NOME BOOK : FRWKGCIC                               *            
      * DESCRICAO : ESTRUTURA COMUN PARA CICS              *            
      * DATA      : 29/08/2006                             *            
      * AUTOR     : CARLA PARALS SENDIN                    *            
      * EMPRESA   : CPM/GFT                                *            
      * GRUPO     : TI MELHORIAS                           *            
      * COMPONENTE: FRAMEWORK ONLINE                       *            
      *                                                    *            
      ******************************************************            
      *                                                    *            
      * FRWKGCIC-HEADER.                                   *            
      *   FRWKGCIC-COD-LAYOUT     = CODIGO DE LAYOUT       *            
      *   FRWKGCIC-TAM-LAYOUT     = TAMANHO DO LAYOUT      *            
      * FRWKGCIC-REGISTRO.                                 *            
      *   FRWKGCIC-EIBFN          = CóDIGO DO COMANDO CICS *            
      *   FRWKGCIC-EIBRCODE       = EIBRCODE DO CICS       *            
      *   FRWKGCIC-EIBRSRCE       = IDENT RESOURCE         *            
      *   FRWKGCIC-EIBRESP        = EIBRESP DO CICS        *            
      *   FRWKGCIC-EIBRESP2       = EIBRESP DO CICS        *            
      *   FRWKGCIC-EIBTASKN       = NúMERO IDENT TASK CICS *            
      *                                                                 
      ******************************************************            
      * DATA       AUTOR        MODIFICACAO                *            
      * --------   ---------    -------------------------- *            
      * DD/MM/AAAA JNNNNNN      XXXXXXXXXXXXXXXXXXXXXXXXXX *            
      ******************************************************            
       05 FRWKGCIC-HEADER.                                              
          07 FRWKGCIC-COD-LAYOUT PIC X(008) VALUE 'FRWKGCIC'.           
          07 FRWKGCIC-TAM-LAYOUT PIC 9(005) VALUE 41.                   
       05 FRWKGCIC-REGISTRO.                                            
          07 FRWKGCIC-EIBFN                 PIC  X(02).                 
          07 FRWKGCIC-EIBRCODE              PIC  X(06).                 
          07 FRWKGCIC-EIBRSRCE              PIC  X(08).                 
          07 FRWKGCIC-EIBRESP               PIC S9(08) COMP.            
          07 FRWKGCIC-EIBRESP2              PIC S9(08) COMP.            
          07 FRWKGCIC-EIBTASKN              PIC S9(07) COMP-3.          

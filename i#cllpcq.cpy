      *---------------------------------------------------------------* 
      *          DEVEDORES NAO SELECIONADOS PARA SERASA / SPC         * 
      *                  INC I#CLLPCQ   LRECL  215                    * 
      *---------------------------------------------------------------* 
                                                                        
       01  INCON-REGTO.                                                 
           03  INCON-CHAVE.                                             
               05  INCON-DEV.                                           
                   07  INCON-NUMERO-DEV     PIC 9(09)   COMP-3.         
                   07  INCON-FILIAL         PIC 9(05)   COMP-3.         
                   07  INCON-CTR-DEV        PIC 9(02).                  
               05  INCON-COD-NATUREZA-OPER  PIC X(03).                  
               05  INCON-CONTRATO           PIC 9(07)   COMP-3.         
           03  INCON-EMPRESA                PIC 9(05)   COMP-3.         
           03  INCON-AGENCIA                PIC 9(05)   COMP-3.         
           03  INCON-CONTA                  PIC 9(07)   COMP-3.         
           03  INCON-CARTEIRA               PIC X(03).                  
           03  INCON-VCTO                   PIC 9(08).                  
           03  INCON-ID                     PIC X(02).                  
           03  INCON-MOEDA                  PIC X(02).                  
           03  INCON-TIPO-PEND              PIC 9(05)   COMP-3.         
           03  INCON-COD-OCORR              PIC 9(03)   COMP-3.         
           03  INCON-TOT-VR-RESGATE         PIC 9(13)V99   COMP-3.      
           03  INCON-TOT-VR-VENCIDOS        PIC 9(13)V99   COMP-3.      
           03  INCON-TOT-VR-VINCENDOS       PIC 9(13)V99   COMP-3.      
           03  INCON-NOME-DEVEDOR.                                      
               05  INCON-NOME-20            PIC X(20).                  
               05  FILLER                   PIC X(20).                  
           03  INCON-NOME-AVALISTA          PIC X(40).                  
           03  INCON-CPF-AVAL.                                          
               05  INCON-NUMERO-AVAL        PIC 9(09)   COMP-3.         
               05  INCON-FILIAL-AVAL        PIC 9(05)   COMP-3.         
               05  INCON-CTR-AVAL           PIC 9(02).                  
           03  INCON-NOME-AVAL2             PIC X(40).                  
           03  INCON-CPF-AVAL2.                                         
               05  INCON-NUMERO-AVAL2       PIC 9(09)   COMP-3.         
               05  INCON-FILIAL-AVAL2       PIC 9(05)   COMP-3.         
               05  INCON-CTR-AVAL2          PIC 9(02).                  
           03  INCON-COD-EMPR               PIC 9(02).                  
           03  INCON-COD-ERRO               PIC 9(02).                  

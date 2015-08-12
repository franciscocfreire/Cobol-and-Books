      *************************************************************     
      *  INC IMAGEM DO RELATORIO GERADO PELO CLLP7637 - I#CLLPAR  *     
      *                                            * LRECL = 0116 *     
      *************************************************************     
       01  IMAGAVIS-REGISTRO.                                           
           03  IMAGAVIS-AGENCIA    PIC  9(005).                         
           03  IMAGAVIS-CONTA      PIC  9(007).                         
           03  IMAGAVIS-NUMCGC     PIC  9(009).                         
           03  IMAGAVIS-FILCGC     PIC  9(004).                         
           03  IMAGAVIS-DIGCGC     PIC  9(002).                         
           03  IMAGAVIS-NOMECLI    PIC  X(036).                         
           03  IMAGAVIS-CART       PIC  X(003).                         
           03  IMAGAVIS-TIPO       PIC  X(007).                         
           03  IMAGAVIS-VENCIMENTO PIC  9(008).                         
           03  FILLER REDEFINES IMAGAVIS-VENCIMENTO.                    
               05  IMAGAVIS-DD     PIC 9(002).                          
               05  IMAGAVIS-MM     PIC 9(002).                          
               05  IMAGAVIS-AA     PIC 9(004).                          
           03  IMAGAVIS-VALOR      PIC 9(011)V99.                       
           03  IMAGAVIS-NATUREZA   PIC X(002).                          
           03  IMAGAVIS-DESCRICAO  PIC X(020).                          

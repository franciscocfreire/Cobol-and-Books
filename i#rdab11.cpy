      ***************************************************************** 
      *  - I#RDAB11 ARQUIVO DE CADASTRO DE PESSOA JURIDICA  =   081   * 
      *  - DATA DA  CRIACAO : 24/01/03   POR: CARLOS CARREIRA - CPM   * 
      ***************************************************************** 
       01  APJ-REG.                                                     
           05   APJ-CBASE-CPF             PIC S9(009)      COMP-3.      
           05   APJ-CFLIAL-CNPJ           PIC S9(005)      COMP-3.      
           05   APJ-CCTRL-CNPJ-CPF        PIC S9(002)      COMP-3.      
           05   APJ-IPRIM-TTLAR-RENEG     PIC  X(040).                  
           05   APJ-DFUNDC-EMPR-RENEG     PIC  X(010).                  
           05   APJ-VFATMT-EMPR-RENEG     PIC S9(015)V99   COMP-3.      
           05   APJ-CINDCD-COBR-RENEG     PIC  X(001).                  
      *         INDICADOR DE COBRANCA: 0-NAO                            
      *                                1-CACS                           
           05   APJ-CINDCD-RENEG-BASE     PIC  X(001).                  
      *         INDICADOR DE RENEGOCIACAO : 0-NAO                       
      *                                     1-PODE RENEGOCIAR           
      *                                     2-PROPOSTA RENEGOCIACAO     
      *                                     3-EM RENEGOCIACAO           
           05   APJ-HATULZ                PIC  X(010).                  
      *                                                                 
      *APJ-REG                                       1    81  A         
      *APJ-CBASE-CPF                                 1     5  P         
      *APJ-CFLIAL-CNPJ                               6     3  P         
      *APJ-CCTRL-CNPJ-CPF                            9     2  P         
      *APJ-IPRIM-TTLAR-RENEG                        11    40  A         
      *APJ-DFUNDC-EMPR-RENEG                        51    10  A         
      *APJ-VFATMT-EMPR-RENEG                        61     9  P   2     
      *APJ-CINDCD-COBR-RENEG                        70     1  A         
      *APJ-CINDCD-RENEG-BASE                        71     1  A         
      *APJ-HATULZ                                   72    10  A         
      *                                                                 

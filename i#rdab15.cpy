      ***************************************************************** 
      *  - I#RDAB15 ARQUIVO DE CADASTRO DE TELEFONE         =   044   * 
      *  - DATA DA  CRIACAO : 24/01/03   POR: CARLOS CARREIRA - CPM   * 
      ***************************************************************** 
                                                                        
       01  AFN-REG.                                                     
           05   AFN-CBCO                      PIC S9(003)      COMP-3.  
           05   AFN-CAG-BCRIA                 PIC S9(005)      COMP-3.  
           05   AFN-CCTA-CORR                 PIC S9(013)      COMP-3.  
           05   AFN-CINDCD-ORIGE-FONE         PIC  X(001).              
      *         INDICADOR DE ORIGEM DE TELEFONE: 1-FICA                 
      *                                          2-CLIE                 
      *                                          3-CACS                 
      *                                          4-RENEGOCIACAO         
           05   AFN-CINDCD-TPO-FONE           PIC  X(001).              
      *         INDICADOR DE TIPO DE TELEFONE: 1-RESIDENCIAL            
      *                                        2-COMERCIAL              
      *                                        3-CELULAR                
      *                                        4-RECADO                 
           05   AFN-CSEQ-FONE                 PIC S9(002)      COMP-3.  
           05   AFN-CDDD-CLI-RENEG            PIC  X(004).              
BRQ059     05   AFN-CFONE-RENEG               PIC  X(011).              
           05   AFN-CINDCD-FONE               PIC  X(001).              
      *         INDICADOR DE TELEFONE INCORRETO: 1-SIM                  
      *                                          2-NAO                  
           05   AFN-CINDCD-ATULZ              PIC S9(001)      COMP-3.  
      *         INDICADOR DE ATUALIZACAO: 1-SIM                         
      *                                   2-NAO                         
           05   AFN-HATULZ                    PIC  X(010).              
           05   AFN-CSIT-RETOR-FONE           PIC  X(001).              
      ***************************************************************   
      *AFN-REG                              1    40  A                  
      *AFN-CBCO                             1     2  P                  
      *AFN-CAG-BCRIA                        3     3  P                  
      *AFN-CCTA-CORR                        6     7  P                  
      *AFN-CINDCD-ORIGE-FONE               13     1  A                  
      *AFN-CINDCD-TPO-FONE                 14     1  A                  
      *AFN-CSEQ-FONE                       15     2  P                  
      *AFN-CDDD-CLI-RENEG                  17     4  A                  
      *AFN-CFONE-RENEG                     21     8  A                  
      *AFN-CINDCD-FONE                     29     1  A                  
      *AFN-CINDCD-ATULZ                    30     1  P                  
      *AFN-HATULZ                          31    10  A                  
      *AFN-CSIT-RETOR-FONE                 41    01  A                  

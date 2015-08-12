      ***************************************************************** 
      *  - I#RDAB13 ARQUIVO DE CADASTRO DE CONTA CORRENTE   =   098   * 
      *  - DATA DA  CRIACAO : 24/01/03   POR: CARLOS CARREIRA - CPM   * 
      ***-************************************************************* 
       01  ACC-REG.                                                     
           05   ACC-CBCO                      PIC S9(003)      COMP-3.  
           05   ACC-CAG-BCRIA                 PIC S9(005)      COMP-3.  
           05   ACC-CCTA-CORR                 PIC S9(013)      COMP-3.  
           05   ACC-CBASE-CPF                 PIC S9(009)      COMP-3.  
           05   ACC-CFLIAL-CNPJ               PIC S9(005)      COMP-3.  
           05   ACC-CCTRL-CNPJ-CPF            PIC S9(002)      COMP-3.  
           05   ACC-CIDTFD-TPO-PSSOA          PIC  X(001).              
      *         INDICADOR DE TIPO DE PESSOA: 1-FISICA                   
      *                                      2-JURIDICA                 
           05   ACC-DABERT-CTA-RENEG          PIC  X(010).              
           05   ACC-IOUTRO-TTLAR              PIC  X(040).              
           05   ACC-IOUTRO-TTLAR-NULL         PIC  X(001).              
           05   ACC-CINDCD-EXTR-BLOQ          PIC  X(001).              
      *         INDICADOR DE EXTRATO BLOQUEADO: 1-SIM                   
      *                                         2-NAO                   
           05   ACC-CINDCD-CTA-GERC           PIC S9(001)      COMP-3.  
      *         INDICADOR DE CONTA GERENCIADA: 1-SIM                    
      *                                        2-NAO                    
           05   ACC-CINDCD-PRVTE              PIC  X(001).              
      *         INDICADOR DE CONTA PRIVATE: 0-NAO                       
      *                                     1-SIM                       
           05   ACC-CLETRA-RTING-RENEG        PIC  X(002).              
           05   ACC-CRAMO-ATVDD-RENEG         PIC S9(005)      COMP-3.  
           05   ACC-HATULZ                    PIC  X(010).              
           05   ACC-CPOSTO-SERVC              PIC S9(003)      COMP-3.  
           05   ACC-CTPO-POSTO-SERVC          PIC S9(002)      COMP-3.  
           05   ACC-CSGMTO-CLI                PIC S9(003)      COMP-3.  
      ****************************************************************  
      *ACC-REG                                        1    98  A        
      *ACC-CBCO                                       1     2  P        
      *ACC-CAG-BCRIA                                  3     3  P        
      *ACC-CCTA-CORR                                  6     7  P        
      *ACC-CBASE-CPF                                 13     5  P        
      *ACC-CFLIAL-CNPJ                               18     3  P        
      *ACC-CCTRL-CNPJ-CPF                            21     2  P        
      *ACC-CIDTFD-TPO-PSSOA                          23     1  A        
      *ACC-DABERT-CTA-RENEG                          24    10  A        
      *ACC-IOUTRO-TTLAR                              34    40  A        
      *ACC-IOUTRO-TTLAR-NULL                         74     1  A        
      *ACC-CINDCD-EXTR-BLOQ                          75     1  A        
      *ACC-CINDCD-CTA-GERC                           76     1  P        
      *ACC-CINDCD-PRVTE                              77     1  A        
      *ACC-CLETRA-RTING-RENEG                        78     2  A        
      *ACC-CRAMO-ATVDD-RENEG                         80     3  P        
      *ACC-HATULZ                                    83    10  A        
      *ACC-CPOSTO-SERVC                              93     2  P        
      *ACC-CTPO-POSTO-SERVC                          95     2  P        
      *ACC-CSGMTO-CLI                                97     2  P        

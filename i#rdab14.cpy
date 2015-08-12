      ***************************************************************** 
      *  - I#RDAB14 ARQUIVO DE CADASTRO DE LOGRADOURO       =   146   * 
      *  - DATA DA  CRIACAO : 24/01/03   POR: CARLOS CARREIRA - CPM   * 
      ***************************************************************** 
       01  ALG-REG.                                                     
           05   ALG-CBCO                  PIC S9(003)      COMP-3.      
           05   ALG-CAG-BCRIA             PIC S9(005)      COMP-3.      
           05   ALG-CCTA-CORR             PIC S9(013)      COMP-3.      
           05   ALG-CINDCD-ORIGE-LOGDR    PIC  X(001).                  
      *         INDICADOR DE ORIGEM DE LOGRADOURO: 1-FICA               
      *                                            2-CLIE               
      *                                            3-CACS               
      *                                            4-RENEGOCIACAO       
           05   ALG-CINDCD-TPO-LOGDR      PIC  X(001).                  
      *         INDICADOR DE TIPO LOGRADOURO: 1-RESIDENCIAL             
      *                                       2-COMERCIAL               
      *                                       3-CORRESPONDENCIA         
           05   ALG-ELOGDR-CLI-RENEG      PIC  X(040).                  
           05   ALG-ENRO-LOGDR-RENEG      PIC  X(007).                  
           05   ALG-ECOMPL-LOGDR-RENEG    PIC  X(020).                  
           05   ALG-EBAIRO-LOGDR-RENEG    PIC  X(020).                  
           05   ALG-IMUN-IBGE-RENEG       PIC  X(025).                  
           05   ALG-CCEP-CLI-RENEG        PIC S9(005)      COMP-3.      
           05   ALG-CCEP-COMPL-RENEG      PIC S9(003)      COMP-3.      
           05   ALG-CSGL-UF-CLI-RENEG     PIC  X(002).                  
           05   ALG-CINDCD-LOGDR          PIC  X(001).                  
      *         INDICADOR DE LOGRADOURO INCORRETO: 1-SIM                
      *                                            2-NAO                
           05   ALG-CINDCD-ATULZ          PIC S9(001)      COMP-3.      
      *         INDICADOR DE ATUALIZACAO: 1-SIM                         
      *                                   2-NAO                         
           05   ALG-HATULZ                PIC  X(010).                  
           05   ALG-CSIT-RETOR-LOGDR      PIC  X(01).                   
      ****************************************************************  
      *ALG-REG                                        1   145  A        
      *ALG-CBCO                                       1     2  P        
      *ALG-CAG-BCRIA                                  3     3  P        
      *ALG-CCTA-CORR                                  6     7  P        
      *ALG-CINDCD-ORIGE-LOGDR                        13     1  A        
      *ALG-CINDCD-TPO-LOGDR                          14     1  A        
      *ALG-ELOGDR-CLI-RENEG                          15    40  A        
      *ALG-ENRO-LOGDR-RENEG                          55     7  A        
      *ALG-ECOMPL-LOGDR-RENEG                        62    20  A        
      *ALG-EBAIRO-LOGDR-RENEG                        82    20  A        
      *ALG-IMUN-IBGE-RENEG                          102    25  A        
      *ALG-CCEP-CLI-RENEG                           127     3  P        
      *ALG-CCEP-COMPL-RENEG                         130     2  P        
      *ALG-CSGL-UF-CLI-RENEG                        132     2  A        
      *ALG-CINDCD-LOGDR                             134     1  A        
      *ALG-CINDCD-ATULZ                             135     1  N        
      *ALG-HATULZ                                   136    10  A        
      *ALG-CINDCD-LOGR-INCOR                        146     1  A        

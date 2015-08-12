      ***************************************************************** 
BRQ059*  - I#RDABS7 CADASTRO DE LOGRADOURO/TELEFONE         =   343   * 
      *  - DATA DA  CRIACAO : 29/08/04   POR: HENRIQUE      -   DDS   * 
      ***************************************************************** 
       01  ALG-REG.                                                     
           05   AET-CHAVE.                                              
                10  AET-CBCO              PIC S9(003)      COMP-3.      
                10  AET-CAG-BCRIA         PIC S9(005)      COMP-3.      
                10  AET-CCTA-CORR         PIC S9(013)      COMP-3.      
           05   AET-ELOGDR-RES            PIC  X(040).                  
           05   AET-ENRO-LOGDR-RES        PIC  X(007).                  
           05   AET-ECOMPL-LOGDR-RES      PIC  X(020).                  
           05   AET-EBAIRO-LOGDR-RES      PIC  X(020).                  
           05   AET-IMUN-IBGE-RES         PIC  X(025).                  
           05   AET-CCEP-CLI-RES          PIC S9(005)      COMP-3.      
           05   AET-CCEP-COMPL-RES        PIC S9(003)      COMP-3.      
           05   AET-CSGL-UF-CLI-RES       PIC  X(002).                  
           05   AET-CINDCD-LOGDR-RES      PIC  X(001).                  
           05   AET-ELOGDR-COM            PIC  X(040).                  
           05   AET-ENRO-LOGDR-COM        PIC  X(007).                  
           05   AET-ECOMPL-LOGDR-COM      PIC  X(020).                  
           05   AET-EBAIRO-LOGDR-COM      PIC  X(020).                  
           05   AET-IMUN-IBGE-COM         PIC  X(025).                  
           05   AET-CCEP-CLI-COM          PIC S9(005)      COMP-3.      
           05   AET-CCEP-COMPL-COM        PIC S9(003)      COMP-3.      
           05   AET-CSGL-UF-CLI-COM       PIC  X(002).                  
           05   AET-CINDCD-LOGDR-COM      PIC  X(001).                  
           05   AET-TAB-FONE              OCCURS 6 TIMES.               
                07 AET-CINDCD-TPO-FONE    PIC  X(001).                  
                07 AET-CDDD-CLI-RENEG     PIC  X(004).                  
BRQ059          07 AET-CFONE-RENEG        PIC  X(011).                  
                07 AET-CINDCD-FONE        PIC  X(001).                  
           05   FILLER                    PIC  X(004).                  
      ***************************************************************** 
      *   ALG-REG                                   1   340  A          
      *   AET-CBCO                                  1     2  P          
      *   AET-CAG-BCRIA                             3     3  P          
      *   AET-CCTA-CORR                             6     7  P          
      *   AET-ELOGDR-RES                           13    40  A          
      *   AET-ENRO-LOGDR-RES                       53     7  A          
      *   AET-ECOMPL-LOGDR-RES                     60    20  A          
      *   AET-EBAIRO-LOGDR-RES                     80    20  A          
      *   AET-IMUN-IBGE-RES                       100    25  A          
      *   AET-CCEP-CLI-RES                        125     3  P          
      *   AET-CCEP-COMPL-RES                      128     2  P          
      *   AET-CSGL-UF-CLI-RES                     130     2  A          
      *   AET-CINDCD-LOGDR-RES                    132     1  A          
      *   AET-ELOGDR-COM                          133    40  A          
      *   AET-ENRO-LOGDR-COM                      173     7  A          
      *   AET-ECOMPL-LOGDR-COM                    180    20  A          
      *   AET-EBAIRO-LOGDR-COM                    200    20  A          
      *   AET-IMUN-IBGE-COM                       220    25  A          
      *   AET-CCEP-CLI-COM                        245     3  P          
      *   AET-CCEP-COMPL-COM                      248     2  P          
      *   AET-CSGL-UF-CLI-COM                     250     2  A          
      *   AET-CINDCD-LOGDR-COM                    252     1  A          
      *   AET-TAB-FONE                            253    14  A OCCURS 6 
      *   AET-CINDCD-TPO-FONE      AET-TAB-FONE           1  A          
      *   AET-CDDD-CLI-RENEG       AET-TAB-FONE    +1     4  A          
      *   AET-CFONE-RENEG          AET-TAB-FONE    +5     8  A          
      *   AET-CINDCD-FONE          AET-TAB-FONE   +13     1  A          

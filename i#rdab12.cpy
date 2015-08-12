      ***************************************************************** 
      *  - I#RDAB12 ARQUIVO DE CADASTRO DE PESSOA FISICA    =   270   * 
      *  - DATA DA  CRIACAO : 24/01/03   POR: CARLOS CARREIRA - CPM   * 
      *  - DATA DA  ALTERAC : 08/09/04   POR: LEANDRO  -  RONNAM      * 
      ***************************************************************** 
       01  APF-REG.                                                     
           05   APF-CBASE-CPF                 PIC S9(009)      COMP-3.  
           05   APF-CCTRL-CNPJ-CPF            PIC S9(002)      COMP-3.  
           05   APF-IPRIM-TTLAR-RENEG         PIC  X(040).              
           05   APF-DNASC-CLI-RENEG           PIC  X(010).              
           05   APF-VRENDA-CLI-RENEG          PIC S9(015)V99   COMP-3.  
           05   APF-IPAI-CLI-RENEG            PIC  X(040).              
           05   APF-IPAI-NULL                 PIC  X(001).              
           05   APF-IMAE-CLI-RENEG            PIC  X(040).              
           05   APF-IMAE-NULL                 PIC  X(001).              
           05   APF-ICONJG-CLI-RENEG          PIC  X(040).              
           05   APF-ICONJG-NULL               PIC  X(001).              
           05   APF-CINDCD-COBR-RENEG         PIC  X(001).              
      *         INDICADOR DE COBRANCA: 0-NAO                            
      *                                1-CACS                           
           05   APF-CINDCD-RENEG-BASE         PIC  X(001).              
      *         INDICADOR DE RENEGOCIACAO : 0-NAO                       
      *                                     1-PODE RENEGOCIAR           
      *                                     2-PROPOSTA RENEGOCIACAO     
      *                                     3-EM RENEGOCIACAO           
           05   APF-HATULZ                    PIC  X(010).              
           05   APF-CTPO-DOCTO-ID             PIC  X(020).              
           05   APF-CTPO-DOCTO-ID-NULL        PIC  X(001).              
           05   APF-CDOCTO-ID                 PIC  X(015).              
           05   APF-CDOCTO-ID-NULL            PIC  X(001).              
           05   APF-IORG-EMISR-ID             PIC  X(020).              
           05   APF-IORG-EMISR-ID-NULL        PIC  X(001).              
           05   APF-DEMIS                     PIC  X(010).              
           05   APF-DEMIS-NULL                PIC  X(001).              
      *                                                                 
      *APF-REG                                       1   270  A         
      *APF-CBASE-CPF                                 1     5  P         
      *APF-CCTRL-CNPJ-CPF                            6     2  P         
      *APF-IPRIM-TTLAR-RENEG                         8    40  A         
      *APF-DNASC-CLI-RENEG                          48    10  A         
      *APF-VRENDA-CLI-RENEG                         58     9  P   2     
      *APF-IPAI-CLI-RENEG                           67    40  A         
      *APF-IPAI-NULL                               107     1  A         
      *APF-IMAE-CLI-RENEG                          108    40  A         
      *APF-IMAE-NULL                               148     1  A         
      *APF-ICONJG-CLI-RENEG                        149    40  A         
      *APF-ICONJG-NULL                             189     1  A         
      *APF-CINDCD-COBR-RENEG                       190     1  A         
      *APF-CINDCD-RENEG-BASE                       191     1  A         
      *APF-HATULZ                                  192    10  A         
      *APF-CTPO-DOCTO-ID                           202    20  A         
      *APF-CTPO-DOCTO-ID-NULL                      222     1  A         
      *APF-CDOCTO-ID                               223    15  A         
      *APF-CDOCTO-ID-NULL                          238     1  A         
      *APF-IORG-EMISR-ID                           239    20  A         
      *APF-IORG-EMISR-ID-NULL                      259     1  A         
      *APF-DEMIS                                   260    10  A         
      *APF-DEMIS-NULL                              270     1  A         

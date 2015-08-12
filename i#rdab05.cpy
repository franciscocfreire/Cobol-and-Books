      ***************************************************************** 
      *  - I#RDAB05 ARQUIVO DE DADOS DA PESSOA            LRECL = 500 * 
      *  - DATA DA  CRIACAO : 08/01/03   POR: HENRY HIGA - CPM        * 
      **-************************************************************** 
                                                                        
       01  ADP-REGISTRO.                                                
           05  CBASE-CPF             PIC  9(09)       COMP-3.           
           05  CFILIAL-CNPJ          PIC  9(05)       COMP-3.           
           05  CCTRL-CNPJ-CPF        PIC  9(02)       COMP-3.           
           05  CPSSOA-CADTR          PIC  X(26).                        
           05  CD-CLI                PIC  X(26).                        
           05  IPRIM-TTLAR           PIC  X(40).                        
           05  DFUNDC-EMPR           PIC  X(10).                        
           05  VFATMT-EMPR           PIC  9(15)V9(02) COMP-3.           
           05  CINDCD-COBR           PIC  X(01).                        
           05  IPAI-CLI              PIC  X(40).                        
           05  IMAE-CLI              PIC  X(40).                        
           05  ICONJG-CLI            PIC  X(40).                        
           05  ICONJG-CLI-NULL       PIC  X(01).                        
           05  CBCO                  PIC  9(03)       COMP-3.           
           05  CAG-BCRIA             PIC  9(05)       COMP-3.           
           05  CCTA-CORR             PIC  9(13)       COMP-3.           
           05  CIDTFD-TPO-PSSOA      PIC  X(01).                        
           05  DABERT-CTA            PIC  X(10).                        
           05  IOUTRO-TTLAR          PIC  X(40).                        
           05  IOUTRO-TTLAR-NULL     PIC  X(01).                        
           05  CINDCD-EXTR-BLOQ      PIC  X(01).                        
           05  CINDCD-CTA-GERC       PIC  X(01).                        
           05  CINDCD-PRVTE          PIC  X(01).                        
           05  CLETRA-RTING          PIC  X(02).                        
           05  CRAMO-ATVDD           PIC  9(05)       COMP-3.           
           05  CINDCD-ORIGE          PIC  X(01).                        
           05  CINDCD-TPO-LOGDR      PIC  X(01).                        
           05  ELOGDR-CLI            PIC  X(40).                        
           05  ENUM-LOGDR            PIC  X(07).                        
           05  ECOMPL-LOGDR          PIC  X(20).                        
           05  EBAIRO-LOGDR          PIC  X(20).                        
           05  CMUN-IBGE             PIC  9(07)       COMP-3.           
           05  IMUN-IBGE             PIC  X(25).                        
           05  CCEP-CLI              PIC  9(05)       COMP-3.           
           05  CCEP-COMPL            PIC  9(03)       COMP-3.           
           05  CSGL-UF-CLI           PIC  X(02).                        
           05  CINDCD-LOGDR          PIC  X(01).                        
           05  CINDCD-ATULZ          PIC  X(01).                        
           05  CINDCD-TPO-FONE       PIC  X(01).                        
           05  CDDD-CLI              PIC  X(04).                        
BRQ059     05  CFONE                 PIC  X(11).                        
           05  HATULZ                PIC  X(10).                        
           05  STATUS-FICHA          PIC  X(02).                        
           05  CARTEIRA              PIC  9(05)       COMP-3.           
           05  CARTEIRA-R  REDEFINES  CARTEIRA        PIC  X(03).       
           05  CONTRATO              PIC  9(17)       COMP-3.           
           05  CPOSTO-SERVC          PIC  9(03)       COMP-3.           
           05  CSGMTO-CLI            PIC  9(03)       COMP-3.           
BRQ006     05  CCLUB                 PIC  9(10)       COMP-3.           
BRQ006     05  FILLER                PIC  X(08).                        

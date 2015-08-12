      ***************************************************************** 
      *  - I#RDAB01 ARQUIVO DE PARCELAS VENCIDAS/VINCENDAS LRECL= 150 * 
      *  - DATA DA  CRIACAO : 26/12/02   POR: MAGALI LIMA - CPM       * 
      ***************************************************************** 
                                                                        
       01  PVV-REGISTRO.                                                
           05  PVV-BANCO               PIC  9(03)       COMP-3.         
           05  PVV-AGENCIA             PIC  9(05)       COMP-3.         
           05  PVV-CONTA               PIC  9(13)       COMP-3.         
           05  PVV-CARTEIRA            PIC  9(05)       COMP-3.         
           05  PVV-CARTEIRA-R  REDEFINES  PVV-CARTEIRA  PIC  X(03).     
           05  PVV-CONTRATO            PIC  9(17)       COMP-3.         
           05  PVV-CGCNUM              PIC  9(09)       COMP-3.         
           05  PVV-CGCFIL              PIC  9(05)       COMP-3.         
           05  PVV-CGCCTR              PIC  9(02).                      
           05  PVV-TPPESSOA            PIC  X(01).                      
           05  PVV-CPSSOA              PIC  X(26).                      
           05  PVV-NOME-PESSOA.                                         
               07  PVV-CPSSOA-CADTR    PIC  X(26).                      
               07  PVV-CPSSOA-CDCLI    PIC  X(26).                      
           05  PVV-CPSSOA-LIGADA       PIC  X(26).                      
           05  PVV-TPCONTA             PIC  9(02).                      
           05  PVV-CPOSTO-SERVC        PIC  9(03)       COMP-3.         
           05  PVV-CCLUB               PIC  9(10)       COMP-3.         
           05  PVV-FILLER              PIC  X(02).                      
      ***************************************************************** 
      * PVV-REGISTRO                      1   150  A                    
      * PVV-BANCO                         1     2  P                    
      * PVV-AGENCIA                       3     3  P                    
      * PVV-CONTA                         6     7  P                    
      * PVV-CARTEIRA                     13     3  P                    
      * PVV-CARTEIRA-R                   13     3  A                    
      * PVV-CONTRATO                     16     9  P                    
      * PVV-CGCNUM                       25     5  P                    
      * PVV-CGCFIL                       30     3  P                    
      * PVV-CGCCTR                       33     2  N                    
      * PVV-TPPESSOA                     35     1  A                    
      * PVV-CPSSOA                       36    26  A                    
      * PVV-CPSSOA-CADTR                 62    26  A                    
      * PVV-CPSSOA-CDCLI                 88    26  A                    
      * PVV-CPSSOA-LIGADA                88    26  A                    
      * PVV-CPSSOA-LIGADA               114    26  A                    
      * PVV-TPCONTA                     140     2  N                    
      * PVV-CPOSTO-SERVC                142     2  N                    
      * PVV-CCLUB                       144     6  N                    
      * PVV-FILLER                      150     1  A                    

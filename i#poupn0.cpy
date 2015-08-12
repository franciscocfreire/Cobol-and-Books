      ***************************************************************** 
      *                     MOVIMENTO DO SUBCENTRO                    * 
      *                                                               * 
      *   -INC = I#POUPN0   -   LRECL = 0140   -   RECFM = FB         * 
      *===============================================================* 
      *   INC ORIGINAL:                                               * 
      *   -INC = I#POUPAW   -   LRECL = 0080   -   RECFM = FB         * 
      ***************************************************************** 
      *                                                                 
       01  MOVSUBC.                                                     
           05  MVSB-CJUNC-DEPDC               PIC  9(5).                
           05  MVSB-RAZAO                     PIC  9(5).                
           05  MVSB-CONTA-POUP.                                         
               10  MVSB-CCTA-POUP             PIC  9(7).                
               10  MVSB-CDIG-CCTA             PIC  X(1).                
           05  MVSB-CDEB-CRED                 PIC  9.                   
               88  MVSB-CDEB                  VALUE IS 1.               
               88  MVSB-CRED                  VALUE IS 2.               
           05  MVSB-HISTOR-LCTO               PIC  9(3).                
           05  MVSB-CDOCTO-LCTO               PIC  9(15).               
           05  MVSB-VMOVTO                    PIC S9(13)V99.            
           05  MVSB-DLCTO-MOVTO               PIC  9(8).                
           05  MVSB-DMOVTO-REALZ              PIC  9(8).                
           05  MVSB-CSUB-CTRO                 PIC  9(5).                
           05  MVSB-CPOSTO-SERVC              PIC  9(3).                
           05  MVSB-CMODLD-POUP               PIC  9.                   
           05  MVSB-DDIA-RETDA                PIC  99.                  
           05  FILLER                         PIC  X(4).                
           05  MVSB-CODLCTO-5POS              PIC  9(5).                
           05  MVSB-HISTOR-COMPL              PIC  X(32).               
           05  FILLER                         PIC  X(20).               

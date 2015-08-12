       01  REG-ARQECT.                                                  
           03  ECT-NUM-LOTE            PIC 9(05).                       
           03  ECT-CEP                 PIC 9(08).                       
           03  FILLER   REDEFINES  ECT-CEP.                             
               05  ECT-NUM-CEP         PIC 9(05).                       
               05  ECT-SUF-CEP         PIC 9(03).                       
           03  ECT-TIPO-EXTRATO        PIC X(01).                       
           03  ECT-NUM-EXTRATO         PIC 9(11).                       
           03  ECT-JOBNAME             PIC X(08).                       
           03  ECT-DT-FRANQ            PIC 9(08).                       
           03  ECT-CONT-FAC            PIC X(01).                       
           03  FILLER                  PIC X(04).                       

      *---------------------------------------------------------------* 
      *    ARQUIVO PARA EMISSAO DE CARTAS PARA AVALISTA               * 
      *             *** FORMATO DAS DATAS = AAAAMMDD ***              * 
      *    INC I#CLLPLC   LRECL  0385                                 * 
      *    INCLUSAO DOS CAMPOS LEI DA TRANSPARENCIA 13-0358           * 
      *---------------------------------------------------------------* 
                                                                        
       01  REG-CADLPCL.                                                 
           03  CHAVE-CADLPCL.                                           
               05  CHAVE-CADLPCL-NAT.                                   
                   07  CGCCPF-NUM-CADLPCL   PIC 9(09)   COMP-3.         
                   07  CGCCPF-FIL-CADLPCL   PIC 9(05)   COMP-3.         
                   07  CGCCPF-CTR-CADLPCL   PIC 9(03)   COMP-3.         
                   07  NATUREZA-CADLPCL     PIC X(02).                  
               05  DAT-VENCTO-CADLPCL       PIC 9(09)    COMP-3.        
           03  VAL-RESG-CADLPCL             PIC 9(11)V99 COMP-3.        
           03  VAL-ENC-VEN-CADLPCL          PIC 9(11)V99 COMP-3.        
           03  VAL-ENC-VIN-CADLPCL          PIC 9(11)V99 COMP-3.        
           03  CAMPOS-CADLPCL.                                          
               05  NOME-CAD-CADLPCL.                                    
                   07  NOME-CAD-39-CADLPCL  PIC X(39).                  
                   07  FILLER               PIC X(01).                  
               05  CAD-CGC-CADLPCL.                                     
                   07  NUM-CGC-CAD-CADLPCL  PIC 9(09)   COMP-3.         
                   07  FIL-CGC-CAD-CADLPCL  PIC 9(05)   COMP-3.         
                   07  CONTROLE-CAD-CADLPCL PIC 9(03)   COMP-3.         
               05  CAD-NOME1-CADLPCL        PIC X(40).                  
               05  CAD-CGC1-CADLPCL.                                    
                   07  CAD-NCGC1-CADLPCL    PIC 9(09)   COMP-3.         
                   07  CAD-FIL1-CADLPCL     PIC 9(05)   COMP-3.         
                   07  CAD-CTR1-CADLPCL     PIC 9(03)   COMP-3.         
               05  CAD-NOME2-CADLPCL        PIC X(40).                  
               05  CAD-CGC2-CADLPCL.                                    
                   07  CAD-NCGC2-CADLPCL    PIC 9(09)   COMP-3.         
                   07  CAD-FIL2-CADLPCL     PIC 9(05)   COMP-3.         
                   07  CAD-CTR2-CADLPCL     PIC 9(03)   COMP-3.         
           03  NOME-3-AVAL-CADLPCL          PIC X(30).                  
BRQ=E******03  CARTEIRA-CADLPCL             PIC 9(03)   COMP-3.         
BRQ=I      03  CARTEIRA-CADLPCL             PIC X(03).                  
           03  CGC-CPF-CADLPCL.                                         
               05  CGC-NRO-CADLPCL          PIC 9(09)   COMP-3.         
               05  CGC-FIL-CADLPCL          PIC 9(05)   COMP-3.         
               05  CGC-CTR-CADLPCL          PIC 9(03)   COMP-3.         
           03  EMPRESA-CADLPCL              PIC 9(05)   COMP-3.         
           03  MOEDA-CADLPCL                PIC X(02).                  
           03  COD-NATUREZA-CADLPCL         PIC 9(03).                  
           03  CAD-IDENT-CADLPCL            PIC X(02).                  
           03  AGENCIA-CADLPCL              PIC 9(05)   COMP-3.         
           03  CONTA-CADLPCL                PIC 9(07)   COMP-3.         
           03  CONTRATO-CADLPCL             PIC 9(07)   COMP-3.         
           03  CONTR-MULTIPLO-CADLPCL       PIC X(01).                  
BRQ=E******03  FILLER                       PIC X(33).                  
BRQ=I      03  FILLER                       PIC X(32).                  
           03  CADLPCL-LT.                                              
              05   CADLPCL-TAXA-CONTRATO    PIC 9(02)V9(06)  COMP-3.    
              05   CADLPCL-VR-REMUNERATORIO PIC S9(13)V99  COMP-3.      
              05   CADLPCL-VALOR-MORATORIO  PIC S9(13)V99  COMP-3.      
              05   CADLPCL-VALOR-MULTA      PIC S9(13)V99  COMP-3.      
              05   CADLPCL-DESP-JUD-CUSTAS  PIC S9(11)V99  COMP-3.      
              05   CADLPCL-HONORARIOS       PIC S9(11)V99  COMP-3.      
              05   CADLPCL-VL-TOTAL-DIVIDA  PIC S9(15)V99  COMP-3.      
              05   CADLPCL-VL-TAXA-TARIFA   PIC S9(15)V99  COMP-3.      
              05   FILLER                   PIC  X(39).                 

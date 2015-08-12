      ***************************************************************** 
      *  - I#RDAB39 ARQUIVO DE PARCELA (ISD)                          * 
      *  - LRECL=247   DT.CRIACAO : 12/2003 POR: MARCELO NARDAO - CPM * 
      ***************************************************************** 
      *  - LRECL=368   DT.ALTERACAO: 08/2014 POR: ROBSON        - BRQ358
      ***************************************************************** 
       01  RDAB-REGISTRO-PB.                                            
           05  RDAB-BANCO-PB           PIC  9(03).                      
           05  RDAB-PV-1-PB            PIC  X(01).                      
           05  RDAB-AGENCIA-PB         PIC  9(05).                      
           05  RDAB-PV-2-PB            PIC  X(01).                      
           05  RDAB-EMPRESA-PB         PIC  9(05).                      
           05  RDAB-PV-3-PB            PIC  X(01).                      
           05  RDAB-NUMERO-CL-PB       PIC  9(15).                      
           05  RDAB-PV-4-PB            PIC  X(01).                      
           05  RDAB-DIGITO-PB          PIC  X(01).                      
           05  RDAB-PV-5-PB            PIC  X(01).                      
           05  RDAB-NUMERO-CC-PB       PIC  9(13).                      
           05  RDAB-PV-6-PB            PIC  X(01).                      
           05  RDAB-CC-DIGITO-PB       PIC  X(02).                      
           05  RDAB-PV-7-PB            PIC  X(01).                      
           05  RDAB-CARTEIRA-PB        PIC  X(03).                      
           05  RDAB-PV-8-PB            PIC  X(01).                      
           05  RDAB-CONTRATO-PB        PIC  9(17).                      
           05  RDAB-PV-9-PB            PIC  X(01).                      
           05  RDAB-VCTO-PB            PIC  X(10).                      
           05  RDAB-PV-10-PB           PIC  X(01).                      
           05  RDAB-ID-PB              PIC  X(02).                      
           05  RDAB-PV-11-PB           PIC  X(01).                      
           05  RDAB-ADVCPF-NUM-PB      PIC  9(09).                      
           05  RDAB-PV-12-PB           PIC  X(01).                      
           05  RDAB-ADVCPF-CTR-PB      PIC  9(02).                      
           05  RDAB-PV-13-PB           PIC  X(01).                      
           05  RDAB-ULTOCOR-COD-PB     PIC  9(03).                      
           05  RDAB-PV-14-PB           PIC  X(01).                      
           05  RDAB-VR-PRINCIP-PB      PIC  9(15)V9(02).                
           05  RDAB-PV-15-PB           PIC  X(01).                      
           05  RDAB-VR-LIQUIDO-PB      PIC  9(15)V9(02).                
           05  RDAB-PV-16-PB           PIC  X(01).                      
           05  RDAB-VR-COBRANCA-PB     PIC  9(15)V9(02).                
           05  RDAB-PV-17-PB           PIC  X(01).                      
           05  RDAB-COD-NAT-OPER-PB    PIC  X(03).                      
           05  RDAB-PV-18-PB           PIC  X(01).                      
           05  RDAB-NOME-PB            PIC  X(40).                      
           05  RDAB-PV-19-PB           PIC  X(01).                      
           05  RDAB-COD-EMPRESA-PB     PIC  X(02).                      
           05  RDAB-PV-20-PB           PIC  X(01).                      
           05  RDAB-CPRODT-BDSCO-PB    PIC  9(03).                      
           05  RDAB-PV-21-PB           PIC  X(01).                      
           05  RDAB-CFAML-CONTR-PB     PIC  9(01).                      
           05  RDAB-PV-22-PB           PIC  X(01).                      
           05  RDAB-VR-IOF-COMPL-PB    PIC  9(15)V9(02).                
           05  RDAB-PV-23-PB           PIC  X(01).                      
           05  RDAB-VR-CONTABIL-PB     PIC  9(15)V9(02).                
           05  RDAB-PV-24-PB           PIC  X(01).                      
           05  RDAB-TAXA-CONTRATO-PB    PIC  9(02)V9(06).               
           05  RDAB-PV-25-PB            PIC  X(01).                     
           05  RDAB-VR-REMUNERATORIO-PB PIC S9(13)V99.                  
           05  RDAB-PV-26-PB            PIC  X(01).                     
           05  RDAB-VALOR-MORATORIO-PB  PIC S9(13)V99.                  
           05  RDAB-PV-27-PB            PIC  X(01).                     
           05  RDAB-VALOR-MULTA-PB      PIC S9(13)V99.                  
           05  RDAB-PV-28-PB            PIC  X(01).                     
           05  RDAB-DESP-JUD-CUSTAS-PB  PIC S9(11)V99.                  
           05  RDAB-PV-29-PB            PIC  X(01).                     
           05  RDAB-HONORARIOS-PB       PIC S9(11)V99.                  
           05  RDAB-PV-30-PB            PIC  X(01).                     
           05  RDAB-VL-TOTAL-DIVIDA-PB  PIC S9(15)V99.                  
           05  RDAB-PV-31-PB            PIC  X(01).                     
           05  RDAB-VL-TAXA-TARIFA-PB   PIC S9(15)V99.                  

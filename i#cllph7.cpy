      ***************************************************************** 
      * NOME BOOK : I#CLLPH7                                          * 
      * DESCRICAO : LAYOUT PARA O PROGRAMA CLLP0162 ARQUIVO EXPANDIDO * 
      *             LEI DA TRANSPARENCIA                              * 
      *                                                               * 
      * DATA      : 05/05/2014                                        * 
      * AUTOR     : ADRIANO DE SA RANEA                               * 
      * EMPRESA   : BRQ                                               * 
      * TAMANHO   : 414                                               * 
      ***************************************************************** 
       01  REG-ENTRADA2.                                                
           05  ENT2-CHAVE.                                              
               10 ENT2-EMPRESA           PIC  9(05) COMP-3.             
               10 ENT2-AGENCIA           PIC  9(05) COMP-3.             
               10 ENT2-NUMCL             PIC  9(15) COMP-3.             
           05  ENT2-DIGITO               PIC  X(01).                    
           05  ENT2-DATA-BAIXA           PIC  X(10).                    
           05  ENT2-CONTA-CORR.                                         
               10 ENT2-NUM-CC            PIC  9(07) COMP-3.             
               10 ENT2-DIG-CC            PIC  X(01).                    
           05  ENT2-CARTEIRA             PIC  X(03).                    
           05  ENT2-CONTRATO             PIC  9(07) COMP-3.             
           05  ENT2-ID                   PIC  X(02).                    
           05  ENT2-TIPO-BAIXA           PIC  X(01).                    
           05  ENT2-DEVEDOR.                                            
               10 ENT2-NOME              PIC  X(40).                    
               10 ENT2-CGC-CPF.                                         
                  15 ENT2-NUM-CGC-CPF    PIC 9(09) COMP-3.              
                  15 ENT2-FIL-CGC-CPF    PIC 9(05) COMP-3.              
                  15 ENT2-CTR-CGC-CPF    PIC 9(02).                     
           05  ENT2-DATA-VENCTO          PIC  X(10).                    
           05  ENT2-DATA-CL              PIC  X(10).                    
           05  ENT2-DATA-LP              PIC  X(10).                    
           05  ENT2-VR-CONTABIL          PIC  9(13)V9(02) COMP-3.       
           05  ENT2-VR-DEV-INICIAL       PIC  9(13)V9(02) COMP-3.       
           05  ENT2-VR-BASE              PIC  9(13)V9(02) COMP-3.       
           05  ENT2-ENCARGO-VENCIDO      PIC  9(13)V9(02) COMP-3.       
           05  ENT2-ENCARGO-VINCENDO     PIC 9(13)V9(02) COMP-3.        
           05  ENT2-DEBITO-CC            PIC  9(13)V9(02) COMP-3.       
           05  ENT2-PRINCIPAL            PIC  9(13)V9(02) COMP-3.       
           05  ENT2-JUROS-MORA           PIC  9(13)V9(02) COMP-3.       
           05  ENT2-CORR-MONETARIA       PIC  9(13)V9(02) COMP-3.       
           05  ENT2-JUROS-12PAA          PIC  9(13)V9(02) COMP-3.       
           05  ENT2-COBRANCA-DIARIO      PIC  9(13)V9(02) COMP-3.       
           05  ENT2-TOTAL-CONTRATO       PIC  9(13)V9(02) COMP-3.       
           05  ENT2-COD-EMPRESA          PIC  X(02).                    
           05  ENT2-COD-DIR-REGIONAL     PIC 9(05) COMP-3.              
           05  ENT2-DT-CLPDD-180         PIC  X(10).                    
           05  ENT2-MARCA-PDD-180        PIC  X(01).                    
           05  ENT2-RAZAO-PRINCIP        PIC  9(05) COMP-3.             
           05  ENT2-RAZAO-RENDAS         PIC  9(05) COMP-3.             
           05  ENT2-COD-NAT-OPER         PIC  X(03).                    
           05  ENT2-MOEDA                PIC  X(02).                    
           05  ENT2-TIPO-GARANTIA        PIC  X(02).                    
           05  ENT2-LOCAL                PIC  X(02).                    
           05  ENT2-TIPO-PENDENCI        PIC  9(05) COMP-3.             
           05  ENT2-MARCA-IMPE           PIC  X(01).                    
           05  ENT2-DATA-AJUIZAMENTO     PIC X(10).                     
           05  ENT2-IOF-NORMAL           PIC  9(13)V9(02) COMP-3.       
           05  ENT2-IOF-COMPL            PIC  9(13)V9(02) COMP-3.       
           05  ENT2-ALIQ-CONT-RECOL      PIC  9(03)V9(06) COMP-3.       
           05  ENT2-ALIQ-REC-TR-CL       PIC  9(03)V9(06) COMP-3.       
           05  ENT2-ALIQ-COMPL           PIC  9(03)V9(06) COMP-3.       
           05  ENT2-IOF-OPCAO            PIC  X(01).                    
           05  ENT2-VR-BASE-IOF          PIC  9(13)V9(02) COMP-3.       
           05  ENT2-DATA-SITEL           PIC  X(10).                    
           05  ENT2-IND-SISTEL           PIC  X(01).                    
           05  ENT2-NOTIF-SISTEL         PIC  X(01).                    
           05  ENT2-COMPL-TPO-BAIXA      PIC  X(01).                    
      * NOVOS CAMPOS DA LEI DA TRANSPARENCIA 100 BYTES                  
           05  ENT2-MOV-LT.                                             
               10 TAXA-CONTRATO-N      PIC  9(02)V9(06) COMP-3.         
               10 VR-REMUNERATORIO-N   PIC S9(13)V99 COMP-3.            
               10 VALOR-MORATORIO-N    PIC S9(13)V99 COMP-3.            
               10 VALOR-MULTA-N        PIC S9(13)V99 COMP-3.            
               10 DESP-JUD-CUSTAS-N    PIC S9(11)V99 COMP-3.            
               10 HONORARIOS-N         PIC S9(11)V99 COMP-3.            
               10 VL-TOTAL-DIVIDA-N    PIC S9(15)V99 COMP-3.            
               10 VL-TAXA-TARIFA-N     PIC S9(15)V99 COMP-3.            
               10 FILLER               PIC  X(39).                      

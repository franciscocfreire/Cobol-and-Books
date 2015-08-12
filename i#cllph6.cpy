      ***************************************************************** 
      * NOME BOOK : I#CLLPH6                                          * 
      * DESCRICAO : LAYOUT PARA O PROGRAMA CLLP0162 ARQUIVO ROTINA    * 
      *             NORMAL                                            * 
      *                                                               * 
      * DATA      : 05/05/2014                                        * 
      * AUTOR     : ADRIANO DE SA RANEA                               * 
      * EMPRESA   : BRQ                                               * 
      * TAMANHO   : 314                                               * 
      ***************************************************************** 
       01  REG-ENTRADA1.                                                
           05  ENT1-CHAVE.                                              
               10 ENT1-EMPRESA           PIC  9(05) COMP-3.             
               10 ENT1-AGENCIA           PIC  9(05) COMP-3.             
               10 ENT1-NUMCL             PIC  9(15) COMP-3.             
           05  ENT1-DIGITO               PIC  X(01).                    
           05  ENT1-DATA-BAIXA           PIC  X(10).                    
           05  ENT1-CONTA-CORR.                                         
               10 ENT1-NUM-CC            PIC  9(07) COMP-3.             
               10 ENT1-DIG-CC            PIC  X(01).                    
           05  ENT1-CARTEIRA             PIC  X(03).                    
           05  ENT1-CONTRATO             PIC  9(07) COMP-3.             
           05  ENT1-ID                   PIC  X(02).                    
           05  ENT1-TIPO-BAIXA           PIC  X(01).                    
           05  ENT1-DEVEDOR.                                            
               10 ENT1-NOME              PIC  X(40).                    
               10 ENT1-CGC-CPF.                                         
                  15 ENT1-NUM-CGC-CPF    PIC 9(09) COMP-3.              
                  15 ENT1-FIL-CGC-CPF    PIC 9(05) COMP-3.              
                  15 ENT1-CTR-CGC-CPF    PIC 9(02).                     
           05  ENT1-DATA-VENCTO          PIC  X(10).                    
           05  ENT1-DATA-CL              PIC  X(10).                    
           05  ENT1-DATA-LP              PIC  X(10).                    
           05  ENT1-VR-CONTABIL          PIC  9(13)V9(02) COMP-3.       
           05  ENT1-VR-DEV-INICIAL       PIC  9(13)V9(02) COMP-3.       
           05  ENT1-VR-BASE              PIC  9(13)V9(02) COMP-3.       
           05  ENT1-ENCARGO-VENCIDO      PIC  9(13)V9(02) COMP-3.       
           05  ENT1-ENCARGO-VINCENDO     PIC 9(13)V9(02) COMP-3.        
           05  ENT1-DEBITO-CC            PIC  9(13)V9(02) COMP-3.       
           05  ENT1-PRINCIPAL            PIC  9(13)V9(02) COMP-3.       
           05  ENT1-JUROS-MORA           PIC  9(13)V9(02) COMP-3.       
           05  ENT1-CORR-MONETARIA       PIC  9(13)V9(02) COMP-3.       
           05  ENT1-JUROS-12PAA          PIC  9(13)V9(02) COMP-3.       
           05  ENT1-COBRANCA-DIARIO      PIC  9(13)V9(02) COMP-3.       
           05  ENT1-TOTAL-CONTRATO       PIC  9(13)V9(02) COMP-3.       
           05  ENT1-COD-EMPRESA          PIC  X(02).                    
           05  ENT1-COD-DIR-REGIONAL     PIC 9(05) COMP-3.              
           05  ENT1-DT-CLPDD-180         PIC  X(10).                    
           05  ENT1-MARCA-PDD-180        PIC  X(01).                    
           05  ENT1-RAZAO-PRINCIP        PIC  9(05) COMP-3.             
           05  ENT1-RAZAO-RENDAS         PIC  9(05) COMP-3.             
           05  ENT1-COD-NAT-OPER         PIC  X(03).                    
           05  ENT1-MOEDA                PIC  X(02).                    
           05  ENT1-TIPO-GARANTIA        PIC  X(02).                    
           05  ENT1-LOCAL                PIC  X(02).                    
           05  ENT1-TIPO-PENDENCI        PIC  9(05) COMP-3.             
           05  ENT1-MARCA-IMPE           PIC  X(01).                    
           05  ENT1-DATA-AJUIZAMENTO     PIC X(10).                     
           05  ENT1-IOF-NORMAL           PIC  9(13)V9(02) COMP-3.       
           05  ENT1-IOF-COMPL            PIC  9(13)V9(02) COMP-3.       
           05  ENT1-ALIQ-CONT-RECOL      PIC  9(03)V9(06) COMP-3.       
           05  ENT1-ALIQ-REC-TR-CL       PIC  9(03)V9(06) COMP-3.       
           05  ENT1-ALIQ-COMPL           PIC  9(03)V9(06) COMP-3.       
           05  ENT1-IOF-OPCAO            PIC  X(01).                    
           05  ENT1-VR-BASE-IOF          PIC  9(13)V9(02) COMP-3.       
           05  ENT1-DATA-SITEL           PIC  X(10).                    
           05  ENT1-IND-SISTEL           PIC  X(01).                    
           05  ENT1-NOTIF-SISTEL         PIC  X(01).                    
           05  ENT1-COMPL-TPO-BAIXA      PIC  X(01).                    

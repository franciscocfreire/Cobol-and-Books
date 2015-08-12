      ***************************************************************** 
      * NOME BOOK : I#CLLPI0                                          * 
      * DESCRICAO : LAYOUT PARA O PROGRAMA CLLP0162 ARQUIVO BALANCE   * 
      *             LINE LEI DA TRANSPARENCIA                         * 
      *                                                               * 
      * DATA      : 05/05/2014                                        * 
      * AUTOR     : ADRIANO DE SA RANEA                               * 
      * EMPRESA   : BRQ                                               * 
      * TAMANHO   : 414                                               * 
      ***************************************************************** 
       01  REG-SAIDA.                                                   
           05  SAI-CHAVE.                                               
               10 SAI-EMPRESA            PIC  9(05) COMP-3.             
               10 SAI-AGENCIA            PIC  9(05) COMP-3.             
               10 SAI-NUMCL              PIC  9(15) COMP-3.             
           05  SAI-DIGITO                PIC  X(01).                    
           05  SAI-DATA-BAIXA            PIC  X(10).                    
           05  SAI-CONTA-CORR.                                          
               10 SAI-NUM-CC             PIC  9(07) COMP-3.             
               10 SAI-DIG-CC             PIC  X(01).                    
           05  SAI-CARTEIRA              PIC  X(03).                    
           05  SAI-CONTRATO              PIC  9(07) COMP-3.             
           05  SAI-ID                    PIC  X(02).                    
           05  SAI-TIPO-BAIXA            PIC  X(01).                    
           05  SAI-DEVEDOR.                                             
               10 SAI-NOME               PIC  X(40).                    
               10 SAI-CGC-CPF.                                          
                  15 SAI-NUM-CGC-CPF     PIC 9(09) COMP-3.              
                  15 SAI-FIL-CGC-CPF     PIC 9(05) COMP-3.              
                  15 SAI-CTR-CGC-CPF     PIC 9(02).                     
           05  SAI-DATA-VENCTO           PIC  X(10).                    
           05  SAI-DATA-CL               PIC  X(10).                    
           05  SAI-DATA-LP               PIC  X(10).                    
           05  SAI-VR-CONTABIL           PIC  9(13)V9(02) COMP-3.       
           05  SAI-VR-DEV-INICIAL        PIC  9(13)V9(02) COMP-3.       
           05  SAI-VR-BASE               PIC  9(13)V9(02) COMP-3.       
           05  SAI-ENCARGO-VENCIDO       PIC  9(13)V9(02) COMP-3.       
           05  SAI-ENCARGO-VINCENDO      PIC 9(13)V9(02) COMP-3.        
           05  SAI-DEBITO-CC             PIC  9(13)V9(02) COMP-3.       
           05  SAI-PRINCIPAL             PIC  9(13)V9(02) COMP-3.       
           05  SAI-JUROS-MORA            PIC  9(13)V9(02) COMP-3.       
           05  SAI-CORR-MONETARIA        PIC  9(13)V9(02) COMP-3.       
           05  SAI-JUROS-12PAA           PIC  9(13)V9(02) COMP-3.       
           05  SAI-COBRANCA-DIARIO       PIC  9(13)V9(02) COMP-3.       
           05  SAI-TOTAL-CONTRATO        PIC  9(13)V9(02) COMP-3.       
           05  SAI-COD-EMPRESA           PIC  X(02).                    
           05  SAI-COD-DIR-REGIONAL      PIC 9(05) COMP-3.              
           05  SAI-DT-CLPDD-180          PIC  X(10).                    
           05  SAI-MARCA-PDD-180         PIC  X(01).                    
           05  SAI-RAZAO-PRINCIP         PIC  9(05) COMP-3.             
           05  SAI-RAZAO-RENDAS          PIC  9(05) COMP-3.             
           05  SAI-COD-NAT-OPER          PIC  X(03).                    
           05  SAI-MOEDA                 PIC  X(02).                    
           05  SAI-TIPO-GARANTIA         PIC  X(02).                    
           05  SAI-LOCAL                 PIC  X(02).                    
           05  SAI-TIPO-PENDENCI         PIC  9(05) COMP-3.             
           05  SAI-MARCA-IMPE            PIC  X(01).                    
           05  SAI-DATA-AJUIZAMENTO      PIC X(10).                     
           05  SAI-IOF-NORMAL            PIC  9(13)V9(02) COMP-3.       
           05  SAI-IOF-COMPL             PIC  9(13)V9(02) COMP-3.       
           05  SAI-ALIQ-CONT-RECOL       PIC  9(03)V9(06) COMP-3.       
           05  SAI-ALIQ-REC-TR-CL        PIC  9(03)V9(06) COMP-3.       
           05  SAI-ALIQ-COMPL            PIC  9(03)V9(06) COMP-3.       
           05  SAI-IOF-OPCAO             PIC  X(01).                    
           05  SAI-VR-BASE-IOF           PIC  9(13)V9(02) COMP-3.       
           05  SAI-DATA-SITEL            PIC  X(10).                    
           05  SAI-IND-SISTEL            PIC  X(01).                    
           05  SAI-NOTIF-SISTEL          PIC  X(01).                    
           05  SAI-COMPL-TPO-BAIXA       PIC  X(01).                    
      * NOVOS CAMPOS DA LEI DA TRANSPARENCIA 100 BYTES                  
           05  SAI-MOV-LT.                                              
               10 TAXA-CONTRATO-N      PIC  9(02)V9(06) COMP-3.         
               10 VR-REMUNERATORIO-N   PIC S9(13)V99 COMP-3.            
               10 VALOR-MORATORIO-N    PIC S9(13)V99 COMP-3.            
               10 VALOR-MULTA-N        PIC S9(13)V99 COMP-3.            
               10 DESP-JUD-CUSTAS-N    PIC S9(11)V99 COMP-3.            
               10 HONORARIOS-N         PIC S9(11)V99 COMP-3.            
               10 VL-TOTAL-DIVIDA-N    PIC S9(15)V99 COMP-3.            
               10 VL-TAXA-TARIFA-N     PIC S9(15)V99 COMP-3.            
               10 FILLER               PIC  X(39).                      

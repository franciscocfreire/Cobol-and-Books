      ***************************************************************** 
      *  - I#CLLP23       ARQUIVO RECEBIDO DO FBNH        LRECL= 600  * 
      *  CLONE DA INC I#CLLPFB - TRATAMENTO DE CARTEIRA COM ALFA      * 
      ***************************************************************** 
                                                                        
       01  MOV-REGISTRO.                                                
           05  MOV-IDENT               PIC  9(02).                      
      *        IDENTIFICACAO DO SISTEMA  -  FIXO = 13 (FBNH)            
           05  MOV-AGENCIA             PIC  9(04).                      
           05  MOV-CONTA               PIC  9(07).                      
           05  MOV-SIGLA               PIC  X(04).                      
           05  MOV-CARTEIRA            PIC  X(03).                      
           05  MOV-CONTRATO            PIC  9(07).                      
           05  MOV-DT-VECTO            PIC  9(09)    COMP-3.            
      *        DATA DE VENCIMENTO          -  DDMMAAAA                  
           05  MOV-DT-INIC             PIC  9(09)    COMP-3.            
      *        DATA DE INICIO DA OPERACAO  -  DDMMAAAA                  
           05  MOV-DT-MOVTO            PIC  9(09)    COMP-3.            
      *        DATA DE MOVIMENTO           -  DDMMAAAA                  
           05  MOV-MOEDA               PIC  X(02).                      
           05  MOV-GARANTIA            PIC  9(02).                      
           05  MOV-MARCA               PIC  X(01).                      
           05  MOV-MOTIVO              PIC  9(02).                      
           05  MOV-DUPL-PROM           PIC  X(30).                      
           05  MOV-VRTIT               PIC  9(13)V99.                   
      *        VALOR TITULO (DEVEDOR INICIAL)                           
           05  MOV-VREVE               PIC  9(13)V99.                   
      *        VALOR ENCARGOS VENCIDOS                                  
           05  MOV-VREDC               PIC  9(13)V99.                   
      *        VALOR ENCARGOS (DESCONTADO EM CONTA CORRENTE)            
           05  MOV-VRETM               PIC  9(13)V99.                   
      *        VALOR ENCARGOS TAXA DE MERCADO                           
           05  MOV-VRIOF               PIC  9(13)V99.                   
      *        VALOR IOF                                                
           05  MOV-NOME                PIC  X(40).                      
      *        NOME DO DEVEDOR RESPONSAVEL                              
           05  MOV-COOBRIGADO1         PIC  X(40).                      
           05  MOV-COOBRIGADO2         PIC  X(40).                      
           05  MOV-CGCNUM              PIC  9(09).                      
      *        NUMERO CGC/CPF DO DEVEDOR RESPONSAVEL                    
           05  MOV-CGCFIL              PIC  9(04).                      
           05  MOV-CGCCTR              PIC  9(02).                      
           05  MOV-DT-VECTO-INV        PIC  9(09)    COMP-3.            
      *        DATA VENCIMENTO INVERTIDA   -  AAAAMMDD                  
           05  MOV-FILLER              PIC  X(01).                      
           05  MOV-FIRMA               PIC  9(02).                      
           05  MOV-EMPRESA             PIC  9(05).                      
           05  MOV-NUMCL               PIC  9(15).                      
           05  FILLER  REDEFINES  MOV-NUMCL.                            
               10  FILLER              PIC  X(12).                      
               10  MOV-PARCELA         PIC  9(03).                      
           05  MOV-DIGCL               PIC  X(01).                      
           05  MOV-CPFNUM              PIC  9(09).                      
      *        NUMERO CPF DO ADVOGADO                                   
           05  MOV-CPFCTR              PIC  9(02).                      
           05  MOV-DT-INCL-CL          PIC  9(08).                      
      *        DATA DE INCLUSAO EM CL      -  DDMMAAAA                  
           05  MOV-VRCON               PIC  9(13)V99.                   
      *        VALOR CONTABIL                                           
           05  MOV-VRLIQ               PIC  9(13)V99.                   
      *        VALOR LIQUIDO                                            
           05  MOV-VRCOB               PIC  9(13)V99.                   
      *        VALOR COBRANCA                                           
           05  MOV-VRCOR               PIC  9(13)V99.                   
      *        VALOR CORRECAO MONETARIA                                 
           05  MOV-VRCOR-300694        PIC  9(13)V99.                   
      *        VALOR CORRECAO MONETARIA ATE 300694                      
           05  MOV-VRJRM               PIC  9(13)V99.                   
      *        VALOR JUROS DE MORA                                      
           05  MOV-VRJ12               PIC  9(13)V99.                   
      *        VALOR JUROS 12% AO ANO                                   
           05  MOV-MARCA-PDD           PIC  X(01).                      
           05  MOV-DT-WRITEOFF         PIC  9(08).                      
      *        DATA DE WRITEOFF            -  DDMMAAAA                  
           05  FILLER                  PIC  X(49).                      
           05  MOV-LT.                                                  
               07 MOV-TAXA-CONTRATO       PIC  9(02)V9(06) COMP-3.      
               07 MOV-VR-REMUNERATORIO PIC S9(13)V99 COMP-3.            
               07 MOV-VALOR-MORATORIO     PIC S9(13)V99 COMP-3.         
               07 MOV-VALOR-MULTA         PIC S9(13)V99 COMP-3.         
               07 MOV-DESP-JUD-CUSTAS     PIC S9(11)V99 COMP-3.         
               07 MOV-HONORARIOS          PIC S9(11)V99 COMP-3.         
               07 MOV-VL-TOTAL-DIVIDA     PIC S9(15)V99 COMP-3.         
               07 MOV-VL-TAXA-TARIFA      PIC S9(15)V99 COMP-3.         
               07 MOV-VL-IOF              PIC S9(13)V99 USAGE COMP-3.   
               07 MOV-VL-CORRECAO         PIC S9(13)V99 USAGE COMP-3.   
               07 MOV-VL-JUROS-12PCAA     PIC S9(13)V99 USAGE COMP-3.   
               07 MOV-PERIODICIDADE       PIC S9(02)    USAGE COMP-3.   
               07 FILLER                  PIC  X(13).                   

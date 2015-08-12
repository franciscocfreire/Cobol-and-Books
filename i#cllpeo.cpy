       01  CLLPEO-REG-CADATIVO.                                         
           03  CLLPEO-EMP              PIC  9(05)    COMP-3.            
           03  CLLPEO-AGEN             PIC  9(05)    COMP-3.            
           03  CLLPEO-NUMCL            PIC  9(15)    COMP-3.            
           03  CLLPEO-DIG              PIC  X(01).                      
           03  CLLPEO-CC               PIC  9(07)    COMP-3.            
           03  CLLPEO-PEND             PIC  9(05)    COMP-3.            
           03  CLLPEO-SIGLA            PIC  X(04).                      
           03  CLLPEO-CART             PIC  9(03)    COMP-3.            
           03  CLLPEO-CONTR            PIC  9(07)    COMP-3.            
           03  CLLPEO-CART2            PIC  X(03).                      
           03  CLLPEO-RZCL             PIC  9(05)    COMP-3.            
           03  CLLPEO-CTCL             PIC  9(07)    COMP-3.            
           03  CLLPEO-RZ1CL            PIC  9(05)    COMP-3.            
           03  CLLPEO-CT1CL            PIC  9(07)    COMP-3.            
           03  CLLPEO-VCTO             PIC  9(09)    COMP-3.            
           03  CLLPEO-DTECL            PIC  9(09)    COMP-3.            
           03  CLLPEO-DTBCL            PIC  9(09)    COMP-3.            
           03  CLLPEO-DTIOP            PIC  9(09)    COMP-3.            
           03  CLLPEO-DTPRO            PIC  9(09)    COMP-3.            
           03  CLLPEO-DTELP            PIC  9(09)    COMP-3.            
           03  CLLPEO-DTBLP            PIC  9(09)    COMP-3.            
           03  CLLPEO-DTTR             PIC  9(09)    COMP-3.            
           03  CLLPEO-MOVTO            PIC  9(09)    COMP-3.            
           03  CLLPEO-DTREX            PIC  9(09)    COMP-3.            
           03  CLLPEO-DTVTR            PIC  9(09)    COMP-3.            
           03  CLLPEO-VRDVI            PIC  9(13)V99 COMP-3.            
           03  CLLPEO-VRCON            PIC  9(13)V99 COMP-3.            
           03  CLLPEO-VRBXA            PIC  9(13)V99 COMP-3.            
           03  CLLPEO-VBASE            PIC  9(13)V99 COMP-3.            
           03  CLLPEO-VRBBX            PIC  9(13)V99 COMP-3.            
           03  CLLPEO-COBRM            PIC  9(13)V99 COMP-3.            
           03  CLLPEO-COBRB            PIC  9(13)V99 COMP-3.            
           03  CLLPEO-DEBCC            PIC  9(13)V99 COMP-3.            
           03  CLLPEO-COBRD            PIC  9(13)V99 COMP-3.            
           03  CLLPEO-VREVE            PIC  9(13)V99 COMP-3.            
           03  CLLPEO-VREVI            PIC  9(13)V99 COMP-3.            
           03  CLLPEO-VRINI            PIC  9(13)V99 COMP-3.            
           03  CLLPEO-BXINI            PIC  9(13)V99 COMP-3.            
           03  CLLPEO-VRIOF            PIC  9(13)V99 COMP-3.            
           03  CLLPEO-BXIOF            PIC  9(13)V99 COMP-3.            
           03  CLLPEO-ANTEX            PIC  9(13)V99 COMP-3.            
           03  CLLPEO-MOEDA            PIC  X(02).                      
           03  CLLPEO-MOEDA-ANT        PIC  X(01).                      
           03  FILLER                  PIC  X(01).                      
           03  CLLPEO-AGRESP           PIC  9(05)    COMP-3.            
           03  CLLPEO-NOME             PIC  X(40).                      
           03  CLLPEO-CGC.                                              
               05  CLLPEO-NCGC         PIC  9(09)    COMP-3.            
               05  CLLPEO-FIL          PIC  9(05)    COMP-3.            
               05  CLLPEO-CTR          PIC  9(03)    COMP-3.            
           03  CLLPEO-NOME1            PIC  X(40).                      
           03  CLLPEO-CGC1.                                             
               05  CLLPEO-NCGC1        PIC  9(09)    COMP-3.            
               05  CLLPEO-FIL1         PIC  9(05)    COMP-3.            
               05  CLLPEO-CTR1         PIC  9(03)    COMP-3.            
           03  CLLPEO-NOME2            PIC  X(40).                      
           03  CLLPEO-CGC2.                                             
               05  CLLPEO-NCGC2        PIC  9(09)    COMP-3.            
               05  CLLPEO-FIL2         PIC  9(05)    COMP-3.            
               05  CLLPEO-CTR2         PIC  9(03)    COMP-3.            
           03  CLLPEO-TPGAR            PIC  X(02).                      
           03  CLLPEO-NOGAR            PIC  X(32).                      
           03  CLLPEO-STARET           PIC  X.                          
           03  CLLPEO-TRAG             PIC  X(04).                      
           03  CLLPEO-TRAG-AGEN        PIC  X(04).                      
           03  CLLPEO-TRAG-DATA        PIC  9(09)    COMP-3.            
           03  CLLPEO-DATA-RETORNA     PIC  9(09)    COMP-3.            
           03  CLLPEO-DT-WRITE         PIC  9(09)    COMP-3.            
           03  CLLPEO-ENC-TRANSF       PIC  9(13)V99 COMP-3.            
           03  CLLPEO-COMPL-TPO-BAIXA PIC   X(01).                      
           03  CLLPEO-ADVOG            PIC  9(11)    COMP-3.            
           03  CLLPEO-LOCAL            PIC  XX.                         
           03  CLLPEO-LOCA2            PIC  X.                          
           03  CLLPEO-SUBSTAB          PIC  X(01).                      
           03  CLLPEO-CDULT            PIC  9(03)    COMP-3.            
           03  CLLPEO-DTULT            PIC  9(09)    COMP-3.            
           03  CLLPEO-OCORRENCIAS.                                      
               05  CLLPEO-OCORR        PIC  9(03)    COMP-3 OCCURS 50.  
           03  CLLPEO-MARCA            PIC  X.                          
           03  CLLPEO-IDCON            PIC  X.                          
           03  CLLPEO-CARTA            PIC  X.                          
           03  CLLPEO-TXPUN            PIC  X.                          
           03  CLLPEO-AJUP             PIC  X.                          
           03  CLLPEO-REGBX            PIC  X.                          
           03  CLLPEO-TPTRANLP         PIC  X(02).                      
           03  CLLPEO-NOVOREGI         PIC  X(01).                      
           03  CLLPEO-PRECL-SALVA      PIC  X.                          
           03  CLLPEO-FALCONC          PIC  X.                          
           03  CLLPEO-EMISSAO          PIC  X.                          
           03  CLLPEO-PRECL            PIC  X.                          
           03  CLLPEO-DTPRECL          PIC  9(09)    COMP-3.            
           03  CLLPEO-ORIGEMCL         PIC  99.                         
           03  CLLPEO-TPEXPUR          PIC  X.                          
           03  CLLPEO-NATUREZA         PIC  9(03)    COMP-3.            
           03  CLLPEO-IDENT            PIC  X(02).                      
           03  CLLPEO-TIPBX            PIC  X.                          
           03  CLLPEO-TIPO             PIC  9.                          
030305     03  CLLPEO-TULTCAL          PIC  9(009)        COMP-3.       
030305     03  CLLPEO-DTBASE           PIC  9(009)        COMP-3.       
030305     03  CLLPEO-VRJURMORA        PIC  9(013)V9(002) COMP-3.       
030305     03  CLLPEO-VRCORRE          PIC  9(013)V9(002) COMP-3.       
030305     03  CLLPEO-VRJUR12          PIC  9(013)V9(002) COMP-3.       
030305     03  CLLPEO-VRIOF-CONTREC    PIC  9(003)V9(006) COMP-3.       
030305     03  CLLPEO-VRIOF-RECOLH     PIC  9(003)V9(006) COMP-3.       
030305     03  CLLPEO-VRIOF-COMPL      PIC  9(003)V9(006) COMP-3.       
030305     03  CLLPEO-VRIOF-VALRECOLH PIC   9(015)V9(002) COMP-3.       
030305     03  CLLPEO-IDCALC-IOF       PIC  X(001).                     
030305     03  CLLPEO-VRIOF-VALBASE    PIC  9(015)V9(002) COMP-3.       
           03  CLLPEO-LT.                                               
               05 CLLPEO-TAXA-CONTRATO  PIC  9(02)V9(06) COMP-3.        
               05 CLLPEO-VR-REMUNERATORIO PIC S9(13)V99 COMP-3.         
               05 CLLPEO-VALOR-MORATORIO PIC S9(13)V99 COMP-3.          
               05 CLLPEO-VALOR-MULTA    PIC S9(13)V99 COMP-3.           
               05 CLLPEO-DESP-JUD-CUSTAS PIC S9(11)V99 COMP-3.          
               05 CLLPEO-HONORARIOS     PIC S9(11)V99 COMP-3.           
               05 CLLPEO-VL-TOTAL-DIVIDA PIC S9(15)V99 COMP-3.          
               05 CLLPEO-VL-TAXA-TARIFA PIC S9(15)V99 COMP-3.           
               05 FILLER                PIC  X(39).                     

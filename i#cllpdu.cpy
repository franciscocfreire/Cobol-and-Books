       01  CLLPDU-REG-CADATIVO.                                         
           03  CLLPDU-EMP              PIC  9(05)    COMP-3.            
           03  CLLPDU-AGEN             PIC  9(05)    COMP-3.            
           03  CLLPDU-NUMCL            PIC  9(15)    COMP-3.            
           03  CLLPDU-DIG              PIC  X(01).                      
           03  CLLPDU-CC               PIC  9(07)    COMP-3.            
           03  CLLPDU-PEND             PIC  9(05)    COMP-3.            
           03  CLLPDU-SIGLA            PIC  X(04).                      
           03  CLLPDU-CART             PIC  9(03)    COMP-3.            
           03  CLLPDU-CONTR            PIC  9(07)    COMP-3.            
           03  CLLPDU-CART2            PIC  X(03).                      
           03  CLLPDU-RZCL             PIC  9(05)    COMP-3.            
           03  CLLPDU-CTCL             PIC  9(07)    COMP-3.            
           03  CLLPDU-RZ1CL            PIC  9(05)    COMP-3.            
           03  CLLPDU-CT1CL            PIC  9(07)    COMP-3.            
           03  CLLPDU-VCTO             PIC  9(09)    COMP-3.            
           03  CLLPDU-DTECL            PIC  9(09)    COMP-3.            
           03  CLLPDU-DTBCL            PIC  9(09)    COMP-3.            
           03  CLLPDU-DTIOP            PIC  9(09)    COMP-3.            
           03  CLLPDU-DTPRO            PIC  9(09)    COMP-3.            
           03  CLLPDU-DTELP            PIC  9(09)    COMP-3.            
           03  CLLPDU-DTBLP            PIC  9(09)    COMP-3.            
           03  CLLPDU-DTTR             PIC  9(09)    COMP-3.            
           03  CLLPDU-MOVTO            PIC  9(09)    COMP-3.            
           03  CLLPDU-DTREX            PIC  9(09)    COMP-3.            
           03  CLLPDU-DTVTR            PIC  9(09)    COMP-3.            
           03  CLLPDU-VRDVI            PIC  9(13)V99 COMP-3.            
           03  CLLPDU-VRCON            PIC  9(13)V99 COMP-3.            
           03  CLLPDU-VRBXA            PIC  9(13)V99 COMP-3.            
           03  CLLPDU-VBASE            PIC  9(13)V99 COMP-3.            
           03  CLLPDU-VRBBX            PIC  9(13)V99 COMP-3.            
           03  CLLPDU-COBRM            PIC  9(13)V99 COMP-3.            
           03  CLLPDU-COBRB            PIC  9(13)V99 COMP-3.            
           03  CLLPDU-DEBCC            PIC  9(13)V99 COMP-3.            
           03  CLLPDU-COBRD            PIC  9(13)V99 COMP-3.            
           03  CLLPDU-VREVE            PIC  9(13)V99 COMP-3.            
           03  CLLPDU-VREVI            PIC  9(13)V99 COMP-3.            
           03  CLLPDU-VRINI            PIC  9(13)V99 COMP-3.            
           03  CLLPDU-BXINI            PIC  9(13)V99 COMP-3.            
           03  CLLPDU-VRIOF            PIC  9(13)V99 COMP-3.            
           03  CLLPDU-BXIOF            PIC  9(13)V99 COMP-3.            
           03  CLLPDU-ANTEX            PIC  9(13)V99 COMP-3.            
           03  CLLPDU-MOEDA            PIC  X(02).                      
           03  CLLPDU-MOEDA-ANT        PIC  X(01).                      
           03  FILLER                  PIC  X(01).                      
           03  CLLPDU-AGRESP           PIC  9(05)    COMP-3.            
           03  CLLPDU-NOME             PIC  X(40).                      
           03  CLLPDU-CGC.                                              
               05  CLLPDU-NCGC         PIC  9(09)    COMP-3.            
               05  CLLPDU-FIL          PIC  9(05)    COMP-3.            
               05  CLLPDU-CTR          PIC  9(03)    COMP-3.            
           03  CLLPDU-NOME1            PIC  X(40).                      
           03  CLLPDU-CGC1.                                             
               05  CLLPDU-NCGC1        PIC  9(09)    COMP-3.            
               05  CLLPDU-FIL1         PIC  9(05)    COMP-3.            
               05  CLLPDU-CTR1         PIC  9(03)    COMP-3.            
           03  CLLPDU-NOME2            PIC  X(40).                      
           03  CLLPDU-CGC2.                                             
               05  CLLPDU-NCGC2        PIC  9(09)    COMP-3.            
               05  CLLPDU-FIL2         PIC  9(05)    COMP-3.            
               05  CLLPDU-CTR2         PIC  9(03)    COMP-3.            
           03  CLLPDU-TPGAR            PIC  X(02).                      
           03  CLLPDU-NOGAR            PIC  X(32).                      
           03  CLLPDU-STARET           PIC  X.                          
           03  CLLPDU-TRAG             PIC  X(04).                      
           03  CLLPDU-TRAG-AGEN        PIC  X(04).                      
           03  CLLPDU-TRAG-DATA        PIC  9(09)    COMP-3.            
           03  CLLPDU-DATA-RETORNA     PIC  9(09)    COMP-3.            
           03  CLLPDU-DT-WRITE         PIC  9(09)    COMP-3.            
           03  CLLPDU-ENC-TRANSF       PIC  9(13)V99 COMP-3.            
           03  CLLPDU-COMPL-TPO-BAIXA  PIC  X(01).                      
           03  CLLPDU-ADVOG            PIC  9(11)    COMP-3.            
           03  CLLPDU-LOCAL            PIC  XX.                         
           03  CLLPDU-LOCA2            PIC  X.                          
           03  CLLPDU-SUBSTAB          PIC  X(01).                      
           03  CLLPDU-CDULT            PIC  9(03)    COMP-3.            
           03  CLLPDU-DTULT            PIC  9(09)    COMP-3.            
           03  CLLPDU-OCORRENCIAS.                                      
               05  CLLPDU-OCORR        PIC  9(03)    COMP-3 OCCURS 50.  
           03  CLLPDU-MARCA            PIC  X.                          
           03  CLLPDU-IDCON            PIC  X.                          
           03  CLLPDU-CARTA            PIC  X.                          
           03  CLLPDU-TXPUN            PIC  X.                          
           03  CLLPDU-AJUP             PIC  X.                          
           03  CLLPDU-REGBX            PIC  X.                          
           03  CLLPDU-TPTRANLP         PIC  X(02).                      
           03  CLLPDU-NOVOREGI         PIC  X(01).                      
           03  CLLPDU-PRECL-SALVA      PIC  X.                          
           03  CLLPDU-FALCONC          PIC  X.                          
           03  CLLPDU-EMISSAO          PIC  X.                          
           03  CLLPDU-PRECL            PIC  X.                          
           03  CLLPDU-DTPRECL          PIC  9(09)    COMP-3.            
           03  CLLPDU-ORIGEMCL         PIC  99.                         
           03  CLLPDU-TPEXPUR          PIC  X.                          
           03  CLLPDU-NATUREZA         PIC  9(03)    COMP-3.            
           03  CLLPDU-IDENT            PIC  X(02).                      
           03  CLLPDU-TIPBX            PIC  X.                          
           03  CLLPDU-TIPO             PIC  9.                          
030305     03  CLLPDU-TULTCAL          PIC  9(009)        COMP-3.       
030305     03  CLLPDU-DTBASE           PIC  9(009)        COMP-3.       
030305     03  CLLPDU-VRJURMORA        PIC  9(013)V9(002) COMP-3.       
030305     03  CLLPDU-VRCORRE          PIC  9(013)V9(002) COMP-3.       
030305     03  CLLPDU-VRJUR12          PIC  9(013)V9(002) COMP-3.       
030305     03  CLLPDU-VRIOF-CONTREC    PIC  9(003)V9(006) COMP-3.       
030305     03  CLLPDU-VRIOF-RECOLH     PIC  9(003)V9(006) COMP-3.       
030305     03  CLLPDU-VRIOF-COMPL      PIC  9(003)V9(006) COMP-3.       
030305     03  CLLPDU-VRIOF-VALRECOLH  PIC  9(015)V9(002) COMP-3.       
030305     03  CLLPDU-IDCALC-IOF       PIC  X(001).                     
030305     03  CLLPDU-VRIOF-VALBASE    PIC  9(015)V9(002) COMP-3.       

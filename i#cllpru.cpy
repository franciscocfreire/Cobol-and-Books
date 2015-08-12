      ***************************************************************** 
      *  - AREA DE LEITURA / GRAVACAO DO ARQUIVO - CAD DE LPCL LEI    * 
      *  - TODOS CAMPOS DE DATA ESTAO NO FORMATO  DDMMAAAA            * 
      ***************************************************************** 
       01  WKCADCL.                                                     
           03  CAD-EMP                 PIC  9(005)        COMP-3.       
           03  CAD-AGEN                PIC  9(005)        COMP-3.       
           03  CAD-NUMCL               PIC  9(015)        COMP-3.       
           03  CAD-DIG                 PIC  X(001).                     
           03  CAD-CC                  PIC  9(007)        COMP-3.       
           03  CAD-PEND                PIC  9(005)        COMP-3.       
           03  CAD-SIGLA               PIC  X(004).                     
           03  CAD-CART                PIC  9(003)        COMP-3.       
           03  CAD-CONTR               PIC  9(007)        COMP-3.       
           03  CAD-CART2               PIC  X(003).                     
           03  CAD-RZCL                PIC  9(005)        COMP-3.       
           03  CAD-CTCL                PIC  9(007)        COMP-3.       
           03  CAD-RZ1CL               PIC  9(005)        COMP-3.       
           03  CAD-CT1CL               PIC  9(007)        COMP-3.       
           03  CAD-VCTO                PIC  9(009)        COMP-3.       
           03  CAD-DTECL               PIC  9(009)        COMP-3.       
           03  CAD-DTBCL               PIC  9(009)        COMP-3.       
           03  CAD-DTIOP               PIC  9(009)        COMP-3.       
           03  CAD-DTPRO               PIC  9(009)        COMP-3.       
           03  CAD-DTELP               PIC  9(009)        COMP-3.       
           03  CAD-DTBLP               PIC  9(009)        COMP-3.       
           03  CAD-DTPGTO              PIC  9(009)        COMP-3.       
           03  CAD-MOVTO               PIC  9(009)        COMP-3.       
           03  CAD-DTREX               PIC  9(009)        COMP-3.       
           03  CAD-DTVTR               PIC  9(009)        COMP-3.       
           03  CAD-VRDVI               PIC  9(013)V9(002) COMP-3.       
           03  CAD-VRCON               PIC  9(013)V9(002) COMP-3.       
           03  CAD-VRBXA               PIC  9(013)V9(002) COMP-3.       
           03  CAD-VBASE               PIC  9(013)V9(002) COMP-3.       
           03  CAD-VRBBX               PIC  9(013)V9(002) COMP-3.       
           03  CAD-COBRM               PIC  9(013)V9(002) COMP-3.       
           03  CAD-COBRB               PIC  9(013)V9(002) COMP-3.       
           03  CAD-DEBCC               PIC  9(013)V9(002) COMP-3.       
           03  CAD-COBRD               PIC  9(013)V9(002) COMP-3.       
           03  CAD-VREVE               PIC  9(013)V9(002) COMP-3.       
           03  CAD-VREVI               PIC  9(013)V9(002) COMP-3.       
           03  CAD-VRINI               PIC  9(013)V9(002) COMP-3.       
           03  CAD-BXINI               PIC  9(013)V9(002) COMP-3.       
           03  CAD-VRIOF               PIC  9(013)V9(002) COMP-3.       
           03  CAD-BXIOF               PIC  9(013)V9(002) COMP-3.       
           03  CAD-ANTEX               PIC  9(013)V9(002) COMP-3.       
           03  CAD-MOEDA               PIC  X(002).                     
           03  CAD-MOEDA-ANT           PIC  X(001).                     
           03  CAD-VAGO                PIC  X(001).                     
           03  CAD-AGRESP              PIC  9(005)        COMP-3.       
           03  CAD-NOME                PIC  X(040).                     
           03  CAD-CGC.                                                 
               05  CAD-NCGC            PIC  9(009)        COMP-3.       
               05  CAD-FIL             PIC  9(005)        COMP-3.       
               05  CAD-CTR             PIC  9(003)        COMP-3.       
           03  CAD-NOME1               PIC  X(040).                     
           03  CAD-CGC1.                                                
               05  CAD-NCGC1           PIC  9(009)        COMP-3.       
               05  CAD-FIL1            PIC  9(005)        COMP-3.       
               05  CAD-CTR1            PIC  9(003)        COMP-3.       
           03  CAD-NOME2               PIC  X(040).                     
           03  CAD-CGC2.                                                
               05  CAD-NCGC2           PIC  9(009)        COMP-3.       
               05  CAD-FIL2            PIC  9(005)        COMP-3.       
               05  CAD-CTR2            PIC  9(003)        COMP-3.       
           03  CAD-TPGAR               PIC  X(002).                     
           03  CAD-NOGAR               PIC  X(032).                     
           03  CAD-STARET              PIC  X(001).                     
           03  CAD-TRAG                PIC  X(004).                     
           03  CAD-TRAG-AGEN           PIC  X(004).                     
           03  CAD-TRAG-DATA           PIC  9(009)        COMP-3.       
           03  CAD-DATA-RETORNO        PIC  9(009)        COMP-3.       
           03  CAD-DATA-TRANSLP        PIC  9(009)        COMP-3.       
           03  CAD-ENC-TRANSF          PIC  9(013)V9(002) COMP-3.       
           03  CAD-COMPL-TPO-BAIXA     PIC  X(001).                     
           03  CAD-ADVOG               PIC  9(011)        COMP-3.       
           03  CAD-LOCAL               PIC  X(002).                     
           03  CAD-LOCA2               PIC  X(001).                     
           03  CAD-SUBSTAB             PIC  X(001).                     
           03  CAD-CDULT               PIC  9(003)        COMP-3.       
           03  CAD-DTULT               PIC  9(009)        COMP-3.       
           03  CAD-OCORRENCIAS.                                         
               05  CAD-OCORR           PIC  9(003)        COMP-3        
                                       OCCURS 50 TIMES.                 
           03  CAD-MARCA               PIC  X(001).                     
           03  CAD-IDCON               PIC  X(001).                     
           03  CAD-CARTA               PIC  X(001).                     
           03  CAD-TXPUN               PIC  X(001).                     
           03  CAD-AJUP                PIC  X(001).                     
           03  CAD-REGBX               PIC  X(001).                     
           03  FILLER                  PIC  X(006).                     
           03  CAD-PRECL               PIC  X(001).                     
           03  CAD-DTPRECL             PIC  9(009)        COMP-3.       
           03  CAD-ORIGEMCL            PIC  9(002).                     
           03  CAD-TPEXPUR             PIC  X(001).                     
           03  CAD-NATUREZA            PIC  9(003)        COMP-3.       
           03  CAD-IDENT               PIC  X(002).                     
           03  CAD-TIPBX               PIC  X(001).                     
           03  CAD-TIPO                PIC  9(001).                     
030305     03  CAD-DTULTCAL            PIC  9(009)        COMP-3.       
030305     03  CAD-DTBASE              PIC  9(009)        COMP-3.       
030305     03  CAD-VRJURMORA           PIC  9(013)V9(002) COMP-3.       
030305     03  CAD-VRCORRE             PIC  9(013)V9(002) COMP-3.       
030305     03  CAD-VRJUR12             PIC  9(013)V9(002) COMP-3.       
030305     03  CAD-VRIOF-CONTREC       PIC  9(003)V9(006) COMP-3.       
030305     03  CAD-VRIOF-RECOLH        PIC  9(003)V9(006) COMP-3.       
030305     03  CAD-VRIOF-COMPL         PIC  9(003)V9(006) COMP-3.       
030305     03  CAD-VRIOF-VALRECOLH     PIC  9(015)V9(002) COMP-3.       
030305     03  CAD-IDCALC-IOF          PIC  X(001).                     
030305     03  CAD-VRIOF-VALBASE       PIC  9(015)V9(002) COMP-3.       
           03  CAD-LT.                                                  
              05 CAD-TAXA-CONTRATO       PIC  9(02)V9(06) COMP-3.       
              05 CAD-VR-REMUNERATORIO PIC S9(13)V99 COMP-3.             
              05 CAD-VALOR-MORATORIO     PIC S9(13)V99 COMP-3.          
              05 CAD-VALOR-MULTA         PIC S9(13)V99 COMP-3.          
              05 CAD-DESP-JUD-CUSTAS     PIC S9(11)V99 COMP-3.          
              05 CAD-HONORARIOS          PIC S9(11)V99 COMP-3.          
              05 CAD-VL-TOTAL-DIVIDA     PIC S9(15)V99 COMP-3.          
              05 CAD-VL-TAXA-TARIFA      PIC S9(15)V99 COMP-3.          
              05 CAD-VL-IOF              PIC S9(13)V99 USAGE COMP-3.    
              05 CAD-VL-CORRECAO         PIC S9(13)V99 USAGE COMP-3.    
              05 CAD-VL-JUROS-12PCAA     PIC S9(13)V99 USAGE COMP-3.    
              05 CAD-PERIODICIDADE       PIC S9(02)    USAGE COMP-3.    
              05 FILLER                  PIC  X(13).                    

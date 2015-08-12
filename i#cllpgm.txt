      *---------------------------------------------------------------* 
      *     INPUT:     ARQUIVO MOVIMENTO GERAL CAD LPCL               * 
      *                ORG. SEQUENCIAL  - LRECL = 575                 * 
      *     *** INC I#CLLPGM ***  ANTERIOR   -> I#LPCLA9              * 
      *---------------------------------------------------------------* 
       01   REG-MOVG                PIC X(575).                         
       01   FILLER.                                                     
            05 MOV-TIPO             PIC 9(001).                         
            05 MOV-EMPRESA          PIC 9(005).                         
            05 MOV-AGENCIA          PIC 9(005).                         
            05 MOV-NUMERO           PIC 9(015).                         
            05 MOV-DIGITO           PIC X(001).                         
            05 MOV-CLRAZAO          PIC 9(005).                         
            05 MOV-CLCONTA          PIC 9(007).                         
            05 MOV-ORRAZAO          PIC 9(005).                         
            05 MOV-ORCONTA          PIC 9(007).                         
            05 MOV-CC               PIC 9(007).                         
            05 MOV-DTVENC           PIC 9(008).                         
            05 MOV-DTENTR-CL        PIC 9(008).                         
            05 MOV-DTBAIX-CL        PIC 9(008).                         
            05 MOV-DTINIOP          PIC 9(008).                         
            05 MOV-DTPROTE          PIC 9(008).                         
            05 MOV-DTENTLP          PIC 9(008).                         
            05 MOV-DTBAILP          PIC 9(008).                         
            05 MOV-DTMOV            PIC 9(008).                         
            05 MOV-DTVCTLP          PIC 9(008).                         
            05 MOV-NATSIGL          PIC X(004).                         
            05 MOV-CARTCL           PIC X(003).                         
            05 MOV-NUMCONTR         PIC 9(007).                         
            05 MOV-VLROP            PIC 9(013)V9(002).                  
            05 MOV-VLRCONT          PIC 9(013)V9(002).                  
            05 MOV-MOEDA            PIC X(002).                         
            05 MOV-LOCAL            PIC X(002).                         
            05 MOV-IDCONT           PIC X(001).                         
            05 MOV-TXPUN            PIC X(001).                         
            05 MOV-CL-LP            PIC X(002).                         
            05 MOV-NOMRESP          PIC X(040).                         
            05 MOV-CGCDEV           PIC 9(009).                         
            05 MOV-CGCFIL           PIC 9(004).                         
            05 MOV-CGCCTR           PIC 9(002).                         
            05 MOV-COMPEN           PIC 9(001).                         
            05 MOV-AVAL1            PIC X(040).                         
            05 MOV-AVAL1-CGC        PIC 9(009).                         
            05 MOV-AVAL1-FIL        PIC 9(004).                         
            05 MOV-AVAL1-CTR        PIC 9(002).                         
            05 MOV-AVAL2            PIC X(040).                         
            05 MOV-AVAL2-CGC        PIC 9(009).                         
            05 MOV-AVAL2-FIL        PIC 9(004).                         
            05 MOV-AVAL2-CTR        PIC 9(002).                         
            05 MOV-VALBASE          PIC 9(013)V9(002).                  
            05 MOV-ALIQ-CONT-REC    REDEFINES MOV-VALBASE               
                                    PIC 9(15).                          
            05 MOV-DTREC            PIC 9(008).                         
            05 MOV-SUBST            PIC 9(001).                         
            05 MOV-SUBST-ALF     REDEFINES   MOV-SUBST                  
                                    PIC X(001).                         
            05 MOV-ADVCGC           PIC 9(009).                         
            05 MOV-ADVCTR           PIC 9(002).                         
            05 MOV-CART-ORIG        PIC X(003).                         
            05 MOV-TIPO-GARA        PIC 9(002).                         
            05 MOV-TIPO-GARA-ALF REDEFINES   MOV-TIPO-GARA              
                                    PIC X(002).                         
            05 MOV-TIPO-PEND        PIC 9(004).                         
            05 MOV-MARCA            PIC X(001).                         
            05 MOV-CODFUNC          PIC 9(006).                         
            05 MOV-CODFUNC-R  REDEFINES  MOV-CODFUNC.                   
               07 MOV-STATUS-REAT   PIC 9(001).                         
               07 MOV-AGRESP        PIC 9(004).                         
               07 MOV-EMISSAO       PIC X(001).                         
            05 MOV-COMPL-TPO-BAIXA  PIC X(001).                         
            05 MOV-GERNOME          PIC X(015).                         
            05 FILLER               REDEFINES MOV-GERNOME.              
               06 MOV-VRBASE-IOFC   PIC 9(13)V9(002).                   
            05 MOV-GARANT           PIC X(032).                         
            05 MOV-TPBAIXA          PIC X(001).                         
            05 MOV-TPBAIXA-ALF   REDEFINES   MOV-TPBAIXA                
                                    PIC X(001).                         
            05 MOV-VLR-ENC          PIC 9(013)V9(002).                  
            05 MOV-VLR-VIN          PIC 9(013)V9(002).                  
            05 MOV-VLR-BAI          PIC 9(013)V9(002).                  
            05 MOV-VLR-DEV          PIC 9(013)V9(002).                  
            05 MOV-ALIQ-REC-TR-CL   REDEFINES MOV-VLR-DEV               
                                    PIC 9(15).                          
            05 MOV-VLR-IOF          PIC 9(013)V9(002).                  
            05 MOV-ALIQ-IOFCOMPL    REDEFINES MOV-VLR-IOF               
                                    PIC 9(15).                          
            05 MOV-VLR-DEB          PIC 9(013)V9(002).                  
            05 MOV-VLR-COB          PIC 9(013)V9(002).                  
            05 MOV-CD-ULT-OC        PIC 9(002).                         
            05 MOV-BLOQ             PIC X(001).                         
            05 MOV-DTPGTO           PIC 9(008).                         
            05 MOV-ACERTO           PIC 9(001).                         
            05 MOV-DA-DE            PIC X(002).                         
            05 FILLER               PIC X(001).                         
            05 MOV-DEB-S-N          PIC X(001).                         
            05 MOV-ORIGEM           PIC X(002).                         
            05 MOV-SOL              PIC X(004).                         

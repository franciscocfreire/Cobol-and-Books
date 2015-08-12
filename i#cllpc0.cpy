      *---------------------------------------------------------------* 
      *    ARQUIVO CADASTRO SERASA(ATUALIZADO)                        * 
      *                  *** FORMATO DAS DATAS AAAAMMDD ***           * 
      *    INC I#CLLPC0   LRECL  700        DATA CRIACAO 24/03/2014   * 
      *    COPIA DA I#CLLPCO COM CAMPOS DA LT - 13-0358               * 
      *---------------------------------------------------------------* 
       01  REG-CADSEATU.                                                
           02  CHAVE-CATU.                                              
               03  SPC-CGC-NUM                 PIC 9(09)       COMP-3.  
               03  SPC-CGC-FIL                 PIC 9(05)       COMP-3.  
               03  SPC-CGC-CTR                 PIC 9(03)       COMP-3.  
               03  SPC-NATUREZA-CATU           PIC X(02).               
               03  SPC-VCMTO-CATU              PIC 9(09)       COMP-3.  
           02  SPC-EMPRESA-CATU                PIC 9(05)       COMP-3.  
           02  SPC-AGENCIA-CATU                PIC 9(05)       COMP-3.  
           02  SPC-NUM-CC-CATU                 PIC 9(07)       COMP-3.  
           02  SPC-CARTEIRA-CATU               PIC X(03).               
           02  SPC-CONTRATO-CATU               PIC 9(07)       COMP-3.  
           02  SPC-VALOR-RESGATE-CATU          PIC 9(11)V99    COMP-3.  
           02  SPC-VALOR-VENCIDOS-CATU         PIC 9(11)V99    COMP-3.  
           02  SPC-VALOR-VINCENDOS-CATU        PIC 9(11)V99    COMP-3.  
           02  SPC-NOMERESP-CATU               PIC X(40).               
           02  SPC-CGCCPFRESP-CATU.                                     
               03  SPC-NUMCGC-CATU             PIC 9(09)       COMP-3.  
               03  SPC-FILCGC-CATU             PIC 9(05)       COMP-3.  
               03  SPC-CTRCGC-CATU             PIC 9(03)       COMP-3.  
           02  SPC-DATA-NASCIMEN-CATU          PIC 9(09)       COMP-3.  
           02  SPC-NATURALIDADE-CATU           PIC X(30).               
           02  SPC-UF-ORIGEM-CATU              PIC X(02).               
           02  SPC-NOME-CONJUGE-CATU           PIC X(40).               
           02  SPC-DEV-ENDER-CATU              PIC X(40).               
           02  SPC-DEV-NRO-CATU                PIC X(07).               
           02  SPC-DEV-COMPL-CATU              PIC X(20).               
           02  SPC-DEV-BAIRRO-CATU             PIC X(20).               
           02  SPC-DEV-CIDADE-CATU             PIC X(30).               
           02  SPC-DEV-UF-CATU                 PIC X(02).               
           02  SPC-DEV-CCEP-CATU               PIC 9(05).               
           02  SPC-DEV-CCEP-COMPL-CATU         PIC 9(03).               
           02  SPC-NOMEAVAL1-CATU              PIC X(40).               
           02  SPC-CGC-CPF-AVAL1-CATU.                                  
               03 SPC-CGCCPFAVAL1-CATU         PIC 9(09)       COMP-3.  
               03 SPC-FILCGC-AVAL1-CATU        PIC 9(05)       COMP-3.  
               03 SPC-CTRCGC-AVAL1-CATU        PIC 9(03)       COMP-3.  
           02  SPC-AVA-ENDER-CATU              PIC X(40).               
           02  SPC-AVA-NRO-CATU                PIC X(07).               
           02  SPC-AVA-COMPL-CATU              PIC X(20).               
           02  SPC-AVA-BAIRRO-CATU             PIC X(20).               
           02  SPC-AVA-CIDADE-CATU             PIC X(30).               
           02  SPC-AVA-UF-CATU                 PIC X(02).               
           02  SPC-AVA-CCEP-CATU               PIC 9(05).               
           02  SPC-AVA-CCEP-COMPL-CATU         PIC 9(03).               
           02  SPC-NOMEAVAL2-CATU              PIC X(40).               
           02  SPC-CGC-CPF-AVAL2-CATU.                                  
               03 SPC-CGCCPFAVAL2-CATU         PIC 9(09)       COMP-3.  
               03 SPC-FILCGC-AVAL2-CATU        PIC 9(05)       COMP-3.  
               03 SPC-CTRCGC-AVAL2-CATU        PIC 9(03)       COMP-3.  
           02  SPC-CHAVE-LP-CL-CATU.                                    
               03  SPC-NATUREZA-OPER-CATU      PIC X(02).               
               03  FILLER                      PIC 9(10).               
           02  SPC-NUMERO-CL-CATU              PIC 9(15).               
           02  SPC-DATA-OCORR-CATU             PIC 9(09)       COMP-3.  
           02  SPC-COD-NATUREZA-CATU           PIC 9(03).               
           02  SPC-DTECL-CATU                  PIC 9(09)       COMP-3.  
           02  SPC-DTBCL-CATU                  PIC 9(09)       COMP-3.  
           02  SPC-DTELP-CATU                  PIC 9(09)       COMP-3.  
           02  SPC-DTBLP-CATU                  PIC 9(09)       COMP-3.  
           02  SPC-ID-LPCL-CATU                PIC X(02).               
           02  SPC-TIPBX-CATU                  PIC X(01).               
           02  SPC-TIPO-CATU                   PIC 9(01).               
           02  SPC-TPO-CLIENTE                 PIC X(01).               
           02  SPC-ORIG-ENDER                  PIC X(01).               
           02  SPC-TPO-LOGRA                   PIC X(01).               
           02  SPC-SIT-LOCALIZ                 PIC X(01).               
           02  FILLER                          PIC X(01).               
           02  SPC-LT.                                                  
              03 SPC-TAXA-CONTRATO            PIC 9(02)V9(06) COMP-3.   
              03 SPC-VR-REMUNERATORIO          PIC S9(13)V99   COMP-3.  
              03 SPC-VALOR-MORATORIO           PIC S9(13)V99   COMP-3.  
              03 SPC-VALOR-MULTA               PIC S9(13)V99   COMP-3.  
              03 SPC-DESP-JUD-CUSTAS           PIC S9(11)V99   COMP-3.  
              03 SPC-HONORARIOS                PIC S9(11)V99   COMP-3.  
              03 SPC-VL-TOTAL-DIVIDA           PIC S9(15)V99   COMP-3.  
              03 SPC-VL-TAXA-TARIFA            PIC S9(15)V99   COMP-3.  
              03 FILLER                        PIC  X(39).              

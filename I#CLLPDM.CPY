      *---------------------------------------------------------------* 
      *    ARQUIVO CADASTRO SERASA(SELECIONADO)                       * 
      *             *** FORMATO DAS DATAS = AAAAMMDD ***              * 
      *    INC I#CLLPDM   LRECL  0385                                 * 
      *    CLONE DA INC I#CLLPCJ - TRATAMENTO DE CARTEIRA COM ALFA    * 
      *    CLONE DA INC I#CLLP26 - INCLUIDOS CAMPOS DA LT  13-0358    * 
      *---------------------------------------------------------------* 
       01  REG-WOR-EXP.                                                 
           05 CHAVE-WOR.                                                
              07  CHAVE-WOR-NAT.                                        
               09 CGCCPF-NUM-WOR       PIC 9(09)    COMP-3.             
               09 CGCCPF-FIL-WOR       PIC 9(05)    COMP-3.             
               09 CGCCPF-CTR-WOR       PIC 9(03)    COMP-3.             
               09 NATUREZA-WOR         PIC X(02).                       
              07  DAT-VENCTO-WOR       PIC 9(09)    COMP-3.             
           05 VAL-RESG-WOR             PIC 9(11)V99 COMP-3.             
           05 VAL-ENC-VEN-WOR          PIC 9(11)V99 COMP-3.             
           05 VAL-ENC-VIN-WOR          PIC 9(11)V99 COMP-3.             
           05 CAMPOS-WOR.                                               
              07 NOME-CAD-WOR.                                          
                 09  NOME-CAD-39-WOR   PIC X(39).                       
                 09  FILLER            PIC X(01).                       
              07 CAD-CGC-WOR.                                           
                 09  NUM-CGC-CAD-WOR   PIC 9(09)    COMP-3.             
                 09  FIL-CGC-CAD-WOR   PIC 9(05)    COMP-3.             
                 09  CONTROLE-CAD-WOR  PIC 9(03)    COMP-3.             
              07 CAD-NOME1-WOR         PIC X(40).                       
              07 CAD-CGC1-WOR.                                          
                 09  CAD-NCGC1-WOR     PIC 9(09)    COMP-3.             
                 09  CAD-FIL1-WOR      PIC 9(05)    COMP-3.             
                 09  CAD-CTR1-WOR      PIC 9(03)    COMP-3.             
              07 CAD-NOME2-WOR         PIC X(40).                       
              07 CAD-CGC2-WOR.                                          
                 09  CAD-NCGC2-WOR     PIC 9(09)    COMP-3.             
                 09  CAD-FIL2-WOR      PIC 9(05)    COMP-3.             
                 09  CAD-CTR2-WOR      PIC 9(03)    COMP-3.             
           05 NOME-3-AVAL-WOR          PIC X(30).                       
BRQ=E******05 CARTEIRA-WOR             PIC 9(03)    COMP-3.             
BRQ=I      05 CARTEIRA-WOR             PIC X(03).                       
           05 CGC-CPF-WOR.                                              
              07 CGC-NRO-WOR           PIC 9(09)    COMP-3.             
              07 CGC-FIL-WOR           PIC 9(05)    COMP-3.             
              07 CGC-CTR-WOR           PIC 9(03)    COMP-3.             
           05 EMPRESA-WOR              PIC 9(05)    COMP-3.             
           05 MOEDA-WOR                PIC X(02).                       
           05 COD-NATUREZA-WOR         PIC 9(03).                       
           05 CAD-IDENT-WOR            PIC X(02).                       
           05 AGENCIA-WOR              PIC 9(05)    COMP-3.             
           05 CONTA-WOR                PIC 9(07)    COMP-3.             
           05 CONTRATO-WOR             PIC 9(07)    COMP-3.             
BRQ=E******05 FILLER                   PIC X(34).
           05 CONTR-MULTIPLO-CADLPCL   PIC X(01).
BRQ=I      05 FILLER                   PIC X(32).
           05 WOR-LT.                                                   
              10 WOR-TAXA-CONTRATO     PIC 9(02)V9(06)  COMP-3.         
              10 WOR-VR-REMUNERATORIO  PIC S9(13)V99  COMP-3.           
              10 WOR-VALOR-MORATORIO   PIC S9(13)V99  COMP-3.           
              10 WOR-VALOR-MULTA       PIC S9(13)V99  COMP-3.           
              10 WOR-DESP-JUD-CUSTAS   PIC S9(11)V99  COMP-3.           
              10 WOR-HONORARIOS        PIC S9(11)V99  COMP-3.           
              10 WOR-VL-TOTAL-DIVIDA   PIC S9(15)V99  COMP-3.           
              10 WOR-VL-TAXA-TARIFA    PIC S9(15)V99  COMP-3.           
              10 FILLER                PIC  X(39).                      

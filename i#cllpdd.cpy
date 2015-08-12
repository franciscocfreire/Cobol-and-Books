      *---------------------------------------------------------------* 
      *    ARQUIVO CADASTRO SERASA(SELECIONADO)                       * 
      *                                                               * 
      *    INC I#CLLPDD   LRECL  0362       DATA CRIACAO 20/03/2014   * 
      *    COPIA DA INC I#CLLPCN COM CAMPOS DA LT  13-0358            * 
      *---------------------------------------------------------------* 
       01  REG-WOR.                                                     
           05 CHAVE-WOR.                                                
              07  CHAVE-WOR-NAT.                                        
               09 NUM-CPF-WOR          PIC 9(09)       COMP-3.          
               09 FIL-CPF-WOR          PIC 9(05)       COMP-3.          
               09 CTR-CPF-WOR          PIC 9(03)       COMP-3.          
               09 NATUREZA-WOR         PIC X(02).                       
              07  DAT-VENCTO-WOR       PIC 9(09)       COMP-3.          
           05 VAL-RESG-WOR             PIC 9(11)V99    COMP-3.          
           05 VAL-ENC-VEN-WOR          PIC 9(11)V99    COMP-3.          
           05 VAL-ENC-VIN-WOR          PIC 9(11)V99    COMP-3.          
           05 CAMPOS-WOR.                                               
              07 NOME-CAD-WOR.                                          
                 09  NOME-CAD-39-WOR   PIC X(39).                       
                 09  FILLER            PIC X(01).                       
              07 CAD-CGC-WOR.                                           
                 09  NUM-CGC-CAD-WOR   PIC 9(09)       COMP-3.          
                 09  FIL-CGC-CAD-WOR   PIC 9(05)       COMP-3.          
                 09  CONTROLE-CAD-WOR  PIC 9(03)       COMP-3.          
              07 CAD-NOME1-WOR         PIC X(40).                       
              07 CAD-CGC1-WOR.                                          
                 09  CAD-NCGC1-WOR     PIC 9(09)       COMP-3.          
                 09  CAD-FIL1-WOR      PIC 9(05)       COMP-3.          
                 09  CAD-CTR1-WOR      PIC 9(03)       COMP-3.          
              07 CAD-NOME2-WOR         PIC X(40).                       
              07 CAD-CGC2-WOR.                                          
                 09  CAD-NCGC2-WOR     PIC 9(09)       COMP-3.          
                 09  CAD-FIL2-WOR      PIC 9(05)       COMP-3.          
                 09  CAD-CTR2-WOR      PIC 9(03)       COMP-3.          
           05 NOME-3-AVAL-WOR          PIC X(30).                       
           05 CC-OP-WOR                PIC 9(07)       COMP-3.          
BRQ=E******05 CARTEIRA-WOR             PIC 9(03)       COMP-3.          
BRQ=I      05 CARTEIRA-WOR             PIC X(03).                       
           05 CONTRATO-WOR             PIC 9(07)       COMP-3.          
           05 CGC-CPF-WOR.                                              
              07 CGC-NRO-WOR           PIC 9(09)       COMP-3.          
              07 CGC-FIL-WOR           PIC 9(05)       COMP-3.          
              07 CGC-CTR-WOR           PIC 9(03)       COMP-3.          
           05 CHAVE-LP-CL-WOR.                                          
              07 COD-JUN-WOR           PIC 9(05)       COMP-3.          
              07 AGE-OP-WOR            PIC 9(05)       COMP-3.          
              07 NUMERO-CL-WOR         PIC 9(15)       COMP-3.          
           05 COD-NATUREZA-WOR         PIC 9(03).                       
BRQ=E******05 FILLER                   PIC X(07).                       
BRQ=I      05 FILLER                   PIC X(06).                       
           05 WOR-LT.                                                   
              10 WOR-TAXA-CONTRATO     PIC 9(02)V9(06) COMP-3.          
              10 WOR-VR-REMUNERATORIO  PIC S9(13)V99   COMP-3.          
              10 WOR-VALOR-MORATORIO   PIC S9(13)V99   COMP-3.          
              10 WOR-VALOR-MULTA       PIC S9(13)V99   COMP-3.          
              10 WOR-DESP-JUD-CUSTAS   PIC S9(11)V99   COMP-3.          
              10 WOR-HONORARIOS        PIC S9(11)V99   COMP-3.          
              10 WOR-VL-TOTAL-DIVIDA   PIC S9(15)V99   COMP-3.          
              10 WOR-VL-TAXA-TARIFA    PIC S9(15)V99   COMP-3.          
              10 FILLER                PIC  X(39).                      
                                                                        

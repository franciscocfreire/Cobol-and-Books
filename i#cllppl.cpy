      ******************************************************************
      *            -  INC  * I#CLLPPL *                                *
      *                                                                *
      *  AREA PARA REGISTRO DE SELECAO DE AVISOS                       *
      *            ARQSELE   -   LRECL  =  1530                        *
      *                                                                *
      *  REGISTROS SELECIONADOS P/ AVISOS DE COBRANCA (SAIDA CLLP7634) *
      * COPIA DO I#CLLPPA COM CAMPOS DA LT  13-0358                    *
      ******************************************************************
      *                                                                 
       01  REG-ARQSELE.                                                 
           02  SELE-CHAVE.                                              
               03  SELE-NUMCPF          PIC 9(09)     COMP-3.           
               03  SELE-FILCPF          PIC 9(05)     COMP-3.           
               03  SELE-CTRCPF          PIC 9(03)     COMP-3.           
           02  SELE-EMPRESA             PIC 9(05)     COMP-3.           
           02  SELE-AGENCIA             PIC 9(05)     COMP-3.           
           02  SELE-NUM-CC              PIC 9(07)     COMP-3.           
           02  SELE-VALOR-VENCIDOS      PIC 9(11)V99  COMP-3.           
           02  SELE-VALOR-VINCENDOS     PIC 9(11)V99  COMP-3.           
           02  SELE-NOMERESP            PIC X(40).                      
           02  SELE-CGCCPF-RESP.                                        
               03  SELE-NUMCGC          PIC 9(09)     COMP-3.           
               03  SELE-FILCGC          PIC 9(05)     COMP-3.           
               03  SELE-CTRCGC          PIC 9(03)     COMP-3.           
           02  SELE-DATA-NASCIMEN       PIC 9(09)     COMP-3.           
           02  SELE-NATURALIDADE        PIC X(30).                      
           02  SELE-UF-ORIGEM           PIC X(02).                      
           02  SELE-NOME-CONJUGE        PIC X(40).                      
           02  SELE-DEV-ENDER           PIC X(40).                      
      *                                                                 
      * ESTA LOCALIZACAO EH PARA POSICIONAR AS OCORRENCIAS NA MESMA     
      * POSICAO DAS OUTRAS BOOKS QUE TEM OCORRENCIAS                    
      * I#CLLPLJ E I#CLLPPN                                             
      *                                                                 
           02  SELE-OCORRENCIAS OCCURS 11 TIMES.                        
               03 SELE-POSSIBILITI      PIC X(01).                      
               03 SELE-NATUREZA         PIC X(02).                      
               03 SELE-DAT-VENCTO       PIC 9(09)     COMP-3.           
               03 SELE-CONTRATO         PIC 9(07)     COMP-3.           
               03 SELE-CARTEIRA         PIC X(03).                      
               03 SELE-IOF-NORMAL       PIC S9(13)V99 COMP-3.           
               03 SELE-RESGATE          PIC 9(011)V99 COMP-3.           
               03 SELE-VR-REMUNERATORIO PIC S9(13)V99 COMP-3.           
               03 SELE-VALOR-MORATORIO  PIC S9(13)V99 COMP-3.           
               03 SELE-VALOR-MULTA      PIC S9(13)V99 COMP-3.           
               03 SELE-DESP-JUD-CUSTAS  PIC S9(11)V99 COMP-3.           
               03 SELE-HONORARIOS       PIC S9(11)V99 COMP-3.           
               03 SELE-VL-TAXA-TARIFA   PIC S9(15)V99 COMP-3.           
               03 SELE-VL-TOTAL-DIVIDA  PIC S9(15)V99 COMP-3.           
           02  SELE-DEV-NRO             PIC X(07).                      
           02  SELE-DEV-COMPL           PIC X(20).                      
           02  SELE-DEV-BAIRRO          PIC X(20).                      
           02  SELE-DEV-CIDADE          PIC X(30).                      
           02  SELE-DEV-UF              PIC X(02).                      
           02  SELE-DEV-CCEP            PIC 9(05).                      
           02  SELE-DEV-CCEP-COMPL      PIC 9(03).                      
           02  SELE-NOMEAVAL1           PIC X(40).                      
           02  SELE-CGC-CPF-AVAL1.                                      
               03 SELE-CGCCPF-AVAL1     PIC 9(09)     COMP-3.           
               03 SELE-FILCGC-AVAL1     PIC 9(05)     COMP-3.           
               03 SELE-CTRCGC-AVAL1     PIC 9(03)     COMP-3.           
           02  SELE-AVA-ENDER           PIC X(40).                      
           02  SELE-AVA-NRO             PIC X(07).                      
           02  SELE-AVA-COMPL           PIC X(20).                      
           02  SELE-AVA-BAIRRO          PIC X(20).                      
           02  SELE-AVA-CIDADE          PIC X(30).                      
           02  SELE-AVA-UF              PIC X(02).                      
           02  SELE-AVA-CCEP            PIC 9(05).                      
           02  SELE-AVA-CCEP-COMPL      PIC 9(03).                      
           02  SELE-NOMEAVAL2           PIC X(40).                      
           02  SELE-CGC-CPF-AVAL.                                       
               03 SELE-CGCCPF-AVAL2     PIC 9(09)     COMP-3.           
               03 SELE-FILCGC-AVAL2     PIC 9(05)     COMP-3.           
               03 SELE-CTRCGC-AVAL2     PIC 9(03)     COMP-3.           
           02  SELE-CHAVE-LP-CL.                                        
               03  SELE-NATUREZA-OPER   PIC X(02).                      
               03  FILLER               PIC 9(10).                      
           02  SELE-NUMERO-CL           PIC 9(15).                      
           02  SELE-DATA-OCORR          PIC 9(09)     COMP-3.           
           02  SELE-COD-NATUREZA        PIC 9(03).                      
           02  SELE-DTECL               PIC 9(09)     COMP-3.           
           02  SELE-DTBCL               PIC 9(09)     COMP-3.           
           02  SELE-DTELP               PIC 9(09)     COMP-3.           
           02  SELE-DTBLP               PIC 9(09)     COMP-3.           
           02  SELE-ID-LPCL             PIC X(02).                      
           02  SELE-TIPBX               PIC X(01).                      
           02  SELE-TIPO                PIC 9(01).                      
           02  SELE-TPO-CLIENTE         PIC X(01).                      
           02  SELE-ORIG-ENDER          PIC X(01).                      
           02  SELE-TPO-LOGRA           PIC X(01).                      
           02  SELE-CSIT-LOCALIZ        PIC 9(01).                      
           02  SELE-IDENT               PIC 9(02).                      
           02  FILLER                   PIC X(04).                      

      ******************************************************************
      *            -  INC  * I#CLLPPN *                                *
      *                                                                *
      *  AREA PARA REGISTRO DE AGENCIA E CORREIO SPC                   *
      *                      -   LRECL  =  1290                        *
      *                                                                *
      *  REGISTROS SELECIONADOS P/ AVISOS DE COBRANCA (SAIDA CLLP7625) *
      *  BOOK INTERNA DO CLLP7625  COM CAMPOS DA LT  13-0358           *
      ******************************************************************
      *                                                                 
       01 REG-SPC.                                                      
          03 SPC-CHAVE.                                                 
             05 SPC-CHAVE-CGC.                                          
                10 SPC-CGC-NUM        PIC  9(09)       COMP-3.          
                10 SPC-CGC-FIL        PIC  9(05)       COMP-3.          
                10 SPC-CGC-CTR        PIC  9(03)       COMP-3.          
           03 SPC-EMPRESA             PIC  9(05)       COMP-3.          
           03 SPC-AGENCIA             PIC  9(05)       COMP-3.          
           03 SPC-NUM-CC              PIC  9(07)       COMP-3.          
           03 SPC-NOMERESP            PIC  X(40).                       
           03 SPC-CGCCPFRESP.                                           
              05 SPC-PRINCIPAL        PIC  9(09)       COMP-3.          
              05 SPC-FILIAL           PIC  9(05)       COMP-3.          
              05 SPC-CONTROLE         PIC  9(03)       COMP-3.          
           03 SPC-ENDER               PIC  X(40).                       
           03 SPC-NRO                 PIC  X(07).                       
           03 SPC-COMPL               PIC  X(20).                       
           03 SPC-BAIRRO              PIC  X(20).                       
           03 SPC-CIDADE              PIC  X(30).                       
           03 SPC-UF                  PIC  X(02).                       
           03 SPC-CCEP                PIC  9(05).                       
           03 SPC-CCEP-COMPL          PIC  9(03).                       
           03 SPC-IDENT               PIC  9(02).                       
           03 FILLER                  PIC  X(02).                       
      *                                                                 
      * ESTE FILLER EH PARA POSICIONAR AS OCORRENCIAS NA MESMA          
      * POSICAO DAS OUTRAS BOOKS QUE TEM OCORRENCIAS                    
      * I#CLLPLJ E I#CLLPPL                                             
      *                                                                 
           03 SPC-OCORRENCIAS OCCURS 11 TIMES.                          
              05 SPC-POSSIBILIT       PIC  X(01).                       
              05 SPC-NATUREZA         PIC  X(02).                       
              05 SPC-DAT-VENCTO       PIC  9(09)       COMP-3.          
              05 SPC-CONTRATO         PIC  9(07)       COMP-3.          
              05 SPC-CARTEIRA         PIC  X(03).                       
              05 SPC-IOF-NORMAL       PIC S9(013)V99   COMP-3.          
              05 SPC-RESGATE          PIC  9(011)V99   COMP-3.          
              05 SPC-VR-REMUNERATORIO PIC S9(13)V99    COMP-3.          
              05 SPC-VALOR-MORATORIO  PIC S9(13)V99    COMP-3.          
              05 SPC-VALOR-MULTA      PIC S9(13)V99    COMP-3.          
              05 SPC-DESP-JUD-CUSTAS  PIC S9(11)V99    COMP-3.          
              05 SPC-HONORARIOS       PIC S9(11)V99    COMP-3.          
              05 SPC-VL-TAXA-TARIFA   PIC S9(15)V99    COMP-3.          
              05 SPC-VL-TOTAL-DIVIDA  PIC S9(15)V99    COMP-3.          
           03 SPC-TIPO                PIC  9(01).                       
           03 SPC-COD-ENTR            PIC  9(01).                       
           03 SPC-NOME-AGENCIA        PIC  X(20).                       
           03 SPC-END-AGENCIA         PIC  X(25).                       
           03 SPC-CEP-AGENCIA         PIC  9(09)       COMP-3.          
           03 SPC-SIGLA-AGENCIA       PIC  X(02).                       
           03 SPC-MUNIC-AGENCIA       PIC  X(14).                       
           03 SPC-CD-AGENCIA          PIC  9(05)       COMP-3.          
           03 SPC-COD-BARRA.                                            
              05 SPC-CB-NUMCPF-X.                                       
                 07 SPC-CB-NUMCPF     PIC  9(009).                      
                 07 SPC-CB-FILCPF     PIC  9(004).                      
                 07 SPC-CB-CTRCPF     PIC  9(002).                      
              05 SPC-CB-DT-MOVTO      PIC  9(008).                      
              05 SPC-CB-AGENCIA       PIC  9(005).                      
              05 SPC-CB-TPO-CLIENTE   PIC  9(001).                      
           03 FILLER                  PIC  X(39).                       
           03 SPC-SEQUENCIAL          PIC  9(007)      COMP-3.          

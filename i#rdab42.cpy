      ***************************************************************** 
      *  - I#RDAB42 ARQUIVO INTERMEDIARIO DE CONTRATO DE RENEGOCIACAO * 
      *  - LRECL=215   DT.CRIACAO: 12/2003  POR: MARCELO NARDAO - CPM * 
      *    (ISD)                                                      * 
      ***************************************************************** 
      * ALTERACAO                                                     * 
      * DATA      : 31/10/2011                                        * 
      * AUTOR     : TALLES                                            * 
      * EMPRESA   : BRQ                                               * 
      * TAMANHO   : 242                                               * 
      ***************************************************************** 
      * ALTERACAO PARA O PROJETO LEI DA TRANSPARENCIA                 * 
      * DATA      : AGOSTO/2014                                       * 
      * AUTOR     : ROBSON                                            * 
      * EMPRESA   : BRQ                                               * 
      * TAMANHO   : 365                                               * 
      ***************************************************************** 
                                                                        
       01  ACR-REGISTRO-PB.                                             
           05  ACR-BANCO-PB            PIC  9(03).                      
           05  ACR-PV-1-PB             PIC  X(01).                      
           05  ACR-AGENCIA-PB          PIC  9(05).                      
           05  ACR-PV-2-PB             PIC  X(01).                      
           05  ACR-CONTA-PB            PIC  9(13).                      
           05  ACR-PV-3-PB             PIC  X(01).                      
           05  ACR-CARTEIRA-PB         PIC  X(03).                      
           05  ACR-PV-4-PB             PIC  X(01).                      
           05  ACR-CONTRATO-PB         PIC  9(17).                      
           05  ACR-PV-5-PB             PIC  X(01).                      
           05  ACR-TIPO-COBR-PB        PIC  X(01).                      
           05  ACR-PV-6-PB             PIC  X(01).                      
           05  ACR-AVISO-COBR-PB       PIC  X(06).                      
           05  ACR-PV-7-PB             PIC  X(01).                      
           05  ACR-NOTIF-PB            PIC  X(01).                      
           05  ACR-PV-8-PB             PIC  X(01).                      
           05  ACR-BLOQ-LMNAR-PB       PIC  X(01).                      
           05  ACR-PV-9-PB             PIC  X(01).                      
           05  ACR-ADVOG-NUM-PB        PIC  9(09).                      
           05  ACR-PV-10-PB            PIC  X(01).                      
           05  ACR-ADVOG-CTRL-PB       PIC  9(02).                      
           05  ACR-PV-11-PB            PIC  X(01).                      
           05  ACR-CDIR-RGNAL-PB       PIC  9(03).                      
           05  ACR-PV-12-PB            PIC  X(01).                      
           05  ACR-CJUNC-DIR-RGNAL-PB  PIC  9(05).                      
           05  ACR-PV-13-PB            PIC  X(01).                      
           05  ACR-NATUZ-CONTR-PB      PIC  X(03).                      
           05  ACR-PV-14-PB            PIC  X(01).                      
           05  ACR-TPO-GARNT-PB        PIC  9(03).                      
           05  ACR-PV-15-PB            PIC  X(01).                      
           05  ACR-AJUIZ-PB            PIC  X(01).                      
           05  ACR-PV-16-PB            PIC  X(01).                      
           05  ACR-BENEF-PB            PIC  X(03).                      
           05  ACR-PV-17-PB            PIC  X(01).                      
           05  ACR-QPCELA-VNCER-PB     PIC  9(05).                      
           05  ACR-PV-18-PB            PIC  X(01).                      
           05  ACR-QPCELA-TOT-PB       PIC  9(05).                      
           05  ACR-PV-19-PB            PIC  X(01).                      
           05  ACR-CONVENIO-PB         PIC  9(09).                      
           05  ACR-PV-20-PB            PIC  X(01).                      
           05  ACR-PRODUTO-PB          PIC  9(03).                      
           05  ACR-PV-21-PB            PIC  X(01).                      
           05  ACR-SUB-PRODUTO-PB      PIC  9(03).                      
           05  ACR-PV-22-PB            PIC  X(01).                      
           05  ACR-FAMILIA-PB          PIC  9(01).                      
           05  ACR-PV-23-PB            PIC  X(01).                      
           05  ACR-SDO-VENCER-PB       PIC  9(15)V9(02).                
           05  ACR-PV-24-PB            PIC  X(01).                      
           05  ACR-NEGTV-SERASA-PB     PIC  X(01).                      
           05  ACR-PV-25-PB            PIC  X(01).                      
           05  ACR-NEGTV-SPC-PB        PIC  X(01).                      
           05  ACR-PV-26-PB            PIC  X(01).                      
           05  ACR-SISTEMA-PB          PIC  X(04).                      
           05  ACR-PV-27-PB            PIC  X(01).                      
           05  ACR-PTX-OPER-PB         PIC  9(04)V9(07).                
           05  ACR-PV-28-PB            PIC  X(01).                      
           05  ACR-CPERDC-TX-PB        PIC  9(01).                      
           05  ACR-PV-29-PB            PIC  X(01).                      
           05  ACR-RTING-FINAL-PB      PIC  X(02).                      
           05  ACR-PV-30-PB            PIC  X(01).                      
           05  ACR-DT-INIC-OPER-PB     PIC  X(10).                      
           05  ACR-PV-31-PB            PIC  X(01).                      
           05  ACR-DT-VECTO-CONTR-PB   PIC  X(10).                      
           05  ACR-PV-32-PB            PIC  X(01).                      
           05  ACR-DT-PROX-VENC-PB     PIC  X(10).                      
           05  ACR-PV-33-PB            PIC  X(01).                      
           05  ACR-DT-ULT-PGTO-PB      PIC  X(10).                      
      *                                                                 
      *    INCLUSAO DOS CAMPOS FATOR DE RISCO DO CLIENTE                
      *                                                                 
           05  ACR-PV-34-PB            PIC  X(01).                      
           05  ACR-PD-PB               PIC 9(003)V99.                   
           05  ACR-PV-35-PB            PIC  X(01).                      
           05  ACR-BS-PB               PIC 9(003).                      
           05  ACR-PV-36-PB            PIC  X(01).                      
           05  ACR-CS-PB               PIC 9(003).                      
           05  ACR-PV-37-PB            PIC  X(01).                      
           05  ACR-CONTROLE-PB         PIC 9(003).                      
           05  ACR-PV-38-PB            PIC  X(01).                      
           05  ACR-DT-DDMMAAAA-ATU-PB  PIC X(010).                      
           05  ACR-PV-39-PB            PIC  X(01).                      
BRQLT      05  ACR-TAXA-CONTRATO-PB    PIC  9(02)V9(06).                
           05  ACR-PV-40-PB            PIC  X(01).                      
BRQLT      05  ACR-VR-REMUNERATORIO-PB PIC  S9(13)V99.                  
           05  ACR-PV-41-PB            PIC  X(01).                      
BRQLT      05  ACR-VALOR-MORATORIO-PB  PIC  S9(13)V99.                  
           05  ACR-PV-42-PB            PIC  X(01).                      
BRQLT      05  ACR-VALOR-MULTA-PB      PIC  S9(13)V99.                  
           05  ACR-PV-43-PB            PIC  X(01).                      
BRQLT      05  ACR-DESP-JUD-CUSTAS-PB  PIC  S9(11)V99.                  
           05  ACR-PV-44-PB            PIC  X(01).                      
BRQLT      05  ACR-VL-HONORARIOS-PB    PIC  S9(11)V99.                  
           05  ACR-PV-45-PB            PIC  X(01).                      
BRQLT      05  ACR-VL-TOTAL-DIVIDA-PB  PIC  S9(15)V99.                  
           05  ACR-PV-46-PB            PIC  X(01).                      
BRQLT      05  ACR-VL-TAXA-TARIFA-PB   PIC  S9(15)V99.                  

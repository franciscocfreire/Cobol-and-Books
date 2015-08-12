      *---------------------------------------------------------------* 
      *    ARQUIVO DE TABELA DB2 - LPCLB000                           * 
      *                                                               * 
      *    INC I#CLLPFD  - TAMANHO  LRECL 690  -                      * 
      *                                                               * 
      *    COPIA DA I#CLLPKA COM OS CAMPOS NOVOS DA LT.   13-0358     * 
      *---------------------------------------------------------------* 
                                                                        
       01   DB2-REGTO.                                                  
            02  DB2-CHAVE.                                              
                03  DB2-EMPRESA                 PIC  9(05)       COMP-3.
                03  DB2-AGENCIA                 PIC  9(05)       COMP-3.
                03  DB2-NUM-CL                  PIC  9(15)       COMP-3.
                03  DB2-DIG                     PIC  X(01).             
            02  DB2-CON-CORR.                                           
                03  DB2-CONTA                   PIC  9(07)       COMP-3.
                03  DB2-DIG-CON                 PIC  X(01).             
            02  DB2-CARTEIRA                    PIC  X(03).             
            02  DB2-CONTRATO                    PIC  9(07)       COMP-3.
'           02  DB2-VCTO                        PIC  X(10).             
            02  DB2-ID                          PIC  X(02).             
            02  DB2-DATAS.                                              
                03  DB2-ENTRADAS                PIC  X(10).             
                03  DB2-VCTO-MORA               PIC  X(10).             
            02  DB2-MOEDA                       PIC  X(02).             
            02  DB2-TIPO-GAR                    PIC  X(02).             
            02  DB2-LOCAL                       PIC  X(02).             
            02  DB2-TIPO-PEND                   PIC  9(05)       COMP-3.
            02  DB2-SIGLA                       PIC  X(04).             
            02  DB2-TELEFONE                    PIC  9(07)       COMP-3.
            02  DB2-CPF-ADVOG.                                          
                03  DB2-NUMERO-ADV              PIC  9(09)       COMP-3.
                03  DB2-CTR                     PIC  9(02).             
            02  DB2-ULT-OCORR.                                          
                03  DB2-CODIGO                  PIC  9(03)       COMP-3.
                03  DB2-DATA-OCORR              PIC  X(10).             
                03  DB2-STATUS                  PIC  X(02).             
            02  DB2-DATA-VENC-CARENCIA          PIC  X(10).             
            02  DB2-VALORES.                                            
                03  DB2-DEV-INIC                PIC  9(13)V99    COMP-3.
                03  DB2-PRINCIPAL               PIC  9(13)V99    COMP-3.
                03  DB2-CONTABIL                PIC  9(13)V99    COMP-3.
                03  DB2-LIQUIDO                 PIC  9(13)V99    COMP-3.
                03  DB2-COBRANCA                PIC  9(13)V99    COMP-3.
                03  DB2-COBRANCA-INF            PIC  9(13)V99    COMP-3.
                03  DB2-JUROS-MORA              PIC  9(13)V99    COMP-3.
                03  DB2-CORR-MONET              PIC  9(13)V99    COMP-3.
                03  DB2-JUROS-12AA              PIC  9(13)V99    COMP-3.
                03  DB2-TOTTAL-CONTRATO         PIC  9(13)V99    COMP-3.
            02  DB2-CARTEIRA-ORIGEM             PIC  X(03).             
            02  DB2-COD-NATUREZA-OPER           PIC  X(03).             
            02  DB2-NOME-DEVEDOR                PIC  X(40).             
            02  DB2-CPF-DEV.                                            
                03  DB2-NUMERO-DEV              PIC  9(09)       COMP-3.
                03  DB2-FILIAL                  PIC  9(05)       COMP-3.
                03  DB2-CTR-DEV                 PIC  9(02).             
            02  DB2-NOME-AVALISTA               PIC  X(40).             
            02  DB2-CPF-AVAL.                                           
                03  DB2-NUMERO-AVAL             PIC  9(09)       COMP-3.
                03  DB2-FILIAL-AVAL             PIC  9(05)       COMP-3.
                03  DB2-CTR-AVAL                PIC  9(02).             
            02  DB2-NOME-AVAL2                  PIC  X(40).             
            02  DB2-CPF-AVAL2.                                          
                03  DB2-NUMERO-AVAL2            PIC  9(09)       COMP-3.
                03  DB2-FILIAL-AVAL2            PIC  9(05)       COMP-3.
                03  DB2-CTR-AVAL2               PIC  9(02).             
            02  DB2-BLOQUEIO-TRANSF             PIC  X(01).             
            02  DB2-DIRETORIA.                                          
                03  DB2-EXEC                    PIC  9(03)       COMP-3.
                03  DB2-REG.                                            
                    05  DB2-REGIONAL            PIC  9(03)       COMP-3.
                    05  DB2-COD-JUNCAO          PIC  9(05)       COMP-3.
            02  DB2-DEP-CAMBIO                  PIC  9(05)       COMP-3.
            02  DB2-COD-EMPR                    PIC  9(02).             
            02  DB2-OCORRENCIAS                 OCCURS 50 TIMES.        
                03  DB2-OCCORS                  PIC  9(03)       COMP-3.
            02  DB2-OPER                        PIC  X(10).             
            02  DB2-DT-PDD-180                  PIC  X(10).             
            02  DB2-MARCA-PDD-180               PIC  X(01).             
            02  DB2-RAZAO-PRINCIP               PIC  9(05)       COMP-3.
            02  DB2-RAZAO-RENDAS                PIC  9(05)       COMP-3.
            02  DB2-VR-VENCIDOS                 PIC  9(13)V99    COMP-3.
            02  DB2-VR-VINCENDOS                PIC  9(13)V99    COMP-3.
            02  DB2-MARCA-IMPE                  PIC  X(01).             
            02  DB2-DATA-RETORNO-CL             PIC  X(10).             
            02  DB2-DATA-PRIM-TRANSF            PIC  X(10).             
            02  DB2-VR-ENCVEN-CONG              PIC  9(13)V99    COMP-3.
            02  DB2-STATUS-REATIVACAO           PIC  X(01).             
            02  DB2-EMPF-PRODUTO                PIC  9(03)       COMP-3.
            02  DB2-EMPF-FAMILIA                PIC  9(01)       COMP-3.
            02  DB2-IND-PEND-EXP                PIC  X(01).             
            02  DB2-DATA-AJUIZAMENTO            PIC  X(10).             
            02  DB2-IOF-NORMAL                  PIC  9(13)V9(02) COMP-3.
            02  DB2-IOF-COMPL                   PIC  9(13)V9(02) COMP-3.
            02  DB2-ALIQ-CONT-RECOL             PIC  9(03)V9(06) COMP-3.
            02  DB2-ALIQ-REC-TR-CL              PIC  9(03)V9(06) COMP-3.
            02  DB2-ALIQ-COMPL                  PIC  9(03)V9(06) COMP-3.
            02  DB2-IOF-OPCAO                   PIC  X(01).             
            02  DB2-DATA-SISTEL                 PIC  X(10).             
            02  DB2-IND-SISTEL                  PIC  X(01).             
            02  DB2-NOTIF-SISTEL                PIC  X(01).             
            02  DB2-AGENCIA-RESP                PIC  9(05)       COMP-3.
            02  DB2-LT.                                                 
                03 DB2-TAXA-CONTRATO            PIC 9(02)V9(06)  COMP-3.
                03 DB2-VR-REMUNERATORIO         PIC S9(13)V99    COMP-3.
                03 DB2-VALOR-MORATORIO          PIC S9(13)V99    COMP-3.
                03 DB2-VALOR-MULTA              PIC S9(13)V99    COMP-3.
                03 DB2-DESP-JUD-CUSTAS          PIC S9(11)V99    COMP-3.
                03 DB2-HONORARIOS               PIC S9(11)V99    COMP-3.
                03 DB2-VL-TOTAL-DIVIDA          PIC S9(15)V99    COMP-3.
                03 DB2-VL-TAXA-TARIFA           PIC S9(15)V99    COMP-3.
                03 FILLER                       PIC  X(39).             

      ***************************************************************** 
      * NOME BOOK : LPCLW02I                                          * 
      * DESCRICAO : BOOK DO FUNCIONAL LPCL102L - CONTRATOS POR CLIENTE* 
      * DATA      : JUNHO/2013                                        * 
      * AUTOR     : HEBE                                              * 
      * EMPRESA   : BRQ                                               * 
      * GRUPO     : TI MELHORIAS                                      * 
      * COM1ONENTE: SISTEMA LUCRO E PERDAS CREDITO LIQUIDACAO         * 
      * TAMANHO   : 6731 BYTES                                        * 
      ***************************************************************** 
      * DATA       AUTOR             DESCRICAO / MANUTENCAO           * 
      ***************************************************************** 
      * XX/XX/XXXX XXXXXXXXXXXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX * 
      ***************************************************************** 
         05 LPCLW02I-HEADER.                                            
           10 LPCLW02I-COD-LAYOUT           PIC  X(08) VALUE 'LPCLW02I'.
           10 LPCLW02I-TAM-LAYOUT           PIC  9(05) VALUE 6731.      
         05 LPCLW02I-BLOCO-REGISTRO.                                    
           10 LPCLW02I-BLOCO-PAGINACAO.                                 
             15 LPCLW02I-INDICADOR-PAGINACAO     PIC X(01).             
               88 LPCLW02I-P-INICIAL             VALUE 'I'.             
               88 LPCLW02I-P-PRIMEIRA            VALUE 'P'.             
               88 LPCLW02I-P-SEGUINTE            VALUE 'S'.             
               88 LPCLW02I-P-ANTERIOR            VALUE 'A'.             
               88 LPCLW02I-P-ULTIMA              VALUE 'U'.             
             15 LPCLW02I-INI-CHAVE.                                     
                20 LPCLW02I-INI-DVCTO           PIC X(10).              
                20 LPCLW02I-INI-VL-CONTR-MORA   PIC 9(013)V99.          
                20 LPCLW02I-INI-CCTA-CORR       PIC 9(07).              
                20 LPCLW02I-INI-CCART           PIC X(03).              
                20 LPCLW02I-INI-CCONTR          PIC 9(07).              
             15 LPCLW02I-FIM-CHAVE.                                     
                20 LPCLW02I-FIM-DVCTO           PIC X(10).              
                20 LPCLW02I-FIM-VL-CONTR-MORA   PIC 9(013)V99.          
                20 LPCLW02I-FIM-CCTA-CORR       PIC 9(07).              
                20 LPCLW02I-FIM-CCART           PIC X(03).              
                20 LPCLW02I-FIM-CCONTR          PIC 9(07).              
           10 LPCLW02I-BLOCO-ENTRADA.                                   
             15 LPCLW02I-E-AGENCIA-GER          PIC 9(05).              
             15 LPCLW02I-E-COD-FUNC-BRD         PIC 9(09).              
             15 LPCLW02I-E-CPF-CNPJ             PIC 9(09).              
             15 LPCLW02I-E-CFLIAL-CPF-CNPJ      PIC 9(05).              
             15 LPCLW02I-E-CCTRL-CPF-CNPJ       PIC 9(02).              
           10 LPCLW02I-BLOCO-SAIDA.                                     
             15 LPCLW02I-NUM-CONSULTAS          PIC 9(03).              
             15 LPCLW02I-CONSULTAS-SAIDA        OCCURS 1 TO 50 TIMES    
                 DEPENDING ON LPCLW02I-NUM-CONSULTAS.                   
                20 LPCLW02I-S-CCTA-CORR         PIC 9(07).              
                20 LPCLW02I-S-CDIG-CTA-REFT     PIC X(01).              
                20 LPCLW02I-S-CCART             PIC X(03).              
                20 LPCLW02I-S-CCONTR-VENCD      PIC 9(07).              
                20 LPCLW02I-S-CPCELA-CONTR      PIC 9(15).              
                20 LPCLW02I-S-CDIG-CRED-LIQ     PIC X(01).              
                20 LPCLW02I-S-VLIQUI            PIC 9(15)V99.           
                20 LPCLW02I-S-VATULZ            PIC 9(13)V99.           
                20 LPCLW02I-S-VVENCD            PIC 9(13)V99.           
                20 LPCLW02I-S-VTOTAL            PIC 9(13)V99.           
                20 LPCLW02I-S-RFASE-PROC        PIC X(16).              
                20 LPCLW02I-S-DTINIC-OPER       PIC X(10).              
                20 LPCLW02I-S-DVENC             PIC X(10).              

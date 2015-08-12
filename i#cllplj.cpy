      *---------------------------------------------------------------* 
      *    ARQUIVO PARA EMISSAO DE CARTAS PARA AVALISTA               * 
      *             *** FORMATO DAS DATAS = AAAAMMDD ***              * 
      *    INC I#CLLPLJ   LRECL  1290                                 * 
      *    COPIA DO I#CLLPLE   LRECL  0600 PARA ATENDER A LT 13-0358  * 
      *---------------------------------------------------------------* 
                                                                        
       01  REG-AVISO.                                                   
           03  AVI-CHAVE.                                               
               05  AVI-CHAVE-CGC.                                       
                   10  AVI-CGC         PIC 9(09)     COMP-3.            
                   10  AVI-FIL         PIC 9(05)     COMP-3.            
                   10  AVI-CTR         PIC 9(03)     COMP-3.            
           03  AVI-EMPRESA             PIC 9(05)     COMP-3.            
           03  AVI-AGENCIA             PIC 9(05)     COMP-3.            
           03  AVI-NUM-CC              PIC 9(07)     COMP-3.            
           03  AVI-NOME-AVAL           PIC X(40).                       
           03  AVI-CGCCPFAVAL.                                          
               05  AVI-CGC-AVAL        PIC 9(09)     COMP-3.            
               05  AVI-FIL-AVAL        PIC 9(05)     COMP-3.            
               05  AVI-CTR-AVAL        PIC 9(03)     COMP-3.            
           03  AVI-ENDER               PIC X(40).                       
           03  AVI-NRO                 PIC X(07).                       
           03  AVI-COMPL               PIC X(20).                       
           03  AVI-BAIRRO              PIC X(20).                       
           03  AVI-CIDADE              PIC X(30).                       
           03  AVI-UF                  PIC X(02).                       
           03  AVI-CCEP                PIC 9(05).                       
           03  AVI-CCEP-COMPL          PIC 9(03).                       
           03  AVI-IDENT               PIC X(02).                       
           03  FILLER                  PIC X(02).                       
      *                                                                 
      * ESTE FILLER EH PARA POSICIONAR AS OCORRENCIAS NA MESMA          
      * POSICAO DAS OUTRAS BOOKS QUE TEM OCORRENCIAS                    
      * I#CLLPPN E I#CLLPPL                                             
      *                                                                 
           03  AVI-CAMPO1    OCCURS 11 TIMES.                           
              07 AVI-POSSIBILIT        PIC X(01).                       
              07 AVI-NATUREZA          PIC X(02).                       
              07 AVI-DAT-VENCTO        PIC 9(09)     COMP-3.            
              07 AVI-CONTRATO          PIC 9(07)     COMP-3.            
              07 AVI-CARTEIRA          PIC X(03).                       
              07 AVI-IOF-NORMAL        PIC S9(13)V99 COMP-3.            
              07 AVI-RESGATE           PIC 9(011)V99 COMP-3.            
              07 AVI-VR-REMUNERATORIO  PIC S9(13)V99 COMP-3.            
              07 AVI-VALOR-MORATORIO   PIC S9(13)V99 COMP-3.            
              07 AVI-VALOR-MULTA       PIC S9(13)V99 COMP-3.            
              07 AVI-DESP-JUD-CUSTAS   PIC S9(11)V99 COMP-3.            
              07 AVI-HONORARIOS        PIC S9(11)V99 COMP-3.            
              07 AVI-VL-TAXA-TARIFA    PIC S9(15)V99 COMP-3.            
              07 AVI-VL-TOTAL-DIVIDA   PIC S9(15)V99 COMP-3.            
           03  AVI-TIPO                PIC 9(01).                       
           03  AVI-COD-ENTR            PIC 9(01).                       
           03  AVI-NOME-AGENCIA        PIC X(20).                       
           03  AVI-END-AGENCIA         PIC X(25).                       
           03  AVI-CEP-AGENCIA         PIC 9(09)     COMP-3.            
           03  AVI-SIGLA-AGENCIA       PIC X(02).                       
           03  AVI-MUNIC-AGENCIA       PIC X(14).                       
           03  AVI-CD-AGENCIA          PIC 9(05)     COMP-3.            
           03  AVI-NOME-DEVEDOR        PIC X(40).                       
           03  AVI-AGENCIA-AVAL        PIC 9(05)     COMP-3.            
           03  AVI-CONTA-AVAL          PIC 9(07)     COMP-3.            
           03  AVI-TPO-LOGRA           PIC 9(01).                       
           03  AVI-ORIGEM-END          PIC 9(01).                       
           03  AVI-CRET-LOGRA          PIC 9(01).                       
           03  AVI-DTULTATU            PIC X(10).                       
           03  AVI-DATA-MVTO           PIC 9(08).                       
           03  AVI-SEQUENCIAL          PIC 9(007)    COMP-3.            
                                                                        

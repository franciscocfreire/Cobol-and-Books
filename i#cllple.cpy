      *---------------------------------------------------------------* 
      *    ARQUIVO PARA EMISSAO DE CARTAS PARA AVALISTA               * 
      *             *** FORMATO DAS DATAS = AAAAMMDD ***              * 
      *    INC I#CLLPLE   LRECL  0600                                 * 
      *---------------------------------------------------------------* 
004760                                                                  
004770 01  REG-AVISO.                                                   
004780     03  AVI-CHAVE.                                               
004790         05  AVI-CHAVE-CGC.                                       
004800             10  AVI-CGC         PIC 9(09)    COMP-3.             
004810             10  AVI-FIL         PIC 9(05)    COMP-3.             
004820             10  AVI-CTR         PIC 9(03)    COMP-3.             
004830         05  AVI-NATUREZA-AV     PIC X(02).                       
004840         05  AVI-VCMTO           PIC 9(09)    COMP-3.             
004850     03  AVI-EMPRESA             PIC 9(05)    COMP-3.             
004860     03  AVI-AGENCIA             PIC 9(05)    COMP-3.             
004870     03  AVI-NUM-CC              PIC 9(07)    COMP-3.             
004880     03  AVI-CARTEIRA            PIC X(03).                       
004890     03  AVI-CONTRATO            PIC 9(07)    COMP-3.             
004900     03  AVI-VALOR-RESGATE       PIC 9(11)V99 COMP-3.             
004910     03  AVI-NOME-AVAL           PIC X(40).                       
004920     03  AVI-CGCCPFAVAL.                                          
004930         05  AVI-CGC-AVAL        PIC 9(09)    COMP-3.             
004940         05  AVI-FIL-AVAL        PIC 9(05)    COMP-3.             
004950         05  AVI-CTR-AVAL        PIC 9(03)    COMP-3.             
004960     03  AVI-ENDER               PIC X(40).                       
004970     03  AVI-NRO                 PIC X(07).                       
004980     03  AVI-COMPL               PIC X(20).                       
004990     03  AVI-BAIRRO              PIC X(20).                       
005000     03  AVI-CIDADE              PIC X(30).                       
005010     03  AVI-UF                  PIC X(02).                       
005020     03  AVI-CCEP                PIC 9(05).                       
005030     03  AVI-CCEP-COMPL          PIC 9(03).                       
005040     03  AVI-IDENT               PIC X(02).                       
005050     03  AVI-CAMPO1    OCCURS 7 TIMES.                            
              07  AVI-NATUREZA         PIC X(02).                       
              07  AVI-DAT-VENCTO       PIC 9(09)     COMP-3.            
              07  AVI-RESGATE          PIC 9(011)V99 COMP-3.            
              07  AVI-DESCRIC          PIC X(020).                      
005060     03  AVI-TIPO                PIC 9(01).                       
005070     03  AVI-COD-ENTR            PIC 9(01).                       
005080     03  AVI-NOME-AGENCIA        PIC X(20).                       
005090     03  AVI-END-AGENCIA         PIC X(25).                       
005100     03  AVI-CEP-AGENCIA         PIC 9(09)  COMP-3.               
005110     03  AVI-SIGLA-AGENCIA       PIC X(02).                       
005120     03  AVI-MUNIC-AGENCIA       PIC X(14).                       
005130     03  AVI-CD-AGENCIA          PIC 9(05)  COMP-3.               
004910     03  AVI-NOME-DEVEDOR        PIC X(40).                       
004910     03  AVI-AGENCIA-AVAL        PIC 9(05)  COMP-3.               
004910     03  AVI-CONTA-AVAL          PIC 9(07)  COMP-3.               
004910     03  AVI-TPO-LOGRA           PIC 9(01).                       
004910     03  AVI-ORIGEM-END          PIC 9(01).                       
004910     03  AVI-CRET-LOGRA          PIC 9(01).                       
004910     03  AVI-DTULTATU            PIC X(10).                       
004840     03  AVI-DATA-MVTO           PIC 9(08).                       
004910     03  FILLER                  PIC X(03).                       
005140                                                                  

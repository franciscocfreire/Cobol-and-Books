      **** I#CLLPZG ****************************************************
      *                                                                *
      *    BOOK UTILIZADA NO PROCESSO DE GERACAO DE AVISOS DO CLLP     *
      *    TAMANHO : 558 BYTES                                         *
      *                                                                *
      *    CONFECCAO: KLENK / VINICIUS - BSI - ABRIL/2013              *
      *                                                                *
      ******************************************************************
                                                                        
       01  CLLPZG-AREA.                                                 
           03  CLLPZG-CHAVE.                                            
               05  CLLPZG-CHAVE-CGC.                                    
                   10  FILLER          PIC 9(09)    COMP-3.             
                   10  FILLER          PIC 9(05)    COMP-3.             
                   10  FILLER          PIC 9(03)    COMP-3.             
               05  CLLPZG-NATUREZA     PIC X(02).                       
               05  CLLPZG-VCMTO        PIC 9(09)    COMP-3.             
           03  CLLPZG-EMPRESA          PIC 9(05)    COMP-3.             
           03  CLLPZG-AGENCIA          PIC 9(05)    COMP-3.             
           03  CLLPZG-NUM-CC           PIC 9(07)    COMP-3.             
           03  CLLPZG-CARTEIRA         PIC X(03).                       
           03  CLLPZG-CONTRATO         PIC 9(07)    COMP-3.             
           03  CLLPZG-VALOR-RESGATE    PIC 9(11)V99 COMP-3.             
           03  CLLPZG-NOMERESP         PIC X(40).                       
           03  CLLPZG-CGCCPFRESP.                                       
               05  CLLPZG-PRINCIPAL    PIC 9(09)    COMP-3.             
               05  CLLPZG-FILIAL       PIC 9(05)    COMP-3.             
               05  CLLPZG-CONTROLE     PIC 9(03)    COMP-3.             
           03  CLLPZG-ENDER            PIC X(40).                       
           03  CLLPZG-NRO              PIC X(07).                       
           03  CLLPZG-COMPL            PIC X(20).                       
           03  CLLPZG-BAIRRO           PIC X(20).                       
           03  CLLPZG-CIDADE           PIC X(30).                       
           03  CLLPZG-UF               PIC X(02).                       
           03  CLLPZG-CCEP             PIC 9(05).                       
           03  CLLPZG-CCEP-COMPL       PIC 9(03).                       
           03  CLLPZG-IDENT            PIC 9(02).                       
           03  CLLPZG-CAMPO1           PIC X(238).                      
           03  CLLPZG-TIPO             PIC 9(01).                       
           03  CLLPZG-COD-ENTR         PIC 9(01).                       
           03  CLLPZG-NOME-AGENCIA     PIC X(20).                       
           03  CLLPZG-END-AGENCIA      PIC X(25).                       
           03  CLLPZG-CEP-AGENCIA      PIC 9(09)  COMP-3.               
           03  CLLPZG-SIGLA-AGENCIA    PIC X(02).                       
           03  CLLPZG-MUNIC-AGENCIA    PIC X(14).                       
           03  CLLPZG-CD-AGENCIA       PIC 9(05)  COMP-3.               
           03  CLLPZG-COD-BARRA.                                        
             05  CLLPZG-CB-NUMCPF-X.                                    
                07  CLLPZG-CB-NUMCPF  PIC  9(009).                      
                07  CLLPZG-CB-FILCPF  PIC  9(004).                      
                07  CLLPZG-CB-CTRCPF  PIC  9(002).                      
             05  CLLPZG-CB-DT-MOVTO   PIC  9(008).                      
             05  CLLPZG-CB-AGENCIA    PIC  9(005).                      
             05  CLLPZG-CB-TPO-CLIENTE PIC 9(001).                      
      ******************************************************************

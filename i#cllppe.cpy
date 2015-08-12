      ******************************************************************
      *            -  INC  * I#CLLPPE *                                *
      *            ARQUIVO CONTMVTO - LRECL = 300                      *
      *                                                                *
      * REGISTRO CONTROLE DOS MOVIMENTOS ENVIADOS PARA A EMPRESA 'SM'  *
      *                                                                *
      ******************************************************************
      *                                                                 
       01  REG-CONTMVTO.                                                
           02  CMVTO-DTMOVTO          PIC  9(008).                      
           02  CMVTO-AGENCIA          PIC  9(005)   COMP-3.             
           02  CMVTO-CONTA            PIC  9(007)   COMP-3.             
           02  CMVTO-CARTEIRA         PIC  X(003).                      
           02  CMVTO-CONTRATO         PIC  9(007)   COMP-3.             
           02  CMVTO-VCTO             PIC  9(009)   COMP-3.             
           02  CMVTO-TPO-CLI          PIC  X(001).                      
           02  CMVTO-NUMCPF           PIC  9(009)   COMP-3.             
           02  CMVTO-FILCPF           PIC  9(005)   COMP-3.             
           02  CMVTO-CTRCPF           PIC  9(003)   COMP-3.             
           02  CMVTO-NOME             PIC  X(040).                      
           02  CMVTO-TPO-FONE         PIC  X(001).                      
           02  CMVTO-DDD              PIC  9(004).                      
           02  CMVTO-FONE             PIC  9(011).                      
           02  CMVTO-CRET-FONE        PIC  9(001).                      
           02  CMVTO-TPO-LOGRA        PIC  X(001).                      
           02  CMVTO-LOGRA            PIC  X(040).                      
           02  CMVTO-NUM-LOGRA        PIC  X(007).                      
           02  CMVTO-COMPLE-LOGRA     PIC  X(020).                      
           02  CMVTO-BAIRRO           PIC  X(020).                      
           02  CMVTO-CEP              PIC  9(005).                      
           02  CMVTO-COMPLE-CEP       PIC  9(003).                      
           02  CMVTO-CIDADE           PIC  X(025).                      
           02  CMVTO-ESTADO           PIC  X(002).                      
           02  CMVTO-CRET-LOGRA       PIC  9(001).                      
           02  CMVTO-EMISSAO          PIC  X(001).                      
           02  CMVTO-DTULTATU         PIC  X(010).                      
           02  CMVTO-ORIGEM-END       PIC  X(001).                      
           02  CMVTO-CPFCNPJ-DEV.                                       
               03 CMVTO-NUMCPF-DEV    PIC  9(009)   COMP-3.             
               03 CMVTO-FILCPF-DEV    PIC  9(005)   COMP-3.             
               03 CMVTO-CTRCPF-DEV    PIC  9(003)   COMP-3.             
           02  FILLER                 PIC  X(059).                      

      ***************************************************************** 
      * NOME BOOK : I#CLLPH1                                          * 
      * DESCRICAO : LAYOUT PARA O PROGRAMA CLLP0147 ARQUIVO EXPANDIDO * 
      *             DA LEI DA TRANSPARENCIA (100 BYTES)               * 
      *                                                               * 
      * DATA      : 04/04/2014                                        * 
      * AUTOR     : ADRIANO DE SA RANEA                               * 
      * EMPRESA   : BRQ                                               * 
      * TAMANHO   : 675                                               * 
      ***************************************************************** 
        01  CLNV-REGISTRO-ACERAUX      PIC X(675).                      
        01  FILLER     REDEFINES    CLNV-REGISTRO-ACERAUX.              
           03  CLNV-ACCHAVE            PIC X(27).                       
           03  FILLER  REDEFINES     CLNV-ACCHAVE.                      
               05  CLNV-ACTIPO         PIC X(001).                      
               05  CLNV-ACFIRMA        PIC 9(005).                      
               05  CLNV-ACAGENC        PIC 9(005).                      
               05  CLNV-ACNROCL        PIC 9(016).                      
               05  FILLER  REDEFINES   CLNV-ACNROCL.                    
                   07  CLNV-ACNUMBER       PIC 9(012).                  
                   07  CLNV-ACPARC         PIC 9(003).                  
                   07  CLNV-ACDIDAC        PIC 9(001).                  
           03  CLNV-ACRAZCL        PIC 9(005).                          
           03  CLNV-ACCTACL        PIC 9(007).                          
           03  CLNV-ACRAZOR        PIC 9(005).                          
           03  CLNV-ACCTAOR        PIC 9(007).                          
           03  CLNV-ACCORRE        PIC 9(007).                          
           03  CLNV-ACVENCI        PIC 9(008).                          
           03  CLNV-ACENTCL        PIC 9(008).                          
           03  CLNV-ACBAICL        PIC 9(008).                          
           03  CLNV-ACINICI        PIC 9(008).                          
           03  CLNV-ACPROTE        PIC 9(008).                          
           03  CLNV-ACENTLP        PIC 9(008).                          
           03  CLNV-ACBAILP        PIC 9(008).                          
           03  CLNV-ACMOVTO        PIC 9(008).                          
           03  CLNV-ACVTOTRN       PIC 9(008).                          
           03  CLNV-ACSIGBA        PIC 9(004).                          
           03  CLNV-ACOPANO        PIC 9(003).                          
           03  CLNV-ACOPANO-X      REDEFINES                            
               CLNV-ACOPANO        PIC X(003).                          
           03  CLNV-ACOPNUM        PIC 9(007).                          
           03  CLNV-ACVALOP        PIC 9(013)V9(02).                    
           03  CLNV-ACVALR         PIC 9(013)V9(02).                    
           03  CLNV-ACMOEDA        PIC 9(002).                          
           03  CLNV-ACLOCAL        PIC 9(002).                          
           03  CLNV-ACIDCON        PIC 9(001).                          
           03  CLNV-ACTXPUN        PIC 9(001).                          
           03  CLNV-ACICLLP        PIC X(002).                          
           03  CLNV-ACNDEVE        PIC X(040).                          
           03  CLNV-ACCGC          PIC 9(015).                          
           03  CLNV-ACCOMP1        PIC X(001).                          
           03  CLNV-ACNAVA1        PIC X(040).                          
           03  CLNV-ACCGC1         PIC 9(015).                          
           03  CLNV-ACNAVA2        PIC X(040).                          
           03  CLNV-ACCGC2         PIC 9(015).                          
           03  CLNV-ACVRBASE       PIC 9(013)V9(02).                    
           03  CLNV-ACDTREC        PIC 9(008).                          
           03  CLNV-ACSUBSTA       PIC X(001).                          
           03  CLNV-ACCPFADV       PIC 9(009).                          
           03  CLNV-ACCTRADV       PIC 9(002).                          
           03  CLNV-ACCART         PIC 9(003).                          
           03  CLNV-ACCART-X       REDEFINES                            
               CLNV-ACCART         PIC X(003).                          
           03  CLNV-ACTPGARA       PIC 9(002).                          
           03  CLNV-ACIDENT        PIC 9(004).                          
           03  CLNV-ACMARCA        PIC 9(001).                          
           03  CLNV-ACCODG         PIC 9(007).                          
           03  FILLER  REDEFINES   CLNV-ACCODG.                         
               05  CLNV-ACSTREAT       PIC 9(001).                      
               05  CLNV-ACAGRESP       PIC 9(004).                      
               05  CLNV-ACFILL         PIC 9(001).                      
               05  CLNV-ACCOMPBX       PIC X(001).                      
           03  CLNV-ACVRBIOF       PIC 9(013)V9(02).                    
           03  CLNV-ACGARA         PIC X(032).                          
           03  CLNV-ACCODAGE       PIC X(001).                          
           03  CLNV-ACVREVE        PIC 9(013)V9(02).                    
           03  CLNV-ACVREVI        PIC 9(013)V9(02).                    
           03  CLNV-ACVRBXP        PIC 9(013)V9(02).                    
           03  CLNV-ACVRINIC       PIC 9(013)V9(02).                    
           03  CLNV-ACVRIOF        PIC 9(013)V9(02).                    
           03  CLNV-ACVRDEB        PIC 9(013)V9(02).                    
           03  CLNV-ACVRCOBR       PIC 9(013)V9(02).                    
           03  CLNV-ACCDOCOR       PIC 9(002).                          
           03  CLNV-ACBQTRAN       PIC X(001).                          
           03  CLNV-ACDTPGTO       PIC 9(008).                          
           03  CLNV-ACERTOS        PIC X(001).                          
           03  CLNV-ACDEDA         PIC X(002).                          
           03  CLNV-ACPRELP        PIC X(001).                          
           03  CLNV-ACDEBCC        PIC X(001).                          
           03  CLNV-ACORIGEM       PIC X(002).                          
           03  CLNV-ACSOL8273      PIC X(004).                          
           03  CLNV-LEI-T.                                              
              05 CLNV-TAXA-CONTRATO    PIC 9(02)V9(06) USAGE COMP-3.    
              05 CLNV-VR-REMUNERATORIO PIC S9(13)V99 USAGE COMP-3.      
              05 CLNV-VALOR-MORATORIO  PIC S9(13)V99 USAGE COMP-3.      
              05 CLNV-VALOR-MULTA      PIC S9(13)V99 USAGE COMP-3.      
              05 CLNV-DESP-JUD-CUSTAS  PIC S9(11)V99 USAGE COMP-3.      
              05 CLNV-HONORARIOS       PIC S9(11)V99 USAGE COMP-3.      
              05 CLNV-VL-TOTAL-DIVIDA  PIC S9(15)V99 USAGE COMP-3.      
              05 CLNV-VL-TAXA-TARIFA   PIC S9(15)V99 USAGE COMP-3.      
              05 FILLER               PIC  X(39).                       
                                                                        

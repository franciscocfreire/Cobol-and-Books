      ***************************************************************** 
      *  -      INC  ** LPCF0018 **                                   * 
      *  - AREA DE GRAVACAO DO ARQUIVO TRANSFERENCIA DE MORA PARA CL  * 
      *  - LRECL -  295                                               * 
      *  - TODOS CAMPOS DE DATA ESTAO NO FORMATO  DDMMAA              * 
      *  - TODOS CAMPOS DE VALOR ESTAO COM 2 DECIMAIS                 * 
      ***************************************************************** 
       01  ATU-REG.                                                     
           02  ATU-FIXO                PIC  9(002).                     
           02  ATU-AGEN                PIC  9(004).                     
           02  ATU-CONTA               PIC  9(007).                     
           02  ATU-SIGLA               PIC  X(004).                     
           02  ATU-CART                PIC  9(003).                     
           02  ATU-NUMERO              PIC  9(007).                     
           02  ATU-VCTO                PIC  9(008).                     
           02  ATU-DTINIOP             PIC  9(008).                     
           02  ATU-DTMOVTO             PIC  9(008).                     
           02  ATU-MOEDA               PIC  X(002).                     
           02  ATU-TIPGAR              PIC  9(002).                     
           02  ATU-MARCA               PIC  X(001).                     
           02  ATU-MOTIVO              PIC  9(002).                     
           02  ATU-GARANTIA            PIC  X(030).                     
           02  ATU-VRTITULO            PIC  9(013)V99.                  
           02  ATU-VRENCVE             PIC  9(013)V99.                  
           02  ATU-VRENC               PIC  9(013)V99.                  
           02  ATU-VRTXMER             PIC  9(013)V99.                  
           02  ATU-VRIOF               PIC  9(013)V99.                  
           02  ATU-NOME.                                                
               03  ATU-NOME6           PIC  X(006).                     
               03  ATU-NOME34          PIC  X(034).                     
           02  ATU-AVAL1               PIC  X(040).                     
           02  ATU-AVAL2               PIC  X(040).                     
           02  ATU-CGCCPF.                                              
               03  ATU-CGCNUM          PIC  9(009).                     
               03  ATU-CGCFIL          PIC  9(004).                     
               03  ATU-CGCCTR          PIC  9(002).                     
           02  FILLER                  PIC  X(003).                     

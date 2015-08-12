      ***************************************************************** 
      *  -      INC  ** I#CLLPRR **                                   * 
      *  - AREA DE GRAVACAO DO ARQUIVO TRANSFERENCIA DE MORA PARA CL  * 
      *  - LRECL -  401                                               * 
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
           02  ATU-LT.                                                  
               07 ATU-TAXA-CONTRATO       PIC  9(02)V9(06) COMP-3.      
               07 ATU-VR-REMUNERATORIO PIC S9(13)V99 COMP-3.            
               07 ATU-VALOR-MORATORIO     PIC S9(13)V99 COMP-3.         
               07 ATU-VALOR-MULTA         PIC S9(13)V99 COMP-3.         
               07 ATU-DESP-JUD-CUSTAS     PIC S9(11)V99 COMP-3.         
               07 ATU-HONORARIOS          PIC S9(11)V99 COMP-3.         
               07 ATU-VL-TOTAL-DIVIDA     PIC S9(15)V99 COMP-3.         
               07 ATU-VL-TAXA-TARIFA      PIC S9(15)V99 COMP-3.         
               07 ATU-VL-IOF              PIC S9(13)V99 USAGE COMP-3.   
               07 ATU-VL-CORRECAO         PIC S9(13)V99 USAGE COMP-3.   
               07 ATU-VL-JUROS-12PCAA     PIC S9(13)V99 USAGE COMP-3.   
               07 ATU-PERIODICIDADE       PIC S9(02)    USAGE COMP-3.   
               07 FILLER                  PIC  X(13).                   
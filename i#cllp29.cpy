      ***************************************************************** 
      *  -      INC  ** I#CLLP29 **                                   * 
BRQ=A *  - CLONE DA INC I#CLLPIH MUDANDO PREFIXOS                       
      *  - AREA DE GRAVACAO DO ARQUIVO TRANSFERENCIA PARA LP          * 
      *  - LRECL -  301                                               * 
      ***************************************************************** 
       01  ANT-REG.                                                     
           02  ANT-FIXO                PIC  9(002).                     
           02  ANT-AGEN                PIC  9(004).                     
           02  ANT-CONTA               PIC  9(007).                     
           02  ANT-SIGLA               PIC  X(004).                     
BRQ=E******02  ANT-CART                PIC  9(003).                     
BRQ-I      02  ANT-CART                PIC  X(003).                     
           02  ANT-NUMERO              PIC  9(007).                     
           02  ANT-VCTO                PIC  9(008).                     
           02  ANT-DTINIOP             PIC  9(008).                     
           02  ANT-DTMOVTO             PIC  9(008).                     
           02  ANT-MOEDA               PIC  X(002).                     
      *        OBS.- SOMENTE PARA ROTINA EMPF:                          
      *              OS CONTRATOS REFERENTES A CONTA GRAFICA (BBV)      
      *              SERAO IDENTIFICADOS COM 'CG' NESTE CAMPO           
      *              E 'CR' PARA OS DEMAIS CONTRATOS                    
           02  ANT-TIPGAR              PIC  9(002).                     
           02  ANT-MARCA               PIC  X(001).                     
           02  ANT-MOTIVO              PIC  9(002).                     
           02  ANT-GARANTIA            PIC  X(030).                     
           02  ANT-VRTITULO            PIC  9(013)V9(002).              
      *        OBS.- VALOR DO TITULO NA DATA DO VENCIMENTO              
           02  ANT-VR-CONTABIL-BRUTO   PIC  9(013)V9(002).              
      *         OBS.- VALOR DO TITULO + VALOR DO RAP + VALOR  ENCARGOS  
           02  ANT-VRRAP               PIC  9(013)V9(002).              
      *         OBS.- VALOR DO RAP DE 60 DIAS ATE A DATA DO VENCIMENTO  
           02  ANT-VR-ENCARGOS         PIC  9(013)V9(002).              
      *         OBS.- VALOR DOS ENCARGOS ATE 59 DIAS                    
           02  ANT-VRIOF               PIC  9(013)V9(002).              
           02  ANT-NOME.                                                
               03  ANT-NOME6           PIC  X(006).                     
               03  ANT-NOME34          PIC  X(034).                     
           02  ANT-AVAL1               PIC  X(040).                     
           02  ANT-AVAL2               PIC  X(040).                     
           02  ANT-CGCCPF.                                              
               03  ANT-CGCNUM          PIC  9(009).                     
               03  ANT-CGCFIL          PIC  9(004).                     
               03  ANT-CGCCTR          PIC  9(002).                     
           02  ANT-NATOPER             PIC  9(003).                     

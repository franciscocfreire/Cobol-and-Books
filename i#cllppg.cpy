      ******************************************************************
      *            -  INC  * I#CLLPPG *                                *
      *            ARQUIVO CADTHIST - LRECL = 100                      *
      *                                                                *
      *  HISTORICO DE REGISTROS ENVIADOS PARA EMISSAO DE AVISOS DE     *
      *  COBRANCA.                                                     *
      *                                                                *
      ******************************************************************
      *                                                                 
       01  REG-HISTAVIS.                                                
           03  HIST-COD-BARRA.                                          
              05  HIST-NUMCPF-X.                                        
                 07  HIST-NUMCPF     PIC  9(009).                       
                 07  HIST-FILCPF     PIC  9(004).                       
                 07  HIST-CTRCPF     PIC  9(002).                       
              05  HIST-DT-MOVTO      PIC  9(008).                       
              05  HIST-AGENCIA       PIC  9(005).                       
              05  HIST-TPO-CLIENTE   PIC  9(001).                       
           03  HIST-CTA-CORRENT      PIC  9(009).                       
           03  HIST-TPO-ENDEREC      PIC  9(001).                       
           03  HIST-DT-ATULZ         PIC  X(010).                       
           03  HIST-SIT-LOCALIZ      PIC  9(001).                       
           03  HIST-ORIG-ENDEREC     PIC  9(001).                       
           03  HIST-MOT-DEVOLUC      PIC  9(002).                       
           03  HIST-AGENCIA-AVAL     PIC  9(005).                       
           03  HIST-CTA-CORRENT-AVAL PIC  9(009).                       
           03  HIST-NUMCPF-X-DEV.                                       
              05  HIST-NUMCPF-DEV    PIC  9(009).                       
              05  HIST-FILCPF-DEV    PIC  9(004).                       
              05  HIST-CTRCPF-DEV    PIC  9(002).                       
           03  HIST-RESERVA          PIC  X(018).                       

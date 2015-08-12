      *---------------------------------------------------------------* 
      *   PARAMETRO A SER PASSADO P/ A POOL7100 (MODULO TRATA/ ERRO)  * 
      *                    (-INC POL7100C)                            * 
      *---------------------------------------------------------------* 
       01  ERRO-AREA.                                                   
           05  ERR-TIPO-ACESSO        PIC  X(03).                       
           05  ERR-PGM                PIC  X(08).                       
           05  ERR-DADOS-IMS-DB2.                                       
               07  ERR-DBD-TAB        PIC  X(18).                       
               07  ERR-FUN-COMANDO    PIC  X(10).                       
               07  ERR-STA-CODE       PIC  X(04).                       
               07  ERR-SQL-CODE  REDEFINES                              
                   ERR-STA-CODE       PIC S9(09) COMP-4.                
               07  ERR-LOCAL          PIC  X(04).                       
               07  ERR-SEGM           PIC  X(08).                       
               07  FILLER             PIC  X(31).                       
                                                                        
           05  ERR-MQSERIES REDEFINES  ERR-DADOS-IMS-DB2.               
               07  ERR-MQ-FILA        PIC  X(48).                       
               07  ERR-MQ-FUNCAO      PIC  X(10).                       
               07  ERR-MQ-RETURN-CODE PIC S9(09) COMP-4.                
               07  ERR-MQ-REASON-CODE PIC S9(09) COMP-4.                
               07  ERR-MQ-LOCAL       PIC  X(04).                       
               07  FILLER             PIC  X(05).                       
           05  ERR-TEXTO  REDEFINES  ERR-MQSERIES                       
                                      PIC X(75).                        
      *---------------------------------------------------------------* 
      *  OS CPOS ABAIXO DEVEM SER INICIALIZADOS SO' QDO O APLICATIVO  * 
      *  USAR O SISTEMA DE SENHAS (POOL1000).                         * 
      *---------------------------------------------------------------* 
           05  ERR-DADOS-SENHAS.                                        
               07  ERR-COD-USER       PIC  X(07).                       
               07  ERR-COD-DEPTO      PIC  X(06).                       
               07  ERR-MODULO         PIC  X(08)  VALUE SPACES.         

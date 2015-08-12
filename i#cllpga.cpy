      ******************************************************************
      *            -  INC  * I#CLLPGA *                                *
      *  AREA PARA REGISTRO DE DATA  - ARQUIVO - ARQDATA               *
      *            GRAVADO NO PROGRAMA LPCL0001                        *
      *            LRECL  =  60                                        *
      *    ANTIGO  LRECL  =  47                                        *
      *  FORMATO:  DT       (DD/MM/AAAA)  DTMOV    (DDMMAAAA)          *
      *            DTMOVINV (AAAAMMDD)    DTMOVMOD (DDMMAAAA)          *
      *            DTMOVMOI (AAAAMMDD)    CODMOV   (AAAADDD)           *
      *            DTULDIA  (DD/MM/AAAA)  DTULTDIP (DDMMAAAA)          *
      ******************************************************************
       01  WDATA.                                                       
           03  DT                      PIC  X(10).                      
           03  DTMOV.                                                   
               05  DTDIA               PIC  9(02).                      
               05  DTMES               PIC  9(02).                      
               05  DTSECULO            PIC  9(02).                      
               05  DTANO               PIC  9(02).                      
           03  DTMOVINV                PIC  9(08).                      
           03  DTMOVMOD                PIC  9(09)  COMP-3.              
           03  DTMOVMOI                PIC  9(09)  COMP-3.              
           03  CODMOV                  PIC  9(07)  COMP-3.              
           03  DTULDIA                 PIC  X(10).                      
           03  DTULTDIP                PIC  9(09)  COMP-3.              
           03  DTDIA20                 PIC  9(09)  COMP-3.              

      ***************************************************************** 
      *    AREA                    : SOC-REGISTRO                       
      *                              I#RDAB17                           
      *    AUTOR                   : CARMEN                             
      *    DATA ULTIMA ATUALIZACAO : 16/03/2004.                        
      *    OBJETIVO                :                                    
      *    TAMANHO                 : 223 (BYTES)                        
      ***************************************************************** 
       01  SOC-REGISTRO.                                                
           10 SOC-CCLUB-CADTR            PIC 9(10).
           10 SOC-CCLUB                  PIC 9(10).
           10 FILLER                     PIC X(06).
           10 SOC-CGCNUM-EMPRESA         PIC 9(09) COMP-3.              
           10 SOC-CGCFIL-EMPRESA         PIC 9(05) COMP-3.              
           10 SOC-CGCNUM                 PIC 9(09) COMP-3.              
           10 SOC-CGCFIL                 PIC 9(05) COMP-3.              
           10 SOC-CGCCTR                 PIC 9(03) COMP-3.              
           10 SOC-NOME                   PIC X(40).                     
           10 SOC-RAMO-ATVDD             PIC 9(05) COMP-3.              
           10 SOC-ELOGDR                 PIC X(40).                     
           10 SOC-ENRO-LOGDR             PIC X(07).                     
           10 SOC-ECOMPL-LOGDR           PIC X(20).                     
           10 SOC-EBAIRO                 PIC X(20).                     
           10 SOC-CCEP                   PIC 9(05) COMP-3.              
           10 SOC-CCEP-COMPL             PIC 9(03) COMP-3.              
           10 SOC-RMUN                   PIC X(25).                     
           10 SOC-CSGL-UF                PIC X(02).                     
           10 SOC-CDDD                   PIC X(04).                     
BRQ059     10 SOC-CFONE                  PIC X(11).                     
           10 SOC-CLETRA-RTING           PIC X(02).                     

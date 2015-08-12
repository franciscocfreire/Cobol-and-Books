      ******************************************************************
      *    AREA                    : ENT-FICAV000                      *
      *                              I#FICAC6                          *
      *    AUTOR                   : RODRIGO LOPES                     *
      *    DATA ULTIMA ATUALIZACAO : 18/10/2000.                       *
      *    OBJETIVO                : IMAGEM DA TABELA FICAV000         *
      *    TAMANHO                 : 339 (BYTES)                       *
      *----------------------------------------------------------------*
ALTF  *    DATA ULTIMA ATUALIZACAO : 03/04/2012.                       *
-     *    PROGRAMADOR             : RODRIGO MATEU                     *
-     *    ANALISTA                : FELIPE NERI                       *
-     *    OBJETIVO                : ALTERACAO TELEFONE (9 DIGITOS)    *
ALTF  *    TAMANHO                 : 346 (BYTES)                       *
      ******************************************************************
       01  ENT-FICAV000.                                                
           10 V000-E-CPSSOA-CADTR         PIC X(26).                    
           10 V000-E-CPSSOA               PIC X(26).                    
           10 V000-E-DINCL                PIC X(10).                    
           10 V000-E-CTPO-PSSOA-CADTR     PIC X(01).                    
           10 V000-E-DCTRL-OPER           PIC X(10).                    
           10 V000-E-CEMIS-EXTRT-DEB      PIC S9(1)  COMP-3.            
           10 V000-E-DULTMA-ATULZ-DSTNO   PIC X(10).                    
           10 V000-E-DULTMA-ATULZ-DSTNO-NN PIC X(1).                    
           10 V000-E-CSIT-FICHA-CAD       PIC S9(1)  COMP-3.            
           10 V000-E-CFUNC-BDSCO          PIC S9(9)  COMP-3.            
           10 V000-E-CANAL-FICHA-CAD      PIC X(01).                    
           10 V000-E-CANAL-FICHA-CAD-NN   PIC X(01).                    
           10 V000-E-CSTTUS-FICHA-CAD     PIC X(02).                    
           10 V000-E-CIND-IMPED-REST      PIC X(01).                    
           10 V000-E-RREFT-BCRIA          PIC X(40).                    
           10 V000-E-RREFT-BCRIA-NN       PIC X(01).                    
           10 V000-E-VQUITD-NAO-COMPV     PIC S9(15)  COMP-3.           
           10 V000-E-VTOT-IMOV-ONUS       PIC S9(15)  COMP-3.           
           10 V000-E-VTOT-IMOV-COMPV      PIC S9(15)  COMP-3.           
           10 V000-E-CDDD                 PIC X(04).                    
           10 V000-E-CFONE                PIC S9(8)  COMP-3.            
           10 V000-E-CISENC-DEB-CAD       PIC X(01).                    
           10 V000-E-CJUNC-DEPDC          PIC S9(5)  COMP-3.            
           10 V000-E-CDIG-DEPDC           PIC X(01).                    
           10 V000-E-CCTA-CORR            PIC S9(7)  COMP-3.            
           10 V000-E-CDIG-CTA-CORR        PIC X(01).                    
           10 V000-E-CCEP                 PIC S9(5)  COMP-3.            
           10 V000-E-CCEP-COMPL           PIC S9(3)  COMP-3.            
           10 V000-E-CCEP-COMPL-NN        PIC X(01).                    
           10 V000-E-DABERT-CTA-CORR      PIC X(10).                    
           10 V000-E-DABERT-CTA-CORR-NN   PIC X(01).                    
           10 V000-E-IMUN                 PIC X(30).                    
           10 V000-E-ELOGDR               PIC X(40).                    
           10 V000-E-ENRO-LOGDR           PIC S9(5)  COMP-3.            
           10 V000-E-ECOMPL-LOGDR         PIC X(10).                    
           10 V000-E-CAUTRZ-CONS          PIC X(01).                    
           10 V000-E-CAUTRZ-CONS-NN       PIC X(01).                    
           10 V000-E-HULTMA-ATULZ-CONS    PIC X(26).                    
           10 V000-E-HULTMA-ATULZ-CON-NN  PIC X(01).                    
           10 V000-E-DENVIO-CAD-CREDT     PIC X(10).                    
           10 V000-E-DENVIO-CAD-CREDT-NN  PIC X(01).                    
           10 V000-E-CORIGE-FICHA         PIC S9(5)V USAGE COMP-3.      
           10 V000-E-CORIGE-FICHA-NN      PIC X(01).                    
           10 V000-E-CSGL-UF-COML         PIC X(02).                    
           10 V000-E-CSGL-UF-COML-NN      PIC X(01).                    
           10 V000-E-CTPO-CATAO-CREDT     PIC S9(3)V USAGE COMP-3.      
           10 V000-E-CTPO-CATAO-CREDT-NN  PIC X(01).                    
           10 V000-E-CPORTE-EMPR          PIC S9(3)V USAGE COMP-3.      
           10 V000-E-CPORTE-EMPR-NN       PIC X(01).                    
           10 V000-E-CFUNC-ISENC-COBR     PIC S9(9)V USAGE COMP-3.      
           10 V000-E-CFUNC-ISENC-COBR-NN  PIC X(01).                    
ALTF       10 V000-E-CFONE-NOVO           PIC S9(11)  COMP-3.           
ALTF       10 V000-E-CFONE-NOVO-NN        PIC X(01).                    

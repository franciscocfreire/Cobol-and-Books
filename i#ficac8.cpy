      ******************************************************************
      *    AREA                    : ENT-FICAV001                      *
      *                              I#FICAC8                          *
      *    AUTOR                   : RODRIGO LOPES                     *
      *    DATA ULTIMA ATUALIZACAO : 18/10/2000.                       *
      *    OBJETIVO                : IMAGEM DA TABELA FICAV001         *
      *    TAMANHO                 : 516 (BYTES)                       *
      *----------------------------------------------------------------*
ALTF  *    DATA ULTIMA ATUALIZACAO : 04/04/2012.                       *
-     *    PROGRAMADOR             : RODRIGO MATEU                     *
-     *    ANALISTA                : FELIPE NERI                       *
-     *    OBJETIVO                : ALTERACAO TELEFONE (9 DIGITOS)    *
ALTF  *    TAMANHO                 : 523 (BYTES)                       *
      ******************************************************************
       01  ENT-FICAV001.                                                
           10 V001-E-CPSSOA-CADTR         PIC X(26).                    
           10 V001-E-DNASC                PIC X(10).                    
           10 V001-E-CID-EST-CVIL         PIC S9(1)  COMP-3.            
           10 V001-E-IMAE                 PIC X(40).                    
           10 V001-E-IPAI                 PIC X(40).                    
           10 V001-E-QDEPDT               PIC S9(2)  COMP-3.            
           10 V001-E-DRESID-MUN           PIC X(10).                    
           10 V001-E-CTPO-FICHA-CADTR     PIC X(1).                     
           10 V001-E-CCEP-RESID           PIC S9(5)  COMP-3.            
           10 V001-E-IEMPR-TRAB           PIC X(40).                    
           10 V001-E-TSERVC-PSSOA-FIS     PIC X(2).                     
           10 V001-E-IPROFS               PIC X(40).                    
           10 V001-E-ICARGO               PIC X(40).                    
           10 V001-E-CNAC                 PIC S9(3)  COMP-3.            
           10 V001-E-CDDD-RESID           PIC X(4).                     
           10 V001-E-CFONE-RESID          PIC S9(8)  COMP-3.            
           10 V001-E-VALUG-PREST-SFH      PIC S9(15)  COMP-3.           
           10 V001-E-VTOT-OUTRO-RENDA     PIC S9(15)  COMP-3.           
           10 V001-E-VSALRL-MES           PIC S9(15)  COMP-3.           
           10 V001-E-RPROC-OUTRA-RENDA    PIC X(40).                    
           10 V001-E-IPROFS-ANTER         PIC X(40).                    
           10 V001-E-CCEP-COMPL-RESID     PIC S9(3)  COMP-3.            
           10 V001-E-CCEP-COMPL-RESID-NN  PIC  X(1).                    
           10 V001-E-IMUN-PROC            PIC X(30).                    
           10 V001-E-IMUN-RESID           PIC X(30).                    
           10 V001-E-ELOGDR               PIC X(40).                    
           10 V001-E-ENRO-LOGDR           PIC S9(5)  COMP-3.            
           10 V001-E-ECOMPL-LOGDR         PIC X(10).                    
           10 V001-E-CSGL-UF-PROC         PIC X(2).                     
           10 V001-E-DADMIS               PIC X(10).                    
           10 V001-E-DADMIS-NN            PIC X(01).                    
           10 V001-E-CCATEG-PROFS         PIC S9(3)  COMP-3.            
           10 V001-E-CCATEG-PROFS-NN      PIC X(1).                     
           10 V001-E-CTPO-RESID           PIC S9(1)  COMP-3.            
           10 V001-E-CTPO-RESID-NN        PIC X(1).                     
           10 V001-E-CSGL-UF-RESID        PIC X(2).                     
           10 V001-E-CSGL-UF-RESID-NN     PIC X(1).                     
           10 V001-E-CTPO-FONE            PIC S9(3)  COMP-3.            
           10 V001-E-CTPO-FONE-NN         PIC X(1).                     
           10 V001-E-COCUPC-PF            PIC S9(3)  COMP-3.            
           10 V001-E-COCUPC-PF-NN         PIC X(1).                     
           10 V001-E-CPROFS-ANLSE         PIC S9(3)  COMP-3.            
           10 V001-E-CPROFS-ANLSE-NN      PIC X(1).                     
ALTF       10 V001-E-CFONE-RESID-NOVO     PIC S9(11)  COMP-3.           
ALTF       10 V001-E-CFONE-RESID-NOVO-NN  PIC X(1).                     

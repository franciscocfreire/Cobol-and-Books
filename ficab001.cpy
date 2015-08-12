      ******************************************************************
      * DCLGEN TABLE(DB2PRD.PESSOA_FIS_CADTR)                          *
      *        LIBRARY(AD.DB2.DCLGEN(FICAB001))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(FICAB001)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.PESSOA_FIS_CADTR TABLE               
           ( CPSSOA_CADTR                   TIMESTAMP NOT NULL,         
             DNASC                          DATE NOT NULL,              
             CID_EST_CVIL                   DECIMAL(1, 0) NOT NULL,     
             IMAE                           CHAR(40) NOT NULL,          
             IPAI                           CHAR(40) NOT NULL,          
             QDEPDT                         DECIMAL(2, 0) NOT NULL,     
             DRESID_MUN                     DATE NOT NULL,              
             CTPO_FICHA_CADTR               CHAR(1) NOT NULL,           
             CCEP_RESID                     DECIMAL(5, 0) NOT NULL,     
             IEMPR_TRAB                     CHAR(40) NOT NULL,          
             TSERVC_PSSOA_FIS               CHAR(2) NOT NULL,           
             IPROFS                         CHAR(40) NOT NULL,          
             ICARGO                         CHAR(40) NOT NULL,          
             CNAC                           DECIMAL(3, 0) NOT NULL,     
             CDDD_RESID                     CHAR(4) NOT NULL,           
             CFONE_RESID                    DECIMAL(8, 0) NOT NULL,     
             VALUG_PREST_SFH                DECIMAL(15, 0) NOT NULL,    
             VTOT_OUTRO_RENDA               DECIMAL(15, 0) NOT NULL,    
             VSALRL_MES                     DECIMAL(15, 0) NOT NULL,    
             RPROC_OUTRA_RENDA              CHAR(40) NOT NULL,          
             IPROFS_ANTER                   CHAR(40) NOT NULL,          
             CCEP_COMPL_RESID               DECIMAL(3, 0),              
             IMUN_PROC                      CHAR(30) NOT NULL,          
             IMUN_RESID                     CHAR(30) NOT NULL,          
             ELOGDR                         CHAR(40) NOT NULL,          
             ENRO_LOGDR                     DECIMAL(5, 0) NOT NULL,     
             ECOMPL_LOGDR                   CHAR(10) NOT NULL,          
             CSGL_UF_PROC                   CHAR(2) NOT NULL,           
             DADMIS                         DATE,                       
             CCATEG_PROFS                   DECIMAL(3, 0),              
             CTPO_RESID                     DECIMAL(1, 0),              
             CSGL_UF_RESID                  CHAR(2),                    
             CTPO_FONE                      DECIMAL(3, 0),              
             COCUPC_PF                      DECIMAL(3, 0),              
             CPROFS_ANLSE                   DECIMAL(3, 0),              
             NLIN_TFONI_RESID               DECIMAL(11, 0)              
           ) END-EXEC.                                                  
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.PESSOA_FIS_CADTR            *
      ******************************************************************
       01  FICAB001.                                                    
      *    *************************************************************
           10 CPSSOA-CADTR         PIC X(26).                           
      *    *************************************************************
           10 DNASC                PIC X(10).                           
      *    *************************************************************
           10 CID-EST-CVIL         PIC S9(1)V USAGE COMP-3.             
      *    *************************************************************
           10 IMAE                 PIC X(40).                           
      *    *************************************************************
           10 IPAI                 PIC X(40).                           
      *    *************************************************************
           10 QDEPDT               PIC S9(2)V USAGE COMP-3.             
      *    *************************************************************
           10 DRESID-MUN           PIC X(10).                           
      *    *************************************************************
           10 CTPO-FICHA-CADTR     PIC X(1).                            
      *    *************************************************************
           10 CCEP-RESID           PIC S9(5)V USAGE COMP-3.             
      *    *************************************************************
           10 IEMPR-TRAB           PIC X(40).                           
      *    *************************************************************
           10 TSERVC-PSSOA-FIS     PIC X(2).                            
      *    *************************************************************
           10 IPROFS               PIC X(40).                           
      *    *************************************************************
           10 ICARGO               PIC X(40).                           
      *    *************************************************************
           10 CNAC                 PIC S9(3)V USAGE COMP-3.             
      *    *************************************************************
           10 CDDD-RESID           PIC X(4).                            
      *    *************************************************************
           10 CFONE-RESID          PIC S9(8)V USAGE COMP-3.             
      *    *************************************************************
           10 VALUG-PREST-SFH      PIC S9(15)V USAGE COMP-3.            
      *    *************************************************************
           10 VTOT-OUTRO-RENDA     PIC S9(15)V USAGE COMP-3.            
      *    *************************************************************
           10 VSALRL-MES           PIC S9(15)V USAGE COMP-3.            
      *    *************************************************************
           10 RPROC-OUTRA-RENDA    PIC X(40).                           
      *    *************************************************************
           10 IPROFS-ANTER         PIC X(40).                           
      *    *************************************************************
           10 CCEP-COMPL-RESID     PIC S9(3)V USAGE COMP-3.             
      *    *************************************************************
           10 IMUN-PROC            PIC X(30).                           
      *    *************************************************************
           10 IMUN-RESID           PIC X(30).                           
      *    *************************************************************
           10 ELOGDR               PIC X(40).                           
      *    *************************************************************
           10 ENRO-LOGDR           PIC S9(5)V USAGE COMP-3.             
      *    *************************************************************
           10 ECOMPL-LOGDR         PIC X(10).                           
      *    *************************************************************
           10 CSGL-UF-PROC         PIC X(2).                            
      *    *************************************************************
           10 DADMIS               PIC X(10).                           
      *    *************************************************************
           10 CCATEG-PROFS         PIC S9(3)V USAGE COMP-3.             
      *    *************************************************************
           10 CTPO-RESID           PIC S9(1)V USAGE COMP-3.             
      *    *************************************************************
           10 CSGL-UF-RESID        PIC X(2).                            
      *    *************************************************************
           10 CTPO-FONE            PIC S9(3)V USAGE COMP-3.             
      *    *************************************************************
           10 COCUPC-PF            PIC S9(3)V USAGE COMP-3.             
      *    *************************************************************
           10 CPROFS-ANLSE         PIC S9(3)V USAGE COMP-3.             
      *    *************************************************************
           10 NLIN-TFONI-RESID     PIC S9(11)V USAGE COMP-3.            
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 36      *
      ******************************************************************

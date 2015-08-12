      ******************************************************************
      * DCLGEN TABLE(DB2PRD.FONE_BASE_UNIC)                            *
      *        LIBRARY(AD.DB2.DCLGEN(RDABB005))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(RDABB005)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.FONE_BASE_UNIC TABLE                 
           ( CBCO                           DECIMAL(3, 0) NOT NULL,     
             CAG_BCRIA                      DECIMAL(5, 0) NOT NULL,     
             CCTA_BCRIA_CLI                 DECIMAL(13, 0) NOT NULL,    
             CINDCD_ORIGE_FONE              CHAR(1) NOT NULL,           
             CINDCD_TPO_FONE                CHAR(1) NOT NULL,           
             CSEQ_FONE_BASE                 DECIMAL(2, 0) NOT NULL,     
             CDDD_CLI_BASE_UNIC             CHAR(4) NOT NULL,           
             CFONE_CLI_BASE                 CHAR(11) NOT NULL,          
             CINDCD_FONE_INCOR              CHAR(1) NOT NULL,           
             CINDCD_ATULZ_BASE              DECIMAL(1, 0) NOT NULL,     
             DULT_ATULZ_BASE                DATE NOT NULL,              
             CSIT_RETOR_FONE                CHAR(1) NOT NULL            
           ) END-EXEC.                                                  
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.FONE_BASE_UNIC              *
      ******************************************************************
       01  RDABB005.                                                    
      *    *************************************************************
           10 CBCO                 PIC S9(3)V USAGE COMP-3.             
      *    *************************************************************
           10 CAG-BCRIA            PIC S9(5)V USAGE COMP-3.             
      *    *************************************************************
           10 CCTA-BCRIA-CLI       PIC S9(13)V USAGE COMP-3.            
      *    *************************************************************
           10 CINDCD-ORIGE-FONE    PIC X(1).                            
      *    *************************************************************
           10 CINDCD-TPO-FONE      PIC X(1).                            
      *    *************************************************************
           10 CSEQ-FONE-BASE       PIC S9(2)V USAGE COMP-3.             
      *    *************************************************************
           10 CDDD-CLI-BASE-UNIC   PIC X(4).                            
      *    *************************************************************
           10 CFONE-CLI-BASE       PIC X(11).                           
      *    *************************************************************
           10 CINDCD-FONE-INCOR    PIC X(1).                            
      *    *************************************************************
           10 CINDCD-ATULZ-BASE    PIC S9(1)V USAGE COMP-3.             
      *    *************************************************************
           10 DULT-ATULZ-BASE      PIC X(10).                           
      *    *************************************************************
           10 CSIT-RETOR-FONE      PIC X(1).                            
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 12      *
      ******************************************************************

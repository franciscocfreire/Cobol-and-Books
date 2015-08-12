      ******************************************************************
      * DCLGEN TABLE(DB2PRD.LOGDR_BASE_UNIC)                           *
      *        LIBRARY(AD.DB2.DCLGEN(RDABB006))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(RDABB006)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.LOGDR_BASE_UNIC TABLE                
           ( CBCO                           DECIMAL(3, 0) NOT NULL,     
             CAG_BCRIA                      DECIMAL(5, 0) NOT NULL,     
             CCTA_BCRIA_CLI                 DECIMAL(13, 0) NOT NULL,    
             CINDCD_ORIGE_LOGDR             CHAR(1) NOT NULL,           
             CINDCD_TPO_LOGDR               CHAR(1) NOT NULL,           
             ELOGDR_CLI_BASE                CHAR(40) NOT NULL,          
             ENRO_LOGDR_BASE                CHAR(7) NOT NULL,           
             ECOMPL_LOGDR_BASE              CHAR(20) NOT NULL,          
             EBAIRO_LOGDR_BASE              CHAR(20) NOT NULL,          
             RMUN_BASE                      CHAR(25) NOT NULL,          
             CCEP_CLI_BASE                  DECIMAL(5, 0) NOT NULL,     
             CCEP_COMPL_BASE                DECIMAL(3, 0) NOT NULL,     
             CSGL_UF_CLI_BASE               CHAR(2) NOT NULL,           
             CINDCD_LOGDR_INCOR             CHAR(1) NOT NULL,           
             CINDCD_ATULZ_BASE              DECIMAL(1, 0) NOT NULL,     
             DULT_ATULZ_BASE                DATE NOT NULL,              
             CSIT_RETOR_LOGDR               CHAR(1) NOT NULL            
           ) END-EXEC.                                                  
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.LOGDR_BASE_UNIC             *
      ******************************************************************
       01  RDABB006.                                                    
      *    *************************************************************
           10 CBCO                 PIC S9(3)V USAGE COMP-3.             
      *    *************************************************************
           10 CAG-BCRIA            PIC S9(5)V USAGE COMP-3.             
      *    *************************************************************
           10 CCTA-BCRIA-CLI       PIC S9(13)V USAGE COMP-3.            
      *    *************************************************************
           10 CINDCD-ORIGE-LOGDR   PIC X(1).                            
      *    *************************************************************
           10 CINDCD-TPO-LOGDR     PIC X(1).                            
      *    *************************************************************
           10 ELOGDR-CLI-BASE      PIC X(40).                           
      *    *************************************************************
           10 ENRO-LOGDR-BASE      PIC X(7).                            
      *    *************************************************************
           10 ECOMPL-LOGDR-BASE    PIC X(20).                           
      *    *************************************************************
           10 EBAIRO-LOGDR-BASE    PIC X(20).                           
      *    *************************************************************
           10 RMUN-BASE            PIC X(25).                           
      *    *************************************************************
           10 CCEP-CLI-BASE        PIC S9(5)V USAGE COMP-3.             
      *    *************************************************************
           10 CCEP-COMPL-BASE      PIC S9(3)V USAGE COMP-3.             
      *    *************************************************************
           10 CSGL-UF-CLI-BASE     PIC X(2).                            
      *    *************************************************************
           10 CINDCD-LOGDR-INCOR   PIC X(1).                            
      *    *************************************************************
           10 CINDCD-ATULZ-BASE    PIC S9(1)V USAGE COMP-3.             
      *    *************************************************************
           10 DULT-ATULZ-BASE      PIC X(10).                           
      *    *************************************************************
           10 CSIT-RETOR-LOGDR     PIC X(1).                            
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 17      *
      ******************************************************************

      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TPRODT_SERVC_CTBIL)                        *
      *        LIBRARY(AD.DB2.DCLGEN(FXCGB059))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(FXCGB059)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TPRODT_SERVC_CTBIL TABLE             
           ( NPRODT_SERVC_CTBIL             DECIMAL(9, 0) NOT NULL,     
             CPRODT_SERVC                   DECIMAL(8, 0),              
             CPRODT_SERVC_ADMTV             DECIMAL(8, 0),              
             CPRODT_SERVC_EXTER             DECIMAL(8, 0)               
           ) END-EXEC.                                                  
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TPRODT_SERVC_CTBIL          *
      ******************************************************************
       01  FXCGB059.                                                    
      *    *************************************************************
           10 NPRODT-SERVC-CTBIL   PIC S9(9)V USAGE COMP-3.             
      *    *************************************************************
           10 CPRODT-SERVC         PIC S9(8)V USAGE COMP-3.             
      *    *************************************************************
           10 CPRODT-SERVC-ADMTV   PIC S9(8)V USAGE COMP-3.             
      *    *************************************************************
           10 CPRODT-SERVC-EXTER   PIC S9(8)V USAGE COMP-3.             
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 4       *
      ******************************************************************

      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TTPO_PROCS_EXTER)                          *
      *        LIBRARY(AD.DB2.DCLGEN(SCSBB045))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(SCSBB045)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TTPO_PROCS_EXTER TABLE               
           ( CTPO_PROCS_EXTER               DECIMAL(4, 0) NOT NULL,     
             CPSSOA_UND_ORGNZ               DECIMAL(10, 0) NOT NULL,    
             NSEQ_UND_ORGNZ                 DECIMAL(8, 0) NOT NULL      
           ) END-EXEC.                                                  
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TTPO_PROCS_EXTER            *
      ******************************************************************
       01  SCSBB045.                                                    
      *    *************************************************************
           10 CTPO-PROCS-EXTER     PIC S9(4)V USAGE COMP-3.             
      *    *************************************************************
           10 CPSSOA-UND-ORGNZ     PIC S9(10)V USAGE COMP-3.            
      *    *************************************************************
           10 NSEQ-UND-ORGNZ       PIC S9(8)V USAGE COMP-3.             
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 3       *
      ******************************************************************

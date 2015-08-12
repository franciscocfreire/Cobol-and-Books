      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TGSTAO_CORP_EXTER)                         *
      *        LIBRARY(AD.DB2.DCLGEN(SCSBB026))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(SCSBB026)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TGSTAO_CORP_EXTER TABLE              
           ( CPSSOA_UND_ORGNZ               DECIMAL(10, 0) NOT NULL,    
             NSEQ_UND_ORGNZ                 DECIMAL(8, 0) NOT NULL,     
             CPSSOA_JURID                   DECIMAL(10, 0) NOT NULL,    
             CINDCD_EMPR_MSTER              DECIMAL(1, 0) NOT NULL,     
             DBLOQ_EMPR_EXTER               DATE                        
           ) END-EXEC.                                                  
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TGSTAO_CORP_EXTER           *
      ******************************************************************
       01  SCSBB026.                                                    
      *    *************************************************************
           10 CPSSOA-UND-ORGNZ     PIC S9(10)V USAGE COMP-3.            
      *    *************************************************************
           10 NSEQ-UND-ORGNZ       PIC S9(8)V USAGE COMP-3.             
      *    *************************************************************
           10 CPSSOA-JURID         PIC S9(10)V USAGE COMP-3.            
      *    *************************************************************
           10 CINDCD-EMPR-MSTER    PIC S9(1)V USAGE COMP-3.             
      *    *************************************************************
           10 DBLOQ-EMPR-EXTER     PIC X(10).                           
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 5       *
      ******************************************************************

      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TPRODT_SERVC_ADMTV)                        *
      *        LIBRARY(AD.DB2.DCLGEN(FXCGB044))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(FXCGB044)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TPRODT_SERVC_ADMTV TABLE             
           ( CPRODT_SERVC_ADMTV             DECIMAL(8, 0) NOT NULL,     
             RPRODT_SERVC                   CHAR(100) NOT NULL,         
             RRSUMO_PRODT_SERVC             CHAR(30) NOT NULL,          
             CPSSOA_JURID_GTORA             DECIMAL(10, 0) NOT NULL,    
             NSEQ_UND_GTORA                 DECIMAL(8, 0) NOT NULL,     
             CPSSOA_JURID_CONGL             DECIMAL(10, 0) NOT NULL,    
             CUSUAR_INCL                    CHAR(9) NOT NULL,           
             HINCL_REG                      TIMESTAMP NOT NULL,         
             CUSUAR_MANUT                   CHAR(9) NOT NULL,           
             HMANUT_REG                     TIMESTAMP NOT NULL          
           ) END-EXEC.                                                  
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TPRODT_SERVC_ADMTV          *
      ******************************************************************
       01  FXCGB044.                                                    
      *    *************************************************************
           10 CPRODT-SERVC-ADMTV   PIC S9(8)V USAGE COMP-3.             
      *    *************************************************************
           10 RPRODT-SERVC         PIC X(100).                          
      *    *************************************************************
           10 RRSUMO-PRODT-SERVC   PIC X(30).                           
      *    *************************************************************
           10 CPSSOA-JURID-GTORA   PIC S9(10)V USAGE COMP-3.            
      *    *************************************************************
           10 NSEQ-UND-GTORA       PIC S9(8)V USAGE COMP-3.             
      *    *************************************************************
           10 CPSSOA-JURID-CONGL   PIC S9(10)V USAGE COMP-3.            
      *    *************************************************************
           10 CUSUAR-INCL          PIC X(9).                            
      *    *************************************************************
           10 HINCL-REG            PIC X(26).                           
      *    *************************************************************
           10 CUSUAR-MANUT         PIC X(9).                            
      *    *************************************************************
           10 HMANUT-REG           PIC X(26).                           
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 10      *
      ******************************************************************

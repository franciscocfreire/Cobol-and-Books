      ******************************************************************
      * DCLGEN TABLE(DB2PRD.CTRL_ROTNA_RDAB)                           *
      *        LIBRARY(AD.DB2.DCLGEN(RDABB000))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(RDABB000)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.CTRL_ROTNA_RDAB TABLE                
           ( CPROG                          CHAR(8) NOT NULL,           
             CJOB                           CHAR(8) NOT NULL,           
             QREG_COMIT                     DECIMAL(5, 0) NOT NULL,     
             CSIT_RESTT                     CHAR(1) NOT NULL,           
             CUSUAR_SENHA                   CHAR(7),                    
             HATULZ                         TIMESTAMP,                  
             WCHAVE_RESTT                   VARCHAR(250) NOT NULL       
           ) END-EXEC.                                                  
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.CTRL_ROTNA_RDAB             *
      ******************************************************************
       01  RDABB000.                                                    
      *    *************************************************************
           10 CPROG                PIC X(8).                            
      *    *************************************************************
           10 CJOB                 PIC X(8).                            
      *    *************************************************************
           10 QREG-COMIT           PIC S9(5)V USAGE COMP-3.             
      *    *************************************************************
           10 CSIT-RESTT           PIC X(1).                            
      *    *************************************************************
           10 CUSUAR-SENHA         PIC X(7).                            
      *    *************************************************************
           10 HATULZ               PIC X(26).                           
      *    *************************************************************
           10 WCHAVE-RESTT.                                             
              49 WCHAVE-RESTT-LEN  PIC S9(4) USAGE COMP.                
              49 WCHAVE-RESTT-TEXT  PIC X(250).                         
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 7       *
      ******************************************************************

      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TPAIS_GEOGR_RECTA)                         *
      *        LIBRARY(AD.DB2.DCLGEN(SCSBB011))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(SCSBB011)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TPAIS_GEOGR_RECTA TABLE              
           ( CPAIS                          DECIMAL(3, 0) NOT NULL,     
             CPAIS_RECTA_FEDRL              DECIMAL(3, 0) NOT NULL,     
             IPAIS                          CHAR(60) NOT NULL           
           ) END-EXEC.                                                  
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TPAIS_GEOGR_RECTA           *
      ******************************************************************
       01  SCSBB011.                                                    
      *    *************************************************************
           10 CPAIS                PIC S9(3)V USAGE COMP-3.             
      *    *************************************************************
           10 CPAIS-RECTA-FEDRL    PIC S9(3)V USAGE COMP-3.             
      *    *************************************************************
           10 IPAIS                PIC X(60).                           
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 3       *
      ******************************************************************

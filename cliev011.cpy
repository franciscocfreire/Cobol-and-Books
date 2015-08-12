      ******************************************************************
      * DCLGEN TABLE(DB2PRD.V01FAMILIA_CLI_PF)                         *
      *        LIBRARY(AD.DB2.DCLGEN(CLIEV011))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(CLIEV011)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.V01FAMILIA_CLI_PF TABLE              
           ( CID_CLI                        CHAR(26) NOT NULL,          
             CPARNT                         CHAR(1) NOT NULL,           
             IPSSOA_ABREV                   CHAR(40) NOT NULL,          
             CPREFX_NOME_PSSOA              CHAR(5) NOT NULL            
           ) END-EXEC.                                                  
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.V01FAMILIA_CLI_PF           *
      ******************************************************************
       01  CLIEV011.                                                    
      *    *************************************************************
           10 CID-CLI              PIC X(26).                           
      *    *************************************************************
           10 CPARNT               PIC X(1).                            
      *    *************************************************************
           10 IPSSOA-ABREV         PIC X(40).                           
      *    *************************************************************
           10 CPREFX-NOME-PSSOA    PIC X(5).                            
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 4       *
      ******************************************************************

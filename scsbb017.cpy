      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TSERVC_VAR_PATRM)                          *
      *        LIBRARY(AD.DB2.DCLGEN(SCSBB017))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(SCSBB017)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TSERVC_VAR_PATRM TABLE               
           ( CSERVC_VAR_PATRM               CHAR(9) NOT NULL,           
             RSERVC_VAR_PATRM               CHAR(250) NOT NULL,         
             DINIC_SERVC_VAR                DATE NOT NULL,              
             DFIM_SERVC_VAR                 DATE NOT NULL,              
             CINDCD_EXIBC_SERVC             DECIMAL(1, 0) NOT NULL      
           ) END-EXEC.                                                  
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TSERVC_VAR_PATRM            *
      ******************************************************************
       01  SCSBB017.                                                    
      *    *************************************************************
           10 CSERVC-VAR-PATRM     PIC X(9).                            
      *    *************************************************************
           10 RSERVC-VAR-PATRM     PIC X(250).                          
      *    *************************************************************
           10 DINIC-SERVC-VAR      PIC X(10).                           
      *    *************************************************************
           10 DFIM-SERVC-VAR       PIC X(10).                           
      *    *************************************************************
           10 CINDCD-EXIBC-SERVC   PIC S9(1)V USAGE COMP-3.             
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 5       *
      ******************************************************************

      ******************************************************************
      * DCLGEN TABLE(DB2PRD.V01CLIENTE_AG_PF)                          *
      *        LIBRARY(AD.DB2.DCLGEN(CLIEV006))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(CLIEV006)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.V01CLIENTE_AG_PF TABLE               
           ( CID_CLI                        CHAR(26) NOT NULL,          
             DNASC                          DATE NOT NULL,              
             INAT                           CHAR(30) NOT NULL,          
             CSGL_UF_ORIGE                  CHAR(2) NOT NULL,           
             INAC                           CHAR(20) NOT NULL,          
             CSEXO                          CHAR(1) NOT NULL,           
             CEST_CVIL                      DECIMAL(1, 0) NOT NULL,     
             CCAPAC_CVIL                    DECIMAL(1, 0) NOT NULL,     
             CTPO_DOCTO_ID                  CHAR(20) NOT NULL,          
             CDOCTO_ID                      CHAR(15) NOT NULL,          
             IORG_EMISR_ID                  CHAR(20) NOT NULL,          
             DEMIS                          DATE NOT NULL,              
             COCUPC_PF                      DECIMAL(3, 0) NOT NULL,     
             CIDTFD_DOCTO_ID                DECIMAL(2, 0)               
           ) END-EXEC.                                                  
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.V01CLIENTE_AG_PF            *
      ******************************************************************
       01  CLIEV006.                                                    
      *    *************************************************************
           10 CID-CLI              PIC X(26).                           
      *    *************************************************************
           10 DNASC                PIC X(10).                           
      *    *************************************************************
           10 INAT                 PIC X(30).                           
      *    *************************************************************
           10 CSGL-UF-ORIGE        PIC X(2).                            
      *    *************************************************************
           10 INAC                 PIC X(20).                           
      *    *************************************************************
           10 CSEXO                PIC X(1).                            
      *    *************************************************************
           10 CEST-CVIL            PIC S9(1)V USAGE COMP-3.             
      *    *************************************************************
           10 CCAPAC-CVIL          PIC S9(1)V USAGE COMP-3.             
      *    *************************************************************
           10 CTPO-DOCTO-ID        PIC X(20).                           
      *    *************************************************************
           10 CDOCTO-ID            PIC X(15).                           
      *    *************************************************************
           10 IORG-EMISR-ID        PIC X(20).                           
      *    *************************************************************
           10 DEMIS                PIC X(10).                           
      *    *************************************************************
           10 COCUPC-PF            PIC S9(3)V USAGE COMP-3.             
      *    *************************************************************
           10 CIDTFD-DOCTO-ID      PIC S9(2)V USAGE COMP-3.             
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 14      *
      ******************************************************************

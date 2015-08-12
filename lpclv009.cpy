      ******************************************************************
      * DCLGEN TABLE(DB2PRD.V01LPCL_NATUREZ_OP)                        *
      *        LIBRARY(AD.DB2.DCLGEN(LPCLV009))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(LPCLV009)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.V01LPCL_NATUREZ_OP TABLE             
           ( NTOP_COD_NAT_OPER              CHAR(3) NOT NULL,           
             NTOP_DESCRIC                   CHAR(20) NOT NULL,          
             NTOP_PRAZO_MIN                 DECIMAL(3, 0) NOT NULL,     
             NTOP_PRAZO_MAX                 DECIMAL(3, 0) NOT NULL,     
             NTOP_VALOR_MAX                 DECIMAL(17, 2) NOT NULL,    
             NTOP_VALOR_MIN                 DECIMAL(17, 2) NOT NULL,    
             NTOP_INDIC_RESTR               CHAR(1) NOT NULL,           
             NTOP_DIA_MAX_DIR               DECIMAL(3, 0) NOT NULL,     
             NTOP_DIA_MIN_DIR               DECIMAL(3, 0) NOT NULL,     
             NTOP_VLR_MIN_DIR               DECIMAL(17, 2) NOT NULL,    
             NTOP_VLR_MAX_DIR               DECIMAL(17, 2) NOT NULL,    
             NTOP_INDCD_DIR                 CHAR(1) NOT NULL,           
             NTOP_CLASSIF_CACS              DECIMAL(3, 0) NOT NULL,     
             NTOP_MNEMO_NAT                 CHAR(4) NOT NULL,           
             NTOP_DESCRIC_RESU              CHAR(10) NOT NULL           
           ) END-EXEC.                                                  
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.V01LPCL_NATUREZ_OP          *
      ******************************************************************
       01  LPCLV009.                                                    
      *    *************************************************************
           10 NTOP-COD-NAT-OPER    PIC X(3).                            
      *    *************************************************************
           10 NTOP-DESCRIC         PIC X(20).                           
      *    *************************************************************
           10 NTOP-PRAZO-MIN       PIC S9(3)V USAGE COMP-3.             
      *    *************************************************************
           10 NTOP-PRAZO-MAX       PIC S9(3)V USAGE COMP-3.             
      *    *************************************************************
           10 NTOP-VALOR-MAX       PIC S9(15)V9(2) USAGE COMP-3.        
      *    *************************************************************
           10 NTOP-VALOR-MIN       PIC S9(15)V9(2) USAGE COMP-3.        
      *    *************************************************************
           10 NTOP-INDIC-RESTR     PIC X(1).                            
      *    *************************************************************
           10 NTOP-DIA-MAX-DIR     PIC S9(3)V USAGE COMP-3.             
      *    *************************************************************
           10 NTOP-DIA-MIN-DIR     PIC S9(3)V USAGE COMP-3.             
      *    *************************************************************
           10 NTOP-VLR-MIN-DIR     PIC S9(15)V9(2) USAGE COMP-3.        
      *    *************************************************************
           10 NTOP-VLR-MAX-DIR     PIC S9(15)V9(2) USAGE COMP-3.        
      *    *************************************************************
           10 NTOP-INDCD-DIR       PIC X(1).                            
      *    *************************************************************
           10 NTOP-CLASSIF-CACS    PIC S9(3)V USAGE COMP-3.             
      *    *************************************************************
           10 NTOP-MNEMO-NAT       PIC X(4).                            
      *    *************************************************************
           10 NTOP-DESCRIC-RESU    PIC X(10).                           
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 15      *
      ******************************************************************

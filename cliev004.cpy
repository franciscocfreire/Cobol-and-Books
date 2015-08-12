      ******************************************************************
      * DCLGEN TABLE(DB2PRD.V01CLIENTE_AGENCIA)                        *
      *        LIBRARY(AD.DB2.DCLGEN(CLIEV004))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(CLIEV004)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.V01CLIENTE_AGENCIA TABLE             
           ( CID_CLI                        CHAR(26) NOT NULL,          
             CCGC_CPF                       DECIMAL(9, 0) NOT NULL,     
             CFLIAL_CGC                     DECIMAL(4, 0) NOT NULL,     
             CCTRL_CPF_CGC                  DECIMAL(2, 0) NOT NULL,     
             CDEPDT_CLI_AG                  DECIMAL(7, 0) NOT NULL,     
             CTPO_PSSOA                     CHAR(1) NOT NULL,           
             CDDD                           CHAR(4) NOT NULL,           
             CFONE                          DECIMAL(8, 0) NOT NULL,     
             CDDD_FAX                       CHAR(4) NOT NULL,           
             CFAX                           DECIMAL(8, 0) NOT NULL,     
             IPSSOA_COPLT                   CHAR(70) NOT NULL,          
             ELOGDR                         CHAR(40) NOT NULL,          
             ENRO_IMOV                      CHAR(7) NOT NULL,           
             ECOMPL_NRO_LOGDR               CHAR(20) NOT NULL,          
             EBAIRO_LOGDR                   CHAR(20) NOT NULL,          
             DULT_ACERT                     DATE NOT NULL,              
             CCEP                           DECIMAL(5, 0) NOT NULL,     
             CCEP_COMPL                     DECIMAL(3, 0) NOT NULL,     
             CJUNC_DEPDC                    DECIMAL(5, 0) NOT NULL,     
             CFUNC_BDSCO                    DECIMAL(9, 0) NOT NULL,     
             CFUNC_BDSCO_RESP               DECIMAL(9, 0) NOT NULL      
           ) END-EXEC.                                                  
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.V01CLIENTE_AGENCIA          *
      ******************************************************************
       01  CLIEV004.                                                    
      *    *************************************************************
           10 CID-CLI              PIC X(26).                           
      *    *************************************************************
           10 CCGC-CPF             PIC S9(9)V USAGE COMP-3.             
      *    *************************************************************
           10 CFLIAL-CGC           PIC S9(4)V USAGE COMP-3.             
      *    *************************************************************
           10 CCTRL-CPF-CGC        PIC S9(2)V USAGE COMP-3.             
      *    *************************************************************
           10 CDEPDT-CLI-AG        PIC S9(7)V USAGE COMP-3.             
      *    *************************************************************
           10 CTPO-PSSOA           PIC X(1).                            
      *    *************************************************************
           10 CDDD                 PIC X(4).                            
      *    *************************************************************
           10 CFONE                PIC S9(8)V USAGE COMP-3.             
      *    *************************************************************
           10 CDDD-FAX             PIC X(4).                            
      *    *************************************************************
           10 CFAX                 PIC S9(8)V USAGE COMP-3.             
      *    *************************************************************
           10 IPSSOA-COPLT         PIC X(70).                           
      *    *************************************************************
           10 ELOGDR               PIC X(40).                           
      *    *************************************************************
           10 ENRO-IMOV            PIC X(7).                            
      *    *************************************************************
           10 ECOMPL-NRO-LOGDR     PIC X(20).                           
      *    *************************************************************
           10 EBAIRO-LOGDR         PIC X(20).                           
      *    *************************************************************
           10 DULT-ACERT           PIC X(10).                           
      *    *************************************************************
           10 CCEP                 PIC S9(5)V USAGE COMP-3.             
      *    *************************************************************
           10 CCEP-COMPL           PIC S9(3)V USAGE COMP-3.             
      *    *************************************************************
           10 CJUNC-DEPDC          PIC S9(5)V USAGE COMP-3.             
      *    *************************************************************
           10 CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.             
      *    *************************************************************
           10 CFUNC-BDSCO-RESP     PIC S9(9)V USAGE COMP-3.             
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 21      *
      ******************************************************************

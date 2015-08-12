      ******************************************************************
      * DCLGEN TABLE(DB2PRD.V01CEP)                                    *
      *        LIBRARY(AD.DB2.DCLGEN(CEPNV003))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(CEPNV003)                                     *
      *        APOST                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.V01CEP TABLE                         
           ( CCEP                           DECIMAL(5, 0) NOT NULL,     
             CCEP_COMPL                     DECIMAL(3, 0) NOT NULL,     
             ILOGDR_CEP                     CHAR(60) NOT NULL,          
             CENTRG_CORSP                   CHAR(1) NOT NULL,           
             CSEQ_LOGDR_INIC                DECIMAL(5, 0) NOT NULL,     
             CSEQ_LOGDR_FNAL                DECIMAL(5, 0) NOT NULL,     
             CIDTFD_LADO_LOGDR              CHAR(1) NOT NULL,           
             CSGL_UF                        CHAR(2) NOT NULL,           
             CMUN                           DECIMAL(5, 0) NOT NULL,     
             CDIG_MUN                       CHAR(1) NOT NULL,           
             IMUN                           CHAR(30) NOT NULL,          
             CBCO_AG_DEPOS                  DECIMAL(7, 0) NOT NULL,     
             CCEP_ANTIG                     DECIMAL(5, 0) NOT NULL,     
             CCDD                           DECIMAL(5, 0) NOT NULL,     
             EBAIRO_LOGDR                   CHAR(20) NOT NULL,          
             CTPO_LOGDR                     CHAR(5) NOT NULL,           
             DCTRL_OPER                     DATE,                       
             CMUN_INSTC_PBLIC               CHAR(10) NOT NULL,          
             CINSTC_RCONH_PBLIC             DECIMAL(5, 0) NOT NULL,     
             ELOGDR_REDZD                   CHAR(40) NOT NULL           
           ) END-EXEC.                                                  
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.V01CEP                      *
      ******************************************************************
       01  CEPNV003.                                                    
           10 CCEP                 PIC S9(5)V USAGE COMP-3.             
           10 CCEP-COMPL           PIC S9(3)V USAGE COMP-3.             
           10 ILOGDR-CEP           PIC X(60).                           
           10 CENTRG-CORSP         PIC X(1).                            
           10 CSEQ-LOGDR-INIC      PIC S9(5)V USAGE COMP-3.             
           10 CSEQ-LOGDR-FNAL      PIC S9(5)V USAGE COMP-3.             
           10 CIDTFD-LADO-LOGDR    PIC X(1).                            
           10 CSGL-UF              PIC X(2).                            
           10 CMUN                 PIC S9(5)V USAGE COMP-3.             
           10 CDIG-MUN             PIC X(1).                            
           10 IMUN                 PIC X(30).                           
           10 CBCO-AG-DEPOS        PIC S9(7)V USAGE COMP-3.             
           10 CCEP-ANTIG           PIC S9(5)V USAGE COMP-3.             
           10 CCDD                 PIC S9(5)V USAGE COMP-3.             
           10 EBAIRO-LOGDR         PIC X(20).                           
           10 CTPO-LOGDR           PIC X(5).                            
           10 DCTRL-OPER           PIC X(10).                           
           10 CMUN-INSTC-PBLIC     PIC X(10).                           
           10 CINSTC-RCONH-PBLIC   PIC S9(5)V USAGE COMP-3.             
           10 ELOGDR-REDZD         PIC X(40).                           
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 20      *
      ******************************************************************

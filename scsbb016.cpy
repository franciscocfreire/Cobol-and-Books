      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TREG_VDA_RECTA)                            *
      *        LIBRARY(AD.DB2.DCLGEN(SCSBB016))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(SCSBB016)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TREG_VDA_RECTA TABLE                 
           ( DANO_VDA_RECTA                 DECIMAL(4, 0) NOT NULL,     
             NREG_VDA_RECTA                 DECIMAL(9, 0) NOT NULL,     
             CPSSOA_UND_ORGNZ               DECIMAL(10, 0) NOT NULL,    
             NSEQ_UND_ORGNZ                 DECIMAL(8, 0) NOT NULL,     
             CPSSOA_JURID                   DECIMAL(10, 0) NOT NULL,    
             ICOMPR_EXTER                   CHAR(150) NOT NULL,         
             EEXTER_COMPR                   CHAR(150) NOT NULL,         
             CPAIS                          DECIMAL(3, 0) NOT NULL,     
             CFSCAL_OPER_EXTER              CHAR(40),                   
             CINDCD_ECONM                   DECIMAL(5, 0) NOT NULL,     
             CSIT_REG                       CHAR(1) NOT NULL,           
             CTPO_REG_EXTER                 DECIMAL(1, 0) NOT NULL,     
             CUSUAR_INCL                    CHAR(9) NOT NULL,           
             HINCL_REG                      TIMESTAMP NOT NULL,         
             CUSUAR_ALT                     CHAR(9),                    
             HALT_REG                       TIMESTAMP,                  
             WCOMPL_COMPR_VDA               VARCHAR(5000),              
             CORIGE_REG_RECTA               CHAR(1) NOT NULL,           
             VREG_SERVC_RECTA               DECIMAL(15, 2) NOT NULL,    
             HVALDC_REG_RECTA               TIMESTAMP                   
           ) END-EXEC.                                                  
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TREG_VDA_RECTA              *
      ******************************************************************
       01  SCSBB016.                                                    
      *    *************************************************************
           10 DANO-VDA-RECTA       PIC S9(4)V USAGE COMP-3.             
      *    *************************************************************
           10 NREG-VDA-RECTA       PIC S9(9)V USAGE COMP-3.             
      *    *************************************************************
           10 CPSSOA-UND-ORGNZ     PIC S9(10)V USAGE COMP-3.            
      *    *************************************************************
           10 NSEQ-UND-ORGNZ       PIC S9(8)V USAGE COMP-3.             
      *    *************************************************************
           10 CPSSOA-JURID         PIC S9(10)V USAGE COMP-3.            
      *    *************************************************************
           10 ICOMPR-EXTER         PIC X(150).                          
      *    *************************************************************
           10 EEXTER-COMPR         PIC X(150).                          
      *    *************************************************************
           10 CPAIS                PIC S9(3)V USAGE COMP-3.             
      *    *************************************************************
           10 CFSCAL-OPER-EXTER    PIC X(40).                           
      *    *************************************************************
           10 CINDCD-ECONM         PIC S9(5)V USAGE COMP-3.             
      *    *************************************************************
           10 CSIT-REG             PIC X(1).                            
      *    *************************************************************
           10 CTPO-REG-EXTER       PIC S9(1)V USAGE COMP-3.             
      *    *************************************************************
           10 CUSUAR-INCL          PIC X(9).                            
      *    *************************************************************
           10 HINCL-REG            PIC X(26).                           
      *    *************************************************************
           10 CUSUAR-ALT           PIC X(9).                            
      *    *************************************************************
           10 HALT-REG             PIC X(26).                           
      *    *************************************************************
           10 WCOMPL-COMPR-VDA.                                         
              49 WCOMPL-COMPR-VDA-LEN                                   
                 PIC S9(4) USAGE COMP.                                  
              49 WCOMPL-COMPR-VDA-TEXT                                  
                 PIC X(5000).                                           
      *    *************************************************************
           10 CORIGE-REG-RECTA     PIC X(1).                            
      *    *************************************************************
           10 VREG-SERVC-RECTA     PIC S9(13)V9(2) USAGE COMP-3.        
      *    *************************************************************
           10 HVALDC-REG-RECTA     PIC X(26).                           
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 20      *
      ******************************************************************

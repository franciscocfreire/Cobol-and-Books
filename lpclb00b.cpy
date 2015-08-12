      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TCONS_CONTR_MORA)                          *
      *        LIBRARY(AD.DB2.DCLGEN(LPCLB00B))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(LPCLB00B)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TCONS_CONTR_MORA TABLE               
           ( CFUNC_BDSCO                    DECIMAL(9, 0) NOT NULL,     
             CAG_BCRIA                      DECIMAL(5, 0) NOT NULL,     
             CCTA_CORR                      DECIMAL(7, 0) NOT NULL,     
             CCART                          CHAR(3) NOT NULL,           
             CCONTR_VENCD                   DECIMAL(7, 0) NOT NULL,     
             DVCTO                          DATE NOT NULL,              
             CDIG_CTA_REFT                  CHAR(1),                    
             CCPF_CNPJ                      DECIMAL(9, 0) NOT NULL,     
             CFLIAL_CNPJ                    DECIMAL(5, 0),              
             CCTRL_CPF_CNPJ                 DECIMAL(2, 0) NOT NULL,     
             CPCELA_CONTR_MORA              DECIMAL(15, 0) NOT NULL,    
             CDIG_CREDT_LIQDC               CHAR(1) NOT NULL,           
             VLIQ                           DECIMAL(17, 2) NOT NULL,    
             VATULZ_CONTR_MORA              DECIMAL(15, 2) NOT NULL,    
             VCONTR_MORA_VENCD              DECIMAL(15, 2) NOT NULL,    
             VTOT_CONTR_MORA                DECIMAL(15, 2) NOT NULL,    
             RFASE_PROCS                    CHAR(16),                   
             DINIC_CONTR_ATRSO              DATE NOT NULL               
           ) END-EXEC.                                                  
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TCONS_CONTR_MORA            *
      ******************************************************************
       01  LPCLB00B.                                                    
      *    *************************************************************
           10 CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.             
      *    *************************************************************
           10 CAG-BCRIA            PIC S9(5)V USAGE COMP-3.             
      *    *************************************************************
           10 CCTA-CORR            PIC S9(7)V USAGE COMP-3.             
      *    *************************************************************
           10 CCART                PIC X(3).                            
      *    *************************************************************
           10 CCONTR-VENCD         PIC S9(7)V USAGE COMP-3.             
      *    *************************************************************
           10 DVCTO                PIC X(10).                           
      *    *************************************************************
           10 CDIG-CTA-REFT        PIC X(1).                            
      *    *************************************************************
           10 CCPF-CNPJ            PIC S9(9)V USAGE COMP-3.             
      *    *************************************************************
           10 CFLIAL-CNPJ          PIC S9(5)V USAGE COMP-3.             
      *    *************************************************************
           10 CCTRL-CPF-CNPJ       PIC S9(2)V USAGE COMP-3.             
      *    *************************************************************
           10 CPCELA-CONTR-MORA    PIC S9(15)V USAGE COMP-3.            
      *    *************************************************************
           10 CDIG-CREDT-LIQDC     PIC X(1).                            
      *    *************************************************************
           10 VLIQ                 PIC S9(15)V9(2) USAGE COMP-3.        
      *    *************************************************************
           10 VATULZ-CONTR-MORA    PIC S9(13)V9(2) USAGE COMP-3.        
      *    *************************************************************
           10 VCONTR-MORA-VENCD    PIC S9(13)V9(2) USAGE COMP-3.        
      *    *************************************************************
           10 VTOT-CONTR-MORA      PIC S9(13)V9(2) USAGE COMP-3.        
      *    *************************************************************
           10 RFASE-PROCS          PIC X(16).                           
      *    *************************************************************
           10 DINIC-CONTR-ATRSO    PIC X(10).                           
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 18      *
      ******************************************************************

      ******************************************************************
      * DCLGEN TABLE(DB2PRD.PESSOA_CADASTRADA)                         *
      *        LIBRARY(AD.DB2.DCLGEN(FICAB000))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(FICAB000)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.PESSOA_CADASTRADA TABLE              
           ( CPSSOA_CADTR                   TIMESTAMP NOT NULL,         
             CPSSOA                         TIMESTAMP NOT NULL,         
             DINCL                          DATE NOT NULL,              
             CTPO_PSSOA_CADTR               CHAR(1) NOT NULL,           
             DCTRL_OPER                     DATE NOT NULL,              
             CEMIS_EXTRT_DEB                DECIMAL(1, 0) NOT NULL,     
             DULTMA_ATULZ_DSTNO             DATE,                       
             CSIT_FICHA_CAD                 DECIMAL(1, 0) NOT NULL,     
             CFUNC_BDSCO                    DECIMAL(9, 0) NOT NULL,     
             CANAL_FICHA_CAD                CHAR(1),                    
             CSTTUS_FICHA_CAD               CHAR(2) NOT NULL,           
             CIND_IMPED_REST                CHAR(1) NOT NULL,           
             RREFT_BCRIA                    CHAR(40),                   
             VQUITD_NAO_COMPV               DECIMAL(15, 0) NOT NULL,    
             VTOT_IMOV_ONUS                 DECIMAL(15, 0) NOT NULL,    
             VTOT_IMOV_COMPV                DECIMAL(15, 0) NOT NULL,    
             CDDD                           CHAR(4) NOT NULL,           
             CFONE                          DECIMAL(8, 0) NOT NULL,     
             CISENC_DEB_CAD                 CHAR(1) NOT NULL,           
             CJUNC_DEPDC                    DECIMAL(5, 0) NOT NULL,     
             CDIG_DEPDC                     CHAR(1) NOT NULL,           
             CCTA_CORR                      DECIMAL(7, 0) NOT NULL,     
             CDIG_CTA_CORR                  CHAR(1) NOT NULL,           
             CCEP                           DECIMAL(5, 0) NOT NULL,     
             CCEP_COMPL                     DECIMAL(3, 0),              
             DABERT_CTA_CORR                DATE,                       
             IMUN                           CHAR(30) NOT NULL,          
             ELOGDR                         CHAR(40) NOT NULL,          
             ENRO_LOGDR                     DECIMAL(5, 0) NOT NULL,     
             ECOMPL_LOGDR                   CHAR(10) NOT NULL,          
             CAUTRZ_CONS                    CHAR(1),                    
             HULTMA_ATULZ_CONS              TIMESTAMP,                  
             DENVIO_CAD_CREDT               DATE,                       
             CORIGE_FICHA                   DECIMAL(5, 0),              
             CSGL_UF_COML                   CHAR(2),                    
             CTPO_CATAO_CREDT               DECIMAL(3, 0),              
             CPORTE_EMPR                    DECIMAL(3, 0),              
             CFUNC_ISENC_COBR               DECIMAL(9, 0),              
             NLIN_TFONI                     DECIMAL(11, 0)              
           ) END-EXEC.                                                  
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.PESSOA_CADASTRADA           *
      ******************************************************************
       01  FICAB000.                                                    
      *    *************************************************************
           10 CPSSOA-CADTR         PIC X(26).                           
      *    *************************************************************
           10 CPSSOA               PIC X(26).                           
      *    *************************************************************
           10 DINCL                PIC X(10).                           
      *    *************************************************************
           10 CTPO-PSSOA-CADTR     PIC X(1).                            
      *    *************************************************************
           10 DCTRL-OPER           PIC X(10).                           
      *    *************************************************************
           10 CEMIS-EXTRT-DEB      PIC S9(1)V USAGE COMP-3.             
      *    *************************************************************
           10 DULTMA-ATULZ-DSTNO   PIC X(10).                           
      *    *************************************************************
           10 CSIT-FICHA-CAD       PIC S9(1)V USAGE COMP-3.             
      *    *************************************************************
           10 CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.             
      *    *************************************************************
           10 CANAL-FICHA-CAD      PIC X(1).                            
      *    *************************************************************
           10 CSTTUS-FICHA-CAD     PIC X(2).                            
      *    *************************************************************
           10 CIND-IMPED-REST      PIC X(1).                            
      *    *************************************************************
           10 RREFT-BCRIA          PIC X(40).                           
      *    *************************************************************
           10 VQUITD-NAO-COMPV     PIC S9(15)V USAGE COMP-3.            
      *    *************************************************************
           10 VTOT-IMOV-ONUS       PIC S9(15)V USAGE COMP-3.            
      *    *************************************************************
           10 VTOT-IMOV-COMPV      PIC S9(15)V USAGE COMP-3.            
      *    *************************************************************
           10 CDDD                 PIC X(4).                            
      *    *************************************************************
           10 CFONE                PIC S9(8)V USAGE COMP-3.             
      *    *************************************************************
           10 CISENC-DEB-CAD       PIC X(1).                            
      *    *************************************************************
           10 CJUNC-DEPDC          PIC S9(5)V USAGE COMP-3.             
      *    *************************************************************
           10 CDIG-DEPDC           PIC X(1).                            
      *    *************************************************************
           10 CCTA-CORR            PIC S9(7)V USAGE COMP-3.             
      *    *************************************************************
           10 CDIG-CTA-CORR        PIC X(1).                            
      *    *************************************************************
           10 CCEP                 PIC S9(5)V USAGE COMP-3.             
      *    *************************************************************
           10 CCEP-COMPL           PIC S9(3)V USAGE COMP-3.             
      *    *************************************************************
           10 DABERT-CTA-CORR      PIC X(10).                           
      *    *************************************************************
           10 IMUN                 PIC X(30).                           
      *    *************************************************************
           10 ELOGDR               PIC X(40).                           
      *    *************************************************************
           10 ENRO-LOGDR           PIC S9(5)V USAGE COMP-3.             
      *    *************************************************************
           10 ECOMPL-LOGDR         PIC X(10).                           
      *    *************************************************************
           10 CAUTRZ-CONS          PIC X(1).                            
      *    *************************************************************
           10 HULTMA-ATULZ-CONS    PIC X(26).                           
      *    *************************************************************
           10 DENVIO-CAD-CREDT     PIC X(10).                           
      *    *************************************************************
           10 CORIGE-FICHA         PIC S9(5)V USAGE COMP-3.             
      *    *************************************************************
           10 CSGL-UF-COML         PIC X(2).                            
      *    *************************************************************
           10 CTPO-CATAO-CREDT     PIC S9(3)V USAGE COMP-3.             
      *    *************************************************************
           10 CPORTE-EMPR          PIC S9(3)V USAGE COMP-3.             
      *    *************************************************************
           10 CFUNC-ISENC-COBR     PIC S9(9)V USAGE COMP-3.             
      *    *************************************************************
           10 NLIN-TFONI           PIC S9(11)V USAGE COMP-3.            
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 39      *
      ******************************************************************

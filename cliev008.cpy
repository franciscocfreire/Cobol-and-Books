      ******************************************************************
      * DCLGEN TABLE(DB2PRD.V01CTA_CORRENTE)                           *
      *        LIBRARY(AD.DB2.DCLGEN(CLIEV008))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(CLIEV008)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.V01CTA_CORRENTE TABLE                
           ( CJUNC_DEPDC                    DECIMAL(5, 0) NOT NULL,     
             CCTA_CLI                       DECIMAL(7, 0) NOT NULL,     
             CGRP_CTBIL_ATUAL               DECIMAL(3, 0) NOT NULL,     
             CSGRP_CTBIL                    DECIMAL(3, 0) NOT NULL,     
             CTPO_CTA                       DECIMAL(2, 0) NOT NULL,     
             CFORMA_MOVTC_CTA               DECIMAL(1, 0) NOT NULL,     
             CBLOQ_TALAO_CHEQ               DECIMAL(1, 0) NOT NULL,     
             CPERDC_EMIS_EXTRT              CHAR(1) NOT NULL,           
             DABERT_CTA                     DATE NOT NULL,              
             DULT_ACERT                     DATE NOT NULL,              
             CSIT_CTA_MOVTC                 CHAR(1) NOT NULL,           
             DRCADT                         DATE NOT NULL,              
             ICTA_MOVTC                     CHAR(40) NOT NULL,          
             ELOGDR                         CHAR(40) NOT NULL,          
             ENRO_IMOV                      CHAR(7) NOT NULL,           
             ECOMPL_NRO_LOGDR               CHAR(20) NOT NULL,          
             EBAIRO_LOGDR                   CHAR(20) NOT NULL,          
             CID_CLI                        CHAR(26) NOT NULL,          
             CPOSTO_SERVC                   DECIMAL(3, 0) NOT NULL,     
             CCEP                           DECIMAL(5, 0) NOT NULL,     
             CCEP_COMPL                     DECIMAL(3, 0) NOT NULL,     
             CFUNC_BDSCO                    DECIMAL(9, 0) NOT NULL,     
             ELOGDR_NRO_COMPL               CHAR(40) NOT NULL,          
             CCEP_NOVO                      DECIMAL(5, 0) NOT NULL,     
             CCEP_NOVO_COMPL                DECIMAL(3, 0) NOT NULL,     
             CORIGE_CTA_ABERT               DECIMAL(2, 0) NOT NULL,     
             DULT_ATULZ_ENDER               DATE NOT NULL,              
             CSGMTO_CLI                     DECIMAL(3, 0) NOT NULL      
           ) END-EXEC.                                                  
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.V01CTA_CORRENTE             *
      ******************************************************************
       01  CLIEV008.                                                    
      *    *************************************************************
           10 CJUNC-DEPDC          PIC S9(5)V USAGE COMP-3.             
      *    *************************************************************
           10 CCTA-CLI             PIC S9(7)V USAGE COMP-3.             
      *    *************************************************************
           10 CGRP-CTBIL-ATUAL     PIC S9(3)V USAGE COMP-3.             
      *    *************************************************************
           10 CSGRP-CTBIL          PIC S9(3)V USAGE COMP-3.             
      *    *************************************************************
           10 CTPO-CTA             PIC S9(2)V USAGE COMP-3.             
      *    *************************************************************
           10 CFORMA-MOVTC-CTA     PIC S9(1)V USAGE COMP-3.             
      *    *************************************************************
           10 CBLOQ-TALAO-CHEQ     PIC S9(1)V USAGE COMP-3.             
      *    *************************************************************
           10 CPERDC-EMIS-EXTRT    PIC X(1).                            
      *    *************************************************************
           10 DABERT-CTA           PIC X(10).                           
      *    *************************************************************
           10 DULT-ACERT           PIC X(10).                           
      *    *************************************************************
           10 CSIT-CTA-MOVTC       PIC X(1).                            
      *    *************************************************************
           10 DRCADT               PIC X(10).                           
      *    *************************************************************
           10 ICTA-MOVTC           PIC X(40).                           
      *    *************************************************************
           10 ELOGDR               PIC X(40).                           
      *    *************************************************************
           10 ENRO-IMOV            PIC X(7).                            
      *    *************************************************************
           10 ECOMPL-NRO-LOGDR     PIC X(20).                           
      *    *************************************************************
           10 EBAIRO-LOGDR         PIC X(20).                           
      *    *************************************************************
           10 CID-CLI              PIC X(26).                           
      *    *************************************************************
           10 CPOSTO-SERVC         PIC S9(3)V USAGE COMP-3.             
      *    *************************************************************
           10 CCEP                 PIC S9(5)V USAGE COMP-3.             
      *    *************************************************************
           10 CCEP-COMPL           PIC S9(3)V USAGE COMP-3.             
      *    *************************************************************
           10 CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.             
      *    *************************************************************
           10 ELOGDR-NRO-COMPL     PIC X(40).                           
      *    *************************************************************
           10 CCEP-NOVO            PIC S9(5)V USAGE COMP-3.             
      *    *************************************************************
           10 CCEP-NOVO-COMPL      PIC S9(3)V USAGE COMP-3.             
      *    *************************************************************
           10 CORIGE-CTA-ABERT     PIC S9(2)V USAGE COMP-3.             
      *    *************************************************************
           10 DULT-ATULZ-ENDER     PIC X(10).                           
      *    *************************************************************
           10 CSGMTO-CLI           PIC S9(3)V USAGE COMP-3.             
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 28      *
      ******************************************************************

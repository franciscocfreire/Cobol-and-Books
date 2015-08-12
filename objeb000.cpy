      ******************************************************************
      * DCLGEN TABLE(DB2PRD.OBJE_CLIENTES)                             *
      *        LIBRARY(AD.DB2.DCLGEN(OBJEB000))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(OBJEB000)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.OBJE_CLIENTES TABLE                  
           ( AGENCIA                        DECIMAL(5, 0) NOT NULL,     
             CONTA_CORRENTE                 DECIMAL(7, 0) NOT NULL,     
             NOME                           CHAR(40) NOT NULL,          
             ENDERECO                       CHAR(40) NOT NULL,          
             CEP                            DECIMAL(5, 0) NOT NULL,     
             FONE                           DECIMAL(7, 0) NOT NULL,     
             SEXO                           CHAR(1) NOT NULL,           
             DT_NASCIMENTO                  DECIMAL(7, 0) NOT NULL,     
             NUM_CGCCPF                     DECIMAL(9, 0) NOT NULL,     
             FILIAL_CGCCPF                  DECIMAL(5, 0) NOT NULL,     
             CONTR_CGCCPF                   CHAR(2) NOT NULL,           
             RAZAO                          DECIMAL(5, 0) NOT NULL,     
             RAMO                           DECIMAL(5, 0) NOT NULL,     
             TIPO_CONTA                     CHAR(2) NOT NULL,           
             ISENCAO                        CHAR(1) NOT NULL,           
             GRUPO                          DECIMAL(5, 0) NOT NULL,     
             CHEQUE_ESPECIAL                CHAR(1) NOT NULL,           
             ULTIMA_FOLHA                   DECIMAL(7, 0) NOT NULL,     
             DT_EMISSAO                     DECIMAL(7, 0) NOT NULL,     
             SERIE                          CHAR(1) NOT NULL,           
             BLOQUEIO                       CHAR(1) NOT NULL,           
             QTDE_FOLHA                     CHAR(1) NOT NULL,           
             TIPO_EXTRATO                   CHAR(1) NOT NULL,           
             POSTO                          DECIMAL(3, 0) NOT NULL,     
             DT_ABERTURA                    DECIMAL(7, 0) NOT NULL,     
             DT_ULTIMO_ACERTO               DECIMAL(7, 0) NOT NULL,     
             CEP_NOVO_NRO                   DECIMAL(5, 0) NOT NULL,     
             CEP_NOVO_COMPL                 DECIMAL(3, 0) NOT NULL,     
             FONE_DDD                       DECIMAL(5, 0) NOT NULL,     
             FAX_DDD                        DECIMAL(5, 0) NOT NULL,     
             FAX_FONE                       DECIMAL(7, 0) NOT NULL,     
             CTPO_PSSOA                     CHAR(1) NOT NULL,           
             CDDD                           CHAR(4) NOT NULL,           
             CFONE                          DECIMAL(8, 0) NOT NULL,     
             CDDD_FAX                       CHAR(4) NOT NULL,           
             CFAX                           DECIMAL(8, 0) NOT NULL,     
             DNASC                          DATE NOT NULL,              
             DABERT_CTA                     DATE NOT NULL,              
             DULT_ACERT                     DATE NOT NULL,              
             DRCADT                         DATE NOT NULL,              
             CFUNC_BDSCO                    DECIMAL(9, 0) NOT NULL,     
             CID_CLI                        CHAR(26) NOT NULL,          
             CRAMO_ATVDD_CNPJ               DECIMAL(7, 0) NOT NULL,     
             ELOGDR                         CHAR(40) NOT NULL,          
             ENRO_IMOV                      CHAR(7) NOT NULL,           
             ECOMPL_NRO_LOGDR               CHAR(20) NOT NULL,          
             EBAIRO_LOGDR                   CHAR(20) NOT NULL,          
             CCEP                           DECIMAL(5, 0) NOT NULL,     
             CCEP_COMPL                     DECIMAL(3, 0) NOT NULL,     
             NLIN_TFONI                     DECIMAL(11, 0),             
             NLIN_TFONI_FAX                 DECIMAL(11, 0)              
           ) END-EXEC.                                                  
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.OBJE_CLIENTES               *
      ******************************************************************
       01  OBJEB000.                                                    
      *    *************************************************************
           10 AGENCIA              PIC S9(5)V USAGE COMP-3.             
      *    *************************************************************
           10 CONTA-CORRENTE       PIC S9(7)V USAGE COMP-3.             
      *    *************************************************************
           10 NOME                 PIC X(40).                           
      *    *************************************************************
           10 ENDERECO             PIC X(40).                           
      *    *************************************************************
           10 CEP                  PIC S9(5)V USAGE COMP-3.             
      *    *************************************************************
           10 FONE                 PIC S9(7)V USAGE COMP-3.             
      *    *************************************************************
           10 SEXO                 PIC X(1).                            
      *    *************************************************************
           10 DT-NASCIMENTO        PIC S9(7)V USAGE COMP-3.             
      *    *************************************************************
           10 NUM-CGCCPF           PIC S9(9)V USAGE COMP-3.             
      *    *************************************************************
           10 FILIAL-CGCCPF        PIC S9(5)V USAGE COMP-3.             
      *    *************************************************************
           10 CONTR-CGCCPF         PIC X(2).                            
      *    *************************************************************
           10 RAZAO                PIC S9(5)V USAGE COMP-3.             
      *    *************************************************************
           10 RAMO                 PIC S9(5)V USAGE COMP-3.             
      *    *************************************************************
           10 TIPO-CONTA           PIC X(2).                            
      *    *************************************************************
           10 ISENCAO              PIC X(1).                            
      *    *************************************************************
           10 GRUPO                PIC S9(5)V USAGE COMP-3.             
      *    *************************************************************
           10 CHEQUE-ESPECIAL      PIC X(1).                            
      *    *************************************************************
           10 ULTIMA-FOLHA         PIC S9(7)V USAGE COMP-3.             
      *    *************************************************************
           10 DT-EMISSAO           PIC S9(7)V USAGE COMP-3.             
      *    *************************************************************
           10 SERIE                PIC X(1).                            
      *    *************************************************************
           10 BLOQUEIO             PIC X(1).                            
      *    *************************************************************
           10 QTDE-FOLHA           PIC X(1).                            
      *    *************************************************************
           10 TIPO-EXTRATO         PIC X(1).                            
      *    *************************************************************
           10 POSTO                PIC S9(3)V USAGE COMP-3.             
      *    *************************************************************
           10 DT-ABERTURA          PIC S9(7)V USAGE COMP-3.             
      *    *************************************************************
           10 DT-ULTIMO-ACERTO     PIC S9(7)V USAGE COMP-3.             
      *    *************************************************************
           10 CEP-NOVO-NRO         PIC S9(5)V USAGE COMP-3.             
      *    *************************************************************
           10 CEP-NOVO-COMPL       PIC S9(3)V USAGE COMP-3.             
      *    *************************************************************
           10 FONE-DDD             PIC S9(5)V USAGE COMP-3.             
      *    *************************************************************
           10 FAX-DDD              PIC S9(5)V USAGE COMP-3.             
      *    *************************************************************
           10 FAX-FONE             PIC S9(7)V USAGE COMP-3.             
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
           10 DNASC                PIC X(10).                           
      *    *************************************************************
           10 DABERT-CTA           PIC X(10).                           
      *    *************************************************************
           10 DULT-ACERT           PIC X(10).                           
      *    *************************************************************
           10 DRCADT               PIC X(10).                           
      *    *************************************************************
           10 CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.             
      *    *************************************************************
           10 CID-CLI              PIC X(26).                           
      *    *************************************************************
           10 CRAMO-ATVDD-CNPJ     PIC S9(7)V USAGE COMP-3.             
      *    *************************************************************
           10 ELOGDR               PIC X(40).                           
      *    *************************************************************
           10 ENRO-IMOV            PIC X(7).                            
      *    *************************************************************
           10 ECOMPL-NRO-LOGDR     PIC X(20).                           
      *    *************************************************************
           10 EBAIRO-LOGDR         PIC X(20).                           
      *    *************************************************************
           10 CCEP                 PIC S9(5)V USAGE COMP-3.             
      *    *************************************************************
           10 CCEP-COMPL           PIC S9(3)V USAGE COMP-3.             
      *    *************************************************************
           10 NLIN-TFONI           PIC S9(11)V USAGE COMP-3.            
      *    *************************************************************
           10 NLIN-TFONI-FAX       PIC S9(11)V USAGE COMP-3.            
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 51      *
      ******************************************************************

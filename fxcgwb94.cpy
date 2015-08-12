      *----------------------------------------------------------------*
      * INCLUDE: FXCGWB94                                              *
      * LRECL  : 0080                                                  *
      * DESCR  : ARQUIVO DE INCONSISTENTES - SLDO PRODUTO              *
      * F.BLOCO: F                                                     *
      * TI MELHORIAS                                                   *
      *----------------------------------------------------------------*
       01  FXCGWB94-REG.                                                
           05  FXCGWB94-DEPTO-GESTOR.                                   
            10 FXCGWB94-DEPTO-GESTOR-N      PIC  9(005).                
           05  FXCGWB94-EMPRESA             PIC  X(004).                
           05  FXCGWB94-CPRODUTO            PIC S9(008)       COMP-3.   
           05  FXCGWB94-SISTEMA             PIC  X(004).                
           05  FXCGWB94-DTMOVTO             PIC  X(010).                
           05  FXCGWB94-CLUCRO-CTBIL        PIC  X(010).                
           05  FXCGWB94-VLR-SAP             PIC S9(015)V9(02) COMP-3.   
           05  FXCGWB94-VLR-PROD            PIC S9(015)V9(02) COMP-3.   
           05  FXCGWB94-CPSSOA-JURID        PIC  9(010).                
           05  FXCGWB94-NSEQ-UND-ORGNZ      PIC  9(008).                
           05  FXCGWB94-COBS-FLUXO-CTBIL    PIC S9(002)       COMP-3.   
           05  FILLER                       PIC  X(004).                

      *================================================================*
      * NOME BOOK  : I#SCSB94                                          *
      * LRECL      : 450 - FB                                          *
      * DESCRICAO  : RECEPCAO DE ARQUIVO - DADOS FATURAMENTO/PAGAMENTO *
      * COMUNICACAO: BATCH                                             *
      * DATA       : 22/07/2015                                        *
      * AUTOR      : FERNANDA CARUSO                                   *
      * EMPRESA    : BRQ IT SERVICES                                   *
      * GRUPO      : 17 - CONTABILIDADE E IMPOSTOS                     *
      * COMPONENTE : SCSB - SISCOSERV BRADESCO                         *
      *----------------------------------------------------------------*
      * DATA       AUTOR             ALTERACAO                         *
      * ---------- ----------------- --------------------------------- *
      * 99/99/9999                   XXXXXXXXXXXXXXXXXXXXXXX           *
      *================================================================*
                                                                        
       01 SCSB94-REG.                                                   
          05 SCSB94-NSEQ               PIC 9(009).                      
          05 SCSB94-TPO-SERVC          PIC 9(001).                      
          05 SCSB94-TPO-SERVC-X        REDEFINES                        
             SCSB94-TPO-SERVC          PIC X(001).                      
          05 SCSB94-EMPR-DEPEND        PIC 9(010).                      
          05 SCSB94-DEPEND             PIC 9(008).                      
          05 SCSB94-NPROCS             PIC 9(004).                      
          05 SCSB94-NREG               PIC 9(009).                      
          05 SCSB94-NREG-X             REDEFINES                        
             SCSB94-NREG               PIC X(009).                      
          05 SCSB94-CD-FATMT-PGTO      PIC 9(010).                      
          05 SCSB94-CD-FATMT-PGTO-X    REDEFINES                        
             SCSB94-CD-FATMT-PGTO      PIC X(010).                      
          05 SCSB94-NOTA-FISCAL        PIC X(060).                      
          05 SCSB94-DT-FATMT-PGTO      PIC X(010).                      
          05 SCSB94-ERROS.                                              
             10 SCSB94-COD-ERRO        PIC 9(003)                       
                                       OCCURS 100 TIMES.                
          05 FILLER                    PIC X(029).                      

      *================================================================*
      * NOME BOOK  : I#SCSB93                                          *
      * LRECL      : 350 - FB                                          *
      * DESCRICAO  : RECEPCAO DE ARQUIVO - DADOS DO RE/DI              *
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
                                                                        
       01 SCSB93-REG.                                                   
          05 SCSB93-NSEQ               PIC 9(009).                      
          05 SCSB93-TPO-SERVC          PIC 9(001).                      
          05 SCSB93-TPO-SERVC-X        REDEFINES                        
             SCSB93-TPO-SERVC          PIC X(001).                      
          05 SCSB93-NPROCS             PIC 9(004).                      
          05 SCSB93-NREG               PIC 9(009).                      
          05 SCSB93-NREG-X             REDEFINES                        
             SCSB93-NREG               PIC X(009).                      
          05 SCSB93-CD-RE-DI           PIC 9(012).                      
          05 SCSB93-CD-RE-DI-X         REDEFINES                        
             SCSB93-CD-RE-DI           PIC X(012).                      
          05 SCSB93-ERROS.                                              
             10 SCSB93-COD-ERRO        PIC 9(003)                       
                                       OCCURS 100 TIMES.                
          05 FILLER                    PIC X(015).                      

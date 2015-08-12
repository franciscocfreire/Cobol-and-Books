      *================================================================*
      * NOME BOOK  : I#SCSB92                                          *
      * LRECL      : 360 - FB                                          *
      * DESCRICAO  : RECEPCAO DE ARQUIVO - DADOS DO ENQUADRAMENTO      *
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
                                                                        
       01 SCSB92-REG.                                                   
          05 SCSB92-NSEQ               PIC 9(009).                      
          05 SCSB92-TPO-SERVC          PIC 9(001).                      
          05 SCSB92-TPO-SERVC-X        REDEFINES                        
             SCSB92-TPO-SERVC          PIC X(001).                      
          05 SCSB92-NPROCS             PIC 9(004).                      
          05 SCSB92-NREG               PIC 9(009).                      
          05 SCSB92-NREG-X             REDEFINES                        
             SCSB92-NREG               PIC X(009).                      
          05 SCSB92-CD-OPER            PIC 9(010).                      
          05 SCSB92-CD-OPER-X          REDEFINES                        
             SCSB92-CD-OPER            PIC X(010).                      
          05 SCSB92-CD-ENQUA           PIC 9(010).                      
          05 SCSB92-CD-ENQUA-X         REDEFINES                        
             SCSB92-CD-ENQUA           PIC X(010).                      
          05 SCSB92-RC                 PIC 9(008).                      
          05 SCSB92-RC-X               REDEFINES                        
             SCSB92-RC                 PIC X(008).                      
          05 SCSB92-ERROS.                                              
             10 SCSB92-COD-ERRO        PIC 9(003)                       
                                       OCCURS 100 TIMES.                
          05 FILLER                    PIC X(009).                      

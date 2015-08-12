      *================================================================*
      * NOME BOOK  : I#SCSB97                                          *
      * LRECL      : 50 - FB                                           *
      * DESCRICAO  : RECEPCAO DE ARQUIVO - TIPO DE REGISTRO INVALIDO   *
      * COMUNICACAO: BATCH                                             *
      * DATA       : 24/07/2015                                        *
      * AUTOR      : FERNANDA CARUSO                                   *
      * EMPRESA    : BRQ IT SERVICES                                   *
      * GRUPO      : 17 - CONTABILIDADE E IMPOSTOS                     *
      * COMPONENTE : SCSB - SISCOSERV BRADESCO                         *
      *----------------------------------------------------------------*
      * DATA       AUTOR             ALTERACAO                         *
      * ---------- ----------------- --------------------------------- *
      * 99/99/9999                   XXXXXXXXXXXXXXXXXXXXXXX           *
      *================================================================*
                                                                        
       01 SCSB97-REG.                                                   
          05 SCSB97-NSEQ               PIC 9(009).                      
          05 SCSB97-TPO-SERVC          PIC 9(001).                      
          05 SCSB97-TPO-SERVC-X        REDEFINES                        
             SCSB97-TPO-SERVC          PIC X(001).                      
          05 SCSB97-NPROCS             PIC 9(004).                      
          05 SCSB97-NREG               PIC 9(009).                      
          05 SCSB97-NREG-X             REDEFINES                        
             SCSB97-NREG               PIC X(009).                      
          05 SCSB97-TPO-REG            PIC 9(001).                      
          05 SCSB97-TPO-REG-X          REDEFINES                        
             SCSB97-TPO-REG            PIC X(001).                      
          05 SCSB97-EMPR-DEPEND        PIC 9(010).                      
          05 SCSB97-DEPEND             PIC 9(008).                      
          05 FILLER                    PIC X(008).                      

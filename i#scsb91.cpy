      *================================================================*
      * NOME BOOK  : I#SCSB91                                          *
      * LRECL      : 400 - FB                                          *
      * DESCRICAO  : RECEPCAO DE ARQUIVO - DADOS DA OPERACAO           *
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
                                                                        
       01 SCSB91-REG.                                                   
          05 SCSB91-NSEQ               PIC 9(009).                      
          05 SCSB91-TPO-SERVC          PIC 9(001).                      
          05 SCSB91-TPO-SERVC-X        REDEFINES                        
             SCSB91-TPO-SERVC          PIC X(001).                      
          05 SCSB91-NPROCS             PIC 9(004).                      
          05 SCSB91-NREG               PIC 9(009).                      
          05 SCSB91-NREG-X             REDEFINES                        
             SCSB91-NREG               PIC X(009).                      
          05 SCSB91-CD-OPER            PIC 9(010).                      
          05 SCSB91-CD-OPER-X          REDEFINES                        
             SCSB91-CD-OPER            PIC X(010).                      
          05 SCSB91-CD-PAIS-DESTNO     PIC 9(003).                      
          05 SCSB91-CD-PAIS-DESTNO-X   REDEFINES                        
             SCSB91-CD-PAIS-DESTNO     PIC X(003).                      
          05 SCSB91-CD-NBS             PIC X(009).                      
          05 SCSB91-CD-MODO-PREST      PIC 9(001).                      
          05 SCSB91-CD-MODO-PREST-X    REDEFINES                        
             SCSB91-CD-MODO-PREST      PIC X(001).                      
          05 SCSB91-DT-INICIO          PIC X(010).                      
          05 SCSB91-DT-CONCLUSAO       PIC X(010).                      
          05 SCSB91-VLR-OPER           PIC 9(013)V99.                   
          05 SCSB91-VLR-OPER-X         REDEFINES                        
             SCSB91-VLR-OPER           PIC X(015).                      
          05 SCSB91-ERROS.                                              
             10 SCSB91-COD-ERRO        PIC 9(003)                       
                                       OCCURS 100 TIMES.                
          05 FILLER                    PIC X(019).                      

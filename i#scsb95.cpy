      *================================================================*
      * NOME BOOK  : I#SCSB95                                          *
      * LRECL      : 400 - FB                                          *
      * DESCRICAO  : RECEPCAO DE ARQUIVO - DADOS OPERACAO DE FAT/PAG   *
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
                                                                        
       01 SCSB95-REG.                                                   
          05 SCSB95-NSEQ               PIC 9(009).                      
          05 SCSB95-TPO-SERVC          PIC 9(001).                      
          05 SCSB95-TPO-SERVC-X        REDEFINES                        
             SCSB95-TPO-SERVC          PIC X(001).                      
          05 SCSB95-NPROCS             PIC 9(004).                      
          05 SCSB95-NREG               PIC 9(009).                      
          05 SCSB95-NREG-X             REDEFINES                        
             SCSB95-NREG               PIC X(009).                      
          05 SCSB95-CD-FATMT-PGTO      PIC 9(010).                      
          05 SCSB95-CD-FATMT-PGTO-X    REDEFINES                        
             SCSB95-CD-FATMT-PGTO      PIC X(010).                      
          05 SCSB95-CD-OPER            PIC 9(010).                      
          05 SCSB95-CD-OPER-X          REDEFINES                        
             SCSB95-CD-OPER            PIC X(010).                      
          05 SCSB95-VLR-FATMT-PGTO     PIC 9(013)V99.                   
          05 SCSB95-VLR-FATMT-PGTO-X   REDEFINES                        
             SCSB95-VLR-FATMT-PGTO     PIC X(015).                      
          05 SCSB95-VLR-MANTD-EXTR     PIC 9(013)V99.                   
          05 SCSB95-VLR-MANTD-EXTR-X   REDEFINES                        
             SCSB95-VLR-MANTD-EXTR     PIC X(015).                      
          05 SCSB95-ERROS.                                              
             10 SCSB95-COD-ERRO        PIC 9(003)                       
                                       OCCURS 100 TIMES.                
          05 FILLER                    PIC X(027).                      

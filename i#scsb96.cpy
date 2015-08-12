      *================================================================*
      * NOME BOOK  : I#SCSB96                                          *
      * LRECL      : 360 - FB                                          *
      * DESCRICAO  : RECEPCAO DE ARQUIVO - DADOS RE/DI DO FATUR/PAGTO  *
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
                                                                        
       01 SCSB96-REG.                                                   
          05 SCSB96-NSEQ               PIC 9(009).                      
          05 SCSB96-TPO-SERVC          PIC 9(001).                      
          05 SCSB96-TPO-SERVC-X        REDEFINES                        
             SCSB96-TPO-SERVC          PIC X(001).                      
          05 SCSB96-NPROCS             PIC 9(004).                      
          05 SCSB96-NREG               PIC 9(009).                      
          05 SCSB96-NREG-X             REDEFINES                        
             SCSB96-NREG               PIC X(009).                      
          05 SCSB96-CD-FATMT-PGTO      PIC 9(010).                      
          05 SCSB96-CD-FATMT-PGTO-X    REDEFINES                        
             SCSB96-CD-FATMT-PGTO      PIC X(010).                      
          05 SCSB96-TPO-REG            PIC 9(001).                      
          05 SCSB96-TPO-REG-X          REDEFINES                        
             SCSB96-TPO-REG            PIC X(001).                      
          05 SCSB96-CD-RE-DI           PIC 9(012).                      
          05 SCSB96-CD-RE-DI-X         REDEFINES                        
             SCSB96-CD-RE-DI           PIC X(012).                      
          05 SCSB96-ERROS.                                              
             10 SCSB96-COD-ERRO        PIC 9(003)                       
                                       OCCURS 100 TIMES.                
          05 FILLER                    PIC X(014).                      

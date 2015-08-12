      *================================================================*
      * NOME BOOK  : I#SCSB90                                          *
      * LRECL      : 1750 - FB                                         *
      * DESCRICAO  : RECEPCAO DE ARQUIVO - DADOS PRINCIPAIS            *
      * COMUNICACAO: BATCH                                             *
      * DATA       : 17/07/2015                                        *
      * AUTOR      : FERNANDA CARUSO                                   *
      * EMPRESA    : BRQ IT SERVICES                                   *
      * GRUPO      : 17 - CONTABILIDADE E IMPOSTOS                     *
      * COMPONENTE : SCSB - SISCOSERV BRADESCO                         *
      *----------------------------------------------------------------*
      * DATA       AUTOR             ALTERACAO                         *
      * ---------- ----------------- --------------------------------- *
      * 99/99/9999                   XXXXXXXXXXXXXXXXXXXXXXX           *
      *================================================================*
                                                                        
       01 SCSB90-REG.                                                   
          05 SCSB90-NSEQ               PIC 9(009).                      
          05 SCSB90-TPO-SERVC          PIC 9(001).                      
          05 SCSB90-TPO-SERVC-X        REDEFINES                        
             SCSB90-TPO-SERVC          PIC X(001).                      
          05 SCSB90-NPROCS             PIC 9(004).                      
          05 SCSB90-NREG               PIC 9(009).                      
          05 SCSB90-NREG-X             REDEFINES                        
             SCSB90-NREG               PIC X(009).                      
          05 SCSB90-CNPJ-PRINC         PIC 9(009).                      
          05 SCSB90-CNPJ-PRINC-X       REDEFINES                        
             SCSB90-CNPJ-PRINC         PIC X(009).                      
          05 SCSB90-CNPJ-FLIAL         PIC 9(005).                      
          05 SCSB90-CNPJ-FLIAL-X       REDEFINES                        
             SCSB90-CNPJ-FLIAL         PIC X(005).                      
          05 SCSB90-CNPJ-CTRL          PIC 9(002).                      
          05 SCSB90-CNPJ-CTRL-X        REDEFINES                        
             SCSB90-CNPJ-CTRL          PIC X(002).                      
          05 SCSB90-EMPR-DEPEND        PIC 9(010).                      
          05 SCSB90-DEPEND             PIC 9(008).                      
          05 SCSB90-NOME               PIC X(150).                      
          05 SCSB90-ENDERECO           PIC X(150).                      
          05 SCSB90-CD-PAIS-RF         PIC 9(003).                      
          05 SCSB90-CD-PAIS-RF-X       REDEFINES                        
             SCSB90-CD-PAIS-RF         PIC X(003).                      
          05 SCSB90-NIF                PIC X(040).                      
          05 SCSB90-CD-MOEDA           PIC 9(005).                      
          05 SCSB90-CD-MOEDA-X         REDEFINES                        
             SCSB90-CD-MOEDA           PIC X(005).                      
          05 SCSB90-VLR-CONVERT        PIC 9(013)V99.                   
          05 SCSB90-VLR-CONVERT-X      REDEFINES                        
             SCSB90-VLR-CONVERT        PIC X(015).                      
          05 SCSB90-INF-COMPLEM        PIC X(1000).                     
          05 SCSB90-ERROS.                                              
             10 SCSB90-COD-ERRO        PIC 9(003)                       
                                       OCCURS 100 TIMES.                
          05 FILLER                    PIC X(030).                      

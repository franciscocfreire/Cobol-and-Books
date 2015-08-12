      *================================================================*
      * NOME BOOK  : I#SCSB89                                          *
      * LRECL      : 1459 - FB                                         *
      * DESCRICAO  : LAYOUT IGUAL AO I#SCSB99 + NUMERACAO SEQUENCIAL   *
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
                                                                        
       01 SCSB89-REG.                                                   
          05 SCSB89-NSEQ                    PIC 9(009).                 
          05 SCSB89-ID-REG                  PIC X(001).                 
          05 SCSB89-REGISTRO                PIC X(1390).                
          05 SCSB89-HEADER                  REDEFINES SCSB89-REGISTRO.  
             10 SCSB89-EMPR-DEPEND          PIC X(004).                 
             10 SCSB89-DEPEND               PIC 9(005).                 
             10 SCSB89-DATA                 PIC X(010).                 
             10 SCSB89-NPROCS               PIC 9(004).                 
             10 SCSB89-CUSUAR               PIC X(009).                 
             10 FILLER                      PIC X(1358).                
          05 SCSB89-TRAILLER                REDEFINES SCSB89-REGISTRO.  
             10 SCSB89-QTDE-REG             PIC 9(009).                 
             10 FILLER                      PIC X(1381).                
          05 SCSB89-DETALHE                 REDEFINES SCSB89-REGISTRO.  
             10 SCSB89-TPO-SERVC            PIC 9(001).                 
             10 SCSB89-NREG                 PIC 9(009).                 
             10 SCSB89-TPO-REG              PIC 9(001).                 
             10 SCSB89-REG-DET              PIC X(1379).                
             10 SCSB89-PRINCIPAL            REDEFINES SCSB89-REG-DET.   
                15 SCSB89-CNPJ-PRINC        PIC 9(009).                 
                15 SCSB89-CNPJ-FLIAL        PIC 9(005).                 
                15 SCSB89-CNPJ-CTRL         PIC 9(002).                 
                15 SCSB89-NOME              PIC X(150).                 
                15 SCSB89-ENDERECO          PIC X(150).                 
                15 SCSB89-CD-PAIS           PIC 9(003).                 
                15 SCSB89-NIF               PIC X(040).                 
                15 SCSB89-CD-MOEDA          PIC 9(005).                 
                15 SCSB89-VLR-CONVERT       PIC 9(013)V99.              
                15 SCSB89-INF-COMPLEM       PIC X(1000).                
             10 SCSB89-OPERACAO             REDEFINES SCSB89-REG-DET.   
                15 SCSB89-CD-OPER           PIC 9(010).                 
                15 SCSB89-CD-PAIS-DESTNO    PIC 9(003).                 
                15 SCSB89-CD-NBS            PIC X(009).                 
                15 SCSB89-CD-MODO-PREST     PIC 9(001).                 
                15 SCSB89-DT-INICIO         PIC X(010).                 
                15 SCSB89-DT-CONCLUSAO      PIC X(010).                 
                15 SCSB89-VLR-OPER          PIC 9(013)V99.              
                15 FILLER                   PIC X(1321).                
             10 SCSB89-ENQUADRAMENTO        REDEFINES SCSB89-REG-DET.   
                15 SCSB89-CD-OPER           PIC 9(010).                 
                15 SCSB89-CD-ENQUA          PIC 9(010).                 
                15 SCSB89-RC                PIC 9(008).                 
                15 FILLER                   PIC X(1351).                
             10 SCSB89-RE                   REDEFINES SCSB89-REG-DET.   
                15 SCSB89-CD-RE             PIC 9(012).                 
                15 FILLER                   PIC X(1367).                
             10 SCSB89-DI                   REDEFINES SCSB89-REG-DET.   
                15 SCSB89-CD-DI             PIC 9(010).                 
                15 FILLER                   PIC X(1369).                
             10 SCSB89-FATMT-PGTO           REDEFINES SCSB89-REG-DET.   
                15 SCSB89-CD-FATMT-PGTO     PIC 9(010).                 
                15 SCSB89-NOTA-FISCAL       PIC X(060).                 
                15 SCSB89-DT-FATMT-PGTO     PIC X(010).                 
                15 FILLER                   PIC X(1299).                
             10 SCSB89-OPER-FATMT-PGTO      REDEFINES SCSB89-REG-DET.   
                15 SCSB89-CD-FATMT-PGTO     PIC 9(010).                 
                15 SCSB89-CD-OPER           PIC 9(010).                 
                15 SCSB89-VLR-FATMT-PGTO    PIC 9(013)V99.              
                15 SCSB89-VLR-MANTD-EXTR    PIC 9(013)V99.              
                15 FILLER                   PIC X(1329).                
             10 SCSB89-RE-FATMT-PGTO        REDEFINES SCSB89-REG-DET.   
                15 SCSB89-CD-FATMT-PGTO     PIC 9(010).                 
                15 SCSB89-CD-RE             PIC 9(012).                 
                15 FILLER                   PIC X(1357).                
             10 SCSB89-DI-FATMT-PGTO        REDEFINES SCSB89-REG-DET.   
                15 SCSB89-CD-FATMT-PGTO     PIC 9(010).                 
                15 SCSB89-CD-DI             PIC 9(010).                 
                15 FILLER                   PIC X(1359).                
          05 SCSB89-COD-RETORNO             PIC 9(001).                 
          05 FILLER                         PIC X(058).                 

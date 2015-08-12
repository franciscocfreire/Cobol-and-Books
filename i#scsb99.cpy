      *================================================================*
      * NOME BOOK  : I#SCSB99                                          *
      * LRECL      : 1450 - FB                                         *
      * DESCRICAO  : LAYOUT DO ARQUIVO DE SERVICOS RECEPCIONADO PARA   *
      *              CARGA NAS TABELAS DO SCSB.                        *
      * COMUNICACAO: BATCH                                             *
      * DATA       : 06/07/2015                                        *
      * AUTOR      : FERNANDA CARUSO                                   *
      * EMPRESA    : BRQ IT SERVICES                                   *
      * GRUPO      : 17 - CONTABILIDADE E IMPOSTOS                     *
      * COMPONENTE : SCSB - SISCOSERV BRADESCO                         *
      *----------------------------------------------------------------*
      * DATA       AUTOR             ALTERACAO                         *
      * ---------- ----------------- --------------------------------- *
      * 99/99/9999                   XXXXXXXXXXXXXXXXXXXXXXX           *
      *================================================================*
                                                                        
       01 SCSB99-REG.                                                   
          05 SCSB99-ID-REG                  PIC X(001).                 
          05 SCSB99-REGISTRO                PIC X(1390).                
          05 SCSB99-HEADER                  REDEFINES SCSB99-REGISTRO.  
             10 SCSB99-EMPR-DEPEND          PIC X(004).                 
             10 SCSB99-DEPEND               PIC 9(005).                 
             10 SCSB99-DATA                 PIC X(010).                 
             10 SCSB99-NPROCS               PIC 9(004).                 
             10 SCSB99-CUSUAR               PIC X(009).                 
             10 FILLER                      PIC X(1358).                
          05 SCSB99-TRAILLER                REDEFINES SCSB99-REGISTRO.  
             10 SCSB99-QTDE-REG             PIC 9(009).                 
             10 FILLER                      PIC X(1381).                
          05 SCSB99-DETALHE                 REDEFINES SCSB99-REGISTRO.  
             10 SCSB99-TPO-SERVC            PIC 9(001).                 
             10 SCSB99-NREG                 PIC 9(009).                 
             10 SCSB99-TPO-REG              PIC 9(001).                 
             10 SCSB99-REG-DET              PIC X(1379).                
             10 SCSB99-PRINCIPAL            REDEFINES SCSB99-REG-DET.   
                15 SCSB99-CNPJ-PRINC        PIC 9(009).                 
                15 SCSB99-CNPJ-FLIAL        PIC 9(005).                 
                15 SCSB99-CNPJ-CTRL         PIC 9(002).                 
                15 SCSB99-NOME              PIC X(150).                 
                15 SCSB99-ENDERECO          PIC X(150).                 
                15 SCSB99-CD-PAIS           PIC 9(003).                 
                15 SCSB99-NIF               PIC X(040).                 
                15 SCSB99-CD-MOEDA          PIC 9(005).                 
                15 SCSB99-VLR-CONVERT       PIC 9(013)V99.              
                15 SCSB99-INF-COMPLEM       PIC X(1000).                
             10 SCSB99-OPERACAO             REDEFINES SCSB99-REG-DET.   
                15 SCSB99-CD-OPER           PIC 9(010).                 
                15 SCSB99-CD-PAIS-DESTNO    PIC 9(003).                 
                15 SCSB99-CD-NBS            PIC X(009).                 
                15 SCSB99-CD-MODO-PREST     PIC 9(001).                 
                15 SCSB99-DT-INICIO         PIC X(010).                 
                15 SCSB99-DT-CONCLUSAO      PIC X(010).                 
                15 SCSB99-VLR-OPER          PIC 9(013)V99.              
                15 FILLER                   PIC X(1321).                
             10 SCSB99-ENQUADRAMENTO        REDEFINES SCSB99-REG-DET.   
                15 SCSB99-CD-OPER           PIC 9(010).                 
                15 SCSB99-CD-ENQUA          PIC 9(010).                 
                15 SCSB99-RC                PIC 9(008).                 
                15 FILLER                   PIC X(1351).                
             10 SCSB99-RE                   REDEFINES SCSB99-REG-DET.   
                15 SCSB99-CD-RE             PIC 9(012).                 
                15 FILLER                   PIC X(1367).                
             10 SCSB99-DI                   REDEFINES SCSB99-REG-DET.   
                15 SCSB99-CD-DI             PIC 9(010).                 
                15 FILLER                   PIC X(1369).                
             10 SCSB99-FATMT-PGTO           REDEFINES SCSB99-REG-DET.   
                15 SCSB99-CD-FATMT-PGTO     PIC 9(010).                 
                15 SCSB99-NOTA-FISCAL       PIC X(060).                 
                15 SCSB99-DT-FATMT-PGTO     PIC X(010).                 
                15 FILLER                   PIC X(1299).                
             10 SCSB99-OPER-FATMT-PGTO      REDEFINES SCSB99-REG-DET.   
                15 SCSB99-CD-FATMT-PGTO     PIC 9(010).                 
                15 SCSB99-CD-OPER           PIC 9(010).                 
                15 SCSB99-VLR-FATMT-PGTO    PIC 9(013)V99.              
                15 SCSB99-VLR-MANTD-EXTR    PIC 9(013)V99.              
                15 FILLER                   PIC X(1329).                
             10 SCSB99-RE-FATMT-PGTO        REDEFINES SCSB99-REG-DET.   
                15 SCSB99-CD-FATMT-PGTO     PIC 9(010).                 
                15 SCSB99-CD-RE             PIC 9(012).                 
                15 FILLER                   PIC X(1357).                
             10 SCSB99-DI-FATMT-PGTO        REDEFINES SCSB99-REG-DET.   
                15 SCSB99-CD-FATMT-PGTO     PIC 9(010).                 
                15 SCSB99-CD-DI             PIC 9(010).                 
                15 FILLER                   PIC X(1359).                
          05 SCSB99-COD-RETORNO             PIC 9(001).                 
          05 FILLER                         PIC X(058).                 

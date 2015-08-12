      *================================================================*
      * NOME BOOK  : I#SCSB51                                          *
      * LRECL      : 5500 - FB                                         *
      * DESCRICAO  : DADOS P/ BAIXA GERAR XML - TIPO INCLUSAO/RETIFIC. *
      * COMUNICACAO: BATCH                                             *
      * DATA       : 17/05/2013                                        *
      * AUTOR      : FERNANDA CARUSO                                   *
      * EMPRESA    : BRQ IT SERVICES                                   *
      * GRUPO      : 17 - CONTABILIDADE E IMPOSTOS                     *
      * COMPONENTE : SCSB - SISCOSERV BRADESCO                         *
      *----------------------------------------------------------------*
      * DATA       AUTOR             ALTERACAO                         *
      * ---------- ----------------- --------------------------------- *
      * 01/04/2015 FERNANDA CARUSO   EXCLUSAO DO CAMPO SCSB51-EMPRESA. *
      *                              INCLUSAO DOS CAMPOS SCSB51-       *
      *                              CNPJ-PRINC/CNPJ-FLIAL/CNPJ-CTRL/  *
      *                              DEPEND/DT-SOLICIT.                *
      *                              ALTERACAO DO FORMATO/TAMANHO DO   *
      *                              CAMPO SCSB51-RC DE X(06) P/ 9(08).*
      *                              AUMENTO DO FILLER DE 33 PARA 91.  *
      *================================================================*
                                                                        
       01 SCSB51-REG.                                                   
          05 SCSB51-ID-REG                  PIC X(001).                 
          05 SCSB51-REGISTRO                PIC X(5408).                
          05 SCSB51-HEADER                  REDEFINES SCSB51-REGISTRO.  
             10 SCSB51-DATA                 PIC X(010).                 
             10 SCSB51-NUM-LOTE.                                        
                15 SCSB51-DANO-LOTE         PIC 9(004).                 
                15 SCSB51-CTPO-LOTE         PIC 9(001).                 
                15 SCSB51-NLOTE             PIC 9(009).                 
             10 FILLER                      PIC X(5384).                
          05 SCSB51-DETALHE                 REDEFINES SCSB51-REGISTRO.  
             10 SCSB51-CNPJ-PRINC           PIC 9(009).                 
             10 SCSB51-CNPJ-FLIAL           PIC 9(004).                 
             10 SCSB51-CNPJ-CTRL            PIC 9(002).                 
             10 SCSB51-DEPEND               PIC 9(005).                 
             10 SCSB51-TPO-REG              PIC 9(001).                 
             10 SCSB51-DANO-RECTA           PIC 9(004).                 
             10 SCSB51-NREG-RECTA           PIC 9(009).                 
             10 SCSB51-DT-SOLICIT           PIC X(026).                 
             10 SCSB51-REG-DET              PIC X(5348).                
             10 SCSB51-PRINCIPAL            REDEFINES SCSB51-REG-DET.   
                15 SCSB51-NOME              PIC X(150).                 
                15 SCSB51-ENDERECO          PIC X(150).                 
                15 SCSB51-CD-PAIS           PIC 9(003).                 
                15 SCSB51-NIF               PIC X(040).                 
                15 SCSB51-CD-MOEDA          PIC 9(005).                 
                15 SCSB51-INF-COMPLEM       PIC X(5000).                
             10 SCSB51-OPERACAO             REDEFINES SCSB51-REG-DET.   
                15 SCSB51-COPER             PIC 9(010).                 
                15 SCSB51-CD-NBS            PIC X(009).                 
                15 SCSB51-CD-PAIS-DESTNO    PIC 9(003).                 
                15 SCSB51-CD-MODO-PREST     PIC 9(001).                 
                15 SCSB51-DT-INICIO         PIC X(010).                 
                15 SCSB51-DT-CONCLUSAO      PIC X(010).                 
                15 SCSB51-VLR-OPER          PIC 9(013)V99.              
                15 FILLER                   PIC X(5290).                
             10 SCSB51-ENQUADRAMENTO        REDEFINES SCSB51-REG-DET.   
                15 SCSB51-COPER-ENQUA       PIC 9(010).                 
                15 SCSB51-CD-ENQUA          PIC 9(010).                 
                15 SCSB51-RC                PIC 9(008).                 
                15 SCSB51-ACAO-ENQUA        PIC X(001).                 
                15 FILLER                   PIC X(5319).                
             10 SCSB51-RE                   REDEFINES SCSB51-REG-DET.   
                15 SCSB51-CD-RE             PIC 9(012).                 
                15 SCSB51-ACAO-RE           PIC X(001).                 
                15 FILLER                   PIC X(5335).                
             10 SCSB51-DI                   REDEFINES SCSB51-REG-DET.   
                15 SCSB51-CD-DI             PIC 9(010).                 
                15 SCSB51-ACAO-DI           PIC X(001).                 
                15 FILLER                   PIC X(5337).                
          05 FILLER                         PIC X(091).                 

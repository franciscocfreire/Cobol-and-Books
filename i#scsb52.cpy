      *================================================================*
      * NOME BOOK  : I#SCSB52                                          *
      * LRECL      : 150 - FB                                          *
      * DESCRICAO  : DADOS P/ BAIXA GERAR XML - TIPO ADITIVO (INCL/RET)*
      * COMUNICACAO: BATCH                                             *
      * DATA       : 24/07/2013                                        *
      * AUTOR      : FERNANDA CARUSO                                   *
      * EMPRESA    : BRQ IT SERVICES                                   *
      * GRUPO      : 17 - CONTABILIDADE E IMPOSTOS                     *
      * COMPONENTE : SCSB - SISCOSERV BRADESCO                         *
      *----------------------------------------------------------------*
      * DATA       AUTOR             ALTERACAO                         *
      * ---------- ----------------- --------------------------------- *
      * 01/04/2015 FERNANDA CARUSO   EXCLUSAO DO CAMPO SCSB52-EMPRESA. *
      *                              INCLUSAO DOS CAMPOS SCSB52-       *
      *                              CNPJ-PRINC/CNPJ-FLIAL/CNPJ-CTRL/  *
      *                              DEPEND/DT-SOLICIT.                *
      *                              ALTERACAO DO FORMATO/TAMANHO DO   *
      *                              CAMPO SCSB52-RC DE X(06) P/ 9(08).*
      *                              AUMENTO DO FILLER DE 23 PARA 31.  *
      *================================================================*
                                                                        
       01 SCSB52-REG.                                                   
          05 SCSB52-ID-REG                  PIC X(001).                 
          05 SCSB52-REGISTRO                PIC X(118).                 
          05 SCSB52-HEADER                  REDEFINES SCSB52-REGISTRO.  
             10 SCSB52-DATA                 PIC X(010).                 
             10 SCSB52-NUM-LOTE.                                        
                15 SCSB52-DANO-LOTE         PIC 9(004).                 
                15 SCSB52-CTPO-LOTE         PIC 9(001).                 
                15 SCSB52-NLOTE             PIC 9(009).                 
             10 FILLER                      PIC X(094).                 
          05 SCSB52-DETALHE                 REDEFINES SCSB52-REGISTRO.  
             10 SCSB52-CNPJ-PRINC           PIC 9(009).                 
             10 SCSB52-CNPJ-FLIAL           PIC 9(004).                 
             10 SCSB52-CNPJ-CTRL            PIC 9(002).                 
             10 SCSB52-DEPEND               PIC 9(005).                 
             10 SCSB52-TPO-REG              PIC 9(001).                 
             10 SCSB52-DANO-RECTA           PIC 9(004).                 
             10 SCSB52-NREG-RECTA           PIC 9(009).                 
             10 SCSB52-COPER                PIC 9(010).                 
             10 SCSB52-DT-SOLICIT           PIC X(026).                 
             10 SCSB52-REG-DET              PIC X(048).                 
             10 SCSB52-OPERACAO             REDEFINES SCSB52-REG-DET.   
                15 SCSB52-CD-NBS            PIC X(009).                 
                15 SCSB52-CD-PAIS-DESTNO    PIC 9(003).                 
                15 SCSB52-CD-MODO-PREST     PIC 9(001).                 
                15 SCSB52-DT-INICIO         PIC X(010).                 
                15 SCSB52-DT-CONCLUSAO      PIC X(010).                 
                15 SCSB52-VLR-OPER          PIC 9(013)V99.              
             10 SCSB52-ENQUADRAMENTO        REDEFINES SCSB52-REG-DET.   
                15 SCSB52-CD-ENQUA          PIC 9(010).                 
                15 SCSB52-RC                PIC 9(008).                 
                15 SCSB52-ACAO-ENQUA        PIC X(001).                 
                15 FILLER                   PIC X(029).                 
          05 FILLER                         PIC X(031).                 

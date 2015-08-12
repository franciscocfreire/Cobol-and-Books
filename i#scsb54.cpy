      *================================================================*
      * NOME BOOK  : I#SCSB54                                          *
      * LRECL      : 100 - FB                                          *
      * DESCRICAO  : DADOS P/ BAIXA GERAR XML - TIPO FATUR/PAGTO (CANC)*
      * COMUNICACAO: BATCH                                             *
      * DATA       : 25/07/2013                                        *
      * AUTOR      : FERNANDA CARUSO                                   *
      * EMPRESA    : BRQ IT SERVICES                                   *
      * GRUPO      : 17 - CONTABILIDADE E IMPOSTOS                     *
      * COMPONENTE : SCSB - SISCOSERV BRADESCO                         *
      *----------------------------------------------------------------*
      * DATA       AUTOR             ALTERACAO                         *
      * ---------- ----------------- --------------------------------- *
      * 01/04/2015 FERNANDA CARUSO   EXCLUSAO DO CAMPO SCSB54-EMPRESA. *
      *                              INCLUSAO DOS CAMPOS SCSB54-       *
      *                              CNPJ-PRINC/CNPJ-FLIAL/CNPJ-CTRL/  *
      *                              DEPEND/DT-SOLICIT.                *
      *                              AUMENTO DO FILLER DE 22 PARA 30.  *
      *================================================================*
                                                                        
       01 SCSB54-REG.                                                   
          05 SCSB54-ID-REG                  PIC X(001).                 
          05 SCSB54-REGISTRO                PIC X(069).                 
          05 SCSB54-HEADER                  REDEFINES SCSB54-REGISTRO.  
             10 SCSB54-DATA                 PIC X(010).                 
             10 SCSB54-NUM-LOTE.                                        
                15 SCSB54-DANO-LOTE         PIC 9(004).                 
                15 SCSB54-CTPO-LOTE         PIC 9(001).                 
                15 SCSB54-NLOTE             PIC 9(009).                 
             10 FILLER                      PIC X(045).                 
          05 SCSB54-DETALHE                 REDEFINES SCSB54-REGISTRO.  
             10 SCSB54-CNPJ-PRINC           PIC 9(009).                 
             10 SCSB54-CNPJ-FLIAL           PIC 9(004).                 
             10 SCSB54-CNPJ-CTRL            PIC 9(002).                 
             10 SCSB54-DEPEND               PIC 9(005).                 
             10 SCSB54-DANO-RECTA           PIC 9(004).                 
             10 SCSB54-NREG-RECTA           PIC 9(009).                 
             10 SCSB54-NFATMT-NPGTO         PIC 9(010).                 
             10 SCSB54-DT-SOLICIT           PIC X(026).                 
          05 FILLER                         PIC X(030).                 

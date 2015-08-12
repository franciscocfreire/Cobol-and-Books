      *================================================================*
      * NOME BOOK  : I#SCSB53                                          *
      * LRECL      : 170 - FB                                          *
      * DESCRICAO  : DADOS P/ BAIXA GERAR XML - TIPO FATUR/PAGTO (INCL)*
      * COMUNICACAO: BATCH                                             *
      * DATA       : 25/07/2013                                        *
      * AUTOR      : FERNANDA CARUSO                                   *
      * EMPRESA    : BRQ IT SERVICES                                   *
      * GRUPO      : 17 - CONTABILIDADE E IMPOSTOS                     *
      * COMPONENTE : SCSB - SISCOSERV BRADESCO                         *
      *----------------------------------------------------------------*
      * DATA       AUTOR             ALTERACAO                         *
      * ---------- ----------------- --------------------------------- *
      * 01/04/2015 FERNANDA CARUSO   EXCLUSAO DO CAMPO SCSB53-EMPRESA. *
      *                              INCLUSAO DOS CAMPOS SCSB53-       *
      *                              CNPJ-PRINC/CNPJ-FLIAL/CNPJ-CTRL/  *
      *                              DEPEND/DT-SOLICIT.                *
      *                              AUMENTO DO FILLER DE 21 PARA 29.  *
      *================================================================*
                                                                        
       01 SCSB53-REG.                                                   
          05 SCSB53-ID-REG                  PIC X(001).                 
          05 SCSB53-REGISTRO                PIC X(140).                 
          05 SCSB53-HEADER                  REDEFINES SCSB53-REGISTRO.  
             10 SCSB53-DATA                 PIC X(010).                 
             10 SCSB53-NUM-LOTE.                                        
                15 SCSB53-DANO-LOTE         PIC 9(004).                 
                15 SCSB53-CTPO-LOTE         PIC 9(001).                 
                15 SCSB53-NLOTE             PIC 9(009).                 
             10 FILLER                      PIC X(116).                 
          05 SCSB53-DETALHE                 REDEFINES SCSB53-REGISTRO.  
             10 SCSB53-CNPJ-PRINC           PIC 9(009).                 
             10 SCSB53-CNPJ-FLIAL           PIC 9(004).                 
             10 SCSB53-CNPJ-CTRL            PIC 9(002).                 
             10 SCSB53-DEPEND               PIC 9(005).                 
             10 SCSB53-TPO-REG              PIC 9(001).                 
             10 SCSB53-DANO-RECTA           PIC 9(004).                 
             10 SCSB53-NREG-RECTA           PIC 9(009).                 
             10 SCSB53-NFATMT-NPGTO         PIC 9(010).                 
             10 SCSB53-DT-SOLICIT           PIC X(026).                 
             10 SCSB53-REG-DET              PIC X(070).                 
             10 SCSB53-PRINCIPAL            REDEFINES SCSB53-REG-DET.   
                15 SCSB53-CNOTA             PIC X(060).                 
                15 SCSB53-DFATMT-DPGTO      PIC X(010).                 
             10 SCSB53-OPERACAO             REDEFINES SCSB53-REG-DET.   
                15 SCSB53-COPER             PIC 9(010).                 
                15 SCSB53-VFATMT-VPGTO      PIC 9(013)V99.              
                15 SCSB53-VMANTD-EXTER      PIC 9(013)V99.              
                15 FILLER                   PIC X(030).                 
             10 SCSB53-RE                   REDEFINES SCSB53-REG-DET.   
                15 SCSB53-CD-RE             PIC 9(012).                 
                15 FILLER                   PIC X(058).                 
             10 SCSB53-DI                   REDEFINES SCSB53-REG-DET.   
                15 SCSB53-CD-DI             PIC 9(010).                 
                15 FILLER                   PIC X(060).                 
          05 FILLER                         PIC X(029).                 

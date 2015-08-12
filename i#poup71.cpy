      *================================================================*
      * NOME BOOK  : I#POUP71                                          *
      * LRECL      : 70 - FB                                           *
      * DESCRICAO  : INFORMACOES PARA GERAR ARQUIVO PARA O BACEN.      *
      *              VALOR TOTAL DOS DEPOSITOS/SAQUES E VALOR DA       *
      *              RENDA SALARIAL POR CPF.                           *
      * COMUNICACAO: BATCH                                             *
      * DATA       : 07/08/2015                                        *
      * AUTOR      : FERNANDA CARUSO                                   *
      * EMPRESA    : BRQ IT SERVICES                                   *
      * GRUPO      : 45 - CONTAS E TRANSFERENCIAS                      *
      *----------------------------------------------------------------*
      * DATA       AUTOR             ALTERACAO                         *
      * ---------- ----------------- --------------------------------- *
      * 99/99/9999                   XXXXXXXXXXXXXXXXXXXXXXX           *
      *================================================================*
                                                                        
       01 POUP71-REG.                                                   
          05 POUP71-CPF-PRINC          PIC 9(09)    COMP-3.             
          05 POUP71-CPF-CTRL           PIC 9(02)    COMP-3.             
          05 POUP71-VMOVTO-DEP         PIC 9(15)V99 COMP-3.             
          05 POUP71-VMOVTO-SAQ         PIC 9(15)V99 COMP-3.             
          05 POUP71-VRENDA             PIC 9(13)V99 COMP-3.             
          05 POUP71-FAIXA-RENDA-DE     PIC 9(13)V99 COMP-3.             
          05 POUP71-FAIXA-RENDA-ATE    PIC 9(13)V99 COMP-3.             
          05 FILLER                    PIC X(21).                       

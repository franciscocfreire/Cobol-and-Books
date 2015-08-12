      *================================================================*
      * NOME BOOK  : I#POUP70                                          *
      * LRECL      : 50 - FB                                           *
      * DESCRICAO  : INFORMACOES PARA GERAR ARQUIVO PARA O BACEN.      *
      *              VALOR TOTAL DOS DEPOSITOS E SAQUES POR CONTA.     *
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
                                                                        
       01 POUP70-REG.                                                   
          05 POUP70-CJUNC-DEPDC        PIC 9(05)    COMP-3.             
          05 POUP70-CCTA-POUP          PIC 9(07)    COMP-3.             
          05 POUP70-CPF-PRINC          PIC 9(09)    COMP-3.             
          05 POUP70-CPF-CTRL           PIC 9(02)    COMP-3.             
          05 POUP70-VMOVTO-DEP         PIC 9(15)V99 COMP-3.             
          05 POUP70-VMOVTO-SAQ         PIC 9(15)V99 COMP-3.             
          05 FILLER                    PIC X(18).                       

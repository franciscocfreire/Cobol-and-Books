      ******************************************************************
      **                                                              **
      ** INCLUDE  : I#CKRS04                                          **
      **    DATA  : MAIO/2006                                         **
      **   AUTOR  : CPM                                               **
      ** SISTEMA  : CKRS-CHECKPOINT E RESTART                         **
      ** OBJETIVO : AREA DE COMUNICACAO DO PGM CKRS0100 (INTERFACE)   **
      **            COM O PGM CKRS0105 (CONEXAO/DESCONEXAO RRS).      **
      **                                                              **
      ** -----------  ----------------------------------------------  **
      ** CAMPOS       DESCRICAO DOS CAMPOS                            **
      ** -----------  ----------------------------------------------  **
      ** PARM-OP      CAMPO DE OPCAO DE PROCESSAMENTO                 **
      **                  C - ESTABELECE A CONEXAO DB2 RRS            **
      **                  D - ESTABELECE A DESCONEXAO DB2 RRS         **
      **                  P - EFETUA COMMIT RRS                       **
      ** PARM-SSID    NOME DO SSID(LIBNAME)                           **
      ** PARM-PLAN    NOME DO PLANO                                   **
      ** PARM-CORRID  NOME DA CORRELACAO                              **
      **                                                              **
      ******************************************************************
       01  PARM-CKRS0105.                                               
           05  PARM-OP                   PIC  X(001).                   
           05  PARM-SSID                 PIC  X(004).                   
           05  PARM-PLAN                 PIC  X(008).                   
           05  PARM-CORRID               PIC  X(012).                   
      ******************   FIM COPY I#CKRS04   *************************

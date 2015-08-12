      *================================================================*
      * NOME BOOK : I#POUPSL                                           *
      * TAMANHO   : 50                                                 *
      * DESCRICAO : LAYOUT DO ARQUIVO DE SALDOS DAS CONTAS ESTOQUE E   *
      *             MP. BASE TABELA POUPB005.                          *
      * EMPRESA   : BRQ IT SERVICES                                    *
      *================================================================*
                                                                        
       01  REG-POUPSL.                                                  
           05  POUPSL-AGE-DE           PIC  9(05)      COMP-3.          
           05  POUPSL-CTA-DE           PIC  9(07)      COMP-3.          
           05  POUPSL-SLD-DE           PIC S9(13)V9(2) COMP-3.          
           05  POUPSL-AGE-PARA         PIC  9(05)      COMP-3.          
           05  POUPSL-CTA-PARA         PIC  9(07)      COMP-3.          
           05  POUPSL-SLD-PARA         PIC S9(13)V9(2) COMP-3.          
           05  POUPSL-OPCAO-CLIE       PIC  X(01).                      
           05  FILLER                  PIC  X(19).                      

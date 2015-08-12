      ******************************************************************
      * DESCRICAO : AREA DE COMUNICACAO COM O MODULO DE DATA DE MVTO   *
      * BOOK      : RECRWDAA                                           *
      * DATA      : 09/03/2012                                         *
      * AUTOR     : CESAR SOARES PEREGO                                *
      * EMPRESA   : CPM BRAXIS                                         *
      * GRUPO     : TI MELHORIAS                                       *
      * COMPONENTE: RECR                                               *
      * TAMANHO   : 285 BYTES                                          *
      ******************************************************************
           05 RECRWDAA-HEADER.                                          
              10 RECRWDAA-COD-LAYOUT       PIC  X(08)  VALUE 'RECRWDAA'.
              10 RECRWDAA-TAM-LAYOUT       PIC  9(05)  VALUE 285.       
           05 RECRWDAA-REGISTRO.                                        
              10 RECRWDAA-AREA-SAIDA.                                   
                 15 RECRWDAA-DT-MVTO-P-DDMMAAAA    PIC  X(10).          
                 15 RECRWDAA-DT-MVTO-AAAAMMDD      PIC  9(08).          
                 15 RECRWDAA-DT-ACAO-P-DDMMAAAA    PIC  X(10).          
                 15 RECRWDAA-DT-ACAO-AAAAMMDD      PIC  9(08).          
              10 RECRWDAA-BLOCO-RETORNO.                                
                 15 RECRWDAA-COD-RETORNO           PIC  9(02).          
                 15 RECRWDAA-COD-ERRO              PIC  X(04).          
                 15 RECRWDAA-COD-MENSAGEM          PIC  X(08).          
              10 RECRWDAA-AREA-ERRO-DB2            PIC  X(222).         

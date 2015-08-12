      ***************************************************************** 
      ***                                                            ** 
      *** INCLUDE  : I#CKRS01                                        ** 
      ***    DATA  : AGOSTO/2006                                     ** 
      ***   AUTOR  : CPM                                             ** 
      *** SISTEMA  : CKRS-CHECKPOINT E RESTART                       ** 
      *** OBJETIVO : AREA DE COMUNICACAO DAS APLICACOES COM O        ** 
      ***            PROGRAMA CKRS0100 (INTERFACE).                  ** 
      ***                                                            ** 
      ***----------------------------------------------------------- ** 
      *** CAMPOS                 DESCRICAO DOS CAMPOS                ** 
      *** ---------------------  ----------------------------------- ** 
      *** CK01-PLAN              -  QUANDO PLAN DIFERENTE CHAMADOR   ** 
      *** CK01-ID-DB2            -  INDICA DB2 QUE ESTA A APLICACAO  ** 
      *** CK01-FUNCAO            -  CODIGO DA FUNCAO (CHAMADA)       ** 
      ***                           'I' - INICIALIZA FUNCOES         ** 
      ***                           'P' - EFETUA CHECKPOINT (COMMIT) ** 
      ***                           'F' - FINALIZA FUNCOES           ** 
      *** CK01-STATUS            -  SITUACAO DO PROCESSAMENTO        ** 
      ***                           '    ' - PROCESSAMENTO NORMAL    ** 
      ***                           'REST' - PROCESSAMENTO RESTART   ** 
      *** CK01-TAM-AREA-RESTART  -  TAMANHO AREA USADA P/ RESTART    ** 
      *** CK01-AREA-RESTART      -  DESCRICAO AREA USADA P/ RESTART  ** 
      *** CK01-CODIGO-RETORNO    -  CODIGO DE RETORNO (CKRS0100)     ** 
      *** CK01-CODIGO-MENSAGEM   -  CODIGO DA MENSAGEM               ** 
      *** CK01-TEXTO-MENSAGEM    -  TEXTO DA MENSAGEM                ** 
      *** CK01-FILLER            -  AREA DE USO LIVRE                ** 
      ***************************************************************** 
       01  CKRS01-INTERFACE.                                            
           05  CK01-PLAN               PIC X(008).                      
           05  CK01-ID-DB2             PIC X(004).                      
           05  CK01-FUNCAO             PIC X(001).                      
               88  CK01-INICIALIZAR             VALUE 'I'.              
               88  CK01-CHECKPOINT              VALUE 'P'.              
               88  CK01-FINALIZAR               VALUE 'F'.              
           05  CK01-STATUS             PIC X(004).                      
               88  CK01-NORMAL                  VALUE SPACES.           
               88  CK01-RESTART                 VALUE 'REST'.           
           05  CK01-TAM-AREA-RESTART   PIC S9(005) COMP.                
           05  CK01-AREA-RESTART       PIC X(4096).                     
           05  CK01-CODIGO-RETORNO     PIC 9(002).                      
           05  CK01-CODIGO-MENSAGEM    PIC X(008).                      
           05  CK01-TEXTO-MENSAGEM     PIC X(080).                      
           05  CK01-FILLER             PIC X(100).                      
                                                                        

      ******************************************************            
      *                                                    *            
      * NOME BOOK : FRWKGERR                               *            
      * DESCRICAO : COMMAREA DE GESTOR DE ERROS            *            
      * DATA      : 29/08/2006                             *            
      * AUTOR     : CARLA PARALS SENDIN                    *            
      * EMPRESA   : CPM/GFT                                *            
      * GRUPO     : TI MELHORIAS                           *            
      * COMPONENTE: FRAMEWORK ONLINE                       *            
      *                                                    *            
      ******************************************************            
      * FRWKGERR.                                          *            
      *   FRWKGERR-HEADER.                                 *            
      *     FRWKGERR-COD-LAYOUT      = CODIGO DE LAYOUT    *            
      *     FRWKGERR-TAM-LAYOUT      = TAMANHO DO LAYOUT   *            
      *   FRWKGERR-REGISTRO.                               *            
      *     FRWKGERR-BLOCO-RETORNO.                        *            
      *       FRWKGERR-COD-RETORNO   = CODIGO DE RETORNO   *            
      *       FRWKGERR-COD-ERRO      = CODIGO DE ERRO      *            
      *       FRWKGERR-COD-MENSAGEM  = CODIGO DA MENSAGEM  *            
      *     FRWKGERR-BLOCO-HEADER.                         *            
      *       FRWKGHEA               = INCLUDE COPY HEADER *            
      *                                                    *            
      ******************************************************            
      * DATA       AUTOR        MODIFICACAO                *            
      * --------   ---------    -------------------------- *            
      * DD/MM/AAAA JNNNNNN      XXXXXXXXXXXXXXXXXXXXXXXXXX *            
      ******************************************************            
        03 FRWKGERR.                                                    
           05 FRWKGERR-HEADER.                                          
              07 FRWKGERR-COD-LAYOUT PIC X(008) VALUE 'FRWKGERR'.       
              07 FRWKGERR-TAM-LAYOUT PIC 9(005) VALUE 85.               
           05 FRWKGERR-REGISTRO.                                        
              07 FRWKGERR-BLOCO-RETORNO.                                
                 09 FRWKGERR-COD-RETORNO          PIC 9(002).           
                 09 FRWKGERR-COD-ERRO             PIC X(004).           
                    88 FRWKGERR-TIPO-ERRO-INCOR   VALUE '0001'.         
                    88 FRWKGERR-NOME-PROG-INCOR   VALUE '0002'.         
                    88 FRWKGERR-IDEN-PARA-INCOR   VALUE '0003'.         
                    88 FRWKGERR-TAM-DADOS-INCOR   VALUE '0004'.         
                    88 FRWKGERR-SQLCODE-INCOR     VALUE '0005'.         
                    88 FRWKGERR-NOME-TABELA-INCOR VALUE '0006'.         
                    88 FRWKGERR-COMANDO-DB2-INCOR VALUE '0007'.         
                    88 FRWKGERR-EIBRESP-INCOR     VALUE '0008'.         
                    88 FRWKGERR-EIBFN-INCOR       VALUE '0009'.         
                    88 FRWKGERR-EIBTASKN-INCOR    VALUE '0010'.         
                    88 FRWKGERR-PROG-MQ-INCOR     VALUE '0011'.         
                    88 FRWKGERR-NOME-FILA-INCOR   VALUE '0012'.         
                    88 FRWKGERR-FUNCAO-MQ-INCOR   VALUE '0013'.         
                    88 FRWKGERR-COMPL-CODE-INCOR  VALUE '0014'.         
                    88 FRWKGERR-NOME-MODULO-INCOR VALUE '0015'.         
                    88 FRWKGERR-COD-RETOR-INCOR   VALUE '0016'.         
                    88 FRWKGERR-PREFIXO-INCOR     VALUE '0017'.         
                    88 FRWKGERR-COD-MENS-INCOR    VALUE '0018'.         
                    88 FRWKGERR-FSTATUS-INCOR     VALUE '0019'.         
                    88 FRWKGERR-NOME-ARQ-INCOR    VALUE '0020'.         
                    88 FRWKGERR-COMAND-ARQ-IMCOR  VALUE '0021'.         
                    88 FRWKGERR-IDIOMA-NAO-NUM    VALUE '0022'.         
                    88 FRWKGERR-ERRO-CICS-GMSG    VALUE '0023'.         
                    88 FRWKGERR-ERRO-GMSG         VALUE '0024'.         
                    88 FRWKGERR-ERRO-CICS-GLOG    VALUE '0025'.         
                    88 FRWKGERR-ERRO-GLOG         VALUE '0026'.         
                    88 FRWKGERR-ERRO-CICS-GPAR    VALUE '0027'.         
                    88 FRWKGERR-ERRO-GPAR         VALUE '0028'.         
                    88 FRWKGERR-COD-MENS-NAO-NUM  VALUE '0029'.         
                    88 FRWKGERR-TAM-DAD01-INCOR   VALUE '0030'.         
                    88 FRWKGERR-TAM-DAD02-INCOR   VALUE '0031'.         
                    88 FRWKGERR-TAM-DAD03-INCOR   VALUE '0032'.         
                    88 FRWKGERR-TAM-DAD04-INCOR   VALUE '0033'.         
                    88 FRWKGERR-TAM-DAD05-INCOR   VALUE '0034'.         
                    88 FRWKGERR-TAM-DAD06-INCOR   VALUE '0035'.         
                    88 FRWKGERR-TAM-DAD07-INCOR   VALUE '0036'.         
                    88 FRWKGERR-TAM-DAD08-INCOR   VALUE '0037'.         
                    88 FRWKGERR-TAM-DAD09-INCOR   VALUE '0038'.         
                    88 FRWKGERR-TAM-DAD10-INCOR   VALUE '0039'.         
                    88 FRWKGERR-TAM-DAD11-INCOR   VALUE '0040'.         
                    88 FRWKGERR-TAM-DAD12-INCOR   VALUE '0041'.         
                    88 FRWKGERR-IDEN-PARA-MONIT   VALUE '0042'.         
                    88 FRWKGERR-EVTO-PARA-MONIT   VALUE '0043'.         
                    88 FRWKGERR-TMSP-INVALIDO     VALUE '0044'.         
                    88 FRWKGERR-ERRO-CHAM-CDES    VALUE '0045'.         
CRM-01              88 FRWKGERR-MSGS-ERRO-REC     VALUE '0046'.         
*                                                                       
                 09 FRWKGERR-COD-MENSAGEM         PIC X(008).           
              07 FRWKGERR-BLOCO-HEADER.                                 
              COPY 'I#FRWKHE'.

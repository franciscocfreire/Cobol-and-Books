      ******************************************************************
      * NOME BOOK : LPCLW001                                           *
      *             INTERFACE ENTRE APLICATIVO DA ADMINISTRACAO WEB    *
      *             E MAINFRAME                                        *
      * DESCRICAO : AREA DE USO COMUM PARA TRATAMENTO DE ERROS         *
      * DATA      : 25/06/2013.                                        *
      * AUTOR     : HEBE LUIZA                                         *
      * EMPRESA   : BRQ                                                *
      * GRUPO     : TI MELHORIAS                                       *
      * COMPONENTE: SISTEMA LUCRO E PERDAS CREDITO LIQUIDACAO          *
      * TAMANHO   : 00390 BYTES                                        *
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      ******************************************************************
      * XX/XX/XXXX XXXXXXXXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *
      ******************************************************************
      * LPCLW001-COD-LAYOUT     = CODIGO DESTE LEIAUTE                 *
      * LPCLW001-TAM-LAYOUT     = TAMANHO DO REGISTRO                  *
      * LPCLW001-COD-RET        = CODIGO DE RETORNO PARA ORIGEM        *
      * LPCLW001-COD-ERRO       = CODIGO DE ERRO PARA ORIGEM           *
      * LPCLW001-COD-MSG        = CODIGO DE MENSAGEM PARA ORIGEM       *
      * LPCLW001-LISTA-PARAM    = LISTA DE PARAMENTROS                 *
      * LPCLW001-CUSUAR-MANUT   = CODIGO DO USUARIO                    *
      *                                                                *
      ******************************************************************
           05 LPCLW001-HEADER.                                          
              10 LPCLW001-COD-LAYOUT     PIC  X(08) VALUE 'LPCLW001'.   
              10 LPCLW001-TAM-LAYOUT     PIC  9(05) VALUE 00390.        
           05 LPCLW001-REGISTRO.                                        
              10 LPCLW001-BLOCO-RETORNO.                                
                 15 LPCLW001-COD-RETORNO           PIC  9(002).         
                 15 LPCLW001-COD-ERRO              PIC  X(004).         
                 15 LPCLW001-COD-MENSAGEM          PIC  X(008).         
              10 LPCLW001-LISTA-PARAM              PIC  X(100).         
              10 LPCLW001-BLOCO-ENTRADA.                                
                 15 LPCLW001-AREA-SESSAO           PIC  X(263).         
                 15 FILLER REDEFINES LPCLW001-AREA-SESSAO.              
                    20 FILLER                      PIC  X(013).         
                    20 LPCLW001-CSESS-FRWK         PIC  X(032).         
                    20 LPCLW001-NOPER-FLUXO        PIC  X(040).         
                    20 LPCLW001-CULT-FLUXO-EXTER   PIC  X(008).         
                    20 LPCLW001-EMPRESA-OPER       PIC  9(005).         
                    20 LPCLW001-DEPENDENCIA-OPER   PIC  9(005).         
                    20 LPCLW001-CCANAL             PIC  9(003).         
                    20 LPCLW001-WINFO-DADOS-IDIOM  PIC  9(002).         
                    20 LPCLW001-DT-LOCAL           PIC  X(008).         
                    20 LPCLW001-HR-LOCAL           PIC  X(006).         
                    20 LPCLW001-FLAG-MONETARIO     PIC  X(001).         
                    20 LPCLW001-SOLIC-MAIS-DADOS   PIC  X(001).         
                    20 LPCLW001-TIPO-USUAR         PIC  X(001).         
                    20 LPCLW001-CAUTEN-SEGRC.                           
                       25 LPCLW001-CUSUAR-MANUT    PIC  X(009).         
                       25 FILLER                   PIC  X(021).         
                    20 LPCLW001-TIPO-USUAR-AUTORIZ PIC  X(001).         
                    20 LPCLW001-USUARIO-AUTORIZ    PIC  X(030).         
                    20 LPCLW001-PERFIL-USU-AUTORIZ PIC  X(008).         
                    20 LPCLW001-WINFO-DADOS-EMPRE  PIC  9(005).         
                    20 LPCLW001-CODIGO-DEPENDENCIA PIC  9(005).         
                    20 LPCLW001-WINFO-DADOS-DTSIST PIC  9(008).         
                    20 LPCLW001-WINFO-DADOS-DTOPER PIC  9(008).         
                    20 LPCLW001-TAM-DISP-BLK-SAIDA PIC S9(005) COMP-3.  
                    20 LPCLW001-FLAG-ASSINATURA    PIC  X(001).         
                    20 LPCLW001-FLAG-ALCADA        PIC  X(001).         
                    20 LPCLW001-EMPR-USUAR-TRAB    PIC  9(010).         
                    20 LPCLW001-DEPEND-USUAR-TRAB  PIC  9(008).         
                    20 LPCLW001-EMPR-OPERANTE      PIC  9(010).         
                    20 LPCLW001-DEPEND-OPERANTE    PIC  9(008).         
                    20 LPCLW001-NIVEL-SEGUR        PIC  9(002).         

      ******************************************************************
      * DESCRICAO : ARQUIVO DE INFORMACOES AVISO DE COBRANCA COM DADOS *
      *             DE TIPOS DE MODELO DE AVISO. (FORMULARIOS DICD)    *
      * BOOK      : RECRWIJA                                           *
      * DATA      : 27/07/2011                                         *
      * AUTOR     : CESAR S PEREGO                                     *
      * EMPRESA   : CPM BRAXIS                                         *
      * GRUPO     : TI MELHORIAS                                       *
      * COMPONENTE: RECR                                               *
      * TAMANHO   : 1500 BYTES                                         *
      ******************************************************************
      *                                                                 
           05 RECRWIJA-REGISTRO.                                        
              10 RECRWIJA-INF-ACAO-COBR.                                
                 15 RECRWIJA-NACAO-COBR-CONTR       PIC  9(10)   COMP-3.
                 15 RECRWIJA-NREGRA-ENQUA-CONTR     PIC  9(05)   COMP-3.
                 15 RECRWIJA-NREGRA-ACAO-COBR       PIC  9(04)   COMP-3.
                 15 RECRWIJA-CTPO-ACAO-COBR         PIC  9(02)   COMP-3.
                 15 RECRWIJA-CTPO-CANAL-COBR        PIC  9(02)   COMP-3.
                 15 RECRWIJA-TIPO-DISTR-AVISO       PIC  9(02)   COMP-3.
                 15 RECRWIJA-MODELO-AVISO           PIC  X(08).         
              10 RECRWIJA-INF-CONTRATO.                                 
                 15 RECRWIJA-CPSSOA-JURID-CONTR     PIC  9(10)   COMP-3.
                 15 RECRWIJA-CTPO-CONTR-NEGOC       PIC  9(03)   COMP-3.
                 15 RECRWIJA-NSEQ-CONTR-NEGOC       PIC  9(10)   COMP-3.
                 15 RECRWIJA-CPRODT-SERVC           PIC  9(08)   COMP-3.
                 15 RECRWIJA-DESC-PRODT             PIC  X(30).         
                 15 RECRWIJA-DBASE-INADP-CONTR      PIC  X(10).         
                 15 RECRWIJA-QDIA-ATRSO             PIC  9(05)   COMP-3.
                 15 RECRWIJA-VVENCD-CONTR           PIC  9(15)V99       
                                                                 COMP-3.
                 15 RECRWIJA-COD-SEGM-CLIENTE       PIC  9(05)   COMP-3.
                 15 RECRWIJA-CIDTFD-CONTR-CRRTT     PIC  9(01)   COMP-3.
                 15 RECRWIJA-CBCO-CONTR-ORIGE       PIC  9(03)   COMP-3.
                 15 RECRWIJA-CAG-CONTR-ORIGE        PIC  9(05)   COMP-3.
                 15 RECRWIJA-CCTA-CONTR-ORIGE       PIC  9(13)   COMP-3.
                 15 RECRWIJA-CCART-CONTR-ORIGE      PIC  X(03).         
                 15 RECRWIJA-CCONTR-NEGOC-ORIGE     PIC  9(07)   COMP-3.
                 15 RECRWIJA-CID-TPO-PSSOA          PIC  X(01).         
                 15 RECRWIJA-CTPO-PRTCP-PSSOA       PIC  9(03)   COMP-3.
              10 RECRWIJA-INF-PESSOAL.                                  
                 15 RECRWIJA-CPF-CNPJ.                                  
                    20 RECRWIJA-CPF-CNPJ-NRO        PIC  9(09)   COMP-3.
                    20 RECRWIJA-CPF-CNPJ-FIL        PIC  9(04)   COMP-3.
                    20 RECRWIJA-CPF-CNPJ-CTR        PIC  9(02)   COMP-3.
                 15 RECRWIJA-CCLUB                  PIC  9(10)   COMP-3.
                 15 RECRWIJA-IPSSOA-COPLT           PIC  X(70).         
              10 RECRWIJA-INF-ENDERECO.                                 
                 15 RECRWIJA-CTPO-ENDER             PIC  9(01)   COMP-3.
                 15 RECRWIJA-ELOGDR-PSSOA           PIC  X(70).         
                 15 RECRWIJA-ELOGDR-NRO             PIC  X(07).         
                 15 RECRWIJA-RCOMPL-ENDER           PIC  X(30).         
                 15 RECRWIJA-EBAIRO-ENDER           PIC  X(40).         
                 15 RECRWIJA-ICIDDE-ENDER           PIC  X(40).         
                 15 RECRWIJA-CSGL-UF                PIC  X(02).         
                 15 RECRWIJA-CCEP                   PIC  9(05)   COMP-3.
                 15 RECRWIJA-CCEP-COMPL             PIC  9(03)   COMP-3.
              10 RECRWIJA-INF-UND-ORGNZ.                                
                 15 RECRWIJA-CPSSOA-JURID-ORGNZ     PIC  9(10)   COMP-3.
                 15 RECRWIJA-NSEQ-UND-ORGNZ         PIC  9(08)   COMP-3.
                 15 RECRWIJA-CTPO-UND-ORGNZ         PIC  9(03)   COMP-3.
                 15 RECRWIJA-CDIG-UND-ORGNZ         PIC  X(01).         
                 15 RECRWIJA-NM-AGENCIA-UORG        PIC  X(40).         
                 15 RECRWIJA-ELOGDR-PSSO-UORG       PIC  X(70).         
                 15 RECRWIJA-ELOGDR-NRO-UORG        PIC  X(07).         
                 15 RECRWIJA-RCOMPL-ENDER-UORG      PIC  X(30).         
                 15 RECRWIJA-EBAIRO-ENDER-UORG      PIC  X(40).         
                 15 RECRWIJA-ICIDDE-ENDER-UORG      PIC  X(40).         
                 15 RECRWIJA-CSGL-UF-UORG           PIC  X(02).         
                 15 RECRWIJA-CCEP-UORG              PIC  9(05)   COMP-3.
                 15 RECRWIJA-CCEP-COMPL-UORG        PIC  9(03)   COMP-3.
              10 RECRWIJA-INF-TITULAR.                                  
                 15 RECRWIJA-CCLUB-TITULAR          PIC  9(10).         
                 15 RECRWIJA-IPSSOA-COPLT-TITULAR   PIC  X(40).         
              10 RECRWIJA-QT-CONTRATOS              PIC  9(03).         
              10 RECRWIJA-INF-NATUREZA.                                 
                 15 RECRWIJA-CID-NATUREZA           PIC  X(02).         
                 15 RECRWIJA-DESC-NATUREZA          PIC  X(20).         
              10 RECRWIJA-INF-MOTIVO-REJ.                               
                 20 RECRWIJA-CID-MTVO-REJ           PIC  X(03).         
                 20 RECRWIJA-DESC-MTVO-REJ          PIC  X(50).         
              10 RECRWIJA-INF-ARRASTO.                                  
                 20 RECRWIJA-ARRASTADO              PIC  9(01).         
              10 RECRWIJA-INF-SEGURO.                                   
                 20 RECRWIJA-SEG-PRESTAMISTA        PIC  9(01).         
              10 FILLER                             PIC  X(15).         
           05 RECRWIJA-AREA-DADOS-DICD              PIC  X(703).        

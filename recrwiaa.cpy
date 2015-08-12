      ******************************************************************
      * DESCRICAO : ARQUIVO DE INFORMACOES AVISO DE COBRANCA           *
      * BOOK      : RECRWIAA                                           *
      * DATA      : 20/07/2011                                         *
      * AUTOR     : CESAR S PEREGO                                     *
      * EMPRESA   : CPM BRAXIS                                         *
      * GRUPO     : TI MELHORIAS                                       *
      * COMPONENTE: RECR                                               *
      * TAMANHO   : 800 BYTES                                          *
      ******************************************************************
      *                                                                 
           05 RECRWIAA-REGISTRO.                                        
              10 RECRWIAA-INF-ACAO-COBR.                                
                 15 RECRWIAA-NACAO-COBR-CONTR       PIC  9(10)   COMP-3.
                 15 RECRWIAA-NREGRA-ENQUA-CONTR     PIC  9(05)   COMP-3.
                 15 RECRWIAA-NREGRA-ACAO-COBR       PIC  9(04)   COMP-3.
                 15 RECRWIAA-CTPO-ACAO-COBR         PIC  9(02)   COMP-3.
                 15 RECRWIAA-CTPO-CANAL-COBR        PIC  9(02)   COMP-3.
                 15 RECRWIAA-TIPO-DISTR-AVISO       PIC  9(02)   COMP-3.
                 15 RECRWIAA-MODELO-AVISO           PIC  X(08).         
              10 RECRWIAA-INF-CONTRATO.                                 
                 15 RECRWIAA-CPSSOA-JURID-CONTR     PIC  9(10)   COMP-3.
                 15 RECRWIAA-CTPO-CONTR-NEGOC       PIC  9(03)   COMP-3.
                 15 RECRWIAA-NSEQ-CONTR-NEGOC       PIC  9(10)   COMP-3.
                 15 RECRWIAA-CPRODT-SERVC           PIC  9(08)   COMP-3.
                 15 RECRWIAA-DESC-PRODT             PIC  X(30).         
                 15 RECRWIAA-DBASE-INADP-CONTR      PIC  X(10).         
                 15 RECRWIAA-QDIA-ATRSO             PIC  9(05)   COMP-3.
                 15 RECRWIAA-VVENCD-CONTR           PIC  9(15)V99       
                                                                 COMP-3.
                 15 RECRWIAA-COD-SEGM-CLIENTE       PIC  9(05)   COMP-3.
                 15 RECRWIAA-CIDTFD-CONTR-CRRTT     PIC  9(01)   COMP-3.
                 15 RECRWIAA-CBCO-CONTR-ORIGE       PIC  9(03)   COMP-3.
                 15 RECRWIAA-CAG-CONTR-ORIGE        PIC  9(05)   COMP-3.
                 15 RECRWIAA-CCTA-CONTR-ORIGE       PIC  9(13)   COMP-3.
                 15 RECRWIAA-CCART-CONTR-ORIGE      PIC  X(03).         
                 15 RECRWIAA-CCONTR-NEGOC-ORIGE     PIC  9(07)   COMP-3.
                 15 RECRWIAA-CID-TPO-PSSOA          PIC  X(01).         
                 15 RECRWIAA-CTPO-PRTCP-PSSOA       PIC  9(03)   COMP-3.
              10 RECRWIAA-INF-PESSOAL.                                  
                 15 RECRWIAA-CPF-CNPJ.                                  
                    20 RECRWIAA-CPF-CNPJ-NRO        PIC  9(09)   COMP-3.
                    20 RECRWIAA-CPF-CNPJ-FIL        PIC  9(04)   COMP-3.
                    20 RECRWIAA-CPF-CNPJ-CTR        PIC  9(02)   COMP-3.
                 15 RECRWIAA-CCLUB                  PIC  9(10)   COMP-3.
                 15 RECRWIAA-IPSSOA-COPLT           PIC  X(70).         
              10 RECRWIAA-INF-ENDERECO.                                 
                 15 RECRWIAA-CTPO-ENDER             PIC  9(01)   COMP-3.
                 15 RECRWIAA-ELOGDR-PSSOA           PIC  X(70).         
                 15 RECRWIAA-ELOGDR-NRO             PIC  X(07).         
                 15 RECRWIAA-RCOMPL-ENDER           PIC  X(30).         
                 15 RECRWIAA-EBAIRO-ENDER           PIC  X(40).         
                 15 RECRWIAA-ICIDDE-ENDER           PIC  X(40).         
                 15 RECRWIAA-CSGL-UF                PIC  X(02).         
                 15 RECRWIAA-CCEP                   PIC  9(05)   COMP-3.
                 15 RECRWIAA-CCEP-COMPL             PIC  9(03)   COMP-3.
              10 RECRWIAA-INF-UND-ORGNZ.                                
                 15 RECRWIAA-CPSSOA-JURID-ORGNZ     PIC  9(10)   COMP-3.
                 15 RECRWIAA-NSEQ-UND-ORGNZ         PIC  9(08)   COMP-3.
                 15 RECRWIAA-CTPO-UND-ORGNZ         PIC  9(03)   COMP-3.
                 15 RECRWIAA-CDIG-UND-ORGNZ         PIC  X(01).         
                 15 RECRWIAA-NM-AGENCIA-UORG        PIC  X(40).         
                 15 RECRWIAA-ELOGDR-PSSO-UORG       PIC  X(70).         
                 15 RECRWIAA-ELOGDR-NRO-UORG        PIC  X(07).         
                 15 RECRWIAA-RCOMPL-ENDER-UORG      PIC  X(30).         
                 15 RECRWIAA-EBAIRO-ENDER-UORG      PIC  X(40).         
                 15 RECRWIAA-ICIDDE-ENDER-UORG      PIC  X(40).         
                 15 RECRWIAA-CSGL-UF-UORG           PIC  X(02).         
                 15 RECRWIAA-CCEP-UORG              PIC  9(05)   COMP-3.
                 15 RECRWIAA-CCEP-COMPL-UORG        PIC  9(03)   COMP-3.
              10 RECRWIAA-INF-TITULAR.                                  
                 15 RECRWIAA-CCLUB-TITULAR          PIC  9(10).         
                 15 RECRWIAA-IPSSOA-COPLT-TITULAR   PIC  X(40).         
              10 RECRWIAA-QT-CONTRATOS              PIC  9(03).         
              10 RECRWIAA-INF-NATUREZA.                                 
                 15 RECRWIAA-CID-NATUREZA           PIC  X(02).         
                 15 RECRWIAA-DESC-NATUREZA          PIC  X(20).         
              10 RECRWIAA-INF-MOTIVO-REJ.                               
                 20 RECRWIAA-CID-MTVO-REJ           PIC  X(03).         
                 20 RECRWIAA-DESC-MTVO-REJ          PIC  X(50).         
              10 RECRWIAA-INF-ARRASTO.                                  
                 20 RECRWIAA-ARRASTADO              PIC  9(01).         
              10 RECRWIAA-INF-SEGURO.                                   
                 20 RECRWIAA-SEG-PRESTAMISTA        PIC  9(01).         
              10 FILLER                             PIC  X(18).         

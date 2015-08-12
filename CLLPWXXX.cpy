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
           05 CLLPWXXX-REGISTRO.
              10 CLLPWXXX-INF-ACAO-COBR.
                 15 CLLPWXXX-NACAO-COBR-CONTR       PIC  9(10)   COMP-3.
                 15 CLLPWXXX-NREGRA-ENQUA-CONTR     PIC  9(05)   COMP-3.
                 15 CLLPWXXX-NREGRA-ACAO-COBR       PIC  9(04)   COMP-3.
                 15 CLLPWXXX-CTPO-ACAO-COBR         PIC  9(02)   COMP-3.
                 15 CLLPWXXX-CTPO-CANAL-COBR        PIC  9(02)   COMP-3.
                 15 CLLPWXXX-TIPO-DISTR-AVISO       PIC  9(02)   COMP-3.
                 15 CLLPWXXX-MODELO-AVISO           PIC  X(08).
              10 CLLPWXXX-INF-CONTRATO.
                 15 CLLPWXXX-CPSSOA-JURID-CONTR     PIC  9(10)   COMP-3.
                 15 CLLPWXXX-CTPO-CONTR-NEGOC       PIC  9(03)   COMP-3.
                 15 CLLPWXXX-NSEQ-CONTR-NEGOC       PIC  9(10)   COMP-3.
                 15 CLLPWXXX-CPRODT-SERVC           PIC  9(08)   COMP-3.
                 15 CLLPWXXX-DESC-PRODT             PIC  X(30).
                 15 CLLPWXXX-DBASE-INADP-CONTR      PIC  X(10).
                 15 CLLPWXXX-QDIA-ATRSO             PIC  9(05)   COMP-3.
                 15 CLLPWXXX-VVENCD-CONTR           PIC  9(15)V99
                                                                 COMP-3.
                 15 CLLPWXXX-COD-SEGM-CLIENTE       PIC  9(05)   COMP-3.
                 15 CLLPWXXX-CIDTFD-CONTR-CRRTT     PIC  9(01)   COMP-3.
                 15 CLLPWXXX-CBCO-CONTR-ORIGE       PIC  9(03)   COMP-3.
                 15 CLLPWXXX-CAG-CONTR-ORIGE        PIC  9(05)   COMP-3.
                 15 CLLPWXXX-CCTA-CONTR-ORIGE       PIC  9(13)   COMP-3.
                 15 CLLPWXXX-CCART-CONTR-ORIGE      PIC  X(03).
                 15 CLLPWXXX-CCONTR-NEGOC-ORIGE     PIC  9(07)   COMP-3.
                 15 CLLPWXXX-CID-TPO-PSSOA          PIC  X(01).
                 15 CLLPWXXX-CTPO-PRTCP-PSSOA       PIC  9(03)   COMP-3.
              10 CLLPWXXX-INF-PESSOAL.
                 15 CLLPWXXX-CPF-CNPJ.
                    20 CLLPWXXX-CPF-CNPJ-NRO        PIC  9(09)   COMP-3.
                    20 CLLPWXXX-CPF-CNPJ-FIL        PIC  9(04)   COMP-3.
                    20 CLLPWXXX-CPF-CNPJ-CTR        PIC  9(02)   COMP-3.
                 15 CLLPWXXX-CCLUB                  PIC  9(10)   COMP-3.
                 15 CLLPWXXX-IPSSOA-COPLT           PIC  X(70).
              10 CLLPWXXX-INF-ENDERECO.
                 15 CLLPWXXX-CTPO-ENDER             PIC  9(01)   COMP-3.
                 15 CLLPWXXX-ELOGDR-PSSOA           PIC  X(70).
                 15 CLLPWXXX-ELOGDR-NRO             PIC  X(07).
                 15 CLLPWXXX-RCOMPL-ENDER           PIC  X(30).
                 15 CLLPWXXX-EBAIRO-ENDER           PIC  X(40).
                 15 CLLPWXXX-ICIDDE-ENDER           PIC  X(40).
                 15 CLLPWXXX-CSGL-UF                PIC  X(02).
                 15 CLLPWXXX-CCEP                   PIC  9(05)   COMP-3.
                 15 CLLPWXXX-CCEP-COMPL             PIC  9(03)   COMP-3.
              10 CLLPWXXX-INF-UND-ORGNZ.
                 15 CLLPWXXX-CPSSOA-JURID-ORGNZ     PIC  9(10)   COMP-3.
                 15 CLLPWXXX-NSEQ-UND-ORGNZ         PIC  9(08)   COMP-3.
                 15 CLLPWXXX-CTPO-UND-ORGNZ         PIC  9(03)   COMP-3.
                 15 CLLPWXXX-CDIG-UND-ORGNZ         PIC  X(01).
                 15 CLLPWXXX-NM-AGENCIA-UORG        PIC  X(40).
                 15 CLLPWXXX-ELOGDR-PSSO-UORG       PIC  X(70).
                 15 CLLPWXXX-ELOGDR-NRO-UORG        PIC  X(07).
                 15 CLLPWXXX-RCOMPL-ENDER-UORG      PIC  X(30).
                 15 CLLPWXXX-EBAIRO-ENDER-UORG      PIC  X(40).
                 15 CLLPWXXX-ICIDDE-ENDER-UORG      PIC  X(40).
                 15 CLLPWXXX-CSGL-UF-UORG           PIC  X(02).
                 15 CLLPWXXX-CCEP-UORG              PIC  9(05)   COMP-3.
                 15 CLLPWXXX-CCEP-COMPL-UORG        PIC  9(03)   COMP-3.
              10 CLLPWXXX-INF-TITULAR.
                 15 CLLPWXXX-CCLUB-TITULAR          PIC  9(10).
                 15 CLLPWXXX-IPSSOA-COPLT-TITULAR   PIC  X(40).
              10 CLLPWXXX-QT-CONTRATOS              PIC  9(03).
              10 CLLPWXXX-INF-NATUREZA.
                 15 CLLPWXXX-CID-NATUREZA           PIC  X(02).
                 15 CLLPWXXX-DESC-NATUREZA          PIC  X(20).
              10 CLLPWXXX-INF-MOTIVO-REJ.
                 20 CLLPWXXX-CID-MTVO-REJ           PIC  X(03).
                 20 CLLPWXXX-DESC-MTVO-REJ          PIC  X(50).
              10 CLLPWXXX-INF-ARRASTO.
                 20 CLLPWXXX-ARRASTADO              PIC  9(01).
              10 CLLPWXXX-INF-SEGURO.
                 20 CLLPWXXX-SEG-PRESTAMISTA        PIC  9(01).
              10 FILLER                             PIC  X(15).
           05 CLLPWXXX-AREA-DADOS-DICD              PIC  X(1290).

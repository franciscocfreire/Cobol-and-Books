      *---------------------------------------------------------------*
      *                                                               *
      *   ARQUIVO FINAL PARA RENEGOCIACAO                             *
      *          - DADOS DA TABELA LPCLB000                           *
      *          - IDENTIFICACAO DIVERSAS ROTINAS                     *
      *          - DADOS CADASTRAIS (CLIE/FICA)                       *
      *                                                               *
      *                                                               *
      *   LRECL = 500 - FB                                            *
      *                                                               *
      *---------------------------------------------------------------*

       01 REG-RDABAS.
           03  REG-RPF.
               05  RPF-AGENCIA              PIC 9(5)         COMP-3.
               05  RPF-CONTA                PIC 9(7)         COMP-3.
               05  RPF-CC-DIG               PIC X(1).
               05  RPF-CARTEIRA             PIC X(3).
               05  RPF-CONTRATO             PIC 9(7)         COMP-3.
               05  RPF-VCTO                 PIC 9(9)         COMP-3.
               05  RPF-DATA-OPERACAO        PIC 9(9)         COMP-3.
               05  RPF-ADVCPF-NUM           PIC 9(9)         COMP-3.
               05  RPF-ADVCPF-CTR           PIC X(2).
               05  RPF-ULTOCOR-COD          PIC 9(3)         COMP-3.
               05  RPF-VR-LIQUIDO           PIC S9(13)V9(2)  COMP-3.
               05  RPF-VR-COBRANCA          PIC S9(13)V9(2)  COMP-3.
               05  RPF-VR-A-VENCER          PIC S9(13)V9(2)  COMP-3.
               05  RPF-CARTEIRA-ORIG        PIC X(3).
               05  RPF-COD-NAT-OPER         PIC X(3).
               05  RPF-NOME                 PIC X(40).
               05  RPF-CPFCNPJ-NUM          PIC 9(9)         COMP-3.
               05  RPF-CPFCNPJ-FIL          PIC 9(5)         COMP-3.
               05  RPF-CPFCNPJ-CTR          PIC X(2).
               05  RPF-DT-INIC-OPER         PIC 9(9)         COMP-3.
               05  RPF-CPRODT-BDSCO         PIC 9(3)         COMP-3.
               05  RPF-CFAML-CONTR          PIC 9(1)         COMP-3.
               05  RPF-GARANTIA             PIC X(2).
               05  RPF-ID-CPF-ZERADO        PIC X(1).
               05  RPF-ID-EXTRATO-BLOQ      PIC X(1).
               05  RPF-ID-ESCRITORIO        PIC X(1).
               05  RPF-ID-RESTR-SERASA-SPC  PIC X(1).
               05  RPF-ID-INAD-PRIM-PARC    PIC X(2).
               05  RPF-ID-INAD-UNICO-PROD   PIC X(2).
               05  RPF-ID-RESTR-IRES        PIC X(2).
               05  RPF-ID-NAT-DIF-PARM      PIC X(2).
               05  RPF-PRODUTOS-RENEGOCIACAO OCCURS 10 TIMES.
                   07  RPF-NAT-OP           PIC X(3).
               05  RPF-EMPRESA              PIC 9(5)         COMP-3.
               05  RPF-NATUREZA-SPC         PIC X(2).
               05  RPF-VCTO-CPF             PIC 9(9)         COMP-3.
               05  RPF-ID                   PIC X(2).
               05  RPF-RESERVA              PIC X(2).
               05  RPF-DADOS-CLIENTE.
                   07  RPF-DADOS-SISTEMA-CLIE.
                       09  RPF-ENDERECO-CLIE   PIC X(40).
                       09  RPF-CEP-CLIE        PIC 9(05)  COMP-3.
                       09  RPF-CEP-COMPL-CLIE  PIC 9(03)  COMP-3.
                       09  RPF-UF-CLIE         PIC X(02).
                       09  RPF-CIDADE-CLIE     PIC X(30).
                       09  RPF-DDD-CLIE        PIC X(04).
                       09  RPF-TELEFONE-CLIE   PIC 9(09)  COMP-3.
                   07  RPF-DADOS-SISTEMA-FICA.
                       09  RPF-ENDERECO-FICA   PIC X(40).
                       09  RPF-NUMERO-FICA     PIC 9(05)  COMP-3.
                       09  RPF-COMPL-FICA      PIC X(10).
                       09  RPF-CEP-FICA        PIC 9(05)  COMP-3.
                       09  RPF-CEP-COMPL-FICA  PIC 9(03)  COMP-3.
                       09  RPF-UF-FICA         PIC X(02).
                       09  RPF-CIDADE-FICA     PIC X(30).
                       09  RPF-DDD-FICA        PIC X(04).
                       09  RPF-TELEFONE-FICA   PIC 9(09)  COMP-3.
                       09  RPF-NOME-PAI-FICA   PIC X(40).
                       09  RPF-NOME-MAE-FICA   PIC X(40).
                       09  RPF-RENDA-FICA      PIC 9(13)V9(02)  COMP-3.
                       09  RPF-DT-NASC-FICA    PIC 9(09)  COMP-3.
                       09  RPF-RG-FICA         PIC 9(09)  COMP-3.
                       09  RPF-SEXO-FICA       PIC X(01).
                       09  RPF-RAMO-ATIV-FICA  PIC 9(05)  COMP-3.
               05  RPF-RESERVA-2               PIC X(13).
           03  RPF-CLUB                        PIC 9(10).
           03  RPF-QREG-CNTRL-FONE             PIC 9(03).
           03  RPF-QREG-ENDER                  PIC 9(03).
           03  FILLER                          PIC X(04).


      *---------------------------------------------------------------* 
      *                                                               * 
      *   ARQUIVO FINAL PARA RENEGOCIACAO                             * 
      *          - DADOS DA TABELA LPCLB000                           * 
      *          - IDENTIFICACAO DIVERSAS ROTINAS                     * 
      *          - DADOS CADASTRAIS (CLIE/FICA)                       * 
      *                                                               * 
      *   CAMPANHA 2002 - 13O. SALARIO                                * 
      *                                                               * 
      *   LRECL = 480 - FB                                            * 
      *                                                               * 
      *---------------------------------------------------------------* 
                                                                        
       01  REG-PEND-FINAL.                                              
           02  RF-AGENCIA              PIC 9(5)         COMP-3.         
           02  RF-CONTA                PIC 9(7)         COMP-3.         
           02  RF-CC-DIG               PIC X(1).                        
           02  RF-CARTEIRA             PIC X(3).                        
           02  RF-CONTRATO             PIC 9(7)         COMP-3.         
           02  RF-VCTO                 PIC 9(9)         COMP-3.         
           02  RF-DATA-OPERACAO        PIC 9(9)         COMP-3.         
           02  RF-ADVCPF-NUM           PIC 9(9)         COMP-3.         
           02  RF-ADVCPF-CTR           PIC X(2).                        
           02  RF-ULTOCOR-COD          PIC 9(3)         COMP-3.         
           02  RF-VR-LIQUIDO           PIC S9(13)V9(2)  COMP-3.         
           02  RF-VR-COBRANCA          PIC S9(13)V9(2)  COMP-3.         
           02  RF-VR-A-VENCER          PIC S9(13)V9(2)  COMP-3.         
           02  RF-CARTEIRA-ORIG        PIC X(3).                        
           02  RF-COD-NAT-OPER         PIC X(3).                        
           02  RF-NOME                 PIC X(40).                       
           02  RF-CPFCNPJ-NUM          PIC 9(9)         COMP-3.         
           02  RF-CPFCNPJ-FIL          PIC 9(5)         COMP-3.         
           02  RF-CPFCNPJ-CTR          PIC X(2).                        
           02  RF-DT-INIC-OPER         PIC 9(9)         COMP-3.         
           02  RF-CPRODT-BDSCO         PIC 9(3)         COMP-3.         
           02  RF-CFAML-CONTR          PIC 9(1)         COMP-3.         
           02  RF-GARANTIA             PIC X(2).                        
           02  RF-ID-CPF-ZERADO        PIC X(1).                        
           02  RF-ID-EXTRATO-BLOQ      PIC X(1).                        
           02  RF-ID-ESCRITORIO        PIC X(1).                        
           02  RF-ID-RESTR-SERASA-SPC  PIC X(1).                        
           02  RF-ID-INAD-PRIM-PARC    PIC X(2).                        
           02  RF-ID-INAD-UNICO-PROD   PIC X(2).                        
           02  RF-ID-RESTR-IRES        PIC X(2).                        
           02  RF-ID-NAT-DIF-PARM      PIC X(2).                        
           02  RF-PRODUTOS-RENEGOCIACAO OCCURS 10 TIMES.                
               03  RF-NAT-OP           PIC X(3).                        
           02  RF-EMPRESA              PIC 9(5)         COMP-3.         
           02  RF-NATUREZA-SPC         PIC X(2).                        
           02  RF-VCTO-CPF             PIC 9(9)         COMP-3.         
           02  RF-ID                   PIC X(2).                        
           02  RF-RESERVA              PIC X(2).                        
           02  RF-DADOS-CLIENTE.                                        
               03  RF-DADOS-SISTEMA-CLIE.                               
                   04  RF-ENDERECO-CLIE   PIC X(40).                    
                   04  RF-CEP-CLIE        PIC 9(05)  COMP-3.            
                   04  RF-CEP-COMPL-CLIE  PIC 9(03)  COMP-3.            
                   04  RF-UF-CLIE         PIC X(02).                    
                   04  RF-CIDADE-CLIE     PIC X(30).                    
                   04  RF-DDD-CLIE        PIC X(04).                    
                   04  RF-TELEFONE-CLIE   PIC 9(09)  COMP-3.            
               03  RF-DADOS-SISTEMA-FICA.                               
                   04  RF-ENDERECO-FICA   PIC X(40).                    
                   04  RF-NUMERO-FICA     PIC 9(05)  COMP-3.            
                   04  RF-COMPL-FICA      PIC X(10).                    
                   04  RF-CEP-FICA        PIC 9(05)  COMP-3.            
                   04  RF-CEP-COMPL-FICA  PIC 9(03)  COMP-3.            
                   04  RF-UF-FICA         PIC X(02).                    
                   04  RF-CIDADE-FICA     PIC X(30).                    
                   04  RF-DDD-FICA        PIC X(04).                    
                   04  RF-TELEFONE-FICA   PIC 9(09)  COMP-3.            
                   04  RF-NOME-PAI-FICA   PIC X(40).                    
                   04  RF-NOME-MAE-FICA   PIC X(40).                    
                   04  RF-RENDA-FICA      PIC 9(13)V9(02)  COMP-3.      
                   04  RF-DT-NASC-FICA    PIC 9(09)  COMP-3.            
                   04  RF-RG-FICA         PIC 9(09)  COMP-3.            
                   04  RF-SEXO-FICA       PIC X(01).                    
                   04  RF-RAMO-ATIV-FICA  PIC 9(05)  COMP-3.            
           02  RF-RESERVA-2               PIC X(13).                    
                                                                        

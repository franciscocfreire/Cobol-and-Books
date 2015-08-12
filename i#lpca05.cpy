      *---------------------------------------------------------------* 
      *   I#LPCA05                               ANTIGA I#CLLPRC      * 
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
                                                                        
       01  REG-LPCA05.                                                  
           02  LPCA05-AGENCIA              PIC 9(5)         COMP-3.     
           02  LPCA05-CONTA                PIC 9(7)         COMP-3.     
           02  LPCA05-CC-DIG               PIC X(1).                    
           02  LPCA05-CARTEIRA             PIC X(3).                    
           02  LPCA05-CONTRATO             PIC 9(7)         COMP-3.     
           02  LPCA05-VCTO                 PIC 9(9)         COMP-3.     
           02  LPCA05-DATA-OPERACAO        PIC 9(9)         COMP-3.     
           02  LPCA05-ADVCPF-NUM           PIC 9(9)         COMP-3.     
           02  LPCA05-ADVCPF-CTR           PIC X(2).                    
           02  LPCA05-ULTOCOR-COD          PIC 9(3)         COMP-3.     
           02  LPCA05-VR-LIQUIDO           PIC S9(13)V9(2)  COMP-3.     
           02  LPCA05-VR-COBRANCA          PIC S9(13)V9(2)  COMP-3.     
           02  LPCA05-VR-A-VENCER          PIC S9(13)V9(2)  COMP-3.     
           02  LPCA05-CARTEIRA-ORIG        PIC X(3).                    
           02  LPCA05-COD-NAT-OPER         PIC X(3).                    
           02  LPCA05-NOME                 PIC X(40).                   
           02  LPCA05-CPFCNPJ-NUM          PIC 9(9)         COMP-3.     
           02  LPCA05-CPFCNPJ-FIL          PIC 9(5)         COMP-3.     
           02  LPCA05-CPFCNPJ-CTR          PIC X(2).                    
           02  LPCA05-DT-INIC-OPER         PIC 9(9)         COMP-3.     
           02  LPCA05-CPRODT-BDSCO         PIC 9(3)         COMP-3.     
           02  LPCA05-CFAML-CONTR          PIC 9(1)         COMP-3.     
           02  LPCA05-GARANTIA             PIC X(2).                    
           02  LPCA05-ID-CPF-ZERADO        PIC X(1).                    
           02  FILLER                      PIC X(1).                    
           02  LPCA05-ID-ESCRITORIO        PIC X(1).                    
           02  FILLER                      PIC X(1).                    
           02  LPCA05-ID-INAD-PRIM-PARC    PIC X(2).                    
AMS281     02  LPCA05-BENEFICIO-FISCAL     PIC X(1).                    
A          02  LPCA05-AJUIZADO             PIC X(1).                    
A          02  LPCA05-VALOR-PRINCIPAL      PIC 9(13)V99.                
AMS281     02  FILLER                      PIC X(19).                   
           02  LPCA05-EMPRESA              PIC 9(5)         COMP-3.     
           02  LPCA05-NATUREZA-SPC         PIC X(2).                    
           02  LPCA05-VCTO-CPF             PIC 9(9)         COMP-3.     
           02  LPCA05-ID                   PIC X(2).                    
           02  LPCA05-RESERVA              PIC X(2).                    
           02  LPCA05-DADOS-CLIENTE.                                    
               03  LPCA05-DADOS-SISTEMA-CLIE.                           
                   04  LPCA05-ENDERECO-CLIE   PIC X(40).                
                   04  LPCA05-CEP-CLIE        PIC 9(05)  COMP-3.        
                   04  LPCA05-CEP-COMPL-CLIE  PIC 9(03)  COMP-3.        
                   04  LPCA05-UF-CLIE         PIC X(02).                
                   04  LPCA05-CIDADE-CLIE     PIC X(30).                
                   04  LPCA05-DDD-CLIE        PIC X(04).                
                   04  LPCA05-TELEFONE-CLIE   PIC 9(09)  COMP-3.        
               03  LPCA05-DADOS-SISTEMA-FICA.                           
                   04  LPCA05-ENDERECO-FICA   PIC X(40).                
                   04  LPCA05-NUMERO-FICA     PIC 9(05)  COMP-3.        
                   04  LPCA05-COMPL-FICA      PIC X(10).                
                   04  LPCA05-CEP-FICA        PIC 9(05)  COMP-3.        
                   04  LPCA05-CEP-COMPL-FICA  PIC 9(03)  COMP-3.        
                   04  LPCA05-UF-FICA         PIC X(02).                
                   04  LPCA05-CIDADE-FICA     PIC X(30).                
                   04  LPCA05-DDD-FICA        PIC X(04).                
                   04  LPCA05-TELEFONE-FICA   PIC 9(09)  COMP-3.        
                   04  LPCA05-NOME-PAI-FICA   PIC X(40).                
                   04  LPCA05-NOME-MAE-FICA   PIC X(40).                
                   04  LPCA05-RENDA-FICA      PIC 9(13)V9(02)  COMP-3.  
                   04  LPCA05-DT-NASC-FICA    PIC 9(09)  COMP-3.        
                   04  LPCA05-RG-FICA         PIC 9(09)  COMP-3.        
                   04  LPCA05-SEXO-FICA       PIC X(01).                
                   04  LPCA05-RAMO-ATIV-FICA  PIC 9(05)  COMP-3.        
           02  LPCA05-RESERVA-2               PIC X(13).                
                                                                        
       01  REG-LPCA05-H.                                                
           02  LPCA05-H-AGENCIA            PIC 9(5)         COMP-3.     
           02  LPCA05-H-CONTA              PIC 9(7)         COMP-3.     
           02  LPCA05-H-CC-DIG             PIC X(1).                    
           02  LPCA05-H-CARTEIRA           PIC X(3).                    
           02  LPCA05-H-CONTRATO           PIC 9(7)         COMP-3.     
           02  FILLER                      PIC X(89).                   
           02  LPCA05-H-CPFCNPJ-NUM        PIC 9(9)         COMP-3.     
           02  LPCA05-H-CPFCNPJ-FIL        PIC 9(5)         COMP-3.     
           02  LPCA05-H-CPFCNPJ-CTR        PIC X(2).                    
           02  LPCA05-H-NUM-CAMPANHA       PIC 9(5).                    
           02  LPCA05-H-NOM-CAMPANHA       PIC X(15).                   
           02  LPCA05-H-DAT-CAMPANHA       PIC X(10).                   
           02  FILLER                      PIC X(336).                  
                                                                        

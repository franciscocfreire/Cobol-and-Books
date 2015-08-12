      *---------------------------------------------------------------* 
      *      CADASTRO DE CLIENTES - MODULOS MULTIBANCO                * 
      *                                                               * 
      *      LAYOUT DE RETORNO MODULO CLIB1002 - INC I#CLIB21         * 
      *                                                               * 
      *      ACESSO AS TABELAS OBJEB000, OBJEB011, OBJBB011           * 
      *                                                               * 
      *---------------------------------------------------------------* 
                                                                        
       01  OBJBB011.                                                    
           05 BANCO                PIC S9(3)V USAGE COMP-3.             
           05 AGENCIA              PIC S9(5)V USAGE COMP-3.             
           05 CONTA-CORRENTE       PIC S9(13)V USAGE COMP-3.            
           05 RAZAO                PIC S9(5)V USAGE COMP-3.             
           05 DABERT-CTA           PIC X(10).                           
           05 CTPO-DOCTO-ID        PIC X(20).                           
           05 CDOCTO-ID            PIC X(15).                           
           05 IORG-EMISR-ID        PIC X(20).                           
           05 DEMIS                PIC X(10).                           
                                                                        

      *                                                                 
      *---------------------------------------------------------------* 
      *   INC      : I#CLLPZY                                         * 
      *   SISTEMA  : CLLP                                             * 
      *   DESCRICAO: AREA PADRAO PARA DISPLAY DAS ESTATISTICAS DE     * 
      *              PROCESSAMENTO DE PROGRAMAS                       * 
      *   TAMANHO  : 70 (CONSIDERAR SOMENTE OS NIVES 03)              * 
      *   DATA     : 15-06-2012                                       * 
      *   ANALISTA : ELISANGELA - BRQ IT SERVICES                     * 
      *---------------------------------------------------------------* 
      *                                                                 
       01  CLLPZY-REG.                                                  
           03 CLLPZY-DISP1.                                             
              05 FILLER                     PIC X(70) VALUE ALL '*'.    
      *                                                                 
           03 CLLPZY-DISP2.                                             
              05 FILLER                     PIC X(02) VALUE ALL '*'.    
              05 FILLER                     PIC X(19) VALUE ALL ' '.    
              05 FILLER                     PIC X(29) VALUE             
             'ESTATISTICAS DE PROCESSAMENTO'.                           
              05 FILLER                     PIC X(18) VALUE ALL ' '.    
              05 FILLER                     PIC X(02) VALUE ALL '*'.    
      *                                                                 
           03 CLLPZY-DISP3.                                             
              05 FILLER                     PIC X(02) VALUE ALL '*'.    
              05 FILLER                     PIC X(66) VALUE ALL '-'.    
              05 FILLER                     PIC X(02) VALUE ALL '*'.    
      *                                                                 
           03 CLLPZY-DISP4.                                             
              05 FILLER                     PIC X(03) VALUE '** '.      
              05 FILLER                     PIC X(08) VALUE 'PROGRAMA'. 
              05 FILLER                     PIC X(03) VALUE ' | '.      
              05 FILLER                     PIC X(09) VALUE 'JOBNAME  '.
              05 FILLER                     PIC X(03) VALUE ' | '.      
              05 FILLER                     PIC X(21) VALUE             
             'DATA DE PROCESSAMENTO'.                                   
              05 FILLER                     PIC X(03) VALUE ' | '.      
              05 FILLER                     PIC X(18) VALUE             
             'DATA DE MOVIMENTO '.                                      
              05 FILLER                     PIC X(02) VALUE '**'.       
      *                                                                 
           03 CLLPZY-DISP5.                                             
              05 FILLER                     PIC X(03) VALUE '** '.      
              05 FILLER                     PIC X(08) VALUE '--------'. 
              05 FILLER                     PIC X(03) VALUE ' | '.      
              05 FILLER                     PIC X(09) VALUE '-------- '.
              05 FILLER                     PIC X(03) VALUE ' | '.      
              05 FILLER                     PIC X(21) VALUE             
             '---------------------'.                                   
              05 FILLER                     PIC X(03) VALUE ' | '.      
              05 FILLER                     PIC X(18) VALUE             
             '----------------- '.                                      
              05 FILLER                     PIC X(02) VALUE '**'.       
      *                                                                 
           03 CLLPZY-DISP6.                                             
              05 FILLER                     PIC X(03) VALUE '** '.      
              05 CLLPZY-PROGNAME            PIC X(08).                  
              05 FILLER                     PIC X(03) VALUE ' | '.      
              05 CLLPZY-JOBNAME             PIC X(08).                  
              05 FILLER                     PIC X(05) VALUE '  |  '.    
              05 CLLPZY-DATAPROC            PIC X(10).                  
              05 FILLER                     PIC X(01) VALUE SPACES.     
              05 CLLPZY-HORAPROC            PIC X(08).                  
              05 FILLER                     PIC X(08) VALUE '  |     '. 
              05 CLLPZY-DATAMOV             PIC X(10).                  
              05 FILLER                     PIC X(06) VALUE '    **'.   
      *                                                                 
           03 CLLPZY-DISP7.                                             
              05 FILLER                     PIC X(04) VALUE '**  '.     
              05 FILLER                     PIC X(07) VALUE 'DDNAME '.  
              05 FILLER                     PIC X(03) VALUE ' | '.      
              05 FILLER                     PIC X(03) VALUE 'I/O'.      
              05 FILLER                     PIC X(03) VALUE ' | '.      
              05 FILLER                     PIC X(27) VALUE             
             'DESCRICAO DO ARQUIVO       '.                             
              05 FILLER                     PIC X(03) VALUE ' | '.      
              05 FILLER                     PIC X(18) VALUE             
             '    QUANTIDADE    '.                                      
              05 FILLER                     PIC X(02) VALUE '**'.       
      *                                                                 
           03 CLLPZY-DISP8.                                             
              05 FILLER                     PIC X(03) VALUE '** '.      
              05 FILLER                     PIC X(08) VALUE '--------'. 
              05 FILLER                     PIC X(03) VALUE ' | '.      
              05 FILLER                     PIC X(03) VALUE '---'.      
              05 FILLER                     PIC X(03) VALUE ' | '.      
              05 FILLER                     PIC X(27) VALUE             
             '---------------------------'.                             
              05 FILLER                     PIC X(03) VALUE ' | '.      
              05 FILLER                     PIC X(18) VALUE             
             '----------------- '.                                      
              05 FILLER                     PIC X(02) VALUE '**'.       
      *                                                                 
           03 CLLPZY-DISP9.                                             
              05 FILLER                     PIC X(03) VALUE '** '.      
              05 CLLPZY-DDNAME              PIC X(08).                  
              05 FILLER                     PIC X(04) VALUE ' |  '.     
              05 CLLPZY-I-O                 PIC X(01).                  
              05 FILLER                     PIC X(04) VALUE '  | '.     
              05 CLLPZY-DESCARQ             PIC X(27).                  
              05 FILLER                     PIC X(05) VALUE ' |   '.    
              05 CLLPZY-QTDEARQ             PIC ZZZ.ZZZ.ZZZ.ZZ9.        
              05 FILLER                     PIC X(03) VALUE ' **'.      
      *                                                                 
           03 CLLPZY-DISP10.                                            
              05 FILLER                     PIC X(04) VALUE '**  '.     
              05 FILLER                     PIC X(07) VALUE 'TABELA '.  
              05 FILLER                     PIC X(03) VALUE ' | '.      
              05 FILLER                     PIC X(03) VALUE 'FUN'.      
              05 FILLER                     PIC X(03) VALUE ' | '.      
              05 FILLER                     PIC X(27) VALUE             
             'DESCRICAO DA TABELA        '.                             
              05 FILLER                     PIC X(03) VALUE ' | '.      
              05 FILLER                     PIC X(18) VALUE             
             '    QUANTIDADE    '.                                      
              05 FILLER                     PIC X(02) VALUE '**'.       
      *                                                                 
           03 CLLPZY-DISP11.                                            
              05 FILLER                     PIC X(03) VALUE '** '.      
              05 CLLPZY-TABLENAME           PIC X(08).                  
              05 FILLER                     PIC X(04) VALUE ' |  '.     
              05 CLLPZY-FUN                 PIC X(01).                  
              05 FILLER                     PIC X(04) VALUE '  | '.     
              05 CLLPZY-DESCTAB             PIC X(27).                  
              05 FILLER                     PIC X(05) VALUE ' |   '.    
              05 CLLPZY-QTDETAB             PIC ZZZ.ZZZ.ZZZ.ZZ9.        
              05 FILLER                     PIC X(03) VALUE ' **'.      
      *                                                                 
           03 CLLPZY-DISP12.                                            
              05 FILLER                     PIC X(03) VALUE '** '.      
              05 FILLER                     PIC X(08) VALUE 'PARM  = '. 
              05 CLLPZY-PARM                PIC X(56) VALUE SPACES.     
              05 FILLER                     PIC X(03) VALUE ' **'.      
      *                                                                 
           03 CLLPZY-DISP13.                                            
              05 FILLER                     PIC X(03) VALUE '** '.      
              05 CLLPZY-COMENTARIO          PIC X(64) VALUE SPACES.     
              05 FILLER                     PIC X(03) VALUE ' **'.      
      *                                                                 
           03 CLLPZY-DISP14.                                            
              05 FILLER                     PIC X(02) VALUE ALL '*'.    
              05 FILLER                     PIC X(05) VALUE ALL ' '.    
              05 FILLER                     PIC X(04) VALUE ALL '*'.    
              05 FILLER                     PIC X(02) VALUE ALL ' '.    
              05 FILLER                     PIC X(45) VALUE             
             'ATENCAO, PROGRAMA OPERANDO EM MODO DE RESTART'.           
              05 FILLER                     PIC X(02) VALUE ALL ' '.    
              05 FILLER                     PIC X(04) VALUE ALL '*'.    
              05 FILLER                     PIC X(04) VALUE ALL ' '.    
              05 FILLER                     PIC X(02) VALUE ALL '*'.    
      *                                                                 
           03 CLLPZY-DISP15.                                            
              05 FILLER                     PIC X(03) VALUE '** '.      
              05 FILLER                     PIC X(16) VALUE             
             'CHAVE RESTART = '.                                        
              05 CLLPZY-RESTART             PIC X(48) VALUE SPACES.     
              05 FILLER                     REDEFINES CLLPZY-RESTART.   
                 07 CLLPZY-RESTART-CONT     PIC 9(11).                  
                 07 FILLER                  PIC X(37).                  
              05 FILLER                     PIC X(03) VALUE ' **'.      
      *                                                                 

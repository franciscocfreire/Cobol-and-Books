*********** CONVERTIDO DE AMODE=24 P/ AMODE=31 EM 17.01.2003-11:18:32   
*********** PROGRAMA COPIA DA POOL0210                                  
*********************************************************************** 
*                                                                     * 
*                                                                     * 
*                       B R A D 0 2 1 0                               * 
*                                                                     * 
*                                                                     * 
*        OBJETIVO       ESTE PGM E UM MODULO DE UM PGM QUALQUER       * 
*                       QUE PASSA O CONTROLE ENVIANDO O ENDERE-       * 
*                       CO DA DCB  DE  UM  OU  VARIOS ARQUIVOS.       * 
*                       SE O ARQUIVO  FOR  DE     ENTRADA O PGM       * 
*                       BRAD0210  PASSA  O    CONTROLE P/ O PGM       * 
*                       BRAD0190  E  SE  O ARQUIVO FOR DE SAIDA       * 
*                       PASSA O CONTROLE  P/ O PGM    BRAD0202.       * 
*                                                                     * 
*                                                                     * 
*********************************************************************** 
         EJECT                                                          
BRAD0210 AMODE 31                                                       
BRAD0210 RMODE ANY                                                      
BRAD0210 LKAGE BASE=3                                                   
         PRINT NOGEN                                                    
         LR    6,1                                                      
LACO     MVC   ENDCB,0(6)                                               
         L     4,ENDCB                                                  
         C     4,=F'0'                                                  
         BE    RETORNA                                                  
         C     4,=F'2'                                                  
         BNE   CLC                                                      
         NI    OUTPUT+1,X'0F'                                           
         B     RETORNA                                                  
*                                                                       
CLC      CLC   40(4,4),=F'0'                                            
         BNE   OPEN                                                     
         L     15,44(4)                                                 
         CNOP  0,4                                                      
         BAL   1,*+8                                                    
ENDCB    DC    F'0'                                                     
         BALR  14,15                                                    
RETORNA  TM    ENDCB,X'80'                                              
         BO    RET2                                                     
         LA    6,4(6)                                                   
         B     LACO                                                     
RET2     RETORNA                                                        
*                                                                       
OPEN     MVC   ENDCB1,ENDCB                                             
         NI    ENDCB1,X'7F'                                             
         CLC   50(2,4),=X'4800'                                         
         BE    INPUT                                                    
         CLC   50(2,4),=X'5000'                                         
         BE    INPUT                                                    
         CLC   50(2,4),=X'0048'                                         
         BE    OUTPUT                                                   
         CLC   50(2,4),=X'0050'                                         
         BE    OUTPUT                                                   
         WTO   'BRAD0210*ERRO NO PARAMETRO MACRF'                       
         ABEND 111,DUMP                                                 
*                                                                       
INPUT    LOAD  EP=BRAD0190                                              
NORM     LR    5,0                                                      
         LR    15,0                                                     
         CNOP  0,4                                                      
         BAL   1,*+8                                                    
ENDCB1   DC    F'0'                                                     
         BALR  14,15                                                    
         L     4,ENDCB1                                                 
         MVC   40(4,4),=F'0'                                            
         ST    5,44(4)                                                  
         OI    OUTPUT+1,X'F0'                                           
         B     RETORNA                                                  
*                                                                       
OUTPUT   B     LOAD                                                     
         OI    ENDCB1,X'80'                                             
LOAD     LOAD  EP=BRAD0202                                              
         B     NORM                                                     
         END                                                            

*********** CONVERTIDO DE AMODE=24 P/ AMODE=31 EM 17.01.2003-11:18:43   
*********** PROGRAMA COPIA DA POOL0300                                  
BRAD0300 AMODE 31                                                       
BRAD0300 RMODE ANY                                                      
BRAD0300 LKAGE BASE=3                                                   
         PRINT NOGEN                                                    
         L     4,4(1)                                                   
         L     1,0(1)                                                   
         MVC   ARQUIVO,0(1)                                             
NOPPESQ  NOP   PESQTAB                                          JAPI=I  
         OI    NOPPESQ+1,X'F0'                                  JAPI=I  
         GETMAIN R,LV=AREALEN,LOC=(BELOW,64)                    JAPI=I  
         LTR   15,15                                            JAPI=I  
         BZ    AREAOK                                           JAPI=I  
         WTO   'BRAD0300 * NAO FOI POSSIVEL ALOCAR MEMORIA *'   JAPI=I  
         B     ABEND                                            JAPI=I  
AREAOK   ST    1,TABELA                                         JAPI=I  
         USING DSAREA,6                                         JAPI=I  
         USING IHADCB,DSDCB                                     JAPI=I  
         USING DCBE,DSDCBE                                      JAPI=I  
         LR    8,1                                              JAPI=I  
         LR    10,1                                             JAPI=I  
         LA    9,AREALEN                                        JAPI=I  
         SLR   11,11                                            JAPI=I  
         MVCL  8,10                                             JAPI=I  
PESQTAB  LM    6,7,TABELA                                       JAPI=A  
PROCURA  CLC   DSARQ,ARQUIVO                                    JAPI=A  
         BE    ENC                                                      
         CLI   DSMARCA,X'00'                                    JAPI=A  
         BE    ABRE                                                     
         LA    6,UMAAREA(6)                                     JAPI=A  
         BCT   7,PROCURA                                                
         WTO   'BRAD0300 * ESTOURO DE TABELA *'                         
ABEND    ABEND 1111,DUMP                                                
*                                                                       
ABRE     MVI   DSMARCA,X'F0'                                    JAPI=A  
         MVC   DSARQ,ARQUIVO                                    JAPI=A  
         MVC   DSDCB,COPIA                                      JAPI=A  
         MVC   DSDCBE,COPIAE                                    JAPI=I  
         MVC   DSOPEN,OPENAREA                                  JAPI=I  
         LA    0,DCBE                                           JAPI=I  
         ST    0,DCBDCBE                                        JAPI=I  
         MVC   DCBDDNAM,ARQUIVO                                 JAPI=A  
         LA    1,DSDCB                                          JAPI=A  
         ST    1,FULLCALL                                               
         OI    FULLCALL,X'80'                                           
         CNOP  0,4                                                      
         BAL   1,*+12                                                   
FULLCALL DC    F'0'                                                     
ENDEBRAD DC    V(BRAD0210)                                              
         L     15,ENDEBRAD                                              
         BALR  14,15                                                    
         B     LER                                                      
*                                                                       
ENC      CLI   DSMARCA,X'F0'                                    JAPI=A  
         BE    LER                                                      
         WTO   'BRAD0300 * ARQUIVO JA FOI FECHADO *'                    
         B     ABEND                                                    
*                                                                       
LER      LR    0,4                                                      
         LA    1,DSDCB                                          JAPI=A  
         L     15,DCBGET                                        JAPI=A  
         BALR  14,15                                                    
         L     13,SAVEAREA+4                                            
         RETURN (14,12),RC=0                                            
*                                                                       
FIM      MVI   DSMARCA,X'FF'                                            
         LA    1,DSDCB                                          JAPI=A  
         ST    1,FC2                                                    
         OI    FC2,X'80'                                                
         CNOP  0,4                                                      
         BAL   1,*+8                                                    
FC2      DC    F'0'                                                     
         L     15,ENDEBRAD                                              
         BALR  14,15                                                    
         L     13,SAVEAREA+4                                            
         RETURN (14,12),RC=4                                            
*                                                                       
TABELA   DC    F'0',F'10'                                               
*                                                                       
MARCA    DC    CL1' '                                                   
ARQUIVO  DC    CL8' '                                                   
         DC    CL3' '                                                   
COPIA    DCB   DDNAME=XXXXXXXX,DSORG=PS,MACRF=GM,DCBE=COPIAE            
COPIAE   DCBE  EODAD=FIM,RMODE31=BUFF                                   
OPENAREA OPEN  (,INPUT),MF=L,MODE=24                                    
UMAAREA  EQU   *-MARCA                                                  
AREALEN  EQU   UMAAREA*10                                               
         LTORG                                                          
*                                                                       
* DSECT PARA MAPEAMENTO DA DCB                                          
         DCBD  DSORG=QS,DEVD=DA                                         
*                                                                       
* DSECT PARA MAPEAMENTO DA DCBE                                         
         IHADCBE                                                        
*                                                                       
* DSECT PARA MAPEAMENTO DA AREA ALOCADA                                 
DSAREA   DSECT                                                          
DSMARCA  DS    CL01                                                     
DSARQ    DS    CL08                                                     
         DS    CL03                                                     
DSDCB    DS    XL(DCBLNGQS)                                             
DSDCBE   DS    XL(DCBEEND-DCBE)                                         
DSOPEN   OPEN  (,),MF=L                                                 
         END                                                            

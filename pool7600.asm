         TITLE '*POOL7600* - CHAMA DINAMICAMENTE O MODULO POOL7601'     
POOL7600 AMODE 31                                                       
POOL7600 RMODE 24                                                       
POOL7600 CSECT                                                          
         USING POOL7600,15                                              
NOP1     NOP   PULA                                                     
         OI    NOP1+1,X'F0'                                             
         ST    1,FULL1                                                  
         LA    0,POOL7601                                               
         SR    1,1                                                      
         SVC   8                                                        
         BALR  15,0                                                     
POOL     LA    1,POOL-POOL7600                                          
         SR    15,1                                                     
         ST    0,FULL2                                                  
         L     1,FULL1                                                  
PULA     L     15,FULL2                                                 
         BR    15                                                       
*                                                                       
FULL1    DC    F'0'                                                     
FULL2    DC    F'0'                                                     
POOL7601 DC    CL8'POOL7601'                                            
         END                                                            

*********** CONVERTIDO DE AMODE=24 P/ AMODE=31 EM 17.01.2003-11:17:36   
*********** PROGRAMA COPIA DA POOL0002                                  
BRAD0002 TITLE '    ** COMPRIME UM ARQUIVO **'                          
         PRINT NOGEN                                                    
BRAD0002 AMODE 31                                                       
BRAD0002 RMODE ANY                                                      
BRAD0002 LKAGE                                                          
         GETMAIN R,LV=LENDCB1,LOC=(BELOW,64)                            
         ST    1,ADRDCB1                                                
         LR    5,1                                                      
         USING DSAREA,5                                                 
         USING IHADCB,DSDCB                                             
         USING DCBE,DSDCBE                                              
         MVC   IHADCB(LENDCB1),ENTRA                                    
         LA    0,DCBE                                                   
         ST    0,DCBDCBE                                                
         OPEN  (IHADCB,),MF=(E,DSOPEN)                                  
         GETMAIN R,LV=LENDCB2,LOC=(BELOW,64)                            
         ST    1,ADRDCB2                                                
         LR    5,1                                                      
         MVC   IHADCB(LENDCB2),SAI                                      
         LA    0,DCBE                                                   
         ST    0,DCBDCBE                                                
         CALL  BRAD0210,((5)),VL                                        
GET      L     5,ADRDCB1                                                
         GET   IHADCB                                                   
         LR    0,1                                                      
         AP    CONT,=P'1'                                               
         L     5,ADRDCB2                                                
*        PUT   IHADCB,(0)                                               
* A MACRO PUT FOI CODIFICADA DE MODO EXPANDIDO, PORQUE NA VERDADE, ESTE 
* PUT DESVIA PARA O BRAD0202. O ENDERECO DO BRAD0202 ESTA EM 31 BITS E  
* A MACRO PUT GERA INSTRUCOES QUE CONSIDERAM APENAS OS TRES BYTES DE    
* MAIS BAIXA DO ENDERECO DE DESVIO, O QUE OCASIONARIA ABEND S0C1.       
* PARA EVITAR O ABEND, CODIFICAMOS A PUT EXPANDIDA PARA QUE OS QUATRO   
* BYTES DO ENDERECO DE DESVIO SEJAM CONSIDERADOS.                       
*                                                                       
         LR    1,5                    >PUT EXPANDIDA                    
         L     15,48(1)               >PUT EXPANDIDA                    
         BALR  14,15                  >PUT EXPANDIDA                    
*                                                                       
         B     GET                                                      
FIM      L     5,ADRDCB1                                                
         CLOSE MF=(E,DSOPEN)                                            
         L     5,ADRDCB2                                                
         CALL  BRAD0210,((5)),VL                                        
         MVC   WTO+53(15),PADRAO                                        
         ED    WTO+53(15),CONT                                          
WTO      WTO   '* BRAD0002 * TOTAL DE REGISTROS DO ARQUIVO = 4XX.XXX.XX*
               X.XXX'                                                   
         RETORNA                                                        
CONT     DC    PL6'0'                                                   
PADRAO   DC    XL15'4020204B2020204B2020204B202020'                     
*                                                                       
ADRDCB1  DS    F                                                        
ADRDCB2  DS    F                                                        
ENTRA    DCB   DDNAME=ENTRA,DSORG=PS,MACRF=GL,DCBE=ENTRAE               
ENTRAE   DCBE  EODAD=FIM,RMODE31=BUFF                                   
         OPEN  (,INPUT),MF=L,MODE=24                                    
LENDCB1  EQU   *-ENTRA                                                  
SAI      DCB   DDNAME=SAI,DSORG=PS,MACRF=PM,DCBE=SAIE                   
SAIE     DCBE  RMODE31=BUFF                                             
         OPEN  (,OUTPUT),MF=L,MODE=24                                   
LENDCB2  EQU   *-SAI                                                    
*                                                                       
* DSECT PARA MAPEAMENTO DAS DCBS                                        
         DCBD  DSORG=QS,DEVD=DA                                         
*                                                                       
* DSECT PARA MAPEAMENTO DAS DCBES                                       
         IHADCBE                                                        
*                                                                       
* DSECT PARA MAPEAMENTO DA AREA ALOCADA                                 
DSAREA   DSECT                                                          
DSDCB    DS    XL(DCBLNGQS)                                             
DSDCBE   DS    XL(DCBEEND-DCBE)                                         
DSOPEN   OPEN  (,),MF=L                                                 
         END                                                            

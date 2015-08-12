*** CONVERTIDO DE AMODE=24 P/ AMODE=31 EM 14.01.2003-09:33:01   JAPI=I  
*** PROGRAMA COPIA DA POOL0431                                  JAPI=I  
****************************************************************        
*                                                              *        
*              U L T I M A    A L T E R A C A O                *        
****************************************************************        
*                                                              *        
*                        ----------                            *        
*                       | BRAD0431 |                           *        
*                        ----------                            *        
*                                                              *        
*    . ANALISTA          ==========>     KEILA                 *        
*    . GRUPO             ==========>     88                    *        
*    . DATA              ==========>     JULHO / 96            *        
*                                                              *        
*                                                              *        
*    . OBJETIVOS:   - CHAMAR DINAMICAMENTE O MODULO BRAD0441.  *        
*                     NESTA DATA TAMBEM FOI CORRIGIDO UM ERRO  *        
*                     NA BRAD QUANDO O VALOR ERA COMPACTADO E  *        
*                     TAMANHO DO VALOR IGUAL A ZEROS.   NESTA  *        
*                     SITUACAO A BRAD DEVOLVIA '.' NO   CAMPO  *        
*                     DIGITO MAS UMA AREA DE TRABALHO INTERNA  *        
*                     NAO ERA REINICIALIZADA OCASIONANDO ERRO  *        
*                     NO CALCULO DO DIGITO DA CHAMADA SEGUIN-  *        
*                     TE.                                      *        
*                                                              *        
****************************************************************        
         EJECT                                                          
BRAD0431 AMODE 31                                                       
BRAD0431 RMODE ANY                                                      
BRAD0431 CSECT                                                          
         USING BRAD0431,15                                              
NOP1     NOP   PULA                                                     
         OI    NOP1+1,X'F0'                                             
         ST    1,FULL1                                                  
         LA    0,=CL8'BRAD0441'                                         
         SR    1,1                                                      
         SVC   8                                                        
         BALR  15,0                                                     
BRAD     LA    1,BRAD-BRAD0431                                          
         SR    15,1                                                     
         ST    0,FULL2                                                  
         L     1,FULL1                                                  
PULA     L     15,FULL2                                                 
         BR    15                                                       
*                                                                       
FULL1    DC    F'0'                                                     
FULL2    DC    F'0'                                                     
         END                                                            

*********** CONVERTIDO DE AMODE=24 P/ AMODE=31 EM 28.01.2003-13:55:05   
*********** PROGRAMA COPIA DA POOL0202                                  
*********************************************************************** 
*                                                                     * 
*                          B R A D 0 2 0 2                            * 
*                                                                     * 
*                                                                     * 
*                                                                     * 
*        OBJETIVO       FAZER GRAVACAO DE REGISTROS DE UM             * 
*                       ARQUIVO,  COMPRIMINDO ESTES REGISTROS         * 
*                       COM BASE EM CODIGOS ESTABELECIDOS             * 
*                       P/ COMPRESSAO                                 * 
*                                                                     * 
*********************************************************************** 
         SPACE 3                                                        
*********************************************************************** 
*                                                                     * 
*            CODIGOS USADOS NA COMPRESSAO DE REGISTROS                * 
*                                                                     * 
*                                                                     * 
*     CODIGOS     SIGNIFICADO                                         * 
*                                                                     * 
*                                                                     * 
*     0X          OS PROXIMOS BYTES COM TAMANHO CONTIDO EM   X        * 
*                 NAO SE ALTERAM (USADO PARA TAMANHO DE CAMPOS        * 
*                 DE ATE 16 BYTES)                                    * 
*                                                                     * 
*     10 XX       OS PROXIMOS BYTES COM TAMANHO CONTIDO EM  XX        * 
*                 NAO SE ALTERAM (USADO PARA TAMANHO DE CAMPOS        * 
*                 DE ATE 256 BYTES)                                   * 
*                                                                     * 
*     2X          OS PROXIMOS BYTES DO REGISTRO SE REPETEM  DO        * 
*                 REGISTRO ANTERIOR NO TAMANHO INDICADO POR  X        * 
*                 (ATE 16 BYTES)                                      * 
*                                                                     * 
*     30 XX       OS PROXIMOS BYTES DO REGISTRO SE REPETEM  DO        * 
*                 REGISTRO ANTERIOR NO TAMANHO INDICADO POR XX        * 
*                 (ATE 256 BYTES)                                     * 
*                                                                     * 
*     4X          O PROXIMO BYTE SE REPETE O NUMERO  DE  VEZES        * 
*                 INDICADO POR  X  (ATE 16 BYTES)                     * 
*                                                                     * 
*     50 XX       O PROXIMO BYTE SE REPETE O NUMERO  DE  VEZES        * 
*                 INDICADO POR  XX  (ATE 256 BYTES)                   * 
*                                                                     * 
*     D0          FINAL DE REGISTRO                                   * 
*                                                                     * 
*     E0          FINAL DE BLOCO                                      * 
*                                                                     * 
*     F0          FINAL DE ARQUIVO                                    * 
*                                                                     * 
*********************************************************************** 
         EJECT                                                          
BRAD0202 AMODE 31                                                       
BRAD0202 RMODE ANY                                                      
BRAD0202 LKAGE BASE=R3                                                  
         PRINT NOGEN                                                    
         SPACE                                                          
*   RECEBE ENDERECO DA DCB DO CHAMADOR                                  
         SPACE                                                          
         MVC   DCBPGM,0(R1)                                             
NOP      NOP   CLOSE                                                    
         OI    NOP+1,X'F0'                                              
         SPACE 2                                                        
R0       EQU   0                POINTER REGATUAL CLCL                   
R1       EQU   1                LENGTH OPERANDO 1 CLCL                  
R2       EQU   2                                                        
R3       EQU   3                BASE                                    
R4       EQU   4                POINTER REGATUAL                        
R5       EQU   5                POINTER REGANT                          
R6       EQU   6                FIMBUF                                  
R7       EQU   7                                                        
R8       EQU   8                POINTER BUFFER DE GRAVACAO              
R9       EQU   9                                                        
R10      EQU  10                POINTER REGANT OU REGATUAL+1 CLCL       
R11      EQU  11                LENGTH OPERANDO 2 CLCL                  
R12      EQU  12                FIM REGANT - 1                          
R13      EQU  13                                                        
R14      EQU  14                                                        
R15      EQU  15                NUMERO DE BYTES IGUAIS,REPET OU DIFER.  
         EJECT                                                          
**************************************************                      
*        FASE 1 - ABRE ARQUIVO PARA GRAVACAO     *                      
**************************************************                      
         SPACE                                                          
*   MOVE DCB DO CHMADOR P/ AREA NO PGM                                  
         SPACE                                                          
         GETMAIN R,LV=DSAREALE,LOC=(BELOW,64)                           
         ST    R1,DCB                                                   
         GETMAIN R,LV=DSAREALE,LOC=(BELOW,64)                           
         ST    R1,DCB2                                                  
         USING DSAREA,R5                                                
         L     R5,DCBPGM                                                
         L     R2,DCB                                                   
         MVC   0(DSAREALE,R2),0(R5)                                     
         L     R2,DCB2                                                  
         MVC   0(DSAREALE,R2),0(R5)                                     
         SPACE                                                          
*   CHECA MACRF - SE PM ALTERA P/ PL                                    
         SPACE                                                          
         L     R5,DCB                                                   
         CLC   50(2,R5),=X'0048'               PL                       
         BNE   VEPM                                                     
         OI    NOP1+1,X'F0'                                             
         OI    NOP2+1,X'F0'                                             
         B     OPEN                                                     
VEPM     CLC   50(2,R5),=X'0050'               PM                       
         BNE   ERRO                                                     
         MVI   51(R5),X'48'                                             
         SPACE                                                          
*   ABRE ARQUIVO E CHECA DSORG E RECFM E EMITE O PUT LOCATE             
         SPACE                                                          
OPEN     OPEN  (DSDCB,),MF=(E,DSOPEN)                                   
         CLI   26(R5),X'40'             DSORG=PS                        
         BNE   ERRO                                                     
         CLI   36(R5),X'90'             RECFM=FB                        
         BE    PL                                                       
         CLI   36(R5),X'80'             RECFM=F                         
         BNE   ERRO                                                     
PL       TM    DCBPGM,X'80'                                             
         BNO   ALTIO                                                    
         L     R1,DCB                                                   
         L     R2,DCB2                                                  
         MVI   51(R2),X'50'                                             
         MVC   40(3,R2),=C'BAC'                                         
         MVC   82(2,R2),82(R1)                                          
         MVC   62(2,R2),62(R1)                                          
         ST    R5,SAVREG05                                              
         LR    R5,R2                                                    
         OPEN  (DSDCB,),MF=(E,DSOPEN)                                   
         L     R5,SAVREG05                                              
         CLI   48(R2),X'02'                                             
         BNE   OKOPEN                                                   
         MVC   WTODD+19(8),40(R2)                                       
WTODD    WTO   'BRAD0202 - XXXXXXXX DD STATEMENT MISSING'               
         WTO   'BRAD0202 - FALTA CARTAO DD PARA O ARQUIVO DE BACKUP  - X
                 PROVIDENCIAR ALTERACAO NO JCL E REEXECUTAR O PROGRAMA' 
         ABEND 1111,DUMP                                                
OKOPEN   OI    DCB2,X'80'                                               
         NI    B+1,X'0F'                                                
* ALTERA ENDERECO DA ROTINA INTERNA DE PUT                              
         SPACE 1                                                        
ALTIO    MVC   ENDPUT,48(R5)                                            
         L     R7,DCBPGM                                                
         MVC   48(4,R7),=A(END1)                                        
         SPACE 2                                                        
RETORNA  L     R13,SAVEAREA+4                                           
         LM    R14,R12,12(R13)                                          
         LA    R15,0                                                    
         BR    R14                                                      
         EJECT                                                          
***********************************************************             
*        FASE 2 - COMPRESSAO E GRAVACAO DOS REGISTROS     *             
***********************************************************             
         SPACE                                                          
* SALVA REGISTRADORES DO CHAMADOR E POSICIONA REGISTRADOR BASE          
         SPACE                                                          
END1     EQU   *                                                        
         STM   R14,R12,12(R13)                                          
         LR    R3,R15                                                   
         LA    R4,END1-BRAD0202                                         
         SR    R3,R4                                                    
         LA    R2,SAVEAREA                                              
         ST    R13,4(R2)                                                
         ST    R2,8(R13)                                                
         LR    R13,R2                                                   
         SPACE                                                          
NOP0     NOP   NOP1                                                     
         OI    NOP0+1,X'F0'                                             
         ST    R0,REGATU                                                
         L     R5,DCB                                                   
PL2      PUT   (R5)                                                     
         ST    R1,BUF                                                   
         ST    R1,BUF2                                                  
         SPACE 1                                                        
* GUARDA O LRECL                                                        
         SPACE 1                                                        
         MVC   LRECL+2(2),82(R5)                                        
         A     R1,LRECL                                                 
         ST    R1,FIMBUF        LOAD FIMBUF                             
         SPACE 1                                                        
* DETERMINA O NUMERO DE REGISTROS NO BLOCO                              
         SPACE 1                                                        
         SR    R8,R8                                                    
         LH    R9,62(R5)                                                
         D     R8,LRECL                                                 
         ST    R9,CONTADOR                                              
         ST    R9,TOTREGS                                               
         SPACE 1                                                        
* COLOCA O CONTADOR DE BLOCO NO BUFFER                                  
         SPACE 1                                                        
         L     R1,BUF                                                   
         MVC   0(4,R1),COBL                                             
         LA    R1,4(R1)                                                 
         ST    R1,BUF                                                   
         SPACE 1                                                        
* ALOCA AREAS DE TRABALHO                                               
         SPACE 1                                                        
         L     R0,LRECL                                                 
         TM    NOP1+1,X'F0'                                             
         BNO   P2                                                       
         MH    R0,=H'2'                                                 
P2       A     R0,=F'16'                                                
         GETMAIN R,LV=(R0)                                              
         ST    R1,REGANT                                                
         A     R1,LRECL                                                 
         ST    R1,FIMREGAN                                              
         ST    R1,BUFFER                                                
         SPACE 1                                                        
* BRANQUEIA O REGISTRO ANTERIOR                                         
         SPACE 1                                                        
P3       LM    R8,R9,REGANT                                             
         LM    R10,R11,XL8                                              
         MVCL  R8,R10                                                   
         L     R0,REGATU                                                
         SPACE                                                          
*   PUT MOVE OU PUT LOCATE                                              
         SPACE                                                          
NOP1     NOP   RETLOC1                                                  
PUTM     ST    R0,REGATU                                                
         SPACE 2                                                        
BRAD0205 EQU   *                                                        
         L     R6,FIMBUF        LOAD R6 FIMBUF                          
         MVC   END8,BUF                                                 
*                                                                       
LACO10   EQU   *                                                        
         LM    R4,R5,REGATU     END. REG ATUAL E REG ANTERIOR           
         L     R8,BUF                                                   
LACO     NOP   LACO2                                                    
*********************************************                           
* COMPARACAO DE IGUAL REPETICAO E DIFERENTE *                           
*********************************************                           
LACO1    EQU   *                                                        
         LR    R0,R4            END. REG ATUAL                          
         LR    R10,R5           END. REG ANTERIOR                       
         SR    R15,R15          ZERA NUMERO DE BYTES IGUAIS OU REPETIV. 
         L     R11,FIMREGAN     FIM REG. ANTERIOR                       
         LR    R12,R11                                                  
         BCTR  R12,R0           FIM REG. ANTERIOR -1                    
         EJECT                                                          
*********************************************                           
*         COMPARACAO DE IGUAL               *                           
*********************************************                           
VEIGUAL  EQU   *                                                        
         CR    R10,R12          COMPARA REG. ANT X REG. ANT -1          
         BE    COMPCLC1                                                 
         BH    ABEND01                                                  
         SPACE                                                          
         SR    R11,R10          TAMANHO DA COMPARACAO CLCL              
         CH    R11,=H'256'      MAIOR TAMANHO = 256                     
         BNH   TAMOK                                                    
         LA    R11,256          SE MAIOR, FORCA 256                     
TAMOK    LR    R1,R11           TAMANHO DOS 2 OPERANDOS CLCL            
         LR    R15,R11          TAMANHO NO REG. 15                      
         BCTR  R15,R0           TAMANHO -1                              
         LR    R7,R0                                                    
         LA    R7,1(R7)         END. REGISTRO ATUAL + 1                 
         CLCL  R0,R10           COMPARA REGISTRO ATUAL X ANTERIOR       
         BE    IGUAL                                                    
         CR    R0,R7            VERIFICA ONDE PAROU A COMPARACAO        
         BL    VEREPET                                                  
         BE    VEREPET1                                                 
         SR    R15,R1           CARREGA NUMERO DE BYTES IGUAIS          
         SPACE 1                                                        
* SUB-ROTINA DE IGUAL AO REGISTRO ANTERIOR                              
         SPACE 1                                                        
IGUAL    NOP   IGUALX                                                   
         L     R1,FULLIG                                                
         AR    R1,R15                                                   
         ST    R1,FULLIG                                                
IGUALX   CH    R15,=H'15'                                               
         BH    IGUAL1                                                   
         STC   R15,0(R8)                                                
         OI    0(R8),X'20'                                              
         LA    R8,1(R8)                                                 
         CR    R8,R6            FIM DO BUFFER?                          
         BL    DESL1                                                    
         BAL   R10,SALTA2                                               
DESL1    LA    R4,1(R4,R15)                                             
         LA    R5,1(R5,R15)                                             
         C     R5,FIMREGAN      FIM REGISTRO ANTERIOR?                  
         BH    ABEND02                                                  
         BL    LACO                                                     
         BE    FIM                                                      
         SPACE                                                          
IGUAL1   MVI   0(R8),X'30'                                              
         LA    R8,1(R8)                                                 
         CR    R8,R6            FIM DO BUFFER?                          
         BL    STC1                                                     
         BAL   R10,SALTA2                                               
         STC   R15,0(R8)                                                
         LA    R8,1(R8)                                                 
         B     DESL2                                                    
STC1     STC   15,0(8)                                                  
         LA    R8,1(R8)                                                 
         CR    R8,R6            FIM DO BUFFER?                          
         BL    DESL2                                                    
         BAL   R10,SALTA2                                               
DESL2    LA    R4,1(R4,R15)                                             
         LA    R5,1(R5,R15)                                             
         C     R5,FIMREGAN      FIM REGISTRO ANTERIOR?                  
         BH    ABEND03                                                  
         BL    LACO                                                     
         BE    FIM                                                      
         SPACE                                                          
COMPCLC1 LR    R9,R0                                                    
         CLC   0(1,R9),0(R10)   QUANDO ULTIMO BYTE DOS REGISTROS        
         BNE   DIFER            COMPARA SOMENTE UMA POSICAO             
         B     IGUAL                                                    
         SPACE 2                                                        
********************************************************                
* COMPARACAO DE IGUAL REPETICAO E DIFERENTE - MODO CLC *                
********************************************************                
LACO2    EQU   *                                                        
         LR    R9,R4                                                    
         LR    R10,R5                                                   
         SR    R15,R15                                                  
         L     R11,FIMREGAN                                             
         BCTR  R11,R0                                                   
         SPACE 2                                                        
*********************************************                           
*         COMPARACAO DE IGUAL - CLC         *                           
*********************************************                           
VEIGUAL2 EQU   *                                                        
         CR    R10,R11                                                  
         BE    DIFER                                                    
         BH    ABEND04                                                  
         SPACE                                                          
COMP22   CLC   0(2,R9),0(R10)                                           
         BNE   VEREPET2                                                 
         LA    R9,2(R9)                                                 
         LA    R10,2(R10)                                               
         LA    R15,1(R15)                                               
         C     R10,FIMREGAN                                             
         BE    IGUAL                                                    
         SPACE                                                          
COMP11   CLC   0(1,R9),0(R10)                                           
         BNE   IGUAL                                                    
         LA    R9,1(R9)                                                 
         LA    R10,1(R10)                                               
         LA    R15,1(R15)                                               
         C     R10,FIMREGAN                                             
         BE    IGUAL                                                    
         CH    R15,=H'255'                                              
         BNE   COMP11                                                   
         B     IGUAL                                                    
         EJECT                                                          
*********************************************                           
*         COMPARACAO DE REPETICAO           *                           
*********************************************                           
VEREPET  EQU   *                                                        
         BCTR  R1,R0                                                    
         BCTR  R11,R0           SUBTRAI 1 DO TAMANHO DA COMPARACAO      
VEREPET1 LR    R0,R4            END REG. ATUAL                          
         LR    R10,R7           END REG. ATUAL + 1                      
         CLCL  R0,R10           COMPARA REG ATUAL X REG ATUAL + 1       
         BE    REPET                                                    
         CR    R10,R7           VERIFICA ONDE PAROU A COMPARACAO        
         BNH   DIFERENT                                                 
         SR    R15,R1           CARREGA NUMERO DE BYTES QUE SE REPETEM  
         SPACE 1                                                        
* SUB-ROTINA DE REPETICAO DE BYTES *                                    
         SPACE 1                                                        
REPET    NOP   REPETX                                                   
         L     R1,FULLRE                                                
         AR    R1,R15                                                   
         ST    R1,FULLRE                                                
REPETX   CH    R15,=H'15'                                               
         BH    REPET2                                                   
         STC   R15,0(R8)                                                
         OI    0(R8),X'40'                                              
         LA    R8,1(R8)                                                 
         CR    R8,R6            FIM DO BUFFER?                          
         BL    REPET1                                                   
         BAL   R10,SALTA2                                               
         MVC   0(1,R8),0(R4)                                            
         LA    R8,1(R8)                                                 
         B     DESL3                                                    
REPET1   MVC   0(1,R8),0(R4)                                            
         LA    R8,1(R8)                                                 
         CR    R8,R6            FIM DO BUFFER?                          
         BL    DESL3                                                    
         BAL   R10,SALTA2                                               
DESL3    LA    R4,1(R4,R15)                                             
         LA    R5,1(R5,R15)                                             
         C     R5,FIMREGAN      FIM REGISTRO ANTERIOR?                  
         BH    ABEND05                                                  
         BL    LACO                                                     
         BE    FIM                                                      
         SPACE                                                          
REPET2   MVI   0(R8),X'50'                                              
         LA    R8,1(R8)                                                 
         CR    R8,R6            FIM DO BUFFER?                          
         BL    STC2                                                     
         BAL   R10,SALTA2                                               
         STC   R15,0(R8)                                                
         MVC   1(1,R8),0(R4)                                            
         LA    R8,2(R8)                                                 
         B     DESL4                                                    
STC2     STC   R15,0(R8)                                                
         LA    R8,1(R8)                                                 
         CR    R8,R6            FIM DO BUFFER?                          
         BL    MVC                                                      
         BAL   R10,SALTA2                                               
         MVC   0(1,R8),0(R4)                                            
         LA    R8,1(R8)                                                 
         B     DESL4                                                    
MVC      MVC   0(1,R8),0(R4)                                            
         LA    R8,1(R8)                                                 
         CR    R8,R6            FIM DO BUFFER?                          
         BL    DESL4                                                    
         BAL   R10,SALTA2                                               
DESL4    LA    R4,1(R4,R15)                                             
         LA    R5,1(R5,R15)                                             
         C     R5,FIMREGAN      FIM REGISTRO ANTERIOR?                  
         BH    ABEND06                                                  
         BL    LACO                                                     
         BE    FIM                                                      
         SPACE 2                                                        
*********************************************                           
*         COMPARACAO DE REPETICAO - CLC     *                           
*********************************************                           
VEREPET2 EQU   *                                                        
         CLC   0(1,R9),1(R9)                                            
         BNE   DIFEREN2                                                 
         LA    R15,1(R15)                                               
L2       LA    R9,1(R9)                                                 
         LA    R10,1(R10)                                               
         CR    R10,R11                                                  
         BE    REPET                                                    
         CLC   0(1,R9),1(R9)                                            
         BNE   REPET                                                    
         LA    R15,1(R15)                                               
         CH    R15,=H'255'                                              
         BNE   L2                                                       
         B     REPET                                                    
         EJECT                                                          
*********************************************                           
*         COMPARACAO DE DIFERENTE           *                           
*********************************************                           
DIFERENT EQU   *                                                        
         SR    R15,R15          ZERA NUMERO DE BYTES                    
         LR    R10,R5           CARREGA END REG ANTERIOR                
         LR    R9,R0            CARREGA END REG ATUAL                   
         SPACE                                                          
DESLDIF  LA    R9,1(R9)         DESLOCA END REG ATUAL                   
         LA    R10,1(R10)       DESLOCA END REG ANTERIOR                
         CR    R10,R12          COMPARA END ANT X END ANT + 1           
         BH    ABEND07                                                  
         BE    ADDIFER                                                  
         CLC   0(2,R9),0(R10)   COMPARA REG ATUAL X REG ANT             
         BE    DIFER                                                    
         CLC   0(1,R9),1(R9)    COMPARA REG ATUAL X REG ATUAL + 1       
         BE    DIFER                                                    
         LA    R15,1(R15)                                               
         CH    R15,=H'255'                                              
         BNE   DESLDIF                                                  
         B     DIFER                                                    
ADDIFER  LA    R15,1(R15)                                               
         SPACE 1                                                        
* SUB-ROTINA DE DIFERENTE *                                             
         SPACE 1                                                        
DIFER    NOP   DIFERX                                                   
         L     R1,FULLDI                                                
         AR    R1,R15                                                   
         ST    R1,FULLDI                                                
DIFERX   CH    R15,=H'15'                                               
         BH    DIFER2                                                   
DIFER1   STC   R15,0(R8)                                                
         LA    R8,1(R8)                                                 
         CR    R8,R6            FIM DO BUFFER?                          
         BL    LA                                                       
         BAL   R10,SALTA2                                               
LA       LA    R11,1(R15,R8)                                            
         CR    R11,R6           FIM DO BUFFER?                          
         BNH   OKK                                                      
         SR    R11,R6                                                   
         SR    R15,R11                                                  
         EX    R15,MOVE                                                 
         LA    R4,1(R4,R15)                                             
         LA    R5,1(R5,R15)                                             
         BAL   R10,SALTA2                                               
         LR    R15,R11                                                  
         BCTR  R15,R0                                                   
         B     LA                                                       
OKK      EX    R15,MOVE                                                 
         LA    R8,1(R15,R8)                                             
         CR    R8,R6            FIM DO BUFFER?                          
         BNL   VAISALT                                                  
DESL5    LA    R4,1(R4,R15)                                             
         LA    R5,1(R5,R15)                                             
         C     R5,FIMREGAN      FIM REGISTRO ANTERIOR?                  
         BH    ABEND08                                                  
         BL    LACO                                                     
         BE    FIM                                                      
VAISALT  LA    R10,DESL5                                                
         B     SALTA2                                                   
         SPACE                                                          
DIFER2   MVI   0(R8),X'10'                                              
         LA    R8,1(R8)                                                 
         CR    R8,R6            FIM DO BUFFER?                          
         BL    DIFER1                                                   
         LA    R10,DIFER1                                               
         B     SALTA2                                                   
*                                                                       
MOVE     MVC   0(0,R8),0(R4)                                            
*                                                                       
         SPACE 2                                                        
*********************************************                           
*         COMPARACAO DE DIFERENTE - CLC     *                           
*********************************************                           
DIFEREN2 EQU   *                                                        
         LA    R9,1(R9)                                                 
         LA    R10,1(R10)                                               
         CR    R10,R11                                                  
         BH    ABEND09                                                  
         BE    ADDIFER2                                                 
         CLC   0(2,R9),0(R10)                                           
         BE    DIFER                                                    
         CLC   0(1,R9),1(R9)                                            
         BE    DIFER                                                    
         LA    R15,1(R15)                                               
         CH    R15,=H'255'                                              
         BNE   DIFEREN2                                                 
         B     DIFER                                                    
ADDIFER2 LA    R15,1(R15)                                               
         B     DIFER                                                    
         EJECT                                                          
*********************************************                           
*     ROTINA PARA ENCERRAMENTO DO BLOCO     *                           
*********************************************                           
SALTA2   EQU   *                                                        
         ST    R15,GUARDA                                               
         L     R15,CONTADOR                                             
         BCT   R15,SALTA2A                                              
         SPACE                                                          
         SP    CHECOUNT,=P'1'                                           
         BNZ   LM89                                                     
NOPABRE  NOP   ABRECONT                                                 
         OI    NOPABRE+1,X'F0'                                          
         ZAP   CHECOUNT,=P'100'                                         
         L     R9,FULLDI                                                
         M     R8,=F'10000'                                             
         L     R10,FULLIG                                               
         A     R10,FULLRE                                               
         DR    R8,R10                                                   
         L     R10,=F'10000'                                            
         SR    R10,R9                                                   
         XC    FULLIG(12),FULLIG                                        
         OI    IGUAL+1,X'F0'                                            
         OI    REPET+1,X'F0'                                            
         OI    DIFER+1,X'F0'                                            
         C     R10,=F'2000'                                             
         BL    USARCLC                                                  
         NI    LACO+1,X'0F'                                             
*                                                                       
LM89     LM    R8,R9,REGANT                                             
         LM    R10,R11,XL8                                              
         MVCL  R8,R10                                                   
         L     R8,END8                                                  
         MVI   0(R8),X'E0'                                              
         LA    R8,1(R8)                                                 
         LR    R9,R6            LOAD FIMBUF                             
         SR    R9,R8                                                    
         SR    R10,R10                                                  
         SR    R11,R11                                                  
         MVCL  R8,R10                                                   
         LA    R10,LACO10                                               
*                                                                       
SALTA2A  TM    DCB2,X'80'                                               
         BNO   SALTA2B                                                  
         LM    R0,R1,BUF2                                               
         SLR   R15,R15                                                  
         ICM   R15,7,49(R1)                                             
         BALR  R14,R15                                                  
*                                                                       
SALTA2B  L     R1,DCB                                                   
         SLR   R15,R15                                                  
         ICM   R15,7,ENDPUT+1                                           
         BALR  R14,R15                                                  
         LR    R8,R1                                                    
         ST    R1,BUF2                                                  
         L     R15,CONTADOR                                             
         BCT   R15,SALTA3                                               
         L     R15,COBL                                                 
         LA    R15,1(R15)                                               
         ST    R15,0(R8)                                                
         ST    R15,COBL                                                 
         LA    R8,4(R8)                                                 
         L     R15,TOTREGS                                              
SALTA3   ST    R15,CONTADOR                                             
         ST    R8,BUF                                                   
         A     R1,LRECL                                                 
         LR    R6,R1            NOW LOAD FIMBUF                         
         L     R15,GUARDA                                               
         BR    R10                                                      
         SPACE 2                                                        
ABRECONT ZAP   CHECOUNT,=P'1'                                           
         NI    NOPABRE+1,X'0F'                                          
         NI    IGUAL+1,X'0F'                                            
         NI    REPET+1,X'0F'                                            
         NI    DIFER+1,X'0F'                                            
         B     LM89                                                     
         SPACE 2                                                        
USARCLC  OI    LACO+1,X'F0'                                             
         B     LM89                                                     
         EJECT                                                          
*********************************************                           
*     ROTINA DE FIM DO REGISTRO COMPRIMIDO  *                           
*********************************************                           
FIM      EQU   *                                                        
         MVI   0(R8),X'D0'                                              
         LA    R8,1(R8)                                                 
         CR    R8,R6            FIM DO BUFFER?                          
         BL    ST8                                                      
         BAL   R10,SALTA2                                               
ST8      ST    R8,BUF                                                   
         SPACE                                                          
*   MOVE REGISTRO ATUAL P/ REGISTRO ANTERIOR                            
         SPACE                                                          
         LM    R8,R9,REGANT                                             
         L     R10,REGATU                                               
         LR    R11,R9                                                   
         MVCL  R8,R10                                                   
         ST    R6,FIMBUF        STOR FIMBUF                             
NOP2     NOP   RETLOC2                                                  
         L     R13,SAVEAREA+4                                           
         LM    R14,R12,12(R13)                                          
         BR    R14                                                      
         SPACE 3                                                        
*********************************************                           
***   ROTINA MODO DE GRAVACAO PUT LOCATE  ***                           
*********************************************                           
         SPACE                                                          
*   DEVOLVE ENDERECO DO REGISTRO COMPRIMIDO NO REGISTRADOR 1            
         SPACE                                                          
RETLOC1  MVC   NOP1+2(2),=S(PUTL)                                       
RETLOC2  L     R13,SAVEAREA+4                                           
         MVC   24(4,R13),BUFFER                                         
         LM    R14,R12,12(R13)                                          
         BR    R14                                                      
         SPACE                                                          
PUTL     MVC   REGATU,BUFFER                                            
         B     BRAD0205                                                 
         EJECT                                                          
*************************************************************           
*        FASE 3 - GRAVA ULTIMO REGISTRO E FECHA ARQUIVO     *           
*************************************************************           
         SPACE 2                                                        
CLOSE    TM    NOP0+1,X'F0'                                             
         BNO   FECHA                                                    
         TM    NOP1+1,X'F0'                                             
         BNO   CLOSE2                                                   
         MVC   NOP2+2(2),=S(CLOSE2)                                     
         B     PUTL                                                     
         SPACE 2                                                        
CLOSE2   L     R8,BUF                                                   
         MVI   0(R8),X'F0'                                              
FECHA    L     R5,DCB                                                   
         MVC   48(4,R5),ENDPUT                                          
         CLOSE MF=(E,DSOPEN)                                            
B        B     RETORNA                                                  
         L     R1,DCB2                                                  
         L     R0,BUF2                                                  
         SLR   R15,R15                                                  
         ICM   R15,7,49(R1)                                             
         TM    NOP0+1,X'F0'                                             
         BNO   FECHAB                                                   
         BALR  R14,R15                                                  
FECHAB   ST    R5,SAVREG05                                              
         L     R5,DCB2                                                  
         CLOSE MF=(E,DSOPEN)                                            
         L     R5,SAVREG05                                              
         B     RETORNA                                                  
         EJECT                                                          
*                                                                       
*   AREAS AUXILIARES                                                    
*                                                                       
CHECOUNT DC    PL2'1'                                                   
FULLIG   DC    F'0'                                                     
FULLRE   DC    F'0'                                                     
FULLDI   DC    F'0'                                                     
*                                                                       
REGATU   DC    A(0)                      ****                           
REGANT   DC    A(0)                      **** PARTE DE INSTRUCAO LM     
LRECL    DC    F'0'                      ****                           
*                                                                       
BUF2     DC    A(0)                      **** PARTE DE INSTRUCAO LM     
DCB2     DS    F                         ****                           
*                                                                       
FIMBUF   DC    A(0)                                                     
FIMREGAN DC    A(0)                                                     
TOTREGS  DC    F'0'                                                     
DCB      DS    F                                                        
ENDPUT   DC    A(0)                                                     
BUF      DC    A(0)                                                     
COBL     DC    F'1'                                                     
CONTADOR DC    F'0'                                                     
*                                                                       
*                                                                       
BUFFER   DC    A(0)                                                     
DCBPGM   DC    F'0'                                                     
GUARDA   DC    F'0'                                                     
END8     DC    F'0'                                                     
         DS    0F                                                       
XL8      DC    XL8'0000000040000000'                                    
SAVREG05 DS    F                                                        
*                                                                       
*        D C B                                                          
*                                                                       
         DS    0F                                                       
*DCBREAL  DC    CL96' '                                                 
*DCBREBK  DC    CL96' '                                                 
*                                                                       
*                                                                       
ERRO     WTO   'BRAD0202 - MACRF,DSORG OU RECFM FORA DO PADRAO'         
         ABEND 1234,DUMP                                                
AB       DC    PL1'0'                                                   
ABEND09  AP    AB,=P'1'                                                 
ABEND08  AP    AB,=P'1'                                                 
ABEND07  AP    AB,=P'1'                                                 
ABEND06  AP    AB,=P'1'                                                 
ABEND05  AP    AB,=P'1'                                                 
ABEND04  AP    AB,=P'1'                                                 
ABEND03  AP    AB,=P'1'                                                 
ABEND02  AP    AB,=P'1'                                                 
ABEND01  AP    AB,=P'1'                                                 
         OI    AB,X'0F'                                                 
         UNPK  ABEND+34(1),AB                                           
ABEND    WTO   'BRAD0202 * ERRO DE LOGICA X *'                          
         ABEND 123,DUMP                                                 
         SPACE 2                                                        
         LTORG                                                          
*                                                                       
* DSECT PARA MAPEAMENTO DAS DCBS                                        
         DCBD  DSORG=QS,DEVD=DA                                         
*                                                                       
* DSECT PARA MAPEAMENTO DAS DCBES                                       
         IHADCBE                                                        
*                                                                       
* DSECT PARA MAPEAMENTO DA AREA PASSADA PELO BRAD0002/0300              
DSAREA   DSECT                                                          
DSDCB    DS    XL(DCBLNGQS)                                             
DSDCBE   DS    XL(DCBEEND-DCBE)                                         
DSOPEN   OPEN  (,),MF=L                                                 
DSAREALE EQU   *-DSAREA                                                 
         END   BRAD0202                                                 

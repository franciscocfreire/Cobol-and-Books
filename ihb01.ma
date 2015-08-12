**********************************************************************  
*  COPYRIGHT:                                                        *  
*PROPRIETARY V3 STATEMENT                                               
*LICENSED MATERIALS - PROPERTY OF IBM                                   
*"RESTRICTED MATERIALS OF IBM"                                          
*5694-A01                                                               
*COPYRIGHT IBM CORP. 1982,2010                                          
*END PROPRIETARY V3 STATEMENT                                           
**********************************************************************  
         MACRO                                                          
         IHB01 &DSORG,&MACRF,&BFTEK,&BFALN,&EODAD,&RECFM,&EXLST,       *
               &BUFCB,&BUFL,&IOBAD,&EOEA,&PCIA,&SIOA,&CENDA,&XENDA,    *
               &AERR,&PGFX,&OPTCD,&BUFNO,&DEVD,&REPOS,&DCBE             
.*  THIS MACRO CHECKS DSORG, MACRF, RECFM, OPTCD, & DEVD FOR DCB MACRO  
.*                                                                      
.*                      SET SYMBOL DECLARATIONS                         
.*                                                                      
         GBLA  &A0                                                      
         GBLB  &DSORG0,&DSORG1,&DSORG2,&DSORG3,&DSORG4,&DSORG5,&DSORG6  
         GBLB  &DSORG7,&MACRF0,&MACRF1,&MACRF2,&MACRF3,&MACRF4,&MACRF5  
         GBLB  &MACRF6,&MACRF7,&MACRF8,&MACRF9,&MACRFA,&MACRFB,&MACRFC  
         GBLB  &MACRFD,&MACRFE,&MACRFF,&MAC5,&MAC6,&MAC7,&FXSW,&RECFM0  
         GBLB  &RECFM1,&RECFM2,&RECFM3,&RECFM4,&RECFM5,&RECFM6,&RECFM7  
         GBLB  &OPTCD0,&OPTCD1,&OPTCD2,&OPTCD3,&OPTCD4,&OPTCD6,&OPTCD7  
         GBLB  &COMSW,&MAC4,&DSORG8,&OPTCD5                        RER1 
         GBLB  &DSORG9,&DSORGA                                          
         LCLA  &A1,&A3                                                  
         LCLC  &C0,&C1,&C2                                              
.*                                                                      
.* ******************************************************************** 
.*                                                                      
.* CHANGE ACTIVITY =                                                    
.*                                                                      
.*C(035510),A(035515),C(035520),A(035525),C(035870)            @ZA44457 
.*A(035875),C(035880),A(035895),C(035940)                      @ZA44457 
.*C(059100)                                                    @ZA53118 
.*$01=OY11903,HDP2230,,NSDWDC: ERROR WITH DSORG=PO RECFM=VBM       @01A 
.*$L1=DCBE,JDZ1110,920903,SJPLREB: SUPPORT FOR DCBE                @L1A 
.*$L2=XTIOT,HDZ1C1C0,090511,SJPLJAL: REMOVE XTIOT CHANGES          @L2A 
.*                                                                      
.* ******************************************************************** 
&MACRF0  SETB  0                       ZERO SWITCHES                    
&MACRF1  SETB  0                                                        
&MACRF2  SETB  0                                                        
&MACRF3  SETB  0                                                        
&MACRF4  SETB  0                                                        
&MACRF5  SETB  0                                                        
&MACRF6  SETB  0                                                        
&MACRF7  SETB  0                                                        
&MACRF8  SETB  0                                                        
&MACRF9  SETB  0                                                        
&MACRFA  SETB  0                                                        
&MACRFB  SETB  0                                                        
&MACRFC  SETB  0                                                        
&MACRFD  SETB  0                                                        
&MACRFE  SETB  0                                                        
&MACRFF  SETB  0                                                        
&MAC4    SETB  0                                                        
&MAC5    SETB  0                                                        
&MAC6    SETB  0                                                        
&MAC7    SETB  0                                                        
&FXSW    SETB  0                                                        
&COMSW   SETB  0                                                        
.*                                                                      
.*                      ANALYZE DSORG OPERAND                           
.*                                                                      
&DSORG0  SETB  0                       ZERO SWITCHES                    
&DSORG1  SETB  0                                                        
&DSORG2  SETB  0                                                        
&DSORG3  SETB  0                                                        
&DSORG4  SETB  0                                                        
&DSORG5  SETB  0                                                        
&DSORG6  SETB  0                                                        
&DSORG7  SETB  0                                                        
&DSORG8  SETB  0                                                        
&DSORG9  SETB  0                        TEST TCAM                       
&DSORGA  SETB  0                        TEST TCAM                       
         AIF   (T'&DSORG NE 'O').DSORGB  IF DSORG CODED                 
         AIF   (T'&MACRF EQ 'O').DSORGA  IF MACRF NOT CODED             
         AIF   ('&MACRF(1)' EQ 'E').DSORGX  IF EXCP ACCESS              
.DSORGA  IHBERMAC 158                  DSORG OMITTED                    
         MEXIT                                                          
.*                                                                      
.*                 TEST VALIDITY OF DSORG                               
.DSORGB  AIF   (K'&DSORG LT 2).DSORGD  ALL VALID CODES ARE 2 OR 3       
&C0      SETC  '&DSORG'(1,2)           ISOLATE 1ST TWO CHAR OF CODE     
         AIF   ('&C0' EQ 'IS' OR '&C0' EQ 'PS' OR '&C0' EQ 'DA' OR '&C0*
               ' EQ 'CX' OR '&C0' EQ 'PO' OR '&C0' EQ 'GS').DSORGE      
.*                                                             @ZA02213 
         AIF   ('&C0' EQ 'TX' OR '&C0' EQ 'TQ').DSORGE    IF TCAM       
.DSORGD  IHBERMAC 159,,&C0             INVALID CODE FOR DSORG           
         MEXIT                                                          
.DSORGE  ANOP                                                           
&FXSW    SETB  1                                                        
&COMSW   SETB  1                                                        
         AIF   (K'&DSORG LT 3).DSORGG  IF NO QUALIFIER                  
&C1      SETC  '&DSORG'(3,1)           ISOLATE QUALIFIER                
         AIF   ('&C1' NE 'U').DSORGF   IF INVALID QUALIFIER             
&DSORG7  SETB  1                       SET QUALIFIER SWITCH             
         AGO   .DSORGG                 TO ANALYZE PRIMARY CODE          
.*                                                                      
.DSORGF  IHBERMAC 160,,&C1             INVALID DSORG QUALIFIER-IGNORED  
.*                                                                      
.DSORGG  ANOP                                                           
&DSORG0  SETB  ('&C0' EQ 'IS')         TEST INDEXED SEQUENTIAL          
&DSORG1  SETB  ('&C0' EQ 'PS')         TEST PHYSICAL SEQENTIAL          
&DSORG2  SETB  ('&C0' EQ 'DA')         TEST DIRECT ACCESS               
&DSORG3  SETB  ('&C0' EQ 'CX')         TEST COMM. LINE GROUP            
&DSORG6  SETB  ('&C0' EQ 'PO')         TEST PARTITIONED                 
&DSORG8  SETB  ('&C0' EQ 'GS')          TEST FOR GRAPHICS               
&DSORG9  SETB  ('&C0' EQ 'TX')                                          
&DSORGA  SETB  ('&C0' EQ 'TQ')                                          
.DSORGX  ANOP                                                           
.*--------------------------------------------------------------------- 
.*                                                                      
.*                      ANALYZE MACRF OPERAND                           
.*                                                                      
         AIF   (T'&MACRF NE 'O').MACRFA IF MACRF SPECIFIED              
         IHBERMAC 161                  MACRF NOT SPECIFIED-EXCP ASSUMED 
         AGO   .MACRFC                 TO EXCP ANALYSIS                 
.MACRFA  ANOP                                                           
&A3      SETA  0                       RESET SUBLIST ITEM INDEX         
.*                                                                      
.MACRFB  ANOP                          START OF LOOP ON ITEM INDEX      
&A3      SETA  &A3+1                   STEP ITEM INDEX                  
         AIF   (&A3 GT N'&MACRF).MACRFX  IF ALL ITEMS ANALYZED          
         AIF   (K'&MACRF(&A3) EQ 0).MACRFB IF SUBLIST ITEM NULL         
&C0      SETC  '&MACRF(&A3)'           ISOLATE SUBLIST ITEM             
&A1      SETA  1                       INITIALIZE QUALIFIER POINTER     
&C1      SETC  '&C0'(1,1)              ISOLATE MAJOR MACRO TYPE CODE    
         AIF   ('&C1' NE 'E').MACRFD   TEST EXCP                        
         AIF   (N'&MACRF EQ 1).MACRFC  EXCP MUST STAND ALONE            
         IHBERMAC 170,,&MACRF          MACRF INVALID-EXCP ASSUMED       
.MACRFC  ANOP                                                           
&MACRF0  SETB  1                       SET EXCP SWITCH                  
&MACRF1  SETB  (T'&BFTEK NE 'O' OR T'&BFALN NE 'O' OR '&EODAD' NE '1' O*
               R T'&RECFM NE 'O' OR '&EXLST' NE '0' OR '&BUFNO' NE '0' *
               OR '&BUFCB' NE '1' OR '&BUFL' NE '0' OR '&IOBAD' NE '1') 
&MACRF1  SETB  (&MACRF1 OR T'&DEVD NE 'O' OR &FXSW OR &COMSW)           
&MACRF1  SETB  (&MACRF1 OR T'&OPTCD NE 'O')                        RER1 
&MACRF1  SETB  (&MACRF1 OR T'&DCBE NE 'O')                         @L1A 
&FXSW    SETB  (&MACRF1)               EXTENSION REQUIRED               
&MACRF2  SETB  ('&EOEA' NE '0' OR '&PCIA' NE '0' OR '&SIOA' NE '0' OR '*
               &CENDA' NE '0' OR '&XENDA' NE '0' OR '&AERR' NE '0')     
&MACRF2  SETB  (&MACRF2 OR T'&OPTCD NE 'O')                        RER1 
.*                                                                      
&MACRF3  SETB  ('&BUFNO' NE '0' OR '&BUFCB' NE '1' OR '&BUFL' NE '0' OR*
                '&IOBAD' NE '1' OR &FXSW OR &COMSW)                     
&MACRF3  SETB  (&MACRF3 OR T'&DCBE NE 'O')                         @L1A 
&COMSW   SETB  (&MACRF3)                                                
&MACRF4  SETB  0                                                        
         AIF   ('&REPOS' NE 'Y').MACRFC6                                
&MACRF5  SETB  1                                                        
.MACRFC6 ANOP                                                           
&MACRF6  SETB  0                                                        
         AIF   ('&PGFX' EQ 'NO').MACRFC7                                
         AIF   ('&PGFX' EQ 'YES' AND '&SIOA' EQ '0').MACRFC7            
&MACRF6  SETB  1                        INDICATE USER PAGE FIX ROUTINE  
.MACRFC7 ANOP                                                           
&MACRF7  SETB  0                                                        
&MACRF8  SETB  0                                                        
&MACRF9  SETB  0                                                        
&MACRFA  SETB  0                                                        
&MACRFB  SETB  0                                                        
&MACRFC  SETB  0                                                        
&MACRFD  SETB  0                                                        
&MACRFE  SETB  0                                                        
&MACRFF  SETB  0                                                        
         AGO   .MACRFX                 SKIP TO RECFM ANALYSIS           
.*                                                                      
.MACRFD  AIF   (('&C1' EQ 'G' OR '&C1' EQ 'P') AND (&DSORG0 OR &DSORG1 *
               OR &DSORG3 OR &DSORG9 OR &DSORGA)).MACRGA  IF VALID G/P  
.*                                                             @ZA02213 
         AIF   (('&C1' EQ 'R' OR '&C1' EQ 'W') AND (&DSORG0 OR &DSORG1 *
               OR &DSORG2 OR &DSORG3 OR &DSORG6 OR &DSORG8 OR &DSORG9 O*
               R &DSORGA)).MACRFE IF VALID R/W                          
         AIF   ('&C1' EQ 'S' AND &DSORG0).MACRSA  IF VALID S            
         IHBERMAC 162,,&C0,&DSORG      MACRF INVALID WITH DSORG         
         AGO   .MACRFB                 TO ADVANCE TO NEXT ELEMENT       
.*                                                                      
.*  MACRF=R/W ANALYSIS FOR BDAM, BPAM, BSAM, BISAM, BTAM, AND GRAPHICS  
.*                                                                      
.MACRFE  ANOP                          SET BIT FOR MAJOR MACRO TYPE     
&MACRF2  SETB  ('&C1' EQ 'R' OR &MACRF2) READ                           
&MACRFA  SETB  ('&C1' EQ 'W' OR &MACRFA) WRITE                          
.*                                                                      
         AIF   (K'&MACRF(&A3) GT 1).MACRFT  TO START R/W QUALIFIER LOOP 
         AIF   ((&DSORG0 AND '&C1' EQ 'R') OR &DSORG1 OR &DSORG3 OR    *
               &DSORG6 OR &DSORG8 OR &DSORG9).MACRFB                    
&C2      SETC  ''                      SET NULL FOR ERROR MESSAGE       
.*                                                                      
.MACRFS  IHBERMAC 163,,&C2,&C1,&DSORG  INVALID QUALIFIER FOR R/W MACRF  
.*                                                                      
.MACRFT  ANOP                          LOOP ON MACRF ITEM QUALIFIERS    
&A1      SETA  &A1+1                   STEP QUALIFIER INDEX             
         AIF   (K'&MACRF(&A3) LT &A1).MACRFB  IF NO MORE QUALIFIERS     
&C2      SETC  '&C0'(&A1,1)            ISOLATE QUALIFIER                
.*                                                                      
         AIF   ('&C2' NE 'K').MACRFI   IF NOT KEY REFERENCE             
         AIF   (NOT &DSORG2).MACRFS    VALID FOR DA                     
&MACRF3  SETB  (&MACRF3 OR '&C1' EQ 'R')  IF K QUALIFIES READ           
&MACRFB  SETB  (&MACRFB OR '&C1' EQ 'W')  IF K QUALIFIES WRITE          
         AGO   .MACRFT                 TO  BEGIN NEW LOOP               
.*                                                                      
.MACRFI  AIF   ('&C2' NE 'L').MACRFJ   IF NOT BSAM LOAD BDAM MODE       
         AIF   (NOT(&DSORG1 AND '&C1' EQ 'W')).MACRFS  VALID PS WRITE   
         AIF   ('&DSORG' EQ 'PS').MACRFIB YES-BDAM CREATE      @ZA40451 
         AIF   ('&DSORG' EQ 'PSU').MACRFIB YES-BDAM CREATE     @ZA44457 
         AGO   .MACRFIY                DSORG NOT EQUAL TO      @ZA44457 
.*                                     PS OR TO PSU, NOT VALID @ZA44457 
.*                                     WITH MACRF=WL.          @ZA44457 
.* **********************                                      @ZA40451 
.MACRFIB ANOP                                                  @ZA40451 
         AIF   ('&A3' NE '1' AND '&C2' EQ 'L').MACRFIW         @ZA40451 
.*                                     ONLY ONE 'L' ALLOWED.   @ZA40451 
.*                                                             @ZA40451 
&MACRFC  SETB  1                       L QUALIFIES WRITE                
.*                                                             @ZA40451 
.MACRFIC ANOP                                                  @ZA40451 
.*                                                             @ZA40451 
&A1      SETA  &A1+1                   STEP QUALIFIER INDEX    @ZA40451 
         AIF   (K'&MACRF(&A3) LT &A1).MACRFIX EXIT-IF NO MORE  @ZA40451 
.*                                            QUALIFIERS.      @ZA40451 
&C2      SETC  '&C0'(&A1,1)            ISOLATE QUALIFIER.      @ZA40451 
         IHBERMAC 163,,&C2,&MACRF,&DSORG ANY OTHER QUALIFIERS  @ZA40451 
.*                                     NOT VALID WITH MACRF=WL @ZA40451 
.*                                     IGNORE THEM.            @ZA40451 
         AGO   .MACRFIB                CHECK FOR MORE          @ZA40451 
.*                                     QUALIFIERS.             @ZA40451 
.* ***********************                                     @ZA40451 
.MACRFIX ANOP                          CHECK, MORE OPERANDS.   @ZA40451 
&A3      SETA  &A3+1                   STEP ITEM INDEX.        @ZA40451 
         AIF   (&A3 GT N'&MACRF).MACRFX EXIT IF ALL ITEMS      @ZA40451 
.*                                      ANALYZED.              @ZA40451 
         AIF   (K'&MACRF(&A3) EQ 0).MACRFX EXIT IF SUBLIST     @ZA40451 
.*                                         IS NULL.            @ZA40451 
.* *********************                                       @ZA40451 
.*                                     IF SUBLIST IS NOT NULL  @ZA40451 
.*                                     IGNORE MSG WILL BE      @ZA40451 
.*                                     ISSUED.                 @ZA40451 
&C0      SETC  '&MACRF(&A3)'           GET SUBLIST ITEM.       @ZA40451 
&A1      SETA  0                       INITIALIZE QUALIFIER    @ZA40451 
.*                                     POINTER.                @ZA40451 
&C1      SETC  '&C0'(1,1)              GET MAJOR OPERAND       @ZA40451 
.*                                     TYPE CODE.              @ZA40451 
         AIF   ('&C1' EQ 'L' AND '&A3' EQ '2').MACRFIX         @ZA44457 
.*                                     AVOID UNECESSARY MSG.   @ZA44457 
.*                                     &A3 IS POINTING AT THE  @ZA44457 
.*                                     L OF WL IN MACRF.       @ZA44457 
         IHBERMAC 171                  MSG-INCONSISTENT        @ZA40451 
.*                                     OPERAND-FOR SECOND      @ZA40451 
.*                                     OPERAND.                @ZA40451 
         AGO   .MACRFIC                CHECK FOR QUALIFIERS.   @ZA44457 
.* *********************                                       @ZA40451 
.MACRFIY ANOP                                                  @ZA40451 
         IHBERMAC 162,,&C2,&DSORG     'L' OF MACRF INVALID     @ZA40451 
.*                                     IF DSORG NOT PS OR PSU. @ZA44457 
         MEXIT                                                 @ZA40451 
.* *********************                                       @ZA40451 
.MACRFIW ANOP                                                  @ZA40451 
         IHBERMAC 171                 MSG=INCONSISTENT         @ZA40451 
.*                                        OPERAND.             @ZA40451 
         IHBERMAC 162,,&C2,&DSORG     'L' OF MACRF INVALID     @ZA40451 
.*                                    IF NOT FIRST 'L'.        @ZA40451 
         AGO   .MACRFIC                                        @ZA40451 
.* *********************                                       @ZA40451 
.*                                                             @ZA40451 
.*                                                                      
.MACRFJ  AIF   ('&C2' NE 'I').MACRFK   IF NOT ID REFERENCE              
         AIF   (NOT &DSORG2).MACRFS    VALID FOR DA                     
&MACRF4  SETB  (&MACRF4 OR '&C1' EQ 'R') I QUALIFIES READ               
&MACRFC  SETB  (&MACRFC OR '&C1' EQ 'W') I QUALIFIES WRITE              
         AGO   .MACRFT                 TO BEGIN NEW LOOP                
.*                                                                      
.MACRFK  AIF   ('&C2' NE 'P').MACRFM   IF NOT NOTE/POINT                
         AIF   (NOT &DSORG1).MACRFS    VALID FOR PS                     
&MACRF5  SETB  (&MACRF5 OR '&C1' EQ 'R') IF P QUALIFIES READ            
&MACRFD  SETB  (&MACRFD OR '&C1' EQ 'W') IF P QUALIFIES WRITE           
         AGO   .MACRFT                 TO BEGIN NEW LOOP                
.*                                                                      
.MACRFM  AIF   ('&C2' NE 'S').MACRFN   IF NOT DYNAMIC BUFFERING         
         AIF   (NOT (&DSORG0 OR &DSORG2)).MACRFS  VALID FOR IS,DA       
         AIF   ('&C1' NE 'R').MACRFS   VALID FOR READ ONLY              
&MACRF5  SETB  1                       S QUALIFIES READ                 
         AGO   .MACRFT                 TO BEGIN NEW LOOP                
.*                                                                      
.MACRFN  AIF   ('&C2' NE 'C').MACRFO   IF NOT CNTRL OR CHECK            
         AIF   (&DSORG0 OR &DSORG2).MACRF6  IF IS,DA CHECK              
         AIF   (NOT(&DSORG1 OR &DSORG8)).MACRFS  VALID FOR PS, GS       
&MACRF6  SETB  (&MACRF6 OR '&C1' EQ 'R')  IF C QUALIFIES READ  CNTRL    
&MACRFE  SETB  (&MACRFE OR '&C1' EQ 'W')  IF C QUALIFIES WRITE CNTRL    
         AGO   .MACRFT                 TO BEGIN NEW LOOP                
.MACRF6  ANOP                          VALID IS,DA CHECK                
&MACRF6  SETB  (&MACRF6 OR &DSORG0)    IF C QUALIFIES IS R/W CHECK      
&MACRF7  SETB  (&MACRF7 OR &DSORG2)    IF C QUALIFIES DA R/W CHECK      
         AGO   .MACRFT                 TO BEGIN NEW LOOP                
.*                                                                      
.MACRFO  AIF   ('&C2' NE 'X').MACRFP   IF NOT EXCLUSIVE LOGIC           
         AIF   (NOT(&DSORG2 AND '&C1' EQ 'R')).MACRFS VALID FOR DA READ 
&MACRF6  SETB  1                       X QUALIFIES READ EXCLUSIVE       
         AGO   .MACRFT                 TO BEGIN NEW LOOP                
.*                                                                      
.MACRFP  AIF   ('&C2' NE 'U').MACRFQ   IF NOT UPDATE MODE               
         AIF   (NOT &DSORG0).MACRFS    VALID IF IS                      
&MAC4    SETB  (&MAC4 OR '&C1' EQ 'R') IF U QUALIFIES READ              
&MAC5    SETB  (&MAC5 OR '&C1' EQ 'W') IF U QUALIFIES WRITE             
         AGO   .MACRFT                 TO BEGIN NEW LOOP                
.*                                                                      
.MACRFQ  AIF   ('&C2' NE 'A').MACRFS   IF NOT ADD MODE                  
         AIF   (NOT (&DSORG0 OR &DSORG2) OR '&C1' NE 'W').MACRFS  IS-DA 
&MACRFE  SETB  (&MACRFE OR &DSORG2)    IF DA WRITE                      
&MAC6    SETB  (&MAC6 OR &DSORG0)      IF IS WRITE                      
         AGO   .MACRFT                 TO BEGIN NEW LOOP                
.*                                                                      
.*  MACRF=G/P ANALYSIS FOR QSAM, QISAM, AND QTAM                        
.*                                                                      
.MACRGA  ANOP                          SET BIT FOR MAJOR MACRO TYPE     
&MACRF1  SETB  ('&C1' EQ 'G' OR &MACRF1) GET                            
&MACRF9  SETB  ('&C1' EQ 'P' OR &MACRF9) PUT                            
.*                                                                      
         AIF   (K'&MACRF(&A3) GT 1).MACRGE  TO START G/P QUALIFIER LOOP 
         AIF   (&DSORG3 OR &DSORGA).MACRFB                     @ZA02213 
         AIF   (&DSORG9).MACRFB                                         
&C2      SETC  ''                      SET NULL FOR ERROR MESSAGE       
.*                                                                      
.MACRGB  IHBERMAC 163,,&C2,&C1,&DSORG  INVALID QUALIFIER FOR G/P MACRF  
.*                                                                      
.MACRGE  ANOP                          LOOP ON MACRF ITEM QUALIFIERS    
&A1      SETA  &A1+1                   STEP QUALIFIER INDEX             
         AIF   (K'&MACRF(&A3) LT &A1).MACRFB  IF NO MORE QUALIFIERS     
&C2      SETC  '&C0'(&A1,1)            ISOLATE QUALIFIER                
.*                                                                      
         AIF   ('&C2' NE 'C').MACRGD   IF NOT CNTRL                     
         AIF   (NOT &DSORG1).MACRGB    VALID FOR PS                     
&MACRF6  SETB  (&MACRF6 OR '&C1' EQ 'G')  IF C QUALIFIES GET            
&MACRFE  SETB  (&MACRFE OR '&C1' EQ 'P')  IF C QUALIFIES PUT            
         AGO   .MACRGE                 TO BEGIN NEW LOOP                
.*                                                                      
.MACRGD  AIF   ('&C2' NE 'D').MACRGL   IF NOT MOVE DATA MODE            
         AIF   (NOT &DSORG1).MACRGB    VALID FOR PS                     
&MACRF3  SETB  (&MACRF3 OR '&C1' EQ 'G')  IF D IMPLIES M FOR GET        
&MACRF7  SETB  (&MACRF7 OR '&C1' EQ 'G')  IF D QUALIFIES GET            
&MACRFB  SETB  (&MACRFB OR '&C1' EQ 'P')  IF D IMPLIES M FOR PUT        
&MACRFF  SETB  (&MACRFF OR '&C1' EQ 'P')  IF D QUALIFIES PUT            
         AGO   .MACRGE                 TO BEGIN NEW LOOP                
.*                                                                      
.MACRGL  AIF   ('&C2' NE 'L').MACRGM   IF NOT LOCATE MODE               
         AIF   (NOT(&DSORG0 OR &DSORG1)).MACRGB  VALID FOR IS-PS        
&MACRF4  SETB  (&MACRF4 OR '&C1' EQ 'G')  IF L QUALIFIES GET            
&MACRFC  SETB  (&MACRFC OR '&C1' EQ 'P')  IF L QUALIFIES PUT            
         AGO   .MACRGE                 TO BEGIN NEW LOOP                
.*                                                                      
.MACRGM  AIF   ('&C2' NE 'M').MACRGT   IF NOT MOVE MODE                 
         AIF   (NOT(&DSORG0 OR &DSORG1)).MACRGB  VALID FOR IS-PS        
&MACRF3  SETB  (&MACRF3 OR '&C1' EQ 'G')  IF M QUALIFIES GET            
&MACRFB  SETB  (&MACRFB OR '&C1' EQ 'P')  IF M QUALIFIES PUT            
         AGO   .MACRGE                 TO BEGIN NEW LOOP                
.*                                                                      
.MACRGT  AIF   ('&C2' NE 'T').MACRGU   IF NOT SUBSTITUTE MODE           
         AIF   (NOT &DSORG1).MACRGB    VALID FOR PS                     
&MACRF5  SETB  (&MACRF5 OR '&C1' EQ 'G')  IF T QUALIFIES GET            
&MACRFD  SETB  (&MACRFD OR '&C1' EQ 'P')  IF T QUALIFIES PUT            
         AGO   .MACRGE                 TO BEGIN NEW LOOP                
.*                                                                      
.MACRGU  AIF   ('&C2' NE 'U').MACRGB   IF NOT UPDATE MODE               
         AIF   (NOT(&DSORG0 AND '&C1' EQ 'P')).MACRGB  VALID FOR IS PUT 
&MACRFD  SETB  1                       U QUALIFIES PUT                  
         AGO   .MACRGE                 TO BEGIN NEW LOOP                
.*                                                                      
.*  MACRF=S ANYALYSIS FOR QISAM                                         
.*                                                                      
.MACRSA  ANOP                          SET BIT FOR MAJOR MACRO TYPE     
&MACRF8  SETB  1                       SETL                             
         AIF   (K'&MACRF(&A3) EQ 1).MACRFB  IF NO QUALIFIER             
&C2      SETC  '&C0'(2,1)              ISOLATE QUALIFIER                
         AIF   (NOT('&C2' EQ 'I' OR '&C2' EQ 'K')).MACRSB  VALID IF I K 
&MACRFE  SETB  (&MACRFE OR '&C2' EQ 'K')  IF K QUALIFIES SETL           
&MACRFF  SETB  (&MACRFF OR '&C2' EQ 'I')  IF I QUALIFIES SETL           
         AIF   (K'&MACRF(&A3) EQ 2).MACRFB  RETURN IF 1 QUALIFIER       
&C2      SETC  '&C0'(3,8)              ISOLATE EXTRA QUALIFIERS         
.MACRSB  IHBERMAC 163,,&C2,S,&DSORG    INVALID QUALIFIER FOR S MACRF    
         AGO   .MACRFB                 RETURN TO BEGIN NEW LOOP         
.*                                                                      
.MACRFX  AIF   (NOT &DSORG6).MACRFY    IF NOT PO                        
&MACRF5  SETB  (&MACRF5 OR &MACRF2)    IF PO, READ  IMPLIES NOTE/POINT  
&MACRFD  SETB  (&MACRFD OR &MACRFA)    IF PO, WRITE IMPLIES NOTE/POINT  
.MACRFY  AIF   (&DSORG8).OPTCDX        IF GRAPHICS, SKIP RECFM + OPTCD  
.*--------------------------------------------------------------------- 
.*                                                                      
.*                      ANALYZE RECFM                                   
.*                                                                      
&RECFM0  SETB  0                                                        
&RECFM1  SETB  0                       ZERO SWITCHES                    
&RECFM2  SETB  0                                                        
&RECFM3  SETB  0                                                        
&RECFM4  SETB  0                                                        
&RECFM5  SETB  0                                                        
&RECFM6  SETB  0                                                        
&RECFM7  SETB  0                                                        
.*                                                                      
&A1      SETA  K'&RECFM                SET NUMBER OF ELEMENTS           
.RECFMA  AIF   (&A1 EQ 0).RECFMX       LOOP RETURN-IF 0, ALL DONE       
&C0      SETC  '&RECFM'(&A1,1)         ISOLATE ELEMENT                  
         AIF   ('&C0' NE 'F').RECFMB   IF NOT FIXED LENGTH              
         AIF   (NOT (&DSORG0 OR &DSORG1 OR &DSORG2 OR &DSORG6 OR &MACRF*
               0)).RECFML              IF VALID CONTEXT                 
&RECFM0  SETB  1                                                        
         AGO   .RECFMK                 TO START NEW LOOP                
.RECFMB  AIF   ('&C0' NE 'V').RECFMC   IF NOT VARIABLE LENGTH           
         AIF   (NOT (&DSORG0 OR &DSORG1 OR &DSORG2 OR &DSORG6 OR &MACRF*
               0)).RECFML              IF VALID CONTEXT                 
&RECFM1  SETB  1                                                        
         AGO   .RECFMK                 TO START NEW LOOP                
.RECFMC  AIF   ('&C0' NE 'U').RECFMD   IF NOT UNKNOWN FORMAT            
         AIF   (NOT (&DSORG1 OR &DSORG2 OR &DSORG6 OR &MACRF0)).RECFML  
&RECFM0  SETB  1                                                        
&RECFM1  SETB  1                                                        
         AGO   .RECFMK                 TO START NEW LOOP                
.RECFMD  AIF   ('&C0' NE 'D').RECFMD1   IF NOT D-FORMAT          S20038 
         AIF   (NOT &DSORG1).RECFML                              S20038 
&RECFM2  SETB  1                        D-FORMAT                 S20038 
         AGO   .RECFMK       TO START NEW LOOP                   S20038 
.RECFMD1 AIF   ('&C0' NE 'B').RECFME    IF NOT BLOCKED RECORDS   S20038 
         AIF   (NOT (&DSORG0 OR &DSORG1 OR &DSORG2 OR &DSORG6 OR &MACRFX
               0)).RECFML          GOTO MNOTE                           
&RECFM3  SETB  1                                                        
         AGO   .RECFMK                 TO START NEW LOOP                
.RECFME  AIF   ('&C0' NE 'S').RECFMF   IF NOT STANDARD RECORDS          
         AIF   (NOT (&DSORG1 OR &DSORG2 OR &DSORG6 OR &MACRF0)).RECFML  
.*                                                             @ZA02213 
&RECFM4  SETB  1                                                        
         AGO   .RECFMK                 TO START NEW LOOP                
.RECFMF  AIF   ('&C0' NE 'A' AND '&C0' NE 'G').RECFMG                   
         AIF   ('&C0' EQ 'A' AND NOT (&DSORG1 OR &DSORG6 OR &MACRF0)).R*
               ECFML                                           @ZA53118 
         AIF   ('&C0' EQ 'G' AND NOT (&MACRF0)).RECFML         @ZA02213 
&RECFM5  SETB  1                                                        
         AGO   .RECFMK                 TO START NEW LOOP                
.RECFMG  AIF   ('&C0' NE 'M' AND '&C0' NE 'R').RECFMI                   
         AIF   ('&C0' EQ 'M' AND NOT (&DSORG1 OR &DSORG6 OR &MACRF0)).R*
               ECFML                                               @01C 
         AIF   ('&C0' EQ 'R' AND NOT (&MACRF0)).RECFML         @ZA02213 
&RECFM6  SETB  1                                                        
         AGO   .RECFMK                                                  
.RECFMI  AIF   ('&C0' NE 'T').RECFMJ   IF NOT TRACK OVERFLOW            
         AIF   (NOT (&DSORG0 OR &DSORG1 OR &DSORG2 OR &DSORG6 OR &MACRF*
               0)).RECFML                                               
&RECFM2  SETB  1                                                        
         AGO   .RECFMK                 TO  START NEW LOOP               
.RECFML  IHBERMAC 164,,&C0,&DSORG      RECFM INVALID WITH DSORG         
         AGO   .RECFMK                 TO START NEW LOOP                
.RECFMJ  IHBERMAC 148,RECFM,&C0        INVALID CODE FOR RECFM IGNORED   
.RECFMK  ANOP                                                           
&A1      SETA  &A1-1                   STEP ELEMENT INDEX               
         AGO   .RECFMA                 TO REPEAT LOOP                   
.RECFMX  ANOP                                                           
.*--------------------------------------------------------------------- 
.*                                                                      
.*                      ANALYZE OPTCD                                   
.*                                                                      
&OPTCD0  SETB  0                       ZERO SWITCHES                    
&OPTCD1  SETB  0                                                        
&OPTCD2  SETB  0                                                        
&OPTCD3  SETB  0                                                        
&OPTCD4  SETB  0                                                        
&OPTCD5  SETB  0                                                   RER1 
&OPTCD6  SETB  0                                                        
&OPTCD7  SETB  0                                                        
.*                                                                      
&A1      SETA  K'&OPTCD                SET ELEMENT INDEX                
         AIF  (T'&OPTCD EQ 'O' AND (&DSORG9 OR &DSORGA)).OPTCDX ZA07595 
.OPTCDB  AIF   (&A1 LE 0).OPTCDX       LOOP RETURN-IF 0, ALL DONE       
&C0      SETC  '&OPTCD'(&A1,1)         ISOLATE ELEMENT                  
         AIF   (&DSORG3).OPTCDJ                                @ZA02213 
         AIF   ('&C0' NE 'W').OPTCDA   IF NOT WRITE CHECK               
         AIF   (NOT (&DSORG0 OR &DSORG1 OR &DSORG2 OR &DSORG6)).OPTCDJ  
&OPTCD0  SETB  1                                                        
         AGO   .OPTCDI                 TO START NEW LOOP                
.OPTCDA  AIF   ('&C0' NE 'U').OPTCDC                                    
         AIF   (NOT (&DSORG0 OR &DSORG1)).OPTCDJ                        
&OPTCD1  SETB  1                                                        
         AGO   .OPTCDI                 TO START NEW LOOP                
.OPTCDC  AIF   ('&C0' NE 'C' AND '&C0' NE 'M' AND '&C0' NE 'E').OPTCDD  
         AIF   ('&C0' EQ 'M' AND NOT &DSORG0).OPTCDJ                    
         AIF   ('&C0' EQ 'E' AND NOT &DSORG2).OPTCDJ                    
         AIF   ('&C0' EQ 'C' AND NOT (&DSORG1 OR &DSORG6 OR &DSORGA)).O*
               PTCDJ                                                    
&OPTCD2  SETB  1                                                        
         AGO   .OPTCDI                 TO START NEW LOOP                
.OPTCDD  AIF   ('&C0' NE 'I' AND '&C0' NE 'F' AND '&C0' NE 'O' AND '&C0X
               ' NE 'H').OPTCDE                                         
         AIF   ('&C0' EQ 'I' AND NOT (&DSORG0 OR                       *
               &DSORGA)).OPTCDJ                                  ICB447 
         AIF   ('&C0' EQ 'F' AND NOT &DSORG2).OPTCDJ                    
         AIF   (('&C0' EQ 'O' OR '&C0' EQ 'H') AND NOT &DSORG1).OPTCDJ  
         AIF   ('&C0' EQ 'O' AND NOT &MACRF1).OPTCDL                    
         AIF   ('&C0' EQ 'H' AND NOT &MACRF2).OPTCDL                    
&OPTCD3  SETB  1                                                        
         AGO   .OPTCDI                 TO START NEW LOOP                
.OPTCDE  AIF   ('&C0' NE 'Y' AND '&C0' NE 'A' AND '&C0' NE 'Q').OPTCDF  
         AIF   ('&C0' EQ 'Y' AND NOT &DSORG0).OPTCDJ                    
         AIF   ('&C0' EQ 'A' AND NOT &DSORG2).OPTCDJ                    
         AIF   ('&C0' EQ 'Q' AND (NOT &DSORG1)).OPTCDJ                  
&OPTCD4  SETB  1                                                        
         AGO   .OPTCDI                 TO START NEW LOOP                
.OPTCDF  AIF   ('&C0' NE 'R').OPTCDG                                    
         AIF   (NOT (&DSORG0 OR &DSORG2 OR &DSORGA)).OPTCDJ             
&OPTCD7  SETB  1                                                        
         AGO   .OPTCDI                 TO START NEW LOOP                
.OPTCDG  AIF   ('&C0' NE 'J').OPTCDM    BR IF NOT OPTCD J      @Z40MSKC 
         AIF   (NOT &DSORG1).OPTCDJ     BR IF NOT PHYSICAL SEQ @Z40MSKC 
&OPTCD7  SETB  1                        SET BIT FOR OPTCD=J    @Z40MSKC 
         AGO   .OPTCDI                                         @Z40MSKC 
.OPTCDM  AIF   ('&C0' NE 'L' AND '&C0' NE 'T').OPTCDK                   
         AIF   (NOT (&DSORG0 OR &DSORG1 OR &DSORGA)).OPTCDJ             
&OPTCD6  SETB  1                                                        
         AGO   .OPTCDI                 TO START NEW LOOP                
.OPTCDK  AIF   ('&C0' NE 'Z').OPTCDH                               RER1 
         AIF   (NOT (&DSORG1 OR &MACRF0)).OPTCDJ                   RER1 
&OPTCD5  SETB  1                                                   RER1 
         AGO   .OPTCDI    TO START NEW LOOP.                       RER1 
.OPTCDJ  IHBERMAC 165,,&C0,&DSORG      OPTCD INVALID WITH DSORG         
         AGO   .OPTCDI                 TO START NEW LOOP                
.OPTCDL  IHBERMAC 171                                                   
         AGO   .OPTCDI                                                  
.OPTCDLH ANOP                                                           
&C0      SETC  '&OPTCD'                                                 
.OPTCDH  IHBERMAC 149,OPTCD,&C0        INVALID CODE FOR OPTCD IGNORED   
.*                                                                      
.OPTCDI  ANOP                                                           
&A1      SETA  &A1-1                   STEP ELEMENT INDEX               
         AGO   .OPTCDB                 TO REPEAT LOOP                   
.OPTCDX  ANOP                                                           
.*--------------------------------------------------------------------- 
.*                                                                      
.*                      ANALYZE NEGATIVE ORIGIN OFFSET                  
.*                                                                      
&A0      SETA  0                       ZERO ACCUMULATOR                 
         AIF   ('&DSORG8' EQ '1').ORG3X   GRAPHICS                      
         AIF   (NOT (&MACRF0 AND NOT (&MACRF1 OR &MACRF3))).ORG1        
&A0      SETA  40                      SET NEGATIVE OFFSET              
         MEXIT                         RETURN TO OUTER MACRO            
.*                                                                      
.*                 DEVICE DEPENDENT INTERFACE                           
.*                                                                      
.ORG1    AIF   (T'&DCBE NE 'O').ORG3W  DCBE SPECIFIED, LEAVE &A0=0 @L1A 
         AIF   (T'&DEVD EQ 'O').ORG10                                   
         AIF   ('&DEVD' EQ 'LD').ORG10                         @ZA21103 
         AIF   ('&DEVD' EQ 'BS').ORG1A                                  
         AGO   .ORG1B         IF DEVD CODED                             
.ORG10   AIF   (&DSORG1 OR &DSORG6).ORG3W  IF PS OR PO                  
         AIF   (NOT &MACRF0).ORG1A     IF NOT EXCP                      
&A0      SETA  4                       SET NEGATIVE OFFSET FOR DA       
         AGO   .ORG3W                  SKIP TO FINISH                   
.ORG1A   ANOP                                                           
&A0      SETA  16                      SET NEGATIVE OFFSET FOR IS/DA    
         AIF   (&DSORG0 OR &DSORG2).ORG3W IF IS OR DA SKIP TO FINISH    
&A0      SETA  20                      OTHERWISE NO DEVD INTERFACE      
         AGO   .ORG3W                  SKIP TO FINISH                   
.*                                                                      
.ORG1B   AIF   (K'&DEVD NE 2).ORG1G     IF INVALID LENGTH               
         AIF   ('&DEVD' EQ 'WT').ORG1H                                  
         AIF   (NOT (&DSORG1 OR &DSORG6 OR &MACRF0)).ORG1G INVALID CODE 
         AIF   ('&DEVD' NE 'DA').ORG1C IF NOT DA INTERFACE              
         AIF   (&DSORG1 OR &DSORG6).ORG3W  IF PS OR PO                  
&A0      SETA  4                       SET NEGATIVE OFFSET              
         AGO   .ORG3W                  SKIP TO FINISH                   
.*                                                                      
.ORG1C   AIF   (NOT (&DSORG1 OR &MACRF0)).ORG1G  INVALID CODE           
         AIF   ('&DEVD' NE 'TA').ORG1D IF NOT MAGNETIC TAPE INTERFACE   
         AIF   (&DSORG1).ORG3W         IF PS                            
&A0      SETA  4                       SET NEGATIVE OFFSET              
         AGO   .ORG3W                  SKIP TO FINISH                   
.*                                                                      
.ORG1D   AIF   ('&DEVD' NE 'PT').ORG1E IF NOT PAPER TAPE INTERFACE      
&A0      SETA  8                       SET NEGATIVE OFFSET              
         AGO   .ORG3W                  SKIP TO FINISH                   
.*                                                                      
.ORG1E   AIF   ('&DEVD' NE 'RD' AND '&DEVD' NE 'PC' AND '&DEVD' NE 'PR'*
               ).ORG1F                 IF NOT UNIT RECORD INTERFACE     
&A0      SETA  16                      SET NEGATIVE OFFSET              
         AGO   .ORG3W                  SKIP TO FINISH                   
.ORG1F   AIF   ('&DEVD' EQ 'OR' AND &DSORG1).ORG3W                      
.*                                                                      
         AIF   ('&DEVD' EQ 'MR' AND &DSORG1).ORG3W                      
.ORG1G   IHBERMAC 166,,&DEVD,&DSORG                                     
         AGO   .ORG3W                                                   
.ORG1H   ANOP                                                           
&A0      SETA  16                                                       
.ORG3W   AIF   (NOT &MACRF0).ORG3X                                      
&MACRFC  SETB  (&A0 EQ 0)                                               
&MACRFD  SETB  (&A0 EQ 4)                                               
&MACRFE  SETB  (&A0 EQ 8)                                               
&MACRFF  SETB  (&A0 EQ 16)                                              
.ORG3X   MEND                                                           
         END                                                            

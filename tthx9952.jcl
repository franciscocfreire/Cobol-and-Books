//TTHX9952 JOB 'RDAB,4120,PR32','B430134',MSGCLASS=Z,REGION=7M,         
//       SCHENV=TEST                                                    
//OUT1     OUTPUT JESDS=ALL,DEST=ANYLOCAL                               
//OUTE     OUTPUT DEST=ANYLOCAL,CLASS=E                                 
//OUTF     OUTPUT DEST=ANYLOCAL,CLASS=F                                 
//OUTG     OUTPUT DEST=ANYLOCAL,CLASS=G                                 
//OUTJ     OUTPUT DEST=ANYLOCAL,CLASS=J                                 
//OUTM     OUTPUT DEST=ANYLOCAL,CLASS=M                                 
//OUTN     OUTPUT DEST=ALNJE01,CLASS=N                                  
//OUTO     OUTPUT DEST=ANYLOCAL,CLASS=O                                 
//OUTP     OUTPUT DEST=ALNJE01,CLASS=P                                  
//OUTQ     OUTPUT DEST=ALNJE01,CLASS=Q                                  
//OUTR     OUTPUT DEST=ALNJE01,CLASS=R                                  
//OUTRR    OUTPUT DEST=ANYLOCAL,CLASS=R                                 
//OUTV     OUTPUT DEST=ANYLOCAL,CLASS=V                                 
//OUTW     OUTPUT DEST=ALNJE02,CLASS=W                                  
//OUT5     OUTPUT DEST=ALNJE02,CLASS=5                                  
//OUTZ     OUTPUT DEST=ANYLOCAL,CLASS=Z                                 
//*                                                                     
//* ***    %%LIBSYM MJ.EVTA.PRV.CAFORM %%MEMSYM OUTLKSO                 
//* ***    %%LIBSYM MJ.EVTA.PRV.CAFORM %%MEMSYM OUTLKHX                 
//* ***    %%LIBSYM MJ.EVTA.PRV.CAFORM %%MEMSYM OUTLKHY                 
//*                                                                     
//JOBLIB   DD DSN=MX.BIBGERTT,DISP=SHR                                  
//         DD DSN=MX.BIBGERAL,DISP=SHR                                  
//         DD DSN=MJ.BIBGERAL,DISP=SHR                                  
//         DD DSN=DB2M1.R2.DSNLOAD,DISP=SHR                             
//         DD DSN=SYS1.CEE.SCEERUN,DISP=SHR                             
//STEP0    EXEC SORTD                                                   
//*                                                                     
//* ***    CLASSIFICA ARQUIVO BDRPTRAN                                  
//* ***    SEPARANDO POR: LOCATION (FIS/JUR)                            
//*                                                                     
//SORTIN   DD DSN=MX.CACS.DB280.BDRPTRAN(0),                            
//       DISP=SHR                                                       
//SAIDA1   DD DSN=MX.TTHX.BDRPTRAN.CARTEJUR(+1),                        
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(001000,0200),RLSE),                                
//       DCB=(MX.A,LRECL=0396,RECFM=FB)                                 
//SAIDA2   DD DSN=MX.TTHX.BDRPTRAN.CARTEFIS(+1),                        
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(001000,0200),RLSE),                                
//       DCB=(MX.A,LRECL=0396,RECFM=FB)                                 
//SYSIN    DD *                                                         
 SORT FIELDS=(04,06,A,10,18,A),FORMAT=BI                                
 OUTFIL INCLUDE=((04,6,CH,EQ,C'010101'),AND,                            
                 111,6,CH,EQ,C'000260'),FNAMES=SAIDA1                   
 OUTFIL INCLUDE=((04,6,CH,EQ,C'010102'),AND,                            
                 111,6,CH,EQ,C'000260'),FNAMES=SAIDA2                   
 END                                                                    
//SORTWK01 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00460))                                            
//SORTWK02 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00460))                                            
//SORTWK03 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00460))                                            
//SORTWK04 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00460))                                            
//SORTWK05 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00460))                                            
//SORTWK06 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00460))                                            
//SORTWK07 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00460))                                            
//SORTWK08 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00460))                                            
//SORTWK09 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00460))                                            
//SORTWK10 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00460))                                            
//SORTWK11 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00460))                                            
//SORTWK12 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00460))                                            
//SORTWK13 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00460))                                            
//SORTWK14 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00460))                                            
//SORTWK15 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00460))                                            
//SORTWK16 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00460))                                            
//SORTWK17 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00460))                                            
//SORTWK18 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00460))                                            
//SORTWK19 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00460))                                            
//SORTWK20 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00460))                                            
//*                                                                     
//STEP1    EXEC PGM=RDAB7480,                                           
//       COND=(0,NE)                                                    
//*                                                                     
//* ***    (PESSOA JURIDICA)                                            
//* ***    GERA EXTRACT ZERADO PARA QUE O CACS INATIVE A CONTA QUE      
//* ***    ESTA ERRADA.                                                 
//* ***    FAZ O BATE ENTRE O ARQUIVO BDRPTRAN (ERRO=000260)            
//* ***    E O ARQUIVO EXTRACT NOVO.                                    
//*                                                                     
//BDRPTRAN DD DSN=*.STEP0.SORT.SAIDA1,                                  
//       DISP=SHR                                                       
//ARQEXTRC DD DSN=MX.CACS.EXTRACT.SEMATU.GERAL.SEL(0),                  
//       DISP=SHR                                                       
//ARQSAIDA DD DSN=MX.TTHX.INATIVA.SEMATU.CARTEJUR(+1),                  
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO,08),                                               
//       SPACE=(TRK,(020000,4000),RLSE),                                
//       DCB=(MX.A,LRECL=3400,RECFM=FB,BUFNO=30)                        
//BDRPSCOR DD DSN=MX.TTHX.INATIVA.SEMATU.DESPCJUR(+1),                  
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO,08),                                               
//       SPACE=(TRK,(020000,4000),RLSE),                                
//       DCB=(MX.A,LRECL=0396,RECFM=FB,BUFNO=30)                        
//SYSOUT   DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=Y                                                  
//*                                                                     
//STEP2    EXEC PGM=RDAB7480,                                           
//       COND=(0,NE)                                                    
//*                                                                     
//* ***    (PESSOA FISICA)                                              
//* ***    GERA EXTRACT ZERADO PARA QUE O CACS INATIVE A CONTA QUE      
//* ***    ESTA ERRADA.                                                 
//* ***    FAZ O BATE ENTRE O ARQUIVO BDRPTRAN (ERRO=000260)            
//* ***    E O ARQUIVO EXTRACT NOVO.                                    
//*                                                                     
//BDRPTRAN DD DSN=*.STEP0.SORT.SAIDA2,                                  
//       DISP=SHR                                                       
//ARQEXTRC DD DSN=MX.CACS.EXTRACT.SEMATU.GERAL.SEL(0),                  
//       DISP=SHR                                                       
//ARQSAIDA DD DSN=MX.TTHX.INATIVA.SEMATU.CARTEFIS(+1),                  
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO,08),                                               
//       SPACE=(TRK,(020000,4000),RLSE),                                
//       DCB=(MX.A,LRECL=3400,RECFM=FB,BUFNO=30)                        
//BDRPSCOR DD DSN=MX.TTHX.INATIVA.SEMATU.CARTAFIS(+1),                  
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO,08),                                               
//       SPACE=(TRK,(020000,4000),RLSE),                                
//       DCB=(MX.A,LRECL=0396,RECFM=FB,BUFNO=30)                        
//SYSOUT   DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=Y                                                  
//*                                                                     
//STEP3    EXEC SORTD                                                   
//*                                                                     
//* ***    CLASSIFICA ARQUIVO REP360WK - RELATÓRIO VGO4                 
//* ***    POR: LOCATION / CONTA CACS                                   
//* ***    SELECIONANDO LOCATION - CARTÃO E ERRO 260                    
//*                                                                     
//SORTIN   DD DSN=MX.CACS.DB280.REP360WK(0),                            
//       DISP=SHR                                                       
//SORTOUT  DD DSN=MX.TTHX.CARTE.REP360WK.COMPARA(+1),                   
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO,40),                                               
//       SPACE=(TRK,(020000,4000),RLSE),                                
//       DCB=(MX.A,LRECL=24,RECFM=FB)                                   
//SORTLIB  DD DSN=SYS1.SORTLIB,                                         
//       DISP=SHR                                                       
//SORTWK01 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00500))                                            
//SORTWK02 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00500))                                            
//SORTWK03 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00500))                                            
//SORTWK04 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00500))                                            
//SORTWK05 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00500))                                            
//SORTWK06 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00500))                                            
//SORTWK07 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00500))                                            
//SORTWK08 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00500))                                            
//SORTWK09 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00500))                                            
//SORTWK10 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00500))                                            
//SORTWK11 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00500))                                            
//SORTWK12 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00500))                                            
//SORTWK13 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00500))                                            
//SORTWK14 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00500))                                            
//SORTWK15 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00500))                                            
//SORTWK16 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00500))                                            
//SORTWK17 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00500))                                            
//SORTWK18 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00500))                                            
//SYSIN    DD *                                                         
 OPTION VLSCMP                                                          
 SORT FIELDS=(08,06,A,14,18,A),FORMAT=BI                                
 INCLUDE COND=((08,6,CH,EQ,C'010101',OR,08,6,CH,EQ,C'010102'),AND,      
                 115,6,CH,EQ,C'000260')                                 
 OUTFIL OUTREC=(08,06,14,18),VTOF                                       
 END                                                                    
//*                                                                     
//STEP4    EXEC SORTD                                                   
//*                                                                     
//* ***    CLASSIFICA ARQUIVO EXTRACT PARA O CACS                       
//* ***    POR: LOCATION / CONTA CACS                                   
//* ***    REMOVE DUPLICADOS                                            
//*                                                                     
//SORTIN   DD DSN=*.STEP1.ARQSAIDA,                                     
//       DISP=SHR                                                       
//         DD DSN=*.STEP2.ARQSAIDA,                                     
//       DISP=SHR                                                       
//SORTOUT  DD DSN=MX.TTHX.EXTRACT.CARTE.COMPARA(+1),                    
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO,40),                                               
//       SPACE=(TRK,(020000,4000),RLSE),                                
//       DCB=(MX.A,LRECL=24,RECFM=FB)                                   
//SORTLIB  DD DSN=SYS1.SORTLIB,                                         
//       DISP=SHR                                                       
//SORTWK01 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00500))                                            
//SORTWK02 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00500))                                            
//SORTWK03 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00500))                                            
//SORTWK04 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00500))                                            
//SORTWK05 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00500))                                            
//SORTWK06 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00500))                                            
//SORTWK07 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00500))                                            
//SORTWK08 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00500))                                            
//SORTWK09 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00500))                                            
//SORTWK10 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00500))                                            
//SORTWK11 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00500))                                            
//SORTWK12 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00500))                                            
//SORTWK13 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00500))                                            
//SORTWK14 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00500))                                            
//SORTWK15 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00500))                                            
//SORTWK16 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00500))                                            
//SORTWK17 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00500))                                            
//SORTWK18 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00500))                                            
//SYSIN    DD *                                                         
 SORT FIELDS=(16,06,A,22,18,A),FORMAT=BI                                
 OUTREC FIELDS=(16,06,22,18)                                            
 END                                                                    
//*                                                                     
//STEP5    EXEC PGM=CVDT0150,                                           
//       PARM='001001'                                                  
//ARQTESTE DD DSN=*.STEP3.SORT.SORTOUT,                                 
//       DISP=SHR                                                       
//ARQPROD  DD DSN=*.STEP4.SORT.SORTOUT,                                 
//       DISP=SHR                                                       
//DIVERGE  DD DSN=MX.TTHX.CARTE.DIVERGE(+1),                            
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(010,002),RLSE),                                    
//       DCB=(MX.A,LRECL=024,RECFM=FB)                                  
//PRODSEM  DD DSN=MX.TTHX.CARTE.PRODSEM(+1),                            
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO,02),                                               
//       SPACE=(TRK,(001990,0398),RLSE),                                
//       DCB=(MX.A,LRECL=24,RECFM=FB)                                   
//TESTSEM  DD DSN=MX.TTHX.CARTE.TESTSEM(+1),                            
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(010,002),RLSE),                                    
//       DCB=(MX.A,LRECL=24,RECFM=FB)                                   
//SYSOUT   DD SYSOUT=T                                                  
//SYSUDUMP DD SYSOUT=Y                                                  
//*                                                                     
//STEP6    EXEC PGM=ICETOOL                                             
//IN       DD DSN=*.STEP5.DIVERGE,                                      
//       DISP=SHR                                                       
//LISTA    DD SYSOUT=%%FORM#LKSO,                                       
//       DCB=(LRECL=133,RECFM=FBA)                                      
//DFSMSG   DD SYSOUT=T                                                  
//TOOLMSG  DD SYSOUT=T                                                  
//TOOLIN   DD *                                                         
 DISPLAY FROM(IN) LIST(LISTA) -                                         
 BLANK -                                                                
 BETWEEN(1) -                                                           
 TITLE('DIVERGENCIAS ENTRE ARQUIVOS') -                                 
 DATE(DM4/)   TIME   PAGE -                                             
 HEADER('LOCATION')      ON(1,6,CH) -                                   
 HEADER('CONTA CACS')    ON(7,18,CH)                                    
//SYSOUT   DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=Y                                                  
//*                                                                     
//STEP7    EXEC PGM=ICETOOL                                             
//IN       DD DSN=*.STEP5.PRODSEM,                                      
//       DISP=SHR                                                       
//LISTA    DD SYSOUT=%%FORM#LKHX,                                       
//       DCB=(LRECL=133,RECFM=FBA)                                      
//DFSMSG   DD SYSOUT=T                                                  
//TOOLMSG  DD SYSOUT=T                                                  
//TOOLIN   DD *                                                         
 DISPLAY FROM(IN) LIST(LISTA) -                                         
 BLANK -                                                                
 BETWEEN(1) -                                                           
 TITLE('EXTRACT SEM CORRESPONDENTES') -                                 
 DATE(DM4/)   TIME   PAGE -                                             
 HEADER('LOCATION')      ON(1,6,CH) -                                   
 HEADER('CONTA CACS')    ON(7,18,CH)                                    
//SYSOUT   DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=Y                                                  
//*                                                                     
//STEP8    EXEC PGM=ICETOOL                                             
//IN       DD DSN=*.STEP5.TESTSEM,                                      
//       DISP=SHR                                                       
//LISTA    DD SYSOUT=%%FORM#LKHY,                                       
//       DCB=(LRECL=133,RECFM=FBA)                                      
//DFSMSG   DD SYSOUT=T                                                  
//TOOLMSG  DD SYSOUT=T                                                  
//TOOLIN   DD *                                                         
 DISPLAY FROM(IN) LIST(LISTA) -                                         
 BLANK -                                                                
 BETWEEN(1) -                                                           
 TITLE('RELATORIO VGO4 SEM CORRESPONDENTES') -                          
 DATE(DM4/)   TIME   PAGE -                                             
 HEADER('LOCATION')      ON(1,6,CH) -                                   
 HEADER('CONTA CACS')    ON(7,18,CH)                                    
//SYSOUT   DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=Y                                                  
//*                                                                     

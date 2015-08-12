//TTHX9954 JOB 'RDAB,4120,PR32','A299565',MSGCLASS=Z,REGION=7M,         
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
//*                                                                     
//* ***    DEPENDE DO UNLOAD DA TABELA RDAB005 - (RDAB)                 
//* ***    DEPENDE DO UNLOAD DA TABELA RDAB006 - (RDAB)                 
//*                                                                     
//STEP0    EXEC SORTD                                                   
//*                                                                     
//* ***    CLASSIFICA POR BCO,CTA,ORIGEM,TIPO E SEQUENCIA TELEFONE      
//*                                                                     
//SORTIN   DD DSN=MX.RDAB.CACS.TELEFONE,                                
//       DISP=SHR                                                       
//SORTOUT  DD DSN=MX.TTHX.TELEFONE.CLAS(+1),                            
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(020000,4000),RLSE),                                
//       DCB=(MX.A,LRECL=0044,RECFM=FB)                                 
//SYSIN    DD *                                                         
 SORT FIELDS=(1,2,A,3,3,A,6,7,A,32,1,D,40,4,D,37,2,D,34,2,D),           
             FORMAT=BI                                                  
 END                                                                    
//*                                                                     
//STEP1    EXEC PGM=RDAB7130                                            
//*                                                                     
//* ***    MATCH - BASE UNICA ENDEREÇOS C/ BASE UNICA TELEFONES         
//*                                                                     
//ARQTELEL DD DSN=*.STEP0.SORT.SORTOUT,                                 
//       DISP=SHR                                                       
//ARQLOGRA DD DSN=MX.RDAB.CACS.ENDERECO,                                
//       DISP=SHR                                                       
//ARQENTEL DD DSN=MX.TTHX.ENDFONE(+1),                                  
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(020000,4000),RLSE),                                
//       DCB=(MX.A,LRECL=358,RECFM=FB)                                  
//SYSOUT   DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=Y                                                  
//*                                                                     
//STEP5    EXEC PGM=CVDT0150,                                           
//       PARM='001001'                                                  
//ARQTESTE DD DSN=*.STEP1.ARQENTEL,                                     
//       DISP=SHR                                                       
//ARQPROD  DD DSN=MX.CACS.ENDFONE,                                      
//       DISP=SHR                                                       
//DIVERGE  DD DSN=MX.TTHX.CARTE.DIVERGE(+1),                            
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO),                                                  
//       SPACE=(TRK,(010,002),RLSE),                                    
//       DCB=(MX.A,LRECL=358,RECFM=FB)                                  
//PRODSEM  DD DSN=MX.TTHX.CARTE.PRODSEM(+1),                            
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO),                                                  
//       SPACE=(TRK,(010,002),RLSE),                                    
//       DCB=(MX.A,LRECL=358,RECFM=FB)                                  
//TESTSEM  DD DSN=MX.TTHX.CARTE.TESTSEM(+1),                            
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO),                                                  
//       SPACE=(TRK,(010,002),RLSE),                                    
//       DCB=(MX.A,LRECL=358,RECFM=FB)                                  
//SYSOUT   DD SYSOUT=T                                                  
//SYSUDUMP DD SYSOUT=Y                                                  
//*                                                                     
//STEP6    EXEC PGM=ICETOOL                                             
//IN       DD DSN=*.STEP5.DIVERGE,                                      
//       DISP=SHR                                                       
//LISTA    DD SYSOUT=%%FORM#MAOZ,                                       
//       DCB=(LRECL=133,RECFM=FBA)                                      
//DFSMSG   DD SYSOUT=T                                                  
//TOOLMSG  DD SYSOUT=T                                                  
//TOOLIN   DD *                                                         
 DISPLAY FROM(IN) LIST(LISTA) -                                         
 BLANK -                                                                
 BETWEEN(1) -                                                           
 TITLE('DIVERGENCIAS ENTRE ARQUIVOS') -                                 
 DATE(DM4/)   TIME   PAGE -                                             
 HEADER('BCO')           ON(1,2,PD) -                                   
 HEADER('AG')            ON(3,3,PD) -                                   
 HEADER('CONTA')         ON(6,7,PD) -                                   
 HEADER('M1')            ON(253,1,CH) -                                 
 HEADER('M1')            ON(269,1,CH) -                                 
 HEADER('DDD1')          ON(254,4,CH) -                                 
 HEADER('TELEFONE 1')    ON(258,11,CH) -                                
 HEADER('M2')            ON(270,1,CH) -                                 
 HEADER('M2')            ON(286,1,CH) -                                 
 HEADER('DDD2')          ON(271,4,CH) -                                 
 HEADER('TELEFONE 2')    ON(275,11,CH)-                                 
 HEADER('M3')            ON(287,1,CH) -                                 
 HEADER('M3')            ON(303,1,CH) -                                 
 HEADER('DDD3')          ON(288,4,CH) -                                 
 HEADER('TELEFONE 3')    ON(292,11,CH) -                                
 HEADER('M4')            ON(304,1,CH) -                                 
 HEADER('M4')            ON(320,1,CH) -                                 
 HEADER('DDD4')          ON(305,4,CH) -                                 
 HEADER('TELEFONE 4')    ON(309,11,CH)-                                 
 HEADER('M5')            ON(321,1,CH) -                                 
 HEADER('M5')            ON(337,1,CH) -                                 
 HEADER('DDD5')          ON(322,4,CH) -                                 
 HEADER('TELEFONE 5')    ON(326,11,CH)-                                 
 HEADER('M6')            ON(338,1,CH) -                                 
 HEADER('M6')            ON(354,1,CH) -                                 
 HEADER('DDD6')          ON(339,4,CH) -                                 
 HEADER('TELEFONE 6')    ON(343,11,CH)                                  
//SYSOUT   DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=Y                                                  
//*                                                                     
//STEP7    EXEC PGM=ICETOOL                                             
//IN       DD DSN=*.STEP5.PRODSEM,                                      
//       DISP=SHR                                                       
//LISTA    DD SYSOUT=%%FORM#MAPA,                                       
//       DCB=(LRECL=133,RECFM=FBA)                                      
//DFSMSG   DD SYSOUT=T                                                  
//TOOLMSG  DD SYSOUT=T                                                  
//TOOLIN   DD *                                                         
 DISPLAY FROM(IN) LIST(LISTA) -                                         
 BLANK -                                                                
 BETWEEN(1) -                                                           
 TITLE('PRODUCAO SEM CORRESPONDENTE') -                                 
 DATE(DM4/)   TIME   PAGE -                                             
 HEADER('BCO')           ON(1,2,PD) -                                   
 HEADER('AG')            ON(3,3,PD) -                                   
 HEADER('CONTA')         ON(6,7,PD) -                                   
 HEADER('M1')            ON(253,1,CH) -                                 
 HEADER('M1')            ON(269,1,CH) -                                 
 HEADER('DDD1')          ON(254,4,CH) -                                 
 HEADER('TELEFONE 1')    ON(258,11,CH) -                                
 HEADER('M2')            ON(270,1,CH) -                                 
 HEADER('M2')            ON(286,1,CH) -                                 
 HEADER('DDD2')          ON(271,4,CH) -                                 
 HEADER('TELEFONE 2')    ON(275,11,CH)-                                 
 HEADER('M3')            ON(287,1,CH) -                                 
 HEADER('M3')            ON(303,1,CH) -                                 
 HEADER('DDD3')          ON(288,4,CH) -                                 
 HEADER('TELEFONE 3')    ON(292,11,CH) -                                
 HEADER('M4')            ON(304,1,CH) -                                 
 HEADER('M4')            ON(320,1,CH) -                                 
 HEADER('DDD4')          ON(305,4,CH) -                                 
 HEADER('TELEFONE 4')    ON(309,11,CH)-                                 
 HEADER('M5')            ON(321,1,CH) -                                 
 HEADER('M5')            ON(337,1,CH) -                                 
 HEADER('DDD5')          ON(322,4,CH) -                                 
 HEADER('TELEFONE 5')    ON(326,11,CH)-                                 
 HEADER('M6')            ON(338,1,CH) -                                 
 HEADER('M6')            ON(354,1,CH) -                                 
 HEADER('DDD6')          ON(339,4,CH) -                                 
 HEADER('TELEFONE 6')    ON(343,11,CH)                                  
//SYSOUT   DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=Y                                                  
//*                                                                     
//STEP8    EXEC PGM=ICETOOL                                             
//IN       DD DSN=*.STEP5.TESTSEM,                                      
//       DISP=SHR                                                       
//LISTA    DD SYSOUT=%%FORM#MAPB,                                       
//       DCB=(LRECL=133,RECFM=FBA)                                      
//DFSMSG   DD SYSOUT=T                                                  
//TOOLMSG  DD SYSOUT=T                                                  
//TOOLIN   DD *                                                         
 DISPLAY FROM(IN) LIST(LISTA) -                                         
 BLANK -                                                                
 BETWEEN(1) -                                                           
 TITLE('TESTE SEM CORRESPONDENTE') -                                    
 DATE(DM4/)   TIME   PAGE -                                             
 HEADER('BCO')           ON(1,2,PD) -                                   
 HEADER('AG')            ON(3,3,PD) -                                   
 HEADER('CONTA')         ON(6,7,PD) -                                   
 HEADER('M1')            ON(253,1,CH) -                                 
 HEADER('M1')            ON(269,1,CH) -                                 
 HEADER('DDD1')          ON(254,4,CH) -                                 
 HEADER('TELEFONE 1')    ON(258,11,CH) -                                
 HEADER('M2')            ON(270,1,CH) -                                 
 HEADER('M2')            ON(286,1,CH) -                                 
 HEADER('DDD2')          ON(271,4,CH) -                                 
 HEADER('TELEFONE 2')    ON(275,11,CH)-                                 
 HEADER('M3')            ON(287,1,CH) -                                 
 HEADER('M3')            ON(303,1,CH) -                                 
 HEADER('DDD3')          ON(288,4,CH) -                                 
 HEADER('TELEFONE 3')    ON(292,11,CH) -                                
 HEADER('M4')            ON(304,1,CH) -                                 
 HEADER('M4')            ON(320,1,CH) -                                 
 HEADER('DDD4')          ON(305,4,CH) -                                 
 HEADER('TELEFONE 4')    ON(309,11,CH)-                                 
 HEADER('M5')            ON(321,1,CH) -                                 
 HEADER('M5')            ON(337,1,CH) -                                 
 HEADER('DDD5')          ON(322,4,CH) -                                 
 HEADER('TELEFONE 5')    ON(326,11,CH)-                                 
 HEADER('M6')            ON(338,1,CH) -                                 
 HEADER('M6')            ON(354,1,CH) -                                 
 HEADER('DDD6')          ON(339,4,CH) -                                 
 HEADER('TELEFONE 6')    ON(343,11,CH)                                  
//SYSOUT   DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=Y                                                  
//*                                                                     
//STEP9    EXEC PGM=ICETOOL                                             
//IN       DD DSN=*.STEP0.SORT.SORTOUT,                                 
//       DISP=SHR                                                       
//LISTA    DD SYSOUT=%%FORM#MAPC,                                       
//       DCB=(LRECL=133,RECFM=FBA)                                      
//DFSMSG   DD SYSOUT=T                                                  
//TOOLMSG  DD SYSOUT=T                                                  
//TOOLIN   DD *                                                         
 DISPLAY FROM(IN) LIST(LISTA) -                                         
 BLANK -                                                                
 BETWEEN(1) -                                                           
 TITLE('TELEFONES NA BASE UNICA') -                                     
 DATE(DM4/)   TIME   PAGE -                                             
 HEADER('BCO')           ON(1,2,PD) -                                   
 HEADER('AG')            ON(3,3,PD) -                                   
 HEADER('CONTA')         ON(6,7,PD) -                                   
 HEADER('IND-INVALIDO')  ON(32,1,CH) -                                  
 HEADER('DDD')           ON(17,4,CH) -                                  
 HEADER('TELEFONE')      ON(21,11,CH)                                   
//SYSOUT   DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=Y                                                  
//*                                                                     

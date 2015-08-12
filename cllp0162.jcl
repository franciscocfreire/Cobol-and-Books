//CLLP0162 JOB 'CLLP,4120,PR32','A172212',MSGCLASS=Z,REGION=4M,         
//       SCHENV=DB2                                                     
//*                                                                     
//STEP2    EXEC PGM=RDAB0005                                            
//*                                                                     
//* ***    OBTEM CPSSOA-CATDTR E DADOS DE PJ, QUANDO NAO ENCONTRADOS    
//* ***    ESTAO NAS TABS. C/BCO,AG E CTA                               
//*                                                                     
//ARQPARVV DD DSN=AD.BRQ.P006.SAICOMCR.IRESCLAS,
//       DISP=OLD
//PENDFICA DD DSN=AD.BRQ.P006.CADUV000,
//       DISP=OLD                                                       
//ARQDAPES DD DSN=AD.BRQ.P006.LOGRJUR5,
//       DISP=(,CATLG,DELETE),
//       UNIT=(DISCO,03),                                               
//       SPACE=(TRK,(005650,1130),RLSE),                                
//       DCB=(,LRECL=500,RECFM=FB)
//RELVAZIO DD SYSOUT=*
//RELTOTAL DD SYSOUT=*
//RELNENCO DD SYSOUT=*
//SYSOUT   DD SYSOUT=*                                                  
//SYSPRINT DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=Y                                                  
//*

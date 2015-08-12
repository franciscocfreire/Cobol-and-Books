//CLLP0170 JOB 'CLLP,4120,PR32','A172212',MSGCLASS=Z,REGION=4M          
//JOBLIB   DD DSN=AV.BIBGERAL,DISP=SHR                                  
//         DD DSN=AV.BIBGERTT,DISP=SHR                                  
//         DD DSN=DB2A2.R2.DSNLOAD,DISP=SHR                             
//         DD DSN=SYS1.CEE.SCEERUN,DISP=SHR                             
//*                                                                     
//*                                                                     
//STEP2    EXEC PGM=RDAB0150                                            
//*                                                                     
//* ***    GERA ARQUIVO P/CARGA DAS TABELAS DA BASE UNICA               
//*                                                                     
//TABASUNI DD DSN=AD.BRQ.A006.BASEFICA.CLASS,
//       DISP=SHR                                                       
//ARQPESJD DD DSN=AD.BRQ.P006.PESSJURI.BASEFICA,
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(000050,0010),RLSE),                                
//       DCB=(,LRECL=081,RECFM=FB)
//ARQPESFS DD DSN=AD.BRQ.P006.PESSFISI.BASEFICA,
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO,02),                                               
//       SPACE=(TRK,(002300,0460),RLSE),                                
//       DCB=(,LRECL=270,RECFM=FB)
//ARQCTCOR DD DSN=AD.BRQ.P006.CTACORR.BASEFICA,
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(000870,0174),RLSE),                                
//       DCB=(,LRECL=098,RECFM=FB)
//ARQLOGDR DD DSN=AD.BRQ.P006.ARQLOGRD.BASEFICA,
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO,02),                                               
//       SPACE=(TRK,(001200,0240),RLSE),                                
//       DCB=(,LRECL=146,RECFM=FB)
//ARQFONES DD DSN=AD.BRQ.P006.ARQFONES.BASEFICA,
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(000310,0062),RLSE),                                
//       DCB=(,LRECL=044,RECFM=FB)
//REGTOTAL DD SYSOUT=(W,,AM19),                                         
//       DCB=(LRECL=0081,RECFM=FBA)                                     
//SYSOUT   DD SYSOUT=*                                                  
//SYSPRINT DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=Y                                                  
//*                                                                     

//RDABX060 JOB 'RDAB,4120,PR32','C085384',MSGCLASS=Z                    
//JOBLIB   DD DSN=MX.BIBGERAL,DISP=SHR                                  
//         DD DSN=DB2M1.R2.DSNLOAD,DISP=SHR                             
//         DD DSN=SYS1.CEE.SCEERUN,DISP=SHR                             
//STEP1    EXEC PGM=PLAN1010,                                           
//       PARM='SIM'                                                     
//*                                                                     
//* ***    RECEPCIONA ARQUIVO AL1 PARA UPDATE RDABB006                  
//*                                                                     
//SYSUT1   DD DSN=TM.CLLP.RDAB.CADURDAB.LOGRD.CORR,                     
//       DISP=(OLD,DELETE,KEEP)                                         
//SYSUT2   DD DSN=MX.RDAB.JX060S01.CADURDAB.CORR(+1),                   
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(030000,6000),RLSE),                                
//       DCB=(MX.A,LRECL=0146,RECFM=FB)                                 
//SYSOUT   DD SYSOUT=*                                                  
//PRINTER  DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=Y
//*
//STEP1A   EXEC PGM=PLAN1010,                                           
//       PARM='SIM'                                                     
//*                                                                     
//* ***    RECEPCIONA ARQUIVO AL1 PARA ATUALIZACAO PRV
//*                                                                     
//SYSUT1   DD DSN=TM.CLLP.RDAB.JX003S4B.HPURDABS.AGCTA,
//       DISP=(OLD,DELETE,KEEP)                                         
//SYSUT2   DD DSN=MX.RDAB.JX060S01.HPURDABS.AGCTA(+1),                   
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(030000,6000),RLSE),                                
//       DCB=(MX.A,LRECL=0320,RECFM=FB)                                 
//SYSOUT   DD SYSOUT=*                                                  
//PRINTER  DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=Y                                                   
//*                                                                     
//STEP2    EXEC PGM=RDAB0284                                            
//*                                                                     
//* ***    ATUALIZACAO DO RDABB006                                      
//*                                                                     
//ARQEND   DD DSN=*.STEP1.SYSUT2,                                       
//       DISP=OLD                                                       
//ARQUPDT  DD DSN=MX.RDAB.JX060S02.ARQUPDT(+1),                         
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO),                                                  
//       SPACE=(TRK,(001000,0200),RLSE),                                
//       DCB=(MX.A,LRECL=146,RECFM=FB)                                  
//SYSOUT   DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=Y                                                  
//*                                                                     
//STEP3    EXEC PGM=RDAB0278                                            
//*                                                                     
//* ***    ATUALIZACAO DO PRV ENDERECO
//*                                                                     
//ENDRCADU DD DSN=*.STEP1A.SYSUT2,                                       
//       DISP=OLD   
//ENDRRDAB DD DSN=*.STEP1.SYSUT2,                                       
//       DISP=OLD                                                      
//CADURDAB DD DSN=MX.RDAB.JX060S03.CADURDAB(+1),                         
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO),                                                  
//       SPACE=(TRK,(001000,0200),RLSE),                                
//       DCB=(MX.A,LRECL=146,RECFM=FB)
//DESPRDAB DD DSN=MX.RDAB.JX060S03.DESPRDAB(+1),                         
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO),                                                  
//       SPACE=(TRK,(001000,0200),RLSE),                                
//       DCB=(MX.A,LRECL=146,RECFM=FB)                                  
//SYSOUT   DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=Y                                                  
//*                      
//CLLP0175 JOB 'CLLP,4120,PR32','B013444',MSGCLASS=Z                    
//JOBLIB   DD DSN=AV.BIBGERTT,DISP=SHR                                  
//         DD DSN=AV.BIBGERAL,DISP=SHR                                  
//         DD DSN=AV.CHGM.LOADLIB.BSL.LOB,DISP=SHR                      
//         DD DSN=SA.RDG2.SREST.AL2.DCALOAD,DISP=SHR                    
//         DD DSN=DB2A2.R2.DSNLOAD,DISP=SHR                             
//         DD DSN=SYS1.CEE.SCEERUN,DISP=SHR                             
//*                                                                     
//STEP4    EXEC PGM=CLLP0175                                            
//*                                                                     
//* ***    CONSISTIR ARQUIVOS DE AGENCIAS OPERADORAS                    
//*                                                                     
//ARQENT01 DD DSN=AD.TEMP.CLLP.CLLP0175.SORT1,
//       DISP=SHR                                                       
//ARQENT02 DD DSN=AD.TEMP.CLLP.CLLP0175.SORT2,
//       DISP=SHR                                                       
//ARQEXP01 DD DSN=AD.TEMP.CLLP0175.ARQEXP01,                            
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO,50),                                               
//       SPACE=(TRK,(020000,4000),RLSE),                                
//       DCB=(,LRECL=0768,RECFM=FB)
//SYSOUT   DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=Y                                                  
//*                                                                     

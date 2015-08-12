//CLLP1402 JOB 'CLLP,4120,PR32','B610523',MSGCLASS=Z,SCHENV=DB2         
//OUT1     OUTPUT JESDS=ALL,DEST=ANYLOCAL                               
//OUT2     OUTPUT DEST=ANYLOCAL,CLASS=U                                 
//OUT3     OUTPUT DEST=ALNJE01,CLASS=P                                  
//JOBLIB   DD DSN=MX.BIBGERAL,DISP=SHR                                  
//         DD DSN=DB2M1.R2.DSNLOAD,DISP=SHR                             
//         DD DSN=SYS1.CEE.SCEERUN,DISP=SHR                             
//*                                                                     
//STEP1    EXEC MODMESU                                                 
//*                                                                     
//* ***    GERACAO DE ARQUIVO MESTRE DE AGENCIAS                        
//*                                                                     
//MODMESUS DD DSN=MX.CLLP.MESGEMAV(+1),                                 
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(000030,0006),RLSE),                                
//       DCB=(MX.A,LRECL=0080,RECFM=FB)                                 
//MESUPARM DD *                                                         
00080004020302025487005054002334014165003BBB008                         
//*                                                                     

//CLLPA404 JOB 'CLLP,4120,PR32','B706725',MSGCLASS=Z                    
//JOBLIB   DD DSN=MX.BIBGERAL,DISP=SHR                                  
//         DD DSN=SYS1.CEE.SCEERUN,DISP=SHR                             
//*                                                                     
//STEP1    EXEC SORTD                                                   
//*                                                                     
//* ***    ACUMULAR OS MOVIMENTOS DO MES                                
//*                                                                     
//SORTIN   DD DSN=MX.CLLP.PRV.AGENACU.LEI(0),                           
//       DISP=OLD                                                       
//         DD DSN=MX.CLLP.PRV.AGACUMES.LEI(0),                          
//       DISP=OLD                                                       
//SORTOUT  DD DSN=MX.CLLP.PRV.AGENACU.LEI(+1),                          
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO,03),                                               
//       SPACE=(TRK,(006160,1232),RLSE),                                
//       DCB=(MX.A,LRECL=0050,RECFM=FB)                                 
//SYSIN    DD *                                                         
 SORT FIELDS=(1,3,A,17,5,A),FORMAT=BI                                   
 END                                                                    
//*                                                                     
//STEP2    EXEC PGM=PLAN7220                                            
//*                                                                     
//* ***    ZERA O ARQUIVO MENSAL                                        
//*                                                                     
//ZERA001  DD DSN=MX.CLLP.PRV.AGACUMES.LEI(+1),                         
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(000140,0028),RLSE),                                
//       DCB=(MX.A,LRECL=050,RECFM=FB)                                  
//PRINTER  DD SYSOUT=*                                                  
//SYSOUT   DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=Y                                                  
//*                                                                     

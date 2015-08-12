//CLLP1035 JOB 'CLLP,4120,PR32','A299565',MSGCLASS=Z                    
//JOBLIB   DD DSN=AV.BIBGERTT,DISP=SHR                                  
//         DD DSN=AV.BIBGERAL,DISP=SHR                                  
//         DD DSN=AV.CHGM.LOADLIB.BSL.LOB,DISP=SHR                      
//         DD DSN=SA.RDG2.SREST.AL2.DCALOAD,DISP=SHR                    
//         DD DSN=DB2A2.R2.DSNLOAD,DISP=SHR                             
//         DD DSN=SYS1.CEE.SCEERUN,DISP=SHR                             
//STEP0E3  EXEC PGM=CLLP7658                                            
//*                                                                     
//* ***    RETIRAR MIGRADOS DO PROCESSAMENTO                            
//*                                                                     
//ARQSAIDA DD DSN=*.STEP0E1.SORT.SORTOUT,                               
//       DISP=SHR                                                       
//CONTMIGR DD DSN=AD.BRQ.P141.PRV.CONTRMIG.NEGATI(0),                   
//       DISP=SHR                                                       
//ARQSAIDS DD DSN=AD.BRQ.P141.J10350E3.ARQSAIDS(+1),                    
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(050000,20000),RLSE),                               
//       DCB=(AD.A,LRECL=0285,RECFM=FB)                                 
//ARQMIGR  DD DSN=AD.BRQ.P141.J10350E3.ARQMIGR(+1),                     
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(010000,2500),RLSE),                                
//       DCB=(AD.A,LRECL=0285,RECFM=FB)                                 
//RELCONS  DD SYSOUT=(W,,AM71),                                         
//       DCB=(LRECL=133,RECFM=FBA)                                      
//*#RELINCON DD SYSOUT=%%FORM#KIGL,                                     
//RELINCON DD DUMMY,                                                    
//       DCB=(LRECL=132,RECFM=FBA)                                      
//SYSOUT   DD SYSOUT=*                                                  
//*SYSUDUMP DD SYSOUT=Y                                                 
//*                                                                     

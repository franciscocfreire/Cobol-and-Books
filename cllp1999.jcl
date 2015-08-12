//CLLP1999 JOB 'CLLP,4120,PR32','A299565',MSGCLASS=Z                    
//JOBLIB   DD DSN=AV.BIBGERTT,DISP=SHR                                  
//         DD DSN=AV.BIBGERAL,DISP=SHR                                  
//         DD DSN=AV.CHGM.LOADLIB.BSL.LOB,DISP=SHR                      
//         DD DSN=SA.RDG2.SREST.AL2.DCALOAD,DISP=SHR                    
//         DD DSN=DB2A2.R2.DSNLOAD,DISP=SHR                             
//         DD DSN=SYS1.CEE.SCEERUN,DISP=SHR                             
//*                                                                     
//*==============================================================*      
//DELETA   EXEC PGM=IEFBR14                                             
//*==============================================================*      
//DD001   DD DSN=AD.TEMP.CHICAO.CADALPCL.COMP,                          
//         DISP=(MOD,DELETE),SPACE=(TRK,1)                              
//DD001   DD DSN=AD.BRQ.A144.CADATU.CLLP1999.TESTCLLP,                  
//         DISP=(MOD,DELETE),SPACE=(TRK,1)                              
//DD001   DD DSN=AD.BRQ.A144.CADACER.CLLP1999.TESTCLLP,                 
//         DISP=(MOD,DELETE),SPACE=(TRK,1)                              
//*                                                                     
//*STEP1B   EXEC PGM=BRAD0002
//*                                                                     
//* ***    COMPRIMI ARQUIVO DE ENTRADA                                  
//*                                                                     
//*ENTRA    DD DSN=*.STEP1A.SYSUT2,                                     
//*ENTRA    DD DSN=AD.TEMP.CHICAO.CADALPCL,
//*       DISP=SHR
//*SAI      DD DSN=AD.TEMP.CHICAO.CADALPCL.COMP,
//*       DISP=(,CATLG,DELETE),
//*       UNIT=(DISCO),
//*       SPACE=(CYL,(001333,0266),RLSE),
//*       DCB=(MX.A,LRECL=600,RECFM=FB)
//*SYSOUT   DD SYSOUT=Y
//*SYSABOUT DD SYSOUT=Y
//*SYSDUMP  DD SYSOUT=Y
//*SYSUDUMP DD SYSOUT=Y
//*                                                                     
//STEP1C   EXEC PGM=CLLP1999                                            
//*                                                                     
//* ***    ACERTA VALOR CONTABIL BRUTO                                  
//*                                                                     
//CADANT   DD DSN=AD.TEMP.CHICAO.CADALPCL,
//       DISP=OLD                                                       
//CADATU   DD DSN=AD.BRQ.A144.CADATU.CLLP1999.TESTCLLP,                 
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO,12),                                               
//       SPACE=(TRK,(020000,4000),RLSE),                                
//       DCB=(,BLKSIZE=32400,LRECL=0600,RECFM=FB)
//CADACER  DD DSN=AD.BRQ.A144.CADACER.CLLP1999.TESTCLLP,                
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(000010,0002),RLSE),                                
//       DCB=(,BLKSIZE=32400,LRECL=0600,RECFM=FB)
//RELATO   DD SYSOUT=(W,,AM71),                                         
//       DCB=(LRECL=0133,RECFM=FBM)                                     
//SYSOUT   DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=Y                                                  
//*                                                                     

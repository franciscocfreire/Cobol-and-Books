//CLLPA755 JOB 'CLLP,4120,PR32','A299565',MSGCLASS=Z,REGION=6144K       
//JOBLIB   DD DSN=MX.BIBGERAL,DISP=SHR                                  
//         DD DSN=DB2M1.R2.DSNLOAD,DISP=SHR                             
//         DD DSN=SA.RDG4.PKZIP.MZ1.LOADLIB,DISP=SHR                    
//         DD DSN=SYS1.CEE.SCEERUN,DISP=SHR                             
//STEP07   EXEC PGM=CLLPG121,                                           
//       PARM='CBRAD'                                                   
//*                                                                     
//* ***    LISTAGEM - TODAS AS CARTAS EMITIDAS                          
//*                                                                     
//LADO107  DD DSN=CORR1L1.CLLPA760,
//       DISP=SHR                                                       
//PARMCLLP DD DSN=PARM415,
//       DISP=SHR                                                       
//RELATO   DD DSN=MX.CLLP.PDSECAME.CLLPA755(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(050000,020000),RLSE),
//       DCB=(,LRECL=0244,RECFM=FBA)
//IMAGAVIS DD DUMMY,                                                    
//       DCB=(,LRECL=0116,RECFM=FBA)
//SYSOUT   DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=Y                                                  

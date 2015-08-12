//T7WP0043 JOB 'PEAT,4510,PR41','G00347',MSGCLASS=Z,SCHENV=TEST         
//JOBLIB   DD DSN=AV.BIBGERTT,DISP=SHR                                  
//         DD DSN=AV.BIBGERAL,DISP=SHR                                  
//         DD DSN=SYS1.CEE.SCEERUN,DISP=SHR                             
//STEP5    EXEC PGM=SORT
//*                                                                     
//* ***    CLASSIFICAR POR: AGE E CTA DE (ASCE)                         
//*                                                                     
//SORTIN   DD DSN=AD.C87.T7WP.UPDT.J0043S05.ARQSALDO,                   
//       DISP=OLD                                                       
//SORTOUT  DD DSN=AD.BRQ.TEMP.J0043S05.ARQSALDO,                        
//       DISP=(OLD,CATLG,DELETE),
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(0020000,08000),RLSE),                              
//       DCB=(,LRECL=0050,RECFM=FB)
//SYSIN    DD *                                                         
 SORT FIELDS=(1,7,A),FORMAT=BI                                          
 END                                                                    
//*                                                                     
//STEP5A   EXEC PGM=SORT
//*                                                                     
//* ***    CLASSIFICAR POR: AGE, CTA, DT-LCTO, CRED/DEB, VALOR          
//*                                                                     
//SORTIN   DD DSN=AD.C87.T7WP.UPDT.J0043S5A.MOVDIAR,                    
//       DISP=SHR                                                       
//SORTOUT  DD DSN=AD.BRQ.TEMP.J0043S5A.MOVDIAR.CLASS,                   
//       DISP=(OLD,CATLG,DELETE),
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(0020000,08000),RLSE),                              
//       DCB=(,LRECL=0102,RECFM=FB)
//SYSIN    DD *                                                         
 SORT FIELDS=(3,3,A,10,4,A,15,5,A,27,1,D,28,8,D),                       
         FORMAT=BI                                                      
 END                                                                    
//*                                                                     
//STEP6    EXEC PGM=PEAT1064                                            
//*                                                                     
//* ***    CARIMBA NO MOVIMENTO CONTA RAZAO 10.71                       
//*                                                                     
//MVTODIAE DD DSN=*.STEP5A.SORTOUT,
//       DISP=OLD                                                       
//ARQSALDO DD DSN=*.STEP5.SORTOUT,
//       DISP=OLD                                                       
//MVTODIAS DD DSN=AD.BRQ.CHICAO.MVTODIAS.J0043.AFTER4,                  
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(0020000,08000),RLSE),                              
//       DCB=(,LRECL=0102,RECFM=FB)
//SYSOUT   DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=Y                                                  
//*                                                                     

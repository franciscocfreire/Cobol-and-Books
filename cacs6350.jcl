//CACS6350 JOB 'CACS,4120,PR32','C089177',MSGCLASS=Z,SCHENV=DB2         
//JOBLIB   DD DSN=MX.BIBGERAL,DISP=SHR                                  
//         DD DSN=DB2M1.R2.DSNLOAD,DISP=SHR                             
//         DD DSN=SYS1.CEE.SCEERUN,DISP=SHR                             
//*                                                                     
//* ***    DEPENDE DO UNLOAD DA TABELA RDAB005 - (RDAB)                 
//* ***    DEPENDE DO UNLOAD DA TABELA RDAB006 - (RDAB)                 
//*                                                                     
//STEP01   EXEC PGM=RDAB7130
//*                                                                     
//* ***    MATCH - BASE UNICA ENDEREÇOS C/ BASE UNICA TELEFONES         
//*                                                                     
//*ARQTELEL DD DSN=AD.TEMP.CHICAO.TELEFONE,
//ARQTELEL DD DSN=AD.BRQ.CACS.TELEFONE.ORI2,
//       DISP=SHR                                                       
//*ARQLOGRA DD DSN=AD.TEMP.CHICAO.ENDERECO,
//ARQLOGRA DD DSN=AD.BRQ.CACS.ENDERECO.ORI2,
//       DISP=SHR
//ARQENTEL DD DSN=AD.TEMP.CHICAO.ENDFONE,
//       DISP=(OLD,CATLG,KEEP),
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(020000,4000),RLSE),                                
//       DCB=(,LRECL=358,RECFM=FB)
//SYSOUT   DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=Y                                                  
//*                                                                     

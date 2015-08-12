//CLLP0142 JOB 'CLLP,4120,PR32','A172212',MSGCLASS=Z,REGION=8M          
//JOBLIB   DD DSN=AV.BIBGERTT,DISP=SHR                                  
//         DD DSN=AV.BIBGERAL,DISP=SHR                                  
//         DD DSN=AV.CHGM.LOADLIB.BSL.LOB,DISP=SHR                      
//         DD DSN=SA.RDG2.SREST.AL2.DCALOAD,DISP=SHR                    
//         DD DSN=DB2A2.R2.DSNLOAD,DISP=SHR                             
//         DD DSN=SYS1.CEE.SCEERUN,DISP=SHR                             
//*                                                                     
//*STEP4    EXEC PGM=RDAB0885
//*                                                                     
//* ***    ATUALIZA O ARQUIVO COM DADOS COMPLEMENTARES                  
//* ***                                                                 
//*                                                                     
//*CADUV001 DD DSN=AD.BRQ.CADU.V001FAMI,
//*       DISP=OLD
//*CADUB067 DD DSN=AD.BRQ.CADU.CADUB067,
//*       DISP=OLD
//*ARQSAIDA DD DSN=AD.BRQ.P006.CADUV001,
//*       DISP=(,CATLG,DELETE),
//*       UNIT=(DISCO,03),
//*       SPACE=(TRK,(005650,1130),RLSE),
//*       DCB=(,LRECL=448,RECFM=FB)
//*SYSOUT   DD SYSOUT=*
//*SYSPRINT DD SYSOUT=*
//*SYSUDUMP DD SYSOUT=Y
//*                                                                     
//STEP7    EXEC PGM=RDAB0263
//*
//* ***    GERA ARQUIVO DE NOMES DO CADU + RDAB
//* ***
//*
//NOMERDAB DD DSN=AD.BRQ.CLLP.HPURDAB.CLASS,
//       DISP=OLD
//CADUV000 DD DSN=AD.BRQ.TEMP.CADUV000.JAGCTA,
//       DISP=OLD
//CADURDAB DD DSN=AD.BRQ.CLLP.CADURDAB.LOGRD,
//       DISP=(,CATLG,DELETE),
//       UNIT=(DISCO),
//       SPACE=(TRK,(001000,0200),RLSE),
//       DCB=(,LRECL=146,RECFM=FB)
//DESPRDAB DD DSN=AD.BRQ.CLLP.DESPRDAB.LOGRD,
//       DISP=(,CATLG,DELETE),
//       UNIT=(DISCO),
//       SPACE=(TRK,(001000,0200),RLSE),
//       DCB=(,LRECL=146,RECFM=FB)

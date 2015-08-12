//CLLP1401 JOB 'CLLP,4120,PR32','B430134',MSGCLASS=Z                    
//JOBLIB   DD DSN=MX.BIBGERAL,DISP=SHR                                  
//         DD DSN=DB2M1.R2.DSNLOAD,DISP=SHR                             
//         DD DSN=SYS1.CEE.SCEERUN,DISP=SHR                             
//*                                                                     
//* ***    GERACAO DE ARQUIVO COM PARAMETROS PARA EMISSAO DE AVISO      
//*                                                                     
//STEP2    EXEC PGM=PARM4000                                            
//*                                                                     
//* ***    **********************************************************   
//* ***    SELECIONA VIA PARM OS REG.A SEREM UTILIZADOS PELO REES0415   
//* ***    **********************************************************   
//*                                                                     
//ENTRADA  DD *                                                         
CLLP0002CLLP7615                                                        
//REGISTRO DD DSN=MX.CLLP.PAR415.LEI(+1),                               
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(000010,0002),RLSE),                                
//       DCB=(MX.A,LRECL=0255,RECFM=FB)                                 
//SYSOUT   DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=Y                                                  
//*                                                                     
//STEP2A   EXEC PGM=PLAN1010,                                           
//       PARM=SIM                                                       
//SYSUT1   DD DSN=*.STEP2.REGISTRO,                                     
//       DISP=SHR                                                       
//SYSUT2   DD DSN=MX.CLLP.PAR415(+2),                                   
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(000010,0002),RLSE),                                
//       DCB=(MX.A,LRECL=250,RECFM=FB)                                  
//PRINTER  DD SYSOUT=*                                                  
//SYSOUT   DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=Y                                                  
//*                                                                     
//STEP3    EXEC PGM=ATSOAC01,                                           
//       PARM='00-IKJEFT01',                                            
//       DYNAMNBR=20                                                    
//*                                                                     
//* ***    DESCARREGA A TABELA DB2PRD.REST_NEGATIVACAO (LPCLB037)       
//* ***    EM ARQUIVO SEQUENCIAL.                                       
//*                                                                     
//SYSTSPRT DD SYSOUT=*                                                  
//SYSPRINT DD SYSOUT=*                                                  
//LISTING  DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=Y                                                  
//SYSPUNCH DD DSN=MX.DBAS.LPCLB037.JA401(+1),                           
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(000010,0002),RLSE),                                
//       DCB=(MX.A)                                                     
//SYSREC00 DD DSN=MX.CLLP.LPCLB037.JA401(+1),                           
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(000730,0146),RLSE),                                
//       DCB=(MX.A)                                                     
//SYSTSIN  DD DSN=DB2M1.R2.SYSIN(DSNTIAUL),                             
//       DISP=SHR                                                       
//SYSIN    DD *                                                         
DB2PRD.REST_NEGATIVACAO                                                 
//SYSOUT   DD SYSOUT=*                                                  
//*                                                                     
//STEP4    EXEC SORTD                                                   
//*                                                                     
//* ***    CLASS.REST_NEGATIVACAO (SEQUENCIAL) POR AG/CONTA/NATUREZA    
//*                                                                     
//SORTIN   DD DSN=*.STEP3.SYSREC00,                                     
//       DISP=OLD                                                       
//SORTOUT  DD DSN=MX.CLLP.LPCLB037.JA401.CLAS(+1),                      
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(000550,0110),RLSE),                                
//       DCB=(MX.A,LRECL=0043,RECFM=FB)                                 
//SYSIN    DD *                                                         
 INCLUDE COND=(26,1,CH,EQ,C'X',AND,21,3,CH,NE,C'DCC')                   
 SORT FIELDS=(1,5,PD,A,6,3,PD,A,9,2,PD,A,11,1,PD,A,12,3,PD,A,           
              15,4,PD,A,19,2,PD,A,21,3,CH,A)                            
 END                                                                    
//*                                                                     

//CLLPX003 JOB 'CLLP,4120,PR32','C085384',MSGCLASS=Z,SCHENV=DB2         
//JOBLIB   DD DSN=AC.BIBGERAL,DISP=SHR                                  
//         DD DSN=DB2A1.R2.DSNLOAD,DISP=SHR                             
//         DD DSN=SYS1.CEE.SCEERUN,DISP=SHR                             
//STEP1    EXEC PGM=PLAN1010,                                           
//       PARM='SIM'                                                     
//*                                                                     
//* ***    RECEPCIONA ARQUIVO MZ1 HPU DA TABELA RDABB006                
//*                                                                     
//SYSUT1   DD DSN=TA.RDAB.CLLP.RDABB006.CORR,                           
//       DISP=(OLD,DELETE,KEEP)                                         
//SYSUT2   DD DSN=AC.CLLP.JX003S01.RDABB006.HPU(+1),                    
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(030000,6000),RLSE),                                
//       DCB=(AC.A,LRECL=0146,RECFM=FB)                                 
//SYSOUT   DD SYSOUT=*                                                  
//PRINTER  DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=Y                                                  
//*                                                                     
//STEP2    EXEC SORTD,                                                  
//       PARM='DYNALLOC=(DISCO,50),FILSZ=E100000000'                    
//*                                                                     
//* ***    RECEPCIONA ARQ. ACUM. DO MOVIMENTO RDAB CLASS. POR CPF/CNPJ  
//*                                                                     
//SORTIN   DD DSN=TA.RDAB.CLLP.CPFCNPJ.CORR,                            
//       DISP=(OLD,DELETE,KEEP)                                         
//SORTOUT  DD DSN=AC.CLLP.CPFCNPJ.CLASS.CORR(+1),                       
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(010000,2000),RLSE),                                
//       DCB=(AC.A,LRECL=0150,RECFM=FB),                                
//       DATACLAS=PRODX37                                               
//SYSIN    DD *                                                         
  SORT FIELDS=(25,8,A),FORMAT=BI                                        
  SUM  FIELDS=NONE                                                      
  END                                                                   
//*                                                                     
//STEP4    EXEC SORTD,                                                  
//       PARM='DYNALLOC=(DISCO,50),FILSZ=E100000000'                    
//*                                                                     
//* ***    CLASSIFICA POR CPF/CNPJ DATA DE ATUALIZACAO CADUV000         
//*                                                                     
//SORTIN   DD DSN=AC.CLLP.PRV.CADUV000(0),                              
//       DISP=SHR                                                       
//SORTOUT  DD DSN=AC.CLLP.JX003S04.CADUV000(+1),                        
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO),                                                  
//       SPACE=(TRK,(10000,2000),RLSE),                                 
//       DCB=(AC.A,LRECL=320,RECFM=FB),                                 
//       DATACLAS=PRODX37                                               
//SYSIN    DD *                                                         
 SORT FIELDS=(12,9,A,21,4,A,25,2,A,55,26,D),FORMAT=BI                   
 SUM FIELDS=NONE                                                        
//*                                                                     
//STEP4A   EXEC PGM=RDAB0262                                            
//*                                                                     
//* ***    BALANCELINE DO ARQUIVO DE MOV DO RDAB COM A CADUV000 PARA    
//* ***    MANTER APENAS OS REGISTROS DA CADUV000 COM O MOVIMENTO       
//* ***    PROCESSADO                                                   
//* ***                                                                 
//*                                                                     
//HPURDABE DD DSN=*.STEP4.SORT.SORTOUT,                                 
//       DISP=OLD                                                       
//BKPRDAB  DD DSN=*.STEP2.SORT.SORTOUT,                                 
//       DISP=OLD                                                       
//HPURDABS DD DSN=AC.CLLP.JX003S04.HPURDABS(+1),                        
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO),                                                  
//       SPACE=(TRK,(010000,2000),RLSE),                                
//       DCB=(AC.A,LRECL=320,RECFM=FB)                                  
//SYSOUT   DD SYSOUT=*                                                  
//SYSPRINT DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=Y                                                  
//*                                                                     
//STEP4B   EXEC SORTD,                                                  
//       PARM='DYNALLOC=(DISCO,50),FILSZ=E100000000'                    
//*                                                                     
//* ***    CLASS. CADUV000 POR AGENCIA E CONTA RETIRANDO AS AGENCIAS    
//* ***    IGUAL A ZEROS                                                
//*                                                                     
//SORTIN   DD DSN=*.STEP4A.HPURDABS,                                    
//       DISP=SHR                                                       
//SORTOUT  DD DSN=AC.CLLP.JX003S4B.HPURDABS.AGCTA(+1),                  
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO),                                                  
//       SPACE=(TRK,(10000,2000),RLSE),                                 
//       DCB=(AC.A,LRECL=320,RECFM=FB),                                 
//       DATACLAS=PRODX37                                               
//SYSIN    DD *                                                         
 INCLUDE COND=(37,5,ZD,NE,0)                                            
 SORT FIELDS=(37,5,A,42,13,A),FORMAT=BI                                 
 SUM FIELDS=NONE                                                        
//*                                                                     
//STEP7    EXEC PGM=RDAB0263                                            
//*                                                                     
//* ***    GERA ARQUIVO PARA ATUALIZACAO DA BASE RDAB006                
//* ***                                                                 
//*                                                                     
//NOMERDAB DD DSN=*.STEP1.SYSUT2,                                       
//       DISP=OLD                                                       
//CADUV000 DD DSN=*.STEP4B.SORT.SORTOUT,                                
//       DISP=OLD                                                       
//CADURDAB DD DSN=AC.CLLP.JX003S07.ARQLOGDR(+1),                        
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO),                                                  
//       SPACE=(TRK,(010000,2000),RLSE),                                
//       DCB=(AC.A,LRECL=146,RECFM=FB)                                  
//DESPRDAB DD DSN=AC.CLLP.JX003S07.DESPRDAB(+1),                        
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO),                                                  
//       SPACE=(TRK,(010000,2000),RLSE),                                
//       DCB=(AC.A,LRECL=146,RECFM=FB)                                  
//SYSOUT   DD SYSOUT=*                                                  
//SYSPRINT DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=Y                                                  
//*                                                                     
//STEP8    EXEC PGM=PLAN2440,                                           
//       PARM=001                                                       
//*                                                                     
//* ***    TRANSMITE ARQUIVO DE ATUALIZACAO DA TABELA RDABB006 PARA MZ1 
//*                                                                     
//ENT00001 DD DSN=*.STEP7.CADURDAB,                                     
//       DISP=SHR                                                       
//SAI00001 DD DSN=TM.CLLP.RDAB.CADURDAB.LOGRD.CORR,                     
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(010000,2000),RLSE),                                
//       DCB=(LRECL=146,RECFM=FB)                                       
//SYSOUT   DD SYSOUT=*                                                  
//PRINTER  DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=Y                                                  
//*                                                                     
//STEP9    EXEC PGM=PLAN2440,                                           
//       PARM=001                                                       
//*                                                                     
//* ***    TRANSMITE ARQUIVO CADUV000 PARA ATUALIZACAO DO PRV
//*                                                                     
//ENT00001 DD DSN=*.STEP4B.SORT.SORTOUT,                                    
//       DISP=SHR                                                       
//SAI00001 DD DSN=TM.CLLP.RDAB.JX003S4B.HPURDABS.AGCTA,
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(010000,2000),RLSE),                                
//       DCB=(LRECL=320,RECFM=FB)                                       
//SYSOUT   DD SYSOUT=*                                                  
//PRINTER  DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=Y                                                  
//*                                                                     

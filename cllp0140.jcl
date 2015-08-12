//CLLP0140 JOB 'CLLP,4120,PR32','B752247',MSGCLASS=Z,REGION=8M          
//JOBLIB   DD DSN=AC.BIBGERAL,DISP=SHR                                  
//         DD DSN=DB2A1.R2.DSNLOAD,DISP=SHR                             
//         DD DSN=SYS1.CEE.SCEERUN,DISP=SHR                             
//*                                                                     
//* ***    GERACAO DO ARQUIVO CADUV000                                  
//*                                                                     
//STEP0    EXEC PGM=POOL0003                                            
//*                                                                     
//* ***    DESCOMPRIMIR ARQUIVO                                         
//*                                                                     
//ENTRA    DD DSN=AC.PSDC.PRV.CADUB0A4(0),                              
//       DISP=SHR                                                       
//SAI      DD DSN=AC.CLLP.CADUB0A4.CLAS(+1),                            
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(300000,100000),RLSE),                              
//       DCB=(AC.A,LRECL=0642,RECFM=FB),                                
//       DATACLAS=PRODX37                                               
//SYSOUT   DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=Y                                                  
//*                                                                     
//STEP1    EXEC SORTD                                                   
//*                                                                     
//* ***    CLASSIFICA ARQUIVO DADOS PRINCIPAIS POR CPF + CLUB           
//*                                                                     
//SORTIN   DD DSN=*.STEP0.SAI,                                          
//       DISP=SHR                                                       
//SORTOUT  DD DSN=AC.CLLP.CADUB0A4.CLAS(+2),                            
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(300000,100000),RLSE),                              
//       DCB=(AC.A,LRECL=0642,RECFM=FB),                                
//       DATACLAS=PRODX37                                               
//SYSIN    DD *                                                         
  SORT FIELDS=(11,6,A),FORMAT=BI                                        
  END                                                                   
//*                                                                     
//STEP1A   EXEC PGM=POOL0003                                            
//*                                                                     
//* ***    DESCOMPRIMIR ARQUIVO                                         
//*                                                                     
//ENTRA    DD DSN=AC.PSDC.PRV.CADUB018(0),                              
//       DISP=SHR                                                       
//SAI      DD DSN=AC.CLLP.CADUB018.CLAS(+1),                            
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(300000,100000),RLSE),                              
//       DCB=(AC.A,LRECL=0642,RECFM=FB),                                
//       DATACLAS=PRODX37                                               
//SYSOUT   DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=Y                                                  
//*                                                                     
//STEP2    EXEC SORTD                                                   
//*                                                                     
//* ***    CLASSIFICA ARQUIVO DE ENDERECO POR CPF + CLUB + CATEGORIA    
//*                                                                     
//SORTIN   DD DSN=*.STEP1A.SAI,                                         
//       DISP=SHR                                                       
//SORTOUT  DD DSN=AC.CLLP.CADUB018.CLAS(+2),                            
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(300000,100000),RLSE),                              
//       DCB=(AC.A,LRECL=0642,RECFM=FB),                                
//       DATACLAS=PRODX37                                               
//SYSIN    DD *                                                         
  SORT FIELDS=(11,6,A,20,3,A,375,2,A),FORMAT=BI                         
  INCLUDE COND=(375,2,PD,EQ,1),FORMAT=BI                                
  END                                                                   
//*                                                                     
//STEP3    EXEC PGM=RDAB0850                                            
//*                                                                     
//* ***    ATUALIZA O ARQUIVO COM DADOS DE ENDERECO                     
//* ***                                                                 
//*                                                                     
//CADUB0A4 DD DSN=*.STEP1.SORT.SORTOUT,                                 
//       DISP=SHR                                                       
//CADUB018 DD DSN=*.STEP2.SORT.SORTOUT,                                 
//       DISP=SHR                                                       
//ARQSAIDA DD DSN=AC.CLLP.V000END(+1),                                  
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(300000,100000),RLSE),                              
//       DCB=(AC.A,LRECL=0320,RECFM=FB),                                
//       DATACLAS=PRODX37                                               
//SYSOUT   DD SYSOUT=*                                                  
//SYSPRINT DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=Y                                                  
//*                                                                     
//STEP3A   EXEC PGM=POOL0003                                            
//*                                                                     
//* ***    DESCOMPRIMIR ARQUIVO                                         
//*                                                                     
//ENTRA    DD DSN=AC.PSDC.PRV.CADUB006(0),                              
//       DISP=SHR                                                       
//SAI      DD DSN=AC.CLLP.CADUB006.DESC(+1),                            
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(300000,100000),RLSE),                              
//       DCB=(AC.A,LRECL=0642,RECFM=FB),                                
//       DATACLAS=PRODX37                                               
//SYSOUT   DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=Y                                                  
//*                                                                     
//STEP4    EXEC SORTD                                                   
//*                                                                     
//* ***    CLASSIFICA ARQUIVO DE TELEFONES POR CPF + CLUB               
//*                                                                     
//SORTIN   DD DSN=*.STEP3A.SAI,                                         
//       DISP=SHR                                                       
//SORTOUT  DD DSN=AC.CLLP.CADUB006.CLAS(+1),                            
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(300000,100000),RLSE),                              
//       DCB=(AC.A,LRECL=0642,RECFM=FB),                                
//       DATACLAS=PRODX37                                               
//SYSIN    DD *                                                         
  SORT FIELDS=(11,6,A,20,3,A),FORMAT=BI                                 
  END                                                                   
//*                                                                     
//STEP5    EXEC PGM=RDAB0855                                            
//*                                                                     
//* ***    ATUALIZA O ARQUIVO COM DADOS DE TELEFONE                     
//* ***                                                                 
//*                                                                     
//CADUV000 DD DSN=*.STEP3.ARQSAIDA,                                     
//       DISP=SHR                                                       
//CADUB006 DD DSN=*.STEP4.SORT.SORTOUT,                                 
//       DISP=SHR                                                       
//ARQSAIDA DD DSN=AC.CLLP.TELEV000(+1),                                 
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(300000,100000),RLSE),                              
//       DCB=(AC.A,LRECL=0320,RECFM=FB),                                
//       DATACLAS=PRODX37                                               
//SYSOUT   DD SYSOUT=*                                                  
//SYSPRINT DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=Y                                                  
//*                                                                     
//STEP5A   EXEC PGM=POOL0003                                            
//*                                                                     
//* ***    DESCOMPRIMIR ARQUIVO                                         
//*                                                                     
//ENTRA    DD DSN=AC.PSDC.PRV.CADUB0M1(0),                              
//       DISP=SHR                                                       
//SAI      DD DSN=AC.CLLP.CADUB0M1.DESC(+1),                            
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(300000,100000),RLSE),                              
//       DCB=(AC.A,LRECL=0642,RECFM=FB),                                
//       DATACLAS=PRODX37                                               
//SYSOUT   DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=Y                                                  
//*                                                                     
//STEP6    EXEC SORTD                                                   
//*                                                                     
//* ***    COMPLEMENTA O ARQUIVO CADUV000 COM DATA DE ATUALIZACAO       
//*                                                                     
//SORTIN   DD DSN=*.STEP5A.SAI,                                         
//       DISP=SHR                                                       
//SORTOUT  DD DSN=AC.CLLP.CADUB0M1.CLAS(+1),                            
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(300000,100000),RLSE),                              
//       DCB=(AC.A,LRECL=0642,RECFM=FB),                                
//       DATACLAS=PRODX37                                               
//SYSIN    DD *                                                         
  SORT FIELDS=(11,6,A),FORMAT=BI                                        
  END                                                                   
//*                                                                     
//STEP7    EXEC PGM=RDAB0857                                            
//*                                                                     
//* ***    ATUALIZA O ARQUIVO COM DADOS DE TELEFONE                     
//* ***                                                                 
//*                                                                     
//CADUV000 DD DSN=*.STEP5.ARQSAIDA,                                     
//       DISP=SHR                                                       
//CADUB0M1 DD DSN=*.STEP6.SORT.SORTOUT,                                 
//       DISP=SHR                                                       
//ARQSAIDA DD DSN=AC.CLLP.CADUV000(+1),                                 
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(300000,100000),RLSE),                              
//       DCB=(AC.A,LRECL=0320,RECFM=FB),                                
//       DATACLAS=PRODX37                                               
//SYSOUT   DD SYSOUT=*                                                  
//SYSPRINT DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=Y                                                  
//*                                                                     

//CLLPA403 JOB 'CLLP,4120,PR32','B013444',MSGCLASS=Z                    
//OUT1     OUTPUT JESDS=ALL,DEST=ANYLOCAL                               
//OUT2     OUTPUT DEST=ANYLOCAL,CLASS=E                                 
//OUT3     OUTPUT DEST=ALNJE01,CLASS=U                                  
//JOBLIB   DD DSN=MX.BIBGERAL,DISP=SHR                                  
//         DD DSN=SYS1.CEE.SCEERUN,DISP=SHR                             
//*                                                                     
//STEP1    EXEC SORTD                                                   
//*                                                                     
//* ***    CLASSIFICAR POR EMPRESA E CARTEIRA                           
//*                                                                     
//SORTIN   DD DSN=MX.CLLP.PRV.CADAPEND(0),                              
//       DISP=SHR                                                       
//SORTOUT  DD DSN=MX.CLLP.CADPENDR.LEI(+1),                             
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(000120,0024),RLSE),                                
//       DCB=(MX.A,LRECL=0168,RECFM=FB)                                 
//SYSIN    DD *                                                         
 SORT FIELDS=(1,3,BI,A,7,3,BI,A)                                        
 END                                                                    
//SORTLIB  DD DSN=SYS1.SORTLIB,                                         
//       DISP=SHR                                                       
//SORTWK01 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00120))                                            
//SORTWK02 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00120))                                            
//SORTWK03 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00120))                                            
//SORTWK04 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00120))                                            
//SORTWK05 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00120))                                            
//SORTWK06 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00120))                                            
//SORTWK07 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00120))                                            
//SORTWK08 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00120))                                            
//SORTWK09 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00120))                                            
//SORTWK10 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00120))                                            
//SORTWK11 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00120))                                            
//SORTWK12 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00120))                                            
//*                                                                     
//STEP2    EXEC MODMESU                                                 
//*                                                                     
//* ***    GERACAO DE ARQUIVO MESTRE DE AGENCIAS                        
//*                                                                     
//MODMESUS DD DSN=MX.CLLP.MESGEMAV.ORIGIN.LEI(+1),                      
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(000030,0006),RLSE),                                
//       DCB=(MX.A,LRECL=0080,RECFM=FB)                                 
//MESUPARM DD *                                                         
00080004020302025487005054002334014165003BBB008                         
//*                                                                     
//STEP3    EXEC PGM=PLAN7220                                            
//*                                                                     
//* ***    ZERA ARQUIVOS DA ROTINA 'CLLP'                               
//*                                                                     
//ZERA001  DD DSN=MX.CLLP.MAIO99L1.LEI(+1),                             
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,000010),                                            
//       DCB=(MX.A,LRECL=0533,RECFM=FB)                                 
//ZERA002  DD DSN=MX.CLLP.MAIO99L2.LEI(+1),                             
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,000010),                                            
//       DCB=(MX.A,LRECL=0533,RECFM=FB)                                 
//PRINTER  DD SYSOUT=*                                                  
//SYSOUT   DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=Y                                                  

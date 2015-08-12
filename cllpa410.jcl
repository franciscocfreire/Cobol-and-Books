//CLLPA410 JOB 'CLLP,4120,PR32','A299565',MSGCLASS=Z,REGION=6144K,      
//       SCHENV=DB2                                                     
//*MAIN    CLASS=MZDB2A                                                 
//OUT1     OUTPUT JESDS=ALL,DEST=ANYLOCAL                               
//OUTE     OUTPUT DEST=ANYLOCAL,CLASS=E                                 
//OUTF     OUTPUT DEST=ANYLOCAL,CLASS=F                                 
//OUTG     OUTPUT DEST=ANYLOCAL,CLASS=G                                 
//OUTJ     OUTPUT DEST=ANYLOCAL,CLASS=J                                 
//OUTM     OUTPUT DEST=ANYLOCAL,CLASS=M                                 
//OUTN     OUTPUT DEST=ALNJE01,CLASS=N                                  
//OUTO     OUTPUT DEST=ANYLOCAL,CLASS=O                                 
//OUTP     OUTPUT DEST=ALNJE01,CLASS=P                                  
//OUTQ     OUTPUT DEST=ALNJE01,CLASS=Q                                  
//OUTR     OUTPUT DEST=ALNJE01,CLASS=R                                  
//OUTRR    OUTPUT DEST=ANYLOCAL,CLASS=R                                 
//OUTV     OUTPUT DEST=ANYLOCAL,CLASS=V                                 
//OUTW     OUTPUT DEST=ALNJE02,CLASS=W                                  
//OUT5     OUTPUT DEST=ALNJE02,CLASS=5                                  
//OUTZ     OUTPUT DEST=ANYLOCAL,CLASS=Z                                 
//*                                                                     
//* ***    %%LIBSYM MJ.EVTA.PRV.CAFORM %%MEMSYM OUTJJ31                 
//*                                                                     
//JOBLIB   DD DSN=MX.BIBGERAL,DISP=SHR                                  
//         DD DSN=DB2M1.R2.DSNLOAD,DISP=SHR                             
//         DD DSN=SYS1.CEE.SCEERUN,DISP=SHR                             
//STEP1    EXEC PGM=CLLPG616,                                           
//       PARM='000000020'                                               
//*                                                                     
//* ***    SELECIONA REGISTROS DO CADASTRO ATUAL                        
//*                                                                     
//CADASTRO DD DSN=MX.CLLP.CADADAAV.LEI(0),                              
//       DISP=SHR                                                       
//PARMCLLP DD DSN=MX.CLLP.PAR415.LEI(0),                                
//       DISP=SHR                                                       
//CADPENDE DD DSN=MX.CLLP.CADPENDR.LEI(0),                              
//       DISP=SHR                                                       
//ARQESTAT DD DSN=MX.CLLP.PRP.ARQESTAT(0),                              
//       DISP=SHR                                                       
//ARQDATA  DD DSN=MX.CLLP.PRV.DATAVISO(0),                              
//       DISP=SHR                                                       
//CADSELEC DD DSN=MX.CLLP.ARQSE7AV(+1),                                 
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO,02),                                               
//       SPACE=(TRK,(001140,0228),RLSE),                                
//       DCB=(MX.A,LRECL=0362,RECFM=FB)                                 
//TOTAIS   DD SYSOUT=%%FORM#JJ31,                                       
//       DCB=(LRECL=0080,RECFM=FBA)                                     
//SORTLIB  DD DSN=SYS1.SORTLIB,                                         
//       DISP=SHR                                                       
//SORTWK01 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00420))                                            
//SORTWK02 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00420))                                            
//SORTWK03 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00420))                                            
//SORTWK04 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00420))                                            
//SORTWK05 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00420))                                            
//SORTWK06 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00420))                                            
//SORTWK07 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00420))                                            
//SORTWK08 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00420))                                            
//SORTWK09 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00420))                                            
//SORTWK10 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00420))                                            
//SORTWK11 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00420))                                            
//SORTWK12 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00420))                                            
//SYSOUT   DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=Y                                                  
//*                                                                     
//STEP2    EXEC SORTD                                                   
//*                                                                     
//* ***    CLASSIFICAR POR EMPRESA, TIPO DE PENDENCIA E CONTRATO        
//*                                                                     
//SORTIN   DD DSN=*.STEP1.CADSELEC,                                     
//       DISP=(OLD,DELETE,KEEP)                                         
//SORTOUT  DD DSN=MX.CLLP.CADP0792(+1),                                 
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO,02),                                               
//       SPACE=(TRK,(001140,0228),RLSE),                                
//       DCB=(MX.A,LRECL=0362,RECFM=FB)                                 
//SYSIN    DD *                                                         
 SORT FIELDS=(240,3,PD,A,243,3,PD,A,246,8,PD,A)                         
 END                                                                    
//*                                                                     
//STEP3    EXEC PGM=CLLPE002                                            
//*                                                                     
//* ***    RETORNA VALORES PARA CRUZEIROS REAIS.                        
//*                                                                     
//CADACR   DD DSN=MX.CLLP.PRV.CADSELEC.CRUZEIRO(0),                     
//       DISP=SHR                                                       
//CADANT   DD DSN=*.STEP2.SORT.SORTOUT,                                 
//       DISP=OLD                                                       
//CADATU   DD DSN=MX.CLLP.CADRETO3.LEI(+1),                             
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO,02),                                               
//       SPACE=(TRK,(001140,0228),RLSE),                                
//       DCB=(MX.A,LRECL=0362,RECFM=FB)                                 
//SYSOUT   DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=Y                                                  
//*                                                                     
//STEP4    EXEC SORTD                                                   
//*                                                                     
//* ***    CLASSIFICAR POR CGC/CPF E VENCIMENTO                         
//*                                                                     
//SORTIN   DD DSN=*.STEP3.CADATU,                                       
//       DISP=(OLD,DELETE,KEEP)                                         
//SORTOUT  DD DSN=MX.CLLP.CADSRTAV.LEI(+1),                             
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO,02),                                               
//       SPACE=(TRK,(001140,0228),RLSE),                                
//       DCB=(MX.A,LRECL=0362,RECFM=FB)                                 
//SYSIN    DD *                                                         
 SORT FIELDS=(1,10,BI,A,13,5,BI,A)                                      
 END                                                                    
//*                                                                     

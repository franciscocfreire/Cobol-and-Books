//CLLP1070 JOB 'CLLP,4120,PR32','B142376',MSGCLASS=Z,REGION=4M,         
//       SCHENV=DB2                                                     
//OUT1     OUTPUT JESDS=ALL,DEST=ANYLOCAL                               
//OUT2     OUTPUT DEST=MZNJE01,CLASS=E                                  
//OUT3     OUTPUT DEST=ANYLOCAL,CLASS=F                                 
//OUT4     OUTPUT DEST=ANYLOCAL,CLASS=G                                 
//OUT5     OUTPUT DEST=MZNJE01,CLASS=J                                  
//OUT6     OUTPUT DEST=MZNJE01,CLASS=M                                  
//OUT7     OUTPUT DEST=ANYLOCAL,CLASS=N                                 
//OUT8     OUTPUT DEST=MZNJE01,CLASS=O                                  
//OUT9     OUTPUT DEST=ANYLOCAL,CLASS=P                                 
//OUT10    OUTPUT DEST=ANYLOCAL,CLASS=Q                                 
//OUT11    OUTPUT DEST=ANYLOCAL,CLASS=R                                 
//OUT12    OUTPUT DEST=MZNJE01,CLASS=R                                  
//OUT13    OUTPUT DEST=MZNJE01,CLASS=V                                  
//OUT14    OUTPUT DEST=ALNJE02,CLASS=W                                  
//OUT15    OUTPUT DEST=ALNJE02,CLASS=5                                  
//OUT16    OUTPUT DEST=ANYLOCAL,CLASS=Z                                 
//*                                                                     
//* ***    %%LIBSYM AC.EVTA.PRV.CAFORM %%MEMSYM OUTOS08                 
//* ***    %%LIBSYM AC.EVTA.PRV.CAFORM %%MEMSYM OUTOTA5                 
//* ***    %%LIBSYM AC.EVTA.PRV.CAFORM %%MEMSYM OUTOTA6                 
//* ***    %%LIBSYM AC.EVTA.PRV.CAFORM %%MEMSYM OUTOTA7                 
//* ***    %%LIBSYM AC.EVTA.PRV.CAFORM %%MEMSYM OUTOTA8                 
//* ***    %%LIBSYM AC.EVTA.PRV.CAFORM %%MEMSYM OUTOTA9                 
//* ***    %%LIBSYM AC.EVTA.PRV.CAFORM %%MEMSYM OUTOTB0                 
//* ***    %%LIBSYM AC.EVTA.PRV.CAFORM %%MEMSYM OUTOTB1                 
//* ***    %%LIBSYM AC.EVTA.PRV.CAFORM %%MEMSYM OUTOTB2                 
//* ***    %%LIBSYM AC.EVTA.PRV.CAFORM %%MEMSYM OUTOTB3                 
//* ***    %%LIBSYM AC.EVTA.PRV.CAFORM %%MEMSYM OUTOTB4                 
//* ***    %%LIBSYM AC.EVTA.PRV.CAFORM %%MEMSYM OUTOTB7                 
//* ***    %%LIBSYM AC.EVTA.PRV.CAFORM %%MEMSYM OUTOTB8                 
//* ***    %%LIBSYM AC.EVTA.PRV.CAFORM %%MEMSYM OUTOTB9                 
//* ***    %%LIBSYM AC.EVTA.PRV.CAFORM %%MEMSYM OUTOTC0                 
//* ***    %%LIBSYM AC.EVTA.PRV.CAFORM %%MEMSYM OUTOTC1                 
//* ***    %%LIBSYM AC.EVTA.PRV.CAFORM %%MEMSYM OUTOTC5                 
//*                                                                     
//JOBLIB   DD DSN=AC.BIBGERAL,DISP=SHR                                  
//         DD DSN=DB2A1.R2.DSNLOAD,DISP=SHR                             
//         DD DSN=SYS1.CEE.SCEERUN,DISP=SHR                             
//*                                                                     
//* ***    INIBIDOS OS RELATORIOS DOS STEP'S 1, 3 E 11 EM               
//* ***    21.03.2006 VISTO ESTAR GERANDO RELATORIOS ACIMA              
//* ***    DE 5 MILHOES DE LINHAS(ACIMA DE 50 MILHOES NO TO-            
//* ***    TAL) ABENDANDO O JOB.                                        
//* ***    COM AUTORIZACAO DO SR HOMERO (ANALISTA DA ROTINA).           
//*                                                                     
//STEP1    EXEC PGM=RDAB0001                                            
//*                                                                     
//* ***    OBTER CPESSOA                                                
//*                                                                     
//ARQPARVV DD DSN=AC.CLLP.CPFPJALT.CLAS(0),                             
//       DISP=SHR                                                       
//PENDIRES DD DSN=AC.CLLP.TABIRES.IRESV001(0),                          
//       DISP=(OLD,DELETE,KEEP)                                         
//SAICOMCR DD DSN=AC.CLLP.SAICOMCR.IRESALT(+1),                         
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO,18),                                               
//       SPACE=(TRK,(020000,4000),RLSE),                                
//       DCB=(AC.A,LRECL=150,RECFM=FB)                                  
//SAISEMCR DD DSN=AC.CLLP.SAISEMCR.IRESALT(+1),                         
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO,03),                                               
//       SPACE=(TRK,(007580,1516),RLSE),                                
//       DCB=(AC.A,LRECL=150,RECFM=FB)                                  
//*#RELENCVV DD SYSOUT=(P,,OSL9),                                      
//*#RELENCVV DD DUMMY,                                                    
//*#       DCB=(LRECL=0132,RECFM=FBA)                                     
//*#RELTOTAL DD SYSOUT=(,,OSM0),                                        
//*#RELTOTAL DD DUMMY,                                                    
//*#       DCB=(LRECL=0081,RECFM=FBA)                                     
//*#*RELNENCO DD SYSOUT=(P,,OSM1),                                      
//RELNENCO DD DUMMY,                                                    
//       DCB=(LRECL=0132,RECFM=FBA)                                     
//SYSOUT   DD SYSOUT=*                                                  
//SYSPRINT DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=Y                                                  
//*                                                                     
//STEP2    EXEC SORTD                                                   
//*                                                                     
//* ***    CLASSIFICA POR CPSSOA                                        
//*                                                                     
//SORTIN   DD DSN=*.STEP1.SAICOMCR,                                     
//       DISP=SHR                                                       
//SORTOUT  DD DSN=AC.CLLP.SAICOMCR.ALTCLAS(+1),                         
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO,18),                                               
//       SPACE=(TRK,(020000,4000),RLSE),                                
//       DCB=(AC.A,LRECL=0150,RECFM=FB)                                 
//SYSIN    DD *                                                         
 SORT FIELDS=(25,5,PD,A,30,3,PD,A,33,2,ZD,A),FORMAT=BI                  
 END                                                                    
//*                                                                     
//STEP3    EXEC PGM=RDAB0005                                            
//*                                                                     
//* ***    OBTEM CPSSOA-CATDTR E DADOS DE PJ, QUANDO NAO ENCONTRADOS    
//* ***    ESTAO NAS TABS. C/BCO,AG E CTA                               
//*                                                                     
//ARQPARVV DD DSN=*.STEP2.SORT.SORTOUT,                                 
//       DISP=SHR                                                       
//PENDFICA DD DSN=AC.CLLP.TABFICA.FICAV000(0),                          
//       DISP=OLD                                                       
//ARQDAPES DD DSN=AC.CLLP.LOGJRALT(+1),                                 
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO,40),                                               
//       SPACE=(TRK,(020000,4000),RLSE),                                
//       DCB=(AC.A,LRECL=500,RECFM=FB)                                  
//*#RELVAZIO DD SYSOUT=(,,OSM2),                                        
//*#RELVAZIO DD DUMMY,                                                    
//*#       DCB=(LRECL=0081,RECFM=FBA)                                     
//*#RELTOTAL DD SYSOUT=(,,OSZ3),                                        
//*#RELTOTAL DD DUMMY,                                                    
//*#       DCB=(LRECL=0081,RECFM=FBA)                                     
//*#RELNENCO DD DUMMY,                                                    
//*#       DCB=(LRECL=0132,RECFM=FBA)                                     
//SYSOUT   DD SYSOUT=*                                                  
//SYSPRINT DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=Y                                                  
//*                                                                     
//STEP3A   EXEC SORTD                                                   
//*                                                                     
//* ***    CLASSIFICA POR CPF/CNPJ, STATUS E DATA DE ATUALIZACAO        
//*                                                                     
//SORTIN   DD DSN=*.STEP3.ARQDAPES,                                     
//       DISP=SHR                                                       
//SORTOUT  DD DSN=AC.CLLP.ARQPSALT.CLAS(+1),                            
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO,40),                                               
//       SPACE=(TRK,(020000,4000),RLSE),                                
//       DCB=(AC.A,LRECL=0500,RECFM=FB)                                 
//SYSIN    DD *                                                         
 SORT FIELDS=(1,5,PD,A,6,3,PD,A,9,2,PD,A,469,2,CH,A,465,4,CH,D,         
              462,2,CH,D,459,2,CH,D)                                    
 END                                                                    
//*                                                                     
//STEP3B   EXEC PGM=RDAB0007                                            
//*                                                                     
//* ***    RETIRAR DUPLICIDADE DE CHAVES (CPSSOA)                       
//*                                                                     
//ARQPARVV DD DSN=*.STEP3A.SORT.SORTOUT,                                
//       DISP=OLD                                                       
//ARQPEFIS DD DSN=AC.CLLP.SAICOMCR.PEFISALT(+1),                        
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO,08),                                               
//       SPACE=(TRK,(020000,4000),RLSE),                                
//       DCB=(AC.A,LRECL=150,RECFM=FB)                                  
//ARQPEJUR DD DSN=AC.CLLP.SAICOMCR.PEJURALT(+1),                        
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO,02),                                               
//       SPACE=(TRK,(002520,0504),RLSE),                                
//       DCB=(AC.A,LRECL=150,RECFM=FB)                                  
//ARQDAPES DD DSN=AC.CLLP.LOGJURAL(+1),                                 
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO,30),                                               
//       SPACE=(TRK,(020000,4000),RLSE),                                
//       DCB=(AC.A,LRECL=500,RECFM=FB)                                  
//RELVAZIO DD SYSOUT=%%FORM#OS08,                                       
//       DCB=(LRECL=0081,RECFM=FBA)                                     
//RELTOTAL DD SYSOUT=%%FORM#OTA5,                                       
//       DCB=(LRECL=0081,RECFM=FBA)                                     
//SYSOUT   DD SYSOUT=*                                                  
//SYSPRINT DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=Y                                                  
//*                                                                     
//STEP4    EXEC SORTD                                                   
//*                                                                     
//* ***    CLASSIFICA POR CPSSOA-CADTR                                  
//*                                                                     
//SORTIN   DD DSN=*.STEP3B.ARQPEJUR,                                    
//       DISP=SHR                                                       
//SORTOUT  DD DSN=AC.CLLP.SAICOMCR.PJCLALT(+1),                         
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO,02),                                               
//       SPACE=(TRK,(002520,0504),RLSE),                                
//       DCB=(AC.A,LRECL=0150,RECFM=FB)                                 
//SYSIN    DD *                                                         
 SORT FIELDS=(144,6,A),FORMAT=BI                                        
 END                                                                    
//*                                                                     
//STEP5    EXEC SORTD                                                   
//*                                                                     
//* ***    CLASSIFICA POR AG - C/C                                      
//*                                                                     
//SORTIN   DD DSN=*.STEP1.SAISEMCR,                                     
//       DISP=SHR                                                       
//         DD DSN=AC.CLLP.AGCTALT(0),                                   
//       DISP=SHR                                                       
//SORTOUT  DD DSN=AC.CLLP.SAISEMCR.IRESCLAL(+1),                        
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO,03),                                               
//       SPACE=(TRK,(007600,1520),RLSE),                                
//       DCB=(AC.A,LRECL=0150,RECFM=FB)                                 
//SYSIN    DD *                                                         
 SORT FIELDS=(3,3,PD,A,6,7,PD,A)                                        
 END                                                                    
//*                                                                     
//STEP6    EXEC PGM=RDAB0010                                            
//*                                                                     
//* ***    OBTEM CPSSOA, CPSSOA-CADTR E DADOS DE PJ.QUANDO NAO          
//* ***    ENCONTRADOS, ESTÃO NAS TABS C/BCO, AG E CTA                  
//*                                                                     
//ARQPARVV DD DSN=*.STEP5.SORT.SORTOUT,                                 
//       DISP=SHR                                                       
//PENDFICA DD DSN=AC.CLLP.TABFICA.FICAV000.CLASFI(0),                   
//       DISP=(OLD,DELETE,KEEP)                                         
//ARQPEFIS DD DSN=AC.CLLP.RDAB0010.PEFISALT(+1),                        
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO,02),                                               
//       SPACE=(TRK,(001320,0264),RLSE),                                
//       DCB=(AC.A,LRECL=150,RECFM=FB)                                  
//ARQPEJUR DD DSN=AC.CLLP.RDAB0010.PEJURALT(+1),                        
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(01000,0400),RLSE),                                 
//       DCB=(AC.A,LRECL=150,RECFM=FB)                                  
//ARQDAPES DD DSN=AC.CLLP.LOGJRALT.RDAB0010(+1),                        
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO,07),                                               
//       SPACE=(TRK,(020000,4000),RLSE),                                
//       DCB=(AC.A,LRECL=500,RECFM=FB)                                  
//RELVAZIO DD SYSOUT=%%FORM#OTA6,                                       
//       DCB=(LRECL=0081,RECFM=FBA)                                     
//RELTOTAL DD SYSOUT=%%FORM#OTA7,                                       
//       DCB=(LRECL=0081,RECFM=FBA)                                     
//RELNENCO DD SYSOUT=%%FORM#OTA8,                                       
//       DCB=(LRECL=0132,RECFM=FBA)                                     
//SYSOUT   DD SYSOUT=*                                                  
//SYSPRINT DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=Y                                                  
//*                                                                     
//STEP7    EXEC SORTD                                                   
//*                                                                     
//* ***    CLASSIFICA POR CCLUB                                         
//*                                                                     
//SORTIN   DD DSN=*.STEP6.ARQPEJUR,                                     
//       DISP=SHR                                                       
//         DD DSN=*.STEP4.SORT.SORTOUT,                                 
//       DISP=SHR                                                       
//SORTOUT  DD DSN=AC.CLLP.PEJURALT.RDAB0010.CLAS(+1),                   
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO,02),                                               
//       SPACE=(TRK,(001920,0384),RLSE),                                
//       DCB=(AC.A,LRECL=0150,RECFM=FB)                                 
//SYSIN    DD *                                                         
 SORT FIELDS=(144,6,PD,A),FORMAT=BI                                     
 END                                                                    
//*                                                                     
//STEP8    EXEC PGM=RDAB0020                                            
//*                                                                     
//* ***    OBTEM DT FUNDACAO E VALOR DO FATURAMENTO                     
//*                                                                     
//ARQPARVV DD DSN=*.STEP7.SORT.SORTOUT,                                 
//       DISP=SHR                                                       
//PENDFICA DD DSN=AC.CLLP.TABFICA.FICAV002(0),                          
//       DISP=SHR                                                       
//ARQDAPES DD DSN=AC.CLLP.RDAB0020.VRFATALT(+1),                        
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO,03),                                               
//       SPACE=(TRK,(006430,1286),RLSE),                                
//       DCB=(AC.A,LRECL=500,RECFM=FB)                                  
//RELVAZIO DD SYSOUT=%%FORM#OTA9,                                       
//       DCB=(LRECL=0081,RECFM=FBA)                                     
//RELTOTAL DD SYSOUT=%%FORM#OTB0,                                       
//       DCB=(LRECL=0081,RECFM=FBA)                                     
//RELNENCO DD SYSOUT=%%FORM#OTB1,                                       
//       DCB=(LRECL=0132,RECFM=FBA)                                     
//SYSOUT   DD SYSOUT=*                                                  
//SYSPRINT DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=Y                                                  
//*                                                                     
//STEP9    EXEC SORTD                                                   
//*                                                                     
//* ***    CLASSIFICA POR CPF/CNPJ                                      
//*                                                                     
//SORTIN   DD DSN=*.STEP3B.ARQPEFIS,                                    
//       DISP=SHR                                                       
//         DD DSN=*.STEP6.ARQPEFIS,                                     
//       DISP=SHR                                                       
//SORTOUT  DD DSN=AC.CLLP.PEFISALT.CLASS(+1),                           
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO,08),                                               
//       SPACE=(TRK,(020000,4000),RLSE),                                
//       DCB=(AC.A,LRECL=0150,RECFM=FB)                                 
//SYSIN    DD *                                                         
 SORT FIELDS=(144,6,PD,A),FORMAT=BI                                     
 END                                                                    
//*                                                                     
//STEP10   EXEC PGM=RDAB0015                                            
//*                                                                     
//* ***    OBTEM DADOS PF E LOGRADOURO                                  
//*                                                                     
//ARQPARVV DD DSN=*.STEP9.SORT.SORTOUT,                                 
//       DISP=OLD                                                       
//PENDFICA DD DSN=AC.CLLP.TABFICA.FICAV001(0),                          
//       DISP=OLD                                                       
//ARQDAPES DD DSN=AC.CLLP.RDAB0015.LOGFSALT(+1),                        
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO,30),                                               
//       SPACE=(TRK,(020000,4000),RLSE),                                
//       DCB=(AC.A,LRECL=500,RECFM=FB)                                  
//RELVAZIO DD SYSOUT=%%FORM#OTB2,                                       
//       DCB=(LRECL=0081,RECFM=FBA)                                     
//RELTOTAL DD SYSOUT=%%FORM#OTB3,                                       
//       DCB=(LRECL=0081,RECFM=FBA)                                     
//RELNENCO DD SYSOUT=%%FORM#OTB4,                                       
//       DCB=(LRECL=0132,RECFM=FBA)                                     
//SYSOUT   DD SYSOUT=*                                                  
//SYSPRINT DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=Y                                                  
//*                                                                     
//STEP13   EXEC PGM=RDAB0028                                            
//*                                                                     
//* ***    OBTEM NOME DO CONJUGE                                        
//*                                                                     
//ARQUIRES DD DSN=AC.CLLP.TABFICA.FICAV001(0),                          
//       DISP=OLD                                                       
//ARQPARVV DD DSN=*.STEP9.SORT.SORTOUT,                                 
//       DISP=SHR                                                       
//ARQDAPES DD DSN=AC.CLLP.RDAB0028.CONJGALT(+1),                        
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO,04),                                               
//       SPACE=(TRK,(011910,2382),RLSE),                                
//       DCB=(AC.A,LRECL=500,RECFM=FB)                                  
//RELNENCO DD SYSOUT=%%FORM#OTB7,                                       
//       DCB=(LRECL=0132,RECFM=FBA)                                     
//RELTOTAL DD SYSOUT=%%FORM#OTB8,                                       
//       DCB=(LRECL=0080,RECFM=FBA)                                     
//SYSOUT   DD SYSOUT=*                                                  
//SYSPRINT DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=Y                                                  
//*                                                                     
//STEP14   EXEC PGM=RDAB0030                                            
//*                                                                     
//* ***    OBTEM RATING DO CLIENTE                                      
//*                                                                     
//ARQPARVV DD DSN=AC.CLLP.CPFPJALT.CLAS(0),                             
//       DISP=SHR                                                       
//PENDIRES DD DSN=AC.CLLP.TABIRES.IRESB018(0),                          
//       DISP=OLD                                                       
//ARQDAPES DD DSN=AC.CLLP.RDAB0030.RTINGALT(+1),                        
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO,30),                                               
//       SPACE=(TRK,(020000,4000),RLSE),                                
//       DCB=(AC.A,LRECL=500,RECFM=FB)                                  
//RELVAZIO DD SYSOUT=%%FORM#OTB9,                                       
//       DCB=(LRECL=0081,RECFM=FBA)                                     
//RELTOTAL DD SYSOUT=%%FORM#OTC0,                                       
//       DCB=(LRECL=0081,RECFM=FBA)                                     
//RELNENCO DD SYSOUT=%%FORM#OTC1,                                       
//       DCB=(LRECL=0133,RECFM=FBA)                                     
//SYSOUT   DD SYSOUT=*                                                  
//SYSPRINT DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=Y                                                  
//*                                                                     
//STEP15   EXEC SORTD                                                   
//*                                                                     
//* ***    CLASSIFICA ARQUIVO SOCIOS POR  CLUB                          
//*                                                                     
//SORTIN   DD DSN=AC.PSDC.PRV.CADUB0Q4.DESCOMPR(0),                     
//       DISP=SHR                                                       
//SORTOUT  DD DSN=AC.CLLP.CADUB0Q4.CLAS(+1),                            
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(300000,100000),RLSE),                              
//       DCB=(AC.A,LRECL=0642,RECFM=FB),                                
//       DATACLAS=PRODX37                                               
//SYSIN    DD *                                                         
  SORT FIELDS=(11,6,A),FORMAT=BI                                        
  END                                                                   
//*                                                                     
//STEP16   EXEC PGM=RDAB0900                                            
//*                                                                     
//* ***    OBTER DADOS DOS SOCIOS P/ COMPLEMENTAR                       
//* ***    ARQ. DE CLIENTES.                                            
//* ***    ( TABELA RDAB0500 - ARQ. CLIENTES )                          
//*                                                                     
//ARQPARVV DD DSN=*.STEP7.SORT.SORTOUT,                                 
//       DISP=SHR                                                       
//CADUB0Q4 DD DSN=*.STEP15.SORT.SORTOUT,                                
//       DISP=(OLD,DELETE,KEEP)                                         
//ARQSAIDA DD DSN=AC.CLLP.RDAB0032.SOCIOALT.ANT(+1),                    
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(01000,0400),RLSE),                                 
//       DCB=(AC.A,LRECL=0223,RECFM=FB)                                 
//PRINTER  DD SYSOUT=*                                                  
//SYSOUT   DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=Y                                                  
//*                                                                     
//STEP17   EXEC SORTD                                                   
//*                                                                     
//* ***    CLASSIFICA PENDFICA  :                                       
//* ***    PVV-CPSSOA-LIGADA                                            
//*                                                                     
//SORTIN   DD DSN=*.STEP16.ARQSAIDA,                                    
//       DISP=OLD                                                       
//SORTOUT  DD DSN=AC.CLLP.RDAB0031.SOCIOALT.CLAS(+1),                   
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO,02),                                               
//       SPACE=(TRK,(002530,0506),RLSE),                                
//       DCB=(AC.A,LRECL=0223,RECFM=FB)                                 
//SYSIN    DD *                                                         
 SORT FIELDS=(35,10,CH,A)                                               
 END                                                                    
//*                                                                     
//STEP18   EXEC SORTD                                                   
//*                                                                     
//* ***    CLASSIFICA CADUV001  :                                       
//* ***    CPF/CNPJ                                                     
//*                                                                     
//SORTIN   DD DSN=AC.CLLP.TABFICA.FICAV001(0),                          
//       DISP=OLD                                                       
//SORTOUT  DD DSN=AC.CLLP.ARQCADU.CADUV001.TEMP(+1),                    
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO,02),                                               
//       SPACE=(TRK,(002530,0506),RLSE),                                
//       DCB=(AC.A,LRECL=0570,RECFM=FB)                                 
//SYSIN    DD *                                                         
 SORT FIELDS=(35,5,PD,A,40,3,PD,A)                                              
 END                                                                    
//*                                                                     
//STEP19   EXEC PGM=RDAB0901                                            
//*                                                                     
//* ***    OBTER CPSSOA-CADTR, CPF/CNPJ E NOME SOCIO                    
//*                                                                     
//ARQPARVV DD DSN=*.STEP17.SORT.SORTOUT,                                
//       DISP=(OLD,DELETE,KEEP)                                         
//CADUV001 DD DSN=*.STEP18.SORT.SORTOUT,                                
//       DISP=(OLD,DELETE,KEEP)                                         
//ARQSAIDA DD DSN=AC.CLLP.RDAB0032.SOCIOALT(+1),                        
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(01000,0400),RLSE),                                 
//       DCB=(AC.A,LRECL=0223,RECFM=FB)                                 
//PRINTER  DD SYSOUT=*                                                  
//SYSOUT   DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=Y                                                  
//*                                                                     
//STEP22   EXEC SORTD                                                   
//*                                                                     
//* ***    CLASSIFICA ARQSOCIO POR:                                     
//* ***    SOC-CGCNUM E SOC-CGCFIL                                      
//*                                                                     
//SORTIN   DD DSN=*.STEP19.ARQSAIDA,                                    
//       DISP=OLD                                                       
//SORTOUT  DD DSN=AC.CLLP.RDAB0033.SOCIOALT.CLAS(+1),                   
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO,02),                                               
//       SPACE=(TRK,(003710,0742),RLSE),                                
//       DCB=(AC.A,LRECL=0223,RECFM=FB)                                 
//SYSIN    DD *                                                         
 SORT FIELDS=(35,5,PD,A,40,3,PD,A)                                      
 END                                                                    
//*                                                                     
//STEP23   EXEC PGM=RDAB0034                                            
//*                                                                     
//* ***    FORMATAR RATING                                              
//*                                                                     
//ENTSOCIO DD DSN=*.STEP22.SORT.SORTOUT,                                
//       DISP=SHR                                                       
//PENDIRES DD DSN=AC.CLLP.TABIRES.IRESB018(0),                          
//       DISP=(OLD,DELETE,KEEP)                                         
//SAISOCIO DD DSN=AC.CLLP.RDAB0034.SOCIOALT(+1),                        
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO,02),                                               
//       SPACE=(TRK,(003710,0742),RLSE),                                
//       DCB=(AC.A,LRECL=0223,RECFM=FB)                                 
//RELATO   DD SYSOUT=%%FORM#OTC5,                                       
//       DCB=(LRECL=080,RECFM=FBA)                                      
//PRINTER  DD SYSOUT=*                                                  
//SYSOUT   DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=Y                                                  
//*                                                                     
//STEP24   EXEC SORTD                                                   
//*                                                                     
//* ***    CLASSIFICA ARQSOCIO                                          
//*                                                                     
//SORTIN   DD DSN=*.STEP23.SAISOCIO,                                    
//       DISP=OLD                                                       
//SORTOUT  DD DSN=AC.CLLP.ARQSOCIO.BSFICALT(+1),                        
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO,02),                                               
//       SPACE=(TRK,(003270,0654),RLSE),                                
//       DCB=(AC.A,LRECL=0197,RECFM=FB)                                 
//SYSIN    DD *                                                         
 OUTREC FIELDS=(27,197)                                                 
 SORT FIELDS=(27,5,PD,A,32,3,PD,A,35,5,PD,A)                            
 INCLUDE COND=(27,5,PD,GT,0,AND,32,3,PD,GT,0,AND,35,5,PD,GT,0)          
 END                                                                    
//*                                                                     

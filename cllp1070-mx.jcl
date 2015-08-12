//CLLP1070 JOB 'CLLP,4120,PR32','C085384',MSGCLASS=Z,REGION=8096K,      
//       SCHENV=DB2                                                     
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
//* ***    %%LIBSYM MJ.EVTA.PRV.CAFORM %%MEMSYM OUTKLQ7                 
//* ***    %%LIBSYM MJ.EVTA.PRV.CAFORM %%MEMSYM OUTTHW9                 
//* ***    %%LIBSYM MJ.EVTA.PRV.CAFORM %%MEMSYM OUTHP32                 
//* ***    %%LIBSYM MJ.EVTA.PRV.CAFORM %%MEMSYM OUTHP33                 
//* ***    %%LIBSYM MJ.EVTA.PRV.CAFORM %%MEMSYM OUTHP35                 
//* ***    %%LIBSYM MJ.EVTA.PRV.CAFORM %%MEMSYM OUTHHBO                 
//* ***    %%LIBSYM MJ.EVTA.PRV.CAFORM %%MEMSYM OUTHHBP                 
//* ***    %%LIBSYM MJ.EVTA.PRV.CAFORM %%MEMSYM OUTHHBQ                 
//* ***    %%LIBSYM MJ.EVTA.PRV.CAFORM %%MEMSYM OUTBAGM                 
//* ***    %%LIBSYM MJ.EVTA.PRV.CAFORM %%MEMSYM OUTBAGN                 
//* ***    %%LIBSYM MJ.EVTA.PRV.CAFORM %%MEMSYM OUTHHBR                 
//* ***    %%LIBSYM MJ.EVTA.PRV.CAFORM %%MEMSYM OUTHHBS                 
//* ***    %%LIBSYM MJ.EVTA.PRV.CAFORM %%MEMSYM OUTBAGO                 
//* ***    %%LIBSYM MJ.EVTA.PRV.CAFORM %%MEMSYM OUTBAGP                 
//* ***    %%LIBSYM MJ.EVTA.PRV.CAFORM %%MEMSYM OUTHHBT                 
//* ***    %%LIBSYM MJ.EVTA.PRV.CAFORM %%MEMSYM OUTHHBU                 
//* ***    %%LIBSYM MJ.EVTA.PRV.CAFORM %%MEMSYM OUTSTH7                 
//* ***    %%LIBSYM MJ.EVTA.PRV.CAFORM %%MEMSYM OUTMID6                 
//* ***    %%LIBSYM MJ.EVTA.PRV.CAFORM %%MEMSYM OUTHP36                 
//* ***    %%LIBSYM MJ.EVTA.PRV.CAFORM %%MEMSYM OUTHHBV                 
//* ***    %%LIBSYM MJ.EVTA.PRV.CAFORM %%MEMSYM OUTHP37                 
//* ***    %%LIBSYM MJ.EVTA.PRV.CAFORM %%MEMSYM OUTHP61                 
//* ***    %%LIBSYM MJ.EVTA.PRV.CAFORM %%MEMSYM OUTHQ96                 
//* ***    %%LIBSYM MJ.EVTA.PRV.CAFORM %%MEMSYM OUTDT69                 
//* ***    %%LIBSYM MJ.EVTA.PRV.CAFORM %%MEMSYM OUTHP68                 
//*                                                                     
//JOBLIB   DD DSN=MX.BIBGERAL,DISP=SHR                                  
//         DD DSN=DB2M1.R2.DSNLOAD,DISP=SHR                             
//         DD DSN=SYS1.CEE.SCEERUN,DISP=SHR                             
//STEP01   EXEC SORTD                                                   
//*                                                                     
//* ***    CLASSIFICA POR CGC.CPF DEV/NATUREZA/AGENCIA/DT.VCTO          
//*                                                                     
//SORTIN   DD DSN=MX.CLLP.CDSERAT.DEV(0),                               
//       DISP=SHR                                                       
//         DD DSN=MX.CLLP.SERATDV4.SA(0),                               
//       DISP=SHR                                                       
//SORTOUT  DD DSN=MX.CLLP.SERATDV6.CLASS.SA(+1),                        
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO,30),                                               
//       SPACE=(TRK,(020000,4000),RLSE),                                
//       DCB=(MX.A,LRECL=0500,RECFM=FB)                                 
//SORTWK01 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK02 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK03 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK04 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK05 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK06 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SYSIN    DD *                                                         
 SORT FIELDS=(1,12,A,21,3,A,13,5,A),FORMAT=BI                           
 END                                                                    
//*                                                                     
//STEP02   EXEC SORTD                                                   
//*                                                                     
//* ***    CLASSIFICA POR CGC.CPF AVAL/NATUREZA/AGENCIA/DT.VCTO         
//*                                                                     
//SORTIN   DD DSN=MX.CLLP.CDSERAT.AVAL(0),                              
//       DISP=SHR                                                       
//         DD DSN=MX.CLLP.SERATAV5.SA(0),                               
//       DISP=SHR                                                       
//SORTOUT  DD DSN=MX.CLLP.SERATAV6.CLASS.SA(+1),                        
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO,02),                                               
//       SPACE=(TRK,(002250,0450),RLSE),                                
//       DCB=(MX.A,LRECL=0500,RECFM=FB)                                 
//SORTWK01 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK02 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK03 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK04 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK05 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK06 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SYSIN    DD *                                                         
 SORT FIELDS=(413,12,A,21,3,A,1,12,A,13,5,A),FORMAT=BI                  
 END                                                                    
//*                                                                     
//STEP2A   EXEC PGM=CLLP7219                                            
//*                                                                     
//* ***    RETIRAR DO ARQUIVO DE AVALISTAS REGISTROS                    
//* ***    SEM DEVEDOR CORRESPONDENTE                                   
//*                                                                     
//ARQDEVED DD DSN=*.STEP01.SORT.SORTOUT,                                
//       DISP=OLD                                                       
//ARQAVALI DD DSN=*.STEP02.SORT.SORTOUT,                                
//       DISP=OLD                                                       
//ARQDATAS DD DSN=MX.CLLP.DATAMOV.CLLP1000(0),                          
//       DISP=SHR                                                       
//ARQSAIDA DD DSN=MX.CLLP.SERATAV7.SA(+1),                              
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO,02),                                               
//       SPACE=(TRK,(002240,0448),RLSE),                                
//       DCB=(MX.A,LRECL=0500,RECFM=FB)                                 
//ARQINCON DD DSN=MX.CLLP.INCO7219.SA(+1),                              
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(000020,0004),RLSE),                                
//       DCB=(MX.A,LRECL=0500,RECFM=FB)                                 
//RELINCON DD SYSOUT=%%FORM#KLQ7,                                       
//       DCB=(LRECL=0133,RECFM=FBA)                                     
//SYSOUT   DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=Y                                                  
//*                                                                     
//STEP03   EXEC PGM=CLLP7535                                            
//*                                                                     
//* ***    ATUALIZA ARQUIVO NUM.SEQUECIAL PARA ROTINA IRES (SPC/SERASA) 
//* ***                                                                 
//* ***    NA 1. EXECUCAO UTILIZAR MX.CLLP.PRV.NSEQIRES(0)              
//*                                                                     
//ARQDATA  DD DSN=MX.CLLP.DATAMOV.CLLP1000(0),                          
//       DISP=SHR                                                       
//NSEQIRES DD DSN=MX.CLLP.PRV.NSEQIRES.SA(0),                           
//       DISP=SHR                                                       
//IRESNATU DD DSN=MX.CLLP.PRV.NSEQIRES.SA(+1),                          
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(000010,0002),RLSE),                                
//       DCB=(MX.A,LRECL=0014,RECFM=FB)                                 
//SYSOUT   DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=Y                                                  
//*                                                                     
//STEP03B  EXEC PGM=CLLP7107                                            
//*                                                                     
//* ***    LISTA CONTRATOS VENCIDOS ENTRE 1795 DIAS E 1825 DIAS         
//* ***    (PRECISA VER SE TEVE RESTRICAO NA TABELA LPCLB037)           
//*                                                                     
//ARQDB2   DD DSN=*.STEP01.SORT.SORTOUT,                                
//       DISP=OLD                                                       
//TOTAIS   DD SYSOUT=%%FORM#THW9,                                       
//       DCB=(LRECL=0080,RECFM=FBA)                                     
//SYSOUT   DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=Y                                                  
//*                                                                     
//STEP03C  EXEC SORTD                                                   
//*                                                                     
//* ***    SELECIONA INCLUSOES FTSERASA                                 
//*                                                                     
//SORTIN   DD DSN=*.STEP01.SORT.SORTOUT,                                
//       DISP=SHR                                                       
//SORTOUT1 DD DSN=MX.CLLP.SERATDV6.CLASS.SA.B237(+1),                   
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO,30),                                               
//       SPACE=(TRK,(020000,4000),RLSE),                                
//       DCB=(MX.A,LRECL=0500,RECFM=FB)                                 
//SORTOUT2 DD DSN=MX.CLLP.SERATDV6.CLASS.SA.B204(+1),                   
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO,05),                                               
//       SPACE=(TRK,(015180,3036),RLSE),                                
//       DCB=(MX.A,LRECL=0500,RECFM=FB)                                 
//SORTWK01 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK02 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK03 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK04 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK05 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK06 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SYSIN    DD *                                                         
 SORT FIELDS=COPY                                                       
 OUTFIL OUTREC=(1,500),                                                 
       INCLUDE=(028,3,CH,EQ,C'435',OR,028,3,CH,EQ,C'439'),              
       FNAMES=SORTOUT2                                                  
 OUTFIL OUTREC=(1,500),                                                 
        FNAMES=SORTOUT1,SAVE                                            
 END                                                                    
//*                                                                     
//STEP04   EXEC PGM=CLLP7095,                                           
//       PARM='0237'                                                    
//*                                                                     
//* ***    CRIA ARQUIVO PARA A CENTRAL 'S E R A S A'.                   
//* ***    GERA ARQUIVO PARA A CENTRAL 'S P C'.                         
//*                                                                     
//TABELAL  DD DSN=MX.CLLP.ARQNSEL.CLLP0037.CLAS(0),                     
//       DISP=SHR,                                                      
//       DCB=(BUFNO=1)                                                  
//ARQDATA  DD DSN=MX.CLLP.DATAMOV.CLLP1000(0),                          
//       DISP=SHR,                                                      
//       DCB=(BUFNO=1)                                                  
//SEANTDEV DD DSN=MX.CLLP.CDSERASA.DEV.SA(0),                           
//       DISP=SHR,                                                      
//       DCB=(BUFNO=1)                                                  
//SEDIADEV DD DSN=*.STEP03C.SORT.SORTOUT1,                              
//       DISP=(OLD,DELETE,KEEP),                                        
//       DCB=(BUFNO=1)                                                  
//SEANTAVA DD DSN=MX.CLLP.CDSERASA.AVAL.SA(0),                          
//       DISP=SHR,                                                      
//       DCB=(BUFNO=1)                                                  
//SEDIAAVA DD DSN=*.STEP2A.ARQSAIDA,                                    
//       DISP=OLD,                                                      
//       DCB=(BUFNO=1)                                                  
//CDSEQNUM DD DSN=MX.CLLP.PRV.CDNSEQSE.SA(0),                           
//       DISP=SHR,                                                      
//       DCB=(BUFNO=1)                                                  
//NSEQIRES DD DSN=*.STEP03.IRESNATU,                                    
//       DISP=SHR,                                                      
//       DCB=(BUFNO=1)                                                  
//SEATUDEV DD DSN=MX.CLLP.CDSERASA.CL7096.DEV.SA(+1),                   
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO,30),                                               
//       SPACE=(TRK,(020000,4000),RLSE),                                
//       DCB=(MX.A,LRECL=0500,RECFM=FB)                                 
//SEATUAVA DD DSN=MX.CLLP.CDSERASA.CL7096.AVAL.SA(+1),                  
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO,02),                                               
//       SPACE=(TRK,(002230,0446),RLSE),                                
//       DCB=(MX.A,LRECL=0500,RECFM=FB)                                 
//SEHISDEV DD DSN=MX.CLLP.HISTODIA.DEV.SA(+1),                          
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO,10),                                               
//       SPACE=(TRK,(020000,4000),RLSE),                                
//       DCB=(MX.A,LRECL=0500,RECFM=FB)                                 
//SEHISAVA DD DSN=MX.CLLP.HISTODIA.AVAL.SA(+1),                         
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(000540,0108),RLSE),                                
//       DCB=(MX.A,LRECL=0500,RECFM=FB)                                 
//FITASERA DD DSN=MX.CLLP.FTSERASA.CL7096.SA(+1),                       
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO,10),                                               
//       SPACE=(TRK,(020000,4000),RLSE),                                
//       DCB=(MX.A,LRECL=0500,RECFM=FB)                                 
//SPCFDEVE DD DSN=MX.CLLP.CDSPCDIA.DEV.SA(+1),                          
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO,02),                                               
//       SPACE=(TRK,(001830,0366),RLSE),                                
//       DCB=(MX.A,LRECL=0530,RECFM=FB)                                 
//SPCFAVAL DD DSN=MX.CLLP.CDSPCDIA.AVAL.SA(+1),                         
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(000210,0042),RLSE),                                
//       DCB=(MX.A,LRECL=0530,RECFM=FB)                                 
//ARQTEMP  DD DSN=MX.CLLP.ARQTEMP.SA(+1),                               
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO,10),                                               
//       SPACE=(TRK,(020000,4000),RLSE),                                
//       DCB=(MX.A,LRECL=0537,RECFM=FB)                                 
//CADIMPE  DD DUMMY,                                                    
//       DCB=(LRECL=0270,RECFM=FB)                                      
//CDOUTSEQ DD DSN=MX.CLLP.PRV.CDNSEQSE.SA(+1),                          
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(000010,0002),RLSE),                                
//       DCB=(MX.A,LRECL=0010,RECFM=FB)                                 
//TOTAIS   DD SYSOUT=%%FORM#HP32,                                       
//       DCB=(LRECL=0080,RECFM=FBA)                                     
//ROTULO   DD SYSOUT=%%FORM#HP33,                                       
//       DCB=(LRECL=0080,RECFM=FBA)                                     
//RELATO   DD SYSOUT=%%FORM#HP35,                                       
//       DCB=(LRECL=0133,RECFM=FBA)                                     
//SYSOUT   DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=Y                                                  
//*                                                                     
//STEP04Z  EXEC PGM=CLLP7095,                                           
//       PARM='0204'                                                    
//*                                                                     
//* ***    CRIA ARQUIVO PARA A CENTRAL 'S E R A S A'.                   
//* ***    GERA ARQUIVO PARA A CENTRAL 'S P C'.                         
//*                                                                     
//TABELAL  DD DSN=MX.CLLP.ARQNSEL.CLLP0037.CLAS(0),                     
//       DISP=SHR,                                                      
//       DCB=(BUFNO=1)                                                  
//ARQDATA  DD DSN=MX.CLLP.DATAMOV.CLLP1000(0),                          
//       DISP=SHR,                                                      
//       DCB=(BUFNO=1)                                                  
//SEANTDEV DD DSN=MX.CLLP.CDSERASA.DEV.SA.B204(0),                      
//       DISP=OLD                                                       
//SEDIADEV DD DSN=*.STEP03C.SORT.SORTOUT2,                              
//       DISP=(OLD,DELETE,KEEP),                                        
//       DCB=(BUFNO=1)                                                  
//SEANTAVA DD DSN=MX.CLLP.CDSERASA.AVAL.SA.B204(0),                     
//       DISP=SHR                                                       
//SEDIAAVA DD DSN=*.STEP2A.ARQSAIDA,                                    
//       DISP=OLD,                                                      
//       DCB=(BUFNO=1)                                                  
//CDSEQNUM DD DSN=MX.CLLP.PRV.CDNSEQSE.SA.B204(0),                      
//       DISP=SHR                                                       
//NSEQIRES DD DSN=*.STEP03.IRESNATU,                                    
//       DISP=SHR,                                                      
//       DCB=(BUFNO=1)                                                  
//SEATUDEV DD DSN=MX.CLLP.CDSERASA.CL7096.DEV.SA.B204(+1),              
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO,05),                                               
//       SPACE=(TRK,(015080,3016),RLSE),                                
//       DCB=(MX.A,LRECL=0500,RECFM=FB)                                 
//SEATUAVA DD DSN=MX.CLLP.CDSERASA.CL7096.AVA.SA.B204(+1),              
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(000010,0002),RLSE),                                
//       DCB=(MX.A,LRECL=0500,RECFM=FB)                                 
//SEHISDEV DD DSN=MX.CLLP.HISTODIA.DEV.SA.B204(+1),                     
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(000350,0070),RLSE),                                
//       DCB=(MX.A,LRECL=0500,RECFM=FB)                                 
//SEHISAVA DD DSN=MX.CLLP.HISTODIA.AVAL.SA.B204(+1),                    
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(000010,0002),RLSE),                                
//       DCB=(MX.A,LRECL=0500,RECFM=FB)                                 
//FITASERA DD DSN=MX.CLLP.FTSERASA.CL7096.SA.B204(+1),                  
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(000900,0180),RLSE),                                
//       DCB=(MX.A,LRECL=0500,RECFM=FB)                                 
//SPCFDEVE DD DSN=MX.CLLP.CDSPCDIA.DEV.SA.B204(+1),                     
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(000010,0002),RLSE),                                
//       DCB=(MX.A,LRECL=0530,RECFM=FB)                                 
//SPCFAVAL DD DSN=MX.CLLP.CDSPCDIA.AVAL.SA.B204(+1),                    
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(000010,0002),RLSE),                                
//       DCB=(MX.A,LRECL=0530,RECFM=FB)                                 
//ARQTEMP  DD DSN=MX.CLLP.ARQTEMP.SA.B204(+1),                          
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(000950,0190),RLSE),                                
//       DCB=(MX.A,LRECL=0537,RECFM=FB)                                 
//CADIMPE  DD DUMMY,                                                    
//       DCB=(LRECL=0270,RECFM=FB)                                      
//CDOUTSEQ DD DSN=MX.CLLP.PRV.CDNSEQSE.SA.B204(+1),                     
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(000010,0002),RLSE),                                
//       DCB=(MX.A,LRECL=0010,RECFM=FB)                                 
//TOTAIS   DD SYSOUT=%%FORM#HHBO,                                       
//       DCB=(LRECL=0080,RECFM=FBA)                                     
//ROTULO   DD SYSOUT=%%FORM#HHBP,                                       
//       DCB=(LRECL=0080,RECFM=FBA)                                     
//RELATO   DD SYSOUT=%%FORM#HHBQ,                                       
//       DCB=(LRECL=0133,RECFM=FBA)                                     
//SYSOUT   DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=Y                                                  
//*                                                                     
//STEP04L  EXEC SORTD                                                   
//*                                                                     
//* ***    SELECIONA INCLUSOES FTSERASA                                 
//*                                                                     
//SORTIN   DD DSN=*.STEP04.FITASERA,                                    
//       DISP=SHR                                                       
//SORTOUT1 DD DSN=MX.CLLP.FTSERASA.INC.DEV(+1),                         
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO,04),                                               
//       SPACE=(TRK,(010720,2144),RLSE),                                
//       DCB=(MX.A,LRECL=0500,RECFM=FB)                                 
//SORTOUT2 DD DSN=MX.CLLP.FTSERASA.INC.AVAL(+1),                        
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(000210,0042),RLSE),                                
//       DCB=(MX.A,LRECL=0500,RECFM=FB)                                 
//SORTOUT3 DD DSN=MX.CLLP.FTSERASA.EXCLUSAO(+1),                        
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO,10),                                               
//       SPACE=(TRK,(020000,4000),RLSE),                                
//       DCB=(MX.A,LRECL=0500,RECFM=FB)                                 
//SORTWK01 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK02 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK03 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK04 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK05 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK06 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SYSIN    DD *                                                         
 SORT FIELDS=COPY                                                       
 OUTFIL OUTREC=(1,500),                                                 
       INCLUDE=(002,1,CH,EQ,C'I',AND,057,1,CH,EQ,C' '),FNAMES=SORTOUT1  
 OUTFIL OUTREC=(1,500),                                                 
       INCLUDE=(002,1,CH,EQ,C'I',AND,057,1,CH,NE,C' '),FNAMES=SORTOUT2  
 OUTFIL OUTREC=(1,500),                                                 
       INCLUDE=(002,1,CH,NE,C'I'),FNAMES=SORTOUT3                       
 END                                                                    
//*                                                                     
//STEP04L1 EXEC SORTD                                                   
//*                                                                     
//* ***    SELECIONA INCLUSOES FTSERASA                                 
//*                                                                     
//SORTIN   DD DSN=*.STEP04Z.FITASERA,                                   
//       DISP=SHR                                                       
//SORTOUT1 DD DSN=MX.CLLP.FTSERASA.INC.DEV.B204(+1),                    
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(000890,0178),RLSE),                                
//       DCB=(MX.A,LRECL=0500,RECFM=FB)                                 
//SORTOUT2 DD DSN=MX.CLLP.FTSERASA.INC.AVAL.B204(+1),                   
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(000010,0002),RLSE),                                
//       DCB=(MX.A,LRECL=0500,RECFM=FB)                                 
//SORTOUT3 DD DSN=MX.CLLP.FTSERASA.EXCLUSAO.B204(+1),                   
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(000380,0076),RLSE),                                
//       DCB=(MX.A,LRECL=0500,RECFM=FB)                                 
//SORTWK01 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK02 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK03 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK04 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK05 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK06 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SYSIN    DD *                                                         
 SORT FIELDS=COPY                                                       
 OUTFIL OUTREC=(1,500),                                                 
       INCLUDE=(002,1,CH,EQ,C'I',AND,057,1,CH,EQ,C' '),FNAMES=SORTOUT1  
 OUTFIL OUTREC=(1,500),                                                 
       INCLUDE=(002,1,CH,EQ,C'I',AND,057,1,CH,NE,C' '),FNAMES=SORTOUT2  
 OUTFIL OUTREC=(1,500),                                                 
       INCLUDE=(002,1,CH,NE,C'I'),FNAMES=SORTOUT3                       
 END                                                                    
//*                                                                     
//STEP04M  EXEC PGM=CLLP7026                                            
//*                                                                     
//* ***    ACERTA CAMPO DE CPF/CNPJ ARQ SERASA DEVEDOR                  
//*                                                                     
//ENTRA    DD DSN=*.STEP04L.SORT.SORTOUT1,                              
//       DISP=SHR                                                       
//SAI      DD DSN=MX.CLLP.FTSERA.CL7026.DEV(+1),                        
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO,03),                                               
//       SPACE=(TRK,(009500,1900),RLSE),                                
//       DCB=(MX.A,LRECL=530,RECFM=FB)                                  
//SYSOUT   DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=Y                                                  
//*                                                                     
//STEP04M1 EXEC PGM=CLLP7026                                            
//*                                                                     
//* ***    ACERTA CAMPO DE CPF/CNPJ ARQ SERASA DEVEDOR                  
//*                                                                     
//ENTRA    DD DSN=*.STEP04L1.SORT.SORTOUT1,                             
//       DISP=SHR                                                       
//SAI      DD DSN=MX.CLLP.FTSERA.CL7026.DEV.B204(+1),                   
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(000940,0188),RLSE),                                
//       DCB=(MX.A,LRECL=530,RECFM=FB)                                  
//SYSOUT   DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=Y                                                  
//*                                                                     
//STEP04N  EXEC PGM=CLLP7026                                            
//*                                                                     
//* ***    ACERTA CAMPO DE CPF/CNPJ ARQ SERASA AVALISTA                 
//*                                                                     
//ENTRA    DD DSN=*.STEP04L.SORT.SORTOUT2,                              
//       DISP=SHR                                                       
//SAI      DD DSN=MX.CLLP.FTSERA.CL7026.AVAL(+1),                       
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(000230,0046),RLSE),                                
//       DCB=(MX.A,LRECL=530,RECFM=FB)                                  
//SYSOUT   DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=Y                                                  
//*                                                                     
//STEP04N1 EXEC PGM=CLLP7026                                            
//*                                                                     
//* ***    ACERTA CAMPO DE CPF/CNPJ ARQ SERASA AVALISTA                 
//*                                                                     
//ENTRA    DD DSN=*.STEP04L1.SORT.SORTOUT2,                             
//       DISP=SHR                                                       
//SAI      DD DSN=MX.CLLP.FTSERA.CL7026.AVAL.B204(+1),                  
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(000010,0002),RLSE),                                
//       DCB=(MX.A,LRECL=530,RECFM=FB)                                  
//SYSOUT   DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=Y                                                  
//*                                                                     
//STEP04O  EXEC SORTD                                                   
//*                                                                     
//* ***    SORT DO CADASTRO DO FTSERASA DEVEDOR ACERTADO                
//* ***    POR CPF/CGC, NATUREZA E AGENCIA                              
//*                                                                     
//SORTIN   DD DSN=*.STEP04M.SAI,                                        
//       DISP=SHR                                                       
//SORTOUT  DD DSN=MX.CLLP.FTSERASA.DEV.CLA(+1),                         
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO,03),                                               
//       SPACE=(TRK,(009500,1900),RLSE),                                
//       DCB=(MX.A,LRECL=530,RECFM=FB)                                  
//SORTWK01 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK02 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK03 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK04 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK05 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK06 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SYSIN    DD *                                                         
 SORT FIELDS=(501,15,CH,A,17,3,CH,A,3,4,CH,A)                           
 END                                                                    
//*                                                                     
//STEP04O1 EXEC SORTD                                                   
//*                                                                     
//* ***    SORT DO CADASTRO DO FTSERASA DEVEDOR ACERTADO                
//* ***    POR CPF/CGC, NATUREZA E AGENCIA                              
//*                                                                     
//SORTIN   DD DSN=*.STEP04M1.SAI,                                       
//       DISP=SHR                                                       
//SORTOUT  DD DSN=MX.CLLP.FTSERASA.DEV.CLA.B204(+1),                    
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(000940,0188),RLSE),                                
//       DCB=(MX.A,LRECL=530,RECFM=FB)                                  
//SORTWK01 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK02 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK03 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK04 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK05 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK06 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SYSIN    DD *                                                         
 SORT FIELDS=(501,15,CH,A,17,3,CH,A,3,4,CH,A)                           
 END                                                                    
//*                                                                     
//STEP04P  EXEC SORTD                                                   
//*                                                                     
//* ***    SORT DO CADASTRO DO FTSERASA AVALISTA ACERTADO               
//* ***    POR CPF/CGC DEVEDOR, NATUREZA, AGENCIA E                     
//* ***    CPF/CGC AVALISTA                                             
//*                                                                     
//SORTIN   DD DSN=*.STEP04N.SAI,                                        
//       DISP=SHR                                                       
//SORTOUT  DD DSN=MX.CLLP.FTSERASA.AVAL.CLA(+1),                        
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(000230,0046),RLSE),                                
//       DCB=(MX.A,LRECL=530,RECFM=FB)                                  
//SORTWK01 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK02 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK03 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK04 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK05 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK06 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SYSIN    DD *                                                         
 SORT FIELDS=(501,15,CH,A,17,3,CH,A,3,4,CH,A,516,15,CH,A)               
 END                                                                    
//*                                                                     
//STEP04P1 EXEC SORTD                                                   
//*                                                                     
//* ***    SORT DO CADASTRO DO FTSERASA AVALISTA ACERTADO               
//* ***    POR CPF/CGC DEVEDOR, NATUREZA, AGENCIA E                     
//* ***    CPF/CGC AVALISTA                                             
//*                                                                     
//SORTIN   DD DSN=*.STEP04N1.SAI,                                       
//       DISP=SHR                                                       
//SORTOUT  DD DSN=MX.CLLP.FTSERASA.AVAL.CLA.B204(+1),                   
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(000010,0002),RLSE),                                
//       DCB=(MX.A,LRECL=530,RECFM=FB)                                  
//SORTWK01 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK02 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK03 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK04 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK05 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK06 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SYSIN    DD *                                                         
 SORT FIELDS=(501,15,CH,A,17,3,CH,A,3,4,CH,A,516,15,CH,A)               
 END                                                                    
//*                                                                     
//STEP04Q  EXEC SORTD                                                   
//*                                                                     
//* ***    FAZ SORT DO CADASTRO DO CDSERASA DEVEDOR                     
//* ***    POR CPF/CGC, NATUREZA E AGENCIA                              
//*                                                                     
//SORTIN   DD DSN=*.STEP04.SEATUDEV,                                    
//       DISP=SHR                                                       
//SORTOUT  DD DSN=MX.CLLP.CDSERASA.DEV.CLA(+1),                         
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO,30),                                               
//       SPACE=(TRK,(020000,4000),RLSE),                                
//       DCB=(MX.A,LRECL=500,RECFM=FB)                                  
//SORTWK01 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK02 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK03 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK04 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK05 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK06 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SYSIN    DD *                                                         
 SORT FIELDS=(1,10,CH,A,11,2,CH,A,21,3,CH,A)                            
 END                                                                    
//*                                                                     
//STEP04Q1 EXEC SORTD                                                   
//*                                                                     
//* ***    FAZ SORT DO CADASTRO DO CDSERASA DEVEDOR                     
//* ***    POR CPF/CGC, NATUREZA E AGENCIA                              
//*                                                                     
//SORTIN   DD DSN=*.STEP04Z.SEATUDEV,                                   
//       DISP=SHR                                                       
//SORTOUT  DD DSN=MX.CLLP.CDSERASA.DEV.CLA.B204(+1),                    
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO,05),                                               
//       SPACE=(TRK,(015080,3016),RLSE),                                
//       DCB=(MX.A,LRECL=500,RECFM=FB)                                  
//SORTWK01 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK02 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK03 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK04 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK05 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK06 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SYSIN    DD *                                                         
 SORT FIELDS=(1,10,CH,A,11,2,CH,A,21,3,CH,A)                            
 END                                                                    
//*                                                                     
//STEP04R  EXEC SORTD                                                   
//*                                                                     
//* ***    FAZ SORT DO CADASTRO DO CDSERASA AVALISTA                    
//* ***    POR CPF/CGC DEVEDOR, NATUREZA, AGENCIA E                     
//* ***    CPF/CGC DO AVALISTA                                          
//*                                                                     
//SORTIN   DD DSN=*.STEP04.SEATUAVA,                                    
//       DISP=SHR                                                       
//SORTOUT  DD DSN=MX.CLLP.CDSERASA.AVAL.CLA(+1),                        
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO,02),                                               
//       SPACE=(TRK,(002230,0446),RLSE),                                
//       DCB=(MX.A,LRECL=500,RECFM=FB)                                  
//SORTWK01 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK02 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK03 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK04 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK05 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK06 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SYSIN    DD *                                                         
 SORT FIELDS=(413,10,CH,A,423,2,CH,A,21,3,CH,A,001,10,CH,A)             
 END                                                                    
//*                                                                     
//STEP04R1 EXEC SORTD                                                   
//*                                                                     
//* ***    FAZ SORT DO CADASTRO DO CDSERASA AVALISTA                    
//* ***    POR CPF/CGC DEVEDOR, NATUREZA, AGENCIA E                     
//* ***    CPF/CGC DO AVALISTA                                          
//*                                                                     
//SORTIN   DD DSN=*.STEP04Z.SEATUAVA,                                   
//       DISP=SHR                                                       
//SORTOUT  DD DSN=MX.CLLP.CDSERASA.AVAL.CLA.B204(+1),                   
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(000010,0002),RLSE),                                
//       DCB=(MX.A,LRECL=500,RECFM=FB)                                  
//SORTWK01 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK02 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK03 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK04 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK05 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK06 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SYSIN    DD *                                                         
 SORT FIELDS=(413,10,CH,A,423,2,CH,A,21,3,CH,A,001,10,CH,A)             
 END                                                                    
//*                                                                     
//STEP04S  EXEC PGM=CLLP7027                                            
//*                                                                     
//* ***    JOGA 'SPACES' NO PRACA EMBRATEL PARA DEVEDOR.                
//*                                                                     
//FTSERASA DD DSN=*.STEP04O.SORT.SORTOUT,                               
//       DISP=SHR                                                       
//CDSERASA DD DSN=*.STEP04Q.SORT.SORTOUT,                               
//       DISP=SHR                                                       
//SAIFTSER DD DSN=MX.CLLP.FTSAIDA.CL7027.DEV(+1),                       
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO,03),                                               
//       SPACE=(TRK,(008980,1796),RLSE),                                
//       DCB=(MX.A,LRECL=0500,RECFM=FB)                                 
//SAICDSER DD DSN=MX.CLLP.CDSERASA.DEV.SA(+1),                          
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO,30),                                               
//       SPACE=(TRK,(020000,4000),RLSE),                                
//       DCB=(MX.A,LRECL=0500,RECFM=FB)                                 
//SAIFTDIF DD DSN=MX.CLLP.FTDIFSA.CL7027.DEV(+1),                       
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(000010,0002),RLSE),                                
//       DCB=(MX.A,LRECL=0500,RECFM=FB)                                 
//SAICDDIF DD DSN=MX.CLLP.CDSERDIF.CL7027.DEV.SA(+1),                   
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO,30),                                               
//       SPACE=(TRK,(020000,4000),RLSE),                                
//       DCB=(MX.A,LRECL=0500,RECFM=FB)                                 
//RELATO01 DD SYSOUT=%%FORM#BAGM,                                       
//       DCB=(LRECL=081,RECFM=FBA)                                      
//RELATO02 DD SYSOUT=%%FORM#BAGN,                                       
//       DCB=(LRECL=081,RECFM=FBA)                                      
//SYSUDUMP DD SYSOUT=*                                                  
//SYSOUT   DD SYSOUT=Y                                                  
//*                                                                     
//STEP04S1 EXEC PGM=CLLP7027                                            
//*                                                                     
//* ***    JOGA 'SPACES' NO PRACA EMBRATEL PARA DEVEDOR.                
//*                                                                     
//FTSERASA DD DSN=*.STEP04O1.SORT.SORTOUT,                              
//       DISP=SHR                                                       
//CDSERASA DD DSN=*.STEP04Q1.SORT.SORTOUT,                              
//       DISP=SHR                                                       
//SAIFTSER DD DSN=MX.CLLP.FTSAIDA.CL7027.DEV.B204(+1),                  
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(000890,0178),RLSE),                                
//       DCB=(MX.A,LRECL=0500,RECFM=FB)                                 
//SAICDSER DD DSN=MX.CLLP.CDSERASA.DEV.SA.B204(+1),                     
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO,04),                                               
//       SPACE=(TRK,(012320,2464),RLSE),                                
//       DCB=(MX.A,LRECL=0500,RECFM=FB)                                 
//SAIFTDIF DD DSN=MX.CLLP.FTDIFSA.CL7027.DEV.B204(+1),                  
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(000450,0090),RLSE),                                
//       DCB=(MX.A,LRECL=0500,RECFM=FB)                                 
//SAICDDIF DD DSN=MX.CLLP.CDSERDIF.CL7027.DEV.SA.B204(+1),              
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO,05),                                               
//       SPACE=(TRK,(015080,3016),RLSE),                                
//       DCB=(MX.A,LRECL=0500,RECFM=FB)                                 
//RELATO01 DD SYSOUT=%%FORM#HHBR,                                       
//       DCB=(LRECL=081,RECFM=FBA)                                      
//RELATO02 DD SYSOUT=%%FORM#HHBS,                                       
//       DCB=(LRECL=081,RECFM=FBA)                                      
//SYSUDUMP DD SYSOUT=*                                                  
//SYSOUT   DD SYSOUT=Y                                                  
//*                                                                     
//STEP04T  EXEC PGM=CLLP7029                                            
//*                                                                     
//* ***    JOGA SPACES NO PRAÇA EMBRATEL PARA AVALISTA.                 
//*                                                                     
//FTSERASA DD DSN=*.STEP04P.SORT.SORTOUT,                               
//       DISP=SHR                                                       
//CDSERASA DD DSN=*.STEP04R.SORT.SORTOUT,                               
//       DISP=SHR                                                       
//SAIFTSER DD DSN=MX.CLLP.FTSAIDA.CL7027.AVAL(+1),                      
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(000210,0042),RLSE),                                
//       DCB=(MX.A,LRECL=0500,RECFM=FB)                                 
//SAICDSER DD DSN=MX.CLLP.CDSERASA.AVAL.SA(+1),                         
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO,02),                                               
//       SPACE=(TRK,(002230,0446),RLSE),                                
//       DCB=(MX.A,LRECL=0500,RECFM=FB)                                 
//SAIFTDIF DD DSN=MX.CLLP.FTDIFSA.CL7027.AVAL(+1),                      
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(000010,0002),RLSE),                                
//       DCB=(MX.A,LRECL=0500,RECFM=FB)                                 
//SAICDDIF DD DSN=MX.CLLP.CDSERDIF.CL7027.AVAL.SA(+1),                  
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO,02),                                               
//       SPACE=(TRK,(002190,0438),RLSE),                                
//       DCB=(MX.A,LRECL=0500,RECFM=FB)                                 
//RELATO01 DD SYSOUT=%%FORM#BAGO,                                       
//       DCB=(LRECL=081,RECFM=FBA)                                      
//RELATO02 DD SYSOUT=%%FORM#BAGP,                                       
//       DCB=(LRECL=081,RECFM=FBA)                                      
//SYSUDUMP DD SYSOUT=*                                                  
//SYSOUT   DD SYSOUT=Y                                                  
//*                                                                     
//STEP04T1 EXEC PGM=CLLP7029                                            
//*                                                                     
//* ***    JOGA SPACES NO PRAÇA EMBRATEL PARA AVALISTA.                 
//*                                                                     
//FTSERASA DD DSN=*.STEP04P1.SORT.SORTOUT,                              
//       DISP=SHR                                                       
//CDSERASA DD DSN=*.STEP04R1.SORT.SORTOUT,                              
//       DISP=SHR                                                       
//SAIFTSER DD DSN=MX.CLLP.FTSAIDA.CL7027.AVAL.B204(+1),                 
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(000010,0002),RLSE),                                
//       DCB=(MX.A,LRECL=0500,RECFM=FB)                                 
//SAICDSER DD DSN=MX.CLLP.CDSERASA.AVAL.SA.B204(+1),                    
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(000010,0002),RLSE),                                
//       DCB=(MX.A,LRECL=0500,RECFM=FB)                                 
//SAIFTDIF DD DSN=MX.CLLP.FTDIFSA.CL7027.AVAL.B204(+1),                 
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(000010,0002),RLSE),                                
//       DCB=(MX.A,LRECL=0500,RECFM=FB)                                 
//SAICDDIF DD DSN=MX.CLLP.CDSERDIF.CL7027.AVA.SA.B204(+1),              
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(000010,0002),RLSE),                                
//       DCB=(MX.A,LRECL=0500,RECFM=FB)                                 
//RELATO01 DD SYSOUT=%%FORM#HHBT,                                       
//       DCB=(LRECL=081,RECFM=FBA)                                      
//RELATO02 DD SYSOUT=%%FORM#HHBU,                                       
//       DCB=(LRECL=081,RECFM=FBA)                                      
//SYSUDUMP DD SYSOUT=*                                                  
//SYSOUT   DD SYSOUT=Y                                                  
//*                                                                     
//STEP04U  EXEC SORTD                                                   
//*                                                                     
//* ***    MONTAR O FTSERASA COM A EXCLUSAO E A INCLUSAO                
//* ***    CLASSIFICANDO POR NUMERO DE SEQUENCIA                        
//*                                                                     
//SORTIN   DD DSN=*.STEP04L.SORT.SORTOUT3,                              
//       DISP=SHR                                                       
//         DD DSN=*.STEP04S.SAIFTSER,                                   
//       DISP=SHR                                                       
//         DD DSN=*.STEP04T.SAIFTSER,                                   
//       DISP=SHR                                                       
//SORTOUT  DD DSN=MX.CLLP.FTSERASA.SA(+1),                              
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO,10),                                               
//       SPACE=(TRK,(020000,4000),RLSE),                                
//       DCB=(MX.A,LRECL=0500,RECFM=FB)                                 
//SORTWK01 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK02 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK03 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK04 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK05 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK06 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SYSIN    DD *                                                         
 SORT FIELDS=(494,7,CH,A)                                               
 END                                                                    
//*                                                                     
//STEP04U1 EXEC SORTD                                                   
//*                                                                     
//* ***    MONTAR O FTSERASA COM A EXCLUSAO E A INCLUSAO                
//* ***    CLASSIFICANDO POR NUMERO DE SEQUENCIA                        
//* ***    BANCO BRADESCO CARTOES S/A                                   
//*                                                                     
//SORTIN   DD DSN=*.STEP04L1.SORT.SORTOUT3,                             
//       DISP=SHR                                                       
//         DD DSN=*.STEP04S1.SAIFTSER,                                  
//       DISP=SHR                                                       
//         DD DSN=*.STEP04T1.SAIFTSER,                                  
//       DISP=SHR                                                       
//SORTOUT  DD DSN=MX.CLLP.FTSERASA.SA.B204(+1),                         
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(000900,0180),RLSE),                                
//       DCB=(MX.A,LRECL=0500,RECFM=FB)                                 
//SORTWK01 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK02 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK03 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK04 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK05 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK06 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SYSIN    DD *                                                         
 SORT FIELDS=(494,7,CH,A)                                               
 END                                                                    
//*                                                                     
//STEP04V  EXEC SORTD                                                   
//*                                                                     
//* ***    CLASSIFICA POR CGC-CPF                                       
//* ***    NATUREZA / AGENCIA                                           
//*                                                                     
//SORTIN   DD DSN=MX.CLLP.N040394.CDSERASA.AVAL.SA(0),                  
//       DISP=SHR                                                       
//SORTOUT  DD DSN=MX.CLLP.CDSERASA.AVALCL1.SA(+1),                      
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO,02),                                               
//       SPACE=(TRK,(002210,0442),RLSE),                                
//       DCB=(MX.A,LRECL=0500,RECFM=FB)                                 
//SYSIN    DD *                                                         
 SORT FIELDS=(1,5,PD,A,6,3,PD,A,9,2,PD,A,11,2,CH,A,21,3,PD,A,           
           24,4,PD,A,28,3,CH,A,31,4,PD,A)                               
 END                                                                    
//*                                                                     
//STEP04W  EXEC SORTD                                                   
//*                                                                     
//* ***    CLASSIFICA POR CGC-CPF                                       
//* ***    NATUREZA / AGENCIA                                           
//*                                                                     
//SORTIN   DD DSN=*.STEP04T.SAICDSER,                                   
//       DISP=SHR                                                       
//         DD DSN=*.STEP04T1.SAICDSER,                                  
//       DISP=SHR                                                       
//SORTOUT  DD DSN=MX.CLLP.CDSERASA.AVALCL2.SA(+1),                      
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO,02),                                               
//       SPACE=(TRK,(002230,0446),RLSE),                                
//       DCB=(MX.A,LRECL=0500,RECFM=FB)                                 
//SYSIN    DD *                                                         
 SORT FIELDS=(1,5,PD,A,6,3,PD,A,9,2,PD,A,11,2,CH,A,21,3,PD,A,           
           24,4,PD,A,28,3,CH,A,31,4,PD,A)                               
 END                                                                    
//*                                                                     
//STEP04X  EXEC PGM=CLLP7149                                            
//SEANTAVA DD DSN=*.STEP04V.SORT.SORTOUT,                               
//       DISP=OLD                                                       
//SEATUAVA DD DSN=*.STEP04W.SORT.SORTOUT,                               
//       DISP=OLD                                                       
//SEIEAVA  DD DSN=MX.CLLP.CDSERASA.AVAL.SELEC(+1),                      
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(000640,0128),RLSE),                                
//       DCB=(MX.A,LRECL=0500,RECFM=FB)                                 
//TOTAIS   DD SYSOUT=%%FORM#STH7,                                       
//       DCB=(LRECL=0080,RECFM=FBA)                                     
//SYSOUT   DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=Y                                                  
//*                                                                     
//STEP04A  EXEC SORTD                                                   
//*                                                                     
//* ***    CLASSIFICA POR AGENCIA E CONTA/CORRENTE                      
//*                                                                     
//SORTIN   DD DSN=*.STEP04X.SEIEAVA,                                    
//       DISP=SHR                                                       
//SORTOUT  DD DSN=MX.CLLP.CDSERASA.CLASS2.SA(+1),                       
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(000640,0128),RLSE),                                
//       DCB=(MX.A,LRECL=0500,RECFM=FB)                                 
//SORTWK01 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK02 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK03 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK04 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK05 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK06 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SYSIN    DD *                                                         
 SORT FIELDS=(21,3,A,24,4,A),FORMAT=BI                                  
 END                                                                    
//*                                                                     
//STEP04B  EXEC PGM=CLLP7151                                            
//*                                                                     
//* ***    LISTA AVALISTAS INSCRITOS NO S E R A S A'                    
//*                                                                     
//AVALSERA DD DSN=*.STEP04A.SORT.SORTOUT,                               
//       DISP=OLD                                                       
//RELAVASE DD SYSOUT=%%FORM#MID6,                                       
//       DCB=(LRECL=0131,RECFM=FBA)                                     
//SYSOUT   DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=Y                                                  
//*                                                                     
//STEP05   EXEC PGM=CLLP7156,                                           
//       PARM=0237                                                      
//*                                                                     
//* ***    LISTA ARQUIVO A SER ENVIADO PARA 'S E R A S A'               
//*                                                                     
//FTSER782 DD DSN=*.STEP04U.SORT.SORTOUT,                               
//       DISP=OLD                                                       
//RELATO   DD SYSOUT=%%FORM#HP36,                                       
//       DCB=(LRECL=0133,RECFM=FBA)                                     
//SYSOUT   DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=Y                                                  
//*                                                                     
//STEP05A  EXEC PGM=CLLP7156,                                           
//       PARM=0204                                                      
//*                                                                     
//* ***    LISTA ARQUIVO A SER ENVIADO PARA 'S E R A S A'               
//*                                                                     
//FTSER782 DD DSN=*.STEP04U1.SORT.SORTOUT,                              
//       DISP=OLD                                                       
//RELATO   DD SYSOUT=%%FORM#HHBV,                                       
//       DCB=(LRECL=0133,RECFM=FBA)                                     
//SYSOUT   DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=Y                                                  
//*                                                                     
//STEP06   EXEC SORTD                                                   
//*                                                                     
//* ***    CLASSIFICA POR TIPO REG. AGENCIA/CONTA/NATUREZA/CONTRATO/    
//* ***    NUM-DOC-AVAL                                                 
//*                                                                     
//SORTIN   DD DSN=*.STEP04.ARQTEMP,                                     
//       DISP=OLD                                                       
//         DD DSN=*.STEP04Z.ARQTEMP,                                    
//       DISP=OLD                                                       
//SORTOUT  DD DSN=MX.CLLP.ARQTEMP.CLASS(+1),                            
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO,10),                                               
//       SPACE=(TRK,(020000,4000),RLSE),                                
//       DCB=(MX.A,LRECL=0537,RECFM=FB)                                 
//SYSIN    DD *                                                         
 SORT FIELDS=(2,1,CH,A,514,3,PD,A,517,4,PD,A,511,2,CH,A,521,4,PD,A,     
           58,15,CH,A)                                                  
 END                                                                    
//SORTWK01 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK02 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK03 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK04 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK05 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK06 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//*                                                                     
//STEP07   EXEC PGM=CLLP7672                                            
//*                                                                     
//* ***     EMITE RELATORIO DOS CLIENTES ENVIADOS PARA SERASA           
//*                                                                     
//ENTRADA  DD DSN=*.STEP06.SORT.SORTOUT,                                
//       DISP=(OLD,DELETE,KEEP)                                         
//RELATO   DD SYSOUT=%%FORM#HP37,                                       
//       DCB=(LRECL=0133,RECFM=FBA)                                     
//SYSOUT   DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=Y                                                  
//*                                                                     
//STEP08   EXEC PGM=CLLP7146                                            
//*                                                                     
//* ***    LISTA DEVEDORES E SEUS AVALISTAS                             
//*                                                                     
//* ***    !!!!!!!!!!   ATENCAO !!!!!!!!!!!                             
//* ***    SE CASO FOR REATIVAR A LISTAGEM DESSE STEP,                  
//* ***    SERA NECESSARIO GERAR UM ARQUIVO E CRIAR UM                  
//* ***    STEP SEGUINTE PARA GERAR O RELATORIO(DEVIDO                  
//* ***    AO PARAMETRO OUTLIM)                                         
//* ***    !!!!!!!!!!   ATENCAO !!!!!!!!!!!                             
//*                                                                     
//SEATUDEV DD DSN=*.STEP04S.SAICDSER,                                   
//       DISP=SHR                                                       
//SEATUAVA DD DSN=*.STEP04T.SAICDSER,                                   
//       DISP=SHR                                                       
//CADAVAL  DD DSN=MX.CLLP.CADAVAL.SA(+1),                               
//       DISP=(,CATLG,CATLG),                                           
//       UNIT=(DISCO,02),                                               
//       SPACE=(TRK,(001550,0310),RLSE),                                
//       DCB=(MX.A,LRECL=0420,RECFM=FB)                                 
//RELATO   DD DUMMY                                                     
//*#RELATO   DD SYSOUT=(,,WSF2),                                        
//*#       OUTLIM=4900000,                                              
//*#       DCB=(LRECL=133,RECFM=FBA),                                   
//*#       OUTPUT=(*.OUT4,*.OUT5)                                       
//SYSOUT   DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=Y                                                  
//*                                                                     
//STEP09   EXEC SORTD                                                   
//*                                                                     
//* ***      CLASSIFICQ POR CPF AVA / NATUREZA / CPF DEV                
//*                                                                     
//SORTIN   DD DSN=*.STEP08.CADAVAL,                                     
//       DISP=SHR                                                       
//SORTOUT  DD DSN=MX.CLLP.CADAVAL.CLASS.SA(+1),                         
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO,02),                                               
//       SPACE=(TRK,(001550,0310),RLSE),                                
//       DCB=(MX.A,LRECL=0420,RECFM=FB)                                 
//SYSIN    DD *                                                         
 SORT FIELDS=(1,10,BI,A,192,2,BI,A,182,10,BI,A)                         
 END                                                                    
//*                                                                     
//STEP10   EXEC PGM=CLLP7148                                            
//*                                                                     
//* ***    LISTA AVALISTAS E SEUS DEVEDORES                             
//*                                                                     
//ARQAVACT DD DSN=*.STEP09.SORT.SORTOUT,                                
//       DISP=SHR                                                       
//RELATO   DD SYSOUT=%%FORM#HP61,                                       
//       DCB=(LRECL=133,RECFM=FBA)                                      
//SYSOUT   DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=Y                                                  
//*                                                                     
//STEP11   EXEC PGM=CLLP7150                                            
//*                                                                     
//* ***     EMITE RELATORIO DIARIA DE INCONSISTENCIA POR MOTIVO DE      
//* ***     ENDERECO INCORRETO NO CLIE/CEPN                             
//*                                                                     
//MESTAG   DD DSN=MX.CLLP.MESTAGEN.NOVO(0),                             
//       DISP=SHR                                                       
//INCLIEAT DD DSN=MX.CLLP.INAVADEV.SA(0),                               
//       DISP=SHR                                                       
//INCLIEAN DD DUMMY                                                     
//RELATO   DD SYSOUT=%%FORM#HQ96,                                       
//       DCB=(LRECL=0132,RECFM=FBA)                                     
//SYSOUT   DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=Y                                                  
//*                                                                     
//STEP12   EXEC PGM=CLLP7150                                            
//*                                                                     
//* ***    EMITE RELATORIO DIARIA DE INCONSISTENCIA POR MOTIVO DE       
//* ***    ENDERECO INCORRETO NO CLIE/CEPN                              
//*                                                                     
//MESTAG   DD DSN=MX.CLLP.MESTAGEN.NOVO(0),                             
//       DISP=SHR                                                       
//INCLIEAT DD DSN=MX.CLLP.INAVADEV.SA(0),                               
//       DISP=SHR                                                       
//INCLIEAN DD DUMMY                                                     
//RELATO   DD SYSOUT=%%FORM#DT69,                                       
//       DCB=(LRECL=0132,RECFM=FBA)                                     
//SYSOUT   DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=Y                                                  
//*                                                                     
//STEP13   EXEC PGM=CLLP7150                                            
//*                                                                     
//* ***    EMITE RELATORIO DIARIA DE INCONSISTENCIA POR MOTIVO DE       
//* ***    ENDERECO INCORRETO NO CLIE/CEPN                              
//*                                                                     
//MESTAG   DD DSN=MX.CLLP.MESTAGEN.NOVO(0),                             
//       DISP=SHR                                                       
//INCLIEAT DD DSN=MX.CLLP.INAVADEV.SA(0),                               
//       DISP=SHR                                                       
//INCLIEAN DD DUMMY                                                     
//RELATO   DD SYSOUT=%%FORM#HP68,                                       
//       DCB=(LRECL=0132,RECFM=FBA)                                     
//SYSOUT   DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=Y                                                  
//*                                                                     
//STEP14   EXEC PGM=PLAN1010                                            
//*                                                                     
//* ***    CRIACAO DO MX.CLLP.RDAB.CDSERASA A SER LIDO JO JOB RDAB009   
//*                                                                     
//SYSUT1   DD DSN=*.STEP04S.SAICDSER,                                   
//       DISP=SHR                                                       
//SYSUT2   DD DSN=MX.CLLP.RDAB.CDSERASA,                                
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO,50),                                               
//       SPACE=(TRK,(020000,5000),RLSE),                                
//       DCB=(LRECL=0500,RECFM=FB)                                      
//PRINTER  DD SYSOUT=*                                                  
//SYSOUT   DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=Y                                                  
//*                                                                     
//STEP15   EXEC SORTD                                                   
//*                                                                     
//* ***    *********************************************************    
//* ***    CLASSIFICA O ARQUIVOS DE INCLUSOES NA SERASA:                
//* ***       AGENCIA, E DATA DE MOVTO                                  
//* ***    *********************************************************    
//*                                                                     
//SORTIN   DD DSN=*.STEP04.ARQTEMP,                                     
//       DISP=SHR                                                       
//SORTOUT  DD DSN=MX.CLLP.MVTO.SERASA.DIARIO.CLAS(+1),                  
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO,10),                                               
//       SPACE=(TRK,(020000,4000),RLSE),                                
//       DCB=(MX.A,LRECL=0537,RECFM=FB)                                 
//SORTWK01 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK02 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK03 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK04 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK05 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SORTWK06 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(0300))                                             
//SYSIN    DD *                                                         
 SORT FIELDS=(514,3,PD,A,530,5,PD,A)                                    
 END                                                                    
//*                                                                     
//STEP16   EXEC PGM=CLLP0880                                            
//*                                                                     
//* ***    *********************************************************    
//* ***    GERA MOVIMENTO DIARIO DE INCLUSOES NO SERASA,                
//* ***    POR DIA E AGENCIA                                            
//* ***    *********************************************************    
//*                                                                     
//MVSERASA DD DSN=*.STEP15.SORT.SORTOUT,                                
//       DISP=SHR                                                       
//MOVIMDIA DD DSN=MX.CLLP.MOVTO.DIARIO.SERASA(+1),                      
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(000010,0002),RLSE),                                
//       DCB=(MX.A,LRECL=0020,RECFM=FB)                                 
//SYSOUT   DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=Y                                                  
//*                                                                     
//* ***    *********************************************************    
//* ***    ACUMULA MOVIMENTO DIARIO DE INCLUSOES NO SERASA,             
//* ***    PARA O MES                                                   
//* ***    *********************************************************    
//*                                                                     
//*                                                                     
//STEP17   EXEC PGM=PLAN1010                                            
//SYSUT1   DD DSN=*.STEP16.MOVIMDIA,                                    
//       DISP=SHR                                                       
//         DD DSN=MX.CLLP.PRV.SERASA.ACUMULAD(0),                       
//       DISP=SHR                                                       
//SYSUT2   DD DSN=MX.CLLP.PRV.SERASA.ACUMULAD(+1),                      
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(000050,0010),RLSE),                                
//       DCB=(MX.A,LRECL=0020,RECFM=FB)                                 
//PRINTER  DD SYSOUT=*                                                  
//SYSOUT   DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=*                                                  
//*                                                                     
//STEP18   EXEC PGM=CLLP7099                                            
//*                                                                     
//* ***    CRIA ARQUIVO DE RESTRICAO PARA RECR                          
//*                                                                     
//TABELAL  DD DSN=MX.CLLP.ARQNSEL.CLLP0037.CLAS(0),                     
//       DISP=SHR                                                       
//SEDIADEV DD DSN=*.STEP01.SORT.SORTOUT,                                
//       DISP=OLD                                                       
//SEDIAAVA DD DSN=*.STEP02.SORT.SORTOUT,                                
//       DISP=OLD                                                       
//BLQREST  DD DSN=MX.CLLP.J1070S18.BLQREST(+1),                         
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(010000,0010),RLSE),                                
//       DCB=(MX.A,LRECL=0040,RECFM=FB)                                 
//SYSOUT   DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=Y                                                  
//*                                                                     

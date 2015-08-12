//CLLPA405 JOB 'CLLP,4120,PR32','D115848',MSGCLASS=Z,REGION=4M          
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
//* ***    %%LIBSYM MJ.EVTA.PRV.CAFORM %%MEMSYM OUTYDN2                 
//* ***    %%LIBSYM MJ.EVTA.PRV.CAFORM %%MEMSYM OUTAAET                 
//* ***    %%LIBSYM MJ.EVTA.PRV.CAFORM %%MEMSYM OUTAAEU                 
//* ***    %%LIBSYM MJ.EVTA.PRV.CAFORM %%MEMSYM OUTAAEV                 
//* ***    %%LIBSYM MJ.EVTA.PRV.CAFORM %%MEMSYM OUTAAEW                 
//* ***    %%LIBSYM MJ.EVTA.PRV.CAFORM %%MEMSYM OUTAAEY                 
//* ***    %%LIBSYM MJ.EVTA.PRV.CAFORM %%MEMSYM OUTAAEX                 
//* ***    %%LIBSYM MJ.EVTA.PRV.CAFORM %%MEMSYM OUTLJYA                 
//* ***    %%LIBSYM MJ.EVTA.PRV.CAFORM %%MEMSYM OUTLJYB                 
//* ***    %%LIBSYM MJ.EVTA.PRV.CAFORM %%MEMSYM OUTJJ30                 
//*                                                                     
//JOBLIB   DD DSN=MX.BIBGERAL,DISP=SHR                                  
//         DD DSN=SYS1.CEE.SCEERUN,DISP=SHR                             
//STEP0    EXEC SORTD                                                   
//*                                                                     
//* ***    SEPARAR DO ARQUIVO SEQUENCIAL DA TABELA 37                   
//* ***    QUEM TEM UNIDADE FEDERAL                                     
//*                                                                     
//SORTIN   DD DSN=MX.CLLP.LPCLB037.JA401.CLAS(0),                       
//       DISP=SHR                                                       
//SORTOUT1 DD DSN=MX.CLLP.LPCLB037.UFCATCI.LEI(+1),                     
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(000010,0002),RLSE),                                
//       DCB=(MX.A,LRECL=0043,RECFM=FB)                                 
//SORTOUT2 DD DSN=MX.CLLP.LPCLB037.UFNATCI.LEI(+1),                     
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(000010,0002),RLSE),                                
//       DCB=(MX.A,LRECL=0043,RECFM=FB)                                 
//SORTOUT3 DD DSN=MX.CLLP.LPCLB037.COMUF.LEI(+1),                       
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(000010,0002),RLSE),                                
//       DCB=(MX.A,LRECL=0043,RECFM=FB)                                 
//SYSIN    DD *                                                         
 SORT FIELDS=COPY                                                       
 OUTFIL OUTREC=(1,43),                                                  
        INCLUDE=(24,2,CH,NE,C'00',AND,(21,3,CH,EQ,C'502',OR,            
                 21,3,CH,EQ,C'572',OR,21,3,CH,EQ,C'590',OR,             
                 21,3,CH,EQ,C'532',OR,                                  
                 21,3,CH,EQ,C'561')),FNAMES=SORTOUT1                    
 OUTFIL OUTREC=(1,43),                                                  
        INCLUDE=(24,2,CH,NE,C'00',AND,(19,2,PD,EQ,75,OR,                
                 19,2,PD,EQ,76,OR,19,2,PD,EQ,78,OR,19,2,PD,EQ,84,OR,    
                 19,2,PD,EQ,88,OR,19,2,PD,EQ,89)),FNAMES=SORTOUT2       
 OUTFIL OUTREC=(1,43),                                                  
        INCLUDE=(24,2,CH,NE,C'00',AND,(21,3,CH,EQ,C'000',AND,           
                 19,2,PD,EQ,00)),FNAMES=SORTOUT3                        
 END                                                                    
//*                                                                     
//STEP0A   EXEC SORTD                                                   
//*                                                                     
//* ***    JUNTAR QUEM TEM UF E CLASSIFICAR POR UF / NAT / CART         
//*                                                                     
//SORTIN   DD DSN=*.STEP0.SORT.SORTOUT1,                                
//       DISP=OLD                                                       
//         DD DSN=*.STEP0.SORT.SORTOUT2,                                
//       DISP=OLD                                                       
//         DD DSN=*.STEP0.SORT.SORTOUT3,                                
//       DISP=OLD                                                       
//SORTOUT  DD DSN=MX.CLLP.LPCLB037.CICLAS.LEI(+1),                      
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(000010,0002),RLSE),                                
//       DCB=(MX.A,LRECL=0043,RECFM=FB)                                 
//SYSIN    DD *                                                         
 SORT FIELDS=(24,2,CH,A,19,2,PD,A,21,3,CH,A)                            
 END                                                                    
//*                                                                     
//STEP1    EXEC SORTD                                                   
//*                                                                     
//* ***    MERGE DEVEORES DA ZIPCODE E AFINDER                          
//* ***    CPF / CNPJ                                                   
//*                                                                     
//SORTIN   DD DSN=MX.CLLP.PRV.MVTOEMIS.DEVEDOR(0),                      
//       DISP=SHR                                                       
//         DD DSN=MX.CLLP.PRV.MVTOEMIS.DEVEDOR.AF(0),                   
//       DISP=SHR                                                       
//SORTOUT  DD DSN=MX.CLLP.PRV.DEVEDOR.ZCAF(+1),                         
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO,02),                                               
//       SPACE=(TRK,(001080,0216),RLSE),                                
//       DCB=(MX.A,LRECL=300,RECFM=FB)                                  
//SYSIN    DD *                                                         
 SORT FIELDS=(29,05,PD,A,34,03,PD,A,37,02,PD,A)                         
 END                                                                    
//*                                                                     
//STEP2    EXEC PGM=CLLPH670                                            
//*                                                                     
//* ***    RETORNO DA EMPRESA ZIPCODE                                   
//* ***    GERAR MOVIMENTO PARA EMISSAO DE AVISOS                       
//* ***    COM PENDENCIAS NO LPCL                                       
//*                                                                     
//MVTOEMIE DD DSN=*.STEP1.SORT.SORTOUT,                                 
//       DISP=SHR                                                       
//DBSECARG DD DSN=MX.LPCL.DBSECARG.CPFCLMS(0),                          
//       DISP=SHR                                                       
//MVTOEMIS DD DSN=MX.CLLP.MVTOEMIS.CLLPH670(+1),                        
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(000980,0196),RLSE),                                
//       DCB=(MX.A,LRECL=0300,RECFM=FB)                                 
//DBSECARS DD DSN=MX.CLLP.DBSECARS.CLLPH670(+1),                        
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO,02),                                               
//       SPACE=(TRK,(004350,0870),RLSE),                                
//       DCB=(MX.A,LRECL=0690,RECFM=FB)                                 
//MVTONENC DD DSN=MX.CLLP.MVTONENC.CLLPH670(+1),                        
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(000980,0196),RLSE),                                
//       DCB=(MX.A,LRECL=0300,RECFM=FB)                                 
//RELATO   DD SYSOUT=%%FORM#YDN2,                                       
//       DCB=(LRECL=080,RECFM=FBA)                                      
//SYSOUT   DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=Y                                                  
//*                                                                     
//STEP2A   EXEC SORTD                                                   
//*                                                                     
//* ***     SEPARAR DO CADASTRO CREDITO IMOBILIARIO E CLASSIFICAR       
//* ***     POR AGENCIA / CONTA / CONTRATO / DATA DE VENCIMENTO         
//*                                                                     
//SORTIN   DD DSN=*.STEP2.DBSECARS,                                     
//       DISP=OLD                                                       
//SORTOUT1 DD DSN=MX.CLLP.DBSECARS.DIFCI.LEI(+1),                       
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO,02),                                               
//       SPACE=(TRK,(004340,0868),RLSE),                                
//       DCB=(MX.A,LRECL=0690,RECFM=FB)                                 
//SORTOUT2 DD DSN=MX.CLLP.DBSECARS.FBNHCI.LEI(+1),                      
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(000010,0002),RLSE),                                
//       DCB=(MX.A,LRECL=0690,RECFM=FB)                                 
//SORTOUT3 DD DSN=MX.CLLP.DBSECARS.DCIRCI.LEI(+1),                      
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(000020,0004),RLSE),                                
//       DCB=(MX.A,LRECL=0690,RECFM=FB)                                 
//SYSIN    DD *                                                         
 SORT FIELDS=(4,3,PD,A,16,5,CH,A,24,4,PD,A,34,4,CH,A,31,2,CH,A,         
              28,2,CH,A)                                                
 OUTFIL OUTREC=(1,690),                                                 
        INCLUDE=(21,3,CH,NE,C'502',AND,21,3,CH,NE,C'572',               
                 AND,21,3,CH,NE,C'590',AND,21,3,CH,NE,C'532',           
                 AND,21,3,CH,NE,C'561'),FNAMES=SORTOUT1                 
 OUTFIL OUTREC=(1,690),                                                 
        INCLUDE=(21,3,CH,EQ,C'502',OR,21,3,CH,EQ,C'572',                
                 OR,21,3,CH,EQ,C'590'),FNAMES=SORTOUT2                  
 OUTFIL OUTREC=(1,690),                                                 
        INCLUDE=(21,3,CH,EQ,C'532',                                     
                 OR,21,3,CH,EQ,C'561'),FNAMES=SORTOUT3                  
 END                                                                    
//*                                                                     
//STEP2B   EXEC PGM=CLLPG614,                                           
//       PARM='FBNH'                                                    
//*                                                                     
//* ***    VERIFICAR SE REGISTRO DE CREDITO IMOBILIARIO DO FBNH         
//* ***    EXISTE NO ARQUIVO GERADO PELO FBNH E SE A UF DO IMOVEL       
//* ***    ESTA OK                                                      
//*                                                                     
//ARQENT   DD DSN=MX.CLLP.PRV.ENDERECO.FBNH(0),                         
//       DISP=SHR                                                       
//ARQDB2   DD DSN=*.STEP2A.SORT.SORTOUT2,                               
//       DISP=SHR                                                       
//ARQSAIDA DD DSN=MX.CLLP.DBSECARS.FBNH.CLG614(+1),                     
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(000010,0002),RLSE),                                
//       DCB=(MX.A,LRECL=0692,RECFM=FB)                                 
//RELATO   DD SYSOUT=%%FORM#AAET,                                       
//       DCB=(LRECL=132,RECFM=FBA)                                      
//RELINCO  DD SYSOUT=%%FORM#AAEU,                                       
//       DCB=(LRECL=132,RECFM=FBA)                                      
//SYSOUT   DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=Y                                                  
//*                                                                     
//STEP2C   EXEC PGM=CLLPG614,                                           
//       PARM='DCIR'                                                    
//*                                                                     
//* ***    VERIFICAR SE REGISTRO DE CREDITO IMOBILIARIO DO DCIR         
//* ***    EXISTE NO ARQUIVO GERADO PELO DCIR E SE A UF DO IMOVEL       
//* ***    ESTA OK                                                      
//*                                                                     
//ARQENT   DD DSN=MX.CLLP.PRV.ENDERECO.DCIR(0),                         
//       DISP=SHR                                                       
//ARQDB2   DD DSN=*.STEP2A.SORT.SORTOUT3,                               
//       DISP=SHR                                                       
//ARQSAIDA DD DSN=MX.CLLP.DBSECARS.DCIR.CLG614(+1),                     
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(000020,0004),RLSE),                                
//       DCB=(MX.A,LRECL=0692,RECFM=FB)                                 
//RELATO   DD SYSOUT=%%FORM#AAEV,                                       
//       DCB=(LRECL=132,RECFM=FBA)                                      
//RELINCO  DD SYSOUT=%%FORM#AAEW,                                       
//       DCB=(LRECL=132,RECFM=FBA)                                      
//SYSOUT   DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=Y                                                  
//*                                                                     
//STEP2D   EXEC SORTD                                                   
//*                                                                     
//* ***    JUNTAR ARQUIVOS DE AVISOS DO FBNH E DCIR E                   
//* ***    CLASSIFICAR POR AGENCIA / CONTA / CONTRATO                   
//* ***    VENCIMENTO / UF DO IMOVEL / NATUREZA / CARTEIRA              
//*                                                                     
//SORTIN   DD DSN=*.STEP2B.ARQSAIDA,                                    
//       DISP=OLD                                                       
//         DD DSN=*.STEP2C.ARQSAIDA,                                    
//       DISP=OLD                                                       
//SORTOUT  DD DSN=MX.CLLP.DBSECARS.CITOT.CLAS.LEI(+1),                  
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(000020,0004),RLSE),                                
//       DCB=(MX.A,LRECL=0692,RECFM=FB)                                 
//SYSIN    DD *                                                         
 SORT FIELDS=(4,3,PD,A,16,5,CH,A,24,4,PD,A,34,4,CH,A,31,2,CH,A,         
              28,2,CH,A,691,2,CH,A,191,3,CH,A,21,3,CH,A)                
 END                                                                    
//*                                                                     
//STEP2E   EXEC PGM=CLLPG612                                            
//*                                                                     
//* ***    VERIFICAR SE EXISTE BLOQUEIO POR UNIDADE FEDERAL NA TABELA 37
//*                                                                     
//ARQAVISO DD DSN=*.STEP2D.SORT.SORTOUT,                                
//       DISP=OLD                                                       
//TABELA   DD DSN=*.STEP0A.SORT.SORTOUT,                                
//       DISP=OLD                                                       
//ARQSAIDA DD DSN=MX.CLLP.DBSECARS.CLLPG612(+1),                        
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(000020,0004),RLSE),                                
//       DCB=(MX.A,LRECL=0690,RECFM=FB)                                 
//ARQBLOQ  DD DSN=MX.CLLP.ARQBLOQ.CLLPG612(+1),                         
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(000010,0002),RLSE),                                
//       DCB=(MX.A,LRECL=0692,RECFM=FB)                                 
//BLQREST  DD DSN=MX.CLLP.JA405S2E.BLQREST(+1),                         
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(001000,0100),RLSE),                                
//       DCB=(MX.A,LRECL=0040,RECFM=FB)                                 
//RELATO   DD SYSOUT=%%FORM#AAEY,                                       
//       DCB=(LRECL=132,RECFM=FBA)                                      
//RELINCO  DD SYSOUT=%%FORM#AAEX,                                       
//       DCB=(LRECL=132,RECFM=FBA)                                      
//SYSOUT   DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=Y                                                  
//*                                                                     
//STEP3    EXEC SORTD                                                   
//*                                                                     
//* ***    CLASSIFICAR ARQUIVO DBSECARS POR CPF                         
//*                                                                     
//SORTIN   DD DSN=*.STEP2E.ARQSAIDA,                                    
//       DISP=OLD                                                       
//         DD DSN=*.STEP2A.SORT.SORTOUT1,                               
//       DISP=OLD                                                       
//SORTOUT  DD DSN=MX.CLLP.DBSECARS.CLAS.LEI(+1),                        
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO,02),                                               
//       SPACE=(TRK,(004350,0870),RLSE),                                
//       DCB=(MX.A,LRECL=690,RECFM=FB)                                  
//SYSIN    DD *                                                         
 SORT FIELDS=(4,3,PD,A,16,4,PD,A,21,3,CH,A,24,4,PD,A)                   
 END                                                                    
//SORTLIB  DD DSN=SYS1.SORTLIB,                                         
//       DISP=SHR                                                       
//SORTWK01 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00350))                                            
//SORTWK02 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00350))                                            
//SORTWK03 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00350))                                            
//SORTWK04 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00350))                                            
//SORTWK05 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00350))                                            
//SORTWK06 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00350))                                            
//SORTWK07 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00350))                                            
//SORTWK08 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00350))                                            
//SORTWK09 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00350))                                            
//SORTWK10 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00350))                                            
//SORTWK11 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00350))                                            
//SORTWK12 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00350))                                            
//SORTWK13 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00350))                                            
//SORTWK14 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00350))                                            
//SORTWK15 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00350))                                            
//SORTWK16 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00350))                                            
//SORTWK17 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00350))                                            
//SORTWK18 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00350))                                            
//SORTWK19 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00350))                                            
//SORTWK20 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00350))                                            
//SORTWK21 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00350))                                            
//SORTWK22 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00350))                                            
//SORTWK23 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00350))                                            
//SORTWK24 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00350))                                            
//SORTWK25 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00350))                                            
//SORTWK26 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00350))                                            
//SORTWK27 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00350))                                            
//SORTWK28 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00350))                                            
//SORTWK29 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00350))                                            
//SORTWK30 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00350))                                            
//SORTWK31 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00350))                                            
//SORTWK32 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00350))                                            
//SYSOUT   DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=Y                                                  
//*                                                                     
//STEP3A   EXEC PGM=CLLPG659                                            
//*                                                                     
//* ***    RETIRAR DO MOVIMENTO DE AVISOS CONTRATOS MIGRADOS POR CPF    
//*                                                                     
//CONTMIGR DD DSN=MX.CLLP.PRV.CONTRMIG.NEGATI(0),                       
//       DISP=SHR                                                       
//ARQAVISO DD DSN=*.STEP3.SORT.SORTOUT,                                 
//       DISP=SHR                                                       
//AVINMIGR DD DSN=MX.CLLP.JA405S3A.AVINMIGR(+1),                        
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO,02),                                               
//       SPACE=(TRK,(003370,0674),RLSE),                                
//       DCB=(MX.A,LRECL=0690,RECFM=FB)                                 
//AVIMIGR  DD DSN=MX.CLLP.JA405S3A.AVIMIGR(+1),                         
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO,02),                                               
//       SPACE=(TRK,(002280,0456),RLSE),                                
//       DCB=(MX.A,LRECL=0690,RECFM=FB)                                 
//RELCONS  DD SYSOUT=%%FORM#LJYA,                                       
//       DCB=(LRECL=0133,RECFM=FBA)                                     
//RELINCON DD SYSOUT=%%FORM#LJYB,                                       
//       DCB=(LRECL=0133,RECFM=FBA)                                     
//SYSOUT   DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=Y                                                  
//*                                                                     
//STEP3B   EXEC SORTD                                                   
//*                                                                     
//* ***    CLASSIFICAR ARQUIVO DBSECARS POR                             
//* ***    AGENCIA, CONTA, CARTEIRA, CONTRATO E DATA DE VENCIMENTO      
//*                                                                     
//SORTIN   DD DSN=*.STEP3A.AVINMIGR,                                    
//       DISP=OLD                                                       
//SORTOUT  DD DSN=MX.CLLP.JA405S3B.DBSECARS.CLAS(+1),                   
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=(DISCO,02),                                               
//       SPACE=(TRK,(004350,0870),RLSE),                                
//       DCB=(MX.A,LRECL=690,RECFM=FB)                                  
//SYSIN    DD *                                                         
 SORT FIELDS=(4,3,A,16,5,A,21,3,A,24,4,A,34,4,A,31,2,A,28,2,A,140,8,A), 
        FORMAT=BI                                                       
 END                                                                    
//SORTLIB  DD DSN=SYS1.SORTLIB,                                         
//       DISP=SHR                                                       
//SORTWK01 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00350))                                            
//SORTWK02 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00350))                                            
//SORTWK03 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00350))                                            
//SORTWK04 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00350))                                            
//SORTWK05 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00350))                                            
//SORTWK06 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00350))                                            
//SORTWK07 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00350))                                            
//SORTWK08 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00350))                                            
//SORTWK09 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00350))                                            
//SORTWK10 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00350))                                            
//SORTWK11 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00350))                                            
//SORTWK12 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00350))                                            
//SORTWK13 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00350))                                            
//SORTWK14 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00350))                                            
//SORTWK15 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00350))                                            
//SORTWK16 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00350))                                            
//SORTWK17 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00350))                                            
//SORTWK18 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00350))                                            
//SORTWK19 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00350))                                            
//SORTWK20 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00350))                                            
//SORTWK21 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00350))                                            
//SORTWK22 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00350))                                            
//SORTWK23 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00350))                                            
//SORTWK24 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00350))                                            
//SORTWK25 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00350))                                            
//SORTWK26 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00350))                                            
//SORTWK27 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00350))                                            
//SORTWK28 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00350))                                            
//SORTWK29 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00350))                                            
//SORTWK30 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00350))                                            
//SORTWK31 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00350))                                            
//SORTWK32 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00350))                                            
//SYSOUT   DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=Y                                                  
//*                                                                     
//STEP4    EXEC PGM=CLLPG610                                            
//*                                                                     
//* ***    GERA CADALPCL DOS VENCIDOS DA 1A., 2A. E 3A. CARTA           
//* ***    E ID = MO, A PARTIR DO ARQDB2 (LPCLB000 SEQUENCIAL)          
//* ***    (GRAVA ARQUIVO COMPRIMIDO)                                   
//*                                                                     
//ARQDB2   DD DSN=*.STEP3B.SORT.SORTOUT,                                
//       DISP=SHR                                                       
//CADALPCL DD DSN=MX.CLLP.CADAL7AV.LEI(+1),                             
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(000540,0108),RLSE),                                
//       DCB=(MX.A,LRECL=0700,RECFM=FB)                                 
//TOTAIS   DD SYSOUT=%%FORM#JJ30,                                       
//       DCB=(LRECL=0080,RECFM=FBA)                                     
//SYSOUT   DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=Y                                                  
//*                                                                     
//STEP5    EXEC PGM=POOL0260                                            
//*                                                                     
//* ***    CLASSIFICA O CADASTRO SELECIONADO, EM ORDEM DE               
//* ***    EMPRESA, AGENCIA, NUMERO CL E DATA VCTO.                     
//* ***    SORT MERGE (ARQUIVOS COMPRIMIDOS) MO E CL/LP.                
//*                                                                     
//SORTIN   DD DSN=*.STEP4.CADALPCL,                                     
//       DISP=OLD                                                       
//*#//         DD DSN=MX.CLLP.S073735.CADATIVO.JOB225(0),               
//*#//       DISP=SHR                                                   
//SORTOUT  DD DSN=MX.CLLP.CADADAAV.LEI(+1),                             
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(000480,0096),RLSE),                                
//       DCB=(MX.A,LRECL=0700,RECFM=FB)                                 
//SYSIN    DD *                                                         
 SORT FIELDS=(1,3,PD,A,4,3,PD,A,51.5,2,BI,A,50.5,1,BI,A,49.5,1,BI,A)    
 END                                                                    
//SORTWK01 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00450))                                            
//SORTWK02 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00450))                                            
//SORTWK03 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00450))                                            
//SORTWK04 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00450))                                            
//SORTWK05 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00450))                                            
//SORTWK06 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00450))                                            
//SORTWK07 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00450))                                            
//SORTWK08 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00450))                                            
//SORTWK09 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00450))                                            
//SORTWK10 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00450))                                            
//SORTWK11 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00450))                                            
//SORTWK12 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00450))                                            
//SORTWK13 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00450))                                            
//SORTWK14 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00450))                                            
//SORTWK15 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00450))                                            
//SORTWK16 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00450))                                            
//SORTWK17 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00450))                                            
//SORTWK18 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00450))                                            
//SORTWK19 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00450))                                            
//SORTWK20 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00450))                                            
//SORTWK21 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00450))                                            
//SORTWK22 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00450))                                            
//SORTWK23 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00450))                                            
//SORTWK24 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00450))                                            
//SORTWK25 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00450))                                            
//SORTWK26 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00450))                                            
//SORTWK27 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00450))                                            
//SORTWK28 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00450))                                            
//SORTWK29 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00450))                                            
//SORTWK30 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00450))                                            
//SORTWK31 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00450))                                            
//SORTWK32 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00450))                                            
//SORTWK33 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00450))                                            
//SORTWK34 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00450))                                            
//SORTWK35 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00450))                                            
//SORTWK36 DD UNIT=DISCO,                                               
//       SPACE=(CYL,(00450))                                            
//SYSOUT   DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=Y                                                  
//*                                                                     
//STEPY    EXEC PGM=IEFBR14                                             
//*                                                                     
//* ***    ***********************************************************  
//* ***    * DELECOES DOS ARQUIVOS P/ LIBERACAO DOS SPACES EM DISCOS *  
//* ***    ***********************************************************  
//*                                                                     
//DD1      DD DSN=MX.LPCL.DBSECARG.CPFCLAS(0),                          
//       DISP=(OLD,DELETE,KEEP)                                         
//SYSOUT   DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=Y                                                  
//*                                                                     

//BOLPEAT  JOB 'PEAT,4510,PR41','G00347',MSGCLASS=Z,SCHENV=TEST         
//JOBLIB   DD DSN=AV.BIBGERTT,DISP=SHR                                  
//         DD DSN=AV.BIBGERAL,DISP=SHR                                  
//         DD DSN=SYS1.CEE.SCEERUN,DISP=SHR                             
//STEP1    EXEC SORTD                                                   
//*                                                                     
//* ***    CLASSIFICAR POR: AGE E CTA DE (ASCE)                         
//*                                                                     
//SORTIN   DD DSN=AD.BRQ.TEMP.J0043S5A.MOVDIAR.CLASS,                   
//       DISP=OLD                                                       
//SORTOUT  DD DSN=AD.BRQ.TEMP.PEAT1064.ICETOOL,                         
//       DISP=(OLD,CATLG,DELETE),                                       
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(0020000,08000),RLSE),                              
//       DCB=(AD.A,LRECL=0102,RECFM=FB)                                 
//SYSIN    DD *                                                         
 SORT FIELDS=(3,3,A,10,4,A,15,5,A,27,1,D,28,8,D),                       
         FORMAT=BI                                                      
 OUTFIL OUTREC=(1,102),                                                 
       INCLUDE=((3,3,PD,EQ,00002,AND,10,4,PD,EQ,0037960),OR,            
                (3,3,PD,EQ,00002,AND,10,4,PD,EQ,0051409),OR,            
                (3,3,PD,EQ,00002,AND,10,4,PD,EQ,0063692))               
 END                                                                    
//*                                                                     
//*                                                                     
//*************************************************************         
//* ICETOOL - GERA RELATORIO A PARTIR DE ARQUIVO SEQUENCIAL             
//*************************************************************         
//* EXPORTAR OS REGISTROS ABAIXO PARA O ARQ 'IN' (PARA TESTE)           
//*                                                                     
//*12222133331AAAAAAAAAA 20010719 27012005  0010001000000000001         
//*12222233332BBBBBBBBBB 19960616 01012005  0002002000000000002         
//*22222333333CCCCCCCCCC 20040129 09102004  3000000031000000003         
//*22222433334DDDDDDDDDD 20050101 30032004  0400004000000000004         
//*************************************************************         
//* NOST = SUPRIME TOTALIZACAO/MEDIA,ETC                                
//* BLANK = ELIMINA ZEROS A ESQUERDA                                    
//* STATLEFT = CABECALHO DE TOTAL EM DESTAQUE A ESQUERDA                
//* BETWEEN(0) = SUPRIME BRANCOS ENTRE AS COLUNAS, DEFAULT=3            
//* ON(42,07,ZD,C2,N07) = C2 EDITA VL , N07 EXIBE 7 DIGITOS             
//*************************************************************         
//*                                                                     
//*************************************************************         
//* TOTAL TESTE SUMARIZADO                                              
//*************************************************************         
//STEP2    EXEC PGM=ICETOOL                                             
//IN       DD DSN=*.STEP1.SORT.SORTOUT,                                 
//       DISP=SHR                                                       
//LISTA    DD SYSOUT=(W,,AM19)                                          
//DFSMSG   DD SYSOUT=*                                                  
//TOOLMSG  DD SYSOUT=*                                                  
//TOOLIN   DD *                                                         
 DISPLAY FROM(IN) LIST(LISTA) -                                         
 BLANK -                                                                
 BETWEEN(1) -                                                           
 TITLE('CONTAS SELECIONADAS DO MOVIMENTO DIARIO') -                     
 DATE(DM4/)   TIME   PAGE -                                             
 HEADER('BCO')             ON(001,02,PD,E'999',NOST) -                  
 HEADER('AG')              ON(003,03,PD,E'99999',NOST) -                
 HEADER('GRP')             ON(006,02,PD,E'999',NOST) -                  
 HEADER('SGRP')            ON(008,02,PD,E'999',NOST) -                  
 HEADER('CONTA')           ON(010,04,PD,E'9999999',NOST) -              
 HEADER('DIG')             ON(014,01,CH) -                              
 HEADER('DT MOV')          ON(015,05,PD,E'999999999',NOST) -            
 HEADER('LANC')            ON(020,03,PD,E'99999',NOST) -                
 HEADER('N DOC')           ON(023,04,PD,E'9999999',NOST) -              
 HEADER('DEB','CRED')      ON(027,01,CH) -                              
 HEADER('VALOR')           ON(028,08,PD,C2,N15) -                       
 HEADER('DIA','VIN')       ON(036,02,PD,E'999',NOST) -                  
 HEADER('CENT','CUST')     ON(038,04,CH) -                              
 HEADER('SUBC','CRS')      ON(042,03,PD,E'99999',NOST) -                
 HEADER('SERV')            ON(045,02,CH) -                              
 HEADER('TP','ENT')        ON(047,01,CH) -                              
 HEADER('SN')              ON(048,01,CH) -                              
 HEADER('ORIGEM')          ON(049,32,CH) -                              
 HEADER('DIA','LA')        ON(081,02,ZD,E'99',NOST) -                   
 HEADER('DT ORI')          ON(083,05,PD,E'999999999',NOST) -            
 HEADER('PROD')            ON(088,04,CH) -                              
 HEADER('OPER')            ON(092,07,CH) -                              
 HEADER('SEQ')             ON(099,03,CH)                                
//*                                                                     
//STEP3    EXEC PGM=PEAT1064                                            
//*                                                                     
//* ***    CARIMBA NO MOVIMENTO CONTA RAZAO 10.71                       
//*                                                                     
//MVTODIAE DD DSN=*.STEP1.SORT.SORTOUT,                                 
//       DISP=OLD                                                       
//ARQSALDO DD DSN=AD.BRQ.TEMP.J0043S05.ARQSALDO,                        
//       DISP=OLD                                                       
//MVTODIAS DD DSN=AD.BRQ.CHICAO.MVTODIAS.J0043.ICE,                     
//       DISP=(,CATLG,DELETE),                                          
//       UNIT=DISCO,                                                    
//       SPACE=(TRK,(0020000,08000),RLSE),                              
//       DCB=(AD.A,LRECL=0102,RECFM=FB)                                 
//SYSOUT   DD SYSOUT=*                                                  
//SYSUDUMP DD SYSOUT=Y                                                  
//*                                                                     
//STEP4    EXEC PGM=ICETOOL                                             
//IN       DD DSN=*.STEP3.MVTODIAS,                                     
//       DISP=SHR                                                       
//LISTA    DD SYSOUT=(W,,AM19)                                          
//DFSMSG   DD SYSOUT=*                                                  
//TOOLMSG  DD SYSOUT=*                                                  
//TOOLIN   DD *                                                         
 DISPLAY FROM(IN) LIST(LISTA) -                                         
 BLANK -                                                                
 BETWEEN(1) -                                                           
 TITLE('CONTAS SELECIONADAS SENSIBILIZADAS') -                          
 DATE(DM4/)   TIME   PAGE -                                             
 HEADER('BCO')             ON(001,02,PD,E'999',NOST) -                  
 HEADER('AG')              ON(003,03,PD,E'99999',NOST) -                
 HEADER('GRP')             ON(006,02,PD,E'999',NOST) -                  
 HEADER('SGRP')            ON(008,02,PD,E'999',NOST) -                  
 HEADER('CONTA')           ON(010,04,PD,E'9999999',NOST) -              
 HEADER('DIG')             ON(014,01,CH) -                              
 HEADER('DT MOV')          ON(015,05,PD,E'999999999',NOST) -            
 HEADER('LANC')            ON(020,03,PD,E'99999',NOST) -                
 HEADER('N DOC')           ON(023,04,PD,E'9999999',NOST) -              
 HEADER('DEB','CRED')      ON(027,01,CH) -                              
 HEADER('VALOR')           ON(028,08,PD,C2,N15) -                       
 HEADER('DIA','VIN')       ON(036,02,PD,E'999',NOST) -                  
 HEADER('CENT','CUST')     ON(038,04,CH) -                              
 HEADER('SUBC','CRS')      ON(042,03,PD,E'99999',NOST) -                
 HEADER('SERV')            ON(045,02,CH) -                              
 HEADER('TP','ENT')        ON(047,01,CH) -                              
 HEADER('SN')              ON(048,01,CH) -                              
 HEADER('ORIGEM')          ON(049,32,CH) -                              
 HEADER('DIA','LA')        ON(081,02,ZD,E'99',NOST) -                   
 HEADER('DT ORI')          ON(083,05,PD,E'999999999',NOST) -            
 HEADER('PROD')            ON(088,04,CH) -                              
 HEADER('OPER')            ON(092,07,CH) -                              
 HEADER('SEQ')             ON(099,03,CH)                                
//*                                                                     

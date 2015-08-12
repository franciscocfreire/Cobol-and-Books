      *================================================================*
       IDENTIFICATION DIVISION.                                         
      *================================================================*
                                                                        
       PROGRAM-ID.                     LPCL302L.                        
       AUTHOR.                         HEBE.                            
      *================================================================*
      *                            B  R  Q                             *
      *----------------------------------------------------------------*
      *    PROGRAMA....: LPCL302L                                      *
      *    PROGRAMADOR.: HEBE                       - BRQ              *
      *    ANALISTA....: HEBE                       - BRQ              *
      *    DATA........: 25/06/2013                                    *
      *----------------------------------------------------------------*
      *    OBJETIVO....: LISTA CONTRATOS DE UM CLIENTE DE UM DETERMINA-*
      *                  DO GERENTE DE UMA AGENCIA                     *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *               DB2                                              *
      *                TABLE                        INCLUDE/BOOKS      *
      *                  DB2PRD.TCONS_CONTR_MORA      LPCLB00B         *
      *----------------------------------------------------------------*
      *    BOOKS FUNCIONAIS.....:                                      *
      *                                                                *
      *    LPCLW001 - BOOK DE CONTROLE DO ACESSO AO SERVICO FUNCIONAL  *
      *    LPCLW02I - BOOK DE ACESSO AO SERVICO FUNCIONAL              *
      *                                                                *
      *----------------------------------------------------------------*
      *    BOOKS ARQUITETURAIS..:                                      *
      *                                                                *
      *    I#FRWKGE - COMMAREA FRWK1999 (LOG DE ERRO)                  *
      *    I#FRWKHE - COMMAREA FRWK1999 (AREA COMUM ERROS)             *
      *    I#FRWKDB - COMMAREA FRWK1999 (LOG DE ERROS DB2)             *
      *    I#FRWKMD - COMMAREA FRWK1999 (LOG DE ERROS MODULO)          *
      *    I#FRWKCI - COMMAREA FRWK1999 (LOG DE ERROS CICS)            *
      *                                                                *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    FRWK1999 - PROCEDIMENTOS PARA GRAVACAO DE LOGS DE ERRO      *
      *================================================================*
                                                                        
      *================================================================*
       ENVIRONMENT                     DIVISION.                        
      *================================================================*
                                                                        
      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.                         
      *----------------------------------------------------------------*
                                                                        
       SPECIAL-NAMES.                                                   
           DECIMAL-POINT               IS COMMA.                        
                                                                        
      *================================================================*
       DATA                            DIVISION.                        
      *================================================================*
                                                                        
      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.                         
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE        
           '*** INICIO DA WORKING LPCL302L ***'.                        
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE        
           'AREA PARA INDEXADORES'.                                     
      *----------------------------------------------------------------*
                                                                        
       77  IND-1                       PIC  9(003) COMP-3  VALUE ZEROS. 
                                                                        
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE        
           'AREA PARA VARIAVEIS AUXILIARES'.                            
      *----------------------------------------------------------------*
                                                                        
       77  WRK-PROGRAMA                PIC  X(008)         VALUE        
           'LPCL302L'.                                                  
       77  WRK-FRWK1999                PIC  X(008)         VALUE        
           'FRWK1999'.                                                  
                                                                        
       77  WRK-FIM-CURSOR-1            PIC  X(001)         VALUE SPACES.
                                                                        
       01  WRK-CHAVE.                                                   
           05  WRK-CHV-DVCTO           PIC  X(010)         VALUE ZEROS.
           05  WRK-CHV-VL-CONTR-MORA   PIC  9(013)V99      VALUE ZEROS. 
           05  WRK-CHV-CCTA-CORR       PIC  9(007)         VALUE ZEROS. 
           05  WRK-CHV-CCART           PIC  X(003)         VALUE SPACES.
           05  WRK-CHV-CCONTR          PIC  9(007)         VALUE ZEROS. 
                                                                        
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE        
           'AREA PARA VARIAVEIS NULAS'.                                 
      *----------------------------------------------------------------*
                                                                        
       01  WRK-NULOS.                                                   
           05  WRK-CDIG-CTA-REFT-NULL  PIC S9(004) COMP    VALUE ZEROS. 
           05  WRK-RFASE-PROCS-NULL    PIC S9(004) COMP    VALUE ZEROS. 
           05  WRK-CFLIAL-CNPJ-NULL    PIC S9(004) COMP    VALUE ZEROS. 
                                                                        
                                                                        
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE        
           'AREA DE COMUNICACAO DO FRWK1999 (LOG DE ERRO)'.             
      *----------------------------------------------------------------*
                                                                        
       01  WRK-AREA-ERRO.                                               
           COPY 'I#FRWKGE'.
           05  WRK-BLOCO-INFO-ERRO.                                     
               10 WRK-CHAR-INFO-ERRO   PIC X(01) OCCURS 0 TO 30000 TIMES
                                       DEPENDING ON FRWKGHEA-TAM-DADOS. 
                                                                        
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE        
           'AREA DE TRATAMENTO DE ERRO DB2  (API-ERRO)'.                
      *----------------------------------------------------------------*
                                                                        
       01  WRK-AREA-ERRO-DB2.                                           
           COPY 'I#FRWKDB'.
                                                                        
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE        
           'AREA PARA DB2'.                                             
      *----------------------------------------------------------------*
                                                                        
           EXEC SQL                                                     
               INCLUDE SQLCA                                            
           END-EXEC.                                                    
                                                                        
           EXEC SQL                                                     
               INCLUDE LPCLB00B                                         
           END-EXEC.                                                    
                                                                        
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE        
           'AREA PARA CURSORES'.                                        
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
                                                                        
           EXEC SQL                                                     
             DECLARE CSR01-LPCLB00B CURSOR FOR                          
              SELECT                                                    
                  CFUNC_BDSCO,                                          
                  CAG_BCRIA,                                            
                  CCTA_CORR,                                            
                  CCART,                                                
                  CCONTR_VENCD,                                         
                  CDIG_CTA_REFT,                                        
                  CCPF_CNPJ,                                            
                  CFLIAL_CNPJ,                                          
                  CCTRL_CPF_CNPJ,                                       
                  CPCELA_CONTR_MORA,                                    
                  CDIG_CREDT_LIQDC,                                     
                  VLIQ,                                                 
                  VATULZ_CONTR_MORA,                                    
                  VCONTR_MORA_VENCD,                                    
                  VTOT_CONTR_MORA,                                      
                  RFASE_PROCS,                                          
                  DINIC_CONTR_ATRSO,                                    
                  DVCTO                                                 
             FROM DB2PRD.TCONS_CONTR_MORA                               
             WHERE  CFUNC_BDSCO       =  :LPCLB00B.CFUNC-BDSCO          
               AND  CAG_BCRIA         =  :LPCLB00B.CAG-BCRIA            
               AND  CCPF_CNPJ         =  :LPCLB00B.CCPF-CNPJ            
               AND  CFLIAL_CNPJ       =  :LPCLB00B.CFLIAL-CNPJ          
               AND  CCTRL_CPF_CNPJ    =  :LPCLB00B.CCTRL-CPF-CNPJ       
      *  RESTART                                                        
                                                                        
               AND (                                                    
                   (CCONTR_VENCD      >  :LPCLB00B.CCONTR-VENCD)        
               OR                                                       
                   (CCONTR_VENCD      =  :LPCLB00B.CCONTR-VENCD         
               AND  DVCTO             <  :LPCLB00B.DVCTO)               
               OR                                                       
                   (CCONTR_VENCD      =  :LPCLB00B.CCONTR-VENCD         
               AND  DVCTO             =  :LPCLB00B.DVCTO                
               AND  VATULZ_CONTR_MORA >  :LPCLB00B.VATULZ-CONTR-MORA)   
               OR                                                       
                   (CCONTR_VENCD      =  :LPCLB00B.CCONTR-VENCD         
               AND  DVCTO             =  :LPCLB00B.DVCTO                
               AND  VATULZ_CONTR_MORA =  :LPCLB00B.VATULZ-CONTR-MORA    
               AND  CCTA_CORR         <  :LPCLB00B.CCTA-CORR)           
               OR                                                       
                   (CCONTR_VENCD      =  :LPCLB00B.CCONTR-VENCD         
               AND  DVCTO             =  :LPCLB00B.DVCTO                
               AND  VATULZ_CONTR_MORA =  :LPCLB00B.VATULZ-CONTR-MORA    
               AND  CCTA_CORR         =  :LPCLB00B.CCTA-CORR            
               AND  CCART             >  :LPCLB00B.CCART)               
                    )                                                   
             ORDER  BY  CCONTR_VENCD      ASC ,                         
                        DVCTO             DESC,                         
                        CCART             ASC ,                         
                        VATULZ_CONTR_MORA DESC,                         
                        CCTA_CORR         ASC                           
             WITH UR                                                    
           END-EXEC.                                                    
HEBE                                                                    
                                                                        
           EXEC SQL                                                     
             DECLARE CSR02-LPCLB00B CURSOR FOR                          
              SELECT                                                    
                  CFUNC_BDSCO,                                          
                  CAG_BCRIA,                                            
                  CCTA_CORR,                                            
                  CCART,                                                
                  CCONTR_VENCD,                                         
                  CDIG_CTA_REFT,                                        
                  CCPF_CNPJ,                                            
                  CFLIAL_CNPJ,                                          
                  CCTRL_CPF_CNPJ,                                       
                  CPCELA_CONTR_MORA,                                    
                  CDIG_CREDT_LIQDC,                                     
                  VLIQ,                                                 
                  VATULZ_CONTR_MORA,                                    
                  VCONTR_MORA_VENCD,                                    
                  VTOT_CONTR_MORA,                                      
                  RFASE_PROCS,                                          
                  DINIC_CONTR_ATRSO,                                    
                  DVCTO                                                 
             FROM DB2PRD.TCONS_CONTR_MORA                               
             WHERE  CFUNC_BDSCO       =  :LPCLB00B.CFUNC-BDSCO          
               AND  CAG_BCRIA         =  :LPCLB00B.CAG-BCRIA            
               AND  CCPF_CNPJ         =  :LPCLB00B.CCPF-CNPJ            
               AND  CFLIAL_CNPJ       =  :LPCLB00B.CFLIAL-CNPJ          
               AND  CCTRL_CPF_CNPJ    =  :LPCLB00B.CCTRL-CPF-CNPJ       
      *  RESTART                                                        
                                                                        
               AND  (                                                   
                    (CCONTR_VENCD      <  :LPCLB00B.CCONTR-VENCD)       
               OR                                                       
                    (CCONTR_VENCD      =  :LPCLB00B.CCONTR-VENCD        
               AND   DVCTO             >  :LPCLB00B.DVCTO)              
               OR                                                       
                    (CCONTR_VENCD      =  :LPCLB00B.CCONTR-VENCD        
               AND   DVCTO             =  :LPCLB00B.DVCTO               
               AND   VATULZ_CONTR_MORA <  :LPCLB00B.VATULZ-CONTR-MORA)  
               OR                                                       
                    (CCONTR_VENCD      =  :LPCLB00B.CCONTR-VENCD        
               AND   DVCTO             =  :LPCLB00B.DVCTO               
               AND   VATULZ_CONTR_MORA =  :LPCLB00B.VATULZ-CONTR-MORA   
               AND   CCTA_CORR         >  :LPCLB00B.CCTA-CORR)          
               OR                                                       
                    (CCONTR_VENCD      =  :LPCLB00B.CCONTR-VENCD        
               AND   DVCTO             =  :LPCLB00B.DVCTO               
               AND   VATULZ_CONTR_MORA =  :LPCLB00B.VATULZ-CONTR-MORA   
               AND   CCTA_CORR         =  :LPCLB00B.CCTA-CORR           
               AND   CCART             <  :LPCLB00B.CCART)              
                    )                                                   
             ORDER  BY  CCONTR_VENCD      DESC,                         
                        DVCTO             ASC ,                         
                        CCART             DESC,                         
                        VATULZ_CONTR_MORA ASC ,                         
                        CCTA_CORR         DESC                          
             WITH UR                                                    
           END-EXEC.                                                    
                                                                        
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE        
           '*** FIM DA WORKING LPCL302L ***'.                           
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       LINKAGE                         SECTION.                         
      *----------------------------------------------------------------*
                                                                        
       01  DFHCOMMAREA.                                                 
           COPY 'LPCLW001'.
           COPY 'LPCLW02I'.
                                                                        
      *================================================================*
       PROCEDURE                       DIVISION USING DFHCOMMAREA.      
      *================================================================*
                                                                        
      *----------------------------------------------------------------*
      *    ROTINA PRINCIPAL DO PROGRAMA.                               *
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           PERFORM 1000-INICIALIZAR                                     
                                                                        
           PERFORM 2000-PROCESSAR                                       
                                                                        
           PERFORM 3000-FINALIZAR                                       
           .                                                            
                                                                        
      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *    ROTINA INICIAL DO PROGRAMA.                                 *
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           INITIALIZE  FRWKGERR-REGISTRO                                
                       FRWKGHEA-REGISTRO                                
                       FRWKGDB2-REGISTRO                                
                       LPCLW001-BLOCO-RETORNO                           
                                       OF DFHCOMMAREA                   
                                                                        
           PERFORM 1100-CONSISTIR-DADOS-ENTRADA                         
                                                                        
           PERFORM 1200-VERIFICAR-PAGINACAO                             
           .                                                            
                                                                        
      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *    ROTINA PARA CONSISTENCIA DOS DADOS DE ENTRADA.              *
      *----------------------------------------------------------------*
       1100-CONSISTIR-DADOS-ENTRADA    SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           IF  LPCLW02I-INDICADOR-PAGINACAO                             
                                       NOT EQUAL 'I' AND 'P' AND 'S'    
                                             AND 'A' AND 'U'            
               MOVE 08                 TO LPCLW001-COD-RETORNO          
                                       OF DFHCOMMAREA                   
               MOVE '0010'             TO LPCLW001-COD-ERRO             
                                       OF DFHCOMMAREA                   
               MOVE 'LPCL0001'         TO LPCLW001-COD-MENSAGEM         
                                       OF DFHCOMMAREA                   
               PERFORM 3000-FINALIZAR                                   
           END-IF                                                       
                                                                        
           IF  LPCLW02I-E-AGENCIA-GER  NOT NUMERIC OR                   
               LPCLW02I-E-AGENCIA-GER  EQUAL ZEROS                      
               MOVE 08                 TO LPCLW001-COD-RETORNO          
                                       OF DFHCOMMAREA                   
               MOVE '0020'             TO LPCLW001-COD-ERRO             
                                       OF DFHCOMMAREA                   
               MOVE 'LPCL0002'         TO LPCLW001-COD-MENSAGEM         
                                       OF DFHCOMMAREA                   
               PERFORM 3000-FINALIZAR                                   
           END-IF                                                       
                                                                        
           IF  LPCLW02I-E-COD-FUNC-BRD   NOT NUMERIC                    
               MOVE 08                 TO LPCLW001-COD-RETORNO          
                                       OF DFHCOMMAREA                   
               MOVE '0030'             TO LPCLW001-COD-ERRO             
                                       OF DFHCOMMAREA                   
               MOVE 'LPCL0003'         TO LPCLW001-COD-MENSAGEM         
                                       OF DFHCOMMAREA                   
               PERFORM 3000-FINALIZAR                                   
           END-IF                                                       
                                                                        
           IF  LPCLW02I-E-CPF-CNPJ   NOT NUMERIC OR                     
               LPCLW02I-E-CPF-CNPJ   EQUAL ZEROS                        
               MOVE 08                 TO LPCLW001-COD-RETORNO          
                                       OF DFHCOMMAREA                   
               MOVE '0030'             TO LPCLW001-COD-ERRO             
                                       OF DFHCOMMAREA                   
               MOVE 'LPCL0004'         TO LPCLW001-COD-MENSAGEM         
                                       OF DFHCOMMAREA                   
               PERFORM 3000-FINALIZAR                                   
           END-IF                                                       
                                                                        
           IF  LPCLW02I-E-CFLIAL-CPF-CNPJ NOT NUMERIC                   
               MOVE 08                 TO LPCLW001-COD-RETORNO          
                                       OF DFHCOMMAREA                   
               MOVE '0030'             TO LPCLW001-COD-ERRO             
                                       OF DFHCOMMAREA                   
               MOVE 'LPCL0004'         TO LPCLW001-COD-MENSAGEM         
                                       OF DFHCOMMAREA                   
               PERFORM 3000-FINALIZAR                                   
           END-IF                                                       
                                                                        
           IF  LPCLW02I-E-CCTRL-CPF-CNPJ NOT NUMERIC                    
               MOVE 08                 TO LPCLW001-COD-RETORNO          
                                       OF DFHCOMMAREA                   
               MOVE '0030'             TO LPCLW001-COD-ERRO             
                                       OF DFHCOMMAREA                   
               MOVE 'LPCL0004'         TO LPCLW001-COD-MENSAGEM         
                                       OF DFHCOMMAREA                   
               PERFORM 3000-FINALIZAR                                   
           END-IF                                                       
           .                                                            
      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *    ROTINA PARA VERIFICAR PAGINACAO.                            *
      *----------------------------------------------------------------*
       1200-VERIFICAR-PAGINACAO        SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           EVALUATE TRUE                                                
             WHEN LPCLW02I-P-INICIAL                                    
             WHEN LPCLW02I-P-PRIMEIRA                                   
               MOVE '2029-12-31'       TO WRK-CHV-DVCTO                 
               MOVE 9999999999999,99   TO WRK-CHV-VL-CONTR-MORA         
               MOVE ZEROS              TO WRK-CHV-CCTA-CORR             
               MOVE SPACES             TO WRK-CHV-CCART                 
               MOVE ZEROS              TO WRK-CHV-CCONTR                
                                                                        
             WHEN LPCLW02I-P-SEGUINTE                                   
               IF  LPCLW02I-FIM-CHAVE  GREATER LPCLW02I-INI-CHAVE       
                   MOVE LPCLW02I-FIM-CHAVE                              
                                       TO WRK-CHAVE                     
               ELSE                                                     
                   MOVE LPCLW02I-INI-CHAVE                              
                                       TO WRK-CHAVE                     
               END-IF                                                   
                                                                        
             WHEN LPCLW02I-P-ANTERIOR                                   
               IF  LPCLW02I-FIM-CHAVE  GREATER LPCLW02I-INI-CHAVE       
                   MOVE LPCLW02I-INI-CHAVE                              
                                       TO WRK-CHAVE                     
               ELSE                                                     
                   MOVE LPCLW02I-FIM-CHAVE                              
                                       TO WRK-CHAVE                     
               END-IF                                                   
                                                                        
             WHEN LPCLW02I-P-ULTIMA                                     
               MOVE '0001-01-01'       TO WRK-CHV-DVCTO                 
               MOVE ZEROS              TO WRK-CHV-VL-CONTR-MORA         
               MOVE 9999999            TO WRK-CHV-CCTA-CORR             
               MOVE '999'              TO WRK-CHV-CCART                 
               MOVE 9999999            TO WRK-CHV-CCONTR                
           END-EVALUATE                                                 
           .                                                            
                                                                        
      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *    PROCESSAMENTO PRINCIPAL DO PROGRAMA.                        *
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           IF  LPCLW02I-P-PRIMEIRA OR                                   
               LPCLW02I-P-SEGUINTE OR                                   
               LPCLW02I-P-INICIAL                                       
               PERFORM 2100-TRATA-PAGINACAO-PROXIMA                     
           ELSE                                                         
               PERFORM 2200-TRATA-PAGINACAO-ANTERIOR                    
           END-IF                                                       
           .                                                            
                                                                        
      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAR AVANCO DE PAGINA.                        *
      *----------------------------------------------------------------*
       2100-TRATA-PAGINACAO-PROXIMA    SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           PERFORM 2110-OPEN-CSR01-LPCLB00B                             
                                                                        
           PERFORM 2120-FETCH-CSR01-LPCLB00B                            
                                                                        
           IF  WRK-FIM-CURSOR-1        EQUAL 'S'                        
               IF  LPCLW02I-P-PRIMEIRA OR LPCLW02I-P-INICIAL            
                   MOVE 08             TO LPCLW001-COD-RETORNO          
                                       OF DFHCOMMAREA                   
                   MOVE '0120'         TO LPCLW001-COD-ERRO             
                                       OF DFHCOMMAREA                   
                   MOVE 'LPCL0006'     TO LPCLW001-COD-MENSAGEM         
                                       OF DFHCOMMAREA                   
               ELSE                                                     
                   MOVE 08             TO LPCLW001-COD-RETORNO          
                                       OF DFHCOMMAREA                   
                   MOVE '0130'         TO LPCLW001-COD-ERRO             
                                       OF DFHCOMMAREA                   
                   MOVE 'LPCL0007'     TO LPCLW001-COD-MENSAGEM         
                                       OF DFHCOMMAREA                   
               END-IF                                                   
           ELSE                                                         
               PERFORM 2130-MONTA-PAGINACAO-PROXIMA                     
           END-IF                                                       
                                                                        
           PERFORM 2140-CLOSE-CSR01-LPCLB00B                            
           .                                                            
                                                                        
      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *    ROTINA PARA ABERTURA DO CURSOR ASCENDENTE.                  *
      *----------------------------------------------------------------*
       2110-OPEN-CSR01-LPCLB00B        SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           INITIALIZE LPCLB00B                                          
                                                                        
           MOVE 'N'                     TO WRK-FIM-CURSOR-1             
                                                                        
           MOVE LPCLW02I-E-AGENCIA-GER  TO CAG-BCRIA        OF LPCLB00B 
           MOVE LPCLW02I-E-COD-FUNC-BRD TO CFUNC-BDSCO      OF LPCLB00B 
           MOVE LPCLW02I-E-CPF-CNPJ     TO CCPF-CNPJ        OF LPCLB00B 
           MOVE LPCLW02I-E-CFLIAL-CPF-CNPJ TO CFLIAL-CNPJ   OF LPCLB00B 
           MOVE LPCLW02I-E-CCTRL-CPF-CNPJ TO CCTRL-CPF-CNPJ OF LPCLB00B 
           MOVE WRK-CHV-DVCTO           TO DVCTO            OF LPCLB00B 
           MOVE WRK-CHV-VL-CONTR-MORA  TO VATULZ-CONTR-MORA OF LPCLB00B 
           MOVE WRK-CHV-CCTA-CORR       TO CCTA-CORR        OF LPCLB00B 
           MOVE SPACES                  TO CDIG-CTA-REFT    OF LPCLB00B 
           MOVE WRK-CHV-CCART           TO CCART            OF LPCLB00B 
           MOVE WRK-CHV-CCONTR          TO CCONTR-VENCD     OF LPCLB00B 

           EXEC SQL                                                     
                OPEN CSR01-LPCLB00B                                     
           END-EXEC.                                                    
                                                                        
           IF (SQLCODE                 NOT EQUAL ZEROS) OR              
              (SQLWARN0                EQUAL 'W')                       
               SET DB2-OPEN            TO TRUE                          
               MOVE '0140'             TO LPCLW001-COD-ERRO             
                                       OF DFHCOMMAREA                   
               MOVE 'TCONS_CONTR_MORA'                                  
                                       TO FRWKGDB2-NOME-TABELA          
               MOVE '2110-OPEN-CSR01-LPCLB00B'                          
                                       TO FRWKGHEA-IDEN-PARAGRAFO       
               PERFORM 9100-ERRO-DB2                                    
           END-IF                                                       
           .                                                            
                                                                        
      *----------------------------------------------------------------*
       2110-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *    ROTINA PARA LEITURA DO CURSOR ASCENDENTE.                   *
      *----------------------------------------------------------------*
       2120-FETCH-CSR01-LPCLB00B       SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           INITIALIZE                                                   
                      LPCLB00B                                          
                                                                        
           EXEC SQL                                                     
                FETCH CSR01-LPCLB00B INTO                               
                        :LPCLB00B.CFUNC-BDSCO,                          
                        :LPCLB00B.CAG-BCRIA,                            
                        :LPCLB00B.CCTA-CORR,                            
                        :LPCLB00B.CCART,                                
                        :LPCLB00B.CCONTR-VENCD,                         
                        :LPCLB00B.CDIG-CTA-REFT                         
                         :WRK-CDIG-CTA-REFT-NULL,                       
                        :LPCLB00B.CCPF-CNPJ,                            
                        :LPCLB00B.CFLIAL-CNPJ                           
                         :WRK-CFLIAL-CNPJ-NULL,                         
                        :LPCLB00B.CCTRL-CPF-CNPJ,                       
                        :LPCLB00B.CPCELA-CONTR-MORA,                    
                        :LPCLB00B.CDIG-CREDT-LIQDC,                     
                        :LPCLB00B.VLIQ,                                 
                        :LPCLB00B.VATULZ-CONTR-MORA,                    
                        :LPCLB00B.VCONTR-MORA-VENCD,                    
                        :LPCLB00B.VTOT-CONTR-MORA,                      
                        :LPCLB00B.RFASE-PROCS                           
                         :WRK-RFASE-PROCS-NULL,                         
                        :LPCLB00B.DINIC-CONTR-ATRSO,                    
                        :LPCLB00B.DVCTO                                 
           END-EXEC.                                                    
                                                                        
           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR     
              (SQLWARN0                EQUAL 'W')                       
               SET DB2-FETCH           TO TRUE                          
               MOVE '0150'             TO LPCLW001-COD-ERRO             
                                       OF DFHCOMMAREA                   
               MOVE 'TCONS_CONTR_MORA'                                  
                                       TO FRWKGDB2-NOME-TABELA          
               MOVE '2120-FETCH-CSR01-LPCLB00B'                         
                                       TO FRWKGHEA-IDEN-PARAGRAFO       
               PERFORM 9100-ERRO-DB2                                    
           END-IF                                                       
                                                                        
           IF  SQLCODE                 EQUAL  +100                      
               MOVE 'S'                TO     WRK-FIM-CURSOR-1          
               GO TO 2120-99-FIM                                        
           END-IF                                                       
                                                                        
           IF WRK-CDIG-CTA-REFT-NULL   LESS   ZEROS                     
              MOVE  SPACES         TO  CDIG-CTA-REFT OF LPCLB00B        
           END-IF                                                       
           IF WRK-RFASE-PROCS-NULL     LESS   ZEROS                     
              MOVE  SPACES         TO  RFASE-PROCS   OF LPCLB00B        
           END-IF                                                       
           IF WRK-CFLIAL-CNPJ-NULL     LESS   ZEROS                     
                   MOVE  ZEROS     TO  CFLIAL-CNPJ   OF LPCLB00B        
           END-IF                                                       
           .                                                            
                                                                        
      *----------------------------------------------------------------*
       2120-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *    ROTINA PARA MONTAR A PAGINACAO PROXIMA.                     *
      *----------------------------------------------------------------*
       2130-MONTA-PAGINACAO-PROXIMA    SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           PERFORM                     UNTIL                            
               (WRK-FIM-CURSOR-1      EQUAL 'S')  OR                    
               (IND-1             NOT LESS  +50)                        
                PERFORM 2180-MOVER-DADOS                                
                PERFORM 2120-FETCH-CSR01-LPCLB00B                       
           END-PERFORM                                                  
                                                                        
           MOVE IND-1                  TO LPCLW02I-NUM-CONSULTAS        
                                                                        
           IF  WRK-FIM-CURSOR-1        EQUAL 'S'                        
               IF  LPCLW02I-NUM-CONSULTAS                               
                                       EQUAL ZEROS                      
                   MOVE 08             TO LPCLW001-COD-RETORNO          
                                       OF DFHCOMMAREA                   
                   MOVE '0160'         TO LPCLW001-COD-ERRO             
                                       OF DFHCOMMAREA                   
                   MOVE 'LPCL0006'     TO LPCLW001-COD-MENSAGEM         
                                       OF DFHCOMMAREA                   
               ELSE                                                     
                   MOVE ZEROS          TO LPCLW001-COD-RETORNO          
                                       OF DFHCOMMAREA                   
                   MOVE '0170'         TO LPCLW001-COD-ERRO             
                                       OF DFHCOMMAREA                   
                   MOVE 'LPCL0007'     TO LPCLW001-COD-MENSAGEM         
                                       OF DFHCOMMAREA                   
               END-IF                                                   
           ELSE                                                         
               MOVE 01                 TO LPCLW001-COD-RETORNO          
                                       OF DFHCOMMAREA                   
               MOVE '0180'             TO LPCLW001-COD-ERRO             
                                       OF DFHCOMMAREA                   
               MOVE 'LPCL0008'         TO LPCLW001-COD-MENSAGEM         
                                       OF DFHCOMMAREA                   
           END-IF                                                       
           .                                                            
                                                                        
      *----------------------------------------------------------------*
       2130-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *    ROTINA PARA FECHAMENTO CURSOR ASCENDENTE.                   *
      *----------------------------------------------------------------*
       2140-CLOSE-CSR01-LPCLB00B       SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           EXEC SQL                                                     
                CLOSE CSR01-LPCLB00B                                    
           END-EXEC.                                                    
                                                                        
           IF (SQLCODE                 NOT EQUAL ZEROS) OR              
              (SQLWARN0                EQUAL 'W')                       
               SET DB2-CLOSE           TO TRUE                          
               MOVE '0190'             TO LPCLW001-COD-ERRO             
                                       OF DFHCOMMAREA                   
               MOVE 'TCONS_CONTR_MORA'                                  
                                       TO FRWKGDB2-NOME-TABELA          
               MOVE '2140-CLOSE-CSR01-LPCLB00B'                         
                                       TO FRWKGHEA-IDEN-PARAGRAFO       
               PERFORM 9100-ERRO-DB2                                    
           END-IF                                                       
           .                                                            
                                                                        
      *----------------------------------------------------------------*
       2140-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *    ROTINA PARA MOVIMENTACAO DOS DADOS DE SAIDA.                *
      *----------------------------------------------------------------*
       2180-MOVER-DADOS                SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           ADD 1                  TO IND-1                              
                                                                        
           IF  IND-1              EQUAL  1                              
               MOVE CCTA-CORR     OF LPCLB00B                           
                                  TO LPCLW02I-INI-CCTA-CORR             
               MOVE CCART         OF LPCLB00B                           
                                  TO LPCLW02I-INI-CCART                 
               MOVE CCONTR-VENCD  OF LPCLB00B                           
                                  TO LPCLW02I-INI-CCONTR

               STRING  DVCTO      OF LPCLB00B(7:4) '-'
                       DVCTO      OF LPCLB00B(4:2) '-'
                       DVCTO      OF LPCLB00B(1:2)
               DELIMITED BY SIZE  INTO    LPCLW02I-INI-DVCTO


               MOVE VATULZ-CONTR-MORA OF LPCLB00B                       
                                  TO LPCLW02I-INI-VL-CONTR-MORA         
           END-IF                                                       
                                                                        
           MOVE CCTA-CORR         OF LPCLB00B                           
                                  TO LPCLW02I-FIM-CCTA-CORR             
                                     LPCLW02I-S-CCTA-CORR(IND-1).       
           MOVE CDIG-CTA-REFT     OF LPCLB00B                           
                                  TO LPCLW02I-S-CDIG-CTA-REFT(IND-1).   
           MOVE CCART             OF LPCLB00B                           
                                  TO LPCLW02I-FIM-CCART                 
                                     LPCLW02I-S-CCART(IND-1).           
           MOVE CCONTR-VENCD      OF LPCLB00B                           
                                  TO LPCLW02I-FIM-CCONTR                
                                     LPCLW02I-S-CCONTR-VENCD(IND-1).    
           MOVE CPCELA-CONTR-MORA OF LPCLB00B                           
                                  TO LPCLW02I-S-CPCELA-CONTR(IND-1).    
           MOVE CDIG-CREDT-LIQDC  OF LPCLB00B                           
                                  TO LPCLW02I-S-CDIG-CRED-LIQ(IND-1).   
           MOVE VLIQ              OF LPCLB00B                           
                                  TO LPCLW02I-S-VLIQUI(IND-1).          
           MOVE VATULZ-CONTR-MORA OF LPCLB00B                           
                                  TO LPCLW02I-FIM-VL-CONTR-MORA         
                                     LPCLW02I-S-VATULZ(IND-1).          
           MOVE VCONTR-MORA-VENCD OF LPCLB00B                           
                                  TO LPCLW02I-S-VVENCD(IND-1).          
           MOVE RFASE-PROCS       OF LPCLB00B                           
                                  TO LPCLW02I-S-RFASE-PROC(IND-1).      
           MOVE DINIC-CONTR-ATRSO OF LPCLB00B                           
                                  TO LPCLW02I-S-DTINIC-OPER(IND-1).     
           MOVE DVCTO             OF LPCLB00B                           
                                  TO LPCLW02I-S-DVENC(IND-1)

           STRING  DVCTO      OF LPCLB00B(7:4) '-'
                   DVCTO      OF LPCLB00B(4:2) '-'
                   DVCTO      OF LPCLB00B(1:2)
           DELIMITED BY SIZE  INTO    LPCLW02I-FIM-DVCTO.

      *----------------------------------------------------------------*
       2180-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *    ROTINA PARA PAGINACAO DESCENDENTE.                          *
      *----------------------------------------------------------------*
       2200-TRATA-PAGINACAO-ANTERIOR   SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           PERFORM 2210-OPEN-CSR02-LPCLB00B                             
                                                                        
           PERFORM 2220-FETCH-CSR02-LPCLB00B                            
                                                                        
           IF  WRK-FIM-CURSOR-1        EQUAL 'S'                        
               IF  LPCLW02I-P-ULTIMA                                    
                   MOVE 08             TO LPCLW001-COD-RETORNO          
                                       OF DFHCOMMAREA                   
                   MOVE '0210'         TO LPCLW001-COD-ERRO             
                                       OF DFHCOMMAREA                   
                   MOVE 'LPCL0006'     TO LPCLW001-COD-MENSAGEM         
                                       OF DFHCOMMAREA                   
               ELSE                                                     
                   MOVE 08             TO LPCLW001-COD-RETORNO          
                                       OF DFHCOMMAREA                   
                   MOVE '0220'         TO LPCLW001-COD-ERRO             
                                       OF DFHCOMMAREA                   
                   MOVE 'LPCL0007'     TO LPCLW001-COD-MENSAGEM         
                                       OF DFHCOMMAREA                   
               END-IF                                                   
           ELSE                                                         
               PERFORM 2230-MONTA-PAGINACAO-ANTERIOR                    
           END-IF                                                       
                                                                        
           PERFORM 2240-CLOSE-CSR02-LPCLB00B                            
           .                                                            
                                                                        
      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *    ROTINA PARA ABERTURA DO CURSOR DESCENDENTE.                 *
      *----------------------------------------------------------------*
       2210-OPEN-CSR02-LPCLB00B        SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           INITIALIZE LPCLB00B                                          
                                                                        
           MOVE 'N'                    TO WRK-FIM-CURSOR-1              
                                                                        
                                                                        
           MOVE LPCLW02I-E-AGENCIA-GER  TO CAG-BCRIA        OF LPCLB00B 
           MOVE LPCLW02I-E-COD-FUNC-BRD TO CFUNC-BDSCO      OF LPCLB00B 
           MOVE LPCLW02I-E-CPF-CNPJ     TO CCPF-CNPJ        OF LPCLB00B 
           MOVE LPCLW02I-E-CFLIAL-CPF-CNPJ TO CFLIAL-CNPJ   OF LPCLB00B 
           MOVE LPCLW02I-E-CCTRL-CPF-CNPJ TO CCTRL-CPF-CNPJ OF LPCLB00B 
           MOVE WRK-CHV-CCTA-CORR       TO CCTA-CORR        OF LPCLB00B 
           MOVE SPACES                  TO CDIG-CTA-REFT    OF LPCLB00B 
           MOVE WRK-CHV-CCART           TO CCART            OF LPCLB00B 
           MOVE WRK-CHV-CCONTR          TO CCONTR-VENCD     OF LPCLB00B 
           MOVE WRK-CHV-DVCTO           TO DVCTO            OF LPCLB00B 
           MOVE WRK-CHV-VL-CONTR-MORA  TO VATULZ-CONTR-MORA OF LPCLB00B 
                                                                        
           EXEC SQL                                                     
                OPEN CSR02-LPCLB00B                                     
           END-EXEC.                                                    
                                                                        
           IF (SQLCODE                 NOT EQUAL ZEROS) OR              
              (SQLWARN0                EQUAL 'W')                       
               SET DB2-OPEN            TO TRUE                          
               MOVE '0230'             TO LPCLW001-COD-ERRO             
                                       OF DFHCOMMAREA                   
               MOVE 'TCONS_CONTR_MORA'                                  
                                       TO FRWKGDB2-NOME-TABELA          
               MOVE '2210-OPEN-CSR02-LPCLB00B'                          
                                       TO FRWKGHEA-IDEN-PARAGRAFO       
               PERFORM 9100-ERRO-DB2                                    
           END-IF                                                       
           .                                                            
                                                                        
      *----------------------------------------------------------------*
       2210-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *    ROTINA PARA LEITURA DO CURSOR DESCENDENTE.                  *
      *----------------------------------------------------------------*
       2220-FETCH-CSR02-LPCLB00B       SECTION.                         
      *----------------------------------------------------------------*
                                                                        
            EXEC SQL                                                    
                 FETCH  CSR02-LPCLB00B INTO                             
                        :LPCLB00B.CFUNC-BDSCO,                          
                        :LPCLB00B.CAG-BCRIA,                            
                        :LPCLB00B.CCTA-CORR,                            
                        :LPCLB00B.CCART,                                
                        :LPCLB00B.CCONTR-VENCD,                         
                        :LPCLB00B.CDIG-CTA-REFT                         
                         :WRK-CDIG-CTA-REFT-NULL,                       
                        :LPCLB00B.CCPF-CNPJ,                            
                        :LPCLB00B.CFLIAL-CNPJ                           
                         :WRK-CFLIAL-CNPJ-NULL,                         
                        :LPCLB00B.CCTRL-CPF-CNPJ,                       
                        :LPCLB00B.CPCELA-CONTR-MORA,                    
                        :LPCLB00B.CDIG-CREDT-LIQDC,                     
                        :LPCLB00B.VLIQ,                                 
                        :LPCLB00B.VATULZ-CONTR-MORA,                    
                        :LPCLB00B.VCONTR-MORA-VENCD,                    
                        :LPCLB00B.VTOT-CONTR-MORA,                      
                        :LPCLB00B.RFASE-PROCS                           
                         :WRK-RFASE-PROCS-NULL,                         
                        :LPCLB00B.DINIC-CONTR-ATRSO,                    
                        :LPCLB00B.DVCTO                                 
            END-EXEC.                                                   
                                                                        
           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR     
              (SQLWARN0                EQUAL 'W')                       
               SET DB2-FETCH           TO TRUE                          
               MOVE '0240'             TO LPCLW001-COD-ERRO             
                                       OF DFHCOMMAREA                   
               MOVE 'CONS_CLI_MORA'                                     
                                       TO FRWKGDB2-NOME-TABELA          
               MOVE '2120-FETCH-CSR01-LPCLB00B'                         
                                       TO FRWKGHEA-IDEN-PARAGRAFO       
               PERFORM 9100-ERRO-DB2                                    
           END-IF                                                       
                                                                        
           IF  SQLCODE                  EQUAL   +100                    
               MOVE 'S'                 TO      WRK-FIM-CURSOR-1        
           END-IF                                                       
                                                                        
           IF  SQLCODE                  EQUAL   ZEROS                   
                                                                        
               IF WRK-CDIG-CTA-REFT-NULL   LESS   ZEROS                 
                  MOVE  SPACES          TO  CDIG-CTA-REFT OF LPCLB00B   
               END-IF                                                   
               IF WRK-RFASE-PROCS-NULL     LESS   ZEROS                 
                  MOVE  SPACES          TO  RFASE-PROCS   OF LPCLB00B   
               END-IF                                                   
               IF WRK-CFLIAL-CNPJ-NULL     LESS   ZEROS                 
                  MOVE  ZEROS           TO  CFLIAL-CNPJ   OF LPCLB00B   
               END-IF                                                   
           END-IF                                                       
           .                                                            
                                                                        
      *----------------------------------------------------------------*
       2220-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *    ROTINA PARA MONTAR A PAGINACAO ANTERIOR.                    *
      *----------------------------------------------------------------*
       2230-MONTA-PAGINACAO-ANTERIOR   SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           PERFORM                     UNTIL                            
               (WRK-FIM-CURSOR-1 EQUAL      'S')  OR                    
               (IND-1            NOT LESS   +50)                        
                PERFORM 2180-MOVER-DADOS                                
                PERFORM 2220-FETCH-CSR02-LPCLB00B                       
           END-PERFORM                                                  
                                                                        
           MOVE IND-1                  TO LPCLW02I-NUM-CONSULTAS        
                                                                        
           IF  WRK-FIM-CURSOR-1        EQUAL 'S'                        
               IF  LPCLW02I-NUM-CONSULTAS                               
                                       EQUAL ZEROS                      
                   MOVE 08             TO LPCLW001-COD-RETORNO          
                                       OF DFHCOMMAREA                   
                   MOVE '0250'         TO LPCLW001-COD-ERRO             
                                       OF DFHCOMMAREA                   
                   MOVE 'LPCL0006'     TO LPCLW001-COD-MENSAGEM         
                                       OF DFHCOMMAREA                   
               ELSE                                                     
                   MOVE ZEROS          TO LPCLW001-COD-RETORNO          
                                       OF DFHCOMMAREA                   
                   MOVE '0260'         TO LPCLW001-COD-ERRO             
                                       OF DFHCOMMAREA                   
                   MOVE 'LPCL0007'     TO LPCLW001-COD-MENSAGEM         
                                       OF DFHCOMMAREA                   
               END-IF                                                   
           ELSE                                                         
               MOVE 01                 TO LPCLW001-COD-RETORNO          
                                       OF DFHCOMMAREA                   
               MOVE '0270'             TO LPCLW001-COD-ERRO             
                                       OF DFHCOMMAREA                   
               MOVE 'LPCL0008'         TO LPCLW001-COD-MENSAGEM         
                                       OF DFHCOMMAREA                   
           END-IF                                                       
           .                                                            
                                                                        
      *----------------------------------------------------------------*
       2230-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *    ROTINA PARA FECHAMENTO CURSOR DESCENDENTE.                  *
      *----------------------------------------------------------------*
       2240-CLOSE-CSR02-LPCLB00B       SECTION.                         
      *----------------------------------------------------------------*
                                                                        
            EXEC SQL                                                    
                 CLOSE CSR02-LPCLB00B                                   
            END-EXEC                                                    
                                                                        
           IF (SQLCODE                 NOT EQUAL ZEROS) OR              
              (SQLWARN0                EQUAL 'W')                       
               SET DB2-CLOSE           TO TRUE                          
               MOVE '0280'             TO LPCLW001-COD-ERRO             
                                       OF DFHCOMMAREA                   
               MOVE 'TCONS_CONTR_MORA'                                  
                                       TO FRWKGDB2-NOME-TABELA          
               MOVE '2240-CLOSE-CSR02-LPCLB00B'                         
                                       TO FRWKGHEA-IDEN-PARAGRAFO       
               PERFORM 9100-ERRO-DB2                                    
           END-IF                                                       
           .                                                            
                                                                        
      *----------------------------------------------------------------*
       2240-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
                                                                        
      *----------------------------------------------------------------*
      *    ROTINA PARA FINALIZAR O PROCESSAMENTO.                      *
      *----------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           EXEC CICS                                                    
               RETURN                                                   
           END-EXEC                                                     
           .                                                            
                                                                        
      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAMENTO DE ERRO DB2.                         *
      *----------------------------------------------------------------*
       9100-ERRO-DB2                   SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           SET  ERRO-DB2               TO TRUE                          
           MOVE 12                     TO LPCLW001-COD-RETORNO          
                                       OF DFHCOMMAREA                   
           MOVE 'LPCL0009'             TO LPCLW001-COD-MENSAGEM         
                                       OF DFHCOMMAREA                   
           MOVE WRK-PROGRAMA           TO FRWKGHEA-NOME-PROGRAMA        
           MOVE FRWKGDB2-TAM-LAYOUT    TO FRWKGHEA-TAM-DADOS            
           MOVE SQLSTATE               TO FRWKGDB2-SQLSTATE             
           MOVE SQLCA                  TO FRWKGDB2-SQLCA                
           MOVE SQLCODE                TO FRWKGDB2-SQLCODE              
           MOVE WRK-AREA-ERRO-DB2      TO WRK-BLOCO-INFO-ERRO(1:222)    
                                                                        
           PERFORM 9999-API-ERROS                                       
           .                                                            
                                                                        
      *----------------------------------------------------------------*
       9100-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *    ROTINA PARA GRAVAR LOG DE ERRO CHAMADA AO FRWK1999.         *
      *----------------------------------------------------------------*
       9999-API-ERROS                  SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           EXEC CICS LINK                                               
               PROGRAM  (WRK-FRWK1999)                                  
               COMMAREA (WRK-AREA-ERRO)                                 
               LENGTH   (LENGTH OF WRK-AREA-ERRO)                       
               NOHANDLE                                                 
           END-EXEC                                                     
                                                                        
           IF  EIBRESP                 NOT EQUAL DFHRESP(NORMAL)        
               CONTINUE                                                 
           END-IF                                                       
                                                                        
           PERFORM 3000-FINALIZAR                                       
           .                                                            
                                                                        
      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*

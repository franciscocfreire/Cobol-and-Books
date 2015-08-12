*********** Convertido de OS/VS COBOL p/ COBOL for MVS em 15/01/04 19:47
      *===============================================================* 
       IDENTIFICATION DIVISION.                                         
      *===============================================================* 
       PROGRAM-ID.    CLLP5840.                                         
      ************************** CLLP5840 ***************************** 
      *K************************ CLLP5840 ***************************** 
      *                                                               * 
      *    PROGRAMADOR :   ALEXANDRE DAVIDIUK DIV.11                  * 
      *    SUPERVISORA :   ROSANA FELIX                               * 
      *    ANALISTA    :   ALVARO GP.14                               * 
      *    DATA        :   06/02/92                                   * 
      *                                                               * 
      *    OBJETIVO:                                                  * 
      *                                                               * 
      *       CRIAR ARQUIVO PARA CARGA ONLINE (DB2).                  * 
      *                                                               * 
      *                                                               * 
      *    ENTRADA:                                                   * 
      *       ARQLPCL - ARQUIVO A SER CLASSIFICADO.                   * 
      *       CHAVE   - AGENCIA,C/C,CARTEIRA,CONTRATO E VCTO.         * 
      *                                                               * 
      *       ARQCOMO - ARQUIVO A SER FORMATADO.                      * 
      *       CHAVE   - AGENCIA,C/C,CARTEIRA,CONTRATO E VCTO.         * 
      *                                                               * 
      *    SAIDA :                                                    * 
      *       ARQDB2   - CONTERA AMBOS ARQUIVOS CLASSIFICADOS.        * 
      *       ARQMUTI  - CONTERA REG. COM CART >= 500 < 600.          * 
      *       ARQPRESI - CONTERA REG. COM CART >= 500 < 600.          * 
      *                                                               * 
      *    OBS : PROGRAMA FEITO EM D.H.P.                             * 
      *                                                               * 
      ***************************************************************** 
      *                           A L T E R A C A O                   * 
      *                                                               * 
      *    PROGRAMADOR  :   ELCIO    C P M                            * 
      *    ANALISTA     :   ALVARO   GP 82                            * 
      *    DATA         :   11/06/1997                                * 
      *                                                               * 
      *    OBJETIVO     :   ALTERAR LAY-OUT DOS ARQUIVOS ARQLPCL E    * 
      *                     ARQDB2.                                   * 
      *                                                               * 
      ***************************************************************** 
      *                           A L T E R A C A O                   * 
      *                                                               * 
      *    ANALISTA     :   LOURIVAL SANTI - CPM                      * 
      *    DATA         :   09/09/1997                                * 
      *                                                               * 
      *    OBJETIVO     :   A CARTEIRA ORIGEM DEVERA SER IGUAL A      * 
      *                     CARTEIRA PREFIXO QUANDO O REGISTRO FOR    * 
      *                     FORMATADO PELO ARQCOMO.                   * 
      ***************************************************************** 
      *                           A L T E R A C A O                   * 
      *    ANALISTA     :   LUIZ BECHIELI                             * 
      *    DATA             26/04/99                                  * 
      *                                                               * 
      *    OBJETIVO     :   GERAR INFORMACOES PARA INTEGRACAO         * 
      *                     SYSOPEN X CLLP.                           * 
      ***************************************************************** 
      *                           A L T E R A C A O                   * 
      *    ANALISTA     : PATRICIA GUILHERMINO    - CPM SISTEMAS      * 
      *    DATA         : 29/06/1999                                  * 
      *                                                               * 
      *    OBJETIVO     : ALTERACAO DO LAY-OUT DE 94 P/ 140 POS.      * 
      *                   PARA ATENDER A NOVA PREVISAO/TRANSFERENCIA  * 
      *                   DE CL.                                      * 
      *                   ARQUIVO: ARQCOMO                            * 
      *                                                               * 
      ***************************************************************** 
      *                       A L T E R A C A O                       * 
      *---------------------------------------------------------------* 
      *    PROGRAMADOR.:   KHARUZO INOCENCIO LEITE   - CPM            * 
      *    ANALISTA CPM:   ALESSANDRO G. MORAES      - CPM            * 
      *    ANALISTA....:   ELIANE                    - GRUPO 82       * 
      *    DATA........:   28/01/2002                                 * 
      *---------------------------------------------------------------* 
      *    OBJETIVO....:   AUMENTAR O LAY-OUT DO ARQUIVO DE ENTRADA   * 
      *      ARQCOMO  DE  140  PARA  240  POSICOES  E  FORMATAR  AS   * 
      *      INFORMACOES REFERENTES AOS AVALISTAS 1 E 2 NO  ARQUIVO   * 
      *      SORT.                                                    * 
      ***************************************************************** 
STF   *---------------------------------------------------------------* 
STF   *                  U L T I M A  A L T E R A C A O               * 
STF   *---------------------------------------------------------------* 
STF   *                                                               * 
STF   *    PROGRAMADOR...: WERNER CJ DENZIN            - STEFANINI    * 
STF   *    SUPERVISOR....: RICARDO PINHO               - STEFANINI    * 
STF   *    ANALISTA......: CLAY SUSINI                 - CPM          * 
STF   *                                                               * 
STF   *    DATA..........: 07/01/2003                                 * 
STF   *    OBJETIVOS.....: ALTERAR LAYOUT  DO  ARQ DE ENTRADA ARQCOMO * 
STF   *                    INCLUINDO A INC I#TRCLAE.                  * 
STF   *                                                               * 
STF   *---------------------------------------------------------------* 
      ***************************************************************** 
      *                           A L T E R A C A O                   * 
      *                                                               * 
      *    ANALISTA     :   LUCIA VALERA - CPM                        * 
      *    DATA         :   13/06/2005                                * 
      *                                                               * 
      *    OBJETIVO     :   VALORIZAR O CAMPO DB2-CONTABIL.           * 
      ***************************************************************** 
BRQ141******************************************************************
BRQ141* Maio/2012 - (BRQ) - TRATAMENTO DE CARTEIRA  ALFANUMERICA        
BRQ141******************************************************************
           EJECT                                                        
      *===============================================================* 
       ENVIRONMENT DIVISION.                                            
      *===============================================================* 
       CONFIGURATION SECTION.                                           
       SPECIAL-NAMES.                                                   
           DECIMAL-POINT IS COMMA.                                      
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    
           SELECT  ARQLPCL     ASSIGN  TO  UT-S-ARQLPCL.                
           SELECT  ARQCOMO     ASSIGN  TO  UT-S-ARQCOMO.                
           SELECT  ARQDB2      ASSIGN  TO  UT-S-ARQDB2.                 
           SELECT  ARQSORT     ASSIGN  TO  UT-S-ARQSORT.                
           SELECT  ARQMUTI     ASSIGN  TO  UT-S-ARQMUTI.                
           SELECT  ARQPRESI    ASSIGN  TO  UT-S-ARQPRESI.               
           SELECT  RELATO      ASSIGN  TO  UT-S-RELATO.                 
           EJECT                                                        
      *===============================================================* 
       DATA DIVISION.                                                   
      *===============================================================* 
       FILE SECTION.                                                    
       FD  ARQLPCL                                                      
           RECORDING F LABEL RECORDS STANDARD                           
           BLOCK 0.                                                     
       01  PCL-REGTO.                                                   
           02  PCL-CHAVE.                                               
               03  PCL-EMPRESA                 PIC  9(05)  COMP-3.      
               03  PCL-AGENCIA                 PIC  9(05)  COMP-3.      
               03  PCL-NUM-CL                  PIC  9(15)  COMP-3.      
               03  PCL-DIG                     PIC  X(01).              
           02  PCL-CON-CORR.                                            
               03  PCL-CONTA                   PIC  9(07)  COMP-3.      
               03  PCL-DIG                     PIC  X(01).              
           02  PCL-CARTEIRA                    PIC  X(03).              
           02  PCL-CONTRATO                    PIC  9(07)  COMP-3.      
           02  PCL-VCTO                        PIC  X(10).              
           02  PCL-ID                          PIC  X(02).              
           02  PCL-DATAS.                                               
               03  PCL-ENTRADAS             PIC X(10).                  
               03  PCL-VCTO-MORA            PIC X(10).                  
           02  PCL-MOEDA                       PIC  X(02).              
           02  PCL-TIPO-GAR                    PIC  X(02).              
           02  PCL-LOCAL                       PIC  X(02).              
           02  PCL-TIPO-PEND                   PIC  9(05)  COMP-3.      
           02  PCL-SIGLA                       PIC  X(04).              
           02  PCL-TELEFONE                    PIC  9(07)  COMP-3.      
           02  PCL-CPF-ADVOG.                                           
               03  PCL-NUMERO-ADV              PIC  9(09)  COMP-3.      
               03  PCL-CTR                     PIC  9(02).              
           02  PCL-ULT-OCORR.                                           
               03  PCL-CODIGO                  PIC  9(03)  COMP-3.      
               03  PCL-DATA-OCORR              PIC  X(10).              
               03  PCL-STATUS                  PIC  X(02).              
               03  PCL-VENC-CARENCIA           PIC  X(10).              
           02  PCL-VALORES.                                             
               03  PCL-DEV-INIC                PIC  9(13)V99 COMP-3.    
               03  PCL-PRINCIPAL               PIC  9(13)V99 COMP-3.    
               03  PCL-CONTABIL                PIC  9(13)V99 COMP-3.    
               03  PCL-LIQUIDO                 PIC  9(13)V99 COMP-3.    
               03  PCL-COBRANCA                PIC  9(13)V99 COMP-3.    
               03  PCL-COBR-INF                PIC  9(13)V99 COMP-3.    
               03  PCL-JUROS-MORA              PIC  9(13)V99 COMP-3.    
               03  PCL-CORR-MONET              PIC  9(13)V99 COMP-3.    
               03  PCL-JUROS-12AA              PIC  9(13)V99 COMP-3.    
               03  PCL-TOTAL-CONTRATO          PIC  9(13)V99 COMP-3.    
           02  PCL-CARTEIRA-ORIGEM             PIC  X(03).              
           02  PCL-COD-NAT-OPER                PIC  X(03).              
           02  PCL-NOME-DEVEDOR                PIC  X(40).              
           02  PCL-CPF-DEV.                                             
               03  PCL-NUMERO-DEV              PIC  9(09)  COMP-3.      
               03  PCL-FILIAL                  PIC  9(05)  COMP-3.      
               03  PCL-CTR                     PIC  9(02).              
           02  PCL-NOME-AVALISTA               PIC  X(40).              
           02  PCL-CPF-AVAL.                                            
               03  PCL-NUMERO-AVAL             PIC  9(09)  COMP-3.      
               03  PCL-FILIAL-AVAL             PIC  9(05)  COMP-3.      
               03  PCL-CTR-AVAL                PIC  9(02).              
           02  PCL-NOME-AVAL2                  PIC  X(40).              
           02  PCL-CPF-AVAL2.                                           
               03  PCL-NUMERO-AVAL2            PIC  9(09)  COMP-3.      
               03  PCL-FILIAL-AVAL2            PIC  9(05)  COMP-3.      
               03  PCL-CTR-AVAL2               PIC  9(02).              
           02  PCL-BLOQUEIO-TRANF              PIC  X(01).              
           02  PCL-DIRETORIA.                                           
               03  PCL-EXEC                    PIC  9(03)  COMP-3.      
               03  PCL-REG.                                             
                   05  PCL-REGIONAL            PIC  9(03)  COMP-3.      
                   05  PCL-COD-JUNCAO          PIC  9(05)  COMP-3.      
           02  PCL-DEP-CAMBIO                  PIC  9(05)  COMP-3.      
           02  PCL-COD-EMPR                    PIC  9(02).              
           02  PCL-OCORRENCIAS                 OCCURS 50 TIMES.         
               03  PCL-OCORRS                  PIC 9(03)  COMP-3.       
           02  PCL-DT-INICIO-OPER              PIC X(10).               
           02  PCL-DT-PDD-180                  PIC X(10).               
           02  PCL-MARCA-PDD-180               PIC X(01).               
           02  PCL-RAZAO-PRINCIP               PIC 9(05)  COMP-3.       
           02  PCL-RAZAO-RENDAS                PIC 9(05)  COMP-3.       
           02  PCL-VR-VENCIDOS                 PIC 9(13)V99 COMP-3.     
           02  PCL-VR-VINCENDOS                PIC 9(13)V99 COMP-3.     
           02  PCL-MARCA-IMPE                  PIC X(01).               
           02  PCL-DT-RETORNO-CL               PIC X(10).               
           02  PCL-DT-PRIM-TRANSF-LP           PIC X(10).               
           02  PCL-VR-ENC-VENCIDOS-CONG        PIC 9(13)V99 COMP-3.     
           02  PCL-STATUS-REATIVACAO           PIC X(01).               
           02  PCL-PRODUTO-EMPF                PIC 9(03)    COMP-3.     
           02  PCL-FAMILIA-EMPF                PIC 9(01)    COMP-3.     
           02  FILLER                          PIC X(01).               
           02  PCL-DATA-AJUIZAMENTO            PIC X(10).               
           02  PCL-IOF-NORMAL                  PIC 9(13)V9(02) COMP-3.  
           02  PCL-IOF-COMPL                   PIC 9(13)V9(02) COMP-3.  
           02  PCL-ALIQ-CONT-RECOL             PIC 9(03)V9(06) COMP-3.  
           02  PCL-ALIQ-REC-TR-CL              PIC 9(03)V9(06) COMP-3.  
           02  PCL-ALIQ-COMPL                  PIC 9(03)V9(06) COMP-3.  
           02  PCL-IOF-OPCAO                   PIC X(01).               
           02  PCL-DATA-SISTEL                 PIC X(10).               
           02  PCL-IND-SISTEL                  PIC X(01).               
           02  PCL-NOTIF-SISTEL                PIC X(01).               
           02  PCL-AGRESP                      PIC 9(05)       COMP-3.  
       FD  ARQCOMO                                                      
           RECORDING F LABEL RECORDS STANDARD                           
           BLOCK 0.                                                     
STF                                                                     
BRQ=E******-INC I#TRCLAE                                                
BRQ=I      COPY I#CLLP28.                                               
       FD  ARQDB2                                                       
           RECORDING F LABEL RECORDS STANDARD                           
           BLOCK 0.                                                     
       01   DB2-REGTO.                                                  
            02  DB2-CHAVE.                                              
                03  DB2-EMPRESA                 PIC  9(05)  COMP-3.     
                03  DB2-AGENCIA                 PIC  9(05)  COMP-3.     
                03  DB2-NUM-CL                  PIC  9(15)  COMP-3.     
                03  DB2-DIG                     PIC  X(01).             
            02  DB2-CON-CORR.                                           
                03  DB2-CONTA                   PIC  9(07)  COMP-3.     
                03  DB2-DIG                     PIC  X(01).             
            02  DB2-CARTEIRA                    PIC  X(03).             
            02  DB2-CONTRATO                    PIC  9(07)  COMP-3.     
            02  DB2-VCTO                        PIC  X(10).             
            02  DB2-ID                          PIC  X(02).             
            02  DB2-DATAS.                                              
                03  DB2-ENTRADAS                PIC  X(10).             
                03  DB2-VCTO-MORA               PIC  X(10).             
            02  DB2-MOEDA                       PIC  X(02).             
            02  DB2-TIPO-GAR                    PIC  X(02).             
            02  DB2-LOCAL                       PIC  X(02).             
            02  DB2-TIPO-PEND                   PIC  9(05)  COMP-3.     
            02  DB2-SIGLA                       PIC  X(04).             
            02  DB2-TELEFONE                    PIC  9(07)  COMP-3.     
            02  DB2-CPF-ADVOG.                                          
                03  DB2-NUMERO-ADV              PIC  9(09)  COMP-3.     
                03  DB2-CTR                     PIC  9(02).             
            02  DB2-ULT-OCORR.                                          
                03  DB2-CODIGO                  PIC  9(03)  COMP-3.     
                03  DB2-DATA-OCORR              PIC  X(10).             
                03  DB2-STATUS                  PIC  X(02).             
            02  DB2-DATA-VENC-CARENCIA          PIC  X(10).             
            02  DB2-VALORES.                                            
                03  DB2-DEV-INIC                PIC  9(13)V99 COMP-3.   
                03  DB2-PRINCIPAL               PIC  9(13)V99 COMP-3.   
                03  DB2-CONTABIL                PIC  9(13)V99 COMP-3.   
                03  DB2-LIQUIDO                 PIC  9(13)V99 COMP-3.   
                03  DB2-COBRANCA                PIC  9(13)V99 COMP-3.   
                03  DB2-COBRANCA-INF            PIC  9(13)V99 COMP-3.   
                03  DB2-JUROS-MORA              PIC  9(13)V99 COMP-3.   
                03  DB2-CORR-MONET              PIC  9(13)V99 COMP-3.   
                03  DB2-JUROS-12AA              PIC  9(13)V99 COMP-3.   
                03  DB2-TOTTAL-CONTRATO         PIC  9(13)V99 COMP-3.   
            02  DB2-CARTEIRA-ORIGEM             PIC  X(03).             
            02  DB2-COD-NATUREZA-OPER           PIC  X(03).             
            02  DB2-NOME-DEVEDOR                PIC  X(40).             
            02  DB2-CPF-DEV.                                            
                03  DB2-NUMERO-DEV              PIC  9(09)  COMP-3.     
                03  DB2-FILIAL                  PIC  9(05)  COMP-3.     
                03  DB2-CTR                     PIC  9(02).             
            02  DB2-NOME-AVALISTA               PIC  X(40).             
            02  DB2-CPF-AVAL.                                           
                03  DB2-NUMERO-AVAL             PIC  9(09)  COMP-3.     
                03  DB2-FILIAL-AVAL             PIC  9(05)  COMP-3.     
                03  DB2-CTR-AVAL                PIC  9(02).             
            02  DB2-NOME-AVAL2                  PIC  X(40).             
            02  DB2-CPF-AVAL2.                                          
                03  DB2-NUMERO-AVAL2            PIC  9(09)  COMP-3.     
                03  DB2-FILIAL-AVAL2            PIC  9(05)  COMP-3.     
                03  DB2-CTR-AVAL2               PIC  9(02).             
            02  DB2-BLOQUEIO-TRANSF             PIC  X(01).             
            02  DB2-DIRETORIA.                                          
                03  DB2-EXEC                    PIC  9(03)  COMP-3.     
                03  DB2-REG.                                            
                    05  DB2-REGIONAL            PIC  9(03)  COMP-3.     
                    05  DB2-COD-JUNCAO          PIC  9(05)  COMP-3.     
            02  DB2-DEP-CAMBIO                  PIC  9(05)  COMP-3.     
            02  DB2-COD-EMPR                    PIC  9(02).             
            02  DB2-OCORRENCIAS  OCCURS 50 TIMES.                       
                03  DB2-OCCORS   PIC 9(03) COMP-3.                      
            02  DB2-OPER                        PIC  X(10).             
            02  DB2-DT-PDD-180                  PIC  X(10).             
            02  DB2-MARCA-PDD-180               PIC  X(01).             
            02  DB2-RAZAO-PRINCIP               PIC  9(05) COMP-3.      
            02  DB2-RAZAO-RENDAS                PIC  9(05) COMP-3.      
            02  DB2-VR-VENCIDOS                 PIC  9(13)V99 COMP-3.   
            02  DB2-VR-VINCENDOS                PIC  9(13)V99 COMP-3.   
            02  DB2-MARCA-IMPE                  PIC  X(01).             
            02  DB2-DT-RETORNO-CL               PIC  X(10).             
            02  DB2-DT-PRIM-TRANSF-LP           PIC  X(10).             
            02  DB2-VR-ENC-VENCIDOS-CONG        PIC  9(13)V99 COMP-3.   
            02  DB2-STATUS-REATIVACAO           PIC  X(01).             
            02  DB2-PRODUTO-EMPF                PIC  9(03)    COMP-3.   
            02  DB2-FAMILIA-EMPF                PIC  9(01)    COMP-3.   
            02  FILLER                          PIC  X(01).             
            02  DB2-DATA-AJUIZAMENTO            PIC  X(10).             
            02  DB2-IOF-NORMAL                  PIC  9(13)V9(02) COMP-3.
            02  DB2-IOF-COMPL                   PIC  9(13)V9(02) COMP-3.
            02  DB2-ALIQ-CONT-RECOL             PIC  9(03)V9(06) COMP-3.
            02  DB2-ALIQ-REC-TR-CL              PIC  9(03)V9(06) COMP-3.
            02  DB2-ALIQ-COMPL                  PIC  9(03)V9(06) COMP-3.
            02  DB2-IOF-OPCAO                   PIC  X(01).             
            02  DB2-DATA-SISTEL                 PIC  X(10).             
            02  DB2-IND-SISTEL                  PIC  X(01).             
            02  DB2-NOTIF-SISTEL                PIC  X(01).             
            02  DB2-AGRESP                      PIC  9(05)       COMP-3.
           EJECT                                                        
       FD  ARQMUTI                                                      
           RECORDING F LABEL RECORDS STANDARD                           
           BLOCK 0.                                                     
-INC I#LPCLBB                                                           
       FD  ARQPRESI                                                     
           RECORDING F LABEL RECORDS STANDARD                           
           BLOCK 0.                                                     
BRQ=E******-INC I#LPCLBC                                                
BRQ=I      COPY I#CLLP31.                                               
                                                                        
       FD  RELATO                                                       
           RECORDING F LABEL RECORDS STANDARD                           
           BLOCK 0.                                                     
       01  REL-REGTO   PIC X(133).                                      
           EJECT                                                        
       SD  ARQSORT.                                                     
       01   SORT-REGTO.                                                 
            02 SORT-CHAVE.                                              
                03 SORT-EMPRESA                 PIC  9(05)  COMP-3.     
                03 SORT-AGENCIA                 PIC  9(05)  COMP-3.     
                03 SORT-NUM-CL                  PIC  9(15)  COMP-3.     
                03 SORT-DIG-CL                  PIC  X(01).             
            02 SORT-CON-CORR.                                           
                03 SORT-CONTA                   PIC  9(07)  COMP-3.     
                03 SORT-DIG                     PIC  X(01).             
            02 SORT-CARTEIRA.                                           
BRQ=E***********03  SORT-CARTEIRA-R             PIC  9(03).             
BRQ=I           03  SORT-CARTEIRA-R             PIC  X(03).             
            02 SORT-CONTRATO                    PIC  9(07)  COMP-3.     
            02 SORT-VCTO.                                               
               03   SORT-VCTO-2.                                        
                    05  SORT-VCTO-ANO           PIC  X(04).             
                    05  FILLER                  PIC  X(01).             
                    05  SORT-VCTO-MES           PIC  X(02).             
                    05  FILLER                  PIC  X(01).             
                    05  SORT-VCTO-DIA           PIC  X(02).             
            02 SORT-ID                          PIC  X(02).             
            02 SORT-DATAS.                                              
JAPI=I          03  FILLER.                                             
                   05  SORT-ENTR-2.                                     
                        07  SORT-ENTR-ANO           PIC  X(04).         
                        07  FILLER                  PIC  X(01).         
                        07  SORT-ENTR-MES           PIC  X(02).         
                        07  FILLER                  PIC  X(01).         
                        07  SORT-ENTR-DIA           PIC  X(02).         
                03 SORT-VCTO-MORA.                                      
                   05  SORT-MORA-2.                                     
                        07  SORT-MORA-ANO       PIC  X(04).             
                        07  FILLER              PIC  X(01).             
                        07  SORT-MORA-MES       PIC  X(02).             
                        07  FILLER              PIC  X(01).             
                        07  SORT-MORA-DIA       PIC  X(02).             
            02 SORT-MOEDA                       PIC  X(02).             
            02 SORT-TIPO-GAR                    PIC  X(02).             
            02 SORT-LOCAL                       PIC  X(02).             
            02 SORT-TIPO-PEND                   PIC  9(05)  COMP-3.     
            02 SORT-SIGLA                       PIC  X(04).             
            02 SORT-TELEFONE                    PIC  9(07)  COMP-3.     
            02 SORT-CPF-ADVOG.                                          
                03 SORT-NUMERO-ADV              PIC  9(09)  COMP-3.     
                03 SORT-CTR                     PIC  9(02).             
            02 SORT-ULT-OCORR.                                          
                03 SORT-CODIGO                  PIC  9(03)  COMP-3.     
                03 SORT-DATA-OCORR.                                     
                   05  SORT-OCOR-2.                                     
                       07  SORT-OCOR-ANO        PIC  X(04).             
                       07  FILLER               PIC  X(01).             
                       07  SORT-OCOR-MES        PIC  X(02).             
                       07  FILLER               PIC  X(01).             
                       07  SORT-OCOR-DIA        PIC  X(02).             
                03 SORT-STATUS                  PIC  X(02).             
            02 SORT-DATA-VENC-CARENCIA          PIC  X(10).             
            02 SORT-VALORES.                                            
                03 SORT-DEV-INIC                PIC  9(13)V99 COMP-3.   
                03 SORT-PRINCIPAL               PIC  9(13)V99 COMP-3.   
                03 SORT-CONTABIL                PIC  9(13)V99 COMP-3.   
                03 SORT-LIQUIDO                 PIC  9(13)V99 COMP-3.   
                03 SORT-COBRANCA                PIC  9(13)V99 COMP-3.   
                03 SORT-COBRANCA-INF            PIC  9(13)V99 COMP-3.   
                03 SORT-JUROS-MORA              PIC  9(13)V99 COMP-3.   
                03 SORT-CORR-MONET              PIC  9(13)V99 COMP-3.   
                03 SORT-JUROS-12AA              PIC  9(13)V99 COMP-3.   
                03 SORT-TOTAL-CONTRATO          PIC  9(13)V99 COMP-3.   
            02 SORT-CARTEIRA-ORIGEM.                                    
BRQ=E***********03  SORT-CARTEIRA-ORIGEM-R      PIC  9(03).             
BRQ=I           03  SORT-CARTEIRA-ORIGEM-R      PIC  X(03).             
            02 SORT-COD-NATUREZA-OPER           PIC  X(03).             
            02 SORT-NOME-DEVEDOR                PIC  X(40).             
            02 SORT-CPF-DEV.                                            
                03 SORT-NUMERO-DEV              PIC  9(09)  COMP-3.     
                03 SORT-FILIAL                  PIC  9(05)  COMP-3.     
                03 SORT-CTR-DEV                 PIC  9(02).             
            02 SORT-NOME-AVALISTA               PIC  X(40).             
            02 SORT-CPF-AVAL.                                           
                03 SORT-NUMERO-AVAL             PIC  9(09)  COMP-3.     
                03 SORT-FILIAL-AVAL             PIC  9(05)  COMP-3.     
                03 SORT-CTR-AVAL                PIC  9(02).             
            02 SORT-NOME-AVAL2                  PIC  X(40).             
            02 SORT-CPF-AVAL2.                                          
                03 SORT-NUMERO-AVAL2            PIC  9(09)  COMP-3.     
                03 SORT-FILIAL-AVAL2            PIC  9(05)  COMP-3.     
                03 SORT-CTR-AVAL2               PIC  9(02).             
            02 SORT-BLOQUEIO-TRANSF             PIC  X(01).             
            02 SORT-DIRETORIA.                                          
                03 SORT-EXEC                    PIC  9(03)  COMP-3.     
                03 SORT-REG.                                            
                   05 SORT-REGIONAL              PIC  9(03)  COMP-3.    
                   05 SORT-COD-JUNCAO            PIC  9(05)  COMP-3.    
            02  SORT-DEP-CAMBIO                  PIC  9(05)  COMP-3.    
            02  SORT-COD-EMPR                    PIC  9(02).            
            02  SORT-OCORRENCIA                  OCCURS 50 TIMES.       
                03 SORT-OCCORS                   PIC 9(03) COMP-3.      
            02 SORT-OPER.                                               
               03   SORT-OPER-2.                                        
                    05  SORT-OPER-ANO            PIC  X(04).            
                    05  FILLER                   PIC  X(01).            
                    05  SORT-OPER-MES            PIC  X(02).            
                    05  FILLER                   PIC  X(01).            
                    05  SORT-OPER-DIA            PIC  X(02).            
            02 SORT-DT-PDD-180                   PIC  X(10).            
            02 SORT-MARCA-PDD-180                PIC  X(01).            
            02 SORT-RAZAO-PRINCIP                PIC  9(05) COMP-3.     
            02 SORT-RAZAO-RENDAS                 PIC  9(05) COMP-3.     
            02 SORT-VR-VENCIDOS                  PIC  9(13)V99 COMP-3.  
            02 SORT-VR-VINCENDOS                 PIC  9(13)V99 COMP-3.  
            02 SORT-MARCA-IMPE                   PIC  X(01).            
            02 SORT-DT-RETORNO-CL                PIC  X(10).            
            02 SORT-DT-PRIM-TRANSF-LP            PIC  X(10).            
            02 SORT-VR-ENC-VENCIDOS-CONG         PIC  9(13)V99 COMP-3.  
            02 SORT-STATUS-REATIVACAO            PIC  X(01).            
            02 SORT-PRODUTO-EMPF                 PIC  9(03)    COMP-3.  
            02 SORT-FAMILIA-EMPF                 PIC  9(01)    COMP-3.  
            02 SORT-RAMO                         PIC  X(01).            
            02 SORT-DATA-AJUIZAMENTO             PIC  X(10).            
            02 SORT-IOF-NORMAL                PIC  9(13)V9(02) COMP-3.  
            02 SORT-IOF-COMPL                 PIC  9(13)V9(02) COMP-3.  
            02 SORT-ALIQ-CONT-RECOL           PIC  9(03)V9(06) COMP-3.  
            02 SORT-ALIQ-REC-TR-CL            PIC  9(03)V9(06) COMP-3.  
            02 SORT-ALIQ-COMPL                PIC  9(03)V9(06) COMP-3.  
            02 SORT-IOF-OPCAO                 PIC  X(01).               
            02 SORT-DATA-SISTEL               PIC  X(10).               
            02 SORT-IND-SISTEL                PIC  X(01).               
            02 SORT-NOTIF-SISTEL              PIC  X(01).               
            02 SORT-AGRESP                    PIC  9(05)       COMP-3.  
           EJECT                                                        
      *===============================================================* 
       WORKING-STORAGE SECTION.                                         
      *===============================================================* 
        01  FILLER      PIC X(25)    VALUE 'INICIO DA WORKING-STORAGE'. 
        77  IND-TAB                PIC 9(05)    COMP-3   VALUE ZEROS.   
JAPI-U  77  WRK-POOL1205           PIC  X(08)   VALUE 'BRAD1205'.       
        77  DIGITO                 PIC X(01)             VALUE ZEROS.   
        77  TAM-VALOR              PIC 9(02)    VALUE    7.             
        77  ACU-ATIVOS             PIC 9(13)    COMP-3   VALUE ZEROS.   
        77  ACU-EXPURGADOS         PIC 9(13)    COMP-3   VALUE ZEROS.   
        77  ACU-MORA               PIC 9(13)    COMP-3   VALUE ZEROS.   
        77  ACU-EXCLUIDOS          PIC 9(13)    COMP-3   VALUE ZEROS.   
        77  ACU-PAG                PIC 9(09)    COMP-3   VALUE ZEROS.   
        77  ACU-LIN                PIC 9(05)    COMP-3   VALUE 66.      
STF     77  WRK-GARANTIA           PIC 9(02)             VALUE ZEROS.   
        01  CHAVE-AUX-SRT.                                              
            03 AUX-AGE-SRT         PIC  9(05)  COMP-3 VALUE ZEROS.      
            03 AUX-CONTA-SRT       PIC  9(07)  COMP-3 VALUE ZEROS.      
BRQ=E*******03 AUX-CART-SRT        PIC  9(03)         VALUE ZEROS.      
BRQ=I       03 AUX-CART-SRT        PIC  X(03)         VALUE SPACES.     
            03 AUX-CONTR-SRT       PIC  9(07)  COMP-3 VALUE ZEROS.      
        01 CHAVE-AUX-MI.                                                
            03 AUX-AGE-MI          PIC  9(05)  COMP-3 VALUE ZEROS.      
            03 AUX-CONTA-MI        PIC  9(07)  COMP-3 VALUE ZEROS.      
BRQ=E*******03 AUX-CART-MI         PIC  9(03)         VALUE ZEROS.      
BRQ=I       03 AUX-CART-MI         PIC  X(03)         VALUE SPACES.     
            03 AUX-CONTR-MI        PIC  9(07)  COMP-3 VALUE ZEROS.      
        01  AUX-DATA-PI.                                                
            03  AUX-ANO-PI         PIC 9(04)    VALUE ZEROS.            
            03  AUX-MES-PI         PIC 9(02)    VALUE ZEROS.            
            03  AUX-DIA-PI         PIC 9(02)    VALUE ZEROS.            
        01  AUX-DATA-PI-R  REDEFINES    AUX-DATA-PI PIC 9(08).          
      *---------------------------------------------------------------* 
      * AREA USADA PARA ACESSAR DATA DO SISTEMA E PARA DESCOMPACTAR   * 
      * A DATA DO VENCIMENTO DO ARQCOM.                               * 
      *---------------------------------------------------------------* 
       01  DATA-HORA.                                                   
           03  DT-JULIANA              PIC 9(05)  COMP-3.               
           03  DT-AAMMDD               PIC 9(07)  COMP-3.               
           03  DT-AAAAMMDD             PIC 9(09)  COMP-3.               
           03  TI-HHMMSS               PIC 9(07)  COMP-3.               
           03  TI-HHMMSSMMMMMM         PIC 9(13)  COMP-3.               
           03  TIMESTAMP               PIC X(20).                       
        01  AUX-DATA-DESC.                                              
            03  AUX-ANO-DESC       PIC 9(04)    VALUE ZEROS.            
            03  AUX-MES-DESC       PIC 9(02)    VALUE ZEROS.            
            03  AUX-DIA-DESC       PIC 9(02)    VALUE ZEROS.            
        01  AUX-DATA-DESC-R      REDEFINES    AUX-DATA-DESC PIC 9(08).  
        01  AUX-DT-POOL.                                                
            02  AUX-ANO-POOL  PIC X(04)  VALUE ZEROS.                   
            02  AUX-MES-POOL  PIC X(02)  VALUE ZEROS.                   
            02  AUX-DIA-POOL  PIC X(02)  VALUE ZEROS.                   
        01  AUX-DT-POOL-R     REDEFINES  AUX-DT-POOL PIC 9(08).         
      * POOL1205.                                                     * 
        01  WRK-ROTINA-DE-DATAS-FERI.                                   
            02  WRK-DATA-ENVIADA        PIC 9(09)   COMP-3.             
            02  WRK-UTIL-FERI-INCONS    PIC X(01)   VALUE SPACES.       
            02  WRK-CODIGO-DE-DATA      PIC 9(07)   VALUE ZEROS COMP-3. 
            02  WRK-DATA-COM-BARRAS     PIC X(10)   VALUE SPACES.       
            02  WRK-DATA-COMPACTADA     PIC 9(08)   VALUE ZEROS.        
            02  WRK-DIA-DA-SEMANA       PIC X(13)   VALUE SPACES.       
            02  WRK-NOME-DO-MES         PIC X(09)   VALUE SPACES.       
            02  WRK-DATA-UTIL-ANTERIOR  PIC 9(08)   VALUE ZEROS.        
            02  WRK-DATA-UTIL-POSTERIOR PIC 9(08)   VALUE ZEROS.        
        01  WRK-MENSAGEM                PIC X(050)  VALUE SPACES.       
           EJECT                                                        
      *===============================================================* 
      *    I N I C I O   D A S   A R E A S   D E   I M P R E S S A O  * 
      *===============================================================* 
        01  CABEC1.                                                     
            02  FILLER      PIC X(53)    VALUE                          
            '1*CLLP5840*      LISTAGEM DOS REGISTROS EXCLUIDOS NA '.    
            02  FILLER      PIC X(39)    VALUE                          
            'MONTAGEM DO ARQUIVO - ONLINE        EM '.                  
            02  CB1-DATA.                                               
                03  CB1-DIA   PIC 99/.                                  
                03  CB1-MES   PIC 99/.                                  
                03  CB1-ANO   PIC 9999.                                 
            02  FILLER      PIC X(19)    VALUE  ' '.                    
            02  FILLER      PIC X(06)    VALUE  'FOLHA '.               
            02  CB1-PAG     PIC ZZZZ9.                                  
        01  CABEC2.                                                     
            02  FILLER     PIC X(133)  VALUE                            
            '0    C H A V E  LPCL          ID        C/C    VCTO   DT.EN
      -     'T CL   DT.BASE  DT.ULT.OC.  DT.OPER.  CART   NOME DO DEVEDO
      -     'R'.                                                        
        01  LINDET1.                                                    
            02  FILLER             PIC  X(01)  VALUE ' '.               
            02  LD1-EMPRESA        PIC  9(05)/.                         
            02  LD1-AGENCIA        PIC  9(05)/.                         
            02  LD1-NUMERO-CL      PIC  9(15).                          
            02  FILLER             PIC  X(01)  VALUE '-'.               
            02  LD1-DIGITO-CL      PIC  X(01)B.                         
            02  LD1-ID             PIC  X(02)B.                         
            02  LD1-CONTA-CORR     PIC  Z.ZZZ.ZZ9BB.                    
            02  LD1-VCTO-DIA       PIC  XX.                             
            02  LD1-VCTO-MES       PIC  XX.                             
            02  LD1-VCTO-ANO       PIC  XXXXBB.                         
            02  LD1-DIA-CL         PIC  XX.                             
            02  LD1-MES-CL         PIC  XX.                             
            02  LD1-ANO-CL         PIC  XXXXBB.                         
            02  LD1-DIA-BASE       PIC  XX.                             
            02  LD1-MES-BASE       PIC  XX.                             
            02  LD1-ANO-BASE       PIC  XXXXBBB.                        
            02  LD1-DIA-OCOR       PIC  XX.                             
            02  LD1-MES-OCOR       PIC  XX.                             
            02  LD1-ANO-OCOR       PIC  XXXXBBB.                        
            02  LD1-DIA-OPER       PIC  XX.                             
            02  LD1-MES-OPER       PIC  XX.                             
            02  LD1-ANO-OPER       PIC  XXXXBBB.                        
            02  LD1-CART           PIC  X(03)BBB.                       
            02  LD1-NOME           PIC  X(40).                          
        01  LINTOT1.                                                    
            02  FILLER             PIC  X(22)  VALUE                    
            ' TOTAIS DE REGISTROS- '.                                   
            02  LT1-ATIVOS         PIC  Z.ZZZ.ZZ9.                      
            02  FILLER             PIC  X(14)  VALUE                    
            '- CL/LP ATIVOS'.                                           
        01  LINTOT2.                                                    
            02  FILLER             PIC  X(22)   VALUE SPACES.           
            02  LT2-EXPURGADOS     PIC  Z.ZZZ.ZZ9.                      
            02  FILLER             PIC  X(18)   VALUE                   
            '- CL/LP EXPURGADOS'.                                       
        01  LINTOT3.                                                    
            02  FILLER             PIC  X(22)   VALUE SPACES.           
            02  LT3-MORA           PIC  Z.ZZZ.ZZ9.                      
            02  FILLER             PIC  X(18)   VALUE                   
            '- MORA/DESCONTO   '.                                       
        01  LINTOT4.                                                    
            02  FILLER             PIC  X(22)   VALUE SPACES.           
            02  LT4-TOTAL          PIC  Z.ZZZ.ZZ9.                      
            02  FILLER             PIC  X(11)   VALUE                   
            '- T O T A L'.                                              
        01  LINTOT5.                                                    
            02  FILLER             PIC  X(22)   VALUE SPACES.           
            02  LT5-EXCLUIDOS      PIC  Z.ZZZ.ZZ9.                      
            02  FILLER             PIC  X(11)   VALUE                   
            '- EXCLUIDOS'.                                              
        01  FILLER      PIC X(23)    VALUE 'FIM DA WORKING-STORAGE'.    
           EJECT                                                        
      *===============================================================* 
       PROCEDURE DIVISION.                                              
      *===============================================================* 
      *---------------------------------------------------------------* 
       00000-INICIO-PROCESSAMENTO SECTION.                              
      *---------------------------------------------------------------* 
           OPEN    INPUT   ARQCOMO  ARQLPCL                             
                   OUTPUT  ARQDB2   ARQMUTI                             
                           ARQPRESI RELATO.                             
JAPI-U     CALL  'BRAD7600'  USING  DATA-HORA.                          
           MOVE   DT-AAAAMMDD    TO  AUX-DATA-DESC-R.                   
           MOVE   AUX-DIA-DESC   TO  CB1-DIA.                           
           MOVE   AUX-MES-DESC   TO  CB1-MES.                           
           MOVE   AUX-ANO-DESC   TO  CB1-ANO.                           
           PERFORM 10000-VERIFICA-ARQ-VAZIO.                            
           SORT ARQSORT ASCENDING KEY SORT-AGENCIA                      
                                      SORT-CONTA                        
                                      SORT-CARTEIRA                     
                                      SORT-CONTRATO                     
                                      SORT-VCTO                         
                INPUT  PROCEDURE      20000-PREPARA-SORT                
                OUTPUT PROCEDURE      30000-CONSISTE-DATA               
           PERFORM 40000-IMPRIME-TOTAIS.                                
           PERFORM 50000-GRAVA-REG-VAZIO.                               
           CLOSE  ARQCOMO  ARQLPCL                                      
                  ARQDB2   ARQMUTI                                      
                  ARQPRESI RELATO.                                      
           STOP RUN.                                                    
      *---------------------------------------------------------------* 
       00000-FINALIZA. EXIT.                                            
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       10000-VERIFICA-ARQ-VAZIO SECTION.                                
      *---------------------------------------------------------------* 
           READ   ARQLPCL  AT END                                       
                                MOVE 99999 TO PCL-AGENCIA.              
           IF     PCL-AGENCIA   EQUAL 99999                             
            DISPLAY  '*******************************' UPON CONSOLE     
            DISPLAY  '*      SR. OPERADOR           *' UPON CONSOLE     
            DISPLAY  '*                             *' UPON CONSOLE     
            DISPLAY  '*   O  ARQUIVO  ARQLPCL       *' UPON CONSOLE     
            DISPLAY  '*                             *' UPON CONSOLE     
            DISPLAY  '*      ESTA VAZIO             *' UPON CONSOLE     
            DISPLAY  '*                             *' UPON CONSOLE     
            DISPLAY  '* FAVOR INFORMAR O ANALISTA   *' UPON CONSOLE     
            DISPLAY  '*******************************' UPON CONSOLE.    
           READ   ARQCOMO  AT END                                       
STF                           MOVE 99999 TO PVE-AGENCIA.                
STF        IF     PVE-AGENCIA   EQUAL 99999                             
            DISPLAY  '********************************' UPON CONSOLE    
            DISPLAY  '*     SR. OPERADOR             *' UPON CONSOLE    
            DISPLAY  '*                              *' UPON CONSOLE    
            DISPLAY  '*  O  ARQUIVO  ARQCOMO         *' UPON CONSOLE    
            DISPLAY  '*                              *' UPON CONSOLE    
            DISPLAY  '*     ESTA VAZIO               *' UPON CONSOLE    
            DISPLAY  '*                              *' UPON CONSOLE    
            DISPLAY  '*  FAVOR INFORMAR O ANALISTA   *' UPON CONSOLE    
            DISPLAY  '********************************' UPON CONSOLE.   
      *---------------------------------------------------------------* 
       10000-VERIFICOU-ARQUIVOS. EXIT.                                  
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       20000-PREPARA-SORT SECTION.                                      
      *---------------------------------------------------------------* 
           PERFORM 21000-GRAVAR-ARQLPCL UNTIL                           
                                            PCL-AGENCIA EQUAL 99999.    
           PERFORM 22000-FORMATAR-ARQCOMO UNTIL                         
STF                                         PVE-AGENCIA EQUAL 99999.    
      *---------------------------------------------------------------* 
       20000-PREPAROU-SORT. EXIT.                                       
      *---------------------------------------------------------------* 
           SKIP3                                                        
      *---------------------------------------------------------------* 
       21000-GRAVAR-ARQLPCL SECTION.                                    
      *---------------------------------------------------------------* 
           MOVE     PCL-REGTO      TO   SORT-REGTO.                     
           RELEASE  SORT-REGTO.                                         
           READ     ARQLPCL   AT END                                    
                              MOVE 99999 TO PCL-AGENCIA.                
      *---------------------------------------------------------------* 
       21000-GRAVOU-ARQLPCL. EXIT.                                      
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       22000-FORMATAR-ARQCOMO  SECTION.                                 
      *---------------------------------------------------------------* 
                PERFORM   99000-LIMPA-REG-SORT.                         
                IF       PVE-TIPREG       EQUAL '22'                    
                         MOVE 5000           TO  SORT-EMPRESA           
                ELSE                                                    
                         MOVE ZEROS          TO  SORT-EMPRESA.          
STF             MOVE      PVE-AGENCIA        TO  SORT-AGENCIA.          
                MOVE      ZEROS              TO  SORT-NUM-CL.           
STF             MOVE      PVE-ID-PRECL       TO  SORT-DIG-CL.           
                MOVE      7                  TO  DIGITO.                
JAPI-I          CALL 'BRAD0431' USING   PVE-CONTA                       
                                                 DIGITO                 
                                                 TAM-VALOR.             
STF             MOVE      PVE-CONTA          TO  SORT-CONTA.            
                MOVE      DIGITO             TO  SORT-DIG.              
BRQ=E***********IF       PVE-CARTEIRA   IS NUMERIC                      
STF                      MOVE PVE-CARTEIRA   TO  SORT-CARTEIRA-R        
                                                 SORT-CARTEIRA-ORIGEM-R 
BRQ=E***********ELSE                                                    
BRQ=E***********         MOVE PVE-CARTEIRA-R TO  SORT-CARTEIRA          
BRQ=E***********                                 SORT-CARTEIRA-ORIGEM.  
STF             MOVE      PVE-CONTRATO       TO  SORT-CONTRATO.         
STF             MOVE      PVE-DEPT-CAMBIO    TO  SORT-DEP-CAMBIO.       
STF             MOVE      PVE-DT-VECTO       TO  AUX-DATA-DESC-R.       
                MOVE      AUX-ANO-DESC       TO  SORT-VCTO-ANO.         
                MOVE      AUX-MES-DESC       TO  SORT-VCTO-MES.         
                MOVE      AUX-DIA-DESC       TO  SORT-VCTO-DIA.         
STF             IF (PVE-DT-OPER NUMERIC) AND (PVE-DT-OPER GREATER ZEROS)
STF                 MOVE      PVE-DT-OPER    TO  AUX-DATA-DESC-R        
                    MOVE      AUX-ANO-DESC   TO  SORT-OPER-ANO          
                    MOVE      AUX-MES-DESC   TO  SORT-OPER-MES          
                    MOVE      AUX-DIA-DESC   TO   SORT-OPER-DIA         
                ELSE                                                    
                    MOVE      '0001.01.01'   TO   SORT-OPER.            
                                                                        
280305          IF  PVE-ID-PRECL        EQUAL     '7'                   
280305              MOVE          'LP'       TO   SORT-ID               
280305          ELSE                                                    
280305              MOVE          'MO'       TO   SORT-ID               
280305          END-IF.                                                 
                                                                        
                MOVE      SORT-VCTO-2        TO   SORT-MORA-2           
                                                  SORT-ENTR-2           
                                                  SORT-OCOR-2.          
                MOVE      SPACES             TO   SORT-MOEDA            
                                                  SORT-LOCAL.           
STF             MOVE      PVE-GARANTIA       TO   WRK-GARANTIA.         
STF             MOVE      WRK-GARANTIA       TO   SORT-TIPO-GAR.        
STF             IF        PVE-ID-PRECL    EQUAL   '9'                   
                          MOVE    ' S'       TO   SORT-MOEDA.           
                MOVE      ZEROS              TO   SORT-TIPO-PEND.       
                MOVE      SPACES             TO   SORT-SIGLA.           
                MOVE      ZEROS              TO   SORT-TELEFONE         
                                                  SORT-NUMERO-ADV       
                                                  SORT-CTR              
                                                  SORT-CODIGO.          
                MOVE      SPACES             TO   SORT-STATUS.          
BRQ=E***********IF        PVE-VR-CONTABIL    IS   NUMERIC               
BRQ=E*****           MOVE PVE-VR-CONTABIL    TO   SORT-CONTABIL         
BRQ=E***********ELSE                                                    
                     MOVE ZEROS              TO   SORT-CONTABIL.        
                MOVE      PVE-VR-PARCELA     TO   SORT-LIQUIDO          
                                                  SORT-DEV-INIC         
                                                  SORT-PRINCIPAL.       
STF             MOVE      PVE-VR-DEBITO      TO   SORT-COBRANCA.        
STF             MOVE      PVE-NOME           TO   SORT-NOME-DEVEDOR.    
      *---------------------------------------------------------------* 
      *         FORMATA CGC/CPF DO DEVEDOR ORIUNDO DA ROTINA MORA     * 
      *---------------------------------------------------------------* 
STF             MOVE      PVE-CGCNUM         TO   SORT-NUMERO-DEV.      
STF             MOVE      PVE-CGCFIL         TO   SORT-FILIAL.          
STF             MOVE      PVE-CGCCTR         TO   SORT-CTR-DEV.         
      *                                                                 
STF             MOVE PVE-NOME-AVAL1          TO   SORT-NOME-AVALISTA.   
STF             MOVE PVE-NUMERO-AVAL1        TO   SORT-NUMERO-AVAL.     
STF             MOVE PVE-FILIAL-AVAL1        TO   SORT-FILIAL-AVAL.     
STF             MOVE PVE-CTR-AVAL1           TO   SORT-CTR-AVAL.        
STF             MOVE PVE-NOME-AVAL2          TO   SORT-NOME-AVAL2.      
STF             MOVE PVE-NUMERO-AVAL2        TO   SORT-NUMERO-AVAL2.    
STF             MOVE PVE-FILIAL-AVAL2        TO   SORT-FILIAL-AVAL2.    
STF             MOVE PVE-CTR-AVAL2           TO   SORT-CTR-AVAL2.       
                MOVE     ZEROS               TO   SORT-EXEC.            
STF             MOVE     PVE-DIRREG          TO   SORT-REGIONAL.        
STF             MOVE     PVE-JUNC-DIRREG     TO   SORT-COD-JUNCAO.      
STF             MOVE     PVE-RAMO-ATIV       TO   SORT-RAMO.            
                MOVE     'N'                 TO   SORT-BLOQUEIO-TRANSF. 
                MOVE     '0001.01.01'        TO   SORT-DT-PDD-180       
                                                  SORT-DT-RETORNO-CL    
                                                  SORT-DT-PRIM-TRANSF-LP
                MOVE     SPACES              TO   SORT-MARCA-PDD-180    
                                                  SORT-MARCA-IMPE       
                                                  SORT-STATUS-REATIVACAO
                MOVE     ZEROS               TO   SORT-RAZAO-PRINCIP    
                                                  SORT-RAZAO-RENDAS     
                                                  SORT-VR-VENCIDOS      
                                                  SORT-VR-VINCENDOS     
                                              SORT-VR-ENC-VENCIDOS-CONG.
STF             MOVE     PVE-PRODUTO         TO   SORT-PRODUTO-EMPF.    
STF             MOVE     PVE-FAMILIA         TO   SORT-FAMILIA-EMPF.    
                MOVE     '0001.01.01'        TO   SORT-DATA-AJUIZAMENTO.
                MOVE     ZEROS               TO   SORT-IOF-NORMAL       
                                                  SORT-IOF-COMPL        
                                                  SORT-ALIQ-CONT-RECOL  
                                                  SORT-ALIQ-REC-TR-CL   
                                                  SORT-ALIQ-COMPL.      
                MOVE     '01.01.0001'        TO   SORT-DATA-SISTEL.     
                MOVE     'N'                 TO   SORT-IND-SISTEL       
                                                  SORT-NOTIF-SISTEL.    
                MOVE     ZEROS               TO   SORT-AGRESP.          
                                                                        
       22100-ZERAR-OCORRENCIAS-SORT.                                    
      ******************************                                    
            ADD    1           TO      IND-TAB.                         
            MOVE   ZEROS       TO      SORT-OCCORS(IND-TAB)             
            IF     IND-TAB   GREATER   49                               
                   MOVE ZEROS  TO      IND-TAB                          
                   GO          TO      22100-ZEROU.                     
             GO TO  22100-ZERAR-OCORRENCIAS-SORT.                       
       22100-ZEROU.                                                     
      *************                                                     
            RELEASE  SORT-REGTO.                                        
            READ  ARQCOMO  AT END                                       
STF                            MOVE 99999  TO  PVE-AGENCIA.             
      *---------------------------------------------------------------* 
       22000-FORMATOU-ARQCOMO. EXIT.                                    
      *---------------------------------------------------------------* 
         EJECT                                                          
      *---------------------------------------------------------------* 
       30000-CONSISTE-DATA SECTION.                                     
      *---------------------------------------------------------------* 
           RETURN   ARQSORT  AT END                                     
                    MOVE 99999     TO   SORT-AGENCIA                    
                    GO TO 30000-CONSISTIU-DATA.                         
           MOVE     SORT-OPER-DIA  TO   AUX-DIA-POOL                    
           MOVE     SORT-OPER-MES  TO   AUX-MES-POOL                    
           MOVE     SORT-OPER-ANO  TO   AUX-ANO-POOL                    
           MOVE     AUX-DT-POOL-R  TO   WRK-DATA-ENVIADA                
JAPI-I     CALL     'BRAD1205' USING                                    
                                        WRK-ROTINA-DE-DATAS-FERI        
                                        WRK-MENSAGEM                    
           IF  WRK-UTIL-FERI-INCONS     EQUAL   'I'                     
               MOVE  '0001.01.01'  TO   SORT-OPER.                      
           IF SORT-VCTO-ANO             EQUAL 9999                      
              MOVE 2099            TO   SORT-VCTO-ANO.                  
           MOVE     SORT-VCTO-DIA  TO   AUX-DIA-POOL.                   
           MOVE     SORT-VCTO-MES  TO   AUX-MES-POOL.                   
           MOVE     SORT-VCTO-ANO  TO   AUX-ANO-POOL.                   
           MOVE     AUX-DT-POOL-R  TO   WRK-DATA-ENVIADA                
JAPI-I     CALL     'BRAD1205' USING                                    
                                        WRK-ROTINA-DE-DATAS-FERI        
                                        WRK-MENSAGEM                    
           IF  WRK-UTIL-FERI-INCONS     EQUAL   'I'                     
               ADD   1 TO  ACU-EXCLUIDOS                                
               PERFORM 31000-IMPRESSAO                                  
           ELSE                                                         
               MOVE SORT-ENTR-DIA  TO   AUX-DIA-POOL                    
               MOVE SORT-ENTR-MES  TO   AUX-MES-POOL                    
               MOVE SORT-ENTR-ANO  TO   AUX-ANO-POOL                    
               MOVE AUX-DT-POOL-R  TO   WRK-DATA-ENVIADA                
JAPI-I         CALL 'BRAD1205' USING                                    
                                        WRK-ROTINA-DE-DATAS-FERI        
                                        WRK-MENSAGEM                    
               IF  WRK-UTIL-FERI-INCONS   EQUAL   'I'                   
                   ADD   1 TO  ACU-EXCLUIDOS                            
                   PERFORM 31000-IMPRESSAO                              
               ELSE                                                     
                   MOVE SORT-MORA-DIA  TO    AUX-DIA-POOL               
                   MOVE SORT-MORA-MES  TO    AUX-MES-POOL               
                   MOVE SORT-MORA-ANO  TO    AUX-ANO-POOL               
                   MOVE AUX-DT-POOL-R  TO    WRK-DATA-ENVIADA           
JAPI-I             CALL 'BRAD1205' USING                                
                                             WRK-ROTINA-DE-DATAS-FERI   
                                             WRK-MENSAGEM               
                   IF  WRK-UTIL-FERI-INCONS   EQUAL   'I'               
                       ADD   1 TO  ACU-EXCLUIDOS                        
                       PERFORM 31000-IMPRESSAO                          
                   ELSE                                                 
                     MOVE     SORT-OCOR-DIA  TO    AUX-DIA-POOL         
                     MOVE     SORT-OCOR-MES  TO    AUX-MES-POOL         
                     MOVE     SORT-OCOR-ANO  TO    AUX-ANO-POOL         
                     MOVE AUX-DT-POOL-R      TO WRK-DATA-ENVIADA        
JAPI-I               CALL 'BRAD1205' USING                              
                                             WRK-ROTINA-DE-DATAS-FERI   
                                             WRK-MENSAGEM               
                     IF  WRK-UTIL-FERI-INCONS   EQUAL   'I'             
                         ADD   1 TO  ACU-EXCLUIDOS                      
                         PERFORM 31000-IMPRESSAO                        
                     ELSE                                               
                         PERFORM 32000-COMPARA-CART                     
                         IF SORT-ID   EQUAL 'CL' OR 'LP'                
                            ADD  1 TO ACU-ATIVOS                        
                            MOVE SORT-REGTO  TO DB2-REGTO               
                            WRITE  DB2-REGTO                            
                         ELSE                                           
                            IF  SORT-ID EQUAL 'EX'                      
                                ADD 1  TO    ACU-EXPURGADOS             
                                MOVE SORT-REGTO  TO DB2-REGTO           
                                WRITE  DB2-REGTO                        
                            ELSE                                        
                               IF  SORT-ID  EQUAL 'MO'                  
                                   ADD  1  TO    ACU-MORA               
                                   MOVE SORT-REGTO   TO DB2-REGTO       
                                   COMPUTE DB2-JUROS-MORA =             
                                            SORT-COBRANCA - SORT-LIQUIDO
                                   WRITE  DB2-REGTO.                    
            GO TO 30000-CONSISTE-DATA.                                  
      *---------------------------------------------------------------* 
       30000-CONSISTIU-DATA. EXIT.                                      
      *---------------------------------------------------------------* 
           SKIP3                                                        
      *---------------------------------------------------------------* 
       31000-IMPRESSAO SECTION.                                         
      *---------------------------------------------------------------* 
           IF  ACU-LIN  GREATER 59                                      
               ADD     1          TO    ACU-PAG                         
               MOVE  ZEROS        TO    ACU-LIN                         
               MOVE  ACU-PAG      TO    CB1-PAG                         
               WRITE REL-REGTO   FROM   CABEC1                          
               WRITE REL-REGTO   FROM   CABEC2                          
               MOVE SPACES        TO    REL-REGTO                       
               WRITE REL-REGTO.                                         
           MOVE  SORT-EMPRESA     TO    LD1-EMPRESA.                    
           MOVE  SORT-AGENCIA     TO    LD1-AGENCIA.                    
           MOVE  SORT-NUM-CL      TO    LD1-NUMERO-CL.                  
           MOVE  SORT-DIG-CL      TO    LD1-DIGITO-CL.                  
           MOVE  SORT-ID          TO    LD1-ID.                         
           MOVE  SORT-CONTA       TO    LD1-CONTA-CORR.                 
           MOVE  SORT-VCTO-DIA    TO    LD1-VCTO-DIA.                   
           MOVE  SORT-VCTO-MES    TO    LD1-VCTO-MES.                   
           MOVE  SORT-VCTO-ANO    TO    LD1-VCTO-ANO.                   
           MOVE  SORT-ENTR-DIA    TO    LD1-DIA-CL.                     
           MOVE  SORT-ENTR-MES    TO    LD1-MES-CL.                     
           MOVE  SORT-ENTR-ANO    TO    LD1-ANO-CL.                     
           MOVE  SORT-MORA-DIA    TO    LD1-DIA-BASE.                   
           MOVE  SORT-MORA-MES    TO    LD1-MES-BASE.                   
           MOVE  SORT-MORA-ANO    TO    LD1-ANO-BASE.                   
           MOVE  SORT-OCOR-DIA    TO    LD1-DIA-OCOR.                   
           MOVE  SORT-OCOR-MES    TO    LD1-MES-OCOR.                   
           MOVE  SORT-OCOR-ANO    TO    LD1-ANO-OCOR.                   
           MOVE  SORT-OPER-DIA    TO    LD1-DIA-OPER.                   
           MOVE  SORT-OPER-MES    TO    LD1-MES-OPER.                   
           MOVE  SORT-OPER-ANO    TO    LD1-ANO-OPER.                   
           MOVE  SORT-CARTEIRA    TO    LD1-CART.                       
           MOVE  SORT-NOME-DEVEDOR TO   LD1-NOME.                       
           WRITE REL-REGTO       FROM   LINDET1.                        
           ADD  1 TO  ACU-LIN.                                          
      *---------------------------------------------------------------* 
       31000-IMPRIMIU. EXIT.                                            
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       32000-COMPARA-CART SECTION.                                      
      *---------------------------------------------------------------* 
BRQ=E******IF SORT-CARTEIRA IS NUMERIC                                  
BRQ=E******   IF (SORT-CARTEIRA-R GREATER 499) AND                      
BRQ=E******      (SORT-CARTEIRA-R LESS 600)                             
BRQ=I         IF (SORT-CARTEIRA-R GREATER '499') AND                    
BRQ=I            (SORT-CARTEIRA-R LESS '600')                           
                 PERFORM 32100-GRAVA-PI                                 
                 MOVE SORT-AGENCIA    TO AUX-AGE-SRT                    
                 MOVE SORT-CONTA      TO AUX-CONTA-SRT                  
                 MOVE SORT-CARTEIRA-R TO AUX-CART-SRT                   
                 MOVE SORT-CONTRATO   TO AUX-CONTR-SRT                  
                 IF CHAVE-AUX-MI NOT EQUAL CHAVE-AUX-SRT                
                    PERFORM 32200-GRAVA-MI.                             
      *---------------------------------------------------------------* 
       32000-FIM-COMPARA-CART. EXIT.                                    
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       32100-GRAVA-PI SECTION.                                          
      *---------------------------------------------------------------* 
           MOVE SORT-CONTRATO TO ARQI-CONTRATO                          
           MOVE SORT-ID       TO ARQI-ID                                
           MOVE SORT-VCTO-ANO TO AUX-ANO-PI.                            
           MOVE SORT-VCTO-MES TO AUX-MES-PI.                            
           MOVE SORT-VCTO-DIA TO AUX-DIA-PI.                            
           MOVE AUX-DATA-PI-R TO ARQI-VENCTO-PRES.                      
           MOVE SORT-LIQUIDO  TO ARQI-VLR-PARC.                         
           COMPUTE ARQI-VLR-JRS-MORA =                                  
                    SORT-COBRANCA - SORT-LIQUIDO.                       
           IF SORT-ID EQUAL  'MO'                                       
              MOVE SORT-DIG-CL TO ARQI-COD-BLOQ                         
           ELSE                                                         
              MOVE SPACES  TO ARQI-COD-BLOQ.                            
           IF SORT-LIQUIDO EQUAL SORT-COBRANCA                          
              MOVE 'N' TO ARQI-COD-COBRA-JRS                            
           ELSE                                                         
              MOVE 'S' TO ARQI-COD-COBRA-JRS.                           
           MOVE SORT-RAMO TO ARQI-COD-RAMO.                             
           MOVE SORT-CARTEIRA-R TO ARQI-COD-CART.                       
           WRITE REG-PARC-INAD-SAI.                                     
      *---------------------------------------------------------------* 
       32100-FIM-GRAVA-PI. EXIT.                                        
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       32200-GRAVA-MI SECTION.                                          
      *---------------------------------------------------------------* 
           MOVE  SORT-CONTRATO     TO  ARQMI-CONTRATO.                  
           MOVE  SORT-AGENCIA      TO  ARQMI-AGENCIA.                   
           MOVE  SORT-CONTA        TO  ARQMI-CTA-CORR.                  
           MOVE  SORT-NOME-DEVEDOR TO  ARQMI-NOME.                      
           MOVE  SORT-ID           TO  ARQMI-ID.                        
           WRITE REG-CAD-EN.                                            
           MOVE  SORT-AGENCIA      TO  AUX-AGE-MI.                      
           MOVE  SORT-CONTA        TO  AUX-CONTA-MI.                    
           MOVE  SORT-CARTEIRA-R   TO  AUX-CART-MI.                     
           MOVE  SORT-CONTRATO     TO  AUX-CONTR-MI.                    
      *---------------------------------------------------------------* 
       32200-FIM-GRAVA-MI. EXIT.                                        
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       40000-IMPRIME-TOTAIS SECTION.                                    
      *---------------------------------------------------------------* 
           IF ACU-LIN  GREATER 59                                       
              ADD    1             TO    ACU-PAG                        
              MOVE  ZEROS          TO    ACU-LIN                        
              MOVE  ACU-PAG        TO    CB1-PAG                        
              WRITE REL-REGTO     FROM   CABEC1                         
              WRITE REL-REGTO     FROM   CABEC2.                        
           MOVE  ACU-ATIVOS        TO    LT1-ATIVOS.                    
           MOVE  ACU-EXPURGADOS    TO    LT2-EXPURGADOS.                
           MOVE  ACU-MORA          TO    LT3-MORA.                      
           COMPUTE LT4-TOTAL = ACU-ATIVOS + ACU-EXPURGADOS + ACU-MORA.  
           MOVE  ACU-EXCLUIDOS     TO LT5-EXCLUIDOS.                    
           WRITE REL-REGTO  FROM LINTOT1.                               
           WRITE REL-REGTO  FROM LINTOT2.                               
           WRITE REL-REGTO  FROM LINTOT3.                               
           WRITE REL-REGTO  FROM LINTOT4.                               
           WRITE REL-REGTO  FROM LINTOT5.                               
      *---------------------------------------------------------------* 
       40000-IMPRIMIU. EXIT.                                            
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       50000-GRAVA-REG-VAZIO SECTION.                                   
      *---------------------------------------------------------------* 
           IF (AUX-AGE-SRT EQUAL ZEROS)   AND                           
              (AUX-CONTA-SRT EQUAL ZEROS) AND                           
BRQ=E*********(AUX-CART-SRT EQUAL ZEROS)  AND                           
BRQ=I         (AUX-CART-SRT EQUAL '000')  AND                           
              (AUX-CONTR-SRT EQUAL ZEROS)                               
              MOVE 999999999999999  TO ARQI-VLR-PARC                    
                                       ARQI-VLR-JRS-MORA                
              MOVE 999999999        TO ARQI-VENCTO-PRES                 
              MOVE 9999999          TO ARQI-CONTRATO                    
                                       ARQMI-CONTRATO                   
                                       ARQMI-CTA-CORR                   
BRQ=E*********MOVE 99999            TO ARQI-COD-CART                    
BRQ=E*********                         ARQMI-AGENCIA                    
BRQ=I         MOVE '99999'          TO ARQI-COD-CART                    
BRQ=I         MOVE 99999            TO ARQMI-AGENCIA                    
              MOVE SPACES           TO ARQI-ID                          
                                       ARQI-COD-BLOQ                    
                                       ARQI-COD-COBRA-JRS               
                                       ARQI-COD-RAMO                    
                                       ARQMI-NOME                       
                                       ARQMI-ID                         
              WRITE REG-CAD-EN                                          
              WRITE REG-PARC-INAD-SAI.                                  
      *---------------------------------------------------------------* 
       50000-FIM-GRAVA-REG-VAZIO. EXIT.                                 
      *---------------------------------------------------------------* 
           EJECT                                                        
      *---------------------------------------------------------------* 
       99000-LIMPA-REG-SORT  SECTION.                                   
      *---------------------------------------------------------------* 
           MOVE   ZEROS    TO    SORT-EMPRESA                           
           MOVE   ZEROS    TO    SORT-AGENCIA                           
           MOVE   ZEROS    TO    SORT-NUM-CL                            
           MOVE   SPACES   TO    SORT-DIG-CL                            
           MOVE   ZEROS    TO    SORT-CONTA                             
           MOVE   SPACES   TO    SORT-DIG                               
           MOVE   SPACES   TO    SORT-CARTEIRA.                         
           MOVE   ZEROS    TO    SORT-CONTRATO                          
           MOVE   SPACES   TO    SORT-VCTO                              
                                 SORT-OPER.                             
           MOVE   SPACES   TO    SORT-ID                                
           MOVE   SPACES   TO    SORT-ENTR-2                            
           MOVE   SPACES   TO    SORT-MORA-2.                           
           MOVE   SPACES   TO    SORT-MOEDA                             
                                 SORT-TIPO-GAR                          
                                 SORT-LOCAL                             
           MOVE  ZEROS     TO    SORT-TIPO-PEND                         
           MOVE  SPACES    TO    SORT-SIGLA                             
           MOVE  ZEROS     TO    SORT-TELEFONE                          
                                 SORT-NUMERO-ADV                        
                                 SORT-CTR                               
                                 SORT-CODIGO                            
           MOVE   SPACES   TO    SORT-OCOR-2.                           
           MOVE   SPACES   TO    SORT-STATUS                            
                                 SORT-DATA-VENC-CARENCIA                
           MOVE  ZEROS     TO                                           
                   SORT-DEV-INIC                                        
                   SORT-PRINCIPAL                                       
                   SORT-CONTABIL                                        
                   SORT-LIQUIDO                                         
                   SORT-COBRANCA                                        
                   SORT-COBRANCA-INF                                    
                   SORT-JUROS-MORA                                      
                   SORT-CORR-MONET                                      
                   SORT-JUROS-12AA                                      
                   SORT-TOTAL-CONTRATO                                  
           MOVE    SPACES  TO    SORT-CARTEIRA-ORIGEM                   
                                 SORT-COD-NATUREZA-OPER                 
                                 SORT-NOME-DEVEDOR                      
           MOVE    ZEROS   TO                                           
                   SORT-NUMERO-DEV                                      
                   SORT-FILIAL                                          
                   SORT-CTR-DEV                                         
           MOVE   SPACES   TO    SORT-NOME-AVALISTA                     
           MOVE   ZEROS    TO    SORT-NUMERO-AVAL                       
                                 SORT-FILIAL-AVAL                       
                                 SORT-CTR-AVAL                          
           MOVE   SPACES   TO    SORT-NOME-AVAL2                        
           MOVE   ZEROS    TO    SORT-NUMERO-AVAL2                      
                                 SORT-FILIAL-AVAL2                      
                                 SORT-CTR-AVAL2                         
           MOVE   'N'      TO    SORT-BLOQUEIO-TRANSF                   
           MOVE   ZEROS    TO    SORT-EXEC                              
           MOVE   ZEROS    TO    SORT-REGIONAL                          
                                 SORT-COD-JUNCAO                        
                                 SORT-DEP-CAMBIO                        
                                 SORT-COD-EMPR                          
           MOVE   ZEROS    TO    SORT-OCCORS(1)                         
           MOVE   ZEROS    TO    SORT-OCCORS(2)                         
           MOVE   ZEROS    TO    SORT-OCCORS(3)                         
           MOVE   ZEROS    TO    SORT-OCCORS(4)                         
           MOVE   ZEROS    TO    SORT-OCCORS(5)                         
           MOVE   ZEROS    TO    SORT-OCCORS(6)                         
           MOVE   ZEROS    TO    SORT-OCCORS(7)                         
           MOVE   ZEROS    TO    SORT-OCCORS(8)                         
           MOVE   ZEROS    TO    SORT-OCCORS(9)                         
           MOVE   ZEROS    TO    SORT-OCCORS(10)                        
           MOVE   ZEROS    TO    SORT-OCCORS(11)                        
           MOVE   ZEROS    TO    SORT-OCCORS(12)                        
           MOVE   ZEROS    TO    SORT-OCCORS(13)                        
           MOVE   ZEROS    TO    SORT-OCCORS(14)                        
           MOVE   ZEROS    TO    SORT-OCCORS(15)                        
           MOVE   ZEROS    TO    SORT-OCCORS(16)                        
           MOVE   ZEROS    TO    SORT-OCCORS(17)                        
           MOVE   ZEROS    TO    SORT-OCCORS(18)                        
           MOVE   ZEROS    TO    SORT-OCCORS(19)                        
           MOVE   ZEROS    TO    SORT-OCCORS(20)                        
           MOVE   ZEROS    TO    SORT-OCCORS(21)                        
           MOVE   ZEROS    TO    SORT-OCCORS(22)                        
           MOVE   ZEROS    TO    SORT-OCCORS(23)                        
           MOVE   ZEROS    TO    SORT-OCCORS(24)                        
           MOVE   ZEROS    TO    SORT-OCCORS(25)                        
           MOVE   ZEROS    TO    SORT-OCCORS(26)                        
           MOVE   ZEROS    TO    SORT-OCCORS(27)                        
           MOVE   ZEROS    TO    SORT-OCCORS(28)                        
           MOVE   ZEROS    TO    SORT-OCCORS(29)                        
           MOVE   ZEROS    TO    SORT-OCCORS(30)                        
           MOVE   ZEROS    TO    SORT-OCCORS(31)                        
           MOVE   ZEROS    TO    SORT-OCCORS(32)                        
           MOVE   ZEROS    TO    SORT-OCCORS(33)                        
           MOVE   ZEROS    TO    SORT-OCCORS(34)                        
           MOVE   ZEROS    TO    SORT-OCCORS(35)                        
           MOVE   ZEROS    TO    SORT-OCCORS(36)                        
           MOVE   ZEROS    TO    SORT-OCCORS(37)                        
           MOVE   ZEROS    TO    SORT-OCCORS(38)                        
           MOVE   ZEROS    TO    SORT-OCCORS(39)                        
           MOVE   ZEROS    TO    SORT-OCCORS(40)                        
           MOVE   ZEROS    TO    SORT-OCCORS(41)                        
           MOVE   ZEROS    TO    SORT-OCCORS(42)                        
           MOVE   ZEROS    TO    SORT-OCCORS(43)                        
           MOVE   ZEROS    TO    SORT-OCCORS(44)                        
           MOVE   ZEROS    TO    SORT-OCCORS(45)                        
           MOVE   ZEROS    TO    SORT-OCCORS(46)                        
           MOVE   ZEROS    TO    SORT-OCCORS(47)                        
           MOVE   ZEROS    TO    SORT-OCCORS(48)                        
           MOVE   ZEROS    TO    SORT-OCCORS(49)                        
           MOVE   ZEROS    TO    SORT-OCCORS(50)                        
           MOVE   SPACES   TO    SORT-RAMO.                             
           MOVE   '0001.01.01' TO SORT-DT-RETORNO-CL                    
                                 SORT-DT-PRIM-TRANSF-LP.                
           MOVE   ZEROS    TO    SORT-VR-ENC-VENCIDOS-CONG              
                                 SORT-PRODUTO-EMPF                      
                                 SORT-FAMILIA-EMPF.                     
           MOVE   SPACES   TO    SORT-STATUS-REATIVACAO.                
           MOVE   '01.01.0001' TO SORT-DATA-SISTEL.                     
           MOVE   'N'      TO    SORT-IND-SISTEL                        
                                 SORT-NOTIF-SISTEL.                     
      *---------------------------------------------------------------* 
       99000-FIM-LIMPA-REG-SORT. EXIT.                                  
      *---------------------------------------------------------------* 

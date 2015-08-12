*********************** I#RDAB21 ***************************            
*            RENEGOCIACAO AUTOMATICA DE DIVIDAS            *            
*                                                          *            
* DESCRICAO        - LAY-OUT DOS DADOS DA SIMULACAO        *            
* NOME DA INC      - I#RDAB21   LRECL - 2500               *            
* AUTOR            - ELIANE FAGUNDES                       *            
* DATA ATUALIZACAO - JANEIRO/2003                          *            
*                                                          *            
*----------------------------------------------------------*            
*                                                          *            
* - SIMU-CHAMADOR        - NOME DO PGM CHAMADOR            *            
* - SIMU-TP-PROCESSO     - 1-CONSULTA / 2-ATUALIZACAO      *            
* - SIMU-TP-CONSULTA     - 1-LISTA / 2-PELA CHAVE /        *            
*                          3-HISTORICO                     *            
* - SIMU-STATUS-PROCESSO - 1-OK / 2-ERRO                   *            
* - SIMU-MSG-ERRO        - MENSAGEM DE ERRO FORMATADA      *            
* - SIMU-TP-APLICATIVO   - 1-SBAT / 2-IMS                  *            
* - SIMU-QTDE-OCORR-REST - QTDE DE OCORRENCIAS PARA RESTART*            
* - SIMU-RT-F1-QTDE-OCORR-TAB - QTDE DE OCORR QUE CABEM NA *            
*                               TAB. DA FASE 1             *            
* - SIMU-RT-F2-QTDE-OCORR-TAB - QTDE DE OCORR QUE CABEM NA *            
*                               TAB. DA FASE 2             *            
* - SIMU-RT-F3-QTDE-OCORR-TAB - QTDE DE OCORR QUE CABEM NA *            
*                               TAB. DA FASE 3             *            
* - SIMU-RT-F4-QTDE-OCORR-TAB - QTDE DE OCORR QUE CABEM NA *            
*                               TAB. DA FASE 4             *            
*                                                          *            
*----------------------------------------------------------*            
*                                                          *            
*   CAMPOS OBRIGATORIOS DE ENVIO AO MODULO (POR FASE)      *            
*   =======================================                *            
*                                                          *            
*   - PARA CONTROLE NRO SIMULADO :                         *            
*     SIMU-ENRT-DATA                                       *            
*                                                          *            
*   - PARA CONSULTA DOS DADOS DA SIMULACAO :               *            
*                                                          *            
*     SIMU-EN-IND-FASE EH OBRIGATORIO EM TODAS AS FASES    *            
*     DEVERA SER 1/2/3/4                                   *            
*                                                          *            
*   FASE 1                                                 *            
*     SIMU-EN-CGC-CPF                                      *            
*                                                          *            
*   FASE 2                                                 *            
*     SIMU-EN-CGC-CPF                                      *            
*     SIMU-EN-CD-SIMU                                      *            
*                                                          *            
*   FASE 3 E 4                                             *            
*     SIMU-EN-CD-SIMU                                      *            
*                                                          *            
*----------------------------------------------------------*            
*   AREAS DE CONTROLE DA TABELA DE RETORNO                 *            
*   ========================================               *            
*                                                          *            
*   QTDE-OCORR     - QTDE DE OCORRENCIAS QUE FORAM         *            
*                    FORMATADAS                            *            
*   QTDE-MAX       - QTDE MAXIMA DE OCORRENCIAS QUE FORAM  *            
*                    ENCONTRADAS NA TABELA                 *            
*                                                          *            
************************************************************            
                                                                        
      01 SIMLACAO-RENEG.                                                
         03  SIMU-CHAMADOR                         PIC X(08).           
         03  SIMU-TP-PROCESSO                      PIC X(01).           
         03  SIMU-TP-CONSULTA                      PIC X(01).           
         03  SIMU-STATUS-PROCESSO                  PIC X(01).           
         03  SIMU-MSG-ERRO                         PIC X(80).           
         03  SIMU-TP-APLICATIVO                    PIC X(01).           
         03  SIMU-QTDE-OCORR-REST                  PIC 9(05) COMP-3.    
         03  SIMU-RT-F1-QTDE-OCORR-TAB PIC 9(03) COMP-3 VALUE 75.       
         03  SIMU-RT-F2-QTDE-OCORR-TAB PIC 9(03) COMP-3 VALUE 23.       
         03  SIMU-RT-F3-QTDE-OCORR-TAB PIC 9(03) COMP-3 VALUE 46.       
         03  SIMU-RT-F4-QTDE-OCORR-TAB PIC 9(03) COMP-3 VALUE 50.       
         03  SIMU-RESERVA                          PIC X(47).           
         03  SIMU-FILLER                           PIC X(2350).         
                                                                        
         03  SIMU-ENVIO-RETORNO REDEFINES SIMU-FILLER.                  
             04  SIMU-ENRT-DATA                    PIC X(10).           
             04  SIMU-ENRT-NR-SIMU                 PIC 9(09)     COMP-3.
             04  SIMU-ENRT-FILLER                  PIC X(2335).         
                                                                        
         03  SIMU-ENVIO REDEFINES SIMU-FILLER.                          
             04  SIMU-EN-CGC-CPF                   PIC 9(17)     COMP-3.
             04  SIMU-EN-CD-SIMU                   PIC 9(09)     COMP-3.
             04  SIMU-EN-IND-FASE                  PIC X(01).           
             04  SIMU-EN-FILLER                    PIC X(2335).         
                                                                        
         03  SIMU-RETORNO-FASE1 REDEFINES SIMU-FILLER.                  
             04  SIMU-RT-F1-QTDE-OCORR             PIC 9(03)     COMP-3.
             04  SIMU-RT-F1-QTDE-MAX               PIC 9(03)     COMP-3.
             04  SIMU-RT-F1-FILLER     OCCURS      75 TIMES.            
                 05  SIMU-RT-F1-CGC-CPF            PIC 9(17)     COMP-3.
                 05  SIMU-RT-F1-CD-SIMU            PIC 9(09)     COMP-3.
                 05  SIMU-RT-F1-VLR-RENEG          PIC 9(15)V99  COMP-3.
                 05  SIMU-RT-F1-RESERVA            PIC X(07).           
             04  SIMU-RT-F1-FILLER2                PIC X(90).           
                                                                        
         03  SIMU-RETORNO-FASE2 REDEFINES SIMU-FILLER.                  
             04  SIMU-RT-F2-QTDE-OCORR            PIC 9(03)     COMP-3. 
             04  SIMU-RT-F2-QTDE-MAX              PIC 9(03)     COMP-3. 
             04  SIMU-RT-F2-FILLER     OCCURS     23 TIMES.             
                 05  SIMU-RT-F2-CGC-CPF           PIC 9(17)     COMP-3. 
                 05  SIMU-RT-F2-CD-SIMU           PIC 9(09)     COMP-3. 
                 05  SIMU-RT-F2-CD-SIMUA          PIC 9(03)     COMP-3. 
                 05  SIMU-RT-F2-CD-CANAL          PIC 9(05)     COMP-3. 
                 05  SIMU-RT-F2-CD-UNIDADE        PIC 9(05)     COMP-3. 
                 05  SIMU-RT-F2-IND-TP-SIMUA      PIC X(01).            
                 05  SIMU-RT-F2-IND-ALÇADA        PIC X(01).            
                 05  SIMU-RT-F2-IND-SERASA-SPC    PIC X(01).            
                 05  SIMU-RT-F2-VLR-TOT-VENC      PIC 9(15)V99  COMP-3. 
                 05  SIMU-RT-F2-PERC-DESC         PIC 9(03)V99  COMP-3. 
                 05  SIMU-RT-F2-VLR-RENEG         PIC 9(15)V99  COMP-3. 
                 05  SIMU-RT-F2-QTDE-PARC         PIC 9(05)     COMP-3. 
                 05  SIMU-RT-F2-FORMA-PGTO        PIC X(01).            
                 05  SIMU-RT-F2-VLR-ENTRADA       PIC 9(05)V99  COMP-3. 
                 05  SIMU-RT-F2-DT-PGTO-ENTRADA   PIC X(10).            
                 05  SIMU-RT-F2-TP-PGTO-ENTRADA   PIC X(01).            
                 05  SIMU-RT-F2-VLR-PCELA         PIC 9(05)V99  COMP-3. 
                 05  SIMU-RT-F2-TP-PGTO-PCELA     PIC X(01).            
                 05  SIMU-RT-F2-IND-RESTRICAO     PIC X(03).            
                 05  SIMU-RT-F2-RENEGOCIADOR      PIC X(08).            
                 05  SIMU-RT-F2-CANAL-RENEG       PIC 9(05)     COMP-3. 
                 05  SIMU-RT-F2-BCO               PIC 9(03)     COMP-3. 
                 05  SIMU-RT-F2-UNIDADE-RENEG     PIC 9(09)     COMP-3. 
                 05  SIMU-RT-F2-RESERVA           PIC X(09).            
             04  SIMU-RT-F2-FILLER2               PIC X(46).            
                                                                        
         03  SIMU-RETORNO-FASE3 REDEFINES SIMU-FILLER.                  
             04  SIMU-RT-F3-QTDE-OCORR            PIC 9(03)     COMP-3. 
             04  SIMU-RT-F3-QTDE-MAX              PIC 9(03)     COMP-3. 
             04  SIMU-RT-F3-FILLER     OCCURS     46 TIMES.             
                 05  SIMU-RT-F3-CD-SIMU           PIC 9(09)     COMP-3. 
                 05  SIMU-RT-F3-CD-CONTR          PIC 9(09)     COMP-3. 
                 05  SIMU-RT-F3-VLR-TOT-VCIDO     PIC 9(15)V99  COMP-3. 
                 05  SIMU-RT-F3-VLR-TOT-VCER      PIC 9(15)V99  COMP-3. 
                 05  SIMU-RT-F3-VLR-TOT-PG        PIC 9(15)V99  COMP-3. 
                 05  SIMU-RT-F3-QTDE-PARC-VCIDA   PIC 9(05)     COMP-3. 
                 05  SIMU-RT-F3-QTDE-TOT-PARC     PIC 9(05)     COMP-3. 
                 05  SIMU-RT-F3-RESERVA           PIC X(07).            
             04  SIMU-RT-F3-FILLER2               PIC X(46).            
                                                                        
         03  SIMU-RETORNO-FASE4 REDEFINES SIMU-FILLER.                  
             04  SIMU-RT-F4-QTDE-OCORR            PIC 9(03)     COMP-3. 
             04  SIMU-RT-F4-QTDE-MAX              PIC 9(03)     COMP-3. 
             04  SIMU-RT-F4-FILLER     OCCURS     50 TIMES.             
                 05  SIMU-RT-F4-CD-SIMU           PIC 9(09)     COMP-3. 
                 05  SIMU-RT-F4-NRO-PARC          PIC 9(03)     COMP-3. 
                 05  SIMU-RT-F4-DT-PGTO           PIC X(10).            
                 05  SIMU-RT-F4-RESERVA           PIC X(29).            
             04  SIMU-RT-F4-FILLER2               PIC X(46).            

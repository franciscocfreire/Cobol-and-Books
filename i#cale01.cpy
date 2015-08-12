      ******************************************************************
      * COPY......: I#CALE01                                           *
      *                                                                *
      * DESCRICAO.: AREA DE COMUNICACAO ENTRE O SISTEMA LEGADO E O     *
      *             COMPONENTE CALENDARIO (CALE), PARA PROCESSAMENTO   *
      *             OPERACOES DE CONVERSAO, CONSISTENCIA, CONSULTA,    *
      *             E CALCULOS COM DATAS E/ OU HORARIOS.               *
      *                                                                *
      * TAMANHO...: 1182 Bytes                                         *
      * GRUPO.....: DDS-GP-34                                          *
      * COMPONENTE: CALE - CALENDARIO                                  *
      ******************************************************************
      *            DESCRICAO  RESUMIDA  DA  ESTRUTURA  DE  DADOS       *
      ******************************************************************
      *                                                                *
      *CALE01-REGISTRO.                                                *
      * CALE01-IDENTIFICACAO.............IDENTIFICACAO DO REGISTRO     *
      *                                                                *
      * CALE01-ID-BLOCO..................IDENTIFICADOR DO COPY BOOK QUE*
      *                                  TRAFEGA AS INFORMACOES        *
      *                                                                *
      * CALE01-TAM-BLOCO.................TAMANHO DO BLOCO - CORRESPONDE*
      *                                  A AREA DO BOOK                *
      *                                                                *
      * CALE01-FUNCAO....................IDENTIFICA A FUNCAO           *
      *                                  (F1/F2/F3)                    *
      *                                                                *
      * CALE01-SUB-FUNCAO................IDENTIFICADOR DA SUBFUNCAO    *
      * VALORES PREVISTOS:  SF1001/ SF2001/ SF3001/ SF3002/ SF3003/    *
      * SF3004/ SF3005/ SF3006/ SF3007/ SF3008/ SF3009/ SF3010.        *
      *                                                                *
      * CALE01-RETORNO.                                                *
      * CALE01-COD-RETORNO...............CODIGO DE RETORNO DA API      *
      *                                                                *
      * CALE01-COD-ERRO..................CODIGO SECUNDARIO QUE COMPLE- *
      * MENTA O CONTEUDO INFORMADO NO CAMPO: CALE01-COD-RETORNO        *
      *                                                                *
      * CALE01-MENSAGEM-GMSG.                                          *
      * CALE01-COD-MENSAGEM-GMSG.........CODIGO DA MENSAGEM GMSG -     *
      * APRESENTA O RESULTADO DO PROCESSAMENTO DA OPERACAO             *
      *                                                                *
      * CALE01-COD-IDIOMA-MSG-GMSG.......CODIGO DO IDIOMA DA MENSAGEM  *
      *                                  GMSG                          *
      *                                                                *
      * CALE01-COD-RECURSO-MSG-GMSG......CODIGO DO RECURSO DA MENSAGEM *
      *                                  GMSG (FIXO 50)                *
      *                                                                *
      * CALE01-TIPO-RECURSO-MSG-GMSG.....IDENTIFDICADOR DO TIPO DE RE_ *
      *                                  CURSO INFORMADO (FIXO ZEROS)  *
      *                                                                *
      * CALE01-PARAMETRO-MENSAGE-GMSG....PARAMETRO DA MENSAGEM         *
      *                                                                *
      * CALE01-ENTRADA.                                                *
      * CALE01-ARGUMENTO-INICIAL.........CONJUNTO DE INFORMACOES QUE   *
      * CARACTERIZAM O ARGUMENTO DE ENTRADA INICIAL                    *
      *                                                                *
      * CALE01-FORMATO-ARGUMENTO-INI.....FORMATO DO ARGUMENTO INICIAL  *
      * ESTA RELACIONADO COM O VALOR DO ARGUMENTO INICIAL              *
      *                                                                *
      * CALE01-VLR-ARGUMENTO-INI.........VALOR DO ARGUMENTO INICIAL    *
      * VALORES ACEITOS: FORMATOS DIVERSOS DE DATA E HORA              *
      *                                                                *
      * CALE01-TP-INCL-ARGUMENTO-INI.....TIPO DE INCLUSAO DO ARGUMENTO *
      * INICIAL - VALORES ACEITOS: 'I' OU 'E', SENDO:                  *
      * 'I' -> INCLUSIVO E 'E' -> EXCLUSIVO                            *
      *                                                                *
      * CALE01-ARGUMENTO-FINAL...........CONJUNTO DE INFORMACOES QUE   *
      * CARACTERIZAM O ARGUMENTO DE ENTRADA FINAL                      *
      *                                                                *
      * CALE01-FORMATO-ARGUMENTO-FINAL...FORMATO DO ARGUMENTO FINAL    *
      * ESTA RELACIONADO COM O VALOR DO ARGUMENTO FINAL                *
      *                                                                *
      * CALE01-VLR-ARGUMENTO-FINAL.......VALOR DO ARGUMENTO FINAL      *
      * VALORES ACEITOS: FORMATOS DIVERSOS DE DATA E HORA              *
      *                                                                *
      * CALE01-TP-INCL-ARGUMENTO-FINAL...TIPO DE INCLUSAO DO ARGUMENTO *
      * INICIAL - VALORES ACEITOS: 'I' OU 'E', SENDO:                  *
      * 'I' -> INCLUSIVO E 'E' -> EXCLUSIVO                            *
      *                                                                *
      * CALE01-ARGUMENTO-DIA-SEMANA......CODIGO CORRESPONDENTE AO DIA  *
      * DA SEMANA - ONDE: 1-DOMINGO/ 2- SEGUNDA-FEIRA/ 3- TERCA-FEIRA  *
      * 4-QUARTA-FEIRA/ 5-QUINTA-FEIRA/ 6-SEXTA-FEIRA/ 7-SABADO        *
      *                                                                *
      * CALE01-FRMTO-RELAC-DT-SAIDA......CODIGO IDENTIFICADOR DO FORMA_*
      *                                TO DAS DATAS NA RELACAO DE SAIDA*
      *                                                                *
      * CALE01-CALCULO-DT................CONJUNTO DE INFORMACOES REFE_ *
      * RENTE ARGUMENTOS DE CALCULO DE DATAS OU HORARIOS               *
      *                                                                *
      * CALE01-TP-INCREMENTO-CALC........INDICADOR DO TIPO DE INCREMEN_*
      * TO DE CALCULO - VALORES PREVISTOS: 'DC'/ 'DU'/ 'MM'/ 'AA'      *
      *                                                                *
      * CALE01-VLR-INCREMENTO-CALC.......VALOR DO INCREMENTO DE CALCULO*
      *                                                                *
      * CALE01-SINAL-INCREMENTO-CALC.....SINAL DO INCREMENTO DE CALCULO*
      * INDICATIVO DA OPERACAO ARITMETICA A SER PROCESSADA, SENDO:     *
      *  'N' -> NEGATIVO E 'P' -> POSITIVO                             *
      *                                                                *
      * CALE01-OPCAO-TRAT-SABADO.........INDICA A OPCAO DE TRATAMENTO  *
      * DO SABADO - VALORES PREVISTOS:'DN':DIA NAO UTIL E 'DU':DIA UTIL*
      *                                                                *
      * CALE01-TRAT-ULT-DIA-ANO..........INDICA A OPCAO DE TRATAMENTO  *
      *                                  DO ULTIMO DIA DO ANO          *
      *                                                                *
      * CALE01-LOCAL-GEOGRAFICO..........CODIGO DO LOCAL GEOGRAFICO    *
      *                                                                *
      * CALE01-TPO-LOCALIDADE............INDICA O TIPO DE LOCAL GEOGRA-*
      * FICO - VALORES PREVISTOS: 'P':PAIS/'E':ESTADO/'M':MUNICIPIO    *
      *                                                                *
      * CALE01-COD-INSTC-RCONH-PBLIC.....CODIGO DA INSTITUICAO DE RECO-*
      * NHECIMENTO PUBLICO                                             *
      *                                                                *
      * CALE01-COD-LOCALIDADE............CODIGO DO LOCAL GEOGRAFICO    *
      * DO COMPONENTE AGEO - DEFAULT = ZEROS QUE CORRESPONDE AO BRASIL *
      *                                                                *
      * CALE01-IND-ABRANGENCIA-FER.......INDICA A ABRANGENCIA DE OCOR- *
      *                                  RENCIAS DE FERIADOS           *
      * VALORES ACEITOS: 'L'-FEIRADOS LOCAIS OU 'T' TODOS OS FERIADOS  *
      *                                                                *
      * CALE01-COD-IDIOMA................CODIGO DO IDIOMA DO COMPONENTE*
      * ICOR - DEFAULT = ZEROS QUE CORRESPONDE AO BRASIL (PORTUGUES)   *
      *                                                                *
      * CALE01-SAIDA.....................CONJUNTO DE INFORMACOES DE SAI*
      * DA DE ACORDO COM O PROCESSAMENTO                               *
      *                                                                *
      * CALE01-CONSIST-DT-HORA...........FORNECE RESULTADO DA CONSISTEN*
      * CIA, SENDO: 'C' - CONSISTENTE E 'I' - INCONSISTENTE            *
      *                                                                *
      * CALE01-INFORMACOES-DATA..........CONJUNTO DE INFORMACOES SOBRE *
      * DATAS FORNECIDAS PELA API-CALE                                 *
      *                                                                *
      * CALE01-IND-TIPO-DT...............INFORMA O TIPO DE DATA - VALO_*
      * RES ACEITOS:'DU':DIA UTIL/'FR':FERIADO OU 'FS':FINAL DE SEMANA *
      *                                                                *
      * CALE01-DT-UTIL-ANTER-AAAAMMDD....DATA UTIL ANTERIOR NO FORMATO *
      *                                  GREGORIANO INVERTIDO          *
      *                                                                *
      * CALE01-DT-UTIL-POST-AAAAMMDD.....DATA UTIL DO PROXIMO DIA UTIL *
      *                                  POSTERIOR GREGORIANO INVERTIDO*
      *                                                                *
      * CALE01-DT-ANTERIOR-AAAAMMDD......DATA ANTERIOR NO FORMATO GRE_ *
      * GORIANO INVERTIDO - DIA IMEDIATAMENTE ANTERIOR                 *
      *                                                                *
      * CALE01-DT-POSTERIOR-AAAAMMDD.....DATA POSTERIOR NO FORMATO GRE_*
      * GORIANO INVERTIDO - DIA IMEDIATAMENTE POSTERIOR                *
      *                                                                *
      * CALE01-NUM-ORDINAL-DD-UTIL-MM....NUMERO ORDINAL DO DIA UTIL NO *
      *                                  MES                           *
      *                                                                *
      * CALE01-NUM-ORDINAL-DD-UTIL-AA....NUMERO ORDINAL DO DIA UTIL NO *
      *                                  ANO                           *
      *                                                                *
      * CALE01-NUM-ORDINAL-DD-AA.........NUMERO ORDINAL DO DIA NO ANO  *
      *                                                                *
      * CALE01-DENOMINACAO-DIA-SMN-DT....DENOMINACAO DO DIA DA SEMANA  *
      *                                  SANTA                         *
      *                                                                *
      * CALE01-DD-SMN-EXT-PORT...........DENOMINACAO DO DIA DA SEMANA  *
      *                                  EM PORTUGUES                  *
      *                                                                *
      * CALE01-DD-SMN-EXT-IDI-SOLIC......DENOMINACAO DO DIA DA SEMANA  *
      *                                  NO IDIOMA SOLICITADO          *
      *                                                                *
      * CALE01-DENOMINACAO-MES-DATA......DENOMINACAO DO MES DA DATA    *
      *                                                                *
      * CALE01-MES-EXT-PORTUGUES.........DENOMINACAO DO DIA DA SEMANA  *
      *                                  EM PORTUGUES                  *
      *                                                                *
      * CALE01-MES-EXT-IDI-SOLIC.........DENOMINACAO DO DIA MES NO     *
      *                                  IDIOMA SOLICITADO             *
      *                                                                *
      * CALE01-INFORMACOES-DT-FER........INFORMACOES SOBRE FERIADO NA  *
      *                                  DATA                          *
      *                                                                *
      * CALE01-NM-FER-PORTUGUES..........DENOMINACAO DO DIA DA SEMANA  *
      *                                  EM PORTUGUES                  *
      *                                                                *
      * CALE01-NM-FER-IDIOMA-SOLIC.......DENOMINACAO DO MES NO IDIOMA  *
      *                                  SOLICITADO                    *
      *                                                                *
      * CALE01-FORMATACAO-DATA...........CONJUNTO DE INFORMACOES SOBRE *
      *                                  DATAS EM DIVERSOS FORMATOS    *
      *                                                                *
      * CALE01-DT-GREGO-F-FAAMMDD........DATA GREGORIANA INVERTIDA COM *
      *                                  FLAG NO ANO                   *
      *                                                                *
      * CALE01-DT-GREGO-AAAAMMDD.........DATA EDITADA NO FORMATO GREGO_*
      *                                  RIANO INVERTIDO               *
      *                                                                *
      * CALE01-DT-GREGO-DDMMAAAA.........DATA EDITADA NO FORMATO GREGO_*
      *                                  RIANO                         *
      *                                                                *
      * CALE01-DT-GREGO-MMDDAAAA.........DATA EDITADA NO FORMATO GREGO_*
      *                                  RIANO E AMERICANO             *
      *                                                                *
      * CALE01-DT-GREGO-B-DDMMAAAA.......DATA EDITADA NO FORMATO GREGO_*
      *                                  RIANO COM BARRAS              *
      *                                                                *
      * CALE01-DT-GREGO-P-DDMMAAAA.......DATA EDITADA NO FORMATO GREGO_*
      *                                  RIANO COM PONTOS              *
      *                                                                *
      * CALE01-DT-JULIANA-F-FAADDD.......DATA EDITADA NO FORMATO JULIA_*
      *                                  NO COM FLAG NO ANO            *
      *                                                                *
      * CALE01-DT-JULIANA-AAAADDD........DATA EDITADA NO FORMATO JULIA_*
      *                                  NO                            *
      *                                                                *
      * CALE01-DT-MMAA-F-FAAMM...........DATA EDITADA NO FORMATO MES   *
      *                                  ANO COM FLAG                  *
      *                                                                *
      * CALE01-DT-MMAA-AAAAMM............DATA EDITADA NO FORMATO MES   *
      *                                  ANO INVERTIDA                 *
      *                                                                *
      * CALE01-DT-MMAA-MMAAAA............DATA EDITADA NO FORMATO MES   *
      *                                  ANO                           *
      *                                                                *
      * CALE01-DT-MMAA-BARRA-MMAAAA......DATA EDITADA NO FORMATO MES   *
      *                                  ANO COM BARRA                 *
      *                                                                *
      * CALE01-DT-MMAA-SG-BRR-XXXAAAA....DATA EDITADA NO FORMATO SIGLA-*
      *                                  MES-ANO COM BARRA             *
      *                                                                *
      * CALE01-INFORMACOES-HORARIO.......CONJUNTO DE INFORMACOES SOBRE *
      *                                  HORARIO                       *
      *                                                                *
      * CALE01-HH-HHMM...................INFORMA HORARIO NO FORMATO    *
      *                                  'HHMM'                        *
      *                                                                *
      * CALE01-HH-2PONTOS-HHMM...........INFORMA HORARIO NO FORMATO    *
      *                                  'HH:MM:SS'                    *
      *                                                                *
      * CALE01-HH-HHMMSS.................INFORMA HORARIO NO FORMATO    *
      *                                  'HHMMSS'                      *
      *                                                                *
      * CALE01-HH-2PONTOS-HHMMSS.........INFORMA HORARIO NO FORMATO    *
      *                                  'HH:MM:SS'                    *
      *                                                                *
      * CALE01-HH-HHMMSSMMMMMM...........INFORMA HORARIO NO FORMATO    *
      *                                  'HHMMSSNNNNNN'                *
      *                                                                *
      * CALE01-HH-TIMESTAMP..............INFORMA HORARIO NO FORMATO    *
      * TIMESTAMP: 'AAAAMMDDHHMMSSNNNNNN'                              *
      *                                                                *
      * CALE01-HH-TIMESTAMP-DB2..........INFORMA HORARIO NO FORMATO    *
      * TIMESTAMP - EDITADO DB2: 'AAAA-MM-DD-HH.MM.SS.NNNNNN'          *
      *                                                                *
      * CALE01-RESULTADO-QTDE-DIAS.......CONJUNTO DE INFORMACOES SOBRE *
      * O RESULTADO DO PROCESSAMENTO - QUANTIDADE DE DIAS              *
      *                                                                *
      * CALE01-QTDE-DIAS-PERIODO.........INFORMA QUANTIDADE DE DIAS    *
      * UTEIS OU CORRIDOS PARA UM PERIODO DE DATAS                     *
      *                                                                *
      * CALE01-UNID-QTDE-DIAS-PERIODO....INDICA A UNIDADE DE MEDIDA REF*
      * AO CALCULO EFETUADO P/ O PERIODO DE DATAS - VALORES ACEITOS:   *
      * 'DC' -> DIAS CORRIDOS / 'DU' -> DIAS UTEIS                     *
      *                                                                *
      * CALE01-RESULTADO-QTDE-HORAS......CONJUNTO DE INFORMACOES REFE- *
      * RENTE AO PROCESSAMENTO DO CALCULO REALIZADO C/ HORARIOS        *
      *                                                                *
      * CALE01-QTDE-HORAS-PERIODO........INFORMA A QUANTIDADE DE HORAS *
      * E MINUTOS NO FORMATO 'HHHMM'                                   *
      *                                                                *
      * CALE01-RELAC-DT-MESMO-DD-SMN.....CONJUNTO DE INFORMACOES DA RE_*
      * LACAO DE DATAS QUE OCORREM NO MESMO DIA DA SEMANA              *
      *                                                                *
      * CALE01-FOR-DT-MESMO-DD-SMN.......CODIGO IDENTIFICADOR DO FORMA_*
      * DA DATA VARIANDO DO CODIGO DE: 001 A 009                       *
      *                                                                *
      * CALE01-QTDE-DT-MESMO-DIA-SMN.....INFORMA A QUANTIDADE REAL DE  *
      * DE DATAS QUE OCORREM NO MESMO DIA DA SEMANA                    *
      *                                                                *
      * CALE01-OCCORENCIA-DATA  OCCURS  5  TIMES.                       
      *                                                                *
      * CALE01-DT-MESMO-DD-SEMANA.............INFORMA A DATA QUE OCOR_ *
      * REM NO MESMO DIA DA SEMANA.                                     
      *                                                                *
      ******************************************************************
      * MANUTENCAO:                                                    *
      ******************************************************************
      * DATA       AUTOR        DESCRICAO                              *
      ******************************************************************
      * 05/06/2008 IGOR         AREA DE COMUNUCACAO DE PROGRAMAS       *
      *                         ONLINE E BATCH                         *
      ******************************************************************
       01  CALE01-REGISTRO.                                             
           03  CALE01-IDENTIFICACAO.                                    
               05  CALE01-ID-BLOCO                       PIC X(008).    
               05  CALE01-TAM-BLOCO                      PIC 9(005).    
               05  CALE01-FUNCAO                         PIC X(064).    
               05  CALE01-SUB-FUNCAO                     PIC X(064).    
           03  CALE01-RETORNO.                                          
               05  CALE01-COD-RETORNO                    PIC 9(002).    
               05  CALE01-COD-ERRO                       PIC X(004).    
               05  CALE01-MENSAGEM-GMSG.                                
                   07  CALE01-COD-MENSAGEM-GMSG          PIC X(008).    
                   07  CALE01-COD-IDIOMA-MSG-GMSG        PIC 9(005).    
                   07  CALE01-COD-RECURSO-MSG-GMSG       PIC 9(005).    
                   07  CALE01-TIPO-RECURSO-MSG-GMSG      PIC 9(001).    
                   07  CALE01-PARAMETRO-MENSAGE-GMSG     PIC X(500).    
           03  CALE01-ENTRADA.                                          
               05  CALE01-ARGUMENTO-INICIAL.                            
                   07  CALE01-FORMATO-ARGUMENTO-INI      PIC 9(003).    
                   07  CALE01-VLR-ARGUMENTO-INI          PIC X(010).    
                   07  CALE01-TP-INCL-ARGUMENTO-INI      PIC X(001).    
               05  CALE01-ARGUMENTO-FINAL.                              
                   07  CALE01-FORMATO-ARGUMENTO-FINAL    PIC 9(003).    
                   07  CALE01-VLR-ARGUMENTO-FINAL        PIC X(010).    
                   07  CALE01-TP-INCL-ARGUMENTO-FINAL    PIC X(001).    
               05  CALE01-ARGUMENTO-DIA-SEMANA           PIC 9(001).    
               05  CALE01-FORMT-RELAC-DT-SAIDA           PIC 9(003).    
               05  CALE01-CALCULO-DT.                                   
                   07  CALE01-TP-INCREMENTO-CALC         PIC X(002).    
                   07  CALE01-VLR-INCREMENTO-CALC        PIC 9(005).    
                   07  CALE01-SINAL-INCREMENTO-CALC      PIC X(001).    
               05  CALE01-OPCAO-TRAT-SABADO              PIC X(002).    
               05  CALE01-TRAT-ULT-DIA-ANO               PIC X(002).    
               05  CALE01-LOCAL-GEOGRAFICO.                             
                   07  CALE01-TPO-LOCALIDADE             PIC X(001).    
                   07  CALE01-COD-INSTC-RCONH-PBLIC      PIC 9(005).    
                   07  CALE01-COD-LOCALIDADE             PIC 9(010).    
               05  CALE01-IND-ABRANGENCIA-FER            PIC X(001).    
               05  CALE01-COD-IDIOMA                     PIC 9(003).    
           03  CALE01-SAIDA.                                            
               05  CALE01-CONSIST-DT-HORA                PIC X(001).    
               05  CALE01-INFORMACOES-DATA.                             
                   07  CALE01-IND-TIPO-DT                PIC X(002).    
                   07  CALE01-DT-UTIL-ANTER-AAAAMMDD     PIC 9(008).    
                   07  CALE01-DT-UTIL-POST-AAAAMMDD      PIC 9(008).    
                   07  CALE01-DT-ANTERIOR-AAAAMMDD       PIC 9(008).    
                   07  CALE01-DT-POSTERIOR-AAAAMMDD      PIC 9(008).    
                   07  CALE01-NUM-ORDINAL-DD-UTIL-MM     PIC 9(002).    
                   07  CALE01-NUM-ORDINAL-DD-UTIL-AA     PIC 9(003).    
                   07  CALE01-NUM-ORDINAL-DD-AA          PIC 9(003).    
               05  CALE01-DENOMINACAO-DIA-SMN-DT.                       
                   07  CALE01-DD-SMN-EXT-PORT            PIC X(015).    
                   07  CALE01-DD-SMN-EXT-IDI-SOLIC       PIC X(030).    
               05  CALE01-DENOMINACAO-MES-DATA.                         
                   07  CALE01-MES-EXT-PORTUGUES          PIC X(010).    
                   07  CALE01-MES-EXT-IDI-SOLIC          PIC X(030).    
               05  CALE01-INFORMACOES-DT-FER.                           
                   07  CALE01-NM-FER-PORTUGUES           PIC X(040).    
                   07  CALE01-NM-FER-IDIOMA-SOLIC        PIC X(040).    
               05  CALE01-FORMATACAO-DATA.                              
                   07  CALE01-DT-GREGO-F-FAAMMDD         PIC 9(007).    
                   07  CALE01-DT-GREGO-AAAAMMDD          PIC 9(008).    
                   07  CALE01-DT-GREGO-DDMMAAAA          PIC 9(008).    
                   07  CALE01-DT-GREGO-MMDDAAAA          PIC 9(008).    
                   07  CALE01-DT-GREGO-B-DDMMAAAA        PIC X(010).    
                   07  CALE01-DT-GREGO-P-DDMMAAAA        PIC X(010).    
                   07  CALE01-DT-JULIANA-F-FAADDD        PIC 9(006).    
                   07  CALE01-DT-JULIANA-AAAADDD         PIC 9(007).    
                   07  CALE01-DT-MMAA-F-FAAMM            PIC 9(005).    
                   07  CALE01-DT-MMAA-AAAAMM             PIC 9(006).    
                   07  CALE01-DT-MMAA-MMAAAA             PIC 9(006).    
                   07  CALE01-DT-MMAA-BARRA-MMAAAA       PIC X(007).    
                   07  CALE01-DT-MMAA-SG-BRR-XXXAAAA     PIC X(008).    
               05  CALE01-INFORMACOES-HORARIO.                          
                   07  CALE01-HH-HHMM                    PIC 9(004).    
                   07  CALE01-HH-2PONTOS-HHMM            PIC X(005).    
                   07  CALE01-HH-HHMMSS                  PIC 9(006).    
                   07  CALE01-HH-2PONTOS-HHMMSS          PIC X(008).    
                   07  CALE01-HH-HHMMSSMMMMMM            PIC 9(012).    
                   07  CALE01-HH-TIMESTAMP               PIC X(020).    
                   07  CALE01-HH-TIMESTAMP-DB2           PIC X(026).    
               05  CALE01-RESULTADO-QTDE-DIAS.                          
                   07  CALE01-QTDE-DIAS-PERIODO          PIC S9(005).   
                   07  CALE01-UNID-QTDE-DIAS-PERIODO     PIC X(002).    
               05  CALE01-RESULTADO-QTDE-HORAS.                         
                   07  CALE01-QTDE-HORAS-PERIODO         PIC 9(006).    
               05  CALE01-RELAC-DT-MESMO-DD-SMN.                        
                   07  CALE01-FOR-DT-MESMO-DD-SMN        PIC 9(003).    
                   07  CALE01-QTDE-DT-MESMO-DIA-SMN      PIC 9(001).    
                   07  CALE01-OCCORENCIA-DATA  OCCURS  5  TIMES.        
                       10  CALE01-DT-MESMO-DD-SEMANA     PIC X(010).    

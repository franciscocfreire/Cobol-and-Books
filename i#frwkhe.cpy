      ******************************************************            
      *                                                    *            
      * NOME BOOK : FRWKGHEA                               *            
      * DESCRICAO : ESTRUTURA COMUNICACAO PARA ERROS       *            
      * DATA      : 29/08/2006                             *            
      * AUTOR     : CARLA PARALS SENDIN                    *            
      * EMPRESA   : CPM/GFT                                *            
      * GRUPO     : TI MELHORIAS                           *            
      * COMPONENTE: FRAMEWORK ONLINE                       *            
      *                                                    *            
      ******************************************************            
      *                                                    *            
      * FRWKGHEA-HEADER.                                   *            
      *   FRWKGHEA-COD-LAYOUT        = CODIGO DE LAYOUT    *            
      *   FRWKGHEA-TAM-LAYOUT        = TAMANHO DO LAYOUT   *            
      * FRWKGHEA-REGISTRO.                                 *            
      *   FRWKGHEA-TIPO-ERRO         = TIPO DE ERRO        *            
      *     ERRO-MQ                  = ERRO MQ             *            
      *     ERRO-DB2                 = ERRO DB2            *            
      *     ERRO-CICS                = ERRO CICS           *            
      *     ERRO-MODULO              = ERRO NO MODULO      *            
      *     ERRO-ARQUIVO             = ERRO NO ARQUIVO     *            
      *     ERRO-LIVRE               = ERRO LIVRE          *            
      *   FRWKGHEA-NOME-PROGRAMA     = NOME DO PROBRAMA    *            
      *   FRWKGHEA-IDEN-PARAGRAFO    = IDENT PARAGRAFO     *            
      *   FRWKGHEA-TAM-DADOS         = TAMANHO DA INFORMA- *            
      *                                NAO DO ERRO         *            
      ******************************************************            
      * DATA       AUTOR        MODIFICACAO                *            
      * --------   ---------    -------------------------- *            
CRM-01* 27/03/2013 J721175      . NOVO TIPO DE ERRO        *            
CRM-01*                                                    *            
      ******************************************************            
       09 FRWKGHEA-HEADER.                                              
          11 FRWKGHEA-COD-LAYOUT PIC X(008) VALUE 'FRWKGHEA'.           
          11 FRWKGHEA-TAM-LAYOUT PIC 9(005) VALUE 58.                   
       09 FRWKGHEA-REGISTRO.                                            
          11 FRWKGHEA-TIPO-ERRO                 PIC X(02).              
             88 TIPO-ERRO-CORRETO-ONLINE        VALUE 'MQ' 'DB' 'CI'    
                                                      'MO' 'AR' 'LI'.   
CRM-01       88 TIPO-ERRO-CORRETO-BATCH         VALUE 'MQ' 'DB' 'RE'    
                                                      'MO' 'AR' 'LI'.   
             88 ERRO-MQ                         VALUE 'MQ'.             
             88 ERRO-DB2                        VALUE 'DB'.             
             88 ERRO-CICS                       VALUE 'CI'.             
             88 ERRO-MODULO                     VALUE 'MO'.             
             88 ERRO-ARQUIVO                    VALUE 'AR'.             
             88 ERRO-LIVRE                      VALUE 'LI'.             
CRM-01       88 ERRO-RECORRENTE                 VALUE 'RE'.             
          11 FRWKGHEA-NOME-PROGRAMA             PIC X(08).              
          11 FRWKGHEA-IDEN-PARAGRAFO            PIC X(30).              
          11 FRWKGHEA-TAM-DADOS                 PIC 9(05).              

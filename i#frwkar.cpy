      ******************************************************            
      *                                                    *            
      * NOME BOOK : FRWKGARQ                               *            
      * DESCRICAO : COPY DE ERRO DE ARQUIVO                *            
      * DATA      : 29/08/2006                             *            
      * AUTOR     : CARLA PARALS SENDIN                    *            
      * EMPRESA   : CPM/GFT                                *            
      * GRUPO     : TI MELHORIAS                           *            
      * COMPONENTE: FRAMEWORK ONLINE                       *            
      *                                                    *            
      ******************************************************            
      *                                                    *            
      * FRWKGARQ-HEADER.                                   *            
      *   FRWKGARQ-COD-LAYOUT        = CODIGO DE LAYOUT    *            
      *   FRWKGARQ-TAM-LAYOUT        = TAMANHO DO LAYOUT   *            
      * FRWKGARQ-REGISTRO.                                 *            
      *   FRWKGARQ-FILE-STATUS       = FILE STATUS         *            
      *   FRWKGARQ-NOME-ARQUIVO      = NOME DO ARQUIVO     *            
      *   FRWKGARQ-COMANDO           = NOME DO COMANDO     *            
      *     ARQ-WRITE                = WRITE               *            
      *     ARQ-REWRITE              = REWRITE             *            
      *     ARQ-OPEN                 = OPEN                *            
      *     ARQ-READ                 = READ                *            
      *     ARQ-CLOSE                = CLOSE               *            
      *     ARQ-START                = START               *            
      *     ARQ-DELETE               = DELETE              *            
      *                                                    *            
      ******************************************************            
      * DATA       AUTOR        MODIFICACAO                *            
      * --------   ---------    -------------------------- *            
      * DD/MM/AAAA JNNNNNN      XXXXXXXXXXXXXXXXXXXXXXXXXX *            
      ******************************************************            
       05 FRWKGARQ-HEADER.                                              
          07 FRWKGARQ-COD-LAYOUT PIC X(008) VALUE 'FRWKGARQ'.           
          07 FRWKGARQ-TAM-LAYOUT PIC 9(005) VALUE 29.                   
       05 FRWKGARQ-REGISTRO.                                            
          07 FRWKGARQ-FILE-STATUS               PIC X(02).              
          07 FRWKGARQ-NOME-ARQUIVO              PIC X(08).              
          07 FRWKGARQ-COMANDO                   PIC X(06).              
             88 ARQ-WRITE                       VALUE 'WRITE '.         
             88 ARQ-REWRITE                     VALUE 'RWRITE'.         
             88 ARQ-OPEN                        VALUE 'OPEN  '.         
             88 ARQ-READ                        VALUE 'READ  '.         
             88 ARQ-CLOSE                       VALUE 'CLOSE '.         
             88 ARQ-START                       VALUE 'START '.         
             88 ARQ-DELETE                      VALUE 'DELETE'.         

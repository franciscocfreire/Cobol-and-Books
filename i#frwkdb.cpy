      ******************************************************            
      *                                                    *            
      * NOME BOOK : FRWKGDB2                               *            
      * DESCRICAO : ESTRUTURA DE ERRO DB2                  *            
      * DATA      : 29/08/2006                             *            
      * AUTOR     : CARLA PARALS SENDIN                    *            
      * EMPRESA   : CPM/GFT                                *            
      * GRUPO     : TI MELHORIAS                           *            
      * COMPONENTE: FRAMEWORK ONLINE                       *            
      *                                                    *            
      ******************************************************            
      *                                                    *            
      * FRWKGDB2-HEADER.                                   *            
      *   FRWKGDB2-COD-LAYOUT   = CODIGO DE LAYOUT         *            
      *   FRWKGDB2-TAM-LAYOUT    = TAMANHO DO LAYOUT       *            
      * FRWKGDB2-REGISTRO.                                 *            
      *   FRWKGDB2-NOME-TABELA   = NOME DA TABELA          *            
      *   FRWKGDB2-COMANDO-DB2   = NOME DO COMANDO DB2     *            
      *   FRWKGDB2-STORED-PROC   = NOME DO STORED PROCEDURE*            
      *   FRWKGDB2-LOCAL         = NOME DO LOCAL           *            
      *   FRWKGDB2-SQLCODE       = SQLCODE                 *            
      *   FRWKGDB2-SQLSTATE      = SQLSTATE                *            
      *   FRWKGDB2-SQLCA         = SQLCA                   *            
      *                                                    *            
      ******************************************************            
      * DATA       AUTOR        MODIFICACAO                *            
      * --------   ---------    -------------------------- *            
      * DD/MM/AAAA JNNNNNN      XXXXXXXXXXXXXXXXXXXXXXXXXX *            
      ******************************************************            
       05 FRWKGDB2-HEADER.                                              
          07 FRWKGDB2-COD-LAYOUT PIC X(008) VALUE 'FRWKGDB2'.           
          07 FRWKGDB2-TAM-LAYOUT PIC 9(005) VALUE 222.                  
       05 FRWKGDB2-REGISTRO.                                            
          07 FRWKGDB2-NOME-TABELA    PIC  X(32).                        
          07 FRWKGDB2-COMANDO-DB2    PIC  X(09).                        
             88 DB2-SELECT           VALUE 'SELECT   '.                 
             88 DB2-UPDATE           VALUE 'UPDATE   '.                 
             88 DB2-INSERT           VALUE 'INSERT   '.                 
             88 DB2-DELETE           VALUE 'DELETE   '.                 
             88 DB2-OPEN             VALUE 'OPEN     '.                 
             88 DB2-FETCH            VALUE 'FETCH    '.                 
             88 DB2-CLOSE            VALUE 'CLOSE    '.                 
             88 DB2-ASSOCIATE        VALUE 'ASSOCIATE'.                 
             88 DB2-ALLOCATE         VALUE 'ALLOCATE '.                 
             88 DB2-CALL-SP          VALUE 'CALL-SP  '.                 
          07 FRWKGDB2-STORED-PROC    PIC  X(08).                        
          07 FRWKGDB2-LOCAL          PIC  X(16).                        
          07 FRWKGDB2-SQLCODE2       PIC S9(03).                        
          07 FRWKGDB2-SQLSTATE       PIC X(05).                         
          07 FRWKGDB2-SQLCA.                                            
             09 FRWKGDB2-SQLCAID     PIC X(8).                          
             09 FRWKGDB2-SQLCABC     PIC S9(9) COMP-4.                  
             09 FRWKGDB2-SQLCODE     PIC S9(9) COMP-4.                  
             09 FRWKGDB2-SQLERRM.                                       
                49 FRWKGDB2-SQLERRML PIC S9(4) COMP-4.                  
                49 FRWKGDB2-SQLERRMC PIC X(70).                         
             09 FRWKGDB2-SQLERRP     PIC X(8).                          
             09 FRWKGDB2-SQLERRD OCCURS 6 TIMES PIC S9(9) COMP-4.       
             09 FRWKGDB2-SQLWARN.                                       
                10 FRWKGDB2-SQLWARN0 PIC X.                             
                10 FRWKGDB2-SQLWARN1 PIC X.                             
                10 FRWKGDB2-SQLWARN2 PIC X.                             
                10 FRWKGDB2-SQLWARN3 PIC X.                             
                10 FRWKGDB2-SQLWARN4 PIC X.                             
                10 FRWKGDB2-SQLWARN5 PIC X.                             
                10 FRWKGDB2-SQLWARN6 PIC X.                             
                10 FRWKGDB2-SQLWARN7 PIC X.                             
             09 FRWKGDB2-SQLEXT.                                        
                10 FRWKGDB2-SQLWARN8 PIC X.                             
                10 FRWKGDB2-SQLWARN9 PIC X.                             
                10 FRWKGDB2-SQLWARNA PIC X.                             
                10 FRWKGDB2-SQLSTATE2 PIC X(5).                         

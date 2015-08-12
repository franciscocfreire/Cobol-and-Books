      *----------------------------------------------------------------*
      *                      Inicio da Geracao                         *
      *           010-CopyBook-Source.Cobol.Metalinguagem              *
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                       Inicio do Include                        *
      *     includes\includes-Genericos\010-CopyBook-Source.Cobol      *
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      * Copy Book - RECRW0CA                                            
      *----------------------------------------------------------------*
       05 CD-FORMULARIO                  PIC X(8)                       
           VALUE 'RECRW0CA'.                                            
       05 NM-CLIENTE                     PIC X(40).                     
       05 LOCAL-DATA                     PIC X(60).                     
       05 CONTA-BCRIA                    PIC X(50).                     
       05 VR-TOTAL-ACUMULADO             PIC 9(15)V9(2).                
       05 NOME-AGENCIA                   PIC X(20).                     
       05 ENDE-AGENCIA                   PIC X(40).                     
       05 CIDADE-AGENCIA                 PIC X(40).                     
       05 IMPRIME-EMPRESA                PIC 9(1).                      
       05 IMPRIME-PRESTAMISTA            PIC 9(1).                      
       05 QTDE-OCORRENCIAS-1             PIC 9(3).                      
       05 BLC-OCORRENCIAS                                               
           OCCURS 1 TO 9 TIMES                                          
           DEPENDING ON QTDE-OCORRENCIAS-1.                             
          10 DS-NATUREZA-ACAO-REST          PIC X(20).                  
          10 DT-VENCTO-PARC-ATRS            PIC X(10).                  
          10 VR-VENCIDO-CONTRATO            PIC 9(15)V9(2).             
      *----------------------------------------------------------------*
      *                       Final do Include                         *
      *     includes\includes-Genericos\010-CopyBook-Source.Cobol      *
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                       Final da Geracao                         *
      *           010-CopyBook-Source.Cobol.Metalinguagem              *
      *----------------------------------------------------------------*

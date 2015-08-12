      *----------------------------------------------------------------*
      *                      Inicio da Geracao                         *
      *           010-CopyBook-Source.Cobol.Metalinguagem              *
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                       Inicio do Include                        *
      *     includes\includes-Genericos\010-CopyBook-Source.Cobol      *
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      * Copy Book - RECRW0AA                                            
      *----------------------------------------------------------------*
       05 CD-FORMULARIO                  PIC X(8)                       
           VALUE 'RECRW0AA'.                                            
       05 NM-CLIENTE                     PIC X(40).                     
       05 LOCAL-DATA                     PIC X(60).                     
       05 VR-TOTAL-ACUMULADO             PIC 9(15)V9(2).                
       05 IMPRIME-EMPRESA                PIC 9(1).                      
       05 QTDE-OCORRENCIAS-2             PIC 9(3).                      
       05 BLC-OCORRENCIAS                                               
           OCCURS 1 TO 10 TIMES                                         
           DEPENDING ON QTDE-OCORRENCIAS-2.                             
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

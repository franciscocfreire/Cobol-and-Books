      *----------------------------------------------------------------*
      *                      Inicio da Geracao                         *
      *           010-CopyBook-Source.Cobol.Metalinguagem              *
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                       Inicio do Include                        *
      *     includes\includes-Genericos\010-CopyBook-Source.Cobol      *
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      * Copy Book - RECRW830                                            
      *----------------------------------------------------------------*
       05 RECRW830-CD-FORMULARIO         PIC X(8)                       
           VALUE 'RECRW830'.                                            
       05 RECRW830-NM-CLIENTE            PIC X(40).                     
       05 RECRW830-LOCAL-DATA            PIC X(60).                     
       05 RECRW830-CONTA-BCRIA           PIC X(50).                     
       05 RECRW830-NOME-AGENCIA          PIC X(20).                     
       05 RECRW830-ENDE-AGENCIA          PIC X(40).                     
       05 RECRW830-CIDADE-AGENCIA        PIC X(40).                     
       05 RECRW830-IMPRIME-EMPRESA       PIC 9(1).                      
       05 RECRW830-IMPRIME-PRESTAMISTA   PIC 9(1).                      
       05 RECRW830-VR-TOTAL-ACUMULADO    PIC 9(15).                     
       05 RECRW830-DT-BASE               PIC X(10).                     
       05 RECRW830-QTDE-OCORRENCIAS      PIC 9(3).                      
       05 RECRW830-BLC-OCORRENCIAS                                      
           OCCURS 13 TIMES                                              
           DEPENDING ON RECRW830-QTDE-OCORRENCIAS.                      
          10 RECRW830-CD-OCOR-CSF           PIC X(8)                    
              VALUE 'RC003L'.                                           
          10 RECRW830-DS-PRODUTO            PIC X(20).                  
          10 RECRW830-DT-VENCTO-PARC-ATRS   PIC X(10).                  
          10 RECRW830-NU-CONTRATO           PIC 9(10).                  
          10 RECRW830-VR-PRINCIPAL          PIC 9(11).                  
          10 RECRW830-VR-JUROS-REMUNERATOR  PIC 9(10).                  
          10 RECRW830-VR-JUROS-MORATORIOS   PIC 9(10).                  
          10 RECRW830-VR-IOF                PIC 9(10).                  
          10 RECRW830-VR-MULTA              PIC 9(10).                  
          10 RECRW830-VR-DESP-JUDICIAIS     PIC 9(10).                  
          10 RECRW830-VR-HONORARIOS         PIC 9(10).                  
          10 RECRW830-VR-TAXA-TARIFA        PIC 9(10).                  
          10 RECRW830-VR-VENCIDO-CONTRATO   PIC 9(12).                  
      *----------------------------------------------------------------*
      *                       Final do Include                         *
      *     includes\includes-Genericos\010-CopyBook-Source.Cobol      *
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                       Final da Geracao                         *
      *           010-CopyBook-Source.Cobol.Metalinguagem              *
      *----------------------------------------------------------------*

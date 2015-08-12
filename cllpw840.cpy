      *----------------------------------------------------------------*
      *                      Inicio da Geracao                         *
      *           010-CopyBook-Source.Cobol.Metalinguagem              *
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                       Inicio do Include                        *
      *     includes\includes-Genericos\010-CopyBook-Source.Cobol      *
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      * Copy Book - RECRW840                                            
      *----------------------------------------------------------------*
       05 RECRW840-CD-FORMULARIO         PIC X(8)                       
           VALUE 'RECRW840'.                                            
       05 RECRW840-NM-CLIENTE            PIC X(40).                     
       05 RECRW840-LOCAL-DATA            PIC X(60).                     
       05 RECRW840-VR-TOTAL-ACUMULADO    PIC 9(15).                     
       05 RECRW840-IMPRIME-EMPRESA       PIC 9(1).                      
       05 RECRW840-IMPRIME-PRESTAMISTA   PIC 9(1).                      
       05 RECRW840-DT-BASE               PIC X(10).                     
       05 RECRW840-QTDE-OCORRENCIAS      PIC 9(3).                      
       05 RECRW840-BLC-OCORRENCIAS                                      
           OCCURS 15 TIMES                                              
           DEPENDING ON RECRW840-QTDE-OCORRENCIAS.                      
          10 RECRW840-CD-OCOR-CSF           PIC X(8)                    
              VALUE 'RC004L'.                                           
          10 RECRW840-DS-PRODUTO            PIC X(20).                  
          10 RECRW840-DT-VENCTO-PARC-ATRS   PIC X(10).                  
          10 RECRW840-NU-CONTRATO           PIC 9(10).                  
          10 RECRW840-VR-PRINCIPAL          PIC 9(11).                  
          10 RECRW840-VR-JUROS-REMUNERATOR  PIC 9(10).                  
          10 RECRW840-VR-JUROS-MORATORIOS   PIC 9(10).                  
          10 RECRW840-VR-IOF                PIC 9(10).                  
          10 RECRW840-VR-MULTA              PIC 9(10).                  
          10 RECRW840-VR-DESP-JUDICIAIS     PIC 9(10).                  
          10 RECRW840-VR-HONORARIOS         PIC 9(10).                  
          10 RECRW840-VR-TAXA-TARIFA        PIC 9(10).                  
          10 RECRW840-VR-VENCIDO-CONTRATO   PIC 9(12).                  
      *----------------------------------------------------------------*
      *                       Final do Include                         *
      *     includes\includes-Genericos\010-CopyBook-Source.Cobol      *
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                       Final da Geracao                         *
      *           010-CopyBook-Source.Cobol.Metalinguagem              *
      *----------------------------------------------------------------*

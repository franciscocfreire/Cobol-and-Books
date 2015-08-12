      *----------------------------------------------------------------*
      *                      Inicio da Geracao                         *
      *           010-CopyBook-Source.Cobol.Metalinguagem              *
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                       Inicio do Include                        *
      *     includes\includes-Genericos\010-CopyBook-Source.Cobol      *
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      * Copy Book - RECRW810                                            
      *----------------------------------------------------------------*
       05 RECRW810-CD-FORMULARIO         PIC X(8)                       
           VALUE 'RECRW810'.                                            
       05 RECRW810-NM-CLIENTE            PIC X(40).                     
       05 RECRW810-LOCAL-DATA            PIC X(60).                     
       05 RECRW810-VR-TOTAL-ACUMULADO    PIC 9(15).                     
       05 RECRW810-IMPRIME-EMPRESA       PIC 9(1).                      
       05 RECRW810-DT-BASE               PIC X(10).                     
       05 RECRW810-QTDE-OCORRENCIAS      PIC 9(3).                      
       05 RECRW810-BLC-OCORRENCIAS                                      
           OCCURS 16 TIMES                                              
           DEPENDING ON RECRW810-QTDE-OCORRENCIAS.                      
          10 RECRW810-CD-OCOR-CSF           PIC X(8)                    
              VALUE 'RC001L'.                                           
          10 RECRW810-DS-PRODUTO            PIC X(20).                  
          10 RECRW810-DT-VENCTO-PARC-ATRS   PIC X(10).                  
          10 RECRW810-NU-CONTRATO           PIC 9(10).                  
          10 RECRW810-VR-PRINCIPAL          PIC 9(15).                  
          10 RECRW810-VR-JUROS-REMUNERATOR  PIC 9(15).                  
          10 RECRW810-VR-JUROS-MORATORIOS   PIC 9(15).                  
          10 RECRW810-VR-IOF                PIC 9(15).                  
          10 RECRW810-VR-MULTA              PIC 9(15).                  
          10 RECRW810-VR-DESP-JUDICIAIS     PIC 9(15).                  
          10 RECRW810-VR-HONORARIOS         PIC 9(15).                  
          10 RECRW810-VR-TAXA-TARIFA        PIC 9(15).                  
          10 RECRW810-VR-VENCIDO-CONTRATO   PIC 9(15).                  
      *----------------------------------------------------------------*
      *                       Final do Include                         *
      *     includes\includes-Genericos\010-CopyBook-Source.Cobol      *
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                       Final da Geracao                         *
      *           010-CopyBook-Source.Cobol.Metalinguagem              *
      *----------------------------------------------------------------*

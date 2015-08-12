      *----------------------------------------------------------------*
      *                      Inicio da Geracao                         *
      *           010-CopyBook-Source.Cobol.Metalinguagem              *
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                       Inicio do Include                        *
      *     includes\includes-Genericos\010-CopyBook-Source.Cobol      *
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      * Copy Book - RECRW820                                            
      *----------------------------------------------------------------*
       05 RECRW820-CD-FORMULARIO         PIC X(8)                       
           VALUE 'RECRW820'.                                            
       05 RECRW820-NM-CLIENTE            PIC X(40).                     
       05 RECRW820-LOCAL-DATA            PIC X(60).                     
       05 RECRW820-CONTA-BCRIA           PIC X(50).                     
       05 RECRW820-NOME-AGENCIA          PIC X(20).                     
       05 RECRW820-ENDE-AGENCIA          PIC X(40).                     
       05 RECRW820-CIDADE-AGENCIA        PIC X(40).                     
       05 RECRW820-IMPRIME-EMPRESA       PIC 9(1).                      
       05 RECRW820-VR-TOTAL-ACUMULADO    PIC 9(15).                     
       05 RECRW820-DT-BASE               PIC X(10).                     
       05 RECRW820-QTDE-OCORRENCIAS      PIC 9(3).                      
       05 RECRW820-BLC-OCORRENCIAS                                      
           OCCURS 14 TIMES                                              
           DEPENDING ON RECRW820-QTDE-OCORRENCIAS.                      
          10 RECRW820-CD-OCOR-CSF           PIC X(8)                    
              VALUE 'RC002L'.                                           
          10 RECRW820-DS-PRODUTO            PIC X(20).                  
          10 RECRW820-DT-VENCTO-PARC-ATRS   PIC X(10).                  
          10 RECRW820-NU-CONTRATO           PIC 9(10).                  
          10 RECRW820-VR-PRINCIPAL          PIC 9(11).                  
          10 RECRW820-VR-JUROS-REMUNERATOR  PIC 9(10).                  
          10 RECRW820-VR-JUROS-MORATORIOS   PIC 9(10).                  
          10 RECRW820-VR-IOF                PIC 9(10).                  
          10 RECRW820-VR-MULTA              PIC 9(10).                  
          10 RECRW820-VR-DESP-JUDICIAIS     PIC 9(10).                  
          10 RECRW820-VR-HONORARIOS         PIC 9(10).                  
          10 RECRW820-VR-TAXA-TARIFA        PIC 9(10).                  
          10 RECRW820-VR-VENCIDO-CONTRATO   PIC 9(12).                  
      *----------------------------------------------------------------*
      *                       Final do Include                         *
      *     includes\includes-Genericos\010-CopyBook-Source.Cobol      *
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                       Final da Geracao                         *
      *           010-CopyBook-Source.Cobol.Metalinguagem              *
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *   ****  TABELA P/CONSISTENCIA DE UNIDADES DA FEDERACAO  ****   *
      *----------------------------------------------------------------*
       01  LPCL-TAB-UF.                                                 
           02  FILLER              PIC X(02)       VALUE 'AC'.          
           02  FILLER              PIC X(02)       VALUE 'AL'.          
           02  FILLER              PIC X(02)       VALUE 'AM'.          
           02  FILLER              PIC X(02)       VALUE 'AP'.          
           02  FILLER              PIC X(02)       VALUE 'BA'.          
           02  FILLER              PIC X(02)       VALUE 'CE'.          
           02  FILLER              PIC X(02)       VALUE 'DF'.          
           02  FILLER              PIC X(02)       VALUE 'ES'.          
           02  FILLER              PIC X(02)       VALUE 'GO'.          
           02  FILLER              PIC X(02)       VALUE 'MA'.          
           02  FILLER              PIC X(02)       VALUE 'MG'.          
           02  FILLER              PIC X(02)       VALUE 'MS'.          
           02  FILLER              PIC X(02)       VALUE 'MT'.          
           02  FILLER              PIC X(02)       VALUE 'PA'.          
           02  FILLER              PIC X(02)       VALUE 'PB'.          
           02  FILLER              PIC X(02)       VALUE 'PE'.          
           02  FILLER              PIC X(02)       VALUE 'PI'.          
           02  FILLER              PIC X(02)       VALUE 'PR'.          
           02  FILLER              PIC X(02)       VALUE 'RJ'.          
           02  FILLER              PIC X(02)       VALUE 'RN'.          
           02  FILLER              PIC X(02)       VALUE 'RO'.          
           02  FILLER              PIC X(02)       VALUE 'RR'.          
           02  FILLER              PIC X(02)       VALUE 'RS'.          
           02  FILLER              PIC X(02)       VALUE 'SC'.          
           02  FILLER              PIC X(02)       VALUE 'SE'.          
           02  FILLER              PIC X(02)       VALUE 'SP'.          
           02  FILLER              PIC X(02)       VALUE 'TO'.          
       01  LPCL-TAB-UF-R           REDEFINES       LPCL-TAB-UF.         
           02  LPCL-OCC-UF         OCCURS 27   INDEXED BY LPCL-INDX     
                                   PIC X(02).                           

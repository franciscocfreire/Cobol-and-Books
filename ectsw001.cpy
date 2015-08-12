      **--------------------------------------------------------------**
      **  SISTEMA       - ECTS                DATA        : 10/2011   **
      **  CODIGO INC    - ECTSW001            TAMANHO     : 172       **
      **  CONTEUDO      - AREA DE TRABALHO PARA ACESSO AO MODULO      **
      **                  ECTS9010                                    **
      **  USADO PGM'S   - TODOS OS PROGRAMAS DE CORRESPONDENCIA FAC   **
      **--------------------------------------------------------------**
       01  ECTSW001-AREA-FAC.                                           
           03  ECTSW001-VERSAO-FAC           PIC X(06)  VALUE 'VRS001'. 
           03  ECTSW001-MENSAGEM-FAC         PIC X(83)  VALUE SPACES.   
           03  ECTSW001-CEP-FAC              PIC 9(08)  VALUE ZEROS.    
           03  FILLER  REDEFINES  ECTSW001-CEP-FAC.                     
               05  ECTSW001-NUMCEP-FAC       PIC 9(05).                 
               05  ECTSW001-CPLCEP-FAC       PIC 9(03).                 
           03  ECTSW001-JOBNAME              PIC X(08)  VALUE SPACES.   
           03  ECTSW001-RETORNO-FAC          PIC X(59)  VALUE SPACES.   
           03  FILLER  REDEFINES  ECTSW001-RETORNO-FAC.                 
               05  ECTSW001-DR-FAC           PIC 9(02).                 
               05  ECTSW001-CD-ADM-FAC       PIC 9(08).                 
               05  ECTSW001-DEST-FAC         PIC 9(01).                 
               05  ECTSW001-NUM-CART-CLI-FAC PIC 9(12).                 
               05  ECTSW001-NUM-LOTE-FAC     PIC 9(05).                 
               05  ECTSW001-COD-UNI-POS-FAC  PIC 9(08).                 
               05  ECTSW001-CEP-UNI-ORI-FAC  PIC 9(08).                 
               05  ECTSW001-NUM-CONTR-FAC    PIC 9(10).                 
               05  ECTSW001-COD-CATE-FAC     PIC 9(05).                 
      **--------------------------------------------------------------**
      **                     F I M   D A   I N C                      **
      **--------------------------------------------------------------**

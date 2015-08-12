      *----------------------------------------------------------------*
      *    INC 'I#CEPN31'       - AREA DE COMUNICACAO DO MODULO        *
      *                           CEPN5060                             *
      *                                                                *
      *    TAMANHO              -  280 BYTES                           *
      *                                                                *
      *    ANALISTA RESPONSAVEL - KIYOSHI TAKAESU (CAPGEMINI)          *
      *    ATUALIZACAO          - 16/07/2013                           *
      *                                                                *
      *    DESCRICAO            - AREA DE COMUNICACAO COM O MODULO     *
      *                           CEPN5060                             *
      *----------------------------------------------------------------*
      *                                                                 
       01  CEPN31-AREA-CEPN5060.                                        
           05   CEPN31-DADOS-ENTRADA.                                   
                07  CEPN31-NUM-CEP         PIC  9(005).                 
                07  CEPN31-COMPL-CEP       PIC  9(003).                 
           05       CEPN31-CTPO-LOGDR      PIC  X(005).                 
           05       CEPN31-ENDER           PIC  X(060).                 
           05       CEPN31-NUMERO          PIC  9(005).                 
           05       CEPN31-BAIRRO          PIC  X(020).                 
           05       CEPN31-CIDADE          PIC  X(030).                 
           05       CEPN31-UF              PIC  X(002).                 
           05  CEPN31-DADOS-SAIDA.                                      
               07  CEPN31-COD-RETORNO      PIC  9(003).                 
               07  CEPN31-COD-ERRO         PIC  9(003).                 
               07  CEPN31-SAIDA.                                        
                   09  CEPN31-NUMS-CEP     PIC  9(005).                 
                   09  CEPN31-COMPLS-CEP   PIC  9(003).                 
                   09  CEPN31-ENDERS       PIC  X(060).                 
                   09  CEPN31-BAIRROS      PIC  X(020).                 
                   09  CEPN31-CIDADES      PIC  X(030).                 
                   09  CEPN31-UFS          PIC  X(002).                 
                   09  CEPN31-COD-ENTRS    PIC  X(001).                 
                   09  CEPN31-COD-MUNICS   PIC  9(005).                 
                   09  CEPN31-CDIG-MUNICS  PIC  X(001).                 
                   09  CEPN31-AGE-DEPS     PIC  9(007).                 
                   09  CEPN31-CTPO-LOGDRS  PIC  X(005).                 
                   09  CEPN31-COD-DISTRS   PIC  9(005).                 

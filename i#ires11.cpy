      ***************************************************************** 
      *   INC  =   I#IRES11   -  PESSOA (IRESV001)                    * 
      *                                                               * 
      *   DATA DA ULTIMA ATUALIZACAO -  10/11/1999                    * 
      *   AUTOR    - ROSANGELA VITORETTI                              * 
      *   TAMANHO  - 0169 BYTES                                       * 
      *                                                               * 
      *   OBJETIVO - IMAGEM DA TABELA IRESV001 - PESSOA               * 
      *                                                               * 
      *---------------------------------------------------------------* 
      *            POSICIONAMENTO DOS CAMPOS NO LAY-OUT               * 
      *---------------------------------------------------------------* 
      *                                                               * 
      *                                                               * 
      *       V001-CPSSOA                   001-026                   * 
      *       V001-CTPO-PSSOA               027-027                   * 
      *       V001-CCGC-CPF                 028-032                   * 
      *       V001-CFLIAL-CGC               033-035                   * 
      *       V001-CCTRL-CPF-CGC            036-037                   * 
      *       V001-COUTRO-DOCTO             038-050                   * 
      *       V001-CSGL-UF-DOCTO-ID         051-052                   * 
      *       V001-DCTRL-OPER               053-062                   * 
      *       V001-CFUNC-BDSCO              063-067                   * 
      *       V001-QIMPED-REST-PSSOA        068-071                   * 
      *       V001-QIMPED-REST-PSSOA-NULL   072-072                   * 
      *       V001-QLIG-PSSOA               073-076                   * 
      *       V001-QLIG-PSSOA-NULL          077-077                   * 
      *       V001-IPSSOA-RSUMO             078-107                   * 
      *       V001-IPSSOA-LEN               108-109                   * 
      *       V001-IPSSOA-TEXT              110-169                   * 
      *---------------------------------------------------------------* 
       01  IRESV001.                                                    
           03 V001-CPSSOA               PIC X(26).                      
           03 V001-CTPO-PSSOA             PIC S9(01)   COMP-3.          
           03 V001-CCGC-CPF               PIC S9(09)   COMP-3.          
           03 V001-CFLIAL-CGC             PIC S9(05)   COMP-3.          
           03 V001-CCTRL-CPF-CGC          PIC S9(02)   COMP-3.          
           03 V001-COUTRO-DOCTO           PIC  X(13).                   
           03 V001-CSGL-UF-DOCTO-ID       PIC  X(02).                   
           03 V001-DCTRL-OPER             PIC  X(10).                   
           03 V001-CFUNC-BDSCO            PIC S9(09)   COMP-3.          
           03 V001-QIMPED-REST-PSSOA      PIC S9(07)   COMP-3.          
           03 V001-QIMPED-REST-PSSOA-NULL PIC  X(01).                   
           03 V001-QLIG-PSSOA             PIC S9(07)   COMP-3.          
           03 V001-QLIG-PSSOA-NULL        PIC  X(01).                   
           03 V001-IPSSOA-RSUMO           PIC  X(30).                   
           03 V001-IPSSOA.                                              
              05 V001-IPSSOA-LEN          PIC S9(04)   COMP.            
              05 V001-IPSSOA-TEXT         PIC  X(60).                   

      ******************************************************************
      * NOME BOOK : INECWPHI                                           *
      * DESCRICAO : DETALHE FONTE DE CONSULTA DETALHE INDICADOR ECONO- *
      *             MICO FONTE (BACEN).                                *
      * DATA      : 06/MAR/2013                                        *
      * EMPRESA   : BSI TECNOLOGIA                                     *
      * COMPONENTE: INEC - INDICADORES ECONOMICOS                      *
      *             CONVIVENCIA                                        *
      * TAMANHO   : FIXO - 124 POS                                     *
      ******************************************************************
      * INECWPHI-COD-LAYOUT         = CODIGO DESTE LAYOUT              *
      * INECWPHI-TAM-LAYOUT         = TAMANHO DO REGISTRO              *
      *                                                                *
      * INECWPHI-COD-RETORNO        = CODIGO DE RETORNO                *
      * INECWPHI-COR-ERRO           = CODIGO DE ERRO                   *
      * INECWPHI-COD-MENSAGEM       = CODIGO DE MENSAGEM               *
      *                                                                *
      * ENTRADA                                                        *
      * INECWPHI-CINDCD-ECONM       = CODIGO DO INDICADOR ECONOMICO    *
      *                                                                *
      * SAIDA                                                          *
      * INECWPHI-CINDCD-FONTE-CONS  = CODIGO INDICADOR ECONM BACEN     *
      * INECWPHI-IINDCD-ECONM-FONTE = DESCRICAO INDICADOR ECONOMICO    *
      * INECWPHI-CSIT-INDCD-ECONM   = CODIGO SITUACAO INDICADOR ECONOMI*
      * INECWPHI-ISIT-INDCD-ECONM   = DESCRICAO SITUACAO INDICADOR ECON*
      ******************************************************************
           05 INECWPHI-HEADER.                                          
              07 INECWPHI-COD-LAYOUT      PIC X(008) VALUE 'INECWPHI'.  
              07 INECWPHI-TAM-LAYOUT      PIC 9(005) VALUE      00124.  
           05 INECWPHI-BLOCO-RETORNO.                                   
              07 INECWPHI-COD-RETORNO           PIC  9(002).            
              07 INECWPHI-COD-ERRO              PIC  X(004).            
              07 INECWPHI-COD-MENSAGEM          PIC  X(008).            
           05 INECWPHI-REGISTRO.                                        
              07 INECWPHI-BLOCO-ENTRADA.                                
                 09 INECWPHI-CINDCD-ECONM       PIC  9(005).            
              07 INECWPHI-BLOCO-SAIDA.                                  
                 09 INECWPHI-CINDCD-FONTE-CONS  PIC  X(020).            
                 09 INECWPHI-IINDCD-ECONM-FONTE PIC  X(040).            
                 09 INECWPHI-CSIT-INDCD-ECONM   PIC  9(002).            
                 09 INECWPHI-ISIT-INDCD-ECONM   PIC  X(030).            

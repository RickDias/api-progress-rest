/*----------------------------------------------------------------------------------------------/
 Programa..: esint001e.i
 Objetivo..: Include com Defini‡Æo das Tabelas Tempor rias
 Data......: 16/05/2019
 Autor.....: Rog‚rio Dias
 VersÆo....: 1.000.000
-----------------------------------------------------------------------------------------------*/


DEFINE TEMP-TABLE tt_credito_cliente NO-UNDO
      FIELD cod-emitente    LIKE emitente.cod-emitente SERIALIZE-NAME "CodigoCliente"
      FIELD val-sdo-credito AS DECIMAL                  SERIALIZE-NAME "SaldoCredito"
      FIELD val-divida      AS DECIMAL                  SERIALIZE-NAME "ValorDivida"
      FIELD dt-div-mlonga   AS DATE                     SERIALIZE-NAME "DataDividaMaisLonga".
                
 
     
    

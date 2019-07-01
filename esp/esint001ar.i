/*----------------------------------------------------------------------------------------------/
 Programa..: esint001ar.i
 Objetivo..: Include com Defini‡Æo das Tabelas Tempor rias
 Data......: 29/04/2019
 Autor.....: Rog‚rio Dias
 VersÆo....: 1.000.000
-----------------------------------------------------------------------------------------------*/



DEFINE TEMP-TABLE tt_emitente_ret NO-UNDO
    field CodigoCliente         	    as INTEGER                   column-label "C¢digo Cliente Datasul"     format ">,>>>,>>9"
    FIELD IdSalesforce                  AS CHARACTER                 COLUMN-LABEL "ID Sales Force"
    field l-Status    	                as LOGICAL                   column-label "Status"            
    FIELD c-Description                 AS CHARACTER                 COLUMN-LABEL "Descri‡Æo"
    .
      

DEFINE TEMP-TABLE tt_endereco_list_Ret NO-UNDO                                                                   
    FIELD IdSalesforce                  AS CHARACTER                 COLUMN-LABEL "ID Sales Force"
    field l-Status    	                as LOGICAL                   column-label "Status"            
    FIELD c-Description                 AS CHARACTER                 COLUMN-LABEL "Descri‡Æo" .


DEFINE TEMP-TABLE tt_condicao_pagto_ret NO-UNDO
    FIELD IdSalesforce                  AS CHARACTER                 COLUMN-LABEL "ID Sales Force"
    field l-Status    	                as LOGICAL                   column-label "Status"            
    FIELD c-Description                 AS CHARACTER                 COLUMN-LABEL "Descri‡Æo".


DEFINE TEMP-TABLE tt_contato_ret NO-UNDO
    FIELD IdSalesforce                  AS CHARACTER                 COLUMN-LABEL "ID Sales Force"
    field l-Status    	                as LOGICAL                   column-label "Status"            
    FIELD c-Description                 AS CHARACTER                 COLUMN-LABEL "Descri‡Æo".

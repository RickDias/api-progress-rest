/*----------------------------------------------------------------------------------------------/
 Programa..: esint001c.i
 Objetivo..: Include com Defini‡Æo das Tabelas Tempor rias
 Data......: 03/05/2019
 Autor.....: Rog‚rio Dias
 VersÆo....: 1.000.000
-----------------------------------------------------------------------------------------------*/


DEFINE TEMP-TABLE tt_item NO-UNDO
    field it-codigo    LIKE Item.it-codigo     SERIALIZE-NAME "CodigoItem"                   column-label "C¢digo do Produto"	            format "X(16)" 
    field desc-item    LIKE Item.desc-item     SERIALIZE-NAME "Descricao"                    column-label "Descri‡Æo do Produto"	        format "X(60)" 
    field fm-codigo    like Item.fm-codigo     SERIALIZE-NAME "CodigoFamiliaMaterial"        column-label "C¢digo da Fam¡lia de material"	format "X(8)"  
    field fm-cod-com   like Item.fm-cod-com    SERIALIZE-NAME "FamiliaComercial"	         COLUMN-LABEL "C¢digo da Fam¡lia de Comercial" 	format "X(8)"  
    FIELD un           like Item.un            SERIALIZE-NAME "UnidadeMedida"                column-label "Unidade de medida do item" 	    format "X(2)"  
    field inform-compl LIKE Item.inform-compl  SERIALIZE-NAME "InformacoesComplementares"    column-label "Informa‡äes complementares"	    format "X(16)" 
    field situacao     AS CHARACTER            SERIALIZE-NAME "Situacao"                     column-label "C¢digo de obsolecˆncia do item"
    FIELD peso-liquido LIKE item.peso-liquido  SERIALIZE-NAME "PesoLiquido" 
    FIELD peso-bruto   LIKE item.peso-bruto    SERIALIZE-NAME "PesoBruto"
    FIELD lote-mulven  LIKE item.lote-mulven   SERIALIZE-NAME "LoteMultiplo"
    FIELD cod-ean      LIKE ITEM-mat.cod-ean   SERIALIZE-NAME "EAN".

                         





 

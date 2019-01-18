/*------------------------------------------------------------------------
    File        : paymentTerms.p
    Purpose     : API REST para consulta de condi‡Æo de pagamento
    Syntax      :
    Description : Condi‡Æo de Pagamento
    Author(s)   : Cleberson Silva - TOTVS Private - CDE
    Created     : 06/12/2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{utp/ut-api.i}
{utp/ut-api-utils.i}

{include/i-prgvrs.i paymentTerms 2.00.00.000} /*** 010000 ***/

{utp/ut-api-action.i pi-get GET /~*/~*}
{utp/ut-api-action.i pi-getAll GET /~*}
{utp/ut-api-notfound.i}

DEFINE TEMP-TABLE tt-cond-pagto NO-UNDO SERIALIZE-NAME "CondicaoPagamentoList"
    FIELD cod-cond-pag  LIKE cond-pagto.cod-cond-pag SERIALIZE-NAME "CodigoCondicao"
    FIELD descricao     LIKE cond-pagto.descricao SERIALIZE-NAME "DescricaoCondicao"
    INDEX condPagto IS PRIMARY UNIQUE cod-cond-pag.
    
/* ********************  Preprocessor Definitions  ******************** */

/* ***************************  Main Block  *************************** */

/* **********************  Internal Procedures  *********************** */
/*------------------------------------------------------------------------------
 Purpose: Retorna a lista de condi‡Æo de pagamentos.
 Notes:
------------------------------------------------------------------------------*/
PROCEDURE pi-getAll:
    DEFINE INPUT  PARAMETER jsonInput  AS JsonObject NO-UNDO.
    DEFINE OUTPUT PARAMETER jsonOutput AS JsonObject NO-UNDO.
    
    DEFINE VARIABLE jsonCondPagto AS JsonArray NO-UNDO.
    
    EMPTY TEMP-TABLE tt-cond-pagto.
    EMPTY TEMP-TABLE RowErrors.
    
    FOR EACH cond-pagto NO-LOCK BY cod-cond-pag:
        CREATE tt-cond-pagto.
        ASSIGN tt-cond-pagto.cod-cond-pag = cond-pagto.cod-cond-pag
               tt-cond-pagto.descricao    = cond-pagto.descricao.
        
    END.
    
    ASSIGN jsonCondPagto = NEW JsonArray().
           jsonCondPagto:Read(TEMP-TABLE tt-cond-pagto:HANDLE).
    
    RUN createJsonResponse(INPUT jsonCondPagto, INPUT TABLE RowErrors, INPUT FALSE, OUTPUT jsonOutput).

    
END PROCEDURE.

/*------------------------------------------------------------------------------
 Purpose: Retorna as informa‡äes de condi‡Æo de pagamento informado na 
          requisi‡Æo
 Notes:
------------------------------------------------------------------------------*/
PROCEDURE pi-get:
    DEFINE INPUT  PARAMETER jsonInput  AS JsonObject NO-UNDO.
    DEFINE OUTPUT PARAMETER jsonOutput AS JsonObject NO-UNDO.
    
    DEFINE VARIABLE codCondPag AS INTEGER NO-UNDO.
    
    EMPTY TEMP-TABLE tt-cond-pagto.
    EMPTY TEMP-TABLE RowErrors.
    
    ASSIGN codCondPag = jsonInput:GetJsonArray("pathParams"):GetInteger(1).
    
    FIND FIRST cond-pagto NO-LOCK WHERE cond-pagto.cod-cond-pag = codCondPag NO-ERROR.
    
    IF  AVAILABLE cond-pagto THEN
        RUN pi-load-cond-pagto-json IN THIS-PROCEDURE(OUTPUT jsonOutput).        
    ELSE
        ASSIGN jsonOutput = NEW JsonObject().
    
    RUN createJsonResponse(INPUT jsonOutput, INPUT TABLE RowErrors, INPUT FALSE, OUTPUT jsonOutput).
END PROCEDURE.

/*------------------------------------------------------------------------------
 Purpose: Cria um JSON a partir do condi‡Æo de pagamento posicionado.
 Notes:
------------------------------------------------------------------------------*/
PROCEDURE pi-load-cond-pagto-json PRIVATE:
    DEFINE OUTPUT PARAMETER jsonCondPagto AS JsonObject NO-UNDO.
    
    ASSIGN jsonCondPagto = NEW JsonObject().
    
    jsonCondPagto:Add("CodigoCondicao", cond-pagto.cod-cond-pag).
    jsonCondPagto:Add("DescricaoCondicao", cond-pagto.descricao).
    
END PROCEDURE.

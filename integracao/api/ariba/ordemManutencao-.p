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

{utp/ut-api-action.i pi-get GET /~*}
{utp/ut-api-action.i pi-getAll GET /~*}
{utp/ut-api-notfound.i}

DEFINE TEMP-TABLE tt-ord-manut NO-UNDO SERIALIZE-NAME "OrdemManutencaoRetorno"
    FIELD nr-ord-produ  LIKE ord-manut.nr-ord-produ SERIALIZE-NAME "nrOrdProdu"
    INDEX ordem IS PRIMARY UNIQUE nr-ord-produ.

DEFINE TEMP-TABLE tt-ord-manut-retorno NO-UNDO SERIALIZE-NAME "OrdemManutencaoRetorno"
    FIELD nr-ord-produ  LIKE ord-manut.nr-ord-produ SERIALIZE-NAME "nrOrdProdu"
    FIELD validado      AS LOG SERIALIZE-NAME "validado"
    INDEX ordem IS PRIMARY UNIQUE nr-ord-produ.


DEF TEMP-TABLE tt-erros
    FIELD cod-erro  AS INTEGER
    FIELD desc-erro AS CHARACTER FORMAT "x(256)"
    FIELD desc-arq  AS CHARACTER.



    
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
    
    DEFINE VARIABLE jsonOrdemManut AS JsonArray NO-UNDO.
    
    EMPTY TEMP-TABLE tt-ord-manut.
    EMPTY TEMP-TABLE RowErrors.
    
    FOR EACH ord-manut NO-LOCK BY nr-ord-produ:
        CREATE tt-ord-manut-retorno.
        ASSIGN tt-ord-manut-retorno.nr-ord-produ = ord-manut.nr-ord-produ
               tt-ord-manut-retorno.validado     = YES.
        
    END.
    
    ASSIGN jsonOrdemManut = NEW JsonArray().
           jsonOrdemManut:Read(TEMP-TABLE tt-ord-manut:HANDLE).
    
    RUN createJsonResponse(INPUT jsonOrdemManut, INPUT TABLE RowErrors, INPUT FALSE, OUTPUT jsonOutput).

    
END PROCEDURE.

/*------------------------------------------------------------------------------
 Purpose: Retorna as informa‡äes de condi‡Æo de pagamento informado na 
          requisi‡Æo
 Notes:
------------------------------------------------------------------------------*/
PROCEDURE pi-get:
    DEFINE INPUT  PARAMETER jsonInput  AS JsonObject NO-UNDO.
    DEFINE OUTPUT PARAMETER jsonOutput AS JsonObject NO-UNDO.

    DEF VAR lRetOK AS LOG NO-UNDO.
    DEF VAR lc-json AS LONGCHAR NO-UNDO.
    
    DEFINE VARIABLE numeroOrdem    AS INTEGER NO-UNDO.
    DEFINE VARIABLE queryParams    AS JsonObject  NO-UNDO.
    
    EMPTY TEMP-TABLE tt-ord-manut.
    EMPTY TEMP-TABLE RowErrors.

    MESSAGE ">>>>>ANTES DE EXECUTAR API DE LOGIN".

    RUN btb/btapi910ze.p   (INPUT "tcpasilva", /*USUARIO*/
                            INPUT "",          /*SENHA*/
                            INPUT "1",         /*EMPRESA*/
                            OUTPUT TABLE tt-erros). /*RETORNO DE ERROSl*/

    //LOG-MANAGER:WRITE-MESSAGE(">>>>>DEPOIS DE EXECUTAR API DE LOGIN") NO-ERROR.

    MESSAGE ">>>>>DEPOIS DE EXECUTAR API DE LOGIN".


    DEFINE VARIABLE oRequestParser AS JsonAPIRequestParser NO-UNDO.

    DEF VAR cURI                   AS CHAR NO-UNDO.
    DEF VAR cMethod                AS CHAR NO-UNDO.
    //DEF VAR oHeaders               AS OBJECT NO-UNDO. 
    //DEF VAR aPathParams            AS CHAR NO-UNDO.
    //DEF VAR oQueryParams           AS OBJECT NO-UNDO.
    DEF VAR iStartRow              AS INT NO-UNDO.
    DEF VAR iPageSize              AS INT NO-UNDO.
    DEF VAR iPage                  AS INT NO-UNDO.
    DEF VAR cFields                AS CHAR NO-UNDO.
    DEF VAR cExpandables           AS CHAR NO-UNDO.
    //DEF VAR oOrder                 AS OBJECT NO-UNDO.
    DEF VAR lcPayload              AS LONGCHAR NO-UNDO.   


 
    oRequestParser = NEW JsonAPIRequestParser(jsonInput).
     
    ASSIGN cURI = oRequestParser:getURI()
           cMethod = oRequestParser:getMethod()
           //oHeaders = oRequestParser:getHeaders()            
           //aPathParams = oRequestParser:getPathParams()
           //oQueryParams = oRequestParser:getQueryParams()
           iStartRow = oRequestParser:getStartRow()
           iPageSize = oRequestParser:getPageSize()
           iPage = oRequestParser:getPage()
           cFields = oRequestParser:getFieldsChar()
           cExpandables = oRequestParser:getExpandChar()
           //oOrder = oRequestParser:getOrder()
           lcPayload = oRequestParser:getPayloadLongChar().





    lRetOK = TEMP-TABLE tt-ord-manut:WRITE-JSON("JsonObject", jsonInput, TRUE, "UTF-8") NO-ERROR.

    MESSAGE "lRetOK " lRetOK.

    OUTPUT TO /totvs/erp/camil/teste-qa-dev/especificos/teste.json.

    FOR EACH tt-ord-manut:
        EXPORT DELIMITER ";"
         tt-ord-manut.nr-ord-produ.
    END.

    OUTPUT CLOSE.

    ASSIGN queryParams = jsonInput:GetJsonObject("queryParams").
    ASSIGN numeroOrdem = INT(queryParams:GetJsonArray("nrOrdProdu"):getCharacter(1)).




    IF NOT CAN-FIND(FIRST tt-erros where tt-erros.cod-erro <> 3147) THEN
    DO:
        
        //LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE(">>>>>FAZENDO FIND NA ORDEM DE MANUTENCAO &1",numeroOrdem)) NO-ERROR.

        MESSAGE SUBSTITUTE(">>>>>FAZENDO FIND NA ORDEM DE MANUTENCAO &1",numeroOrdem).
        
        FIND FIRST ord-manut NO-LOCK WHERE ord-manut.nr-ord-produ = numeroOrdem NO-ERROR.
    
        RUN pi-load-ord-manut-json IN THIS-PROCEDURE(OUTPUT jsonOutput).  
    END.
    DO:
        FOR EACH tt-erros NO-LOCK:
            CREATE RowErrors.
            ASSIGN RowErrors.ErrorNumber      = tt-erros.cod-erro 
                   RowErrors.ErrorDescription = tt-erros.desc-erro. 
        END.
    END.
    
    RUN createJsonResponse(INPUT jsonOutput, INPUT TABLE RowErrors, INPUT FALSE, OUTPUT jsonOutput).
END PROCEDURE.

/*------------------------------------------------------------------------------
 Purpose: Cria um JSON a partir do condi‡Æo de pagamento posicionado.
 Notes:
------------------------------------------------------------------------------*/
PROCEDURE pi-load-ord-manut-json PRIVATE:
    DEFINE OUTPUT PARAMETER jsonOrdemManut AS JsonObject NO-UNDO.
    
    DEFINE VARIABLE numeroOrdem AS INTEGER     NO-UNDO.

    ASSIGN jsonOrdemManut = NEW JsonObject().

    IF AVAIL ord-manut THEN 
    DO:
        LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE(">>>>>A ORDEM  &1 FOI ENCONTRADA NO PRODUTO",numeroOrdem)) NO-ERROR.

        MESSAGE SUBSTITUTE(">>>>>FAZENDO FIND NA ORDEM DE MANUTENCAO &1",numeroOrdem).
        jsonOrdemManut:Add("nrOrdProdu", ord-manut.nr-ord-produ).         
        jsonOrdemManut:Add("validado", TRUE).              
    END.
    ELSE
    DO:
        MESSAGE SUBSTITUTE(">>>>>FAZENDO FIND NA ORDEM DE MANUTENCAO &1",numeroOrdem).
        jsonOrdemManut:Add("nrOrdProdu", numeroOrdem).     
        jsonOrdemManut:Add("validado", FALSE).                         
    END.
    
    
    
END PROCEDURE.

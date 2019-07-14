{utp/ut-api.i}
{utp/ut-api-utils.i}
{include/i-prgvrs.i idiomas 2.00.00.000} /*** 010000 ***/
{utp/ut-api-action.i pi-create POST /~*}
{utp/ut-api-action.i pi-getAll GET /~*}
{utp/ut-api-notfound.i}

DEFINE TEMP-TABLE ttContrato NO-UNDO
    LIKE contrato-for.

DEFINE TEMP-TABLE ttItemContrato NO-UNDO
    LIKE item-contrat.

DEFINE TEMP-TABLE ttReceiptContrato NO-UNDO
    field nr-contrato LIKE contrato-for.nr-contrato serialize-name "nrContrato".

DEFINE TEMP-TABLE ttMatrizItem NO-UNDO
    LIKE matriz-rat-item.

PROCEDURE pi-create:
    DEFINE INPUT  PARAMETER jsonInput  AS JsonObject NO-UNDO.
    DEFINE OUTPUT PARAMETER jsonOutput AS JsonObject NO-UNDO.

    RUN pi-upsert IN THIS-PROCEDURE(INPUT jsonInput:GetJsonObject("payload"), INPUT FALSE, OUTPUT jsonOutput).
END PROCEDURE.
PROCEDURE pi-upsert:
    DEFINE INPUT  PARAMETER jsonContrato AS JsonObject NO-UNDO.
    DEFINE INPUT  PARAMETER isUpdate     AS LOGICAL    NO-UNDO.
    DEFINE OUTPUT PARAMETER jsonOutput   AS JsonObject NO-UNDO.

    DEFINE VARIABLE inrContrato AS INTEGER NO-UNDO.
    DEFINE VARIABLE cDados AS CHARACTER NO-UNDO.

    jsonContrato:writeFile("c:\temp\testejson.json").

    /*
    ASSIGN cDados = jsonContrato:GetCharacter("nrContrato").

    oJsonObjectMain:GetCharacter("RazaoSocial")


    CREATE RowErrors.
    ASSIGN
        RowErrors.ErrorNumber      = 17006
        RowErrors.ErrorType        = "error"
        RowErrors.ErrorDescription = "Contrato Recebido com Sucesso" + string(cDados).

    RUN createJsonResponse(NEW JsonObject(), INPUT TABLE RowErrors, INPUT FALSE, OUTPUT jsonOutput).
    */

END PROCEDURE.
PROCEDURE pi-getAll:
    DEFINE INPUT  PARAMETER jsonInput  AS JsonObject NO-UNDO.
    DEFINE OUTPUT PARAMETER jsonOutput AS JsonObject NO-UNDO.


    DEFINE VARIABLE jsonContrato        AS JsonArray NO-UNDO.
    DEFINE VARIABLE jsonMatrizContrato  AS JsonArray NO-UNDO.
    DEFINE VARIABLE jsonItContrato      AS JsonArray NO-UNDO.

    DEFINE VARIABLE oJsonContrato  AS JsonObject NO-UNDO.
    DEFINE VARIABLE oJsonItContrato  AS JsonObject NO-UNDO.
    DEFINE VARIABLE oJsonMarizContrato  AS JsonObject NO-UNDO.

    EMPTY TEMP-TABLE ttContrato.
    EMPTY TEMP-TABLE ttItemContrato.

    ASSIGN jsonContrato = NEW JsonArray().
    //ASSIGN oJsonContrato = NEW jsonObject().

    FOR FIRST contrato-for WHERE contrato-for.nr-contrato = 13 NO-LOCK:
        CREATE ttContrato.
        BUFFER-COPY contrato-for TO ttContrato.

        jsonContrato:READ(TEMP-TABLE ttContrato:HANDLE).
        oJsonContrato:ADD("Contrato", jsonContrato).

        ASSIGN jsonItContrato = NEW jsonArray().
        FOR EACH item-contrat OF contrato-for NO-LOCK:
            CREATE ttItemContrato.
            BUFFER-COPY item-contrat TO ttItemContrato.                
            jsonItContrato:READ(TEMP-TABLE ttItemContrato:HANDLE).
            jsonItContrato:ADD(jsonItContrato).
        END.
        oJsonContrato:ADD("ItensContrato",jsonItContrato).

        jsonMatrizContrato = NEW JsonArray().
        FOR EACH matriz-rat-item OF item-contrat NO-LOCK:
            CREATE ttMatrizItem.
            BUFFER-COPY matriz-rat-item TO ttMatrizItem.

            jsonMatrizContrato:READ(TEMP-TABLE ttMatrizItem:HANDLE).   
            jsonMatrizContrato:ADD(jsonMatrizContrato).                      


        END.
        oJsonContrato:ADD("Matriz",jsonMatrizContrato).

        jsonContrato:ADD(oJsonContrato).



        
    END.
    

    RUN createJsonResponse(INPUT jsonContrato, INPUT TABLE RowErrors, INPUT FALSE, OUTPUT jsonOutput).
END PROCEDURE.




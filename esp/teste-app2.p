USING com.totvs.framework.abl.json.*.

DEFINE VARIABLE oJsonObject AS CLASS JsonObject NO-UNDO.
DEFINE VARIABLE oJsonArray  AS CLASS JsonArray  NO-UNDO.


oJsonArray = NEW JsonArray().


oJsonObject = NEW JsonObject().

oJsonObject:putValue("CodigoPropostaContrato","1234412").
oJsonObject:putValue("CodigoInstituicao","216574888991").
oJsonArray:putValue(oJsonObject).


oJsonArray:saveToFile("c:\temp\teste_salvo.json").

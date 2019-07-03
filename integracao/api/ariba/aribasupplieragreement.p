/*----------------------------------------------------------------------------------------------/
 Programa..: aribasupplieragreement.p
 Objetivo..: API Json REST para integra‡Æo de Contrato de Fornecedor
 Data......: 29/04/2019
 Autor.....: Rog‚rio Dias
 VersÆo....: 1.000.000
-----------------------------------------------------------------------------------------------*/

{utp/ut-api.i}
{utp/ut-api-utils.i}

{include/i-prgvrs.i aribasupplieragreement 2.00.00.001 } /*** "010005" ***/

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i aribasupplieragreement MCD}
&ENDIF

{utp/ut-api-action.i pi-create POST /~* }

/* ------- Defini‡Æo Temp-tables ------ */   
DEF TEMP-TABLE tt-erros
    FIELD cod-erro  AS INTEGER
    FIELD desc-erro AS CHARACTER FORMAT "x(256)"
    FIELD desc-arq  AS CHARACTER.

DEF TEMP-TABLE ttRetorno 
    FIELD codigocliente   AS CHARACTER
    FIELD situacao        AS LOGICAL 
    FIELD descricao   AS CHAR FORMAT "x(200)".

/* ------- Defini‡Æo Vari veis ------ */
DEF VAR i-seq-erro AS INTEGER NO-UNDO.

EMPTY TEMP-TABLE tt-Erros.
EMPTY TEMP-TABLE ttRetorno.



PROCEDURE pi-create:
    DEF INPUT  PARAM jsonInput  AS JsonObject NO-UNDO.
    DEF OUTPUT PARAM jsonOutput AS JsonObject NO-UNDO.

    DEFINE VARIABLE jsonRetorno        AS JsonArray            NO-UNDO.     
    DEFINE VARIABLE json_recebido      AS LONGCHAR             NO-UNDO.     
    DEFINE VARIABLE oRequestParser     AS JsonAPIRequestParser NO-UNDO.     
    DEFINE VARIABLE CodigoCliente      AS CHARACTER INITIAL ?  NO-UNDO.     
    DEFINE VARIABLE c-erro             AS CHARACTER            NO-UNDO.     
    DEFINE VARIABLE iCountMain         AS INTEGER              NO-UNDO.     
    DEFINE VARIABLE oJsonObjectMain    AS JsonObject           NO-UNDO.     
    DEFINE VARIABLE oJsonArrayMain     AS JsonArray            NO-UNDO.   
    DEFINE VARIABLE c_numero_contrato AS CHARACTER             NO-UNDO.


    jsonInput:writeFile("c:\temp\aribasupplieragreement.json").

    fix-codepage(json_recebido) = "UTF-8".

    ASSIGN  oRequestParser = NEW JsonAPIRequestParser(jsonInput)
            json_recebido = oRequestParser:getPayloadLongChar() .

    /* ---- Lˆ propriedade Principal ---- */        
    oJsonArrayMain = jsonInput:GetJsonObject("payload":U):GetJsonArray("req":U).


    
    CREATE  sfa-import-contr.
    ASSIGN  sfa-import-contr.cd-tipo-integr  = 1 /*--- import ---*/
            sfa-import-contr.Id-movto        = NEXT-VALUE(seq-import)
            sfa-import-contr.data-movto      = TODAY
            sfa-import-contr.c-json          = json_recebido.
            
    CREATE  sfa-import.
    ASSIGN  sfa-import.ind-tipo-trans   = 1 /*--- import ---*/
            sfa-import.cd-tipo-integr   = sfa-import-contr.cd-tipo-integr
            sfa-import.id-movto         = sfa-import-contr.Id-movto
            sfa-import.chave            = c_numero_contrato
            sfa-import.data-movto       = NOW
            sfa-import.data-inicio      = NOW
            sfa-import.data-fim         = ?
            sfa-import.ind-situacao     = 1 /*--- Pendente ---*/
            sfa-import.cod-status       = 0 /*--- sem status ---*/  .








    


END PROCEDURE.


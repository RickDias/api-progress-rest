/*------------------------------------------------------------------------
    File        : levelApprovalMLA.p
    Purpose     : API REST para exportar n¡vei de aprova‡Æo
    Syntax      :
    Description : Niveis de Hierarquia de Aprova‡Æo
    Author(s)   : Cleberson Silva
    Created     : 02/07/2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{utp/ut-api.i}
{utp/ut-api-utils.i}

{include/i-prgvrs.i levelApprovalMLA 2.00.00.000} /*** 010000 ***/

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i levelApprovalMLA MCC}
&ENDIF


{utp/ut-api-action.i pi-getAll GET /~*}
{utp/ut-api-action.i pi-get    GET /~*}
{utp/ut-api-notfound.i}

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



PROCEDURE pi-getAll:
    DEFINE INPUT  PARAM jsonInput AS JsonObject NO-UNDO.
    DEFINE OUTPUT PARAM jsonOutput AS JsonObject NO-UNDO.

    DEFINE VARIABLE jsonRetorno     AS JsonArray            NO-UNDO.
    DEFINE VARIABLE json_recebido   AS LONGCHAR             NO-UNDO.
    DEFINE VARIABLE oRequestParser  AS JsonAPIRequestParser NO-UNDO.
    DEFINE VARIABLE CodigoCliente   AS CHARACTER INITIAL ?  NO-UNDO.
    DEFINE VARIABLE c-erro          AS CHARACTER            NO-UNDO.
    DEFINE VARIABLE iCountMain      AS INTEGER              NO-UNDO.
    DEFINE VARIABLE oJsonObjectMain AS JsonObject           NO-UNDO.
    DEFINE VARIABLE oJsonArrayMain  AS JsonArray            NO-UNDO.


    







END PROCEDURE.


PROCEDURE pi-get:
END PROCEDURE. 


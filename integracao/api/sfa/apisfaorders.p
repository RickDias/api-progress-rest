/*----------------------------------------------------------------------------------------------/
 *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
 Programa..: apisfaorders.p
 Objetivo..: API Json REST para integraá∆o de Pedidos
 Data......: 29/04/2019
 Autor.....: RogÇrio Dias
 Vers∆o....: 1.000.000
-----------------------------------------------------------------------------------------------*/

{utp/ut-api.i}
{utp/ut-api-utils.i}

{include/i-prgvrs.i apisfaorders 2.00.00.005 } /*** "010005" ***/

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i apisfaorders MCD}
&ENDIF

{utp/ut-api-action.i pi-create POST /~* }


/* ------- Definiá∆o Temp-tables ------ */   
DEF TEMP-TABLE tt-erros
    FIELD cod-erro  AS INTEGER
    FIELD desc-erro AS CHARACTER FORMAT "x(256)"
    FIELD desc-arq  AS CHARACTER.

DEF TEMP-TABLE ttRetorno 
    FIELD CodigoPedido    AS CHARACTER
    FIELD situacao        AS LOGICAL 
    FIELD descricao   AS CHAR FORMAT "x(200)".

/* ------- Definiá∆o Vari†veis ------ */
DEF VAR i-seq-erro AS INTEGER NO-UNDO.

EMPTY TEMP-TABLE tt-Erros.
EMPTY TEMP-TABLE ttRetorno.



PROCEDURE pi-create:

    DEF INPUT  PARAM jsonInput  AS JsonObject NO-UNDO.
    DEF OUTPUT PARAM jsonOutput AS JsonObject NO-UNDO.
    
    DEFINE VARIABLE jsonRetorno      AS JsonArray            NO-UNDO.
    DEFINE VARIABLE json_recebido    AS LONGCHAR             NO-UNDO.
    DEFINE VARIABLE oRequestParser   AS JsonAPIRequestParser NO-UNDO.
    DEFINE VARIABLE CodigoCliente    AS CHARACTER INITIAL ?  NO-UNDO.
    DEFINE VARIABLE c-erro           AS CHARACTER            NO-UNDO.
    DEFINE VARIABLE CodigoSalesforce AS CHARACTER            NO-UNDO.
    DEFINE VARIABLE oJsonArrayMain   AS JsonArray            NO-UNDO.
    DEFINE VARIABLE oJsonObjectMain  AS JsonObject           NO-UNDO.
    DEFINE VARIABLE iCountMain       AS INTEGER              NO-UNDO.

    ASSIGN  oRequestParser = NEW JsonAPIRequestParser(jsonInput)
            json_recebido = oRequestParser:getPayloadLongChar().

    oJsonArrayMain = jsonInput:GetJsonObject("payload":U):GetJsonArray("req":U).

    DO iCountMain = 1 TO oJsonArrayMain:LENGTH:
        oJsonObjectMain =  oJsonArrayMain:GetJsonObject(iCountMain).

        IF oJsonObjectMain:Has("CodigoSalesforce")                   then do:
            CodigoSalesforce = oJsonObjectMain:GetCharacter("CodigoSalesforce")  NO-ERROR             .       
            LEAVE.
        END.
    END.

    CREATE  sfa-import-ped.
    ASSIGN  sfa-import-ped.cd-tipo-integr  = 3 /*--- import ---*/
            sfa-import-ped.Id-movto        = NEXT-VALUE(seq-import)
            sfa-import-ped.data-movto      = TODAY
            sfa-import-ped.c-json          = json_recebido
            sfa-import-ped.nr-ped-sfa      = CodigoSalesforce.
            
    CREATE  sfa-import.
    ASSIGN  sfa-import.ind-tipo-trans   = 1 /*--- import ---*/
            sfa-import.cd-tipo-integr   = sfa-import-ped.cd-tipo-integr
            sfa-import.id-movto         = sfa-import-ped.Id-movto
            sfa-import.chave            = ""
            sfa-import.data-movto       = NOW
            sfa-import.data-inicio      = NOW
            sfa-import.data-fim         = ?
            sfa-import.ind-situacao     = 1 /*--- Pendente ---*/
            sfa-import.cod-status       = 0 /*--- sem status ---*/  .

    FIND FIRST es-api-param 
         WHERE es-api-param.ind-tipo-trans = 1
           AND es-api-param.cd-tipo-integr = sfa-import-ped.cd-tipo-integr NO-LOCK 
         NO-ERROR.
    IF NOT AVAIL es-api-param 
    THEN RETURN. /*-----tratar erro */
   

    /* ------ Executa progama espec°fico para o tipo de integraá∆o ------ */
    
    MESSAGE "***** Time 0: " + STRING(TIME,"HH:MM:SS").

    RUN VALUE( es-api-param.programa-integr ) (INPUT ROWID(sfa-import),
                                               OUTPUT c-erro,
                                               INPUT jsonInput) NO-ERROR.   

    MESSAGE "***** Time 8: " + STRING(TIME,"HH:MM:SS").
    
    MESSAGE "*****" c-erro.

    IF ERROR-STATUS:ERROR THEN 
        c-erro = c-erro + SEARCH(es-api-param.programa-integr) + " - " + ERROR-STATUS:GET-MESSAGE(1) + 'propath: ' + PROPATH.

    ASSIGN sfa-import.data-fim     = NOW
           sfa-import.ind-situacao = 2 .
    
    MESSAGE "***** Time 9: " + STRING(TIME,"HH:MM:SS").
    /* ------ Gerencia retorno do processo -----*/
    IF c-erro = "" THEN ASSIGN sfa-import.cod-status = 1.
    ELSE ASSIGN sfa-import.cod-status = 2.
    
    RUN pi-gera-status (c-erro).

    MESSAGE "***** Time 10: " + STRING(TIME,"HH:MM:SS").

    /* -------- Grava retorno ------*/
    ASSIGN  jsonRetorno = NEW JsonArray().
            jsonRetorno:Read(TEMP-TABLE ttRetorno:HANDLE).

    RUN createJsonResponse(INPUT jsonRetorno, 
                           INPUT TABLE RowErrors, 
                           INPUT FALSE, 
                           OUTPUT jsonOutput).

    MESSAGE "***** Time 11: " + STRING(TIME,"HH:MM:SS").
                           
END PROCEDURE.


PROCEDURE pi-grava-erro:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER pErrornumber AS INTEGER   NO-UNDO.
DEFINE INPUT PARAMETER pErrorDescr  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER pErrorHelp   AS CHARACTER NO-UNDO.

ASSIGN i-seq-erro = i-seq-erro + 1.

CREATE RowErrors.                                                    
ASSIGN RowErrors.ErrorSequence    = i-seq-erro                              
       RowErrors.ErrorNumber      = pErrorNumber                               
       RowErrors.ErrorParameters  = ""                        
       RowErrors.ErrorType        = "":U                        
       RowErrors.ErrorSubType     = "":U                        
       RowErrors.ErrorDescription = pErrorDescr                    
       RowErrors.ErrorHelp        = pErrorHelp
       .  

END PROCEDURE.


PROCEDURE pi-gera-status:

    DEFINE INPUT PARAMETER c-erro AS CHARACTER NO-UNDO.

    DEFINE VARIABLE i-nr-seq AS INTEGER NO-UNDO.

    FIND LAST sfa-import-log NO-LOCK OF sfa-import NO-ERROR.
    IF AVAIL sfa-import-log THEN
        ASSIGN i-nr-seq = sfa-import-log.nr-seq + 1.
    ELSE i-nr-seq = 1.

/*
    IF c-erro = "" AND sfa-import.chave = ""
    THEN ASSIGN
       c-erro = "Pedido n∆o foi integrado".
*/
         

    CREATE sfa-import-log.
    ASSIGN sfa-import-log.ind-tipo-trans = sfa-import.ind-tipo-trans
           sfa-import-log.cd-tipo-integr = sfa-import.cd-tipo-integr
           sfa-import-log.id-movto       = sfa-import.id-movto      
           sfa-import-log.data-log       = NOW
           sfa-import-log.des-log        = IF c-erro <> "" THEN c-erro ELSE "Registro integrado com sucesso" 
           sfa-import-log.nr-seq         = i-nr-seq.

    CREATE ttRetorno.
    ASSIGN ttRetorno.CodigoPedido  = sfa-import.chave
           ttRetorno.situacao      = IF c-erro = "" THEN YES ELSE NO
           ttRetorno.descricao = IF c-erro = "" THEN "Registro integrado com sucesso" ELSE c-erro.

    
END PROCEDURE.


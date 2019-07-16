/*----------------------------------------------------------------------------------------------/
 Programa..: callbackfornecedor.p
 Objetivo..: API Json REST para retorno de avalia‡Æo de fornecedores do B2E
 Data......: 27/05/2019
 Autor.....: Marcelo Brasil
 VersÆo....: 1.000.000
-----------------------------------------------------------------------------------------------*/

{utp/ut-api.i}
{utp/ut-api-utils.i}

{include/i-prgvrs.i apisfacustomers 2.00.00.005 } /*** "010005" ***/

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i apisfacustomers MCD}
&ENDIF

{utp/ut-api-action.i pi-create POST /~* }

/* ------- Defini‡Æo Temp-tables ------ */   
DEF TEMP-TABLE tt-erros
    FIELD cod-erro         AS INTEGER
    FIELD desc-erro        AS CHARACTER FORMAT "x(256)"
    FIELD desc-arq         AS CHARACTER.

DEF TEMP-TABLE ttRetorno 
    FIELD codigofornecedor AS CHARACTER
    FIELD Situacao         AS LOGICAL 
    FIELD Descricao        AS CHAR FORMAT "x(200)".

/* ------- Defini‡Æo Vari veis ------ */
DEF VAR i-seq-erro AS INTEGER NO-UNDO.

EMPTY TEMP-TABLE tt-Erros.
EMPTY TEMP-TABLE ttRetorno.

PROCEDURE pi-create:
    DEF INPUT  PARAM jsonInput  AS JsonObject NO-UNDO.
    DEF OUTPUT PARAM jsonOutput AS JsonObject NO-UNDO.
    
    DEFINE VARIABLE jsonRetorno     AS JsonArray            NO-UNDO.
    DEFINE VARIABLE json_recebido   AS LONGCHAR             NO-UNDO.
    DEFINE VARIABLE oRequestParser  AS JsonAPIRequestParser NO-UNDO.
    DEFINE VARIABLE c-chave         AS CHARACTER INITIAL ?  NO-UNDO.
    DEFINE VARIABLE c-erro          AS CHARACTER            NO-UNDO.

    ASSIGN  oRequestParser = NEW JsonAPIRequestParser(jsonInput)
            json_recebido = oRequestParser:getPayloadLongChar().

    CREATE  api-import-for.
    ASSIGN  api-import-for.cd-tipo-integr  = 14 /*--- import ---*/
            api-import-for.Id-movto        = NEXT-VALUE(seq-import)
            api-import-for.data-movto      = TODAY
            api-import-for.c-json          = json_recebido.
            
    CREATE  sfa-import.
    ASSIGN  sfa-import.ind-tipo-trans   = 14 /*--- import ---*/
            sfa-import.cd-tipo-integr   = api-import-for.cd-tipo-integr
            sfa-import.id-movto         = api-import-for.Id-movto
            sfa-import.data-movto       = NOW
            sfa-import.data-inicio      = NOW
            sfa-import.data-fim         = ?
            sfa-import.ind-situacao     = 1 /*--- Pendente ---*/
            sfa-import.cod-status       = 0 /*--- sem status ---*/  .

    FIND FIRST es-api-param NO-LOCK 
         WHERE es-api-param.ind-tipo-trans = 1
           AND es-api-param.cd-tipo-integr = api-import-for.cd-tipo-integr 
         NO-ERROR.
    IF NOT AVAIL es-api-param 
    THEN RETURN. /*-----tratar erro */
   

    /* ------ Executa progama espec¡fico para o tipo de integra‡Æo ------ */
    RUN VALUE( es-api-param.programa-integr ) (INPUT ROWID(sfa-import),
                                               OUTPUT c-erro,
                                               OUTPUT sfa-import.chave,
                                               INPUT  jsonInput) NO-ERROR.   
    IF ERROR-STATUS:ERROR THEN DO:
        c-erro = ERROR-STATUS:GET-MESSAGE(1) + 'propath: ' + PROPATH.
    END.
    ASSIGN 
       sfa-import.data-fim     = NOW
       sfa-import.ind-situacao = 2 .
           

    /* ------ Gerencia retorno do processo -----*/
    IF c-erro = "" THEN ASSIGN sfa-import.cod-status = 1.
    ELSE ASSIGN sfa-import.cod-status = 2.
   
    RUN pi-gera-status (INPUT c-erro).

    /* -------- Grava retorno ------*/
    ASSIGN  jsonRetorno = NEW JsonArray().
            jsonRetorno:Read(TEMP-TABLE ttRetorno:HANDLE).

    RUN createJsonResponse(INPUT jsonRetorno, 
                           INPUT TABLE RowErrors, 
                           INPUT FALSE, 
                           OUTPUT jsonOutput).
                           
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

    FIND LAST sfa-import-log NO-LOCK 
           OF sfa-import NO-ERROR.
    IF AVAIL sfa-import-log 
    THEN ASSIGN 
       i-nr-seq = sfa-import-log.nr-seq + 1.
    ELSE ASSIGN 
       i-nr-seq = 1.

    CREATE sfa-import-log.
    ASSIGN sfa-import-log.ind-tipo-trans = sfa-import.ind-tipo-trans
           sfa-import-log.cd-tipo-integr = sfa-import.cd-tipo-integr
           sfa-import-log.id-movto       = sfa-import.id-movto      
           sfa-import-log.data-log       = NOW
           sfa-import-log.des-log        = c-erro
           sfa-import-log.nr-seq         = i-nr-seq.

    FIND FIRST emitente NO-LOCK 
         WHERE emitente.cod-emitente = INT(sfa-import.chave) 
         NO-ERROR.
    
    CREATE ttRetorno.
    ASSIGN ttRetorno.codigofornecedor = IF AVAIL emitente THEN string(emitente.cod-emitente) ELSE "0"
           ttRetorno.Situacao         = IF c-erro = "" THEN YES ELSE NO
           ttRetorno.Descricao        = c-erro.

END PROCEDURE.

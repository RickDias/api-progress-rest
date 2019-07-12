/*----------------------------------------------------------------------------------------------/
 Programa..: aribasupplieragreement.p
 Objetivo..: API Json REST para Integração de Contrato de Fornecedor
 Data......: 03/07/2019
 Autor.....: 
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
    FIELD cod-erro      AS INTEGER
    FIELD desc-erro     AS CHARACTER FORMAT "x(256)"
    FIELD desc-arq      AS CHARACTER.

DEF TEMP-TABLE ttRetorno 
    FIELD c-numero-contrato AS CHARACTER
    FIELD situacao          AS LOGICAL 
    FIELD descricao         AS CHAR FORMAT "x(200)".

/* ------- Defini‡Æo Vari veis ------ */
DEF VAR i-seq-erro AS INTEGER NO-UNDO.

EMPTY TEMP-TABLE tt-Erros.
EMPTY TEMP-TABLE ttRetorno.

//Consome o Json - Recebe informações do Ariba 
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
    DEFINE VARIABLE c-numero-contrato AS CHARACTER             NO-UNDO.


    jsonInput:writeFile("c:\temp\aribasupplieragreement.json").

    fix-codepage(json_recebido) = "UTF-8".

    ASSIGN  oRequestParser = NEW JsonAPIRequestParser(jsonInput)
            json_recebido = oRequestParser:getPayloadLongChar() .

    /* ---- Lˆ propriedade Principal ---- */        
   // oJsonArrayMain = jsonInput:GetJsonObject("payload":U):GetJsonArray("req":U).
    oJsonArrayMain = jsonInput:GetJsonObject("payload":U):GetJsonArray("ContratoFornecedor":U).

      DO iCountMain = 1 TO oJsonArrayMain:LENGTH:
        oJsonObjectMain =  oJsonArrayMain:GetJsonObject(iCountMain).

        IF oJsonObjectMain:Has("ContratoFornecedor") then do:
            c-numero-contrato = oJsonObjectMain:GetCharacter("nr-contrato")  NO-ERROR.       
            LEAVE.
        END.
    END.

    //Necessario criar a Df da Tabela (sfa-import-contrato
    //basear a tabela na sfa-import-cli 
    CREATE  sfa-import-contr.
    ASSIGN  sfa-import-contr.cd-tipo-integr  = 1 /*--- import ---*/
            sfa-import-contr.Id-movto        = NEXT-VALUE(seq-import)
            sfa-import-contr.data-movto      = TODAY
            sfa-import-contr.c-json          = json_recebido
            sfa-import-contr.nr-contrato     = int(c-numero-contrato).

    RELEASE sfa-import-contr.

    //Cria as informa‡äes de log da Importa‡Æo na tabela (Visualiza no esp\esint006)        
    CREATE  sfa-import.
    ASSIGN  sfa-import.ind-tipo-trans   = 1 /*--- import ---*/
            sfa-import.cd-tipo-integr   = sfa-import-contr.cd-tipo-integr
            sfa-import.id-movto         = sfa-import-contr.Id-movto
            sfa-import.chave            = c-numero-contrato
            sfa-import.data-movto       = NOW
            sfa-import.data-inicio      = NOW
            sfa-import.data-fim         = ?
            sfa-import.ind-situacao     = 1 /*--- Pendente ---*/
            sfa-import.cod-status       = 0 /*--- sem status ---*/  .

     RELEASE sfa-import.

     FIND FIRST es-api-param WHERE es-api-param.ind-tipo-trans = 1
                              AND es-api-param.cd-tipo-integr = sfa-import-contr.cd-tipo-integr NO-LOCK NO-ERROR.
     IF NOT AVAIL es-api-param THEN RETURN. /*-----tratar erro */

     /* ------ Executa progama espec¡fico para o tipo de integra‡Æo ------ */
    RUN VALUE( es-api-param.programa-integr ) (INPUT ROWID(sfa-import),
                                               OUTPUT c-erro,
                                               INPUT jsonInput) NO-ERROR.   
    IF ERROR-STATUS:ERROR THEN DO:
        c-erro = c-erro + ERROR-STATUS:GET-MESSAGE(1) + 'propath: ' + PROPATH.
    END.
    ASSIGN sfa-import.data-fim     = NOW
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

    FIND LAST sfa-import-log NO-LOCK OF sfa-import NO-ERROR.
    IF AVAIL sfa-import-log THEN
        ASSIGN i-nr-seq = sfa-import-log.nr-seq + 1.
    ELSE i-nr-seq = 1.

    CREATE sfa-import-log.
    ASSIGN sfa-import-log.ind-tipo-trans = sfa-import.ind-tipo-trans
           sfa-import-log.cd-tipo-integr = sfa-import.cd-tipo-integr
           sfa-import-log.id-movto       = sfa-import.id-movto      
           sfa-import-log.data-log       = NOW
           sfa-import-log.des-log        = IF c-erro = "" THEN "Registro integrado com sucesso" ELSE c-erro
           sfa-import-log.nr-seq         = i-nr-seq.

    FIND FIRST contrato-for NO-LOCK WHERE contrato-for.nr-contrato = int(sfa-import.chave) NO-ERROR.

    //Verificar a tabela de retorno para informa o erro contrato-for
    CREATE ttRetorno.
    ASSIGN ttRetorno.c-numero-contrato = IF AVAIL contrato-for THEN string(contrato-for.nr-contrato) ELSE "0"
           ttRetorno.situacao      = IF c-erro = "" THEN YES ELSE NO
           ttRetorno.descricao     = IF c-erro = "" THEN "Registro integrado com sucesso" ELSE c-erro.

    
END PROCEDURE.



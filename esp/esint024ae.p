/*----------------------------------------------------------------------------------------------/
 Programa..: esint024ae.p
 Objetivo..: Interface Chamada Alteracao de Codigo de Fornecedor Ariba
 Data......: 29/05/2019
 Autor.....: Marcelo Brasil
 Vers∆o....: 1.000.000
-----------------------------------------------------------------------------------------------*/
/* ------ Definiá∆o das classes de objetos ------ */

using OpenEdge.Core.String.
using OpenEdge.Net.HTTP.IHttpClientLibrary.
using OpenEdge.Net.HTTP.ConfigBuilder.
using OpenEdge.Net.HTTP.ClientBuilder.
using OpenEdge.Net.HTTP.Credentials.
using OpenEdge.Net.HTTP.IHttpClient.
using OpenEdge.Net.HTTP.IHttpRequest.
using OpenEdge.Net.HTTP.RequestBuilder.
using OpenEdge.Net.HTTP.ResponseBuilder.
using OpenEdge.Net.URI.
using OpenEdge.Net.HTTP.IHttpResponse.
using Progress.Json.OBJECTModel.JsonOBJECT.
using Progress.Json.OBJECTModel.JsonArray.
USING Progress.Lang.*. 
USING Progress.Json.ObjectModel.*. 


MESSAGE PROGRAM-NAME(1)
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

{include/i-prgvrs.i ESINT020BE 1.00.00.000} 

/* ------- Definiá∆o de ParÉmetros ----- */
DEFINE INPUT  PARAMETER r-table AS ROWID     NO-UNDO.
DEFINE OUTPUT PARAMETER c-erro  AS CHARACTER NO-UNDO.

/* ------- Definiá∆o de Vari†veis ----- */
DEFINE VARIABLE oJsonObjMain      AS JsonObject        NO-UNDO.
DEFINE VARIABLE oJsonArrayMain    AS JsonArray         NO-UNDO.
DEFINE VARIABLE oJsonObjIni       AS jsonObject        NO-UNDO.
DEFINE VARIABLE ojsonArrayIni     AS JsonArray         NO-UNDO.
DEFINE VARIABLE ojsonObjAux       AS JsonObject        NO-UNDO.
DEFINE VARIABLE ojsonArrayAux     AS JsonArray         NO-UNDO.
                                                      
DEFINE VARIABLE h-temp            AS HANDLE            NO-UNDO.
DEFINE VARIABLE h-esint002        AS HANDLE            NO-UNDO.
DEFINE VARIABLE c-json            AS LONGCHAR          NO-UNDO.
DEFINE VARIABLE lEnviou           AS LOGICAL           NO-UNDO.
DEFINE VARIABLE c-arq-json        AS CHARACTER         NO-UNDO.
DEFINE VARIABLE lresp             AS LOGICAL           NO-UNDO.

DEF         VAR ojsonRet          AS jsonobject        NO-UNDO.

DEF         VAR ojson             AS jsonobject        NO-UNDO.
DEF         VAR ojsonarraysec     AS jsonarray         NO-UNDO.
DEF         VAR oJsonObjectSec    AS jsonobject        NO-UNDO.
DEF         VAR iCountSec         AS i                 NO-UNDO.
DEF         VAR myParser          AS ObjectModelParser NO-UNDO.
DEF         VAR c-retorno         AS LONGCHAR          NO-UNDO.
        

DEF         VAR cSucesso          AS l                 NO-UNDO.
DEF         VAR daDtConsulta      AS da                NO-UNDO.

ASSIGN
   daDtConsulta = TODAY.

/* ------- Definiá∆o de Temp-Tables ------ */

//{esp/esint021ae.i}


DEF TEMP-TABLE fornecedor-ariba NO-UNDO SERIALIZE-NAME "Fornecedor_Ariba"
    FIELD InternalID                          AS CHAR
    
    FIELD ID                                  AS c
    FIELD UUID                                AS c
    FIELD ID_1                                AS c
    FIELD UUID_1                              AS c
    FIELD UUID_2                              AS c
    FIELD ReceiverUUID                        AS c
    FIELD ReceiverInternalID                  AS c.

DEFINE TEMP-TABLE RowErrors NO-UNDO
    FIELD ErrorSequence    AS INTEGER
    FIELD ErrorNumber      AS INTEGER
    FIELD ErrorDescription AS CHARACTER
    FIELD ErrorParameters  AS CHARACTER
    FIELD ErrorType        AS CHARACTER
    FIELD ErrorHelp        AS CHARACTER
    FIELD ErrorSubType     AS CHARACTER.

/*------------------------------ Main Begin ----------------------------*/

ASSIGN 
   c-erro = "".

/* ---- Chama o programa persistent ----- */
RUN esp/esint002.p PERSISTENT SET h-esint002 NO-ERROR.
IF ERROR-STATUS:ERROR THEN DO:
    ASSIGN c-erro = ERROR-STATUS:GET-MESSAGE(1).
    RETURN "NOK".
END.

/* --------- Cria Objeto ------------------*/
RUN piGeraObjJson IN h-esint002 (OUTPUT oJsonObjMain) NO-ERROR.
IF ERROR-STATUS:ERROR THEN DO:
    ASSIGN c-erro = ERROR-STATUS:GET-MESSAGE(1).
    RETURN "NOK".
END.
FIND FIRST sfa-export NO-LOCK 
     WHERE ROWID(sfa-export) = r-table 
     NO-ERROR.
IF AVAIL sfa-export 
THEN DO:
    FIND FIRST es-api-param NO-LOCK 
         WHERE es-api-param.ind-tipo-trans = sfa-export.ind-tipo-trans
           AND es-api-param.cd-tipo-integr = sfa-export.cd-tipo-integr 
         NO-ERROR. 

    FIND FIRST api-export-ariba-codigo 
            OF sfa-export 
         NO-ERROR.
    IF AVAIL api-export-ariba-codigo 
    THEN DO:
        RUN piGravaTTFornecedor (OUTPUT c-json,
                                 OUTPUT c-erro).

        ASSIGN 
            api-export-ariba-codigo.c-json = c-Json.
        
        /* ------------ Envia Objeto Json --------- */
        RUN piPostJsonObj /*IN h-esint002*/ 
                          (INPUT oJsonObjMain,
                           INPUT rowid(es-api-param),
                           OUTPUT lResp,
                           OUTPUT TABLE RowErrors,
                           OUTPUT c-retorno,
                           OUTPUT ojsonRet
                          ).
                                   
        IF TEMP-TABLE rowErrors:HAS-RECORDS 
        THEN DO:
            FOR EACH rowErrors:
                ASSIGN 
                   c-erro = c-erro + string(rowerrors.ErrorNumber)  + " - " + rowerrors.ErrorDescription.
                DELETE OBJECT h-esint002.
            END.
        END.        
        ELSE DO: 
           //RUN piCast (ojsonRet).
           //RUN piAtualizaFornecedor.
        END.
    END.
    ELSE ASSIGN 
       c-erro = c-erro
              + "Registro tabela do fornecedor n∆o localizada".
    
    IF c-erro > ""
    THEN RETURN "NOK".

END.

IF VALID-HANDLE(h-esint002) 
THEN DELETE OBJECT h-esint002.

/* -------------------------------------------------------------- Procedures ------------------------------------------------*/
PROCEDURE piGravaTTFornecedor:

    DEFINE OUTPUT PARAMETER pArquivoEnvio  AS LONGCHAR  NO-UNDO.
    DEFINE OUTPUT PARAMETER pErro          AS CHARACTER NO-UNDO.
    DEF VAR h-temp AS HANDLE NO-UNDO.

    FIND FIRST es-fornecedor-ariba
         WHERE es-fornecedor-ariba.Number      = api-export-ariba-codigo.Number          
           AND es-fornecedor-ariba.dt-consulta = api-export-ariba-codigo.dt-consulta     
         NO-ERROR.

    MESSAGE 
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

    CREATE fornecedor-ariba.

    ASSIGN 
       fornecedor-ariba.InternalID           = es-fornecedor-ariba.number
       fornecedor-ariba.ID                   = es-fornecedor-ariba.ID    
       fornecedor-ariba.UUID                 = es-fornecedor-ariba.UUID  
       fornecedor-ariba.ID_1                 = es-fornecedor-ariba.ID_1  
       fornecedor-ariba.UUID_1               = es-fornecedor-ariba.UUID_1
       fornecedor-ariba.UUID_2               = es-fornecedor-ariba.UUID_2
       fornecedor-ariba.ReceiverUUID         = ""
       fornecedor-ariba.ReceiverInternalID   = STRING(es-fornecedor-ariba.cod-emitente).

    MESSAGE 
        "fornecedor-ariba.InternalID        " fornecedor-ariba.InternalID          SKIP
        "fornecedor-ariba.ReceiverInternalID" fornecedor-ariba.ReceiverInternalID
    
        "es-fornecedor-ariba.number      " es-fornecedor-ariba.number               SKIP
        "es-fornecedor-ariba.cod-emitente" STRING(es-fornecedor-ariba.cod-emitente)
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

    h-temp = BUFFER fornecedor-ariba:HANDLE.

    RUN piCriaObj IN h-esint002 (INPUT h-temp,
                                 OUTPUT ojsonObjIni,
                                 OUTPUT ojsonArrayIni,
                                 INPUT NO) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        ASSIGN c-erro = ERROR-STATUS:GET-MESSAGE(1).
        DELETE OBJECT h-temp.
        RETURN "NOK".
    END.            
    DELETE OBJECT h-temp.

    oJsonArrayMain = NEW JsonArray().
    oJsonArrayMain:ADD(ojsonObjIni).
        
    /* ----- Cria Json Principal ------- */
    oJsonObjMain = NEW JsonObject().
    oJsonObjMain:ADD("Fornecedor_Ariba",oJsonArrayMain).
    
END PROCEDURE.



PROCEDURE piPostJsonObj:

    DEF INPUT  PARAM oInputData       AS JsonObject NO-UNDO.
    DEF INPUT  PARAM pRowTipoIntegr   AS ROWID      NO-UNDO.
    DEF OUTPUT PARAM pResp            AS LOGICAL    NO-UNDO.
    DEF OUTPUT PARAM TABLE FOR RowErrors.
    DEF OUTPUT PARAM JsonString       AS LONGCHAR   NO-UNDO.
    DEF OUTPUT PARAM oOutputData      AS JsonObject NO-UNDO.
    
    DEF VAR oRequest         as IHttpRequest                  no-undo.
    DEF VAR oResponse        as IHttpResponse                 no-undo.
    DEF VAR oHttpClient      AS OpenEdge.Net.HTTP.IHttpClient NO-UNDO.
    DEF VAR oRequestBody     as String                        no-undo.
    DEF VAR oJsonObject      AS JsonObject                    NO-UNDO.
    DEF VAR oJsonEntity      AS JsonArray                     NO-UNDO.
    DEF VAR iIndexErro       AS INTEGER                       NO-UNDO.    
    DEF VAR icount           AS INTEGER                       NO-UNDO.
    DEF VAR itoken           AS i                             NO-UNDO.
    DEF VAR oURI             AS URI                           NO-UNDO.
    DEF VAR oClient          AS IHttpClient                   NO-UNDO.
    DEF VAR httpUrl          as character                     no-undo.
    DEF VAR cToken           AS CHARACTER                     NO-UNDO.
    DEF VAR client           AS COM-HANDLE.
    
    FIND FIRST es-api-param NO-LOCK WHERE ROWID(es-api-param) = pRowTipoIntegr NO-ERROR.
    IF NOT AVAIL es-api-param THEN DO:
       RUN piErro IN h-esint002 ("Tipo de Integraá∆o nao encontrada","").
       RETURN "NOK".
    END.

    DO itoken = 1 TO 30:
       RUN piGeraTokenApigee IN h-esint002 (OUTPUT ctoken).
       IF cToken > ""
       THEN LEAVE.
    END.

    IF cToken = "" THEN DO:
        MESSAGE "Token n∆o encontrado!" .
        RUN piErro IN h-esint002 ("Token n∆o encontrado!","").
        RETURN "NOK".
    END.
    ELSE DO:

        ASSIGN httpUrl = es-api-param.host-integr + ":" + string(es-api-param.porta-integr) + es-api-param.path-integr.
        oJsonObject = CAST(oInputData, JsonObject). 
        JsonString = STRING(oJsonObject:getJsonText()).

/**/
       OUTPUT TO c:\temp\jsonalterafornecedor1.json.
       EXPORT JsonString.
       OUTPUT CLOSE.
/**/                


        ASSIGN oRequest = RequestBuilder:Post(httpUrl, oInputData)
                                        :ContentType('application/json')
                                        :AcceptJson()
                                        :AddHeader("Authorization":U, "Bearer ":U + cToken)                                
                                        :Request.
                
        oClient = ClientBuilder:Build():Client.
        oResponse = ResponseBuilder:Build():Response.
        oClient:Execute(oRequest,oResponse)  NO-ERROR.

        IF ERROR-STATUS:ERROR THEN DO:
            RUN piErro IN h-esint002 ("Ocorreram erros no envio do Json - " + STRING(ERROR-STATUS:GET-MESSAGE(1)), "" ).
            RETURN "NOK".            
        END.

        oJsonObject = NEW JsonObject(). 


        MESSAGE PROGRAM-NAME(1) 0 oResponse:StatusCode
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.

        IF oResponse:StatusCode < 200 OR oResponse:StatusCode > 299 THEN DO:
            RUN piErro IN h-esint002 ("Ocorreram erros no envio do Json - " + 
                        STRING(oResponse:statusCode)          + 
                        " - " + 
                        STRING(oResponse:StatusReason),"").
            RETURN "NOK".
        END.
        ELSE DO:
            IF TYPE-OF(oResponse:Entity, JsonArray) THEN DO:
                oJsonEntity = CAST(oResponse:Entity, JsonArray).                
                oJsonObject:ADD("retorno",oJsonEntity).
                JsonString = string(oJsonObject:getJsonText()).
                MESSAGE PROGRAM-NAME(1) 1
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
            END.
            IF TYPE-OF(oResponse:Entity, JsonObject) 
            THEN DO:
                MESSAGE PROGRAM-NAME(1) 2
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
                oJsonObject = CAST(oResponse:Entity, JsonObject). 
                ASSIGN
                   oOutputData = oJsonObject.

                RUN piGeraVarJson IN h-esint002 (INPUT oJsonObject,
                                                 OUTPUT c-Json) NO-ERROR.
                IF ERROR-STATUS:ERROR THEN DO:
                    ASSIGN c-erro = ERROR-STATUS:GET-MESSAGE(1).
                    DELETE OBJECT h-esint002.
                    RETURN "NOK".
                END.
/**/
                OUTPUT TO c:\temp\jsonaltforn1.json.
                EXPORT c-json.
                OUTPUT CLOSE.
/**/                
                COPY-LOB c-json TO sfa-export.clob-retorno.
                
            END.
            ELSE DO:
                MESSAGE PROGRAM-NAME(1) 3
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
                IF TYPE-OF(oResponse:Entity, String) 
                THEN JsonString = CAST(oResponse:Entity, String):Value.
                ELSE JsonString = oResponse:ToString().
                MESSAGE PROGRAM-NAME(1) 3.1 TYPE-OF(oResponse:Entity, String)  SKIP(3)
                    STRING(JsonString)
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.

            END.
            MESSAGE PROGRAM-NAME(1) 4
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
            IF index(JsonString,'"Status":false') > 0 THEN DO:
                IF LOOKUP("Description",JsonString,'"') > 0 THEN 
                    RUN piErro (ENTRY(LOOKUP("Description",JsonString,'"') + 2,JsonString,'"'),"").
            END.
        END.
    END.


END PROCEDURE.


{esp\esint001rp.i}

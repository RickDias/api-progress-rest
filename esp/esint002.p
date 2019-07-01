/*----------------------------------------------------------------------------------------------/
Programa..: esapi002.p
Objetivo..: API Integraá‰es JSon
Data......: 26/02/2019
Autor.....: RogÇrio Dias
Vers∆o....: 1.000.000
-----------------------------------------------------------------------------------------------*/


/* ------- Importaá∆o de Classe ----- */
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

{include/i-prgvrs.i ESAPI002 1.00.00.000} 

DEF TEMP-TABLE ttCurlRet NO-UNDO
        FIELD Seq AS i
        FIELD Ret AS c
        INDEX i1 Seq
        INDEX i2 Ret.

DEFINE TEMP-TABLE RowErrors NO-UNDO
    FIELD ErrorSequence    AS INTEGER
    FIELD ErrorNumber      AS INTEGER
    FIELD ErrorDescription AS CHARACTER
    FIELD ErrorParameters  AS CHARACTER
    FIELD ErrorType        AS CHARACTER
    FIELD ErrorHelp        AS CHARACTER
    FIELD ErrorSubType     AS CHARACTER.

DEF STREAM sCurl.
DEF VAR cResposta AS c NO-UNDO.
DEF VAR iErro     AS i NO-UNDO.

PROCEDURE piGeraObjJson:
    /*-----------------------------------------------------------
      Prop¢sito: Instancia Objeto JsonOBJect
    ------------------------------------------------------------*/

    DEFINE OUTPUT PARAM pJsonObjMaster AS JsonObject NO-UNDO.

    /* --------- Cria Objeto Principal ------ */
    pJsonObjMaster = NEW JsonObject().

END PROCEDURE.



PROCEDURE piCriaObj:
    /*-----------------------------------------------------------
      Prop¢sito: Adiciona elementos ao objeto JsonOBJect
    ------------------------------------------------------------*/
    
    /* ------- Definiá∆o de ParÉmetros ----- */
    DEFINE INPUT  PARAM pTable         AS HANDLE     NO-UNDO.  
    DEFINE OUTPUT PARAM pJsonObjAux    AS JsonObject NO-UNDO.
    DEFINE OUTPUT PARAM pJsonArrayAux  AS JsonArray  NO-UNDO.
    DEFINE INPUT  PARAM lArray         AS LOGICAL    NO-UNDO.
    
    /* ------- Definiá∆o de Vari†veis ----- */
    DEFINE VARIABLE i_reg         AS INTEGER    NO-UNDO.
    DEFINE VARIABLE i_fld         AS INTEGER    NO-UNDO.   
    DEFINE VARIABLE qh            AS HANDLE     NO-UNDO.

    CREATE query qh.
    qh:SET-BUFFERS(pTable).
    qh:QUERY-PREPARE("for each " + pTable:NAME).
    qh:QUERY-OPEN(). 

    repeat:

        qh:get-next().  
        IF qh:QUERY-OFF-END THEN LEAVE.

        i_reg = i_reg + 1.

        pJsonObjAux = NEW JsonObject().

        DO i_fld = 1 TO pTable:NUM-FIELDS:

            IF pTable:BUFFER-FIELD(i_fld):NAME = "fld-rel" THEN NEXT.

            IF pTable:BUFFER-FIELD(i_fld):EXTENT > 0 THEN
                pJsonObjAux:ADD(pTable:BUFFER-FIELD(i_fld):SERIALIZE-NAME,pTable:BUFFER-FIELD(i_fld):BUFFER-VALUE(1)).                
            ELSE
                pJsonObjAux:ADD(pTable:BUFFER-FIELD(i_fld):SERIALIZE-NAME,pTable:BUFFER-FIELD(i_fld):BUFFER-VALUE()).                
        END.

        IF lArray THEN DO:

            IF i_reg = 1 THEN
                pJsonArrayAux = NEW JsonArray().

            pJsonArrayAux:ADD(pJsonObjAux).
        END.

    END.
    qh:QUERY-CLOSE().

    delete object pTable.
    delete object qh.     


END PROCEDURE.


PROCEDURE piGeraArqJson:

    /*-----------------------------------------------------------
      Prop¢sito: Gera um arquivo .json do Objeto criado
     ------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pJsonObjMaster AS JsonObject NO-UNDO.
    DEFINE INPUT PARAMETER pCaminho       AS CHARACTER  NO-UNDO.
    DEFINE INPUT PARAMETER pTipoIntegr    AS CHARACTER  NO-UNDO.
    DEFINE OUTPUT PARAMETER pJsonFile     AS CHARACTER  NO-UNDO.

    ASSIGN pJsonFile = pCaminho + "sfa-" + pTipoIntegr + "-" + STRING(TIME) + STRING(RANDOM(100000,200000)) + ".json".

    pJsonObjMaster:WriteFile(pJsonFile, TRUE).

END.

PROCEDURE piGeraVarJson:
    /*-----------------------------------------------------------
      Prop¢sito: Gera um arquivo .json do Objeto criado
     ------------------------------------------------------------*/

    DEFINE INPUT PARAMETER pJsonObjMaster AS JsonObject NO-UNDO.
    DEFINE OUTPUT PARAMETER pJson         AS LONGCHAR   NO-UNDO.

    pJsonObjMaster:WRITE(INPUT-OUTPUT pJson, TRUE).

END.
   

PROCEDURE piEnvArqJson:

    DEFINE INPUT PARAM pArqJson AS CHARACTER NO-UNDO.

END PROCEDURE.


PROCEDURE piGeraTokenApigeeCurl:
    /*-----------------------------------------------------------
      Prop¢sito: Recupera um token v†lido no host APiGee
     ------------------------------------------------------------*/

    DEFINE OUTPUT PARAM pToken AS CHARACTER NO-UNDO.
      
    SESSION:DEBUG-ALERT = true.
    
    DEF VAR cInput AS c NO-UNDO.
    DEF VAR cCurl  AS c NO-UNDO.
    DEF VAR iSeq   AS i NO-UNDO.
    DEF VAR cArq   AS c NO-UNDO.

    FIND FIRST es-api-token-param NO-LOCK NO-ERROR.
    IF NOT AVAIL es-api-token-param THEN RETURN.
    
    FIND FIRST es-api-token-apigee NO-LOCK WHERE es-api-token-apigee.Validade-Final > NOW NO-ERROR.
    IF AVAIL es-api-token-apigee THEN DO:
        ASSIGN pToken = es-api-token-apigee.Token.
        RETURN.
    END.

    ASSIGN 
       cArq  = SESSION:TEMP-DIRECTORY + "apigee-" + STRING(TIME) + STRING(RANDOM(100000,200000)) + ".json"
       cCurl = 'curl -silent'
             + '-X '
             + ' POST '
             + '"'                   + es-api-token-param.uri + '"'
             + ' -d "client_id='     + es-api-token-param.client_id     + '" '
             + ' -d "client_secret=' + es-api-token-param.client_secret + '"'
             + ' -H "Content-Type: application/x-www-form-urlencoded"'
             + ' > ' + cArq 
             .

    OS-COMMAND SILENT VALUE(cCurl).

    INPUT STREAM sCurl FROM VALUE(cArq) NO-ECHO.
    REPEAT:
        CREATE ttCurlRet.
        ASSIGN
           ttCurlRet.Seq = iSeq
           iSeq          = iSeq + 1.

        IMPORT STREAM sCurl UNFORMATTED 
           ttCurlRet.Ret.
    END.
    INPUT STREAM sCurl CLOSE.

    FIND FIRST ttCurlRet WHERE ttCurlRet.Ret MATCHES '*"access_token"*' NO-ERROR.
    IF AVAIL ttCurlRet THEN DO:
        CREATE es-api-token-apigee.
        ASSIGN es-api-token-apigee.Token = ENTRY(4,ttCurlRet.Ret,'"')
               pToken                    = es-api-token-apigee.Token.

        FIND FIRST ttCurlRet  WHERE ttCurlRet.Ret MATCHES '*"expires_in"*' NO-ERROR.
        IF AVAIL ttCurlRet THEN DO:
            ASSIGN es-api-token-apigee.Validade-Inicio = DATETIME(STRING(TODAY) + " " + STRING(TIME,"hh:mm:ss"))
                   es-api-token-apigee.Validade-Final  = DATETIME(STRING(TODAY) + " " + STRING(TIME + INT(ENTRY(4,ttCurlRet.Ret,'"')),"hh:mm:ss")).
                   es-api-token-apigee.Validade-Time   = INT(ENTRY(4,ttCurlRet.Ret,'"')).
       END.
    END.

    OS-DELETE VALUE(cArq).

END PROCEDURE.   

PROCEDURE piGeraTokenApigee:

    DEFINE OUTPUT PARAM pToken       AS CHARACTER                     NO-UNDO.

    DEF             VAR cExpire      AS c                             NO-UNDO.
    DEF             VAR cCurl        AS c                             NO-UNDO.
    DEF             VAR httpUrl      as character                     no-undo.
    DEF             VAR oRequest     as IHttpRequest                  no-undo.
    DEF             VAR oResponse    as IHttpResponse                 no-undo.
    DEF             VAR oHttpClient  AS OpenEdge.Net.HTTP.IHttpClient NO-UNDO.
    DEF             VAR oRequestBody as String                        no-undo.
    DEF             VAR JsonString   AS LONGCHAR                      NO-UNDO.
    DEF             VAR oJsonEntity  AS JsonObject                    NO-UNDO.
    

    
    FIND FIRST es-api-token-param NO-LOCK NO-ERROR.
    IF NOT AVAIL es-api-token-param THEN RETURN.

    FIND FIRST es-api-token-apigee NO-LOCK WHERE es-api-token-apigee.Validade-Final > NOW NO-ERROR.
    IF AVAIL es-api-token-apigee THEN DO:
        ASSIGN pToken = es-api-token-apigee.Token.
        RETURN.
    END.

    httpUrl      = es-api-token-param.uri.
    oRequestBody = new String("client_id="     + es-api-token-param.client_id
                            + '&'
                            + "client_secret=" + es-api-token-param.client_secret).

    oRequest     = RequestBuilder:Post(httpUrl, oRequestBody)
                  :ContentType('application/x-www-form-urlencoded')
                  :AcceptJson()
                  :Request.

    oResponse = ClientBuilder:Build():Client:Execute(oRequest).
    
    IF oResponse:StatusCode >= 200 AND oResponse:StatusCode <= 299 THEN DO:   

         oJsonEntity = CAST(oResponse:Entity, JsonObject).                    
         pToken      = oJsonEntity:GetJsonText("access_token").                
         cExpire     = oJsonEntity:GetJsonText("expires_in").    

         CREATE es-api-token-apigee.
         ASSIGN es-api-token-apigee.Token           = pToken
            
            es-api-token-apigee.Validade-Inicio = DATETIME(STRING(TODAY) + " " + STRING(TIME,"hh:mm:ss"))
            es-api-token-apigee.Validade-Final  = DATETIME(STRING(TODAY) + " " + STRING(INT(cExpire),"hh:mm:ss")).
            es-api-token-apigee.Validade-Time   = INT(cExpire).

    END.
    ELSE ASSIGN pToken = "".                                              
END.


PROCEDURE piErro:

    DEF INPUT PARAM cErrorDescription AS CHARACTER NO-UNDO.
    DEF INPUT PARAM cErrorHelp        AS CHARACTER NO-UNDO.

    CREATE RowErrors.
    ASSIGN iErro            = iErro + 1
           ErrorSequence    = iErro
           ErrorNumber      = 17006
           ErrorType        = "Error"
           ErrorDescription = cErrorDescription
           ErrorHelp        = cErrorHelp       .
END.

PROCEDURE piConvLongObj:

    DEFINE INPUT  PARAMETER pJsonString AS LONGCHAR          NO-UNDO. 
    DEFINE OUTPUT PARAMETER ojson       AS JsonObject        NO-UNDO. 
    
    DEFINE VARIABLE myParser AS ObjectModelParser NO-UNDO. 
    
    myParser = NEW ObjectModelParser(). 
    ojson = CAST(myParser:Parse(pJsonString ),JsonObject).

END PROCEDURE.


PROCEDURE piPostJsonObj:

    DEF INPUT  PARAM oInputData       AS JsonObject NO-UNDO.
    DEF INPUT  PARAM pRowTipoIntegr   AS ROWID      NO-UNDO.
    DEF OUTPUT PARAM pResp            AS LOGICAL    NO-UNDO.
    DEF OUTPUT PARAM TABLE FOR RowErrors.
    DEF OUTPUT PARAM JsonString       AS LONGCHAR   NO-UNDO.

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
       RUN piErro ("Tipo de Integraá∆o nao encontrada","").
       RETURN "NOK".
    END.

    DO itoken = 1 TO 30:
       RUN piGeraTokenApigee (OUTPUT ctoken).
       IF cToken > ""
       THEN LEAVE.
    END.

    IF cToken = "" THEN DO:
        MESSAGE "Token n∆o encontrado!" VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RUN piErro ("Token n∆o encontrado!","").
        RETURN "NOK".
    END.
    ELSE DO:

        ASSIGN httpUrl = es-api-param.host-integr + ":" + string(es-api-param.porta-integr) + es-api-param.path-integr.

        ASSIGN oRequest = RequestBuilder:Post(httpUrl, oInputData)
                                        :ContentType('application/json')
                                        :AcceptJson()
                                        :AddHeader("Authorization":U, "Bearer ":U + cToken)                                
                                        :Request.

        oClient = ClientBuilder:Build():Client.
        oResponse = ResponseBuilder:Build():Response.
        oClient:Execute(oRequest,oResponse)  NO-ERROR .
        
        IF ERROR-STATUS:ERROR THEN DO:
            RUN piErro ("Ocorreram erros no envio do Json - " + STRING(ERROR-STATUS:GET-MESSAGE(1)), "" ).
            RETURN "NOK".            
        END.

        oJsonObject = NEW JsonObject(). 

        IF oResponse:StatusCode < 200 OR oResponse:StatusCode > 299 THEN DO:
            RUN piErro ("Ocorreram erros no envio do Json - " + 
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
            END.
            ELSE IF TYPE-OF(oResponse:Entity, JsonObject) THEN DO:
                oJsonObject = CAST(oResponse:Entity, JsonObject). 
                JsonString = STRING(oJsonObject:getJsonText()).
            END.
            ELSE DO:
                JsonString = CAST(oResponse:Entity, String):Value.
            END.

            IF index(JsonString,'"Status":false') > 0 THEN DO:
                IF LOOKUP("Description",JsonString,'"') > 0 THEN DO:
                    RUN piErro (ENTRY(LOOKUP("Description",JsonString,'"') + 2,JsonString,'"'),"").
                END.
            END.
        END.
    END.


END PROCEDURE.


PROCEDURE piPostJson:
    /*-----------------------------------------------------------
      PropΩsito: Envia objeto utilizando m≤todo POST:
      Obs.: Necessˇrio parametrizar informaªÑes de conexío no programa: 
    ------------------------------------------------------------*/

    /* ------ Par≥metros ------ */
    /*DEFINE INPUT  PARAM pJsonObjMaster AS JsonObject NO-UNDO.*/
    DEF INPUT  PARAM lc-json-enviado AS LONGCHAR  NO-UNDO.
    DEF INPUT  PARAM pRowTipoIntegr  AS ROWID     NO-UNDO.
    DEF OUTPUT PARAM pResp           AS LOGICAL   NO-UNDO.
    DEF OUTPUT PARAM TABLE FOR RowErrors.

    DEF VAR oRequest     as IHttpRequest                  no-undo.
    DEF VAR oResponse    as IHttpResponse                 no-undo.
    DEF VAR oHttpClient  AS OpenEdge.Net.HTTP.IHttpClient NO-UNDO.
    DEF VAR oRequestBody as String                        no-undo.
    DEF VAR oJsonObject  AS JsonObject                    NO-UNDO.
    DEF VAR JsonString   AS LONGCHAR                      NO-UNDO.
    DEF VAR oJsonEntity  AS JsonObject                    NO-UNDO.
    DEF VAR iIndexErro   AS INTEGER                       NO-UNDO.    
    DEF VAR icount       AS INTEGER                       NO-UNDO.
    
    /* ------ Variˇveis ------ */
    DEF VAR httpUrl as character no-undo.
    DEF VAR cToken  AS CHARACTER NO-UNDO.
    DEF VAR client  AS COM-HANDLE.
    
    FIND FIRST es-api-param NO-LOCK WHERE ROWID(es-api-param) = pRowTipoIntegr NO-ERROR.
    IF NOT AVAIL es-api-param THEN DO:
       RUN piErro ("Tipo de Integraá∆o nao encontrada","").
       RETURN "NOK".
    END.

    RUN piGeraTokenApigee (OUTPUT ctoken).

    IF cToken = "" THEN DO:
        MESSAGE "Token nío encontrado" VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RUN piErro ("Token n∆o encontrado","").
        RETURN "NOK".
    END.
    ELSE DO:

        httpUrl = es-api-param.host-integr + ":" + string(es-api-param.porta-integr) + es-api-param.path-integr.

        CREATE "MSXML2.XMLHTTP" client.

        client:OPEN("post", httpUrl, FALSE).
        client:SetRequestHeader ("Content-Type", "application/json").
        client:SetRequestHeader ("Authorization","Bearer " + cToken).
        client:Send(lc-json-enviado).

        IF index(client:ResponseText,'"Status":false') > 0 THEN DO:
           IF LOOKUP("Description",client:ResponseText,'"') > 0
               THEN RUN piErro (ENTRY(LOOKUP("Description",client:ResponseText,'"') + 2,
                        client:ResponseText,
                        '"'),
                        ""
                        ).
         END.
         ASSIGN
            cResposta = client:ResponseText.
         RELEASE OBJECT client.
    END.

    RETURN cResposta.    

END PROCEDURE.


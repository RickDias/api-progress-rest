/*----------------------------------------------------------------------------------------------/
 Programa..: esint020ae.p
 Objetivo..: Interface Chamada B2E Fornecedores PJ Ariba
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

/*
MESSAGE PROGRAM-NAME(1)
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
 */
{include/i-prgvrs.i ESINT021ae 1.00.00.000} 

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
DEFINE VARIABLE cLongJson         AS LONGCHAR          NO-UNDO.

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

{esp/esint021ae.i}

DEFINE TEMP-TABLE RowErrors NO-UNDO
    FIELD ErrorSequence    AS INTEGER
    FIELD ErrorNumber      AS INTEGER
    FIELD ErrorDescription AS CHARACTER
    FIELD ErrorParameters  AS CHARACTER
    FIELD ErrorType        AS CHARACTER
    FIELD ErrorHelp        AS CHARACTER
    FIELD ErrorSubType     AS CHARACTER.

/*------------------------------ Main Begin ----------------------------*/
LOG-MANAGER:WRITE-MESSAGE("---Main Begin --") NO-ERROR.

ASSIGN c-erro = "".

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

    FIND FIRST api-import-for OF sfa-export EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL api-import-for THEN DO:
        /**********************************************************
         alterado ponto para corrigir erro que a tabela n∆o estava
         mais disponivel
        ***********************************************************/

        //MESSAGE "###4-lendo a api-import-for".
        //MESSAGE "###5-imprimindo info json" STRING(api-import-for.c-json).

        
        RUN piGravaTTFornecedor (OUTPUT c-json,
                                 OUTPUT c-erro).

        ASSIGN api-import-for.c-json = c-Json.
        
        /* ------------ Envia Objeto Json --------- */
        RUN piPostJsonObj /*IN h-esint002*/ 
                          (INPUT oJsonObjMain,
                           INPUT rowid(es-api-param),
                           OUTPUT lResp,
                           OUTPUT TABLE RowErrors,
                           OUTPUT c-retorno,
                           OUTPUT ojsonRet).
        /*-----------------------------------------*/
        /* ------ Grava conteudo do Json em variavel -----*/                
        RUN piGeraVarJson IN h-esint002 (INPUT ojsonRet,                
                                         OUTPUT cLongJson) NO-ERROR.           
        IF ERROR-STATUS:ERROR THEN DO:                                      
            ASSIGN c-erro = ERROR-STATUS:GET-MESSAGE(1).                    
            DELETE OBJECT h-esint002.                                       
            RETURN "NOK".                                                   
        END. 
       
        
                                                                            
        //ASSIGN sfa-export-cli.c-json = c-Json.                              
                                                                            
        /* ------------ Envia Objeto Json --------- */                      
        //RUN piPostJsonObj IN h-esint002 (INPUT oJsonObjMain,                
        //                                 INPUT rowid(es-api-param),            
        //                                 OUTPUT lResp,                         
        //                                 OUTPUT TABLE RowErrors,               
        //                                 OUTPUT c-retorno). 

        //MESSAGE "####-retorno " STRING(c-retorno).

        IF c-retorno <> "" THEN
            ASSIGN sfa-export.text-retorno = c-retorno.  


        IF TEMP-TABLE rowErrors:HAS-RECORDS THEN DO:                                                             
            FOR EACH rowErrors:                                                                                  
                ASSIGN c-erro = c-erro + string(rowerrors.ErrorNumber)  + " - " + rowerrors.ErrorDescription.    
                DELETE OBJECT h-esint002.                                                                        
                RETURN "NOK".                                                                                    
            END.                                                                                                 
        END. 
        ELSE
        DO:
            
            RUN piConvLongObj IN h-esint002 (INPUT cLongJson, OUTPUT ojsonRet).

            /*----------- Grava Json ---------- */
            RUN piGeraArqJson IN h-esint002 (INPUT ojsonRet, 
                                             INPUT es-api-param.dir-export,
                                             INPUT "import-for",
                                             OUTPUT c-arq-json).

            //MESSAGE "###-arquivo de log " c-arq-json.
            
            RUN piCast (INPUT cLongJson).
            RUN piAtualizaFornecedor.    
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

    FIND FIRST es-ariba-b2e-param EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL es-ariba-b2e-param AND (es-ariba-b2e-param.dt-ult-consulta = ?) THEN
        ASSIGN es-ariba-b2e-param.dt-ult-consulta = NOW.

    CREATE consulta-fornecedor.
    ASSIGN consulta-fornecedor.PollingMessage         = es-ariba-b2e-param.PollingMessage
           consulta-fornecedor.InboundServiceName     = "BusinessPartnerSUITEBulkReplicateRequest_In".

    IF AVAIL es-ariba-b2e-param THEN
        ASSIGN es-ariba-b2e-param.dt-ult-consulta = NOW.


    ASSIGN h-temp = BUFFER consulta-fornecedor:HANDLE.

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
    oJsonObjMain:ADD("Consulta_Fornecedor",oJsonArrayMain).

    
    
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
        //MESSAGE "Token n∆o encontrado!" .
        RUN piErro IN h-esint002 ("Token n∆o encontrado!","").
        RETURN "NOK".
    END.
    ELSE DO:

        ASSIGN httpUrl = es-api-param.host-integr + ":" + string(es-api-param.porta-integr) + es-api-param.path-integr.
        oJsonObject = CAST(oInputData, JsonObject). 
        JsonString = STRING(oJsonObject:getJsonText()).

/**/
        //JsonString:writeJson("C:/temp/jsonconsultafornecedor1.json").
/**/                


        ASSIGN oRequest = RequestBuilder:Post(httpUrl, oInputData)
                                        :ContentType('application/json')
                                        :AcceptJson()
                                        :AddHeader("Authorization":U, "Bearer ":U + cToken)                                
                                        :Request.
                
        oClient = ClientBuilder:Build():Client.
        oResponse = ResponseBuilder:Build():Response.
        oClient:Execute(oRequest,oResponse)  NO-ERROR .

        IF ERROR-STATUS:ERROR THEN DO:
            RUN piErro IN h-esint002 ("Ocorreram erros no envio do Json - " + STRING(ERROR-STATUS:GET-MESSAGE(1)), "" ).
            RETURN "NOK".            
        END.

        oJsonObject = NEW JsonObject(). 

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
            END.
            ELSE IF TYPE-OF(oResponse:Entity, JsonObject) THEN DO:
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
                COPY-LOB c-json TO sfa-export.clob-retorno.
                
            END.
            ELSE DO:
                JsonString = CAST(oResponse:Entity, String):Value.
            END.
            IF index(JsonString,'"Status":false') > 0 THEN DO:
                IF LOOKUP("Description",JsonString,'"') > 0 THEN 
                    RUN piErro IN h-esint002 (ENTRY(LOOKUP("Description",JsonString,'"') + 2,JsonString,'"'),"").
            END.
        END.
    END.


END PROCEDURE.


PROCEDURE piCast:
   //DEF INPUT PARAM oJson AS JsonObject NO-UNDO.
    DEFINE INPUT PARAM pLongJson AS LONGCHAR NO-UNDO.

    DEFINE VARIABLE oJson AS JsonObject  NO-UNDO.

    ASSIGN pLongJson = REPLACE(pLongJson,"!UTF-8!","").

   
    myParser   = NEW ObjectModelParser().
    oJson      = CAST(myParser:Parse(cLongJson),JsonObject).
    
   
   IF oJson:Has("Cadastro_Fornecedor") 
   THEN DO:  
       oJsonArraySec = oJson:GetJsonArray("Cadastro_Fornecedor").
       DO iCountSec = 1 TO oJsonArraySec:LENGTH:
           oJsonObjectSec =  oJsonArraySec:GetJsonObject(iCountSec).          

           CREATE cadastro-fornecedor.
           IF oJsonObjectSec:Has("Corporate-Name")                
           THEN DO:
               IF oJsonObjectSec:GetType("Corporate-Name") = 1
               THEN cadastro-fornecedor.Corporate-Name = oJsonObjectSec:GetCharacter("Corporate-Name").               
               IF oJsonObjectSec:GetType("Corporate-Name") = 2
               THEN cadastro-fornecedor.Corporate-Name = STRING(oJsonObjectSec:GetInt64("Corporate-Name")).               
           END.
           IF oJsonObjectSec:Has("Trading-name")                
           THEN DO:
               IF oJsonObjectSec:GetType("Trading-name") = 1
               THEN cadastro-fornecedor.Trading-name = oJsonObjectSec:GetCharacter("Trading-name").               
               IF oJsonObjectSec:GetType("Trading-name") = 2
               THEN cadastro-fornecedor.Trading-name = STRING(oJsonObjectSec:GetInt64("Trading-name")).               
           END.
           IF oJsonObjectSec:Has("Number")                
           THEN DO:
               IF oJsonObjectSec:GetType("Number") = 1
               THEN cadastro-fornecedor.Number = oJsonObjectSec:GetCharacter("Number").               
               IF oJsonObjectSec:GetType("Number") = 2
               THEN cadastro-fornecedor.Number = STRING(oJsonObjectSec:GetInt64("Number")).               
           END.
           IF oJsonObjectSec:Has("CNPJ")                
           THEN DO:
               IF oJsonObjectSec:GetType("CNPJ") = 1
               THEN cadastro-fornecedor.CNPJ = oJsonObjectSec:GetCharacter("CNPJ").               
               IF oJsonObjectSec:GetType("CNPJ") = 2
               THEN cadastro-fornecedor.CNPJ = STRING(oJsonObjectSec:GetInt64("CNPJ")).               
           END.
           IF oJsonObjectSec:Has("CPF")                
           THEN DO:
               IF oJsonObjectSec:GetType("CPF") = 1
               THEN cadastro-fornecedor.CPF = oJsonObjectSec:GetCharacter("CPF").               
               IF oJsonObjectSec:GetType("CPF") = 2
               THEN cadastro-fornecedor.CPF = STRING(oJsonObjectSec:GetInt64("CPF")).               
           END.
           IF oJsonObjectSec:Has("PIS-Number")                
           THEN DO:
               IF oJsonObjectSec:GetType("PIS-Number") = 1
               THEN cadastro-fornecedor.PIS-Number = oJsonObjectSec:GetCharacter("PIS-Number").               
               IF oJsonObjectSec:GetType("PIS-Number") = 2
               THEN cadastro-fornecedor.PIS-Number = STRING(oJsonObjectSec:GetInt64("PIS-Number")).               
           END.
           IF oJsonObjectSec:Has("NIS-Number")                
           THEN DO:
               IF oJsonObjectSec:GetType("NIS-Number") = 1
               THEN cadastro-fornecedor.NIS-Number = oJsonObjectSec:GetCharacter("NIS-Number").               
               IF oJsonObjectSec:GetType("NIS-Number") = 2
               THEN cadastro-fornecedor.NIS-Number = STRING(oJsonObjectSec:GetInt64("NIS-Number")).               
           END.
           IF oJsonObjectSec:Has("IE")                
           THEN DO:
               IF oJsonObjectSec:GetType("IE") = 1
               THEN cadastro-fornecedor.IE = oJsonObjectSec:GetCharacter("IE").               
               IF oJsonObjectSec:GetType("IE") = 2
               THEN cadastro-fornecedor.IE = STRING(oJsonObjectSec:GetInt64("IE")).               
           END.
           IF oJsonObjectSec:Has("State")                
           THEN DO:
               IF oJsonObjectSec:GetType("State") = 1
               THEN cadastro-fornecedor.State = oJsonObjectSec:GetCharacter("State").               
               IF oJsonObjectSec:GetType("State") = 2
               THEN cadastro-fornecedor.State = STRING(oJsonObjectSec:GetInt64("State")).               
           END.
           IF oJsonObjectSec:Has("Street")                
           THEN DO:
               IF oJsonObjectSec:GetType("Street") = 1
               THEN cadastro-fornecedor.Street = oJsonObjectSec:GetCharacter("Street").               
               IF oJsonObjectSec:GetType("Street") = 2
               THEN cadastro-fornecedor.Street = STRING(oJsonObjectSec:GetInt64("Street")).               
           END.
           IF oJsonObjectSec:Has("Complement")                
           THEN DO:
               IF oJsonObjectSec:GetType("Complement") = 1
               THEN cadastro-fornecedor.Complement = oJsonObjectSec:GetCharacter("Complement").               
               IF oJsonObjectSec:GetType("Complement") = 2
               THEN cadastro-fornecedor.Complement = STRING(oJsonObjectSec:GetInt64("Complement")).               
           END.
           IF oJsonObjectSec:Has("District")                
           THEN DO:
               IF oJsonObjectSec:GetType("District") = 1
               THEN cadastro-fornecedor.District = oJsonObjectSec:GetCharacter("District").               
               IF oJsonObjectSec:GetType("District") = 2
               THEN cadastro-fornecedor.District = STRING(oJsonObjectSec:GetInt64("District")).               
           END.
           IF oJsonObjectSec:Has("Zip-Code")                
           THEN DO:
               IF oJsonObjectSec:GetType("Zip-Code") = 1
               THEN cadastro-fornecedor.Zip-Code = oJsonObjectSec:GetCharacter("Zip-Code").               
               IF oJsonObjectSec:GetType("Zip-Code") = 2
               THEN cadastro-fornecedor.Zip-Code = STRING(oJsonObjectSec:GetInt64("Zip-Code")).               
           END.
           IF oJsonObjectSec:Has("Country")                
           THEN DO:
               IF oJsonObjectSec:GetType("Country") = 1
               THEN cadastro-fornecedor.Country = oJsonObjectSec:GetCharacter("Country").               
               IF oJsonObjectSec:GetType("Country") = 2
               THEN cadastro-fornecedor.Country = STRING(oJsonObjectSec:GetInt64("Country")).               
           END.
           IF oJsonObjectSec:Has("Pais")                
           THEN DO:
               IF oJsonObjectSec:GetType("Pais") = 1
               THEN cadastro-fornecedor.Pais = oJsonObjectSec:GetCharacter("Pais").               
               IF oJsonObjectSec:GetType("Pais") = 2
               THEN cadastro-fornecedor.Pais = STRING(oJsonObjectSec:GetInt64("Pais")).               
           END.
           IF oJsonObjectSec:Has("CNAE-principal")                
           THEN DO:
               IF oJsonObjectSec:GetType("CNAE-principal") = 1
               THEN cadastro-fornecedor.CNAE-principal = oJsonObjectSec:GetCharacter("CNAE-principal").               
               IF oJsonObjectSec:GetType("CNAE-principal") = 2
               THEN cadastro-fornecedor.CNAE-principal = STRING(oJsonObjectSec:GetInt64("CNAE-principal")).               
           END.
           IF oJsonObjectSec:Has("E-mail")                
           THEN DO:
               IF oJsonObjectSec:GetType("E-mail") = 1
               THEN cadastro-fornecedor.E-mail = oJsonObjectSec:GetCharacter("E-mail").               
               IF oJsonObjectSec:GetType("E-mail") = 2
               THEN cadastro-fornecedor.E-mail = STRING(oJsonObjectSec:GetInt64("E-mail")).               
           END.
           IF oJsonObjectSec:Has("Simples-Nacional")                
           THEN DO:
               IF oJsonObjectSec:GetType("Simples-Nacional") = 1
               THEN cadastro-fornecedor.Simples-Nacional = oJsonObjectSec:GetCharacter("Simples-Nacional").               
               IF oJsonObjectSec:GetType("Simples-Nacional") = 2
               THEN cadastro-fornecedor.Simples-Nacional = STRING(oJsonObjectSec:GetInt64("Simples-Nacional")).               
           END.
           IF oJsonObjectSec:Has("Municipality")                
           THEN DO:
               IF oJsonObjectSec:GetType("Municipality") = 1
               THEN cadastro-fornecedor.Municipality = oJsonObjectSec:GetCharacter("Municipality").               
               IF oJsonObjectSec:GetType("Municipality") = 2
               THEN cadastro-fornecedor.Municipality = STRING(oJsonObjectSec:GetInt64("Municipality")).               
           END.
           IF oJsonObjectSec:Has("Nome-Responsavel")                
           THEN DO:
               IF oJsonObjectSec:GetType("Nome-Responsavel") = 1
               THEN cadastro-fornecedor.Nome-Responsavel = oJsonObjectSec:GetCharacter("Nome-Responsavel").               
               IF oJsonObjectSec:GetType("Nome-Responsavel") = 2
               THEN cadastro-fornecedor.Nome-Responsavel = STRING(oJsonObjectSec:GetInt64("Nome-Responsavel")).               
           END.
           IF oJsonObjectSec:Has("Date-Birth")                
           THEN DO:
               IF oJsonObjectSec:GetType("Date-Birth") = 1
               THEN cadastro-fornecedor.Date-Birth = oJsonObjectSec:GetCharacter("Date-Birth").               
               IF oJsonObjectSec:GetType("Date-Birth") = 2
               THEN cadastro-fornecedor.Date-Birth = STRING(oJsonObjectSec:GetInt64("Date-Birth")).               
           END.
           IF oJsonObjectSec:Has("Codigo-Pais")                
           THEN DO:
               IF oJsonObjectSec:GetType("Codigo-Pais") = 1
               THEN cadastro-fornecedor.Codigo-Pais = oJsonObjectSec:GetCharacter("Codigo-Pais").               
               IF oJsonObjectSec:GetType("Codigo-Pais") = 2
               THEN cadastro-fornecedor.Codigo-Pais = STRING(oJsonObjectSec:GetInt64("Codigo-Pais")).               
           END.
           IF oJsonObjectSec:Has("Codigo-area")                
           THEN DO:
               IF oJsonObjectSec:GetType("Codigo-area") = 1
               THEN cadastro-fornecedor.Codigo-area = oJsonObjectSec:GetCharacter("Codigo-area").               
               IF oJsonObjectSec:GetType("Codigo-area") = 2
               THEN cadastro-fornecedor.Codigo-area = STRING(oJsonObjectSec:GetInt64("Codigo-area")).               
           END.
           IF oJsonObjectSec:Has("Numero-Telefone")                
           THEN DO:
               IF oJsonObjectSec:GetType("Numero-Telefone") = 1
               THEN cadastro-fornecedor.Numero-Telefone = oJsonObjectSec:GetCharacter("Numero-Telefone").               
               IF oJsonObjectSec:GetType("Numero-Telefone") = 2
               THEN cadastro-fornecedor.Numero-Telefone = STRING(oJsonObjectSec:GetInt64("Numero-Telefone")).               
           END.
           IF oJsonObjectSec:Has("Banco")                
           THEN DO:
               IF oJsonObjectSec:GetType("Banco") = 1
               THEN cadastro-fornecedor.Banco = oJsonObjectSec:GetCharacter("Banco").               
               IF oJsonObjectSec:GetType("Banco") = 2
               THEN cadastro-fornecedor.Banco = STRING(oJsonObjectSec:GetInt64("Banco")).               
           END.
           IF oJsonObjectSec:Has("Agencia")                
           THEN DO:
               IF oJsonObjectSec:GetType("Agencia") = 1
               THEN cadastro-fornecedor.Agencia = oJsonObjectSec:GetCharacter("Agencia").               
               IF oJsonObjectSec:GetType("Agencia") = 2
               THEN cadastro-fornecedor.Agencia = STRING(oJsonObjectSec:GetInt64("Agencia")).               
           END.
           IF oJsonObjectSec:Has("Dig-Agencia")                
           THEN DO:
               IF oJsonObjectSec:GetType("Dig-Agencia") = 1
               THEN cadastro-fornecedor.Dig-Agencia = oJsonObjectSec:GetCharacter("Dig-Agencia").               
               IF oJsonObjectSec:GetType("Dig-Agencia") = 2
               THEN cadastro-fornecedor.Dig-Agencia = STRING(oJsonObjectSec:GetInt64("Dig-Agencia")).               
           END.
           IF oJsonObjectSec:Has("Conta-corrente")                
           THEN DO:
               IF oJsonObjectSec:GetType("Conta-corrente") = 1
               THEN cadastro-fornecedor.Conta-corrente = oJsonObjectSec:GetCharacter("Conta-corrente").               
               IF oJsonObjectSec:GetType("Conta-corrente") = 2
               THEN cadastro-fornecedor.Conta-corrente = STRING(oJsonObjectSec:GetInteger  ("Conta-corrente")).               
           END.
           IF oJsonObjectSec:Has("Dig-conta-corrente")                
           THEN DO:
               IF oJsonObjectSec:GetType("Dig-conta-corrente") = 1
               THEN cadastro-fornecedor.Dig-conta-corrente = oJsonObjectSec:GetCharacter("Dig-conta-corrente").               
               IF oJsonObjectSec:GetType("Dig-conta-corrente") = 2
               THEN cadastro-fornecedor.Dig-conta-corrente = STRING(oJsonObjectSec:GetInteger  ("Dig-conta-corrente")).               
           END.
           IF oJsonObjectSec:Has("DeletedIndicator")                
           THEN DO:
               IF oJsonObjectSec:GetType("DeletedIndicator") = 1
               THEN cadastro-fornecedor.DeletedIndicator = oJsonObjectSec:GetCharacter("DeletedIndicator").               
               IF oJsonObjectSec:GetType("DeletedIndicator") = 2
               THEN cadastro-fornecedor.DeletedIndicator = STRING(oJsonObjectSec:GetInt64("DeletedIndicator")).               
           END.
           IF oJsonObjectSec:Has("BlockedIndicator")                
           THEN DO:
               IF oJsonObjectSec:GetType("BlockedIndicator") = 1
               THEN cadastro-fornecedor.BlockedIndicator = oJsonObjectSec:GetCharacter("BlockedIndicator").               
               IF oJsonObjectSec:GetType("BlockedIndicator") = 2
               THEN cadastro-fornecedor.BlockedIndicator = STRING(oJsonObjectSec:GetInt64("BlockedIndicator")).               
           END.
           IF oJsonObjectSec:Has("BuildingID")                
           THEN DO:
               IF oJsonObjectSec:GetType("BuildingID") = 1
               THEN cadastro-fornecedor.BuildingID = oJsonObjectSec:GetCharacter("BuildingID").               
               IF oJsonObjectSec:GetType("BuildingID") = 2
               THEN cadastro-fornecedor.BuildingID = STRING(oJsonObjectSec:GetInt64("BuildingID")).               
           END.
           IF oJsonObjectSec:Has("POBoxDeviatingCityName")                
           THEN DO:
               IF oJsonObjectSec:GetType("POBoxDeviatingCityName") = 1
               THEN cadastro-fornecedor.POBoxDeviatingCityName = oJsonObjectSec:GetCharacter("POBoxDeviatingCityName").               
               IF oJsonObjectSec:GetType("POBoxDeviatingCityName") = 2
               THEN cadastro-fornecedor.POBoxDeviatingCityName = STRING(oJsonObjectSec:GetInt64("POBoxDeviatingCityName")).               
           END.

           IF oJsonObjectSec:Has("ID")                
           THEN DO:
               IF oJsonObjectSec:GetType("ID") = 1
               THEN cadastro-fornecedor.ID = oJsonObjectSec:GetCharacter("ID").               
               IF oJsonObjectSec:GetType("ID") = 2
               THEN cadastro-fornecedor.ID = STRING(oJsonObjectSec:GetInt64("ID")).               
           END.
           IF oJsonObjectSec:Has("UUID")                
           THEN DO:
               IF oJsonObjectSec:GetType("UUID") = 1
               THEN cadastro-fornecedor.UUID = oJsonObjectSec:GetCharacter("UUID").               
               IF oJsonObjectSec:GetType("UUID") = 2
               THEN cadastro-fornecedor.UUID = STRING(oJsonObjectSec:GetInt64("UUID")).               
           END.
           IF oJsonObjectSec:Has("ID_1")                
           THEN DO:
               IF oJsonObjectSec:GetType("ID_1") = 1
               THEN cadastro-fornecedor.ID_1 = oJsonObjectSec:GetCharacter("ID_1").               
               IF oJsonObjectSec:GetType("ID_1") = 2
               THEN cadastro-fornecedor.ID_1 = STRING(oJsonObjectSec:GetInt64("ID_1")).               
           END.
           IF oJsonObjectSec:Has("UUID_1")                
           THEN DO:
               IF oJsonObjectSec:GetType("UUID_1") = 1
               THEN cadastro-fornecedor.UUID_1 = oJsonObjectSec:GetCharacter("UUID_1").               
               IF oJsonObjectSec:GetType("UUID_1") = 2
               THEN cadastro-fornecedor.UUID_1 = STRING(oJsonObjectSec:GetInt64("UUID_1")).               
           END.
           IF oJsonObjectSec:Has("UUID_2")                
           THEN DO:
               IF oJsonObjectSec:GetType("UUID_2") = 1
               THEN cadastro-fornecedor.UUID_2 = oJsonObjectSec:GetCharacter("UUID_2").               
               IF oJsonObjectSec:GetType("UUID_2") = 2
               THEN cadastro-fornecedor.UUID_2 = STRING(oJsonObjectSec:GetInt64("UUID_2")).               
           END.
           IF oJsonObjectSec:Has("ReceiverUUID")                
           THEN DO:
               IF oJsonObjectSec:GetType("ReceiverUUID") = 1
               THEN cadastro-fornecedor.ReceiverUUID = oJsonObjectSec:GetCharacter("ReceiverUUID").               
               IF oJsonObjectSec:GetType("ReceiverUUID") = 2
               THEN cadastro-fornecedor.ReceiverUUID = STRING(oJsonObjectSec:GetInt64("ReceiverUUID")).               
           END.
           IF oJsonObjectSec:Has("ReceiverInternalID")                
           THEN DO:
               IF oJsonObjectSec:GetType("ReceiverInternalID") = 1
               THEN cadastro-fornecedor.ReceiverInternalID = oJsonObjectSec:GetCharacter("ReceiverInternalID").               
               IF oJsonObjectSec:GetType("ReceiverInternalID") = 2
               THEN cadastro-fornecedor.ReceiverInternalID = STRING(oJsonObjectSec:GetInt64("ReceiverInternalID")).               
           END.
           IF oJsonObjectSec:Has("Categorias-de-Fornecimento")                
           THEN DO:
               IF oJsonObjectSec:GetType("Categorias-de-Fornecimento") = 1
               THEN cadastro-fornecedor.TaxGroupCode = oJsonObjectSec:GetCharacter("Categorias-de-Fornecimento").               
               IF oJsonObjectSec:GetType("Categorias-de-Fornecimento") = 2
               THEN cadastro-fornecedor.TaxGroupCode = STRING(oJsonObjectSec:GetInt64("Categorias-de-Fornecimento")).               
           END.
           IF oJsonObjectSec:Has("PollingMessage")
           THEN DO:
               IF oJsonObjectSec:GetType("PollingMessage") = 1
               THEN cadastro-fornecedor.PollingMessage = oJsonObjectSec:GetCharacter("PollingMessage").               
               IF oJsonObjectSec:GetType("PollingMessage") = 2
               THEN cadastro-fornecedor.PollingMessage = STRING(oJsonObjectSec:GetInt64("PollingMessage")).               
           END.

       END.
   END.
END.

PROCEDURE piAtualizaFornecedor:
   DEF BUFFER bf-fornecedor-ariba FOR es-fornecedor-ariba.

   DEF VAR lSendB2E          AS l NO-UNDO.
   DEF VAR lSendBOFornecedor AS l NO-UNDO.

   IF NOT AVAIL es-ariba-b2e-param THEN                       
       FIND FIRST es-ariba-b2e-param EXCLUSIVE-LOCK NO-ERROR. 


   FOR EACH cadastro-fornecedor NO-LOCK:

//       IF cadastro-fornecedor.CNPJ + cadastro-fornecedor.CPF + cadastro-fornecedor.IE = ""
//       THEN NEXT.

       //MESSAGE "####-Tag PollingMessage: " string(cadastro-fornecedor.PollingMessage).

       IF cadastro-fornecedor.PollingMessage = "" THEN NEXT.

       FIND FIRST es-fornecedor-ariba EXCLUSIVE-LOCK
            WHERE es-fornecedor-ariba.Number      = cadastro-fornecedor.Number
              AND es-fornecedor-ariba.dt-consulta = daDtConsulta
            NO-ERROR.

       IF NOT AVAIL es-fornecedor-ariba THEN 
       DO:
           //MESSAGE "#### fornecedor n∆o cadastrado na es-fornecedor-ariba".
          CREATE es-fornecedor-ariba.
          ASSIGN es-fornecedor-ariba.Number             = cadastro-fornecedor.Number            
                 es-fornecedor-ariba.dt-consulta        = daDtConsulta.
       END.

//       IF es-fornecedor-ariba.ind-inativado = 9
//       THEN NEXT.

       ASSIGN es-fornecedor-ariba.Corporate-Name         = cadastro-fornecedor.Corporate-Name    
              es-fornecedor-ariba.Trading-name           = cadastro-fornecedor.Trading-name      
              es-fornecedor-ariba.CNPJ                   = cadastro-fornecedor.CNPJ              
              es-fornecedor-ariba.CPF                    = cadastro-fornecedor.CPF               
              es-fornecedor-ariba.PIS-Number             = cadastro-fornecedor.PIS-Number
              es-fornecedor-ariba.NIS-Number             = cadastro-fornecedor.NIS-Number
              es-fornecedor-ariba.IE                     = cadastro-fornecedor.IE                
              es-fornecedor-ariba.State                  = cadastro-fornecedor.State             
              es-fornecedor-ariba.Street                 = cadastro-fornecedor.Street            
              es-fornecedor-ariba.Complement             = cadastro-fornecedor.Complement        
              es-fornecedor-ariba.District               = cadastro-fornecedor.District          
              es-fornecedor-ariba.Zip-Code               = cadastro-fornecedor.Zip-Code          
              es-fornecedor-ariba.Country                = cadastro-fornecedor.Country           
              es-fornecedor-ariba.Pais                   = cadastro-fornecedor.Pais              
              es-fornecedor-ariba.CNAE-principal         = cadastro-fornecedor.CNAE-principal
              es-fornecedor-ariba.E-mail                 = cadastro-fornecedor.E-mail            
              es-fornecedor-ariba.Municipality           = cadastro-fornecedor.Municipality      
              es-fornecedor-ariba.Nome-Responsavel       = cadastro-fornecedor.Nome-Responsavel  
              es-fornecedor-ariba.Date-Birth             = DATE(cadastro-fornecedor.Date-Birth)
              es-fornecedor-ariba.Codigo-Pais            = cadastro-fornecedor.Codigo-Pais       
              es-fornecedor-ariba.Codigo-area            = cadastro-fornecedor.Codigo-area
              es-fornecedor-ariba.Numero-Telefone        = cadastro-fornecedor.Numero-Telefone   
              es-fornecedor-ariba.Banco                  = cadastro-fornecedor.Banco
              es-fornecedor-ariba.Agencia                = cadastro-fornecedor.Agencia
              es-fornecedor-ariba.Dig-Agencia            = cadastro-fornecedor.Dig-Agencia
              es-fornecedor-ariba.Conta-corrente         = cadastro-fornecedor.Conta-corrente
              es-fornecedor-ariba.Dig-conta-corrente     = cadastro-fornecedor.Dig-conta-corrente
              es-fornecedor-ariba.ID                     = cadastro-fornecedor.ID                     
              es-fornecedor-ariba.ID_1                   = cadastro-fornecedor.ID_1                   
              es-fornecedor-ariba.UUID                   = cadastro-fornecedor.UUID                   
              es-fornecedor-ariba.UUID_1                 = cadastro-fornecedor.UUID_1                 
              es-fornecedor-ariba.UUID_2                 = cadastro-fornecedor.UUID_2                 
              es-fornecedor-ariba.ReceiverUUID           = cadastro-fornecedor.ReceiverUUID           
              es-fornecedor-ariba.ReceiverInternalID     = cadastro-fornecedor.ReceiverInternalID     
              es-fornecedor-ariba.DeletedIndicator       = cadastro-fornecedor.DeletedIndicator       
              es-fornecedor-ariba.BlockedIndicator       = cadastro-fornecedor.BlockedIndicator       
              es-fornecedor-ariba.BuildingID             = cadastro-fornecedor.BuildingID             
              es-fornecedor-ariba.POBoxDeviatingCityName = cadastro-fornecedor.POBoxDeviatingCityName 
          //es-fornecedor-ariba.TaxGroupCode           = cadastro-fornecedor.TaxGroupCode           
          //es-fornecedor-ariba.PollingMessage         = cadastro-fornecedor.PollingMessage
          .
       
       
       ASSIGN es-ariba-b2e-param.PollingMessage = MAX(es-ariba-b2e-param.PollingMessage,INT64(cadastro-fornecedor.PollingMessage)).

      // MESSAGE 
      //     "es-ariba-b2e-param.PollingMessage" es-ariba-b2e-param.PollingMessage SKIP
      //     VIEW-AS ALERT-BOX INFO BUTTONS OK.

       IF cadastro-fornecedor.Simples-Nacional = "yes" THEN 
           ASSIGN es-fornecedor-ariba.Simples-Nacional = YES.
       ELSE ASSIGN es-fornecedor-ariba.Simples-Nacional = NO.

       FIND  LAST bf-fornecedor-ariba NO-LOCK
            WHERE bf-fornecedor-ariba.number        = es-fornecedor-ariba.number
              AND bf-fornecedor-ariba.dt-consulta   < daDtConsulta
              AND bf-fornecedor-ariba.ind-inativado < 9
            NO-ERROR.
       IF NOT AVAIL bf-fornecedor-ariba
       THEN DO:
          IF  es-fornecedor-ariba.cnpj + es-fornecedor-ariba.cpf > ""
          THEN ASSIGN
             lSendB2E          = YES.
          ELSE ASSIGN
             lSendBOFornecedor = YES.
       END.
       ELSE DO:
          FIND FIRST emitente NO-LOCK
               WHERE emitente.cod-emitente = bf-fornecedor-ariba.cod-emitente
               NO-ERROR.
          IF AVAIL emitente
          THEN DO:
             IF emitente.nome-emit  <> es-fornecedor-ariba.Corporate-Name
             OR emitente.estado     <> es-fornecedor-ariba.state
             OR emitente.pais       <> es-fornecedor-ariba.country
             THEN ASSIGN
                lSendB2E          = YES.
             ELSE ASSIGN
                lSendBOFornecedor = YES.
             ASSIGN
                es-fornecedor-ariba.cod-emitente = bf-fornecedor-ariba.cod-emitente.

             IF es-fornecedor-ariba.BlockedIndicator = "false"
             THEN ASSIGN
                es-fornecedor-ariba.ind-inativado = 1.
             ELSE ASSIGN
                es-fornecedor-ariba.ind-inativado = 2.
                
          END.
       END.


       //IF lSendB2E THEN
       //
       //MESSAGE "####-status de consultas" SKIP
       //        "lSendB2E" lSendB2E SKIP
       //        "lSendBOFornecedor" lSendBOFornecedor
       //    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
       //
       /* IF YES // es-fornecedor-ariba.cpf = "571.279.888-38"                                                */
       /*    //es-fornecedor-ariba.corporate-name = "TESTE INT34HK EST"                                       */
       /* THEN DO:                                                                                            */
       /*                                                                                                     */
      /*      MESSAGE                                                                                            */
      /*          "***** "                                                                                       */
      /*          "es-fornecedor-ariba.cpf" es-fornecedor-ariba.cpf                                SKIP          */
      /*          "es-fornecedor-ariba.corporate-name" es-fornecedor-ariba.corporate-name          SKIP          */
      /*          "lSendB2E"  lSendB2E                                                             SKIP          */
      /*          "es-fornecedor-ariba.enviado-b2e" es-fornecedor-ariba.enviado-b2e                SKIP(1)       */
      /*          "lSendBOFornecedor" lSendBOFornecedor                                            SKIP          */
      /*          "es-fornecedor-ariba.ind-atualizado-ems" es-fornecedor-ariba.ind-atualizado-ems  SKIP          */
      /*          "es-fornecedor-ariba.ind-inativado" es-fornecedor-ariba.ind-inativado            SKIP          */
      /*          VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                             */
       /*                                                                                                     */
       /*                                                                                                     */
       IF  lSendB2E                               =  YES
       AND es-fornecedor-ariba.enviado-b2e        =  NO
       THEN RUN piSendB2E. //RUN piBOFornecedor
   
       IF   lSendBOFornecedor                      =  YES
       AND  lSendB2E                               =  NO
       AND (es-fornecedor-ariba.ind-atualizado-ems <> 2
       OR   es-fornecedor-ariba.ind-inativado      >  0)
       THEN RUN piBOFornecedor.
   END. 
   IF AVAIL es-fornecedor-ariba THEN RELEASE es-fornecedor-ariba.
   IF AVAIL es-ariba-b2e-param  THEN RELEASE es-ariba-b2e-param. 

END.


PROCEDURE piSendB2E:
   {esp/esint021ae.i1 &Tabela=es-fornecedor-ariba}
END.

PROCEDURE piBOFornecedor:
   RUN esp/esint020aif.p ("",
                          ROWID(es-fornecedor-ariba),
                          OUTPUT c-erro).
END.


{esp/esint001rp.i}

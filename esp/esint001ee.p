/*----------------------------------------------------------------------------------------------/
 Programa..: esint001e.p
 Objetivo..: Interface Integra‡Æo An lise de Cr‚dito SFA
 Data......: 28/02/2019
 Autor.....: Rog‚rio Dias
 VersÆo....: 1.000.000
-----------------------------------------------------------------------------------------------*/


/* ------ Defini‡Æo das classes de objetos ------ */
using Progress.Json.OBJECTModel.JsonOBJECT.
using Progress.Json.OBJECTModel.JsonArray.

{include/i-prgvrs.i esint001EE 1.00.00.000} 

/* ------- Defini‡Æo de Parƒmetros ----- */
DEFINE INPUT  PARAMETER r-table AS ROWID     NO-UNDO.
DEFINE OUTPUT PARAMETER c-erro  AS CHARACTER NO-UNDO.


/* ------- Defini‡Æo de Vari veis ----- */
DEFINE VARIABLE oJsonObjMain      AS JsonObject NO-UNDO.
DEFINE VARIABLE oJsonArrayMain    AS JsonArray  NO-UNDO.
DEFINE VARIABLE oJsonObjIni       AS jsonObject NO-UNDO.
DEFINE VARIABLE ojsonArrayIni     AS JsonArray  NO-UNDO.
DEFINE VARIABLE ojsonObjAux       AS JsonObject NO-UNDO.
DEFINE VARIABLE ojsonArrayAux     AS JsonArray  NO-UNDO.

DEFINE VARIABLE h-temp              AS HANDLE     NO-UNDO.
DEFINE VARIABLE h-esint002          AS HANDLE     NO-UNDO.
DEFINE VARIABLE c-json              AS LONGCHAR   NO-UNDO.
DEFINE VARIABLE lEnviou             AS LOGICAL    NO-UNDO.
DEFINE VARIABLE c-arq-json          AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lresp               AS LOGICAL    NO-UNDO.
DEFINE VARIABLE c-retorno           AS LONGCHAR   NO-UNDO.

DEF BUFFER pais FOR mgcad.pais.

/* ------- Defini‡Æo de Temp-Tables ------ */
{esp\esint001e.i}

DEFINE TEMP-TABLE RowErrors NO-UNDO
    FIELD ErrorSequence    AS INTEGER
    FIELD ErrorNumber      AS INTEGER
    FIELD ErrorDescription AS CHARACTER
    FIELD ErrorParameters  AS CHARACTER
    FIELD ErrorType        AS CHARACTER
    FIELD ErrorHelp        AS CHARACTER
    FIELD ErrorSubType     AS CHARACTER.

/*------------------------------ Main Begin ----------------------------*/

ASSIGN c-erro = "".

/* ---- Chama o programa persistent ----- */
RUN esp\esint002.p PERSISTENT SET h-esint002 NO-ERROR.
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


FIND FIRST sfa-export NO-LOCK WHERE ROWID(sfa-export) = r-table NO-ERROR.
IF AVAIL sfa-export THEN DO:

    FIND FIRST sfa-export-cred OF sfa-export NO-ERROR.
    IF AVAIL sfa-export-cred THEN DO:

        /*------------------------------------------ Nota Fiscal -------------------------------------------*/
        RUN piGravaTTCred (OUTPUT h-temp,
                           OUTPUT c-erro).
        IF valid-handle(h-temp) THEN DO:

            oJsonObjMain = NEW JsonObject().

            RUN piCriaObj IN h-esint002 (INPUT h-temp,
                                         OUTPUT ojsonObjAux,
                                         OUTPUT ojsonArrayAux,
                                         INPUT YES) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN DO:
                ASSIGN c-erro = ERROR-STATUS:GET-MESSAGE(1).
                DELETE OBJECT h-temp.
                RETURN "NOK".
            END.            
            
            oJsonObjMain:ADD("req",ojsonArrayAux).

            DELETE OBJECT h-temp.
        END.

        
        FIND FIRST es-api-param WHERE es-api-param.ind-tipo-trans = sfa-export.ind-tipo-trans
                                  AND es-api-param.cd-tipo-integr = sfa-export.cd-tipo-integr NO-LOCK NO-ERROR.                            

        /*----- Grava conteudo do Json em variavel -----*/
        RUN piGeraVarJson IN h-esint002 (INPUT oJsonObjMain,
                                         OUTPUT c-Json) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:
            ASSIGN c-erro = ERROR-STATUS:GET-MESSAGE(1).
            DELETE OBJECT h-esint002.
            RETURN "NOK".
        END.

        ASSIGN sfa-export-cred.c-json = c-Json.

        /* ------------ Envia Objeto Json --------- */
         RUN piPostJsonObj IN h-esint002 (INPUT oJsonObjMain,
                                          INPUT rowid(es-api-param),
                                          OUTPUT lResp,
                                          OUTPUT TABLE RowErrors,
                                          OUTPUT c-retorno).
         IF TEMP-TABLE rowErrors:HAS-RECORDS THEN DO:
             FOR EACH rowErrors:
                 ASSIGN c-erro = c-erro + string(rowerrors.ErrorNumber)  + " - " + rowerrors.ErrorDescription.
                 DELETE OBJECT h-esint002.
                 RETURN "NOK".
             END.
         END.        
    END.
    ELSE do: 
        ASSIGN c-erro = "Registro tabela de Cr‚dito do Cliente nÆo localizado".
        RETURN "NOK".
    END.
END.



IF VALID-HANDLE(h-esint002) THEN
    DELETE OBJECT h-esint002.



/* -------------------------------------------------------------- Procedures ------------------------------------------------*/

PROCEDURE piGravaTTCred:

    DEFINE OUTPUT PARAMETER pTemp AS HANDLE    NO-UNDO.
    DEFINE OUTPUT PARAMETER pErro AS CHARACTER NO-UNDO.

    DEF VAR i AS INTEGER NO-UNDO.

    FOR EACH sfa-aval-credito NO-LOCK WHERE sfa-aval-credito.dt-atualizacao = date(sfa-export-cred.data-movto): 
        CREATE tt_credito_cliente.
        BUFFER-COPY sfa-aval-credito TO tt_credito_cliente.

        i = i + 1.
    END.
    
    IF TEMP-TABLE tt_credito_cliente:HAS-RECORDS THEN
        ASSIGN pTemp = BUFFER tt_credito_cliente:HANDLE.

END PROCEDURE.


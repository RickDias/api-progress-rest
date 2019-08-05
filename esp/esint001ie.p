/*----------------------------------------------------------------------------------------------/
 Programa..: esint001ie.p
 Objetivo..: Interface Integra‡Æo Lead Time SFA
 Data......: 21/05/2019
 Autor.....: Rog‚rio Dias
 VersÆo....: 1.000.000
-----------------------------------------------------------------------------------------------*/


/* ------ Defini‡Æo das classes de objetos ------ */
using Progress.Json.OBJECTModel.JsonOBJECT.
using Progress.Json.OBJECTModel.JsonArray.

{include/i-prgvrs.i esint001ie 1.00.00.000} 

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

DEFINE VARIABLE h-temp            AS HANDLE     NO-UNDO.
DEFINE VARIABLE h-esint002        AS HANDLE     NO-UNDO.
DEFINE VARIABLE c-json            AS LONGCHAR   NO-UNDO.
DEFINE VARIABLE lEnviou           AS LOGICAL    NO-UNDO.
DEFINE VARIABLE c-arq-json        AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lresp             AS LOGICAL    NO-UNDO.
DEFINE VARIABLE i-count           AS INTEGER    NO-UNDO.
DEFINE VARIABLE c-retorno         AS LONGCHAR   NO-UNDO.

/* ------- Defini‡Æo de Temp-Tables ------ */
{esp/esint001ie.i}

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

DEF VAR i-tot AS INTEGER NO-UNDO.

FIND FIRST es-api-export NO-LOCK WHERE ROWID(es-api-export) = r-table NO-ERROR.
IF AVAIL es-api-export THEN DO:

    FIND FIRST es-api-param WHERE es-api-param.ind-tipo-trans = es-api-export.ind-tipo-trans
                              AND es-api-param.cd-tipo-integr = es-api-export.cd-tipo-integr NO-LOCK NO-ERROR. 

    FIND FIRST es-api-export-lead OF es-api-export NO-ERROR.
    IF AVAIL es-api-export-lead THEN DO:

        /*----- Envia lotes de 5000 mil registros ----- */
        FOR EACH sfa-lead-time WHERE data-atualiz = TODAY :

            i-tot = i-tot + 1.

            /*IF i-tot > 1050 THEN LEAVE.*/
            

            i-count = i-count + 1.

            CREATE tt_lead_time.
            BUFFER-COPY sfa-lead-time TO tt_lead_time.
            ASSIGN tt_lead_time.cod-lead-time = ?.

            IF i-count = 1000 THEN DO:
                IF TEMP-TABLE tt_lead_time:HAS-RECORDS THEN
                    ASSIGN h-Temp = BUFFER tt_lead_time:HANDLE.

                RUN pi-processa-json (INPUT h-temp,
                                      OUTPUT c-erro).
                EMPTY TEMP-TABLE tt_lead_time.
                ASSIGN i-count = 0.

                IF c-erro <> "" THEN
                    RETURN "NOK".
            END.
        END.

        /* ------ Envia caso tenha menos de 1000 regitros ---- */
        IF TEMP-TABLE tt_lead_time:HAS-RECORDS THEN
            ASSIGN h-Temp = BUFFER tt_lead_time:HANDLE.

         RUN pi-processa-json (INPUT h-temp,
                               OUTPUT c-erro).
         EMPTY TEMP-TABLE tt_lead_time.
         IF c-erro <> "" THEN
             RETURN "NOK".

    END.
END.



IF VALID-HANDLE(h-esint002) THEN
    DELETE OBJECT h-esint002.



/* -------------------------------------------------------------- Procedures ------------------------------------------------*/

PROCEDURE pi-processa-json:

    DEFINE INPUT  PARAM p-temp AS HANDLE NO-UNDO.
    DEFINE OUTPUT PARAM p-erro AS CHARACTER NO-UNDO.

    /*------------------------------------------ Item -------------------------------------------*/
    IF valid-handle(p-temp) THEN DO:

        oJsonObjMain = NEW JsonObject().

        RUN piCriaObj IN h-esint002 (INPUT p-temp,
                                     OUTPUT ojsonObjAux,
                                     OUTPUT ojsonArrayAux,
                                     INPUT YES) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:
            ASSIGN p-erro = ERROR-STATUS:GET-MESSAGE(1).
            DELETE OBJECT h-temp.
            RETURN "NOK".
        END.

        oJsonObjMain:ADD("req",ojsonArrayAux).

        DELETE OBJECT p-temp.
    END.
    
    FIND FIRST es-api-param WHERE es-api-param.ind-tipo-trans = es-api-export.ind-tipo-trans
                              AND es-api-param.cd-tipo-integr = es-api-export.cd-tipo-integr NO-LOCK NO-ERROR.                            
    /* ------ Grava conteudo do Json em variavel -----*/
    RUN piGeraVarJson IN h-esint002 (INPUT oJsonObjMain,
                                     OUTPUT c-Json) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        ASSIGN p-erro = ERROR-STATUS:GET-MESSAGE(1).
        DELETE OBJECT h-esint002.
        RETURN "NOK".
    END.

    ASSIGN es-api-export-lead.c-json = c-Json.

    /* ------------ Envia Objeto Json --------- */
    RUN piPostJsonObj IN h-esint002 (INPUT oJsonObjMain,
                                     INPUT rowid(es-api-param),
                                     OUTPUT lResp,
                                     OUTPUT TABLE RowErrors,
                                     OUTPUT c-retorno).
    IF TEMP-TABLE rowErrors:HAS-RECORDS THEN DO:
        FOR EACH rowErrors:
            ASSIGN p-erro = c-erro + string(rowerrors.ErrorNumber)  + " - " + rowerrors.ErrorDescription.
            DELETE OBJECT h-esint002.
            RETURN "NOK".
        END.
    END.

END PROCEDURE.

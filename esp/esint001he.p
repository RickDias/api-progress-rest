/*----------------------------------------------------------------------------------------------/
 Programa..: esint001he.p
 Objetivo..: Interface Integraá∆o Fam°lia Comercial SFA
 Data......: 21/05/2019
 Autor.....: RogÇrio Dias
 Vers∆o....: 1.000.000
-----------------------------------------------------------------------------------------------*/


/* ------ Definiá∆o das classes de objetos ------ */
using Progress.Json.OBJECTModel.JsonOBJECT.
using Progress.Json.OBJECTModel.JsonArray.

{include/i-prgvrs.i esint001HE 1.00.00.000} 

/* ------- Definiá∆o de ParÉmetros ----- */
DEFINE INPUT  PARAMETER r-table AS ROWID     NO-UNDO.
DEFINE OUTPUT PARAMETER c-erro  AS CHARACTER NO-UNDO.

/* ------- Definiá∆o de Vari†veis ----- */
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

/* ------- Definiá∆o de Temp-Tables ------ */
{esp\esint001h.i}

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

    FIND FIRST es-api-param WHERE es-api-param.ind-tipo-trans = sfa-export.ind-tipo-trans
                              AND es-api-param.cd-tipo-integr = sfa-export.cd-tipo-integr NO-LOCK NO-ERROR. 

    FIND FIRST sfa-export-fam-comerc OF sfa-export NO-ERROR.
    IF AVAIL sfa-export-fam-comerc THEN DO:

        /*------------------------------------------ Item -------------------------------------------*/
        RUN piGravaTTFamiliaComercial (OUTPUT h-temp,
                                       OUTPUT c-erro).
        IF valid-handle(h-temp) THEN DO:

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
        END.

        oJsonArrayMain = NEW JsonArray().
        oJsonArrayMain:ADD(ojsonObjIni).
        
        /* ----- Cria Json Principal ------- */
        oJsonObjMain = NEW JsonObject().
        oJsonObjMain:ADD("req",oJsonArrayMain).

        FIND FIRST es-api-param WHERE es-api-param.ind-tipo-trans = sfa-export.ind-tipo-trans
                                  AND es-api-param.cd-tipo-integr = sfa-export.cd-tipo-integr NO-LOCK NO-ERROR.                            

        /* ------ Grava conteudo do Json em variavel -----*/
        RUN piGeraVarJson IN h-esint002 (INPUT oJsonObjMain,
                                       OUTPUT c-Json) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:
            ASSIGN c-erro = ERROR-STATUS:GET-MESSAGE(1).
            DELETE OBJECT h-esint002.
            RETURN "NOK".
        END.

        ASSIGN sfa-export-fam-comerc.c-json = c-Json.
    
        /* ------------ Envia Objeto Json --------- */
         RUN piPostJsonObj IN h-esint002 (INPUT oJsonObjMain,
                                          INPUT rowid(es-api-param),
                                          OUTPUT lResp,
                                          OUTPUT TABLE RowErrors,
                                          OUTPUT c-retorno).
         ASSIGN sfa-export.text-retorno     = c-retorno.

         IF TEMP-TABLE rowErrors:HAS-RECORDS THEN DO:
             FOR EACH rowErrors:
                 ASSIGN c-erro = c-erro + string(rowerrors.ErrorNumber)  + " - " + rowerrors.ErrorDescription.
                 DELETE OBJECT h-esint002.
                 RETURN "NOK".
             END.
         END.
    END.
    ELSE do: 
        ASSIGN c-erro = "Registro Tabela de Fam°lia Comercial n∆o localizado".
        RETURN "NOK".
    END.
END.



IF VALID-HANDLE(h-esint002) THEN
    DELETE OBJECT h-esint002.



/* -------------------------------------------------------------- Procedures ------------------------------------------------*/

PROCEDURE piGravaTTFamiliaComercial:

    DEFINE OUTPUT PARAMETER pTemp AS HANDLE    NO-UNDO.
    DEFINE OUTPUT PARAMETER pErro AS CHARACTER NO-UNDO.

    FIND FIRST fam-comerc NO-LOCK WHERE fam-comerc.fm-cod-com = sfa-export-fam-comerc.fm-cod-com NO-ERROR.
    IF AVAIL fam-comerc THEN DO:
        CREATE tt_fam_comerc.
        BUFFER-COPY fam-comerc TO tt_fam_comerc.
    END.
    ELSE DO:
        pErro = "Registro Fam°lia Comercial n∆o localizado com o campo cod-fam-comerc: " + string(sfa-export-fam-comerc.fm-cod-com).
        RETURN "NOK".
    END.

    IF TEMP-TABLE tt_fam_comerc:HAS-RECORDS THEN
        ASSIGN pTemp = BUFFER tt_fam_comerc:HANDLE.

END PROCEDURE.

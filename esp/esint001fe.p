/*----------------------------------------------------------------------------------------------/
 Programa..: esint001f.p
 Objetivo..: Interface Integraá∆o Representantes SFA
 Data......: 28/02/2019
 Autor.....: RogÇrio Dias
 Vers∆o....: 1.000.000
-----------------------------------------------------------------------------------------------*/


/* ------ Definiá∆o das classes de objetos ------ */
using Progress.Json.OBJECTModel.JsonOBJECT.
using Progress.Json.OBJECTModel.JsonArray.

{include/i-prgvrs.i esint00ife 1.00.00.000} 

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
{esp\esint001f.i}

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

    FIND FIRST sfa-export-repres OF sfa-export NO-ERROR.
    IF AVAIL sfa-export-repres THEN DO:

        /*------------------------------------------ Emitente -------------------------------------------*/
        RUN piGravaTTRepres (OUTPUT h-temp,
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

        /*---------------------------------------- Contact List ---------------------------------------*/
        RUN piGravaTTRepresFamilia (OUTPUT h-temp).

        IF valid-handle(h-temp) THEN DO:

            /* ------ Adiciona Array -----*/
            RUN piCriaObj IN h-esint002 (INPUT h-temp,
                                         OUTPUT ojsonObjAux,
                                         OUTPUT ojsonArrayAux,
                                         INPUT YES) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN DO:
                ASSIGN c-erro = ERROR-STATUS:GET-MESSAGE(1).
                DELETE OBJECT h-temp.
                RETURN "NOK".
            END.

            ojsonObjIni:ADD("FamiliaList",ojsonArrayAux).
            
            DELETE OBJECT h-temp.
        END.

        RUN piGravaTTRepresCliente (OUTPUT h-temp).

        IF valid-handle(h-temp) THEN DO:

            /* ------ Adiciona Array -----*/
            RUN piCriaObj IN h-esint002 (INPUT h-temp,
                                         OUTPUT ojsonObjAux,
                                         OUTPUT ojsonArrayAux,
                                         INPUT YES) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN DO:
                ASSIGN c-erro = ERROR-STATUS:GET-MESSAGE(1).
                DELETE OBJECT h-temp.
                RETURN "NOK".
            END.

            ojsonObjIni:ADD("AccountList",ojsonArrayAux).

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

        ASSIGN sfa-export-repres.c-json = c-Json.
    
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
        ASSIGN c-erro = "Registro Tabela de Representante n∆o localizado".
        RETURN "NOK".
    END.
END.



IF VALID-HANDLE(h-esint002) THEN
    DELETE OBJECT h-esint002.



/* -------------------------------------------------------------- Procedures ------------------------------------------------*/

PROCEDURE piGravaTTRepres:

    DEFINE OUTPUT PARAMETER pTemp AS HANDLE    NO-UNDO.
    DEFINE OUTPUT PARAMETER pErro AS CHARACTER NO-UNDO.

    FIND FIRST repres NO-LOCK WHERE repres.cod-rep = sfa-export-repres.cod-rep NO-ERROR.
    IF AVAIL repres THEN DO:
        CREATE tt_repres.
        BUFFER-COPY repres TO tt_repres.
        IF repres.ind-situacao = 1 THEN
            ASSIGN tt_repres.situacao = NO.
        ELSE 
            ASSIGN tt_repres.situacao = YES.
    END.
    ELSE DO:
        pErro = "Registro Representante n∆o localizado com o campo cod-rep: " + string(sfa-export-repres.cod-rep).
        RETURN "NOK".
    END.

    IF TEMP-TABLE tt_repres:HAS-RECORDS THEN
        ASSIGN pTemp = BUFFER tt_repres:HANDLE.

END PROCEDURE.


PROCEDURE piGravaTTRepresFamilia:

    DEFINE OUTPUT PARAMETER pTemp AS HANDLE    NO-UNDO.
    
    FOR EACH es-gp-mix-produto NO-LOCK WHERE es-gp-mix-produto.cod-rep = sfa-export-repres.cod-rep:
        CREATE tt_repres_familia.
        BUFFER-COPY es-gp-mix-produto TO tt_repres_familia.
    END.
    
    IF TEMP-TABLE tt_repres_familia:HAS-RECORDS THEN
        ASSIGN pTemp = BUFFER tt_repres_familia:HANDLE.

END PROCEDURE.

PROCEDURE piGravaTTRepresCliente:


    DEFINE OUTPUT PARAMETER pTemp AS HANDLE    NO-UNDO.
    
    FOR EACH es-gp-mix-vendedor NO-LOCK WHERE es-gp-mix-vendedor.cod-rep = sfa-export-repres.cod-rep :
        CREATE tt_repres_cliente.
        
        FIND FIRST emitente WHERE emitente.cod-emitente = es-gp-mix-vendedor.cod-emitente NO-LOCK NO-ERROR.
        IF AVAIL emitente THEN
            ASSIGN tt_repres_cliente.cgc = emitente.cgc.
    END.

    IF TEMP-TABLE tt_repres_cliente:HAS-RECORDS THEN
        ASSIGN pTemp = BUFFER tt_repres_cliente:HANDLE.

END PROCEDURE.


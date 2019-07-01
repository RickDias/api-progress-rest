/*----------------------------------------------------------------------------------------------/
 Programa..: esint020be.p
 Objetivo..: Interface Chamada B2E Fornecedores Ariba
 Data......: 29/05/2019
 Autor.....: Marcelo Brasil
 Vers∆o....: 1.000.000
-----------------------------------------------------------------------------------------------*/


/* ------ Definiá∆o das classes de objetos ------ */
using Progress.Json.OBJECTModel.JsonOBJECT.
using Progress.Json.OBJECTModel.JsonArray.
USING System.Text.RegularExpressions.*. 

{include/i-prgvrs.i ESINT020BE 1.00.00.000} 

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

DEFINE VARIABLE h-temp            AS HANDLE     NO-UNDO.
DEFINE VARIABLE h-esint002        AS HANDLE     NO-UNDO.
DEFINE VARIABLE c-json            AS LONGCHAR   NO-UNDO.
DEFINE VARIABLE lEnviou           AS LOGICAL    NO-UNDO.
DEFINE VARIABLE c-arq-json        AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lresp             AS LOGICAL    NO-UNDO.

DEF BUFFER pais FOR mgcad.pais.


/* ------- Definiá∆o de Temp-Tables ------ */
{esp/esint020ae.i}

DEFINE TEMP-TABLE RowErrors NO-UNDO
    FIELD ErrorSequence    AS INTEGER
    FIELD ErrorNumber      AS INTEGER
    FIELD ErrorDescription AS CHARACTER
    FIELD ErrorParameters  AS CHARACTER
    FIELD ErrorType        AS CHARACTER
    FIELD ErrorHelp        AS CHARACTER
    FIELD ErrorSubType     AS CHARACTER.

FUNCTION fncValidaMail RETURN CHARACTER
    (INPUT pmail AS CHARACTER ) FORWARD.

FUNCTION fncFormataTelefone RETURN CHARACTER 
    (INPUT pFone AS CHARACTER) FORWARD.


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

FIND FIRST sfa-export NO-LOCK 
     WHERE ROWID(sfa-export) = r-table 
     NO-ERROR.
IF AVAIL sfa-export 
THEN DO:

    FIND FIRST es-api-param NO-LOCK 
         WHERE es-api-param.ind-tipo-trans = sfa-export.ind-tipo-trans
           AND es-api-param.cd-tipo-integr = sfa-export.cd-tipo-integr 
         NO-ERROR. 

    FIND FIRST api-export-b2e-pj 
            OF sfa-export 
         NO-ERROR.
    IF AVAIL api-export-b2e-pj 
    THEN DO:

        /*------------------------------------------ Emitente -------------------------------------------*/
        RUN piGravaTTFornecedor (OUTPUT h-temp,
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

        
        /*----------- Grava Json ---------- */
        RUN piGeraArqJson  IN h-esint002 (INPUT oJsonObjMain,
                                          INPUT es-api-param.dir-export,
                                          INPUT "cli",
                                          OUTPUT c-arq-json) .
        IF ERROR-STATUS:ERROR THEN DO:
            ASSIGN c-erro = ERROR-STATUS:GET-MESSAGE(1).
            DELETE OBJECT h-esint002.
            RETURN "NOK".
        END.
        
        /* ------ Grava conteudo do Json em variavel -----*/
        RUN piGeraVarJson IN h-esint002 (INPUT oJsonObjMain,
                                         OUTPUT c-Json) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:
            ASSIGN c-erro = ERROR-STATUS:GET-MESSAGE(1).
            DELETE OBJECT h-esint002.
            RETURN "NOK".
        END.

        ASSIGN api-export-b2e-pj.c-json = c-Json.
    
        /* ------------ Envia Objeto Json --------- */
        RUN piPostJson IN h-esint002 (INPUT c-Json,
                                      INPUT rowid(es-api-param),
                                      OUTPUT lResp,
                                      OUTPUT TABLE RowErrors).
        
        ASSIGN 
           api-export-b2e-pj.text-retorno = RETURN-VALUE
           sfa-export.text-retorno        = RETURN-VALUE.

        IF TEMP-TABLE rowErrors:HAS-RECORDS THEN DO:
            FOR EACH rowErrors:
                ASSIGN c-erro = c-erro + string(rowerrors.ErrorNumber)  + " - " + rowerrors.ErrorDescription.
                DELETE OBJECT h-esint002.
                RETURN "NOK".
            END.
        END.        

    END.
    ELSE do: 
        ASSIGN c-erro = "Registro tabela do fornecedor n∆o localizada".
        RETURN "NOK".
    END.
END.

IF VALID-HANDLE(h-esint002) 
THEN DELETE OBJECT h-esint002.

/* -------------------------------------------------------------- Procedures ------------------------------------------------*/
PROCEDURE piGravaTTFornecedor:

    DEFINE OUTPUT PARAMETER pTemp AS HANDLE    NO-UNDO.
    DEFINE OUTPUT PARAMETER pErro AS CHARACTER NO-UNDO.

    DEFINE VARIABLE i            AS INTEGER     NO-UNDO. 
    
    FIND FIRST es-fornecedor-ariba NO-LOCK 
         WHERE es-fornecedor-ariba.cnpj = api-export-b2e-pj.cgc 
         NO-ERROR.
    IF AVAIL es-fornecedor-ariba 
    THEN DO:
        CREATE tt_fornecedor_b2e.
        BUFFER-COPY es-fornecedor-ariba TO tt_fornecedor_b2e.
        ASSIGN
           tt_fornecedor_b2e.tipo-fornecedor = "SERVICO".
    END.
    ELSE DO:
        pErro = "Registro fornecedor n∆o localizado com o campo CGC: " + api-export-b2e-pj.cgc.
        RETURN "NOK".
    END.

    IF TEMP-TABLE tt_fornecedor_b2e:HAS-RECORDS 
    THEN ASSIGN 
        pTemp = BUFFER tt_fornecedor_b2e:HANDLE.

END PROCEDURE.


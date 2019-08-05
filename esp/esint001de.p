/*----------------------------------------------------------------------------------------------/
 Programa..: esint001de.p
 Objetivo..: Interface Integraá∆o Notas Fiscais SFA
 Data......: 10/05/2019
 Autor.....: RogÇrio Dias
 Vers∆o....: 1.000.000
-----------------------------------------------------------------------------------------------*/


/* ------ Definiá∆o das classes de objetos ------ */
using Progress.Json.OBJECTModel.JsonOBJECT.
using Progress.Json.OBJECTModel.JsonArray.

{include/i-prgvrs.i esint001DE 1.00.00.000} 

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

DEF BUFFER pais FOR mgcad.pais.

/* ------- Definiá∆o de Temp-Tables ------ */
{esp\esint001d.i}

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

FIND FIRST es-api-export 
     WHERE ROWID(es-api-export) = r-table 
     NO-ERROR.
IF AVAIL es-api-export THEN DO:

    FIND FIRST es-api-export-nf OF es-api-export NO-ERROR.
    IF AVAIL es-api-export-nf THEN DO:

        /*------------------------------------------ Nota Fiscal -------------------------------------------*/
        RUN piGravaTTNf (OUTPUT h-temp,
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

        /*--------------------------------------- Item NF List -----------------------------------------*/
        RUN piGravaTTItemNf (OUTPUT h-temp).
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

            ojsonObjIni:ADD("ItemNotaFiscalList",ojsonArrayAux).

            DELETE OBJECT h-temp.
        END.

        oJsonArrayMain = NEW JsonArray().
        oJsonArrayMain:ADD(ojsonObjIni).
        
        /* ----- Cria Json Principal ------- */
        oJsonObjMain = NEW JsonObject().
        oJsonObjMain:ADD("req",oJsonArrayMain).
               
        FIND FIRST es-api-param WHERE es-api-param.ind-tipo-trans = es-api-export.ind-tipo-trans
                                  AND es-api-param.cd-tipo-integr = es-api-export.cd-tipo-integr NO-LOCK NO-ERROR.                            

        /* ------ Grava conteudo do Json em variavel -----*/
        RUN piGeraVarJson IN h-esint002 (INPUT oJsonObjMain,
                                         OUTPUT c-Json) NO-ERROR.

        IF ERROR-STATUS:ERROR THEN DO:
            ASSIGN c-erro = ERROR-STATUS:GET-MESSAGE(1).
            DELETE OBJECT h-esint002.
            RETURN "NOK".
        END.
        CLIPBOARD:VALUE = STRING(c-Json).

        ASSIGN es-api-export-nf.c-json = c-Json.

        /* ------------ Envia Objeto Json --------- */
        RUN piPostJsonObj IN h-esint002 (INPUT oJsonObjMain,
                                         INPUT rowid(es-api-param),
                                         OUTPUT lResp,
                                         OUTPUT TABLE RowErrors,
                                         OUTPUT c-retorno).
        ASSIGN es-api-export.text-retorno = c-retorno.

        IF TEMP-TABLE rowErrors:HAS-RECORDS THEN DO:
            FOR EACH rowErrors:
                ASSIGN c-erro = c-erro + string(rowerrors.ErrorNumber)  + " - " + rowerrors.ErrorDescription.
                DELETE OBJECT h-esint002.
                RETURN "NOK".
            END.
        END.        
    END.
    ELSE do: 
        ASSIGN c-erro = "Registro Tabela de Cliente n∆o localizado".
        RETURN "NOK".
    END.
END.



IF VALID-HANDLE(h-esint002) THEN
    DELETE OBJECT h-esint002.



/* -------------------------------------------------------------- Procedures ------------------------------------------------*/

PROCEDURE piGravaTTNf:

    DEFINE OUTPUT PARAMETER pTemp AS HANDLE    NO-UNDO.
    DEFINE OUTPUT PARAMETER pErro AS CHARACTER NO-UNDO.

    DEF VAR cTipoFrete AS CHARACTER NO-UNDO INITIAL "CIF,FOB".

    FIND FIRST nota-fiscal NO-LOCK WHERE nota-fiscal.cod-estabel = es-api-export-nf.cod-estabel
                                     AND nota-fiscal.serie       = es-api-export-nf.serie
                                     AND nota-fiscal.nr-nota-fis = es-api-export-nf.nr-nota-fis NO-ERROR.
    IF AVAIL nota-fiscal THEN DO:

        CREATE tt_nota_fiscal.
        BUFFER-COPY nota-fiscal TO tt_nota_fiscal. 
        ASSIGN tt_nota_fiscal.nr-nota-fis2 = tt_nota_fiscal.nr-nota-fis
               tt_nota_fiscal.cgc = TRIM(nota-fiscal.cgc).

        FIND FIRST pais WHERE pais.nome-pais = nota-fiscal.pais NO-LOCK NO-ERROR.
        IF AVAIL pais THEN
            ASSIGN  tt_nota_fiscal.Pais = substring(pais.char-1,23,02).

        ASSIGN tt_nota_fiscal.modalidadeFrete = entry(nota-fiscal.ind-tp-frete,cTipoFrete)
               tt_nota_fiscal.c-cod-cond-pag  = trim(STRING(nota-fiscal.cod-cond-pag,">99"))
               tt_nota_fiscal.endereco = STRING(nota-fiscal.cod-entrega).

        IF tt_nota_fiscal.endereco = 'PADRAO' THEN
            tt_nota_fiscal.endereco = "Padr∆o".
        ASSIGN tt_nota_fiscal.obs-gerada = "Obs Gerada: " + nota-fiscal.obs-gerada + chr(10) + " Obs Nota:" + nota-fiscal.observ-nota.

    END.
    ELSE DO:
        pErro = "Registro Nota Fiscal n∆o localizado " + es-api-export-nf.cod-estabel + " | " + es-api-export-nf.serie + " | " + es-api-export-nf.nr-nota-fis.
        RETURN "NOK".
    END.

    ASSIGN pTemp = BUFFER tt_nota_fiscal:HANDLE.

END PROCEDURE.


PROCEDURE piGravaTTItemNf:


    DEFINE OUTPUT PARAMETER pTemp AS HANDLE NO-UNDO.

    FOR EACH it-nota-fisc  WHERE it-nota-fisc.cod-estabel = es-api-export-nf.cod-estabel
                             AND it-nota-fisc.serie       = es-api-export-nf.serie
                             AND it-nota-fisc.nr-nota-fis = es-api-export-nf.nr-nota-fis NO-LOCK:
        CREATE tt_item_nota_fisc.
        BUFFER-COPY it-nota-fisc TO tt_item_nota_fisc.   
        ASSIGN
           tt_item_nota_fisc.codigo-item = it-nota-fisc.cod-estabel 
                                         + "-"
                                         + it-nota-fisc.serie
                                         + "-"
                                         + it-nota-fisc.nr-nota-fis
                                         + "-"
                                         + STRING(it-nota-fisc.nr-seq-fat,"99999").
    END.

    ASSIGN pTemp = BUFFER tt_item_nota_fisc:HANDLE.

END PROCEDURE.


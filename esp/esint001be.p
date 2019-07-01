/*----------------------------------------------------------------------------------------------/
 Programa..: esint001be.p
 Objetivo..: Interface Integraá∆o Pedidos SFA
 Data......: 26/02/2019
 Autor.....: RogÇrio Dias
 Vers∆o....: 1.000.000
-----------------------------------------------------------------------------------------------*/


/* ------ Definiá∆o das classes de objetos ------ */
using Progress.Json.OBJECTModel.JsonOBJECT.
using Progress.Json.OBJECTModel.JsonArray.

{include/i-prgvrs.i esint001BE 1.00.00.000} 

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
{esp/esint001b.i}

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


FIND FIRST sfa-export NO-LOCK WHERE ROWID(sfa-export) = r-table NO-ERROR.
IF AVAIL sfa-export THEN DO:

    FIND FIRST es-api-param WHERE es-api-param.ind-tipo-trans = sfa-export.ind-tipo-trans
                              AND es-api-param.cd-tipo-integr = sfa-export.cd-tipo-integr NO-LOCK NO-ERROR. 

    FIND FIRST sfa-export-ped OF sfa-export NO-ERROR.
    IF AVAIL sfa-export-ped THEN DO:

        /*------------------------------------------ Pedido -------------------------------------------*/
        RUN piGravaTTPedVenda (OUTPUT h-temp,
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

        /*--------------------------------------- Item do Pedido -----------------------------------------*/
        RUN piGravaTTPedItem (OUTPUT h-temp).

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

            ojsonObjIni:ADD("ItemPedidoList",ojsonArrayAux).

            DELETE OBJECT h-temp.

        END.

        oJsonArrayMain = NEW JsonArray().
        oJsonArrayMain:ADD(ojsonObjIni).
        
        /* ----- Cria Json Principal ------- */
        oJsonObjMain = NEW JsonObject().
        oJsonObjMain:ADD("req",oJsonArrayMain).

        /*
        /*----------- Grava Json ---------- */
        RUN piGeraArqJson  IN h-esint002 (INPUT oJsonObjMain,
                                          INPUT es-api-param.dir-export,
                                          INPUT "ped",
                                          OUTPUT c-arq-json) NO-ERROR.
        
        IF ERROR-STATUS:ERROR THEN DO:
            ASSIGN c-erro = ERROR-STATUS:GET-MESSAGE(1).
            DELETE OBJECT h-esint002.
            RETURN "NOK".
        END.
        */

        /* ------ Grava conteudo do Json em variavel -----*/
        RUN piGeraVarJson IN h-esint002 (INPUT oJsonObjMain,
                                         OUTPUT c-Json) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:
            ASSIGN c-erro = ERROR-STATUS:GET-MESSAGE(1).
            DELETE OBJECT h-esint002.
            RETURN "NOK".
        END.

        ASSIGN 
           sfa-export-ped.c-json = c-Json.

        /* ------------ Envia Objeto Json --------- */
        RUN piPostJsonObj IN h-esint002 (INPUT oJsonObjMain,
                                         INPUT rowid(es-api-param),
                                         OUTPUT lResp,
                                         OUTPUT TABLE RowErrors,
                                         OUTPUT c-retorno).

        ASSIGN sfa-export-ped.text-retorno = c-retorno
               sfa-export.text-retorno     = c-retorno.
           
        IF TEMP-TABLE rowErrors:HAS-RECORDS THEN DO:
            FOR EACH rowErrors:
                ASSIGN c-erro = c-erro + string(rowerrors.ErrorNumber)  + " - " + rowerrors.ErrorDescription.
                DELETE OBJECT h-esint002.
                RETURN "NOK".
            END.
        END.

    END.
    ELSE do: 
        ASSIGN c-erro = "Registro Tabela de Pedido n∆o localizado".
        RETURN "NOK".
    END.
END.

IF VALID-HANDLE(h-esint002) THEN
    DELETE OBJECT h-esint002.



/* -------------------------------------------------------------- Procedures ------------------------------------------------*/
PROCEDURE piGravaTTPedVenda:

    DEFINE OUTPUT PARAMETER pTemp AS HANDLE    NO-UNDO.
    DEFINE OUTPUT PARAMETER pErro AS CHARACTER NO-UNDO.

    DEF VAR cDestMerc AS CHARACTER NO-UNDO INITIAL "ComÇrcio/Industria,Cons Pr¢prio/Ativo".

    FIND FIRST ped-venda NO-LOCK WHERE ped-venda.nome-abrev = sfa-export-ped.nome-abrev
                                   AND ped-venda.nr-pedcli  = sfa-export-ped.nr-pedcli NO-ERROR.
    IF AVAIL ped-venda THEN DO:

        FIND FIRST repres NO-LOCK WHERE repres.nome-abrev = ped-venda.no-ab-reppri NO-ERROR.
        CREATE tt_PedVenda.
        BUFFER-COPY ped-venda TO tt_pedVenda. 

        ASSIGN tt_pedvenda.mensagem          = STRING(ped-venda.cod-mensagem)
               tt_pedvenda.moeda             = IF ped-venda.mo-fatur = 0 THEN "Real" ELSE ""
               tt_pedvenda.codcondpag        = trim(STRING(ped-venda.cod-cond-pag,">99"))
               tt_pedvenda.destinomercadoria = ENTRY(ped-venda.cod-des-merc,cDestMerc)
               tt_pedvenda.cod-rep           = repres.cod-rep
               tt_pedvenda.modalidade-frete  = ENTRY(ped-venda.ind-tp-frete,"CIF,FOB")
               tt_pedvenda.nr-pedrep         = ped-venda.nr-pedrep.

        IF tt_pedvenda.cod-entrega = 'PADRAO' THEN
            ASSIGN tt_pedvenda.cod-entrega = "Padr∆o".
    END.
    ELSE DO:
        pErro = "Registro Pedido de Venda n∆o localizado com o nome-abrev: " + sfa-export-ped.nome-abrev + " e nr-pedcli: " + sfa-export-ped.nr-pedcli.
        RETURN "NOK".
    END.

    ASSIGN pTemp = BUFFER tt_PedVenda:HANDLE.

END PROCEDURE.


PROCEDURE piGravaTTPedItem:

    DEFINE OUTPUT PARAMETER pTemp AS HANDLE NO-UNDO.

    FOR EACH ped-item NO-LOCK WHERE ped-item.nome-abrev = sfa-export-ped.nome-abrev
                                AND ped-item.nr-pedcli  = sfa-export-ped.nr-pedcli:

        CREATE tt_pedItem.
        BUFFER-COPY ped-item TO tt_pedItem.
    END.

    ASSIGN pTemp = BUFFER tt_PedItem:HANDLE.

END PROCEDURE.

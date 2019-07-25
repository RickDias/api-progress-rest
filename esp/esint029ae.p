/*----------------------------------------------------------------------------------------------/
 Programa..: levelapprovalmla.p
 Objetivo..: API REST para exportar n°vei de aprovaá∆o
 Data......: 02/07/2019
 Autor.....: Cleberson Silva
 Vers∆o....: 1.000.000
-----------------------------------------------------------------------------------------------*/
/* ------ Definiá∆o das classes de objetos ------ */
using Progress.Json.OBJECTModel.JsonOBJECT.
using Progress.Json.OBJECTModel.JsonArray.
USING System.Text.RegularExpressions.*. 

{include/i-prgvrs.i esint029ae 1.00.00.000} 

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
{esp/esint001a.i}

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

FIND FIRST sfa-export NO-LOCK WHERE ROWID(sfa-export) = r-table NO-ERROR.
IF AVAIL sfa-export THEN DO:

    FIND FIRST es-api-param WHERE es-api-param.ind-tipo-trans = sfa-export.ind-tipo-trans
                              AND es-api-param.cd-tipo-integr = sfa-export.cd-tipo-integr NO-LOCK NO-ERROR. 

    FIND FIRST sfa-export-cli OF sfa-export NO-ERROR.
    IF AVAIL sfa-export-cli THEN DO:

        /*------------------------------------------ Emitente -------------------------------------------*/
        RUN piGravaTTFaixaAprov (OUTPUT h-temp,
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

        
        /* ------ Grava conteudo do Json em variavel -----*/
        RUN piGeraVarJson IN h-esint002 (INPUT oJsonObjMain,
                                         OUTPUT c-Json) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:
            ASSIGN c-erro = ERROR-STATUS:GET-MESSAGE(1).
            DELETE OBJECT h-esint002.
            RETURN "NOK".
        END.

        ASSIGN sfa-export-cli.c-json = c-Json.

        /* ------------ Envia Objeto Json --------- */
        RUN piPostJsonObj IN h-esint002 (INPUT oJsonObjMain,
                                      INPUT rowid(es-api-param),
                                      OUTPUT lResp,
                                      OUTPUT TABLE RowErrors,
                                      OUTPUT c-retorno).
        IF c-retorno <> "" THEN
            ASSIGN sfa-export.text-retorno = c-retorno.

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
PROCEDURE piGravaTTFaixaAprov:

    DEFINE OUTPUT PARAMETER pTemp AS HANDLE    NO-UNDO.
    DEFINE OUTPUT PARAMETER pErro AS CHARACTER NO-UNDO.

    DEFINE VARIABLE c_conteudo AS CHARACTER  NO-UNDO.

    RUN piDeParaDocto.




    ASSIGN conteudo = "".
    FOR EACH mla-hierarquia-faixa NO-LOCK:

        IF NOT CAN-FIND (FIRST de-para-tipo 
                         WHERE de-para-tipo.doc-totvs = mla-hierarquia-faixa.cod-tip-doc) THEN NEXT.

        FOR EACH de-para-tipo NO-LOCK
           WHERE de-para-tipo.doc-totvs = mla-hierarquia-faixa.cod-tip-doc:

            FIND FIRST mla-faixa-aprov OF mla-hierarquia-faixa NO-LOCK NO-ERROR.

             IF c_conteudo = "" THEN                                                             
                 ASSIGN c_conteudo = SUBSTITUTE("&1,&2,&3,&4,&5,&6,&7,&8",                         
                                   mla-hierarquia-faixa.cod-usuar,                               
                                   STRING(mla-hierarquia-faixa.seq-aprov),                       
                                   de-para-tipo.tp-ariba,                                        
                                   STRING(mla-hierarquia-faixa.cod-lotacao),                     
                                   mla-hierarquia-faixa.cod-estabel,                             
                                   STRING(mla-hierarquia-faixa.ep-codigo),                       
                                   STRING(mla-hierarquia-faixa.num-faixa),                       
                                   REPLACE(STRING(mla-faixa-aprov.limite-ini),",","")            
                                   + "-" + REPLACE(STRING(mla-faixa-aprov.limite-fim),",","")).  
             ELSE                                                                                
                                                                                                 
                 ASSIGN c_conteudo = c_conteudo + "," +                                              
                                   SUBSTITUTE("&1,&2,&3,&4,&5,&6,&7,&8",                         
                                   mla-hierarquia-faixa.cod-usuar,                               
                                   STRING(mla-hierarquia-faixa.seq-aprov),                       
                                   de-para-tipo.tp-ariba,                                        
                                   STRING(mla-hierarquia-faixa.cod-lotacao),                     
                                   mla-hierarquia-faixa.cod-estabel,                             
                                   STRING(mla-hierarquia-faixa.ep-codigo),                       
                                   STRING(mla-hierarquia-faixa.num-faixa),                       
                                   REPLACE(STRING(mla-faixa-aprov.limite-ini),",","")            
                                   + "-" + REPLACE(STRING(mla-faixa-aprov.limite-fim),",","")).  
        END.
    END.


    IF c_conteudo <> "" THEN
    DO:
        CREATE tt_csv.
        ASSIGN tt_csv.conteudo = c_conteudo.

        FIND FIRST es-exporta-mla EXCLUSIVE-LOCK NO-ERROR.
        IF AVAIL es-exporta-mla THEN ASSIGN es-exporta-mla.ind-exportar = NO.
        FIND CURRENT es-exporta-mla NO-LOCK NO-ERROR.
    END.
    

    

    IF TEMP-TABLE tt_csv:HAS-RECORDS THEN
        ASSIGN pTemp = BUFFER tt_csv:HANDLE.

END PROCEDURE.



PROCEDURE piDeParaDocto:
/*----------------------------------------------------------------
   Purpose: Cria de-para de documento
------------------------------------------------------------------*/
    CREATE de-para-tipo.
    ASSIGN de-para-tipo.doc-totvs = 7           
           de-para-tipo.tp-ariba  = "CUS_REQ_01".
    CREATE de-para-tipo.
    ASSIGN de-para-tipo.doc-totvs = 7           
           de-para-tipo.tp-ariba  = "CUS_REQ_02".
    CREATE de-para-tipo.
    ASSIGN de-para-tipo.doc-totvs = 8           
           de-para-tipo.tp-ariba  = "CUS_REQ_03".
    CREATE de-para-tipo.
    ASSIGN de-para-tipo.doc-totvs = 7           
           de-para-tipo.tp-ariba  = "CUS_REQ_04".
    CREATE de-para-tipo.
    ASSIGN de-para-tipo.doc-totvs = 7           
           de-para-tipo.tp-ariba  = "CUS_REQ_05".
    CREATE de-para-tipo.
    ASSIGN de-para-tipo.doc-totvs = 7           
           de-para-tipo.tp-ariba  = "CUS_REQ_06".
    CREATE de-para-tipo.
    ASSIGN de-para-tipo.doc-totvs = 7           
           de-para-tipo.tp-ariba  = "CUS_REQ_07".
    CREATE de-para-tipo.
    ASSIGN de-para-tipo.doc-totvs = 7           
           de-para-tipo.tp-ariba  = "CUS_REQ_08".
    CREATE de-para-tipo.
    ASSIGN de-para-tipo.doc-totvs = 7           
           de-para-tipo.tp-ariba  = "CUS_REQ_09".
    CREATE de-para-tipo.
    ASSIGN de-para-tipo.doc-totvs = 7           
           de-para-tipo.tp-ariba  = "CUS_REQ_10".
    CREATE de-para-tipo.
    ASSIGN de-para-tipo.doc-totvs = 7           
           de-para-tipo.tp-ariba  = "CUS_REQ_11".
    CREATE de-para-tipo.
    ASSIGN de-para-tipo.doc-totvs = 2           
           de-para-tipo.tp-ariba  = "*".
END PROCEDURE.



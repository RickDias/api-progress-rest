/*------------------------------------------------------------------------
    File        : levelApprovalMLA.p
    Purpose     : API REST para exportar n¡vei de aprova‡Æo
    Syntax      :
    Description : Niveis de Hierarquia de Aprova‡Æo
    Author(s)   : Cleberson Silva
    Created     : 02/07/2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{utp/ut-api.i}
{utp/ut-api-utils.i}

{include/i-prgvrs.i levelApprovalMLA 2.00.00.000} /*** 010000 ***/

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i levelApprovalMLA MCC}
&ENDIF


{utp/ut-api-action.i pi-getAll GET /~*}
//{utp/ut-api-action.i pi-get    GET /~*}
{utp/ut-api-notfound.i}

/* ------- Defini‡Æo Temp-tables ------ */   
DEF TEMP-TABLE tt-erros
    FIELD cod-erro  AS INTEGER
    FIELD desc-erro AS CHARACTER FORMAT "x(256)"
    FIELD desc-arq  AS CHARACTER.

DEF TEMP-TABLE ttRetorno 
    FIELD codigocliente   AS CHARACTER
    FIELD situacao        AS LOGICAL 
    FIELD descricao   AS CHAR FORMAT "x(200)".

DEF TEMP-TABLE de-para-tipo NO-UNDO
    FIELD doc-totvs         AS INT
    FIELD tp-ariba          AS CHAR.


DEFINE TEMP-TABLE tt_csv NO-UNDO
    field conteudo AS CHARACTER serialize-name "conteudo".  


/* ------- Defini‡Æo Vari veis ------ */
DEFINE VARIABLE i-seq-erro AS INTEGER     NO-UNDO.
DEFINE VARIABLE h-esint002 AS HANDLE      NO-UNDO.





PROCEDURE pi-getAll:
    DEFINE INPUT  PARAM jsonInput AS JsonObject NO-UNDO.
    DEFINE OUTPUT PARAM jsonOutput AS JsonObject NO-UNDO.

    DEFINE VARIABLE jsonRetorno      AS JsonArray            NO-UNDO.
    DEFINE VARIABLE json_recebido    AS LONGCHAR             NO-UNDO.
    DEFINE VARIABLE oRequestParser   AS JsonAPIRequestParser NO-UNDO.
    DEFINE VARIABLE CodigoCliente    AS CHARACTER INITIAL ?  NO-UNDO.
    DEFINE VARIABLE c-erro           AS CHARACTER            NO-UNDO.
    DEFINE VARIABLE iCountMain       AS INTEGER              NO-UNDO.
    DEFINE VARIABLE oJsonObjectMain  AS JsonObject           NO-UNDO.
    DEFINE VARIABLE oJsonArrayMain   AS JsonArray            NO-UNDO.
    DEFINE VARIABLE oJsonObjMain     AS JsonObject           NO-UNDO.
    DEFINE VARIABLE h-temp           AS HANDLE               NO-UNDO.
    DEFINE VARIABLE oJsonObjIni       AS jsonObject NO-UNDO.
    DEFINE VARIABLE ojsonArrayIni     AS JsonArray  NO-UNDO.

    EMPTY TEMP-TABLE tt-Erros.
    EMPTY TEMP-TABLE ttRetorno.


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



    CREATE  sfa-import-mla.                                                             
    ASSIGN  sfa-import-mla.cd-tipo-integr  = 29 /*--- import ---*/                       
            sfa-import-mla.Id-movto        = NEXT-VALUE(seq-import)                     
            sfa-import-mla.data-movto      = TODAY                                      
            sfa-import-mla.c-json          = "".                             
                                                                                        
    CREATE  sfa-import.                                                                 
    ASSIGN  sfa-import.ind-tipo-trans   = 1 /*--- import ---*/                          
            sfa-import.cd-tipo-integr   = sfa-import-mla.cd-tipo-integr                 
            sfa-import.id-movto         = sfa-import-mla.Id-movto                       
            sfa-import.chave            = STRING(INT(TODAY) + TIME)                                    
            sfa-import.data-movto       = NOW                                           
            sfa-import.data-inicio      = NOW                                           
            sfa-import.data-fim         = ?                                             
            sfa-import.ind-situacao     = 1 /*--- Pendente ---*/                        
            sfa-import.cod-status       = 0 /*--- sem status ---*/  .   


    RUN piGravaTTCSV (OUTPUT h-temp,                           
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
    /*jsonRetorno = NEW JsonObject().                           
    jsonRetorno:ADD("req",oJsonArrayMain).  */
                                                             
    RUN createJsonResponse(INPUT oJsonArrayMain,                
                           INPUT TABLE RowErrors,            
                           INPUT FALSE,                      
                           OUTPUT jsonOutput).               



END PROCEDURE.


PROCEDURE piGravaTTCSV:

    DEFINE OUTPUT PARAMETER pTemp AS HANDLE    NO-UNDO.
    DEFINE OUTPUT PARAMETER pErro AS CHARACTER NO-UNDO.

    DEFINE VARIABLE c_conteudo AS CHARACTER  NO-UNDO.

    /*-- cria tabela de-para --*/
    RUN piDeParaDocto.

    EMPTY TEMP-TABLE tt_csv.

    ASSIGN c_conteudo = "".
    FOR EACH mla-hierarquia-faixa NO-LOCK:

        IF NOT CAN-FIND (FIRST de-para-tipo 
                         WHERE de-para-tipo.doc-totvs = mla-hierarquia-faixa.cod-tip-doc) THEN NEXT.


        FOR EACH de-para-tipo NO-LOCK
           WHERE de-para-tipo.doc-totvs = mla-hierarquia-faixa.cod-tip-doc:

            FIND FIRST mla-faixa-aprov OF mla-hierarquia-faixa NO-LOCK NO-ERROR.

            IF c_conteudo = "" THEN
                ASSIGN conteudo = SUBSTITUTE("&1,&2,&3,&4,&5,&6,&7,&8",
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

                ASSIGN conteudo = conteudo + "," + 
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

    CREATE tt_csv.
    ASSIGN tt_csv.conteudo = c_conteudo.

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


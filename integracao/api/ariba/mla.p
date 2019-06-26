&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : importarpedcompra.p
    Purpose     : Integra‡ao - ARIBA x Pedido de Compra

    Syntax      :

    Description :

    Author(s)   : TOTVS
    Created     : 04/2019
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


// O campo ind-tipo-movto trata qual movimento esta realizando
// 1 - Inclusao / 2 - Alteracao / 3 - Exclusao

{utp/ut-api.i}
{utp/ut-api-utils.i}
{cdp/cdcfgmat.i}

DEF TEMP-TABLE mla-exportar NO-UNDO
    FIELD usuario         AS CHAR
    FIELD sequencia       AS CHAR
    FIELD documento       AS CHAR
    FIELD lotacao         AS CHAR
    FIELD estabelecimento AS CHAR
    FIELD empresa         AS CHAR
    FIELD faixa           AS CHAR
    FIELD TotalCost           AS CHAR.

DEF TEMP-TABLE de-para-tipo NO-UNDO
    FIELD doc-totvs         AS INT
    FIELD tp-ariba          AS CHAR.

DEF TEMP-TABLE tt-erros
    FIELD cod-erro  AS INTEGER
    FIELD desc-erro AS CHARACTER FORMAT "x(256)"
    FIELD desc-arq  AS CHARACTER.

DEFINE TEMP-TABLE ttError      SERIALIZE-NAME "Retorno"
    FIELD SessionId         AS CHARACTER
    FIELD referencia        AS CHARACTER
    FIELD codigo            AS CHARACTER
    FIELD descricao         AS CHARACTER
    INDEX idx01 referencia codigo.


DEFINE NEW GLOBAL SHARED VARIABLE c-seg-usuario AS CHAR FORMAT "x(12)" NO-UNDO.

DEFINE VARIABLE h-acomp AS HANDLE NO-UNDO.
DEFINE VARIABLE iNumNewPedido AS INTEGER NO-UNDO.
DEFINE VARIABLE i-num-pedido AS INTEGER NO-UNDO.
DEFINE VARIABLE c-formato-cgc AS CHARACTER NO-UNDO.
DEFINE VARIABLE l-modulo-ge AS LOGICAL NO-UNDO.
DEFINE VARIABLE i-cond-pagto AS INTEGER NO-UNDO.
DEFINE VARIABLE i-num-ordem AS INTEGER NO-UNDO.
DEFINE VARIABLE l-manut-item-fornec AS LOGICAL NO-UNDO.
DEFINE VARIABLE i-parcela AS INTEGER NO-UNDO.
DEFINE VARIABLE c-un AS CHARACTER NO-UNDO.
DEFINE VARIABLE da-dt-entrega AS DATE NO-UNDO.
DEFINE VARIABLE de-quantidade AS DECIMAL NO-UNDO.
DEFINE VARIABLE i-nro-rows AS INTEGER NO-UNDO.

DEFINE VARIABLE l-existe-despesa AS LOGICAL NO-UNDO.
DEFINE VARIABLE c-pais-emit AS CHARACTER NO-UNDO.
DEFINE VARIABLE c-pais-estab AS CHARACTER NO-UNDO.
DEFINE VARIABLE rw-cotacao AS ROWID NO-UNDO.
DEFINE VARIABLE i-natureza AS INTEGER NO-UNDO.
DEFINE VARIABLE l-despesa AS LOG INIT YES NO-UNDO.
DEFINE VARIABLE lLoop AS LOG INIT YES NO-UNDO.
DEFINE VARIABLE lRetOK AS LOG NO-UNDO.


DEFINE VARIABLE l-codigo-icm-sensitive AS LOGICAL NO-UNDO.
DEFINE VARIABLE l-aliquota-icm-sensitive AS LOGICAL NO-UNDO.
DEFINE VARIABLE l-aliquota-iss-sensitive AS LOGICAL NO-UNDO.
DEFINE VARIABLE l-valor-taxa-sensitive AS LOGICAL NO-UNDO.
DEFINE VARIABLE l-taxa-financ-sensitive AS LOGICAL NO-UNDO.
DEFINE VARIABLE l-possui-reaj-sensitive AS LOGICAL NO-UNDO.

DEFINE VARIABLE i-cont AS INT INIT 1 NO-UNDO.


DEFINE VARIABLE c-end-cobranca-aux LIKE pedido-compr.end-cobranca NO-UNDO.
DEFINE VARIABLE c-end-entrega-aux LIKE pedido-compr.end-entrega NO-UNDO.
DEFINE VARIABLE i-cod-mensagem LIKE pedido-compr.cod-mensagem NO-UNDO.

DEFINE VARIABLE json_recebido  AS LONGCHAR NO-UNDO.
DEFINE VARIABLE json_retorno   AS LONGCHAR NO-UNDO.


DEFINE VARIABLE h-boin295 AS HANDLE NO-UNDO.
DEFINE VARIABLE h-boin274sd AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15.29
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

RUN btb/btapi910ze.p   (INPUT "tcpasilva", /*USUARIO*/
                        INPUT "",          /*SENHA*/
                        INPUT "1",         /*EMPRESA*/
                        OUTPUT TABLE tt-erros). /*RETORNO DE ERROSl*/

{utp/ut-api-action.i pi-00-get GET /~*}
{utp/ut-api-notfound.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-pi-00-get) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-00-get Procedure 
PROCEDURE pi-00-get :
DEFINE INPUT  PARAMETER jsonInput  AS JsonObject NO-UNDO.
DEFINE OUTPUT PARAMETER jsonOutput AS JsonObject NO-UNDO.

DEF VAR lc-teste AS LONGCHAR NO-UNDO.

DEFINE VARIABLE oRequestParser AS JsonAPIRequestParser NO-UNDO.
DEFINE VARIABLE jsonRetorno AS JsonArray NO-UNDO.

RUN pi-02-processa-mla.

ASSIGN jsonRetorno = NEW JsonArray().
       jsonRetorno:Read(TEMP-TABLE mla-exportar:HANDLE).

RUN createJsonResponse(INPUT jsonRetorno, INPUT TABLE RowErrors, INPUT FALSE, OUTPUT jsonOutput).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-02-processa-mla) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-02-processa-mla Procedure 
PROCEDURE pi-02-processa-mla :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR c-lim-ini AS CHAR NO-UNDO.
DEF VAR c-lim-fim AS CHAR NO-UNDO.

EMPTY TEMP-TABLE mla-exportar.


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


FOR EACH mla-hierarquia-faixa NO-LOCK:

    IF NOT CAN-FIND (FIRST de-para-tipo WHERE
                           de-para-tipo.doc-totvs = mla-hierarquia-faixa.cod-tip-doc) THEN NEXT.

    FOR EACH de-para-tipo WHERE
             de-para-tipo.doc-totvs = mla-hierarquia-faixa.cod-tip-doc NO-LOCK:
        
        FIND FIRST mla-faixa-aprov OF mla-hierarquia-faixa NO-LOCK NO-ERROR.

        CREATE mla-exportar.
        ASSIGN mla-exportar.usuario         = mla-hierarquia-faixa.cod-usuar 
               mla-exportar.sequencia       = STRING(mla-hierarquia-faixa.seq-aprov)
               mla-exportar.documento       = de-para-tipo.tp-ariba
               mla-exportar.lotacao         = STRING(mla-hierarquia-faixa.cod-lotacao)
               mla-exportar.estabelecimento = mla-hierarquia-faixa.cod-estabel 
               mla-exportar.empresa         = STRING(mla-hierarquia-faixa.ep-codigo)
               mla-exportar.faixa           = STRING(mla-hierarquia-faixa.num-faixa)
               mla-exportar.TotalCost       = STRING(mla-faixa-aprov.limite-ini) + "-" + STRING(mla-faixa-aprov.limite-fim).

    END.
        
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


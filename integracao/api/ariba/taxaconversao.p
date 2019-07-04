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

DEF TEMP-TABLE CurrencyMapExport NO-UNDO
    FIELD Preferred  AS CHAR
    FIELD c-Value    AS CHAR
    FIELD Comment    AS CHAR
    FIELD c-Key      AS CHAR.

DEF TEMP-TABLE CurrencyConversionRateExport NO-UNDO
    FIELD FromCurrency  AS CHAR
    FIELD ToCurrency    AS CHAR
    FIELD UniqueName    AS CHAR
    FIELD Rate          AS CHAR
    FIELD Date          AS CHAR.

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

DEFINE DATASET ds-taxa-conversao SERIALIZE-HIDDEN  FOR CurrencyMapExport, CurrencyConversionRateExport.

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

RUN pi-02-processa-cotacao.

ASSIGN jsonRetorno = NEW JsonArray().
       jsonRetorno:Read(TEMP-TABLE CurrencyConversionRateExport:HANDLE).

RUN createJsonResponse(INPUT jsonRetorno, INPUT TABLE RowErrors, INPUT FALSE, OUTPUT jsonOutput).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-02-processa-cotacao) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-02-processa-cotacao Procedure 
PROCEDURE pi-02-processa-cotacao :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR c-periodo    AS CHAR NO-UNDO.
DEF VAR c-siglas     AS CHAR NO-UNDO.
DEF VAR i-num-mes    AS INT  NO-UNDO.
DEF VAR i-num-sem    AS INT  NO-UNDO.
DEF VAR c-codigo     AS CHAR NO-UNDO.
DEF VAR c-cotacao    AS CHAR NO-UNDO.
DEF VAR c-dt-cotacao AS CHAR NO-UNDO.
DEF VAR dt-cotacao   AS DATE NO-UNDO.

DEFINE VARIABLE lista-mes AS CHARACTER FORMAT "x(20)"
    INITIAL "Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec".

DEFINE VARIABLE lista-semana AS CHARACTER FORMAT "x(20)"
    INITIAL "Sun,Mon,Tue,Wed,Thur,Fri,Sat".

EMPTY TEMP-TABLE CurrencyConversionRateExport.

c-periodo = STRING(YEAR(TODAY - 10),"9999") + STRING(MONTH(TODAY - 10),"99").

MESSAGE "c-periodo " c-periodo
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

FOR EACH cotacao WHERE
         cotacao.ano-periodo  >= c-periodo NO-LOCK:

    ASSIGN i-cont = 1.

    FIND FIRST moeda WHERE 
               moeda.mo-codigo = cotacao.mo-codigo NO-LOCK NO-ERROR.
    IF AVAIL moeda THEN 
        c-siglas = STRING(moeda.mo-codigo) + "-" + "BRL:" + moeda.sigla.
    ELSE NEXT.

    ASSIGN i-num-mes = INT(SUBSTRING(cotacao.ano-periodo,5,2)).
           

    REPEAT WHILE i-cont <= 31 :

        IF cotacao.cotacao[i-cont] <> 0 THEN DO:

            ASSIGN c-codigo     = STRING(cotacao.mo-codigo)
                   c-cotacao    = STRING(cotacao.cotacao[i-cont])
                   c-cotacao    = REPLACE(c-cotacao,",",".")
                   dt-cotacao   = DATE(STRING(i-cont,"99") + "/" + SUBSTRING(cotacao.ano-periodo,5,2) + "/" + SUBSTRING(cotacao.ano-periodo,1,4))
                   i-num-sem    = WEEKDAY(dt-cotacao)
                   //c-dt-cotacao = ENTRY(i-num-sem,lista-semana) + " " + STRING(i-cont,"99") + " " + ENTRY(i-num-mes,lista-mes) + " 00:00:00 PST " + SUBSTRING(cotacao.ano-periodo,1,4)
                   c-dt-cotacao = SUBSTRING(cotacao.ano-periodo,1,4) + SUBSTRING(cotacao.ano-periodo,5,2) + STRING(i-cont,"99").

            CREATE CurrencyConversionRateExport.
            ASSIGN FromCurrency = c-codigo
                   ToCurrency   = "0" 
                   UniqueName   = c-siglas 
                   Rate         = c-cotacao       
                   DATE         = c-dt-cotacao.

            
        END.

        ASSIGN i-cont = i-cont + 1.

    END.

END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


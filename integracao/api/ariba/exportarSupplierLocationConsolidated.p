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

DEF TEMP-TABLE SupplierLocationConsolidated NO-UNDO
    FIELD vendorID                 AS CHAR
    FIELD NAME                     AS CHAR
    FIELD City                     AS CHAR
    FIELD Phone                    AS CHAR
    FIELD Country                  AS CHAR
    FIELD PostalCode               AS CHAR
    FIELD Region                   AS CHAR
    FIELD EmailAddress             AS CHAR
    FIELD ContactName              AS CHAR
    FIELD Locale                   AS CHAR
    FIELD Street                   AS CHAR
    FIELD ContactID                AS CHAR
    FIELD PreferredOrderingMethod  AS CHAR INITIAL "Print".


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

RUN pi-02-processa.

IF CAN-FIND (FIRST tt-erros) THEN DO:

    MESSAGE "CLF ----> NOK".

    ASSIGN jsonRetorno = NEW JsonArray().
           jsonRetorno:Read(TEMP-TABLE tt-erros:HANDLE).
END.
ELSE DO:
    MESSAGE "CLF ----> OK".

    ASSIGN jsonRetorno = NEW JsonArray().
           jsonRetorno:Read(TEMP-TABLE SupplierLocationConsolidated:HANDLE).
END.
    

RUN createJsonResponse(INPUT jsonRetorno, INPUT TABLE RowErrors, INPUT FALSE, OUTPUT jsonOutput).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-02-processa) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-02-processa Procedure 
PROCEDURE pi-02-processa :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE cLocale AS CHARACTER INITIAL "pt_BR"   NO-UNDO.
DEFINE VARIABLE cPais   AS CHARACTER INITIAL ""        NO-UNDO.


DEFINE BUFFER b-es-fornecedor-ariba FOR es-fornecedor-ariba.


EMPTY TEMP-TABLE SupplierLocationConsolidated.


IF NOT CAN-FIND (FIRST es-fornecedor-ariba WHERE
                       es-fornecedor-ariba.cod-emitente <> 0 AND
                       es-fornecedor-ariba.enviado-SupplierLocationConsolid = NO NO-LOCK) THEN DO:
    CREATE tt-erros.
    ASSIGN tt-erros.cod-erro  = 1
           tt-erros.desc-erro = "NÆo existe itens para serem Integrados".

END.


FOR EACH es-fornecedor-ariba NO-LOCK
   WHERE es-fornecedor-ariba.cod-emitente <> 0    
     AND es-fornecedor-ariba.enviado-SupplierLocationConsolid = NO:

    FIND FIRST emitente NO-LOCK
         WHERE emitente.cod-emitente = es-fornecedor-ariba.cod-emitente  NO-ERROR.
    IF NOT AVAIL emitente THEN NEXT.

    ASSIGN cPais = "".
    FIND FIRST mguni.pais WHERE pais.nome-pais = emitente.pais NO-LOCK NO-ERROR.
    IF AVAIL mguni.pais THEN
        ASSIGN cPais = trim(substring(pais.char-1,23,02)).


    IF cPais <> "BR" THEN
        ASSIGN cLocale = "en_US".


    CREATE SupplierLocationConsolidated.
    ASSIGN SupplierLocationConsolidated.vendorID     = STRING(emitente.cod-emitente)
           SupplierLocationConsolidated.NAME         = emitente.nome-emit
           SupplierLocationConsolidated.City         = emitente.cidade
           SupplierLocationConsolidated.Country      = cPais
           SupplierLocationConsolidated.PostalCode   = STRING(emitente.cep,"99999-999")
           SupplierLocationConsolidated.Region       = emitente.estado
           SupplierLocationConsolidated.Locale       = cLocale
           SupplierLocationConsolidated.Street       = emitente.endereco
           SupplierLocationConsolidated.EmailAddress = emitente.e-mail
           SupplierLocationConsolidated.Phone        = emitente.telefone[1].
           

    FIND FIRST cont-emit OF emitente NO-LOCK NO-ERROR.
    IF AVAIL cont-emit THEN
        ASSIGN SupplierLocationConsolidated.ContactID    = cont-emit.e-mail
               SupplierLocationConsolidated.ContactName  = cont-emit.nome.
    ELSE
        ASSIGN SupplierLocationConsolidated.ContactID    = emitente.e-mail
               SupplierLocationConsolidated.ContactName  = es-fornecedor-ariba.nome-responsavel.    

    
    
    
    
    
    
    /*-- incluido para atualizar o status do registro --*/
    //FIND FIRST b-es-fornecedor-ariba EXCLUSIVE-LOCK
    //     WHERE ROWID(b-es-fornecedor-ariba) = ROWID(es-fornecedor-ariba)  NO-ERROR.
    //IF AVAIL b-es-fornecedor-ariba THEN
    //    ASSIGN es-fornecedor-ariba.enviado-SupplierLocationConsolid = YES.
    //
    //FIND CURRENT b-es-fornecedor-ariba NO-LOCK NO-ERROR.

    
END.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


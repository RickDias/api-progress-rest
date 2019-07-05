&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : importarpedcompra.p
    Purpose     : Integra�ao - ARIBA x Pedido de Compra

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

{include/i-prgvrs.i fornecedorb2e 2.00.00.000} /*** 010000 ***/

{cdp/cdcfgmat.i}

DEF TEMP-TABLE csvFornecedor NO-UNDO
    FIELD sourceSystem        AS CHAR SERIALIZE-NAME "sourceSystem"
    FIELD erpVendorId         AS CHAR SERIALIZE-NAME "erpVendorId" 
    FIELD IE                  AS CHAR SERIALIZE-NAME "IE"          
    FIELD Street              AS CHAR SERIALIZE-NAME "Street"      
    FIELD Number              AS CHAR SERIALIZE-NAME "Number"      
    FIELD Complement          AS CHAR SERIALIZE-NAME "Complement"  
    FIELD ZipCode             AS CHAR SERIALIZE-NAME "ZipCode"     
    FIELD District            AS CHAR SERIALIZE-NAME "District"    
    FIELD Municipality        AS CHAR SERIALIZE-NAME "Municipality"
    FIELD State               AS CHAR SERIALIZE-NAME "State"       
    FIELD Country             AS CHAR SERIALIZE-NAME "Country"     
    FIELD SINTEGRA            AS CHAR SERIALIZE-NAME "SINTEGRA"    
    FIELD CNPJAtivo           AS CHAR SERIALIZE-NAME "CNPJAtivo"   
    FIELD Simples             AS CHAR SERIALIZE-NAME "Simples"    
    FIELD ScoreAceito         AS CHAR SERIALIZE-NAME "ScoreAceito" 
    FIELD CNAE                AS CHAR SERIALIZE-NAME "CNAE"        
    FIELD Mensagem            AS CHAR SERIALIZE-NAME "Mensagem"    
    FIELD Parecer             AS CHAR SERIALIZE-NAME "Parecer"     
    FIELD Motivo              AS CHAR SERIALIZE-NAME "Motivo"
    INDEX emitente_id IS PRIMARY UNIQUE erpVendorId.


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


DEFINE VARIABLE l-codigo-icm-sensitive   AS LOGICAL NO-UNDO.
DEFINE VARIABLE l-aliquota-icm-sensitive AS LOGICAL NO-UNDO.
DEFINE VARIABLE l-aliquota-iss-sensitive AS LOGICAL NO-UNDO.
DEFINE VARIABLE l-valor-taxa-sensitive   AS LOGICAL NO-UNDO.
DEFINE VARIABLE l-taxa-financ-sensitive  AS LOGICAL NO-UNDO.
DEFINE VARIABLE l-possui-reaj-sensitive  AS LOGICAL NO-UNDO.

DEFINE VARIABLE i-cont AS INT INIT 1 NO-UNDO.


DEFINE VARIABLE c-end-cobranca-aux LIKE pedido-compr.end-cobranca NO-UNDO.
DEFINE VARIABLE c-end-entrega-aux  LIKE pedido-compr.end-entrega NO-UNDO.
DEFINE VARIABLE i-cod-mensagem     LIKE pedido-compr.cod-mensagem NO-UNDO.

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

 FIND FIRST param-global NO-LOCK NO-ERROR.

/*-- validar um novo usuario para colocar neste parametro --*/


RUN btb/btapi910ze.p   (INPUT "tcpasilva",       /*USUARIO*/
                        INPUT "",                /*SENHA*/
                        INPUT "1",               /*EMPRESA*/
                        OUTPUT TABLE tt-erros).  /*RETORNO DE ERROSl*/

//{utp/ut-api-action.i pi-00-get GET /~*}


{utp/ut-api-action.i pi-getAll GET /~*}
{utp/ut-api-notfound.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-pi-getAll) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-getAll Procedure 
PROCEDURE pi-getAll :
/*------------------------------------------------------------------------------
  Purpose: retorna as informacoes dos fornecedores B2E    
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER jsonInput   AS JsonObject NO-UNDO.
    DEFINE OUTPUT PARAMETER jsonOutput  AS JsonObject NO-UNDO.

    DEFINE VARIABLE jsonFornecedores AS JsonArray NO-UNDO.

    EMPTY TEMP-TABLE csvFornecedor.
    EMPTY TEMP-TABLE RowErrors.

    //Rotina para carregar os fornecedores
    RUN pi-load-providers.

    ASSIGN jsonFornecedores = NEW JsonArray(). 
           jsonFornecedores:READ(TEMP-TABLE csvFornecedor:HANDLE). 


    jsonFornecedores:WriteFile(SUBSTITUTE("/totvs/erp/camil/teste-rosa/log_appserver/jsonFornecedores-BF-&1.json",STRING(TIME))).

    RUN createJsonResponse(INPUT jsonFornecedores, INPUT TABLE RowErrors, INPUT FALSE, OUTPUT jsonOutput).

    jsonFornecedores:WriteFile(SUBSTITUTE("/totvs/erp/camil/teste-rosa/log_appserver/jsonFornecedores-AF-&1.json",STRING(TIME))).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-load-providers) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-load-providers Procedure 
PROCEDURE pi-load-providers :
/*------------------------------------------------------------------------------
  Purpose: Carrega na ttcsvFornecedor os fornecedores cadastrados no ultimos
           30 dias    
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE c-source-codepage AS CHARACTER INITIAL "utf-8"  NO-UNDO.
    
    FOR EACH emitente 
       WHERE emitente.identific > 1 /*-- n�o exportar cliente --*/
         AND emitente.data-implant >= TODAY - 30 NO-LOCK:

        FIND LAST es-fornecedor-ariba WHERE
                  es-fornecedor-ariba.cod-emitente = emitente.cod-emitente NO-LOCK NO-ERROR.

        CREATE csvFornecedor.
        ASSIGN csvFornecedor.sourceSystem      = "SAP"
               csvFornecedor.erpVendorId       = STRING(emitente.cod-emitente)
               csvFornecedor.IE                = STRING(emitente.ins-estadual)
               csvFornecedor.Street            = emitente.endereco
               csvFornecedor.Number            = ""
               csvFornecedor.Complement        = emitente.endereco2
               csvFornecedor.ZipCode           = STRING(emitente.cep,param-global.formato-cep)
               csvFornecedor.District          = emitente.bairro
               csvFornecedor.Municipality      = emitente.cidade
               csvFornecedor.State             = emitente.estado
               csvFornecedor.Country           = emitente.pais
               csvFornecedor.SINTEGRA          = STRING(es-fornecedor-ariba.sintegra)
               csvFornecedor.CNPJAtivo         = STRING(es-fornecedor-ariba.CNPJAtivo)
               csvFornecedor.Simples           = STRING(es-fornecedor-ariba.Simples-Nacional)
               csvFornecedor.ScoreAceito       = STRING(es-fornecedor-ariba.ScoreAceito)
               csvFornecedor.CNAE              = STRING(es-fornecedor-ariba.CNAE)
               csvFornecedor.Mensagem          = CODEPAGE-CONVERT(es-fornecedor-ariba.mensagem,SESSION:CHARSET,c-source-codepage)
               csvFornecedor.Parecer           = CODEPAGE-CONVERT(es-fornecedor-ariba.parecer,SESSION:CHARSET,c-source-codepage)
               csvFornecedor.Motivo            = CODEPAGE-CONVERT(es-fornecedor-ariba.motivo,SESSION:CHARSET,c-source-codepage).

    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


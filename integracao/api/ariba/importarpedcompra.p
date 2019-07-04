&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : importarpedcompra.p
    Purpose     : Integraáao - ARIBA x Pedido de Compra

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

/*DIRETORIO ESPECIFICO*/
{ccp/ccapi202.i}
{ccp/ccapi203.i}
{ccp/ccapi205.i}
{ccp/ccapi207.i}

//{include/i-prgvrs.i paymentTerms 2.00.00.000} /*** 010000 ***/
DEFINE TEMP-TABLE tt-versao-integr 
    FIELD cod-versao-integracaoo AS INTEGER
    FIELD ind-origem-msg         AS INTEGER.

DEF TEMP-TABLE tt-imp-pedido-compr NO-UNDO SERIALIZE-NAME "Pedido_Compra"
    FIELD num-pedido            AS CHAR                                                 //n£mero do Pedido de Compras                                            
    FIELD num-pedido-totvs      AS INT                                                  //N£mero do Pedido de Compras Totvs 
    FIELD Cod-estabel           AS CHAR                                                 //C¢digo do Estabelecimento 
    FIELD Cod-emitente          AS CHAR                                                 //C¢digo de Emitente 
    FIELD Cod-cond-pag          AS CHAR                                                 //C¢digo da Candiá∆o de Pagamento 
    FIELD Natureza              AS INT                                                  //Natureza de operaá∆o 
    FIELD data-pedido           AS DATE                                                 //Data de implantaá∆o do Pedido
    FIELD Situacao              AS INT                                                  //Situaá∆o do Pedido - 
    FIELD Frete                 AS CHAR
    FIELD Cod-transp            AS INT      
    FIELD Responsavel           AS CHAR     
    FIELD Impr-pedido           AS LOGICAL  
    FIELD Comentarios           AS CHAR     
    FIELD Mot-elimina           AS CHAR     
    FIELD Emergencial           AS LOGICAL  
    FIELD Contr-forn            AS LOGICAL  
    FIELD Compl-entrega         AS CHAR     
    FIELD End-entrega           AS CHAR     
    FIELD End-cobranca          AS CHAR     
    FIELD Via-transp            AS INT
    FIELD data-pgto-1           AS DATE
    FIELD data-pgto-2           AS DATE
    FIELD data-pgto-3           AS DATE
    FIELD perc-pgto-1           AS DECIMAL
    FIELD perc-pgto-2           AS DECIMAL
    FIELD perc-pgto-3           AS DECIMAL
    FIELD num-processo          AS INTEGER FORMAT "999999999"
    FIELD num-sequencia         AS INTEGER FORMAT "999999"
    FIELD ind-tipo-movto        AS INTEGER FORMAT "99" INITIAL 1.


DEF TEMP-TABLE tt-imp-ordem-compra NO-UNDO SERIALIZE-NAME "Ordem_Compra"
    FIELD  Numero-ordem    AS INT
    FIELD  sequencia       AS INT
    FIELD  cod-estabel     AS CHAR
    FIELD  Num-pedido      AS CHAR
    FIELD  cod-emitente    AS INT
    FIELD  it-codigo       AS CHAR
    FIELD  cod-comprado    AS CHAR
    FIELD  nr-contrato     AS INT
    FIELD  requisitante    AS CHAR
    FIELD  dep-almoxar     AS CHAR
    FIELD  situacao        AS INT
    FIELD  preco-fornec    AS DEC
    FIELD  qt-solic        AS DEC
    FIELD  CT-CODIGO       AS CHAR
    FIELD  SC-CODIGO       AS INT
    FIELD  tp-despesa      AS INT
    FIELD  data-emissao    AS DATE
    FIELD  ordem-servic    AS INT
    FIELD  natureza        AS INT
    FIELD  narrativa       AS CHAR
    FIELD  unidade-negocio AS CHAR
    FIELD  num-ord-inv     AS INT
    FIELD  pre-unit-for    AS DEC
    FIELD  mo-codigo       AS CHAR
    FIELD  Codigo-ipi      AS LOGICAL
    FIELD  Aliquota-ipi    AS DEC
    FIELD  Codigo-icm      AS INT
    FIELD  Aliquota-icm    AS DEC
    FIELD  Aliquota-iss    AS DEC
    FIELD  Valor-frete     AS DEC
    FIELD  cod-cond-pag    AS CHAR
    FIELD  Usuario         AS CHAR
    FIELD num-processo     AS INTEGER FORMAT "999999999"
    FIELD num-sequencia    AS INTEGER FORMAT "999999"
    FIELD ind-tipo-movto   AS INTEGER FORMAT "99".

DEF TEMP-TABLE tt-imp-prazo-compra NO-UNDO SERIALIZE-NAME "Prazo_Compra"
    FIELD Numero-ordem          AS INT
    FIELD sequencia             AS INT
    FIELD Num-pedido            AS CHAR
    FIELD Data-entrega          AS DATE
    FIELD Natureza              AS INT
    FIELD un                    AS CHAR
    //FIELD Parcela               AS INT   
    FIELD it-codigo             AS CHAR   
    FIELD Quantidade            AS DEC   
    FIELD Quant-saldo           AS DEC
    FIELD Quantid-orig          AS DEC
    FIELD qtd-sal-forn          AS DEC
    FIELD qtd-do-forn           AS DEC
    FIELD num-processo          AS INTEGER FORMAT "999999999"
    FIELD num-sequencia         AS INTEGER FORMAT "999999"
    FIELD ind-tipo-movto        AS INTEGER FORMAT "99".


DEF TEMP-TABLE tt-imp-cotacao NO-UNDO SERIALIZE-NAME "Cotacao_Item"
    FIELD Numero-ordem          AS INT
    FIELD sequencia             AS INT
    FIELD Num-pedido            AS CHAR
    FIELD cod-emitente          AS INT
    FIELD it-codigo             AS CHAR
    FIELD Cod-transp            AS INT
    FIELD Preco-fornec          AS DEC
    FIELD Contato               AS CHAR
    FIELD mo-codigo             AS CHAR
    FIELD Data-cotacao          AS DATE
    FIELD Preco-unit            AS DEC
    FIELD pre-unit-for          AS DEC
    FIELD Codigo-ipi            AS LOGICAL
    FIELD Aliquota-ipi          AS DEC
    FIELD Codigo-icm            AS INT
    FIELD Aliquota-icm          AS DEC
    FIELD Aliquota-iss          AS DEC
    FIELD Frete                 AS LOGICAL
    FIELD Valor-frete           AS DEC
    FIELD Taxa-financ           AS LOGICAL
    FIELD Valor-taxa            AS DEC
    FIELD Perc-descto           AS DEC
    FIELD cod-cond-pag          AS CHAR
    FIELD Prazo-entreg          AS INT
    FIELD Narrativa             AS CHAR
    FIELD Itinerario            AS INT
    FIELD num-processo          AS INTEGER FORMAT "999999999"
    FIELD num-sequencia         AS INTEGER FORMAT "999999"
    FIELD ind-tipo-movto        AS INTEGER FORMAT "99".

DEF TEMP-TABLE tt-imp-pedido-compr-retorno NO-UNDO SERIALIZE-NAME "Pedido_Compra_Retorno"
    FIELD num-pedido            AS INT      
    FIELD vl-pedido             AS DECIMAL     
    FIELD qt-pedido             AS DECIMAL     
    FIELD dt-inclusao           AS DATETIME
    FIELD NumberInCollection    AS CHAR
    FIELD SAPPOLineNumber       AS CHAR
    FIELD UniqueName            AS CHAR.

DEF TEMP-TABLE tt-retorno-nok NO-UNDO SERIALIZE-NAME "Retorno"
    FIELD data                  AS DATETIME
    FIELD cod-erro              AS INT
    FIELD sequencia             AS INT     
    FIELD desc-erro             AS CHAR
    FIELD UniqueName            AS CHAR.

DEFINE TEMP-TABLE tt-erros-geral 
    FIELD identif-msg        AS CHARACTER 
    FIELD num-sequencia-erro AS INTEGER
    FIELD cod-erro           AS INTEGER
    FIELD des-erro           AS CHARACTER FORMAT "X(200)"
    FIELD cod-maq-origem     AS INTEGER 
    FIELD num-processo       AS INTEGER.


DEFINE DATASET httPedidoCompra SERIALIZE-HIDDEN  FOR tt-imp-pedido-compr, tt-imp-ordem-compra, tt-imp-prazo-compra
    DATA-RELATION dr-OrdemCompra FOR tt-imp-pedido-compr, tt-imp-ordem-compra
       RELATION-FIELDS (num-pedido, num-pedido)  NESTED   
    DATA-RELATION dr-PrazoCompra FOR tt-imp-ordem-compra, tt-imp-prazo-compra
       RELATION-FIELDS (it-codigo, it-codigo) NESTED .

/*ENVIO PEDIDO DE COMPRA*/
DEF TEMP-TABLE tt-po-ariba NO-UNDO SERIALIZE-NAME "Pedido_Compra"
    FIELD payloadID                           AS CHAR
    FIELD timestamp                           AS DATETIME
    field from-domain-network                 as CHAR
    field to-domain-network                   as CHAR
    field to-domain-buyersystemid             as CHAR
    field sender-domain                       as CHAR
    field sender-useragent                    as CHAR
    field Type                                as CHAR
    FIELD orderType                           as CHAR
    FIELD orderVersion                        as CHAR
    field orderDate                           as DATETIME
    field orderID                             as CHAR
    field Total-Money                         as CHAR
    field Total-currency                      as CHAR
    field ShipTo-isoCountryCode               as CHAR
    field ShipTo-addressID                    as CHAR
    field ShipTo-Name                         as CHAR
    field ShipTo-DeliverTo                    as char
    field ShipTo-Street                       as char
    field ShipTo-City                         as char
    field ShipTo-State                        as char
    field ShipTo-PostalCode                   as char
    field ShipTo-Country                      as char
    field ShipTo-Email                        as char
    field ShipTo-CountryCode                  as char  
    field ShipTo-AreaorCityCode               as char
    field ShipTo-Number                       as char
    field BillTo-addressID                    as char
    field BillTo-Name                         as char
    //field BillTo-DeliverTo                    as char
    field BillTo-Street                       as char
    field BillTo-City                         as char
    field BillTo-State                        as char
    field BillTo-PostalCode                   as char
    field BillTo-Country                      as char
    //field BillTo-Email                        as char
    field BillTo-CountryCode                  as char
    field BillTo-AreaorCityCode               as char
    field BillTo-Number                       as char
    field PurchasingUnit                      as char
    field PUName                              as char.

DEF TEMP-TABLE tt-it-po-ariba NO-UNDO SERIALIZE-NAME "Ordem_Compra"
    field orderID                     AS CHAR
    FIELD isAdHoc                     AS CHAR
    field Quantity                    as char
    field LineNumber                  as char
    field SupplierPartID              as char
    field Money                       as char
    field Currency                    as char
    field C-Description               as char
    field UnitOfMeasure               as char
    field Classification              as char
    field LeadTime                    as char
    field ReqLineNo                   as char
    field Requester                   as char
    field PRNo                        as char.

DEFINE DATASET ds-po-ariba SERIALIZE-HIDDEN  FOR tt-po-ariba, tt-it-po-ariba
    DATA-RELATION dr-po-ariba FOR tt-po-ariba, tt-it-po-ariba
    RELATION-FIELDS (orderID, orderID)  NESTED.

DEF TEMP-TABLE tt-erros
    FIELD cod-erro  AS INTEGER
    FIELD desc-erro AS CHARACTER FORMAT "x(256)"
    FIELD desc-arq  AS CHARACTER.

//DEFINE VARIABLE c-transacao-global AS CHARACTER INITIAL 'a'  NO-UNDO.

//DEFINE VARIABLE l-ok                  AS LOGICAL     NO-UNDO.
//DEFINE VARIABLE l-xml                 AS LOGICAL     NO-UNDO.
//DEFINE VARIABLE c-arquivo-xsd         AS CHARACTER   NO-UNDO.
//DEFINE VARIABLE i-count               AS INTEGER     NO-UNDO.


/*
//SIT
DEFINE TEMP-TABLE ttError      SERIALIZE-NAME "Retorno"
    FIELD SessionId         AS CHARACTER
    FIELD referencia        AS CHARACTER
    FIELD codigo            AS CHARACTER
    FIELD descricao         AS CHARACTER
    INDEX idx01 referencia codigo.
*/


DEFINE BUFFER estabelec-entrega     FOR estabelec.
DEFINE BUFFER estabelec-faturamento FOR estabelec.


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
DEFINE VARIABLE lLoop AS LOG NO-UNDO.
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


DEFINE VARIABLE h-boin295   AS HANDLE     NO-UNDO.
DEFINE VARIABLE h-boin274sd AS HANDLE     NO-UNDO.
DEFINE VARIABLE h-boin274vl AS HANDLE     NO-UNDO.
DEFINE VARIABLE h-esapi002  AS HANDLE     NO-UNDO. 
DEFINE VARIABLE c-url       AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-token     AS CHARACTER  NO-UNDO.
DEFINE VARIABLE v-longchar  AS LONGCHAR   NO-UNDO. 
DEFINE VARIABLE c-texto     AS CHARACTER  NO-UNDO.
DEFINE VARIABLE client      AS COM-HANDLE NO-UNDO.

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

DEFINE VARIABLE oRequestParser AS JsonAPIRequestParser NO-UNDO.
DEFINE VARIABLE jsonRetorno AS JsonArray NO-UNDO.

oRequestParser = NEW JsonAPIRequestParser(jsonInput).
 
ASSIGN json_recebido = oRequestParser:getPayloadLongChar().

MESSAGE ">>>>>>" STRING(json_recebido).

OUTPUT TO /totvs/erp/camil/teste-rosa/log_appserver/jsonrecebidopedido.json.

    EXPORT json_recebido.

OUTPUT CLOSE.

lretOK = DATASET httPedidoCompra:READ-JSON("LONGCHAR", json_recebido, "empty") NO-ERROR.

MESSAGE "lretOK " lretOK
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

IF lretOK = NO THEN DO:
    
    CREATE tt-retorno-nok.
    ASSIGN tt-retorno-nok.data        = NOW
           tt-retorno-nok.cod-erro    = 0
           tt-retorno-nok.desc-erro   = "N∆o foi poss°vel fazer o parse do arquivo"
           tt-retorno-nok.sequencia   = 1.
    
END.
ELSE
    // Validar os registros e processar 
    RUN pi-01-valida.
    
// Retornar os dados da integracao
//RUN pi-03-retorno.

IF CAN-FIND (FIRST tt-retorno-nok NO-LOCK) THEN DO:
    ASSIGN jsonRetorno = NEW JsonArray().
           jsonRetorno:Read(TEMP-TABLE tt-retorno-nok:HANDLE).
END.
ELSE DO:
    ASSIGN jsonRetorno = NEW JsonArray().
           jsonRetorno:Read(TEMP-TABLE tt-imp-pedido-compr-retorno:HANDLE).
END.

RUN createJsonResponse(INPUT jsonRetorno, INPUT TABLE RowErrors, INPUT FALSE, OUTPUT jsonOutput).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-01-valida) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-01-valida Procedure 
PROCEDURE pi-01-valida :
/*------------------------------------------------------------------------------
  Purpose: Localizar dados da integraá∆o para validar as execuá‰es     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
EMPTY TEMP-TABLE tt-retorno-nok.

IF CAN-FIND (FIRST tt-imp-pedido-compr) THEN DO:
    FOR FIRST tt-imp-pedido-compr.

        IF tt-imp-pedido-compr.frete = ?  OR 
           tt-imp-pedido-compr.frete = "" THEN DO:

            CREATE tt-retorno-nok.
            ASSIGN tt-retorno-nok.data        = NOW
                   tt-retorno-nok.cod-erro    = 99
                   tt-retorno-nok.desc-erro   = "IncoTerms est† em branco."
                   tt-retorno-nok.sequencia   = 1
                   tt-retorno-nok.UniqueName  = tt-imp-pedido-compr.num-pedido.

        END.
            


        /*INCLUS«O*/
        ASSIGN tt-imp-pedido-compr.ind-tipo-movto = 1.

        // Validar a operaá∆o
        CASE tt-imp-pedido-compr.ind-tipo-movto:
            WHEN 1 THEN DO: // Inclusao de registro

                // Verificar se o codigo do emitente j† existe cadastrado 
                IF tt-imp-pedido-compr.num-pedido-totvs <> 0 THEN DO:
                    FIND FIRST pedido-compr NO-LOCK
                        WHERE STRING(pedido-compr.num-pedido) = tt-imp-pedido-compr.num-pedido NO-ERROR.
                    IF AVAIL pedido-compr THEN DO:
                            CREATE tt-retorno-nok.
                            ASSIGN tt-retorno-nok.data        = NOW
                                   tt-retorno-nok.cod-erro    = 1
                                   tt-retorno-nok.desc-erro   = "Pedido de Compra j† existe com o n£mero " + STRING(tt-imp-pedido-compr.num-pedido)
                                   tt-retorno-nok.sequencia   = 1.

                    END.
                END.
            END.
            WHEN 2 THEN DO:
                FIND FIRST pedido-compr NO-LOCK
                    WHERE STRING(pedido-compr.num-pedido) = tt-imp-pedido-compr.num-pedido NO-ERROR.
                IF NOT AVAIL pedido-compr THEN DO:
                    CREATE tt-retorno-nok.
                    ASSIGN tt-retorno-nok.data        = NOW
                           tt-retorno-nok.cod-erro    = 2
                           tt-retorno-nok.desc-erro   = "Ordem de Compra n∆o localizada com o n£mero " + STRING(tt-imp-pedido-compr.num-pedido)
                           tt-retorno-nok.sequencia   = 1.

                END.
            END.
            WHEN 3 THEN DO:
                FIND FIRST pedido-compr NO-LOCK
                    WHERE STRING(pedido-compr.num-pedido) = tt-imp-pedido-compr.num-pedido NO-ERROR.
                IF NOT AVAIL pedido-compr THEN DO:
                    CREATE tt-retorno-nok.
                    ASSIGN tt-retorno-nok.data        = NOW
                           tt-retorno-nok.cod-erro    = 3
                           tt-retorno-nok.desc-erro   = "Ordem de Compra n∆o localizada com o n£mero " + STRING(tt-imp-pedido-compr.num-pedido)
                           tt-retorno-nok.sequencia   = 1.

                END.
            END.
            OTHERWISE DO:
                CREATE tt-retorno-nok.
                ASSIGN tt-retorno-nok.data        = NOW
                       tt-retorno-nok.cod-erro    = 4
                       tt-retorno-nok.desc-erro   = "N∆o identificada a operaá∆o a ser realizada na TAG <ind-tipo-movto>."
                       tt-retorno-nok.sequencia   = 1.
                
            END.
        END CASE.

        // Caso n∆o localize os erros, processar a integraá∆o 
        IF NOT CAN-FIND(FIRST tt-retorno-nok) THEN DO:

            RUN pi-02-processa.

        END.
            
    END.

END.
ELSE DO:

    CREATE tt-retorno-nok.
    ASSIGN tt-retorno-nok.data        = NOW
           tt-retorno-nok.cod-erro    = 5
           tt-retorno-nok.desc-erro   = "Dados invalidos no XML/JSON"
           tt-retorno-nok.sequencia   = 1.
    
END.
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
DEF VAR d-total-pedido AS DECIMAL NO-UNDO.
DEF VAR d-total-item   AS DECIMAL NO-UNDO.

DO TRANSACTION ON ERROR UNDO, RETURN ON STOP UNDO, RETURN:
    
    CREATE tt-versao-integr.
    ASSIGN tt-versao-integr.cod-versao-integracao = 001
           tt-versao-integr.ind-origem-msg        = 01.

    FOR FIRST tt-imp-pedido-compr EXCLUSIVE-LOCK:

        EMPTY TEMP-TABLE tt-pedido-compr.
        EMPTY TEMP-TABLE tt-prazo-compra.
        EMPTY TEMP-TABLE tt-ordem-compra.
        EMPTY TEMP-TABLE tt-cotacao-item. 
        RUN pi-04-geraPedidoCompra.

        FOR EACH tt-imp-ordem-compra EXCLUSIVE-LOCK:

            RUN pi-05-geraOrdemCompra.
    
            FOR EACH tt-imp-prazo-compra WHERE
                     tt-imp-prazo-compra.sequencia  = tt-imp-ordem-compra.sequencia  NO-LOCK:

                RUN pi-06-geraPrazoCompra.
    
            END.

            RUN pi-07-geraCotacaoItem.

            IF tt-imp-ordem-compra.cod-cond-pag = "0" THEN
                RUN pi-08-geraCondPgto.

            
        END.

        RUN ccp/ccapi303.p (INPUT  TABLE tt-versao-integr,
                            OUTPUT TABLE tt-erros-geral,
                            INPUT  TABLE tt-pedido-compr,
                            INPUT  TABLE tt-cond-especif,
                            INPUT  TABLE tt-ordem-compra,
                            INPUT  TABLE tt-prazo-compra,
                            INPUT  TABLE tt-cotacao-item,
                            INPUT  TABLE tt-desp-cotacao-item).

        FIND FIRST tt-erros-geral NO-LOCK NO-ERROR.
        IF AVAIL tt-erros-geral THEN
            RUN pi-08-log-erros.
        ELSE DO:


            RUN pi-10-calc-total-pedido (INPUT  tt-imp-pedido-compr.num-pedido-totvs,
                                         OUTPUT d-total-item,
                                         OUTPUT d-total-pedido).

            CREATE tt-imp-pedido-compr-retorno.
            ASSIGN tt-imp-pedido-compr-retorno.num-pedido         = tt-imp-pedido-compr.num-pedido-totvs
                   tt-imp-pedido-compr-retorno.dt-inclusao        = NOW
                   tt-imp-pedido-compr-retorno.qt-pedido          = d-total-item
                   tt-imp-pedido-compr-retorno.vl-pedido          = d-total-pedido
                   tt-imp-pedido-compr-retorno.NumberInCollection = "1"
                   tt-imp-pedido-compr-retorno.SAPPOLineNumber    = "00010"
                   tt-imp-pedido-compr-retorno.UniqueName         = STRING(tt-imp-pedido-compr.num-pedido).

            FOR EACH tt-imp-ordem-compra NO-LOCK:

                RUN pi-09-geraUnidNegoc.

            END.

            IF NOT CAN-FIND(FIRST sfa-export WHERE sfa-export.cd-tipo-integr = 20
                                               AND sfa-export.chave          = STRING(tt-imp-pedido-compr.num-pedido-totvs)
                                               AND sfa-export.ind-situacao   < 2) THEN DO: 
    
                CREATE sfa-export-ped-ariba.
                ASSIGN sfa-export-ped-ariba.cd-tipo-integr = 20
                       sfa-export-ped-ariba.id-movto       = NEXT-VALUE(seq-export)
                       sfa-export-ped-ariba.num-pedido     = tt-imp-pedido-compr.num-pedido-totvs
                       sfa-export-ped-ariba.data-movto     = NOW
                       sfa-export-ped-ariba.tp-integracao  = 1
                       sfa-export-ped-ariba.c-json         = ?.
        
                CREATE sfa-export.
                ASSIGN sfa-export.ind-tipo-trans = 2
                       sfa-export.id-movto       = sfa-export-ped-ariba.id-movto
                       sfa-export.cd-tipo-integr = sfa-export-ped-ariba.cd-tipo-integr
                       sfa-export.chave          = STRING(sfa-export-ped-ariba.num-pedido)
                       sfa-export.cod-status     = 0      /* ---- sem status ----*/
                       sfa-export.data-fim       = ?
                       sfa-export.data-inicio    = ?
                       sfa-export.data-movto     = NOW
                       sfa-export.ind-situacao   = 1       /*---- Pendente -----*/.
    
            END.

            
        END.
        
        
    END.
    
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-03-retorno) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-03-retorno Procedure 
PROCEDURE pi-03-retorno :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

IF CAN-FIND (FIRST tt-retorno-nok NO-LOCK) THEN
    TEMP-TABLE tt-retorno-nok:WRITE-JSON("LONGCHAR", json_retorno, TRUE).
ELSE
    TEMP-TABLE tt-imp-pedido-compr-retorno:WRITE-JSON("LONGCHAR", json_retorno, TRUE).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-04-geraPedidoCompra) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-04-geraPedidoCompra Procedure 
PROCEDURE pi-04-geraPedidoCompra :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    IF NOT VALID-HANDLE(h-boin295) THEN
        RUN inbo/boin295.p PERSISTENT SET h-boin295.

    RUN geraNumeroPedidoCompra IN h-boin295 (OUTPUT i-num-pedido).

    CREATE tt-pedido-compr.
    ASSIGN tt-pedido-compr.num-pedido    =  i-num-pedido
           tt-pedido-compr.char-2        =  tt-imp-pedido-compr.num-pedido
           tt-pedido-compr.cod-estabel   =  tt-imp-pedido-compr.cod-estabel
           tt-pedido-compr.cod-emitente  =  INT(tt-imp-pedido-compr.cod-emitente)
           tt-pedido-compr.cod-emit-terc =  INT(tt-imp-pedido-compr.cod-emitente)
           tt-pedido-compr.cod-cond-pag  =  INT(tt-imp-pedido-compr.cod-cond-pag)
           tt-pedido-compr.natureza      =  tt-imp-pedido-compr.natureza     
           tt-pedido-compr.data-pedido   =  tt-imp-pedido-compr.data-pedido  
           tt-pedido-compr.situacao      =  2                                        /*FIXO*/
           //tt-pedido-compr.frete         =  tt-imp-pedido-compr.frete        
           //tt-pedido-compr.cod-transp    =  tt-imp-pedido-compr.cod-transp
           tt-pedido-compr.responsavel   =  tt-imp-pedido-compr.responsavel  
           tt-pedido-compr.impr-pedido   =  YES
           tt-pedido-compr.comentarios   =  tt-imp-pedido-compr.comentarios  
           tt-pedido-compr.mot-elimina   =  tt-imp-pedido-compr.mot-elimina  
           tt-pedido-compr.emergencial   =  YES
           tt-pedido-compr.contr-forn    =  tt-imp-pedido-compr.contr-forn   
           tt-pedido-compr.compl-entrega =  tt-imp-pedido-compr.compl-entrega
           tt-pedido-compr.end-entrega   =  tt-imp-pedido-compr.end-entrega  
           tt-pedido-compr.end-cobranca  =  tt-imp-pedido-compr.end-cobranca 
           //tt-pedido-compr.via-transp    =  tt-imp-pedido-compr.via-transp
           tt-pedido-compr.cod-mensagem  =  100.                         /*VERIFICA*/

    IF tt-imp-pedido-compr.frete = "CIF" THEN
        ASSIGN tt-pedido-compr.frete = 1.
    ELSE
        ASSIGN tt-pedido-compr.frete = 2.

    FOR FIRST emitente WHERE
              emitente.cod-emitente = INT(tt-imp-pedido-compr.cod-emitente) NO-LOCK:
    
        ASSIGN tt-pedido-compr.cod-transp     = emitente.cod-transp
               tt-imp-pedido-compr.cod-transp = emitente.cod-transp.
        
        FIND FIRST transporte WHERE
                   transporte.cod-transp = emitente.cod-transp NO-LOCK NO-ERROR.
        IF AVAIL transporte THEN
            ASSIGN tt-pedido-compr.via-transp = transporte.via-transp.
    
    END.

    /*
    FIND FIRST tt-imp-ordem-compra WHERE
               tt-imp-ordem-compra.num-pedido = tt-imp-pedido-compr.num-pedido NO-LOCK NO-ERROR.
    IF AVAIL tt-imp-ordem-compra THEN DO:
    
        FIND FIRST item-uni-estab WHERE
                   item-uni-estab.it-codigo = tt-imp-ordem-compra.it-codigo NO-LOCK NO-ERROR.
        IF AVAIL item-uni-estab THEN
            //ASSIGN tt-pedido-compr.responsavel = item-uni-estab.cod-comprado.
    
        ELSE DO:
    
            FIND FIRST ITEM WHERE
                       ITEM.it-codigo = tt-imp-ordem-compra.it-codigo NO-LOCK NO-ERROR.
            IF AVAIL ITEM THEN
                ASSIGN tt-pedido-compr.responsavel = ITEM.cod-comprado.
    
        END.
    
    END.
    */


    /*
    FIND FIRST emitente WHERE
               emitente.cod-emitente = INT(tt-imp-pedido-compr.cod-emitente) NO-LOCK NO-ERROR.
    IF AVAIL emitente THEN DO:
    
        ASSIGN tt-pedido-compr.cod-transp  = emitente.cod-transp
               tt-imp-pedido-compr.via-transp = emitente.cod-transp.
    
        FIND FIRST transporte WHERE 
                   transporte.cod-transp = emitente.cod-transp NO-LOCK NO-ERROR.
        IF AVAIL transporte THEN
            ASSIGN tt-pedido-compr.via-transp   = transporte.via-transp
                   tt-imp-pedido-compr.via-transp  = transporte.via-transp.
    
    END.
    */

    ASSIGN //tt-imp-pedido-compr.num-pedido       = i-num-pedido
           tt-imp-pedido-compr.num-pedido-totvs = i-num-pedido.
    
    IF VALID-HANDLE(h-boin295) THEN DO:
        DELETE PROCEDURE h-boin295.
        ASSIGN h-boin295 = ?.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-05-geraOrdemCompra) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-05-geraOrdemCompra Procedure 
PROCEDURE pi-05-geraOrdemCompra :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEF BUFFER b-tt-ordem-compra FOR tt-ordem-compra.
    
    CREATE tt-ordem-compra.
    ASSIGN tt-ordem-compra.num-pedido     = tt-imp-pedido-compr.num-pedido-totvs
           tt-ordem-compra.sequencia      = tt-imp-ordem-compra.sequencia
           tt-ordem-compra.cod-estabel    = tt-imp-ordem-compra.cod-estabel 
           tt-ordem-compra.ep-codigo      = "1"
           //tt-ordem-compra.sequencia      = 10
           tt-ordem-compra.cod-emitente   = INT(tt-imp-pedido-compr.cod-emitente)
           tt-ordem-compra.it-codigo      = tt-imp-ordem-compra.it-codigo   
           tt-ordem-compra.cod-comprado   = tt-imp-ordem-compra.requisitante
           tt-ordem-compra.cod-unid-negoc = tt-imp-ordem-compra.unidade-negocio
           tt-ordem-compra.dep-almoxar    = tt-imp-ordem-compra.dep-almoxar 
           tt-ordem-compra.situacao       = 2                                       /*FIXO*/
           tt-ordem-compra.preco-fornec   = tt-imp-ordem-compra.preco-fornec
           tt-ordem-compra.preco-unit     = tt-imp-ordem-compra.preco-fornec
           tt-ordem-compra.preco-orig     = tt-imp-ordem-compra.preco-fornec
           tt-ordem-compra.qt-solic       = tt-imp-ordem-compra.qt-solic    
           tt-ordem-compra.ct-codigo      = STRING(tt-imp-ordem-compra.ct-codigo)
           tt-ordem-compra.cod-transp     = tt-imp-pedido-compr.cod-transp
           tt-ordem-compra.sc-codigo      = STRING(tt-imp-ordem-compra.sc-codigo)
           //tt-ordem-compra.tp-despesa     = 714
           tt-ordem-compra.data-emissao   = tt-imp-ordem-compra.data-emissao
           tt-ordem-compra.ordem-servic   = tt-imp-ordem-compra.ordem-servic
           tt-ordem-compra.natureza       = tt-imp-ordem-compra.natureza    
           tt-ordem-compra.num-ord-inv    = tt-imp-ordem-compra.num-ord-inv 
           /** comentado a pedido do CPAS - 25.06
           tt-ordem-compra.pre-unit-for   = tt-imp-ordem-compra.pre-unit-for
           **/
           tt-ordem-compra.pre-unit-for   =  tt-imp-ordem-compra.preco-fornec                                                     //25-06 Leandro Policarpo
           tt-ordem-compra.mo-codigo      = INT(tt-imp-ordem-compra.mo-codigo)
           tt-ordem-compra.Codigo-ipi     = tt-imp-ordem-compra.Codigo-ipi  
           tt-ordem-compra.aliquota-ipi   = IF tt-imp-ordem-compra.Aliquota-ipi = ? THEN 0 ELSE tt-imp-ordem-compra.Aliquota-ipi  //27-06 Leandro Policarpo 
           tt-ordem-compra.Codigo-icm     = tt-imp-ordem-compra.Codigo-icm  
           tt-ordem-compra.aliquota-icm   = IF tt-imp-ordem-compra.Aliquota-icm = ? THEN 0 ELSE tt-imp-ordem-compra.Aliquota-ipi  //27-6 Leandro Policarpo 
           //tt-ordem-compra.Aliquota-iss   = tt-imp-ordem-compra.Aliquota-iss
           tt-ordem-compra.valor-frete    = tt-imp-ordem-compra.Valor-frete 
           tt-ordem-compra.cod-cond-pag   = INT(tt-imp-ordem-compra.cod-cond-pag)
           tt-ordem-compra.requisitante   = tt-imp-ordem-compra.requisitante
           tt-ordem-compra.usuario        = tt-imp-ordem-compra.usuario
           tt-ordem-compra.narrativa      = tt-imp-ordem-compra.narrativa.

    FIND FIRST item-uni-estab WHERE
               item-uni-estab.it-codigo   = tt-imp-ordem-compra.it-codigo   AND 
               item-uni-estab.cod-estabel = tt-imp-pedido-compr.cod-estabel NO-LOCK NO-ERROR.
    IF AVAIL item-uni-estab THEN DO:

        ASSIGN //tt-ordem-compra.cod-comprado     = item-uni-estab.cod-comprado
               //tt-ordem-compra.usuario          = item-uni-estab.cod-comprado
               //tt-imp-ordem-compra.requisitante = item-uni-estab.cod-comprado
               tt-ordem-compra.tp-despesa       = item-uni-estab.tp-desp-padrao
               tt-ordem-compra.dep-almoxar      = item-uni-estab.deposito-pad .

        
        IF SUBSTRING(item-uni-estab.char-1,133,1) = "7" THEN
            ASSIGN tt-ordem-compra.codigo-icm     = 1
                   tt-imp-ordem-compra.codigo-icm = 1.
        ELSE
            ASSIGN tt-ordem-compra.codigo-icm     = 2
                   tt-imp-ordem-compra.codigo-icm = 2.
        

    END.
    ELSE DO:

        FIND FIRST ITEM WHERE
                   ITEM.it-codigo = tt-imp-ordem-compra.it-codigo NO-LOCK NO-ERROR.
        IF AVAIL ITEM THEN
            ASSIGN //tt-ordem-compra.cod-comprado     = ITEM.cod-comprado
                   //tt-ordem-compra.usuario          = ITEM.cod-comprado
                   //tt-imp-ordem-compra.requisitante = ITEM.cod-comprado
                   tt-ordem-compra.tp-despesa       = ITEM.tp-desp-padrao
                   tt-ordem-compra.dep-almoxar      = ITEM.deposito-pad.


    END.

    ASSIGN tt-ordem-compra.codigo-ipi     = NO        /* 03/07 - BCC */
           tt-imp-ordem-compra.codigo-ipi = NO.   /* 03/07 - BCC */
    
    IF tt-imp-ordem-compra.Aliquota-ipi > 0 THEN
        ASSIGN tt-ordem-compra.codigo-ipi     = YES
               tt-imp-ordem-compra.codigo-ipi = YES.

    IF tt-imp-ordem-compra.natureza = 2 THEN DO:

        FIND FIRST ITEM WHERE
                   ITEM.it-codigo = tt-imp-ordem-compra.it-codigo NO-LOCK NO-ERROR.
        IF AVAIL ITEM THEN
            ASSIGN tt-ordem-compra.aliquota-iss     = ITEM.aliquota-iss
                   tt-imp-ordem-compra.aliquota-iss = ITEM.aliquota-iss.
                                    
    END.

    IF NOT VALID-HANDLE(h-boin274sd) THEN
        RUN inbo/boin274sd.p PERSISTENT SET h-boin274sd.

    IF lLoop THEN DO:

        FIND LAST b-tt-ordem-compra NO-LOCK NO-ERROR.
        IF AVAIL  b-tt-ordem-compra THEN
            ASSIGN i-num-ordem = b-tt-ordem-compra.numero-ordem + 1. 

    END.
    ELSE
        RUN geraNumeroOrdemPedEmerg in h-boin274sd (OUTPUT i-num-ordem, 
                                                    OUTPUT TABLE RowErrors). 

    ASSIGN tt-ordem-compra.numero-ordem     = i-num-ordem
           tt-imp-ordem-compra.numero-ordem = i-num-ordem.

    MESSAGE "i-num-ordem " i-num-ordem.

    IF NOT VALID-HANDLE(h-boin274sd) THEN
        RUN inbo/boin274vl.p PERSISTENT SET h-boin274vl.

    MESSAGE "CLF >>>>>>>>>>>> L-MANUT-ITEM-FORNEC " l-manut-item-fornec
        VIEW-AS ALERT-BOX INFO BUTTONS OK.


    

    ASSIGN lLoop = TRUE.

    IF VALID-HANDLE(h-boin274sd) THEN DO:
        DELETE PROCEDURE h-boin274sd.
        ASSIGN h-boin274sd = ?.
    END.

    IF VALID-HANDLE(h-boin274vl) THEN DO:
        DELETE PROCEDURE h-boin274vl.
        ASSIGN h-boin274vl = ?.
    END.
    



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-06-geraPrazoCompra) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-06-geraPrazoCompra Procedure 
PROCEDURE pi-06-geraPrazoCompra :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    FIND FIRST tt-ordem-compra WHERE
               tt-ordem-compra.numero-ordem = tt-imp-ordem-compra.numero-ordem   NO-LOCK NO-ERROR.

    CREATE tt-prazo-compra.
    ASSIGN tt-prazo-compra.Numero-ordem  = tt-ordem-compra.numero-ordem
           tt-prazo-compra.Data-entrega  = tt-imp-prazo-compra.Data-entrega 
           tt-prazo-compra.Natureza      = tt-imp-prazo-compra.Natureza     
           tt-prazo-compra.un            = tt-imp-prazo-compra.un           
           tt-prazo-compra.Parcela       = 1
           tt-prazo-compra.it-codigo     = tt-imp-prazo-compra.it-codigo    
           tt-prazo-compra.quantidade    = tt-imp-prazo-compra.quantidade   
           tt-prazo-compra.quant-saldo   = tt-imp-prazo-compra.quantidade  
           tt-prazo-compra.quantid-orig  = tt-imp-prazo-compra.quantidade 
           tt-prazo-compra.qtd-sal-forn  = tt-imp-prazo-compra.quantidade
           //tt-prazo-compra.qtd-do-forn   = tt-imp-prazo-compra.quantidade
           tt-prazo-compra.situacao      = 2.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-07-geraCotacaoItem) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-07-geraCotacaoItem Procedure 
PROCEDURE pi-07-geraCotacaoItem :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FIND FIRST tt-ordem-compra WHERE
           tt-ordem-compra.numero-ordem = tt-imp-ordem-compra.numero-ordem NO-LOCK NO-ERROR.

FIND FIRST tt-prazo-compra WHERE
           tt-prazo-compra.numero-ordem = tt-imp-ordem-compra.numero-ordem   NO-LOCK NO-ERROR.

CREATE tt-cotacao-item.
ASSIGN tt-cotacao-item.numero-ordem = tt-imp-ordem-compra.numero-ordem
       tt-cotacao-item.cod-emitente = INT(tt-imp-pedido-compr.cod-emitente)
       tt-cotacao-item.un           = tt-prazo-compra.un
       tt-cotacao-item.it-codigo    = tt-imp-ordem-compra.it-codigo   
       tt-cotacao-item.cod-transp   = tt-imp-pedido-compr.cod-transp
       tt-cotacao-item.preco-fornec = tt-imp-ordem-compra.preco-fornec
       tt-cotacao-item.mo-codigo    = INT(tt-imp-ordem-compra.mo-codigo)
       tt-cotacao-item.data-cotacao = TODAY
       tt-cotacao-item.preco-unit   = tt-imp-ordem-compra.preco-fornec       //25-06 Leandro Policarpo
       tt-cotacao-item.pre-unit-for = tt-imp-ordem-compra.preco-fornec       //25-06 Leandro Policarpo 
       //tt-cotacao-item.preco-unit   = tt-imp-ordem-compra.pre-unit-for  
       /** comentado a pedido do CPAS - 25.06
       tt-cotacao-item.pre-unit-for = tt-imp-ordem-compra.pre-unit-for
       ***/
       tt-cotacao-item.codigo-ipi   = tt-imp-ordem-compra.codigo-ipi  
       tt-cotacao-item.aliquota-ipi = IF tt-imp-ordem-compra.aliquota-ipi = ? THEN 0 ELSE tt-imp-ordem-compra.aliquota-ipi
       tt-cotacao-item.codigo-icm   = tt-imp-ordem-compra.codigo-icm
       tt-cotacao-item.aliquota-icm = IF tt-imp-ordem-compra.aliquota-icm = ? THEN 0 ELSE tt-imp-ordem-compra.aliquota-icm
       tt-cotacao-item.aliquota-iss = tt-imp-ordem-compra.aliquota-iss
       tt-cotacao-item.valor-frete  = IF tt-imp-ordem-compra.valor-frete = ? THEN 0 ELSE tt-imp-ordem-compra.valor-frete
       tt-cotacao-item.cod-cond-pag = INT(tt-imp-pedido-compr.cod-cond-pag)
       tt-cotacao-item.contato      = "IMPORTADO ARIBA" 
       tt-cotacao-item.valor-taxa   = 0   /**aqui taxa **/
       tt-cotacao-item.cod-comprado = tt-imp-pedido-compr.responsavel
       tt-cotacao-item.hora-atualiz = STRING(TIME,'HH:MM')
       tt-cotacao-item.cot-aprovada = YES.

IF tt-cotacao-item.Valor-frete > 0 THEN
    ASSIGN tt-cotacao-item.Frete = YES.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-08-geraCondPgto) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-08-geraCondPgto Procedure 
PROCEDURE pi-08-geraCondPgto :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

CREATE  tt-cond-especif.
ASSIGN  tt-cond-especif.num-pedido    = tt-imp-pedido-compr.num-pedido-totvs
        //cond-especif.nr-contrato   = tt-imp-pedido-compr.nr-contrato
        tt-cond-especif.data-pagto[1] = tt-imp-pedido-compr.data-pgto-1     
        tt-cond-especif.data-pagto[2] = tt-imp-pedido-compr.data-pgto-2     
        tt-cond-especif.data-pagto[3] = tt-imp-pedido-compr.data-pgto-3     
        tt-cond-especif.perc-pagto[1] = tt-imp-pedido-compr.perc-pgto-1     
        tt-cond-especif.perc-pagto[2] = tt-imp-pedido-compr.perc-pgto-2     
        tt-cond-especif.perc-pagto[3] = tt-imp-pedido-compr.perc-pgto-3.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-08-log-erros) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-08-log-erros Procedure 
PROCEDURE pi-08-log-erros :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FOR EACH tt-erros-geral NO-LOCK.

    MESSAGE tt-erros-geral.des-erro.

    CREATE tt-retorno-nok.
    ASSIGN tt-retorno-nok.data        = NOW
           tt-retorno-nok.cod-erro    = tt-erros-geral.cod-erro
           tt-retorno-nok.desc-erro   = tt-erros-geral.des-erro
           tt-retorno-nok.sequencia   = 1
           tt-retorno-nok.UniqueName  = tt-imp-pedido-compr.num-pedido.
           //p-erro              = TRUE.
    
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-09-geraUnidNegoc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-09-geraUnidNegoc Procedure 
PROCEDURE pi-09-geraUnidNegoc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


CREATE  unid-neg-ordem.
ASSIGN  unid-neg-ordem.numero-ordem   = tt-imp-ordem-compra.numero-ordem
        unid-neg-ordem.cod_unid_negoc = tt-imp-ordem-compra.unidade-negocio
        unid-neg-ordem.perc-unid-neg  = 100.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-10-calc-total-pedido) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-10-calc-total-pedido Procedure 
PROCEDURE pi-10-calc-total-pedido :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT  PARAMETER p-num-pedido   AS INT  NO-UNDO.
DEFINE OUTPUT PARAMETER p-total-item   AS DECIMAL NO-UNDO.
DEFINE OUTPUT PARAMETER p-total-pedido AS DECIMAL NO-UNDO.

FOR EACH ordem-compra WHERE
         ordem-compra.num-pedido = p-num-pedido NO-LOCK:

    ASSIGN p-total-item   = p-total-item   + ordem-compra.qt-solic
           p-total-pedido = p-total-pedido + ordem-compra.preco-fornec.

END.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


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

/*ENVIO PEDIDO DE COMPRA*/
DEF TEMP-TABLE tt-recebimento-fisico NO-UNDO SERIALIZE-NAME "Recebimento_Fisico"
    FIELD ERPReceiptNumber                    AS CHAR
    FIELD Comment                             AS CHAR
    field NumberAccepted                      LIKE it-doc-fisico.quantidade 
    field NumberRejected                      LIKE it-doc-fisico.quantidade 
    FIELD AmountAccepted                      AS DECIMAL
    FIELD AmountCurrency                      AS CHAR
    FIELD ReceivingType                       AS INT
    field ReceivableLineItemId                LIKE it-doc-fisico.sequencia
    field ReceivedDate                        AS DATETIME
    field OriginatingSystem                   AS CHAR
    field ReceivableId                        AS CHAR.

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


DEFINE VARIABLE h-boin295   AS HANDLE NO-UNDO.
DEFINE VARIABLE h-boin274sd AS HANDLE NO-UNDO.
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

DEF VAR l-situacao AS LOG      NO-UNDO.
DEF VAR lc-json    AS LONGCHAR NO-UNDO.
DEF VAR c-erro     AS CHAR     NO-UNDO.
DEF VAR i-nr-seq   AS INT      NO-UNDO.

FOR EACH sfa-export WHERE
         sfa-export.cd-tipo-integr = 23 AND 
         sfa-export.ind-situacao   = 1  EXCLUSIVE-LOCK:

    FOR FIRST sfa-export-receb-ariba WHERE
              sfa-export-receb-ariba.cd-tipo-integr = sfa-export.cd-tipo-integr AND
              sfa-export-receb-ariba.id-movto       = sfa-export.id-movto       EXCLUSIVE-LOCK:

        ASSIGN sfa-export.data-inicio  = NOW.

        IF sfa-export-receb-ariba.tp-recebimento = 2 THEN
            RUN pi-20-gera-recebimento-fiscal (INPUT  sfa-export-receb-ariba.ROWID,
                                               INPUT  sfa-export-receb-ariba.ind-serv-mat,
                                               OUTPUT l-situacao,
                                               OUTPUT lc-json,
                                               OUTPUT c-erro).
        ELSE
            RUN pi-10-gera-recebimento-fisico (INPUT  sfa-export-receb-ariba.ROWID,
                                               INPUT  sfa-export-receb-ariba.ind-serv-mat,
                                               OUTPUT l-situacao,
                                               OUTPUT lc-json,
                                               OUTPUT c-erro).


        ASSIGN sfa-export.data-fim     = NOW
               sfa-export.ind-situacao = 2 .

        IF l-situacao THEN DO:

        ASSIGN sfa-export.cod-status = 1
               sfa-export-receb-ariba.c-json = lc-json.

        FIND LAST sfa-export-log NO-LOCK OF sfa-export NO-ERROR.
        IF AVAIL sfa-export-log THEN
            ASSIGN i-nr-seq = sfa-export-log.nr-seq + 1.
        ELSE i-nr-seq = 1.

        CREATE sfa-export-log.
        ASSIGN sfa-export-log.ind-tipo-trans = sfa-export.ind-tipo-trans
               sfa-export-log.cd-tipo-integr = sfa-export.cd-tipo-integr
               sfa-export-log.id-movto       = sfa-export.id-movto      
               sfa-export-log.data-log       = NOW
               sfa-export-log.des-log        = "Registro integrado com sucesso"
               sfa-export-log.nr-seq         = i-nr-seq.

    END.
    ELSE DO:

        ASSIGN sfa-export.cod-status = 2.

        FIND LAST sfa-export-log NO-LOCK OF sfa-export NO-ERROR.
        IF AVAIL sfa-export-log THEN
            ASSIGN i-nr-seq = sfa-export-log.nr-seq + 1.
        ELSE i-nr-seq = 1.

        CREATE sfa-export-log.
        ASSIGN sfa-export-log.ind-tipo-trans = sfa-export.ind-tipo-trans
               sfa-export-log.cd-tipo-integr = sfa-export.cd-tipo-integr
               sfa-export-log.id-movto       = sfa-export.id-movto      
               sfa-export-log.data-log       = NOW
               sfa-export-log.des-log        = "Registro integrado com erro: " + c-erro.
               sfa-export-log.nr-seq         = i-nr-seq.

    END.

    END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-pi-10-gera-recebimento-fisico) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-10-gera-recebimento-fisico Procedure 
PROCEDURE pi-10-gera-recebimento-fisico :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT  PARAMETER p-rowid        AS RECID    NO-UNDO.
DEFINE INPUT  PARAMETER p-ind-serv-mat AS INT      NO-UNDO.
DEFINE OUTPUT PARAMETER p-retorno      AS LOG      NO-UNDO.
DEFINE OUTPUT PARAMETER p-json         AS LONGCHAR NO-UNDO.
DEFINE OUTPUT PARAMETER p-menssagem    AS CHAR     NO-UNDO.

DEF VAR c-token         AS CHAR     NO-UNDO.
DEF VAR pArquivoEntrada AS LONGCHAR NO-UNDO.
DEF VAR lc-erro         AS LONGCHAR NO-UNDO.

FOR FIRST it-doc-fisico WHERE
         RECID(it-doc-fisico) = p-rowid NO-LOCK:

    FIND FIRST doc-fisico OF it-doc-fisico NO-LOCK NO-ERROR.

    FIND FIRST ordem-compra OF it-doc-fisico NO-LOCK NO-ERROR.

    FIND FIRST pedido-compr OF it-doc-fisico NO-LOCK NO-ERROR.

    CREATE tt-recebimento-fisico.
    ASSIGN tt-recebimento-fisico.ERPReceiptNumber      = it-doc-fisico.nro-docto
           tt-recebimento-fisico.Comment               = it-doc-fisico.narrativa
           tt-recebimento-fisico.NumberAccepted        = it-doc-fisico.quantidade 
           tt-recebimento-fisico.NumberRejected        = it-doc-fisico.qt-saldo
           tt-recebimento-fisico.ReceivableLineItemId  = ordem-compra.sequencia
           tt-recebimento-fisico.ReceivedDate          = doc-fisico.dt-emissao
           tt-recebimento-fisico.OriginatingSystem     = "TOTVS"
           tt-recebimento-fisico.ReceivableId          = STRING(pedido-compr.char-2)
           tt-recebimento-fisico.ReceivingType         = 2.

    IF p-ind-serv-mat = 1 THEN
        ASSIGN tt-recebimento-fisico.AmountAccepted = it-doc-fisico.valor.
    ELSE
        ASSIGN tt-recebimento-fisico.AmountAccepted = it-doc-fisico.quantidade.

    IF ordem-compra.mo-codigo = 0 THEN
        ASSIGN tt-recebimento-fisico.AmountCurrency = "BRL".
    ELSE
        ASSIGN tt-recebimento-fisico.AmountCurrency = "USD".

END.

IF NOT VALID-HANDLE(h-esapi002) THEN
    RUN esp/esint002.p PERSISTENT SET h-esapi002.

lRetOK = TEMP-TABLE tt-recebimento-fisico:WRITE-JSON("LONGCHAR", pArquivoEntrada, TRUE, "UTF-8") NO-ERROR.

ASSIGN p-json = pArquivoEntrada.

OUTPUT TO c:/temp/jsonrecebimentofisico.json.

    EXPORT pArquivoEntrada.

OUTPUT CLOSE.

RUN piGeraTokenApigee IN h-esapi002 (OUTPUT c-token).

CREATE "MSXML2.XMLHTTP" client.

//IF p-ind-serv-mat = 2 THEN DO:

FIND FIRST es-api-param NO-LOCK WHERE es-api-param.cd-tipo-integr = 23 /*---- Integra»’o Receb Ariba  ------*/ NO-ERROR.
IF AVAIL es-api-param THEN DO:

    client:OPEN("post", es-api-param.host-integr + ":" + STRING(es-api-param.porta-integr) + path-integr, FALSE).
    client:SetRequestHeader ("Content-Type", "application/json").
    client:SetRequestHeader ("Authorization", "Bearer " + c-token).
    client:Send(pArquivoEntrada).
    
    
    MESSAGE 'ResponseText  '    client:ResponseText   SKIP
            'Responsexml   '    client:Responsexml    SKIP
            'responseStream'    client:responseStream SKIP
            'STATUS        '    client:STATUS         SKIP
            VIEW-AS ALERT-BOX TITLE "retorno".

END.

/*END.
ELSE IF p-ind-serv-mat = 1 THEN DO:

    FIND FIRST es-api-param NO-LOCK WHERE es-api-param.cd-tipo-integr = 26 /*---- Integra»’o Receb Servi‡o Ariba  ------*/ NO-ERROR.
    IF AVAIL es-api-param THEN DO:
    
        client:OPEN("post", es-api-param.host-integr + ":" + STRING(es-api-param.porta-integr) + path-integr, FALSE).
        client:SetRequestHeader ("Content-Type", "application/json").
        client:SetRequestHeader ("Authorization", "Bearer " + c-token).
        client:Send(pArquivoEntrada).
        
        
        MESSAGE 'ResponseText  '    client:ResponseText   SKIP
                'Responsexml   '    client:Responsexml    SKIP
                'responseStream'    client:responseStream SKIP
                'STATUS        '    client:STATUS         SKIP
                VIEW-AS ALERT-BOX TITLE "retorno".
    
    END.

END.*/

IF client:STATUS = "200" THEN
    ASSIGN p-retorno   = YES.
ELSE
    ASSIGN p-menssagem = client:ResponseText.

IF VALID-HANDLE(h-esapi002) THEN DO:
    DELETE PROCEDURE h-esapi002.
    ASSIGN h-esapi002 = ?.
END.

RELEASE OBJECT client.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-20-gera-recebimento-fiscal) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-20-gera-recebimento-fiscal Procedure 
PROCEDURE pi-20-gera-recebimento-fiscal :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT  PARAMETER p-rowid        AS RECID    NO-UNDO.
DEFINE INPUT  PARAMETER p-ind-serv-mat AS INT      NO-UNDO.
DEFINE OUTPUT PARAMETER p-retorno      AS LOG      NO-UNDO.
DEFINE OUTPUT PARAMETER p-json         AS LONGCHAR NO-UNDO.
DEFINE OUTPUT PARAMETER p-menssagem    AS CHAR     NO-UNDO.

DEF VAR c-token         AS CHAR     NO-UNDO.
DEF VAR pArquivoEntrada AS LONGCHAR NO-UNDO.
DEF VAR lc-erro         AS LONGCHAR NO-UNDO.

FOR FIRST item-doc-est WHERE
         RECID(item-doc-est) = p-rowid NO-LOCK:

    //FIND FIRST doc-estoque OF it-doc-fisico NO-LOCK NO-ERROR.

    FIND FIRST ordem-compra OF item-doc-est NO-LOCK NO-ERROR.

    FIND FIRST pedido-compr OF item-doc-est NO-LOCK NO-ERROR.
    
    CREATE tt-recebimento-fisico.
    ASSIGN tt-recebimento-fisico.ERPReceiptNumber      = item-doc-est.nro-docto
           tt-recebimento-fisico.Comment               = item-doc-est.narrativa
           tt-recebimento-fisico.NumberAccepted        = item-doc-est.quantidade 
           tt-recebimento-fisico.NumberRejected        = item-doc-est.qt-saldo
           tt-recebimento-fisico.ReceivableLineItemId  = ordem-compra.sequencia
           tt-recebimento-fisico.ReceivedDate          = item-doc-est.data
           tt-recebimento-fisico.OriginatingSystem     = "TOTVS"
           tt-recebimento-fisico.ReceivableId          = STRING(pedido-compr.char-2)
           tt-recebimento-fisico.ReceivingType         = 2.

    IF p-ind-serv-mat = 1 THEN
        ASSIGN tt-recebimento-fisico.AmountAccepted = item-doc-est.quantidade * ordem-compra.preco-fornec.
    ELSE
        ASSIGN tt-recebimento-fisico.AmountAccepted = item-doc-est.quantidade.

    IF ordem-compra.mo-codigo = 0 THEN
        ASSIGN tt-recebimento-fisico.AmountCurrency = "BRL".
    ELSE
        ASSIGN tt-recebimento-fisico.AmountCurrency = "USD".

END.

IF NOT VALID-HANDLE(h-esapi002) THEN
    RUN esp/esint002.p PERSISTENT SET h-esapi002.

lRetOK = TEMP-TABLE tt-recebimento-fisico:WRITE-JSON("LONGCHAR", pArquivoEntrada, TRUE, "UTF-8") NO-ERROR.

ASSIGN p-json = pArquivoEntrada.

OUTPUT TO c:/temp/jsonrecebimentofisico.json.

    EXPORT pArquivoEntrada.

OUTPUT CLOSE.

RUN piGeraTokenApigee IN h-esapi002 (OUTPUT c-token).

CREATE "MSXML2.XMLHTTP" client.

FIND FIRST es-api-param NO-LOCK WHERE es-api-param.cd-tipo-integr = 23 /*---- Integra»’o Receb Ariba  ------*/ NO-ERROR.
IF AVAIL es-api-param THEN DO:

    client:OPEN("post", es-api-param.host-integr + ":" + STRING(es-api-param.porta-integr) + path-integr, FALSE).
    client:SetRequestHeader ("Content-Type", "application/json").
    client:SetRequestHeader ("Authorization", "Bearer " + c-token).
    client:Send(pArquivoEntrada).
    
    
    MESSAGE 'ResponseText  '    client:ResponseText   SKIP
            'Responsexml   '    client:Responsexml    SKIP
            'responseStream'    client:responseStream SKIP
            'STATUS        '    client:STATUS         SKIP
            VIEW-AS ALERT-BOX TITLE "retorno".

END.


IF client:STATUS = "200" THEN
    ASSIGN p-retorno   = YES.
ELSE
    ASSIGN p-menssagem = client:ResponseText.



IF VALID-HANDLE(h-esapi002) THEN DO:
    DELETE PROCEDURE h-esapi002.
    ASSIGN h-esapi002 = ?.
END.

RELEASE OBJECT client.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


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
DEFINE INPUT  PARAMETER p-cod-emitente  AS INT  NO-UNDO.


// O campo ind-tipo-movto trata qual movimento esta realizando
// 1 - Inclusao / 2 - Alteracao / 3 - Exclusao

DEF TEMP-TABLE fornecedor-ariba NO-UNDO SERIALIZE-NAME "Fornecedor_Ariba"
    FIELD InternalID                          AS CHAR

    FIELD ID                                  AS c
    FIELD UUID                                AS c
    FIELD ID_1                                AS c
    FIELD UUID_1                              AS c
    FIELD UUID_2                              AS c
    FIELD ReceiverUUID                        AS c
    FIELD ReceiverInternalID                  AS c
    .

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


DEFINE VARIABLE h-boin295            AS HANDLE NO-UNDO.
DEFINE VARIABLE h-boin274sd          AS HANDLE NO-UNDO.
DEFINE VARIABLE h-esint002           AS HANDLE     NO-UNDO.
DEFINE VARIABLE c-url                AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-token              AS CHARACTER  NO-UNDO.
DEFINE VARIABLE v-longchar           AS LONGCHAR   NO-UNDO.
DEFINE VARIABLE c-texto              AS CHARACTER  NO-UNDO.
DEFINE VARIABLE client               AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE l-retorno-fornecedor AS LOGICAL NO-UNDO.

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

RUN pi-00-envia-cod-fornecedor.

{esp\esint001rp.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-pi-00-envia-cod-fornecedor) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-00-envia-cod-fornecedor Procedure
PROCEDURE pi-00-envia-cod-fornecedor :
DEFINE VARIABLE pArquivoEnvio   AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE pArquivoRetorno AS LONGCHAR NO-UNDO.

    FIND FIRST es-fornecedor-ariba
         WHERE es-fornecedor-ariba.cod-emitente = p-cod-emitente
           AND es-fornecedor-ariba.number       > ""
         NO-ERROR.

    MESSAGE PROGRAM-NAME(1) AVAIL es-fornecedor-ariba
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

    IF AVAIL es-fornecedor-ariba
    THEN DO:
       FIND FIRST es-api-param NO-LOCK
            WHERE es-api-param.ind-tipo-trans = 2  /*---- Saida ----*/
              AND es-api-param.cd-tipo-integr = 27
            NO-ERROR.
       IF AVAIL es-api-param
       THEN DO:
           MESSAGE PROGRAM-NAME(2)  1
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
          CREATE api-export-ariba-codigo.
          ASSIGN
             api-export-ariba-codigo.cd-tipo-integr = es-api-param.cd-tipo-integr
             api-export-ariba-codigo.id-movto       = NEXT-VALUE(seq-export)
             api-export-ariba-codigo.data-movto     = NOW
             api-export-ariba-codigo.c-json         = ?
             api-export-ariba-codigo.Number         = es-fornecedor-ariba.Number
             api-export-ariba-codigo.dt-consulta    = es-fornecedor-ariba.dt-consulta
             .
          MESSAGE PROGRAM-NAME(2)  2
              VIEW-AS ALERT-BOX INFO BUTTONS OK.

          CREATE sfa-export.
          ASSIGN
             sfa-export.ind-tipo-trans = es-api-param.ind-tipo-trans
             sfa-export.id-movto       = api-export-ariba-codigo.id-movto
             sfa-export.cd-tipo-integr = api-export-ariba-codigo.cd-tipo-integr
             sfa-export.chave          = STRING(api-export-ariba-codigo.id-movto)
             sfa-export.cod-status     = 0      /* ---- sem status ----*/
             sfa-export.data-fim       = ?
             sfa-export.data-inicio    = ?
             sfa-export.data-movto     = NOW
             sfa-export.ind-situacao   = 1      /*---- Pendente -----*/.
          MESSAGE PROGRAM-NAME(2)  3
              VIEW-AS ALERT-BOX INFO BUTTONS OK.


          MESSAGE PROGRAM-NAME(2)  4
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

       END.
       MESSAGE PROGRAM-NAME(2)  5
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

       RUN pi-processa (es-api-param.ind-tipo-trans,
                        es-api-param.cd-tipo-integr
                       ).
       MESSAGE PROGRAM-NAME(2)  5
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

    END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : cancelapedcompra.p
    Purpose     : Integraáao - ARIBA x Pedido de Compra

    Syntax      :

    Description : Rotina para cancelamento do pedido de compra

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
    FIELD num-pedido            AS INT      
    FIELD num-pedido-totvs      AS INT      
    FIELD Cod-estabel           AS CHAR     
    FIELD Cod-emitente          AS INT      
    FIELD Cod-cond-pag          AS INT      
    FIELD Natureza              AS INT      
    FIELD data-pedido           AS DATE     
    FIELD Situacao              AS INT      
    FIELD Frete                 AS INT      
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
    FIELD num-processo          AS INTEGER FORMAT "999999999"
    FIELD num-sequencia         AS INTEGER FORMAT "999999"
    FIELD ind-tipo-movto        AS INTEGER FORMAT "99" INITIAL 1.


DEF TEMP-TABLE tt-imp-ordem-compra NO-UNDO SERIALIZE-NAME "Ordem_Compra"
    FIELD  Numero-ordem    AS INT
    FIELD  sequencia       AS INT
    FIELD  cod-estabel     AS CHAR
    FIELD  Num-pedido      AS INT
    FIELD  cod-emitente    AS INT
    FIELD  it-codigo       AS CHAR
    FIELD  cod-comprado    AS CHAR
    FIELD  requisitante    AS CHAR
    FIELD  dep-almoxar     AS CHAR
    FIELD  situacao        AS INT
    FIELD  preco-fornec    AS DEC
    FIELD  qt-solic        AS DEC
    FIELD  CT-CODIGO       AS CHAR
    FIELD  SC-CODIGO       AS CHAR
    FIELD  tp-despesa      AS INT
    FIELD  data-emissao    AS DATE
    FIELD  ordem-servic    AS INT
    FIELD  natureza        AS INT
    FIELD  narrativa       AS CHAR
    FIELD  num-ord-inv     AS INT
    FIELD  pre-unit-for    AS DEC
    FIELD  mo-codigo       AS INT
    FIELD  Codigo-ipi      AS LOGICAL
    FIELD  Aliquota-ipi    AS DEC
    FIELD  Codigo-icm      AS INT
    FIELD  Aliquota-icm    AS DEC
    FIELD  Aliquota-iss    AS DEC
    FIELD  Valor-frete     AS DEC
    FIELD  cod-cond-pag    AS INT
    FIELD  Usuario         AS CHAR
    FIELD num-processo     AS INTEGER FORMAT "999999999"
    FIELD num-sequencia    AS INTEGER FORMAT "999999"
    FIELD ind-tipo-movto   AS INTEGER FORMAT "99".

DEF TEMP-TABLE tt-imp-prazo-compra NO-UNDO SERIALIZE-NAME "Prazo_Compra"
    FIELD Numero-ordem          AS INT
    FIELD sequencia             AS INT
    FIELD Num-pedido            AS INT
    FIELD Data-entrega          AS DATE
    FIELD Natureza              AS INT
    FIELD un                    AS CHAR
    FIELD Parcela               AS INT   
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
    FIELD Num-pedido            AS INT
    FIELD cod-emitente          AS INT
    FIELD it-codigo             AS CHAR
    FIELD Cod-transp            AS INT
    FIELD Preco-fornec          AS DEC
    FIELD Contato               AS CHAR
    FIELD mo-codigo             AS INT
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
    FIELD cod-cond-pag          AS INT
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

DEF TEMP-TABLE tt-erros
    FIELD cod-erro  AS INTEGER
    FIELD desc-erro AS CHARACTER FORMAT "x(256)"
    FIELD desc-arq  AS CHARACTER.

//DEFINE VARIABLE c-transacao-global AS CHARACTER INITIAL 'a'  NO-UNDO.

//DEFINE VARIABLE l-ok                  AS LOGICAL     NO-UNDO.
//DEFINE VARIABLE l-xml                 AS LOGICAL     NO-UNDO.
//DEFINE VARIABLE c-arquivo-xsd         AS CHARACTER   NO-UNDO.
//DEFINE VARIABLE i-count               AS INTEGER     NO-UNDO.



//SIT
DEFINE TEMP-TABLE ttError    SERIALIZE-NAME "Retorno"
    FIELD SessionId          AS CHARACTER
    FIELD referencia         AS CHARACTER
    FIELD codigo             AS CHARACTER
    FIELD desc-erro          AS CHARACTER
    FIELD UniqueName         AS CHARACTER
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
         HEIGHT             = 15.33
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

// Localizar o arquivo XSD


/*ASSIGN c-arquivo-xsd = "mep\esme0002.xsd".
DO  i-count = 1 TO NUM-ENTRIES(PROPATH):
    ASSIGN c-arquivo-xsd = ENTRY(i-count,PROPATH) + "\" + "mep\esme0002.xsd".
    IF SEARCH(c-arquivo-xsd) <> ? THEN ASSIGN i-count = NUM-ENTRIES(PROPATH).
END.

MESSAGE STRING(p-arquivo-entrada)
    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
//l-ok = DATASET Fornecedor:READ-XML("LONGCHAR", p-arquivo-entrada, "empty","F:\TOTVS\datasul\MIR\ESPECIFICO\Fornecedor.xsd",?,?,?) NO-ERROR.

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

lretOK = DATASET httPedidoCompra:READ-JSON("LONGCHAR", json_recebido, "empty") NO-ERROR.

IF lretOK = NO THEN DO:
    
    CREATE tt-retorno-nok.
    ASSIGN tt-retorno-nok.data        = NOW
           tt-retorno-nok.cod-erro    = 0
           tt-retorno-nok.desc-erro   = "N∆o foi poss°vel fazer o parse do arquivo. Integraá∆o Pedido de Compra."
           tt-retorno-nok.sequencia   = 1
           tt-retorno-nok.UniqueName  = "".
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
  Notes: ind-tipo-movto -> 1 - Inclusao / 2 - Alteracao / 3 - Exclusao
------------------------------------------------------------------------------*/
EMPTY TEMP-TABLE ttError.

IF CAN-FIND (FIRST tt-imp-pedido-compr) THEN DO:
    FOR FIRST tt-imp-pedido-compr.

        /*Alteracao*/
        ASSIGN tt-imp-pedido-compr.ind-tipo-movto = 2.

        // Validar a operaá∆o
        CASE tt-imp-pedido-compr.ind-tipo-movto:
            WHEN 1 THEN DO: // Inclusao de registro

                // Verificar se o codigo do emitente j† existe cadastrado 
                IF tt-imp-pedido-compr.num-pedido <> 0 THEN DO:
                    FIND FIRST pedido-compr NO-LOCK
                        WHERE pedido-compr.num-pedido = tt-imp-pedido-compr.num-pedido NO-ERROR.
                    IF AVAIL pedido-compr THEN DO:
                        CREATE tt-retorno-nok.
                        ASSIGN tt-retorno-nok.data        = NOW
                               tt-retorno-nok.cod-erro    = 1
                               tt-retorno-nok.desc-erro   = "Pedido de Compra j† existe com o n£mero " + STRING(tt-imp-pedido-compr.num-pedido)
                               tt-retorno-nok.sequencia   = 1
                               tt-retorno-nok.UniqueName  = pedido-compr.char-2. 

                    END.
                END.
            END.
            WHEN 2 THEN DO:
                FIND FIRST pedido-compr NO-LOCK
                    WHERE pedido-compr.num-pedido = tt-imp-pedido-compr.num-pedido NO-ERROR.
                IF NOT AVAIL pedido-compr THEN DO:
                    CREATE tt-retorno-nok.
                    ASSIGN tt-retorno-nok.data        = NOW
                           tt-retorno-nok.cod-erro    = 1
                           tt-retorno-nok.desc-erro   = "Pedido de Compra n∆o localizado com o n£mero " + STRING(tt-imp-pedido-compr.num-pedido)
                           tt-retorno-nok.sequencia   = 2
                           tt-retorno-nok.UniqueName  = pedido-compr.char-2.                    
                END.
            END.
            WHEN 3 THEN DO:
                FIND FIRST pedido-compr NO-LOCK
                    WHERE pedido-compr.num-pedido = tt-imp-pedido-compr.num-pedido NO-ERROR.
                IF NOT AVAIL pedido-compr THEN DO:
                    CREATE tt-retorno-nok.
                    ASSIGN tt-retorno-nok.data        = NOW
                           tt-retorno-nok.cod-erro    = 1
                           tt-retorno-nok.desc-erro   = "Pedido de Compra n∆o localizado com o n£mero " + STRING(tt-imp-pedido-compr.num-pedido)
                           tt-retorno-nok.sequencia   = 3
                           tt-retorno-nok.UniqueName  = pedido-compr.char-2.
                    
                END.
            END.
            OTHERWISE DO:
                CREATE tt-retorno-nok.
                ASSIGN tt-retorno-nok.data        = NOW
                       tt-retorno-nok.cod-erro    = 1
                       tt-retorno-nok.desc-erro   = "N∆o identificada a operaá∆o a ser realizada na TAG <ind-tipo-movto>."
                       tt-retorno-nok.sequencia   = 4
                       tt-retorno-nok.UniqueName  = pedido-compr.char-2.
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
    ASSIGN tt-retorno-nok.data       = NOW
           tt-retorno-nok.cod-erro   = 1
           tt-retorno-nok.desc-erro  = "Dados inv†lidos no XML/JSON"
           tt-retorno-nok.sequencia  = 5
           tt-retorno-nok.UniqueName = "".
        
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
DEF VAR d-total-pedido    AS DECIMAL           NO-UNDO.
DEF VAR d-total-item      AS DECIMAL           NO-UNDO.
DEF VAR cOrderSituation   AS CHARACTER         NO-UNDO.
DEF VAR iErrorCode        AS INTEGER INITIAL 5 NO-UNDO.

DO TRANSACTION ON ERROR UNDO, RETURN ON STOP UNDO, RETURN:

    CREATE tt-versao-integr.
    ASSIGN tt-versao-integr.cod-versao-integracao = 001
           tt-versao-integr.ind-origem-msg        = 01.

    FIND FIRST tt-imp-pedido-compr NO-LOCK NO-ERROR.
    IF AVAIL tt-imp-pedido-compr THEN
        
        FOR FIRST pedido-compr 
            WHERE pedido-compr.num-pedido = tt-imp-pedido-compr.num-pedido NO-LOCK:

        EMPTY TEMP-TABLE tt-pedido-compr.
        EMPTY TEMP-TABLE tt-prazo-compra.
        EMPTY TEMP-TABLE tt-ordem-compra.
        EMPTY TEMP-TABLE tt-cotacao-item.
        EMPTY TEMP-TABLE tt-erros-geral.
        RUN pi-04-geraPedidoCompra.

        FOR EACH ordem-compra 
           WHERE ordem-compra.num-pedido = tt-imp-pedido-compr.num-pedido NO-LOCK:

            /************************************************
             25.06 - CPAS
             Regra para n∆o permitir eliminar uma ordem 
             j† eliminada ou recebida
            ************************************************/
            IF ordem-compra.situacao = 4 /*Eliminada*/ OR 
               ordem-compra.situacao = 6 /*Recebida*/ THEN
            DO:
                
                ASSIGN cOrderSituation = {ininc/i02in274.i 4 ordem-compra.situacao}.

                MESSAGE SUBSTITUTE("Ordem: &1 - Situaá∆o: &2", ordem-compra.numero-ordem,cOrderSituation).

                CREATE tt-retorno-nok.
                ASSIGN tt-retorno-nok.data       = NOW
                       tt-retorno-nok.cod-erro   = 1
                       tt-retorno-nok.desc-erro  = SUBSTITUTE("Ordem &1 j† &2. Operaá∆o n∆o permitida",ordem-compra.numero-ordem,cOrderSituation)
                       tt-retorno-nok.sequencia  = iErrorCode
                       tt-retorno-nok.UniqueName = pedido-compr.char-2.


            END.
            
            IF NOT TEMP-TABLE tt-retorno-nok:HAS-RECORDS THEN
            DO:
                MESSAGE SUBSTITUTE("Ordem: &1 - Sem Erro. Gerando ttOrdem e ttPrazoCompra", ordem-compra.numero-ordem).
                RUN pi-05-geraOrdemCompra.

                FOR EACH prazo-compra WHERE prazo-compra.numero-ordem  = ordem-compra.numero-ordem  NO-LOCK:

                    /*-- item recebimento fiscal --*/
                    FOR FIRST item-doc-est USE-INDEX itmdctst-09 NO-LOCK
                        WHERE item-doc-est.num-pedido   = pedido-compr.num-pedido
                          AND item-doc-est.numero-ordem = ordem-compra.numero-ordem
                          AND item-doc-est.parcela      = prazo-compra.parcela: 
                    END.

                    /*-- item recebimento fisico --*/
                    FOR FIRST it-doc-fisico NO-LOCK
                        WHERE it-doc-fisico.num-pedido   = pedido-compr.num-pedido
                          AND it-doc-fisico.numero-ordem = ordem-compra.numero-ordem:
                    END.

                    IF AVAIL item-doc-est  OR 
                       AVAIL it-doc-fisico THEN
                    DO:
                        CREATE tt-retorno-nok.
                        ASSIGN tt-retorno-nok.data       = NOW
                               tt-retorno-nok.cod-erro   = 1
                               tt-retorno-nok.desc-erro  = SUBSTITUTE("Ordem &1 possui documento lanáado no RE1001. Operaá∆o n∆o permitida",ordem-compra.numero-ordem)
                               tt-retorno-nok.sequencia  = iErrorCode
                               tt-retorno-nok.UniqueName = pedido-compr.char-2.
                    END.
                    ELSE RUN pi-06-geraPrazoCompra.

                END.
            END.

            //RUN pi-07-geraCotacaoItem.
            
        END.
        

        IF NOT TEMP-TABLE tt-erros-geral:HAS-RECORDS THEN
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
        DO:
            MESSAGE "Ha Erro na tentativa de eliminaá∆o pedido/ordem".

            RUN pi-08-log-erros.
        END.
        ELSE DO:

            FOR FIRST tt-pedido-compr NO-LOCK:

                RUN pi-10-calc-total-pedido (INPUT  tt-imp-pedido-compr.num-pedido-totvs,
                                             OUTPUT d-total-item,
                                             OUTPUT d-total-pedido).
    
                CREATE tt-imp-pedido-compr-retorno.
                ASSIGN tt-imp-pedido-compr-retorno.num-pedido         = pedido-compr.num-pedido
                       tt-imp-pedido-compr-retorno.dt-inclusao        = NOW
                       tt-imp-pedido-compr-retorno.qt-pedido          = d-total-item
                       tt-imp-pedido-compr-retorno.vl-pedido          = d-total-pedido
                       tt-imp-pedido-compr-retorno.NumberInCollection = "1"
                       tt-imp-pedido-compr-retorno.SAPPOLineNumber    = "00010"
                       tt-imp-pedido-compr-retorno.UniqueName         = STRING(pedido-compr.char-2).

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

IF CAN-FIND (FIRST ttError NO-LOCK) THEN
    TEMP-TABLE ttError:WRITE-JSON("LONGCHAR", json_retorno, TRUE).
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

    CREATE tt-pedido-compr.
    ASSIGN tt-pedido-compr.num-pedido     = tt-imp-pedido-compr.num-pedido
           tt-pedido-compr.char-2         = pedido-compr.char-2
           tt-pedido-compr.ind-tipo-movto = tt-imp-pedido-compr.ind-tipo-movto
           tt-pedido-compr.situacao       = 3  /*Eliminado*/          
           tt-pedido-compr.cod-estabel   =  pedido-compr.cod-estabel  
           tt-pedido-compr.cod-emitente  =  pedido-compr.cod-emitente 
           tt-pedido-compr.cod-cond-pag  =  pedido-compr.cod-cond-pag 
           tt-pedido-compr.natureza      =  pedido-compr.natureza     
           tt-pedido-compr.data-pedido   =  pedido-compr.data-pedido  
           tt-pedido-compr.frete         =  pedido-compr.frete        
           tt-pedido-compr.cod-transp    =  pedido-compr.cod-transp
           tt-pedido-compr.responsavel   =  pedido-compr.responsavel  
           tt-pedido-compr.comentarios   =  pedido-compr.comentarios  
           tt-pedido-compr.mot-elimina   =  pedido-compr.mot-elimina  
           tt-pedido-compr.contr-forn    =  pedido-compr.contr-forn   
           tt-pedido-compr.compl-entrega =  pedido-compr.compl-entrega
           tt-pedido-compr.end-entrega   =  pedido-compr.end-entrega  
           tt-pedido-compr.end-cobranca  =  pedido-compr.end-cobranca 
           tt-pedido-compr.via-transp    =  pedido-compr.via-transp.
           

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

    CREATE tt-ordem-compra.
    ASSIGN tt-ordem-compra.num-pedido     = ordem-compra.num-pedido
           tt-ordem-compra.numero-ordem   = ordem-compra.numero-ordem
           tt-ordem-compra.ind-tipo-movto = 2 /*Alteracao*/
           tt-ordem-compra.situacao       = 4 /*Eliminada*/                                      
           tt-ordem-compra.cod-estabel    = ordem-compra.cod-estabel 
           tt-ordem-compra.cod-emitente   = ordem-compra.cod-emitente
           tt-ordem-compra.it-codigo      = ordem-compra.it-codigo   
           tt-ordem-compra.cod-comprado   = ordem-compra.cod-comprado
           tt-ordem-compra.requisitante   = ordem-compra.requisitante
           tt-ordem-compra.dep-almoxar    = ordem-compra.dep-almoxar 
           tt-ordem-compra.preco-fornec   = ordem-compra.preco-fornec
           tt-ordem-compra.qt-solic       = ordem-compra.qt-solic    
           tt-ordem-compra.ct-codigo      = ordem-compra.ct-codigo   
           tt-ordem-compra.sc-codigo      = ordem-compra.sc-codigo   
           tt-ordem-compra.tp-despesa     = ordem-compra.tp-despesa  
           tt-ordem-compra.ordem-servic   = ordem-compra.ordem-servic
           tt-ordem-compra.natureza       = ordem-compra.natureza    
           tt-ordem-compra.num-ord-inv    = ordem-compra.num-ord-inv 
           tt-ordem-compra.pre-unit-for   = ordem-compra.pre-unit-for
           tt-ordem-compra.mo-codigo      = ordem-compra.mo-codigo   
           tt-ordem-compra.Codigo-ipi     = ordem-compra.Codigo-ipi  
           tt-ordem-compra.Aliquota-ipi   = ordem-compra.Aliquota-ipi
           tt-ordem-compra.Codigo-icm     = ordem-compra.Codigo-icm  
           tt-ordem-compra.Aliquota-icm   = ordem-compra.Aliquota-icm
           tt-ordem-compra.Aliquota-iss   = ordem-compra.Aliquota-iss
           tt-ordem-compra.Valor-frete    = ordem-compra.Valor-frete 
           tt-ordem-compra.cod-cond-pag   = ordem-compra.cod-cond-pag
           tt-ordem-compra.Usuario        = ordem-compra.Usuario
           tt-ordem-compra.cod-unid-negoc = ordem-compra.cod-unid-negoc
           tt-ordem-compra.cod-transp     = ordem-compra.cod-transp.
    
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

    CREATE tt-prazo-compra.
    ASSIGN tt-prazo-compra.numero-ordem   = prazo-compra.numero-ordem
           tt-prazo-compra.ind-tipo-movto = 2 /*Alteracao*/    
           tt-prazo-compra.situacao       = 4 /*Eliminada*/   /*-- CPAS Valor Anterior Cotada (3) --*/ 
           tt-prazo-compra.data-entrega   = prazo-compra.data-entrega 
           tt-prazo-compra.natureza       = prazo-compra.natureza     
           tt-prazo-compra.un             = prazo-compra.un           
           tt-prazo-compra.parcela        = prazo-compra.parcela      
           tt-prazo-compra.it-codigo      = prazo-compra.it-codigo    
           tt-prazo-compra.quantidade     = prazo-compra.quantidade   
           tt-prazo-compra.quant-saldo    = prazo-compra.quantidade  
           tt-prazo-compra.quantid-orig   = prazo-compra.quantidade 
           tt-prazo-compra.qtd-sal-forn   = prazo-compra.quantidade.
           

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

    CREATE tt-cotacao-item.
    ASSIGN tt-cotacao-item.numero-ordem = ordem-compra.numero-ordem
           tt-cotacao-item.cod-emitente = tt-imp-ordem-compra.cod-emitente
           tt-cotacao-item.it-codigo    = tt-imp-ordem-compra.it-codigo   
           tt-cotacao-item.cod-transp   = tt-imp-pedido-compr.cod-transp
           tt-cotacao-item.preco-fornec = tt-imp-ordem-compra.preco-fornec
           tt-cotacao-item.mo-codigo    = tt-imp-ordem-compra.mo-codigo   
           tt-cotacao-item.data-cotacao = TODAY
           tt-cotacao-item.preco-unit   = tt-imp-ordem-compra.pre-unit-for  
           tt-cotacao-item.pre-unit-for = tt-imp-ordem-compra.pre-unit-for
           tt-cotacao-item.codigo-ipi   = tt-imp-ordem-compra.codigo-ipi  
           tt-cotacao-item.aliquota-ipi = tt-imp-ordem-compra.aliquota-ipi
           tt-cotacao-item.codigo-icm   = tt-imp-ordem-compra.codigo-icm  
           tt-cotacao-item.aliquota-icm = tt-imp-ordem-compra.aliquota-icm
           tt-cotacao-item.aliquota-iss = tt-imp-ordem-compra.aliquota-iss
           tt-cotacao-item.valor-frete  = tt-imp-ordem-compra.valor-frete 
           tt-cotacao-item.cod-cond-pag = tt-imp-pedido-compr.cod-cond-pag
           tt-cotacao-item.contato      = tt-imp-ordem-compra.requisitante
           tt-cotacao-item.cod-comprado = tt-imp-ordem-compra.requisitante
           tt-cotacao-item.hora-atualiz = STRING(TIME,'HH:MM').

    IF tt-cotacao-item.Valor-frete > 0 THEN
        ASSIGN tt-cotacao-item.Frete = YES.

    IF tt-imp-pedido-compr.cod-transp = 0 THEN
        ASSIGN tt-cotacao-item.cod-transp = 1.
    
    
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

    //CREATE tt-retorno-nok.
    //ASSIGN tt-retorno-nok.data        = NOW
    //       tt-retorno-nok.cod-erro    = tt-erros-geral.cod-erro
    //       tt-retorno-nok.desc-erro   = tt-erros-geral.des-erro
    //       tt-retorno-nok.sequencia   = 1
    //       tt-retorno-nok.UniqueName  = STRING(pedido-compr.char-2).

    CREATE ttError.
    ASSIGN ttError.referencia  = "TOTVS"
           ttError.codigo      = STRING(tt-erros-geral.cod-erro)  
           ttError.desc-erro   = tt-erros-geral.des-erro   
           ttError.UniqueName  = STRING(pedido-compr.char-2).

    
END.

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


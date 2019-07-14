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

//{include/i-prgvrs.i paymentTerms 2.00.00.000} /*** 010000 ***/
DEF TEMP-TABLE tt-budgetcheck NO-UNDO SERIALIZE-NAME "BudgetCheck"
    FIELD tp-verificacao          AS INT
    FIELD tp-requisicao           AS CHAR
    FIELD sequencia               AS INT
    FIELD num-req-ariba           AS CHAR
    FIELD Num-ord-inv             AS CHAR
    FIELD Usuario                 AS CHAR
    FIELD vl-investimento         AS DECIMAL.
    

DEF TEMP-TABLE tt-budgetcheck-ok NO-UNDO SERIALIZE-NAME "BudgetCheckRetorno"
    FIELD sequencia               AS INT
    FIELD num-req-ariba           AS CHAR.

DEF TEMP-TABLE tt-budgetcheck-nok NO-UNDO SERIALIZE-NAME "BudgetCheckRetorno"
    FIELD sequencia               AS INT
    FIELD num-req-ariba           AS CHAR
    FIELD cod-erro                AS INT
    FIELD desc-erro               AS CHAR
    FIELD data                    AS DATETIME.

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

DEFINE VARIABLE oRequestParser AS JsonAPIRequestParser NO-UNDO.
DEFINE VARIABLE jsonRetorno AS JsonArray NO-UNDO.

oRequestParser = NEW JsonAPIRequestParser(jsonInput).
 
ASSIGN json_recebido = oRequestParser:getPayloadLongChar().

MESSAGE ">>>>>>" STRING(json_recebido).

OUTPUT TO /totvs/erp/camil/teste-rosa/log_appserver/jsonbudgetcheck.json.

    EXPORT json_recebido.

OUTPUT CLOSE.

lretOK = TEMP-TABLE tt-budgetcheck:READ-JSON("LONGCHAR", json_recebido, "empty") NO-ERROR.

IF lretOK = NO THEN DO:

    CREATE tt-budgetcheck-nok.
    ASSIGN tt-budgetcheck-nok.sequencia     = 1
           tt-budgetcheck-nok.num-req-ariba = ""
           tt-budgetcheck-nok.data          = NOW
           tt-budgetcheck-nok.cod-erro      = 0
           tt-budgetcheck-nok.desc-erro     = "N∆o foi poss°vel fazer o parse do arquivo no TOTVS".
    
    CREATE ttError.
    ASSIGN ttError.SessionId   = ""
           ttError.referencia  = "TOTVS"
           ttError.codigo      = "0"
           ttError.descricao   = "N∆o foi poss°vel fazer o parse do arquivo. Integraá∆o Pedido de Compra.".
    
END.
ELSE
    // Validar os registros e processar 
    RUN pi-01-valida.
    
// Retornar os dados da integracao
//RUN pi-03-retorno.

IF CAN-FIND (FIRST ttError NO-LOCK) THEN DO:
    ASSIGN jsonRetorno = NEW JsonArray().
           jsonRetorno:Read(TEMP-TABLE tt-budgetcheck-nok:HANDLE).
END.
ELSE DO:

    IF CAN-FIND (FIRST tt-budgetcheck-ok NO-LOCK) THEN DO:
    
        ASSIGN jsonRetorno = NEW JsonArray().
               jsonRetorno:Read(TEMP-TABLE tt-budgetcheck-ok:HANDLE).

    END.
    ELSE DO:
    
        ASSIGN jsonRetorno = NEW JsonArray().
               jsonRetorno:Read(TEMP-TABLE tt-budgetcheck-nok:HANDLE).

    END.

               
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
EMPTY TEMP-TABLE ttError.

RUN pi-02-processa.

/*IF CAN-FIND (FIRST tt-imp-pedido-compr) THEN DO:
    FOR FIRST tt-imp-pedido-compr.

        /*INCLUS«O*/
        ASSIGN tt-imp-pedido-compr.ind-tipo-movto = 1.

        // Validar a operaá∆o
        CASE tt-imp-pedido-compr.ind-tipo-movto:
            WHEN 1 THEN DO: // Inclusao de registro

                MESSAGE "clf aqui"
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.

                // Verificar se o codigo do emitente j† existe cadastrado 
                IF tt-imp-pedido-compr.num-pedido-totvs <> 0 THEN DO:
                    FIND FIRST pedido-compr NO-LOCK
                        WHERE STRING(pedido-compr.num-pedido) = tt-imp-pedido-compr.num-pedido NO-ERROR.
                    IF AVAIL pedido-compr THEN DO:
                        CREATE ttError.
                        ASSIGN //ttError.SessionId   = p-session 
                               ttError.referencia  = "TOTVS"
                               ttError.codigo      = "1"
                               ttError.descricao   = "Pedido de Compra j† existe com o n£mero " + STRING(tt-imp-pedido-compr.num-pedido).

                    END.
                END.
            END.
            WHEN 2 THEN DO:
                FIND FIRST pedido-compr NO-LOCK
                    WHERE STRING(pedido-compr.num-pedido) = tt-imp-pedido-compr.num-pedido NO-ERROR.
                IF NOT AVAIL pedido-compr THEN DO:
                    CREATE ttError.
                    ASSIGN //ttError.SessionId   = p-session 
                           ttError.referencia  = "TOTVS"
                           ttError.codigo      = "2"
                           ttError.descricao   = "Ordem de Compra n∆o localizada com o n£mero " + STRING(tt-imp-pedido-compr.num-pedido).
                    
                END.
            END.
            WHEN 3 THEN DO:
                FIND FIRST pedido-compr NO-LOCK
                    WHERE STRING(pedido-compr.num-pedido) = tt-imp-pedido-compr.num-pedido NO-ERROR.
                IF NOT AVAIL pedido-compr THEN DO:
                    CREATE ttError.
                    ASSIGN //ttError.SessionId   = p-session 
                           ttError.referencia  = "TOTVS"
                           ttError.codigo      = "3"
                           ttError.descricao   = "Ordem de Compra n∆o localizada com o n£mero " + STRING(tt-imp-pedido-compr.num-pedido).
                    
                END.
            END.
            OTHERWISE DO:
                CREATE ttError.
                ASSIGN //ttError.SessionId   = p-session 
                       ttError.referencia  = "TOTVS"
                       ttError.codigo      = "4"
                       ttError.descricao   = "N∆o identificada a operaá∆o a ser realizada na TAG <ind-tipo-movto>.".
                
            END.
        END CASE.

        // Caso n∆o localize os erros, processar a integraá∆o 
        IF NOT CAN-FIND(FIRST ttError) THEN DO:

            MESSAGE "clf 2"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

            RUN pi-02-processa.

        END.
            
    END.

END.
ELSE DO:
    
    CREATE ttError.
    ASSIGN //ttError.SessionId   = p-session 
           ttError.referencia  = "TOTVS"
           ttError.codigo      = "5"
           ttError.descricao   = "Dados invalidos no XML/JSON".
    
END.
*/

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

FOR EACH tt-budgetcheck NO-LOCK:

    IF tt-budgetcheck.num-ord-inv = ?  OR 
       tt-budgetcheck.num-ord-inv = "" THEN DO:

        MESSAGE "CLF>>>>> SEM NUMERO ORDEM"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        FIND FIRST tt-budgetcheck-ok NO-LOCK NO-ERROR.
        IF NOT AVAIL tt-budgetcheck-ok THEN DO:

            CREATE tt-budgetcheck-ok.
            ASSIGN tt-budgetcheck-ok.sequencia     = tt-budgetcheck.sequencia
                   tt-budgetcheck-ok.num-req-ariba = tt-budgetcheck.num-req-ariba.

        END.

    END.
    ELSE IF tt-budgetcheck.tp-requisicao = "CUS_REQ_07" /*ORDEM MANUTENCAO*/ THEN DO:

        FIND FIRST ord-manut WHERE 
                   ord-manut.nr-ord-produ = INT(tt-budgetcheck.num-ord-inv) NO-LOCK NO-ERROR.
        IF AVAIL ord-manut THEN DO:

            MESSAGE "CLF ORDEM MANUTENÄ«O>>> OK"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

            FIND FIRST tt-budgetcheck-ok NO-LOCK NO-ERROR.
            IF NOT AVAIL tt-budgetcheck-ok THEN DO:
                
                CREATE tt-budgetcheck-ok.
                ASSIGN tt-budgetcheck-ok.sequencia     = tt-budgetcheck.sequencia
                       tt-budgetcheck-ok.num-req-ariba = tt-budgetcheck.num-req-ariba.

            END.
    
        END.
        ELSE DO:

            MESSAGE "CLF ORDEM MANUTENÄ«O>>> NOK"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            
            CREATE tt-budgetcheck-nok.
            ASSIGN tt-budgetcheck-nok.sequencia     = tt-budgetcheck.sequencia
                   tt-budgetcheck-nok.num-req-ariba = tt-budgetcheck.num-req-ariba
                   tt-budgetcheck-nok.data          = NOW
                   tt-budgetcheck-nok.cod-erro      = 1
                   tt-budgetcheck-nok.desc-erro     = "Numero de Ordem de Manutená∆o N«O Ç V†lida. Linha: " + STRING(tt-budgetcheck.sequencia).
    
        END.

    END.
    ELSE DO:

        IF tt-budgetcheck.tp-verificacao = 1 /*VALIDAR SALDO ORDEM INVESTIMENTO*/ THEN DO:


            FIND FIRST sub-div-ordem WHERE 
                       sub-div-ordem.num-ord-magnus = INT(tt-budgetcheck.num-ord-inv) NO-LOCK NO-ERROR.

            FIND FIRST ordem-inv OF sub-div-ordem NO-LOCK NO-ERROR.
            IF AVAIL ordem-inv THEN DO:

                FIND FIRST user-inv WHERE
                           //user-inv.num-ordem = ordem-inv.num-ordem AND
                           user-inv.usuario      = tt-budgetcheck.usuario AND
                           user-inv.dt-desat     = ?                      NO-LOCK NO-ERROR.
                IF AVAIL user-inv THEN DO:
    
                    MESSAGE "CLF>>>>> OK ORDEM INVESTIMENTO USUARIO"
                        VIEW-AS ALERT-BOX INFO BUTTONS OK.

                    FIND FIRST tt-budgetcheck-ok NO-LOCK NO-ERROR.
                    IF NOT AVAIL tt-budgetcheck-ok THEN DO:
        
                        CREATE tt-budgetcheck-ok.
                        ASSIGN tt-budgetcheck-ok.sequencia     = tt-budgetcheck.sequencia
                               tt-budgetcheck-ok.num-req-ariba = tt-budgetcheck.num-req-ariba.

                    END.
                    
                END.
                ELSE DO:
    
                    MESSAGE "CLF>>>>> NOK ORDEM INVESTIMENTO USUARIO"
                        VIEW-AS ALERT-BOX INFO BUTTONS OK.
        
                    CREATE tt-budgetcheck-nok.
                    ASSIGN tt-budgetcheck-nok.sequencia     = tt-budgetcheck.sequencia
                           tt-budgetcheck-nok.num-req-ariba = tt-budgetcheck.num-req-ariba
                           tt-budgetcheck-nok.data          = NOW
                           tt-budgetcheck-nok.cod-erro      = 2
                           tt-budgetcheck-nok.desc-erro     = "Usu†rio N«O habilitado para efetuar Compra para Ordem de Investimento. Linha: " + STRING(tt-budgetcheck.sequencia).
        
                END.

            END.
            ELSE DO:

                MESSAGE "CLF>>>>> NOK ORDEM INVESTIMENTO NUMERO"
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.

                CREATE tt-budgetcheck-nok.
                ASSIGN tt-budgetcheck-nok.sequencia     = tt-budgetcheck.sequencia
                       tt-budgetcheck-nok.num-req-ariba = tt-budgetcheck.num-req-ariba
                       tt-budgetcheck-nok.data          = NOW
                       tt-budgetcheck-nok.cod-erro      = 4
                       tt-budgetcheck-nok.desc-erro     = "Ordem de Investimento N«O encontrada. Linha: " + STRING(tt-budgetcheck.sequencia).
    
            END.

        END.
        ELSE IF tt-budgetcheck.tp-verificacao = 2 /*VALIDAR SALDO ORDEM INVESTIMENTO*/ THEN DO:

            FIND FIRST sub-div-ordem WHERE 
                       sub-div-ordem.num-ord-magnus = INT(tt-budgetcheck.num-ord-inv) NO-LOCK NO-ERROR.

            FIND FIRST ordem-inv OF sub-div-ordem NO-LOCK NO-ERROR.
            IF AVAIL ordem-inv THEN DO:

                FIND FIRST controle-verba OF ordem-inv NO-LOCK NO-ERROR.
                IF AVAIL controle-verba AND 
                   tt-budgetcheck.vl-investimento <= (controle-verba.vl-verba[1] - controle-verba.vl-comp[1]) THEN DO:
    
                    MESSAGE "CLF>>>>> OK ORDEM INVESTIMENTO SALDO"
                        VIEW-AS ALERT-BOX INFO BUTTONS OK.
                    
                    FIND FIRST tt-budgetcheck-ok NO-LOCK NO-ERROR.
                    IF NOT AVAIL tt-budgetcheck-ok THEN DO:
        
                        CREATE tt-budgetcheck-ok.
                        ASSIGN tt-budgetcheck-ok.sequencia     = tt-budgetcheck.sequencia
                               tt-budgetcheck-ok.num-req-ariba = tt-budgetcheck.num-req-ariba.

                    END.
        
                END.
                ELSE DO:
    
                    MESSAGE "CLF>>>>> NOK ORDEM INVESTIMENTO SALDO"
                        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    
                    CREATE tt-budgetcheck-nok.
                    ASSIGN tt-budgetcheck-nok.sequencia     = tt-budgetcheck.sequencia
                           tt-budgetcheck-nok.num-req-ariba = tt-budgetcheck.num-req-ariba
                           tt-budgetcheck-nok.data          = NOW
                           tt-budgetcheck-nok.cod-erro      = 4
                           tt-budgetcheck-nok.desc-erro     = "Ordem de Investimento N«O possu° SALDO para a Requisiá∆o. Linha: " + STRING(tt-budgetcheck.sequencia).
        
                END.

            END.
            ELSE DO:
    
                MESSAGE "CLF>>>>> NOK ORDEM INVESTIMENTO N«O ENCONTRADA"
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.

                CREATE tt-budgetcheck-nok.
                ASSIGN tt-budgetcheck-nok.sequencia     = tt-budgetcheck.sequencia
                       tt-budgetcheck-nok.num-req-ariba = tt-budgetcheck.num-req-ariba
                       tt-budgetcheck-nok.data          = NOW
                       tt-budgetcheck-nok.cod-erro      = 4
                       tt-budgetcheck-nok.desc-erro     = "Ordem de Investimento N«O encontrada. Linha: " + STRING(tt-budgetcheck.sequencia).
        
            END.

    
        END.

    END.

END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


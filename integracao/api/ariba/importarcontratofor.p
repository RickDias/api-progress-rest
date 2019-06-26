&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : importarcontratofor.p
    Purpose     : Integraáao - ARIBA x Contrato Compra

    Syntax      :

    Description :

    Author(s)   : TOTVS
    Created     : 05/2019
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* // O campo ind-tipo-movto trata qual movimento esta realizando  */
/* // 1 - Inclusao / 2 - Alteracao / 3 - Exclusao                  */

{utp/ut-api.i}
{utp/ut-api-utils.i}
{cdp/cdcfgmat.i}

/*DIRETORIO ESPECIFICO*/


/* //{include/i-prgvrs.i paymentTerms 2.00.00.000} /*** 010000 ***/  */
DEFINE TEMP-TABLE tt-versao-integr 
    FIELD cod-versao-integracaoo AS INTEGER
    FIELD ind-origem-msg         AS INTEGER.

DEF TEMP-TABLE tt-imp-contrato-for NO-UNDO SERIALIZE-NAME 'Contrato_Compra'
    FIELD nr-contrato           AS CHAR
    FIELD cod-emitente          AS INT
    FIELD des-contrat           AS CHAR
    FIELD cod-cond-pag          AS INT
    FIELD val-total             AS DEC FORMAT "->>>,>>>,>>9.9999"
    FIELD cod-comprador         AS CHAR
    FIELD ind-sit-contrat       AS INT 
    FIELD qtd-total             AS DEC FORMAT "->>>>>,>>>,>>9.9999"
    FIELD sld-qtd               AS DEC FORMAT "->>>>>,>>>,>>9.9999"
    FIELD sld-val               AS DEC FORMAT "->>>,>>>,>>>.9999"
    FIELD acum-rec-qtd          AS DEC FORMAT "->>>>>,>>>,>>9.9999"
    FIELD acum-rec-val          AS DEC FORMAT "->>>>>,>>>,>>9.9999"
    FIELD sld-qtd-liber         AS DEC FORMAT "->>>>>,>>>,>>9.9999"
    FIELD sld-val-liber         AS DEC FORMAT "->>>>>,>>>,>>9.9999"
    FIELD val-fatur-minimo      AS DEC FORMAT ">>>,>>>,>>9.9999"
    FIELD dt-ini-validade       AS DATE
    FIELD dt-ter-validade       AS DATE
    FIELD cod-estabel           AS CHAR
    FIELD cod-estab-cobr        AS CHAR
    FIELD cod-estab-entr        AS CHAR
    FIELD acum-val-pago         AS DEC FORMAT ">>>,>>>,>>9.9999"
    FIELD mo-codigo             AS INT
    FIELD log-libera            AS CHAR FORMAT "x(01)"
    FIELD sld-qtd-med           AS DEC FORMAT "->>>>>,>>>,>>9.9999"
    FIELD sal-qtd-liber-med     AS DEC FORMAT "->>>>>,>>>,>>9.9999"
    FIELD sld-val-med           AS DEC FORMAT "->>>,>>>,>>9.999"
    FIELD sld-val-liber-med     AS DEC FORMAT "->>>,>>>,>>9.9999"
    FIELD cod-projeto           AS CHAR
    FIELD contato               AS CHAR
    FIELD narrat-contrat        AS CHAR
    /*Codigo do Registro (MC00)*/
    FIELD ct-codigo             AS CHAR
    FIELD sc-codigo             AS CHAR
    FIELD perc-rateio           AS DEC FORMAT ">>9.99"
    FIELD cod-unid-negoc        AS CHAR
    FIELD ind-tipo-movto        AS INTEGER FORMAT "99".

DEF TEMP-TABLE tt-imp-item-contrato NO-UNDO SERIALIZE-NAME "Item_Contrato"
    /*Codigo do Registro (IC00)*/
    FIELD nr-contrato           AS CHAR
    FIELD num-seq-item          AS INT
    FIELD it-codigo             AS CHAR
    FIELD un                    AS CHAR
    FIELD preco-unit            AS DEC FORMAT ">>>,>>>,>>9.99999"
    FIELD qtd-minima            AS DEC FORMAT ">>>>,>>9.9999"
    FIELD val-fatur-minimo      AS DEC FORMAT ">>>,>>>,>>9.9999"
    FIELD mo-codigo             AS INT
    FIELD log-libera            AS CHAR FORMAT "x(01)"
    FIELD val-total             AS DEC FORMAT "->>>,>>>,>>9.99999"
    FIELD ipi-incluso           AS CHAR FORMAT "x(01)"
    FIELD codigo-icm            AS INT /*1-Tributado 2-Isento 3-Reduzido 4-Outros*/
    FIELD qtd-total             AS DEC FORMAT "->>>>>,>>>,>>9.9999"
    FIELD ind-un-contrato       AS INT
    FIELD sld-qtd               AS DEC FORMAT "->>>>>,>>>,>>9.9999"
    FIELD acum-rec-val          AS DEC FORMAT "->>>>>,>>>,>>9.9999"
    FIELD acum-rec-qtd          AS DEC FORMAT "->>>>>,>>>,>>9.9999"
    FIELD aliquota-icm          AS DEC FORMAT ">>9.99"
    FIELD aliquota-ipi          AS DEC FORMAT ">>9.99"
    FIELD tp-despesa            AS INT
    FIELD cod-cond-pag          AS INT
    FIELD frete                 AS CHAR FORMAT "x(01)"
    FIELD contato               AS CHAR
    FIELD cod-emitente          AS INT
    /*Codigo do Registro (IC01)*/
    FIELD preco-fornec          AS DEC FORMAT ">>>>>,>>>,>>9.99999"
    FIELD val-frete             AS DEC FORMAT ">>>,>>>,>>9.9999"
    FIELD prazo-ent             AS INT
    FIELD preco-base            AS DEC FORMAT ">>>>>,>>>,>>9.99999"
    FIELD cod-comprador         AS CHAR
    FIELD perc-desconto         AS DEC FORMAT ">>9.99"
    FIELD narrat-compra         AS CHAR
    FIELD pre-unit-for          AS DEC FORMAT ">>>>>,>>>,>>9.99999"
    FIELD sld-qtd-receb         AS DEC FORMAT "->>>>>,>>9.9999"
    FIELD sld-val-receb         AS DEC FORMAT "->>>>>,>>9.9999"
    FIELD narrat-item           AS CHAR
    /*Codigo do Registro (MI00)*/
    FIELD ct-codigo             AS CHAR
    FIELD sc-codigo             AS CHAR
    FIELD perc-rateio           AS DEC FORMAT ">>9.99"
    FIELD cod-unid-negoc        AS CHAR
    FIELD ind-tipo-movto        AS INTEGER FORMAT "99".

DEF TEMP-TABLE tt-imp-contrato-retorno NO-UNDO SERIALIZE-NAME "Contrato_Retorno"
    FIELD nr-contrato           AS CHAR      
    FIELD val-total             AS DEC FORMAT "->>>,>>>,>>9.9999"     
    FIELD dt-inclusao           AS DATETIME.

DEFINE TEMP-TABLE tt-erros-geral 
    FIELD identif-msg        AS CHARACTER 
    FIELD num-sequencia-erro AS INTEGER
    FIELD cod-erro           AS INTEGER
    FIELD des-erro           AS CHARACTER FORMAT "X(200)"
    FIELD cod-maq-origem     AS INTEGER 
    FIELD num-processo       AS INTEGER.


DEFINE DATASET httContratoCompra SERIALIZE-HIDDEN  FOR tt-imp-contrato-for, tt-imp-item-contrato
    DATA-RELATION dr-ContratoCompra FOR tt-imp-contrato-for, tt-imp-item-contrato
       RELATION-FIELDS (Nr-Contrato, Nr-Contrato)  NESTED.   

/* DEFINE DATASET httContratoCompra SERIALIZE-HIDDEN  FOR tt-imp-pedido-compr, tt-imp-ordem-compra, tt-imp-prazo-compra  */
/*     DATA-RELATION dr-OrdemCompra FOR tt-imp-pedido-compr, tt-imp-ordem-compra                                         */
/*        RELATION-FIELDS (num-pedido, num-pedido)  NESTED                                                               */
/*     DATA-RELATION dr-PrazoCompra FOR tt-imp-ordem-compra, tt-imp-prazo-compra                                         */
/*        RELATION-FIELDS (it-codigo, it-codigo) NESTED .                                                                */

/*ENVIO CONTRATO DE COMPRA*/

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

DEFINE BUFFER estabelec-entrega     FOR estabelec.
DEFINE BUFFER estabelec-faturamento FOR estabelec.


DEFINE NEW GLOBAL SHARED VARIABLE c-seg-usuario AS CHAR FORMAT "x(12)" NO-UNDO.

DEFINE VARIABLE h-acomp AS HANDLE NO-UNDO.
DEFINE VARIABLE lLoop AS LOG NO-UNDO.
DEFINE VARIABLE lRetOK AS LOG NO-UNDO.
DEFINE VARIABLE i-cont AS INT INIT 1 NO-UNDO.

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

OUTPUT TO /totvs/erp/camil/teste-rosa/log_appserver/jsonrecebidocontrato.json.

    EXPORT json_recebido.

OUTPUT CLOSE.

lretOK = DATASET httcontratoCompra:READ-JSON("LONGCHAR", json_recebido, "empty") NO-ERROR.

MESSAGE "lretOK " lretOK
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

IF lretOK = NO THEN DO:
    
    CREATE ttError.
    ASSIGN ttError.SessionId   = ""
           ttError.referencia  = "TOTVS"
           ttError.codigo      = "0"
           ttError.descricao   = "N∆o foi poss°vel fazer o parse do arquivo. Integraá∆o Contrato de Compra.".
    
END.
ELSE
    // Validar os registros e processar 
    RUN pi-01-valida.
    
// Retornar os dados da integracao
//RUN pi-03-retorno.

IF CAN-FIND (FIRST ttError NO-LOCK) THEN DO:
    ASSIGN jsonRetorno = NEW JsonArray().
           jsonRetorno:Read(TEMP-TABLE ttError:HANDLE).
END.
ELSE DO:
    ASSIGN jsonRetorno = NEW JsonArray().
           jsonRetorno:Read(TEMP-TABLE tt-imp-contrato-retorno:HANDLE).
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

IF CAN-FIND (FIRST tt-imp-contrato-for) THEN DO:
    FOR FIRST tt-imp-contrato-for.

        /*INCLUS«O*/
        ASSIGN tt-imp-contrato-for.ind-tipo-movto = 1.

        // Validar a operaá∆o
        CASE tt-imp-contrato-for.ind-tipo-movto:
            WHEN 1 THEN DO: // Inclusao de registro

                // Verificar se o codigo do emitente j† existe cadastrado 
                IF string(tt-imp-contrato-for.nr-contrato) <> "0" AND string(tt-imp-contrato-for.nr-contrato) <> "" THEN DO:
                    FIND FIRST contrato-for NO-LOCK
                        WHERE string(contrato-for.nr-contrato) = tt-imp-contrato-for.nr-contrato NO-ERROR.
                    IF AVAIL contrato-for THEN DO:
                        CREATE ttError.
                        ASSIGN //ttError.SessionId   = p-session 
                               ttError.referencia  = "TOTVS"
                               ttError.codigo      = "1"
                               ttError.descricao   = "Contrato de Compra j† existe com o n£mero " + STRING(tt-imp-contrato-for.nr-contrato).

                    END.
                END.
            END.
            WHEN 2 THEN DO:
                FIND FIRST contrato-for NO-LOCK
                    WHERE string(contrato-for.nr-contrato) = tt-imp-contrato-for.nr-contrato NO-ERROR.
                IF NOT AVAIL contrato-for THEN DO:
                    CREATE ttError.
                    ASSIGN //ttError.SessionId   = p-session 
                           ttError.referencia  = "TOTVS"
                           ttError.codigo      = "2"
                           ttError.descricao   = "Contrato de Compra n∆o localizada com o n£mero " + STRING(tt-imp-contrato-for.nr-contrato).
                    
                END.
            END.
            WHEN 3 THEN DO:
                FIND FIRST contrato-for NO-LOCK
                    WHERE string(contrato-for.nr-contrato) = tt-imp-contrato-for.nr-contrato NO-ERROR.
                IF NOT AVAIL pedido-compr THEN DO:
                    CREATE ttError.
                    ASSIGN //ttError.SessionId   = p-session 
                           ttError.referencia  = "TOTVS"
                           ttError.codigo      = "3"
                           ttError.descricao   = "Contrato de Compra n∆o localizada com o n£mero " + STRING(tt-imp-contrato-for.nr-contrato).
                    
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

DO TRANSACTION ON ERROR UNDO, RETURN ON STOP UNDO, RETURN:
    
    CREATE tt-versao-integr.
    ASSIGN tt-versao-integr.cod-versao-integracao = 001
           tt-versao-integr.ind-origem-msg        = 01.

    FOR EACH tt-imp-contrato-for EXCLUSIVE-LOCK:

        CASE tt-imp-contrato-for.ind-tipo-movto:
            WHEN 1 THEN RUN pi-04-geraContratoCompra.
            WHEN 2 THEN RUN pi-04-geraContratoCompra.
            WHEN 3 THEN RUN pi-04-geraContratoCompra.
        END CASE.

        FOR EACH tt-imp-item-contrato
            WHERE tt-imp-item-contrato.nr-contrato = tt-imp-contrato-for.nr-contrato EXCLUSIVE-LOCK:

            CASE tt-imp-contrato-for.ind-tipo-movto:
                WHEN 1 THEN RUN pi-05-geraItemContrato.
                WHEN 2 THEN RUN pi-05-geraItemContrato.
                WHEN 3 THEN RUN pi-05-geraItemContrato.
            END CASE.
            
        END.

        /*
        IF NOT CAN-FIND(FIRST sfa-export WHERE sfa-export.cd-tipo-integr = 20
                                           AND sfa-export.chave          = STRING(tt-imp-contrato-for.nr-contrato)
                                           AND sfa-export.ind-situacao   < 2) THEN DO: 

            CREATE sfa-export-contrat-ariba.
            ASSIGN sfa-export-contrat-ariba.cd-tipo-integr = 20
                   sfa-export-contrat-ariba.id-movto       = NEXT-VALUE(seq-export)
                   sfa-export-contrat-ariba.nr-contrato    = tt-imp-contrato-for.nr-contrato
                   sfa-export-contrat-ariba.data-movto     = NOW
                   sfa-export-contrat-ariba.tp-integracao  = 1
                   sfa-export-contrat-ariba.c-json         = ?.
    
            CREATE sfa-export.
            ASSIGN sfa-export.ind-tipo-trans = 2
                   sfa-export.id-movto       = sfa-export-contrat-ariba.id-movto
                   sfa-export.cd-tipo-integr = sfa-export-contrat-ariba.cd-tipo-integr
                   sfa-export.chave          = STRING(sfa-export-contrat-ariba.nr-contrato)
                   sfa-export.cod-status     = 0      /* ---- sem status ----*/
                   sfa-export.data-fim       = ?
                   sfa-export.data-inicio    = ?
                   sfa-export.data-movto     = NOW
                   sfa-export.ind-situacao   = 1       /*---- Pendente -----*/.

        END.
        */
        
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
    TEMP-TABLE tt-imp-contrato-retorno:WRITE-JSON("LONGCHAR", json_retorno, TRUE).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-04-geraContratoCompra) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-04-geraContratoCompra Procedure 
PROCEDURE pi-04-geraContratoCompra :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    MESSAGE "Contrato Compra"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

    CREATE contrato-for.
    ASSIGN contrato-for.nr-contrato         = int(tt-imp-contrato-for.nr-contrato)
           contrato-for.cod-emitente        = tt-imp-contrato-for.cod-emitente
           contrato-for.dt-contrato         = tt-imp-contrato-for.dt-ini-validade
           contrato-for.dt-ini-validade     = tt-imp-contrato-for.dt-ini-validade
           contrato-for.dt-ter-validade     = tt-imp-contrato-for.dt-ter-validade
           contrato-for.cod-comprado        = tt-imp-contrato-for.cod-comprador
           contrato-for.cod-cond-pag        = tt-imp-contrato-for.cod-cond-pag
           contrato-for.via-transp          = 1 /*1-Rodovi†rio*/
           contrato-for.cod-transp          = 0 
           contrato-for.tp-fornecim         = 1 /*1-reposicao*/
           contrato-for.frete               = 1 /*1-Pago ou 2-A Pagar*/
           contrato-for.natureza            = 1 /*1-Compra*/
           contrato-for.moeda               = tt-imp-contrato-for.mo-codigo
           contrato-for.cod-estabel         = tt-imp-contrato-for.cod-estabel
           contrato-for.contato             = tt-imp-contrato-for.contato
           contrato-for.impr-contrat        = YES 
           contrato-for.cod-tipo-contrat    = 01
           contrato-for.gestor-tecnico      = "Marta"
           contrato-for.variacao-qtd        = 0
           contrato-for.variacao-preco      = 0
           contrato-for.cod-estab-orig      = tt-imp-contrato-for.cod-estabel
           contrato-for.cod-estab-cobr      = tt-imp-contrato-for.cod-estab-cobr
           contrato-for.val-total           = tt-imp-contrato-for.val-total
           contrato-for.cod-estab-entr      = tt-imp-contrato-for.cod-estab-entr
           contrato-for.qtd-total           = tt-imp-contrato-for.qtd-total
           contrato-for.sld-qtd             = tt-imp-contrato-for.sld-qtd
           contrato-for.sld-val             = tt-imp-contrato-for.sld-val
           contrato-for.acum-rec-qtd        = tt-imp-contrato-for.acum-rec-qtd
           contrato-for.acum-rec-val        = tt-imp-contrato-for.acum-rec-val
           contrato-for.sld-qtd-liber       = tt-imp-contrato-for.sld-qtd-liber
           contrato-for.sld-val-liber       = tt-imp-contrato-for.sld-val-liber
           contrato-for.val-fatur-minimo    = tt-imp-contrato-for.val-fatur-minimo
           contrato-for.des-contrat         = tt-imp-contrato-for.des-contrat
           contrato-for.acum-val-pago       = tt-imp-contrato-for.acum-val-pago
           contrato-for.mo-codigo           = tt-imp-contrato-for.mo-codigo
/*            contrato-for.log-libera          = tt-imp-contrato-for.log-libera  */
           contrato-for.sld-qtd-med         = tt-imp-contrato-for.sld-qtd-med
           contrato-for.sal-qtd-liber-med   = tt-imp-contrato-for.sal-qtd-liber-med
           contrato-for.sld-val-med         = tt-imp-contrato-for.sld-val-med
           contrato-for.sld-val-liber-med   = tt-imp-contrato-for.sld-val-liber-med
           contrato-for.cod-projeto         = tt-imp-contrato-for.cod-projeto
           contrato-for.ind-sit-contrat     = tt-imp-contrato-for.ind-sit-contrat
           contrato-for.narrat-contrat      = tt-imp-contrato-for.narrat-contrat
           contrato-for.ind-preco           = 1
           contrato-for.sc-codigo           = tt-imp-contrato-for.sc-codigo
           contrato-for.ct-codigo           = tt-imp-contrato-for.ct-codigo
           .

    IF tt-imp-contrato-for.log-libera = "S" THEN DO:
        ASSIGN contrato-for.log-libera = YES.
    END.
    ELSE DO:
        ASSIGN contrato-for.log-libera = NO.
    END.

    FIND FIRST emitente USE-INDEX codigo
        WHERE emitente.cod-emitente = tt-imp-contrato-for.cod-emitente
        NO-LOCK NO-ERROR.
    IF AVAIL emitente THEN DO:
        ASSIGN contrato-for.cod-transp        = emitente.cod-transp.
        
        FIND FIRST transporte WHERE
                   transporte.cod-transp = emitente.cod-transp NO-LOCK NO-ERROR.
        IF AVAIL transporte THEN
            ASSIGN contrato-for.via-transp = transporte.via-transp.

    END.

    /*matriz-rat-contr*/
    CREATE matriz-rat-contr.
    ASSIGN matriz-rat-contr.nr-contrato         = int(tt-imp-contrato-for.nr-contrato)
           matriz-rat-contr.ct-codigo           = tt-imp-contrato-for.ct-codigo
           matriz-rat-contr.sc-codigo           = tt-imp-contrato-for.sc-codigo
           matriz-rat-contr.perc-rateio         = tt-imp-contrato-for.perc-rateio
           matriz-rat-contr.cod-unid-negoc      = tt-imp-contrato-for.cod-unid-negoc.
           

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-05-geraItemContrato) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-05-geraItemContrato Procedure 
PROCEDURE pi-05-geraItemContrato :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    FIND FIRST ITEM USE-INDEX codigo WHERE ITEM.it-codigo = tt-imp-item-contrato.it-codigo NO-LOCK NO-ERROR.

    /*item-contrat*/
    CREATE item-contrat.
    ASSIGN item-contrat.cod-emitente            = tt-imp-contrato-for.cod-emitente
           item-contrat.nr-contrato             = int(tt-imp-contrato-for.nr-contrato)
           item-contrat.preco-unit              = tt-imp-item-contrato.preco-unit
           item-contrat.qtd-minima              = tt-imp-item-contrato.qtd-minima
           item-contrat.sld-val                 = 0 /**/
           item-contrat.mo-codigo               = tt-imp-item-contrato.mo-codigo
           item-contrat.it-codigo               = tt-imp-item-contrato.it-codigo
           item-contrat.val-total               = tt-imp-item-contrato.val-total
           item-contrat.cod-refer               = (IF AVAIL ITEM THEN ITEM.cod-refer ELSE "")
           item-contrat.codigo-ipi              = (IF tt-imp-item-contrato.ipi-incluso = "S" THEN YES ELSE NO)
           item-contrat.codigo-icm              = tt-imp-item-contrato.codigo-icm
           item-contrat.un                      = tt-imp-item-contrato.un
           item-contrat.contato                 = tt-imp-item-contrato.contato
           item-contrat.num-seq-item            = tt-imp-item-contrato.num-seq-item
           item-contrat.frequencia              = 1 /*Parametro*/
           item-contrat.qtd-total               = tt-imp-item-contrato.qtd-total
           item-contrat.ind-un-contrato         = tt-imp-item-contrato.ind-un-contrato
           item-contrat.sld-qtd                 = tt-imp-item-contrato.sld-qtd
           item-contrat.acum-rec-val            = tt-imp-item-contrato.acum-rec-val
           item-contrat.acum-rec-qtd            = tt-imp-item-contrato.acum-rec-qtd
           item-contrat.log-control-event       = YES /*Parametro*/
           item-contrat.log-obrig-item          = YES /*Parametro*/
           item-contrat.narrat-item             = (IF AVAIL ITEM THEN ITEM.narrativa ELSE "")
           item-contrat.log-ind-multa           = YES /*Parametro*/
           item-contrat.perc-multa-dia          = 0
           item-contrat.perc-multa-limite       = 0
           item-contrat.cod-depos               = (IF AVAIL ITEM THEN ITEM.deposito-pad ELSE "")
           item-contrat.aliquota-icm            = tt-imp-item-contrato.aliquota-icm
           item-contrat.aliquota-ipi            = tt-imp-item-contrato.aliquota-ipi
           item-contrat.tp-despesa              = tt-imp-item-contrato.tp-despesa
           item-contrat.cod-cond-pag            = tt-imp-item-contrato.cod-cond-pag
           item-contrat.preco-fornec            = tt-imp-item-contrato.preco-fornec
           item-contrat.val-frete               = tt-imp-item-contrato.val-frete
           item-contrat.prazo-ent               = tt-imp-item-contrato.prazo-ent
           item-contrat.preco-base              = tt-imp-item-contrato.preco-base
           item-contrat.cod-comprado            = tt-imp-item-contrato.cod-comprador
           item-contrat.perc-desconto           = tt-imp-item-contrato.perc-desconto
           item-contrat.narrat-compra           = tt-imp-item-contrato.narrat-compra
           item-contrat.pre-unit-for            = tt-imp-item-contrato.pre-unit-for
           item-contrat.sld-qtd-receb           = tt-imp-item-contrato.sld-qtd-receb
           item-contrat.sld-val-receb           = tt-imp-item-contrato.sld-val-receb.

    IF tt-imp-item-contrato.log-libera = "S" THEN DO:
        ASSIGN item-contrat.log-libera = YES.
    END.
    ELSE DO:
        ASSIGN item-contrat.log-libera = NO.
    END.

    IF tt-imp-item-contrato.frete = "S" THEN DO:
        ASSIGN item-contrat.frete = YES.
    END.
    ELSE DO:
        ASSIGN item-contrat.frete = NO.
    END.

    /*Matriz-rat-item*/
    CREATE matriz-rat-item.
    ASSIGN matriz-rat-item.nr-contrato          = int(tt-imp-contrato-for.nr-contrato)
           matriz-rat-item.num-seq-item         = tt-imp-item-contrato.num-seq-item
           matriz-rat-item.it-codigo            = tt-imp-item-contrato.it-codigo
           matriz-rat-item.ct-codigo            = tt-imp-item-contrato.ct-codigo
           matriz-rat-item.sc-codigo            = tt-imp-item-contrato.sc-codigo
           matriz-rat-item.perc-rateio          = tt-imp-item-contrato.perc-rateio
           matriz-rat-item.cod-unid-negoc       = tt-imp-item-contrato.cod-unid-negoc.

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

    CREATE ttError.
    ASSIGN //ttError.SessionId   = p-session 
           ttError.referencia  = "TOTVS"
           ttError.codigo      = STRING(tt-erros-geral.cod-erro)
           ttError.descricao   = tt-erros-geral.des-erro.
           //p-erro              = TRUE.
    
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


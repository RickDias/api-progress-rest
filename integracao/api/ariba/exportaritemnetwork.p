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

{include/i-freeac.i}


DEFINE TEMP-TABLE ttItemIntegrar NO-UNDO
    FIELD it-codigo                   AS CHAR
    FIELD tipo-integracao             AS INT
    FIELD categoria-item              AS CHAR
    FIELD cod-categoria               AS CHAR
    FIELD enviado-ariba               AS LOGICAL
    FIELD id-movto                    AS INT.

DEFINE TEMP-TABLE ttStatusEnvioCategoria NO-UNDO
    FIELD cod-categoria               AS CHAR
    FIELD data-inicio                 AS DATE
    FIELD data-fim                    AS DATE
    FIELD ind-situacao                AS INT
    FIELD cod-status                  AS INT
    FIELD c-json                      AS CHAR
    FIELD menssagem                   AS CHAR.


DEFINE TEMP-TABLE ttXMLItem NO-UNDO
    FIELD Timestamp                   AS DATETIME
    FIELD payloadID                   AS DATETIME
    FIELD Operation                   AS CHAR
    FIELD CatalogName                 AS CHAR
    FIELD loadmode                    AS CHAR
    FIELD Description                 AS CHAR    
    FIELD ENABLED                     AS LOGICAL  
    FIELD Notification                AS CHAR
    FIELD Email                       AS CHAR.

DEFINE TEMP-TABLE ttItem NO-UNDO                                                                                             
    FIELD payloadID                   AS DATETIME  /*CODIGO CATALOGO*/
    FIELD Supplier_ID                 AS CHARACTER FORMAT 'x(15)' INITIAL 'AN01427656340-T'  /* Fixo AN01427656340-T         */ 
    FIELD Supplier_Part_ID            AS CHARACTER FORMAT 'x(15)' INITIAL 'AN01427656340-T'  /* Fixo AN01427656340-T         */ 
    FIELD Manufacturer_Part_ID        LIKE item.it-codigo                                    /* item.it-codigo               */  
    FIELD Item_Description            LIKE item.desc-item                                    /* item.desc-item               */        
    FIELD SPSC_Code                   AS CHAR                                                /* deixar branco                */               
    FIELD Unit_Price                  LIKE item.preco-ul-ent                                 /* item.preco-ul-ent            */             
    FIELD Unit_of_Measure             LIKE item.un                                           /* item.un                      */    
    FIELD Lead_Time                   AS INTEGER  INITIAL 15                                 /* ver ???????                  */                            
    FIELD Manufacturer_Name           AS CHARACTER                                           /* deixar branco                */        
    FIELD Supplier_URL                AS CHARACTER                                           /* deixar branco                */    
    FIELD Manufacturer_URL            AS CHARACTER                                           /* deixar branco                */ 
    FIELD Market_Price                AS CHARACTER                                           /* deixar branco                */  
    FIELD Currency                    AS CHARACTER  FORMAT 'x(3)' INITIAL 'BRL'              /* Fixo  'BRL'                  */ 
    FIELD Language                    AS CHARACTER  FORMAT 'x(5)' INITIAL 'pt_BR'            /* Fixo  'pt_BR'                */ 
    FIELD Parametric_Name             AS CHARACTER                                           /* deixar branco                */                 
    FIELD Parametric_Data             AS CHARACTER                                           /* deixar branco                */                 
    FIELD Classification_Codes        AS CHARACTER                                           /* deixar branco                */                 
    FIELD Short_Name                  AS CHARACTER                                           /* deixar branco                */               
    FIELD C_Delete                    AS CHARACTER                                           /* deixar branco                */        
    FIELD IMAGE                       AS CHARACTER                                           /* deixar branco                */        
    FIELD MaterialCode                AS CHARACTER                                           /* codigo item                  */         
    INDEX ii Manufacturer_Part_ID.  

DEFINE DATASET ds-item SERIALIZE-HIDDEN  FOR ttXMLItem, ttItem
    DATA-RELATION dr-item FOR ttXMLItem, ttItem
       RELATION-FIELDS (payloadID, payloadID)  NESTED.

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

DEF VAR h-prog              AS HANDLE     NO-UNDO.

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
DEF VAR i-nr-seq   AS INT      NO-UNDO.
DEF VAR i-cont-it  AS INT      NO-UNDO.

RUN utp/ut-acomp.p PERSISTENT SET h-prog.

RUN pi-inicializar IN h-prog (INPUT "Inicializando Integra‡Æo Item ARIBA!").

FOR EACH sfa-export WHERE
         sfa-export.cd-tipo-integr = 22 AND 
         sfa-export.ind-tipo-trans = 2  AND 
         sfa-export.ind-situacao   = 1  EXCLUSIVE-LOCK,
    FIRST sfa-export-item-ariba OF sfa-export EXCLUSIVE-LOCK:

    ASSIGN i-cont-it = i-cont-it + 1.

    IF i-cont-it = 2000 THEN LEAVE.

    RUN pi-acompanhar IN h-prog("Preparando Itens para Integra‡Æo: " + STRING(sfa-export-item-ariba.it-codigo)).

    FIND FIRST es-it-categoria WHERE
               es-it-categoria.it-codigo = sfa-export-item-ariba.it-codigo EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL es-it-categoria THEN DO:

        CREATE ttItemIntegrar.
        ASSIGN ttItemIntegrar.it-codigo       = sfa-export-item-ariba.it-codigo
               ttItemIntegrar.tipo-integracao = sfa-export-item-ariba.tp-integracao.
        
        FOR FIRST es-categoria-item WHERE
                  es-categoria-item.cod-categoria = es-it-categoria.cod-categoria NO-LOCK:

            RUN pi-acompanhar IN h-prog("Categoria Itens para Integra‡Æo: " + STRING(sfa-export-item-ariba.it-codigo)).

            ASSIGN ttItemIntegrar.categoria-item   = es-categoria-item.desc-categoria
                   ttItemIntegrar.cod-categoria    = es-categoria-item.cod-categoria
                   ttItemIntegrar.enviado-ariba    = es-categoria-item.enviado-ariba.
            
        END.

    END.
    
END.

IF CAN-FIND (FIRST ttItemIntegrar) THEN
    RUN pi-10-envia-item-network (OUTPUT l-situacao,
                                  OUTPUT lc-json).


FOR EACH sfa-export WHERE
         sfa-export.cd-tipo-integr = 22 AND 
         sfa-export.ind-situacao   = 1  EXCLUSIVE-LOCK,
    FIRST sfa-export-item-ariba OF sfa-export EXCLUSIVE-LOCK:

    RUN pi-acompanhar IN h-prog("Gravando Log: " + STRING(sfa-export-item-ariba.it-codigo)).

    
    FOR FIRST ttItemIntegrar WHERE
              ttItemIntegrar.it-codigo = sfa-export-item-ariba.it-codigo NO-LOCK:

        FIND FIRST ttStatusEnvioCategoria WHERE
                   ttStatusEnvioCategoria.cod-categoria = ttItemIntegrar.cod-categoria NO-LOCK NO-ERROR.
        IF AVAIL ttStatusEnvioCategoria THEN DO:

            ASSIGN sfa-export.data-inicio       = ttStatusEnvioCategoria.data-inicio
                   sfa-export.data-fim          = ttStatusEnvioCategoria.data-fim
                   sfa-export.ind-situacao      = ttStatusEnvioCategoria.ind-situacao
                   sfa-export.cod-status        = ttStatusEnvioCategoria.cod-status
                   sfa-export-item-ariba.c-json = ttStatusEnvioCategoria.c-json.

            IF ttStatusEnvioCategoria.cod-status = 1 THEN DO:
    
                FIND LAST sfa-export-log NO-LOCK OF sfa-export NO-ERROR.
                IF AVAIL sfa-export-log THEN
                    ASSIGN i-nr-seq = sfa-export-log.nr-seq + 1.
                ELSE i-nr-seq = 1.
            
                CREATE sfa-export-log.
                ASSIGN sfa-export-log.ind-tipo-trans = sfa-export.ind-tipo-trans
                       sfa-export-log.cd-tipo-integr = sfa-export.cd-tipo-integr
                       sfa-export-log.id-movto       = sfa-export.id-movto      
                       sfa-export-log.data-log       = NOW
                       sfa-export-log.des-log        = "Registro integrado com sucesso: " + ttStatusEnvioCategoria.menssagem
                       sfa-export-log.nr-seq         = i-nr-seq.
    
    
            END.
            ELSE DO:
    
                FIND LAST sfa-export-log NO-LOCK OF sfa-export NO-ERROR.
                IF AVAIL sfa-export-log THEN
                    ASSIGN i-nr-seq = sfa-export-log.nr-seq + 1.
                ELSE i-nr-seq = 1.
            
                CREATE sfa-export-log.
                ASSIGN sfa-export-log.ind-tipo-trans = sfa-export.ind-tipo-trans
                       sfa-export-log.cd-tipo-integr = sfa-export.cd-tipo-integr
                       sfa-export-log.id-movto       = sfa-export.id-movto      
                       sfa-export-log.data-log       = NOW
                       sfa-export-log.des-log        = "Registro integrado com erro: " + ttStatusEnvioCategoria.menssagem
                       sfa-export-log.nr-seq         = i-nr-seq.
    
    
            END.

        END.

    END.
    
END.

RUN pi-finalizar IN h-prog.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-pi-10-envia-item-network) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-10-envia-item-network Procedure 
PROCEDURE pi-10-envia-item-network :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE OUTPUT PARAMETER p-retorno      AS LOG      NO-UNDO.
DEFINE OUTPUT PARAMETER p-json         AS LONGCHAR NO-UNDO.

DEF VAR c-token              AS CHAR                  NO-UNDO.
DEF VAR pArquivoEntrada      AS LONGCHAR              NO-UNDO.
DEF VAR lc-erro              AS LONGCHAR              NO-UNDO.
DEF VAR l-enviado            AS LOG INITIAL NO        NO-UNDO.
DEF VAR c-narrativa          AS CHAR FORMAT "x(2000)" NO-UNDO.
DEF VAR c-descricao          AS CHAR                  NO-UNDO.
DEF VAR c-descricao-short    AS CHAR FORMAT "x(49)"   NO-UNDO.

IF NOT VALID-HANDLE(h-esapi002) THEN
    RUN esp/esint002.p PERSISTENT SET h-esapi002.

FOR EACH ttItemIntegrar NO-LOCK
    BREAK BY ttItemIntegrar.categoria:

    //RUN pi-acompanhar IN h-prog("Reunindo Itens por Categoria: " + STRING(ttItemIntegrar.it-codigo)).
    
    IF FIRST-OF (ttItemIntegrar.categoria)  THEN DO:

        CREATE ttXMLItem.
        ASSIGN ttXMLItem.Timestamp    = NOW
               ttXMLItem.payloadID    = NOW
               ttXMLItem.Operation    = "new"   
               ttXMLItem.loadmode     = "F"   
               ttXMLItem.CatalogName  = ttItemIntegrar.cod-categoria + "-" + ttItemIntegrar.categoria
               ttXMLItem.Description  = ttItemIntegrar.cod-categoria + "-" + ttItemIntegrar.categoria
               ttXMLItem.ENABLED      = NO 
               ttXMLItem.Notification = "INTEGRA€ÇO ITEM NETWORK TESTE"   
               ttXMLItem.Email        = "CLEBER.FERRNEIRA@THEMCONSULTORIA.COM.BR".   

        IF ttItemIntegrar.enviado-ariba THEN
            ASSIGN ttXMLItem.Operation    = "update"   
                   ttXMLItem.loadmode     = "I"
                   ttXMLItem.ENABLED      = YES.


    END.

    FIND FIRST ttXMLItem NO-LOCK NO-ERROR.

    FIND FIRST ITEM WHERE
               ITEM.it-codigo = ttItemIntegrar.it-codigo NO-LOCK NO-ERROR.

    IF AVAIL ITEM THEN DO:

        ASSIGN c-narrativa       = REPLACE(item.narrativa,",","-")
               c-narrativa       = REPLACE(c-narrativa,CHR(10), "<br>")
               c-descricao       = REPLACE(ITEM.desc-item,",",".")
               //c-descricao       = fn-free-accent(c-descricao)
               c-descricao-short = SUBSTRING(c-descricao,1,49).

    
        CREATE ttItem.
        ASSIGN ttItem.payloadID                      =  ttXMLItem.payloadID
               ttItem.Supplier_ID                    =  "AN01427656340-T"
               ttItem.Supplier_Part_ID               =  REPLACE(item.it-codigo,",",".")
               ttItem.Manufacturer_Part_ID           =  REPLACE(item.it-codigo,",",".")
               ttItem.Item_Description               =  IF c-narrativa <> "" THEN c-narrativa ELSE c-descricao
               ttItem.SPSC_Code                      =  ""
               ttItem.Unit_Price                     =  0.01 /*IF item.preco-ul-ent = 0 THEN 1 ELSE item.preco-ul-ent*/
               ttItem.Unit_of_Measure                =  item.un                            
               ttItem.Lead_Time                      =  15
               ttItem.Manufacturer_Name              =  ""
               ttItem.Supplier_URL                   =  ""
               ttItem.Manufacturer_URL               =  ""
               ttItem.Market_Price                   =  ""
               ttItem.Currency                       =  "BRL"
               ttItem.Language                       =  "pt_BR"
               ttItem.Classification_Codes           =  CHR(123) + "custom=" + ttItemIntegrar.cod-categoria + CHR(125)
               ttItem.Short_Name                     =  c-descricao-short
               ttItem.C_Delete                       =  "false"
               ttItem.IMAGE                          =  "itens.jpg"
               ttItem.MaterialCode                   =  REPLACE(item.it-codigo,",",".").

        IF item.ind-serv-mat = 2 THEN
            ASSIGN ttItem.Parametric_Name =  "system:catalogitem"
                   ttItem.Parametric_Data =  CHR(123) + CHR(125).
        ELSE
            ASSIGN ttItem.Parametric_Name =  "system:service"
                   ttItem.Parametric_Data =  CHR(123) + "ariba_item_type=service" + CHR(125).

    END.

    IF LAST-OF (ttItemIntegrar.categoria)  THEN DO:

        //RUN pi-acompanhar IN h-prog("Enviado Integra‡Æo dos Itens por Categoria: " + STRING(ttItemIntegrar.it-codigo)).

        lRetOK = DATASET ds-Item:WRITE-JSON("LONGCHAR", pArquivoEntrada, TRUE, "UTF-8") NO-ERROR.

        ASSIGN p-json = pArquivoEntrada.

        OUTPUT TO c:/temp/jsonenvioitem.json.

            EXPORT pArquivoEntrada.
        
        OUTPUT CLOSE.


        RUN piGeraTokenApigee IN h-esapi002 (OUTPUT c-token).
        
        CREATE "MSXML2.XMLHTTP" client.
        
        FIND FIRST es-api-param NO-LOCK WHERE es-api-param.cd-tipo-integr = 22 NO-ERROR.
        IF AVAIL es-api-param THEN DO:

            CREATE ttStatusEnvioCategoria.
            ASSIGN ttStatusEnvioCategoria.cod-categoria = ttItemIntegrar.cod-categoria
                   ttStatusEnvioCategoria.data-inicio   = NOW
                   ttStatusEnvioCategoria.c-json        = SUBSTRING(pArquivoEntrada,1,2000).

            client:OPEN("post", es-api-param.host-integr + ":" + STRING(es-api-param.porta-integr) + path-integr, FALSE).
            client:SetRequestHeader ("Content-Type", "application/json").
            client:SetRequestHeader ("Authorization", "Bearer " + c-token).
            client:Send(pArquivoEntrada).

            ASSIGN ttStatusEnvioCategoria.data-fim     = NOW
                   ttStatusEnvioCategoria.ind-situacao = 2 
                   ttStatusEnvioCategoria.menssagem    = client:ResponseText.

            
            MESSAGE 'ResponseText  '    client:ResponseText   SKIP
                    'Responsexml   '    client:Responsexml    SKIP
                    'responseStream'    client:responseStream SKIP
                    'STATUS        '    client:STATUS         SKIP
                    VIEW-AS ALERT-BOX TITLE "retorno".

            IF client:STATUS = "200" THEN DO:

                FIND FIRST es-categoria WHERE 
                           es-categoria.cod-categoria = ttItemIntegrar.cod-categoria EXCLUSIVE-LOCK NO-ERROR.
                IF AVAIL es-categoria THEN
                    ASSIGN es-categoria.enviado-ariba = YES.

                ASSIGN ttStatusEnvioCategoria.cod-status = 1
                       p-retorno                         = YES.

            END.
            ELSE
                ASSIGN ttStatusEnvioCategoria.cod-status = 2.

            
            
        
        END.

        EMPTY TEMP-TABLE ttXMLItem.
        EMPTY TEMP-TABLE ttItem.

    END.
                                                                                           
END.

IF VALID-HANDLE(h-esapi002) THEN DO:
    DELETE PROCEDURE h-esapi002.
    ASSIGN h-esapi002 = ?.
END.

RELEASE OBJECT client.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


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

//DEFINE INPUT  PARAMETER p-rowid   AS ROWID  NO-UNDO.

/*ENVIO PEDIDO DE COMPRA*/
DEF TEMP-TABLE tt-po-ariba NO-UNDO SERIALIZE-NAME "Pedido_Compra"
    FIELD payloadID                           AS DATETIME
    FIELD timestamp                           AS DATETIME
    field from-domain-network                 as CHAR
    field to-domain-network                   as CHAR
    
    field VendorID                            as CHAR

    field soldTo-Name                         as char
    field soldTo-Street                       as char
    field soldTo-Street2                      as char
    field soldTo-City                         as char
    field soldTo-State                        as char
    field soldTo-PostalCode                   as char
    field soldTo-Country                      as char
    field soldTo-Email                        as char
    field soldTo-CountryCode                  as char
    field soldTo-AreaorCityCode               as char
    field soldTo-Number                       as char

    field ShipTo-addressID                    as CHAR
    field ShipTo-Name                         as CHAR
    field ShipTo-DeliverTo                    as CHAR
    field ShipTo-Street                       as CHAR
    field ShipTo-Street2                      as CHAR
    field ShipTo-City                         as CHAR
    field ShipTo-State                        as CHAR
    field ShipTo-PostalCode                   as CHAR
    field ShipTo-Country                      as CHAR
    field ShipTo-Email                        as CHAR
    field ShipTo-CountryCode                  as CHAR  
    field ShipTo-AreaorCityCode               as CHAR
    field ShipTo-Number                       as CHAR
    field ShipTo-CNPJ                         as CHAR
    
    field BillTo-addressID                    as char
    field BillTo-Name                         as char
    field BillTo-DeliverTo                    as char
    field BillTo-Street                       as char
    field BillTo-Street2                      as char
    field BillTo-City                         as char
    field BillTo-State                        as char
    field BillTo-PostalCode                   as char
    field BillTo-Country                      as char
    field BillTo-Email                        as char
    field BillTo-CountryCode                  as char
    field BillTo-AreaorCityCode               as char
    field BillTo-Number                       as char
    field BillTo-CNPJ                         as char

    field Supplier-Name                       as CHAR
    field Supplier-Street                     as CHAR
    field Supplier-Street2                    as CHAR
    field Supplier-City                       as CHAR
    field Supplier-State                      as CHAR
    field Supplier-PostalCode                 as CHAR
    field Supplier-Country                    as CHAR
    field Supplier-Email                      as CHAR
    field Supplier-CountryCode                as CHAR
    field Supplier-AreaorCityCode             as CHAR
    field Supplier-Number                     as CHAR
    field Supplier-CNPJ                       as CHAR
    
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
    field PurchasingUnit                      as char
    field PUName                              as char
    field incoTerm                            as char
    field incoTermLocation                    as char
    field soldTo-addressID                    as char
    field soldTo-CNPJ                         as char
    .

DEF TEMP-TABLE tt-it-po-ariba NO-UNDO SERIALIZE-NAME "Ordem_Compra"
    field orderID                     AS CHAR
    FIELD isAdHoc                     AS CHAR
    field Quantity                    as char
    field LineNumber                  as char
    field SupplierPartID              as char
    field BuyerPartID                 as char
    field Money                       as char
    field Currency                    as char
    field C-Description               as char
    field UnitOfMeasure               as char
    field Classification              as char
    field LeadTime                    as char
    field ReqLineNo                   as char
    field Requester                   as char
    field PRNo                        as char
    
    field ContractID                          as char
    
    FIELD Tax-currency                        as char
    field Tax-Money                           as char
    field Tax-ICMS-currency                   as char
    field Tax-ICMS-perc                       as char
    field Tax-ICMS-money                      as char
    field Tax-IPI-currency                    as char
    field Tax-IPI-perc                        as char
    field Tax-IPI-money                       as char
    
    field narrativa-item                      as CHAR.

DEFINE DATASET ds-po-ariba SERIALIZE-HIDDEN  FOR tt-po-ariba, tt-it-po-ariba
    DATA-RELATION dr-po-ariba FOR tt-po-ariba, tt-it-po-ariba
    RELATION-FIELDS (orderID, orderID)  NESTED.

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
         sfa-export.cd-tipo-integr = 20 AND 
        (sfa-export.cod-status     = 0  OR
         sfa-export.cod-status     = 2) EXCLUSIVE-LOCK:

    FOR FIRST sfa-export-ped-ariba WHERE
              sfa-export-ped-ariba.cd-tipo-integr = sfa-export.cd-tipo-integr AND
              sfa-export-ped-ariba.id-movto       = sfa-export.id-movto       EXCLUSIVE-LOCK:

        ASSIGN sfa-export.data-inicio  = NOW.

        RUN pi-10-gera-pedido-network (INPUT  sfa-export-ped-ariba.num-pedido,
                                       OUTPUT l-situacao,
                                       OUTPUT lc-json,
                                       OUTPUT c-erro).

        ASSIGN sfa-export.data-fim     = NOW
               sfa-export.ind-situacao = 2 .

    END.

    IF l-situacao THEN DO:

        ASSIGN sfa-export.cod-status = 1
               sfa-export-ped-ariba.c-json = lc-json.

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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-pi-10-gera-pedido-network) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-10-gera-pedido-network Procedure 
PROCEDURE pi-10-gera-pedido-network :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER p-num-pedido   AS INT      NO-UNDO.
DEFINE OUTPUT PARAMETER p-retorno      AS LOG      NO-UNDO.
DEFINE OUTPUT PARAMETER p-json         AS LONGCHAR NO-UNDO.
DEFINE OUTPUT PARAMETER p-menssagem    AS CHAR     NO-UNDO.

EMPTY TEMP-TABLE tt-po-ariba.
EMPTY TEMP-TABLE tt-it-po-ariba.

DEF VAR d-total-pedido AS DECIMAL NO-UNDO.
DEF VAR c-token        AS CHAR    NO-UNDO.

DEF VAR pArquivoEntrada AS LONGCHAR NO-UNDO.
DEF VAR lc-erro AS LONGCHAR NO-UNDO.

FIND FIRST pedido-compr WHERE
           pedido-compr.num-pedido = p-num-pedido NO-LOCK NO-ERROR.
IF AVAIL pedido-compr THEN DO:

    RUN pi-11-calc-total-pedido (INPUT  pedido-compr.num-pedido,
                                 OUTPUT d-total-pedido).

    CREATE tt-po-ariba.
    ASSIGN tt-po-ariba.payloadID                 = NOW
           tt-po-ariba.timestamp                 = NOW
           tt-po-ariba.from-domain-network       = "AN01423369292-T"    

           tt-po-ariba.vendorID                  = STRING(pedido-compr.cod-emitente)

           tt-po-ariba.sender-domain             = "sysadmin@ariba.com"    
           tt-po-ariba.sender-useragent          = "Buyer 14s2"    

           tt-po-ariba.Type                      = "new"
           tt-po-ariba.orderType                 = "regular"
           tt-po-ariba.orderVersion              = "1"
           tt-po-ariba.orderID                   = STRING(pedido-compr.num-pedido)
           tt-po-ariba.orderDate                 = NOW

           tt-po-ariba.Total-Money               = STRING(d-total-pedido)
           tt-po-ariba.Total-currency            = "BRL"    
           tt-po-ariba.ShipTo-isoCountryCode     = "BR".
           //tt-po-ariba.PurchasingUnit            = "1000"    
           //tt-po-ariba.PUName                    = "Dummy_Purchase_Unit"
           //tt-po-ariba.incoTermLocation          = "".
           
    IF pedido-compr.frete = 1 THEN
        ASSIGN tt-po-ariba.incoTerm = "CIF".
    ELSE
        ASSIGN tt-po-ariba.incoTerm = "FOB".


    FIND FIRST emitente WHERE
               emitente.cod-emitente = pedido-compr.cod-emitente NO-LOCK NO-ERROR.
    IF AVAIL emitente THEN DO:

        ASSIGN tt-po-ariba.SoldTo-Name               = emitente.nome-emit
               //tt-po-ariba.SoldTo-DeliverTo          = emitente.nome-emit
               tt-po-ariba.SoldTo-Street             = emitente.endereco
               tt-po-ariba.SoldTo-Street2            = emitente.endereco2
               tt-po-ariba.SoldTo-City               = emitente.cidade
               tt-po-ariba.SoldTo-State              = emitente.estado
               tt-po-ariba.SoldTo-PostalCode         = emitente.cep
               tt-po-ariba.SoldTo-Country            = emitente.pais.

        ASSIGN tt-po-ariba.Supplier-Name         = emitente.nome-emit
               tt-po-ariba.Supplier-Street       = emitente.endereco  
               tt-po-ariba.Supplier-Street2      = emitente.endereco2 
               tt-po-ariba.Supplier-City         = emitente.cidade    
               tt-po-ariba.Supplier-State        = emitente.estado    
               tt-po-ariba.Supplier-PostalCode   = emitente.cep       
               tt-po-ariba.Supplier-Country      = emitente.pais
               tt-po-ariba.Supplier-CNPJ         = emitente.cgc.

        FIND FIRST cont-emit OF emitente NO-LOCK NO-ERROR.
        IF AVAIL cont-emit THEN
            ASSIGN tt-po-ariba.SoldTo-Number = cont-emit.telefone 
                   tt-po-ariba.SoldTo-Email  = cont-emit.e-mail
                   tt-po-ariba.Supplier-Email  = cont-emit.e-mail
                   tt-po-ariba.Supplier-Number = cont-emit.telefone.
                   //tt-po-ariba.ShipTo-CountryCode        = ""    
                   //tt-po-ariba.ShipTo-AreaorCityCode     = ""    
        
    END.


    FIND FIRST estabelec WHERE
               estabelec.cod-estabel = pedido-compr.end-entrega NO-LOCK NO-ERROR.
    IF AVAIL estabelec THEN DO:

        ASSIGN tt-po-ariba.ShipTo-addressID          = estabelec.cod-estabel
               tt-po-ariba.ShipTo-Name               = estabelec.nome
               tt-po-ariba.ShipTo-DeliverTo          = estabelec.nome
               tt-po-ariba.ShipTo-Street             = estabelec.endereco
               tt-po-ariba.ShipTo-City               = estabelec.cidade
               tt-po-ariba.ShipTo-State              = estabelec.estado
               tt-po-ariba.ShipTo-PostalCode         = STRING(estabelec.cep)
               tt-po-ariba.ShipTo-Country            = estabelec.pais
               tt-po-ariba.ShipTo-CNPJ               = estabelec.cgc.
               //tt-po-ariba.ShipTo-Street2            = estabelec.complemento
               //tt-po-ariba.ShipTo-Email              = estabelec.e-mail
               //tt-po-ariba.ShipTo-Number             = estabelec.telefone.
               //tt-po-ariba.ShipTo-CountryCode        = ""    
               //tt-po-ariba.ShipTo-AreaorCityCode     = ""    
        
    END.

    FIND FIRST estabelec WHERE
               estabelec.cod-estabel = pedido-compr.end-cobranca NO-LOCK NO-ERROR.
    IF AVAIL estabelec THEN DO:

        ASSIGN tt-po-ariba.BillTo-addressID          = estabelec.cod-estabel
               tt-po-ariba.BillTo-Name               = estabelec.nome
               tt-po-ariba.BillTo-DeliverTo          = estabelec.nome
               tt-po-ariba.BillTo-Street             = estabelec.endereco
               tt-po-ariba.BillTo-City               = estabelec.cidade
               tt-po-ariba.BillTo-State              = estabelec.estado
               tt-po-ariba.BillTo-PostalCode         = STRING(estabelec.cep)
               tt-po-ariba.BillTo-Country            = estabelec.pais
               tt-po-ariba.BillTo-CNPJ               = estabelec.cgc.
               //tt-po-ariba.BillTo-Street2            = estabelec.complemento
               //tt-po-ariba.BillTo-Email              = estabelec.e-mail
               //tt-po-ariba.BillTo-Number             = estabelec.telefone.
               //tt-po-ariba.BillTo-CountryCode        = ""    
               //tt-po-ariba.BillTo-AreaorCityCode     = ""    
        
    END.

    IF sfa-export-ped-ariba.tp-integracao = 2 THEN
        ASSIGN tt-po-ariba.Type = "update"
               tt-po-ariba.orderVersion = STRING(pedido-compr.int-2).

    FOR EACH ordem-compra WHERE 
             ordem-compra.num-pedido = pedido-compr.num-pedido NO-LOCK:

        FIND FIRST ITEM WHERE
                   ITEM.it-codigo = ordem-compra.it-codigo NO-LOCK NO-ERROR.

        CREATE tt-it-po-ariba.
        ASSIGN tt-it-po-ariba.orderID           = STRING(pedido-compr.num-pedido)
               tt-it-po-ariba.isAdHoc           = "no" 
               tt-it-po-ariba.Quantity          = STRING(ordem-compra.qt-solic)
               tt-it-po-ariba.LineNumber        = STRING(ordem-compra.sequencia)
               tt-it-po-ariba.SupplierPartID    = ordem-compra.it-codigo         /*CODIGO FORNECEDOR*/
               tt-it-po-ariba.BuyerPartID       = ordem-compra.it-codigo         /*CODIGO TOTVS*/
               tt-it-po-ariba.Money             = STRING(ordem-compra.preco-fornec)
               tt-it-po-ariba.Currency          = "BRL" 
               tt-it-po-ariba.C-Description     = ITEM.desc-item
               tt-it-po-ariba.UnitOfMeasure     = ITEM.un
               tt-it-po-ariba.LeadTime          = "15" 
               tt-it-po-ariba.ReqLineNo         = STRING(ordem-compra.sequencia)
               tt-it-po-ariba.Requester         = ordem-compra.usuario
               tt-it-po-ariba.PRNo              = STRING(ordem-compra.num-pedido)

               tt-it-po-ariba.narrativa-item    = ordem-compra.narrativa

               tt-it-po-ariba.ContractID        = STRING(ordem-compra.nr-contrato)

               tt-it-po-ariba.Tax-currency      = ""   /*MOEDA IMPOSTOS*/
               tt-it-po-ariba.Tax-Money         = ""   /*TOTAL VALOR IMPOSTOS*/
               tt-it-po-ariba.Tax-ICMS-currency = ""   /*MOEDA ICMS*/
               tt-it-po-ariba.Tax-ICMS-perc     = ""   /*PERC ICMS*/
               tt-it-po-ariba.Tax-ICMS-money    = ""   /*VALOR ICMS*/
               tt-it-po-ariba.Tax-IPI-currency  = ""   /*MOEDA IPI*/ 
               tt-it-po-ariba.Tax-IPI-perc      = ""   /*PERC IPI*/  
               tt-it-po-ariba.Tax-IPI-money     = ""   /*VALOR IPI*/ 
               .


        FIND FIRST es-it-categoria WHERE
                   es-it-categoria.it-codigo = ordem-compra.it-codigo NO-LOCK NO-ERROR.
        IF AVAIL es-it-categoria THEN
            ASSIGN tt-it-po-ariba.Classification = es-it-categoria.cod-categoria.

    
    END.

END.

IF NOT VALID-HANDLE(h-esapi002) THEN
    RUN esp/esint002.p PERSISTENT SET h-esapi002.

lRetOK = DATASET ds-po-ariba:WRITE-JSON("LONGCHAR", pArquivoEntrada, TRUE, "UTF-8") NO-ERROR.

OUTPUT TO c:/temp/jsonenviapedido.json.

    EXPORT pArquivoEntrada.

OUTPUT CLOSE.


ASSIGN p-json = pArquivoEntrada.

RUN piGeraTokenApigee IN h-esapi002 (OUTPUT c-token).

CREATE "MSXML2.XMLHTTP" client.

FIND FIRST es-api-param NO-LOCK WHERE es-api-param.cd-tipo-integr = 20 /*---- Integra»’o Receb Ariba  ------*/ NO-ERROR.
IF AVAIL es-api-param THEN DO:

    client:OPEN("post", es-api-param.host-integr + ":" + STRING(es-api-param.porta-integr) + path-integr, FALSE).
    //client:OPEN("post", "http://papige.camil.com.br:15212/ariba/purchase-orders", FALSE).
    client:SetRequestHeader ("Content-Type", "application/json").
    client:SetRequestHeader ("Authorization", "Bearer " + c-token).
    client:Send(pArquivoEntrada).

    MESSAGE 'ResponseText  '    client:ResponseText   SKIP
            'Responsexml   '    client:Responsexml    SKIP
            'responseStream'    client:responseStream SKIP
            'STATUS        '    client:STATUS         SKIP
            VIEW-AS ALERT-BOX TITLE "retorno".

    IF client:STATUS = "201" THEN
        ASSIGN p-retorno   = YES.
    ELSE
        ASSIGN p-menssagem = client:ResponseText.
               

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

&IF DEFINED(EXCLUDE-pi-11-calc-total-pedido) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-11-calc-total-pedido Procedure 
PROCEDURE pi-11-calc-total-pedido :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT  PARAMETER p-num-pedido   AS INT  NO-UNDO.
DEFINE OUTPUT PARAMETER p-total-pedido AS DECIMAL NO-UNDO.

FOR EACH ordem-compra WHERE
         ordem-compra.num-pedido = p-num-pedido NO-LOCK:

    ASSIGN p-total-pedido = p-total-pedido + ordem-compra.preco-fornec.

END.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


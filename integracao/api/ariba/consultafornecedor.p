&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : importarpedcompra.p
    Purpose     : Integra»ao - ARIBA x Pedido de Compra

    Syntax      :

    Description :

    Author(s)   : TOTVS
    Created     : 04/2019
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/
USING Progress.Lang.*. 
USING Progress.Json.ObjectModel.*. 

/* ***************************  Definitions  ************************** */

// O campo ind-tipo-movto trata qual movimento esta realizando
// 1 - Inclusao / 2 - Alteracao / 3 - Exclusao

//{utp/ut-api.i}
//{utp/ut-api-utils.i}
//{cdp/cdcfgmat.i}

DEF TEMP-TABLE consulta-fornecedor NO-UNDO SERIALIZE-NAME "Consulta_Fornecedor"
    FIELD CreationDateTime                    AS CHAR
    FIELD InboundServiceName                  AS CHAR.


DEF TEMP-TABLE cadastro-fornecedor NO-UNDO SERIALIZE-NAME "Cadastro_Fornecedor"
    FIELD Corporate-Name                      AS CHAR
    FIELD Trading-name                        AS CHAR
    FIELD Number                              AS CHAR
    FIELD CNPJ                                AS CHAR
    FIELD CPF                                 AS CHAR
    FIELD PIS-Number                          AS CHAR
    FIELD NIS-Number                          AS CHAR
    FIELD IE                                  AS CHAR
    FIELD State                               AS CHAR
    FIELD Street                              AS CHAR
    FIELD Complement                          AS CHAR
    FIELD District                            AS CHAR
    FIELD Zip-Code                            AS CHAR
    FIELD Country                             AS CHAR
    FIELD Pais                                AS CHAR
    FIELD CNAE-principal                      AS CHAR
    FIELD E-mail                              AS CHAR
    FIELD Simples-Nacional                    AS CHAR
    FIELD Municipality                        AS CHAR
    FIELD Nome-Responsavel                    AS CHAR
    FIELD Date-Birth                          AS CHAR
    FIELD Codigo-Pais                         AS CHAR
    FIELD Codigo-area                         AS CHAR
    FIELD Numero-Telefone                     AS CHAR
    FIELD Banco                               AS CHAR
    FIELD Agencia                             AS CHAR
    FIELD Dig-Agencia                         AS CHAR
    FIELD Conta-corrente                      AS CHAR
    FIELD Dig-conta-corrente                  AS CHAR

    FIELD DeletedIndicator                    AS CHAR
    FIELD BlockedIndicator                    AS CHAR
    FIELD BuildingID                          AS CHAR
    FIELD POBoxDeviatingCityName              AS CHAR.

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
DEFINE VARIABLE h-esapi002           AS HANDLE     NO-UNDO. 
DEFINE VARIABLE c-url                AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-token              AS CHARACTER  NO-UNDO.
DEFINE VARIABLE v-longchar           AS LONGCHAR   NO-UNDO. 
DEFINE VARIABLE c-texto              AS CHARACTER  NO-UNDO.
DEFINE VARIABLE client               AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE l-retorno-fornecedor AS LOGICAL NO-UNDO.

DEF STREAM str-rp.

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
RUN pi-00-consulta-fornecedor (OUTPUT l-retorno-fornecedor).

/*
IF l-retorno-fornecedor THEN
    RUN pi-10-grava-fornecedor.

*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-pi-00-consulta-fornecedor) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-00-consulta-fornecedor Procedure 
PROCEDURE pi-00-consulta-fornecedor :
DEFINE OUTPUT PARAMETER p-retorno AS LOG NO-UNDO.
    
    DEFINE VARIABLE pArquivoEnvio   AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE pArquivoRetorno AS LONGCHAR NO-UNDO.
    
    CREATE consulta-fornecedor.
    ASSIGN CreationDateTime    = STRING(YEAR(TODAY)) + "-" + STRING(MONTH(TODAY),"99") + "-" + STRING(DAY(TODAY),"99") + "T00:00:00Z"
           InboundServiceName  = "BusinessPartnerSUITEBulkReplicateRequest_In".
    
    TEMP-TABLE consulta-fornecedor:WRITE-JSON("LONGCHAR", pArquivoEnvio, TRUE, "UTF-8") NO-ERROR.
    
    OUTPUT TO c:/temp/jsonconsultafornecedor.json.
    
    EXPORT pArquivoEnvio.
    
    OUTPUT CLOSE.
    
    
    IF NOT VALID-HANDLE(h-esapi002) THEN
        RUN esp/esint002.p PERSISTENT SET h-esapi002.
    
    RUN piGeraTokenApigee IN h-esapi002 (OUTPUT c-token).
    
    CREATE "MSXML2.XMLHTTP" client.
    
    FIND FIRST es-api-param NO-LOCK WHERE es-api-param.cd-tipo-integr = 25 /*---- Integra»’o Fornec Ariba  ------*/ NO-ERROR.
    IF AVAIL es-api-param THEN DO:
    
        client:OPEN("post", es-api-param.host-integr + ":" + STRING(es-api-param.porta-integr) + path-integr, FALSE).
        client:SetRequestHeader ("Content-Type", "application/json").
        client:SetRequestHeader ("Authorization", "Bearer " + c-token).
        client:Send(pArquivoEnvio).
    
        //pArquivoRetorno = replace(client:responseText,'encoding="utf-8"','encoding="ISO-8859-1"').
        pArquivoRetorno = client:responseText.

        //ASSIGN pArquivoRetorno = client:ResponseText. //CODEPAGE-CONVERT(client:ResponseText, SESSION:CHARSET, "utf-8").
    
        /*MESSAGE STRING(client:ResponseText)
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
            */
    /*
        MESSAGE 'ResponseText  '    client:ResponseText    SKIP
                'Responsexml   '    client:Responsexml:xml SKIP
                'responseStream'    client:responseStream  SKIP
                'STATUS        '    client:STATUS          SKIP
                VIEW-AS ALERT-BOX TITLE "retorno".
      */  
    
    END.
    
    //ASSIGN pArquivoRetorno = REPLACE(pArquivoRetorno,"null",'""').
    
    OUTPUT TO c:/temp/jsonretornofornecedor.json.
    
    EXPORT pArquivoRetorno.
    
    OUTPUT CLOSE.
    
    p-retorno = TEMP-TABLE cadastro-fornecedor:READ-JSON("LONGCHAR", pArquivoRetorno, "empty") .
    //p-retorno = TEMP-TABLE cadastro-fornecedor:READ-JSON("file", "c:/temp/jsonretornofornecedor.json", "empty") .
    
    IF VALID-HANDLE(h-esapi002) THEN DO:
        DELETE PROCEDURE h-esapi002.
        ASSIGN h-esapi002 = ?.
    END.
    
    RELEASE OBJECT client.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-10-grava-fornecedor) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-10-grava-fornecedor Procedure 
PROCEDURE pi-10-grava-fornecedor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   FOR EACH cadastro-fornecedor NO-LOCK:

       MESSAGE 
           cadastro-fornecedor.CNPJ SKIP              
           cadastro-fornecedor.CPF                
           VIEW-AS ALERT-BOX INFO BUTTONS OK.

       FIND FIRST es-fornecedor-ariba WHERE
                 (es-fornecedor-ariba.CNPJ = cadastro-fornecedor.CNPJ  OR 
                  es-fornecedor-ariba.CPF  = cadastro-fornecedor.CPF)  AND
                  es-fornecedor-ariba.IE   = cadastro-fornecedor.IE    NO-LOCK NO-ERROR.
       IF NOT AVAIL es-fornecedor-ariba THEN DO:
    
           CREATE es-fornecedor-ariba.
           ASSIGN es-fornecedor-ariba.Corporate-Name     = cadastro-fornecedor.Corporate-Name    
                  es-fornecedor-ariba.Trading-name       = cadastro-fornecedor.Trading-name      
                  es-fornecedor-ariba.Number             = cadastro-fornecedor.Number            
                  es-fornecedor-ariba.CNPJ               = cadastro-fornecedor.CNPJ              
                  es-fornecedor-ariba.CPF                = cadastro-fornecedor.CPF               
                  es-fornecedor-ariba.PIS-Number         = cadastro-fornecedor.PIS-Number
                  es-fornecedor-ariba.NIS-Number         = cadastro-fornecedor.NIS-Number
                  es-fornecedor-ariba.IE                 = cadastro-fornecedor.IE                
                  es-fornecedor-ariba.State              = cadastro-fornecedor.State             
                  es-fornecedor-ariba.Street             = cadastro-fornecedor.Street            
                  es-fornecedor-ariba.Complement         = cadastro-fornecedor.Complement        
                  es-fornecedor-ariba.District           = cadastro-fornecedor.District          
                  es-fornecedor-ariba.Zip-Code           = cadastro-fornecedor.Zip-Code          
                  es-fornecedor-ariba.Country            = cadastro-fornecedor.Country           
                  es-fornecedor-ariba.Pais               = cadastro-fornecedor.Pais              
                  es-fornecedor-ariba.CNAE-principal     = cadastro-fornecedor.CNAE-principal
                  es-fornecedor-ariba.E-mail             = cadastro-fornecedor.E-mail            
                  es-fornecedor-ariba.Municipality       = cadastro-fornecedor.Municipality      
                  es-fornecedor-ariba.Nome-Responsavel   = cadastro-fornecedor.Nome-Responsavel  
                  es-fornecedor-ariba.Date-Birth         = DATE(cadastro-fornecedor.Date-Birth)
                  es-fornecedor-ariba.Codigo-Pais        = cadastro-fornecedor.Codigo-Pais       
                  es-fornecedor-ariba.Codigo-area        = cadastro-fornecedor.Codigo-area
                  es-fornecedor-ariba.Numero-Telefone    = cadastro-fornecedor.Numero-Telefone   
                  es-fornecedor-ariba.Banco              = cadastro-fornecedor.Banco
                  es-fornecedor-ariba.Agencia            = cadastro-fornecedor.Agencia
                  es-fornecedor-ariba.Dig-Agencia        = cadastro-fornecedor.Dig-Agencia
                  es-fornecedor-ariba.Conta-corrente     = cadastro-fornecedor.Conta-corrente
                  es-fornecedor-ariba.Dig-conta-corrente = cadastro-fornecedor.Dig-conta-corrente.

           IF cadastro-fornecedor.Simples-Nacional = "yes" THEN
               es-fornecedor-ariba.Simples-Nacional = YES.
           ELSE
               es-fornecedor-ariba.Simples-Nacional = NO.
               
       END.

   END.
    
       /*IF es-fornecedor-ariba.cnpj > ""
       THEN DO:
          FIND FIRST es-api-param NO-LOCK 
               WHERE es-api-param.ind-tipo-trans = 2  /*---- Saida ----*/
                 AND es-api-param.cd-tipo-integr = 13 /*---- Integra»’o B2E PJ ------*/ 
               NO-ERROR.
          IF AVAIL es-api-param 
          THEN DO:
             IF NOT CAN-FIND(FIRST sfa-export 
                             WHERE sfa-export.chave = es-fornecedor-ariba.cnpj
                               AND sfa-export.ind-situacao < 2) 
             THEN DO:
                CREATE api-export-b2e-pj.
                ASSIGN api-export-b2e-pj.cd-tipo-integr = es-api-param.cd-tipo-integr
                       api-export-b2e-pj.id-movto       = NEXT-VALUE(seq-export)
                       api-export-b2e-pj.cgc            = es-fornecedor-ariba.cnpj
                       api-export-b2e-pj.data-movto     = NOW
                       api-export-b2e-pj.c-json         = ?.
          
                CREATE sfa-export.
                ASSIGN sfa-export.ind-tipo-trans = es-api-param.ind-tipo-trans
                       sfa-export.id-movto       = es-api-param.cd-tipo-integr
                       sfa-export.cd-tipo-integr = api-export-b2e-pj.cd-tipo-integr
                       sfa-export.chave          = api-export-b2e-pj.cgc
                       sfa-export.cod-status     = 0      /* ---- sem status ----*/
                       sfa-export.data-fim       = ?
                       sfa-export.data-inicio    = ?
                       sfa-export.data-movto     = NOW
                       sfa-export.ind-situacao   = 1      /*---- Pendente -----*/.
          
             END.
          END.
       END.
       IF es-fornecedor-ariba.cpf > ""
       THEN DO:
          FIND FIRST es-api-param NO-LOCK                               
               WHERE es-api-param.ind-tipo-trans = 2  /*---- Saida ----*/
                 AND es-api-param.cd-tipo-integr = 12 /*---- Integra»’o B2E PJ ------*/ 
               NO-ERROR.
          IF AVAIL es-api-param 
          THEN DO:
             IF NOT CAN-FIND(FIRST sfa-export 
                             WHERE sfa-export.chave = es-fornecedor-ariba.cnpj
                               AND sfa-export.ind-situacao < 2) 
             THEN DO:
                CREATE api-export-b2e-pf.
                ASSIGN api-export-b2e-pf.cd-tipo-integr = es-api-param.cd-tipo-integr
                       api-export-b2e-pf.id-movto       = NEXT-VALUE(seq-export)
                       api-export-b2e-pf.cpf            = es-fornecedor-ariba.cpf
                       api-export-b2e-pf.data-movto     = NOW
                       api-export-b2e-pf.c-json         = ?.
          
                CREATE sfa-export.
                ASSIGN sfa-export.ind-tipo-trans = es-api-param.ind-tipo-trans
                       sfa-export.id-movto       = es-api-param.cd-tipo-integr
                       sfa-export.cd-tipo-integr = api-export-b2e-pf.cd-tipo-integr
                       sfa-export.chave          = api-export-b2e-pf.cpf
                       sfa-export.cod-status     = 0      /* ---- sem status ----*/
                       sfa-export.data-fim       = ?
                       sfa-export.data-inicio    = ?
                       sfa-export.data-movto     = NOW
                       sfa-export.ind-situacao   = 1      /*---- Pendente -----*/.
          
             END.
          END.
       END.
   END.*/

   //RUN pi-processa (es-api-param.ind-tipo-trans,
   //                     es-api-param.cd-tipo-integr
   //                    ).  
    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF



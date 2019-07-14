&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : aribalevelapprovalmla.p
    Purpose     : API REST para exportar n¡vel de aprova‡Æo mla
    Syntax      :
    Description : Clientes
    Author(s)   : Cleberson Silva - TOTVS Private - FSW
    Created     : 02/07/2019
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{utp/ut-api.i}
{utp/ut-api-utils.i}

{include/i-prgvrs.i aribalevelapprovalmla 2.00.00.000} /*** 010000 ***/

//{utp/ut-api-action.i pi-get GET /~*}
{utp/ut-api-action.i pi-getAll GET /~**}
{utp/ut-api-notfound.i}

DEFINE TEMP-TABLE tt-erros
    FIELD cod-erro  AS INTEGER
    FIELD desc-erro AS CHARACTER FORMAT "x(256)"
    FIELD desc-arq  AS CHARACTER.


DEFINE TEMP-TABLE ttCSV NO-UNDO SERIALIZE-NAME "MLAList"
    FIELD conteudo AS CHAR SERIALIZE-NAME "conteudo".


DEF TEMP-TABLE de-para-tipo NO-UNDO
    FIELD doc-totvs         AS INT
    FIELD tp-ariba          AS CHAR.

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
   Other Settings: CODE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-pi-getAll) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-getAll Procedure 
PROCEDURE pi-getAll :
/*------------------------------------------------------------------------------
  Purpose:  Retorna a lista de clientes   
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER jsonInput   AS JsonObject NO-UNDO.
    DEFINE OUTPUT PARAMETER jsonOutput  AS JsonObject NO-UNDO.

    DEFINE VARIABLE jsonNivelAprov AS JsonArray   NO-UNDO.

    DEFINE VARIABLE cConteudo AS CHARACTER   NO-UNDO.

    EMPTY TEMP-TABLE RowErrors.

    RUN pi-load-depara-docto.

    FOR EACH mla-hierarquia-faixa NO-LOCK:

        IF NOT CAN-FIND(FIRST de-para-tipo 
                        WHERE de-para-tipo.doc-totvs = mla-hierarquia-faixa.cod-tip-doc) THEN NEXT.

        FOR EACH de-para-tipo NO-LOCK
           WHERE de-para-tipo.doc-totvs = mla-hierarquia-faixa.cod-tip-doc:

            FIND FIRST mla-faixa-aprov OF mla-hierarquia-faixa NO-LOCK NO-ERROR.

            ASSIGN cConteudo = SUBSTITUTE("&1,&2,&3,&4,&5,&6,&7,&8",                        
                              mla-hierarquia-faixa.cod-usuar,                              
                              STRING(mla-hierarquia-faixa.seq-aprov),                      
                              de-para-tipo.tp-ariba,                                       
                              STRING(mla-hierarquia-faixa.cod-lotacao),                    
                              mla-hierarquia-faixa.cod-estabel,                            
                              STRING(mla-hierarquia-faixa.ep-codigo),                      
                              STRING(mla-hierarquia-faixa.num-faixa),                      
                              REPLACE(STRING(mla-faixa-aprov.limite-ini),",","")           
                              + "-" + REPLACE(STRING(mla-faixa-aprov.limite-fim),",","")).


            CREATE ttCsv.
            ASSIGN ttCsv.conteudo = cConteudo.
        END.
    END.

    ASSIGN jsonNivelAprov = NEW JsonArray().
           jsonNivelAprov:Read(TEMP-TABLE ttCSV:HANDLE).


    RUN createJsonResponse(INPUT jsonNivelAprov, INPUT TABLE RowErrors, INPUT FALSE, OUTPUT jsonOutput).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-load-cond-payments-json) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-load-cond-payments-json Procedure 
PROCEDURE pi-load-cond-payments-json :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER jsonCondPayment AS JsonObject NO-UNDO.
    
    ASSIGN jsonCondPayment = NEW JsonObject().
    
    jsonCondPayment:Add("CodigoCondicao", cond-pagto.cod-cond-pag).
    jsonCondPayment:Add("DescricaoCondicao", cond-pagto.descricao).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-load-customer-json) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-load-customer-json Procedure 
PROCEDURE pi-load-customer-json :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER jsonCustomer AS JsonObject NO-UNDO.

    ASSIGN jsonCustomer = NEW JsonObject().
    jsonCustomer:Add("RazaoSocial",emitente.nome-emit).
    jsonCustomer:Add("Cnpj",emitente.cgc).        
    jsonCustomer:Add("IE",emitente.ins-estadual).
    jsonCustomer:Add("Email",emitente.e-mail).     
    jsonCustomer:Add("Telefone",emitente.telefone[1]).
    jsonCustomer:Add("CodigoCliente",emitente.cod-emitente).
    jsonCustomer:Add("TipoClienteCanal","").
    jsonCustomer:Add("GrupoEconomico",emitente.cod-gr-cli). 
    jsonCustomer:Add("EmailXml",emitente.e-mail).       
    jsonCustomer:Add("EmailFinanceiro",emitente.e-mail). 
    jsonCustomer:Add("TelefoneFinanceiro",emitente.telefone[1]). 
    jsonCustomer:Add("Contribuinteicms", emitente.contrib-icms). 
    jsonCustomer:Add("Suframa",emitente.cod-suframa). 
    jsonCustomer:Add("NomeAbreviado",emitente.nome-abrev). 
    jsonCustomer:Add("LimiteCredito",emitente.lim-credito). 
    jsonCustomer:Add("AvaliacaoCredito", "").
    jsonCustomer:Add("TipoCredito", ""). 
    jsonCustomer:Add("DataLimiteCredito", emitente.dt-lim-cred).
    jsonCustomer:Add("SaldoCredito", 0). 
    jsonCustomer:Add("Banco",STRING(emitente.cod-banco,"999")).      
    jsonCustomer:Add("Agencia",emitente.agencia).             
    jsonCustomer:Add("Conta",emitente.conta-corren).       
    jsonCustomer:Add("IM",emitente.ins-municipal).   
    jsonCustomer:Add("NaturezaCliente",""). 
    jsonCustomer:Add("Matriz",emitente.nome-matriz).      
    jsonCustomer:Add("Representante",emitente.cod-rep).  
    jsonCustomer:Add("Microregiao",emitente.nome-mic-reg).  
    jsonCustomer:Add("ClienteExigeLTDAUnico", NO).  
    jsonCustomer:Add("ExigeCertifAnalise", NO). 
    jsonCustomer:Add("NaoRecebeLtdaProxVencto",NO).
    jsonCustomer:Add("RamoAtividade",emitente.atividade).  
    jsonCustomer:Add("Portador",emitente.portador).  
    jsonCustomer:Add("PortadorPreferencial",emitente.port-prefer).   
    jsonCustomer:Add("ClienteCobranca",emitente.end-cobranca). 


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-load-depara-docto) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-load-depara-docto Procedure 
PROCEDURE pi-load-depara-docto :
/*------------------------------------------------------------------------------
  Purpose:  carrega depara dos documentos ariba   
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    CREATE de-para-tipo.
    ASSIGN de-para-tipo.doc-totvs = 7           
           de-para-tipo.tp-ariba  = "CUS_REQ_01".
    CREATE de-para-tipo.
    ASSIGN de-para-tipo.doc-totvs = 7           
           de-para-tipo.tp-ariba  = "CUS_REQ_02".
    CREATE de-para-tipo.
    ASSIGN de-para-tipo.doc-totvs = 8           
           de-para-tipo.tp-ariba  = "CUS_REQ_03".
    CREATE de-para-tipo.
    ASSIGN de-para-tipo.doc-totvs = 7           
           de-para-tipo.tp-ariba  = "CUS_REQ_04".
    CREATE de-para-tipo.
    ASSIGN de-para-tipo.doc-totvs = 7           
           de-para-tipo.tp-ariba  = "CUS_REQ_05".
    CREATE de-para-tipo.
    ASSIGN de-para-tipo.doc-totvs = 7           
           de-para-tipo.tp-ariba  = "CUS_REQ_06".
    CREATE de-para-tipo.
    ASSIGN de-para-tipo.doc-totvs = 7           
           de-para-tipo.tp-ariba  = "CUS_REQ_07".
    CREATE de-para-tipo.
    ASSIGN de-para-tipo.doc-totvs = 7           
           de-para-tipo.tp-ariba  = "CUS_REQ_08".
    CREATE de-para-tipo.
    ASSIGN de-para-tipo.doc-totvs = 7           
           de-para-tipo.tp-ariba  = "CUS_REQ_09".
    CREATE de-para-tipo.
    ASSIGN de-para-tipo.doc-totvs = 7           
           de-para-tipo.tp-ariba  = "CUS_REQ_10".
    CREATE de-para-tipo.
    ASSIGN de-para-tipo.doc-totvs = 7           
           de-para-tipo.tp-ariba  = "CUS_REQ_11".
    CREATE de-para-tipo.
    ASSIGN de-para-tipo.doc-totvs = 2           
           de-para-tipo.tp-ariba  = "*".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : customer.p
    Purpose     : API REST para consulta de clientes
    Syntax      :
    Description : Clientes
    Author(s)   : Cleberson Silva - TOTVS Private - CDE
    Created     : 06/12/2018
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{utp/ut-api.i}
{utp/ut-api-utils.i}

{include/i-prgvrs.i customer 2.00.00.000} /*** 010000 ***/

{utp/ut-api-action.i pi-get GET /~*/~*}
{utp/ut-api-action.i pi-getAll GET /~*}
{utp/ut-api-notfound.i}

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

&IF DEFINED(EXCLUDE-pi-get) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-get Procedure 
PROCEDURE pi-get :
/*------------------------------------------------------------------------------
  Purpose: Retorna as informacoes do cliente    
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER jsonInput   AS JsonObject NO-UNDO.
    DEFINE OUTPUT PARAMETER jsonOutput AS JsonObject NO-UNDO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

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

    DEFINE VARIABLE jsonCustomersArray AS JsonArray   NO-UNDO.
    DEFINE VARIABLE jsonAddressArray   AS JsonArray   NO-UNDO.
    DEFINE VARIABLE jsonCondPayArray   AS JsonArray   NO-UNDO.


    DEFINE VARIABLE jsonCustomerObj   AS JsonObject  NO-UNDO.
    DEFINE VARIABLE jsonAddressObj    AS JsonObject  NO-UNDO.
    DEFINE VARIABLE jsonCondPayObj    AS JsonObject  NO-UNDO.

    DEFINE VARIABLE oJson   AS JsonObject  NO-UNDO.


    EMPTY TEMP-TABLE RowErrors.

    ASSIGN jsonCustomersArray = NEW JsonArray().
    
    /*Retorna somente cliente*/
    FOR EACH emitente NO-LOCK 
       WHERE emitente.cod-emitente <> 0
         AND emitente.identific    <> 2:

        /*-- carrega as informacoes dos clientes --*/
        RUN pi-load-customer-json IN THIS-PROCEDURE(OUTPUT jsonCustomerObj). 
        
        ASSIGN jsonAddressArray   = NEW JsonArray().
        FOR EACH loc-entr NO-LOCK
           WHERE loc-entr.nome-abrev = emitente.nome-abrev:
            /*-- carrega informcoes do endereco dos clientes --*/
            RUN pi-load-address-json IN THIS-PROCEDURE(OUTPUT jsonAddressObj).     
            jsonAddressArray:ADD(jsonAddressObj).                                      
        END.

       
        //IF NOT AVAILABLE loc-entr THEN
        //DO:
        //    ASSIGN jsonAddressObj = NEW JsonObject().
        //    jsonAddressArray:ADD(jsonAddressObj).
        //END.
            
        
        /*-- cria o array de enderecos --*/
        jsonCustomerObj:ADD("EnderecoList",jsonAddressArray).


        /*-- inicializa o array de condicao de pagamento --*/
        ASSIGN jsonCondPayArray = NEW JsonArray().


        FIND FIRST cond-pagto WHERE cond-pagto.cod-cond-pag = emitente.cod-cond-pag NO-LOCK NO-ERROR.
        IF  AVAILABLE cond-pagto THEN
            RUN pi-load-cond-pagto-json IN THIS-PROCEDURE(OUTPUT jsonCondPayObj).
        ELSE
            jsonCondPayObj = NEW JsonObject().
        
            
        /*-- cria o array de condicao de pagamento --*/  
        jsonCondPayArray:ADD(jsonCondPayObj).
        jsonCustomerObj:ADD("CondicaoPagamentoList",jsonCondPayArray). 
        

        jsonCustomersArray:ADD(jsonCustomerObj).
        
    END.



    RUN createJsonResponse(INPUT jsonCustomersArray, INPUT TABLE RowErrors, INPUT FALSE, OUTPUT jsonOutput).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-load-address-json) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-load-address-json Procedure 
PROCEDURE pi-load-address-json :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oJsonAddress AS JsonObject NO-UNDO.

    ASSIGN oJsonAddress = NEW JsonObject().
    oJsonAddress:ADD("Pais",loc-entr.pais).
    oJsonAddress:ADD("Complemento",loc-entr.endereco_text).
    oJsonAddress:ADD("Cidade",loc-entr.cidade).
    oJsonAddress:ADD("Bairro",loc-entr.bairro).
    oJsonAddress:ADD("Estado",loc-entr.estado).

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


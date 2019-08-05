/*----------------------------------------------------------------------------------------------/
 Programa..: esint001bi.p
 Objetivo..: Interface Integraá∆o Pedidos SFA - Importaá∆o
 Data......: 26/02/2019
 Autor.....: RogÇrio Dias
 Vers∆o....: 1.000.000
-----------------------------------------------------------------------------------------------*/

/* ------- Importaá∆o de Classes ------ */
using Progress.Json.OBJECTModel.*.

{utp/ut-glob.i} 
/*{include/i-rpexa.i}*/

/* ------- Definiá∆o de ParÉmetros ----- */
DEFINE INPUT  PARAMETER r-table     AS ROWID     NO-UNDO.
DEFINE OUTPUT PARAMETER c-erro      AS CHARACTER NO-UNDO.
/*DEFINE INPUT  PARAMETER pJsonInput  AS JsonObject NO-UNDO.*/

/* ------- Definiá∆o de Vari†veis ------ */
DEFINE VARIABLE cLongJson        AS LONGCHAR   NO-UNDO.
DEFINE VARIABLE lRetJson         AS LOGICAL    NO-UNDO.
DEFINE VARIABLE body             AS JsonObject NO-UNDO.
DEFINE VARIABLE jsonOutput       AS JsonObject NO-UNDO.
DEFINE VARIABLE arrJson          AS JsonArray  NO-UNDO.
DEFINE VARIABLE oJsonObject      AS JsonObject NO-UNDO.
DEFINE VARIABLE hBufferMain      AS HANDLE     NO-UNDO.
DEFINE VARIABLE hBufferSec       AS HANDLE     NO-UNDO.
DEFINE VARIABLE oJsonObjectMain  AS JsonObject NO-UNDO.
DEFINE VARIABLE oJsonObjectSec   AS JsonObject NO-UNDO.
DEFINE VARIABLE oJsonArrayMain   AS JsonArray  NO-UNDO.
DEFINE VARIABLE oJsonArraySec    AS JsonArray  NO-UNDO.
DEFINE VARIABLE iCountMain       AS INTEGER    NO-UNDO.
DEFINE VARIABLE iCountSec        AS INTEGER    NO-UNDO.
DEFINE VARIABLE cprop            AS CHARACTER  NO-UNDO.
DEFINE VARIABLE icodentrega      AS INTEGER    NO-UNDO.
DEFINE VARIABLE ccodentrega      AS CHARACTER  NO-UNDO.

DEF TEMP-TABLE tt-param NO-UNDO 
    FIELD destino           AS INTEGER 
    FIELD arquivo           AS CHAR FORMAT "x(35)"
    FIELD usuario           AS CHAR FORMAT "x(12)"
    FIELD data-exec         AS DATE 
    FIELD tipo-excel        AS INT
    FIELD hora-exec         AS INTEGER                     
    FIELD cod-rep-ini       LIKE repres.cod-rep INITIAL 0
    FIELD cod-rep-fim       LIKE repres.cod-rep INITIAL 9999999
    FIELD dt-dig-ini        AS DATE
    FIELD dt-dig-fim        AS DATE
    FIELD cod-emit-ini      LIKE emitente.cod-emit INITIAL 0
    FIELD cod-emit-fim      LIKE emitente.cod-emit INITIAL 9999999.

DEF TEMP-TABLE tt-digita no-undo
    FIELD ordem             as integer   format ">>>>9"
    FIELD exemplo           as character format "x(30)"
    INDEX id ordem.

def var raw-param           as raw no-undo.
def temp-table tt-raw-digita
   field raw-digita         as raw.

DEFINE TEMP-TABLE tt-retorno NO-UNDO
    FIELD codigoSales AS CHARACTER SERIALIZE-NAME "CodigoSalesforce"
    FIELD l-status    AS LOGICAL   SERIALIZE-NAME "Status"
    FIELD c-descr     AS CHARACTER SERIALIZE-NAME "Descricao"
    FIELD nr-pedcli   AS CHARACTER serialize-name "NumeroPedidoCliente"  
    FIELD nome-abrev  AS CHARACTER serialize-name "NomeAbreviadoCliente"  
    FIELD nr-pedido   AS INTEGER   serialize-name "CodigoPedido"                 .


DEF VAR c-nr-pedcli         AS c                 NO-UNDO.
DEF VAR i                   AS i                 NO-UNDO.
DEF VAR c-aux-loc-entr      AS c                 NO-UNDO.
DEFINE VARIABLE m-json      AS MEMPTR            NO-UNDO.
DEFINE VARIABLE myParser    AS ObjectModelParser NO-UNDO. 
DEFINE VARIABLE pJsonInput  AS JsonObject        NO-UNDO.

/* ------- Definiá∆o de Temp-Tables e Datasets ------ */
/**/
{esp\esint001ai.i}
{esp\esint001ar.i}
{esp\esint007.i}
/**/   
{method/dbotterr.i}

DEF TEMP-TABLE tt-sf-pedido              NO-UNDO 
          LIKE geo-pedido
    FIELD nr-pedcli-orig AS CHARACTER.

DEF TEMP-TABLE tt-sf-item-pedido         NO-UNDO 
          LIKE geo-item_pedido
    FIELD it-codigo AS CHARACTER
    INDEX i it-codigo.

DEF TEMP-TABLE tt-pedido-erro NO-UNDO
         FIELD cd-pedido-palm LIKE tt-sf-pedido.cd_pedido_palm
         FIELD cd-vendedor    LIKE tt-sf-pedido.cd_vendedor
         FIELD cod-msg        AS INT
         FIELD msg-erro       AS CHAR
         FIELD msg-padrao     AS LOG
         FIELD cd-cliente     AS INT
         FIELD nome-abrev     AS CHARACTER
         FIELD nr-pedcli      AS CHARACTER.

DEF VAR c-nome-abrev LIKE emitente.nome-abrev NO-UNDO.

/*------------------------------ Main Begin ----------------------------*/
ASSIGN c-erro = "".

FIND FIRST es-api-import NO-LOCK WHERE ROWID(es-api-import) = r-table NO-ERROR.
IF NOT AVAIL es-api-import THEN RETURN "NOK".

/* ------- Grava clob para longchar ----- */
FIND FIRST es-api-import-ped OF es-api-import NO-LOCK NO-ERROR.
IF NOT AVAIL es-api-import-ped THEN RETURN "NOK".

/* ------ RogÇrio Dias - Gera Json Ö partir de Longchar convertendo para UTF8 ----- */
FIX-CODEPAGE(cLongJson) = "UTF-8".

COPY-LOB es-api-import-ped.c-json TO m-json.
COPY-LOB m-json TO cLongJson NO-CONVERT.

myParser = NEW ObjectModelParser(). 
pJsonInput = CAST(myParser:Parse(cLongJson),JsonObject).

/*---------------------------------------------------------------------------------*/

/* ---- Là propriedade Principal ---- */        
oJsonArrayMain = pJsonInput /*:GetJsonObject("payload":U) */ :GetJsonArray("req":U) NO-ERROR.     
IF ERROR-STATUS:ERROR THEN DO:
    c-erro = "Ocorreram erros durante o processamento: " + ERROR-STATUS:GET-MESSAGE(1).
    RETURN "NOK".
END.

CREATE tt-sf-pedido.

RUN pi-cria-tt-sf-pedido.

FOR FIRST emitente NO-LOCK
    WHERE emitente.cod-emitente = tt-sf-pedido.cd_cliente:
    
    ASSIGN tt-sf-pedido.nr_cnpj_cpf_entr = emitente.cgc
           c-nome-abrev = emitente.nome-abrev.
    ASSIGN icodentrega = INTEGER(tt-sf-pedido.cd_endereco_entr) NO-ERROR.
    ASSIGN ccodentrega = "".
    IF icodentrega <> ? THEN
       ASSIGN ccodentrega = STRING(tt-sf-pedido.cd_endereco_entr, "99").
    IF tt-sf-pedido.cd_endereco_entr BEGINS "Padr" 
    THEN FOR  FIRST loc-entr NO-LOCK
         WHERE loc-entr.nome-abrev  = emitente.nome-abrev
           AND loc-entr.cod-entrega  BEGINS "Padr":
        ASSIGN
           tt-sf-pedido.nm_cidade_entr = TRIM(loc-entr.cidade).
    END.
    ELSE
    DO:
       IF ccodentrega = "" THEN
       DO:
          FOR FIRST loc-entr NO-LOCK
              WHERE loc-entr.nome-abrev  = emitente.nome-abrev
              AND loc-entr.cod-entrega = tt-sf-pedido.cd_endereco_entr:
             ASSIGN tt-sf-pedido.nm_cidade_entr = loc-entr.cidade.
          END.
       END.  /* ccodentrega = "" */
       IF ccodentrega <> "" THEN
       DO:
          FOR FIRST loc-entr NO-LOCK
              WHERE loc-entr.nome-abrev  = emitente.nome-abrev
              AND   loc-entr.cod-entrega = ccodentrega:
             ASSIGN tt-sf-pedido.nm_cidade_entr = loc-entr.cidade.
          END.
       END.  /* ccodentrega <> "" */

    END.

END.

IF NOT TEMP-TABLE tt-sf-pedido:HAS-RECORDS 
THEN DO: 
    ASSIGN c-erro = "N∆o h† registros para processar".
    RETURN "NOK".
END.
ELSE DO:
   CREATE tt-param.
   ASSIGN tt-param.usuario        = c-seg-usuario  
          tt-param.destino        = 2              
          tt-param.data-exec      = TODAY          
          tt-param.hora-exec      = TIME           
          tt-param.cod-rep-ini    = 0
          tt-param.cod-rep-fim    = 99999
          tt-param.cod-emit-ini   = 0              
          tt-param.cod-emit-fim   = 999999
          tt-param.arquivo        = SESSION:TEMP-DIRECTORY + "imp-sf-" + REPLACE(REPLACE(STRING(NOW,"999999 hh:mm:ss")," ",""),":","") + ".lst".        
          tt-param.arquivo        = SESSION:TEMP-DIRECTORY + 'pedido.lst'.

   RAW-TRANSFER tt-param TO raw-param.

   RUN esp/esint001birp.p (INPUT raw-param                      ,
                           INPUT TABLE tt-raw-digita            ,
                           INPUT TABLE tt-sf-pedido             ,
                           INPUT TABLE tt-sf-item-pedido        ,
                           OUTPUT TABLE tt-pedido-erro           ,
                           OUTPUT       c-nr-pedcli ) NO-ERROR.
   IF ERROR-STATUS:ERROR THEN DO:
       c-erro = "Ocorreram erros durante o processamento: " + ERROR-STATUS:GET-MESSAGE(1).
       RETURN "NOK".
   END.
   
   IF c-nr-pedcli <> "" THEN do:
       FIND CURRENT es-api-import EXCLUSIVE-LOCK NO-ERROR.
       IF AVAIL es-api-import THEN
           ASSIGN es-api-import.chave = c-nr-pedcli.
   END.

   RELEASE es-api-import.

   FOR EACH tt-pedido-erro:
       IF c-erro > "" THEN ASSIGN c-erro = c-erro  + CHR(10).

       ASSIGN c-erro = c-erro
                 + "ERR: " 
                 + STRING(tt-pedido-erro.cod-msg)
                 + " - "
                 + tt-pedido-erro.msg-erro  
                 + CHR(10)
                 .

       IF tt-pedido-erro.nome-abrev <> "" THEN
          ASSIGN c-nome-abrev = tt-pedido-erro.nome-abrev.

       IF tt-pedido-erro.nr-pedcli <> "" THEN
           ASSIGN c-nr-pedcli = tt-pedido-erro.nr-pedcli.
   END.      

   IF c-erro <> "" THEN do:         
       IF AVAIL tt-sf-pedido THEN DO:
           RUN pi-processa-erro (INPUT-OUTPUT c-erro,
                                 INPUT tt-sf-pedido.id_pedido_web,
                                 INPUT c-nome-abrev ,
                                 INPUT c-nr-pedcli  ).                                 
           RETURN "NOK".
       END.
    END.
              
END.        
   


PROCEDURE pi-cria-tt-sf-pedido:

    DO iCountMain = 1 TO oJsonArrayMain:LENGTH:
        oJsonObjectMain =  oJsonArrayMain:GetJsonObject(iCountMain).
    
        if oJsonObjectMain:Has("ConcedeBonificacao" )           THEN ASSIGN	tt-sf-pedido.cd_tipo_pedido        = IF oJsonObjectMain:GetLogical("ConcedeBonificacao" ) = NO
                                                                                                                THEN "1" ELSE "2".
        if oJsonObjectMain:Has("CodigoEmitente" )               THEN ASSIGN	tt-sf-pedido.cd_cliente            = oJsonObjectMain:GetInteger("CodigoEmitente" ) NO-ERROR.
        
        if oJsonObjectMain:Has("CodigoEstabelecimento" )        THEN ASSIGN	tt-sf-pedido.cd_org_venda          = oJsonObjectMain:GetCharacter("CodigoEstabelecimento" ) NO-ERROR.
        
        /*if oJsonObjectMain:Has("NumeroPedidoRepresentante" )    THEN ASSIGN	tt-sf-pedido.cd_pedido_cliente     = oJsonObjectMain:GetCharacter("NumeroPedidoRepresentante" ) NO-ERROR.*/

        if oJsonObjectMain:Has("NumeroPedidoCliente" )          THEN ASSIGN	tt-sf-pedido.cd_pedido_cliente     = oJsonObjectMain:GetCharacter("NumeroPedidoCliente" ) NO-ERROR.
        
        if oJsonObjectMain:Has("CondicaoPagamento" )            THEN do:
            
            ASSIGN	tt-sf-pedido.cd_cond_pgto          = integer(oJsonObjectMain:GetCharacter("CondicaoPagamento" )) NO-ERROR.

            MESSAGE " tt-sf-pedido.cd_cond_pgto: "  tt-sf-pedido.cd_cond_pgto.

        END.
                

        if oJsonObjectMain:Has("Representante" )                THEN ASSIGN	tt-sf-pedido.cd_vendedor           = oJsonObjectMain:GetInteger("Representante" ) NO-ERROR.
        
        if oJsonObjectMain:Has("ModalidadeFrete" )              THEN ASSIGN	tt-sf-pedido.id_tipo_frete         = oJsonObjectMain:GetCharacter("ModalidadeFrete" ) NO-ERROR.
        
        if oJsonObjectMain:Has("LocalEntrega" )                 THEN ASSIGN	tt-sf-pedido.cd_endereco_entr      = oJsonObjectMain:GetCharacter("LocalEntrega" ) NO-ERROR.
        
        if oJsonObjectMain:Has("NumeroPedidoBonificacao" )      THEN ASSIGN	tt-sf-pedido.nr-pedcli-orig    = oJsonObjectMain:GetCharacter("NumeroPedidoBonificacao" ) NO-ERROR.
        
        if oJsonObjectMain:Has("DataEntrega" )                  THEN ASSIGN	tt-sf-pedido.dt_entrega            = oJsonObjectMain:GetDate("DataEntrega" ) NO-ERROR.
        
        if oJsonObjectMain:Has("CodigoSalesforce" )             THEN ASSIGN	tt-sf-pedido.id_pedido_web          = oJsonObjectMain:GetCharacter("CodigoSalesforce" ) NO-ERROR.
        
        if oJsonObjectMain:Has("DataEntregaPrevista" )          THEN DO: 
            ASSIGN	tt-sf-pedido.dt_entrega_calculada  = oJsonObjectMain:GetDate("DataEntregaPrevista" ) NO-ERROR.            
        END.
        ELSE DO:
           IF tt-sf-pedido.dt_entrega <> ?
           THEN ASSIGN
               tt-sf-pedido.dt_entrega_calculada = tt-sf-pedido.dt_entrega.
        END.
    
        if oJsonObjectMain:Has("TipoPedido" ) THEN ASSIGN	tt-sf-pedido.cd_tipo_pedido        = oJsonObjectMain:GetCharacter("TipoPedido" ) .

        /*
        IF tt-sf-pedido.cd_pedido_cliente = 0 OR tt-sf-pedido.cd_pedido_cliente = ? THEN DO:
            ASSIGN c-erro = c-erro = "N£mero Pedido Cliente nao informado".
            RETURN "NOK".
        END.
        */
        IF tt-sf-pedido.cd_vendedor = ? THEN DO:
            ASSIGN c-erro = c-erro + "C¢digo do Representante n∆o informado.".
            RETURN "NOK".
        END.
    
        IF tt-sf-pedido.cd_endereco_entr = ? THEN DO:
            ASSIGN c-erro = c-erro + "Local de entrega n∆o informado".
            RETURN "NOK".
        END.
    
        IF ERROR-STATUS:ERROR THEN DO:
            c-erro = "Ocorreram erros durante o processamento: " + ERROR-STATUS:GET-MESSAGE(1).
            RETURN "NOK".
        END.
    
        /* ------ Objetos diferentes de array  ------ */
        IF oJsonObjectMain:Has("ItemPedidoList") THEN DO:  
            oJsonArraySec = oJsonObjectMain:GetJsonArray("ItemPedidoList").
    
            DO iCountSec = 1 TO oJsonArraySec:LENGTH:
                oJsonObjectSec =  oJsonArraySec:GetJsonObject(iCountSec).          
    
                CREATE tt-sf-item-pedido.
                ASSIGN tt-sf-item-pedido.cd_produto = iCountSec.
           
                if oJsonObjectSec:Has("CodigoItem" )                   THEN ASSIGN	tt-sf-item-pedido.it-codigo       = oJsonObjectSec:GetCharacter("CodigoItem").
                if oJsonObjectSec:Has("QuantidadeUnidadeFaturamento" ) THEN ASSIGN	tt-sf-item-pedido.qt_item          = oJsonObjectSec:GetDecimal("QuantidadeUnidadeFaturamento" ).
                if oJsonObjectSec:Has("PrecoOriginal" )                THEN ASSIGN	tt-sf-item-pedido.vr_item          = oJsonObjectSec:GetDecimal("PrecoOriginal" ).
                ASSIGN tt-sf-pedido.vr_pedido = tt-sf-pedido.vr_pedido + (tt-sf-item-pedido.qt_item * tt-sf-item-pedido.vr_item).                   
            END.
        END.      
    END.


END PROCEDURE.

PROCEDURE pi-processa-erro:

    DEFINE INPUT-OUTPUT PARAMETER pErro        AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER pcodigosales AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER pNomeAbrev   AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER pNrPedcli    AS CHARACTER NO-UNDO.
    
    
    DEFINE VARIABLE h-temp            AS HANDLE     NO-UNDO.
    DEFINE VARIABLE h-esint002        AS HANDLE     NO-UNDO.
    DEFINE VARIABLE oJsonObjMain      AS JsonObject NO-UNDO.
    DEFINE VARIABLE oJsonArrayMain    AS JsonArray  NO-UNDO.
    DEFINE VARIABLE oJsonObjIni       AS jsonObject NO-UNDO.
    DEFINE VARIABLE ojsonArrayIni     AS JsonArray  NO-UNDO.
    DEFINE VARIABLE ojsonObjAux       AS JsonObject NO-UNDO.
    DEFINE VARIABLE ojsonArrayAux     AS JsonArray  NO-UNDO.
    DEFINE VARIABLE lresp             AS LOGICAL    NO-UNDO.
    DEFINE VARIABLE c-retorno         AS LONGCHAR   NO-UNDO.

    DEFINE VARIABLE c-Json            AS LONGCHAR   NO-UNDO.

    RUN esp/esint002.p PERSISTENT SET h-esint002.

    FIND FIRST ped-venda WHERE ped-venda.nome-abrev = pNomeAbrev 
                           AND ped-venda.nr-pedcli  = pNrPedCli NO-LOCK NO-ERROR.
    
    CREATE tt-retorno.
    ASSIGN tt-retorno.codigoSales     = pcodigoSales
           tt-retorno.l-status        = FALSE
           tt-retorno.c-descr         = perro
           tt-retorno.nome-abrev      = pNomeAbrev
           tt-retorno.nr-pedcli       = pNrPedCli
           tt-retorno.nr-pedido       = IF AVAIL ped-venda THEN ped-venda.nr-pedido ELSE 0.

    
    ASSIGN h-temp = BUFFER tt-retorno:HANDLE.

    IF valid-handle(h-temp) THEN DO:

        RUN piCriaObj IN h-esint002 (INPUT h-temp,
                                     OUTPUT ojsonObjIni,
                                     OUTPUT ojsonArrayIni,
                                     INPUT NO) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:
            ASSIGN c-erro = ERROR-STATUS:GET-MESSAGE(1).
            DELETE OBJECT h-temp.
            RETURN "NOK".
        END.
        DELETE OBJECT h-temp.
    END.

    oJsonArrayMain = NEW JsonArray().
    oJsonArrayMain:ADD(ojsonObjIni).

    /* ----- Cria Json Principal ------- */
    oJsonObjMain = NEW JsonObject().
    oJsonObjMain:ADD("req",oJsonArrayMain).

    RUN piGeraVarJson IN h-esint002 (INPUT oJsonObjMain,
                                     OUTPUT c-Json) NO-ERROR.

    MESSAGE "Variavel: " SUBstring(c-json,1,2000).
    
    IF ERROR-STATUS:ERROR THEN DO:
        ASSIGN c-erro = ERROR-STATUS:GET-MESSAGE(1).
        DELETE OBJECT h-esint002.
        RETURN "NOK".
    END.

    FIND FIRST es-api-param WHERE es-api-param.ind-tipo-trans = 2
                              AND es-api-param.cd-tipo-integr = 4 NO-LOCK NO-ERROR.

    /* ------------ Envia Objeto Json --------- */
    RUN piPostJsonObj IN h-esint002 (INPUT oJsonObjMain,
                                     INPUT rowid(es-api-param),
                                     OUTPUT lResp,
                                     OUTPUT TABLE RowErrors,
                                     OUTPUT c-retorno).    

    MESSAGE 'c-retorno: ' string(c-retorno).

    IF TEMP-TABLE rowErrors:HAS-RECORDS THEN DO:

        ASSIGN perro = "".

        FOR EACH rowErrors:
            ASSIGN Perro = Perro + string(rowerrors.ErrorNumber)  + " - " + rowerrors.ErrorDescription.
            DELETE OBJECT h-esint002.
            RETURN "NOK".
        END.
    END.       
    

END PROCEDURE.

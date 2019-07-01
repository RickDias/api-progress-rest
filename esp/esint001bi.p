/*----------------------------------------------------------------------------------------------/
 Programa..: esint001bi.p
 Objetivo..: Interface Integra‡Æo Pedidos SFA - Importa‡Æo
 Data......: 26/02/2019
 Autor.....: Rog‚rio Dias
 VersÆo....: 1.000.000
-----------------------------------------------------------------------------------------------*/

/* ------- Importa‡Æo de Classes ------ */
using Progress.Json.OBJECTModel.*.

{utp/ut-glob.i} 
/*{include/i-rpexa.i}*/

/* ------- Defini‡Æo de Parƒmetros ----- */
DEFINE INPUT  PARAMETER r-table     AS ROWID     NO-UNDO.
DEFINE OUTPUT PARAMETER c-erro      AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER pJsonInput  AS JsonObject NO-UNDO.

/* ------- Defini‡Æo de Vari veis ------ */
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

DEF VAR c-nr-pedcli         AS c   NO-UNDO.
DEF VAR i                   AS i   NO-UNDO.
DEF VAR c-aux-loc-entr      AS c   NO-UNDO.
DEFINE VARIABLE m-json      AS MEMPTR            NO-UNDO.
DEFINE VARIABLE myParser    AS ObjectModelParser NO-UNDO. 

/* ------- Defini‡Æo de Temp-Tables e Datasets ------ */
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
         FIELD cd-cliente     AS INT.

DEF VAR c-nome-abrev LIKE emitente.nome-abrev NO-UNDO.

/*------------------------------ Main Begin ----------------------------*/
ASSIGN c-erro = "".


MESSAGE "***** Time 1: " + STRING(TIME,"HH:MM:SS").

FIND FIRST sfa-import NO-LOCK WHERE ROWID(sfa-import) = r-table NO-ERROR.
IF AVAIL sfa-import THEN DO:

    /* ------- Grava clob para longchar ----- */
    FIND FIRST sfa-import-ped OF sfa-import NO-ERROR.

    MESSAGE "***** Time 2: " + STRING(TIME,"HH:MM:SS").

    IF AVAIL sfa-import-ped THEN DO:

        /* ------ Rog‚rio Dias - Gera Json … partir de Longchar convertendo para UTF8 ----- */
        FIX-CODEPAGE(cLongJson) = "UTF-8".

        COPY-LOB sfa-import-ped.c-json TO m-json.
        COPY-LOB m-json TO cLongJson NO-CONVERT.

        myParser = NEW ObjectModelParser(). 
        pJsonInput = CAST(myParser:Parse(cLongJson),JsonObject).

        /*---------------------------------------------------------------------------------*/


        /*COPY-LOB sfa-import-ped.c-json TO cLongJson.*/

        MESSAGE "***** Time 3: " + STRING(TIME,"HH:MM:SS").
        
        /* ---- Lˆ propriedade Principal ---- */        
        oJsonArrayMain = pJsonInput /*:GetJsonObject("payload":U) */ :GetJsonArray("req":U) NO-ERROR.     
        IF ERROR-STATUS:ERROR THEN DO:
            c-erro = "Ocorreram erros durante o processamento: " + ERROR-STATUS:GET-MESSAGE(1).
            RETURN "NOK".
        END.

        MESSAGE "***** Time 4: " + STRING(TIME,"HH:MM:SS").

        CREATE tt-sf-pedido.
        DO iCountMain = 1 TO oJsonArrayMain:LENGTH:
            oJsonObjectMain =  oJsonArrayMain:GetJsonObject(iCountMain).

            if oJsonObjectMain:Has("ConcedeBonificacao" )           THEN ASSIGN	tt-sf-pedido.cd_tipo_pedido        = IF oJsonObjectMain:GetLogical("ConcedeBonificacao" ) = NO
                                                                                                                     THEN "1" ELSE "2".
            MESSAGE " *****tt-sf-pedido.cd_tipo_pedido " tt-sf-pedido.cd_tipo_pedido.
            
            if oJsonObjectMain:Has("CodigoEmitente" )               THEN ASSIGN	tt-sf-pedido.cd_cliente            = oJsonObjectMain:GetInteger("CodigoEmitente" ) NO-ERROR.
            MESSAGE " *****tt-sf-pedido.cd_cliente " tt-sf-pedido.cd_cliente.
            if oJsonObjectMain:Has("CodigoEstabelecimento" )        THEN ASSIGN	tt-sf-pedido.cd_org_venda          = oJsonObjectMain:GetCharacter("CodigoEstabelecimento" ) NO-ERROR.
            MESSAGE " *****tt-sf-pedido.cd_org_venda " tt-sf-pedido.cd_org_venda.
            if oJsonObjectMain:Has("NumeroPedidoRepresentante" )    THEN ASSIGN	tt-sf-pedido.cd_pedido_cliente     = oJsonObjectMain:GetCharacter("NumeroPedidoRepresentante" ) NO-ERROR.
            MESSAGE " *****tt-sf-pedido.cd_pedido_cliente " tt-sf-pedido.cd_pedido_cliente.
            if oJsonObjectMain:Has("CondicaoPagamento" )            THEN ASSIGN	tt-sf-pedido.cd_cond_pgto          = integer(oJsonObjectMain:GetCharacter("CondicaoPagamento" )) NO-ERROR.
            MESSAGE " *****tt-sf-pedido.cd_cond_pgto " tt-sf-pedido.cd_cond_pgto.
            if oJsonObjectMain:Has("Representante" )                THEN ASSIGN	tt-sf-pedido.cd_vendedor           = oJsonObjectMain:GetInteger("Representante" ) NO-ERROR.
            MESSAGE " *****tt-sf-pedido.cd_vendedor " tt-sf-pedido.cd_vendedor.
            if oJsonObjectMain:Has("ModalidadeFrete" )              THEN ASSIGN	tt-sf-pedido.id_tipo_frete         = oJsonObjectMain:GetCharacter("ModalidadeFrete" ) NO-ERROR.
            MESSAGE " *****tt-sf-pedido.id_tipo_frete " tt-sf-pedido.id_tipo_frete.
            if oJsonObjectMain:Has("LocalEntrega" )                 THEN ASSIGN	tt-sf-pedido.cd_endereco_entr      = oJsonObjectMain:GetCharacter("LocalEntrega" ) NO-ERROR.
            MESSAGE " *****tt-sf-pedido.cd_endereco_entr " tt-sf-pedido.cd_endereco_entr.
            if oJsonObjectMain:Has("NumeroPedidoBonificacao" )      THEN ASSIGN	tt-sf-pedido.nr-pedcli-orig    = oJsonObjectMain:GetCharacter("NumeroPedidoBonificacao" ) NO-ERROR.
            MESSAGE " *****tt-sf-pedido.cd_endereco_entr " tt-sf-pedido.cd_endereco_entr.

            if oJsonObjectMain:Has("DataEntrega" )                  THEN ASSIGN	tt-sf-pedido.dt_entrega            = oJsonObjectMain:GetDate("DataEntrega" ) NO-ERROR.
            MESSAGE " *****tt-sf-pedido.dt_entrega " tt-sf-pedido.dt_entrega.

            if oJsonObjectMain:Has("CodigoSalesforce" )             THEN ASSIGN	tt-sf-pedido.id_pedido_web          = oJsonObjectMain:GetCharacter("CodigoSalesforce" ) NO-ERROR.
            MESSAGE " *****tt-sf-pedido.id_pedido_web " tt-sf-pedido.id_pedido_web.

            if oJsonObjectMain:Has("DataEntregaPrevista" )          
            THEN DO: 
               ASSIGN	tt-sf-pedido.dt_entrega_calculada  = oJsonObjectMain:GetDate("DataEntregaPrevista" ) NO-ERROR.
               MESSAGE " *****tt-sf-pedido.dt_entrega_calculada " tt-sf-pedido.dt_entrega_calculada.
            END.
            ELSE DO:
               IF tt-sf-pedido.dt_entrega <> ?
               THEN ASSIGN
                   tt-sf-pedido.dt_entrega_calculada = tt-sf-pedido.dt_entrega.
            END.

            if oJsonObjectMain:Has("TipoPedido" ) THEN ASSIGN	tt-sf-pedido.cd_tipo_pedido        = oJsonObjectMain:GetCharacter("TipoPedido" ) .

            IF tt-sf-pedido.cd_vendedor = ? THEN DO:
                ASSIGN c-erro = c-erro + "C¢digo do Representante nÆo informado.".
                RETURN "NOK".
            END.

            IF tt-sf-pedido.cd_endereco_entr = ? THEN DO:
                ASSIGN c-erro = c-erro + "Local de entrega nÆo informado".
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
                    ASSIGN
                       tt-sf-item-pedido.cd_produto = iCountSec.
                    MESSAGE " *****tt-sf-item-pedido.cd_codigo " tt-sf-item-pedido.cd_produto.

                    if oJsonObjectSec:Has("CodigoItem" )                   THEN ASSIGN	tt-sf-item-pedido.it-codigo       = oJsonObjectSec:GetCharacter("CodigoItem").
                    MESSAGE " *****tt-sf-item-pedido.it-codigo " tt-sf-item-pedido.it-codigo.
                    if oJsonObjectSec:Has("QuantidadeUnidadeFaturamento" ) THEN ASSIGN	tt-sf-item-pedido.qt_item          = oJsonObjectSec:GetDecimal("QuantidadeUnidadeFaturamento" ).
                    MESSAGE " *****tt-sf-item-pedido.qt_item " tt-sf-item-pedido.qt_item.
                    if oJsonObjectSec:Has("PrecoOriginal" )                THEN ASSIGN	tt-sf-item-pedido.vr_item          = oJsonObjectSec:GetDecimal("PrecoOriginal" ).
                    MESSAGE " *****tt-sf-item-pedido.vr_item " tt-sf-item-pedido.vr_item.
                    ASSIGN 
                       tt-sf-pedido.vr_pedido = tt-sf-pedido.vr_pedido 
                                              + (tt-sf-item-pedido.qt_item * tt-sf-item-pedido.vr_item).                   
                END.
            END.      
        END.
        
        FOR FIRST emitente NO-LOCK
            WHERE emitente.cod-emitente = tt-sf-pedido.cd_cliente:
            
            ASSIGN tt-sf-pedido.nr_cnpj_cpf_entr = emitente.cgc
                   c-nome-abrev = emitente.nome-abrev.

            IF tt-sf-pedido.cd_endereco_entr BEGINS "Padr" 
            THEN FOR  FIRST loc-entr NO-LOCK
                 WHERE loc-entr.nome-abrev  = emitente.nome-abrev
                   AND loc-entr.cod-entrega  BEGINS "Padr":
                ASSIGN
                   tt-sf-pedido.nm_cidade_entr = TRIM(loc-entr.cidade).
            END.
            ELSE FOR  FIRST loc-entr NO-LOCK
                 WHERE loc-entr.nome-abrev  = emitente.nome-abrev
                   AND loc-entr.cod-entrega = tt-sf-pedido.cd_endereco_entr:
                ASSIGN
                   tt-sf-pedido.nm_cidade_entr = loc-entr.cidade.
            END.

        END.

        MESSAGE "***** Time 5: " + STRING(TIME,"HH:MM:SS").
        
        MESSAGE "**** Iniciando cria‡Æo de pedido".
        IF NOT TEMP-TABLE tt-sf-pedido:HAS-RECORDS 
        THEN DO: 
            ASSIGN c-erro = "NÆo h  registros para processar".
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
                 /*   
                    tt-param.dt-dig-ini     = 1/1/1          
                    tt-param.dt-dig-fim     = 12/31/9999     
                 */   
                 tt-param.cod-emit-ini   = 0              
                 tt-param.cod-emit-fim   = 999999
                 tt-param.arquivo        = SESSION:TEMP-DIRECTORY + "imp-sf-" + REPLACE(REPLACE(STRING(NOW,"999999 hh:mm:ss")," ",""),":","") + ".lst".        
                
                 tt-param.arquivo        = SESSION:TEMP-DIRECTORY + 'pedido.lst'.
 
           RAW-TRANSFER tt-param TO raw-param.

           MESSAGE "***** Time 6: " + STRING(TIME,"HH:MM:SS").
           
           RUN esp/esint001birp.p (INPUT raw-param                      ,
                                   INPUT TABLE tt-raw-digita            ,
                                   INPUT TABLE tt-sf-pedido             ,
                                   INPUT TABLE tt-sf-item-pedido        ,
                                   OUTPUT TABLE tt-pedido-erro           ,
                                   OUTPUT       c-nr-pedcli ) NO-ERROR.
           MESSAGE "***** Time 7: " + STRING(TIME,"HH:MM:SS").
           IF ERROR-STATUS:ERROR THEN DO:
               c-erro = "Ocorreram erros durante o processamento: " + ERROR-STATUS:GET-MESSAGE(1).
               RETURN "NOK".
           END.
           
           IF c-nr-pedcli > "" THEN do:
               ASSIGN sfa-import.chave = c-nr-pedcli.
           END.
        
           FOR EACH tt-pedido-erro:
               IF c-erro > "" THEN ASSIGN c-erro = c-erro  + CHR(10).
        
               ASSIGN c-erro = c-erro
                         + "ERR: " 
                         + STRING(tt-pedido-erro.cod-msg)
                         + " - "
                         + tt-pedido-erro.msg-erro  
                         + CHR(10)
                         .
           END.

           /*
           IF c-erro = "" AND c-nr-pedcli <> "" THEN DO:
               CREATE ext-ped-venda.
               ASSIGN ext-ped-venda.nome-abrev = c-nome-abrev
                      ext-ped-venda.nr-pedcli  = c-nr-pedcli
                      ext-ped-venda.nr-ped-sfa = IF AVAIL sfa-import-ped THEN sfa-import-ped.nr-ped-sfa ELSE "".
           END.
           */
           
           MESSAGE "**** ERRO: " + c-erro.
           MESSAGE "**** PEDIDO CRIADO: " c-nr-pedcli.
           MESSAGE "**** CHAVE  CRIADA: " sfa-import.chave.
        END.
        MESSAGE "**** Finalizando cria‡Æo de pedido".
    END.   
END.


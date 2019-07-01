/* Local Variable Definitions ---                                       */


def var l-ok               as logical no-undo.
def var c-arq-digita       as char    no-undo.
def var c-terminal         as char    no-undo.
def var c-arq-term         as char    no-undo.
DEF VAR c-arquivo-destino  AS CHAR    NO-UNDO.
DEF VAR c-xml-code         AS CHAR    NO-UNDO.
DEF VAR c-xmlLinha         AS CHAR    NO-UNDO.
DEF VAR natureza-operacao  as CHAR    NO-UNDO.
DEF VAR contaRegistro      as INTEGER    NO-UNDO.


{include/i-imdef.i}
{utp/ut-glob.i}
{cdp/cdcfgdis.i} /*include para pr²-processadores*/


{method/dbotterr.i}

DEF TEMP-TABLE tt-param-RowErrors NO-UNDO
    LIKE RowErrors.

{dibo/bodi159.i  "tt-ped-venda"}
{dibo/bodi154.i  "tt-ped-item"}
{dibo/bodi159.i2 "RowPedParam"}
{dibo/bodi157.i  "tt-ped-repre"}


DEF TEMP-TABLE tt-param-ped-venda NO-UNDO
    FIELD nome-abrev   LIKE tt-ped-venda.nome-abrev  
    FIELD nr-pedcli    LIKE tt-ped-venda.nr-pedcli
    FIELD cod-estabel  LIKE tt-ped-venda.cod-estabel 
    FIELD nr-pedrep    LIKE tt-ped-venda.nr-pedrep   
    FIELD no-ab-rep    LIKE tt-ped-venda.no-ab-reppri   
    FIELD nat-operacao LIKE tt-ped-venda.nat-operacao
    FIELD dt-emissao   LIKE tt-ped-venda.dt-emissao  
    FIELD dt-entrega   LIKE tt-ped-venda.dt-entrega  
    FIELD dt-entorig   LIKE tt-ped-venda.dt-entorig   
    FIELD ind-fat-par  LIKE tt-ped-venda.ind-fat-par 
    FIELD mo-codigo    LIKE tt-ped-venda.mo-codigo   
    FIELD ind-lib-nota LIKE tt-ped-venda.ind-lib-nota
    FIELD ind-tp-frete LIKE tt-ped-venda.ind-tp-frete
    FIELD nr-ind-finan LIKE tt-ped-venda.nr-ind-finan
    FIELD observacoes  LIKE tt-ped-venda.observacoes 
    FIELD cod-cond-pag LIKE tt-ped-venda.cod-cond-pag.


DEF TEMP-TABLE tt-param-ped-item NO-UNDO
    FIELD it-codigo    LIKE tt-ped-item.it-codigo
    FIELD qt-pedida    LIKE tt-ped-item.qt-pedida
    FIELD qt-un-fat    LIKE tt-ped-item.qt-un-fat
    FIELD vl-preori    LIKE tt-ped-item.vl-preori.

/*Informa»oes para o funcionamento das includes e chamada do rp.p*/ 
DEF VAR rs-execucao      AS i INIT 1.

DEF VAR c-programa-mg97  AS c.
DEF VAR c-versao-mg97    AS c.
DEF VAR c-arquivo        AS c.
DEF VAR c-seg-usuario    AS c.

DEF VAR hbodi159         AS HANDLE.
DEF VAR hShowMsg         AS HANDLE.
DEF VAR p-nr-pedido      AS CHAR.




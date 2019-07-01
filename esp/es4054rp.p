/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ES4054RP 2.09.00.034 } /*** "019034" ***/
{include/i-bfems2cad.i}

/*******************************************************************************
**
**  PROGRAMA:  es4054rp.p
**  DATA    :  dezembro/2005
**  AUTOR   :  Erika
**  OBJETIVO:  MµSCARA DE nf - A SER ENVIADO POR E-MAIL (EXCEL)
**
**  26/03/2007 - 11:18 RNK - foi incluido o campo ped-venda.contato na temp-table
**               tt-nota-fiscal, que ser† impresso no campo "S/PEDIDO No" da nf 
**               quando for <> brancos.
**               TambÇm foram alterados os prgs ft0515.i1, ft0515rp.p, ft0515rpi.p e
**               es4054rpi.p.
**  29/09/2009 - 09:32 - Alterado o ft0515.i1
**
** ago/2016 - SMF - Kraft - convers∆o campos totvs 12
******************************************************************************/

/* ---   Definicao de Tabelas Temporarias   --- */

def temp-table tt-msg
    field cod-mensagem like mensagem.cod-mensagem.



DEF TEMP-TABLE tt-raw-digita 
    FIELD raw-digita AS RAW.


{ftp/ft2010.i1} /* Definicao da temp-table tt-notas-geradas */
    

DEFINE NEW SHARED TEMP-TABLE item-nota NO-UNDO 
           FIELD registro              AS   ROWID 
           FIELD it-codigo             LIKE it-nota-fisc.it-codigo
           FIELD aliquota-icm          LIKE it-nota-fisc.aliquota-icm
           FIELD nr-seq-fat            LIKE it-nota-fisc.nr-seq-fat
           FIELD sit-tribut            AS   INTEGER FORMAT ">>>".

DEFINE TEMP-TABLE tt-certificado-exame LIKE es-certific-saldo
           FIELD cdn-exame             LIKE es-certific-exame.cdn-exame
           FIELD des-resultado         LIKE es-certific-exame.des-resultado
           FIELD nr-seq-impr           LIKE es-exame.nr-seq-impr
           FIELD desc-familia          AS   CHARACTER FORMAT "X(40)"
           FIELD des-categoria         LIKE es-certificado.des-categoria   
           FIELD des-classe             LIKE es-certificado.des-classe      
           FIELD des-tipo-class        LIKE es-certificado.des-tipo-class 
           FIELD cod-estabel           LIKE it-nota-fisc.cod-estabel
           FIELD serie                 LIKE it-nota-fisc.serie
           FIELD nr-nota-fis           LIKE it-nota-fisc.nr-nota-fis
           FIELD nr-seq-fat            LIKE it-nota-fisc.nr-seq-fat
           FIELD item-nota             LIKE it-nota-fisc.it-codigo
           INDEX codigo IS PRIMARY cod-uf-emiss
                                   cod-serie          
                                   cod-certific-class
           INDEX item-nota         cod-estabel
                                   serie
                                   nr-nota-fis
                                   nr-seq-fat
                                   item-nota.

DEFINE TEMP-TABLE tt-exame LIKE es-exame
           FIELD cod-estabel           LIKE it-nota-fisc.cod-estabel
           FIELD serie                 LIKE it-nota-fisc.serie
           FIELD nr-nota-fis           LIKE it-nota-fisc.nr-nota-fis
           FIELD nr-seq-fat            LIKE it-nota-fisc.nr-seq-fat
           FIELD item-nota             LIKE it-nota-fisc.it-codigo
           INDEX item-nota         cod-estabel
                                   serie
                                   nr-nota-fis
                                   nr-seq-fat
                                   item-nota.  

DEFINE TEMP-TABLE tt-certificado LIKE es-certific-saldo
          FIELD cod-estabel           LIKE it-nota-fisc.cod-estabel
          FIELD serie                 LIKE it-nota-fisc.serie
          FIELD nr-nota-fis           LIKE it-nota-fisc.nr-nota-fis
          FIELD nr-seq-fat            LIKE it-nota-fisc.nr-seq-fat
          FIELD item-nota             LIKE it-nota-fisc.it-codigo
          INDEX codigo IS PRIMARY cod-uf-emiss
                                  cod-serie          
                                  cod-certific-class
          INDEX item-nota         cod-estabel
                                  serie
                                  nr-nota-fis
                                  nr-seq-fat
                                  item-nota.
           
DEFINE TEMP-TABLE RowErrors NO-UNDO                    
           FIELD ErrorSequence         AS   INTEGER 
           FIELD ErrorNumber           AS   INTEGER 
           FIELD ErrorDescription      AS   CHAR 
           FIELD ErrorParameters       AS   CHAR 
           FIELD ErrorType             AS   CHAR 
           FIELD ErrorHelp             AS   CHAR 
           FIELD ErrorSubType          AS   CHAR.

DEFINE TEMP-TABLE RowErrorsAux NO-UNDO                    
           FIELD ErrorSequence         AS   INTEGER 
           FIELD ErrorNumber           AS   INTEGER 
           FIELD ErrorDescription      AS   CHAR 
           FIELD ErrorParameters       AS   CHAR 
           FIELD ErrorType             AS   CHAR 
           FIELD ErrorHelp             AS   CHAR 
           FIELD ErrorSubType          AS   CHAR.

/* Integracao Modulo de Importacao */
def temp-table tt-desp
    field cod-desp  as integer format ">>>>9"
    field descricao as char    format "x(30)"
    field val-desp  as decimal format ">>>>>,>>>,>>9.99999"
    index codigo is unique primary
          cod-desp.

DEF TEMP-TABLE tt-notas-recusadas NO-UNDO
    FIELD cod-estabel   LIKE nota-fiscal.cod-estabel
    FIELD serie         LIKE nota-fiscal.serie 
    FIELD nr-nota-fis   LIKE nota-fiscal.nr-nota-fis.


DEF TEMP-TABLE tt-nota-fiscal
    FIELD nr-nota-fis   LIKE nota-fiscal.nr-nota-fis
    FIELD cod-estabel   LIKE nota-fiscal.cod-estabel
    FIELD serie         LIKE nota-fiscal.serie 
    FIELD tipo          LIKE natur-oper.tipo  /*1-Entrada 2-Saida*/
    FIELD nome-emit     LIKE emitente.nome-emit
    FIELD nome-ab-cli   LIKE nota-fiscal.nome-ab-cli
    FIELD cod-emitente  LIKE emitente.cod-emitente
    FIELD dt-emis-nota  LIKE nota-fiscal.dt-emis-nota
    FIELD dt-saida      LIKE nota-fiscal.dt-saida
    FIELD esp-docto     LIKE nota-fiscal.esp-docto
    FIELD nome-trans    LIKE nota-fiscal.nome-trans
    FIELD cgc           LIKE emitente.cgc 
    FIELD nat-operacao  AS CHAR FORMAT "x(20)"
    /*FIELD nat           as character format "x.xx"*/
    FIELD nome-tr-red   LIKE nota-fiscal.nome-tr-red
    FIELD endereco      LIKE nota-fiscal.endereco
    FIELD bairro        LIKE nota-fiscal.bairro  
    FIELD cep           LIKE nota-fiscal.cep     
    FIELD cidade        LIKE nota-fiscal.cidade     
    FIELD telefone      LIKE emitente.telefone[1]   
    FIELD estado        LIKE nota-fiscal.estado     
    FIELD ins-estadual  LIKE nota-fiscal.ins-estadua
    FIELD denominacao   AS CHAR FORMAT "x(49)"
    FIELD hr-saida      as char format "xx:xx:xx"
    FIELD tipo-venda    as CHARACTER format "x(13)"
    FIELD parcela       as integer                             extent 6
    FIELD fatura        as char      format "x(16)"            extent 6
    FIELD venc-dup      as date      format "99/99/9999"       extent 6
    FIELD vl-dup        as decimal   format ">>>>>,>>>,>>9.99" extent 6
    FIELD vl-mercad     LIKE nota-fiscal.vl-mercad   
    FIELD vl-frete      LIKE nota-fiscal.vl-frete      
    FIELD vl-seguro     LIKE nota-fiscal.vl-seguro                           
    FIELD vl-ipi        LIKE nota-fiscal.vl-tot-ipi   
    FIELD vl-embalagem  LIKE nota-fiscal.vl-embalagem
    FIELD vl-tot-nota   LIKE nota-fiscal.vl-tot-nota 
    FIELD cod-entrega   LIKE nota-fiscal.cod-entrega
    FIELD nome-ab-trans like transporte.nome-abrev
    FIELD cod-transp    LIKE transporte.cod-transp
    FIELD nome          LIKE transporte.nome        
    FIELD pago          as character format "x" init " "
    FIELD placa         LIKE nota-fiscal.placa      
    FIELD uf-placa      LIKE nota-fiscal.uf-placa   
    FIELD trsp-endereco LIKE transporte.endereco    
    FIELD trsp-cidade   LIKE transporte.cidade      
    FIELD trsp-estado   LIKE transporte.estado      
    FIELD trsp-ins-estadual LIKE transporte.ins-estadual
    FIELD redesp        as character
    FIELD trsp-nome     LIKE transporte.nome
    FIELD trsp-cgc      LIKE transporte.cgc
    FIELD qt-volumes    as integer   extent 5 format ">>>,>>9"         
    FIELD especie       as character extent 5 format "x(30)"            
    FIELD marca-volume  LIKE nota-fiscal.marca-volume
    FIELD nr-volumes    LIKE nota-fiscal.nr-volumes  
    FIELD peso-bru-tot  LIKE nota-fiscal.peso-bru-tot
    FIELD peso-liq-tot  LIKE nota-fiscal.peso-liq-tot
    FIELD cod-rep       LIKE nota-fiscal.cod-rep  
    FIELD nr-pedcli     LIKE nota-fiscal.nr-pedcli
    FIELD nr-pedrep     LIKE ped-venda.nr-pedrep
    FIELD tp-pedido     LIKE ped-venda.tp-pedido
    FIELD repres        as character format "x(15)"
    FIELD mess          as character format "x(83)" extent 24 init "  "
    FIELD mensagem      AS CHAR format "x(152)"
    FIELD observacoes   AS char      FORMAT "x(200)"
    FIELD ind-sit-nota  LIKE nota-fiscal.ind-sit-nota
    FIELD contato       LIKE ped-venda.contato /* 26/03/2007 */
    INDEX idx-nota IS PRIMARY UNIQUE cod-estabel
                                     serie
                                     nr-nota-fis. 
 
DEF TEMP-TABLE tt-it-nota-fisc
    FIELD nr-nota-fis   LIKE nota-fiscal.nr-nota-fis
    FIELD cod-estabel   LIKE nota-fiscal.cod-estabel
    FIELD serie         LIKE nota-fiscal.serie 
    FIELD nr-seq-fat    LIKE it-nota-fisc.nr-seq-fat
    FIELD it-codigo     LIKE it-nota-fisc.it-codigo
    FIELD fm-codigo     LIKE ITEM.fm-codigo
    FIELD descricao     LIKE ITEM.desc-item
    FIELD class-fiscal  LIKE it-nota-fisc.class-fiscal    
    FIELD nat-operacao  LIKE it-nota-fisc.nat-operacao
    FIELD sit-tribut    as integer format ">>>" 
    FIELD un-fatur      LIKE it-nota-fisc.un-fatur  
    FIELD qt-fatur      LIKE it-nota-fisc.qt-fatur  
    FIELD aliquota-ipi  LIKE it-nota-fisc.aliquota-ipi
    FIELD vl-bicms-it   LIKE it-nota-fisc.vl-bicms-it  
    FIELD vl-icms-it    LIKE it-nota-fisc.vl-icms-it
    FIELD vl-bsubs-it   LIKE it-nota-fisc.vl-bsubs-it 
    FIELD vl-icmsub-it  LIKE it-nota-fisc.vl-icmsub-it
    FIELD vl-ipi-it     LIKE it-nota-fisc.vl-ipi-it
    FIELD cd-trib-ipi   LIKE it-nota-fisc.cd-trib-ipi
    FIELD vl-preuni     LIKE it-nota-fisc.vl-preuni
    FIELD vl-preori     LIKE it-nota-fisc.vl-preori
    FIELD vl-merc-liq   LIKE it-nota-fisc.vl-merc-liq 
    FIELD vl-merc-ori   LIKE it-nota-fisc.vl-merc-ori
    FIELD cd-trib-icm   LIKE it-nota-fisc.cd-trib-icm 
    FIELD aliquota-icm  LIKE it-nota-fisc.aliquota-icm
    FIELD conteudo      AS CHAR FORMAT "x(42)"
    INDEX idx-item IS PRIMARY cod-estabel
                              serie
                              nr-nota-fis
                              nr-seq-fat
                              it-codigo.

/*tabela que reune todas as naturezas que serao impressas no cabecalho da nota */
DEF TEMP-TABLE tt-natureza
    FIELD natureza AS CHAR FORMAT "x(10)"
    FIELD descricao LIKE natur-oper.denominacao
    index idx-natureza natureza.

DEF BUFFER bf-tt-natureza FOR tt-natureza.
DEF BUFFER bf-item        FOR ITEM.
DEF BUFFER bf-es-ticket   FOR es-ticket.

/* tabela de erro para tratamento da certidao */
DEFINE TEMP-TABLE tt-erro2 NO-UNDO
       FIELD i-cod-erro  AS INTEGER
       FIELD c-desc-erro AS CHARACTER FORMAT "x(40)"
       FIELD c-help-erro AS CHARACTER FORMAT "x(40)"
       INDEX idx-cod-erro IS PRIMARY i-cod-erro.

/* define temp-table tt-param                                    */
/*     field destino          as integer                         */
/*     field arquivo          as char                            */
/*     field usuario          as char                            */
/*     field data-exec        as date                            */
/*     field hora-exec        as integer                         */
/*     field classifica       as integer                         */
/*     field identific        as char                            */
/*     field ini-cod-estabel  as char                            */
/*     field fim-cod-estabel  as char                            */
/*     field ini-serie        as char                            */
/*     field fim-serie        as char                            */
/*     field ini-cdd-embarq  as integer                         */
/*     field fim-cdd-embarq  as integer                         */
/*     field ini-nr-nota-fis  as char                            */
/*     field fim-nr-nota-fis  as char                            */
/*     field rs-imprime       as integer                         */
/*     field banco            as INTEGER FORMAT "-999999999999"  */
/*     field cod-febraban     as INTEGER FORMAT "-999999999999"  */
/*     field cod-portador     as INTEGER FORMAT "-999999999999"  */
/*     field prox-bloq        as char                            */
/*     field c-instrucao      as char extent 5                   */
/*     field imprime-bloq     as logical.                        */
/*                                                               */

define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    field c_funcao         as character format "x(8)"
    field cod-estabel      like nota-fiscal.cod-estabel
    field serie            like nota-fiscal.serie
    field nr-nota-fis-ini  like nota-fiscal.nr-nota-fis
    field nr-nota-fis-fim  like nota-fiscal.nr-nota-fis
    field cdd-embarq-ini  like nota-fiscal.cdd-embarq
    field cdd-embarq-fim  like nota-fiscal.cdd-embarq           
    field data             as date initial ?
    field hora             as CHAR INITIAL "000000"
    field tipo             as int.


/* Parametros do Boleto */
define temp-table tt-param-2 NO-UNDO  
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    field classifica       as integer
    field desc-classifica  as char format "x(40)"
    FIELD ep-codigo        LIKE empresa.ep-codigo
    FIELD cod-estabel      LIKE estabelec.cod-estabel
    FIELD serie            LIKE nota-fiscal.serie
    FIELD nr-nota-ini      LIKE nota-fiscal.nr-nota-fis
    FIELD nr-nota-fim      LIKE nota-fiscal.nr-nota-fis
    FIELD dt-emissao-ini   AS DATE FORMAT 99/99/9999
    FIELD dt-emissao-fim   AS DATE FORMAT 99/99/9999
    FIELD cod-port-ini     LIKE mgadm.portador.cod-port
    FIELD cod-port-fim     LIKE mgadm.portador.cod-port
    FIELD nota-fiscal      AS LOGICAL
    FIELD boleto           AS LOGICAL
    FIELD reimpressao      AS LOGICAL
    Field padrao-banco     As Character    Format "x(40)" .


DEFINE NEW shared temp-table tt-notas-impressas
    field r-nota as rowid.

define temp-table tt-digita
    field ordem            as integer   format ">>>>9"
    field exemplo          as character format "x(30)"
    index id is primary unique
        ordem.


/* Temp-Table usada na formacao da observacao da Nota de Entrada */
DEF NEW SHARED TEMP-TABLE tt-detalhes
    FIELD c-esp-docto       LIKE es-movto-arroz.esp-docto
    FIELD i-tipo-trans      LIKE es-movto-arroz.tipo-trans
    FIELD c-tp-desconto     LIKE es-movto-arroz.tp-desconto
    FIELD de-analise        AS DEC DECIMALS 3
    FIELD de-quantidade     LIKE es-movto-arroz.quantidade
    FIELD de-valor-unit     LIKE es-movto-arroz.valor
    FIELD de-valor-tot      LIKE es-movto-arroz.valor
    FIELD de-perc           AS DEC
    FIELD c-historico       LIKE es-movto-arroz.historico.

/* ---   Definicao de PArametros de entrada   --- */

DEFINE INPUT PARAMETER raw-param AS RAW NO-UNDO.   
DEFINE INPUT PARAMETER TABLE FOR tt-raw-digita.

{esp/esbcapi004.i}
{cdp/cd0666.i}
{utp/ut-glob.i}
/* ---    Definicao de Buffer    --- */

DEFINE BUFFER b-nota-fiscal     FOR nota-fiscal.

/************************************
*   definicao de variaveis globais
************************************/

def new shared var l-aliq-nat        as logical                        no-undo.
def new shared var l-tipo-nota       as logical format "Entrada/Saida" no-undo.
def new shared var de-aliquota-icm like natur-oper.aliquota-icm.
def new shared var r-ped-venda       as rowid.
def new shared var r-pre-fat         as rowid.
def new shared var r-emitente        as rowid.
def new shared var r-estabel         as rowid.
def new shared var r-docum-est       as rowid.

def new shared var r-nota            as rowid.
def new shared var de-cotacao        as decimal format ">>>,>>9.99999999".
def new shared var r-nota-fiscal     as rowid.
def new shared var i-codigo          as integer.
def new shared var i-cont            as integer.
def new shared var de-conv           as decimal format ">>>>9.99".
def new shared var r-item            as rowid.
def new shared var r-natur-oper      as rowid.
def new shared var c-num-nota        as char format "x(16)".
def new shared var c-num-duplic      as char.

def new shared var de-tot-icms-obs      like it-nota-fisc.vl-icms-it.
def new shared var de-tot-icmssubs-obs  like it-nota-fisc.vl-icmsub-it.
def new shared var de-tot-bicmssubs-obs like it-nota-fisc.vl-bsubs-it.
def new shared var de-tot-ipi-dev-obs   like it-nota-fisc.vl-ipi-it.
def new shared var de-tot-ipi-calc      like it-nota-fisc.vl-ipi-it.
def new shared var de-tot-ipi-nota      like it-nota-fisc.vl-ipi-it.
DEF NEW SHARED VAR de-conv-total        AS DECIMAL.

/**********************************
*   definicao de variaveis shared
**********************************/

def new shared var c-ser           like nota-fiscal.serie.
def new shared var c-est           like nota-fiscal.cod-estabel.
def new shared var da-dt-emis      like nota-fiscal.dt-emis-nota.
def new shared var i-nr-nota       like nota-fiscal.nr-nota-fis.
def new shared var i-nota-ini      as integer format ">>>,>>9".
def new shared var i-nota-fim      as integer format "999,999".
def new shared var i-embarq-ini    like nota-fiscal.cdd-embarq.
def new shared var i-embarq-fim    like nota-fiscal.cdd-embarq.
def new shared var l-dt              as logica. 
def new shared var dt-saida          as date format "99/99/9999".
def new shared var hr-saida          as char format "xx:xx:xx".

/**********************************
*   definicao de tabelas globais
**********************************/

def new shared var i-parcela   as integer                             extent 6.
def new shared var i-fatura    as char      format "x(16)"            extent 6.
def new shared var da-venc-dup as date      format "99/99/9999"       extent 6.
def new shared var de-vl-dup   as decimal   format ">>>>>,>>>,>>9.99" extent 6.

/************************************
*   definicao das variaveis locais
************************************/
               
def var l-resposta             as logical format "Sim/Nao" init yes.
def var i-cont4                as integer.
def var i-cont6                as integer.
def var l-tem-ipi              as logical.
def var i                      as integer.
def var i-qt-volumes           as integer extent 5 format ">>>,>>9".
def var i-cont-item            as integer.
def var i-dup                  as integer.
def var l-sub                  as logical.
def var de-aliquota-iss      like it-nota-fisc.aliquota-iss.
def var de-desc              like it-nota-fisc.vl-preori.
def var c-cod-suframa-est    like estabelec.cod-suframa.
def var c-cod-suframa-cli    like emitente.cod-suframa.
def var de-qt-fatur            as decimal format ">>>>,>>9.9999".
def var de-tot-icmssubs      like it-nota-fisc.vl-icmsub-it.
def var de-tot-bicmssubs     like it-nota-fisc.vl-bsubs-it.
def var de-tot-bas-icm       like it-nota-fisc.vl-icms-it.
def var de-tot-bas-iss       like it-nota-fisc.vl-iss-it.
def var de-tot-bas-ipi       like it-nota-fisc.vl-ipi-it.
def var de-tot-icm           like it-nota-fisc.vl-icms-it.
def var de-tot-iss           like it-nota-fisc.vl-iss-it.
def var de-tot-ipi           like it-nota-fisc.vl-ipi-it.
def var de-vl-bipi-it        like it-nota-fisc.vl-bipi-it.
def var de-vl-ipi-it         like it-nota-fisc.vl-ipi-it.
def var l-frete-bipi           as log.
def var i-cep                like nota-fiscal.cep.
def var c-pago                 as character format "x" init " ".
def var c-opcao                as character.
def var c-class-fiscal         as character format {cdp/cd0603.i3}.
def var c-mensagem1            as character format "x(380)".
def var c-mensagem2            as character format "x(152)".
def var c-especie              as character extent 5 format "x(30)".
def var c-desc-prod            as character format "x(42)".
def var c-repres               as character format "x(15)".
def var c-redesp               as character .
def var c-nat                  as character format "x.xx".
def var c-un-fatur             as character format "x(2)".
def var c-tipo-venda           as character extent 6 initial
    ["1 - Laticin.","2 - Corantes","3 - Frigorif.","4 - Export.",
     "5 - Prest.Serv.","6 - Outros"] format "x(13)".
def var r-it-nota              as rowid.
def var i-sit-nota-ini         as integer.
def var i-sit-nota-fim         as integer.
def var c-formato-cfop         as char.
def var l-importacao           as logical no-undo.
def var c-mess                 as character format "x(130)" extent 24 init "  ".
def var c-cgc                  like estabelec.cgc.
def var c-arq-nota             as char format 'x(40)'.
def var c-arquivo              as char format "X(40)".
DEF VAR l-sn                   AS LOG INIT NO.
DEF VAR c-tp-nota              AS CHAR FORMAT "x".
DEF VAR c-nat2                 as char.
DEF VAR c-mensa-ped            AS CHAR.

DEFINE VARIABLE i-qtd-itens       AS INTEGER    NO-UNDO.
DEFINE VARIABLE hShowMsg          AS HANDLE     NO-UNDO.
DEFINE VARIABLE h-acomp           AS HANDLE     NO-UNDO.
DEFINE VARIABLE hdboCertificMovto AS HANDLE     NO-UNDO.
DEFINE VARIABLE raw-param-1       AS RAW        NO-UNDO. 
DEFINE VARIABLE de-val-desc-it    AS DECIMAL    NO-UNDO.
DEFINE VARIABLE de-val-cdo        AS DECIMAL    NO-UNDO.
DEFINE VARIABLE l-nf-compra       AS LOGICAL    NO-UNDO.

DEF NEW SHARED VAR de-cdo-unit        AS DEC EXTENT 100.
DEF NEW SHARED VAR de-funrural-unit   AS DEC EXTENT 100.
DEF NEW SHARED VAR de-total           AS DEC.
DEF NEW SHARED VAR de-vl-unit         AS DEC. /* 310706 */
DEF NEW SHARED VAR de-vl-tot          AS DEC. /* 310706 */
def NEW SHARED var de-tot-cdo         as dec. /* 070806 */
def NEW SHARED var de-tot-funrural    as dec. /* 070806 */
def NEW SHARED var l-nat-oper         AS LOG INITIAL NO. /* 050906 */
def NEW shared var de-imp-umi-armaz as DEC.       /* 041006 */


def NEW SHARED temp-table b1-es-movto-arroz like es-movto-arroz
index prim nr-ticket tp-desconto
INDEX re1005 nr-ticket esp-docto quantidade tp-desconto.


/* 270105 mp */
def var de-val-irrf as dec.
def var de-val-cofins as dec.
def var l-ir-ou-cofins as log.
def VAR c-notas-geradas AS CHAR.
def VAR l-notas-geradas AS log.

def var de-val-funr-1 as dec.
def var de-val-funr-2 as dec.
def var de-val-funr-3 as dec.
def var i-conta-item  as int.



/* ---   Definicao de Stream de saida   --- */
/*DEFINE STREAM arq-erro.*/

/* --- Atualiza Arquivo de Parametros --- */

CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.

CREATE tt-param-2.
ASSIGN 
    tt-param-2.destino         = tt-param.destino        
    tt-param-2.arquivo         = tt-param.arquivo        
    tt-param-2.usuario         = tt-param.usuario        
    tt-param-2.data-exec       = tt-param.data-exec      
    tt-param-2.hora-exec       = tt-param.hora-exec      
    tt-param-2.classifica      = 1     
    tt-param-2.desc-classifica = ""
    tt-param-2.ep-codigo       = '001' /*tt-param.ep-codigo */ 
    tt-param-2.cod-estabel     = tt-param.cod-estabel
    tt-param-2.serie           = tt-param.serie
    tt-param-2.nr-nota-ini     = tt-param.nr-nota-fis-ini    
    tt-param-2.nr-nota-fim     = tt-param.nr-nota-fis-fim    
    tt-param-2.dt-emissao-ini  = date("01/01/1900")
    tt-param-2.dt-emissao-fim  = date("31/12/9999")
    tt-param-2.cod-port-ini    = 0      /*nao esta utilizando*/
    tt-param-2.cod-port-fim    = 99999  /*nao esta utilizando*/
    tt-param-2.nota-fiscal     = yes    /*nao esta utilizando*/
    tt-param-2.boleto          = YES    /*nao esta utilizando*/
    tt-param-2.reimpressao     = YES
    tt-param-2.padrao-banco    = "".
                               
/* ---    Inicializa Programas Persistentes    --- */

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.

/* /*--- Verifica se o DBO ja esta inicializado ---*/            */
/* IF NOT VALID-HANDLE(hdboCertificMovto) OR                     */
/*    hdboCertificMovto:TYPE <> "PROCEDURE":U OR                 */
/*    hdboCertificMovto:FILE-NAME <> "esbo/boes304.p":U THEN     */
/* DO:                                                           */
/*     /*RUN esbo/boes304.p PERSISTENT SET hdboCertificMovto. */ */
/* END.                                                          */
        
/* ---       --- */

{include/i-rpvar.i}

FIND FIRST param-global NO-LOCK NO-ERROR.
FIND FIRST para-ped     NO-LOCK NO-ERROR.

ASSIGN    
    c-empresa  = IF AVAILABLE param-global THEN grupo ELSE ''
    c-arquivo  = substring(string(time,"HH:MM:SS"),1,2)
               + substring(string(time,"HH:MM:SS"),4,2)
               + string(day(today))
               + string(month(today))
               + ".lst"
    c-programa = "es4054"
    c-versao   = "01.00"
    c-revisao  = "01".

{utp/ut-liter.i Emiss∆o_de_Notas_Fiscais * C}
ASSIGN c-titulo-relat = return-value.
{utp/ut-liter.i Faturamento * C}
assign c-sistema = return-value.

{include/i-rpcab.i}
{include/i-rpout.i} 
{include/tt-edit.i}
{include/pi-edit.i}


/* --- Elimina Arquivos tempoararios--- */

EMPTY TEMP-TABLE tt-notas-impressas NO-ERROR.

RUN pi-inicializar IN h-acomp (INPUT c-titulo-relat).

   ASSIGN i-sit-nota-ini = 2
          i-sit-nota-fim = 7
          c-arq-nota     = tt-param.arquivo.

/*IF tt-param.destino = 2 THEN DO:
   IF SUBSTRING(c-arq-nota,LENGTH(c-arq-nota) - 2,3 ) <> "txt" THEN
      c-arq-nota = SUBSTRING(c-arq-nota,1,LENGTH(c-arq-nota) - 3) + "txt".
   ELSE
      c-arq-nota = SUBSTRING(c-arq-nota,1,LENGTH(c-arq-nota) - 3) + "lst".

   OUTPUT TO VALUE(c-arq-nota).
   output close.
end.*/


FIND FIRST tt-raw-digita NO-ERROR.
IF AVAIL tt-raw-digita THEN DO :

   FOR EACH tt-raw-digita:
       CREATE tt-notas-geradas.
       RAW-TRANSFER tt-raw-digita.raw-digita TO tt-notas-geradas.
   END.
   
   FOR EACH tt-notas-geradas,
       EACH nota-fiscal
      WHERE ROWID(nota-fiscal) = tt-notas-geradas.rw-nota-fiscal NO-LOCK :
          
      RUN pi-imprime-nota.

   END.

END.
ELSE DO:      


   nota-fiscal:
   FOR EACH  nota-fiscal USE-INDEX ch-nota
       where nota-fiscal.cod-estabel   = tt-param.cod-estabel
       and   nota-fiscal.serie         = tt-param.serie
       and   nota-fiscal.nr-nota-fis  >= tt-param.nr-nota-fis-ini
       and   nota-fiscal.nr-nota-fis  <= tt-param.nr-nota-fis-fim
       and   nota-fiscal.cdd-embarq  >= 0
       and   nota-fiscal.cdd-embarq  <= 999999999
       and   nota-fiscal.ind-sit-nota >= i-sit-nota-ini 
       and   nota-fiscal.ind-sit-nota <= i-sit-nota-fim no-lock
       break by nota-fiscal.cod-estabel
             by nota-fiscal.serie
             by nota-fiscal.nr-nota-fis:
       
     
       FIND emitente NO-LOCK OF nota-fiscal NO-ERROR.

       FIND natur-oper WHERE
            natur-oper.nat-operacao = nota-fiscal.nat-operacao 
            NO-LOCK NO-ERROR.

       FIND es-param-empres WHERE
            es-param-empres.ep-codigo = param-global.empresa-pri
            NO-LOCK NO-ERROR.

       find FIRST es-ticket no-lock where 
                  es-ticket.nro-docto    = nota-fiscal.nr-nota-fis AND
                  es-ticket.serie        = nota-fiscal.serie       AND
                  es-ticket.cod-produtor = emitente.cod-emitente   NO-ERROR.
       IF AVAIL es-ticket THEN DO:
           FIND FIRST es-contrato-arroz WHERE
                      es-contrato-arroz.nr-contrato = es-ticket.nr-contrato
               NO-LOCK NO-ERROR.
       END. /* 041006 */

       
       /* c-tp-nota ==> OK 0= NF Transferencia Saida
                        OK 1= NF Remessa a Deposito (Consignacao)
                        OK 2= NF Devolucao da Remessa Deposito (Ret Consignacao)
                        OK 3= NF Entrada Compra Direta Arroz
                        OK 4= NF Entrada Compra a Deposito Arroz
                        OK 5= NF Armazenagem com mensagem igual a NF Entrada Compra a Deposito de Arroz /* 041006 */
                               */                   

       
       ASSIGN c-tp-nota = "".

       IF natur-oper.transf and
          nota-fiscal.tipo = 1 THEN
          ASSIGN c-tp-nota = "0".


       IF AVAIL es-ticket               and
          natur-oper.tipo        = 1   AND
          natur-oper.emite-duplic = NO  AND
          natur-oper.terceiros    = YES AND
          natur-oper.tp-oper-terc = 3   and
          emitente.cod-gr-forn    = es-param-empres.gr-produtor and
          int(emitente.natureza)  < 3  THEN
          ASSIGN c-tp-nota = "1".

       IF natur-oper.tipo        = 2   AND
          natur-oper.emite-duplic = NO  AND
          natur-oper.terceiros    = YES AND
          natur-oper.tp-oper-terc = 5   and
          emitente.cod-gr-forn    = es-param-empres.gr-produtor and
          int(emitente.natureza)  < 3  THEN
          ASSIGN c-tp-nota = "2".

       IF AVAIL es-ticket               and
          natur-oper.tipo        = 1   AND
          natur-oper.emite-duplic = YES AND
          natur-oper.terceiros    = NO  AND
          INT(emitente.natureza)  < 3   AND
          emitente.cod-gr-forn    = es-param-empres.gr-produtor then
          ASSIGN c-tp-nota = "3".

       IF NOT AVAIL es-ticket           and
          natur-oper.tipo        = 1   AND
          natur-oper.emite-duplic = YES AND
          natur-oper.terceiros    = NO  AND
          INT(emitente.natureza)  < 3   AND
          emitente.cod-gr-forn    = es-param-empres.gr-produtor then
          ASSIGN c-tp-nota = "4".

       IF AVAIL es-ticket           and
         AVAIL es-contrato-arroz   AND
         (es-contrato-arroz.tipo-contrato = 4 or
          es-contrato-arroz.tipo-contrato = 5) AND
         natur-oper.tipo        = 1   AND
         natur-oper.emite-duplic = NO AND
         natur-oper.terceiros    = YES  AND
         INT(emitente.natureza)  < 3   AND
         emitente.cod-gr-forn    = es-param-empres.gr-produtor then
         ASSIGN c-tp-nota = "5". /* 041006 */


/*        RUN pi-valida-certificado. */
       
       IF RETURN-VALUE <> 'NOK' THEN
          RUN pi-imprime-nota.      
       ELSE 
          UNDO nota-fiscal, NEXT nota-fiscal.
     
        EMPTY TEMP-TABLE tt-certificado-exame.
        EMPTY TEMP-TABLE tt-exame.
        EMPTY TEMP-TABLE tt-certificado.
        
        /*RUN pi-le-certificado.*/
               
        
        /* ---   Efetua impressao das notas fiscais geradas   --- */
        RUN esp/es4054rpi.p (INPUT TABLE tt-nota-fiscal,
                             INPUT TABLE tt-it-nota-fisc,
                             INPUT TABLE tt-certificado-exame,
                             INPUT TABLE tt-exame,
                             INPUT tt-param.destino,
                             INPUT c-arq-nota,
                             INPUT c-tp-nota).
        
        EMPTY TEMP-TABLE tt-nota-fiscal  NO-ERROR.
        EMPTY TEMP-TABLE tt-it-nota-fisc NO-ERROR.

       /* */ 

   END.
       
END.
IF tt-param.destino = 2 THEN DO:
   MESSAGE "Arquivo gerado com sucesso !" SKIP(1)
           "Nome: " + CAPS(c-arq-nota)
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
end.
                      
RUN pi-finalizar IN h-acomp.
/* DELETE PROCEDURE  hdboCertificMovto NO-ERROR. */

{include/i-rpclo.i}


/* ---    Lista Arquivo de Erros   --- */

IF CAN-FIND(FIRST RowErrors
            WHERE RowErrors.ErrorType <> "INTERNAL":U) THEN DO :
          {method/ShowMessage.i1}
          {method/ShowMessage.i2}
END.
RETURN "OK".




PROCEDURE pi-imprime-nota :

    find b-nota-fiscal
         where rowid(b-nota-fiscal) = rowid(nota-fiscal)
         no-error.


     RUN pi-acompanhar IN h-acomp (INPUT "Lendo Nota: " + b-nota-fiscal.nr-nota-fis +
                                         "     Data: " + string(b-nota-fiscal.dt-emis-nota,"99/99/9999")).


    if b-nota-fiscal.dt-cancela <> ? then
       return.




    if  tt-param.data-exec <> ? then
        assign /*b-nota-fiscal.dt-saida = IF tt-param.data-exec < b-nota-fiscal.dt-emis-nota THEN
                                           b-nota-fiscal.dt-emis-nota
                                        ELSE tt-param.data-exec 041006 */
               dt-saida               = b-nota-fiscal.dt-saida
               hr-saida               = string(b-nota-fiscal.hr-atualiza,"999999")
               l-dt                   = NO. 

    find ped-venda
         where ped-venda.nome-abrev = nota-fiscal.nome-ab-cli
         and   ped-venda.nr-pedcli = nota-fiscal.nr-pedcli
         no-lock no-error.

    find estabelec
         where estabelec.cod-estabel = nota-fiscal.cod-estabel
         no-lock no-error.

    find emitente
         where emitente.nome-abrev  = nota-fiscal.nome-ab-cli
         no-lock no-error.

    find pre-fatur use-index ch-embarque
         where pre-fatur.cdd-embarq = nota-fiscal.cdd-embarq
         and   pre-fatur.nome-abrev  = nota-fiscal.nome-ab-cli
         and   pre-fatur.nr-pedcli   = nota-fiscal.nr-pedcli
         and   pre-fatur.nr-resumo   = nota-fiscal.nr-resumo
         no-lock no-error.

    find natur-oper
         where natur-oper.nat-operacao = nota-fiscal.nat-operacao
         no-lock no-error.

    assign r-estabel    = rowid(estabelec)
           r-ped-venda  = rowid(ped-venda)
           r-emitente   = rowid(emitente)
           r-natur-oper = rowid(natur-oper)
           r-pre-fat    = if   avail pre-fatur
                          then rowid(pre-fatur)
                          else ?
           l-tipo-nota  = no.

    /*----------------------------------------------------------------------------*/

    assign de-conv = 1.

    find first cidade-zf where cidade-zf.cidade = nota-fiscal.cidade
                         and   cidade-zf.estado = nota-fiscal.estado
                         no-lock no-error.

    if  avail cidade-zf 
    and dec(substr(natur-oper.char-2,66,5)) > 0 then
        assign de-conv = (100 - dec(substr(natur-oper.char-2,66,5))) / 100.
        /* valor para tratamento de ZFM */
       
    {ftp/ft0515.i1}
    
    /* muda o status da nota fiscal. */
    assign r-nota = rowid(nota-fiscal).
    run "ftp/ft0503a.p".
    
    create tt-notas-impressas.
    assign tt-notas-impressas.r-nota = rowid(nota-fiscal).

/*     /* GERA ETIQUETAS PARA O MODULO DE COLETA DE DADOS */                                    */
/*                                                                                              */
/*     if  avail param-global                                                                   */
/*     and param-global.modulo-cl                                                               */
/*     and (   tt-param.rs-imprime      = 1                                                     */
/*          or nota-fiscal.ind-tip-nota = 2) /* tipo de nota-fiscal Manual */                   */
/*     then do:                                                                                 */
/*         create tt-prog-bc.                                                                   */
/*         assign tt-prog-bc.cod-prog-dtsul        = "es4054"                                   */
/*                tt-prog-bc.cod-versao-integracao = 1                                          */
/*                tt-prog-bc.usuario               = tt-param.usuario                           */
/*                tt-prog-bc.opcao                 = 1.                                         */
/*                                                                                              */
/*         run bcp/bcapi004.p (input-output table tt-prog-bc,                                   */
/*                             input-output table tt-erro).                                     */
/*                                                                                              */
/*         find first tt-prog-bc no-error.                                                      */
/*                                                                                              */
/*         assign  c-arquivo = tt-prog-bc.nome-dir-etiq + "/" + c-arquivo.                      */
/*                                                                                              */
/*         if  return-value = "OK" then do:                                                     */
/*                                                                                              */
/*             {utp/ut-liter.i Gerando_Etiquetas  MRE R}                                        */
/*             run pi-acompanhar in h-acomp (input return-value).                               */
/*                                                                                              */
/*             erro:                                                                            */
/*             do  on stop     undo erro,leave erro                                             */
/*                 on quit     undo erro,leave erro                                             */
/*                 on error    undo erro,leave erro                                             */
/*                 on endkey   undo erro,leave erro:                                            */
/*                                                                                              */
/*                 run value(tt-prog-bc.prog-criacao)(input tt-prog-bc.cd-trans,                */
/*                                                    input rowid(nota-fiscal),                 */
/*                                                    input-output table tt-erro) no-error.     */
/*                                                                                              */
/*                 if  ERROR-STATUS:ERROR                                                       */
/*                 or  (    error-status:get-number(1) <> 138                                   */
/*                      and error-status:num-messages  <> 0)                                    */
/*                 then do:                                                                     */
/*                     output stream arq-erro to value(c-arquivo) append.                       */
/*                                                                                              */
/*                     {utp/ut-liter.i Ocorreu_na_Geraá∆o_de_Etiquetas_-_Progress MRE R}        */
/*                     put stream arq-erro "***" return-value skip.                             */
/*                     {utp/ut-liter.i Programa * R}                                            */
/*                     put stream arq-erro error-status:get-message(1) skip.                    */
/*                     put stream arq-erro return-value ": " tt-prog-bc.prog-criacao skip.      */
/*                     put stream arq-erro nota-fiscal.serie                           at 1.    */
/*                     put stream arq-erro nota-fiscal.nr-nota-fis                     at 7.    */
/*                     put stream arq-erro nota-fiscal.cod-estabel                     at 24.   */
/*                                                                                              */
/*                     output stream arq-erro close.                                            */
/*                 end.                                                                         */
/*                                                                                              */
/*                 if  return-value = "NOK" then do:                                            */
/*                     find first tt-erro no-error.                                             */
/*                     if  avail tt-erro                                                        */
/*                     then do:                                                                 */
/*                         output stream arq-erro to value(c-arquivo) append.                   */
/*                                                                                              */
/*                         {utp/ut-liter.i Ocorreu_na_Geraá∆o_de_Etiquetas MRE R}               */
/*                         put stream arq-erro "***" return-value skip.                         */
/*                         for each tt-erro:                                                    */
/*                             put stream arq-erro skip tt-erro.cd-erro " - " tt-erro.mensagem. */
/*                         end.                                                                 */
/*                         put stream arq-erro skip.                                            */
/*                                                                                              */
/*                         output stream arq-erro close.                                        */
/*                     end.                                                                     */
/*                 end.                                                                         */
/*             end.                                                                             */
/*         end.                                                                                 */
/*         else do:                                                                             */
/*             /**** caso tenha integraá∆o com o coleta e ocorreu erros ***/                    */
/*             find first tt-erro no-error.                                                     */
/*             if  avail tt-erro then do:                                                       */
/*                 output stream arq-erro to value(c-arquivo) append.                           */
/*                                                                                              */
/*                 {utp/ut-liter.i Ocorreu_na_Geraá∆o_de_Etiquetas MRE R}                       */
/*                 put stream arq-erro "***" return-value skip.                                 */
/*                 for each tt-erro:                                                            */
/*                     put  stream arq-erro skip tt-erro.cd-erro " - " tt-erro.mensagem.        */
/*                 end.                                                                         */
/*                 put stream arq-erro skip.                                                    */
/*                 output stream arq-erro close.                                                */
/*             end.                                                                             */
/*         end.                                                                                 */
/*     end.                                                                                     */
    
END PROCEDURE.




/*PROCEDURE pi-valida-certificado :
     
    DEFINE VARIABLE l-erro        AS LOGICAL    NO-UNDO INIT FALSE.

    EMPTY TEMP-TABLE RowErrorsAux NO-ERROR.
    EMPTY TEMP-TABLE tt-erro2     NO-ERROR.

    /* --- Nota Fiscal ja foi emitida --- */
    
    IF nota-fiscal.ind-sit-nota > 1 THEN DO:
      
        
        FIND FIRST es-certific-movto NO-LOCK
             WHERE es-certific-movto.cod-estabel = nota-fiscal.cod-estabel
               AND es-certific-movto.serie       = nota-fiscal.serie
               AND es-certific-movto.nr-nota-fis = nota-fiscal.nr-nota-fis
               NO-ERROR.
        IF AVAIL es-certific-movto THEN
           RETURN 'OK'.

    END.

    /* --- Verificac certificado para cada item da nota fiscal --- */

    FOR EACH it-nota-fisc OF nota-fiscal NO-LOCK :
        
        
        RUN setConstraintItNotaFisc IN hdboCertificMovto (INPUT ROWID(it-nota-fisc)).
        RUN getItNotaFisc           IN hdboCertificMovto.
        RUN mtdoCriaMovto           IN hdboCertificMovto.
        RUN RetornaErros            IN hdboCertificMovto (OUTPUT TABLE  RowErrorsAux).
                    
        FOR EACH RowErrorsAux :

            CREATE RowErrors.
            BUFFER-COPY RowErrorsAux TO RowErrors.         
            IF RowErrorsAux.ErrorSubType = "ERROR":U  THEN
               l-erro = TRUE.

        END.

    END. /* FOR EACH tt-it-nota-fisc */
    
    IF l-erro THEN 
       RETURN 'NOK':U.
    
    
    RETURN 'OK':U.
             
END PROCEDURE.




PROCEDURE pi-le-certificado:
    
    for each tt-certificado:
       delete tt-certificado.    
    end.
    
    for each tt-certificado-exame:
       DELETE tt-certificado-exame.
    END.
    
    
    FOR EACH tt-it-nota-fisc 
       BREAK BY tt-it-nota-fisc.cod-estabel
             BY tt-it-nota-fisc.serie
             BY tt-it-nota-fisc.nr-nota-fis
             BY tt-it-nota-fisc.nr-seq-fat
             BY tt-it-nota-fisc.it-codigo :

       FOR EACH es-certific-movto NO-LOCK 
          WHERE es-certific-movto.cod-estabel = tt-it-nota-fisc.cod-estabel
            AND es-certific-movto.serie       = tt-it-nota-fisc.serie
            AND es-certific-movto.nr-nota-fis = tt-it-nota-fisc.nr-nota-fis
            AND es-certific-movto.nr-seq-fat  = tt-it-nota-fisc.nr-seq-fat
            AND es-certific-movto.item-nota   = tt-it-nota-fisc.it-codigo 
            AND es-certific-movto.tipo        = 2 :
          
                  /* Buscou certificado */
           FIND tt-certificado NO-LOCK WHERE 
                tt-certificado.cod-uf-emis         = es-certific-movto.cod-uf-emis        AND
                tt-certificado.cod-serie           = es-certific-movto.cod-serie          AND
                tt-certificado.cod-certific-class  = es-certific-movto.cod-certific-class AND
                tt-certificado.cod-estabel         = es-certific-movto.cod-estabel        AND
                tt-certificado.serie               = es-certific-movto.serie              AND
                tt-certificado.nr-nota-fis         = es-certific-movto.nr-nota-fis        AND
                tt-certificado.nr-seq-fat          = es-certific-movto.nr-seq-fat         AND
                tt-certificado.item-nota           = es-certific-movto.item-nota          NO-ERROR.                 
           IF NOT AVAIL tt-certificado THEN DO:
             
             FIND es-certificado NO-LOCK WHERE 
                  es-certificado.cod-uf-emis         = es-certific-movto.cod-uf-emis        AND
                  es-certificado.cod-serie           = es-certific-movto.cod-serie          AND
                  es-certificado.cod-certific-class  = es-certific-movto.cod-certific-class NO-ERROR.
      
             FIND familia WHERE
                  familia.fm-codigo = es-certificado.fm-codigo NO-LOCK NO-ERROR.
      

             RUN pi-acompanhar IN h-acomp (INPUT "Lendo Nota: " + tt-it-nota-fisc.nr-nota-fis + 
                                                  "     Certificado: " + es-certificado.cod-certific-class).
     
                   
             /* --- Cria Arquivo de Certificados temporarios --- */
             CREATE tt-certificado.
             BUFFER-COPY es-certific-movto TO tt-certificado.
             
             /* ---  acha os exames do certificado --- */
            
             FOR EACH es-certific-exame NO-LOCK
                WHERE es-certific-exame.cod-uf-emis         = tt-certificado.cod-uf-emis         
                  AND es-certific-exame.cod-serie           = tt-certificado.cod-serie           
                  AND es-certific-exame.cod-certific-class  = tt-certificado.cod-certific-class,
                FIRST es-exame OF es-certific-exame WHERE 
                      es-exame.log-nota-fisc NO-LOCK:
                 
                /* --- cria tt-certificado-exame --- */
                FIND FIRST tt-certificado-exame 
                     WHERE tt-certificado-exame.cod-uf-emis         = tt-certificado.cod-uf-emis         
                       AND tt-certificado-exame.cod-serie           = tt-certificado.cod-serie           
                       AND tt-certificado-exame.cod-certific-class  = tt-certificado.cod-certific-class
                       AND tt-certificado-exame.cod-estabel         = tt-certificado.cod-estabel 
                       AND tt-certificado-exame.serie               = tt-certificado.serie              
                       AND tt-certificado-exame.nr-nota-fis         = tt-certificado.nr-nota-fis        
                       AND tt-certificado-exame.nr-seq-fat          = tt-certificado.nr-seq-fat         
                       AND tt-certificado-exame.item-nota           = tt-certificado.item-nota          
                       AND tt-certificado-exame.cdn-exame           = es-exame.cdn-exame 
                       NO-ERROR.
                IF NOT AVAIL tt-certificado-exame THEN DO:
                
                   CREATE tt-certificado-exame.
                   BUFFER-COPY tt-certificado    TO tt-certificado-exame.
                   BUFFER-COPY es-certificado    TO tt-certificado-exame.
                   BUFFER-COPY es-exame          TO tt-certificado-exame.
                   BUFFER-COPY es-certific-exame TO tt-certificado-exame.   
                   tt-certificado-exame.desc-familia = familia.descricao.
                          
                END.
                               
                /* cria tt-exame */
                FIND FIRST tt-exame WHERE 
                           tt-exame.cdn-exame   = es-exame.cdn-exame          AND 
                           tt-exame.cod-estabel = tt-it-nota-fisc.cod-estabel AND 
                           tt-exame.serie       = tt-it-nota-fisc.serie       AND       
                           tt-exame.nr-nota-fis = tt-it-nota-fisc.nr-nota-fis NO-ERROR.
                IF NOT AVAIL tt-exame THEN DO:
                   CREATE tt-exame.
                   BUFFER-COPY es-exame TO tt-exame.
                   ASSIGN
                       tt-exame.cod-estabel = tt-it-nota-fisc.cod-estabel
                       tt-exame.serie       = tt-it-nota-fisc.serie           
                       tt-exame.nr-nota-fis = tt-it-nota-fisc.nr-nota-fis.
                END.
        
             END. /* certificado-exame */

           END.
         
       END.

       IF LAST-OF (tt-it-nota-fisc.nr-nota-fis) THEN DO:
           

          /* --- Equaliza Exames dos Certificados --- */
                                                        
          FOR EACH tt-exame WHERE
                  tt-exame.cod-estabel         = tt-it-nota-fisc.cod-estabel AND 
                  tt-exame.serie               = tt-it-nota-fisc.serie       AND       
                  tt-exame.nr-nota-fis         = tt-it-nota-fisc.nr-nota-fis NO-LOCK,                              
             EACH tt-certificado WHERE 
                  tt-certificado.cod-estabel   = tt-it-nota-fisc.cod-estabel AND 
                  tt-certificado.serie         = tt-it-nota-fisc.serie       AND 
                  tt-certificado.nr-nota-fis   = tt-it-nota-fisc.nr-nota-fis NO-LOCK:
            
             FIND es-certificado NO-LOCK WHERE 
                  es-certificado.cod-uf-emis         = tt-certificado.cod-uf-emis        AND
                  es-certificado.cod-serie           = tt-certificado.cod-serie          AND
                  es-certificado.cod-certific-class  = tt-certificado.cod-certific-class NO-ERROR.

             FIND FIRST tt-certificado-exame 
                WHERE tt-certificado-exame.cod-uf-emis         = tt-certificado.cod-uf-emis         
                  AND tt-certificado-exame.cod-serie           = tt-certificado.cod-serie           
                  AND tt-certificado-exame.cod-certific-class  = tt-certificado.cod-certific-class
                  AND tt-certificado-exame.cod-estabel         = tt-certificado.cod-estabel 
                  AND tt-certificado-exame.serie               = tt-certificado.serie              
                  AND tt-certificado-exame.nr-nota-fis         = tt-certificado.nr-nota-fis        
                  AND tt-certificado-exame.item-nota           = tt-certificado.item-nota
                  AND tt-certificado-exame.cdn-exame           = tt-exame.cdn-exame NO-ERROR.    
            
            IF NOT AVAIL tt-certificado-exame THEN DO:
               CREATE tt-certificado-exame.
               BUFFER-COPY tt-certificado TO tt-certificado-exame.   
               BUFFER-COPY es-certificado TO tt-certificado-exame.   
               BUFFER-COPY tt-exame       TO tt-certificado-exame.
               tt-certificado-exame.des-resultado = '.'.     
              
            END.
                               
          END.  

       END.

    END.      
END PROCEDURE.
  */
 

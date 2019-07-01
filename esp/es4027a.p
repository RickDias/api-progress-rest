/******************************************************************************
** CAMIL
** PROGRAMA : ES4027A
** DESCRICAO: COMPRA ITEM CONSIGNADO
** AUTOR    : DATASUL METROPOLITANA
** DATA     : OUTUBRO DE 2002
** VERSAO   : 2.04.00.000 - Marcos Hoff - Versao Inicial
** ECCB - Senar 21062010
** ECCB - Beto solicitou parcela do senar para 13  - 07122010
** ECCB - Joca/Waldemar tratamento eccb - PIS/COFINS- 28032011
** ECCB - colocando bases de aliquotas icm e ipi - 26/10/2012
** ECCB - complemento de preáo tratado com senar- pis cofins - 23/11/2012
** ECCB - CEI 13/04/2013
** ECCB - ajuste CEI 23/05/2013
** ECCB - complemento preáo Itapecuru 11/10/2013
** ECCB - Interpretaá∆o impostos fr/cei/cdo/senar por estabelecimento 07/12/2015
**
** rde - kraft - ago/2016 - migraá∆o campos totvs 12
** Ramon Kraft - 28/08/2017 - Atualizaá∆o das alteraá‰es da vers∆o EMS2
**
*******************************************************************************/
{include/i-prgvrs.i ES4027A 2.09.00.005 } /*** "019005" ***/

DEFINE INPUT PARAMETER num-nota AS CHAR FORMAT "x(10)".
DEFINE INPUT PARAMETER num-serie AS CHAR FORMAT "x(10)".

{utp/ut-glob.i}
{esp/esapi003.i}  /*** TEMP-TABLES ESAPI003 ***/

/* vars da narrativa de complemento de preáos */
DEFINE VARIABLE sentence1 AS CHARACTER INITIAL "Motivo do Complemento: ".
DEFINE VARIABLE found1 AS INTEGER.
DEFINE VARIABLE sentence2 AS CHARACTER INITIAL "Contrato: ".
DEFINE VARIABLE found2 AS INTEGER.
DEFINE VARIABLE sentence3 AS CHARACTER INITIAL "Corretor: ".
DEFINE VARIABLE found3 AS INTEGER.

DEF VAR l-tem-cei AS LOG INITIAL NO. /* cei 18/04/2013 */
DEF VAR i-num-cei AS INT INITIAL 1. /*  cei 29/04/2013 */

DEF VAR de-unit-item AS DEC.
DEF VAR de-unit-item-4 AS DEC DECIMALS 4.

DEF VAR de-ajuste-senar-cei AS DEC INITIAL 0.
DEF VAR de-diferenca-cei    AS DEC INITIAL 0.
DEF VAR de-valor-merc       AS DEC INITIAL 0.
 
DEF VAR de-val-ajuste-fr AS DEC INITIAL 0. /* 22/05/2013 */

DEF VAR c-modelo AS CHAR. /* 24/02/2012 */

DEF VAR c-serie    AS CHAR.
DEF VAR c-serieE   AS CHAR.
DEF VAR ct-initial AS char.
DEF VAR sc-initial AS char.
DEF VAR de-indice  AS DEC.
DEF VAR de-qtd-estrut AS DEC.
DEF VAR de-tot-peso AS DEC.
DEF VAR de-quant AS DEC.
DEF VAR c-un AS char.
DEF VAR de-soma-mp AS DEC INITIAL 0.
DEF VAR l-compl-preco AS LOG INITIAL NO.

DEF NEW GLOBAL SHARED VAR gr-pedido-compr AS ROWID NO-UNDO.
DEF VAR i-seq AS INT INITIAL 9.
DEF BUFFER b2nota-fiscal      FOR nota-fiscal.
DEF BUFFER b-natur-oper       FOR natur-oper.
DEF BUFFER b-saldo-estoq      FOR saldo-estoq.
DEF BUFFER bf-es-saldo-arroz  FOR es-saldo-arroz.
DEF BUFFER b2-emitente        FOR emitente.
DEF BUFFER b-ordem-compra     FOR ordem-compra.
DEF BUFFER b-es-param-empresa FOR es-param-empresa.
DEF BUFFER b-es-mp232-emit    FOR es-mp232-emit.
DEF BUFFER b2-natur-oper      FOR natur-oper.

DEFINE VARIABLE vl-saldo     AS DECIMAL    NO-UNDO.
DEFINE VARIABLE vl-bloqu     AS DECIMAL    NO-UNDO.
DEFINE VARIABLE i-contrato   AS INTEGER    NO-UNDO.

DEF VAR de-total               AS DEC NO-UNDO.
DEF VAR de-cdo-unit            AS DEC NO-UNDO.
DEF VAR de-funrural-unit       AS DEC NO-UNDO.
DEF VAR de-valor-dp            AS DEC NO-UNDO.
DEF VAR de-senar-unit          AS DEC NO-UNDO.
DEF VAR de-irrf-unit           AS DEC NO-UNDO. 
DEF VAR de-cofins-unit         AS DEC NO-UNDO. 

/****erika****/
DEF VAR de-dif-dupl-nota     AS DEC NO-UNDO. /* 12/11/12 */
DEF VAR de-total-sem-imp     AS DEC NO-UNDO. /* 09/11/12 */
DEF VAR de-total-t           AS DEC NO-UNDO.
DEF VAR de-total-t-sem-imp   AS DEC NO-UNDO. /* 09/11/12 */
DEF VAR de-cdo-unit-t        AS DEC NO-UNDO.
DEF VAR de-funrural-unit-t   AS DEC NO-UNDO.
DEF VAR de-senar-unit-t      AS DEC NO-UNDO.
DEF VAR de-irrf-unit-t       AS DEC NO-UNDO. 
DEF VAR de-cofins-unit-t     AS DEC NO-UNDO. 

DEF VAR de-cdo-doc           AS DEC NO-UNDO.
DEF VAR de-funrural-doc      AS DEC NO-UNDO.
DEF VAR de-senar-doc         AS DEC NO-UNDO.
DEF VAR de-irrf-doc          AS DEC NO-UNDO. 
DEF VAR de-cofins-doc        AS DEC NO-UNDO. 

DEF VAR l-prim               AS LOG INITIAL YES.
DEF VAR l-prim-cdo           AS LOG INITIAL YES.
DEF VAR l-prim-fun           AS LOG INITIAL YES.
DEF VAR l-prim-irrf          AS LOG INITIAL YES. 
DEF VAR l-prim-cofins        AS LOG INITIAL YES. 

DEF VAR de-qt-solic          AS DEC.
def var i-conta-msg          as integer              no-undo init 0.

def var cod-versao-integracao as integer format "999" no-undo. 
def new global shared var c-RE0301-usuario    like param-re.usuario no-undo.
def new global shared var r-RE0301-documento as rowid     no-undo. 
def new global shared var c-RE0301-origem     as character no-undo.

def new shared var de-agreg-aca as de no-undo.
def new shared var de-mat-env-a as de no-undo.
def new shared var l-obrig as logical format "Sim/Nao" init no.
def new shared var l-erro-lote as logical no-undo.
def new shared var l-erro-fc   as logical no-undo init no.
def new shared var l-sem-valor as logical no-undo.
def new shared var l-sem-cotacao as logical no-undo.

def new shared var c-ult-ver   as character format "x(8)" no-undo.

def var l-industria as logical no-undo.
def var l-devolucao as logical no-undo.
def var l-comercio  as logical no-undo.
def var l-servicos  as logical no-undo.
def var l-remessa   as logical no-undo.
def var l-retorno   as logical no-undo.
def var l-entrada   as logical no-undo.
def var l-transf    as logical no-undo.
def var l-ent-cons  as logical no-undo.
def var l-sai-cons  as logical no-undo.

def var de-aux-1 as decimal no-undo.
def var de-aux-2 as decimal no-undo.
def var de-tot-desp as decimal no-undo.
def var de-desp as decimal no-undo.
def var de-vl-frete as decimal no-undo.

def var l-resposta  as logical no-undo.
def var l-peso      as logical no-undo.
def var l-notas     as logical no-undo.
def var l-descontos as logical no-undo.
def var l-valor     as logical no-undo.
def var l-cif-fob   as logical no-undo.
def var l-cliente   as logical format "CIF/FOB" no-undo.
def var c-natureza   like docum-est.nat-operacao no-undo.
def var rw-registro  as rowid no-undo.

DEF VAR i-nr-docto    AS INT FORMAT "9999999" INIT 0 NO-UNDO.
def var c-docto-aux like docum-est.nro-docto.

/*********/
 
DEF VAR da-vencimento        AS DATE.
DEF VAR da-vencto-dp        AS DATE. /* 171005 */
DEF VAR l-ok                AS LOG INITIAL NO. /* 171005 */

DEF VAR de-unitario          AS DEC DECIMALS 5. /* DE-UNITARIO DECIMALS DE 5 - 08/11/12 ERIKA */
DEF VAR i-parcela-dp         AS INT NO-UNDO.


DEFINE TEMP-TABLE tt-dupli
    field dt-vencimen         as date
    field tp-despesa          as integer
    field vl-apagar           as decimal.

/*erika*/
define temp-table tt_erros_modulo no-undo
       field identifi-msg                   as char format "x(60)"
       field num-sequencia-erro             as int  format "999"
       field cod-erro                       as int  format "99999"
       field des-erro                       as char format "x(60)".
     

FOR EACH tt-docum-est:
    DELETE tt-docum-est.
END.
FOR EACH tt-item-doc-est:
    DELETE tt-item-doc-est.
END.
FOR EACH tt-rat-lote:
    DELETE tt-rat-lote.
END.
FOR EACH  tt_erros_modulo:
    DELETE tt_erros_modulo.
END.  

FIND FIRST param-global NO-LOCK NO-ERROR.
IF  NOT AVAIL param-global THEN
    RETURN.

FIND pedido-compr WHERE ROWID(pedido-compr) = gr-pedido-compr NO-ERROR.
IF  NOT AVAIL pedido-compr THEN RETURN.


IF pedido-compr.log-2 = YES THEN DO:
    MESSAGE "Compra j† efetuada, favor verificar" VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
END.

IF pedido-compr.Comentario BEGINS "### Pedido de Complemento de Preáo ###" THEN ASSIGN l-compl-preco = YES.


/**********************************************************
** eccb - PIS/COFINS- 28032011 
** verificando as naturezas se pessoa f°sica 
** ou jur°dica para interpretaá∆o
** PF        PJ
** 1101DC - 1101SB
** 1101CT - 1101CM
** 1101OO - 1101SA
** 1101AS - 1101SS
** emitente.natureza = 1 F°sica / >= 2 Jur°dica/Estrangeira
***********************************************************/

FIND emitente WHERE emitente.cod-emitente = pedido-compr.cod-emitente NO-LOCK NO-ERROR. /* PIS/COFINS- 28032011 */

FIND es-param-estab WHERE es-param-estab.cod-estabel = pedido-compr.cod-estabel NO-LOCK NO-ERROR.

IF l-compl-preco = NO THEN do:
    
    /* eccb - PIS/COFINS- 28032011 */
    IF emitente.natureza = 1 THEN
       FIND FIRST natur-oper WHERE natur-oper.nat-operacao BEGINS /*"1101oo"*/ "1101dc" NO-LOCK NO-ERROR.
    IF emitente.natureza >= 2 THEN
       FIND FIRST natur-oper WHERE natur-oper.nat-operacao BEGINS /*"1101oo" "1101dc"*/ "1101sb" NO-LOCK NO-ERROR.

    /* sÇrie de entrada */
    IF natur-oper.tipo = 1 /* entrada */ THEN ASSIGN c-serieE = string(es-param-estab.u-int-1).
                           /* sa°da   */ ELSE ASSIGN c-serieE = string(es-param-estab.u-int-2).
                        
END.
ELSE DO: 

  /* eccb - PIS/COFINS- 28032011 */
    IF emitente.natureza = 1 THEN
       FIND FIRST natur-oper WHERE natur-oper.nat-operacao BEGINS "1101ct" NO-LOCK NO-ERROR.
    IF emitente.natureza >= 2 THEN
       FIND FIRST natur-oper WHERE natur-oper.nat-operacao BEGINS "1101cm" NO-LOCK NO-ERROR.

  /* sÇrie de entrada */
  IF natur-oper.tipo = 1 /* entrada */ THEN ASSIGN c-serieE = string(es-param-estab.u-int-1).
                           /* sa°da   */ ELSE ASSIGN c-serieE = string(es-param-estab.u-int-2).                         
END.


FIND FIRST b-natur-oper WHERE b-natur-oper.nat-operacao BEGINS "5949dc" OR  b-natur-oper.nat-operacao BEGINS "5949sb" NO-LOCK NO-ERROR.
IF b-natur-oper.tipo = 1 /* entrada */ THEN ASSIGN c-serie = string(es-param-estab.u-int-1).
                         /* sa°da   */ ELSE ASSIGN c-serie = string(es-param-estab.u-int-2).

/* 2355743 */
FIND FIRST ext-natur-oper-cfop NO-LOCK
     WHERE ext-natur-oper-cfop.dev-liq-deposito-pf = YES
        OR ext-natur-oper-cfop.dev-liq-deposito-pj = YES NO-ERROR.

IF AVAIL ext-natur-oper-cfop THEN DO:

    FIND FIRST b2-natur-oper NO-LOCK
         WHERE b2-natur-oper.nat-operacao = ext-natur-oper-cfop.nat-operacao NO-ERROR.

    IF AVAIL b2-natur-oper THEN DO:

        IF b2-natur-oper.tipo = 1 /* entrada */ THEN ASSIGN c-serie = string(es-param-estab.u-int-1).
                                  /* sa°da   */ ELSE ASSIGN c-serie = string(es-param-estab.u-int-2).
    END.
END.
                   
FIND FIRST b2nota-fiscal
   WHERE b2nota-fiscal.cod-estabel = pedido-compr.cod-estabel                    
     AND b2nota-fiscal.serie       = string(es-param-estab.u-int-2) /* sa°da */  
     AND b2nota-fiscal.nr-nota-fis = SUBSTR(pedido-compr.c-observacao[3],51,7) NO-LOCK USE-INDEX ch-nota NO-ERROR.


IF AVAIL b2nota-fiscal AND
   SUBSTR(b2nota-fiscal.observ-nota,1900,7) = "SECAGEM" THEN DO:

    /* eccb - PIS/COFINS- 28032011 */
    IF emitente.natureza = 1 THEN
       FIND FIRST natur-oper WHERE natur-oper.nat-operacao BEGINS "1101as" NO-LOCK NO-ERROR.
    IF emitente.natureza >= 2 THEN
       FIND FIRST natur-oper WHERE natur-oper.nat-operacao BEGINS "1101ss" NO-LOCK NO-ERROR.
    

END.


FIND es-ticket WHERE es-ticket.nr-ticket = INT(SUBSTR(pedido-compr.c-observacao[2],20,9))NO-LOCK NO-ERROR.

/* erika */
FOR EACH ordem-compra
    WHERE ordem-compra.num-pedido = pedido-compr.num-pedido NO-LOCK :

    FIND FIRST cotacao-item OF ordem-compra NO-LOCK NO-ERROR.
    ASSIGN de-unitario  = IF cotacao-item.preco-fornec > 0 THEN cotacao-item.preco-fornec ELSE cotacao-item.dec-1. /* inverti 09/11/12 */



    ASSIGN de-total-t  = de-total-t + ROUND(ordem-compra.qt-solic * de-unitario,2)
           de-total-t-sem-imp  = de-total-t-sem-imp + ROUND(ordem-compra.qt-solic * cotacao-item.dec-1,2)
           de-qt-solic = de-qt-solic + ordem-compra.qt-solic
           i-contrato  = ordem-compr.int-1.


    /* funrural total */
    FIND FIRST cotacao-item OF ordem-compra NO-LOCK NO-ERROR.
    
    ASSIGN i-contrato = ordem-compr.int-1.        
    
    FIND emitente WHERE emitente.cod-emitente = pedido-compr.cod-emitente NO-LOCK NO-ERROR.

    FIND es-param-empresa NO-LOCK WHERE es-param-empresa.ep-codigo = i-ep-codigo-usuario NO-ERROR.
    
    FIND es-item NO-LOCK WHERE es-item.it-codigo = ordem-compra.it-codigo NO-ERROR.
    
    
    /* Compra a Deposito */
    ASSIGN de-cdo-unit      = 0
           de-funrural-unit = 0
           de-senar-unit    = 0
           de-irrf-unit     = 0
           de-cofins-unit   = 0.
    


    ASSIGN de-unitario      = IF cotacao-item.preco-fornec > 0 THEN cotacao-item.preco-fornec ELSE cotacao-item.dec-1 /* inverti 09/11/12 */
           de-total         = ROUND(ordem-compra.qt-solic * de-unitario,2)
           de-total-sem-imp = ROUND(ordem-compra.qt-solic * cotacao-item.dec-1,2). /* erika 09/11/12 para calcular sem impostos, pelo unit†rio dig na tela es4019 */
    
    IF l-compl-preco = NO        AND 
       es-param-estab.l-cdo      AND
       es-item.tem-cdo           AND
       /*emitente.estado    = "RS" AND 07/12/2015 retirei porque com a interpretaá∆o de l-cdo n∆o precisa mais */
      (emitente.natureza  = 1    OR emitente.natureza = 4) THEN DO:
          ASSIGN de-cdo-unit  = ROUND(es-param-empresa.vl-cdo * (ordem-compra.qt-solic / es-item.unidade-saco),2) /* alt round de 2 para 1  -04/11/12- voltei 12/11/12 */
                 de-cdo-doc   = de-cdo-doc + de-cdo-unit.
    END. /* cdo */

/*     MESSAGE es-item.tem-funrural es-item.it-codigo SKIP               */
/*         es-param-empresa.vl-funrural-1 es-param-empresa.vl-funrural-2 */
/*         es-param-empresa.vl-funrural-3  de-total-sem-imp              */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK.                            */ 


    IF AVAIL es-item AND 
       es-param-estab.l-fr     AND
       es-item.tem-funrural    AND 
      (emitente.natureza  = 1  OR emitente.natureza = 4) THEN DO:
        ASSIGN de-funrural-unit = ROUND((de-total-sem-imp - de-cdo-unit) *
                                              ((es-param-empresa.vl-funrural-1 +
                                                es-param-empresa.vl-funrural-2 +
                                                es-param-empresa.vl-funrural-3) / 100),2) /* alterado 04/18/2017 */.               

        ASSIGN de-funrural-doc = de-funrural-doc + de-funrural-unit.


/*         MESSAGE "senar de-total-sem-imp /((100 - (es-param-empresa.vl-senar)) / 100)) - de-total-sem-imp,2)" */
/*             round((de-total-sem-imp /((100 - (es-param-empresa.vl-senar)) / 100)) - de-total-sem-imp,2) SKIP */
/*             "tot sem imp" de-total-sem-imp                                                                   */
/*             "param val senar" es-param-empresa.vl-senar                                                      */
/*             "tot sem imp" de-total-sem-imp                                                                   */
/*             VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                               */
    END.

    IF AVAIL es-item AND 
       es-param-estab.l-senar  AND
       (emitente.natureza  = 1  OR emitente.natureza = 4) THEN DO:
        ASSIGN de-senar-unit = ROUND(((de-total-sem-imp - de-cdo-unit) * (es-param-empresa.vl-senar / 100)),2). /* alterado 04/18/2017 */

        ASSIGN de-senar-doc    = de-senar-doc + de-senar-unit.

        /* para criar tabela de controle de ajuste do Senar 23/05/2013 - cei arrendondamento DP com sefaz */
        ASSIGN de-ajuste-senar-cei = de-senar-unit.



/*         MESSAGE "senar de-total-sem-imp /((100 - (es-param-empresa.vl-senar)) / 100)) - de-total-sem-imp,2)" */
/*             round((de-total-sem-imp /((100 - (es-param-empresa.vl-senar)) / 100)) - de-total-sem-imp,2) SKIP */
/*             "tot sem imp" de-total-sem-imp                                                                   */
/*             "param val senar" es-param-empresa.vl-senar                                                      */
/*             "tot sem imp" de-total-sem-imp                                                                   */
/*             VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                               */
    END.
                                     
    /* Medida Provis¢ria 232 - 160105 */
    ASSIGN de-soma-mp = 0.
    
    IF l-compl-preco = NO THEN do:
        FIND nota-fiscal NO-LOCK
        WHERE nota-fiscal.cod-estabel = pedido-compr.cod-estabel
          AND nota-fiscal.nr-nota-fis = substr(pedido-compr.c-observacao[3],51,07)
          AND nota-fiscal.serie = c-serie NO-ERROR.    
    END. /* <> compl-preco */

    FIND FIRST b-es-param-empresa WHERE b-es-param-empresa.ep-codigo = i-ep-codigo-usuario NO-LOCK NO-ERROR.
    IF b-es-param-empresa.mp-ativa THEN DO:                

        ASSIGN da-vencimento = date(MONTH(TODAY),01,YEAR(TODAY)) + 32
               da-vencimento = date(MONTH(da-vencimento),15,YEAR(da-vencimento)).
        RUN pi-cria-mp232. /* pra qq caso cria */

        FIND FIRST b-es-mp232-emit WHERE
                   b-es-mp232-emit.cod-estabel  = pedido-compr.cod-estabel  AND
                   b-es-mp232-emit.cod-emitente = pedido-compr.cod-emitente AND
                   b-es-mp232-emit.mes          = STRING(MONTH(es-param-empres.u-date-1))
                   EXCLUSIVE-LOCK NO-ERROR.
        IF AVAIL b-es-mp232-emit THEN DO:
          FOR EACH b-es-mp232-emit WHERE
                   b-es-mp232-emit.cod-estabel  = pedido-compr.cod-estabel  AND
                   b-es-mp232-emit.cod-emitente = pedido-compr.cod-emitente AND
                   b-es-mp232-emit.mes          = STRING(MONTH(es-param-empres.u-date-1))
            NO-LOCK.
              IF b-es-mp232-emit.gerou = NO THEN
                ASSIGN de-soma-mp = de-soma-mp + de-total.
          END. /* for each */        
        END. /* existe acumulado */

        /* pessoa f°sica */
        IF (emitente.natureza = 1 OR emitente.natureza = 4) THEN DO:
            
            IF de-soma-mp >= b-es-param-empresa.val-min-pf THEN DO:
                                                      
                ASSIGN de-irrf-unit = ROUND(de-soma-mp * (es-param-empresa.perc-irrf / 100),2). /* alt round de 2 para 1  -04/11/12- voltei 12/11/12 */
                ASSIGN de-irrf-doc = de-irrf-doc + de-irrf-unit.
                
                IF de-total < b-es-param-empresa.val-min-pf THEN DO:
                    /* coloca para os outros registros que j† existiam no arquivo a nota que est† contendo o acumulado */
                    FOR EACH b-es-mp232-emit WHERE
                       b-es-mp232-emit.cod-estabel  = pedido-compr.cod-estabel  AND
                       b-es-mp232-emit.cod-emitente = pedido-compr.cod-emitente AND
                       b-es-mp232-emit.mes          = STRING(MONTH(es-param-empres.u-date-1))
                        EXCLUSIVE-LOCK.
    
                        ASSIGN b-es-mp232-emit.gerou       = yes
                               b-es-mp232-emit.dt-geracao  = TODAY
                               b-es-mp232-emit.obs-geracao  = "Impostos Irrf-Cofins gerados na nota " + string(num-nota)
                               b-es-mp232-emit.nota-geracao  = string(num-nota).
                    END. /* for each es-mp */
                END.
                FIND FIRST es-mp232-emit WHERE
                   es-mp232-emit.cod-estabel  = pedido-compr.cod-estabel  AND
                   es-mp232-emit.cod-emitente = pedido-compr.cod-emitente AND
                   es-mp232-emit.mes          = STRING(MONTH(es-param-empres.u-date-1)) AND
                   es-mp232-emit.nr-nota-fis  = num-nota
                   EXCLUSIVE-LOCK NO-ERROR.
                ASSIGN es-mp232-emit.gerou       = yes
                       es-mp232-emit.dt-geracao  = TODAY
                       es-mp232-emit.vl-irrf  = de-irrf-unit
                       es-mp232-emit.obs-geracao  = "Impostos Irrf-Cofins gerados nesta nota " + string(num-nota)
                       es-mp232-emit.nota-geracao  = string(num-nota).
            END. /* deve gerar */
        END. /* PF */

        /* pessoa jur°dica */
        IF (emitente.natureza = 2) THEN DO:
            
            IF de-soma-mp >= b-es-param-empresa.val-min-pj THEN DO:
            FIND FIRST es-emitente WHERE
                       es-emitente.cod-emitente = emitente.cod-emitente
                NO-LOCK NO-ERROR.
            IF NOT AVAIL es-emitente or
               not(es-emitente.u-log-1  OR /* cooperativa */
                   es-emitente.u-log-2) /* adepto simples */ THEN DO:
                
                    ASSIGN de-irrf-unit   = ROUND(de-soma-mp * (es-param-empresa.perc-irrf / 100),2)
                           de-cofins-unit = ROUND(de-soma-mp * (es-param-empresa.perc-cofins / 100),2). /* alt round de 2 para 1  -04/11/12- voltei 12/11/12 */

                    ASSIGN de-irrf-doc   = de-irrf-doc   + de-irrf-unit
                           de-cofins-doc = de-cofins-doc + de-cofins-unit.

                    IF de-total < b-es-param-empresa.val-min-pj THEN DO:
                        /* coloca para os outros registros que j† existiam no arquivo a nota que est† contendo o acumulado */
                        FOR EACH b-es-mp232-emit WHERE
                               b-es-mp232-emit.cod-estabel  = pedido-compr.cod-estabel  AND
                               b-es-mp232-emit.cod-emitente = pedido-compr.cod-emitente AND
                               b-es-mp232-emit.mes          = STRING(MONTH(es-param-empres.u-date-1))
                            EXCLUSIVE-LOCK.
        
                            ASSIGN b-es-mp232-emit.gerou       = yes
                                   b-es-mp232-emit.dt-geracao  = TODAY
                                   b-es-mp232-emit.obs-geracao  = "Impostos Irrf-Cofins gerados na nota " + string(num-nota)
                                   b-es-mp232-emit.nota-geracao  = string(num-nota).
                        END. /* for each es-mp */
                    end. /* val min */    
                    FIND FIRST es-mp232-emit WHERE
                               es-mp232-emit.cod-estabel  = pedido-compr.cod-estabel  AND
                               es-mp232-emit.cod-emitente = pedido-compr.cod-emitente AND
                               es-mp232-emit.mes          = STRING(MONTH(es-param-empres.u-date-1)) AND
                               es-mp232-emit.nr-nota-fis  = num-nota
                               EXCLUSIVE-LOCK NO-ERROR.
                    ASSIGN es-mp232-emit.gerou       = yes
                               es-mp232-emit.dt-geracao  = TODAY
                               es-mp232-emit.vl-irrf   = de-irrf-unit
                               es-mp232-emit.vl-cofins = de-cofins-unit
                               es-mp232-emit.obs-geracao  = "Impostos Irrf-Cofins gerados nesta nota " + string(num-nota)
                               es-mp232-emit.nota-geracao  = string(num-nota).
                    
            END. /* n∆o cooperativa e n∆o adepto do simples ou n∆o existe extens∆o */
          END. /* vl maior */
        END. /* PJ */

    END. /* MP ATIVA */
   
END.


FOR EACH bf-es-saldo-arroz WHERE bf-es-saldo-arroz.nr-contrato = i-contrato NO-LOCK:

    ASSIGN vl-saldo = vl-saldo + bf-es-saldo-arroz.qtidade-atu
           vl-bloqu = vl-bloqu + bf-es-saldo-arroz.qt-bloqueada.
END.

ASSIGN de-cdo-unit-t      = 0
       de-funrural-unit-t = 0
       de-senar-unit-t    = 0
       de-irrf-unit-t     = 0
       de-cofins-unit-t   = 0.


IF  param-global.modulo-ft THEN DO: 
    IF  natur-oper.imp-nota THEN DO:
        ASSIGN i-nr-docto = INT(STRING(DAY(TODAY),"99") + STRING(TIME,"99999")).
    END.
    ELSE DO:
        MESSAGE "Natureza de Operaá∆o n∆o gera nota no Faturamento!" VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN.
    END.
END.
ELSE DO:
    MESSAGE "M¢dulo Faturamento n∆o est† implantado. Verificar!" VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
END.

/*ERIKA*/
FOR EACH ORDEM-COMPRA WHERE
         ORDEM-COMPRA.NUM-PEDIDO = PEDIDO-COMPR.NUM-PEDIDO NO-LOCK BREAK BY ordem-compra.it-codigo:

    FIND FIRST cotacao-item OF ordem-compra NO-LOCK NO-ERROR.
    
    FIND FIRST prazo-compra OF ordem-compra NO-LOCK NO-ERROR.
       
  
    ASSIGN i-contrato = ordem-compr.int-1
           vl-saldo   = 0.00
           vl-bloqu   = 0.00.
        
    FIND es-contrato-arroz NO-LOCK WHERE es-contrato-arroz.nr-contrato = i-contrato NO-ERROR.
    
    FIND emitente WHERE emitente.cod-emitente = pedido-compr.cod-emitente NO-LOCK NO-ERROR.
       
    FIND es-param-empresa NO-LOCK WHERE es-param-empresa.ep-codigo = i-ep-codigo-usuario NO-ERROR.
    
    FOR EACH bf-es-saldo-arroz WHERE bf-es-saldo-arroz.nr-contrato = es-contrato-arroz.nr-contrato NO-LOCK:
        ASSIGN vl-saldo = vl-saldo + bf-es-saldo-arroz.qtidade-atu
               vl-bloqu = vl-bloqu + bf-es-saldo-arroz.qt-bloqueada.
    END.
    
    FIND es-item NO-LOCK WHERE es-item.it-codigo = ordem-compra.it-codigo NO-ERROR.
    
    IF l-compl-preco = NO then do:
        FIND nota-fiscal NO-LOCK
            WHERE nota-fiscal.cod-estabel = pedido-compr.cod-estabel
              AND nota-fiscal.nr-nota-fis = substr(pedido-compr.c-observacao[3],51,07)
              AND nota-fiscal.serie       = c-serie NO-ERROR.
        
        IF AVAIL nota-fiscal THEN DO:

          FIND b2-emitente NO-LOCK WHERE b2-emitente.nome-abrev = nota-fiscal.nome-ab-cli NO-ERROR.
        END.
    END. /* <> compl */
    ELSE DO:
        
      FIND b2-emitente NO-LOCK WHERE b2-emitente.cod-emitente = pedido-compr.cod-emitente NO-ERROR. /* para complemento de preáo */

    END.


    /* Compra a Deposito */
    ASSIGN de-cdo-unit      = 0
           de-funrural-unit = 0
           de-senar-unit = 0
           de-irrf-unit     = 0
           de-cofins-unit   = 0
           de-unitario      = IF cotacao-item.preco-fornec > 0 THEN cotacao-item.preco-fornec ELSE cotacao-item.dec-1 /* inverti 09/11/12 */
           de-total         = ROUND(ordem-compra.qt-solic * de-unitario,2)
           de-total-sem-imp = ROUND(ordem-compra.qt-solic * cotacao-item.dec-1,2). /* erika 09/11/12 para calcular sem impostos, pelo unit†rio dig na tela es4019 */
        
    IF l-compl-preco = NO          AND
       es-item.tem-cdo             AND
       es-param-estab.l-cdo        AND
       /*emitente.estado     = "RS" AND 07/12/2015 retirei porque agora tem o controle pelo estabelcimento */
      (emitente.natureza   = 1     OR emitente.natureza = 4) THEN DO:
        ASSIGN de-cdo-unit   = ROUND(es-param-empresa.vl-cdo * (ordem-compra.qt-solic / es-item.unidade-saco),2) /* alt round de 2 para 1  -04/11/12- voltei 12/11/12 */
               de-cdo-unit-t = de-cdo-unit-t + de-cdo-unit
               l-prim-cdo    = NO .
    END.


    IF AVAIL es-item AND 
       es-param-estab.l-fr        AND
       es-item.tem-funrural    AND 
      (emitente.natureza  = 1     OR emitente.natureza = 4) THEN DO:
        ASSIGN de-funrural-unit = ROUND((de-total-sem-imp - de-cdo-unit) *
                                              ((es-param-empresa.vl-funrural-1 +
                                                es-param-empresa.vl-funrural-2 +
                                                es-param-empresa.vl-funrural-3) / 100),2) /* alterado 04/18/2017 */.               
                         
            ASSIGN de-funrural-unit-t = de-funrural-unit-t + de-funrural-unit
                   l-prim-fun         = NO.
          


    END.

    IF AVAIL es-item AND 
       es-param-estab.l-senar  AND
      (emitente.natureza  = 1     OR emitente.natureza = 4) THEN DO:
        ASSIGN de-senar-unit = ROUND(((de-total-sem-imp - de-cdo-unit) * (es-param-empresa.vl-senar / 100)),2). /* alterado 04/18/2017 */
                         
            ASSIGN de-senar-unit-t    = de-senar-unit-t + de-senar-unit.
           /* para criar tabela de controle de ajuste do Senar 23/05/2013 - cei arrendondamento DP com sefaz */
           ASSIGN de-ajuste-senar-cei = de-senar-unit.


    END.


    FIND FIRST b-es-param-empresa WHERE b-es-param-empresa.ep-codigo = i-ep-codigo-usuario NO-LOCK NO-ERROR.
    IF b-es-param-empresa.mp-ativa THEN DO:
        FIND nota-fiscal NO-LOCK
        WHERE nota-fiscal.cod-estabel = pedido-compr.cod-estabel
          AND nota-fiscal.nr-nota-fis = substr(pedido-compr.c-observacao[3],51,07)
          AND nota-fiscal.serie = c-serie NO-ERROR.

        /* MP - 160105 */
        ASSIGN da-vencimento = date(MONTH(TODAY),01,YEAR(TODAY)) + 32
               da-vencimento = date(MONTH(da-vencimento),15,YEAR(da-vencimento)).
        RUN pi-cria-mp232. /* pra qq caso cria */

        FIND FIRST b-es-mp232-emit WHERE
                   b-es-mp232-emit.cod-estabel  = pedido-compr.cod-estabel  AND
                   b-es-mp232-emit.cod-emitente = pedido-compr.cod-emitente AND
                   b-es-mp232-emit.mes          = STRING(MONTH(es-param-empres.u-date-1))
                   EXCLUSIVE-LOCK NO-ERROR.
        IF AVAIL b-es-mp232-emit THEN DO:
          FOR EACH b-es-mp232-emit WHERE
                   b-es-mp232-emit.cod-estabel  = pedido-compr.cod-estabel  AND
                   b-es-mp232-emit.cod-emitente = pedido-compr.cod-emitente AND
                   b-es-mp232-emit.mes          = STRING(MONTH(es-param-empres.u-date-1))
            NO-LOCK.
              IF b-es-mp232-emit.gerou = NO THEN
                ASSIGN de-soma-mp = de-soma-mp + de-total.
          END. /* for each */        
        END. /* existe acumulado */

        /* pessoa f°sica */
        IF (emitente.natureza = 1 OR emitente.natureza = 4) THEN DO:
            IF de-soma-mp >= b-es-param-empresa.val-min-pf THEN DO:
                
                ASSIGN de-irrf-unit = ROUND(de-soma-mp * (es-param-empresa.perc-irrf / 100),2). /* alt round de 2 para 1  -04/11/12- voltei 12/11/12 */
                ASSIGN de-irrf-doc = de-irrf-doc + de-irrf-unit.
                
                IF de-total < b-es-param-empresa.val-min-pf THEN DO:
                    /* coloca para os outros registros que j† existiam no arquivo a nota que est† contendo o acumulado */
                    FOR EACH b-es-mp232-emit WHERE
                       b-es-mp232-emit.cod-estabel  = pedido-compr.cod-estabel  AND
                       b-es-mp232-emit.cod-emitente = pedido-compr.cod-emitente AND
                       b-es-mp232-emit.mes          = STRING(MONTH(es-param-empres.u-date-1))
                        EXCLUSIVE-LOCK.
    
                        ASSIGN b-es-mp232-emit.gerou       = yes
                               b-es-mp232-emit.dt-geracao  = TODAY
                               b-es-mp232-emit.obs-geracao  = "Impostos Irrf-Cofins gerados na nota " + string(num-nota)
                               b-es-mp232-emit.nota-geracao  = string(num-nota).
                    END. /* for each es-mp */
                END.
                FIND FIRST es-mp232-emit WHERE
                   es-mp232-emit.cod-estabel  = pedido-compr.cod-estabel  AND
                   es-mp232-emit.cod-emitente = pedido-compr.cod-emitente AND
                   es-mp232-emit.mes          = STRING(MONTH(es-param-empres.u-date-1)) AND
                   es-mp232-emit.nr-nota-fis  = num-nota
                   EXCLUSIVE-LOCK NO-ERROR.
                ASSIGN es-mp232-emit.gerou       = yes
                       es-mp232-emit.dt-geracao  = TODAY
                       es-mp232-emit.vl-irrf  = de-irrf-unit
                       es-mp232-emit.obs-geracao  = "Impostos Irrf-Cofins gerados nesta nota " + string(num-nota)
                       es-mp232-emit.nota-geracao  = string(num-nota).
            END. /* deve gerar */
        END. /* PF */
       
        /* pessoa jur°dica */
        IF (emitente.natureza = 2) THEN DO:
          IF de-soma-mp >= b-es-param-empresa.val-min-pj THEN DO:
            FIND FIRST es-emitente WHERE
                       es-emitente.cod-emitente = emitente.cod-emitente
                NO-LOCK NO-ERROR.
            IF NOT AVAIL es-emitente or
               not(es-emitente.u-log-1  OR /* cooperativa */
                   es-emitente.u-log-2) /* adepto simples */ THEN DO:
                
                    ASSIGN de-irrf-unit   = ROUND(de-soma-mp * (es-param-empresa.perc-irrf / 100),2)
                           de-cofins-unit = ROUND(de-soma-mp * (es-param-empresa.perc-cofins / 100),2). /* alt round de 2 para 1  -04/11/12- voltei 12/11/12 */

                    ASSIGN de-irrf-doc   = de-irrf-doc   + de-irrf-unit
                           de-cofins-doc = de-cofins-doc + de-cofins-unit.

                    IF de-total < b-es-param-empresa.val-min-pj THEN DO:
                        /* coloca para os outros registros que j† existiam no arquivo a nota que est† contendo o acumulado */
                        FOR EACH b-es-mp232-emit WHERE
                               b-es-mp232-emit.cod-estabel  = pedido-compr.cod-estabel  AND
                               b-es-mp232-emit.cod-emitente = pedido-compr.cod-emitente AND
                               b-es-mp232-emit.mes          = STRING(MONTH(es-param-empres.u-date-1))
                            EXCLUSIVE-LOCK.
        
                            ASSIGN b-es-mp232-emit.gerou       = yes
                                   b-es-mp232-emit.dt-geracao  = TODAY
                                   b-es-mp232-emit.obs-geracao  = "Impostos Irrf-Cofins gerados na nota " + string(num-nota)
                                   b-es-mp232-emit.nota-geracao  = string(num-nota).
                        END. /* for each es-mp */
                        
                        FIND FIRST es-mp232-emit WHERE
                               es-mp232-emit.cod-estabel  = pedido-compr.cod-estabel  AND
                               es-mp232-emit.cod-emitente = pedido-compr.cod-emitente AND
                               es-mp232-emit.mes          = STRING(MONTH(es-param-empres.u-date-1)) AND
                               es-mp232-emit.nr-nota-fis  = num-nota
                               EXCLUSIVE-LOCK NO-ERROR.
                        ASSIGN es-mp232-emit.vl-irrf   = de-irrf-unit
                               es-mp232-emit.vl-cofins = de-cofins-unit
                               es-mp232-emit.obs-geracao  = "Impostos Irrf-Cofins gerados nesta nota " + string(num-nota)
                               es-mp232-emit.nota-geracao  = string(num-nota).
                    END. /* de-total */

            END. /* n∆o cooperativa e n∆o adepto do simples ou n∆o existe extens∆o */
          END. /* vl maior */
        END. /* PJ */

    END. /* mp ativa */


    /* tirei impostos em 12/11/12 ASSIGN de-total  = de-total + de-cdo-unit + de-funrural-unit + de-senar-unit + de-irrf-unit + de-cofins-unit. */


    FIND saldo-estoq
        WHERE saldo-estoq.cod-estabel = pedido-compr.cod-estabel
          AND saldo-estoq.it-codigo   = ordem-compra.it-codigo
          AND saldo-estoq.cod-depos   = es-param-estab.cod-dep-consig
          AND saldo-estoq.lote        = ""
          AND saldo-estoq.cod-localiz = ""
          AND saldo-estoq.cod-refer   = ""
        NO-LOCK NO-ERROR.
    IF  NOT AVAIL saldo-estoq THEN DO:
        MESSAGE "Registro de Saldo Estoque n∆o dispon°vel!" VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN.
    END.
    
    FIND estabelec WHERE estabelec.cod-estabel = saldo-estoq.cod-estabel NO-LOCK NO-ERROR.
    IF  NOT AVAIL estabelec THEN RETURN.
    
    FIND emitente WHERE emitente.cod-emitente = estabelec.cod-emitente NO-LOCK NO-ERROR.
    IF  NOT AVAIL emitente THEN RETURN.
    
    FIND ITEM WHERE ITEM.it-codigo = saldo-estoq.it-codigo NO-LOCK NO-ERROR.
    IF  NOT AVAIL ITEM THEN RETURN.
    
    FIND item-estab WHERE 
         item-estab.cod-estabel = saldo-estoq.cod-estabel AND 
         item-estab.it-codigo   = saldo-estoq.it-codigo NO-LOCK NO-ERROR.    
    
    FIND b-saldo-estoq
        WHERE b-saldo-estoq.cod-estabel = pedido-compr.cod-estabel
          AND b-saldo-estoq.cod-depos   = es-param-estab.cod-dep-compra
          AND b-saldo-estoq.it-codigo   = saldo-estoq.it-codigo
          AND b-saldo-estoq.lote        = saldo-estoq.lote
          AND b-saldo-estoq.cod-localiz = saldo-estoq.cod-localiz
          AND b-saldo-estoq.cod-refer   = saldo-estoq.cod-refer
        NO-ERROR.
    IF  NOT AVAIL b-saldo-estoq THEN DO:
        CREATE b-saldo-estoq.
        ASSIGN b-saldo-estoq.cod-estabel  = pedido-compr.cod-estabel         
               b-saldo-estoq.cod-depos    = es-param-estab.cod-dep-compra
               b-saldo-estoq.it-codigo    = saldo-estoq.it-codigo  
               b-saldo-estoq.lote         = saldo-estoq.lote
               b-saldo-estoq.cod-localiz  = saldo-estoq.cod-localiz
               b-saldo-estoq.cod-refer    = saldo-estoq.cod-refer
               b-saldo-estoq.dt-vali-lote = saldo-estoq.dt-vali-lote.  
    END.
     
    FIND estabelec WHERE estabelec.cod-estabel = b-saldo-estoq.cod-estabel NO-LOCK NO-ERROR.
    IF NOT AVAIL estabelec THEN RETURN.
 
    FIND FIRST es-param-estab WHERE es-param-estab.cod-estabel = estabelec.cod-estabel NO-LOCK NO-ERROR.
 
  /*LOCALIZANDO PLANO CONTAS PRINCIPAL*/
    FIND FIRST plano_cta_unid_organ
         WHERE plano_cta_unid_organ.cod_unid_organ         = v_cod_empres_usuar
           AND plano_cta_unid_organ.ind_tip_plano_cta_ctbl = "Prim†rio"
           AND plano_cta_unid_organ.dat_inic_valid         <= TODAY
           AND plano_cta_unid_organ.dat_fim_valid          >= TODAY 
               NO-LOCK NO-ERROR. 
    IF AVAIL plano_cta_unid_organ THEN DO:
        FIND FIRST plano_cta_ctbl 
             WHERE plano_cta_ctbl.cod_plano_cta_ctbl = plano_cta_unid_organ.cod_plano_cta_ctbl
                   NO-LOCK NO-ERROR.
    END.
 
    FIND FIRST cta_ctbl 
         WHERE cta_ctbl.cod_plano_cta_ctbl = (IF AVAIL plano_cta_ctbl THEN plano_cta_ctbl.cod_plano_cta_ctbl ELSE "PLA01")
           AND cta_ctbl.cod_cta_ctbl       = es-param-estab.conta-contab-comp-dep
               NO-LOCK NO-ERROR.
 /* FIND FIRST conta-contab WHERE 
               conta-contab.conta-contab = es-param-estab.conta-contab-comp-dep AND 
               conta-contab.ep-codigo    = param-global.empresa-prin NO-LOCK  NO-ERROR. */
    
    DO TRANS:
    IF l-prim THEN DO:

     /* coment†rio do complemento de preáo */
     assign found1 = INDEX(pedido-compr.Comentario,sentence1,1)
            found2 = INDEX(pedido-compr.Comentario,sentence2,1)
            found3 = INDEX(pedido-compr.Comentario,sentence3,1).

        ASSIGN l-tem-cei = NO.
        
        /*FIND FIRST es-param-estab WHERE es-param-estab.cod-estabel = estabelec.cod-estabel NO-LOCK NO-ERROR.
        IF es-param-estab.l-cei = NO THEN DO:
            MESSAGE "ATENÄ«O!!! Estabelecimento " estabelec.cod-estabel " n∆o est† com CEI ativa!" VIEW-AS ALERT-BOX INFORMATION.
        END.*/
        
        FIND FIRST es-emitente-cei WHERE es-emitente-cei.cod-emitente = b2-emitente.cod-emitente NO-LOCK NO-ERROR.
        /* natureza = 1 - PF */
        IF AVAIL es-emitente-cei AND es-emitente-cei.l-ativo = YES AND es-emitente-cei.dt-validade-cei >= TODAY AND b2-emitente.natureza = 1 AND es-param-estab.l-cei THEN DO:
           ASSIGN l-tem-cei = YES.
        END.
       

        /* tratando se Ç para aplicar o funrural deixa como j† calculado com base nos percentuais do estabelecimento, sen∆o zera o que se calculou e abaixo tb n∆o permite gerar parc FR */
        IF l-tem-cei THEN do:
            ASSIGN de-funrural-doc    = 0
                   de-funrural-unit   = 0
                   i-num-cei          = 1 /* para descontar s¢ uma vez o FR da DP */
                   /*de-funrural-unit-t = 0 n∆o zerando 26/04/2013 para descontar da duplicata */.

        END.
        ELSE i-num-cei = 2. /* para retirar o valor da base da DP */     

        /* 20/05/2013 n∆o tem estabelcimento setado para CEI ZERA os FrÔs */
        IF es-param-estab.l-cei = NO THEN DO:
            ASSIGN de-funrural-doc    = 0
                   de-funrural-unit   = 0
                   i-num-cei          = 1 /* para descontar s¢ uma vez o FR da DP */
                   /*de-funrural-unit-t = 0 n∆o zerando 26/04/2013 para descontar da duplicata */.
        END.


  

    ASSIGN de-unit-item = IF l-compl-preco THEN de-total-t-sem-imp - de-cdo-doc + de-irrf-doc + de-cofins-doc 
                                           ELSE  (de-total-t-sem-imp - de-cdo-doc + de-irrf-doc + de-cofins-doc) / ordem-compra.qt-solic.
                                                              


        ASSIGN l-prim = NO
               de-valor-merc = IF l-tem-cei THEN de-total-t-sem-imp - de-cdo-doc + de-irrf-doc + de-cofins-doc 
                                            ELSE de-total-t-sem-imp - de-cdo-doc + de-irrf-doc + de-cofins-doc 
                                                
               de-unit-item-4 = de-unit-item. /* para deixar unit†rio com quatro casas se maior n∆o passa sefaz se menor n∆o chega no arredondamento */.

        CREATE tt-docum-est.
        ASSIGN tt-docum-est.serie-docto      = c-serieE
               tt-docum-est.nro-docto        = string(i-nr-docto,"9999999")
               tt-docum-est.cod-emitente     = IF AVAIL b2-emitente THEN
                                                  b2-emitente.cod-emitente
                                               ELSE
                                               IF AVAIL es-ticket THEN 
                                                  es-ticket.cod-produtor 
                                               ELSE es-contrato-arroz.cod-emitente
               tt-docum-est.nat-operacao     = natur-oper.nat-operacao
               tt-docum-est.cod-estabel      = pedido-compr.cod-estabel
               /*erikatt-docum-est.estab-de-or      = saldo-estoq.cod-estabel*/
               tt-docum-est.pais-origem      = "RE1001"
               tt-docum-est.esp-docto        = 21  /*** NFE ***/
               tt-docum-est.esp-fiscal       = "NFE"
               tt-docum-est.aliq-irf         = 0
               tt-docum-est.aliquota-icm     = natur-oper.aliquota-icm
               tt-docum-est.aliquota-iss     = 0
               tt-docum-est.base-cofins-subs = 0

               tt-docum-est.base-icm         = de-unit-item-4 * ordem-compra.qt-solic /*de-valor-merc*/
                                                  
               tt-docum-est.tot-valor        = de-unit-item-4 * ordem-compra.qt-solic /* de-valor-merc*/
                                                  
               tt-docum-est.valor-mercad     = de-unit-item-4 * ordem-compra.qt-solic /*de-valor-merc*/

                                                               

               tt-docum-est.base-ipi         = 0 /*voltei dia 30 de-total-t + de-cdo-doc + de-funrural-doc + de-senar-doc + de-irrf-doc + de-cofins-doc /* erika 29/03/11 - era 0 */*/
               tt-docum-est.base-iss         = 0
               tt-docum-est.base-pis-subs    = 0
               tt-docum-est.base-subs        = 0
               tt-docum-est.cod-observa      = 1   /*** INDUSTRIA ***/
               tt-docum-est.conta-transit    = "" /*(IF AVAIL cta_ctbl THEN cta_ctbl.cod_cta_ctbl ELSE "")*/   /*conta-contab.conta-contab*/
               tt-docum-est.ct-transit       = (IF AVAIL cta_ctbl THEN cta_ctbl.cod_cta_ctbl ELSE "")          /*conta-contab.ct-codigo   */
               tt-docum-est.sc-transit       = ""                                                              /*conta-contab.sc-codigo   */
               tt-docum-est.despesa-nota     = 0
               tt-docum-est.dt-emissao       = TODAY   
               tt-docum-est.dt-trans         = TODAY   
               tt-docum-est.dt-venc-icm      = TODAY
               tt-docum-est.dt-venc-ipi      = TODAY
               tt-docum-est.dt-venc-iss      = TODAY
               tt-docum-est.estab-fisc       = tt-docum-est.cod-estabel
               tt-docum-est.icm-complem      = 0
               tt-docum-est.icm-deb-cre      = 0 
               tt-docum-est.icm-fonte        = 0
               tt-docum-est.icm-nao-trib     = 0
               tt-docum-est.icm-outras       = 0
               tt-docum-est.ind-orig-entrada = 1   /*** NORMAL ***/
               tt-docum-est.ind-rateio       = NO
               tt-docum-est.ind-via-envio    = 1   /*** NORMAL ***/
               tt-docum-est.ipi-deb-cre      = 0
               tt-docum-est.ipi-despesa      = 0 
               tt-docum-est.ipi-nao-trib     = 0
               tt-docum-est.ipi-outras       = 0
               tt-docum-est.iss-deb-cre      = 0
               tt-docum-est.iss-nao-trib     = 0
               tt-docum-est.iss-outras       = 0
               tt-docum-est.observacao       = IF l-compl-preco THEN "### Pedido de Complemento de Preáo ###" + chr(10) +
                                                                    SUBSTRING(pedido-compr.Comentario,found1,73) + chr(10) +  
                                                                    SUBSTRING(pedido-compr.Comentario,found2,15) + chr(10) +
                                                                    SUBSTRING(pedido-compr.Comentario,found3,37) + chr(10) +
                                                                    string(SUBSTR(pedido-compr.c-observacao[2],20,70),"x(70)") + chr(10) +
                                                                    "NFP:" + string(num-nota,"x(10)")
                                                               ELSE ""
               tt-docum-est.rec-fisico       = NO
               tt-docum-est.sit-docum        = 1   /*** OK ***/
               tt-docum-est.tipo-docto       = 1   /*** ENTRADA ***/
               tt-docum-est.tipo-nota        = 1   /*** COMPRA ***/
               tt-docum-est.tot-peso         = de-qt-solic
              
               tt-docum-est.usuario          = v_cod_usuar_corren
               
               tt-docum-est.valor-frete      = 0
               tt-docum-est.valor-irf        = 0
               tt-docum-est.valor-outras     = 0
               tt-docum-est.valor-seguro     = 0
               tt-docum-est.via-transp       = 1   /*** RODOVIARIO ***/
               tt-docum-est.vl-cofins-subs   = 0
               tt-docum-est.vl-imp-frete     = 0
               tt-docum-est.vl-imp-outras    = 0
               tt-docum-est.vl-imp-seguro    = 0
               tt-docum-est.vl-pis-subs      = 0
               tt-docum-est.vl-subs          = 0

               /* 30/11/2011 */
               tt-docum-est.cidade           = IF AVAIL b2-emitente THEN 
                                                  b2-emitente.cidade ELSE emitente.cidade
               tt-docum-est.uf               = IF AVAIL b2-emitente THEN 
                                                  b2-emitente.estado ELSE emitente.estado
               tt-docum-est.pais             = IF AVAIL b2-emitente THEN 
                                                  b2-emitente.pais ELSE emitente.pais
               tt-docum-est.bairro           = IF AVAIL b2-emitente THEN 
                                                  b2-emitente.bairro ELSE emitente.bairro
               tt-docum-est.cep              = IF AVAIL b2-emitente THEN 
                                                  b2-emitente.cep ELSE emitente.cep
               tt-docum-est.endereco         = IF AVAIL b2-emitente THEN 
                                                  b2-emitente.endereco ELSE emitente.endereco
               tt-docum-est.cod-entrega      = IF AVAIL b2-emitente THEN 
                                                  b2-emitente.cod-entrega ELSE emitente.cod-entrega.

           MESSAGE "es4027a  CREATE tt-docum-est"  SKIP
                   "tt-docum-est.conta-transit" tt-docum-est.conta-transit SKIP
                   "tt-docum-est.ct-transit   " tt-docum-est.ct-transit    SKIP
                   "tt-docum-est.sc-transit   " tt-docum-est.sc-transit    SKIP
                   VIEW-AS ALERT-BOX INFO BUTTONS OK.

        RUN esp/esapi003a.p (INPUT TABLE tt-docum-est).
        
        /* Pedido de Complemento de Preáo 27/02 erika */
        IF l-compl-preco THEN do:    

            /* cria para podeer relacionar a nota ao pedido e encontrar depois no ure1005 para criar nota-fisc-adc  informaá‰es referenciadas */
            FIND FIRST es-pedido-compr where
                       es-pedido-compr.num-pedido = pedido-compr.num-pedido NO-ERROR.
            IF AVAIL es-pedido-compr THEN DO:
                DELETE es-pedido-compr.
            END.

            CREATE es-pedido-compr.
            ASSIGN es-pedido-compr.num-pedido             = pedido-compr.num-pedido
                   es-pedido-compr.cod-docto-referado     = string(num-nota)
                   es-pedido-compr.cod-ser-docto-referado = string(num-serie).
            

        END. /* complemento para gravar nota referenciada para cria no re1001 */

    END. /* l-prim */
    END. /* trans */

 

ASSIGN /* erika retirei pq agora est† trazendo o certo 09/11/12 de-unitario = de-total / ordem-compra.qt-solic*/
           i-seq = i-seq + 1.
 
    CREATE tt-item-doc-est.
    ASSIGN tt-item-doc-est.serie-docto      = c-serieE
           tt-item-doc-est.nro-docto        = string(i-nr-docto,"9999999")
           tt-item-doc-est.cod-emitente     = IF AVAIL b2-emitente THEN
                                                  b2-emitente.cod-emitente
                                               ELSE
                                               IF AVAIL es-ticket THEN 
                                                  es-ticket.cod-produtor 
                                               ELSE es-contrato-arroz.cod-emitente
           tt-item-doc-est.nat-operacao     = natur-oper.nat-operacao        
           tt-item-doc-est.sequencia        = i-seq /*primeiro seq */
           tt-item-doc-est.it-codigo        = saldo-estoq.it-codigo
           tt-item-doc-est.cod-depos        = b-saldo-estoq.cod-depos
           tt-item-doc-est.cod-localiz      = b-saldo-estoq.cod-localiz
           tt-item-doc-est.cod-refer        = b-saldo-estoq.cod-refer
           tt-item-doc-est.lote             = b-saldo-estoq.lote
           tt-item-doc-est.dt-vali-lote     = b-saldo-estoq.dt-vali-lote
           
           tt-item-doc-est.aliquota-icm     = 12 /* IF natur-oper.nat-operacao = "1101sb" THEN 12 ELSE 0  voltei dia 30 12 /* erika 29/03/2011 ver de onde pega a aliquota natur-oper.aliquota-icm */*/
           tt-item-doc-est.aliquota-ipi     = 0
           tt-item-doc-est.aliquota-iss     = 0
           tt-item-doc-est.base-cofins-subs = 0
           tt-item-doc-est.base-icm-cmi     = 0
           tt-item-doc-est.base-ipi-cmi     = 0
           tt-item-doc-est.base-iss-cmi     = 0
           tt-item-doc-est.base-pis-subs    = 0
           tt-item-doc-est.cd-trib-icm      = 5 /*IF natur-oper.nat-operacao = "1101sb" THEN 5 ELSE  natur-oper.cd-trib-icm  era s¢ da natureza erika 30/03 */
           tt-item-doc-est.cd-trib-ipi      = natur-oper.cd-trib-ipi
           tt-item-doc-est.cd-trib-iss      = 2  /*** ISENTO ***/
           tt-item-doc-est.class-fiscal     = ITEM.class-fiscal
           tt-item-doc-est.conta-contabil   = ""
           tt-item-doc-est.ct-codigo        = tt-docum-est.ct-transit 
           tt-item-doc-est.sc-codigo        = ""
           tt-item-doc-est.dt-ent-prev      = TODAY
           tt-item-doc-est.est-cob          = pedido-compr.cod-estabel
           tt-item-doc-est.narrativa        = string(SUBSTR(pedido-compr.c-observacao[2],20,70),"x(70)") + string(num-nota,"x(10)")   
           tt-item-doc-est.peso-liquido     = ordem-compra.qt-solic
           tt-item-doc-est.preco-total[1]   =  /*ROUND(de-unit-item,2) */ de-unit-item-4 * ordem-compra.qt-solic
           tt-item-doc-est.preco-total[2]   =  /*ROUND(de-unit-item,2) */ de-unit-item-4 * ordem-compra.qt-solic
           tt-item-doc-est.preco-unit[1]    = de-unit-item-4  /*ROUND(de-unit-item,2) */
           tt-item-doc-est.preco-unit[2]    = de-unit-item-4 /*round(de-unit-item,2)*/
           tt-item-doc-est.qt-do-forn       = ordem-compra.qt-solic
           tt-item-doc-est.qt-real          = ordem-compra.qt-solic        
           tt-item-doc-est.qt-saldo         = ordem-compra.qt-solic
           tt-item-doc-est.quantidade       = ordem-compra.qt-solic
           tt-item-doc-est.un               = ITEM.un
           tt-item-doc-est.usuario          = v_cod_usuar_corren
           tt-item-doc-est.hora             = STRING(TIME,"HH:MM:SS")
           tt-item-doc-est.valor-frete      = 0
           tt-item-doc-est.vl-cofins-subs   = 0
           tt-item-doc-est.vl-isr           = 0
           tt-item-doc-est.vl-iss-cmi       = 0
           tt-item-doc-est.vl-pis-subs      = 0
           tt-item-doc-est.vl-subs-cmi      = 0
           tt-item-doc-est.vl-taxa          = 0
           tt-item-doc-est.vl-unit-mob      = 0
           tt-item-doc-est.pr-total[1]      = de-unit-item-4 * ordem-compra.qt-solic
           tt-item-doc-est.pr-total[2]      = de-unit-item-4 * ordem-compra.qt-solic
           tt-item-doc-est.pr-total[3]      = de-unit-item-4 * ordem-compra.qt-solic
           tt-item-doc-est.baixa-ce         = natur-oper.baixa-estoq
           tt-item-doc-est.base-icm[1]      = 0 /* erika 29/03/2011 era 0 */
           tt-item-doc-est.base-icm[2]      = 0
           tt-item-doc-est.icm-outras[1]    = 0 /* voltei 30 de-total /* deixar erika 29/03/2011 */ */
           tt-item-doc-est.icm-outras[2]    = 0
           tt-item-doc-est.base-ipi[1]      = 0
           tt-item-doc-est.base-ipi[2]      = 0
           tt-item-doc-est.ipi-outras[1]    = 0 /* voltei dia 30de-total /* erika 29/03/2011 era 0 */*/
           tt-item-doc-est.ipi-outras[2]    = 0
           tt-item-doc-est.base-iss[1]      = 0
           tt-item-doc-est.base-iss[2]      = 0
           tt-item-doc-est.base-subs[1]     = 0
           tt-item-doc-est.base-subs[2]     = 0
           tt-item-doc-est.num-pedido       = pedido-compr.num-pedido
           tt-item-doc-est.numero-ordem     = ordem-compr.numero-ordem
           tt-item-doc-est.parcela          = prazo-compra.parcela
           tt-item-doc-est.valor-icm[1]     = 0 /* voltei 30 de-total * 12 / 100 /* erika 30/03 era 0 */*/
           tt-item-doc-est.ipi-outras[1]    = 0 /* voltei 30 de-total /* erika 30/03 era 0 */*/
           /* rde - totvs 12 tt-item-doc-est.num-sit-trib-icms = */ .

        MESSAGE "es4027a  CREATE tt-item-doc-est"  SKIP
                 "tt-docum-est.conta-transit" tt-docum-est.conta-transit SKIP
                 "tt-docum-est.ct-transit   " tt-docum-est.ct-transit    SKIP
                 "tt-docum-est.sc-transit   " tt-docum-est.sc-transit    SKIP

                 "tt-item-doc-est.conta-contabil " tt-item-doc-est.conta-contabil  SKIP
                 "tt-item-doc-est.ct-codigo      " tt-item-doc-est.ct-codigo       SKIP
                 "tt-item-doc-est.sc-codigo      " tt-item-doc-est.sc-codigo       SKIP
                 VIEW-AS ALERT-BOX INFO BUTTONS OK.


    /* erika 30/03/2011 pis/cofins */
    IF not(substring(ITEM.char-2,32,4) = "") OR
       not(substring(ITEM.char-2,37,4) = "") THEN DO:


/* verificar que no ems2.06 vaqi deixar de ser char-2 pra ter campos no item-doc-est mesmo, como o idi-tributac-pis */
/* campo foi mudado e percebido em 25/10/2012                                                                       */
/* Washington e Waldemar pediram pra colocar direto do Item 26/10/2012                                              */
/* comentado para atribuir direto nos campos que agora existem no ems - 25/10/2012 - erika */
/* n∆o usar os campos de pis cofins da natureza e sim do item 26/10/2012 */

/*         MESSAGE                                                 */
/* "cof" DEC(substring(ITEM.char-2,37,4)) SKIP                     */
/* "tt-item-doc-est.val-aliq-pis "dec(substring(ITEM.char-2,32,4)) */
/*             " nat" tt-docum-est.nat-operacao                    */
/*                                                                 */
/*             natur-oper.per-fin-soc[1]                           */
/*                                                                 */
/*             VIEW-AS ALERT-BOX INFO BUTTONS OK.                  */

      

       
             ASSIGN
                 tt-item-doc-est.idi-tributac-cofins  = 1 /* tributado  "tipo cofins" */
                 tt-item-doc-est.val-aliq-cofins      = natur-oper.per-fin-soc[1] /* aliquota do pis */ /* 07/06/213 michel pedir pra por nat de volta DEC(substring(ITEM.char-2,37,4))  /* "alq cofins" */  */


                 tt-item-doc-est.val-base-calc-cofins = de-unit-item-4 * ordem-compra.qt-solic
                 tt-item-doc-est.val-cofins           = (de-unit-item-4 * ordem-compra.qt-solic) * (natur-oper.per-fin-soc[1] / 100) 

                 tt-item-doc-est.idi-tributac-pis     = 1 /* tributado  "tipo cofins" */                      
                 tt-item-doc-est.val-aliq-pis         = natur-oper.perc-pis[1] /* aliquota do pis */ /* 0706213 michel pediu pra por nat de volta dec(substring(ITEM.char-2,32,4))  /* "alq pis" */ */
                 tt-item-doc-est.base-pis             = de-unit-item-4 * ordem-compra.qt-solic          
                 tt-item-doc-est.valor-pis            = (de-unit-item-4 * ordem-compra.qt-solic) * (natur-oper.perc-pis[1] / 100) .

    END.
               
    FIND FIRST rat-lote where
               rat-lote.serie-docto  = tt-item-doc-est.serie-docto and
               rat-lote.nro-docto    = tt-item-doc-est.nro-docto AND
               rat-lote.cod-emitente = tt-item-doc-est.cod-emitente  AND
               rat-lote.nat-operacao = tt-item-doc-est.nat-operacao   AND
               rat-lote.sequencia    = tt-item-doc-est.sequencia
               NO-LOCK USE-INDEX documento NO-ERROR.
    IF NOT AVAIL rat-lote THEN DO:        
        CREATE rat-lote.
        ASSIGN rat-lote.serie-docto  = tt-item-doc-est.serie-docto
               rat-lote.nro-docto    = tt-item-doc-est.nro-docto
               rat-lote.cod-emitente = tt-item-doc-est.cod-emitente
               rat-lote.nat-operacao = tt-item-doc-est.nat-operacao
               rat-lote.sequencia    = tt-item-doc-est.sequencia
               rat-lote.it-codigo    = tt-item-doc-est.it-codigo
               rat-lote.cod-depos    = b-saldo-estoq.cod-depos
               rat-lote.cod-localiz  = b-saldo-estoq.cod-localiz
               rat-lote.lote         = b-saldo-estoq.lote
               rat-lote.dt-vali-lote = b-saldo-estoq.dt-vali-lote
               rat-lote.cod-refer    = b-saldo-estoq.cod-refer
               rat-lote.quantidade   = ordem-compra.qt-solic.
    END.



END. /* novo end de ordem-compra erika */          

FIND FIRST docum-est OF tt-item-doc-est NO-ERROR.

find first tt-docum-est where 
           tt-docum-est.nro-docto    = docum-est.nro-docto
     and   tt-docum-est.cod-emitente = docum-est.cod-emitente
     and   tt-docum-est.serie-docto  = docum-est.serie-docto
     and   tt-docum-est.nat-operacao = docum-est.nat-operacao no-lock no-error.

{utp/ut-glob.i}

{cdp/cdapi150.i3} /*temp-table de versá∆o */ 

def buffer b-item-doc-est for item-doc-est.

def new global shared var r-RE0301-documento as rowid no-undo.
def new global shared var c-RE0301-origem as character no-undo.
def new global shared var c-RE0301-usuario like param-re.usuario no-undo.
def new global shared var i-ct-componente like item-doc-est.ct-codigo no-undo.
def new global shared var i-sc-componente like item-doc-est.sc-codigo no-undo.
def new global shared var l-consiste-saldo as logical initial yes no-undo.

def new shared var de-qtd-tot-ordem as decimal no-undo.

/*{cdp/cd9999.i3}*/

def var i-nr-item as int no-undo.
def var l-compras as logical no-undo.

def var c-mo-1 as character format "x(10)" no-undo.
def var c-mo-2 as character format "x(10)" no-undo.
def var de-mo-cot1 as decimal decimals 4 format ">>>,>>>,>>9.9999" no-undo.
def var de-mo-cot2 as decimal decimals 4 format ">>>,>>>,>>9.9999" no-undo.
def var de-aliq-icm as decimal no-undo.
def var de-conversao as decimal no-undo.
def var de-conv-aux  as decimal no-undo.
def var da-dt-conv   as date no-undo.
def var de-desc-tot-ord  as decimal no-undo.
def var de-preco-tot-ord as decimal no-undo.
def var de-desconto  as decimal no-undo.
def var l-dig-qtd as logical no-undo.
  

/* Vari†veis para c†lculo dos impostos */
{rep/re9301.i04 new}

find first param-global no-lock no-error.
find first param-estoq  no-lock no-error.
find first param-compra no-lock no-error.
find first param-cq     no-lock no-error.

if param-estoq.moeda1 <> 0 then 
find  mgcad.moeda where  mgcad.moeda.mo-codigo = param-estoq.moeda1.
if  AVAIL  mgcad.moeda then
    c-mo-1 =  mgcad.moeda.descricao.

if param-estoq.moeda2 <> 0 then 
find  mgcad.moeda where  mgcad.moeda.mo-codigo = param-estoq.moeda2.
if  avail  mgcad.moeda then
    c-mo-2 =  mgcad.moeda.descricao.

find param-re use-index ind-usu where param-re.usuario = USERID("mgadm") no-lock no-error.

find natur-oper where natur-oper.nat-operacao = docum-est.nat-operacao no-lock no-error.

find emitente where emitente.cod-emit = docum-est.cod-emit no-lock no-error.

find first para-fat no-lock no-error.
                           
/* Seta flags referentes a codigo de observacao */
                

 /*{rep/re0152.i} */
assign l-industria = docum-est.cod-observa = 1
       l-comercio  = docum-est.cod-observa = 2
       l-devolucao = docum-est.cod-observa = 3
       l-servicos  = docum-est.cod-observa = 4.

if  avail natur-oper then
    assign l-remessa  = not natur-oper.transf
                        and natur-oper.terceiros
                        and natur-oper.tipo      <> 1
                        and natur-oper.oper-terc <> 1
           l-retorno  = not natur-oper.transf
                        and natur-oper.tipo = 1
                        and natur-oper.terceiros
                        and natur-oper.oper-terc <> 1
           l-entrada  = natur-oper.tipo = 1 
           l-transf   = natur-oper.transf
           l-ent-cons = not natur-oper.transf
                        and natur-oper.terceiros
                        and natur-oper.tipo      = 1
                        and natur-oper.oper-terc = 1
           l-sai-cons = not natur-oper.transf
                        and natur-oper.terceiros
                        and natur-oper.tipo     <> 1
                        and natur-oper.oper-terc = 1.


find first tt-docum-est where 
           tt-docum-est.nro-docto    = docum-est.nro-docto
     and   tt-docum-est.cod-emitente = docum-est.cod-emitente
     and   tt-docum-est.serie-docto  = docum-est.serie-docto
     and   tt-docum-est.nat-operacao = docum-est.nat-operacao no-lock no-error.
         

     assign i-nr-item = 0.
 
for each tt-item-doc-est where 
         tt-item-doc-est.nro-docto    = docum-est.nro-docto 
    and  tt-item-doc-est.cod-emitente = docum-est.cod-emitente
    and  tt-item-doc-est.serie-docto  = docum-est.serie-docto
    and  tt-item-doc-est.nat-operacao = docum-est.nat-operacao no-lock trans:

    

    find last item-doc-est WHERE 
        item-doc-est.serie-docto = docum-est.serie-docto AND
        item-doc-est.nro-docto = docum-est.nro-docto  AND
        item-doc-est.cod-emitente = docum-est.cod-emitente and
        item-doc-est.nat-operacao = docum-est.nat-operacao 
        no-lock no-error.
    
    
    find last item-doc-est OF docum-est no-lock no-error.
    if  avail item-doc-est then
       assign i-seq = item-doc-est.sequencia + param-re.inc-seq.
    else            
       assign i-seq = param-re.seq-item-um.

    assign i-nr-item = i-nr-item + 1.
   

    /*****Numero de itens da nota excede o m ximo permitido pelo faturamento 48*****/  
    if  param-global.modulo-ft and natur-oper.imp-nota and avail para-fat then do:
            if  i-nr-item > para-fat.nr-item-nota then do:
            i-conta-msg = i-conta-msg + 1.
            run utp/ut-msgs.p ('msg',6342,''). 
            create tt_erros_modulo.
            assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                   tt_erros_modulo.cod-erro           = 6342
                   tt_erros_modulo.des-erro           = return-value
                   tt_erros_modulo.identifi-msg       = 
                      string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                      string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                      string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                      string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
      
            undo, leave.
        end.
    end.   


    create item-doc-est.
           
    assign item-doc-est.sequencia      = tt-item-doc-est.sequencia 
           item-doc-est.aliquota-icm   = tt-item-doc-est.aliquota-icm       
           item-doc-est.aliquota-ipi   = tt-item-doc-est.aliquota-ipi       
           item-doc-est.aliquota-iss   = tt-item-doc-est.aliquota-iss       
           item-doc-est.class-fiscal   = tt-item-doc-est.class-fiscal       
           item-doc-est.cod-depos      = tt-item-doc-est.cod-depos
           item-doc-est.cod-emitente   = tt-item-doc-est.cod-emitente      
           item-doc-est.cod-refer      = tt-item-doc-est.cod-refer        
           item-doc-est.desconto[1]    = tt-item-doc-est.desconto[1]
           item-doc-est.despesas[1]    = tt-item-doc-est.despesas[1]     
           item-doc-est.dt-vali-lote   = tt-item-doc-est.dt-vali-lote   
           item-doc-est.lote           = tt-item-doc-est.lote
           item-doc-est.cod-localiz    = tt-item-doc-est.cod-localiz  
           item-doc-est.encerra-pa     = tt-item-doc-est.encerra-pa     
           item-doc-est.it-codigo      = tt-item-doc-est.it-codigo      
           item-doc-est.nat-comp       = tt-item-doc-est.nat-comp       
           item-doc-est.nat-operacao   = tt-item-doc-est.nat-operacao   
           item-doc-est.nr-ord-prod    = tt-item-doc-est.nr-ord-prod        
           item-doc-est.nr-pd-seq      = tt-item-doc-est.nr-pd-seq         
           item-doc-est.nr-pedcli      = tt-item-doc-est.nr-pedcli         
           item-doc-est.nro-comp       = tt-item-doc-est.nro-comp         
           item-doc-est.nro-docto      = tt-item-doc-est.nro-docto        
           item-doc-est.num-pedido     = tt-item-doc-est.num-pedido         
           item-doc-est.numero-ordem   = tt-item-doc-est.numero-ordem      
           item-doc-est.parcela        = tt-item-doc-est.parcela         
           item-doc-est.peso-liquido   = tt-item-doc-est.peso-liquido    
           item-doc-est.preco-total[1] = tt-item-doc-est.preco-total[1]  
           item-doc-est.preco-unit[1]  = tt-item-doc-est.preco-unit[1]   
           item-doc-est.qt-do-forn     = tt-item-doc-est.qt-do-forn      
           item-doc-est.serie-docto    = tt-item-doc-est.serie-docto     
           item-doc-est.valor-ipi[1]   = 0 /*erika 20/07 tt-item-doc-est.valor-ipi[1]*/
           item-doc-est.char-2         = tt-item-doc-est.char-2 /* 30/03 erika */
           item-doc-est.num-sit-trib-icms = tt-item-doc-est.num-sit-trib-icms /* rde - totvs 12 */
           
           /* novos campos ipi e cofins 29/10/2012 - erika */
           item-doc-est.idi-tributac-cofins = tt-item-doc-est.idi-tributac-cofins         
           item-doc-est.val-aliq-cofins     = tt-item-doc-est.val-aliq-cofins             
           item-doc-est.val-base-calc-cofins= tt-item-doc-est.val-base-calc-cofins        
           item-doc-est.val-cofins          = tt-item-doc-est.val-cofins                  
           item-doc-est.idi-tributac-pis    = tt-item-doc-est.idi-tributac-pis            
           item-doc-est.val-aliq-pis        = tt-item-doc-est.val-aliq-pis                
           item-doc-est.base-pis            = tt-item-doc-est.base-pis                    
           item-doc-est.valor-pis           = tt-item-doc-est.valor-pis.
    
    
    
     IF (l-compl-preco AND l-tem-cei = NO) AND es-param-estab.l-cei = YES THEN DO: /*26*/
         

            ASSIGN item-doc-est.preco-total[1]       = item-doc-est.preco-total[1] /* - de-funrural-unit-t 18/04/2017 */
                   item-doc-est.preco-unit[1]        = item-doc-est.preco-total[1]
                   docum-est.valor-mercad            = item-doc-est.preco-total[1]
                   item-doc-est.val-base-calc-cofins = item-doc-est.preco-total[1]
                   item-doc-est.base-pis             = item-doc-est.preco-total[1]
                   docum-est.base-icm                = item-doc-est.preco-total[1]
                   docum-est.tot-valor               = item-doc-est.preco-total[1].

     END.



    /*****Classificaá∆o fiscal n∆o cadastrada 49******/
    find classif-fisc where classif-fisc.class-fiscal = item-doc-est.class-fiscal no-lock no-error.
        if  not avail classif-fisc then do:
        i-conta-msg = i-conta-msg + 1.
        run utp/ut-msgs.p ('msg',6744,''). 
        create tt_erros_modulo.
        assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
               tt_erros_modulo.cod-erro           = 6744
               tt_erros_modulo.des-erro           = return-value
               tt_erros_modulo.identifi-msg       = 
                   string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                   string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                   string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                   string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
    end. 
      
    /*****Item n∆o cadastrado 50*****/  
    find item where item.it-codigo = item-doc-est.it-codigo no-lock no-error.
        if  not avail item then do:
        i-conta-msg = i-conta-msg + 1.
        run utp/ut-msgs.p ('msg',166,''). 
        create tt_erros_modulo.
        assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
               tt_erros_modulo.cod-erro           = 166
               tt_erros_modulo.des-erro           = return-value
               tt_erros_modulo.identifi-msg       = 
                   string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                   string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                   string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                   string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
    end.    
    else 

    /*****Ordem de produá∆o n∆o cadastrada 51*****/
    find familia where item.fm-codigo = familia.fm-codigo no-lock no-error.
    if  item-doc-est.nr-ord-prod <> 0 then do:

        find ord-prod where ord-prod.nr-ord-prod = item-doc-est.nr-ord-prod no-lock no-error.

        if  not avail ord-prod then do:
            i-conta-msg = i-conta-msg + 1.
            run utp/ut-msgs.p ('msg',1879,''). 
            create tt_erros_modulo.
            assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                   tt_erros_modulo.cod-erro           = 1879
                   tt_erros_modulo.des-erro           = return-value
                   tt_erros_modulo.identifi-msg       = 
                      string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                      string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                      string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                     string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
        end.  
        else do: 
        /*****Ordem de produá∆o n∆o pertence ao estabelecimento 52******/                
        if  ord-prod.cod-estabel = docum-est.cod-estabel then do:
            i-conta-msg = i-conta-msg + 1.
            run utp/ut-msgs.p ('msg',6347,''). 
            create tt_erros_modulo.
            assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                   tt_erros_modulo.cod-erro           = 6347
                   tt_erros_modulo.des-erro           = return-value
                   tt_erros_modulo.identifi-msg       = 
                      string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                      string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                      string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                      string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
        end.
                       
        /*****Ordem de produá∆o j  est  finalizada 53*****/              
        if  ord-prod.estado = 7 or ord-prod.estado = 8 then do:
            i-conta-msg = i-conta-msg + 1.
            run utp/ut-msgs.p ('msg',1202,''). 
            create tt_erros_modulo.
            assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                   tt_erros_modulo.cod-erro           = 1202
                   tt_erros_modulo.des-erro           = return-value
                   tt_erros_modulo.identifi-msg       = 
                      string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                      string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                      string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                      string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
        end.  
   
        /*****Ordem de manuten∆o suspensa ou terminada 54*****/                                                    
        if  param-global.modulo-mi then do:

            find ord-manut where ord-manut.nr-ord-prod = ord-prod.nr-ord-prod no-lock no-error.

            if  avail ord-manut then do:
                if  ord-manut.estado-om = 3 or ord-manut.estado-om = 4 then do:
                    i-conta-msg = i-conta-msg + 1.
                    run utp/ut-msgs.p ('msg',5676,''). 
                    create tt_erros_modulo.
                    assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                           tt_erros_modulo.cod-erro           = 5676
                           tt_erros_modulo.des-erro           = return-value
                           tt_erros_modulo.identifi-msg       = 
                              string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                              string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                              string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                              string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
            end. 
        end.
        end.
              
       assign item-doc-est.ct-codigo = ord-prod.ct-codigo
              item-doc-est.sc-codigo = ord-prod.sc-codigo.
       end.
    end.
          
    /*****Pedido n∆o cadastrado 55*****/  
    if  item-doc-est.num-pedido <> 0 then do:
        assign l-compras = yes.
        find pedido-compr where pedido-compr.num-pedido = item-doc-est.num-pedido no-lock no-error.
    
        if  not avail pedido-compr then do: 
            i-conta-msg = i-conta-msg + 1.
            run utp/ut-msgs.p ('msg',6269,''). 
            create tt_erros_modulo.
            assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                   tt_erros_modulo.cod-erro           = 6269
                   tt_erros_modulo.des-erro           = return-value
                   tt_erros_modulo.identifi-msg       = 
                      string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                      string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                      string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                      string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
        end.
         
        /******Pedido eliminado 56*****/ 
        if  avail pedido-compr and pedido-compr.situacao = 3 then do:
            i-conta-msg = i-conta-msg + 1.
            run utp/ut-msgs.p ('msg',1813,''). 
            create tt_erros_modulo.
            assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                   tt_erros_modulo.cod-erro           = 1813
                   tt_erros_modulo.des-erro           = return-value
                   tt_erros_modulo.identifi-msg       = 
                      string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                      string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                      string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                      string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
        end.  

        /*****Ordem invalida 57*****/                   
        if  item-doc-est.numero-ordem = 0 then do:
            i-conta-msg = i-conta-msg + 1.
            run utp/ut-msgs.p ('msg',3138,''). 
            create tt_erros_modulo.
            assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                   tt_erros_modulo.cod-erro           = 3138
                   tt_erros_modulo.des-erro           = return-value
                   tt_erros_modulo.identifi-msg       = 
                      string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                      string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                      string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                      string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
        end.

        /*****Parcela invalida 58*****/
        if  item-doc-est.parcela = 0 then do:
        i-conta-msg = i-conta-msg + 1.
        run utp/ut-msgs.p ('msg',6348,''). 
        create tt_erros_modulo.
        assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
               tt_erros_modulo.cod-erro           = 6348
               tt_erros_modulo.des-erro           = return-value
               tt_erros_modulo.identifi-msg       = 
                  string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                  string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                  string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                  string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
        end. 
     end. 
     
     /*****Ordem compra n∆o cadastrada 59*****/
     if  item-doc-est.numero-ordem <> 0 then do: 

         find b-ordem-compra where b-ordem-compra.numero-ordem = item-doc-est.numero-ordem no-lock no-error.
         if  not avail b-ordem-compra then do:
             i-conta-msg = i-conta-msg + 1.
             run utp/ut-msgs.p ('msg',6349,''). 
             create tt_erros_modulo.
             assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                    tt_erros_modulo.cod-erro           = 6349
                    tt_erros_modulo.des-erro           = return-value
                    tt_erros_modulo.identifi-msg       = 
                       string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                       string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                       string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                       string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
         end.
      
        /*****Ordem de compra com situaá∆o invalida 60*****/                  
        find first b-ordem-compra use-index pedido where b-ordem-compra.num-pedido = item-doc-est.num-pedido
        and b-ordem-compra.numero-ordem = item-doc-est.numero-ordem no-lock no-error.
        if  l-entrada and avail b-ordem-compra and b-ordem-compra.situacao <> 2 then do:
            i-conta-msg = i-conta-msg + 1.
            run utp/ut-msgs.p ('msg',6350,''). 
            create tt_erros_modulo.
            assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                   tt_erros_modulo.cod-erro           = 6350
                   tt_erros_modulo.des-erro           = return-value
                   tt_erros_modulo.identifi-msg       = 
                      string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                      string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                      string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                      string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
        end.     
        else do:                   
            /*****Ordem de compra com situaá∆o invalida para devoluá∆o 61*****/           
            if  avail b-ordem-compra and b-ordem-compra.situacao <> 2 and 
                b-ordem-compra.situacao <> 6 then do:
                i-conta-msg = i-conta-msg + 1.
                run utp/ut-msgs.p ('msg',6352,''). 
                create tt_erros_modulo.
                assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                       tt_erros_modulo.cod-erro           = 6352
                       tt_erros_modulo.des-erro           = return-value
                       tt_erros_modulo.identifi-msg       = 
                          string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                          string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                          string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                          string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
            end. 
        end.
        
          /*****Item possui O.C. de investimento n∆o pode possuir O.P. 62******/                 
        if  avail b-ordem-compra and b-ordem-compra.num-ord-inv <> 0 
                       and item-doc-est.nr-ord-prod <> 0  then do:
            i-conta-msg = i-conta-msg + 1.
            run utp/ut-msgs.p ('msg',6353,''). 
            create tt_erros_modulo.
            assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                   tt_erros_modulo.cod-erro           = 6353
                   tt_erros_modulo.des-erro           = return-value
                   tt_erros_modulo.identifi-msg       = 
                      string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                      string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                      string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                      string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
        end. 
                
        /*****Pedido inv†lido 63*****/
        if  item-doc-est.num-pedido = 0 then do:
            i-conta-msg = i-conta-msg + 1.
            run utp/ut-msgs.p ('msg',6354,''). 
            create tt_erros_modulo.
            assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                   tt_erros_modulo.cod-erro           = 6354
                   tt_erros_modulo.des-erro           = return-value
                   tt_erros_modulo.identifi-msg       = 
                      string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                      string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                      string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                      string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
        end.
 
        /*****Parcela inv†lida 64*****/               
        if  item-doc-est.parcela = 0 then do:
            i-conta-msg = i-conta-msg + 1.
            run utp/ut-msgs.p ('msg',6348,''). 
            create tt_erros_modulo.
            assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                   tt_erros_modulo.cod-erro           = 6348
                   tt_erros_modulo.des-erro           = return-value
                   tt_erros_modulo.identifi-msg       = 
                      string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                      string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                      string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                      string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
        end. 
     end. 

     /*****Parcela n∆o cadastrada 65*****/ 
     if  item-doc-est.parcela <> 0 then do:
         find prazo-compra where prazo-compra.numero-ordem = item-doc-est.numero-ordem and
                            prazo-compra.parcela      = item-doc-est.parcela no-lock no-error.
         if  not avail prazo-compra then do:
             i-conta-msg = i-conta-msg + 1.
             run utp/ut-msgs.p ('msg',6357,''). 
             create tt_erros_modulo.
             assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                    tt_erros_modulo.cod-erro           = 6357
                    tt_erros_modulo.des-erro           = return-value
                    tt_erros_modulo.identifi-msg       = 
                       string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                       string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                       string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                       string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
         end. 
                  
         /*****Parcela com situaá∆o inv†lida 66*****/                 
         if  l-entrada and avail prazo-compra and prazo-compra.situacao <> 2 then do:
             i-conta-msg = i-conta-msg + 1.
             run utp/ut-msgs.p ('msg',6359,''). 
             create tt_erros_modulo.
             assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                    tt_erros_modulo.cod-erro           = 6359
                    tt_erros_modulo.des-erro           = return-value
                    tt_erros_modulo.identifi-msg       = 
                       string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                       string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                       string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                       string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
         end. 
         else do:
                   
             /*****Parcela com situaá∆o inv†lida para devoluá∆o 67*****/
             if  avail prazo-compra and prazo-compra.situacao <> 2 and 
                 prazo-compra.situacao <> 6 then do:
                 i-conta-msg = i-conta-msg + 1.
                 run utp/ut-msgs.p ('msg',6360,''). 
                 create tt_erros_modulo.
                 assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                        tt_erros_modulo.cod-erro           = 6360
                        tt_erros_modulo.des-erro           = return-value
                        tt_erros_modulo.identifi-msg       = 
                        string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                        string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                        string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                        string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
             end.               
         end.  

        /*****Pedido compra inv†lido 68*****/  
        if  item-doc-est.num-pedido = 0 then do:
            i-conta-msg = i-conta-msg + 1.
            run utp/ut-msgs.p ('msg',6363,''). 
            create tt_erros_modulo.
            assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                   tt_erros_modulo.cod-erro           = 6363
                   tt_erros_modulo.des-erro           = return-value
                   tt_erros_modulo.identifi-msg       = 
                      string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                      string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                      string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                      string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
        end. 

       /*****Ordem compra inv†lida 69*****/
       if  item-doc-est.numero-ordem = 0 then do:
           i-conta-msg = i-conta-msg + 1.
           run utp/ut-msgs.p ('msg',6364,''). 
           create tt_erros_modulo.
           assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                  tt_erros_modulo.cod-erro           = 6364
                  tt_erros_modulo.des-erro           = return-value
                  tt_erros_modulo.identifi-msg       = 
                     string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                     string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                     string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                     string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
       end. 
                
       assign de-qtd-tot-ordem = 0.
       for each prazo-compra
                where prazo-compra.numero-ordem = item-doc-est.numero-ordem no-lock:
           assign de-qtd-tot-ordem = de-qtd-tot-ordem + prazo-compra.quantidade.
       end.
     end. 
     
     if  can-find(first tt_erros_modulo where
                       tt_erros_modulo.identifi-msg = string(tt-docum-est.serie-docto)  + chr(24) +
                                                      string(tt-docum-est.nro-docto)    + chr(24) +
                                                      string(tt-docum-est.cod-emitente) + chr(24) +
                                                      string(tt-docum-est.nat-operacao)) then
     undo, next.
            
     if  (can-do("4,1",string(item.tipo-contr)) and (item-doc-est.nr-ord-prod = 0 
          or  (l-remessa and item-doc-est.baixa-ce 
               and item-doc-est.nr-ord-prod = 0)))
          or  (item.tipo-contr = 2 and l-devolucao = no and
               item-doc-est.nr-ord-prod = 0)
          or  ((l-retorno or l-remessa) and item-doc-est.nr-ord-prod = 0) 
          or  (avail b-ordem-compra and b-ordem-compra.num-ord-inv <> 0) 
          or  (l-sai-cons and (not item-doc-est.baixa-ce and
               item.tipo-contr <> 3)) then do:
               assign
               item-doc-est.ct-codigo = if  item-doc-est.ct-codigo <>
                                                ct-initial then
                                                    item-doc-est.ct-codigo
                                                else
                                                if  (l-retorno 
                                                and item-doc-est.nr-ord-prod = 0)
                                                then i-ct-componente
                                                else
                                                if avail b-ordem-compra
                                                then b-ordem-compra.ct-codigo
                                                else item.ct-codigo
               item-doc-est.sc-codigo = if  item-doc-est.sc-codigo <>
                                                sc-initial then
                                                    item-doc-est.sc-codigo
                                                else
                                                if  (l-retorno 
                                                and item-doc-est.nr-ord-prod = 0)
                                                then i-sc-componente
                                                else
                                                if avail b-ordem-compra
                                                then b-ordem-compra.sc-codigo
                                                else item.sc-codigo.
               if can-do("4,1",string(item.tipo-contr))
               or item-doc-est.ct-codigo <> ct-initial
               or item-doc-est.sc-codigo <> sc-initial
               or item-doc-est.baixa-ce = no then do: 


                /*LOCALIZANDO PLANO CONTAS PRINCIPAL*/
                  FIND FIRST plano_cta_unid_organ
                       WHERE plano_cta_unid_organ.cod_unid_organ         = v_cod_empres_usuar
                         AND plano_cta_unid_organ.ind_tip_plano_cta_ctbl = "Prim†rio"
                         AND plano_cta_unid_organ.dat_inic_valid         <= TODAY
                         AND plano_cta_unid_organ.dat_fim_valid          >= TODAY 
                             NO-LOCK NO-ERROR. 
                  IF AVAIL plano_cta_unid_organ THEN DO:
                      FIND FIRST plano_cta_ctbl 
                           WHERE plano_cta_ctbl.cod_plano_cta_ctbl = plano_cta_unid_organ.cod_plano_cta_ctbl
                                 NO-LOCK NO-ERROR.
                  END.

                  FIND FIRST cta_ctbl 
                       WHERE cta_ctbl.cod_plano_cta_ctbl = (IF AVAIL plano_cta_ctbl THEN plano_cta_ctbl.cod_plano_cta_ctbl ELSE "PLA01")
                         AND cta_ctbl.cod_cta_ctbl       = item-doc-est.conta-contabil
                             NO-LOCK NO-ERROR.
             /*   FIND conta-contab WHERE 
                       conta-contab.ep-codigo = param-global.empresa-prin AND 
                       conta-contab.ct-codigo = item-doc-est.ct-codigo AND 
                       conta-contab.sc-codigo = item-doc-est.sc-codigo
                              no-lock no-error. */
               
                  /*****Conta contabil nío cadastrada 70*****/                              
                  IF NOT avail cta_ctbl THEN DO:       /* if not avail conta-contab then do: */
                     i-conta-msg = i-conta-msg + 1.
                     run utp/ut-msgs.p ('msg',1060,''). 
                     create tt_erros_modulo.
                     assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                            tt_erros_modulo.cod-erro           = 1060
                            tt_erros_modulo.des-erro           = return-value
                            tt_erros_modulo.identifi-msg       = 
                               string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                               string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                               string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                               string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
                  end.                                


                 /*****Conta n∆o ≤ de sistema 71*****/ 
                 IF AVAIL cta_ctbl AND cta_ctbl.ind_espec_cta_ctbl <> "Anal°tica" THEN DO: /*if avail conta-contab and conta-contab.estado <> 3 then do:*/
                    i-conta-msg = i-conta-msg + 1.
                    run utp/ut-msgs.p ('msg',443,''). 
                    create tt_erros_modulo.
                    assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                           tt_erros_modulo.cod-erro           = 443
                           tt_erros_modulo.des-erro           = return-value
                           tt_erros_modulo.identifi-msg       = 
                              string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                              string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                              string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                              string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
                 end. 
                        
                 /*****Conta deve ser de despesa receita, ativo ou passivo 72*****/                        
                 IF AVAIL cta_ctbl AND NOT CAN-DO ("1,2,3,4",STRING (cta_ctbl.cod_grp_cta_ctbl)) THEN DO:   /* if avail conta-contab and not can-do("1,2,4,5",string(conta-contab.tipo)) then do: */
                    i-conta-msg = i-conta-msg + 1.
                    run utp/ut-msgs.p ('msg',6365,''). 
                    create tt_erros_modulo.
                    assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                           tt_erros_modulo.cod-erro           = 6365
                           tt_erros_modulo.des-erro           = return-value
                           tt_erros_modulo.identifi-msg       = 
                              string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                              string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                              string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                              string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
                 end.
                    
                 /* 
                 /*****Conta n∆o permite lanªamento de estoque 73*****/                        
                 if avail conta-contab and conta-contab.estoq = 0 then do:
                    i-conta-msg = i-conta-msg + 1.
                    run utp/ut-msgs.p ('msg',445,''). 
                    create tt_erros_modulo.
                    assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                           tt_erros_modulo.cod-erro           = 445
                           tt_erros_modulo.des-erro           = return-value
                           tt_erros_modulo.identifi-msg       = 
                              string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                              string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                              string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                              string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
                 end. 
                 */
 
                 /*
                 /*****Conta n∆o aceita lanªamentos referentes a requisiªÑes 74*****/
                 if avail conta-contab and not can-do("1,2,3,5,6",string(conta-contab.estoq)) then do:
                    i-conta-msg = i-conta-msg + 1.
                    run utp/ut-msgs.p ('msg',107,''). 
                    create tt_erros_modulo.
                    assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                           tt_erros_modulo.cod-erro           = 107
                           tt_erros_modulo.des-erro           = return-value
                           tt_erros_modulo.identifi-msg       = 
                              string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                              string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                              string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                              string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
                 end.
                 */ 
                 
         end.

     end.
        
     find prazo-compra where 
          prazo-compra.numero-ordem = item-doc-est.numero-ordem and
          prazo-compra.parcela      = item-doc-est.parcela no-lock no-error.
     assign de-indice = 1.
     if item.tipo-contr <> 4 then do:
        find item-fornec where item-fornec.it-codigo = item-doc-est.it-codigo and
                                     item-fornec.cod-emite = item-doc-est.cod-emite no-lock no-error.
        if available item-fornec then do:
           assign de-indice = item-fornec.fator-conver / exp(10,item-fornec.num-casa-dec)
                  c-un      = item-fornec.unid-med-for. 
        end.
        else
            assign c-un      = item.un.
     end.
     else
     if avail b-ordem-compra then do:
        find cotacao-item where 
        cotacao-item.numero-ordem = b-ordem-compra.numero-ordem
        and cotacao-item.cod-emitente = b-ordem-compra.cod-emitente
        and cotacao-item.data-cotacao = b-ordem-compra.data-cotacao no-lock no-error.
        if available cotacao-item then do:
           assign c-un      = cotacao-item.un.
           find tab-conv-un where tab-conv-un.unid-med-for = cotacao-item.un
                              and tab-conv-un.un           = prazo-compra.un no-lock no-error.
           if available tab-conv-un then
           assign de-indice = tab-conv-un.fator-conver / exp(10,tab-conv-un.num-casa-dec).
        end.
        else assign  c-un      = item.un.
     end.
   
     assign de-qtd-estrut   = de-qtd-estrut * de-indice
                              item-doc-est.un = c-un.
     if  not l-devolucao and not l-transf and l-compras then 
     assign de-aux-1 = round(prazo-compra.quant-saldo +
            (de-qtd-tot-ordem * familia.var-qtd-perm / 100), 4)
            de-aux-2 = round(prazo-compra.quant-saldo - (de-qtd-tot-ordem * familia.var-qtd-perm / 100), 4).
     if (l-entrada and l-retorno = no and l-sai-cons = no) and
         not(l-entrada and docum-est.esp-docto = 20) then do:
         assign item-doc-est.quantidade = item-doc-est.qt-do-forn / de-indice.
     end. 
               
     /*Quantidade estaãfora da variaá∆o permitida 74*****/  
     if l-entrada and not l-devolucao and not l-transf and l-compras then do:
        if item-doc-est.quantidade > de-aux-1 and param-re.aceita-var = no then do:
           i-conta-msg = i-conta-msg + 1.
           run utp/ut-msgs.p ('msg',5928,''). 
           create tt_erros_modulo.
           assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                  tt_erros_modulo.cod-erro           = 5928
                  tt_erros_modulo.des-erro           = return-value
                  tt_erros_modulo.identifi-msg       = 
                     string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                     string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                     string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                     string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
        end. 
     end. 

     /*****Quantidade n∆o pode ser fracionada 75*****/ 
     if not item.fraciona then do:
        if integer(item-doc-est.quantidade) <> item-doc-est.quantidade then do:
           i-conta-msg = i-conta-msg + 1.
           run utp/ut-msgs.p ('msg',6381,''). 
           create tt_erros_modulo.
           assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                  tt_erros_modulo.cod-erro           = 6381
                  tt_erros_modulo.des-erro           = return-value
                  tt_erros_modulo.identifi-msg       = 
                     string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                     string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                     string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                     string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
        end.     
     end.

     /*****Quantidade informada n∆o pode ser igual a 0 (zero) 76*****/
     if l-remessa or l-ent-cons or (l-transf and not l-entrada) then do:
        if item-doc-est.quantidade = 0 then do:
           i-conta-msg = i-conta-msg + 1.
           run utp/ut-msgs.p ('msg',1521,''). 
           create tt_erros_modulo.
           assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                  tt_erros_modulo.cod-erro           = 1521
                  tt_erros_modulo.des-erro           = return-value
                  tt_erros_modulo.identifi-msg       = 
                     string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                     string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                     string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                     string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
        end. 
     end.  

     /*****Deposito n∆o cadastrado 77*****/
     find deposito where deposito.cod-depos = item-doc-est.cod-depos no-lock no-error.
     if not avail deposito then do:
         i-conta-msg = i-conta-msg + 1.
         run utp/ut-msgs.p ('msg',530,''). 
         create tt_erros_modulo.
         assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                tt_erros_modulo.cod-erro           = 530
                tt_erros_modulo.des-erro           = return-value
                tt_erros_modulo.identifi-msg       = 
                   string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                   string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                   string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                   string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
     end. 
  
     /*****n∆o pode efetuar saida de CQ sem aprovaá∆o do controle de qualidade 78*****/              
     if  avail deposito and natur-oper.tipo = 2 
         and (deposito.ind-dep-cq = yes or deposito.ind-dep-rej = yes) 
         and item.contr-qualid = yes 
         and param-global.modulo-cq and param-cq.tipo-cq > 0 then do:
         i-conta-msg = i-conta-msg + 1.
         run utp/ut-msgs.p ('msg',6382,''). 
         create tt_erros_modulo.
         assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                tt_erros_modulo.cod-erro           = 6382
                tt_erros_modulo.des-erro           = return-value
                tt_erros_modulo.identifi-msg       = 
                   string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                   string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                   string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                   string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
     end. 
    
              
     /*****Localizaá∆o unica para o item 79****/
     if  avail item and item.loc-unica and item.cod-localiz <> item-doc-est.cod-localiz then do:
         i-conta-msg = i-conta-msg + 1.
         run utp/ut-msgs.p ('msg',6383,''). 
         create tt_erros_modulo.
         assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                tt_erros_modulo.cod-erro           = 6383
                tt_erros_modulo.des-erro           = return-value
                tt_erros_modulo.identifi-msg       = 
                   string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                   string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                   string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                   string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
     end. 
    
     /*****Lote deve ser diferente de branco 80*****/
     if  avail item and item.tipo-con-est <> 1 and 
         item-doc-est.lote = "" then do:
         i-conta-msg = i-conta-msg + 1.
         run utp/ut-msgs.p ('msg',1818,''). 
         create tt_erros_modulo.
         assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                tt_erros_modulo.cod-erro           = 1818
                tt_erros_modulo.des-erro           = return-value
                tt_erros_modulo.identifi-msg       = 
                   string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                   string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                   string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                   string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
     end.
 
     /*****Item controlado por numero de serie j  existente em outra sequencia do documento 81*****/              
     if item.tipo-con-est = 2 and item-doc-est.quantidade <> 0 then do:
        find first b-item-doc-est where
            b-item-doc-est.serie-docto =  docum-est.serie-docto AND
            b-item-doc-est.nro-docto =  docum-est.nro-docto AND
            b-item-doc-est.cod-emitente =  docum-est.cod-emitente AND
            b-item-doc-est.nat-operacao =  docum-est.nat-operacao 

            and b-item-doc-est.it-codigo = item.it-codigo
            and b-item-doc-est.lote      = item-doc-est.lote
            and b-item-doc-est.sequencia <> item-doc-est.sequencia no-lock no-error.
        if avail b-item-doc-est then do:
           i-conta-msg = i-conta-msg + 1.
           run utp/ut-msgs.p ('msg',6388,''). 
           create tt_erros_modulo.
           assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                  tt_erros_modulo.cod-erro           = 6388
                  tt_erros_modulo.des-erro           = return-value
                  tt_erros_modulo.identifi-msg       = 
                     string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                     string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                     string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                     string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
        end.         
        /*****Item controlado por numero de serie j  existente em saldo de estoque 82*****/  
        find first b-saldo-estoq use-index lote where 
           b-saldo-estoq.lote = item-doc-est.lote
           and b-saldo-estoq.it-codigo = item.it-codigo
           and b-saldo-estoq.qtidade-atu <> 0 no-lock no-error.
        if avail b-saldo-estoq and ((b-saldo-estoq.qtidade-atu < 0 and  natur-oper.tipo = 2)
                         or (b-saldo-estoq.qtidade-atu > 0 and natur-oper.tipo = 1)) then do:
           i-conta-msg = i-conta-msg + 1.
           run utp/ut-msgs.p ('msg',1252,''). 
           create tt_erros_modulo.
           assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                  tt_erros_modulo.cod-erro           = 1252
                  tt_erros_modulo.des-erro           = return-value
                  tt_erros_modulo.identifi-msg       = 
                     string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                     string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                     string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                     string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
        end. 
     end.
  
     if natur-oper.tipo = 2 and item.tipo-contr <> 4 
                                and item-doc-est.baixa-ce and 
        not((l-entrada = no and not l-transf and not l-remessa and
        not l-sai-cons) and item-doc-est.nr-ord-prod <> 0) then do:
        find saldo-estoq use-index estabel-dep where
             saldo-estoq.cod-estab = docum-est.cod-estab        and 
             saldo-estoq.cod-depos = item-doc-est.cod-depos     and
             saldo-estoq.cod-localiz = item-doc-est.cod-localiz and
             saldo-estoq.lote = item-doc-est.lote               and
             saldo-estoq.it-codigo = item-doc-est.it-codigo     and
             saldo-estoq.cod-refer = item-doc-est.cod-refer no-lock no-error.
             de-quant = if avail saldo-estoq then
                        saldo-estoq.qtidade-atu -
                        saldo-estoq.qt-alocada  -
                        saldo-estoq.qt-aloc-ped -
                        saldo-estoq.qt-aloc-prod
                        else 0.
                    
        /*****Quantidade de saida maior que a disponivel em estoque 83*****/                       
        if l-consiste-saldo and item-doc-est.quantidade  > de-quant 
                    and item-doc-est.quantidade <> 0  
                    and item.perm-saldo-neg = 0 then do:
           i-conta-msg = i-conta-msg + 1.
           run utp/ut-msgs.p ('msg',5266,''). 
           create tt_erros_modulo.
           assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                  tt_erros_modulo.cod-erro           = 5266
                  tt_erros_modulo.des-erro           = return-value
                  tt_erros_modulo.identifi-msg       = 
                     string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                     string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                     string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                     string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
        end. 
     end.
      
        /*****Referància do lote n∆o confere com a referància informada 84*****/
     if item.tipo-con-est = 4 then do:
        find first saldo-estoq where saldo-estoq.it-codigo   = item-doc-est.it-codigo 
                                and saldo-estoq.lote        = item-doc-est.lote no-lock no-error.
        if avail saldo-estoq then do:
           i-conta-msg = i-conta-msg + 1.
           run utp/ut-msgs.p ('msg',1785,''). 
           create tt_erros_modulo.
           assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                  tt_erros_modulo.cod-erro           = 1785
                  tt_erros_modulo.des-erro           = return-value
                  tt_erros_modulo.identifi-msg       = 
                     string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                     string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                     string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                     string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
        end. 
     end. 

           /*****Somat¢ria dos pesos dos itens ≤ maior que peso toatal do documento 85*****/  
     if param-re.rateia-frete = 1 and docum-est.valor-frete > 0 and not l-servicos then do:
        assign de-tot-peso = 0.
        for each b-item-doc-est where
                b-item-doc-est.serie-docto =  docum-est.serie-docto AND
                b-item-doc-est.nro-docto =  docum-est.nro-docto AND
            b-item-doc-est.cod-emitente =  docum-est.cod-emitente AND
            b-item-doc-est.nat-operacao =  docum-est.nat-operacao :

            assign de-tot-peso = de-tot-peso + b-item-doc-est.peso-liquido.
        end.
        assign item-doc-est.peso-liquido = item.peso-liquido
                                          * item-doc-est.quantidade
                            de-tot-peso = de-tot-peso + item-doc-est.peso-liquido.
        if de-tot-peso > docum-est.tot-peso then do:
           i-conta-msg = i-conta-msg + 1.
           run utp/ut-msgs.p ('msg',6394,''). 
           create tt_erros_modulo.
           assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                  tt_erros_modulo.cod-erro           = 6394
                  tt_erros_modulo.des-erro           = return-value
                  tt_erros_modulo.identifi-msg       = 
                     string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                     string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                     string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                     string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
        end.    
     end.
     else do:
        if  l-servicos then 
        assign item-doc-est.peso-liquido = tt-item-doc-est.peso-liquido.
        if  l-servicos = no then do:
            assign item-doc-est.peso-liquido = item.peso-liquido
                                                  * item-doc-est.quantidade.
        end.
     end.
  
     if  param-re.rateia-desc and natur-oper.tipo = 1 
           and not l-ent-cons and not l-retorno
           and not l-devolucao and not natur-oper.transf then do:
  
        assign item-doc-est.desconto[1] = 
                         truncate(item-doc-est.preco-total[1] *
                         docum-est.tot-desconto *
                         integer(docum-est.valor-mercad <> 0) /
                         (docum-est.valor-mercad +
                         integer(docum-est.valor-mercad = 0)),2).
  
        for each b-item-doc-est  where
            b-item-doc-est.serie-docto  =  docum-est.serie-docto AND
            b-item-doc-est.nro-docto    =  docum-est.nro-docto AND
            b-item-doc-est.cod-emitente =  docum-est.cod-emitente AND
            b-item-doc-est.nat-operacao =  docum-est.nat-operacao 
                  no-lock:
                  accum b-item-doc-est.preco-total(total).
                  accum b-item-doc-est.desconto(total).
        end.
        assign de-aux-1 = accum total b-item-doc-est.preco-total[1].
                     de-aux-2 = accum total b-item-doc-est.desconto[1].
  
        if de-aux-1 = docum-est.valor-mercad then
        if de-aux-2 <> docum-est.tot-desconto then
           assign item-doc-est.desconto[1] = item-doc-est.desconto[1] +
                                             docum-est.tot-desconto - de-aux-2.
  
        if item-doc-est.desconto[1] = ? then
                 assign item-doc-est.desconto[1] = 0.
     end.
     
     if  can-find(first tt_erros_modulo where
                       tt_erros_modulo.identifi-msg = string(tt-docum-est.serie-docto)  + chr(24) +
                                                      string(tt-docum-est.nro-docto)    + chr(24) +
                                                      string(tt-docum-est.cod-emitente) + chr(24) +
                                                      string(tt-docum-est.nat-operacao)) then
        undo, next.
     

     
     FIND FIRST item-doc-est OF tt-item-doc-est EXCLUSIVE-LOCK NO-ERROR.
     

    {utp/ut-glob.i}
    
    def new global shared var r-RE0301-documento as rowid no-undo.
    def new global shared var c-RE0301-origem as character no-undo.
    def new global shared var c-RE0301-usuario like param-re.usuario no-undo.
    
        
    find first param-global no-lock no-error.
    find first param-estoq no-lock no-error.
    find first param-compra no-lock no-error.
    
    if param-estoq.moeda1 <> 0 then 
    find  mgcad.moeda where  mgcad.moeda.mo-codigo = param-estoq.moeda1.
    if avail  mgcad.moeda THEN c-mo-1 =  mgcad.moeda.descricao.
    if param-estoq.moeda2 <> 0 then 
    find  mgcad.moeda where  mgcad.moeda.mo-codigo = param-estoq.moeda2.
    if avail  mgcad.moeda then c-mo-2 =  mgcad.moeda.descricao.


 {rep/re0152.i}
  
    find first tt-docum-est where 
               tt-docum-est.nro-docto    = docum-est.nro-docto
         and   tt-docum-est.cod-emitente = docum-est.cod-emitente
         and   tt-docum-est.serie-docto  = docum-est.serie-docto
         and   tt-docum-est.nat-operacao = docum-est.nat-operacao no-lock no-error.
      
    if not l-servicos then do:
      
       assign l-resposta = yes.
       {rep/re0301.i02 ipi}
       {rep/re0301.i02 icm}
      
       
           
       find classif-fisc
       where classif-fisc.class-fiscal = item-doc-est.class-fiscal
       no-lock no-error.
      
       assign de-IPI-item = 0
              de-IPI-outras-item = 0
              de-IPI-ntrib-item = 0.
      
       /* Calculo do IPI */
       run rep/re9992.p (input rowid(item-doc-est)).
      
       /* Definicao da aliquota de icms */
       run rep/re9991.p (rowid(docum-est), output de-aliq-icm).
           
       /*****Valor de IPI item difere do calculodo " item-doc-est.sequencia" 97*****/      
       if  item-doc-est.valor-ipi[1] <> de-IPI-item
       then do:
              i-conta-msg = i-conta-msg + 1.
              run utp/ut-msgs.p ('msg',6424,''). 
              create tt_erros_modulo.
              assign tt_erros_modulo.num-sequencia-erro = i-conta-msg
                     tt_erros_modulo.cod-erro           = 6424
                     tt_erros_modulo.des-erro           = return-value
                     tt_erros_modulo.identifi-msg       = 
                        string(if string(tt-docum-est.serie-docto)    = ? then ' ' else string(tt-docum-est.serie-docto))  + chr(24) +
                        string(if string(tt-docum-est.nro-docto)      = ? then ' ' else string(tt-docum-est.nro-docto))    + chr(24) +
                        string(if string(tt-docum-est.cod-emitente)   = ? then ' ' else string(tt-docum-est.cod-emitente)) + chr(24) +
                        string(if string(tt-docum-est.nat-operacao)   = ? then ' ' else string(tt-docum-est.nat-operacao)).
       end. 
                
       assign item-doc-est.valor-ipi[1]  = 0 
              item-doc-est.base-ipi[1]   = 0 
              item-doc-est.ipi-ntrib[1]  = 0 
              item-doc-est.ipi-outras[1] = 0 
              item-doc-est.aliquota-icm  = de-aliq-icm
              l-resposta = yes.
      
       /* Impresso Personalizado */
       if  item.tipo-contr <> 4
       and item.cd-trib-iss = 1
       and item.aliquota-iss > 0 then do:
       assign item-doc-est.aliquota-iss = item.aliquota-iss
       item-doc-est.cd-trib-iss  = item.cd-trib-iss.
    end.
 
        if  l-transf and param-re.inclui-icm
        and item.tipo-contr = 2 then
            run rep/re0301e6.p (rowid(item-doc-est)). /*este prog. sera subsituido.*/
  
     end.
     else do:
        if item.cd-trib-iss = 2 then 
           assign item-doc-est.cd-trib-iss = 2.
        else do:
           assign item-doc-est.cd-trib-iss = 3
                  item-doc-est.aliquota-iss = item.aliquota-iss.
        end.
     end.
    
  end.

   if  can-find(first tt_erros_modulo where
                      tt_erros_modulo.cod-erro     <> 5268 AND     
                      tt_erros_modulo.identifi-msg  = string(tt-docum-est.serie-docto)  + chr(24) +
                                                      string(tt-docum-est.nro-docto)    + chr(24) +
                                                      string(tt-docum-est.cod-emitente) + chr(24) +
                                                      string(tt-docum-est.nat-operacao)) THEN DO:
             
             undo, next.
   END. /* tt-_erros */
   
   
   
   FIND FIRST docum-est OF tt-docum-est.
   
   run esp/esreap152a.p (input rowid(docum-est),
                       input-output i-conta-msg,
                       input-output table tt_erros_modulo,
                       input table tt-docum-est).         

   FIND FIRST tt-item-doc-est OF docum-est NO-ERROR.
   FIND FIRST item-doc-est OF docum-est NO-ERROR.

   IF AVAIL item-doc-est then
     ASSIGN item-doc-est.narrativa  = tt-item-doc-est.narrativa.


FIND FIRST docum-est
     WHERE docum-est.nro-docto    = tt-docum-est.nro-docto
       AND docum-est.serie        = tt-docum-est.serie
       AND docum-est.nat-operacao = tt-docum-est.nat-operacao NO-ERROR.


FIND FIRST item-doc-est NO-ERROR.
FOR EACH tt-dupli:
    DELETE tt-dupli.
END.


RUN esp/es4010fa.p (INPUT (de-total-t),    
                    INPUT (pedido-compr.num-pedido),
                    INPUT (item-doc-est.serie-docto),
                    INPUT (item-doc-est.nro-docto),
                    INPUT (item-doc-est.cod-emitente),
                    INPUT (item-doc-est.nat-operacao), 
                    INPUT (item-doc-est.numero-ordem),
                    INPUT-OUTPUT TABLE tt-dupli).

/****************
IF de-cdo-unit > 0           AND
   es-param-estab.l-dupl-cdo THEN DO:

    ASSIGN da-vencimento = date(MONTH(TODAY),01,YEAR(TODAY)) + 32
           da-vencimento = date(MONTH(da-vencimento),es-param-empresa.u-int-1,YEAR(da-vencimento)).
    CREATE dupli-apagar.
    ASSIGN dupli-apagar.cod-emitente = tt-docum-est.cod-emitente
           dupli-apagar.serie-docto  = tt-docum-est.serie-docto
           dupli-apagar.cod-esp      = es-param-estab.cod-esp-cdo
           dupli-apagar.nro-docto    = tt-docum-est.nro-docto
           dupli-apagar.nr-duplic    = tt-docum-est.nro-docto
           dupli-apagar.parcela      = "11"
           dupli-apagar.dt-emissao   = TODAY
           dupli-apagar.dt-trans     = TODAY
           dupli-apagar.dt-vencim    = da-vencimento
           dupli-apagar.valor        = de-cdo-unit-t
           dupli-apagar.nat-operacao = tt-docum-est.nat-operacao
           dupli-apagar.cod-estabel  = tt-docum-est.cod-estabel
           dupli-apagar.ep-codigo    = i-ep-codigo-usuario
           dupli-apagar.tp-despesa   = ITEM.tp-desp-padrao
           dupli-apagar.vl-a-pagar   = de-cdo-unit-t
           dupli-apagar.esp-movto    = 1
           dupli-apagar.estado       = 1.

END.
*****************/

IF b-es-param-empresa.mp-ativa THEN DO:
    
    FIND FIRST tt-dupli exclusive-LOCK.
    ASSIGN de-soma-mp         = tt-dupli.vl-apagar /* - de-cdo-unit-t */ - de-funrural-unit-t - de-senar-unit-t
           tt-dupli.vl-apagar = de-soma-mp. 

    IF de-irrf-unit > 0 THEN DO:
    ASSIGN da-vencimento = es-param-empres.u-date-1.
    
    ASSIGN de-irrf-unit = ROUND(de-soma-mp * (es-param-empresa.perc-irrf / 100),2). /* alt round de 2 para 1  -04/11/12- voltei 12/11/12 */
    ASSIGN es-mp232-emit.vl-irrf  = de-irrf-unit
           es-mp232-emit.gerou       = yes
           es-mp232-emit.dt-geracao  = TODAY
           es-mp232-emit.nota-geracao = STRING(num-nota).

    /* 290105 */
    find FIRST item-doc-est OF docum-est EXCLUSIVE-LOCK no-error.
    if NOT avail item-doc-est THEN
        
    IF l-compl-preco = NO THEN do:
        ASSIGN item-doc-est.preco-total = de-soma-mp.  

    END.

   CREATE dupli-imp.
   ASSIGN dupli-imp.cod-emitente  = tt-docum-est.cod-emitente
          dupli-imp.serie-docto   = tt-docum-est.serie-docto
          dupli-imp.cod-esp       = es-param-empresa.espec-irrf
          dupli-imp.nro-docto     = tt-docum-est.nro-docto
          dupli-imp.nr-duplic     = tt-docum-est.nro-docto
          dupli-imp.nro-docto-imp = tt-docum-est.nro-docto
          dupli-imp.parcela       = "1" 
          dupli-imp.parcela-imp   = dupli-imp.parcela
          dupli-imp.cod-tax       = 0 
          dupli-imp.dt-venc-imp   = da-vencimento            
          dupli-imp.aliquota      = es-param-empresa.perc-irrf
          dupli-imp.cod-forn-imp  = int(dupli-imp.parcela)
          dupli-imp.nat-operacao  = tt-docum-est.nat-operacao
          dupli-imp.rend-trib     = tt-dupli.vl-apagar
          dupli-imp.tp-codigo     = 999
          dupli-imp.tp-imposto    = 1
          dupli-imp.vl-imposto    = de-irrf-unit
          dupli-imp.int-1         = IF (emitente.natureza = 2) THEN 14 ELSE 13
          dupli-imp.cod-retencao  = IF (emitente.natureza = 2) THEN 3842 ELSE 3850.

    END.

    IF de-cofins-unit > 0 THEN DO:
    ASSIGN da-vencimento = es-param-empres.u-date-1.
    
    ASSIGN de-cofins-unit = ROUND(de-soma-mp * (es-param-empresa.perc-cofins / 100),2). /* alt round de 2 para 1  -04/11/12- voltei 12/11/12 */
    ASSIGN es-mp232-emit.vl-cofins    = de-cofins-unit
           es-mp232-emit.gerou        = yes
           es-mp232-emit.dt-geracao   = TODAY
           es-mp232-emit.nota-geracao = STRING(num-nota).
   

   CREATE dupli-imp.
   ASSIGN dupli-imp.cod-emitente  = tt-docum-est.cod-emitente
          dupli-imp.serie-docto   = tt-docum-est.serie-docto
          dupli-imp.cod-esp       = es-param-empresa.espec-cofins
          dupli-imp.nro-docto     = tt-docum-est.nro-docto
          dupli-imp.nr-duplic     = tt-docum-est.nro-docto
          dupli-imp.nro-docto-imp = tt-docum-est.nro-docto
          dupli-imp.parcela       = "2" /*"14"*/
          dupli-imp.parcela-imp   = dupli-imp.parcela
          dupli-imp.cod-tax       = 0 
          dupli-imp.dt-venc-imp   = da-vencimento            
          dupli-imp.aliquota      = es-param-empresa.perc-cofins
          dupli-imp.cod-forn-imp  = int(dupli-imp.parcela)
          dupli-imp.nat-operacao  = tt-docum-est.nat-operacao
          dupli-imp.rend-trib     = tt-dupli.vl-apagar
          dupli-imp.tp-codigo     = 999
          dupli-imp.tp-imposto    = 1
          dupli-imp.vl-imposto    = de-irrf-unit
          dupli-imp.int-1         = 13
          dupli-imp.cod-retencao  = 1708.


    END.
END. /* ativa mp */



ASSIGN i-parcela-dp = 0.
FOR EACH tt-dupli:
    
    /* inicio - nova interpretaá∆o calend†rio comercial s¢ para DP */
    ASSIGN da-vencto-dp = tt-dupli.dt-vencimen.
    achou-dia-util:
    DO WHILE l-ok = no: 
        FIND FIRST  calen-coml WHERE
                    calen-coml.ep-codigo   = i-ep-codigo-usuario AND
                    calen-coml.cod-estabel = docum-est.cod-estabel AND
                    calen-coml.data        = da-vencto-dp
                    NO-LOCK NO-ERROR.
        
        IF AVAIL calen-coml THEN DO:
            
               IF tipo-dia = 1 THEN DO:
                  ASSIGN l-ok = yes.
                  LEAVE achou-dia-util.
               END.
               ELSE DO:
                  da-vencto-dp = da-vencto-dp + 1.
               END.
         END.
    END.
    /* fim- nova interpretaá∆o calend†rio comercial s¢ para DP */


          
     

     /* troquei de lugar a criacao do FR que Ç para ajustar o valor da diferenáa da nota com o DP + impostos */

     IF de-funrural-unit > 0     AND
        es-param-estab.l-dupl-fr THEN DO:

        ASSIGN da-vencimento = date(MONTH(TODAY),01,YEAR(TODAY)) + 32
               da-vencimento = date(MONTH(da-vencimento),es-param-empresa.u-int-2,YEAR(da-vencimento)) /* era 10 */.
        CREATE dupli-apagar.
        ASSIGN dupli-apagar.cod-emitente = tt-docum-est.cod-emitente
               dupli-apagar.serie-docto  = tt-docum-est.serie-docto
               dupli-apagar.cod-esp      = es-param-estab.cod-esp-funrural
               dupli-apagar.nro-docto    = tt-docum-est.nro-docto
               dupli-apagar.nr-duplic    = tt-docum-est.nro-docto
               dupli-apagar.parcela      = "12"
               dupli-apagar.dt-emissao   = TODAY
               dupli-apagar.dt-trans     = TODAY
               dupli-apagar.dt-vencim    = da-vencimento
               dupli-apagar.valor        = de-funrural-unit-t - (de-val-ajuste-fr)
               dupli-apagar.nat-operacao = tt-docum-est.nat-operacao
               dupli-apagar.cod-estabel  = tt-docum-est.cod-estabel
               dupli-apagar.ep-codigo    = i-ep-codigo-usuario
               dupli-apagar.tp-despesa   = ITEM.tp-desp-padrao
               dupli-apagar.vl-a-pagar   = de-funrural-unit-t - (de-val-ajuste-fr)
               dupli-apagar.esp-movto    = 1
               dupli-apagar.estado       = 1.
     END.


     IF de-senar-unit > 0           AND
        es-param-estab.l-dupl-senar THEN DO:

         
       
        ASSIGN da-vencimento    = date(MONTH(TODAY),01,YEAR(TODAY)) + 32
               da-vencimento    = date(MONTH(da-vencimento),es-param-empresa.u-int-2,YEAR(da-vencimento)) /* era 10 */
               de-diferenca-cei = /*docum-est.valor-mercad*/ de-valor-merc - (tt-item-doc-est.preco-unit[2] * tt-item-doc-est.quantidade).


                                                                            



        CREATE dupli-apagar.
        ASSIGN dupli-apagar.cod-emitente = tt-docum-est.cod-emitente
               dupli-apagar.serie-docto  = tt-docum-est.serie-docto
               dupli-apagar.cod-esp      = es-param-estab.cod-esp-senar
               dupli-apagar.nro-docto    = tt-docum-est.nro-docto
               dupli-apagar.nr-duplic    = tt-docum-est.nro-docto
               dupli-apagar.parcela      = "13" 
               dupli-apagar.dt-emissao   = TODAY
               dupli-apagar.dt-trans     = TODAY
               dupli-apagar.dt-vencim    = da-vencimento
               dupli-apagar.valor        = de-senar-unit-t - de-diferenca-cei
               dupli-apagar.nat-operacao = tt-docum-est.nat-operacao
               dupli-apagar.cod-estabel  = tt-docum-est.cod-estabel
               dupli-apagar.ep-codigo    = i-ep-codigo-usuario
               dupli-apagar.tp-despesa   = ITEM.tp-desp-padrao
               dupli-apagar.vl-a-pagar   = de-senar-unit-t - de-diferenca-cei
               dupli-apagar.esp-movto    = 1
               dupli-apagar.estado       = 1.
        
        /* 11/10/2013 */
        IF l-compl-preco AND l-tem-cei = NO THEN DO: 


            ASSIGN dupli-apagar.valor      = de-senar-unit-t
                   dupli-apagar.vl-a-pagar = de-senar-unit-t.

        END.
    
        /* novo valor do senar */
         /* cria tabela de controle de ajuste do Senar 23/05/2013 - cei arrendondamento DP com sefaz */
        IF DE-DIFERENCA-CEI <> 0 THEN DO:
        
             FIND FIRST es-ajuste-senar-cei WHERE
                        es-ajuste-senar-cei.serie-docto  = tt-docum-est.serie-docto  and
                        es-ajuste-senar-cei.nro-docto    = tt-docum-est.nro-docto    and
                        es-ajuste-senar-cei.cod-emitente = tt-docum-est.cod-emitente and
                        es-ajuste-senar-cei.nat-operacao = tt-docum-est.nat-operacao NO-ERROR.
             IF NOT AVAIL es-ajuste-senar-cei THEN DO:
                 
                 CREATE es-ajuste-senar-cei.
                 ASSIGN es-ajuste-senar-cei.serie-docto  = tt-docum-est.serie-docto 
                        es-ajuste-senar-cei.nro-docto    = tt-docum-est.nro-docto   
                        es-ajuste-senar-cei.nat-operacao = tt-docum-est.nat-operacao
                        es-ajuste-senar-cei.cod-emitente = tt-docum-est.cod-emitente.
             END.
    
             ASSIGN es-ajuste-senar-cei.valor-original = de-senar-unit-t 
                    es-ajuste-senar-cei.valor-ajuste   = de-senar-unit-t - de-diferenca-cei.
           
        END. /* sen∆o tem cei n∆o grava */
    END.

/*     MESSAGE "ANTES DA dp" de-valor-dp                                                                          */
/*         SKIP                                                                                                     */
/*         "fr" de-funrural-unit-t SKIP                                                                             */
/*         "dif"  de-diferenca-cei SKIP                                                                             */
/*         "irr" de-irrf-unit SKIP                                                                                  */
/*         "cof"  de-cofins-unit  SKIP                                                                              */
/*         'de-cdo-unit-t' de-cdo-unit-t                                                                          */
/*         "de-total - de-cdo-doc" de-total - de-cdo-doc                                                          */
/*         "de-total - de-cdo-doc " de-total - de-cdo-doc - de-funrural-unit - de-diferenca-cei - de-senar-unit-t */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                                       */

     
       IF l-compl-preco THEN do:
           ASSIGN de-valor-dp = de-total - de-cdo-doc - de-funrural-unit - de-diferenca-cei - de-senar-unit-t - de-irrf-unit - de-cofins-unit.
/*            MESSAGE "compl"                                                                                                  */
/*              "  de-total              " de-total                                                                            */
/*              "   de-cdo-doc           "  de-cdo-doc                                                                         */
/*              "   de-funrural-unit     "  de-funrural-unit                                                                   */
/*              "   de-diferenca-cei     "  de-diferenca-cei                                                                   */
/*              "  de-senar-unit-t       " de-senar-unit-t                                                                     */
/*              "   de-irrf-unit         "  de-irrf-unit                                                                       */
/*              "   de-cofins-unit       "  de-cofins-unit                                                                     */
/*                "de-valor-dp" de-valor-dp                                                                                    */
/*                "oi"  "de-total - de-cdo-doc " de-total - de-cdo-doc - de-funrural-unit - de-diferenca-cei - de-senar-unit-t */
/*                VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                                           */
       END.
       ELSE do:
/*           MESSAGE " aqui problema que ele est† entrando NO aqui 2 "                              */
/*              "l-tem-cei " l-tem-cei                                                              */
/*              "l-dupl-cei"  es-param-estab.l-dupl-cei   SKIP                                      */
/*                de-total-t - de-irrf-unit - de-cofins-unit - de-funrural-unit-t - de-senar-unit-t */
/*               VIEW-AS ALERT-BOX.                                                                 */
           IF l-tem-cei and
              es-param-estab.l-dupl-cei THEN do:
               ASSIGN de-valor-dp = de-total-t - de-irrf-unit - de-cofins-unit - de-funrural-unit-t - de-senar-unit-t. /* soma o FR */
/*                MESSAGE "aqui"                                                                          */
/*                      de-total-t - de-irrf-unit - de-cofins-unit - de-funrural-unit-t - de-senar-unit-t */
/*                    "de-total" de-total-t              skip                                             */
/*                    "de-cdo-doc"  de-cdo-doc         skip                                               */
/*                    "ir-ubnit"  de-irrf-unit         skip                                               */
/*                    "cof unit" de-cofins-unit        skip                                               */
/*                    "fr unot"  de-funrural-unit-t    skip                                               */
/*                    "senar" de-senar-unit-t          skip                                               */
/*                    VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                  */
           END.
           ELSE do:
               ASSIGN de-valor-dp = de-total-t - de-irrf-unit - de-senar-unit-t - de-funrural-unit-t.
/*                 MESSAGE "aqui 2"                                                       */
/*                  de-total-t - de-irrf-unit - de-senar-unit-t - de-funrural-unit-t SKIP */
/*                     "totak t" de-total-t                                               */
/*                     "ir" de-irrf-unit                                                  */
/*                     "senar t" de-senar-unit-t                                          */
/*                     "fr t"  de-funrural-unit-t                                         */
/*                     "semmm "                                                           */
/*                      "senar" de-senar-unit                                             */
/*                     "fr"  de-funrural-unit                                             */
/*                    VIEW-AS ALERT-BOX INFO BUTTONS OK.                                  */
           END.
       END.

       IF es-param-estab.l-cei = NO THEN ASSIGN de-valor-dp = ordem-compra.qt-solic * cotacao-item.dec-1.


/*        MESSAGE "de-valor-dp" de-valor-dp      */
/*             VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                                      */


       ASSIGN i-parcela-dp = i-parcela-dp + 1.
       CREATE dupli-apagar.
       ASSIGN dupli-apagar.cod-emitente = tt-docum-est.cod-emitente
              dupli-apagar.serie-docto  = tt-docum-est.serie-docto
              dupli-apagar.cod-esp      = "DP"
              dupli-apagar.nro-docto    = tt-docum-est.nro-docto
              dupli-apagar.nr-duplic    = tt-docum-est.nro-docto
              dupli-apagar.parcela      = string(i-parcela-dp)
              dupli-apagar.dt-emissao   = TODAY
              dupli-apagar.dt-trans     = TODAY
              dupli-apagar.dt-vencim    = DA-VENCTO-DP
              dupli-apagar.valor        = de-valor-dp

/*               dupli-apagar.valor        = IF l-compl-preco THEN /*tt-docum-est.valor-mercad */ de-valor-merc - de-cdo-unit-t - de-irrf-unit - de-cofins-unit - de-funrural-unit - de-diferenca-cei - de-senar-unit */
/*                                                            ELSE de-total-t-sem-imp - de-irrf-unit - de-cofins-unit - de-funrural-unit-t /*- de-diferenca-cei */                                                    */


              dupli-apagar.nat-operacao = tt-docum-est.nat-operacao
              dupli-apagar.cod-estabel  = tt-docum-est.cod-estabel
              dupli-apagar.ep-codigo    = i-ep-codigo-usuario
              dupli-apagar.tp-despesa   = ITEM.tp-desp-padrao
              dupli-apagar.vl-a-pagar   = de-valor-dp


/*               dupli-apagar.vl-a-pagar   = IF l-compl-preco THEN /*tt-docum-est.valor-mercad */ de-valor-merc - de-cdo-unit-t - de-irrf-unit - de-cofins-unit - de-funrural-unit - de-diferenca-cei - de-senar-unit */
/*                                                            ELSE de-total-t-sem-imp - de-irrf-unit - de-cofins-unit - de-funrural-unit-t /*-  de-diferenca-cei */                                                   */

                                                               /*IF l-compl-preco THEN tt-dupli.vl-apagar - de-irrf-unit - de-cofins-unit - de-cdo-unit-t - (i-num-cei  * de-funrural-unit-t) - de-diferenca-cei /* 22/11/12 compl valor com a diferenáa entre nota e duplicata 26/04/2013 2* o FR que desconta da dp */
                                                           ELSE tt-dupli.vl-apagar - de-irrf-unit - de-cofins-unit - de-cdo-unit-t - (i-num-cei  * de-funrural-unit-t) - de-senar-unit-t - de-diferenca-cei /* 22/11/12 compl valor com a diferenáa entre nota e duplicata 26/04/2013 2* o FR que desconta da dp */*/
              dupli-apagar.esp-movto    = 1
              dupli-apagar.estado       = 1.


/*      /* 11/10/2013 */                                                                                    */
/*      IF (l-compl-preco AND l-tem-cei = NO) AND es-param-estab.l-cei = YES THEN DO: /*26*/                */
/*                                                                                                          */
/*                                                                                                          */
/*                                                                                                          */
/*            ASSIGN dupli-apagar.valor          = de-valor-dp /*- de-senar-unit-t */ - de-funrural-unit-t  */
/*                   dupli-apagar.vl-a-pagar     = de-valor-dp /*- de-senar-unit-t */ - de-funrural-unit-t. */
/*                                                                                                          */
/*   comentado 05022017                                                                                     */

/*        END.                                                                                              */
                                                                   

END.

FIND pedido-compr WHERE ROWID(pedido-compr) = gr-pedido-compr NO-ERROR.
IF NOT AVAIL pedido-compr THEN RETURN.

ASSIGN pedido-compr.log-2 = YES.

MESSAGE "Recebimento Atualizado! " i-nr-docto SKIP
        "Para Pedido " pedido-compr.num-pedido VIEW-AS ALERT-BOX INFO BUTTONS OK.

PROCEDURE pi-cria-mp232.
    CREATE es-mp232-emit.
    ASSIGN es-mp232-emit.cod-estabel  = pedido-compr.cod-estabel
           es-mp232-emit.cod-emitente = pedido-compr.cod-emitente
           es-mp232-emit.mes          = STRING(MONTH(es-param-empres.u-date-1))   
           es-mp232-emit.vl-irrf      = 0 
           es-mp232-emit.vl-cofins    = 0
           es-mp232-emit.vl-cdo       = de-cdo-unit-t
           es-mp232-emit.vl-funrural  = de-funrural-unit-t
           es-mp232-emit.val-nf       = de-total           
           es-mp232-emit.nr-nota-fis  = num-nota          
           es-mp232-emit.gerou        = NO
           es-mp232-emit.dt-pagto     = es-param-empres.u-date-1
           es-mp232-emit.dt-geracao   = ?
           es-mp232-emit.obs-geracao  = "N∆o possuia valor suficiente para taxaá∆o do imposto".

END.


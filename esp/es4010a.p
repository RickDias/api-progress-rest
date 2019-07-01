/**************************************************************************************
** EMPRESA  : CAMIL
** PROGRAMA : ES4010A
** DESCRICAO: ATUALIZAÄ«O DO RECEBIMENTO F÷SICO
** AUTOR    : DATASUL METROPOLITANA
** DATA     : OUTUBRO DE 2002
** VERSAO   : 2.04.00.000
** ECCB    : 16/04/2010- alterando mp232 para receber registro de FR que n∆o mais 
**            ser† descontado, essa tabela ser† somente para controle
** ECCB    : 16/06/2010 - alterando novo imposto SENAR, substitui o FR
** ECCB    : 07/12/2010 - Beto solicitou parcela 13 para Senar
** ECCB    : 05/04/2011 - Pis/Cofins waldemar
** ECCB    : 14/11/2012 - recalculo pis/cofins e vl unit
** ECCB    : 16/05/2013 - volta FR - CEI
** ECCB    : 17/06/2013 - RE n∆o mais uatim†tico segundo param do estabelecimento
** ECCB/Ramon : 07/11/2013 - Definiá∆o aliquotas ICM IPI PIS COFINS  CST DO ICMS
** ECCB - BB 18/02/2015
** ECCB - Interpretaá∆o impostos fr/cei/cdo/senar por estabelecimento 07/12/2015
***************************************************************************************/
{include/i-prgvrs.i ES4010A 2.09.00.004 } /*** "019004" ***/

{utp/ut-glob.i}
{cdp/cd0666.i}
{rep/reapi151.i}
{rep/reapi151.i1}
{cdp/cdapi150.i3} /*temp-table de vers∆o */ 
{cdp/cdapi150.i4} /*temp-table de erros */

/* Variaveis para calculo dos impostos */
{rep/re9301.i04 new}

def var de-aliq-icm as decimal no-undo.
DEF VAR vlo-debug AS LOG NO-UNDO INIT NO.

DEFINE INPUT PARAMETER p-rw-es-ticket AS ROWID.

def new global shared var r-RE0301-documento as rowid no-undo. 

DEF VAR de-ajuste-senar-cei AS DEC INITIAL 0.
DEF VAR de-diferenca-cei    AS DEC INITIAL 0.

DEF VAR de-salva-unit      AS DEC.
def var c-serie-comp       like item-doc-est.serie-docto.
def var c-nro-comp         like item-doc-est.nro-docto.
def var c-nat-comp         like natur-oper.nat-operacao.        
def var de-saldo           as dec initial 0.
DEF VAR l-zera             AS LOG INITIAL NO. /* 06/0404 */
def buffer b-docum-est      for docum-est.
def buffer b-es-movto-arroz for es-movto-arroz.
DEF BUFFER b-natur-oper FOR natur-oper.

DEF VAR de-total-com-imp AS DEC DECIMALS 2 INITIAL 0. /* 16/11/2012 */

DEF VAR l-tem-cei AS LOG INITIAL NO. /* cei 18/04/2013 */
DEF VAR i-num-cei AS INT INITIAL 1. /*  cei 29/04/2013 */


DEF VAR di-quantidade         AS DEC NO-UNDO. /* 081104 */
DEF VAR de-soma-mp            AS DEC INITIAL 0. /* 210105 */
DEF BUFFER b-es-param-empresa FOR es-param-empresa. /* 210105 */

DEF VAR i-tipo-nota           LIKE docum-est.tipo-nota.
DEF VAR i-esp-docto           LIKE docum-est.esp-docto.


def var c-item-troca          like item.it-codigo.
DEF VAR i-nr-nota             LIKE es-ticket.nr-nota-fornec INIT 0  NO-UNDO.
DEF VAR c-nat-operacao        LIKE natur-oper.nat-operacao  INIT "" NO-UNDO.
def var i-conta-seq           as int initial 0.
def var i-conta-item          as int initial 0.
def var de-pre-uni            as dec.
def var de-qtde-aux           as dec.
                   
DEF VAR de-variacao           LIKE item-uni-estab.var-qtd-re NO-UNDO.

DEF VAR quantidade            AS DEC NO-UNDO. /* 081104 */

DEF VAR de-quantidade         AS DEC NO-UNDO.
DEF VAR de-unitario           AS DEC DECIMALS 8 NO-UNDO.
DEF VAR de-total              AS DEC DECIMALS 2 NO-UNDO.
DEF VAR da-vencimento         AS DATE.
DEF VAR da-vencto-dp          AS DATE. /* 171005 */
DEF VAR l-ok                  AS LOG INITIAL NO. /* 171005 */
DEF VAR l-ok-fr               AS LOG INITIAL NO. /* 29/08/2014 */
DEF VAR l-ok-se               AS LOG INITIAL NO. /* 29/08/2014 */
DEF VAR i-cod-emitentees4010a LIKE emitente.cod-emitente NO-UNDO.
DEF VAR de-cdo-unit           AS DEC NO-UNDO.
DEF VAR de-funrural-unit      AS DEC NO-UNDO.
DEF VAR de-senar-unit         AS DEC NO-UNDO.
DEF VAR de-funrural-unit-ctrl AS DEC NO-UNDO. /* 16042010 */
DEF VAR de-irrf-unit          AS DEC NO-UNDO. /* 210105 */
DEF VAR de-cofins-unit        AS DEC NO-UNDO.
DEF VAR num-nota              AS char.

DEF VAR de-cdofunrural-unit   AS DEC NO-UNDO.
DEF VAR i-parcela-dp          AS INT NO-UNDO.
DEF VAR i-conta-parcela-dp    AS INT NO-UNDO.
DEF VAR de-base-calculo       AS DEC DECIMALS 2 NO-UNDO.
/* 210205 */                 
DEF VAR de-base-calculo-ori   AS DEC DECIMALS 2 NO-UNDO.
DEF VAR de-total-ori          AS DEC DECIMALS 2 NO-UNDO.
                             
DEF VAR h-acomp               As Handle No-Undo.

DEFINE TEMP-TABLE tt-dupli
    field dt-vencimen         as date
    field tp-despesa          as integer
    field vl-apagar           as decimal.

DEF NEW SHARED VAR i-num-pedido  LIKE ordem-compra.num-pedido   NO-UNDO.                                     
DEF NEW SHARED VAR i-num-ordem   LIKE ordem-compra.numero-ordem NO-UNDO.
DEF NEW SHARED VAR i-parcela     LIKE prazo-compra.parcela      NO-UNDO.

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


/* 07/11/2013 */
DEF VAR d-aliquota-icm LIKE natur-oper.aliquota-icm NO-UNDO.
DEF VAR i-cd-trib-icm  LIKE natur-oper.cd-trib-icm  NO-UNDO.
DEF VAR i-cd-trib-ipi  LIKE natur-oper.cd-trib-ipi  NO-UNDO.

DEF VAR d-perc-pis  AS DEC LABEL "perc pis"   NO-UNDO .
DEF VAR d-perc-cof  AS DEC LABEL "perc cof"   NO-UNDO.

DEF VAR d-valor-pis AS DEC LABEL "valor pis"  NO-UNDO.
DEF VAR d-valor-cof AS DEC LABEL "valor cof"  NO-UNDO.
 
DEF VAR c-cst-icms AS CHARACTER   NO-UNDO.
DEF VAR de-preco-unit-aux    LIKE item-doc-est.preco-unit[1] NO-UNDO.
DEF VAR de-total-com-imp-cor LIKE de-total-com-imp           NO-UNDO.



FIND FIRST param-global NO-LOCK NO-ERROR.      

FOR EACH tt-docum-est:
    DELETE tt-docum-est.
END.

FOR EACH tt-item-doc-est:
    DELETE tt-item-doc-est.
END.

FIND es-ticket WHERE ROWID(es-ticket) = p-rw-es-ticket NO-ERROR.
IF  NOT AVAIL es-ticket THEN RETURN.

FIND es-contrato-arroz NO-LOCK WHERE es-contrato-arroz.nr-contrato = es-ticket.nr-contrato NO-ERROR.

FIND es-param-estab NO-LOCK WHERE es-param-estab.cod-estabel = es-ticket.cod-estabel NO-ERROR.
IF  NOT AVAIL es-param-estab THEN RETURN.

FIND es-param-empresa NO-LOCK WHERE es-param-empresa.ep-codigo = i-ep-codigo-usuario NO-ERROR.

ASSIGN i-cod-emitentees4010a = IF es-ticket.operacao = 4 THEN es-ticket.cod-emitente ELSE es-ticket.cod-produtor.


FIND emitente NO-LOCK WHERE emitente.cod-emitente = i-cod-emitentees4010a NO-ERROR.


/* Verifica se existe portador obrigatorio para esse ticket */
IF es-contrato-arroz.u-int-3 > 0 THEN
DO:
   IF emitente.portador <> es-contrato-arroz.u-int-3 THEN
   DO:
      MESSAGE "O portador do Fornecedor deve ser o mesmo do informado para o Contrato !" SKIP(1)
              "A operaÁ∆o n∆o foi realizada !" emitente.portador "<>" es-contrato-arroz.u-int-3 
              VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      RETURN.
   END.
END.


FIND es-item NO-LOCK
    WHERE es-item.it-codigo = es-ticket.it-codigo
    NO-ERROR.

FIND item NO-LOCK
    WHERE item.it-codigo = es-ticket.it-codigo
    NO-ERROR.


/*RUN esp/esapi005.p (INPUT es-ticket.nr-ticket).*/
/* aqui somente era chamado o esapi005, agora est· chamando o pi-quantidade tambêm
             para calcular a bonificacao com peso limpo e cahmo o esapi005b tambêm que faz o 
             c·lculo correto 081104 */
          /* 040205 - nova interpretaá∆o 14 para limpo e n∆o limpo para 0 14 */
          IF es-ticket.cod-estabel = "14" THEN DO: 
              RUN esp/esapi005.p (INPUT es-ticket.nr-ticket).          
          END. /* est 14 */
          ELSE DO:
          
                RUN esp/esapi005.p (INPUT es-ticket.nr-ticket).          
                
                  IF es-ticket.dt-entrada > 11/09/04 THEN DO: 
                  
                      RUN pi-quantidade.
            
                      FOR EACH tt-detalhes:
                          DELETE tt-detalhes.
                      END.
            
                      RUN esp/esapi005b.p (INPUT es-ticket.nr-ticket,
                                           INPUT di-quantidade).
            
                  END. /* interpreta data */
          END. /* n∆o 14 */

ASSIGN de-quantidade   = 0
       de-unitario     = 0
       de-total        = 0
       de-base-calculo = 0
       i-num-pedido    = 0
       i-num-ordem     = 0
       i-parcela       = 0
       i-conta-item    = 0
       i-conta-seq     = 0
       de-pre-uni      = 0.

FOR EACH tt-detalhes:

  FIND es-tipo-desconto NO-LOCK
      WHERE es-tipo-desconto.tp-desconto = tt-detalhes.c-tp-desconto
      NO-ERROR.
 
  ASSIGN de-unitario = tt-detalhes.de-valor-unit.

  IF tt-detalhes.c-esp-docto = "DEP" OR 
     tt-detalhes.c-esp-docto = "CMP" OR 
     tt-detalhes.c-esp-docto = "IMP" THEN DO:
 
     ASSIGN de-quantidade    = de-quantidade   + tt-detalhes.de-quantidade
            de-total         = de-total        + tt-detalhes.de-valor-tot
            de-base-calculo  = de-base-calculo + (tt-detalhes.de-quantidade * (if es-ticket.vl-quilo <> 0 THEN es-ticket.vl-quilo ELSE IF AVAIL es-contrato-arroz THEN (es-contrato-arroz.vl-saco / es-item.unidade-saco) ELSE 0)). /* era s¢ valor es-contrato-arroz.vl-saco / unidade-saco -pedido do Michel 290305 */
     
  END. 
  IF tt-detalhes.c-esp-docto = "BON" THEN DO:
  
     ASSIGN de-total         = de-total        + de-valor-tot
            de-base-calculo  = de-base-calculo + (tt-detalhes.de-quantidade * (if es-ticket.vl-quilo <> 0 THEN es-ticket.vl-quilo ELSE IF AVAIL es-contrato-arroz THEN (es-contrato-arroz.vl-saco / es-item.unidade-saco) ELSE 0)).
  END.
  IF tt-detalhes.c-esp-docto = "DSC" THEN DO:

     IF es-tipo-desconto.quantidade THEN
        ASSIGN de-quantidade = de-quantidade - tt-detalhes.de-quantidade.

     IF es-tipo-desconto.financeiro THEN
          IF es-param-estab.base-descto = NO THEN
             ASSIGN de-quantidade = de-quantidade - tt-detalhes.de-quantidade.

         
     ASSIGN de-total        = round(de-total - de-valor-tot,2)
            de-base-calculo = de-base-calculo - (tt-detalhes.de-quantidade * (if es-ticket.vl-quilo <> 0 THEN es-ticket.vl-quilo ELSE IF AVAIL es-contrato-arroz THEN (es-contrato-arroz.vl-saco / es-item.unidade-saco) ELSE 0)).     
  END.
END.

ASSIGN de-base-calculo-ori = round(de-base-calculo,2).
       
FIND FIRST param-estoq NO-LOCK NO-ERROR.
IF  NOT AVAIL param-estoq THEN
    RETURN.

/* Compra Direta */

ASSIGN de-cdo-unit         = 0
       de-funrural-unit    = 0
       de-senar-unit       = 0
       de-irrf-unit        = 0
       de-cofins-unit      = 0
       de-cdofunrural-unit = 0.

ASSIGN num-nota = IF es-ticket.operacao = 4 THEN es-ticket.nro-docto ELSE es-ticket.nr-nota-fornec.
                                                                          
IF es-ticket.operacao = 1 THEN DO:  

    IF es-param-estab.l-cdo       AND /* 07/12/2015 */
       es-item.tem-cdo            AND
       /*emitente.estado     = "RS"  AND  estou tirando a interpretaá∆o fixa se Ç RS para que possa ver diretamente do estabelecimento */
      (emitente.natureza   = 1    OR emitente.natureza = 4) THEN DO:

      ASSIGN de-cdo-unit         = ROUND(es-param-empresa.vl-cdo * 
                                         ((IF emitente.natureza = 1 THEN de-quantidade ELSE 
                                           IF es-ticket.peso-desmem[1] > 0 THEN es-ticket.peso-desmem[1]
                                              ELSE de-quantidade) 
                                          / es-item.unidade-saco),2).
    
    END.



    IF es-param-estab.l-fr        AND /* 07/12/2015 */
       es-item.tem-funrural       AND 
      (emitente.natureza  = 1     OR emitente.natureza = 4) THEN DO:
     /* meda ASSIGN de-funrural-unit    = ROUND((de-base-calculo /
                                   ((100 - (es-param-empresa.vl-funrural-1 +
                                            es-param-empresa.vl-funrural-2 +
                                            es-param-empresa.vl-funrural-3)) / 100)) - de-base-calculo,2).    */
        ASSIGN de-funrural-unit = ROUND((de-base-calculo - de-cdo-unit) *
                                              ((es-param-empresa.vl-funrural-1 +
                                                es-param-empresa.vl-funrural-2 +
                                                es-param-empresa.vl-funrural-3) / 100),2) /* alterado 04/18/2017 */
            de-salva-unit = de-base-calculo - de-cdo-unit.



        
    END.

    ELSE

    /* aqui22 rde - 08/01/2018 - migraá∆o - totvs 12 */
    ASSIGN de-salva-unit = de-base-calculo.





    IF es-param-estab.l-senar     AND /* 07/12/2015 */
       (emitente.natureza  = 1     OR emitente.natureza = 4) THEN DO:
      /* senar */
      /*ASSIGN de-senar-unit    = ROUND((de-base-calculo / ((100 - (es-param-empresa.vl-senar)) / 100)) - de-base-calculo,2).*/

        ASSIGN de-senar-unit = ROUND(((de-base-calculo - de-cdo-unit) * (es-param-empresa.vl-senar / 100)),2). /* alterado 04/18/2017 */



       
      /* para criar tabela de controle de ajuste do Senar 23/05/2013 - cei arrendondamento DP com sefaz */
      ASSIGN de-ajuste-senar-cei = de-senar-unit.
    END.
    

    /* 16/04/2010 fixa valores de furural pq foram zerados pra n∆o cobrar mais, mas aqui precisa gerar tabela de controle
       usando a tabela de medida provis¢ria que n∆o foi usada */
    IF es-param-estab.l-cei       AND /* 07/12/2015 */
       es-item.tem-funrural       AND 
      (emitente.natureza  = 1     OR emitente.natureza = 4) THEN DO:

     /* ASSIGN de-funrural-unit-ctrl = ROUND((de-base-calculo /
                                   ((100 - (es-param-empresa.vl-funrural-1 /*2*/ +
                                            es-param-empresa.vl-funrural-2 /*0.2*/ +
                                            es-param-empresa.vl-funrural-3 /*0.1*/)) / 100)) - de-base-calculo,2). /* tirei fixo em 10/05/2013 */*/
      
        ASSIGN de-funrural-unit = ROUND((de-base-calculo - de-cdo-unit) *
                                              ((es-param-empresa.vl-funrural-1 +
                                                es-param-empresa.vl-funrural-2 +
                                                es-param-empresa.vl-funrural-3) / 100),2) /* alterado 04/18/2017 */.    

       
      ASSIGN l-tem-cei = NO.

        /* troca de lugar 16/04/2013 para consistencia CEI */
        /*FIND FIRST emitente WHERE emitente.cod-emitente = es-ticket.cod-produtor NO-LOCK NO-ERROR. /* 28/03/2011 */*/
        FIND FIRST es-emitente-cei WHERE es-emitente-cei.cod-emitente = emitente.cod-emitente NO-LOCK NO-ERROR.
        IF AVAIL es-emitente-cei AND es-emitente-cei.l-ativo = YES AND es-emitente-cei.dt-validade-cei < TODAY THEN DO:
           MESSAGE "Fornecedor " emitente.cod-emitente  " possui CEI Ativa, porÇm a data de validade da mesma est† EXPIRADA!" SKIP
               "Verifique, pois n∆o ser† permitida a sua utilizaá∆o!!!"
               VIEW-AS ALERT-BOX ERROR.
           RETURN NO-APPLY.
        END.
    
        IF (AVAIL es-emitente-cei AND es-emitente-cei.l-ativo = NO) OR NOT AVAIL es-emitente-cei THEN DO:
            IF es-param-estab.l-cei THEN DO: /*21/11/2013 */
               MESSAGE "Fornecedor " emitente.cod-emitente  " possui Controle de CEI INATIVO!" SKIP
                   "Confirma?"
                   VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE l-cont-cei-1 AS LOG.
               IF l-cont-cei-1 = NO THEN RETURN NO-APPLY.
            END.
        END.
              
        
        /* natureza = 1 - PF */
        IF AVAIL es-emitente-cei AND es-emitente-cei.l-ativo = YES AND es-emitente-cei.dt-validade-cei >= TODAY AND emitente.natureza = 1 AND es-param-estab.l-cei THEN DO:
           ASSIGN l-tem-cei = YES.
        END.
       

        /* tratando se Ç para aplicar o funrural deixa como j† calculado com base nos percentuais do estabelecimento, sen∆o zera o que se calculou e abaixo tb n∆o permite gerar parc FR */
        IF l-tem-cei THEN do:
            ASSIGN de-funrural-unit-ctrl = 0 
                   de-funrural-unit      = 0
                   i-num-cei             = 1 /* para descontar s¢ uma vez o FR da DP */
                   /*de-funrural-unit-t = 0 n∆o zerando 26/04/2013 para descontar da duplicata */.
        END.
        ELSE i-num-cei = 2. /* para retirar o valor da base da DP */            

/**/


      /* controle de funrural n∆o mais cobrado */
      CREATE es-mp232-emit.                                                                              
      ASSIGN es-mp232-emit.cod-estabel  = es-ticket.cod-estabel                                             
             es-mp232-emit.cod-emitente = es-ticket.cod-emitente          
             es-mp232-emit.mes          = STRING(MONTH(TODAY))
             es-mp232-emit.vl-cdo       = de-cdo-unit                                                       
             es-mp232-emit.vl-funrural  = de-funrural-unit-ctrl                                               
             es-mp232-emit.val-nf       = de-total                                                          
             es-mp232-emit.nr-nota-fis  = num-nota                                                          
             es-mp232-emit.dt-geracao   = TODAY
             es-mp232-emit.obs-geracao  = string(es-ticket.nr-ticket).

    END.
    
    /* Medida Provis¢ria 232 - 210105 */
    ASSIGN de-soma-mp = 0.
    
    FIND FIRST b-es-param-empresa WHERE 
               b-es-param-empresa.ep-codigo = i-ep-codigo-usuario
        NO-LOCK NO-ERROR.

    IF b-es-param-empresa.mp-ativa THEN DO:
        ASSIGN da-vencimento = date(MONTH(TODAY),01,YEAR(TODAY)) + 32
               da-vencimento = date(MONTH(da-vencimento),15,YEAR(da-vencimento)).
        RUN pi-cria-mp232. /* pra qq caso cria */
                
        FIND FIRST es-mp232-emit WHERE
                   es-mp232-emit.cod-estabel  = es-ticket.cod-estabel  AND
                   es-mp232-emit.cod-emitente = i-cod-emitentees4010a AND
                   es-mp232-emit.mes          = STRING(MONTH(es-param-empres.u-date-1))
                   EXCLUSIVE-LOCK NO-ERROR.
        IF AVAIL es-mp232-emit THEN DO:
          FOR EACH es-mp232-emit WHERE
                   es-mp232-emit.cod-estabel  = es-ticket.cod-estabel  AND
                   es-mp232-emit.cod-emitente = i-cod-emitentees4010a AND
                   es-mp232-emit.mes          = STRING(MONTH(es-param-empres.u-date-1))
            NO-LOCK.
              IF es-mp232-emit.gerou = NO THEN
                 ASSIGN de-soma-mp = de-soma-mp + es-mp232-emit.val-nf.
          END. /* for each */        
        END. /* existe acumulado */
        RELEASE es-mp232-emit.

        /* pessoa f°sica */
        IF (emitente.natureza = 1 OR emitente.natureza = 4) THEN DO:
            IF de-soma-mp >= b-es-param-empresa.val-min-pf THEN DO:
                                        
                ASSIGN de-irrf-unit = ROUND((de-soma-mp / ((100 - (es-param-empresa.perc-irrf)) / 100)) - de-soma-mp,2).
                
                IF de-total < b-es-param-empresa.val-min-pf THEN DO:
                    /* coloca para os outros registros que j· existiam no arquivo a nota que est· contendo o acumulado */
                    FOR EACH es-mp232-emit WHERE
                       es-mp232-emit.cod-estabel  = es-ticket.cod-estabel  AND
                       es-mp232-emit.cod-emitente = i-cod-emitentees4010a AND
                       es-mp232-emit.mes          = STRING(MONTH(es-param-empres.u-date-1))
                        EXCLUSIVE-LOCK.
                        
    
                        ASSIGN es-mp232-emit.gerou       = yes
                               es-mp232-emit.dt-geracao  = TODAY
                               es-mp232-emit.obs-geracao  = "Impostos Irrf-Cofins gerados na nota " + string(num-nota)
                               es-mp232-emit.nota-geracao  = string(num-nota).
                    END. /* for each es-mp */
                    RELEASE es-mp232-emit.
                END.
                FIND FIRST es-mp232-emit WHERE
                   es-mp232-emit.cod-estabel  = es-ticket.cod-estabel  AND
                   es-mp232-emit.cod-emitente = i-cod-emitentees4010a AND
                   es-mp232-emit.mes          = STRING(MONTH(es-param-empres.u-date-1)) AND
                   es-mp232-emit.nr-nota-fis  = num-nota
                   EXCLUSIVE-LOCK NO-ERROR.

                ASSIGN es-mp232-emit.gerou       = yes
                       es-mp232-emit.dt-geracao  = TODAY
                       es-mp232-emit.vl-irrf  = de-irrf-unit
                       es-mp232-emit.obs-geracao  = "Impostos Irrf-Cofins gerados nesta nota " + string(num-nota)
                       es-mp232-emit.nota-geracao  = string(num-nota).
            END. /* deve gerar */
            RELEASE es-mp232-emit.
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
                    ASSIGN de-irrf-unit   = ROUND((de-soma-mp / ((100 - (es-param-empresa.perc-irrf)) / 100)) - de-soma-mp,2)
                           de-cofins-unit = ROUND((de-soma-mp / ((100 - (es-param-empresa.perc-cofins)) / 100)) - de-soma-mp,2).
                    
                    IF de-total < b-es-param-empresa.val-min-pj THEN DO:
                        /* coloca para os outros registros que j· existiam no arquivo a nota que est· contendo o acumulado */
                        FOR EACH es-mp232-emit WHERE
                               es-mp232-emit.cod-estabel  = es-ticket.cod-estabel  AND
                               es-mp232-emit.cod-emitente = i-cod-emitentees4010a AND
                               es-mp232-emit.mes          = STRING(MONTH(es-param-empres.u-date-1))
                            EXCLUSIVE-LOCK.                        
       
                            ASSIGN es-mp232-emit.gerou       = yes
                                   es-mp232-emit.dt-geracao  = TODAY
                                   es-mp232-emit.obs-geracao = "Impostos Irrf-Cofins gerados na nota " + string(num-nota)
                                   es-mp232-emit.nota-geracao  = string(num-nota).
                        END. /* for each es-mp */
                        RELEASE es-mp232-emit.
                    END. /* val min */
                    FIND FIRST es-mp232-emit WHERE
                               es-mp232-emit.cod-estabel  = es-ticket.cod-estabel  AND
                               es-mp232-emit.cod-emitente = i-cod-emitentees4010a AND
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
            RELEASE es-mp232-emit.
          END. /* vl maior */
        END. /* PJ */
    END. /* MP ATIVA */

    IF (/* 26/04/2017 de-cdo-unit + */ de-funrural-unit + de-senar-unit + de-irrf-unit + de-cofins-unit) > 0 THEN DO:
       ASSIGN de-base-calculo     = ROUND(de-base-calculo,2)
              de-cdofunrural-unit = /*de-cdo-unit + */ de-funrural-unit + de-senar-unit + de-irrf-unit + de-cofins-unit
              de-total            = ROUND(de-total,2).
    END.

  
    
END.


ASSIGN c-nat-operacao = "". /* 120905 */

/* 110905 */
ASSIGN c-nat-operacao = es-ticket.nat-operacao.
/* 110905 */


FIND FIRST natur-oper  
    WHERE natur-oper.nat-operacao = c-nat-operacao  
    NO-LOCK NO-ERROR.
IF  NOT AVAIL natur-oper THEN DO:
    MESSAGE "Natureza de Operaá∆o: " c-nat-operacao " n∆o encontrada."
            VIEW-AS ALERT-BOX.
    RETURN.
END.

FIND estabelec
    WHERE estabelec.cod-estabel = es-ticket.cod-estabel
    NO-LOCK NO-ERROR.
IF  NOT AVAIL estabelec THEN DO:
    MESSAGE "Estabelecimento: " + es-ticket.cod-estabel + " n∆o encontrado."
            VIEW-AS ALERT-BOX.
    RETURN.
END.

FIND ITEM
    WHERE ITEM.it-codigo = es-ticket.it-codigo
    NO-LOCK NO-ERROR.
IF  NOT AVAIL ITEM THEN DO:
    MESSAGE "Item: " + es-ticket.it-codigo + " n∆o encontrado."
            VIEW-AS ALERT-BOX.
    RETURN.
END.

/* Validacao do Saldo da Ordem de Compra e Limite de Variacao */
IF  es-ticket.operacao = 1 or es-ticket.operacao = 4 THEN DO:


    FIND es-contrato-arroz NO-LOCK
       WHERE es-contrato-arroz.nr-contrato = es-ticket.nr-contrato NO-ERROR.
    IF AVAIL es-contrato-arroz THEN DO:
       FIND FIRST pedido-compr WHERE pedido-compr.num-pedido = int(es-contrato-arroz.nr-pedido) NO-LOCK NO-ERROR.  

       IF AVAIL pedido-compr THEN DO:
          FIND FIRST ordem-compra
              WHERE ordem-compra.num-pedido = pedido-compr.num-pedido
              AND   ordem-compra.it-codigo  = es-contrato-arroz.it-codigo
              NO-LOCK NO-ERROR.
          IF AVAIL ordem-compra THEN DO:
             FIND FIRST prazo-compra 
                WHERE prazo-compra.numero-ordem = ordem-compra.numero-ordem
                NO-LOCK NO-ERROR.
             IF AVAIL prazo-compra THEN DO:
                IF prazo-compra.quant-saldo < de-quantidade THEN DO:
                   /* Variacao do recebimento */
                   ASSIGN de-variacao = (((de-quantidade / prazo-compra.quant-saldo) - 1) * 100).
                   FIND FIRST item-uni-estab
                       WHERE item-uni-estab.it-codigo  = es-ticket.it-codigo
                       AND   item-uni-estab.cod-estabel = es-ticket.cod-estabel
                       NO-LOCK NO-ERROR.
                   IF AVAIL item-uni-estab THEN DO: 
                      IF de-variacao > item-uni-estab.var-qtd-re THEN DO:
                         run utp/ut-msgs.p (input "show":U, input 17006, 
                                            INPUT "Item " +  STRING(es-ticket.it-codigo) + " com variaá∆o:" + 
                                                  STRING(item-uni-estab.var-qtd-re) + " %" +
                                                  " Menor que variacao: " + STRING(de-variacao) + " % a ser recebida" + 
                                                  "Saldo da Ordem de Compra: " + STRING(prazo-compra.quant-saldo) + 
                                                  " menor que a Qtde Informada no Ticket: " + STRING(de-quantidade) +  
                                                  "    Pedido: " + STRING(pedido-compr.num-pedido) + 
                                                  "  Ordem Compra: " + STRING(prazo-compra.numero-ordem)).
                         RETURN.
                      END.
                   END.
                   ELSE DO:
                      run utp/ut-msgs.p (input "show":U, input 17006, 
                                         input "Saldo da Ordem de Compra: " +
                                               STRING(prazo-compra.quant-saldo) + 
                                               " menor que a Qtde Informada no Ticket: " +
                                               STRING(de-quantidade) +  
                                               "    Pedido: " + STRING(pedido-compr.num-pedido) + 
                                               "  Ordem Compra: " + STRING(prazo-compra.numero-ordem)).
                      RETURN.
                   END.
                END.
             END.
             ELSE DO:
                run utp/ut-msgs.p (input "show":U, input 17006, 
                                   input "Nao existe Prazo de Compra para o Pedido: " +
                                         STRING(pedido-compr.num-pedido) + 
                                         "  Item: " + string(es-contrato-arroz.it-codigo)).
                RETURN.
             END.
          END.
          ELSE DO:
             run utp/ut-msgs.p (input "show":U, input 17006, 
                                input "Nao existe Ordem de Compra para o Pedido: " + 
                                      STRING(pedido-compr.num-pedido) + 
                                      "  Item: " + string(es-contrato-arroz.it-codigo)).
             RETURN.
          END.
       END.
       ELSE DO:
           run utp/ut-msgs.p (input "show":U, input 17006, 
                              input "Pedido de Compra: " + 
                                    STRING(es-contrato-arroz.nr-pedido) + 
                                    " informado no Contrato: " + 
                                    string(es-ticket.nr-contrato) +  
                                    " inexistente" ).
           RETURN.
       END.
    END.
    ELSE DO:
       run utp/ut-msgs.p (input "show":U, input 17006, 
                          input "Contrato: " + string(es-ticket.nr-contrato) + " inexistente").
       RETURN.
    END.
END.

Run utp/ut-acomp.p persistent set h-acomp.
Run pi-inicializar In h-acomp (Input "Atualizando Recebimento").
Run pi-seta-tipo   In h-acomp (Input 6).

DO  TRANSACTION:
 
    ASSIGN i-nr-nota = IF es-ticket.operacao = 4 THEN es-ticket.nro-docto ELSE es-ticket.nr-nota-fornec.
    
    FIND FIRST saldo-estoq
        WHERE saldo-estoq.cod-estabel = es-ticket.cod-estabel
          AND saldo-estoq.it-codigo   = es-ticket.it-codigo
          AND saldo-estoq.cod-depos   = es-ticket.cod-depos
          AND saldo-estoq.cod-localiz = ""
          AND saldo-estoq.lote        = ""
          AND saldo-estoq.cod-refer   = ""
        NO-LOCK NO-ERROR.
    IF  NOT AVAIL saldo-estoq THEN DO:
        CREATE saldo-estoq.
        ASSIGN saldo-estoq.cod-estabel = es-ticket.cod-estabel
               saldo-estoq.it-codigo   = es-ticket.it-codigo
               saldo-estoq.cod-depos   = es-ticket.cod-depos
               saldo-estoq.cod-localiz = ""
               saldo-estoq.lote        = ""
               saldo-estoq.cod-refer   = "".
    END.

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
 
    IF es-ticket.operacao = 1 THEN DO: /* Compra Direta */
      /* FIND FIRST conta-contab 
             WHERE conta-contab.conta-contab = es-param-estab.conta-contab-comp-dep
               AND conta-contab.ep-codigo    = param-global.empresa-prin
                   NO-LOCK NO-ERROR. */
        FIND FIRST cta_ctbl 
             WHERE cta_ctbl.cod_plano_cta_ctbl = (IF AVAIL plano_cta_ctbl THEN plano_cta_ctbl.cod_plano_cta_ctbl ELSE "PLA01")
               AND cta_ctbl.cod_cta_ctbl       = es-param-estab.conta-contab-comp-dep
                   NO-LOCK NO-ERROR.
    END.
    ELSE                           
        IF es-ticket.operacao = 2 OR 
           es-ticket.operacao = 7 THEN DO: /* Arroz a Deposito ou Transferencia de Titularidade*/
         /* FIND FIRST conta-contab 
                 WHERE conta-contab.conta-contab = es-param-estab.conta-contab-ent-dep
                   AND conta-contab.ep-codigo    = param-global.empresa-prin
                       NO-LOCK NO-ERROR. */
            FIND FIRST cta_ctbl 
                 WHERE cta_ctbl.cod_plano_cta_ctbl = (IF AVAIL plano_cta_ctbl THEN plano_cta_ctbl.cod_plano_cta_ctbl ELSE "PLA01")
                   AND cta_ctbl.cod_cta_ctbl       = es-param-estab.conta-contab-ent-dep
                       NO-LOCK NO-ERROR.
        END.
        ELSE
            IF es-ticket.operacao = 4 THEN DO: /* Importacao */
             /* FIND FIRST conta-contab 
                     WHERE conta-contab.conta-contab = "1113016" + string(es-ticket.cod-estabel,"99") + "000200"
                       AND conta-contab.ep-codigo    = param-global.empresa-prin
                           NO-LOCK NO-ERROR. */
                FIND FIRST cta_ctbl 
                     WHERE cta_ctbl.cod_plano_cta_ctbl = (IF AVAIL plano_cta_ctbl THEN plano_cta_ctbl.cod_plano_cta_ctbl ELSE "PLA01")
                       AND cta_ctbl.cod_cta_ctbl       = "1113016"
                           NO-LOCK NO-ERROR.
            END.
            ELSE
                IF es-ticket.operacao =  8 OR 
                   es-ticket.operacao =  9 OR 
                   es-ticket.operacao = 10 THEN DO: /* erika Armazenagem e tra-estab */
    
                    FIND FIRST estab-mat 
                         WHERE estab-mat.cod-estabel = es-ticket.cod-estabel 
                               NO-LOCK NO-ERROR.
    
                    /* 28/11/2011 - devido a mudanáa de transferàncias no ems206 invertemos a busca da conta para a unidade de origem mesmo para acertar a transit¢ria */
                    IF es-ticket.operacao = 10 THEN DO:
                        FIND FIRST estab-mat 
                             WHERE estab-mat.cod-estabel = trim(substring(es-ticket.u-char-3,31,3)) /*c-estab-de-or*/ NO-LOCK NO-ERROR.
                    END.
                
                    /* transferencia */              
                    IF es-ticket.nat-operacao BEGINS "11" THEN DO:
                     /* FIND FIRST conta-contab 
                             WHERE conta-contab.conta-contab = estab-mat.conta-transf
                               AND conta-contab.ep-codigo    = param-global.empresa-prin
                                   NO-LOCK NO-ERROR.*/
                        FIND FIRST cta_ctbl 
                             WHERE cta_ctbl.cod_plano_cta_ctbl = (IF AVAIL plano_cta_ctbl THEN plano_cta_ctbl.cod_plano_cta_ctbl ELSE "PLA01")
                               AND cta_ctbl.cod_cta_ctbl       = estab-mat.cod-cta-transf-unif /*ems2 estab-mat.conta-transf*/
                                   NO-LOCK NO-ERROR.
                    END.    
                    ELSE DO:
                        /* consignacao */
                      /*FIND FIRST conta-contab 
                             WHERE conta-contab.conta-contab = estab-mat.conta-sai-consig
                               AND conta-contab.ep-codigo    = param-global.empresa-prin
                                   NO-LOCK NO-ERROR.*/
                        FIND FIRST cta_ctbl 
                             WHERE cta_ctbl.cod_plano_cta_ctbl = (IF AVAIL plano_cta_ctbl THEN plano_cta_ctbl.cod_plano_cta_ctbl ELSE "PLA01")
                               AND cta_ctbl.cod_cta_ctbl       = estab-mat.cod-cta-saida-consig-unif /*estab-mat.conta-sai-consig*/
                                   NO-LOCK NO-ERROR.
                    END.
                    
                END. /* armazenagem */
  
    /*erika armazenagem e tra-estab */
    IF es-ticket.operacao =  8 or
       es-ticket.operacao =  9 or
       es-ticket.operacao = 10 THEN DO:

        ASSIGN de-quantidade   = es-ticket.peso-desmem[1].
               
    END. /* armazenagem */
    
    
    IF emitente.natureza <> 1 AND /* Juridica */
        es-ticket.peso-desmem[1] > 0 THEN DO:
                       
        ASSIGN de-unitario = (if es-ticket.vl-quilo <> 0 THEN es-ticket.vl-quilo ELSE IF AVAIL es-contrato-arroz THEN (es-contrato-arroz.vl-saco / es-item.unidade-saco) ELSE 0)  /*es-contrato.vl-quilo 220205 */
               de-total    = de-unitario * (IF emitente.natureza = 1 THEN de-quantidade ELSE 
                                            IF es-ticket.peso-desmem[1] > 0 THEN es-ticket.peso-desmem[1]
                                            ELSE de-quantidade)
               de-base-calculo = de-total.
                                                 
    END.


    ASSIGN de-unitario = de-total / (IF emitente.natureza = 1 THEN de-quantidade ELSE
                                     IF es-ticket.peso-desmem[1] > 0 THEN es-ticket.peso-desmem[1]
                                        ELSE de-quantidade).
                                             

                                          
    
    IF es-ticket.operacao = 2 AND
       emitente.natureza = 1 THEN /* Pessoa Fisica */ DO:


    
        ASSIGN de-unitario = (if es-ticket.vl-quilo <> 0 THEN es-ticket.vl-quilo ELSE IF AVAIL es-contrato-arroz THEN (es-contrato-arroz.vl-saco / es-item.unidade-saco) ELSE 0)  /*es-contrato.vl-quilo 220205 */
               de-total    = de-quantidade * de-unitario.

                     

    END.
     /* 250705 */
     ASSIGN i-tipo-nota = 6
            i-esp-docto = 21  /*** NFE ***/.
                                                                                                                                                                              
    /*erika armazenagem tra-estab */
    IF es-ticket.operacao =  8 or
       es-ticket.operacao =  9 or
       es-ticket.operacao = 10 THEN DO:
                       
        ASSIGN de-unitario = (if es-ticket.vl-quilo <> 0 THEN es-ticket.vl-quilo ELSE IF AVAIL es-contrato-arroz THEN (es-contrato-arroz.vl-saco / es-item.unidade-saco) ELSE 0)  /*es-contrato.vl-quilo 220205 */
               de-total    = de-unitario * (IF emitente.natureza = 1 THEN de-quantidade ELSE 
                                            IF es-ticket.peso-desmem[1] > 0 THEN es-ticket.peso-desmem[1]
                                            ELSE de-quantidade)
               de-base-calculo = de-total
               de-quantidade   = es-ticket.peso-desmem[1]
               i-tipo-nota     = 3
               i-esp-docto     = 23  /*** NFT ***/.

                                                
               

                                                 
    END. /* armazenagem */
    assign de-pre-uni = de-total / de-quantidade.

    IF es-param-estab.l-re THEN DO: /* nova interpretaá∆o para unidade 32 n∆o gerar re 19/06/203 */
    
        IF l-zera = NO THEN DO:
     
            /*totvs12 teste
            MESSAGE "es4010a "
                    "conta " (IF AVAIL cta_ctbl THEN cta_ctbl.cod_cta_ctbl ELSE "")
                     VIEW-AS ALERT-BOX INFO BUTTONS OK.
            */

            CREATE tt-docum-est.

            ASSIGN tt-docum-est.serie-docto = es-ticket.serie.


            IF es-ticket.operacao = 10 THEN ASSIGN tt-docum-est.serie-docto = substring(es-ticket.u-char-3,1,5).

            IF es-ticket.operacao = 8 AND es-contrato-arroz.l-danf THEN ASSIGN tt-docum-est.serie-docto = string(es-ticket.u-int-2). /* 23/07/2013 erika alteraá∆o para armazenagem */
            IF es-ticket.operacao = 1 AND es-contrato-arroz.l-danf THEN ASSIGN tt-docum-est.serie-docto = string(es-ticket.u-int-2). /* 21/11/2013 erika alteraá∆o para compra cuiaba */

            /* colocando a sÇrie do fornecedor N«O ê GERADA CONTRA NOTA, O QUE CARACTERIZA QUE O FORNECEDOR EMITE NFE NORMAL MODELO 55 E N«O MODELO 4 COMO PRODUTOR, O QUE DEVERIA GERAR CONTRA NOTA */
            IF es-ticket.operacao = 2 AND es-contrato-arroz.l-danf AND es-contrato-arroz.cod-estabel = "27" THEN ASSIGN tt-docum-est.serie-docto = string(es-ticket.u-int-2). /* 05/05/2015 erika alteraá∆o para deposito itapecuru */
           
            ASSIGN tt-docum-est.nro-docto        = i-nr-nota
                   tt-docum-est.cod-emitente     = i-cod-emitentees4010a
                   tt-docum-est.nat-operacao     = natur-oper.nat-operacao
                   tt-docum-est.cod-estabel      = es-ticket.cod-estabel
                   tt-docum-est.estab-de-or      = trim(substring(es-ticket.u-char-3,31,3)) /*c-estab-de-or*/
                   tt-docum-est.esp-docto        = i-esp-docto /*21  /*** NFE ***/*/
                   tt-docum-est.esp-fiscal       = natur-oper.especie-doc
                   tt-docum-est.aliq-irf         = 0
                   tt-docum-est.aliquota-icm     = natur-oper.aliquota-icm
                   tt-docum-est.aliquota-iss     = 0
                   tt-docum-est.base-cofins-subs = 0
                   tt-docum-est.base-icm         = de-base-calculo-ori - de-cdo-unit
                   tt-docum-est.base-ipi         = de-base-calculo-ori - de-cdo-unit
                   tt-docum-est.base-iss         = 0
                   tt-docum-est.base-pis-subs    = 0
                   tt-docum-est.base-subs        = 0
                   tt-docum-est.cod-observa      = 1   /*** INDUSTRIA  ***/
  
                   tt-docum-est.conta-transit    = "" /*IF AVAIL cta_ctbl THEN cta_ctbl.cod_cta_ctbl ELSE "" */ /* IF AVAIL conta-contab THEN conta-contab.conta-contab ELSE "" */
                   tt-docum-est.ct-transit       = IF AVAIL cta_ctbl THEN cta_ctbl.cod_cta_ctbl ELSE "" /* IF AVAIL conta-contab THEN conta-contab.ct-codigo    ELSE "" */
                   tt-docum-est.sc-transit       = ""                                                   /* IF AVAIL conta-contab THEN conta-contab.sc-codigo    ELSE "" */
 
                   tt-docum-est.despesa-nota     = 0
                   tt-docum-est.dt-emissao       = es-ticket.dt-entrada   
                   tt-docum-est.dt-trans         = TODAY   
                   tt-docum-est.dt-venc-icm      = TODAY
                   tt-docum-est.dt-venc-ipi      = TODAY
                   tt-docum-est.dt-venc-iss      = TODAY
                   tt-docum-est.estab-fisc       = tt-docum-est.cod-estabel
                   tt-docum-est.icm-complem      = 0
                   
                   tt-docum-est.icm-fonte        = 0
                   tt-docum-est.icm-nao-trib     = 0
                   tt-docum-est.icm-outras       = de-base-calculo-ori - de-cdo-unit
                   tt-docum-est.ind-orig-entrada = 1   /*** NORMAL ***/
                   tt-docum-est.ind-rateio       = NO
                   tt-docum-est.ind-via-envio    = 1   /*** NORMAL ***/
                   tt-docum-est.ipi-deb-cre      = 0
                   tt-docum-est.ipi-despesa      = 0 
                   tt-docum-est.ipi-nao-trib     = 0
                   tt-docum-est.ipi-outras       = de-base-calculo-ori - de-cdo-unit
                   tt-docum-est.iss-deb-cre      = 0
                   tt-docum-est.iss-nao-trib     = 0
                   tt-docum-est.iss-outras       = 0
                   tt-docum-est.observacao       = ""
                   tt-docum-est.rec-fisico       = NO
                   tt-docum-est.sit-docum        = 1   /*** OK ***/
                   tt-docum-est.tipo-docto       = 1   /*** ENTRADA ***/
                   tt-docum-est.tipo-nota        = i-tipo-nota /*6   /*** COMPRA ***/*/
                   tt-docum-est.tot-peso         = IF emitente.natureza = 1 THEN de-quantidade ELSE 
                                                   IF es-ticket.peso-desmem[1] > 0 THEN es-ticket.peso-desmem[1]
                                                      ELSE de-quantidade
                   tt-docum-est.tot-valor        = de-base-calculo-ori - de-cdo-unit
                   tt-docum-est.usuario          = v_cod_usuar_corren
                   tt-docum-est.uf               = emitente.estado
                   tt-docum-est.valor-frete      = 0
                   tt-docum-est.valor-irf        = 0
                   tt-docum-est.valor-mercad     = de-base-calculo-ori - de-cdo-unit
                   tt-docum-est.valor-outras     = 0
                   tt-docum-est.valor-seguro     = 0
                   tt-docum-est.via-transp       = 1   /*** RODOVIARIO ***/
                   tt-docum-est.vl-cofins-subs   = 0
                   tt-docum-est.vl-imp-frete     = 0
                   tt-docum-est.vl-imp-outras    = 0
                   tt-docum-est.vl-imp-seguro    = 0
                   tt-docum-est.vl-pis-subs      = 0
                   tt-docum-est.vl-subs          = 0
                   tt-docum-est.pais-origem      = "re1001"
    
                   tt-docum-est.cidade           = emitente.cidade
                   tt-docum-est.uf               = emitente.estado
                   tt-docum-est.pais             = emitente.pais
                   tt-docum-est.bairro           = emitente.bairro
                   tt-docum-est.cep              = emitente.cep
                   tt-docum-est.endereco         = emitente.endereco
                   tt-docum-est.cod-entrega      = emitente.cod-entrega.

    IF vlo-debug THEN
    MESSAGE "ponto 4 " SKIP(1)
        'tt-docum-est.tot-valor        = de-base-calculo-ori - de-cdo-unit' tt-docum-est.tot-valor SKIP
        'de-base-calculo-ori' de-base-calculo-ori SKIP
        'de-cdo-unit' de-cdo-unit
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

                   /*totvs12 teste
                    MESSAGE "es4010a " cta_ctbl.cod_cta_ctbl SKIP
                            "tt-docum-est.conta-transit "  tt-docum-est.conta-transit    SKIP
                            "tt-docum-est.ct-transit    "  tt-docum-est.ct-transit       
                            VIEW-AS ALERT-BOX INFO BUTTONS OK.
                   */

                                                         
        END. /* l-zera */
    
        if substring(es-ticket.u-char-3,6,16)   <> "" then assign i-conta-item = i-conta-item + 1.    
        if substring(es-ticket.u-char-3,39,16)  <> "" then assign i-conta-item = i-conta-item + 1.
        if substring(es-ticket.u-char-3,72,16)  <> "" then assign i-conta-item = i-conta-item + 1.
        if substring(es-ticket.u-char-3,105,16) <> "" then assign i-conta-item = i-conta-item + 1.
        if substring(es-ticket.u-char-3,138,16) <> "" then assign i-conta-item = i-conta-item + 1.
        if substring(es-ticket.u-char-3,171,16) <> "" then assign i-conta-item = i-conta-item + 1.
        if substring(es-ticket.u-char-3,204,16) <> "" then assign i-conta-item = i-conta-item + 1.
        if substring(es-ticket.u-char-3,237,16) <> "" then assign i-conta-item = i-conta-item + 1.
        if substring(es-ticket.u-char-3,270,16) <> "" then assign i-conta-item = i-conta-item + 1.
        if substring(es-ticket.u-char-3,303,16) <> "" then assign i-conta-item = i-conta-item + 1.       
        if substring(es-ticket.u-char-3,336,16) <> "" then assign i-conta-item = i-conta-item + 1.
            
        IF es-ticket.operacao =  8 or
           es-ticket.operacao =  9 or
           es-ticket.operacao = 10  THEN DO:
     
            if substring(es-ticket.u-char-3,6,16) <> "" then do:
                run pi-tt-item.
                assign
                   /*tt-item-doc-est.seq-comp         = tt-item-doc-est.sequencia*/
                   tt-item-doc-est.serie-comp       = trim(substring(es-ticket.u-char-3,1,5)) /* serie-docto */
                   tt-item-doc-est.nro-comp         = trim(substring(es-ticket.u-char-3,6,16)) /* nro-docto */
                   tt-item-doc-est.nat-comp         = trim(substring(es-ticket.u-char-3,22,6)) /* nat-operacao */.        
                
            end.
        
            if substring(es-ticket.u-char-3,39,16) <> "" then do:
                run pi-tt-item.
                assign
                   /*tt-item-doc-est.seq-comp         = tt-item-doc-est.sequencia*/
                   tt-item-doc-est.serie-comp       = trim(substring(es-ticket.u-char-3,34,5)) /* serie-docto */
                   tt-item-doc-est.nro-comp         = trim(substring(es-ticket.u-char-3,39,16)) /* nro-docto */
                   tt-item-doc-est.nat-comp         = trim(substring(es-ticket.u-char-3,55,6)) /* nat-operacao */.        
            end.
        
            if substring(es-ticket.u-char-3,72,16) <> "" then do:
                run pi-tt-item.
                assign
                   /*tt-item-doc-est.seq-comp         = tt-item-doc-est.sequencia*/
                   tt-item-doc-est.serie-comp       = trim(substring(es-ticket.u-char-3,67,5))  /* serie-docto */
                   tt-item-doc-est.nro-comp         = trim(substring(es-ticket.u-char-3,72,16)) /* nro-docto */
                   tt-item-doc-est.nat-comp         = trim(substring(es-ticket.u-char-3,88,6))  /* nat-operacao */.        
            end.
        
            if substring(es-ticket.u-char-3,105,16) <> "" then do:
                run pi-tt-item.
                assign
                   /*tt-item-doc-est.seq-comp         = tt-item-doc-est.sequencia*/
                   tt-item-doc-est.serie-comp       = trim(substring(es-ticket.u-char-3,100,5)) /* serie-docto */
                   tt-item-doc-est.nro-comp         = trim(substring(es-ticket.u-char-3,105,16)) /* nro-docto */
                   tt-item-doc-est.nat-comp         = trim(substring(es-ticket.u-char-3,121,6)) /* nat-operacao */.        
            end.
        
        
            if substring(es-ticket.u-char-3,138,16) <> "" then do:
                run pi-tt-item.
                assign
                   /*tt-item-doc-est.seq-comp         = tt-item-doc-est.sequencia*/
                   tt-item-doc-est.serie-comp       = trim(substring(es-ticket.u-char-3,133,5)) /* serie-docto */
                   tt-item-doc-est.nro-comp         = trim(substring(es-ticket.u-char-3,138,16)) /* nro-docto */
                   tt-item-doc-est.nat-comp         = trim(substring(es-ticket.u-char-3,154,6)) /* nat-operacao */.        
            end.
        
        
            if substring(es-ticket.u-char-3,171,16) <> "" then do:
                run pi-tt-item.
                assign
                   /*tt-item-doc-est.seq-comp         = tt-item-doc-est.sequencia*/
                   tt-item-doc-est.serie-comp       = trim(substring(es-ticket.u-char-3,166,5)) /* serie-docto */
                   tt-item-doc-est.nro-comp         = trim(substring(es-ticket.u-char-3,171,16)) /* nro-docto */
                   tt-item-doc-est.nat-comp         = trim(substring(es-ticket.u-char-3,187,6)) /* nat-operacao */.        
            end.
        
            if substring(es-ticket.u-char-3,204,16) <> "" then do:
                run pi-tt-item.
                assign
                   /*tt-item-doc-est.seq-comp         = tt-item-doc-est.sequencia*/
                   tt-item-doc-est.serie-comp       = trim(substring(es-ticket.u-char-3,199,5)) /* serie-docto */
                   tt-item-doc-est.nro-comp         = trim(substring(es-ticket.u-char-3,204,16)) /* nro-docto */
                   tt-item-doc-est.nat-comp         = trim(substring(es-ticket.u-char-3,220,6)) /* nat-operacao */.        
            end.
        
            if substring(es-ticket.u-char-3,237,16) <> "" then do:
                run pi-tt-item.
                assign
                   /*tt-item-doc-est.seq-comp         = tt-item-doc-est.sequencia*/
                   tt-item-doc-est.serie-comp       = trim(substring(es-ticket.u-char-3,232,5)) /* serie-docto */
                   tt-item-doc-est.nro-comp         = trim(substring(es-ticket.u-char-3,237,16)) /* nro-docto */
                   tt-item-doc-est.nat-comp         = trim(substring(es-ticket.u-char-3,253,6)) /* nat-operacao */.        
            end.
           
        
            if substring(es-ticket.u-char-3,270,16) <> "" then do:
                run pi-tt-item.
                assign
                   /*tt-item-doc-est.seq-comp         = tt-item-doc-est.sequencia*/
                   tt-item-doc-est.serie-comp       = trim(substring(es-ticket.u-char-3,265,5)) /* serie-docto */
                   tt-item-doc-est.nro-comp         = trim(substring(es-ticket.u-char-3,270,16)) /* nro-docto */
                   tt-item-doc-est.nat-comp         = trim(substring(es-ticket.u-char-3,286,6)) /* nat-operacao */.        
            end.
        
            if substring(es-ticket.u-char-3,303,16) <> "" then do:
                run pi-tt-item.
                assign
                   /*tt-item-doc-est.seq-comp         = tt-item-doc-est.sequencia*/
                   tt-item-doc-est.serie-comp       = trim(substring(es-ticket.u-char-3,298,5)) /* serie-docto */
                   tt-item-doc-est.nro-comp         = trim(substring(es-ticket.u-char-3,303,16)) /* nro-docto */
                   tt-item-doc-est.nat-comp         = trim(substring(es-ticket.u-char-3,319,6)) /* nat-operacao */.        
            end.
               
            if substring(es-ticket.u-char-3,336,16) <> "" then do:
                run pi-tt-item.
                assign
                   /*tt-item-doc-est.seq-comp         = tt-item-doc-est.sequencia*/
                   tt-item-doc-est.serie-comp       = trim(substring(es-ticket.u-char-3,331,5)) /* serie-docto */
                   tt-item-doc-est.nro-comp         = trim(substring(es-ticket.u-char-3,336,16)) /* nro-docto */
                   tt-item-doc-est.nat-comp         = trim(substring(es-ticket.u-char-3,352,6)) /* nat-operacao */.        
            end.
               
       end. /* armazenagem */       
       else do:
         i-conta-item = 1.
         run pi-tt-item.
        
       end.
    
    
            
        FOR EACH  tt_erros_modulo:
            DELETE tt_erros_modulo.
        END.  
    
        CREATE tt_versao_efetiv_edi.
        ASSIGN tt_versao_efetiv_edi.tta_cdn_versao_integracao = 001.
         
        FIND FIRST natur-oper-ext  
            WHERE natur-oper-ext.cod-natur-oper = es-ticket.nat-operacao NO-LOCK NO-ERROR.
            
        IF  NOT AVAIL natur-oper-ext THEN DO:
            
            CREATE natur-oper-ext.
            ASSIGN natur-oper-ext.cod-natur-oper = es-ticket.nat-operacao.
            
        END.
        FIND  estabelec-ext
        WHERE estabelec-ext.cod-estabel = es-ticket.cod-estabel NO-LOCK NO-ERROR.
            
        IF  NOT AVAIL estabelec-ext THEN DO:
    
            CREATE estabelec-ext.                                        
            ASSIGN estabelec-ext.cod-estabel = es-ticket.cod-estabel.

        END.

        /* Se o estabelecimento estiver parametrizado para utilizar   **
        ** a CONTRA-NOTA, n∆o precisa validar a natureza de operaá∆o. */
        IF estabelec-ext.log-gera-contra-nota = YES THEN DO:
        
            /* Atual API Contra-nota: */                 
            RUN esp/esapi004.p (INPUT es-ticket.nr-ticket,
                                INPUT  TABLE tt_versao_efetiv_edi,
                                INPUT  TABLE tt-docum-est,
                                INPUT  TABLE tt-item-doc-est,
                                OUTPUT TABLE tt_erros_modulo).
        END.
        ELSE DO: 
            /* Se o estabelecimento N«O estiver parametrizado para utilizar **
            ** a CONTRA-NOTA, valida a natureza de operaá∆o.                */
            IF natur-oper-ext.log-gera-contra-nota = YES THEN DO: /* Natureza Ç de CONTRA-NOTA */
                /* Atual API Contra-nota: */                 
                RUN esp/esapi004.p (INPUT es-ticket.nr-ticket,
                                    INPUT  TABLE tt_versao_efetiv_edi,
                                    INPUT  TABLE tt-docum-est,
                                    INPUT  TABLE tt-item-doc-est,
                                    OUTPUT TABLE tt_erros_modulo).    
            END.
            ELSE DO: /* Natureza n∆o Ç de CONTRA-NOTA */
                /* Nova API que ser† implementada: */
                RUN esp/esapi013.p (INPUT es-ticket.nr-ticket,
                                    INPUT  TABLE tt_versao_efetiv_edi,
                                    INPUT  TABLE tt-docum-est,
                                    INPUT  TABLE tt-item-doc-est,
                                    OUTPUT TABLE tt_erros_modulo).
            END.
        END.

        FOR EACH tt_erros_modulo
            WHERE tt_erros_modulo.cod-erro <> 5268:
    
            CREATE tt-erro.
            ASSIGN tt-erro.i-sequen = tt_erros_modulo.num-sequencia-erro
                   tt-erro.cd-erro  = tt_erros_modulo.cod-erro
                   tt-erro.mensagem = tt_erros_modulo.des-erro.         
        END.
    
        FOR EACH tt_erros_modulo
            WHERE tt_erros_modulo.cod-erro = 5268:
    
            RUN esp/MESSAGE3.p (INPUT "Nota Fiscal alterada!",
                                INPUT tt_erros_modulo.des-erro). 
        END.
    
        FIND FIRST tt-erro NO-ERROR .
        IF AVAILABLE(tt-erro) THEN DO:
            MESSAGE "Verificar tela de erros." VIEW-AS ALERT-BOX.
            RUN esp/es4010c.w (INPUT TABLE tt-erro).
        END.
        ELSE DO:
            
            FIND docum-est WHERE ROWID(docum-est) = r-RE0301-documento EXCLUSIVE-LOCK NO-ERROR.
            IF  AVAIL docum-est THEN DO:                
                     
    /*   /* Definicao da aliquota de icms */
       run rep/re9991.p (rowid(docum-est), output de-aliq-icm).
       run rep/re9994.p (input rowid(item-doc-est)). /* ICMS */

    */
     
                /* grava transp e placas */
                 ASSIGN /*docum-est.mod-frete                 = 2 /* modalidade FOB - sempre fob para frete compra */ s¢ TMS */
                        docum-est.nome-transp  = es-ticket.u-char-1     /* transportadora */
                        docum-est.cod-placa[1] = es-ticket.nr-placa-cam /* placa 1 */
                        docum-est.cod-placa[2] = es-ticket.nr-placa-car1 /* placa 2 */
                        docum-est.cod-placa[3] = es-ticket.nr-placa-car2 /* placa 3 */.
        
             /* IF AVAIL conta-contab THEN DO:
                    ASSIGN docum-est.conta-transit     = conta-contab.conta-contab
                           docum-est.ct-transit        = conta-contab.ct-codigo
                           docum-est.sc-transit        = conta-contab.sc-codigo.
                END. */
                IF AVAIL cta_ctbl THEN DO:
                    ASSIGN docum-est.conta-transit     = "" /*cta_ctbl.cod_cta_ctbl*/
                           docum-est.ct-transit        = cta_ctbl.cod_cta_ctbl
                           docum-est.sc-transit        = "".
                END.

                /*
                MESSAGE "ELSE DO " (IF AVAIL cta_ctbl THEN  cta_ctbl.cod_cta_ctbl ELSE "cta_ctbl not avail") SKIP
                        "docum-est.conta-transit" docum-est.conta-transit SKIP
                        "docum-est.ct-transit   " docum-est.ct-transit    SKIP
                        "docum-est.sc-transit   " docum-est.sc-transit    SKIP
                        VIEW-AS ALERT-BOX INFO BUTTONS OK.
                */
                FOR EACH item-doc-est OF docum-est EXCLUSIVE-LOCK:
                     
                    /* PIS-COFINS 05/04/2011 para a entrada */
                    IF (NOT(SUBSTRING(ITEM.char-2,32,4) = "") OR
                        NOT(SUBSTRING(ITEM.char-2,37,4) = "")) AND 
                       es-ticket.operacao <> 10 THEN DO:
    
                             /* era Washington pediu pra pegar tributaá∆o do item e n∆o da nat IF natur-oper.perc-pis[1] <> 0 THEN DO: 14/11/12 */                                    
                              ASSIGN 
                                 de-total-com-imp                  = round(de-base-calculo-ori /* retirando cei 16/05/2013 + de-funrural-unit */ + de-cdo-unit + de-senar-unit,2)
                                 item-doc-est.idi-tributac-pis     = 1 /* tributado  "tipo pis" era "1", passei para numÇrico 22/11/2011 erika */
                                 item-doc-est.val-aliq-pis         = dec(substring(ITEM.char-2,32,4))  /* "alq pis" */  /* alterado como es4027 14/11/12 dec(string(natur-oper.perc-pis[1])) /* aliquota do pis */*/                             
                                 item-doc-est.base-pis             = dec(string(de-total-com-imp))  /* subst de-toatl por de-total-com-imp - 14/11/12 alterando para os novos campos da base, n∆o mais strings base pis erika alterei para o anterior pq n∆o passa */
                                 item-doc-est.valor-pis            = dec(string(de-total-com-imp * (dec(substring(ITEM.char-2,32,4)) / 100)))  /* "val pis"  */                           
    
                                 item-doc-est.idi-tributac-cofins  = 1 /* tributado  "tipo cofins" */
                                 item-doc-est.val-aliq-cofins      = DEC(substring(ITEM.char-2,37,4))  /* "alq cofins" *//* erika 14/11/12 dec(string(natur-oper.per-fin-soc[1]))  /* "alq cofins" */*/
                                 item-doc-est.val-base-calc-cofins = dec(string(de-total-com-imp)) /* subst de-toatl por de-total-com-imp -"base cofins"*/
                                 item-doc-est.val-cofins           = dec(string(de-total-com-imp * (dec(substring(ITEM.char-2,37,4)) / 100))) /* "val cofins" */.
                    
    
    
                     END. /* PIS-COFINS 14/11/12 - 05/04/2011 para a entrada */
                        
                        
                   
                    
                    if  es-ticket.operacao <> 8 AND es-ticket.operacao <> 10 then do:
                       
                      ASSIGN item-doc-est.peso-liquido  = IF emitente.natureza = 1 THEN (de-quantidade / i-conta-item) ELSE 
                                                          IF es-ticket.peso-desmem[1] > 0 THEN (es-ticket.peso-desmem[1] / i-conta-item) 
                                                             ELSE (de-quantidade / i-conta-item) 
                             item-doc-est.base-icm[1]   = 0
                             item-doc-est.base-icm[2]   = 0
                             item-doc-est.icm-outras[1] = (de-total-com-imp / i-conta-item) /* subst de-toatl por de-total-com-imp - 16/11/12 */ 
                             item-doc-est.icm-outras[2] = 0 
                             item-doc-est.base-ipi[1]   = 0 
                             item-doc-est.base-ipi[2]   = 0
                             item-doc-est.ipi-outras[1] = (de-total-com-imp / i-conta-item) 
                             item-doc-est.ipi-outras[2] = 0
                             item-doc-est.base-iss[1]   = 0
                             item-doc-est.base-iss[2]   = 0
                             item-doc-est.base-subs[1]  = 0
                             item-doc-est.base-subs[2]  = 0.    
                             
                    end. /* <> 8 */
                    
                    /* Geracao da Ordem de Compra do Item informado no Ticket */
    
                    ASSIGN i-num-pedido  = 0
                           i-num-ordem   = 0
                           i-parcela     = 0.
            
                    IF  es-ticket.operacao = 1 or es-ticket.operacao = 4 THEN DO:
                        RUN esp/es4010d.p (input es-ticket.nr-ticket, input de-quantidade).
                    END. 
    
                    ASSIGN item-doc-est.num-pedido   = i-num-pedido
                           item-doc-est.numero-ordem = i-num-ordem
                           item-doc-est.parcela      = i-parcela
                           item-doc-est.serie-comp   = tt-item-doc-est.serie-comp.                                
    
                    /* ------------------------------------------------------- */
    
                    IF l-zera = NO THEN do:
                      create rat-lote.
                      assign 
                           rat-lote.serie-docto    = item-doc-est.serie-docto
                           rat-lote.cod-emitente   = item-doc-est.cod-emitente
                           rat-lote.nro-docto      = string(item-doc-est.nro-docto,"9999999")
                           rat-lote.nat-operacao   = item-doc-est.nat-operacao
                           rat-lote.it-codigo      = item-doc-est.it-codigo
                           rat-lote.sequencia      = item-doc-est.sequencia
                           rat-lote.cod-depos      = item-doc-est.cod-depos
                           rat-lote.cod-localiz    = item-doc-est.cod-localiz
                           rat-lote.dt-vali-lote   = item-doc-est.dt-vali-lote  
                           rat-lote.lote           = item-doc-est.lote
                           rat-lote.quantidade     = item-doc-est.quantidade.
                    END. /*l-zera */


                    IF es-ticket.operacao = 10 THEN do: /* somente transferencia 12/08/2013 Mauricio Fatima */
                                
                         FIND FIRST b-natur-oper WHERE b-natur-oper.nat-operacao = trim(substring(es-ticket.u-char-3,22,6)) NO-LOCK NO-ERROR.
                         IF AVAIL b-natur-oper THEN DO:
                              /* c¢digo da tributaá∆o vem sempre da natureza, conforme F†tima 08/08/2013 */
                              ASSIGN item-doc-est.idi-tributac-pis    = int(substring(b-natur-oper.char-1,86,1)) /* cod tribtac pis 1-trib, 2-isento, 3-outros, 4-reduz */
                                     item-doc-est.idi-tributac-cofins = int(substring(b-natur-oper.char-1,87,1)) /* cod tribtac cofins 1-trib, 2-isento, 3-outros, 4-reduz */ .
                         END.
            
                         FIND FIRST it-nota-fisc WHERE
                                    it-nota-fisc.cod-estabel =  trim(substring(es-ticket.u-char-3,31,3)) AND
                                    it-nota-fisc.serie       =  trim(substring(es-ticket.u-char-3,1,5)) AND
                                    it-nota-fisc.nr-nota-fis =  trim(substring(es-ticket.u-char-3,6,16)) NO-LOCK NO-ERROR.
                         IF AVAIL it-nota-fisc THEN DO:
                             /* coloca informaá∆o da nota de origem para ICMS - fATIMA E mAURICIO 19/08/2013 */
    
                                
                             ASSIGN item-doc-est.aliquota-icm         = it-nota-fisc.aliquota-icm
                                    item-doc-est.cd-trib-icm          = it-nota-fisc.cd-trib-icm
                                    item-doc-est.val-perc-red-icms    = natur-oper.perc-red-icm. /* erika 05/09/2013 */
    
                             IF natur-oper.perc-red-icm <> 0 THEN DO:
                                ASSIGN
                                    docum-est.aliquota-icm    = it-nota-fisc.aliquota-icm
                                    docum-est.icm-deb-cre     = /*de-total*/ de-base-calculo-ori - de-cdo-unit * (it-nota-fisc.aliquota-icm - (it-nota-fisc.aliquota-icm * natur-oper.perc-red-icm / 100)) / 100 /* 03/09/2013 para ajuste Mauricio - Fatima */
                                    item-doc-est.valor-icm[1] = /*de-total */ de-base-calculo-ori - de-cdo-unit * (it-nota-fisc.aliquota-icm - (it-nota-fisc.aliquota-icm * natur-oper.perc-red-icm / 100)) / 100 /* 03/09/2013 para ajuste Mauricio - Fatima */
                                    item-doc-est.base-icm[1]  = /*de-total */ (de-base-calculo-ori - de-cdo-unit * de-base-calculo-ori - de-cdo-unit * (100 - DEC(natur-oper.perc-red-icm))  / 100) /* erika 03/09/2013 */.
                             END.
                             ELSE DO:
                                 ASSIGN
                                    docum-est.aliquota-icm    = it-nota-fisc.aliquota-icm
                                    docum-est.icm-deb-cre     = (/*de-total*/ de-base-calculo-ori - de-cdo-unit * it-nota-fisc.aliquota-icm) / 100 /* 03/09/2013 para ajuste Mauricio - Fatima */
                                    item-doc-est.valor-icm[1] = (/*de-total*/ de-base-calculo-ori - de-cdo-unit * it-nota-fisc.aliquota-icm) / 100 /* 03/09/2013 para ajuste Mauricio - Fatima */
                                    item-doc-est.base-icm[1]  =  de-base-calculo-ori - de-cdo-unit /*de-total*/                                    /* erika 03/09/2013 */.
                             END.
                         END.    
                    END.  

                                 

                    IF es-ticket.operacao = 1 THEN do: /* somente compra 07/11/2013 Erika e Ramon */
                       
                      /*VERIFICAÄ«O DE ALIQUOTAS ICMS PIS e COFINS */
                        FIND FIRST emitente     WHERE emitente.cod-emitente     = docum-est.cod-emitente  NO-LOCK NO-ERROR.
                        FIND FIRST estabelec    WHERE estabelec.cod-estabel     = docum-est.cod-estabel   NO-LOCK NO-ERROR.
                        FIND FIRST ITEM         WHERE ITEM.it-codigo            = item-doc-est.it-codigo  NO-LOCK NO-ERROR.
                        FIND FIRST b-natur-oper WHERE b-natur-oper.nat-operacao = item-doc-est.nat-operacao NO-LOCK NO-ERROR.

                      /*ALIQUOTA ICMS*/
                        RUN cdp/cd4301.p (INPUT emitente.contrib-icms,
                                          INPUT emitente.natureza,
                                          INPUT estabelec.estado,
                                          INPUT estabelec.pais,
                                          INPUT estabelec.estado, /* considera venda dentro do estado */
                                          INPUT item-doc-est.it-codigo,
                                          INPUT b-natur-oper.aliquota-icm,
                                          OUTPUT d-aliquota-icm).


                      /*CD TRIBUT ICMS*/
                        RUN cdp/cd4302.p ( INPUT item.cd-trib-icm,
                                           INPUT b-natur-oper.cd-trib-icm,
                                           OUTPUT i-cd-trib-icm).

                      /*IPI codigo de tributacao*/
                        RUN cdp/cd4303a.p (INPUT item.cd-trib-ipi,
                                           INPUT b-natur-oper.cd-trib-ipi,
                                           INPUT SUBSTR(emitente.char-1,21,1),
                                           INPUT SUBSTR(item.char-2,51,1),
                                           OUTPUT i-cd-trib-ipi).
                      /*ALIQ PIS*/
                        IF SUBSTRING(ITEM.char-2,52,1) = "1" THEN /*1=item 2=natureza*/
                            ASSIGN d-perc-pis = DEC(SUBSTRING(ITEM.char-2,31,5)).
                        ELSE
                            ASSIGN d-perc-pis = b-natur-oper.perc-pis[1] .
                      /*ALIQ COFINS*/
                        IF SUBSTRING(ITEM.char-2,53,1) = "1" THEN /*1=item 2=natureza*/
                            ASSIGN d-perc-cof = DEC(SUBSTRING(ITEM.char-2,36,5)).
                        ELSE
                            ASSIGN d-perc-cof= b-natur-oper.per-fin-soc[1].  
 
                      /*DEFINICAO DO CST DO ICMS*/
                        IF  i-cd-trib-icm = 3
                        AND INT(b-natur-oper.ind-it-sub-dif) = ? THEN  /* 1 = suspenso  ? = diferido - Item ICMS Suspenso Item ICMS Diferido */  
                            ASSIGN c-cst-icms = "51".
                        ELSE
                          IF  i-cd-trib-icm = 3
                          AND INT(b-natur-oper.ind-it-sub-dif) = 1 THEN  /* 1 = suspenso  ? = diferido - Item ICMS Suspenso Item ICMS Diferido */
                              ASSIGN c-cst-icms = "50".                  /* 50 - Suspens∆o                                                           */
                          ELSE                                           /*     O parÉmetro "Item ICMS Suspenso" do CD0606, pasta ICMS est† marcado. */
                            IF i-cd-trib-icm = 3 THEN                  /* 90 - Outras                                      */   
                                ASSIGN c-cst-icms = "90".              /*      O resultado da tributaá∆o do Item Ç "Outros". */ 
                            ELSE
                              IF  (i-cd-trib-icm = 2 OR                    /* 30 - Isenta ou                                        */ 
                                   b-natur-oper.ind-tipo-vat = YES)        /*      n∆o tributada e                                  */ 
                              AND b-natur-oper.subs-trib THEN              /*      com cobranáa do ICMS por substituiá∆o tribut†ria */ 
                                  ASSIGN c-cst-icms = "30".
                              ELSE
                                IF  i-cd-trib-icm = 2                             /* 41 - N∆o tributada                                   */  
                                AND b-natur-oper.ind-tipo-vat = YES THEN          /*      O resultado da tributaá∆o do item Ç "Isento"    */  
                                    ASSIGN c-cst-icms = "41".                     /*      e o parÉmetro "N∆o Tributada (ICMS)" do CD0606, */  
                                ELSE                                              /*                      (pasta ICMS est† marcado).      */  
                                  IF i-cd-trib-icm = 2 THEN
                                      ASSIGN c-cst-icms = "40".   /* 40 - Isenta */
                                  ELSE
                                    IF  b-natur-oper.perc-red-icm > 0                    /* 70 - Com reduá∆o de base de c†lculo e    */          
                                    AND b-natur-oper.subs-trib    = YES THEN             /*      cobranáa do ICMS por Substituiá∆o tribut†ria */ 
                                        ASSIGN c-cst-icms = "70".
                                    ELSE
                                      IF  b-natur-oper.perc-red-icm > 0 THEN
                                          ASSIGN c-cst-icms = "20".                   /* 20 - Com reduá∆o de base de calculo */
                                      ELSE
                                        IF b-natur-oper.ind-it-icms THEN    /* 60 - ICMS cobrado anteriormente por substituiá∆o tribut†ria    */                           
                                            ASSIGN c-cst-icms = "60".       /*      O parÉmetro "Item ICMS Cobrado Subs Tribut†ria" do CD0606,pasta ICMS est† marcado. */
                                        ELSE
                                          IF  i-cd-trib-icm          = 1
                                          AND b-natur-oper.subs-trib = YES THEN
                                              ASSIGN c-cst-icms = "10".             /* 10 - Tributaá∆o e com cobranáa do ICMS por substituiá∆o tributaria */
                                          ELSE
                                            IF  i-cd-trib-icm = 1 THEN
                                                ASSIGN c-cst-icms = "00".           /* 00 - Tributaá∆o integralmente */
                                            ELSE 
                                                ASSIGN c-cst-icms = "  ".    /* N∆o encontrado */
                                   
                       /*ICMS*/      
                        ASSIGN item-doc-est.aliquota-icm         = d-aliquota-icm
                               item-doc-est.cd-trib-icm          = i-cd-trib-icm
                               item-doc-est.val-perc-red-icms    = b-natur-oper.perc-red-icm .
                                                
                        ASSIGN SUBSTRING(item-doc-est.char-2,502,3) = c-cst-icms .

                        /*ACERTO DA BASE DE IMPOSTOS*/
                        ASSIGN de-preco-unit-aux    = (de-total-com-imp / item-doc-est.quantidade)
                               de-total-com-imp-cor = ROUND(de-preco-unit-aux * item-doc-est.quantidade,2) .
 
                        IF b-natur-oper.perc-red-icm <> 0 THEN DO:
                           ASSIGN
                               docum-est.aliquota-icm    = it-nota-fisc.aliquota-icm
                               docum-est.icm-deb-cre     = (de-total-com-imp-cor * (item-doc-est.aliquota-icm - (item-doc-est.aliquota-icm * b-natur-oper.perc-red-icm / 100)) / 100) 
                               item-doc-est.valor-icm[1] = (de-total-com-imp-cor * (item-doc-est.aliquota-icm - (item-doc-est.aliquota-icm * b-natur-oper.perc-red-icm / 100)) / 100) 
                               item-doc-est.base-icm[1]  = (de-total-com-imp-cor * (100 - DEC(b-natur-oper.perc-red-icm))  / 100)  .
                        END.
                        ELSE DO:
                            ASSIGN docum-est.aliquota-icm    = item-doc-est.aliquota-icm
                                   docum-est.icm-deb-cre     = (de-total-com-imp-cor * item-doc-est.aliquota-icm) / 100 
                                   item-doc-est.valor-icm[1] = (de-total-com-imp-cor * item-doc-est.aliquota-icm) / 100 
                                   item-doc-est.base-icm[1]  = (de-total-com-imp-cor * item-doc-est.aliquota-icm) / 100 .
                        END.
 
 

                      /* PIS COFINS */
                        IF SUBSTRING(b-natur-oper.char-1,86,1) <> "2" AND 
                           SUBSTRING(b-natur-oper.char-1,86,1) <> "3" THEN DO: /*cd-trib-pis*/ 
                            ASSIGN  item-doc-est.valor-pis  = ROUND( ( de-total-com-imp-cor * d-perc-pis / 100 ),2) .   
                        END.
            
                        IF SUBSTRING(b-natur-oper.char-1,87,1) <> "2" AND  
                           SUBSTRING(b-natur-oper.char-1,87,1) <> "3" THEN DO: /*cd-trib-cofins*/ 
                            ASSIGN item-doc-est.val-cofins = ROUND( ( de-total-com-imp-cor * d-perc-cof / 100 ),2).   
                        END.
                        ASSIGN item-doc-est.val-aliq-pis         = d-perc-pis        
                               item-doc-est.idi-tributac-pis     = INT (SUBSTRING(b-natur-oper.char-1,86,1))
                               item-doc-est.base-pis             = de-total-com-imp-cor 
                               item-doc-est.val-aliq-cofins      = d-perc-cof
                               item-doc-est.idi-tributac-cofins  = int(SUBSTRING(b-natur-oper.char-1,87,1))
                               item-doc-est.val-base-calc-cofins = de-total-com-imp-cor .

                        /*ACERTO BASE IPI */
                        ASSIGN item-doc-est.base-ipi[1]   = 0
                               item-doc-est.ipi-outras[1] = 0
                               item-doc-est.base-ipi[2]   = 0 
                               item-doc-est.ipi-outras[2] = 0.
                        IF i-cd-trib-ipi  = 1 THEN ASSIGN item-doc-est.base-ipi[1]   = de-total-com-imp-cor.
                        IF i-cd-trib-ipi  = 3 THEN ASSIGN item-doc-est.ipi-outras[1] = de-total-com-imp-cor.

                       
                    END.  
 
                END. /* item-doc-est */

                RELEASE item-doc-est.
                
    
                /* erika */
                IF es-ticket.operacao =  8 or
                   es-ticket.operacao =  9 or
                   es-ticket.operacao = 10 THEN DO:
    
                 for each item-doc-est OF docum-est no-lock:
                   find first natur-oper where
                              natur-oper.nat-operacao = es-ticket.nat-operacao
                              no-lock no-error.
                    if not avail natur-oper then do:
                        message "Natureza de Operaá∆o n∆o Cadastrada!" view-as alert-box error.
                        return no-apply.    
                    end.    
                    if natur-oper.terceiros or 
                       natur-oper.transf /* transf */ then do: 
                        IF l-zera = NO THEN DO:
                        
                          create unid-neg-nota.
                          assign unid-neg-nota.cod-emitente     = docum-est.cod-emitente          
                                 unid-neg-nota.serie-docto    = docum-est.serie-docto
                                 unid-neg-nota.nro-docto      = docum-est.nro-docto
                                 unid-neg-nota.nat-operacao   = docum-est.nat-operacao       
                                 unid-neg-nota.sequencia      = item-doc-est.sequencia 
                                 unid-neg-nota.cod_unid_negoc = trim(substring(es-ticket.u-char-3,28,3))
                                 unid-neg-nota.perc-unid-neg  = 100.
                                                          
                        /* NOTA CORRENTE*/
                          FIND saldo-terc
                            WHERE saldo-terc.serie-docto  = item-doc-est.serie-docto
                              AND saldo-terc.nro-docto    = string(i-nr-nota,"9999999")
                              AND saldo-terc.cod-emitente = item-doc-est.cod-emitente
                              AND saldo-terc.nat-operacao = item-doc-est.nat-operacao
                              AND saldo-terc.it-codigo    = item-doc-est.it-codigo
                              AND saldo-terc.cod-refer    = item-doc-est.cod-refer
                              AND saldo-terc.sequencia    = item-doc-est.sequencia  
                            NO-ERROR.
                          IF  NOT AVAIL saldo-terc THEN DO:
    
                            /*CREATE saldo-terc.
                            ASSIGN saldo-terc.serie-docto    = item-doc-est.serie-docto                 
                                   saldo-terc.nro-docto      = string(i-nr-nota,"9999999")
                                   saldo-terc.cod-emitente   = item-doc-est.cod-emitente                
                                   saldo-terc.nat-operacao   = item-doc-est.nat-operacao                
                                   saldo-terc.it-codigo      = item-doc-est.it-codigo                   
                                   saldo-terc.cod-refer      = item-doc-est.cod-refer                   
                                   saldo-terc.sequencia      = item-doc-est.sequencia
                                   saldo-terc.cod-depos      = item-doc-est.cod-depos
                                   saldo-terc.cod-estabel    = docum-est.cod-estabel
                                   saldo-terc.quantidade     = item-doc-est.quantidade
                                   saldo-terc.tipo           = YES  /*** CONSIGNACAO ***/
                                   saldo-terc.tipo-sal-terc  = 5    /*** ENTRADA CONSIGNACAO ***/
                                   saldo-terc.tipo-valor     = 2. /**/ 250705 - solicitado Helder */
                          END.
    
                        END. /* l-zera */
                    end. /* natur */
                 end. /* for each item */
                 
                 assign
                    c-serie-comp       = ""
                    c-nro-comp         = ""
                    c-nat-comp         = ""
                    de-saldo           = es-ticket.peso-desmem[1].
    
                 if natur-oper.terceiros or 
                    natur-oper.transf /* transf */ then do: 
                                                                                                       
                    if substring(es-ticket.u-char-3,6,16) <> "" then do:
                          assign
                             c-serie-comp       = trim(substring(es-ticket.u-char-3,1,5)) /* serie-docto */
                             c-nro-comp         = trim(substring(es-ticket.u-char-3,6,16)) /* nro-docto */
                             c-nat-comp         = trim(substring(es-ticket.u-char-3,22,6)) /* nat-operacao */.                                 
                          run pi-saldo-comp.
                    end.
          
                    if substring(es-ticket.u-char-3,39,16) <> "" then do:
                       
                       assign
                          c-serie-comp       = trim(substring(es-ticket.u-char-3,34,5)) /* serie-docto */
                          c-nro-comp         = trim(substring(es-ticket.u-char-3,39,16)) /* nro-docto */
                          c-nat-comp         = trim(substring(es-ticket.u-char-3,55,6)) /* nat-operacao */.        
                       run pi-saldo-comp.
                    end.
                      
                    if substring(es-ticket.u-char-3,72,16) <> "" then do:
                        assign
                        c-serie-comp       = trim(substring(es-ticket.u-char-3,67,5)) /* serie-docto */
                        c-nro-comp         = trim(substring(es-ticket.u-char-3,72,16)) /* nro-docto */
                        c-nat-comp         = trim(substring(es-ticket.u-char-3,88,6)) /* nat-operacao */.        
                        run pi-saldo-comp.   
                    end.
                      
                    if substring(es-ticket.u-char-3,105,16) <> "" then do:
                              assign
                                 c-serie-comp       = trim(substring(es-ticket.u-char-3,100,5)) /* serie-docto */
                                 c-nro-comp         = trim(substring(es-ticket.u-char-3,105,16)) /* nro-docto */
                                 c-nat-comp         = trim(substring(es-ticket.u-char-3,121,6)) /* nat-operacao */.        
                              run pi-saldo-comp.      
                    end.
                  
                  
                    if substring(es-ticket.u-char-3,138,16) <> "" then do:
                          assign
                             c-serie-comp       = trim(substring(es-ticket.u-char-3,133,5)) /* serie-docto */
                             c-nro-comp         = trim(substring(es-ticket.u-char-3,138,16)) /* nro-docto */
                             c-nat-comp         = trim(substring(es-ticket.u-char-3,154,6)) /* nat-operacao */.        
                          run pi-saldo-comp.                               
                    end.                  
                  
                    if substring(es-ticket.u-char-3,171,16) <> "" then do:
                          assign
                             c-serie-comp       = trim(substring(es-ticket.u-char-3,166,5)) /* serie-docto */
                             c-nro-comp         = trim(substring(es-ticket.u-char-3,171,16)) /* nro-docto */
                             c-nat-comp         = trim(substring(es-ticket.u-char-3,187,6)) /* nat-operacao */.        
                          run pi-saldo-comp.                               
                    end.
                  
                    if substring(es-ticket.u-char-3,204,16) <> "" then do:
                          assign
                             c-serie-comp       = trim(substring(es-ticket.u-char-3,199,5)) /* serie-docto */
                             c-nro-comp         = trim(substring(es-ticket.u-char-3,204,16)) /* nro-docto */
                             c-nat-comp         = trim(substring(es-ticket.u-char-3,220,6)) /* nat-operacao */.        
                          run pi-saldo-comp.      
                    end.
                  
                    if substring(es-ticket.u-char-3,237,16) <> "" then do:
                          assign
                             c-serie-comp       = trim(substring(es-ticket.u-char-3,232,5)) /* serie-docto */
                             c-nro-comp         = trim(substring(es-ticket.u-char-3,237,16)) /* nro-docto */
                             c-nat-comp         = trim(substring(es-ticket.u-char-3,253,6)) /* nat-operacao */.        
                          run pi-saldo-comp.      
                    end.                     
                  
                    if substring(es-ticket.u-char-3,270,16) <> "" then do:
                          assign
                             c-serie-comp       = trim(substring(es-ticket.u-char-3,265,5)) /* serie-docto */
                             c-nro-comp         = trim(substring(es-ticket.u-char-3,270,16)) /* nro-docto */
                             c-nat-comp         = trim(substring(es-ticket.u-char-3,286,6)) /* nat-operacao */.        
                          run pi-saldo-comp.      
                    end.
                  
                    if substring(es-ticket.u-char-3,303,16) <> "" then do:
                          assign
                             c-serie-comp       = trim(substring(es-ticket.u-char-3,298,5)) /* serie-docto */
                             c-nro-comp         = trim(substring(es-ticket.u-char-3,303,16)) /* nro-docto */
                             c-nat-comp         = trim(substring(es-ticket.u-char-3,319,6)) /* nat-operacao */.        
                          run pi-saldo-comp.      
                    end.
                         
                    if substring(es-ticket.u-char-3,336,16) <> "" then do:
                          assign
                             c-serie-comp       = trim(substring(es-ticket.u-char-3,331,5)) /* serie-docto */
                             c-nro-comp         = trim(substring(es-ticket.u-char-3,336,16)) /* nro-docto */
                             c-nat-comp         = trim(substring(es-ticket.u-char-3,352,6)) /* nat-operacao */.        
                          run pi-saldo-comp.      
                    end.       
     
                    if de-saldo <> 0 then do:
                      if i-conta-item = 1 then do:                  
                        message "Quantidade restante"  de-saldo
                                " ê superior ao Saldo Dispon°vel da(s) Nota de Origem selecionada!" VIEW-AS alert-box information.
                        IF l-zera THEN do:
                          MESSAGE "Sendo Criado Documento ZERADO no Recebimento!" VIEW-AS ALERT-BOX error.
                          /*release saldo-terc.
                          LEAVE.*/
                        END. /* erika */
                        
                      end.
                      else do:
                        message "Quantidade restante"  de-saldo
                                " ê superior ao Saldo Dispon°vel das Notas de Origem selecionadas!" view-as alert-box information.
                      end.
                    end. /* sobrou saldo */
                    
                    ASSIGN 
                        es-ticket.situacao     = 4
                        es-ticket.serie        = docum-est.serie 
                        es-ticket.nro-docto    = docum-est.nro-docto
                        es-ticket.cod-emitente = docum-est.cod-emitente
                        es-ticket.nat-operacao = docum-est.nat-operacao
                        es-ticket.usuario-atu = c-seg-usuario
                        es-ticket.hora-atual  = STRING(TIME,"HH:MM:SS")
                        es-ticket.data-atual  = TODAY.
    
                    end. /* terceiros e transf */              
                 
                end. /* armazenagem */
        
    
    
                IF es-ticket.operacao = 1 THEN DO:
                    
                    
                    FIND FIRST item-doc-est OF docum-est NO-LOCK.
                    
                    FOR EACH tt-dupli:
                        DELETE tt-dupli.
                    END.
                    ASSIGN de-base-calculo = de-total /* 26/04/2017 - de-cdo-unit*/ - de-funrural-unit - de-senar-unit. /* - de-irrf-unit - de-cofins-unit*/
                        
                    RUN esp/es4010f.p (INPUT de-base-calculo-ori - de-cdo-unit /*de-total de-base-calculo*/,
                                       INPUT RECID(item-doc-est),
                                       INPUT-OUTPUT TABLE tt-dupli).
                     
                    ASSIGN i-parcela-dp       = 0
                           i-conta-parcela-dp = 0.
    
                    /* 091205 */
                    FOR EACH tt-dupli:
                        ASSIGN i-conta-parcela-dp = i-conta-parcela-dp + 1.
                    END. /* para saber qtas parecelas tem */
    
                    FOR EACH tt-dupli:
                        ASSIGN i-parcela-dp = i-parcela-dp + 1.
                       
                        /* 240105 */
                        /*ASSIGN de-soma-mp = tt-dupli.vl-apagar. erika 250205 */

                    /* n∆o gera mais 27/04    
                    IF de-cdo-unit > 0 and
                       es-param-estab.l-dupl-cdo THEN DO: /* 07/12/2015 */
                        ASSIGN da-vencimento = date(MONTH(TODAY),01,YEAR(TODAY)) + 32
                               da-vencimento = date(MONTH(da-vencimento),es-param-empresa.u-int-1,YEAR(da-vencimento)).
          
                        /* 071205 - s¢ cria na primeira parcela da duplicata */
                        FIND FIRST dupli-apagar WHERE
                               dupli-apagar.cod-emitente = docum-est.cod-emitente AND
                               dupli-apagar.serie-docto  = docum-est.serie-docto AND
                               dupli-apagar.cod-esp      = es-param-estab.cod-esp-cdo AND
                               dupli-apagar.nro-docto    = docum-est.nro-docto AND
                               dupli-apagar.nr-duplic    = docum-est.nro-docto AND
                               dupli-apagar.parcela      = "11"
                            NO-LOCK NO-ERROR.
                        IF NOT AVAIL dupli-apagar THEN DO:
                            CREATE dupli-apagar.
                            ASSIGN dupli-apagar.cod-emitente = docum-est.cod-emitente
                                   dupli-apagar.serie-docto  = docum-est.serie-docto
                                   dupli-apagar.cod-esp      = es-param-estab.cod-esp-cdo
                                   dupli-apagar.nro-docto    = docum-est.nro-docto
                                   dupli-apagar.nr-duplic    = docum-est.nro-docto
                                   dupli-apagar.parcela      = "11"
                                   dupli-apagar.dt-emissao   = TODAY
                                   dupli-apagar.dt-trans     = TODAY
                                   dupli-apagar.dt-vencim    = da-vencimento
                                   dupli-apagar.valor        = de-cdo-unit
                                   dupli-apagar.nat-operacao = docum-est.nat-operacao
                                   dupli-apagar.cod-estabel  = docum-est.cod-estabel
                                   dupli-apagar.ep-codigo    = i-ep-codigo-usuario
                                   dupli-apagar.tp-despesa   = ITEM.tp-desp-padrao
                                   dupli-apagar.vl-a-pagar   = de-cdo-unit
                                   dupli-apagar.esp-movto    = 1
                                   dupli-apagar.estado       = 1.
                        END. /* not avail parc 11 */
                    END. */
    
                    IF de-funrural-unit > 0 and
                       es-param-estab.l-dupl-fr THEN DO:
                        ASSIGN da-vencimento = date(MONTH(TODAY),01,YEAR(TODAY)) + 32
                               da-vencimento = date(MONTH(da-vencimento),es-param-empresa.u-int-2,YEAR(da-vencimento)). /* era 9 / 10 */
        
                        /* 071205 - s¢ cria na primeira parcela da duplicata */
                        FIND FIRST dupli-apagar WHERE
                               dupli-apagar.cod-emitente = docum-est.cod-emitente AND
                               dupli-apagar.serie-docto  = docum-est.serie-docto AND
                               dupli-apagar.cod-esp      = es-param-estab.cod-esp-funrural AND
                               dupli-apagar.nro-docto    = docum-est.nro-docto AND
                               dupli-apagar.nr-duplic    = docum-est.nro-docto AND
                               dupli-apagar.parcela      = "12"
                            NO-LOCK NO-ERROR.

                       
                        IF NOT AVAIL dupli-apagar AND l-tem-cei = NO THEN DO:

                          
                            achou-dia-util-fr:
                            DO WHILE l-ok-fr = no: 
                                FIND FIRST  calen-coml WHERE
                                            calen-coml.ep-codigo   = "1" AND
                                            calen-coml.cod-estabel = docum-est.cod-estabel AND
                                            calen-coml.data        = da-vencimento
                                            NO-LOCK NO-ERROR.
                                
                                IF AVAIL calen-coml THEN DO:
                                    
                                       IF tipo-dia = 1 THEN DO:
                                          ASSIGN l-ok-fr = yes.
                                          LEAVE achou-dia-util-fr.
                                       END.
                                       ELSE DO:
                                          da-vencimento = da-vencimento - 1. 
                                       END.
                                 END.
                            END. /* antecipaá∆o de datas para FR - 28/08/2014 */
    

                            CREATE dupli-apagar.
                            ASSIGN dupli-apagar.cod-emitente = docum-est.cod-emitente
                                   dupli-apagar.serie-docto  = docum-est.serie-docto
                                   dupli-apagar.cod-esp      = es-param-estab.cod-esp-funrural
                                   dupli-apagar.nro-docto    = docum-est.nro-docto
                                   dupli-apagar.nr-duplic    = docum-est.nro-docto
                                   dupli-apagar.parcela      = "12"
                                   dupli-apagar.dt-emissao   = TODAY
                                   dupli-apagar.dt-trans     = TODAY
                                   dupli-apagar.dt-vencim    = da-vencimento
                                   dupli-apagar.valor        = de-funrural-unit
                                   dupli-apagar.nat-operacao = docum-est.nat-operacao
                                   dupli-apagar.cod-estabel  = docum-est.cod-estabel
                                   dupli-apagar.ep-codigo    = i-ep-codigo-usuario
                                   dupli-apagar.tp-despesa   = ITEM.tp-desp-padrao
                                   dupli-apagar.vl-a-pagar   = de-funrural-unit
                                   dupli-apagar.esp-movto    = 1
                                   dupli-apagar.estado       = 1.
                        END. /* not avail 12 */
                    END.
    

                    
                    /* s¢ l† embaixo /* novo senar Beto pediu pra colocar a mesma parcela do FR antigo, no dia 07/12/2010 pediu pra ir pra 13 */
                    IF de-senar-unit > 0 THEN DO:
                        ASSIGN da-vencimento = date(MONTH(TODAY),01,YEAR(TODAY)) + 32
                               da-vencimento = date(MONTH(da-vencimento),es-param-empresa.u-int-2,YEAR(da-vencimento)). /* era 9 / 10 */
        
                        /* 071205 - s¢ cria na primeira parcela da duplicata */
                        FIND FIRST dupli-apagar WHERE
                               dupli-apagar.cod-emitente = docum-est.cod-emitente AND
                               dupli-apagar.serie-docto  = docum-est.serie-docto AND
                               dupli-apagar.cod-esp      = es-param-estab.cod-esp-senar AND
                               dupli-apagar.nro-docto    = docum-est.nro-docto AND
                               dupli-apagar.nr-duplic    = docum-est.nro-docto AND
                               dupli-apagar.parcela      = "13"
                            NO-LOCK NO-ERROR.
                        IF NOT AVAIL dupli-apagar THEN DO:
    
                            CREATE dupli-apagar.
                            ASSIGN dupli-apagar.cod-emitente = docum-est.cod-emitente
                                   dupli-apagar.serie-docto  = docum-est.serie-docto
                                   dupli-apagar.cod-esp      = es-param-estab.cod-esp-senar
                                   dupli-apagar.nro-docto    = docum-est.nro-docto
                                   dupli-apagar.nr-duplic    = docum-est.nro-docto
                                   dupli-apagar.parcela      = "13"
                                   dupli-apagar.dt-emissao   = TODAY
                                   dupli-apagar.dt-trans     = TODAY
                                   dupli-apagar.dt-vencim    = da-vencimento
                                   dupli-apagar.valor        = de-senar-unit
                                   dupli-apagar.nat-operacao = docum-est.nat-operacao
                                   dupli-apagar.cod-estabel  = docum-est.cod-estabel
                                   dupli-apagar.ep-codigo    = i-ep-codigo-usuario
                                   dupli-apagar.tp-despesa   = ITEM.tp-desp-padrao
                                   dupli-apagar.vl-a-pagar   = de-senar-unit
                                   dupli-apagar.esp-movto    = 1
                                   dupli-apagar.estado       = 1.
                        END. /* not avail 12 */
                    END. /* senar */*//* 290105 */
                   
                        find FIRST item-doc-est OF docum-est EXCLUSIVE-LOCK no-error.
                        if avail item-doc-est THEN DO:                                                                

                            ASSIGN /* 16/11/12 - assign aqui do unit†rio para calcular sobre o preáo j† com impostos */
                                   item-doc-est.preco-unit[1]        = de-salva-unit / item-doc-est.quantidade /*(de-total-com-imp / item-doc-est.quantidade)*/
                                   item-doc-est.preco-unit[2]        = de-salva-unit / item-doc-est.quantidade /*(de-total-com-imp / item-doc-est.quantidade)*/
                                   item-doc-est.preco-total          = round(item-doc-est.preco-unit[2] * item-doc-est.quantidade,2) /* 16/11/12 de-total-com-imp  de-total-ori  210205 de-soma-mp*/  
                                   item-doc-est.ipi-outras[1]        = round(item-doc-est.preco-unit[2] * item-doc-est.quantidade,2) /* 16/11/12 de-total-com-imp p/ arredondar */                                      
                                   item-doc-est.icm-outras           = round(item-doc-est.preco-unit[2] * item-doc-est.quantidade,2) /* 16/11/12 de-total-com-imp p/ arredondar */ 
                                   item-doc-est.base-pis             = round(item-doc-est.preco-unit[2] * item-doc-est.quantidade,2) /* 16/11/12 de-total-com-imp p/ arredondar */ 
                                   item-doc-est.val-base-calc-cofins = round(item-doc-est.preco-unit[2] * item-doc-est.quantidade,2). /* 16/11/12 de-total-com-imp p/ arredondar */ 

                            IF vlo-debug THEN
                            MESSAGE 'ponto 1' SKIP
                                'de-salva-unit' de-salva-unit SKIP
                                'item-doc-est.preco-unit[2]' item-doc-est.preco-unit[2] SKIP
                                'item-doc-est.quantidade' item-doc-est.quantidade SKIP(1)
                                'round(item-doc-est.preco-unit[2] * item-doc-est.quantidade,2)' round(item-doc-est.preco-unit[2] * item-doc-est.quantidade,2)

                                VIEW-AS ALERT-BOX INFO BUTTONS OK.

                            ASSIGN item-doc-est.base-ipi[1]   = 0 
                                   item-doc-est.base-ipi[2]   = 0. /*agora */
                            ASSIGN item-doc-est.base-icm[1]   = 0
                                   item-doc-est.base-icm[2]   = 0
                                   docum-est.base-icm         = 0. /* 30/09/2013 */
                          
    
                         END.

    
                        IF de-senar-unit > 0 and
                           es-param-estab.l-dupl-senar THEN DO:
                            ASSIGN da-vencimento = date(MONTH(TODAY),01,YEAR(TODAY)) + 32
                                   da-vencimento = date(MONTH(da-vencimento),es-param-empresa.u-int-2,YEAR(da-vencimento)). /* era 9 / 10 */
            
                            /* 071205 - s¢ cria na primeira parcela da duplicata */
                            FIND FIRST dupli-apagar WHERE
                                   dupli-apagar.cod-emitente = docum-est.cod-emitente AND
                                   dupli-apagar.serie-docto  = docum-est.serie-docto AND
                                   dupli-apagar.cod-esp      = es-param-estab.cod-esp-senar AND
                                   dupli-apagar.nro-docto    = docum-est.nro-docto AND
                                   dupli-apagar.nr-duplic    = docum-est.nro-docto AND
                                   dupli-apagar.parcela      = "13"
                                NO-LOCK NO-ERROR.
                            IF NOT AVAIL dupli-apagar THEN DO:

                            achou-dia-util-se:
                            DO WHILE l-ok-se = no: 
                                FIND FIRST  calen-coml WHERE
                                            calen-coml.ep-codigo   = "1" AND
                                            calen-coml.cod-estabel = docum-est.cod-estabel AND
                                            calen-coml.data        = da-vencimento
                                            NO-LOCK NO-ERROR.
                                
                                IF AVAIL calen-coml THEN DO:
                                    
                                       IF tipo-dia = 1 THEN DO:
                                          ASSIGN l-ok-se = yes.
                                          LEAVE achou-dia-util-se.
                                       END.
                                       ELSE DO:
                                          da-vencimento = da-vencimento - 1. /* oi */
                                       END.
                                 END.
                            END. /* antecipaá∆o de datas para SE - 29/08/2014 */
                            
    
                                CREATE dupli-apagar.
                                ASSIGN dupli-apagar.cod-emitente = docum-est.cod-emitente
                                       dupli-apagar.serie-docto  = docum-est.serie-docto
                                       dupli-apagar.cod-esp      = es-param-estab.cod-esp-senar
                                       dupli-apagar.nro-docto    = docum-est.nro-docto
                                       dupli-apagar.nr-duplic    = docum-est.nro-docto
                                       dupli-apagar.parcela      = "13"
                                       dupli-apagar.dt-emissao   = TODAY
                                       dupli-apagar.dt-trans     = TODAY
                                       dupli-apagar.dt-vencim    = da-vencimento
                                       dupli-apagar.valor        = de-senar-unit
                                       dupli-apagar.nat-operacao = docum-est.nat-operacao
                                       dupli-apagar.cod-estabel  = docum-est.cod-estabel
                                       dupli-apagar.ep-codigo    = i-ep-codigo-usuario
                                       dupli-apagar.tp-despesa   = ITEM.tp-desp-padrao
                                       dupli-apagar.vl-a-pagar   = de-senar-unit
                                       dupli-apagar.esp-movto    = 1
                                       dupli-apagar.estado       = 1.
                            END. /* not avail 12 */
                        END. /* senar s¢ cria aqui com o novo valor de ajuste */
    
                        IF round(item-doc-est.preco-unit[2] * item-doc-est.quantidade,2) - docum-est.valor-mercad - de-irrf-unit - de-cofins-unit - de-cdo-unit - de-ajuste-senar-cei <> 0 THEN DO:
                            
                             /* novo valor do senar */
                             /* cria tabela de controle de ajuste do Senar 23/05/2013 - cei arrendondamento DP com sefaz */
                             FIND FIRST es-ajuste-senar-cei WHERE
                                        es-ajuste-senar-cei.serie-docto  = docum-est.serie-docto  and
                                        es-ajuste-senar-cei.nro-docto    = docum-est.nro-docto    and
                                        es-ajuste-senar-cei.cod-emitente = docum-est.cod-emitente and
                                        es-ajuste-senar-cei.nat-operacao = docum-est.nat-operacao NO-ERROR.
                             IF NOT AVAIL es-ajuste-senar-cei THEN DO:
                                 CREATE es-ajuste-senar-cei.
                                 ASSIGN es-ajuste-senar-cei.serie-docto  = docum-est.serie-docto 
                                        es-ajuste-senar-cei.nro-docto    = docum-est.nro-docto   
                                        es-ajuste-senar-cei.nat-operacao = docum-est.nat-operacao
                                        es-ajuste-senar-cei.cod-emitente = docum-est.cod-emitente.
                             END.
                             ASSIGN es-ajuste-senar-cei.valor-original = de-ajuste-senar-cei 
                                    de-diferenca-cei                   = round(item-doc-est.preco-unit[2] * item-doc-est.quantidade,2) - docum-est.valor-mercad - de-irrf-unit - de-cofins-unit - de-cdo-unit - de-ajuste-senar-cei
                                    es-ajuste-senar-cei.valor-ajuste   = round(item-doc-est.preco-unit[2] * item-doc-est.quantidade,2) - docum-est.valor-mercad - de-irrf-unit - de-cofins-unit - de-cdo-unit.
    
                        END.

                        
    
                         /*ASSIGN docum-est.valor-mercad = round(item-doc-est.preco-unit[2] * item-doc-est.quantidade,2)
                                docum-est.base-ipi     = round(item-doc-est.preco-unit[2] * item-doc-est.quantidade,2)
                                docum-est.base-icm     = round(item-doc-est.preco-unit[2] * item-doc-est.quantidade,2)
                                docum-est.tot-valor    = round(item-doc-est.preco-unit[2] * item-doc-est.quantidade,2).*/
    
                         RELEASE item-doc-est.
    
                    IF b-es-param-empresa.mp-ativa THEN DO:
                        IF de-irrf-unit > 0 THEN DO:
                        ASSIGN da-vencimento = es-param-empres.u-date-1.
                    
                        ASSIGN de-irrf-unit = ROUND(de-soma-mp * (es-param-empresa.perc-irrf / 100),2).
                        ASSIGN es-mp232-emit.vl-irrf  = de-irrf-unit
                               es-mp232-emit.gerou       = yes
                               es-mp232-emit.dt-geracao  = TODAY
                               es-mp232-emit.nota-geracao = STRING(num-nota).
                        
    
                        /* 071205 - s¢ cria na primeira parcela da duplicata */
                        FIND FIRST dupli-imp WHERE
                               dupli-imp.cod-emitente = tt-docum-est.cod-emitente AND
                               dupli-imp.serie-docto  = tt-docum-est.serie-docto AND
                               dupli-imp.cod-esp       = es-param-empresa.espec-irrf AND
                               dupli-imp.nro-docto     = tt-docum-est.nro-docto AND
                               dupli-imp.nr-duplic     = tt-docum-est.nro-docto  AND
                               dupli-imp.nro-docto-imp = tt-docum-est.nro-docto  AND
                               dupli-imp.parcela       = "1" 
                            NO-LOCK NO-ERROR.
                        IF NOT AVAIL dupli-apagar THEN DO:
    
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
                                  dupli-imp.rend-trib     = /* tt-dupli.vl-apagar*/ de-base-calculo-ori
                                  dupli-imp.tp-codigo     = 999
                                  dupli-imp.tp-imposto    = 1
                                  dupli-imp.vl-imposto    = de-irrf-unit
                                  dupli-imp.int-1         = IF (emitente.natureza = 2) THEN 14 ELSE 13
                                  dupli-imp.cod-retencao  = IF (emitente.natureza = 2) THEN 3842 ELSE 3850.
                        END. /* not avail 1 */
    
                        END.
                    
                        IF de-cofins-unit > 0 THEN DO:
                        ASSIGN da-vencimento = es-param-empresa.u-date-1.
                        
                        ASSIGN de-cofins-unit = ROUND((de-soma-mp / ((100 - (es-param-empresa.perc-cofins)) / 100)) - de-soma-mp,2).
                        ASSIGN es-mp232-emit.vl-cofins  = de-cofins-unit
                               es-mp232-emit.gerou       = yes
                               es-mp232-emit.dt-geracao  = TODAY
                               es-mp232-emit.nota-geracao = STRING(num-nota).
    
                        /* 071205 - s¢ cria na primeira parcela da duplicata */
                        FIND FIRST dupli-imp WHERE
                               dupli-imp.cod-emitente = tt-docum-est.cod-emitente AND
                               dupli-imp.serie-docto  = tt-docum-est.serie-docto AND
                               dupli-imp.cod-esp       = es-param-empresa.espec-irrf AND
                               dupli-imp.nro-docto     = tt-docum-est.nro-docto AND
                               dupli-imp.nr-duplic     = tt-docum-est.nro-docto  AND
                               dupli-imp.nro-docto-imp = tt-docum-est.nro-docto  AND
                               dupli-imp.parcela       = "2" 
                            NO-LOCK NO-ERROR.
                        IF NOT AVAIL dupli-apagar THEN DO:
    
                           CREATE dupli-imp.
                           ASSIGN dupli-imp.cod-emitente  = tt-docum-est.cod-emitente
                                  dupli-imp.serie-docto   = tt-docum-est.serie-docto
                                  dupli-imp.cod-esp       = es-param-empresa.espec-cofins
                                  dupli-imp.nro-docto     = tt-docum-est.nro-docto
                                  dupli-imp.nr-duplic     = tt-docum-est.nro-docto
                                  dupli-imp.nro-docto-imp = tt-docum-est.nro-docto
                                  dupli-imp.parcela       = "2" 
                                  dupli-imp.parcela-imp   = dupli-imp.parcela
                                  dupli-imp.cod-tax       = 0 
                                  dupli-imp.dt-venc-imp   = da-vencimento            
                                  dupli-imp.aliquota      = es-param-empresa.perc-cofins
                                  dupli-imp.cod-forn-imp  = int(dupli-imp.parcela)
                                  dupli-imp.nat-operacao  = tt-docum-est.nat-operacao
                                  dupli-imp.rend-trib     = /* tt-dupli.vl-apagar*/ de-base-calculo-ori
                                  dupli-imp.tp-codigo     = 999
                                  dupli-imp.tp-imposto    = 1
                                  dupli-imp.vl-imposto    = de-cofins-unit
                                  dupli-imp.int-1         = 15
                                  dupli-imp.cod-retencao  = 3877.
                        END. /* not avail parc 2 */
                    
    
                        END.
                    END. /* ativa mp */
    
                   
                        /* 171005 - inicio - nova interpretaá∆o calend·rio comercial s¢ para DP */
                        ASSIGN da-vencto-dp = tt-dupli.dt-vencimen.
                        achou-dia-util:
                        DO WHILE l-ok = no: 
                            FIND FIRST  calen-coml WHERE
                                        calen-coml.ep-codigo   = "1" AND
                                        calen-coml.cod-estabel = docum-est.cod-estabel AND
                                        calen-coml.data        = da-vencto-dp
                                        NO-LOCK NO-ERROR.
                            
                            IF AVAIL calen-coml THEN DO:
                                
                                   IF tipo-dia = 1 THEN DO:
                                      ASSIGN l-ok = yes.
                                      LEAVE achou-dia-util.
                                   END.
                                   ELSE DO:
                                      da-vencto-dp = da-vencto-dp - 1. /* oi */
                                   END.
                             END.
                        END.
                        /* 171005 - fim- nova interpretaá∆o calend·rio comercial s¢ para DP */
                         
 
                        
                        CREATE dupli-apagar.
                        ASSIGN dupli-apagar.cod-emitente = docum-est.cod-emitente
                               dupli-apagar.serie-docto  = docum-est.serie-docto
                               dupli-apagar.cod-esp      = "DP"
                               dupli-apagar.nro-docto    = docum-est.nro-docto
                               dupli-apagar.nr-duplic    = docum-est.nro-docto
                               dupli-apagar.parcela      = string(i-parcela-dp)
                               dupli-apagar.dt-emissao   = TODAY
                               dupli-apagar.dt-trans     = TODAY
                               dupli-apagar.dt-vencim    = DA-VENCTO-DP /* 171005 tt-dupli.dt-vencimen */
                               dupli-apagar.nat-operacao = docum-est.nat-operacao
                               dupli-apagar.cod-estabel  = docum-est.cod-estabel
                               dupli-apagar.ep-codigo    = i-ep-codigo-usuario
                               dupli-apagar.tp-despesa   = ITEM.tp-desp-padrao
                               dupli-apagar.esp-movto    = 1
                               dupli-apagar.estado       = 1.
                        
                        /* 27/11/2013 se tem cei com FR sen∆o sem FR ver dp*/
                        IF l-tem-cei THEN DO:

                            ASSIGN dupli-apagar.valor      = de-base-calculo-ori - de-cdo-unit - de-funrural-unit - de-senar-unit /*docum-est.valor-mercad - de-irrf-unit - de-cofins-unit  - de-cdo-unit  - de-senar-unit - de-diferenca-cei 27/04  16/11/12 tt-dupli.vl-apagar + ((de-funrural-unit + de-cdo-unit) / i-conta-parcela-dp)*/  /*de-base-calculo-ori / i-conta-parcela-dp 131205*/ /* 091205 */ /*tt-dupli.vl-apagar - de-irrf-unit - de-cofins-unit*/
                                   dupli-apagar.vl-a-pagar = de-base-calculo-ori - de-cdo-unit - de-funrural-unit - de-senar-unit. /*docum-est.valor-mercad - de-irrf-unit - de-cofins-unit  - de-cdo-unit  - de-senar-unit - de-diferenca-cei.27/04  16/11/12 tt-dupli.vl-apagar + ((de-funrural-unit + de-cdo-unit) / i-conta-parcela-dp)*/  /*de-base-calculo-ori / i-conta-parcela-dp 131205*/ /* 091205 */ /*tt-dupli.vl-apagar - de-irrf-unit - de-cofins-unit*/   
                        END.
                        ELSE DO:

                            
                            ASSIGN dupli-apagar.valor      = de-base-calculo-ori - de-cdo-unit - de-funrural-unit - de-senar-unit /*docum-est.valor-mercad - de-irrf-unit - de-cofins-unit - /* de-cdo-unit - */ de-funrural-unit - de-senar-unit - de-diferenca-cei  16/11/12 tt-dupli.vl-apagar + ((de-funrural-unit + de-cdo-unit) / i-conta-parcela-dp)*/  /*de-base-calculo-ori / i-conta-parcela-dp 131205*/ /* 091205 */ /*tt-dupli.vl-apagar - de-irrf-unit - de-cofins-unit*/
                                   dupli-apagar.vl-a-pagar = de-base-calculo-ori - de-cdo-unit - de-funrural-unit - de-senar-unit. /*docum-est.valor-mercad - de-irrf-unit - de-cofins-unit - /* de-cdo-unit - */ de-funrural-unit - de-senar-unit - de-diferenca-cei.  16/11/12 tt-dupli.vl-apagar + ((de-funrural-unit + de-cdo-unit) / i-conta-parcela-dp)*/  /*de-base-calculo-ori / i-conta-parcela-dp 131205*/ /* 091205 */ /*tt-dupli.vl-apagar - de-irrf-unit - de-cofins-unit*/   
                        END.
                    END.
                END.
                 
                RUN esp/MESSAGE3.p (INPUT "Criado Documento no Recebimento.",
                                    INPUT "Serie: "      + docum-est.serie-docto          + "                              "         +
                                          "Documento: "  + docum-est.nro-docto            + "                    " +
                                          "Fornecedor: " + string(docum-est.cod-emitente) + "                          " +
                                          "Natureza Operacao: " + docum-est.nat-operacao).
         
                ASSIGN es-ticket.situacao     = 4
                       es-ticket.serie        = docum-est.serie 
                       es-ticket.nro-docto    = docum-est.nro-docto
                       es-ticket.cod-emitente = docum-est.cod-emitente
                       es-ticket.nat-operacao = docum-est.nat-operacao
                es-ticket.usuario-atu = c-seg-usuario
                es-ticket.hora-atual  = STRING(TIME,"HH:MM:SS")
                es-ticket.data-atual  = TODAY.
            END.  /* docum-est */
            RELEASE docum-est.                                                                          
    
        END. /* sem erro */
    END. /* l-re = yes */
END.

RUN pi-finalizar IN h-acomp.

PROCEDURE pi-tt-item.
 
        ASSIGN i-conta-seq = i-conta-seq + 10.

        IF l-zera = NO THEN DO: 

          CREATE tt-item-doc-est.
          ASSIGN 
               tt-item-doc-est.serie-docto          = tt-docum-est.serie-docto
               tt-item-doc-est.nro-docto            = tt-docum-est.nro-docto
               tt-item-doc-est.cod-emitente         = tt-docum-est.cod-emitente
               tt-item-doc-est.nat-operacao         = tt-docum-est.nat-operacao
               tt-item-doc-est.nat-of               = tt-docum-est.nat-operacao
               tt-item-doc-est.sequencia            = i-conta-seq /*10*/
               tt-item-doc-est.it-codigo            = es-ticket.it-codigo
               tt-item-doc-est.cod-depos            = es-ticket.cod-depos 
               tt-item-doc-est.cod-localiz          = saldo-estoq.cod-localiz
               tt-item-doc-est.cod-refer            = saldo-estoq.cod-refer
               tt-item-doc-est.lote                 = saldo-estoq.lote
               tt-item-doc-est.dt-vali-lote         = saldo-estoq.dt-vali-lote

               tt-item-doc-est.base-pis             = (de-base-calculo-ori - de-cdo-unit / i-conta-item) /* erika 03/09/2013 de-total */
               tt-item-doc-est.val-base-calc-cofins = (de-base-calculo-ori - de-cdo-unit / i-conta-item) /* erika 03/09/2013 de-total */

               /*tt-item-doc-est.cod-emit-benef  =
               tt-item-doc-est.serie-terc        =
               tt-item-doc-est.nro-docto-terc    =  VERIFICAR
               tt-item-doc-est.seq-terc          =
               tt-item-doc-est.cod-emit-terc     =
               tt-item-doc-est.nat-terc          =  */
               tt-item-doc-est.aliquota-icm      = natur-oper.aliquota-icm 
              
               tt-item-doc-est.aliquota-ipi      = 0
               tt-item-doc-est.aliquota-iss      = 0
               tt-item-doc-est.cd-trib-icm       = natur-oper.cd-trib-icm
               tt-item-doc-est.cd-trib-ipi       = natur-oper.cd-trib-ipi
               tt-item-doc-est.cd-trib-iss       = 2  /*** ISENTO ***/
               tt-item-doc-est.class-fiscal      = ITEM.class-fiscal
               tt-item-doc-est.conta-contabil    = ""
               tt-item-doc-est.ct-codigo         = tt-docum-est.ct-transit
               tt-item-doc-est.sc-codigo         = ""
               tt-item-doc-est.dt-ent-prev       = TODAY
               tt-item-doc-est.est-cob           = tt-docum-est.cod-estabel
               tt-item-doc-est.narrativa         = STRING(es-ticket.nr-ticket)
               tt-item-doc-est.peso-liquido      = IF emitente.natureza = 1 THEN 
                                                    (de-quantidade / i-conta-item)
                                                                            ELSE 
                                                   IF es-ticket.peso-desmem[1] > 0 THEN 
                                                      (es-ticket.peso-desmem[1] / i-conta-item)
                                                   ELSE (de-quantidade / i-conta-item)
               tt-item-doc-est.preco-total[1]    = (de-base-calculo-ori - de-cdo-unit / i-conta-item)   /*(de-total / i-conta-item)*/
               tt-item-doc-est.preco-total[2]    = (de-base-calculo-ori - de-cdo-unit / i-conta-item)   /*(de-total / i-conta-item)*/
               tt-item-doc-est.preco-unit[1]     = (de-base-calculo-ori - de-cdo-unit / i-conta-item) /*(de-unitario / i-conta-item) */
               tt-item-doc-est.preco-unit[2]     = (de-base-calculo-ori - de-cdo-unit / i-conta-item) /*(de-unitario / i-conta-item) */
               tt-item-doc-est.base-cofins-subs  = 0
               tt-item-doc-est.base-icm-cmi      = 0
               tt-item-doc-est.base-ipi-cmi      = 0 
               tt-item-doc-est.base-iss-cmi      = 0
               tt-item-doc-est.base-pis-subs     = 0 
               tt-item-doc-est.qt-do-forn        = IF emitente.natureza = 1 THEN (de-quantidade / i-conta-item)
                                                     ELSE 
                                                   IF es-ticket.peso-desmem[1] > 0 THEN 
                                                     (es-ticket.peso-desmem[1]/ i-conta-item)
                                                      ELSE (de-quantidade / i-conta-item)
               tt-item-doc-est.qt-real           = IF emitente.natureza = 1 THEN 
                                                     (de-quantidade / i-conta-item) ELSE 
                                                   IF es-ticket.peso-desmem[1] > 0 THEN 
                                                   (es-ticket.peso-desmem[1] / i-conta-item)
                                                      ELSE (de-quantidade / i-conta-item)
               tt-item-doc-est.qt-saldo          = IF emitente.natureza = 1 THEN (de-quantidade / i-conta-item) ELSE 
                                                   IF es-ticket.peso-desmem[1] > 0 THEN 
                                                      (es-ticket.peso-desmem[1] / i-conta-item)
                                                      ELSE (de-quantidade / i-conta-item)
               tt-item-doc-est.quantidade        = IF emitente.natureza = 1 THEN 
                                                        (de-quantidade / i-conta-item)
                                                        ELSE 
                                                   IF es-ticket.peso-desmem[1] > 0 THEN 
                                                   (es-ticket.peso-desmem[1] / i-conta-item)
                                                      ELSE (de-quantidade / i-conta-item)
               tt-item-doc-est.un                = es-ticket.un
               tt-item-doc-est.usuario           = v_cod_usuar_corren
               tt-item-doc-est.hora              = STRING(TIME,"HH:MM:SS")
               tt-item-doc-est.valor-frete       = 0
               tt-item-doc-est.vl-cofins-subs    = 0
               tt-item-doc-est.vl-isr            = 0
               tt-item-doc-est.vl-iss-cmi        = 0
               tt-item-doc-est.vl-pis-subs       = 0
               tt-item-doc-est.vl-subs-cmi       = 0
               tt-item-doc-est.vl-taxa           = 0
               tt-item-doc-est.vl-unit-mob       = 0
               tt-item-doc-est.pr-total[1]       = (de-base-calculo-ori - de-cdo-unit / i-conta-item) /* de-total */
               tt-item-doc-est.pr-total[2]       = (de-base-calculo-ori - de-cdo-unit / i-conta-item) /* de-total */
               tt-item-doc-est.pr-total[3]       = (de-base-calculo-ori - de-cdo-unit / i-conta-item) /* de-total */
               tt-item-doc-est.baixa-ce          = YES
               
               tt-item-doc-est.base-icm[2]       = 0
               tt-item-doc-est.icm-outras[1]     = (de-base-calculo-ori - de-cdo-unit / i-conta-item) /* de-total */
               tt-item-doc-est.icm-outras[2]     = 0 
               tt-item-doc-est.base-ipi[1]       = 0
               tt-item-doc-est.base-ipi[2]       = 0
               tt-item-doc-est.ipi-outras[1]     = (de-base-calculo-ori - de-cdo-unit / i-conta-item) /* de-total */
               tt-item-doc-est.ipi-outras[2]     = 0
               tt-item-doc-est.base-iss[1]       = 0
               tt-item-doc-est.base-iss[2]       = 0
               tt-item-doc-est.base-subs[1]      = 0
               tt-item-doc-est.base-subs[2]      = 0.


    IF vlo-debug THEN
    MESSAGE    'ponto 2' SKIP(1)         
        'tt-item-doc-est.preco-total[1] ' tt-item-doc-est.preco-total[1] SKIP
        'tt-item-doc-est.preco-total[2] ' tt-item-doc-est.preco-total[2]  SKIP
        'de-base-calculo-ori' de-base-calculo-ori SKIP
        'de-cdo-unit' de-cdo-unit SKIP
        'i-conta-item' i-conta-item SKIP(1)
        
        'tt-item-doc-est.preco-total[1]    = (de-base-calculo-ori - de-cdo-unit / i-conta-item) ' SKIP
               
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
           /*
           MESSAGE "CREATE tt-item-doc-est "  SKIP
                   "tt-docum-est.conta-transit" tt-docum-est.conta-transit SKIP
                   "tt-docum-est.ct-transit   " tt-docum-est.ct-transit    SKIP
                   "tt-docum-est.sc-transit   " tt-docum-est.sc-transit    SKIP
                   
                   "tt-item-doc-est.conta-contabil " tt-item-doc-est.conta-contabil  SKIP
                   "tt-item-doc-est.ct-codigo      " tt-item-doc-est.ct-codigo       SKIP
                   "tt-item-doc-est.sc-codigo      " tt-item-doc-est.sc-codigo       SKIP
                   VIEW-AS ALERT-BOX INFO BUTTONS OK.
           */                                       
           ASSIGN tt-item-doc-est.num-pedido   =  i-num-pedido
                  tt-item-doc-est.numero-ordem =  i-num-ordem
                  tt-item-doc-est.parcela      =  i-parcela.
           
        END. /* l-zera */

END. /* pi-tt-item */

PROCEDURE pi-saldo-comp:
            
  if de-saldo <> 0 then do: 
    assign de-qtde-aux = 0.

            /* aloca qtde no documento de origem */
            find first saldo-terc where
                       saldo-terc.serie-docto  = c-serie-comp and
                       saldo-terc.nro-docto    = c-nro-comp and
                       saldo-terc.cod-emitente = es-ticket.cod-produtor and
                       saldo-terc.nat-operacao = c-nat-comp and
                       saldo-terc.it-codigo    = es-ticket.it-codigo
                       exclusive-lock no-error.
            if avail saldo-terc then do:
              if /*es-ticket.peso-desmem[1]*/ 
                 de-saldo > (saldo-terc.quantidade - saldo-terc.dec-1) and
                 i-conta-item = 1 then do: /* so consiste para uma nota */
                message "Quantidade da Nota "  es-ticket.peso-desmem[1]
                        " ê superior ao Saldo Dispon°vel da Nota de Origem selecionada!" (saldo-terc.quantidade - saldo-terc.dec-1) SKIP
                        "Nota de Origem " saldo-terc.serie-docto " / " 
                        saldo-terc.nro-docto " / "  
                        saldo-terc.cod-emitente " / "  
                        saldo-terc.nat-operacao " / "  
                        saldo-terc.it-codigo "!"  
                        view-as alert-box information.
                /* 06/01/04 para todo o saldo de terceiros alocado n∆o deixo passar */ 
                
                IF (saldo-terc.quantidade - saldo-terc.dec-1) = 0 THEN do:
                    ASSIGN l-zera = YES.
                END. /* erika */
                ELSE l-zera = NO.
              end.
              /* novo */             
              if (saldo-terc.quantidade - saldo-terc.dec-1) <= de-saldo then do:                
                assign 
                  de-qtde-aux      = (saldo-terc.quantidade - saldo-terc.dec-1)
                  de-saldo         = de-saldo - (saldo-terc.quantidade - saldo-terc.dec-1)
                  saldo-terc.dec-1 = saldo-terc.dec-1 + (saldo-terc.quantidade - saldo-terc.dec-1) /* es-ticket.peso-desmem[1] */.
              end.
              else do:
                assign 
                  de-qtde-aux      = de-saldo
                  saldo-terc.dec-1 = saldo-terc.dec-1 + de-saldo
                  de-saldo         = 0.
              end.
  
            end. /* saldo-terc */
                    
            /********* 
            Erika - 13/08/2003 - alterar o item se for diferente 
            casos em que o arroz foi enviado para depÛsito de terceiros como tipo 0001 (um arroz bom)
            e retornou a Camil como tipo 0002, e ai o item em terceiros deve ser alterado para que
            a entrada no Recebimento seja feita corretamente no re1005
            ********/
            /* consiste documento de origem */
            find first saldo-terc where
                       saldo-terc.serie-docto  = c-serie-comp and
                       saldo-terc.nro-docto    = c-nro-comp and
                       saldo-terc.cod-emitente = es-ticket.cod-produtor and
                       saldo-terc.nat-operacao = c-nat-comp and
                       saldo-terc.it-codigo    = es-ticket.it-codigo
                       no-lock no-error.

            if not avail saldo-terc then do:
                if es-ticket.it-codigo = "0001" then do:
                    c-item-troca = "".
                    find first saldo-terc where
                               saldo-terc.serie-docto  = c-serie-comp and
                               saldo-terc.nro-docto    = c-nro-comp and
                               saldo-terc.cod-emitente = es-ticket.cod-produtor and
                               saldo-terc.nat-operacao = c-nat-comp and
                               saldo-terc.it-codigo    = "0002"
                               exclusive-lock no-error.
                    if avail saldo-terc then assign c-item-troca = "0002".
                    /*else do:*/

                        if c-item-troca = "" then do:
      
                          find first saldo-terc where
                                     saldo-terc.serie-docto  = c-serie-comp and
                                     saldo-terc.nro-docto    = c-nro-comp and
                                     saldo-terc.cod-emitente = es-ticket.cod-produtor and
                                     saldo-terc.nat-operacao = c-nat-comp and
                                     saldo-terc.it-codigo    = "0040"
                                     exclusive-lock no-error.
                          if avail saldo-terc then do:
                            assign c-item-troca = "0040".
                          end.
                                     
                          if not avail saldo-terc then do:
      
                              message "ERRO-Documento de Origem n∆o encontrado para !!!!" 
                                       c-serie-comp 
                                       c-nro-comp 
                                       es-ticket.cod-produtor 
                                       c-nat-comp 
                                       es-ticket.it-codigo                                          
                                       view-as alert-box error.
                                       
                              return.
                          end. /* not saldo-terc */
                        end. /* "" */
                        
                        message "Documento de Origem n∆o encontrado para 0001 e sim para " c-item-troca "!" 
                                 c-serie-comp 
                                 c-nro-comp 
                                 es-ticket.cod-produtor 
                                 c-nat-comp 
                                 es-ticket.it-codigo skip                                        
                                 "Deseja continuar alterar de" c-item-troca " para 0001?"
                                 view-as alert-box question buttons yes-no update l-cont-docto as log.
                        if l-cont-docto then do:
                                                        
                          assign saldo-terc.it-codigo = "0001".
                  
                          FIND componente
                            WHERE componente.serie-docto  = c-serie-comp 
                              AND componente.nro-docto    = c-nro-comp
                              AND componente.cod-emitente = es-ticket.cod-produtor
                              AND componente.nat-operacao = c-nat-comp
                              AND componente.it-codigo    = c-item-troca exclusive-lock NO-ERROR.
                          IF AVAIL componente THEN DO:
                        
                             assign componente.it-codigo = "0001".
                          end.
                        
                        end.
 
                        /* aloca saldos */
                         
                         /* novo */             
                         if (saldo-terc.quantidade - saldo-terc.dec-1) <= de-saldo then do:                
                           assign 
                             de-qtde-aux      = (saldo-terc.quantidade - saldo-terc.dec-1)
                             de-saldo         = de-saldo - (saldo-terc.quantidade - saldo-terc.dec-1)
                             saldo-terc.dec-1 = saldo-terc.dec-1 + (saldo-terc.quantidade - saldo-terc.dec-1) /* es-ticket.peso-desmem[1] */.
                             
                         end.
                         else do:
                           assign 
                             de-qtde-aux      = de-saldo
                             saldo-terc.dec-1 = saldo-terc.dec-1 + de-saldo
                             de-saldo         = 0.
                         end.
                    /*end. erika */
                end. /* saldo-terc*/            

                else do:
                    c-item-troca = "".
                    find first saldo-terc where
                               saldo-terc.serie-docto  = c-serie-comp and
                               saldo-terc.nro-docto    = c-nro-comp and
                               saldo-terc.cod-emitente = es-ticket.cod-produtor and
                               saldo-terc.nat-operacao = c-nat-comp and
                               saldo-terc.it-codigo    = "0001"
                               exclusive-lock no-error.
                    if avail saldo-terc then assign c-item-troca = "0001".

                    /*else do:*/
                        if c-item-troca = "" then do:
      
                          find first saldo-terc where
                                     saldo-terc.serie-docto  = c-serie-comp and
                                     saldo-terc.nro-docto    = c-nro-comp and
                                     saldo-terc.cod-emitente = es-ticket.cod-produtor and
                                     saldo-terc.nat-operacao = c-nat-comp and
                                     saldo-terc.it-codigo    = "0040"
                                     exclusive-lock no-error.
                          if avail saldo-terc then do:
                          assign c-item-troca = "0040".
                          end.
                                     
                          if not avail saldo-terc then do:
      
                              message "ERRO-Documento de Origem n∆o encontrado para !!!!" 
                                       c-serie-comp 
                                       c-nro-comp 
                                       es-ticket.cod-produtor 
                                       c-nat-comp 
                                       es-ticket.it-codigo                                          
                                       view-as alert-box error.
                                       
                              return.
                          end. /* not saldo-terc */
                        end. /* "" */
                        
                        message "Documento de Origem n∆o encontrado para 0002 e sim para " c-item-troca "!" 
                                 c-serie-comp 
                                 c-nro-comp 
                                 es-ticket.cod-produtor 
                                 c-nat-comp 
                                 es-ticket.it-codigo skip                                        
                                 "Deseja continuar alterar de " c-item-troca " para 0002?"
                                 view-as alert-box question buttons yes-no update l-cont-docto2 as log.
                        if l-cont-docto2 then do:
                                                        
                          assign saldo-terc.it-codigo = "0002".
                          FIND componente
                            WHERE componente.serie-docto  = c-serie-comp 
                              AND componente.nro-docto    = c-nro-comp
                              AND componente.cod-emitente = es-ticket.cod-produtor
                              AND componente.nat-operacao = c-nat-comp
                              AND componente.it-codigo    = c-item-troca exclusive-lock NO-ERROR.
                          IF AVAIL componente THEN DO:

                             assign componente.it-codigo = "0002".
                          end.
                          /* aloca saldos */
                       /* aloca saldos */
                         
                         /* novo */             
                         if (saldo-terc.quantidade - saldo-terc.dec-1) <= de-saldo then do:                
                           assign 
                             de-qtde-aux      = (saldo-terc.quantidade - saldo-terc.dec-1)
                             de-saldo         = de-saldo - (saldo-terc.quantidade - saldo-terc.dec-1)
                             saldo-terc.dec-1 = saldo-terc.dec-1 + (saldo-terc.quantidade - saldo-terc.dec-1) /* es-ticket.peso-desmem[1] */.
                         end.
                         else do:
                            assign 
                             de-qtde-aux      = de-saldo
                             saldo-terc.dec-1 = saldo-terc.dec-1 + de-saldo
                             de-saldo         = 0.
                         end.                     
                        end.                            
                    /* end.erika */
                
                end. /* else */

            end. /* saldo */
            
            
  
    /***** 20/08 - novo assign para colocar o item exatamente com a qtde do saldo-terc *****/
    FIND b-docum-est
        WHERE ROWID(b-docum-est) = r-RE0301-documento 
        NO-LOCK NO-ERROR.


    IF  AVAIL docum-est THEN DO:
        for each item-doc-est of b-docum-est exclusive-lock:
        
          if item-doc-est.serie-comp = c-serie-comp and
             item-doc-est.nro-comp   = c-nro-comp and
             item-doc-est.nat-comp   = c-nat-comp then do:


             assign    
                  item-doc-est.peso-liquido     = de-qtde-aux
                  item-doc-est.preco-total[1]   = de-qtde-aux * de-pre-uni
                  item-doc-est.preco-total[2]   = de-qtde-aux * de-pre-uni
                  /*item-doc-est.preco-unit[1]    = de-qtde-aux * de-pre-uni
                  item-doc-est.preco-unit[2]    = de-qtde-aux * de-pre-uni erika porque estava pondo o total no unit†rio para transferencia 03/09/2013 */
                  item-doc-est.qt-do-forn       = de-qtde-aux 
                  item-doc-est.qt-real          = de-qtde-aux 
                  item-doc-est.qt-saldo         = de-qtde-aux
                  item-doc-est.quantidade       = de-qtde-aux
                  item-doc-est.pr-total[1]      = de-qtde-aux * de-pre-uni
                  item-doc-est.pr-total[2]      = de-qtde-aux * de-pre-uni
                  item-doc-est.pr-total[3]      = de-qtde-aux * de-pre-uni
                  item-doc-est.icm-outras[1]    = de-qtde-aux * de-pre-uni
                  item-doc-est.ipi-outras[1]    = de-qtde-aux * de-pre-uni
                  item-doc-est.seq-comp         = saldo-terc.sequencia.

             IF vlo-debug THEN
             MESSAGE 'ponto 3 ' SKIP(1)
                 'item-doc-est.preco-total[1]   = de-qtde-aux * de-pre-uni' SKIP
                 'de-qtde-aux' de-qtde-aux SKIP
                 'de-pre-uni' de-pre-uni SKIP(1)
                 'item-doc-est.qt-do-forn'

                 VIEW-AS ALERT-BOX INFO BUTTONS OK.



               ASSIGN item-doc-est.base-ipi[1]   = 0 
                      item-doc-est.base-ipi[2]   = 0. /*agora */

               ASSIGN item-doc-est.base-icm[1]   = 0
                      item-doc-est.base-icm[2]   = 0

                      docum-est.base-icm         = 0 /* 30/09/2013 */
                      /*item-doc-est.base-pis      = de-base-calculo-ori - de-cdo-unit /*de-total*/
                      item-doc-est.val-base-calc-cofins = de-base-calculo-ori - de-cdo-unit /*de-total*/*/.

              

                find first rat-lote where
                           rat-lote.serie-docto    = item-doc-est.serie-docto and
                           rat-lote.nro-docto      = string(item-doc-est.nro-docto,"9999999") and 
                           rat-lote.cod-emitente   = item-doc-est.cod-emitente and
                           rat-lote.nat-operacao   = item-doc-est.nat-operacao and
                           rat-lote.sequencia      = item-doc-est.sequencia exclusive-lock no-error.
                if avail rat-lote then      
                       assign rat-lote.quantidade = de-qtde-aux.

            end. /* avail */
        end. /* for each item */
    end. /* avail docum */    
  end. /* de-saldo */
  RELEASE rat-lote.
  RELEASE item-doc-est.
  
END. /* pi-saldo-comp */


PROCEDURE pi-quantidade :

  ASSIGN di-quantidade = 0.
      
  FOR EACH tt-detalhes:

      FIND es-tipo-desconto NO-LOCK
          WHERE es-tipo-desconto.tp-desconto = tt-detalhes.c-tp-desconto
          NO-ERROR.

      FIND es-param-estab NO-LOCK
    WHERE es-param-estab.cod-estabel = es-ticket.cod-estabel
    NO-ERROR.


      IF tt-detalhes.c-esp-docto = "DEP" OR 
         tt-detalhes.c-esp-docto = "CMP" OR 
         tt-detalhes.c-esp-docto = "TRA" OR 
         tt-detalhes.c-esp-docto = "IMP" THEN
         ASSIGN di-quantidade = di-quantidade + tt-detalhes.de-quantidade.
       
      IF tt-detalhes.c-esp-docto = "DSC" THEN DO:

         IF es-tipo-desconto.quantidade THEN
            ASSIGN di-quantidade = di-quantidade - tt-detalhes.de-quantidade.

         IF es-tipo-desconto.financeiro THEN
              IF es-param-estab.base-descto = NO THEN
                 ASSIGN di-quantidade = di-quantidade - tt-detalhes.de-quantidade.
      END.
      
  END. 

END PROCEDURE.



RETURN 'OK'.
PROCEDURE pi-cria-mp232.
    CREATE es-mp232-emit.
    ASSIGN es-mp232-emit.cod-estabel  = es-ticket.cod-estabel
           es-mp232-emit.cod-emitente = i-cod-emitentees4010a /* es-ticket.cod-emitente 240105*/
           es-mp232-emit.mes          = STRING(MONTH(es-param-empres.u-date-1))   
           es-mp232-emit.vl-irrf      = 0 
           es-mp232-emit.vl-cofins    = 0
           es-mp232-emit.vl-cdo       = de-cdo-unit
           es-mp232-emit.vl-funrural  = de-funrural-unit
           es-mp232-emit.val-nf       = de-total           
           es-mp232-emit.nr-nota-fis  = num-nota          
           es-mp232-emit.gerou        = NO
           es-mp232-emit.dt-pagto     = es-param-empres.u-date-1
           es-mp232-emit.dt-geracao   = ?
           es-mp232-emit.obs-geracao  = "N∆o possuia valor suficiente para taxaá∆o do imposto".

END.



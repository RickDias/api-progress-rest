/******************************************************************************
** EMPRESA  : CAMIL
** PROGRAMA : ES4045RP
** DESCRICAO: Modelo 2
** AUTOR    : Erika 
** DATA     : MAIO DE 2005
** VERSAO   : 2.04.00.000 - Versao Inicial.
** set/2016 - SMF - Kraft - convers∆o campos totvs 12
*******************************************************************************/
{utp/ut-glob.i}
{include/i-prgvrs.i ES4045RP 2.09.00.000 } /*** 010000 ***/ 
{include/i-bfems2cad.i}

/*------------- Definicao de Temp-Table --------------------------------------*/
/* erika 06/07 */
DEF VAR val-saldo-ent AS DEC DECIMALS 4.   
DEF VAR val-saldo-sai AS DEC DECIMALS 4.
DEF VAR val-saldo-bon AS DEC DECIMALS 4.
DEF VAR val-saldo-fin AS DEC DECIMALS 4.
DEF VAR val-saldo-tra AS DEC DECIMALS 4.
DEF VAR val-saldo-dev AS DEC DECIMALS 4.

DEFINE VARIABLE vl-saldo        AS DECIMAL DECIMALS 4.
DEFINE VARIABLE vl-aloca        AS DECIMAL DECIMALS 4.
DEFINE VARIABLE vl-bloqu        AS DECIMAL DECIMALS 4.
DEFINE VARIABLE vl-contr        AS DECIMAL DECIMALS 4.
DEFINE VARIABLE vl-entre        AS DECIMAL DECIMALS 4.
DEFINE VARIABLE vl-secag        AS DECIMAL DECIMALS 4.
DEFINE VARIABLE vl-finan        AS DECIMAL DECIMALS 4.

DEF VAR di-quantidade AS DEC NO-UNDO. /* 081104 */

DEF BUFFER bf-emitente FOR emitente.
DEF BUFFER bf-es-ticket FOR es-ticket.

define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    field classifica       as integer
    field desc-classifica  as char format "x(40)"
    FIELD cod-emitente-ini LIKE emitente.cod-emitente
    FIELD cod-emitente-fim LIKE emitente.cod-emitente
    FIELD nr-contrato-ini  LIKE es-contrato-arroz.nr-contrato
    FIELD nr-contrato-fim  LIKE es-contrato-arroz.nr-contrato
    FIELD cod-estab-ini    LIKE es-movto-arroz.cod-estabel    
    FIELD cod-estab-fim    LIKE es-movto-arroz.cod-estabel   
    FIELD it-codigo-ini    LIKE ITEM.it-codigo
    FIELD it-codigo-fim    LIKE ITEM.it-codigo
    FIELD dt-trans-ini     LIKE es-movto-arroz.dt-trans
    FIELD dt-trans-fim     LIKE es-movto-arroz.dt-trans
    FIELD matriz-ini       LIKE es-movto-arroz.nome-matriz
    FIELD matriz-fim       LIKE es-movto-arroz.nome-matriz
    FIELD medias           AS LOG
    FIELD l-compra         AS LOGICAL
    FIELD l-deposito       AS LOGICAL
    FIELD l-terceiro       AS LOGICAL 
    FIELD l-importacao     AS LOGICAL 
    FIELD l-o-entradas     AS LOGICAL
    FIELD l-o-saidas       AS LOGICAL
    FIELD l-tr-tit         AS LOGICAL
    FIELD l-arm-ent        AS LOGICAL
    FIELD l-arm-sai        AS LOGICAL
    FIELD l-tra-est        AS LOGICAL.

    
DEF VAR de-soma-extrato-secagem AS DEC INITIAL 0.

DEF TEMP-TABLE tt-extrato NO-UNDO
    FIELD nome-matriz       LIKE es-movto-arroz.nome-matriz
    FIELD cod-emitente      LIKE emitente.cod-emitente
    FIELD nr-contrato       LIKE es-contrato-arroz.nr-contrato
    FIELD dt-trans          LIKE es-movto-arroz.dt-trans   
    FIELD nr-ticket         LIKE es-ticket.nr-ticket       
    FIELD nr-placa          LIKE es-ticket.nr-placa-cam    
    FIELD nr-nota-fornec    LIKE es-ticket.nr-nota-fornec  
    FIELD nro-docto         LIKE es-movto-arroz.nro-docto  
    FIELD serie             LIKE es-movto-arroz.serie
    FIELD esp-docto         LIKE es-movto-arroz.esp-docto  
    FIELD tipo-trans        LIKE es-movto-arroz.tipo-trans
    FIELD peso-fornec       LIKE es-ticket.peso-liq
    FIELD peso-liq          AS DEC FORMAT "->>,>>>,>>9.99"
    FIELD gr-umidade        LIKE es-ticket.gr-umidade
    FIELD desc-umidade      LIKE es-movto-arroz.quantidade
    FIELD gr-impureza       LIKE es-ticket.gr-impureza
    FIELD desc-impureza     LIKE es-movto-arroz.quantidade
    FIELD gr-secagem        LIKE es-ticket.gr-secagem
    FIELD desc-secagem      LIKE es-movto-arroz.quantidade
    FIELD rend-inteiro      LIKE es-ticket.rend-inteiro
    FIELD rend-quebrado     LIKE es-ticket.rend-quebr
    FIELD desc-rendimento   LIKE es-movto-arroz.quantidade
    FIELD rend-desc-inteiro LIKE es-ticket.rend-inteiro
    FIELD desc-inteiro      LIKE es-movto-arroz.quantidade
    FIELD gr-amarelo        LIKE es-ticket.gr-amarelo
    FIELD desc-amarelo      LIKE es-movto-arroz.quantidade
    FIELD gr-vermelho       LIKE es-ticket.gr-vermelho
    FIELD desc-vermelho     LIKE es-movto-arroz.quantidade
    FIELD gr-preto-verm     LIKE es-ticket.gr-preto-verm  
    FIELD desc-preto-verm   LIKE es-movto-arroz.quantidade
    FIELD gr-verde          LIKE es-ticket.gr-verde
    FIELD desc-verde        LIKE es-movto-arroz.quantidade
    FIELD gr-gesso          LIKE es-ticket.gr-gesso
    FIELD desc-gesso        LIKE es-movto-arroz.quantidade
    FIELD gr-manch-pic      LIKE es-ticket.gr-manch-pic    
    FIELD desc-manch-pic    LIKE es-movto-arroz.quantidade
    FIELD qtd-bonific       LIKE es-movto-arroz.quantidade
    FIELD val-bonific       LIKE es-movto-arroz.valor
    FIELD valor-unit        LIKE es-movto-arroz.valor
    FIELD peso-total          AS DECIMAL  FORMAT "->>,>>>,>>9.99"
    FIELD valor-bruto         AS DECIMAL
    FIELD valor-total         AS DECIMAL
    FIELD rendimento          AS CHAR FORMAT "x(20)"
    INDEX codigo IS PRIMARY UNIQUE 
          cod-emitente 
          nr-contrato 
          nr-ticket
          esp-docto
          nro-docto
          serie.
    
/* 31/01/04 */
define temp-table tt-soma-contrato no-undo
    FIELD umidade        LIKE es-contrato-arroz.gr-umidade
    FIELD rend-inteiro   LIKE es-contrato-arroz.rend-inteiro
    FIELD preto-vermelho LIKE es-contrato-arroz.u-dec-3
    FIELD verde          LIKE es-contrato-arroz.u-dec-5
    FIELD gesso          LIKE es-contrato-arroz.gr-gesso
    FIELD manch-pic      LIKE es-contrato-arroz.u-dec-4
    FIELD impureza       LIKE es-contrato-arroz.u-dec-4
    FIELD rend-quebr       LIKE es-contrato-arroz.rend-quebr
    FIELD nr-contratos   AS INT.

def var c-gr-impureza    like es-contrato-arroz.gr-impureza. /* 040504 */
def var de-peso-final    as dec initial 0. /* 040504 */
def var de-total         as dec initial 0. /* 040504 */
def var de-dif           as dec initial 0. /* 040504 */
def var de-preto-ver     as dec initial 0. /* 040504 */
def var de-dec-preto-ver as dec initial 0. /* 040504 */
def var de-valor-unit    as dec initial 0. /* 040504 */
def var de-valor-total   as dec initial 0. /* 040504 */
def var da-dt-trans      as date. /* 040504 */
def var c-esp-docto      like tt-extrato.esp-docto. /* 040504 */
DEF VAR c-tipo           AS CHAR.

Define Temp-Table tt-raw-digita Field raw-digita As Raw.
    
/*-------------- Definicao Parametros -----------------------------------*/
Define Input Parameter raw-param As Raw No-Undo.
Define Input Parameter Table For tt-raw-digita.

/*--------------- Definicao de Variaveis --------------------------------*/
Define Variable h-acomp As Handle No-Undo.

DEF VAR de-qtde-tot-esp   AS DEC NO-UNDO FORMAT "->>,>>>,>>9.99".
DEF VAR de-qtde-tot-forn  AS DEC NO-UNDO.
DEF VAR de-qtde-tot-geral AS DEC NO-UNDO.
DEF VAR de-val-tot-esp    AS DEC NO-UNDO FORMAT "->>,>>>,>>9.99".
DEF VAR de-val-tot-forn   AS DEC NO-UNDO FORMAT "->>>,>>>,>>9.99".
DEF VAR de-val-tot-geral  AS DEC NO-UNDO.
DEF VAR i-tot-carg        AS INT NO-UNDO.
def var c-destino         as char format "x(16)".

DEF VAR de-tot-dep     AS DEC NO-UNDO FORMAT "->>,>>>,>>9.99".
DEF VAR de-tot-dev     AS DEC NO-UNDO FORMAT "->>,>>>,>>9.99".
DEF VAR de-tot-tra-ent AS DEC NO-UNDO FORMAT "->>,>>>,>>9.99".
DEF VAR de-tot-tra-sai AS DEC NO-UNDO FORMAT "->>,>>>,>>9.99".
DEF VAR de-tot-cmp     AS DEC NO-UNDO FORMAT "->>,>>>,>>9.99".
                      
DEF VAR i-nr-contrato LIKE es-contrato-arroz.nr-contrato.
DEF VAR i-tick-ini    LIKE es-ticket.nr-ticket.
DEF VAR i-tick-fim    LIKE es-ticket.nr-ticket.

DEF VAR de-entra      AS DEC DECIMALS 3.
DEF VAR de-saida      AS DEC DECIMALS 3.
DEF VAR de-tot-saldo  AS DEC DECIMALS 3.
DEF VAR de-tot-secag  AS DEC DECIMALS 3.
DEF VAR de-tot-seco   AS DEC DECIMALS 3.
DEF VAR de-a-secar    AS DEC DECIMALS 3.
DEF VAR vl-tot-secag  AS DEC DECIMALS 3.
DEF VAR vl-tot-seco   AS DEC DECIMALS 3.
DEF VAR vl-a-secar    AS DEC DECIMALS 3.


/*--------------- Variaveis para o Rendimento Medio ---------------------*/

DEF VAR c-rend-medio      AS CHAR FORMAT "x(17)".
DEF VAR c-rend-contr      AS CHAR FORMAT "x(20)".
DEF VAR de-inteiro        AS DECI FORMAT ">>>,>>>,>>9.9999"  DECIMALS 3 NO-UNDO.
DEF VAR de-quebrado       AS DECI FORMAT ">>>,>>>,>>9.9999"  DECIMALS 3 NO-UNDO.
DEF VAR de-peso-liq       AS DECI FORMAT ">>>,>>>,>>9.99999" DECIMALS 5 NO-UNDO.
DEF VAR de-quantidade     AS DECI FORMAT ">>>,>>>,>>9.99999" DECIMALS 5 NO-UNDO.
DEF VAR de-contrato       AS DECI FORMAT ">>>,>>>,>>9.99999" DECIMALS 5 NO-UNDO.
DEF VAR i-contador        AS INTE NO-UNDO.

/* 21/01/04 */
DEF VAR de-valor-formula AS DEC.
DEF VAR de-soma AS DEC.

/*--------------- Definicao de Frames -----------------------------------*/

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

def temp-table tt-detalhes-sec
field desc-secagem like tt-detalhes.de-quantidade
field dt-trans    as date
field esp-docto   LIKE es-movto-arroz.esp-docto.

/*--------------- Cria Parametro ----------------------------------------*/
Create tt-param. 
Raw-Transfer raw-param To tt-param.


Find First tt-param No-Lock No-Error.
ASSIGN c-destino = {varinc/var00002.i 04 tt-param.destino}.

{include/i-rpvar.i}
/*{include/i-rpcab.i}*/

 Find First param-global No-Lock No-Error.

 Find empresa 
      Where empresa.ep-codigo = param-global.empresa-prin
      No-Lock No-Error.

 Assign c-empresa      = empresa.razao-social
        c-titulo-relat = "Modelo 2"
        c-sistema      = "Controle de Entradas"
        c-programa     = "ES4045"
        c-versao       = "2.04"                    
        c-revisao      = "00.000".

 Form HEADER
     Fill("-", 200)          Format "x(200)"     Skip
     c-empresa 
     c-titulo-relat  At 95
     "PerÌodo: "     AT 145
     tt-param.dt-trans-ini 
     " A "
     tt-param.dt-trans-fim
     "Folha:"        At 187 
     page-number     TO 200  Format ">>>>9"      Skip
     Fill("-", 178)          Format "x(178)" 
     Today                   Format "99/99/9999"
     "-" 
     String(Time,"HH:MM:SS")                     Skip(1)
     With stream-io width 200 no-labels no-box page-top frame f-cabec.

 Assign c-rodape="DATASUL - " + c-sistema + " - " + c-programa + " - V:" + c-versao +
                 "." + c-revisao
        c-rodape=fill("-", 200 - length(c-rodape)) + c-rodape.

 Form Header
     c-rodape format "x(200)"
     With stream-io width 200 no-labels no-box page-bottom frame f-rodape.



 FORM   da-dt-trans                 COLUMN-LABEL "Data" FORMAT "99/99/99"
        c-esp-docto                 COLUMN-LABEL "Operaá∆o"
        tt-extrato.nr-placa         COLUMN-LABEL "Placa"
        tt-extrato.nr-nota-fornec   COLUMN-LABEL "NFP"
        tt-extrato.nro-docto        COLUMN-LABEL "Dcto"
        /*tt-extrato.nfd              COLUMN-LABEL "NFD"*/
        tt-extrato.peso-fornec      COLUMN-LABEL "Peso NF KG"
        tt-extrato.peso-liq         COLUMN-LABEL "Peso Balanáa KG"
        de-dif                      COLUMN-LABEL "DIF"                 
        es-ticket.gr-impureza       COLUMN-LABEL "Impureza %"
        tt-extrato.desc-impureza    COLUMN-LABEL "Impureza KG"
        es-ticket.gr-umidade        COLUMN-LABEL "Umidade %"
        tt-extrato.desc-umidade     COLUMN-LABEL "Umidade KG"
        de-peso-final               COLUMN-LABEL "Peso Final KG" 
        tt-extrato.rend-inteiro     COLUMN-LABEL "Inteiros %"
        tt-extrato.desc-inteiro     COLUMN-LABEL "Inteiros KG"
        tt-extrato.rend-quebr       COLUMN-LABEL "Quebrados %"
        tt-extrato.desc-rendimento  COLUMN-LABEL "Quebrados KG"
        de-preto-ver                COLUMN-LABEL "Preto/Verm %"
        de-dec-preto-ver            COLUMN-LABEL "Preto/Verm KG"
        es-ticket.gr-verde          COLUMN-LABEL "Verde %"
        tt-extrato.desc-verde       COLUMN-LABEL "Verde KG"
        es-ticket.gr-gesso          COLUMN-LABEL "Gesso %"
        tt-extrato.desc-gesso       COLUMN-LABEL "Gesso KG"
        es-ticket.gr-manch-pic      COLUMN-LABEL "Manch/Pic %"
        tt-extrato.desc-manch-pic   COLUMN-LABEL "Manch/Pic KG"
        c-gr-impureza               COLUMN-LABEL "Impurezas %"
        tt-extrato.qtd-bonific      COLUMN-LABEL "Impurezas KG"
        tt-extrato.gr-secagem       COLUMN-LABEL "Secagem %"
        tt-extrato.desc-secagem     COLUMN-LABEL "Secagem KG"
        de-total                    COLUMN-LABEL "Total"
        de-valor-unit               COLUMN-LABEL "Valor Unit†rio por KG"
        de-valor-total              COLUMN-LABEL "Total a Pagar"
        WITH DOWN STREAM-IO WIDTH 250 FRAME f-detalhe.
             
    
/*---------------- Bloco Principal ---------------------------------------*/

Run utp/ut-acomp.p persistent set h-acomp.
Run pi-inicializar In h-acomp (Input "Impress∆o").
Run pi-seta-tipo   In h-acomp (Input 6).

EMPTY TEMP-TABLE tt-extrato.
       
ASSIGN vl-saldo = 0.00
       vl-aloca = 0.00
       vl-bloqu = 0.00
       vl-contr = 0.00
       vl-entre = 0.00
       vl-secag = 0.00
       vl-finan = 0.00
       val-saldo-ent = 0
       val-saldo-sai = 0
       val-saldo-bon = 0
       val-saldo-fin = 0
       val-saldo-tra = 0
       val-saldo-dev = 0.

FOR EACH es-contrato-arroz NO-LOCK
     WHERE es-contrato-arroz.nr-contrato >= tt-param.nr-contrato-ini
       AND es-contrato-arroz.nr-contrato <= tt-param.nr-contrato-fim:


    IF es-contrato-arroz.tipo-contrato = 1 THEN DO: /* Compra Direta */
       RUN pi-movto-arroz (INPUT "IPL").
       RUN pi-movto-arroz (INPUT "CMP").
    END.
    IF es-contrato-arroz.tipo-contrato = 2 THEN DO: /* Deposito */
       RUN pi-movto-arroz (INPUT "IPL").
       RUN pi-movto-arroz (INPUT "DEP").
    END.
    IF es-contrato-arroz.tipo-contrato = 3 THEN DO: /* Importacao */
       RUN pi-movto-arroz (INPUT "IPL").
       RUN pi-movto-arroz (INPUT "IMP").
    END.
    /* armazenagem - erika - 07/08 */
    IF es-contrato-arroz.tipo-contrato = 4 OR       /* Armazenagem */
       es-contrato-arroz.tipo-contrato = 5 THEN DO: /* tra-estab */
       RUN pi-movto-arroz (INPUT "DEP").
       RUN pi-movto-arroz (INPUT "DSC").
    END.

    RUN pi-resumo. /* erika 06/07 */

    /* 31/01/04 */
    FIND FIRST tt-soma-contrato NO-LOCK NO-ERROR.
    IF NOT AVAIL tt-soma-contrato THEN
      CREATE tt-soma-contrato.
    ASSIGN 
        tt-soma-contrato.umidade        = tt-soma-contrato.umidade        + es-contrato-arroz.gr-umidade
        tt-soma-contrato.rend-inteiro   = tt-soma-contrato.rend-inteiro   + es-contrato-arroz.rend-inteiro
        tt-soma-contrato.preto-vermelho = tt-soma-contrato.preto-vermelho + es-contrato-arroz.u-dec-3
        tt-soma-contrato.verde          = tt-soma-contrato.verde          + es-contrato-arroz.u-dec-5
        tt-soma-contrato.gesso          = tt-soma-contrato.gesso          + es-contrato-arroz.gr-gesso
        tt-soma-contrato.manch-pic      = tt-soma-contrato.manch-pic      + es-contrato-arroz.u-dec-4
        tt-soma-contrato.impureza       = tt-soma-contrato.impureza       + es-contrato-arroz.gr-impureza
        tt-soma-contrato.rend-quebr     = tt-soma-contrato.rend-quebr     + es-contrato-arroz.rend-quebr
        tt-soma-contrato.nr-contratos   = tt-soma-contrato.nr-contratos   + 1.
END.


{include/i-rpout.i &pagesize = "0"}

RUN pi-impressao.
{include/i-rpclo.i}
RUN pi-finalizar IN h-acomp.
RETURN 'OK'.

PROCEDURE pi-movto-arroz:

    DEF INPUT PARAM c-esp-docto LIKE es-movto-arroz.esp-docto.
        

    FOR EACH es-movto-arroz NO-LOCK 
        WHERE es-movto-arroz.nome-matriz  >= tt-param.matriz-ini 
          AND es-movto-arroz.nome-matriz  <= tt-param.matriz-fim
          AND es-movto-arroz.cod-emitente >= tt-param.cod-emitente-ini
          AND es-movto-arroz.cod-emitente <= tt-param.cod-emitente-fim
          AND es-movto-arroz.nr-contrato   = es-contrato-arroz.nr-contrato
          /*AND es-movto-arroz.nr-contrato  >= tt-param.nr-contrato-ini
          AND es-movto-arroz.nr-contrato  <= tt-param.nr-contrato-fim  */
          AND es-movto-arroz.cod-estabel  >= tt-param.cod-estab-ini
          AND es-movto-arroz.cod-estabel  <= tt-param.cod-estab-fim  
          AND es-movto-arroz.it-codigo    >= tt-param.it-codigo-ini
          AND es-movto-arroz.it-codigo    <= tt-param.it-codigo-fim  
          AND es-movto-arroz.dt-trans     >= tt-param.dt-trans-ini
          AND es-movto-arroz.dt-trans     <= tt-param.dt-trans-fim
          AND es-movto-arroz.esp-docto     = c-esp-docto
        BREAK BY es-movto-arroz.cod-emitente
              BY es-movto-arroz.esp-docto   
              BY es-movto-arroz.dt-trans
              BY es-movto-arroz.nr-contrato:
       

        FIND FIRST emitente NO-LOCK
            WHERE emitente.nome-abrev = es-movto-arroz.nome-matriz
            NO-ERROR.
        if NOT avail emitente then do:    
        
            FIND FIRST emitente NO-LOCK
                WHERE emitente.cod-emitente = es-movto-arroz.cod-emitente
                NO-ERROR.
        
        end.
            

        FIND FIRST es-ticket use-index idx-docto
            WHERE es-ticket.serie        = es-movto-arroz.serie-docto
              AND es-ticket.nro-docto    = es-movto-arroz.nro-docto
              AND es-ticket.cod-emitente = es-movto-arroz.cod-emitente
              AND es-ticket.nat-operacao = es-movto-arroz.nat-operacao             
              AND ((tt-param.l-compra     AND es-ticket.operacao =  1) 
               OR  (tt-param.l-deposito   AND es-ticket.operacao =  2) 
               OR  (tt-param.l-terceiro   AND es-ticket.operacao =  3) 
               OR  (tt-param.l-importacao AND es-ticket.operacao =  4) 
               OR  (tt-param.l-o-entradas AND es-ticket.operacao =  5) 
               OR  (tt-param.l-o-saidas   AND es-ticket.operacao =  6) 
               OR  (tt-param.l-tr-tit     AND es-ticket.operacao =  7)
               OR  (tt-param.l-arm-ent    AND es-ticket.operacao =  8)
               OR  (tt-param.l-arm-sai    AND es-ticket.operacao =  9)
               OR  (tt-param.l-tra-est    AND es-ticket.operacao = 10))
            NO-LOCK NO-ERROR.

        IF  NOT AVAIL es-ticket THEN DO:
            FIND FIRST es-ticket use-index idx-docto
            WHERE es-ticket.serie        = es-movto-arroz.serie-docto
              AND es-ticket.nro-docto    = es-movto-arroz.nro-docto
              AND es-ticket.cod-produtor = es-movto-arroz.cod-emitente
              AND es-ticket.nat-operacao = es-movto-arroz.nat-operacao             
              AND ((tt-param.l-compra     AND es-ticket.operacao =  1) 
               OR  (tt-param.l-deposito   AND es-ticket.operacao =  2) 
               OR  (tt-param.l-terceiro   AND es-ticket.operacao =  3) 
               OR  (tt-param.l-importacao AND es-ticket.operacao =  4) 
               OR  (tt-param.l-o-entradas AND es-ticket.operacao =  5) 
               OR  (tt-param.l-o-saidas   AND es-ticket.operacao =  6) 
               OR  (tt-param.l-tr-tit     AND es-ticket.operacao =  7)
               OR  (tt-param.l-arm-ent    AND es-ticket.operacao =  8)
               OR  (tt-param.l-arm-sai    AND es-ticket.operacao =  9)
               OR  (tt-param.l-tra-est    AND es-ticket.operacao = 10))
            NO-LOCK NO-ERROR.
        END.

        IF  NOT AVAIL es-ticket THEN DO:
            /* 230304 para sair transferencia de titularidade */
            FIND FIRST es-transferencia where
                       es-transferencia.nr-contrato-ent = es-movto-arroz.nr-contrato and
                       es-transferencia.nr-ticket       = es-movto-arroz.nr-ticket
                       NO-LOCK no-error.
        END.
            
                /* 230304 */
        IF NOT AVAIL  es-ticket and
           NOT AVAIL es-transferencia THEN NEXT.


        FIND FIRST docum-est NO-LOCK
            WHERE docum-est.nro-docto    = es-movto-arroz.nro-docto
              AND docum-est.serie        = es-movto-arroz.serie
              AND docum-est.nat-operacao = es-movto-arroz.nat-operacao NO-ERROR.
                                                                               
        IF AVAIL emitente THEN DO:
            

            FIND FIRST tt-extrato
                WHERE tt-extrato.cod-emitente = emitente.cod-emitente
                  AND tt-extrato.nr-contrato  = es-movto-arroz.nr-contrato
                  AND tt-extrato.nr-ticket    = es-movto-arroz.nr-ticket  
                  AND tt-extrato.serie        = es-movto-arroz.serie
                  AND tt-extrato.nro-docto    = es-movto-arroz.nro-docto
                  AND tt-extrato.esp-docto    = es-movto-arroz.esp-docto
                NO-ERROR.
            IF  NOT AVAIL tt-extrato THEN DO:
                IF  es-movto-arroz.esp-docto <> "BON" AND
                    es-movto-arroz.esp-docto <> "DSC" AND
                    es-movto-arroz.esp-docto <> "SEC" THEN DO:

                    

                    CREATE tt-extrato.
                    ASSIGN tt-extrato.nome-matriz    = es-movto-arroz.nome-matriz
                           tt-extrato.cod-emitente   = emitente.cod-emitente
                           tt-extrato.nr-contrato    = es-movto-arroz.nr-contrato 
                           tt-extrato.nr-ticket      = es-movto-arroz.nr-ticket
                           tt-extrato.serie          = es-movto-arroz.serie
                           tt-extrato.esp-docto      = es-movto-arroz.esp-docto
                           tt-extrato.tipo-trans     = es-movto-arroz.tipo-trans
                           tt-extrato.dt-trans       = es-movto-arroz.dt-trans
                           tt-extrato.nr-placa       = IF AVAIL es-ticket THEN es-ticket.nr-placa-cam   ELSE ""
                           tt-extrato.nr-nota-fornec = IF AVAIL es-ticket THEN es-ticket.nr-nota-fornec ELSE ""
                           tt-extrato.nro-docto      = es-movto-arroz.nro-docto
                           tt-extrato.peso-fornec    = IF AVAIL es-ticket THEN es-ticket.peso-desmem[1] ELSE 0
                           tt-extrato.valor-unit     = es-movto-arroz.valor 
                           tt-extrato.peso-liq       = es-movto-arroz.quantidade
                           tt-extrato.peso-total     = tt-extrato.peso-liq
                           tt-extrato.rendimento     = IF AVAIL es-ticket THEN
                                                       STRING((es-ticket.rend-inteiro),"Z99.9999") + " X " + 
                                                       STRING((es-ticket.rend-quebr),"Z99.9999")
                                                       ELSE es-movto-arroz.u-char-1.
                           
                    ASSIGN tt-extrato.valor-bruto    = 0
                           tt-extrato.valor-total    = 0.
                    
                    /*MESSAGE "create tt-ext" es-movto-arroz.cod-estabel tt-extrato.nro-docto tt-extrato.esp-docto 
                        emitente.cod-emitente es-movto-arroz.nome-matriz VIEW-AS ALERT-BOX.*/

                    IF AVAIL docum-est THEN DO:
                        FOR EACH dupli-apagar OF docum-est.
                            IF dupli-apagar.parcela <> "11" AND
                               dupli-apagar.parcela <> "12" THEN
                               ASSIGN tt-extrato.valor-total = tt-extrato.valor-total + dupli-apagar.valor.
    
                            ASSIGN tt-extrato.valor-bruto = tt-extrato.valor-bruto + dupli-apagar.valor.
                        END.
                    END.
             
                END.
            END.
        END. /*if avail emitente*/
    END.

    FOR EACH es-movto-arroz NO-LOCK 
        WHERE es-movto-arroz.nome-matriz  >= tt-param.matriz-ini 
          AND es-movto-arroz.nome-matriz  <= tt-param.matriz-fim
          AND es-movto-arroz.cod-emitente >= tt-param.cod-emitente-ini
          AND es-movto-arroz.cod-emitente <= tt-param.cod-emitente-fim
          AND es-movto-arroz.nr-contrato   = es-contrato-arroz.nr-contrato
          /*AND es-movto-arroz.nr-contrato  >= tt-param.nr-contrato-ini
          AND es-movto-arroz.nr-contrato  <= tt-param.nr-contrato-fim  */
          AND es-movto-arroz.cod-estabel  >= tt-param.cod-estab-ini
          AND es-movto-arroz.cod-estabel  <= tt-param.cod-estab-fim  
          AND es-movto-arroz.it-codigo    >= tt-param.it-codigo-ini
          AND es-movto-arroz.it-codigo    <= tt-param.it-codigo-fim  
          AND es-movto-arroz.dt-trans     >= tt-param.dt-trans-ini
          AND es-movto-arroz.dt-trans     <= tt-param.dt-trans-fim
          AND (es-movto-arroz.esp-docto     = "DSC" OR es-movto-arroz.esp-docto = "BON" OR es-movto-arroz.esp-docto = "SEC"),
           FIRST es-ticket NO-LOCK
                WHERE es-ticket.nr-ticket = es-movto-arroz.nr-ticket
        BREAK BY es-movto-arroz.cod-emitente
              BY es-movto-arroz.esp-docto   
              BY es-movto-arroz.dt-trans
              BY es-movto-arroz.nr-contrato:

        FIND FIRST tt-extrato
            WHERE tt-extrato.cod-emitente = emitente.cod-emitente
              AND tt-extrato.nr-contrato  = es-movto-arroz.nr-contrato
              AND tt-extrato.nr-ticket    = es-movto-arroz.nr-ticket  
              AND tt-extrato.esp-docto    = c-esp-docto
            NO-ERROR.
        
          IF  AVAIL tt-extrato THEN DO:
            
            /*** DESCONTOS ***/
            IF  es-movto-arroz.esp-docto = "DSC" THEN DO:
                /*RUN esp/esapi005.p (INPUT es-ticket.nr-ticket).*/


                /* aqui somente era chamado o esapi005, agora est† chamando o pi-quantidade tambÇm
                     para calcular a bonificacao com peso limpo e cahmo o esapi005b tambÇm que faz o 
                     c†lculo correto 081104 */
                  RUN esp/esapi005.p (INPUT es-ticket.nr-ticket).          

                  IF es-ticket.dt-entrada > 11/09/04 THEN DO: 
                  
                      RUN pi-quantidade.
            
                      FOR EACH tt-detalhes:
                          DELETE tt-detalhes.
                      END.
            
                      RUN esp/esapi005b.p (INPUT es-ticket.nr-ticket,
                                           INPUT di-quantidade).
            
                  END. /* interpreta data */ 
                    /* Umidade */    

                    FIND FIRST tt-detalhes NO-LOCK
                        WHERE tt-detalhes.c-esp-docto   = "DSC"
                          AND tt-detalhes.c-tp-desconto MATCHES ("*UMIDADE*")
                        NO-ERROR.
                    
                    IF AVAIL tt-detalhes THEN
                        ASSIGN tt-extrato.gr-umidade   = tt-detalhes.de-perc.
                    IF es-movto-arroz.tp-desconto MATCHES ("*UMIDADE*") THEN
                        ASSIGN tt-extrato.desc-umidade = tt-extrato.desc-umidade + es-movto-arroz.quantidade. /*13/01/2004 */ 
                
                    /* Impureza */
                
                    FIND FIRST tt-detalhes NO-LOCK
                        WHERE tt-detalhes.c-esp-docto   = "DSC"
                          AND tt-detalhes.c-tp-desconto MATCHES ("*IMPUREZA*")
                        NO-ERROR.
                    IF AVAIL tt-detalhes THEN
                        ASSIGN tt-extrato.gr-impureza   = tt-detalhes.de-perc.
                    IF es-movto-arroz.tp-desconto MATCHES ("*IMPUREZA*") THEN
                        ASSIGN tt-extrato.desc-impureza = tt-extrato.desc-impureza + es-movto-arroz.quantidade.  /*13/01/2004 */ 
                
                    /* Rendimento */
                    
                    FIND FIRST tt-detalhes NO-LOCK
                        WHERE tt-detalhes.c-esp-docto   = "DSC"
                          AND tt-detalhes.c-tp-desconto MATCHES ("*RENDIMENTO*")
                        NO-ERROR.
                        
                    
                    IF es-movto-arroz.tp-desconto MATCHES ("*RENDIMENTO*") THEN
                        ASSIGN tt-extrato.desc-rendimento = tt-extrato.desc-rendimento + es-movto-arroz.quantidade. /* 1301/2004 */

                    ASSIGN tt-extrato.rend-inteiro    = es-ticket.rend-inteiro
                           tt-extrato.rend-quebr      = es-ticket.rend-quebr.

                    /* Inteiro */
                    
                    FIND FIRST tt-detalhes NO-LOCK
                        WHERE tt-detalhes.c-esp-docto   = "DSC"
                          AND tt-detalhes.c-tp-desconto MATCHES ("*INTEIRO*")
                        NO-ERROR.
                    
                    IF es-movto-arroz.tp-desconto MATCHES ("*INTEIRO*") THEN
                        ASSIGN tt-extrato.desc-inteiro      = tt-extrato.desc-inteiro +  es-movto-arroz.quantidade. /* 13/01/2004 */

                    ASSIGN tt-extrato.rend-desc-inteiro = es-ticket.rend-inteiro.

                    /* Secagem */

                    FIND FIRST tt-detalhes NO-LOCK
                        WHERE tt-detalhes.c-esp-docto   = "DSC"
                          AND tt-detalhes.c-tp-desconto MATCHES ("*SECAGEM*")
                        NO-ERROR.

                    IF es-movto-arroz.tp-desconto MATCHES ("*SECAGEM*") THEN
                       ASSIGN tt-extrato.desc-secagem  = tt-extrato.desc-secagem + es-movto-arroz.quantidade. /*13/01/04*/

                    ASSIGN tt-extrato.gr-secagem = es-ticket.gr-secagem.
 
                    /* 17/10 erika - secagem por ticket MARROM*/   
                    FIND FIRST tt-detalhes NO-LOCK
                        WHERE tt-detalhes.c-esp-docto   = "SEC"
                          AND tt-detalhes.c-tp-desconto = "Secagem Proporcional"
                        NO-ERROR.
                    IF AVAIL tt-detalhes THEN do:
                       ASSIGN tt-extrato.desc-secagem = tt-extrato.desc-secagem + tt-detalhes.de-quantidade.
                       
                        


                       create tt-detalhes-sec.
                       assign tt-detalhes-sec.desc-secagem =  tt-detalhes-sec.desc-secagem  + tt-detalhes.de-quantidade /*13/01/04 */
                              tt-detalhes-sec.dt-trans     = es-ticket.dt-entrada
                              tt-detalhes-sec.esp-docto    = "SEC".                       
                    end. /* marrom */
                
                IF  AVAIL es-contrato-arroz AND
                    es-contrato-arroz.u-int-5 <> 0 OR  
                    es-contrato-arroz.u-log-2 = YES THEN DO:

                    ASSIGN i-nr-contrato = tt-extrato.nr-contrato
                           i-tick-ini    = tt-extrato.nr-ticket
                           i-tick-fim    = tt-extrato.nr-ticket.
                   


                   


                    RUN esp/esapi007a.p  (INPUT i-nr-contrato,
                                         INPUT i-tick-ini,
                                         INPUT i-tick-fim,
                                         input  es-movto-arroz.ins-estadual,
                                         INPUT  es-movto-arroz.cod-estabel,
                                         OUTPUT de-entra,
                                         OUTPUT de-saida,
                                         OUTPUT de-tot-saldo,
                                         OUTPUT de-tot-secag,
                                         OUTPUT de-tot-seco,
                                         OUTPUT de-a-secar,
                                         OUTPUT vl-tot-secag,
                                         OUTPUT vl-tot-seco,
                                         OUTPUT vl-a-secar).
      




                    /*MESSAGE "secag" de-tot-secag "seco" de-tot-seco VIEW-AS ALERT-BOX.

          
              
             MESSAGE "apagou tudo e coloqcou esse " de-tot-secag - de-tot-seco VIEW-AS ALERT-BOX.*/

             de-soma = de-soma + (de-tot-secag - de-tot-seco).

                /*MESSAGE "de-soma" de-soma VIEW-AS ALERT-BOX.*/
      
                    IF es-contrato-arroz.u-int-5 > 0 THEN do:
                      ASSIGN tt-extrato.gr-secagem   = es-contrato-arroz.u-int-5 / 100
                             tt-extrato.desc-secagem = de-tot-secag.
                    end. /* u-int */         
                    else do:
                        FIND FIRST bf-es-ticket WHERE
                                    bf-es-ticket.nr-ticket = es-movto-arroz.nr-ticket NO-LOCK NO-ERROR.
            
                        RUN esp\esapi009.p (INPUT es-movto-arroz.cod-estabel,
                                            INPUT tt-extrato.nr-ticket,
                                            INPUT bf-es-ticket.dt-entrada,
                                            OUTPUT de-valor-formula).  

                        
                        /*de-soma = de-soma + (de-tot-secag * (de-valor-formula / 100)).*/


                      ASSIGN tt-extrato.gr-secagem   = de-valor-formula /*/ 100 */
                             tt-extrato.desc-secagem = de-tot-secag.
                    /*MESSAGE "depois secagem"

                         "âs-param" es-param-estab.cod-estabel 
                         "inpu ticket" tt-extrato.nr-ticket
                          tt-extrato.nro-docto
                          tt-extrato.desc-secagem 
                        
                        VIEW-AS ALERT-BOX.*/
                    end. /* u-log */

                END.
      


                
                    /* Amarelo */

                    FIND FIRST tt-detalhes NO-LOCK
                        WHERE tt-detalhes.c-esp-docto   = "DSC"
                          AND tt-detalhes.c-tp-desconto MATCHES ("*AMARELO*")
                        NO-ERROR.

                    IF AVAIL tt-detalhes THEN
                        ASSIGN tt-extrato.gr-amarelo    = tt-detalhes.de-perc.
                    IF es-movto-arroz.tp-desconto MATCHES ("*AMARELO*") THEN
                        ASSIGN tt-extrato.desc-amarelo  = tt-extrato.desc-amarelo   + es-movto-arroz.quantidade. /*1301/04*/
                          
                    /* Vermelho */
                             
                    FIND FIRST tt-detalhes NO-LOCK
                        WHERE tt-detalhes.c-esp-docto   = "DSC"
                          AND tt-detalhes.c-tp-desconto MATCHES ("*VERMELHO*")
                        NO-ERROR.
                        
                    IF AVAIL tt-detalhes THEN
                        ASSIGN tt-extrato.gr-vermelho  = tt-detalhes.de-perc.
                    IF es-movto-arroz.tp-desconto MATCHES ("*VERMELHO*") THEN
                        ASSIGN tt-extrato.desc-vermelho = tt-extrato.desc-vermelho + es-movto-arroz.quantidade. /*13/01/04*/

                    /* Preto / Vermelho */

                    FIND FIRST tt-detalhes NO-LOCK
                        WHERE tt-detalhes.c-esp-docto   = "DSC"
                          AND tt-detalhes.c-tp-desconto MATCHES ("*PRETO/VERMEL*")
                        NO-ERROR.
                
                    IF AVAIL tt-detalhes THEN
                        ASSIGN tt-extrato.gr-preto-verm    = tt-detalhes.de-perc.
                    IF es-movto-arroz.tp-desconto MATCHES ("*PRETO/VERMEL*") THEN
                        ASSIGN tt-extrato.desc-preto-verm  = tt-extrato.desc-preto-verm + es-movto-arroz.quantidade. /*13/01/04*/
                    
                    /* Verde */
                    
                    FIND FIRST tt-detalhes NO-LOCK
                        WHERE tt-detalhes.c-esp-docto   = "DSC"
                          AND tt-detalhes.c-tp-desconto MATCHES ("*VERDE*")
                        NO-ERROR.
                
                    IF AVAIL tt-detalhes THEN
                        ASSIGN tt-extrato.gr-verde    = tt-detalhes.de-perc.
                    IF es-movto-arroz.tp-desconto MATCHES ("*VERDE*") THEN
                        ASSIGN tt-extrato.desc-verde  = tt-extrato.desc-verde + es-movto-arroz.quantidade. /*13/01/04*/

                    /* gesso */
                    
                    FIND FIRST tt-detalhes NO-LOCK
                        WHERE tt-detalhes.c-esp-docto   = "DSC"
                          AND tt-detalhes.c-tp-desconto MATCHES ("*gesso*")
                        NO-ERROR.
                
                    IF AVAIL tt-detalhes THEN
                        ASSIGN tt-extrato.gr-gesso    = tt-detalhes.de-perc.
                    IF es-movto-arroz.tp-desconto MATCHES ("*gesso*") THEN
                        ASSIGN tt-extrato.desc-gesso  = tt-extrato.desc-gesso + es-movto-arroz.quantidade. /*13/01/04*/

                    /* Manchado / Picado */

                    FIND FIRST tt-detalhes NO-LOCK
                        WHERE tt-detalhes.c-esp-docto   = "DSC"
                          AND tt-detalhes.c-tp-desconto MATCHES ("*MANCHADO/PIC*")
                        NO-ERROR.
                    
                    IF AVAIL tt-detalhes THEN
                        ASSIGN tt-extrato.gr-manch-pic   = tt-detalhes.de-perc.
                    IF es-movto-arroz.tp-desconto MATCHES ("*MANCHADO/PIC*") THEN
                        ASSIGN tt-extrato.desc-manch-pic = tt-extrato.desc-manch-pic + es-movto-arroz.quantidade. /*13/01/04*/
            

            END.
            /*** BONIFICACAO ***/
            IF  es-movto-arroz.esp-docto = "BON" THEN DO:
                ASSIGN tt-extrato.qtd-bonific = tt-extrato.qtd-bonific + es-movto-arroz.quantidade  /*13/01/04*/
                       tt-extrato.val-bonific = tt-extrato.val-bonific + (es-movto-arroz.quantidade * es-movto-arroz.valor).
            END.       

            
        END.
        RUN pi-acompanhar IN h-acomp (INPUT "Contrato: " + STRING(es-movto-arroz.nr-contrato)).
    END.
END.


PROCEDURE pi-impressao:

 VIEW FRAME f-cabec.
 VIEW FRAME f-rodape.

    FIND FIRST es-contrato-arroz NO-LOCK
         WHERE es-contrato-arroz.nr-contrato = tt-param.nr-contrato-ini
         NO-ERROR.
    
    FIND estabelec NO-LOCK
        WHERE estabel.cod-estabel = es-contrato-arroz.cod-estabel
        NO-ERROR.

    FIND ITEM NO-LOCK
        WHERE ITEM.it-codigo = es-contrato-arroz.it-codigo
        NO-ERROR.

    FIND emitente NO-LOCK
        WHERE emitente.cod-emitente = es-contrato-arroz.cod-emitente
        NO-ERROR.

    
    ASSIGN c-tipo = IF es-contrato-arroz.tipo-contrato = 1 THEN "Compra Direta" ELSE
                    IF es-contrato-arroz.tipo-contrato = 2 THEN "Dep¢sito"      ELSE 
                    IF es-contrato-arroz.tipo-contrato = 3 then "Importaá∆o"    else
                    IF es-contrato-arroz.tipo-contrato = 4 then "Armazenagem"   else
                    IF es-contrato-arroz.tipo-contrato = 5 then "Tra.Estab"     else "".
    

    DISP "Data da emiss∆o do relat¢rio: " STRING(TODAY,"99/99/9999")  at 40 skip
      "N£mero do contrato DE: " STRING(tt-param.nr-contrato-ini) 
      " ATê " STRING(tt-param.nr-contrato-fim) at 40 skip
      "Local de entrada: " estabel.nome " - " + estabel.cidade at 40 skip
      "Operaá∆o: "  c-tipo at 40 skip
      " Pedido: " emitente.nome-emit " " ITEM.desc-item
      WITH DOWN STREAM-IO WIDTH 250 FRAME f-data. /* 040504 */
    
    ASSIGN de-tot-dep     = 0
           de-tot-dev     = 0
           de-tot-cmp     = 0
           de-tot-tra-ent = 0
           de-tot-tra-sai = 0.

    FOR EACH tt-extrato
        BY tt-extrato.dt-trans:
        IF tt-extrato.esp-docto BEGINS "sec" THEN NEXT.
        FIND FIRST es-ticket NO-LOCK
            WHERE es-ticket.nr-ticket = tt-extrato.nr-ticket
            NO-ERROR.
        
        FIND FIRST es-contrato-arroz NO-LOCK
            WHERE es-contrato-arroz.nr-contrato = tt-extrato.nr-contrato
            NO-ERROR.
         
        ASSIGN tt-extrato.peso-total  = tt-extrato.peso-total - 
                                        tt-extrato.desc-umidade - 
                                        tt-extrato.desc-impureza.
        
        /*
        IF tt-extrato.esp-doc = "DEP" OR
           tt-extrato.esp-doc = "IPL" OR
           tt-extrato.esp-doc = "IMP" THEN
           
            ASSIGN de-tot-dep        = de-tot-dep        + tt-extrato.peso-liq
                   de-qtde-tot-esp   = de-qtde-tot-esp   + tt-extrato.peso-total 
                   de-qtde-tot-forn  = de-qtde-tot-forn  + tt-extrato.peso-total
                   de-val-tot-esp    = de-val-tot-esp    + tt-extrato.valor-total
                   de-val-tot-forn   = de-val-tot-forn   + tt-extrato.valor-total
                   de-qtde-tot-geral = de-qtde-tot-geral + (tt-extrato.peso-total 
                                
            - tt-extrato.desc-rendimento 
            - tt-extrato.desc-inteiro    
            - tt-extrato.desc-secagem  
            - tt-extrato.desc-amarelo  
            - tt-extrato.desc-vermelho 
            - tt-extrato.desc-preto-verm  
            - tt-extrato.desc-verde  
            - tt-extrato.desc-manch-pic )
                   de-val-tot-geral  = de-val-tot-geral  + tt-extrato.valor-total.
            
        ELSE
        IF tt-extrato.nr-ticket = 0 or
           tt-extrato.esp-doc = "TRA" THEN
            ASSIGN de-qtde-tot-esp   = de-qtde-tot-esp   - tt-extrato.peso-total 
                   de-qtde-tot-forn  = de-qtde-tot-forn  - tt-extrato.peso-total
                   de-val-tot-esp    = de-val-tot-esp    - tt-extrato.valor-total
                   de-val-tot-forn   = de-val-tot-forn   - tt-extrato.valor-total
                   de-qtde-tot-geral = de-qtde-tot-geral - tt-extrato.peso-total
                   de-val-tot-geral  = de-val-tot-geral  - tt-extrato.valor-total.

        ELSE
            ASSIGN de-qtde-tot-esp   = de-qtde-tot-esp   + tt-extrato.peso-total 
                   de-qtde-tot-forn  = de-qtde-tot-forn  + tt-extrato.peso-total
                   de-val-tot-esp    = de-val-tot-esp    + tt-extrato.valor-total
                   de-val-tot-forn   = de-val-tot-forn   + tt-extrato.valor-total
                   de-qtde-tot-geral = de-qtde-tot-geral + tt-extrato.peso-total
                   de-val-tot-geral  = de-val-tot-geral  + tt-extrato.valor-total.*/



        IF  tt-extrato.esp-doc = "DEP" OR
            tt-extrato.esp-doc = "IPL" OR
            tt-extrato.esp-doc = "IMP" THEN
            ASSIGN de-tot-dep = de-tot-dep + tt-extrato.peso-liq.
        ELSE
        IF  tt-extrato.esp-doc = "DEV" THEN
            ASSIGN de-tot-dev = de-tot-dev + tt-extrato.peso-liq.
        ELSE
        IF  tt-extrato.esp-doc = "CMP" THEN 
            ASSIGN de-tot-cmp = de-tot-cmp + tt-extrato.peso-liq.
        ELSE
        IF  tt-extrato.esp-doc = "TRA" THEN DO:
            IF  tt-extrato.tipo-trans = 1 THEN
                ASSIGN de-tot-tra-ent = de-tot-tra-ent + tt-extrato.peso-liq.
            ELSE
                ASSIGN de-tot-tra-sai = de-tot-tra-sai + tt-extrato.peso-liq.
        END.

        /* erika */
        IF  tt-extrato.esp-doc = "DEP" and
            es-contrato-arroz.tipo-contrato = 4 then 
            tt-extrato.esp-doc = "ARM". 
        IF  tt-extrato.esp-doc = "DEP" and
            es-contrato-arroz.tipo-contrato = 5 then 
            tt-extrato.esp-doc = "TRE".
        
              
        assign de-peso-final    = tt-extrato.peso-liq - tt-extrato.desc-impureza - tt-extrato.desc-umidade

               de-dif           = tt-extrato.peso-liq - tt-extrato.peso-fornec

               c-gr-impureza    = IF es-contrato-arroz.gr-impureza <= tt-extrato.gr-impureza
                                    THEN es-contrato-arroz.gr-impureza
                                    ELSE tt-extrato.gr-impureza

               de-total         = tt-extrato.qtd-bonific - tt-extrato.desc-inteiro - 
                                  tt-extrato.desc-rendimento - tt-extrato.desc-verde - tt-extrato.desc-gesso -
                                  tt-extrato.desc-manch-pic - tt-extrato.desc-secagem

               de-preto-ver     = IF es-ticket.gr-preto-ver <> 0 
                                     THEN  es-ticket.gr-preto-ver
                                     ELSE  es-ticket.gr-vermelho

               de-dec-preto-ver = IF es-ticket.gr-preto-ver <> 0 
                                    THEN  tt-extrato.desc-preto-ver
                                    ELSE  tt-extrato.desc-vermelho
                                    
               da-dt-trans      = tt-extrato.dt-trans
               
               c-esp-docto      = tt-extrato.esp-docto.

                                
                                
        IF  es-contrato-arroz.tipo-contrato = 1 THEN DO:
          ASSIGN de-valor-unit  = tt-extrato.valor-unit
                 de-valor-total = tt-extrato.valor-total.
        END.                         
        else assign de-valor-unit  = 0
                    de-valor-total = 0.
                                        
        
        disp   da-dt-trans
               c-esp-docto
               tt-extrato.nr-placa
               tt-extrato.nr-nota-fornec
               tt-extrato.nro-docto
               
               /*tt-extrato.nfd*/
               

               tt-extrato.peso-fornec
               tt-extrato.peso-liq
               de-dif
               /* Impureza */
               es-ticket.gr-impureza
               tt-extrato.desc-impureza
               /* Umidade */
               es-ticket.gr-umidade
               tt-extrato.desc-umidade
               de-peso-final
               /* Inteiro */
               tt-extrato.rend-inteiro
               tt-extrato.desc-inteiro
               /* Quebrado */
               tt-extrato.rend-quebr
               tt-extrato.desc-rendimento
               /* Preto Vermelho */
               de-preto-ver
               de-dec-preto-ver    
               /* Verde */
               es-ticket.gr-verde
               tt-extrato.desc-verde
               /* gesso */
               es-ticket.gr-gesso
               tt-extrato.desc-gesso
               /* Manchado Picado */
               es-ticket.gr-manch-pic
               tt-extrato.desc-manch-pic
               /* Bofificacao - Impurezas */
               c-gr-impureza
               tt-extrato.qtd-bonific
               /* Secagem */               
               tt-extrato.gr-secagem
               tt-extrato.desc-secagem
               de-total
               de-valor-unit 
               de-valor-total
               with frame f-detalhe.
           
           
           


    END.

    /* 19/10 - inicio - marrom somente uma vez para exibir detalhes na primeira parte da planilha */
    for each tt-detalhes-sec NO-LOCK.
                

        IF tt-detalhes-sec.esp-docto = "SEC" THEN next.
          
        assign de-total     = tt-detalhes-sec.desc-secagem        
               da-dt-trans  = tt-detalhes-sec.dt-trans
               c-esp-docto  = tt-detalhes-sec.esp-docto.

            
        disp da-dt-trans
             c-esp-docto
             de-total
             with frame f-detalhes.
   

    end. /* avail tt-detalhes */
    /* fim 19/10 */

    /************************
    IF tt-param.medias = YES THEN DO:
        RUN esp/esapi006.p (INPUT es-contrato-arroz.nr-contrato,
                            OUTPUT c-rend-medio).
        DISP "RENDIMENTO MêDIO" SKIP
               chworksheet:range("F164"):VALUE   = "=F162/" + STRING(i-cont)
               chworksheet:range("G164"):VALUE   = "=G162/" + STRING(i-cont)
               chworksheet:range("H164"):VALUE   = "=H162/" + STRING(i-cont)
               chworksheet:range("J164"):VALUE   = "=J162/" + STRING(i-cont)
               chworksheet:range("L164"):VALUE   = "=L162/" + STRING(i-cont)
               chworksheet:range("M164"):VALUE   = "=M162/" + STRING(i-cont)
               /*
               chworksheet:range("L164"):VALUE   = "=L162/" + STRING(i-cont)
               */
               chworksheet:range("O164"):VALUE   = "=O162/" + STRING(i-cont)
               /*
               chworksheet:range("N164"):VALUE   = "=N162/" + STRING(i-cont)
               */
               chworksheet:range("Q164"):VALUE   = "=Q162/"  + STRING(i-cont)
               chworksheet:range("S164"):VALUE   = "=S162/"  + STRING(i-cont)
               chworksheet:range("U164"):VALUE   = "=U162/"  + STRING(i-cont)
               chworksheet:range("W164"):VALUE   = "=W162/"  + STRING(i-cont)
               chworksheet:range("Y164"):VALUE   = "=Y162/"  + STRING(i-cont)
               
              /*chworksheet:range("AA164"):VALUE  = "=AA162/" + STRING(i-cont) 30/01  de aa para ab */
               chworksheet:range("AB164"):VALUE  = "=AB162/" + STRING(i-cont)

               /*chworksheet:range("AB164"):VALUE  = "=AB162/" + STRING(i-cont) 30/01  de ab para z */
               chworksheet:range("Z164"):VALUE  = "=Z162/" + STRING(i-cont)

               chworksheet:range("AD164"):VALUE  = "=AD162/" + STRING(i-cont).
    END.
    ***********************/

   de-soma-extrato-secagem = 0.
   FOR EACH tt-extrato.
       ASSIGN de-soma-extrato-secagem = de-soma-extrato-secagem + tt-extrato.desc-secagem.
   END.
    
   /* Resumo */
   DISP
        "Entradas Ö Dep¢sito: " (val-saldo-ent - val-saldo-sai)
        "Compra de Dep¢sito: "  vl-entre
        "Transferàncias (+): "  (val-saldo-ent - val-saldo-sai) - val-saldo-tra
        "Transferàncias (-): "  val-saldo-tra
        "Devoluá∆o F°sica: "    val-saldo-dev
        "Bloqueios F°sicos: "   vl-bloqu
        "Saldo Ö Liquidar: "    (val-saldo-ent - val-saldo-sai) - vl-entre - val-saldo-tra - val-saldo-dev - 
                                vl-bloqu - de-soma-extrato-secagem
    WITH FRAME f-resumido.
                 

        /******
        /*chworksheet:range("C283"):VALUE = (val-saldo-ent - val-saldo-sai)*/
           chworksheet:range("C284"):VALUE = vl-entre
           chworksheet:range("C285"):VALUE = val-saldo-dev
           chworksheet:range("C286"):VALUE = de-soma-extrato-secagem /*de-a-secar 010304 */ /*07/07/2003 vl-aloca*/
           chworksheet:range("C287"):VALUE = val-saldo-tra
           chworksheet:range("C288"):VALUE = vl-bloqu.
           
    /* new resumo 08/10 */
    ASSIGN chworksheet:range("E283"):VALUE = (val-saldo-bon - val-saldo-fin)
           chworksheet:range("E286"):VALUE = de-soma-extrato-secagem /*de-a-secar 010304 */ /*07/07/2003 vl-aloca*/
           chworksheet:range("E288"):VALUE = vl-bloqu.
           
    /* 31/01/04 coloca valores de mÇdia dos contratos de umidade, inteiros, preto-verm, verde, manch/pic, rend-quebr e impureza */
    FOR EACH tt-soma-contrato.
    ASSIGN 
        chworksheet:range("EJ13"):VALUE = (tt-soma-contrato.umidade        / tt-soma-contrato.nr-contratos)
        chworksheet:range("EK13"):VALUE = (tt-soma-contrato.rend-inteiro   / tt-soma-contrato.nr-contratos)
        chworksheet:range("EL13"):VALUE = (tt-soma-contrato.preto-vermelho / tt-soma-contrato.nr-contratos)
        chworksheet:range("EM13"):VALUE = (tt-soma-contrato.verde          / tt-soma-contrato.nr-contratos)
        chworksheet:range("EN13"):VALUE = (tt-soma-contrato.manch-pic      / tt-soma-contrato.nr-contratos)
        chworksheet:range("EO13"):VALUE = (tt-soma-contrato.rend-quebr     / tt-soma-contrato.nr-contratos)
        chworksheet:range("EP13"):VALUE = (tt-soma-contrato.impureza       / tt-soma-contrato.nr-contratos).
           
    END. /* tt-soma contrato */



    /*

        DISP tt-extrato.dt-trans   
             tt-extrato.nr-ticket       
             tt-extrato.nr-contrato
             tt-extrato.nr-placa
             tt-extrato.nr-nota-fornec  
             tt-extrato.nro-docto  
             tt-extrato.esp-docto  
             tt-extrato.peso-fornec 
             tt-extrato.peso-liq    
             tt-extrato.valor-bruto

             tt-extrato.gr-umidade  
             tt-extrato.desc-umidade
             tt-extrato.gr-impureza  
             tt-extrato.desc-impureza
             
             tt-extrato.rend-inteiro   
             tt-extrato.desc-rendimento
             
             tt-extrato.rend-desc-inteiro
             tt-extrato.desc-inteiro     
             
             tt-extrato.gr-secagem     
             tt-extrato.desc-secagem   
             tt-extrato.gr-amarelo     
             tt-extrato.desc-amarelo   
             tt-extrato.gr-vermelho    
             tt-extrato.desc-vermelho  
             tt-extrato.gr-preto-verm  
             tt-extrato.desc-preto-verm
             tt-extrato.gr-verde       
             tt-extrato.desc-verde     
             tt-extrato.gr-manch-pic   
             tt-extrato.desc-manch-pic 
             tt-extrato.qtd-bonific
             tt-extrato.val-bonific
             tt-extrato.peso-total 
             tt-extrato.valor-total
             tt-extrato.rendimento 

             es-ticket.gr-umidade    WHEN AVAIL es-ticket
             es-ticket.gr-impureza   WHEN AVAIL es-ticket 
             es-ticket.gr-secagem    WHEN AVAIL es-ticket
             es-ticket.gr-amarelo    WHEN AVAIL es-ticket
             es-ticket.gr-vermelho   WHEN AVAIL es-ticket
             es-ticket.gr-preto-ver  WHEN AVAIL es-ticket
             es-ticket.gr-verde      WHEN AVAIL es-ticket
             es-ticket.gr-manch-pic  WHEN AVAIL es-ticket

    */
    ***/
END PROCEDURE.

PROCEDURE pi-resumo.



    FOR EACH es-movto-arroz WHERE
             es-movto-arroz.nr-contrato = es-contrato-arroz.nr-contrato 
             NO-LOCK BY es-movto-arroz.tipo-trans:
             
        
        IF es-movto-arroz.esp-docto = "BON" THEN ASSIGN val-saldo-bon = val-saldo-bon + es-movto-arroz.quantidade. 
        IF es-movto-arroz.esp-docto = "TRA" THEN ASSIGN val-saldo-tra = val-saldo-tra + es-movto-arroz.quantidade. 
        IF es-movto-arroz.esp-docto = "DEV" THEN ASSIGN val-saldo-dev = val-saldo-dev + es-movto-arroz.quantidade. 

        IF es-movto-arroz.tipo-trans = 1 THEN DO:
        
            IF  es-contrato-arroz.tipo-contrato = 1  AND
                es-movto-arroz.tp-desconto    <> "Compra Direta" THEN NEXT.
            
            IF es-contrato-arroz.tipo-contrato = 2  and
               es-movto-arroz.tp-desconto      <> "Deposito" THEN NEXT.
    
            IF es-contrato-arroz.tipo-contrato = 3 and
               es-movto-arroz.tp-desconto      <> "Importacao" THEN NEXT.
        
              ASSIGN val-saldo-ent = val-saldo-ent + es-movto-arroz.quantidade. 
    
        end. /* entradas */
             
    
        IF es-movto-arroz.tipo-trans = 2 THEN DO:
            FIND es-tipo-desconto WHERE
                 es-tipo-desconto.tp-desconto = es-movto-arroz.tp-desconto
                 NO-LOCK NO-ERROR.

            IF NOT AVAIL es-tipo-desconto THEN NEXT.
            IF (es-tipo-desconto.financeiro) THEN do:
                ASSIGN val-saldo-fin = val-saldo-fin + es-movto-arroz.quantidade. 
                NEXT.
            END.
            
            ASSIGN val-saldo-sai = val-saldo-sai + es-movto-arroz.quantidade. 
    
        END. /* saidas */
    
            
    END. /* for each movto */
    
    /* valor comprado */
    FOR EACH bf-emitente NO-LOCK
        WHERE bf-emitente.nome-matriz = emitente.nome-abrev /*c-fornec-ini*/
      ,EACH ordem-compra NO-LOCK
       WHERE ordem-compra.cod-emitente = bf-emitente.cod-emitente
         AND ordem-compra.situacao    <> 4
/*          AND ordem-compra.int-1        = es-contrato-arroz.nr-contrato */
      ,EACH prazo-compra NO-LOCK OF ordem-compra:

       IF ordem-compra.int-1 <>  es-contrato-arroz.nr-contrato THEN NEXT.
    
       ASSIGN vl-aloca = vl-aloca + prazo-compra.quant-saldo
              vl-entre = vl-entre + prazo-compra.quant-receb.
    END. /* comprado */
    
    /* bloqueado */
    FOR EACH es-saldo-arroz NO-LOCK
       WHERE es-saldo-arroz.nr-contrato  = es-contrato-arroz.nr-contrato
         AND es-saldo-arroz.cod-emitente >= tt-param.cod-emitente-ini
         AND es-saldo-arroz.cod-emitente <= tt-param.cod-emitente-fim:

        /*AND es-saldo-arroz.ins-estadual >= c-produ-ini
         AND es-saldo-arroz.ins-estadual <= c-produ-fim:*/

    
       ASSIGN vl-saldo = vl-saldo + es-saldo-arroz.qtidade-atu
              vl-bloqu = vl-bloqu + es-saldo-arroz.qt-bloqueada.
    END.

    ASSIGN i-nr-contrato = tt-param.nr-contrato-ini.

    RUN esp/esapi007.p  (INPUT i-nr-contrato,
                         INPUT 0,
                         INPUT 99999999,
                         OUTPUT de-entra,
                         OUTPUT de-saida,
                         OUTPUT de-tot-saldo,
                         OUTPUT de-tot-secag,
                         OUTPUT de-tot-seco,
                         OUTPUT de-a-secar,
                         OUTPUT vl-tot-secag,
                         OUTPUT vl-tot-seco,
                         OUTPUT vl-a-secar).


END. /* pi-resumo */


PROCEDURE pi-quantidade :
  
    ASSIGN di-quantidade = 0.
      
  /*MESSAGE "no pi-quantidade" VIEW-AS ALERT-BOX.*/

  FOR EACH tt-detalhes:

      FIND es-tipo-desconto NO-LOCK
          WHERE es-tipo-desconto.tp-desconto = tt-detalhes.c-tp-desconto
          NO-ERROR.

   FIND FIRST es-param-estab NO-LOCK
          WHERE cod-estabel = es-ticket.cod-estabel.

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
  /*MESSAGE di-quantidade VIEW-AS ALERT-BOX.*/

END PROCEDURE.

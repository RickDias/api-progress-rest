/************************************************************************
** Programa: es4042rp.p - Saldo Poder Terceiros
** Autor   : Erika
** Data    : 23/02/04
** VersÆo  : 2.04.00.000
** Cliente : Camil Alimentos S/A
** set/2016 - SMF - Kraft - conversÆo campos totvs 12
************************************************************************/
{include/i-prgvrs.i ES4042RP 2.09.00.001 } /*** "019001" ***/
{include/i-bfems2cad.i}

/* ***************************  Definitions  ************************** */
&global-define programa nome-do-programa

    define temp-table tt-param no-undo
        field destino               as integer
        field arquivo               as char format "x(35)"
        field usuario               as char format "x(12)"
        field data-exec             as date
        field hora-exec             as integer
        field classifica            as INTEGER    
        FIELD fi-cod-estabel-ini    AS CHAR
        FIELD fi-cod-estabel-fin    AS CHAR
        FIELD fi-familia-ini        AS CHAR
        FIELD fi-familia-fin        AS CHAR
        FIELD fi-cod-emitente-ini   LIKE emitente.cod-emitente
        FIELD fi-cod-emitente-fin   LIKE emitente.cod-emitente
        FIELD fi-it-codigo-ini      LIKE est-prod-coml.it-codigo
        FIELD fi-it-codigo-fin      LIKE est-prod-coml.it-codigo
        FIELD to-sai-ben            AS LOGICAL
        FIELD to-ent-ben            AS LOGICAL
        FIELD to-transf             AS LOGICAL
        FIELD to-sai-con            AS LOGICAL
        FIELD to-ent-con            AS LOGICAL
        FIELD to-saldo              AS LOGICAL.

define temp-table tt-digita
    field ordem            as integer   format ">>>>9"
    field exemplo          as character format "x(30)"
    index id is primary unique
          ordem.

def temp-table tt-raw-digita
    field raw-digita as raw.

define temp-table tt-saldo
   FIELD cod-emitente    AS INTEGER 
   field nome-abrev      AS CHAR FORMAT "x(12)"
   FIELD valor-total     AS DEC FORMAT ">>,>>>,>>>,>>9.99"
   FIELD valor-unit      AS DEC FORMAT ">>,>>>,>>>,>>9.99"
   FIELD nome-emit       LIKE emitente.nome-emit
   FIELD tipo            AS INT
   FIELD cod-estabel     LIKE estabelec.cod-estabel
   FIELD it-codigo       LIKE ITEM.it-codigo
   FIELD desc-item       AS CHAR
   FIELD un              LIKE ITEM.un
   FIELD quantidade      AS DEC FORMAT ">>,>>>,>>9.9999"
   FIELD qtde-alocada    AS DEC FORMAT ">>,>>>,>>9.9999"
index emitente cod-estabel cod-emitente it-codigo tipo
INDEX ITEM cod-estabel it-codigo cod-emitente tipo.

DEF VAR c-tipo AS CHAR.
DEF VAR de-soma AS DEC.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.
def var h-acomp                as handle no-undo.    


DEFINE VARIABLE de-tot-estab        AS DECIMAL    NO-UNDO FORMAT "->,>>>,>>>,>>9.9999".


create tt-param.
raw-transfer raw-param to tt-param.

{include/i-rpvar.i}

/* ***************************  Main Block  *************************** */

do on stop undo, leave:

    find first param-global no-lock no-error.
                    
    find empresa
        where empresa.ep-codigo = param-global.empresa-prin
        no-lock no-error.
    
    if available param-global then assign c-empresa = param-global.grupo.
    
    assign
      c-programa    = "ES4042RP"
      c-versao      = "2.04"
      c-revisao     = "000".
    
    assign 
      c-titulo-relat = "Saldo Poder Terceiros"
      c-sistema      = "Especifico".

    {include/i-rpcab.i}
    
    {include/i-rpout.i}

    view frame f-cabec.
    view frame f-rodape.

    run utp/ut-acomp.p persistent set h-acomp.  
                           
    run pi-inicializar in h-acomp (input ""). 
    
    FOR EACH saldo-terc NO-LOCK
            WHERE saldo-terc.cod-estabel  >= tt-param.fi-cod-estabel-ini
              AND saldo-terc.cod-estabel  <= tt-param.fi-cod-estabel-fin
              AND saldo-terc.cod-emitente >= tt-param.fi-cod-emitente-ini
              AND saldo-terc.cod-emitente <= tt-param.fi-cod-emitente-fin
              AND saldo-terc.it-codigo    >= tt-param.fi-it-codigo-ini 
              AND saldo-terc.it-codigo    <= tt-param.fi-it-codigo-fin,
            EACH ITEM WHERE ITEM.it-codigo = saldo-terc.it-codigo 
                        AND ITEM.fm-codigo >= tt-param.fi-familia-ini 
                        AND item.fm-codigo <= tt-param.fi-familia-fin
            BY saldo-terc.cod-estabel
            BY saldo-terc.cod-emitente
            BY saldo-terc.it-codigo:
                                                                                               
            IF not(tt-param.to-sai-ben) AND saldo-terc.tipo-sal-terc = 1 THEN NEXT.
            IF not(tt-param.to-ent-ben) AND saldo-terc.tipo-sal-terc = 2 THEN NEXT.
            IF not(tt-param.to-transf)  AND saldo-terc.tipo-sal-terc = 3 THEN NEXT.
            IF not(tt-param.to-sai-con) AND saldo-terc.tipo-sal-terc = 4 THEN NEXT.
            IF not(tt-param.to-ent-con) AND saldo-terc.tipo-sal-terc = 5 THEN NEXT.

            /* 2355743 */
            FIND FIRST ext-natur-oper-cfop NO-LOCK
                 WHERE ext-natur-oper-cfop.nat-operacao = saldo-terc.nat-operacao NO-ERROR.
            
            IF tt-param.to-saldo AND
              not(saldo-terc.nat-operacao = "1949oo" or
                  saldo-terc.nat-operacao = "5949oo" OR
                  (AVAIL ext-natur-oper-cfop AND ext-natur-oper-cfop.entrada-deposito = YES) OR
                  (AVAIL ext-natur-oper-cfop AND ext-natur-oper-cfop.entrada-deposito-pj = YES)) THEN NEXT. /* 06/03/04 */

            RUN pi-acompanhar IN h-acomp (INPUT "Saldo Poder Terceiros").           
            
            FIND FIRST tt-saldo WHERE
                       tt-saldo.cod-estabel  = saldo-terc.cod-estabel AND
                       tt-saldo.cod-emitente = saldo-terc.cod-emitente AND
                       tt-saldo.it-codigo    = saldo-terc.it-codigo AND
                       tt-saldo.tipo         = INT(tt-saldo.tipo)
                EXCLUSIVE-LOCK USE-INDEX emitente NO-ERROR.

            IF NOT AVAIL tt-saldo THEN do:
                FIND FIRST emitente WHERE
                           emitente.cod-emitente = saldo-terc.cod-emitente
                    NO-LOCK NO-ERROR.

                CREATE tt-saldo.
                ASSIGN tt-saldo.cod-emitente = saldo-terc.cod-emitente
                       tt-saldo.cod-estabel  = saldo-terc.cod-estabel
                       tt-saldo.it-codigo    = saldo-terc.it-codigo
                       tt-saldo.nome-abrev   = emitente.nome-abrev
                       tt-saldo.nome-emit    = emitente.nome-emit
                       tt-saldo.desc-item    = ITEM.desc-item
                       tt-saldo.un           = ITEM.un
                       tt-saldo.tipo         = int(saldo-terc.tipo-sal-terc).
            END. /* not avail */

            
            ASSIGN tt-saldo.quantidade             = tt-saldo.quantidade + saldo-terc.quantidade
                   tt-saldo.valor-unit             = tt-saldo.valor-unit + (saldo-terc.valor-mat-m[1] + saldo-terc.valor-mob-m[1] + saldo-terc.valor-ggf-m[1])
                   tt-saldo.valor-total            = tt-saldo.valor-total + 
                                                      ((saldo-terc.valor-mat-m[1] + saldo-terc.valor-mob-m[1] + saldo-terc.valor-ggf-m[1]) * saldo-terc.quantidade)
                   tt-saldo.qtde-alocada           = tt-saldo.qtde-alocada + saldo-terc.dec-1.

            
          
    END.  /* for each */


    /* IMPRESSÇO DA PµGINA DE PAR¶METROS 
    PAGE.
    PUT "SELE€ÇO: " SKIP
        "Per¡odo        : " tt-param.periodo-ini "         |< >| " tt-param.periodo-fim SKIP
        "Repres Indireto: " tt-param.rep-indireto-ini "            |< >| " tt-param.rep-indireto-fim SKIP
        "Representante  : " tt-param.qtde-alocada-ini "            |< >| " tt-param.qtde-alocada-fim SKIP
        "Produto Coml   : " tt-param.it-codigouto-coml-ini " |< >| " tt-param.it-codigouto-coml-fim SKIP(1)
        "CLASSIFICA€ÇO:" SKIP
        tt-param.desc-classif SKIP
        "PAR¶METROS:" SKIP
        "TIPO: " IF tt-param.tipo-relat = 1 THEN "DETALHADO" ELSE "RESUMIDO" SKIP
        "Imprime log: " tt-param.imprime-log FORMAT "Sim/NÆo" SKIP
        "Imprime Separador: " tt-param.imprime-separador FORMAT "Sim/NÆo".*/

    
        
    IF tt-param.classific = 1 THEN DO:

            FOR EACH tt-saldo NO-LOCK
               BREAK BY tt-saldo.cod-estabel
                     BY tt-saldo.it-codigo
                     BY tt-saldo.cod-emitente
                     BY tt-saldo.tipo:
                IF tt-saldo.quantidade = 0 THEN NEXT.

                IF FIRST-OF(tt-saldo.cod-estabel) AND
                   FIRST-OF(tt-saldo.tipo) THEN DO:
                  FIND FIRST estabelec where
                      estabelec.cod-estabel = tt-saldo.cod-estabel
                      NO-LOCK no-error.
                  PUT SKIP(3) "Estabelecimento: " tt-saldo.cod-estabel " - " estabelec.nome SKIP(2).
                END.

                IF FIRST-OF(tt-saldo.it-codigo) THEN DO:
                  PUT SKIP(2) "Item: " tt-saldo.it-codigo " - " tt-saldo.desc-item " - " tt-saldo.un SKIP(2).
                END.

                IF tt-saldo.tipo = 1 THEN c-tipo = "Sa¡da Beneficiamento".
                IF tt-saldo.tipo = 2 THEN c-tipo = "Entrada Beneficiamento".
                IF tt-saldo.tipo = 3 THEN c-tipo = "Transferˆncia".
                IF tt-saldo.tipo = 4 THEN c-tipo = "Sa¡da Consigna‡Æo".
                IF tt-saldo.tipo = 5 THEN c-tipo = "Entrada Consigna‡Æo".

                DISPLAY tt-saldo.cod-emitente       LABEL "Emitente"
                        tt-saldo.nome-abrev         LABEL "Nome Abrev"
                        tt-saldo.nome-emit          LABEL "Nome Emit"
                        tt-saldo.quantidade         LABEL "Quantidade"
                        tt-saldo.qtde-alocada       LABEL "Qtde Alocada"
                        tt-saldo.valor-unit         LABEL "Valor Unit rio"
                        tt-saldo.valor-total        LABEL "Valor Total"        
                        c-TIPO                      LABEL "Tipo Saldo" FORMAT "X(25)"
                    WITH WIDTH 300 STREAM-IO.
                 
                ASSIGN de-soma = de-soma + tt-saldo.valor-total.
                

                IF LAST-OF(tt-saldo.cod-estabel) and
                   LAST-OF(tt-saldo.tipo) THEN DO:
                  PUT SKIP(3) "Total do Estabelecimento: " AT 90 de-soma 
                      FORMAT ">>,>>>,>>>,>>9.99" " - " c-tipo  FORMAT "X(25)"   SKIP(2).
                  ASSIGN de-soma = 0.
                END.                

                
            END. /* for each */
        
    END. /* class 1 */


    IF tt-param.classific = 2 THEN DO:
        
        FOR EACH tt-saldo NO-LOCK
           BREAK BY tt-saldo.cod-estabel                 
                 BY tt-saldo.cod-emitente
                 BY tt-saldo.it-codigo
                 BY tt-saldo.tipo:

            IF FIRST-OF(tt-saldo.cod-estabel) AND
               FIRST-OF(tt-saldo.tipo) THEN DO:
              FIND FIRST estabelec where
                  estabelec.cod-estabel = tt-saldo.cod-estabel
                  NO-LOCK no-error.
              PUT SKIP(3) "Estabelecimento: " tt-saldo.cod-estabel " - " estabelec.nome SKIP(2).
            END.

            IF FIRST-OF(tt-saldo.cod-emitente) THEN DO:
              PUT SKIP(2) "Emitente: " tt-saldo.cod-emitente " - " tt-saldo.nome-abrev " - " tt-saldo.nome-emit SKIP(2).
            END.

            IF tt-saldo.tipo = 1 THEN c-tipo = "Sa¡da Beneficiamento".
            IF tt-saldo.tipo = 2 THEN c-tipo = "Entrada Beneficiamento".
            IF tt-saldo.tipo = 3 THEN c-tipo = "Transferˆncia".
            IF tt-saldo.tipo = 4 THEN c-tipo = "Sa¡da Consigna‡Æo".
            IF tt-saldo.tipo = 5 THEN c-tipo = "Entrada Consigna‡Æo".

            DISPLAY tt-saldo.it-codigo          LABEL "Item"
                    tt-saldo.desc-item          LABEL "Descri‡Æo"
                    tt-saldo.quantidade         LABEL "Quantidade"
                    tt-saldo.qtde-alocada       LABEL "Qtde Alocada"
                    tt-saldo.valor-unit         LABEL "Valor Unit rio"
                    tt-saldo.valor-total        LABEL "Valor Total"        
                    c-TIPO                      LABEL "Tipo Saldo" FORMAT "X(25)"
                WITH WIDTH 300 STREAM-IO.

            ASSIGN de-soma = de-soma + tt-saldo.valor-total.


            IF LAST-OF(tt-saldo.cod-estabel) and
               LAST-OF(tt-saldo.tipo) THEN DO:
              PUT SKIP(3) "Total do Estabelecimento: " AT 59 de-soma " - " c-tipo FORMAT "X(25)"  SKIP(2).
              ASSIGN de-soma = 0.
            END.                
        END. /* for each */
    END. /* clas 2 */
        
    
    
    
    RUN pi-finalizar IN h-acomp.
    {include/i-rpclo.i}
END.

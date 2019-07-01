&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
** set/2016 - SMF - Kraft - conversÆo campos totvs 12
*******************************************************************************/
{include/i-prgvrs.i ES0086RP 2.09.00.001 } /*** "019001" ***/
{include/i-bfems2cad.i}

/* ***************************  Definitions  ************************** */
&global-define programa nome-do-programa


define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    FIELD c-ini-dttrans    AS DATE
    FIELD c-fim-dtTrans    AS DATE.

define temp-table tt-digita
    field ordem            as integer   format ">>>>9"
    field exemplo          as character format "x(30)"
    index id is primary unique
        ordem.

def temp-table tt-raw-digita
    field raw-digita as raw.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

def var h-acomp         as handle no-undo.    

DEFINE VARIABLE l-cfop AS LOGICAL NO-UNDO.

/* Frame de Cabe‡alho */           
def frame f-cabec1 header
    "Emitente"   at 01
    "Item"       at 28
    "Dt.EmissÆo" at 43
    "Seq"        at 53
    "Serie"      at 63
    "Nr.Docto"   at 71
    "Nat.Oper"   at 81
    "Quantidade" at 91
    "Saldo"      at 101
    "Valor"      at 114
    "O.P."
    fill("-", 132) format "x(132)" skip
    with width 132 page-top  no-labels no-box stream-io.
       
 
create tt-param.
raw-transfer raw-param to tt-param.

{include/i-rpvar.i}

   

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 1.99
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME
 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Procedure 
/* ************************* Included-Libraries *********************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

do on stop undo, leave:

    find first param-global no-lock no-error.
    
    find empresa
        where empresa.ep-codigo = param-global.empresa-prin
        no-lock no-error.
    
    if available param-global then assign c-empresa = param-global.grupo.
    
    assign
      c-programa    = "ES0086RP"
      c-versao      = "2.04"
      c-revisao     = "000".
    
    assign 
      c-titulo-relat = "Zeramento das NFs de Arroz a Deposito"
      c-sistema      = "Especifico".
    
    
    
    {include/i-rpcab.i}
    
    {include/i-rpout.i}
    
    
    view frame f-cabec.
    view frame f-rodape.

    run utp/ut-acomp.p persistent set h-acomp.  
    
  
    run pi-inicializar in h-acomp (input ""). 
    
    /* 2355743 */
    /*FOR EACH movto-estoq 
       WHERE movto-estoq.dt-trans >= tt-param.c-ini-dtTrans
         AND movto-estoq.dt-trans <= tt-param.c-fim-dtTrans
         AND (movto-estoq.nat-oper = "1949oo"
          OR  movto-estoq.nat-oper = "5949dc"         
          OR  movto-estoq.nat-oper = "1949MT"
          OR  movto-estoq.nat-oper = "5949AF"
          OR  movto-estoq.nat-oper = "5949AS"
          OR  movto-estoq.nat-oper = "5949MT"
          OR  movto-estoq.nat-oper = "5949PJ"
          OR  movto-estoq.nat-oper = "5949SB"
          OR  movto-estoq.nat-oper = "1949SB"):*/

    FOR EACH movto-estoq 
       WHERE movto-estoq.dt-trans >= tt-param.c-ini-dtTrans
         AND movto-estoq.dt-trans <= tt-param.c-fim-dtTrans:

        ASSIGN l-cfop = NO.

        IF movto-estoq.nat-oper = "1949oo"
        OR movto-estoq.nat-oper = "5949dc"         
        OR movto-estoq.nat-oper = "1949MT"
        OR movto-estoq.nat-oper = "5949AF"
        OR movto-estoq.nat-oper = "5949AS"
        OR movto-estoq.nat-oper = "5949MT"
        OR movto-estoq.nat-oper = "5949PJ"
        OR movto-estoq.nat-oper = "5949SB"
        OR movto-estoq.nat-oper = "1949SB" THEN
            ASSIGN l-cfop = YES.

        IF NOT l-cfop THEN DO:

            FIND FIRST ext-natur-oper-cfop NO-LOCK
                 WHERE ext-natur-oper-cfop.nat-operacao = movto-estoq.nat-operacao NO-ERROR.

            IF AVAIL ext-natur-oper-cfop
                AND (ext-natur-oper-cfop.entrada-deposito
                 OR  ext-natur-oper-cfop.entrada-deposito-pj
                 OR  ext-natur-oper-cfop.dev-liq-deposito-pf
                 OR  ext-natur-oper-cfop.mudanca-titularidade
                 OR  ext-natur-oper-cfop.mudanca-fisica
                 OR  ext-natur-oper-cfop.dev-cob-secagem
                 OR  ext-natur-oper-cfop.dev-mud-titularidade
                 OR  ext-natur-oper-cfop.devolucao-fiscal
                 OR  ext-natur-oper-cfop.dev-liq-deposito-pj) THEN
                ASSIGN l-cfop = YES.
        END.

        IF NOT l-cfop THEN
            NEXT.

        /* Antes de zerar os valores, estes sÆo guardados nos campos abaixos */

        ASSIGN movto-estoq.dec-1 = movto-estoq.valor-mat-m[1]
               movto-estoq.dec-2 = movto-estoq.valor-ggf-m[1].

        /* Zerar os valores */

        ASSIGN movto-estoq.valor-mat-m[1] = 0
               movto-estoq.valor-mob-m[1] = 0
               movto-estoq.valor-ggf-m[1] = 0
               movto-estoq.valor-mat-m[2] = 0
               movto-estoq.valor-mob-m[2] = 0
               movto-estoq.valor-ggf-m[2] = 0.

       DISP
             movto-estoq.it-codigo
             movto-estoq.serie
             movto-estoq.nro-docto
             movto-estoq.cod-estabel
/*              movto-estoq.cod-depos  */
/*              movto-estoq.esp-docto  */
/*              movto-estoq.cod-local  */
             movto-estoq.dt-trans
             movto-estoq.quantidade 
             movto-estoq.lote
             movto-estoq.dt-trans
             movto-estoq.valor-mat-m[1](TOTAL)
             movto-estoq.valor-mob-m[1](TOTAL)
             movto-estoq.valor-ggf-m[1](TOTAL)
             movto-estoq.valor-mat-m[2](TOTAL)
             movto-estoq.valor-mob-m[2](TOTAL)
             movto-estoq.valor-ggf-m[2](TOTAL)
             movto-estoq.dec-1(TOTAL)
             movto-estoq.dec-2(TOTAL) WITH WIDTH 500.                     

    END.


    run pi-finalizar in h-acomp.
    {include/i-rpclo.i}
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



/******************************************************************************
** EMPRESA  : CAMIL
** PROGRAMA : es4068RP
** DESCRICAO: EXTRATO CONTA CORRENTE (EXCEL)
** DATA     : NOVEMBRO DE 2006
** VERSAO   : NOVO API010, NOVA L‡GICA-PERFORMANCE (erika/planilha) - novas pastas
**            de liquidaá∆o, transferàncias, bloq/desbloqueios e devoluá‰es
**           ECCB- 18/06/2010 - SENAR 
**           ECCB- Joca/Waldemar tratamento EFD - PIS/COFINS - 28/03/2011 
**           ECCB - BB 23/02/2015
** set/2016 - SMF - Kraft - convers∆o campos totvs 12
*******************************************************************************/
{utp/ut-glob.i}
{include/i-prgvrs.i ES4068RP 2.09.00.001 } /*** "019001" ***/
{include/i-bfems2cad.i}

/*------------- Definicao de Temp-Table e vars --------------------------------------*/

define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    field classifica       as integer
    field desc-classifica  as char format "x(40)"
    FIELD cod-estab-ini    LIKE es-movto-arroz.cod-estabel    
    FIELD cod-estab-fim    LIKE es-movto-arroz.cod-estabel   
    FIELD nr-contrato-ini  LIKE es-movto-arroz.nr-contrato
    FIELD nr-contrato-fim  LIKE es-movto-arroz.nr-contrato
    FIELD cod-emitente-ini LIKE es-movto-arroz.cod-emitente
    FIELD cod-emitente-fim LIKE es-movto-arroz.cod-emitente
    FIELD corretor-ini     LIKE es-movto-arroz.nr-contrato
    FIELD medias           AS LOG
    FIELD l-compra         AS LOGICAL
    FIELD l-deposito       AS LOGICAL
    FIELD l-importacao     AS LOGICAL 
    FIELD l-tr-tit         AS LOGICAL
    FIELD l-arm-ent        AS LOGICAL
    FIELD l-item           AS LOGICAL.


    DEF TEMP-TABLE tt-extrato NO-UNDO
        FIELD ins-estadual      LIKE es-movto-arroz.ins-estadual
        FIELD cod-estabel       LIKE es-movto-arroz.cod-estabel
        FIELD cod-emitente      LIKE emitente.cod-emitente
        FIELD nr-contrato       LIKE es-contrato-arroz.nr-contrato
        FIELD nr-ticket         LIKE es-ticket.nr-ticket
        FIELD nro-docto         LIKE es-movto-arroz.nro-docto
        FIELD serie             LIKE es-movto-arroz.serie
        FIELD esp-docto         LIKE es-movto-arroz.esp-docto
        FIELD dt-trans          LIKE es-movto-arroz.dt-trans   
        FIELD gr-secagem        LIKE es-ticket.gr-secagem
        FIELD desc-secagem      LIKE es-movto-arroz.quantidade
        FIELD peso-liq          AS DEC FORMAT "->>,>>>,>>9.99" 
        FIELD desc-impureza     LIKE es-movto-arroz.quantidade 
        FIELD qtd-bonific       LIKE es-movto-arroz.quantidade 
        FIELD desc-umidade      LIKE es-movto-arroz.quantidade 
        FIELD gr-umidade        LIKE es-ticket.gr-umidade   
        FIELD gr-impureza       LIKE es-ticket.gr-impureza  
        FIELD rend-inteiro      LIKE es-ticket.rend-inteiro 
        FIELD rend-quebrado     LIKE es-ticket.rend-quebr   
        FIELD gr-verde          LIKE es-ticket.gr-verde   
        FIELD desc-verde        LIKE es-movto-arroz.quantidade 
        FIELD gr-barriga        LIKE es-ticket.gr-barriga
        FIELD desc-barriga      LIKE es-movto-arroz.quantidade 
        FIELD desc-gesso      LIKE es-movto-arroz.quantidade 
        FIELD gr-gesso        LIKE es-ticket.gr-gesso
        FIELD desc-amarelo      LIKE es-movto-arroz.quantidade 
        FIELD gr-amarelo        LIKE es-ticket.gr-amarelo   
        FIELD desc-vermelho     LIKE es-movto-arroz.quantidade  
        FIELD gr-vermelho       LIKE es-ticket.gr-vermelho 
        FIELD desc-preto-verm   LIKE es-movto-arroz.quantidade
        FIELD gr-preto-verm     LIKE es-ticket.gr-preto-verm  
        FIELD desc-manch-pic    LIKE es-movto-arroz.quantidade
        FIELD gr-manch-pic      LIKE es-ticket.gr-manch-pic
        FIELD desc-descascado   LIKE es-movto-arroz.quantidade 
        FIELD gr-descascado     LIKE es-ticket.gr-manch-pic 
        FIELD nr-placa          LIKE es-ticket.nr-placa-cam
        FIELD nr-nota-fornec    LIKE es-movto-arroz.nro-docto
        FIELD peso-fornec       LIKE es-movto-arroz.quantidade 
        FIELD desc-inteiro      LIKE es-movto-arroz.quantidade 
        FIELD desc-rendimento   LIKE es-movto-arroz.quantidade 
        FIELD valor-unit        LIKE es-movto-arroz.quantidade 
        FIELD valor-total       LIKE es-movto-arroz.quantidade 
        FIELD tp-desconto       LIKE es-movto-arroz.tp-desconto
        FIELD tipo-trans        LIKE es-movto-arroz.tipo-trans
        FIELD rec-extrato       AS RECID
        INDEX codigo IS PRIMARY UNIQUE
              ins-estadual
              cod-estabel
              cod-emitente
              nr-contrato
              nr-ticket
              esp-docto
              nro-docto
              serie.

    DEF temp-table tt-est-ins
            field cod-estabel         like es-saldo-arroz.cod-estabel 
            field cod-emitente        like es-saldo-arroz.cod-emitente
            field ins-estadual        like es-saldo-arroz.ins-estadual
            FIELD nr-contrato         LIKE es-contrato-arroz.nr-contrato
            FIELD dt-impl             LIKE es-contrato-arroz.dt-contrato
            field de-tot-secag-0001   AS DEC DECIMALS 3
            field de-tot-seco-0001    AS DEC DECIMALS 3
            field de-a-secar-0001     AS DEC DECIMALS 3
            field de-tot-secag-0002   AS DEC DECIMALS 3
            field de-tot-seco-0002    AS DEC DECIMALS 3
            field de-a-secar-0002     AS DEC DECIMALS 3
            field de-tot-secag-0004   AS DEC DECIMALS 3
            field de-tot-seco-0004    AS DEC DECIMALS 3
            field de-a-secar-0004     AS DEC DECIMALS 3
            field de-tot-secag-0007   AS DEC DECIMALS 3
            field de-tot-seco-0007    AS DEC DECIMALS 3
            field de-a-secar-0007     AS DEC DECIMALS 3
            field de-tot-secag-0015   AS DEC DECIMALS 3
            field de-tot-seco-0015    AS DEC DECIMALS 3
            field de-a-secar-0015     AS DEC DECIMALS 3
            field de-tot-secag-0020   AS DEC DECIMALS 3
            field de-tot-seco-0020    AS DEC DECIMALS 3
            field de-a-secar-0020     AS DEC DECIMALS 3
            field de-tot-secag-0036   AS DEC DECIMALS 3
            field de-tot-seco-0036    AS DEC DECIMALS 3
            field de-a-secar-0036     AS DEC DECIMALS 3
            field de-tot-secag-0040   AS DEC DECIMALS 3
            field de-tot-seco-0040    AS DEC DECIMALS 3
            field de-a-secar-0040     AS DEC DECIMALS 3
            field de-qt-001           AS DECIMAL    
            field de-qt-002           AS DECIMAL    
            field de-qt-040           AS DECIMAL    
            field de-qt-004           AS DECIMAL    
            field de-qt-007           AS DECIMAL    
            field de-qt-015           AS DECIMAL    
            field de-qt-020           AS DECIMAL    
            field de-qt-036           AS DECIMAL        
            field de-qt-001Entra      AS DECIMAL    
            field de-qt-002Entra      AS DECIMAL    
            field de-qt-040Entra      AS DECIMAL    
            field de-qt-004Entra      AS DECIMAL    
            field de-qt-007Entra      AS DECIMAL    
            field de-qt-015Entra      AS DECIMAL    
            field de-qt-020Entra      AS DECIMAL    
            field de-qt-036Entra      AS DECIMAL            
            field de-qt-001sai        AS DECIMAL    
            field de-qt-002sai        AS DECIMAL    
            field de-qt-040sai        AS DECIMAL    
            field de-qt-004sai        AS DECIMAL    
            field de-qt-007sai        AS DECIMAL    
            field de-qt-015sai        AS DECIMAL    
            field de-qt-020sai        AS DECIMAL    
            field de-qt-036sai        AS DECIMAL            
            field de-qt-001Entre      AS DECIMAL    
            field de-qt-002Entre      AS DECIMAL    
            field de-qt-040Entre      AS DECIMAL    
            field de-qt-004Entre      AS DECIMAL    
            field de-qt-007Entre      AS DECIMAL    
            field de-qt-015Entre      AS DECIMAL    
            field de-qt-020Entre      AS DECIMAL    
            field de-qt-036Entre      AS DECIMAL            
            field de-qt-001aloca      AS DECIMAL    
            field de-qt-002aloca      AS DECIMAL    
            field de-qt-040aloca      AS DECIMAL    
            field de-qt-004aloca      AS DECIMAL    
            field de-qt-007aloca      AS DECIMAL    
            field de-qt-015aloca      AS DECIMAL    
            field de-qt-020aloca      AS DECIMAL    
            field de-qt-036aloca      AS DECIMAL 
            field de-qt-001bloq       AS DECIMAL
            field de-qt-002bloq       AS DECIMAL
            field de-qt-040bloq       AS DECIMAL
            field de-qt-004bloq       AS DECIMAL
            field de-qt-007bloq       AS DECIMAL
            field de-qt-015bloq       AS DECIMAL
            field de-qt-020bloq       AS DECIMAL
            field de-qt-036bloq       AS DECIMAL
            field de-qt-001dev        AS DECIMAL    
            field de-qt-002dev        AS DECIMAL    
            field de-qt-040dev        AS DECIMAL    
            field de-qt-004dev        AS DECIMAL    
            field de-qt-007dev        AS DECIMAL    
            field de-qt-015dev        AS DECIMAL    
            field de-qt-020dev        AS DECIMAL    
            field de-qt-036dev        AS DECIMAL        
            field de-qt-001tra        AS DECIMAL    
            field de-qt-002tra        AS DECIMAL    
            field de-qt-040tra        AS DECIMAL    
            field de-qt-004tra        AS DECIMAL    
            field de-qt-007tra        AS DECIMAL    
            field de-qt-015tra        AS DECIMAL    
            field de-qt-020tra        AS DECIMAL    
            field de-qt-036tra        AS DECIMAL
            field de-qt-001secag      AS DECIMAL    
            field de-qt-002secag      AS DECIMAL    
            field de-qt-040secag      AS DECIMAL    
            field de-qt-004secag      AS DECIMAL    
            field de-qt-007secag      AS DECIMAL    
            field de-qt-015secag      AS DECIMAL    
            field de-qt-020secag      AS DECIMAL    
            field de-qt-036secag      AS DECIMAL
            field de-tot-saldo-0001   AS DEC DECIMALS 3
            field de-tot-saldo-0002   AS DEC DECIMALS 3 
            field de-tot-saldo-0004   AS DEC DECIMALS 3 
            field de-tot-saldo-0007   AS DEC DECIMALS 3 
            field de-tot-saldo-0015   AS DEC DECIMALS 3 
            field de-tot-saldo-0020   AS DEC DECIMALS 3 
            field de-tot-saldo-0036   AS DEC DECIMALS 3 
            field de-tot-saldo-0040   AS DEC DECIMALS 3 
            FIELD vl-tot-secag        AS DEC DECIMALS 3
            FIELD vl-tot-seco         AS DEC DECIMALS 3
            FIELD vl-a-secar          AS DEC DECIMALS 3

            FIELD de-qt-entra         AS DEC DECIMALS 3
            FIELD de-qt-sai           AS DEC DECIMALS 3
            FIELD de-qt-tra           AS DEC DECIMALS 3
            FIELD de-qt-dev           AS DEC DECIMALS 3
            FIELD de-qt-entre         AS DEC DECIMALS 3
            FIELD de-qt-aloca         AS DEC DECIMALS 3
            FIELD de-qt-secag         AS DEC DECIMALS 3
            FIELD de-qt-secar         AS DEC DECIMALS 3
            FIELD de-qt-seco          AS DEC DECIMALS 3
            field de-qt-bloq          AS DEC DECIMALS 3
            FIELD de-dsc-financ       AS DEC DECIMALS 3
            FIELD de-bon-financ       AS DEC DECIMALS 3

            FIELD de-total            AS DEC DECIMALS 3 
            FIELD de-sec              AS DEC DECIMALS 3 
            FIELD de-liq              AS DEC DECIMALS 3 
            field de-livre            AS DEC DECIMALS 3 
            INDEX prin AS PRIMARY nr-contrato ins-estadual cod-estabel cod-emitente
            INDEX sec nr-contrato. 

        define temp-table tt-soma-contrato no-undo
            FIELD umidade        LIKE es-contrato-arroz.gr-umidade
            FIELD rend-inteiro   LIKE es-contrato-arroz.rend-inteiro
            FIELD preto-vermelho LIKE es-contrato-arroz.u-dec-3
            FIELD verde          LIKE es-contrato-arroz.u-dec-5
            FIELD barriga        LIKE es-contrato-arroz.gr-barriga
            FIELD gesso        LIKE es-contrato-arroz.gr-gesso
            FIELD manch-pic      LIKE es-contrato-arroz.u-dec-4
            FIELD impureza       LIKE es-contrato-arroz.u-dec-4
            FIELD rend-quebr       LIKE es-contrato-arroz.rend-quebr
            FIELD nr-contratos   AS INT.

        def temp-table tt-detalhes-sec
            field desc-secagem AS DEC
            field dt-trans    as date
            field esp-docto   LIKE es-movto-arroz.esp-docto.

        def temp-table tt-aplic-desc-bon
            field nr-ticket like es-aplic-desc-bon.nr-ticket 
            field qt-bon    like es-aplic-desc-bon.qt-bon 
            field qt-desc   like es-aplic-desc-bon.qt-desc.
            
        def temp-table tt-aplic-sec-desc-bon
            field nr-ticket like es-aplic-desc-bon.nr-ticket 
            field qt-bon    like es-aplic-desc-bon.qt-bon 
            field qt-desc   like es-aplic-desc-bon.qt-desc.

        /* 230107 */
        DEF TEMP-TABLE tt-imp
            FIELD nr-placa         LIKE tt-extrato.nr-placa
            FIELD dt-trans         LIKE tt-extrato.dt-trans
            FIELD serie            LIKE tt-extrato.serie
            FIELD cod-estabel      LIKE tt-extrato.cod-estabel
            FIELD esp-docto        like tt-extrato.esp-docto       
            FIELD nr-ticket        like tt-extrato.nr-ticket       
            FIELD nr-nota-fornec   like tt-extrato.nr-nota-fornec  
            FIELD nro-docto        like tt-extrato.nro-docto       
            FIELD peso-fornec      like tt-extrato.peso-fornec     
            FIELD peso-liq         like tt-extrato.peso-liq        
            FIELD gr-impureza      like tt-extrato.gr-impureza     
            FIELD desc-impureza    like tt-extrato.desc-impureza   
            FIELD gr-umidade       like tt-extrato.gr-umidade      
            FIELD desc-umidade     like tt-extrato.desc-umidade    
            FIELD rend-inteiro     like tt-extrato.rend-inteiro    
            FIELD desc-inteiro     like tt-extrato.desc-inteiro    
            FIELD rend-quebrado    like tt-extrato.rend-quebrado     
            FIELD desc-rendimento  like tt-extrato.desc-rendimento 
            FIELD gr-verde         like tt-extrato.gr-verde        
            FIELD desc-verde       like tt-extrato.desc-verde      
            FIELD gr-barriga       like tt-extrato.gr-barriga
            FIELD desc-barriga     like tt-extrato.desc-barriga    
            FIELD gr-gesso       like tt-extrato.gr-gesso
            FIELD desc-gesso     like tt-extrato.desc-gesso    
            FIELD gr-manch-pic     like tt-extrato.gr-manch-pic    
            FIELD desc-manch-pic   like tt-extrato.desc-manch-pic  
            FIELD gr-descascado    like tt-extrato.gr-descascado   
            FIELD desc-descascado  like tt-extrato.desc-descascado 
            FIELD gr-secagem       like tt-extrato.gr-secagem      
            FIELD gr-preto-verm    like tt-extrato.gr-preto-verm    
            FIELD desc-preto-verm  like tt-extrato.desc-preto-verm
            FIELD gr-vermelho      like tt-extrato.gr-vermelho     
            FIELD desc-vermelho    like tt-extrato.desc-vermelho   
            FIELD valor-unit       like tt-extrato.valor-unit      
            FIELD valor-total      like tt-extrato.valor-total
            FIELD qtd-bonific      LIKE tt-extrato.qtd-bonific
            FIELD desc-secagem     LIKE tt-extrato.desc-secagem
            INDEX nr-ticket nr-ticket.

        def var de-val-bon             as dec.
        def var de-val-desc            as dec.
        def var de-sec-val-bon         as dec.
        def var de-sec-val-desc        as dec.
        DEF VAR de-bloq-desc           AS DEC.
        DEF VAR de-bloq-bon            AS DEC.
        DEF VAR de-soma-ent-compra-dir AS DEC NO-UNDO FORMAT "->>,>>>,>>9.99".
        DEF VAR l-aponta-tra           AS LOG INITIAL NO. 
        DEF VAR c-tra-nfd              AS CHAR.

/*         DEF VAR l-data AS LOGICAL INITIAL NO. */
        
        Define Temp-Table tt-raw-digita Field raw-digita As Raw.
        /*-------------- Definicao Parametros -----------------------------------*/
        Define Input Parameter raw-param As Raw No-Undo.
        Define Input Parameter Table For tt-raw-digita.
        
        /*--------------- Definicao de Variaveis --------------------------------*/
        Define Variable h-acomp As Handle No-Undo.

        DEFINE VARIABLE wh-pesquisa AS HANDLE     NO-UNDO.
        DEF VAR c-rend-medio-int    AS CHAR.
        DEF VAR c-rend-medio-que    AS CHAR.
        DEF VAR c-rend-medio        AS CHAR.
        DEF VAR de-qt-contr         AS DEC DECIMALS 3.

        DEF VAR fl-acum-geral       AS dec.
        DEF VAR fl-acum-saldo       AS DEC.  
        DEF VAR fl-acum-aloca       AS DEC.  
        DEF VAR fl-acum-bloqu       AS DEC.  
        DEF VAR fl-acum-contr       AS DEC.  
        DEF VAR fl-acum-entre       AS DEC.  
        DEF VAR de-acum-secagem     AS DEC.  
        DEF VAR de-acum-seco        AS DEC.  
        DEF VAR de-acum-asecar      AS DEC.  
        DEF VAR fl-acum-transferido AS DEC.
        DEF VAR fl-acum-devolvido   AS DEC.
        DEF VAR fl-acum-bon-des     AS DEC. 

        DEFINE VARIABLE sentence AS CHARACTER INITIAL "NF de Devoluá∆o: ".
        DEFINE VARIABLE found AS INTEGER.
        DEFINE VARIABLE sentence1 AS CHARACTER INITIAL "Valor Pago ao Produtor R$: ".
        DEFINE VARIABLE found1 AS INTEGER.
        DEFINE VARIABLE sentence2 AS CHARACTER INITIAL "Quantidade Liquidada em Scs ".
        DEFINE VARIABLE found2 AS INTEGER.
        DEFINE VARIABLE sentence3 AS CHARACTER INITIAL "Valor P/kg Negociado R$: ".
        DEFINE VARIABLE found3 AS INTEGER.
        DEFINE VARIABLE sentence4 AS CHARACTER INITIAL "Valor P/kg Pago (Bonif/Desc.Financeiro) R$: ".
        DEFINE VARIABLE found4 AS INTEGER.
        DEFINE VARIABLE sentence5 AS CHARACTER INITIAL "Data de Pagamento: ".
        DEFINE VARIABLE found5 AS INTEGER.
        DEFINE VARIABLE sentence6 AS CHARACTER INITIAL "Quantidade Liquidada em KG: ".
        DEFINE VARIABLE found6 AS INTEGER.

        DEFINE VARIABLE x-int  AS INT.
        DEFINE VARIABLE x-int1 AS INT.
        DEFINE VARIABLE l-x    AS LOG.

        DEFINE VARIABLE sentence7 AS CHARACTER INITIAL "NFP:".
        DEFINE VARIABLE found7 AS INTEGER.
        DEFINE VARIABLE sentence8 AS CHARACTER INITIAL "Data de Pagamento:".
        DEFINE VARIABLE found8 AS INTEGER.
        DEFINE VARIABLE sentence9f AS CHARACTER INITIAL "Valor do Complemento Bruto (+ Funrural) R$:". /* 22042010 isená∆o fr */
        DEFINE VARIABLE found9f AS INTEGER.
        /* senar DEFINE VARIABLE sentence9 AS CHARACTER INITIAL "Valor do Complemento Bruto R$:".    */
        DEFINE VARIABLE sentence9 AS CHARACTER INITIAL "Valor do Complemento Bruto (+ Senar) R$:".
        DEFINE VARIABLE found9 AS INTEGER.

        DEF BUFFER b-es-contrato-arroz FOR es-contrato-arroz.
        DEF BUFFER b-ins-emitente FOR emitente.

/*--------------- Variaveis para o EXCEL --------------------------------*/

Define Variable chexcelapplication as com-handle.
Define Variable chworkbook         as com-handle.
Define Variable chworksheet        as com-handle.
DEF VAR c-arq    AS CHAR.
DEF VAR c-arq2   AS CHAR.

DEF VAR stat     AS INTEGER.

DEF VAR c-tipo   AS CHAR.
DEF VAR i-ind    AS INT.
DEF VAR i-cont   AS INT.
DEF VAR c-celula AS CHAR.
def var c-destino         as char format "x(16)".
DEF VAR i-conta AS INT INITIAL 1.


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
    
/*---------------- Bloco Principal ---------------------------------------*/

Run utp/ut-acomp.p persistent set h-acomp.
Run pi-inicializar In h-acomp (Input "Impress∆o").
Run pi-seta-tipo   In h-acomp (Input 6).


    SESSION:SET-WAIT-STATE("general":U).

    FIND FIRST es-param-empres NO-LOCK.

    FOR EACH es-contrato-arroz WHERE
             es-contrato-arroz.nr-contrato >= tt-param.nr-contrato-ini  AND
             es-contrato-arroz.nr-contrato <= tt-param.nr-contrato-fim  AND
             es-contrato-arroz.cod-estabel >= tt-param.cod-estab-ini    AND
             es-contrato-arroz.cod-estabel <= tt-param.cod-estab-fim  USE-INDEX relat NO-LOCK:                                      
       
                IF es-contrato-arroz.situacao > 1 THEN NEXT.
                
                IF es-contrato-arroz.tipo-contrato = 1 AND tt-param.l-compra = NO THEN NEXT.
                IF es-contrato-arroz.tipo-contrato = 2 AND tt-param.l-deposito = NO THEN NEXT.
                IF es-contrato-arroz.tipo-contrato = 3 AND tt-param.l-importacao = NO THEN NEXT.
                IF es-contrato-arroz.tipo-contrato = 4 AND tt-param.l-arm-ent = NO THEN NEXT.
                IF es-contrato-arroz.tipo-contrato = 5 AND tt-param.l-tr-tit = NO THEN NEXT.

                IF es-contrato-arroz.u-int-2 <> tt-param.corretor-ini THEN NEXT.

                FIND FIRST es-saldo-arroz where
                    es-saldo-arroz.nr-contrato = es-contrato-arroz.nr-contrato
                    NO-LOCK no-error.
                IF NOT AVAIL es-saldo-arroz THEN NEXT.
                IF es-saldo-arroz.qtidade-atu = 0 THEN NEXT.

                FOR EACH tt-soma-contrato.      DELETE tt-soma-contrato.      END.
                FOR EACH tt-extrato:            DELETE tt-extrato.            END.                              
                FOR EACH tt-est-ins:            DELETE tt-est-ins.            END.     
                FOR EACH tt-imp.                DELETE tt-imp.                END.  
                FOR EACH tt-detalhes-sec.       DELETE tt-detalhes-sec.       END.   
                FOR EACH tt-aplic-desc-bon.     DELETE tt-aplic-desc-bon.     END.   
                FOR EACH tt-aplic-sec-desc-bon. DELETE tt-aplic-sec-desc-bon. END.   
                                                  
                ASSIGN fl-acum-geral       = 0    de-acum-secagem     = 0   
                       fl-acum-saldo       = 0    de-acum-seco        = 0   
                       fl-acum-aloca       = 0    de-acum-asecar      = 0   
                       fl-acum-bloqu       = 0    fl-acum-transferido = 0   
                       fl-acum-entre       = 0    fl-acum-devolvido   = 0   
                       fl-acum-bon-des     = 0.  
                

                RUN esp\esapi010e.p (INPUT es-contrato-arroz.nr-contrato,
                                     INPUT es-contrato-arroz.nr-contrato,
                                     INPUT es-contrato-arroz.cod-estabel,  
                                     INPUT es-contrato-arroz.cod-estabel,
                                     INPUT 0,   
                                     INPUT 999999999,    
                                     INPUT "",
                                     INPUT "ZZZZZZZZZZZZZZZZ", 
                                     INPUT 01/01/2000, 
                                     INPUT TODAY, 
                                     INPUT NO, /* "sintetico/analitico" quanto ao retorno de valores por item */
                                     INPUT "4021",
                                    
                                     OUTPUT TABLE tt-est-ins,
                                     OUTPUT TABLE tt-extrato,
                                     OUTPUT c-rend-medio-int,
                                     OUTPUT c-rend-medio-que,
                                     OUTPUT de-qt-contr).
                

                FIND FIRST tt-soma-contrato NO-LOCK NO-ERROR.
                IF NOT AVAIL tt-soma-contrato THEN
                CREATE tt-soma-contrato.
                ASSIGN 
                    tt-soma-contrato.umidade        = tt-soma-contrato.umidade        + es-contrato-arroz.gr-umidade
                    tt-soma-contrato.rend-inteiro   = tt-soma-contrato.rend-inteiro   + es-contrato-arroz.rend-inteiro
                    tt-soma-contrato.preto-vermelho = tt-soma-contrato.preto-vermelho + es-contrato-arroz.u-dec-3
                    tt-soma-contrato.verde          = tt-soma-contrato.verde          + es-contrato-arroz.u-dec-5
                    tt-soma-contrato.barriga        = tt-soma-contrato.barriga        + es-contrato-arroz.gr-barriga
                    tt-soma-contrato.gesso          = tt-soma-contrato.gesso        + es-contrato-arroz.gr-gesso
                    tt-soma-contrato.manch-pic      = tt-soma-contrato.manch-pic      + es-contrato-arroz.u-dec-4
                    tt-soma-contrato.impureza       = tt-soma-contrato.impureza       + es-contrato-arroz.gr-impureza
                    tt-soma-contrato.rend-quebr     = tt-soma-contrato.rend-quebr     + es-contrato-arroz.rend-quebr
                    tt-soma-contrato.nr-contratos   = tt-soma-contrato.nr-contratos   + 1.  

               
                FOR EACH tt-est-ins
                 BREAK BY tt-est-ins.ins-estadual
                       BY tt-est-ins.cod-estabel:
                    /* acumula valores */
                    ASSIGN fl-acum-geral       = fl-acum-geral        + (tt-est-ins.de-qt-entra - (tt-est-ins.de-qt-sai * -1) - (tt-est-ins.de-qt-aloca * -1) + (tt-est-ins.de-qt-bloq  * -1) -
                                                                         tt-est-ins.de-qt-seco - (tt-est-ins.de-qt-entre * -1) - (tt-est-ins.de-qt-tra * -1) - (tt-est-ins.de-qt-dev * -1))
                           fl-acum-saldo       = fl-acum-saldo        + tt-est-ins.de-qt-entra - (tt-est-ins.de-qt-sai * -1)  
                           fl-acum-aloca       = fl-acum-aloca        + (tt-est-ins.de-qt-aloca * -1)
                           fl-acum-bloqu       = fl-acum-bloqu        + (tt-est-ins.de-qt-bloq  * -1)
                           fl-acum-entre       = fl-acum-entre        + (tt-est-ins.de-qt-entre * -1) + + tt-est-ins.de-qt-seco
                           de-acum-secagem     = de-acum-secagem      + tt-est-ins.de-qt-secag
                           de-acum-seco        = de-acum-seco         + tt-est-ins.de-qt-seco
                           de-acum-asecar      = de-acum-asecar       + tt-est-ins.de-qt-secar
                           fl-acum-transferido = fl-acum-transferido  + (tt-est-ins.de-qt-tra * -1)               
                           fl-acum-devolvido   = fl-acum-devolvido    + (tt-est-ins.de-qt-dev * -1)
                           fl-acum-bon-des     = fl-acum-bon-des      + de-bon-financ - de-dsc-financ.
                END.

                
                RUN pi-impressao.
                
               /*** bloqueio ***/
                RUN pi-bloqueio.
             
                           
                /*** devoluá∆o ***/
                RUN pi-devol.
             
                /*** transferància ***/                                          
                RUN pi-transf.
                          
                /*** liquidaá‰es ***/    
                 RUN pi-liquid. 

                 /*chworksheet:saveas(c-arq2).        */

                /* excel */
                RELEASE OBJECT chworksheet.
                RELEASE OBJECT chworkbook.    
                RELEASE OBJECT chexcelapplication.

            
END. /* for each contrato */
MESSAGE "TÇrmino da Execuá∆o!" VIEW-AS ALERT-BOX.

{include/i-rpclo.i}
RUN pi-finalizar IN h-acomp.
RETURN 'OK'.

PROCEDURE pi-impressao:   
    
    RUN pi-acompanhar IN h-acomp (INPUT es-contrato-arroz.nr-contrato).

    FIND emitente NO-LOCK
      WHERE emitente.cod-emitente = es-contrato-arroz.cod-emitente
      NO-ERROR.
    FIND FIRST repres NO-LOCK
      WHERE repres.cod-rep = es-contrato-arroz.u-INT-2 NO-ERROR.

    ASSIGN c-arq  = es-param-empresa.caminho-matriz + "\Modelo2 (Fisico).xls"
             i-conta = i-conta + 1.
/*       IF AVAIL repres THEN                                                                                                                                                                       */
/*           ASSIGN c-arq2   = "x:/ems204/programas especiais/erika/planilha/" + string(es-contrato-arroz.cod-estabel) + STRING(es-contrato-arroz.u-INT-2) + string(es-contrato-arroz.nr-contrato). */
/*                                                                                                                                                                                                  */
/*       ELSE ASSIGN c-arq2   = "x:/ems204/programas especiais/erika/planilha/" + string(es-contrato-arroz.cod-estabel) + string(es-contrato-arroz.nr-contrato).                                    */


      IF AVAIL repres THEN ASSIGN c-arq2   = es-param-empresa.caminho-matriz  +  string(es-contrato-arroz.cod-estabel) + STRING(es-contrato-arroz.u-INT-2) + string(es-contrato-arroz.nr-contrato).
                      ELSE ASSIGN c-arq2   = es-param-empresa.caminho-matriz  + string(es-contrato-arroz.cod-estabel) + string(es-contrato-arroz.nr-contrato).
  
      IF SEARCH(c-arq2) <> ? THEN do:
          MESSAGE "osdelete" VIEW-AS alert-box.
          OS-DELETE VALUE(c-arq2).
      END.

    IF SEARCH(c-arq) = ? THEN DO:      
      MESSAGE "Matriz de Impress∆o do Dep¢sito Bloqueado n∆o foi encontrado  ( " c-arq " ) "
               VIEW-AS ALERT-BOX.
      RETURN.
    END.   

/*     IF SEARCH(c-arqaux) = ? THEN DO:                                                                                */
/*       OS-CREATE-DIR VALUE(c-arqaux).                                                                                */
/*       ASSIGN stat = OS-ERROR.                                                                                       */
/*       IF stat NE 0 THEN ASSIGN c-arq2   = "x:/especificos/erika/planilha/" + string(es-contrato-arroz.nr-contrato). */
/*     END.                                                                                                            */
    
    /* excel */
    CREATE "excel.application" chexcelapplication.
    ASSIGN chexcelapplication:VISIBLE = TRUE
           chworkbook  = chexcelapplication:workbooks:ADD(c-arq).
   
    FIND estabelec NO-LOCK
        WHERE estabelec.cod-estabel = es-contrato-arroz.cod-estabel
        NO-ERROR.

    FIND ITEM NO-LOCK
        WHERE ITEM.it-codigo = es-contrato-arroz.it-codigo
        NO-ERROR.
    
    ASSIGN chworksheet = chexcelapplication:sheets:ITEM(1).

    
    /************************************************************ PµGINA 1 **************************************************************************/
    ASSIGN c-tipo = IF es-contrato-arroz.tipo-contrato = 1 THEN "Compra Direta" ELSE
                    IF es-contrato-arroz.tipo-contrato = 2 THEN "Dep¢sito"      ELSE 
                    IF es-contrato-arroz.tipo-contrato = 3 then "Importaá∆o"    else
                    IF es-contrato-arroz.tipo-contrato = 4 then "Armazenagem"   ELSE 
                    IF es-contrato-arroz.tipo-contrato = 5 then "Tra.Estab"     else "".

    ASSIGN chworksheet:range("M02"):VALUE  = "'" + STRING(TODAY,"99/99/9999")
           chworksheet:range("M03"):VALUE  = "'DE: " + STRING(es-contrato-arroz.nr-contrato) + " ATê " + STRING(es-contrato-arroz.nr-contrato)
           chworksheet:range("M04"):VALUE  = "'" + estabelec.nome + " - " + estabelec.cidade
           chworksheet:range("M05"):VALUE  = "'" + c-tipo            
           chworksheet:range("C05"):VALUE  = "'" + emitente.nome-emit
           chworksheet:range("C06"):VALUE  = "'" + ITEM.desc-item
           chworksheet:range("AD03"):VALUE = "'" + "001/001".

    ASSIGN chworksheet:range("F06"):VALUE  = "'" + IF AVAIL repres THEN repres.nome ELSE "".

    ASSIGN i-ind  = 11
           i-cont = 0.

    FOR EACH tt-extrato
        BY tt-extrato.nr-ticket
        BY tt-extrato.dt-trans:                                                                      

       IF tt-extrato.esp-docto BEGINS "sec" THEN NEXT.

       FIND FIRST es-contrato-arroz NO-LOCK
            WHERE es-contrato-arroz.nr-contrato = tt-extrato.nr-contrato
            NO-ERROR.

        /* erika/planilha */
        IF  tt-extrato.esp-doc = "DEP" and
            es-contrato-arroz.tipo-contrato = 4 then 
            tt-extrato.esp-doc = "ARM".    
        IF  tt-extrato.esp-doc = "DEP" and
            es-contrato-arroz.tipo-contrato = 5 then 
            tt-extrato.esp-doc = "TRE". 

        ASSIGN de-soma-ent-compra-dir = de-soma-ent-compra-dir + 
                                        (tt-extrato.peso-liq - tt-extrato.desc-impureza - tt-extrato.desc-umidade).
        
        FIND FIRST tt-imp where
                   tt-imp.nr-ticket = tt-extrato.nr-ticket
            NO-LOCK USE-INDEX nr-ticket no-error.
        IF NOT AVAIL tt-imp THEN DO:
            CREATE tt-imp.
            ASSIGN tt-imp.nr-ticket = tt-extrato.nr-ticket.
        END.

        IF tt-extrato.gr-impureza     <> 0  THEN tt-imp.gr-impureza      = tt-extrato.gr-impureza.
        IF tt-extrato.desc-impureza   <> 0  THEN tt-imp.desc-impureza    = tt-extrato.desc-impureza.
        IF tt-extrato.gr-umidade      <> 0  THEN tt-imp.gr-umidade       = tt-extrato.gr-umidade.
        IF tt-extrato.desc-umidade    <> 0  THEN tt-imp.desc-umidade     = tt-extrato.desc-umidade.               
        IF tt-extrato.gr-verde        <> 0  THEN tt-imp.gr-verde         = tt-extrato.gr-verde.
        IF tt-extrato.desc-verde      <> 0  THEN tt-imp.desc-verde       = tt-extrato.desc-verde.
        IF tt-extrato.gr-barriga      <> 0  THEN tt-imp.gr-barriga       = tt-extrato.gr-barriga.
        IF tt-extrato.desc-barriga    <> 0  THEN tt-imp.desc-barriga     = tt-extrato.desc-barriga.
        IF tt-extrato.gr-gesso      <> 0  THEN tt-imp.gr-gesso       = tt-extrato.gr-gesso.
        IF tt-extrato.desc-gesso    <> 0  THEN tt-imp.desc-gesso     = tt-extrato.desc-gesso.
        IF tt-extrato.gr-manch-pic    <> 0  THEN tt-imp.gr-manch-pic     = tt-extrato.gr-manch-pic.
        IF tt-extrato.desc-manch-pic  <> 0  THEN tt-imp.desc-manch-pic   = tt-extrato.desc-manch-pic.
        IF tt-extrato.gr-descascado   <> 0  THEN tt-imp.gr-descascado    = tt-extrato.gr-descascado.
        IF tt-extrato.desc-descascado <> 0  THEN tt-imp.desc-descascado  = tt-extrato.desc-descascado.
        IF tt-extrato.gr-secagem      <> 0  THEN tt-imp.gr-secagem       = tt-extrato.gr-secagem.
        IF tt-extrato.gr-preto-verm   <> 0  THEN tt-imp.gr-preto-verm    = tt-extrato.gr-preto-verm.
        IF tt-extrato.desc-preto-verm <> 0  THEN tt-imp.desc-preto-verm  = tt-extrato.desc-preto-verm.
        IF tt-extrato.gr-vermelho     <> 0  THEN tt-imp.gr-vermelho      = tt-extrato.gr-vermelho.
        IF tt-extrato.desc-vermelho   <> 0  THEN tt-imp.desc-vermelho    = tt-extrato.desc-vermelho.
        IF tt-extrato.desc-secagem    <> 0  THEN tt-imp.desc-secagem     = tt-extrato.desc-secagem.
        IF tt-extrato.desc-inteiro    <> 0  THEN tt-imp.desc-inteiro     = tt-extrato.desc-inteiro.
        IF tt-extrato.nr-placa        <> "" THEN tt-imp.nr-placa         = tt-extrato.nr-placa.
        IF tt-extrato.dt-trans        <> ?  THEN tt-imp.dt-trans         = tt-extrato.dt-trans.
        IF tt-extrato.serie           <> "" THEN tt-imp.serie            = tt-extrato.serie.
        IF tt-extrato.cod-estabel     <> "" THEN tt-imp.cod-estabel      = tt-extrato.cod-estabel.
        IF tt-extrato.esp-docto       <> "" THEN tt-imp.esp-docto        = tt-extrato.esp-docto.
        IF tt-extrato.nr-ticket       <> 0  THEN tt-imp.nr-ticket        = tt-extrato.nr-ticket.
        IF tt-extrato.nr-nota-fornec  <> "" THEN tt-imp.nr-nota-fornec   = tt-extrato.nr-nota-fornec.
        IF tt-extrato.nro-docto       <> "" THEN tt-imp.nro-docto        = tt-extrato.nro-docto.
        IF tt-extrato.peso-fornec     <> 0  THEN tt-imp.peso-fornec      = tt-extrato.peso-fornec.
        IF tt-extrato.peso-liq        <> 0  THEN tt-imp.peso-liq         = tt-extrato.peso-liq.
        IF tt-extrato.rend-inteiro    <> 0  THEN tt-imp.rend-inteiro     = tt-extrato.rend-inteiro.
        IF tt-extrato.rend-quebrado   <> 0  THEN tt-imp.rend-quebrado    = tt-extrato.rend-quebrado.
        IF tt-extrato.desc-rendimento <> 0  THEN tt-imp.desc-rendimento  = tt-extrato.desc-rendimento.
        IF tt-extrato.valor-unit      <> 0  THEN tt-imp.valor-unit       = tt-extrato.valor-unit.
        IF tt-extrato.valor-total     <> 0  THEN tt-imp.valor-total      = tt-extrato.valor-total.
        IF tt-extrato.qtd-bonific     <> 0  THEN tt-imp.qtd-bonific      = tt-extrato.qtd-bonific.
        
        
    END. /* tt-extrato */              

    FOR EACH tt-imp BY tt-imp.nr-ticket:
        
        IF tt-imp.nr-placa       = "" AND
           tt-imp.nr-nota-fornec = ""  THEN NEXT.    
            
        ASSIGN i-ind    = i-ind  + 1
               i-cont   = i-cont + 1.

        FIND FIRST nota-fiscal WHERE
                   nota-fiscal.cod-estabel = tt-imp.cod-estabel AND
                   nota-fiscal.serie       = tt-imp.serie  AND
                   nota-fiscal.nr-nota-fis = tt-imp.nro-docto
                   NO-LOCK USE-INDEX ch-nota NO-ERROR.
        
        IF NOT AVAIL nota-fiscal AND (es-contrato-arroz.tipo-contrato <> 4 OR es-contrato-arroz.tipo-contrato <> 5) THEN NEXT.

        FIND FIRST it-nota-fisc OF nota-fiscal NO-LOCK NO-ERROR.
        IF NOT AVAIL it-nota-fisc THEN NEXT.

        ASSIGN c-celula = "A" + STRING(i-ind)
               chworksheet:range(c-celula):VALUE = "'" + IF es-contrato-arroz.tipo-contrato = 3 OR  es-contrato-arroz.tipo-contrato = 4 or
                                                                                                    es-contrato-arroz.tipo-contrato = 5 THEN string(tt-imp.dt-trans) 
                                                                                                                                        ELSE string(it-nota-fisc.dt-emis-nota)
               c-celula = "B" + STRING(i-ind)
               chworksheet:range(c-celula):VALUE = "'" + tt-imp.esp-docto
               c-celula = "C" + STRING(i-ind)
               chworksheet:range(c-celula):VALUE = "'" + tt-imp.nr-placa
               c-celula = "D" + STRING(i-ind)
               chworksheet:range(c-celula):VALUE = "'" + tt-imp.nr-nota-fornec
               c-celula = "E" + STRING(i-ind)
               chworksheet:range(c-celula):VALUE = "'" + tt-imp.nro-docto
               c-celula = "F" + STRING(i-ind)
               chworksheet:range(c-celula):VALUE = "'" + IF tt-param.l-item AND (es-contrato-arroz.tipo-contrato <> 4 or
                                                                                 es-contrato-arroz.tipo-contrato <> 5) THEN STRING(it-nota-fisc.it-codigo) ELSE "0040"

               c-celula = "G" + STRING(i-ind)
               chworksheet:range(c-celula):VALUE = tt-imp.peso-fornec
               c-celula = "H" + STRING(i-ind)
               chworksheet:range(c-celula):VALUE = tt-imp.peso-liq
               /* Impureza */
               c-celula = "J" + STRING(i-ind)
               chworksheet:range(c-celula):VALUE = tt-imp.gr-impureza
               c-celula = "K" + STRING(i-ind)
               chworksheet:range(c-celula):VALUE = tt-imp.desc-impureza
               /* Umidade */
               c-celula = "L" + STRING(i-ind)
               chworksheet:range(c-celula):VALUE = tt-imp.gr-umidade
               c-celula = "M" + STRING(i-ind)
               chworksheet:range(c-celula):VALUE = tt-imp.desc-umidade

               /* Inteiro */
               c-celula = "N" + STRING(i-ind)
               chworksheet:range(c-celula):VALUE = tt-imp.rend-inteiro
               c-celula = "O" + STRING(i-ind)
               chworksheet:range(c-celula):VALUE = tt-imp.desc-inteiro

               /* Quebrado */
               c-celula = "P" + STRING(i-ind)
               chworksheet:range(c-celula):VALUE = tt-imp.rend-quebr
               c-celula = "Q" + STRING(i-ind)
               chworksheet:range(c-celula):VALUE = tt-imp.desc-rendimento
               
               /* Verde */
               c-celula = "T" + STRING(i-ind)
               chworksheet:range(c-celula):VALUE = tt-imp.gr-verde
               c-celula = "U" + STRING(i-ind)
               chworksheet:range(c-celula):VALUE = tt-imp.desc-verde

               /* Barriga */
               c-celula = "V" + STRING(i-ind)
               chworksheet:range(c-celula):VALUE = tt-imp.gr-barriga
               c-celula = "W" + STRING(i-ind)
               chworksheet:range(c-celula):VALUE = tt-imp.desc-barriga

               /* gesso */
               c-celula = "X" + STRING(i-ind)
               chworksheet:range(c-celula):VALUE = tt-imp.gr-gesso
               c-celula = "Y" + STRING(i-ind)
               chworksheet:range(c-celula):VALUE = tt-imp.desc-gesso


               /* Manchado Picado */
               c-celula = "AA" + STRING(i-ind)
               chworksheet:range(c-celula):VALUE = tt-imp.gr-manch-pic
               c-celula = "AB" + STRING(i-ind)
               chworksheet:range(c-celula):VALUE = tt-imp.desc-manch-pic

               /* descascado */
               c-celula = "AC" + STRING(i-ind)
               chworksheet:range(c-celula):VALUE = tt-imp.gr-descascado
               c-celula = "AD" + STRING(i-ind)
               chworksheet:range(c-celula):VALUE = tt-imp.desc-descascado

               /* Bofificacao - Impurezas */
               c-celula = "AE" + STRING(i-ind)
               chworksheet:range(c-celula):VALUE = IF es-contrato-arroz.gr-impureza <= tt-imp.gr-impureza
                                                      THEN es-contrato-arroz.gr-impureza
                                                      ELSE IF not(l-aponta-tra) THEN tt-imp.gr-impureza /* 280905 */
                                                                                ELSE
                                                   (tt-imp.qtd-bonific / tt-imp.peso-liq) * 100
               c-celula = "AF" + STRING(i-ind)
               chworksheet:range(c-celula):VALUE = tt-imp.qtd-bonific

               /* Secagem */
               /*c-celula = "Z"  + STRING(i-ind) 30/01 de z para aa */
               c-celula = "AG"  + STRING(i-ind)
               chworksheet:range(c-celula):VALUE = tt-imp.gr-secagem
               
               /*c-celula = "AA" + STRING(i-ind) 30/01 de aa para ab */
               c-celula = "AH" + STRING(i-ind)
               chworksheet:range(c-celula):VALUE = tt-imp.desc-secagem.
                                                   
           
           /* Preto Vermelho - 18/02/04 agora tb Vermelho */
           IF tt-imp.gr-preto-ver <> 0 THEN 
             ASSIGN
               c-celula = "AI" + STRING(i-ind)
               chworksheet:range(c-celula):VALUE = tt-imp.gr-preto-ver
               c-celula = "AJ" + STRING(i-ind)
               chworksheet:range(c-celula):VALUE = tt-imp.desc-preto-ver.
           ELSE 
               ASSIGN
               c-celula = "AI" + STRING(i-ind)
               chworksheet:range(c-celula):VALUE = tt-imp.gr-vermelho
               c-celula = "AJ" + STRING(i-ind)
               chworksheet:range(c-celula):VALUE = tt-imp.desc-vermelho.
           
           IF  es-contrato-arroz.tipo-contrato = 1 THEN DO:
               ASSIGN c-celula = "AK" + STRING(i-ind)
                      chworksheet:range(c-celula):VALUE   = tt-imp.valor-unit
                      c-celula = "AL" + STRING(i-ind)
                      chworksheet:range(c-celula):VALUE   = tt-imp.valor-total.
           END.
    END.  /* tt-imp */  

    /* 19/10 - inicio - marrom somente uma vez para exibir detalhes na primeira parte da planilha */
    for each tt-detalhes-sec NO-LOCK.        
                
        ASSIGN i-ind    = i-ind  + 1
               i-cont   = i-cont + 1.

        IF tt-detalhes-sec.esp-docto = "SEC" THEN next.
            
        ASSIGN c-celula = "A" + STRING(i-ind)
               chworksheet:range(c-celula):VALUE = tt-detalhes-sec.dt-trans
               c-celula = "B" + STRING(i-ind)
               chworksheet:range(c-celula):VALUE = "'" + tt-detalhes-sec.esp-docto

               /*c-celula = "AA" + STRING(i-ind) 30/01 de aa para ab */
               c-celula = "AE" + STRING(i-ind)
               chworksheet:range(c-celula):VALUE = tt-detalhes-sec.desc-secagem.
           
    end. /* avail tt-detalhes */
    /* fim 19/10 */    

    IF tt-param.medias = YES THEN DO:
        RUN esp/esapi006.p (INPUT es-contrato-arroz.nr-contrato,
                            OUTPUT c-rend-medio).
        ASSIGN chworksheet:range("A164"):VALUE   = "RENDIMENTO MêDIO"
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


    /* erika 30/03/212 IF int(es-contrato-arroz.tipo-contrato) = 1 THEN /* compra direta */
       ASSIGN chworksheet:range("C484"):VALUE = de-soma-ent-compra-dir. /* se contrato do tipo de compra ent∆o colocar o valor de entrada */*/
    IF int(es-contrato-arroz.tipo-contrato) <> 1 THEN /* diferente de compra direta */
       ASSIGN chworksheet:range("C484"):VALUE = fl-acum-entre /*(tt-est-ins.de-qt-entre  * -1)*/.

    ASSIGN chworksheet:range("C485"):VALUE = fl-acum-devolvido
           chworksheet:range("C486"):VALUE = de-acum-secagem - de-acum-seco
           chworksheet:range("C487"):VALUE = fl-acum-transferido
           chworksheet:range("C488"):VALUE = (fl-acum-bloqu * -1).

    /* /* 251104 */ - soma valores desconto e bonificacao para serem exibidos */
    for each tt-aplic-desc-bon:
      delete tt-aplic-desc-bon.
    end.
    for each tt-aplic-sec-desc-bon:
      delete tt-aplic-sec-desc-bon.
    end.

    FOR EACH es-ticket WHERE
        es-ticket.nr-contrato = es-saldo-arroz.nr-contrato
        NO-LOCK.
       
       FOR EACH es-aplic-desc-bon WHERE
                es-aplic-desc-bon.nr-ticket = es-ticket.nr-ticket
                NO-LOCK .
          find first tt-aplic-desc-bon where
                     tt-aplic-desc-bon.nr-ticket = es-aplic-desc-bon.nr-ticket 
                     no-lock no-error.
          if not avail tt-aplic-desc-bon then do:
            create tt-aplic-desc-bon .
            assign tt-aplic-desc-bon.nr-ticket = es-aplic-desc-bon.nr-ticket.
          END.
          ASSIGN   tt-aplic-desc-bon.qt-bon  = tt-aplic-desc-bon.qt-bon + es-aplic-desc-bon.qt-bon
                   tt-aplic-desc-bon.qt-desc = tt-aplic-desc-bon.qt-desc + es-aplic-desc-bon.qt-desc.
        end. /* for each es-aplic */

        FOR EACH es-aplic-sec-desc-bon WHERE
                es-aplic-sec-desc-bon.nr-ticket = es-ticket.nr-ticket
                NO-LOCK .
         
          find first tt-aplic-sec-desc-bon where
                     tt-aplic-sec-desc-bon.nr-ticket = es-aplic-sec-desc-bon.nr-ticket 
                     no-lock no-error.
          if not avail tt-aplic-sec-desc-bon then do:
            create tt-aplic-sec-desc-bon .
            assign tt-aplic-sec-desc-bon.nr-ticket = es-aplic-sec-desc-bon.nr-ticket.
          END.
          ASSIGN   tt-aplic-sec-desc-bon.qt-bon  = tt-aplic-sec-desc-bon.qt-bon + es-aplic-sec-desc-bon.qt-bon
                   tt-aplic-sec-desc-bon.qt-desc = tt-aplic-sec-desc-bon.qt-desc + es-aplic-sec-desc-bon.qt-desc.
        end. /* for each es-aplic-sec */

    END. /* for each es-ticket */

    ASSIGN 
         de-val-desc     = 0
         de-val-bon      = 0
         de-sec-val-bon  = 0
         de-sec-val-desc = 0.
     
    for each tt-aplic-desc-bon.
        assign de-val-bon  = de-val-bon  + tt-aplic-desc-bon.qt-bon 
               de-val-desc = de-val-desc + tt-aplic-desc-bon.qt-desc.
    end. /* for each tt-aplic */

    for each tt-aplic-sec-desc-bon.
        assign de-sec-val-bon  = de-sec-val-bon  + tt-aplic-sec-desc-bon.qt-bon 
               de-sec-val-desc = de-sec-val-desc + tt-aplic-sec-desc-bon.qt-desc.
    end. /* for each tt-aplic */

    

     /**** 29/11 - bloqueio proporcional ****/
     ASSIGN
         de-bloq-desc = 0
         de-bloq-bon  = 0.

     FIND FIRST es-bloq-desc-bon WHERE
                es-bloq-desc-bon.nr-contrato = 0 /*0i-nr-contrato*/
        NO-LOCK NO-ERROR.
     IF AVAIL es-bloq-desc-bon THEN DO:
         
       ASSIGN
         de-bloq-desc = es-bloq-desc-bon.qt-desc
         de-bloq-bon  = es-bloq-desc-bon.qt-bon.
     END.        
                                                                                         
    
    /* new resumo */
    ASSIGN /*chworksheet:range("E484"):VALUE = (de-bon-financ - de-dsc-financ)
           chworksheet:range("E486"):VALUE = de-acum-secagem 
           chworksheet:range("E488"):VALUE = de-qt-bloq*/
           chworksheet:range("C490"):VALUE = (tt-est-ins.de-bon-financ - de-dsc-financ).

    /* 31/01/04 coloca valores de mÇdia dos contratos de umidade, inteiros, preto-verm, verde, BARRIGA, GESSO, manch/pic, rend-quebr e impureza */
    FOR EACH tt-soma-contrato.
    ASSIGN 
        chworksheet:range("En13"):VALUE = (tt-soma-contrato.umidade        / tt-soma-contrato.nr-contratos)
        chworksheet:range("Eo13"):VALUE = (tt-soma-contrato.rend-inteiro   / tt-soma-contrato.nr-contratos)
        chworksheet:range("Ep13"):VALUE = (tt-soma-contrato.preto-vermelho / tt-soma-contrato.nr-contratos)
        chworksheet:range("Eq13"):VALUE = (tt-soma-contrato.verde          / tt-soma-contrato.nr-contratos)
        chworksheet:range("Er13"):VALUE = (tt-soma-contrato.barriga        / tt-soma-contrato.nr-contratos)
        chworksheet:range("ES13"):VALUE = (tt-soma-contrato.GESSO        / tt-soma-contrato.nr-contratos)
        chworksheet:range("ET13"):VALUE = (tt-soma-contrato.manch-pic      / tt-soma-contrato.nr-contratos)
        chworksheet:range("Eu13"):VALUE = (tt-soma-contrato.rend-quebr     / tt-soma-contrato.nr-contratos)
        chworksheet:range("Ev13"):VALUE = (tt-soma-contrato.impureza       / tt-soma-contrato.nr-contratos).
           
    END. /* tt-soma contrato */
   
  
END PROCEDURE.

PROCEDURE pi-bloqueio:

   FIND FIRST emitente WHERE 
              emitente.cod-emitente = es-contrato-arroz.cod-emitente NO-LOCK USE-INDEX codigo NO-ERROR.

   ASSIGN chworksheet = chexcelapplication:sheets:ITEM(4)

          chworksheet:range("D06"):VALUE  = "'" + STRING(es-contrato-arroz.nr-contrato)
          chworksheet:range("F02"):VALUE  = "'" + STRING(TODAY,"99/99/9999")
          chworksheet:range("D07"):VALUE  = "'" + emitente.nome-emit
          chworksheet:range("D08"):VALUE  = "'" + es-contrato-arroz.cod-estabel.

   ASSIGN i-ind  = 11
          i-cont = 0.

    FOR EACH es-bloqueio where
             es-bloqueio.nome-matriz = STRING(es-contrato-arroz.cod-emitente) AND
             es-bloqueio.nr-contrato = es-contrato-arroz.nr-contrato
        NO-LOCK:
        
        ASSIGN i-ind  = i-ind  + 1
               i-cont = i-cont + 1
               c-celula = "B" + STRING(i-ind)
               chworksheet:range(c-celula):VALUE = "'" + string(es-bloqueio.dt-trans)
               c-celula = "C" + STRING(i-ind)
               chworksheet:range(c-celula):VALUE = "'" + IF es-bloqueio.tp-trans THEN "Bloqueado" ELSE "Desbloqueado"
               c-celula = "D" + STRING(i-ind)
               chworksheet:range(c-celula):VALUE = IF es-bloqueio.tp-trans THEN es-bloqueio.quantidade ELSE (es-bloqueio.quantidade * -1)
               c-celula = "E" + STRING(i-ind)
               chworksheet:range(c-celula):VALUE = "'" + es-bloqueio.u-char-2
               c-celula = "F" + STRING(i-ind)
               chworksheet:range(c-celula):VALUE = "'" + es-bloqueio.usuario.
        
    END. /* bloqueio */    

END PROCEDURE. /* bloqueio */

PROCEDURE pi-devol.

    FIND FIRST emitente WHERE 
               emitente.cod-emitente = es-contrato-arroz.cod-emitente NO-LOCK USE-INDEX codigo NO-ERROR.
        
    ASSIGN chworksheet = chexcelapplication:sheets:ITEM(5)

         chworksheet:range("D06"):VALUE  = "'" + STRING(es-contrato-arroz.nr-contrato)
         chworksheet:range("H02"):VALUE  = "'" + STRING(TODAY,"99/99/9999")
         chworksheet:range("D07"):VALUE  = "'" + emitente.nome-emit
         chworksheet:range("D08"):VALUE  = "'" + es-contrato-arroz.cod-estabel.

    FOR EACH es-devolucao WHERE
             es-devolucao.nr-contrato = es-contrato-arroz.nr-contrato NO-LOCK:

        FIND FIRST es-movto-arroz WHERE
                   es-movto-arroz.nr-contrato = es-contrato-arroz.nr-contrato AND
                   es-movto-arroz.esp-docto   = "dev" AND
                   es-movto-arroz.dt-criacao  = es-devolucao.dt-devol
                   NO-LOCK USE-INDEX ch-esp NO-ERROR.
        FIND FIRST emitente WHERE 
                   emitente.cod-emitente = es-movto-arroz.cod-emitente NO-LOCK.
       
    
        ASSIGN i-ind  = i-ind  + 1
               i-cont = i-cont + 1
               c-celula = "B" + STRING(i-ind)
               chworksheet:range(c-celula):VALUE = "'" + string(es-devolucao.dt-devol)
               c-celula = "C" + STRING(i-ind)
               chworksheet:range(c-celula):VALUE = "'" + string(emitente.cod-emitente)
               c-celula = "D" + STRING(i-ind)            
               chworksheet:range(c-celula):VALUE = "'" + es-movto-arroz.ins-estadual
               c-celula = "E" + STRING(i-ind)
               chworksheet:range(c-celula):VALUE = "'" + es-movto-arroz.nro-docto
               c-celula = "F" + STRING(i-ind)
               chworksheet:range(c-celula):VALUE = es-devolucao.qt-devol
               c-celula = "G" + STRING(i-ind)
               chworksheet:range(c-celula):VALUE = "'" + es-devolucao.histor
               c-celula = "H" + STRING(i-ind)
               chworksheet:range(c-celula):VALUE = "'" + es-movto-arroz.usuario.
   END. /* devol */

END PROCEDURE. /* pi-devol */

PROCEDURE pi-transf.

    FIND FIRST emitente WHERE 
               emitente.cod-emitente = es-contrato-arroz.cod-emitente NO-LOCK USE-INDEX codigo NO-ERROR.

    ASSIGN chworksheet = chexcelapplication:sheets:ITEM(3)

         chworksheet:range("D06"):VALUE  = "'" + STRING(es-contrato-arroz.nr-contrato)
         chworksheet:range("K02"):VALUE  = "'" + STRING(TODAY,"99/99/9999")
         chworksheet:range("D07"):VALUE  = "'" + emitente.nome-emit
         chworksheet:range("D08"):VALUE  = "'" + es-contrato-arroz.cod-estabel
         i-ind  = 11
         i-cont = 0.

    FOR EACH es-transferencia WHERE
             es-transferencia.nr-contrato-sai = es-contrato-arroz.nr-contrato
             NO-LOCK:

        
        FIND FIRST es-movto-arroz WHERE
                   es-movto-arroz.nr-contrato = es-transferencia.nr-contrato-sai
            NO-LOCK USE-INDEX ch-esp NO-ERROR.
        FIND FIRST es-ticket WHERE
                   es-ticket.nr-ticket = es-transferencia.nr-ticket-entra
            NO-LOCK NO-ERROR.
        IF NOT AVAIL es-ticket THEN NEXT.

        assign found = INDEX(es-ticket.obs,sentence,1).
        if found = 0 then c-tra-nfd = "".
        ELSE c-tra-nfd = SUBSTRING(es-ticket.obs,found + 17,7).

        ASSIGN i-ind  = i-ind  + 1
               i-cont = i-cont + 1
               c-celula = "B" + STRING(i-ind)
               chworksheet:range(c-celula):VALUE = "'" + string(es-movto-arroz.cod-emitente)
               c-celula = "C" + STRING(i-ind)
               chworksheet:range(c-celula):VALUE = "'" + string(es-movto-arroz.ins-estadual)
               c-celula = "D" + STRING(i-ind)            
               chworksheet:range(c-celula):VALUE = "'" + string(es-transferencia.dt-transf)
               c-celula = "E" + STRING(i-ind)
               chworksheet:range(c-celula):VALUE = "'" + c-tra-nfd
               c-celula = "F" + STRING(i-ind)
               chworksheet:range(c-celula):VALUE = es-transferencia.qt-transf.

        FIND FIRST es-movto-arroz WHERE 
                   es-movto-arroz.nr-contrato = es-transferencia.nr-contrato-ent and
                   es-movto-arroz.esp-docto   = "tra" AND
                   es-movto-arroz.quantidade  = es-transferencia.qt-transf
              NO-LOCK NO-ERROR.
        IF AVAIL es-movto-arroz THEN DO:
            FIND FIRST es-ticket WHERE es-ticket.nr-ticket = es-movto-arroz.nr-ticket NO-LOCK NO-ERROR.
            FIND FIRST emitente WHERE emitente.cod-emitente = es-movto-arroz.cod-emitente NO-LOCK NO-ERROR.
        END.
        
        FIND FIRST emitente WHERE emitente.cod-emitente = es-movto-arroz.cod-emitente NO-LOCK NO-ERROR.

        ASSIGN c-celula = "G" + STRING(i-ind)
               chworksheet:range(c-celula):VALUE = "'" + string(es-transferencia.nr-contrato-entra)
               c-celula = "H" + STRING(i-ind)
               chworksheet:range(c-celula):VALUE = "'" + string(es-ticket.cod-produtor)
               c-celula = "I" + STRING(i-ind)
               chworksheet:range(c-celula):VALUE = "'" + emitente.nome-emit
               c-celula = "J" + STRING(i-ind)
               chworksheet:range(c-celula):VALUE = "'" + es-ticket.ins-produtor
               c-celula = "K" + STRING(i-ind)
               chworksheet:range(c-celula):VALUE = "'" + string(es-ticket.nr-nota-fornec)
               c-celula = "L" + STRING(i-ind)
               chworksheet:range(c-celula):VALUE = "'" + string(es-ticket.nro-docto)
               c-celula = "M" + STRING(i-ind)
               chworksheet:range(c-celula):VALUE = "'" + string(es-transferencia.nr-ticket-entra).
   END. /* devol */    

END PROCEDURE. /* pi-transf */

PROCEDURE pi-liquid.

  FIND FIRST emitente WHERE 
             emitente.cod-emitente = es-contrato-arroz.cod-emitente NO-LOCK USE-INDEX codigo NO-ERROR.
  ASSIGN chworksheet = chexcelapplication:sheets:ITEM(2)

         chworksheet:range("D06"):VALUE  = "'" + STRING(es-contrato-arroz.nr-contrato)
         chworksheet:range("N02"):VALUE  = "'" + STRING(TODAY,"99/99/9999")
         chworksheet:range("D07"):VALUE  = "'" + emitente.nome-emit
         chworksheet:range("D08"):VALUE  = "'" + es-contrato-arroz.cod-estabel
         i-ind  = 11
         i-cont = 0.
        

  FOR EACH tt-extrato WHERE 
           tt-extrato.ins-estadual >= "" AND                            
           tt-extrato.cod-estabel  >= "" AND                            
           tt-extrato.cod-emitente >= 0  AND                             
           tt-extrato.nr-contrato   = es-contrato-arroz.nr-contrato AND 
           tt-extrato.nr-ticket    >= 0  AND
          (tt-extrato.esp-docto = "CMP"  OR
           tt-extrato.esp-docto = "SEC") USE-INDEX codigo.
  
       FIND FIRST docum-est 
            WHERE docum-est.serie         = tt-extrato.serie
              AND docum-est.nro-docto     = tt-extrato.nro-docto
              AND docum-est.cod-emitente  = tt-extrato.cod-emitente  
              AND (docum-est.nat-operacao = "1949oo"
              OR   docum-est.nat-operacao = "1101oo"
              OR   docum-est.nat-operacao = "1101sa")
       NO-LOCK USE-INDEX documento NO-ERROR. 

       /* 2355743 */
       IF NOT AVAIL docum-est THEN DO:

           blk_nat:
           FOR EACH ext-natur-oper-cfop NO-LOCK:

              IF ext-natur-oper-cfop.entrada-deposito    = YES
              OR ext-natur-oper-cfop.entrada-deposito-pj = YES THEN DO:

                  FIND FIRST docum-est 
                       WHERE docum-est.serie         = tt-extrato.serie
                         AND docum-est.nro-docto     = tt-extrato.nro-docto
                         AND docum-est.cod-emitente  = tt-extrato.cod-emitente  
                         AND docum-est.nat-operacao  = ext-natur-oper-cfop.nat-operacao
                  NO-LOCK USE-INDEX documento NO-ERROR. 

                  IF AVAIL docum-est THEN
                      LEAVE blk_nat.
              END.
          END.
       END.

       IF AVAIL docum-est THEN DO:
         FOR EACH item-doc-est OF docum-est NO-LOCK.
    
           IF item-doc-est.num-pedido <> 0 THEN DO:

             FOR EACH  ordem-compra NO-LOCK
                  WHERE ordem-compra.num-pedido = item-doc-est.num-pedido USE-INDEX pedido
                  ,EACH prazo-compra NO-LOCK OF ordem-compra:
                 
               IF ordem-compra.situacao <> 4 THEN DO:
                              
                   FIND FIRST pedido-comp WHERE pedido-compr.num-pedido = ordem-compra.num-pedido.
                   IF NOT AVAIL  pedido-comp THEN NEXT.

                    
    
                   FIND FIRST es-ticket WHERE 
                              es-ticket.nr-ticket = int(TRIM(substring(pedido-compr.c-observacao[2],20,20))) NO-LOCK NO-ERROR.

                   IF NOT AVAIL es-ticket THEN NEXT.

                   IF ordem-compra.int-1 <> tt-extrato.nr-contrato THEN NEXT.
                   
                   FIND FIRST nota-fiscal WHERE
                              nota-fiscal.cod-estabel = tt-extrato.cod-estabel AND
                              nota-fiscal.serie       = tt-extrato.serie       AND
                              nota-fiscal.nr-nota-fis = tt-extrato.nro-docto
                              NO-LOCK USE-INDEX ch-nota NO-ERROR.                                         
                   
                   IF NOT AVAIL nota-fiscal AND (es-contrato-arroz.tipo-contrato <> 4 OR es-contrato-arroz.tipo-contrato <> 5) THEN NEXT.

                   FIND FIRST es-movto-arroz WHERE 
                              es-movto-arroz.nr-contrato = tt-extrato.nr-contrato AND
                              es-movto-arroz.esp-docto   = tt-extrato.esp-docto AND
                              es-movto-arroz.nr-ticket   = tt-extrato.nr-ticket 
                      NO-LOCK USE-INDEX ch-esp NO-ERROR.

                   FIND FIRST es-ticket WHERE es-ticket.nr-ticket = es-movto-arroz.nr-ticket NO-LOCK  NO-ERROR.
    
                   assign found1 = INDEX(pedido-compr.comentario,sentence1,1) /* "Quantidade Liquidada em Kg: " */
                          found2 = INDEX(pedido-compr.comentario,sentence2,1) /* "Quantidade Liquidada em Scs " */
                          found3 = INDEX(pedido-compr.comentario,sentence3,1) /* "Valor P/kg Negociado R$: " */
                          found4 = INDEX(pedido-compr.comentario,sentence4,1) /* "Valor P/kg Pago (Bonif/Desc.Financeiro) R$: */
                          found5 = INDEX(pedido-compr.comentario,sentence5,1)  /* "Data de Pagamento: " */
                          found6 = INDEX(pedido-compr.comentario,sentence6,1). /* "Quantidade Liquidada em Kg: " */
                   
                   

                   ASSIGN i-ind  = i-ind  + 1
                          i-cont = i-cont + 1
                          c-celula = "B" + STRING(i-ind)
                          chworksheet:range(c-celula):VALUE = "'" + string(tt-extrato.dt-trans)
                          c-celula = "C" + STRING(i-ind)
                          chworksheet:range(c-celula):VALUE = "'" + string(tt-extrato.cod-emitente)
                          c-celula = "D" + STRING(i-ind)            
                          chworksheet:range(c-celula):VALUE = "'" + string(tt-extrato.ins-estadual)
                          c-celula = "E" + STRING(i-ind)            
                          chworksheet:range(c-celula):VALUE = "'" + string(substr(item-doc-est.narrativa,71,10))
                          c-celula = "F" + STRING(i-ind)            
                          chworksheet:range(c-celula):VALUE = "'" + string(tt-extrato.nro-docto)
                          c-celula = "G" + STRING(i-ind)
                          chworksheet:range(c-celula):VALUE = "'" + IF tt-extrato.esp-docto = "sec" THEN "Secagem" ELSE "Normal"
                          c-celula = "H" + STRING(i-ind)
                          chworksheet:range(c-celula):VALUE = if found6 <> 0 THEN dec(trim(SUBSTRING(pedido-compr.comentario,found6 + 29,14))) ELSE 0
                          c-celula = "I" + STRING(i-ind)
                          chworksheet:range(c-celula):VALUE = if found2 <> 0 THEN dec(SUBSTRING(pedido-compr.comentario,found2 + 27,15)) ELSE 0
                           c-celula = "J" + STRING(i-ind)
                          chworksheet:range(c-celula):VALUE = if found3 <> 0 THEN dec(string( 50 * dec(trim(SUBSTRING(pedido-compr.comentario,found3 + 30,8))))) ELSE 0
                         
                          c-celula = "L" + STRING(i-ind)
                          chworksheet:range(c-celula):VALUE = if found1 <> 0 THEN dec(SUBSTRING(pedido-compr.comentario,found1 + 27,15)) ELSE 0

                          c-celula = "K" + STRING(i-ind)
                          chworksheet:range(c-celula):VALUE = if found1 <> 0 THEN dec(SUBSTRING(pedido-compr.comentario,found1 + 27,15)) /  dec(SUBSTRING(pedido-compr.comentario,found2 + 27,15))  ELSE 0
                          c-celula = "M" + STRING(i-ind)
                          chworksheet:range(c-celula):VALUE = "'" + if found5 <> 0 THEN SUBSTRING(pedido-compr.comentario,found5 + 19,25)  ELSE "0"
                          c-celula = "N" + STRING(i-ind)
                          chworksheet:range(c-celula):VALUE = IF (es-contrato-arroz.tipo-contrato <> 4 or
                                                                  es-contrato-arroz.tipo-contrato <> 5) THEN nota-fiscal.user-calc ELSE es-movto-arroz.usuario.
                   
/*                    ASSIGN x-int = INTEGER(ENTRY(1, trim(SUBSTRING(pedido-compr.comentario,found6 + 29,14)), " ")) NO-ERROR.                                                 */
/*                    IF ERROR-STATUS:ERROR THEN DO:                                                                                                                           */
/*                        ASSIGN c-celula = "H" + STRING(i-ind)                                                                                                                */
/*                               chworksheet:range(c-celula):VALUE =  0.                                                                                                       */
/*                    END.                                                                                                                                                     */
/*                    ELSE DO:                                                                                                                                                 */
/*                        ASSIGN c-celula = "H" + STRING(i-ind)                                                                                                                */
/*                               chworksheet:range(c-celula):VALUE = if found6 <> 0 THEN dec(trim(SUBSTRING(pedido-compr.comentario,found6 + 29,14))) ELSE 0.                  */
/*                    END.                                                                                                                                                     */
/*                                                                                                                                                                             */
/*                    ASSIGN x-int = INTEGER(ENTRY(1, trim(SUBSTRING(pedido-compr.comentario,found2 + 27,15)), " ")) NO-ERROR.                                                 */
/*                    IF ERROR-STATUS:ERROR THEN DO:                                                                                                                           */
/*                        ASSIGN c-celula = "I" + STRING(i-ind)                                                                                                                */
/*                               chworksheet:range(c-celula):VALUE =  0.                                                                                                       */
/*                    END.                                                                                                                                                     */
/*                    ELSE DO:                                                                                                                                                 */
/*                        ASSIGN c-celula = "I" + STRING(i-ind)                                                                                                                */
/*                               chworksheet:range(c-celula):VALUE = if found2 <> 0 THEN dec(trim(SUBSTRING(pedido-compr.comentario,found2 + 27,15))) ELSE 0.                  */
/*                    END.                                                                                                                                                     */
/*                                                                                                                                                                             */
/*                    ASSIGN x-int = INTEGER(ENTRY(1, trim(SUBSTRING(pedido-compr.comentario,found3 + 30,4)), " ")) NO-ERROR.                                                  */
/*                    IF ERROR-STATUS:ERROR THEN DO:                                                                                                                           */
/*                        ASSIGN c-celula = "J" + STRING(i-ind)                                                                                                                */
/*                               chworksheet:range(c-celula):VALUE =  0.                                                                                                       */
/*                    END.                                                                                                                                                     */
/*                    ELSE DO:                                                                                                                                                 */
/*                       ASSIGN c-celula = "J" + STRING(i-ind)                                                                                                                 */
/*                              chworksheet:range(c-celula):VALUE = if found3 <> 0 THEN dec(string( 50 * dec(trim(SUBSTRING(pedido-compr.comentario,found3 + 30,4))))) ELSE 0. */
/*                    END.                                                                                                                                                     */
/*                                                                                                                                                                             */
/*                    ASSIGN x-int = INTEGER(ENTRY(1, trim(SUBSTRING(pedido-compr.comentario,found3 + 30,4)), " ")) NO-ERROR.                                                  */
/*                    IF ERROR-STATUS:ERROR THEN DO:                                                                                                                           */
/*                        ASSIGN  c-celula = "L" + STRING(i-ind)                                                                                                               */
/*                                chworksheet:range(c-celula):VALUE =   0.                                                                                                     */
/*                    END.                                                                                                                                                     */
/*                    ELSE DO:                                                                                                                                                 */
/*                       ASSIGN c-celula = "L" + STRING(i-ind)                                                                                                                 */
/*                              chworksheet:range(c-celula):VALUE = if found1 <> 0 THEN dec(trim(SUBSTRING(pedido-compr.comentario,found1 + 27,15))) ELSE 0.                   */
/*                    END.                                                                                                                                                     */
/*                                                                                                                                                                             */
/*                    ASSIGN x-int = INTEGER(ENTRY(1, trim(SUBSTRING(pedido-compr.comentario,found1 + 27,15)), " ")) NO-ERROR.                                                 */
/*                    IF ERROR-STATUS:ERROR THEN l-x = YES. ELSE l-x = NO.                                                                                                     */
/*                    ASSIGN x-int1 = INTEGER(ENTRY(1, trim(SUBSTRING(pedido-compr.comentario,found2 + 27,15)), " ")) NO-ERROR.                                                */
/*                    IF ERROR-STATUS:ERROR THEN DO:                                                                                                                           */
/*                        ASSIGN  c-celula = "K" + STRING(i-ind)                                                                                                               */
/*                                chworksheet:range(c-celula):VALUE =   0.                                                                                                     */
/*                    END.                                                                                                                                                     */
/*                    ELSE DO:                                                                                                                                                 */
/*                       IF l-x = NO THEN DO:                                                                                                                                  */
/*                         ASSIGN c-celula = "K" + STRING(i-ind)                                                                                                               */
/*                                chworksheet:range(c-celula):VALUE = if found1 <> 0 THEN dec(trim(SUBSTRING(pedido-compr.comentario,found1 + 27,15))) /                       */
/*                                                                                     dec(trim(SUBSTRING(pedido-compr.comentario,found2 + 27,15)))  ELSE 0.                   */
/*                       END.                                                                                                                                                  */
/*                       ELSE  ASSIGN  c-celula = "K" + STRING(i-ind)                                                                                                          */
/*                                chworksheet:range(c-celula):VALUE =   0.                                                                                                     */
/*                    END.                                                                                                                                                     */
/*                                                                                                                                                                             */
/*                    ASSIGN                                                                                                                                                   */
/*                           c-celula = "M" + STRING(i-ind)                                                                                                                    */
/*                           chworksheet:range(c-celula):VALUE = "'" + if found5 <> 0 THEN SUBSTRING(pedido-compr.comentario,found5 + 19,25)  ELSE "0"                         */
/*                           c-celula = "N" + STRING(i-ind)                                                                                                                    */
/*                           chworksheet:range(c-celula):VALUE = IF (es-contrato-arroz.tipo-contrato <> 4 or                                                                   */
/*                                                                   es-contrato-arroz.tipo-contrato <> 5) THEN nota-fiscal.user-calc ELSE es-movto-arroz.usuario.             */

                                                                                                                                      

               END. /* situacao */
             END. /* for each ordem-compra */
           END. /* num-pedido <> 0 */
         END. /* item doc-est */
       END. /* avail docum */ 


  END. /* for each tt-extrato */
 
  /* liquidaá∆o complemento de preáo */
  FOR EACH es-movto-arroz WHERE 
         es-movto-arroz.nr-contrato   = es-contrato-arroz.nr-contrato AND
         es-movto-arroz.cod-emitente >= 0 AND
         es-movto-arroz.dt-trans     >= 01/01/2000 AND
         es-movto-arroz.dt-trans     <= 12/31/2007 AND
         es-movto-arroz.it-codigo    >= "" AND
         es-movto-arroz.cod-estabel  >= "" AND
         es-movto-arroz.tp-desconto   = "COMPLEMENTO" NO-LOCK USE-INDEX relat-31.

     
    FIND FIRST docum-est 
                WHERE docum-est.serie         = es-movto-arroz.serie
                  AND docum-est.nro-docto     = es-movto-arroz.nro-docto
                  AND docum-est.cod-emitente  = es-movto-arroz.cod-emitente  
                  AND (docum-est.nat-operacao  = "1101ct" OR docum-est.nat-operacao = "1101cm")
           NO-LOCK USE-INDEX documento NO-ERROR. 
           IF AVAIL docum-est THEN DO:

               FIND FIRST item-doc-est OF docum-est NO-LOCK NO-ERROR.

               FIND FIRST nota-fiscal WHERE
                          nota-fiscal.cod-estabel = es-movto-arroz.cod-estabel AND
                          nota-fiscal.serie       = es-movto-arroz.serie       AND
                          nota-fiscal.nr-nota-fis = es-movto-arroz.nro-docto
                          NO-LOCK USE-INDEX ch-nota NO-ERROR.                                         
               
               IF NOT AVAIL nota-fiscal AND (es-contrato-arroz.tipo-contrato <> 4 OR es-contrato-arroz.tipo-contrato <> 5) THEN NEXT.
            
               ASSIGN i-ind  = i-ind  + 1
                      i-cont = i-cont + 1
                      c-celula = "B" + STRING(i-ind)
                      chworksheet:range(c-celula):VALUE = "'" + string(es-movto-arroz.dt-trans)
                      c-celula = "C" + STRING(i-ind)
                      chworksheet:range(c-celula):VALUE = "'" + string(es-movto-arroz.cod-emitente)
                      c-celula = "D" + STRING(i-ind)            
                      chworksheet:range(c-celula):VALUE = "'" + string(es-movto-arroz.ins-estadual)
                      c-celula = "E" + STRING(i-ind)            
                      chworksheet:range(c-celula):VALUE = "'" + string(substr(docum-est.observacao,1,20))
                      c-celula = "F" + STRING(i-ind)            
                      chworksheet:range(c-celula):VALUE = "'" + string(es-movto-arroz.nro-docto)
                      c-celula = "G" + STRING(i-ind)
                      chworksheet:range(c-celula):VALUE = "'" + "Complem.Preáo"
                      c-celula = "L" + STRING(i-ind)
                      chworksheet:range(c-celula):VALUE = es-movto-arroz.valor
                      c-celula = "I" + STRING(i-ind)
                      chworksheet:range(c-celula):VALUE = /*if found2 <> 0 THEN DEC(SUBSTRING(pedido-compr.comentario,found2 + 27,15)) ELSE */ 0
                      c-celula = "J" + STRING(i-ind)
                      chworksheet:range(c-celula):VALUE = /*if found3 <> 0 THEN DEC(string( 50 * dec(SUBSTRING(pedido-compr.comentario,found3 + 30,8)))) ELSE */ 0
                      c-celula = "K" + STRING(i-ind)
                      chworksheet:range(c-celula):VALUE = /*if found4 <> 0 THEN DEC(string( 50 * dec(SUBSTRING(pedido-compr.comentario,found4 + 45,15)))) ELSE*/ 0
/*                       c-celula = "J" + STRING(i-ind)                                                                                             */
/*                       chworksheet:range(c-celula):VALUE = /*if found1 <> 0 THEN DEC(SUBSTRING(pedido-compr.comentario,found5 + 27,15)) ELSE */ 0 */
                      c-celula = "M" + STRING(i-ind)
                      chworksheet:range(c-celula):VALUE = /*"'" + if found5 <> 0 THEN SUBSTRING(pedido-compr.comentario,found5 + 19,25)  ELSE */ "0"
                      c-celula = "N" + STRING(i-ind)
                      chworksheet:range(c-celula):VALUE = es-movto-arroz.usuario.
           END. /* nota-fiscal */
               
  END. /* for each es-movto do compl. preáo atÇ 31/12/2007 */
   FOR EACH ordem-compra NO-LOCK
      WHERE ordem-compra.cod-emitente = es-contrato-arroz.cod-emitente AND
             ordem-compra.situacao    <> 4 USE-INDEX emitente
       ,EACH prazo-compra NO-LOCK OF ordem-compra:

      IF ordem-compra.int-1  <> es-contrato-arroz.nr-contrato THEN NEXT.
      IF ordem-compra.data-emissao < 12/31/2007 THEN NEXT. /* data anterior implantaá∆o compl.preáo no es4019 */
      IF ordem-compra.it-codigo <> "0001" THEN NEXT. /* compl.preáo somente no 0001 */

      FIND FIRST pedido-comp WHERE pedido-compr.num-pedido = ordem-compra.num-pedido.      
      IF not(pedido-compr.Comentario BEGINS "### Pedido de Complemento de Preáo ###") THEN NEXT.

      /* coment†rio do complemento de preáo */
      assign found7  = INDEX(pedido-compr.Comentario,sentence7,1)
             found8  = INDEX(pedido-compr.Comentario,sentence8,1)
             found9  = INDEX(pedido-compr.Comentario,sentence9,1)
             found9f = INDEX(pedido-compr.Comentario,sentence9f,1).


      FIND FIRST b-ins-emitente WHERE b-ins-emitente.cod-emitente = pedido-compr.cod-emitente NO-LOCK NO-ERROR.

      ASSIGN i-ind  = i-ind  + 1
             i-cont = i-cont + 1
             c-celula = "B" + STRING(i-ind)
             chworksheet:range(c-celula):VALUE = "'" + string(pedido-compr.data-pedido)
             c-celula = "C" + STRING(i-ind)
             chworksheet:range(c-celula):VALUE = "'" + string(pedido-compr.cod-emitente)
             c-celula = "D" + STRING(i-ind)            
             chworksheet:range(c-celula):VALUE = "'" + string(b-ins-emitente.ins-estadual)
             c-celula = "E" + STRING(i-ind)            
             chworksheet:range(c-celula):VALUE = "'" + SUBSTRING(pedido-compr.Comentario,(found7 + 5),7)
             c-celula = "F" + STRING(i-ind)            
             chworksheet:range(c-celula):VALUE = "'" + string(es-movto-arroz.nro-docto)
             c-celula = "G" + STRING(i-ind)
             chworksheet:range(c-celula):VALUE = "'" + "Complem.Preáo"
             c-celula = "L" + STRING(i-ind)
             chworksheet:range(c-celula):VALUE = ordem-compra.preco-orig
             c-celula = "I" + STRING(i-ind)
             chworksheet:range(c-celula):VALUE = /*if found2 <> 0 THEN DEC(SUBSTRING(pedido-compr.comentario,found2 + 27,15)) ELSE */ 0.
      IF TODAY < 04/22/2010 THEN do:
        ASSIGN c-celula = "J" + STRING(i-ind)
               chworksheet:range(c-celula):VALUE = if found9f <> 0 THEN DEC(string(SUBSTRING(pedido-compr.Comentario,(found9f + 45),10))) ELSE  0.
      END.
      ELSE DO:
            ASSIGN c-celula = "J" + STRING(i-ind)
               chworksheet:range(c-celula):VALUE = if found9 <> 0 THEN DEC(string(SUBSTRING(pedido-compr.Comentario,(found9 + 32),10))) ELSE  0.
      END.
      ASSIGN c-celula = "K" + STRING(i-ind)
             chworksheet:range(c-celula):VALUE = /*if found4 <> 0 THEN DEC(string( 50 * dec(SUBSTRING(pedido-compr.comentario,found4 + 45,15)))) ELSE*/ 0
 /*             c-celula = "J" + STRING(i-ind)                                                                                             */
 /*             chworksheet:range(c-celula):VALUE = /*if found1 <> 0 THEN DEC(SUBSTRING(pedido-compr.comentario,found5 + 27,15)) ELSE */ 0 */
             c-celula = "M" + STRING(i-ind)
             chworksheet:range(c-celula):VALUE = "'" + SUBSTRING(pedido-compr.Comentario,(found8 + 5),7)
             c-celula = "N" + STRING(i-ind)
             chworksheet:range(c-celula):VALUE = ordem-compra.cod-comprado.


   END. /* ordem compra para complemento de preáo pedido pelo es4019 a partir de 01012008 */
   /*** fim compl preáo ***/




  /* liquidaá‰es de secagem */
  FOR EACH es-movto-arroz WHERE 
         es-movto-arroz.nr-contrato = es-contrato-arroz.nr-contrato AND
         es-movto-arroz.esp-docto   = "SEC" NO-LOCK USE-INDEX ch-esp.

      IF not(es-movto-arroz.hist BEGINS "compra") THEN NEXT.
      
      ELSE DO:
          
           FIND FIRST docum-est 
                WHERE docum-est.serie         = es-movto-arroz.serie
                  AND docum-est.nro-docto     = es-movto-arroz.nro-docto
                  AND docum-est.cod-emitente  = es-movto-arroz.cod-emitente  
                  AND (docum-est.nat-operacao = "1949oo"
                  OR   docum-est.nat-operacao = "1101oo"
                  OR   docum-est.nat-operacao = "1101sa")
           NO-LOCK USE-INDEX documento NO-ERROR. 

           /* 2355743 */
           IF NOT AVAIL docum-est THEN DO:
    
               blk_nat:
               FOR EACH ext-natur-oper-cfop NO-LOCK:
    
                  IF ext-natur-oper-cfop.entrada-deposito    = YES
                  OR ext-natur-oper-cfop.entrada-deposito-pj = YES THEN DO:
    
                      FIND FIRST docum-est 
                           WHERE docum-est.serie         = tt-extrato.serie
                             AND docum-est.nro-docto     = tt-extrato.nro-docto
                             AND docum-est.cod-emitente  = tt-extrato.cod-emitente  
                             AND docum-est.nat-operacao  = ext-natur-oper-cfop.nat-operacao
                      NO-LOCK USE-INDEX documento NO-ERROR. 
    
                      IF AVAIL docum-est THEN
                          LEAVE blk_nat.
                  END.
              END.
           END.

           IF AVAIL docum-est THEN DO:
               
             FOR EACH item-doc-est OF docum-est NO-LOCK.        

               IF item-doc-est.num-pedido <> 0 THEN DO:
               
                 FOR EACH  ordem-compra NO-LOCK
                      WHERE ordem-compra.num-pedido = item-doc-est.num-pedido USE-INDEX pedido
                      ,EACH prazo-compra NO-LOCK OF ordem-compra:
                     
                   IF ordem-compra.situacao <> 4 THEN DO:
                                  
                       FIND FIRST pedido-comp WHERE pedido-compr.num-pedido = ordem-compra.num-pedido.
        
                       FIND FIRST es-ticket WHERE 
                                  es-ticket.nr-ticket = int(TRIM(substring(pedido-compr.c-observacao[2],20,20))) NO-LOCK NO-ERROR.
        
                       IF NOT AVAIL es-ticket THEN NEXT.
                       
                       IF ordem-compra.int-1 <> es-contrato-arroz.nr-contrato THEN NEXT.
                       
                       
                       FIND FIRST nota-fiscal WHERE
                                  nota-fiscal.cod-estabel = es-movto-arroz.cod-estabel AND
                                  nota-fiscal.serie       = es-movto-arroz.serie       AND
                                  nota-fiscal.nr-nota-fis = es-movto-arroz.nro-docto
                                  NO-LOCK USE-INDEX ch-nota NO-ERROR.                                         
                       
                       IF NOT AVAIL nota-fiscal AND (es-contrato-arroz.tipo-contrato <> 4 OR es-contrato-arroz.tipo-contrato <> 5)  THEN NEXT.
        
                       assign found1 = INDEX(pedido-compr.comentario,sentence1,1) /* "Quantidade Liquidada em Kg: " */
                              found2 = INDEX(pedido-compr.comentario,sentence2,1) /* "Quantidade Liquidada em Scs " */
                              found3 = INDEX(pedido-compr.comentario,sentence3,1) /* "Valor P/kg Negociado R$: " */
                              found4 = INDEX(pedido-compr.comentario,sentence4,1) /* "Valor P/kg Pago (Bonif/Desc.Financeiro) R$: */
                              found5 = INDEX(pedido-compr.comentario,sentence5,1) /* "Data de Pagamento: " */
                              found6 = INDEX(pedido-compr.comentario,sentence6,1). /* "Quantidade Liquidada em Kg: " */

                       ASSIGN i-ind  = i-ind  + 1
                              i-cont = i-cont + 1
                              c-celula = "B" + STRING(i-ind)
                              chworksheet:range(c-celula):VALUE = "'" + string(es-movto-arroz.dt-trans)
                              c-celula = "C" + STRING(i-ind)
                              chworksheet:range(c-celula):VALUE = "'" + string(es-movto-arroz.cod-emitente)
                              c-celula = "D" + STRING(i-ind)            
                              chworksheet:range(c-celula):VALUE = "'" + string(es-movto-arroz.ins-estadual)
                              c-celula = "E" + STRING(i-ind)            
                              chworksheet:range(c-celula):VALUE = "'" + string(substr(item-doc-est.narrativa,71,10))
                              c-celula = "F" + STRING(i-ind)            
                              chworksheet:range(c-celula):VALUE = "'" + string(es-movto-arroz.nro-docto)
                              c-celula = "G" + STRING(i-ind)
                              chworksheet:range(c-celula):VALUE = "'" + "Secagem"
                              c-celula = "H" + STRING(i-ind)
                              chworksheet:range(c-celula):VALUE = es-movto-arroz.quantidade
                              c-celula = "I" + STRING(i-ind)
                              chworksheet:range(c-celula):VALUE = if found2 <> 0 THEN DEC(SUBSTRING(pedido-compr.comentario,found2 + 27,15)) ELSE 0
                              c-celula = "J" + STRING(i-ind)
                              chworksheet:range(c-celula):VALUE = if found3 <> 0 THEN DEC(string( 50 * dec(trim(SUBSTRING(pedido-compr.comentario,found3 + 30,8))))) ELSE 0
                              c-celula = "K" + STRING(i-ind)
                              chworksheet:range(c-celula):VALUE = es-movto-arroz.valor / DEC(SUBSTRING(pedido-compr.comentario,found2 + 27,15))
                              c-celula = "L" + STRING(i-ind)
                              chworksheet:range(c-celula):VALUE = es-movto-arroz.valor
                              c-celula = "M" + STRING(i-ind)
                              chworksheet:range(c-celula):VALUE = "'" + if found5 <> 0 THEN SUBSTRING(pedido-compr.comentario,found5 + 19,8)  ELSE "0"
                              c-celula = "N" + STRING(i-ind)
                              chworksheet:range(c-celula):VALUE = es-movto-arroz.usuario.
                                                                                                                                                   
                   END. /* situacao */
                 END. /* for each ordem-compra */
               END. /* num-pedido <> 0 */
             END. /* item doc-est */
           END. /* avail docum */
      END.
  END. /* for each movto para pegar detalhes das liquidaá‰es de secagem */
   
END PROCEDURE. /* liquidaá‰es */

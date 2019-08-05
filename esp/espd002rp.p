/* include de controle de versÆo */
{include/i-prgvrs.i ESPD002RP 1.00.00.000}

/* pr‚processador para ativar ou nÆo a sa¡da para RTF */
&GLOBAL-DEFINE RTF YES

/* pr‚processador para setar o tamanho da p gina */
&SCOPED-DEFINE pagesize 42   
/* defini‡Æo das temp-tables para recebimento de parƒmetros */

define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    field classifica       as integer
    field desc-classifica  as char format "x(40)"
    field modelo-rtf       as char format "x(35)"
    field l-habilitaRtf    as LOG
    FIELD ped-ini          AS CHAR 
    FIELD ped-fim          AS CHAR
    FIELD ped-rep-ini      AS CHAR
    FIELD ped-rep-fim      AS CHAR
    FIELD ped-sal-ini      AS CHAR
    FIELD ped-sal-fim      AS CHAR
    FIELD dt-entr-orig-ini AS DATE
    FIELD dt-entr-orig-fim AS DATE
    FIELD dt-entr-ini      AS DATE
    FIELD dt-entr-fim      AS DATE
    FIELD cod-rep-ini      AS INT
    FIELD cod-rep-fim      AS INT.

DEFINE TEMP-TABLE tt-raw-digita NO-UNDO
    FIELD raw-digita	   AS RAW.

/* recebimento de parƒmetros */
DEFINE INPUT PARAMETER raw-param AS RAW NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR tt-raw-digita.

DEFINE TEMP-TABLE tt-ped 
    FIELD nr-pedido     LIKE ped-venda.nr-pedido
    FIELD nr-sequencia  LIKE ped-item.nr-sequencia
    FIELD it-codigo     LIKE ped-item.it-codigo
    FIELD marca         AS CHAR
    FIELD cod-rep-padr  LIKE es-gp-mix-produto.cod-rep.

CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.

/* include padrÆo para vari veis de relat¢rio  */
{include/i-rpvar.i}

/* defini‡Æo de vari veis  */
DEFINE VARIABLE h-acomp   AS HANDLE     NO-UNDO.
DEF VAR l-cria            AS LOG        NO-UNDO.

/* defini‡Æo de frames do relat¢rio */
FORM /* usar ordenado por order-num */
     ped-venda.nr-pedcli     
     ped-venda.cod-estab     
     ped-venda.nome-abrev    
     ped-venda.no-ab-reppri  
     ped-item.it-codigo      
     ITEM.desc-item          
    WITH WIDTH 600 FRAME f-order DOWN STREAM-IO.

/*
FORM /* usar ordenado por cust-num */
    Order.Cust-Num 
    Order.Order-num 
    Order.Order-Date 
    Order.Promise-Date 
    Order.Sales-Rep 
    Order.Ship-Date 
    Order.Carrier
    WITH FRAME f-customer DOWN stream-io.*/

/* include padrÆo para output de relat¢rios */
{include/i-rpout.i &STREAM="stream str-rp"}

/* include com a defini‡Æo da frame de cabe‡alho e rodap‚ */
{include/i-rpcab.i &STREAM="str-rp"}


/* bloco principal do programa */
ASSIGN  
      c-programa 	    = "ESPD002RP"
	  c-versao	        = "1.00"
	  c-revisao	        = ".00.000"
	  c-empresa         = "Camil Alimentos"
	  c-sistema	        = "TOTVS"
	  c-titulo-relat    = "Relat¢rio de Exce‡Æo de Pedidos EDI".


/* para nÆo visualizar cabe‡alho/rodap‚ em sa¡da RTF */
IF tt-param.l-habilitaRTF <> YES THEN DO:
    VIEW STREAM str-rp FRAME f-cabec.
    VIEW STREAM str-rp FRAME f-rodape.
END.

/* executando de forma persistente o utilit rio de acompanhamento */
RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
{utp/ut-liter.i Imprimindo *}
RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

.MESSAGE 
        " ped-ini          " tt-param.ped-ini          sKIP
        " ped-fim          " tt-param.ped-fim          sKIP
        " ped-rep-ini      " tt-param.ped-rep-ini      sKIP
        " ped-rep-fim      " tt-param.ped-rep-fim      sKIP
        " ped-sal-ini      " tt-param.ped-sal-ini      SKIP
        " ped-sal-fim      " tt-param.ped-sal-fim      SKIP
        " dt-entr-orig-ini " tt-param.dt-entr-orig-ini SKIP
        " dt-entr-orig-fim " tt-param.dt-entr-orig-fim SKIP
        " dt-entr-ini      " tt-param.dt-entr-ini      SKIP
        " dt-entr-fim      " tt-param.dt-entr-fim      SKIP
        " cod-rep-ini      " tt-param.cod-rep-ini      SKIP
        " cod-rep-fim      " tt-param.cod-rep-fim      SKIP
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

/* corpo do relat¢rio */
FOR EACH ped-venda
   WHERE ped-venda.nr-pedcli       = "42326000019" 
     AND ped-venda.nr-pedcli      >= tt-param.ped-ini
     AND ped-venda.nr-pedcli      <= tt-param.ped-fim 
     AND ped-venda.nr-pedrep      >= tt-param.ped-rep-ini
     AND ped-venda.nr-pedrep      <= tt-param.ped-rep-fim 
     AND ped-venda.dt-entorig     >= tt-param.dt-entr-orig-ini  
     AND ped-venda.dt-entorig     <= tt-param.dt-entr-orig-fim 
     AND ped-venda.dt-entrega     >= tt-param.dt-entr-ini     
     AND ped-venda.dt-entrega     <= tt-param.dt-entr-fim ,
    EACH ext-ped-venda 
   WHERE ext-ped-venda.nr-pedcli   = ped-venda.nr-pedcli
     AND ext-ped-venda.nome-abrev  = ped-venda.nome-abrev 
     AND ext-ped-venda.nr-ped-sfa >= tt-param.ped-sal-ini
     AND ext-ped-venda.nr-ped-sfa <= tt-param.ped-sal-fim ,
    EACH repres
   WHERE repres.nome-abrev   = ped-venda.no-ab-reppri 
     AND repres.cod-rep     >= tt-param.cod-rep-ini
     AND repres.cod-rep     <= tt-param.cod-rep-fim 
    BREAK BY ped-venda.nr-pedcli
    ON STOP UNDO, LEAVE:

   RUN pi-acompanhar IN h-acomp (INPUT "Nr. Pedido: " + STRING(ped-venda.nr-pedcli)).

    ASSIGN l-cria = NO.
    FOR EACH ped-item OF ped-venda,
        EACH ITEM OF ped-item :

        /* Localiza a fam do item que o rep do ped nao entende */
        IF NOT CAN-FIND(FIRST es-gp-mix-produto
                        WHERE es-gp-mix-produto.fm-cod-com = ITEM.fm-cod-com 
                          AND es-gp-mix-produto.cod-rep    = repres.cod-rep) THEN DO:

            /* procura o primeiro rep que atenda o emitente do ped */
            FOR EACH  es-gp-mix-vendedor 
                WHERE es-gp-mix-vendedor.cod-emitente = ped-venda.cod-emitente NO-LOCK,
                FIRST es-gp-mix-produto 
                WHERE (es-gp-mix-produto.fm-cod-com = ITEM.fm-cod-com 
                   OR  es-gp-mix-produto.it-codigo  = ITEM.it-codigo) 
                  AND es-gp-mix-produto.cod-rep     = es-gp-mix-vendedor.cod-rep NO-LOCK:
                ASSIGN l-cria = YES.
            END.
        END.
    END.

    IF NOT l-cria THEN NEXT.

    FOR EACH ped-item OF ped-venda,
        EACH ITEM OF ped-item:

        CREATE tt-ped.
        ASSIGN tt-ped.nr-pedido     = ped-venda.nr-pedido
               tt-ped.nr-sequencia  = ped-item.nr-sequencia
               tt-ped.it-codigo     = ped-item.it-codigo.

        /* procura o primeiro rep que atenda o emitente do ped */
        FOR EACH  es-gp-mix-vendedor 
            WHERE es-gp-mix-vendedor.cod-emitente = ped-venda.cod-emitente NO-LOCK,
            FIRST es-gp-mix-produto 
            WHERE (es-gp-mix-produto.fm-cod-com = ITEM.fm-cod-com 
               OR  es-gp-mix-produto.it-codigo  = ITEM.it-codigo) 
              AND es-gp-mix-produto.cod-rep     = es-gp-mix-vendedor.cod-rep NO-LOCK:
    
            ASSIGN tt-ped.cod-rep-padr  = es-gp-mix-produto.cod-rep
                   tt-ped.marca         = "X".
        END.
    END.
END.


OUTPUT TO VALUE(SESSION:TEMP-DIR + "ESPD002.csv") NO-CONVERT.

PUT "Estab"                    
    ";Ped.Venda Cliente"         
    ";Pedido Venda Representante"
    ";Pedido Sales Force"        
    ";Cod. Representante"        
    ";Nome Representante"        
    ";Produto"                   
    ";Descri‡Æo"                 
    ";Quantidade"                
    ";Valor Total do Produto"    
    ";Data de Entrega"           
    ";Data de Entrega (ajustada)"
    ";Cod. Familia Comercial"    
    ";Desc. Familia Comercial"   
    ";Cod. Representante MIX"    
    ";Div"                       
    /*";Repres.Ped;"*/  SKIP. 

FOR EACH tt-ped:

    FIND FIRST ped-venda 
         WHERE ped-venda.nr-pedido = tt-ped.nr-pedido NO-LOCK NO-ERROR.

    FIND FIRST ped-item
         WHERE ped-item.it-codigo    = tt-ped.it-codigo    
           AND ped-item.nr-sequencia = tt-ped.nr-sequencia NO-LOCK NO-ERROR.

    FIND FIRST ITEM
         WHERE ITEM.it-codigo = tt-ped.it-codigo NO-LOCK NO-ERROR.

    FIND FIRST ext-ped-venda 
         WHERE ext-ped-venda.nr-pedcli  = ped-venda.nr-pedcli
           AND ext-ped-venda.nome-abrev = ped-venda.nome-abrev NO-LOCK NO-ERROR.

    FIND FIRST repres
         WHERE repres.nome-abrev = ped-venda.no-ab-reppri NO-LOCK NO-ERROR.

    FIND FIRST fam-comerc
         WHERE fam-comerc.fm-cod-com = ITEM.fm-cod-com NO-LOCK NO-ERROR.

    DISP STREAM str-rp
        ped-venda.cod-estab         COLUMN-LABEL "Estab."
        ped-venda.nr-pedcli         COLUMN-LABEL "Ped.Venda Cliente"
        ped-venda.nr-pedrep         COLUMN-LABEL "Pedido Venda Representante"
        ext-ped-venda.nr-ped-sfa    COLUMN-LABEL "Pedido Sales Force"
        repres.cod-rep              COLUMN-LABEL "Cod. Representante"
        mgcad.repres.nome           COLUMN-LABEL "Nome Representante"
        tt-ped.it-codigo            COLUMN-LABEL "Produto"
        ITEM.desc-item              COLUMN-LABEL "Descri‡Æo"
        ped-item.qt-pedida          COLUMN-LABEL "Quantidade"
        ped-item.vl-tot-it          COLUMN-LABEL "Valor Total do Produto"
        ped-venda.dt-entorig        COLUMN-LABEL "Data de Entrega"
        ped-venda.dt-entrega        COLUMN-LABEL "Data de Entrega (ajustada)"
        ITEM.fm-cod-com             COLUMN-LABEL "Cod. Familia Comercial"
        fam-comerc.descricao        COLUMN-LABEL "Desc. Familia Comercial"
        tt-ped.cod-rep-padr         COLUMN-LABEL "Cod. Representante MIX"
        tt-ped.marca                COLUMN-LABEL "Div"
        /*ped-venda.no-ab-reppri      COLUMN-LABEL "Repres.Ped"*/
        WITH FRAME f-order.
    DOWN STREAM str-rp WITH FRAME f-order.

    EXPORT DELIMITER ";" 
        ped-venda.cod-estab      
        ped-venda.nr-pedcli      
        ped-venda.nr-pedrep      
        ext-ped-venda.nr-ped-sfa 
        repres.cod-rep           
        mgcad.repres.nome       
        tt-ped.it-codigo        
        ITEM.desc-item          
        ped-item.qt-pedida      
        ped-item.vl-tot-it      
        ped-venda.dt-entorig    
        ped-venda.dt-entrega    
        ITEM.fm-cod-com         
        fam-comerc.descricao    
        tt-ped.cod-rep-padr     
        tt-ped.marca.            
        /*ped-venda.no-ab-reppri .*/
END.

OUTPUT CLOSE.


/*fechamento do output do relat¢rio*/
{include/i-rpclo.i &STREAM="stream str-rp"}
RUN pi-finalizar IN h-acomp.
RETURN "OK":U.

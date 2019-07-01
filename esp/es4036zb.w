&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME w-window
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-window 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
** performance
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante 
** autorizacao expressa.
** ECCB - BB 23/02/2013
*******************************************************************************/
{include/i-prgvrs.i ES4036ZB 2.09.00.002 } /*** "019002" ***/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */
                                           
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

{cdp/cdcfgdis.i}     /* Definicao dos pre-processadores     */

/* Definiªío da tabela temporˇria para itens de terceiros */
{esp/esbodi317sd.i1}
{esp/esbodi515.i tt-nota-fisc-adc}
 
 
/* Parameters Definitions ---                                           */

def shared var c-nat-operacao like saldo-terc.nat-operacao no-undo.

DEF NEW SHARED TEMP-TABLE tt-seco NO-UNDO
    FIELD ins-estadual AS CHAR
    field de-tot-qt-seco-0001 as dec
    field de-tot-qt-seco-0002 as dec
    field de-tot-qt-seco-0004 as dec
    field de-tot-qt-seco-0007 as dec
    field de-tot-qt-seco-0015 as dec
    field de-tot-qt-seco-0020 as dec
    field de-tot-qt-seco-0036 as dec
    field de-tot-qt-seco-0040 as DEC.    


DEFINE INPUT        PARAMETER Pi-operacao AS INT.
DEFINE INPUT        PARAMETER i-cod-emit-4036 LIKE saldo-terc.cod-emitente no-undo.
DEFINE INPUT        PARAMETER i-cod-estab-4036  LIKE saldo-terc.cod-estabel  no-undo.

DEFINE INPUT-OUTPUT PARAMETER TABLE FOR tt-it-terc-nf.
    
DEF NEW SHARED VAR de-quant-4036     AS DEC.
def shared var i-nr-contrato-4036    like es-contrato-arroz.nr-contrato no-undo. /* 280404 */ 
def shared var c-it-codigo-4036      like es-item.it-codigo no-undo. /* 191005 */ 
DEF SHARED VAR dt-trans-esapi007a-36 AS DATE. /* 011206 */ 

/* Local Variable Definitions --- */
DEF BUFFER bs-componente    FOR componente.
DEF BUFFER b-componente     FOR componente.
DEF BUFFER b-tt-it-terc-nf  FOR tt-it-terc-nf.
DEF BUFFER bf-emitente-api  FOR emitente. 

DEF VAR i-sequencia      AS INTEGER  NO-UNDO.
DEF VAR de-quant-aux     AS DEC DECIMALS 4.
DEF VAR de-quant-dev     AS DEC DECIMALS 4.
DEF VAR de-entra         AS DEC DECIMALS 4.
DEF VAR de-saida         AS DEC DECIMALS 4.
DEF VAR de-tot-saldo     AS DEC DECIMALS 4.
DEF VAR de-tot-secag     AS DEC DECIMALS 4.
DEF VAR de-tot-seco      AS DEC DECIMALS 4.
DEF VAR de-tot-seco-comp AS DEC DECIMALS 4.
DEF VAR de-a-secar       AS DEC DECIMALS 4.
DEF VAR de-a-secart      AS DEC DECIMALS 4.
DEF VAR de-acum-seco     AS DEC DECIMALS 4.
DEF VAR i-operacao       AS INT.     
DEF VAR de-acum-asecar   AS DEC INITIAL 0.

DEF NEW SHARED TEMP-TABLE tt-detalhes
    FIELD c-esp-docto       LIKE es-movto-arroz.esp-docto
    FIELD i-tipo-trans      LIKE es-movto-arroz.tipo-trans
    FIELD c-tp-desconto     LIKE es-movto-arroz.tp-desconto
    FIELD de2-analise        AS DEC DECIMALS 4
    FIELD de2-quantidade     LIKE es-movto-arroz.quantidade
    FIELD de2-valor-unit     LIKE es-movto-arroz.valor
    FIELD de2-valor-tot      LIKE es-movto-arroz.valor
    FIELD de2-perc           AS DEC
    FIELD c-historico       LIKE es-movto-arroz.historico.
   
DEF NEW SHARED TEMP-TABLE tt-escolhe
    FIELD linha AS INT.

DEF var de-soma-carga AS DEC.

def SHARED temp-table tt-cargas
    field l-marca   AS CHAR FORMAT "X(1)"
    field data      AS DATE
    field nr-ticket like es-ticket.nr-ticket
    field nfp       like es-ticket.nro-docto
    FIELD docto     LIKE es-ticket.nro-docto
    FIELD peso-nf   LIKE es-ticket.peso-liq
    FIELD peso-bal  LIKE es-ticket.peso-liq
    FIELD secagem   LIKE es-ticket.peso-liq
    FIELD peso      LIKE es-ticket.peso-liq
    FIELD desc-imp   AS DEC
    FIELD desc-umi   AS DEC
    FIELD desc-peso  AS DEC
    FIELD rend-int  AS DEC
    FIELD rend-que  AS DEC
    FIELD ver-ges   AS DEC
    FIELD pre-ver   AS DEC
    FIELD man-pic   AS DEC
    FIELD impureza  AS DEC
    FIELD i-linha AS INT
    FIELD it-codigo LIKE es-item.it-codigo
    INDEX i-linha l-marca nr-ticket.


/* 12/02/04 - passar cod-estabel do tt-wt-docto como parametro para es4036zb.w */
DEF SHARED VAR c-cod-estabel-esapi007a-36 LIKE estabelec.cod-estabel.
/* 17/02/04 - passar cod-emitente do tt-wt-docto como parametro para es4036zb.w */
DEF SHARED VAR c-cod-emitente-esapi007a-36 LIKE emitente.cod-emitente.

/**/
def NEW SHARED temp-table tt-movto-arroz
    field cod-estabel  like  es-saldo-arroz.cod-estabel 
    field cod-emitente like es-saldo-arroz.cod-emitente 
    field ins-estadual like es-saldo-arroz.ins-estadual 
    field nome-abrev   like emitente.nome-abrev 
    field it-codigo    like es-saldo-arroz.it-codigo 
    field secagem      like es-saldo-arroz.qtidade-atu 
    field bloqueada    like es-saldo-arroz.qt-bloqueada 
    field liquidar     like es-saldo-arroz.qt-bloqueada 
    field atual        like es-saldo-arroz.qt-bloqueada
    FIELD dt-trans     LIKE es-movto-arroz.dt-trans
    INDEX unico cod-estabel cod-emitente  ins-estadual nome-abrev it-codigo.

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
            FIELD gr-gesso        LIKE es-ticket.gr-gesso
            FIELD desc-gesso      LIKE es-movto-arroz.quantidade 
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
            INDEX codigo IS PRIMARY UNIQUE ins-estadual cod-estabel cod-emitente nr-contrato nr-ticket esp-docto nro-docto serie.

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
    
    DEF VAR c-rend-medio-int    AS CHAR.
    DEF VAR c-rend-medio-que    AS CHAR.
    DEF VAR c-rend-medio        AS CHAR.
    DEF VAR de-qt-contr         AS DEC DECIMALS 3.
    DEF VAR de-tot-qt-saldo AS DEC DECIMALS 4.

    /**/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE JanelaDetalhe
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS i-nr-contrato c-it-codigo de-quantidade ~
bt-ok bt-cancelar bt-ajuda bt-par RECT-1 RECT-2 
&Scoped-Define DISPLAYED-OBJECTS i-nr-contrato c-nome-matriz c-ins-estadual ~
c-it-codigo c-desc-item c-un de-quantidade 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-window AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-ajuda 
     LABEL "Ajuda" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-par 
     LABEL "ParÉm. Alternativo" 
     SIZE 20 BY 1.

DEFINE BUTTON bt-cancelar AUTO-END-KEY 
     LABEL "Cancelar" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "OK" 
     SIZE 10 BY 1.

DEFINE VARIABLE c-desc-item AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 49 BY .88 NO-UNDO.

DEFINE VARIABLE c-ins-estadual AS CHARACTER FORMAT "X(12)":U 
     LABEL "Insc. Estadual" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE c-it-codigo AS CHARACTER FORMAT "x(16)" 
     LABEL "Item":R5 
     VIEW-AS FILL-IN 
     SIZE 18 BY .88.

DEFINE VARIABLE c-nome-matriz AS CHARACTER FORMAT "X(12)":U 
     LABEL "Matriz" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE c-un AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE NEW SHARED VARIABLE de-quantidade AS DECIMAL FORMAT ">>>,>>>,>>9.9999":U 
    DECIMALS 4
     LABEL "Quantidade" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88 NO-UNDO.

DEFINE VARIABLE i-nr-contrato AS INTEGER FORMAT ">>>>>>>9":U INITIAL 0 
     LABEL "Contrato" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 77.86 BY 1.38
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 78 BY 6.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     i-nr-contrato AT ROW 1.63 COL 9.43 COLON-ALIGNED
     c-nome-matriz AT ROW 1.63 COL 33.14 COLON-ALIGNED
     c-ins-estadual AT ROW 1.63 COL 63.86 COLON-ALIGNED
     c-it-codigo AT ROW 3.29 COL 6 COLON-ALIGNED HELP
          "C¢digo do Item"
     c-desc-item AT ROW 3.29 COL 24 COLON-ALIGNED NO-LABEL
     c-un AT ROW 3.29 COL 73.29 COLON-ALIGNED NO-LABEL
     de-quantidade AT ROW 5.00 COL 23 COLON-ALIGNED
     bt-par AT ROW 5.00 COL 46
     bt-ok AT ROW 7.71 COL 3
     bt-cancelar AT ROW 7.71 COL 14
     bt-ajuda AT ROW 7.71 COL 69
     RECT-1 AT ROW 7.5 COL 2
     RECT-2 AT ROW 1.25 COL 2
     WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 81.86 BY 9.33.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: JanelaDetalhe
   Allow: Basic,Browse,DB-Fields,Smart,Window,Query
   Container Links: 
   Add Fields to: Neither
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-window ASSIGN
         HIDDEN             = YES
         TITLE              = "Devoluá∆o"
         HEIGHT             = 8.08
         WIDTH              = 82.29
         MAX-HEIGHT         = 29
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 29
         VIRTUAL-WIDTH      = 146.29
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB w-window 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-window.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-window
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
                                                                        */
/* SETTINGS FOR FILL-IN c-desc-item IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-ins-estadual IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-nome-matriz IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-un IN FRAME F-Main
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-window)
THEN w-window:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 
/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-window
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-window w-window
ON END-ERROR OF w-window /* Devolucao */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-window w-window
ON WINDOW-CLOSE OF w-window /* Devolucao */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ajuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ajuda w-window
ON CHOOSE OF bt-ajuda IN FRAME F-Main /* Ajuda */
OR HELP OF FRAME {&FRAME-NAME}
DO:
  {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-par
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-par w-window
ON CHOOSE OF bt-par IN FRAME F-Main /* Parametros */
OR HELP OF FRAME {&FRAME-NAME}
DO:
    FOR EACH tt-cargas:
        DELETE tt-cargas.
    END.
    /* 081105 - para secagem n∆o permite abrir o bot∆o de escolha do carga-a-carga */
    IF i-operacao = 2 THEN do:
       MESSAGE "Compra de Secagem n∆o permite Seleá∆o de Carga-a-Carga!" skip
           VIEW-AS ALERT-BOX ERROR.
    END. /* secagem */
    ELSE RUN esp/es4050.w.
    IF de-quantidade = 0 THEN DO:
      DISP de-quant-4036      @ de-quantidade 
           i-nr-contrato-4036 @ i-nr-contrato
           c-it-codigo-4036   @ c-it-codigo
           WITH FRAME f-main.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME bt-cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancelar w-window
ON CHOOSE OF bt-cancelar IN FRAME F-Main /* Cancelar */
DO:
  apply "close" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME  
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok w-window
ON CHOOSE OF bt-ok IN FRAME F-Main /* OK */
DO:
  SESSION:SET-WAIT-STATE("general":U).
       
  RUN geraItensTerceirosTtItTercNf.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL de-quantidade w-window
ON ENTRY OF i-nr-contrato IN FRAME F-Main /* Contrato */
DO:
    /* 2355743 */
    /* 14032012 para somente permitir a natureza certa para a operaá∆o correta */
    /*IF c-nat-operacao <> "5949MT" THEN DO:
        IF i-operacao = 1 THEN DO:
            IF not(c-nat-operacao = "5949DC" OR c-nat-operacao = "5949SB") THEN DO:
                MESSAGE "Natureza para Compra a Dep¢sito SOMENTE pode ser '5949DC' ou '5949SB'!!!" SKIP
                    "Verifique!"
                    VIEW-AS ALERT-BOX ERROR.
                APPLY 'close' TO THIS-PROCEDURE.
                
            END.
        END.
        ELSE DO:
            IF c-nat-operacao <> "5949AS" THEN DO:
                MESSAGE "Natureza para Compra de Secagem SOMENTE pode ser '5949AS'!!!" SKIP
                    "Verifique!"
                    VIEW-AS ALERT-BOX ERROR.
                APPLY 'close' TO THIS-PROCEDURE.
            END.
        END.
    END. /* mt 19/03/2012 */ */

    IF c-nat-operacao <> "5949MT" THEN DO:

        FIND FIRST ext-natur-oper-cfop NO-LOCK
             WHERE ext-natur-oper-cfop.nat-operacao = c-nat-operacao NO-ERROR.

        IF NOT AVAIL ext-natur-oper-cfop
        OR    (AVAIL ext-natur-oper-cfop AND NOT ext-natur-oper-cfop.dev-mud-titularidade) THEN DO:

            IF i-operacao = 1 THEN DO:

                IF not(c-nat-operacao = "5949DC" OR c-nat-operacao = "5949SB") THEN DO:

                    IF NOT AVAIL ext-natur-oper-cfop
                    OR   ((AVAIL ext-natur-oper-cfop AND NOT ext-natur-oper-cfop.dev-liq-deposito-pf) AND
                          (AVAIL ext-natur-oper-cfop AND NOT ext-natur-oper-cfop.dev-liq-deposito-pj)) THEN DO:
                
                        MESSAGE "Natureza para Compra a Dep¢sito SOMENTE pode ser 'Devoluá∆o de Liquidaá∆o de Dep¢sito PF' ou 'Devoluá∆o de Liquidaá∆o de Dep¢sito PJ'!!!" SKIP
                            "Verifique!"
                            VIEW-AS ALERT-BOX ERROR.
                        APPLY 'close' TO THIS-PROCEDURE.
                    END.
                END.
            END.
            ELSE DO:
                IF c-nat-operacao <> "5949AS" THEN DO:

                    IF NOT AVAIL ext-natur-oper-cfop
                    OR    (AVAIL ext-natur-oper-cfop AND NOT ext-natur-oper-cfop.dev-cob-secagem) THEN DO:

                        MESSAGE "Natureza para Compra de Secagem SOMENTE pode ser 'Devoluá∆o para Cobranáa de Serviáo de Secagem'!!!" SKIP
                            "Verifique!"
                            VIEW-AS ALERT-BOX ERROR.
                        APPLY 'close' TO THIS-PROCEDURE.
                    END.
                END.
            END. 
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL de-quantidade w-window
ON /*ENTRY */ LEAVE OF de-quantidade IN FRAME F-Main /* Quantidade */
DO:
 

    /********************** louca
   /*IF i-operacao <> 1 THEN DO: erika 13/03/2012 */
  
    FOR EACH tt-movto-arroz.
        DELETE tt-movto-arroz.
    END.
    FOR EACH tt-extrato:
        DELETE tt-extrato.
    END.
    FOR EACH tt-est-ins:
        DELETE tt-est-ins.
    END.
                       
    SESSION:SET-WAIT-STATE("general":U).
    
    FIND emitente NO-LOCK WHERE emitente.cod-emitente = int(c-cod-emitente-esapi007a-36) NO-ERROR.    
    
    

    ASSIGN de-tot-qt-saldo = 0
           de-acum-asecar  = 0.

    /* dois */

    MESSAGE "esapi entry de-quantidade"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RUN esp\esapi010.p (INPUT INPUT FRAME {&FRAME-NAME} i-nr-contrato,
                        INPUT INPUT FRAME {&FRAME-NAME} i-nr-contrato,
                        INPUT "",  
                        INPUT "99",
                        INPUT 0,
                        INPUT 999999999,
                        INPUT "",
                        INPUT "ZZZZZZZZZZZZZZZZ", 
                        INPUT 01/01/2000, 
                        INPUT TODAY, 
                        INPUT NO, /* "sintetico/analitico" quanto ao retorno de valores por item */
                        INPUT "4021", /* nome do prog que passa parametros */            
                        
                        OUTPUT TABLE tt-est-ins,
                        OUTPUT TABLE tt-extrato,
                        OUTPUT TABLE tt-seco,
                        OUTPUT c-rend-medio-int,
                        OUTPUT c-rend-medio-que,
                        OUTPUT de-qt-contr).
    
    de-acum-seco = 0.


    FOR EACH tt-est-ins.
      
       ASSIGN tt-est-ins.de-tot-seco-0001 = 0
              tt-est-ins.de-tot-seco-0002 = 0
              tt-est-ins.de-tot-seco-0004 = 0
              tt-est-ins.de-tot-seco-0007 = 0
              tt-est-ins.de-tot-seco-0015 = 0
              tt-est-ins.de-tot-seco-0020 = 0
              tt-est-ins.de-tot-seco-0036 = 0
              tt-est-ins.de-tot-seco-0040 = 0.
    END.
    

    saldo:
    FOR EACH tt-est-ins WHERE 
             tt-est-ins.nr-contrato  = INPUT FRAME {&FRAME-NAME} i-nr-contrato AND
             tt-est-ins.ins-estadual = emitente.ins-estadual AND
             tt-est-ins.cod-estabel  = i-cod-estab-4036 AND
             tt-est-ins.cod-emitente = i-cod-emit-4036 
         BREAK BY tt-est-ins.ins-estadual
               BY tt-est-ins.cod-estabel:
       

          IF tt-est-ins.ins-estadual = "isento" THEN NEXT.
         
          
          /* 16/03/2012 - PARA CARGA A CARGA - INFORMANDO O ITEM NA TELA para achar saldo de cada item e n∆o deixar devolver todo o saldo, s¢ qdo informado o item - Itaqui */
          IF c-it-codigo <> "" THEN DO:
              
            IF c-it-codigo = "0001" THEN DO:
                    FOR EACH tt-seco WHERE tt-seco.ins-estadual = tt-est-ins.ins-estadual NO-LOCK:
                       ASSIGN tt-est-ins.de-tot-seco-0001 = tt-est-ins.de-tot-seco-0001 + tt-seco.de-tot-qt-seco-0001
                              de-acum-seco = de-acum-seco + tt-seco.de-tot-qt-seco-0001.        
                    END.
                 
                    IF (round(tt-est-ins.de-qt-001,0) <> 0) OR (tt-est-ins.de-tot-secag-0001 <> 0) THEN DO:
                        ASSIGN de-acum-asecar  = (tt-est-ins.de-tot-secag-0001 - tt-est-ins.de-tot-seco-0001)
                               de-tot-qt-saldo = de-qt-001entra - (de-qt-001sai * -1) - (de-qt-001aloca * -1) + (de-qt-001bloq) -
                                                          de-qt-001secag - (de-qt-001entre * -1) - (de-qt-001tra * -1) - (de-qt-001dev * -1).


                        



                    END. /* item 0001 */
            END. /* 0001 */

            IF c-it-codigo = "0002" THEN DO:
                    FOR EACH tt-seco WHERE tt-seco.ins-estadual = tt-est-ins.ins-estadual NO-LOCK:
                       ASSIGN tt-est-ins.de-tot-seco-0002 = tt-est-ins.de-tot-seco-0002 + tt-seco.de-tot-qt-seco-0002
                              de-acum-seco = de-acum-seco + tt-seco.de-tot-qt-seco-0002.        
                    END.
                 
                    IF (round(tt-est-ins.de-qt-002,0) <> 0) OR (tt-est-ins.de-tot-secag-0002 <> 0) THEN DO:
                        ASSIGN de-acum-asecar  = (tt-est-ins.de-tot-secag-0002 - tt-est-ins.de-tot-seco-0002)
                               de-tot-qt-saldo = de-qt-002entra - (de-qt-002sai * -1) - (de-qt-002aloca * -1) + (de-qt-002bloq) -
                                                          de-qt-002secag - (de-qt-002entre * -1) - (de-qt-002tra * -1) - (de-qt-002dev * -1).
                    END. /* item 0002 */
            END. /* 0002 */

            IF c-it-codigo = "0004" THEN DO:
                    FOR EACH tt-seco WHERE tt-seco.ins-estadual = tt-est-ins.ins-estadual NO-LOCK:
                       ASSIGN tt-est-ins.de-tot-seco-0004 = tt-est-ins.de-tot-seco-0004 + tt-seco.de-tot-qt-seco-0004
                              de-acum-seco = de-acum-seco + tt-seco.de-tot-qt-seco-0004.        
                    END.
                 
                    IF (round(tt-est-ins.de-qt-004,0) <> 0) OR (tt-est-ins.de-tot-secag-0004 <> 0) THEN DO:
                        ASSIGN de-acum-asecar  = (tt-est-ins.de-tot-secag-0004 - tt-est-ins.de-tot-seco-0004)
                               de-tot-qt-saldo = de-qt-004entra - (de-qt-004sai * -1) - (de-qt-004aloca * -1) + (de-qt-004bloq) -
                                                          de-qt-004secag - (de-qt-004entre * -1) - (de-qt-004tra * -1) - (de-qt-004dev * -1).
                    END. /* item 0004 */
            END. /* 0004 */

            IF c-it-codigo = "0007" THEN DO:
                    FOR EACH tt-seco WHERE tt-seco.ins-estadual = tt-est-ins.ins-estadual NO-LOCK:
                       ASSIGN tt-est-ins.de-tot-seco-0007 = tt-est-ins.de-tot-seco-0007 + tt-seco.de-tot-qt-seco-0007
                              de-acum-seco = de-acum-seco + tt-seco.de-tot-qt-seco-0007.        
                    END.
                 
                    IF (round(tt-est-ins.de-qt-007,0) <> 0) OR (tt-est-ins.de-tot-secag-0007 <> 0) THEN DO:
                        ASSIGN de-acum-asecar  = (tt-est-ins.de-tot-secag-0007 - tt-est-ins.de-tot-seco-0007)
                               de-tot-qt-saldo = de-qt-007entra - (de-qt-007sai * -1) - (de-qt-007aloca * -1) + (de-qt-007bloq) -
                                                          de-qt-007secag - (de-qt-007entre * -1) - (de-qt-007tra * -1) - (de-qt-007dev * -1).
                    END. /* item 0007 */
            END. /* 0007 */

            IF c-it-codigo = "0015" THEN DO:
                    FOR EACH tt-seco WHERE tt-seco.ins-estadual = tt-est-ins.ins-estadual NO-LOCK:
                       ASSIGN tt-est-ins.de-tot-seco-0015 = tt-est-ins.de-tot-seco-0015 + tt-seco.de-tot-qt-seco-0015
                              de-acum-seco = de-acum-seco + tt-seco.de-tot-qt-seco-0015.        
                    END.
                 
                    IF (round(tt-est-ins.de-qt-015,0) <> 0) OR (tt-est-ins.de-tot-secag-0015 <> 0) THEN DO:
                        ASSIGN de-acum-asecar  = (tt-est-ins.de-tot-secag-0015 - tt-est-ins.de-tot-seco-0015)
                               de-tot-qt-saldo = de-qt-015entra - (de-qt-015sai * -1) - (de-qt-015aloca * -1) + (de-qt-015bloq) -
                                                          de-qt-015secag - (de-qt-015entre * -1) - (de-qt-015tra * -1) - (de-qt-015dev * -1).
                    END. /* item 0015 */
            END. /* 0015 */

            IF c-it-codigo = "0020" THEN DO:
                    FOR EACH tt-seco WHERE tt-seco.ins-estadual = tt-est-ins.ins-estadual NO-LOCK:
                       ASSIGN tt-est-ins.de-tot-seco-0020 = tt-est-ins.de-tot-seco-0020 + tt-seco.de-tot-qt-seco-0020
                              de-acum-seco = de-acum-seco + tt-seco.de-tot-qt-seco-0020.        
                    END.
                 
                    IF (round(tt-est-ins.de-qt-020,0) <> 0) OR (tt-est-ins.de-tot-secag-0020 <> 0) THEN DO:
                        ASSIGN de-acum-asecar  = (tt-est-ins.de-tot-secag-0020 - tt-est-ins.de-tot-seco-0020)
                               de-tot-qt-saldo = de-qt-020entra - (de-qt-020sai * -1) - (de-qt-020aloca * -1) + (de-qt-020bloq) -
                                                          de-qt-020secag - (de-qt-020entre * -1) - (de-qt-020tra * -1) - (de-qt-020dev * -1).
                    END. /* item 0020 */
            END. /* 0020 */

            IF c-it-codigo = "0036" THEN DO:
                    FOR EACH tt-seco WHERE tt-seco.ins-estadual = tt-est-ins.ins-estadual NO-LOCK:
                       ASSIGN tt-est-ins.de-tot-seco-0036 = tt-est-ins.de-tot-seco-0036 + tt-seco.de-tot-qt-seco-0036
                              de-acum-seco = de-acum-seco + tt-seco.de-tot-qt-seco-0036.        
                    END.
                 
                    IF (round(tt-est-ins.de-qt-036,0) <> 0) OR (tt-est-ins.de-tot-secag-0036 <> 0) THEN DO:
                        ASSIGN de-acum-asecar  = (tt-est-ins.de-tot-secag-0036 - tt-est-ins.de-tot-seco-0036)
                               de-tot-qt-saldo = de-qt-036entra - (de-qt-036sai * -1) - (de-qt-036aloca * -1) + (de-qt-036bloq) -
                                                          de-qt-036secag - (de-qt-036entre * -1) - (de-qt-036tra * -1) - (de-qt-036dev * -1).
                    END. /* item 0036 */
            END. /* 0036 */

            IF c-it-codigo = "0040" THEN DO:
                    FOR EACH tt-seco WHERE tt-seco.ins-estadual = tt-est-ins.ins-estadual NO-LOCK:
                       ASSIGN tt-est-ins.de-tot-seco-0040 = tt-est-ins.de-tot-seco-0040 + tt-seco.de-tot-qt-seco-0040
                              de-acum-seco = de-acum-seco + tt-seco.de-tot-qt-seco-0040.        
                    END.
                 
                    IF (round(tt-est-ins.de-qt-040,0) <> 0) OR (tt-est-ins.de-tot-secag-0040 <> 0) THEN DO:
                        ASSIGN de-acum-asecar  = (tt-est-ins.de-tot-secag-0040 - tt-est-ins.de-tot-seco-0040)
                               de-tot-qt-saldo = de-qt-040entra - (de-qt-040sai * -1) - (de-qt-040aloca * -1) + (de-qt-040bloq) -
                                                          de-qt-040secag - (de-qt-040entre * -1) - (de-qt-040tra * -1) - (de-qt-040dev * -1).
                    END. /* item 0040 */
            END. /* 0040 */

          END. /* LIQUIDAÄÂES INFORMANDO O ITEM NA TELA, DAI TRATA SOMENTE OS VALORES INFORMADOS */
          
         


          /* PARA LIQUIDAÄÂES SEM INFORMAR O ITEM */

          FOR EACH tt-seco WHERE tt-seco.ins-estadual = tt-est-ins.ins-estadual NO-LOCK:
    
               ASSIGN tt-est-ins.de-tot-seco-0001 = tt-est-ins.de-tot-seco-0001 + tt-seco.de-tot-qt-seco-0001
                      tt-est-ins.de-tot-seco-0002 = tt-est-ins.de-tot-seco-0002 + tt-seco.de-tot-qt-seco-0002
                      tt-est-ins.de-tot-seco-0004 = tt-est-ins.de-tot-seco-0004 + tt-seco.de-tot-qt-seco-0004
                      tt-est-ins.de-tot-seco-0007 = tt-est-ins.de-tot-seco-0007 + tt-seco.de-tot-qt-seco-0007
                      tt-est-ins.de-tot-seco-0015 = tt-est-ins.de-tot-seco-0015 + tt-seco.de-tot-qt-seco-0015
                      tt-est-ins.de-tot-seco-0020 = tt-est-ins.de-tot-seco-0020 + tt-seco.de-tot-qt-seco-0020
                      tt-est-ins.de-tot-seco-0036 = tt-est-ins.de-tot-seco-0036 + tt-seco.de-tot-qt-seco-0036
                      tt-est-ins.de-tot-seco-0040 = tt-est-ins.de-tot-seco-0040 + tt-seco.de-tot-qt-seco-0040.
        
               ASSIGN de-acum-seco = de-acum-seco + tt-seco.de-tot-qt-seco-0001 + tt-seco.de-tot-qt-seco-0002 + tt-seco.de-tot-qt-seco-0004 + tt-seco.de-tot-qt-seco-0007 + 
                                                    tt-seco.de-tot-qt-seco-0015 + tt-seco.de-tot-qt-seco-0020 + tt-seco.de-tot-qt-seco-0036 + tt-seco.de-tot-qt-seco-0040.

          END.



           ASSIGN de-tot-qt-saldo = de-tot-qt-saldo + (tt-est-ins.de-qt-entra - (tt-est-ins.de-qt-sai * -1) - (tt-est-ins.de-qt-aloca * -1) + (tt-est-ins.de-qt-bloq  * -1) - 
                                    tt-est-ins.de-qt-secag - (tt-est-ins.de-qt-entre * -1) - (tt-est-ins.de-qt-tra * -1) - (tt-est-ins.de-qt-dev * -1)) 
                  de-acum-asecar  = tt-est-ins.de-qt-secag - tt-est-ins.de-tot-seco-0001 - tt-est-ins.de-tot-seco-0002 - tt-est-ins.de-tot-seco-0004 - tt-est-ins.de-tot-seco-0007 -
                                    tt-est-ins.de-tot-seco-0015 - tt-est-ins.de-tot-seco-0020 - tt-est-ins.de-tot-seco-0036 - tt-est-ins.de-tot-seco-0040                                     
                  de-acum-asecar  = round(de-acum-asecar,2)
                  de-a-secar      = de-a-secar + de-qt-secag
                  de-tot-seco     = de-tot-seco + tt-est-ins.de-tot-seco-0001 - tt-est-ins.de-tot-seco-0002 - tt-est-ins.de-tot-seco-0004 - tt-est-ins.de-tot-seco-0007 -
                                    tt-est-ins.de-tot-seco-0015 - tt-est-ins.de-tot-seco-0020 - tt-est-ins.de-tot-seco-0036 - tt-est-ins.de-tot-seco-0040.





    END. /* FOR EACH tt-est-ins */

    louca ***********/    


    
    IF  i-nr-contrato-4036 > 0 THEN DO:
        FIND FIRST es-contrato-arroz NO-LOCK WHERE es-contrato-arroz.nr-contrato  = i-nr-contrato-4036 NO-ERROR.
   
        IF  AVAIL es-contrato-arroz AND
            es-contrato-arroz.tipo-contrato <> 2 THEN DO:
            MESSAGE "Este Contrato n∆o se refere a Dep¢sito de Produto"
                VIEW-AS ALERT-BOX ERROR.
            
            RETURN NO-APPLY.
        END.
        ELSE
        IF NOT AVAIL es-contrato-arroz THEN DO:
            MESSAGE "Contrato n∆o Cadastrado, Verificar" VIEW-AS ALERT-BOX ERROR.
        END.
    
            
    END.
    ELSE DO:
        ASSIGN c-nome-matriz:SCREEN-VALUE  = ""
               c-ins-estadual:SCREEN-VALUE = "".
    END.
    
/*                                                                            */
/*   IF i-operacao = 2 THEN DO:      doida                                         */
/*                                                                            */
/*       MESSAGE                                                              */
/*           "aqui" de-acum-asecar "aquiii" de-quantidade                     */
/*           VIEW-AS ALERT-BOX INFO BUTTONS OK.                               */
/*           DISP de-acum-asecar @ de-quantidade                              */
/*              i-nr-contrato-4036 @ i-nr-contrato  WITH FRAME {&FRAME-NAME}. */
/*   END.                                                                     */

  /*END. secagem - tirei para calcular secagem de qq jeito, para subtrair no caso de compra normal */
  
/* END.                                                             */
/* /* _UIB-CODE-BLOCK-END */                                        */
/* &ANALYZE-RESUME                                                  */
/*                                                                  */
/* &ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL de-quantidade w-window */
/* ON LEAVE OF de-quantidade IN FRAME F-Main /* Quantidade */       */
/* DO:                                                              */
    
    ASSIGN de-quant-4036  = INPUT de-quantidade
           de-quant-dev   = INPUT de-quantidade
           de-quant-aux   = 0
           de-quantidade  = INPUT FRAME {&FRAME-NAME} de-quantidade
           de-quantidade  = round(de-quantidade,0).
   

END.
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME c-it-codigo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-it-codigo w-window
ON F5 OF c-it-codigo IN FRAME F-Main /* Item */
DO:
    {include/zoomvar.i &prog-zoom=inzoom/z02in172.w
                       &campo=c-it-codigo
                       &campozoom=it-codigo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-it-codigo w-window
ON LEAVE OF c-it-codigo IN FRAME F-Main /* Item */
DO:
    
   
    ASSIGN i-nr-contrato.
    ASSIGN c-it-codigo-4036 = INPUT c-it-codigo. /* 191005 */

    IF  INPUT FRAME {&FRAME-NAME} c-it-codigo <> "" THEN DO:
        FIND ITEM
            WHERE ITEM.it-codigo = INPUT FRAME {&FRAME-NAME} c-it-codigo
            NO-LOCK NO-ERROR.
        IF  NOT AVAIL ITEM THEN DO:
            ASSIGN c-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
                   c-un:SCREEN-VALUE        IN FRAME {&FRAME-NAME} = "".
            MESSAGE "Item n∆o cadastrado."
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
            APPLY 'entry' TO c-it-codigo IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY.
        END.
        ELSE 
            ASSIGN c-it-codigo = c-it-codigo:SCREEN-VALUE IN FRAME {&FRAME-NAME}
                   c-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ITEM.desc-item
                   c-un:SCREEN-VALUE        IN FRAME {&FRAME-NAME} = ITEM.un.    END.
    ELSE
        ASSIGN c-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
               c-un:SCREEN-VALUE        IN FRAME {&FRAME-NAME} = "".

  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-it-codigo w-window
ON MOUSE-SELECT-DBLCLICK OF c-it-codigo IN FRAME F-Main /* Item */
DO:
    APPLY 'F5' TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME i-nr-contrato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL i-nr-contrato w-window
ON F5 OF i-nr-contrato IN FRAME F-Main /* Contrato */
DO:
    {include/zoomvar.i 
        &prog-zoom  = "eszoom\z02es407.w"
        &campo      = i-nr-contrato
        &campozoom  = nr-contrato
        &campo2     = c-nome-matriz
        &campozoom2 = nome-matriz
        &campo3     = c-ins-estadual
        &campozoom3 = ins-estadual}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL i-nr-contrato w-window
ON LEAVE OF i-nr-contrato IN FRAME F-Main /* Contrato */
DO:
    IF  INPUT FRAME {&FRAME-NAME} i-nr-contrato > 0 THEN DO:
        FIND FIRST es-contrato-arroz NO-LOCK
             WHERE es-contrato-arroz.nr-contrato  = INT(SELF:SCREEN-VALUE)
             NO-ERROR.
    
        IF  AVAIL es-contrato-arroz AND
            es-contrato-arroz.tipo-contrato <> 2 THEN DO:
            MESSAGE "Este Contrato n∆o se refere a Dep¢sito de Produto"
                VIEW-AS ALERT-BOX ERROR.
            APPLY "ENTRY" TO i-nr-contrato IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY.
        END.
        ELSE
        IF NOT AVAIL es-contrato-arroz THEN DO:
            MESSAGE "Contrato n∆o Cadastrado, Verificar"
                VIEW-AS ALERT-BOX ERROR.
        END.
        ELSE DO:
            
            ASSIGN c-nome-matriz:SCREEN-VALUE = es-contrato-arroz.nome-matriz
                   c-ins-estadual:SCREEN-VALUE = es-contrato-arroz.ins-estadual
                   i-nr-contrato-4036          = INPUT i-nr-contrato. /* 280404 */
        END.
            
    END.
    ELSE
        ASSIGN c-nome-matriz:SCREEN-VALUE  = ""
               c-ins-estadual:SCREEN-VALUE = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL i-nr-contrato w-window
ON MOUSE-SELECT-DBLCLICK OF i-nr-contrato IN FRAME F-Main /* Contrato */
DO:
    APPLY 'F5':U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-window 


/* ***************************  Main Block  *************************** */

i-nr-contrato:LOAD-MOUSE-POINTER("image\lupa.cur").
c-it-codigo:LOAD-MOUSE-POINTER("image\lupa.cur").

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects w-window  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available w-window  _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI w-window  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-window)
  THEN DELETE WIDGET w-window.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI w-window  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY i-nr-contrato c-nome-matriz c-ins-estadual c-it-codigo c-desc-item 
          c-un de-quantidade 
      WITH FRAME F-Main IN WINDOW w-window.
  ENABLE i-nr-contrato c-it-codigo de-quantidade bt-ok bt-cancelar bt-ajuda 
         bt-par RECT-1 RECT-2 
      WITH FRAME F-Main IN WINDOW w-window.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW w-window.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE gera-backup w-window 
PROCEDURE gera-backup :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE geraItensTerceirosTtItTercNf w-window 
PROCEDURE geraItensTerceirosTtItTercNf :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    FOR EACH tt-movto-arroz.
        DELETE tt-movto-arroz.
    END.
    FOR EACH tt-extrato:
        DELETE tt-extrato.
    END.
    FOR EACH tt-est-ins:
        DELETE tt-est-ins.
    END.


                       
    SESSION:SET-WAIT-STATE("general":U).
        FIND emitente NO-LOCK WHERE emitente.cod-emitente = int(c-cod-emitente-esapi007a-36) NO-ERROR.
    IF i-operacao = 2 THEN do: /* s¢ pra secagem */
        IF emitente.identif = 2 /* fornecedores */ THEN DO:
            MESSAGE "O Emitente " int(c-cod-emitente-esapi007a-36) " est† cadastrado somente como Fornecedor!!!" SKIP
                    "Verifique, pois do contr†rio a emiss∆o da Nota de Secagem n∆o ser† poss°vel!!!"
                VIEW-AS ALERT-BOX error.
        END.
    END.

    ASSIGN de-tot-qt-saldo = 0.


    
    RUN esp\esapi010.p (INPUT INPUT FRAME {&FRAME-NAME} i-nr-contrato,
                        INPUT INPUT FRAME {&FRAME-NAME} i-nr-contrato,
                        INPUT "",  
                        INPUT "99",
                        INPUT 0,   
                        INPUT 999999999,   
                        INPUT "",
                        INPUT "ZZZZZZZZZZZZZZZZ", 
                        INPUT 01/01/2000, 
                        INPUT TODAY, 
                        INPUT NO, /* "sintetico/analitico" quanto ao retorno de valores por item */
                        INPUT "4021", /* nome do prog que passa parametros */            
                        
                        OUTPUT TABLE tt-est-ins,
                        OUTPUT TABLE tt-extrato,
                        OUTPUT TABLE tt-seco,
                        OUTPUT c-rend-medio-int,
                        OUTPUT c-rend-medio-que,
                        OUTPUT de-qt-contr).
                       

    de-acum-seco = 0.
    
    FOR EACH tt-est-ins.      
       ASSIGN tt-est-ins.de-tot-seco-0001 = 0
              tt-est-ins.de-tot-seco-0002 = 0
              tt-est-ins.de-tot-seco-0004 = 0
              tt-est-ins.de-tot-seco-0007 = 0
              tt-est-ins.de-tot-seco-0015 = 0
              tt-est-ins.de-tot-seco-0020 = 0
              tt-est-ins.de-tot-seco-0036 = 0
              tt-est-ins.de-tot-seco-0040 = 0.
    END.

    FOR EACH tt-est-ins WHERE 
             tt-est-ins.nr-contrato  = INPUT FRAME {&FRAME-NAME} i-nr-contrato AND
             tt-est-ins.ins-estadual = emitente.ins-estadual AND
             tt-est-ins.cod-estabel  = i-cod-estab-4036 AND
             tt-est-ins.cod-emitente = i-cod-emit-4036 
         BREAK BY tt-est-ins.ins-estadual
               BY tt-est-ins.cod-estabel: /* 13/03/2012 interpertando tb a inscriá∆o */

          IF tt-est-ins.ins-estadual = "isento" THEN NEXT.


          /* loucura" */
          /* 16/03/2012 - PARA CARGA A CARGA - INFORMANDO O ITEM NA TELA para achar saldo de cada item e n∆o deixar devolver todo o saldo, s¢ qdo informado o item - Itaqui */
          IF c-it-codigo <> "" THEN DO:
              
            IF c-it-codigo = "0001" THEN DO:
                    FOR EACH tt-seco WHERE tt-seco.ins-estadual = tt-est-ins.ins-estadual NO-LOCK:
                       ASSIGN tt-est-ins.de-tot-seco-0001 = tt-est-ins.de-tot-seco-0001 + tt-seco.de-tot-qt-seco-0001
                              de-acum-seco = de-acum-seco + tt-seco.de-tot-qt-seco-0001.        
                    END.
                 
                    IF (round(tt-est-ins.de-qt-001,0) <> 0) OR (tt-est-ins.de-tot-secag-0001 <> 0) THEN DO:
                        ASSIGN de-acum-asecar  = (tt-est-ins.de-tot-secag-0001 - tt-est-ins.de-tot-seco-0001)
                               de-tot-qt-saldo = de-qt-001entra - (de-qt-001sai * -1) - (de-qt-001aloca * -1) + (de-qt-001bloq) -
                                                          de-qt-001secag - (de-qt-001entre * -1) - (de-qt-001tra * -1) - (de-qt-001dev * -1).
                    END. /* item 0001 */
            END. /* 0001 */

            IF c-it-codigo = "0002" THEN DO:
                    FOR EACH tt-seco WHERE tt-seco.ins-estadual = tt-est-ins.ins-estadual NO-LOCK:
                       ASSIGN tt-est-ins.de-tot-seco-0002 = tt-est-ins.de-tot-seco-0002 + tt-seco.de-tot-qt-seco-0002
                              de-acum-seco = de-acum-seco + tt-seco.de-tot-qt-seco-0002.        
                    END.
                 
                    IF (round(tt-est-ins.de-qt-002,0) <> 0) OR (tt-est-ins.de-tot-secag-0002 <> 0) THEN DO:
                        ASSIGN de-acum-asecar  = (tt-est-ins.de-tot-secag-0002 - tt-est-ins.de-tot-seco-0002)
                               de-tot-qt-saldo = de-qt-002entra - (de-qt-002sai * -1) - (de-qt-002aloca * -1) + (de-qt-002bloq) -
                                                          de-qt-002secag - (de-qt-002entre * -1) - (de-qt-002tra * -1) - (de-qt-002dev * -1).
                    END. /* item 0002 */
            END. /* 0002 */

            IF c-it-codigo = "0004" THEN DO:
                    FOR EACH tt-seco WHERE tt-seco.ins-estadual = tt-est-ins.ins-estadual NO-LOCK:
                       ASSIGN tt-est-ins.de-tot-seco-0004 = tt-est-ins.de-tot-seco-0004 + tt-seco.de-tot-qt-seco-0004
                              de-acum-seco = de-acum-seco + tt-seco.de-tot-qt-seco-0004.        
                    END.
                 
                    IF (round(tt-est-ins.de-qt-004,0) <> 0) OR (tt-est-ins.de-tot-secag-0004 <> 0) THEN DO:
                        ASSIGN de-acum-asecar  = (tt-est-ins.de-tot-secag-0004 - tt-est-ins.de-tot-seco-0004)
                               de-tot-qt-saldo = de-qt-004entra - (de-qt-004sai * -1) - (de-qt-004aloca * -1) + (de-qt-004bloq) -
                                                          de-qt-004secag - (de-qt-004entre * -1) - (de-qt-004tra * -1) - (de-qt-004dev * -1).
                    END. /* item 0004 */
            END. /* 0004 */

            IF c-it-codigo = "0007" THEN DO:
                    FOR EACH tt-seco WHERE tt-seco.ins-estadual = tt-est-ins.ins-estadual NO-LOCK:
                       ASSIGN tt-est-ins.de-tot-seco-0007 = tt-est-ins.de-tot-seco-0007 + tt-seco.de-tot-qt-seco-0007
                              de-acum-seco = de-acum-seco + tt-seco.de-tot-qt-seco-0007.        
                    END.
                 
                    IF (round(tt-est-ins.de-qt-007,0) <> 0) OR (tt-est-ins.de-tot-secag-0007 <> 0) THEN DO:
                        ASSIGN de-acum-asecar  = (tt-est-ins.de-tot-secag-0007 - tt-est-ins.de-tot-seco-0007)
                               de-tot-qt-saldo = de-qt-007entra - (de-qt-007sai * -1) - (de-qt-007aloca * -1) + (de-qt-007bloq) -
                                                          de-qt-007secag - (de-qt-007entre * -1) - (de-qt-007tra * -1) - (de-qt-007dev * -1).
                    END. /* item 0007 */
            END. /* 0007 */

            IF c-it-codigo = "0015" THEN DO:
                    FOR EACH tt-seco WHERE tt-seco.ins-estadual = tt-est-ins.ins-estadual NO-LOCK:
                       ASSIGN tt-est-ins.de-tot-seco-0015 = tt-est-ins.de-tot-seco-0015 + tt-seco.de-tot-qt-seco-0015
                              de-acum-seco = de-acum-seco + tt-seco.de-tot-qt-seco-0015.        
                    END.
                 
                    IF (round(tt-est-ins.de-qt-015,0) <> 0) OR (tt-est-ins.de-tot-secag-0015 <> 0) THEN DO:
                        ASSIGN de-acum-asecar  = (tt-est-ins.de-tot-secag-0015 - tt-est-ins.de-tot-seco-0015)
                               de-tot-qt-saldo = de-qt-015entra - (de-qt-015sai * -1) - (de-qt-015aloca * -1) + (de-qt-015bloq) -
                                                          de-qt-015secag - (de-qt-015entre * -1) - (de-qt-015tra * -1) - (de-qt-015dev * -1).
                    END. /* item 0015 */
            END. /* 0015 */

            IF c-it-codigo = "0020" THEN DO:
                    FOR EACH tt-seco WHERE tt-seco.ins-estadual = tt-est-ins.ins-estadual NO-LOCK:
                       ASSIGN tt-est-ins.de-tot-seco-0020 = tt-est-ins.de-tot-seco-0020 + tt-seco.de-tot-qt-seco-0020
                              de-acum-seco = de-acum-seco + tt-seco.de-tot-qt-seco-0020.        
                    END.
                 
                    IF (round(tt-est-ins.de-qt-020,0) <> 0) OR (tt-est-ins.de-tot-secag-0020 <> 0) THEN DO:
                        ASSIGN de-acum-asecar  = (tt-est-ins.de-tot-secag-0020 - tt-est-ins.de-tot-seco-0020)
                               de-tot-qt-saldo = de-qt-020entra - (de-qt-020sai * -1) - (de-qt-020aloca * -1) + (de-qt-020bloq) -
                                                          de-qt-020secag - (de-qt-020entre * -1) - (de-qt-020tra * -1) - (de-qt-020dev * -1).
                    END. /* item 0020 */
            END. /* 0020 */

            IF c-it-codigo = "0036" THEN DO:
                    FOR EACH tt-seco WHERE tt-seco.ins-estadual = tt-est-ins.ins-estadual NO-LOCK:
                       ASSIGN tt-est-ins.de-tot-seco-0036 = tt-est-ins.de-tot-seco-0036 + tt-seco.de-tot-qt-seco-0036
                              de-acum-seco = de-acum-seco + tt-seco.de-tot-qt-seco-0036.        
                    END.
                 
                    IF (round(tt-est-ins.de-qt-036,0) <> 0) OR (tt-est-ins.de-tot-secag-0036 <> 0) THEN DO:
                        ASSIGN de-acum-asecar  = (tt-est-ins.de-tot-secag-0036 - tt-est-ins.de-tot-seco-0036)
                               de-tot-qt-saldo = de-qt-036entra - (de-qt-036sai * -1) - (de-qt-036aloca * -1) + (de-qt-036bloq) -
                                                          de-qt-036secag - (de-qt-036entre * -1) - (de-qt-036tra * -1) - (de-qt-036dev * -1).
                    END. /* item 0036 */
            END. /* 0036 */

            IF c-it-codigo = "0040" THEN DO:
                    FOR EACH tt-seco WHERE tt-seco.ins-estadual = tt-est-ins.ins-estadual NO-LOCK:
                       ASSIGN tt-est-ins.de-tot-seco-0040 = tt-est-ins.de-tot-seco-0040 + tt-seco.de-tot-qt-seco-0040
                              de-acum-seco = de-acum-seco + tt-seco.de-tot-qt-seco-0040.        
                    END.
                 
                    IF (round(tt-est-ins.de-qt-040,0) <> 0) OR (tt-est-ins.de-tot-secag-0040 <> 0) THEN DO:
                        ASSIGN de-acum-asecar  = (tt-est-ins.de-tot-secag-0040 - tt-est-ins.de-tot-seco-0040)
                               de-tot-qt-saldo = de-qt-040entra - (de-qt-040sai * -1) - (de-qt-040aloca * -1) + (de-qt-040bloq) -
                                                          de-qt-040secag - (de-qt-040entre * -1) - (de-qt-040tra * -1) - (de-qt-040dev * -1).
                    END. /* item 0040 */
            END. /* 0040 */

          END. /* LIQUIDAÄÂES INFORMANDO O ITEM NA TELA, DAI TRATA SOMENTE OS VALORES INFORMADOS */
          ELSE DO:
              
    
              FOR EACH tt-seco WHERE tt-seco.ins-estadual = tt-est-ins.ins-estadual NO-LOCK:
        
                   ASSIGN tt-est-ins.de-tot-seco-0001 = tt-est-ins.de-tot-seco-0001 + tt-seco.de-tot-qt-seco-0001
                          tt-est-ins.de-tot-seco-0002 = tt-est-ins.de-tot-seco-0002 + tt-seco.de-tot-qt-seco-0002
                          tt-est-ins.de-tot-seco-0004 = tt-est-ins.de-tot-seco-0004 + tt-seco.de-tot-qt-seco-0004
                          tt-est-ins.de-tot-seco-0007 = tt-est-ins.de-tot-seco-0007 + tt-seco.de-tot-qt-seco-0007
                          tt-est-ins.de-tot-seco-0015 = tt-est-ins.de-tot-seco-0015 + tt-seco.de-tot-qt-seco-0015
                          tt-est-ins.de-tot-seco-0020 = tt-est-ins.de-tot-seco-0020 + tt-seco.de-tot-qt-seco-0020
                          tt-est-ins.de-tot-seco-0036 = tt-est-ins.de-tot-seco-0036 + tt-seco.de-tot-qt-seco-0036
                          tt-est-ins.de-tot-seco-0040 = tt-est-ins.de-tot-seco-0040 + tt-seco.de-tot-qt-seco-0040.
            
                   ASSIGN de-acum-seco = de-acum-seco + tt-seco.de-tot-qt-seco-0001 + tt-seco.de-tot-qt-seco-0002 + tt-seco.de-tot-qt-seco-0004 + tt-seco.de-tot-qt-seco-0007 + 
                                                        tt-seco.de-tot-qt-seco-0015 + tt-seco.de-tot-qt-seco-0020 + tt-seco.de-tot-qt-seco-0036 + tt-seco.de-tot-qt-seco-0040.
    
              END.              
    
               ASSIGN de-tot-qt-saldo = de-tot-qt-saldo + (tt-est-ins.de-qt-entra - (tt-est-ins.de-qt-sai * -1) - (tt-est-ins.de-qt-aloca * -1) + (tt-est-ins.de-qt-bloq) - 
                                        tt-est-ins.de-qt-secag - (tt-est-ins.de-qt-entre * -1) - (tt-est-ins.de-qt-tra * -1) - (tt-est-ins.de-qt-dev * -1)) 
                      
                      de-acum-asecar  = tt-est-ins.de-qt-secag - tt-est-ins.de-tot-seco-0001 - tt-est-ins.de-tot-seco-0002 - tt-est-ins.de-tot-seco-0004 - tt-est-ins.de-tot-seco-0007 -
                                        tt-est-ins.de-tot-seco-0015 - tt-est-ins.de-tot-seco-0020 - tt-est-ins.de-tot-seco-0036 - tt-est-ins.de-tot-seco-0040                                     
                      de-acum-asecar  = round(de-acum-asecar,2)

                      de-a-secar      = de-a-secar + de-qt-secag
                      de-tot-seco     = de-tot-seco + tt-est-ins.de-tot-seco-0001 - tt-est-ins.de-tot-seco-0002 - tt-est-ins.de-tot-seco-0004 - tt-est-ins.de-tot-seco-0007 -
                                        tt-est-ins.de-tot-seco-0015 - tt-est-ins.de-tot-seco-0020 - tt-est-ins.de-tot-seco-0036 - tt-est-ins.de-tot-seco-0040  /* de-acum-seco de-qt-seco*/.
          END. /* loucura */
                           

            
            IF tt-est-ins.de-qt-001 <> 0 THEN DO: 
                
                FIND FIRST emitente WHERE
                           emitente.cod-emitente = tt-est-ins.cod-emitente 
                           NO-LOCK NO-ERROR.
                find first tt-movto-arroz where
                         tt-movto-arroz.cod-estabel  = tt-est-ins.cod-estabel and
                         tt-movto-arroz.cod-emitente = tt-est-ins.cod-emitente AND
                         tt-movto-arroz.ins-estadual = emitente.ins-estadual and
                         tt-movto-arroz.nome-abrev   = emitente.nome-abrev and
                         tt-movto-arroz.it-codigo    = "0001"
                         no-lock USE-INDEX unico no-error.
                if not avail tt-movto-arroz then do:               
                    create tt-movto-arroz.
                    assign      
                      tt-movto-arroz.cod-estabel  = tt-est-ins.cod-estabel 
                      tt-movto-arroz.cod-emitente = tt-est-ins.cod-emitente
                      tt-movto-arroz.ins-estadual = emitente.ins-estadual 
                      tt-movto-arroz.nome-abrev   = emitente.nome-abrev 
                      tt-movto-arroz.it-codigo    = "0001"
                      tt-movto-arroz.dt-trans     = tt-est-ins.dt-impl.
                end. /* not avail tt-movto-arroz */
    
              
                ASSIGN tt-movto-arroz.secagem   = tt-movto-arroz.secagem   + tt-est-ins.de-qt-secar
                       tt-movto-arroz.liquidar  = tt-movto-arroz.liquidar  + de-qt-001 - de-qt-001bloq
                       tt-movto-arroz.atual     = tt-movto-arroz.atual     + de-qt-001 - de-a-secar-0001.
            END. /* item 0001 */
    
            IF tt-est-ins.de-qt-002 <> 0 THEN DO: 
                
                FIND FIRST emitente WHERE
                           emitente.cod-emitente = tt-est-ins.cod-emitente 
                           NO-LOCK NO-ERROR.
                find first tt-movto-arroz where
                         tt-movto-arroz.cod-estabel  = tt-est-ins.cod-estabel and
                         tt-movto-arroz.cod-emitente = tt-est-ins.cod-emitente AND
                         tt-movto-arroz.ins-estadual = emitente.ins-estadual and
                         tt-movto-arroz.nome-abrev   = emitente.nome-abrev and
                         tt-movto-arroz.it-codigo    = "0002"
                         no-lock USE-INDEX unico no-error.
                if not avail tt-movto-arroz then do:               
                    create tt-movto-arroz.
                    assign      
                      tt-movto-arroz.cod-estabel  = tt-est-ins.cod-estabel 
                      tt-movto-arroz.cod-emitente = tt-est-ins.cod-emitente
                      tt-movto-arroz.ins-estadual = emitente.ins-estadual 
                      tt-movto-arroz.nome-abrev   = emitente.nome-abrev 
                      tt-movto-arroz.it-codigo    = "0002"
                      tt-movto-arroz.dt-trans     = tt-est-ins.dt-impl.
                end. /* not avail tt-movto-arroz */
                ASSIGN tt-movto-arroz.secagem   = tt-movto-arroz.secagem   + tt-est-ins.de-qt-secar
                       
                       tt-movto-arroz.liquidar  = tt-movto-arroz.liquidar  + de-qt-002 - de-qt-002bloq
                       tt-movto-arroz.atual     = tt-movto-arroz.atual     + de-qt-002 - de-a-secar-0002.
            END. /* item 0002 */
    
            IF tt-est-ins.de-qt-004 <> 0 THEN DO: 
                
                FIND FIRST emitente WHERE
                           emitente.cod-emitente = tt-est-ins.cod-emitente 
                           NO-LOCK NO-ERROR.
                find first tt-movto-arroz where
                         tt-movto-arroz.cod-estabel  = tt-est-ins.cod-estabel and
                         tt-movto-arroz.cod-emitente = tt-est-ins.cod-emitente AND
                         tt-movto-arroz.ins-estadual = emitente.ins-estadual and
                         tt-movto-arroz.nome-abrev   = emitente.nome-abrev and
                         tt-movto-arroz.it-codigo    = "0004"
                         no-lock USE-INDEX unico no-error.
                if not avail tt-movto-arroz then do:               
                    create tt-movto-arroz.
                    assign      
                      tt-movto-arroz.cod-estabel  = tt-est-ins.cod-estabel 
                      tt-movto-arroz.cod-emitente = tt-est-ins.cod-emitente
                      tt-movto-arroz.ins-estadual = emitente.ins-estadual 
                      tt-movto-arroz.nome-abrev   = emitente.nome-abrev 
                      tt-movto-arroz.it-codigo    = "0004"
                      tt-movto-arroz.dt-trans     = tt-est-ins.dt-impl.
                end. /* not avail tt-movto-arroz */
                ASSIGN tt-movto-arroz.secagem   = tt-movto-arroz.secagem   + tt-est-ins.de-qt-secar
                       
                       tt-movto-arroz.liquidar  = tt-movto-arroz.liquidar  + de-qt-004 - de-qt-004bloq
                       tt-movto-arroz.atual     = tt-movto-arroz.atual     + de-qt-004 - de-a-secar-0004.
            END. /* item 0004 */
    
            IF tt-est-ins.de-qt-007 <> 0 THEN DO:
                
                FIND FIRST emitente WHERE
                           emitente.cod-emitente = tt-est-ins.cod-emitente 
                           NO-LOCK NO-ERROR.
                find first tt-movto-arroz where
                         tt-movto-arroz.cod-estabel  = tt-est-ins.cod-estabel and
                         tt-movto-arroz.cod-emitente = tt-est-ins.cod-emitente AND
                         tt-movto-arroz.ins-estadual = emitente.ins-estadual and
                         tt-movto-arroz.nome-abrev   = emitente.nome-abrev and
                         tt-movto-arroz.it-codigo    = "0007"
                         no-lock USE-INDEX unico no-error.
                if not avail tt-movto-arroz then do:               
                    create tt-movto-arroz.
                    assign      
                      tt-movto-arroz.cod-estabel  = tt-est-ins.cod-estabel 
                      tt-movto-arroz.cod-emitente = tt-est-ins.cod-emitente
                      tt-movto-arroz.ins-estadual = emitente.ins-estadual 
                      tt-movto-arroz.nome-abrev   = emitente.nome-abrev 
                      tt-movto-arroz.it-codigo    = "0007"
                      tt-movto-arroz.dt-trans     = tt-est-ins.dt-impl.
                end. /* not avail tt-movto-arroz */
                ASSIGN tt-movto-arroz.secagem   = tt-movto-arroz.secagem   + tt-est-ins.de-qt-secar
                       
                       tt-movto-arroz.liquidar  = tt-movto-arroz.liquidar  + de-qt-007 - de-qt-007bloq
                       tt-movto-arroz.atual     = tt-movto-arroz.atual     + de-qt-007 - de-a-secar-0007.
            END. /* item 0007 */
    
            IF tt-est-ins.de-qt-015 <> 0 THEN DO:
                
                FIND FIRST emitente WHERE
                           emitente.cod-emitente = tt-est-ins.cod-emitente 
                           NO-LOCK NO-ERROR.
                find first tt-movto-arroz where
                         tt-movto-arroz.cod-estabel  = tt-est-ins.cod-estabel and
                         tt-movto-arroz.cod-emitente = tt-est-ins.cod-emitente AND
                         tt-movto-arroz.ins-estadual = emitente.ins-estadual and
                         tt-movto-arroz.nome-abrev   = emitente.nome-abrev and
                         tt-movto-arroz.it-codigo    = "0015"
                         no-lock USE-INDEX unico no-error.
                if not avail tt-movto-arroz then do:               
                    create tt-movto-arroz.
                    assign      
                      tt-movto-arroz.cod-estabel  = tt-est-ins.cod-estabel 
                      tt-movto-arroz.cod-emitente = tt-est-ins.cod-emitente
                      tt-movto-arroz.ins-estadual = emitente.ins-estadual 
                      tt-movto-arroz.nome-abrev   = emitente.nome-abrev 
                      tt-movto-arroz.it-codigo    = "0015"
                      tt-movto-arroz.dt-trans     = tt-est-ins.dt-impl.
                end. /* not avail tt-movto-arroz */
                ASSIGN tt-movto-arroz.secagem   = tt-movto-arroz.secagem   + tt-est-ins.de-qt-secar
                       
                       tt-movto-arroz.liquidar  = tt-movto-arroz.liquidar  + de-qt-015 - de-qt-015bloq
                       tt-movto-arroz.atual     = tt-movto-arroz.atual     + de-qt-015 - de-a-secar-0015.
            END. /* item 0015 */
    
            IF tt-est-ins.de-qt-040 <> 0 THEN DO: 
                
                FIND FIRST emitente WHERE
                           emitente.cod-emitente = tt-est-ins.cod-emitente 
                           NO-LOCK NO-ERROR.
                find first tt-movto-arroz where
                         tt-movto-arroz.cod-estabel  = tt-est-ins.cod-estabel and
                         tt-movto-arroz.cod-emitente = tt-est-ins.cod-emitente AND
                         tt-movto-arroz.ins-estadual = emitente.ins-estadual and
                         tt-movto-arroz.nome-abrev   = emitente.nome-abrev and
                         tt-movto-arroz.it-codigo    = "0040"
                         no-lock USE-INDEX unico no-error.
                if not avail tt-movto-arroz then do:               
                    create tt-movto-arroz.
                    assign      
                      tt-movto-arroz.cod-estabel  = tt-est-ins.cod-estabel 
                      tt-movto-arroz.cod-emitente = tt-est-ins.cod-emitente
                      tt-movto-arroz.ins-estadual = emitente.ins-estadual 
                      tt-movto-arroz.nome-abrev   = emitente.nome-abrev 
                      tt-movto-arroz.it-codigo    = "0040"
                      tt-movto-arroz.dt-trans     = tt-est-ins.dt-impl.
                end. /* not avail tt-movto-arroz */
                ASSIGN tt-movto-arroz.secagem   = tt-movto-arroz.secagem   + tt-est-ins.de-qt-secar
                       tt-movto-arroz.liquidar  = tt-movto-arroz.liquidar  + de-qt-040 - de-qt-040bloq
                       tt-movto-arroz.atual     = tt-movto-arroz.atual     + de-qt-040 - de-a-secar-0040.
            END. /* item 0040 */
    
            IF tt-est-ins.de-qt-020 <> 0 THEN DO: 
                
                FIND FIRST emitente WHERE
                           emitente.cod-emitente = tt-est-ins.cod-emitente 
                           NO-LOCK NO-ERROR.
                find first tt-movto-arroz where
                         tt-movto-arroz.cod-estabel  = tt-est-ins.cod-estabel and
                         tt-movto-arroz.cod-emitente = tt-est-ins.cod-emitente AND
                         tt-movto-arroz.ins-estadual = emitente.ins-estadual and
                         tt-movto-arroz.nome-abrev   = emitente.nome-abrev and
                         tt-movto-arroz.it-codigo    = "0020"
                         no-lock USE-INDEX unico no-error.
                if not avail tt-movto-arroz then do:               
                    create tt-movto-arroz.
                    assign      
                      tt-movto-arroz.cod-estabel  = tt-est-ins.cod-estabel 
                      tt-movto-arroz.cod-emitente = tt-est-ins.cod-emitente
                      tt-movto-arroz.ins-estadual = emitente.ins-estadual 
                      tt-movto-arroz.nome-abrev   = emitente.nome-abrev 
                      tt-movto-arroz.it-codigo    = "0020"
                      tt-movto-arroz.dt-trans     = tt-est-ins.dt-impl.
                end. /* not avail tt-movto-arroz */
                ASSIGN tt-movto-arroz.secagem   = tt-movto-arroz.secagem   + tt-est-ins.de-qt-secar
                       tt-movto-arroz.liquidar  = tt-movto-arroz.liquidar  + de-qt-020 - de-qt-020bloq
                       tt-movto-arroz.atual     = tt-movto-arroz.atual     + de-qt-020 - de-a-secar-0020.
            END. /* item 0020 */
    
            IF tt-est-ins.de-qt-036 <> 0 THEN DO:
    
                
                FIND FIRST emitente WHERE
                           emitente.cod-emitente = tt-est-ins.cod-emitente 
                           NO-LOCK NO-ERROR.
                find first tt-movto-arroz where
                         tt-movto-arroz.cod-estabel  = tt-est-ins.cod-estabel and
                         tt-movto-arroz.cod-emitente = tt-est-ins.cod-emitente AND
                         tt-movto-arroz.ins-estadual = emitente.ins-estadual and
                         tt-movto-arroz.nome-abrev   = emitente.nome-abrev and
                         tt-movto-arroz.it-codigo    = "0036"
                         no-lock USE-INDEX unico no-error.
                if not avail tt-movto-arroz then do:               
                    create tt-movto-arroz.
                    assign      
                      tt-movto-arroz.cod-estabel  = tt-est-ins.cod-estabel 
                      tt-movto-arroz.cod-emitente = tt-est-ins.cod-emitente
                      tt-movto-arroz.ins-estadual = emitente.ins-estadual 
                      tt-movto-arroz.nome-abrev   = emitente.nome-abrev 
                      tt-movto-arroz.it-codigo    = "0036"
                      tt-movto-arroz.dt-trans     = tt-est-ins.dt-impl.
                end. /* not avail tt-movto-arroz */
                ASSIGN tt-movto-arroz.secagem   = tt-movto-arroz.secagem   + tt-est-ins.de-qt-secar
                       tt-movto-arroz.liquidar  = tt-movto-arroz.liquidar  + de-qt-036 - de-qt-036bloq
                       tt-movto-arroz.atual     = tt-movto-arroz.atual     + de-qt-036 - de-a-secar-0036.
            END. /* item 0036 */
        

            /* quantidade bloqueada por item no browse */
            FIND FIRST emitente WHERE
                       emitente.cod-emitente = tt-est-ins.cod-emitente 
                       NO-LOCK NO-ERROR.
            find first tt-movto-arroz where
                     tt-movto-arroz.cod-estabel  = tt-est-ins.cod-estabel and
                     tt-movto-arroz.cod-emitente = tt-est-ins.cod-emitente AND
                     tt-movto-arroz.ins-estadual = emitente.ins-estadual and
                     tt-movto-arroz.nome-abrev   = emitente.nome-abrev and
                     tt-movto-arroz.it-codigo    = "0001"
                     no-lock USE-INDEX unico no-error.
            if NOT avail tt-movto-arroz then do: 
                create tt-movto-arroz.
                assign      
                      tt-movto-arroz.cod-estabel  = tt-est-ins.cod-estabel 
                      tt-movto-arroz.cod-emitente = tt-est-ins.cod-emitente
                      tt-movto-arroz.ins-estadual = emitente.ins-estadual 
                      tt-movto-arroz.nome-abrev   = emitente.nome-abrev 
                      tt-movto-arroz.it-codigo    = "0001"
                      tt-movto-arroz.dt-trans     = tt-est-ins.dt-impl.
            END.
            if avail tt-movto-arroz then do:       
                ASSIGN tt-movto-arroz.bloqueada = tt-movto-arroz.bloqueada - tt-est-ins.de-qt-001bloq.
            END.
            
    
            FIND FIRST emitente WHERE
                       emitente.cod-emitente = tt-est-ins.cod-emitente 
                       NO-LOCK NO-ERROR.
            find first tt-movto-arroz where
                     tt-movto-arroz.cod-estabel  = tt-est-ins.cod-estabel and
                     tt-movto-arroz.cod-emitente = tt-est-ins.cod-emitente AND
                     tt-movto-arroz.ins-estadual = emitente.ins-estadual and
                     tt-movto-arroz.nome-abrev   = emitente.nome-abrev and
                     tt-movto-arroz.it-codigo    = "0002"
                     no-lock USE-INDEX unico no-error.
            if NOT avail tt-movto-arroz then do: 
                create tt-movto-arroz.
                assign      
                      tt-movto-arroz.cod-estabel  = tt-est-ins.cod-estabel 
                      tt-movto-arroz.cod-emitente = tt-est-ins.cod-emitente
                      tt-movto-arroz.ins-estadual = emitente.ins-estadual 
                      tt-movto-arroz.nome-abrev   = emitente.nome-abrev 
                      tt-movto-arroz.it-codigo    = "0002"
                      tt-movto-arroz.dt-trans     = tt-est-ins.dt-impl.
            END.
            if avail tt-movto-arroz then do:       
                ASSIGN tt-movto-arroz.bloqueada = tt-movto-arroz.bloqueada - tt-est-ins.de-qt-002bloq.
    
            END.
            
    
            FIND FIRST emitente WHERE
                       emitente.cod-emitente = tt-est-ins.cod-emitente 
                       NO-LOCK NO-ERROR.
            find first tt-movto-arroz where
                     tt-movto-arroz.cod-estabel  = tt-est-ins.cod-estabel and
                     tt-movto-arroz.cod-emitente = tt-est-ins.cod-emitente AND
                     tt-movto-arroz.ins-estadual = emitente.ins-estadual and
                     tt-movto-arroz.nome-abrev   = emitente.nome-abrev and
                     tt-movto-arroz.it-codigo    = "0004"
                     no-lock USE-INDEX unico no-error.
            if NOT avail tt-movto-arroz then do: 
                create tt-movto-arroz.
                assign      
                      tt-movto-arroz.cod-estabel  = tt-est-ins.cod-estabel 
                      tt-movto-arroz.cod-emitente = tt-est-ins.cod-emitente
                      tt-movto-arroz.ins-estadual = emitente.ins-estadual 
                      tt-movto-arroz.nome-abrev   = emitente.nome-abrev 
                      tt-movto-arroz.it-codigo    = "0004"
                      tt-movto-arroz.dt-trans     = tt-est-ins.dt-impl.
            END.
            if avail tt-movto-arroz then do:       
                ASSIGN tt-movto-arroz.bloqueada = tt-movto-arroz.bloqueada - tt-est-ins.de-qt-004bloq.
    
            END.
    
            FIND FIRST emitente WHERE
                       emitente.cod-emitente = tt-est-ins.cod-emitente 
                       NO-LOCK NO-ERROR.
            find first tt-movto-arroz where
                     tt-movto-arroz.cod-estabel  = tt-est-ins.cod-estabel and
                     tt-movto-arroz.cod-emitente = tt-est-ins.cod-emitente AND
                     tt-movto-arroz.ins-estadual = emitente.ins-estadual and
                     tt-movto-arroz.nome-abrev   = emitente.nome-abrev and
                     tt-movto-arroz.it-codigo    = "0007"
                     no-lock USE-INDEX unico no-error.
            if NOT avail tt-movto-arroz then do: 
                create tt-movto-arroz.
                assign      
                      tt-movto-arroz.cod-estabel  = tt-est-ins.cod-estabel 
                      tt-movto-arroz.cod-emitente = tt-est-ins.cod-emitente
                      tt-movto-arroz.ins-estadual = emitente.ins-estadual 
                      tt-movto-arroz.nome-abrev   = emitente.nome-abrev 
                      tt-movto-arroz.it-codigo    = "0007"
                      tt-movto-arroz.dt-trans     = tt-est-ins.dt-impl.
            END.
            if avail tt-movto-arroz then do:       
                ASSIGN tt-movto-arroz.bloqueada = tt-movto-arroz.bloqueada - tt-est-ins.de-qt-007bloq.
    
            END.
    
            FIND FIRST emitente WHERE
                       emitente.cod-emitente = tt-est-ins.cod-emitente 
                       NO-LOCK NO-ERROR.
            find first tt-movto-arroz where
                     tt-movto-arroz.cod-estabel  = tt-est-ins.cod-estabel and
                     tt-movto-arroz.cod-emitente = tt-est-ins.cod-emitente AND
                     tt-movto-arroz.ins-estadual = emitente.ins-estadual and
                     tt-movto-arroz.nome-abrev   = emitente.nome-abrev and
                     tt-movto-arroz.it-codigo    = "0015"
                     no-lock USE-INDEX unico no-error.
            if NOT avail tt-movto-arroz then do: 
                create tt-movto-arroz.
                assign      
                      tt-movto-arroz.cod-estabel  = tt-est-ins.cod-estabel 
                      tt-movto-arroz.cod-emitente = tt-est-ins.cod-emitente
                      tt-movto-arroz.ins-estadual = emitente.ins-estadual 
                      tt-movto-arroz.nome-abrev   = emitente.nome-abrev 
                      tt-movto-arroz.it-codigo    = "0015"
                      tt-movto-arroz.dt-trans     = tt-est-ins.dt-impl.
            END.
            if avail tt-movto-arroz then do:       
                ASSIGN tt-movto-arroz.bloqueada = tt-movto-arroz.bloqueada - tt-est-ins.de-qt-015bloq. 
    
            END.
    
            FIND FIRST emitente WHERE
                       emitente.cod-emitente = tt-est-ins.cod-emitente 
                       NO-LOCK NO-ERROR.
            find first tt-movto-arroz where
                     tt-movto-arroz.cod-estabel  = tt-est-ins.cod-estabel and
                     tt-movto-arroz.cod-emitente = tt-est-ins.cod-emitente AND
                     tt-movto-arroz.ins-estadual = emitente.ins-estadual and
                     tt-movto-arroz.nome-abrev   = emitente.nome-abrev and
                     tt-movto-arroz.it-codigo    = "0040"
                     no-lock USE-INDEX unico no-error.
            if NOT avail tt-movto-arroz then do: 
                create tt-movto-arroz.
                assign      
                      tt-movto-arroz.cod-estabel  = tt-est-ins.cod-estabel 
                      tt-movto-arroz.cod-emitente = tt-est-ins.cod-emitente
                      tt-movto-arroz.ins-estadual = emitente.ins-estadual 
                      tt-movto-arroz.nome-abrev   = emitente.nome-abrev 
                      tt-movto-arroz.it-codigo    = "0040"
                      tt-movto-arroz.dt-trans     = tt-est-ins.dt-impl.
            END.
            if avail tt-movto-arroz then do:       
                ASSIGN tt-movto-arroz.bloqueada = tt-movto-arroz.bloqueada - tt-est-ins.de-qt-040bloq.
    
            END.
    
            FIND FIRST emitente WHERE
                       emitente.cod-emitente = tt-est-ins.cod-emitente 
                       NO-LOCK NO-ERROR.
            find first tt-movto-arroz where
                     tt-movto-arroz.cod-estabel  = tt-est-ins.cod-estabel and
                     tt-movto-arroz.cod-emitente = tt-est-ins.cod-emitente AND
                     tt-movto-arroz.ins-estadual = emitente.ins-estadual and
                     tt-movto-arroz.nome-abrev   = emitente.nome-abrev and
                     tt-movto-arroz.it-codigo    = "0020"
                     no-lock USE-INDEX unico no-error.
            if NOT avail tt-movto-arroz then do: 
                create tt-movto-arroz.
                assign      
                      tt-movto-arroz.cod-estabel  = tt-est-ins.cod-estabel 
                      tt-movto-arroz.cod-emitente = tt-est-ins.cod-emitente
                      tt-movto-arroz.ins-estadual = emitente.ins-estadual 
                      tt-movto-arroz.nome-abrev   = emitente.nome-abrev 
                      tt-movto-arroz.it-codigo    = "0020"
                      tt-movto-arroz.dt-trans     = tt-est-ins.dt-impl.
            END.
            if avail tt-movto-arroz then do:       
                ASSIGN tt-movto-arroz.bloqueada = tt-movto-arroz.bloqueada - tt-est-ins.de-qt-020bloq.
    
            END.
    
            FIND FIRST emitente WHERE
                       emitente.cod-emitente = tt-est-ins.cod-emitente 
                       NO-LOCK NO-ERROR.
            find first tt-movto-arroz where
                     tt-movto-arroz.cod-estabel  = tt-est-ins.cod-estabel and
                     tt-movto-arroz.cod-emitente = tt-est-ins.cod-emitente AND
                     tt-movto-arroz.ins-estadual = emitente.ins-estadual and
                     tt-movto-arroz.nome-abrev   = emitente.nome-abrev and
                     tt-movto-arroz.it-codigo    = "0036"
                     no-lock USE-INDEX unico no-error.
            if NOT avail tt-movto-arroz then do: 
                create tt-movto-arroz.
                assign      
                      tt-movto-arroz.cod-estabel  = tt-est-ins.cod-estabel 
                      tt-movto-arroz.cod-emitente = tt-est-ins.cod-emitente
                      tt-movto-arroz.ins-estadual = emitente.ins-estadual 
                      tt-movto-arroz.nome-abrev   = emitente.nome-abrev 
                      tt-movto-arroz.it-codigo    = "0036"
                      tt-movto-arroz.dt-trans     = tt-est-ins.dt-impl.
            END.
            if avail tt-movto-arroz then do:       
                ASSIGN tt-movto-arroz.bloqueada = tt-movto-arroz.bloqueada - tt-est-ins.de-qt-036bloq .
            END.
        END. /* browse por item */  

  END. /* tt-est-ins */

    
  
    FOR EACH tt-movto-arroz:
          IF tt-movto-arroz.secagem   = 0 AND
             tt-movto-arroz.bloqueada = 0 AND
             tt-movto-arroz.liquidar  = 0 AND
             tt-movto-arroz.atual     = 0  THEN DELETE tt-movto-arroz.
          ELSE 
            IF tt-movto-arroz.liquidar <= 0 THEN tt-movto-arroz.liquidar = 0.
    END.  /* for each tt-movto-arroz */

 
    
   {&OPEN-QUERY-{&BROWSE-NAME}} 

   SESSION:SET-WAIT-STATE("":U).
   
 
        IF  i-operacao = 1 THEN DO: 
            
            IF de-tot-qt-saldo = 0 THEN DO:
        
                    MESSAGE  "1-Quantidade informada n∆o pode ser superior ao saldo total a dep¢sito: 0" 
                        VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                    APPLY 'entry' TO de-quantidade IN FRAME {&FRAME-NAME}.
                    RETURN NO-APPLY.
        
            END.
            ELSE DO:
                
                IF INPUT FRAME {&FRAME-NAME} de-quantidade > de-tot-qt-saldo THEN DO:
        
                    MESSAGE "2-Quantidade informada n∆o pode ser superior ao saldo total a dep¢sito: " + STRING(de-tot-qt-saldo) 
                            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                    APPLY 'entry' TO de-quantidade IN FRAME {&FRAME-NAME}.
                    RETURN NO-APPLY.
                END.
            END.
        
        END. /* compra arroz */
        ELSE DO:
            /* secagem */

    
            IF  INPUT FRAME {&FRAME-NAME} de-quantidade > de-acum-asecar THEN DO:
                MESSAGE "Quantidade informada n∆o pode ser superior ao saldo total de secagem: " + STRING(de-acum-asecar) 
                        VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                APPLY 'entry' TO de-quantidade IN FRAME {&FRAME-NAME}.
                RETURN NO-APPLY.
            END.
            
        END. /* secagem */

        /* bloco principal */
        DO:

            
        FIND LAST tt-cargas NO-LOCK no-error.
        IF AVAIL tt-cargas THEN DO:
        
            main-block-carga:
            FOR EACH tt-cargas NO-LOCK BY tt-cargas.nr-ticket:
        
                FIND FIRST es-ticket WHERE es-ticket.nr-ticket = tt-cargas.nr-ticket NO-LOCK NO-ERROR.   
                
                FIND FIRST saldo-terc WHERE
                           saldo-terc.cod-estabel  = es-ticket.cod-estabel    
                       AND saldo-terc.nro-docto    = es-ticket.nro-docto     
                       AND saldo-terc.serie        = es-ticket.serie
                       AND saldo-terc.cod-emitente = es-ticket.cod-emitente
                    NO-LOCK NO-ERROR.
            
                IF AVAIL es-ticket AND
                   AVAIL saldo-terc THEN DO:
            
                    IF  INPUT FRAME {&FRAME-NAME} c-it-codigo <> "" AND
                        saldo-terc.it-codigo <> INPUT FRAME {&FRAME-NAME} c-it-codigo THEN
                        NEXT Main-block-carga.
                
                 
                    FIND ITEM WHERE ITEM.it-codigo = saldo-terc.it-codigo NO-LOCK NO-ERROR.
                    IF  NOT AVAIL ITEM THEN NEXT Main-Block-carga.
        
                    IF  saldo-terc.quantidade < saldo-terc.dec-1 THEN NEXT Main-Block-carga.
                
                    FIND FIRST es-movto-arroz USE-INDEX ch-item-doc-est
                         WHERE es-movto-arroz.serie-docto  = saldo-terc.serie-docto   AND
                               es-movto-arroz.nro-docto    = saldo-terc.nro-docto     AND
                               es-movto-arroz.cod-emitente = saldo-terc.cod-emitente  AND  
                               es-movto-arroz.nat-operacao = saldo-terc.nat-operacao    
                        NO-LOCK NO-ERROR.
                    IF saldo-terc.nro-docto <> "9999999" then
                      IF NOT AVAIL es-movto-arroz THEN NEXT Main-Block-carga.
                    
                    if es-movto-arroz.cod-estabel <> i-cod-estab-4036 THEN NEXT main-block-carga.
                    if es-ticket.cod-estabel <> i-cod-estab-4036 THEN NEXT main-block-carga.
                    
                    
                    IF  INPUT FRAME {&FRAME-NAME} i-nr-contrato > 0 AND
                        es-movto-arroz.nr-contrato <> INPUT FRAME {&FRAME-NAME} i-nr-contrato  THEN NEXT Main-block-carga.
                
                
/*                     message "6 carga achou saldo-terc"                                                 */
/*                       "saldo-terc.cod-emitente" saldo-terc.cod-emitente                                */
/*                                 "WHERE componente.serie-docto " saldo-terc.serie-docto                 */
/*                             "ERIIIIIIIIIII-docto    = " saldo-terc.nro-docto                           */
/*                             "AND   componente.cod-emitente =" saldo-terc.cod-emitente                  */
/*                             "AND   componente.nat-operacao =" saldo-terc.nat-operacao                  */
/*                             "AND   componente.it-codigo    =" saldo-terc.it-codigo                     */
/*                             "AND   componente.cod-refer    =" saldo-terc.cod-refer                     */
/*                             "AND   componente.sequencia     " saldo-terc.sequencia  SKIP               */
/*                          "inscricao bf-emitente-api.ins-estadual "  bf-emitente-api.ins-estadual  SKIP */
/*                          "es-movto-arroz.ins-estadual " es-movto-arroz.ins-estadual                    */
/*                     view-as ALERT-BOX.                                                                 */
                
                
                    FIND FIRST bf-emitente-api WHERE
                               bf-emitente-api.cod-emitente = c-cod-emitente-esapi007a-36
                               NO-LOCK NO-ERROR.
                    IF NOT AVAIL bf-emitente-api THEN do:
                        MESSAGE "N∆o encontrou bf-emitente-api para " c-cod-emitente-esapi007a-36 VIEW-AS ALERT-BOX.
                    END.
        
                    IF  bf-emitente-api.ins-estadual <> "" AND                                              
                        es-movto-arroz.ins-estadual  <> bf-emitente-api.ins-estadual THEN next.  
                
                    IF  c-cod-estabel-esapi007a-36 <> "" AND
                        es-movto-arroz.cod-estabel <> c-cod-estabel-esapi007a-36 THEN NEXT.
                        
                    /* 100804  interpretar o de-a-asecar pelo tt-extrato*/
                    RUN esp/esapi007a.p (INPUT  es-movto-arroz.nr-contrato,
                                         INPUT  es-movto-arroz.nr-ticket,
                                         INPUT  es-movto-arroz.nr-ticket,
                                         INPUT  bf-emitente-api.ins-estadual,
                                         INPUT  c-cod-estabel-esapi007a-36,
                                         OUTPUT de-entra,
                                         OUTPUT de-saida,
                                         OUTPUT de-tot-saldo,
                                         OUTPUT de-tot-secag,
                                         OUTPUT de-tot-seco,
                                         OUTPUT de-a-secar,
                                         OUTPUT vl-tot-secag,
                                         OUTPUT vl-tot-seco,
                                         OUTPUT vl-a-secar).
                    
                    FIND FIRST tt-extrato WHERE tt-extrato.nr-ticket = es-movto-arroz.nr-ticket NO-LOCK NO-ERROR.
                    IF AVAIL tt-extrato THEN DO:
                        ASSIGN de-a-secar = tt-extrato.desc-secagem.
                    END.

                    ASSIGN de-tot-seco-comp = 0.
                    /* 120406 - rotina para achar o valor seco DO saldo-terceiros em quest∆o */
                    FOR EACH bs-componente WHERE
                             bs-componente.cod-emitente = saldo-terc.cod-emitente AND
                             bs-componente.serie-comp   = saldo-terc.serie-docto AND
                             bs-componente.nro-comp     = saldo-terc.nro-docto  and
                             bs-componente.nat-comp     = saldo-terc.nat-oper USE-INDEX comp:

                         FIND FIRST nota-fiscal WHERE
                                    nota-fiscal.cod-estabel = saldo-terc.cod-estabel AND
                                    nota-fiscal.serie       = bs-componente.serie-docto AND
                                    nota-fiscal.nr-nota-fis = bs-componente.nro-docto.
                         IF AVAIL nota-fiscal and
                            SUBSTR(nota-fiscal.observ-nota,1900,7) = "secagem" THEN do:
                             FOR EACH it-nota-fisc OF nota-fiscal:
                               IF it-nota-fisc.conh-fre = STRING(es-movto-arroz.nr-ticket) THEN DO: /* conhecto de frete Ç o nro do ticket */
                                  ASSIGN de-tot-seco-comp = de-tot-seco-comp + it-nota-fisc.qt-faturada[1].
                               END.
                             END.

                         END.

                    END.
                                    
        
                    IF de-acum-asecar < 0.01 THEN ASSIGN de-a-secar = 0. 
                
                   /***** /* 040406 */
                   ASSIGN de-a-secar = de-a-secar - de-tot-seco.*****/


/*                     ASSIGN de-a-secar = de-acum-asecar. /* 13/03/2012 */ */
/*                     MESSAGE "aqui" de-a-secar                            */
/*                         VIEW-AS ALERT-BOX INFO BUTTONS OK.               */



                
/*                 message "7 carga                                                                                  */
/*                      achou saldo-terc"                                                                            */
/*                     de-quantidade                                                                                 */
/*                   "saldo-terc.cod-emitente" saldo-terc.cod-emitente                                               */
/*                             "WHERE componente.serie-docto " saldo-terc.serie-docto                                */
/*                         "ERIIIIIIIIIII-docto    = " saldo-terc.nro-docto                                          */
/*                         "AND   componente.cod-emitente =" saldo-terc.cod-emitente                                 */
/*                         "AND   componente.nat-operacao =" saldo-terc.nat-operacao                                 */
/*                         "AND   componente.it-codigo    =" saldo-terc.it-codigo                                    */
/*                         "AND   componente.cod-refer    =" saldo-terc.cod-refer                                    */
/*                         "AND   componente.sequencia     " saldo-terc.sequencia  skip(3)                           */
/*                          "oper" i-operacao                                                                        */
/*                         "recid" recid(saldo-terc) "saldo" (saldo-terc.quantidade - de-a-secar - saldo-terc.dec-1) */
/*                         "de-asecar erika" de-a-secar skip                                                         */
/*                         "aloca" saldo-terc.dec-1                                                                  */
/*                         "saldo" (saldo-terc.quantidade - de-a-secar - saldo-terc.dec-1)                           */
/*                         "quant" saldo-terc.quantidade                                                             */
/*                         "a secar erika 2"  de-a-secar                                                             */
/*                         " alocado" saldo-terc.dec-1 SKIP                                                          */
/*                             "ticket" es-movto-arroz.nr-ticket       SKIP                                          */
/*                              "tot secag" de-tot-secag                                                             */
/*                                                 "tot seco" de-tot-seco "NOVO"                                     */
/*                         view-as alert-box.                                                                        */
                
                    
                    /* 110406 */
                    /* porque teria pego somente o total seco, e aqui pelo de-tot-seco-comp pegou somente o seco para o ticket */
                
                    IF de-a-secar <> 0 THEN 
                            ASSIGN de-tot-seco = de-tot-seco-comp
                                   de-a-secar  = de-a-secar - de-tot-seco.
                   
                
                    IF i-operacao = 1 THEN DO:
                        IF (saldo-terc.quantidade - de-a-secar - saldo-terc.dec-1) < 0.01 OR saldo-terc.quantidade <= 0.01 THEN NEXT main-block-carga.
                    END.
                        
                    ELSE DO:
                        IF de-a-secar <= 0 OR saldo-terc.quantidade <= 0.01 THEN NEXT main-block-carga.                
                    END.

                    /* 240507 */
                    IF de-a-secar < 0 THEN ASSIGN de-a-secar = 0.
                    IF i-operacao  = 1 AND (saldo-terc.quantidade - de-a-secar) = 0 THEN NEXT main-block-carga.
                    IF i-operacao <> 1 AND de-a-secar = 0 THEN NEXT main-block-carga.
                
                    FOR FIRST componente USE-INDEX documento
                        WHERE componente.serie-docto  = saldo-terc.serie-docto
                        AND   componente.nro-docto    = saldo-terc.nro-docto
                        AND   componente.cod-emitente = saldo-terc.cod-emitente
                        AND   componente.nat-operacao = saldo-terc.nat-operacao
                        AND   componente.it-codigo    = saldo-terc.it-codigo
                        AND   componente.cod-refer    = saldo-terc.cod-refer
                        AND   componente.sequencia    = saldo-terc.sequencia NO-LOCK:
                
                        IF  i-operacao = 1 THEN DO:
                
                            IF  (saldo-terc.quantidade /**/ - de-a-secar /*280405 */ - saldo-terc.dec-1) >= de-quantidade - (de-quantidade - de-quant-dev) THEN DO:
                                ASSIGN de-quant-aux = de-quantidade - (de-quantidade - de-quant-dev)
                                       de-quant-dev = de-quant-dev  - (de-quantidade - (de-quantidade - de-quant-dev)).
                                
                            END.
                            ELSE DO:
                                ASSIGN de-quant-aux = (saldo-terc.quantidade /**/ - de-a-secar /*280405 */ - saldo-terc.dec-1)
                                       de-quant-dev = de-quant-dev - (saldo-terc.quantidade - saldo-terc.dec-1 /**/ - de-a-secar /*280405 */ ).
                            END.
                        END.
                        ELSE DO:
                            IF  de-a-secar >= de-quantidade - (de-quantidade - de-quant-dev) THEN DO:
                                ASSIGN de-quant-aux = de-quantidade - (de-quantidade - de-quant-dev)
                                       de-quant-dev = de-quant-dev  - de-quant-aux.
                            END.
                            ELSE DO:
                                ASSIGN de-quant-aux = de-a-secar
                                       de-quant-dev = de-quant-dev - de-a-secar.
                            END.
                        END.
                
                     
                        FIND LAST b-tt-it-terc-nf USE-INDEX codigo 
                            NO-LOCK NO-ERROR.
                        IF  AVAIL b-tt-it-terc-nf THEN
                            ASSIGN i-sequencia = b-tt-it-terc-nf.sequencia + 10.
                        ELSE
                            ASSIGN i-sequencia = 10.
                                 
                            CREATE tt-it-terc-nf.
                            ASSIGN tt-it-terc-nf.rw-saldo-terc     = ROWID(saldo-terc)
                                   tt-it-terc-nf.sequencia         = i-sequencia
                                   tt-it-terc-nf.it-codigo         = saldo-terc.it-codigo
                                   tt-it-terc-nf.cod-refer         = saldo-terc.cod-refer
                                   tt-it-terc-nf.desc-nar          = IF  item.tipo-contr = 4 /* D≤bito Direto */
                                                                     THEN SUBSTR(item.narrativa,1,60)
                                                                     ELSE item.desc-item
                                   tt-it-terc-nf.quantidade        = IF  i-operacao = 1 
                                                                         THEN round((saldo-terc.quantidade - de-a-secar),2)
                                                                         ELSE round(de-tot-secag,2)
                                   tt-it-terc-nf.qt-alocada        = IF  i-operacao = 1  
                                                                         THEN round(saldo-terc.dec-1,2)
                                                                         ELSE round(de-tot-seco,2)
                                   tt-it-terc-nf.qt-disponivel     = IF  i-operacao = 1 
                                                                         THEN round((saldo-terc.quantidade - de-a-secar - saldo-terc.dec-1),2)
                                                                         ELSE round(de-a-secar,2)
                                   tt-it-terc-nf.qt-disponivel-inf = round(de-quant-aux,2)
                                                                             /*
                                   tt-it-terc-nf.qt-disponivel-inf = tt-it-terc-nf.qt-disponivel
                                   */
                                   tt-it-terc-nf.preco-total       = componente.preco-total[1]
                                   tt-it-terc-nf.selecionado       = NO
                                   tt-it-terc-nf.qt-faturada       = tt-it-terc-nf.qt-disponivel-inf                                         
                                   tt-it-terc-nf.un-faturada       = ITEM.un
                                   tt-it-terc-nf.un-estoque        = ITEM.un.
                
                        IF  i-operacao = 1 THEN do:
                           IF de-quant-aux > saldo-terc.quantidade /**/ - de-a-secar  /*280405 */  THEN DO:
                              assign  tt-it-terc-nf.quantidade = round(tt-it-terc-nf.qt-disponivel-inf,2).
                           END.
                        END.
                    
                        find first b-componente USE-INDEX comp
                              WHERE b-componente.cod-emitente = componente.cod-emitente 
                              AND   b-componente.nro-comp     = componente.nro-docto    
                              AND   b-componente.serie-comp   = componente.serie-docto  
                              AND   b-componente.nat-comp     = componente.nat-operacao 
                              AND   b-componente.it-codigo    = componente.it-codigo    
                              AND   b-componente.cod-refer    = componente.cod-refer   
                              AND   b-componente.seq-comp     = componente.sequencia 
                              AND   (   b-componente.quantidade > 0
                                     OR b-componente.dt-retorno = dt-trans-esapi007a-36) NO-LOCK NO-ERROR. /* NF Reajuste ate Dt Trans */ 
                        if avail b-componente then do:
                            IF  b-componente.componente = 1 then do: /* Envio */
                                ASSIGN tt-it-terc-nf.preco-total = tt-it-terc-nf.preco-total + b-componente.preco-total[1].        
                            end.
                            ELSE do: /* Retorno */
                                ASSIGN tt-it-terc-nf.preco-total = tt-it-terc-nf.preco-total - b-componente.preco-total[1].                        
                            end.    
                        END.
                
                        ASSIGN tt-it-terc-nf.preco-total-inf = tt-it-terc-nf.preco-total.
                
                        IF  de-quant-dev <= 0 THEN LEAVE Main-Block-carga.
                
                    END. /* FOR FIRST componente */
                END. /* avail saldo-terc e ticket */
            END. /* for each */
        END. /* if avail tt-carg */
        
            
            
            
        ELSE DO:
             
        /* NORMAL FIFO e n∆o pelo carga-a-carga */
        Main-Block:
        FOR EACH saldo-terc 
              WHERE saldo-terc.cod-emitente  = i-cod-emit-4036           AND
                    /*saldo-terc.it-codigo     = INPUT FRAME {&FRAME-NAME} c-it-codigo AND */
                    saldo-terc.quantidade    > 0                             AND
                    saldo-terc.tipo-sal-terc = 5  /* ENTRADA CONSIGNACAO */  NO-LOCK,
            FIRST es-ticket NO-LOCK                               
            WHERE (es-ticket.cod-estabel        = saldo-terc.cod-estabel                                 
                   AND  es-ticket.nro-docto     = saldo-terc.nro-docto
                   AND  es-ticket.serie         = saldo-terc.serie 
                   AND es-ticket.cod-emitente   = saldo-terc.cod-emitente) 
            BY es-ticket.nr-ticket:

         
        
/*               message "1 achou saldo-terc" SKIP                       */
/*                                                                       */
/*                     "de-quantidade" de-quantidade                     */
/*                     "saldo-terc.cod-emitente" saldo-terc.cod-emitente */
/*                     " saldo-terc.quantidade" saldo-terc.quantidade    */
/*                     "saldo-terc.DEC-1" saldo-terc.DEC-1               */
/*                     "oper" i-operacao                                 */
/*                     view-as alert-box.                                */
               
            IF  INPUT FRAME {&FRAME-NAME} c-it-codigo <> "" AND
                saldo-terc.it-codigo <> INPUT FRAME {&FRAME-NAME} c-it-codigo THEN NEXT Main-block.
        
/*            message "2 achou saldo-terc"                                    */
/*           "saldo-terc.cod-emitente" saldo-terc.cod-emitente                */
/*                     "WHERE componente.serie-docto " saldo-terc.serie-docto */
/*                 "AND   componente.nro-docto    = " saldo-terc.nro-docto    */
/*                 "AND   componente.cod-emitente =" saldo-terc.cod-emitente  */
/*                 "AND   componente.nat-operacao =" saldo-terc.nat-operacao  */
/*                 "AND   componente.it-codigo    =" saldo-terc.it-codigo     */
/*                 "AND   componente.cod-refer    =" saldo-terc.cod-refer     */
/*                 "AND   componente.sequencia     " saldo-terc.sequencia     */
/*         view-as alert-box.                                                 */
        
        
            FIND ITEM
                WHERE ITEM.it-codigo = saldo-terc.it-codigo
                NO-LOCK NO-ERROR.
            IF  NOT AVAIL ITEM THEN NEXT Main-Block.
                
/*           message "3 achou saldo-terc"                                     */
/*           "saldo-terc.cod-emitente" saldo-terc.cod-emitente                */
/*                     "WHERE componente.serie-docto " saldo-terc.serie-docto */
/*                 "AND   componente.nro-docto    = " saldo-terc.nro-docto    */
/*                 "AND   componente.cod-emitente =" saldo-terc.cod-emitente  */
/*                 "AND   componente.nat-operacao =" saldo-terc.nat-operacao  */
/*                 "AND   componente.it-codigo    =" saldo-terc.it-codigo     */
/*                 "AND   componente.cod-refer    =" saldo-terc.cod-refer     */
/*                 "AND   componente.sequencia     " saldo-terc.sequencia     */
/*         view-as alert-box.                                                 */
        
            
            IF  saldo-terc.quantidade < saldo-terc.dec-1 THEN NEXT Main-Block.
            
        
/*                     MESSAGE "saldo-terc dados para encontrar o es-movto-arroz" */
/*                        "saldo-terc.serie-docto "  saldo-terc.serie-docto       */
/*                        "saldo-terc.nro-docto   "  saldo-terc.nro-docto         */
/*                        "saldo-terc.cod-emitente"  saldo-terc.cod-emitente      */
/*                        "saldo-terc.it-codigo   "  saldo-terc.it-codigo         */
/*                        "saldo-terc.nat-operacao " saldo-terc.nat-operacao      */
/*                         VIEW-AS alert-box.                                     */
        
        
        
            FIND FIRST es-movto-arroz USE-INDEX ch-item-doc-est
                 WHERE es-movto-arroz.serie-docto  = saldo-terc.serie-docto   AND
                       es-movto-arroz.nro-docto    = saldo-terc.nro-docto     AND
                       es-movto-arroz.cod-emitente = saldo-terc.cod-emitente  AND   
                       es-movto-arroz.nat-operacao = saldo-terc.nat-operacao    
                NO-LOCK NO-ERROR.
            IF saldo-terc.nro-docto <> "9999999" then
            /*IF  NOT AVAIL es-movto-arroz THEN NEXT Main-Block.*/
        
            
            if AVAIL es-movto-arroz AND es-movto-arroz.cod-estabel <> i-cod-estab-4036 then NEXT main-block.
            if es-ticket.cod-estabel <> i-cod-estab-4036 then NEXT main-block.
            
        
/*            message "5                                                           */
/*                CON achou saldo-terc" es-movto-arroz.nr-contrato  skip           */
/*         es-movto-arroz.nr-ticket                                                */
/*         es-movto-arroz.nr-contrato                                              */
/*           saldo-terc.quantidade                                                 */
/*           "saldo-terc.cod-emitente" saldo-terc.cod-emitente                     */
/*                     "WHERE componente.serie-docto " saldo-terc.serie-docto      */
/*                 "new ESSE AQUI componente.nro-docto    = " saldo-terc.nro-docto */
/*                 "AND   componente.cod-emitente =" saldo-terc.cod-emitente       */
/*                 "AND   componente.nat-operacao =" saldo-terc.nat-operacao       */
/*                 "AND   componente.it-codigo    =" saldo-terc.it-codigo          */
/*                 "AND   componente.cod-refer    =" saldo-terc.cod-refer          */
/*                 "AND   componente.sequencia     " saldo-terc.sequencia          */
/*         "recid"        recid(es-movto-arroz)                                    */
/*         skip(3)                                                                 */
/*         i-nr-contrato                                                           */
/*         es-movto-arroz.nr-contrato SKIP(3)                                      */
/*         INPUT FRAME {&FRAME-NAME} i-nr-contrato                                 */
/*         es-movto-arroz.nr-contrato                                              */
/*         view-as alert-box.                                                      */
        
            
           IF AVAIL es-movto-arroz THEN DO: 
                IF  INPUT FRAME {&FRAME-NAME} i-nr-contrato > 0 AND
                es-movto-arroz.nr-contrato <> INPUT FRAME {&FRAME-NAME} i-nr-contrato  THEN NEXT Main-block.
           END.
           ELSE DO:
                IF  INPUT FRAME {&FRAME-NAME} i-nr-contrato > 0 AND
                es-ticket.nr-contrato <> INPUT FRAME {&FRAME-NAME} i-nr-contrato  THEN NEXT Main-block.
           END.
        
/*         message "6 achou saldo-terc"                                                       */
/*           "saldo-terc.cod-emitente" saldo-terc.cod-emitente                                */
/*                     "WHERE componente.serie-docto " saldo-terc.serie-docto                 */
/*                 "ERIIIIIIIIIII-docto    = " saldo-terc.nro-docto                           */
/*                 "AND   componente.cod-emitente =" saldo-terc.cod-emitente                  */
/*                 "AND   componente.nat-operacao =" saldo-terc.nat-operacao                  */
/*                 "AND   componente.it-codigo    =" saldo-terc.it-codigo                     */
/*                 "AND   componente.cod-refer    =" saldo-terc.cod-refer                     */
/*                 "AND   componente.sequencia     " saldo-terc.sequencia  SKIP               */
/*              "inscricao bf-emitente-api.ins-estadual "  bf-emitente-api.ins-estadual  SKIP */
/*              "es-movto-arroz.ins-estadual " es-movto-arroz.ins-estadual                    */
/*         view-as ALERT-BOX.                                                                 */
        
            FIND FIRST bf-emitente-api WHERE
                       bf-emitente-api.cod-emitente = c-cod-emitente-esapi007a-36
                       NO-LOCK NO-ERROR.
            IF NOT AVAIL bf-emitente-api THEN do:
                MESSAGE "N∆o encontrou bf-emitente-api para " c-cod-emitente-esapi007a-36 VIEW-AS ALERT-BOX.
            END.

            IF AVAIL es-movto-arroz THEN do:
                IF  bf-emitente-api.ins-estadual <> "" AND                                              
                es-movto-arroz.ins-estadual <> bf-emitente-api.ins-estadual THEN next.  
            END.
            ELSE DO:
                IF  bf-emitente-api.ins-estadual <> "" AND                                              
                es-ticket.ins-estadual <> bf-emitente-api.ins-estadual THEN next.  
            END.
        
/*              MESSAGE "depois ins" VIEW-AS ALERT-BOX. */
        
            /* 12/02/04 */
            IF AVAIL es-movto-arroz THEN do:
                IF  c-cod-estabel-esapi007a-36 <> "" AND
                es-movto-arroz.cod-estabel <> c-cod-estabel-esapi007a-36 THEN NEXT.
            END.
            ELSE DO:
                 IF  c-cod-estabel-esapi007a-36 <> "" AND
                es-ticket.cod-estabel <> c-cod-estabel-esapi007a-36 THEN NEXT.
            END.

/* erika 14/03/2012 comentado para pegar linear sem levar em considereá∆o percentual por percentual os valore de secagem linha-a-linha de secagem
pega liquida o que tem a liquidar, pois acima j† Ç fechado e consistido o valor de secagem e de dep¢sito a liquidar, asssim vai pegando o que precisa 
de cada um deles do saldo de terceiro n∆o mais armazenando os restinhos de secagem para cada linha de saldo depositada
            
            MESSAGE bf-emitente-api.ins-estadual
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
                
            /* 100804  interpretar o de-a-asecar pelo tt-extrato*/
                    RUN esp/esapi007a.p (INPUT  es-ticket.nr-contrato,
                                         INPUT  es-ticket.nr-ticket,
                                         INPUT  es-ticket.nr-ticket,
                                         INPUT  bf-emitente-api.ins-estadual,
                                         INPUT  c-cod-estabel-esapi007a-36,
                                         OUTPUT de-entra,
                                         OUTPUT de-saida,
                                         OUTPUT de-tot-saldo,
                                         OUTPUT de-tot-secag,
                                         OUTPUT de-tot-seco,
                                         OUTPUT de-a-secar,
                                         OUTPUT vl-tot-secag,
                                         OUTPUT vl-tot-seco,
                                         OUTPUT vl-a-secar).


                    ASSIGN de-tot-seco-comp = 0.
                    /* 120406 - rotina para achar o valor seco DO saldo-terceiros em quest∆o */
                    FOR EACH bs-componente WHERE
                             bs-componente.cod-emitente = saldo-terc.cod-emitente AND
                             bs-componente.serie-comp   = saldo-terc.serie-docto AND
                             bs-componente.nro-comp     = saldo-terc.nro-docto  and
                             bs-componente.nat-comp     = saldo-terc.nat-oper USE-INDEX comp:

                         FIND FIRST nota-fiscal WHERE
                                    nota-fiscal.cod-estabel = saldo-terc.cod-estabel AND
                                    nota-fiscal.serie       = bs-componente.serie-docto AND
                                    nota-fiscal.nr-nota-fis = bs-componente.nro-docto.
                         IF AVAIL nota-fiscal and
                            SUBSTR(nota-fiscal.observ-nota,1900,7) = "secagem" THEN do:
                             FOR EACH it-nota-fisc OF nota-fiscal:
                               IF it-nota-fisc.conh-fre = STRING(es-ticket.nr-ticket) THEN DO: /* conhecto de frete Ç o nro do ticket */
                                  ASSIGN de-tot-seco-comp = de-tot-seco-comp + it-nota-fisc.qt-faturada[1].
                               END.
                             END.

                         END.

                    END.
                    
                    
/*             FOR EACH tt-extrato:                                                                                */
/*                                                                                                                 */
/*                                                                                                                 */
/*                  MESSAGE "avail tt-extrat" tt-extrato.nr-ticket "extrato secagem " tt-extrato.desc-secagem SKIP */
/*                  "de-a-secar" de-a-secar skip                                                                   */
/*                  "de-tot-seco" de-tot-seco VIEW-AS ALERT-BOX.                                                   */
/*             END.                                                                                                */


*****************************************************************************************************************************************************************************/
            /* erika 14/03/2012


            FIND FIRST tt-extrato WHERE
                tt-extrato.nr-ticket = es-ticket.nr-ticket NO-LOCK NO-ERROR.
            IF AVAIL tt-extrato THEN DO:                
                ASSIGN de-a-secar = tt-extrato.desc-secagem.
/*                   MESSAGE "avail tt-extrat" tt-extrato.nr-ticket "extrato secagem "tt-extrato.desc-secagem SKIP */
/*                  "de-a-secar" de-a-secar skip                                                                   */
/*                  "de-tot-seco" de-tot-seco VIEW-AS ALERT-BOX.                                                   */
            END.
          
            IF de-acum-asecar < 0.01 THEN ASSIGN de-a-secar = 0. 

            ASSIGN de-a-secar = de-acum-asecar.
            MESSAGE "de-a-secar" de-a-secar
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
        
          */
        
/*            message "7 achou saldo-terc" SKIP                                                                                                                                                      */
/*        "i-operacao" i-operacao                                                                                                                                                                    */
/*                                                                                                                                                                                                   */
/*                                                                                                                                                                                                   */
/*                     "de-a-secar" de-a-secar SKIP                                                                                                                                                  */
/*                     "de-tot-seco" de-tot-seco SKIP                                                                                                                                                */
/*                     "de-quantidade" de-quantidade                                                                                                                                                 */
/*                     "saldo-terc.cod-emitente" saldo-terc.cod-emitente                                                                                                                             */
/*                     "componente.serie-docto " saldo-terc.serie-docto                                                                                                                              */
/*                     "nro-docto              " saldo-terc.nro-docto                                                                                                                                */
/*                     "componente.cod-emitente" saldo-terc.cod-emitente                                                                                                                             */
/*                     "componente.nat-operacao" saldo-terc.nat-operacao                                                                                                                             */
/*                     "componente.it-codigo   " saldo-terc.it-codigo                                                                                                                                */
/*                     "componente.cod-refer   " saldo-terc.cod-refer                                                                                                                                */
/*                     "componente.sequencia   " saldo-terc.sequencia  skip(3)                                                                                                                       */
/*                     "oper" i-operacao                                                                                                                                                             */
/*                     "recid" recid(saldo-terc) skip(2)                                                                                                                                             */
/*                     "saldo (saldo-terc.quantidade - de-a-secar - de-tot-seco /* 060406 */ - saldo-terc.dec-1)" (saldo-terc.quantidade - de-a-secar - de-tot-seco /* 060406 */ - saldo-terc.dec-1) */
/*                      SKIP(2)                                                                                                                                                                      */
/*                     "aloca" saldo-terc.dec-1                                                                                                                                                      */
/*                     "quant nota" saldo-terc.quantidade                                                                                                                                            */
/*                      SKIP                                                                                                                                                                         */
/*                     "ticket" es-movto-arroz.nr-ticket       SKIP                                                                                                                                  */
/*                     "tot secag" de-tot-secag                                                                                                                                                      */
/*                     "tot seco" de-tot-seco  SKIP(3)                                                                                                                                               */
/*                     i-operacao SKIP                                                                                                                                                               */
/*                     "de-tot-seco-comp" de-tot-seco-comp                                                                                                                                           */
/*                     view-as alert-box.                                                                                                                                                            */

            ASSIGN de-tot-seco = 0.
            FOR EACH tt-seco WHERE tt-seco.ins-estadual = tt-extrato.ins-estadual:    
                ASSIGN de-tot-seco = tt-seco.de-tot-qt-seco-0001 +   
                                     tt-seco.de-tot-qt-seco-0002 +   
                                     tt-seco.de-tot-qt-seco-0004 +   
                                     tt-seco.de-tot-qt-seco-0007 +   
                                     tt-seco.de-tot-qt-seco-0015 +   
                                     tt-seco.de-tot-qt-seco-0020 +   
                                     tt-seco.de-tot-qt-seco-0036 +   
                                     tt-seco.de-tot-qt-seco-0040.
            END.

/* erika 14/03/2012
           MESSAGE "(saldo-terc.quantidade - de-a-secar - saldo-terc.dec-1) antes zb"
               (saldo-terc.quantidade - de-a-secar - saldo-terc.dec-1) SKIP
              "saldo terc qtde" saldo-terc.quantidade SKIP
               "de-a-secar"  de-a-secar SKIP
               "dec-1"  saldo-terc.dec-1
                SKIP
               "de-tot-seco" de-tot-seco
               VIEW-AS ALERT-BOX.

                /* 110406 */
                /* porque teria pego somente o total seco, e aqui pelo de-tot-seco-comp pegou somente o seco para o ticket */
            IF de-a-secar <> 0 THEN 
                    ASSIGN de-tot-seco = de-tot-seco-comp
                           de-a-secar  = de-a-secar - de-tot-seco.
*/        




            /* erika 14/03/2012 */
            ASSIGN de-a-secar = de-acum-asecar.

/*             MESSAGE "de-a-secar" de-a-secar        */
/*                 VIEW-AS ALERT-BOX INFO BUTTONS OK. */
             
/*             MESSAGE "(saldo-terc.quantidade - saldo-terc.dec-1) aqui zb"                  */
/*                (saldo-terc.quantidade - saldo-terc.dec-1) SKIP "quantidade" de-quantidade */
/*                VIEW-AS ALERT-BOX.                                                         */
/*                                                                                      */

            IF i-operacao = 1 THEN DO:  
                IF (saldo-terc.quantidade - saldo-terc.dec-1) < 0.01 OR saldo-terc.quantidade <= 0.01 THEN DO:
/*                      MESSAGE " next 1" VIEW-AS ALERT-BOX. */
                     NEXT main-block.   
                

                END.
            END. /* oper 1 */
            ELSE DO:
              IF de-a-secar <= 0 OR (saldo-terc.quantidade - saldo-terc.dec-1)  <= 0.01 THEN DO: 
/*                   MESSAGE " next 1" VIEW-AS ALERT-BOX. */
                 NEXT main-block.              
              

              END.
            END.


/*             MESSAGE "passou"                                  */
/*                    "sld terc qtde" saldo-terc.quantidade SKIP */
/*                    "de-a-secar" de-a-secar skip               */
/*                    "dec-1" saldo-terc.dec-1 SKIP              */
/*                 "de-quantidade" de-quantidade                 */
/*                 VIEW-AS ALERT-BOX.                            */

            /* 240507 */
            IF de-a-secar < 0 THEN ASSIGN de-a-secar = 0.
            IF i-operacao  = 1 AND (saldo-terc.quantidade - de-a-secar) = 0 THEN NEXT main-block.
            IF i-operacao <> 1 AND de-a-secar = 0 THEN NEXT main-block.

/*             MESSAGE 'antes componente'             */
/*                 VIEW-AS ALERT-BOX INFO BUTTONS OK. */

            FOR FIRST componente USE-INDEX documento
                WHERE componente.serie-docto  = saldo-terc.serie-docto
                AND   componente.nro-docto    = saldo-terc.nro-docto
                AND   componente.cod-emitente = saldo-terc.cod-emitente
                AND   componente.nat-operacao = saldo-terc.nat-operacao
                AND   componente.it-codigo    = saldo-terc.it-codigo
                AND   componente.cod-refer    = saldo-terc.cod-refer
                AND   componente.sequencia    = saldo-terc.sequencia NO-LOCK:












                    IF  (saldo-terc.quantidade - saldo-terc.dec-1) >= de-quantidade - (de-quantidade - de-quant-dev) THEN DO:
/*                         MESSAGE "MAIOR de-quant-aux " de-quantidade - (de-quantidade - de-quant-dev)             */
/*                                 "de-quant-dev " de-quant-dev  - (de-quantidade - (de-quantidade - de-quant-dev)) */
/*                             VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                   */


                        ASSIGN de-quant-aux = de-quantidade - (de-quantidade - de-quant-dev)
                               de-quant-dev = de-quant-dev  - (de-quantidade - (de-quantidade - de-quant-dev)).
                    END.
                    ELSE DO:
/*                         MESSAGE                                                                       */
/*                                                                                                       */
/*                             "MENOR de-quant-aux" (saldo-terc.quantidade - saldo-terc.dec-1)           */
/*                             "de-quant-dev " de-quant-dev - (saldo-terc.quantidade - saldo-terc.dec-1) */
/*                             VIEW-AS ALERT-BOX INFO BUTTONS OK.                                        */

                        ASSIGN de-quant-aux = (saldo-terc.quantidade - saldo-terc.dec-1)
                               de-quant-dev = de-quant-dev - (saldo-terc.quantidade - saldo-terc.dec-1).
                    END.











              
/*                 IF  i-operacao = 1 THEN DO:                                                                                   */
/*                                                                                                                               */
/*                     IF  (saldo-terc.quantidade - saldo-terc.dec-1) >= de-quantidade - (de-quantidade - de-quant-dev) THEN DO: */
/*                         ASSIGN de-quant-aux = de-quantidade - (de-quantidade - de-quant-dev)                                  */
/*                                de-quant-dev = de-quant-dev  - (de-quantidade - (de-quantidade - de-quant-dev)).               */
/*                     END.                                                                                                      */
/*                     ELSE DO:                                                                                                  */
/*                         ASSIGN de-quant-aux = (saldo-terc.quantidade - saldo-terc.dec-1)                                      */
/*                                de-quant-dev = de-quant-dev - (saldo-terc.quantidade - saldo-terc.dec-1).                      */
/*                     END.                                                                                                      */
/*                 END.                                                                                                          */
/*                 ELSE DO:                                                                                                      */
/*                     /* secagem */                                                                                             */
/*                                                                                                                               */
/*                     IF  (saldo-terc.quantidade - saldo-terc.dec-1) >= de-quantidade - (de-quantidade - de-quant-dev) THEN DO: */
/*                         ASSIGN de-quant-aux = de-quantidade - (de-quantidade - de-quant-dev)                                  */
/*                                de-quant-dev = de-quant-dev  - (de-quantidade - (de-quantidade - de-quant-dev)).               */
/*                     END.                                                                                                      */
/*                     ELSE DO:                                                                                                  */
/*                         ASSIGN de-quant-aux = (saldo-terc.quantidade - saldo-terc.dec-1)                                      */
/*                                de-quant-dev = de-quant-dev - (saldo-terc.quantidade - saldo-terc.dec-1).                      */
/*                     END.                                                                                                      */
/*                 END.                                                                                                          */


/* deixei igual AS duas operaá‰es porque mudamos A compra de secagem n∆o mais proporcional e sim integral 15/03/2012 - erika         */
/*                     IF  de-a-secar >= de-quantidade - (de-quantidade - de-quant-dev) THEN DO:                                      */
/*                         MESSAGE "maior" de-a-secar >= de-quantidade - (de-quantidade - de-quant-dev) SKIP                          */
/*                             "de-a-secar" de-a-secar SKIP                                                                           */
/*                             " de-quantidade - (de-quantidade - de-quant-dev)"  de-quantidade - (de-quantidade - de-quant-dev) SKIP */
/*                             "de-quant-dev" de-quant-dev SKIP(7)                                                                    */
/*                                                                                                                                    */
/*                                                                                                                                    */
/*                                                                                                                                    */
/*                              "de-quant-aux " de-quantidade - (de-quantidade - de-quant-dev) SKIP                                   */
/*                             "de-quantidade" de-quantidade                                       SKIP                               */
/*                             "de-quant-dev" de-quant-dev SKIP                                                                       */
/*                             "nova de-quant-dev = de-quant-dev  - de-quant-aux" de-quant-dev  - de-quant-aux                        */
/*                             "de-quant-dev" de-quant-dev   skip                                                                     */
/*                             "de-quant-aux" de-quant-aux                                                                            */
/*                                                                                                                                    */
/*                             VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                                     */
/*                                                                                                                                    */
/*                         ASSIGN de-quant-aux = de-quantidade - /*(de-quantidade - de-quant-dev)*/                                   */
/*                                de-quant-dev = de-quant-dev  - de-quant-aux.                                                        */
/*                     END.                                                                                                           */
/*                     ELSE DO:                                                                                                       */
/*                         MESSAGE "menor" de-a-secar >= de-quantidade - (de-quantidade - de-quant-dev)                               */
/*                             VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                                     */
/*                         ASSIGN de-quant-aux = de-a-secar                                                                           */
/*                                de-quant-dev = de-quant-dev - de-a-secar.                                                           */
/*                     END.                                                                                                           */
        


/*                 MESSAGE " antes"                                                                               */
/*                   "saldo-terc.quantidade" saldo-terc.quantidade  skip                                          */
/*                   "saldo-terc.dec-1" saldo-terc.dec-1 SKIP                                                     */
/*                    "saldo-terc.quantidade -  saldo-terc.dec-1"  saldo-terc.quantidade -  saldo-terc.dec-1 SKIP */
/*                   "de-quantidade  " de-quantidade   SKIP                                                       */
/*                   "de-quant-aux   " de-quant-aux   SKIP                                                        */
/*                   "DE-quant-dev" de-quant-dev SKIP                                                             */
/*                   "de-a-secar Ç a qtde que aloca para secagem" de-a-secar                                      */
/*                   VIEW-AS ALERT-BOX.                                                                           */

        
                
                FIND LAST b-tt-it-terc-nf USE-INDEX codigo 
                    NO-LOCK NO-ERROR.
                IF  AVAIL b-tt-it-terc-nf THEN
                    ASSIGN i-sequencia = b-tt-it-terc-nf.sequencia + 10.
                ELSE ASSIGN i-sequencia = 10.

                
                         
                    CREATE tt-it-terc-nf.
                    ASSIGN tt-it-terc-nf.rw-saldo-terc     = ROWID(saldo-terc)
                           tt-it-terc-nf.sequencia         = i-sequencia
                           tt-it-terc-nf.it-codigo         = saldo-terc.it-codigo
                           tt-it-terc-nf.cod-refer         = saldo-terc.cod-refer
                           tt-it-terc-nf.desc-nar          = IF  item.tipo-contr = 4 /* DÇbito Direto */
                                                             THEN SUBSTR(item.narrativa,1,60)
                                                             ELSE item.desc-item
                           tt-it-terc-nf.quantidade        = IF  i-operacao = 1 
                                                                 THEN (saldo-terc.quantidade)
                                                                 ELSE de-a-secar
                           tt-it-terc-nf.qt-alocada        = IF  i-operacao = 1  
                                                                 THEN saldo-terc.dec-1
                                                                 ELSE de-a-secar
                           tt-it-terc-nf.qt-disponivel     = IF  i-operacao = 1 
                                                                 THEN (saldo-terc.quantidade - saldo-terc.dec-1)
                                                                 ELSE de-a-secar
                           tt-it-terc-nf.qt-disponivel-inf = de-quant-aux
                                                                     /*
                           tt-it-terc-nf.qt-disponivel-inf = tt-it-terc-nf.qt-disponivel
                           */
                           tt-it-terc-nf.preco-total       = componente.preco-total[1]
                           tt-it-terc-nf.selecionado       = NO
                           tt-it-terc-nf.qt-faturada       = tt-it-terc-nf.qt-disponivel-inf                                          
                           tt-it-terc-nf.un-faturada       = ITEM.un
                           tt-it-terc-nf.un-estoque        = ITEM.un.
        
/*         MESSAGE                                                           */
/*         "tt-it-terc-nf.sequencia  "       tt-it-terc-nf.sequencia         */
/*         "tt-it-terc-nf.it-codigo  "       tt-it-terc-nf.it-codigo         */
/*         "tt-it-terc-nf.cod-refer  "       tt-it-terc-nf.cod-refer         */
/*         "tt-it-terc-nf.desc-nar "         tt-it-terc-nf.desc-nar          */
/*         "tt-it-terc-nf.quantidade  "      tt-it-terc-nf.quantidade        */
/*         "tt-it-terc-nf.qt-alocada "       tt-it-terc-nf.qt-alocada        */
/*         "tt-it-terc-nf.qt-disponivel  "   tt-it-terc-nf.qt-disponivel     */
/*         "tt-it-terc-nf.qt-disponivel-inf" tt-it-terc-nf.qt-disponivel-inf */
/*         "tt-it-terc-nf.preco-total "      tt-it-terc-nf.preco-total       */
/*         "tt-it-terc-nf.selecionado "      tt-it-terc-nf.selecionado       */
/*         VIEW-AS ALERT-BOX.                                                */
        
        
                IF  i-operacao = 1 THEN do:
                  IF de-quant-aux > saldo-terc.quantidade  THEN assign tt-it-terc-nf.quantidade = round(tt-it-terc-nf.qt-disponivel-inf,2).
                END.
        
        
                find first b-componente USE-INDEX comp
                    WHERE b-componente.cod-emitente = componente.cod-emitente 
                    AND   b-componente.nro-comp     = componente.nro-docto    
                    AND   b-componente.serie-comp   = componente.serie-docto  
                    AND   b-componente.nat-comp     = componente.nat-operacao 
                    AND   b-componente.it-codigo    = componente.it-codigo    
                    AND   b-componente.cod-refer    = componente.cod-refer   
                    AND   b-componente.seq-comp     = componente.sequencia 
                    AND   (   b-componente.quantidade > 0
                           OR b-componente.dt-retorno = dt-trans-esapi007a-36) NO-LOCK NO-ERROR. /* NF Reajuste ate Dt Trans */ 
                if avail b-componente then do:
                    IF  b-componente.componente = 1 then do:
                    /* Envio */
                        ASSIGN tt-it-terc-nf.preco-total = tt-it-terc-nf.preco-total + b-componente.preco-total[1].
                    end.
                    ELSE do: /* Retorno */
                        ASSIGN tt-it-terc-nf.preco-total = tt-it-terc-nf.preco-total - b-componente.preco-total[1].
                    end.    
                END.
                  
                ASSIGN tt-it-terc-nf.preco-total-inf = tt-it-terc-nf.preco-total.
/*                 MESSAGE "antes de-quant-dev <= 0 THEN LEAVE Main-Block." view-as alert-box. */

                 IF  de-quant-dev <= 0 THEN LEAVE Main-Block.

/*                  MESSAGE "depois de-quant-dev <= 0 THEN LEAVE Main-Block." view-as alert-box. */

                  

            END.
            
/*             MESSAGE "antes DOIS de-quant-dev <= 0 THEN LEAVE Main-Block." view-as alert-box. */

                 IF  de-quant-dev <= 0 THEN LEAVE Main-Block.

/*                  MESSAGE "depois DOIS de-quant-dev <= 0 THEN LEAVE Main-Block." view-as alert-box. */

        END.
     END. /* end else */

END. /* DO */

APPLY 'close' TO THIS-PROCEDURE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy w-window 
PROCEDURE local-destroy :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'destroy':U ) .
  {include/i-logfin.i}

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit w-window 
PROCEDURE local-exit :
/* -----------------------------------------------------------
  Purpose:  Starts an "exit" by APPLYing CLOSE event, which starts "destroy".
  Parameters:  <none>
  Notes:    If activated, should APPLY CLOSE, *not* dispatch adm-exit.   
-------------------------------------------------------------*/
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   
   RETURN.
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize w-window 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  {include/win-size.i}
  
{utp/ut9000.i "ES4036ZB" "2.09.00.002"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  ASSIGN i-operacao = pi-operacao.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records w-window  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this JanelaDetalhe, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed w-window 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
